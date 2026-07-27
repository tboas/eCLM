module dynCovercropFileMod

#include "shr_assert.h"

  !---------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Read the cover-crop rotation file and switch patch ivt after harvest.
  !
  ! Rotation file format:
  !   dimensions: time, cft, lndgrid
  !   integer YEAR(time)                  -- calendar year per time slice
  !   real    PCT_CFT(time, cft, lndgrid) -- CFT fractions 0-100%
  !
  ! After harvest of any cash crop, covercrop_switch_ivt finds the dominant
  ! CFT in the next year slice for this gridcell and switches ivt(p).
  ! Works identically for point and regional/global domains.
  !
  ! Backward compatibility:
  !   use_covercropping = .false. -> never called, zero overhead
  !   use_covercropping = .true.  -> transient_landuse_file must be set
  ! tboas
  !
  ! !USES:
  use shr_kind_mod            , only : r8 => shr_kind_r8
  use shr_log_mod             , only : errMsg => shr_log_errMsg
  use decompMod               , only : bounds_type, BOUNDS_LEVEL_PROC
  use dynFileMod              , only : dyn_file_type
  use clm_varctl              , only : iulog, use_covercropping, transient_landuse_file
  use clm_varcon              , only : grlnd
  use clm_varpar              , only : cft_size, cft_lb
  use abortutils              , only : endrun
  use histFileMod             , only : hist_addfld1d
  use spmdMod                 , only : masterproc
  use PatchType               , only : patch
  use pftconMod               , only : pftcon, ncovercrop_1, ncovercrop_2
  use clm_varctl              , only : use_grainproduct
  !
  implicit none
  private
  save

  public :: dyncovercrop_init
  public :: dyncovercrop_interp
  public :: covercrop_switch_ivt

  !---------------------------------------------------------------------------
  type(dyn_file_type), target :: dyncovercrop_file
  real(r8), allocatable :: pct_cft_cur (:,:)  ! (begg:endg, cft_size)
  real(r8), allocatable :: pct_cft_next(:,:)  ! (begg:endg, cft_size)
  real(r8), pointer :: active_ivt_patch(:) => null() ! (begp:endp) active itype for history  ! tboas

  character(len=*), parameter, private :: sourcefile = __FILE__

contains

  !-----------------------------------------------------------------------
  subroutine dyncovercrop_init(bounds)
    use dynTimeInfoMod        , only : YEAR_POSITION_START_OF_TIMESTEP
    use dynVarTimeUninterpMod , only : dyn_var_time_uninterp_type
    use ncdio_pio             , only : check_dim
    type(bounds_type), intent(in) :: bounds
    type(dyn_var_time_uninterp_type) :: wtcft_obj
    integer :: num_points, pct_cft_shape(2), pi
    character(len=*), parameter :: subname = "dyncovercrop_init"
    !-----------------------------------------------------------------------
    SHR_ASSERT_ALL(bounds%level == BOUNDS_LEVEL_PROC, &
         subname // ": argument must be PROC-level bounds")
    if (.not. use_covercropping) return
    if (trim(transient_landuse_file) == "" .or. &
        trim(transient_landuse_file) == " ") then
       call endrun(msg=" ERROR: use_covercropping=.true. but transient_landuse_file" // &
            " is not set in lnd_in." // errMsg(sourcefile, __LINE__))
    end if
    if (masterproc) write(iulog,*) &
         "dyncovercrop_init: opening ", trim(transient_landuse_file)
    dyncovercrop_file = dyn_file_type( &
         trim(transient_landuse_file), YEAR_POSITION_START_OF_TIMESTEP)
    call check_dim(dyncovercrop_file, "cft", cft_size)
    num_points    = bounds%endg - bounds%begg + 1
    pct_cft_shape = [num_points, cft_size]
    allocate(pct_cft_cur (bounds%begg:bounds%endg, cft_size))
    allocate(pct_cft_next(bounds%begg:bounds%endg, cft_size))
    pct_cft_cur  = 0._r8
    pct_cft_next = 0._r8
    call dyncovercrop_interp(bounds)

    ! Register IVT as history field for rotation diagnostics --- tboas
    allocate(active_ivt_patch(bounds%begp:bounds%endp))
    do pi = bounds%begp, bounds%endp
       active_ivt_patch(pi) = real(patch%itype(pi), r8)
    end do
    call hist_addfld1d(fname='IVT', units='unitless', &
         avgflag='A', long_name='current patch vegetation type index', &
         ptr_patch=active_ivt_patch, default='active')
  end subroutine dyncovercrop_init

  !-----------------------------------------------------------------------
  subroutine dyncovercrop_interp(bounds)
    use dynVarTimeUninterpMod , only : dyn_var_time_uninterp_type
    use ncdio_pio             , only : ncd_io
    type(bounds_type), intent(in) :: bounds
    type(dyn_var_time_uninterp_type) :: wtcft_obj
    integer :: num_points, pct_cft_shape(2), pi
    integer :: idx_next, ntimes
    real(r8), pointer :: raw_next(:,:)  ! (lndgrid, cft) pointer for ncd_io_2d
    logical :: readvar
    !-----------------------------------------------------------------------
    if (.not. use_covercropping)       return
    if (.not. allocated(pct_cft_cur))  return
    call dyncovercrop_file%time_info%set_current_year()
    num_points    = bounds%endg - bounds%begg + 1
    pct_cft_shape = [num_points, cft_size]

    ! Read current year PCT_CFT via standard mechanism
    wtcft_obj = dyn_var_time_uninterp_type( &
         dyn_file              = dyncovercrop_file, &
         varname               = "PCT_CFT", &
         dim1name              = grlnd, &
         conversion_factor     = 100._r8, &
         do_check_sums_equal_1 = .false., &
         data_shape            = pct_cft_shape)
    call wtcft_obj%get_current_data(pct_cft_cur(bounds%begg:bounds%endg, :))

    ! Read next year PCT_CFT directly via ncd_io_2d at time_index_lower + 1
    ! This is needed for post-harvest planting of winter crops/cover crops
    ! which are sown in fall of current year but belong to next year's rotation
    ntimes   = dyncovercrop_file%time_info%get_time_index_upper()
    idx_next = min(dyncovercrop_file%time_info%get_time_index_lower() + 1, ntimes)
    nullify(raw_next)
    allocate(raw_next(num_points, cft_size))
    raw_next => raw_next
    call ncd_io(varname="PCT_CFT", data=raw_next, dim1name=grlnd, &
         flag="read", ncid=dyncovercrop_file, nt=idx_next, readvar=readvar)
    if (readvar) then
       pct_cft_next(bounds%begg:bounds%endg, :) = raw_next * 100._r8
    else
       ! Fallback: end of timeseries — use current year
       pct_cft_next(bounds%begg:bounds%endg, :) = pct_cft_cur(bounds%begg:bounds%endg, :)
       if (masterproc) write(iulog,*) "dyncovercrop_interp: end of timeseries, using current year for pct_cft_next"
    end if
    deallocate(raw_next)
    ! Update IVT history field with current patch itype --- tboas
    if (associated(active_ivt_patch)) then
       do pi = bounds%begp, bounds%endp
          active_ivt_patch(pi) = real(patch%itype(pi), r8)
       end do
    end if
  end subroutine dyncovercrop_interp

  !-----------------------------------------------------------------------
  subroutine covercrop_switch_ivt(p, crop_inst, cnveg_state_inst)
    use CropType         , only : crop_type
    use CNVegStateType   , only : cnveg_state_type
    integer               , intent(in)    :: p
    type(crop_type)       , intent(inout) :: crop_inst
    type(cnveg_state_type), intent(inout) :: cnveg_state_inst
    integer  :: g, cft, best_cft, new_ivt
    real(r8) :: best_pct
    integer, parameter :: NOT_Planted = 999
    !-----------------------------------------------------------------------
    if (.not. use_covercropping)       return
    if (.not. allocated(pct_cft_cur)) return
    g = patch%gridcell(p)
    ! Find dominant CFT in next-year slice for this gridcell
    best_cft = 1
    best_pct = -1._r8
    do cft = 1, cft_size
       if (pct_cft_cur(g, cft) > best_pct) then
          best_pct = pct_cft_cur(g, cft)
          best_cft = cft
       end if
    end do
    ! Convert CFT array index to global PFT index
    ! cft_lb = first crop PFT index = natpft_ub + 1
    new_ivt = cft_lb + best_cft - 1
    if (new_ivt == patch%itype(p)) return
    patch%itype(p)                       = new_ivt
    if (associated(active_ivt_patch)) active_ivt_patch(p) = real(new_ivt, r8)
    crop_inst%croplive_patch(p)          = .false.
    crop_inst%cropplant_patch(p)         = .false.
    cnveg_state_inst%idop_patch(p)       = NOT_Planted
    ! tboas: C/N pools kept from previous crop; C balance tolerance relaxed in CNBalanceCheckMod
    ! Suppress grain product accounting for cover crops
    if (new_ivt == ncovercrop_1 .or. new_ivt == ncovercrop_2) then
       use_grainproduct = .false.
    else
       use_grainproduct = .true.
    end if
  end subroutine covercrop_switch_ivt

end module dynCovercropFileMod
