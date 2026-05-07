module clm_varctl

!-----------------------------------------------------------------------
! !DESCRIPTION:
! Module containing run control variables
!
! !USES:
use shr_kind_mod, only: r8 => shr_kind_r8, SHR_KIND_CL
use shr_sys_mod , only: shr_sys_abort ! cannot use endrun here due to circular dependency
!
! !PUBLIC MEMBER FUNCTIONS:
implicit none
public :: clm_varctl_set ! Set variables
public :: cnallocate_carbon_only_set
public :: cnallocate_carbon_only
!
private
save
!
! !PUBLIC TYPES:
!
integer , parameter, public :: iundef = -9999999
real(r8), parameter, public :: rundef = -9999999._r8
integer , parameter, public :: fname_len = SHR_KIND_CL ! max length of file names in this module

!----------------------------------------------------------
! Run control variables
!----------------------------------------------------------
character(len=256), public :: caseid  = ' '
character(len=256), public :: ctitle  = ' '
integer, public :: nsrest             = iundef
logical, public :: is_cold_start      = .false.
logical, public :: is_interpolated_start = .false.
integer, public, parameter :: nsrStartup  = 0
integer, public, parameter :: nsrContinue = 1
integer, public, parameter :: nsrBranch   = 2
logical, public :: brnch_retain_casename = .false.
logical, public :: noland             = .false.
character(len=256), public :: hostname = ' '
character(len=256), public :: username = ' '
character(len=256), public :: source   = "Community Land Model CLM4.0"
character(len=256), public :: version  = " "
character(len=256), public :: conventions = "CF-1.0"

!----------------------------------------------------------
! Unit Numbers
!----------------------------------------------------------
integer, public :: iulog = 6

!----------------------------------------------------------
! Output NetCDF files
!----------------------------------------------------------
logical, public :: outnc_large_files = .true.

!----------------------------------------------------------
! Run input files
!----------------------------------------------------------
character(len=fname_len), public :: finidat    = ' '
character(len=fname_len), public :: fsurdat    = ' '
character(len=fname_len), public :: fatmgrid   = ' '
character(len=fname_len), public :: fatmlndfrc = ' '
character(len=fname_len), public :: paramfile  = ' '
character(len=fname_len), public :: nrevsn     = ' '
character(len=fname_len), public :: fsnowoptics = ' '
character(len=fname_len), public :: fsnowaging  = ' '

!----------------------------------------------------------
! Flag to read ndep rather than obtain it from coupler
!----------------------------------------------------------
logical, public :: ndep_from_cpl = .false.

!----------------------------------------------------------
! Interpolation of finidat if requested
!----------------------------------------------------------
logical, public :: bound_h2osoi = .true.
character(len=fname_len), public :: finidat_interp_source = ' '
character(len=fname_len), public :: finidat_interp_dest   = ''

!----------------------------------------------------------
! Crop & Irrigation logic
!----------------------------------------------------------
logical, public :: use_crop             = .false.
logical, public :: create_crop_landunit = .false.
logical, public :: irrigate             = .false.

!----------------------------------------------------------
! Other subgrid logic
!----------------------------------------------------------
logical, public :: run_zero_weight_urban = .false.
logical, public :: all_active            = .false.

!----------------------------------------------------------
! BGC logic and datasets
!----------------------------------------------------------
character(len=16), public :: co2_type    = 'constant'
integer, public :: spinup_state          = 0
logical, public :: anoxia                = .true.
logical, public :: override_bgc_restart_mismatch_dump = .false.
logical, private:: carbon_only
real(r8), public :: nfix_timeconst       = -1.2345_r8

!----------------------------------------------------------
! Physics
!----------------------------------------------------------
integer, public  :: subgridflag = 1
logical, public  :: wrtdia      = .false.
real(r8), public :: co2_ppmv    = 355._r8

!----------------------------------------------------------
! C isotopes
!----------------------------------------------------------
logical, public :: use_c13 = .false.
logical, public :: use_c14 = .false.
logical, public :: for_testing_allow_interp_non_ciso_to_ciso = .false.

!----------------------------------------------------------
! FATES switches
!----------------------------------------------------------
logical, public :: use_fates                      = .false.
integer, public :: fates_parteh_mode              = -9
logical, public :: use_fates_spitfire             = .false.
logical, public :: use_fates_logging              = .false.
logical, public :: use_fates_planthydro           = .false.
logical, public :: use_fates_ed_st3               = .false.
logical, public :: use_fates_ed_prescribed_phys   = .false.
logical, public :: use_fates_inventory_init        = .false.
character(len=256), public :: fates_inventory_ctrl_filename = ''

!----------------------------------------------------------
! LUNA switches
!----------------------------------------------------------
logical, public :: use_luna = .false.

!----------------------------------------------------------
! flexibleCN
!----------------------------------------------------------
logical, public :: use_flexibleCN     = .false.
logical, public :: MM_Nuptake_opt     = .false.
logical, public :: downreg_opt        = .true.
integer, public :: plant_ndemand_opt  = 0
logical, public :: substrate_term_opt = .true.
logical, public :: nscalar_opt        = .true.
logical, public :: temp_scalar_opt    = .true.
logical, public :: CNratio_floating   = .false.
logical, public :: lnc_opt            = .false.
logical, public :: reduce_dayl_factor = .false.
integer, public :: vcmax_opt          = 0
integer, public :: CN_residual_opt    = 0
integer, public :: CN_partition_opt   = 0
integer, public :: CN_evergreen_phenology_opt = 0
integer, public :: carbon_resp_opt    = 0

!----------------------------------------------------------
! prescribed soil moisture streams switch
!----------------------------------------------------------
logical, public :: use_soil_moisture_streams = .false.

!----------------------------------------------------------
! lai streams switch for Sat. Phenology
!----------------------------------------------------------
logical, public :: use_lai_streams = .false.

!----------------------------------------------------------
! bedrock / soil depth switch
!----------------------------------------------------------
logical, public :: use_bedrock = .false.
character(len=16), public :: soil_layerstruct = '10SL_3.5m'

!----------------------------------------------------------
! plant hydraulic stress switch
!----------------------------------------------------------
logical, public :: use_hydrstress = .false.

!----------------------------------------------------------
! dynamic root switch
!----------------------------------------------------------
logical, public :: use_dynroot = .false.

!----------------------------------------------------------
! glacier_mec control variables
!----------------------------------------------------------
logical , public :: glc_do_dynglacier          = .false.
integer , public :: glc_snow_persistence_max_days = 7300

!----------------------------------------------------------
! single column control variables
!----------------------------------------------------------
logical, public  :: single_column = .false.
real(r8), public :: scmlat        = rundef
real(r8), public :: scmlon        = rundef

!----------------------------------------------------------
! instance control
!----------------------------------------------------------
integer, public           :: inst_index
character(len=16), public :: inst_name
character(len=16), public :: inst_suffix

!----------------------------------------------------------
! Decomp control variables
!----------------------------------------------------------
integer, public :: nsegspc = 20

!----------------------------------------------------------
! Derived variables (run, history and restart file)
!----------------------------------------------------------
character(len=256), public :: rpntdir = '.'
character(len=256), public :: rpntfil = 'rpointer.lnd'
logical, public :: hist_wrtch4diag = .false.

!----------------------------------------------------------
! FATES
!----------------------------------------------------------
character(len=fname_len), public :: fates_paramfile = ' '

!----------------------------------------------------------
! SSRE diagnostic
!----------------------------------------------------------
logical, public :: use_SSRE = .false.

!----------------------------------------------------------
! Migration of CPP variables
!----------------------------------------------------------
logical, public :: use_lch4             = .false.
logical, public :: use_nitrif_denitrif  = .false.
logical, public :: use_vertsoilc        = .false.
logical, public :: use_extralakelayers  = .false.
logical, public :: use_vichydro         = .false.
logical, public :: use_century_decomp   = .false.
logical, public :: use_cn               = .false.
logical, public :: use_cndv             = .false.
logical, public :: use_grainproduct     = .false.
logical, public :: use_fertilizer       = .false.
logical, public :: use_ozone            = .false.
logical, public :: use_snicar_frc       = .false.
logical, public :: use_vancouver        = .false.
logical, public :: use_mexicocity       = .false.
logical, public :: use_noio             = .false.
logical, public :: use_nguardrail       = .false.

! tboas: namelist parameter for optional organic carbon fertilizer from manure
logical, public :: use_cfert            = .false.
  real(r8), public :: manure_CN_ratio      = 25.0_r8  ! tboas: C:N ratio for farmyard manure (cfert_inparm)
  real(r8), public :: manure_fmet          = 0.60_r8  ! tboas: metabolic litter fraction of manure C (cfert_inparm)
  real(r8), public :: manure_fcel          = 0.30_r8  ! tboas: cellulose litter fraction of manure C (cfert_inparm)
  real(r8), public :: manure_flig          = 0.10_r8  ! tboas: lignin litter fraction of manure C (cfert_inparm)
  real(r8), public :: manure_injection_depth = 0.0_r8 ! tboas: manure C injection depth [m] (cfert_inparm); 0=surface only

!----------------------------------------------------------
! To retrieve namelist
!----------------------------------------------------------
character(len=SHR_KIND_CL), public :: NLFilename_in ! Namelist filename
!
logical, private :: clmvarctl_isset = .false.

!-----------------------------------------------------------------------
contains

!---------------------------------------------------------------------------
subroutine clm_varctl_set( caseid_in, ctitle_in, brnch_retain_casename_in, &
     single_column_in, scmlat_in, scmlon_in, nsrest_in, &
     version_in, hostname_in, username_in)
  character(len=256), optional, intent(IN) :: caseid_in
  character(len=256), optional, intent(IN) :: ctitle_in
  logical,            optional, intent(IN) :: brnch_retain_casename_in
  logical,            optional, intent(IN) :: single_column_in
  real(r8),           optional, intent(IN) :: scmlat_in
  real(r8),           optional, intent(IN) :: scmlon_in
  integer,            optional, intent(IN) :: nsrest_in
  character(len=256), optional, intent(IN) :: version_in
  character(len=256), optional, intent(IN) :: hostname_in
  character(len=256), optional, intent(IN) :: username_in
  !-----------------------------------------------------------------------
  if ( clmvarctl_isset )then
     call shr_sys_abort(' ERROR:: control variables already set, cannot call this routine')
  end if
  if ( present(caseid_in               ) ) caseid                = caseid_in
  if ( present(ctitle_in               ) ) ctitle                = ctitle_in
  if ( present(single_column_in        ) ) single_column         = single_column_in
  if ( present(scmlat_in               ) ) scmlat                = scmlat_in
  if ( present(scmlon_in               ) ) scmlon                = scmlon_in
  if ( present(nsrest_in               ) ) nsrest                = nsrest_in
  if ( present(brnch_retain_casename_in) ) brnch_retain_casename = brnch_retain_casename_in
  if ( present(version_in              ) ) version               = version_in
  if ( present(username_in             ) ) username              = username_in
  if ( present(hostname_in             ) ) hostname              = hostname_in
end subroutine clm_varctl_set

! Set module carbon_only flag
subroutine cnallocate_carbon_only_set(carbon_only_in)
  logical, intent(in) :: carbon_only_in
  carbon_only = carbon_only_in
end subroutine cnallocate_carbon_only_set

! Get module carbon_only flag
logical function CNAllocate_Carbon_only()
  cnallocate_carbon_only = carbon_only
end function CNAllocate_Carbon_only

end module clm_varctl
