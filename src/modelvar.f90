MODULE MODELVAR
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_short,          &
                                         c_int,            &
                                         c_long_long,      &
                                         c_float,          &
                                         c_double,         &
                                         c_float_complex,  &
                                         c_double_complex, &
                                         c_bool            
  USE MODELTYPE
  IMPLICIT NONE 
  

  INTEGER, PARAMETER   :: i1  = SELECTED_INT_KIND(2)
  INTEGER, PARAMETER   :: i2  = c_short
  INTEGER, PARAMETER   :: i4  = c_int
  INTEGER, PARAMETER   :: i8  = c_long_long
  INTEGER, PARAMETER   :: sp  = c_float
  INTEGER, PARAMETER   :: dp  = c_double
  INTEGER, PARAMETER   :: spc = c_float_complex
  INTEGER, PARAMETER   :: dpc = c_double_complex
  INTEGER, PARAMETER   :: lgt = KIND(.TRUE.)
  CHARACTER(LEN = 10)  ::  logdate            !<Date for log-file name
  CHARACTER(LEN = 10)  ::  logtime            !<Time for log-file name
  CHARACTER(LEN = 10)  ::  logzone            !<hhmm
  INTEGER              ::  logdtvalue(8)
  CHARACTER(LEN = 30)  ::  logfilename
  REAL(sp),PARAMETER         :: eps_sp        = EPSILON(1.0_sp)
  REAL(dp),PARAMETER         :: eps_dp        = EPSILON(1.0_dp)
  REAL(sp),PARAMETER         :: twothird_sp   = 0.6666666666666666666666666666666666667_sp
  REAL(dp),PARAMETER         :: twothird_dp   = 0.6666666666666666666666666666666666667_dp
  REAL(KIND=8),PARAMETER     :: realzero      = 1.E-37
  REAL(KIND=8),PARAMETER     :: pi            = 3.1415927 
  REAL(KIND=8),PARAMETER     :: solar         = 0.0820    !<MJ/m2/min, solar constant
  REAL(KIND=8),PARAMETER     :: missing_value = -9999. 
  INTEGER              ::      nrow_grid           
  INTEGER              ::      ncol_grid           
  INTEGER              ::      num_cell
  INTEGER              ::      num_sim_cell
  INTEGER              ::      sim_length
  INTEGER              ::      comp_length 
  INTEGER              ::      warm_length    
  INTEGER              ::      sim_yr  
  INTEGER              ::      comp_yr          
  TYPE(DateType)       ::      bdate
  TYPE(DateType)       ::      cdate
  TYPE(DateType)       ::      edate
  TYPE(DateType)       ::      steplen
  INTEGER              ::      num_sub 
  INTEGER,ALLOCATABLE  ::      sub_num_cell(:)            
  INTEGER              ::      num_landuse
  INTEGER              ::      num_soiltype 
  INTEGER              ::      max_soillayer 
  INTEGER              ::      num_rotation
  INTEGER              ::      num_node 
  INTEGER              ::      num_ant_type     !<Antibotic types
  INTEGER              ::      num_raingauge
  REAL(KIND=8)         ::      cell_area
  REAL(KIND=8)         ::      nday_fert
  REAL(KIND=8)         ::      nday_residue
  INTEGER              ::      num_crop
  INTEGER, ALLOCATABLE ::      crop_luid(:)
 

  TYPE(SIMCFGTYPE)     ::      simcfg
  INTEGER              ::      num_threads
  INTEGER, PARAMETER   ::      num_np = 4
  INTEGER, PARAMETER   ::      id_in = 1
  INTEGER, PARAMETER   ::      id_on = 2
  INTEGER, PARAMETER   ::      id_sp = 3
  INTEGER, PARAMETER   ::      id_pp = 4
  LOGICAL              ::      conductwarning  !<Status for warning output to hyss.log
  INTEGER              ::      seconds_per_timestep    !<Number of seconds for one time step
  LOGICAL              ::      firstoutstep            !<Flag for first timestep with print out
  INTEGER              ::      dtmonday                !<Number of timesteps between first timestep (bdate) and (first timestep of) previous Monday 
  INTEGER, PARAMETER   ::      idtlag = 0  !<Number of timesteps between 00:00 and first timestep, 0 for daily
  TYPE(DateType),ALLOCATABLE ::  times_sim(:)
  CHARACTER(LEN=16),ALLOCATABLE :: date_sim(:)
  CHARACTER(LEN=16),ALLOCATABLE :: date_comp(:)
  TYPE(TIMEINFORMATIONTYPE)  ::  current_time
  REAL(KIND=8), ALLOCATABLE   ::    soildepth(:,:)   !<layer, soiltype    
  REAL(KIND=8), ALLOCATABLE   ::    soilthick(:,:)   !!m      
  REAL(KIND=8), ALLOCATABLE   ::    streamdepth(:)         
  REAL(KIND=8), ALLOCATABLE   ::    ksat(:,:)
  REAL(KIND=8), ALLOCATABLE   ::    clay_soil(:,:)
  REAL(KIND=8), ALLOCATABLE   ::    silt_soil(:,:)
  REAL(KIND=8), ALLOCATABLE   ::    sand_soil(:,:)  
  REAL(KIND=8), ALLOCATABLE   ::    bd_soil(:,:) 
  INTEGER, PARAMETER          ::    timeformat = 0     
  REAL(KIND=8), PARAMETER     ::    temp_t = 0.1    
  
  ! steplen%Year=0;

  INTEGER, PARAMETER :: dem_file_unit        = 10
  INTEGER, PARAMETER :: landuse_file_unit    = 11
  INTEGER, PARAMETER :: subbasin_file_unit   = 12
  INTEGER, PARAMETER :: soiltype_file_unit   = 13 
  INTEGER, PARAMETER :: soildata_file_unit   = 14
  INTEGER, PARAMETER :: slope_file_unit      = 15
  INTEGER, PARAMETER :: rivdata_file_unit    = 16
  INTEGER, PARAMETER :: prec_file_unit       = 20
  INTEGER, PARAMETER :: temp_file_unit       = 21  
  INTEGER, PARAMETER :: pet_file_unit        = 22
  INTEGER, PARAMETER :: wind_file_unit       = 23
  INTEGER, PARAMETER :: humidity_file_unit   = 24
  INTEGER, PARAMETER :: cfg_file_unit        = 30
  INTEGER, PARAMETER :: par_file_unit        = 31
  INTEGER, PARAMETER :: crop_file_unit       = 32
  INTEGER, PARAMETER :: aquiferdata_file_unit  = 33
  INTEGER, PARAMETER :: river_file_unit      = 34
  INTEGER, PARAMETER :: surodir_file_unit    = 35

  INTEGER, PARAMETER :: test_unit      = 999
  INTEGER, PARAMETER :: test2_unit     = 998

  INTEGER, PARAMETER :: log_unit       = 100
  INTEGER, PARAMETER :: riv_out_unit   = 101

  INTEGER, PARAMETER :: timesteps_per_day = 1  !<1 day as time step  

  CHARACTER(LEN = *), PARAMETER :: cfg_file       = 'dat/SHUI.cfg'
  CHARACTER(LEN = *), PARAMETER :: dem_file       = 'dat/DEM.txt'
  CHARACTER(LEN = *), PARAMETER :: landuse_file   = 'dat/landuse.txt'
  CHARACTER(LEN = *), PARAMETER :: subbasin_file  = 'dat/subbasin.txt'
  CHARACTER(LEN = *), PARAMETER :: soiltype_file  = 'dat/soiltype.txt'
  CHARACTER(LEN = *), PARAMETER :: soildata_file  = 'dat/soildata.txt'
  CHARACTER(LEN = *), PARAMETER :: slope_file     = 'dat/slope.txt' 
  CHARACTER(LEN = *), PARAMETER :: river_file     = 'dat/river.txt' 
  CHARACTER(LEN = *), PARAMETER :: rivdata_file   = 'dat/riverdata.txt' 
  CHARACTER(LEN = *), PARAMETER :: prec_file      = 'dat/precipitation.txt'
  CHARACTER(LEN = *), PARAMETER :: temp_file      = 'dat/temperature.txt'
  CHARACTER(LEN = *), PARAMETER :: wind_file      = 'dat/windspeed.txt'
  CHARACTER(LEN = *), PARAMETER :: humidity_file  = 'dat/humidity.txt'
  CHARACTER(LEN = *), PARAMETER :: par_file       = 'dat/parameter.txt'
  CHARACTER(LEN = *), PARAMETER :: crop_file      = 'dat/cropdata.txt'
  CHARACTER(LEN = *), PARAMETER :: aquiferdata_file = 'dat/aquiferdata.txt'
  CHARACTER(LEN = *), PARAMETER :: surodir_file   = 'dat/surodir.txt'
  CHARACTER(LEN = *), PARAMETER :: prec_nc_file   = 'dat/precipitation.nc'
  

  CHARACTER(LEN = *), PARAMETER :: riv_out_file   = 'out/rch.flow' 

  CHARACTER(LEN = *), PARAMETER :: hotstart_file  = 'out/out_hot.nc' 
  CHARACTER(LEN = *), PARAMETER :: land_nc_file   = 'out/out_land.nc'
  
  INTEGER, ALLOCATABLE           ::    landuse (:)
  INTEGER, ALLOCATABLE           ::    subbasin(:)
  INTEGER, ALLOCATABLE           ::    soiltype(:)
  REAL(KIND=8), ALLOCATABLE      ::    slope(:)
  REAL(KIND=8), ALLOCATABLE      ::    soildata(:, :) 
  REAL(KIND=8), ALLOCATABLE      ::    aquiferdata(:, :) 
  REAL(KIND=8), ALLOCATABLE      ::    elevation(:, :)
  REAL(KIND=8), ALLOCATABLE      ::    temp_air(:, :)
  REAL(KIND=8), ALLOCATABLE      ::    windspeed(:)
  REAL(KIND=8), ALLOCATABLE      ::    humidity(:) 
  REAL(KIND=8), ALLOCATABLE      ::    prec_gauge(:,:)
  REAL(KIND=8), ALLOCATABLE      ::    prec_cell(:,:)   
  REAL(KIND=8), ALLOCATABLE      ::    coef_soil_runoff(:,:)  
  ! REAL(KIND=8), ALLOCATABLE      ::    surodir(:,:) 
  REAL(KIND=8), ALLOCATABLE      ::    surotime(:) 
  INTEGER, ALLOCATABLE           ::    upID(:,:)
  INTEGER, ALLOCATABLE           ::    rivID(:)  
  INTEGER              ::      num_plevel
  INTEGER              ::      num_riv
  !>cell based output: layer, time_len, num_cell, 
  TYPE(VarLand)      :: out_land
  TYPE(VarRch)       :: out_rch
  TYPE(VarHot)       :: out_hot

  REAL(KIND=8) :: prec_b
  REAL(KIND=8) :: thrf_b
  REAL(KIND=8) :: pet_b
  REAL(KIND=8) :: infilt_b
  REAL(KIND=8) :: rechg_b
  REAL(KIND=8) :: epc_b
  REAL(KIND=8) :: eps_b
  REAL(KIND=8) :: suro_b
  REAL(KIND=8) :: soro1_b
  REAL(KIND=8) :: soro2_b
  REAL(KIND=8) :: gwro_b
  REAL(KIND=8) :: surosat_b
  REAL(KIND=8) :: suroexc_b

  REAL(KIND=8),ALLOCATABLE  ::  runoff_surf     (:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  runoff_soil     (:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  recharge        (:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  infiltration    (:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  percolation     (:,:)
  REAL(KIND=8),ALLOCATABLE  ::  ep_soil         (:,:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  soilwater       (:,:,:) 
  REAL(KIND=8),ALLOCATABLE  ::  temp_soil       (:,:,:) 
  
  REAL(KIND=8), ALLOCATABLE ::  suro_sub(:,:)
  REAL(KIND=8), ALLOCATABLE ::  soro_sub(:,:)
  REAL(KIND=8), ALLOCATABLE ::  gwro_sub(:,:)  

  REAL(KIND=8), ALLOCATABLE   ::  flow_node(:)
  REAL(KIND=8), ALLOCATABLE   ::  sw_fc(:, :)    !<layer,soiltype          
  REAL(KIND=8), ALLOCATABLE   ::  sw_wp(:, :)              
  REAL(KIND=8), ALLOCATABLE   ::  sw_ep(:, :)              
  REAL(KIND=8), ALLOCATABLE   ::  sw_pv(:, :)              
  REAL(KIND=8), ALLOCATABLE   ::  ini_soilwater(:, :)
  REAL(KIND=8), ALLOCATABLE   ::  soilmem(:, :)            
  REAL(KIND=8), ALLOCATABLE   ::  epotdist(:, :)                                                               
  REAL(KIND=8), ALLOCATABLE   ::  coef_delay_perc(:,:)   

  TYPE(SUB2RIVTYPE),      ALLOCATABLE   ::    sub2riv(:)
  TYPE(AQUIFERTYPE),      ALLOCATABLE   ::    aquifer(:) 
  TYPE(RIVFLUXTYPE),      ALLOCATABLE   ::    riverflux(:)
  TYPE(RIVSTATETYPE),     ALLOCATABLE   ::    riverstate(:)
  TYPE(RIVERTYPE),        ALLOCATABLE   ::    river(:)


  REAL(KIND=8),PARAMETER      ::    bulkdensity = 1300. !<kg/m3
  REAL(KIND=8),PARAMETER      ::    soil_density = 2.65 !<g/cm3


  INTEGER,DIMENSION(12) :: level1=(/1,2,4,6,8,10,12,14,16,18,19,22/)
  INTEGER,DIMENSION(3) ::  level2=(/3,11,20/)
  INTEGER,DIMENSION(2) ::  level3=(/5,13/)
  INTEGER,DIMENSION(2) ::  level4=(/7,15/)
  INTEGER,DIMENSION(2) ::  level5=(/9,17/)
  INTEGER,DIMENSION(1) ::  level6=(/21/)
  INTEGER,DIMENSION(1) ::  level7=(/23/)
  INTEGER,DIMENSION(1) ::  level8=(/24/)


  REAL(KIND=8),ALLOCATABLE :: outpeo(:,:)
  REAL(KIND=8),ALLOCATABLE :: outanim(:,:)
  REAL(KIND=8),ALLOCATABLE :: outant(:,:)

END MODULE MODELVAR

