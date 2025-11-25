MODULE MODELVAR_ANT
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_short,          &
                                         c_int,            &
                                         c_long_long,      &
                                         c_float,          &
                                         c_double,         &
                                         c_float_complex,  &
                                         c_double_complex, &
                                         c_bool            
  USE MODELTYPE_ANT
  IMPLICIT NONE 
!##########################################################################################################
  !>antibiotic 
  INTEGER :: num_farm

  INTEGER, PARAMETER   ::      id_tc  = 1
  INTEGER, PARAMETER   ::      id_ctc = 2
  INTEGER, PARAMETER   ::      id_otc = 3
  INTEGER, PARAMETER   ::      id_dc  = 4
  
  TYPE(ANTUSAGETYPE),   ALLOCATABLE    ::    antusage(:)
  TYPE(ANTCROPDATATYPE),ALLOCATABLE    ::    antcropdata(:)
  TYPE(ANTBALANCETYPE), ALLOCATABLE    ::    antbalan(:)
  TYPE(FARMTYPE),       ALLOCATABLE    ::    farmdata(:)
  TYPE(ANTSUB2RIVTYPE), ALLOCATABLE    ::    antsub2riv(:)
  TYPE(ANTSTATETYPE),   ALLOCATABLE    ::    antistate(:)
  TYPE(ANTPOOLTYPE)  ::    antpool


  REAL(KIND=8), ALLOCATABLE            ::    popu(:,:)
  REAL(KIND=8), ALLOCATABLE            ::    iland_area(:,:)
  REAL(KIND=8), ALLOCATABLE            ::    peosource(:,:) !!<mg/km2/d
  REAL(KIND=8), ALLOCATABLE            ::    animsource(:,:)  !!<mg/km2/d num_cell,num_ant_type 
  REAL(KIND=8), ALLOCATABLE            ::    peo(:,:)
      
  INTEGER, ALLOCATABLE                 ::    people (:)
  INTEGER, ALLOCATABLE                 ::    farm (:)


  REAL(KIND=8), ALLOCATABLE ::  sulo_sub(:,:,:)   !!<num_ant_type,num_sub,sim_length, kg/m2
  REAL(KIND=8), ALLOCATABLE ::  solo_sub(:,:,:)   !!<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  gwlo_sub(:,:,:)   !!<num_ant_type,num_sub,sim_length  
  REAL(KIND=8), ALLOCATABLE ::  palo_sub(:,:,:)   !!<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  path_sub(:,:,:,:,:) !<num_ant_type,num_sub,sim_length,pathway,landuse
!---output------
  REAL(KIND=8), ALLOCATABLE ::  source_sub(:,:,:,:)!!<num_ant_type,num_sub,sim_length,type 
                                                  !!1<animal 2<people 3<air
  REAL(KIND=8), ALLOCATABLE ::  solrunoff_sub(:,:,:,:)!<num_ant_type,num_sub,sim_length,layer
  REAL(KIND=8), ALLOCATABLE ::  leach_sub(:,:,:)!<num_ant_type,num_sub,sim_length,layer
  REAL(KIND=8), ALLOCATABLE ::  surfer_sub(:,:,:)!<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  gw_sub(:,:,:)!<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  eroed_sub(:,:,:)!<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  init_sol(:,:,:)!num_ant_type,num_Cell,;layer
  REAL(KIND=8), ALLOCATABLE ::  end_sol(:,:,:)!num_ant_type,num_Cell,;layer
  REAL(KIND=8), ALLOCATABLE ::  decay_sol(:,:,:,:)!!num_ant_type,sim,num_Cell,;layer
  REAL(KIND=8), ALLOCATABLE ::  deg_sub(:,:,:,:) !<num_ant_type,num_sub,sim_length
  REAL(KIND=8), ALLOCATABLE ::  end_sub(:,:,:,:) 
  REAL(KIND=8), ALLOCATABLE ::  init_sub(:,:,:,:) 
  REAL(KIND=8), ALLOCATABLE ::  celllo(:,:,:,:) !!num_ant_type,num_cell,sim_length,5
                      !!>1>discharge 2>degradation 3>leaching 4>mixinglayer loss 5>soillayer loss
  REAL(KIND=8), ALLOCATABLE ::  cellloyr(:,:,:) !!num_ant_type,num_cell,5

  REAL(KIND=8), ALLOCATABLE ::  rchdeg(:,:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchburl(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchint(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchmass(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchstart(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchend(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchload(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rchout(:,:,:)
  REAL(KIND=8), ALLOCATABLE ::  rch1(:,:)

  INTEGER, PARAMETER :: ant_usage_file_unit     = 61
  INTEGER, PARAMETER :: ant_popu_file_unit      = 62
  INTEGER, PARAMETER :: ant_crop_file_unit      = 63
  INTEGER, PARAMETER :: people_file_unit        = 64
  INTEGER, PARAMETER :: farm_file_unit          = 65
  INTEGER, PARAMETER :: farmdata_file_unit      = 66
  INTEGER, PARAMETER :: par_ant_unit            = 67

  CHARACTER(LEN = *), PARAMETER :: ant_usage_file = 'dat/tyh_antuse.txt'
  CHARACTER(LEN = *), PARAMETER :: ant_popu_file  = 'dat/tyh_antpopu.txt'
  CHARACTER(LEN = *), PARAMETER :: ant_crop_file  = 'dat/tyh_antcrop.txt'
  CHARACTER(LEN = *), PARAMETER :: people_file    = 'dat/people.txt'
  CHARACTER(LEN = *), PARAMETER :: farm_file      = 'dat/tyh_farm.txt'
  CHARACTER(LEN = *), PARAMETER :: farmdata_file  = 'dat/tyh_farmdata.txt'
  CHARACTER(LEN = *), PARAMETER :: par_ant_file  = 'dat/tyh_par_ant.txt'

  REAL(KIND=8), ALLOCATABLE :: airdrydep(:) !!mg/km2/d
  REAL(KIND=8), ALLOCATABLE :: wetdep(:) !!ng/L

END MODULE MODELVAR_ANT


