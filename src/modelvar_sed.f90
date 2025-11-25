MODULE MODELVAR_SED
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_short,          &
                                         c_int,            &
                                         c_long_long,      &
                                         c_float,          &
                                         c_double,         &
                                         c_float_complex,  &
                                         c_double_complex, &
                                         c_bool            
  USE MODELTYPE_SED
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


  INTEGER, PARAMETER :: tyh_sed_unit   = 51

  CHARACTER(LEN = *), PARAMETER :: tyh_sed_file       = 'dat/tyh_par_sed.txt'

  
  REAL(KIND=8)  ::    vs_sand
  REAL(KIND=8)  ::    vs_silt
  REAL(KIND=8)  ::    vs_clay 

  REAL(KIND=8),PARAMETER  ::    sediment_density  = 2.65  !<g/cm3
  REAL(KIND=8),PARAMETER  ::    flow_density  = 1100      !<kg/m3 
  REAL(KIND=8),PARAMETER  ::    g_acc         = 9.81      !<m/s2
  REAL(KIND=8),PARAMETER  ::    eta           = 0.0015    !<kg/m/s
  REAL(KIND=8),PARAMETER  ::    sand_size     = 0.08E-3   !<m, 100 weimi
  REAL(KIND=8),PARAMETER  ::    silt_size     = 0.02E-3   !<m, 30 weimi
  REAL(KIND=8),PARAMETER  ::    clay_size     = 0.001E-3  !<m, 1 weimi
  REAL(KIND=8),PARAMETER  ::    d_reference   = 0.005     !<0.005 for unchanneled flow, 0.01 for shallow rills, 
                                                          !and 0.25 for deeper rills
  REAL(KIND=8),PARAMETER  ::    n_reference   = 0.015     !<reference manning's roughness, for a standard surface condition 
  

  INTEGER, ALLOCATABLE           ::    surocell(:,:)
  !>erosion 
  REAL(KIND=8), ALLOCATABLE      ::    dep_pc(:,:,:)
  REAL(KIND=8), ALLOCATABLE      ::    erod_tc(:,:,:)
  REAL(KIND=8), ALLOCATABLE      ::    erod_g(:,:,:)
  REAL(KIND=8), ALLOCATABLE      ::    relsed(:,:,:)

  REAL(KIND=8), ALLOCATABLE ::  paro_sub(:,:)  !!clay+silt+sand kg/m2
  REAL(KIND=8), ALLOCATABLE ::  clro_sub(:,:,:)  !!clay erosion kg/m2
  REAL(KIND=8), ALLOCATABLE ::  slro_sub(:,:,:)  !!silt erosion kg/m2
  REAL(KIND=8), ALLOCATABLE ::  snro_sub(:,:,:)  !!sand erosion kg/m2

  TYPE(CROPDATATYPE), ALLOCATABLE ::   cropdata(:)
  TYPE(SEDSTATETYPE), ALLOCATABLE ::   sedstate(:)
  TYPE(SEDRESPTYPE),  ALLOCATABLE ::   sedresp(:)
  TYPE(SEDFLUXTYPE),  ALLOCATABLE ::   sedflux(:)

  INTEGER, PARAMETER :: resp_unit    = 50
  CHARACTER(LEN = *), PARAMETER :: resp_file   = 'dat/tau.txt'

  REAL(KIND=8),DIMENSION(3,7) :: crunoff(3,7)=reshape([10.,10.,10.,10.,10.,10.,0.,&
                                                       10.,10.,10.,10.,10.,10.,0.,&
                                                       10.,10.,10.,10.,10.,10.,0.],[3,7])!<mg/L SS(clay,silt,sand) concentration soil runoff   
  
                              
END MODULE MODELVAR_SED


