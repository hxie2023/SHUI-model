MODULE MODELTYPE
  IMPLICIT NONE
!####################################################################################################################
  TYPE DateType
    INTEGER :: Year
    INTEGER :: Month 
    INTEGER :: Day
    INTEGER :: Hour 
    INTEGER :: Minute
  END TYPE DateType  
!####################################################################################################################
  TYPE SIMCFGTYPE
    LOGICAL               ::  prec_nc  = .FALSE.
    LOGICAL               ::  log      = .FALSE.
    LOGICAL               ::  hotstart = .FALSE.
    LOGICAL               ::  rch      = .FALSE.
    LOGICAL               ::  write_txt_rch  = .FALSE.
    LOGICAL               ::  write_nc_land  = .FALSE.
    LOGICAL               ::  write_nc_rch   = .FALSE.  
    LOGICAL               ::  record_state   = .FALSE. 
    LOGICAL               ::  sim_sed        = .FALSE. 
    LOGICAL               ::  sim_N          = .FALSE. 
    LOGICAL               ::  sim_P          = .FALSE. 
    LOGICAL               ::  sim_ant        = .FALSE. 
    INTEGER               ::  rf_model    !<0: default; 1: saturation-excess combined 
    INTEGER               ::  rf_route    !<0: default linear-reservoir; 1: route time
  END TYPE SIMCFGTYPE
!####################################################################################################################
  TYPE ParType
    CHARACTER(LEN = 15) :: parname       
    INTEGER             :: deptype  
    INTEGER             :: parid        
  END TYPE ParType
!####################################################################################################################
  ! TYPE WATERBALANCE
  !   REAL(KIND=8) :: prec = 0.
  !   REAL(KIND=8) :: thrf = 0.
  !   REAL(KIND=8) :: evap_c = 0.
  !   REAL(KIND=8) :: evap_s = 0.
  !   REAL(KIND=8) :: recharge = 0.
  !   REAL(KIND=8) :: suro = 0.
  !   REAL(KIND=8) :: suro_sat = 0.
  !   REAL(KIND=8) :: suro_exc = 0.
  !   REAL(KIND=8) :: soro = 0.
  !   REAL(KIND=8) :: gwro = 0.
  ! END TYPE WATERBALANCE
!####################################################################################################################
  TYPE SOILSTATETYPE
    REAL(KIND=8), ALLOCATABLE :: water  (:,:)      
    REAL(KIND=8), ALLOCATABLE :: temp   (:,:)       
    REAL(KIND=8), ALLOCATABLE :: deeptem(:)   
    REAL(KIND=8), ALLOCATABLE :: conc   (:,:,:)  
  END TYPE SOILSTATETYPE
!####################################################################################################################
  TYPE AQSTATETYPE
    REAL(KIND=8)               :: water  
    REAL(KIND=8), ALLOCATABLE  :: conc(:)
  END TYPE AQSTATETYPE
!####################################################################################################################
  TYPE AQUIFERTYPE
    !>含水层属性
    REAL(KIND=8)    :: area                
    REAL(KIND=8)    :: iniwater            
    REAL(KIND=8)    :: porosity            
    REAL(KIND=8)    :: maxvol              
    REAL(KIND=8)    :: basedepth           
    REAL(KIND=8)    :: passivedep          
    REAL(KIND=8)    :: reference           
    REAL(KIND=8)    :: inivol              
    REAL(KIND=8)    :: temperature = 0.    
    REAL(KIND=8)    :: conc_IN = 4.        
    REAL(KIND=8)    :: conc_SP = 0.1       
    REAL(KIND=8)    :: FLO_MIN      
  END TYPE AQUIFERTYPE
!####################################################################################################################
  TYPE RIVSTATETYPE
    REAL(KIND=8)              ::   Q(1:2) !<Q(1): previous time 0, inflow 0 
                                          !<Q(2): previous time 0, outflow 0
    REAL(KIND=8)              ::   temp
    REAL(KIND=8)              ::   temp10
    REAL(KIND=8)              ::   temp20
  END TYPE RIVSTATETYPE
!####################################################################################################################

!####################################################################################################################
  TYPE RIVFLUXTYPE
    !>river flux
    REAL(KIND=8)              ::   VOL(0:1) 
    REAL(KIND=8),ALLOCATABLE  ::   flow(:) 
    REAL(KIND=8)              ::   vel           !<average velocity [m/s]     
    REAL(KIND=8)              ::   q_up(0:1)    
    REAL(KIND=8)              ::   VOL0
  END TYPE RIVFLUXTYPE
!####################################################################################################################
  TYPE RIVERTYPE  
    !>river attributes
    INTEGER   ::   nodeB
    INTEGER   ::   nodeE
    INTEGER   ::   downID
    INTEGER   ::   gaugeID
    INTEGER   ::   plevel
    REAL(KIND=8)      ::   length
    REAL(KIND=8)      ::   width
    REAL(KIND=8)      ::   slope
    REAL(KIND=8)      ::   rough
    REAL(KIND=8)      ::   inidepth
    REAL(KIND=8)      ::   vol
    REAL(KIND=8)      ::   maxdepth
    REAL(KIND=8)      ::   mindepth
    REAL(KIND=8)      ::   maxvol
    REAL(KIND=8)      ::   minvol
  END TYPE RIVERTYPE
!####################################################################################################################
  TYPE LAND2SUBTYPE
    REAL(KIND=8), ALLOCATABLE  ::   suro(:)
    REAL(KIND=8), ALLOCATABLE  ::   soro(:)
    REAL(KIND=8), ALLOCATABLE  ::   conc(:, :)  
  END TYPE LAND2SUBTYPE
!####################################################################################################################
  TYPE SUB2RIVTYPE
    REAL(KIND=8), ALLOCATABLE  ::   flow(:)     
    REAL(KIND=8), ALLOCATABLE  ::   suflow(:)
    REAL(KIND=8), ALLOCATABLE  ::   soflow(:)
    REAL(KIND=8), ALLOCATABLE  ::   gwflow(:)
    REAL(KIND=8), ALLOCATABLE  ::   conc(:,:)
    REAL(KIND=8), ALLOCATABLE  ::   erosion(:)     !<kg/d  
    REAL(KIND=8), ALLOCATABLE  ::   sed(:,:,:)     !<clay,silt,sand;surf,soilrunoff;idt
    REAL(KIND=8), ALLOCATABLE  ::   erosion_c(:)   !<kg/d clay
    REAL(KIND=8), ALLOCATABLE  ::   erosion_l(:)   !<kg/d silt
    REAL(KIND=8), ALLOCATABLE  ::   erosion_s(:)   !<kg/d sand 
  END TYPE SUB2RIVTYPE
!####################################################################################################################
  TYPE JulianDateType
    REAL(KIND=8) :: Head, Tail
  END TYPE JulianDateType
!####################################################################################################################
  TYPE TIMEINFORMATIONTYPE
    TYPE(DateType) date 
    INTEGER :: dayno           
    INTEGER :: prevdoy         
    INTEGER :: tsofday          
    INTEGER :: dayofweek       
    INTEGER :: m = 0           
    INTEGER :: d = 0           
    INTEGER :: end_m           
    INTEGER :: day_m = 0       
    INTEGER :: yrc = 0         
    INTEGER :: yrs = 0         
    INTEGER :: day_mo = 0      
    INTEGER :: end_mo = 0      
    INTEGER :: end_yr = 0      
    INTEGER :: end_sim = 0     
    INTEGER :: end_aa_prt = 0  
    INTEGER :: day_start = 0   
    INTEGER :: day_end_yr = 0  
    INTEGER :: day_end = 0     
    INTEGER :: nbyr = 0        
    REAL    :: dtm = 0.         
    REAL    :: days_prt = 0.    
    INTEGER :: yrs_prt = 0.     
    INTEGER :: yrs_prt_int = 0. 
    INTEGER :: num_leap = 0     
    INTEGER :: prt_int_cur = 1 
    INTEGER :: yrc_tot = 0
  END TYPE TIMEINFORMATIONTYPE
!####################################################################################################################
  TYPE VarInfo
    REAL(KIND=8), ALLOCATABLE  ::   value1(:)
    REAL(KIND=8), ALLOCATABLE  ::   value2(:,:)
    REAL(KIND=8), ALLOCATABLE  ::   value3(:,:,:)
    CHARACTER(LEN=20)          ::   name    
  END TYPE VarInfo
!####################################################################################################################
  TYPE VarLand
    TYPE(VarInfo)  :: runoff_surf
    TYPE(VarInfo)  :: infiltration
    TYPE(VarInfo)  :: runoff_soil
    TYPE(VarInfo)  :: recharge
    TYPE(VarInfo)  :: ep_soil
    TYPE(VarInfo)  :: soil_water
    TYPE(VarInfo)  :: soil_temp
    TYPE(VarInfo)  :: sub_suflow
    TYPE(VarInfo)  :: sub_soflow
    TYPE(VarInfo)  :: sub_gwflow
  END TYPE VarLand
!####################################################################################################################
  TYPE VarRch
    TYPE(VarInfo)  :: rch_flow
    TYPE(VarInfo)  :: rch_vol
  END TYPE VarRch
!####################################################################################################################
  TYPE VarHot
    TYPE(VarInfo)  :: icept_storage
    TYPE(VarInfo)  :: soil_water
    TYPE(VarInfo)  :: soil_temp
    TYPE(VarInfo)  :: soil_deeptemp
    TYPE(VarInfo)  :: recharge
    TYPE(VarInfo)  :: runoff_gw
    TYPE(VarInfo)  :: aquifer_water
    TYPE(VarInfo)  :: infiltration
    TYPE(VarInfo)  :: infilt_day_old
    TYPE(VarInfo)  :: infilt_rate_old
    TYPE(VarInfo)  :: sub_suflow
    TYPE(VarInfo)  :: sub_soflow
    TYPE(VarInfo)  :: sub_gwflow
    TYPE(VarInfo)  :: rivflux_V1
    TYPE(VarInfo)  :: rivstate_Q1
    TYPE(VarInfo)  :: rivstate_Q2
    TYPE(VarInfo)  :: q_up
  END TYPE VarHot
!####################################################################################################################
  TYPE pair
    INTEGER  ::  x
    INTEGER  ::  y
  END TYPE pair
!####################################################################################################################
! !####################################################################################################################
!   TYPE CROPDATATYPE 
!     INTEGER         :: luid
!     REAL(KIND=8)    :: part          !<Propotion of coverage on the cell (-)
!     REAL(KIND=8)    :: fertnamount1  !<fertiliser amount N (kg/km2)
!     REAL(KIND=8)    :: fertpamount1  !<fertiliser amount P (kg/km2)
!     INTEGER         :: fertday1      !<day for fertilizing 1 (dayno)
!     REAL(KIND=8)    :: fertdown1     !<part of fertilizer amount ploughed down to soillayer 2
!     REAL(KIND=8)    :: mannamount1   !<manure amount N (kg/km2)
!     REAL(KIND=8)    :: manpamount1   !<manure amount P (kg/km2)
!     INTEGER         :: manday1       !<day for manureing 1 (dayno)
!     REAL(KIND=8)    :: mandown1      !<part of manure amount ploughed down to soillayer 2
!     REAL(KIND=8)    :: fertnamount2  !<fertiliser amount N (kg/km2)
!     REAL(KIND=8)    :: fertpamount2  !<fertiliser amount P (kg/km2)
!     INTEGER         :: fertday2      !<day for fertilizing 2 (dayno)
!     REAL(KIND=8)    :: fertdown2     !<part of fertilizer amount ploughed down to soillayer 2
!     REAL(KIND=8)    :: mannamount2   !<manure amount N (kg/km2)
!     REAL(KIND=8)    :: manpamount2   !<manure amount P (kg/km2)
!     INTEGER         :: manday2       !<day for manureing 2 (dayno)
!     REAL(KIND=8)    :: mandown2      !<part of manure amount ploughed down to soillayer 2
!     REAL(KIND=8)    :: resnamount    !<plant residual amount of N (kg/km2/yr)
!     REAL(KIND=8)    :: respamount    !<plant residual amount of P (kg/km2/yr)
!     REAL(KIND=8)    :: rescamount    !<plant resudual (litterfall) of C (kg/km2/yr)
!     INTEGER         :: resdayno      !<day for residual (dayno)
!     REAL(KIND=8)    :: resdown       !<part of residual amount ploughed down to soillayer 2
!     REAL(KIND=8)    :: resfast       !<part of residual amount to fast pool (rest to humus)
!     REAL(KIND=8)    :: uptake1       !<parameter 1 for plant uptake of nutrient 
!     REAL(KIND=8)    :: uptake2       !<parameter 2 for plant uptake of nutrient
!     REAL(KIND=8)    :: uptake3       !<parameter 3 for plant uptake of nutrient
!     REAL(KIND=8)    :: uptakeupper   !<fraction of plant uptake in upper soil layer
!     REAL(KIND=8)    :: PNuptakeRatio !<phosphorus plant uptake as a factor of nitrogen uptake
!     INTEGER         :: baredayno1    !<day for beginning of first period with bare soil (dayno), typically 1
!     INTEGER         :: baredayno2    !<day for end of first period with bare soil (dayno), typically sawing date + a few days
!     INTEGER         :: baredayno3    !<day for beginning of second period with bare soil (dayno), typically ploughing date
!     INTEGER         :: baredayno4    !<day for end of second period with bare soil (dayno), typically 365
!     INTEGER         :: baredayno5    !<day for end of second period with bare soil (dayno), typically saw date of autumn crop
!     REAL(KIND=8)    :: ccmax1        !<crop cover during summer
!     REAL(KIND=8)    :: ccmax2        !<crop cover during winter and all year for year-round-crops (e.g. forest)
!     REAL(KIND=8)    :: gcmax1        !<ground cover during summer
!     REAL(KIND=8)    :: gcmax2        !<ground cover during winter and all year for year-round-crops (e.g. forest)
!     REAL(KIND=8)    :: gddsow        !<accumulated growing degree days needed for sowing
!     REAL(KIND=8)    :: daylength     !<minimum daylength required for start to accumulate gdd (hours)
!     REAL(KIND=8)    :: basetemp      !<temperature which is deducted from air temperature when calculating gdd (degrees)
!     INTEGER         :: firstday      !<first possible day for gdd accumulation (dayno)
!     REAL(KIND=8)    :: t1amount      !<amount T1 (kg/km2)       
!     INTEGER         :: t1year        !<year of T1 application      
!     INTEGER         :: t1day         !<first day of T1 application
!     INTEGER         :: t1numberofdays!<number of days with T1 application       
!     INTEGER         :: t1daydown     !<day of T1 incorporation 
!     REAL(KIND=8)    :: t1down1       !<part of T1 ploughed down to soillayer 1
!     REAL(KIND=8)    :: t1down2       !<part of T1 ploughed down to soillayer 2
!     REAL(KIND=8)    :: phmax1        !<max plant height in summer (m)
!     REAL(KIND=8)    :: phmax2        !<max plant height in winter (m)
!   END TYPE CROPDATATYPE
! !####################################################################################################################


END MODULE MODELTYPE
