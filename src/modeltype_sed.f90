MODULE MODELTYPE_SED
  IMPLICIT NONE
!####################################################################################################################
  TYPE SEDSTATETYPE
  !>River water suspended sediment
  REAL(KIND=8)                 ::   sedconc     !<mg/L
  REAL(KIND=8)                 ::   clayconc    !<mg/L
  REAL(KIND=8)                 ::   siltconc    !<mg/L
  REAL(KIND=8)                 ::   sandconc    !<mg/L
  REAL(KIND=8)                 ::   sedpool     !<kg, suspended sediment pool
  REAL(KIND=8)                 ::   claypool    !kg
  REAL(KIND=8)                 ::   siltpool    !kg
  REAL(KIND=8)                 ::   sandpool    !kg
  !>Riverbed sediment              
  REAL(KIND=8)                 ::   sedbed      !<kg, sediment in river bed
  REAL(KIND=8)                 ::   claybed     !<kg 
  REAL(KIND=8)                 ::   siltbed     !<kg
  REAL(KIND=8)                 ::   sandbed     !<kg
  REAL(KIND=8),ALLOCATABLE     ::   sedload(:)  !<kg,length=4,1:clay;2:silt;3:sand;4:sum
  END TYPE SEDSTATETYPE
!####################################################################################################################
  TYPE SEDFLUXTYPE !<Primarily for print
    REAL(KIND=8),ALLOCATABLE   ::   sedload(:,:)    !<kg,length=4,1:clay;2:silt;3:sand;4:sum, river+sub2riv
    REAL(KIND=8),ALLOCATABLE   ::   spsconc(:,:)    !<mg/L,length=4,1:clay;2:silt;3:sand;4:sum
    REAL(KIND=8),ALLOCATABLE   ::   sedresus(:,:)   !<kg,length=4,1:clay;2:silt;3:sand;4:sum
    REAL(KIND=8),ALLOCATABLE   ::   seddep(:,:)     !<kg,length=4,1:clay;2:silt;3:sand;4:sum
  END TYPE SEDFLUXTYPE
!####################################################################################################################
  TYPE CROPDATATYPE 
    INTEGER         :: luid
    REAL(KIND=8)    :: part          !<Propotion of coverage on the cell (-)
    REAL(KIND=8)    :: fertnamount1  !<fertiliser amount N (kg/km2)
    REAL(KIND=8)    :: fertpamount1  !<fertiliser amount P (kg/km2)
    INTEGER         :: fertday1      !<day for fertilizing 1 (dayno)
    REAL(KIND=8)    :: fertdown1     !<part of fertilizer amount ploughed down to soillayer 2
    REAL(KIND=8)    :: mannamount1   !<manure amount N (kg/km2)
    REAL(KIND=8)    :: manpamount1   !<manure amount P (kg/km2)
    INTEGER         :: manday1       !<day for manureing 1 (dayno)
    REAL(KIND=8)    :: mandown1      !<part of manure amount ploughed down to soillayer 2
    REAL(KIND=8)    :: fertnamount2  !<fertiliser amount N (kg/km2)
    REAL(KIND=8)    :: fertpamount2  !<fertiliser amount P (kg/km2)
    INTEGER         :: fertday2      !<day for fertilizing 2 (dayno)
    REAL(KIND=8)    :: fertdown2     !<part of fertilizer amount ploughed down to soillayer 2
    REAL(KIND=8)    :: mannamount2   !<manure amount N (kg/km2)
    REAL(KIND=8)    :: manpamount2   !<manure amount P (kg/km2)
    INTEGER         :: manday2       !<day for manureing 2 (dayno)
    REAL(KIND=8)    :: mandown2      !<part of manure amount ploughed down to soillayer 2
    REAL(KIND=8)    :: resnamount    !<plant residual amount of N (kg/km2/yr)
    REAL(KIND=8)    :: respamount    !<plant residual amount of P (kg/km2/yr)
    REAL(KIND=8)    :: rescamount    !<plant resudual (litterfall) of C (kg/km2/yr)
    INTEGER         :: resdayno      !<day for residual (dayno)
    REAL(KIND=8)    :: resdown       !<part of residual amount ploughed down to soillayer 2
    REAL(KIND=8)    :: resfast       !<part of residual amount to fast pool (rest to humus)
    REAL(KIND=8)    :: uptake1       !<parameter 1 for plant uptake of nutrient 
    REAL(KIND=8)    :: uptake2       !<parameter 2 for plant uptake of nutrient
    REAL(KIND=8)    :: uptake3       !<parameter 3 for plant uptake of nutrient
    REAL(KIND=8)    :: uptakeupper   !<fraction of plant uptake in upper soil layer
    REAL(KIND=8)    :: PNuptakeRatio !<phosphorus plant uptake as a factor of nitrogen uptake
    INTEGER         :: baredayno1    !<day for beginning of first period with bare soil (dayno), typically 1
    INTEGER         :: baredayno2    !<day for end of first period with bare soil (dayno), typically sawing date + a few days
    INTEGER         :: baredayno3    !<day for beginning of second period with bare soil (dayno), typically ploughing date
    INTEGER         :: baredayno4    !<day for end of second period with bare soil (dayno), typically 365
    INTEGER         :: baredayno5    !<day for end of second period with bare soil (dayno), typically saw date of autumn crop
    REAL(KIND=8)    :: ccmax1        !<crop cover during summer
    REAL(KIND=8)    :: ccmax2        !<crop cover during winter and all year for year-round-crops (e.g. forest)
    REAL(KIND=8)    :: gcmax1        !<ground cover during summer
    REAL(KIND=8)    :: gcmax2        !<ground cover during winter and all year for year-round-crops (e.g. forest)
    REAL(KIND=8)    :: gddsow        !<accumulated growing degree days needed for sowing
    REAL(KIND=8)    :: daylength     !<minimum daylength required for start to accumulate gdd (hours)
    REAL(KIND=8)    :: basetemp      !<temperature which is deducted from air temperature when calculating gdd (degrees)
    INTEGER         :: firstday      !<first possible day for gdd accumulation (dayno)
    REAL(KIND=8)    :: t1amount      !<amount T1 (kg/km2)       
    INTEGER         :: t1year        !<year of T1 application      
    INTEGER         :: t1day         !<first day of T1 application
    INTEGER         :: t1numberofdays!<number of days with T1 application       
    INTEGER         :: t1daydown     !<day of T1 incorporation 
    REAL(KIND=8)    :: t1down1       !<part of T1 ploughed down to soillayer 1
    REAL(KIND=8)    :: t1down2       !<part of T1 ploughed down to soillayer 2
    REAL(KIND=8)    :: phmax1        !<max plant height in summer (m)
    REAL(KIND=8)    :: phmax2        !<max plant height in winter (m)
  END TYPE CROPDATATYPE
!####################################################################################################################
  TYPE SEDRESPTYPE
  REAL(KIND=8)    :: claycd
  REAL(KIND=8)    :: claycs
  REAL(KIND=8)    :: mclay
  REAL(KIND=8)    :: siltcd
  REAL(KIND=8)    :: siltcs
  REAL(KIND=8)    :: msilt
  REAL(KIND=8)    :: ks
  END TYPE SEDRESPTYPE
!####################################################################################################################

END MODULE MODELTYPE_SED
