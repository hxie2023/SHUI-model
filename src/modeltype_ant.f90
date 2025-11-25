MODULE  MODELTYPE_ANT
  IMPLICIT NONE
!####################################################################################################################
  TYPE ANTUSAGETYPE 
    REAL(KIND=8)    :: Uhuman               !�?< unit usage of human(g DDD)
    REAL(KIND=8)    :: Ehuman               !�?< human excretion
    REAL(KIND=8)    :: Rwwtp                !�?< removal efficicency of wwtp
    REAL(KIND=8)    :: Rferment             !�?< removal efficicency of fermentation
    REAL(KIND=8)    :: Upig                 !�?< unit usage of pig (g DDD)
    REAL(KIND=8)    :: Epig                 !�?< pig excretion
    REAL(KIND=8)    :: Uchicken             !�?< unit usage of chicken (mg DDD)
    REAL(KIND=8)    :: Echicken             !�?< chicken excretion
    REAL(KIND=8)    :: DTsoil               !�?< Degradation half-life of the chemical in soil 
    REAL(KIND=8)    :: DTwater              !�?< Degradation half-life of the chemical in water(days) 
    REAL(KIND=8)    :: Kpwater              !�?< partition coefficient (m3/g)
    REAL(KIND=8)    :: Kow                  !�?< octanol-water partition coefficient
    REAL(KIND=8)    :: molwt                !�?< molecular weight of antibiotics
    REAL(KIND=8)    :: DTmanure             !�?< 
    REAL(KIND=8)    :: kpsoil               !�?< the soil adsorption coefficient ((mg/kg)/(mg/L)
    REAL(KIND=8)    :: kd_c                 
    REAL(KIND=8)    :: kd_l                  
    REAL(KIND=8)    :: kd_s 
    REAL(KIND=8)    :: kpsed                  !! L/mg kp in bedsediment 
    REAL(KIND=8)    :: DTsediment
    REAL(KIND=8)    :: wsol                !! Solubility of the chemical in water (mg/L)
            
  END TYPE ANTUSAGETYPE
!####################################################################################################################
  TYPE ANTCROPDATATYPE 
    INTEGER    :: luid
    REAL(KIND=8)    :: part  
    REAL(KIND=8)    :: ferttcamount1  !<fertiliser amount otc (kg/km2)
    REAL(KIND=8)    :: fertctcamount1  !<fertiliser amount ctc (kg/km2)
    REAL(KIND=8)    :: fertotcamount1  !<fertiliser amount otc (kg/km2)
    REAL(KIND=8)    :: fertdcamount1  !<fertiliser amount dc (kg/km2)
    INTEGER         :: fertday1      !<day for fertilizing 1 (dayno)
    REAL(KIND=8)    :: fertdown1     !<part of fertilizer amount ploughed down to soillayer 2
    REAL(KIND=8)    :: mantcamount1   !<manure amount TC (kg/km2)
    REAL(KIND=8)    :: manctcamount1   !<manure amount CTC (kg/km2)
    REAL(KIND=8)    :: manotcamount1   !<manure amount OTC (kg/km2)
    REAL(KIND=8)    :: mandcamount1   !<manure amount DC (kg/km2)
    INTEGER         :: manday1       !<day for manureing 1 (dayno)
    REAL(KIND=8)    :: mandown1      !<part of manure amount ploughed down to soillayer 2
    REAL(KIND=8)    :: ferttcamount2  !<fertiliser amount otc (kg/km2)
    REAL(KIND=8)    :: fertctcamount2  !<fertiliser amount ctc (kg/km2)
    REAL(KIND=8)    :: fertotcamount2  !<fertiliser amount otc (kg/km2)
    REAL(KIND=8)    :: fertdcamount2  !<fertiliser amount dc (kg/km2)
    INTEGER         :: fertday2      !<day for fertilizing 2 (dayno)
    REAL(KIND=8)    :: fertdown2     !<part of fertilizer amount ploughed down to soillayer 2
    REAL(KIND=8)    :: mantcamount2   !<manure amount TC (kg/km2)
    REAL(KIND=8)    :: manctcamount2   !<manure amount CTC (kg/km2)
    REAL(KIND=8)    :: manotcamount2   !<manure amount OTC (kg/km2)
    REAL(KIND=8)    :: mandcamount2   !<manure amount DC (kg/km2)
    INTEGER         :: manday2       !<day for manureing 2 (dayno)
    REAL(KIND=8)    :: mandown2      !<part of manure amount ploughed down to soillayer 2
    REAL(KIND=8)    :: restcamount    !<plant residual amount of TC (kg/km2/yr)
    REAL(KIND=8)    :: resctcamount    !<plant residual amount of CTC (kg/km2/yr)
    REAL(KIND=8)    :: resotcamount    !<plant residual amount of OTC (kg/km2/yr)
    REAL(KIND=8)    :: resdcamount    !<plant residual amount of DC (kg/km2/yr)
    INTEGER         :: resdayno      !<day for residual (dayno)
    REAL(KIND=8)    :: resdown       !<part of residual amount ploughed down to soillayer 2
    REAL(KIND=8)    :: resfast       !<part of residual amount to fast pool (rest to humus)
    REAL(KIND=8)    :: uptake1       !<parameter 1 for plant uptake of nutrient !TODO: these could be an array
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
    REAL(KIND=8)    :: phmax1
    REAL(KIND=8)    :: phmax2
  END TYPE ANTCROPDATATYPE
!####################################################################################################################

!####################################################################################################################
  TYPE ANTBALANCETYPE
    REAL(KIND=8)    :: decay_p  ! antibiotics decayed on soil particles
    REAL(KIND=8)    :: decay_s  ! antibiotics decayed on soilwater
    REAL(KIND=8)    :: decay_a  ! antibiotics decayed on aquifer
    REAL(KIND=8)    :: decay_f  ! antibiotics decayed on ferment
    REAL(KIND=8)    :: decay_0  !
  END TYPE ANTBALANCETYPE
!####################################################################################################################
!####################################################################################################################
  TYPE FARMTYPE 
  INTEGER           :: number_pig                 !number of animal pig equivalent
  INTEGER           :: number_chicken             !number of animal chicken equivalent
  INTEGER           :: species                    !species of animals 1:broiler chicken 2:layer hen
                                                       !3:goose 4:sheep
  INTEGER           :: scale                      !1:farm 2:family farm
  END TYPE FARMTYPE
!####################################################################################################################    
  TYPE ANTSUB2RIVTYPE
  REAL(KIND=8), ALLOCATABLE  ::   suload(:,:)   !!<kg/d
  REAL(KIND=8), ALLOCATABLE  ::   soload(:,:)   !!<kg/d
  REAL(KIND=8), ALLOCATABLE  ::   gwload(:,:)   !!<kg/d
  REAL(KIND=8), ALLOCATABLE  ::   paload(:,:)   !!<kg/d antibiotic
  REAL(KIND=8), ALLOCATABLE  ::   load(:,:)     !! num_ant_type,sim_length antibiotic
  END TYPE ANTSUB2RIVTYPE

  TYPE ANTSTATETYPE
      !dissolved antibiotic in reach
  REAL(KIND=8),ALLOCATABLE     ::   conc(:)   !<num_ant_type  mg/L
  !particulate antibiotic in reach
  REAL(KIND=8),ALLOCATABLE     ::   parconc(:) !<particulate antibiotic concentration mg/kg
  !particulate antibiotic in bed sediment
  REAL(KIND=8),ALLOCATABLE     ::   bedconc(:) !<bed sediment antibiotic conc mg/kg
  !dissolved antibiotic in bed sediment
  REAL(KIND=8),ALLOCATABLE     ::   outant(:,:)
  REAL(KIND=8),ALLOCATABLE     ::   outant_p(:)
  REAL(KIND=8),ALLOCATABLE     ::   outant_d(:)
  REAL(KIND=8),ALLOCATABLE     ::   outant_p1(:,:)
  REAL(KIND=8),ALLOCATABLE     ::   outant_d1(:,:)
  REAL(KIND=8),ALLOCATABLE     ::   spsantmass(:,:) !! mg sorbed antibiotic pool in reach
  REAL(KIND=8),ALLOCATABLE     ::   disantmass(:) !! mg dissolved antibiotic pool  in reach
  REAL(KIND=8),ALLOCATABLE     ::   chantmass(:)  !! mg
  REAL(KIND=8),ALLOCATABLE     ::   sedantmass(:) !! mg
  REAL(KIND=8),ALLOCATABLE     ::   srbconc(:,:)  !! mg/kg (num_ant_type, sed)
  !------------------output--------------------------
  REAL(KIND=8),ALLOCATABLE     ::   deg(:,:,:)
  REAL(KIND=8),ALLOCATABLE     ::   bural(:,:)
  REAL(KIND=8),ALLOCATABLE     ::   mass(:,:,:)
  REAL(KIND=8),ALLOCATABLE     ::   load(:,:)
  REAL(KIND=8),ALLOCATABLE     ::   antconc(:,:)

  REAL(KIND=8),ALLOCATABLE     ::   antex(:,:,:,:)
  REAL(KIND=8),ALLOCATABLE     ::   antdif(:,:)
  END TYPE ANTSTATETYPE

  TYPE ANTPOOlTYPE

   REAL(KIND=8),ALLOCATABLE   ::    m_dissol(:,:)    !!mg/km2 dissolved phases of the Mobile pool
   REAL(KIND=8),ALLOCATABLE   ::    m_sorbed(:,:)    !!mg/km2 sorbed phases of the Mobile pool
   REAL(KIND=8),ALLOCATABLE   ::    im_dissol(:,:)   !!mg/km2 dissolved phases of the IMobile pool
   REAL(KIND=8),ALLOCATABLE   ::    im_sorbed(:,:)   !!mg/km2 sorbed phases of the IMobile pool
   REAL(KIND=8),ALLOCATABLE   ::    conc_mdis(:,:) !!mg/m3 the concentration of ant in the Dissolved phase of the Mobile or Immobile pool 
   REAL(KIND=8),ALLOCATABLE   ::    conc_imsor(:,:) !!mg/kg the concentration of ant in the sorbed phase of the Mobile or Immobile pool
   REAL(KIND=8),ALLOCATABLE   ::    conc_imdis(:,:) !!mg/m3 the concentration of ant in the Dissolved phase of the Mobile or Immobile pool 
   REAL(KIND=8),ALLOCATABLE   ::    conc_msor(:,:) !!mg/kg the concentration of ant in the sorbed phase of the Mobile or Immobile pool
   REAL(KIND=8),ALLOCATABLE   ::    m_antpool(:,:) !!mg/kg 
  END TYPE ANTPOOLTYPE



END MODULE MODELTYPE_ANT