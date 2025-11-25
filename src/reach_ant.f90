MODULE REACH_ANT
  IMPLICIT NONE
CONTAINS
  !#################################################################################################################
  SUBROUTINE routing_ant(isub,tss,sulo,solo,palo,gwlo,asub_to_riv)
    USE MODELVAR, ONLY : num_cell,    &
                         num_sub,     &
                         subbasin,    &
                         landuse,     &
                         soiltype,    &
                         sim_length,  &
                         cell_area,   &
                         num_ant_type

    USE MODELTYPE_ANT
    USE MODELPAR
    INTEGER :: c
    INTEGER :: b
    INTEGER, INTENT(IN)                ::    isub
    INTEGER, INTENT(IN)                ::    tss 
    REAL(KIND=8), INTENT(IN)           ::    sulo(num_ant_type,sim_length)  !>kg/m2    
    REAL(KIND=8), INTENT(IN)           ::    solo(num_ant_type,sim_length)  !>kg/m2
    REAL(KIND=8), INTENT(IN)           ::    gwlo(num_ant_type,sim_length)  !>kg/m2
    REAL(KIND=8), INTENT(IN)           ::    palo(num_ant_type,sim_length)   !kg/m2  
    TYPE(ANTSUB2RIVTYPE), INTENT(INOUT)   ::    asub_to_riv

    
    !---------------
    INTEGER i,ic ,count,cell,ib,idt
    REAL(KIND=8)    ::  suro_conf,soro_conf
    REAL(KIND=8)    ::  help_t1_l(num_ant_type)
    REAL(KIND=8)    ::  help_t1(num_ant_type)
    REAL(KIND=8)    ::  suload_t0(num_ant_type),soload_t0(num_ant_type),paload_t0(num_ant_type)
    REAL(KIND=8)    ::  precipitation
    ! suro_conf = 0.2168
    ! soro_conf = 0.4752
    
    suro_conf = par_general(genid_suroconf)
    soro_conf = par_general(genid_soroconf)

    DO idt = 1, tss
      ! precipitation=prec_gauge(idt,isub)
      IF(idt.EQ.1) THEN 
        soload_t0=0.
      ELSE
        soload_t0(:)=asub_to_riv%soload(:,idt-1)
        help_t1_l(:)=soload_t0(:)*soro_conf+(1.0-soro_conf)*solo(:,idt)*cell_area
        asub_to_riv%soload(:,idt)=help_t1_l(:)
      ENDIF

        asub_to_riv%suload(:,idt)=sulo(:,idt)*cell_area    !<kg/d
        asub_to_riv%paload(:,idt)=palo(:,idt)*cell_area    !<kg/d
        asub_to_riv%gwload(:,idt)=gwlo(:,idt)*cell_area    !<kg/d
        asub_to_riv%load(:,idt)=asub_to_riv%suload(:,idt)+asub_to_riv%soload(:,idt)
     
  END DO
  END SUBROUTINE routing_ant
  !#################################################################################################################       
  SUBROUTINE rch_ant(ir,&
                     it,&
                     rch_temp,&
                     fsol, &
                     sed_resus,&
                     sed_settle,&
                     partic_up,&
                     dissol_up,&
                     riv,&
                     rivstate,&
                     rivflux,&
                     sedstate1,&
                     antstate1,&
                     burl,&
                     deg,&
                     dif&
    )

    USE MODELVAR,   ONLY: num_ant_type,bulkdensity,sim_length
    USE MODELPAR_ANT
    USE MODELPAR_SED
    USE MODELVAR_ANT, ONLY :  antusage,        &
                              antbalan,        &
                              rchmass,rchstart,rchend,rchload,rchout,rchburl,rchdeg
    USE MODELTYPE, only: RIVFLUXTYPE,RIVSTATETYPE,RIVERTYPE
    USE MODELTYPE_SED,ONLY : SEDSTATETYPE
    USE MODELTYPE_ANT,ONLY : ANTSTATETYPE

    INTEGER, INTENT(IN)         :: ir
    INTEGER, INTENT(IN)         :: it
    REAL(KIND=8), INTENT(IN)    :: rch_temp
    REAL(KIND=8), INTENT(INOUT) :: fsol(num_ant_type)
    ! REAL(KIND=8), INTENT(INOUT) :: frsol       !! fraction of pesticide in reach that is soluble
    ! REAL(KIND=8), INTENT(INOUT) :: frsrb       !! fraction of pesticide in reach that is sorb
    REAL(KIND=8), INTENT(IN)    :: sed_resus(4)  !!kg
    REAL(KIND=8), INTENT(IN)    :: sed_settle(4) !!kg
    REAL(KIND=8), INTENT(IN)    :: partic_up(num_ant_type) !!mg
    REAL(KIND=8), INTENT(IN)    :: dissol_up(num_ant_type) !!mg
    REAL(KIND=8), INTENT(OUT)    :: burl(num_ant_type) !!mg
    REAL(KIND=8), INTENT(OUT)    :: deg(num_ant_type,2) !!mg
    REAL(KIND=8), INTENT(OUT)    :: dif(num_ant_type) !!mg
    TYPE(RIVERTYPE),    INTENT(IN)    ::   riv
    TYPE(RIVSTATETYPE), INTENT(IN)    ::   rivstate
    TYPE(RIVFLUXTYPE),  INTENT(IN)    ::   rivflux
    TYPE(SEDSTATETYPE), INTENT(IN)    ::   sedstate1
    TYPE(ANTSTATETYPE), INTENT(INOUT)  ::   antstate1
    
    REAL(KIND=8) antex(num_ant_type,sim_length,2,4) !resus&settle 

  
    CALL dis_advect(ir,it,dissol_up,rivstate,rivflux,antstate1)

    ! if(ir==23)write(998,'(1F20.8)',ADVANCE="no")antstate1%chantmass(1)
    CALL par_advect(ir,it,sed_resus,sed_settle,partic_up,sedstate1,antstate1,antstate1%antex)
                                          
   
    CALL ant_degradation(antstate1%chantmass,antstate1%sedantmass,antstate1,rch_temp,fsol,deg)
    ! if(ir==23)write(998,'(2F20.8)',ADVANCE="no")antstate1%sedantmass(1)+antstate1%chantmass(1),deg(1,1)+deg(1,2)
    ! if(ir==23)write(998,'(1F20.8)',ADVANCE="no")antstate1%chantmass(1)
    CALL ant_diffusion(riv,rivflux,fsol,antstate1%chantmass,antstate1%sedantmass,dif)

    ! CALL ant_burial(antstate1%chantmass,antstate1%sedantmass,antpar_general(genid_benbury),burl)
    CALL ant_conc(ir,rivflux%VOL(1),sedstate1,antstate1,fsol)

  END SUBROUTINE rch_ant
  !####################################################################################################################
  SUBROUTINE dis_advect(ir,it,ant_up,rivstate,rivflux,antstate1)

    USE MODELVAR,   ONLY: num_ant_type
    USE MODELPAR_SED, ONLY: sedpar_general,genid_idriv
    USE MODELVAR_SED,   ONLY: sedresp 
    USE MODELTYPE, only: RIVFLUXTYPE,RIVSTATETYPE
    USE MODELTYPE_SED,ONLY : SEDSTATETYPE
    USE MODELTYPE_ANT,ONLY : ANTSTATETYPE

    INTEGER, INTENT(IN) :: ir
    INTEGER, INTENT(IN) :: it
    REAL(KIND=8), INTENT(IN) :: ant_up(num_ant_type) !!mg
    TYPE(RIVSTATETYPE), INTENT(IN)  ::   rivstate
    TYPE(RIVFLUXTYPE),  INTENT(IN)  ::   rivflux
    TYPE(ANTSTATETYPE), INTENT(INOUT)  ::   antstate1

    !>local
    INTEGER iant
    REAL(KIND=8) :: antload(num_ant_type)
    REAL(KIND=8) :: SROVOL, EROVOL
    REAL(KIND=8) :: ant_conc(num_ant_type) 
    REAL(KIND=8) :: ks
    ! if(ir==7)write(998,'(2F20.8)',ADVANCE="no")antstate1%disantmass(1),ant_up(1)
    ks = sedresp(ir)%ks
    ! ks=0.
    SROVOL = 0.
    EROVOL = (1.-ks)*rivstate%Q(2)*3600.*24.  !<m3
    ant_conc = (ant_up + antstate1%disantmass)/(rivflux%VOL(1)+EROVOL) !!mg/m3
    antload  = SROVOL*antstate1%conc + EROVOL*ant_conc  !<mg   

    antstate1%outant_d=antload  !!mg   
    antstate1%outant_d1(:,it)=antstate1%outant_d(:)                                                                                                  
    antstate1%conc  =ant_conc / 1000.  !!mg/m3->mg/L
    antstate1%disantmass=antstate1%conc*rivflux%VOL(1)*1000 !!mg


  END SUBROUTINE dis_advect
!####################################################################################################################
!####################################################################################################################
  SUBROUTINE par_advect(iriv,it,sed_resus,sed_settle,parant_up,sedstate1,antstate1,antex)

    USE MODELTYPE
    USE MODELTYPE_SED,ONLY:SEDSTATETYPE
    USE MODELTYPE_ANT,ONLY:  ANTSTATETYPE
    USE MODELVAR, ONLY : num_ant_type,sim_length
    USE MODELVAR_ANT,ONLY: antusage
    USE MODELPAR_SED 

    INTEGER, INTENT(IN)               :: iriv
    INTEGER, INTENT(IN)               :: it
    REAL(KIND=8), INTENT(IN)          :: sed_resus(4)  !!kg
    REAL(KIND=8), INTENT(IN)          :: sed_settle(4) !!kg
    REAL(KIND=8), INTENT(IN)          :: parant_up(num_ant_type) !!mg
    TYPE(SEDSTATETYPE), INTENT(IN) :: sedstate1
    TYPE(ANTSTATETYPE), INTENT(INOUT) :: antstate1

    !>local
    INTEGER iant
    REAL(KIND=8) :: frclay,frsilt,frsand
    REAL(KIND=8), INTENT(OUT) :: antex(num_ant_type,sim_length,2,4)!!1>resuspended 2>settle 1>caly2>silt3>sand4>sum
    ! frclay=0.43254716981132074;frsilt=0.3333333333333333;frsand=0.23411949685534592
    ! frclay=0.43;frsilt=0.33;frsand=0.24
    REAL(KIND=8):: sedex(3)

sedex(1)=sed_resus(1)-sed_settle(1)
sedex(2)=sed_resus(2)-sed_settle(2)
sedex(3)=sed_resus(3)-sed_settle(3)    

    DO iant=1,num_ant_type
      frclay=antusage(iant)%kd_c/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
      frsilt=antusage(iant)%kd_l/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
      frsand=antusage(iant)%kd_s/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
      !>----clay-------
      ! if(iriv==23.and.iant==1)write(998,'(3F20.8)',ADVANCE="no")antstate1%sedantmass(1)+antstate1%spsantmass(1,1),&
      ! parant_up(1)*frclay
      if(sedex(1).gt.0)then
      antstate1%sedantmass(iant)= antstate1%sedantmass(iant)-antstate1%srbconc(iant,1)*sedex(1)
      antex(iant,it,1,1)=antstate1%srbconc(iant,1)*sed_resus(1)
      antex(iant,it,2,1)=antstate1%srbconc(iant,1)*sed_settle(1)
      antstate1%srbconc(iant,1)=(parant_up(iant)*frclay+antstate1%spsantmass(iant,1)+&
                                antstate1%srbconc(iant,1)*sedex(1))/&
                                (sedstate1%claypool + sedstate1%sedload(1))                        
      else
        antstate1%srbconc(iant,1)=(parant_up(iant)*frclay+antstate1%spsantmass(iant,1))/&
                                  (sedstate1%claypool + sedstate1%sedload(1)+abs(sedex(1)))
        antstate1%sedantmass(iant)= antstate1%sedantmass(iant)+antstate1%srbconc(iant,1)*abs(sedex(1))
        antex(iant,it,1,1)=antstate1%srbconc(iant,1)*sed_resus(1)
        antex(iant,it,2,1)=antstate1%srbconc(iant,1)*sed_settle(1)
      ENDIF
      ! if(iriv==23.and.iant==1)write(998,'(3F20.8)',ADVANCE="no")antstate1%sedantmass(1)+&
      ! antstate1%srbconc(iant,1)*sedstate1%claypool,&
      ! sedstate1%sedload(1)* antstate1%srbconc(iant,1)
      ! if(iriv==23.and.iant==1)write(998,*)
      ! if(sed_resus(1).gt.0)then

      ! antex(iant,it,1,1)=sed_resus(1)*antstate1%srbconc(iant,1)
      ! antstate1%sedantmass(iant)=antstate1%sedantmass(iant)-sed_resus(1)*antstate1%srbconc(iant,1)  

      ! antstate1%srbconc(iant,1)=(parant_up(iant)*frclay+antstate1%spsantmass(iant,1)+     &
      !                           sed_resus(1)*antstate1%srbconc(iant,1))/                  &
      !                           (sedstate1%claypool + sedstate1%sedload(1)) !!mg/kg

      ! ELSEIF(sed_settle(1).ge.0)then

      !   antstate1%sedantmass(iant)=antstate1%sedantmass(iant)+antstate1%srbconc(iant,1)*sed_settle(1)
      !   antex(iant,it,2,1)=antstate1%srbconc(iant,1)*sed_settle(1)

      ! antstate1%srbconc(iant,1)=(parant_up(iant)*frclay+antstate1%spsantmass(iant,1))/    &
      !                           (sedstate1%claypool + sedstate1%sedload(1)+sed_settle(1))


      ! endif
      ! if(iriv==23.and.iant==1)write(998,'(3F20.8)',ADVANCE="no")antstate1%sedantmass(1)+antstate1%spsantmass(1,2),&
      ! parant_up(1)*frsilt
      !>----silt-------
      if(sedex(2).gt.0)then
        antex(iant,it,1,2)=antstate1%srbconc(iant,2)*sed_resus(2)
        antex(iant,it,2,2)=antstate1%srbconc(iant,2)*sed_settle(2)
      antstate1%sedantmass(iant)= antstate1%sedantmass(iant)-antstate1%srbconc(iant,2)*sedex(2)
      antstate1%srbconc(iant,2)=(parant_up(iant)*frsilt+antstate1%spsantmass(iant,2)+&
                                antstate1%srbconc(iant,2)*sedex(2))/&
                                (sedstate1%siltpool + sedstate1%sedload(2))                        
      else
        antstate1%srbconc(iant,2)=(parant_up(iant)*frsilt+antstate1%spsantmass(iant,2))/&
                                  (sedstate1%siltpool + sedstate1%sedload(2)+abs(sedex(2)))
        antstate1%sedantmass(iant)= antstate1%sedantmass(iant)+antstate1%srbconc(iant,2)*abs(sedex(2))
        antex(iant,it,1,2)=antstate1%srbconc(iant,2)*sed_resus(2)
        antex(iant,it,2,2)=antstate1%srbconc(iant,2)*sed_settle(2)
      ENDIF

      ! if(iriv==23.and.iant==1)write(998,'(3F20.8)',ADVANCE="no")antstate1%sedantmass(1)+&
      ! antstate1%srbconc(iant,2)*sedstate1%siltpool,&
      ! sedstate1%sedload(2)* antstate1%srbconc(iant,2)
      ! if(iriv==23.and.iant==1)write(998,*)


      ! IF(sed_resus(2).GT.0.)THEN
      !   antstate1%sedantmass(iant)=antstate1%sedantmass(iant)-sed_resus(2)*antstate1%srbconc(iant,2)!bed sediment
      !   antex(iant,it,1,2)=sed_resus(2)*antstate1%srbconc(iant,2)  

      ! antstate1%srbconc(iant,2)=(parant_up(iant)*frsilt+antstate1%spsantmass(iant,2)+        &
      !                           sed_resus(2)*antstate1%srbconc(iant,2))/&
      !                           (sedstate1%siltpool + sedstate1%sedload(2))
                        
      ! ELSEIF(sed_settle(2).GE.0.)THEN

      ! antstate1%srbconc(iant,2)=(parant_up(iant)*frsilt+antstate1%spsantmass(iant,2))/       &
      !                             (sedstate1%siltpool + sedstate1%sedload(2)+sed_settle(2))
      ! antstate1%sedantmass(iant)=antstate1%sedantmass(iant)+antstate1%srbconc(iant,2)*sed_settle(2)
      ! antex(iant,it,2,2)=antstate1%srbconc(iant,2)*sed_settle(2)

      ! ENDIF
      !>----sand-------
      if(sedex(3).gt.0)then
        antex(iant,it,1,3)=antstate1%srbconc(iant,3)*sed_resus(3)
        antex(iant,it,2,3)=antstate1%srbconc(iant,3)*sed_settle(3)
        antstate1%sedantmass(iant)= antstate1%sedantmass(iant)-antstate1%srbconc(iant,3)*sedex(3)
      antstate1%srbconc(iant,3)=(parant_up(iant)*frsand+antstate1%spsantmass(iant,3)+&
                                antstate1%srbconc(iant,3)*sedex(3))/&
                                (sedstate1%sandpool+sedstate1%sedload(3))                        
      else
        antstate1%srbconc(iant,3)=(parant_up(iant)*frsand+antstate1%spsantmass(iant,3))/&
                                  (sedstate1%sandpool+sedstate1%sedload(3)+abs(sedex(3)))
        antstate1%sedantmass(iant)= antstate1%sedantmass(iant)+antstate1%srbconc(iant,3)*abs(sedex(3))
        antex(iant,it,1,3)=antstate1%srbconc(iant,3)*sed_resus(3)
        antex(iant,it,2,3)=antstate1%srbconc(iant,3)*sed_settle(3)
      ENDIF

      ! IF(sed_resus(3).GT.0.)THEN

      !   antstate1%sedantmass(iant)=antstate1%sedantmass(iant)-sed_resus(3)*antstate1%srbconc(iant,3)
      !   antex(iant,it,1,3)=sed_resus(3)*antstate1%srbconc(iant,3)

      !   antstate1%srbconc(iant,3)=(parant_up(iant)*frsand+antstate1%spsantmass(iant,3)+&
      !                           sed_resus(3)*antstate1%srbconc(iant,3))/&
      !                           (sedstate1%sandpool + sedstate1%sedload(3)) 
                                                


      ! ELSEIF(sed_settle(3).GE.0.)THEN
      ! antstate1%srbconc(iant,3)=(parant_up(iant)*frsand+antstate1%spsantmass(iant,3))/&
      !                           (sedstate1%sandpool+sedstate1%sedload(3)+sed_settle(3))

      ! antstate1%sedantmass(iant)=antstate1%sedantmass(iant)+antstate1%srbconc(iant,3)*sed_settle(3)
      ! antex(iant,it,2,3)=antstate1%srbconc(iant,3)*sed_settle(3)

      ! ENDIF

      !>-----------------------------------
      antstate1%outant_p(iant) = antstate1%srbconc(iant,1)*sedstate1%sedload(1)+&
                                antstate1%srbconc(iant,2)*sedstate1%sedload(2)+&
                                antstate1%srbconc(iant,3)*sedstate1%sedload(3) !!mg/kg*kg

    END DO
    antstate1%outant_p1(:,it)=antstate1%outant_p(:)
    !--caculate antpool--------
    antstate1%spsantmass(:,1)=antstate1%srbconc(:,1)*sedstate1%claypool !!mg
    antstate1%spsantmass(:,2)=antstate1%srbconc(:,2)*sedstate1%siltpool !!mg
    antstate1%spsantmass(:,3)=antstate1%srbconc(:,3)*sedstate1%sandpool !!mg
    antstate1%spsantmass(:,4)=antstate1%spsantmass(:,1)+antstate1%spsantmass(:,2)+&
                              antstate1%spsantmass(:,3)
    antstate1%chantmass(:)=antstate1%disantmass(:) + antstate1%spsantmass(:,4) !!mg
    antstate1%outant(:,it) = antstate1%outant_d+antstate1%outant_p

    antex(:,it,1,4)=antex(:,it,1,1)+antex(:,it,1,2)+antex(:,it,1,3)
    antex(:,it,2,4)=antex(:,it,2,1)+antex(:,it,2,2)+antex(:,it,2,3)

    ! if(iriv==23)write(998,*)antex(1,it,1,4),antex(1,it,2,4),&
    ! sed_resus(4),sed_settle(4),antstate1%sedantmass(1),&
    !                         antstate1%chantmass(1)

    ! if(iriv==23)write(998,*)antex(1,it,1,1),antex(1,it,2,1)
  END SUBROUTINE par_advect
!####################################################################################################################

!####################################################################################################################
SUBROUTINE ant_burial(chantmass,sedantmass,ben_bury,burl)
  
  USE MODELVAR,ONLY : num_ant_type,bulkdensity
  USE MODELVAR_ANT,ONLY:antusage

  REAL(KIND=8), INTENT(INOUT) :: chantmass(num_ant_type)   ! mg calculate mass of antibiotic in reach
  REAL(KIND=8), INTENT(INOUT) :: sedantmass(num_ant_type)  ! mg calculate mass of antibiotic in bed sediment
  REAL(KIND=8), INTENT(IN)    :: ben_bury!! m/day         |burial velocity in benthic sediment
  REAL(KIND=8), INTENT(OUT)    :: burl(num_ant_type)
  INTEGER iant
  REAL(KIND=8) :: ben_dep !! m             |depth of active benthic layer
  REAL(KIND=8) :: ant_bury(num_ant_type)
  REAL(KIND=8) :: ch_bd,por,fdis
!! calculate removal of antbiotic from active sediment layer by burial
   ben_dep  = 0.02 !!与初始底泥厚度取相同�?
  ! ben_bury = 0.05 !0-0.1

  DO iant = 1,num_ant_type

    ch_bd=bulkdensity*1.e-3
    por = 1. - ch_bd / 2.65   !< t/m3 = g/cm3
    fdis=1/(por+(1-por)*antusage(iant)%Kpsed*ch_bd*1.e6)

  ant_bury(iant) = ben_bury * sedantmass(iant)*(1-fdis) / ben_dep
  IF (ant_bury(iant) > sedantmass(iant)) THEN
    ant_bury(iant) = sedantmass(iant)
    sedantmass(iant) = 0.
  ELSE
    sedantmass(iant) = sedantmass(iant) - ant_bury(iant)
  END IF
END DO
  burl=ant_bury

END SUBROUTINE ant_burial
!####################################################################################################################

!####################################################################################################################
SUBROUTINE ant_diffusion(riv,rivflux,fsol,chantmass,sedantmass,dif)

  USE MODELVAR,ONLY : num_ant_type,bulkdensity
  USE MODELVAR_ANT,ONLY : antusage
  USE MODELPAR_ANT
  USE MODELTYPE,    ONLY :  RIVERTYPE,       &
                            RIVFLUXTYPE

  REAL(KIND=8), INTENT(INOUT) :: chantmass(num_ant_type)   ! mg calculate mass of antibiotic in reach
  REAL(KIND=8), INTENT(INOUT) :: sedantmass(num_ant_type)  ! mg calculate mass of antibiotic in bed sediment
  REAL(KIND=8), INTENT(IN)    :: fsol(num_ant_type)
  ! REAL(KIND=8), INTENT(IN)    :: frsol
  TYPE(RIVERTYPE),    INTENT(IN)    :: riv
  TYPE(RIVFLUXTYPE),  INTENT(IN)    :: rivflux

  INTEGER iant
  REAL(KIND=8)  :: ant_difus(num_ant_type) !!mg
  REAL(KIND=8)  :: mixvel
  REAL(KIND=8)  :: por         !! none          |porosity of bottom sediments
  REAL(KIND=8)  :: fd2,fdis
  REAL(KIND=8)  :: rttime
  REAL(KIND=8)  :: tday
  REAL(KIND=8)  :: rchdep      !! m
  REAL(KIND=8)  :: ch_bd       !! t/m3          |dry bulk density
  REAL(KIND=8)  :: kdif        !! (day-1)  diffusion rate
  REAL(KIND=8),INTENT(OUT)    ::  dif(num_ant_type) !!mg
  REAL(KIND=8)  :: dis_bed,dis_ch
  ch_bd = bulkdensity*1.e-3!1.3 kg/m3->t/m3
  rchdep = rivflux%VOL(1)/(riv%length*riv%width)  
  kdif = antpar_general(genid_kdif_r)

  !! calculate velocity and travel time
    rttime = riv%length  / (3600. * rivflux%vel) !< m/ m/h = h
  !! calculate flow duration
    tday = rttime / 24.0 !day
    if (tday > 1.0) tday = 1.0
    tday = 1.0

   !! calculate diffusion of antibiotic between reach and sediment
  DO iant = 1,num_ant_type

    !! ASSUME DENSITY=2.6E6; KD2=KD1
    por = 1. - ch_bd / 2.65   !< t/m3 = g/cm3
    fdis=1/(por+(1-por)*antusage(iant)%Kpsed*ch_bd*1.e6) ! L/mg

    ant_difus(iant) = kdif * (fdis * sedantmass(iant) - fsol(iant)*chantmass(iant)) * tday / rchdep
    dif(iant)=ant_difus(iant)
    dis_bed=fdis*sedantmass(iant)
    dis_ch=fsol(iant)*chantmass(iant)
    IF (ant_difus(iant) > 0.) THEN
      ! ant_difus(iant)=max(Abs(ant_difus(iant)),dis_bed)
      IF (ant_difus(iant) > sedantmass(iant)) THEN
        ant_difus(iant) = sedantmass(iant)
        sedantmass(iant) = 0.
      ELSE
        sedantmass(iant) = sedantmass(iant) - Abs(ant_difus(iant))
      END IF
      chantmass(iant) = chantmass(iant) + Abs(ant_difus(iant))
    ELSEIF(ant_difus(iant)<0.)THEN
      ! ant_difus(iant)=max(Abs(ant_difus(iant)),dis_ch)
      IF (Abs(ant_difus(iant)) > chantmass(iant)) THEN
        ant_difus(iant) = -chantmass(iant)
        chantmass(iant) = 0.
      ELSE
        chantmass(iant) = chantmass(iant) - Abs(ant_difus(iant))
      END IF
        sedantmass(iant) = sedantmass(iant) + Abs(ant_difus(iant))
    ENDIF
    !if(chantmass(iant).lt.0)STOP
  END DO
 
END SUBROUTINE ant_diffusion
!####################################################################################################################

!####################################################################################################################
SUBROUTINE ant_degradation(chantmass,sedantmass,antstate1,rch_temp,fsol,deg1)

  USE MODELVAR,ONLY : num_ant_type,bulkdensity
  USE MODELVAR_ANT,ONLY : antbalan,antusage
  USE MODELPAR_ANT
  USE MODELTYPE_ANT,ONLY : ANTSTATETYPE
  REAL(KIND=8), INTENT(INOUT) :: chantmass(num_ant_type)   ! mg calculate mass of antibiotic in reach
  REAL(KIND=8), INTENT(INOUT) :: sedantmass(num_ant_type)  ! mg calculate mass of antibiotic in bed sediment
  TYPE(ANTSTATETYPE), INTENT(INOUT) :: antstate1
  ! REAL(KIND=8), INTENT(IN)    :: frsol
  REAL(KIND=8), INTENT(IN) :: rch_temp
  REAL(KIND=8), INTENT(OUT) :: deg1(num_ant_type,2)
  REAL(KIND=8), INTENT(IN)    :: fsol(num_ant_type)
  REAL(KIND=8) :: k,kt,decay1,por,fd2,ch_bd,fdis,decay2
  ! REAL(KIND=8) :: deg1(num_ant_type,2)
  INTEGER iant

  kt  = antpar_general(genid_kt)
  k = kt ** (rch_temp-20)

  !! calculate amount of antibiotic that degradation on day in reach
  DO iant = 1,num_ant_type
    ch_bd=bulkdensity*1.e-3
    por = 1. - ch_bd / 2.65   !< t/m3 = g/cm3
    fdis=1/(por+(1-por)*antusage(iant)%Kpsed*ch_bd*1.e6)
    decay1 = Exp(-(0.693/antusage(iant)%DTwater) * k)
    decay2 = Exp(-(0.693/antusage(iant)%DTsediment) * k)
    IF (chantmass(iant) > 1.e-12) THEN
      deg1(iant,1)=chantmass(iant)*fsol(iant)*(1-decay1)
      chantmass(iant) = chantmass(iant)*(1-fsol(iant)) +chantmass(iant)*decay1*fsol(iant)
    ENDIF
    
    IF (sedantmass(iant) > 1.e-12) THEN
      deg1(iant,2)= sedantmass(iant)*fdis*(1-decay2)
      sedantmass(iant) = sedantmass(iant)*(1-fdis)+sedantmass(iant)*decay2*fdis
    ENDIF
    
  END DO
END SUBROUTINE ant_degradation
!####################################################################################################################

!####################################################################################################################
SUBROUTINE ant_conc(iriv,rchvol,sedstate1,antstate1,fsol)

  USE MODELVAR,ONLY      : num_ant_type,bulkdensity
  USE MODELVAR_ANT,ONLY  : antusage
  USE MODELTYPE_SED,ONLY : SEDSTATETYPE
  USE MODELTYPE_ANT,ONLY : ANTSTATETYPE
  USE MODELPAR_SED
  
  INTEGER, INTENT(IN)      :: iriv
  REAL(KIND=8), INTENT(IN) :: rchvol !! m3
  TYPE(SEDSTATETYPE), INTENT(IN)    :: sedstate1
  TYPE(ANTSTATETYPE), INTENT(INOUT) :: antstate1
  REAL(KIND=8) , INTENT(OUT) :: fsol(num_ant_type)
  REAL(KIND=8):: frsol      !  fraction of antibiotic in reach that is soluble
  REAL(KIND=8):: frsrb      !  fraction of antibiotic in reach that is sorb
  
  INTEGER iant
  REAL(KIND=8) :: kd         !  m3/g =l/mg partition coefficient 
  REAL(KIND=8) :: antconc_spm    !! mg/kg antibiotic conc of suspended sediment
  REAL(KIND=8) :: antconc_bedsed !! mg/kg antibiotic conc of bed sediment
  REAL(KIND=8) :: antconc_dis    !! mg/L
  REAL(KIND=8) :: frclay    !! fraction of antibiotic in clay
  REAL(KIND=8) :: frsilt    !! fraction of antibiotic in clay
  REAL(KIND=8) :: frsand    !! fraction of antibiotic in clay
  REAL(KIND=8) :: a,b,por,fdis,ch_bd
  
  
  DO iant=1,num_ant_type
    kd = antusage(iant)%Kpwater
    !! calculate fraction of soluble and sorbed antibiotic
    frsol = 1. / (1. + antusage(iant)%Kpwater * sedstate1%sedconc)
    frsrb = 1. - frsol
    fsol(iant)= frsol
! if(iant==1)write(6,*)fsol(iant)
    ch_bd=bulkdensity*1.e-3
    por = 1. - ch_bd / 2.65   !< t/m3 = g/cm3
    fdis=1/(por+(1-por)*antusage(iant)%Kpsed*ch_bd*1.e6)

    frclay=antusage(iant)%kd_c/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
    frsilt=antusage(iant)%kd_l/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
    frsand=antusage(iant)%kd_s/(antusage(iant)%kd_c+antusage(iant)%kd_l+antusage(iant)%kd_s)
    
    antconc_spm = antstate1%chantmass(iant)*frsrb/(sedstate1%sedpool)
    antconc_dis = antstate1%chantmass(iant)*frsol/(rchvol*1.e3)
    antconc_bedsed=antstate1%sedantmass(iant)*(1-fdis)/(sedstate1%sedbed)

    antstate1%bedconc(iant) = antconc_bedsed
    antstate1%parconc(iant) = antconc_spm
    antstate1%conc(iant)    = antconc_dis

    antstate1%disantmass(iant)=antstate1%chantmass(iant)*frsol
    antstate1%spsantmass(iant,1)=antstate1%chantmass(iant)*frsrb*frclay
    antstate1%spsantmass(iant,2)=antstate1%chantmass(iant)*frsrb*frsilt
    antstate1%spsantmass(iant,3)=antstate1%chantmass(iant)*frsrb*frsand
    antstate1%spsantmass(iant,4)=antstate1%chantmass(iant)*frsrb
    
  END DO
  ! write(6,*)fsol(:)
END SUBROUTINE ant_conc
!####################################################################################################################
END MODULE REACH_ANT
