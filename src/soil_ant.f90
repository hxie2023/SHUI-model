MODULE SOIL_ANT
  
  IMPLICIT NONE
CONTAINS

SUBROUTINE module_soil_ant(cur_time,        &
                           c,               &
                           idt,             &
                           isoil,           &
                           iland,           &
                           sthick,          &
                           sol_temp,        &
                           sol_water,       &
                           sol_runoff,      &
                           prk,             &
                           surflow,         &
                           erodedsed,       &
                           soload,          &
                           suload,          &
                           perc_to_aqu_ant, &
                           erodedant,       &
                           antpool,&
                           degrade,prklo) 
USE MODELVAR,       ONLY: max_soillayer,   &
                          num_ant_type
USE MODELTYPE,      ONLY: TIMEINFORMATIONTYPE
USE MODELTYPE_ANT,  ONLY:  ANTPOOlTYPE

TYPE(TIMEINFORMATIONTYPE),INTENT(IN)  :: cur_time
INTEGER,INTENT(IN)               :: c 
INTEGER,INTENT(IN)               :: idt 
INTEGER,INTENT(IN)               :: isoil
INTEGER,INTENT(IN)               :: iland
REAL(KIND=8),INTENT(IN)          :: sthick(0:max_soillayer)        !m
REAL(KIND=8),INTENT(IN)          :: sol_temp(0:max_soillayer)
REAL(KIND=8),INTENT(IN)          :: sol_water(0:max_soillayer)     !mm
REAL(KIND=8),INTENT(IN)          :: sol_runoff(1:max_soillayer)    !mm
REAL(KIND=8),INTENT(IN)          :: prk(0:max_soillayer)           !mm
REAL(KIND=8),INTENT(IN)          :: surflow                        !mm
REAL(KIND=8),INTENT(IN)          :: erodedsed                      !<kg/m2
REAL(KIND=8),INTENT(OUT)         :: soload(num_ant_type)          !!kg/m2
REAL(KIND=8),INTENT(OUT)         :: suload(num_ant_type)          !!kg/m2
REAL(KIND=8),INTENT(OUT)         :: perc_to_aqu_ant(num_ant_type) !!mg/km2
REAL(KIND=8),INTENT(OUT)         :: erodedant(num_ant_type)       !!(kg/m2) the amount of sorbed ant 
REAL(KIND=8),INTENT(OUT)         :: degrade(num_ant_type,2)
REAL(KIND=8),INTENT(OUT)         :: prklo(num_ant_type,0:max_soillayer)!!mg/km2
TYPE(ANTPOOLTYPE),INTENT(INOUT)  :: antpool

  CALL ant_dissolved_load(isoil,sthick,sol_water,sol_runoff,prk,&
                          surflow,soload,suload,perc_to_aqu_ant,antpool%m_antpool,prklo)

  CALL ant_eroded_load(isoil,surflow,erodedsed,sthick(0),erodedant,antpool%m_antpool)
  CALL ant_sorce(c,cur_time,sthick,iland,antpool%im_sorbed,antpool%m_antpool)
  CALL ant_transfer(isoil,sthick,sol_water,antpool)
  CALL ant_degradation(c,idt,sol_temp,antpool,degrade)
END SUBROUTINE module_soil_ant
!#################################################################################################################
!  SUBROUTINE ant_mobile(idt,            &
!                       isoil,           &
!                       sthick,          &
!                       sol_water,       &
!                       sol_runoff,      &
!                       prk,             &
!                       surflow,         &
!                       erodedsed,       &
!                       soload,          &
!                       suload,          &
!                       perc_to_aqu_ant, &
!                       erodedant,       &
!                       m_antpool)        
!   USE MODELVAR,       ONLY: max_soillayer,   &
!                             num_ant_type

!   INTEGER,INTENT(IN)          :: idt 
!   INTEGER,INTENT(IN)          :: isoil
!   REAL(KIND=8),INTENT(IN)     :: sthick(0:max_soillayer) !m
!   REAL(KIND=8),INTENT(IN)     :: sol_water(0:max_soillayer)  !mm
!   REAL(KIND=8),INTENT(IN)     :: sol_runoff(1:max_soillayer) !mm
!   REAL(KIND=8),INTENT(IN)     :: prk(0:max_soillayer) !mm
!   REAL(KIND=8),INTENT(IN)     :: surflow !mm
!   REAL(KIND=8),INTENT(IN)     :: erodedsed  !<kg/m2
!   REAL(KIND=8),INTENT(OUT)    :: soload(num_ant_type) !!kg/m2
!   REAL(KIND=8),INTENT(OUT)    :: suload(num_ant_type) !!kg/m2
!   REAL(KIND=8),INTENT(OUT)    :: perc_to_aqu_ant(num_ant_type) !!mg/km2
!   REAL(KIND=8),INTENT(OUT)    :: erodedant(num_ant_type) !(kg/m2) the amount of sorbed ant 
!   REAL(KIND=8),INTENT(INOUT)  :: m_antpool(num_ant_type,0:max_soillayer)!!mg/km2
 
!   CALL ant_dissolved_load(isoil,sthick,sol_water,sol_runoff,prk,&
!                           surflow,soload,suload,perc_to_aqu_ant,m_antpool)
!   CALL ant_eroded_load(isoil,surflow,erodedsed,sthick(0),erodedant,m_antpool)

!  END SUBROUTINE ant_mobile
!#################################################################################################################

!#################################################################################################################
  SUBROUTINE ant_dissolved_load(isoil,          &
                                sthick,         &
                                sol_water,      &
                                sol_runoff,     &
                                prk,            &
                                surflow,        &
                                soload,         &
                                suload,         &
                                perc_to_aqu_ant,&
                                m_antpool,prklo)        

    USE MODELVAR, ONLY: max_soillayer,   &
                        bd_soil,         &
                        sw_pv,           &
                        num_ant_type
    USE MODELVAR_ANT, ONLY: antusage
    USE MODELPAR_ANT
    USE MODELTYPE_ANT, ONLY:ANTPOOlTYPE
    INTEGER,INTENT(IN)          :: isoil
    REAL(KIND=8),INTENT(IN)     :: sthick(0:max_soillayer) !m
    REAL(KIND=8),INTENT(IN)     :: sol_water(0:max_soillayer)  !mm
    REAL(KIND=8),INTENT(IN)     :: sol_runoff(1:max_soillayer) !mm
    REAL(KIND=8),INTENT(IN)     :: prk(0:max_soillayer) !mm
    REAL(KIND=8),INTENT(IN)     :: surflow !mm
    REAL(KIND=8),INTENT(OUT)    :: soload(num_ant_type) !!kg/m2
    REAL(KIND=8),INTENT(OUT)    :: suload(num_ant_type) !!kg/m2
    REAL(KIND=8),INTENT(OUT)    :: perc_to_aqu_ant(num_ant_type) !!mg/km2
    REAL(KIND=8),INTENT(INOUT)  :: m_antpool(num_ant_type,0:max_soillayer)!!mg/km2
    REAL(KIND=8),INTENT(OUT)    :: prklo(num_ant_type,0:max_soillayer)
    ! REAL(KIND=8),INTENT(OUT)    :: leach(num_ant_type) !!kg/m2
  !>local
    INTEGER ily,iant
    REAL(KIND=8) zdb1     !mm
    REAL(KIND=8) vf       !mm
    REAL(KIND=8) xx       !mg/km2
    REAL(KIND=8) yy       !mg/km2
    REAL(KIND=8) co,csurf !mg/km2 *mm-1 = mg/L
    REAL(KIND=8) percop   !none
    REAL(KIND=8) bd       !g/cm3
    REAL(KIND=8) kp       !!L/kg
    REAL(KIND=8) sol_load(num_ant_type,max_soillayer) !!kg/m2


    percop=antpar_general(genid_percop)
    co=0.

    DO ily=0,max_soillayer
      DO iant=1,num_ant_type
      ! m_antpool(iant,ily)=antpool%m_dissol(iant,ily)+antpool%m_sorbed(iant,ily)
      ! if(iant==1.and.ily==0) write(996,'(F20.8)',ADVANCE="no")m_antpool(iant,ily)
      ! if(iant==1.and.ily==1) write(997,'(F20.8)',ADVANCE="no")m_antpool(iant,ily)
        kp = antusage(iant)%Kpsoil !L/KG
   
        IF(ily==0) THEN
          bd = bd_soil(1,isoil)
        ELSE
          bd = bd_soil(ily,isoil)
        ENDIF
        zdb1=sw_pv(ily,isoil)+kp*bd*sthick(ily)*1.e3!mm
        ! zdb1=sol_water(ily)+kp*bd*sthick(ily)*1.e3 !mm

        IF(ily==0)THEN
        vf = prk(ily)+surflow
        ELSE
        vf = prk(ily)+sol_runoff(ily)!mm
        ENDIF

        xx =  m_antpool(iant,ily) * (1.-Exp(-vf/(zdb1)))  !mg/km2
      !  xx =  m_antpool(iant,ily) * (1. - Exp(-(prk(ily)+surflow*0.05) / (zdb1 + 1.e-6)))  !mg/km2 
      ! if(iant==1.and.ily==0) write(996,'(4F20.8)',ADVANCE="no") m_antpool(iant,ily),(1.-Exp(-vf / (zdb1 + 1.e-6))),&
      ! vf,zdb1
        IF(vf.gt.0.)co=xx/vf !mg/km2 *mm-1 
        ! IF(co.ge.antusage(iant)%wsol*0.01)co=antusage(iant)%wsol*0.01 !solubility
        csurf = percop * co  
      !----lost in surflow-----------
        IF(ily==0) THEN
          yy=csurf*surflow  !mg/km2
          IF(yy.GT.m_antpool(iant,ily))yy=m_antpool(iant,ily) 
          m_antpool(iant,ily)= m_antpool(iant,ily)-yy
          suload(iant)=yy *1.e-12 !mg/km2->kg/m2
        ENDIF
      ! if(iant==1.and.ily==0) write(996,'(F20.8)',ADVANCE="no")yy!mg/L
                                              
      ! if(iant==1.and.ily==0) write(996,*)                                                          
      ! if(iant==1.and.ily==0) write(996,'(3F20.8)',ADVANCE="no")(1.-Exp(-vf/(zdb1 + 1.e-6))), yy, m_antpool(iant,ily)!mg/L
      ! if(iant==1.and.ily==1) write(996,'(2F20.8)',ADVANCE="no") yy, m_antpool(iant,ily)!mg/L
      ! if(iant==1.and.ily==0) write(996,'(2F20.8)',ADVANCE="no")yy,m_dispool(iant,ily)
      ! if(iant==1.and.ily==0)write(996,'(3F20.8)',ADVANCE="no")surflow,yy,m_dispool(iant,ily)
      ! if(iant==1.and.ily==0)write(996,*)   
      !----lost in sol_runoff-------
      IF(ily.GT.0) THEN
        yy=co*sol_runoff(ily) !mg/km2 *mm-1 *mm =mg/km2
        IF(yy.GT.m_antpool(iant,ily))yy=m_antpool(iant,ily) 
        m_antpool(iant,ily)= m_antpool(iant,ily)-yy
      ! if(iant==1.and.ily==0) write(996,'(2F20.8)',ADVANCE="no") yy, m_antpool(iant,ily)!mg/L
      ! if(iant==1.and.ily==1) write(997,'(3F20.8)',ADVANCE="no") yy, sol_runoff(ily),m_antpool(iant,ily)!mg/L
        sol_load(iant,ily)=yy *1.e-12 !mg/km2->kg/m2
      ENDIF

      !----lost in percolation-------
        IF(ily==0)THEN
          yy=csurf*prk(ily)
        ELSE
          yy=co*prk(ily)
        ENDIF
        IF(yy.GT.m_antpool(iant,ily))yy=m_antpool(iant,ily) 
        m_antpool(iant,ily)= m_antpool(iant,ily)-yy
        prklo(iant,ily)=yy
        IF(ily+1.LE.max_soillayer)THEN
          IF(sthick(ily+1) .GT. 0.) THEN
            m_antpool(iant,ily+1)= m_antpool(iant,ily+1)+yy
          ELSEIF(sthick(ily+1) .LE. 0.)THEN
           perc_to_aqu_ant(iant) = yy
          ENDIF
        ENDIF
      ! if(iant==1.and.ily==0) write(996,'(2F20.8)',ADVANCE="no") yy, m_antpool(iant,ily)!mg/L
      ! if(iant==1.and.ily==1) write(997,'(3F20.8)',ADVANCE="no") yy,prk(ily), m_antpool(iant,ily)!mg/L
      ! if(iant==1.and.ily==1) write(997,*)
      END DO !<iant
    END DO!<ily
    soload(:)=sol_load(:,1)+sol_load(:,2)+sol_load(:,3)

  END SUBROUTINE ant_dissolved_load
!#################################################################################################################

!##########################################################################################################
  SUBROUTINE ant_eroded_load(isoil,      &
                             surflow,    &
                             erodedsed,  &
                             thickness,  &
                             erodedant,  &
                             m_antpool)

    USE MODELVAR, ONLY: num_ant_type,    &
                        max_soillayer,    &
                        cell_area,        &
                        bd_soil,          &
                        sw_pv,            &
                        realzero
    USE MODELVAR_ANT,  ONLY: antusage
    ! USE GENERAL
    INTEGER, INTENT(IN)           ::  isoil
    REAL(KIND=8), INTENT(IN)      ::  surflow
    REAL(KIND=8), INTENT(IN)      ::  erodedsed  !<kg/m2
    REAL(KIND=8), INTENT(IN)      ::  thickness !m
    REAL(KIND=8), INTENT(OUT)     ::  erodedant(num_ant_type) !(kg/m2) the amount of sorbed ant 
                                                             !transported to the main channel in surface runoff
    REAL(KIND=8), INTENT(INOUT)   ::  m_antpool(num_ant_type,0:max_soillayer)  !!mg/km2
    !>local
    INTEGER iant
    REAL(KIND=8) csed ! concentration of sediment in surface runoff (t/m3)
    REAL(KIND=8) pool
    REAL(KIND=8) ant_remove
    REAL(KIND=8) ant_init
    REAL(KIND=8) enratio !none
    REAL(KIND=8) conc !!mg/kg
    REAL(KIND=8) zdb1
    REAL(KIND=8) kd

    

    pool    = 0.
    enratio = 0.

    !! CREAMS method for calculating enrichment ratio
    DO iant = 1 , num_ant_type
      IF (erodedsed .GT. 0) THEN
        csed = (erodedsed*cell_area*0.001)/ (cell_area * surflow*0.001) !<t/m3=kg/L
        IF (csed> 0.) THEN
          enratio = .78 * csed ** (-.2468) !none
        ELSE
          enratio = 0.
          csed = 0.
        ENDIF
        ! IF (enratio > 3.0) enratio = 3.0
      ENDIF

      ant_init = m_antpool(iant,0)
      IF (ant_init .GT.0) THEN
        kd = antusage(iant)%kpsoil
        zdb1 = sw_pv(0,isoil) + kd * bd_soil(1,isoil) * thickness *1.e3 !mm
        !! units: mm + (L/kg)*(g/cm^3)*m *1.e3= mm
        conc = 1.e-6* kd * ant_init / (zdb1 + 1.e-10) !!unit:mg/kg
      ENDIF
       !caculation erodedant
      IF(enratio.gt.0.)THEN
        erodedant(iant) = 1.e-6* erodedsed * conc * enratio  !!mg/m2->kg/m2 
      ENDIF
      !kg/m2*mg/kg*1.e6=kg/m2
      ant_remove=erodedant(iant)*1.e12!KG/M2->mg/km2
      IF(ant_remove .GT. ant_init) ant_remove = m_antpool(iant,0) !mg/km2 
      pool = ant_init - ant_remove !mg/km2
      IF (pool .GE. realzero) THEN
        m_antpool(iant,0) = pool
      ELSE
        m_antpool(iant,0) = 0.
      ENDIF 
! if(iant==1) write(996,'(2F20.8)',ADVANCE="no")ant_remove,m_antpool(1,0)!mg/L
! write(996,'(F20.8)',ADVANCE="no")m_antpool(1,1)!mg/L
! if(iant==1) write(996,'(2F20.8)',ADVANCE="no")ant_remove,m_sorpool(iant,0)           
! if(iant==1) write(996,*)      
  END DO !<nub_substance
  
  
  END SUBROUTINE ant_eroded_load
!##################################################################################################################
!##################################################################################################################
  SUBROUTINE ant_sorce(c,          &
                       cur_time,   &
                       thickness,  &
                       iland,      &
                       im_sorpool, &
                       mantpool)
    USE MODELVAR,      ONLY  : num_ant_type,    &
                               max_soillayer

    USE MODELVAR_ANT, ONLY  :  peosource,        &
                               animsource,       &
                               antcropdata
    USE MODELPAR_ANT
    USE DATETIME,       ONLY  :     in_season_period 
    USE MODELTYPE,      ONLY  :     TIMEINFORMATIONTYPE
    
    TYPE(TIMEINFORMATIONTYPE),INTENT(IN)  :: cur_time
    INTEGER,INTENT(IN)                    :: c
    INTEGER,INTENT(IN)                    :: iland
    REAL(KIND=8),INTENT(IN)               :: thickness(0:max_soillayer)
    REAL(kind=8), INTENT(INOUT)           :: im_sorpool(num_ant_type,0:max_soillayer)
    REAL(kind=8), INTENT(INOUT)           :: mantpool(num_ant_type,0:max_soillayer)

    !<local
    REAL(KIND=8) layer1,ratem
    REAL(KIND=8) emh_s(num_ant_type)        !<mg/km2 human emission 
    REAL(KIND=8) emh_add0_m(num_ant_type)   !<mg/km2
    REAL(KIND=8) emh_add0_im(num_ant_type)  !<mg/km2
    REAL(KIND=8) emh_add1_m(num_ant_type)   !<mg/km2
    REAL(KIND=8) emh_add1_im(num_ant_type)  !<mg/km2
    REAL(KIND=8) ema_s(num_ant_type)        !<animal animal emission 
    REAL(KIND=8) ema_add1_m(num_ant_type)   !<mg/km2
    REAL(KIND=8) ema_add1_im(num_ant_type)  !<mg/km2 
    REAL(KIND=8) ema_add0_m(num_ant_type)   !<mg/km2
    REAL(KIND=8) ema_add0_im(num_ant_type)  !<mg/km2 

    INTEGER bd2,bd5        !>cultivation dates
    INTEGER iant,k
    INTEGER updays

    updays    = 20  
    layer1    = antpar_general(genid_ratelayer) !进入�?1层的比例
    ratem     = antpar_general(genid_ratem)     !进入移动库的比例
    !----------people source input------------------------
    DO k = 1, 2 !>rice (4-paddyfield)and wheat(2-dryland)
      IF(iland.EQ.4) THEN  !
        IF(k.NE.1) CYCLE 
      ELSEIF(iland.EQ.2) THEN 
        IF(k.NE.2) CYCLE
      ELSE 
        CYCLE
      END IF

      bd2 = antcropdata(k)%baredayno2        !sow date / beginning of growing season 
      bd5 = antcropdata(k)%baredayno5        !winter crops sowing date
      IF(bd2.ne.0)THEN
        IF(in_season_period(bd2,updays,cur_time))THEN!
          emh_s(:) = peosource(c,:)/updays
        ENDIF
      ELSEIF(bd5.ne.0)THEN
        IF(in_season_period(bd5,updays,cur_time))THEN
          emh_s(:) = peosource(c,:)/updays
        ENDIF
      ENDIF

      
        DO iant=1, num_ant_type   
          IF(emh_s(iant) .GE. 0) THEN
          IF(thickness(1).GT.0.)THEN

            emh_add0_m(iant)  = emh_s(iant) * (1-layer1) * ratem
            emh_add0_im(iant) = emh_s(iant) * (1-layer1) * (1-ratem)
            emh_add1_m(iant)  = emh_s(iant) * layer1 * ratem
            emh_add1_im(iant) = emh_s(iant) * layer1 * (1-ratem)
            !分比例进�?0�?1层土壤中
            CALL ant_add_pool(1,mantpool(iant,0), emh_add0_m(iant))
            CALL ant_add_pool(1,im_sorpool(iant,0),emh_add0_im(iant))
            CALL ant_add_pool(1,mantpool(iant,1), emh_add1_m(iant))
            CALL ant_add_pool(1,im_sorpool(iant,1),emh_add1_im(iant))
! if(iant==1)write(996,'(2F20.8)',ADVANCE="no")emh_add0_m(iant),mantpool(1,0)!mg/L           
! if(iant==1)write(997,'(2F20.8)',ADVANCE="no")emh_add1_m(iant),mantpool(1,1)
          ELSE 
            emh_add0_m(iant)  = emh_s(iant) * ratem
            emh_add0_im(iant) = emh_s(iant) * (1-ratem)
            CALL ant_add_pool(1,mantpool(iant,0), emh_add0_m(iant) )
            CALL ant_add_pool(1,im_sorpool(iant,0),emh_add0_im(iant))      
          ENDIF!<layer
        END IF
        ENDDO !<num_ant_type
      
    ENDDO !>k

   !----------animal source input------------------------
    ema_s(:) = animsource(c,:) 
    
      DO iant=1, num_ant_type
        IF(ema_s(iant).GE.0) THEN
          IF(thickness(1).GT.0.)THEN
            ema_add0_m(iant) = ema_s(iant) * (1-layer1) * ratem
            ema_add0_im(iant)= ema_s(iant) * (1-layer1) * (1-ratem)
            ema_add1_m(iant) = ema_s(iant) * layer1 * ratem
            ema_add1_im(iant)= ema_s(iant) * layer1 * (1-ratem)
            CALL ant_add_pool(1,mantpool(iant,0), ema_add0_m(iant) )!分比例进�?0�?1层土壤中
            CALL ant_add_pool(1,im_sorpool(iant,0),ema_add0_im(iant))!分比例进�?0�?1层土壤中
            CALL ant_add_pool(1,mantpool(iant,1), ema_add1_m(iant) )
            CALL ant_add_pool(1,im_sorpool(iant,1),ema_add1_im(iant))
  ! if(iant==1)write(996,'(2F20.8)',ADVANCE="no")ema_add0_m(iant),mantpool(1,0)
  ! ! if(iant==1)write(996,*)
  ! if(iant==1)write(997,'(2F20.8)',ADVANCE="no")ema_add1_m(iant),mantpool(1,1)              
          ELSE
            ema_add0_m(iant)  = ema_s(iant) * ratem
            ema_add0_im(iant) = ema_s(iant) * (1-ratem)
            CALL ant_add_pool(1,mantpool(iant,0), ema_add0_m(iant) )
            CALL ant_add_pool(1,im_sorpool(iant,0),ema_add0_im(iant))
          ENDIF!<layer
        ENDIF
      ENDDO !>nub_substance
    

  END SUBROUTINE ant_sorce
!##################################################################################################################
!#################################################################################################################
  SUBROUTINE ant_degradation(c,         &
                             it,         &
                             sol_temp,   &
                             antpool,degrade)

    USE MODELVAR, ONLY : num_ant_type, &
                         max_soillayer
    USE MODELVAR_ANT, ONLY : antusage,decay_sol
    USE MODELPAR_ANT
    USE MODELTYPE_ANT,ONLY: ANTPOOlTYPE
    INTEGER,INTENT(IN)          ::   c 
    INTEGER,INTENT(IN)          ::   it
    REAL(KIND=8), INTENT(IN)    ::   sol_temp(0:max_soillayer)
    TYPE(ANTPOOLTYPE),INTENT(INOUT)  :: antpool
    REAL(KIND=8), INTENT(OUT)    ::  degrade(num_ant_type,2)
    !>LOCAL
    INTEGER iant,ilayer
    REAL(kind=8) :: k !!rate constant at temperature d-1
    REAL(kind=8) :: kt !!temperature codfficient
    REAL(kind=8) :: decay
    REAL(kind=8) :: dant
    REAL(kind=8) :: deg(num_ant_type,0:max_soillayer) !!mg/km2
    kt  = antpar_general(genid_kt)
    deg = 0.
    DO iant = 1, num_ant_type
      DO ilayer = 0, max_soillayer
        k = kt ** (sol_temp(ilayer)-20)
        decay = Exp(-(0.693/antusage(iant)%DTsoil) * k)
        if(antpool%m_dissol(iant,ilayer).ge.0)then
          deg(iant,ilayer)=antpool%m_dissol(iant,ilayer)*(1-decay)+antpool%im_dissol(iant,ilayer)*(1-decay)
          antpool%m_dissol(iant,ilayer)=antpool%m_dissol(iant,ilayer)*decay
          
        ! if(mantpool(iant,ilayer).ge.0)mantpool(iant,ilayer)=mantpool(iant,ilayer)*decay
        if(antpool%im_dissol(iant,ilayer).ge.0)antpool%im_dissol(iant,ilayer)=antpool%im_dissol(iant,ilayer)*decay
        endif
! if(iant==1.and.ilayer==0)write(996,'(F20.8)',ADVANCE="no")mantpool(1,0)!mg/L           
! if(iant==1.and.ilayer==1)write(997,'(F20.8)',ADVANCE="no")mantpool(1,1)!mg/L    
               
        decay_sol(iant,it,c,ilayer)=dant
      ENDDO
      degrade(iant,1)=deg(iant,0)
      degrade(iant,2)=sum(deg(iant,1:3))
    ENDDO
    
    antpool%m_antpool=antpool%m_dissol+antpool%m_sorbed

  END SUBROUTINE ant_degradation
!#################################################################################################################
!############################################################################################################
  SUBROUTINE ant_transfer(isoil,sthick,sol_water,antpool)

    USE MODELVAR_ANT, ONLY:  antusage
    USE MODELPAR_ANT
    USE MODELVAR,     ONLY:  bd_soil,       &
                             max_soillayer, &
                             num_ant_type, &
                             sw_pv
    USE MODELTYPE_ANT,ONLY:  ANTPOOlTYPE

    INTEGER,INTENT(IN)                   ::  isoil
    REAL(KIND=8), INTENT(IN)             ::  sthick(0:max_soillayer)    !!m
    REAL(KIND=8), INTENT(IN)             ::  sol_water(0:max_soillayer) !!mm
    TYPE(ANTPOOLTYPE),INTENT(INOUT)      ::  antpool                    !!mg/km2     
    !>local
    INTEGER ily,iant
    ! REAL(KIND=8) conc_mdis  !!mg/m3 the concentration of ant in the Dissolved phase of the Mobile or Immobile pool 
    ! REAL(KIND=8) conc_imsor !!mg/kg the concentration of ant in the sorbed phase of the Mobile or Immobile pool
    ! REAL(KIND=8) conc_imdis !!mg/m3 the concentration of ant in the Dissolved phase of the Mobile or Immobile pool 
    ! REAL(KIND=8) conc_msor  !!mg/kg the concentration of ant in the sorbed phase of the Mobile or Immobile pool
    REAL(KIND=8) R
    REAL(KIND=8) porosity
    REAL(KIND=8) bd         !!g/cm3
    REAL(KIND=8) kex        !!first-order mass transfer rate (day -1)
    REAL(KIND=8) kp         !!L/kg
    REAL(KIND=8) ex_ant(num_ant_type,0:max_soillayer) !mg/km2
    kex = antpar_general(genid_kex)

    DO ily = 0,max_soillayer

      IF(ily==0) THEN
        bd=bd_soil(ily+1,isoil)
      ELSE
        bd=bd_soil(ily,isoil)
      END IF

      DO iant=1,num_ant_type
        kp=antusage(iant)%Kpsoil
        porosity = 1.0 - bd/2.65
        IF(sw_pv(ily,isoil).gt.0)THEN
          antpool%conc_mdis(iant,ily) = (antpool%m_antpool(iant,ily))*1.e-6/&
                                        (sw_pv(ily,isoil)*1.e-3+kp*bd*sthick(ily))
          antpool%conc_msor(iant,ily) = antpool%conc_mdis(iant,ily)*1.e-3*kp

          antpool%conc_imdis(iant,ily) =(antpool%im_dissol(iant,ily)+antpool%im_sorbed(iant,ily))*1.e-6/&
                                        (sw_pv(ily,isoil)*1.e-3+kp*bd*sthick(ily))
          antpool%conc_imsor(iant,ily) = antpool%conc_imdis(iant,ily)*1.e-3*kp


          antpool%m_dissol(iant,ily)  = antpool%conc_mdis(iant,ily)*sw_pv(ily,isoil)*1.e-3*1.e6  !!mg/m3 * mm ->mg/km2
          antpool%m_sorbed(iant,ily)  = antpool%conc_msor(iant,ily)*bd*sthick(ily)*1.e3*1.e6

          antpool%im_dissol(iant,ily) = antpool%conc_imdis(iant,ily)*sw_pv(ily,isoil)*1.e-3*1.e6  !!mg/m3 * mm->mg/km2
          antpool%im_sorbed(iant,ily) = antpool%conc_imsor(iant,ily)*bd*sthick(ily)*1.e3*1.e6

!---------------
          R = 1 + antusage(iant)%Kpsoil*bd/porosity
          ex_ant(iant,ily)=(-kex*(antpool%m_dissol(iant,ily)-antpool%im_dissol(iant,ily)))/R
          antpool%m_dissol(iant,ily)=antpool%m_dissol(iant,ily)+abs(ex_ant(iant,ily))
          antpool%im_dissol(iant,ily)=antpool%im_dissol(iant,ily)-abs(ex_ant(iant,ily))
          
        
! if(iant==1.and.ily==0) write(996,'(2F20.8)',ADVANCE="no") antpool%m_dissol(iant,ily),antpool%m_sorbed(iant,ily) !mg/L
! if(iant==1.and.ily==0) write(996,*)
! if(iant==1.and.ily==1) write(997,'(2F20.8)',ADVANCE="no") antpool%m_dissol(iant,ily),antpool%m_sorbed(iant,ily) !mg/L
! if(iant==1.and.ily==1) write(997,*)
! ! if(iant==1.and.ily==1) write(996,'(2F20.8)',ADVANCE="no")antpool%m_dissol(iant,ily),antpool%m_sorbed(iant,ily) !mg/L
! if(iant==1.and.ily==1) write(997,'(3F20.8)',ADVANCE="no")antpool%m_antpool(iant,ily),&
! antpool%m_dissol(iant,ily),antpool%m_sorbed(iant,ily) !mg/L
        ENDIF
      END DO!<nub_substance
    END DO!<layer



  END SUBROUTINE ant_transfer
!############################################################################################################

  SUBROUTINE ant_add_pool(n,pool,source)

    INTEGER, INTENT(IN) :: n                          !<number of soillayers
    REAL(KIND=8), INTENT(INOUT) :: pool(n)            !<soil pool array
    REAL(KIND=8), INTENT(IN)    :: source(n)          !<amount to be added

    pool(:) = pool(:) + source(:)
  END SUBROUTINE ant_add_pool
  !##################################################################################################################
!##########################################################################################################
  SUBROUTINE int_antpool_antibiotic(isoil,iland,par_conc,sol_conc,sol_water,antpool,aq_water,gw_conc,gwantpool) 
    USE MODELVAR,ONLY :   max_soillayer,   &
                          num_ant_type,   &
                          cell_area,       &
                          sw_pv,&
                          soilthick,&
                          bd_soil,&
                          clay_soil,     &
                          silt_soil,     &
                          sand_soil
    USE MODELTYPE_ANT,ONLY:ANTPOOlTYPE

    INTEGER,INTENT(IN)            :: isoil
    INTEGER,INTENT(IN)            :: iland
    REAL(kind=8),INTENT(INOUT)    :: par_conc(num_ant_type,0:max_soillayer,4)!mg/g->mg/km2
                                    !1>clay;2>silt;3>sand;4>sum
    REAL(kind=8),INTENT(INOUT)    :: sol_conc(num_ant_type,0:max_soillayer)!mg/L-> mg/km2
    REAL(KIND=8), INTENT(INOUT)   :: sol_water(0:max_soillayer)
    
    REAL(KIND=8), INTENT(OUT)     :: aq_water !!mm
    REAL(KIND=8), INTENT(OUT)     :: gw_conc(num_ant_type) !!mg/L
    REAL(KIND=8), INTENT(OUT)     :: gwantpool(num_ant_type)!!mg/km2
    TYPE(ANTPOOlTYPE), INTENT(INOUT)  :: antpool

    INTEGER iant,ily
    REAL(KIND=8) :: mrate,imrate
    REAL(KIND=8) :: pool(num_ant_type,0:max_soillayer)
 
    INTEGER isly,is
    REAL(KIND=8) wt1
    DO iant=1,num_ant_type
      DO isly = 0, max_soillayer
        IF(soilthick(isly,isoil) .GT. 0) THEN!存在土层赋值pool，无土层�?0
          IF(isly==0)THEN
            is=1
          ELSE
            is=isly
          ENDIF
          wt1 = bd_soil(is,isoil) * soilthick(isly,isoil) * 1.0e6!g/cm3 * m ->g/m2
          par_conc(iant,isly,4)=wt1*cell_area*par_conc(iant,isly,4)/(cell_area*1.e-6) ! mg/km2
          par_conc(iant,isly,3)=wt1*cell_area*par_conc(iant,isly,3)/(cell_area*1.e-6)*sand_soil(is,isoil)*0.01
          par_conc(iant,isly,2)=wt1*cell_area*par_conc(iant,isly,2)/(cell_area*1.e-6)*silt_soil(is,isoil)*0.01
          par_conc(iant,isly,1)=wt1*cell_area*par_conc(iant,isly,1)/(cell_area*1.e-6)*clay_soil(is,isoil)*0.01 
        ELSE
          par_conc(iant,isly,:) = 0.
        END IF
      END DO
    END DO
    DO ily =0,max_soillayer
      DO iant= 1,num_ant_type
        ! IF(ily==0)THEN
        !   mrate=0.9
        !   imrate=0.1
        ! ELSE
          mrate=0.3
          imrate=0.7
        ! ENDIF

        antpool%m_dissol (iant,ily) = sol_conc(iant,ily)*sw_pv(ily,isoil) *1.e6 *mrate
        antpool%im_dissol(iant,ily) = sol_conc(iant,ily)*sw_pv(ily,isoil) *1.e6 *imrate
        antpool%m_sorbed (iant,ily) = par_conc(iant,ily,4) *mrate
        antpool%im_sorbed(iant,ily) = par_conc(iant,ily,4) *imrate
        antpool%m_antpool(iant,ily) = antpool%m_dissol (iant,ily)+antpool%m_sorbed (iant,ily)

      END DO
    END DO
    !----gwantpool----
    gwantpool=gw_conc*aq_water*1.e6 !mg/km2

  END SUBROUTINE int_antpool_antibiotic

  !##################################################################################################################
!############################################################################################################
  SUBROUTINE output
    USE DATETIME
    USE DATETIME,       ONLY  :     in_season_period 
    USE MODELTYPE,      ONLY  :     TIMEINFORMATIONTYPE
    USE MODELVAR, ONLY: sim_length,num_cell,subbasin,landuse,soiltype,times_sim,cell_area,date_sim,&
                        num_ant_type,num_sub
    USE MODELVAR_ANT,ONLY: source_sub,peosource,animsource,antcropdata,airdrydep,init_sol,end_sol,&
                           decay_sol,celllo,cellloyr
    INTEGER :: i, t, k, n,j,ik,bd2,bd5,c,iland,count_cell
    INTEGER :: il, is, ib, ip,idt,updays
    INTEGER ::sumcell(4)!!1>animal 2>peo !>air !>sub
    REAL(KIND=8):: emh_s(4) !!mg/km2/d
    TYPE(TIMEINFORMATIONTYPE) :: cur_time
    REAL(KIND=8):: grid(num_ant_type)
    REAL(KIND=8):: init(num_ant_type)
    REAL(KIND=8):: last(4)
    REAL(KIND=8):: cell_source(num_cell,num_ant_type,3)!!mg/yr 1>animal 2>peo !3>sum !>sub
    REAL(KIND=8):: sub_source(num_ant_type,num_sub,2)

    grid=0.
    init=0.
    updays=20
    cell_source=0.
    count_cell =0
      !   write(996,*) clro_sub(20,idt,1),clro_sub(20,idt,2)  
      
      DO c=1,num_cell
      ! DO c= 51720, 51720
          ib=subbasin(c);iland=landuse(c);is=soiltype(c)
          IF(iland.EQ.-9999.OR.is.EQ.-9999.OR.ib.EQ.-9999) CYCLE
          do k=1,4
                    cellloyr(k,c,1)=sum(celllo(k,c,1828:2192,1))
                    cellloyr(k,c,2)=sum(celllo(k,c,1828:2192,2))
                    cellloyr(k,c,3)=sum(celllo(k,c,1828:2192,3))
                    cellloyr(k,c,4)=sum(celllo(k,c,1828:2192,4))
                    cellloyr(k,c,5)=sum(celllo(k,c,1828:2192,5))

            ! cellloyr(k,c,1)=sum(celllo(k,c,1827:1917,1))
            ! cellloyr(k,c,2)=sum(celllo(k,c,1918:2008,1))
            ! cellloyr(k,c,3)=sum(celllo(k,c,2009:2100,1))
            ! cellloyr(k,c,4)=sum(celllo(k,c,2101:2193,1))

          !           ! init(k)=init(k)+init_sol(k,c,0)+init_sol(k,c,1)+init_sol(k,c,2)
          !           ! grid(k)=grid(k)+end_sol(k,c,0)+end_sol(k,c,1)+end_sol(k,c,2)     
          !           ! last(k)= last(k)+end_sol(k,c,0)+end_sol(k,c,1)+end_sol(k,c,2)
                    
          !           ! grid_sol(k,0)=grid_sol(k,0)+end_sol(k,c,0)
          !           ! grid_sol(k,1)=grid_sol(k,1)+end_sol(k,c,1)
          !           ! grid_sol(k,2)=grid_sol(k,2)+end_sol(k,c,2)
          !           ! grid_sol(k,3)=grid_sol(k,3)+end_sol(k,c,3)
          end do
! last(1)=last(1)+end_sol(1,c,0)+end_sol(1,c,1)+end_sol(1,c,2)
! write(996,*)c,is,iland,init_sol(1,c,2)
          ! sumcell(4)=sumcell(4)+1
          ! if(animsource(c,1).gt.0)sumcell(1)=sumcell(1)+1
        DO idt=1,sim_length
          CALL define_current_time(it         =  idt,                           &          
                                   date       =  times_sim(idt),                &
                                   cur_time   =  cur_time                       &
                                  )
          source_sub(:,ib,idt,1)=source_sub(:,ib,idt,1)+animsource(c,:)*0.0025
          cell_source(c,:,1)=cell_source(c,:,1)+animsource(c,:)*0.0025
          last(:)= last(:)+decay_sol(:,idt,c,0)+decay_sol(:,idt,c,1)+decay_sol(:,idt,c,2)
          DO k = 1, 2
          !   !------------------------------------------------------------------------------------------
          !   !>rice (4-paddyfield)and wheat(2-dryland)
            IF(iland.EQ.4) THEN  !
              IF(k.NE.1) CYCLE 
            ELSEIF(iland.EQ.2) THEN 
              IF(k.NE.2) CYCLE 
            END IF
          !   ik =2*k
            bd2 = antcropdata(k)%baredayno2        !sow date / beginning of growing season 
            bd5 = antcropdata(k)%baredayno5        !winter crops sowing date
      
            IF(bd2.ne.0)THEN
              IF(in_season_period(bd2,updays,cur_time))THEN!
                emh_s(:) = peosource(c,:)/updays
                source_sub(:,ib,idt,2)=source_sub(:,ib,idt,2)+peosource(c,:)/updays*0.0025
                cell_source(c,:,2)=cell_source(c,:,2)+peosource(c,:)/updays*0.0025
              ENDIF
            ELSEIF(bd5.ne.0)THEN
              IF(in_season_period(bd5,updays,cur_time))THEN
                source_sub(:,ib,idt,2)=source_sub(:,ib,idt,2)+peosource(c,:)/updays*0.0025
                cell_source(c,:,2)=cell_source(c,:,2)+peosource(c,:)/updays*0.0025
              ENDIF
            ENDIF
          !     ! write(6,*)emh_s(:)
            
            ! source_sub(:,ib,idt,2)=source_sub(:,ib,idt,2)+emh_s(:)*0.0025
          ENDDO  
      
        END DO
        cell_source(c,:,3)=cell_source(c,:,1)+cell_source(c,:,2)

      ENDDO

      
    !   DO i = 1, sim_length
    !     IF(i.gt.1827 .and. i.lt.2193)THEN
    !     WRITE(992, '(A11)', ADVANCE='NO') date_sim(i)
    !   ! DO j = 1, 24
    !     ! WRITE(996, '(F20.4)',ADVANCE='NO') source_sub(1,j,i,1)+source_sub(1,j,i,2)+source_sub(1,j,i,3) !!mg/km2/d
    !   !   WRITE(992, *) sum(source_sub(1,:,i,1)),sum(source_sub(2,:,i,1)),&
    !   !                 sum(source_sub(3,:,i,1)),sum(source_sub(4,:,i,1)),& !!mg/d
    !   !                 sum(source_sub(1,:,i,2)),sum(source_sub(2,:,i,2)),&
    !   !                 sum(source_sub(3,:,i,2)),sum(source_sub(4,:,i,2))
    !     write(992,*) sum(source_sub(1,:,i,1))+sum(source_sub(1,:,i,2))+&
    !                  sum(source_sub(2,:,i,1))+sum(source_sub(2,:,i,2))+&
    !                  sum(source_sub(3,:,i,1))+sum(source_sub(3,:,i,2))+&
    !                  sum(source_sub(4,:,i,1))+sum(source_sub(4,:,i,2))
    !   ! END DO
    ! !     ! WRITE(992, *)
    !     ENDIF
    !  END DO
      ! write(6,*)sumcell(:)
      ! write(996,*)last(:),init(:)
      !!>1>discharge 2>degradation 3>leaching 4>mixinglayer loss 5>soillayer loss

     

    !  do k=1,num_cell
    !   ! do k= 1,2570
    !     il=landuse(k);is=soiltype(k);ib=subbasin(k)
    !     ! if(peosource(k,1).gt.0)write(997,*)peosource(k,1),k
    !     IF(il.EQ.-9999.OR.is.EQ.-9999.OR.ib.EQ.-9999) THEN
    !       IF(mod(k,258).eq.0) THEN
    !         write(666,"(I6)" ,advance="no")-9999
    !         WRITE(666,*)
    !         write(665,"(I6)" ,advance="no")-9999
    !         WRITE(665,*)
    !         write(664,"(I6)" ,advance="no")-9999
    !         WRITE(664,*)
    !         write(663,"(I6)" ,advance="no")-9999
    !         WRITE(663,*)
    !         write(662,"(I6)" ,advance="no")-9999
    !         WRITE(662,*)
    !       else
    !         write(666,"(I6)" ,advance="no")-9999
    !         write(665,"(I6)" ,advance="no")-9999
    !         write(664,"(I6)" ,advance="no")-9999
    !         write(663,"(I6)" ,advance="no")-9999
    !         write(662,"(I6)" ,advance="no")-9999
    !       endif
    !     ELSE
          
    !       IF(mod(k,258).eq.0) THEN
    !         ! write(996,"(F20.4)",advance="no") animsource(k,1)
    !         ! write(666,"(F20.4)",advance="no") sum(cellloyr(:,k,1))
    !         ! write(666,"(F20.4)",advance="no") sum(cell_source(k,:,3))
    !         ! write(665,"(F20.4)",advance="no") sum(cellloyr(:,k,2))
    !         ! write(664,"(F20.4)",advance="no") sum(cellloyr(:,k,3))
    !         ! write(663,"(F20.4)",advance="no") sum(cell_source(k,:,3))*0.3-sum(cellloyr(:,k,4))
    !         ! write(662,"(F20.4)",advance="no") sum(cell_source(k,:,3))*0.7-sum(cellloyr(:,k,5))

    !         write(666,"(F20.4)",advance="no")sum(cellloyr(:,k,1))/(0.25)
    !         write(665,"(F20.4)",advance="no")sum(cellloyr(:,k,2))/(0.25)
    !         write(664,"(F20.4)",advance="no")sum(cellloyr(:,k,3))/(0.25)
    !         write(663,"(F20.4)",advance="no")sum(cellloyr(:,k,4))/(0.25)
    !         ! write(662,"(F20.4)",advance="no")sum(cellloyr(:,k,1))

    !         ! if(animsource(k,1).gt.0)write(6,*)animsource(k,1)
    !   !       ! write(996,"(F16.4)",advance="no") outanim(k,5)
    !   !       ! write(996,"(F16.4)",advance="no") outpeo(k,4)
    !         WRITE(666,*)
    !         WRITE(665,*)
    !         WRITE(664,*)
    !         WRITE(663,*)
    !         WRITE(662,*)
    !       ELSE
    !         ! write(996,"(F20.4)",advance="no") animsource(k,1)
    !         ! write(666,"(F20.4)",advance="no") sum(cellloyr(:,k,1))!sum(cell_source(k,:,1))
    !         ! write(666,"(F20.4)",advance="no") sum(cell_source(k,:,3))
    !         ! write(665,"(F20.4)",advance="no") sum(cellloyr(:,k,2))
    !         ! write(664,"(F20.4)",advance="no") sum(cellloyr(:,k,3))
    !         ! write(663,"(F20.4)",advance="no") sum(cell_source(k,:,3))*0.3-sum(cellloyr(:,k,4))
    !         ! write(662,"(F20.4)",advance="no") sum(cell_source(k,:,3))*0.7-sum(cellloyr(:,k,5))
    !   !       write(996,"(F16.4)",advance="no") outant(k,5)
    !   !       ! write(996,"(F16.4)",advance="no") outanim(k,5)
    !   !       ! write(996,"(F16.4)",advance="no") outpeo(k,4)

    !         write(666,"(F20.4)",advance="no")sum(cellloyr(:,k,1))/(0.25)
    !         write(665,"(F20.4)",advance="no")sum(cellloyr(:,k,2))/(0.25)
    !         write(664,"(F20.4)",advance="no")sum(cellloyr(:,k,3))/(0.25)
    !         write(663,"(F20.4)",advance="no")sum(cellloyr(:,k,4))/(0.25)

    !       ENDIF
    !     ENDIF
    !   enddo

  do j=1,num_sub
    do k =1,4
      sub_source(k,j,1)=sum(source_sub(k,j,1828:2192,1))
      sub_source(k,j,2)=sum(source_sub(k,j,1828:2192,2)) !!mg
  end do
  ! write(996,*)j,sum(sub_source(:,j,1)),sum(sub_source(:,j,2))          
end do

! write(996,*)(sum(sub_source(1,:,1))+sum(sub_source(1,:,2)))*1.e-6
! write(996,*)(sum(sub_source(2,:,1))+sum(sub_source(2,:,2)))*1.e-6
! write(996,*)(sum(sub_source(3,:,1))+sum(sub_source(3,:,2)))*1.e-6
! write(996,*)(sum(sub_source(4,:,1))+sum(sub_source(4,:,2)))*1.e-6
  END SUBROUTINE output

!############################################################################################################
  SUBROUTINE outload
    USE DATETIME
    USE MODELVAR, ONLY: sim_length,num_cell,date_sim,num_ant_type,num_sub,num_landuse
    USE MODELVAR_ANT,ONLY:antsub2riv,deg_sub,init_sub,end_sub,leach_sub,sulo_sub,solo_sub,palo_sub,gwlo_sub,&
                          path_sub
    INTEGER i, j,k
    REAL(KIND=8) suload(sim_length,num_ant_type)
    REAL(KIND=8) soload(sim_length,num_ant_type)
    REAL(KIND=8) gwload(sim_length,num_ant_type)
    REAL(KIND=8) paload(sim_length,num_ant_type)
    REAL(KIND=8) degload1(sim_length,num_ant_type)
    REAL(KIND=8) degload2(sim_length,num_ant_type)
    REAL(KIND=8) iniload(sim_length,num_ant_type,2) !!1>par 2>dis
    REAL(KIND=8) endload(sim_length,num_ant_type,2)
    REAL(KIND=8) leachload(sim_length,num_ant_type)
    REAL(KIND=8) dischargeload(sim_length,num_ant_type)


    REAL(KIND=8) path1(num_sub,sim_length,4,3) !!sum of four Tcs !林地 2>农田 3>其他
    REAL(KIND=8) path2(num_sub,sim_length,4)
    REAL(KIND=8) path3(num_sub,sim_length)
    suload=0.;soload=0.;gwload=0.;paload=0.;degload1=0.;endload=0.;leachload=0.;iniload=0.;degload2=0.;path1=0.;path2=0.
    path3=0.
    

  DO i = 1, sim_length
    IF(i.gt.1827 .and. i.lt.2193)THEN
      WRITE(993, '(A11)', ADVANCE='NO') date_sim(i)
      
      
      DO j = 1,24
        
        !     ! WRITE(996, '(F10.4)',ADVANCE='NO') sub2riv(j)%sed(1,1,i)
          ! WRITE(996,*) sub2riv(11)%gwflow(i)!sum(gwro_sub(:,i))
            ! suload(i,:)=suload(i,:)+antsub2riv(j)%suload(:,i)
            ! soload(i,:)=soload(i,:)+antsub2riv(j)%soload(:,i)
            ! gwload(i,:)=gwload(i,:)+antsub2riv(j)%gwload(:,i)
            ! paload(i,:)=paload(i,:)+antsub2riv(j)%paload(:,i)
            suload(i,:)=suload(i,:)+sulo_sub(:,j,i)*2500 !kg
            soload(i,:)=soload(i,:)+solo_sub(:,j,i)*2500!kg
            gwload(i,:)=gwload(i,:)+gwlo_sub(:,j,i)*2500!kg
            paload(i,:)=paload(i,:)+palo_sub(:,j,i)*2500!kg
            degload1(i,:) = degload1(i,:) + deg_sub(:,j,i,1) !!mg
            degload2(i,:) = degload2(i,:) + deg_sub(:,j,i,2) !!mg
            leachload(i,:) = leachload(i,:) + leach_sub(:,j,i) !!mg

            iniload(i,:,1) = iniload(i,:,1) + init_sub(:,j,i,1)
            iniload(i,:,2) = iniload(i,:,2) + init_sub(:,j,i,2)
            endload(i,:,1) = endload(i,:,1) + end_sub(:,j,i,1)
            endload(i,:,2) = endload(i,:,2) + end_sub(:,j,i,2)
            dischargeload(i,:)=suload(i,:)+soload(i,:)+gwload(i,:)+paload(i,:)
            ! WRITE(997,*) antsub2riv(11)%suload(1,i), antsub2riv(11)%soload(1,i),antsub2riv(11)%paload(1,i),antsub2riv(11)%gwload(1,i)!!kg/d 
            ! WRITE(996, '(F20.16)',ADVANCE='NO') antsub2riv(j)%load(1,i)+antsub2riv(j)%paload(1,i)+antsub2riv(j)%gwload(1,i)!!kg/d 
              do k=1,4
                path1(j,i,k,1)=sum(path_sub(:,j,i,k,1))
                path1(j,i,k,2)=sum(path_sub(:,j,i,k,2))+sum(path_sub(:,j,i,k,4))
                path1(j,i,k,3)=sum(path_sub(:,j,i,k,3))+sum(path_sub(:,j,i,k,5))

                path2(j,i,k)=sum(path_sub(:,j,i,k,1))+sum(path_sub(:,j,i,k,2))+sum(path_sub(:,j,i,k,3))+&
                              sum(path_sub(:,j,i,k,4))+sum(path_sub(:,j,i,k,5))  

                path3(j,i)=sum(path2(j,i,:))
              end do
     
       END DO
      ! WRITE(993,*) sum(suload(i,:)),sum(soload(i,:)),sum(paload(i,:)),sum(gwload(i,:))
                ! ,degload1(i,:),degload2(i,:),iniload(i,:,1),iniload(i,:,2),&
                  !  endload(i,:,1),endload(i,:,2),leachload(i,:) !!kg/d
          ! WRITE(996,*)sum(leachload(:,1))
    ENDIF
  END DO
  ! WRITE(996,*) sum(soload(1828:2192,1))+sum(suload(1828:2192,1))+&
  !              sum(soload(1828:2192,2))+sum(suload(1828:2192,2))+&
  !              sum(soload(1828:2192,3))+sum(suload(1828:2192,3))+&
  !              sum(soload(1828:2192,4))+sum(suload(1828:2192,4))
  ! WRITE(996,*) sum(soload(1828:2192,1))+sum(suload(1828:2192,1)),endload(2192,1,1)*1.e-6,endload(2192,1,2)*1.e-6!2021 year
  ! WRITE(996,*) sum(soload(1828:2192,2))+sum(suload(1828:2192,2)),endload(2192,2,1)*1.e-6,endload(2192,2,2)*1.e-6
  ! WRITE(996,*) sum(soload(1828:2192,3))+sum(suload(1828:2192,3)),endload(2192,3,1)*1.e-6,endload(2192,3,2)*1.e-6
  ! WRITE(996,*) sum(soload(1828:2192,4))+sum(suload(1828:2192,4)),endload(2192,4,1)*1.e-6,endload(2192,4,2)*1.e-6
  ! ! WRITE(996,*) endload(1828,1,1)*1.e-6+endload(1828,1,2)*1.e-6!2021 year
  ! WRITE(996,*) endload(1828,2,1)*1.e-6+endload(1828,2,2)*1.e-6
  ! WRITE(996,*) endload(1828,3,1)*1.e-6+endload(1828,3,2)*1.e-6
  ! WRITE(996,*) endload(1828,4,1)*1.e-6+endload(1828,4,2)*1.e-6
  ! WRITE(996,*) endload(2192,1,1)*1.e-6+endload(2192,1,2)*1.e-6!2021 year
  ! WRITE(996,*) endload(2192,2,1)*1.e-6+endload(2192,2,2)*1.e-6
  ! WRITE(996,*) endload(2192,3,1)*1.e-6+endload(2192,3,2)*1.e-6
  ! WRITE(996,*) endload(2192,4,1)*1.e-6+endload(2192,4,2)*1.e-6
  ! WRITE(996,*) (sum(leachload(1828:2192,1))+sum(degload1(1828:2192,1))&
  !             +sum(degload2(1828:2192,1))+sum(paload(1828:2192,1))+sum(gwload(1828:2192,1)))*1.e-6
  ! WRITE(996,*) (sum(leachload(1828:2192,2))+sum(degload1(1828:2192,2))&
  !             +sum(degload2(1828:2192,2))+sum(paload(1828:2192,2))+sum(gwload(1828:2192,2)))*1.e-6
  ! WRITE(996,*) (sum(leachload(1828:2192,3))+sum(degload1(1828:2192,3))&
  !             +sum(degload2(1828:2192,3))+sum(paload(1828:2192,3))+sum(gwload(1828:2192,3)))*1.e-6
  ! WRITE(996,*) (sum(leachload(1828:2192,4))+sum(degload1(1828:2192,4))&
  !             +sum(degload2(1828:2192,4))+sum(paload(1828:2192,4))+sum(gwload(1828:2192,4)))*1.e-6
  DO i = 1, sim_length
    IF(i.gt.1827 .and. i.lt.2193)THEN
  !   WRITE(881, '(A11)', ADVANCE='NO') date_sim(i)
  !   WRITE(880, '(A11)', ADVANCE='NO') date_sim(i)
  !   WRITE(881,*)sum(path1(:,i,1,1)),sum(path1(:,i,2,1)),sum(path1(:,i,3,1)),sum(path1(:,i,4,1)),& !!林地
  !   sum(path1(:,i,1,2)),sum(path1(:,i,2,2)),sum(path1(:,i,3,2)),sum(path1(:,i,4,2)),&
  !   sum(path1(:,i,1,3)),sum(path1(:,i,2,3)),sum(path1(:,i,3,3)),sum(path1(:,i,4,3))
  !   ! WRITE(880,*)path2(:,i,1),path2(:,i,2),path2(:,i,3),path2(:,i,4)
  !   WRITE(880,*)path3(:,i)
      ! write(996,*)date_sim(i),(suload(i,1)+soload(i,1)+gwload(i,1)+paload(i,1))*1.e3,&
      !                         (suload(i,2)+soload(i,2)+gwload(i,2)+paload(i,2))*1.e3,&
      !                         (suload(i,3)+soload(i,3)+gwload(i,3)+paload(i,3))*1.e3,&
      !                         (suload(i,4)+soload(i,4)+gwload(i,4)+paload(i,4))*1.e3
      write(996,*)date_sim(i),sum(suload(i,:))*1.e6,sum(soload(i,:))*1.e6,sum(gwload(i,:))*1.e6,sum(paload(i,:))*1.e6,&
      sum(leachload(i,:))!!unit:mg
    endif
  enddo
  END SUBROUTINE outload
END MODULE SOIL_ANT