MODULE SURFACEWATER
  IMPLICIT NONE
CONTAINS
!#################################################################################################
  SUBROUTINE route_rch_1ts(it,             & 
                           para_map,       &
                           sub_to_rivs,    & 
                           rivs,           & 
                           q_up,           &
                           rivstates,      & 
                           rivfluxes,      &
                           sedstates,      &
                           sedfluxes,      &
                           ierr,           & 
                           message,&
                           antistate)    
    USE OMP_LIB
    USE EROSION
    USE MODELTYPE
    USE MODELTYPE_ANT,ONLY:ANTSTATETYPE
    USE DATETIME
    USE REACH_ANT
    USE REACH_SED
    USE MODELTYPE_SED, ONLY :  SEDSTATETYPE, SEDFLUXTYPE
    USE MODELVAR_SED, ONLY :   sedresp
    USE MODELPAR_SED 
    USE MODELVAR,  ONLY   :   num_sub,        &
                              flow_node,      &
                              riv_out_unit,   &
                              num_plevel,     &
                              num_riv,        &
                              simcfg,         &
                              warm_length,    &
                              upID,           &
                              num_ant_type,  &
                              out_hot,        &
                              num_threads,    &
                              times_sim,      &
                              temp_air,       &
                              prec_gauge,     &
                              date_sim,sim_length
    USE MODELVAR_ANT,ONLY :antsub2riv,antusage,airdrydep,wetdep,rchout,rchload,rchdeg,rchmass,rchburl,rchstart
    INTEGER, INTENT(IN)                       ::    it 
    TYPE(pair), INTENT(IN)                    ::    para_map(8,24)      
    TYPE(SUB2RIVTYPE), INTENT(IN)             ::    sub_to_rivs(num_sub)                          
    TYPE(RIVERTYPE), INTENT(IN)               ::    rivs(num_riv)
    REAL(KIND=8), INTENT(INOUT)               ::    q_up(num_riv)
    TYPE(RIVSTATETYPE), INTENT(INOUT)         ::    rivstates(num_riv)
    TYPE(RIVFLUXTYPE), INTENT(INOUT)          ::    rivfluxes(num_riv)
    TYPE(SEDSTATETYPE), INTENT(INOUT)         ::    sedstates(num_riv)
    TYPE(SEDFLUXTYPE), INTENT(INOUT)          ::    sedfluxes(num_riv)
    TYPE(ANTSTATETYPE), INTENT(INOUT)         ::    antistate(num_riv)
    
    INTEGER, INTENT(OUT)                 ::    ierr               
    CHARACTER(*), INTENT(OUT)            ::    message     

    TYPE(TIMEINFORMATIONTYPE) :: c_time
    INTEGER  ::  idt,i
    INTEGER  ::  iriv
    INTEGER  ::  ilayer,ib, k, up, qq
    INTEGER  ::  nodeE, downID
    REAL(KIND=8)        ::  rchpet
    REAL(KIND=8)        ::  precipitation
    REAL(KIND=8)        ::  precvol  !!m3
    REAL(KIND=8)        ::  precipitation_vol  !!m3
    !-------sed-----------
    REAL(KIND=8)        ::  erosion_up_c(num_sub)   !<kg
    REAL(KIND=8)        ::  erosion_up_l(num_sub)   !<kg
    REAL(KIND=8)        ::  erosion_up_s(num_sub)   !<kg
    REAL(KIND=8)        ::  rchvol
    REAL(KIND=8)        ::  TAU
    REAL(KIND=8)        ::  KS
    REAL(KIND=8)        ::  sed_resus(4)
    REAL(KIND=8)        ::  sed_settle(4),input(num_ant_type,sim_length)

    !-----antibiotic------
    REAL(KIND=8)        ::  dissol_up(num_sub,num_ant_type)!!mg
    REAL(KIND=8)        ::  partic_up(num_sub,num_ant_type)!!mg
    REAL(KIND=8)        ::  outant_p(num_ant_type)!!mg
    REAL(KIND=8)        ::  outant_d(num_ant_type)!!mg
    REAL(KIND=8)        ::  frsol(num_ant_type)       !! fraction of pesticide in reach that is soluble
    REAL(KIND=8)        ::  frsrb       !! fraction of pesticide in reach that is sorb

    REAL(KIND=8)        ::  deg(num_riv,num_ant_type)
    REAL(KIND=8)        ::  burl(num_riv,num_ant_type)
    REAL(KIND=8)        ::  rchint(num_ant_type,sim_length)
    ! REAL(KIND=8)        ::  rchend(num_ant_type,sim_length)
    ! REAL(KIND=8)        ::  rchout(num_ant_type,sim_length)
    REAL(KIND=8)        ::  load(num_riv,num_ant_type)
    ! REAL(KIND=8)        ::  load(num_riv,num_ant_type,sim_length)
    REAL(KIND=8)        ::  out(num_riv,num_ant_type)
    !----------------------
    ierr=0; message='Error in mc_route. '
    ! unit : sedstate%clayconc mg/L  sedstate%claypool kg
    ! unit : parconc mg/kg   conc mg/L
    !>传递上一时刻的上游来水，和上一时刻的河道初始水体积
    DO k = 1, num_riv
      rivfluxes(k)%VOL0 = rivfluxes(k)%VOL(0)
      
    END DO

    CALL omp_set_num_threads(1) 
! print *, "OK2"
    !$OMP PARALLEL DO PRIVATE(ilayer,ib,idt,q_up,iriv)
    DO ilayer = 1, num_plevel
      DO iriv = 1, num_riv
      ! DO iriv = 1, num_sub
        ib  = para_map(ilayer,iriv)%x
        idt = para_map(ilayer,iriv)%y
        
        IF(ib.EQ.0)  CYCLE
        IF(idt.EQ.0) CYCLE

        ! DO i = 1, SIZE(yongan)
        !   IF(iriv.EQ.yongan(i)) THEN 
            ! isub = rivs(iriv)%basinID   !<Match drainage basin to riv
            !>q_up & erosion_up calculation
            q_up(:)=0.
            erosion_up_c(:)=0.
            erosion_up_l(:)=0.
            erosion_up_s(:)=0.
            dissol_up(:,:)=0. !!mg
            partic_up(:,:)=0. !!mg
            out(:,:)=0.
            load(:,:)=0.

            DO qq = 2, num_plevel
              IF(it.GE.qq) THEN 
                !>level qq
                DO k = 1, num_riv
                  IF(rivs(k)%plevel.NE.qq) CYCLE  
                  DO up = 1, 5  !<Max number of upstreams set to 5
                    IF(upID(up,k).EQ.0) CYCLE
                    q_up(k)=q_up(k)+rivfluxes(upID(up,k))%flow(idt)

                    !>记录当前时刻的上游来
                    rivfluxes(k)%q_up(1) = q_up(k)

                    erosion_up_c(k)=erosion_up_c(k)+ sub_to_rivs(upID(up,k))%erosion_c(idt)+&
                                    sedstates(upID(up,k))%sedload(1)
                    erosion_up_l(k)=erosion_up_l(k)+ sub_to_rivs(upID(up,k))%erosion_l(idt)+&
                                    sedstates(upID(up,k))%sedload(2)
                    erosion_up_s(k)=erosion_up_s(k)+ sub_to_rivs(upID(up,k))%erosion_s(idt)+&
                                    sedstates(upID(up,k))%sedload(3)

                    dissol_up(k,:)=dissol_up(k,:)+antsub2riv(upID(up,k))%load(:,idt)*1.e6+&
                                    antsub2riv(upID(up,k))%gwload(:,idt)*1.e6+&
                                    antistate(upID(up,k))%outant_d(:)
                    partic_up(k,:)=partic_up(k,:)+antsub2riv(upID(up,k))%paload(:,idt)*1.e6+&
                                   antistate(upID(up,k))%outant_p(:)

                    ! dissol_up(k,:)=dissol_up(k,:)+&
                    !                 antistate(upID(up,k))%outant_d(:)

                    ! partic_up(k,:)=partic_up(k,:)+&
                    !                 antistate(upID(up,k))%outant_p(:)

                    load(k,:)=load(k,:)+antsub2riv(upID(up,k))%load(:,idt)*1.e6+&
                              antsub2riv(upID(up,k))%gwload(:,idt)*1.e6+&
                              antsub2riv(upID(up,k))%paload(:,idt)*1.e6

                    ! out(upID(up,k),:)=antistate(upID(up,k))%outant_d(:)+antistate(upID(up,k))%outant_p(:)
                    ! rchout(upID(up,k),:,idt)=antistate(upID(up,k))%outant_d(:)+antistate(upID(up,k))%outant_p(:)
                    ! out(upID(up,k),:) = antistate(upID(up,k))%outant_d(:)+antistate(upID(up,k))%outant_p(:)
                    ! rchout(upID(up,k),:,idt)=out(upID(up,k),:) 
                    ! antistate(upID(up,k))%outant_d1(:,idt)=antistate(upID(up,k))%outant_d(:)+antistate(upID(up,k))%outant_p(:)
                  END DO
                END DO
              END IF 
            END DO

          CALL define_current_time(idt,times_sim(idt),c_time)
          CALL rch_pet(c_time,temp_air(idt,1),rchpet)
          CALL rch_temp(temp_air(idt,1),rivstates(iriv))

          precipitation=prec_gauge(idt,rivs(iriv)%gaugeID)
          precvol=(precipitation-rchpet)*1.e-3*rivs(iriv)%length*rivs(iriv)%width  !!<m3
          precipitation_vol=precipitation*1.e-3*rivs(iriv)%length*rivs(iriv)%width !!<m3
          ! precvol=0.
            
          CALL muskingum_cunge(idt,                        &
                               rivs(iriv),                 &
                               q_up(iriv),                 &
                               sub_to_rivs(iriv),          &
                               rivstates(iriv),            &
                               rivfluxes(iriv),            &   
                               ierr,message                &
                               )  

          rivfluxes(iriv)%VOL(1)=MAX(rivfluxes(iriv)%VOL(1)+precvol,1.e-6)                      
!################################################################################################   
      CALL advect_sed(iriv,                    &
                      idt,                     & 
                      q_up(iriv),              &
                      precvol,                 &
                      erosion_up_c(iriv),      &
                      erosion_up_l(iriv),      &
                      erosion_up_s(iriv),      &
                      sub_to_rivs(iriv),       &
                      rivstates(iriv),         &
                      rivfluxes(iriv),         &
                      sedstates(iriv)          &
                      )              
                        
      rivfluxes(iriv)%q_up(0) = rivfluxes(iriv)%q_up(1)

      CALL rch_resuspended( iriv,                            &
                            idt ,                            &
                            sedpar_general(genid_rchwclay),  &
                            sedpar_general(genid_rchwsilt),  &
                            sedpar_general(genid_rchmclay),  &
                            sedpar_general(genid_rchmsilt),  &
                            sedpar_general(genid_rchvpeak),  &
                            sedpar_general(genid_rchsmcoef), &
                            sedpar_general(genid_rchsmexp),  &
                            sedpar_general(genid_rchk),      &
                            sedpar_general(genid_rchc),      &
                            rivfluxes(iriv)%VOL(1),          &
                            rivs(iriv),                      &
                            rivfluxes(iriv),                 &
                            sedstates(iriv),                 &
                            TAU,                             &
                            sed_resus,                       &
                            sed_settle                       &
                            )

          !>Update sediment concentration and load
          !>Note: sedflux%sedload = sedstate%load + sub2riv load
          sedfluxes(iriv)%spsconc(4,idt) = sedstates(iriv)%sedconc
          sedfluxes(iriv)%sedload(4,idt) = sedstates(iriv)%sedload(4) + &
                                           sub_to_rivs(iriv)%erosion_c(idt)+sub_to_rivs(iriv)%erosion_s(idt)&
                                           +sub_to_rivs(iriv)%erosion_s(idt)

           CALL rch_ant(ir         = iriv,                       &
                        it         = idt,                        &
                        rch_temp   = rivstates(iriv)%temp,       &
                        fsol       = frsol,                      &
                        sed_resus  = sed_resus,                  &
                        sed_settle = sed_settle,                 &
                        partic_up  = partic_up(iriv,:),          &
                        dissol_up  = dissol_up(iriv,:),          &
                        riv        = rivs(iriv),                 &
                        rivstate   = rivstates(iriv),            &
                        rivflux    = rivfluxes(iriv),            &
                        sedstate1  = sedstates(iriv),             &
                        antstate1  = antistate(iriv),            &
                        burl  = antistate(iriv)%bural(:,idt),    &
                        deg   = antistate(iriv)%deg(:,idt,:),    &
                        dif   = antistate(iriv)%antdif(:,idt))
                 

    
antistate(iriv)%load(:,idt)=load(iriv,:)
! IF(rivs(iriv)%nodeB.gt.0)antistate(iriv)%mass(:,idt,1)=antistate(iriv)%chantmass(:)
! IF(rivs(iriv)%nodeB.gt.0)antistate(iriv)%mass(:,idt,2)=antistate(iriv)%sedantmass(:)
antistate(iriv)%antconc(:,idt)=antistate(iriv)%conc(:)
! if(iriv==24.and.idt.gt.1826 .and. idt.lt.2193)write(998,*)TAU,sedstate(iriv)%sedconc,antistate(iriv)%conc(:)*1.e3
! if(iriv==24.and.idt.gt.1827.and.idt.lt.2193)write(664,*)sed_settle(1),sed_settle(2),sed_settle(3),&
!                         sed_resus(1),sed_resus(2),sed_resus(3)
! if(iriv==5)write(998,*) antistate(23)%bedconc(:)*1.e3
antistate(iriv)%mass(:,idt,1)=antistate(iriv)%chantmass(:)+antistate(iriv)%sedantmass(:) 

! !-----test +干沉�?---
!         antistate(iriv)%spsantmass(:,4)=antistate(iriv)%spsantmass(:,4)+&
!         airdrydep(:)*rivs(iriv)%length*rivs(iriv)%width*1.e-6  !m2->km2
! !-----test +湿沉�?---
!         wetdep=5
!         antistate(iriv)%disantmass=antistate(iriv)%disantmass+precipitation_vol*1.e3*wetdep*1.e-6!m3->L ng->mg
!         antistate(iriv)%chantmass=antistate(iriv)%disantmass+antistate(iriv)%spsantmass(:,4)


!################################################################################################   
 ! if(iriv==7)write(996,*)date_sim(idt),rivfluxes(iriv)%VOL(1),dissol_up(iriv,1)+partic_up(iriv,1),&
! !                       (1.-sedresp(iriv)%ks)*rivstates(iriv)%Q(2)*3600.*24.

      END DO !<node loop, river loop
      
    END DO !<layer loop
    !$OMP END PARALLEL DO

! print *, "OK3"

  END SUBROUTINE route_rch_1ts
!#####################################################################################################
   SUBROUTINE muskingum_cunge(it,            & 
                              riv,           & 
                              q_up,          & 
                              sub_to_riv,    & 
                              rivstate,      & 
                              rivflux,       & 
                              ierr,          &
                              message)
    USE MODELTYPE
    USE MODELVAR,   ONLY             :           missing_value,          &
                                                 sim_length

    INTEGER,INTENT(IN)               ::          it                                            
    TYPE(RIVERTYPE),INTENT(IN)       ::          riv
    REAL(KIND=8),INTENT(IN)          ::          q_up
    TYPE(SUB2RIVTYPE),INTENT(IN)     ::          sub_to_riv
    TYPE(RIVSTATETYPE),INTENT(INOUT) ::          rivstate
    TYPE(RIVFLUXTYPE),INTENT(INOUT)  ::          rivflux
    INTEGER,INTENT(OUT)              ::          ierr
    CHARACTER(LEN=*),INTENT(OUT)     ::          message
    REAL(KIND=8)                     :: alpha       
    REAL(KIND=8)                     :: beta        
    REAL(KIND=8)                     :: theta       
    REAL(KIND=8)                     :: X          
    REAL(KIND=8)                     :: dt          
    REAL(KIND=8)                     :: dx          
    REAL(KIND=8)                     :: Q(0:1,0:1)  
    REAL(KIND=8)                     :: Qbar        
    REAL(KIND=8)                     :: Abar        
    REAL(KIND=8)                     :: Vbar        
    REAL(KIND=8)                     :: Ybar        
    REAL(KIND=8)                     :: B           
    REAL(KIND=8)                     :: ck          
    REAL(KIND=8)                     :: Cn          
    REAL(KIND=8)                     :: dTsub       
    REAL(KIND=8)                     :: C0,C1,C2    
    REAL(KIND=8), allocatable        :: QoutLocal(:)
    REAL(KIND=8), allocatable        :: QinLocal(:) 
    INTEGER                          :: ix          
    INTEGER                          :: ntSub       
    REAL(KIND=8), PARAMETER          :: Y = 0.5     
    REAL(KIND=8)                     :: ks

    ks = 0.2
    Q(0,0) = rivstate%Q(1)    !<previous time step 0, inflow 0
    Q(0,1) = rivstate%Q(2)    !<previous time step 1, outflow 1
    Q(1,1) = missing_value    !<current time step 1, outflow 1
    dt = 3600.*24.    
    IF(riv%nodeB .NE. 0) THEN
      alpha = sqrt(riv%slope)/(riv%rough*riv%width**(2./3.))
      beta  = 5./3.
      dx = riv%length
      theta = dt/dx     
      Q(1,0) = q_up   !<current time step 1, inflow 0
      Qbar = (Q(0,0)+Q(1,0)+Q(0,1))/3.0  
      Abar = (Qbar/alpha)**(1/beta)      
      Vbar = Qbar/Abar                   
      ck   = beta*Vbar                   
      Cn   = ck*theta                
      ntSub = 1
      dTsub = dt
      IF(Cn .GT. 1.0) THEN
        ntSub = ceiling(dt/dx*cK)
        dTsub = dt/ntSub
      END IF
      ALLOCATE(QoutLocal(0:ntSub), QinLocal(0:ntSub))
      QoutLocal(:) = missing_value
      QoutLocal(0) = Q(0,1)        
      QinLocal(0)  = Q(0,0)        
      QinLocal(1:ntSub)  = Q(1,0)  
      DO ix = 1, nTsub
        Qbar = (QinLocal(ix)+QinLocal(ix-1)+QoutLocal(ix-1))/3.0 
        Abar = (Qbar/alpha)**(1/beta)                            
        Ybar = Abar/riv%width                                    
        B = riv%width                                            
        ck = beta*(Qbar/Abar)                                    
        X = 0.5*(1.0 - Qbar/(B*riv%slope*ck*dX))                 
        Cn = ck*dTsub/dx                                         
        C0 = (-X+Cn*(1-Y))/(1-X+Cn*(1-Y))
        C1 = (X+Cn*Y)/(1-X+Cn*(1-Y))
        C2 = (1-X-Cn*Y)/(1-X+Cn*(1-Y))
        QoutLocal(ix) = C0* QinLocal(ix)+ C1* QinLocal(ix-1)+ C2* QoutLocal(ix-1)
        QoutLocal(ix) = MAX(0.0, QoutLocal(ix))   
        IF(isnan(QoutLocal(ix))) THEN
          ierr=10; message=TRIM(message)//'QoutLocal is Nan'; RETURN
        END IF
      END DO
      Q(1,1) = SUM(QoutLocal(1:nTsub))/REAL(nTsub,kind=4)
    ELSE
      Q(1,0) = 0.
      Q(1,1) = 0.
    END IF

    rivflux%VOL(0) = rivflux%VOL(1)
    Q(1,1) = MIN(rivflux%VOL(0)/dt + Q(1,0)*0.999, Q(1,1))
    rivflux%VOL(1) = rivflux%VOL(0) + (Q(1,0)-Q(1,1))*dt 
    ! rivflux%VOL(1) = rivflux%VOL(0)+ Q(1,0)*dt - (ks*Q(0,1)*dt+(1-ks)*Q(1,1)*dt)

    rivflux%flow(it) = Q(1,1) + sub_to_riv%flow(it)
    rivstate%Q(1) = Q(1,0)   !<current time, inflow
    rivstate%Q(2) = Q(1,1)   !<current time, outflow
    !>Average flow velocity, m/s
    IF(riv%nodeB.NE.0 .AND. rivflux%VOL(1).GT.0.) THEN 
      ! rivflux%vel = (q_up+rivflux%flow(it))*0.5/(rivflux%VOL(1)/riv%length)!!
      rivflux%vel = Q(1,1)/(rivflux%VOL(1)/riv%length)!
    ELSE 
      ! rivflux%vel = rivflux%flow(it)/(rivflux%VOL(1)/riv%length)
      rivflux%vel = 0.
    END IF  
    IF(rivflux%VOL(1).GT.riv%maxvol)rivflux%VOL(1) = riv%maxvol
    IF(rivflux%VOL(1).LT.riv%minvol)rivflux%VOL(1) = riv%minvol

  END SUBROUTINE muskingum_cunge
!#####################################################################################################
  SUBROUTINE update_time(p)
    USE MODELTYPE,  ONLY :  pair
    TYPE(pair) :: p
    IF (p%y /= 0) THEN
        p%y = p%y + 1
    END IF
  END SUBROUTINE
!####################################################################################################################
  SUBROUTINE update_node(p, t)
    USE MODELTYPE,  ONLY :  pair
    TYPE(PAIR) :: p
    INTEGER, INTENT(IN) :: T
    IF (p%x /= 0 .AND. p%y == 0) THEN
        p%y = t
    END IF
  END SUBROUTINE
!####################################################################################################################
  SUBROUTINE cal_node_time_map(map,t)
    USE MODELTYPE,  ONLY :  pair
    TYPE(pair),INTENT(INOUT) :: map(:,:)
    INTEGER, INTENT(IN)      :: t
    INTEGER  ::  i,l,m
    DO i = 2, t
      DO l = 1, min(i, size(map,1))
        DO m = 1, size(map,2)
          CALL update_node(map(l,m),1)
        END DO
        IF (l <= (i - 1)) THEN
          DO m = 1,size(map,2)
            CALL update_time(map(l,m))
          END DO
        END IF
      END DO
    END DO
  END SUBROUTINE cal_node_time_map
!####################################################################################################################
!####################################################################################################################
SUBROUTINE rch_pet(cur_time,     &
                           temp,         &
                           p_et)

      USE MODELVAR, ONLY :     temp_t,            &   
                               pi,                &
                               timesteps_per_day

      USE MODELTYPE, ONLY  :   TIMEINFORMATIONTYPE
      USE MODELPAR
      TYPE(TIMEINFORMATIONTYPE),INTENT(IN) :: cur_time
      ! INTEGER, INTENT(IN)          ::    id_landuse
      REAL(KIND=8), INTENT(IN)     ::    temp
      REAL(KIND=8), INTENT(OUT)    ::    p_et
      REAL(KIND=8) coef_pet_am
      REAL(KIND=8) coef_pet_ph
      REAL(KIND=8) coef_pet
      coef_pet = par_general(genid_wpet)  
      coef_pet_am = par_general(genid_coef_pet_am)
      coef_pet_ph = par_general(genid_coef_pet_ph)
      coef_pet = coef_pet * (1 + coef_pet_am * SIN((2.0 * pi * (cur_time%dayno - 1  &
                     + REAL(cur_time%tsofday) / REAL(timesteps_per_day) - coef_pet_ph))/365.))
      p_et = 0.0
      IF(temp .GT. temp_t) THEN 
        p_et = coef_pet * (temp - temp_t)
      ELSE 
        p_et = 0.0
      ENDIF
    END SUBROUTINE rch_pet
!####################################################################################################################
!######################################################################################################
  SUBROUTINE rch_temp(airtemp,         &
                      rivstate         &
                      )

    USE MODELTYPE,    ONLY             :     RIVSTATETYPE
    USE MODELVAR,     ONLY             :     timesteps_per_day,    &
                                             simcfg
    !>IN&OUT
    REAL(KIND=8), INTENT(IN)           ::    airtemp
    TYPE(RIVSTATETYPE), INTENT(INOUT)  ::    rivstate
    !>Local
    REAL(KIND=8)               mtimesteps, mtimesteps2  !<Number of timesteps temperature is averaged over
    REAL(KIND=8), PARAMETER :: rivertemp_days = 20.     !Number of days for river temperature calculation
    REAL(KIND=8), PARAMETER :: laketemp_days  = 5.      !Number of days for lake temperature calculation
    REAL(KIND=8), PARAMETER :: T10day_parameter = 10.
    REAL(KIND=8), PARAMETER :: T20day_parameter = 20.

    mtimesteps = timesteps_per_day*rivertemp_days
    rivstate%temp = rivstate%temp+((airtemp-rivstate%temp)/mtimesteps)
    
    ! IF(simcfg%simN .OR. simcfg%simP) THEN 
    !   mtimesteps  = timesteps_per_day*t10day_parameter
    !   mtimesteps2 = timesteps_per_day*t20day_parameter
    !   rivstate%temp10=rivstate%temp10+(rivstate%temp-rivstate%temp10)/mtimesteps
    !   rivstate%temp20=rivstate%temp20+(rivstate%temp-rivstate%temp20)/mtimesteps2
    ! END IF
  END SUBROUTINE rch_temp
!######################################################################################################
END MODULE SURFACEWATER