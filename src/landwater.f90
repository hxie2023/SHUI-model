MODULE LANDWATER
  USE DATAWRITE, ONLY      :      log_process
  IMPLICIT NONE
CONTAINS 
  SUBROUTINE model_soil(c,               &
                        it,              &
                        isoil,           &
                        iland,           &
                        prec,            &
                        sthick,          &
                        sdepth,          &
                        wp,              &
                        fc,              &
                        ep,              &
                        pv,              &
                        temp,            &
                        rain_to_surface, & 
                        p_et,            &
                        soilmemdeep,     &
                        soilmemlayer,    &
                        adhc,            &
                        b,               &
                        bf,              &
                        macrocf,         &
                        infilt,          &
                        surflow,         &
                        surfsat,         &
                        surfexc,         &
                        sw_inc,          &
                        epotfrac,        &
                        evap,            &
                        ddepth,          &
                        sol_rc,          &
                        sol_runoff,      &
                        perc_to_aqu,     &
                        macroflow,       &
                        cmacroflow,      &
                        sol_temp,        &
                        sol_deeptemp,    &
                        sol_water,       &
                        infilt_rate_old, &
                        infilt_day_old,  &
                        prk)
    USE MODELVAR,  ONLY          :  max_soillayer,  &
                                    num_ant_type,  &
                                    ksat,           &
                                    bd_soil,        &
                                    clay_soil,      &
                                    sand_soil,      &
                                    simcfg,         &
                                    dp,sw_fc, times_sim 
    USE MODELTYPE_ANT, ONLY: ANTPOOlTYPE
    USE MODELVAR_ANT,  ONLY: antusage 
    ! USE ANT_SOURCE                
    INTEGER, INTENT(IN)          :: c
    INTEGER, INTENT(IN)          :: it
    INTEGER, INTENT(IN)          :: isoil
    INTEGER, INTENT(IN)          :: iland
    REAL(KIND=8), INTENT(IN)     :: prec
    REAL(KIND=8), INTENT(IN)     :: sthick(0:max_soillayer)
    REAL(KIND=8), INTENT(IN)     :: sdepth(max_soillayer)
    REAL(KIND=8), INTENT(IN)     :: fc(0:max_soillayer)
    REAL(KIND=8), INTENT(IN)     :: wp(0:max_soillayer)
    REAL(KIND=8), INTENT(IN)     :: ep(0:max_soillayer) 
    REAL(KIND=8), INTENT(IN)     :: pv(0:max_soillayer) 
    REAL(KIND=8), INTENT(IN)     :: temp
    REAL(KIND=8), INTENT(IN)     :: rain_to_surface
    REAL(KIND=8), INTENT(IN)     :: p_et
    REAL(KIND=8), INTENT(IN)     :: soilmemdeep      
    REAL(KIND=8), INTENT(IN)     :: soilmemlayer(max_soillayer)
    REAL(KIND=8), INTENT(IN)     :: adhc
    REAL(KIND=8), INTENT(IN)     :: b
    REAL(KIND=8), INTENT(IN)     :: bf
    REAL(KIND=8), INTENT(IN)     :: macrocf
    REAL(KIND=8), INTENT(OUT)    :: surflow 
    REAL(KIND=8), INTENT(OUT)    :: surfsat  !<R1
    REAL(KIND=8), INTENT(OUT)    :: surfexc  !<R2
    REAL(KIND=8), INTENT(OUT)    :: sw_inc   !<soilwater in layer1 increment
    REAL(KIND=8), INTENT(OUT)    :: macroflow
    REAL(KIND=8), INTENT(IN)     :: cmacroflow(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)  :: infilt
    REAL(KIND=8), INTENT(IN)     :: epotfrac(2)
    REAL(KIND=8), INTENT(OUT)    :: evap
    REAL(KIND=8), INTENT(IN)     :: ddepth
    REAL(KIND=8), INTENT(IN)     :: sol_rc(max_soillayer)
    REAL(KIND=8), INTENT(OUT)    :: sol_runoff(1:max_soillayer)
    REAL(KIND=8), INTENT(OUT)    :: perc_to_aqu
    REAL(KIND=8), INTENT(INOUT)  :: sol_temp(0:max_soillayer) 
    REAL(KIND=8), INTENT(INOUT)  :: sol_deeptemp 
    REAL(KIND=8), INTENT(INOUT)  :: sol_water(0:max_soillayer)
    REAL(KIND=8), INTENT(INOUT)  :: infilt_rate_old
    REAL(KIND=8), INTENT(INOUT)  :: infilt_day_old
    REAL(KIND=8), INTENT(OUT)    :: prk(0:max_soillayer)


    LOGICAL :: write = .TRUE.

    
    REAL(KIND=8) vf,xx,init_ant,zdb1,co,percop,yy,prk0,csurf
    REAL(KIND=8)     :: sol_conc(num_ant_type,0:max_soillayer)

IF(write.AND.c==51720) WRITE(999,*) times_sim(it), "start,",rain_to_surface, sol_water(0), sol_water(1), sol_water(2) 
    CALL soil_temp(temp,soilmemdeep,soilmemlayer,sol_deeptemp,sol_temp)
    
    CALL soil_evap(isoil,iland,temp,wp,fc,p_et,epotfrac,sol_temp,evap,sol_water,sol_conc)
IF(write.AND.c==51720) WRITE(999,*) times_sim(it), "ep,",rain_to_surface, sol_water(0), sol_water(1), sol_water(2) 
    CALL vic_runoff(isoil, rain_to_surface, SUM(pv(0:1)), b, surflow, sol_water,sol_conc)
IF(write.AND.c==51720) WRITE(999,*) times_sim(it), "vic,",rain_to_surface, sol_water(0), sol_water(1), sol_water(2)     
    ! CALL percolation(isoil,sthick,perc_to_aqu,sol_water,sol_conc,prk)
CALL percolation(isoil,sthick,perc_to_aqu,sol_water,prk)
IF(write.AND.c==51720) WRITE(999,*) times_sim(it), "perc,",rain_to_surface, sol_water(0), sol_water(1), sol_water(2)      
 

! IF(it==229) WRITE(6,*) "after suro",rain_to_surface, sol_water(0), sol_water(1), sol_water(2) 
    ! IF(simcfg%rf_model.EQ.0) THEN 
    !   CALL surf_infil_default(isoil,rain_to_surface,pv,infilt,macroflow,surflow,sol_water) 
    ! ELSEIF(simcfg%rf_model.EQ.1) THEN
    !   CALL infil_ga(it,rain_to_surface,fc(1),ksat(1,isoil),bd_soil(1,isoil), &
    !                   clay_soil(1,isoil),sand_soil(1,isoil), adhc, infilt, surflow, &
    !                   infilt_rate_old,infilt_day_old,sol_water(1))
    !   ! CALL sat_exc_runoff(rain_to_surface, infilt, pv(1), b, bf, &
    !   !                     macrocf, macroflow, surflow, surfsat, surfexc, sw_inc, sol_water)
    !   CALL sat_exc_runoff(rain_to_surface, infilt, SUM(pv(:)), b, bf, &
    !                       macrocf, macroflow, surflow, surfsat, surfexc, sw_inc, sol_water)

    ! END IF
! WRITE(999,*) 'before perc',rain_to_surface,sol_water(0),sol_water(1),sol_water(2),perc_to_aqu


    ! CALL percolation(isoil,sthick,perc_to_aqu,sol_water)
 
    CALL soil_runoff(isoil,iland,ep(1:3),ddepth,sol_rc,sol_runoff,sol_water(1:3),sol_conc(:,1:3))
IF(write.AND.c==51720) WRITE(999,*) times_sim(it), "interflow,",rain_to_surface, sol_water(0), sol_water(1), sol_water(2)        
    ! CALL macro_flow(isoil,iland,macroflow,cmacroflow,ep(1:3),pv(1:3),&
                    ! sdepth,sthick(1:3),sol_water(1:3),sol_conc(:,1:3))

  END SUBROUTINE model_soil
!############################################################################################################
  SUBROUTINE vic_runoff(isoil,           &
                        rain_to_surface, &
                        sw_max,          &  
                        b,               &
                        surflow,         &       
                        sol_water,       &
                        sol_conc        &
                        )

    USE MODELVAR,   ONLY :  max_soillayer, soilthick,num_ant_type
    USE MODELTYPE_ANT, ONLY: ANTPOOLTYPE
    INTEGER, INTENT(IN)         :: isoil
    REAL(KIND=8), INTENT(IN)    :: rain_to_surface
    REAL(KIND=8), INTENT(IN)    :: sw_max
    REAL(KIND=8), INTENT(IN)    :: b
    REAL(KIND=8), INTENT(OUT)   :: surflow
    REAL(KIND=8), INTENT(INOUT) :: sol_water(0:max_soillayer)
    REAL(KIND=8), INTENT(INOUT) :: sol_conc(num_ant_type,0:max_soillayer)

    REAL(KIND=8)  ::  sol_water_all, ex, A, infilt_max, i0, basis, weight0, weight1
    REAL(KIND=8)  ::  cq(num_ant_type)
    REAL(KIND=8)  ::  rm_ant(num_ant_type)
    INTEGER iant

    sol_water_all = SUM(sol_water(0:1))
    IF(sol_water_all.GT.sw_max) THEN 
      sol_water_all = sw_max
    END IF

    ex = b/(1.+b)
    A = 1. - (1.-sol_water_all/sw_max)**ex
    infilt_max = (1.+b)*sw_max
    i0 = infilt_max*(1.-(1.-A)**(1./b))

    IF(rain_to_surface .EQ. 0) THEN 
      surflow = 0.
    ELSE IF(infilt_max .EQ. 0.) THEN
      surflow = rain_to_surface
    ELSE IF((i0 + rain_to_surface).GT.infilt_max) THEN 
      surflow = rain_to_surface - sw_max + sol_water_all
    ELSE
      basis = 1.-(i0+rain_to_surface)/infilt_max
      surflow = (rain_to_surface-sw_max+sol_water_all)+sw_max*basis**(1.+b)
    END IF
    
    IF(surflow .LT. 0.) surflow = 0. 
    ! IF(rain_to_surface .GT. 100.) surflow = surflow * 0.1
    weight0 = 0.01/(0.01+soilthick(1,isoil))
    weight1 = 1.-weight0
    cq=0.
    ! CALL add_conc(num_ant_type,sol_water(0),sol_conc(:,0),(rain_to_surface-surflow)*weight0,cq)
    sol_water(0) = sol_water(0) + (rain_to_surface-surflow)*weight0    
    sol_water(1) = sol_water(1) + (rain_to_surface-surflow)*weight1

    ! DO iant=1,num_ant_type

    ! rm_ant(iant)= antpool%conc_mdis(iant,0)*(rain_to_surface-surflow)*weight1*1.e-3*1.e6!mg/m3 * mm*1.e-3 *1.e6 ->mg/km2
    ! IF(rm_ant(iant).GT.antpool%m_dissol((iant),0))rm_ant(iant)=antpool%m_dissol(iant,0)
    !  antpool%m_dissol(iant,0)=antpool%m_dissol(iant,0)-rm_ant(iant)    
    !  antpool%m_dissol(iant,1)=antpool%m_dissol(iant,1)+rm_ant(iant)


    ! if(iant==1) write(996,'(2F20.8)',ADVANCE="no")rm_ant(iant),antpool%m_dissol(iant,0)!mg/km2
    ! if(iant==1) write(996,'(2F20.8)',ADVANCE="no")rm_ant(iant),antpool%m_dissol(iant,1)!mg/km2
    ! if(iant==1)write(996,'(2F20.8)',ADVANCE="no")rm_ant(iant),antpool%m_dissol(iant,0)

    ! END DO

  END SUBROUTINE vic_runoff
!############################################################################################################
  SUBROUTINE soil_temp(temp,           &
                       soilmemdeep,    &
                       soilmemlayer,   &
                       sol_deeptem,    &
                       sol_temp)
    USE MODELVAR, ONLY : timesteps_per_day,   &
                       max_soillayer,       &
                       dp  

    REAL(KIND=8), INTENT(IN)     :: temp                          
    REAL(KIND=8), INTENT(IN)     :: soilmemdeep                   
    REAL(KIND=8), INTENT(IN)     :: soilmemlayer(max_soillayer)   
    REAL(KIND=8), INTENT(INOUT)  :: sol_deeptem                   
    REAL(KIND=8), INTENT(INOUT)  :: sol_temp(0:max_soillayer)       
    REAL(KIND=8), PARAMETER :: weightdeep = 0.001      
    INTEGER k               
    REAL(KIND=8) weightair   
    IF(soilmemdeep .GT. 1.) THEN
      weightair = 1./ (soilmemdeep * timesteps_per_day)
    ELSE
      weightair = 1.
    ENDIF
    CALL weighted_temperature(temp, weightair, 0.0_dp, 0.0_dp, sol_deeptem)
    DO k = 1, max_soillayer
      IF(soilmemlayer(k) .GT. 1. * timesteps_per_day) THEN
        weightair = 1./ (soilmemlayer(k))
      ELSE
        weightair = 1. - weightdeep
      ENDIF
      CALL weighted_temperature(temp, weightair, sol_deeptem, weightdeep, sol_temp(k))
    ENDDO
    sol_temp(0)=temp
   END SUBROUTINE soil_temp
!############################################################################################################
  SUBROUTINE weighted_temperature(temp1, weight1, temp2, weight2, soiltemp)
    REAL(KIND=8), INTENT(IN)     :: temp1    
    REAL(KIND=8), INTENT(IN)     :: weight1  
    REAL(KIND=8), INTENT(IN)     :: temp2    
    REAL(KIND=8), INTENT(IN)     :: weight2  
    REAL(KIND=8), INTENT(INOUT)  :: soiltemp 
    soiltemp = soiltemp * (1. - weight1 - weight2) + &
               temp1    *  weight1  +   &
               temp2    *  weight2
  END SUBROUTINE weighted_temperature
!############################################################################################################
  SUBROUTINE surf_infil_default(isoil,              & 
                                rain_to_surface,    &
                                pv,                 &
                                infilt,             &  
                                macroflow,          &
                                surfaceflow,        &
                                sol_water)
    USE MODELVAR,    ONLY   :     max_soillayer,   &
                                  realzero
    USE MODELPAR                      
    INTEGER, INTENT(IN)           ::       isoil            
    REAL(KIND=8), INTENT(IN)      ::       rain_to_surface
    REAL(KIND=8), INTENT(IN)      ::       pv(0:max_soillayer) 
    REAL(KIND=8), INTENT(OUT)     ::       surfaceflow   
    REAL(KIND=8), INTENT(OUT)     ::       macroflow
    REAL(KIND=8), INTENT(OUT)     ::       infilt   
    REAL(KIND=8), INTENT(OUT)     ::       sol_water(0:max_soillayer)  

    !>Local
    REAL(KIND=8) new_sw_1   
    INTEGER      surolayer    
    REAL(KIND=8) macfrac, surofrac
    REAL(KIND=8) beta, suroratio, S, Smax
    infilt = rain_to_surface
    surfaceflow  = 0.
    macroflow    = 0.   
    IF (rain_to_surface .LT. realzero) RETURN
    macfrac = par_soil(soilid_macfrac, isoil)
    surofrac = 1.0 - macfrac
    beta = par_general(genid_surobeta)
    surolayer = 2     
    S = SUM(sol_water(0:surolayer)) 
    Smax = SUM(pv(0:surolayer))
    suroratio = (S/Smax)**beta
    suroratio = MIN(suroratio, 1.)
    surfaceflow = suroratio * rain_to_surface
    macroflow = macfrac * surfaceflow
    surfaceflow = surfaceflow - macroflow
    infilt = rain_to_surface - macroflow - surfaceflow
    new_sw_1 = sol_water(1) + infilt
    IF(new_sw_1 .GT. 0.) THEN
      sol_water(1) = new_sw_1
    ELSE
      sol_water(1) = 0
    ENDIF
  END SUBROUTINE surf_infil_default
!############################################################################################################
  SUBROUTINE infil_ga(it,              &
                      rain_to_surface, &
                      fc,              &
                      ksat,            &
                      bd,              &
                      clay,            &
                      sand,            &
                      efhc,            &
                      infilt,          &
                      excessflow,      &
                      infilt_rate_old, &
                      infilt_day_old,  &
                      sol_water        &
                      )
    !>Calculate infiltration rate (mm) by Green-Ampt Mein-Larson method
    USE MODELVAR,     ONLY    :   sim_length

    INTEGER, INTENT(IN)       ::  it
    REAL(KIND=8), INTENT(IN)  ::  rain_to_surface
    REAL(KIND=8), INTENT(IN)  ::  fc
    REAL(KIND=8), INTENT(IN)  ::  ksat
    REAL(KIND=8), INTENT(IN)  ::  bd      
    REAL(KIND=8), INTENT(IN)  ::  clay
    REAL(KIND=8), INTENT(IN)  ::  sand
    REAL(KIND=8), INTENT(IN)  ::  efhc
    REAL(KIND=8), INTENT(INOUT)  :: infilt
    REAL(KIND=8), INTENT(OUT)  :: excessflow
    REAL(KIND=8), INTENT(INOUT)  ::  infilt_rate_old
    REAL(KIND=8), INTENT(INOUT)  ::  infilt_day_old
    REAL(KIND=8), INTENT(INOUT)  ::  sol_water

    REAL(KIND=8)  ::  prec_hour(24)           !<rainfall(mm) at the time step, hourly
    REAL(KIND=8)  ::  prec_day(24)            !<accumulated rainfall at each hour
    REAL(KIND=8)  ::  ef_hc                   !<effective hydraulic connectivity
    REAL(KIND=8)  ::  moist_deficit           !<moisture deficit between the saturation and wetting front
    REAL(KIND=8)  ::  infilt_rate(24)         !<infiltration rate at each hour
    REAL(KIND=8)  ::  infilt_day(24)          !<accumulated infiltration at each hour
    REAL(KIND=8)  ::  excess_runoff(24)       !<infiltration excess runoff at each hour
    REAL(KIND=8)  ::  excess_runoff_day(24)   !<accumulated excess runoff at each hour
    REAL(KIND=8)  ::  porosity                !<g/cm^3
    REAL(KIND=8)  ::  wfmp                    !<wetting front matrix potential
    REAL(KIND=8)  ::  psidt                   !<mm
    INTEGER       ::  i
    REAL(KIND=8)  ::  tst, f1

    ! IF(rain_to_surface.EQ.0.) RETURN
    prec_day(:) = 0.
    prec_hour(:) = 0.
    excess_runoff_day(:) = 0.
    infilt_day(:) = 0.
    infilt_rate(:) = 0.
    excess_runoff(:) = 0.
    ! infilt = 0.
    excessflow = 0.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! infilt_day(1) = infilt_day_old
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    prec_hour(:) = rain_to_surface/24.
    porosity = 1.0 - bd/2.65
    ! ef_hc = (56.82*(ksat)**0.286)/(1.+0.051*EXP(0.062*cn))-2.
    ef_hc = efhc * ksat
    IF(ef_hc.LE.0.) ef_hc=0.001  
    !{<moist_deficit is calculated at the beginning of each day!
    moist_deficit = 0.
    ! IF(it.EQ.1) THEN !<Assume rainfall one day before was 0.
    !   IF(sol_water .GE. fc) sol_water = 0.999*fc
    !   moist_deficit = (1.-sol_water/fc)*0.95*porosity
    !   infilt_rate(1) = 2000. 
    ! ELSE 
    !   IF(prec(it-1).GT.0.) THEN 
    !   ! IF(infilt.GT.0.) THEN
    !     moist_deficit = 0.001*porosity*0.95
    !     infilt_rate(1) = infilt_rate_old
    !     ! infilt_rate_old = 0.
    !   ELSE !<No rainfall 1d before
    !     IF(sol_water .GE. fc) sol_water = 0.999*fc
    !     moist_deficit = (1.-sol_water/fc)*0.95*porosity
    !     infilt_rate(1) = 2000.
    !   END IF 
    ! END IF
    IF(infilt_day_old.EQ.0.) THEN 
      IF(sol_water.GE.fc) sol_water = 0.999*fc
      moist_deficit = (1.-sol_water/fc)*0.95*porosity
    ELSE
      moist_deficit = 0.001*porosity*0.95
    END IF

    infilt_rate(1) = infilt_rate_old 
    !}

    !>wetting front matrix potential
    wfmp = 10.*EXP(6.5309-7.32561*porosity+3.809479*porosity**2.+0.001583*  &
           clay**2.+0.000344*sand*clay-0.049837*porosity*sand+0.001608*porosity**2.* &
           sand**2.+0.001602*porosity**2.*clay**2.-0.0000136*sand**2.  * &
           clay-0.003479*clay**2.*porosity-0.000799*sand**2.*porosity)
    psidt = wfmp * moist_deficit
! WRITE(6,*) 'infilt_day_old', infilt_day_old
    !>Calculate at each hour 
    !{
    DO i = 1, 24
      !>Accumulated rainfall at each hour
      IF(i.EQ.1) THEN 
        prec_day(i) = prec_hour(i)
      ELSE
        prec_day(i) = prec_day(i-1) + prec_hour(i)
      END IF
      IF(infilt_rate(i) .GE. prec_hour(i)) THEN !<下渗率大于雨强，则降雨全部入渗，无超渗地表产流
        IF(i.EQ.1) THEN 
          excess_runoff(i) = 0.
          IF(infilt.GT.0) THEN 
            infilt_day(i) = infilt_day_old+prec_hour(i)
          ELSE
            infilt_day(i) = prec_hour(i)
          END IF
          excess_runoff_day(i) = 0
        ELSE !<非第1小时
          infilt_day(i) = infilt_day(i-1) + prec_hour(i)
          IF(excess_runoff_day(i-1) .GT. 0.) THEN 
            excess_runoff_day(i) = excess_runoff_day(i-1)
            excess_runoff(i) = 0.
          ELSE
            excess_runoff_day(i) = 0.
            excess_runoff(i) = 0.
          END IF
        END IF
! WRITE(6,*) 'infilt_day(i)=', infilt_day(i)
     !#####################################################################################################
      ELSE 
     !#####################################################################################################  
      !<下渗率小于雨强，部分降雨变为产流，部分降雨入渗
        tst = ef_hc
        IF(i.EQ.1) THEN 
          IF(infilt.GT.0.) THEN 
            infilt_day(i)=infilt_day_old
          ELSE
            infilt_day(i)=0.
          END IF
        ENDIF      
        !{Start loop
! WRITE(6,*) 'infilt_day(i)=', infilt_day(i)
        !---------------------------------------------------------------------------------------------------
        DO 
          f1 = infilt_day(i) + ef_hc + psidt * LOG((tst + psidt)/(infilt_day(i) + psidt))
! WRITE(6,*) f1, tst
          IF (ABS(f1 - tst) .LE. 0.001) THEN
            infilt_day(i) = f1
            IF(infilt_day(i)-infilt_day_old.GT.prec_day(i)) infilt_day(i) = infilt_day_old+infilt_rate(i)
! WRITE(6,*) 'infilt_day',infilt_day(i),'infilt_day_old',infilt_day_old
            excess_runoff_day(i) = prec_day(i) - (infilt_day(i) - infilt_day_old)
! WRITE(6,*) 'excess_runoff_day', excess_runoff_day(i)
            ! excess_runoff_day(i) = prec_day(i) - infilt_day(i)
            IF (i .EQ. 1) then
              excess_runoff(i) = excess_runoff_day(i)
            ELSE
              excess_runoff(i) = excess_runoff_day(i) - excess_runoff_day(i-1)

            END IF
            IF (excess_runoff(i) .LT. 0.) excess_runoff(i) = 0.
            EXIT
          ELSE
            tst = f1
          END IF

        END DO
        !} End of loop, GET infilt_day(i)
        !---------------------------------------------------------------------------------------------------

      END IF
      !>New infilt_rate for the next time step
      IF(i.LT.24) THEN 
        infilt_rate(i+1) = ef_hc * (psidt/(infilt_day(i) + 1.E-6) + 1.) !<SWAT manual, P107, E 2:1.2.1
        infilt_day(i+1) = infilt_day(i) 
      END IF
       
    END DO 
    !} End of time loop

    infilt_rate_old = infilt_rate(24)
    excessflow = excess_runoff_day(24)
    infilt = prec_day(24) - excessflow
    infilt_day_old = infilt_day(24) - infilt_day_old
    ! infilt_day_old = infilt

! WRITE(6,*)'after loop, infilt = ', infilt

  END SUBROUTINE infil_ga
!#####################################################################################################
  SUBROUTINE sat_exc_runoff(rain_to_surface,      &
                            infilt,               &
                            sw_max,               &
                            b,                    &
                            bf,                   &
                            macrocf,              &
                            macroflow,            &
                            surfaceflow,          &
                            surfsat,              &
                            surfexc,              &
                            sw_inc,               &
                            sol_water)
   
    USE MODELPAR
    USE MODELVAR,    ONLY        :   dp, max_soillayer
    REAL(KIND=8), INTENT(IN)     ::  rain_to_surface
    REAL(KIND=8), INTENT(IN)     ::  infilt
    REAL(KIND=8), INTENT(IN)     ::  sw_max
    REAL(KIND=8), INTENT(IN)     ::  b
    REAL(KIND=8), INTENT(IN)     ::  bf
    REAL(KIND=8), INTENT(IN)     ::  macrocf
    REAL(KIND=8), INTENT(OUT)    ::  macroflow
    REAL(KIND=8), INTENT(OUT)    ::  surfaceflow
    REAL(KIND=8), INTENT(OUT)    ::  surfsat
    REAL(KIND=8), INTENT(OUT)    ::  surfexc
    REAL(KIND=8), INTENT(OUT)    ::  sw_inc
    REAL(KIND=8), INTENT(INOUT)  ::  sol_water(0:max_soillayer)

    REAL(KIND=8)  ::  R1, R2, y, fm 
    REAL(KIND=8)  ::  y_min, y_max, epsilon
    REAL(KIND=8)  ::  sol_water_all
   
    IF(rain_to_surface.EQ.0.) THEN 
      surfsat = 0.
      surfexc = 0.
      surfaceflow = 0.
    END IF

    IF(rain_to_surface.GT.0. .AND. infilt.EQ.0.) THEN
      surfaceflow = rain_to_surface
      RETURN
    END IF

    sol_water_all = SUM(sol_water(:))

    fm = infilt*(1+bf)
    ! IF(sol_water(1) .GT. sw_max) THEN 
    !   sol_water(1) = sw_max
    ! END IF
    IF(sol_water_all .GT. sw_max) THEN 
      sol_water_all = sw_max
    END IF

    epsilon = 1.E-6
    y_min = 0.; y_max = rain_to_surface

    ! CALL bisection(y,y_min,y_max,epsilon,sw_max,b,sol_water(1),rain_to_surface,infilt,fm,bf)
    CALL bisection(y,y_min,y_max,epsilon,sw_max,b,sol_water_all,rain_to_surface,infilt,fm,bf)
    ! CALL calculate_sat_exc_runoff(y, sw_max, b, sol_water(1), rain_to_surface, infilt, fm, bf, R1, R2, sw_inc)
    CALL calculate_sat_exc_runoff(y, sw_max, b, sol_water_all, rain_to_surface, infilt, fm, bf, R1, R2, sw_inc)
    
    sol_water(0) = sol_water(0) + sw_inc
    !<R1: saturation excess surfacef runoff; R2: infiltration excess surface runoff
    surfaceflow = (R1 + R2)*(1-macrocf) 
    macroflow = (R1 + R2)*macrocf
    surfsat = R1*(1-macrocf) 
    surfexc = R2*(1-macrocf) 

  END SUBROUTINE sat_exc_runoff
!#####################################################################################################  
  SUBROUTINE bisection(y, y_min, y_max, epsilon, sw_max, b, sol_water, rain_to_surface, infilt, fm, bf)
    REAL(KIND=8), INTENT(OUT)   :: y
    REAL(KIND=8), INTENT(IN)    ::  epsilon, sw_max, b, sol_water, rain_to_surface, infilt, fm, bf
    REAL(KIND=8), INTENT(INOUT) :: y_min, y_max
    REAL(KIND=8) :: R1, R2, sw_inc, f, midpoint

    DO
      midpoint = (y_min + y_max)/2.0
      call calculate_sat_exc_runoff(midpoint, sw_max, b, sol_water, rain_to_surface, infilt, fm, bf, R1, R2, sw_inc)
      f = R1 + R2 + sw_inc - rain_to_surface  !<equation to solve
      IF (ABS(f) .LE. epsilon) EXIT
      IF (f < 0) THEN
          y_min = midpoint
      ELSE
          y_max = midpoint
      END IF
    END DO 
    y = midpoint
  END SUBROUTINE bisection
!#####################################################################################################
  SUBROUTINE calculate_sat_exc_runoff(y,             &
                                     sw_max,         &
                                     b,              &
                                     sol_water,      &
                                     rain_to_surface,&
                                     infilt,         &
                                     fm,             & 
                                     bf,             &
                                     R1,             &
                                     R2,             &
                                     sw_inc)
    REAL(KIND=8), INTENT(IN)  :: y
    REAL(KIND=8), INTENT(IN)  :: sw_max
    REAL(KIND=8), INTENT(IN)  :: b
    REAL(KIND=8), INTENT(IN)  :: sol_water
    REAL(KIND=8), INTENT(IN)  :: rain_to_surface
    REAL(KIND=8), INTENT(IN)  :: infilt
    REAL(KIND=8), INTENT(IN)  :: fm
    REAL(KIND=8), INTENT(IN)  :: bf
    REAL(KIND=8), INTENT(OUT) :: R1, R2, sw_inc
    !--------------------------------------------------------------------------------------
    !>R1, saturation excess surface runoff
    IF(y.GE.0. .AND. y.LE.sw_max-sol_water) THEN 
      R1 = y-sw_max/(b+1)*((1-sol_water/sw_max)**(b+1)-(1-(sol_water+y)/sw_max)**(b+1)) 
    ELSEIF(y.GT.(sw_max-sol_water) .AND. y.LE.rain_to_surface) THEN
      R1 = y-(sw_max/(b+1)*((1.-sol_water/sw_max)**(b+1)))
      ! R1 = (sw_max-sol_water)-sw_max/(b+1)*((1-sol_water/sw_max)**(b+1)-(1-(sol_water+(sw_max-sol_water))/sw_max)**(b+1))+&
      !      y-(sw_max-sol_water) 
    END IF
    !--------------------------------------------------------------------------------------
    !>soil water increment
    IF(y.GE.0. .AND. y.LE.(sw_max-sol_water)) THEN 
      sw_inc = sw_max/(b+1)*((1-sol_water/sw_max)**(b+1)-(1-(sol_water+y)/sw_max)**(b+1))
    ELSEIF(y.GT.sw_max-sol_water .AND. y.LE.rain_to_surface) THEN
      ! sw_inc = sw_max-sol_water-(sw_max-sol_water)+ &
      !          sw_max/(b+1)*((1-sol_water/sw_max)**(b+1)-(1-(sol_water+sw_max-sol_water)/sw_max)**(b+1))
      sw_inc = sw_max/(1+b)*(1-sol_water/sw_max)**(1+b)
    END IF
    !--------------------------------------------------------------------------------------
    !>R2, infiltration excess surface runoff
    IF((rain_to_surface-R1)/fm .LE. 1.) THEN 
      R2 = rain_to_surface - R1 - infilt*(1-(1-(rain_to_surface-R1)/fm) **(bf+1))
    ELSE
      R2 = rain_to_surface - R1 - infilt
    END IF
  END SUBROUTINE calculate_sat_exc_runoff
!#####################################################################################################
  ! SUBROUTINE percolation(isoil,        &
  !                        sthick,       &
  !                        perc_to_aqu,  &
  !                        sol_water,    &
  !                        sol_conc,prk)

  !   USE MODELTYPE, ONLY    :     SOILSTATETYPE  
  !   USE GENERAL,   ONLY    :     remove_water,              &
  !                                add_water
  !   USE ANT_SOURCE, ONLY   :     remove_conc,    &
  !                                add_conc
  !   USE MODELPAR
  !   USE MODELVAR,    ONLY    :   ksat,                      &
  !                                timesteps_per_day,         &
  !                                max_soillayer,             &
  !                                num_ant_type,             &
  !                                coef_delay_perc, sw_pv, sw_fc
                    
  !   INTEGER, INTENT(IN)          ::  isoil
  !   REAL(KIND=8), INTENT(IN)     ::  sthick(0:max_soillayer)
  !   REAL(KIND=8), INTENT(OUT)    ::  perc_to_aqu
  !   REAL(KIND=8), INTENT(INOUT)  ::  sol_water(0:max_soillayer)
  !   REAL(KIND=8), INTENT(INOUT)  ::  sol_conc(num_ant_type,0:max_soillayer-1)
  !   REAL(KIND=8)  :: perc3,perc2,perc1,perc0
  !   REAL(KIND=8)  :: correction 
  !   REAL(KIND=8)  :: cprec(num_ant_type)
  !   REAL(KIND=8), INTENT(OUT)   :: prk(0:max_soillayer)
  !   INTEGER  ::  status
    
  !   correction = par_soil(soilid_macfrac, isoil)
  !   perc_to_aqu = 0.
  !   prk=0.

  !   IF(sthick(3).GT.0.) THEN 

  !     perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil) 
  !     cprec=sol_conc(:,1)
  !     ! CALL remove_conc(num_ant_type,sol_water(1),sol_conc(:,1),perc1,cprec)
  !     CALL remove_sol_water(perc1,sol_water(1),status)
  !     ! CALL add_conc(num_ant_type,sol_water(2),sol_conc(:,2),perc1,cprec)
  !     sol_water(2) = sol_water(2)+perc1

  !     perc2 = sw_minus_fc(2,isoil,sol_water(2))*coef_delay_perc(2,isoil) 
  !     cprec=sol_conc(:,2)
  !     ! CALL remove_conc(num_ant_type,sol_water(2),sol_conc(:,2),perc2,cprec)
  !     CALL remove_sol_water(perc2,sol_water(2),status)
  !     ! CALL add_conc(num_ant_type,sol_water(3),sol_conc(:,3),perc2,cprec)
  !     sol_water(3) = sol_water(3)+perc2

  !     perc3 = sw_minus_fc(3,isoil,sol_water(3))*coef_delay_perc(3,isoil) 
  !     cprec=sol_conc(:,3)
  !     ! CALL remove_conc(num_ant_type,sol_water(3),sol_conc(:,3),perc3,cprec)
  !     sol_water(3) = sol_water(3)-perc3

  !     perc_to_aqu = perc3
      
  !   ELSEIF(sthick(2).GT.0.) THEN 

  !     perc0 = sw_minus_fc(0,isoil,sol_water(0))*coef_delay_perc(0,isoil)*correction
  !     ! perc0 = sw_minus_fc(0,isoil,sol_water(0))*0.025
  !     cprec=sol_conc(:,0)
  !     ! CALL remove_conc(num_ant_type,sol_water(0),sol_conc(:,0),perc0,cprec)
  !     CALL remove_sol_water(perc0,sol_water(0),status)
  !     ! CALL add_conc(num_ant_type,sol_water(1),sol_conc(:,1),perc0,cprec)
  !     sol_water(1) = sol_water(1)+perc0

  !     ! perc1 = sw_minus_fc(1,isoil,sol_water(1))*0.025
  !     perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil)*correction
  !     cprec=sol_conc(:,1)
  !     ! CALL remove_conc(num_ant_type,sol_water(1),sol_conc(:,1),perc1,cprec)
  !     CALL remove_sol_water(perc1,sol_water(1),status)
  !     ! CALL add_conc(num_ant_type,sol_water(2),sol_conc(:,2),perc1,cprec)
  !     sol_water(2) = sol_water(2)+perc1

  !     perc2 = sw_minus_fc(2,isoil,sol_water(2))*coef_delay_perc(2,isoil)*correction
  !     cprec=sol_conc(:,2)
  !     ! CALL remove_conc(num_ant_type,sol_water(2),sol_conc(:,2),perc2,cprec)
  !     CALL remove_sol_water(perc2,sol_water(2),status)
  !     perc_to_aqu = perc2
      
  !     prk(0)=perc0
  !     prk(1)=perc1
  !     prk(2)=perc2

  !   ELSEIF(sthick(1).GT.0.) THEN 

  !     perc0 = sw_minus_fc(0,isoil,sol_water(0))*coef_delay_perc(0,isoil)
  !     CALL remove_sol_water(perc0,sol_water(0),status)
  !     sol_water(1) = sol_water(1)+perc0

  !     perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil) 
  !     cprec=sol_conc(:,1)
  !     ! CALL remove_conc(num_ant_type,sol_water(1),sol_conc(:,1),perc1,cprec)
  !     CALL remove_sol_water(perc1,sol_water(1),status)

  !     perc_to_aqu = perc1

  !     prk(0)=perc0
  !     prk(1)=perc1
    
  !   END IF
  !   ! perc_to_aqu = perc_to_aqu * 2.

  ! END SUBROUTINE percolation
!#####################################################################################################
!#####################################################################################################

SUBROUTINE percolation(  isoil,        &
                         sthick,       &
                         perc_to_aqu,  &
                         sol_water,prk)

    USE MODELTYPE, ONLY    :     SOILSTATETYPE  
    USE GENERAL,   ONLY    :     remove_water,              &
                                 add_water
    USE MODELPAR
    USE MODELVAR,    ONLY    :   ksat,                      &
                                 timesteps_per_day,         &
                                 max_soillayer,             &
                                 num_ant_type,             &
                                 coef_delay_perc, sw_pv, sw_fc
    
    ! INTEGER, INTENT(IN)          ::  isub
    INTEGER, INTENT(IN)          ::  isoil
    REAL(KIND=8), INTENT(IN)     ::  sthick(0:max_soillayer)
    REAL(KIND=8), INTENT(OUT)    ::  perc_to_aqu
    REAL(KIND=8), INTENT(INOUT)  ::  sol_water(0:max_soillayer)
    REAL(KIND=8), INTENT(OUT)   :: prk(0:max_soillayer)
    REAL(KIND=8)  :: perc3,perc2,perc1,perc0
    REAL(KIND=8)  :: correction 
    INTEGER  ::  status
    INTEGER  ::  i
    ! INTEGER  ::  tong_sub(9)
    ! tong_sub = (/1,2,3,4,5,6,7,8,9/)

    correction = par_soil(soilid_macfrac, isoil)
    perc_to_aqu = 0.
    prk=0.

    IF(sthick(3).GT.0.) THEN 

      perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil) 
      CALL remove_sol_water(perc1,sol_water(1),status)
      sol_water(2) = sol_water(2)+perc1

      perc2 = sw_minus_fc(2,isoil,sol_water(2))*coef_delay_perc(2,isoil) 
      CALL remove_sol_water(perc2,sol_water(2),status)
      sol_water(3) = sol_water(3)+perc2

      perc3 = sw_minus_fc(3,isoil,sol_water(3))*coef_delay_perc(3,isoil) 
      sol_water(3) = sol_water(3)-perc3

      perc_to_aqu = perc3

    ELSEIF(sthick(2).GT.0.) THEN 

      perc0 = sw_minus_fc(0,isoil,sol_water(0))*coef_delay_perc(0,isoil)*correction
      ! perc0 = sw_minus_fc(0,isoil,sol_water(0))*0.025
      CALL remove_sol_water(perc0,sol_water(0),status)
      sol_water(1) = sol_water(1)+perc0

      ! perc1 = sw_minus_fc(1,isoil,sol_water(1))*0.025
      perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil)*correction
      CALL remove_sol_water(perc1,sol_water(1),status)
      sol_water(2) = sol_water(2)+perc1

      perc2 = sw_minus_fc(2,isoil,sol_water(2))*coef_delay_perc(2,isoil)*correction
      CALL remove_sol_water(perc2,sol_water(2),status)

      perc_to_aqu = perc2

      prk(0)=perc0
      prk(1)=perc1
      prk(2)=perc2

    ELSEIF(sthick(1).GT.0.) THEN 

      perc0 = sw_minus_fc(0,isoil,sol_water(0))*coef_delay_perc(0,isoil)*correction
      CALL remove_sol_water(perc0,sol_water(0),status)
      sol_water(1) = sol_water(1)+perc0

      perc1 = sw_minus_fc(1,isoil,sol_water(1))*coef_delay_perc(1,isoil)*correction 
      CALL remove_sol_water(perc1,sol_water(1),status)

      perc_to_aqu = perc1

      prk(0)=perc0
      prk(1)=perc1

    END IF


  END SUBROUTINE percolation

!#####################################################################################################

!#####################################################################################################

  SUBROUTINE soil_evap(isoil,      &
                       iland,      &
                       temp,       &
                       wp,         &
                       fc,         &
                       p_et,       &
                       epotfrac,   &
                       sol_temp,   &
                       evap,       &
                       sol_water,  &
                       sol_conc    &
                       )
    USE MODELVAR,   ONLY  :     max_soillayer,            &
                                num_ant_type,            &
                                soilthick,                &
                                realzero
    USE GENERAL,  ONLY    :     remove_water
    USE MODELPAR
    USE MODELTYPE, ONLY          :  SOILSTATETYPE  
    INTEGER, INTENT(IN)           ::      isoil
    INTEGER, INTENT(IN)           ::      iland
    REAL(KIND=8), INTENT(IN)      ::      temp
    REAL(KIND=8), INTENT(IN)      ::      wp(0:max_soillayer)
    REAL(KIND=8), INTENT(IN)      ::      fc(0:max_soillayer)
    REAL(KIND=8), INTENT(IN)      ::      p_et
    REAL(KIND=8), INTENT(IN)      ::      epotfrac(2)
    REAL(KIND=8), INTENT(IN)      ::      sol_temp(0:max_soillayer)    
    REAL(KIND=8), INTENT(OUT)     ::      evap
    REAL(KIND=8), INTENT(INOUT)   ::      sol_water(0:max_soillayer)
    REAL(KIND=8), INTENT(INOUT)   ::      sol_conc(num_ant_type,0:max_soillayer)
    REAL(KIND=8) :: soiltemp_factor(0:max_soillayer), soiltemp
    REAL(KIND=8) :: efwater(0:2)   
    REAL(KIND=8) :: evapsl(0:2)    
    REAL(KIND=8) :: cevapsl(num_ant_type,0:2), ctemp(num_ant_type)
    INTEGER      :: ilayer, status

    evap = 0.
    IF(p_et .LE. 0.) RETURN

    soiltemp_factor(:) = 1.
    IF(par_landuse(luid_soiltempreA, iland).GT.0.) THEN 
      DO ilayer = 0, 2
        IF(soilthick(ilayer, isoil) .GT. 0.) THEN 
          soiltemp = sol_temp(ilayer)
          IF(soiltemp .GT. par_landuse(luid_soiltempttrig, iland)) THEN 
          soiltemp_factor(ilayer)= 1.-EXP(-par_landuse(luid_soiltempreA,iland)* &
                                  (soiltemp-par_landuse(luid_soiltempttrig,iland))**par_landuse(luid_soiltempreB,iland))  
          END IF
        END IF
      END DO
    END IF
    
    IF(temp.GT.0.1) THEN !>Soil EP exists
      evapsl = 0.
      cevapsl = 0.

      !>First, top 10 mm layer
    
      DO ilayer = 1, 2

        !>evapsl is CRITICAL, potential soil ep
        IF(soilthick(ilayer, isoil).GT.0.) THEN 
          efwater(ilayer)=sol_water(ilayer)-wp(ilayer)
          IF(efwater(ilayer).LE.0.) CYCLE
          evapsl(ilayer)=p_et*epotfrac(ilayer)*soiltemp_factor(ilayer) !<epotfrac <- epotdist(layer1 and layer2)

          ! IF(efwater(ilayer).LT.(fc(ilayer)-wp(ilayer))) THEN 
          !   evapsl(ilayer)=evapsl(ilayer)*efwater(ilayer)/(fc(ilayer)-wp(ilayer))
          ! END IF
          !>SWAT manual, P139
          IF(sol_water(ilayer).LT.fc(ilayer)) THEN 
            evapsl(ilayer)=evapsl(ilayer)*EXP(2.5*(sol_water(ilayer)-fc(ilayer))/(fc(ilayer)-wp(ilayer)))
          END IF

          IF(evapsl(ilayer).GE.sol_water(ilayer)-wp(ilayer)) THEN 
            evapsl(ilayer) = sol_water(ilayer)-wp(ilayer)
          END IF             

          IF(evapsl(ilayer).GT. 0.)THEN
            ctemp = 0. 
            IF(evapsl(ilayer)+realzero.LT.sol_water(ilayer)) THEN
              CALL remove_water(sol_water(ilayer),num_ant_type, &
                                sol_conc(:,ilayer),evapsl(ilayer),ctemp,status)       
              IF(num_ant_type>0) cevapsl(:,ilayer) = ctemp
            ELSE
              sol_water(ilayer) = 0.
              IF(num_ant_type>0) sol_conc(:,ilayer) = 0. 
            ENDIF
          END IF

        END IF
      END DO !<Loop of layer1 and 2

      !>Top 10 mm surface layer
      IF(soilthick(0,isoil).GT.0) THEN 

        IF(sol_water(0).LE.wp(0)) THEN 
          evapsl(0) = 0.
        ELSE
          evapsl(0)=p_et*epotfrac(1)*soiltemp_factor(0)

          IF(sol_water(0).LT.fc(0)) THEN 
            ! evapsl(0)=evapsl(0)*(sol_water(0)-wp(0))/(fc(0)-wp(0))
            evapsl(0)=evapsl(0)*EXP(2.5*(sol_water(0)-fc(0))/(fc(0)-wp(0))) !<SWAT manual, P139
          END IF
  
          IF(evapsl(0).GE.sol_water(0)-wp(0)) THEN 
            evapsl(0) = sol_water(0)-wp(0)
          END IF 
        END IF

        IF(evapsl(0).GT.0.) THEN 
          ctemp = 0. 
          IF(evapsl(0).LT.sol_water(0)) THEN
            CALL remove_water(sol_water(0),num_ant_type, &
                              sol_conc(:,0),evapsl(0),ctemp,status)       
            IF(num_ant_type>0) cevapsl(:,0) = ctemp
          ELSE
            sol_water(0) = 0.
            IF(num_ant_type>0) sol_conc(:,0) = 0. 
          ENDIF
        END IF

      END IF

      evap = SUM(evapsl(:))

    END IF
  END SUBROUTINE soil_evap
!############################################################################################################
  SUBROUTINE soil_runoff(isoil,      &
                         iland,      &
                         ep,         & 
                         ddepth,     &
                         sol_rc,     &
                         sol_runoff, &
                         sol_water,  &
                         sol_conc    &
                         )

    USE MODELVAR, ONLY         :           max_soillayer,    &
                                           num_ant_type,    &
                                           soilthick,        &                                         
                                           soilthick,        &
                                           soildepth
    INTEGER, INTENT(IN)        ::          isoil
    INTEGER, INTENT(IN)        ::          iland
    REAL(KIND=8), INTENT(IN)   ::          ep(max_soillayer)
    REAL(KIND=8), INTENT(IN)   ::          ddepth                     !<Streamdepth, drainage depth
    REAL(KIND=8), INTENT(IN)   ::          sol_rc(max_soillayer)
    REAL(KIND=8), INTENT(OUT)  ::          sol_runoff(max_soillayer)
    REAL(KIND=8), INTENT(INOUT)   ::       sol_water(max_soillayer)
    REAL(KIND=8), INTENT(INOUT)   ::       sol_conc(num_ant_type,max_soillayer)
    INTEGER k, k2
    INTEGER status    
    REAL(KIND=8) delta(max_soillayer)   
    REAL(KIND=8) deltah                 
    REAL(KIND=8) avail(max_soillayer)    
    REAL(KIND=8) epwater(max_soillayer)  
    LOGICAL includedd
    REAL(KIND=8), PARAMETER            ::          mindiff = 0.00002
    sol_runoff = 0.
    IF(ddepth .LE. 0.) RETURN
    delta = 0.
    DO k = 1, max_soillayer
      epwater(k) = sw_minus_fc(k, isoil, sol_water(k))
      avail(k) = epwater(k) 
      IF(ep(k) .GT. 0.) delta(k) = epwater(k) / ep(k) * soilthick(k, isoil)    !< unit: m
    END DO
    DO k = 1, max_soillayer
      IF(soilthick(k, isoil) .GT. 0.) THEN 
        IF(ddepth .LT. soildepth(k, isoil) - soilthick(k, isoil)) CYCLE
        deltah = delta(k)
        includedd = .FALSE.
        IF(ddepth .LE. soildepth(k, isoil)) includedd = .TRUE.
        IF(k .LT. max_soillayer) THEN 
          IF(soilthick(k + 1, isoil) .EQ. 0.) includedd = .TRUE.
        ELSE
          includedd = .TRUE.
        END IF
        IF(includedd) deltah = deltah + (ddepth - soildepth(k, isoil))
        DO k2 = k-1, 1, -1
          IF(epwater(k2 + 1) - ep(k2 + 1) .GE. 0. - mindiff * ep(k2 + 1)) THEN
            deltah = deltah + delta(k2)
          ELSE
            EXIT 
          END IF
        END DO
        CALL pressured_runoff(k,deltah,avail(k),ep(k),soilthick(k,isoil), &
                              sol_rc,sol_runoff(k),sol_water,sol_conc,status)                                            
      END IF
    END DO
  END SUBROUTINE soil_runoff
!############################################################################################################
  SUBROUTINE pressured_runoff(sl,           & 
                              plevel,       & 
                              ava_water,    & 
                              ep,           &
                              sthick,       & 
                              sol_rc,       & 
                              runoff,       & 
                              sol_water,    &
                              sol_conc,     &                                            
                              status        &
                              )
    USE MODELVAR, ONLY       :       max_soillayer,      &
                                     num_ant_type
    USE GENERAL, ONLY        :       remove_water
    INTEGER, INTENT(IN)           ::       sl    
    REAL(KIND=8), INTENT(IN)      ::       plevel
    REAL(KIND=8), INTENT(IN)      ::       ava_water                
    REAL(KIND=8), INTENT(IN)      ::       ep                       
    REAL(KIND=8), INTENT(IN)      ::       sthick                   
    REAL(KIND=8), INTENT(IN)      ::       sol_rc(max_soillayer)
    REAL(KIND=8), INTENT(OUT)     ::       runoff                      
    INTEGER, INTENT(OUT)          ::       status
    REAL(KIND=8), INTENT(INOUT)   ::       sol_water(max_soillayer)
    REAL(KIND=8), INTENT(INOUT)   ::       sol_conc(num_ant_type,max_soillayer)
    REAL(KIND=8)  ::  crunoff(num_ant_type)      
    runoff = 0.
    crunoff = 0.
    
    IF(ava_water .LE. 0.) RETURN  
    IF(plevel .LE. 0.) RETURN     
    IF(sthick .LE. 0.) RETURN     
    runoff = sol_rc(sl) * plevel / sthick * ep
    IF(runoff .GE. ava_water) runoff = ava_water
    IF(num_ant_type .GT. 0) crunoff = sol_conc(:,sl)
    CALL remove_water(sol_water(sl),num_ant_type,sol_conc(:,sl), &
                      runoff, crunoff, status) 
  END SUBROUTINE pressured_runoff
!############################################################################################################
  SUBROUTINE macro_flow(isoil,            &
                        iland,            &
                        macroflow,        &
                        cmacroflow,       & 
                        ep,               &
                        pv,               &
                        sdepth,           &
                        sthick,           &
                        sol_water,        &
                        sol_conc          &
                        )
    USE MODELVAR, ONLY  :     max_soillayer,  &                                                   
                              num_ant_type,sw_fc
    USE GENERAL,ONLY    :     add_water
    INTEGER, INTENT(IN)          ::    isoil
    INTEGER, INTENT(IN)          ::    iland
    REAL(KIND=8), INTENT(IN)     ::    macroflow                  
    REAL(KIND=8), INTENT(IN)     ::    cmacroflow(num_ant_type)  
    REAL(KIND=8), INTENT(IN)     ::    ep(max_soillayer)
    REAL(KIND=8), INTENT(IN)     ::    pv(max_soillayer)
    REAL(KIND=8), INTENT(IN)     ::    sdepth(max_soillayer)
    REAL(KIND=8), INTENT(IN)     ::    sthick(max_soillayer)
    REAL(KIND=8), INTENT(INOUT)  ::    sol_water(max_soillayer)
    REAL(KIND=8), INTENT(INOUT)   ::   sol_conc(num_ant_type,max_soillayer)
    REAL(KIND=8) gwat 
    REAL(KIND=8) newsoil 
    REAL(KIND=8) fill, fill2
    REAL(KIND=8) macroflow_layer(max_soillayer)

    macroflow_layer = 0.
    IF(macroflow .LE. 0.) RETURN
    CALL soilwater_table(isoil, sol_water(:), ep, sdepth(:), sthick(:), gwat)

! WRITE(999,*) gwat,sol_water(2),sw_fc(2,isoil)

    IF(-gwat .GT. sdepth(2)) THEN
      newsoil = sol_water(3) + macroflow
      IF(newsoil .GT. pv(3)) THEN 
        fill = newsoil - pv(3)
        IF(num_ant_type>0) sol_conc(:,3) = (sol_water(3) * sol_conc(:,3) + &
                                                     (macroflow - fill) * cmacroflow)/pv(3)
        sol_water(3) = pv(3)
        macroflow_layer(3) = macroflow - fill
      ELSE
        fill = 0.
        IF(num_ant_type>0) sol_conc(:,3) = (sol_water(3) * sol_conc(:,3) + &
                                                     macroflow * cmacroflow)/newsoil
        sol_water(3) = newsoil
        macroflow_layer(3) = macroflow
      ENDIF
      IF(fill .GT. 0.) THEN 
        newsoil = sol_water(2) + fill
        IF(newsoil > pv(2)) THEN
          fill2 = newsoil - pv(2)
          IF(num_ant_type>0) sol_conc(:,2) = (sol_water(2)*sol_conc(:,2) + &
                                                        (fill-fill2)*cmacroflow)/pv(2)
          sol_water(2) = pv(2)
          macroflow_layer(2) = fill - fill2
        ELSE 
          fill2 = 0.
          IF(num_ant_type>0) sol_conc(:,2) =  &
                             ((newsoil-fill) * sol_conc(:,2)+ fill*cmacroflow)/newsoil   
          sol_water(2) = newsoil
          macroflow_layer(2) = fill 
        ENDIF 
        IF(fill2 .GT. 0.) THEN 
        newsoil = sol_water(1) + fill2
          IF(num_ant_type>0) sol_conc(:,1)= (sol_water(1)* &                                               
                              sol_conc(:,1) + fill2 * cmacroflow)/newsoil
          sol_water(1) = newsoil
          macroflow_layer(1) = fill2
        ENDIF
      ENDIF
    ELSEIF(-gwat .GT. sdepth(1)) THEN
      newsoil = sol_water(2) + macroflow
      IF(newsoil > pv(2)) THEN
        fill = newsoil - pv(2)
        IF(num_ant_type>0) sol_conc(:,2) = (sol_water(2)*  &
                            sol_conc(:,2) + &
                            (macroflow - fill)*cmacroflow)/pv(2)
        sol_water(2) = pv(2)
        macroflow_layer(2) = macroflow - fill
      ELSE
        fill = 0.
        IF(num_ant_type>0) sol_conc(:,2) = (sol_conc(:,2)*  &                          
                                                   sol_conc(:,2) + &
                                                     macroflow*cmacroflow)/newsoil
        sol_water(2) = newsoil
        macroflow_layer(2) = macroflow
      ENDIF
      IF(fill .GT. 0.) THEN
        CALL add_water(num_ant_type,sol_water(1),sol_conc(:,1),fill,cmacroflow)
        macroflow_layer(1) = fill
      ENDIF
    ELSE
      CALL add_water(num_ant_type,sol_water(1),sol_conc(:,1),fill,cmacroflow)
      macroflow_layer(1) = macroflow
    ENDIF

! WRITE(999,*) macroflow,macroflow_layer(2), gwat,sol_water(2)

  END SUBROUTINE macro_flow
  !######################################################################################################
  SUBROUTINE soilwater_table(is,             &
                             sw,             &
                             epvol,          &
                             soildep,        &
                             soilthickness,  &
                             sw_wat)
    USE MODELVAR,  ONLY   :    max_soillayer
    !>IN&OUT
    INTEGER, INTENT(IN)        ::    is
    REAL(KIND=8), INTENT(IN)   ::    sw(max_soillayer)          
    REAL(KIND=8), INTENT(IN)   ::    epvol(max_soillayer)
    REAL(KIND=8), INTENT(IN)   ::    soildep(max_soillayer)
    REAL(KIND=8), INTENT(IN)   ::    soilthickness(max_soillayer)    
    REAL(KIND=8), INTENT(OUT)  ::    sw_wat     !>soil water table 
    REAL(KIND=8), PARAMETER    ::    mindiff = 0.000002
    INTEGER k
    REAL(KIND=8) gwatk 

    sw_wat = 0. 
    DO k = max_soillayer, 1, -1
      IF(soilthickness(k) .GT. 0.)THEN
        IF(sw_above_fc(k, is, sw(k)))THEN
          gwatk = sw_minus_pv(k, is, sw(k))/epvol(k) * soilthickness(k)   
          IF(k .EQ. 1)THEN
            IF(gwatk .GT. 0.) gwatk = sw_minus_pv(k, is, sw(k)) * 0.001 
          ELSE
            gwatk = gwatk - soildep(k - 1)
          ENDIF
          IF(sw_minus_pv(k, is, sw(k)) .GE. 0. - mindiff*epvol(k))THEN
            CYCLE
          ELSE
            EXIT
          ENDIF
        ELSE
          gwatk = -soildep(k)
          EXIT
        ENDIF
      ENDIF
    ENDDO
    sw_wat = gwatk

  END SUBROUTINE soilwater_table
!######################################################################################################  
  REAL(KIND=8) FUNCTION sw_minus_fc(sl, isoil, water)  
    
    USE MODELVAR, ONLY  :  sw_fc
    
    INTEGER, INTENT(IN) :: sl    
    INTEGER, INTENT(IN) :: isoil     
    REAL(KIND=8), INTENT(IN)    :: water    
    REAL(KIND=8) fcn
    fcn = 0.
    IF(sw_above_fc(sl, isoil, water)) fcn = water - sw_fc(sl, isoil)
    sw_minus_fc = fcn
  END FUNCTION sw_minus_fc
!############################################################################################################
  LOGICAL FUNCTION sw_above_fc(sl, isoil, water)  
    USE MODELVAR, ONLY  :  sw_fc
    INTEGER, INTENT(IN) :: sl        
    INTEGER, INTENT(IN) :: isoil  
    REAL(KIND=8), INTENT(IN)    :: water 
    LOGICAL fcn
    fcn = .FALSE.
    IF(water - sw_fc(sl, isoil) .GT. 0.) fcn = .TRUE.
    sw_above_fc = fcn
  END FUNCTION sw_above_fc
!############################################################################################################
  REAL(KIND=8) FUNCTION sw_minus_wp(sl, isoil, water)  
    USE MODELVAR, ONLY    :  sw_wp
    INTEGER, INTENT(IN) :: sl    
    INTEGER, INTENT(IN) :: isoil    
    REAL(KIND=8), INTENT(IN)    :: water    
    REAL(KIND=8) fcn
    fcn = 0.
    IF(sw_above_wp(sl, isoil, water)) &
      fcn = water - sw_wp(sl, isoil) 
    sw_minus_wp = fcn
  END FUNCTION sw_minus_wp
!############################################################################################################ 
  LOGICAL FUNCTION sw_above_wp(sl, isoil, water)  
    USE MODELVAR, ONLY    :  sw_wp
    INTEGER, INTENT(IN) :: sl        
    INTEGER, INTENT(IN) :: isoil    
    REAL(KIND=8), INTENT(IN)    :: water 
    LOGICAL fcn
    fcn = .FALSE.
    IF(water - sw_wp(sl, isoil) .GT. 0.) fcn = .TRUE.
    sw_above_wp = fcn
  END FUNCTION sw_above_wp
!############################################################################################################
  REAL(KIND=8) FUNCTION sw_minus_pv(sl, isoil, water)  
    USE MODELVAR, ONLY    :  sw_pv,   &
                             soilthick
    INTEGER, INTENT(IN) :: sl        
    INTEGER, INTENT(IN) :: isoil      
    REAL(KIND=8), INTENT(IN) :: water         
    REAL(KIND=8) fcn
    fcn = 0.
    IF(soilthick(sl, isoil)>0.) fcn = water - sw_pv(sl, isoil)
    sw_minus_pv = fcn
  END FUNCTION sw_minus_pv
!############################################################################################################ 
  SUBROUTINE remove_sol_water(q,sw,err)  
    REAL(KIND=8), INTENT(INOUT) :: q          
    REAL(KIND=8), INTENT(INOUT) :: sw       
    INTEGER, INTENT(OUT) :: err
    err = 0
    IF(sw-q.GT.0.) THEN 
      sw = sw-q
    ELSE
      err = 1
      sw = 0.
      q = sw
      WRITE(6,*) 'Error: cannot remove enough soil water.'
    END IF 
  END SUBROUTINE remove_sol_water
 !############################################################################################################ 
END MODULE LANDWATER