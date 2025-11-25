MODULE GROUNDWATER
  IMPLICIT NONE
CONTAINS
  SUBROUTINE model_aquifer(isub,                &
                           sw_to_gw,            &
                           re_to_gw,            &
                           re_to_gw_conc,       &
                           base_flow,           &
                           base_flow_conc,      &
                           sol_water,           &
                           sol_conc,            &
                           aq_water,            &
                           aq_conc              &
                           )
    USE MODELTYPE
    USE MODELVAR,        ONLY      :     num_ant_type, &
                                         max_soillayer, &
                                         simcfg,        &
                                         current_time,  &
                                         num_soiltype
    USE GENERAL,         ONLY      :     add_water,     &
                                         remove_water
    USE GW_ANT
    USE MODELPAR_ANT

    INTEGER, INTENT(IN)          ::     isub
    REAL(KIND=8), INTENT(IN)     ::     sw_to_gw 
    REAL(KIND=8), INTENT(INOUT)  ::     re_to_gw                    
    REAL(KIND=8), INTENT(INOUT)  ::     re_to_gw_conc(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)  ::     base_flow
    REAL(KIND=8), INTENT(INOUT)  ::     base_flow_conc(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)  ::     sol_water(0:max_soillayer)
    REAL(KIND=8), INTENT(INOUT)  ::     sol_conc(num_ant_type, 0:max_soillayer)      
    REAL(KIND=8), INTENT(INOUT)  ::     aq_water
    REAL(KIND=8), INTENT(INOUT)  ::     aq_conc(num_ant_type) 

    ! REAL(KIND=8), INTENT(IN)     ::     sw_to_gw_ant(num_ant_type)
    ! REAL(KIND=8), INTENT(INOUT)  ::     gwantpool(num_ant_type)     
    ! REAL(KIND=8), INTENT(INOUT)  ::     recharge2gw_ant(num_ant_type) !!mg/km2
    ! REAL(KIND=8), INTENT(INOUT)  ::     gwload(num_ant_type) !kg/m2

    CALL soilwater_to_aquifer(sw_to_gw,re_to_gw,re_to_gw_conc,sol_water,sol_conc)
    CALL add_water(num_ant_type,aq_water,aq_conc,re_to_gw,re_to_gw_conc)
    CALL baseflow_to_river(isub,re_to_gw,re_to_gw_conc,base_flow,base_flow_conc,aq_water,aq_conc)


    ! CALL ant_rchrg(sw_to_gw_ant,recharge2gw_ant,gwantpool)  
    ! CALL ant_baseflow(base_flow,aq_water,antpar_general(genid_gw_mix),gwantpool,gwload)

  END SUBROUTINE model_aquifer
!############################################################################################################
  SUBROUTINE soilwater_to_aquifer(sw_to_gw,         &
                                  re_to_gw,         &
                                  re_to_gw_conc,    &
                                  sol_water,        &
                                  sol_conc          &
                                  )
    
    USE MODELTYPE,   ONLY       :       SOILSTATETYPE
    USE MODELPAR
    USE MODELVAR,    ONLY       :       num_ant_type,       &
                                        max_soillayer,       &
                                        coef_delay_perc 

    REAL(KIND=8),INTENT(IN)     ::      sw_to_gw
    REAL(KIND=8),INTENT(INOUT)  ::      re_to_gw
    REAL(KIND=8),INTENT(INOUT)  ::      re_to_gw_conc(num_ant_type)
    REAL(KIND=8),INTENT(INOUT)  ::      sol_water(0:max_soillayer)
    REAL(KIND=8),INTENT(INOUT)  ::      sol_conc(num_ant_type,0:max_soillayer)
    REAL(KIND=8) sw_to_gw_conc(num_ant_type)
    REAL(KIND=8) gw_delay
    ! REAL(KIND=8) frac_gw(0:3)  
    REAL(KIND=8) help, help_c(num_ant_type)

    ! frac_gw(:) = coef_delay_perc(:,isoil)
    ! IF(sthick(3) .GT. 0.) THEN
    !   CALL remove_soilwater_to_groundwater(num_ant_type, sol_water(3), &
    !        sol_conc(:,3),frac_gw(3),fc(3),sw_to_gw,sw_to_gw_conc(:))
    ! ELSEIF(sthick(2) .GT. 0.) THEN
    !   CALL remove_soilwater_to_groundwater(num_ant_type, sol_water(2), &
    !        sol_conc(:,2),frac_gw(2),fc(2),sw_to_gw,sw_to_gw_conc(:))
    ! ELSEIF(sthick(1) .GT. 0.) THEN
    !   CALL remove_soilwater_to_groundwater(num_ant_type, sol_water(1), &
    !        sol_conc(:,1),frac_gw(1),fc(1),sw_to_gw,sw_to_gw_conc(:))
    ! ENDIF
    help   = re_to_gw        
    help_c = re_to_gw_conc   
    gw_delay = par_general(genid_gw_delay)
    re_to_gw = (1.0 - EXP(-1./gw_delay))*sw_to_gw + EXP(-1./gw_delay)*help

    IF(num_ant_type.GT.0) THEN
      IF(re_to_gw .GT. 0.) re_to_gw_conc(:) = ((1.0 - EXP(-1./gw_delay))*re_to_gw* &
                           re_to_gw_conc(:)+EXP(-1./gw_delay)*help*help_c)/re_to_gw  
    ENDIF

  END SUBROUTINE soilwater_to_aquifer
!############################################################################################################
  SUBROUTINE remove_soilwater_to_groundwater(n,         &
                                             sw,        &
                                             csw,       &
                                             frac_gw,   &
                                             fc,        &
                                             flow,      &
                                             cflow)

    USE GENERAL,        ONLY     :   remove_water
    INTEGER, INTENT(IN) :: n                    
    REAL(KIND=8), INTENT(IN)    :: frac_gw                
    REAL(KIND=8), INTENT(IN)    :: fc           
    REAL(KIND=8), INTENT(INOUT) :: sw           
    REAL(KIND=8), INTENT(INOUT) :: csw(n)       
    REAL(KIND=8), INTENT(OUT)   :: flow         
    REAL(KIND=8), INTENT(OUT)   :: cflow(n) 
    INTEGER ::  status

    IF(sw .GT. fc)THEN
      flow = frac_gw * (sw - fc)
      IF(n .GT. 0) CALL set_perc_conc(n, csw, cflow) 
      CALL remove_water(sw, n, csw, flow, cflow, status)  
    ELSE 
      flow = 0; cflow(:) = 0             
    ENDIF
    
  END SUBROUTINE remove_soilwater_to_groundwater
!############################################################################################################
  SUBROUTINE baseflow_to_river(isub,               &
                               recharge2gw,        &
                               recharge2gw_conc,   & 
                               base_flow,          &
                               base_flow_conc,     & 
                               aq_water,           &
                               aq_conc             &
                               )
    USE MODELVAR,      ONLY     :    num_ant_type,  &
                                   aquifer
    USE MODELTYPE,   ONLY     :    AQSTATETYPE
    USE GENERAL,     ONLY     :    remove_water
    USE MODELPAR
    INTEGER, INTENT(IN)                 ::    isub
    REAL(KIND=8), INTENT(IN)            ::    recharge2gw
    REAL(KIND=8), INTENT(IN)            ::    recharge2gw_conc(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)         ::    base_flow
    REAL(KIND=8), INTENT(INOUT)         ::    base_flow_conc(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)         ::    aq_water
    REAL(KIND=8), INTENT(INOUT)         ::    aq_conc(num_ant_type)
    INTEGER       ::  status
    REAL(KIND=8)  ::  help, help_c(num_ant_type)
    REAL(KIND=8)  ::  rc_gw

    rc_gw = par_general(genid_gwrc) 
    help = base_flow
    help_c = base_flow_conc
    IF(aq_water .GT. aquifer(isub)%FLO_MIN) THEN
      base_flow = help*EXP(-rc_gw)+recharge2gw*(1.0-EXP(-rc_gw))
      base_flow_conc=(help_c*help*EXP(-rc_gw)+recharge2gw_conc*recharge2gw*(1.0-EXP(-rc_gw)))/base_flow
    ELSE
      base_flow = 0.
    ENDIF
    base_flow_conc(:) = aq_conc(:)
    CALL remove_water(aq_water,num_ant_type,aq_conc,base_flow,base_flow_conc,status)
  END SUBROUTINE baseflow_to_river
!############################################################################################################
  SUBROUTINE set_perc_conc(n,           &
                           waterconc,   & 
                           percconc)
    USE MODELTYPE
    USE MODELVAR,       ONLY      :      simcfg, &
                                       id_on,  &
                                       id_pp
    INTEGER, INTENT(IN)         :: n            
    REAL(KIND=8), INTENT(IN)    :: waterconc(n) 
    REAL(KIND=8), INTENT(OUT)   :: percconc(n)  
    IF(n==0)RETURN
    percconc = waterconc
    percconc(id_on) = 0.
    percconc(id_pp) = 0.
  END SUBROUTINE set_perc_conc
!############################################################################################################
END MODULE GROUNDWATER