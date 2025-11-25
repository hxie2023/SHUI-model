MODULE GW_ANT
  
  IMPLICIT NONE
CONTAINS
  !##################################################################################################################
  SUBROUTINE module_gw_ant(base_flow,aq_water,sw_to_gw_ant,recharge2gw_ant,gwantpool,gwload)
    USE MODELVAR, ONLY :num_ant_type
    USE MODELPAR_ANT

    REAL(KIND=8), INTENT(IN)     ::     base_flow
    REAL(KIND=8), INTENT(IN)     ::     aq_water
    REAL(KIND=8), INTENT(IN)     ::     sw_to_gw_ant(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)  ::     recharge2gw_ant(num_ant_type) !!mg/km2
    REAL(KIND=8), INTENT(INOUT)  ::     gwantpool(num_ant_type)
    REAL(KIND=8), INTENT(INOUT)  ::     gwload(num_ant_type) !kg/m2

    CALL ant_rchrg(sw_to_gw_ant,recharge2gw_ant,gwantpool)  
    CALL ant_baseflow(base_flow,aq_water,antpar_general(genid_gw_mix),gwantpool,gwload)

  END SUBROUTINE module_gw_ant
!############################################################################################################
  SUBROUTINE ant_rchrg(sw_to_gw_ant,recharge2gw_ant,gwantpool)
    
    USE MODELTYPE,   ONLY       :       SOILSTATETYPE
    USE MODELPAR_ANT
    USE MODELVAR,    ONLY       :       num_ant_type,       &
                                        max_soillayer,       &
                                        coef_delay_perc 
    USE MODELPAR
    REAL(KIND=8),INTENT(IN)     ::      sw_to_gw_ant(num_ant_type)    !!mg/km2
    REAL(KIND=8),INTENT(INOUT)  ::      recharge2gw_ant(num_ant_type) !!mg/km2
    REAL(KIND=8),INTENT(INOUT)  ::      gwantpool(num_ant_type)       !!mg.km2

    REAL(KIND=8) gw_delay
    REAL(KIND=8) help(num_ant_type)


    help   = recharge2gw_ant       
    gw_delay = antpar_general(genid_ant_gwdelay)

    recharge2gw_ant = (1.0 - EXP(-1./gw_delay))*sw_to_gw_ant + EXP(-1./gw_delay)*help
    gwantpool = gwantpool + recharge2gw_ant
    ! IF(num_ant_type.GT.0) THEN
    !   IF(re_to_gw .GT. 0.) re_to_gw_conc(:) = ((1.0 - EXP(-1./gw_delay))*re_to_gw* &
    !                        re_to_gw_conc(:)+EXP(-1./gw_delay)*help*help_c)/re_to_gw  
    ! ENDIF

  END SUBROUTINE ant_rchrg
!############################################################################################################

!############################################################################################################
  SUBROUTINE ant_baseflow(base_flow,aq_water,gwmix,gwantpool,gwload)

    USE MODELVAR,      ONLY     :    num_ant_type,  &
                                   aquifer
    USE MODELTYPE,   ONLY     :    AQSTATETYPE
    USE GENERAL,     ONLY     :    remove_water
    USE MODELPAR
 
   
    REAL(KIND=8), INTENT(INOUT)      ::    gwantpool(num_ant_type)   !!mg/km2
    REAL(KIND=8), INTENT(INOUT)      ::    gwload(num_ant_type)    !!kg/m2

    REAL(KIND=8), INTENT(IN)         ::    base_flow !!mm
    REAL(KIND=8), INTENT(IN)         ::    aq_water  !!mm
    REAL(KIND=8), INTENT(IN)         ::    gwmix  !!none

    REAL(KIND=8)  ::    gw_antconc(num_ant_type)
  


    gw_antconc = gwantpool/(aq_water*gwmix+base_flow) !!mg/km2 * mm-1
! write(996,'(2F20.8)',ADVANCE="no") gwantpool(1),gw_antconc(1)  
    gwload = gw_antconc*base_flow*1.e-12 !!mg/km2->kg/m2

    gwantpool=gwantpool-(gwload*1.e12)


  END SUBROUTINE ant_baseflow
!############################################################################################################

END MODULE GW_ANT