MODULE ABOVEGROUND
    IMPLICIT NONE
    PRIVATE 
    PUBLIC above_surface
  CONTAINS   
    SUBROUTINE above_surface(cur_time,       &
                             isub,           &
                             iland,          &
                             temp,           &
                             prec,           &
                             p_et,           &
                             rain_to_ground, &
                             evap_canopy,    &
                             icept_s)
      USE MODELTYPE, ONLY      :      TIMEINFORMATIONTYPE
      USE MODELVAR,    ONLY      :      simcfg
      TYPE(TIMEINFORMATIONTYPE),INTENT(IN) :: cur_time
      INTEGER, INTENT(IN)       ::     isub
      INTEGER, INTENT(IN)       ::     iland
      REAL(KIND=8), INTENT(IN)  ::     temp
      REAL(KIND=8), INTENT(IN)  ::     prec
      REAL(KIND=8), INTENT(OUT) ::     p_et
      REAL(KIND=8), INTENT(OUT) ::     rain_to_ground
      REAL(KIND=8), INTENT(OUT) ::     evap_canopy
      REAL(KIND=8), INTENT(OUT) ::     icept_s
      CALL pet_default(cur_time,iland,temp,p_et)
      CALL canopy_icept(iland,prec,p_et,rain_to_ground,evap_canopy,icept_s)
    END SUBROUTINE above_surface

    SUBROUTINE pet_default(cur_time,     &
                           id_landuse,   &
                           temp,         &
                           p_et)

      USE MODELVAR, ONLY :     temp_t,            &   
                               pi,                &
                               timesteps_per_day

      USE MODELTYPE, ONLY  :   TIMEINFORMATIONTYPE
      USE MODELPAR
      TYPE(TIMEINFORMATIONTYPE),INTENT(IN) :: cur_time
      INTEGER, INTENT(IN)          ::    id_landuse
      REAL(KIND=8), INTENT(IN)     ::    temp
      REAL(KIND=8), INTENT(OUT)    ::    p_et
      REAL(KIND=8) coef_pet_am
      REAL(KIND=8) coef_pet_ph
      REAL(KIND=8) coef_pet
      coef_pet = par_landuse(luid_coef_pet, id_landuse)  
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
    END SUBROUTINE pet_default

    SUBROUTINE canopy_icept(id_landuse,        &
                            prec,              &
                            p_et,              & 
                            rain_to_ground,    &
                            evap_canopy,       &
                            icept_s)
      USE MODELVAR, ONLY : dp,             &
                         eps_dp,         &
                         twothird_dp
      USE MODELPAR
      INTEGER, INTENT(IN)         :: id_landuse           
      REAL(KIND=8), INTENT(IN)    :: prec
      REAL(KIND=8), INTENT(IN)    :: p_et
      REAL(KIND=8), INTENT(OUT)   :: rain_to_ground     
      REAL(KIND=8), INTENT(OUT)   :: evap_canopy
      REAL(KIND=8), INTENT(INOUT) :: icept_s
      REAL(KIND=8) prec_plus_content
      REAL(KIND=8) icept_max, icept
      icept_max = par_landuse(luid_icept_max, id_landuse)
      prec_plus_content = icept_s + prec
      IF (prec_plus_content .GE. icept_max) THEN 
        rain_to_ground = prec_plus_content - icept_max
        icept = icept_max - icept_s
        icept_s = icept_max
      ELSE
        rain_to_ground = 0.0_dp
        icept_s = prec_plus_content
        icept = prec
      END IF
      IF(icept_max .GT. eps_dp) THEN !<icept_max happens
        evap_canopy = p_et * (icept_s/icept_max) ** twothird_dp
      ELSE 
        evap_canopy = 0.0_dp
      END IF
      IF(icept_s .GT. evap_canopy) THEN 
        icept_s = icept_s - evap_canopy
      ELSE 
        evap_canopy = icept_s
        icept_s = 0.0_dp
      END IF
    END SUBROUTINE canopy_icept
END MODULE ABOVEGROUND