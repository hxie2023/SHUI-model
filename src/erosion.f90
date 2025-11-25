MODULE EROSION
  ! USE DATAWRITE, ONLY : log_process
  IMPLICIT NONE
CONTAINS
SUBROUTINE module_soil_erosion(cur_time,c,it,iland,isoil,rain_to_surface,&
                            surflow,sol_runoff,&
                            surfrelpool,soilrelpool,relsurflow,relrunoff)

  USE MODELVAR, ONLY : num_cell, &
                       sim_length,&
                       max_soillayer,& 
                       slope,     &
                       clay_soil, &
                       silt_soil, &
                       sand_soil
  USE MODELTYPE, ONLY   :   TIMEINFORMATIONTYPE
  USE MODELPAR_SED
  USE MODELPAR


  TYPE(TIMEINFORMATIONTYPE)   ::   cur_time
  INTEGER,INTENT(IN)          ::   it
  INTEGER,INTENT(IN)          ::   c 
  INTEGER,INTENT(IN)          ::   iland
  INTEGER,INTENT(IN)          ::   isoil
  REAL(KIND=8), INTENT(IN)    ::   surflow                 !<saturated overland flow and excess infiltration (mm)
  REAL(KIND=8), INTENT(IN)    ::   rain_to_surface            !mm
  REAL(KIND=8), INTENT(IN)    ::   sol_runoff(max_soillayer)  !mm
  REAL(KIND=8) erodcell(4) !!1>clay 2>silt 3>sand 4>sum
  REAL(KIND=8), INTENT(INOUT) ::   surfrelpool(3)  !kg/m2
  REAL(KIND=8), INTENT(INOUT) ::   soilrelpool(3)  !kg/m2
  REAL(KIND=8), INTENT(OUT)   ::   relsurflow(3)   !kg/m2
  REAL(KIND=8), INTENT(OUT)   ::   relrunoff(3)    !kg/m2


  REAL(KIND=8) :: total_cropcover
  REAL(KIND=8) :: total_groundcover
  REAL(KIND=8) :: ph

    CALL get_cover_ph(cur_time,              &
                      iland,                   &
                      total_cropcover,         &
                      total_groundcover,       &
                      ph                       &
                      )
    CALL soil_erosion(c,                                  & 
                      it,                                 &
                      rain_to_surface,                    &
                      surflow,                            &
                      ph,                                 &
                      slope(c),                           &
                      sedpar_general(genid_imp),          &
                      total_cropcover,                    &
                      total_groundcover,                  &
                      sedpar_soil(soilid_dksand,isoil),   &
                      sedpar_soil(soilid_dksilt,isoil),   &
                      sedpar_soil(soilid_dkclay,isoil),   &
                      sedpar_soil(soilid_drsand,isoil),   &
                      sedpar_soil(soilid_drsilt,isoil),   &
                      sedpar_soil(soilid_drclay,isoil),   &
                      sand_soil(1,isoil)/100,             &
                      silt_soil(1,isoil)/100,             &
                      clay_soil(1,isoil)/100,             &
                      sedpar_landuse(luid_manning,iland), &
                      sedpar_landuse(c_factor,iland),     & 
                      erodcell                        &
                      )

  CALL soil_release (isoil,                                  &
                   surflow ,                                 &
                   sol_runoff  ,                             &
                   sedpar_general(genid_m_eroddecay),      &
                   sedpar_general(genid_m_pprelmax),        &
                   sedpar_general(genid_m_pprelexp),        &
                   erodcell,&
                   surfrelpool,                              &
                   soilrelpool ,                             &
                   relsurflow ,                              &
                   relrunoff                               &
                   )                   

END SUBROUTINE module_soil_erosion  
!##########################################################################################################

SUBROUTINE soil_erosion(c,                  &
                          it,                 &
                          rain_to_surface,    &
                          surflow,            &
                          ph,                 &
                          slope1,             &
                          imp,                &
                          c_cover,            &
                          g_cover,            &
                          dk_sand,            &
                          dk_silt,            &
                          dk_clay,            &
                          dr_sand,            &
                          dr_silt,            &
                          dr_clay,            &                      
                          pro_sand,           &
                          pro_silt,           &
                          pro_clay,           &
                          par_vr,             &
                          c_factor,           &
                          erodcell            &
                          )
    !>Reference: Daily MMF method by Choi et al., 2017
    !>First, detachment, second, transport, third, settling, finally, loss
    USE INITIALIZE_SED
    USE MODELPAR
    USE MODELVAR_SED, ONLY :  vs_sand,       &
                              vs_silt,       &
                              vs_clay,       &
                              n_reference,   &
                              d_reference
    USE MODELVAR, ONLY  :     cell_area,     &
                              pi,            &
                              num_cell,      &
                              sim_length
                           
    INTEGER, INTENT(IN)       ::  c 
    INTEGER, INTENT(IN)       ::  it                       
    REAL(KIND=8), INTENT(IN)  ::  rain_to_surface !<mm
    REAL(KIND=8), INTENT(IN)  ::  surflow         !<mm
    REAL(KIND=8), INTENT(IN)  ::  ph              !<plant height,m
    REAL(KIND=8), INTENT(INOUT)  ::  slope1       !<original slope1 unit was %
    REAL(KIND=8), INTENT(IN)  ::  imp             
    REAL(KIND=8), INTENT(IN)  ::  c_cover         
    REAL(KIND=8), INTENT(IN)  ::  g_cover         
    REAL(KIND=8), INTENT(IN)  ::  dk_sand         
    REAL(KIND=8), INTENT(IN)  ::  dk_silt
    REAL(KIND=8), INTENT(IN)  ::  dk_clay
    REAL(KIND=8), INTENT(IN)  ::  dr_sand
    REAL(KIND=8), INTENT(IN)  ::  dr_silt
    REAL(KIND=8), INTENT(IN)  ::  dr_clay
    REAL(KIND=8), INTENT(IN)  ::  pro_sand
    REAL(KIND=8), INTENT(IN)  ::  pro_silt
    REAL(KIND=8), INTENT(IN)  ::  pro_clay
    REAL(KIND=8), INTENT(IN)  ::  par_vr
    REAL(KIND=8), INTENT(IN)  ::  c_factor
    REAL(KIND=8), INTENT(OUT) ::  erodcell(4) !!1>clay 2>silt 3>sand 4>sum

    !>local
    REAL(KIND=8) ::  DEP_sand
    REAL(KIND=8) ::  DEP_silt
    REAL(KIND=8) ::  DEP_clay
    REAL(KIND=8) :: ri
    REAL(KIND=8) :: UDT, ULD  !<kinetic energy density
    REAL(KIND=8) :: KE        !<kinetic energy of effective rainfall
    REAL(KIND=8) :: EPA
    REAL(KIND=8) :: slope1_ang, slope_aux, v_runoff, len_s
    REAL(KIND=8) :: F_sand, F_silt, F_clay, H_sand, H_silt, H_clay, G_sand, G_silt, G_clay
    REAL(KIND=8) :: Nf_sand, Nf_silt, Nf_clay
    REAL(KIND=8) :: TC_sand, TC_silt, TC_clay, TC
    REAL(KIND=8) :: v_reference, tt
  
    ri = rain_to_surface/24.   !<rainfall intensity, mm/h
    IF(slope1.LE.0.) slope1 = 0.
    slope_aux = slope1/100.  !< % -> m/m
    IF(slope_aux.GT.0.04) slope_aux = 0.05247+0.06363*slope_aux-0.182*EXP(-62.38*slope_aux)
    len_s = SQRT(cell_area) !>meter
    !>Different method calculating velocity of overland flow
!     v_runoff = par_landuse(luid_suroroute,4)*SQRT(slope_aux)
    v_runoff = surflow**0.667*SQRT(slope_aux)/par_vr
    ! v_runoff = surflow**0.667*SQRT(slope_aux)/SQRT(par_vr*par_vr+8.E-3*300*(d_reference)**1.333/2./9.8)!<include D & NV
!     tt = (surflow*1.E-3/3600./24.)**(-0.4)*len_s**0.6*(par_vr)**0.6/((slope1/100.)**0.3)
!     v_runoff = len_s/tt
    slope1_ang = ATAN(slope1/100.)      !<from % -> angle
  
    UDT = 10.3*ri**(2./9.)  !<kinetic energy density of direct throughfall (UDT; J m−2 mm−1)
    ! UDT = 9.81 + 10.6 * LOG10(ri)  !<For east asia, CAREFULL may be negtive
    ULD = MAX(15.8*SQRT(ph)-5.87,0.)  !<kinetic energy density of leaf drainage (ULT; Jm −2 mm−1)
    KE = rain_to_surface*((1-c_cover)*UDT+c_cover*ULD)  
    EPA = imp+(1-imp)*g_cover

    F_sand = dk_sand*pro_sand*MAX(0.,(1-epa))*KE*1.E-3
    F_silt = dk_silt*pro_silt*MAX(0.,(1-epa))*KE*1.E-3
    F_clay = dk_clay*pro_clay*MAX(0.,(1-epa))*KE*1.E-3
  
    H_sand = dr_sand*pro_sand*surflow**1.5*MAX(0.,(1-epa))*(SIN(slope1_ang))**0.3*1.E-3
    H_silt = dr_silt*pro_silt*surflow**1.5*MAX(0.,(1-epa))*(SIN(slope1_ang))**0.3*1.E-3
    H_clay = dr_clay*pro_clay*surflow**1.5*MAX(0.,(1-epa))*(SIN(slope1_ang))**0.3*1.E-3
  
    IF(v_runoff.GT.0..AND.surflow.GT.0) THEN 
    !>flow depth, use urflow to represent flow depth?
      Nf_sand = vs_sand*len_s/(v_runoff*(surflow*0.001))      !<runoff depth unit should be meter
      Nf_silt = vs_silt*len_s/(v_runoff*(surflow*0.001))
      Nf_clay = vs_clay*len_s/(v_runoff*(surflow*0.001))
  
! write(998,*) 0.441*(Nf_sand**0.29),0.441*(Nf_silt**0.29),0.441*(Nf_clay**0.29)
  
      DEP_sand = MIN(0.441*(Nf_sand**0.29), 1.)
      DEP_silt = MIN(0.441*(Nf_silt**0.29), 1.)
      DEP_clay = MIN(0.441*(Nf_clay**0.29), 1.)
  ! write(6,*)Nf_sand, DEP_sand
    ELSE 
      DEP_sand = 1.
      DEP_silt = 1.
      DEP_clay = 1.
    END IF
    !-----------
    G_sand = (F_sand+H_sand)*(1-DEP_sand)   !<kg/m2
    G_silt = (F_silt+H_silt)*(1-DEP_silt)
    G_clay = (F_clay+H_clay)*(1-DEP_clay)
    !-----------
  
    !>Transport capacity of runoff, kg/m2, based on reference velocity
    v_reference = SQRT(TAN(slope1_ang))*d_reference**(2./3.)/n_reference
    IF(v_reference.GT.0.) THEN 
      ! TC = (v_runoff/v_reference)*surflow*surflow*SIN(slope1_ang)*0.001
      TC = c_factor*surflow*surflow*SIN(slope1_ang)*0.001
    ELSE 
      TC = 0.
    END IF
    !>Another method to calculate TC, early version of MMF
    !>C factor equals that in SWAT
!     TC = 0.2*surflow*surflow*SIN(slope1_ang)*0.001
    TC_sand = TC*pro_sand    !<kg/m2
    TC_silt = TC*pro_silt
    TC_clay = TC*pro_clay

    erodcell(1)=min(TC_clay, G_clay)
    erodcell(2)=min(TC_silt, G_silt)
    erodcell(3)=min(TC_sand, G_sand)
    erodcell(4)=erodcell(1)+erodcell(2)+erodcell(4)

  END SUBROUTINE soil_erosion
!########################################################################################################################
!########################################################################################################################
  SUBROUTINE get_cover_ph(cur_time,              &
                          iland,                 &
                          total_cropcover,       &
                          total_groundcover,     &
                          ph)

    USE MODELVAR,  ONLY : num_crop,           &
                          crop_luid
    USE MODELVAR_SED, ONLY : cropdata
    USE MODELTYPE, ONLY : TIMEINFORMATIONTYPE
    USE DATETIME,  ONLY : in_season_period,   &
                          in_season_end

    TYPE(TIMEINFORMATIONTYPE), INTENT(IN) :: cur_time 
    INTEGER, INTENT(IN)       :: iland
    REAL(KIND=8), INTENT(OUT) :: total_cropcover     !<total crop cover of a cell (-)
    REAL(KIND=8), INTENT(OUT) :: total_groundcover   !<total ground cover of a cell (-)
    REAL(KIND=8), INTENT(OUT) :: ph                  !<plant height, meter
    LOGICAL permanent   !<crop is permanent 
    INTEGER k, i, j
    LOGICAL  ::  found                          !>crop/row in CropData
    INTEGER  ::  bd1, bd2, bd3, bd4, bd5        !>cultivation dates
    INTEGER  ::  maxday1, maxday2
    REAL(KIND=8)  ::  cropcover, groundcover    !>cover by crop
    REAL(KIND=8)  ::  part                      !>fraction of cell area covered by the crop
    REAL(KIND=8)  ::  gperiod, gperiod2         !>growth period (days)

    INTEGER  ::  position(num_crop)
  
    total_cropcover = 1.
    total_groundcover = 1.
    cropcover = 0.
    groundcover = 0.
    ph = 0.

    found = .FALSE.
    position(:) = 0
    DO j = 1, num_crop
      IF(iland.EQ.crop_luid(j)) THEN 
        found = .TRUE.
        position(j) = j
      END IF
    END DO
    IF(found.EQV..FALSE.) RETURN

    DO i = 1, SIZE(position)
      k = position(i)
      IF(k.EQ.0) CYCLE
      !------------------------------------------------------------------------------------------
      !>k /= 0, has position values
      part = cropdata(k)%part      !<Propotion that crops/forest cover the cell
      bd1 = cropdata(k)%baredayno1        !spring ploughing
      bd2 = cropdata(k)%baredayno2        !sow date/beginning of growing season 
      bd3 = cropdata(k)%baredayno3        !harvest
      bd4 = cropdata(k)%baredayno4        !autumn ploughing
      bd5 = cropdata(k)%baredayno5        !winter crops sowing date
      permanent = .FALSE.
      IF(bd1.EQ.0.AND.bd4.EQ.0) permanent=.TRUE.
      IF(bd1.GT.0.AND.bd4.EQ.0) bd4 = bd1  !<???
    
      IF(permanent) THEN  !<permanent, e.g., forest
        cropcover = cropdata(k)%ccmax1
        groundcover = cropdata(k)%gcmax1
        ph = MAX(cropdata(k)%phmax1,cropdata(k)%phmax2)
      ELSE  !<spring, winter and row crops
        !>Calculate growth periods
        IF(bd2<=bd3) gperiod = REAL(bd3-bd2)/2.
        IF(bd2>bd3)  gperiod = REAL(bd3 + cur_time%prevdoy - bd2)/2.
        maxday1 = INT(bd2 + gperiod)  !<day of maximum crop and ground cover in summer
        gperiod = REAL(maxday1 - bd2)
        IF(maxday1>cur_time%prevdoy) maxday1 = maxday1 - cur_time%prevdoy

        !>
        IF(bd5>=181)THEN 
          maxday2 = INT(bd5 + REAL(cur_time%prevdoy-bd5)/2.)!<day of maximum crop and ground cover for winter crops in autumn (northern hemispere)
        ELSE
          maxday2 = INT(bd5 + REAL(181-bd5)/2.)   !same on southern hemispere
        ENDIF
    
        gperiod2 = REAL(maxday2 - bd5)
    
        !Calculate cover by crops
        IF(bd5>0)THEN        !<winter crop
          IF(maxday2/=bd2.AND.in_season_end(maxday2,bd2-1,cur_time))THEN !<confirmed
            cropcover   = cropdata(k)%ccmax2
            groundcover = cropdata(k)%gcmax2
            ph          = MAX(cropdata(k)%phmax2,ph)
          ELSEIF(in_season_period(bd2,NINT(gperiod),cur_time))THEN !<confirmed
            IF(cur_time%dayno>=bd2)THEN
              cropcover   = cropdata(k)%ccmax2 +(cropdata(k)%ccmax1-cropdata(k)%ccmax2)*((cur_time%dayno-REAL(bd2))/gperiod)
              groundcover = cropdata(k)%gcmax2 +(cropdata(k)%gcmax1-cropdata(k)%gcmax2)*((cur_time%dayno-REAL(bd2))/gperiod)
              ph = MAX(cropdata(k)%phmax2 +(cropdata(k)%phmax1-cropdata(k)%phmax2)*((cur_time%dayno-REAL(bd2))/gperiod),ph)
            ELSE    !bd2 and dayno on different sides of new year
              cropcover   = cropdata(k)%ccmax2 + (cropdata(k)%ccmax1-cropdata(k)%ccmax2)* & 
                            ((cur_time%dayno+cur_time%prevdoy-REAL(bd2))/gperiod)
              groundcover = cropdata(k)%gcmax2 + (cropdata(k)%gcmax1-cropdata(k)%gcmax2)* &
                            ((cur_time%dayno+cur_time%prevdoy-REAL(bd2))/gperiod)
              ph = MAX(cropdata(k)%phmax2 + (cropdata(k)%phmax1-cropdata(k)%phmax2)* & 
                            ((cur_time%dayno+cur_time%prevdoy-REAL(bd2))/gperiod),ph)              
            ENDIF
          ELSEIF(maxday1/=bd3.AND.in_season_end(maxday1,bd3-1,cur_time))THEN !<confirmed
            cropcover   = cropdata(k)%ccmax1
            groundcover = cropdata(k)%gcmax1
            ph          = MAX(cropdata(k)%phmax1,ph)
          ELSEIF(bd3/=bd4.AND.in_season_end(bd3,bd4-1,cur_time))THEN 
            cropcover   = cropdata(k)%gcmax1
            groundcover = cropdata(k)%gcmax1
            ph          = MAX(0.,ph)
          ELSEIF(bd4/=bd5.AND.in_season_end(bd4,bd5-1,cur_time))THEN
            cropcover = 0.
            groundcover = 0.
            ph = MAX(0.,ph)
          ELSEIF(in_season_period(bd5,NINT(gperiod2),cur_time))THEN
            IF(cur_time%dayno>=bd5)THEN
              cropcover   = cropdata(k)%ccmax2 * ((cur_time%dayno - REAL(bd5)) / gperiod2)
              groundcover = cropdata(k)%gcmax2 * ((cur_time%dayno - REAL(bd5)) / gperiod2)
              ph = MAX(cropdata(k)%phmax2 * ((cur_time%dayno - REAL(bd5)) / gperiod2),ph)
            ELSE
              cropcover   = cropdata(k)%ccmax2 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd5)) / gperiod2)
              groundcover = cropdata(k)%gcmax2 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd5)) / gperiod2)
              ph = MAX(cropdata(k)%phmax2 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd5)) / gperiod2),ph)
            ENDIF
          ENDIF
        ELSE  !<bd5=0？represent no winter crop
          IF(bd4/=bd2.AND.in_season_end(bd4,bd2-1,cur_time))THEN
            cropcover   = 0.
            groundcover = 0.
            ph = MAX(0.,ph)
          ELSEIF(in_season_period(bd2,NINT(gperiod),cur_time))THEN
            IF(cur_time%dayno>=bd2)THEN
              cropcover   = cropdata(k)%ccmax1 * ((cur_time%dayno - REAL(bd2)) / gperiod)
              groundcover = cropdata(k)%gcmax1 * ((cur_time%dayno - REAL(bd2)) / gperiod)
              ph = MAX(cropdata(k)%phmax1 * ((cur_time%dayno - REAL(bd2)) / gperiod),ph)
            ELSE
              cropcover   = cropdata(k)%ccmax1 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd2)) / gperiod)
              groundcover = cropdata(k)%gcmax1 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd2)) / gperiod)
              ph = MAX(cropdata(k)%phmax1 * ((cur_time%dayno + cur_time%prevdoy - REAL(bd2)) / gperiod),ph)
            ENDIF
          ELSEIF(maxday1/=bd3.AND.in_season_end(maxday1,bd3-1,cur_time))THEN
            cropcover   = cropdata(k)%ccmax1
            groundcover = cropdata(k)%gcmax1
            ph = MAX(cropdata(k)%phmax1,ph)
          ELSEIF(bd3/=bd4.AND.in_season_end(bd3,bd4-1,cur_time))THEN
            cropcover   = cropdata(k)%gcmax1
            groundcover = cropdata(k)%gcmax1  
            ph = MAX(0.,ph)  
          ENDIF
        ENDIF
      ENDIF !bd5>0

! WRITE(999,*) total_cropcover, total_groundcover
      total_cropcover   = total_cropcover * (1. - part * cropcover) !<not covered area
      ! total_cropcover   = 1 * (1. - part * cropcover)
      total_groundcover = total_groundcover * (1. - part * groundcover)
      ! total_groundcover = 1 * (1. - part * groundcover)
! IF(k==2) WRITE(999,*) total_cropcover, total_groundcover
    ENDDO !<End of crop loop

    total_cropcover   = 1. - total_cropcover   !<covered area
    total_groundcover = 1. - total_groundcover !<covered area

! WRITE(999,*) total_cropcover, total_groundcover

  END SUBROUTINE get_cover_ph
!#################################################################################################################################

  !##########################################################################################################
  SUBROUTINE routing_erosion(isub,            &
                              tss,            &
                              clro,           &
                              slro,           &
                              snro,           &
                              sub_to_riv      &
                              )
    USE MODELPAR
    USE MODELTYPE, ONLY      :    SUB2RIVTYPE
    USE MODELVAR,  ONLY      :    cell_area,        &
                                  sim_length,       &
                                  num_ant_type,    &
                                  simcfg
    !>IN&OUT
    INTEGER, INTENT(IN)                ::    isub                         
    INTEGER, INTENT(IN)                ::    tss  
    ! REAL(KIND=8), INTENT(IN)           ::    paro(sim_length) !kg/d
    REAL(KIND=8), INTENT(IN)           ::    clro(sim_length,2)!kg/m2/d idt;surf,soil
    REAL(KIND=8), INTENT(IN)           ::    slro(sim_length,2)!kg/m2/d idt;surf,soil
    REAL(KIND=8), INTENT(IN)           ::    snro(sim_length,2)!kg/m2/d idt;surf,soil 
    TYPE(SUB2RIVTYPE), INTENT(INOUT)   ::    sub_to_riv
    !>Local
    INTEGER idt,ised
    REAL(KIND=8)    ::  erosion_t0
    REAL(KIND=8)    ::  erodclay_t0,erodsilt_t0,erodsand_t0
    REAL(KIND=8)    ::  help_t1
    REAL(KIND=8)    ::  suro_conf,soro_conf

    ! suro_conf = 0.2168
    ! soro_conf = 0.4752

    suro_conf = par_general(genid_suroconf)
    soro_conf = par_general(genid_soroconf)

    DO idt = 1, tss


      !----suf----
      sub_to_riv%sed(1,1,idt)=clro(idt,1)*cell_area
      sub_to_riv%sed(2,1,idt)=slro(idt,1)*cell_area
      sub_to_riv%sed(3,1,idt)=snro(idt,1)*cell_area
      sub_to_riv%sed(4,1,idt)=sub_to_riv%sed(1,1,idt)+sub_to_riv%sed(2,1,idt)+sub_to_riv%sed(3,1,idt)

      IF(idt.EQ.1) THEN 
        erosion_t0=0.
      else
      !----sol----
      erosion_t0=sub_to_riv%sed(1,2,idt-1)
      help_t1=erosion_t0*soro_conf+(1.0-soro_conf)*clro(idt,2)*cell_area
      sub_to_riv%sed(1,2,idt)=help_t1

      erosion_t0=sub_to_riv%sed(2,2,idt-1)
      help_t1=erosion_t0*soro_conf+(1.0-soro_conf)*slro(idt,2)*cell_area
      sub_to_riv%sed(2,2,idt)=help_t1

      erosion_t0=sub_to_riv%sed(3,2,idt-1)
      help_t1=erosion_t0*soro_conf+(1.0-soro_conf)*snro(idt,2)*cell_area
      sub_to_riv%sed(3,2,idt)=help_t1 

      !-----------------------------------------------------------------------------------------------
      ENDIF 
      sub_to_riv%sed(4,2,idt)=sub_to_riv%sed(1,2,idt)+sub_to_riv%sed(2,2,idt)+sub_to_riv%sed(3,2,idt)
      sub_to_riv%sed(4,3,idt)=sub_to_riv%sed(4,1,idt)+sub_to_riv%sed(4,2,idt)
      sub_to_riv%erosion_c(idt)=sub_to_riv%sed(1,1,idt)+sub_to_riv%sed(1,2,idt)
      sub_to_riv%erosion_l(idt)=sub_to_riv%sed(2,1,idt)+sub_to_riv%sed(2,2,idt)
      sub_to_riv%erosion_s(idt)=sub_to_riv%sed(3,1,idt)+sub_to_riv%sed(3,2,idt)
    END DO

  END SUBROUTINE routing_erosion
!##########################################################################################################
!>Eroded sediment delay to reach
!#################################################################################################################### 
   SUBROUTINE soil_release( isoil,        &
                          surflow,      &
                          sol_runoff,   &
                          m_eroddecay,  &
                          m_pprelmax,   &
                          m_pprelexp,   &
                          erodcell,     &
                          surfrelpool,  &
                          soilrelpool,  &
                          relsurflow,  &
                          relrunoff   &
                        )

      USE MODELVAR , ONLY : max_soillayer, &
                            clay_soil,     &
                            silt_soil,     &
                            sand_soil
      USE MODELVAR_SED,ONLY : crunoff

      INTEGER,INTENT(IN)          ::   isoil
      REAL(KIND=8), INTENT(IN)    ::   surflow                    !mm
      REAL(KIND=8), INTENT(IN)    ::   sol_runoff(max_soillayer)  !mm
      REAL(KIND=8), INTENT(IN)    ::   m_eroddecay
      REAL(KIND=8), INTENT(IN)    ::   m_pprelmax
      REAL(KIND=8), INTENT(IN)    ::   m_pprelexp
      REAL(KIND=8), INTENT(INOUT) ::   erodcell(4) !!1>clay 2>silt 3>sand 4>sum      
      REAL(KIND=8), INTENT(INOUT) ::   surfrelpool(3) !kg/m2
      REAL(KIND=8), INTENT(INOUT) ::   soilrelpool(3) !kg/m2
      REAL(KIND=8), INTENT(OUT)   ::   relsurflow(3)
      REAL(KIND=8), INTENT(OUT)   ::   relrunoff(3)
      !>LOCAL
      INTEGER :: ised
      REAL(KIND=8) :: totalrunoff
      REAL(KIND=8) :: f_surf,f_soil
      
      relsurflow=0.
      relrunoff=0.
      crunoff(1,isoil) = 10.

!>先计算地表径流浓度
      totalrunoff = surflow+sol_runoff(1)+sol_runoff(2)+sol_runoff(3)
      f_surf=surflow/totalrunoff
      f_soil=(sol_runoff(1)+sol_runoff(2)+sol_runoff(3))/totalrunoff
      
      erodcell(1)=min(erodcell(1),66250.*clay_soil(1,isoil)*0.01)
      erodcell(2)=min(erodcell(2),66250.*silt_soil(1,isoil)*0.01)
      erodcell(3)=min(erodcell(3),66250.*sand_soil(1,isoil)*0.01)
      erodcell(4)=SUM(erodcell(1:3))
      !>%ssrelpool的单位为 kg/m2
      IF(totalrunoff>0.) THEN 
        surfrelpool=surfrelpool+erodcell(1:3)
        soilrelpool(1)=soilrelpool(1) + crunoff(1,isoil)*sum(sol_runoff(1:3))*1.e-6 !!mg/L
        soilrelpool(2)=soilrelpool(2) + crunoff(2,isoil)*sum(sol_runoff(1:3))*1.e-6 !!mg/L
        soilrelpool(3)=soilrelpool(3) + crunoff(3,isoil)*sum(sol_runoff(1:3))*1.e-6 !!mg/L

        !>%ssrelpool每日有衰减
        surfrelpool=surfrelpool*EXP(-1. * m_eroddecay)
        soilrelpool=soilrelpool*EXP(-1. * m_eroddecay)
        !>再计算pool中的泥沙释放入河量，SSrel并更新pool
        IF(m_pprelmax>0.)THEN
          DO ised=1,3
          relsurflow(ised) = MIN(surfrelpool(ised), surfrelpool(ised)*((surflow/m_pprelmax)**m_pprelexp))
          relrunoff(ised)  = MIN(soilrelpool(ised), soilrelpool(ised)*((sum(sol_runoff(1:3))/(m_pprelmax))**m_pprelexp))
          ENDDO
          surfrelpool=surfrelpool-relsurflow
          soilrelpool=soilrelpool-relrunoff
        ELSE
          relsurflow =surfrelpool
          relrunoff  =soilrelpool
          surfrelpool=0.
          soilrelpool=0.
        ENDIF
      ELSE !>没有地表和地下径流，则只发生每日衰减
        surfrelpool=surfrelpool*EXP(-1. * m_eroddecay)
        soilrelpool=soilrelpool*EXP(-1. * m_eroddecay)
      END IF
    END SUBROUTINE soil_release

END MODULE EROSION