MODULE RUN 
  IMPLICIT NONE
CONTAINS
  RECURSIVE SUBROUTINE run_land_1cell(tss,        &
                            c,          &
                            suro,       &
                            soro,       &
                            gwro,       &
                            prec_b,     &
                            thrf_b,     &
                            pet_b,      &
                            epc_b,      &
                            eps_b,      &
                            infilt_b,   &
                            rechg_b,    &
                            suro_b,     &
                            soro1_b,    &
                            soro2_b,    &
                            gwro_b,     &
                            surosat_b,  &
                            suroexc_b,  &
                            clro,       &
                            slro,       &
                            snro,       &
                            sulo,       &
                            solo,       &
                            palo,       &
                            gwlo,       &
                            leach,deglo,iniant,endant,&
                            pathlo,celllo1&
                           )
    USE MODELTYPE
    USE DATETIME
    USE MODELTYPE_ANT,ONLY: ANTPOOlTYPE
    USE MODELPAR
    USE MODELPAR_SED
    USE MODELPAR_ANT
    USE INITIALIZE
    USE INITIALIZE_SED
    USE ABOVEGROUND
    USE GROUNDWATER
    USE LANDWATER
    USE GW_ANT
    USE SOIL_ANT
    USE EROSION
    USE MODELVAR_ANT, ONLY : antusage,peosource,animsource,source_sub,airdrydep,init_sol,end_sol
    USE MODELVAR_SED, ONLY : dep_pc,erod_g,erod_tc,relsed
    USE MODELVAR, ONLY : dp,                &
                          soiltype,          &
                         landuse,           &
                         subbasin,          &
                         river,             &
                         times_sim,         &
                         simcfg,            &
                         sim_length,        &
                         comp_length,       &
                         warm_length,       &
                         num_ant_type,     &
                         max_soillayer,     &
                         num_soiltype,      &
                         num_landuse,       &
                         num_sub,           &
                         temp_air,          &
                         prec_gauge,        &
                         soilthick,         &
                         soildepth,         &
                         sw_wp,             &
                         sw_fc,             &
                         sw_ep,             &
                         sw_pv,             &
                         bd_soil,           &
                         soilmem,           &
                         epotdist,          &
                         streamdepth,       &
                         coef_soil_runoff,  &
                         ini_soilwater,     &
                         aquifer,           &
                         surotime,          &
                         out_land,          &
                         out_hot,           &
                         prec_cell,         &
                         num_sim_cell,      &
                         date_sim,          &
                         num_cell,          &
                         slope,             &
                         sand_soil,         &
                         silt_soil,         &
                         cell_area,         &
                         clay_soil,         &
                         nday_fert,         &
                         outanim,           &
                         outpeo
    INTEGER, INTENT(IN)           ::   tss
    INTEGER, INTENT(IN)           ::   c
    REAL(KIND=8), INTENT(INOUT)   ::   suro(num_sub,sim_length)
    REAL(KIND=8), INTENT(INOUT)   ::   soro(num_sub,sim_length)
    REAL(KIND=8), INTENT(INOUT)   ::   gwro(num_sub,sim_length)
    
    REAL(KIND=8), INTENT(INOUT)   ::   prec_b
    REAL(KIND=8), INTENT(INOUT)   ::   thrf_b
    REAL(KIND=8), INTENT(INOUT)   ::   pet_b
    REAL(KIND=8), INTENT(INOUT)   ::   epc_b
    REAL(KIND=8), INTENT(INOUT)   ::   eps_b
    REAL(KIND=8), INTENT(INOUT)   ::   infilt_b
    REAL(KIND=8), INTENT(INOUT)   ::   rechg_b
    REAL(KIND=8), INTENT(INOUT)   ::   suro_b
    REAL(KIND=8), INTENT(INOUT)   ::   soro1_b
    REAL(KIND=8), INTENT(INOUT)   ::   soro2_b
    REAL(KIND=8), INTENT(INOUT)   ::   gwro_b
    REAL(KIND=8), INTENT(INOUT)   ::   surosat_b
    REAL(KIND=8), INTENT(INOUT)   ::   suroexc_b

    REAL(KIND=8), INTENT(INOUT)   ::    clro(num_sub,sim_length,2)!<kg/m2/d
    REAL(KIND=8), INTENT(INOUT)   ::    slro(num_sub,sim_length,2)!<kg/m2/d
    REAL(KIND=8), INTENT(INOUT)   ::    snro(num_sub,sim_length,2)!<kg/m2/d 

    TYPE(TIMEINFORMATIONTYPE) :: c_time
    INTEGER             ::     is,il,ib,idt,ilayer,iant
    REAL(KIND=8)        ::     pet1
    REAL(KIND=8)        ::     throughfall1
    REAL(KIND=8)        ::     ep_canopy1
    REAL(KIND=8)        ::     icept_storage1
    REAL(KIND=8)        ::     recharge1                    
    REAL(KIND=8)        ::     recharge_conc1(num_ant_type)
    REAL(KIND=8)        ::     runoff_gw1
    REAL(KIND=8)        ::     runoffgw_conc1(num_ant_type)
    REAL(KIND=8)        ::     cdep1(4)
    REAL(KIND=8)        ::     infiltration1
    REAL(KIND=8)        ::     ep_soil1
    REAL(KIND=8)        ::     runoff_surf1
    REAL(KIND=8)        ::     runoff_sat1
    REAL(KIND=8)        ::     runoff_exc1
    REAL(KIND=8)        ::     sw_inc1
    REAL(KIND=8)        ::     macroporeflow1
    REAL(KIND=8)        ::     runoff_soil1(1:max_soillayer)
    REAL(KIND=8)        ::     perc2aqu
    REAL(KIND=8)        ::     s_deeptemp(num_soiltype)
    REAL(KIND=8)        ::     s_temp(0:max_soillayer,num_soiltype)
    REAL(KIND=8)        ::     s_water(0:max_soillayer,num_soiltype)

    REAL(KIND=8)        ::     a_water
    REAL(KIND=8)        ::     a_conc(num_ant_type)
    REAL(KIND=8)        ::     precipitation
    REAL(KIND=8)        ::     temperature

    REAL(KIND=8)        ::     surotime1   !<hour
    INTEGER             ::     offset      !<days to goto rch

    REAL(KIND=8)        ::     infilt_rate_old
    REAL(KIND=8)        ::     infilt_day_old

    REAL(KIND=8)        ::     erosion_c
    REAL(KIND=8)        ::     erosion_l
    REAL(KIND=8)        ::     erosion_s
    REAL(KIND=8)        ::     c1surface_ss(3)
    REAL(KIND=8)        ::     c1runoff1(3)   
    REAL(KIND=8)        ::     c1runoff2(3)   
    REAL(KIND=8)        ::     c1runoff3(3)   
    REAL(KIND=8)        ::     c1baserunoff(3)
    REAL(KIND=8)        ::     ssrelpool1(3)

    REAL(KIND=8)        ::     sufrelpool(3)
    REAL(KIND=8)        ::     solrelpool(3)
    REAL(KIND=8)        ::     baserelpool(3)

    REAL(KIND=8)        ::     surferoded(3)
    REAL(KIND=8)        ::     baseeroded(3)
    REAL(KIND=8)        ::     soleroded(3)
    REAL(KIND=8)        ::     sol1eroded(3)
    REAL(KIND=8)        ::     sol2eroded(3)
    REAL(KIND=8)        ::     sol3eroded(3)

    REAL(KIND=8)        ::     erodclay1
    REAL(KIND=8)        ::     erodsilt1
    REAL(KIND=8)        ::     erodsand1
    
    REAL(KIND=8)        ::     percolate(0:max_soillayer)
!------------antibiotics

    REAL(KIND=8), INTENT(INOUT)   ::   sulo(num_ant_type,num_sub,sim_length) !!kg/m2
    REAL(KIND=8), INTENT(INOUT)   ::   solo(num_ant_type,num_sub,sim_length) !!kg/m2
    REAL(KIND=8), INTENT(INOUT)   ::   palo(num_ant_type,num_sub,sim_length) !!kg/m2
    REAL(KIND=8), INTENT(INOUT)   ::   gwlo(num_ant_type,num_sub,sim_length)
    REAL(KIND=8), INTENT(INOUT)   ::   leach(num_ant_type,num_sub,sim_length)
    REAL(KIND=8), INTENT(INOUT)   ::   deglo(num_ant_type,num_sub,sim_length,2)
    REAL(KIND=8), INTENT(INOUT)   ::   iniant(num_ant_type,num_sub,sim_length,2)
    REAL(KIND=8), INTENT(INOUT)   ::   endant(num_ant_type,num_sub,sim_length,2)

    ! REAL(KIND=8)        ::     s_conc(num_ant_type,0:max_soillayer-1,num_landuse)
    REAL(KIND=8)        ::     p_conc(num_ant_type,0:max_soillayer,num_landuse,4)!!mg/km2
                              !1>clay;2>silt;3>sand;4>sum123
    REAL(KIND=8) ::     s_conc(num_ant_type,0:max_soillayer,num_landuse)
    REAL(KIND=8) ::     suload1(num_ant_type)      !!kg/m2
    REAL(KIND=8) ::     soload1(num_ant_type)      !!kg/m2
    REAL(KIND=8) ::     gwload1(num_ant_type)      !!kg/m2
    REAL(KIND=8) ::     paload1(num_ant_type)      !!kg/m2
    REAL(KIND=8) ::     perc2aqu_ant(num_ant_type) !!mg/km2
    REAL(KIND=8) ::     recharge2gw_ant1(num_ant_type) !!mg/km2
    REAL(KIND=8) ::     erodant1(num_ant_type) !!KG/M2 
    REAL(KIND=8) ::     erosionsed(4) !kg/m2
    REAL(KIND=8) ::     gwantpool(num_ant_type) !!mg/km2
    REAL(KIND=8) ::     gwconc(num_ant_type)   !!ng/L
    TYPE(ANTPOOlTYPE) :: sol_antpool
    REAL(KIND=8) :: degant(num_ant_type,2)
    REAL(KIND=8) ::  init(num_ant_type,2)!!1>par 2>dis
    REAL(KIND=8) ::  end(num_ant_type,2)!!1>par 2>dis

    REAL(KIND=8), INTENT(INOUT) ::  pathlo(num_ant_type,num_sub,sim_length,4,num_landuse)!!
    !!1>surface 2>soilrunoff 3>baseflow 4>eroded
    !!1>林地 2>农田 3>其他

    REAL(KIND=8), INTENT(OUT) :: celllo1(num_ant_type,num_cell,sim_length,5)!!
    !!>1>discharge 2>degradation 3>leaching 4>mixinglayer loss 5>soillayer loss

    REAL(KIND=8) prklo1(num_ant_type,0:max_soillayer)
!--------------------------------------------

    IF(.NOT.ALLOCATED(sol_antpool%m_dissol)) ALLOCATE(sol_antpool%m_dissol(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%m_sorbed)) ALLOCATE(sol_antpool%m_sorbed(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%im_dissol))ALLOCATE(sol_antpool%im_dissol(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%im_sorbed))ALLOCATE(sol_antpool%im_sorbed(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%m_antpool))ALLOCATE(sol_antpool%m_antpool(num_ant_type,0:max_soillayer),source=0._dp)


    IF(.NOT.ALLOCATED(sol_antpool%conc_mdis)) ALLOCATE(sol_antpool%conc_mdis(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%conc_msor)) ALLOCATE(sol_antpool%conc_msor(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%conc_imdis))ALLOCATE(sol_antpool%conc_imdis(num_ant_type,0:max_soillayer),source=0._dp)
    IF(.NOT.ALLOCATED(sol_antpool%conc_imsor))ALLOCATE(sol_antpool%conc_imsor(num_ant_type,0:max_soillayer),source=0._dp)
    gwload1 = 0.
    perc2aqu_ant = 0.
    recharge2gw_ant1 = 0.
    erodant1=0.
    sufrelpool=0.
    solrelpool=0.
    init = 0.
    end=0.
    infilt_rate_old = 0.
    infilt_day_old = 0.

    is=soiltype(c)
    il=landuse(c)
    ib=subbasin(c) 

    cdep1=0.
    icept_storage1 = 0.1
    s_temp(:,:)    = 5.
    s_deeptemp(:)  = 5.
    s_water(:,:)   = ini_soilwater(:,:)

    gwconc(:)=1.*1.E-6 !!mg/L
    erodclay1=0.;erodsilt1=0.;erodsand1=0.
    !<init antibiotic in soil particles mg/g
    !<num_ant_type,max_soillayer,num_landuse>
    ! DO il=1,5
      if(il.eq.2 .or. il.eq.4)then
        p_conc(1, 0:1, il,:) = 1.E-6* 0.1*2!2.48!2.25!2.03!1.80 !2!4.30!3.05!1.8!2 !0.1/1000   !<TC  ng/g->mg/g
        p_conc(2, 0:1, il,:) = 1.E-6* 0.1*2!21.84!14.61!7.37!0.14  !2!14.61!7.37!0.14!2 !0.1/1000   !<CTC  
        p_conc(3, 0:1, il,:) = 1.E-6* 0.1*2!14.78!10.65!6.53!2.40 !2!10.65!6.53!2.4!2 !0.1/1000  !<OTC 
        p_conc(4, 0:1, il,:) = 1.E-6* 0.1*2!10.43!6.96!3.49!0.02   !2!9.31!4.66!0.018!2 !0.1/1000  !<DC 

        p_conc(1, 2, il,:) = 1.E-6*0.01 * 0.1*2!2.48!2.25!2.03!1.80 !2 !4.30!3.05!1.8!2!4.30!3.05!1.8!2!0.1*1.e-3/3  !<TC  
        p_conc(2, 2, il,:) = 1.E-6*0.01 * 0.1*2!21.84!14.61!7.37!0.14 !2 !14.61!7.37!0.14!2!!14.61!7.37!0.14!2!0.1*1.e-3/3!<CTC  
        p_conc(3, 2, il,:) = 1.E-6*0.01 * 0.1*2!14.78!10.65!6.53!2.40 !2 !10.65!6.53!2.4!2!!9.31!4.66!0.018!2!0.1*1.e-3/3 !<OTC 
        p_conc(4, 2, il,:) = 1.E-6*0.01 * 0.1*2!10.43!6.96!3.49!0.02 !2 !9.31!4.66!0.018!2!!10.65!6.53!2.4!2!0.1*1.e-3/3!<DC 

        !<init antibiotic in soilwater ng/L->mg/L
        s_conc(1, 0:1, il) = 4/1000000. !<TC  
        s_conc(2, 0:1, il) = 4/1000000.   !<CTC  
        s_conc(3, 0:1, il) = 4/1000000.  !<OTC 
        s_conc(4, 0:1, il) = 4/1000000.  !<DC 

        s_conc(1, 2, il) = 2/1000000. !<TC  
        s_conc(2, 2, il) = 2/1000000.  !<CTC  
        s_conc(3, 2, il) = 2/1000000.  !<OTC 
        s_conc(4, 2, il) = 2/1000000.  !<DC 
      else
        ! p_conc=!0.
        s_conc=0.10/1000000.
        p_conc(1, 0:1, il,:) = 1.E-6* 0.1*2!2.48!2.25!2.03!1.80 !2 !4.30!3.05!1.8!2!1.8       !<TC  ng/g->mg/g
        p_conc(2, 0:1, il,:) = 1.E-6* 0.1*2!21.84!14.61!7.37!0.14 !2 !14.61!7.37!0.14!2!0.14     !<CTC  
        p_conc(3, 0:1, il,:) = 1.E-6* 0.1*2!14.78!10.65!6.53!2.40 !2 !10.65!6.53!2.4!2!2.4      !<OTC 
        p_conc(4, 0:1, il,:) = 1.E-6* 0.1*2!10.43!6.96!3.49!0.02 !2 !9.31!4.66!0.018!2!       !<DC 

        p_conc(1, 2, il,:) = 1.E-6*0.01 * 0.1*2!2.48!2.25!2.03!1.80 !2 !4.30!3.05!1.8 !2!1.8  !5.55!4.30!3.05! 1.8 !2  !<TC  
        p_conc(2, 2, il,:) = 1.E-6*0.01 * 0.1*2!21.84!14.61!7.37!0.14 !2 !14.61!7.37!0.14 !2!0.14 !!!21.84!14.61!7.37! 0.14 !2  !<CTC  
        p_conc(3, 2, il,:) = 1.E-6*0.01 * 0.1*2!14.78!10.65!6.53!2.40 !2 !10.65!6.53!2.4 !2!2.4  !!!13.95!9.31!4.66! 0.018 !2 !<OTC 
        p_conc(4, 2, il,:) = 1.E-6*0.01 * 0.1*2!10.43!6.96!3.49!0.02 !2 !9.31!4.66!0.018 !2!0.018!!!14.78!10.65!6.53! 2.4 !2 !<DC 
      endif
    ! END DO
    
 
    recharge1      = 0.
    runoff_gw1     = 0.
    a_water        = aquifer(ib)%iniwater
    a_conc(1)      = aquifer(ib)%conc_IN
    a_conc(3)      = aquifer(ib)%conc_SP
    a_conc(2)      = 0.
    a_conc(4)      = 0.

        !TEST:antibiotic source input soil
    CALL int_antpool_antibiotic(isoil      =   is,               &
                                iland      =   il,               &
                                par_conc   =   p_conc(:,:,il,:), &
                                sol_conc   =   s_conc(:,:,il),   &
                                sol_water  =   s_water(:,is),    &
                                antpool    =   sol_antpool,      &
                                aq_water   =   a_water,          &
                                gw_conc    =   gwconc,           &
                                gwantpool  =   gwantpool         &
                               )
  ! init_sol(:,c,0)=sol_antpool%m_dissol(:,0)+sol_antpool%im_dissol(:,0)+&
  !                 sol_antpool%m_sorbed(:,0)+sol_antpool%im_sorbed(:,0)
  ! init_sol(:,c,1)=sol_antpool%m_dissol(:,1)+sol_antpool%im_dissol(:,1)+&
  !                 sol_antpool%m_sorbed(:,1)+sol_antpool%im_sorbed(:,1)
  ! init_sol(:,c,2)=sol_antpool%m_dissol(:,2)+sol_antpool%im_dissol(:,2)+&
  !                 sol_antpool%m_sorbed(:,2)+sol_antpool%im_sorbed(:,2)
  ! init_sol(:,c,3)=sol_antpool%m_dissol(:,3)+sol_antpool%im_dissol(:,3)+&
  !                 sol_antpool%m_sorbed(:,3)+sol_antpool%im_sorbed(:,3) 


!----------------------------------------------------------------------
    DO idt = 1, tss          
   
      IF(simcfg%prec_nc) THEN 
        precipitation = prec_cell(c,idt)
      ELSE
        precipitation = prec_gauge(idt,river(ib)%gaugeID)
      END IF
      
      temperature = temp_air(idt,river(ib)%gaugeID)

      !>Read recorded states
      IF(simcfg%hotstart) THEN
        IF(idt.LE.warm_length) THEN 
          CYCLE
        ELSEIF(idt.EQ.warm_length+1) THEN
          icept_storage1   =out_hot%icept_storage%value1(c)
          s_water(:,is)    =out_hot%soil_water%value2(:,c)
          s_temp(:,is)     =out_hot%soil_temp%value2(:,c)
          s_deeptemp(is)   =out_hot%soil_deeptemp%value1(c)
          runoff_gw1       =out_hot%runoff_gw%value1(c)
          recharge1        =out_hot%recharge%value1(c)
          a_water          =out_hot%aquifer_water%value1(c)
          infilt_day_old   = out_hot%infilt_day_old%value1(c)
          infilt_rate_old  = out_hot%infilt_rate_old%value1(c)
          infiltration1    = out_hot%infiltration%value1(c)
        END IF
      END IF

      if(idt==1828)then
        DO iant = 1,num_ant_type
          init(iant,1)=sum(sol_antpool%m_sorbed(iant,:))+sum(sol_antpool%im_sorbed(iant,:))
          init(iant,2)=sum(sol_antpool%m_dissol(iant,:))+sum(sol_antpool%im_dissol(iant,:))
        ENDDO
          iniant(:,ib,idt,1) = iniant(:,ib,idt,1) + init(:,1)*0.0025
          iniant(:,ib,idt,2) = iniant(:,ib,idt,2) + init(:,2)*0.0025
      endif

      CALL define_current_time(it         =  idt,                           &          
                               date       =  times_sim(idt),                &
                               cur_time   =  c_time                         &
                              )

      CALL above_surface(  cur_time       =  c_time,                        &
                           isub           =  ib,                            &
                           iland          =  il,                            &
                           temp           =  temp_air(idt,1),               &
                           prec           =  precipitation,                 &
                           p_et           =  pet1,                          &
                           rain_to_ground =  throughfall1,                  &
                           evap_canopy    =  ep_canopy1,                    &
                           icept_s        =  icept_storage1                 &
                         )

      CALL model_soil(     c               =  c,                             &
                           it              =  idt,                           &
                           isoil           =  is,                            &
                           iland           =  il,                            &
                           prec            =  precipitation,                 &
                           sthick          =  soilthick(:,is),               &
                           sdepth          =  soildepth(:,is),               &
                           wp              =  sw_wp(:,is),                   &
                           fc              =  sw_fc(:,is),                   &
                           ep              =  sw_ep(:,is),                   &
                           pv              =  sw_pv(:,is),                   &
                           temp            =  temp_air(idt,1),               &
                           rain_to_surface =  throughfall1,                  &
                           p_et            =  pet1,                          &
                           soilmemdeep     =  par_general(genid_deepmem),    &
                           soilmemlayer    =  soilmem(:,is),                 &
                           adhc            =  par_soil(soilid_adhc,is),      &
                           b               =  par_soil(soilid_b,is),         &
                           bf              =  par_soil(soilid_bf,is),        &
                           macrocf         =  par_soil(soilid_macfrac,is),   &
                           infilt          =  infiltration1,                 &
                           surflow         =  runoff_surf1,                  &
                           surfsat         =  runoff_sat1,                   &
                           surfexc         =  runoff_exc1,                   &
                           sw_inc          =  sw_inc1,                       &
                           epotfrac        =  epotdist(:,is),                &
                           evap            =  ep_soil1,                      &
                           ddepth          =  streamdepth(is),               &
                           sol_rc          =  coef_soil_runoff(:,c),         &
                           sol_runoff      =  runoff_soil1(:),               &
                           perc_to_aqu     =  perc2aqu,                      &
                           macroflow       =  macroporeflow1,                &
                           cmacroflow      =  cdep1(:),                      &
                           sol_temp        =  s_temp(:,is),                  &
                           sol_deeptemp    =  s_deeptemp(is),                &
                           sol_water       =  s_water(:,is),                 &
                           infilt_rate_old =  infilt_rate_old,               &
                           infilt_day_old  =  infilt_day_old,                &
                           prk             =  percolate)

      CALL model_aquifer( isub            =  ib,                            &
                           sw_to_gw        =  perc2aqu,                      &
                           re_to_gw        =  recharge1,                     &
                           re_to_gw_conc   =  recharge_conc1(:),             &
                           base_flow       =  runoff_gw1,                    &
                           base_flow_conc  =  runoffgw_conc1(:),             &
                           sol_water       =  s_water(:,is),                 &
                           sol_conc        =  s_conc(:,:,il),                &
                           aq_water        =  a_water,                       &
                           aq_conc         =  a_conc(:)                     &
                           )

    CALL module_soil_erosion(cur_time         =  c_time,                      &
                            c                =  c,                           &
                            it               =  idt,                         &
                            iland            =  il,                          &
                            isoil            =  is,                          &
                            rain_to_surface  =  throughfall1,                &
                            surflow          =  runoff_surf1,                &
                            sol_runoff       =  runoff_soil1(:),             &
                            surfrelpool      =  sufrelpool,                  &
                            soilrelpool      =  solrelpool,                  &
                            relsurflow       =  surferoded,                  &
                            relrunoff        =  soleroded                    &
                            )

  erodclay1 = surferoded(1)  + soleroded(1) !+ baseeroded(1) 
  erodsilt1 = surferoded(2)  + soleroded(2) !+ baseeroded(2)
  erodsand1 = surferoded(3)  + soleroded(3) !+ baseeroded(3)      
      
    CALL module_soil_ant(cur_time        = c_time,                        &
                         c               = c,                             &
                         idt             = idt,                           &
                         isoil           = is,                            &
                         iland           = il,                            &
                         sthick          = soilthick(:,is),               &
                         sol_temp        = s_temp(:,is),                  &
                         sol_water       = s_water(:,is),                 &
                         sol_runoff      = runoff_soil1(:),               &
                         prk             = percolate,                     &
                         surflow         = runoff_surf1,                  &
                         erodedsed       = erodclay1+erodsilt1+erodsand1, &
                         soload          = soload1,                       &
                         suload          = suload1,                       &
                         perc_to_aqu_ant = perc2aqu_ant,                  &
                         erodedant       = erodant1,                      &
                         antpool         = sol_antpool,                   &
                         degrade         = degant,                        &
                         prklo           = prklo1)


  CALL module_gw_ant(base_flow       =  runoff_gw1,                    &
                     aq_water        =  a_water,                       &
                     sw_to_gw_ant    =  perc2aqu_ant,                  &
                     recharge2gw_ant =  recharge2gw_ant1,              &
                     gwantpool       =  gwantpool,                     &
                     gwload          =  gwload1  )


  IF(idt.gt.1827 .and. idt.lt.2193)THEN
  DO iant = 1, num_ant_type
    end(iant,1)=sum(sol_antpool%m_sorbed(iant,:))+sum(sol_antpool%im_sorbed(iant,:))
    end(iant,2)=sum(sol_antpool%m_dissol(iant,:))+sum(sol_antpool%im_dissol(iant,:))
  ENDDO
    endant(:,ib,idt,1) = endant(:,ib,idt,1) + end(:,1)*0.0025
    endant(:,ib,idt,2) = endant(:,ib,idt,2) + end(:,2)*0.0025+gwantpool(:)*0.0025
ENDIF 

    IF(idt+1.LE.tss)THEN
    clro(ib,idt+1,1) = clro(ib,idt+1,1) + surferoded(1)
    slro(ib,idt+1,1) = slro(ib,idt+1,1) + surferoded(2)
    snro(ib,idt+1,1) = snro(ib,idt+1,1) + surferoded(3)
    ENDIF

    clro(ib,idt,2) = clro(ib,idt,2)  + soleroded(1) 
    slro(ib,idt,2) = slro(ib,idt,2)  + soleroded(2) 
    snro(ib,idt,2) = snro(ib,idt,2)  + soleroded(3) 


!---------------antibiotic-------------------------------------------
      IF(idt+1.LE.tss)sulo(:,ib,idt+1) = sulo(:,ib,idt+1) + suload1(:)
      ! sulo(:,ib,idt) = sulo(:,ib,idt) + suload1(:)
      solo(:,ib,idt) = solo(:,ib,idt) + soload1(:)
      palo(:,ib,idt) = palo(:,ib,idt) + erodant1(:)!颗粒态ant入河方式2
      gwlo(:,ib,idt) = gwlo(:,ib,idt) + gwload1(:)
      leach(:,ib,idt)= leach(:,ib,idt)+ perc2aqu_ant(:)*0.0025
      deglo(:,ib,idt,1) = deglo(:,ib,idt,1) + degant(:,1)*0.0025
      deglo(:,ib,idt,2) = deglo(:,ib,idt,2) + degant(:,2)*0.0025
      ! IF(idt+1.LE.tss)palo(:,ib,idt+1) = palo(:,ib,idt+1) + erodant1(:)  !颗粒态ant入河方式2
      ! IF(idt+1.LE.tss)gwlo(:,ib,idt+1) = gwlo(:,ib,idt+1) + gwload1(:)
!----------------------------------------------------------------------

        ! pathlo(:,ib,idt,1,il)=pathlo(:,ib,idt,1,il)+ suload1(:)*2500
        ! pathlo(:,ib,idt,2,il)=pathlo(:,ib,idt,2,il)+ soload1(:)*2500
        ! pathlo(:,ib,idt,3,il)=pathlo(:,ib,idt,3,il)+ gwload1(:)*2500
        ! pathlo(:,ib,idt,4,il)=pathlo(:,ib,idt,4,il)+erodant1(:)*2500
      IF(idt.gt.1827 .and. idt.lt.2193)THEN
      celllo1(:,c,idt,1) = suload1(:)*2500*1.e6+soload1(:)*2500*1.e6+erodant1(:)*2500*1.e6  !!mg/m2
      celllo1(:,c,idt,2) = degant(:,1)*0.00256+degant(:,2)*0.0025 !!mg/km2*km2
      celllo1(:,c,idt,3) = perc2aqu_ant(:)*0.0025                            !!mg/km2*km2
      celllo1(:,c,idt,4) = suload1(:)*2500+erodant1(:)*1.e6*2500+prklo1(:,0)*0.0025  !!mg
      celllo1(:,c,idt,5) = soload1(:)*2500*1.e6 + degant(:,2)*0.0025 + perc2aqu_ant(:)*0.0025  !!mg
      ENDIF
      !!>1>discharge 2>degradation 3>leaching 4>mixinglayer loss 5>soillayer loss
!-----------------------------------------------------------------------

! write(996,*)antrel1(1),soload1(1)+suload1(1)

      ! IF(idt.GE.warm_length+1 .AND. idt.LE.tss) THEN 
      IF(idt.gt.1827 .and. idt.lt.2193)THEN
        prec_b = prec_b + precipitation          
        thrf_b = thrf_b + throughfall1
        pet_b = pet_b + pet1
        infilt_b = infilt_b + sw_inc1
        rechg_b = rechg_b + recharge1
        epc_b = epc_b + ep_canopy1
        eps_b = eps_b + ep_soil1
        suro_b = suro_b + runoff_surf1
        soro1_b = soro1_b + runoff_soil1(1)
        soro2_b = soro2_b + runoff_soil1(2)
        gwro_b = gwro_b + runoff_gw1
        surosat_b = surosat_b + runoff_sat1
        suroexc_b = suroexc_b + runoff_exc1
      END IF
        

      !>Surface runoff route, suro is the amount discharged to the river, don't need to route in routing_land subroutine
      IF(simcfg%rf_route.EQ.1) THEN  
        surotime1 = surotime(c)
        offset = INT(surotime1/24.)+1   !<Converted to days, time of concentration
        IF(idt+offset.LE.tss) suro(ib,idt+offset) = suro(ib,idt+offset) + runoff_surf1
      ELSE   !<default route method
        suro(ib,idt) = suro(ib,idt) + runoff_surf1
      END IF
      soro(ib,idt) = soro(ib,idt) + SUM(runoff_soil1(:)) 
      gwro(ib,idt) = gwro(ib,idt) + runoff_gw1

      IF(simcfg%write_nc_land) THEN 
        IF(idt.GT.warm_length) THEN
          out_land%runoff_surf%value2(c,idt-warm_length)  = runoff_surf1
          out_land%infiltration%value2(c,idt-warm_length)  = infiltration1
          out_land%runoff_soil%value2(c,idt-warm_length)  = SUM(runoff_soil1(:))
          out_land%recharge%value2(c,idt-warm_length)  = recharge1
          out_land%ep_soil%value2(c,idt-warm_length)  = ep_soil1
          out_land%soil_water%value3(:,c,idt-warm_length) = s_water(:,is)
          out_land%soil_temp%value3(:,c,idt-warm_length) = s_temp(:,is)
        END IF
      END IF
      
      IF(idt.EQ.warm_length) THEN
        out_hot%icept_storage%value1(c) = icept_storage1
        out_hot%soil_water%value2(:,c) = s_water(1:3,is)
        out_hot%soil_temp%value2(:,c) = s_temp(1:3,is)
        out_hot%soil_deeptemp%value1(c) = s_deeptemp(is)
        out_hot%runoff_gw%value1(c) = runoff_gw1
        out_hot%recharge%value1(c) = recharge1
        out_hot%aquifer_water%value1(c) = a_water

        out_hot%infilt_day_old%value1(c) = infilt_day_old
        out_hot%infilt_rate_old%value1(c) = infilt_rate_old
        out_hot%infiltration%value1(c) = infiltration1

      END IF
    END DO 

  END SUBROUTINE run_land_1cell
!######################################################################################################
  SUBROUTINE routing_land(isub,           &
                          tss,            &
                          suro,           &
                          soro,           &
                          gwro,           &
                          sub_to_riv      &
                          )
    USE MODELPAR
    USE DATAWRITE, ONLY    :    log_process
    USE MODELTYPE, ONLY    :    SUB2RIVTYPE
    USE MODELVAR,  ONLY    :    cell_area,     &
                                simcfg,        &
                                sim_length,    &
                                warm_length,   &
                                out_hot,    &
                                num_sub
    !>IN&OUT
    INTEGER, INTENT(IN)                ::    isub                         
    INTEGER, INTENT(IN)                ::    tss
    REAL(KIND=8), INTENT(IN)           ::    suro(sim_length)
    REAL(KIND=8), INTENT(IN)           ::    soro(sim_length)
    REAL(KIND=8), INTENT(IN)           ::    gwro(sim_length)   
    TYPE(SUB2RIVTYPE), INTENT(INOUT)   ::    sub_to_riv
    REAL(KIND=8)    ::  suflow_t0, soflow_t0, help_t1
    REAL(KIND=8)    ::  suro_conf, soro_conf
    INTEGER :: it 
    suro_conf = par_general(genid_suroconf)
    soro_conf = par_general(genid_soroconf)

    DO it = 1, tss
      IF(simcfg%hotstart) THEN
        IF(it.LE.warm_length) THEN
          CYCLE 
        ELSEIF(it.EQ.warm_length+1) THEN
          IF(simcfg%rf_route.EQ.0) THEN
            suflow_t0 = out_hot%sub_suflow%value1(isub)
            help_t1=suflow_t0*suro_conf+(1.0-suro_conf)*suro(it)*cell_area/24/3600/1000  
            sub_to_riv%suflow(it)=help_t1
          ELSE 
            sub_to_riv%suflow(it)=suro(it)*cell_area/24/3600/1000
          END IF
          soflow_t0=out_hot%sub_soflow%value1(isub)
          help_t1=soflow_t0*soro_conf+(1.0-soro_conf)*soro(it)*cell_area/24/3600/1000
          sub_to_riv%soflow(it)=help_t1
          sub_to_riv%gwflow(it)=gwro(it)*cell_area/24/3600/1000
          sub_to_riv%flow(it)=sub_to_riv%suflow(it)+sub_to_riv%soflow(it)+sub_to_riv%gwflow(it)
        ELSE
          IF(simcfg%rf_route.EQ.0) THEN
            suflow_t0=sub_to_riv%suflow(it-1)
            help_t1=suflow_t0*suro_conf+(1.0-suro_conf)*suro(it)*cell_area/24/3600/1000  
            sub_to_riv%suflow(it)=help_t1
          ELSE 
            sub_to_riv%suflow(it)=suro(it)*cell_area/24/3600/1000
          END IF
          soflow_t0=sub_to_riv%soflow(it-1)
          help_t1=soflow_t0*soro_conf+(1.0-soro_conf)*soro(it)*cell_area/24/3600/1000
          sub_to_riv%soflow(it)=help_t1
          sub_to_riv%gwflow(it)=gwro(it)*cell_area/24/3600/1000

          sub_to_riv%flow(it)=sub_to_riv%suflow(it)+sub_to_riv%soflow(it)+sub_to_riv%gwflow(it)
        END IF
      ELSE !>Cold start, record data at the warm_length
        IF(it.EQ.1) THEN 
          soflow_t0=0.  
          IF(simcfg%rf_route.EQ.0) THEN
            sub_to_riv%suflow(1)=0.
          ELSE 
            sub_to_riv%suflow(1)=suro(it)*cell_area/24/3600/1000
          END IF 
          sub_to_riv%soflow(1)=0.
          sub_to_riv%gwflow(1)=0.
        ELSE
          !>surface runoff
          IF(simcfg%rf_route.EQ.1) THEN
            sub_to_riv%suflow(it)=suro(it)*cell_area/24/3600/1000
          ELSE
            suflow_t0=sub_to_riv%suflow(it-1)
            help_t1=suflow_t0*suro_conf+(1.0-suro_conf)*suro(it)*cell_area/24/3600/1000  
            sub_to_riv%suflow(it)=help_t1
          ENDIF
          !>interflow
          soflow_t0=sub_to_riv%soflow(it-1)
          help_t1=soflow_t0*soro_conf+(1.0-soro_conf)*soro(it)*cell_area/24/3600/1000
          sub_to_riv%soflow(it)=help_t1
          !>baseflow
          sub_to_riv%gwflow(it)=gwro(it)*cell_area/24/3600/1000 
          sub_to_riv%flow(it)=sub_to_riv%suflow(it)+sub_to_riv%soflow(it)+sub_to_riv%gwflow(it)
          !>record states
          IF(it.EQ.warm_length) THEN  !<Record for hotstart
            out_hot%sub_suflow%value1(isub)=sub_to_riv%suflow(it)
            out_hot%sub_soflow%value1(isub)=sub_to_riv%soflow(it)
            out_hot%sub_gwflow%value1(isub)=sub_to_riv%gwflow(it)
          END IF
        END IF
      END IF
! IF(isub==1) WRITE(999,*) sub_to_riv%gwflow(it)
    END DO

    
  
       
    IF(isub.EQ.num_sub) CALL log_process('Land routing completed.')

! IF(isub==1) WRITE(*,*) 'route land:', sub_to_riv%suflow(15),sub_to_riv%soflow(15),sub_to_riv%gwflow(15)

  END SUBROUTINE routing_land
!######################################################################################################
  SUBROUTINE run_rch(tss,              &
                     sub_to_rivs,      &
                     rivs,             &
                     rivstates,        &
                     rivfluxes,        &
                     sedstates,        &
                     sedfluxes         &
                     )
    USE DATAWRITE,     ONLY              :      log_process !print_rch_nc
    USE SURFACEWATER,  ONLY              :      route_rch_1ts,    &
                                                cal_node_time_map
    USE MODELVAR,      ONLY              :      simcfg,           &
                                                num_sub,          &
                                                num_riv,          &
                                                warm_length,      &
                                                comp_length,      &
                                                date_sim,         &
                                                riv_out_unit,       &
                                                out_rch,&
                                                sim_length,num_ant_type
    USE MODELTYPE,     ONLY              :      SUB2RIVTYPE,      &
                                                RIVERTYPE,        &
                                                RIVSTATETYPE,     &
                                                RIVFLUXTYPE,      &
                                                pair
    USE MODELTYPE_SED, ONLY              :      SEDSTATETYPE, SEDFLUXTYPE
    USE MODELTYPE_ANT,ONLY               :      ANTSTATETYPE
    USE MODELVAR_ANT,ONLY                :      antistate,   &
                                                rchdeg,      &
                                                rchburl,     &
                                                rchout,      &
                                                rchload,     &
                                                antsub2riv,  &
                                                rchstart,    &
                                                rchend
    INTEGER, INTENT(IN)                  ::     tss
    TYPE(SUB2RIVTYPE), INTENT(IN)        ::     sub_to_rivs(num_sub)                          
    TYPE(RIVERTYPE), INTENT(IN)          ::     rivs(num_riv)
    TYPE(RIVSTATETYPE), INTENT(INOUT)    ::     rivstates(num_riv)
    TYPE(RIVFLUXTYPE), INTENT(INOUT)     ::     rivfluxes(num_riv) 
    TYPE(SEDSTATETYPE), INTENT(INOUT)    ::     sedstates(num_riv) 
    TYPE(SEDFLUXTYPE), INTENT(INOUT)     ::     sedfluxes(num_riv) 

    REAL(KIND=8) :: q_up(num_riv)

    INTEGER   ::  idt, error
    CHARACTER ::  error_message
    INTEGER   ::  i,j,k,m
    TYPE(pair) :: para_map(8,24), para_map_post(8,24), para_map_t1(8,24)
    TYPE(pair) :: rch_layer_t1(192)
    
    REAL(KIND=8) ::  pathriv(num_sub,sim_length,7)
    REAL(KIND=8) ::  rchdif(num_sub,num_ant_type,sim_length)
    REAL(KIND=8) ::  dif1(num_ant_type,sim_length)
    REAL(KIND=8) ::  rchex(num_sub,num_ant_type,sim_length,2,4)!!1>resuspended 2>settle 1>caly2>silt3>sand4>sum
    REAL(KIND=8) ::  ex1(num_ant_type,sim_length,2)
    REAL(KIND=8) ::  ex2(num_ant_type,sim_length,2,3)!1>resuspended 2>settle  1>caly2>silt3>sand
    rch_layer_t1 = [ &
    pair(1,1),pair(1,1),pair(0,0),pair(1,1),pair(0,0),pair(1,1),pair(0,0),pair(1,1),pair(0,0),pair(1,1),pair(0,0),&
    pair(1,1),pair(0,0),pair(1,1),pair(0,0),pair(1,1),pair(0,0),pair(1,1),pair(1,1),pair(0,0),pair(0,0),pair(1,1),&
    pair(0,0),pair(0,0),&  !<layer1
    pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),&  !<layer2
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),&  !<layer3
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),&  !<layer4
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),&  !<layer5
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(1,0),pair(0,0),&
    pair(0,0),pair(0,0),&  !<layer6
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(1,0),pair(0,0),&  !<layer7
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),pair(0,0),&
    pair(0,0),pair(1,0) &  !<layer8
    ]
    
    DO i = 1, 8
      DO j = 1, 24
        para_map_t1(i,j)=rch_layer_t1((i-1)*24+j)
      END DO
    END DO

    CALL log_process('River routing started...') 

    !>Time loop 
    DO idt = 1, tss 
      !>parallel map calculation
      para_map = para_map_t1
      IF(idt==1) THEN 
        para_map = para_map_t1
      ELSE 
        CALL cal_node_time_map(para_map,idt)
      END IF
! print *, "OK0"
      ! IF(simcfg%hotstart) THEN 
      !   IF(idt .LE. warm_length) THEN 
      !     CYCLE 
      !   ELSE
      !     IF(simcfg%write_txt_rch) WRITE(riv_out_unit,'(A11)',ADVANCE='NO') date_sim(idt)
      !     CALL route_rch_1ts(it            =      idt,            &
      !                        para_map      =      para_map,       &
      !                        sub_to_rivs   =      sub_to_rivs,    &
      !                        rivs          =      rivs,           &
      !                        q_up          =      q_up,           &
      !                        rivstates     =      rivstates,      &
      !                        rivfluxes     =      rivfluxes,      &
      !                        sedstates     =      sedstates,      &
      !                        sedfluxes     =      sedfluxes,      &
      !                        ierr          =      error,          &
      !                        message       =      error_message,  &
      !                        antistate     =      antistate       &
      !                        )
      !   END IF
      ! ELSE !<Cold start
        IF(simcfg%write_txt_rch) WRITE(riv_out_unit,'(A11)',ADVANCE='NO') date_sim(idt)
        ! print *, idt
        CALL route_rch_1ts(it            =      idt,            &
                           para_map      =      para_map,       &
                           sub_to_rivs   =      sub_to_rivs,    &
                           rivs          =      rivs,           &
                           q_up          =      q_up,           &
                           rivstates     =      rivstates,      &
                           rivfluxes     =      rivfluxes,      &
                           sedstates     =      sedstates,      &
                           sedfluxes     =      sedfluxes,      &
                           ierr          =      error,          &
                           message       =      error_message,  &
                           antistate     =      antistate       &
                           )
      ! END IF
      IF(simcfg%write_txt_rch) WRITE(riv_out_unit, *) 
    END DO  !<End time loop

    !>Deal with non-1st layers 
    DO idt = tss+1, tss+SIZE(para_map,1)-1
      !>post map
      para_map_post = para_map_t1
      CALL cal_node_time_map(para_map_post,idt)
      DO k = 1, SIZE(para_map_post,2)
        DO m = 1, idt-tss
          para_map_post(m,k)%x = 0
        END DO
      END DO
      !>Calculate based on post map
      CALL route_rch_1ts(it            =      idt,            &
                         para_map      =      para_map_post,  &
                         sub_to_rivs   =      sub_to_rivs,    &
                         rivs          =      rivs,           &
                         q_up          =      q_up,           &
                         rivstates     =      rivstates,      &
                         rivfluxes     =      rivfluxes,      &
                         sedstates     =      sedstates,      &
                         sedfluxes     =      sedfluxes,      &
                         ierr          =      error,          &
                         message       =      error_message,  &
                         antistate     =      antistate       &
                         )
      
    END DO
    !-------------------------------------------------------------------------------------------------
    !>Record results
    ! IF(simcfg%write_nc_rch) THEN
    !   CALL print_rch_nc(time_len      =    comp_length,          &
    !                      var_record   =    out_rch,             &
    !                      filename     =    'out/out_rch.nc')
    ! END IF

    DO i=1,tss

      ! ex2(:,i,1,3)=rchex(5,:,i,1,3)
      ! ex2(:,i,2,3)=rchex(5,:,i,2,3)
      ! ex2(:,i,1,1)=rchex(5,:,i,1,1)
      ! ex2(:,i,2,1)=rchex(5,:,i,2,1)
      ! ex2(:,i,1,2)=rchex(5,:,i,1,2)
      ! ex2(:,i,2,2)=rchex(5,:,i,2,2)
      ! ex2(:,i,1,3)=rchex(5,:,i,1,3)
      ! ex2(:,i,2,3)=rchex(5,:,i,2,3)
      
      DO j =1,num_riv
      !  rchload(j,:,i)=(antsub2riv(j)%load(:,i)+antsub2riv(j)%paload(:,i)+antsub2riv(j)%gwload(:,i))*1.e6
      !  IF(rivs(j)%nodeB.gt.0)rchout(j,:,i)=antistate(j)%outant(:,i)
       IF(rivs(j)%nodeB.gt.0)rchout(j,:,i)=antistate(j)%outant_d1(:,i)+antistate(j)%outant_p1(:,i)
       IF(rivs(j)%nodeB.gt.0)rchdeg(j,:,i,1)=antistate(j)%deg(:,i,1)
       IF(rivs(j)%nodeB.gt.0)rchdeg(j,:,i,2)=antistate(j)%deg(:,i,2)
       IF(rivs(j)%nodeB.gt.0)rchburl(j,:,i)=antistate(j)%bural(:,i)
       IF(rivs(j)%nodeB.gt.0)rchstart(j,:,i)=antistate(j)%mass(:,i,1)
       IF(rivs(j)%nodeB.gt.0)rchload(j,:,i)=antistate(j)%load(:,i)
       IF(rivs(j)%nodeB.gt.0)rchend(j,:,i)=antistate(j)%mass(:,i,2)
       IF(rivs(j)%nodeB.gt.0)rchdif(j,:,i)=antistate(j)%antdif(:,i)
       IF(rivs(j)%nodeB.gt.0)rchex(j,:,:,:,:)=antistate(j)%antex(:,:,:,:)
       
       IF(rivs(j)%nodeB.gt.0)dif1(:,i)=dif1(:,i)+rchdif(j,:,i)
       IF(rivs(j)%nodeB.gt.0)ex1(:,i,1)=ex1(:,i,1)+rchex(j,:,i,1,4)
       IF(rivs(j)%nodeB.gt.0)ex1(:,i,2)=ex1(:,i,2)+rchex(j,:,i,2,4)

       ex2(:,i,1,1)=ex2(:,i,1,1)+rchex(j,:,i,1,1)
       ex2(:,i,2,1)=ex2(:,i,2,1)+rchex(j,:,i,2,1)

       ex2(:,i,1,2)=ex2(:,i,1,2)+rchex(j,:,i,1,2)
       ex2(:,i,2,2)=ex2(:,i,2,2)+rchex(j,:,i,2,2)

       ex2(:,i,1,3)=ex2(:,i,1,3)+rchex(j,:,i,1,4)
       ex2(:,i,2,3)=ex2(:,i,2,3)+rchex(j,:,i,2,4)
      !  ex2(:,i,2,3)

       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,1)=sum(rchdeg(j,:,i,1))+sum(rchdeg(j,:,i,2))
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,2)=sum(rchburl(j,:,i))
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,3)=sum(rchstart(j,:,i))
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,4)=sum(rchload(j,:,i))
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,5)=sum(rchout(j,:,i))   
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,6)=sum(rchdeg(j,:,i,1))    
       IF(rivs(j)%nodeB.gt.0)pathriv(j,i,7)=sum(rchdeg(j,:,i,2))          
                ! IF(rivs(iriv)%nodeB.gt.0)rchdeg(iriv,:,idt)=deg(iriv,:,idt)
         ! IF(rivs(iriv)%nodeB.gt.0)rchburl(iriv,:,idt)=burl(iriv,:,idt)
      !  if(j==21)write(998,*)antistate(20)%outant(1,i),antistate(17)%outant(1,i)
      !  if(j==17)write(881,*)antistate(20)%outant(1,i),antistate(17)%outant(1,i)
        ! rchout(j,:,i)=antistate(j)%outant_d1(:,i)+antistate(j)%outant_p1(:,i)  

      ENDDO

      !   write(997,*)date_sim(i),&
      !  sum(rchdeg(:,1,i,1))+sum(rchdeg(:,1,i,2)),sum(rchdeg(:,2,i,1))+sum(rchdeg(:,2,i,2)),&
      !  sum(rchdeg(:,3,i,1))+sum(rchdeg(:,3,i,2)),sum(rchdeg(:,4,i,1))+sum(rchdeg(:,4,i,2)),&
      !  rchout(24,:,i),&
      !  sum(rchload(1:23,1,i)),sum(rchload(1:23,2,i)),sum(rchload(1:23,3,i)),sum(rchload(1:23,4,i))

        ! write(998,*)rchout(:,1,i)+rchout(:,2,i)+rchout(:,3,i)+rchout(:,4,i)
        ! write(998,*)sum(ex2(:,i,1,3)),sum(ex2(:,i,2,3)),sum(dif1(:,i))
  !       write(998,*)sum(dif1(:,i)),sum(rchex(:,1,i,1,4))+sum(rchex(:,2,i,1,4))+sum(rchex(:,3,i,1,4))+sum(rchex(:,4,i,1,4)),&
  !                   sum(rchex(:,1,i,2,4))+sum(rchex(:,2,i,2,4))+sum(rchex(:,3,i,2,4))+sum(rchex(:,4,i,2,4)),&
  !                   sum(rchdeg(:,1,i,1))+sum(rchdeg(:,2,i,1))+sum(rchdeg(:,3,i,1))+sum(rchdeg(:,4,i,1)),&
  !                   sum(rchdeg(:,1,i,2))+sum(rchdeg(:,2,i,2))+sum(rchdeg(:,3,i,2))+sum(rchdeg(:,4,i,2)),&
  !     !  sum(rchdeg(:,1,i,1))+sum(rchdeg(:,1,i,2)),sum(rchdeg(:,2,i,1))+sum(rchdeg(:,2,i,2)),&
  !     !  sum(rchdeg(:,3,i,1))+sum(rchdeg(:,3,i,2)),sum(rchdeg(:,4,i,1))+sum(rchdeg(:,4,i,2)),&
  !                   sum(rchburl(:,1,i))+sum(rchburl(:,2,i))+sum(rchburl(:,3,i))+sum(rchburl(:,4,i)),&
  !                   sum(rchload(1:23,1,i))+sum(rchload(1:23,2,i))+sum(rchload(1:23,3,i))+sum(rchload(1:23,4,i)),&
  !                   sum(rchout(24,:,i))


  ! write(996,*)sum(rchload(23,:,i))*1.e3,sum(rchdif(23,:,i))*1.e3
! write(998,*) sum(rchdeg(23,:,i,2))*1.e3,sum(rchburl(23,:,i))*1.e3
      !   write(998,*)rchdeg(:,1,i,1),rchdeg(:,1,i,2)
      !  write(997,*)date_sim(i),&
      ! !  sum(rchdeg(:,1,i,1)),sum(rchdeg(:,2,i,1)),sum(rchdeg(:,3,i,1)),sum(rchdeg(:,4,i,1)),&
      ! !  sum(rchdeg(:,1,i,2)),sum(rchdeg(:,2,i,2)),sum(rchdeg(:,3,i,2)),sum(rchdeg(:,4,i,2)),&
      !  sum(rchdeg(:,1,i,1))+sum(rchdeg(:,1,i,2)),sum(rchdeg(:,2,i,1))+sum(rchdeg(:,2,i,2)),&
      !  sum(rchdeg(:,3,i,1))+sum(rchdeg(:,3,i,2)),sum(rchdeg(:,4,i,1))+sum(rchdeg(:,4,i,2)),&
      !  sum(rchburl(:,1,i)),sum(rchburl(:,2,i)),sum(rchburl(:,3,i)),sum(rchburl(:,4,i)),&
      !  sum(rchload(1:23,1,i)),sum(rchload(1:23,2,i)),sum(rchload(1:23,3,i)),sum(rchload(1:23,4,i)),&
      !  sum(rchstart(:,1,i-1)),sum(rchstart(:,2,i-1)),sum(rchstart(:,3,i-1)),sum(rchstart(:,4,i-1)),&
      !  rchout(24,:,i),&
      !  sum(rchstart(:,1,i)),sum(rchstart(:,2,i)),sum(rchstart(:,3,i)),sum(rchstart(:,4,i))
      !  sum(rchend(:,1,i-1)),sum(rchend(:,2,i-1)),sum(rchend(:,3,i-1)),sum(rchend(:,4,i-1)),&
      !  sum(rchend(:,1,i)),sum(rchend(:,2,i)),sum(rchend(:,3,i)),sum(rchend(:,4,i))

      ! if(i.gt.1826 .and. i.lt.2193)write(996,*)date_sim(i),&
      ! sum(dif1(:,i)),sum(rchex(:,1,i,1,4))+sum(rchex(:,2,i,1,4))+sum(rchex(:,3,i,1,4))+sum(rchex(:,4,i,1,4)),&
      !                sum(rchex(:,1,i,2,4))+sum(rchex(:,2,i,2,4))+sum(rchex(:,3,i,2,4))+sum(rchex(:,4,i,2,4)),&
      !               sum(rchout(24,:,i)),&
      ! sum(rchdeg(:,1,i,1))+sum(rchdeg(:,2,i,1))+sum(rchdeg(:,3,i,1))+sum(rchdeg(:,4,i,1))
      !  if(i.gt.1826 .and. i.lt.2193)write(881,*)rchout(17,1,i),rchout(20,1,i)
        

      ! ENDIF
     ENDDO


    !  DO i=1,tss
    !   ! IF(i.gt.1827 .and. i.lt.2193) write(996,*)date_sim(i),rivfluxes(24)%flow(i)
    !   DO j =1,num_riv

    !     ! if(j==20)write(996,*)antistate(j)%antconc(1,i)*1.e6
    !     IF(i.gt.1827 .and. i.lt.2193) THEN
          
    !       if(j==5)write(888,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==7)write(882,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==9)write(887,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                          antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==15)write(886,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==17)write(885,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==20)write(884,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       if(j==23)write(883,*)date_sim(i), antistate(j)%antconc(1,i)*1.e6,antistate(j)%antconc(2,i)*1.e6,&
    !                           antistate(j)%antconc(3,i)*1.e6,antistate(j)%antconc(4,i)*1.e6
    !       ! if(j==23)write(998,*)antistate(j)%antconc(:,i)*1.e6
    !     ENDIF
    !   ENDDO
      ! IF(i.gt.1827 .and. i.lt.2193)write(777,*)date_sim(i),pathriv(:,i,1),pathriv(:,i,2),pathriv(:,i,3),pathriv(:,i,4),&
      !                                         pathriv(:,i,5)
      ! IF(i.gt.1827 .and. i.lt.2193)write(996,*)rivfluxes(5)%flow(i),rivfluxes(7)%flow(i),rivfluxes(9)%flow(i),&
      !                                          rivfluxes(15)%flow(i),rivfluxes(17)%flow(i),rivfluxes(20)%flow(i),&
      !                                          rivfluxes(23)%flow(i)
      ! ENDDO
    ! write(996,*)'river'
    ! write(996,*)rchstart(23,1,1828),rchstart(23,1,2192),sum(rchload(23,1,1828:2192)),sum(rchout(23,1,1828:2192))
    ! write(996,*)rchstart(23,2,1828),rchstart(23,2,2192),sum(rchload(23,2,1828:2192)),sum(rchout(23,2,1828:2192))
    ! write(996,*)rchstart(23,3,1828),rchstart(23,3,2192),sum(rchload(23,3,1828:2192)),sum(rchout(23,3,1828:2192))
    ! write(996,*)rchstart(23,4,1828),rchstart(23,4,2192),sum(rchload(23,4,1828:2192)),sum(rchout(23,4,1828:2192))
    
    CALL log_process('River routing ended.')

  END SUBROUTINE run_rch
!######################################################################################################


END MODULE RUN