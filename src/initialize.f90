MODULE INITIALIZE
  IMPLICIT NONE
CONTAINS 
!######################################################################################################
  SUBROUTINE initialize_model
    USE MODELVAR,  ONLY  :  hotstart_file,     &
                            simcfg,            &
                            surotime
    USE MODELVAR_SED, ONLY : surocell                
    USE MODELPAR,  ONLY  :  par_landuse,       &
                            luid_suroroute
    CALL initialize_var
    CALL initialize_var_riv
    ! IF(simcfg%hotstart) CALL initialize_state(FILENAME = hotstart_file)  
    CALL runoff_time(parsuroroute  =   par_landuse(luid_suroroute,:),&
                     suro_time     =   surotime,                      &
                     trace         =   surocell ) 

  END SUBROUTINE initialize_model
!######################################################################################################
  SUBROUTINE initialize_var
    USE MODELTYPE
    USE MODELPAR
    USE DATAWRITE,   ONLY   :   log_process
    USE MODELVAR,    ONLY   :   num_ant_type,       &
                                max_soillayer,       &
                                num_soiltype,        &
                                num_landuse,         &
                                num_sub,             &
                                num_riv,             &
                                ncol_grid,           &
                                nrow_grid,           &
                                sim_length,          &
                                comp_length,         &
                                epotdist,            &
                                ksat,                &
                                sw_fc,               &
                                sw_wp,               &
                                sw_pv,               &
                                sw_ep,               &
                                ini_soilwater,       & 
                                riverflux,           &
                                river,               &
                                riverstate,          &
                                aquifer,             &
                                num_node,            &
                                coef_delay_perc,     & 
                                soilmem,             &
                                flow_node,           &
                                dp,                  &
                                i4,                  &
                                bdate,               &
                                edate,               &
                                simcfg,              &
                                num_cell,            &
                                soiltype,            &
                                landuse,             &
                                timesteps_per_day,   &
                                soildepth,           &
                                soilthick,           &
                                coef_soil_runoff,    &
                                slope,               &
                                runoff_soil,         &
                                aquiferdata,           &
                                cell_area,           &
                                subbasin,            &
                                suro_sub,            &
                                soro_sub,            &
                                gwro_sub,            &
                                sub2riv,             &
                                runoff_surf,         &
                                infiltration,        &
                                surotime,            &
                                out_land,            &
                                out_rch,             &
                                out_hot,             &
                                prec_cell,           &
                                num_sim_cell,        &
                                realzero,            &
                                outpeo,              &
                                outanim,outant
    IMPLICIT NONE
    INTEGER      ::   isub, i, is, il
    INTEGER      ::   ir
    INTEGER      ::   num_simyr
    REAL(KIND=8) ::   sums,rc0(num_cell), rc1(num_cell), rc2(num_cell), b(num_cell)
    REAL(KIND=8) ::   perc_tt(0 : max_soillayer, num_soiltype)
    LOGICAL      ::   prepare_hot

    IF(.NOT.ALLOCATED(outanim)) ALLOCATE(outanim(num_cell,num_ant_type+1),source=0._dp)
    IF(.NOT.ALLOCATED(outpeo)) ALLOCATE(outpeo(num_cell,num_ant_type+1),source=0._dp)
    IF(.NOT.ALLOCATED(outant)) ALLOCATE(outant(num_cell,num_ant_type+1),source=0._dp)

    num_sim_cell = 0

    IF(simcfg%rf_route.EQ.1) ALLOCATE(surotime(num_cell))

    IF(simcfg%prec_nc) THEN 
      IF(.NOT.ALLOCATED(prec_cell)) ALLOCATE(prec_cell(num_cell,sim_length))
    END IF

    !>Prepare for writing nc, only for compute-end period
    IF(simcfg%write_nc_land) THEN 
      IF(.NOT.ALLOCATED(out_land%runoff_surf%value2)) THEN
        ALLOCATE(out_land%runoff_surf%value2(num_cell,comp_length),source=-9999._dp)
        out_land%runoff_surf%name = 'runoff_surf'
      END IF 
      IF(.NOT.ALLOCATED(out_land%infiltration%value2)) THEN
        ALLOCATE(out_land%infiltration%value2(num_cell,comp_length),source=-9999._dp)
        out_land%infiltration%name = 'infiltration'
      END IF 
      IF(.NOT.ALLOCATED(out_land%runoff_soil%value2)) THEN
        ALLOCATE(out_land%runoff_soil%value2(num_cell,comp_length),source=-9999._dp)
        out_land%runoff_soil%name = 'runoff_soil'
      END IF 
      IF(.NOT.ALLOCATED(out_land%recharge%value2)) THEN
        ALLOCATE(out_land%recharge%value2(num_cell,comp_length),source=-9999._dp)
        out_land%recharge%name = 'recharge'
      END IF 
      IF(.NOT.ALLOCATED(out_land%ep_soil%value2)) THEN
        ALLOCATE(out_land%ep_soil%value2(num_cell,comp_length),source=-9999._dp)
        out_land%ep_soil%name = 'ep_soil'
      END IF 
      IF(.NOT.ALLOCATED(out_land%soil_water%value3)) THEN
        ALLOCATE(out_land%soil_water%value3(max_soillayer,num_cell,comp_length),source=-9999._dp)
        out_land%soil_water%name = 'soil_water'
      END IF 
      IF(.NOT.ALLOCATED(out_land%soil_temp%value3)) THEN
        ALLOCATE(out_land%soil_temp%value3(max_soillayer,num_cell,comp_length),source=-9999._dp)
        out_land%soil_temp%name = 'soil_temp'
      END IF 
      IF(.NOT.ALLOCATED(out_land%sub_suflow%value2)) THEN
        ALLOCATE(out_land%sub_suflow%value2(num_sub,comp_length),source=-9999._dp)
        out_land%sub_suflow%name = 'sub_suflow'
      END IF 
      IF(.NOT.ALLOCATED(out_land%sub_soflow%value2)) THEN
        ALLOCATE(out_land%sub_soflow%value2(num_sub,comp_length),source=-9999._dp)
        out_land%sub_soflow%name = 'sub_soflow'
      END IF 
      IF(.NOT.ALLOCATED(out_land%sub_gwflow%value2)) THEN
        ALLOCATE(out_land%sub_gwflow%value2(num_sub,comp_length),source=-9999._dp)
        out_land%sub_gwflow%name = 'sub_gwflow'
      END IF 
    END IF

    IF(simcfg%write_nc_rch) THEN 
      IF(.NOT.ALLOCATED(out_rch%rch_flow%value2)) THEN 
        ALLOCATE(out_rch%rch_flow%value2(num_sub,comp_length),source=-9999._dp)
        out_rch%rch_flow%name = 'riv_flow'
      END IF
      IF(.NOT.ALLOCATED(out_rch%rch_vol%value2)) THEN 
        ALLOCATE(out_rch%rch_vol%value2(num_sub,comp_length),source=-9999._dp)
        out_rch%rch_vol%name = 'riv_vol'
      END IF
    END IF

    !{Prepare output for hotstart and also read for hotstart
    prepare_hot = .TRUE.
    IF(prepare_hot.OR.simcfg%hotstart) THEN 
      IF(.NOT.ALLOCATED(out_hot%icept_storage%value1)) THEN
        ALLOCATE(out_hot%icept_storage%value1(num_cell),source=-9999._dp)
        out_hot%icept_storage%name = 'icept_storage'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%soil_water%value2)) THEN
        ALLOCATE(out_hot%soil_water%value2(max_soillayer,num_cell),source=-9999._dp)
        out_hot%soil_water%name = 'soil_water'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%soil_temp%value2)) THEN
        ALLOCATE(out_hot%soil_temp%value2(max_soillayer,num_cell),source=-9999._dp)
        out_hot%soil_temp%name = 'soil_temp'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%soil_deeptemp%value1)) THEN
        ALLOCATE(out_hot%soil_deeptemp%value1(num_cell),source=-9999._dp)
        out_hot%soil_deeptemp%name = 'soil_deeptemp'
      END IF
      IF(.NOT.ALLOCATED(out_hot%recharge%value1)) THEN
        ALLOCATE(out_hot%recharge%value1(num_cell),source=-9999._dp)
        out_hot%recharge%name = 'recharge'
      END IF
      IF(.NOT.ALLOCATED(out_hot%runoff_gw%value1)) THEN
        ALLOCATE(out_hot%runoff_gw%value1(num_cell),source=-9999._dp)
        out_hot%runoff_gw%name = 'runoff_gw'
      END IF  
      IF(.NOT.ALLOCATED(out_hot%aquifer_water%value1)) THEN
        ALLOCATE(out_hot%aquifer_water%value1(num_cell),source=-9999._dp)
        out_hot%aquifer_water%name = 'aquifer_water'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%infilt_day_old%value1)) THEN
        ALLOCATE(out_hot%infilt_day_old%value1(num_cell),source=-9999._dp)
        out_hot%infilt_day_old%name = 'infilt_day_old'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%infilt_rate_old%value1)) THEN
        ALLOCATE(out_hot%infilt_rate_old%value1(num_cell),source=-9999._dp)
        out_hot%infilt_rate_old%name = 'infilt_rate_old'
      END IF
      IF(.NOT.ALLOCATED(out_hot%infiltration%value1)) THEN
        ALLOCATE(out_hot%infiltration%value1(num_cell),source=-9999._dp)
        out_hot%infiltration%name = 'infiltration'
      END IF
      IF(.NOT.ALLOCATED(out_hot%sub_suflow%value1)) THEN
        ALLOCATE(out_hot%sub_suflow%value1(num_sub),source=-9999._dp)
        out_hot%sub_suflow%name = 'sub_suflow'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%sub_soflow%value1)) THEN
        ALLOCATE(out_hot%sub_soflow%value1(num_sub),source=-9999._dp)
        out_hot%sub_soflow%name = 'sub_soflow'
      END IF 
      IF(.NOT.ALLOCATED(out_hot%sub_gwflow%value1)) THEN
        ALLOCATE(out_hot%sub_gwflow%value1(num_sub),source=-9999._dp)
        out_hot%sub_gwflow%name = 'sub_gwflow'
      END IF  
      IF(.NOT.ALLOCATED(out_hot%rivflux_V1%value1)) THEN
        ALLOCATE(out_hot%rivflux_V1%value1(num_sub),source=-9999._dp)
        out_hot%rivflux_V1%name = 'rivflux_V1'
      END IF  
      IF(.NOT.ALLOCATED(out_hot%rivstate_Q1%value1)) THEN
        ALLOCATE(out_hot%rivstate_Q1%value1(num_sub),source=-9999._dp)
        out_hot%rivstate_Q1%name = 'rivstate_Q1'
      END IF  
      IF(.NOT.ALLOCATED(out_hot%rivstate_Q2%value1)) THEN
        ALLOCATE(out_hot%rivstate_Q2%value1(num_sub),source=-9999._dp)
        out_hot%rivstate_Q2%name = 'rivstate_Q2'
      END IF  

      IF(.NOT.ALLOCATED(out_hot%q_up%value1)) THEN
        ALLOCATE(out_hot%q_up%value1(num_sub),source=-9999._dp)
        out_hot%q_up%name = 'q_up'
      END IF 

    END IF
    !}

    num_simyr = edate%Year - bdate%Year + 1

    IF(.NOT.ALLOCATED(coef_soil_runoff))ALLOCATE(coef_soil_runoff(3,num_cell),source=0._dp) !<Include all the 3 layers
    IF(.NOT.ALLOCATED(coef_delay_perc)) ALLOCATE(coef_delay_perc(0:max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(ini_soilwater))   ALLOCATE(ini_soilwater(0:max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(soilmem))         ALLOCATE(soilmem(max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(suro_sub)) ALLOCATE(suro_sub(num_sub,sim_length),source=0._dp) 
    IF(.NOT.ALLOCATED(soro_sub)) ALLOCATE(soro_sub(num_sub,sim_length),source=0._dp) 
    IF(.NOT.ALLOCATED(gwro_sub)) ALLOCATE(gwro_sub(num_sub,sim_length),source=0._dp)     
    IF(.NOT.ALLOCATED(sub2riv)) ALLOCATE(sub2riv(num_sub))
    DO isub = 1, num_sub
      IF(.NOT.ALLOCATED(sub2riv(isub)%suflow)) ALLOCATE(sub2riv(isub)%suflow(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%soflow)) ALLOCATE(sub2riv(isub)%soflow(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%gwflow)) ALLOCATE(sub2riv(isub)%gwflow(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%flow))   ALLOCATE(sub2riv(isub)%flow(sim_length),source=0._dp)
      !----erosion
      IF(.NOT.ALLOCATED(sub2riv(isub)%erosion))ALLOCATE(sub2riv(isub)%erosion(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%erosion_c))ALLOCATE(sub2riv(isub)%erosion_c(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%erosion_l))ALLOCATE(sub2riv(isub)%erosion_l(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%erosion_s))ALLOCATE(sub2riv(isub)%erosion_s(sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(sub2riv(isub)%sed))ALLOCATE(sub2riv(isub)%sed(4,3,sim_length),source=0._dp)
    END DO

    soilmem = 0.

    DO is = 1, num_soiltype
      DO il = 1, max_soillayer
        IF(il > 1)THEN
          soilmem(il,is) = timesteps_per_day*par_landuse(luid_surfmem,il)*EXP(par_landuse(luid_depthrel,il) &
                            *(soildepth(il-1,is)+(soilthick(il,is) / 2.)))
        ELSE  
          soilmem(il,is) = timesteps_per_day*par_landuse(luid_surfmem,il)*EXP(par_landuse(luid_depthrel,il) &
                            *(soilthick(il,is) / 2.))
        ENDIF
      ENDDO
    ENDDO

    IF(.NOT.ALLOCATED(epotdist)) ALLOCATE(epotdist(2,num_soiltype))  
    DO is=1,num_soiltype
      sums=soilthick(1,is)*EXP(-par_general(genid_sopetdis)*soildepth(1,is)/2.)+ &
           soilthick(2,is)*EXP(-par_general(genid_sopetdis)*(soildepth(1,is)+(soildepth(2,is)-soildepth(1,is))/2.))
      epotdist(1,is) = soilthick(1,is)*EXP(-par_general(genid_sopetdis)* soildepth(1,is)/2.)/sums
    END DO
    epotdist(2,:)=1.-epotdist(1,:)
    !------------------------------------------------------------------------------
    !>Soil water content
    IF(.NOT.ALLOCATED(sw_wp)) ALLOCATE(sw_wp(0:max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(sw_fc)) ALLOCATE(sw_fc(0:max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(sw_ep)) ALLOCATE(sw_ep(0:max_soillayer,num_soiltype))
    IF(.NOT.ALLOCATED(sw_pv)) ALLOCATE(sw_pv(0:max_soillayer,num_soiltype)) 
    CALL soilwater_content(sw_wp,sw_fc,sw_ep,sw_pv)
    ini_soilwater=0._dp
    ini_soilwater=ini_soilwater+sw_fc  !>Initial soilwater = field capacity
    !------------------------------------------------------------------------------
    !>Percolation delay coefficient
    !>SWAT manual, P151
    coef_delay_perc(:,:) = 0._dp
    DO i = 1, num_soiltype
      IF(soilthick(3,i).GT.0.) THEN 
        perc_tt(:,i) = sw_ep(:,i)/ksat(:,i)             
        coef_delay_perc(:,i) = 1.-EXP(-1.*timesteps_per_day*24./perc_tt(:,i))
      ELSEIF(soilthick(2,i).GT.0.) THEN
        perc_tt(1:2,i) = sw_ep(1:2,i)/ksat(1:2,i)         
        coef_delay_perc(1:2,i) = 1.-EXP(-1.*timesteps_per_day*24./perc_tt(1:2,i))
      ELSEIF(soilthick(1,i).GT.0.) THEN 
        perc_tt(1,i) = sw_ep(1,i)/ksat(1,i)          
        coef_delay_perc(1,i) = 1.-EXP(-1.*timesteps_per_day*24./perc_tt(1,i))
      ENDIF
      perc_tt(0,i) = sw_ep(0,i)/ksat(1,i) 
      coef_delay_perc(0,i) = 1.-EXP(-1.*timesteps_per_day*24./perc_tt(0,i))
    END DO
    !------------------------------------------------------------------------------
    !>Soil runoff coefficients
    !>Careful, slope raster unit is percentage, not angle or m/m
    DO i = 1, num_cell
      is=soiltype(i)
      IF(is.EQ.-9999.OR.landuse(i).EQ.-9999.OR.subbasin(i).EQ.-9999) CYCLE
      IF(soildepth(1,is).LE.realzero) CYCLE

      rc0(i) = par_soil(soilid_rrcs1,is)
      rc2(i) = par_soil(soilid_rrcs3,is)
      IF(rc0(i).GT.1.) rc0(i) = 1.
      IF(rc2(i).GT.1.) rc2(i) = 1.
      IF(slope(i).LT.0.) THEN    
        rc0(i)=rc0(i)+rc2(i)*1.  !<Incase slope is a negative
      ELSE
        rc0(i)=rc0(i)+rc2(i)*slope(i)
      END IF
      IF(rc0(i).GT.1.) rc0(i) = 1.

      rc1(i) = par_soil(soilid_rrcs2,is)
      IF(rc1(i).GT.1.) rc1(i) = 1.
      IF(rc1(i).EQ.0) rc1(i) = rc0(i) 
      
      b(i)=LOG(rc0(i)/rc1(i))/((soildepth(3,is)-soilthick(3,is)/2.)-soilthick(1,is)/2.) 
      coef_soil_runoff(1,i) = rc0(i)
      coef_soil_runoff(3,i) = rc1(i)
      coef_soil_runoff(2,i) = rc0(i)*EXP(-b(i)*(soildepth(2,is)- &
                              soilthick(2,is)/2.-soilthick(1,is)/2.))
    END DO
    !------------------------------------------------------------------------------
    IF(.NOT.ALLOCATED(aquifer)) ALLOCATE(aquifer(num_sub))
    DO isub = 1, num_sub
      ! aquifer(isub)%area       = aquiferdata(isub, 2)
      aquifer(isub)%porosity   = aquiferdata(isub, 2)
      aquifer(isub)%basedepth  = aquiferdata(isub, 3)
      aquifer(isub)%passivedep = aquiferdata(isub, 6)   !<m
      aquifer(isub)%inivol     = (aquiferdata(isub, 5)-aquifer(isub)%basedepth)*cell_area
      aquifer(isub)%iniwater   = (aquiferdata(isub, 5)-aquifer(isub)%basedepth)*1000.
    END DO

    CALL log_process('Model variables initialized.')

  END SUBROUTINE initialize_var
!######################################################################################################
  SUBROUTINE initialize_var_riv
    
    USE MODELVAR,   ONLY    :      riverstate,   &
                                   river,        &
                                   riverflux,    &
                                   flow_node,    &
                                   num_riv,      &
                                   num_node,     &
                                   sim_length,   &
                                   dp     

    INTEGER                 ::     i
    IF(.NOT.ALLOCATED(flow_node))  ALLOCATE(flow_node(num_node),source = 0._dp)
    IF(.NOT.ALLOCATED(river))      ALLOCATE(river(num_riv))
    IF(.NOT.ALLOCATED(riverflux))  ALLOCATE(riverflux(num_riv))
    IF(.NOT.ALLOCATED(riverstate)) ALLOCATE(riverstate(num_riv))


    DO i = 1, num_riv
      ALLOCATE(riverflux(i)%flow(sim_length))
      riverflux(i)%flow(:) = 0._dp
      riverflux(i)%VOL(:) = river(i)%vol
      riverstate(i)%Q(:) = 0._dp

    END DO

  END SUBROUTINE initialize_var_riv
!######################################################################################################  
  ! SUBROUTINE initialize_state(filename)
  !   USE NETCDF
  !   USE DATAWRITE,     ONLY      :      log_process
  !   USE DATAWRITE,     ONLY      :      
  !   USE GENERAL,       ONLY      :      check_nc
  !   USE MODELTYPE,     ONLY      :      VarHot 
  !   USE MODELVAR,      ONLY      :      num_cell,   &
  !                                       out_hot,    &
  !                                       simcfg
  !   !>Get the variable value in the netcdf file
  !   CHARACTER(LEN = *), INTENT(IN) :: filename
    
  !   INTEGER ::  ncid, varid(17) !<IF variables number changed?

  !   IF(.NOT.simcfg%hotstart) RETURN 
  !   CALL check_nc(nf90_open(filename, nf90_nowrite, ncid))
  !   CALL check_nc(nf90_inq_varid(ncid, "icept_storage", varid(1)))
  !   CALL check_nc(nf90_inq_varid(ncid, "soil_water", varid(2)))
  !   CALL check_nc(nf90_inq_varid(ncid, "soil_temp", varid(3)))
  !   CALL check_nc(nf90_inq_varid(ncid, "soil_deeptemp", varid(4)))
  !   CALL check_nc(nf90_inq_varid(ncid, "recharge", varid(5)))
  !   CALL check_nc(nf90_inq_varid(ncid, "runoff_gw", varid(6)))
  !   CALL check_nc(nf90_inq_varid(ncid, "aquifer_water", varid(7)))
  !   CALL check_nc(nf90_inq_varid(ncid, "sub_suflow", varid(8)))
  !   CALL check_nc(nf90_inq_varid(ncid, "sub_soflow", varid(9)))
  !   CALL check_nc(nf90_inq_varid(ncid, "sub_gwflow", varid(10)))
  !   CALL check_nc(nf90_inq_varid(ncid, "rivflux_V1", varid(11)))
  !   CALL check_nc(nf90_inq_varid(ncid, "rivstate_Q1", varid(12)))
  !   CALL check_nc(nf90_inq_varid(ncid, "rivstate_Q2", varid(13)))

  !   CALL check_nc(nf90_inq_varid(ncid, "infilt_day_old", varid(14)))
  !   CALL check_nc(nf90_inq_varid(ncid, "infilt_rate_old", varid(15)))
  !   CALL check_nc(nf90_inq_varid(ncid, "infiltration", varid(16)))

  !   CALL check_nc(nf90_inq_varid(ncid, "q_up", varid(17)))

  !   CALL check_nc(nf90_get_var(ncid, varid(1), out_hot%icept_storage%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(2), out_hot%soil_water%value2))
  !   CALL check_nc(nf90_get_var(ncid, varid(3), out_hot%soil_temp%value2))
  !   CALL check_nc(nf90_get_var(ncid, varid(4), out_hot%soil_deeptemp%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(5), out_hot%recharge%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(6), out_hot%runoff_gw%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(7), out_hot%aquifer_water%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(8),  out_hot%sub_suflow%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(9),  out_hot%sub_soflow%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(10), out_hot%sub_gwflow%value1))

  !   CALL check_nc(nf90_get_var(ncid, varid(11), out_hot%rivflux_V1%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(12), out_hot%rivstate_Q1%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(13), out_hot%rivstate_Q2%value1))

  !   CALL check_nc(nf90_get_var(ncid, varid(14), out_hot%infilt_day_old%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(15), out_hot%infilt_rate_old%value1))
  !   CALL check_nc(nf90_get_var(ncid, varid(16), out_hot%infiltration%value1))

  !   CALL check_nc(nf90_get_var(ncid, varid(17), out_hot%q_up%value1))

  !   CALL check_nc(nf90_close(ncid))

  !   CALL log_process('Model states loaded.')

  ! END SUBROUTINE initialize_state
  !######################################################################################################
  SUBROUTINE runoff_time(parsuroroute,      &
                         suro_time,trace)

    USE DATAWRITE, ONLY : log_process
    USE MODELVAR,  ONLY : simcfg,           &
                          subbasin,         &
                          landuse,          &
                          slope,            &
                          num_landuse,      &
                          num_cell,         &
                          nrow_grid,        &
                          ncol_grid,        &
                          cell_area,        &
                          river_file,       &
                          river_file_unit,  &
                          surodir_file,     &
                          surodir_file_unit,&
                          realzero

    REAL(KIND=8),INTENT(IN)  :: parsuroroute(num_landuse)
    REAL(KIND=8),INTENT(OUT) :: suro_time(num_cell)   !<hours
    
    INTEGER :: surodir(nrow_grid,ncol_grid)
    INTEGER :: river(nrow_grid,ncol_grid)
    INTEGER :: subbasin2(nrow_grid,ncol_grid)
    INTEGER :: landuse2(nrow_grid,ncol_grid)
    REAL(KIND=8) :: slope2(nrow_grid,ncol_grid)
    LOGICAL ::IsRoute(nrow_grid,ncol_grid)
    REAL(KIND=8) :: accumulatedTime   !<seconds
    REAL(KIND=8) :: rf_len, rf_len_diag
    REAL(KIND=8) :: vel_0, vel_1, time, time_0, time_1 !<velocity and toc at current and next cell
    
    INTEGER :: c, i, j, currI, currJ, nskip, id_sub
    INTEGER :: num_routed(nrow_grid,ncol_grid)
    REAL(KIND=8) :: rf_time(nrow_grid,ncol_grid)   !<hours
    
    INTEGER :: k,currK,count
    INTEGER,ALLOCATABLE,INTENT(OUT):: trace(:,:)
    
    IF(simcfg%rf_route.EQ.0) RETURN

    IF(.NOT.ALLOCATED(trace)) ALLOCATE(trace(num_cell,80))

    ! trace = 0
    currK = 0
    ! rf_time = -9999.
    rf_len = SQRT(cell_area)*0.5               !< half of length, and also coverted from meter to feet
    rf_len_diag = SQRT(cell_area)*SQRT(2.)*0.5 !<velocity calculated as ft/s, parsuroroute unit is ft/s

    !>Load river raster 
    OPEN(UNIT=river_file_unit,FILE =river_file,STATUS='OLD',ACTION='READ')
    DO nskip = 1, 6
      READ(river_file_unit, *) 
    END DO
    DO i = 1, nrow_grid
      READ(river_file_unit, *) (river(i,j), j=1,ncol_grid)
    END DO
    CLOSE(river_file_unit)

    !>Load runoff direction 
    OPEN(UNIT=surodir_file_unit,FILE =surodir_file, STATUS='OLD',ACTION='READ')
    DO nskip = 1, 6
      READ(surodir_file_unit, *) 
    END DO
    DO i = 1, nrow_grid
      READ(surodir_file_unit, *) (surodir(i,j), j=1,ncol_grid)
    END DO
    CLOSE(surodir_file_unit)

    !>Convert 1d array to 2d, AND determine isroute or not
    DO i = 1, nrow_grid
      DO j = 1, ncol_grid
    
        subbasin2(i,j) = subbasin((i-1)*ncol_grid+j)
        landuse2(i,j)  = landuse((i-1)*ncol_grid+j)
        slope2(i,j)    = slope((i-1)*ncol_grid+j)/100.
    
        IF(ABS(slope2(i,j)-0.).LE.realzero) slope2(i,j)=0.001
        IF(slope2(i,j).GT.0.04) THEN  !<Time of concentration: a paradox in modern hydrology
          slope2(i,j)=0.05247+0.06363*slope2(i,j)-0.182*EXP(-62.38*slope2(i,j))
        END IF
    
        IF(subbasin2(i,j).NE.-9999.AND.landuse2(i,j).NE.-9999.AND.river(i,j).EQ.-9999 &
           .AND.surodir(i,j).NE.-9999.AND.slope2(i,j).GE.0.) THEN 
          IsRoute(i,j) = .TRUE.
        ELSE
          IsRoute(i,j) = .FALSE.
        END IF
      END DO
    END DO

! OPEN(UNIT=1300,FILE ='dat/isroute',STATUS='REPLACE',ACTION='WRITE')
! OPEN(UNIT=1301,FILE ='dat/slope2',STATUS='REPLACE',ACTION='WRITE')
! OPEN(UNIT=1302,FILE ='dat/landuse2',STATUS='REPLACE',ACTION='WRITE')
!     DO i = 1, nrow_grid
!       DO j = 1, ncol_grid
!         IF(IsRoute(i,j) .EQV. .TRUE.) THEN 
!           WRITE(1300,'(A7)', ADVANCE='NO') 'TRUE'
!         ELSE
!           WRITE(1300,'(A7)', ADVANCE='NO') 'FALSE'
!         END IF
!         WRITE(1301,'(F9.3)', ADVANCE='NO') slope2(i,j)
!         WRITE(1302,'(I6)', ADVANCE='NO') landuse2(i,j)
!       END DO
!       WRITE(1300,*)
!       WRITE(1301,*)
!       WRITE(1302,*)
!     END DO
! CLOSE(1300)
! CLOSE(1301)
! CLOSE(1302)

!>Accumulate routing time in each cell
    DO i = 1, nrow_grid
      DO j = 1, ncol_grid
    
        id_sub = subbasin2(i,j)
        accumulatedTime = 0.0
        currI = i
        currJ = j
        num_routed(i,j) = 0
        count = 1
        k = (currI-1)*ncol_grid+currJ
        trace(k,count) = k
        DO WHILE(IsRoute(currI,currJ))

          SELECT CASE(surodir(currI,currJ))
            CASE(1)  !>East
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ)) !<feet/s
            time_0 = rf_len/vel_0
            !------------------------------------
            currJ = currJ+1
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1
            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK
            CASE(2)  !>South east
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len_diag/vel_0
            !------------------------------------
            currI = currI+1
            currJ = currJ+1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1
            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len_diag/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK
            CASE(4)  !>South
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len/vel_0
            !------------------------------------
            currI = currI+1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1

            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK
            CASE(8)  !>Southwest
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len_diag/vel_0
            !------------------------------------
            currI = currI+1
            currJ = currJ-1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1

            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len_diag/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK
            CASE(16) !>West
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len/vel_0
            !------------------------------------
            currJ = currJ-1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1

            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK
            CASE(32) !>Northwest
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len_diag/vel_0
            !------------------------------------
            currI = currI-1
            currJ = currJ-1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1
 
            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len_diag/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK           
            CASE(64) !>North
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len/vel_0
            !------------------------------------
            currI = currI-1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1
 
            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK           
            CASE(128) !>Northeast
            vel_0 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_0 = rf_len_diag/vel_0
            !------------------------------------
            currI = currI-1
            currJ = currJ+1
            !----------
            currK = (currI-1)*ncol_grid+currJ
            count = count + 1
  
            IF(currI.LE.0.OR.currI.GT.nrow_grid.OR.currJ.LE.0.OR.currJ.GT.ncol_grid.OR. &
            .NOT.(IsRoute(currI,currJ)).OR.subbasin2(currI,currJ).NE.id_sub) EXIT 
            !------------------------------------
            vel_1 = parsuroroute(landuse2(currI,currJ))*SQRT(slope2(currI,currJ))
            time_1 = rf_len_diag/vel_1
            accumulatedTime = accumulatedTime + time_0 + time_1
            trace(k,count) = currK          
          END SELECT
          num_routed(i,j) = num_routed(i,j)+1 
        END DO !<DO WHILE END
        rf_time(i,j) = accumulatedTime/3600. !<seconds --> hours
        suro_time((i-1)*ncol_grid+j) = rf_time(i,j)
      END DO
    END DO
    ! do k =57907
    
    ! enddo
! OPEN(UNIT=1314,FILE='dat/surotime.txt',ACTION='WRITE',STATUS='REPLACE')
! OPEN(UNIT=1303,FILE ='dat/num_routed',STATUS='REPLACE',ACTION='WRITE')
!     DO i = 1, nrow_grid
!       DO j = 1, ncol_grid
!         WRITE(1314, '(F9.2)', ADVANCE='NO') rf_time(i,j) 
!         WRITE(1303, '(I4)', ADVANCE='NO') num_routed(i,j) 
!       END DO
!       WRITE(1303,*)
!       WRITE(1314,*)
!     END DO
! CLOSE(1303)
! CLOSE(1314)
    ! DO K =1,num_cell
    ! write(119,*)trace(32376,:)
    ! END DO 
  END SUBROUTINE runoff_time
!######################################################################################################

!######################################################################################################
  SUBROUTINE soilwater_content(wp, fc, ep, pv)
  
    USE MODELVAR, ONLY :    max_soillayer,    &
                            soilthick,        &
                            num_soiltype,     &
                            bd_soil,          &
                            soil_density
    USE MODELPAR                 
    REAL(KIND=8),INTENT(OUT) :: wp(0:max_soillayer, num_soiltype)   
    REAL(KIND=8),INTENT(OUT) :: fc(0:max_soillayer, num_soiltype)   
    REAL(KIND=8),INTENT(OUT) :: ep(0:max_soillayer, num_soiltype)   
    REAL(KIND=8),INTENT(OUT) :: pv(0:max_soillayer, num_soiltype)   
    INTEGER :: n, sl
    REAL(KIND=8) soilthick1000(0:max_soillayer, num_soiltype)

    soilthick1000(0:max_soillayer,:) = soilthick(0:max_soillayer,:) * 1000.  !< m --> mm

    DO n = 1, num_soiltype 
      wp(0, n) = par_soil(soilid_wp1, n) * soilthick1000(0, n)
      wp(1, n) = par_soil(soilid_wp1, n) * soilthick1000(1, n)
      IF(max_soillayer.EQ.3) THEN 
        wp(2, n) = par_soil(soilid_wp2, n) * soilthick1000(2, n)
        wp(3, n) = par_soil(soilid_wp3, n) * soilthick1000(3, n)
      ELSEIF(max_soillayer.EQ.2) THEN 
        wp(2, n) = par_soil(soilid_wp2, n) * soilthick1000(2, n)
      END IF 

      fc(0, n) = par_soil(soilid_awc1, n) * soilthick1000(0, n)
      fc(1, n) = par_soil(soilid_awc1, n) * soilthick1000(1, n)
      IF(max_soillayer.EQ.3) THEN 
        fc(2, n) = par_soil(soilid_awc2, n) * soilthick1000(2, n)
        fc(3, n) = par_soil(soilid_awc3, n) * soilthick1000(3, n)
      ELSEIF(max_soillayer.EQ.2) THEN
        fc(2, n) = par_soil(soilid_awc2, n) * soilthick1000(2, n)
      END IF
      fc(:, n) = fc(:, n) + wp(:, n) 

      ! ep(1, n) = par_soil(soilid_ep1, n) * soilthick1000(1, n)
      ! IF(max_soillayer.EQ.3) THEN 
      !   ep(2, n) = par_soil(soilid_ep2, n) * soilthick1000(2, n)
      !   ep(3, n) = par_soil(soilid_ep3, n) * soilthick1000(3, n)
      ! ELSEIF(max_soillayer.EQ.2) THEN
      !   ep(2, n) = par_soil(soilid_ep2, n) * soilthick1000(2, n)
      ! END IF

      pv(0, n) = (1.-bd_soil(1, n)/soil_density)* soilthick1000(0, n)
      pv(1, n) = (1.-bd_soil(1, n)/soil_density)* soilthick1000(1, n)
      IF(max_soillayer.EQ.3) THEN 
        pv(2, n) = (1.-bd_soil(2, n)/soil_density)* soilthick1000(2, n)
        pv(3, n) = (1.-bd_soil(3, n)/soil_density)* soilthick1000(3, n)
      ELSEIF(max_soillayer.EQ.2) THEN
        pv(2, n) = (1.-bd_soil(2, n)/soil_density)* soilthick1000(2, n)
      ENDIF

      ep(:,n) = pv(:,n)-fc(:,n)
      
    ENDDO
    ! pv=wp+fc+ep
  END SUBROUTINE soilwater_content

END MODULE INITIALIZE