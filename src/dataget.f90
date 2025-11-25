MODULE DATAGET
  USE DATAWRITE, ONLY : log_process
  IMPLICIT NONE
  PUBLIC load_forcing
CONTAINS
  SUBROUTINE load_forcing()
    USE MODELVAR,       ONLY       :       cfg_file_unit,        &
                                           cfg_file,             &
                                           landuse_file_unit,    &
                                           landuse_file,         &
                                           subbasin_file_unit,   &
                                           subbasin_file,        &
                                           aquiferdata_file_unit,&
                                           aquiferdata_file,     &
                                           aquiferdata,          &
                                           landuse,              &
                                           slope_file_unit,      &
                                           slope_file,           &
                                           slope,                &
                                           soiltype_file_unit,   &
                                           soiltype_file,        &
                                           soiltype,             &
                                           soildata_file_unit,   &
                                           soildata_file,        &
                                           soildepth,            &
                                           soilthick,            &
                                           streamdepth,          &
                                           ksat,                 &
                                           rivdata_file_unit,    &
                                           rivdata_file,         &
                                           river,                &
                                           prec_file_unit,       &
                                           prec_file,            &
                                           prec_gauge,           &
                                           prec_cell,            &
                                           times_sim,            &
                                           date_sim,             &
                                           date_comp,            &
                                           temp_file_unit,       &
                                           temp_file,            &
                                           temp_air,             &
                                           clay_soil,            &
                                           silt_soil,            &
                                           sand_soil,            &
                                           bd_soil,              &
                                           windspeed,            &
                                           humidity,             &
                                           wind_file_unit,       &
                                           wind_file,            &
                                           humidity_file,        &
                                           humidity_file_unit,   &
                                           prec_nc_file,         &
                                           simcfg,               &
                                           upID,                 &
                                           rivID,                &
                                           num_riv,              &
                                           crop_file,            &
                                           crop_file_unit
    USE MODELVAR_SED, ONLY :cropdata, &
                            sedresp,resp_file,resp_unit
                                           
    
    CALL get_cfg(cfg_file_unit, cfg_file, 1)  
    CALL get_aquiferdata(aquiferdata_file_unit, aquiferdata_file, aquiferdata) 
    CALL get_landuse(landuse_file_unit, landuse_file, landuse) 
    CALL get_slope(slope_file_unit, slope_file, slope)
    CALL get_soiltype(soiltype_file_unit, soiltype_file, soiltype) 
    CALL get_soildata(soildata_file_unit, soildata_file, soildepth, soilthick, streamdepth, ksat, &
                      clay_soil, silt_soil, sand_soil, bd_soil)
    CALL get_rivdata(rivdata_file_unit, rivdata_file, river)
    ! IF(simcfg%prec_nc) THEN
    !   CALL get_prec_nc(prec_nc_file, prec_cell)
    ! ELSE
      CALL get_precipitation(prec_file_unit,prec_file,1,prec_gauge,times_sim)
    ! END IF
    CALL get_temperature(temp_file_unit, temp_file, 1, temp_air)
    CALL get_cropdata(crop_file_unit, crop_file, cropdata)
    CALL get_topology("dat/topology.txt", rivID, upID, num_riv, 5)
    CALL get_sedresp(resp_unit,resp_file,sedresp)
  END SUBROUTINE load_forcing
!######################################################################################################
  SUBROUTINE get_cfg(funit,    &
                     fname,    &
                     nskip)  
    USE MODELVAR, ONLY    :  bdate,              &
                             cdate,              &
                             edate,              &
                             steplen,            &
                             num_sub,            &
                             num_riv,            &
                             num_cell,           &
                             timesteps_per_day,  &
                             num_landuse,        &
                             num_soiltype,       &
                             max_soillayer,      &
                             num_ant_type,      &
                             num_raingauge,      &
                             cell_area,          &
                             simcfg,             &
                             id_in,              &
                             id_on,              &
                             id_sp,              &
                             id_pp,              &
                             nday_fert,          &
                             nday_residue,       &
                             landuse_file_unit,  &
                             landuse_file,       &
                             nrow_grid,          &
                             ncol_grid,          &
                             sim_length,         &
                             comp_length,        &
                             warm_length,        &
                             sim_yr,             &
                             comp_yr,            &
                             date_sim,           &
                             date_comp             
    USE DATETIME, ONLY : string_convert_to_DateType
    INTEGER(KIND = 4), INTENT(IN) :: funit
    CHARACTER(LEN = *),INTENT(IN) :: fname
    INTEGER(KIND = 4), INTENT(IN) :: nskip
    INTEGER       :: i, sim, rf_model, rf_route
    CHARACTER(LEN = 30) :: line, bd_str, cd_str, ed_str, step_str
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')   
      DO i = 1, nskip
        READ(funit, *)
      END DO
      READ(funit, *) line, bd_str
      CALL string_convert_to_DateType(bd_str, bdate)
      READ(funit, *) line, cd_str
      CALL string_convert_to_DateType(cd_str, cdate)
      READ(funit, *) line, ed_str
      CALL string_convert_to_DateType(ed_str, edate)
      READ(funit, *) line, step_str
      CALL string_convert_to_DateType(step_str, steplen)
      READ(funit, *) line, num_sub
      READ(funit, *) line, num_riv
      READ(funit, *) line, num_landuse
      READ(funit, *) line, num_soiltype
      READ(funit, *) line, max_soillayer
      READ(funit, *) line, num_ant_type
      READ(funit, *) line, num_raingauge
      READ(funit, *) line, cell_area
      READ(funit, *) line, rf_model
      simcfg%rf_model = rf_model 
      READ(funit, *) line, rf_route
      simcfg%rf_route = rf_route 
      ! READ(funit, *) line, nday_fert
      ! READ(funit, *) line, nday_residue
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%prec_nc = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%sim_sed = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%sim_N = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%sim_P = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%sim_ant = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%log = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%hotstart = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%rch = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%write_txt_rch = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%write_nc_land = .TRUE.
      END IF
      !-------------------------------------
      READ(funit, *) line, sim
      IF(sim .EQ. 1) THEN
        simcfg%write_nc_rch = .TRUE.
      END IF
      !-------------------------------------
    CLOSE(funit)
    CALL get_simulation_length(bdate, edate, sim_length, sim_yr, date_sim)
    CALL get_simulation_length(cdate, edate, comp_length, comp_yr, date_comp)
    warm_length = sim_length - comp_length
    CALL get_grid_size(landuse_file_unit, landuse_file, nrow_grid, ncol_grid, num_cell)
    CALL log_process('Project configured.')
  END SUBROUTINE get_cfg
!######################################################################################################
  SUBROUTINE get_grid_size(funit,   &
                           fname,   &
                           nrow,    &
                           ncol,    &
                           ncell)
    INTEGER(KIND = 4), INTENT(IN) :: funit
    CHARACTER(LEN = *), INTENT(IN) :: fname
    INTEGER(KIND = 4), INTENT(OUT) :: nrow
    INTEGER(KIND = 4), INTENT(OUT) :: ncol
    INTEGER(KIND = 4), INTENT(OUT) :: ncell
    LOGICAL      ::  alive
    CHARACTER    ::  line
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
      WRITE(6, *) "Error: ", TRIM(fname), " doesn't exist. Cannot get grid size."
    END IF
    OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      READ(funit, *) line, ncol
      READ(funit, *) line, nrow
    CLOSE(funit)
    ncell=nrow*ncol
  END SUBROUTINE get_grid_size
!######################################################################################################
  SUBROUTINE get_aquiferdata(funit,     &
                             fname,     &
                             array)
    USE MODELVAR, ONLY  :  num_sub,            &
                           subbasin_file_unit, &
                           subbasin_file,      &
                           subbasin,           &
                           simcfg
    !>IN&OUT
    INTEGER, INTENT(IN)                    ::      funit
    CHARACTER(LEN=*), INTENT(IN)           ::      fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) ::      array(:,:)
    !>Local
    LOGICAL  ::  alive
    INTEGER  ::  iskip, isub
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
      WRITE(*, *) TRIM(fname), " doesn't exist."
      STOP
    END IF
    IF(.NOT. ALLOCATED(array))  ALLOCATE(array(num_sub, 6))
    OPEN(UNIT = funit, FILE = fname, STATUS = "OLD", ACTION = "READ")
      DO iskip = 1, 1
      READ(funit, *)
      END DO
      
      DO isub = 1, num_sub
        READ(funit, *) array(isub, :)
      END DO
    CLOSE(funit)

    CALL get_subbasin(subbasin_file_unit, subbasin_file, subbasin)  
        
    CALL log_process('Aquifer data loaded.')

  END SUBROUTINE get_aquiferdata
!######################################################################################################
  SUBROUTINE get_subbasin(funit,      &
                          fname,      & 
                          array)
    USE MODELVAR, ONLY :    nrow_grid,      &
                          ncol_grid,      &
                          sub_num_cell,   &
                          num_sub,        &
                          num_cell
    INTEGER, INTENT(IN)              ::   funit
    CHARACTER(LEN = *),INTENT(IN)    ::   fname
    INTEGER,ALLOCATABLE,INTENT(OUT)  ::   array(:)
    LOGICAL alive
    INTEGER  i, j, nskip
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(*, *) TRIM(fname), " doesn't exist."
    STOP
    END IF
    IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
    IF(.NOT.ALLOCATED(sub_num_cell)) ALLOCATE(sub_num_cell(num_sub),source=0)
    OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO nskip = 1, 6
        READ(funit, *) 
      END DO
      DO i = 1, nrow_grid
        READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
      END DO
    CLOSE(funit)  
    DO i = 1, num_cell
      IF(array(i).NE.-9999) sub_num_cell(array(i))=sub_num_cell(array(i))+1
    END DO
  END SUBROUTINE get_subbasin
!######################################################################################################
  SUBROUTINE get_landuse(funit,     &
                         fname,     &
                         array)

    USE MODELVAR, ONLY : nrow_grid, ncol_grid, simcfg, num_cell
    INTEGER, INTENT(IN)               :: funit
    CHARACTER(LEN = *), INTENT(IN)    :: fname
    INTEGER, ALLOCATABLE, INTENT(OUT) :: array(:)
    LOGICAL  ::  alive
    INTEGER  ::  i, j, nskip
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
      WRITE(*, *) TRIM(fname), " doesn't exist."
      STOP
    END IF
    IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
    OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO nskip = 1, 6
        READ(funit, *) 
      END DO
      DO i = 1, nrow_grid
        READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
      END DO
    CLOSE(funit)
    WHERE(array.EQ.810400.OR.array.EQ.810501.OR.array.EQ.810502)                  
      array = 1 !<FOREST
      ELSEWHERE(array.EQ.810503 .OR.array.EQ.810504 .OR. array.EQ.810507 .OR. array.EQ.810601)
      array = 1  
      ELSEWHERE(array.EQ.810302 .OR. array.EQ.810303 .OR. array.EQ.810602 .OR. array.EQ.820200)    
      array = 2 !<DRYLAND
      ELSEWHERE(array.EQ.310200 .OR. array.EQ.310300 .OR. array.EQ.311200)    
      array = 3 !<URBAN
      ELSEWHERE(array.EQ.810301 .OR. array.EQ.810304)    
      array = 4 !<PADDYFIELD
      ELSEWHERE(array.EQ.210101 .OR. array.EQ.220201 .OR. array.EQ.220202 )
      array = 5 !<WATER
      ELSEWHERE(array.EQ.230101  .OR. array.EQ.230102  .OR. array.EQ.240101)             
      array = 5 
      ELSEWHERE(array.EQ.-9999)
      array = 3 !<Nodata changed to urban  
    END WHERE

    CALL log_process('Landuse type loaded.')
  END SUBROUTINE get_landuse
!#######################################################################################################
  SUBROUTINE get_slope(funit,     &
                       fname,     &
                       array) 
    USE MODELVAR,  ONLY   :    nrow_grid, ncol_grid, simcfg, num_cell
    INTEGER, INTENT(IN)                     ::   funit
    CHARACTER(LEN=*), INTENT(IN)            ::   fname
    REAL(KIND=8), INTENT(OUT), ALLOCATABLE  ::   array(:)
    !>Local
    LOGICAL :: alive
    INTEGER :: nskip, i, j
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(*, *) TRIM(fname), " doesn't exist."
    STOP
    END IF
    IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
    OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO nskip = 1, 6
      READ(funit, *) 
    END DO
    DO i = 1, nrow_grid
      READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
    END DO
    CLOSE(funit)
    CALL log_process('Slope data loaded.')

  END SUBROUTINE get_slope 
!#######################################################################################################  
  SUBROUTINE get_soiltype(funit,    &
                          fname,    &
                          array)
    USE MODELVAR, ONLY : nrow_grid, ncol_grid, simcfg, num_cell
    !>IN&OUT
    INTEGER, INTENT(IN)               :: funit
    CHARACTER(LEN = *), INTENT(IN)    :: fname
    INTEGER, ALLOCATABLE, INTENT(OUT) :: array(:)
    !>Local
    LOGICAL  ::  alive
    INTEGER  ::  i, j, nskip
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(*, *) TRIM(fname), " doesn't exist."
    STOP
    END IF
    IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
    OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
    DO nskip = 1, 6
      READ(funit, *) 
    END DO
    DO i = 1, nrow_grid
      READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
    END DO
    CLOSE(funit)
    WHERE(array .EQ. 11367)           
      array = 1
      ELSEWHERE(array .EQ. 11389)    
      array = 2
      ELSEWHERE(array .EQ. 11613)    
      array = 3
      ELSEWHERE(array .EQ. 11627)    
      array = 4
      ELSEWHERE(array .EQ. 11858)    
      array = 5
      ELSEWHERE(array .EQ. 11876)    
      array = 6
      ELSEWHERE(array .EQ. 11927)    
      array = 7
    END WHERE
    CALL log_process('Soil types loaded.')
  END SUBROUTINE get_soiltype
!######################################################################################################
  SUBROUTINE get_soildata(funit,       &
                          fname,       & 
                          sol_depth,   & 
                          sol_thick,   & 
                          str_depth,   &
                          k_sat,       &
                          clay_p,      & 
                          silt_p,      & 
                          sand_p,      & 
                          bd)
    USE MODELVAR, ONLY     :    num_soiltype,       &
                                max_soillayer,      &
                                simcfg,             &
                                dp
    INTEGER, INTENT(IN)                      ::         funit
    CHARACTER(LEN = *), INTENT(IN)           ::         fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         sol_depth(:, :)   
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         sol_thick(:, :)
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         str_depth(:)  
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         k_sat(:, :)       
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         clay_p(:, :)         
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         silt_p(:, :)      
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         sand_p(:, :)      
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   ::         bd(:, :)          
    INTEGER       ::   i, soil
    CHARACTER     ::   line
    INTEGER       ::   layer_max(num_soiltype)
    REAL(KIND=8)  ::   dep1(num_soiltype), dep2(num_soiltype), dep3(num_soiltype)                           
    ALLOCATE(sol_depth (3, num_soiltype), source = 0._dp)
    ALLOCATE(sol_thick (0:3, num_soiltype), source = 0._dp)
    ALLOCATE(str_depth (num_soiltype))
    ALLOCATE(k_sat     (3, num_soiltype), source = 0._dp)
    ALLOCATE(clay_p    (3, num_soiltype), source = 0._dp)
    ALLOCATE(silt_p    (3, num_soiltype), source = 0._dp)
    ALLOCATE(sand_p    (3, num_soiltype), source = 0._dp)
    ALLOCATE(bd        (3, num_soiltype), source = 0._dp)

    OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
      DO i = 1, 1        !<First line is comment
        READ(funit, *)
      END DO
      READ(funit, *) line, layer_max (:)
      READ(funit, *) line, dep1      (:)
      READ(funit, *) line, dep2      (:)
      READ(funit, *) line, dep3      (:)
      READ(funit, *) line, str_depth (:)
      READ(funit, *) line, k_sat     (1, :)
      READ(funit, *) line, k_sat     (2, :)
      READ(funit, *) line, k_sat     (3, :)
      READ(funit, *) line, clay_p    (1, :)
      READ(funit, *) line, clay_p    (2, :)
      READ(funit, *) line, clay_p    (3, :)
      READ(funit, *) line, silt_p    (1, :)
      READ(funit, *) line, silt_p    (2, :)
      READ(funit, *) line, silt_p    (3, :)
      READ(funit, *) line, sand_p    (1, :)
      READ(funit, *) line, sand_p    (2, :) 
      READ(funit, *) line, sand_p    (3, :) 
      READ(funit, *) line, bd        (1, :)
      READ(funit, *) line, bd        (2, :) 
      READ(funit, *) line, bd        (3, :) 
    CLOSE(funit)

    DO soil = 1, num_soiltype
      IF(layer_max(soil) .EQ. 1) THEN 
        sol_depth(1, soil) = dep1(soil)
        sol_thick(1, soil) = dep1(soil)
      ELSEIF(layer_max(soil) .EQ. 2) THEN 
        sol_depth(1, soil) = dep1(soil)
        sol_thick(1, soil) = dep1(soil)
        sol_depth(2, soil) = dep2(soil)
        sol_thick(2, soil) = dep2(soil) - dep1(soil)  
      ELSEIF(layer_max(soil).EQ.3) THEN 
        sol_depth(1, soil) = dep1(soil)
        sol_thick(1, soil) = dep1(soil)
        sol_depth(2, soil) = dep2(soil)
        sol_thick(2, soil) = dep2(soil) - dep1(soil)
        sol_depth(3, soil) = dep3(soil)
        sol_thick(3, soil) = dep3(soil) - dep2(soil)
      END IF
      IF(sol_thick(1,soil).GT.0.) THEN 
        sol_thick(0,soil) = 0.01
      ELSE
        sol_thick(0,soil) = 0.
      END IF
    END DO
    

    CALL log_process('Soil data loaded.')
  END SUBROUTINE get_soildata
!######################################################################################################
  SUBROUTINE get_rivdata(funit,     &
                         fname,     &
                         riv)
    USE MODELTYPE
    USE MODELVAR,      ONLY     :        num_sub,            &
                                         num_riv,            &
                                         num_node,           &
                                         num_plevel,         &
                                         rivdata_file,       &
                                         rivdata_file_unit,  &
                                         simcfg
    INTEGER, INTENT(IN)                               ::           funit
    CHARACTER(LEN = *), INTENT(IN)                    ::           fname
    TYPE(RIVERTYPE),ALLOCATABLE,INTENT(OUT)           ::           riv(:)   
    INTEGER    ::    iskip, iriv, rivID
    LOGICAL    ::    alive

    num_plevel = 0

    IF(.NOT.ALLOCATED(riv)) ALLOCATE(riv(num_riv))

    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(*, *) TRIM(fname), " doesn't exist."
    STOP
    END IF 
    OPEN(UNIT=funit, FILE=fname, STATUS="OLD", ACTION="READ")
    DO iskip = 1, 1
    READ(funit, *)
    END DO
    DO iriv = 1, num_riv
      READ(funit, *) rivID, riv(iriv)%nodeB, riv(iriv)%nodeE, riv(iriv)%length, riv(iriv)%width,  &
      riv(iriv)%slope, riv(iriv)%rough, riv(iriv)%inidepth, riv(iriv)%downID, riv(iriv)%gaugeID,  &
      riv(iriv)%plevel,riv(iriv)%maxdepth,riv(iriv)%mindepth
      num_node = MAX(riv(iriv)%nodeB, riv(iriv)%nodeE, num_node)
      num_plevel = MAX(num_plevel, riv(iriv)%plevel)
      riv(iriv)%vol=riv(iriv)%length*riv(iriv)%width*riv(iriv)%inidepth
      riv(iriv)%maxvol=riv(iriv)%length*riv(iriv)%width*riv(iriv)%maxdepth
      riv(iriv)%minvol=riv(iriv)%length*riv(iriv)%width*riv(iriv)%mindepth
    END DO
    CLOSE(funit)
    CALL log_process('River data loaded.')
  END SUBROUTINE get_rivdata
!######################################################################################################
  ! SUBROUTINE get_prec_nc(fname,      &     
  !                        prec_c      &  
  !                        )
  !   USE MODELVAR, ONLY   :    num_cell,     &
  !                             ncol_grid,    &
  !                             nrow_grid,    &
  !                             sim_length
  !   USE GENERAL,  ONLY   :    check_nc
  !   USE NETCDF
    
  !   CHARACTER(LEN=*), INTENT(IN)  ::  fname
  !   REAL(KIND=8), ALLOCATABLE, INTENT(OUT)     ::  prec_c(:,:)
  !   INTEGER  ::  ncid, varid_prec
  !   INTEGER  ::  i, j, k, nt
  !   LOGICAL  ::  alive
  !   REAL(KIND=8) :: prec_3d(ncol_grid, nrow_grid, sim_length)
  !   INTEGER  ::  ndims,nvars, natts, ierr, dim_lon, dim_lat, dim_time

  !   INQUIRE(FILE = fname, EXIST = alive)
  !   IF(.NOT. alive) THEN 
  !     WRITE(6, *) "Error: ", TRIM(fname), " doesn't exist."
  !   END IF

  !   CALL check_nc(nf90_open(fname, nf90_nowrite, ncid))
  !   CALL check_nc(nf90_inq_varid(ncid, "precipitation", varid_prec))
  !   CALL check_nc(nf90_inq_dimid(ncid, "lon", dim_lon))
  !   CALL check_nc(nf90_inq_dimid(ncid, "lat", dim_lat))
  !   CALL check_nc(nf90_inq_dimid(ncid, "time", dim_time))

  !   CALL check_nc(nf90_get_var(ncid, varid_prec, prec_3d))

  !   CALL check_nc(nf90_close(ncid))

  !   IF(.NOT.ALLOCATED(prec_c)) ALLOCATE(prec_c(num_cell, sim_length))
  !   DO i = 1, ncol_grid
  !     DO j = 1, nrow_grid
  !       DO k = 1, sim_length
  !         prec_c((j-1)*ncol_grid+i, k) = prec_3d(i, j, k)
  !       END DO
  !     END DO
  !   END DO
  !   CALL log_process('Precipitation nc4 data loaded.')

  ! END SUBROUTINE get_prec_nc
!######################################################################################################
  SUBROUTINE get_precipitation(funit,     &   
                               fname,     & 
                               nskip,     & 
                               array,     & 
                               tx         &
                               )
    !>Read precipitation txt file.
    USE MODELVAR, ONLY     :  timeformat,            &
                              sim_length,            &
                              bdate,                 &
                              edate,                 &
                              simcfg,                &
                              num_raingauge
    USE MODELTYPE, ONLY    :  DateType
    USE DATETIME
    
    !>IN&OUT
    INTEGER, INTENT(IN)                      :: funit
    CHARACTER(LEN = *), INTENT(IN)           :: fname
    INTEGER, INTENT(IN)                      :: nskip  
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   :: array(:,:)
    TYPE(DateType), ALLOCATABLE, INTENT(OUT) :: tx(:)    !<Date period of simulation, start from bdate
    !>Local variables
    INTEGER             :: i, ir
    REAL(KIND=8)        :: y(num_raingauge)
    CHARACTER(LEN = 16) :: line, line2
    LOGICAL             :: alive
    TYPE(DateType)      :: d
    
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(6, *) "Error: ", TRIM(fname), " doesn't exist."
    END IF
    
    ALLOCATE(tx(sim_length))
    ALLOCATE(array(sim_length,num_raingauge))
    
    ir = 0
    array = 0.
    
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
    DO i = 1, nskip
    READ(funit, *) 
    END DO
    
    READMATRIX: DO
      IF(timeformat == 0) THEN
        READ(funit, *, END = 200) line, y(:)
        CALL string_convert_to_DateType(line, d)
      ELSEIF(timeformat == 1) THEN
        READ(funit, *, END = 200) line, line2, y(:)    
        line(12 : 16) = line2(1 : 5)
        CALL string_convert_to_DateType(line, d)
      ENDIF
      
      IF(SmallerDates(d, bdate)) THEN
        ir = 0
      ELSEIF(LargerEqualDates(d, bdate) .AND. SmallerEqualDates(d, edate)) THEN
        ir = ir + 1             
        tx(ir) = d            
        array(ir,:) = y(:)   
      ELSEIF(LargerDates(d, edate)) THEN
        EXIT READMATRIX
      END IF
    END DO READMATRIX
    
    CLOSE(funit)
    
    200 CALL log_process('Rainfall data loaded.')

  END SUBROUTINE get_precipitation
!######################################################################################################
  ! SUBROUTINE get_temperature(funit,     &
  !                            fname,     &
  !                            nskip,     &
  !                            array)
  !   USE MODELVAR, ONLY : timeformat, sim_length, bdate, edate, simcfg
  !   USE DATETIME
  !   USE MODELTYPE,  ONLY   :    DateType
  !   INTEGER, INTENT(IN)                    :: funit
  !   CHARACTER(LEN = *), INTENT(IN)         :: fname
  !   INTEGER, INTENT(IN)                    :: nskip  
  !   REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: array(:,:)
  !   INTEGER                     :: i, ir
  !   REAL(KIND=8)                :: temp_ave, temp_min, temp_max
  !   CHARACTER(LEN = 16)         :: line, line2
  !   LOGICAL                     :: alive
  !   TYPE(DateType)              :: d
  !   TYPE(DateType), ALLOCATABLE :: tx(:)
  !   INQUIRE(FILE = fname, EXIST = alive)
  !   IF(.NOT. alive) THEN 
  !   WRITE(100, *) "Error: ", TRIM(fname), " doesn't exist."
  !   END IF
  !   ALLOCATE(tx(sim_length))
  !   ALLOCATE(array(sim_length, 3))    
  !   ir = 0
  !   array = 0.
  !   OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
  !     DO i = 1, nskip
  !     READ(funit, *) 
  !     END DO
  !     READMATRIX: DO
  !       IF(timeformat == 0) THEN
  !       READ(funit, *, END = 200) line, temp_ave, temp_min, temp_max
  !       CALL string_convert_to_DateType(line, d)
  !       ELSEIF(timeformat == 1) THEN
  !       READ(funit, *, END = 200) line, line2, temp_ave, temp_min, temp_max  
  !       line(12 : 16) = line2(1 : 5)
  !       CALL string_convert_to_DateType(line, d)
  !       ENDIF
  !       IF(SmallerDates(d, bdate)) THEN
  !         ir = 0
  !       ELSEIF(LargerEqualDates(d, bdate) .AND. SmallerEqualDates(d, edate)) THEN
  !         ir = ir + 1             
  !         tx(ir) = d            
  !         array(ir, 1) = temp_ave
  !         array(ir, 2) = temp_min
  !         array(ir, 3) = temp_max
  !       ELSEIF(LargerDates(d, edate)) THEN
  !         EXIT READMATRIX
  !       END IF
  !     END DO READMATRIX
  !   CLOSE(funit)
  ! 200 CALL log_process('Temp loaded.')
  ! END SUBROUTINE get_temperature

  SUBROUTINE get_temperature(funit,     &   
                             fname,     & 
                             nskip,     & 
                             array      &
                             )
    !>Read temperature txt file, load average daily temperature
    USE MODELVAR, ONLY     :  timeformat,            &
                              sim_length,            &
                              bdate,                 &
                              edate,                 &
                              simcfg,                &
                              num_raingauge
    USE MODELTYPE, ONLY    :  DateType
    USE DATETIME

    INTEGER, INTENT(IN)                      :: funit
    CHARACTER(LEN = *), INTENT(IN)           :: fname
    INTEGER, INTENT(IN)                      :: nskip  
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT)   :: array(:,:)
    INTEGER             :: i, ir
    REAL(KIND=8)        :: y(num_raingauge)
    CHARACTER(LEN=16) :: line, line2
    LOGICAL             :: alive
    TYPE(DateType)      :: d
    
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(6, *) "Error: ", TRIM(fname), " doesn't exist."
    END IF
    
    ALLOCATE(array(sim_length,num_raingauge))
    
    ir = 0
    array = 0.
    
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
    DO i = 1, nskip
      READ(funit, *) 
    END DO
    
    READMATRIX: DO
      IF(timeformat == 0) THEN
        READ(funit, *, END = 200) line, y(:)
        CALL string_convert_to_DateType(line, d)
      ELSEIF(timeformat == 1) THEN
        READ(funit, *, END = 200) line, line2, y(:)    
        line(12 : 16) = line2(1 : 5)
        CALL string_convert_to_DateType(line, d)
      ENDIF
    
      IF(SmallerDates(d, bdate)) THEN
        ir = 0
      ELSEIF(LargerEqualDates(d, bdate) .AND. SmallerEqualDates(d, edate)) THEN
        ir = ir + 1                       
        array(ir,:) = y(:)   
      ELSEIF(LargerDates(d, edate)) THEN
        EXIT READMATRIX
      END IF
    END DO READMATRIX
    
    CLOSE(funit)

200 CALL log_process('Temperature data loaded.')

END SUBROUTINE get_temperature
!######################################################################################################
  SUBROUTINE get_windspeed(funit,     &
                           fname,     &
                           nskip,     &
                           array)
    USE MODELVAR, ONLY : timeformat, sim_length, bdate, edate
    USE DATETIME
    USE MODELTYPE,  ONLY   :    DateType
    INTEGER, INTENT(IN)                    :: funit
    CHARACTER(LEN = *), INTENT(IN)         :: fname
    INTEGER, INTENT(IN)                    :: nskip  
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: array(:)
    INTEGER                     :: i, ir
    REAL(KIND=8)                :: windspeed
    CHARACTER(LEN = 16)         :: line, line2
    LOGICAL                     :: alive
    TYPE(DateType)              :: d
    TYPE(DateType), ALLOCATABLE :: tx(:)
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(100, *) "Error: ", TRIM(fname), " doesn't exist."
    END IF
    ALLOCATE(tx(sim_length))
    ALLOCATE(array(sim_length))    
    ir = 0
    array = 0.
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i = 1, nskip
        READ(funit, *) 
      END DO
      READMATRIX: DO
      IF(timeformat == 0) THEN
        READ(funit, *, END = 200) line, windspeed
        CALL string_convert_to_DateType(line, d)
      ELSEIF(timeformat == 1) THEN
        READ(funit, *, END = 200) line, line2, windspeed  
        line(12 : 16) = line2(1 : 5)
        CALL string_convert_to_DateType(line, d)
      ENDIF
      IF(SmallerDates(d, bdate)) THEN
        ir = 0
      ELSEIF(LargerEqualDates(d, bdate) .AND. SmallerEqualDates(d, edate)) THEN
        ir = ir + 1             
        tx(ir) = d            
        array(ir) = windspeed
      ELSEIF(LargerDates(d, edate)) THEN
        EXIT READMATRIX
      END IF 
      END DO READMATRIX
    CLOSE(funit)
    200 CALL log_process('Windspeed loaded')
  END SUBROUTINE get_windspeed
!######################################################################################################
  SUBROUTINE get_humidity(funit,     &
                          fname,     &
                          nskip,     &
                          array)
    USE MODELVAR, ONLY : timeformat, sim_length, bdate, edate
    USE DATETIME
    USE MODELTYPE,  ONLY   :    DateType
    INTEGER, INTENT(IN)                    :: funit
    CHARACTER(LEN = *), INTENT(IN)         :: fname
    INTEGER, INTENT(IN)                    :: nskip  
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: array(:)
    !>Local variables
    INTEGER                     :: i, ir
    REAL(KIND=8)                :: rela_humidity
    CHARACTER(LEN = 16)         :: line, line2
    LOGICAL                     :: alive
    TYPE(DateType)              :: d
    TYPE(DateType), ALLOCATABLE :: tx(:)
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
    WRITE(100, *) "Error: ", TRIM(fname), " doesn't exist."
    END IF
    ALLOCATE(tx(sim_length))
    ALLOCATE(array(sim_length))  
    ir = 0
    array = 0.
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i = 1, nskip
        READ(funit, *) 
      END DO
      READMATRIX: DO
      IF(timeformat == 0) THEN
        READ(funit, *, END = 200) line, rela_humidity
        CALL string_convert_to_DateType(line, d)
      ELSEIF(timeformat == 1) THEN
        READ(funit, *, END = 200) line, line2, rela_humidity 
        line(12 : 16) = line2(1 : 5)
        CALL string_convert_to_DateType(line, d)
      ENDIF
      IF(SmallerDates(d, bdate)) THEN
        ir = 0
      ELSEIF(LargerEqualDates(d, bdate) .AND. SmallerEqualDates(d, edate)) THEN
        ir = ir + 1             
        tx(ir) = d            
        array(ir) = rela_humidity
      ELSEIF(LargerDates(d, edate)) THEN
        EXIT READMATRIX
      END IF   
      END DO READMATRIX
    CLOSE(funit)
    200 CALL log_process('get_humidity')
  END SUBROUTINE get_humidity
!######################################################################################################
  SUBROUTINE get_simulation_length(start_date,  &
                                   end_date,    &
                                   sim_len,     &
                                   sim_year,    &
                                   date_char    &
                                   )

    USE MODELTYPE, ONLY    :   DateType
    USE DATETIME,  ONLY    :   period_length
    
    !>IN&OUT
    Type(DateType), INTENT(IN)                   ::   start_date
    Type(DateType), INTENT(IN)                   ::   end_date
    INTEGER,        INTENT(OUT)                  ::   sim_len   
    INTEGER,        INTENT(OUT)                  ::   sim_year
    CHARACTER(LEN=*), ALLOCATABLE, INTENT(OUT)  ::   date_char(:)
    
    INTEGER                      ::   i, year, month, day
    INTEGER, DIMENSION(12)       ::   month_days

    sim_len  = period_length(start_date, end_date)+1
    sim_year = end_date%Year - start_date%Year + 1
    ALLOCATE(date_char(sim_len))

    !! Create date characters, YYYY-MM-DD
    month_days = (/31,28,31,30,31,30,31,31,30,31,30,31/)

    !! Initialize date
    year  = start_date%Year
    month = start_date%Month
    day   = start_date%Day

    DO i = 1, sim_len
      WRITE(date_char(i), '(I4.4,"-",I2.2,"-",I2.2)') year, month, day
      day = day + 1
      !! Wether a leaf year
      IF ((MOD(year, 4) == 0 .AND. MOD(year, 100) /= 0) .OR. MOD(year, 400) == 0) THEN
        month_days(2) = 29
      ELSE
        month_days(2) = 28
      END IF

      !! Forward to next month
      IF (day > month_days(month)) THEN
        day = 1
        month = month + 1
        IF (month > 12) THEN
           month = 1
           year = year + 1
        END IF
      END IF
    END DO

  END SUBROUTINE get_simulation_length
!######################################################################################################
  
!######################################################################################################
  SUBROUTINE get_args
    !>Get command arguments
    USE MODELVAR,    ONLY    :     num_threads
    USE DATAWRITE,   ONLY    :     log_process
    INTEGER num_args, n
    CHARACTER(LEN=10) arg
    
    num_args = command_argument_count()
    IF(num_args.EQ.0) THEN
      CALL log_process('SHUI model')
      CALL log_process('How to execute.')
      STOP
    ELSE
      DO n = 1, num_args
        CALL get_command_argument(n, arg)
        IF(arg.EQ.'-n') THEN 
          IF(n.LT.num_args) THEN
            CALL get_command_argument(n+1, arg)
            READ(arg, '(I5)') num_threads
          ELSE
            num_threads = 1
          END IF
        END IF
      END DO
    END IF
  END SUBROUTINE 
!######################################################################################################
!######################################################################################################
  SUBROUTINE get_cropdata(funit, fname, crop)
    !>Get number of days infuenced by fertilization and residure
    !>Get data of individual crops
      USE MODELTYPE_SED,  ONLY   :   CROPDATATYPE 
      USE MODELVAR,   ONLY   :   simcfg,        &
                                 nday_residue,  &
                                 nday_fert,     &
                                 num_crop,      &
                                 crop_luid
      !>IN&OUT
      INTEGER, INTENT(IN)                            ::         funit
      CHARACTER(LEN = *), INTENT(IN)                 ::         fname
      TYPE(CROPDATATYPE), ALLOCATABLE, INTENT(OUT)   ::         crop(:)
      !>Local variables
      INTEGER i, icrop, is_ration, rotation1, rotation2
      CHARACTER line
      
      OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
      !>Skip comments
      DO i = 1, 3
        READ(funit,*)
      END DO
  
      READ(funit, *) line, num_crop
      READ(funit, *) line, nday_fert
      READ(funit, *) line, nday_residue
  
      IF(.NOT.ALLOCATED(crop_luid)) ALLOCATE(crop_luid(num_crop))
  
      !>Skip comments
      DO i = 1, 3
        READ(funit,*)
      END DO
  
      !>Rotate
      IF(.NOT.ALLOCATED(crop)) ALLOCATE(crop(num_crop)) 
        DO icrop = 1, num_crop
          READ(funit, *) line, crop(icrop)%luid,               &
                               crop(icrop)%part,               &
                               crop(icrop)%fertnamount1,       &
                               crop(icrop)%fertpamount1,       &
                               crop(icrop)%fertday1,           &
                               crop(icrop)%fertdown1,          &
                               crop(icrop)%mannamount1,        &
                               crop(icrop)%manpamount1,        &
                               crop(icrop)%manday1,            &
                               crop(icrop)%mandown1,           &
                               crop(icrop)%fertnamount2,       &
                               crop(icrop)%fertpamount2,       &
                               crop(icrop)%fertday2,           &
                               crop(icrop)%fertdown2,          &
                               crop(icrop)%mannamount2,        &
                               crop(icrop)%manpamount2,        &
                               crop(icrop)%manday2,            &
                               crop(icrop)%mandown2,           &
                               crop(icrop)%resnamount,         &
                               crop(icrop)%respamount,         &
                               crop(icrop)%resdayno,           &
                               crop(icrop)%resdown,            &
                               crop(icrop)%resfast,            &
                               crop(icrop)%uptake1,            &
                               crop(icrop)%uptake2,            &
                               crop(icrop)%uptake3,            &
                               crop(icrop)%baredayno1,         &
                               crop(icrop)%baredayno2,         &
                               crop(icrop)%baredayno3,         &
                               crop(icrop)%baredayno4,         &
                               crop(icrop)%baredayno5,         &
                               crop(icrop)%ccmax1,             &
                               crop(icrop)%ccmax2,             &
                               crop(icrop)%gcmax1,             &
                               crop(icrop)%gcmax2,             &
                               crop(icrop)%PNuptakeRatio,      &
                               crop(icrop)%uptakeupper,        &
                               crop(icrop)%phmax1,             &
                               crop(icrop)%phmax2
          crop_luid(icrop) = crop(icrop)%luid
          crop(icrop)%fertnamount1 = crop(icrop)%fertnamount1 * 100.  !>kg/ha -> kg/km2
          crop(icrop)%fertpamount1 = crop(icrop)%fertpamount1 * 100.
          crop(icrop)%mannamount1  = crop(icrop)%mannamount1  * 100.
          crop(icrop)%manpamount1  = crop(icrop)%manpamount1  * 100.
          crop(icrop)%fertnamount2 = crop(icrop)%fertnamount2 * 100.
          crop(icrop)%fertpamount2 = crop(icrop)%fertpamount2 * 100.
          crop(icrop)%mannamount2  = crop(icrop)%mannamount2  * 100.
          crop(icrop)%manpamount2  = crop(icrop)%manpamount2  * 100.
          crop(icrop)%resnamount   = crop(icrop)%resnamount   * 100.
          crop(icrop)%respamount   = crop(icrop)%respamount   * 100.
        END DO
        
      CLOSE(funit)              
      
      ! CALL log_process('Crop data loaded.')
  
    END SUBROUTINE get_cropdata
  !######################################################################################################
  !######################################################################################################
    subroutine get_topology(filename, rivID, upID, n, max_upstreams)

      ! Input parameters
      character(len=*), intent(in) :: filename
      integer, intent(in) :: n, max_upstreams
  
      ! Output parameters
      integer, dimension(:), allocatable, intent(out) :: rivID
      integer, dimension(:,:), allocatable, intent(out) :: upID
  
      ! Local variables
      character(len=100) :: line
      integer :: i, num_upstreams, ios, unit_number
  
      ! Allocate arrays
      allocate(rivID(n))
      allocate(upID(max_upstreams, n))
      upID = 0  ! Initialize with a dummy value
  
      ! Open file
      unit_number = 1314  ! An arbitrary unit number
      open(unit=unit_number, file=filename, status='old', iostat=ios)  
  
      ! Check for open errors
      if (ios /= 0) then
          print *, 'Error opening file: ', ios
          stop
      end if
  
      read(unit_number,*)
  
      ! Read each line
      do i = 1, n
          read(unit_number, '(A)', iostat=ios) line
          if (ios /= 0) exit  ! Exit loop if read fails
  ! WRITE(6,*) i
          ! Process the line to extract rivID and upID
          call process_line(line, rivID(i), upID(:,i), num_upstreams)
      end do
  
      ! Close file
      close(unit_number)
    contains
      subroutine process_line(line, riv_id, upstream_ids, num_upstreams)
          character(len=*), intent(in) :: line
          integer, intent(out) :: riv_id
          integer, dimension(:), intent(out) :: upstream_ids
          integer, intent(out) :: num_upstreams
          integer :: pos, end_pos, len_line
  
          len_line = len_trim(line)
  
          pos = index(line, ',')  ! Find the tab character
  ! WRITE(6,*) pos
  ! WRITE(6,*) line
          read(line(1:pos-1), *) riv_id  ! Read rivID
  
          ! Initialize
          num_upstreams = 0
          end_pos = pos + 1
  
          ! Process upIDs
          do
              pos = index(line(end_pos:len_line), ',')
              if (pos == 0) then
                  ! Last ID or only one ID
                  pos = len_line - end_pos + 2
                  read(line(end_pos:end_pos+pos-2), *) upstream_ids(num_upstreams+1)
                  num_upstreams = num_upstreams + 1
                  exit
              else
                  ! Read the next upID
                  read(line(end_pos:end_pos+pos-2), *) upstream_ids(num_upstreams+1)
                  num_upstreams = num_upstreams + 1
                  end_pos = end_pos + pos
              end if
          end do
      end subroutine process_line
  
    end subroutine get_topology
  !######################################################################################################

  !######################################################################################################
    SUBROUTINE get_sedresp(funit, fname, resp)

        USE MODELTYPE_SED,  ONLY   :   SEDRESPTYPE 
        USE MODELVAR,   ONLY   :   simcfg,        &
                                   num_riv
        !>IN&OUT
        INTEGER, INTENT(IN)                            ::         funit
        CHARACTER(LEN = *), INTENT(IN)                 ::         fname
        TYPE(SEDRESPTYPE), ALLOCATABLE, INTENT(OUT)   ::         resp(:)
        !>Local variables
        INTEGER i, iriv
        CHARACTER line
        
        OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
        !>Skip comments
        DO i = 1, 1
          READ(funit,*)
        END DO

    
        !>Rotate
        IF(.NOT.ALLOCATED(resp)) ALLOCATE(resp(num_riv)) 
          DO iriv = 1, num_riv
            READ(funit, *) line, resp(iriv)%claycd,               &
                                 resp(iriv)%claycs,               &
                                 resp(iriv)%mclay,                &
                                 resp(iriv)%siltcd,               &
                                 resp(iriv)%siltcs,               &
                                 resp(iriv)%msilt,                &
                                 resp(iriv)%ks       


          END DO
          
        CLOSE(funit)              
        
       
    
      END SUBROUTINE get_sedresp
  !######################################################################################################

END MODULE DATAGET