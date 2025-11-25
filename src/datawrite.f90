MODULE DATAWRITE
  IMPLICIT NONE
CONTAINS
  SUBROUTINE prepare_print
    USE MODELVAR, ONLY :    simcfg,               &
                            num_sub,              &
                            logfilename,          &
                            riv_out_unit,         &
                            riv_out_file

    INTEGER :: basinid(num_sub), i
    basinid(:) = (/(INT(i), i=1,num_sub)/)
    IF(simcfg%write_txt_rch) THEN 
      OPEN(UNIT=riv_out_unit,FILE=riv_out_file,STATUS='REPLACE',ACTION='WRITE')
      ! WRITE(riv_out_unit, '(A11,85I4)') 'Date', basinid(:)
    END IF
  END SUBROUTINE prepare_print
!######################################################################################################
  SUBROUTINE finish_print
    USE MODELVAR, ONLY   :    simcfg,               &
                              riv_out_unit
    IF(simcfg%write_txt_rch) CLOSE(riv_out_unit)
  END SUBROUTINE finish_print
!######################################################################################################
  SUBROUTINE log_process(process)
  !>Logging process in screen and .log file
    USE MODELVAR,             ONLY  :     simcfg,       &
                                          log_unit
    CHARACTER(LEN=*), INTENT(IN)    ::    process
    CHARACTER(LEN = 40)             ::    string
    IF(process .EQ. 'SHUI model') THEN
      WRITE(6, '(A70)') '----------------------------------------------------------------------'
      WRITE(6, '(A70)') '  Simulator for catchment Hydrology and nUtrient Interactions (SHUI)  '
      WRITE(6, '(A70)') '        version 1.1, 2023, by H.XIE, in NIGLAS, CAS, Nanjing          '  
      WRITE(6, '(A70)') '----------------------------------------------------------------------'                                                                                                                                                                             
      WRITE(6, '(A70)') '       SSSSSSSSSSSS   HHHHHH  HHHHHH  UUUUU    UUUUU  IIIIIIIII       '
      WRITE(6, '(A70)') '      S::::SSSSSS:S    H::H    H::H   U::U     U::U   I:::::::I       '
      WRITE(6, '(A70)') '      S:::S     SSS    H::H    H::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '      S:::S            H::H    H::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '      S::::S           H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '         S::::S        H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '          S:::::S      H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '             S::::S    H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '              S:::S    H::H    H::H   U::U     U::U     I:::I         '
      WRITE(6, '(A70)') '      SSS     S:::S    HH:H    H::H   U:::UUUUU:::U     I:::I         '
      WRITE(6, '(A70)') '      S:SSSSSS::::S    H::H    H::H   U:::::::::::U   I:::::::I       '
      WRITE(6, '(A70)') '      SSSSSSSSSSSS    HHHHHH  HHHHHH   UUUUUUUUUUU    IIIIIIIII       '
      WRITE(6, '(A70)') '----------------------------------------------------------------------'
      WRITE(log_unit, '(A70)') '----------------------------------------------------------------------'
      WRITE(log_unit, '(A70)') '  Simulator for catchment Hydrology and nUtrient Interactions (SHUI)  '
      WRITE(log_unit, '(A70)') '        version 1.1, 2023, by H.XIE, in NIGLAS, CAS, Nanjing          '  
      WRITE(log_unit, '(A70)') '----------------------------------------------------------------------'                                                                                                                                                                           
      WRITE(log_unit, '(A70)') '       SSSSSSSSSSSS   HHHHHH  HHHHHH  UUUUU    UUUUU  IIIIIIIII       '
      WRITE(log_unit, '(A70)') '      S::::SSSSSS:S    H::H    H::H   U::U     U::U   I:::::::I       '
      WRITE(log_unit, '(A70)') '      S:::S     SSS    H::H    H::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '      S:::S            H::H    H::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '      S::::S           H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '         S::::S        H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '          S:::::S      H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '             S::::S    H:::HHHH:::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '              S:::S    H::H    H::H   U::U     U::U     I:::I         '
      WRITE(log_unit, '(A70)') '      SSS     S:::S    HH:H    H::H   U:::UUUUU:::U     I:::I         '
      WRITE(log_unit, '(A70)') '      S:SSSSSS::::S    H::H    H::H   U:::::::::::U   I:::::::I       '
      WRITE(log_unit, '(A70)') '      SSSSSSSSSSSS    HHHHHH  HHHHHH   UUUUUUUUUUU    IIIIIIIII       '
      WRITE(log_unit, '(A70)') '----------------------------------------------------------------------'
    ENDIF
    IF(process.EQ.'How to execute.') THEN
      WRITE(6, *) 'Please input:'
      WRITE(6, *) '"./SHUI -n <Number_Threads>"'
      WRITE(6, *) 'Example: ./SHUI -n 10'
      WRITE(6, *) '-n Number of threads set for OpenMP.'
    END IF
    IF(simcfg%log) THEN 
      IF(process .EQ. 'Project configured.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Aquifer data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Landuse type loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Slope data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string     
      ELSEIF(process .EQ. 'Soil data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Soil types loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string  
      ELSEIF(process .EQ. 'River data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Crop data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string 
      ELSEIF(process .EQ. 'Precipitation data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string  
      ELSEIF(process .EQ. 'Temperature data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string 
      ELSEIF(process .EQ. 'Windspeed data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Humidity data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string 
      ELSEIF(process .EQ. 'Parameter values set.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string  
      ELSEIF(process .EQ. 'Model variables initialized.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string  
      ELSEIF(process .EQ. 'Model states loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string 
      ELSEIF(process .EQ. 'Domin loops started...') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Domin loops completed.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Land routing completed.') THEN
          string = ADJUSTL(process)
          WRITE(6, *) string
          WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'River routing started...') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'River routing completed.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Model states loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Model states recorded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      ELSEIF(process .EQ. 'Precipitation nc4 data loaded.') THEN
        string = ADJUSTL(process)
        WRITE(6, *) string
        WRITE(log_unit, *) string
      END IF
    END IF
  END SUBROUTINE log_process
!######################################################################################################
  SUBROUTINE print_rch_flow(filename)
    USE MODELVAR, ONLY     :   num_riv,          &
                               sim_length,       &
                               warm_length,      &
                               comp_length,      &
                               date_comp,        &
                               riverflux

    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, j, ierr, fileunit
    CHARACTER(100)                ::   errmsg 

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      WRITE(fileunit, '(A4)', ADVANCE='NO') 'Date'
        DO i = 1, num_riv
          WRITE(fileunit, '(I10)', ADVANCE='NO') i
        END DO
      WRITE(fileunit,*) 
      DO i = 1, comp_length
        WRITE(fileunit,'(A11)', ADVANCE='NO') date_comp(i)
        DO j = 1, num_riv
          WRITE(fileunit,'(F15.4)',ADVANCE='NO') riverflux(j)%flow(i+warm_length)
        END DO
        WRITE(fileunit,*) 
      END DO
    CLOSE(fileunit)
  END SUBROUTINE print_rch_flow
!######################################################################################################
  !>Print total suspended sediment concentrations (mg/L) of each river
  !>Sum of silt, clay, and sand
  SUBROUTINE print_rch_sedconc(filename)
    USE MODELVAR, ONLY     :   num_riv,          &
                               sim_length,       &
                               warm_length,      &
                               comp_length,      &
                               date_comp
    USE MODELVAR_SED, ONLY :   sedflux                            

    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, j, ierr, fileunit
    CHARACTER(100)                ::   errmsg 

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      WRITE(fileunit, '(A4)', ADVANCE='NO') 'Date'
        DO i = 1, num_riv
          WRITE(fileunit, '(I10)', ADVANCE='NO') i
        END DO
      WRITE(fileunit,*) 
      DO i = 1, comp_length
        WRITE(fileunit,'(A11)', ADVANCE='NO') date_comp(i)
        DO j = 1, num_riv
          WRITE(fileunit,'(F15.4)',ADVANCE='NO') sedflux(j)%spsconc(4,i+warm_length)
        END DO
        WRITE(fileunit,*) 
      END DO
    CLOSE(fileunit)
  END SUBROUTINE print_rch_sedconc
!######################################################################################################
  !>Print total suspended sediment load (kg) of each river
  !>Sum of silt, clay, and sand
  SUBROUTINE print_rch_sedload(filename)
    USE MODELVAR, ONLY     :   num_riv,          &
                               sim_length,       &
                               warm_length,      &
                               comp_length,      &
                               date_comp
    USE MODELVAR_SED, ONLY :   sedflux                            

    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, j, ierr, fileunit
    CHARACTER(100)                ::   errmsg 

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      WRITE(fileunit, '(A4)', ADVANCE='NO') 'Date'
        DO i = 1, num_riv
          WRITE(fileunit, '(I10)', ADVANCE='NO') i
        END DO
      WRITE(fileunit,*) 
      DO i = 1, comp_length
        WRITE(fileunit,'(A11)', ADVANCE='NO') date_comp(i)
        DO j = 1, num_riv
          WRITE(fileunit,'(F15.4)',ADVANCE='NO') sedflux(j)%spsconc(4,i+warm_length)
        END DO
        WRITE(fileunit,*) 
      END DO
    CLOSE(fileunit)
  END SUBROUTINE print_rch_sedload
!###################################################################################################### 
  SUBROUTINE print_indiv_rch_flow(filename,      &
                                  rivid          &
                                  )
    USE MODELVAR, ONLY            :    sim_length,   &
                                       date_comp,    &
                                       warm_length,  &
                                       comp_length,  &
                                       riverflux

    INTEGER, INTENT(IN)           ::   rivid
    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, ierr, fileunit  
    CHARACTER(100)                ::   errmsg             

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      IF(ierr /= 0) THEN
        print *, "Error opening file: ", TRIM(errmsg)
        RETURN
      END IF

      WRITE(fileunit, "(A, 1X, A)") "Date", "Flow(m3/s)"
        DO i = 1, comp_length
          WRITE(fileunit, "(A11, F15.4)") date_comp(i), riverflux(rivid)%flow(i+warm_length)
        END DO

    CLOSE(fileunit, IOSTAT=ierr, IOMSG=errmsg)
    IF(ierr /= 0) print *, "Error closing file: ", TRIM(errmsg)

  END SUBROUTINE print_indiv_rch_flow
!######################################################################################################
  SUBROUTINE print_indiv_rch_sedconc(filename,      &
                                     rivid          &
                                     )
    USE MODELVAR, ONLY            :    sim_length,   &
                                       date_comp,    &
                                       warm_length,  &
                                       comp_length
    USE MODELVAR_SED, ONLY        :    sedflux

    INTEGER, INTENT(IN)           ::   rivid
    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, ierr, fileunit  
    CHARACTER(100)                ::   errmsg             

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      IF(ierr /= 0) THEN
        print *, "Error opening file: ", TRIM(errmsg)
        RETURN
      END IF

      WRITE(fileunit, "(A, 1X, A)") "Date", "SPS(mg/L)"
        DO i = 1, comp_length
          WRITE(fileunit, "(A11, F15.4)") date_comp(i), sedflux(rivid)%spsconc(4,i+warm_length)
        END DO

    CLOSE(fileunit, IOSTAT=ierr, IOMSG=errmsg)
    IF(ierr /= 0) print *, "Error closing file: ", TRIM(errmsg)

  END SUBROUTINE print_indiv_rch_sedconc
!######################################################################################################
    SUBROUTINE print_indiv_rch_sedload(filename,      &
                                       rivid          &
                                      )
    USE MODELVAR, ONLY            :    sim_length,   &
                                       date_comp,    &
                                       warm_length,  &
                                       comp_length
    USE MODELVAR_SED, ONLY        :    sedflux

    INTEGER, INTENT(IN)           ::   rivid
    CHARACTER(LEN=*), INTENT(IN)  ::   filename
    INTEGER                       ::   i, ierr, fileunit  
    CHARACTER(100)                ::   errmsg             

    OPEN(NEWUNIT=fileunit, FILE=filename, STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=ierr, IOMSG=errmsg)
      IF(ierr /= 0) THEN
        print *, "Error opening file: ", TRIM(errmsg)
        RETURN
      END IF

      WRITE(fileunit, "(A, 1X, A)") "Date", "SPS(mg/L)"
        DO i = 1, comp_length
          WRITE(fileunit, "(A11, F15.4)") date_comp(i), sedflux(rivid)%sedload(4,i+warm_length)
        END DO

    CLOSE(fileunit, IOSTAT=ierr, IOMSG=errmsg)
    IF(ierr /= 0) print *, "Error closing file: ", TRIM(errmsg)

  END SUBROUTINE print_indiv_rch_sedload
!######################################################################################################
  ! SUBROUTINE print_land_nc(time_len,         &
  !                          var_record,       &
  !                          filename)
  !   USE NETCDF
  !   USE GENERAL,   ONLY   :    check_nc
  !   USE MODELVAR,  ONLY   :    num_cell,        &
  !                              max_soillayer,   &
  !                              num_sub,         &
  !                              simcfg
  !   USE MODELTYPE, ONLY   :    VarLand
  !   INTEGER, INTENT(IN)                     ::    time_len
  !   TYPE(VarLand), INTENT(IN)             ::    var_record                 
  !   CHARACTER(LEN=*), INTENT(IN)            ::    filename
  !   INTEGER, PARAMETER          ::  ndims2 = 2     !<ncell, time, OR sub, time
  !   INTEGER, PARAMETER          ::  ndims3 = 3     !<layer, cell, time
  !   CHARACTER(LEN=*), PARAMETER ::  UNITS = "units"
  !   CHARACTER(LEN=*), PARAMETER ::  TIME_UNITS = "days since 2014-04-30"
  !   INTEGER :: ncid,time_varid
  !   INTEGER :: varid1,varid2,varid3,varid4,varid5,varid6,varid7
  !   INTEGER :: varid8,varid9,varid10  !<For subbasin
  !   INTEGER  :: varid(10)
  !   INTEGER :: cell_dimid, time_dimid, layer_dimid, sub_dimid, dimids2(ndims2), dimids3(ndims3)
  !   INTEGER :: dimids2_sub(ndims2) !<For subbasin
  !   INTEGER :: i
  !   REAL(KIND=8) :: times(time_len)

  !   IF(.NOT.simcfg%write_nc_land) RETURN
  !   !>Create
  !   CALL check_nc(nf90_create(filename, NF90_NETCDF4, ncid))
  !   !>Define
  !   CALL check_nc(nf90_def_dim(ncid, "cell", num_cell, cell_dimid))
  !   CALL check_nc(nf90_def_dim(ncid, "time", time_len, time_dimid))
  !   CALL check_nc(nf90_def_dim(ncid, "layer", max_soillayer, layer_dimid))
  !   CALL check_nc(nf90_def_dim(ncid, "subbasin", num_sub, sub_dimid))

  !   dimids2=(/cell_dimid, time_dimid/)
  !   dimids3=(/layer_dimid, cell_dimid, time_dimid/)
  !   dimids2_sub=(/sub_dimid, time_dimid/)

  !   CALL check_nc(nf90_def_var(ncid, var_record%runoff_surf%name, NF90_FLOAT, dimids2, varid(1)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%infiltration%name, NF90_FLOAT, dimids2, varid(2)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%runoff_soil%name, NF90_FLOAT, dimids2, varid(3)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%recharge%name, NF90_FLOAT, dimids2, varid(4)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%ep_soil%name, NF90_FLOAT, dimids2, varid(5)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%soil_water%name, NF90_FLOAT, dimids3, varid(6)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%soil_temp%name, NF90_FLOAT, dimids3, varid(7)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_suflow%name, NF90_FLOAT, dimids2_sub, varid(8)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_soflow%name, NF90_FLOAT, dimids2_sub, varid(9)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_gwflow%name, NF90_FLOAT, dimids2_sub, varid(10)))
  !   DO i = 1, 10
  !     CALL check_nc(nf90_put_att(ncid, varid(i), "missing_value", -9999.))
  !   END DO

  !   CALL check_nc(nf90_def_var(ncid, "time", NF90_FLOAT, time_dimid, time_varid))
  !   call check_nc(nf90_put_att(ncid, time_varid, UNITS, TIME_UNITS))
  !   CALL check_nc(nf90_enddef(ncid))
  !   !>Write
  !   CALL check_nc(nf90_put_var(ncid, varid(1), var_record%runoff_surf%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(2), var_record%infiltration%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(3), var_record%runoff_soil%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(4), var_record%recharge%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(5), var_record%ep_soil%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(6), var_record%soil_water%value3))
  !   CALL check_nc(nf90_put_var(ncid, varid(7), var_record%soil_temp%value3))
  !   CALL check_nc(nf90_put_var(ncid, varid(8), var_record%sub_suflow%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(9), var_record%sub_soflow%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(10), var_record%sub_gwflow%value2))

  !   times = [(i, i = 1, time_len)]
  !   CALL check_nc(nf90_put_var(ncid, time_varid, times))
    
  !   CALL check_nc(nf90_close(ncid))
  ! END SUBROUTINE print_land_nc
!######################################################################################################
  ! SUBROUTINE print_rch_nc(time_len,       &
  !                         var_record,        &
  !                         filename)
  !   USE NETCDF
  !   USE GENERAL,   ONLY   :    check_nc
  !   USE MODELVAR,  ONLY   :    num_sub
  !   USE MODELTYPE, ONLY   :    VarRch
  !   INTEGER, INTENT(IN)                     ::    time_len
  !   TYPE(VarRch), INTENT(IN)             ::    var_record                 
  !   CHARACTER(LEN=*), INTENT(IN)            ::    filename
  !   INTEGER, PARAMETER          ::  ndims2 = 2     !<ncell, time, OR sub, time
  !   CHARACTER(LEN=*), PARAMETER ::  UNITS = "units"
  !   CHARACTER(LEN=*), PARAMETER ::  TIME_UNITS = "days since 2014-04-30"
  !   INTEGER :: ncid,time_varid
  !   INTEGER  :: varid(2)
  !   INTEGER :: time_dimid, sub_dimid, dimids2(ndims2)
  !   INTEGER :: dimids2_sub(ndims2) !<For subbasin
  !   INTEGER :: i
  !   REAL(KIND=8) :: times(time_len)
  !   !>Create
  !   CALL check_nc(nf90_create(filename, NF90_NETCDF4, ncid))
  !   !>Define
  !   CALL check_nc(nf90_def_dim(ncid, "rch", num_sub, sub_dimid))

  !   dimids2=(/sub_dimid, time_dimid/)

  !   CALL check_nc(nf90_def_var(ncid, var_record%rch_flow%name, NF90_FLOAT, dimids2, varid(1)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%rch_vol%name, NF90_FLOAT, dimids2, varid(2)))

  !   DO i = 1, 2
  !     CALL check_nc(nf90_put_att(ncid, varid(i), "missing_value", -9999.))
  !   END DO
  !   CALL check_nc(nf90_def_var(ncid, "time", NF90_FLOAT, time_dimid, time_varid))
  !   call check_nc(nf90_put_att(ncid, time_varid, UNITS, TIME_UNITS))
  !   CALL check_nc(nf90_enddef(ncid))

  !   CALL check_nc(nf90_put_var(ncid, varid(1), var_record%rch_flow%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(2), var_record%rch_vol%value2))

  ! END SUBROUTINE print_rch_nc
!######################################################################################################
  ! SUBROUTINE record_state(var_record,     &
  !                         filename)
  !   USE NETCDF
  !   USE MODELTYPE, ONLY   :    VarHot
  !   USE GENERAL,   ONLY   :    check_nc
  !   USE MODELVAR,  ONLY   :    num_cell,      &
  !                              max_soillayer, &
  !                              num_sub,       &
  !                              simcfg
  !   TYPE(VarHot), INTENT(IN)         ::    var_record
  !   CHARACTER(LEN=*), INTENT(IN)     ::    filename
  !   INTEGER, PARAMETER          ::  ndims1 = 1     !<ncell OR subbasin
  !   INTEGER, PARAMETER          ::  ndims2 = 2     !<layer, ncell
  !   INTEGER :: ncid
  !   INTEGER :: varid(17)
  !   INTEGER :: cell_dimid, layer_dimid, sub_dimid
  !   INTEGER :: dimids1(ndims1), dimids2(ndims2), dimids1_sub(ndims1)
  !   INTEGER :: i

  !   IF(simcfg%hotstart) RETURN
  !   CALL check_nc(nf90_create(filename, NF90_NETCDF4, ncid))
  !   !>Define
  !   CALL check_nc(nf90_def_dim(ncid, "cell", num_cell, cell_dimid))
  !   CALL check_nc(nf90_def_dim(ncid, "layer", max_soillayer, layer_dimid))
  !   CALL check_nc(nf90_def_dim(ncid, "subbasin", num_sub, sub_dimid))
    
  !   dimids1=(/cell_dimid/)
  !   dimids2=(/layer_dimid, cell_dimid/)
  !   dimids1_sub=(/sub_dimid/)

  !   CALL check_nc(nf90_def_var(ncid, var_record%icept_storage%name, NF90_FLOAT, dimids1, varid(1)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%soil_water%name, NF90_FLOAT, dimids2, varid(2)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%soil_temp%name, NF90_FLOAT, dimids2, varid(3)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%soil_deeptemp%name, NF90_FLOAT, dimids1, varid(4)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%recharge%name, NF90_FLOAT, dimids1, varid(5)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%runoff_gw%name, NF90_FLOAT, dimids1, varid(6)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%aquifer_water%name, NF90_FLOAT, dimids1, varid(7)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_suflow%name, NF90_FLOAT, dimids1_sub, varid(8)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_soflow%name, NF90_FLOAT, dimids1_sub, varid(9)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%sub_gwflow%name, NF90_FLOAT, dimids1_sub, varid(10)))

  !   CALL check_nc(nf90_def_var(ncid, var_record%rivflux_V1%name, NF90_FLOAT, dimids1_sub, varid(11)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%rivstate_Q1%name, NF90_FLOAT, dimids1_sub, varid(12)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%rivstate_Q2%name, NF90_FLOAT, dimids1_sub, varid(13)))

  !   CALL check_nc(nf90_def_var(ncid, var_record%infilt_day_old%name, NF90_FLOAT, dimids1, varid(14)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%infilt_rate_old%name, NF90_FLOAT, dimids1, varid(15)))
  !   CALL check_nc(nf90_def_var(ncid, var_record%infiltration%name, NF90_FLOAT, dimids1, varid(16)))

  !   CALL check_nc(nf90_def_var(ncid, var_record%q_up%name, NF90_FLOAT, dimids1_sub, varid(17)))

  !   DO i = 1, 17
  !     CALL check_nc(nf90_put_att(ncid, varid(i), "missing_value", -9999.))
  !   END DO
  !   CALL check_nc(nf90_enddef(ncid))
  !   !>Write 
  !   CALL check_nc(nf90_put_var(ncid, varid(1), var_record%icept_storage%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(2), var_record%soil_water%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(3), var_record%soil_temp%value2))
  !   CALL check_nc(nf90_put_var(ncid, varid(4), var_record%soil_deeptemp%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(5), var_record%recharge%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(6), var_record%runoff_gw%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(7), var_record%aquifer_water%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(8), var_record%sub_suflow%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(9), var_record%sub_soflow%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(10), var_record%sub_gwflow%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(11), var_record%rivflux_V1%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(12), var_record%rivstate_Q1%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(13), var_record%rivstate_Q2%value1))

  !   CALL check_nc(nf90_put_var(ncid, varid(14), var_record%infilt_day_old%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(15), var_record%infilt_rate_old%value1))
  !   CALL check_nc(nf90_put_var(ncid, varid(16), var_record%infiltration%value1))

  !   CALL check_nc(nf90_put_var(ncid, varid(17), var_record%q_up%value1))
    
  !   CALL check_nc(nf90_close(ncid))
  !   CALL log_process('Model states recorded.')
    
  ! END SUBROUTINE record_state
!######################################################################################################
END MODULE DATAWRITE