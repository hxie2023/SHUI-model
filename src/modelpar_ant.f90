MODULE MODELPAR_ANT

  USE MODELTYPE,  ONLY : ParType

  IMPLICIT NONE
  TYPE(ParType), ALLOCATABLE   ::      modpar(:)
  REAL(KIND=8), ALLOCATABLE    ::      antpar_general(:)
  REAL(KIND=8), ALLOCATABLE    ::      antpar_soil(:,:)  


  INTEGER, PARAMETER :: genid_gw_mix             = 1
  INTEGER, PARAMETER :: genid_percop             = 2
  INTEGER, PARAMETER :: genid_ratem              = 3
  INTEGER, PARAMETER :: genid_ratelayer          = 4
  INTEGER, PARAMETER :: genid_kex                = 5
  INTEGER, PARAMETER :: genid_benbury            = 6
  INTEGER, PARAMETER :: genid_ant_gwdelay        = 7
  INTEGER, PARAMETER :: genid_kt                 = 8
  INTEGER, PARAMETER :: genid_kdif               = 9
  INTEGER, PARAMETER :: genid_kdif_r             = 10

  INTEGER, PARAMETER :: soilid_freucoef          = 1
  INTEGER, PARAMETER :: soilid_freuexp           = 2
  INTEGER, PARAMETER :: soilid_freurate          = 3 


  INTEGER, PARAMETER :: num_max_antpar            = 13

  INTEGER, PARAMETER :: num_antpar_general        = 10
  INTEGER, PARAMETER :: num_antpar_soil           = 3

CONTAINS
  SUBROUTINE load_ant_parameter
    USE MODELVAR_ANT,     ONLY    :       par_ant_file,      &
                                          par_ant_unit

    CALL get_antpar_general(par_ant_unit, par_ant_file, antpar_general)
    CALL get_antpar_soil(par_ant_unit, par_ant_file, antpar_soil)
    ! CALL log_process('Parameter values set.')

  END SUBROUTINE load_ant_parameter

  SUBROUTINE get_antpar_general(funit,    & 
                                fname,    &
                                par_gen)

    INTEGER, INTENT(IN)                    :: funit
    CHARACTER(LEN = *),INTENT(IN)          :: fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: par_gen(:)       
    INTEGER              :: j, nskip
    REAL(KIND=8)         :: value1
    CHARACTER(LEN=100)   :: line
    LOGICAL              :: existed                
    ALLOCATE(par_gen(num_antpar_general))          
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')    
      existed  = .TRUE.
      nskip = 0
      DO WHILE(existed)
        READ(funit, *) line
        IF(line(1:1) .EQ. "#") THEN
          nskip = nskip + 1
        ELSE
          existed = .FALSE.
        ENDIF
      END DO
      backspace(funit)
      DO j = 1, num_antpar_general
        READ(funit, *) line, value1
        par_gen(j) = value1
      END DO
    CLOSE(funit)
  END SUBROUTINE get_antpar_general

  SUBROUTINE get_antpar_soil(funit, fname, parsoil)  
    USE MODELVAR, ONLY : num_soiltype
    INTEGER, INTENT(IN)                  :: funit
    CHARACTER(LEN = *),INTENT(IN)        :: fname
    REAL(KIND=8),ALLOCATABLE,INTENT(OUT) :: parsoil(:,:)
    INTEGER i, j, t, AllocateStatus
    REAL(KIND=8), ALLOCATABLE            ::   value_soil(:)
    CHARACTER(LEN = 50) line
    ALLOCATE(value_soil(num_soiltype))    
    ALLOCATE(parsoil(num_antpar_soil, num_soiltype), STAT = AllocateStatus)
    IF (AllocateStatus /= 0) STOP "Error"
    value_soil = 0.
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i  = 1, 4 + num_antpar_general + 3 
        READ(funit, *) 
      END DO
      DO j = 1, num_antpar_soil
        READ(funit, *) line, value_soil(1 : num_soiltype) 
        IF(line(1:1) == '!') THEN
          EXIT
        ELSE
          DO t = 1 , num_soiltype
            parsoil(j, t) = value_soil(t)
          END DO
        END IF
      END DO
    CLOSE(funit)
  END SUBROUTINE get_antpar_soil
END MODULE MODELPAR_ANT