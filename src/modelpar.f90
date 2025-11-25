MODULE MODELPAR

  USE DATAWRITE, ONLY : log_process
  USE MODELTYPE,  ONLY : ParType
  IMPLICIT NONE
  TYPE(ParType), ALLOCATABLE   ::      modpar(:)
  REAL(KIND=8), ALLOCATABLE    ::      par_general(:)
  REAL(KIND=8), ALLOCATABLE    ::      par_landuse(:,:) 
  REAL(KIND=8), ALLOCATABLE    ::      par_soil(:,:)  

  INTEGER, PARAMETER :: genid_coef_pet_am       = 1
  INTEGER, PARAMETER :: genid_coef_pet_ph       = 2
  INTEGER, PARAMETER :: genid_surobeta          = 3
  INTEGER, PARAMETER :: genid_suroconf          = 4
  INTEGER, PARAMETER :: genid_soroconf          = 5   
  INTEGER, PARAMETER :: genid_gw_delay          = 6
  INTEGER, PARAMETER :: genid_gwrc              = 7
  INTEGER, PARAMETER :: genid_deepmem           = 8
  INTEGER, PARAMETER :: genid_sopetdis          = 9
  INTEGER, PARAMETER :: genid_wpet              = 10

  INTEGER, PARAMETER :: luid_coef_pet           = 1
  INTEGER, PARAMETER :: luid_icept_max          = 2
  INTEGER, PARAMETER :: luid_surfmem            = 3
  INTEGER, PARAMETER :: luid_depthrel           = 4
  INTEGER, PARAMETER :: luid_soiltempreA        = 5
  INTEGER, PARAMETER :: luid_soiltempreB        = 6
  INTEGER, PARAMETER :: luid_soiltempttrig      = 7
  INTEGER, PARAMETER :: luid_suroroute          = 8

  INTEGER, PARAMETER :: soilid_wp1              = 1
  INTEGER, PARAMETER :: soilid_wp2              = 2
  INTEGER, PARAMETER :: soilid_wp3              = 3
  INTEGER, PARAMETER :: soilid_awc1             = 4
  INTEGER, PARAMETER :: soilid_awc2             = 5
  INTEGER, PARAMETER :: soilid_awc3             = 6 
  INTEGER, PARAMETER :: soilid_macfrac          = 7
  INTEGER, PARAMETER :: soilid_rrcs1            = 8
  INTEGER, PARAMETER :: soilid_rrcs2            = 9
  INTEGER, PARAMETER :: soilid_rrcs3            = 10
  INTEGER, PARAMETER :: soilid_adhc             = 11
  INTEGER, PARAMETER :: soilid_b                = 12
  INTEGER, PARAMETER :: soilid_bf               = 13

  INTEGER, PARAMETER :: num_max_par             = 30

  INTEGER, PARAMETER :: num_par_general         =  10
  INTEGER, PARAMETER :: num_par_landuse         =  8
  INTEGER, PARAMETER :: num_par_soil            =  13

CONTAINS
  SUBROUTINE load_parameter
    USE MODELVAR,     ONLY    :       par_file_unit,      &
                                      par_file

    CALL get_par_general(par_file_unit, par_file, par_general)
    CALL get_par_landuse(par_file_unit, par_file, par_landuse)
    CALL get_par_soil(par_file_unit, par_file, par_soil)
    CALL log_process('Parameter values set.')

  END SUBROUTINE load_parameter

  SUBROUTINE get_par_general(funit,    & 
                             fname,    &
                             par_gen)

    INTEGER, INTENT(IN)                    :: funit
    CHARACTER(LEN = *),INTENT(IN)          :: fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: par_gen(:)       
    INTEGER              :: j, nskip
    REAL(KIND=8)         :: value1
    CHARACTER(LEN=100)   :: line
    LOGICAL              :: existed                
    ALLOCATE(par_gen(num_par_general))          
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
      DO j = 1, num_par_general
        READ(funit, *) line, value1
        par_gen(j) = value1
      END DO
    CLOSE(funit)
  END SUBROUTINE get_par_general

  SUBROUTINE get_par_landuse(funit,     &
                             fname,     &
                             par_lu)  
    USE MODELVAR, ONLY :  num_landuse
    INTEGER,                   INTENT(IN)  :: funit
    CHARACTER(LEN = *),        INTENT(IN)  :: fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: par_lu(:,:)
    INTEGER              :: i, j, t
    CHARACTER(LEN = 100) :: line
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   ::   value_lu
    ALLOCATE(par_lu(num_par_landuse, num_landuse))
    ALLOCATE(value_lu(num_landuse))
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i  = 1, 4 + num_par_general + 3
        READ(funit, *)
      END DO
      DO j = 1, num_par_landuse
        READ(funit, *) line, value_lu(1: num_landuse)
        IF(line(1:1) == '!') THEN
          EXIT
        ELSE
          DO t = 1, num_landuse
            par_lu(j, t) = value_lu(t)
          END DO
        END IF
      END DO
    CLOSE(funit)
  END SUBROUTINE get_par_landuse

  SUBROUTINE get_par_soil(funit, fname, parsoil)  
    USE MODELVAR, ONLY : num_soiltype
    INTEGER, INTENT(IN)                  :: funit
    CHARACTER(LEN = *),INTENT(IN)        :: fname
    REAL(KIND=8),ALLOCATABLE,INTENT(OUT) :: parsoil(:,:)
    INTEGER i, j, t, AllocateStatus
    REAL(KIND=8), ALLOCATABLE            ::   value_soil(:)
    CHARACTER(LEN = 50) line
    ALLOCATE(value_soil(num_soiltype))    
    ALLOCATE(parsoil(num_par_soil, num_soiltype), STAT = AllocateStatus)
    IF (AllocateStatus /= 0) STOP "Error"
    value_soil = 0.
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i  = 1, 4 + num_par_general + 3 + num_par_landuse + 3
        READ(funit, *) 
      END DO
      DO j = 1, num_par_soil
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
  END SUBROUTINE get_par_soil
END MODULE MODELPAR