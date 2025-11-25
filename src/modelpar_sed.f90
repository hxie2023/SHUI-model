MODULE MODELPAR_SED

  USE MODELTYPE,  ONLY : ParType

  IMPLICIT NONE
  TYPE(ParType), ALLOCATABLE   ::      modpar(:)
  REAL(KIND=8), ALLOCATABLE    ::      sedpar_general(:)
  REAL(KIND=8), ALLOCATABLE    ::      sedpar_landuse(:,:) 
  REAL(KIND=8), ALLOCATABLE    ::      sedpar_soil(:,:)  

  INTEGER, PARAMETER :: genid_rchvpeak          = 1
  INTEGER, PARAMETER :: genid_rchsmexp          = 2
  INTEGER, PARAMETER :: genid_rchsmcoef         = 3
  INTEGER, PARAMETER :: genid_rchk              = 4
  INTEGER, PARAMETER :: genid_rchc              = 4
  INTEGER, PARAMETER :: genid_rchwclay          = 6
  INTEGER, PARAMETER :: genid_rchwsilt          = 7
  INTEGER, PARAMETER :: genid_rchwsand          = 8
  INTEGER, PARAMETER :: genid_rchzk             = 9
  INTEGER, PARAMETER :: genid_rchzm             = 10
  INTEGER, PARAMETER :: genid_rchmclay          = 11
  INTEGER, PARAMETER :: genid_rchmsilt          = 12
  INTEGER, PARAMETER :: genid_imp               = 13
  INTEGER, PARAMETER :: genid_m_pprelmax        = 14
  INTEGER, PARAMETER :: genid_m_pprelexp        = 15
  INTEGER, PARAMETER :: genid_m_eroddecay       = 16
  INTEGER, PARAMETER :: genid_idriv             = 17

  INTEGER, PARAMETER :: luid_manning            = 1
  INTEGER, PARAMETER :: c_factor                = 2

  INTEGER, PARAMETER :: soilid_dksand           = 1
  INTEGER, PARAMETER :: soilid_dksilt           = 2
  INTEGER, PARAMETER :: soilid_dkclay           = 3
  INTEGER, PARAMETER :: soilid_drsand           = 4
  INTEGER, PARAMETER :: soilid_drsilt           = 5
  INTEGER, PARAMETER :: soilid_drclay           = 6


  INTEGER, PARAMETER :: num_max_sedpar             = 25

  INTEGER, PARAMETER :: num_sedpar_general         =  17
  INTEGER, PARAMETER :: num_sedpar_landuse         =  2
  INTEGER, PARAMETER :: num_sedpar_soil            =  6

CONTAINS
  SUBROUTINE load_sed_parameter
    USE MODELVAR_SED,     ONLY    :       tyh_sed_unit,      &
                                          tyh_sed_file

    CALL get_sedpar_general(tyh_sed_unit, tyh_sed_file, sedpar_general)
    CALL get_sedpar_landuse(tyh_sed_unit, tyh_sed_file, sedpar_landuse)
    CALL get_sedpar_soil(tyh_sed_unit, tyh_sed_file, sedpar_soil)
    ! CALL log_process('Parameter values set.')

  END SUBROUTINE load_sed_parameter

  SUBROUTINE get_sedpar_general(funit,    & 
                             fname,    &
                             par_gen)

    INTEGER, INTENT(IN)                    :: funit
    CHARACTER(LEN = *),INTENT(IN)          :: fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: par_gen(:)       
    INTEGER              :: j, nskip
    REAL(KIND=8)         :: value1
    CHARACTER(LEN=100)   :: line
    LOGICAL              :: existed                
    ALLOCATE(par_gen(num_sedpar_general))          
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
      DO j = 1, num_sedpar_general
        READ(funit, *) line, value1
        par_gen(j) = value1
      END DO
    CLOSE(funit)
  END SUBROUTINE get_sedpar_general

  SUBROUTINE get_sedpar_landuse(funit,     &
                                fname,     &
                                par_lu)  
    USE MODELVAR, ONLY :  num_landuse
    INTEGER,                   INTENT(IN)  :: funit
    CHARACTER(LEN = *),        INTENT(IN)  :: fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) :: par_lu(:,:)
    INTEGER              :: i, j, t
    CHARACTER(LEN = 100) :: line
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:)   ::   value_lu
    ALLOCATE(par_lu(num_sedpar_landuse, num_landuse))
    ALLOCATE(value_lu(num_landuse))
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i  = 1, 4 + num_sedpar_general + 3
        READ(funit, *)
      END DO
      DO j = 1, num_sedpar_landuse
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
  END SUBROUTINE get_sedpar_landuse

  SUBROUTINE get_sedpar_soil(funit, fname, parsoil)  
    USE MODELVAR, ONLY : num_soiltype
    INTEGER, INTENT(IN)                  :: funit
    CHARACTER(LEN = *),INTENT(IN)        :: fname
    REAL(KIND=8),ALLOCATABLE,INTENT(OUT) :: parsoil(:,:)
    INTEGER i, j, t, AllocateStatus
    REAL(KIND=8), ALLOCATABLE            ::   value_soil(:)
    CHARACTER(LEN = 50) line
    ALLOCATE(value_soil(num_soiltype))    
    ALLOCATE(parsoil(num_sedpar_soil, num_soiltype), STAT = AllocateStatus)
    IF (AllocateStatus /= 0) STOP "Error"
    value_soil = 0.
    OPEN(UNIT = funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO i  = 1, 4 + num_sedpar_general + 3 + num_sedpar_landuse + 3
        READ(funit, *) 
      END DO
      DO j = 1, num_sedpar_soil
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
  END SUBROUTINE get_sedpar_soil
END MODULE MODELPAR_SED