MODULE GENERAL 
  IMPLICIT NONE
CONTAINS
  SUBROUTINE add_water(n,vol,conc,q,cq)
    USE MODELVAR,  ONLY   :  realzero
    INTEGER, INTENT(IN) :: n       
    REAL(KIND=8), INTENT(INOUT) :: vol     
    REAL(KIND=8), INTENT(INOUT) :: conc(n) 
    REAL(KIND=8), INTENT(IN)    :: q       
    REAL(KIND=8), INTENT(IN)    :: cq(n)   
    REAL(KIND=8) newvol
    newvol = vol + q
    IF(newvol > realzero)THEN
      conc = (conc*vol + cq*q)/newvol
      vol = newvol
    ELSE
      conc = 0.
      vol = 0.
    ENDIF
  END SUBROUTINE add_water

  SUBROUTINE remove_water(vol,n,conc,q,cq,err)
    USE MODELVAR,    ONLY    :  realzero
    REAL(KIND=8), INTENT(INOUT)  :: vol       
    INTEGER, INTENT(IN)  :: n                 
    REAL(KIND=8), INTENT(INOUT)  :: conc(n)   
    REAL(KIND=8), INTENT(IN)     :: q         
    REAL(KIND=8), INTENT(IN)     :: cq(n)      
    INTEGER, INTENT(OUT) :: err               
    INTEGER i   
    err = 0
    IF(vol - q .GT. 0.)THEN
      DO i =1, n
        IF(conc(i) .NE. cq(i)) conc(i) = (conc(i)*vol - cq(i)*q)/(vol - q)
      ENDDO
      vol   = vol - q
    ELSEIF(vol - q .GT. - realzero)THEN
      DO i = 1, n
        IF(conc(i)==cq(i) .OR. ISNAN(conc(i)).OR.ISNAN(cq(i)))THEN
          conc(i) = 0.0                 !Takes care of NaN and Infinity concentrations, better way?
        ELSEIF((conc(i)*vol - cq(i)*q) == 0.0)THEN
          conc(i) = 0.0
        ELSE
          WRITE(6,*) 'cq',cq(i),'cvol',conc(i)
          WRITE(6,*) 'q',q,'vol',vol
          WRITE(6,*) 'ERROR - remove water routine cq<>conc'
          err = 1
        ENDIF
      ENDDO
      vol = 0.
    ELSE
      WRITE(6,*) 'ERROR - remove water routine Q>VOL'
      WRITE(6,*) 'ERROR - Q=',q,'VOL=',vol
      err = 1
      vol = 0.
      ! conc(I) = 0.
    ENDIF
  END SUBROUTINE remove_water
!####################################################################################################################
  ! SUBROUTINE check_nc(status)
  !   USE NETCDF
  !   INTEGER, INTENT(IN) :: STATUS
  !   IF(STATUS .NE. NF90_NOERR) THEN 
  !     WRITE(6,*) TRIM(NF90_STRERROR(STATUS))
  !     STOP 'Stopped in nc file manipulating.'
  !   ENDIF
  ! END SUBROUTINE check_nc
 
END MODULE GENERAL