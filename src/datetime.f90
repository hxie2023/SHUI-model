MODULE DATETIME
  USE MODELTYPE,  ONLY   :   DateType, JulianDateType
  USE MODELVAR, ONLY       :   realzero, dp
  IMPLICIT NONE
  INTEGER, DIMENSION(13) :: ndays        = (/0,31,60,91,121,152,182,213,244,274,305,335,366/) 
  INTEGER, DIMENSION(13) :: ndays_leap   = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
  INTEGER, DIMENSION(13) :: ndays_noleap = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
  INTEGER, DIMENSION(12) :: ndmo         = (/0,0,0,0,0,0,0,0,0,0,0,0/) 
  INTEGER, PARAMETER     :: DebugLevel   = 0
CONTAINS
  SUBROUTINE define_current_time(it, date, cur_time)
    USE MODELTYPE, ONLY : TIMEINFORMATIONTYPE
    USE MODELVAR, ONLY    : timesteps_per_day,    &
                          idtlag,               &
                          dtmonday,             &
                          bdate
    INTEGER, INTENT(IN)        ::  it      !<index of current timestep
    TYPE(DateType), INTENT(IN) ::  date     !<current date
    TYPE(TIMEINFORMATIONTYPE),INTENT(OUT) :: cur_time
    TYPE(DateType) yearago
    INTEGER dayofweek, tsofday
    INTEGER dos   
    INTEGER shift 
    cur_time%date = date
    cur_time%dayno = DoY(date)
    CALL simulate_timesteps_to_monday(bdate, dtmonday)
    dos = (it-1+idtlag)/timesteps_per_day   
    shift = (7*timesteps_per_day-dtmonday)/timesteps_per_day  
    dayofweek = MOD(dos+shift,7)  
    cur_time%dayofweek = dayofweek + 1 
    yearago = datetype(date%year-1,date%month,date%day,date%hour,date%minute)
    cur_time%prevdoy = numdays_of_year(yearago)
    tsofday = MOD(it+idtlag, timesteps_per_day)
    IF(tsofday==0)THEN
      tsofday = timesteps_per_day 
    ENDIF
    cur_time%tsofday = tsofday
    cur_time%yrs_prt = 1+cur_time%date%Year-bdate%Year
  END SUBROUTINE define_current_time

  FUNCTION Julian2Dhm(JulianDate)
    TYPE(JulianDateType), INTENT(IN) :: JulianDate
    TYPE(DateType) :: Julian2Dhm
    INTEGER :: D,Hour,Minute,iDum
    REAL(KIND=8) :: Dum
    D = NINT(JulianDate%Head)
    Dum = 60._dp*24._dp*JulianDate%Tail
    IF (DebugLevel.GT.0) WRITE(*,40) Dum
  40 FORMAT('Julian2Dhm : Dum = ',F20.10)
    iDum = NINT(Dum) 
    IF (DebugLevel.GT.0) WRITE(*,30) iDum
  30 FORMAT('Julian2Dhm : iDum = ',I10)
    Hour = iDum/60 
    IF (DebugLevel.GT.0) WRITE(*,10) Hour
  10 FORMAT('Julian2Dhm : Hour = ',I2)
    Minute = iDum - Hour*60
    IF (DebugLevel.GT.0) WRITE(*,20) Minute
  20 FORMAT('Julian2Dhm : Minute = ',I2)
    Julian2Dhm = DateType(0,0,D,Hour,Minute)
  END FUNCTION Julian2Dhm

  INTEGER FUNCTION period_length(sdate, enddate)
    USE MODELVAR, ONLY : steplen
    TYPE(datetype), INTENT(IN) :: sdate !<start date
    TYPE(datetype), INTENT(IN) :: enddate !<end date
    TYPE(datetype)       :: period
    TYPE(juliandatetype) :: julstart,julend,tmp
    REAL(KIND=8)         :: nrsteps
    IF(steplen%year.NE.0.OR.steplen%month.NE.0) THEN
      WRITE(6,*) 'ERROR: monthly or yearly time step not implemented.'
      STOP 1
    ENDIF
    julstart = date2julian(sdate)
    julend = date2julian(enddate)
    tmp%head = julend%head - julstart%head
    tmp%tail = julend%tail - julstart%tail
    IF(tmp%tail.LT.0) THEN
      tmp%tail = tmp%tail + 1._dp
      tmp%head = tmp%head - 1._dp
    ENDIF
    period = julian2dhm(tmp)
    nrsteps = (period%day*24.*60. + period%hour*60. + period%minute)/ &
         (steplen%day*24.*60. + steplen%hour*60. + steplen%minute)
    IF(MOD(nrsteps,1.) .GT. realzero) THEN
      WRITE(6,*) 'WARNING: Defined period does not match steplength.'
    ENDIF
    period_length = INT(nrsteps)
  END FUNCTION period_length

  SUBROUTINE string_convert_to_datetype(datestr, date)
    CHARACTER(LEN = *), INTENT(IN) :: datestr 
    TYPE(DateType), INTENT(OUT)  :: date   
    INTEGER i,l
    l = LEN(TRIM(datestr))
    READ(datestr, '(I4)', ERR = 100) date%Year
    i = 5
    IF(datestr(i:i) == CHAR(45))THEN 
      i = i + 1
    ENDIF
    IF(l < i) THEN
      date%Month = 1  
      date%Day = 1
      date%Hour = 0
      date%Minute = 0
      RETURN
    ENDIF
    READ(datestr(i : i+1), '(I2)') date%Month
    i = i + 2
    IF(datestr(i:i) == CHAR(45)) THEN !-
      i = i + 1
    ENDIF
    IF(l < i) THEN
      date%Day = 1  
      date%Hour = 0 
      date%Minute = 0
      RETURN
    ENDIF
    READ(datestr(i : i+1), '(I2)') date%Day
    i = i + 2
    IF(l < i) THEN 
      date%Hour = 0    
      date%Minute = 0 
      RETURN
    ENDIF
    IF(datestr(i : i) == CHAR(32) .OR. datestr(i : i) == CHAR(9)) THEN 
      i = i + 1
    ENDIF
    READ(datestr(i : i+1), '(I2)') date%Hour
    i = i + 2
    IF(datestr(i:i) == CHAR(58)) THEN !:
      i = i + 1
    ENDIF
    READ(datestr(i : i+1), '(I2)') date%Minute
    RETURN
100 WRITE(*,*) 'ERROR: Converting string to datetype: ', TRIM(datestr)
    RETURN
  END SUBROUTINE string_convert_to_datetype

  SUBROUTINE simulate_timesteps_to_monday(fdate,tstep)
    USE MODELVAR, ONLY : timesteps_per_day
    TYPE(DateType), INTENT(IN) :: fdate        
    INTEGER, INTENT(OUT)       :: tstep   
    !>Local
    INTEGER        :: period
    TYPE(DateType) :: amonday
    tstep = 0
    amonday = DateType(2000,5,1,0,0)        
    period = period_length(fdate,amonday)   
    tstep = MOD(period,7*timesteps_per_day) 
  END SUBROUTINE simulate_timesteps_to_monday

  INTEGER FUNCTION numdays_of_year(date)
    TYPE(datetype), INTENT(IN) :: date  !<date 
    IF(leapyear(date%year))THEN
      numdays_of_year=366
    ELSE
      numdays_of_year=365
    ENDIF
  END FUNCTION numdays_of_year

  LOGICAL FUNCTION LeapYear(iYear)
    INTEGER, INTENT(IN) :: iYear
    LeapYear = ((MOD(iYear, 4) .EQ. 0) .AND. ((MOD(iYear,100) .NE. 0) &
                .OR. (MOD(iYear, 400) .EQ. 0)))
  END FUNCTION LeapYear

  FUNCTION Date2Julian(ThisDate)
    TYPE(DateType), INTENT(IN) :: ThisDate
    TYPE(JulianDateType) :: Date2Julian, DumJulian
    REAL(KIND=8)            :: Dum
    REAL(KIND=8), PARAMETER :: Dum0 = 1720994.5
    INTEGER :: iiYear, iiMonth, iiDay
    INTEGER :: Century, E, F, D
    iiYear = ThisDate%Year
    iiMonth = ThisDate%Month
    iiDay = ThisDate%Day
    IF (iiMonth .LT. 3) THEN
      iiYear = iiYear -1
      iiMonth = iiMonth + 12
    ENDIF
    E = INT(365.0 * iiYear) + iiYear/4
    F = INT(30.6001 * (iiMonth + 1))
    Century = iiYear/100
    D = 2 - Century + Century/4
    Dum = iiDay + Dum0 + D + E + F
    DumJulian%Head = Dum
    DumJulian%Tail = ThisDate%Hour/24._dp + ThisDate%Minute/(24._dp*60._dp)
    IF (DumJulian%Tail.GE.0._dp) THEN
      DumJulian%Head = DumJulian%Head + 0._dp
      DumJulian%Tail = DumJulian%Tail - 0._dp
    ELSE
      DumJulian%Head = DumJulian%Head - 0._dp
      DumJulian%Tail = DumJulian%Tail + 0._dp
    ENDIF
    Date2Julian = DumJulian
  END FUNCTION Date2Julian

  LOGICAL FUNCTION EqualDates( Date1, Date2 )
  !>Check the equality of 2 dates and return a logical variable
    TYPE( DateType ), INTENT( IN ) :: Date1,Date2
    EqualDates = (Date1%Year  .EQ.Date2%Year  )       &
                  .AND.(Date1%Month .EQ.Date2%Month ) &
                  .AND.(Date1%Day   .EQ.Date2%Day   ) &
                  .AND.(Date1%Hour  .EQ.Date2%Hour  ) &
                  .AND.(Date1%Minute.EQ.Date2%Minute)
  END FUNCTION EqualDates

  LOGICAL FUNCTION LargerDates(Date1, Date2)
    TYPE(DateType), INTENT(IN) :: Date1,Date2
    TYPE(JulianDateType) :: Julian1, Julian2
    LOGICAL :: DumBool
    Julian1 = Date2Julian(Date1)
    Julian2 = Date2Julian(Date2)
    DumBool = (Julian1%Head .GT. Julian2%Head) &
               .OR.((NINT(Julian1%Head - Julian2%Head) .EQ. 0) &
               .AND.(Julian1%Tail .GT. Julian2%Tail))
    LargerDates = DumBool
  END FUNCTION LargerDates

  LOGICAL FUNCTION LargerEqualDates( Date1, Date2 )
  !>Check the equality of 2 dates and return a logical variable
    TYPE(DateType), INTENT(IN) :: Date1
    TYPE(DateType), INTENT(IN) :: Date2
    LargerEqualDates = (EqualDates(Date1,Date2).OR.LargerDates(Date1,Date2))
  END FUNCTION LargerEqualDates

  LOGICAL FUNCTION SmallerDates( Date1, Date2 )
    TYPE(DateType), INTENT(IN) :: Date1,Date2
    SmallerDates = LargerDates(Date2,Date1)
  END FUNCTION SmallerDates

  LOGICAL FUNCTION SmallerEqualDates( Date1, Date2 )
    TYPE( DateType ), INTENT( IN ) :: Date1,Date2
    SmallerEqualDates = (EqualDates(Date1,Date2).OR.SmallerDates(Date1,Date2))
  END FUNCTION SmallerEqualDates

  INTEGER FUNCTION DoY(t)
    TYPE(DateType), INTENT(IN) :: t
    INTEGER :: Dum
    INTEGER, DIMENSION(12), PARAMETER :: OffsetDays = (/0,31,59,90,120,151,181,212,243,273,304,334/)
    INTEGER, DIMENSION(12), PARAMETER :: LeapOffsetDays = (/0,31,60,91,121,152,182,213,244,274,305,335/)
    IF (LeapYear(t%Year)) THEN
      Dum = LeapOffsetDays(t%Month) + t%Day
    ELSE
      Dum = OffsetDays(t%Month) + t%Day
    ENDIF
    DOY = Dum
  END FUNCTION DoY

    LOGICAL FUNCTION in_season_period(begjd, period, cur_time)
     USE MODELTYPE, ONLY :  TIMEINFORMATIONTYPE
     INTEGER, INTENT(IN) :: begjd   !<julian day number of beginning of season
     INTEGER, INTENT(IN) :: period  !<length of period in days
     TYPE(TIMEINFORMATIONTYPE) :: cur_time
     INTEGER  ::  endjd
     Logical  ::  status
     endjd = begjd + period - 1  !including endjd in period, handles period=0
     status = .FALSE.
     IF(endjd > 365)THEN
       IF(cur_time%dayno .GE. begjd) status = .TRUE.
       IF(cur_time%dayno .LE. endjd - cur_time%prevdoy) status = .TRUE.
     ELSE
       IF(cur_time%dayno .GE. begjd .AND. cur_time%dayno .LE. endjd) status = .TRUE.
     ENDIF
     in_season_period = status
  END FUNCTION in_season_period
!####################################################################################################################
  LOGICAL FUNCTION in_season_end(begjd, endjd, cur_time)
    USE MODELTYPE, ONLY :  TIMEINFORMATIONTYPE
    INTEGER, INTENT(IN) :: begjd   !<julian day number of beginning of season
    INTEGER, INTENT(IN) :: endjd   !<julian day number of ending of season
    TYPE(TIMEINFORMATIONTYPE) :: cur_time
    LOGICAL             :: status
    INTEGER             :: endday
    endday = endjd
    IF(endjd < begjd) endday = endjd + 365
    status = .FALSE.
    IF(endday > 365)THEN
      IF(cur_time%dayno >= begjd) status = .TRUE.
      IF(cur_time%dayno <= endday - 365) status = .TRUE.
    ELSE
      IF(cur_time%dayno >= begjd .AND. cur_time%dayno <= endday) status = .TRUE.
    ENDIF
    in_season_end = status
  END FUNCTION in_season_end

END MODULE DATETIME