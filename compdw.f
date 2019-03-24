C     DATE TO DAY OF THE WEEK

      PROGRAM COMPDW

      CHARACTER WHATDF*9
      INTEGER YEAR,MONTH,DAY

   10 READ(5,*,END =20) YEAR,MONTH,DAY
        WRITE(6,*)
     +         'IT IS ',WHATDF(YEAR,MONTH,DAY)
      GOTO 10
   20 END

C     TO YEILD THE DAY OF THE WEEK AS CHAR. DATA

      CHARACTER*(*) FUNCTION WHATDF(Y,M,D)

      INTEGER Y,M,D, DAYOFW
      CHARACTER WEEK(0:7)*9

      DATA WEEK/ '???',       'Monday',  'Tuesday',
     +            'Wednesday', 'Thursday','Friday',
     +            'Saturday',  'Sunday' /

      WHATDF = WEEK(DAYOFW(Y,M,D))
      END

C     CONVERSION OF DATE TO A DAY OF THE WEEK

      INTEGER FUNCTION DAYOFW(Y,M,D)

      INTEGER Y,M,D, NOFDAY, DAYS(12), DY,DM
      LOGICAL LEAPYR

      DATA DAYS/ 0 , 31, 59, 90, 120, 151, 181, 212,
     +          243, 273, 304, 334/

      IF((Y.LT.1) .OR. (Y.GT.3000)) GOTO 90
      K = Y-1
      DY = 365 * K + (K/4)-(K/100)+(K/400)

      IF((D.LT.1).OR.(D.GT.NOFDAY(Y,M))) GOTO 90

      DM = DAYS(M)
      IF(LEAPYR(Y) .AND. (M.GT.2)) DM = DM +1

      DAYOFW = MOD((DY+DM+D)-1,7)+1
      RETURN

   90 DAYOFW = 0
      END

C     TO RETURN NUMBER OF DAYS IN A GIVEN MONTH
      INTEGER FUNCTION NOFDAY(Y,M)

      INTEGER Y,M, DAYM(12)
      LOGICAL LEAPYR

      DATA DAYM/ 31, 28, 31, 30, 31, 30, 31, 31,
     +           30, 31, 30 ,31/

      IF((1.LE.M) .AND. (M.LE.12)) THEN
          NOFDAY = DAYM(M)
          IF(LEAPYR(Y) .AND. (M.EQ.2)) NOFDAY = 29
        ELSE
          NOFDAY = 0
        END IF

      END

C     TEST ON LEAP YEAR

      LOGICAL FUNCTION LEAPYR(Y)

      INTEGER Y

      LEAPYR = .FALSE.
      IF(MOD(Y,4).NE.0) RETURN
      IF((MOD(Y,100).EQ.0).AND.
     +             (MOD(Y,400).NE.0)) RETURN
      LEAPYR = .TRUE.

      END
