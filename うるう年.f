C**************************************************
C  ���͂��������l�̔N�����邤�N�����肷��v���O����
C**************************************************
      INTEGER INPUT
      LOGICAL ISURUU
      WRITE(*,*)"���肵�����N�̐����l����͂��Ă�������"
      READ(*,*)INPUT
      IF(ISURUU(INPUT)) THEN
        WRITE(*,*)"���͂��������͂��邤�N�ł��B"
      ELSE
        WRITE(*,*)"���͂��������͂��邤�N�ł͂���܂���B"
      END IF
      END PROGRAM 

      LOGICAL FUNCTION ISURUU(INPUT)
        INTEGER INPUT
        ISURUU = .FALSE.
        IF (MOD(INPUT,4).NE.0) THEN
          RETURN
        ELSE
        IF((MOD(INPUT,100).EQ.0).AND.(MOD(INPUT,400).NE.0))THEN
          RETURN
          ELSE
          ISURUU = .TRUE.
        END IF
        END IF
      END


