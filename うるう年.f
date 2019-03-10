C**************************************************
C  入力した整数値の年がうるう年か判定するプログラム
C**************************************************
      INTEGER INPUT
      LOGICAL ISURUU
      WRITE(*,*)"判定したい年の整数値を入力してください"
      READ(*,*)INPUT
      IF(ISURUU(INPUT)) THEN
        WRITE(*,*)"入力した整数はうるう年です。"
      ELSE
        WRITE(*,*)"入力した整数はうるう年ではありません。"
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


