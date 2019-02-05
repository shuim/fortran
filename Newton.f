      PROGRAM NEWTON
      F(X)=COS(X)-X**2
      DF(X)=-SIN(X)-2*X
      
      EPS = 1.0E-8
      XOLD = 1.D0

C     ニュートン法収束のループ
      DO 100
      XNEW = XOLD-F(XOLD)/DF(XOLD)
      IF(ABS(XNEW-XOLD).LT.EPS) GOTO 110
      WRITE(*,*)"XOLD:",XOLD
      XOLD=XNEW
  100 CONTINUE
  110 WRITE(*,*)"収束値は",XNEW,"です"

      END PROGRAM NEWTON
