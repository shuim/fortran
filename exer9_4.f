      PROGRAM EXER_9_4
C     EXP(-0.7X)*COS(4X)ÇåvéZÇ∑ÇÈ X:0~5,0.05çèÇ›
      IMPLICIT REAL*8 (A-H,O-Z)
      F(X) = EXP(-0.7*X)*COS(4*X)
      WRITE(6,1000)'X','Y'
      X = 0.0
      DO 10 I=0,100
        WRITE(6,2000)X,F(X)
        X = X + 0.05
   10 CONTINUE

 1000 FORMAT(2A10)
 2000 FORMAT(2F10.3)
      END