      PROGRAM SELECTION_SORT

      INTEGER A(200),N
      READ(5,*)N
      READ(5,*)(A(I),I=1,N)
      CALL SORT(A,N)
      WRITE(6,2000)(A(I),I=1,N)
 2000 FORMAT(//(1H,5(I8:',')))
      END

      SUBROUTINE SORT(A,N)
      INTEGER A(*),N
      DO 20 K=1,N-1
        M=K
        DO 10 I=K+1,N
        IF(A(I).LT.A(M))M=I
   10 CONTINUE
      CALL EXCHNG(A(K),A(M))
   20 CONTINUE
      END

      SUBROUTINE EXCHNG(X1,X2)
      INTEGER X1,X2,TEMP
      TEMP = X1
      X1 = X2
      X2 = TEMP
      END
