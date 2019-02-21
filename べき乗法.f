C*********************************
C  �ׂ���@�ɂ��s��̍ő�ŗL�l
C*********************************
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER(M=10)
      DIMENSION A(M,M),X(M),Y(M)
      WRITE(*,*)"�����s��̑傫������͂��Ă�������"
      READ(*,*)N
      DO 15 I=1,N
        DO 10 J=1,N
        WRITE(*,*) "�s��̗v�f����͂��Ă�������. A(",I,",",J,")?"
          READ(*,*) A(I,J)
   10   CONTINUE
      X(I) =0.0
   15 CONTINUE
      X(1) = 1.0
      WRITE(*,*) "�s��̗v�f"
      DO 20 I=1,N
        WRITE(*,600)(A(I,J),J=1,N)
   20 CONTINUE
  600 FORMAT(1H,6F11.5)
      IMAX = 200
      CALL POWER(A,M,N,X,XMAX,IMAX,Y)
      WRITE(*,*)'�ő�ŗL�l ',XMAX,'������ ',IMAX
      WRITE(*,*)'�ŗL�x�N�g��'
      WRITE(*,*)(X(I),I=1,N)
      STOP
      END

C**�ׂ���@�̃T�u���[�`���i�s��̐ς̌v�Z�Ȃǁj
      SUBROUTINE POWER(A,M,N,X,XMAX,IMAX,Y)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(M,M),X(M),Y(M)
      L=0
      XMAX=0.0
      S=0.0
      DO 10 I=1,N
        S = S + X(I)*X(I)
   10 CONTINUE
      DO 20 I=1,N
        X(I) = X(I)/DSQRT(S)
   20 CONTINUE
      DO 30 K=1,IMAX
        DO 40 I=1,N
          Y(I)= 0.0
          DO 50 J=1,N
            Y(I) = Y(I) +A(I,J)*X(J)
   50     CONTINUE
   40   CONTINUE
        S = 0.0
        DO 60 I=1,N
          S = S + Y(I)*Y(I)
   60 CONTINUE
      T=0.
      DO 70 I=1,N
        T = T+Y(I)*X(I)
   70 CONTINUE
      B = S/T
      L = K
      IF(DABS(XMAX-B).LT.1.D-6) GO TO 80
        XMAX = B
        DO 90 I=1,N
          X(I) = Y(I)/DSQRT(S)
   90   CONTINUE
   30 CONTINUE
   80 CONTINUE
      IMAX = L
      RETURN
      END