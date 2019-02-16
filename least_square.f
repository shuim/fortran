      PROGRAM LEASTSQUARE
      DIMENSION X(100),Y(100),S(100),T(100),A(20,20)
      WRITE(*,*)'何組のデータがありますか？'
      READ(*,*)M
      WRITE(*,*)'何次式で近似しますか？'
      READ(*,*)N
      WRITE(*,*)M,'組のデータを順に入力してください'
      DO 10 I=1,M
        WRITE(*,*)'X(',I,')=  '
        READ(*,*)  X(I)
        WRITE(*,*)'Y(',I,')=  '
        READ(*,*)  Y(I)
   10 CONTINUE
      WRITE(*,*) '確認のために入力データを出力します'
      DO 15 I=1,M
        WRITE(*,600) X(I),Y(I)
   15 CONTINUE
  600 FORMAT(1H ,2F15.6)
      DO 20 I=1, N+1
        SS= 0.
        TT= 0.
        DO 25 J=1,M
          XX = X(J)**(I-1)
          SS = SS + XX
          TT = TT + Y(J)*XX
   25   CONTINUE
        S(I) = SS
        T(I) = TT
   20 CONTINUE
      DO 30 I=N+2,2*N+1
        SS  =0.
        DO 35 J = 1, M
          SS = SS + X(J)**(I-1)
   35   CONTINUE
        S(I) = SS
   30 CONTINUE
C
C*** 掃き出し法による連立一次方程式の解法
C
      DO 40 I=1, N+1
        DO 45 J=1, N+1
          A(I,J) = S(I+J-1)
   45   CONTINUE
        A(I,N+2) = T(I)

   40 CONTINUE
      EPS = 1E-8
      DO 50 L = 1, N+1
        P = 0.0
        DO 55 I = L, N+1
          IF (P.GT.ABS(A(I,L))) GO TO 55
          P = ABS(A(I,L))
          K = I
   55   CONTINUE
        IF(P.LE.EPS) THEN
          WRITE(*,*) '---不定です---'
          STOP
        END IF
        DO 60 J = L, N+2
          A1 = A(L,J)
          A(L,J) = A(K,J)
          A(K,J) = A1
   60 CONTINUE
      DO 65 J = L+1,N+2
        A(L,J) = A(L,J)/A(L,L)
   65 CONTINUE
      DO 70 I = 1, N+2
        IF(I.EQ.L) GO TO 70
        DO 75 J = L+1, N+2
          A(I,J) = A(I,J) - A(I,L)*A(L,J)
   75     CONTINUE
   70   CONTINUE
   50 CONTINUE
      WRITE(*,*) '-----計算結果-----'
      DO 80 I = 1, N+1
        WRITE(*,*) 'A(',I-1,')=',A(I,N+2)
   80 CONTINUE
      STOP
      END PROGRAM LEASTSQUARE
