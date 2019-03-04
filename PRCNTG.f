      PROGRAM PRCNTG

      CHARACTER NAME(20)*16
C     テキストではISUMをSUMとしていたが組み込み関数の名前と同じ
c     なので動作がおかしくなるので名称変更
      INTEGER V(20),ISUM,TOTAL,N
      REAL P(20)

      OPEN(1,FILE="input.dat")

      N=1
C     END = 20の意味を調べたいところ
   10 READ(1,*,END=20) NAME(N),V(N)
        N=N+1
      GOTO 10

   20 CLOSE(1)

      N=N-1
      TOTAL = ISUM(V,N)

      DO 30 I=1,N
        P(I) = REAL(V(I))/TOTAL*100
   30 CONTINUE

      DO 40 I=1,N
        WRITE(6,2000) NAME(I),V(I),P(I),'%'
 2000     FORMAT(1H ,A,I8,3X,F5.1,A)
   40 CONTINUE
      END

      INTEGER FUNCTION ISUM(A,N)
      INTEGER A(*),N
      ISUM=0
      DO 100 I=1,N
        ISUM=ISUM+A(I)
  100 CONTINUE
      END