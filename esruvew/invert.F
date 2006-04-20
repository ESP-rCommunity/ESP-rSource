C---------------------------------------------------
      SUBROUTINE  INVERT
#include "viewer.h2"

      N = 0
      L1 = LIB
      DO 100 I = LIB,LIB1
      N = N + 1
      IF (ISIDOR(I) .GT. 0)  GOTO  100
      L2 = I - 1
      CALL  INV(N-1,L1,L2)
      N = 1
      L1 = I
  100 CONTINUE

      CALL  INV(N,L1,LIB1)

      RETURN
      END
C---------------------------------------------------
      SUBROUTINE  INV(N,L1,L2)
#include "viewer.h2"

      DO 100 I = 1,N/2
        J = L1 + I - 1
        K = L2 - I + 1
        IT = ISIDOR(J)
        ISIDOR(J) = ISIDOR(K)
        ISIDOR(K) = IT
  100 CONTINUE

      IF (ISIDOR(L2) .GT. 0)  RETURN

      ISIDOR(L1) = -ISIDOR(L1)
      ISIDOR(L2) = IABS(ISIDOR(L2))

      RETURN
      END
