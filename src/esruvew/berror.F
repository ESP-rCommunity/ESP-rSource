      SUBROUTINE BERROR(IE,IB)

      INCLUDE 'viewer.h2'
      COMMON/OUTIN/IUOUT,IUIN
      character outs *124

      NE = 0

      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),IE

   1  WRITE(IUOUT,101)mpt,IB
 101  FORMAT(' Maximum total points',i5,' exceeded in body',I4)

C DO NOT ALLOW FATAL ERROR TO PROCEED WITH COUNT
      GO TO 600

   2  WRITE(IUOUT,102)msf,IB
 102  FORMAT(' Maximum total surfaces',i5,' exceeded in body',I4)
      GO TO 600

   3  WRITE(IUOUT,103)med,IB
 103  FORMAT(' Maximum total edges',i5,' exceeded in body',I4)
      GO TO 600

C----------------------------------------------------------------
4     RETURN

5     RETURN

C------------------------------------------------------------------
6     GO TO 500

   7  WRITE(IUOUT,107)IB
 107  FORMAT(' MAX SURFACES PER BODY EXCEEDED IN BODY',I4)
      GO TO 500

   8  WRITE(IUOUT,108)IB
 108  FORMAT(' BAD POINT data in BODY',I4)
      GO TO 500

   9  WRITE(IUOUT,109)IB
 109  FORMAT(' BAD SURFACE data in BODY',I4)
      GO TO 500

  10  WRITE(IUOUT,110)IB
 110  FORMAT(' WARNING:BAD COMMAND after BODY',I4)
C***4.0  IGNORE BAD COMMAND,AND SUBSEQUENT,DONT SET ERROR
      RETURN

  11  WRITE(IUOUT,111)IB
 111  FORMAT(' MAX EDGES PER BODY EXCEEDED IN BODY',I4)
      GO TO 500

C-------------------------------------------------------------
12      WRITE(IUOUT,112)IB
112     FORMAT(' UNMATCHED EDGE in BODY',I4)
        GOTO 500

13        WRITE(IUOUT,113)IB
113       FORMAT(' WARNING: END-OF-FILE in BODY',I4,' ,IGNORED')
C CARRY ON
      RETURN

14    WRITE(IUOUT,114)
114   FORMAT(' NO BODIES in DATA FILE')
      GOTO 600

C MAX NR OF BODIES
  15  WRITE(IUOUT,115)IB
 115  FORMAT(' MAX NUMBER of BODIES EXCEEDED in FILE:',/,
     &       ' rest of FILE IGNORED!')
      GO TO 501

500       CONTINUE
          NE = 1
 501  RETURN

C----------------------------------------------------------
600   WRITE(outs,601)NB,NP,NF,NR
601   FORMAT('bodies:',i5,' points:',i5,' surfaces:',i5,' edges:',i5)
      call edisp(iuout,' ERROR(S) IN FILE - VIEWER abandoned at')
      call edisp(iuout,outs)
      CALL EPAGEND
      STOP
      END
