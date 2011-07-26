C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains the following routines:
C   mzipct
C   mzibct
C   mziboc
C   mzintc
C   FZYFNC

C ***************** mzipct

C Mzipct assigns starting values to 
C common DIGPID in plant control loops 1 & 2.

      subroutine mzipct
#include "control.h"
      common/digpid/pid(MCF,7)

C Assign starting values.
      do 1 i=1,7
        do 2 icl=1,mcf
           pid(icl,i)=0.0
 2      continue
 1    continue
      return
      end

C ***************** mzibct

C Mzibct assigns starting values to 
C common BLGPID in building control function 5,

      subroutine mzibct
#include "building.h"
#include "control.h"
      common/blgpid/bpid(mcf,6),nsincr

C Assign starting values to BLGPID,
      do 1 i=1,6
        do 2 icf=1,mcf
           bpid(icf,i)=0.0
 2      continue
 1    continue
 
      return
      end

C ***************** mziboc

C Mziboc assigns starting values to common
C OPTCTL in building control functions 15 & 16.

      subroutine mziboc

      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone
      COMMON/OPTCTL/IRWIND,IRWLAS,IRWCNT,IRWCSV,IRWCHP,IRWCHN,IRWCHG,
     &LGCTIN,INITCT,STFUT,STPRES,STPAS1,STPAS2,IRWFNL,TCTLSV

      logical tsclaw,rewnd,pasone

C (Re)assign values for common OPTCTL,
        
      irwind=0
      irwlas=0
      irwcnt=0
      irwcsv=0
      irwchp=0
      irwchn=0
      irwchg=0
      irwfnl=0      
      lgctin=0
      initct=0
      stfut=0.
      stpres=0.
      stpas1=0.
      stpas2=0.

C Re-initialise value for common, TS6,
      irwcou=0

      return
      end

C ******************** mzintc

C Mzintc initialises TS6 variables.

      subroutine mzintc

      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

      logical tsclaw,rewnd,pasone

      idysv=0
      ihrsv=0
      irwcou=0
      nsncv1=0
      rewnd=.false.
      pasone=.false.
      
      return
      end

C *********************** SUBROUTINE FZYFNC
C This subroutine calculates the slopes and intercepts for
C all the fuzzy membership functions read in by 'FZDATA'.

      SUBROUTINE FZYFNC
#include "building.h"
#include "control.h"

      COMMON/FZCTL1/FZM(MFP,3,5,10),ERRSAV(MFP),QSAV(MFP),
     &ACUT(3,5),FZCTIP(5,5),FZCTOP(5),COA(5)
      COMMON/FZCTL2/NFZF(MFP,3),FZT(MFP,3),NONZRO

      IFP=0

      DO 40 K=1,NCF

       DO 45 KK=1,NBCDT(K)

        DO 50 KKK=1,NBCDP(K,KK)

         IF(IBCLAW(K,KK,KKK).EQ.17)THEN
           IFP=IFP+1
           FZT(IFP,1)=K
           FZT(IFP,2)=KK
           FZT(IFP,3)=KKK          

C Establish Slopes and Intercepts for each Membership Function. 
           DO 20 I=1,3
             DO 15 J=1,NFZF(IFP,I)

             IF(J.EQ.1.OR.J.EQ.NFZF(IFP,I))THEN
               IF(J.EQ.1)THEN
                 FZM(IFP,I,J,7)=0.
                 FZM(IFP,I,J,9)=FZSLOP(FZM(IFP,I,J,3),FZM(IFP,I,J,4),
     &                          FZM(IFP,I,J,5),FZM(IFP,I,J,6))
               ELSEIF(J.EQ.NFZF(IFP,I))THEN
                 FZM(IFP,I,J,7)=FZSLOP(FZM(IFP,I,J,1),FZM(IFP,I,J,2),
     &                          FZM(IFP,I,J,3),FZM(IFP,I,J,4))
                 FZM(IFP,I,J,9)=0.
               ENDIF      

               IF(J.EQ.1)THEN
                 FZM(IFP,I,J,8)=1.
                 FZM(IFP,I,J,10)=FZINCP(FZM(IFP,I,J,3),FZM(IFP,I,J,4),
     &                           FZM(IFP,I,J,5),FZM(IFP,I,J,6))
               ELSEIF(J.EQ.NFZF(IFP,I))THEN
                 FZM(IFP,I,J,8)=FZINCP(FZM(IFP,I,J,1),FZM(IFP,I,J,2),
     &                          FZM(IFP,I,J,3),FZM(IFP,I,J,4))
                 FZM(IFP,I,J,10)=1.
               ENDIF 

            ELSE

               FZM(IFP,I,J,7)=FZSLOP(FZM(IFP,I,J,1),FZM(IFP,I,J,2),
     &                        FZM(IFP,I,J,3),FZM(IFP,I,J,4))

               FZM(IFP,I,J,8)=FZINCP(FZM(IFP,I,J,1),FZM(IFP,I,J,2),
     &                        FZM(IFP,I,J,3),FZM(IFP,I,J,4))

               FZM(IFP,I,J,9)=FZSLOP(FZM(IFP,I,J,3),FZM(IFP,I,J,4),
     &                        FZM(IFP,I,J,5),FZM(IFP,I,J,6))

               FZM(IFP,I,J,10)=FZINCP(FZM(IFP,I,J,3),FZM(IFP,I,J,4),
     &                        FZM(IFP,I,J,5),FZM(IFP,I,J,6))

            ENDIF 

   15   CONTINUE
20    CONTINUE

         ENDIF
  50    CONTINUE
 45    CONTINUE
40    CONTINUE
      RETURN
      END

      FUNCTION FZSLOP(X1,Y1,X2,Y2)
      FZSLOP=(Y2-Y1)/(X2-X1)
      RETURN
      END
              
      FUNCTION FZINCP(X1,Y1,X2,Y2)
      FZINCP=((Y2*X1)-(Y1*X2))/(X1-X2)
      RETURN
      END
