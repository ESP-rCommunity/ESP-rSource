C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains subroutines for imposing momentum and turbulence 
C boundary conditions on blockages when the k-e model is active.
C The subroutines are named BLKBND* with the final letters representing 
C the equations being set up/ solved.


C ********************* BLKBNDU *********************
C Set boundary conditions for U velocity for blockages.

      SUBROUTINE BLKBNDU(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=3,6
        IST=max(3,IVCELLS(IV,ICFD,1))
        IFN=IVCELLS(IV,ICFD,2)
        JST=JVCELLS(IV,ICFD,1)
        JFN=JVCELLS(IV,ICFD,2)
        KST=KVCELLS(IV,ICFD,1)
        KFN=KVCELLS(IV,ICFD,2)

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.3) then
          JST=JVCELLS(IV,ICFD,2)+1
          JFN=JVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IV,ICFD,1)-1
          JFN=JVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.5) then
          KST=KVCELLS(IV,ICFD,2)+1
          KFN=KVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.6) then
          KST=KVCELLS(IV,ICFD,1)-1
          KFN=KVCELLS(IV,ICFD,1)-1
        endif

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

C Calculate distance from wall to next to wall grid point ($DELTA y sub p$).
C Then set source terms in adjacent cells. 
C TAU??: wall shear stress (+ive value means stress operates in axis direction)
C SP: source term, units are force/velocity
C A?=0. : disconnect cell from blocked cell.
C Blockage on South face of adjacent cell.
C Blockage on South face of adjacent cell.
              if (IFACE.eq.3) then
                Dface=YP(JJ)-YV(JJ)
                TMULT=TRBSRCU(II,JJ,KK,Dface)
                TAUSU(II,JJ,KK)=-TMULT*Uf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaNSU(II,KK)
                AS(II,JJ,KK)=0.

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                Dface=YV(JJ+1)-YP(JJ)
                TMULT=TRBSRCU(II,JJ,KK,Dface)
                TAUNU(II,JJ,KK)=-TMULT*Uf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaNSU(II,KK)
                AN(II,JJ,KK)=0.

C Blockage on Low face of adjacent cell.
              elseif (IFACE.eq.5) then
                Dface=ZP(KK)-ZW(KK)
                TMULT=TRBSRCU(II,JJ,KK,Dface)
                TAULU(II,JJ,KK)=-TMULT*Uf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaHLU(II,JJ)
                AL(II,JJ,KK)=0.

C Blockage on High face of adjacent cell.
              elseif (IFACE.eq.6) then
                Dface=ZW(KK+1)-ZP(KK)
                TMULT=TRBSRCU(II,JJ,KK,Dface)
                TAUHU(II,JJ,KK)=-TMULT*Uf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaHLU(II,JJ)
                AH(II,JJ,KK)=0.
              endif
 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END


C ********************* BLKBNDV *********************
C Set boundary conditions for V velocity for blockages.

      SUBROUTINE BLKBNDV(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=1,6
        if (IFACE.lt.3.or.IFACE.gt.4) then
          IST=IVCELLS(IV,ICFD,1)
          IFN=IVCELLS(IV,ICFD,2)
          JST=max(3,JVCELLS(IV,ICFD,1))
          JFN=JVCELLS(IV,ICFD,2)
          KST=KVCELLS(IV,ICFD,1)
          KFN=KVCELLS(IV,ICFD,2)

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
          if (IFACE.eq.1) then
            IST=IVCELLS(IV,ICFD,2)+1
            IFN=IVCELLS(IV,ICFD,2)+1
          elseif (IFACE.eq.2) then
            IST=IVCELLS(IV,ICFD,1)-1
            IFN=IVCELLS(IV,ICFD,1)-1
          elseif (IFACE.eq.5) then
            KST=KVCELLS(IV,ICFD,2)+1
            KFN=KVCELLS(IV,ICFD,2)+1
          elseif (IFACE.eq.6) then
            KST=KVCELLS(IV,ICFD,1)-1
            KFN=KVCELLS(IV,ICFD,1)-1
          endif

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
          do 10 II=IST,IFN
            do 20 JJ=JST,JFN
              do 30 KK=KST,KFN

C Calculate distance from wall to next to wall grid point ($DELTA y sub p$).
C Then set source terms in adjacent cells. 
C TAU??: wall shear stress (+ive value means stress operates in axis direction)
C SP: source term, units are force/velocity
C A?=0. : disconnect cell from blocked cell.
C Blockage on West face of adjacent cell.
                if (IFACE.eq.1) then
                  Dface=XP(II)-XU(II)
                  TMULT=TRBSRCV(II,JJ,KK,Dface)
                  TAUWV(II,JJ,KK)=-TMULT*Vf(II,JJ,KK)
                  SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaEWV(JJ,KK)
                  AW(II,JJ,KK)=0.

C Blockage on East face of adjacent cell.
                elseif (IFACE.eq.2) then
                  Dface=XU(II+1)-XP(II)
                  TMULT=TRBSRCV(II,JJ,KK,Dface)
                  TAUEV(II,JJ,KK)=-TMULT*Vf(II,JJ,KK)
                  SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaEWV(JJ,KK)
                  AE(II,JJ,KK)=0.

C Blockage on Low face of adjacent cell.
                elseif (IFACE.eq.5) then
                  Dface=ZP(KK)-ZW(KK)
                  TMULT=TRBSRCV(II,JJ,KK,Dface)
                  TAULV(II,JJ,KK)=-TMULT*Vf(II,JJ,KK)
                  SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaHLV(II,JJ)
                  AL(II,JJ,KK)=0.

C Blockage on High face of adjacent cell.
                elseif (IFACE.eq.6) then
                  Dface=ZW(KK+1)-ZP(KK)
                  TMULT=TRBSRCV(II,JJ,KK,Dface)
                  TAUHV(II,JJ,KK)=-TMULT*Vf(II,JJ,KK)
                  SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaHLV(II,JJ)
                  AH(II,JJ,KK)=0.
                endif
 30           continue
 20         continue
 10       continue
        endif
 40   continue

      RETURN
      END


C ********************* BLKBNDW *********************
C Set boundary conditions for W velocity for blockages.

      SUBROUTINE BLKBNDW(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=1,4
        IST=IVCELLS(IV,ICFD,1)
        IFN=IVCELLS(IV,ICFD,2)
        JST=JVCELLS(IV,ICFD,1)
        JFN=JVCELLS(IV,ICFD,2)
        KST=max(3,KVCELLS(IV,ICFD,1))
        KFN=KVCELLS(IV,ICFD,2)

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.1) then
          IST=IVCELLS(IV,ICFD,2)+1
          IFN=IVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.2) then
          IST=IVCELLS(IV,ICFD,1)-1
          IFN=IVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.3) then
          JST=JVCELLS(IV,ICFD,2)+1
          JFN=JVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IV,ICFD,1)-1
          JFN=JVCELLS(IV,ICFD,1)-1
        endif

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

C Calculate distance from wall to next to wall grid point ($DELTA y sub p$).
C Then set source terms in adjacent cells. 
C TAU??: wall shear stress (+ive value means stress operates in axis direction)
C SP: source term, units are force/velocity
C A?=0. : disconnect cell from blocked cell.
C Blockage on West face of adjacent cell.
              if (IFACE.eq.1) then
                Dface=XP(II)-XU(II)
                TMULT=TRBSRCW(II,JJ,KK,Dface)
                TAUWW(II,JJ,KK)=-TMULT*Wf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaEWW(JJ,KK)
                AW(II,JJ,KK)=0.

C Blockage on East face of adjacent cell.
              elseif (IFACE.eq.2) then
                Dface=XU(II+1)-XP(II)
                TMULT=TRBSRCW(II,JJ,KK,Dface)
                TAUEW(II,JJ,KK)=-TMULT*Wf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaEWW(JJ,KK)
                AE(II,JJ,KK)=0.

C Blockage on South face of adjacent cell.
              elseif (IFACE.eq.3) then
                Dface=YP(JJ)-YV(JJ)
                TMULT=TRBSRCW(II,JJ,KK,Dface)
                TAUSW(II,JJ,KK)=-TMULT*Wf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaNSW(II,KK)
                AS(II,JJ,KK)=0.

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                Dface=YV(JJ+1)-YP(JJ)
                TMULT=TRBSRCW(II,JJ,KK,Dface)
                TAUNW(II,JJ,KK)=-TMULT*Wf(II,JJ,KK)
                SP(II,JJ,KK)=SP(II,JJ,KK)-TMULT*AreaNSW(II,KK)
                AN(II,JJ,KK)=0.
              endif
 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END


C ********************* BLKBNDKE *********************
C Set boundary conditions for turbulent kinetic energy
C for blockages.

      SUBROUTINE BLKBNDKE(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM2/VolP(ntcelx,ntcely,ntcelz),
     &             VolU(ntcelx,ntcely,ntcelz),
     &             VolV(ntcelx,ntcely,ntcelz),
     &             VolW(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/WALLF/TAUEV(ntcelx,ntcely,ntcelz),
     1          TAUWV(ntcelx,ntcely,ntcelz),TAUEW(ntcelx,ntcely,ntcelz),
     2          TAUWW(ntcelx,ntcely,ntcelz),TAUNU(ntcelx,ntcely,ntcelz),
     3          TAUSU(ntcelx,ntcely,ntcelz),TAUNW(ntcelx,ntcely,ntcelz),
     4          TAUSW(ntcelx,ntcely,ntcelz),TAUHU(ntcelx,ntcely,ntcelz),
     5          TAULU(ntcelx,ntcely,ntcelz),TAUHV(ntcelx,ntcely,ntcelz),
     6          TAULV(ntcelx,ntcely,ntcelz)
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
      COMMON/SUSP/SUKD(ntcelx,ntcely,ntcelz),SPKD(ntcelx,ntcely,ntcelz)

      COMMON/CONST/GREAT,small,GRAV

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=1,6
        IST=IVCELLS(IV,ICFD,1)
        IFN=IVCELLS(IV,ICFD,2)
        JST=JVCELLS(IV,ICFD,1)
        JFN=JVCELLS(IV,ICFD,2)
        KST=KVCELLS(IV,ICFD,1)
        KFN=KVCELLS(IV,ICFD,2)

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.1) then
          IST=IVCELLS(IV,ICFD,2)+1
          IFN=IVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.2) then
          IST=IVCELLS(IV,ICFD,1)-1
          IFN=IVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.3) then
          JST=JVCELLS(IV,ICFD,2)+1
          JFN=JVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IV,ICFD,1)-1
          JFN=JVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.5) then
          KST=KVCELLS(IV,ICFD,2)+1
          KFN=KVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.6) then
          KST=KVCELLS(IV,ICFD,1)-1
          KFN=KVCELLS(IV,ICFD,1)-1
        endif

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

C Calculate distance from wall to next to wall grid point.
C Blockage on West face of adjacent cell.
              if (IFACE.eq.1) then
                Dface=XP(II)-XU(II)
                GENCOU=(ABS(TAUWV(II,JJ+1,KK)*Vf(II,JJ+1,KK))+
     &                  ABS(TAUWV(II,JJ,KK)*Vf(II,JJ,KK))+
     &                  ABS(TAUWW(II,JJ,KK+1)*Wf(II,JJ,KK+1))+
     &                  ABS(TAUWW(II,JJ,KK)*Wf(II,JJ,KK)))/(2.0*Dface)
                AW(II,JJ,KK)=0.

C Blockage on East face of adjacent cell.
              elseif (IFACE.eq.2) then
                Dface=XU(II+1)-XP(II)
                GENCOU=(ABS(TAUEV(II,JJ+1,KK)*Vf(II,JJ+1,KK))+
     &                  ABS(TAUEV(II,JJ,KK)*Vf(II,JJ,KK))+
     &                  ABS(TAUEW(II,JJ,KK+1)*Wf(II,JJ,KK+1))+
     &                  ABS(TAUEW(II,JJ,KK)*Wf(II,JJ,KK)))/(2.0*Dface)
                AE(II,JJ,KK)=0.

C Blockage on South face of adjacent cell.
              elseif (IFACE.eq.3) then
                Dface=YP(JJ)-YV(JJ)
                GENCOU=(ABS(TAUSU(II+1,JJ,KK)*Uf(II+1,JJ,KK))+
     &                  ABS(TAUSU(II,JJ,KK)*Uf(II,JJ,KK))+
     &                  ABS(TAUSW(II,JJ,KK+1)*Wf(II,JJ,KK+1))+
     &                  ABS(TAUSW(II,JJ,KK)*Wf(II,JJ,KK)))/(2.0*Dface)
                AS(II,JJ,KK)=0.

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                Dface=YV(JJ+1)-YP(JJ)
                GENCOU=(ABS(TAUNU(II+1,JJ,KK)*Uf(II+1,JJ,KK))+
     &                  ABS(TAUNU(II,JJ,KK)*Uf(II,JJ,KK))+
     &                  ABS(TAUNW(II,JJ,KK+1)*Wf(II,JJ,KK+1))+
     &                  ABS(TAUNW(II,JJ,KK)*Wf(II,JJ,KK)))/(2.0*Dface)
                AN(II,JJ,KK)=0.

C Blockage on Low face of adjacent cell.
              elseif (IFACE.eq.5) then
                Dface=ZP(KK)-ZW(KK)
                GENCOU=(ABS(TAULU(II+1,JJ,KK)*Uf(II+1,JJ,KK))+
     &                  ABS(TAULU(II,JJ,KK)*Uf(II,JJ,KK))+
     &                  ABS(TAULV(II,JJ+1,KK)*Vf(II,JJ+1,KK))+
     &                  ABS(TAULV(II,JJ,KK)*Vf(II,JJ,KK)))/(2.0*Dface)
                AL(II,JJ,KK)=0.

C Blockage on High face of adjacent cell.
              elseif (IFACE.eq.6) then
                Dface=ZW(KK+1)-ZP(KK)
                GENCOU=(ABS(TAUHU(II+1,JJ,KK)*Uf(II+1,JJ,KK))+
     &                  ABS(TAUHU(II,JJ,KK)*Uf(II,JJ,KK))+
     &                  ABS(TAUHV(II,JJ+1,KK)*Vf(II,JJ+1,KK))+
     &                  ABS(TAUHV(II,JJ,KK)*Vf(II,JJ,KK)))/(2.0*Dface)
                AH(II,JJ,KK)=0.
              endif

C Calculate and set source terms in adjacent cells if inside domain.
              if (II.gt.1.and.JJ.gt.1.and.KK.gt.1.and.
     &            II.lt.NI.and.JJ.lt.NJ.and.KK.lt.NK) then
                SQRTK=sqrt(abs(TEf(II,JJ,KK)))
                Tvis=VIS(II,JJ,KK)-VISCOS+small
                Dplus=DENf(II,JJ,KK)*SQRTK*(CMU**0.25)*Dface/VISCOS
                call VELDERIV(II,JJ,KK,DUDY,DUDZ,DVDX,DVDZ,DWDX,DWDY)
                GENRES=GEN(II,JJ,KK)-Tvis*((DUDY+DVDX)**2+
     &                                   (DUDZ+DWDX)**2+(DVDZ+DWDY)**2)
                GEN(II,JJ,KK)=GENRES+GENCOU
                if (Dplus.gt.11.63) then
                  DITERM=CD*DENf(II,JJ,KK)*(CMU**.75)*SQRTK*
     &                                   ALOG(ELOG*Dplus)/(CAPPA*Dface)
                else
                  DITERM=CD*DENf(II,JJ,KK)*(CMU**.75)*SQRTK*Dplus/Dface
                endif
                SU(II,JJ,KK)=GEN(II,JJ,KK)*VolP(II,JJ,KK)+SUKD(II,JJ,KK)
                SP(II,JJ,KK)=-DITERM*VolP(II,JJ,KK)+SPKD(II,JJ,KK)
              endif
 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END


C ********************* BLKBNDED *********************
C Set boundary conditions for the dissipation of turbulent
C kinetic energy due to solid face for blockages.

      SUBROUTINE BLKBNDED(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)

      COMMON/CONST/GREAT,small,GRAV

      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C Set values for all faces.
      do 40 IFACE=1,6
        IST=IVCELLS(IV,ICFD,1)
        IFN=IVCELLS(IV,ICFD,2)
        JST=JVCELLS(IV,ICFD,1)
        JFN=JVCELLS(IV,ICFD,2)
        KST=KVCELLS(IV,ICFD,1)
        KFN=KVCELLS(IV,ICFD,2)

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.1) then
          IST=IVCELLS(IV,ICFD,2)+1
          IFN=IVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.2) then
          IST=IVCELLS(IV,ICFD,1)-1
          IFN=IVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.3) then
          JST=JVCELLS(IV,ICFD,2)+1
          JFN=JVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IV,ICFD,1)-1
          JFN=JVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.5) then
          KST=KVCELLS(IV,ICFD,2)+1
          KFN=KVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.6) then
          KST=KVCELLS(IV,ICFD,1)-1
          KFN=KVCELLS(IV,ICFD,1)-1
        endif

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through whole blockage.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

C Calculate distance from 
C next to wall grid point and disconnect cell from blockage (A?=0.)
C Blockage on West face of adjacent cell.
              if (IFACE.eq.1) then
                Dface=XP(II)-XU(II)
                AW(II,JJ,KK)=0.

C Blockage on East face of adjacent cell.
              elseif (IFACE.eq.2) then
                Dface=XU(II+1)-XP(II)
                AE(II,JJ,KK)=0.

C Blockage on South face of adjacent cell.
              elseif (IFACE.eq.3) then
                Dface=YP(JJ)-YV(JJ)
                AS(II,JJ,KK)=0.

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                Dface=YV(JJ+1)-YP(JJ)
                AN(II,JJ,KK)=0.

C Blockage on Low face of adjacent cell.
              elseif (IFACE.eq.5) then
                Dface=ZP(KK)-ZW(KK)
                AL(II,JJ,KK)=0.

C Blockage on High face of adjacent cell.
              elseif (IFACE.eq.6) then
                Dface=ZW(KK+1)-ZP(KK)
                AH(II,JJ,KK)=0.
              endif

C Calculate and set source terms.
              TERM=(CMU**.75)/(CAPPA*Dface)
              SU(II,JJ,KK)=GREAT*TERM*ABS(TEf(II,JJ,KK))**1.5
              SP(II,JJ,KK)=-GREAT
 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END


C ********************* BLKBNDT *********************
C Set boundary conditions for the temperature/ heat flux 
C due to solid face for blockages.
C Different behaviour depending on IVTYPE(IV,ICFD):
C 30 - VOLHeat is the prescribed heat flux (W/m^2) at each face.
C 31 - VOLTemp is the prescribed temperature at each face.
C 32 - Temperature taken from building surface, already assigned to
C      VOLTemp.
C 33 - Temperature taken from building surfaces, currently in BLKTEMP.
C 34 - Flux from casual gain, already assigned to VOLHeat (W).
C 35 - Dynamic person model coupling.

      SUBROUTINE BLKBNDT(IV)
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/GEOM3/AreaHLP(ntcelx,ntcely),AreaHLU(ntcelx,ntcely),
     &             AreaHLV(ntcelx,ntcely),AreaEWP(ntcely,ntcelz),
     &             AreaEWV(ntcely,ntcelz),AreaEWW(ntcely,ntcelz),
     &             AreaNSP(ntcelx,ntcelz),AreaNSU(ntcelx,ntcelz),
     &             AreaNSW(ntcelx,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
      COMMON/COEF/AP(ntcelx,ntcely,ntcelz),AE(ntcelx,ntcely,ntcelz),
     1            AW(ntcelx,ntcely,ntcelz),AN(ntcelx,ntcely,ntcelz),
     2            AS(ntcelx,ntcely,ntcelz),AH(ntcelx,ntcely,ntcelz),
     3            AL(ntcelx,ntcely,ntcelz),SU(ntcelx,ntcely,ntcelz),
     4            SP(ntcelx,ntcely,ntcelz)
C      COMMON/ITERAT/NITER
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)

C These commons pass the environmnetal parameters around each body segment
C to the physiology model.
      real headflux1,trunkflux1,larmflux1,rarmflux1,llegflux1
      real rlegflux1,H2Oexp1,H2Oswt1,Qoccsens1,Qocclat1
C       real COflux,Tsko_av1,Tco_av1,Tsk_av1
      COMMON/ocflux1/headflux1(MNZ),trunkflux1(MNZ),larmflux1(MNZ),
     &  rarmflux1(MNZ),llegflux1(MNZ),rlegflux1(MNZ),H2Oexp1(MNZ),
     &  H2Oswt1(MNZ),Qoccsens1(MCOM),Qocclat1(MCOM)
C         COflux,Tsko_av1,Tco_av1,Tsk_av1
      common/occload/sklat(MCOM),skcon(MCOM),skrad(MCOM),ttcr(MCOM),
     &     ttsk(MCOM)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ
      LOGICAL INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,IZEROT,IZanKE
      LOGICAL IMITZ

      logical skip

      CDTERM=CMU**0.25

C Calculate total surface area.
      areatot=2*(YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))*
     &          (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))+
     &        2*(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &          (ZW(KVCELLS(IV,ICFD,2)+1)-ZW(KVCELLS(IV,ICFD,1)))+
     &        2*(XU(IVCELLS(IV,ICFD,2)+1)-XU(IVCELLS(IV,ICFD,1)))*
     &          (YV(JVCELLS(IV,ICFD,2)+1)-YV(JVCELLS(IV,ICFD,1)))

C Set boundary conditions for all faces.
      do 40 IFACE=1,6
        IST=IVCELLS(IV,ICFD,1)
        IFN=IVCELLS(IV,ICFD,2)
        JST=JVCELLS(IV,ICFD,1)
        JFN=JVCELLS(IV,ICFD,2)
        KST=KVCELLS(IV,ICFD,1)
        KFN=KVCELLS(IV,ICFD,2)

c      write(*,*)'UP_istfn,jstfn,kstfn=',IST,IFN,JST,JFN,KST,KFN

C Values are set for cell adjacent to blockage.  Adjust values of I,J,K 
C to adjacent cell in all directions and set start and end cell to same 
C cell for the current face. 
        if (IFACE.eq.1) then
          IST=IVCELLS(IV,ICFD,2)+1
          IFN=IVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.2) then
          IST=IVCELLS(IV,ICFD,1)-1
          IFN=IVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.3) then
          JST=JVCELLS(IV,ICFD,2)+1
          JFN=JVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.4) then
          JST=JVCELLS(IV,ICFD,1)-1
          JFN=JVCELLS(IV,ICFD,1)-1
        elseif (IFACE.eq.5) then
          KST=KVCELLS(IV,ICFD,2)+1
          KFN=KVCELLS(IV,ICFD,2)+1
        elseif (IFACE.eq.6) then
          KST=KVCELLS(IV,ICFD,1)-1
          KFN=KVCELLS(IV,ICFD,1)-1
        endif

c      write(*,*)'LO_istfn,jstfn,kstfn=',IST,IFN,JST,JFN,KST,KFN

C Skip if outside domain.
        if (IST.eq.1.or.IFN.eq.NI.or.JST.eq.1.or.JFN.eq.NJ.or.
     &      KST.eq.1.or.KFN.eq.NK) goto 40

C Loop through cells at this face.
        do 10 II=IST,IFN
          do 20 JJ=JST,JFN
            do 30 KK=KST,KFN

c      write(*,*)'L2=i.,j.,k.,IFACE',IST,IFN,JST,JFN,KST,KFN,IFACE 

C Calculate distance from wall to next to wall grid point.
C Get surface area of face and disconnect cell from blockage (A?=0.)
C Blockage on West face of adjacent cell.
              if (IFACE.eq.1) then
                AREA=AreaEWP(JJ,KK)
                AW(II,JJ,KK)=0.
                DW=XP(II)-XU(II)

C Blockage on East face of adjacent cell.
              elseif (IFACE.eq.2) then
                AREA=AreaEWP(JJ,KK)
                AE(II,JJ,KK)=0.
                DW=XU(II+1)-XP(II)

C Blockage on South face of adjacent cell.
              elseif (IFACE.eq.3) then
                AREA=AreaNSP(II,KK)
                AS(II,JJ,KK)=0.
                DW=YP(JJ)-YV(JJ)

C Blockage on North face of adjacent cell.
              elseif (IFACE.eq.4) then
                AREA=AreaNSP(II,KK)
                AN(II,JJ,KK)=0.
                DW=YV(JJ+1)-YP(JJ)

C Blockage on Low face of adjacent cell.
              elseif (IFACE.eq.5) then
                AREA=AreaHLP(II,JJ)
                AL(II,JJ,KK)=0.
                DW=ZP(KK)-ZW(KK)

C Blockage on High face of adjacent cell.
              elseif (IFACE.eq.6) then
                AREA=AreaHLP(II,JJ)
                AH(II,JJ,KK)=0.
                DW=ZW(KK+1)-ZP(KK)
              endif

C Set combined convective/ diffusive flux terms.

C Impose prescribed heat flux.
              if (IVTYPE(IV,ICFD).eq.30) then
                SU(II,JJ,KK)=SU(II,JJ,KK)+VOLHeat(IV,ICFD)*AREA/SH

C We have a prescribed temperature.
C Calculate heat flux using log-law wall functions.
              elseif (IVTYPE(IV,ICFD).eq.31 .or.
     &                IVTYPE(IV,ICFD).eq.32 .or.
     &                IVTYPE(IV,ICFD).eq.33) then
                if (IVTYPE(IV,ICFD).eq.33) then
                  if (BLKSURF(IV,ICFD,iface)(1:4).eq.'NONE') then
                    skip=.true.
                  else
                    VOLTemp(IV,ICFD)=BLKTEMP(IV,iface)
                    skip=.false.
                  endif
                endif

                if (.not.skip) then
                  if (INCALK.or.IMITZ) then
                    YPLUS=DENf(II,JJ,KK)*
     &                SQRT(ABS(TEf(II,JJ,KK)))*CDTERM*DW/VISCOS
                    IF(YPLUS.GT.11.63) THEN

C y+ is within log-law region. UPLUS is u+; ELOG is `E';
C CAPPA is the von Karmann constant. GT is an intermediate
C quantity and includes the value of T+ in its denominator.
                      UPLUS=ALOG(ELOG*YPLUS)/CAPPA
                      GT=DENf(II,JJ,KK)*CDTERM*
     &                  SQRT(ABS(TEf(II,JJ,KK)))/(PRANDT*(UPLUS+PFUN))
                    ELSE

C y+ is within laminar sub-layer.
                      GT=VISCOS/(PRANDL*DW)
                    ENDIF
                  else

C We have laminar conditions.
                    GT=VISCOS/(PRANDL*DW)
                  endif

C Now calculate the source terms (V&M eq.9.13 & eq.9.24).
                  TERM=GT*AREA
                  SU(II,JJ,KK)=SU(II,JJ,KK)+TERM*VOLTemp(IV,ICFD)
                  SP(II,JJ,KK)=SP(II,JJ,KK)-TERM
                endif

C Flux from casual gain.
C Gains (W) have already been put in VOLHEAT in CFCALC.
              elseif (IVTYPE(IV,ICFD).eq.34) then
                areafrac=area/areatot ! distribute flux by face area
                SU(II,JJ,KK)=SU(II,JJ,KK)+VOLHeat(IV,ICFD)*areafrac/SH

C Dynamic person.
C Person type and body part are stored in BLKSURF.
              elseif (IVTYPE(IV,ICFD).eq.35) then
                if (BLKSURF(IV,ICFD,2)(1:4).eq.'head')then

C Adjust for gender, as in casual.F.
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=headflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=headflux1(ICFD)*0.75
                  else
                    pflux=headflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                Elseif (BLKSURF(IV,ICFD,2)(1:5).eq.'trunk')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=trunkflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=trunkflux1(ICFD)*0.75
                  else
                    pflux=trunkflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                Elseif(BLKSURF(IV,ICFD,2)(1:8).eq.'left_arm')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=larmflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=larmflux1(ICFD)*0.75
                  else
                    pflux=larmflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                Elseif(BLKSURF(IV,ICFD,2)(1:9).eq.'right_arm')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=rarmflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=rarmflux1(ICFD)*0.75
                  else
                    pflux=rarmflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                Elseif(BLKSURF(IV,ICFD,2)(1:8).eq.'left_leg')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=llegflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=llegflux1(ICFD)*0.75
                  else
                    pflux=llegflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                Elseif(BLKSURF(IV,ICFD,2)(1:9).eq.'right_leg')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=rlegflux1(ICFD)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=rlegflux1(ICFD)*0.75
                  else
                    pflux=rlegflux1(ICFD)
                  endif
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*AREA/SH

                ElseIf(BLKSURF(IV,ICFD,2)(1:10).eq.'whole_body')then
                  if (BLKSURF(IV,ICFD,1)(1:5).eq.'woman') then
                    pflux=skcon(ICP)*0.85
                  elseif (BLKSURF(IV,ICFD,1)(1:5).eq.'child') then
                    pflux=skcon(ICP)*0.75
                  else
                    pflux=skcon(ICP)
                  endif
                  areafrac=area/areatot ! distribute flux by face area
                  SU(II,JJ,KK)=SU(II,JJ,KK)+pflux*areafrac/SH

                endif
              endif
 30         continue
 20       continue
 10     continue
 40   continue

      RETURN
      END


C ********************* BLKSETT *********************
C Set temperatures inside blockages. Also calculate local conditions for
C the segmented person model.
C TODO: in future we could modify the two-node model to also use local
C conditions from the CFD.

      SUBROUTINE BLKSETT

#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &  JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &  VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &  VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &  VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),VOLPol(MCTM,MNVLS,MNZ)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &  BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &  INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      COMMON/CFDBCOFF/BCOFF(MNVLS)
      logical BCOFF
      common/dynamico/isdynamicocup(MCOM)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     &  RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN 
C       COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
C      &  Wf(ntcelx,ntcely,ntcelz),P(ntcelx,ntcely,ntcelz),
C      &  PP(ntcelx,ntcely,ntcelz),TEf(ntcelx,ntcely,ntcelz),
C     &  EDf(ntcelx,ntcely,ntcelz)
      REAL tavhead1,tavtrunk1,tavarml1,tavarmr1,tavlegl1,tavlegr1
      real hrhead,hrtrunk,hrarml,hrarmr,hrlegl,hrlegr,HMETA2
      integer cfdcheck, msegflag
      COMMON/segtemp1/tavhead1(MNZ),tavtrunk1(MNZ),tavarml1(MNZ),
     &  tavarmr1(MNZ),tavlegl1(MNZ),tavlegr1(MNZ),hrhead(MNZ),
     &  hrtrunk(MNZ),hrarml(MNZ),hrarmr(MNZ),hrlegl(MNZ),hrlegr(MNZ),
     &  cfdcheck(MNZ),msegflag(MNZ),HMETA2(MNZ)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     &              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALLMA,CALPOL
      CHARACTER POLNAM*12

      logical blkd,LINVOL

C Loop through blockages.
      do iblk=1,NBLK(ICFD)
        iv=INBLK(iblk,ICFD)
        if (BCOFF(iv)) cycle

        ist=IVCELLS(IV,ICFD,1)
        ifn=IVCELLS(IV,ICFD,2)
        jst=JVCELLS(IV,ICFD,1)
        jfn=JVCELLS(IV,ICFD,2)
        kst=KVCELLS(IV,ICFD,1)
        kfn=KVCELLS(IV,ICFD,2)

C If we have prescribed flux, we don't know the temperature of the
C blockage. Get local air temperature at the top face as a rough
C estimate. Note that this might behave a little strangely if blockages
C are stacked on top of one another, locking temperatures to the initial
C assumption, but this is not catastrophic.
        if (IVTYPE(IV,ICFD).eq.30 .or. IVTYPE(IV,ICFD).eq.34 .or. 
     &      IVTYPE(IV,ICFD).eq.35) then
          temp=Tf((ist+ifn)/2,(jst+jfn)/2,kfn+1)

C If we have a coupled surface for each face, we need the average of
C these temperatures.
        elseif (IVTYPE(IV,ICFD).eq.33) then
          n=0
          temp=0.0
          do ii=1,6
            if (BLKSURF(IV,ICFD,ii)(1:4).ne.'NONE') then
              n=n+1
              temp=temp+BLKTEMP(IV,ii)
            endif
          enddo
          temp=temp/REAL(n)
        endif

C Initialise variables for average local conditions.
        if (IVTYPE(IV,ICFD).eq.35) then
          iavt=0; iavh=0
C          iavv=0
          xavt=0.0; xavh=0.0
C          xavv=0.0
        endif

C Loop through whole blockage and set internal temperature.
        do i=ist,ifn
          do j=jst,jfn
            do k=kst,kfn

C Prescribed heat flux, so we don't know the temperature. Use the
C estimate at the top face.
              if (IVTYPE(IV,ICFD).eq.30) then
                Tf(i,j,k)=temp

C Prescribed temperature.
              elseif (IVTYPE(IV,ICFD).eq.31) then
                Tf(i,j,k)=VOLTEMP(IV,ICFD)

C Temperature from building surface.
              elseif (IVTYPE(IV,ICFD).eq.32) then
                Tf(i,j,k)=VOLTEMP(IV,ICFD)

C Temperature from building surfaces. Set for each face, use average for
C everything else.
              elseif (IVTYPE(IV,ICFD).eq.33) then
                if (i.eq.ifn .and. 
     &              BLKSURF(IV,ICFD,1)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,1)
                elseif (i.eq.ist .and. 
     &                  BLKSURF(IV,ICFD,2)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,2)
                elseif (j.eq.jfn .and. 
     &                  BLKSURF(IV,ICFD,3)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,3)
                elseif (j.eq.jst .and. 
     &                  BLKSURF(IV,ICFD,4)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,4)
                elseif (k.eq.kfn .and. 
     &                  BLKSURF(IV,ICFD,5)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,5)
                elseif (k.eq.kst .and. 
     &                  BLKSURF(IV,ICFD,6)(1:4).ne.'NONE') then
                  Tf(i,j,k)=BLKTEMP(IV,6)
                else
                  TF(i,j,k)=temp
                endif

C Flux from casual gain. Use estimate at top face.
              elseif (IVTYPE(IV,ICFD).eq.34) then
                Tf(i,j,k)=temp

C Dynamic person coupling. Use estimate at top face. In future could
C consider taking skin temperature from the various models, but this
C seems unecessary simply for visualisation.
              elseif (IVTYPE(IV,ICFD).eq.35) then
                Tf(i,j,k)=temp
                
C Calculate average local conditions for dynamic person model.
C Take average of all available cells at blockage faces.
C Air velocity currently not used, code left commented just in case.
                if (i.eq.ist) then
                  ii=i-1
                  if (ii.ge.2) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(ii,j,k,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(ii,j,k)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),ii,j,k)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Uf(ii,j,k)**2+
    !  &                  ((Vf(ii,j,k)+Vf(ii,j+1,k))/2)**2+
    !  &                  ((Wf(ii,j,k)+Wf(ii,j,k+1))/2)**2)
                    endif
                  endif
                endif
                if (i.eq.ifn) then
                  ii=i+1
                  if (ii.le.NIM1) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(ii,j,k,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(ii,j,k)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),ii,j,k)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Uf(ii,j,k)**2+
    !  &                  ((Vf(ii,j,k)+Vf(ii,j+1,k))/2)**2+
    !  &                  ((Wf(ii,j,k)+Wf(ii,j,k+1))/2)**2)
                    endif
                  endif
                endif
                if (j.eq.jst) then
                  jj=j-1
                  if (jj.ge.2) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(i,jj,k,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(i,jj,k)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),i,jj,k)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Vf(i,jj,k)**2+
    !  &                  ((Uf(i,jj,k)+Uf(i+1,jj,k))/2)**2+
    !  &                  ((Wf(i,jj,k)+Wf(i,jj,k+1))/2)**2)
                    endif
                  endif
                endif
                if (j.eq.jfn) then
                  jj=j+1
                  if (jj.le.NJM1) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(i,jj,k,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(i,jj,k)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),i,jj,k)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Vf(i,jj,k)**2+
    !  &                  ((Uf(i,jj,k)+Uf(i+1,jj,k))/2)**2+
    !  &                  ((Wf(i,jj,k)+Wf(i,jj,k+1))/2)**2)
                    endif
                  endif
                endif
                if (k.eq.kst) then
                  kk=k-1
                  if (kk.ge.2) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(i,j,kk,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(i,j,kk)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),i,j,kk)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Wf(i,j,kk)**2+
    !  &                  ((Uf(i,j,kk)+Uf(i+1,j,kk))/2)**2+
    !  &                  ((Vf(i,j,kk)+Vf(i,j+1,kk))/2)**2)
                    endif
                  endif
                endif
                if (k.eq.kfn) then
                  kk=k+1
                  if (kk.le.NKM1) then
                    blkd=.false.
                    do iiblk=1,NBLK(ICFD)
                      if (iiblk.eq.iblk) cycle
                      iiv=INBLK(iiblk,ICFD)
                      if (BCOFF(iiv)) cycle 
                      blkd=LINVOL(i,j,kk,
     &                  IVCELLS(iiv,ICFD,1),IVCELLS(iiv,ICFD,2),
     &                  JVCELLS(iiv,ICFD,1),JVCELLS(iiv,ICFD,2),
     &                  KVCELLS(iiv,ICFD,1),KVCELLS(iiv,ICFD,2))
                      if (blkd) exit
                    enddo
                    if (.not.blkd) then
                      iavt=iavt+1; xavt=xavt+Tf(i,j,kk)
                      iavh=iavh+1
                      xavh=xavh+POLCONCf(JHUMINDX(ICFD),i,j,kk)
    !                   iavv=iavv+1
    !                   xavv=SQRT(Wf(i,j,kk)**2+
    !  &                  ((Uf(i,j,kk)+Uf(i+1,j,kk))/2)**2+
    !  &                  ((Vf(i,j,kk)+Vf(i,j+1,kk))/2)**2)
                    endif
                  endif
                endif                  
              endif
            enddo
          enddo
        enddo
        
        if (isdynamicocup(ICP).eq.4 .and. IVTYPE(IV,ICFD).eq.35) then
          If (BLKSURF(IV,ICFD,2)(1:4).eq.'head') then
            tavhead1(ICFD)=xavt/REAL(iavt)
            hrhead(ICFD)=xavh/REAL(iavh)
            ! Velhead1=xavv/REAL(iavv)
          elseif (BLKSURF(IV,ICFD,2)(1:5).eq.'trunk') then
            tavtrunk1(ICFD)=xavt/REAL(iavt)
            hrtrunk(ICFD)=xavh/REAL(iavh)
            ! Veltrunk1=xavv/REAL(iavv)
          elseif (BLKSURF(IV,ICFD,2)(1:9).eq.'right_arm') then
            tavarmr1(ICFD)=xavt/REAL(iavt)
            hrarmr(ICFD)=xavh/REAL(iavh)
            ! Velarmr1=xavv/REAL(iavv)
          elseif (BLKSURF(IV,ICFD,2)(1:8).eq.'left_arm') then
            tavarml1(ICFD)=xavt/REAL(iavt)
            hrarml(ICFD)=xavh/REAL(iavh)
            ! Velarml1=xavv/REAL(iavv)
          elseif (BLKSURF(IV,ICFD,2)(1:9).eq.'right_leg') then
            tavlegr1(ICFD)=xavt/REAL(iavt)
            hrlegr(ICFD)=xavh/REAL(iavh)
            ! Vellegr1=xavv/REAL(iavv)
          elseif (BLKSURF(IV,ICFD,2)(1:8).eq.'left_leg') then
            tavlegl1(ICFD)=xavt/REAL(iavt)
            hrlegl(ICFD)=xavh/REAL(iavh)
            ! Vellegl1=xavv/REAL(iavv)
          endif
          cfdcheck(ICFD)=2
        endif
      enddo

      RETURN
      END

C ********************* TRBSRCU *********************
C Stress/velocity at next-to-wall point for solid BCs, U-vel dir.

      FUNCTION TRBSRCU(I,J,K,Dface)
#include "building.h"
#include "cfd.h"
      
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
     
      CDTERM=CMU**0.25

C Calculate y+ by interpolation.
      YPLUSm1=DENf(I-1,J,K)*SQRT(ABS(TEf(I-1,J,K)))*CDTERM*Dface/VISCOS
      YPLUS  =DENf(I,J,K) * SQRT(ABS(TEf(I,J,K))) * CDTERM*Dface/VISCOS
      YPLUSA=SIFW(I)*YPLUS+SIFE(I-1)*YPLUSm1
      
C Calculate stress/velocity at next-to-wall point. Calculation depends upon
C whether we are in laminar sub-layer or log-law region.
      if (YPLUSA.GT.11.63) then

C Interpolate values of k and density.
        SQRTK=SQRT(ABS(SIFW(I)*TEf(I,J,K)+SIFE(I-1)*TEf(I-1,J,K)))
        DEN=SIFW(I)*DENf(I,J,K)+SIFE(I-1)*DENf(I-1,J,K)
        TRBSRCU=(DEN*CDTERM*SQRTK*CAPPA)/ALOG(ELOG*YPLUSA)
      else
        TRBSRCU=VISCOS/Dface
      endif
      
      return
      end
      
C ********************* TRBSRCV *********************
C Stress/velocity at next-to-wall point for solid BCs, V-vel dir.

      FUNCTION TRBSRCV(I,J,K,Dface)
#include "building.h"
#include "cfd.h"
      
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
     
      CDTERM=CMU**0.25

C Calculate y+ by interpolation.
      YPLUSm1=DENf(I,J-1,K)*SQRT(ABS(TEf(I,J-1,K)))*CDTERM*Dface/VISCOS
      YPLUS  =DENf(I,J,K) * SQRT(ABS(TEf(I,J,K))) * CDTERM*Dface/VISCOS
      YPLUSA=SIFS(J)*YPLUS+SIFN(J-1)*YPLUSm1
      
C Calculate stress/velocity at next-to-wall point. Calculation depends upon
C whether we are in laminar sub-layer or log-law region.
      if (YPLUSA.GT.11.63) then

C Interpolate values of k and density.
        SQRTK=SQRT(ABS(SIFS(J)*TEf(I,J,K)+SIFN(J-1)*TEf(I,J-1,K)))
        DEN=SIFS(J)*DENf(I,J,K)+SIFN(J-1)*DENf(I,J-1,K)
        TRBSRCV=(DEN*CDTERM*SQRTK*CAPPA)/ALOG(ELOG*YPLUSA)
      else
        TRBSRCV=VISCOS/Dface
      endif
      
      return
      end
      
C ********************* TRBSRCW *********************
C Stress/velocity at next-to-wall point for solid BCs, W-vel dir.

      FUNCTION TRBSRCW(I,J,K,Dface)
#include "building.h"
#include "cfd.h"
      
      COMMON/INTERP/SIFE(ntcelx),SIFW(ntcelx),SIFN(ntcely),SIFS(ntcely),
     &              SIFH(ntcelz),SIFL(ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     &            TURBIN,ALAMDA,PRTE,PRED
     
      CDTERM=CMU**0.25

C Calculate y+ by interpolation.
      YPLUSm1=DENf(I,J,K-1)*SQRT(ABS(TEf(I,J,K-1)))*CDTERM*Dface/VISCOS
      YPLUS  =DENf(I,J,K) * SQRT(ABS(TEf(I,J,K))) * CDTERM*Dface/VISCOS
      YPLUSA=SIFL(K)*YPLUS+SIFH(K-1)*YPLUSm1
      
C Calculate stress/velocity at next-to-wall point. Calculation depends upon
C whether we are in laminar sub-layer or log-law region.
      if (YPLUSA.GT.11.63) then

C Interpolate values of k and density.
        SQRTK=SQRT(ABS(SIFL(K)*TEf(I,J,K)+SIFH(K-1)*TEf(I,J,K-1)))
        DEN=SIFL(K)*DENf(I,J,K)+SIFH(K-1)*DENf(I,J,K-1)
        TRBSRCW=(DEN*CDTERM*SQRTK*CAPPA)/ALOG(ELOG*YPLUSA)
      else
        TRBSRCW=VISCOS/Dface
      endif
      
      return
      end
      
      
C ********************* YUANBC *********************
C Boundary conditions on solid wall for Yuan wall functions.

      FUNCTION YUANBC(I,J,K,Dface)
#include "building.h"
#include "cfd.h"

      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/YUANcm/Uqstor(ntcelx,ntcely,ntcelz),Uqinit
     
      LOGICAL Uqinit
      
C Set the friction velocity ($u sub tau$) to initial guess.
      iter = 0
      Utau = 0.06

C Calculate the thermal diffusivity.
      therdif = VISCOS / (PRANDL*DENf(i,j,k))

C Iterate until solution converges.
 10   Utauold = Utau

C Calculate dimensionless distance for inner sublayer ($y sup i sup **$) then
C evaluate wall function ($f sub i$) for inner sublayer. The velocity scale
C based on the heat flux ($U sub q$) was calculated and stored when the
C thermal wall function was evaluated.
      yistst = Dface*(Uqstor(i,j,k)**3) / (therdif*Utauold**2)
      if(yistst .gt. 0.53)then
        fi = 0.228
      else
        fi = yistst*(1.41 - 3.11*yistst + 2.38*yistst*yistst)
      endif

C Calculate dimensionless distance for outer sublayer ($y sup o sup **$) then
C evaluate wall function ($f sub o$) for outer sublayer.
      yostst = Dface*(Uqstor(i,j,k)**7) / (therdif*Utauold**6)
      if(yostst.gt.0.09)then
        fo = 0.024776*EXP(-yostst)
      elseif(yostst.gt.0.005)then
        fo =-0.458-0.258*ALOG(yostst)-0.02425*(ALOG(yostst))**2
      else
        fo = 0.228
      endif

C Set the dimensionless velocity ($u sup **$) according to the wall function.
      ustst = AMIN1(fi,fo)
      ustst = AMAX1(ustst,0.00001)

C Calculate the updated friction velocity. Use relaxation to stabilize solution.
      Utau = ( ((Uqstor(i,j,k))**3) *abs(Wf(i,j,K))/ustst )**0.25
      Utau = 0.5*( Utauold+Utau )

C Check for convergence. Tolerence based on recommendation on p90 of Yuan thesis.
C Allow 15 iterations at least.
      iter = iter + 1
      if( (ABS(Utau-Utauold)/Utau).gt.0.01 .and. iter.le.15 )goto 10
      YUANBC=Utau

      return
      end
      
