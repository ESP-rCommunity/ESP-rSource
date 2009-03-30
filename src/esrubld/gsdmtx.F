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

C *********************************************************************
C This file contains the following 3D zone modelling routines:


C INITST initializes the structured gridding variables.
C IGridN is a function which returns the grid number from the row,
C        column, and layer numbers.
C FLLSRF fills up the internal surface node array for the structured 
C        mesh.
C MAINST is the main controller for structured mesh modelling.
C STUPST Sets up the zone matrix for the zone with a structured mesh.
C MZSCFG calculates the longwave terms for internal surface nodes in 
C        structured mesh.
C SVITS1 solves the structured mesh zone matrix iterratively.
C SVITS2 solves the structured mesh zone matrix based on default 1D
C        gridding iterratively.
C BACKSD performs the back substitution process after adapting the 
C        control function for structured mesh.
C SAVE3D saves the multi-D and structured mesh system matrix coefficients.
C *********************************************************************

C **************************    INITST    *****************************
C INITST initializes the structured gridding variables.
C *********************************************************************
      SUBROUTINE INITST
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRSD31/INDLCL(MS)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD42/LRow(MROW),LCol(MCOL),LLay(MLAY)
      COMMON/GRSD43/DRow(MROW),DCol(MCOL),DLay(MLAY)
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)
      COMMON/GRSD100/IndxSt
      COMMON/GRSD110/IndSN(MGRID)
      COMMON/GRSD111/NColLay
      COMMON/GRSD112/NST3F
      COMMON/GRSD113/IST3F(MSNOD),AST3F(MSNOD),I1ST3F(MS),NTSNOD
      COMMON/GRSD120/RLnrSD,RRLnrSD
      COMMON/GRSD121/InSSlv,MSItrn,TSCrSM,QSCrSM
      COMMON/GRSD122/InSvST,ISTemp

c This variable is only required for the structured mesh configuration dumping.
c      character*5 text(40)

C Building structured mesh relaxation factor.
      InSSlv=2
      RLnrSD=1.6
      MSItrn=1000
      TSCrSM=0.001
      QSCrSM=0.01
      InSvST=0
      SML3=1.E-10
      NColLay=NCol*NLay
      IComp=IndxSt
      NCnst=NCONST(IComp)

C Create the 3D structured mesh and fill it with thermophysical properties, 
C or boundary index.
      NumRow=(NRow-1)/2
      DO 10 IRow=1,NumRow
        DRow(IRow)=LRow(IRow)*RefWidth
   10 CONTINUE
      NumCol=(NCol-1)/2
      DO 20 ICol=1,NumCol
        DCol(ICol)=LCol(ICol)*RefWidth
   20 CONTINUE
      NumLay=(NLay-1)/2
      DO 30 ILay=1,NumLay
        DLay(ILay)=LLay(ILay)*RefWidth
   30 CONTINUE

C NumGrid is the total number of grid points.
      NumGrid=NRow*NCol*NLay

C IndSN is the Index for Structured Nodes.
      DO 40 I=1,NumGrid
        IndSN(I)=0
   40 CONTINUE

C Apply the material geometries.
      DO 50 IMtr=1,NMtr
        IF(IMtrS(IMtr).EQ.0.OR.IMtrE(IMtr).EQ.0)THEN
          Indx=0
        ELSE
          Indx=IMtr
        ENDIF
      DO 50 IRow=IMRowS(IMtr)+1,IMRowE(IMtr),2
      DO 50 ICol=IMColS(IMtr)+1,IMColE(IMtr),2
      DO 50 ILay=IMLayS(IMtr)+1,IMLayE(IMtr),2
        I=IGridN(IRow,ICol,ILay)
        IndSN(I)=Indx
   50 CONTINUE

C Apply the boundary conditions.
      DO 60 IBnd=1,NBnd
        IZon=IBndZ(IBnd)
        ISrf=IBndS(IBnd)
        IF(IZon.LE.0.OR.ISrf.EQ.0)THEN
          Indx=0

C Internal surface.
        ELSEIF(ISrf.GT.0)THEN
          Indx=IBnd

C External surface.
        ELSEIF(ISrf.LT.0)THEN
          Indx=IBnd
        ENDIF
        DO 61 IRow=IBRowS(IBnd),IBRowE(IBnd),2
        DO 61 ICol=IBColS(IBnd)+1,IBColE(IBnd),2
        DO 61 ILay=IBLayS(IBnd)+1,IBLayE(IBnd),2

C Allow boundary condition only when it is defined at acceptable locations.
          I=IGridN(IRow-1,ICol,ILay)
          I1=IndSN(I)
          I=IGridN(IRow+1,ICol,ILay)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   61   CONTINUE
        DO 62 IRow=IBRowS(IBnd)+1,IBRowE(IBnd),2
        DO 62 ICol=IBColS(IBnd),IBColE(IBnd),2
        DO 62 ILay=IBLayS(IBnd)+1,IBLayE(IBnd),2
          I=IGridN(IRow,ICol-1,ILay)
          I1=IndSN(I)
          I=IGridN(IRow,ICol+1,ILay)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   62   CONTINUE
        DO 63 IRow=IBRowS(IBnd)+1,IBRowE(IBnd),2
        DO 63 ICol=IBColS(IBnd)+1,IBColE(IBnd),2
        DO 63 ILay=IBLayS(IBnd),IBLayE(IBnd),2
          I=IGridN(IRow,ICol,ILay-1)
          I1=IndSN(I)
          I=IGridN(IRow,ICol,ILay+1)
          I2=IndSN(I)
          IF((I1.GT.0.AND.I2.EQ.0).OR.(I1.EQ.0.AND.I2.GT.0))THEN
            I=IGridN(IRow,ICol,ILay)
            IndSN(I)=Indx
          ENDIF
   63   CONTINUE
   60 CONTINUE

C locate and number the nodes (by checking the control volumes).
      NST3F=0
      DO 70 IRow=2,NRow,2
      DO 70 ICol=2,NCol,2
      DO 70 ILay=2,NLay,2
        I=IGridN(IRow,ICol,ILay)
        IMat=IndSN(I)

C When a material control volume is located number the associated
C eight nodes if not numbered yet.
        IF(IMat.GT.0)THEN

C 1st node.
          I=IGridN(IRow-1,ICol-1,ILay-1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 2nd node.
          I=IGridN(IRow-1,ICol-1,ILay+1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 3rd node.
          I=IGridN(IRow-1,ICol+1,ILay-1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 4th node.
          I=IGridN(IRow-1,ICol+1,ILay+1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 5th node.
          I=IGridN(IRow+1,ICol-1,ILay-1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 6th node.
          I=IGridN(IRow+1,ICol-1,ILay+1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 7th node.
          I=IGridN(IRow+1,ICol+1,ILay-1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF

C 8th node.
          I=IGridN(IRow+1,ICol+1,ILay+1)
          INod=IndSN(I)
          IF(INod.EQ.0)THEN
            NST3F=NST3F+1
            IndSN(I)=NST3F
            T3F(NST3F)=15.0
          ENDIF
        ENDIF
   70 CONTINUE

C Fill up the internal surface node array.

C Initialize the indices for the internal surface nodes.
      NTSNOD=0
      DO 210 IS=1,NCnst
        I1ST3F(IS)=1
  210 CONTINUE

C First, check the structured mesh.
      DO 220 IRow=3,NRow-2,2
      DO 220 ICol=3,NCol-2,2
      DO 220 ILay=3,NLay-2,2
        IndxR=(IRow-1)/2
        IndxC=(ICol-1)/2
        IndxL=(ILay-1)/2
        DelR1=DRow(IndxR)
        DelR2=DRow(IndxR+1)
        DelC1=DCol(IndxC)
        DelC2=DCol(IndxC+1)
        DelL1=DLay(IndxL)
        DelL2=DLay(IndxL+1)
        I=IGridN(IRow,ICol,ILay)
        Inod=IndSN(I)
        IF(Inod.GT.0)THEN

C Check for the 12 possible boundary surfaces associated with the current node.
          I=IGridN(IRow,ICol-1,ILay-1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelC1*DelL1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow,ICol-1,ILay+1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelC1*DelL2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow,ICol+1,ILay-1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelC2*DelL1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow,ICol+1,ILay+1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelC2*DelL2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow-1,ICol,ILay-1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR1*DelL1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow-1,ICol,ILay+1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR1*DelL2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow+1,ICol,ILay-1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR2*DelL1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow+1,ICol,ILay+1)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR2*DelL2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow-1,ICol-1,ILay)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR1*DelC1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow-1,ICol+1,ILay)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR1*DelC2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow+1,ICol-1,ILay)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR2*DelC1/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
          I=IGridN(IRow+1,ICol+1,ILay)
          IBnd=IndSN(I)
          ISrf=IBndS(IBnd)
          IF(ISrf.GT.0)THEN
            Area=DelR2*DelC2/4.
            CALL FLLSRF(IComp,ISrf,Inod,Area)
          ENDIF
        ENDIF
  220 CONTINUE

C Find the total number of nodes (local and imported constructional nodes).
      IAIR=NST3F
      DO 80 IS=1,NCnst
        IF(INDLCL(IS).EQ.1)THEN
          NNds=2*NELTS(IComp,IS)+1
          DO 90 IN=1,NNds
            IAIR=IAIR+1
            T3F(IAIR)=15.0
          IF(IN.EQ.NNds)THEN
            Inod=IAIR
            Area=SNA(IComp,IS)
            CALL FLLSRF(IComp,IS,Inod,Area)
          ENDIF
   90     CONTINUE
        ENDIF
   80 CONTINUE

C Add the zone air node.
      IAIR=IAIR+1
      IF(IAIR.GT.MNOD3)STOP 'Error (9837): parameter MNOD3 reached.'
      IPLT=IAIR+1
      IPRT=IPLT+1

C Total number of coefficients in the zone matrix.
      NZMCF=IAIR*10

c This block dumps the internal nodes numbers and their areas.
c      do 400 i=1,ncnst
c      write(*,*)i,I1ST3F(i)
c  400 continue
c      do 410 i=1,NTSNOD
c      write(*,*)i,IST3F(i),AST3F(i)
c  410 continue
c      stop


c check the structured mesh configuration.
c      open(unit=87,file='configsd',status='unknown')
c      DO 500 ILay=1,NLay
c      write(87,*)
c      write(87,*)'Layer number :',ILay
c      write(87,'(a3,31I5)')'R/C',(j,j=1,11)
c      DO 500 IRow=1,NRow
c        DO 600 ICol=1,NCol
c          I=IGridN(IRow,ICol,ILay)
c          write(text(ICol),'(I5)')IndSN(I)
c  600   CONTINUE
c        write(87,'(I2,1X,11A5)')IRow,(text(j),j=1,11)
c  500 CONTINUE
c      close(87)
      RETURN
      END

C ***************************     FLLSRF     **************************
C FLLSRF fills up the internal surface node array for the structured 
C        mesh.
C *********************************************************************
      SUBROUTINE FLLSRF(IComp,ISrf,Inod,Area)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GRSD113/IST3F(MSNOD),AST3F(MSNOD),I1ST3F(MS),NTSNOD

      NCnst=NCONST(IComp)
      Loctn=-99

C Check if the internal boundary condition is assigned for the current
C node.
      IStart=I1ST3F(ISrf)
      IF(ISrf.EQ.NCnst)THEN
        IEnd=NTSNOD
      ELSE
        IEnd=I1ST3F(ISrf+1)-1
      ENDIF
      DO 10 I=IStart,IEnd
        IF(IST3F(I).EQ.Inod)THEN
          AST3F(I)=AST3F(I)+Area
          RETURN

C New internal boundary condition for the current boundary.
        ELSEIF(IST3F(I).GT.Inod.AND.Loctn.EQ.-99)THEN
          Loctn=I
        ENDIF
   10 CONTINUE
      IF(Loctn.EQ.-99)Loctn=IEnd+1
      NTSNOD=NTSNOD+1
      IF(NTSNOD.EQ.MSNOD)STOP 'Error (3859): parameter MSNOD exceeded.'
      DO 20 Loc=NTSNOD,Loctn+1,-1
        IST3F(Loc)=IST3F(Loc-1)
        AST3F(Loc)=AST3F(Loc-1)
   20 CONTINUE
      IST3F(Loctn)=Inod
      AST3F(Loctn)=Area
      DO 30 IS=ISrf+1,NCnst
        I1ST3F(IS)=I1ST3F(IS)+1
   30 CONTINUE
      RETURN
      END

C ***************************     MAINST     **************************
C MAINST is the main controller for structured mesh modelling.
C *********************************************************************
      SUBROUTINE MAINST(IComp)

      COMMON/GRSD121/InSSlv,MSItrn,TSCrSM,QSCrSM

C Set up the system (local and imported components) matrix.
      CALL STUPST(IComp)
c        write(*,*)'save the system matrix before lngwave (yes=1)?'
c        read(*,*)iyesno
c        if(iyesno.eq.1)call save3d

C Estimate the internal longwave coefficients.
      CALL MZSCFG(IComp)
c        write(*,*)'save the system matrix after lngwave (yes=1)?'
c        read(*,*)iyesno
c        if(iyesno.eq.1)call save3d

C Solve the system matrix iteratively.
      IF(InSSlv.EQ.1)THEN
        CALL FORW3D(IComp,ISNSR,B1,B2,B3)

C The plant term is not saved as it is known to be -1.0.
        B2=-1.
        CALL CONT3D(IComp,ISNSR,B1,B2,B3)
        CALL BACKSD
      ELSEIF(InSSlv.EQ.2)THEN
        CALL SVITS1(IComp)
      ENDIF

C Solve the local components seperately base on the radiative and 
C convective heat flows for the internal surface nodes.
C OR
C Solve the 1D default matrix based on the plant input and sensor temperature 
C (air node temperature in case of free floating control).
      CALL SVITS2(IComp)
      RETURN
      END

C ***************************     STUPST     **************************
C STUPST Sets up the zone matrix for the zone with a structured mesh.
C *********************************************************************
      SUBROUTINE STUPST(IComp)
#include "building.h"

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      COMMON/BCL9/IHPZON,IHPCON,IHPNDO,QNOPF,QHPSV
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &HCOF(MCOM,MS)
      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC2/VOL(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC14/emarea(MCOM)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE33/QELWP(MS),QELWF(MS)
      COMMON/COE33Z/QELWS(MCOM,MS)
      COMMON/COE34D/QCASR(2),QCASC(2)
      COMMON/COE35/ZAUGEP,ZAUGIP,ZAUGEF,ZAUGIF,QWCP,XX1,ZGIPM,ZGIFM
      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GR3D150/INDX3,DTIME
      COMMON/GRSD31/INDLCL(MS)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD43/DRow(MROW),DCol(MCOL),DLay(MLAY)
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD110/IndSN(MGRID)
      COMMON/GRSD112/NST3F
      COMMON/GRSD130/QCASRS(MCOM)

      DIMENSION LIndB(8,3),IndC(8,3),AreaC(8,3)
      DIMENSION Ind(6),SumN(6),LIndM(8),Vlm(8),DelXA(6)

C The zone matrix configuration is as follows:
C The equation for the i-th node is located at the i-th row.
C The equation for air node is located at the X-th row, where
C (X = number of nodes in the zone).
C The self-coupling coefficient for the i-th node is located
C at the i-th row and the i-th column.
C The cross-coupling coefficient for i-th node with the j-th 
C node is located at the i-th row and the j-th column.
C The plant term is located at the Y-th column (Y = X + 1), (However,
C the plant term is know as -1, so it will not be actually saved).
C The present term is located at the Z-th column (Z = Y + 1).

c Initialize the storage variables.
      DO 10 I=1,IAIR
        IFST(I)=0
   10 CONTINUE
      DO 20 I=1,NZMCF
        NXT(I)=0
   20 CONTINUE

C Define the index for the available storage location.
      IAVL=1
      NXT(1)=2

C Start coefficient generation.
C FIRST, consider only the imported component grids.
      DO 30 IRow=3,NRow-2,2
      DO 30 ICol=3,NCol-2,2
      DO 30 ILay=3,NLay-2,2
        I=IGridN(IRow,ICol,ILay)
        Inod=IndSN(I)
        IF(Inod.GT.0)THEN

C Initialize summation parameters.
C Current node future self coupling coefficent.
          SumNod=0.

C Present self coupling coefficent.
          SumPrt=0.

C Interconstruction connection with the six adjacent nodes
          SumN(1)=0.
          SumN(2)=0.
          SumN(3)=0.
          SumN(4)=0.
          SumN(5)=0.
          SumN(6)=0.

C Cross coupling with air node.
          SumAir=0.

C Define the associated mesh widths.
          IndxR=(IRow-1)/2
          IndxC=(ICol-1)/2
          IndxL=(ILay-1)/2
          DelR1=DRow(IndxR)
          DelR2=DRow(IndxR+1)
          DelC1=DCol(IndxC)
          DelC2=DCol(IndxC+1)
          DelL1=DLay(IndxL)
          DelL2=DLay(IndxL+1)

C Define the associated material indices.
          LIndM(1)=IGridN(IRow-1,ICol-1,ILay-1)
          LIndM(2)=IGridN(IRow-1,ICol-1,ILay+1)
          LIndM(3)=IGridN(IRow-1,ICol+1,ILay-1)
          LIndM(4)=IGridN(IRow-1,ICol+1,ILay+1)
          LIndM(5)=IGridN(IRow+1,ICol-1,ILay-1)
          LIndM(6)=IGridN(IRow+1,ICol-1,ILay+1)
          LIndM(7)=IGridN(IRow+1,ICol+1,ILay-1)
          LIndM(8)=IGridN(IRow+1,ICol+1,ILay+1)

C Initialize the self coupling terms.
          Vlm(1)=DelR1*DelC1*DelL1/8.
          Vlm(2)=DelR1*DelC1*DelL2/8.
          Vlm(3)=DelR1*DelC2*DelL1/8.
          Vlm(4)=DelR1*DelC2*DelL2/8.
          Vlm(5)=DelR2*DelC1*DelL1/8.
          Vlm(6)=DelR2*DelC1*DelL2/8.
          Vlm(7)=DelR2*DelC2*DelL1/8.
          Vlm(8)=DelR2*DelC2*DelL2/8.
          DO 40 I=1,8
            ILocM=LIndM(I)
            IndM=IndSN(ILocM)
            IF(IndM.GT.0)THEN
              IS=IMtrS(IndM)
              IL=IMtrE(IndM)
              RhoC=THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)
              SumNod=SumNod+RhoC*Vlm(I)

C Add the shortwave absorption for tmc nodes.
              IF(IS.GT.0.AND.ITMCFL(IComp,IS).NE.0)THEN
                IN=IL*2
                SAbsorp=2.0*QTMCA(IS,IN,2)/THRMLI(IComp,IS,IL,4)*Vlm(I)
                SumPrt=SumPrt+SAbsorp
              ENDIF
            ENDIF
   40     CONTINUE

C Update the future and present self coupling coefficents.
          SumNod=SumNod/DTIME
          SumPrt=SumPrt+SumNod*T3F(Inod)

C Estimate the cross coupling coefficients.
C Maximum number of connected nodes is 6.  While, maximum number of 
C connections is 24.

C Define the adjacent node locations.
          I=IGridN(IRow-2,ICol,ILay)
          Ind(1)=IndSN(I)
          I=IGridN(IRow,ICol-2,ILay)
          Ind(2)=IndSN(I)
          I=IGridN(IRow,ICol,ILay-2)
          Ind(3)=IndSN(I)
          I=IGridN(IRow,ICol,ILay+2)
          Ind(4)=IndSN(I)
          I=IGridN(IRow,ICol+2,ILay)
          Ind(5)=IndSN(I)
          I=IGridN(IRow+2,ICol,ILay)
          Ind(6)=IndSN(I)

C Accept only adjacent nodes whose number is bigger than the current node.
          DO 50 Ind2=1,6
           IF(Ind(Ind2).LT.Inod)Ind(Ind2)=0
   50     CONTINUE

C Define the location of the 3 connecting nodes for the 8 material 
C volumes seperately.
          IndC(1,1)=1
          IndC(1,2)=2
          IndC(1,3)=3
          IndC(2,1)=1
          IndC(2,2)=2
          IndC(2,3)=4
          IndC(3,1)=1
          IndC(3,2)=5
          IndC(3,3)=3
          IndC(4,1)=1
          IndC(4,2)=5
          IndC(4,3)=4
          IndC(5,1)=6
          IndC(5,2)=2
          IndC(5,3)=3
          IndC(6,1)=6
          IndC(6,2)=2
          IndC(6,3)=4
          IndC(7,1)=6
          IndC(7,2)=5
          IndC(7,3)=3
          IndC(8,1)=6
          IndC(8,2)=5
          IndC(8,3)=4

C Define the location of the 3 boundary conditions for the 8 material 
C volumes seperately.
          LIndB(1,1)=IGridN(IRow,ICol-1,ILay-1)
          LIndB(1,2)=IGridN(IRow-1,ICol,ILay-1)
          LIndB(1,3)=IGridN(IRow-1,ICol-1,ILay)
          LIndB(2,1)=IGridN(IRow,ICol-1,ILay+1)
          LIndB(2,2)=IGridN(IRow-1,ICol,ILay+1)
          LIndB(2,3)=IGridN(IRow-1,ICol-1,ILay)
          LIndB(3,1)=IGridN(IRow,ICol+1,ILay-1)
          LIndB(3,2)=IGridN(IRow-1,ICol,ILay-1)
          LIndB(3,3)=IGridN(IRow-1,ICol+1,ILay)
          LIndB(4,1)=IGridN(IRow,ICol+1,ILay+1)
          LIndB(4,2)=IGridN(IRow-1,ICol,ILay+1)
          LIndB(4,3)=IGridN(IRow-1,ICol+1,ILay)
          LIndB(5,1)=IGridN(IRow,ICol-1,ILay-1)
          LIndB(5,2)=IGridN(IRow+1,ICol,ILay-1)
          LIndB(5,3)=IGridN(IRow+1,ICol-1,ILay)
          LIndB(6,1)=IGridN(IRow,ICol-1,ILay+1)
          LIndB(6,2)=IGridN(IRow+1,ICol,ILay+1)
          LIndB(6,3)=IGridN(IRow+1,ICol-1,ILay)
          LIndB(7,1)=IGridN(IRow,ICol+1,ILay-1)
          LIndB(7,2)=IGridN(IRow+1,ICol,ILay-1)
          LIndB(7,3)=IGridN(IRow+1,ICol+1,ILay)
          LIndB(8,1)=IGridN(IRow,ICol+1,ILay+1)
          LIndB(8,2)=IGridN(IRow+1,ICol,ILay+1)
          LIndB(8,3)=IGridN(IRow+1,ICol+1,ILay)

C Define the distance for the 6 adjacent nodes.
          DelXA(1)=DelR1
          DelXA(2)=DelC1
          DelXA(3)=DelL1
          DelXA(4)=DelL2
          DelXA(5)=DelC2
          DelXA(6)=DelR2

C Define the area for the 3 nodes in the 8 material volumes seperately.
          AreaC(1,1)=DelC1*DelL1/4.
          AreaC(1,2)=DelR1*DelL1/4.
          AreaC(1,3)=DelR1*DelC1/4.
          AreaC(2,1)=DelC1*DelL2/4.
          AreaC(2,2)=DelR1*DelL2/4.
          AreaC(2,3)=DelR1*DelC1/4.
          AreaC(3,1)=DelC2*DelL1/4.
          AreaC(3,2)=DelR1*DelL1/4.
          AreaC(3,3)=DelR1*DelC2/4.
          AreaC(4,1)=DelC2*DelL2/4.
          AreaC(4,2)=DelR1*DelL2/4.
          AreaC(4,3)=DelR1*DelC2/4.
          AreaC(5,1)=DelC1*DelL1/4.
          AreaC(5,2)=DelR2*DelL1/4.
          AreaC(5,3)=DelR2*DelC1/4.
          AreaC(6,1)=DelC1*DelL2/4.
          AreaC(6,2)=DelR2*DelL2/4.
          AreaC(6,3)=DelR2*DelC1/4.
          AreaC(7,1)=DelC2*DelL1/4.
          AreaC(7,2)=DelR2*DelL1/4.
          AreaC(7,3)=DelR2*DelC2/4.
          AreaC(8,1)=DelC2*DelL2/4.
          AreaC(8,2)=DelR2*DelL2/4.
          AreaC(8,3)=DelR2*DelC2/4.

C Loop through the eight associated material volumes.
          DO 60 ICV=1,8
            ILocM=LIndM(ICV)
            IndM=IndSN(ILocM)
            IF(IndM.GT.0)THEN

C Start with interconstructional connections.
              IS=IMtrS(IndM)
              IL=IMtrE(IndM)
              Cond=THRMLI(IComp,IS,IL,1)
              DO 70 ICNC=1,3
                Area=AreaC(ICV,ICNC)
                Ind2=IndC(ICV,ICNC)
                DelX=DelXA(Ind2)
                TERM=Area*Cond/DelX
                SumN(Ind2)=SumN(Ind2)-TERM
   70         CONTINUE

C Now, check if there is any boundary condition defined at the surfaces
C which are associated with the current node control volume.
              DO 80 IB=1,3
                I=LIndB(ICV,IB)
                IBnd=IndSN(I)
                IF(IBnd.GT.0)THEN
                  IZN=IBndZ(IBnd)
                  ISN=IBndS(IBnd)
                  Area=AreaC(ICV,IB)

C External surface boundary condition.
                  IF(ISN.LT.0)THEN
                    ISN=-ISN

C Adiabatic case if IE(IZN,ISN) has been set to -5 in MZADJC.
                    IF(IE(IZN,ISN).NE.-5)THEN
                      TOut=ATF(IZN,ISN)
                      TERM=Area*HCOF(IZN,ISN)

C Cross coupling term (moved to the present side since the outside
C temperature is known).
                      SumPrt=SumPrt+TERM*TOut

C Self coupling term.
                      SumNod=SumNod+TERM

C The external heat fluxes.
                      IF(IZN.EQ.IComp)THEN
                        FluxE=QSOLE(ISN,2)+QELWF(ISN)+ARF(IZN,ISN)
                      ELSE
                        FluxE=QSLEF(IZN,ISN)+QELWS(IZN,ISN)+ARF(IZN,ISN)
                      ENDIF
                      SumPrt=SumPrt+Area*FluxE
                    ENDIF

C Internal surface boundary condition.
                  ELSEIF(ISN.GT.0)THEN
                    TERM=Area*HCIF(IZN,ISN)

C Cross coupling term with zone air node.
                    SumAir=SumAir-TERM

C The internal heat fluxes.
                    IF(IZN.EQ.IComp)THEN
                      QQQ1=QCASR(2)
                      QQQ2=QSOLI(ISN,2)
                    ELSE
                      QQQ1=QCASRS(IZN)
                      QQQ2=QSLIF(IZN,ISN)
                    ENDIF
                    FluxI=QQQ1*ZOA(IZN)*EI(IZN,ISN)/EMAREA(IZN)+QQQ2
                    SumPrt=SumPrt+Area*FluxI
                  ENDIF
                ENDIF
   80         CONTINUE
            ENDIF
   60     CONTINUE

C Update coefficients associated with the current node connection.
C with the six adjacent nodes.
          DO 90 I=1,6
            Nod2=Ind(I)
            IF(Nod2.GT.0)THEN
              TERM=SumN(I)
              CALL CFFVLU(Inod,Nod2,CFV,IPRV,LCTN)
              CRSC=TERM
              CALL UPDZMX(Inod,Nod2,CRSC,IPRV,LCTN)
              SumNod=SumNod-TERM
              CALL CFFVLU(Nod2,Inod,CFV,IPRV,LCTN)
              CRSC=TERM
              CALL UPDZMX(Nod2,Inod,CRSC,IPRV,LCTN)
              CALL CFFVLU(Nod2,Nod2,CFV,IPRV,LCTN)
              SLFC=CFV-TERM
              CALL UPDZMX(Nod2,Nod2,SLFC,IPRV,LCTN)
            ENDIF
   90     CONTINUE

C Update coefficients associated with the current node connection.
C with the zone air node.
          CALL CFFVLU(Inod,IAIR,CFV,IPRV,LCTN)
          CRSC=SumAir
          CALL UPDZMX(Inod,IAIR,CRSC,IPRV,LCTN)
          SumNod=SumNod-SumAir
          CALL CFFVLU(IAIR,Inod,CFV,IPRV,LCTN)
          CRSC=SumAir
          CALL UPDZMX(IAIR,Inod,CRSC,IPRV,LCTN)
          CALL CFFVLU(IAIR,IAIR,CFV,IPRV,LCTN)
          SLFC=CFV-SumAir
          CALL UPDZMX(IAIR,IAIR,SLFC,IPRV,LCTN)

C Update the future self coupling term.
          CALL CFFVLU(Inod,Inod,CFV,IPRV,LCTN)
          SLFC=CFV+SumNod
          CALL UPDZMX(Inod,Inod,SLFC,IPRV,LCTN)

C Update the known (present) term.
          CALL CFFVLU(Inod,IPRT,CFV,IPRV,LCTN)
          SLFC=CFV+SumPrt
          CALL UPDZMX(Inod,IPRT,SLFC,IPRV,LCTN)
        ENDIF
   30 CONTINUE

C SECOND, consider the local components.
      IN=NST3F
      NC=NCONST(IComp)
      DO 100 IS=1,NC

C Local components replaced with imported component will not be considered.
        IF(INDLCL(IS).EQ.0)GOTO 100
        Area=SNA(IComp,IS)

C Number of air gaps within the current consrtuction.
        NGP=NGAPS(IComp,IS)
        M=1
        ILN=0

C Set the air gap flage to off.
        IFLG=0

C External surface node.
        ILN=ILN+1
        IN=IN+1
        IN2=IN+1
        IL=1

C Initialize the (future and present) self coupling terms.
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        DELXL=THRMLI(IComp,IS,IL,4)/2.0
        Volm=Area*DELXL/2.0
        TERM=THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)*Volm/DTIME
        SLFC=TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
        CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
        SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
        CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with outside.
C Special for bcl09.
         IF(IComp.EQ.IHPZON.AND.IS.EQ.IHPCON)THEN
            QHPNOF=QHPSV
         ELSE
            QHPNOF=0.
         ENDIF
        IF(IE(IComp,IS).NE.-5)THEN
          TOut=ATF(IComp,IS)
          TERM=Area*HCOF(IComp,IS)
          CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERM*TOut+Area*(QSOLE(IS,2)+QELWF(IS)+
     &          ARF(IComp,IS)+QHPNOF)
          CALL UPDZMX(IN,IPRT,PTERM,IPRV,LCTN)
          CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
          SLFC=CFV+TERM
          CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
        ELSE
          CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+Area*QHPNOF
          CALL UPDZMX(IN,IPRT,PTERM,IPRV,LCTN)
        ENDIF

C Connection with the next inner node.
        TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
        CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Continue with the second node in the first layer.
        ILN=ILN+1
        IN=IN+1
        IN1=IN-1
        IN2=IN+1

C Initialize the self coupling term.
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        Volm=Area*DELXL
        TERM=THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)*Volm/DTIME
        SLFC=TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
        CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
        SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
        CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
        TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
        CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
        CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Continue with other layers in the construction.
        DO 110 IL=2,NELTS(IComp,IS)

C IL: current layer, IP: previous layer.
          IP=IL-1
          DELXL=THRMLI(IComp,IS,IL,4)/2.0
          DELXP=THRMLI(IComp,IS,IP,4)/2.0
          ILN=ILN+1
          IN=IN+1
          IN1=IN-1
          IN2=IN+1
            
C The current layer is an air gap.
          IF(M.LE.NGP.AND.NPGAP(IComp,IS,M).EQ.IL)THEN

C If the previous layer was a solid.
            IF(IFLG.EQ.0)THEN

C Initialize the self coupling term.
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              CAP=THRMLI(IComp,IS,IP,2)*THRMLI(IComp,IS,IP,3)*
     &            (DELXP/2.)+1.3*1005.5*(DELXL/2.)
              TERM=CAP*Area/DTIME
              SLFC=TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
              CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
              SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
              CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
              TERM=Area*THRMLI(IComp,IS,IP,1)/DELXP
              CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
              TERM=Area/(RGAPS(IComp,IS,M)/2.0)
              CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C If the previous layer was an air gap also.
            ELSE

C Initialize the self coupling term.
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              CAP=1.3*1005.5
              TERM=CAP*Area*(DELXL+DELXL)/(2.0*DTIME)
              SLFC=TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
              CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
              SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
              CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
              TERM=Area/(RGAPS(IComp,IS,M-1)/2.0)
              CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
              TERM=Area/(RGAPS(IComp,IS,M)/2.0)
              CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
            ENDIF
            IFLG=1

C Continue with other nodes (homogeneous) in the air gap.
            ILN=ILN+1
            IN=IN+1
            IN1=IN-1
            IN2=IN+1

C Initialize the self coupling term.
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            CAP=1.3*1005.5
            TERM=CAP*Area*DELXL/DTIME
            SLFC=TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
            CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
            SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
            CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
            TERM=Area/(RGAPS(IComp,IS,M)/2.0)
            CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
            CRSC=-TERM
            CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            SLFC=CFV+TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
            CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
            CRSC=-TERM
            CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            SLFC=CFV+TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Increment the air gap index.
            M=M+1

C The current layer is solid.
          ELSE

C If the previous layer was solid.
            IF(IFLG.EQ.0)THEN

C Initialize the self coupling term.
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              CAP=THRMLI(IComp,IS,IP,2)*THRMLI(IComp,IS,IP,3)*(DELXP/2.)
     &           +THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)*(DELXL/2.)
              TERM=CAP*Area/DTIME
              SLFC=TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
              CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
              SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
              CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
              TERM=Area*THRMLI(IComp,IS,IP,1)/DELXP
              CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
              TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
              CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C If the previous layer was an air gap
            ELSE

C Initialize the self coupling term.
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              CAP=1.3*1005.5*(DELXP/2.)+THRMLI(IComp,IS,IL,2)*
     &            THRMLI(IComp,IS,IL,3)*(DELXL/2.)
              TERM=CAP*Area/DTIME
              SLFC=TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
              CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
              SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
              CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
              TERM=Area/(RGAPS(IComp,IS,M-1)/2.0)
              CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
              TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
              CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
              CRSC=-TERM
              CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
              CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
              SLFC=CFV+TERM
              CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
            ENDIF
            IFLG=0

C Continue with other nodes (homogeneous) in the layer.
            ILN=ILN+1
            IN=IN+1
            IN1=IN-1
            IN2=IN+1

C Initialize the self coupling term.
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            CAP=THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)
            TERM=CAP*Area*DELXL/DTIME
            SLFC=TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
            CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
            SLFC=TERM*T3F(IN)+Area*QTMCA(IS,ILN,2)
            CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
            TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
            CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
            CRSC=-TERM
            CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            SLFC=CFV+TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the next inner node.
            CALL CFFVLU(IN,IN2,CFV,IPRV,LCTN)
            CRSC=-TERM
            CALL UPDZMX(IN,IN2,CRSC,IPRV,LCTN)
            CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
            SLFC=CFV+TERM
            CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
          ENDIF
  110   CONTINUE

C Internal surface node
        ILN=ILN+1
        IN=IN+1
        IN1=IN-1
        IL=NELTS(IComp,IS)
        DELXL=THRMLI(IComp,IS,IL,4)/2.0

C Initialize the self coupling term.
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        Volm=Area*DELXL/2.0
        TERM=THRMLI(IComp,IS,IL,2)*THRMLI(IComp,IS,IL,3)*Volm/DTIME
        SLFC=TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)
        CALL CFFVLU(IN,IPRT,CFV,IPRV,LCTN)
        QCASRF=QCASR(2)*ZOA(IComp)*EI(IComp,IS)/EMAREA(IComp)
        SLFC=TERM*T3F(IN)+Area*(QSOLI(IS,2)+QTMCA(IS,ILN,2)+QCASRF)
        CALL UPDZMX(IN,IPRT,SLFC,IPRV,LCTN)

C Connection with the previous node.
        TERM=Area*THRMLI(IComp,IS,IL,1)/DELXL
        CALL CFFVLU(IN,IN1,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IN,IN1,CRSC,IPRV,LCTN)
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C Connection with the zone air node.
        TERM=Area*HCIF(IComp,IS)

C internal surface node.
        CALL CFFVLU(IN,IAIR,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IN,IAIR,CRSC,IPRV,LCTN)
        CALL CFFVLU(IN,IN,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IN,IN,SLFC,IPRV,LCTN)

C zone air node.
        CALL CFFVLU(IAIR,IN,CFV,IPRV,LCTN)
        CRSC=-TERM
        CALL UPDZMX(IAIR,IN,CRSC,IPRV,LCTN)
        CALL CFFVLU(IAIR,IAIR,CFV,IPRV,LCTN)
        SLFC=CFV+TERM
        CALL UPDZMX(IAIR,IAIR,SLFC,IPRV,LCTN)

C Long wave radiation coefficients (only consider the local components as
C the imported components are considered previuosely.
  100 CONTINUE

C Update the zone air node coefficients.
C future self coupling coefficient.
      CALL CFFVLU(IAIR,IAIR,CFV,IPRV,LCTN)
      TERM=1188*VOL(IComp)/DTIME
      SLFC=CFV+TERM+ZAUGEF+ZAUGIF+CVIF+CVVF
      CALL UPDZMX(IAIR,IAIR,SLFC,IPRV,LCTN)

C present self coupling coefficient.
      CALL CFFVLU(IAIR,IPRT,CFV,IPRV,LCTN)
      SLFC=TERM*T3F(IAIR)+XX1+XX3+QCASC(2)
      CALL UPDZMX(IAIR,IPRT,SLFC,IPRV,LCTN)
      RETURN
      END

C *************************    MZSCFG    ******************************
C MZSCFG calculates the longwave terms for internal surface nodes in 
C        structured mesh.
C *********************************************************************
      SUBROUTINE MZSCFG(IComp)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),
     &              AE(MCOM,MS)
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/V2/CFB(MCOM,MST,MST)
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRSD113/IST3F(MSNOD),AST3F(MSNOD),I1ST3F(MS),NTSNOD
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF


C Set Stefan-Boltzmann constant.
      SBC=56.7E-9
      NCnst=NCONST(IComp)
      DO 10 IS1=1,NCnst

C Define the nodes belonging to the first surface (IS1).
        ISt1=I1ST3F(IS1)
        IF(IS1.EQ.NCnst)THEN
          IEn1=NTSNOD
        ELSE
          IEn1=I1ST3F(IS1+1)-1
        ENDIF

C For each node within the first surface.
        DO 20 IN1=ISt1,IEn1
          ND1=IST3F(IN1)
          A1=AST3F(IN1)
          T3F1=T3F(ND1)
c          SUM=0.

C Define the other internal boundary condition.
          DO 30 IS2=1,NCnst
            IF(IS1.LT.IS2)THEN

C Define the nodes belonging to the second surface (IS2).
              ISt2=I1ST3F(IS2)
              IF(IS2.EQ.NCnst)THEN
                IEn2=NTSNOD
              ELSE
                IEn2=I1ST3F(IS2+1)-1
              ENDIF
              DO 40 IN2=ISt2,IEn2
                ND2=IST3F(IN2)
                IF(ND1.EQ.ND2)GOTO 40
                A2=AST3F(IN2)
                T3F2=T3F(ND2)

C From IN2 to IN1 (/m2 of K).
                T1=T3F1+273.16
                T2=T3F2+273.16
                DTMP=ABS(T1-T2)
                IF(NSINC.EQ.1)THEN
                  HRF21=5.7*CFB(IComp,IS2,IS1)
                ELSEIF(DTMP.LT.1.E-5)THEN
                  HRF21=0.
                ELSE
                  E1=EI(IComp,IS1)
                  E2=EI(IComp,IS2)
                  F12=GETF12(IComp,IS1,IS2,A2)
                  F21=GETF12(IComp,IS2,IS1,A1)
                  R1=1.-E1
                  R2=1.-E2
                  X1=A2*T2**4*F21-A1*T1**4*F12
                  X2=A1*(T2-T1)
                  X3=1.-R1*R2*F12*F21
                  X10=E1*E2*SBC*X1/(X2*X3)
                  X21=0.
                  X31=0.
                  DO 50 IS3=1,NCnst
                    IF(IS3.NE.IS1.AND.IS3.NE.IS2)THEN
                      F13=CFB(IComp,IS1,IS3)
                      F23=CFB(IComp,IS2,IS3)
                      F32=GETF12(IComp,IS3,IS2,A2)
                      F31=GETF12(IComp,IS3,IS1,A1)
                      E3=EI(IComp,IS3)
                      R3=1.-E3
                      X20=1.-R1*R2*R3*F23*F31*F12
                      X21=X21+R3*A2*T2**4*F23*F31/X20
                      X30=1.-R1*R2*R3*F13*F32*F21
                      X31=X31+R3*A1*T1**4*F13*F32/X30
                    ENDIF
   50             CONTINUE
                  X40=E1*E2*SBC*(X21-X31)/X2
                  HRF21=ABS(X10+X40)
                ENDIF

C The value of HRF21 is multiplied by A1*(T2-T1) since it will be added to 
C the known term directly.
                HRF21A1=HRF21*A1*(T2-T1)
c                SUM=SUM+HRF21A1
                CALL CFFVLU(ND1,IPRT,CVF,IPRV,LCTN)
                PTERM=CVF+HRF21A1
                CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
                CALL CFFVLU(ND2,IPRT,CVF,IPRV,LCTN)
                PTERM=CVF-HRF21A1
                CALL UPDZMX(ND2,IPRT,PTERM,IPRV,LCTN)
   40         CONTINUE
            ENDIF
   30     CONTINUE

C Future self-coupling term.
c          CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
c          FSLF=CFV+SUM
c          CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)
   20   CONTINUE
   10 CONTINUE
      RETURN
      END

C *************************    SVITS1    ******************************
C SVITS1 solves the structured mesh zone matrix iterratively.
C *********************************************************************
      SUBROUTINE SVITS1(IComp)
#include "building.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CCTL/ICASCF(MCOM)

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRSD120/RLnrSD,RRLnrSD
      COMMON/GRSD121/InSSlv,MSItrn,TSCrSM,QSCrSM

      DOUBLE PRECISION Sum,Domain

C Define the location of the control sensor.
      IF(ICOMP.GT.0)THEN
        IIC=ICASCF(ICOMP)
        IF(NCF.NE.0.AND.IIC.GT.0.AND.(IBSN(IIC,1).EQ.ICOMP.OR.
     &     IBSN(IIC,1).EQ.0).AND.IBSN(IIC,2).GT.0.AND.
     &     IBSN(IIC,2).LT.IAIR)THEN
          ISNSR=IBSN(IIC,2)
        ELSE
          ISNSR=IAIR
        ENDIF
      ELSE
        ISNSR=IAIR
      ENDIF

C Perform the iterartion.
      DO 10 ITR=1,MSItrn

C Set convergence parameters.
        DeltaMax=-1.0
        DeltaQMax=-1.0

      DO 30 IEQ=1,IAIR
        Sum=0.0
        Domain=0.0

C The first coefficient associated with the current node equation.
        ICFF=IFST(IEQ)
   40   ICLMN=NCLMN(ICFF)

C Check if the current coefficent is future self coupling coefficent.
        IF(ICLMN.EQ.IEQ)THEN
          Domain=W3(ICFF)

C Check if the current coefficent is future cross coupling coefficent.
        ELSEIF(ICLMN.LE.IAIR)THEN
          Sum=Sum-W3(ICFF)*T3F(ICLMN)

C Check if the current coefficent is present or known coefficient.
        ELSEIF(ICLMN.EQ.IPRT)THEN
          Sum=Sum+W3(ICFF)
        ELSE
          STOP 'Error (2213): fatal error in FORW3D.'
        ENDIF
        ICFF=NXT(ICFF)
        IF(ICFF.GT.0)GOTO 40

C The node equation is set.
C Call subroutine if the current node is associated with the control function.
        IF(IEQ.EQ.ISNSR)THEN
          T3save=T3F(IEQ)
          Q3save=T3F(IPLT)
          B1=SNGL(Domain)
          B2=-1.0
          B3=SNGL(Sum)
          CALL CONT3D(ICOMP,ISNSR,B1,B2,B3)
          Delta=ABS(T3F(IEQ)-T3save)
          DeltaMax=AMAX1(Delta,DeltaMax)
          DeltaQ=ABS(T3F(IPLT)-Q3save)
          DeltaQMax=AMAX1(DeltaQ,DeltaQMax)
          T3F(IEQ)=RLnrSD*T3F(IEQ)+RRLnrSD*T3save
        ELSE
          T3save=T3F(IEQ)
          T3F(IEQ)=SNGL(Sum/Domain)
          Delta=ABS(T3F(IEQ)-T3save)
          DeltaMax=AMAX1(Delta,DeltaMax)
          T3F(IEQ)=RLnrSD*T3F(IEQ)+RRLnrSD*T3save
        ENDIF
   30 CONTINUE
      IF(DeltaMax.LT.TSCrSM.and.DeltaQMax.LT.QSCrSM)RETURN
   10 CONTINUE
      CALL EDISP(IUOUT,'WARNING 1: Convergence did not occure.')
      write(*,*)DeltaMax,DeltaQMax
      RETURN
      END

C *************************    SVITS2    ******************************
C SVITS2 solves the structured mesh zone matrix based on default 1D
C        gridding iterratively.
C *********************************************************************
      SUBROUTINE SVITS2(IComp)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRSD121/InSSlv,MSItrn,TSCrSM,QSCrSM


      DOUBLE PRECISION Sum,Domain

C Define the location of the control sensor.
      IF(ICOMP.GT.0)THEN
        IIC=ICASCF(ICOMP)
        IF(NCF.NE.0.AND.IIC.GT.0.AND.(IBSN(IIC,1).EQ.ICOMP.OR.
     &     IBSN(IIC,1).EQ.0).AND.IBSN(IIC,2).GT.0.AND.
     &     IBSN(IIC,2).LT.IAIR)THEN
          ISNSR=IBSN(IIC,2)
        ELSE
          ISNSR=IAIR
        ENDIF
      ELSE
        ISNSR=IAIR
      ENDIF

C For current component determine associated control function.
      IC=ICASCF(ICOMP)
      NC=NCONST(ICOMP)

C Check if the air node is a plant interaction point.
      TFA(IComp)=T3F(IAIR)
      IF(NCF.NE.0.AND.IC.NE.0.AND.
     &  (IBAN(IC,1).NE.ICOMP.AND.IBAN(IC,1).GT.0).OR.
     &  (IBAN(IC,1).NE.-2.AND.IBAN(IC,2).GT.0).OR.
     &  (IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).NE.ICOMP))THEN
        QFA(IComp)=0.0
      ELSE
        QFA(IComp)=T3F(IPLT)
      ENDIF

C Perform the iterartion.
      DO 10 ITR=1,MSItrn

C Set convergence parameters.
        DeltaMax=-1.0

C Consider each construction in turn.
      DO 20 IS=1,NC
        Sum=0.0
        Domain=0.0

C Node is situated at 'outside' surface.
        IN=1
        T2=TFC(IComp,IS,2)

C Check if the current node is a plant interaction point.
        IF(NCF.EQ.0.OR.IC.EQ.0.OR.IBAN(IC,1).NE.ICOMP.OR.
     &    IBAN(IC,3).LE.0.OR.IBAN(IC,2).NE.IS.OR.
     &    IBAN(IC,3).NE.IN)THEN
          Sum=W(IS,IN,5)-W(IS,IN,3)*T2
          Domain=W(IS,IN,2)
          T3save=TFC(IComp,IS,IN)
          TFC(IComp,IS,IN)=SNGL(Sum/Domain)
          Delta=ABS(TFC(IComp,IS,IN)-T3save)
          DeltaMax=AMAX1(Delta,DeltaMax)
        ELSE
          TFC(IComp,IS,IN)=T3F(ISNSR)
          QFC(IComp)=T3F(IPLT)
        ENDIF

C Continue with remaining nodes within construction
        NNM1=NNDC(ICOMP,IS)-1
        DO 30 IN=2,NNM1
          T1=TFC(IComp,IS,IN-1)
          IF(IN.EQ.NNM1)THEN
            T2=TFS(IComp,IS)
          ELSE
            T2=TFC(IComp,IS,IN+1)
          ENDIF

C Check if the current node is a plant interaction point.
          IF(NCF.EQ.0.OR.IC.EQ.0.OR.IBAN(IC,1).NE.ICOMP.OR.
     &      IBAN(IC,3).LE.0.OR.IBAN(IC,2).NE.IS.OR.IBAN(IC,3).NE.IN)THEN
            Sum=W(IS,IN,5)-W(IS,IN,1)*T1-W(IS,IN,3)*T2
            Domain=W(IS,IN,2)
            T3save=TFC(IComp,IS,IN)
            TFC(IComp,IS,IN)=SNGL(Sum/Domain)
            Delta=ABS(TFC(IComp,IS,IN)-T3save)
            DeltaMax=AMAX1(Delta,DeltaMax)
            QFC(IComp)=0.0
          ELSE
            TFC(IComp,IS,IN)=T3F(ISNSR)
            QFC(IComp)=T3F(IPLT)
          ENDIF
   30   CONTINUE

C Node is situated at an inside surface.
        N1=NC+1
        N3=NC+3
        N4=NC+4

C Check if the current node is a plant interaction point.
        IF(NCF.EQ.0.OR.IC.EQ.0.OR.IBAN(IC,1).NE.ICOMP.OR.
     &    IBAN(IC,2).LE.0.OR.IBAN(IC,3).GT.0.OR.IBAN(IC,2).NE.IS)THEN
          Sum=E(IS,N3)-E(IS,N4)*TFC(IComp,IS,NNM1)-E(IS,N1)*TFA(IComp)
          DO 40 J=1,NC
            IF(J.EQ.IS)THEN
              Domain=E(IS,IS)
            ELSE
              Sum=Sum-E(IS,J)*TFS(IComp,J)
            ENDIF
   40     CONTINUE
          T3save=TFS(IComp,IS)
          TFC(IComp,IS,IN)=SNGL(Sum/Domain)
          TFS(IComp,IS)=TFC(IComp,IS,IN)
          Delta=ABS(TFS(IComp,IS)-T3save)
          DeltaMax=AMAX1(Delta,DeltaMax)
          QFS(IComp)=0.0
        ELSE
          TFC(IComp,IS,IN)=T3F(ISNSR)
          TFS(IComp,IS)=T3F(ISNSR)
          QFS(IComp)=T3F(IPLT)
        ENDIF
   20 CONTINUE
      IF(DeltaMax.LT.TSCrSM)RETURN
   10 CONTINUE
      CALL EDISP(IUOUT,'WARNING 2: Convergence did not occure.')
      RETURN
      END

C *************************    BACKSD    ******************************
C BACKSD performs the back substitution process after adapting the 
C        control function for structured mesh.
C *********************************************************************
      SUBROUTINE BACKSD
#include "building.h"

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)

      DOUBLE PRECISION PRESENT,DOMAIN,SUM,TSUM,DT3FN

      DO 10 IEQ=IAIR-1,1,-1
        SUM=0.
        PRESENT=0.
        DOMAIN=0.
        ICFF=IFST(IEQ)
        IF(ICFF.EQ.0)STOP 'no unique solution exists.'
   20   ICLMN=NCLMN(ICFF)

C Present term.
        IF(ICLMN.EQ.IPRT)THEN
          PRESENT=W3(ICFF)
        ELSEIF(ICLMN.EQ.IEQ)THEN
          DOMAIN=W3(ICFF)
        ELSE
          T3F1=T3F(ICLMN)
          TSUM=T3F1*W3(ICFF)
          SUM=SUM+TSUM
        ENDIF
        ICFF=NXT(ICFF)
        IF(ICFF.GT.0)GOTO 20
        DT3FN=(PRESENT-SUM)/DOMAIN
        T3F(IEQ)=SNGL(DT3FN)
   10 CONTINUE
      RETURN
      END

C *********************************************************************
      subroutine TEMPSD (icomp)
#include "building.h"

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD100/IndxSt
      COMMON/GRSD110/IndSN(MGRID)
      COMMON/GRSD122/InSvST,ISTemp

      CHARACTER*5 TEXTC(MCOL)

      IF(IndxSt.NE.icomp.OR.InSvST.EQ.0)RETURN
      WRITE(ISTemp,*)'Time :',IHRF
      WRITE(ISTemp,*)
      DO 10 ILay=3,NLay-2,2
      WRITE(ISTemp,*)
      WRITE(ISTemp,*)'Layer = ',ILay
      WRITE(ISTemp,'(A3,3X,20(I2,4X))')'R/C',(J,J=3,NCol-2,2)
      DO 20 IRow=3,NRow-2,2
        IC=0
      DO 30 ICol=3,NCol-2,2
        IC=IC+1
        I=IGridN(IRow,ICol,ILay)
        Inod=IndSN(I)
        IF(Inod.GT.0)THEN
          WRITE(TEXTC(IC),'(F5.1)')T3F(Inod)
        ELSE
          TEXTC(IC)='+++++'
        ENDIF
   30 CONTINUE
      WRITE(ISTemp,'(1X,I2,1X,20(A5,1X))')IRow,(TEXTC(J),J=1,IC)
   20 CONTINUE
   10 CONTINUE
      RETURN
      END

C ***************************   save3d  ***********************************
C SAVE3D saves the multi-D and structured mesh system matrix coefficients.
C *************************************************************************
      subroutine save3d
#include "building.h"

      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF


      open(unit=88,file='save3d',status='unknown')
      
      DO 30 IEQ=1,IAIR
        ICFF=IFST(IEQ)
        IF(ICFF.EQ.0)GOTO 30
   40   ICLMN=NCLMN(ICFF)
        write(88,'(i4,2x,i4,2x,f16.6)')ieq,iclmn,w3(icff)
        ICFF=NXT(ICFF)
        IF(ICFF.GT.0)GOTO 40
   30 CONTINUE
      close(88)
      return
      end
