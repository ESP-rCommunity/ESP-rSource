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
C WRITEM writes the results to PRESSURE and FLUID files.
C NB this subroutine is called (BUT commented) from MZMSTR.
C NB output units 93 & 94 should be opened and closed within MZNUMA.
C *********************************************************************
      SUBROUTINE WRITEM(ICOMP)
      INCLUDE 'building.h'

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair

      WRITE(93,'(2I6,4X,F10.1)')IDYF,IHRF,Pnair
      WRITE(94,'(2I6,4X,F8.4)')IDYF,IHRF,Fnair
      DO 10 I=1,NCONST(ICOMP)
        NM=MNDC(ICOMP,I)
        WRITE(93,'(I2,2X,9F10.1)')I,APf(ICOMP,I),(Pn(I,J),J=1,NM)
        WRITE(94,'(I2,2X,8F8.4)')I,(Fn(I,J),J=1,NM)
   10 CONTINUE
      RETURN
      END

C *******************************************************************
C MSTRAC can be used for checking the mass conservation within 
C moisture control volumes.
C NB this subroutine is called (BUT commented) from MZMSTR.
C NB output unit number 95 should be opened and closed within MZNUMA.
C *******************************************************************
      SUBROUTINE MSTRAC(ICOMP)
      INCLUDE 'building.h'

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CONEQN/W(MS,MNM,5)
      COMMON/MOIST03/MNDS,MNDZ(MCOM),MNDC(MCOM,MS),MNDL(MCOM,MS,ME)
      COMMON/MOIST21/CPpair,CPfair,RHOpair,RHOfair,Upair,Ufair
      COMMON/MOIST22/APp(MCOM,MS),APf(MCOM,MS)
      COMMON/MOIST23/TMPp(MS,MNM),TMPf(MS,MNM)
      COMMON/MOIST51/Pp(MCOM,MS,MNM),Fp(MCOM,MS,MNM),Sp(MCOM)
      COMMON/MOIST53/Ppair(MCOM),Fpair(MCOM),Spair(MCOM)
      COMMON/MOIST62/Pn(MS,MNM),Fn(MS,MNM),Sn
      COMMON/MOIST64/Pnair,Fnair,Snair
      COMMON/MOIST66/VPINF,VPCPL,VPLST,VPMCH,VPLTN
      COMMON/MOIST67/VFINF,VFCPL,VFLST,VFMCH,VFLTN

      common/trcm01/ep(MS),ef(MS),TERMcpl
      dimension flw(20)

      WRITE(95,'(2(A,I8))')' zone :',ICOMP,'   increment :',NSINC
      WRITE(95,*)' air node :'
      WRITE(95,*)'==========='
      WRITE(95,*)'INF:',VPINF*GEXTP,VFINF*GEXTF
      WRITE(95,*)'CPL:',VPCPL,VFCPL
      WRITE(95,*)'MCH:',VPMCH,VFMCH
      WRITE(95,*)'LTN:',VPLTN,VFLTN
      WRITE(95,*)'LST:',Upair*VPLST,Ufair*VFLST
      NC=NCONST(ICOMP)

C Air node.
      TC=TFA(ICOMP)
      Ps=Psat01(TC)
      RH=Pnair/Ps*100.0
      TOTAL=0.0
      DO 20 I=1,NC
        IMsurf=MNDC(ICOMP,I)
        FLW(I)=ef(I)*(Pn(I,IMsurf)-Pnair)+
     &         ep(I)*(Pp(icomp,I,IMsurf)-Ppair(ICOMP))
        TOTAL=TOTAL+FLW(I)
  20  CONTINUE
      WRITE(95,'(10F8.4)')(FLW(I),I=1,NC)
      WRITE(95,'(5(A,F10.4))')'IN :',TERMcpl,' OUT :',TOTAL,' T :',TC,
     &         ' P:',Pnair,' RH :',RH

C Construction moisture nodes.
      DO 10 I=1,NC
        MNC=MNDC(ICOMP,I)
        WRITE(95,'(A,I3)')' surface :',I
        WRITE(95,'(A)')'=============='
      DO 10 IM=MNC,1,-1
        IF(IM.EQ.1)THEN
          Pn1=APf(ICOMP,I)
        ELSE
          IM1=IM-1
          Pn1=Pn(I,IM1)
        ENDIF
        IF(IM.EQ.MNC)THEN
          Pn2=Pnair
        ELSE
          IM2=IM+1
          Pn2=Pn(I,IM2)
        ENDIF
        TC=TMPf(I,IM)
        WPn1=-W(I,IM,1)*(Pn1-Pn(I,IM))
        WPn2=-W(I,IM,3)*(Pn2-Pn(I,IM))
        WRITE(95,'(I3,2F10.5)')IM,WPn2,WPn1
   10 CONTINUE
      RETURN
      END

C ********************************************************************
      SUBROUTINE WRTCFP(ICOMP)
      INCLUDE 'building.h'

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character*72 TMPFL

      TMPFL='COEFFICIENT'
      CALL EFOPSEQ(88,TMPFL,3,IER)
      NC=NCONST(ICOMP)
      do 1111 i=1,nc
      ndc=nndc(icomp,i)
      write(88,'(a,i3)')'construction :',i
      do 1111 j=1,ndc
      write(88,'(3F15.4)')VCP(i,j,1),VCP(i,j,2),QCP(i,j)
      write(88,'(3F15.4)')VCF(i,j,1),VCF(i,j,2),QCF(i,j)
 1111 continue
      close(88)
      return
      end
