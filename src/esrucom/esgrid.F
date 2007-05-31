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

C ****************************   READST    ****************************
C READST read ASCII structured mesh data file.
C *********************************************************************
      SUBROUTINE READST(IComp,ITRC,IER)
      INCLUDE 'building.h'

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/C1/NCOMP,NCON

      COMMON/GRSD31/INDLCL(MS)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD42/LRow(MROW),LCol(MCOL),LLay(MLAY)
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)
      COMMON/GRSD101/LGrdSt

      CHARACTER LGrdSt*72,OUTSTR*124
      LOGICAL IsEven

      IER=0
      IOUT=IUOUT
      IIN=IFIL+1

C Open existing ASCII structured mesh data file as unit IIN.
      CALL EFOPSEQ(IIN,LGrdSt,1,IER)
      IF(IER.LT.0)RETURN

C The indices for each construction within the zone (1: defined, 0: not)
      NC=NCONST(IComp)
      CALL STRIPC(IIN,OUTSTR,NC,ND,1,'construction index',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      DO 10 IC=1,NC
        CALL EGETWI(OUTSTR,K,Indx,0,1,'F','constr. index',IER)
        IF(IER.NE.0)GOTO 777
        INDLCL(IC)=Indx
   10 CONTINUE

C The reference mesh width.
      CALL STRIPC(IIN,OUTSTR,1,ND,1,'ref. mesh width',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      CALL EGETWR(OUTSTR,K,RefWidth,0.,10.,'F','ref. width',IER)
      IF(IER.NE.0)GOTO 777

C The number of rows, columns, layers.
      CALL STRIPC(IIN,OUTSTR,3,ND,1,'No. of rows, col, lay.',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      MAXR=MROW*2+1
      CALL EGETWI(OUTSTR,K,NRow,7,MAXR,'F','No. of rows',IER)
      IF(IER.NE.0)GOTO 777
      IF(IsEven(NRow))THEN
        CALL USRMSG(' ',' Maximum row number should be odd.','W')
        GOTO 777
      ENDIF
      NumR=(NRow-1)/2
      MAXC=MCOL*2+1
      CALL EGETWI(OUTSTR,K,NCol,7,MAXC,'F','No. of columns',IER)
      IF(IER.NE.0)GOTO 777
      IF(IsEven(NCol))THEN
        CALL USRMSG(' ',' Maximum column should be odd.','W')
        GOTO 777
      ENDIF
      NumC=(NCol-1)/2
      MAXL=MLAY*2+1
      CALL EGETWI(OUTSTR,K,NLay,7,MAXL,'F','No. of layers',IER)
      IF(IER.NE.0)GOTO 777
      IF(IsEven(NLay))THEN
        CALL USRMSG(' ',' Maximum layer should be odd.','W')
        GOTO 777
      ENDIF
      NumL=(NLay-1)/2

C Row mesh widths.
      
      Numb=INT((NumR-1)/10)+1
      J2=0
      DO 20 I=1,Numb
        J1=J2+1
        IF(I.EQ.Numb)THEN
          N=NumR-(I-1)*10
          J2=NumR
        ELSE
          N=10
          J2=J2+10
        ENDIF
        CALL STRIPC(IIN,OUTSTR,N,ND,1,'row mesh widths',IER)
        IF(IER.NE.0)GOTO 777
        K=0
      DO 20 J=J1,J2
        CALL EGETWI(OUTSTR,K,LRow(J),1,1000000,'F','row',IER)
        IF(IER.NE.0)GOTO 777
   20 CONTINUE

C Column mesh widths.
      
      Numb=INT((NumC-1)/10)+1
      J2=0
      DO 30 I=1,Numb
        J1=J2+1
        IF(I.EQ.Numb)THEN
          N=NumC-(I-1)*10
          J2=NumC
        ELSE
          N=10
          J2=J2+10
        ENDIF
        CALL STRIPC(IIN,OUTSTR,N,ND,1,'column mesh widths',IER)
        IF(IER.NE.0)GOTO 777
        K=0
      DO 30 J=J1,J2
        CALL EGETWI(OUTSTR,K,LCol(J),1,1000000,'F','column',IER)
        IF(IER.NE.0)GOTO 777
   30 CONTINUE

C Layer mesh widths.
      
      Numb=INT((NumL-1)/10)+1
      J2=0
      DO 40 I=1,Numb
        J1=J2+1
        IF(I.EQ.Numb)THEN
          N=NumL-(I-1)*10
          J2=NumL
        ELSE
          N=10
          J2=J2+10
        ENDIF
        CALL STRIPC(IIN,OUTSTR,N,ND,1,'layer mesh widths',IER)
        IF(IER.NE.0)GOTO 777
        K=0
      DO 40 J=J1,J2
        CALL EGETWI(OUTSTR,K,LLay(J),1,1000000,'F','layer',IER)
        IF(IER.NE.0)GOTO 777
   40 CONTINUE

C Number of material geometries.
      CALL STRIPC(IIN,OUTSTR,1,ND,1,'No. of material boxes',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      CALL EGETWI(OUTSTR,K,NMtr,1,MMTR,'F','No. of material',IER)
      IF(IER.NE.0)GOTO 777

C Material geometries details.
      DO 50 I=1,NMtr
        CALL STRIPC(IIN,OUTSTR,8,ND,1,'material details',IER)
        IF(IER.NE.0)GOTO 777
        K=0
        CALL EGETWI(OUTSTR,K,IMtrS(I),0,NC,'F','Surface',IER)
        IF(IER.NE.0)GOTO 777
        NL=NELTS(IComp,IMtrS(I))
        CALL EGETWI(OUTSTR,K,IMtrE(I),0,NL,'F','Element',IER)
        IF(IER.NE.0)GOTO 777
        CALL EGETWI(OUTSTR,K,IMRowS(I),1,MAXR-1,'F','1st row',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMRowS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        CALL EGETWI(OUTSTR,K,IMColS(I),1,MAXC-1,'F','1st clmn',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMColS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        CALL EGETWI(OUTSTR,K,IMLayS(I),1,MAXL-1,'F','1st layr',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMLayS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IMRowS(I)+1
        CALL EGETWI(OUTSTR,K,IMRowE(I),M,MAXR-1,'F','last row',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMRowE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IMColS(I)+1
        CALL EGETWI(OUTSTR,K,IMColE(I),M,MAXC-1,'F','end clmn',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMColE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IMLayS(I)+1
        CALL EGETWI(OUTSTR,K,IMLayE(I),M,MAXL-1,'F','end layr',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IMLayE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
   50 CONTINUE

C Number of boundary surface boxes.
      CALL STRIPC(IIN,OUTSTR,1,ND,1,'No. of boundary boxes',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      CALL EGETWI(OUTSTR,K,NBnd,1,MBND,'F','No. of boundary',IER)
      IF(IER.NE.0)GOTO 777

C boundary surface boxes details.
      DO 60 I=1,NBnd
        CALL STRIPC(IIN,OUTSTR,8,ND,1,'boundary details',IER)
        IF(IER.NE.0)GOTO 777
        K=0
        CALL EGETWI(OUTSTR,K,IBndZ(I),1,NComp,'F','Zone',IER)
        IF(IER.NE.0)GOTO 777
        M=NCONST(IBndZ(I))
        CALL EGETWI(OUTSTR,K,IBndS(I),-M,M,'F','Surface',IER)
        IF(IER.NE.0)GOTO 777
        CALL EGETWI(OUTSTR,K,IBRowS(I),0,MAXR,'F','1st row',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBRowS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        CALL EGETWI(OUTSTR,K,IBColS(I),0,MAXC,'F','1st clmn',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBColS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        CALL EGETWI(OUTSTR,K,IBLayS(I),0,MAXL,'F','1st layr',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBLayS(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IBRowS(I)
        CALL EGETWI(OUTSTR,K,IBRowE(I),M,MAXR,'F','last row',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBRowE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IBColS(I)
        CALL EGETWI(OUTSTR,K,IBColE(I),M,MAXC,'F','end clmn',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBColE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
        M=IBLayS(I)
        CALL EGETWI(OUTSTR,K,IBLayE(I),M,MAXL,'F','end layr',IER)
        IF(IER.NE.0)GOTO 777
        IF(IsEven(IBLayE(I)))THEN
          CALL USRMSG(' ',' Box coordinates should be odd.','W')
          GOTO 777
        ENDIF
   60 CONTINUE
      CALL ERPFREE(IIN,ISTAT)
      IF(ITRC.NE.0)CALL EDISP(IOUT,
     &    'Structured mesh configuration file read succesfuly !')
      RETURN
  777 CALL EDISP(IOUT,'Structured mesh configuration file ignored !!!')
      CALL ERPFREE(IIN,ISTAT)
      RETURN
      END

C ****************************   SAVEST    ****************************
C SAVEST 
C *********************************************************************
      SUBROUTINE SAVEST(IComp,ITRC,IER)
      INCLUDE 'building.h'

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/GRSD31/INDLCL(MS)
      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD42/LRow(MROW),LCol(MCOL),LLay(MLAY)
      COMMON/GRSD51/NMtr,IMtrS(MMTR),IMtrE(MMTR)
      COMMON/GRSD52/IMRowS(MMTR),IMRowE(MMTR)
      COMMON/GRSD53/IMColS(MMTR),IMColE(MMTR)
      COMMON/GRSD54/IMLayS(MMTR),IMLayE(MMTR)
      COMMON/GRSD61/NBnd,IBndZ(MBND),IBndS(MBND)
      COMMON/GRSD62/IBRowS(MBND),IBRowE(MBND)
      COMMON/GRSD63/IBColS(MBND),IBColE(MBND)
      COMMON/GRSD64/IBLayS(MBND),IBLayE(MBND)
      COMMON/GRSD101/LGrdSt

      CHARACTER LGrdSt*72,H*72,ltmp*72

      IER=0
      IOUT=IUOUT
      IIN=IFIL+1
      H(1)='    Input the name for the zone file in which'
      H(2)=' the structured mesh configuration details are'
      H(3)=' to be saved.'
      H(4)=' '
      ltmp=LGrdSt
      CALL EASKS(ltmp,'Structured mesh file name ?',' ',
     &       72,' ',' Structured mesh file ',IER,3)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        LGrdSt=ltmp
      endif

C Open existing ASCII construction data file as unit IIN.
      CALL EFOPSEQ(IIN,LGrdSt,4,IER)
      IF(IER.LT.0)RETURN

C The indices for each construction within the zone (1: defined, 0: not).
      NC=NCONST(IComp)
      WRITE(IIN,'(a)')'# The indices for each construction (1: defined)'
      WRITE(IIN,'(20I3)')(INDLCL(IC),IC=1,NC)

C The reference mesh width.
      WRITE(IIN,'(F9.4,15X,A)')RefWidth,'# reference mesh width'

C The number of rows, columns, layers.
      WRITE(IIN,'(3I6,6X,A)')NRow,NCol,NLay,
     &                            '# number of rows, columns, layers'

C Row mesh widths.
      WRITE(IIN,'(a)')'# Row mesh widths'
      NumRow=(NRow-1)/2
      WRITE(IIN,'(10I6)')(LRow(IR),IR=1,NumRow)

C Column mesh widths.
      WRITE(IIN,'(a)')'# Column mesh widths'
      NumCol=(NCol-1)/2
      WRITE(IIN,'(10I6)')(LCol(IC),IC=1,NumCol)

C Layer mesh widths.
      WRITE(IIN,'(a)')'# Layer mesh widths'
      NumLay=(NLay-1)/2
      WRITE(IIN,'(10I6)')(LLay(IL),IL=1,NumLay)

C Number of material geometries.
      WRITE(IIN,'(I6,18X,A)')NMtr,'# Number of material geometries'

C Material geometries details.
      WRITE(IIN,'(a)')'# Material geometries details'
      DO 20 I=1,NMtr
        WRITE(IIN,'(8I6)')IMtrS(I),IMtrE(I),IMRowS(I),IMColS(I),
     &                  IMLayS(I),IMRowE(I),IMColE(I),IMLayE(I)
   20 CONTINUE

C Number of boundary surface boxes.
      WRITE(IIN,'(I6,18X,A)')NBnd,'# Number of boundary surface boxes'

C Boundary surface boxes details.
      WRITE(IIN,'(a)')'# Boundary surface boxes details'
      DO 30 I=1,NBnd
        WRITE(IIN,'(8I6)')IBndZ(I),IBndS(I),IBRowS(I),IBColS(I),
     &                  IBLayS(I),IBRowE(I),IBColE(I),IBLayE(I)
   30 CONTINUE
      CALL ERPFREE(IIN,ISTAT)
      IF(ITRC.NE.0)CALL EDISP(IOUT,
     &    'Structured mesh configuration file saved succesfuly !')
      RETURN
      END

C ****************************   IsEven    ****************************
C IsEven
C *********************************************************************
      LOGICAL FUNCTION IsEven(Num)

      Num2=MOD(Num,2)
      IF(Num2.EQ.0)THEN
        IsEven=.true.
      ELSE
        IsEven=.false.
      ENDIF
      RETURN
      END

C ****************************    IGridN    ***************************
C IGridN is a function which returns the grid number from the row,
C        column, and layer numbers.
C *********************************************************************
      FUNCTION IGridN(IRow,ICol,ILay)

      COMMON/GRSD41/RefWidth,NRow,NCol,NLay
      COMMON/GRSD111/NColLay

      IGridN=(IRow-1)*NColLay+(ICol-1)*NLay+ILay
      RETURN
      END
