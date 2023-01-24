C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following routines.
C    MOSCAS: Selection/control for surface causal energy breakdown.
C    MOSCS1: Display breakdown by plant status.
C    MOSCS2: Display breakdown as gain/loss list.
C    MOSSUM: Collect/sum surface causal data.
C    MOSRGT: Get surface data from results library.

C ********** MOSCAS

C Selection/control for surface causal energy breakdown.

      SUBROUTINE MOSCAS(IZONE,ISFN,IANZSE)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL

      COMMON/AVRAGE/IDAVER
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/SNAP/SNAPSH
      logical SNAPSH

      CHARACTER TSTR*10,CSTR*25
      CHARACTER SN*12,ZN*12
      character DESCRD*5,DESCRJ,DDX*7,DDX1*10,DDX2*8,DHX*5
      character outs*124

C Read in the zone geometry data.
      IUNIT=IFIL+1
      call georead(IUNIT,LGEOM(IZONE),IZONE,0,IUOUT,IER)

C Read construction file to get surface emissivities.
      CALL ECONST(LTHRM(IZONE),IUNIT,IZONE,0,IUOUT,IER)
      IF(IER.NE.0)RETURN

C Determine which surface is of interest.
      if(isfn.eq.0)CALL ASKSUR(IZONE,ISFN)

C Determine the 'other' zone and connection information
C and then if the surface is transparent or opaque.
      CALL SURADJ(IZONE,ISFN,IE,T,IZC,ISC,ICN,CSTR)
      IF(SOTF(IZONE,ISFN)(1:4).EQ.'OPAQ'.OR.
     &   SOTF(IZONE,ISFN)(1:4).EQ.'UNKN')THEN
        TSTR='opaque MLC'
      ELSEIF(SOTF(IZONE,ISFN)(1:4).EQ.'CFC ')THEN
        TSTR='CFC const.'
      ELSEIF(SOTF(IZONE,ISFN)(1:4).EQ.'CFC2')THEN
        TSTR='CFC const.'
      ELSE
        TSTR='trnsp. MLC'
      ENDIF

C Reset ouput channel if redirecting.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C Output general header information for load or energy use.
C Get surface and zone name.
      SN=SNAME(IZONE,ISFN)
      ZN=zname(IZONE)
      if(SNAPSH)then
        call edisp(itru,' ')
        write(outs,44)SN(1:lnblnk(SN)),ISFN,ZN(1:lnblnk(ZN)),IZONE
   44   FORMAT('Causal load breakdown (W) for ',A,' (',I3,')',
     &         ' in ',A,' (',I3,') ')
        call edisp(itru,outs)
        WRITE(outs,43)TSTR,SNA(IZONE,ISFN),CSTR
        call edisp(itru,outs)

        CALL STDATE(IYEAR,IODS,DDX,DDX1,DDX2)
        CALL ESTIME(NTS,IDAVER,IOH1*NTS,DHX,DESCRD,DESCRJ,TIMER)
        write(outs,'(A,A10,A,A5)') 'Time: ',DDX1,'@',DHX
        call edisp(itru,outs)
      else
        call edisp(itru,' ')
        WRITE(outs,42)SN(1:lnblnk(SN)),ISFN,ZN(1:lnblnk(ZN)),IZONE
   42   FORMAT('Causal energy breakdown (Whrs) for ',A,' (',I3,')',
     &         ' in ',A,' (',I3,') ')
        CALL EDISP(itru,outs)
        WRITE(outs,43)TSTR,SNA(IZONE,ISFN),CSTR
   43   FORMAT('Surface is ',A10,', area=',F6.2,'m^2 & connects to',
     &    A25)
        CALL EDISP(itru,outs)
      endif

C Print breakdown headings: plant status or gains/loss.
      IF(IANZSE.EQ.4)THEN
        CALL MOSCS1(IZONE,ISFN,IANZSE,IE,IZC)
      ELSEIF(IANZSE.EQ.5)THEN
        CALL MOSCS2(IZONE,ISFN,IANZSE,IE,IZC)
      ENDIF

      RETURN
      END

C ********** MOSCS1

C Display breakdown by plant status.

      SUBROUTINE MOSCS1(IZONE,ISFN,IANZSE,IE,IZC)
#include "building.h"
#include "schedule.h"

      COMMON/OUTPCH/ICOUT
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DIMENSION IUSERD(MCOM,2)

      CHARACTER*24 LABELS(15)
      CHARACTER*15 TITLE1(15)
      character outs*124

C Initial labels using user defined labels if available.
      LABELS(1) =' Conductive flux        '
      LABELS(2) =' Convective flux        '
      LABELS(3) =' Longwave rad inside    '
      LABELS(4) =' Longwave rad surround  '
      LABELS(5) =' Longwave rad sky       '
      LABELS(6) =' Longwave rad ground    '
      LABELS(7) =' Shortwave radiation    '
      write(LABELS(8),'(a,a)') ' Casual ',lodlabel(izone,1)
      write(LABELS(9),'(a,a)') ' Casual ',lodlabel(izone,2)
      write(LABELS(10),'(a,a)')' Casual ',lodlabel(izone,3)
      if(lodslot(izone,4).ne.0)then
        write(LABELS(11),'(a,a)')' Casual ',lodlabel(izone,4)
      else
        write(LABELS(11),'(a)')  ' Casual --'
      endif
      LABELS(12)=' Controlled casual gn   '
      LABELS(13)=' Heat stored            '
      LABELS(14)=' Plant                  '
      LABELS(15)=' Totals                 '
      TITLE1(1)='|Cond flx'
      TITLE1(2)='|Conv flx'
      TITLE1(3)='|LW rad  '
      TITLE1(4)='|LW >surr'
      TITLE1(5)='|LW >sky '
      TITLE1(6)='|LW >grnd'
      TITLE1(7)='|SW rad  '
      write(TITLE1(8),'(a,a)')'|UC',lodlabel(izone,1)
      write(TITLE1(9),'(a,a)')'|UC',lodlabel(izone,2)
      write(TITLE1(10),'(a,a)')'|UC',lodlabel(izone,3)
      write(TITLE1(11),'(a,a)')'|UC',lodlabel(izone,4)
      TITLE1(12)='|Cnt cas '
      TITLE1(13)='|Heat str'
      TITLE1(14)='|Plant   '
      TITLE1(15)='|........'
      
C VALS storage locations described in MOSSUM.

C Redirect output if required.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

C Print breakdown headings: plant status.
      CALL EDISP(itru,' ')
      WRITE(outs,'(A,I3)')'Facing Zone ',IZONE
      CALL EDISP(itru,outs)
      WRITE(outs,'(17X,A)')
     & 'Heating period        Cooling period          OFF period'
      CALL EDISP(itru,outs)
      WRITE(outs,'(17X,A)')
     & 'Gain      Loss        Gain      Loss        Gain      Loss'
      CALL EDISP(itru,outs)

C Perform sums and averages for this surface (IUSERD passed as a dummy).
      DO 10 I=1,3
        IUSERD(I,1)=0
        IUSERD(I,2)=0
   10 CONTINUE
      CALL MOSSUM(IZONE,ISFN,IANZSE,IE,IUSERD)

C Output results.
      DO 152 I=1,3
        WRITE(outs,86)TITLE1(I),(VAL2(I,J),J=1,6)
        CALL EDISP(itru,outs)
  152 CONTINUE

      DO 151 I=4,6
        WRITE(outs,87)TITLE1(I)
        CALL EDISP(itru,outs)
  151 CONTINUE

      DO 150 I=7,14
        WRITE(outs,86)TITLE1(I),(VAL2(I-3,J),J=1,6)
        CALL EDISP(itru,outs)
  150 CONTINUE

   86 FORMAT(A,6F11.2)
   87 FORMAT(A,8X,'--         --         --         --         --',
     &             '       --')

C Totals: use VAL2(I,22): I=1-6: gain/loss for heating ,cooling, off.
      DO 160 I=1,6
        VAL2(I,22)=0.
  160 CONTINUE

      DO 170 I=1,6
        DO 180 K=1,14
          VAL2(I,22)=VAL2(I,22)+VAL2(K,I)
  180   CONTINUE
  170 CONTINUE
      WRITE(outs,86)LABELS(15),(VAL2(I,22),I=1,6)
      CALL EDISP(itru,outs)

      RETURN
      END

C ********** MOSCS2

C MOSCS2 provides I/O for gain/loss breakdown for surfaces.
C CALLS: MOSSUM MOFADJ    CALLED BY: MOSCAS

      SUBROUTINE MOSCS2(IZONE,ISFN,IANZSE,IE,IZC)

#include "building.h"
#include "geometry.h"
#include "schedule.h"
      COMMON/OUTPCH/ICOUT
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DIMENSION IUSERD(MCOM,2)

      CHARACTER*24 LABELS(15)
      character outs*124

C Initial labels, 8-11 will be overwritten if user labels are available.
      LABELS(1)=' Conductive flux        '
      LABELS(2)=' Convective flux        '
      LABELS(3)=' Longwave rad inside    '
      LABELS(4)=' Longwave rad buildings '
      LABELS(5)=' Longwave rad sky       '
      LABELS(6)=' Longwave rad ground    '
      LABELS(7)=' Shortwave radiation    '
      write(LABELS(8),'(a,a)') ' Casual ',lodlabel(izone,1)
      write(LABELS(9),'(a,a)') ' Casual ',lodlabel(izone,2)
      write(LABELS(10),'(a,a)')' Casual ',lodlabel(izone,3)
      if(lodslot(izone,4).ne.0)then
        write(LABELS(11),'(a,a)')' Casual ',lodlabel(izone,4)
      else
        write(LABELS(11),'(a)')  ' Casual --'
      endif
      LABELS(12)=' Controled casual gn   '
      LABELS(13)=' Heat stored           '
      LABELS(14)=' Plant                 '
      LABELS(15)=' Totals                '

C VALS storage locations described in MOSSUM.

C Redirect output if required.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

C Print breakdown headings: gain/loss.
      IF(IE.EQ.0)THEN
        CALL EDISP(itru,' ')   
        if(IZC.eq.0)then     
          WRITE(outs,'(30X,2a,6x,a)')' Facing ',zname(izone),
     &      'Facing outside'
        else
          WRITE(outs,'(30X,2a,6x,a)')' Facing ',zname(izone),'Facing ',
     &      zname(IZC)
        endif
        CALL EDISP(itru,outs)        
        WRITE(outs,'(30X,A)')
     &  ' Gain       Loss          Gain       Loss'
        CALL EDISP(itru,outs)        
      ELSE
        CALL EDISP(itru,' ')        
        WRITE(outs,'(30X,2A)')' Facing ',zname(izone)
        CALL EDISP(itru,outs)        
        WRITE(outs,'(30X,A)')' Gain       Loss'
        CALL EDISP(itru,outs)        
      ENDIF

C Perform sums and averages for this surface (IUSERD passed as a dummy).
      DO 10 I=1,3
        IUSERD(I,1)=0
        IUSERD(I,2)=0
   10 CONTINUE
      CALL MOSSUM(IZONE,ISFN,IANZSE,IE,IUSERD)

C Output results. Use the slots as per lines 544-559 in this file.
      IF(IE.EQ.0)THEN
        WRITE(outs,81)LABELS(1),(VAL2(1,J),J=1,2),(VAL2(12,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(2),(VAL2(2,J),J=1,2),(VAL2(13,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,84)LABELS(3),(VAL2(3,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(4),(VAL2(14,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(5),(VAL2(15,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(6),(VAL2(16,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(7),(VAL2(4,J),J=1,2),(VAL2(17,J),J=1,2)
        CALL EDISP(itru,outs)

        DO 150 I=8,12   ! for the casual gains
          WRITE(outs,84)LABELS(I),(VAL2(I-3,J),J=1,2)
          CALL EDISP(itru,outs)
  150   CONTINUE
        WRITE(outs,81)LABELS(13),(VAL2(10,J),J=1,2),(VAL2(18,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(14),(VAL2(11,J),J=1,2),(VAL2(19,J),J=1,2)
        CALL EDISP(itru,outs)
      ELSE
        WRITE(outs,82)LABELS(1),(VAL2(1,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,82)LABELS(2),(VAL2(2,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,82)LABELS(3),(VAL2(3,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,86)LABELS(4)
        CALL EDISP(itru,outs)
        WRITE(outs,86)LABELS(5)
        CALL EDISP(itru,outs)
        WRITE(outs,86)LABELS(6)
        CALL EDISP(itru,outs)

        DO 151 I=7,14
          WRITE(outs,81)LABELS(I),(VAL2(I-3,J),J=1,2)
          CALL EDISP(itru,outs)
  151   CONTINUE

      ENDIF

   81 FORMAT(A,4F12.2)
   82 FORMAT(A,2F12.2)
   83 FORMAT(A,8X,' --         --  ',2F20.2)
   84 FORMAT(A,2F12.2,8X,' --          --  ')
   86 FORMAT(A24,8X,' --         -- ')

C Totals: sum inside gain into QGINS, inside loss into QLINS,
C outside gain into QGOUT, and outside loss into QLOUT.
      QGINS=0.
      QLINS=0.
      QGOUT=0.
      QLOUT=0.

      DO 180 K=1,11
        QGINS=QGINS+VAL2(K,1)
        QLINS=QLINS+VAL2(K,2)
  180 CONTINUE

      IF(IE.EQ.0)THEN
        DO 182 K=12,19 
          QGOUT=QGOUT+VAL2(K,1)
          QLOUT=QLOUT+VAL2(K,2)
  182   CONTINUE
      ENDIF

      IF(IE.EQ.0)THEN
        WRITE(outs,81)LABELS(15),QGINS,QLINS,QGOUT,QLOUT
        CALL EDISP(itru,outs)
      ELSE
        WRITE(outs,82)LABELS(15),QGINS,QLINS
        CALL EDISP(itru,outs)
      ENDIF

      RETURN
      END


C ******************** MOSSUM ********************

C Provide sums and averages for surface energy breakdowns.
C CALLS: MOSRGT    CALLED BY: MOSCS1 MOSCS2

      SUBROUTINE MOSSUM(IZONE,ISFN,IANZSE,IE,IUSERD)
#include "building.h"

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      DIMENSION XVAL1(MZS)
      DIMENSION IUSERD(MCOM,2)

C Display in Whrs for surfaces.
      TCMULT=FLOAT(NOUT)/FLOAT(NTS)

C Clear the summation locations: VAL2(I,J) and temp XVAL1.
      DO 20 IV=1,MZS
        XVAL1(IV)=0.
        DO 201 JV=1,7
          VAL2(IV,JV)=0.
  201   CONTINUE
   20 CONTINUE

C Day-by-day recovery.
      DO 30 I=IODS,IODF
        ID=I
        CALL CHKTIME(ID,ISTART,IEND)

C For requested zone and for every computational time-step,
C get results for complete day (held in VAL1).
        CALL MOSRGT(IZONE,ISFN,ID,IE)

C Stepping at defined output interval.
        DO 40 J=ISTART,IEND,NOUT

C Compute average for range to be passed for display.
          irange=MZS
          CALL VALIAV(irange,J,XVAL1)

C Breakdown by plant status: summations held in VAL2(I,J)
C where I=1 to 11 corresponds to the energy flows and J=1 to 6
C corresponds to gains and losses for heating,cooling,off.
C Only the data for the side facing the zone is presented.
C (XVAL1(20) is the plant injection for the time step.
          IF(IANZSE.EQ.4)THEN
            IF(XVAL1(20).LT.0.)THEN
              DO 110 K=1,11
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,4)=VAL2(K,4)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,3)=VAL2(K,3)+XVAL1(K)*TCMULT
                ENDIF
  110         CONTINUE
            ELSEIF(XVAL1(20).GT.0.)THEN
              DO 120 K=1,11
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
                ENDIF
  120         CONTINUE
            ELSE
              DO 100 K=1,11
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,6)=VAL2(K,6)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,5)=VAL2(K,5)+XVAL1(K)*TCMULT
                ENDIF
  100         CONTINUE
            ENDIF
          ENDIF

C Breakdown in terms of net gain and loss: summations held in VAL2(I,J)
C where I=1 to 11 are for inside face and 12 to 19 are for the outside
C face of the surface (calculated only if true outside). J=1 & 2
C relates to GAINS and LOSSES.
          IF(IANZSE.EQ.5)THEN
            DO 130 K=1,11
              IF(XVAL1(K).LE.0.)THEN
                VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
              ELSE
                VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
              ENDIF
  130       CONTINUE

            IF(IE.EQ.0)THEN
              DO 131 K=12,19
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
                ENDIF
  131         CONTINUE
            ENDIF
          ENDIF
   40   CONTINUE
   30 CONTINUE
      RETURN

      END

C ********** MOSRGT

C Display breakdown by plant status. Recovers from the
C results library information for surface flux breakdown
C for any requested zone and day.

      SUBROUTINE MOSRGT(IZONE,ISFN,IDAY,IE)
#include "building.h"
#include "geometry.h"
#include "control.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      common/recver/izver,ipver,iever

      integer icascf
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
C      real FRAC ! controlled fraction.
C      real perocupc,perocupr,perocupl ! average occupant to write out
C      real perlightc,perlightr,perlightl ! average lighting to write out
C      real perequipc,perequipr,perequipl ! average equipment to write out
C      real perotherc,perotherr,perotherl ! average other to write out
C      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      ICF=ICASCF(IZONE)
      N=24*NTS

C VAL1 storage locations:
C                   inside  otherside
C   Conductive flux  ',1     12
C   Convective flux  ',2     13
C   Longwave rad int ',3
C   LW rad ext >bldg ',      14
C   LW rad ext >sky  ',      15
C   LW rad ext >grnd ',      16
C   Shortwave rad.   ',4     17
C   Unctrl cas typ 1 ',5
C          cas typ 2 ',6
C          cas typ 3 ',7
C          cas typ 4 ',8
C   Cntrld casual    ',9
C   Heat stored       ,10    18
C   Plant             ,11    19
C   Zone plant        ,20
C   Zone air          ,21
C   Inside surf temp  ,22
C   Outside surf temp ,XDUM (temp array)

C Clear VAL1.
      DO 18 I=1,MZS
        DO 19 II=1,N
          VAL1(I,II)=0.
   19   CONTINUE
   18 CONTINUE

C Radiant plant is area AND emissivity weighted.
      emarea=0.0
      do 80 kk=1,NZSUR(izone)
        emarea=emarea+sna(izone,kk)*EMISI(kk)
  80  continue

C Find looping range for the day in question.
      CALL CHKTIME(IDAY,ISTART,IEND)

C If izver is 4 then recover casual gain formation at start of
C the day (to find out which casual gain is controlled). Then
C use the explicitly saved radiant data.
C      if(izver.ge.4)then
C        IT=1
C        call getallcas(IDAY,IZONE,ISET,IT,QCASR,QCASC,QCASL,FRAC,
C     &    perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
C     &    perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
C     &    theonectld)
C      endif

C Collect each of the surface balance fluxes. The values saved for
C avqcrioc,avqcrili,avqcrisp,avqcrioth differ from ZCASR recovered
C data so there might be some issues in bps SURBAL routine.
      DO 198 IT=ISTART,IEND
        CALL GSURFB(IDAY,IZONE,ISFN,IT,TSout,qswrdi,qlwrdi,qconvi,
     &    qcondi,qstori,qstore,qconde,qconve,qlwrde,qlwrbd,
     &    qlwrsk,qswrde,avqcrioc,avqcrili,avqcrisp,avqcrioth)
        VAL1(4,IT)=qswrdi
        VAL1(3,IT)=qlwrdi
        VAL1(2,IT)=qconvi
        VAL1(1,IT)=qcondi
        VAL1(10,IT)=qstori
        VAL1(18,IT)=qstore
        VAL1(12,IT)=qconde
        VAL1(13,IT)=qconve
        VAL1(16,IT)=qlwrde
        VAL1(14,IT)=qlwrbd
        VAL1(15,IT)=qlwrsk
        VAL1(17,IT)=qswrde

C Include the radiant casual directly if izver >=4. Use slot
C VAL1(9,I) for which ever one is the controlled gain. Debug
C needed for this.
C        if(izver.ge.4)then
C          if(theonectld.eq.0)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=avqcrioth
C            VAL1(9,IT)=0.0
C          elseif(theonectld.eq.1)then
C            VAL1(5,IT)=0.0
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=avqcrioth
C            VAL1(9,IT)=avqcrioc
C          elseif(theonectld.eq.2)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=0.0
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=avqcrioth
C            VAL1(9,IT)=avqcrili
C          elseif(theonectld.eq.3)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=0.0
C            VAL1(8,IT)=avqcrioth
C            VAL1(9,IT)=avqcrisp
C          elseif(theonectld.eq.3)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=0.0
C            VAL1(9,IT)=avqcrioth
C          endif
C        endif
  198 CONTINUE

C Get zone air temperature, store in VAL1(22,N).
      ISET=ISIM
      CALL GZAIRT(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,21)

C Get inside surface temperature.
      CALL GTMS(IDAY,IZONE,ISFN,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,22)

C Get inside casual radiant amount for this surface and store
C uncontrolled types 1,2,3,4 in VAL1(5,I) to VAL1(8,I).
C As there is only one controlled lump all to slot VAL1(9,I).
      CALL ZCASR(IZONE,ISFN,IDAY)
      DO 28 I=1,N
        VAL1(5,I)=VAL3(15,I)
        VAL1(6,I)=VAL3(16,I)
        VAL1(7,I)=VAL3(17,I)
        VAL1(8,I)=VAL3(18,I)
        VAL1(9,I)=VAL3(19,I)+VAL3(20,I)+VAL3(21,I)+VAL3(22,I)
   28 CONTINUE

C Get the plant injection at each time step, save zone amount in
C VAL1(20,N) for use in sorting by plant heating/cooling/off and
C for deriving radiant portion of plant.
      CALL GZQM(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,20)

C Determine the radiant portion of the plant via checking IBAN.
C Radiant only to one surface node, mixed plant injection, or
C no radiant plant component.
      IF(ICF.GT.0)THEN
        IF(IBAN(ICF,1).EQ.IZONE.AND.IBAN(ICF,2).EQ.ISFN.AND.
     &                                       IBAN(ICF,3).EQ.0)then
          DO 35 I=ISTART,IEND
            VAL1(11,I)=VAL1(20,I)
   35     CONTINUE
        ELSEIF(IBAN(ICF,1).EQ.-2.AND.
     &      (IBAN(ICF,2).EQ.IZONE.OR.IBAN(ICF,2).EQ.0))then
          DO 36 I=ISTART,IEND
            VAL1(11,I)=(sna(izone,ISFN)*(EMISI(ISFN)/emarea))*
     &                  VAL1(20,I)*(1.-(FLOAT(IBAN(ICF,3))*0.01))
   36     CONTINUE
        ELSE
          DO 37 I=ISTART,IEND
            VAL1(11,I)=0.
   37     CONTINUE
        ENDIF
      ELSE
        DO 137 I=ISTART,IEND
          VAL1(11,I)=0.
  137   CONTINUE
      ENDIF

C Assume other side plant to be zero at this stage of program development.
      DO 38 I=ISTART,IEND
        VAL1(19,I)=0.
   38 CONTINUE

      RETURN
      END

