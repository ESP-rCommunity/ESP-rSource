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

C  MOSCAS  Selection/control for surface causal energy breakdown.
C  MOSCS1  Display breakdown by plant status.
C  MOSCS2  Display breakdown as normal gain/loss.
C  MOSSUM  Collect/sum surface causal data.
C  MOSRGT  Get surface data from results lib.

C ******************** MOSCAS ********************
C MOSCAS process breakdown data for surfaces.
C CALLS: MOSCS1 MOSCS2    CALLED BY: MOENER

      SUBROUTINE MOSCAS(IZONE,ISFN,IANZSE)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL

      COMMON/AVRAGE/IDAVER
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/RECV5/SNA(MCOM,MS)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/SNAP/SNAPSH
      logical SNAPSH
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER TSTR*10,CSTR*25
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER SN*12,ZN*12
      character DESCRD*5,DDX*7,DDX1*10,DHX*5,outs*124
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS

C Read in the zone geometry data.
      newgeo=.false.  ! assume older format geometry.
      IUNIT=IFIL+1
      call eclose(gversion(izone),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IUNIT,LGEOM(IZONE),IZONE,0,IUOUT,IER)
      else
        call egomin(IUNIT,LGEOM(IZONE),IZONE,0,0,IUOUT,IER)
      endif

C Read (silently) construction file to get surface emissivities.
      CALL ECONST(LTHRM(IZONE),IUNIT,IZONE,0,IUOUT,IER)
      IF(IER.NE.0)RETURN

C If izver >= 4 then there is no need to read the operation file.
      if(izver.gt.4)then
        continue
      else

C Assume that the operation file name has been read in via MOINIT.
C Read in the zone operational file data.
        CALL EROPER(0,iuout,iunit,izone,IER) ! for older files
        if(ip3ver(izone).eq.0)then
          write(outs,'(a,i2,a)') 
     &    'Operation file for zone ',izone,' periods being sorted!'
          call edisp(iuout,outs)
          call PROCESSOLDCAS(izone,0,iuout,IER)
        endif
        IF(IER.EQ.1)RETURN

C Read casual gain control file.
        IF(ICGC(IZONE).EQ.1)CALL ercgcf(0,iuout,LCGCIN(izone),IZONE,ier)
      endif

C Determine which surface is of interest.
      if(isfn.eq.0)CALL ASKSUR(IZONE,ISFN)

C Determine the 'other' zone and connection information
C and then if the surface is transparent or opaque.
      CALL SURADJ(IZONE,ISFN,IE,T,IZC,ISC,ICN,CSTR)
      IF(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &   SSOTF(ICN)(1:4).EQ.'UNKN')THEN
        TSTR='opaque MLC'
      ELSEIF(SSOTF(ICN)(1:4).EQ.'CFC ')THEN
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
      SN=SSNAME(ICN)
      ZN=zname(IZONE)
      if(SNAPSH)then
        call edisp(itru,' ')
        write(outs,44)SN(1:lnblnk(SN)),ISFN,ZN(1:lnblnk(ZN)),IZONE
   44   FORMAT(' Causal load breakdown (Watts) for ',A,' (',I2,')',
     &         ' in ',A,' (',I2,') ')
        call edisp(itru,outs)
        WRITE(outs,43)TSTR,SNA(IZONE,ISFN),CSTR
        call edisp(itru,outs)

        CALL STDATE(IYEAR,IODS,DDX,DDX1)
        CALL ESTIME(NTS,IDAVER,IOH1*NTS,DHX,DESCRD,TIMER)
        write(outs,'(A,A10,A,A5)') ' Time: ',DDX1,'@',DHX
        call edisp(itru,outs)
      else
        call edisp(itru,' ')
        WRITE(outs,42)SN(1:lnblnk(SN)),ISFN,ZN(1:lnblnk(ZN)),IZONE
   42   FORMAT(' Causal energy breakdown (Whrs) for ',A,' (',I2,')',
     &         ' in ',A,' (',I2,') ')
        CALL EDISP(itru,outs)
        WRITE(outs,43)TSTR,SNA(IZONE,ISFN),CSTR
   43   FORMAT(' Surface is ',A10,', area=',F6.2,'m^2 & connects to',
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

C ******************** MOSCS1 ********************

C MOSCS1 provides I/O for heating/cooling breakdown for surfaces.

      SUBROUTINE MOSCS1(IZONE,ISFN,IANZSE,IE,IZC)

#include "building.h"
      COMMON/OUTPCH/ICOUT
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      common/loadlabel/lodlabel(mcom,7)

      DIMENSION IUSERD(MCOM,2)

      CHARACTER*18 LABELS(16)
      CHARACTER*9 TITLE1(16)
      character outs*124,lodlabel*6

C Initial labels using user defined labels if available.
      LABELS(1) =' Conductive flux  '
      LABELS(2) =' Convective flux  '
      LABELS(3) =' Longwave rad int '
      LABELS(4) =' LW rad ext >bldg '
      LABELS(5) =' LW rad ext >sky  '
      LABELS(6) =' LW rad ext >grnd '
      LABELS(7) =' Shortwave rad.   '
      write(LABELS(8),'(1x,a,a)')lodlabel(izone,1),' casual gn'
      write(LABELS(9),'(1x,a,a)')lodlabel(izone,2),' casual gn'
      write(LABELS(10),'(1x,a,a)')lodlabel(izone,3),' casual gn'
      LABELS(11)=' Controled casual '
      LABELS(12)=' N/A              '
      LABELS(13)=' N/A              '
      LABELS(14)=' Heat stored      '
      LABELS(15)=' Plant            '
      LABELS(16)=' Totals           '
      TITLE1(1)='|Cond flx'
      TITLE1(2)='|Conv flx'
      TITLE1(3)='|LW rad  '
      TITLE1(4)='|LW >bldg'
      TITLE1(5)='|LW >sky '
      TITLE1(6)='|LW >grnd'
      TITLE1(7)='|SW rad  '
      write(TITLE1(8),'(a,a)')'|UC',lodlabel(izone,1)
      write(TITLE1(9),'(a,a)')'|UC',lodlabel(izone,2)
      write(TITLE1(10),'(a,a)')'|UC',lodlabel(izone,3)
      TITLE1(11)='|Cnt cas '
      TITLE1(12)='|        '
      TITLE1(13)='|        '
      TITLE1(14)='|Heat str'
      TITLE1(15)='|Plant   '
      TITLE1(16)='|........'
      
C VALS storage locations described in MOSSUM.

C Redirect output if required.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

C Print breakdown headings: plant status.
      CALL EDISP(itru,' ')
      WRITE(outs,'(A,I2)')' Facing Zone ',IZONE
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

      DO 150 I=7,15
        WRITE(outs,86)TITLE1(I),(VAL2(I-3,J),J=1,6)
        CALL EDISP(itru,outs)
  150 CONTINUE

   86 FORMAT(A9,6F11.2)
   87 FORMAT(A9,8X,'--         --         --         --         --',
     &             '       --')

C Totals: use VAL2(I,7): I=1-6: gain/loss for heating ,cooling, off.
      DO 160 I=1,6
        VAL2(I,7)=0.
  160 CONTINUE

      DO 170 I=1,6
        DO 180 K=1,12
          VAL2(I,7)=VAL2(I,7)+VAL2(K,I)
  180   CONTINUE
  170 CONTINUE
      WRITE(outs,86)LABELS(16),(VAL2(I,7),I=1,6)
      CALL EDISP(itru,outs)

      RETURN
      END

C ******************** MOSCS2 ********************

C MOSCS2 provides I/O for gain/loss breakdown for surfaces.
C CALLS: MOSSUM MOFADJ    CALLED BY: MOSCAS

      SUBROUTINE MOSCS2(IZONE,ISFN,IANZSE,IE,IZC)

#include "building.h"
#include "geometry.h"
      COMMON/OUTPCH/ICOUT
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      common/loadlabel/lodlabel(mcom,7)

      DIMENSION IUSERD(MCOM,2)

      CHARACTER*18 LABELS(16)
      character outs*124,lodlabel*6

C Initial labels, 8-13 will be overwritten if user labels are available.
      LABELS(1)=' Conductive flux  '
      LABELS(2)=' Convective flux  '
      LABELS(3)=' Longwave rad int '
      LABELS(4)=' LW rad ext >bldg '
      LABELS(5)=' LW rad ext >sky  '
      LABELS(6)=' LW rad ext >grnd '
      LABELS(7)=' Shortwave rad.   '
      write(LABELS(8),'(1x,a,a)')lodlabel(izone,1),' casual gn'
      write(LABELS(9),'(1x,a,a)')lodlabel(izone,2),' casual gn'
      write(LABELS(10),'(1x,a,a)')lodlabel(izone,3),' casual gn'
      LABELS(11)=' Controled casual '
      LABELS(12)=' N/A              '
      LABELS(13)=' N/A              '
      LABELS(14)=' Heat stored      '
      LABELS(15)=' Plant            '
      LABELS(16)=' Totals           '

C VALS storage locations described in MOSSUM.

C Redirect output if required.
      itru = icout
      if(ixopen.eq.1)itru = ixunit

C Print breakdown headings: gain/loss.
      IF(IE.EQ.0)THEN
        CALL EDISP(itru,' ')   
        if(IZC.eq.0)then     
          WRITE(outs,'(24X,2a,6x,a)')' Facing ',zname(izone),
     &      'Facing outside'
        else
          WRITE(outs,'(24X,2a,6x,a)')' Facing ',zname(izone),'Facing ',
     &      zname(IZC)
        endif
        CALL EDISP(itru,outs)        
        WRITE(outs,'(24X,A)')
     &  ' Gain       Loss          Gain       Loss'
        CALL EDISP(itru,outs)        
      ELSE
        CALL EDISP(itru,' ')        
        WRITE(outs,'(24X,2A)')' Facing ',zname(izone)
        CALL EDISP(itru,outs)        
        WRITE(outs,'(24X,A)')' Gain       Loss'
        CALL EDISP(itru,outs)        
      ENDIF

C Perform sums and averages for this surface (IUSERD passed as a dummy).
      DO 10 I=1,3
        IUSERD(I,1)=0
        IUSERD(I,2)=0
   10 CONTINUE
      CALL MOSSUM(IZONE,ISFN,IANZSE,IE,IUSERD)

C Output results.
      IF(IE.EQ.0)THEN
        WRITE(outs,81)LABELS(1),(VAL2(1,J),J=1,2),(VAL2(13,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(2),(VAL2(2,J),J=1,2),(VAL2(14,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,84)LABELS(3),(VAL2(3,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(4),(VAL2(15,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(5),(VAL2(16,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,83)LABELS(6),(VAL2(17,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(7),(VAL2(4,J),J=1,2),(VAL2(18,J),J=1,2)
        CALL EDISP(itru,outs)

C Skip un-used slots 12 and 13.
        DO 150 I=8,11
          WRITE(outs,84)LABELS(I),(VAL2(I-3,J),J=1,2)
          CALL EDISP(itru,outs)
  150   CONTINUE
        WRITE(outs,81)LABELS(14),(VAL2(11,J),J=1,2),(VAL2(19,J),J=1,2)
        CALL EDISP(itru,outs)
        WRITE(outs,81)LABELS(15),(VAL2(12,J),J=1,2),(VAL2(20,J),J=1,2)
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

C Skip over the unused items 12 and 13.
        DO 151 I=7,15
          if(i.ne.12.and.i.ne.13)then
            WRITE(outs,81)LABELS(I),(VAL2(I-3,J),J=1,2)
            CALL EDISP(itru,outs)
          endif
  151   CONTINUE

      ENDIF

   81 FORMAT(A18,4F12.2)
   82 FORMAT(A18,2F12.2)
   83 FORMAT(A18,8X,' --         --  ',2F12.2)
   84 FORMAT(A18,2F12.2,8X,' --          --  ')
   86 FORMAT(A18,8X,' --         -- ')

C Totals: sum inside gain into QGINS, inside loss into QLINS,
C outside gain into QGOUT, and outside loss into QLOUT.
      QGINS=0.
      QLINS=0.
      QGOUT=0.
      QLOUT=0.

      DO 180 K=1,12
        QGINS=QGINS+VAL2(K,1)
        QLINS=QLINS+VAL2(K,2)
  180 CONTINUE

      IF(IE.EQ.0)THEN
        DO 182 K=13,20
          QGOUT=QGOUT+VAL2(K,1)
          QLOUT=QLOUT+VAL2(K,2)
  182   CONTINUE
      ENDIF

      IF(IE.EQ.0)THEN
        WRITE(outs,81)LABELS(16),QGINS,QLINS,QGOUT,QLOUT
        CALL EDISP(itru,outs)
      ELSE
        WRITE(outs,82)LABELS(16),QGINS,QLINS
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
C where I=1 to 12 corresponds to the energy flows and J=1 to 6
C corresponds to gains and losses for heating,cooling,off.
C Only the data for the side facing the zone is presented.
C (XVAL1(21) is the plant injection for the time step.
          IF(IANZSE.EQ.4)THEN
            IF(XVAL1(21).LT.0.)THEN
              DO 110 K=1,12
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,4)=VAL2(K,4)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,3)=VAL2(K,3)+XVAL1(K)*TCMULT
                ENDIF
  110         CONTINUE
            ELSEIF(XVAL1(21).GT.0.)THEN
              DO 120 K=1,12
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
                ENDIF
  120         CONTINUE
            ELSE
              DO 100 K=1,12
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,6)=VAL2(K,6)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,5)=VAL2(K,5)+XVAL1(K)*TCMULT
                ENDIF
  100         CONTINUE
            ENDIF
          ENDIF

C Breakdown in terms of net gain and loss: summations held in VAL2(I,J)
C where I=1 to 12 are for inside face and 13 to 20 are for the outside
C face of the surface (calculated only if true outside). J=1 & 2
C relates to GAINS and LOSSES.
          IF(IANZSE.EQ.5)THEN
            DO 130 K=1,12
              IF(XVAL1(K).LE.0.)THEN
                VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
              ELSE
                VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
              ENDIF
  130       CONTINUE

            IF(IE.EQ.0)THEN
              DO 131 K=13,20
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

C ******************** MOSRGT ********************

C MOSRGT recovers from the results library information
C for surface flux breakdown for any requested zone and day.
C This will require quite a bit of work; try to include some
C of the surface convection, conduction, storage calcs here.

C Includes test code to read the explicit radiant gains by
C type. This is work-in-progress and has been commented out
C until the glitches have been sorted.

      SUBROUTINE MOSRGT(IZONE,ISFN,IDAY,IE)
#include "building.h"
#include "geometry.h"
#include "control.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV5/SNA(MCOM,MS)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
C      common/recver/izver,ipver,iever

      COMMON/CCTL/ICASCF(MCOM)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
C      real FRAC ! controlled fraction.
C      real perocupc,perocupr,perocupl ! average occupant to write out
C      real perlightc,perlightr,perlightl ! average lighting to write out
C      real perequipc,perequipr,perequipl ! average equipment to write out
C      real otherc,otherr,otherl ! average other (future expansion) to write out
C      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      ICF=ICASCF(IZONE)
      N=24*NTS

C VAL1 storage locations:
C                   inside  otherside
C   Conductive flux  ',1     13
C   Convective flux  ',2     14
C   Longwave rad int ',3
C   LW rad ext >bldg ',      15
C   LW rad ext >sky  ',      16
C   LW rad ext >grnd ',      17
C   Shortwave rad.   ',4     18
C   Unctrl cas typ 1 ',5
C          cas typ 2 ',6
C          cas typ 3 ',7
C   Cntrld casual    ',8
C                    ',9
C                    ',10
C   Heat stored       ,11    19
C   Plant             ,12    20
C   Zone plant        ,21
C   Zone air          ,22
C   Inside surf temp  ,23
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
C     &    perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
C     &    perequipr,otherr,perocupl,perlightl,perequipl,otherl,
C     &    theonectld)
C      endif

C Collect each of the surface balance fluxes.
      DO 198 IT=ISTART,IEND
        CALL GSURFB(IDAY,IZONE,ISFN,IT,TSout,qswrdi,qlwrdi,qconvi,
     &    qcondi,qstori,qstore,qconde,qconve,qlwrde,qlwrbd,
     &    qlwrsk,qswrde,avqcrioc,avqcrili,avqcrisp,avqcrioth)
        VAL1(4,IT)=qswrdi
        VAL1(3,IT)=qlwrdi
        VAL1(2,IT)=qconvi
        VAL1(1,IT)=qcondi
        VAL1(11,IT)=qstori
        VAL1(19,IT)=qstore
        VAL1(13,IT)=qconde
        VAL1(14,IT)=qconve
        VAL1(17,IT)=qlwrde
        VAL1(15,IT)=qlwrbd
        VAL1(16,IT)=qlwrsk
        VAL1(18,IT)=qswrde

C Include the radiant casual directly if izver >=4. Use slot
C VAL1(8,I) for which ever one is the controlled gain.
C        if(izver.ge.4)then
C          if(theonectld.eq.0)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=0.0
C            VAL1(9,IT)=0.0
C            VAL1(10,IT)=0.0
C          elseif(theonectld.eq.1)then
C            VAL1(5,IT)=0.0
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=avqcrioc
C            VAL1(9,IT)=0.0
C            VAL1(10,IT)=0.0
C          elseif(theonectld.eq.2)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=0.0
C            VAL1(7,IT)=avqcrisp
C            VAL1(8,IT)=avqcrili
C            VAL1(9,IT)=0.0
C            VAL1(10,IT)=0.0
C          elseif(theonectld.eq.3)then
C            VAL1(5,IT)=avqcrioc
C            VAL1(6,IT)=avqcrili
C            VAL1(7,IT)=0.0
C            VAL1(8,IT)=avqcrisp
C            VAL1(9,IT)=0.0
C            VAL1(10,IT)=0.0
C          endif
C        endif
  198 CONTINUE

C Get zone air temperature, store in VAL1(22,N).
      ISET=ISIM
      CALL GZAIRT(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,22)

C Get inside surface temperature.
      CALL GTMS(IDAY,IZONE,ISFN,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,23)

C Get inside casual radiant amount for this surface and store
C uncontrolled types 1,2,3 and controlled in VAL1(5,I) to VAL1(8,I).
C As there is only one controlled lump all to slot VAL1(8,I).
C      if(izver.lt.4)then
        CALL ZCASR(IZONE,ISFN,IDAY)
        DO 28 I=1,N
          VAL1(5,I)=VAL3(15,I)
          VAL1(6,I)=VAL3(16,I)
          VAL1(7,I)=VAL3(17,I)
          VAL1(8,I)=VAL3(18,I)+VAL3(19,I)+VAL3(20,I)
          VAL1(9,I)=0.0
          VAL1(10,I)=0.0
   28   CONTINUE
C      endif

C Get the plant injection at each time step, save zone amount in
C VAL1(21,N) for use in sorting by plant heating/cooling/off and
C for deriving radiant portion of plant.
      CALL GZQM(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,21)

C Determine the radiant portion of the plant via checking IBAN.
C Radiant only to one surface node, mixed plant injection, or
C no radiant plant component.
      IF(ICF.GT.0)THEN
        IF(IBAN(ICF,1).EQ.IZONE.AND.IBAN(ICF,2).EQ.ISFN.AND.
     &                                       IBAN(ICF,3).EQ.0)then
          DO 35 I=ISTART,IEND
            VAL1(12,I)=VAL1(21,I)
   35     CONTINUE
        ELSEIF(IBAN(ICF,1).EQ.-2.AND.IBAN(ICF,2).EQ.IZONE)then
          DO 36 I=ISTART,IEND
            VAL1(12,I)=(sna(izone,ISFN)*(EMISI(ISFN)/emarea))*
     &                  VAL1(21,I)*(1.-(FLOAT(IBAN(ICF,3))*0.01))
   36     CONTINUE
        ELSE
          DO 37 I=ISTART,IEND
            VAL1(12,I)=0.
   37     CONTINUE
        ENDIF
      ELSE
        DO 137 I=ISTART,IEND
          VAL1(12,I)=0.
  137   CONTINUE
      ENDIF

C Assume other side plant to be zero at this stage of program development.
      DO 38 I=ISTART,IEND
        VAL1(20,I)=0.
   38 CONTINUE

      RETURN
      END

