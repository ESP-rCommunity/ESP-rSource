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

C  MOZCAS  Selection of zone causal energy display.
C  MOZSUM  Collect/sum zone causal data.
C  MOEGET  Get air node data from results library.
C  CSCONV  Return timestep convection flux at interior surface.

C ******************** MOZCAS ********************
C MOZCAS process breakdown data for zones.
C CALLS: MOZSUM   CALLED BY: MOSTAT

      SUBROUTINE MOZCAS(IZONE,IANZSE,NIT)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL

      COMMON/AVRAGE/IDAVER
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      integer icascf
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/SNAP/SNAPSH
      logical SNAPSH
      
      character outs*124,xfile*144,tg*1,delim*1

      character DESCRD*5,DDX*7,DDX1*10,DHX*5,sp*1,outsd*124
      CHARACTER*31 LABELS(17)
      CHARACTER*15 TITLE1(17)

C Read in the geometry file to refresh surface attributes.
      iunit=ifil+1
      call georead(iunit,LGEOM(IZONE),IZONE,0,iuout,ier)
      idwins=0

C Generate the labels for the interface and report.

C << revise for additional casual gain types >>
      LABELS(1) =' Infiltration air load         '
      LABELS(2) =' Ventilation air load          '
      LABELS(3) =' N/A                           '
      LABELS(4) =' N/A                           '
      LABELS(5) =' N/A                           '
      write(LABELS(6),'(2a)') ' Casual ',lodlabel(izone,1)
      write(LABELS(7),'(2a)') ' Casual ',lodlabel(izone,2)
      write(LABELS(8),'(2a)') ' Casual ',lodlabel(izone,3)
      LABELS(9) =' Controlled casual gain        '
      LABELS(10)=' Thermal bridge (linear)       '
      LABELS(11)=' Heat storage @ air point      '
      LABELS(12)=' Convection @ opaque surf: ext '
      LABELS(13)=' Convection @ opaque surf: ptn '
      LABELS(14)=' Convection @ transp surf: ext '
      LABELS(15)=' Convection @ transp surf: ptn '
      LABELS(16)=' Convection portion of plant   '
      LABELS(17)=' Totals                        '
      TITLE1(1)='|Infiltr '
      TITLE1(2)='|Ventilat'
      TITLE1(3)='|W cnd ex'
      TITLE1(4)='|W cnd in'
      TITLE1(5)='|AirP sol'
      write(TITLE1(6),'(a,a)')'|UC',lodlabel(izone,1)
      write(TITLE1(7),'(a,a)')'|UC',lodlabel(izone,2)
      write(TITLE1(8),'(a,a)')'|UC',lodlabel(izone,3)
      TITLE1(9)='|Ctl cas '
      TITLE1(10)='|Th brdg'
      TITLE1(11)='|Storage'
      TITLE1(12)='|O MLC ex'
      TITLE1(13)='|O MLC in'
      TITLE1(14)='|T MLC ex'
      TITLE1(15)='|T MLC in'
      TITLE1(16)='|Plant   '
      TITLE1(17)='|........'
      
      if(delim.eq.'T')then
        sp=CHAR(9)
      elseif(delim.eq.'S')then
        sp=' '
      elseif(delim.eq.'C')then
        sp=','
      endif

C Reset display unit if redirecting output.
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
      endif

C Output general header information for load or energy use.
C If snapshot IOH1 is timesteps rather than integer hours.
      if(SNAPSH)then
        call edisp(itru,' ')
        write(outs,42)IZONE,zname(IZONE)
   42   FORMAT(' Causal load breakdown (kW) at air point for zone',I2,
     &         ': ',A12)
        call edisp(itru,outs)
        CALL STDATE(IYEAR,IODS,DDX,DDX1)
        CALL ESTIME(NTS,IDAVER,IOH1,DHX,DESCRD,TIMER)
        write(outs,'(A,A10,A,A5)') ' Time: ',DDX1,'@',DHX
        call edisp(itru,outs)
      else
        call edisp(itru,' ')
        write(outs,43)IZONE,zname(IZONE)
   43   FORMAT(' Causal energy breakdown (kWhrs) at air point for zone',
     &         I2,': ',A12)
        call edisp(itru,outs)
      endif
      if(ISAVE.lt.4.and.izver.lt.2)then
        call edisp(itru,' ')
        call edisp(itru,'Convection is not included in report.')
      endif

C Print breakdown headings: plant status or
C gains/loss or user defined.
      IF(IANZSE.EQ.1)THEN
        call edisp(itru,' ')
        write(outs,'(16X,A)')
     &  ' Htg. period           Clg. period           OFF period'
        call edisp(itru,outs)
        write(outs,'(16X,A)')
     &  ' Gain      Loss        Gain      Loss        Gain      Loss'
        call edisp(itru,outs)
      ELSEIF(IANZSE.EQ.2)THEN
        call edisp(itru,' ')
        write(outs,'(35X,A)')'   Gain         Loss'
        call edisp(itru,outs)
      ENDIF

C Determine the plant type for this zone.
      NIT=0
      ICF=ICASCF(IZONE)
      IF(ICF.EQ.0.OR.NCF.EQ.0)goto 11
      NIT=-1
      IF(IBAN(ICF,1).EQ.IZONE.AND.IBAN(ICF,2).EQ.0)NIT=1
      IF(IBAN(ICF,1).EQ.0)NIT=1
      IF(IBAN(ICF,1).EQ.-2.AND.IBAN(ICF,2).EQ.IZONE)NIT=2  ! mixed Rad & Conv.
   11 CONTINUE

C Perform ZONE sums and averages.
      CALL MOZSUM(IZONE,IANZSE)

C Print out the results.
      DO 150 I=1,16
        IF(I.LT.16)goto 82

C Decode the current environmental control actuation point into
C appropraite text. 
C << recode without the GOTO >>
        II=NIT+2
        GOTO(812,811,82,814),II

C No plant exchange.
  811   call edisp(itru,' No plant input/extract')
        goto 150

C Plant interaction not an air point.
  812   call edisp(itru,' Plant exchange not with air point')
        DO 817 K=1,6
          VAL2(I,K)=0.
  817   CONTINUE
        goto 150

C Plant interacts with air and surface nodes, calculate
C the convective portion.
  814   DO 818 K=1,6
          VAL2(I,K)=VAL2(I,K)*FLOAT(IBAN(ICF,3))*0.01
  818   CONTINUE

C No windows, don't bother with rows 3,4,5. If no casual gain control
C don`t bother with row 9.
   82   if(i.eq.3.or.i.eq.4.or.i.eq.5)then
          continue
        elseif(ICGC(izone).eq.0.and.(i.eq.9))then
          continue
        else
          IF(IANZSE.EQ.1)THEN
            if(delim.eq.'-')then
              write(outs,'(A,6F11.2)')TITLE1(I),(VAL2(I,J),J=1,6)
              call edisp(itru,outs)
            else
              write(outs,'(6F11.2)')(VAL2(I,J),J=1,6)
              call SDELIM(outs,outsd,delim,IW)
              write(outs,'(3a)')TITLE1(I)(1:lnblnk(TITLE1(I))),sp,
     &          outsd(1:lnblnk(outsd))
              call edisp(itru,outs)
            endif
          ELSEIF(IANZSE.EQ.2)THEN
            if(delim.eq.'-')then
              write(outs,'(A,2F12.3)')LABELS(I),(VAL2(I,J),J=1,2)
              call edisp(itru,outs)
            else
              write(outs,'(2F12.3)')(VAL2(I,J),J=1,2)
              call SDELIM(outs,outsd,delim,IW)
              write(outs,'(3a)')LABELS(I)(1:lnblnk(LABELS(I))),sp,
     &          outsd(1:lnblnk(outsd))
              call edisp(itru,outs)
            endif
          endif
        ENDIF
  150 CONTINUE

C Totals: use VAL2(I,8) where I=1 to 6 depending on IANZSE.
      DO 160 I=1,7
        VAL2(I,8)=0.
  160 CONTINUE
      J=6
      IF(IANZSE.EQ.2)J=2
      DO 170 I=1,J
        DO 180 K=1,16
          VAL2(I,8)=VAL2(I,8)+VAL2(K,I)
  180   CONTINUE
  170 CONTINUE

      IF(IANZSE.EQ.1)then
        if(delim.eq.'-')then
          write(outs,'(A,6F11.1)')LABELS(17),(VAL2(I,8),I=1,6)
          call edisp(itru,outs)
        else
          write(outs,'(6F11.1)')(VAL2(I,8),I=1,6)
          call SDELIM(outs,outsd,delim,IW)
          write(outs,'(3a)')LABELS(17)(1:lnblnk(LABELS(17))),sp,
     &      outsd(1:lnblnk(outsd))
          call edisp(itru,outs)
        endif
      elseif(IANZSE.EQ.2)then
        if(delim.eq.'-')then
          write(outs,'(A,2F12.3)')LABELS(17),(VAL2(I,8),I=1,2)
          call edisp(itru,outs)
        else
          write(outs,'(2F12.3)')(VAL2(I,8),I=1,2)
          call SDELIM(outs,outsd,delim,IW)
          write(outs,'(3a)')LABELS(17)(1:lnblnk(LABELS(17))),sp,
     &      outsd(1:lnblnk(outsd))
          call edisp(itru,outs)
        endif
      ENDIF
      RETURN
      END

C ******************** MOZSUM ********************

C Provide sums and averages for zone energy breakdowns.
C CALLS: MOEGET    CALLED BY: MOZCAS

      SUBROUTINE MOZSUM(IZONE,IANZSE)
#include "building.h"
#include "model.h"
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/recver/izver,ipver,iever
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      character outs*124

      DIMENSION XVAL1(MZS)

      TCMULT=NOUT/(NTS*1000.)

C Initialise summation locations: VAL2(I,J) is used to store summations
C where I=1 to 16 (one for each flowpath) and J=1 to 6 (representing
C columns) depending on selected summation mode.
      DO 20 I=1,16
        DO 201 J=1,7
          VAL2(I,J)=0.
  201   CONTINUE
   20 CONTINUE

C Day-by-day recovery.
      DO 30 I=IODS,IODF
        ID=I

C For requested zone and every time-step, get results for
C complete day (held in VAL1) and step at defined output time interval.
        CALL MOEGET(IZONE,ID)
        CALL CHKTIME(ID,ISTART,IEND)
        DO 40 J=ISTART,IEND,NOUT
          JT=J

C Compute average of VAL1() based on IAV.
          CALL VALIAV(16,JT,XVAL1)

C Sum depending on user-defined output mode.

C Breakdown by plant status: summations held in VAL2(I,J) where
C J=1 to 6 corresponds to gains and losses for heating,cooling,off.
          IF(IANZSE.EQ.1)THEN
            IF(XVAL1(16).LT.0.)THEN
              DO 110 K=1,15
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,4)=VAL2(K,4)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,3)=VAL2(K,3)+XVAL1(K)*TCMULT
                ENDIF
  110         CONTINUE
              VAL2(16,4)=VAL2(16,4)+XVAL1(16)*TCMULT
            ELSEIF(XVAL1(16).GT.0.)THEN
              DO 120 K=1,15
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
                ENDIF
  120         CONTINUE
              VAL2(16,1)=VAL2(16,1)+XVAL1(16)*TCMULT
            ELSE
              DO 100 K=1,15
                IF(XVAL1(K).LE.0.)THEN
                  VAL2(K,6)=VAL2(K,6)+XVAL1(K)*TCMULT
                ELSE
                  VAL2(K,5)=VAL2(K,5)+XVAL1(K)*TCMULT
                ENDIF
  100         CONTINUE
            ENDIF
          ENDIF

C Breakdown in terms of net gain and loss: summations
C held in VAL2(I,J) where I=1 to 16 is as above and
C J=1 to 2 relates to GAINS and LOSSES respectively.
          IF(IANZSE.EQ.2)THEN
            DO 130 K=1,16
              IF(XVAL1(K).LE.0.)THEN
                VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
              ELSE
                VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
              ENDIF
  130       CONTINUE
          ENDIF
   40   CONTINUE
   30 CONTINUE

      RETURN
      END


C ******************** MOEGET ********************

C MOEGET recovers from the results library flux related to the air
C node for any requested zone and day.
C CALLS:  GQV1,GQV2,GQW1,GQW2,GQSA,GZQM,SURADJ,CSCONV
C         GCFRAC,CASTYP
C CALLED BY: MOZSUM

      SUBROUTINE MOEGET(IZONE,IDAY)
#include "building.h"
#include "geometry.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C Versions of libraries (zone, plant, electrical).
      common/recver/izver,ipver,iever

      CHARACTER*25 CSTR

      DIMENSION CQ(MTS)

C Set chosen result set.
      ISET=ISIM

C Get single days results for default windows etc. into GVAL.
      ISTART=1
      IEND=24*NTS
      NN=NZSUR(IZONE)

C Clear storage common.
      DO 20 I=1,16
       DO 201 J=1,IEND
        VAL1(I,J)=0.
  201  CONTINUE
   20 CONTINUE

C Infiltration into VAL1(1,N).
      CALL GQV1(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,1)

C Ventilation into VAL1(2,N).
      CALL GQV2(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,2)

C VAL1(3,N) slot un-used.
C VAL1(4,N) slot un-used.
C VAL1(5,N) slot un-used.

C Convection portion of plant into VAL1(16,N).
      CALL GZQM(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,16)

C Surface convection. If version 2 of library then can recover directly
C from records.
      if(izver.ge.2)then

C Put exterior opaque into slot 12, inside opaque into slot 13.
        CALL GOUTOPQ(IDAY,IZONE,ISET)
        CALL XTVAL1(GVAL,ISTART,IEND,12)
        CALL GOPQIN(IDAY,IZONE,ISET)
        CALL XTVAL1(GVAL,ISTART,IEND,13)

C Put exterior transparent into slot 14 and inside trn into slot 15.
        CALL GOUTTRN(IDAY,IZONE,ISET)
        CALL XTVAL1(GVAL,ISTART,IEND,14)
        CALL GTRNIN(IDAY,IZONE,ISET)
        CALL XTVAL1(GVAL,ISTART,IEND,15)
      else

C Opaque external convection contribution, loop through each surface,
C find if interior or exterior and sum. Note convection is calculated
C in terms of surface so negate it to have contribution to air node.
C Note: if ISAVE = 2 or 3 then data is not available, otherwise if ISAVE = 4 then
C use the exact values from the results library.
        DO 60 ISN= 1,NN
          CALL SURADJ(IZONE,ISN,IE,T,IZC,ISC,ICN,CSTR)
          CALL CSCONV(IDAY,IZONE,ISN,CQ)
          DO 61 ISTP=ISTART,IEND
            IF(IE.GE.1)THEN
              IF(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &           SSOTF(ICN)(1:4).EQ.'UNKN')THEN
                VAL1(13,ISTP)=VAL1(13,ISTP)+CQ(ISTP)*(-1.0)
              ELSE
                VAL1(15,ISTP)=VAL1(15,ISTP)+CQ(ISTP)*(-1.0)
              ENDIF
            ELSEIF(IE.EQ.0)THEN
              IF(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &           SSOTF(ICN)(1:4).EQ.'UNKN')THEN
                VAL1(12,ISTP)=VAL1(12,ISTP)+CQ(ISTP)*(-1.0)
              ELSE
                VAL1(14,ISTP)=VAL1(14,ISTP)+CQ(ISTP)*(-1.0)
              ENDIF
            ENDIF
   61     CONTINUE
   60   CONTINUE
      endif

C Get casual gains.
C For each time step move the timestep values from VAL3() into the
C required VAL1() slots for this report. There is only one type of
C casual gain controled at any point in time so dump those into
C the slot VAL1(9 ,I).
      CALL ZCASC(IZONE,IDAY)
      DO 50, I=1,IEND
        VAL1(6 ,I)=VAL3(15,I)
        VAL1(7 ,I)=VAL3(16,I)
        VAL1(8 ,I)=VAL3(17,I)
        VAL1(9 ,I)=VAL3(18,I)+VAL3(19,I)+VAL3(20,I)
   50 CONTINUE

C Get thermal bridge portion into VAL1(10,I)
      CALL GTBR(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,10)

C Get storage at air point into VAL1(11,I)
      CALL GAIRSTOR(IDAY,IZONE,ISET)
      CALL XTVAL1(GVAL,ISTART,IEND,11)

      RETURN
      END

C ******************** CSCONV ********************

C CSCONV is a mid-level routine to return the convection
C at an inside surface for each timestep in a day.
      SUBROUTINE CSCONV(IDAY,IZONE,ISFN,Q)
#include "building.h"
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      DIMENSION Q(MTS)

      N=24*NTS

C If save option 2 or 3 then it is not possible to recover
C hc at inside face of surface.
      IF(ISAVE.GT.1.AND.ISAVE.LT.4)THEN

C Convection at MLC, first get air temperature and place in XDUM.
        call usrmsg('Current save level does not support the recovery',
     &    'of convection at surfaces. Use save level 4.','-')
        DO 127 I=1,N
          Q(I)=0.0
  127   CONTINUE
      ELSEIF(ISAVE.EQ.4)THEN

C Get each timestep surface convection from the results library.
          IGET=4
          CALL G4FLUX(IDAY,IZONE,ISFN,IGET,Q)
      ENDIF

      RETURN
      END
