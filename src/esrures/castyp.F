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

C This file castype.f contains the following routines:

C CASTYP: determines controlled convective casual gains at one timestep.
C ZCASC:  returns one day's timestep values of all convective casual gains via VAL3.
C ZCASR:  returns one day's timestep values of all (surface) rad casual gains via VAL3.
C CASRT:  returns one day's timstep values of all zone rad casual gains 
C ZCASCF: returns one day's unaveraged values of convective casual gains via VAL3. 

C ******************** ZCASC ********************

C ZCASC reconstructs one day's values of each of 
C the uncontrolled and controlled convective casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C For izver <4 the method used shifts information from the operations
C file into the equivalent of bps timestep averaging. For izver >=4 the
C data is read directly from the data fields.
C The parameter IDAY is 
C VAL3(15,?) holds uncontrolled convective casual type 1.
C VAL3(16,?) holds uncontrolled convective casual type 2.
C VAL3(17,?) holds uncontrolled convective casual type 3.
C VAL3(18,?) holds controlled convective casual type 1.
C VAL3(19,?) holds controlled convective casual type 2.
C VAL3(20,?) holds controlled convective casual type 3.

      SUBROUTINE ZCASC(IZONE,IDAY)
#include "building.h"
#include "geometry.h"
#include "tdf2.h"

C      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/AVRAGE/IDAVER

C Hold this for each zone.
      COMMON/CASPD/DUCCT1(MCOM),DUCCT2(MCOM),DUCCT3(MCOM),
     &             DUCRT1(MCOM),DUCRT2(MCOM),DUCRT3(MCOM)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      REAL X1a_QTOTOCCUP,X2a_QTOTOCCUP
      REAL X1c_QTOTEQUIP,X2c_QTOTEQUIP
C.........END: USED FOR ACTIVITY TDF DATA.....................

      DIMENSION VAL(MBITS+2)
C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      IEND=24*NTS

C Initialise 'present values'.
      PUCCT1=0.0
      PUCCT2=0.0
      PUCCT3=0.0
      ICCP=0

C If izver >= 4 the read the data directly.
      if(izver.eq.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perocupc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightc
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipc
            endif
  142     continue
          return
        endif
      endif

C Check to see if there is a temporal (single) casual gain 
C entry for this zone (the values are explicitly saved) and
C place this in the equipment casual gain.
      found = .false.
      if(ICASUAL(izone).ne.0)found = .true.
      IF(found)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        DO 151, I=1,IEND
          VAL3(15,I)=0.0
          VAL3(16,I)=0.0
          VAL3(17,I)=VAL3(21,I)
          VAL3(18,I)=0.0
          VAL3(19,I)=0.0
          VAL3(20,I)=0.0
 151    CONTINUE
        RETURN
      ENDIF

C Check to see if there is a temporal casual gain (separate)
C entry for this zone (this requires recovering from temporal file.
      found = .false.
      if(ICASUAL3(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 10 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 20 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get convective columns of the temporal item.
            itdi=ICASUAL3(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a=VAL(ISD)
            X1b=VAL(ISD+3)
            X1c=VAL(ISD+6)
            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a=VAL(ISD)
            X2b=VAL(ISD+3)
            X2c=VAL(ISD+6)
            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
            call DATIME(IJ,TIME)
C          WRITE(OUTS,'(A,I3,5F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Va ',
C     &      IJ,T,TP,TF,TFL,TIME,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
   20     CONTINUE
   10   CONTINUE
        RETURN
      ENDIF
      
C Check to see if there is a temporal casual gain (activity)
C entry for this zone (this requires recovering from temporal file.
      found = .false.
      if(IACTIV(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 110 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 120 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C .............Get convective columns of the temporal item.
            itdi=IACTIV(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X1a=(X1a_QTOTOCCUP-((X1a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X1b=0
            X1c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X1c=(X1c_QTOTEQUIP-((X1c_QTOTEQUIP*ITMAR(1,5))/100))/2

C Same pattern as the other temporal file items (for
C casual gains): Dealing with future timestep.
            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X2a=(X2a_QTOTOCCUP-((X2a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X2b=0
            X2c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X2c=(X2c_QTOTEQUIP-((X2c_QTOTEQUIP*ITMAR(1,5))/100))/2
            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
            call DATIME(IJ,TIME)
C          WRITE(OUTS,'(A,I3,5F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Va ',
C     &      IJ,T,TP,TF,TFL,TIME,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
  120     CONTINUE
  110   CONTINUE
        RETURN
      ENDIF

C Note that the casual gain at the first timestep of the day is dependent
C on the casual gain at the last timestep of the previous day.    
C If IDAY is equal to the first day of the simulation then there is no
C previous day so assume DUCCT?= current day.
      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCCT1(izone)=UCCT1
        DUCCT2(izone)=UCCT2
        DUCCT3(izone)=UCCT3
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN

C Need to get the last timestep of the previous day to have the last 
C timestep for interpolation before recovering the convective casual 
C gain for the day in question.
        CALL GCASC(IDAY-1,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY-1,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCCT1(izone)=UCCT1
        DUCCT2(izone)=UCCT2
        DUCCT3(izone)=UCCT3
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ELSEIF(IDAY.GT.IODS)THEN

C Subsequent recovery day - information on previous day has already
C been saved so proceed to get convective casual gain for IDAY.
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ENDIF


C For each time step determine each of the uncontrolled casual gains.
      DO 50, I=1,IEND
        CALL CASTYP(IZONE,IDAY,I,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)

C Save average fluxs in VAL1(8 to 13,N). If first timestep, then average with
C that of the last of the previous day.
        IF(I.EQ.1)THEN
          VAL3(15,I)=(UCCT1+DUCCT1(izone))*0.5
          VAL3(16,I)=(UCCT2+DUCCT2(izone))*0.5
          VAL3(17,I)=(UCCT3+DUCCT3(izone))*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ELSE
          VAL3(15,I)=(UCCT1+PUCCT1)*0.5
          VAL3(16,I)=(UCCT2+PUCCT2)*0.5
          VAL3(17,I)=(UCCT3+PUCCT3)*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ENDIF

C Controlled portion is total casual gains less the uncontrolled.
        IF(ICC.EQ.1.OR.ICCP.EQ.1)THEN
          VAL3(18,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.2.OR.ICCP.EQ.2)THEN
          VAL3(19,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.3.OR.ICCP.EQ.3)THEN
          VAL3(20,I)=VAL3(21,I)-TUCC
        ELSE
          VAL3(18,I)=0.
          VAL3(19,I)=0.
          VAL3(20,I)=0.
        ENDIF

C Shift future to present.  Save latest control type so that next timestep
C will be checked to match bps.
        PUCCT1=UCCT1
        PUCCT2=UCCT2
        PUCCT3=UCCT3
        ICCP=ICC
   50 CONTINUE

C Save the last timestep for use the next day.
        DUCCT1(izone)=UCCT1
        DUCCT2(izone)=UCCT2
        DUCCT3(izone)=UCCT3
      RETURN
      END

C ******************** ZCASR ********************

C ZCASR reconstructs one day's values of each of the 
C uncontrolled and controlled radiant casual gains for a 
C surface in a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for 
C temporary storage. The method used shifts information from the
C operations file into the equivalent of bps timestep averaging.
C To account for area and emissivity weighting compute `emarea`
C based on data previously read in from the construction file.

C VAL3(15,?) holds uncontrolled radiant casual type 1.
C VAL3(16,?) holds uncontrolled radiant casual type 2.
C VAL3(17,?) holds uncontrolled radiant casual type 3.
C VAL3(18,?) holds controlled radiant casual type 1.
C VAL3(19,?) holds controlled radiant casual type 2.
C VAL3(20,?) holds controlled radiant casual type 3.

      SUBROUTINE ZCASR(IZONE,ISFN,IDAY)
#include "building.h"
#include "geometry.h"
#include "tdf2.h"

C      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV5/sna(MCOM,MS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/AVRAGE/IDAVER
      COMMON/CASPD/DUCCT1(MCOM),DUCCT2(MCOM),DUCCT3(MCOM),
     &             DUCRT1(MCOM),DUCRT2(MCOM),DUCRT3(MCOM)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      REAL X1a_QTOTOCCUP,X2a_QTOTOCCUP
      REAL X1c_QTOTEQUIP,X2c_QTOTEQUIP

C.........END: USED FOR ACTIVITY TDF DATA.....................

      DIMENSION VAL(MBITS+2)
C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

      IEND=24*NTS

C Set chosen result set.
      ISET=ISIM

C Radiant casual gains are area AND emissivity weighted.
      emarea=0.0
      do 80 kk=1,NZSUR(izone)
        emarea=emarea+sna(izone,kk)*EMISI(kk)
  80  continue

C If izver >= 4 the read the data directly and multiply the radiant
C value by SCALE. Similar pattern of code used elsewhere for izver >=4.
      if(izver.eq.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)
            VAL3(15,I)=perocupr*SCALE
            VAL3(16,I)=perlightr*SCALE
            VAL3(17,I)=perequipr*SCALE
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=perocupr*SCALE
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightr*SCALE
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipr*SCALE
            endif
  142     continue
          return
        endif
      endif

C Initialise 'present values'.
      PUCRT1=0.0
      PUCRT2=0.0
      PUCRT3=0.0
      ICCP=0

C Check to see if there is a temporal single casual gain 
C entry for this zone (the values are explicitly saved).
C If temporal CASUALT set type 1 & 2 and controlled radiant gains to 0.0.
      found = .false.
      if(ICASUAL(izone).ne.0)found = .true.
      IF(found)THEN
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea
        DO 151, I=1,IEND
          VAL3(15,I)=0.0
          VAL3(16,I)=0.0
          VAL3(17,I)=VAL3(21,I)*SCALE
          VAL3(18,I)=0.0
          VAL3(19,I)=0.0
          VAL3(20,I)=0.0
 151    CONTINUE
        RETURN
      ENDIF

C If temporal CASUAL3T then each type gets filled.
      found = .false.
      if(ICASUAL3(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea
        IJ=0
        DO 10 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 20 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get radiant columns of the temporal item.
            itdi=ICASUAL3(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a=VAL(ISD+1)*SCALE
            X1b=VAL(ISD+4)*SCALE
            X1c=VAL(ISD+7)*SCALE
            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a=VAL(ISD+1)*SCALE
            X2b=VAL(ISD+4)*SCALE
            X2c=VAL(ISD+7)*SCALE
            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
            call DATIME(IJ,TIME)
C          WRITE(OUTS,'(A,I3,5F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vb ',
C     &      IJ,T,TP,TF,TIME,TFL,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
   20     CONTINUE
   10   CONTINUE

        RETURN
      ENDIF

C If temporal ACTIVITY then each type gets filled.
      found = .false.
      if(IACTIV(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea
        IJ=0
        DO 110 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 120 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get radiant columns of the temporal item.
C Important: At the moment a 50% radiant/convective split is
C allowed for this type of temporal item. So the radiant
C gains will be equal with the convective.
            itdi=IACTIV(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X1a=
     &      ((X1a_QTOTOCCUP-((X1a_QTOTOCCUP*ITMAR(1,3))/100))/2)*SCALE

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X1b=0*SCALE
            X1c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X1c=
     &      ((X1c_QTOTEQUIP-((X1c_QTOTEQUIP*ITMAR(1,5))/100))/2)*SCALE

            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X2a=
     &      ((X2a_QTOTOCCUP-((X2a_QTOTOCCUP*ITMAR(1,3))/100))/2)*SCALE

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X2b=0*SCALE
            X2c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X2c=
     &      ((X2c_QTOTEQUIP-((X2c_QTOTEQUIP*ITMAR(1,5))/100))/2)*SCALE

            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
            call DATIME(IJ,TIME)
C          WRITE(OUTS,'(A,I3,5F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vb ',
C     &      IJ,T,TP,TF,TIME,TFL,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
  120     CONTINUE
  110   CONTINUE
  
        RETURN
      ENDIF

C Note that the casual gain at the first timestep of the day is dependent
C on the casual gain at the last timestep of the previous day.    
C If IDAY is equal to the first day of the simulation then there is no
C previous day so assume DUCRT?= current day.
      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN

C Need to get the last timestep of the previous day to have the last 
C timestep for interpolation before recovering the radiant casual 
C gain for the day in question.
        CALL GCASR(IDAY-1,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY-1,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ELSEIF(IDAY.GT.IODS)THEN

C Subsequent recovery day - information on the previous day has already
C been saved so proceed to get convective casual gain for IDAY.
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ENDIF

C For each time step determine each of the uncontrolled casual gains.
      DO 50, I=1,IEND
        CALL CASTYP(IZONE,IDAY,I,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea

C Save average fluxs in VAL3(15 to 20,N). If first timestep, then average with
C that of the last of the previous day.
        IF(I.EQ.1)THEN
          VAL3(15,I)=((UCCRT1*SCALE)+(DUCRT1(izone)*SCALE))*0.5
          VAL3(16,I)=((UCCRT2*SCALE)+(DUCRT2(izone)*SCALE))*0.5
          VAL3(17,I)=((UCCRT3*SCALE)+(DUCRT3(izone)*SCALE))*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ELSE
          VAL3(15,I)=((UCCRT1*SCALE)+(PUCRT1*SCALE))*0.5
          VAL3(16,I)=((UCCRT2*SCALE)+(PUCRT2*SCALE))*0.5
          VAL3(17,I)=((UCCRT3*SCALE)+(PUCRT3*SCALE))*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ENDIF

C Controlled portion is total casual gains less the uncontrolled.
        IF(ICC.EQ.1.OR.ICCP.EQ.1)THEN
          VAL3(18,I)=(VAL3(21,I)*SCALE)-TUCC
        ELSEIF(ICC.EQ.2.OR.ICCP.EQ.2)THEN
          VAL3(19,I)=(VAL3(21,I)*SCALE)-TUCC
        ELSEIF(ICC.EQ.3.OR.ICCP.EQ.3)THEN
          VAL3(20,I)=(VAL3(21,I)*SCALE)-TUCC
        ELSE
          VAL3(18,I)=0.
          VAL3(19,I)=0.
          VAL3(20,I)=0.
        ENDIF
C Shift future to present.  Save latest control type so that the
C next timestep will be checked to match bps.
        PUCRT1=UCCRT1
        PUCRT2=UCCRT2
        PUCRT3=UCCRT3
        ICCP=ICC
   50 CONTINUE

C Save the last timestep for use the next day.
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3

      RETURN
      END


C ******************** CASRT ********************

C CASRT reconstructs one day's values of each type of 
C uncontrolled and controlled radiant casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for 
C temporary storage. The method used shifts information from the
C operations file into the equivalent of bps timestep averaging.

C VAL3(15,?) holds uncontrolled casual type 1.
C VAL3(16,?) holds uncontrolled casual type 2.
C VAL3(17,?) holds uncontrolled casual type 3.
C VAL3(18,?) holds controlled casual type 1.
C VAL3(19,?) holds controlled casual type 2.
C VAL3(20,?) holds controlled casual type 3.

      SUBROUTINE CASRT(IZONE,IDAY)
#include "building.h"
#include "geometry.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/AVRAGE/IDAVER
      COMMON/CASPD/DUCCT1(MCOM),DUCCT2(MCOM),DUCCT3(MCOM),
     &             DUCRT1(MCOM),DUCRT2(MCOM),DUCRT3(MCOM)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      REAL X1a_QTOTOCCUP,X2a_QTOTOCCUP
      REAL X1c_QTOTEQUIP,X2c_QTOTEQUIP
C.........END: USED FOR ACTIVITY TDF DATA.....................

      DIMENSION VAL(MBITS+2)
C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

      IEND=24*NTS

C Set chosen result set.
      ISET=ISIM

C If izver >= 4 the read the data directly.
      if(izver.eq.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZ,ISET,JTS,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupr
            VAL3(16,I)=perlightr
            VAL3(17,I)=perequipr
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZ,ISET,JTS,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=perocupr
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightr
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipr
            endif
  142     continue
          return
        endif
      endif

C Initialise 'present values'.
      PUCRT1=0.0
      PUCRT2=0.0
      PUCRT3=0.0
      ICCP=0

C Check to see if there is a temporal casual gain entry
C for this zone (the values are explicitly saved).
C If temporal CASUALT set type 2 & 3 and controlled radiant gains to 0.0.
      found = .false.
      if(ICASUAL(izone).ne.0)found = .true.
      IF(found)THEN
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        DO 151, I=1,IEND
          VAL3(15,I)=VAL3(21,I)
          VAL3(16,I)=0.0
          VAL3(17,I)=0.0
          VAL3(18,I)=0.0
          VAL3(19,I)=0.0
          VAL3(20,I)=0.0
 151    CONTINUE
        RETURN
      ENDIF

C If temporal CASUAL3T then each type gets filled.
      found = .false.
      if(ICASUAL3(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 10 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 20 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get radiant columns of the temporal item.
            itdi=ICASUAL3(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a=VAL(ISD+1)
            X1b=VAL(ISD+4)
            X1c=VAL(ISD+7)
            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a=VAL(ISD+1)
            X2b=VAL(ISD+4)
            X2c=VAL(ISD+7)
            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...TIME=T TIMEF=TF TIMP=TP in case reqd for single timestep.
C            call DATIME(IJ,TIME)
C            TIMEF=real(IJ)/real(NTS)
C            TIMEP=TIMEF - (1./real(NTS))
C          WRITE(OUTS,'(A,I3,6F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vc ',
C     &      IJ,T,TP,TF,TIME,TIMEF,TIMEP,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
   20     CONTINUE
   10   CONTINUE
        RETURN
      ENDIF
      
C If temporal ACTIVITY then each type gets filled.
      found = .false.
      if(IACTIV(izone).ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 110 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 120 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IDAY.EQ.1)IDP=365
C            IF(IDAY.GT.1)IDP=IDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get radiant columns of the temporal item.
C Important: At the moment a 50% radiant/convective split is
C allowed for this type of temporal item. So the radiant
C gains will be equal with the convective.
            itdi=IACTIV(izone)
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X1a=(X1a_QTOTOCCUP-((X1a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X1b=0
            X1c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X1c=(X1c_QTOTEQUIP-((X1c_QTOTEQUIP*ITMAR(1,5))/100))/2

            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X2a=(X2a_QTOTOCCUP-((X2a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X2b=0
            X2c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X2c=(X2c_QTOTEQUIP-((X2c_QTOTEQUIP*ITMAR(1,5))/100))/2

            VAL3(15,IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))
            VAL3(16,IJ)=X1b+(((T-TP)*(X2b-X1b))/(TF-TP))
            VAL3(17,IJ)=X1c+(((T-TP)*(X2c-X1c))/(TF-TP))
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0
            
C Debug...TIME=T TIMEF=TF TIMP=TP in case reqd for single timestep.
C            call DATIME(IJ,TIME)
C            TIMEF=real(IJ)/real(NTS)
C            TIMEP=TIMEF - (1./real(NTS))
C          WRITE(OUTS,'(A,I3,6F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vc ',
C     &      IJ,T,TP,TF,TIME,TIMEF,TIMEP,IDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
  120     CONTINUE
  110   CONTINUE
        RETURN
      ENDIF

C Note that the casual gain at the first timestep of the day is dependent
C on the casual gain at the last timestep of the previous day.    
C If IDAY is equal to the first day of the simulation then there is no
C previous day so assume DUCRT?= current day.
      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN

C Need to get the last timestep of the previous day to have the last 
C timestep for interpolation before recovering the radiant casual 
C gain for the day in question.
        CALL GCASR(IDAY-1,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        CALL CASTYP(IZONE,IDAY-1,IEND,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ELSEIF(IDAY.GT.IODS)THEN

C Subsequent recovery day - information on the previous day has already
C been saved so proceed to get radiant casual gain for IDAY.
        CALL GCASR(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ENDIF

C For each time step determine each of the uncontrolled casual gains.
      DO 50, I=1,IEND
        CALL CASTYP(IZONE,IDAY,I,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)

C Save average fluxs in VAL3(15 to 20,N). If first timestep, then average with
C that of the last of the previous day.
        IF(I.EQ.1)THEN
          VAL3(15,I)=(UCCRT1+DUCRT1(izone))*0.5
          VAL3(16,I)=(UCCRT2+DUCRT2(izone))*0.5
          VAL3(17,I)=(UCCRT3+DUCRT3(izone))*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ELSE
          VAL3(15,I)=(UCCRT1+PUCRT1)*0.5
          VAL3(16,I)=(UCCRT2+PUCRT2)*0.5
          VAL3(17,I)=(UCCRT3+PUCRT3)*0.5
          TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)
        ENDIF

C Controlled portion is total casual gains less the uncontrolled.
        IF(ICC.EQ.1.OR.ICCP.EQ.1)THEN
          VAL3(18,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.2.OR.ICCP.EQ.2)THEN
          VAL3(19,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.3.OR.ICCP.EQ.3)THEN
          VAL3(20,I)=VAL3(21,I)-TUCC
        ELSE
          VAL3(18,I)=0.
          VAL3(19,I)=0.
          VAL3(20,I)=0.
        ENDIF
C Shift future to present.  Save latest control type so that the
C next timestep will be checked to match bps.
        PUCRT1=UCCRT1
        PUCRT2=UCCRT2
        PUCRT3=UCCRT3
        ICCP=ICC
   50 CONTINUE

C Save the last timestep for use the next day.
        DUCRT1(izone)=UCCRT1
        DUCRT2(izone)=UCCRT2
        DUCRT3(izone)=UCCRT3

      RETURN
      END


C ******************** CASTYP ********************

C CASTYP is a generic routine to work out the
C uncontrolled casual gain types 1, 2 & 3 at each time step.
C Given the information in common block, PC3 and P3TYPE returns:
C UCCT1 uncontrolled casual convective type 1,
C UCCT2 uncontrolled casual convective type 2,
C UCCT3 uncontrolled casual convective type 3,
C UCCRT1 uncontrolled casual radiant type 1,
C UCCRT2 uncontrolled casual radiant type 2,
C UCCRT3 uncontrolled casual radiant type 3,
C ICC, controlled casual type
C IF izver >=4 then use direct reads via getallcas and test
C the values of theonectld. If theonectld non-zero set that
C casual gain to zero (because the controlled has been dealt
C with separately.

C With older versions, however, because of the averaging in bps, the beginning and ending
C of each period will have to be shifted in the calling routine.  This
C will be corrected in the revised results library.
      SUBROUTINE CASTYP(IZ,IDAY,ITSTEP,UCCT1,UCCT2,UCCT3,
     &                  UCCRT1,UCCRT2,UCCRT3,ICC)
#include "building.h"
#include "model.h"
#include "geometry.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC

      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Assume that IYEAR is available via RESDEF.
C Determine whether IDAY is a weekday, Saturday or Sunday.
C Determine the month and day of month via EDAYR.
      CALL EDAYR(IDAY,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)

C Find the hour of the day and initialise the values to be returned.
      HOUR=float(ITSTEP)/float(NTS)
      ICC=0
      UCCT1=0.
      UCCRT1=0.
      UCCT2=0.
      UCCRT2=0.
      UCCT3=0.
      UCCRT3=0.

C Set chosen result set.
      ISET=ISIM

C If izver >= 4 the read the data directly.
      if(izver.eq.4)then

C Assume that the original description was from a zone operation file.
        call getallcas(IDAY,IZ,ISET,ITSTEP,QCASR,QCASC,QCASL,FRAC,
     &    perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &    perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &    theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
        if(theonectld.eq.0)then
          UCCT1=perocupc
          UCCT2=perlightc
          UCCT3=perequipc
          UCCRT1=perocupr
          UCCRT2=perlightr
          UCCRT3=perequipr
        elseif(theonectld.eq.1)then
          UCCT1=0.0
          UCCT2=perlightc
          UCCT3=perequipc
          UCCRT1=0.0
          UCCRT2=perlightr
          UCCRT3=perequipr
        elseif(theonectld.eq.2)then
          UCCT1=perocupc
          UCCT2=0.0
          UCCT3=perequipc
          UCCRT1=perocupr
          UCCRT2=0.0
          UCCRT3=perequipr
        elseif(theonectld.eq.3)then
          UCCT1=perocupc
          UCCT2=perlightc
          UCCT3=0.0
          UCCRT1=perocupr
          UCCRT2=perlightr
          UCCRT3=0.0
        endif
        return
      endif

C Note: Because there can be more than one casual gain of each
C type in a zone, UCCRT1 etc. need to be summed.
      IF(NBDAYTYPE.LE.3)THEN
        IDTY=1
        IF(IDWK.EQ.6)THEN
          IDTY=2
        ELSEIF(IDWK.EQ.7)THEN
          IDTY=3
        ENDIF
      ELSE
        IDTY=ICALENDER(IDAY)
      ENDIF

C Check if there are any casual gains at this hour.
C for occupancy, lighting and equipment.
      IF(NCAS(IDTY).GE.1)THEN
        DO 30 ICNN=1,NCAS(IDTY)
          XC=CMGS(IDTY,ICNN)*CONC(IDTY,ICNN)
          XR=CMGS(IDTY,ICNN)*RADC(IDTY,ICNN)
          IF(HOUR.GT.float(ICGS(IDTY,ICNN)).AND.
     &       HOUR.LE.float(ICGF(IDTY,ICNN)))THEN
            IF(ICGT(IDTY,ICNN).EQ.1)THEN
              UCCT1=UCCT1+XC
              UCCRT1=UCCRT1+XR
            ELSEIF(ICGT(IDTY,ICNN).EQ.2)THEN
              UCCT2=UCCT2+XC
              UCCRT2=UCCRT2+XR
            ELSEIF(ICGT(IDTY,ICNN).EQ.3)THEN
              UCCT3=UCCT3+XC
              UCCRT3=UCCRT3+XR
            ELSEIF(ICGT(IDTY,ICNN).EQ.-1)THEN
              UCCT1=UCCT1+(XC*ZBASEA(IZ))
              UCCRT1=UCCRT1+(XR*ZBASEA(IZ))
            ELSEIF(ICGT(IDTY,ICNN).EQ.-2)THEN
              UCCT2=UCCT2+(XC*ZBASEA(IZ))
              UCCRT2=UCCRT2+(XR*ZBASEA(IZ))
            ELSEIF(ICGT(IDTY,ICNN).EQ.-3)THEN
              UCCT3=UCCT3+(XC*ZBASEA(IZ))
              UCCRT3=UCCRT3+(XR*ZBASEA(IZ))
            ENDIF
          ENDIF
   30   CONTINUE
      ENDIF

C Alternative calculation method - return only the uncontrolled casual
C gains and let the calling routines derive the controlled casual gains.

C Note: only one casual gain type can be controlled in a given
C zone. However, there may be more than one of that type in
C existence so they must be summed.

C In the case of casual gain controls
      IF(ICGC(IZ).EQ.1)THEN

C If there is more than one period in this day type, loop for each;
C if there are any casual gains at this hour;
C if there is a control for type 1 (occupancy), test the control periods;
C if one of the periods includes HOUR then save magnitudes;
        IF(NCAS(IDTY).GE.1)THEN
          DO 40 ICNN=1,NCAS(IDTY)
            X7C=CMGS(IDTY,ICNN)*CONC(IDTY,ICNN)
            X7R=CMGS(IDTY,ICNN)*RADC(IDTY,ICNN)
            IF(HOUR.GT.float(ICGS(IDTY,ICNN)).AND.
     &  	     HOUR.LE.float(ICGF(IDTY,ICNN)))THEN

C If time step is in operation period.
              IF(ICGT(IDTY,ICNN).EQ.1.AND.NCGTC(IZ,IDTY).EQ.1)THEN

C If absolute GAINS (W) ARE CONTROLED.
C If time step in the casual gain controled period then adapt control tag ICC.
                IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &             HOUR.LE.float(ICGCF(IZ))) ICC= 1
              ELSEIF(ICGT(IDTY,ICNN).EQ.-1.AND.NCGTC(IZ,IDTY).EQ.1)THEN

C If relative GAINS (W/2) ARE CONTROLED.
        	IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &  	   HOUR.LE.float(ICGCF(IZ))) ICC= 1
              ELSEIF(ICGT(IDTY,ICNN).EQ.1)THEN

C If absolute GAINS (W) ARE NOT CONTROLED.
        	UCCT1=UCCT1+X7C
        	UCCRT1=UCCRT1+X7R
              ELSEIF(ICGT(IDTY,ICNN).EQ.-1)THEN

C If relative GAINS (W/2) ARE NOT CONTROLED.
        	UCCT1=UCCT1+(X7C*ZBASEA(IZ))
        	UCCRT1=UCCRT1+(X7R*ZBASEA(IZ))
              ENDIF

C If there is a control for type 2 (lighting), test the control periods.
C If one of the periods includes HOUR then save magnitudes.
              IF(ICGT(IDTY,ICNN).EQ.2.AND.NCGTC(IZ,IDTY).EQ.2)THEN
        	IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &  	   HOUR.LE.float(ICGCF(IZ))) ICC= 2
              ELSEIF(ICGT(IDTY,ICNN).EQ.-2.AND.NCGTC(IZ,IDTY).EQ.2)THEN
        	IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &  	   HOUR.LE.float(ICGCF(IZ))) ICC= 2
              ELSEIF(ICGT(IDTY,ICNN).EQ.2)THEN

C Casual gain type 2 is not controlled so set magnitudes.
        	UCCT2=UCCT2+X7C
        	UCCRT2=UCCRT2+X7R
              ELSEIF(ICGT(IDTY,ICNN).EQ.-2)THEN
        	UCCT2=UCCT2+(X7C*ZBASEA(IZ))
        	UCCRT2=UCCRT2+(X7R*ZBASEA(IZ))
              ENDIF

C If there is a control for type 3 (equipment), test the control periods.
C If one of the periods includes HOUR then save magnitudes.
              IF(ICGT(IDTY,ICNN).EQ.3.AND.NCGTC(IZ,IDTY).EQ.3)THEN
        	IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &  	   HOUR.LE.float(ICGCF(IZ))) ICC= 3
              ELSEIF(ICGT(IDTY,ICNN).EQ.-3.AND.NCGTC(IZ,IDTY).EQ.3)THEN
        	IF(HOUR.GT.float(ICGCS(IZ)).AND.
     &  	   HOUR.LE.float(ICGCF(IZ))) ICC= 3
              ELSEIF(ICGT(IDTY,ICNN).EQ.3)THEN

C Casual gain type 3 is not controlled so set magnitudes.
        	UCCT3=UCCT3+X7C
        	UCCRT3=UCCRT3+X7R
              ELSEIF(ICGT(IDTY,ICNN).EQ.-3)THEN
        	UCCT3=UCCT3+(X7C*ZBASEA(IZ))
        	UCCRT3=UCCRT3+(X7R*ZBASEA(IZ))
              ENDIF
            ENDIF
   40     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END


C ******************** ZCASCF ********************
C ZCASCF reconstructs one day's unaveraged values of each of 
C the uncontrolled and controlled convective casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C The method does not shift/average data from the operations file.
C The parameter IDAY is the day of the week (1-7).
C The parameter IJDAY is the julian day of the year (for the case
C of checking occupancy on a specific day). If IJDAY is zero then
C do not bother.

C VAL3(15,?) holds uncontrolled casual type 1.
C VAL3(16,?) holds uncontrolled casual type 2.
C VAL3(17,?) holds uncontrolled casual type 3.
C VAL3(18,?) holds controlled casual type 1.
C VAL3(19,?) holds controlled casual type 2.
C VAL3(20,?) holds controlled casual type 3.

      SUBROUTINE ZCASCF(IZONE,IDAY,IJDAY)
#include "building.h"
#include "geometry.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM)

C.........START: USED FOR ACTIVITY TDF DATA.....................
C TDFINT are ranges allowed for integer fields in the instance.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)

      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      REAL X2a_QTOTOCCUP
      REAL X2c_QTOTEQUIP
C.........END: USED FOR ACTIVITY TDF DATA.....................

      DIMENSION VAL(MBITS+2)
C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      IEND=24*NTS

C Initialise 'present values'.
      ICCP=0

C If izver >= 4 the read the data directly.
      if(izver.eq.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0.and.IJDAY.ne.0)found = .true.  ! if separate casual item
        if(IACTIV(izone).ne.0.and.IJDAY.ne.0)found = .true.   ! if UK activity item
        if(IACTIV(izone).ne.0)found = .true.
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perocupc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightc
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipc
            endif
  142     continue
          return
        endif
      endif

C Check to see if there is a temporal database casual gain 
C entry for this zone (the values are explicitly saved).
      found = .false.
      if(ICASUAL(izone).ne.0)found = .true.
      IF(found)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
        DO 151, I=1,IEND
          VAL3(15,I)=0.0
          VAL3(16,I)=0.0
          VAL3(17,I)=VAL3(21,I)
          VAL3(18,I)=0.0
          VAL3(19,I)=0.0
          VAL3(20,I)=0.0
 151    CONTINUE
        RETURN
      ENDIF

C Check to see if there is a temporal casual gain (separate)
C entry for this zone (the values are explicitly saved).
      found = .false.
      if(ICASUAL3(izone).ne.0.and.IJDAY.ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 10 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 20 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IJDAY.EQ.1)IDP=365
C            IF(IJDAY.GT.1)IDP=IJDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get future timestep convective columns of the temporal item.
            itdi=ICASUAL3(izone)
            IFOC=itdi
            CALL RCTDFR(0,IJDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a=VAL(ISD)
            X2b=VAL(ISD+3)
            X2c=VAL(ISD+6)
            VAL3(15,IJ)=X2a
            VAL3(16,IJ)=X2b
            VAL3(17,IJ)=X2c
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
C          WRITE(OUTS,'(A,I3,4F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vd ',
C     &      IJ,T,TP,TF,TFL,IJDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
   20     CONTINUE
   10   CONTINUE
        RETURN
      ENDIF

C Check to see if there is a temporal casual gain (separate)
C entry for this zone (the values are explicitly saved).
      found = .false.
      if(IACTIV(izone).ne.0.and.IJDAY.ne.0)found = .true.
      IF(found)THEN

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 110 I=1,24
          I1=I-1
C          I2=I
          T1=FLOAT(I1)
C          T2=FLOAT(I2)
          DO 120 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)
C            IF(IJDAY.EQ.1)IDP=365
C            IF(IJDAY.GT.1)IDP=IJDAY-1
C            TFL=24.0-(1./real(NTS))
C            LTSOD=24*NTS

C Get future timestep convective columns of the temporal item.
            itdi=IACTIV(izone)
            IFOC=itdi
            CALL RCTDFR(0,IJDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a_QTOTOCCUP=TMAR(1,1)*ITMAR(1,2)*VAL(ISD)*ZBASEA(izone)
            X2a=(X2a_QTOTOCCUP-((X2a_QTOTOCCUP*ITMAR(1,3))/100))/2

C<<< let the lighting gains to be 0 for now until there is a
C a decision on the calculation of the lighting gains>>
            X2b=0
            X2c_QTOTEQUIP=TMAR(1,4)*VAL(ISD+2)*ZBASEA(izone)
            X2c=(X2c_QTOTEQUIP-((X2c_QTOTEQUIP*ITMAR(1,5))/100))/2
 
            VAL3(15,IJ)=X2a
            VAL3(16,IJ)=X2b
            VAL3(17,IJ)=X2c
            VAL3(18,IJ)=0.0
            VAL3(19,IJ)=0.0
            VAL3(20,IJ)=0.0

C Debug...
C          WRITE(OUTS,'(A,I3,4F6.2,I4,3F8.2)') 'IJ T TP TF TFL ID Vd ',
C     &      IJ,T,TP,TF,TFL,IJDAY,VAL3(15,IJ),VAL3(16,IJ),
C     &      VAL3(17,IJ)
C            CALL EDISP(IUOUT,OUTS)
  120     CONTINUE
  110   CONTINUE
        RETURN
      ENDIF

      IF(IDAY.EQ.IODS.AND.IDAY.EQ.ISDS)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ELSEIF(IDAY.EQ.IODS.AND.IDAY.GT.ISDS)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ELSEIF(IDAY.GT.IODS)THEN
        CALL GCASC(IDAY,IZONE,ISET) ! for older versions.
        CALL XTVAL3(GVAL,1,IEND,21)
      ENDIF

C For each time step determine each of the uncontrolled casual gains.
      DO 50, I=1,IEND
        CALL CASTYP(IZONE,IDAY,I,UCCT1,UCCT2,UCCT3,
     &              UCCRT1,UCCRT2,UCCRT3,ICC)

C Save fluxs in VAL1(8 to 13,N).
        VAL3(15,I)=UCCT1
        VAL3(16,I)=UCCT2
        VAL3(17,I)=UCCT3
        TUCC=VAL3(15,I)+VAL3(16,I)+VAL3(17,I)

C Controlled portion is total casual gains less the uncontrolled.
        IF(ICC.EQ.1.OR.ICCP.EQ.1)THEN
          VAL3(18,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.2.OR.ICCP.EQ.2)THEN
          VAL3(19,I)=VAL3(21,I)-TUCC
        ELSEIF(ICC.EQ.3.OR.ICCP.EQ.3)THEN
          VAL3(20,I)=VAL3(21,I)-TUCC
        ELSE
          VAL3(18,I)=0.
          VAL3(19,I)=0.
          VAL3(20,I)=0.
        ENDIF
        ICCP=ICC

   50 CONTINUE
      RETURN
      END

