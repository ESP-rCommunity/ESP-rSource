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

C      MZCLMI
C      MZCLMPT

C ******************** MZCLMI ********************

C MZCLMI interpolates between the present and future
C climatic parameters to predict the values
C prevailing at the future time-row of the current time
C step.

C Interpolation formula is:

C X = X1 + (RATIO * (X2 - X1))

C where:

C      X = interpolated value at time T (T1<=T<=T2)
C      X1= value of climatic parameter at time T1
C      X2= value of climatic parameter at time T2

C RATIOS is used instead of RATIO for the solar parameters as it may be
C different from RATIO if half-hour centred solar data is used.

      SUBROUTINE MZCLMI(IOPT,PATMOS,X1,X2,RATIO,RATIOS,QFFX,TFX,QDFX,
     &VFX,DFX,HFX,TWBFX,HEXTFX,GEXTFX)
#include "building.h"
#include "tdf2.h"

C TDF commons.
      COMMON/DNORGH/IRTYPE
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)

C For each zone a pointer to relevant temporal db entry type 0=not used.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS)

      DIMENSION X1(6),X2(6)

C Diffuse horizontal radiation.
C If using TDF database the value is that returned for the future time.
      QFFX=X1(1)+(RATIOS*(X2(1)-X1(1)))
      IF(IDIFHSL.NE.0)QFFX=X2(1)
      IF(IALLCLM.NE.0)QFFX=X2(1)

C External temperature.
      TFX=X1(2)+(RATIO*(X2(2)-X1(2)))
      IF(IDBTEXT.NE.0)TFX=X2(2)
      IF(IALLCLM.NE.0)TFX=X2(2)

C Direct normal radiation.
      QDFX=X1(3)+(RATIOS*(X2(3)-X1(3)))
      if(IDIRSOL.NE.0)then
        QDFX=X2(3)
        IRTYPE=0
      endif

C Global horizontal - if from TDF db, reset IRTYPE.
      IF(IGLOHSL.NE.0)then
        QDFX=X2(3)
        IRTYPE=1
      endif
      IF(IALLCLM.NE.0)then
        QDFX=X2(3)
        itdi=IALLCLM
        IACC1=ITMAR(itdi,IATDF(itdi,1))
        if(IACC1.eq.0)then
         IRTYPE=0
        elseif(IACC1.eq.123)then
         IRTYPE=1
        endif
      endif

C Wind speed.
      VFX=X1(4)+(RATIO*(X2(4)-X1(4)))
      IF(IWINDVL.NE.0)VFX=X2(4)
      IF(IALLCLM.NE.0)VFX=X2(4)

C Wind direction.
      DFX=X1(5)+(RATIO*(X2(5)-X1(5)))
      IF(IWINDDR.NE.0)DFX=X2(5)
      IF(IALLCLM.NE.0)DFX=X2(5)

C Relative humidity.
      HFX=X1(6)+(RATIO*(X2(6)-X1(6)))
      IF(IRELHUM.NE.0) HFX=X2(6)
      IF(IALLCLM.NE.0) HFX=X2(6)

C Wet bulb temperature, moisture content and enthalpy.
      GEXTFX=HUMRT1(TFX,HFX,PATMOS,IOPT)
      HEXTFX=ENTHP2(TFX,GEXTFX)
      HEXTFX=HEXTFX*1000.
      TWBFX=TWB(TFX,GEXTFX,PATMOS,IOPT)

      RETURN
      END
      

C ******************** MZCLMPT ********************

C This routine prepares the climatic information for interpolation
C in routine MZCLMI, making use of any temporal definition flags
C which might have been setup. This routine is called to prepare data
C for building-side (ITYP=1) and plant-side calculations (ITYP=2)
C because the plant can operate at some subset of the building
C time step.

      SUBROUTINE MZCLMPT(ITYP)
#include "building.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/trc/itrc
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/btime/btimep,btimef

      COMMON/DNORGH/IRTYPE
      COMMON/CLIM/IDIF(MT),ITMP(MT),IDNR(MT),IVEL(MT),IDIR(MT),
     &            IHUM(MT),IDIFF,ITMPF,IDNRF,IVELF,IDIRF,IHUMF

      COMMON/CLIMWB/TWBP,TWBF
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP
      COMMON/WBULBO/IOPT
      COMMON/ATPRES/PATMOS

      COMMON/PCTSTP/NTSTPP
      COMMON/PCTINC/IPTS

      COMMON/TS1/NF(MSCH),IFL(MSCH,MCVT1),FD(MSCH,MCVT1),ITSCF1,
     &           NMAX1(MSCH)
      COMMON/CLMFLG/ICLMFL

C TDF commons.
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)

C For each zone a pointer to relevant temporal db entry 0=not used.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS)

C Treatment of solar radiation data in the climate file.
C ESP-r operates on hourly-centred data. That is, the solar
C irradiance at the recorded hour is the instantaneous irradiance at the
C hour (or the average from half-hour to half-hour). In some climate datasets
C (e.g.Canadian Weather for Energy Calculations (CWEC) files), solar
C radiation is half-hour centred - it is averaged over the previous hour. 
C To account for the half hour shift, a flag indicates
C whether solar radiation data in the weather file is hour-centred
C (default) or half-hour centred.

C The flag (iSlr_half_hr_flg) can be set in the .cfg file or in the bps 
C 'simulation toggles' menu.
C iSlr_half_hr_flg =0 = hour-centred; 1 = half-hour centred.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg

      DIMENSION X1B(6),X2B(6),X1P(6),X2P(6)
      DIMENSION VAL(MBITS+2)

C IOPT=2 ie screen wet bulb temerature required
C PATMOS set here as a tempory measure.
      IOPT=2
      PATMOS=1013.25

      IF(ITYP.EQ.2)goto 1

C Compute future time-row values. (T1 & T2 Not used)
      T1=IHRP
      IF(IHRP.EQ.24)T1=0.
      T2=IHRF

C Equate present time-row values for this time-step to
C future time-row values of previous time-step.
      IF(NSINC.GT.1.OR.ITS.GT.1.OR.ITSCF1.EQ.1)goto 2
      TP=FLOAT(ITMP(1))/10.
      VP=FLOAT(IVEL(1))/10.
      DP=FLOAT(IDIR(1))
      HP=FLOAT(IHUM(1))
      if ( iSlr_half_hr_flg .eq. 0 ) then

C Solar radiation data from weather file is hour-centered.
         QFP=FLOAT(IDIF(1))
         QDP=FLOAT(IDNR(1))
      else

C Solar radiation data from weather file is half-hour centered.
         qfp = 0.5 * (FLOAT(idif(1)) + FLOAT(idif(2)))
         qdp = 0.5 * (FLOAT(idnr(1)) + FLOAT(idnr(2)))
      endif
      goto 3
    2 QFP=QFF
      TP=TF
      QDP=QDF
      VP=VF
      DP=DF
      HP=HF
      TWBP=TWBF
      HEXTP=HEXTF
      GEXTP=GEXTF

    3 CONTINUE

C Set base values from common blocks.
C Diffuse horizontal radiation, external temperature, direct normal radiation.
      X1B(1)=FLOAT(IDIF(IHRP))
      X2B(1)=FLOAT(IDIF(IHRF))
      X1B(2)=FLOAT(ITMP(IHRP))/10.
      X2B(2)=FLOAT(ITMP(IHRF))/10.
      X1B(3)=FLOAT(IDNR(IHRP))
      X2B(3)=FLOAT(IDNR(IHRF))

C Wind speed, direction and RH.
      X1B(4)=FLOAT(IVEL(IHRP))/10.
      X2B(4)=FLOAT(IVEL(IHRF))/10.
      X1B(5)=FLOAT(IDIR(IHRP))
      X2B(5)=FLOAT(IDIR(IHRF))
      X1B(6)=FLOAT(IHUM(IHRP))
      X2B(6)=FLOAT(IHUM(IHRF))
      IF(IHRF.EQ.1)then
        X2B(1)=FLOAT(IDIFF)
        X2B(2)=FLOAT(ITMPF)/10.
        X2B(3)=FLOAT(IDNRF)
        X2B(4)=FLOAT(IVELF)/10.
        X2B(5)=FLOAT(IDIRF)
        X2B(6)=FLOAT(IHUMF)
      endif


C In terms of a database the initial timestep of the day is:
      fstep=1./REAL(NTSTEP)

C If allclimate from temporal db, process.
      if(IALLCLM.ne.0)THEN
        itdi=IALLCLM
        IFOC=itdi
        IACC1=ITMAR(itdi,IATDF(itdi,1))
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(1)=VAL(ISD)
        X2B(2)=VAL(ISD+1)
        X2B(3)=VAL(ISD+2)
        X2B(4)=VAL(ISD+3)
        X2B(5)=VAL(ISD+4)
        X2B(6)=VAL(ISD+5)

C If the last timestep of the day then get profile for the next day.
        IF(btimef.LE.fstep)THEN
          CALL RCTDFB(itrc,fstep,VAL,ISD,IFOC,IER)
          X2B(1)=VAL(ISD)
          X2B(2)=VAL(ISD+1)
          X2B(3)=VAL(ISD+2)
          X2B(4)=VAL(ISD+3)
          X2B(5)=VAL(ISD+4)
          X2B(6)=VAL(ISD+5)
        ENDIF
C        write(6,*)'tdf clm data recovered @',btimef,' is ',X2B
      endif

C Diffuse horizontal radiation. If to be superseded by data from
C the temporal database then establish values to interpolate from.
      IF(IDIFHSL.ne.0)THEN
        itdi=IDIFHSL
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(1)=VAL(ISD)
      ENDIF

C External temperature. If to be superseded by data from the
C temporal database then establish values to interpolate from.
      IF(IDBTEXT.ne.0)THEN
        itdi=IDBTEXT
        IFOC=itdi

C Get data from db and return future value to X2B array.
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(2)=VAL(ISD)

C If the last timestep of the day then get profile for the next day.
        IF(btimef.LE.fstep)THEN
          CALL RCTDFB(itrc,fstep,VAL,ISD,IFOC,IER)
          X2B(2)=VAL(ISD)
        ENDIF
      ENDIF

C If currently working with Global Horizontal jump.
      if(IRTYPE.eq.1) goto 32

C Direct normal radiation. If to be superseded by data from the
C temporal database then establish values to interpolate from.
      IF(IDIRSOL.ne.0)THEN
        itdi=IDIRSOL
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(3)=VAL(ISD)
      ENDIF

C Global horizontal radiation. If to be superseded by data from the
C temporal database then establish values to interpolate from.
      if(IRTYPE.eq.0) goto 33
  32  IF(IGLOHSL.ne.0)THEN
        itdi=IGLOHSL
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(3)=VAL(ISD)
      ENDIF

C Wind speed. If to be superseded by data from the
C temporal database then establish values to interpolate from.
  33  IF(IWINDVL.ne.0)THEN
        itrc=1
        itdi=IWINDVL
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(4)=VAL(ISD)

C If the last timestep of the day then get first profile for the
C next day.
        IF(btimef.LE.fstep)THEN
          CALL RCTDFB(itrc,fstep,VAL,ISD,IFOC,IER)
          X2B(4)=VAL(ISD)
        ENDIF
      ENDIF

C Wind direction. If to be superseded by data from the
C temporal database then establish values to interpolate from.
      IF(IWINDDR.ne.0)THEN
        itdi=IWINDDR
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(5)=VAL(ISD)

C If last timestep of the day get first profile for the next day.
        IF(btimef.LE.fstep)THEN
          CALL RCTDFB(itrc,fstep,VAL,ISD,IFOC,IER)
          X2B(5)=VAL(ISD)
        ENDIF
      ENDIF

C Relative humidity. If to be superceeded by data from the
C temporal database then establish values to interpolate from.
      IF(IRELHUM.ne.0)THEN
        itdi=IRELHUM
        IFOC=itdi
        CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
        X2B(6)=VAL(ISD)

C If last timestep of the day get first profile for the next day.
  62    IF(btimef.LE.fstep)THEN
          CALL RCTDFB(itrc,fstep,VAL,ISD,IFOC,IER)
          X2B(6)=VAL(ISD)
        ENDIF
      ENDIF

C Ratio of time step.
      RATIO=FLOAT(ITS)/FLOAT(NTSTEP)

C If solar radiation data from weather file is half-hour centred, then
C apply a half hour time shift to the solar data. 
C If climate interpolation is off or if the solar data is from the 
C temporal database, then do not apply shift.
      ratios=ratio
      if(iSlr_half_hr_flg.eq.1 .and. iclmfl.eq.0 
     &        .and. idifhsl.eq.0 .and. iallclm.eq.0) then
        ratios=ratios+0.5
        if(ratios.gt.1.0)then

C Get solar data for next hour.
C Present time row.
          X1B(1)=X2B(1)
          X1B(3)=X2B(3)

C Future time row.
          if (ihrf .eq. 24) then
            X2B(1)=FLOAT(idiff)
            X2B(3)=FLOAT(idnrf)       
          else
            X2B(1)=FLOAT(idif(ihrf+1))
            X2B(3)=FLOAT(idnr(ihrf+1))
          endif

C Reset ratio for solar radiation.
          ratios=ratios-1.0
        endif
      endif


C No interpolation if ICLMFL = 1.
       IF(ICLMFL.EQ.1)THEN
         QFF=X2B(1)
         TF=X2B(2)
         QDF=X2B(3)
         VF=X2B(4)
         DF=X2B(5)
         HF=X2B(6)
C         GEXTFX=HUMRT1(TF,HF,PATMOS,IOPT)
      ELSE

C Call climate interpolation routine.
         CALL MZCLMI(IOPT,PATMOS,X1B,X2B,RATIO,RATIOS,QFF,TF,QDF,VF,DF,
     &               HF,TWBF,HEXTF,GEXTF)
      ENDIF

C Assign flow climate variables.
      QFLWP=QFP
      QFLWF=QFF
      TFLWP=TP
      TFLWF=TF
      QDFLP=QDP
      QDFLF=QDF
      VFLP=VP
      VFLF=VF
      DFLP=DP
      DFLF=DF
      HFLP=HP
      HFLF=HF

      IF(NSINC.GT.1)RETURN

C Wet bulb temperature, moisture content and enthalpy.
      TWBP=TWBF
      GEXTP=GEXTF
      HEXTP=HEXTF
      RETURN

C Plant climate interpolation.
    1 IF(NSINC.GT.1.OR.IPTS.GT.1)goto 5

C Equate present time-row values for this time-step to
C future time-row values of previous time-step.
      QFPP=QFP
      TPP=TP
      QDPP=QDP
      VPP=VP
      DPP=DP
      HPP=HP
      TWBPP=TWBP
      GEXTPP=GEXTP
      HEXTPP=HEXTP
      goto 6

    5 QFPP=QFFP
      TPP=TFP
      QDPP=QDFP
      VPP=VFP
      DPP=DFP
      HPP=HFP
      TWBPP=TWBFP
      GEXTPP=GEXTFP
      HEXTPP=HEXTFP

    6 IF(IPTS.GT.1)goto 7
      X1P(1)=QFP
      X2P(1)=QFF
      X1P(2)=TP
      X2P(2)=TF
      X1P(3)=QDP
      X2P(3)=QDF
      X1P(4)=VP
      X2P(4)=VF
      X1P(5)=DP
      X2P(5)=DF
      X1P(6)=HP
      X2P(6)=HF

    7 RATIO=FLOAT(IPTS)/FLOAT(NTSTPP)
      RATIOS=RATIO

      CALL MZCLMI(IOPT,PATMOS,X1P,X2P,RATIO,RATIOS,QFFP,TFP,QDFP,
     &            VFP,DFP,HFP,TWBFP,HEXTFP,GEXTFP)

C Assign flow climate variables.
      QFLWP=QFPP
      QFLWF=QFFP
      TFLWP=TPP
      TFLWF=TFP
      QDFLP=QDPP
      QDFLF=QDFP
      VFLP=VPP
      VFLF=VFP
      DFLP=DPP
      DFLF=DFP
      HFLP=HPP
      HFLF=HFP

      RETURN
      END
