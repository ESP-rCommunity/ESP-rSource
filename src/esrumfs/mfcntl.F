C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

      SUBROUTINE MFCNTR

#include "building.h"
#include "net_flow.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/mfctl/ctlpos(MCNN)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,NIN

      character outs*124

      do 102 i=1,MCNN
        ctlpos(i)=1.0
 102  continue

C Return if no mass flow control function active
      IF(NCC.EQ.0) GOTO 999

C Determine year day number of present day
      IDAY=IYD

      DO 100 ICC=1,NCC
      ICFP=ICC
C Set up day type and period pointers
      NDAYT=NFCDT(ICFP)

c determine current day of the week
      CALL EWEEKD(IRD,IRM,IRY,IDW)

C If NDAYT=0 set data to all day types
      NIN=0
      IF(NDAYT.EQ.0)THEN
        NDAYT=NBDAYTYPE
        NIN=-1*NBDAYTYPE
      ENDIF
      DO 10 IDTYPP=1,NDAYT
        IDS=IFCDV(ICFP,IDTYPP,1)
        IDF=IFCDV(ICFP,IDTYPP,2)
        IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10 CONTINUE
      WRITE(outs,*) ' MFCNTR: mass flow control connection ',ICFP
      call edisp(iuout,outs)
      WRITE(outs,*) '	      no valid day type for year-day ',IDAY
      call edisp(iuout,outs)
      call edisp(iuout,
     &       ' MFCNTR: cannot locate appropriate day type')
      call epwait
      CALL EPAGEND
      STOP

C Check number of periods in each day and the start and finish times
   20 if(nin.le.-1.or.ndayt.lt.1)idtypp=icalender(iday)
      NDAYP=NFCDP(ICFP,IDTYPP)
      IF(NDAYP.EQ.0) STOP ' MFCNTR: no day-periods defined'
      DO 22 IDAYP=1,NDAYP
      IPERP=IDAYP
      TPS=TFCPS(ICFP,IDTYPP,IDAYP)
      IF(IDAYP.LT.NDAYP) THEN
         TPF=TFCPS(ICFP,IDTYPP,IDAYP+1)
      ELSE
         TPF=24.
      END IF
      IF(FLWTIM.GT.TPS.AND.FLWTIM.LE.TPF) GOTO 30
   22 CONTINUE
        call edisp(iuout,'MFCNTR: cannot locate appropriate day-period')
      call epwait
      CALL EPAGEND
      STOP

C Valid period established; now invoke appropriate control law routine
C Mass flow control law 0: ON/OFF controller
   30 IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.0)THEN
         CALL FCL00

C Mass flow control law 1: Proportional controller with hysteresis
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.1)THEN
         CALL FCL01
C Mass flow control law 2: Range controller for components 30 35 40 110.
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.2)THEN
         CALL FCL02
C Mass flow control law 3: multi-sensor ON/OFF controller
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.3)THEN
         CALL FCL03
C Nicol control algorithm
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.4)THEN
         CALL FCL04
C PI controller
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.5)THEN
         CALL FCL05
C Mass flow control law 6: ON/OFF controller with hysteresis
      ELSE IF(IFCLAW(ICFP,IDTYPP,IPERP).EQ.6)THEN
         CALL FCL06

C New controllers inserted here!
      ELSE
        WRITE(outs,*) 'MFCNTR: invalid m. flow control law ',
     &    IFCLAW(ICFP,IDTYPP,IPERP),' has been referenced ',
     &    ICFP,IDTYPP,IPERP
        call edisp(iuout,outs)
        call epwait
        CALL EPAGEND
        STOP
      END IF
 100  CONTINUE

 999  RETURN
      END

C ******************** CFFVAR ********************

C CFFVAR locates the sensor, checks if the sensor corresponds with
C prevailing controller type, determines sensed condition (SVCTL).

      SUBROUTINE CFFVAR(SVCTL,icnn)
#include "plant.h"
#include "building.h"
#include "net_flow.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl4/iasocc(MCF,MCMP),nfsup(MCF)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/CONTM8/CCONC(MNOD,MCONTM)

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/C14PS/NDCON(MPCOM,MNODEC),ISV(MPCOM,MNODEC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

      COMMON/FVALG/GFA(MCOM) ! for humidity control
      common/atpres/patmos

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      character outs*124

C external function reference
      real pcrh2
      
C Establish sensor location
      IFS1=IFSN(ICFP,1)
      IFS2=IFSN(ICFP,2)
      IFS3=IFSN(ICFP,3)
      IFS4=IFSN(ICFP,4) !  for humidity control

C Locate the actuated flow connection
      IF((IFAN(ICFP,1).lt.-4).or.(IFAN(ICFP,1).gt.-3)) THEN
        WRITE(outs,*) ' CFFVAR: mass flow control function ',ICFP
        call edisp(iuout,outs)
        call edisp(iuout,
     &    ' CFFVAR: invalid actuator location referenced')
        call epwait
        CALL EPAGEND
        STOP
      END IF

C Match mass flow controller type.
      ICLCTP=IFCTYP(ICFP,IDTYPP,IPERP)
      if(IFS1.EQ.-3) THEN
        if(ICLCTP.gt.0.and.ICLCTP.le.7)then
          WRITE(outs,*) ' CFFVAR: mass flow control function ',ICFP
          call edisp(iuout,outs)
          WRITE(outs,*) 
     &      ' CFFVAR: invalid mass flow sensed external condition ',
     &      ICLCTP,' day type ',IDTYPP,' period ',IPERP
          call edisp(iuout,outs)
          call epwait
          CALL EPAGEND
          STOP
        endif
      else
        if((ICLCTP.NE.1.AND.ICLCTP.NE.2.AND.ICLCTP.NE.3
     &           .AND.ICLCTP.NE.4.AND.ICLTP.NE.5.AND.ICLCTP.NE.7
     &           .AND.ICLCTP.NE.9.AND.ICLCTP.NE.11.AND.ICLCTP.NE.41)
     &           .AND.(ICLCTP.LT.24.OR.ICLCTP.GT.34))then
          WRITE(outs,*) ' CFFVAR: mass flow control function ',ICFP
          call edisp(iuout,outs)
          WRITE(outs,*) ' CFFVAR: invalid mass flow cntrl type ',
     &      ICLCTP,' day type ',IDTYPP,' period ',IPERP
          call edisp(iuout,outs)
          call epwait
          CALL EPAGEND
          STOP
        endif
      endif

C Determine sensed condition depending on the sensor location
C Sensor measures building zone related temperature or humidity
      IF(IFS1.GT.0) THEN
        SVCTL=TFA(IFS1)
        IF(IFS2.GT.0.AND.IFS3.EQ.0) SVCTL=TFS(IFS1,IFS2)
        IF(IFS2.GT.0.AND.IFS3.GT.0) SVCTL=TFC(IFS1,IFS2,IFS3)
c Sense humidity: -66 R.H., -77 moisture content
        IF(IFS4.EQ.-66) then 
          SVCTL=pcrh2(TFA(IFS1),GFA(IFS1),PATMOS) ! zone relative humidity, %
          SVCTL=min(100.0,SVCTL) ! more than 100% is not possible, however pcrh2 returns values in excess of this!!
        endif
        IF(IFS4.EQ.-77) then 
          SVCTL=GFA(IFS1)
        endif

C Sensor measures plant node state-space variable
      ELSE IF(IFS1.EQ.-1.AND.IFS3.GT.0) THEN
        IN=NPCDAT(IFS2,9)+IFS3-1

C Jump depending on sensed property for control function controller type
C Controller type 1: senses temperature
        IF(ICLCTP.EQ.1) THEN
          SVCTL=CSVF(IN,1)

C Controller type 3: senses fluid enthalpy (kJ/kg) which is
C calculated from temp. and specific heat
C                         Note that ISV=10   indicates water
C                                   ISV=1,11 indicates dry air
C                                   ISV=21   indicates moist air
        ELSE IF(ICLCTP.EQ.3) THEN
          IF(ISV(IFS2,IFS3).EQ.10) THEN
            SVCTL=CSVF(IN,1)*SHTH2O(CSVF(IN,1))
          ELSE IF(ISV(IFS2,IFS3).EQ.1.OR.ISV(IFS2,IFS3).EQ.11) THEN
            SVCTL=ENTHP2(CSVF(IN,1),0.)
          ELSE IF(ISV(IFS2,IFS3).EQ.21) THEN
            SVCTL=ENTHP2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2))
          ELSE
            WRITE(outs,*) ' CFFVAR: loop ',ICFP,' type ',ICLCTP
            call edisp(iuout,outs)
            call edisp(iuout,
     &           ' CFFVAR: invalid SSV for mass flow controller type ')
            call epwait
            CALL EPAGEND
            STOP
          END IF

C Controller type 5: senses 1th phase mass flow rate (kg/s)
        ELSE IF(ICLCTP.EQ.5) THEN
          IF(ISV(IFS2,IFS3).LT.10)then
            call edisp(iuout,
     &         ' CFFVAR: invalid SSV for mass flow controller type')
            call epwait
            CALL EPAGEND
            STOP
          endif
          SVCTL=CSVF(IN,2)

C Controller type 7: senses 2nd phase mass flow rate (kg/s)
        ELSE IF(ICLCTP.EQ.7) THEN
          IF(ISV(IFS2,IFS3).LT.21)then
            call edisp(iuout,
     &         ' CFFVAR: invalid SSV for mass flow controller type')
            call epwait
            CALL EPAGEND
            STOP
          endif
          SVCTL=CSVF(IN,3)

C Controller type 9: senses some additional plant output
        ELSE IF(ICLCTP.EQ.9) THEN
          SVCTL=PCAOUT(IFS2,IFS3)

C Controller type 11 senses relative humidity (%).
        ELSE IF(ICLCTP.EQ.11) THEN
          PATMOS=1013.25
          SVCTL=PCRH2(CSVF(IN,1),CSVF(IN,3)/CSVF(IN,2),PATMOS)
        ELSE
          call edisp(iuout,
     &        ' CFFVAR: invalid mass flow controller type')
          call epwait
          CALL EPAGEND
          STOP
        END IF

C Sensor measures a mix of zone air and mean radiant temperature
      ELSE IF(IFS1.EQ.-2) THEN
         CALL MZMIXT(IFS2,TMRT,TMIX)
         CONV=FLOAT(IFS3)/100.
         SVCTL=TFA(IFS2)*CONV+TMRT*(1.-CONV)

C Sensor measures outside condition; Note sol-air temperature not yet available.
      ELSE IF(IFS1.EQ.-3) THEN
        IF(IFS2.EQ.0.AND.IFS3.EQ.0) THEN
          SVCTL=TFLWF
        else IF(IFS2.EQ.2)then
          SVCTL=VFLF
        else IF(IFS2.EQ.3)then
          SVCTL=DFLF
        else IF(IFS2.EQ.4)then
          SVCTL=QFLWF
        else IF(IFS2.EQ.5)then
          SVCTL=QDFLF
        else IF(IFS2.EQ.6)then
          SVCTL=HFLF
        ELSE
          WRITE(outs,*)' CFFVAR: function ',ICFP,'; outside conditions'
          call edisp(iuout,outs)
          call edisp(iuout,
     &      ' CFFVAR: mass flow control sensor not yet active')
          call epwait
          CALL EPAGEND
          STOP
        END IF

C Sensor measures a flow side condition.
      ELSEIF(IFS1.EQ.-4) THEN

C In case the control concerns a single connection:
        IF(IFAN(ICFP,1).EQ.-3)THEN

C Controller type 1 senses node temperature.
          IF(ICLCTP.EQ.1)then
            SVCTL=TNOD(IFS2)

C Controller type 5 senses mass flow at a connection.
          elseif(ICLCTP.EQ.5)then
            SVCTL=sngl(flw1(ifs2)+flw2(ifs2))

C Controller type 24 senses temperature difference between nodes.
          elseif(ICLCTP.EQ.24)then
            SVCTL=TNOD(ifs2)-TNOD(ifs3)

C Controller type 25 senses ABS temperature difference between nodes.
          elseif(ICLCTP.EQ.25)then
            SVCTL=ABS(TNOD(ifs2)-TNOD(ifs3))

C Controller type 26 senses node pressure.
          elseif(ICLCTP.EQ.26)then
            SVCTL=sngl(pres(ifs2))

C Controller type 27 senses pressure difference between nodes.
          elseif(ICLCTP.EQ.27)then
            SVCTL=sngl(PRES(ifs2)-PRES(ifs3))

C Controller type 28 senses ABS pressure difference between nodes.
          elseif(ICLCTP.EQ.28)then
            SVCTL=sngl(DABS(PRES(ifs2)-PRES(ifs3)))

C Controller type 29 senses ABS mass flow at a connection.
          elseif(ICLCTP.EQ.29)then
            SVCTL=sngl(DABS(FLW1(ifs2)+FLW2(ifs2)))

C Controller type 41 senses contaminant concentration at node
          elseif(ICLCTP.EQ.41)then
            SVCTL=CCONC(IFS2,IFS3)*1000.
          else 
            WRITE(outs,*) ' CFFVAR: function ',ICFP,' type ',ICLCTP
            call edisp(iuout,outs)
            call edisp(iuout,
     &        ' CFFVAR: invalid SSV for mass flow controller type')
            call epwait
            CALL EPAGEND
            STOP
          ENDIF
        ELSE
 
C In case the control concerns a component (possibly used
C in several connections). If icnn is zero then connection
C control is assumed and if NDSCNN(icnn,1) is zero then it
C would cause an array error to check so warn user.
          if(icnn.eq.0)then
            continue
          elseif(NDSCNN(icnn,1).eq.0)then
            WRITE(outs,*) ' CFFVAR: function ',ICFP,' type ',ICLCTP,
     &        ' connection ',icnn,' & sup node ',NDSCNN(ICNN,1),'...'
            call edisp(iuout,outs)
            WRITE(outs,*) ' control nb assoc conn ',nfsup(ICFP),
     &        ' ctl sup ',iasocc(ICFP,1),iasocc(ICFP,2),iasocc(ICFP,3)
            call edisp(iuout,outs)
            continue
          else
            if(ICLCTP.EQ.1)then
              SVCTL=TNOD(NDSCNN(ICNN,1))
            else IF(ICLCTP.EQ.5.OR.ICLCTP.EQ.29)then
              IDN1=NDSCNN(ICNN,1)
              IDN2=NDSCNN(ICNN,2)
              IDCN=0
   10         IDCN=IDCN+1
              IF(IDN1.EQ.NODPS(IDCN).AND.IDN2.EQ.NODNE(IDCN)) GOTO 11
              IF(IDCN.LT.NCNN) GOTO 10
              STOP 'CFFVAR: unresolved error on referenced connection'
   11         IF(ICLCTP.EQ.5)then
                SVCTL=sngl(flw1(IDCN)+flw2(IDCN))
              ELSE
                SVCTL=real(ABS(FLW1(IDCN)+FLW2(IDCN)))
              ENDIF
            elseif(ICLCTP.EQ.24)then
              SVCTL=TNOD(NDSCNN(ICNN,1))-TNOD(NDSCNN(ICNN,2))
            elseif(ICLCTP.EQ.25)then
              SVCTL=ABS(TNOD(NDSCNN(ICNN,1))-TNOD(NDSCNN(ICNN,2)))
            elseif(ICLCTP.EQ.26)then
              SVCTL=sngl(pres(NDSCNN(ICNN,1)))
            elseif(ICLCTP.EQ.27)then
              SVCTL=sngl(PRES(NDSCNN(ICNN,1))-PRES(NDSCNN(ICNN,2)))
            elseif(ICLCTP.EQ.28)then
              SVCTL=sngl(DABS(PRES(NDSCNN(ICNN,1))
     &                       -PRES(NDSCNN(ICNN,2))))
            elseif(ICLCTP.EQ.41)then
              SVCTL=CCONC(NDSCNN(ICNN,1),IFS3)*1000.
            else 
              WRITE(outs,*)' CFFVAR: function ',ICFP,' type ',ICLCTP
              call edisp(iuout,outs)
              call edisp(iuout,
     &         ' CFFVAR: invalid SSV for mass flow controller type')
              call epwait
              CALL EPAGEND
              STOP
            endif
          endif
        endif
      else
        call edisp(iuout,
     &    ' CFFVAR: illegal mass flow-side sensor referenced')
        call epwait
        CALL EPAGEND
        STOP
      ENDIF

      RETURN
      END

C *******  FCL00 ON-OFF controller
C << patched Dec 2005 for additional misc data item specifying
C << what comprises ON 
      SUBROUTINE FCL00

#include "net_flow.h"
#include "building.h"
#include "control.h"
      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      logical close

      SETPNT=fmiscd(ICFP,IDTYPP,IPERP,2)
      INVPOS=INT(fmiscd(ICFP,IDTYPP,IPERP,3))

C If there are two misc data items then ON is 100%, if there
C are more misc data items then get ONFRAC and use it.
      call eclose(fmiscd(ICFP,IDTYPP,IPERP,1),2.0,0.001,close)
      if(close)then
        ONFRAC=1.0
      else
        ONFRAC=fmiscd(ICFP,IDTYPP,IPERP,4)
      endif

      if(ifan(ICFP,1).eq.-3)then

C Controlling a connection. (ICNN not relevant, therefor 0)
        CALL CFFVAR(SIGNAL,0)
        IF(SIGNAL.GE.SETPNT)THEN
          CTLPOS(ifan(icfp,2))=ONFRAC

C Debug.
C          if(ONFRAC.lt.1.0) write(6,*) 'setting ON as ',ONFRAC

        ELSE
          CTLPOS(ifan(icfp,2))=0.0
        ENDIF
        IF(INVPOS.EQ.-1)CTLPOS(ifan(icfp,2))=1.0-CTLPOS(ifan(icfp,2))
      else

C Controlling a component. (ie ifan(ICFP,1).eq.-4)
        do 1 icnn=1,ncnn
C         if((ifan(icfp,2).eq.ITPCON(ICNN)).and.(NDSCNN(ICNN,1).gt.0))
          if((ifan(icfp,2).eq.ITPCON(ICNN)))
     &      then

C Establish sensed variable magnitude and actuated node location.
            CALL CFFVAR(SIGNAL,icnn)
            IF(SIGNAL.GE.SETPNT)THEN
              CTLPOS(icnn)=1.0
            ELSE
              CTLPOS(icnn)=0.0
            ENDIF
            IF(INVPOS.EQ.-1)CTLPOS(icnn)=1.0-CTLPOS(icnn)
          endif
  1     continue
      endif

      RETURN
      END
     
C *******  FCL01 Proportional controller with hysteresis
      SUBROUTINE FCL01

#include "net_flow.h"
#include "building.h"
#include "control.h"

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      if(ifan(ICFP,1).eq.-3)then

C Controlling a connection. (ICNN not relevant, therefor 0)
        CALL CFFVAR(SIGNAL,0)
        CTLPOS(ifan(icfp,2))=POSITN(SIGNAL)
      else

C Controlling a component
        do 1 icnn=1,ncnn
C         if((ifan(icfp,2).eq.ITPCON(ICNN)).and.(NDSCNN(ICNN,1).gt.0))
          if((ifan(icfp,2).eq.ITPCON(ICNN)))
     &      then

C Establish sensed variable magnitude and actuated node location
            CALL CFFVAR(SIGNAL,icnn)

C Establish control position
            ctlpos(icnn)=POSITN(SIGNAL)
          endif
  1     continue  
      endif
          
      RETURN
      END

C *******  FCL02 range (low/default/medium/high) controller for use
C with mass flow components 30 35 40 110. Returns ratio of the
C nominal rate or area associated with each range (0.0 is off,
C 1.0 is nominal rate or area, 1.5 would be 1.5 x nominal etc.).
      SUBROUTINE FCL02

#include "net_flow.h"
#include "building.h"
#include "control.h"

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      SETLOW=fmiscd(ICFP,IDTYPP,IPERP,2)
      SETMID=fmiscd(ICFP,IDTYPP,IPERP,3)
      SETHI=fmiscd(ICFP,IDTYPP,IPERP,4)
      VALLOW=fmiscd(ICFP,IDTYPP,IPERP,5)
      VALMID=fmiscd(ICFP,IDTYPP,IPERP,6)
      VALHI=fmiscd(ICFP,IDTYPP,IPERP,7)

      if(ifan(ICFP,1).eq.-3)then

C Controlling a connection.
        CALL CFFVAR(SIGNAL,0)
        if(signal.lt.SETLOW)then
          CTLPOS(ifan(icfp,2))= VALLOW
        elseif(signal.ge.SETLOW.and.signal.lt.SETMID)then
          CTLPOS(ifan(icfp,2))= 1.0
        elseif(signal.ge.SETMID.and.signal.lt.SETHI)then
          CTLPOS(ifan(icfp,2))= VALMID
        elseif(signal.ge.SETHI)then
          CTLPOS(ifan(icfp,2))= VALHI
        endif
      else

C Controlling a component.
        do 1 icnn=1,ncnn
          if((ifan(icfp,2).eq.ITPCON(ICNN)).and.(NDSCNN(ICNN,1).gt.0))
     &      then

C Establish sensed variable magnitude and actuated node location.
            CALL CFFVAR(SIGNAL,icnn)
            if(signal.lt.SETLOW)then
              CTLPOS(icnn)= VALLOW
            elseif(signal.ge.SETLOW.and.signal.lt.SETMID)then
              CTLPOS(icnn)= 1.0
            elseif(signal.ge.SETMID.and.signal.lt.SETHI)then
              CTLPOS(icnn)= VALMID
            elseif(signal.ge.SETHI)then
              CTLPOS(icnn)= VALHI
            endif

C Debug.
C            write(6,*) icnn,signal,ctlpos(icnn)

          endif
  1     continue
      endif

      RETURN
      END

C ***** POSITN
      FUNCTION POSITN(SIGNAL)
#include "net_flow.h"
#include "building.h"
#include "control.h"

C SIGL  - signal lower limit Sl (whatever is sensed by ctl loop)
C     PSIGL - relative valve position H/H100 at Sl (%)
C SIGU  - signal upper limit Su (whatever is sensed by ctl loop)
C     PSIGU - relative valve position H/H100 at Su (%)
C DSHY  - delta sensor value to overcome valve/damper hysteresis

      COMMON/MFCPOS/HHH100(MCNN),HSASC(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      logical close

      SIGL=fmiscd(ICFP,IDTYPP,IPERP,2)
      PSIGL=fmiscd(ICFP,IDTYPP,IPERP,3)/100.0
      SIGU=fmiscd(ICFP,IDTYPP,IPERP,4)
      PSIGU=fmiscd(ICFP,IDTYPP,IPERP,5)/100.0
      DSHY=fmiscd(ICFP,IDTYPP,IPERP,6)

C Calculate relative valve position, incorporate hysteresis effect.
      IF(SIGNAL.GE.(HSASC(ICFP)-DSHY).AND.SIGNAL.LE.HSASC(ICFP)) THEN
        HH100=HHH100(ICFP)
      ELSE IF(SIGNAL.LT.(HSASC(ICFP)-DSHY)) THEN
        IF(SIGNAL.LE.(SIGL-DSHY)) THEN
          HH100=PSIGL
        ELSE IF(SIGNAL.GE.(SIGU-DSHY)) THEN
          HH100=PSIGU
        ELSE
          HH100=PSIGL+(PSIGU-PSIGL)*(SIGNAL-(SIGL-DSHY))/(SIGU-SIGL)
        END IF
      ELSE
        IF(SIGNAL.LE.SIGL) THEN
          HH100=PSIGL
        ELSE IF(SIGNAL.GE.SIGU) THEN
          HH100=PSIGU
        ELSE
          HH100=PSIGL+(PSIGU-PSIGL)*(SIGNAL-SIGL)/(SIGU-SIGL)
        END IF
      END IF

C Refresh history for H/H100 and in case of change recalculate HSASC
      call eclose(HH100,HHH100(ICFP),0.001,close)
      IF(.NOT.close) THEN
         HHH100(ICFP)=HH100
         IF(HH100.GE.PSIGU) THEN
            HSASC(ICFP)=SIGU
         ELSE IF(HH100.LE.PSIGL) THEN
            HSASC(ICFP)=SIGL
         ELSE
            HSASC(ICFP)=SIGL+(SIGU-SIGL)*(HH100-PSIGL)/(PSIGU-PSIGL)
         END IF
      END IF

      POSITN=HH100

      RETURN
      END

C *******  FCL03 multi-sensor ON/OFF controller
 
C This controller will de/activate a connection/component based
C on the output of multiple sensors; i.e. starting from the
C default position (either open or closed), this controller will
C change (i.e. close or open) the connection/component according to 
C logical AND/OR as specified in the sensor definition
 
      subroutine FCL03
 
#include "net_flow.h"
#include "building.h"
#include "control.h"
 
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      common/MFCTL/CTLPOS(MCNN)
      common/FCLSOL/ICFP,IDTYPP,IPERP
      common/fcl0305/TSETPO

      REAL TSETPO
      CHARACTER ERRMSG*72
      LOGICAL CHANGE,PRESENT
 
C If the NOT inversion is operative invert the sense of this loop as
C compared with the sensed loop. Do nothing for the SLAVE operation
C aux. data item holds connection number for NOT control and holds
C -1.*connection number for SLAVE control.
      IF(NINT(FMISCD(ICFP,IDTYPP,IPERP,3)).EQ.1)THEN
        ICNN=NINT(FMISCD(ICFP,IDTYPP,IPERP,6))
        IF(ICNN.GT.0)THEN
          CHANGE=.TRUE.
        ELSEIF(ICNN.LT.0)THEN
          CHANGE=.FALSE.
        ENDIF
        ICNN=ABS(ICNN)
        DEF=CTLPOS(ICNN)
      ELSE

C Initialise
        DEF=FMISCD(ICFP,IDTYPP,IPERP,2)
        NSEN=int(FMISCD(ICFP,IDTYPP,IPERP,3))

C Debug ... Figure out defualt position of control
        CHANGE=.FALSE.

C Establish for each auxiliary sensor whether change is required.
        do 10 ISEN=1,NSEN
          PRESENT=.FALSE.
          IS1=NINT(FMISCD(ICFP,IDTYPP,IPERP,4+8*(ISEN-1)))
          IS2=NINT(FMISCD(ICFP,IDTYPP,IPERP,5+8*(ISEN-1)))
          IS3=NINT(FMISCD(ICFP,IDTYPP,IPERP,6+8*(ISEN-1)))
          IS4=NINT(FMISCD(ICFP,IDTYPP,IPERP,7+8*(ISEN-1)))
          SETPNT=FMISCD(ICFP,IDTYPP,IPERP,8+8*(ISEN-1))
          INVPOS=NINT(FMISCD(ICFP,IDTYPP,IPERP,9+8*(ISEN-1)))

C fraction 'ON' to be incorporated as future development
c        FRACT=FMISCD(ICFP,IDTYPP,IPERP,10+8*(ISEN-1))
 
C Note that the sensor location as defined in the IFSN
C array is not used by this multi sensor control law.
C Due to this it is allowed to overwrite the IFSN information
C with the details of each individual sensor before
C successive calls to CFFVAR. 
          IFSN(ICFP,1)=IS1
          IFSN(ICFP,2)=IS2
          IFSN(ICFP,3)=IS3
          IFSN(ICFP,4)=IS4

C Set control type depending upon IFSN
          IF(IS1.EQ.-3)THEN
            IFCTYP(ICFP,IDTYPP,IPERP)=0
          ELSEIF(IS1.EQ.-4)THEN

C Only one type of flow condition can be sensed (yet) i.e. temperature.
            IFCTYP(ICFP,IDTYPP,IPERP)=1
          ELSE
            CALL EDISP(IUOUT,
     &      'Multi-sensor not optimised for control law')
            WRITE(ERRMSG,'(A,I4,A)')'number ',ICFP,'; use another law'
            CALL EDISP(IUOUT,ERRMSG)
            call epwait
            CALL EPAGEND
            STOP
          ENDIF
          call CFFVAR(SIGNAL,0)

C Determine setpoint to which flow will be controlled (to be used by
C other control loops using multi sensor)
          IF(ISEN.EQ.1)TSETPO=SIGNAL
          if(INVPOS.eq.1.and.SIGNAL.gt.SETPNT)then
            if(tsetpo.lt.signal)tsetpo=signal
          endif
          if(INVPOS.eq.-1.and.SIGNAL.lt.SETPNT)then
            if(tsetpo.gt.signal)tsetpo=signal
          endif
          if((INVPOS.eq.1.and.SIGNAL.gt.SETPNT).or.
     &      INVPOS.eq.-1.and.SIGNAL.lt.SETPNT) PRESENT=.TRUE.
          IF(ISEN.EQ.1)THEN
            CHANGE=PRESENT
          ELSE
            IF(NLOG.EQ.1)THEN
              IF(PRESENT.AND.CHANGE)THEN
                CHANGE=.TRUE.
              ELSE
                CHANGE=.FALSE.
              ENDIF
            ELSEIF(NLOG.EQ.2)THEN
              IF(PRESENT.OR.CHANGE)THEN
                CHANGE=.TRUE.
              ELSE
                CHANGE=.FALSE.
              ENDIF
            ENDIF
          ENDIF

C Get logical AND or OR to use with next sensor
          NLOG=NINT(FMISCD(ICFP,IDTYPP,IPERP,11+8*(ISEN-1)))
   10   continue
      ENDIF
C Debug...
c      if(present)then
c      write(6,*)'present=true'
c      else
c      write(6,*)'present=false'
c      endif
c      if(CHANGE)then
c      write(6,*)'CHANGE=true'
c      else
c      write(6,*)'CHANGE=false'
c      endif
  
      if(IFAN(ICFP,1).eq.-3)then
 
C Controlling a connection.
        CTLPOS(IFAN(ICFP,2))=DEF
        if(CHANGE)CTLPOS(IFAN(ICFP,2))=1.0-CTLPOS(IFAN(ICFP,2))
      else
 
C Controlling a component.
        do 20 ICNN=1,NCNN
          if((IFAN(ICFP,2).eq.ITPCON(ICNN))) then
            CTLPOS(ICNN)=DEF
            if(CHANGE)CTLPOS(ICNN)=1.0-CTLPOS(ICNN)
          endif
 20     continue
      ENDIF
      return
      end

C *******  FCL04 Nicol Control algorithm

C This subroutine controls an airflow opening based on a statistical
C algorithm emulating occupant behaviour. The algorithm depends upon
C ambient temperature, operative temperature and comfort temperature.
C For further information refer to energy and buildings paper ....
C Get ambient temperature TOUT
C TODM   = outside daily mean temperature TODM
C TRM    = running mean outside air temperature TRM
C TCOMF  = comfort temperature TCOMF
C TOP    = operative temperature TOP
C PW     = probability function for window open PW
C COMF   = comfort COMF
C WINDOW = window status WINDOW
C WSIZE  = window opening size WSIZE

C Note: to allow periods to be set where the Nicol algorithm is set to 
C "window closed" e.g. periods without occupancy then this is achieved 
C by setting the ALPHA parameter = 0.

      subroutine FCL04
 
#include "net_flow.h"
#include "building.h"
#include "geometry.h"
#include "control.h"

C climate file commons
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/MFCTL/CTLPOS(MCNN)
      common/FCLSOL/ICFP,IDTYPP,IPERP

C Time and weather data.
C Commented common blocks to be uncommented and checked for relevance 
C when version 2 of the law will be published
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF

C Casual gain data.
C NCGPER - number of periods for each gain (zone, day type, gain type)
C TCGS - start time of gain (zone, day type, gain type, period)
C CGSENR - sensible gain radiant portion (zone, day type, gain type, period)
C CGSENC - sensible gain convective portion (zone, day type, gain type, period)
C CGLAT - latent gain (zone, day type, gain type, period)
C CGCTL - gain control fraction (zone, present/future, gain type)
C ICGCTL - pointer to controlled gain type.
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      common/btime/btimep,btimef

C Adaptive comfort common
      COMMON/ADPCOM/PREVSTATWIN,PREVSTATFAN,PREVSTATLIGHT,TOTPWR(MCOM)

C Day types 
      common/calena/calename,calentag(MDTY),calendayname(MDTY) 
      character calename*32,calentag*12,calendayname*32 
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365) 
      INTEGER nbdaytype,nbcaldays,icalender 
      
      DIMENSION ICLM(24,6)
      PARAMETER (MMD=20)
      REAL PREVTEMP(MMD)
      CHARACTER*72 LAPROB
      CHARACTER*2 MODE
      REAL Wposs,Fposs,WNposs,ACavail,Havail,PF,PWRfan,FUNCW
      LOGICAL CLOSER

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

C initialise variables
      helpinsub='FCL04'  ! set for subroutine
      WPOSS=0; Fposs=0; WNposs=0; ACavail=0; Havail=0; PF=0
      PWRfan=0; FUNCW=0

C Remember previous state of window and fan
      IWIN=INT(PREVSTATWIN)
      IFANON=INT(PREVSTATFAN)

C calculate outside daily mean temperature TODM and
      IYDS=IDYP-MMD
      IYDF=IDYP-1
      DO 3157 II=1,MMD
        PREVTEMP(II)=0.0
 3157 CONTINUE
      IF(IYDS.GT.0.AND.IYDF.GT.0)THEN
        ID=0
        DO 55 I=IYDS,IYDF
          ID=ID+1
          II=I
          IREC=II
          READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &           J=1,24)
          DO 656 J=1,24
            PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  656     CONTINUE
          PREVTEMP(ID)=PREVTEMP(ID)/24.
   55   CONTINUE
      ELSEIF(IYDS.LE.0)THEN
        IYDS=365+IYDS
        ID=0
        DO 550 I=IYDS,365
          ID=ID+1
          II=I
          IREC=II
          READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &           J=1,24)
          DO 657 J=1,24
            PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  657     CONTINUE
          PREVTEMP(ID)=PREVTEMP(ID)/24.
  550   CONTINUE
        IF(IDYF.GT.0)THEN        
          DO 551 I=1,IDYF
            ID=ID+1
            II=I
            IREC=II
            READ(IFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)((ICLM(J,K),K=1,6),
     &             J=1,24)
            DO 658 J=1,24
              PREVTEMP(ID)=REAL(ICLM(J,2))/10.+PREVTEMP(ID)
  658       CONTINUE
            PREVTEMP(ID)=PREVTEMP(ID)/24.
  551     CONTINUE
        ENDIF
      ENDIF

C Get ambient temperature TOUT
      TOUT=TFLWF

C Read data from control file
      ALPHA=fmiscd(ICFP,IDTYPP,IPERP,2)
      Wposs=fmiscd(ICFP,IDTYPP,IPERP,3)
      Fposs=fmiscd(ICFP,IDTYPP,IPERP,4)
      WNposs=fmiscd(ICFP,IDTYPP,IPERP,5)
      ACavail=fmiscd(ICFP,IDTYPP,IPERP,6)
      Havail=fmiscd(ICFP,IDTYPP,IPERP,7)
c      Dposs=fmiscd(ICFP,IDTYPP,IPERP,8)
      PWRfan=fmiscd(ICFP,IDTYPP,IPERP,9)
c      PWRlight=fmiscd(ICFP,IDTYPP,IPERP,10)
c      DTfan=fmiscd(ICFP,IDTYPP,IPERP,11)
      WD=fmiscd(ICFP,IDTYPP,IPERP,12)
      FD=fmiscd(ICFP,IDTYPP,IPERP,13)
c      AIsurf=fmiscd(ICFP,IDTYPP,IPERP,14)
      CWO=fmiscd(ICFP,IDTYPP,IPERP,15)
      CWC=fmiscd(ICFP,IDTYPP,IPERP,16)
      CFO=fmiscd(ICFP,IDTYPP,IPERP,17)
      CFC=fmiscd(ICFP,IDTYPP,IPERP,18)
      CNC=fmiscd(ICFP,IDTYPP,IPERP,19)
      
C Calculate running mean outside air temperature TRM
      COEFF=1.
      TRM=0.
      DO 245 I=1,MMD
        ISUB=I-1
        TRM=TRM+COEFF*PREVTEMP(MMD-ISUB)
        COEFF=COEFF*ALPHA
 245  CONTINUE
      TRM=(1.-ALPHA)*TRM

C Calculate operative (50% mean radiant, 50% dry bulb) temperature TOP for zone ICOMP
      ICOMP=0
      INOD=IFSN(ICFP,3)
      DO 255 IZN=1,NCOMP
        IF(ICAAS(IZN).EQ.INOD)ICOMP=IZN
 255  CONTINUE
      IF(ICOMP.LE.0)THEN
        CALL EDISP(IUOUT,
     &  'Node not specified for operative temperature using zone 1')
        ICOMP=1
      ENDIF

C Get operative temperature. This is calculated as average of mean 
C radiant and air point temperature. First get mean radiant temperature 
C with no weighting of air temperature. Note that TOP below may hold a
C weighted average for resultant temperature but that is model specifc
C based on the control file being used hence is ignored.
      CALL MZMIXT(ICOMP,TMRT,TOP)
      TOP=0.5*TMRT+0.5*TNOD(INOD)

C Calculate comfort temperature TCOMF (based on the summer and winter behaviours)
C These are algorithm version 1 equations and will be overwritten
C (retained for historic purposes)
        IF(TRM.GE.10.)TCOMF=0.33*TRM+18.8
        IF(TRM.LT.10.)TCOMF=0.09*TRM+22.6

C Mode determination (four modes)
C Mode = AC air conditioning
C Mode = NC night cooling
C Mode = HT heating
C Mode = FR free running (no heating or air conditioning, may have fan)
C Calculate mode and Tcomfort.
      IF(ACAVAIL.GT.0..AND.TRM.GT.28.1)THEN
        MODE='AC'
        TCOMF=0.416*TRM+16.5 ! 2nd version (Pakistan algorithm)
                             ! No 3rd version
      ELSEIF(Havail.GT.0..AND.TRM.LT.10.)THEN
        MODE='HT'
c        TCOMF=0.156*TRM+19.1  ! 2nd version (Pakistan algorithm
        TCOMF=0.090*TRM+22.6  ! 3rd version (Europe and UK)
      ELSE
        MODE='FR'
        IF(IFANON.EQ.0)THEN
c          TCOMF=0.408*TRM+16.6
          TCOMF=0.330*TRM+20.8  ! 3rd version
        ELSE
c          TCOMF=0.480*TRM+16.6
          TCOMF=0.330*TRM+18.8   ! 3rd version
        ENDIF
      ENDIF

C Determine if this timestep is the first occupied timestep
C Assume not occupied
      IOCCUPC=0         ! Current
      IOCCUPP=0         ! Previous
      ISTARTOCC=0

C Determine year day number of present day
      IDAY=IYD
      IDAY=ICALENDER(IYD)

C Calculate the current and previous period id.
      IGN=1 ! Occupancy
      IPERC=NCGPER(ICOMP,IDAY,IGN)
      IPERP=NCGPER(ICOMP,IDAY,IGN)
      do 30 I=1,NCGPER(ICOMP,IDAY,IGN)
        if (BTIMEF.le.TCGS(ICOMP,IDAY,IGN,I+1).and.
     &      BTIMEF.gt.TCGS(ICOMP,IDAY,IGN,I)) then
          IPERC=I
        endif
        if (BTIMEP.le.TCGS(ICOMP,IDAY,IGN,I+1).and.
     &      BTIMEP.gt.TCGS(ICOMP,IDAY,IGN,I)) then
          IPERP=I
        endif
 30   continue

C Determine if current and previous indicate start of occupancy
      IF(IPERC.GT.0)THEN
        IF(CGSENC(ICOMP,IDAY,IGN,IPERC).GT.0.)IOCCUPC=1
      ENDIF
      IF(IPERP.GT.0)THEN
        IF(CGSENC(ICOMP,IDAY,IGN,IPERP).GT.0.)IOCCUPP=1
      ENDIF

      IF(IOCCUPP.EQ.0.AND.IOCCUPC.EQ.1)ISTARTOCC=1

C Determine initial status of window and fan at start of occupancy
      IF(ISTARTOCC.EQ.1)THEN
        IF(WPOSS.GT.0.)THEN
          FUNCW=0.448*TOP-10.3
          PW=EXP(FUNCW)/(1.+EXP(FUNCW))
          RNW=RNOR()
          RNW=ABS(RNW)
          RNW=1000.*RNW
          IRNW=INT(RNW)
          RNW=RNW-REAL(IRNW)
          IF(PW.GT.RNW)THEN
            IWIN=1
          ELSE
            IWIN=0
          ENDIF
        ELSE
          IWIN=0
        ENDIF
        IF(MODE.EQ.'FR'.AND.Fposs.GT.0.)THEN
          FUNCF=0.374*TOP-10.6
          PF=EXP(FUNCF)/(1.+EXP(FUNCF))
          RNF=RNOR()
          RNF=ABS(RNF)
          RNF=1000.*RNF
          IRNF=INT(RNF)
          RNF=RNF-REAL(IRNF)
          IF(PF.GT.RNF)THEN
            IFANON=1
          ELSE
            IFANON=0
          ENDIF
        ELSEIF(MODE.EQ.'AC'.AND.Fposs.GT.0.)THEN
          IFANON=1
        ELSE
          IFANON=0
        ENDIF

C Determine status of window and fan at other occupied times
      ELSE

C Window status determination. If Night cooling then window
C open if Trm > 28.1, WNposs should only be set to 1 outside of the 
C occupied hours!! If window opening not effective for cooling then
C window is closed - this is currently defined as when Tout>(Top+4).
C The windows will be forced shut if alpha is set to 0.   
        IF(MODE.EQ.'HT'.OR.MODE.EQ.'FR')THEN
          CLOSER=.FALSE.
          CALL ECLOSE(PREVSTATWIN,0.0,0.0001,CLOSER)
          IF(CLOSER)THEN   !  If window closed previously
            FUNCWO=0.8*(TOP-TCOMF-CWO)
            PWO=EXP(FUNCWO)/(1.+EXP(FUNCWO))
            RNWO=RNOR()
            RNWO=ABS(RNWO)
            RNWO=1000.*RNWO   
            IRNWO=INT(RNWO)
            RNWO=RNWO-REAL(IRNWO)
            PW=PWO ! For reporting in scratch file
            FUNCW=FUNCWO ! For reporting in scratch file
            RNW=RNWO ! For reporting in scratch file
            IF(PWO.GT.RNWO)THEN
              IWIN=1
            ELSE
              IWIN=0
            ENDIF
          ELSE  ! If window was open previously
            FUNCWC=0.8*(TOP-TCOMF+CWC)
            PWC=EXP(FUNCWC)/(1.+EXP(FUNCWC))
            RNWC=RNOR()
            RNWC=ABS(RNWC)
            RNWC=1000.*RNWC
            IRNWC=INT(RNWC)
            RNWC=RNWC-REAL(IRNWC)
            PW=PWC ! For reporting in scratch file
            FUNCW=FUNCWC ! For reporting in scratch file
            RNW=RNWC ! For reporting in scratch file
            IF(PWC.GT.RNWC)THEN
              IWIN=0
            ELSE
              IWIN=1
            ENDIF
          ENDIF
        ENDIF

C Fan status determination. Note that fans are assumed on
C in AC mode. If Tout < 23.6 then fans are assumed to be off.
      IF(MODE.EQ.'FR'.AND.Fposs.GT.0.)THEN
        CLOSER=.FALSE.
        CALL ECLOSE(PREVSTATFAN,0.0,0.0001,CLOSER)
        IF(CLOSER)THEN   !  If fan off previously
            FUNCFO=0.8*(TOP-TCOMF-CFO)
            PFO=EXP(FUNCFO)/(1.+EXP(FUNCFO))
            RNFO=RNOR()
            RNFO=ABS(RNFO)
            RNFO=1000.*RNFO
            IRNFO=INT(RNFO)
            RNFO=RNFO-REAL(IRNFO)
            PF=PFO ! For reporting in scratch file
            FUNCF=FUNCFO ! For reporting in scratch file
            RNF=RNFO ! For reporting in scratch file
            IF(PFO.GT.RNFO)THEN
              IFANON=1
            ELSE
              IFANON=0
            ENDIF
          ELSE  ! If fan on previously
            FUNCFC=0.8*(TOP-TCOMF+CFC)
            PFC=EXP(FUNCFC)/(1.+EXP(FUNCFC))
            RNFC=RNOR()
            RNFC=ABS(RNFC)
            RNFC=1000.*RNFC
            IRNFC=INT(RNFC)
            RNFC=RNFC-REAL(IRNFC)
            PF=PFC ! For reporting in scratch file
            FUNCF=FUNCFC ! For reporting in scratch file
            RNF=RNFC ! For reporting in scratch file
            IF(PFC.GT.RNFC)THEN
              IFANON=0
            ELSE
              IFANON=1
            ENDIF
          ENDIF
        ENDIF

C Force action based on TRM and other parameters
        IF(TOUT.GT.(TOP+4.).AND.TRM.GT.28.1) IWIN=0
        IF(WNposs.GT.0.1.AND.TRM.GT.28.1) IWIN=1     
        IF(MODE.EQ.'AC')IWIN=0   

C Force action based on alpha = 0
        closer=.false.
        call eclose(alpha,0.0,0.01,closer)
        IF(closer)then
          IWIN=0   
          IFANON=0
        endif

C Force action based on alpha = 1
        closer=.false.
        call eclose(alpha,1.0,0.01,closer)
        IF(closer)then
          IWIN=1
          IFANON=1
        endif

C Force action based on alpha = 0.5
        closer=.false.
        call eclose(alpha,0.5,0.01,closer)
        IF(closer)then
          IWIN=1
          IFANON=0
        endif

C Force action based on alpha = 0.3
        closer=.false.
        call eclose(alpha,0.5,0.01,closer)
        IF(closer)then
          IWIN=0
          IFANON=1
        endif
      ENDIF

C Window operation version 2
C        IF(Wposs.GT.0.)THEN
C          FUNCW=0.525*(TOP-TCOMF+(IWIN-0.5)*WD)
C          PW=EXP(FUNCW)/(1.+EXP(FUNCW))
C          RNW=RNOR()
C          RNW=ABS(RNW)
C          RNW=1000.*RNW   
C          IRNW=INT(RNW)
C          RNW=RNW-REAL(IRNW)
C          IF(PW.GT.RNW)THEN
C            IWIN=1
C          ELSE
C            IWIN=0
C          ENDIF
C        ELSE
C          IWIN=0
C        ENDIF

C Fan operation version 2
C        IF(MODE.EQ.'FR'.AND.Fposs.GT.0.)THEN
C          FUNCF=0.595*(TOP-TCOMF+(IFANON-0.5)*FD)
C          PF=EXP(FUNCF)/(1.+EXP(FUNCF))
C          RNF=RNOR()
C          RNF=ABS(RNF)
C          RNF=1000.*RNF
C          IRNF=INT(RNF)
C          RNF=RNF-REAL(IRNF)
C          IF(PF.GT.RNF)THEN
C            IFANON=1
C          ELSE
C            IFANON=0
C          ENDIF
C        ELSEIF(MODE.EQ.'AC'.AND.Fposs.GT.0.)THEN
C          IFANON=1
C        ELSE
C          IFANON=0
C        ENDIF
C         IF(TRM.LT.23.6)IFANON=0

C Door opening (Aizaz to enable functionality)
C Currently door opening modelled manually by changing the flow
C network component crack (closed) to door (open).
C     IF(Dposs.GT.0.)THEN
C       FUNCD=0.081*TOP-1.34
C       PD=EXP(FUNCD)/(1.+EXP(FUNCD))
C       RNW=RNOR()
C       RNW=ABS(RNW)
C       RNW=1000.*RNW   
C       IRNW=INT(RNW)
C       RNW=RNW-REAL(IRNW)
C       IF(PD.GT.RNW)THEN
C         IDOOR=1
C       ELSE
C         IDOOR=0
C       ENDIF
C     ELSE
C       IDOOR=0
C     ENDIF

C In future - calculate window opening size fraction ONFRAC.
C Currently set to 0 or 1 (100% of open area in flow network)
C may vary with season or deltaT(in-out), no data at present.

C Store values for window, fan, door status (Aizaz to enable Door)
      PREVSTATFAN=REAL(IFANON)
      PREVSTATWIN=REAL(IWIN)
      ONFRAC=REAL(IWIN) 
C     PREVSTATDOOR=REAL(IDOOR)
     
C AIRFLOW NETWORK CONTROL CODE HERE:     
      if(ifan(ICFP,1).eq.-3)then
C Controlling a connection. (ICNN not relevant, therefor 0)
          CTLPOS(ifan(icfp,2))=ONFRAC
      elseif(ifan(ICFP,1).eq.-4)then
C Controlling a component. 
        do 1 icnn=1,ncnn
          if((ifan(icfp,2).eq.ITPCON(ICNN)))then
C Establish sensed variable magnitude and actuated node location.
              CTLPOS(icnn)=ONFRAC
          endif
  1     continue
      endif
     
C fort.96 file write statement here:   
      IF(NSINC.EQ.1.)write(96,*)
c     &'IDAY btimef Top TMRT Tout Trm  Tcomf  FUNCF PF  RNF  FAN FUNCw Pw
c     & RNW  WIN M'
c      write(96,'(i3,1x,F5.2,1x,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,1X,F4.1,
c     & 1X,F5.1,1X,F4.1,
c     & 1X,F4.1,1X,I2,1X,F5.1,1X,F4.1,1X,F4.1,1X,I2,1X,A)')
c     & IDAY,BTIMEF,Top,TMRT,Tout,Trm,Tcomf,FUNCF,PF,RNF,
c     & IFANON,FUNCw,Pw,RNW,IWIN,MODE

     &'IDAY,btimef,Top,TMRT,Tout,Trm,Tcomf,FUNCF,PF,RNF,FAN,FUNCw,Pw,RNW
     &,WIN,M'
      write(96,*)
     & IDAY,',',BTIMEF,',',Top,',',TMRT,',',Tout,',',Trm,',',Tcomf,',',
     & FUNCF,',',PF,',',RNF,',',
     & IFANON,',',FUNCw,',',Pw,',',RNW,',',IWIN,',',MODE,',',ICOMP
     
C node number is ifsn(icfp,3), get zone associated with node
      DO 546 ICOM=1,NCOMP
        IF(IFSN(ICFP,3).EQ.ICAAS(ICOM))THEN
          ICOMP=ICOM
          TOTPWR(ICOMP)=0.
        ENDIF
  546 CONTINUE

C Gains adding to thermal zone(s). Lights not yet enabled
C If fan is on add fan power to casual gains for that zone
      IF(IFANON.EQ.1)THEN
        FNPWR=ZBASEA(ICOMP)*PWRfan
      ENDIF
C If light are on add light power to casual gains for that zone
c      IF(ILIGHTON.EQ.1)THEN
c        GLNPWR=ZBASEA(ICOMP)*PWRlight
c      ENDIF
       GLNPWR=0.
C Find total power of the equipment
      TOTPWR(ICOMP)=FNPWR+GLNPWR

      GOTO 9999

C Error message for this controller.
  999 helptopic='flow_ctl_04_error'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('node connection selection',nbhelp,
     &            'i/o exception handler',0,0,IER)

 9999 RETURN
      END

C *******  FCL05 PI controller
C Returns ratio of the nominal rate or area associated with 
C each component/connection that is to be controlled
C 1.0 is nominal rate or area, 1.5 would be 1.5 x nominal etc.).
      SUBROUTINE FCL05

#include "net_flow.h"
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/BTIME/BTIMEP,BTIMEF
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/fclp05/times,deltai,icfpp
      common/fcl0305/TSETPO
      LOGICAL CLOSER,close1,close2
      REAL TRSPT,QMAX,QMIN,PROPBAND,ACTI,TRS,SIGNAL,DELTA,DELTAI,
     &HACTI,PROPTERM,VALUE,BTIMES,times,TSETPO
      INTEGER IOPTL,ICFPR,icfpp

      TRSPT=FMISCD(ICFP,IDTYPP,IPERP,2)
      QMAX=FMISCD(ICFP,IDTYPP,IPERP,3)
      QMIN=FMISCD(ICFP,IDTYPP,IPERP,4)
      PROPBAND=FMISCD(ICFP,IDTYPP,IPERP,5)
      ACTI=FMISCD(ICFP,IDTYPP,IPERP,6)
      IAFL=NINT(FMISCD(ICFP,IDTYPP,IPERP,7))
      IOPTL=NINT(FMISCD(ICFP,IDTYPP,IPERP,8))
      IF(IOPTL.GT.0)THEN
        ICFPR=ICFP
        ICFP=IOPTL
        CALL FCL03
        ICFP=ICFPR
        TRS=TSETPO
      ELSE
        CALL CFFVAR(SIGNAL,0)
        TRS=SIGNAL
      ENDIF
      DELTA=TRSPT-TRS
      BTIMES=3600./REAL(NTSTEP)
      IF(BTIMES.GE.ACTI)THEN
        CALL EDISP(IUOUT,'PCL05: Warning ')
        CALL EDISP(IUOUT,
     &  ' Building timestep is of the order of integral action')
        CALL EDISP(IUOUT,' control action may be compensated')
      ENDIF

C Implement integral action (once per time step, this subroutine may be
C called more than once in a time step)
      call eclose(times,Btimef,0.001,closer)
      if(closer.and.icfpp.eq.icfp)goto 9595
      icfpp=icfp
      DELTAI=DELTAI+DELTA*BTIMES
      times=btimef

C Reset integral action to avoid integral windup (large integral action)
      HACTI=0.5*ACTI
      PROPTERM=DELTA/PROPBAND
      IF(DELTAI.GT.HACTI*PROPBAND)DELTAI=HACTI*PROPBAND
      IF(DELTAI.LT.-1.*HACTI*PROPBAND)DELTAI=-1.*HACTI*PROPBAND

C Calculate value
      IF(PROPTERM.GT.0.5.OR.PROPTERM.LT.-0.5)DELTAI=0.
      call eclose(propband,0.,0.001,close1)
      call eclose(acti,0.,0.001,close2)
      IF(close1.OR.close2)THEN
        VALUE=QMAX
      ELSE  
        VALUE=(QMAX-QMIN)*(DELTA/PROPBAND+DELTAI/(ACTI*PROPBAND))
     &       +0.5*(QMAX+QMIN)
      ENDIF
      IF(IAFL.EQ.-1)value=1.-value
      IF(VALUE.GT.QMAX)THEN
        VALUE=QMAX
      ELSEIF(VALUE.LT.QMIN)THEN
        VALUE=QMIN
      ENDIF

C Debug.
C      write(6,*)trs,trspt,value

C Controlling a connection.
      if(ifan(ICFP,1).eq.-3)then
          CTLPOS(ifan(icfp,2))=VALUE

C Controlling a component.
      else
        do 1 icnn=1,ncnn
          if(ifan(icfp,2).eq.ITPCON(ICNN))CTLPOS(icnn)=VALUE
  1     continue
      endif
 9595 RETURN
      END

C *******  FCL06 High-Low controller with hysteresis
C Copyright: GaE - July 2010
C
C       ^
C PSIGU ¦           ¦-------<------¦--<->------
C       ¦           ¦              ¦
C       ¦           v              ¦
C       ¦           ¦              ^
C       ¦           ¦              ¦
C PSIGL ----->--<---¦------->------¦-----
C                SIGU-DSHY        SIGU
C
      SUBROUTINE FCL06
      implicit none
#include "net_flow.h"
#include "building.h"
#include "control.h"

      COMMON/mfctl/ctlpos(MCNN)
      real ctlpos
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      integer icfp,idtypp,iperp
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      integer nnod,ncmp,ncnn
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      integer nodps,nodne,itpcon,ndscnn
      real hgtps,hgtne

C local variables
      integer icnn,iControlled
      real SIGNAL

C OnOffHist is a function returning the controller position.
      real OnOffHist

      iControlled=ifan(icfp,2) ! controlled item

      if(ifan(ICFP,1).eq.-3)then

C Controlling a connection. (ICNN not relevant, therefor 0)
        CALL CFFVAR(SIGNAL,0)
        CTLPOS(iControlled)=OnOffHist(SIGNAL)
      else

C Controlling a component
        do 1 icnn=1,ncnn
          if(ITPCON(ICNN).eq.iControlled)
     &      then

C Establish sensed variable magnitude and actuated node location
            CALL CFFVAR(SIGNAL,icnn)

C Establish control position
            ctlpos(iControlled)=OnOffHist(SIGNAL)
          endif
  1     continue  
      endif
          
      RETURN
      END

C ***** OnOffHist
C Signal position logic for High/Low control with hysteresis
C Copyright: GaE - July 2010
C
C SIGL  - signal lower limit Sl (whatever is sensed by ctl loop)
C PSIGL - relative valve position H/H100 at Sl (%)
C SIGU  - signal upper limit Su (whatever is sensed by ctl loop)
C PSIGU - relative valve position H/H100 at Su (%)
C DSHY  - delta sensor value to overcome valve/damper hysteresis
      FUNCTION OnOffHist(SIGNAL)
      implicit none
#include "net_flow.h"
#include "building.h"
#include "control.h"

      real OnOffHist,SIGNAL

      COMMON/MFCPOS/HHH100(MCNN),HSASC(MCNN)
      real HHH100,HSASC
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      integer ICFP,IDTYPP,IPERP

      logical close

C Local variables
      real SIGL,PSIGL,SIGU,PSIGU,DSHY
      real currentPosition,newPosition

C Read controller parameters
      SIGL=fmiscd(ICFP,IDTYPP,IPERP,2)
      PSIGL=fmiscd(ICFP,IDTYPP,IPERP,3)/100.0
      SIGU=fmiscd(ICFP,IDTYPP,IPERP,4)
      PSIGU=fmiscd(ICFP,IDTYPP,IPERP,5)/100.0
      DSHY=fmiscd(ICFP,IDTYPP,IPERP,6)

      currentPosition=HHH100(ICFP)

C Change of controller postion between high and low settings depends
C on current position, current signal and size of hysteresis
      call eclose(currentPosition,PSIGU,0.0001,close)
      if (close) then
	  ! controller is currently "High"
        if (SIGNAL.ge.(SIGU-DSHY)) then
        ! signal is in hysteresis bracketor above high - don't change
          newPosition=currentPosition
        elseif (SIGNAL.lt.(SIGU-DSHY)) then
        ! signal is under high minus hyst - turn "Low"
          newPosition=PSIGL
        endif
      else ! currentPostion=PSIGL
        if (SIGNAL.ge.SIGU) then
        ! signal is above high point, turn on!
          newPosition=PSIGU
        else
        ! signal is below high point, don't change
          newPosition=currentPosition
        endif
      endif

C Refresh history for H/H100 and in case of change recalculate HSASC
      HHH100(ICFP)=newPosition

C Pass value to calling routine
      OnOffHist=newPosition

      RETURN
      END
