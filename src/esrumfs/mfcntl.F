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

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      COMMON/mfctl/ctlpos(MCNN)
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      character outs*124
      character*10 wkd1, wkd2

      do 102 i=1,MCNN
 102    ctlpos(i)=1.0

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

C If NDAYT=0 set data to weekday, saturday and sunday format
      IF(NDAYT.EQ.0) THEN
         IDTYPP=1
         IF(IDW.EQ.idwe1) IDTYPP=2
         IF(IDW.EQ.idwe2) IDTYPP=3
      ELSE
         DO 10 IDTYPP=1,NDAYT
           IDS=IFCDV(ICFP,IDTYPP,1)
           IDF=IFCDV(ICFP,IDTYPP,2)
           IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10    CONTINUE
         WRITE(outs,*) ' MFCNTR: mass flow control connection ',ICFP
         call edisp(iuout,outs)
         WRITE(outs,*) '         no valid day type for year-day ',IDAY
         call edisp(iuout,outs)
         call edisp(iuout,
     &          ' MFCNTR: cannot locate appropriate day type')
         call epwait
         CALL EPAGEND
         STOP
      END IF

C Check number of periods in each day and the start and finish times
   20 NDAYP=NFCDP(ICFP,IDTYPP)
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
      call edisp(iuout,' MFCNTR: cannot locate appropriate day-period')
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

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CLIMIF/QFLWP,QFLWF,TFLWP,TFLWF,QDFLP,QDFLF,VFLP,VFLF,
     &             DFLP,DFLF,HFLP,HFLF
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
      common/fctl4/iasocc(mcc,MCMP),nfsup(mcc)
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

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      character outs*124

C Establish sensor location
      IFS1=IFSN(ICFP,1)
      IFS2=IFSN(ICFP,2)
      IFS3=IFSN(ICFP,3)

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
        if(ICLCTP.gt.0.and.ICLCTP.le.6)then
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
        if((ICLCTP.NE.1.AND.ICLCTP.NE.3.AND.ICLCTP.NE.5.AND.ICLCTP.NE.7
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
C Sensor measures building zone related temperature
      IF(IFS1.GT.0) THEN
        SVCTL=TFA(IFS1)
        IF(IFS2.GT.0.AND.IFS3.EQ.0) SVCTL=TFS(IFS1,IFS2)
        IF(IFS2.GT.0.AND.IFS3.GT.0) SVCTL=TFC(IFS1,IFS2,IFS3)

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
              SVCTL=CCONC(NDSCNN(ICNN,1),IFS3)*1000
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

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
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

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
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

      COMMON/mfctl/ctlpos(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
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
            write(6,*) icnn,signal,ctlpos(icnn)
          endif
  1     continue
      endif

      RETURN
      END

C ***** POSITN
      FUNCTION POSITN(SIGNAL)
#include "net_flow.h"
#include "building.h"

C     SIGL  - signal lower limit Sl (?)
C     PSIGL - relative valve position H/H100 at Sl (%)
C     SIGU  - signal upper limit Su (?)
C     PSIGU - relative valve position H/H100 at Su (%)
C     DSHY  - dS to overcome valve/damper hysteresis (?)

      COMMON/MFCPOS/HHH100(MCNN),HSASC(MCNN)
      COMMON/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)
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
C     ELSE IF(SIGNAL.GT.HSASC(ICFP)) THEN
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
C      IF(HH100.NE.HHH100(ICFP)) THEN
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
 
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      common/MFCTL/CTLPOS(MCNN)
      common/FCLSOL/ICFP,IDTYPP,IPERP
      common/fctl/ncc,ifsn(mcc,4),ifan(mcc,3),nfcdt(mcc),
     &       ifcdv(mcc,mfcdt,2),nfcdp(mcc,mfcdt),tfcps(mcc,mfcdt,mfcdp),
     &       ifctyp(mcc,mfcdt,mfcdp),ifclaw(mcc,mfcdt,mfcdp),
     &       fmiscd(mcc,mfcdt,mfcdp,misc)

      CHARACTER ERRMSG*72
      LOGICAL CHANGE,PRESENT
 
C If the NOT inversion is operative invert the sense of this loop as
C compared with the sensed loop
      IF(NINT(FMISCD(ICFP,IDTYPP,IPERP,3)).EQ.1)THEN
        ICNN=NINT(FMISCD(ICFP,IDTYPP,IPERP,6))
        DEF=CTLPOS(ICNN)
        CHANGE=.TRUE.
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
