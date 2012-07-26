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

C ******************** MZPADJ ********************
C MZPADJ handles plant component connections. It has several functions:

C 1. It organises the information which defines each inter-component
C    coupling. This data is required by the component nodal equation
C    coefficient generators in order to calculate the inter-component
C    connection coefficients.
C    The data is held in four variables; the first, ICONDX, acts as
C    a pointer to the others: CONVAR which holds the actual data,
C    ICONTP which defines the state variable type of the sending
C    node (= ISV), and PCONDR which holds mass diversion info; thus:
C      ICONDX defines - for any component, node and coupling (there
C             may be more than one) - the connection number. Because
C             this only needs to be done once, MZPMXT sets up ICONDX
C      CONVAR holds, for each user defined plant component inter-
C             connection, the following data:
C             1  connection 'sending node' fluid temperature
C             2  connection 'sending node' first phase mass flowrate
C             3  connection 'sending node' second phase mass flowrate
C      ICONTP defines the state variable type of the 'sending node', in
C             order to be able to calculate the fluid specific heat,
C             fluid enthalpy etc. Because this also only needs to be
C             done once, ICONTP is set up by MZPMXT
C      PCONDR holds (irrespective of connection type) the ratio between
C             the mass flow rate through the connection and the mass
C             flow rate entering the receiving node. PCONDR is defined
C             by the user, but is overruled (by MZPMXT) if mfs active
 
C 2. This routine checks whether mass flows are in the assumed
C    direction; ie. the coefficient generators implicitly assume that
C    each connection's mass flow rate >= 0
 
C 3. The routine also establishes plant component containment future
C    temperatures, PCNTMF, if defined to exist
 
      SUBROUTINE MZPADJ(ISTATS)
#include "plant.h"
#include "building.h"
 
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
 
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
 
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALG/GFA(MCOM)
 
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP
 
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
 
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
 
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      logical close,closea
C Contextual buffer
      character cContext*124
      character outs*124  ! for warning messages

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         CALL DAYCLK(IDYP,PTIMEF,ITU)
         WRITE(ITU,*) ' Entering subroutine MZPADJ'
      END IF
      
C Adjust CONVAR depending on component inter-connection type
      IF(NPCON.EQ.0) GOTO 20
      DO 10 IPCON=1,NPCON
        I1=IPC1(IPCON)
        I2=IPN1(IPCON)
        I4=IPC2(IPCON)
        I5=IPN2(IPCON)
        IN1=NPCDAT(I1,9)+I2-1
        IN2=NPCDAT(I4,9)+I5-1

C The 1th phase mass flow rate of types 1,2 and 4 is made equal
C to the 1th phase mass flow rate in the referenced component node
C When mfs active the 1th phase mass flow rate is not adjusted
 
C Type 1 connection: to identical (ie temp. and humidity ratio) node
C Note that at start of simulation csvf(in1,3) is zero so
C use csvf(in2,3) as a first estimate.
        IF(IPCT(IPCON).EQ.1) THEN
          CONVAR(IPCON,1)=CSVF(IN1,1)
          IF(IFLWN.EQ.0) CONVAR(IPCON,2)=CSVF(IN2,2)
          call eclose(CSVF(IN1,2),0.00,0.0001,close)
          call eclose(CSVF(IN1,3),0.00,0.0001,closea)
          IF(.NOT.close) THEN
C          IF(CSVF(IN1,2).NE.0.) THEN
             if(closea) then
C              if(csvf(in1,3).eq.0.0) then
                convar(ipcon,3)=csvf(in2,3)
             else
               CONVAR(IPCON,3)=CONVAR(IPCON,2)*CSVF(IN1,3)/CSVF(IN1,2)
             endif
          ELSE
            CONVAR(IPCON,3)=0.
          END IF

C Hydrogen flow 
          CONVAR(IPCON,4) = 0.

C Type 2 connection: to known fluid temperature and humidity ratio
        ELSE IF(IPCT(IPCON).EQ.2) THEN
          CONVAR(IPCON,1)=PCONSD(IPCON,1)
          IF(IFLWN.EQ.0) CONVAR(IPCON,2)=CSVF(IN2,2)
          CONVAR(IPCON,3)=CONVAR(IPCON,2)*PCONSD(IPCON,2)
          CONVAR(IPCON,4) = 0.  ! Hydrogen flow 

C Type 3 connection: to another component node
        ELSE IF(IPCT(IPCON).EQ.3) THEN
          CONVAR(IPCON,1)=CSVF(IN2,1)
          IF(IFLWN.EQ.0) CONVAR(IPCON,2)=CSVF(IN2,2)
          CONVAR(IPCON,3)=CSVF(IN2,3)
          CONVAR(IPCON,4) = CSVF(IN2,4)  ! Hydrogen flow 

C Type 4 connection: to building zone air (or ambient air when zone=0)
        ELSE IF(IPCT(IPCON).EQ.4) THEN
          IZ=NINT(PCONSD(IPCON,1))
          IF(IZ.EQ.0) THEN
            CONVAR(IPCON,1)=TFP
            CONVAR(IPCON,3)=GEXTFP
          ELSE
            CONVAR(IPCON,1)=TFA(IZ)
            CONVAR(IPCON,3)=GFA(IZ)
          END IF
          IF(IFLWN.EQ.0) CONVAR(IPCON,2)=CSVF(IN2,2)
          CONVAR(IPCON,3)=CONVAR(IPCON,2)*CONVAR(IPCON,3)
          CONVAR(IPCON,4)=0.  ! Hydrogen flow

C New connector types go here!
        ELSE
          write(*,*)'************ invalid connection',IPCT(IPCON)
          STOP ' MZPADJ: unresolvable error: invalid connection type'
        END IF

C Now check direction of flow
        IF(CONVAR(IPCON,2).LT.0..OR.CONVAR(IPCON,3).LT.0.) THEN
          WRITE(outs,*) ' MZPADJ: reverse mass flow connection ',IPCON
          call edisp(iuout,outs)
          CALL DAYCLK(IDYP,PTIMEF,ITU)
          IF(CONVAR(IPCON,2).LT.0.) THEN
            WRITE(outs,*) ' MZPADJ: 1th phase mass flow reset to zero'
            call edisp(iuout,outs)
            CONVAR(IPCON,2)=0.
          END IF
          IF(CONVAR(IPCON,3).LT.0.) THEN
            WRITE(outs,*) ' MZPADJ: 2nd phase mass flow reset to zero'
            call edisp(iuout,outs)
            CONVAR(IPCON,3)=0.
          END IF
        END IF
 
   10 CONTINUE
 
C Now set up containment conditions for each plant component, where
C defined to exist. First initialise PCNTMF to -99; this gives the
C default treatment of no heat transfer with the environment
   20 DO 22 IPCOMP=1,NPCOMP
        PCNTMF(IPCOMP)=-99.
   22 CONTINUE
 
      IF(NCONT.EQ.0) GOTO 900
      DO 29 ICONT=1,NCONT
        IPC=IPCC(ICONT)

C Type 0 containment: ambient air temperature + specified (de)increment
        IF(INDCP(ICONT).EQ.0) THEN
          PCNTMF(IPC)=TFP+CNTDAT(ICONT,1)

C Type 1 containment: containment is at temperature of a specified plant
C node - including the self containment case if CNTDAT(ICONT,1)=0 - plus
C specified (de)increment (CNTDAT(ICONT,3)).
        ELSE IF(INDCP(ICONT).EQ.1) THEN
          ICM=INT(CNTDAT(ICONT,1))
          IF(ICM.EQ.0)ICM=IPC
          INO=INT(CNTDAT(ICONT,2))

C If no node defined (CNTDAT(ICONT,2)=0), use node 1 of the component
          IF(INO.EQ.0)INO=1

C Determine unique node location and assign temperature
          ICC=NPCDAT(ICM,9)+INO-1
          PCNTMF(IPC)=CSVF(ICC,1)+CNTDAT(ICONT,3)

C Type 2 containment: specified temperature CNTDAT(ICONT,1).
        ELSE IF(INDCP(ICONT).EQ.2) THEN
          PCNTMF(IPC)=CNTDAT(ICONT,1)

C Type 3 containment: zone CNTDAT(ICONT,1), surface CNTDAT(ICONT,2)
C and construction node CNTDAT(ICONT,3) counted from outside
        ELSE IF(INDCP(ICONT).EQ.3) THEN
          IZ=INT(CNTDAT(ICONT,1))
          IS=INT(CNTDAT(ICONT,2))
          IN=INT(CNTDAT(ICONT,3))
          IF(IZ.GT.0.AND.IS.EQ.0.AND.IN.EQ.0) THEN
            PCNTMF(IPC)=TFA(IZ)
          ELSE IF(IZ.GT.0.AND.IS.GT.0.AND.IN.EQ.0) THEN
            PCNTMF(IPC)=TFS(IZ,IS)
          ELSE IF(IZ.GT.0.AND.IS.GT.0.AND.IN.GT.0) THEN
            PCNTMF(IPC)=TFC(IZ,IS,IN)
          ELSE
          write(*,*)'!!!!!!!!!!!!!!!! invalid connection',INDCP(ICONT)
            STOP ' MZPADJ: unresolvable error: invalid containment data'
          END IF

C Type 4 containment: read containment temperature from boundary
C condition definition facility 
        ELSEIF(INDCP(ICONT).EQ.4) THEN

C Initialize contextual buffer
          write (cContext, '(A,I2,A)')
     &      'Recovering containment temperature for plant component ',
     &      iPC, ' from BC data facility.'

C Input data      
          iBC_column = int(CNTDAT(ICONT,1))
          iBC_interp = int(CNTDAT(iCONT,2))
C Convert building/plant timestep #'s into real represeting
C current day and fraction thereof
          fDay = fConvert_current_TS_to_DAY()
C Collect containment temperature 
          PCNTMF(IPC) = fGet_BC_data_by_col( iBC_column,
     &                                      fDay,
     &                                      iBC_interp,
     &                                      cContext)
                  
         
C New containment types go here!
        ELSE
          STOP ' MZPADJ: unresolvable error: invalid containment type'
        END IF
   29 CONTINUE
 
C Trace output
  900 IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(36).NE.0) THEN
        WRITE(ITU,*) ' Subroutine MZPADJ      Trace output ',ICNT
        ICNT=ICNT+1
        CALL DAYCLK(IDYP,PTIMEF,ITU)

C Dump CONVAR array
        IF(IPCON.GT.0) THEN
          WRITE(ITU,*) ' Plant connection sending node data (CONVAR)',
     &                 ' for ISTATS = ',ISTATS
          DO 910 IPCON=1,NPCON
            WRITE(ITU,10910) IPCON,(CONVAR(IPCON,J),J=1,MCONVR)
  910     CONTINUE
        ELSE
          WRITE(ITU,*) ' There are no plant connections'
        END IF
10910   FORMAT(I5,3G11.4)

C Dump PCNTMF array
        IF(ICONT.GT.0) THEN
          WRITE(ITU,*) ' Plant containment temperatures (PCNTMF)'
          DO 920 ICONT=1,NCONT
            WRITE(ITU,*) ICONT,PCNTMF(ICONT)
  920     CONTINUE
        ELSE
          WRITE(ITU,*) ' There are no plant containments'
        END IF
      END IF
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MZPADJ'
 
C Return to calling module
      RETURN
      END
