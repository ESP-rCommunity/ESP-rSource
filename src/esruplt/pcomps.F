C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.
asdfsdfsdf
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
C      MZSDAT
C      CMP01S   1 node (ISV=21) AC  converging 2-leg junction
C      CMP02S   1 node (ISV=21) AC  humidifier adiabatic
C      CMP03S   1 node (ISV=21) AC  fan
C      CMP04S   1 node (ISV=21) AC  cooling coil & flux control
C      CMP05S   1 node (ISV=21) AC  heating coil & flux control
C      CMP06S   1 node (ISV=21) AC  duct & condensation ?
C      CMP07S   1 node (ISV=21) AC  damper & flow ratio control
C      CMP09S   1 node (ISV=21) AC  counterflow cooling coil &
C                                   water mass flow rate control
C                                   (Q calculated by TRNSYS type32)
C      CMP10S   1 node (ISV=21) AC  counterflow cooling coil &
C                                   water mass flow rate control
C      CMP11S   1 node (ISV=21) AC  counterflow heating coil &
C                                   water mass flow rate control
C      CMP12S   2 node (ISV=21) AC  plate heat exchange
C      CMP14S   2 node (ISV=21) Cooling tower model
C      CMP20S   1 node (ISV=20) WCH boiler
C      CMP21S   2 node (ISV=20) WCH radiator
C      CMP22S   1 node (ISV=20) WCH pipe
C      CMP23S   1 node (ISV=20) WCH converging 2-leg junction
C      CMP24S   1 node (ISV=20) WCH pump
C      CMP25S   2 node (ISV=20) WCH boiler with on/off control
C      CMP26S   2 node (ISV=20) WCH boiler with aquastat control
C      CMP27S   8 node (ISV=20) WCH radiator
C      CMP28S   1 node (ISV=29) Oil-filled electric panel radiator
C      CMP29S   1 node (ISV=20) WCH flow control valve
C      CMP30S   1 node (ISV=20) Water cooler with flux control.
C      CMP31S   2 node (ISV=20) WCH calorifier.
C      CMP32S   2 node (ISV=20) WCH generic liquid/liquid heat exchanger
C      CMP33S   2 node (ISV=20) WCH generic gas/liquid heat exchanger
C      CMP34S   3 node (ISV>19) WCH heated water storage
C      CMP40S   3 node (ISV>19) AC  counterflow cooling coil fed
C                                   by WCH system
C      CMP41S   3 node (ISV>19) AC  counterflow heating coil fed
C      CMP43S   2 node (ISV>20) AC  cooling coil fed
C                                   by WCH system

C      CMP50S   1 node (ISV=29) WCH thermostatic valve
C      CMP51S   1 node (ISV=29)     mechanical room thermostat
C PP-based components
C      CMP53S   4 node (ISV>19) AC  heat transfer tube with transport delay at water side
C      CMP54S   1 node (ISV=20) Water/steam flow multiplier
C      CMP55S   1 node (ISV=21) AC  air flow multiplier
C      CMP56S   5 node (ISV>19) AC  fan with submerged motor
C      CMP57S   4 node (ISV>19) WCH water pipe with transport delay
C      CMP58S   4 node (ISV>19) AC  air duct with transport delay
C      CMP60S   4 node (ISV>19) AC  air electric heater
C      CMP64S   1 node (ISV=20) Water/steam flow converger
C      CMP65S   3 node (ISV=20) Water/steam flow diverger
C      CMP66S   3 node (ISV=21) AC  moist air flow diverger
C      CMP68S   1 node (ISV=21) AC  moist air flow converger
C PP-based components end
C      CMP67S   3 node (ISV>19) AC  heat transfer tube
C      CMP70S   1 node (ISV=20) Flat plate solar collector (type1)
C      CMP71S   1 node (ISV=20) Solar collector "segment"
C      CMP72S   1 node (ISV=20) Water storage tank layer
C      CMP90S   2 node (ISV>19)     air & water temperature source
C      CMP91S   2 node (ISV>19)     imaginary building-like plant load
C                                   acting on heat and mass balances
C      CMP92S   1 node (ISV=21) AC  fictitious air boundary component
C      CMP93S  10 node (ISV>20) Heat exchanger.
C      CMP94S   3 node (ISV=20) Heat exchanger "segment"
C      CMP98S   1 node (ISV=20) CHP engine component
C      CMP99S   1 node (ISV=20) 3 node small scale a CHP unit
C      FC_static_template  3 node (ISV=21) fuel cell
C      FC_tank_fuel_static_temp  3 node (ISV=20) gas-fired water tank for fuel cell
C      FC_tank_elec_static_temp  1 node (ISV=20) electrically heated water tank for fuel cell
C      AIMS_tank1_fuel_static_temp  3 node (ISV=20) gas-fired water tank for first phase of AIMS work
C      TAC_1_static_template  3 node (ISV=20) first phase thermally activated cooling component
C      TAC_coldtank_static_temp  1 node (ISV=20) cold water storage tank for TAC
C      SOFC_A42_static 7-node (ISV=20) IEA/ECBCS Annex 42 SOFC-cogeneration device
C      simple_solar_coll_static 1 node (ISV=20) simple solar collector 

C ******************** MZSDAT ********************

C MZSDAT calls for each plant component considered, a routine CMP??S
C which establishes for a ?? type component, data derivable from the
C static manufacturers data already read from the database and saves
C it in the static data common block. If appropriate, the user
C specified number of controlled variables is checked
C A CMP??S routine also check whether the number of connections to
C that particular ?? type component is correct and whether the
C connected nodes are of a type as expected by the corresponding
C CMP??C coefficient generator routine

      SUBROUTINE MZSDAT
#include "plant.h"
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Component by component, call routine to deal with relevant type
      DO 10 IPCOMP=1,NPCOMP
      ITYPE=NPCDAT(IPCOMP,4)
      IF(ITYPE.EQ.1) THEN
         CALL CMP01S(IPCOMP)
      ELSE IF(ITYPE.EQ.2) THEN
         CALL CMP02S(IPCOMP)
      ELSE IF(ITYPE.EQ.3) THEN
         CALL CMP03S(IPCOMP)
      ELSE IF(ITYPE.EQ.4) THEN
         CALL CMP04S(IPCOMP)
      ELSE IF(ITYPE.EQ.5) THEN
         CALL CMP05S(IPCOMP)
      ELSE IF(ITYPE.EQ.6) THEN
         CALL CMP06S(IPCOMP)
      ELSE IF(ITYPE.EQ.7) THEN
         CALL CMP07S(IPCOMP)
      ELSE IF(ITYPE.EQ.9) THEN
         CALL CMP09S(IPCOMP)
      ELSE IF(ITYPE.EQ.10) THEN
         CALL CMP10S(IPCOMP)
      ELSE IF(ITYPE.EQ.11) THEN
         CALL CMP11S(IPCOMP)
      ELSE IF(ITYPE.EQ.12) THEN
         CALL CMP12S(IPCOMP)
      ELSE IF(ITYPE.EQ.14) THEN
         CALL CMP14S(IPCOMP)
      ELSE IF(ITYPE.EQ.20) THEN
         CALL CMP20S(IPCOMP)
      ELSE IF(ITYPE.EQ.21) THEN
         CALL CMP21S(IPCOMP)
      ELSE IF(ITYPE.EQ.22) THEN
         CALL CMP22S(IPCOMP)
      ELSE IF(ITYPE.EQ.23) THEN
         CALL CMP23S(IPCOMP)
      ELSE IF(ITYPE.EQ.24) THEN
         CALL CMP24S(IPCOMP)
      ELSE IF(ITYPE.EQ.25) THEN
         CALL CMP25S(IPCOMP)
      ELSE IF(ITYPE.EQ.26) THEN
         CALL CMP26S(IPCOMP)
      ELSE IF(ITYPE.EQ.27) THEN
         CALL CMP27S(IPCOMP)
      ELSE IF(ITYPE.EQ.28) THEN
         CALL CMP28S(IPCOMP)
      ELSE IF(ITYPE.EQ.29) THEN
         CALL CMP29S(IPCOMP)
      ELSE IF(ITYPE.EQ.30) THEN
         CALL CMP30S(IPCOMP)
      ELSE IF(ITYPE.EQ.31) THEN
         CALL CMP31S(IPCOMP)
      ELSE IF(ITYPE.EQ.32) THEN
         CALL CMP32S(IPCOMP)
      ELSE IF(ITYPE.EQ.33) THEN
         CALL CMP33S(IPCOMP)
      ELSE IF(ITYPE.EQ.34) THEN
         CALL CMP34S(IPCOMP)
      ELSE IF(ITYPE.EQ.40) THEN
         CALL CMP40S(IPCOMP)
      ELSE IF(ITYPE.EQ.41) THEN
         CALL CMP41S(IPCOMP)
      ELSE IF(ITYPE.EQ.43) THEN
         CALL CMP43S(IPCOMP)
      ELSE IF(ITYPE.EQ.50) THEN
         CALL CMP50S(IPCOMP)
      ELSE IF(ITYPE.EQ.51) THEN
         CALL CMP51S(IPCOMP)
      ELSE IF(ITYPE.EQ.53) THEN
         CALL CMP53S(IPCOMP)
      ELSE IF(ITYPE.EQ.54) THEN
         CALL CMP54S(IPCOMP)
      ELSE IF(ITYPE.EQ.55) THEN
         CALL CMP55S(IPCOMP)
      ELSE IF(ITYPE.EQ.56) THEN
         CALL CMP56S(IPCOMP)
      ELSE IF(ITYPE.EQ.57) THEN
         CALL CMP57S(IPCOMP)
      ELSE IF(ITYPE.EQ.58) THEN
         CALL CMP58S(IPCOMP)
      ELSE IF(ITYPE.EQ.60) THEN
         CALL CMP60S(IPCOMP)
      ELSE IF(ITYPE.EQ.64) THEN
         CALL CMP64S(IPCOMP)
      ELSE IF(ITYPE.EQ.65) THEN
         CALL CMP65S(IPCOMP)
      ELSE IF(ITYPE.EQ.66) THEN
         CALL CMP66S(IPCOMP)
      ELSE IF(ITYPE.EQ.68) THEN
         CALL CMP68S(IPCOMP)
      ELSE IF(ITYPE.EQ.67) THEN
         CALL CMP67S(IPCOMP)
      ELSE IF(ITYPE.EQ.70) THEN
         CALL CMP70S(IPCOMP)
      ELSE IF(ITYPE.EQ.71) THEN
         CALL CMP71S(IPCOMP)
      ELSE IF(ITYPE.EQ.72) THEN
         CALL CMP72S(IPCOMP)
      ELSE IF(ITYPE.EQ.80) THEN
         CALL FC_static_template(IPCOMP)
      ELSE IF(ITYPE.EQ.81) THEN
         CALL FC_tank_fuel_static_temp(IPCOMP)
      ELSE IF(ITYPE.EQ.82) THEN
         CALL FC_tank_elec_static_temp(IPCOMP)
      ELSE IF(ITYPE.EQ.83) THEN
         CALL AIMS_tank1_fuel_static_temp(IPCOMP)
      ELSE IF(ITYPE.EQ.84) THEN
         CALL TAC_1_static_template(IPCOMP)
      ELSE IF(ITYPE.EQ.85) THEN
         CALL TAC_coldtank_static_temp(IPCOMP)
      ELSE IF(ITYPE.EQ.86) THEN
         CALL PEM_static_template(IPCOMP)
      ELSE IF(ITYPE.EQ.90) THEN
         CALL CMP90S(IPCOMP)
      ELSE IF(ITYPE.EQ.91) THEN
         CALL CMP91S(IPCOMP)
      ELSE IF(ITYPE.EQ.92) THEN
         CALL CMP92S(IPCOMP)
      ELSE IF(ITYPE.EQ.93) THEN
         CALL CMP93S(IPCOMP)
      ELSE IF(ITYPE.EQ.94) THEN
         CALL CMP94S(IPCOMP)
      ELSE IF(ITYPE.EQ.95) THEN
C radiant heating/cooling system
         CALL RadFloor_StaticTemp(IPCOMP)
      ELSE IF(ITYPE.EQ.98) THEN
         CALL CMP98S(IPCOMP)
      ELSE IF(ITYPE.EQ.99) THEN
         CALL CMP99S(IPCOMP)
      ELSE IF(ITYPE.EQ. 87
     &  .OR. ITYPE .EQ. 88
     &  .OR. ITYPE .EQ. 89
     &  .OR. ITYPE .EQ. 100
     &  .OR. ITYPE .EQ. 101
     &  .OR. ITYPE .EQ. 102
     &  .OR. ITYPE .EQ. 103
     &  .OR. ITYPE .EQ. 105
     &  .OR. ITYPE .EQ. 106
     &  .OR. ITYPE .EQ. 107 
     &  .OR. ITYPE .EQ. 108
     &  .OR. ITYPE .EQ. 109 
     &  .OR. ITYPE .EQ. 110 ) THEN
C Generic syntax checker for coefficient generator
         CALL HVAC_model_static_temp(IPCOMP)
      ELSE IF( ITYPE .EQ. 104 ) THEN
C 3 node electrolyser_model
         CALL H2_electrolyser_static_temp(IPCOMP)
      ELSE IF(ITYPE.EQ.111) THEN
         CALL SOFC_A42_static(IPCOMP)
      ELSE IF(ITYPE.EQ.112) THEN
C 3-node residential SOFC
         CALL FC_static_template(IPCOMP)
C Gas-fired storage tank with 3 connections
      ELSE IF(ITYPE.EQ.113) THEN
         CALL ADS_tank_fuel_static_temp(IPCOMP)
C Electric storage tank with 3 connections
      ELSE IF(ITYPE.EQ.114) THEN
         CALL ADS_tank_elec_static_temp(IPCOMP)
C Adsorption storage unit
      ELSE IF(ITYPE.EQ.115) THEN
         CALL ADS_stg_static_template(IPCOMP)
C Pressurized WCH pipe
      ELSE IF(ITYPE.EQ.116) THEN
         CALL Press_WCH_pipe_static_temp(IPCOMP)
C Pressurized WCH pump
      ELSE IF(ITYPE.EQ.117) THEN
         CALL Press_WCH_pump_static_temp(IPCOMP)
C 1 node simple solar collector model
      ELSE IF(ITYPE.EQ.118) THEN
         CALL simple_solar_coll_static(IPCOMP)
      ELSE

        call elinc(1)
        WRITE(IUOUT,*) ' No static template for component ',IPCOMP
        STOP ' MZSDAT: unresolvable error on component type'
      END IF
   10 CONTINUE

      RETURN
      END

C ******************** CMP01S ********************
C
C CMP01S establishes for a plant component with plant db code 10 ie.
C 1 node (ISV=21) AC  converging 2-leg junction
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

      SUBROUTINE CMP01S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  converging 2-leg junction'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP01S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 2 connections only, both to "moist air"
      NCONS=2
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP01S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP01S: unresolvable error'

      END

C ******************** CMP02S ********************
C CMP02S establishes for a plant component with plant db code 20 ie.
C 1 node (ISV=21) AC  humidifier
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP02S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,7)
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  humidifier adiabatic'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=4
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP02S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP02S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP02S: unresolvable error'
C
      END
C
C ******************** CMP03S ********************
C
C CMP03S establishes for a plant component with plant db code 30 ie.
C 1 node (ISV=21) AC  fan
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP03S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  fan'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=3
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP03S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP03S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP03S: unresolvable error'
C
      END
C
C ******************** CMP04S ********************
C
C CMP04S establishes for a plant component with plant db code 40 ie.
C 1 node (ISV=21) AC  cooling coil & flux control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP04S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Negate CDATA(?,1) as this component is a cooling coil
C and CDATA(?,1) relates to the maximum cooling energy.
      CDATA(IPCOMP,1)=-CDATA(IPCOMP,1)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil &',
     &                ' water mass flow rate control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP04S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP04S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP04S: unresolvable error'
C
      END
C
C ******************** CMP05S ********************
C
C CMP05S establishes for a plant component with plant db code 50 ie.
C 1 node (ISV=21) AC  heating coil & flux control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP05S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  heating coil & water',
     &                ' mass flow rate control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP05S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP05S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP05S: unresolvable error'
C
      END
C
C ******************** CMP06S ********************
C
C CMP06S establishes for a plant component with plant db code 20 ie.
C 1 node (ISV=21) AC  duct & condensation ?
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP06S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  duct & condensation ?'
         NITMS=6
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP06S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP06S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP06S: unresolvable error'
C
      END
C
C ******************** CMP07S ********************
C
C CMP07S establishes for a plant component with plant db code 70 ie.
C 1 node (ISV=21) AC  damper & flow ratio control
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      subroutine cmp07s(ipcomp)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  damper & flow ratio control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' cmp07s warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

C Return to calling module
      RETURN
C Error handling
  990 WRITE(IUOUT,*) ' cmp07s: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' cmp07s: unresolvable error'
      END

C ******************** CMP09S ********************
C
C CMP09S establishes for a plant component with plant db code 90 ie.
C 1 node (ISV=21) AC  cooling coil & water mass flow rate control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP09S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,8)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil &',
     &                ' water mass flow rate control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=5
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP09S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP09S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP09S: unresolvable error'
      END

C ******************** CMP10S ********************
C
C CMP10S establishes for a plant component with plant db code 100 ie.
C 1 node (ISV=21) AC  cooling coil & water mass flow rate control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP10S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,9)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  cooling coil &',
     &                ' water mass flow rate control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=6
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP10S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP10S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP10S: unresolvable error'
C
      END
C
C ******************** CMP11S ********************
C
C CMP11S establishes for a plant component with plant db code 110 ie.
C 1 node (ISV=21) AC  heating coil & water mass flow rate control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP11S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,9)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) AC  heating coil & water',
     &                ' mass flow rate control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=6
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP11S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP11S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP11S: unresolvable error'
C
      END
C
C ******************** CMP12S ********************
C
C CMP12S establishes for a plant component with plant db code 120 ie.
C 2 node (ISV=21) AC  plate heat exchanger
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP12S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=21) AC  plate heat exchanger'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=2
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP12S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 2 connections only, one per node, and both to air
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP12S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie ',NCONPN,' per node, and both to air'
      STOP ' CMP12S: unresolvable error'
C
      END
C
C ******************** CMP14S ********************
C
C CMP14S establishes for a plant component with plant db code 120 ie.
C 2 node (ISV>20) Cooling tower model
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP14S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=21) AC  plate heat exchanger'
         NITMS=6
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP14S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 2 connections only, one per node, and both to air
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP14S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie ',NCONPN,' per node, one to air'
      WRITE(IUOUT,*) '         and one to water'
      STOP ' CMP14S: unresolvable error'
C
      END
C
C ******************** CMP20S ********************
C
C CMP20S establishes for a plant component with plant db code 200 ie.
C 1 node (ISV=20) WCH boiler & flux control
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP20S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH boiler & flux control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP20S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP20S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP20S: unresolvable error'
C
      END
C
C ******************** CMP21S ********************
C
C CMP21S establishes for a plant component with plant db code 210 ie.
C 2 node (ISV=20) WCH radiator
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP21S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      NBDAT=7+int(ADATA(IPCOMP,9))*2
      DO 5 IBDAT=1,NBDAT
         BDATA(IPCOMP,IBDAT)=ADATA(IPCOMP,IBDAT+2)
    5 CONTINUE

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH radiator'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=NBDAT
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP21S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP21S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP21S: unresolvable error'
C
      END
C
C ******************** CMP22S ********************
C
C CMP22S establishes for a plant component with plant db code 220 ie.
C 1 node (ISV=20) WCH pipe
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP22S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH pipe'
         NITMS=6
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP22S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP22S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP22S: unresolvable error'
C
      END
C
C ******************** CMP23S ********************
C
C CMP23S establishes for a plant component with plant db code 230 ie.
C 1 node (ISV=20) WCH converging 2-leg junction
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP23S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH converging 2-leg junction'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP23S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 2 connections only, both to water
      NCONS=2
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP23S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP23S: unresolvable error'
C
      END
C
C ******************** CMP24S ********************
C
C CMP24S establishes for a plant component with plant db code 240 ie.
C 1 node (ISV=20) WCH pump
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP24S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH pump'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=3
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP24S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP24S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP24S: unresolvable error'
C
      END
C
C ******************** CMP25S ********************
C
C CMP25S establishes for a plant component with plant db code 250 ie.
C C 2 node (ISV=20) WCH boiler with on/off control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP25S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,3)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,7)=ADATA(IPCOMP,9)
      BDATA(IPCOMP,8)=ADATA(IPCOMP,10)
      BDATA(IPCOMP,9)=ADATA(IPCOMP,11)
      BDATA(IPCOMP,10)=ADATA(IPCOMP,12)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH boiler with on/off control '
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=10
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP25S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP25S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP25S: unresolvable error'
C
      END
C
C ******************** CMP26S ********************
C
C CMP26S establishes for a plant component with plant db code 260 ie.
C C 2 node (ISV=20) WCH boiler with aquastat control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP26S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,3)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,7)=ADATA(IPCOMP,9)
      BDATA(IPCOMP,8)=ADATA(IPCOMP,10)
      BDATA(IPCOMP,9)=ADATA(IPCOMP,11)
      BDATA(IPCOMP,10)=ADATA(IPCOMP,12)
      BDATA(IPCOMP,11)=ADATA(IPCOMP,13)
      BDATA(IPCOMP,12)=ADATA(IPCOMP,14)
      BDATA(IPCOMP,13)=ADATA(IPCOMP,15)
      BDATA(IPCOMP,14)=ADATA(IPCOMP,16)
      BDATA(IPCOMP,15)=ADATA(IPCOMP,17)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH boiler & aquastat control '
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=15
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=2
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=2
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP26S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP26S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP26S: unresolvable error'
C
      END
C
C ******************** CMP27S ********************
C
C CMP27S establishes for a plant component with plant db code 270 ie.
C 8 node (ISV=20) WCH radiator
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP27S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      NBDAT=7+int(ADATA(IPCOMP,9))*2
      DO 5 IBDAT=1,NBDAT
         BDATA(IPCOMP,IBDAT)=ADATA(IPCOMP,IBDAT+2)
    5 CONTINUE

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 8 node (ISV=20) WCH radiator'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=NBDAT
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP27S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to water
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP27S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP27S: unresolvable error'

      END

C ******************** CMP28S ********************
C
C CMP28S establishes for a plant component with plant db code 280 ie.
C 1 node (ISV=29) Oil-filled electric panel radiator
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP28S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      NBDAT=6+int(ADATA(IPCOMP,9))*2
      DO 5 IBDAT=1,NBDAT
         BDATA(IPCOMP,IBDAT)=ADATA(IPCOMP,IBDAT+2)
    5 CONTINUE

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=29) Oilo-filled electric radiator'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=NBDAT
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP28S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has no connection.
      NCONS=0
      IPCON=ICONDX(IPCOMP,1,1)
      IF(IPCON.NE.NCONS) GOTO 990
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP28S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP28S: unresolvable error'
      END

C ******************** CMP29S ********************
C CMP29S establishes for a plant component with plant db code 280 ie.
C 1 node (ISV=20) WCH flow control valve
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
      subroutine cmp29s(ipcomp)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) WCH valve & flow ratio control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' cmp29s warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "water"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

C Return to calling module
      RETURN
C Error handling
  990 WRITE(IUOUT,*) ' cmp29s: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' cmp29s: unresolvable error'
      END

C ******************** CMP30S ********************

C CMP30S establishes for a plant component with plant db code 300 ie.
C 1 node (ISV=20) water cooler & flux control
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

      SUBROUTINE CMP30S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Negate CDATA(?,1) as this component is a water cooler
C and CDATA(?,1) relates to the maximum cooling energy.
      CDATA(IPCOMP,1)=-CDATA(IPCOMP,1)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) Water cooler &',
     &                ' flux control'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP30S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

C Return to calling module.
      RETURN

C Error handling.
  990 WRITE(IUOUT,*) ' CMP30S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP30S: unresolvable error'

      END
C ******************** CMP31S ********************
C
C CMP31S establishes for a plant component with plant db code 310 ie.
C C 2 node (ISV=20) WCH calorifier
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP31S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,8)

C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH calorifier '
         NITMS=4
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=4
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP31S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 2 connections only, to water
      NCONS=2
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,IPCONC,1)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP31S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP31S: unresolvable error'
C
      END

C ******************** CMP32S ********************
C
C CMP32S establishes for a plant component with plant db code 320 ie.
C 2 node (ISV>20) WCH generic liquid/liquid heat exchanger
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents water              (ISV=20)
C node 2 represents water              (ISV=20)
C
      SUBROUTINE CMP32S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,6)
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH  liquid/liquid heat  ',
     &                ' exchangerfed by WCH system'
         NITMS=5
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP32S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 2 connections only, one per node, and both to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.20.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      return
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP32S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to water'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP32S: unresolvable error'


      END
C
C ******************** CMP33S ********************
C
C CMP33S establishes for a plant component with plant db code 330 ie.
C 2 node (ISV>20) WCH generic gas/liquid heat exchanger
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents gas                (ISV=21)
C node 2 represents water              (ISV=20)
C
      SUBROUTINE CMP33S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,6)
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV=20) WCH  liquid/liquid heat  ',
     &                ' exchangerfed by WCH system'
         NITMS=5
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP33S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 2 connections only, one per node, and both to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      return
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP33S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to gas'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP33S: unresolvable error'


      END
C
C ******************** CMP34S ********************
C
C CMP34S establishes for a plant component with plant db code 340 ie.
C 3 node (ISV>20) WCH
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents water              (ISV=20) (2 connections)
C node 2 represents gas                (ISV=21) (1 connection)
C node 3 represents gas                (ISV=21)
C
      SUBROUTINE CMP34S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      INTEGER ADAT

C Establish static data derivable from the data read from database
      NDAT=13
      ADAT=3
      DO 10 IDAT=1,NDAT-ADAT
        BDATA(IPCOMP,IDAT)=ADATA(IPCOMP,IDAT+ADAT)
  10  CONTINUE

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV>20) WCH  heated water storage  '
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=10
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP34S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has at least two connections.
      NCONS=2
      NCONPN=1
      DO 20 IPCONC=1,MPCONC
        IPCON1=ICONDX(IPCOMP,1,IPCONC)
        IPCON2=ICONDX(IPCOMP,2,IPCONC)
        IF(IPCONC.LE.NCONPN) THEN
          IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
             GOTO 990
          ELSE IF(ICONTP(IPCON1).NE.20.OR.ICONTP(IPCON2).NE.21) THEN
             GOTO 990
          END IF
        ELSE
          IF(IPCONC.EQ.2.AND.IPCON2.NE.0) THEN
            GOTO 990
          ELSEIF(IPCONC.EQ.3.AND.IPCON1.NE.0)THEN
            GOTO 990
          ENDIF
        END IF

   20 CONTINUE

      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP34S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be 2 or 3 connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to water node'
      WRITE(IUOUT,*) '             2nd connection to water node'
      WRITE(IUOUT,*) '             3rd connection to combustion node'
      STOP ' CMP34S: unresolvable error'

      END
C
C ******************** CMP40S ********************

C CMP40S establishes for a plant component with plant db code 400 ie.
C 3 node (ISV>19) AC  cooling coil fed by WCH system
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents solid material (ISV=29)
C node 2 represents air            (ISV=21)
C node 3 represents water          (ISV=20)
C
      SUBROUTINE CMP40S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,9)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,10)
      BDATA(IPCOMP,7)=ADATA(IPCOMP,11)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV>19) AC  counterflow cooling coil',
     &                ' fed by WCH system'
         NITMS=4
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=7
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP40S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1st connection to "moist air"
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,2,IPCONC)
      IPCON2=ICONDX(IPCOMP,3,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP40S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP40S: unresolvable error'
C
      END
C
C ******************** CMP41S ********************
C
C CMP41S establishes for a plant component with plant db code 410 ie.
C 3 node (ISV>19) AC  heating coil fed by WCH system
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents solid material (ISV=29)
C node 2 represents air            (ISV=21)
C node 3 represents water          (ISV=20)
C
      SUBROUTINE CMP41S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,9)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV>19) AC  counterflow heating coil',
     &                ' fed by WCH system'
         NITMS=4
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=5
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP41S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1st connection to "moist air"
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,2,IPCONC)
      IPCON2=ICONDX(IPCOMP,3,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP41S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP41S: unresolvable error'
C
      END

C ******************** CMP43S ********************
C
C CMP43S establishes for a plant component with plant db code 430 ie.
C 2 node (ISV>20) AC  cooling coil fed by WCH system
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents solid&air material (ISV=21)
C node 2 represents water              (ISV=20)
C
      SUBROUTINE CMP43S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,9)
      BDATA(IPCOMP,7)=ADATA(IPCOMP,10)
      BDATA(IPCOMP,8)=ADATA(IPCOMP,11)
      BDATA(IPCOMP,9)=ADATA(IPCOMP,12)
      BDATA(IPCOMP,10)=ADATA(IPCOMP,13)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV>20) AC  coling coil',
     &                ' fed by WCH system'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=10
         WRITE(ITU,*) ' BDATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP41S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "moist air"
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP43S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP43S: unresolvable error'

      END

C ******************** CMP50S ********************
C
C CMP50S establishes for a plant component with plant db code 500 ie.
C 1 node (ISV=29) WCH thermostatic valve
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP50S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,3)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,8)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=29) WCH thermostatic valve'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=6
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP50S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 2 connections
      NCONS=2
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP50S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      STOP ' CMP50S: unresolvable error'
C
      END
C
C ******************** CMP51S ********************
C
C CMP51S establishes for a plant component with plant db code 510 ie.
C 1 node (ISV=29)     mechanical room thermostat
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP51S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,3)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,4)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,5)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,6)=ADATA(IPCOMP,8)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=29) mechanical room thermostat'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=6
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP51S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 0 connections
      NCONS=0
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP51S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      STOP ' CMP51S: unresolvable error'
C
      END

C ******************** CMP67S ********************
C
C CMP67S establishes for a plant component with plant db code 670 ie.
C 3 node (ISV>19) AC  heat transfer tube fed by WCH system
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
C node 1 represents solid material (ISV=29)
C node 2 represents air            (ISV=21)
C node 3 represents water          (ISV=20)
C
      SUBROUTINE CMP67S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,5)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,8)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV>19) AC  heat transfer tube',
     &                ' fed by WCH system'
         NITMS=4
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=4
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP67S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1st connection to "moist air"
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,2,IPCONC)
      IPCON2=ICONDX(IPCOMP,3,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP67S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP67S: unresolvable error'
C
      END
C
C ******************** CMP70S ********************
C
C CMP70S establishes for a plant component with plant db code 700 ie.
C 1 node (ISV=20) Flat plate solar collector (type1).
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP70S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1) =ADATA(IPCOMP,4)
      BDATA(IPCOMP,2) =ADATA(IPCOMP,5)
      BDATA(IPCOMP,3) =ADATA(IPCOMP,6)
      BDATA(IPCOMP,4) =ADATA(IPCOMP,7)
      BDATA(IPCOMP,5) =ADATA(IPCOMP,8)
      BDATA(IPCOMP,6) =ADATA(IPCOMP,9)
      BDATA(IPCOMP,7) =ADATA(IPCOMP,10)
      BDATA(IPCOMP,8) =ADATA(IPCOMP,11)
      BDATA(IPCOMP,9) =ADATA(IPCOMP,12)
      BDATA(IPCOMP,10)=ADATA(IPCOMP,13)
      BDATA(IPCOMP,11)=ADATA(IPCOMP,14)
      BDATA(IPCOMP,12)=ADATA(IPCOMP,15)
      BDATA(IPCOMP,13)=ADATA(IPCOMP,16)
      BDATA(IPCOMP,14)=ADATA(IPCOMP,17)
      BDATA(IPCOMP,15)=ADATA(IPCOMP,18)
      BDATA(IPCOMP,16)=ADATA(IPCOMP,19)
      BDATA(IPCOMP,17)=ADATA(IPCOMP,20)
      BDATA(IPCOMP,18)=ADATA(IPCOMP,21)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) Flat-plate solar collector'
         NITMS=3
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=18
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP70S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connections
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP70S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      STOP ' CMP70S: unresolvable error'
      END

C ******************** CMP71S ********************
C
C CMP71S establishes for a plant component with plant db code 710 ie.
C 1 node (ISV=20) solar collector "segment".
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP71S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)


C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) Flat-plate solar collector'
         NITMS=22
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP71S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connections
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP71S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      STOP ' CMP71S: unresolvable error'
      END


C ******************** CMP72S ********************
C
C CMP73S establishes for plant component with plant db code 730 i.e
C 1 node (ISV=20) water storage tank with converging inlets
C layer. Checks are made on the number controlled variables
C the number of connections and whether the connected nodes are of the
C correct type as expected by the plant solver.
C

      SUBROUTINE CMP72S(IPCOMP)

#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node water storage tank layer.'
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C Check component has 2 connections to water
      NCONS=2
      NOCON=0
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
C Allow this as there can be 2 or 1 connections.
           NOCON=NOCON+1
C Check that there is at least ONE connection,
C If there is then NOCON should never be 2.
           IF(NOCON.EQ.2) THEN
              GOTO 990
           ENDIF
         ELSE IF(ICONTP(IPCON).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP72S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be 1 or  2  connection(s)'
      WRITE(IUOUT,*) '         to water.'
      STOP ' CMP73S: unresolvable error'
C
      END

C ******************** CMP90S ********************
C
C CMP90S establishes for a plant component with plant db code 900 ie.
C 2 node (ISV>19)     air & water temperature source
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP90S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV>19) air & water temperature source'
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=2
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP90S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1st connection to "moist air"
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
C
C
      RETURN
C
C Error handling
  990 WRITE(IUOUT,*) ' CMP90S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP90S: unresolvable error'
C
      END
C
C ******************** CMP91S ********************
C
C CMP91S establishes for a plant component with plant db code 80 ie.
C 2 node (ISV>19) imaginary building-like plant load
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine.
c
c node 1 is external wall
c node 2 is zone air
C
      subroutine cmp91s(ipcomp)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,8)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 2 node (ISV>19) imaginary building-like plant',
     &                ' load'
         NITMS=5
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=4
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP91S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connection to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(IPCONC.EQ.1.AND.ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP91S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be 1 connection to air'
      STOP ' CMP91S: unresolvable error'
      END

C ******************** CMP92S ********************
C
C CMP92S establishes for a plant component with plant db code 920 ie.
C 1 node (ISV=21) AC  fictitious boundary component.
C There are no static manufacturers data for this component as it
C acts to establish mass flow rate at a boundary connection.
C Checks are performed to see whether the number of connections to this
C component is correct and whether the connected nodes are of a type as
C expected by the corresponding coefficient generator routine.
C
      SUBROUTINE CMP92S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=21) fictitious boundary component'
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP92S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP92S: unresolvable error'
      END

C ******************** CMP93S ********************
C
C CMP93S establishes for a plant component with plant db code 930 ie.
C 10 node (ISV>20) heat exchanger component.
C
C Nodes 1 to 5 are cold fluid nodes.
C Nodes 6 to 10 are solid nodes.
C Connection to this component is via nodes 1 and 5.
      SUBROUTINE CMP93S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
C
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV>20) Heat exchanger component'
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,6)
      BDATA(IPCOMP,2)=ADATA(IPCOMP,7)
      BDATA(IPCOMP,3)=ADATA(IPCOMP,8)
      BDATA(IPCOMP,4)=ADATA(IPCOMP,9)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 10 node (ISV>21) heat exchanger'
         NITMS=5
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=4
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP93S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connection only, to "moist air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONS) THEN
         IF(IPCON.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP93S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s) to air'
      STOP ' CMP93S: unresolvable error'
      END

C***************** CMP94S ********************
C
C CMP94S establishes for a plant component with plant db code 940 ie.
C 3 node (ISV=20) heat exchanger segment whether the correct number
C of control variables have been specified, whether the number of
C connections is correct and whether the connections are to the
C correct fluid type.
       SUBROUTINE CMP94S(IPCOMP)
#include "plant.h"
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV=20) heat exchanger segment'
         NITMS=13
         WRITE (ITU,*)' ADATA ',(ADATA(IPCOMP,J), J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check component has 1st connection to water
C                     2nd connection to water
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.20.OR.ICONTP(IPCON2).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP94S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to water'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP94S: unresolvable error'
      END

C***************** CMP98S ********************
C
C CMP98S establishes for a plant component with plant db code 980 ie.
C 3 node (ISV>19) CHP engine model whether the correct number
C of control variables have been specified, whether the number of
C connections is correct and whether the connections are to the
C correct fluid type.
       SUBROUTINE CMP98S(IPCOMP)
#include "plant.h"
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) CHP engine component'
         NITMS=10
         WRITE (ITU,*)' ADATA ',(ADATA(IPCOMP,J), J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Fill bdata array
      NITMS=9
      DO 5 I=1,NITMS-3
          BDATA(ipcomp,I)=ADATA(ipcomp,I+3)
  5   CONTINUE

C Check component has 1st connection to water.
C
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.20) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP98S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie connection to water'
      WRITE(IUOUT,*) '             '
      STOP ' CMP98S: unresolvable error'
      END
C ******************** CMP99S ********************
C
C CMP99S establishes for a plant component with plant db code 990 ie.
C a 3 node CHP engine unit model:
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C
      SUBROUTINE CMP99S(IPCOMP)
#include "plant.h"
#include "building.h"
C
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
C
C Fill the BDATA array
      DO 5 I=1,18
         BDATA(IPCOMP,I)=ADATA(IPCOMP,5+I)
  5   CONTINUE

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 3 node (ISV=21) Small scale CHP gas engine unit'
         NITMS=5
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=18
         WRITE(ITU,*) ' BDATA ',(BDATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
         WRITE(ITU,*) ' CDATA ',(CDATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
C
C Check user specified number of controlled variables
      NCITM=1
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP99S warning: user specified wrong number',
     &                ' of controlled variables'
C
C Check component has 1st connection to water
C                     2nd connection to air
      NCONS=2
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IPCON2=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0.OR.IPCON2.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.20.OR.ICONTP(IPCON2).NE.21) THEN
            write(*,*) ICONTP(IPCON1), ICONTP(IPCON2)
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0.OR.IPCON2.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE
      RETURN

C
C Error handling
  990 WRITE(IUOUT,*) ' CMP99S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' 1st to water 2nd to'
      WRITE(IUOUT,*) '         ambient.'
      STOP ' CMP99S: unresolvable error'
C
      END
