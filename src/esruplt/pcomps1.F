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

C ******************** CMP53S ********************
C CMP53S establishes for a plant component with plant db code 530 ie.
C 4 node (ISV>19) AC  heat transfer tube with transport delay
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents solid material   (ISV=29)
C node 2 represents leaving air      (ISV=21)
C node 3 represents water in contact (ISV=20)
C node 4 represents leaving water    (ISV=20)

      SUBROUTINE CMP53S(IPCOMP)
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
         WRITE(ITU,*) ' 4 node (ISV>19) AC  heat transfer tube',
     &                ' with transport delay'
         NITMS=9
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP53S warning: user specified wrong number',
     &                ' of controlled variables'

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

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP53S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      WRITE(IUOUT,*) '             2nd connection to water'
      STOP ' CMP53S: unresolvable error'

      END 

C ******************** CMP54S ********************
C CMP54S establishes for a plant component with plant db code 540 ie.
C 1 node (ISV=20) water circuit flow multiplier built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents leaving water/steam    (ISV=20)

      SUBROUTINE CMP54S(IPCOMP)
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
         WRITE(ITU,*) ' 1 node (ISV=20) water circuit flow multiplier'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP54S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "water"
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
  990 WRITE(IUOUT,*) ' CMP54S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to water'
      STOP ' CMP54S: unresolvable error'

      END 

C ******************** CMP55S ********************
C CMP55S establishes for a plant component with plant db code 550 ie.
C 1 node (ISV=21) air flow multiplier built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents leaving moist air  (ISV=21)

      SUBROUTINE CMP55S(IPCOMP)
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
         WRITE(ITU,*) ' 1 node (ISV=21) air flow multiplier'
         NITMS=2
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP55S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "fluid"
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,1,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
        IF(IPCON1.EQ.0) THEN
          GOTO 990
        ELSE IF(ICONTP(IPCON1).NE.21) THEN
          GOTO 990
        END IF
      ELSE IF(IPCON1.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP55S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      STOP ' CMP55S: unresolvable error'

      END 

C ******************** CMP56S ********************
C CMP56S establishes for a plant component with plant db code 560 ie.
C 5 node (ISV>19) AC fan with submerged motor & built from primitive parts;
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C node 1 represents submerged fan/motor body            (ISV=29)
C node 2 represents fan casing                          (ISV=29)
C node 3 represents air in contact with fan/motor body  (ISV=21)
C node 4 represents air in contact with casing          (ISV=21)
C node 5 represents leaving air                         (ISV=21)

      SUBROUTINE CMP56S(IPCOMP)
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
C Establish static data derivable from the data read from database
      BDATA(IPCOMP,1)=ADATA(IPCOMP,13)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 4 node (ISV>19) AC fan'
         NITMS=12
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
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
     &   WRITE(ITU,*) ' CMP56S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "moist air"
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,3,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP56S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      STOP ' CMP56S: unresolvable error'

      END 

C ******************** CMP57S ********************
C CMP57S establishes for a plant component with plant db code 570 ie.
C 4 node (ISV>19) WCH insulated water pipe with transport delay
C  & built from primitive parts;
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents outer layer solid  (ISV=29)
C node 2 represents inner layer solid  (ISV=29)
C node 3 represents water in contact   (ISV=20)
C node 4 represents leaving water      (ISV=20)

      SUBROUTINE CMP57S(IPCOMP)
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
         WRITE(ITU,*) ' 4 node (ISV>19) WCH water pipe'
         NITMS=11
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP58S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "water"
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,3,IPCONC)
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
  990 WRITE(IUOUT,*) ' CMP57S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to water'
      STOP ' CMP57S: unresolvable error'

      END 

C ******************** CMP58S ********************
C CMP58S establishes for a plant component with plant db code 580 ie.
C 4 node (ISV>19) AC air duct with transport delay & built from
C primitive parts;
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents outer layer solid  (ISV=29)
C node 2 represents inner layer solid  (ISV=29)
C node 3 represents air in contact     (ISV=21)
C node 4 represents leaving air        (ISV=21)

      SUBROUTINE CMP58S(IPCOMP)
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
         WRITE(ITU,*) ' 4 node (ISV>19) AC air duct'
         NITMS=10
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP58S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "moist air"
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,3,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP58S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      STOP ' CMP58S: unresolvable error'

      END 

C ******************** CMP60S ********************
C CMP60S establishes for a plant component with plant db code 600 ie.
C 4 node (ISV>19) AC electric duct heater & built from primitive parts;
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine
C node 1 represents heater element              (ISV=29)
C node 2 represents air in contact with heater  (ISV=21)
C node 3 represents metallic casing             (ISV=29)
C node 4 represents air in contact with casing  (ISV=21)
      SUBROUTINE CMP60S(IPCOMP)
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
      BDATA(IPCOMP,1)=ADATA(IPCOMP,8)

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 4 node (ISV>19) AC duct heater'
         NITMS=7
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         NITMS=1
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
     &   WRITE(ITU,*) ' CMP60S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1st connection to "moist air"
      NCONS=1
      NCONPN=1
      DO 10 IPCONC=1,MPCONC
      IPCON1=ICONDX(IPCOMP,2,IPCONC)
      IF(IPCONC.LE.NCONPN) THEN
         IF(IPCON1.EQ.0) THEN
            GOTO 990
         ELSE IF(ICONTP(IPCON1).NE.21) THEN
            GOTO 990
         END IF
      ELSE IF(IPCON1.NE.0) THEN
         GOTO 990
      END IF
   10 CONTINUE

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP60S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' connection(s)'
      WRITE(IUOUT,*) '         ie  1st connection to air'
      STOP ' CMP60S: unresolvable error'

      END 

C ************************ CMP64S *************************
C CMP64S establishes for a plant component with plant db code 640 ie.
C 1 node (ISV>19) water/steam flow converger built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C node 1 represents mixed fluid   (ISV=20)

      SUBROUTINE CMP64S(IPCOMP)
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
         WRITE(ITU,*) ' 1 node (ISV=20) water/steam flow converger',
     &                ' built from primitive parts'
         NITMS=1
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0 
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP64S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 2 connections only, both to "water"
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

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP64S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP64S: unresolvable error'

      END 

C ************************ CMP65S *************************
C CMP65S establishes for a plant component with plant db code 650 ie.
C 3 nodes (ISV=20) water/steam flow diverger built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C   node 1 represents fluid at first outlet  (ISV=20)
C   node 2 represents fluid at second outlet  (ISV=20)
C   node 3 represents fluid at inlet  (ISV=20)

      SUBROUTINE CMP65S(IPCOMP)
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
         WRITE(ITU,*) ' 3 nodes (ISV=20) water/steam flow diverger',
     &                ' built from primitive parts'
         NITMS=1
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0 
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP65S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has only 1 connection to "water"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,3,IPCONC)
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

      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP65S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' water connection(s)'
      STOP ' CMP65S: unresolvable error'

      END 

C ************************ CMP66S *************************
C CMP66S establishes for a plant component with plant db code 660 ie.
C 3 nodes (ISV=21) moist air flow diverger built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C   node 1 represents air at first outlet  (ISV=21)
C   node 2 represents air at second outlet  (ISV=21)
C   node 3 represents air at inlet  (ISV=21)

      SUBROUTINE CMP66S(IPCOMP)
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
         WRITE(ITU,*) ' 3 nodes (ISV=21) moist air flow diverger',
     &                ' built from primitive parts'
         NITMS=1
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0 
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP66S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has only 1 connection to "air"
      NCONS=1
      DO 10 IPCONC=1,MPCONC
      IPCON=ICONDX(IPCOMP,3,IPCONC)
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
  990 WRITE(IUOUT,*) ' CMP66S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' air connection(s)'
      STOP ' CMP66S: unresolvable error'

      END 

C ************************ CMP68S *************************
C CMP68S establishes for a plant component with plant db code 680 ie.
C 1 node (ISV=21) air flow converger built from primitive parts
C data derivable from the static manufacturers data already read from
C the database and saves it in the static data common block, and also
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine

C   node 1 represents fluid at outlet  (ISV=21)

      SUBROUTINE CMP68S(IPCOMP)
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
         WRITE(ITU,*) ' 1 node (ISV=21) air flow diverger',
     &                ' built from primitive parts'
C         NITMS=1
C         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0 
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP68S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 2 connections, both to "air"
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
  990 WRITE(IUOUT,*) ' CMP68S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' air connection(s)'
      STOP ' CMP68S: unresolvable error'

      END 

C ******************** CMP95S ********************
C CMP95S establishes for a plant component with plant db code 950 ie.
C 1 node (ISV=20) Radiant Heating/Cooling System
C whether the specified number of controlled variables is OK, and also
C whether the number of connections to this component is correct
C and whether the connected nodes are of a type as expected by the
C corresponding coefficient generator routine.

      SUBROUTINE CMP95S(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

C     Flag indicating if radiant floor is active.
      COMMON/RadFloor/RadFloor_active

      LOGICAL RadFloor_active

C     Set radiant floor model flag
      RadFloor_active = .true.

C Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*) ' 1 node (ISV=20) Radiant Heating/Cooling System'
         NITMS=6
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' CMP22S warning: user specified wrong number',
     &                ' of controlled variables'

C Check component has 1 connection only to water
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
      RETURN

C Error handling
  990 WRITE(IUOUT,*) ' CMP68S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' air connection(s)'
      STOP ' CMP68S: unresolvable error'
      END

