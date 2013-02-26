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
C      MZAREA
C      MZVOLM
C      MZPAZI
C      MZVFCL
C      MZVENC
C      MZCASG
C      MZTCON
C      MZMISC

c ******************** MZAREA ********************

c MZAREA computes the area of each zone surface as well
c as hydraulic diameter and characteristic height.

c Common block variables are:

c SNA     - surface area of each multilayered construction
c APRAT   - surface hydraulic diameter (4*Area/Perimeter)
c HEIGHT  - records for each surface the difference between the
c           max. and min. Z-value.   For vertical surfaces this is
c           used as the characteristic dimension (rather than APRAT)
c           in the computation of convection coefficients.
c zonetotsurfacearea - total surface area in the zone (held in geometry.h)
      SUBROUTINE MZAREA(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC1I/APRAT(MCOM,MS),HEIGHT(MCOM,MS)

c The information contained in the following common block represents
c the components of the normal vector to each face and is preserved
c for use in subroutines MZVOLM, MZPAZI and MZCFGB.
      COMMON/NORMV/XSUM(MS),YSUM(MS),ZSUM(MS)

c Set area summations to zero.
      zonetotsurfacearea(ICOMP)=0.

c And consider each surface of the current zone in turn.
      DO 10 I=1,NSUR
        XS=0.
        YS=0.
        ZS=0.
        ZMAX=-1.E+10
        ZMIN=1.E+10
        NV=NVER(I)

c Compute gross surface area.
        ADDL=0.
        DO 20 J=1,NV
          K=J+1
          IF(J.EQ.NV)K=1
          IP1=JVN(I,J)
          IP2=JVN(I,K)
          ZMAX=AMAX1(ZMAX,Z(IP1))
          ZMIN=AMIN1(ZMIN,Z(IP1))
          XS=XS+Y(IP1)*Z(IP2)-Z(IP1)*Y(IP2)
          YS=YS+Z(IP1)*X(IP2)-X(IP1)*Z(IP2)
          ZS=ZS+X(IP1)*Y(IP2)-Y(IP1)*X(IP2)
          ADDL=ADDL+SQRT((X(IP2)-X(IP1))**2+(Y(IP2)-Y(IP1))**2+
     &         (Z(IP2)-Z(IP1))**2)
   20   CONTINUE
        XSUM(I)=XS
        YSUM(I)=YS
        ZSUM(I)=ZS
        ZAREA=.5*SQRT(XS*XS+YS*YS+ZS*ZS)
        APRAT(ICOMP,I)=4.*ZAREA/ADDL
        HEIGHT(ICOMP,I)=ZMAX-ZMIN

c Surface area is given by:
        SNA(ICOMP,I)=ZAREA

c Check for -ve surface area: windows wrongly specified.
        IER=0
        IF(SNA(ICOMP,I).LT.0.00001)then
          call edisp(iuout,' Negative surface area detected.')
          IER=1
        endif

c And surface area summations by:
        zonetotsurfacearea(ICOMP)=zonetotsurfacearea(ICOMP)+SNA(ICOMP,I)
   10 CONTINUE
      RETURN
      END

c ******************** MZVOLM ********************

c MZVOLM computes the volume of any polyhedral zone by
c computing the algebraic sum of the volumes of a prism
c formed by joining the coordinate system origin point
c (0,0,0) with each vertex of each face in turn.   The prism
c volume of a face whose associated outward normal (when the
c face vertices are defined in an anticlockwise direction
c when viewed from 'outside') faces away from the origin is
c computed as a positive value.   Conversely a negative value
c is obtained when the outward normal to the face points
c towards the origin.

c Common block variables are:

c VOL    - zone bounded volume

      SUBROUTINE MZVOLM(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/NORMV/XSUM(MS),YSUM(MS),ZSUM(MS)

      V=0.

c Compute the volume of the prism formed between each face and
c coordinate system origin point and algebraically summate for
c all faces.
      DO 10 I=1,NSUR
      J=JVN(I,1)
      PV=.16667*(X(J)*XSUM(I)+Y(J)*YSUM(I)+Z(J)*ZSUM(I))
      V=V+PV
   10 CONTINUE
      VOL(ICOMP)=ABS(V)

c Check for small volume.
      IER=0
      IF(VOL(ICOMP).LE.0.00001)then
        call edisp(iuout,' Negative zone volume detected. ')
        IER=1
      endif

      RETURN
      END

c ******************** MZPAZI ********************

c MZPAZI computes the azimuth and elevation angles of the
c outward facing normal for each plane in turn and sets
c a characteristic dimension for use in res for the
c computation of surface convection coefficients.   The
c azimuth is the angle - measured from north (Y-axis) in
c degrees - clockwise positive.   The elevation angle is the
c angle between the plane normal and the horizontal
c measured vertically in degrees.

c Common block variables are:

c PAZI    - plane azimuth angle

c PELV    - plane elevation angle

c CHARDM  - plane characteristic dimension for use
c           in res to compute surface
c           convection coefficients.

      SUBROUTINE MZPAZI(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/NORMV/XSUM(MS),YSUM(MS),ZSUM(MS)

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/TRANSO/LTROR,PAZIO(MCOM,MS)
      COMMON/PREC1I/APRAT(MCOM,MS),HEIGHT(MCOM,MS)
      COMMON/PREC3I/CHARDM(MCOM,MS)
      logical close

      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      DO 10 I=1,NSUR
        PAZI(ICOMP,I)=90.0
        IF(XSUM(I).LT.0.0)PAZI(ICOMP,I)=-90.0
        call eclose(XSUM(I),0.0,0.0001,close)
        IF(close)PAZI(ICOMP,I)=0.0
        PELV(ICOMP,I)=90.0
        IF(ZSUM(I).LT.0.0)PELV(ICOMP,I)=-90.0
        call eclose(ZSUM(I),0.0,0.0001,close)
        IF(close)PELV(ICOMP,I)=0.0
        call eclose(YSUM(I),0.0,0.0001,close)
        if(close)goto 1
        AZI=ATAN2(XSUM(I),YSUM(I))
        PAZI(ICOMP,I)=AZI/R
    1   SRX2Y2=SQRT(XSUM(I)*XSUM(I)+YSUM(I)*YSUM(I))
        IF(PAZI(ICOMP,I).LT.0.)PAZI(ICOMP,I)=PAZI(ICOMP,I)+360.
        call eclose(SRX2Y2,0.00,0.001,close)
        IF(close)goto 2
        ELV=ATAN2(ZSUM(I),SRX2Y2)
        PELV(ICOMP,I)=ELV/R
    2   CONTINUE
        CHARDM(ICOMP,I)=APRAT(ICOMP,I)
        IF(PELV(ICOMP,I).LE.45..AND.PELV(ICOMP,I).GE.
     &  -45.)CHARDM(ICOMP,I)=HEIGHT(ICOMP,I)
        PAZIO(ICOMP,I)=PAZI(ICOMP,I)
   10 CONTINUE

      RETURN
      END

c ******************** MZVFCL ********************

c MZVFCL calculates the black body geometric view factor between
c each internal surface pair for each zone based on
c an area weighting technique. This technique is exact for a
c a window- and door-less cube.
c This routine is invoked only if a zone view factor file is
c unavailable.

      SUBROUTINE MZVFCL(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/V2/CFB(MCOM,MST,MST)

      COMMON/PREC1/SNA(MCOM,MS)

      character outs*124

      NS=NSUR
      DO 10 I=1,NS
      II=I
      DO 20 J=1,NS
      JJ=J
      CFB(ICOMP,II,JJ)=0.0
      IF(II.EQ.JJ)goto 20
      CFB(ICOMP,II,JJ)=
     &  (SNA(ICOMP,JJ)/(zonetotsurfacearea(ICOMP)-SNA(ICOMP,II)))
   20 CONTINUE
   10 CONTINUE

c Trace output ?
      IF(ITRACE(4).EQ.1.AND.IZNTRC(ICOMP).EQ.1)goto 2
      goto 3

    2 write(outs,'(A,I4)')' Subroutine MZVFCL   Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A,I4)')'ZONE',ICOMP
      call edisp(itu,outs)
      ICNT=ICNT+1
      call edisp(itu,' ')
      call edisp(itu,' Black body view factors')

C Show first 24 surfaces.
      nss=min0(24,ns)
      write(outs,'(4X,24I5)')(I,I=1,nss)
      call edisp(itu,outs)
      DO 9994 I=1,NS
        write(outs,'(I3,1X,24F5.2)')I,(CFB(ICOMP,I,J),J=1,nss)
        call edisp(itu,outs)
 9994 CONTINUE
    3 RETURN
      END

c ******************** MZVENC ********************

c MZVENC computes the hourly ventilation conductance

c Common block variables are:
c CVIN &    - hourly ventilation conductance for
c CVVN        infiltration and ventilation air respectively
c             and for weekdays
c ICCN      - hourly convection coupling index:
c              ICCN=0 ; ventilation air at constant temp.
c                       (TIA1)
c              ICCN=N ; ventilation air at zone N temperature
c TIAN      - hourly temperature of incoming air during
c             and corresponding to ICCN=0
C Each day type defined (legacy and default are weekday/saturday/sunday)
C may have a different value defined for the above variables

      SUBROUTINE MZVENC(ICOMP)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,idty
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/PREC5N/CVIN(MDTY,MCOM,MT),CVVN(MDTY,MCOM,MT),
     &ICCN(MDTY,MCOM,MT),TIAN(MDTY,MCOM,MT)
      INTEGER ICCN
      REAL CVIN,CVVN,TIAN

      character outs*124

      A=0.33*VOL(ICOMP)

C << what about temporal zone infil and vent? should this require a return? >>
      NDTY=NBDAYTYPE
      DO 500 IDTY=1,NDTY
        DO 501 I=1,24
          ACIX=0.
          ACVX=0.
          ICCX=0
          ATX=0.
          IF(NAC(IDTY).EQ.0)GOTO 601
          DO 502 J=1,NAC(IDTY)
            IT1=IACS(IDTY,J)
            IT2=IACF(IDTY,J)
            IF(I.GT.IT1.AND.I.LE.IT2)GOTO 602
            GOTO 502
 602        ACIX=ACI(IDTY,J)
            ACVX=ACV(IDTY,J)
            ICCX=IPT(IDTY,J)
            ATX=TA(IDTY,J)
            GOTO 601
 502      CONTINUE
 601        CVIN(IDTY,ICOMP,I)=A*ACIX
            CVVN(IDTY,ICOMP,I)=A*ACVX
            ICCN(IDTY,ICOMP,I)=ICCX
            TIAN(IDTY,ICOMP,I)=ATX
 501    CONTINUE
 500  CONTINUE

C Trace output ? (Currently only the first three day types are displayed
      IF(ITRACE(11).EQ.1.AND.IZNTRC(ICOMP).EQ.1)goto  5555
      GOTO 9999
 5555 CONTINUE
      write(outs,'(A,I4)')' Subroutine MZVENC   Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A,I4)')'Zone',ICOMP
      call edisp(itu,outs)
      call edisp(itu,' Time  Weekday........................')
      call edisp(itu,'          CVI    CVV    C.Index  Temp.')
      ICNT=ICNT+1
      DO 9994 I=1,24
      write(outs,'(I4,F10.2,F7.2,I8,F8.2)')I,CVIN(1,ICOMP,I),
     &           CVVN(1,ICOMP,I),ICCN(1,ICOMP,I),TIAN(1,ICOMP,I)
      call edisp(itu,outs)
 9994 CONTINUE
      J=ICNT-1
      write(outs,'(A,I4,A)')' Trace output',J,' Continued'
      call edisp(itu,outs)
      call edisp(itu,' ')
      call edisp(itu,' Time Saturday........................')
      call edisp(itu,'          CVI    CVV    C.Index  Temp.')
      DO 9991 I=1,24
      write(outs,'(I4,F10.2,F7.2,I8,F8.2)')I,CVIN(2,ICOMP,I),
     &           CVVN(2,ICOMP,I),ICCN(2,ICOMP,I),TIAN(2,ICOMP,I)
      call edisp(itu,outs)
 9991 CONTINUE
      write(outs,'(A,I4,A)')' Trace output',J,' Continued'
      call edisp(itu,outs)
      call edisp(itu,' Time   Sunday........................')
      call edisp(itu,'          CVI    CVV    C.Index  Temp.')
      DO 9988 I=1,24
      write(outs,'(I4,F10.2,F7.2,I8,F8.2)')I,CVIN(3,ICOMP,I),
     &           CVVN(3,ICOMP,I),ICCN(3,ICOMP,I),TIAN(3,ICOMP,I)
      call edisp(itu,outs)
 9988 CONTINUE
 9999 RETURN
      END

c ******************** MZCASG ********************

c MZCASG computes the total radiant and convective components
c of casual gains prevailing at each hour during weekdays,
c saturdays and sundays separately.

c Common block variables are:
C NGNTYP - number of gain types (zone, day type)
C NCGPER - number of periods for each gain (zone, day type, gain type)
C TCGS - start time of gain (zone, day type, gain type, period)
C CGSENR - sensible gain radiant portion (zone, day type, gain type, period)
C CGSENC - sensible gain convective portion (zone, day type, gain type, period)
C CGLAT - latent gain (zone, day type, gain type, period)
C CGCTL - gain control fraction (zone, present/future, gain type)

      SUBROUTINE MZCASG(ICOMP)
#include "building.h"

C geometry.h provides commons G0/G1/G2/G4/prec2/prec17/precz.
#include "geometry.h"
#include "schedule.h"
#include "power.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/trc/itrc
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN

C Commons for electrical data
      COMMON/ELECFLG/IELF(MCOM)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/CASGFA/FLOORA(MCOM)

C Derived casual gain and electrical data.
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      common/CASGNSE/ELQ(MCOM,MDTY,MGTY,MGPER,MPHAS),
     &               ELR(MCOM,MDTY,MGTY,MGPER,MPHAS)

C Common for HOT3000 electrical-network -> casual-gain coupling
C Symopsys: E_to_G_coupling: boolian indicating coupling exists
C           Coupling_gain_info(i,j,k,l): array containing data
C           for gain k on daytype j for zone i. 
C           l = 1 -> start hour (0->24)
C           l = 2 -> end hour (0->24)
C           l = 3 -> sensible load (fraction of non-hvac elec load
C           l = 4 -> latent load (fraction of non-hvac elec load
C           l = 5 -> radiative feaction (0->1)
C           l = 6 -> convective fraction (0->1)
C
C - Alex Feb. 20 2004
      COMMON/CasCou/iNumb_Gain_coupling(MCOM,MDTY),
     &     Coupling_gain_info(MCOM,MDTY,MC,6)
      INTEGER iNumb_Gain_coupling,index
      REAL Coupling_Gain_info
      real PERSN  ! number of persons per m^2
      character outs*124
      logical close

      REAL PQ,PR,PA

C << Note: the simulator only deals with geometry of GEN type >>
C << so this logic could be simpler >>
C If zone is of type 'GEN' and casual gains are specified
C relative to the floor area, then we need to ask user for
C planes which comprise the floor.
      IF(CTYPE(icomp)(1:3).EQ.'GEN')THEN

C Establish if floor area needs be specified.
        INDEX=0
        DO 400 IDTY=1,NBDAYTYPE
          IF(INDEX.EQ.1)GOTO 400
          IF(NCAS(IDTY).GT.0)THEN
            DO 160 I=1,NCAS(IDTY)
              if(ICGUnit(IDTY,I).ge.1)then
                INDEX=1  ! if gain type overloaded
              endif
 160        CONTINUE
          ENDIF
 400    CONTINUE
       if (INDEX.ne.0) then

C Floor area is required, if trace level is verbose echo the base area.
          FLOORA(ICOMP)=ZBASEA(ICOMP)
          write(outs,'(a,a,a,F6.1,a)') 'The base area of ',
     &      zname(icomp)(1:lnzname(icomp)),
     &      ' is',FLOORA(ICOMP),'m^2.'
          if (itrc.gt.1) call edisp(iuout,outs)
        endif
      else
        FLOORA(ICOMP)=SNA(ICOMP,NSUR)  ! assume last surface in zone
      ENDIF
      
C Zero derived gains.
      do 110 ID=1,MDTY
        do 120 IG=1,MGTY
          NCGPER(ICOMP,ID,IG)=0
          CGCTL(ICOMP,1,IG)=1.
          CGCTL(ICOMP,2,IG)=1.
          do 130 IP=1,MGPER
            TCGS(ICOMP,ID,IG,IP)=0.
            CGSENC(ICOMP,ID,IG,IP)=0.
            CGSENR(ICOMP,ID,IG,IP)=0.
            CGLAT(ICOMP,ID,IG,IP)=0.
            do 140 IPH=1,MPHAS
              ELQ(ICOMP,ID,IG,IP,IPH)=0.
              ELR(ICOMP,ID,IG,IP,IPH)=0.
 140        continue
 130      continue
 120    continue
 110  continue

C Take gains and process them into the simulation common block.
C NOTE: support for additional casual gains types added:
C - if abs(ICGT(IDAY,I)) = 5, or caskeytype = 'net_utilities' then the
C   casual gain is coupled to electric load and electric_net_utilities
C   and electric_net_utilities is called to characterize
C   the casual gain.
C Alex, Feb 20, 2004.

      do 300 iday=1,nbdaytype
      if (NCAS(iday).gt.0) then
C.....Start counter for electric->casual gain couplings
      ingc = 0
      do 210 I=1,NCAS(iday)
        ITYP=abs(ICGT(IDAY,I))
        if(caskeytype(icomp,ityp)(1:13).eq.'net_utilities'.or.
     &     ityp.eq.5)then
          write(outs,*) 'found a net_utilities ',iday,i,ityp,
     &      caskeytype(icomp,ityp)
          if (itrc.gt.1) call edisp(iuout,outs)
        endif
        IT1=ICGS(IDAY,I)
        IT2=ICGF(IDAY,I)

C If the caskeytype is net_utilities or the slot is 5 do electrical coupling.
        if(caskeytype(icomp,ityp)(1:13).eq.'net_utilities'.or.
     &     ityp.eq.5)then
           ingc = ingc + 1 !Increment counter 
           Coupling_gain_info(ICOMP,IDAY,ingc,1) = real(IT1)    !start hour
           Coupling_gain_info(ICOMP,IDAY,ingc,2) = real(IT2)    !End hour
           Coupling_gain_info(ICOMP,IDAY,ingc,3) = CMGS(IDAY,I) !Sensible load coefficient
           Coupling_gain_info(ICOMP,IDAY,ingc,4) = CMGL(IDAY,I) !Latent load coefficient
           Coupling_gain_info(ICOMP,IDAY,ingc,5) = RADC(IDAY,I) !Radiant fraction
           Coupling_gain_info(ICOMP,IDAY,ingc,6) = CONC(IDAY,I) !Convective fraction

C Latent load couplings are presently not supported. Check for coupling
C in specifications, and if found (i) warn user & (ii) set to zero.
           if( Coupling_gain_info(ICOMP,IDAY,ingc,4) .gt.0. ) then 
              Coupling_gain_info(ICOMP,IDAY,ingc,4) =  0.      
             call edisp(itu,' ')
             write(outs,'(A)') 'WARNING: MZCASG in precal.F'
             call edisp(itu,outs)
             write(outs,'(A)') 'A HOT3000 electrical-load->latent-gain'
             call edisp(itu,outs)
             write(outs,'(A)') 'coupling has been specified in zone #'
             call edisp(itu,outs)
             write(outs,'(a,I2,a,I2,a)') ' ',icomp,
     &            ', Weekday gain # ', I,', but this type of'
             call edisp(itu,outs)
             write(outs,'(a)') 
     &            'coupling is presently unsupported. The '
             call edisp(itu,outs)
             write(outs,'(a)') 
     &            'latent portion of this coupling will be'
             call edisp(itu,outs)
             write(outs,'(a)') 'set to zero.'
             call edisp(itu,outs)
             call edisp(itu,' ')
           ENDIF
        ENDIF
C Debug
C        write(6,*) '560 precal ',ICGT(iday,I),ICGUnit(iday,I)
        if(ICGUnit(iday,I).eq.0)then
          NCGPER(ICOMP,IDAY,ITYP)=NCGPER(ICOMP,IDAY,ITYP)+1
          IPER=NCGPER(ICOMP,IDAY,ITYP)
          TCGS(ICOMP,IDAY,ITYP,IPER)=ICGS(IDAY,I)
          TCGS(ICOMP,IDAY,ITYP,IPER+1)=ICGF(IDAY,I)
          CGSENC(ICOMP,IDAY,ITYP,IPER)=CMGS(IDAY,I)*CONC(IDAY,I)
          CGSENR(ICOMP,IDAY,ITYP,IPER)=CMGS(IDAY,I)*RADC(IDAY,I)
          CGLAT(ICOMP,IDAY,ITYP,IPER)=CMGL(IDAY,I)

C Electrical loads. Determine the reactive power consumption first.
          if (IELF(ICOMP).GT.0) then
            PR=PWR(IDAY,I)
            call PRECOMP(PR,PF(IDAY,I),IPF(IDAY,I),PA,PQ)
            if (IPHAS(IDAY,I).LT.4) then
              ELQ(ICOMP,IDAY,ITYP,IPER,IPHAS(IDAY,I))=PR
              ELR(ICOMP,IDAY,ITYP,IPER,IPHAS(IDAY,I))=PQ
            else
            
C Assign the power consumption evenly over the three phases for a 3-phase load.
              do 213 IP=1,3
                ELQ(ICOMP,IDAY,ITYP,IPER,IP)=PR/3.
                ELR(ICOMP,IDAY,ITYP,IPER,IP)=PQ/3.
 213          continue
            endif
          endif
        elseif(ICGUnit(iday,I).eq.1)then

C This is an explicit W/m2 for any gain type (see the logic below
C for traditional overload of ICGT=-1 which eroper interprets
C as ICGUnit=2).
          write(outs,*) 'found Wm2 ',iday,i,ityp,caskeytype(icomp,ityp)
          if (itrc.gt.1) call edisp(iuout,outs)
          NCGPER(ICOMP,IDAY,ITYP)=NCGPER(ICOMP,IDAY,ITYP)+1
          IPER=NCGPER(ICOMP,IDAY,ITYP)
          TCGS(ICOMP,IDAY,ITYP,IPER)=ICGS(IDAY,I)
          TCGS(ICOMP,IDAY,ITYP,IPER+1)=ICGF(IDAY,I)
          CGSENC(ICOMP,IDAY,ITYP,IPER)=
     &      CMGS(IDAY,I)*CONC(IDAY,I)*FLOORA(ICOMP)
          CGSENR(ICOMP,IDAY,ITYP,IPER)=
     &      CMGS(IDAY,I)*RADC(IDAY,I)*FLOORA(ICOMP)
          CGLAT(ICOMP,IDAY,ITYP,IPER)=
     &      CMGL(IDAY,I)*FLOORA(ICOMP)

C Electrical loads. Determine the reactive power consumption first.
          if (IELF(ICOMP).GT.0) then
            PR=PWR(IDAY,I)*FLOORA(ICOMP)
            call PRECOMP(PR,PF(IDAY,I),IPF(IDAY,I),PA,PQ)
            if (IPHAS(IDAY,I).LT.4) then
              ELQ(ICOMP,IDAY,ITYP,IPER,IPHAS(IDAY,I))=PR
              ELR(ICOMP,IDAY,ITYP,IPER,IPHAS(IDAY,I))=PQ
            else
            
C Assign the power consumption evenly over the three phases for a 3-phase load.
              do 215 IP=1,3
                ELQ(ICOMP,IDAY,ITYP,IPER,IP)=PR/3.
                ELR(ICOMP,IDAY,ITYP,IPER,IP)=PQ/3.
 215          continue
            endif
          endif
        elseif(ICGUnit(iday,I).eq.2)then

C This is a case of m^2 per person (including fractional
C persons) which makes a fixed assumption of 95W sensible and 45W
C latent per person. In eroper the scan of -1 type is converted
C to the unit m^2/object.
          if(caskeytype(icomp,ityp)(1:6).eq.'people')then
C Debug
            write(outs,*) 'found m2 people ',iday,i,ityp,
     &        caskeytype(icomp,ityp)
            if (itrc.gt.1) call edisp(iuout,outs)
            NCGPER(ICOMP,IDAY,ITYP)=NCGPER(ICOMP,IDAY,ITYP)+1
            IPER=NCGPER(ICOMP,IDAY,ITYP)
            TCGS(ICOMP,IDAY,ITYP,IPER)=ICGS(IDAY,I)
            TCGS(ICOMP,IDAY,ITYP,IPER+1)=ICGF(IDAY,I)
            call eclose(CMGS(IDAY,I),0.00,0.001,close)
            if (close) then
              PERSN=0.0  ! if m^2/person was close to zero set to no occup.
            else
              PERSN=FLOORA(ICOMP)/CMGS(IDAY,I)
            endif
            CGSENC(ICOMP,IDAY,ITYP,IPER)=PERSN*95.0*CONC(IDAY,I)
            CGSENR(ICOMP,IDAY,ITYP,IPER)=PERSN*95.0*RADC(IDAY,I)
            CGLAT(ICOMP,IDAY,ITYP,IPER)=PERSN*45.0
          else
            write(6,*) 'unknown m2/object',ICGUnit(iday,I),ITYP
          endif
        else
          write(6,*) 'unknown combination',ICGUnit(iday,I),ITYP
        endif
        iNumb_gain_coupling(icomp,iday)=INGC
 210  continue   ! loop of casual gains in the day

C If no gains defined then set one null period.
      do 211 IG=1,MGTY
        if (NCGPER(ICOMP,IDAY,IG).eq.0) then
          NCGPER(ICOMP,IDAY,IG)=1
        endif
 211  continue
      endif
 300  continue  ! loop of day types

C Trace output ?
      if (ITRACE(12).EQ.1.AND.IZNTRC(ICOMP).EQ.1) then
        write(outs,'(a,I4)')' Subroutine MZCASG     Trace output',ICNT
        call edisp(itu,outs)
        ICNT=ICNT+2
        write (outs,'(23X,A,I7)') 'Zone',ICOMP
        call edisp(itu,outs)
        write (outs,'(2a)') ' Time        Weekday              ',
     &                                  'Saturday              Sunday'
        call edisp(itu,outs)
        write (outs,'(2a)')'     Radnt. Cnvct. Latent Radnt. Cnvct. ',
     &                                   'Latent Radnt. Cnvct. Latent'
        call edisp(itu,outs)
        write (outs,'(2a)')'     Cmpnt. Cmpnt. Cmpnt. Cmpnt. Cmpnt. ',
     &                                   'Cmpnt. Cmpnt. Cmpnt. Cmpnt.'
        call edisp(itu,outs)
        call edisp(itu,' ')

C << hard coded assumption of day types and gain types. >>
        do 900 ID=1,3
          call edisp(itu,' --------------')
          call edisp(itu,'Gain  Start    Rad     Conv  Latent')
          call edisp(itu,'slot  time     gain    gain  gain')
          do 910 IT=1,3
            do 920 IP=1,NCGPER(ICOMP,ID,IT)
              write (outs,'(i4,4f8.2)') IT,TCGS(ICOMP,ID,IT,IP),
     &           CGSENR(ICOMP,ID,IT,IP),CGSENC(ICOMP,ID,IT,IP),
     &           CGLAT(ICOMP,ID,IT,IP)
              call edisp(itu,outs)
 920        continue
 910      continue
 900    continue

        IF(IELF(ICOMP).GT.0) THEN
          call edisp(itu,' ')
          call edisp(itu,' Uncontrolled electrical loads - Real ')
          call edisp(itu,' ')
          write(outs,*)
     &    'Time       Weekday             Saturday               Sunday'
          call edisp(itu,outs)
          write(outs,*)
     &     ' Hrs         Load                Load                  Load'
          call edisp(itu,outs) 
          write(outs,*)
     &  '            |R G B|             |R G B|                |R G B|'
          call edisp(itu,outs)
          call edisp(itu,' ')
          call edisp(itu,' Uncontrolled electrical loads - Reactive ')
          call edisp(itu,' ')
          write(outs,*)
     &    'Time       Weekday             Saturday               Sunday'
          call edisp(itu,outs)
          write(outs,*)
     &     ' Hrs         Load                Load                  Load'
          call edisp(itu,outs) 
          write(outs,*)
     &  '            |R G B|             |R G B|                |R G B|'
          call edisp(itu,outs) 
        ENDIF
      ENDIF

      RETURN
      END

c ******************** MZTCON ********************

c MZTCON determines the simulation start-up time as a
c function of the thermophysical properties of the building
c fabric.

c Common block variables are:

c ITCNST  - number of start-up days

      SUBROUTINE MZTCON(ICOMP)
#include "building.h"
#include "geometry.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/PREC7/ITCNST

C Reset the start-up period if first zone.
      if (ICOMP.eq.1) ITCNST=1

c First determine maximum thermal diffusivity and associated
c homogeneous element.
      TCM=0.
      TDM=0.
      DO 10 I=1,NSUR
      RES=0.
      CAP=0.
      NELT=NE(I)
      NGPS=NAIRG(I)
      IP=1
      DO 20 J=1,NELT
      IF(IP.EQ.-1)goto 201
      IF(NGPS.GT.0.AND.J.EQ.IPAIRG(I,IP))goto 1
  201 TD=CON(I,J)/(DEN(I,J)*SHT(I,J))
      IF(I.GT.1.AND.I.LE.NSUR)RES=RES+THK(I,J)/CON(I,J)
      IF(I.GT.1.AND.I.LE.NSUR)CAP=CAP+DEN(I,J)*SHT(I,J)*THK(I,J)
      IF(TD.GT.TDM)goto 2
      goto 20
    2 TDM=TD
      goto 20
    1 RES=RES+RAIRG(I,IP)
      IP=IP+1
      IF(IP.GT.NGPS)IP=-1
   20 CONTINUE
      RO=THK(I,1)/CON(I,1)
      U=1./(RES+RO)
      TC=((CON(I,1)*DEN(I,1)*SHT(I,1)*(RO-0.1*RES))+(1.1*CAP))/U
      IF(TC.GT.TCM)goto 3
      goto 10
    3 TCM=TC
   10 CONTINUE

c Determine the start-up length as a function of this maximum
c time constant.
      ITCN=(1*INT(TCM/3600.))/24+1
c     IF(ITCN.LT.2)ITCN=2
c     IF(ITCN.GT.7)ITCN=7
      IF(ITCN.GT.ITCNST)ITCNST=ITCN
      RETURN
      END

c ******************** MZMISC ********************

c MZMISC computes miscellaneous data required throughout
c the simulation and for transfer to the results library
c for use during the later output retrieval.

c Common block variables are:

c ZNAME   - the name of each zone comprising the building system

c SLAT &  - site latitude and longitude
c SLON

c NCONST  - number of constructions in each component

c NELT    - number of elements in each construction

c NGAPS   - number of air gaps in each construction

c NPGAP   - the position of each gap (element number counting
c           from 'outside')

c NWINS   - number of default windows in each construction (set to zero)

c EI & EE - emissivity of each internal and external surface
c           respectively

c AI & AE - solar absorptivity of each internal and external
c           surface respectively

      SUBROUTINE MZMISC(ICOMP)
#include "building.h"
#include "site.h"
#include "geometry.h"
#include "roam.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/PREC8/SLAT,SLON
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC10/NWINS(MCOM,MS)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      character outs*124

      SLAT=sitelat
      SLON=sitelongdif
      NCONST(ICOMP)=NSUR
      DO 20 I=1,NSUR
        NELTS(ICOMP,I)=NE(I)
        NGAPS(ICOMP,I)=NAIRG(I)
        NG=NAIRG(I)
        IF(NG.EQ.0)goto 1
        DO 30 J=1,NG
          NPGAP(ICOMP,I,J)=IPAIRG(I,J)
          RGAPS(ICOMP,I,J)=RAIRG(I,J)
   30   CONTINUE
    1   NWINS(ICOMP,I)=0
        EI(ICOMP,I)=EMISI(I)
        EE(ICOMP,I)=EMISE(I)
        AI(ICOMP,I)=ABSI(I)
        AE(ICOMP,I)=ABSE(I)
   20 CONTINUE

c Trace output ?
      IF(ITRACE(14).EQ.1.AND.IZNTRC(ICOMP).EQ.1)THEN
        write(outs,'(A,I4,a,i3,2x,a)')' Subroutine MZMISC Trace output',
     &  ICNT,' Zone ',ICOMP,zname(ICOMP)
        call edisp(itu,outs)
        ICNT=ICNT+1
        call edisp(itu,' ')
        call edisp(itu,outs)
        write(outs,99941)SLAT,SLON
99941   format('Located at',F5.1,' latitude and ',F5.1,' longitude.')
        IF(IROAM.EQ.1)write(outs,'(A)')
     &    'Note that roaming is activated and these will change'
        call edisp(itu,outs)
        write(outs,9993)NCONST(ICOMP)
 9993   format(' No.of multilayered constructions = ',I2)
        call edisp(itu,outs)
        call edisp(itu,' Constr. No.of    No.of    Air Gap')
        call edisp(itu,'         Elements Air Gaps Position')
        NN=NCONST(ICOMP)
        DO 9992 I=1,NN
          I1=NELTS(ICOMP,I)
          I2=NGAPS(ICOMP,I)
          IF(I2.GT.0)goto 9991
          write(outs,9990)I,I1,I2
 9990     format(I5,I10,I9)
          call edisp(itu,outs)
          goto 9992
 9991     write(outs,9989)I,I1,I2,(NPGAP(ICOMP,I,J),J=1,I2)
 9989     format(I5,I10,I9,13I9)
          call edisp(itu,outs)
 9992   CONTINUE
        call edisp(itu,' ')
        call edisp(itu,' Constr. Internal External Internal External')
        call edisp(itu,'          Emiss.   Emiss.   Absorp.  Absorp.')
        call edisp(itu,' ')
        DO 9979 I=1,NN
          write(outs,9978)I,EI(ICOMP,I),EE(ICOMP,I),AI(ICOMP,I),
     &                AE(ICOMP,I)
 9978     format(I5,4F9.2)
          call edisp(itu,outs)
 9979   CONTINUE
      ENDIF
      RETURN
      END

c ******************** PRECOMP ********************
C << subroutine moved to eroper.F >>
C PRECOMP calculates the apparent and reactive rectangular component
C of a power comsuming load.
C Inputs are:
C   PWR - Real power consumption in W
C   PF  - Power factor of load (0.-1.)
C   IPF - Determines whether load in leading 1 lagging -1 or unity 0
C Outputs are:
C   PA - Apparent power component VA
C   PQ - Reactive power component +/- VAr
C Lagging currents return a positive Q Leading currents 
C return a negative value of Q.

C      subroutine PRECOMP(PWR,PF,IPF,PA,PQ)
      
C      real PWR,PF,PA,PQ
C      logical close

C      call eclose(PF,0.00,0.0001,close)
C      if(close)then
C        PF = 1.0
C        PA = PWR
C        PQ = 0.0
C      else

C Calculate the phase angle from PF in rads
C        PA=ACOS(PF)
      
C Calculate the rectangular co-ordinates
C        PQ=PWR*TAN(PA)*real(IPF)
C        PA=sqrt(PWR**2+PQ**2)
C      endif
      
C      RETURN
C      END
