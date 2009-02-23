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

C *********************************************************************
C This file contains the following 3D zone modelling routines:

C UD1TMP updates the temperature array.
C MZ3SU1 parially generates the coefficients for the 3D zone and ground
C        matrix.
C MZ3SU2 completes the setting-up of the time-dependent coefficients for
C        3D zones.
C MZ3SU3 completes the setting-up of the time-dependent coefficients for
C        3D ground.
C ENRGYO determines the present and future terms for outside surface
C        node.
C ENRGYI determines the present and future terms for inside surface
C        node.
C SHWV3D determines the total energy absorbed by a "tmc".
C LWRD3D creates the longwave terms for an internal surface node.
C VZHRPF determines the present and future values for internal longwave
C        radiation.
C UD2TMP updates the temperature file.
C FORW3D performs the forward reduction process for the zone matrix.
C CONT3D solves the equation (b1*t+b2*q=b3)in  terms of the associated
C        control function.
C SVQTMP saves the solution given by CONT3D in the temperature file.
C BACK3D performs the back substitution process after adapting the 
C        control function.
C FILL1D fills the 1D temperature and energy variables.
C *********************************************************************

C **************************    UD1TMP    *****************************
C UD1TMP updates the temperature array.
C *********************************************************************
      SUBROUTINE UD1TMP(ICOMP)
#include "building.h"

      COMMON/GR3D101/NNODT(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)

      IF(ICOMP.EQ.-1)THEN
        IAIR=NNODTG
      ELSE
        IAIR=NNODT(ICOMP)
      ENDIF
      IPLT=IAIR+1
      IPRT=IPLT+1
      DO 10 IND=1,IPLT
        READ(ITAQ,REC=IND,ERR=20)DUMM,T3F(IND)
   10 CONTINUE
      RETURN
   20 STOP 'error occured while reading temp & Q file.'
      END

C ****************************    MZ3SU1    ***************************
C MZ3SU1 parially generates the coefficients for the 3D zone and ground
C matrix. The following connections supported:

C type    supp.1    supp.2           comments
C ---------------------------------------------------------------------
C  11      IZ        IN    with 3D node IN in zone IZ.
C  21      IZ        IS    convective and radiative boundary similar to
C                          exterior boundary for surface IS in zone IZ.
C  22      IZ        IS    convective only boundary similar to exterier
C                          boundary for surface IS in zone IZ.
C  31      IZ        IS    convective and radiative boundary similar to
C                          interior boundary for surface IS in zone IZ.
C  32      IZ        IS    convective only boundary similar to interier
C                          boundary for surface IS in zone IZ.
C  >100    IS        IN    connected to 1D node in zone number (type-100)
C                          surface IS and node IN.

C NB: 1. for 3D ground, IZ= -1.
C     2. in interier convective and radiative boundaries, if the zone
C        number is not equal to the current zone number convective only
C        boundary will be assumed.
C *********************************************************************
      SUBROUTINE MZ3SU1(ICOMP)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &HCOF(MCOM,MS)

C Special for BCL09: Inject QNOPF at the outermost node IHPNDO of
C construction IHPCON of zone IHPZON as calculated from heat pipe model.
C QNOPF is the neagation of the summation of the present and future
C time-row flux injections as applied to the innermost node to
C which the heat pipe connects.
      COMMON/BCL9/IHPZON,IHPCON,IHPNDO,QNOPF,QHPSV
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/GR1D04/GAM,RGAM
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      COMMON/GR3D101/NNODT(MCOM)
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GRND100/GRND3D
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      COMMON/GR3D132/T3F(MNOD3)

      LOGICAL BLDG3D,ZONE3D,GRND3D

      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDNDD,LGDTAQ

      SML3=1.E-8

C The augmented zone matrix configuration is as follows:
C The equation for the i-th node is located at the i-th row.
C The equation for air node is located at the X-th row, where
C (X = number of nodes in the zone).
C The self-coupling coefficient for the i-th node is located
C at the i-th row and the i-th column.
C The cross-coupling coefficient for i-th node with the j-th 
C node is located at the i-th row and the j-th column.
C The plant term is located at the Y-th column (Y = X + 1).
C The present term is located at the Z-th column (Z = Y + 1).
      DO 80 I=1,IAIR
        IFST(I)=0
   80 CONTINUE
      DO 90 I=1,NZMCF
        NXT(I)=0
   90 CONTINUE
      IAVL=1
      NXT(1)=2

C Set up the coefficients (or partial coefficients) related to the 
C connections.
      SUMA1=0.
      SUMA2=0.
      SUMA3=0.
      IF(ICOMP.EQ.-1)THEN
        ICT=1
      ELSE
        ICT=0
      ENDIF
   10 ICT=ICT+1
      READ(ICNC,REC=ICT,IOSTAT=ISTAT,ERR=4)ND1,DST,A1,CND,ITP,I1C,I2C

C Complete the matrix coefficients generation process.
      IF(ISTAT.LT.0)THEN
        IF(ICOMP.EQ.-1)THEN
          CALL MZ3SU3
        ELSE
          CALL MZ3SU2(ICOMP,SUMA1,SUMA2,SUMA3)
        ENDIF
        RETURN
      ENDIF
      T3F1=T3F(ND1)

C *** FIRST, connection is between two nodes within the multilayered
C constructions.
      IF(ITP.EQ.11)THEN

C Define the cross-coupled node.
        IZNC=I1C
        ND2=I2C
        TERM=CND*A1/DST

C For connections with nodes within the same zone.
        IF(IZNC.EQ.ICOMP)THEN
          T3F2=T3F(ND2)

C Future cross-coupling term (first node).
          CALL CFFVLU(ND1,ND2,CFV,IPRV,LCTN)
          FCSC=CFV-TERM*GAM
          CALL UPDZMX(ND1,ND2,FCSC,IPRV,LCTN)

C Future self-coupling term (first node).
          CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
          FSLF=CFV+TERM*GAM
          CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term (first node).
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERM*RGAM*(T3F2-T3F1)
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)

C Future cross-coupling term (second node).
          CALL CFFVLU(ND2,ND1,CFV,IPRV,LCTN)
          FCSC=CFV-TERM*GAM
          CALL UPDZMX(ND2,ND1,FCSC,IPRV,LCTN)

C Future self-coupling term (second node).
          CALL CFFVLU(ND2,ND2,CFV,IPRV,LCTN)
          FSLF=CFV+TERM*GAM
          CALL UPDZMX(ND2,ND2,FSLF,IPRV,LCTN)

C Present term (second node).
          CALL CFFVLU(ND2,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERM*RGAM*(T3F1-T3F2)
          CALL UPDZMX(ND2,IPRT,PTERM,IPRV,LCTN)

C For connections with nodes in the 3D ground model.
        ELSEIF(IZNC.EQ.-1.AND.GRND3D)THEN
          CALL EFOPRAN(ITF3,LGDTAQ,3,3,IER)
          READ(ITF3,REC=ND2,IOSTAT=ISTAT,ERR=6)T3P2,T3F2
          CALL ERPFREE(ITF3,ISTAT)

C Present term (first node).
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERM*(GAM*T3F2+RGAM*(T3P2-T3F1))
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)

C For connections with nodes within other zones in the building.
        ELSEIF(ZONE3D(IZNC))THEN
          CALL EFOPRAN(ITF3,L3DTAQ(IZNC),3,3,IER)
          READ(ITF3,REC=ND2,IOSTAT=ISTAT,ERR=6)T3P2,T3F2
          CALL ERPFREE(ITF3,ISTAT)

C Future self-coupling term (first node).
          CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
          FSLF=CFV+TERM*GAM
          CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term (first node).
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERM*(GAM*T3F2+RGAM*(T3P2-T3F1))
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
        ELSE
          STOP 'connection to a node in undefined zone.'
        ENDIF

C *** SECOND, connection with boundary similar to the external boundary
C for an existing surface in the building.
      ELSEIF(ITP.EQ.21.OR.ITP.EQ.22)THEN

C First set adjacent (internal or external) temperatures.
        IZNO=I1C
        ISFO=I2C
        TT1=ATP(IZNO,ISFO)
        TT2=ATF(IZNO,ISFO)

C Adiabatic case if IE(IZNO,ISFO) has been set to -5 in MZADJC.
        IF(IE(IZNO,ISFO).EQ.-5)GOTO 10

C Update the related coefficients.
        TERMP=HCOP(IZNO,ISFO)*A1*RGAM
        TERMF=HCOF(IZNO,ISFO)*A1*GAM

C Future self-coupling term.
        CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
        FSLF=CFV+TERMF
        CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)
        IF(ITP.EQ.21)THEN

C Special for bcl09.
          IF(IZNO.EQ.IHPZON.AND.ISFO.EQ.IHPCON)THEN
            QHPNOP=QNOPF-QHPSV
            QHPNOF=QHPSV
          ELSE
            QHPNOP=0.
            QHPNOF=0.
          ENDIF
          QTRMP=A1*RGAM
          QTRMF=A1*GAM
          CALL ENRGYO(IZNO,ISFO,QHPNOP,QHPNOF,Q3PTOT,Q3FTOT)
        ELSE
          Q3PTOT=0.
          Q3FTOT=0.
        ENDIF

C Present term.
        CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
        PTERM=CFV+TERMP*(TT1-T3F1)+TERMF*TT2+QTRMP*Q3PTOT+QTRMF*Q3FTOT
        CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)

C *** THIRD, connection with boundary similar to the internal boundary
C for an existing surface in the building.
      ELSEIF(ITP.EQ.31.OR.ITP.EQ.32)THEN

C Define the zone and internal surface.
        IZNI=I1C
        ISFI=I2C

C The surface is in the current zone.
        IF(IZNI.EQ.ICOMP.AND.ICOMP.GT.0)THEN

C Update the related coefficients.
          TERMP=HCIP(ICOMP,ISFI)*A1*RGAM
          TERMF=HCIF(ICOMP,ISFI)*A1*GAM

C Convective term.
          CONVCT=-TERMF

C For the surface node equation.
          CALL CFFVLU(ND1,IAIR,CFV,IPRV,LCTN)
          CALL UPDZMX(ND1,IAIR,CONVCT,IPRV,LCTN)

C For the air node equation.
          CALL CFFVLU(IAIR,ND1,CFV,IPRV,LCTN)
          CALL UPDZMX(IAIR,ND1,CONVCT,IPRV,LCTN)

C Sum up the convective terms to be used in the air node equation 
C future cross coupling term.
          SUMA1=SUMA1+TERMF
          SUMA2=SUMA2+TERMP*T3F1
          SUMA3=SUMA3+TERMP

          IF(ITP.EQ.31)THEN
            QTRMP=A1*RGAM
            QTRMF=A1*GAM

C Estimate the radiation terms.
            CALL LWRD3D(ICOMP,ISFI,ND1,QTRMF,SUM)
            CALL VZHRPF(ICOMP,ISFI,ND1,ZHR3P,ZHR3F)
            CALL ENRGYI(ICOMP,ISFI,SUM,Q3PTOT,Q3FTOT)
          ELSE
            QTRMP=0.
            QTRMF=0.
          ENDIF

C Future self-coupling term.
          CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
          FSLF=CFV+TERMF+QTRMF*ZHR3F
          CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term.
          T3FA=T3F(IAIR)
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV-(TERMP+QTRMP*ZHR3P)*T3F1+TERMP*T3FA+QTRMP*Q3PTOT+
     &          QTRMF*Q3FTOT
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)

C The surface is in another zone.
        ELSEIF(IZNI.NE.ICOMP.AND.IZNI.GT.0.AND.IZNI.LE.NCOMP)THEN
          IAIR2=NNODT(IZNI)

C Update the related coefficients.
          TERMP=HCIP(IZNI,ISFI)*A1*RGAM
          TERMF=HCIF(IZNI,ISFI)*A1*GAM

C Read the 2nd zone's air node temperature.
          CALL EFOPRAN(ITF3,L3DTAQ(IZNI),3,3,IER)
          READ(ITF3,REC=IAIR2,IOSTAT=ISTAT,ERR=6)T3PA2,T3FA2
          CALL ERPFREE(ITF3,ISTAT)

C Future self-coupling term.
          CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
          FSLF=CFV+TERM*GAM
          CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term.
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+TERMP*(T3PA2-T3F1)+TERMF*T3FA2
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
        ELSE
          STOP 'undefined connection type.'
        ENDIF

C *** FORTH, connection with a node in a 1D zone.
      ELSEIF(ITP.GT.100)THEN
        IZNC=ITP-100
        IF(IZNC.GT.NCOMP)STOP 'referance to un-existing zone.'
        ISFC=I1C
        IF(ISFC.EQ.0)THEN
          T3P2=TPA(IZNC)
          T3F2=TFA(IZNC)
        ELSE
          INDC=I2C
          IF(INDC.EQ.0)THEN
            T3P2=TPS(IZNC,ISFC)
            T3F2=TFS(IZNC,ISFC)
          ELSE
            T3P2=TPC(IZNC,ISFC,INDC)
            T3F2=TFC(IZNC,ISFC,INDC)
          ENDIF
        ENDIF
        TERM=CND*A1/DST

C Future self-coupling term (first node).
        CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
        FSLF=CFV+TERM*GAM
        CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term (first node).
        CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
        PTERM=CFV+TERM*RGAM*(T3P2-T3F1)+TERM*GAM*T3F2
        CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
      ELSE
        STOP 'undefined connection type.'
      ENDIF
      GOTO 10
    4 STOP ' error detected in the connections file.'
    6 STOP ' error detected in the 3D temporary file.'
      END

C *************************    MZ3SU2    ******************************
C MZ3SU2 completes the setting-up of the time-dependent coefficients for
C 3D zones.
C *********************************************************************
      SUBROUTINE MZ3SU2(ICOMP,SUMA1,SUMA2,SUMA3)
#include "building.h"
#include "geometry.h"
#include "control.h"

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE34D/QCASR(2),QCASC(2)
      COMMON/COE35/ZAUGEP,ZAUGIP,ZAUGEF,ZAUGIF,QWCP,XX1,ZGIPM,ZGIFM
      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM

      COMMON/CCTL/ICASCF(MCOM)

      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)
      COMMON/GR1D04/GAM,RGAM
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GR3D150/INDX3,DTIME

C For the current zone determine the associated control function.
        IC=ICASCF(ICOMP)

C Complete the matrix coefficients generation process.
      ICV=0
   50 ICV=ICV+1
      READ(ICVS,REC=ICV,IOSTAT=ISTAT,ERR=5)ND1,VOLM,DNHC,IS,IL

C Set-up air point equation coefficients.
      IF(ISTAT.LT.0)THEN
        GOTO 60
      ELSEIF(DNHC.GT.1.E-8)THEN

C Future self-coupling term.
        CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
        FSLF=CFV+DNHC*VOLM/DTIME
        CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term.
        T3F1=T3F(ND1)
        CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
        PTERM=CFV+DNHC*VOLM*T3F1/DTIME
        CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)

C Add the shortwave absorption for tmc nodes.
        IF(IS.GT.0.AND.ITMCFL(ICOMP,IS).NE.0)THEN
          CALL SHWV3D(ICOMP,IS,IL,VOLM,Q3PTOT,Q3FTOT)
          CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
          PTERM=CFV+Q3PTOT*RGAM+Q3FTOT*GAM
          CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
        ENDIF
      ENDIF

C If current node is a plant interaction point set coefficient.
      IF(NCF.GT.0.AND.IC.GT.0.AND.
     &   (IBAN(IC,1).EQ.ICOMP.OR.IBAN(IC,1).EQ.0).AND.
     &    IBAN(IC,3).EQ.ND1.AND.ND1.NE.IAIR)THEN
        PLANT=-GAM
        CALL CFFVLU(ND1,IPLT,CFV,IPRV,LCTN)
        CALL UPDZMX(ND1,IPLT,PLANT,IPRV,LCTN)
        CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
        Q3FC=T3F(IPLT)
        PTERM=CFV+Q3FC*RGAM
        CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
      ELSE
        Q3FC=0.
      ENDIF
      GOTO 50

C Set-up air point equation coefficients.
   60 RCV=1188*VOL(ICOMP)

C Future self coupling term.
      FTERM=RCV/DTIME+SUMA1+(ZAUGEF+ZAUGIF+CVIF+CVVF)*GAM
      CALL CFFVLU(IAIR,IAIR,CFV,IPRV,LCTN)
      CALL UPDZMX(IAIR,IAIR,FTERM,IPRV,LCTN)

C If air node is plant interaction point set coefficient.
      IF(NCF.GT.0.AND.IC.GT.0.AND.
     &  ((IBAN(IC,1).EQ.ICOMP.AND.IBAN(IC,2).EQ.0).OR.
     &  (IBAN(IC,1).EQ.0.AND.IBAN(IC,2).EQ.0).OR.
     &  (IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).EQ.ICOMP)))THEN
        PLANT=-GAM
        CALL CFFVLU(IAIR,IPLT,CFV,IPRV,LCTN)
        CALL UPDZMX(IAIR,IPLT,PLANT,IPRV,LCTN)
        Q3FA=T3F(IPLT)
      ELSE
        Q3FA=0.
      ENDIF

C Present term.
      T3FA=T3F(IAIR)
      PTERM=SUMA2+(RCV/DTIME-SUMA3)*T3FA+RGAM*
     &      (Q3FA+QCASC(1)+QWCP+QVNP+QPLTP(ICOMP)*
     &      CONV(ICOMP))+GAM*(XX1+XX3+QCASC(2))
      CALL CFFVLU(IAIR,IPRT,CFV,IPRV,LCTN)
      CALL UPDZMX(IAIR,IPRT,PTERM,IPRV,LCTN)
      RETURN
    5 STOP ' error detected in the control volumes file.'
      END

C *************************    MZ3SU3    ******************************
C MZ3SU3 completes the setting-up of the time-dependent coefficients for
C 3D ground.
C *********************************************************************
      SUBROUTINE MZ3SU3
#include "building.h"

      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)
      COMMON/GR3D150/INDX3,DTIME

C Complete the matrix coefficients generation process.
      ICV=0
   10 ICV=ICV+1
      READ(ICVS,REC=ICV,IOSTAT=ISTAT,ERR=5)ND1,VOLM,DNHC

C Set-up air point equation coefficients.
      IF(ISTAT.LT.0)THEN
        RETURN
      ELSEIF(DNHC.GT.1.E-8)THEN

C Future self-coupling term.
        CALL CFFVLU(ND1,ND1,CFV,IPRV,LCTN)
        FSLF=CFV+DNHC*VOLM/DTIME
        CALL UPDZMX(ND1,ND1,FSLF,IPRV,LCTN)

C Present term.
        T3F1=T3F(ND1)
        CALL CFFVLU(ND1,IPRT,CFV,IPRV,LCTN)
        PTERM=CFV+DNHC*VOLM*T3F1/DTIME
        CALL UPDZMX(ND1,IPRT,PTERM,IPRV,LCTN)
      ENDIF
      GOTO 10
    5 STOP ' error detected in the control volumes file.'
      END

C ***************************    ENRGYO    ****************************
C ENRGYO determines the present and future terms for outside surface
C node.
C *********************************************************************
      SUBROUTINE ENRGYO(IZ,IS,QHPNOP,QHPNOF,Q3PTOT,Q3FTOT)
#include "building.h"

      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE33/QELWP(MS),QELWF(MS)
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      COMMON/GR3D102/ASURFO(MCOM,MS),ASURFI(MCOM,MS)

      Q3PTOT=QSOLE(IS,1)+QELWP(IS)+ARP(IZ,IS)+QHPNOP
      Q3FTOT=QSOLE(IS,2)+QELWF(IS)+ARF(IZ,IS)+QHPNOF
      IF(IZ.GT.0)THEN
        Q3PTOT=ASURFO(IZ,IS)*Q3PTOT
        Q3FTOT=ASURFO(IZ,IS)*Q3FTOT
      ENDIF
      RETURN
      END

C ***************************    ENRGYI    ****************************
C ENRGYI determines the present and future terms for inside surface
C node.
C *********************************************************************
      SUBROUTINE ENRGYI(ICOMP,I1S,SUM,Q3PTOT,Q3FTOT)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC14/emarea(MCOM)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE34D/QCASR(2),QCASC(2)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)
      COMMON/GR3D102/ASURFO(MCOM,MS),ASURFI(MCOM,MS)

      ARIA=ASURFI(ICOMP,I1S)*EI(ICOMP,I1S)/EMAREA(ICOMP)
      QCASRP=QCASR(1)*ZOA(ICOMP)*ARIA
      QCASRF=QCASR(2)*ZOA(ICOMP)*ARIA
      PADDR=QPLTP(ICOMP)*(1.-CONV(ICOMP))*ARIA
      Q3PTOT=ASURFI(ICOMP,I1S)*QSOLI(I1S,1)+SUM+QCASRP+PADDR
      Q3FTOT=ASURFI(ICOMP,I1S)*QSOLI(I1S,2)+QCASRF
      RETURN
      END

C *************************    SHWV3D    ******************************
C SHWV3D determines the total energy absorbed by a "tmc".
C *********************************************************************
      SUBROUTINE SHWV3D(ICOMP,IS,IL,VOLM,Q3PTOT,Q3FTOT)
#include "building.h"

      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      IF(IL.GT.0)THEN
        REM=AMOD(FLOAT(IL),2.)
        IF(REM.LT.1.E-4)THEN
          ILYR=INT(IL/2)
          QTERM=VOLM/THRMLI(ICOMP,IS,ILYR,4)
          Q3PTOT=QTERM*QTMCA(IS,IL,1)*2.
          Q3FTOT=QTERM*QTMCA(IS,IL,2)*2.
        ELSE
          Q3PTOT=0.
          Q3FTOT=0.
        ENDIF
      ELSE
        Q3PTOT=0.
        Q3FTOT=0.
      ENDIF
      RETURN
      END
      
C *************************      LWRD3D      **************************
C LWRD3D creates the longwave terms for an internal surface node.
C *********************************************************************
      SUBROUTINE LWRD3D(ICOMP,IS,ND1,QTRMF,SUM)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D111/NTND1(MCOM),NTND2(MCOM),N1ND1(MCOM,MS)
      COMMON/GR3D132/T3F(MNOD3)

C Define the records in longwave file belonging to this surface.
      IF1ND=N1ND1(ICOMP,IS)
      IF(IS.EQ.NCONST(ICOMP))THEN
        IL1ND=NTND1(ICOMP)
      ELSE
        IL1ND=N1ND1(ICOMP,IS+1)-1
      ENDIF
      SUM=0.
      DO 20 IR1=IF1ND,IL1ND
        READ(ITLW,REC=IR1,ERR=40)NOD1,DUMM,DUMM,DUMM,IF2ND
        IF(NOD1.EQ.ND1)THEN
          IF(IR1.EQ.NTND1(ICOMP))THEN
            IL2ND=NTND2(ICOMP)
          ELSE
            READ(ITLW,REC=IR1+1,ERR=40)NDUMM,DUMM,DUMM,DUMM,IL2NDP1
            IL2ND=IL2NDP1-1
          ENDIF
          DO 30 IR2=IF2ND,IL2ND
            READ(ILWV,REC=IR2,ERR=50)NOD1,NOD2,HRP21,HRF21
            SUM=SUM+HRP21*T3F(NOD2)
            RADIAT=-QTRMF*HRF21
            CALL CFFVLU(ND1,NOD2,CVF,IPRV,LCTN)
            CALL UPDZMX(ND1,NOD2,RADIAT,IPRV,LCTN)
   30     CONTINUE
          RETURN
        ENDIF
   20 CONTINUE
      STOP ' error in longwave file.'
   40 STOP ' error while reading total longwave file.'
   50 STOP ' error while reading longwave file.'
      END

C *************************    VZHRPF    ******************************
C VZHRPF determines the present and future values for internal longwave
C radiation.
C *********************************************************************
      SUBROUTINE VZHRPF(ICOMP,I1S,ND1,ZHR3P,ZHR3F)
#include "building.h"

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D111/NTND1(MCOM),NTND2(MCOM),N1ND1(MCOM,MS)

      IFR=N1ND1(ICOMP,I1S)
      IF(I1S.EQ.NCONST(ICOMP))THEN
        ILR=NTND1(ICOMP)
      ELSE
        ILR=N1ND1(ICOMP,I1S+1)-1
      ENDIF
      DO 10 IR=IFR,ILR
        READ(ITLW,REC=IR,IOSTAT=ISTAT,ERR=20)NOD1,DUMM,ZHR3P,ZHR3F
        IF(ISTAT.LT.0)STOP 'missed connection in total longwave file.'
        IF(NOD1.EQ.ND1)RETURN
   10 CONTINUE
      STOP 'missed connection in the total longwave file.'
   20 STOP 'error while reading the total longwave file.'
      END

C *************************    FORW3D    ******************************
C FORW3D performs the forward reduction process for the zone matrix.
C *********************************************************************
      SUBROUTINE FORW3D(ICOMP,ISNSR,B1,B2,B3)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/CCTL/ICASCF(MCOM)

      COMMON/GR3D122/W3T(MNOD3T)
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF

      DOUBLE PRECISION FIJ,DW3T

c      NZMCFS=NZMCF
      IWRNG=0
      SML3=1.E-6

C Define the location of the control sensor.
      IF(ICOMP.GT.0)THEN
        IIC=ICASCF(ICOMP)
        IF(NCF.NE.0.AND.IIC.GT.0.AND.IBSN(IIC,1).EQ.ICOMP.AND.
     &     IBSN(IIC,2).GT.0.AND.IBSN(IIC,2).LT.IAIR)THEN
          ISNSR=IBSN(IIC,2)
        ELSE
          ISNSR=IAIR
        ENDIF
       ELSE
         ISNSR=IAIR
       ENDIF

C Perform forward reduction.
      DO 30 IEQ=1,IAIR-1

C Define the pivoting column (j) and element (Ajj).
        JTH=-999
        AJJ=0.
        ICFF=IFST(IEQ)
        IF(ICFF.EQ.0)STOP 'no unique solution exists.'
   20   ICLMN=NCLMN(ICFF)
        IF(ICLMN.GT.0.AND.ICLMN.NE.ISNSR.AND.ICLMN.LE.IAIR.AND.
     &    ABS(W3(ICFF)).GT.ABS(AJJ))THEN
          AJJ=W3(ICFF)
          JTH=ICLMN
        ENDIF
        ICFF=NXT(ICFF)
        IF(ICFF.GT.0)GOTO 20
        IF(JTH.EQ.-999)STOP 'no unique solution exists.'

C Perform elimination.
        DO 50 ITH=IEQ+1,IAIR

C Check if this row requires elimination.
          CALL CFFVLU(ITH,JTH,AIJ,IPRV,LCTN)
          IF(LCTN.EQ.0)GOTO 50
          FIJ=-AIJ/AJJ
          ICFF=IFST(IEQ)
   40     ICLMN=NCLMN(ICFF)
          DW3T=FIJ*W3(ICFF)
          W3T(ICLMN)=SNGL(DW3T)
          ICFF=NXT(ICFF)
          IF(ICFF.GT.0)GOTO 40

          ICF2=IFST(ITH)
          IF(ICF2.EQ.0)STOP 'no unique solution exists.'
          IPRV=0
   60     ICLMN=NCLMN(ICF2)
          ICF3=NXT(ICF2)
          W3NEW=W3(ICF2)+W3T(ICLMN)
          W3T(ICLMN)=0.
          IF(ICLMN.EQ.JTH)THEN
            IF(ABS(W3NEW).GT.1.E-2)THEN
              IWRNG=IWRNG+1
              IF(IWRNG.GT.10)THEN
                IWRNG=0
                CALL EDISP(IUOUT,
     &           'WARNING: possibility for mathematical instability.')
              ENDIF
            ENDIF
            W3NEW=0.
          ENDIF
          CALL UPDZMX(ITH,ICLMN,W3NEW,IPRV,ICF2)
          ICF2=ICF3
          IF(ICF2.GT.0)GOTO 60

          ICFF=IFST(IEQ)
   70     ICLMN=NCLMN(ICFF)
          IF(ABS(W3T(ICLMN)).GT.SML3)THEN
            W3NEW=W3T(ICLMN)
            W3T(ICLMN)=0.
            ICF2=0
            CALL UPDZMX(ITH,ICLMN,W3NEW,IPRV,ICF2)
          ENDIF
          ICFF=NXT(ICFF)
          IF(ICFF.GT.0)GOTO 70
   50   CONTINUE
   30 CONTINUE

C Save the lst equation terms (i.e., b1:temperature, b2:plant,
C b3:present).
C Define the coefficients belonging to the air node equation.
      INDX1=0
      INDX2=0
      INDX3=0
      B1=0.
      B2=0.
      B3=0.
      ICFF=IFST(IAIR)
  120 ICLMN=NCLMN(ICFF)
      IF(ICLMN.EQ.ISNSR.AND.INDX1.EQ.0)THEN
        INDX1=1
        B1=W3(ICFF)
      ELSEIF(ICLMN.EQ.IPLT.AND.INDX2.EQ.0)THEN
        INDX2=1
        B2=W3(ICFF)
      ELSEIF(ICLMN.EQ.IPRT.AND.INDX3.EQ.0)THEN
        INDX3=1
        B3=W3(ICFF)
      ELSE
        STOP 'no unique solution exists.'
      ENDIF
      ICFF=NXT(ICFF)
      IF(ICFF.GT.0)GOTO 120

c      write(*,*)'FORW3D END :',NZMCFS,NZMCF

      IF(INDX1.EQ.0)STOP 'no unique solution exists.'
      RETURN
      END

C **************************    UD2TMP    *****************************
C UD2TMP updates the temperature file.
C *********************************************************************
      SUBROUTINE UD2TMP
#include "building.h"

      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)

      T3FD=-999.0
      DO 10 IND=1,IPLT
        WRITE(ITAQ,REC=IND,ERR=30)T3F(IND),T3FD
        T3F(IND)=T3FD
   10 CONTINUE
      RETURN
   30 STOP 'error occured while writing to temp & Q file.'
      END

C **************************    CONT3D    *****************************
C CONT3D solves the equation (b1*t+b2*q=b3)in  terms of the associated
C control function.
C *********************************************************************
      SUBROUTINE CONT3D(ICOMP,ISNSR,B1,B2,B3)
#include "building.h"
#include "control.h"

      common/outin/iuout,iuin
      common/pophelp/h(60)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/cctl/icascf(mcom)

      common/pstflg/icflg1(mcf),icflg2(mcf)
      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut
      common/tpmodx/iperx,idtypx

      common/setuq/qpltp(mcom),qpltf(mcom),conv(mcom)

      character outs*124,h*72
      logical OK,dok

      BB1=B1
      BB2=B2
      BB3=B3
      IICOMP=ICOMP
      TNP=B3/B1

C Initialise first pass through and assume no control
C irrespective of control data.
      IF(NSINC.EQ.1)THEN
        DO 10 I=1,MCF
          ICFLG1(I)=0
          ICFLG2(I)=0
   10   CONTINUE
        CALL SVQTMP(ICOMP,ISNSR,TNP,0.)
        RETURN
      ENDIF

C Test for control.
      ICF=ICASCF(ICOMP)
      IF(ICF.EQ.0)THEN
        CALL SVQTMP(ICOMP,ISNSR,TNP,0.)
        RETURN
      ENDIF
      CONV(ICOMP)=FLOAT(IBAN(ICF,3))/100.
      IF(IHRP.EQ.24)THEN
        IDAY=IDYF
      ELSE
        IDAY=IDYP
      ENDIF
      N=NBCDT(ICF)
      IF(N.EQ.0)THEN
        IF(IDWP.EQ.6)THEN
          IDTYP=2
        ELSEIF(IDWP.EQ.7)THEN
          IDTYP=3
        ELSE
          IDTYP=1
        ENDIF
      IF(IDAY.LE.IBCDV(ICF,IDTYP,2).AND.IDAY.GE.
     &     IBCDV(ICF,IDTYP,1))GOTO 4
      ELSE
        DO 20 I=1,N
          IDTYP=I
          IF(IDAY.LE.IBCDV(ICF,I,2).AND.IDAY.GE.IBCDV(ICF,I,1))GOTO 4
   20   CONTINUE
      ENDIF

C Error!
      IF(ICFLG1(ICF).NE.1)THEN
        write(outs,'(a,i3,a)') ' Control loop',icf,
     &   ' is being referenced outside its dates of validity.'
        call edisp(iuout,outs)
        icflg1(icf)=1
        dok=.true.
        h(1)='If you say yes, the zone(s) associated with this'
        h(2)='control loop will free-float for any days outwith'
        h(3)='the period of validity. If you say no the simulator'
        h(4)='will stop. '
        CALL ASKOK('Continue with no control if this condition occurs?',
     &             '(see help)',OK,dok,4)
        IF(.NOT.OK)then
          call epwait
          STOP
        endif
      ENDIF
      CALL SVQTMP(ICOMP,ISNSR,TNP,0.)
      RETURN

C Commence control solution.
    4 IF(BTIMEF.GT.24.)BTIMEF=BTIMEF-24.
      N=NBCDP(ICF,IDTYP)
      DO 30 I=1,N
        IPER=I
        IF((I.EQ.N.AND.BTIMEF.GT.TBCPS(ICF,IDTYP,N)).OR.
     &     (I.NE.N.AND.BTIMEF.LE.TBCPS(ICF,IDTYP,I+1)))GOTO 11
   30 CONTINUE

C Error!
      IF(ICFLG2(ICF).NE.1)THEN
        write(outs,'(a,i3,a)') ' Control loop',icf,
     &   ' is being referenced outside its dates of validity.'
        call edisp(iuout,outs)
        dok=.true.
        h(1)='If you say yes, the zone(s) associated with this'
        h(2)='control loop will free-float for any days outwith'
        h(3)='the period of validity. If you say no the simulator'
        h(4)='will stop. '
        CALL ASKOK('Continue with no control if this condition occurs?',
     &             '(see help)',OK,dok,4)
        icflg2(icf)=1
        IF(.NOT.OK)then
          call epwait
          STOP
        endif
      ENDIF
      CALL SVQTMP(ICOMP,ISNSR,TNP,0.)
      RETURN

C Set up day type and period for nested control loop.    
   11 IF(IBSN(ICF,4).NE.0)THEN
        JCF=IBSN(ICF,4)
        IF(NBCDT(JCF).EQ.0)THEN
          IF(IDWP.EQ.6)THEN
            IDTYPX=2
          ELSEIF(IDWP.EQ.7)THEN
            IDTYPX=3
          ELSE
            IDTYPX=1
          ENDIF
          IF(IDAY.LE.IBCDV(JCF,IDTYPX,2).OR.
     &       IDAY.GE.IBCDV(JCF,IDTYPX,1))GOTO 72
        ELSE
          DO 71 I=1,NBCDT(JCF)
            IDTYPX=I
            IF(IDAY.LE.IBCDV(ICF,I,2).AND.IDAY.GE.IBCDV(ICF,I,1))GOTO 72
   71     CONTINUE
        ENDIF
        write(outs,'(A,I3,A)')' Warning: Control function',jcf,
     &                        ' is being referenced'
        call edisp(iuout,outs)
        call edisp(iuout,' outside its dates of validity.')
        call edisp(iuout,' ')
        IDTYPX=1

C Set period.
   72   IF(BTIMEF.GT.24.)BTIMEF=BTIMEF-24.
        N=NBCDP(JCF,IDTYPX)
        DO 75 I=1,N
          IPERX=I
          IF((I.EQ.N.AND.BTIMEF.GT.TBCPS(JCF,IDTYPX,N)).OR.
     &       (I.NE.N.AND.BTIMEF.LT.TBCPS(JCF,IDTYPX,I+1)))GOTO 12
   75   CONTINUE

C Error: issue warning and set period 1.
        write(outs,'(A,I3,A)')' Warning: Control function',jcf,
     &                        ' is being referenced'
        call edisp(iuout,outs)
        call edisp(iuout,' outside its time of validity.')
        call edisp(iuout,' ')
        IPERX=1
      ENDIF
  
C Invoke appropriate control law.
   12 CALL M3BCTL
      TNP=TFUT
      QNP=QFUT

C Assign t and q values.
      CALL SVQTMP(ICOMP,ISNSR,TNP,QNP)
      RETURN
      END

C **************************    SVQTMP    *****************************
C SVQTMP saves the solution given by CONT3D in the temperature file.
C *********************************************************************
      SUBROUTINE SVQTMP(ICOMP,ISNSR,TNP,QNP)
#include "building.h"
#include "control.h"

      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)

      T3F(ISNSR)=TNP
      T3F(IPLT)=QNP
      IF(ICOMP.GT.0)THEN
        IC=ICASCF(ICOMP)
        IF(NCF.NE.0.AND.IC.NE.0.AND.
     &  ((IBAN(IC,1).EQ.ICOMP.AND.IBAN(IC,2).EQ.0).OR.
     &  (IBAN(IC,1).EQ.0.AND.IBAN(IC,2).EQ.0).OR.
     &  (IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).EQ.ICOMP)))THEN
          QFA(ICOMP)=QNP
        ENDIF
      ENDIF
      RETURN
      END

C *************************    BACK3D    ******************************
C BACK3D performs the back substitution process after adapting the 
C control function.
C *********************************************************************
      SUBROUTINE BACK3D
#include "building.h"

      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D130/IFST(MNOD3),W3(MCFF3),NCLMN(MCFF3),NXT(MCFF3),SML3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)

      LOGICAL DEFINED

      DOUBLE PRECISION PRESENT,DOMAIN,SUM,TSUM,DT3FN

      DO 10 IEQ=IAIR-1,1,-1
        DEFINED=.FALSE.
        SUM=0.
        PRESENT=0.
        DOMAIN=0.
        ICFF=IFST(IEQ)
        IF(ICFF.EQ.0)STOP 'no unique solution exists.'
   20   ICLMN=NCLMN(ICFF)

C Present term.
        IF(ICLMN.EQ.IPRT)THEN
          PRESENT=W3(ICFF)
        ELSE
          T3F1=T3F(ICLMN)

C Unknown node temperature.
          IF(ABS(T3F1+999.).LT.1.E-6)THEN
            IF(DEFINED)
     &      STOP 'Backward process: two unknowns in the same equation.'
            DEFINED=.TRUE.
            IND=ICLMN
            DOMAIN=W3(ICFF)
          ELSE
            TSUM=T3F1*W3(ICFF)
            SUM=SUM+TSUM
          ENDIF
        ENDIF
        ICFF=NXT(ICFF)
        IF(ICFF.GT.0)GOTO 20
        DT3FN=(PRESENT-SUM)/DOMAIN
        T3F(IND)=SNGL(DT3FN)
   10 CONTINUE

C Some computer systems assign end of file at the end of the last write
C statement.
      READ(ITAQ,REC=IPLT)T3MPLT
      DO 30 IND=IAIR,1,-1
        READ(ITAQ,REC=IND)T3MP
        WRITE(ITAQ,REC=IND)T3MP,T3F(IND)
   30 CONTINUE
      WRITE(ITAQ,REC=IPLT)T3MPLT,T3F(IPLT)
      RETURN
      END

C ****************************    FILL1D    ***************************
C FILL1D fills the 1D temperature and energy variables.
C *********************************************************************
      SUBROUTINE FILL1D(ICOMP)
#include "building.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GR3D131/IAIR,IPLT,IPRT,IAVL,NZMCF
      COMMON/GR3D132/T3F(MNOD3)

C Initialize the zone's temperatures.
      TFA(ICOMP)=T3F(IAIR)
      DO 10 IS=1,NCONST(ICOMP)
        TFS(ICOMP,IS)=0.
      DO 10 INE=1,NNDC(ICOMP,IS)
        TFC(ICOMP,IS,INE)=0.
   10 CONTINUE
      IC=0
   20 IC=IC+1
      READ(INDD,REC=IC,IOSTAT=ISTAT)ND1,IS1,ILCT,AVOLM

C End of file reached.
      IF(ISTAT.LT.0)THEN
        DO 30 IS=1,NCONST(ICOMP)
          NE=NELTS(ICOMP,IS)
          INE=0
          DO 40 IL=1,NE
            INE=INE+1
            TFC(ICOMP,IS,INE)=TFC(ICOMP,IS,INE)/SNA(ICOMP,IS)
            INE=INE+1
            VOLM=THRMLI(ICOMP,IS,IL,4)*SNA(ICOMP,IS)
            TFC(ICOMP,IS,INE)=TFC(ICOMP,IS,INE)/VOLM
   40     CONTINUE
          TFS(ICOMP,IS)=TFS(ICOMP,IS)/SNA(ICOMP,IS)
          INE=INE+1
          TFC(ICOMP,IS,INE)=TFS(ICOMP,IS)
   30   CONTINUE
        RETURN

      ELSE

C Internal surface node.
        IF(ILCT.LT.0)THEN
          TFS(ICOMP,IS1)=TFS(ICOMP,IS1)+T3F(ND1)*AVOLM

C External surface or enter-construction nodes.
        ELSE
          TFC(ICOMP,IS1,ILCT)=TFC(ICOMP,IS1,ILCT)+T3F(ND1)*AVOLM
        ENDIF
      ENDIF
      GOTO 20
      END
