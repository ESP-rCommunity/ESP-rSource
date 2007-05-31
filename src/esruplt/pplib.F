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

C Note IPCOMP is unused in each of the following subroutines.
C *********************** Primitive Part No. 1.1 ***********************

C Thermal Conduction (solid to solid)

      SUBROUTINE PP11(IPCOMP,ISTATS,Conn1,Conn2,
     &                Cmn,Cm,Cn,TmP,TnP,RMm,RMn)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling & cross-coupling coefficients
C   node 1 (solid m)
       A(11,1)=-Alpha*Cmn-RMm*Cm/(Conn1*Timsec)
       A(11,2)=Alpha*Cmn
C   node 2 (solid n)
       A(11,3)=Alpha*Cmn
       A(11,4)=-Alpha*Cmn-RMn*Cn/(Conn2*Timsec)

C Present-time-and-excitation coefficients
       B(11,1)=((1.-Alpha)*Cmn-RMm*Cm/(Conn1*Timsec))*TmP
     &          -(1.-Alpha)*Cmn*TnP

       B(11,2)=((1.-Alpha)*Cmn-RMn*Cn/(Conn2*Timsec))*TnP
     &          -(1.-Alpha)*Cmn*TmP

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling & cross-coupling coefficients
C   node 1 (solid m)
       A(11,1)=1./Conn1
       A(11,2)=0.
C   node 2 (solid n)
       A(11,3)=0.
       A(11,4)=1./Conn2

C Present-time-and-excitation coefficients
       B(11,1)=0.
       B(11,2)=0.

      END IF

      RETURN
      END

C *********************** Primitive Part No. 1.2 ***********************

C Thermal Conduction (with ambient solid block)

      SUBROUTINE PP12(IPCOMP,ISTATS,Conn1,
     &                Ces,CesP,Cs,CsP,TsP,TeP,RMs)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (solid s)
       A(12,1)=-Alpha*Ces-RMs*Cs/(Conn1*Timsec)

C Present-time-and-excitation coefficients
       B(12,1)=((1.-Alpha)*CesP-RMs*Cs/(Conn1*Timsec))*TsP
     &          -(Alpha*Ces*TeP+(1.-Alpha)*CesP*TeP)

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (solid s)
       A(12,1)=1./Conn1

C Present-time-and-excitation coefficients
       B(12,1)=0.

      END IF

      RETURN
      END

C *********************** Primitive Part No. 2.1 ***********************

C Surface Convection (with moist air)

      SUBROUTINE PP21(IPCOMP,ISTATS,Conn1,Conn2,
     &               Cas,CasP,Cs,CsP,Cav,CavP,Cpma,CpmaP,Cc,
     &               RMs,RMsP,RMa,RMaP,TaP,TsP)  

      INCLUDE 'plant.h'
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(21,1)=-Alpha*Cas-RMs*Cs/(Conn1*Timsec)
       A(21,2)=Alpha*Cas
C    node 2 (air in contact)
       A(21,3)=Alpha*Cas
       A(21,4)=-Alpha*Cas-RMa*Cpma/(Conn2*Timsec)
C Present-time-and-excitation coefficients
       B(21,1)=((1.-Alpha)*CasP-RMsP*CsP/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CasP*TaP + Alpha*Cav +(1.-Alpha)*CavP
       B(21,2)=-(1.-Alpha)*CasP*TsP
     &           +((1.-Alpha)*CasP-RMaP*CpmaP/(Conn2*Timsec))*TaP

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(21,1)=1./Conn1
       A(21,2)=0.
C    node 2 (fluid in contact)
       A(21,3)=0.
       A(21,4)=1./Conn2
C Present-time-and-excitation coefficients
       B(21,1)=0.
       IF(ISTATS.EQ.2) THEN
         B(21,2)=0.
       ELSE
	 B(21,2)=-Cc
       ENDIF
      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 2.2 ***********************

C Surface Convection (with 2-phase fluid)

      SUBROUTINE PP22(IPCOMP,ISTATS,Conn1,Conn2,
     &               Crs,CrsP,Cs,CsP,Cwv,CwvP,Cc,
     &               RMs,RMsP,TrP,TsP,Tsat)
      

      INCLUDE 'plant.h'
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(22,1)=-Alpha*Crs-RMs*Cs/(Conn1*Timsec)
       A(22,2)=Alpha*Crs
C    node 2 (fluid in contact)
       A(22,3)=0.
       A(22,4)=1.
C Present-time-and-excitation coefficients
       B(22,1)=((1.-Alpha)*CrsP-RMsP*CsP/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CrsP*TrP + Alpha*Cwv +(1.-Alpha)*CwvP
       B(22,2)=Tsat

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(22,1)=1./Conn1
       A(22,2)=0.
C    node 2 (fluid in contact)
       A(22,3)=0.
       A(22,4)=1./Conn2
C Present-time-and-excitation coefficients
       B(22,1)=0.
       IF(ISTATS.EQ.2) THEN
         B(22,2)=Cc
       ELSE
	 B(22,2)=-Cc
       ENDIF
      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 2.3 ***********************

C Surface Convection (with 1-phase fluid)

      SUBROUTINE PP23(IPCOMP,ISTATS,Conn1,Conn2,
     &               Csw,CswP,Cs,Cpw,CpwP,
     &               RMs,RMw,TwP,TsP)
      

      INCLUDE 'plant.h'
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(23,1)=-Alpha*Csw-RMs*Cs/(Conn1*Timsec)
       A(23,2)=Alpha*Csw
C    node 2 (fluid in contact)
       A(23,3)=Alpha*Csw
       A(23,4)=-Alpha*Csw-RMw*Cpw/(Conn2*Timsec)
C Present-time-and-excitation coefficients
       B(23,1)=((1.-Alpha)*CswP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CswP*TwP
       B(23,2)=-(1.-Alpha)*CswP*TsP
     &           +((1.-Alpha)*CswP-RMw*CpwP/(Conn2*Timsec))*TwP

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(23,1)=1./Conn1
       A(23,2)=0.
C    node 2 (fluid in contact)
       A(23,3)=0.
       A(23,4)=1./Conn2
C Present-time-and-excitation coefficients
       B(23,1)=0.
       B(23,2)=0.
      ENDIF

      RETURN
      END

C ********************** Primitive Part No. 2.4  ***********************

C Surface convection (with ambient)

      SUBROUTINE PP24(IPCOMP,ISTATS,Conn1,
     &                Ces,CesP,RMs,Cs,Te,TeP,TsP)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficient
C   node 1 (solid)
       A(24,1)=-Alpha*Ces-RMs*Cs/(Conn1*Timsec)

C Present-time-and-excitation coefficients
       B(24,1)=((1.-Alpha)*Ces-RMs*Cs/(Conn1*Timsec))*TsP
     &          -(Alpha*Ces*Te+(1.-Alpha)*CesP*TeP)

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(24,1)=1./conn1
C Present-time-and-excitation coefficients
       B(24,1)=0.

      END IF

      RETURN
      END

C *********************** Primitive Part No. 3.1 ***********************

C Surface Radiation (with local surface)

      SUBROUTINE PP31(IPCOMP,ISTATS,Conn1,Conn2,
     &                Cst,CstP,Cs,Ct,TsP,TtP,RMs,RMt)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling & cross-coupling coefficients
C   node 1 (solid surface S)
       A(31,1)=-Alpha*Cst-RMs*Cs/(Conn1*Timsec)
       A(31,2)=Alpha*Cst
C   node 2 (solid T)
       A(31,3)=Alpha*Cst
       A(31,4)=-Alpha*Cst-RMt*Ct/(Conn2*Timsec)

C Present-time-and-excitation coefficients
       B(31,1)=((1.-Alpha)*Cst-RMs*Cs/(Conn1*Timsec))*TsP
     &          -(1.-Alpha)*CstP*TtP

       B(31,2)=((1.-Alpha)*Cst-RMt*Ct/(Conn2*Timsec))*TtP
     &          -(1.-Alpha)*CstP*TsP

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling & cross-coupling coefficients
C   node 1 (solid S)
       A(31,1)=1./Conn1
       A(31,2)=0.
C   node 2 (solid T)
       A(31,3)=0.
       A(31,4)=1./Conn2

C Present-time-and-excitation coefficients
       B(31,1)=0.
       B(31,2)=0.

      END IF

      RETURN
      END

C ********************** Primitive Part No. 3.2  ***********************

C Surface radiation (with ambient surface)

      SUBROUTINE PP32(IPCOMP,ISTATS,Conn1,
     &                Ces,CesP,RMs,RMsP,Cs,CsP,Te,TeP,TsP)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficient
C   node 1 (solid)
       A(32,1)=-Alpha*Ces-RMs*Cs/(Conn1*Timsec)

C Present-time-and-excitation coefficients
       B(32,1)=((1.-Alpha)*CesP-RMsP*CsP/(Conn1*Timsec))*TsP
     &          -(Alpha*Ces*Te+(1.-Alpha)*CesP*TeP)

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(32,1)=1./conn1
C Present-time-and-excitation coefficients
       B(32,1)=0.

      END IF

      RETURN
      END

C *********************** Primitive Part No. 4.1 ***********************

C Flow (Moist Air) upon Surface

      SUBROUTINE PP41(IPCOMP,ISTATS,SURFACE,Conn1,Conn2,DELAY,Tam,Ta1,
     &               Cas,CasP,Ca0,Ca0P,Ca1,Ca1P,Cs,Cav,CavP,
     &               Cpam,CpamP,Cpa1,ho,Ao,Cd,CdP,Cdmax,
     &               RMs,RMa,Rmam,Rmvm,Ta0P,TamP,Ta1P,Ts,TsP)

      INCLUDE 'plant.h'
      CHARACTER SURFACE*3
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      logical dok

      IF(ISTATS.EQ.1) THEN

C Self-coupling coefficients
C node 1 (solids)
        A(41,1)=-Alpha*Cas-RMs*Cs/(Conn1*Timsec)
        A(41,2)=Alpha*Cas
C node 2 (air in contact)
        A(41,4)=Alpha*Cas
        A(41,5)=-Alpha*Cas-RMa*Cpam/(Conn2*Timsec)
        A(41,6)=-Alpha*Ca1
C node 3 (leaving air)
        A(41,8)=-1.
        A(41,9)=1.
C Cross-coupling coefficients
        A(41,11)=Alpha*Ca0
C Present-time-and-excitation coefficients
        B(41,1)=((1.-Alpha)*CasP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CasP*TamP
     &           +Alpha*Cav+(1.-Alpha)*CavP
        B(41,2)=-(1.-Alpha)*CasP*TsP
     &           +((1.-Alpha)*CasP-RMa*CpamP/(Conn2*Timsec))*TamP
     &           -(1.-Alpha)*Ca0P*Ta0P+(1.-Alpha)*Ca1P*Ta1P
        B(41,3)=0.

C REVISE THE FOLLOWING COEFFICIENTS IF TRANSPORT DELAY IS ACTIVE
        CALL ECLOSE(DELAY,1.0,0.001,dok)
        IF (dok) THEN
          A(41,4)=0.
          A(41,5)=1.
          A(41,6)=0.
          A(41,8)=0.
          A(41,11)=0.
          B(41,2)=Tam
          B(41,3)=Ta1
        ENDIF
      ELSE IF(ISTATS.EQ.2) THEN

C Self-coupling coefficients
C node 1 (solids)
        A(41,1)=1./Conn1
        A(41,2)=0.
C node 2 (air in contact)
        A(41,4)=0.
        A(41,5)=1./Conn2
        A(41,6)=0.
C node 3 (leaving air)
        A(41,8)=-1.
        A(41,9)=1.
C Cross-coupling coefficients
        A(41,11)=-1./Conn2
C Present-time-and-excitation coefficients
        B(41,1)=0.
        B(41,2)=0.
        B(41,3)=0.
      ELSE IF(ISTATS.EQ.3) THEN

C Self-coupling coefficients
C node 1 (solids)
        A(41,1)=1./Conn1
        A(41,2)=0.
C node 2 (air in contact)
        A(41,4)=0.
        A(41,5)=1./Conn2
        A(41,6)=0.
C node 3 (leaving air)
        A(41,8)=-1.
        A(41,9)=1.
C Cross-coupling coefficients
        A(41,11)=-1./Conn2
C Present-time-and-excitation coefficients
        B(41,1)=0.

        IF (SURFACE.EQ.'DRY') THEN
          Condens=0.
        ELSEIF(Cpam.NE.0.0.AND.Rmam.NE.0.0) THEN
          RKd=ho/Cpam
          Ga=Rmvm/Rmam
C Determine the saturated moist content at surface temperature
          Call SATMOIST(Ts,Gs)
          Condens=RKd*Ao*(Ga-Gs)
        END IF

C test for relatively dry incoming air
       if(condens.gt.rmvm) condens=rmvm

         Cd=CdP+Condens*TIMSEC
         IF (Cd.LT.0.) THEN
           Condens=-CdP 
           Cd=0.
         ELSE IF (Cd.GT.Cdmax) THEN
           Cd=Cdmax
         ENDIF

       B(41,2)= -Condens
       B(41,3)= 0.
      END IF

      RETURN
      END

C *********************** Primitive Part No. 4.2 ***********************

C Flow (Two-phase Fluid) upon Surface

      SUBROUTINE PP42(IPCOMP,ISTATS,Conn1,Conn2,Trm,Tr1,Tsat,PHASE,
     &                Crs,CrsP,Cr0,Cr0P,Cr1,Cr1P,Cs,Cpwm,CpwmP,Cpw1,
     &                RMs,RMw,RMwP,Tr0P,TrmP,Tr1P,TsP,Rmc)
      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(42,1)=-Alpha*Crs-RMs*Cs/(Conn1*Timsec)
       A(42,2)=Alpha*Crs
C   node 2 (fluid in contact)
       A(42,4)=Alpha*Crs
       A(42,5)=-Alpha*Crs-RMw*Cpwm/(Conn2*Timsec)
       A(42,6)=-Alpha*Cr1
C   node 3 (leaving fluid)
       A(42,8)=-1.
       A(42,9)=1.
C Cross-coupling coefficients
       A(42,11)=Alpha*Cr0
C Present-time-and-excitation coefficients
       B(42,1)=((1.-Alpha)*CrsP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CrsP*TrmP
       B(42,2)=-(1.-Alpha)*CrsP*TsP
     &           +((1.-Alpha)*CrsP-RMwP*CpwmP/(Conn2*Timsec))*TrmP
     &           +(1.-Alpha)*Cr1P*Tr1P-(1.-Alpha)*Cr0P*Tr0P
       B(42,3)=0.
C avoid iteration under wet steam condition
       if(PHASE.NE.1.) then
         A(42,2)=0
         B(42,1)=((1.-Alpha)*CrsP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CrsP*TrmP-Alpha*Crs*Tsat

         A(42,4)=0.
         A(42,5)=1.       
         A(42,6)=0.
         A(42,11)=0.
         B(42,2)=Trm

         A(42,8)=0.
         B(42,3)=Tr1
       endif

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(42,1)=1./conn1
       A(42,2)=0.
C   node 2 (fluid in contact)
       A(42,4)=0.
       A(42,5)=1./conn2
       A(42,6)=0.
C   node 3 (leaving fluid)
       A(42,8)=-1.
       A(42,9)=1.
C Cross-coupling coefficients
       A(42,11)=-1.
C Present-time-and-excitation coefficients
       B(42,1)=0.
       B(42,2)=Rmc
       B(42,3)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(42,1)=1./conn1
       A(42,2)=0.
C   node 2 (fluid in contact)
       A(42,4)=0.
       A(42,5)=1./conn2
       A(42,6)=0.
C   node 3 (leaving fluid)
       A(42,8)=-1.
       A(42,9)=1.
C Cross-coupling coefficients
       A(42,11)=-1.
C Present-time-and-excitation coefficients
       B(42,1)=0.
       B(42,2)=-Rmc
       B(42,3)=0.
      END IF

      RETURN
      END

C *********************** Primitive Part No. 4.3 ***********************

C Flow (Single-phase Fluid) upon Surface

      SUBROUTINE PP43(IPCOMP,ISTATS,Conn1,Conn2,DELAY,Twm,Tw1,
     &                Csw,CswP,Cw0,Cw0P,Cw1,Cw1P,Cs,Cpwm,CpwmP,Cpw1,
     &                RMs,RMw,Tw0P,TwmP,Tw1P,TsP)
      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
      logical close

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(43,1)=-Alpha*Csw-RMs*Cs/(Conn1*Timsec)
       A(43,2)=Alpha*Csw
C       A(43,3)=0.
C   node 2 (fluid in contact)
       A(43,4)=Alpha*Csw
       A(43,5)=-Alpha*Csw-RMw*Cpwm/(Conn2*Timsec)
       A(43,6)=-Alpha*Cw1
C   node 3 (leaving fluid)
C       A(43,7)=0.
       A(43,8)=-1.
       A(43,9)=1.
C Cross-coupling coefficients
C       A(43,10)=0.
       A(43,11)=Alpha*Cw0
C       A(43,12)=0.
C Present-time-and-excitation coefficients
       B(43,1)=((1.-Alpha)*CswP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CswP*TwmP
       B(43,2)=-(1.-Alpha)*CswP*TsP
     &           +((1.-Alpha)*CswP-RMw*CpwmP/(Conn2*Timsec))*TwmP
     &           +(1.-Alpha)*Cw1P*Tw1P-(1.-Alpha)*Cw0P*Tw0P
       B(43,3)=0.

C REVISE THE FOLLOWING COEFFICIENTS IF TRANSPORT DELAY IS ACTIVE
       call eclose(DELAY,1.00,0.0001,close)
       IF (close) THEN
         A(43,4)=0.
         A(43,5)=1.
         A(43,6)=0.
         A(43,8)=0.
         A(43,11)=0.
         B(43,2)=Twm
         B(43,3)=Tw1
        ENDIF

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(43,1)=1./conn1
       A(43,2)=0.
C   node 2 (fluid in contact)
       A(43,4)=0.
       A(43,5)=1./conn2
       A(43,6)=0.
C   node 3 (leaving fluid)
       A(43,8)=-1.
       A(43,9)=1.
C Cross-coupling coefficients
       A(43,11)=-1.
C Present-time-and-excitation coefficients
       B(43,1)=0.
       B(43,2)=0.
       B(43,3)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(43,1)=1./conn1
       A(43,2)=0.
C   node 2 (fluid in contact)
       A(43,4)=0.
       A(43,5)=1./conn2
       A(43,6)=0.
C   node 3 (leaving fluid)
       A(43,8)=0.
       A(43,9)=1.
C Cross-coupling coefficients
       A(43,11)=0.
C Present-time-and-excitation coefficients
       B(43,1)=0.
       B(43,2)=0.
       B(43,3)=0.
      END IF

      RETURN
      END

C *********************** Primitive Part No. 4.4 ***********************

C Flow (Moist Air) upon Surface: 2 node model

      SUBROUTINE PP44(IPCOMP,ISTATS,Surf,Conn1,Conn2,
     &               Cas,CasP,Ca0,Ca0P,Ca1,Ca1P,Cs,Cav,CavP,
     &               Cpam,CpamP,ho,Ao,Cd,CdP,
     &               RMs,RMa,Rmv0,Ga0,Ta0P,Ta1,Ta1P,Ts,TsP)     

      INCLUDE 'plant.h'
C      CHARACTER SURFACE*3
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 
      logical close1,close2

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(44,1)=-Alpha*Cas-RMs*Cs/(Conn1*Timsec)
       A(44,2)=Alpha*Cas
C    node 2 (air in contact)
       A(44,3)=Alpha*Cas
       A(44,4)=-Alpha*(Ca1+Cas)-RMa*Cpam/(Conn2*Timsec)
C Cross-coupling coefficients
       A(44,6)=Alpha*Ca0
C Present-time-and-excitation coefficients
       B(44,1)=((1.-Alpha)*CasP-RMs*Cs/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CasP*Ta1P
     &           +Alpha*Cav+(1.-Alpha)*CavP

       B(44,2)=-(1.-Alpha)*CasP*TsP
     &         +((1.-Alpha)*(Ca1P+CasP)-RMa*CpamP/(Conn2*Timsec))*Ta1P
     &         -(1.-Alpha)*Ca0P*Ta0P

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(44,1)=1./Conn1
       A(44,2)=0.

C    node 2 (air in contact)
       A(44,3)=0.
       A(44,4)=1./Conn2

C Cross-coupling coefficients
       A(44,6)=-1.

C Present-time-and-excitation coefficients
       B(44,1)=0.
       B(44,2)=0.


      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(44,1)=1./Conn1
       A(44,2)=0.

C    node 2 (air in contact)
       A(44,3)=0.
       A(44,4)=1./Conn2

C Cross-coupling coefficients
       A(44,6)=-1.
C Present-time-and-excitation coefficients
       B(44,1)=0.

       call eclose(Surf,0.00,0.0001,close1)
       call eclose(Cpam,0.00,0.0001,close2)
       IF (close1) THEN
         Condens=0.
       ELSEIF(.NOT.close2) THEN
         RKd=ho/Cpam
C Determine the saturated moist content at surface temperature
         Call SATMOIST(Ts,Gs)
         Condens=RKd*Ao*(Ga0-Gs)
       END IF

C Limit condensing rate for relatively dry incoming air
	if(condens.gt.rmv0) condens=rmv0

C Update amount of condensate at future time step
            Cd=CdP+Condens*TIMSEC
            IF (Cd.LT.0.) THEN
              Condens=-CdP/TIMSEC 
              Cd=0.
            ENDIF

       B(44,2)= -Condens
       B(44,3)= 0.
      END IF

      RETURN
      END


C *********************** Primitive Part No. 4.5 ***********************

C Flow (single-phase fluid) upon Surface: 2 node model

      SUBROUTINE PP45(IPCOMP,ISTATS,Surf,Conn1,Conn2,
     &               Csw,CswP,Cw0,Cw0P,Cw1,Cw1P,Cs,CsP,
     &               Cpw1,Cpw1P,
     &               RMs,RMsP,RMw,RMwP,Tw0P,Tw1P,Ts,TsP)     

      INCLUDE 'plant.h'

      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(45,1)=-Alpha*Csw-RMs*Cs/(Conn1*Timsec)
       A(45,2)=Alpha*Csw
C    node 2 (fluid in contact)
       A(45,3)=Alpha*Csw
       A(45,4)=-Alpha*(Cw1+Csw)-RMw*Cpw1/(Conn2*Timsec)
C Cross-coupling coefficients
       A(45,6)=Alpha*Cw0
C Present-time-and-excitation coefficients
       B(45,1)=((1.-Alpha)*CswP-RMsP*CsP/(Conn1*Timsec))*TsP
     &           -(1.-Alpha)*CswP*Tw1P
       B(45,2)=-(1.-Alpha)*CswP*TsP
     &         +((1.-Alpha)*(Cw1P+CswP)-RMwP*Cpw1P/(Conn2*Timsec))*Tw1P
     &         -(1.-Alpha)*Cw0P*Tw0P

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(45,1)=1./Conn1
       A(45,2)=0.
C    node 2 (air in contact)
       A(45,3)=0.
       A(45,4)=1./Conn2
C Cross-coupling coefficients
       A(45,6)=-1.
C Present-time-and-excitation coefficients
       B(45,1)=0.
       B(45,2)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (solids)
       A(45,1)=1./Conn1
       A(45,2)=0.

C    node 2 (air in contact)
       A(45,3)=0.
       A(45,4)=1./Conn2

C Cross-coupling coefficients.
       A(45,6)=0.

C Present-time-and-excitation coefficients.
       B(45,1)=0.
       B(45,2)= 0.
      END IF

      RETURN
      END

C *********************** Primitive Part No. 5.1 ***********************

C Flow Diverger (for all fluid types)

      SUBROUTINE PP51(IPCOMP,ISTATS,R)

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (fluid at first outlet)
       A(51,1)=1.
C    node 2 (fluid at second outlet)
       A(51,2)=0.
       A(51,3)=1.
C Cross-coupling coefficients
       A(51,4)=-1.
       A(51,5)=-1.
C Present-time-and-excitation coefficients
       B(51,1)=0.
       B(51,2)=0.

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (fluid at first outlet)
       A(51,1)=1.
C    node 2 (fluid at second outlet)
       A(51,2)=1.
       A(51,3)=1.
C Cross-coupling coefficients
       A(51,4)=-R
       A(51,5)=-1.
C Present-time-and-excitation coefficients
       B(51,1)=0.
       B(51,2)=0.

      ENDIF

      RETURN
      END


C *********************** Primitive Part No. 5.2 ***********************

C Flow Multipier (for all fluid types)

      SUBROUTINE PP52(IPCOMP,ISTATS,R1,R2)

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
C Self-coupling coefficients
C    node 1 (leaving fluid)
       A(52,1)=1.
C Cross-coupling coefficients
      IF(ISTATS.EQ.1) THEN
       A(52,2)=-1.
      ELSE IF(ISTATS.EQ.2) THEN
       A(52,2)=-R1
      ELSE IF(ISTATS.EQ.3) THEN
       A(52,2)=-R2
      ENDIF
C Present-time-and-excitation coefficients
       B(52,1)=0.

      RETURN
      END

C *********************** Primitive Part No. 5.3 ***********************

C Flow Inducer (for all fluid types)

      SUBROUTINE PP53(IPCOMP,ISTATS,Conn,Fm,R1,R2)

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
C Self-coupling coefficients
C    node 1 (leaving fluid)
       A(53,1)=1./Conn
C Cross-coupling coefficients
      IF(ISTATS.EQ.1) THEN
       A(53,2)=-1.
      ELSE
       A(53,2)=0.
      ENDIF
C Present-time-and-excitation coefficients
      IF(ISTATS.EQ.1) THEN
       B(53,1)=0.
      ELSE IF(ISTATS.EQ.2) THEN
       B(53,1)=R1*Fm
      ELSE IF(ISTATS.EQ.3) THEN
       B(53,1)=R2*Fm
      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 6.1 ***********************

C Flow Converger (for moist air)

      SUBROUTINE PP61(IPCOMP,ISTATS,Ca1,Ca2,Ca3)

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
      IF(ISTATS.EQ.1) THEN
C Self-coupled & cross-coupled coefficients
C    node 1 (fluid at outlet)
       A(61,1)=Ca3
       A(61,2)=-Ca2
       A(61,3)=-Ca1
C Present-time-and-excitation coefficients
       B(61,1)=0.

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupled & cross-coupled coefficients
C    node 1 (fluid at outlet)
       A(61,1)=1.
       A(61,2)=-1.
       A(61,3)=-1.
C Present-time-and-excitation coefficients
       B(61,1)=0.

      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 6.2 ***********************

C Flow Converger (for 2-phase fluid)

      SUBROUTINE PP62(IPCOMP,ISTATS,Tsat,Hf,Hg,Cpf,Cpg,
     &               Cpw1,Cpv1,Cpw2,Cpv2,Tr1,Tr2,
     &               Rmw1,Rmv1,Rmw2,Rmv2)
      
      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      Rmr1=Rmw1+Rmv1
      Rmr2=Rmw2+Rmv2
      Rmr3=Rmr1+Rmr2
      Hr3=((Rmw1*Cpw1+Rmv1*Cpv1)*Tr1+(Rmw2*Cpw2+Rmv2*Cpv2)*Tr2)/Rmr3

      IF(Hr3.GT.Hf.AND.Hr3.LT.Hg) THEN
        Ct=Tsat
        Xr3=(Hr3-Hf)/(Hg-Hf)
        Cw=Rmr3*(1.-Xr3)
        Cv=Rmr3*Xr3
      ELSEIF(Hr3.LT.Hf) THEN
        Ct=Tsat-(Hf-Hr3)/Cpf
        Cw=Rmr3
        Cv=0.
      ELSEIF(Hr3.GT.Hg) THEN
        Ct=Tsat+(Hr3-Hg)/Cpg
        Cw=0.
        Cv=Rmr3
      ENDIF

C Self-coupling coefficients
C    node 1 (leaving fluid)
       A(62,1)=1.

C Present-time-and-excitation coefficients
      IF(ISTATS.EQ.1) THEN
       B(62,1)=Ct
      ELSE IF(ISTATS.EQ.2) THEN
       B(62,1)=Cw
      ELSE IF(ISTATS.EQ.3) THEN
       B(62,1)=Cv
      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 6.3 ***********************

C Flow Converger (for 1-phase fluid)

      SUBROUTINE PP63(IPCOMP,ISTATS,Tr3P,
     &               Cpw1,Cpv1,Cpw2,Cpv2,Cpw3,Cpv3,
     &               Rmw1,Rmv1,Rmw2,Rmv2)
      

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
      logical close
 
      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
       A(63,1)=(Rmw1*Cpw1+Rmw2*Cpw2)+(Rmv1*Cpv1+Rmv2*Cpv2)

C Cross-coupling coefficients
       A(63,2)=-Rmw1*Cpw1-Rmv1*Cpv1
       A(63,3)=-Rmw2*Cpw2-Rmv2*Cpv2

C Present-time-and-excitation coefficients
       B(63,1)=0.

C To keep temperature unchange under no flow condition
       call eclose(A(63,1),0.00,0.0001,close)
       IF(close) THEN
        A(63,1)=1.0
        B(63,1)=Tr3P
       ENDIF

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
       A(63,1)=1.

C Cross-coupling coefficients
       A(63,2)=-1.
       A(63,3)=-1.

C Present-time-and-excitation coefficients
       B(63,1)=0.

      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 6.4 ***********************

C Flow Converger (for leak-in moist air from outside)

      SUBROUTINE PP64(IPCOMP,ISTATS,Te,
     &               Cpa0,Cpv0,Cpa1,Cpv1,Cpae,Cpve,
     &               Rma0,Rmv0,Rma1,Rmv1,Rmae,Rmve)

      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
       A(64,1)=Rma1*Cpa1+Rmv1*Cpv1
C Cross-coupling coefficients
       A(64,2)=Rma0*Cpa0-Rmv0*Cpv0
C Present-time-and-excitation coefficients
       B(64,1)=(Rmae*Cpae+Rmve*Cpve)*Te

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
       A(64,1)=1.
C Cross-coupling coefficients
       A(64,2)=-1.
C Present-time-and-excitation coefficients
	IF(ISTATS.EQ.2) THEN
        B(64,1)=Rmae
	ELSE
	B(64,1)=Rmve
	ENDIF

      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 7.1 ***********************

C Flow (Moist Air) upon Water Spray: 2 node model

      SUBROUTINE PP71(IPCOMP,ISTATS,Conn1,Conn2,
     &               Caw,CawP,Ca1,Ca1P,Ca2,Ca2P,Cw1,Cw1P,Cw2,Cw2P,
     &               Cpa2,Cpa2P,Cpw2,Cpw2P,Cav,CavP,C21,
     &               RMa2,RMw2,RMa2P,RMw2P,Ta1P,Ta2P,Tw1P,Tw2P)     

      INCLUDE 'plant.h'
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha 

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C    node 1 (moist air)
       A(71,1)=-Alpha*(Ca2+Caw)-RMa2P*Cpa2P/(Conn1*Timsec)
       A(71,2)=Alpha*Caw
C    node 2 (liquid water)
       A(71,3)=Alpha*Caw
       A(71,4)=-Alpha*(Cw2+Caw)-RMw2*Cpw2/(Conn2*Timsec)
C Cross-coupling coefficients
       A(71,5)=Alpha*(Ca1-Caw)
       A(71,6)=Alpha*Caw
       A(71,7)=Alpha*Caw
       A(71,8)=Alpha*(Cw1-Caw)
C Present-time-and-excitation coefficients
       B(71,1)=((1.-Alpha)*(Ca2P+CawP)-RMa2*Cpa2/(Conn1*Timsec))*Ta2P
     &           -(1.-Alpha)*CawP*Tw2P-(1.-Alpha)*(Ca1P-CawP)*Ta1P
     &           -(1.-Alpha)*CawP*Tw1P+Alpha*Cav+(1.-Alpha)*CavP

       B(71,2)=-(1.-Alpha)*CawP*Ta2P
     &         +((1.-Alpha)*(Cw2P+CawP)-RMw2P*Cpw2P/(Conn2*Timsec))*Tw2P
     &         -(1.-Alpha)*CawP*Ta1P-(1.-Alpha)*(Cw1P-CawP)*Tw1P

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C    node 1 (moist air)
       A(71,1)=1./Conn1
       A(71,2)=0.
C    node 2 (liquid water)
       A(71,3)=0.
       A(71,4)=1./Conn2
C Cross-coupling coefficients
       A(71,5)=-1.
       A(71,6)=0.
       A(71,7)=0.
       A(71,8)=-1.
C Present-time-and-excitation coefficients
       B(71,1)=0.
       B(71,2)=C21

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C    node 1 (moist air)
       A(71,1)=1./Conn1
       A(71,2)=0.
C    node 2 (liquid water)
       A(71,3)=1.
       A(71,4)=1./Conn2
C Cross-coupling coefficients
       A(71,5)=-1.
       A(71,6)=0.
       A(71,7)=-1.
       A(71,8)=-1.
C Present-time-and-excitation coefficients
       B(71,1)=-C21
       B(71,2)=0.

      END IF

      RETURN
      END

C *********************** Primitive Part No. 8.1 ***********************

C Fluid Injection (water/steam to moist air)

      SUBROUTINE PP81(IPCOMP,ISTATS,Conn1,Ta2,
     &               Cr0,Ca1,Ca2,Rmw0,Rmv0,Rma1,Rmv1,Hfg,HfgP)
      
      INCLUDE 'plant.h'
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
      logical close

      Rmr0=Rmw0+Rmv0
      call eclose(Rma1,0.00,0.0001,close)
      IF(.NOT.close) THEN
	Ga2=(Rmv1+Rmr0)/Rma1
        ELSE
        Ga2=0.
      ENDIF

      Call SATMOIST(Ta2,Ga2s)
      IF(Ga2.LT.Ga2s) THEN
        Rmc=0.
        Cv=Rma1*Ga2
      ELSE
        Rmc=Rmr0+Rmv1-Rma1*Ga2S
        IF(Rmc.LT.0.) Rmc=0.
        Cv=Rma1*Ga2s
      ENDIF
 
      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
       A(81,1)=Ca2

C Cross-coupling coefficients
       A(81,2)=-Ca1
       A(81,3)=-Cr0

C Present-time-and-excitation coefficients
       B(81,1)=Hfg*Rmc


      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
       A(81,1)=1./Conn1

C Cross-coupling coefficients
       A(81,2)=-1.
       A(81,3)=0.

C Present-time-and-excitation coefficients
       B(81,1)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
       A(81,1)=1./Conn1

C Cross-coupling coefficients
       A(81,2)=0.
       A(81,3)=0.

C Present-time-and-excitation coefficients
       B(81,1)=Cv

      ENDIF

      RETURN
      END

C *********************** Primitive Part No. 9.1 ***********************

C Fluid Accumulator (for Moist Air)

      SUBROUTINE PP91(IPCOMP,ISTATS,Conn1,Conn2,
     &                Ca0,Ca0P,Ca,CaP,Ca2,Ca2P,
     &                Ta0P,Ta1P,Ta2P,Cm2,Ga1)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling & cross-coupling coefficients
C   node 1 (liquid accumulated)
       A(91,1)=-Ca/(Conn1*Timsec)
       A(91,2)=-Alpha*Ca2
       A(91,5)=Alpha*Ca0

C   node 2 (leaving fluid)
       A(91,3)=-1.
       A(91,4)=1.

C Present-time-and-excitation coefficients
       B(91,1)=-(CaP/(Conn1*Timsec))*Ta1P
     &           +(1.-Alpha)*Ca2P*Ta2P-(1.-Alpha)*Ca0P*Ta0P
       B(91,2)=0.

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN

C Self-coupling & cross-coupling coefficients
C   node 1 (liquid accumulated)
       A(91,1)=1./conn1
       A(91,2)=0.
       A(91,5)=-1.
C   node 2 (leaving fluid)
       A(91,3)=0.
       A(91,4)=1./conn2
C Present-time-and-excitation coefficients
       B(91,1)=0.
        IF(ISTATS.EQ.3) Cm2=Cm2*Ga1
       B(91,2)=Cm2

      END IF

      RETURN
      END

C *********************** Primitive Part No. 9.2 ***********************

C Fluid Accumulator (for Single-phase Fluid)

      SUBROUTINE PP92(IPCOMP,ISTATS,Conn1,Conn2,
     &                Cw0,Cw0P,Cw1,Cw1P,Cpw2,Cpw2P,
     &                RMw2,RMw2P,Tw0P,Tw1P,Tw2P,Cm1)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (liquid accumulated)
       A(92,1)=-RMw2*Cpw2/(Conn1*Timsec)
       A(92,2)=-Alpha*Cw1
C   node 2 (leaving fluid)
       A(92,3)=-1.
       A(92,4)=1.
C Cross-coupling coefficients
       A(92,5)=Alpha*Cw0
C Present-time-and-excitation coefficients
       B(92,1)=(-RMw2P*Cpw2P/(Conn1*Timsec))*Tw2P
     &           +(1.-Alpha)*Cw1P*Tw1P-(1.-Alpha)*Cw0P*Tw0P
       B(92,2)=0.

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C   node 1 (liquid accumulated)
       A(92,1)=1./conn1
       A(92,2)=1.
C   node 2 (leaving fluid)
       A(92,3)=0.
       A(92,4)=1./conn2
C Cross-coupling coefficients
       A(92,5)=-1.
C Present-time-and-excitation coefficients
       B(92,1)=0.
       B(92,2)=Cm1

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (liquid accumulated)
       A(92,1)=1./conn1
       A(92,2)=0.
C   node 2 (leaving fluid)
       A(92,3)=0.
       A(92,4)=1./conn2
C Cross-coupling coefficients
       A(92,5)=0.
C Present-time-and-excitation coefficients
       B(92,1)=0.
       B(92,2)=0.
      END IF

      RETURN
      END

C ********************** Primitive Part No. 10.1 ***********************

C Heat Injection (to solid) 

      SUBROUTINE PP101(IPCOMP,ISTATS,Conn1,Cs,RMs,TsP,Q,QP)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(101,1)=-RMs*Cs/(Conn1*Timsec)
C Present-time-and-excitation coefficients
       B(101,1)=-RMs*Cs/(Conn1*Timsec)*TsP-Alpha*Q-(1.-Alpha)*QP

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(101,1)=1./conn1
C Present-time-and-excitation coefficients
       B(101,1)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (solid)
       A(101,1)=1./conn1
C Present-time-and-excitation coefficients
       B(101,1)=0.
      END IF

      RETURN
      END

C ********************** Primitive Part No. 10.2 ***********************

C Heat Injection (to Vapour-generating Fluid)

      SUBROUTINE PP102(IPCOMP,ISTATS,Conn1,Conn2,
     &                Cvw,Cpw1,Cpw1P,
     &                RMw,RMwP,Rmv2,Rmv2P,Tw1P,
     &                Hfg,HfgP,Q,QP)
      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(102,1)=-RMw*Cpw1/(Conn1*Timsec)
       A(102,2)=0.
C   node 2 (leaving vapour)
       A(102,3)=-1.
       A(102,4)=1.
C Present-time-and-excitation coefficients
       B(102,1)=(-RMwP*Cpw1P/(Conn1*Timsec))*Tw1P
     &          +Alpha*Rmv2*Hfg+(1.-Alpha)*Rmv2P*HfgP
     &          -(Alpha*Q+(1.-Alpha)*QP)
       B(102,2)=0.

      ELSE IF(ISTATS.EQ.2) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(102,1)=1./conn1
       A(102,2)=0.
C   node 2 (leaving vapour)
       A(102,3)=0.
       A(102,4)=1./conn2
C Present-time-and-excitation coefficients
       B(102,1)=-Cvw
       B(102,2)=0.

      ELSE IF(ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(102,1)=1./conn1
       A(102,2)=0.
C   node 2 (leaving vapour)
       A(102,3)=0.
       A(102,4)=1./conn2
C Present-time-and-excitation coefficients
       B(102,1)=0.
       B(102,2)=Cvw
      END IF

      RETURN
      END

C ********************** Primitive Part No. 10.3 ***********************

C Heat Injection (to Moist Air)

      SUBROUTINE PP103(IPCOMP,ISTATS,Conn1,TaP,
     &                Ca,CaP,Rmc,RmcP,Hfg,HfgP,Q,QP)

      INCLUDE 'plant.h'   
      COMMON/Pctime/TIMSEC
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      IF(ISTATS.EQ.1) THEN
C Self-coupling coefficient
C   node 1 (air in control volume)
       A(103,1)=-(Alpha*Ca+(1.-Alpha)*CaP)/(Conn1*Timsec)

C Present-time-and-excitation coefficients
       B(103,1)=-((Alpha*Ca+(1.-Alpha)*CaP)/(Conn1*Timsec))*TaP
     &          -(Alpha*Rmc*Hfg+(1.-Alpha)*RmcP*HfgP)
     &          -(Alpha*Q+(1.-Alpha)*QP)

      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
C Self-coupling coefficients
C   node 1 (liquid in control volume)
       A(103,1)=1./conn1
C Present-time-and-excitation coefficients
       B(103,1)=0.

      END IF

      RETURN
      END
