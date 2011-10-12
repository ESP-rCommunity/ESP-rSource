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

C Plant component model coefficient generators (built from primitive
C parts):

C  cmp53c 4 node (ISV>19) AC heat transfer tube with transport
C                         delay at water side
C  cmp54c 1 node (ISV=20) water/steam flow multiplier
C  cmp55c 1 node (ISV=21) air flow multiplier
C  cmp56c 5 node (ISV>19) AC fan with submerged motor 
C  cmp57c 4 node (ISV>19) WCH insulated pipe with transport delay
C  cmp58c 4 node (ISV>19) AC air duct with transport delay
C  cmp60c 4 node (ISV>19) AC air electric heater
C  cmp64c 1 node (ISV=20) water/steam flow converger
C  cmp65c 3 node (ISV=20) water/steam flow diverger
C  cmp66c 3 node (ISV=21) moist air flow diverger
C  cmp68c 1 node (ISV=21) moist air flow converger
C  cmp69c 1 node (ISV=21) water/steam flow diverger with controlled ratio

C ******************** CMP53C ********************

C CMP53C generates for plant component IPCOMP with plant db code 530
C ie. 4 node (ISV>19) AC heat transfer tube with transport delay at
C water side
C   node 1 represents solid materials
C   node 2 represents leaving air
C   node 3 represents water in contact
C   node 4 represents leaving water
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Metallic tube total mass (kg)
C            2 Specific heat of tube (weighed average) (J/kgK)
C            3 Thermal resistance of tube (m^2.C/W)
C            4 Tube outside (air side) heat transfer area (m^2)
C            5 Tube inside (water side) heat transfer area (m^2)
C            6 Air free flow area (m^2)
C            7 Internal tube diameter (m)
C            8 Tube length (m)
C            9 Volume of encapsulated air (m^3)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP53C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE)

      DIMENSION Conn(4)

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP53C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,2,1)
      ICON2=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3

C set the node connectivity values
      Conn(1)=2.
      Conn(2)=1.
      Conn(3)=1.
      Conn(4)=1.

C To generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First mark 1st phase mass flow rates for iteration;
        ICSV(INOD2,2)=1
        CSVI(INOD2,2)=CSVF(INOD2,2)
        ICSV(INOD3,2)=1
        CSVI(INOD3,2)=CSVF(INOD3,2)
        ICSV(INOD4,2)=1
        CSVI(INOD4,2)=CSVF(INOD4,2)
        Rma1=CSVI(INOD2,2)
        Rmwm=CSVI(INOD3,2)
        Rmw2=CSVI(INOD4,2)

C Then mark 2nd phase mass flow rates for iteration
        ICSV(INOD2,3)=1
        CSVI(INOD2,3)=CSVF(INOD2,3)
        Rmv1=CSVI(INOD2,3)

C Mark current temperatures for iteration 
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)
        ICSV(INOD4,1)=1
        CSVI(INOD4,1)=CSVF(INOD4,1)
        Ts=CSVF(INOD1,1)
        Ta1=CSVF(INOD2,1)
        Twm=CSVF(INOD3,1)
        Tw2=CSVF(INOD4,1)

C Establish incoming air and water temperatures & flow rates
        Ta0=CONVAR(ICON1,1)
        Tw0=CONVAR(ICON2,1)
        Rma0=CONVAR(ICON1,2)
        Rmv0=CONVAR(ICON1,3)
        Rmw0=CONVAR(ICON2,2)

C Identify temperatures & flow rates of present time step
        Ta0P=PCTP(ICON1)
        Tw0P=PCTP(ICON2)
        TsP=CSVP(INOD1,1)
        Ta1P=CSVP(INOD2,1)
        TwmP=CSVP(INOD3,1)
        Tw2P=CSVP(INOD4,1)
        Rma1P=CSVP(INOD2,2)
        Rmv1P=CSVP(INOD2,3)
        RmwmP=CSVP(INOD3,2)
        Rmw2P=CSVP(INOD4,2)

C Restore PCDATP values
        SurfP=PCDATP(IPCOMP,1)
        Ca0P=PCDATP(IPCOMP,2)
        Ca1P=PCDATP(IPCOMP,3)
        Cw0P=PCDATP(IPCOMP,4)
        CwmP=PCDATP(IPCOMP,5)
        Cw2P=PCDATP(IPCOMP,6)
        Ga0P=PCDATP(IPCOMP,7)
        Cpam1P=PCDATP(IPCOMP,8)
        CpwmP=PCDATP(IPCOMP,9)
        CasP=PCDATP(IPCOMP,10)
        CswP=PCDATP(IPCOMP,11)
        CavP=PCDATP(IPCOMP,12)
        CdP=PCDATP(IPCOMP,13)

C Identify the specified ADATA and CDATA
        RMs=ADATA(IPCOMP,1)
        Cs=ADATA(IPCOMP,2)
        RTs=ADATA(IPCOMP,3)
        Ao=ADATA(IPCOMP,4)
        Ai=ADATA(IPCOMP,5)
        Ax=ADATA(IPCOMP,6)
        Di=ADATA(IPCOMP,7)
        RL=ADATA(IPCOMP,8)
        Va=ADATA(IPCOMP,9)

C Determine specific heat and latent heat values
        Cpa0=SHTFLD(1,Ta0)
        Cpv0=SHTFLD(2,Ta0)
        Cpw0=SHTFLD(3,Tw0)
        Cpa1=SHTFLD(1,Ta1)
        Cpv1=SHTFLD(2,Ta1)
        Cpwm=SHTFLD(3,Twm)
        Cpw2=SHTFLD(3,Tw2)

        Hfg=CNDWAT(Ta1)

C Establish outstanding arguements used in primitive part subroutines

        RMa=Va*RHOFLD(1,Ta1)
        RMw=3.14159*(Di**2)/4.*RL*RHOFLD(3,Twm)
        Ua=Rma0/(Ax*RHOFLD(1,Ta1))
        Uw=Rmw0/(RHOFLD(3,Twm)*3.14159*(Di**2)/4.)
        hi=1400.*(1.+0.015*Twm)*(Uw**0.8)/Di**0.2
        ho=38.*Ua

C Modify ho for wetted surface
        IF (CdP.GT.0.) ho=ho*0.626*(Ua**0.101)

C Modify heat transfer coefficients by adding tube thermal resistances
        ho=1./(1./ho+RTs/2.)
        hi=1./(1./hi+RTs/2.)

        Ca0=Rma0*Cpa0+Rmv0*Cpv0
        Ca1=Rma1*Cpa1+Rmv1*Cpv1
        Cw0=Rmw0*Cpw0
        Cwm=Rmwm*Cpwm
        Cw2=Rmw2*Cpw2
        Csw=hi*Ai
        Cas=ho*Ao
        Cav=-Hfg*(Rmv0-Rmv1)
        Cd=PCDATF(IPCOMP,13)
        Ga0=Rmv0/AMAX1(Rma0,small)
        Cpam1=Ca1/AMAX1(Rma1,small)

C Check if water side transport delay is required for future time step
        DY=(Uw*TIMSEC)/RL
        IF (DY.LT.1.0.and.DY.GT.0.0) THEN
          WDELAY=1.
          N=int(1./DY)
        ELSE
          WDELAY=0.
        ENDIF

        Call TRNSDLAY(IPCOMP,WDELAY,Tw0P,Tw2P,TsP,N,DY,Cw2,Csw,
     &               Tw0,Twm,Tw2)

C Calculate current component time-constant TC 
C based on response of air temperature to change of incoming water
C temperature and vice versa 
        TCw1=RMw*Cpwm/(hi*Ai)
        TCw2=RMw*Cpwm/amax1(small,(Rmwm*Cpwm))

        IF(abs(Rmwm).lt.small) THEN
          TCw=TCw1
        ELSE
          CALL TIMCONST(TCw1,TCw2,TCw)
        ENDIF

        TCs1=RMs*Cs/(ho*Ao)
        TCs2=RMs*Cs/(hi*Ai)
        CALL TIMCONST(TCs1,TCs2,TCs)
        CALL TIMCONST(TCs,TCw,TCc)
        TC(IPCOMP)=TCc

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          RMs=0.
          RMw=0.
          RMa=0.
          ALPHA=1.
        END IF

      ELSE IF(ISTATS.EQ.3) THEN

C Determine values of valuables used in this component for 
C 2nd phase mass flow iteration

        Ts=CSVF(INOD1,1)
        Rmv0=CONVAR(ICON1,3)
        Ga0=PCDATF(IPCOMP,7)
        Cpam1=PCDATF(IPCOMP,8)
        Cas=PCDATF(IPCOMP,10)
        Ao=ADATA(IPCOMP,4)
        ho=Cas/Ao
        CdP=PCDATP(IPCOMP,13)

C Detect exterior tube surface condition: DRY or WET;
        CALL  SURFWET(Ga0,Ts,CdP,Surf)

C Determine maximum moisture holding capacity (kg) of tube exterior surface
C with an assumed condensate film thickness (0.05 mm)
        film = 0.00005
        Cdmax=1000.*film*Ao
        IF(Cd.GT.Cdmax) Cd=Cdmax

      END IF

C Determine matrix coefficients

      CALL PP43(IPCOMP,ISTATS,Conn(1),Conn(3),WDELAY,Twm,Tw2,
     &          Csw,CswP,Cw0,Cw0P,Cw2,Cw2P,Cs,Cpwm,CpwmP,Cpw2,
     &          RMs,RMw,Tw0P,TwmP,Tw2P,TsP)

      CALL PP44(IPCOMP,ISTATS,Surf,Conn(1),Conn(2),
     &          Cas,CasP,Ca0,Ca0P,Ca1,Ca1P,Cs,Cav,CavP,
     &          Cpam1,Cpam1P,ho,Ao,Cd,CdP,
     &          RMs,RMa,Rmv0,Ga0,Ta0P,Ta1,Ta1P,Ts,TsP)

      COUT(1)=A(43,1)+A(44,1)
      COUT(2)=A(44,2)
      COUT(3)=A(43,2)
      COUT(4)=A(44,3)
      COUT(5)=A(44,4)
      COUT(6)=A(43,4)
      COUT(7)=A(43,5)
      COUT(8)=A(43,6)
      COUT(9)=A(43,8)
      COUT(10)=A(43,9)
      COUT(11)=A(44,6)
      COUT(12)=A(43,11)
      COUT(13)=B(43,1)+B(44,1)
      COUT(14)=B(44,2)
      COUT(15)=B(43,2)
      COUT(16)=B(43,3)

C Store component variables future values
      IF(ISTATS.EQ.1) THEN
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCTF(ICON2)=CONVAR(ICON2,1)
        PCDATF(IPCOMP,1)=SurF
        PCDATF(IPCOMP,2)=Ca0
        PCDATF(IPCOMP,3)=Ca1
        PCDATF(IPCOMP,4)=Cw0
        PCDATF(IPCOMP,5)=Cwm
        PCDATF(IPCOMP,6)=Cw2
        PCDATF(IPCOMP,7)=Ga0
        PCDATF(IPCOMP,8)=Cpam1
        PCDATF(IPCOMP,9)=Cpwm
        PCDATF(IPCOMP,10)=Cas
        PCDATF(IPCOMP,11)=Csw
        PCDATF(IPCOMP,12)=Cav

      ELSEIF(ISTATS.EQ.3) THEN
C store future value of condensate mass on tube
        PCDATF(IPCOMP,13)=Cd
         
      ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 4 node (ISV>19) AC heat transfer tube',
     &               ' with transport delay'
        WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4
        WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' Ca1    = ',Ca1,' (W/K)'
          WRITE(ITU,*) ' Cwm    = ',Cwm,' (W/K)'
          WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
          WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
          WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
          WRITE(ITU,*) ' Tw0    = ',Tw0,' (C)'
          WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
          WRITE(ITU,*) ' Tw2    = ',Tw2,' (C)'
          WRITE(ITU,*) ' Ua     = ',Ua,' (m/s)'
          WRITE(ITU,*) ' Uw     = ',Uw,' (m/s)'
          WRITE(ITU,*) ' ho     = ',ho,' (W/K.m^2)'
          WRITE(ITU,*) ' hi     = ',hi,' (W/K.m^2)' 
          WRITE(ITU,*) ' Ca0    = ',Ca0,' (W/K)'
          WRITE(ITU,*) ' Cw0    = ',Cw0,' (W/k)'
          WRITE(ITU,*) ' Cav    = ',Cav,' (W)'
          WRITE(ITU,*) ' CavP   = ',CavP,' (W)'
          WRITE(ITU,*) ' Hfg    = ',Hfg,' (J/kg)'
          WRITE(ITU,*) ' Rmv0   = ',Rmv0,' (kg/s)'
          WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
          WRITE(ITU,*) ' Ga0    = ',Ga0,' (kg/kg)'
          WRITE(ITU,*) ' Cd     = ',Cd,' (kg)'
          WRITE(ITU,*) ' CdP    = ',CdP,' (kg)'
          WRITE(ITU,*) ' Ts   = ',Ts,' (deg.C)'
          WRITE(ITU,*) ' TsP   = ',TsP,' (deg.C)'
        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        NITMS=16
        WRITE(ITU,*) (COUT(I),I=1,NITMS)
        IF(ITU.EQ.IUOUT) THEN
          IX1=(IPCOMP/4)*4
          IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
        END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP53C'

      RETURN
      END


C **************************** CMP54C ****************************
C CMP54C generates for plant component IPCOMP with plant db code 540
C i.e. 1 node (ISV=20)  water/steam flow multiplier 
C built from primitive parts; fluid temperature remains unchanged
C   node 1 represents leaving water/steam
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1  out-to-in flow ratio (first phase mass flow)
C            2  out-to-in flow ratio (second phase mass flow)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP54C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTC/TC(MPCOM)

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
 
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      REAL      COUT(MPCOE)

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP54C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,1,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.

C mark 1st phase mass flow rate for iteration;
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      Rmw1=CSVI(INOD1,2)

C mark 2nd phase mass flow rate for iteration
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      Rmv1=CSVI(INOD1,3)

C Mark current fluid temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      Tr1=CSVI(INOD1,1)

C establish incoming fluid temperature & flow rates
      Tr0=CONVAR(ICON1,1)
      Rmw0=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)

C identify the specified ADATA 
      R1=ADATA(IPCOMP,1)
      R2=ADATA(IPCOMP,2)

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10   CALL PP52(IPCOMP,ISTATS,R1,R2)

      COUT(1)=A(52,1)
      COUT(2)=A(52,2)
      COUT(3)=B(52,1)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        WRITE(ITU,*) ' 1 node (ISV=20) water circuit flow multiplier',
     &               ' built by Primitive Parts'
        WRITE(ITU,*) ' Matrix node(s) ',INOD1
        WRITE(ITU,*) ' Connection(s)  ',ICON1
        IF(ISTATS.EQ.1) THEN
          WRITE(ITU,*) ' Rmw0    = ',Rmw0,' (Kg/s)'
          WRITE(ITU,*) ' Rmv0    = ',Rmv0,' (Kg/s)'
          WRITE(ITU,*) ' Rmw1   = ',Rmw1,' (kg/s)'
          WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
          WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
          WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
          WRITE(ITU,*) ' Tr0    = ',Tr0,' (C)'
          WRITE(ITU,*) ' Tr1    = ',Tr1,' (C)'
          WRITE(ITU,*) ' R1   = ',R1,' (-)'
          WRITE(ITU,*) ' R2   = ',R2,' (-)'

        END IF
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        NITMS=3
        WRITE(ITU,*) (COUT(I),I=1,NITMS)
        IF(ITU.EQ.IUOUT) THEN
          IX1=(IPCOMP/4)*4
          IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
        END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP54C'

      RETURN
      END

C **************************** CMP55C ****************************

C CMP55C generates for plant component IPCOMP with plant db code 550
C i.e. 1 nodes (ISV=21) air flow multiplier 
C built from primitive parts; fluid temperature remains unchanged
C   node 1 represents leaving air
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1  out-to-in flow ratio (first phase mass flow)
C            2  out-to-in flow ratio (second phase mass flow)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP55C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-20)

      REAL      COUT(MPCOE)

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP55C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,1,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.

C mark 1st phase mass flow rate for iteration;
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      Rma1=CSVI(INOD1,2)

C mark 2nd phase mass flow rate for iteration
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      Rmv1=CSVI(INOD1,3)

C Mark current fluid temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      Ta1=CSVI(INOD1,1)

C establish incoming fluid temperature & flow rates
      Ta0=CONVAR(ICON1,1)
      Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)

C identify the specified ADATA 
      R1=ADATA(IPCOMP,1)
      R2=ADATA(IPCOMP,2)

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10   CALL PP52(IPCOMP,ISTATS,R1,R2)
      COUT(1)=A(52,1)
      COUT(2)=A(52,2)
      COUT(3)=B(52,1)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) air flow multiplier',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rma0    = ',Rma0,' (Kg/s)'
            WRITE(ITU,*) ' Rmv0    = ',Rmv0,' (Kg/s)'
            WRITE(ITU,*) ' Rma1   = ',Rma1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
            WRITE(ITU,*) ' R1   = ',R1,' (-)'
            WRITE(ITU,*) ' R2   = ',R2,' (-)'

         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=3
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP55C'

      RETURN
      END

C ******************** CMP56C ********************

C CMP56C generates for plant component IPCOMP with plant db code 560 ie.
C 5 node (ISV>19) AC fan with submerged motor & built from primitive parts
C node 1 represents submerged fan/motor body
C node 2 represents fan casing
C node 3 represents air in contact with fan/motor body
C node 4 represents air in contact with casing
C node 5 represents leaving air 
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance(ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Submerged fan/motor body total mass (kg)
C            2 Casing total mass (kg)
C            3 Specific heat of fan/motor body (weighed average) (J/kgK)
C            4 Specific heat of casing (weighed average) (J/kgK)
C            5 Thermal resistance of casing (m^2.C/W)
C            6 Exposed surface area of submerged fan/motor body (m^2)
C            7 Outside surface area of casing (m^2)
C            8 Inside surface area of casing (m^2)
C            9 Rated volume flow rate  (m^3/s)
C           10 Rated absorbed power (W)
C           11 Rated flow velocity inside fan (average) (m/s)
C           12 Ambient air velocity (m/s)
C     BDATA: 1 Overall fan efficiency  (-)
C     CDATA: 1 Volume flow rate  (m^3/s)

      SUBROUTINE CMP56C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C      CHARACTER SURFACE3*3,SURFACE4*3
      DIMENSION Conn(5),A1(6),B1(2)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP56C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3
      INOD5=NPCDAT(IPCOMP,9)+4

C set the node connectivity values
      Conn(1)=2.
      Conn(2)=2.
      Conn(3)=2.
      Conn(4)=1.
      Conn(5)=1.

C To generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First mark air 1st phase mass flow rates for iteration;
        ICSV(INOD3,2)=1
        CSVI(INOD3,2)=CSVF(INOD3,2)
        ICSV(INOD4,2)=1
        CSVI(INOD4,2)=CSVF(INOD4,2)
        ICSV(INOD5,2)=1
        CSVI(INOD5,2)=CSVF(INOD5,2)
        Rma1=CSVI(INOD3,2)
        Rma2=CSVI(INOD4,2)
        Rma3=CSVI(INOD5,2)

C Then mark air 2nd phase mass flow rates for iteration
        ICSV(INOD3,3)=1
        CSVI(INOD3,3)=CSVF(INOD3,3)
        ICSV(INOD4,3)=1
        CSVI(INOD4,3)=CSVF(INOD4,3)
        ICSV(INOD5,3)=1
        CSVI(INOD5,3)=CSVF(INOD5,3)
        Rmv1=CSVI(INOD3,3)
        Rmv2=CSVI(INOD4,3)
        Rmv3=CSVI(INOD5,3)

C Mark current solid and air temperatures for iteration 
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)
        ICSV(INOD4,1)=1
        CSVI(INOD4,1)=CSVF(INOD4,1)
        ICSV(INOD5,1)=1
        CSVI(INOD5,1)=CSVF(INOD5,1)
        Ts4=CSVI(INOD1,1)
        Ts5=CSVI(INOD2,1)
        Ta1=CSVI(INOD3,1)
        Ta2=CSVI(INOD4,1)
        Ta3=CSVI(INOD5,1)

C Establish incoming air temperature & flow rates
        Ta0=CONVAR(ICON1,1)
        Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
        Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)

C Identify temperatures & flow rates of present time step
        Ta0P=PCTP(ICON1)
        Ta1P=CSVP(INOD3,1)
        Ta2P=CSVP(INOD4,1)
        Ta3P=CSVP(INOD5,1)
        Ts4P=CSVP(INOD1,1)
        Ts5P=CSVP(INOD2,1)

        Rma1P=CSVP(INOD3,2)
        Rma2P=CSVP(INOD4,2)
        Rma3P=CSVP(INOD5,2)

        Rmv1P=CSVP(INOD3,3)
        Rmv2P=CSVP(INOD4,3)
        Rmv3P=CSVP(INOD5,3)

C Restore PCDATP values
        CesP=PCDATP(IPCOMP,1)
        Cas5P=PCDATP(IPCOMP,2)
        Cas4P=PCDATP(IPCOMP,3)
        Ca0P=PCDATP(IPCOMP,4)
        Ca1P=PCDATP(IPCOMP,5)
        Ca2P=PCDATP(IPCOMP,6)

        Cav5P=PCDATP(IPCOMP,7)
        Cav4P=PCDATP(IPCOMP,8)
        Cd5P=PCDATP(IPCOMP,9)
        Cd4P=PCDATP(IPCOMP,10)
        Surf5P=PCDATP(IPCOMP,11)
        Surf4P=PCDATP(IPCOMP,12)
        Qa1P=PCDATP(IPCOMP,13)
        Qs4P=PCDATP(IPCOMP,14)
        Ga0P=PCDATP(IPCOMP,15)
        Ga1P=PCDATP(IPCOMP,16)
        Ga2P=PCDATF(IPCOMP,17)
        Cpam1P=PCDATP(IPCOMP,18)
        Cpam2P=PCDATP(IPCOMP,19)
        HfgP=PCDATP(IPCOMP,20)

        TeP=PCNTMP(IPCOMP)


C Identify the specified ADATA, BDATA and CDATA
        RMs4=ADATA(IPCOMP,1)
        RMs5=ADATA(IPCOMP,2)
        Cs4=ADATA(IPCOMP,3)
        Cs5=ADATA(IPCOMP,4)
        RTs5=ADATA(IPCOMP,5)
        Ab=ADATA(IPCOMP,6)
        Ao=ADATA(IPCOMP,7)
        Ai=ADATA(IPCOMP,8)
        RVF=ADATA(IPCOMP,9)
        PWR=ADATA(IPCOMP,10)
        RUa=ADATA(IPCOMP,11)
        Ue=ADATA(IPCOMP,12)
        REff=BDATA(IPCOMP,1)
        VF=CDATA(IPCOMP,1)

        call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
        if(closea) PCNTMF(IPCOMP)=Ts5
        Te=PCNTMF(IPCOMP)

C Determine specific heat and latent heat values
        CPa0=SHTFLD(1,Ta0)
        CPv0=SHTFLD(2,Ta0)
        CPa1=SHTFLD(1,Ta1)
        CPv1=SHTFLD(2,Ta1)
        CPa2=SHTFLD(1,Ta2)
        CPv2=SHTFLD(2,Ta2)
        CPa3=SHTFLD(1,Ta3)
        CPv3=SHTFLD(2,Ta3)

        Hfg=CNDWAT(Ta1)

C Establish outstanding arguements used in primitive part subroutines

        RMa=small
        Ua=RUa*VF/RVF

        ho=5.62+3.9*Ue

        IF(Ua.GT.5.0) THEN
          hi=7.2*Ua**0.78
        ELSE
          hi=5.62+3.9*Ua
        ENDIF

        hb=hi 

C Modify heat transfer coefficients by adding casing thermal resistances
        ho=1./(1./ho+RTs5/2.)
        hi=1./(1./hi+RTs5/2.)

C determine Rmc for trace
        Rmc=Rmv0-Rmv1

        Ga0=Rmv0/AMAX1(Rma0,small)
        Ga1=Rmv1/AMAX1(Rma1,small)
        Ga2=Rmv2/AMAX1(Rma2,small)

        Ca0=Rma0*Cpa0+Rmv0*Cpv0
        Ca1=Rma1*Cpa1+Rmv1*Cpv1
        Ca2=Rma2*Cpa2+Rmv2*Cpv2

        Cpam1=Ca1/AMAX1(Rma1,small)
        Cpam2=Ca2/AMAX1(Rma2,small)

        Ces=ho*Ao
        Cas5=hi*Ai
        Cas4=hb*Ab
        Cav4=-Hfg*(Rmv0-Rmv1)
        Cav5=-Hfg*(Rmv1-Rmv2)

C Determine variables for PP5.3 
        RRma=RVF*RHOFLD(1,Ta2)
        Rma3=VF*RHOFLD(1,Ta2)
        R1=1.
        R2=Ga2

C Establish absorbed power E based on current air flow rate which might
C have been calculated by mfs
C In case of mfs E is based on flow rate, pressure *rise*, and efficiency
        IF(IFLWN.NE.0) THEN
          ICNN=ICFFS(ICON1)
          E=real((FLW1(ICNN)+FLW2(ICNN))
     &      *(PRES(NODNE(ICNN))-PRES(NODPS(ICNN)))
     &      /(REff*RHOFLD(1,Ta2)))
        ELSE
          E=PWR*(Rma3/RRma)**3
        END IF
 
C Now Q is made up of all inefficiencies
        Qa1=E*REff
        Qs4=E*(1.-REff)

C Calculate current component time-constant TC (max. of casing and fan/motor)

        TCb=RMs4*Cs4/(hb*Ab)
        TCs1=(RMs5*Cs5)/(ho*Ao)
        TCs2=(RMs5*Cs5)/(hi*Ai)
        CALL TIMCONST(TCs1,TCs2,TCs)

        TC(IPCOMP)=AMAX1(TCs,TCb)

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          RMs4=0.
          RMs5=0.
          ALPHA=1.
        END IF

C To generate coefficients for mass balance equations
      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN

        Ai=ADATA(IPCOMP,8)
        Ab=ADATA(IPCOMP,6)
        VF=CDATA(IPCOMP,1)

        Cas5=PCDATF(IPCOMP,2)
        Cas4=PCDATF(IPCOMP,3)

        Ga0=PCDATF(IPCOMP,15)
        Ga1=PCDATF(IPCOMP,16)
        Ga2=PCDATF(IPCOMP,17)
        Cpam1=PCDATF(IPCOMP,18)
        Cpam2=PCDATF(IPCOMP,19)

        Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)
        Rmv1=CSVF(INOD3,3)

        Ts5=CSVF(INOD2,1)
        Ts4=CSVF(INOD1,1)
        Ta2=CSVF(INOD4,1)

        Cd5P=PCDATP(IPCOMP,9)
        Cd4P=PCDATP(IPCOMP,10)

        hi=Cas5/Ai
        hb=Cas4/Ab

        Rma3=VF*RHOFLD(1,Ta2)
        R1=1.
        R2=Ga2

        CALL  SURFWET(Ga0,Ts4,Cd4P,Surf4)
        CALL  SURFWET(Ga1,Ts5,Cd5P,Surf5)

      END IF

C Determine matrix coefficients
      CALL PP24(IPCOMP,ISTATS,Conn(2),
     &          Ces,CesP,RMs5,Cs5,Te,TeP,Ts5P)

      CALL PP44(IPCOMP,ISTATS,Surf4,Conn(1),Conn(3),
     &          Cas4,Cas4P,Ca0,Ca0P,Ca1,Ca1P,Cs4,Cav4,Cav4P,
     &          Cpam1,Cpam1P,hb,Ab,Cd4,Cd4P,
     &          RMs4,RMa,Rmv0,Ga0,Ta0P,Ta1,Ta1P,Ts4,Ts4P)

      A1(1)=A(44,1)
      A1(2)=A(44,2)
      A1(3)=A(44,3)
      A1(4)=A(44,4)
      A1(6)=A(44,6)
      B1(1)=B(44,1)
      B1(2)=B(44,2)

      CALL PP44(IPCOMP,ISTATS,Surf5,Conn(2),Conn(4),
     &          Cas5,Cas5P,Ca1,Ca1P,Ca2,Ca2P,Cs5,Cav5,Cav5P,
     &          Cpam2,Cpam2P,hi,Ai,Cd5,Cd5P,
     &          RMs5,RMa,Rmv1,Ga1,Ta1P,Ta2,Ta2P,Ts5,Ts5P)

      CALL PP53(IPCOMP,ISTATS,Conn(5),Rma3,R1,R2)

      CALL PP101(IPCOMP,ISTATS,Conn(1),Cs4,RMs4,Ts4P,Qs4,Qs4P)

      Rmc=0.
      RmcP=0.
      CALL PP103(IPCOMP,ISTATS,Conn(3),Ta1P,
     &           Ca1,Ca1P,Rmc,RmcP,Hfg,HfgP,Qa1,Qa1P)

      COUT(1)=A1(1)+A(101,1)
      COUT(2)=A1(2)
      COUT(3)=A(24,1)+A(44,1)
      COUT(4)=A(44,2)
      COUT(5)=A1(3)
      COUT(6)=A1(4)+A(103,1)
      COUT(7)=A(44,3)
      COUT(8)=A(44,6)
      COUT(9)=A(44,4)
      COUT(10)=A(53,2)
      COUT(11)=A(53,1)
      COUT(12)=A1(6)
      COUT(13)=B1(1)+B(101,1)
      COUT(14)=B(24,1)+B(44,1)
      COUT(15)=B1(2)+B(103,1)
      COUT(16)=B(44,2)
      COUT(17)=B(53,1)

C Store component variables future values
      IF(ISTATS.EQ.1) THEN
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCDATF(IPCOMP,1)=Ces
        PCDATF(IPCOMP,2)=Cas5
        PCDATF(IPCOMP,3)=Cas4
        PCDATF(IPCOMP,4)=Ca0
        PCDATF(IPCOMP,5)=Ca1
        PCDATF(IPCOMP,6)=Ca2
        PCDATF(IPCOMP,7)=Cav5
        PCDATF(IPCOMP,8)=Cav4
        PCDATF(IPCOMP,13)=Qa1
        PCDATF(IPCOMP,14)=Qs4
        PCDATF(IPCOMP,15)=Ga0
        PCDATF(IPCOMP,16)=Ga1
        PCDATF(IPCOMP,17)=Ga2
        PCDATF(IPCOMP,18)=Cpam1
        PCDATF(IPCOMP,19)=Cpam2
        PCDATF(IPCOMP,20)=Hfg

      ELSEIF(ISTATS.EQ.3) THEN
        PCDATF(IPCOMP,9)=Cd5
        PCDATF(IPCOMP,10)=Cd4
        PCDATF(IPCOMP,11)=Surf5
        PCDATF(IPCOMP,12)=Surf4

      ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 5 node (ISV>19) AC fan'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4,INOD5
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
            WRITE(ITU,*) ' Ta2    = ',Ta2,' (C)'
            WRITE(ITU,*) ' Ta3    = ',Ta3,' (C)'
            WRITE(ITU,*) ' Ts4    = ',Ts4,' (C)'
            WRITE(ITU,*) ' Ts5    = ',Ts5,' (C)'
            WRITE(ITU,*) ' Te     = ',Te,' (C)'
            WRITE(ITU,*) ' TeP    = ',TeP,' (C)'
            WRITE(ITU,*) ' Ca0    = ',Ca0,' (W/K)'
            WRITE(ITU,*) ' Ca1    = ',Ca1,' (W/K)'
            WRITE(ITU,*) ' Ca2    = ',Ca2,' (W/k)'     
            WRITE(ITU,*) ' Rmv0   = ',Rmv0,' (kg/s)'
            WRITE(ITU,*) ' Rmv2   = ',Rmv2,' (kg/s)'
            WRITE(ITU,*) ' Rma0   = ',Rma0,' (kg/s)'
            WRITE(ITU,*) ' Rma2   = ',Rma2,' (kg/s)'
            WRITE(ITU,*) ' Ga0    = ',Ga0,' (kg/kg)'
            WRITE(ITU,*) ' Ga1    = ',Ga1,' (kg/kg)'
            WRITE(ITU,*) ' Ga2    = ',Ga2,' (kg/kg)'
            WRITE(ITU,*) ' Qs4     = ',Qs4,' (W)'
            WRITE(ITU,*) ' Qa1     = ',Qa1,' (W)'

         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=17
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
           IX1=(IPCOMP/4)*4
           IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP56C'

      RETURN
      END


C ******************** CMP57C ********************

C CMP57C generates for plant component IPCOMP with plant db code 570 ie.
C 4 node (ISV>19) WCH insulated water pipe with transport delay
C & built from primitive parts
C node 1 represents pipe outer layer
C node 2 represents pipe inner layer
C node 3 represents water in contact
C node 4 represents leaving water 
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance(ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 pipe outer layer total mass (kg)
C            2 pipe inner layer total mass (kg)
C            3 Specific heat of outer layer (weighed average) (J/kgK)
C            4 Specific heat of inner layer (weighed average) (J/kgK)
C            5 Thermal resistance of outer layer (m^2.C/W)
C            6 Thermal resistance of inner layer (m^2.C/W)
C            7 Outside heat transfer area (m^2)
C            8 Inside heat transfer area (m^2)
C            9 Inside effective pipe diameter (m)
C           10 Pipe length (m)
C           11 Ambient air velocity (m/s)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP57C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)

      DIMENSION Conn(4)
      logical closea

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP57C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3

C set the node connectivity values
      Conn(1)=2.
      Conn(2)=2.
      Conn(3)=1.
      Conn(4)=1.

C To generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First mark water 1st phase mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
        ICSV(INOD3,2)=1
        CSVI(INOD3,2)=CSVF(INOD3,2)
        ICSV(INOD4,2)=1
        CSVI(INOD4,2)=CSVF(INOD4,2)
        Rmw1=CSVI(INOD3,2)
        Rmw2=CSVI(INOD4,2)

C Mark current solid and water temperatures for iteration 
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)
        ICSV(INOD4,1)=1
        CSVI(INOD4,1)=CSVF(INOD4,1)
        Ts4=CSVI(INOD1,1)
        Ts3=CSVI(INOD2,1)
        Tw1=CSVI(INOD3,1)
        Tw2=CSVI(INOD4,1)

C Establish incoming water temperature & flow rates
        Tw0=CONVAR(ICON1,1)
        Rmw0=PCONDR(ICON1)*CONVAR(ICON1,2)

C Identify temperatures & flow rates of present time step
        Tw0P=PCTP(ICON1)
        Tw1P=CSVP(INOD3,1)
        Tw2P=CSVP(INOD4,1)
        Ts3P=CSVP(INOD2,1)
        Ts4P=CSVP(INOD1,1)
        Rmw1P=CSVP(INOD3,2)
        Rmw2P=CSVP(INOD4,2)

C Restore PCDATP values
        CesP=PCDATP(IPCOMP,1)
        CswP=PCDATP(IPCOMP,2)
        Cw0P=PCDATP(IPCOMP,3)
        Cw2P=PCDATP(IPCOMP,4)
        Cpw1P=PCDATP(IPCOMP,5)

        TeP=PCNTMP(IPCOMP)

C Identify the specified ADATA and CDATA
        RMs4=ADATA(IPCOMP,1)
        RMs3=ADATA(IPCOMP,2)
        Cs4=ADATA(IPCOMP,3)
        Cs3=ADATA(IPCOMP,4)
        RTs4=ADATA(IPCOMP,5)
        RTs3=ADATA(IPCOMP,6)
        Ao=ADATA(IPCOMP,7)
        Ai=ADATA(IPCOMP,8)
        Di=ADATA(IPCOMP,9)
        RL=ADATA(IPCOMP,10)
        Ue=ADATA(IPCOMP,11)

        Te=PCNTMF(IPCOMP)
        call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
        if(closea) Te=Ts4

C Determine specific heat values
        CPw0=SHTFLD(3,Tw0)
        CPw1=SHTFLD(3,Tw1)
        CPw2=SHTFLD(3,Tw2)

C Establish outstanding arguements used in primitive part subroutines

C first flow velocity
        RMw=3.14159*(Di**2)/4.*RL*RHOFLD(3,Tw1)
        Uw=Rmw0/(RHOFLD(3,Tw0)*3.14159*(Di**2)/4.)

C then inside convective heat transfer coefficient
        Nw=3
        Call CVCOEFF1(Nw, Tw1,Uw,Di,hi)
C then the outside convective heat transfer coefficient assuming
C Do is 5% greater than Di
        Na=1
        Do=Di*1.05
        Call CVCOEFF2(Na,Ts4,Te,Ue,Do,ho)

C then modify heat transfer coefficients by re-arranging input data
        C34=(Ao+Ai)/(RTs3+RTs4)
        ho=1./(1./ho+RTs4/2.)
        hi=1./(1./hi+RTs3/2.)

        Cw0=Rmw0*Cpw0
        Cw1=Rmw1*Cpw1
        Cw2=Rmw2*Cpw2
        Ces=ho*Ao
        Csw=hi*Ai

C Check if inside waterr transport delay is required for future time step
        DY=(Uw*TIMSEC)/RL
        IF (DY.lt.1.0.and.abs(tw0P).lt.small) THEN
          DELAY=1.
          N=int(1./DY)
        ELSE
          DELAY=0.
        ENDIF
        Call TRNSDLAY(IPCOMP,DELAY,Tw0P,Tw1P,Ts3P,N,DY,Cw1,Csw,
     &                Tw0,Tw1,Tw2)

C Calculate current component time-constant TC based on response
C of solid node s3 to change of incoming water temperature;
C for solid node s3
        TCs1=(RMs3*Cs3)/(hi*Ai)
C for water node w1
        TCw2=RMw*Cpw1/(Rmw1*Cpw1)
 10     CALL TIMCONST(TCs1,TCw2,TC(IPCOMP))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
        IF(IMPEXP.EQ.1) THEN
          ALPHA=1.
        ELSE IF(IMPEXP.EQ.2) THEN
          ALPHA=RATIMP
        ELSE IF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
            ALPHA=1.
          ELSE
            ALPHA=RATIMP
          END IF
        ELSE IF(IMPEXP.EQ.4) THEN
          RMs3=0.
          RMs4=0.
          RMa=0.
          ALPHA=1.
        END IF

      END IF

C Determine matrix coefficients
      CALL PP11(IPCOMP,ISTATS,Conn(1),Conn(2),
     &          C34,Cs4,Cs3,Ts4P,Ts3P,RMs4,RMs3)

      CALL PP24(IPCOMP,ISTATS,Conn(1),
     &          Ces,CesP,RMs4,Cs4,Te,TeP,Ts4P)

      CALL PP43(IPCOMP,ISTATS,Conn(2),Conn(3),DELAY,Tw1,Tw2,
     &          Csw,CswP,Cw0,Cw0P,Cw2,Cw2P,Cs3,Cpw1,Cpw1P,Cpw2,
     &          RMs3,RMw,Tw0P,Tw1P,Tw2P,Ts3P)

      COUT(1)=A(11,1)+A(24,1)
      COUT(2)=A(11,2)
      COUT(3)=A(11,3)
      COUT(4)=A(11,4)+A(43,1)
      COUT(5)=A(43,2)
      COUT(6)=A(43,4)
      COUT(7)=A(43,5)
      COUT(8)=A(43,6)
      COUT(9)=A(43,8)
      COUT(10)=A(43,9)
      COUT(11)=A(43,11)
      COUT(12)=B(11,1)+B(24,1)
      COUT(13)=B(11,2)+B(43,1)
      COUT(14)=B(43,2)
      COUT(15)=B(43,3)

C Store component variables future values
      IF(ISTATS.EQ.1) THEN
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCDATF(IPCOMP,1)=Ces
        PCDATF(IPCOMP,2)=Csw
        PCDATF(IPCOMP,3)=Cw0
        PCDATF(IPCOMP,4)=Cw2
        PCDATF(IPCOMP,5)=Cpw1
        PCDATF(IPCOMP,6)=Te
      ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 4 node (ISV>19) WCH water pipe'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' delay  = ',delay,' (-)'
            WRITE(ITU,*) ' N      = ',N,' (-)'
            WRITE(ITU,*) ' Tw0    = ',Tw0,' (C)'
            WRITE(ITU,*) ' Tw1    = ',Tw1,' (C)'
            WRITE(ITU,*) ' Tw2    = ',Tw2,' (C)'
            WRITE(ITU,*) ' Ts3    = ',Ts3,' (C)'
            WRITE(ITU,*) ' Ts4    = ',Ts4,' (C)'
            WRITE(ITU,*) ' Cw0    = ',Cw0,' (W/K)'
            WRITE(ITU,*) ' Cw1    = ',Cw1,' (W/K)'
            WRITE(ITU,*) ' Cw2    = ',Cw2,' (W/k)'
            WRITE(ITU,*) ' C34    = ',C34,' (W/k)'
            WRITE(ITU,*) ' Uw     = ',Uw,' (m/s)'
            WRITE(ITU,*) ' hi     = ',hi,' (W/K.m^2)'
            WRITE(ITU,*) ' ho     = ',ho,' (W/K.m^2)'

         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=15
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
           IX1=(IPCOMP/4)*4
           IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP57C'

      RETURN
      END


C ******************** CMP58C ********************

C CMP58C generates for plant component IPCOMP with plant db code 580 ie.
C 4 node (ISV>19) AC air duct with transport delay & built from
C primitive parts
C node 1 represents duct outer layer
C node 2 represents duct inner layer
C node 3 represents air in contact
C node 4 represents leaving air 
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance(ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Duct outer layer total mass (kg)
C            2 Duct inner layer total mass (kg)
C            3 Specific heat of outer layer (weighed average) (J/kgK)
C            4 Specific heat of inner layer (weighed average) (J/kgK)
C            5 Thermal resistance of outer layer (m^2.C/W)
C            6 Thermal resistance of inner layer (m^2.C/W)
C            7 Outside heat transfer area (m^2)
C            8 Inside heat transfer area (m^2)
C            9 Inside effective duct diameter (m)
C           10 Duct length (m)
C           11 Ambient air velocity (m/s)
C     BDATA: none
C     CDATA: none


      SUBROUTINE CMP58C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE)

      CHARACTER SURFACE*3
      DIMENSION Conn(4)
      logical closea,closeb,closec

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
 
C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP58C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3

C set the node connectivity values
      Conn(1)=2.
      Conn(2)=2.
      Conn(3)=1.
      Conn(4)=1.

C To generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First mark air 1st phase mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
        ICSV(INOD3,2)=1
        CSVI(INOD3,2)=CSVF(INOD3,2)
        ICSV(INOD4,2)=1
        CSVI(INOD4,2)=CSVF(INOD4,2)
        Rma1=CSVI(INOD3,2)
        Rma2=CSVI(INOD4,2)

C Then mark air 2nd phase mass flow rates for iteration
        ICSV(INOD3,3)=1
        CSVI(INOD3,3)=CSVF(INOD3,3)
        ICSV(INOD4,3)=1
        CSVI(INOD4,3)=CSVF(INOD4,3)
        Rmv1=CSVI(INOD3,3)
        Rmv2=CSVI(INOD4,3)

C Mark current solid and air temperatures for iteration 
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)
        ICSV(INOD4,1)=1
        CSVI(INOD4,1)=CSVF(INOD4,1)
        Ts4=CSVI(INOD1,1)
        Ts3=CSVI(INOD2,1)
        Ta1=CSVI(INOD3,1)
        Ta2=CSVI(INOD4,1)

C Establish incoming air temperature & flow rates
        Ta0=CONVAR(ICON1,1)
        Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
        Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)

C Identify temperatures & flow rates of present time step
        Ta0P=PCTP(ICON1)
        Ta1P=CSVP(INOD3,1)
        Ta2P=CSVP(INOD4,1)
        Ts3P=CSVP(INOD2,1)
        Ts4P=CSVP(INOD1,1)
        Rma1P=CSVP(INOD3,2)
        Rma2P=CSVP(INOD4,2)
        Rmv1P=CSVP(INOD3,3)
        Rmv2P=CSVP(INOD4,3)

C Restore PCDATP values
        CesP=PCDATP(IPCOMP,1)
        CasP=PCDATP(IPCOMP,2)
        Ca0P=PCDATP(IPCOMP,3)
        Ca2P=PCDATP(IPCOMP,4)
        CavP=PCDATP(IPCOMP,5)
        CpamP=PCDATP(IPCOMP,6)
        CdP=PCDATP(IPCOMP,7)
        TeP=PCDATP(IPCOMP,8)
        SurP=PCDATP(IPCOMP,9)
        Ga1P=PCDATP(IPCOMP,10)

        TeP=PCNTMP(IPCOMP)

C Identify the specified ADATA and CDATA
        RMs4=ADATA(IPCOMP,1)
        RMs3=ADATA(IPCOMP,2)
        Cs4=ADATA(IPCOMP,3)
        Cs3=ADATA(IPCOMP,4)
        RTs4=ADATA(IPCOMP,5)
        RTs3=ADATA(IPCOMP,6)
        Ao=ADATA(IPCOMP,7)
        Ai=ADATA(IPCOMP,8)
        Di=ADATA(IPCOMP,9)
        RL=ADATA(IPCOMP,10)
        Ue=ADATA(IPCOMP,11)

        Te=PCNTMF(IPCOMP)
        call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
        if(closea) Te=Ts4

C Determine specific heat and latent heat values
        CPa0=SHTFLD(1,Ta0)
        CPv0=SHTFLD(2,Ta0)
        CPa1=SHTFLD(1,Ta1)
        CPv1=SHTFLD(2,Ta1)
        CPa2=SHTFLD(1,Ta2)
        CPv2=SHTFLD(2,Ta2)

        Hfg=CNDWAT(Ta1)

C Establish outstanding arguements used in primitive part subroutines

        RMa=3.14159*(Di**2)/4.*RL*RHOFLD(1,Ta1)
        Ua=Rma0/(RHOFLD(1,Ta0)*3.14159*(Di**2)/4.)

        ho=5.62+3.9*Ue
        N=1
        CALL CVCOEFF1(N,Ta0,Ua,Di,hi)

C then modify heat transfer coefficients by re-arranging input data
        C34=(Ao+Ai)/(RTs3+RTs4)
        ho=1./(1./ho+RTs4/2.)
        hi=1./(1./hi+RTs3/2.)

C determine Rmc, Ga0 for trace
        Rmc=Rmv0-Rmv2
        call eclose(Rma0,0.00,0.0001,closeb)
        call eclose(Rma1,0.00,0.0001,closec)
        if(closeb.or.closec) go to 15
        Ga0=Rmv0/Rma0
        Ga1=Rmv1/Rma1

        Ca0=Rma0*Cpa0+Rmv0*Cpv0
        Ca1=Rma1*Cpa1+Rmv1*Cpv1
        Cpam=Ca1/Rma1

        Ca2=Rma2*Cpa2+Rmv2*Cpv2
 15     Ces=ho*Ao
        Cas=hi*Ai
        Cav=-Hfg*(Rmv0-Rmv2)

C Check if inside air transport delay is required for future time step
        DY=(Ua*TIMSEC)/RL
        IF (DY.lt.1.0.and.abs(ta0P).lt.small) THEN
          DELAY=1.

C avoid DY=0 case
          IF(DY.LT.0.0001) DY=0.0001
            N=int(1./DY)
          ELSE
            DELAY=0.
          ENDIF

C suppress transport delay in case condensation occurred
          IF (abs(Cav).GT.small) delay=0.

          Call TRNSDLAY(IPCOMP,DELAY,Ta0P,Ta1P,Ts3P,N,DY,Ca1,Cas,
     &               Ta0,Ta1,Ta2)

C Calculate current component time-constant TC based on response
C of solid node s3 to change of incoming air temperature;
C for solid node s3
          TCs1=(RMs3*Cs3)/(hi*Ai)
C for air node a1
          TCa2=RMa*Cpa1/(Rma1*Cpa1)
          CALL TIMCONST(TCs1,TCa2,TC(IPCOMP))

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
          IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
          ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
          ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
              ALPHA=1.
            ELSE
              ALPHA=RATIMP
            END IF
          ELSE IF(IMPEXP.EQ.4) THEN
            RMs3=0.
            RMs4=0.
            RMa=0.
            ALPHA=1.
          END IF

C << indentation pattern breaks - might be missing an endif? >>

C To generate coefficients for mass balance equations
      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN

        Ai=ADATA(IPCOMP,5)
        Cas=PCDATF(IPCOMP,2)
        Cpam=PCDATF(IPCOMP,6)
        Cd=PCDATF(IPCOMP,7)
        Ga1=PCDATF(IPCOMP,10)
        Ts3=CSVF(INOD2,1)

        hi=Cas/Ai

C Detect duct interior surface condition: DRY or WET; formulae from 
C ASHRAE Handbook:Fundamental for 0 to 70 & -60 to 0 deg.C respectively

        if (ga1.le.0.) go to 10
        Pw=101325.*Ga1/(0.62198+Ga1)
        Td=-35.957-1.8726*LOG(Pw)+1.1689*(LOG(Pw)**2)

        IF(Td.LE.0.0) THEN
          Td=-60.45-7.0322*LOG(Pw)+0.3700*(LOG(Pw)**2)
        END IF

 10     IF (Td.LE.Ts3.AND.abs(CdP).lt.small) THEN
          SURFACE="DRY"
          SurF=0.
        ELSE
          SURFACE="WET"
          SurF=1.
        END IF

        PCDATF(IPCOMP,9)=Surf

      END IF

C Determine matrix coefficients
      CALL PP11(IPCOMP,ISTATS,Conn(1),Conn(2),
     &          C34,Cs4,Cs3,Ts4P,Ts3P,RMs4,RMs3)

      CALL PP24(IPCOMP,ISTATS,Conn(1),
     &          Ces,CesP,RMs4,Cs4,Te,TeP,Ts4P)

      Cdmax=0.
      CALL PP41(IPCOMP,ISTATS,SURFACE,Conn(2),Conn(3),DELAY,Ta1,Ta2,
     &          Cas,CasP,Ca0,Ca0P,Ca2,Ca2P,Cs3,Cav,CavP,
     &          Cpam,CpamP,Cpa2,hi,Ai,Cd,CdP,Cdmax,
     &          RMs3,RMa,Rma1,Rmv1,Ta0P,Ta1P,Ta2P,Ts3,Ts3P)

      COUT(1)=A(11,1)+A(24,1)
      COUT(2)=A(11,2)
      COUT(3)=A(11,3)
      COUT(4)=A(11,4)+A(41,1)
      COUT(5)=A(41,2)
      COUT(6)=A(41,4)
      COUT(7)=A(41,5)
      COUT(8)=A(41,6)
      COUT(9)=A(41,8)
      COUT(10)=A(41,9)
      COUT(11)=A(41,11)
      COUT(12)=B(11,1)+B(24,1)
      COUT(13)=B(11,2)+B(41,1)
      COUT(14)=B(41,2)
      COUT(15)=B(41,3)

C Store component variables future values
      IF(ISTATS.EQ.1) THEN
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCDATF(IPCOMP,1)=Ces
        PCDATF(IPCOMP,2)=Cas
        PCDATF(IPCOMP,3)=Ca0
        PCDATF(IPCOMP,4)=Ca2
        PCDATF(IPCOMP,5)=Cav
        PCDATF(IPCOMP,6)=Cpam
        PCDATF(IPCOMP,8)=Te
        PCDATF(IPCOMP,10)=Ga1
      ELSEIF(ISTATS.EQ.3) THEN
        PCDATF(IPCOMP,7)=Cd
      ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 4 node (ISV>19) AC air duct'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' delay  = ',delay,' (-)'
            WRITE(ITU,*) ' Ua     = ',Ua,' (m/s)'
            WRITE(ITU,*) ' ho     = ',ho,' (W/K.m^2)'
            WRITE(ITU,*) ' hi     = ',hi,' (W/K.m^2)' 
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
            WRITE(ITU,*) ' Ta2    = ',Ta2,' (C)'
            WRITE(ITU,*) ' Ca0    = ',Ca0,' (W/K)'
            WRITE(ITU,*) ' Cav    = ',Cav,' (W/K)'
            WRITE(ITU,*) ' Ca2    = ',Ca2,' (W/k)'
            WRITE(ITU,*) ' Cd    = ',Cd,' (kg)'
            WRITE(ITU,*) ' Rmc   = ',Rmc,' (kg/s)'
            WRITE(ITU,*) ' Hfg    = ',Hfg,' (J/kg)'     
            WRITE(ITU,*) ' Rmv0   = ',Rmv0,' (kg/s)'
            WRITE(ITU,*) ' Rmv2   = ',Rmv2,' (kg/s)'
            WRITE(ITU,*) ' Ga0    = ',Ga0,' (kg/kg)'
            WRITE(ITU,*) ' Ga1    = ',Ga1,' (kg/kg)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=15
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
           IX1=(IPCOMP/4)*4
           IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP58C'

      RETURN
      END


C ******************** CMP60C ********************

C CMP60C generates for plant component IPCOMP with plant db code 600 ie.
C 4 node (ISV>19) AC duct electric heater & built from primitive parts
C node 1 represents heater element
C node 2 represents air in contact with heater
C node 3 represents metallic casing
C node 4 represents air in contact with casing
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, and present-time coefficients) for energy balance(ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1 Heater element total mass (kg)
C            2 Metallic casing total mass (kg)
C            3 Specific heat of metallic casing (J/kgK)
C            4 UA module (W/K)
C            5 Heater element heat transfer area (m^2)
C            6 Casing inside heat transfer area (m^2)
C            7 air flow cross-sectional area (m^2)

C     BDATA: 1 Specific heat of heater element (weighed average) (J/kgK)
C     CDATA: 1 Electric power input  (W)

      SUBROUTINE CMP60C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE)

      DIMENSION Conn(4),A1(11),B1(3)
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP60C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,2,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3

C set the node connectivity values
      Conn(1)=3.
      Conn(2)=1.
      Conn(3)=3.
      Conn(4)=1.

C To generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First mark air 1st phase mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
         ICSV(INOD2,2)=1
         CSVI(INOD2,2)=CSVF(INOD2,2)
         ICSV(INOD4,2)=1
         CSVI(INOD4,2)=CSVF(INOD4,2)
         Rma2=CSVI(INOD2,2)
         Rma4=CSVI(INOD4,2)

C Then mark air 2nd phase mass flow rates for iteration
         ICSV(INOD2,3)=1
         CSVI(INOD2,3)=CSVF(INOD2,3)
         ICSV(INOD4,3)=1
         CSVI(INOD4,3)=CSVF(INOD4,3)
         Rmv2=CSVI(INOD2,3)
         Rmv4=CSVI(INOD4,3)

C Mark current solid and air temperatures for iteration 
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD2,1)=1
         CSVI(INOD2,1)=CSVF(INOD2,1)
         ICSV(INOD3,1)=1
         CSVI(INOD3,1)=CSVF(INOD3,1)
         ICSV(INOD4,1)=1
         CSVI(INOD4,1)=CSVF(INOD4,1)
         Ts1=CSVI(INOD1,1)
         Ta2=CSVI(INOD2,1)
         Ts3=CSVI(INOD3,1)
         Ta4=CSVI(INOD4,1)

C Establish incoming air temperature & flow rates
         Ta0=CONVAR(ICON1,1)
         Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
         Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)

C Identify temperatures & flow rates of present time step
         Ta0P=PCTP(ICON1)
         Ts1P=CSVP(INOD1,1)
         Ta2P=CSVP(INOD2,1)
         Ts3P=CSVP(INOD3,1)
         Ta4P=CSVP(INOD4,1)

         Rma2P=CSVP(INOD2,2)
         Rma4P=CSVP(INOD4,2)

         Rmv2P=CSVP(INOD2,3)
         Rmv4P=CSVP(INOD4,3)


C Restore PCDATP values
         CesP=PCDATP(IPCOMP,1)
         Cas1P=PCDATP(IPCOMP,2)
         Cas3P=PCDATP(IPCOMP,3)
         Ca0P=PCDATP(IPCOMP,4)
         Ca2P=PCDATP(IPCOMP,5)
         Ca4P=PCDATP(IPCOMP,6)
         Cav1P=PCDATP(IPCOMP,7)
         Cav3P=PCDATP(IPCOMP,8)
         Cd1P=PCDATP(IPCOMP,9)
         Cd3P=PCDATP(IPCOMP,10)
         Sur1P=PCDATP(IPCOMP,11)
         Sur3P=PCDATP(IPCOMP,12)
         Ga2P=PCDATP(IPCOMP,13)
         Ga4P=PCDATP(IPCOMP,14)
         QP=PCDATP(IPCOMP,15)
         C13P=PCDATP(IPCOMP,16)
         CpamP=PCDATP(IPCOMP,17)

         TeP=PCNTMP(IPCOMP)


C Identify the specified ADATA, BDATA and CDATA
         RMs1=ADATA(IPCOMP,1)
         RMs3=ADATA(IPCOMP,2)
         Cs3=ADATA(IPCOMP,3)
         UA=ADATA(IPCOMP,4)
         Ar=ADATA(IPCOMP,5)
         Ai=ADATA(IPCOMP,6)
         Ax=ADATA(IPCOMP,7)

         Cs1=BDATA(IPCOMP,1)

         Q=CDATA(IPCOMP,1)

         call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
         if(closea) UA=small
         Te=PCNTMF(IPCOMP)

C Determine specific heat and latent heat values
         CPa0=SHTFLD(1,Ta0)
         CPv0=SHTFLD(2,Ta0)
         CPa2=SHTFLD(1,Ta2)
         CPv2=SHTFLD(2,Ta2)
         CPa4=SHTFLD(1,Ta4)
         CPv4=SHTFLD(2,Ta4)

         Hfg=CNDWAT(Ta2)

C Establish outstanding arguements used in primitive part subroutines

         RMa=small
         Ua0=Rma0/(RHOFLD(1,Ta0)*Ax)

         Na=1
         Di=SQRT(Ax*4/3.1416)
         CALL CVCOEFF1(Na,Ta0,Ua0,Di,hi)
         hr=27.42*(Ua0**0.8)

C assume black body radiation and heater totally enclosed by casing for
C radiation heat transfer
         F13=1.
         Boltman=5.67E-8
         ATs1=273.15+Ts1
         ATs3=273.15+Ts3

         h13=F13*Boltman*(ATs1**2+ATs3**2)*(ATs1+ATs3)
         C13=h13*Ar

C determine Rmc, Ga0 for trace
         Rmc=Rmv0-Rmv2

         Rma0=amax1(small,Rma0)
         Rma2=amax1(small,Rma2)
         Rma4=amax1(small,Rma4)

         Ga0=Rmv0/Rma0
         Ga2=Rmv2/Rma2
         Ga4=Rmv4/Rma4

         Ca0=Rma0*Cpa0+Rmv0*Cpv0
         Ca2=Rma2*Cpa2+Rmv2*Cpv2
         Ca4=Rma4*Cpa4+Rmv4*Cpv4

         Cpam=Ca2/Rma2

         Ces=1./(1./UA-1./(hi*Ai))
         Cas1=hr*Ar
         Cas3=hi*Ai

C check Cav
         Cav1=-Hfg*(Rmv0-Rmv2)
         Cav3=-Hfg*(Rmv2-Rmv4)

C Calculate current component time-constant TC (max. of casing and heater)
         hr=amax1(small,hr)
         TCr=RMs1*Cs1/(hr*Ar)
         TCs=(RMs3*Cs3)/UA
         TC(IPCOMP)=AMAX1(TCr,TCs)

C Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
         IF(IMPEXP.EQ.1) THEN
            ALPHA=1.
         ELSE IF(IMPEXP.EQ.2) THEN
            ALPHA=RATIMP
         ELSE IF(IMPEXP.EQ.3) THEN
            IF(TIMSEC.GT.0.63*TC(IPCOMP)) THEN
               ALPHA=1.
            ELSE
               ALPHA=RATIMP
            END IF
         ELSE IF(IMPEXP.EQ.4) THEN
            RMs3=0.
            RMs4=0.
            RMs5=0.
            RMa=0.
            ALPHA=1.
         END IF

C To generate coefficients for mass balance equations
      ELSE IF(ISTATS.EQ.2.OR.ISTATS.EQ.3) THEN
            Ar=ADATA(IPCOMP,5)
            Ai=ADATA(IPCOMP,6)
            Cas1=PCDATF(IPCOMP,2)
            Cas3=PCDATF(IPCOMP,3)
            Cpam=PCDATF(IPCOMP,17)
            Cd1=PCDATF(IPCOMP,9)
            Cd3=PCDATF(IPCOMP,10)
            Ga2=PCDATF(IPCOMP,13)
            Ga4=PCDATF(IPCOMP,14)
            Ts1=CSVF(INOD1,1)
            Ts3=CSVF(INOD3,1)

            hr=Cas1/Ar
            hi=Cas3/Ai

        CALL SURFWET(Ga2,Ts1,Cd1P,Surf1)
        CALL SURFWET(Ga4,Ts3,Cd3P,Surf3)

      END IF

C Determine matrix coefficients

       CALL  PP24(IPCOMP,ISTATS,Conn(2),
     &                Ces,CesP,RMs3,Cs3,Te,TeP,Ts3P)

       CALL PP31(IPCOMP,ISTATS,Conn(1),Conn(3),
     &                C13,C13P,Cs1,Cs3,Ts1P,Ts3P,RMs1,RMs3)

       CALL PP44(IPCOMP,ISTATS,Surf1,Conn(1),Conn(2),
     &               Cas1,Cas1P,Ca0,Ca0P,Ca2,Ca2P,Cs1,Cav1,Cav1P,
     &               Cpam,CpamP,hr,Ar,Cd1,Cd1P,
     &               RMs1,RMa,Rmv0,Ga0,Ta0P,Ta2,Ta2P,Ts1,Ts1P)

         A1(1)=A(44,1)
         A1(2)=A(44,2)
         A1(3)=A(44,3)
         A1(4)=A(44,4)
         A1(6)=A(44,6)
         B1(1)=B(44,1)
         B1(2)=B(44,2)
         B1(3)=B(44,3)

       CALL PP44(IPCOMP,ISTATS,Surf3,Conn(3),Conn(4),
     &               Cas3,Cas3P,Ca2,Ca2P,Ca4,Ca4P,Cs3,Cav3,Cav3P,
     &               Cpam,CpamP,hr,Ar,Cd3,Cd3P,
     &               RMs3,RMa,Rmv2,Ga2,Ta2P,Ta4,Ta4P,Ts3,Ts3P)

       CALL PP101(IPCOMP,ISTATS,Conn(1),Cs1,RMs1,Ts1P,Q,QP)

         COUT(1)=A(31,1)+A1(1)+A(101,1)
         COUT(2)=A1(2)
         COUT(3)=A(31,2)
         COUT(4)=A1(3)
         COUT(5)=A1(4)
         COUT(6)=A(31,3)
         COUT(7)=A(24,1)+A(31,4)+A(44,1)
         COUT(8)=A(44,2)
         COUT(9)=A(44,6)
         COUT(10)=A(44,3)
         COUT(11)=A(44,4)
         COUT(12)=A1(6)
         COUT(13)=B(31,1)+B1(1)+B(101,1)
         COUT(14)=B1(2)
         COUT(15)=B(24,1)+B(31,2)+B(44,1)
         COUT(16)=B(44,2)

C Store component variables future values
        IF(ISTATS.EQ.1) THEN
         PCTF(ICON1)=CONVAR(ICON1,1)
         PCDATF(IPCOMP,1)=Ces
         PCDATF(IPCOMP,2)=Cas1
         PCDATF(IPCOMP,3)=Cas3
         PCDATF(IPCOMP,4)=Ca0
         PCDATF(IPCOMP,5)=Ca2
         PCDATF(IPCOMP,6)=Ca4
         PCDATF(IPCOMP,7)=Cav1
         PCDATF(IPCOMP,8)=Cav3
         PCDATF(IPCOMP,13)=Ga2
         PCDATF(IPCOMP,14)=Ga4
         PCDATF(IPCOMP,15)=Q
         PCDATF(IPCOMP,16)=C13
         PCDATF(IPCOMP,17)=Cpam

        ELSEIF(ISTATS.EQ.3) THEN
         PCDATF(IPCOMP,9)=Cd1
         PCDATF(IPCOMP,10)=Cd3
         PCDATF(IPCOMP,11)=Surf1
         PCDATF(IPCOMP,12)=Surf3

        ENDIF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 4 node (ISV>19) AC duct heater'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Ta2    = ',Ta2,' (C)'
            WRITE(ITU,*) ' Ta4    = ',Ta4,' (C)'
            WRITE(ITU,*) ' Ts1    = ',Ts1,' (C)'
            WRITE(ITU,*) ' Ts3    = ',Ts3,' (C)'
            WRITE(ITU,*) ' Ca0    = ',Ca0,' (W/K)'
            WRITE(ITU,*) ' Ca2    = ',Ca2,' (W/K)'
            WRITE(ITU,*) ' Ca4    = ',Ca4,' (W/k)'
            WRITE(ITU,*) ' C13    = ',C13,' (W/k)'
            WRITE(ITU,*) ' Rmc   = ',Rmc,' (kg/s)'     
            WRITE(ITU,*) ' Rmv0   = ',Rmv0,' (kg/s)'
            WRITE(ITU,*) ' Rmv4   = ',Rmv4,' (kg/s)'
            WRITE(ITU,*) ' Ga0    = ',Ga0,' (kg/kg)'
            WRITE(ITU,*) ' Ga4    = ',Ga4,' (kg/kg)'

         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=16
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP60C'

      RETURN
      END


C **************************** CMP64C ****************************

C CMP64C generates for plant component IPCOMP with plant db code 640
C i.e. 1 node (ISV=20) water/steam flow converger 
C built from primitive parts;
C   node 1 represents mixed fluid
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1  Working pressure (bar)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP64C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      PARAMETER (SMALL=1.0E-20)
      PARAMETER (QSMALL=1.0E-6)

      REAL      COUT(MPCOE)

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP64C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.

C First mark 1st phase (liquid water) mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
         ICSV(INOD1,2)=1
         CSVI(INOD1,2)=CSVF(INOD1,2)

C Then mark 2nd phase (steam vapour) mass flow rates for iteration
         ICSV(INOD1,3)=1
         CSVI(INOD1,3)=CSVF(INOD1,3)

C Start with establishing incoming fluid temperatures Tw01 & Tw02
            Tr01=CONVAR(ICON1,1)
            Tr02=CONVAR(ICON2,1)

C Mark current fluid temperatures for iteration
            ICSV(INOD1,1)=1
            CSVI(INOD1,1)=CSVF(INOD1,1)
            Tr1=CSVI(INOD1,1)

C Get fluid temperature of present time step
            Tr1P=CSVP(INOD1,1)

C Determine values of valuables used in this component e.g. fluid mass 
C flow rates, specific heats, and latent heat (note: mass of
C encapsulated water & steam vapour to be neglected)

C Firstly identify the specified ADATA 
            Psat=ADATA(IPCOMP,1)

C reverse flow of incoming fluid has been set zero by MZPADJ if any
            Rmw01=PCONDR(ICON1)*CONVAR(ICON1,2)
            Rmw02=PCONDR(ICON2)*CONVAR(ICON2,2)
            Rmv01=PCONDR(ICON1)*CONVAR(ICON1,3)
            Rmv02=PCONDR(ICON2)*CONVAR(ICON2,3)

            Rmw1=CSVI(INOD1,2)
            Rmv1=CSVI(INOD1,3)

C then determine Tsat for the working pressure and get specific heats
C and saturation data
        call SATTEMP(Psat,Tsat)
        call SPECHEAT(Tr1,Tsat,CPw1,CPv1,Hfg)

        call SPECHEAT(Tr01,Tsat,CPw01,CPv01,Hfg)
        call SPECHEAT(Tr02,Tsat,CPw02,CPv02,Hfg)

C Hf, Hg, CPf & CPg values at 5 bars are given here; 
C (subprogramme to be add later!)
        Hf=640.*1000.
        Hg=2749.*1000.
        Cpf=4316.
        Cpg=2330.

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
           TC(IPCOMP)=0.

C Establish matrix equation coefficients
      IF(Rmv01.LT.QSMALL.and.Rmv02.LT.QSMALL) THEN
         CALL PP63(IPCOMP,ISTATS,Tr1P,
     &               Cpw01,Cpv01,Cpw02,Cpv02,Cpw1,Cpv1,
     &               Rmw01,Rmv01,Rmw02,Rmv02)
         COUT(1)=A(63,1)
         COUT(2)=A(63,2)
         COUT(3)=A(63,3)
         COUT(4)=B(63,1)
         GO TO 20
      ENDIF

      CALL PP62(IPCOMP,ISTATS,Tsat,Hf,Hg,Cpf,Cpg,
     &               Cpw01,Cpv01,Cpw02,Cpv02,Tr01,Tr02,
     &               Rmw01,Rmv01,Rmw02,Rmv02)
 
         COUT(1)=A(62,1)
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=B(62,1)

C Trace output
 20   IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) water/steam flow converger',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rmw01    = ',Rmw01,' (Kg/s)'
            WRITE(ITU,*) ' Rmv01    = ',Rmv01,' (Kg/s)'
            WRITE(ITU,*) ' Rmw02    = ',Rmw02,' (Kg/s)'
            WRITE(ITU,*) ' Rmv02    = ',Rmv02,' (Kg/s)'
            WRITE(ITU,*) ' Cpf    = ',Cpf,' (J/Kg.K)'
            WRITE(ITU,*) ' Cpg    = ',Cpg,' (J/Kg.K)'
            WRITE(ITU,*) ' Cpw01    = ',Cpw01,' (J/Kg.K)'
            WRITE(ITU,*) ' Cpv01    = ',Cpv01,' (J/Kg.K)'
            WRITE(ITU,*) ' Cpw02    = ',Cpw02,' (J/Kg.K)'
            WRITE(ITU,*) ' Cpv02    = ',Cpv02,' (J/Kg.K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Tr01    = ',Tr01,' (C)'
            WRITE(ITU,*) ' Tr02    = ',Tr02,' (C)'
            WRITE(ITU,*) ' Tr1    = ',Tr1,' (C)'
            WRITE(ITU,*) ' Hfg    = ',Hfg,' (J/kg)' 
            WRITE(ITU,*) ' Hf    = ',Hf,' (J/kg)' 
            WRITE(ITU,*) ' Hg    = ',Hg,' (J/kg)' 
            WRITE(ITU,*) ' Rmw1   = ',Rmw1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' Tsat   = ',Tsat,' (deg.C)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=4
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP64C'

      RETURN
      END


C **************************** CMP65C ****************************

C CMP65C generates for plant component IPCOMP with plant db code 650
C i.e. 3 nodes (ISV=20) water/steam flow diverger 
C built from primitive parts;
C   node 1 represents fluid at first outlet
C   node 2 represents fluid at second outlet
C   node 3 represents fluid at inlet
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1  flow ratio (first outlet to inlet)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP65C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      PARAMETER (SMALL=1.0E-15)

      REAL      COUT(MPCOE)
      logical closea,close

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP65C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.
      if(CSVF(INOD2,2).LT.0.) CSVF(INOD2,2)=0.
      if(CSVF(INOD2,3).LT.0.) CSVF(INOD2,3)=0.
      if(CSVF(INOD3,2).LT.0.) CSVF(INOD3,2)=0.
      if(CSVF(INOD3,3).LT.0.) CSVF(INOD3,3)=0.

C First mark 1st phase (liquid water) mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)
      ICSV(INOD3,2)=1
      CSVI(INOD3,2)=CSVF(INOD3,2)

C Then mark 2nd phase (steam vapour) mass flow rates for iteration
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      ICSV(INOD2,3)=1
      CSVI(INOD2,3)=CSVF(INOD2,3)
      ICSV(INOD3,3)=1
      CSVI(INOD3,3)=CSVF(INOD3,3)

C Start with establishing incoming fluid temperatures Tr0
      Tr0=CONVAR(ICON1,1)

C Mark current fluid temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD3,1)=1
      CSVI(INOD3,1)=CSVF(INOD3,1)

      Tr1=CSVI(INOD1,1)
      Tr2=CSVI(INOD2,1)
      Tr3=CSVI(INOD3,1)

C Determine values of valuables used in this component e.g. fluid mass 
C flow rates, specific heats, and latent heat (note: mass of
C encapsulated water & steam vapour to be neglected)

C Firstly identify the specified ADATA 
      R=ADATA(IPCOMP,1)

C reverse flow of incoming fluid has been set zero by MZPADJ if any
      Rmw0=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)
      Rmr0=Rmw0+Rmv0
      Rmw1=CSVI(INOD1,2)
      Rmv1=CSVI(INOD1,3)

C Flow rates at nodes 2 & 3 for reporting purpose
      Rmw2=CSVI(INOD2,2)
      Rmv2=CSVI(INOD2,3)
      Rmw3=CSVI(INOD3,2)
      Rmv3=CSVI(INOD3,3)

      call eclose(R,-99.00,0.001,closea)
      IF(.not.closea) GO TO 10

C Get value of R
      call eclose(Rmr0,0.00,0.00001,close)
      IF(.NOT.close) THEN
C      IF(Rmr0.NE.0.) THEN
        R=(Rmw1+Rmv1)/Rmr0
      ELSE
        R=0.5
      ENDIF

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10   CALL PP51(IPCOMP,ISTATS,R)

      COUT(1)=A(51,1)
      COUT(2)=A(51,4)
      COUT(3)=A(51,2)
      COUT(4)=A(51,3)
      COUT(5)=A(51,5)
      COUT(6)=1.
      COUT(7)=-1.
      COUT(8)=B(51,1)
      COUT(9)=B(51,2)
      COUT(10)=0.

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 nodes (ISV=20) water/steam flow diverger',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rmw0    = ',Rmw0,' (Kg/s)'
            WRITE(ITU,*) ' Rmv0    = ',Rmv0,' (Kg/s)'
            WRITE(ITU,*) ' Rmw1   = ',Rmw1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' Rmw2    = ',Rmw2,' (Kg/s)'
            WRITE(ITU,*) ' Rmv2    = ',Rmv2,' (Kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Tr0    = ',Tr0,' (C)'
            WRITE(ITU,*) ' Tr1    = ',Tr1,' (C)'
            WRITE(ITU,*) ' Tr2    = ',Tr2,' (C)' 
            WRITE(ITU,*) ' R   = ',R
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=10
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP65C'

      RETURN
      END

C **************************** CMP66C ****************************

C CMP66C generates for plant component IPCOMP with plant db code 660
C i.e. 3 nodes (ISV=21) moist air flow diverger 
C built from primitive parts;
C   node 1 represents air at first outlet
C   node 2 represents air at second outlet
C   node 3 represents air at inlet
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: 1  flow ratio (first outlet to inlet)
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP66C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
 
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      PARAMETER (SMALL=1.0E-15)

      REAL      COUT(MPCOE)
      logical closea,close

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP66C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C set the connectivity values
C         Conn(1)=1.

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.
      if(CSVF(INOD2,2).LT.0.) CSVF(INOD2,2)=0.
      if(CSVF(INOD2,3).LT.0.) CSVF(INOD2,3)=0.
      if(CSVF(INOD3,2).LT.0.) CSVF(INOD3,2)=0.
      if(CSVF(INOD3,3).LT.0.) CSVF(INOD3,3)=0.

C First mark 1st phase (dry air) mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)
      ICSV(INOD3,2)=1
      CSVI(INOD3,2)=CSVF(INOD3,2)

C Then mark 2nd phase (water vapour) mass flow rates for iteration
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      ICSV(INOD2,3)=1
      CSVI(INOD2,3)=CSVF(INOD2,3)
      ICSV(INOD3,3)=1
      CSVI(INOD3,3)=CSVF(INOD3,3)

C Start with establishing incoming fluid temperatures Tr0
      Ta0=CONVAR(ICON1,1)

C Mark current fluid temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD3,1)=1
      CSVI(INOD3,1)=CSVF(INOD3,1)

      Ta1=CSVI(INOD1,1)
      Ta2=CSVI(INOD2,1)
      Ta3=CSVI(INOD3,1)

C Determine values of valuables used in this component e.g. fluid mass 
C flow rates, specific heats, and latent heat (note: mass of
C encapsulated water vapour to be neglected)

C Firstly identify the specified ADATA 
      R=ADATA(IPCOMP,1)

C reverse flow of incoming fluid has been set zero by MZPADJ if any
      Rma0=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)
      Rma1=CSVI(INOD1,2)
      Rmv1=CSVI(INOD1,3)

C Flow rates at nodes 2 & 3 for reporting purpose
      Rma2=CSVI(INOD2,2)
      Rmv2=CSVI(INOD2,3)
      Rma3=CSVI(INOD3,2)
      Rmv3=CSVI(INOD3,3)

C check if R is user specified
      call eclose(R,-99.00,0.001,closea)
 
      IF(.not.closea) GO TO 10

C Get value of R if otherwise
      call eclose(Rma0,0.00,0.00001,close)
      IF(.NOT.close) THEN
C      IF(Rma0.NE.0.) THEN
        R=Rma1/Rma0
      ELSE
        R=0.
      ENDIF

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10   CALL PP51(IPCOMP,ISTATS,R)

      COUT(1)=A(51,1)
      COUT(2)=A(51,4)
      COUT(3)=A(51,2)
      COUT(4)=A(51,3)
      COUT(5)=A(51,5)
      COUT(6)=1.
      COUT(7)=-1.
      COUT(8)=B(51,1)
      COUT(9)=B(51,2)
      COUT(10)=0.

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 nodes (ISV=21) moist air flow diverger',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rma0    = ',Rma0,' (Kg/s)'
            WRITE(ITU,*) ' Rmv0    = ',Rmv0,' (Kg/s)'
            WRITE(ITU,*) ' Rma1   = ',Rma1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' Rma2    = ',Rma2,' (Kg/s)'
            WRITE(ITU,*) ' Rmv2    = ',Rmv2,' (Kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Ta0    = ',Ta0,' (C)'
            WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
            WRITE(ITU,*) ' Ta2    = ',Ta2,' (C)'
            WRITE(ITU,*) ' R   = ',R
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=10
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP66C'

      RETURN
      END

C **************************** CMP68C ****************************

C CMP68C generates for plant component IPCOMP with plant db code 680
C i.e. 1 node (ISV=21) moist air flow converger 
C built from primitive parts;
C   node 1 represents fluid at outlet
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: none
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP68C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTC/TC(MPCOM)

      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
 
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      PARAMETER (SMALL=1.0E-15)

      REAL      COUT(MPCOE)
      logical closeb,closec

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP68C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)

C mark 1st & second phase mass flow rates for iteration;
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      Rma3=CSVI(INOD1,2)
      Rmv3=CSVI(INOD1,3)

C Mark current air temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      Ta3=CSVI(INOD1,1)

C establish incoming air temperatures Ta1 & Ta2
      Ta1=CONVAR(ICON1,1)
      Ta2=CONVAR(ICON2,1)

C reverse flow of incoming fluid has been set zero by MZPADJ if any
      Rma1=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv1=PCONDR(ICON1)*CONVAR(ICON1,3)
      Rma2=PCONDR(ICON2)*CONVAR(ICON2,2)
      Rmv2=PCONDR(ICON2)*CONVAR(ICON2,3)

C Determine values of valuables used in this component e.g. fluid mass 
C flow rates, specific heats, and latent heat (note: mass of
C encapsulated water & steam vapour to be neglected)

C Determine specific heat and latent heat values
      CPa1=SHTFLD(1,Ta1)
      CPv1=SHTFLD(2,Ta1)
      CPa2=SHTFLD(1,Ta2)
      CPv2=SHTFLD(2,Ta2)
      CPa3=SHTFLD(1,Ta3)
      CPv3=SHTFLD(2,Ta3)

      Ca1=Rma1*Cpa1+Rmv1*Cpv1
      Ca2=Rma2*Cpa2+Rmv2*Cpv2
      Ca3=Rma3*Cpa3+Rmv3*Cpv3

C Safe guard the situation with no inflow of air
      call eclose(Ca1,0.00,0.0001,closeb)
      call eclose(Ca2,0.00,0.0001,closec)
      IF(closeb.AND.closec) Ca3=1.

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10      CALL PP61(IPCOMP,ISTATS,Ca1,Ca2,Ca3)

         COUT(1)=A(61,1)
         COUT(2)=A(61,2)
         COUT(3)=A(61,3)
         COUT(4)=B(61,4)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=21) air flow diverger',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rma1   = ',Rma1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' Rma2    = ',Rma2,' (Kg/s)'
            WRITE(ITU,*) ' Rmv2    = ',Rmv2,' (Kg/s)'
            WRITE(ITU,*) ' Rma3    = ',Rma3,' (Kg/s)'
            WRITE(ITU,*) ' Rmv3    = ',Rmv3,' (Kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Ta1    = ',Ta1,' (C)'
            WRITE(ITU,*) ' Ta2    = ',Ta2,' (C)'
            WRITE(ITU,*) ' Ta3    = ',Ta3,' (C)' 
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=4
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP68C'

      RETURN
      END

C **************************** CMP69C ****************************

C CMP69C generates for plant component IPCOMP with plant db code 690
C i.e. 3 nodes (ISV=20) water/steam flow diverger  with control
C built from primitive parts;
C this is a simple adaption of component CMP65C with controlled flow ratio 
C   node 1 represents fluid at first outlet
C   node 2 represents fluid at second outlet
C   node 3 represents fluid at inlet
C matrix equation coefficients COUT (in order: self-coupling, cross-
C coupling, & present-time coefficients) for energy balance (ISTATS=1),
C 1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ADATA: none
C     BDATA: none
C     CDATA: 1  flow ratio (first outlet to inlet)

      SUBROUTINE CMP69C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha

      PARAMETER (SMALL=1.0E-15)

      REAL      COUT(MPCOE)
      logical closea,close

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP69C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)

C Define location of sub-matrix template
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Following procedures apply to all istats values (1, 2 or 3)

C set all reverse flow to zero if any
      if(CSVF(INOD1,2).LT.0.) CSVF(INOD1,2)=0.
      if(CSVF(INOD1,3).LT.0.) CSVF(INOD1,3)=0.
      if(CSVF(INOD2,2).LT.0.) CSVF(INOD2,2)=0.
      if(CSVF(INOD2,3).LT.0.) CSVF(INOD2,3)=0.
      if(CSVF(INOD3,2).LT.0.) CSVF(INOD3,2)=0.
      if(CSVF(INOD3,3).LT.0.) CSVF(INOD3,3)=0.

C First mark 1st phase (liquid water) mass flow rates for iteration;
C ICSV is the array holding iteration flag for each plant node & each state
C variable
      ICSV(INOD1,2)=1
      CSVI(INOD1,2)=CSVF(INOD1,2)
      ICSV(INOD2,2)=1
      CSVI(INOD2,2)=CSVF(INOD2,2)
      ICSV(INOD3,2)=1
      CSVI(INOD3,2)=CSVF(INOD3,2)

C Then mark 2nd phase (steam vapour) mass flow rates for iteration
      ICSV(INOD1,3)=1
      CSVI(INOD1,3)=CSVF(INOD1,3)
      ICSV(INOD2,3)=1
      CSVI(INOD2,3)=CSVF(INOD2,3)
      ICSV(INOD3,3)=1
      CSVI(INOD3,3)=CSVF(INOD3,3)

C Start with establishing incoming fluid temperatures Tr0
      Tr0=CONVAR(ICON1,1)

C Mark current fluid temperatures for iteration
      ICSV(INOD1,1)=1
      CSVI(INOD1,1)=CSVF(INOD1,1)
      ICSV(INOD2,1)=1
      CSVI(INOD2,1)=CSVF(INOD2,1)
      ICSV(INOD3,1)=1
      CSVI(INOD3,1)=CSVF(INOD3,1)

      Tr1=CSVI(INOD1,1)
      Tr2=CSVI(INOD2,1)
      Tr3=CSVI(INOD3,1)

C Determine values of valuables used in this component e.g. fluid mass 
C flow rates, specific heats, and latent heat (note: mass of
C encapsulated water & steam vapour to be neglected)

C Firstly identify the specified CDATA 
      R=CDATA(IPCOMP,1)

C reverse flow of incoming fluid has been set zero by MZPADJ if any
      Rmw0=PCONDR(ICON1)*CONVAR(ICON1,2)
      Rmv0=PCONDR(ICON1)*CONVAR(ICON1,3)
      Rmr0=Rmw0+Rmv0
      Rmw1=CSVI(INOD1,2)
      Rmv1=CSVI(INOD1,3)

C Flow rates at nodes 2 & 3 for reporting purpose
      Rmw2=CSVI(INOD2,2)
      Rmv2=CSVI(INOD2,3)
      Rmw3=CSVI(INOD3,2)
      Rmv3=CSVI(INOD3,3)

      call eclose(R,-99.00,0.001,closea)
      IF(.not.closea) GO TO 10

C Get value of R
      call eclose(Rmr0,0.00,0.00001,close)
      IF(.NOT.close) THEN
C      IF(Rmr0.NE.0.) THEN
        R=(Rmw1+Rmv1)/Rmr0
      ELSE
        R=0.5
      ENDIF

C Current component time-constant TC set to zero because of 
C no thermal capacitance term in this component
      TC(IPCOMP)=0.

C Establish matrix equation coefficients

 10   CALL PP51(IPCOMP,ISTATS,R)

      COUT(1)=A(51,1)
      COUT(2)=A(51,4)
      COUT(3)=A(51,2)
      COUT(4)=A(51,3)
      COUT(5)=A(51,5)
      COUT(6)=1.
      COUT(7)=-1.
      COUT(8)=B(51,1)
      COUT(9)=B(51,2)
      COUT(10)=0.

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 nodes (ISV=20) water/steam flow diverger',
     &               ' built by Primitive Parts'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Rmw0    = ',Rmw0,' (Kg/s)'
            WRITE(ITU,*) ' Rmv0    = ',Rmv0,' (Kg/s)'
            WRITE(ITU,*) ' Rmw1   = ',Rmw1,' (kg/s)'
            WRITE(ITU,*) ' Rmv1   = ',Rmv1,' (kg/s)'
            WRITE(ITU,*) ' Rmw2    = ',Rmw2,' (Kg/s)'
            WRITE(ITU,*) ' Rmv2    = ',Rmv2,' (Kg/s)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' Tr0    = ',Tr0,' (C)'
            WRITE(ITU,*) ' Tr1    = ',Tr1,' (C)'
            WRITE(ITU,*) ' Tr2    = ',Tr2,' (C)' 
            WRITE(ITU,*) ' R   = ',R
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=10
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP69C'

      RETURN
      END
