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

C  cmp134c 1 node (ISV=20) Load (Heating or Cooling)
C  cmp135c 3 node (ISV=20) Shell Boiler / Water Heater
C  cmp136c 4 node (ISV=20) Buried DH Water Pipe w/ adjacent pipe
C  cmp137c 1 node (ISV=20) Massless Converging Junction
C  cmp138c 3 node (ISV=20) Water 3-port diverter with flow balancing
C  cmp139c 3 node (ISV=20) Flow Branch Water Diverter Junction - Relative Flow 
C  cmp140c 3 node (ISV=20) Water 3-port Diverter Valve - Relative Flow
C  cmp141c 2 node (ISV=20) Massless Diverter Valve

C ******************** CMP134C ********************
C CMP134C generates for plant component IPCOMP with plant db code 1340
C 1 node (ISV=20) Load (Heating or Cooling)
C Required input data (in plant configuration file)
C     ADATA: 1 Minimum Outlet Temperature (C)
C     BDATA: none
C Control variables (set by control loop or fixed value in plant config file) 
C     CDATA: 1 Duty (W) (+ve = cooling load, -ve = heating load)

      SUBROUTINE CMP134C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/Pctime/TIMSEC
      COMMON/PTIME/PTIMEP,PTIMEF

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

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/HN/NODE(MPCOM),RESID(MPCOM)

      PARAMETER (SMALL=1.0E-15)
      REAL      COUT(MPCOE),TMIN,NODE
      integer   RESID
      character outs*124
      logical closea

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP134C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)
      QRESP=PCDATP(IPCOMP,1) ! Residual unmet heat (Watts per model timestep)

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C Then initialize timestep duty Q and required duty, QR, including residual unmet demand 
         IF(CONVAR(ICON1,2).GT.SMALL) THEN
           Q=CDATA(IPCOMP,1)
           QR=Q+QRESP
         ELSE
           Q=0.0
           QR=QRESP
         ENDIF

C Establish inlet and outlet fluid heat capacity rate C1 and C2 (W/K), 
C ie. mass flow * specific heat
         C1=PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,CONVAR(ICON1,1))+
     &      PCONDR(ICON1)*CONVAR(ICON1,3)*SHTFLD(2,CONVAR(ICON1,1))
         Rmw1=CSVF(INOD1,2)
         Tmw1=CSVF(INOD1,1)
         C2=Rmw1*SHTFLD(3,Tmw1)

C Mark temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)

C Establish matrix equation self- and cross-coupling 
C coefficients (Fully Implicit assumed)
         If(C1.gt.0.and.C2.gt.0)then
           COUT(1)=C2
           COUT(2)=-C1

C Limit outlet temperature to minimum to mimic heat exchanger performance
           TOUT=(QR+C1*CONVAR(ICON1,1))/C2
           Q1=QR
           TMIN=ADATA(IPCOMP,1)
           if(TOUT.lt.TMIN.and.CONVAR(ICON1,1).gt.TMIN) then
             Q1=(C2*TMIN)-(C1*CONVAR(ICON1,1))
           elseif(CONVAR(ICON1,1).lt.TMIN) then
             Q1=0. ! No heat transfer if inlet below outlet lower limit
           endif
         else
           COUT(1)=1.
           COUT(2)=-1.
           Q1=0.
         end if
C and then present-time coefficient (ie. right hand side)
         COUT(3)=Q1
         QRES=QRESP+Q-Q1

C Store "environment" variables future values
         PCDATF(IPCOMP,1)=QRES

C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=0.

C 2nd phase mass (ie. vapour) balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=0.
      END IF

C Store additional output variables for the results file.
      napdat(ipcomp)=3
      pcaout(ipcomp,1)=Q
      pcaout(ipcomp,2)=Q1
      pcaout(ipcomp,3)=QRES
      if(QRES.lt.0.and.RESID(IPCOMP).eq.0) then
        WRITE(outs,*) ' Residual heat demand for component',
     &                  IPCOMP, 'check results'
        call edisp(itu,outs)
        RESID(IPCOMP)=1
      end if

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) Load (Heating or Cooling)'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
            WRITE(ITU,*) ' C2     = ',C2,' (W/K)'
            WRITE(ITU,*) ' Q  = ',Q,' (W)'
            WRITE(ITU,*) ' Q1  = ',Q1,' (W)'
            WRITE(ITU,*) ' QR  = ',QR,' (W.ts)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP134C'

      RETURN
      END

C ******************** CMP135C ********************
C CMP135C generates for plant component IPCOMP with plant db code 1350 ie.
C 3 node (ISV=20) Shell Boiler / Water Heater
C Node 1 = Shell, Node 2 = Stored Water, Node 3 = Water Leaving
C Modulating control with user-defined turndown limit and lockout time
C Required input data (in plant configuration file)
C Shell = outer casing + insulation + inner tank walls
C     ADATA: 1 Boiler rating (kW) 
C        2 Shell total mass (kg) (not water)
C        3 Casing outer diameter (m)
C        4 Casing thickness (m)
C        5 Insulation thickness (m)
C        6 Tank wall thickness (m)
C        7 Turndown limit (frac)
C        8 Full load efficiency at Tc (frac)
C        9 Gradient of efficiency curve for Tj < Tc (1/K) (Ashrae)
C       10 Gradient of efficiency curve for Tj > Tc (1/K)
C       11 Upper boiler temperature limit (C)
C       12 Casing thermal conductivity 
C       13 Insulation thermal conductivity
C       14 Tank wall thermal conductivity
C       15 Shell average heat capacity
C       16 Water Volume (m3)
C       17 Lockout Time (min)     
C
C Control variables (set by control loop or fixed value in plant config file) 
C     CDATA: 1 Control signal (frac)

C     PCDATF/P
C            1 Control signal (frac)
C            2 Boiler efficiency (frac)
C            3 Heat input (kW)
C            4 Spare
C            5 Spare

      SUBROUTINE CMP135C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
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
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/LOKOUT1/BINSO(MPCOM),BITSBO(MPCOM)
      INTEGER napdat,NTSBO,BINSO,BITSBO

      PARAMETER (SMALL=1.0E-20)
      REAL      COUT(MPCOE),BLKOUT,CSGNL,QDATA,PCAOUT,KC
      REAL      ID  ! internal diameter
      character outs*124

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP135C'

C Check control data for relevant balance type
      IF(ISTATS.EQ.1.AND.
     &   (CDATA(IPCOMP,1).LT.0..OR.CDATA(IPCOMP,1).GT.1.)) THEN
         CALL DAYCLK(IDYP,PTIMEF,IUOUT)
         WRITE(outs,*) ' CMP135C: invalid control input for component ',
     &                  IPCOMP,' : ',CDATA(IPCOMP,1)
         call edisp(iuout,outs)
         call edisp(iuout,' CMP135C: unresolvable error.')
         close(ieout)
         CALL ERPFREE(ieout,ISTAT)
         call epwait
         call epagend
         STOP
      END IF

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,2,1)
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN

C First calculate boiler efficiency ETA based on current node 1 temp.
C so mark node 1 temperature for iteration
         ICSV(INOD1,1)=1
         CSVI(INOD1,1)=CSVF(INOD1,1)
         ICSV(INOD2,1)=1
         CSVI(INOD2,1)=CSVF(INOD2,1)
         ICSV(INOD3,1)=1
         CSVI(INOD3,1)=CSVF(INOD3,1)
         IF(CSVF(INOD2,1).LT.50.) THEN
            TGALPH=ADATA(IPCOMP,9)
         ELSE
            TGALPH=ADATA(IPCOMP,10)
         END IF
         ETA=ADATA(IPCOMP,8)-TGALPH*(50.-CSVF(INOD2,1))

C Establish boiler setpoint (fraction of full load)
         CSGNL=CDATA(IPCOMP,1)
         if(CSGNL.LE.ADATA(IPCOMP,7)) THEN ! Turndown limit and lockout

C Lock out time (in seconds), If boiler control signal falls below the
C turndown limit it is shut down for this amount of time unless temperature
C reaches the lower control limit.
C BINSO = boiler on/off (1/0) flag for lockout
C BITSBO(IPCOMP) = present no. of timesteps boiler is off due to lockout
C NTSBO = total no. of timesteps boiler is off due to lockout
           BLKOUT=ADATA(IPCOMP,17)*60.
           NTSBO=NINT(BLKOUT/TIMSEC)
           BINSO(IPCOMP)=1
           BITSBO(IPCOMP)=0
         endif

         IF(BINSO(IPCOMP).EQ.1)THEN
           BITSBO(IPCOMP)=BITSBO(IPCOMP)+1
C    Reset at end of lockout period
           IF(BITSBO(IPCOMP).GE.NTSBO) BINSO(IPCOMP)=0 
C    Reset if boiler temperature close to low limit
           IF(CSGNL.GE.0.99) THEN 
             BINSO(IPCOMP)=0
           ELSE
             CSGNL=0
           ENDIF
         ENDIF

C Reset input power in case maximum temperature exceeded
         IF(CSVF(INOD3,1).GE.ADATA(IPCOMP,11))then
            CSGNL=CSGNL*((ADATA(IPCOMP,11)-CONVAR(ICON1,1))
     &          /(CSVF(INOD3,1)-CONVAR(ICON1,1)))
         END IF
C Then calculate heat input into the water Q
         IF(CSGNL.GT.0) THEN !.
            Q=ADATA(IPCOMP,1)*CSGNL*1.0E+3
         ELSE
            Q=ADATA(IPCOMP,1)*CSGNL*1.0E+3
         END IF

C Establish heat capacity of component mass CM (J/K) and
C stroed water mass, CW
         CM=ADATA(IPCOMP,2)*ADATA(IPCOMP,15)
         CW=ADATA(IPCOMP,16)*DENSIT(2,CONVAR(ICON1,1))
     &      *SHTFLD(3,CONVAR(ICON1,1))

C Save boiler control signal
         PCDATF(IPCOMP,1)=CSGNL

C Save inter-node fluid heat capacity rate (W/K)
         PCDATF(IPCOMP,2)=CSVF(INOD2,2)*SHTFLD(3,CSVF(INOD2,1))

C Calculate current component time-constant TC
         TC(IPCOMP)=AMAX1(CM/AMAX1(SMALL,(CW)),
     &        CM/AMAX1(SMALL,(PCDATF(IPCOMP,2)
     &        /(CSVF(INOD2,1)-PCNTMF(IPCOMP)))))


C Identify required variables
         Rmf0=PCONDR(ICON1)*CONVAR(ICON1,2) ! Inlet flow
         Tf0P=PCTP(ICON1) ! Inlet fluid temperature (previous timestep)
         Tf1P=CSVP(INOD2,1) ! Fluid volume temperature (previous ts)
         Tf2P=CSVP(INOD3,1) ! Fluid leaving temperature (previous ts)
         Tf0=CONVAR(ICON1,1) ! Inlet fluid temperature (current ts)
         Tf1=CSVI(INOD2,1) ! Fluid volume temperature (current ts)
         Tf2=CSVI(INOD3,1) ! Fluid leaving temperature (current ts)
         TeP=PCDATP(IPCOMP,4) ! Environment temperature (previous ts)
         Te=PCNTMF(IPCOMP) ! Environment temperature (current ts)
         TsP=CSVP(INOD1,1) ! Boiler shell temperature (previous ts)
         Ts=CSVI(INOD1,1) ! Boiler shell temperature (current ts)
         Rmf1=CSVF(INOD2,2) ! Fluid leaving water volume flow (current ts)
         Rmf2=CSVF(INOD3,2) ! Fluid leaving boiler flow (current ts)
         Cpf0=SHTFLD(3,Tf0) ! Inlet fluid heat capacity (current ts)
         Cpf1=SHTFLD(3,Tf1) ! Fluid volume heat capacity (current ts)
         Cpf2=SHTFLD(3,Tf2) ! Fluid leaving heat capacity (current ts)
         QP=PCQP(INOD2) ! Heat input (previous ts)
         Ue=0.01 ! External air flow
C Flow*heat capacities for fluid nodes
         Cf0P=PCDATP(IPCOMP,1)
         Cf1P=PCDATP(IPCOMP,2)
         Cf2P=PCDATP(IPCOMP,3)
         Cf0=Rmf0*Cpf0
         Cf1=Rmf1*Cpf1
         Cf2=Rmf2*Cpf2

C Outside convective heat transfer coefficient
         Na=1                 ! set as water
         ID=ADATA(IPCOMP,3)   ! and internal diameter
         Call CVCOEFF2(Na,Te,Ts,Ue,ID,ho)
C Inside conduction heat transfer coefficient (no flow assumed)
C Equivalent cylinder with ends ignored assumed
C Cylinder length equivalent to cylinder area without ends
         RL=(4.*ADATA(IPCOMP,16))/(3.14159*ID**2)+(ID/2)
         KC=(2.*3.14159*0.598*RL)/(1./(sqrt(0.5)))

         D2=ID-2.*ADATA(IPCOMP,4)
         D1=D2-2.*ADATA(IPCOMP,5)
         D0=D1-2.*ADATA(IPCOMP,6)
         CTC=ADATA(IPCOMP,12)
         TCI=ADATA(IPCOMP,13)
         TTC=ADATA(IPCOMP,14)

         Rp=(log(D1/D0)/(2.*3.14159*TTC*RL))+(log(D2/D1)/
     &     (2.*3.14159*TCI*RL))+(log(ID/D2)/(2.*3.14159*CTC*RL))  
         Ai=(3.14159*D0*RL)+(2.*3.14159*(D0/2.)**2.)
         Ao=(3.14159*ID*RL)+(2.*3.14159*(ID/2.)**2.)
C Modify heat transfer coefficients by adding shell thermal resistances (50% to each)
         KC=(1./((1./KC)+(Rp/2.)))
         HC=(1./((1./(ho*Ao))+(Rp/2.)))

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
            CM=0.
            ALPHA=1.
         END IF

C Establish matrix equation self-coupling coefficients,
         COUT(1)=(CM/TIMSEC)+(ALPHA*HC)+(ALPHA*KC)
         COUT(2)=-1.*ALPHA*KC
         COUT(3)=-1.*ALPHA*KC
         COUT(4)=(CW/TIMSEC)+(ALPHA*KC)
         COUT(5)=ALPHA*Cf2
         COUT(6)=-1.
         COUT(7)=1.
         COUT(8)=-1*ALPHA*Cf0

         COUT(9)=(CM*TsP/TIMSEC)+(ALPHA*HC*Te)+(1.-ALPHA)
     &     *(HC*(TeP-TsP))+(1.-ALPHA)*(KC*(Tf1P-TsP))
         COUT(10)=(CW*Tf1P/TIMSEC)+(ALPHA*Q)+
     &     (1.-ALPHA)*KC*(TsP-Tf1P)+(1.-ALPHA)*Cf0P*Tf0P-
     &     (1.-ALPHA)*Cf2P*Tf2P+(1.-ALPHA)*QP
         COUT(11)=0.

C Store "environment" variables future values
         PCTF(ICON1)=CONVAR(ICON1,1) ! Inlet fluid temperature
         PCQF(INOD2)=Q
         PCDATF(IPCOMP,1)=Cf0
         PCDATF(IPCOMP,2)=Cf1
         PCDATF(IPCOMP,3)=Cf2
         PCDATF(IPCOMP,4)=Te

C Establish "containment loss" data
         QDATA(IPCOMP)=0.

C Establish additional output variables
         napdat(ipcomp)=5
         PCAOUT(IPCOMP,1)=CSGNL
         PCAOUT(IPCOMP,2)=ETA
         PCAOUT(IPCOMP,3)=Q/1000.
         PCAOUT(IPCOMP,4)=0 ! Optional user-defined output
         PCAOUT(IPCOMP,5)=0 ! Optional user-defined output

C 1st phase mass (ie. water) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=1.
         COUT(5)=0.
         COUT(6)=-1.
         COUT(7)=1.
         COUT(8)=-1.
         COUT(9)=0.
         COUT(10)=0.
         COUT(11)=0.

C 2nd phase mass (ie. "vapour") balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=0.
         COUT(3)=0.
         COUT(4)=1.
         COUT(5)=0.
         COUT(6)=-1.
         COUT(7)=1.
         COUT(8)=-1.
         COUT(9)=0.
         COUT(10)=0.
         COUT(11)=0.
      END IF

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV=20) Shell Boiler / Water Heater'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
            WRITE(ITU,*) ' CW     = ',CW,' (W/K)'
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
            WRITE(ITU,*) ' CDATA  = ',CDATA(IPCOMP,1)
            WRITE(ITU,*) ' CSGNL = ',CSGNL,' (-)'
            WRITE(ITU,*) ' ETA    = ',ETA,' (-)'
            WRITE(ITU,*) ' Q   = ',Q,' (W)'
            WRITE(ITU,*) ' Tf0   = ',Tf0,' (C)'
            WRITE(ITU,*) ' Tf1   = ',Tf1,' (C)'
            WRITE(ITU,*) ' Tf2   = ',Tf2,' (C)'
            WRITE(ITU,*) ' Rmf0   = ',Rmf0,' (kg/s)'
            WRITE(ITU,*) ' Cf2   = ',Cf2,' (-)'
            WRITE(ITU,*) ' KC   = ',KC,' (-)'
            WRITE(ITU,*) ' HC   = ',HC,' (-)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=11
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP135C'

      RETURN
      END

C ******************** CMP136C ********************
C CMP136C generates for plant component IPCOMP with plant db code 1360 ie.
C 4 node (ISV=20) Buried DH Water Pipe w/ adjacent pipe
C Node 1 = pipe outer layer, Node 2 = pipe inner layer
C Node 3 = water in contact with pipe, Node 4 = leaving water 
C Required input data (in plant configuration file)
C     ADATA: 1 Pipe Length (m)
C            2 Pipe Depth (m)
C            3 Outside Diameter (m)
C            4 Pipe Separation (m)
C            5 Fluid Pipe Outside Diameter (m)
C            6 Fluid Pipe Thickness (m)
C            7 Fluid Pipe Thermal Conductivity (W/mK)
C            8 Insulation Thermal Conductivity (W/mK)
C            9 Ground Thermal Conductivity (W/mK)
C           10 Insulation Unit Mass (kg/m)
C           11 Pipe Unit Mass (kg/m)
C           12 Insulation Heat Capacity (J/kgK)
C           13 Pipe Heat Capacity (J/kgK)
C           14 Adjacent Pipe Component Number
C     BDATA: none
C     CDATA: none

      SUBROUTINE CMP136C(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
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
      logical closebp
      integer ADJP

      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PP0C/A(MPP,MPPC),B(MPP,MPPC),Alpha
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP136C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C Define connection number
      ICON1=ICONDX(IPCOMP,3,1)
C Identify component node location in overall solution matrix
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
      INOD4=NPCDAT(IPCOMP,9)+3
      ADJP=int(ADATA(IPCOMP,14)) ! Adjacent Pipe Comp ID
      INOD5=NPCDAT(ADJP,9) ! Outside node of adjacent pipe

C Set the node connectivity values
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
        Rmf3=CSVI(INOD3,2)
        Rmf4=CSVI(INOD4,2)
C Mark current solid and water temperatures for iteration 
        ICSV(INOD1,1)=1
        CSVI(INOD1,1)=CSVF(INOD1,1)
        ICSV(INOD2,1)=1
        CSVI(INOD2,1)=CSVF(INOD2,1)
        ICSV(INOD3,1)=1
        CSVI(INOD3,1)=CSVF(INOD3,1)
        ICSV(INOD4,1)=1
        CSVI(INOD4,1)=CSVF(INOD4,1)
        CSVI(INOD5,1)=CSVF(INOD5,1)
        Tp1=CSVI(INOD1,1)
        Tp2=CSVI(INOD2,1)
        Tf3=CSVI(INOD3,1)
        Tf4=CSVI(INOD4,1)
        Taj=CSVI(INOD5,1)
C Establish incoming water temperature & flow rates
        Tf0=CONVAR(ICON1,1)
        Rmf0=PCONDR(ICON1)*CONVAR(ICON1,2)

C Identify temperatures & flow rates of previous time step
        Tf0P=PCTP(ICON1)
        Tp1P=CSVP(INOD1,1)
        Tp2P=CSVP(INOD2,1)
        Tf3P=CSVP(INOD3,1)
        Tf4P=CSVP(INOD4,1)
        TajP=CSVP(INOD5,1)

C Restore PCDATP values
        CsfP=PCDATP(IPCOMP,1)
        Cf0P=PCDATP(IPCOMP,2)
        Cf4P=PCDATP(IPCOMP,3)
        Cpf3P=PCDATP(IPCOMP,4)

        TeP=PCNTMP(IPCOMP)
C Identify the specified ADATA and CDATA
        RL=ADATA(IPCOMP,1) ! Pipe Length
        PD=ADATA(IPCOMP,2) ! Pipe Depth
        OD=ADATA(IPCOMP,3) ! Pipe Outside Dia.
        PS=ADATA(IPCOMP,4) ! Pipe Separation
        CD=ADATA(IPCOMP,5) ! Carrier Pipe Dia.
        CT=ADATA(IPCOMP,6) ! Carrier Pipe Thickness
        CTC=ADATA(IPCOMP,7) ! Carrier Pipe Thermal Cond.
        OTC=real(ADATA(IPCOMP,8)) ! Insulation Thermal Cond.
        GTC=ADATA(IPCOMP,9) ! Ground Thermal Cond.
        RMp1=ADATA(IPCOMP,10)*RL ! Ins. Mass per unit length*RL
        RMp2=ADATA(IPCOMP,11)*RL ! Pipe Mass per unit length*RL
        Cs1=ADATA(IPCOMP,12) ! Ins. Heat Capacity
        Cs2=ADATA(IPCOMP,13) ! Pipe Heat Capacity

        Di=CD-2*CT ! Carrier Pipe ID 
        RTp2=log(CD/Di)/(2*3.14159*CTC) ! Pipe
        Ao=OD*3.14159*RL
        Ai=CD*3.14159*RL

        Te=PCNTMF(IPCOMP)
        call eclose(pcntmf(ipcomp),-99.00,0.001,closea)
        if(closea) Te=Tp1
C Determine specific heat values
        Cpf0=SHTFLD(3,Tf0)
        Cpf3=SHTFLD(3,Tf3)
        Cpf4=SHTFLD(3,Tf4)

C Establish outstanding arguements used in primitive part subroutines

C first flow velocity
        RMf=3.14159*(Di**2)/4.*RL*RHOFLD(3,Tf3)
        Uf=Rmf0/(RHOFLD(3,Tf0)*3.14159*(Di**2)/4.)

C then inside convective heat transfer coefficient
        call eclose(Uf,0.00,0.001,closebp)        
        if (.not.closebp) then ! Convection (flowing) case
          Nw=3
          Call CVCOEFF1(Nw,Tf3,Uf,Di,hi)
          hi=1./(1./hi+RTp2/2.)
          Csf=hi*Ai
        else ! Conduction (no-flow) case
          Csf=(2.*3.14159*0.598*RL)/(1./(sqrt(0.5)))
C       Modify hA(Csw) by adding pipe thermal resistance
          Csf=1./((1./Csf)+(RTp2/(2.*Ai)))
        endif

C Determine heat transfer coefficients for ground and adjacent pipe 
C with heat transfer coefficients (from Bohm(2000)) 
        Rg=(1./(4.*3.14159*GTC))*log((4.*PD)/Di) ! Ground
        RTp1=log(OD/CD)/(2*3.14159*OTC) ! Insulation
        Rm=(1./(4.*3.14159*GTC))*log(1.+((2.*PD)/(PS+OD))**2.)
        U1=RL*((Rg+(RTp1/2.))/((Rg+(RTp1/2.))**2.-Rm**2.))
        U2=RL*(Rm/((Rg+(RTp1/2.))**2.-Rm**2.))

C Modify heat transfer coefficients by re-arranging input data
        C12=(2*RL)/(RTp1+RTp2)
        Cf0=Rmf0*Cpf0
        Cf3=Rmf3*Cpf3
        Cf4=Rmf4*Cpf4
C Check if inside waterr transport delay is required for future time step
        DY=(Uf*TIMSEC)/RL
C        IF (DY.lt.1.0) THEN
C          IF(DY.LT.0.0001) then
C            DELAY=1.
C            DY=0.0001
C            N=int(1./DY)
C          ELSE
C            DELAY=1.
C            N=int(1./DY)
C          ENDIF
C        ELSE
C          DELAY=0.
C        ENDIF
        DELAY=0.
        N=0
        Call TRNSDLAY(IPCOMP,DELAY,Tf0P,Tf3P,Tp2P,N,DY,Cf3,Csf,
     &                Tf0,Tf3,Tf4)
C Calculate current component time-constant TC based on response
C of solid node p2 to change of incoming water temperature;
C for solid node p2
        TCp2=(RMp2*Cs2)/(hi*Ai)
C for water node f3
        TCf3=RMf*Cpf3/(Rmf3*Cpf3)
 10     CALL TIMCONST(TCp2,TCf3,TC(IPCOMP))

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
          RMp1=0.
          RMp2=0.
          ALPHA=1.
        END IF
      END IF

C Determine matrix coefficients
      CALL PP11(IPCOMP,ISTATS,Conn(1),Conn(2),
     &          C12,Cs1,Cs2,Tp1P,Tp2P,RMp1,RMp2)

      CALL PP43(IPCOMP,ISTATS,Conn(2),Conn(3),DELAY,Tf3,Tf4,
     &          Csf,CsfP,Cf0,Cf0P,Cf4,Cf4P,Cs2,Cpf3,Cpf3P,Cpf4,
     &          RMp2,RMf,Tf0P,Tf3P,Tf4P,Tp2P)

      
      if(ISTATS.GT.1) then
        COUT(1)=2.*A(11,1)
        COUT(12)=0.
      else
        COUT(1)=-((RMp1*Cs1)/Timsec)-(ALPHA*(U1+C12))
        COUT(12)=-(((RMp1*Cs1)/Timsec)*Tp1P)-(ALPHA*(U1-U2)*Te)
     &  -(ALPHA*U2*TajP)-(1-ALPHA)*(U1-U2)*(Te-Tp1P)
     &  -(1-ALPHA)*U2*(TajP-Tp1P)-(1-ALPHA)*C12*(Tp2P-Tp1P)
      endif
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
      COUT(13)=B(11,2)+B(43,1)
      COUT(14)=B(43,2)
      COUT(15)=B(43,3)

C Store component variables future values
      IF(ISTATS.EQ.1) THEN
        PCTF(ICON1)=CONVAR(ICON1,1)
        PCDATF(IPCOMP,1)=Csf
        PCDATF(IPCOMP,2)=Cf0
        PCDATF(IPCOMP,3)=Cf4
        PCDATF(IPCOMP,4)=Cpf3
        PCDATF(IPCOMP,5)=Te
      ENDIF

CC Establish additional output variables
      U1T=RL*((Rg+RTp1+RTp2)/(((Rg+RTp1+RTp2)**2)-Rm**2))
      U2T=RL*(Rm/(((Rg+RTp1+RTp2)**2)-Rm**2))
      HTLS=((U1T-U2T)*(Te-Tf3))+(U2*(TadjP-Tf3))
      napdat(ipcomp)=2
      PCAOUT(IPCOMP,1)=HTLS
      PCAOUT(IPCOMP,2)=U2T*(TadjP-Tf3)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 4 node (ISV=20) Buried DH Water 
     &     Pipe w/ adjacent pipe'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3,INOD4
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
            WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
            WRITE(ITU,*) ' delay  = ',delay,' (-)'
            WRITE(ITU,*) ' N      = ',N,' (-)'
            WRITE(ITU,*) ' Tf0    = ',Tf0,' (C)'
            WRITE(ITU,*) ' Tp1    = ',Tp1,' (C)'
            WRITE(ITU,*) ' Tp2    = ',Tp2,' (C)'
            WRITE(ITU,*) ' Tf3    = ',Tf3,' (C)'
            WRITE(ITU,*) ' Tf4    = ',Tf4,' (C)'
            WRITE(ITU,*) ' Cf0    = ',Cf0,' (W/K)'
            WRITE(ITU,*) ' Cf3    = ',Cf3,' (W/K)'
            WRITE(ITU,*) ' Cf4    = ',Cf4,' (W/k)'
            WRITE(ITU,*) ' Csf    = ',Csf,' (W/k)'
            WRITE(ITU,*) ' Uf     = ',Uf,' (m/s)'
            WRITE(ITU,*) ' hi     = ',hi,' (W/K.m^2)'
            WRITE(ITU,*) ' U1     = ',U1,' (W/K)'
            WRITE(ITU,*) ' U2     = ',U2,' (W/K)'
            WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
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
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Leaving subroutine CMP136C'

      RETURN
      END

C ******************** CMP137C ********************
C CMP137C generates for plant component IPCOMP with plant db code 1370 ie.
C 1 node (ISV=20) Massless Converging Junction.
C No input data required. Fully explicit model (ALPHA=1).

      SUBROUTINE CMP137C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)

      REAL SMALL,CSVF,CSVP
     
      INTEGER IPCOMP

      integer itc,ITCF,ITRACE,ITU,NSINC,ISTATS,ICNT,ICONTP,idynow
      integer ICONDX,ICON1,ICON2,INOD1,NPCDAT,NAPDAT,NITMS,I
      integer IDWF,IDWP,IDYF,IDYP,IHRF,IHRP,IPC1,IPC2,IPCT,IPN1,IPN2
      integer IPOFS1,IPOFS2,ITS,IX1,IZNTRC,NPCON,NPCOMP,NCI

      real CDATA 
      real F1,F2,FT,CONVAR,PCONDR
      real PCAOUT,PCONSD,QDATA 

      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP137C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD
C 2 connections
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,1,2)

C 1 node - outlet
      INOD1=NPCDAT(IPCOMP,9)

C Inlet Mass Flows
      F1=PCONDR(ICON1)*CONVAR(ICON1,2)
      F2=PCONDR(ICON2)*CONVAR(ICON2,2)
      FT=F1+F2

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN

C Massless model therefore no significant heat transfer assumed
        if(FT.GT.0)then
          COUT(1)=1.
          COUT(2)=-F1/FT
          COUT(3)=-F2/FT
          COUT(4)=0.
        else
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=0.
          COUT(4)=CSVP(INOD1,1)
        endif
         
C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        COUT(2)=-PCONDR(ICON1)
        COUT(3)=-PCONDR(ICON2)
        COUT(4)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
        COUT(1)=1.
        COUT(2)=-PCONDR(ICON1)
        COUT(3)=-PCONDR(ICON2)
        COUT(4)=0.
      END IF

C Establish additional output variables
      NAPDAT(IPCOMP)=0

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 1 node (ISV=20) Massless Converging Junction'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=4
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP137C trace'
      ENDIF

      RETURN
      END

C ******************** CMP138C ********************
C CMP143C generates for plant component IPCOMP with plant db code 1380 ie.
C 3 node (ISV>20) DH 3-port diverter/converger for sub-circuit flow balancing.
C Required input data (in plant configuration file)
C Node 1 = Main Inlet, Node 2 = Branch Inlet/Outlet, Node 3 = Main Outlet
C     ADATA: 1 Flow reference node 1 (node 1 driver)
C            2 Flow reference node 2 (node 2 driver)
C
C Component diverts the excess flow required to balance flows between two connected 
C circuits with different flow driving components and control systems. Typically 
C used for district heating networks to balance the flows between the generating
C and distribution circuits via a bypass pipe or 2-port stratified tank. The component
C is massless and has no heat loss calculated.

      SUBROUTINE CMP138C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow

      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &    ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)

      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &    IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      REAL V_POS,SMALL,FLW1,FLW2,F1,F2,FT
     
      INTEGER IMPEXP,IPCOMP,FN1,FN2,FC1,FC2

      integer itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,
     &  IDYP,IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,ICONTP,ISTATS,idynow
      integer ICONDX,ICON1,ICON2,INOD1,INOD2,INOD3,NPCON,NPCDAT,
     &  NAPDAT,NITMS,I,IX1,IPOFS1,IPOFS2,IPC1,IPCT,IPC2,IPN2,IPN1

      real PCONDR,ADATA,BDATA,CONVAR,PCAOUT,QDATA,PCONSD
      real CSVF,CSVP

      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP138C'

C Connections
      ICON1=ICONDX(IPCOMP,1,1)
      ICON2=ICONDX(IPCOMP,2,1)

C Node matrix locations 
C 1) main water inlet 2) balancing circuit in/out 3) main water outlet
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Relative Flow Nodes
      FC1=ADATA(IPCOMP,1)
      FC2=ADATA(IPCOMP,2)
      FN1=NPCDAT(FC1,9)
      FN2=NPCDAT(FC2,9)
      FLW1=CSVF(FN1,2)
      FLW2=CSVF(FN2,2)

C Valve opens bypass to balance flows between two circuits if FLW1>FLW2
C If FLW2>FLW1 component acts as converging with flow delta into node 2.
      IF(FLW1.GE.FLW2)THEN
        IF(FLW1.GT.0) THEN
          V_POS=(FLW1-FLW2)/FLW1
        ELSE
          V_POS=0.
        ENDIF

        IF(V_POS.LT.0.OR.V_POS.GT.1.)THEN
          V_POS=0.
          WRITE(IUOUT,*)
     &  ' WARNING:CMP138C a 3-port valve control setting <0 or >1'
          WRITE(IUOUT,*)
     &  ' has been detected. Resetting to valve position to zero.'
        ENDIF

C Generate coefficients for the energy balance equation
        IF(ISTATS.EQ.1) THEN

C No thermal exchange
C Self-coupling
          COUT(1)=1.
          COUT(2)=1.
          COUT(3)=-1.
          COUT(4)=1.
          COUT(5)=0.
          COUT(6)=-1.
C Cross-coupling
          COUT(7)=-1.
          COUT(8)=0.
C Right-hand side
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.
         
C 1st phase mass (ie. dry air) balance coefficients
        ELSE IF(ISTATS.EQ.2) THEN
          COUT(1)=1.
          COUT(2)=V_POS
          COUT(3)=-1.
          COUT(4)=(1.-V_POS)
          COUT(5)=0.
          COUT(6)=-1.
          COUT(7)=-1.
          COUT(8)=0.
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
        ELSE IF(ISTATS.EQ.3) THEN
          COUT(1)=1.
          COUT(2)=V_POS
          COUT(3)=-1.
          COUT(4)=(1.-V_POS)
          COUT(5)=0.
          COUT(6)=-1.
          COUT(7)=-1.
          COUT(8)=0.
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.
        END IF

      ELSE
        F1=PCONDR(ICON1)*CONVAR(ICON1,2)
        F2=FLW2-F1
        FT=F1+F2
        V_POS=0.

C Generate coefficients for the energy balance equation
        IF(ISTATS.EQ.1) THEN

C Massless model therefore no significant heat transfer assumed
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=1.
          COUT(4)=F1/FT
          COUT(5)=F2/FT
          COUT(6)=-1.
          COUT(7)=-1.
          COUT(8)=-1.
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.

C 1st phase mass (ie. dry air) balance coefficients
        ELSE IF(ISTATS.EQ.2) THEN
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=1.
          COUT(4)=1.
          COUT(5)=1.
          COUT(6)=-1.
          COUT(7)=-1.
          COUT(8)=-1.
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
        ELSE IF(ISTATS.EQ.3) THEN
          COUT(1)=1.
          COUT(2)=0.
          COUT(3)=1.
          COUT(4)=1.
          COUT(5)=1.
          COUT(6)=-1.
          COUT(7)=-1.
          COUT(8)=-1.
          COUT(9)=0.
          COUT(10)=0.
          COUT(11)=0.

        ENDIF
      ENDIF

C Establish additional output variables
      NAPDAT(IPCOMP)=1.
      PCAOUT(IPCOMP,1)=V_POS ! Valve position

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV>21) DH 3-port diverter/converger 
     &     for sub-circuit flow balancing'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1,ICON2
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' V_POS  = ',V_POS,' (0-1)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=11
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP138C trace'
      ENDIF

      RETURN
      END

C ******************** CMP139C ********************
C CMP139C generates for plant component IPCOMP with plant db code 1390 ie.
C 3 node (ISV=20) Flow Branch Water Diverter Junction - Relative Flow 
C Node 1 = Inlet, Node 2 = Outlet 1, Node 3 = Outlet 2
C Required input data (in plant configuration file)
C     ADATA: 1 Outlet 1 RFF Node (see note below for fixed RFF outlet)
C            2 Outlet 2 RFF Node
C            3 Inlet RFF Node
C            4 Relative Flow Factor (RFF) (if ADATA#1 is 99)
C
C Note: For cases where the diverter supplies an 'Outlet 1' node with a 
C fixed RFF value, for example, a thermostat controlled zone demand node
C in a wet central heating model, ADATA#1 should be set to '99' and the
C fixed RFF value input in ADATA#4. See exemplar xxx for details.  

      SUBROUTINE CMP139C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/PCTIME/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

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
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/HN/NODE(MPCOM),RESID(MPCOM)

      REAL P_FRAC,SMALL,NODE,RF
     
      INTEGER IMPEXP,IPCOMP,O1N,O2N,EN,RESID

      integer itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,
     &IDYP,IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,ICONTP,ISTATS,idynow
      integer ICONDX,ICON1,INOD1,INOD2,INOD3,IPN1,IPN2,IPC1,IPC2,NPCON,
     &IPCT,NPCDAT,NAPDAT,NCI,IPOFS1,IPOFS2,IX1,NITMS,I

      real RATIMP,TC,TIMSEC,PCONDR,PCONSD,ADATA,BDATA,CDATA,CSVF,CSVP
      real PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP,CONVAR
      real PCDATF,PCDATP,ALPHA,QDATA,PCAOUT,SHTFLD

      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP139C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD

C 1 connection
      ICON1=ICONDX(IPCOMP,1,1)

C 3 - nodes 1) water inlet, 2) outlet 1, 3) outlet 3
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Relative Flow Nodes
      O1N=ADATA(IPCOMP,1)
      O2N=ADATA(IPCOMP,2)
      EN=ADATA(IPCOMP,3) ! Inlet node 
      RF=ADATA(IPCOMP,4) ! RF value if fixed value for primary node

C Check the specified valve position
      IF(O1N.NE.999)THEN
        NODE(EN)=NODE(O1N)+NODE(O2N)
        IF(NODE(EN).GT.0)THEN
          P_FRAC=NODE(O1N)/NODE(EN)
        ELSE
          P_FRAC=1. ! Force to 1 under no flow conditions to avoid NaNs
        END IF
      ELSE
        NODE(EN)=(RF+NODE(O2N))
        IF(NODE(EN).GT.0)THEN
          P_FRAC=RF/NODE(EN)
        ELSE
          P_FRAC=1. ! Force to 1 under no flow conditions to avoid NaNs
        END IF
      ENDIF

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-1.
         COUT(5)=1.
         COUT(6)=-1.
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
         
C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-P_FRAC
         COUT(3)=1.
         COUT(4)=-(1.-P_FRAC)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-P_FRAC
         COUT(3)=1.
         COUT(4)=-(1.-P_FRAC)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
      END IF

C Establish additional output variables
      NAPDAT(IPCOMP)=1
      PCAOUT(IPCOMP,1)=P_FRAC ! Outlet 1 Fraction

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3 node (ISV=20) Flow Branch Water Diverter 
     &     Junction - Relative Flow '
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' P_FRAC  = ',P_FRAC,' (0-1)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=9
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP139C trace'
      ENDIF

      RETURN
      END


C ******************** CMP140C ********************
C CMP140C generates for plant component IPCOMP with plant db code 1400 ie.
C 2 node (ISV=20) Water 3-port Diverter Valve - Relative Flow
C Node 1 = Primary (Controlled) Outlet 1, Node 2 = Outlet 2
C Required input data (in plant configuration file)
C     ADATA: 1 Outlet 2 RFF Node (99=bypass)
C            2 Inlet RFF Node
C            3 Bypass RFF
C
C     CDATA: 1 Relative Flow Factor (RFF) (-)

      SUBROUTINE CMP140C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/PCTIME/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

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
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/HN/NODE(MPCOM),RESID(MPCOM)

      REAL V_POS,SMALL,BYP,CSL,NODE
     
      INTEGER IMPEXP,IPCOMP,ILN,OLN,RESID

      integer itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,
     &IDYP,IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,ICONTP,ISTATS,idynow
      integer ICONDX,ICON1,INOD1,INOD2,INOD3,IPN1,IPN2,IPC1,IPC2,NPCON,
     &IPCT,NPCDAT,NAPDAT,NCI,IPOFS1,IPOFS2,IX1,NITMS,I

      real RATIMP,TC,TIMSEC,PCONDR,PCONSD,ADATA,BDATA,CDATA,CSVF,CSVP
      real PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP,CONVAR
      real PCDATF,PCDATP,ALPHA,QDATA,PCAOUT,SHTFLD


      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP140C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD

C 1 connection
      ICON1=ICONDX(IPCOMP,1,1)

C 3 - nodes 1) water inlet 2) outlet 1 (controlled) 2) outlet 2
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Relative Flow Nodes
      OLN=ADATA(IPCOMP,1)
      ILN=ADATA(IPCOMP,2)
      BYP=ADATA(IPCOMP,3)

C Check the specified valve position
      CSL=CDATA(IPCOMP,1)
      IF(OLN.NE.99)THEN
        NODE(ILN)=CSL+NODE(OLN)
      ELSE
        NODE(ILN)=CSL+BYP
      ENDIF

      IF(NODE(ILN).GT.0)THEN
        V_POS=CSL/NODE(ILN)
      ELSE
        V_POS=1. ! Force to 1 under no flow conditions to avoid NaNs
      END IF

      IF(V_POS.LT.0.OR.V_POS.GT.1.)THEN
        V_POS=0.
        WRITE(IUOUT,*)
     &' WARNING:CMP140C a 3-port valve control setting <0 or >1'
        WRITE(IUOUT,*)
     &' has been detected. Resetting to valve position to zero.'
      ENDIF

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-1.
         COUT(5)=1.
         COUT(6)=-1.
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
         
C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-V_POS
         COUT(3)=1.
         COUT(4)=-(1.-V_POS)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-V_POS
         COUT(3)=1.
         COUT(4)=-(1.-V_POS)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
      END IF       

C Establish additional output variables
      NAPDAT(IPCOMP)=1
      PCAOUT(IPCOMP,1)=CDATA(IPCOMP,1) ! Valve Position

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' (ISV=20) Water 3-port Diverter Valve 
     &     - Relative Flow'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' V_POS  = ',V_POS,' (0-1)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=9
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP140C trace'
      ENDIF

      RETURN
      END

C ******************** CMP141C ********************
C CMP141C generates for plant component IPCOMP with plant db code 1410 ie.
C 3-node (ISV>20) Massless Diverter Valve.
C Node 1 = Inlet, Node 2 = Primary (Controlled) Outlet 1, Node 3 = Outlet 2
C No required input data (in plant configuration file)
C Control variables (set by control loop or fixed value in plant config file) 
C     CDATA: 1 Valve position (frac)

      SUBROUTINE CMP141C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)

      REAL V_POS,SMALL
     
      INTEGER IPCOMP

      integer itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,
     &IDYP,IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,ICONTP,ISTATS,idynow
      integer ICONDX,ICON1,INOD1,INOD2,INOD3,IPN1,IPN2,IPC1,IPC2,
     &NPCON,IPCT,NPCDAT,NAPDAT,NCI,IPOFS1,IPOFS2,IX1,NITMS,I

      real CDATA,CSVF,CSVP
      real CONVAR,PCONDR,PCONSD,QDATA
      real PCDATF,PCDATP,PCAOUT

      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP141C'

C Initialize pointers to inter-connection(s) ICON, and node(s) INOD

C 1 connection
      ICON1=ICONDX(IPCOMP,1,1)

C 3 Nodes 1) inlet, 2) outlet 1 (controlled), 3) outlet 2
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2

C Check the specified valve position
      V_POS=CDATA(IPCOMP,1)
      IF(V_POS.LT.0.OR.V_POS.GT.1.)THEN
        V_POS=0.
        WRITE(IUOUT,*)
     &' WARNING:CMP141C a 3-port valve control setting <0 or >1'
        WRITE(IUOUT,*)
     &' has been detected. Resetting to valve position to zero.'
      ENDIF

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN

C Massless model therefore no significant heat transfer assumed
        COUT(1)=1.
        COUT(2)=-1.
        COUT(3)=1.
        COUT(4)=-1.
        COUT(5)=1.
        COUT(6)=-1.
        COUT(7)=0.
        COUT(8)=0.
        COUT(9)=0.
        
C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
        COUT(1)=1.
        COUT(2)=-V_POS
        COUT(3)=1.
        COUT(4)=V_POS-1.
        COUT(5)=1.
        COUT(6)=-1.
        COUT(7)=0.
        COUT(8)=0.
        COUT(9)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
        COUT(1)=1.
        COUT(2)=-V_POS
        COUT(3)=1.
        COUT(4)=V_POS-1.
        COUT(5)=1.
        COUT(6)=-1.
        COUT(7)=0.
        COUT(8)=0.
        COUT(9)=0.
      END IF

C Establish additional output variables
      NAPDAT(IPCOMP)=1
      PCAOUT(IPCOMP,1)=CDATA(IPCOMP,1) ! Valve Position

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3-node (ISV>0) Massless Diverter Valve'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' V_POS  = ',V_POS,' (0-1)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=6
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP141C trace'
      ENDIF

      RETURN
      END

C ******************** CMP143C ********************
C CMP143C generates for plant component IPCOMP with plant db code 1430 ie.
C 3-node (ISV>20) DH 3-port diverter for sub-circuit flow balancing.
C Node 1 = Inlet, Node 2 = Primary (Controlled) Outlet 1, Node 3 = Outlet 2
C     ADATA: 1 Flow component 1
C            2 Component 1 outlet flow node
C            3 Flow component 2
C            4 Component 2 outlet flow node
C            5 Flow component 3
C            6 Component 3 outlet flow node
C            7 Type
C
C Valve opens if flow in Flow Node 1 exceeds Flow Node 2 to balance
C overall system flows

      SUBROUTINE CMP143C(IPCOMP,COUT,ISTATS)

       use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer iuout,iuin,ieout
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/PCTIME/TIMSEC
      COMMON/PCTC/TC(MPCOM)

      COMMON/PCEQU/IMPEXP,RATIMP

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
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)

      REAL V_POS,SMALL,FLW1,FLW2,FLW3
     
      INTEGER IMPEXP,IPCOMP,FN1,FN2,FN3,FC1,FC2,FC3,CN1,CN2,CN3,TYP

      integer itc,icnt,ITCF,ITRACE,IZNTRC,ITU,IHRP,IHRF,
     &IDYP,IDYF,IDWP,IDWF,NSINC,ITS,NPCOMP,ICONTP,ISTATS,idynow
      integer ICONDX,ICON1,INOD1,INOD2,INOD3,IPN1,IPN2,IPC1,IPC2,NPCON,
     &IPCT,NPCDAT,NAPDAT,NCI,IPOFS1,IPOFS2,IX1,NITMS,I

      real RATIMP,TC,TIMSEC,PCONDR,PCONSD,ADATA,BDATA,CDATA,CSVF,CSVP
      real PCTF,PCRF,PUAF,PCQF,PCNTMF,PCTP,PCRP,PUAP,PCQP,PCNTMP,CONVAR
      real PCDATF,PCDATP,ALPHA,QDATA,PCAOUT,SHTFLD


      PARAMETER (SMALL=1.0E-15)
      REAL COUT(MPCOE)
      LOGICAL CLOSEA


C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP143C'

      TYP=ADATA(IPCOMP,10)

C 1 connection
      ICON1=ICONDX(IPCOMP,1,1)

C 3 - nodes 1) water inlet 2) outlet 1 (controlled) 2) outlet
      INOD1=NPCDAT(IPCOMP,9)
      INOD2=NPCDAT(IPCOMP,9)+1
      INOD3=NPCDAT(IPCOMP,9)+2
  
      IF(TYP.EQ.1)THEN
C Type 1 is balance flow to make up difference for controlled outlet flow
        FC1=ADATA(IPCOMP,4)
        CN1=ADATA(IPCOMP,5)
        FC2=ADATA(IPCOMP,6)
        CN2=ADATA(IPCOMP,7)
        FC3=ADATA(IPCOMP,8)
        CN3=ADATA(IPCOMP,9)
        FN1=NPCDAT(FC1,9)-1+CN1
        FN2=NPCDAT(FC2,9)-1+CN2
        FN3=NPCDAT(FC3,9)-1+CN3
        FLW1=CSVF(FN1,2)
        FLW2=CSVF(FN2,2)
        FLW3=CSVF(FN3,2)

C Valve opens bypass to balance flows between two circuits
        IF(FLW1.GT.FLW2)THEN
          V_POS=(FLW1-FLW2)/FLW3
          IF(V_POS.GT.1) V_POS=0.9999
        ELSE
          V_POS=0.0001 ! Avoid zero-flow circuit
        ENDIF
      ENDIF

      IF(V_POS.LT.0.OR.V_POS.GT.1.)THEN
        V_POS=0.0001
        WRITE(IUOUT,*)
     &' WARNING:CMP143C a 3-port valve control setting <0 or >1'
        WRITE(IUOUT,*)
     &' has been detected. Resetting to valve position to zero.'
      ENDIF

C Generate coefficients for the energy balance equation
      IF(ISTATS.EQ.1) THEN
         COUT(1)=1.
         COUT(2)=-1.
         COUT(3)=1.
         COUT(4)=-1.
         COUT(5)=1.
         COUT(6)=-1.
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
         
C 1st phase mass (ie. dry air) balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1)=1.
         COUT(2)=-V_POS
         COUT(3)=1.
         COUT(4)=-(1.-V_POS)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.

C 2nd phase mass (ie. vapour) balance coefficients.
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1)=1.
         COUT(2)=-V_POS
         COUT(3)=1.
         COUT(4)=-(1.-V_POS)
         COUT(5)=1.
         COUT(6)=-PCONDR(ICON1)
         COUT(7)=0.
         COUT(8)=0.
         COUT(9)=0.
      END IF 

C Establish additional output variables
      NAPDAT(IPCOMP)=1
      PCAOUT(IPCOMP,1)=V_POS ! Valve Position

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 3-node (ISV>21) Water 3-port diverter 
     &     for sub-circuit flow balancing'
         WRITE(ITU,*) ' Matrix node(s) ',INOD1,INOD2,INOD3
         WRITE(ITU,*) ' Connection(s)  ',ICON1
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' V_POS  = ',V_POS,' (0-1)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         NITMS=9
         WRITE(ITU,*) (COUT(I),I=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) then
        WRITE(ITU,*) ' Leaving subroutine CMP143C trace'
      ENDIF

      RETURN
      END

