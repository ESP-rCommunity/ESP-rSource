C This file is part of the ESP-r system.
C Copyright National Research Council of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details
C concerning licensing.

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

C  ----------------- radiant_floor.F ------------------------
C
C  This file contains routines written by Dr. Abdelaziz (Aziz) Laouadi
C  to model radiant heating / cooling systems.
C
C  RadFloor_store: Transports future (calcuated) state variable data
C     into present time array.
C
C  ----------------------------------------------------------

C  ***************** RadFloor_StaticTemp  ********************
C
C  RadFloor_StaticTemp  establishes for a plant component with
C  plant db code 960 ie. 1 node (ISV=20) Radiant Heating/Cooling System
C  whether the specified number of controlled variables is OK, and also
C  whether the number of connections to this component is correct
C  and whether the connected nodes are of a type as expected by the
C  corresponding coefficient generator routine.
C
      SUBROUTINE RadFloor_StaticTemp(IPCOMP)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &     ICONDX(MPCOM,MNODEC,MPCONC)
C     Flag indicating if radiant floor is active.
      COMMON/RadFloor/RadFloor_active

      LOGICAL RadFloor_active


C     Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
         WRITE(ITU,*)' Component ',IPCOMP,' pre-simulation data for a:'
         WRITE(ITU,*)' 1 node (ISV=20) Radiant Heating/Cooling System'
         NITMS=6
         WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NITMS)
         IF(ITU.EQ.IUOUT) THEN
            IX1=(IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C     Check user specified number of controlled variables
      NCITM=0
      IF(NCI(IPCOMP).NE.NCITM)
     &     WRITE(ITU,*) ' CMP22S warning: user specified wrong number',
     &     ' of controlled variables'

C     Check component has 1 connection only to water
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
 10   CONTINUE


C     Set radiant floor model flag
      RadFloor_active = .true.

      RETURN


C     Error handling
 990  WRITE(IUOUT,*) ' CMP68S: connection error for component ',IPCOMP
      WRITE(IUOUT,*) '         should be ',NCONS,' air connection(s)'
      STOP ' CMP68S: unresolvable error'
      END



C     ***************** RadFloor_CoeffGen  ********************
C     Author        : Dr. Abdelaziz (Aziz) Laouadi
C     Insertion date: January 2004
C     Copyright     : National Research Council of Canada
C     Synopsis: This subroutine calculates the two-dimensional effect
C     of radiant heating/cooling systems embedded in a building
C     structure.  For more information on the theory, see the NRC client report
C     entitled: "Development of a Radiant Heating and Cooling System Module
C     for HOT3000 Software, Report No. B3212.1", or the second report:
C     " DEVELOPMENT OF A PLANT COMPONENT MODEL OF RADIANT HEATING/COOLING SYSTEMS
C     FOR ESP-R/HOT3000 SOFTWARE, Report No. B3212.2 ", or
C     a related technical paper, entitled: "Development of a radiant heating and
C     cooling model for building energy simulation software:. Building and Environment
C     v. 39, n. 4, p. 421, 12 p., 2004.
C     --------------------------------------------------------------------
C     RadFloor_CoeffGen generates for plant component IPCOMP with plant db
C     code 960 ie. 1 node (ISV=20) Radiant Heating/Cooling System
C     matrix equation coefficients COUT (in order: self-coupling, cross-
C     coupling, and present-time coefficients) for energy balance (ISTATS=1),
C     1st phase mass balance (ISTATS=2), or 2nd phase mass (ISTATS=3)
C     ----------------------------------------------------------------------
C     ADATA:
C     Pipe data:
C     1 Number of circuits (integer)
C     2 inside  diameter of pipe (m)
C     3 outside diameter of pipe (m)
C     4 pipe spacing (m)
C     5 Thermal conductivity of pipe material (W/mK)
C     6 Volumetric specific heat of pipe material - rho*Cp (J/m3K)
C     Zone data:
C     7 served zone number (integer)
C     8 zone radiant surface number (integer)
C     9 injection node number (integer)
C     BDATA: none
C     CDATA: none
C     Additional Outputs:
C     Heating systems,
C     1 Minimum indoor surface temperature (C)
C     2 Maximum indoor surface temperature (C)
C     3 Supply power to the radiant system (Watts)
C     4 Average fluid temperature (C)
C     5 Average circuit length (m)
C     Cooling systems, min/max temperatures are inversed
C     1 Maximum indoor surface temperature (C)
C     2 Minimum indoor surface temperature (C)

      SUBROUTINE RadFloor_CoeffGen(IPCOMP,COUT,ISTATS)
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/Pctime/TIMSEC
      COMMON/PCTC/TC(MPCOM)
      COMMON/ptime/ptimep,ptimef
      COMMON/simsdy/iss,isf

      COMMON/PCEQU/IMPEXP,RATIMP

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &     IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &     PCNTMF(MPCOM),
     &     PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &     PCNTMP(MPCOM)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &     ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),NAPDAT(MPCOM)
      REAL QDATA,PCAOUT
      INTEGER napdat

C     building construction common block data
      COMMON/C6/INDCFG
      COMMON/PREC1/SNA(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &     NPGAP(MCOM,MS,MGP)
      COMMON/PREC15/RGAPS(MCOM,MS,MGP)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

C     surface and nodal temperatures of zones
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)

C     Generic nodal flux common - allows a flux to be added or subtracted from a node.
      COMMON/GENFLUX/GENFLXF(MCOM,MS,MN),GENFLXP(MCOM,MS,MN)



C     Parameters and common variables related to this component ----------------------
C     The common block should also be included in the MZNASS subroutine
C     (found in ESRUPLT/PMATRX.F)to store present time values of some variables

      PARAMETER (Nmax = 20, ITmax = 100, PI=3.1415926535, PI2=0.5*PI)
      PARAMETER (Relax = 1.0, Tol = 1.E-6, TolF = 1.E-3, Small = 1.E-15)
      COMMON /RHC/Acof(MCOM,MS,Nmax,Nmax),Fcof(MCOM,MS,Nmax,Nmax),
     &     Fcofp(MCOM,MS,Nmax,Nmax),ConAf(MCOM,MS),ConBf(MCOM,MS),
     &     ConDf(MCOM,MS),ConAp(MCOM,MS),ConBp(MCOM,MS),
     &     ConDp(MCOM,MS), TetaMf(MCOM,MS), TetaMp(MCOM,MS),
     &     Tpipef(MCOM,MS), Tpipep(MCOM,MS)
      REAL    COUT(MPCOE), J11, J12, J21, J22
      
      character outs*124
C----------------------------------------------------------------------

C     Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &     ITRACE(37).NE.0) WRITE(ITU,*) ' Entering subroutine CMP95C'

      IF(INDCFG .NE. 3) THEN
         call usrmsg('Use this plant component only when the simulaton',
     &    'is active, i.e., INDCFG = 3. Leaving subroutine CMP95C','W')
         RETURN
      ENDIF

C     Initialize pointers to inter-connection(s) ICON, and node(s) INOD
      ICON1=ICONDX(IPCOMP,1,1)
      INOD1=NPCDAT(IPCOMP,9)

C     variable assignments -----------------------------------------------
C     Plant time step
      Dt    = TIMSEC
C     Inlet fluid temperature
      Tin   = CONVAR(ICON1,1)
C     Fluid flow rate per circuit
      FlowR = PCONDR(ICON1) * CONVAR(ICON1,2) / ADATA(IPCOMP,1)

C     Cross-section surface area of the pipe occupied by the fluid
      AreaF = 0.25 * PI * ADATA(IPCOMP,2)**2

C     Cross-section surface area of the pipe - excluding the fluid region
      AreaP = 0.25 * PI * ADATA(IPCOMP,3)**2 - AreaF

C     Product of density (Rho), specific heat (Cp) and cross-section surface
C     area of the fluid
      RCPAf = RHOFLD(3, Tin) * SHTFLD(3, Tin) * AreaF

C     Product of density (Rho), specific heat (Cp) and cross-section surface
C     area of the pipe material
      RCPAp = ADATA(IPCOMP,6) * AreaP

C     Conduction resistance of the pipe
      ResC  = LOG( ADATA(IPCOMP,3) / ADATA(IPCOMP,2) ) /
     &     ( 2. * PI * ADATA(IPCOMP,5) )

C     Product of fluid flow rate and specific heat per circuit
      FLWCp = FlowR * SHTFLD(3, Tin) + Small

C     Fluid velocity
      Vel   = FlowR / ( RHOFLD(3, Tin) * AreaF )

C     Served zone No.
      Iz = INT( ADATA(IPCOMP,7) )

C     Zone surface No.
      Is = INT( ADATA(IPCOMP,8) )

C     Surface injection node No.
      In = INT( ADATA(IPCOMP,9) )

C     Area of the radiant surface
      AreaR   = SNA(Iz, Is)

C     1-D average temperature of the pipe outer surface
      Tpipe1D = TFC(Iz, Is, In)

C     1-D average temperature of the inside face of the radiant surface
      Tsurf1D = TFS(Iz, Is)

C---------------------------------------------------------------------------------
C     Calculate the total thickness, average conductivity and volumetric specific heat
C     of the slab system, and the distance of pipe centre from the indoor surface.
C     Variable thermal properties of the construction are accounted for here

C     Number of air gaps
      NairGap  = NGAPS(Iz,Is)

C     Initialize the average conductivity and thermal capacity of the construction
      AKslab  = 0.0
      RCPslab = 0.0

C     and the variable denoting the total thickness of the construction
      THKC    = 0.0

C     This variable denotes the position of the center node of a construction layer
      Ncentr  = 0
      DO 11 J = 1, NELTS(Iz,Is)
C     Layer conductivity
         COND = THRMLI(Iz,Is,J,1)
C     Layer density
         DENS = THRMLI(Iz,Is,J,2)
C     Layer specific heat
         SHT  = THRMLI(Iz,Is,J,3)
C     Layer thickness
         THKL = THRMLI(Iz,Is,J,4)

         THKC = THKC + THKL

C     Find the resistance of the air gap associated with layer J.
         IF(NairGap .GT. 0 .AND. THRMLI(Iz,Is,J,1) .LT. Small)THEN
            DO 12 iNairGap =1, NairGap
               IndxAirG = NPGAP(Iz,Is,iNairGap)
               IF(IndxAirG .EQ. J) THEN
                  Ncentr = Ncentr + 1
                  AirRes = RGAPS(Iz,Is,iNairGap)
                  COND = THKL/AirRes
               ENDIF
 12         CONTINUE
         ELSE
            Ncentr = Ncentr + 2
            IF(In .EQ. Ncentr+1) Xqc = THKC
            IF(In .EQ. Ncentr  ) Xqc = THKC - THKL/2.
            IF(In .EQ. Ncentr-1) Xqc = THKC - THKL
         ENDIF
         AKslab  = AKslab  + COND*THKL
         RCPslab = RCPslab + DENS*SHT*THKL
 11   CONTINUE

C     average thermal conductivity, diffusivity and volumetric
C     specific heat of slab system
      AKslab  = AKslab/THKC
      RCPslab = RCPslab/THKC
      DIFslab = AKslab / RCPslab

C     Check if the specified injection node is valid -----------------
      IF(In .LT. 1 .OR. In .GT. Ncentr+1) THEN
         WRITE(outs,*)'CMP95C Fatal error: injection node',In,
     &     ' not in range 1 to',Ncentr+1
         call edisp(iuout,outs)
         WRITE(*,outs)
         WRITE(*,*)'Source error: subroutine CMP95C.  Simulation halted'
         call pauses(2)   ! pause for 2 seconds then abort program.
         STOP
      ENDIF

C     Average circuit length
      CirL  = AreaR/( ADATA(IPCOMP,1) * ADATA(IPCOMP,4) )

C     Length of the calculation domain along the y-axis - perpendicular to the pipes
      YL = ADATA(IPCOMP,4) / 2.

C     Length of the calculation domain along the x-axis - perpendicular to the surface
      XL = THKC

C     x-position of the pipe centre
      Xq = XL -Xqc

C     Size (Dx, Dy, volume) of the heat generation control volume = pipe volume
      DXq   = 0.25 * SQRT(PI) * ADATA(IPCOMP,3)
      DYq   = DXq
      Volq  = 4. * DXq * DYq * CirL

C--------------------------------------------------------------------------------------
C     INITIALIZE VARIABLES BEFORE SIMULATION
      IF( (ptimep .LE. 1.) .AND. (IDYP .LE. ISS) ) THEN

C     Circuit average temperature difference (Tfluid - Tpipe) at the present time
         TetaMp(Iz,Is) = 0.

C     Pipe outer surface temperature at the present time
         Tpipep(Iz,Is) = Tpipe1D

C     Calculate the film convection coefficient
         CALL CVCOEFF1(3,Tin, Vel, ADATA(IPCOMP,2), Hfilm)

C     Convection thermal resistance
         ResI = 1./( PI * ADATA(IPCOMP,2) * Hfilm + Small)

C     Product of total thermal conductance and pipe perimeter
         UP = 1./(ResC + ResI)

C     Evaluate the equation constants: ConAp, ConBp, ConDp at the present time.
C     See the above reports for more details
         ConAp(Iz,Is) = RCPAf / UP
         ConBp(Iz,Is) = 1.
         ConDp(Iz,Is) = 0.

C     Evaluate the equation constants: ConAf, ConBf, ConDf at the future time.
C     See the above reports for more details
         ConAf(Iz,Is)  = ConAp(Iz,Is)
         ConBf(Iz,Is)  = ConBp(Iz,Is)
         ConDf(Iz,Is)  = ConDp(Iz,Is)

C     calculate series coefficients: Aij abd Fij. See the above reports for more details
         DO 10 I = 1, Nmax
            II = I - 1
            DO 10 J = 1, Nmax

               OmegX = FLOAT(1+2*II) * PI2 / XL
               OmegY = FLOAT(J) * PI / YL
               Cof = OmegX*OmegX + OmegY*OmegY
               DEN = XL * YL * OmegX * OmegY * Cof

               Acof(Iz,Is,I,J) = 8.*SIN(OmegX*DXq) * COS(OmegX*Xq) *
     &              SIN(OmegY*DYq) / DEN
               Fcof(Iz,Is,I,J) = 0.

 10         CONTINUE

         ENDIF

C----------------------------------------------------------------------
C     Generate coefficients for energy balance equation
         IF(ISTATS.EQ.1) THEN

C     Establish heat capacity of component mass CM (J/K) and
C     fluid heat capacity rate(s) C (W/K), ie. SUM(mass flow * specific heat)
C     - these are not used here -.
            CM = (RCPAf + RCPAp) * CirL * ADATA(IPCOMP,1)
            C1 = PCONDR(ICON1)*CONVAR(ICON1,2)*SHTFLD(3,Tin)

C     Calculate current component time-constant TC
C     TC(IPCOMP)=CM/AMAX1(SMALL,(C1+UA))
            TC(IPCOMP) = ConAf(Iz,Is) / AMAX1(Small,ConBf(Iz,Is))

C     Set up implicit/Explicit weighting factor ALPHA (1 = fully implicit)
            IF(IMPEXP.EQ.1) THEN
               ALPHA=1.
            ELSE IF(IMPEXP.EQ.2) THEN
               ALPHA=RATIMP
            ELSE IF(IMPEXP.EQ.3) THEN
               IF(TIMSEC .GT. 0.63*TC(IPCOMP)) THEN
                  ALPHA=1.
               ELSE
                  ALPHA=RATIMP
               END IF
            ELSE IF(IMPEXP.EQ.4) THEN
               RCPAf = 0.
               RCPAp = 0.
               CM=0.
               ALPHA=1.
            END IF
            ALPHAc = 1. - ALPHA

C----------------------------------------------------------------------
C     Start iteration here to calculate the fluid and pipe temperatures
            TetaMf(Iz,Is) = TetaMp(Iz,Is)
            Tpipef(Iz,Is) = Tpipep(Iz,Is)

C     Heat source rate at the present time
            QsorP = GENFLXP(Iz,Is,In) *AreaR / (ADATA(IPCOMP,1)*Volq)

            DO 20 Iter = 1, ITmax
               dTpdt = ( Tpipef(Iz,Is) - Tpipep(Iz,Is) ) / Dt
               TetaIN = Tin - Tpipef(Iz,Is)

C     Fluid average temperature over the circuit length
               TFmean = TetaMf(Iz,Is) + Tpipef(Iz,Is)

C     Calculate film coefficient and convection resistance
               CALL CVCOEFF1(3,TFmean, Vel, ADATA(IPCOMP,2), Hfilm)
               ResI = 1./( PI * ADATA(IPCOMP,2) * Hfilm + Small)

C     Overall conductance
               UP = 1./(ResC + ResI)
               UA = UP*CirL

C     This is a temporary variable
               RCPAm = RCPAf + UP*ResC*RCPAp

C     calculate the constant C (see the above reports)
               C = UP/FLWCp

C     Product of circuit length and constant C
               CL = C * CirL

               ExpCL = EXP(-CL)
               DEN = CL -1. + ExpCL

C     Calculate the constant A, B, D at the future time
               ConAf(Iz,Is) = RCPAf / UP
               ConBf(Iz,Is) = CL / DEN
               ConDf(Iz,Is) = (1.- ExpCL) / DEN * TetaIN
     &              - RCPAm/UP*dTpdt

C     Heat transfer per circuit
               Qcir = UA * ( TetaMf(Iz,Is) - ResI * RCPAp * dTpdt )

C     Calculate the heat generation rate per pipe volume
               Qsor = Qcir/Volq

C     calculate the serires coefficient Fij, pipe temperature and min and max
C     inside surface temperatures
               SumTpipe = 0.
               SumTmin  = 0.
               SumTmax  = 0.
               SumJacob = 0.

               DO 30 J = 1, Nmax
                  Omegj = FLOAT(J)*PI/YL
                  DO 30 I = 1, Nmax
                     II = I - 1
                     Omegi = FLOAT(1+2*II)*PI2 / XL
                     Omeg2 = Omegi**2 + Omegj**2

                     Fact = DIFslab * Omeg2 * Dt
                     Factc = 1. + Fact

                     Fcof(Iz,Is,I,J) = Fcofp(Iz,Is,I,J) *
     &                    ( 1. - ALPHAc * Fact**2 ) / Factc +
     &                    ( ALPHA*Qsor + ALPHAc*Factc*QsorP )
     &                    * Fact/Factc

                     Pro  = Fcof(Iz,Is,I,J) * Acof(Iz,Is,I,J)

                     SumTmax  = SumTmax  + Pro
                     SumTmin  = SumTmin  + Pro*COS(Omegj*YL)
                     SumTpipe = SumTpipe + Pro*COS(Omegi*Xq)
                     SumJacob = SumJacob + Fact / Factc *
     &                    Acof(Iz,Is,I,J) * COS(Omegi*Xq)

 30               CONTINUE

                  SumTmax  = SumTmax  / AKslab
                  SumTmin  = SumTmin  / AKslab
                  SumTpipe = SumTpipe / AKslab
                  SumJacob = SumJacob / AKslab

C     Form the djacobien matrix for the Newton-Raphson method
                  F1 = ConAp(Iz,Is) * ( ConAf(Iz,Is)
     &                 + ConBf(Iz,Is)*Dt ) * TetaMf(Iz,Is)
     &                 - ( ALPHA*ConAp(Iz,Is)*ConAf(Iz,Is)
     &                 + ALPHAc* ( ConAp(Iz,Is)
     &                 - ConBp(Iz,Is)*Dt ) * (ConAf(Iz,Is)
     &                 + ConBf(Iz,Is) * Dt ) ) * TetaMp(Iz,Is) -
     &                 ( ALPHA*ConDf(Iz,Is) * ConAp(Iz,Is)
     &                 + ConDp(Iz,Is)*ALPHAc *
     &                 (ConAf(Iz,Is) + ConBf(Iz,Is) * Dt) ) * Dt

                  F2 = Tpipef(Iz,Is) - Tpipe1D - SumTpipe

                  J11 = ConAp(Iz,Is) * (ConAf(Iz,Is)
     &                 + ConBf(Iz,Is) * Dt)
                  J12 = ALPHA*ConAp(Iz,Is)*( RCPAm/UP +
     &                 Dt * (1.-ExpCL)/DEN )
                  J21 = - UA/Volq * SumJacob
                  J22 = 1. - ResI*RCPAp / Dt * J21

                  Delta  = J11*J22 - J12*J21 + Small
                  Delta1 = -F1*J22 + J12*F2
                  Delta2 = -J11*F2 + J21*F1

                  TetaMf(Iz,Is) = TetaMf(Iz,Is) + Relax * Delta1/Delta
                  Tpipef(Iz,Is) = Tpipef(Iz,Is) + Relax * Delta2/Delta

C     Check whether or not convergence is reached
                  error  = (Delta1**2 + Delta2**2)/Delta**2
                  errorF = F1**2 + F2**2

                  IF( (Error .LT. Tol) .AND. (ErrorF .LT. TolF)) GOTO 50

 20            CONTINUE

 50            CONTINUE

C     iteration now terminated --------------------------------------------

C     temperatures and node heat injection flux (W)
               Ratio = ( (1. + CL)*ExpCL - 1.)/ (CL -1. + ExpCL)
               Tout = Ratio*Tin
     &              + (1. - Ratio)*(TetaMf(Iz,Is) + Tpipef(Iz,Is) )
               Tmin = Tsurf1D + SumTmin
               Tmax = Tsurf1D + SumTmax
               Qboiler = ADATA(IPCOMP,1) * FLWCp * (Tin - Tout)
               Qnode   = ADATA(IPCOMP,1) * Qcir
               GENFLXF(Iz,Is,In) = Qnode / AreaR

C     Establish additional output variables
C     Minimum inside surface temperature
               PCAOUT(IPCOMP,1) = Tmin
C     Maximum inside surface temperature
               PCAOUT(IPCOMP,2) = Tmax
C     Supply power to the radiant surface
               PCAOUT(IPCOMP,3) = Qboiler
C     Average fluid temperature
               PCAOUT(IPCOMP,4) = TFmean
C     Average circuit length
               PCAOUT(IPCOMP,5) = CirL


C     Establish matrix equation self- and cross-coupling coefficients
               COUT(1) = 1.
               COUT(2) = - Ratio
C     and then present-time coefficient (ie. right hand side)
               COUT(3) = (1.- Ratio)*( TetaMf(Iz,Is) + Tpipef(Iz,Is) )

C     Store "environment" variables future values (not used here)
               PUAF(INOD1)=UA
               PCTF(ICON1)=CONVAR(ICON1,1)
               PCRF(ICON1)=C1

C     1st phase mass (ie. water) balance coefficients
            ELSE IF(ISTATS.EQ.2) THEN
               COUT(1)=1.
               COUT(2)=-PCONDR(ICON1)
               COUT(3)=0.

C     2nd phase mass (ie. vapour) balance coefficients
            ELSE IF(ISTATS.EQ.3) THEN
               COUT(1)=1.
               COUT(2)=0.
               COUT(3)=0.
            END IF

C     Trace output
            IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &           ITRACE(37).NE.0) THEN
               WRITE(ITU,*) ' Component      ',IPCOMP,':'
               WRITE(ITU,*)
     &              ' 1 node (ISV=20) Radiant Heating/Cooling System'
               WRITE(ITU,*) ' Matrix node(s) ',INOD1
               WRITE(ITU,*) ' Connection(s)  ',ICON1
               IF(ISTATS.EQ.1) THEN
                  WRITE(ITU,*) ' CM     = ',CM,' (J/K)'
                  WRITE(ITU,*) ' C1     = ',C1,' (W/K)'
                  WRITE(ITU,*) ' TC     = ',TC(IPCOMP),' (s)'
                  WRITE(ITU,*) ' ALPHA  = ',ALPHA,' (-)'
                  WRITE(ITU,*) ' UA     = ',UA,' (W/K)'
                  WRITE(ITU,*) ' PCNTMF = ',PCNTMF(IPCOMP),' (C)'
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
     &           ITRACE(37).NE.0) WRITE(ITU,*)
     &           ' Leaving subroutine CMP95C'

            RETURN
            END

C     ************** RadFloor_store * ********************
C     Author        : Dr. Abdelaziz (Aziz) Laouadi
C     Insertion date: January 2004
C     Copyright     : National Research Council of Canada
C
C     NOTES:
C     This is a storage routine that was originally embedded
C     within  MZNASS (pmatrx.F), but has been relocated here
C     for simplicity's sake. It is **EXTREMELY** SLOW and
C     effectively lengthens simulation run times by a factor
C     of 10!!! If CETC actually plans on using this model,
C     the implementation of this routine will have to
C     be revisited.
C     (Alex Ferguon - May 04)
C
C     REVISIONS:
C
C     This subroutine was expanded to perform 3 basic
C     time-row manipulations, to support inter-domain
C     iteration:
C
C      - Saving present time-row values
C      - Winding time row forward
C        (future values -> present values)
C      - Rewinding time row
C        (present values -> future values,
C         stored values -> present values)
C
C      ( Alex Ferguson - July 04 )
C
C
C     ****************************************************
      SUBROUTINE RadFloor_store(iOperation)
      IMPLICIT NONE

#include "plant.h"
#include "building.h"
#include "CETC_definitions.h"

C number of zones and number of constructions in model
      common/c1/ncomp,ncon
      integer ncomp,ncon

C     Flag indicating if radiant floor is active.
      COMMON/RadFloor/RadFloor_active

C     data for storage
      INTEGER Nmax, Iz, Is, J, I
      PARAMETER (Nmax = 20)
      COMMON /RHC/Acof(MCOM,MS,Nmax,Nmax),Fcof(MCOM,MS,Nmax,Nmax),
     &        Fcofp(MCOM,MS,Nmax,Nmax),ConAf(MCOM,MS),ConBf(MCOM,MS),
     &          ConDf(MCOM,MS),ConAp(MCOM,MS),ConBp(MCOM,MS),
     &          ConDp(MCOM,MS), TetaMf(MCOM,MS), TetaMp(MCOM,MS),
     &          Tpipef(MCOM,MS), Tpipep(MCOM,MS)

      REAL Acof,Fcof,Fcofp,ConAf,ConBf,ConDf,ConAp,ConBp,ConDp,
     &     TetaMf, TetaMp,Tpipef, Tpipep

C.....Storage variables
      REAL Fcofp_store(MCOM,MS,Nmax,Nmax)
      REAL TetaMp_store(MCOM,MS)
      REAL Tpipep_store(MCOM,MS)
      REAL ConAp_store(MCOM,MS)
      REAL ConBp_store(MCOM,MS)
      REAL ConDp_store(MCOM,MS)

      SAVE Fcofp_store,TetaMp_store,Tpipep_store,ConAp_store,ConBp_store
      SAVE ConDp_store


C.....Passed variable
      integer iOperation        ! flag indicating what sort of operation


      LOGICAL RadFloor_active

C     If radiant floor is active, transport future data to present data
C     arrays. Note - it's important that this code is not called if
C     the radiant floor model is not active; otherwise it will
C     unnecessarly slow the simulation down!
      IF ( RadFloor_active ) THEN

         DO Iz = 1, ncomp
            DO Is = 1, ncon
               DO J = 1, Nmax
                  DO I = 1, Nmax

                     if (iOperation .eq. iSaving ) then

                        Fcofp_store(Iz,Is,I,J) = Fcofp(Iz,Is,I,J)

                     elseif ( iOperation .eq. iWinding ) then

                        Fcofp(Iz,Is,I,J)  = Fcof(Iz,Is,I,J)

                     elseif ( iOperation .eq. iRewinding ) then

                        Fcof(Iz,Is,I,J)  = Fcofp(Iz,Is,I,J)
                        Fcofp(Iz,Is,I,J) = Fcofp_store(Iz,Is,I,J)

                     endif

                  enddo
               enddo



               if (iOperation .eq. iSaving ) then

                  TetaMp_store(Iz,Is) = TetaMp(Iz,Is)
                  Tpipep_store(Iz,Is) = Tpipep(Iz,Is)

                  ConAp_store(Iz,Is)  = ConAp(Iz,Is)
                  ConBp_store(Iz,Is)  = ConBp(Iz,Is)
                  ConDp_store(Iz,Is)  = ConDp(Iz,Is)

               elseif (iOperation .eq. iWinding ) then

                  TetaMp(Iz,Is) = TetaMf(Iz,Is)
                  Tpipep(Iz,Is) = Tpipef(Iz,Is)

                  ConAp(Iz,Is)  = ConAf(Iz,Is)
                  ConBp(Iz,Is)  = ConBf(Iz,Is)
                  ConDp(Iz,Is)  = ConDf(Iz,Is)

               elseif (iOperation .eq. iRewinding ) then

                  TetaMf(Iz,Is) = TetaMp(Iz,Is)
                  Tpipef(Iz,Is) = Tpipep(Iz,Is)

                  ConAf(Iz,Is)  = ConAp(Iz,Is)
                  ConBf(Iz,Is)  = ConBp(Iz,Is)
                  ConDf(Iz,Is)  = ConDp(Iz,Is)

                  TetaMp(Iz,Is) = TetaMp_store(Iz,Is)
                  Tpipep(Iz,Is) = Tpipep_store(Iz,Is)

                  ConAp(Iz,Is)  = ConAp_store(Iz,Is)
                  ConBp(Iz,Is)  = ConBp_store(Iz,Is)
                  ConDp(Iz,Is)  = ConDp_store(Iz,Is)

               endif

            enddo

         enddo

      ENDIF

      END

