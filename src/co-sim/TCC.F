C This file is part of the ESP-r system.
C Copyright Carleton University 2011-2012.
C Please Contact Ian Beausoliel-Morrison for details
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

C************************************************************************************
C************************************************************************************
C MODULE TCC
C *******************************************************************************
C This module contains the following subroutines related to the
C TRNSYS coupling component (TCC) in support of the co-simulation
C between ESP-r and TRNSYS:
C TCC_static      Static plant component template, which is executed prior to
C                 the time-step simulation.
C TCC_coeff_gen   Plant component coefficient generator, which is executed each
C                 time-step of the simulation (more if there are iterations within
C                 ESP-r plant domain solver or multiple invocations between ESP-r
C                 and TRNSYS within the time-step).
C************************************************************************************
C************************************************************************************
      MODULE TCC
      USE COSIMDATATYPES
      IMPLICIT NONE

      SAVE  ! This ensures values persist between sucessive invocations within the time-step.
      INTEGER, PARAMETER :: HCC = 1      ! Index to indicate this TCC represents a hydronic coupling (HCC).
      INTEGER, PARAMETER :: ACC = 2      ! Index to indicate this TCC represents an air coupling (ACC).
      LOGICAL :: COSIM                   ! Logical set to true is a TCC plant component is detected.
      INTEGER :: nHCC_S, nHCC_R          ! Number of sending and receiving HCCs specified by user.
      INTEGER :: nACC_S, nACC_R          ! Number of sending and receiving ACCs specified by user.
      TYPE(EsprTrnsysData) :: COSIM_DATA       ! Derived data structure (DDS).
      TYPE(EsprTrnsysData) :: COSIM_DATA_SAVED ! Copy of DDS.
      TYPE(TCC_Indeces) :: iTCC

      CONTAINS
C ***********************************************************************************
C This subroutine initializes the data contained in the EsprTrnsysData DDS.
C ***********************************************************************************
      SUBROUTINE InitializeCosimDataTypes

      IMPLICIT NONE
      integer :: i

      DO i=1,MAX_FCCs
        iTCC%iHCCtoT(i)=0
        iTCC%iHCCtoE(i)=0
        iTCC%iACCtoT(i)=0
        iTCC%iACCtoE(i)=0

        COSIM_DATA%HCC_to_TRNSYS(i)%Flowrate=0
        COSIM_DATA%HCC_to_TRNSYS(i)%Temperature=0
        COSIM_DATA%HCC_to_TRNSYS(i)%Moisture_flowrate=0

        COSIM_DATA%HCC_to_ESPr(i)%Flowrate=0
        COSIM_DATA%HCC_to_ESPr(i)%Temperature=0
        COSIM_DATA%HCC_to_ESPr(i)%Moisture_flowrate=0

        COSIM_DATA%ACC_to_TRNSYS(i)%Flowrate=0
        COSIM_DATA%ACC_to_TRNSYS(i)%Temperature=0
        COSIM_DATA%ACC_to_TRNSYS(i)%Moisture_flowrate=0

        COSIM_DATA%ACC_to_ESPr(i)%Flowrate=0
        COSIM_DATA%ACC_to_ESPr(i)%Temperature=0
        COSIM_DATA%ACC_to_ESPr(i)%Moisture_flowrate=0
      END DO
      DO i=1,MAX_ZONES
        COSIM_DATA%ESPrZonesData(i)%AirPointTemperatures=0
        COSIM_DATA%ESPrZonesData(i)%AirPointHumidities=0
        COSIM_DATA%ESPrZonesData(i)%AirPointCasualGains=0
      END DO
      COSIM_DATA%ESPrConverged=0
      COSIM_DATA%TrnsysConverged=0

      END SUBROUTINE InitializeCosimDataTypes

      
C ***********************************************************************************
C This subroutine is the static template for the TRNSYS coupling component (TCC)
C that is used for coupling ESP-r plant networks to TRNSYS networks in the
C ESP-r / TRNSYS co-simulator.
C This subroutine checks whether the component is properly used in the user-defined
C plant network, it assigns user-input data to module variables, and it performs
C time-invariant calculations.
C This component has a single control volume (CV1).  It can be used to represent
C both sending and receiving and both hydronic (HCC) and air-based (ACC) coupling
C components.
C ***********************************************************************************
      SUBROUTINE TCC_STATIC(IPCOMP,CouplingIndex,CouplingType,Sending)
      IMPLICIT NONE
#include "building.h"
#include "plant.h"


C--------------------------------------------------------------------------------------------
C Declare calling parameters
C--------------------------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: CouplingIndex  ! The port number between ESP-r and TRNSYS.
      INTEGER, INTENT(IN) :: CouplingType   ! 1=HCC; 2=ACC.
      INTEGER, INTENT(IN) :: IPCOMP
      LOGICAL, INTENT(IN) :: Sending        ! Logical: true=sending; false=receiving.

C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C Unit numbers for providing feedback to user during simulation.
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Trace output.
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C Plant component data supplied by user (via plant network file).
      COMMON/PDBDT/ADATA,BDATA
      REAL ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)

C Number of components in plant network and control actuation signals.
      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

C Connecting node data.
      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL CONVAR(MPCON,MCONVR)
      INTEGER ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA, J, N_expect, itemp
      LOGICAL mistake


C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. ITRACE(35)/=0) THEN
        WRITE(ITU,*) ' Component ',IPCOMP,' pre-sim data for the:'
        IF (CouplingType == HCC) THEN
          WRITE(ITU,*) ' 1-node HCC ESP-r / TRNSYS co-simulation '
        ELSE IF (CouplingType == ACC) THEN
          WRITE(ITU,*) ' 1-node ACC ESP-r / TRNSYS co-simulation '
        END IF
        NumADATA = 1  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',(ADATA(IPCOMP,J),J=1,NumADATA)
        IF(ITU==IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/5)*5
          IF(itemp==IPCOMP .OR. IPCOMP==NPCOMP) call epagew ! write 5 lines at a time.
        END IF
      END IF


C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(IPCOMP) holds the number of possible plant control variables
C as specified in the .pln file. This component cannot be controlled.
C---------------------------------------------------------------------------------
      N_expect = 0
      IF(NCI(IPCOMP) /= N_expect) THEN
        WRITE(ITU,*) ' TCC_static: incorrect number',
     &               ' of controlled variables specified.'
      ENDIF


C---------------------------------------------------------------------------------
C Check that the connection to node 1 is of the correct type. This connection should
C be of type ISV=20 for HCC and ISV=21 for ACC so that the TCC component can be used
C in networks in which both first and second phase mass balances are formed.
C Variables used:
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV=0,10,20 node represents water + ....
C               ISV=1,11,21 node represents dry air + ....
C               ISV=9,19,29 node represents some solid material only
C               0 <=ISV<10  model suitable for energy balance only
C               10<=ISV<20  model suitable for energy + single phase mass balance
C               20<=ISV<30  model suitable for energy + two phase mass balances
C---------------------------------------------------------------------------------
      mistake = .false.
      IF (CouplingType == HCC) THEN
        IF( ICONTP( ICONDX(IPCOMP,1,1) ) /= 20 ) mistake=.true.
      ELSE IF (CouplingType == ACC) THEN
        IF( ICONTP( ICONDX(IPCOMP,1,1) ) /= 21 ) mistake=.true.
      END IF

      IF(mistake)THEN
        WRITE(IUOUT,*) ' TCC_static: incorrect connection type',IPCOMP
        STOP ' TCC_static: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Determine the port number (or index) of this coupling to TRNSYS
C (e.g. 1st, 2nd, 3rd HCC/ACC).
C---------------------------------------------------------------------------------
      IF (CouplingType == HCC) THEN
        IF (Sending) THEN
          iTCC%iHCCtoT(CouplingIndex) =INT( ADATA(IPCOMP,1) )
        ELSE
          iTCC%iHCCtoE(CouplingIndex) =INT( ADATA(IPCOMP,1) )
        END IF
      ELSE IF (CouplingType == ACC) THEN
        IF (Sending) THEN
          iTCC%iACCtoT(CouplingIndex) =INT( ADATA(IPCOMP,1) )
        ELSE
          iTCC%iACCtoE(CouplingIndex) =INT( ADATA(IPCOMP,1) )
        END IF
      END IF


C---------------------------------------------------------------------------------
C Completion of analysis prior to commencing time-step simulation.
C---------------------------------------------------------------------------------
      RETURN
      END SUBROUTINE TCC_static


C ***********************************************************************************
C This subroutine is the coefficient generator for the TRNSYS coupling component (TCC)
C that is used for coupling ESP-r plant networks to TRNSYS networks in the
C ESP-r / TRNSYS co-simulator.
C It is invoked each iteration of the plant matrix processing during each time-step
C to establish the coefficients for the sub-matrices that define the energy, 1st phase
C mass flow, and 2nd phase mass flow balances on the TCC's single control volume.
C ***********************************************************************************
      SUBROUTINE TCC_coeff_gen(IPCOMP,COUT,ISTATS,CouplingIndex,
     &                         CouplingType,Sending)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C--------------------------------------------------------------------------------------------
C Declare calling parameters
C--------------------------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: CouplingIndex  ! The port number between ESP-r and TRNSYS.
      INTEGER, INTENT(IN) :: CouplingType   ! 1=HCC; 2=ACC.
      LOGICAL, INTENT(IN) :: Sending        ! Logical: true=sending; false=receiving.
      INTEGER, INTENT(IN) :: IPCOMP         ! Index number of this plant component.
      INTEGER, INTENT(IN) :: ISTATS         ! Flag for balance under consideration: 1=energy;
                                            ! 2=1st phase mass; 3=2nd phase mass.
      REAL, INTENT(OUT) :: COUT(MPCOE)      ! Plant matrix coefficients sent to solver.

C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C Unit numbers for providing feedback to user during simulation.
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Connecting node data.
      COMMON/C10/NPCON,IPC1,IPN1,IPCT,IPC2,IPN2,PCONDR,PCONSD
      INTEGER NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON)
      INTEGER IPC2(MPCON),IPN2(MPCON)
      REAL PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCOND/CONVAR,ICONTP,ICONDX
      REAL CONVAR(MPCON,MCONVR)
      INTEGER ICONTP(MPCON),ICONDX(MPCOM,MNODEC,MPCONC)

C Trace output.
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C Additional res output from plant domain.
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA, PCAOUT
      INTEGER napdat

C Simulation time step data.
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Plant component pointer.
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Iteration control.
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV,CSVI
      INTEGER MAXITP, itrclp, ICSV(MPNODE,MPVAR)
      REAL PERREL, PERTMP, PERFLX, PERMFL, CSVI(MPNODE,MPVAR)

C Plant state variables.
      COMMON/PCVAL/CSVF,CSVP
      REAL CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

C Number of components in plant network and control actuation signals.
      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15, pcname

C `Additional' plant component data.
      common/pcdat/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      REAL PCDATF   ! Future time-row.
      real PCDATP   ! Present time-row.


C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER CV1, con_CV1
      INTEGER itemp
      REAL T_TCC              ! Temperature of fluid entering this TCC (oC).
      REAL Mdot_TCC           ! Mass flow rate of fluid entering this TCC (kg/s).
      REAL Mdot_moisture_TCC  ! Moisture flow rate of air entering this TCC (kg/s).

C---------------------------------------------------------------------------------
C Set flag to indicate that a TCC is present in the plant network.  Generally this
C means there is a co-simulation between ESP-r and TRNSYS; however, it is possible
C to performan an ESP-r-only simulation with TCCs.
C---------------------------------------------------------------------------------
      COSIM=.true.


C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering subroutine TCC_coeff_gen'
      END IF

      
C---------------------------------------------------------------------------------
C Set local variables to point to the node and to the connection.
C Variables used:
C    ICONDX(i,j,k)  the connection number for the k'th connection to the j'th node
C                   of component i. It is used as a pointer.
C    NPCDAT(i,9)    row and column number defining location of component `i'
C                   sub-matrix template in the overall plant network matrix. It is
C                   used to identify the location of the current component's nodes
C                   within the global plant matrix.
C    CV(i)          global matrix node number for node 'i'
C    con_CV1        connection number to node 1
C---------------------------------------------------------------------------------
      CV1     = NPCDAT(IPCOMP,9)
      con_CV1 = ICONDX(IPCOMP,1,1)


C---------------------------------------------------------------------------------
C Sending TCC.  This component is sending a fluid stream to TRNSYS.  The function
C of this instance of the TCC is to simply pass data to TRNSYS.  The matrix
C coefficients are established to make this a flow-through component, i.e.
C the exiting conditions are made equal to the entering conditions.
C---------------------------------------------------------------------------------
      Sending_Receiving: IF (Sending) THEN
      
      
C---------------------------------------------------------------------------------
C Send coupling data to Harmonizer which will pass it into TRNSYS's Type 130.
C Variables/functions used:
C    CONVAR(i,1) holds the temperature (oC) of sending node for connection `i'.
C    CONVAR(i,2) holds the '1st phase' mass flow rate (kg/s) at sending node for conn `i'.
C    CONVAR(i,3) holds the '2nd phase' mass flow rate (kg/s) at sending node for conn `i'.
C    PCONDR(i)   is the ratio of flow rate leaving sending node `i' that reaches
C                receiving node.
C---------------------------------------------------------------------------------
        T_TCC = CONVAR(con_CV1,1)                               ! Temp of water (HCC) or air (ACC) entering TCC (oC).
        Mdot_TCC = PCONDR(con_CV1) * CONVAR(con_CV1,2)          ! Mass flow of water (HCC) or dry air (ACC) entering TCC (kg/s).
        Mdot_moisture_TCC = PCONDR(con_CV1) * CONVAR(con_CV1,3) ! Mass flow of moisture (ACC) entering TCC (not used for HCC) (kg/s).

        
C---------------------------------------------------------------------------------
C Populate the DDS with the data that is to be sent to TRNSYS.
C---------------------------------------------------------------------------------
        IF (CouplingType == HCC) THEN
          COSIM_DATA%HCC_to_TRNSYS(iTCC%iHCCtoT(CouplingIndex))%
     &          Temperature = T_TCC
          COSIM_DATA%HCC_to_TRNSYS(iTCC%iHCCtoT(CouplingIndex))%
     &          Flowrate = Mdot_TCC
        ELSE IF (CouplingType == ACC) THEN
          COSIM_DATA%ACC_to_TRNSYS(iTCC%iACCtoT(CouplingIndex))%
     &          Temperature = T_TCC
          COSIM_DATA%ACC_to_TRNSYS(iTCC%iACCtoT(CouplingIndex))%
     &          Flowrate = Mdot_TCC
          COSIM_DATA%ACC_to_TRNSYS(iTCC%iACCtoT(CouplingIndex))%
     &          Moisture_flowrate = Mdot_moisture_TCC
        END IF

        
C---------------------------------------------------------------------------------
C Mark the single node's temperatures for iteration.
C Variables used:
C     ICSV(i,j)  flag indicating that node `i' is marked for iteration for state
C                variable `j'; j=1 for temperature, j=2 for 1st phase mass flow,
C                j=3 for 2nd phase mass flow.
C     CSVI(i,j)  initial value for judging whether iteration required. Same
C                indices as ICSV.
C     CSVF(i,j)  future time-row solution variable for plant. Same indices as ICSV.
C---------------------------------------------------------------------------------
        ICSV(CV1,1) = 1
        CSVI(CV1,1) = CSVF(CV1,1)

        
C---------------------------------------------------------------------------------
C Establish the energy balance equations.
C---------------------------------------------------------------------------------
        Balances_Sending: IF(ISTATS == 1) THEN
           COUT(1) = 1.
           COUT(2) = -1.
           COUT(3) = 0.

C---------------------------------------------------------------------------------
C Establish 1st phase mass balances.
C---------------------------------------------------------------------------------
        ELSE IF(ISTATS == 2) THEN
           COUT(1) = 1.
           COUT(2) = -1.
           COUT(3) = 0.

           
C---------------------------------------------------------------------------------
C Establish 2nd phase mass balances.
C---------------------------------------------------------------------------------
        ELSE IF(ISTATS==3) THEN
           COUT(1) = 1.
           COUT(2) = -1.
           COUT(3) = 0.

        ENDIF Balances_Sending
        
        
C---------------------------------------------------------------------------------
C End of sending TCC.
C---------------------------------------------------------------------------------

C---------------------------------------------------------------------------------
C Receiving TCC.  This component is receiving a fluid stream from TRNSYS.  The
C function of this instance of the TCC is to establish matrix coefficients such
C that the the plant component solves to the conditions returned by TRNSYS.
C---------------------------------------------------------------------------------
      ELSE
      
      
C---------------------------------------------------------------------------------
C Establish temperature and flow rate of water sent by TRNSYS to this TCC.
C---------------------------------------------------------------------------------
        IF (CouplingType == HCC) THEN
          T_TCC = COSIM_DATA%HCC_to_ESPr(iTCC%iHCCtoE(CouplingIndex))
     &          %Temperature    ! Temp of liquid water entering TCC (oC).
          Mdot_TCC = COSIM_DATA%HCC_to_ESPr(iTCC%iHCCtoE(CouplingIndex))
     &          %Flowrate       ! Mass flow of liquid water entering TCC (kg/s)
        ELSE IF (CouplingType == ACC) THEN
!          WRITE(*,*)'timestep = ',COSIM_DATA%ESPrTimestep,CouplingIndex
!          WRITE(*,*)'T_TCC = ',T_TCC
!          WRITE(*,*)'Mdot_TCC = ',Mdot_TCC

          T_TCC = COSIM_DATA%ACC_to_ESPr(iTCC%iACCtoE(CouplingIndex))
     &         %Temperature     ! Temp of air entering TCC (oC).
          Mdot_TCC = COSIM_DATA%ACC_to_ESPr(iTCC%iACCtoE(CouplingIndex))
     &         %Flowrate        ! Dry air mass flow of air entering TCC (kg/s)
          Mdot_moisture_TCC=COSIM_DATA%ACC_to_ESPr(iTCC%iACCtoE
     &         (CouplingIndex))%Moisture_flowrate  ! Moisture flow rate of air air entering TCC (kg/s).
        END IF

        
C---------------------------------------------------------------------------------
C Establish the energy balance equations.
C---------------------------------------------------------------------------------
        Balances_Receiving: IF(ISTATS == 1) THEN
           COUT(1) = 1.
           COUT(2) = 0.
           COUT(3) = T_TCC
           
           
C---------------------------------------------------------------------------------
C Establish 1st phase mass balances.
C---------------------------------------------------------------------------------
        ELSE IF(ISTATS == 2) THEN
           COUT(1) = 1.
           COUT(2) = 0.
           COUT(3) = Mdot_TCC
           
           
C---------------------------------------------------------------------------------
C Establish end phase mass balances.
C---------------------------------------------------------------------------------
        ELSE IF(ISTATS==3) THEN
          IF (CouplingType == HCC) THEN ! There is no 2nd phase for hydronic couplings.
             COUT(1) = 1.
             COUT(2) = 0.
             COUT(3) = 0.
          ELSE IF (CouplingType == ACC) THEN
             COUT(1) = 1.
             COUT(2) = 0.
             COUT(3) = Mdot_moisture_TCC
          END IF
          
        ENDIF Balances_Receiving

        
C---------------------------------------------------------------------------------
C End of receiving TCC.
C---------------------------------------------------------------------------------
      ENDIF Sending_Receiving

      
C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0)THEN
        WRITE(ITU,*) ' Component      ',IPCOMP,':'
        IF (CouplingType == HCC) THEN !HCC
            WRITE(ITU,*) ' 1-node HCC model '
        ELSE IF (CouplingType == ACC) THEN !ACC
            WRITE(ITU,*) ' 1-node ACC model '
        END IF
        WRITE(ITU,*) ' Matrix node(s) ',CV1
        WRITE(ITU,*) ' Connection(s)  ',con_CV1
        WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
        WRITE(ITU,*) (COUT(itemp),itemp=1,3)
        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
          itemp=(IPCOMP/4)*4
          IF(itemp.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
        END IF
        WRITE(ITU,*) ' Leaving subroutine TCC_coeff_gen '
      END IF


      RETURN
      END SUBROUTINE TCC_coeff_gen


C ***********************************************************************************
C This subroutine is used to pass data to, and receive data from TRNSYS.  The data
C are passed through the EsprTrnsysData DDS which contains an array of plant coupling
C components, electrical coupling components, zone air point temperatures, and zone
C heat injections. It also contains two flags which hold the state of convergence.
C The exchange of data occurs after each convergence of ESP-r's plant domain, which
C can occur multiple times per time-step.
C ***********************************************************************************
      INTEGER FUNCTION TCC_communication()
      USE DLL_Functions, ONLY:PassDataToTRNSYS,GetTRNSYSData,
     &                        GetSystemConv
      IMPLICIT NONE
#include "plant.h"
#include "building.h"


C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C plant iteration number
      COMMON/ITERINDEX/ITERNU
      INTEGER ITERNU

C Unit numbers for providing feedback to user during simulation.
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN


C Trace output.
      COMMON/TC/ITC,ICNT
      INTEGER ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE,IZNTRC,ITU
      INTEGER ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU


C Simulation time step data.
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      INTEGER IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Plant simulation time step data.
      common/pstctr/nsincp,nsncpr
      INTEGER nsincp,nsncpr


C Plant component pointer.
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Number of components in plant network and control actuation signals.
      COMMON/C9/NPCOMP,NCI,CDATA
      INTEGER NPCOMP,NCI(MPCOM)
      REAL CDATA(MPCOM,MMISCD)

C Plant component names
      common/pcnam/pcname(mpcom)
      character*15, pcname

C `Additional' plant component data.
      common/pcdat/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      REAL PCDATF   ! Future time-row.
      real PCDATP   ! Present time-row.

C Common containing zone casual gains
      COMMON/CasDat/Cas_Rad_dat(MCOM),Cas_Con_dat(MCOM),
     &     Cas_Lat_dat(MCOM)
      real Cas_Rad_dat, Cas_Con_dat, Cas_Lat_dat
C Common saving coupled zones/surfaces

C Number of Zones
      integer ncomp,ncon
      common/c1/ncomp,ncon

C Zone air temperatures
      real tpa,qpa,tfa,qfa
      common/pvala/tpa(mcom),qpa(mcom)  !Zone air temperatures and heat transfers (present)
      common/fvala/tfa(mcom),qfa(mcom)  !Zone air temperatures and heat transfers (future)
C Humidity ratio (kg moisture/kg dry air) in zones.
      COMMON/FVALG/GFA(MCOM)
      REAL :: GFA

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      integer i ! count variable


C---------------------------------------------------------------------------------
C Start trace if trace output requested.
C---------------------------------------------------------------------------------
      IF(ITC>0 .AND. NSINC>=ITC .AND.NSINC<=ITCF .AND.ITRACE(37)/=0)THEN
        WRITE(ITU,*) ' Entering subroutine TCC_communication'
      END IF


C---------------------------------------------------------------------------------
C Prepare data from ESP-r's building thermal domain to pass to TRNSYS.
C---------------------------------------------------------------------------------
      DO i=1, ncomp
        COSIM_DATA%ESPrZonesData(i)%AirPointTemperatures = TFA(i) ! Zone air-point temperature (oC).
        COSIM_DATA%ESPrZonesData(i)%AirPointHumidities   = GFA(i) ! Zone air-point humidity (kgw/kga).
      END DO
      

C---------------------------------------------------------------------------------
C Prepare data on total number of iterations of ESP-r's plant domain solver for
C the current time-step, as well as the total number of invocations of ESP-r's
C plant domain for the current time-step.
C---------------------------------------------------------------------------------
      if (COSIM_DATA%ESPrTimestep.EQ.nsincp) then  ! Same time-step as last invocation.
    	COSIM_DATA%TotalESPrIterations = COSIM_DATA%TotalESPrIterations
     &  	                             + ITERNU
    	COSIM_DATA%Invocations = COSIM_DATA%Invocations + 1
      else                                         ! New time-step.
    	COSIM_DATA%TotalESPrIterations = ITERNU
    	COSIM_DATA%Invocations = 1
      end if

      COSIM_DATA%ESPrTimestep = nsincp         ! Iteration number of plant domain solver.


C---------------------------------------------------------------------------------
C Before sending the data to the harmonizer save it (as the harmonizer only copies
C the bare minimum to save time, so all data sent to TRNSYS is lost on it's return
C to ESP-r)
C---------------------------------------------------------------------------------
	  COSIM_DATA_SAVED = COSIM_DATA


C---------------------------------------------------------------------------------
C Pass data to the Harmonizer, which will subsequently pass it onto TRNSYS.
C The data passed are set in the above code as well as that which was populated
C in prior calls to TCC_coeff_gen.
C---------------------------------------------------------------------------------
      call PassDataToTRNSYS(COSIM_DATA)


C---------------------------------------------------------------------------------
C Retrieve data from TRNSYS via the Harmonizer. These data will be used in
C future calls to TCC_coeff_gen. There will be a time lapse between the calls
C PassDataToTRNSYS and GetTRNSYSData during which TRNSYS will solve its network
C to convergence.
C---------------------------------------------------------------------------------
      call GetTRNSYSData(COSIM_DATA)

C---------------------------------------------------------------------------------
C Test the state of overall system convergence. Once the Harmonizer is satisfied
C that the data passed back and forth between ESP-r and TRNSYS have converged
C GetSystemConv will return a value of 1 for TCC_communication. Once this occurs
C then the simulation can march forward in time. Otherwise, this will signal that
C ESP-r's plant domain should be invoked again at the current time-step.
C---------------------------------------------------------------------------------
      call GetSystemConv(TCC_communication)


C---------------------------------------------------------------------------------
C Make all data passed back and forth between ESP-r and TRNSYS through the
C Harmonizer to H3K reports to make it available in .csv and .xml output.
C---------------------------------------------------------------------------------
	  call H3K_Report_TCC

	  
      END FUNCTION TCC_communication


      
C ***********************************************************************************
C This subroutine is make all data passed back and forth between ESP-r and TRNSYS
C through the Harmonizer to H3K reports to make it available in .csv and .xml output.
C ***********************************************************************************
      SUBROUTINE H3K_Report_TCC()
      use h3kmodule
      IMPLICIT NONE
C--------------------------------------------------------------------------------------------
C Common blocks.
C--------------------------------------------------------------------------------------------
C Number of Zones
      integer ncomp,ncon
      common/c1/ncomp,ncon

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
C.....Local Variables
      integer iNameLen              ! Length of a plant component name
      INTEGER :: i					!loop index
      INTEGER :: TCCindex			!Trnsys coupling component index
      CHARACTER*2 cTCCIndex, cZoneIndex
      REAL :: TotalEsprIterations, TotalTrnsysIterations
	  REAL :: Invocations

C---------------------------------------------------------------------------------
C Type variables to real.
C---------------------------------------------------------------------------------
      TotalEsprIterations   = COSIM_DATA_SAVED%TotalESPrIterations 
      TotalTrnsysIterations = COSIM_DATA%TotalTrnsysIterations
	  Invocations           = COSIM_DATA_SAVED%Invocations

	  
C----------------------------------------------------------------------------------
C Add variables to H3K Reports.
C----------------------------------------------------------------------------------

C Total number of ESP-r plant domain iterations for time-step.
      Call AddToReport(rvpltCosimEsprIter%Identifier,
     &      TotalEsprIterations)
C Total number of TRNSYS solver iterations for time-step.
      Call AddToReport(rvpltCosimTrnsysIter%Identifier,
     &      TotalTrnsysIterations)
C Number of invocations for time-step.
      Call AddToReport(rvpltCosimInvocations%Identifier,
     &      Invocations)

C Temperature and water flow for all HCC sending components.
      do i = 1, nHCC_S
	    TCCindex = iTCC%iHCCtoT(i)
C-----Prepare component name
        if ( TCCindex .ge. 10 ) write(cTCCindex, '(I2)') TCCindex
        if ( TCCindex .lt. 10 ) write(cTCCindex, '(A,I1)') '0',TCCindex
        Call AddToReport(rvpltHCCTempToTrnsys%Identifier,
     &      	COSIM_DATA_SAVED%HCC_to_TRNSYS(TCCindex)%Temperature,
     &      	cTCCIndex)
        Call AddToReport(rvpltHCCFlowToTrnsys%Identifier,
     &         COSIM_DATA_SAVED%HCC_to_TRNSYS(TCCindex)%Flowrate,
     &			cTCCIndex)
      end do

C Temperature and water flow for all HCC receiving components.
      do i = 1, nHCC_R
	    TCCindex = iTCC%iHCCtoE(i)
C-----Prepare component name
        if ( TCCindex .ge. 10 ) write(cTCCindex, '(I2)') TCCindex
        if ( TCCindex .lt. 10 ) write(cTCCindex, '(A,I1)') '0',TCCindex
        Call AddToReport(rvpltHCCTempToEspr%Identifier,
     &       COSIM_DATA%HCC_to_ESPr(TCCindex)%Temperature,cTCCIndex)
        Call AddToReport(rvpltHCCFlowToEspr%Identifier,
     &       COSIM_DATA%HCC_to_ESPr(TCCindex)%Flowrate,cTCCIndex)
      end do

C Temperature, air flow, and moisture flow for all ACC sending components.
      do i = 1, nACC_S
	    TCCindex = iTCC%iACCtoT(i)
C-----Prepare component name
        if ( TCCindex .ge. 10 ) write(cTCCindex, '(I2)') TCCindex
        if ( TCCindex .lt. 10 ) write(cTCCindex, '(A,I1)') '0',TCCindex
        Call AddToReport(rvpltACCTempToTrnsys%Identifier,
     &       	COSIM_DATA_SAVED%ACC_to_TRNSYS(TCCindex)%Temperature,
     &			cTCCIndex)
        Call AddToReport(rvpltACCFlowToTrnsys%Identifier,
     &       	COSIM_DATA_SAVED%ACC_to_TRNSYS(TCCindex)%Flowrate,
     &			cTCCIndex)
        Call AddToReport(rvpltACCMoistFlowToTrnsys%Identifier,
     &       COSIM_DATA_SAVED%ACC_to_TRNSYS(TCCindex)%Moisture_flowrate,
     &		 cTCCIndex)
      end do

C Temperature, air flow, and moisture flow for all ACC receiving components.
      do i = 1, nACC_S
	    TCCindex = iTCC%iACCtoE(i)
C-----Prepare component name
        if ( TCCindex .ge. 10 ) write(cTCCindex, '(I2)') TCCindex
        if ( TCCindex .lt. 10 ) write(cTCCindex, '(A,I1)') '0',TCCindex
        Call AddToReport(rvpltACCTempToEspr%Identifier,
     &       	COSIM_DATA%ACC_to_ESPr(TCCindex)%Temperature,cTCCIndex)
        Call AddToReport(rvpltACCFlowToEspr%Identifier,
     &       	COSIM_DATA%ACC_to_ESPr(TCCindex)%Flowrate,cTCCIndex)
        Call AddToReport(rvpltACCMoistFlowToEspr%Identifier,
     &       	COSIM_DATA%ACC_to_ESPr(TCCindex)%Moisture_flowrate,
     &		 	cTCCIndex)
      end do
      
C  Air point temperatures and humidities sent to TRNSYS and casual gains received from TRNSYS.
      DO i=1, ncomp
C-----Prepare component name
        if ( i .ge. 10 ) write(cZoneIndex, '(I2)') i
        if ( i .lt. 10 ) write(cZoneIndex, '(A,I1)') '0',i
        Call AddToReport(rvpltCosimAirPointTemperatures%Identifier,
     &     COSIM_DATA%ESPrZonesData(i)%AirPointTemperatures,cZoneIndex)
        Call AddToReport(rvpltCosimAirPointHumidities%Identifier,
     &     COSIM_DATA%ESPrZonesData(i)%AirPointHumidities,cZoneIndex)
        Call AddToReport(rvpltCosimAirPointCasualGains%Identifier,
     &     COSIM_DATA%ESPrZonesData(i)%AirPointCasualGains,cZoneIndex)
      END DO


      END SUBROUTINE H3K_Report_TCC

      END MODULE TCC
