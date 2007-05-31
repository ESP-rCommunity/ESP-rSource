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

C ************************************************************************************
C At the end of an electrical network solution this routine is responsible for
C results post processing and preparing the newwork for the next-timestep solution. 
C The routines are as follows:

C ENETPOST  - The electrical network post-pocessing routine.
C ENETPU2SI - This routine converts the calculated p.u. voltages to SI equivalents. 
C ENETDVAR  - This routine calculates derived variables (losses, power import/export
C             etc.) in the network.
C ENETLIBSV - This routine saves the electrical network details to the electrical network
C             results library.
C ************************************************************************************
C This routine controlls the post processing of the raw results from the electrical
C network simulation.

      SUBROUTINE ENETPOST
C  named constants
      INCLUDE 'CETC_definitions.h'      
C Dummy variable
      real fDummy

C Using the PU voltages calculate the other network parameters such as
C flows in components, losses, grid import, currents etc.
      CALL ENETDVAR

C Call the routine to convert the network voltages back from per-unit to 
C SI units. 
      CALL ENETPU2SI

C Store electrical data for plant components.  This call prevents the 
C electrical network boundry contitions from getting overwritten upon each 
C successive iteration of the plant domain, and ensures that control of the plant 
C domain in response to the electrical network will work as expected.
      fDummy = elec_net_load_calc(store_data)

      RETURN 
      END
C ************************************************************************************
C Subroutine ENETDVAR derives various network parameters from the basic results
C output (nodal voltages). Note all calculations are in PER-UNIT. 
C Variables introduced in this routine are:
C
C PENOD[L,G,T]PU - The nodal load,generated and transmitted real powers resp.
C QENOD[L,G,T]PU - The nodal load,generated and transmitted reactive powers resp.
C PENODIMPU      - The nodal imported real power.
C QENODIMPU      - The nodal imported reactive power.
C ENODPFPU       - The nodal power factor (for boundary power flows).
C PECONTRNPU     - The real component of transmitted current in each phase.
C PECONTRNPU     - The reactive component of transmitted current in each phase. 
C ECONLOSPU      - The loss in each phase of the connection. 
C ECONPFPU       - The power factor in each phase of the connection. 
C CONTRNIPU      - The transmitted current in each phase.
C ************************************************************************************
      SUBROUTINE ENETDVAR

      INCLUDE 'power.h'
      INCLUDE 'building.h'

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP 

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The array of driving port and transfer admittances (per-unit)
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

C The PER-UNIT electrical network state variables.
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

C The per-unit nodal boundary conditions
      COMMON/ENODBCPU/PENODPU(MENOD),QENODPU(MENOD)

C The load, generated and transmitted nodal power flows (per-unit)
      COMMON/ENODLGTPU/PENODLPU(MENOD),QENODLPU(MENOD),
     &PENODGPU(MENOD),QENODGPU(MENOD),PENODTPU(MENOD),QENODTPU(MENOD)

C The derived nodal variables (per-unit)
      COMMON/ENODDERPU/PENODIMPU(MENOD),QENODIMPU(MENOD),
     &ENODPFPU(MENOD),ENODLPFPU(MENOD),ENODGPFPU(MENOD),ENODIPFPU(MENOD)

C The derived connection variables (per-unit)
      COMMON/ECONDERPU/PECONTRNPU(MECON,MPHAS),QECONTRNPU(MECON,MPHAS),
     &ECONLOSPU(MECON,MPHAS),ECONPFPU(MECON,MPHAS),
     &CONTRNIPU(MECON,MPHAS)

C The derived network variables (per-unit).
      COMMON/ENETDERPU/PENETIMPPU,QENETIMPPU,PENETLODPU,QENETLODPU,
     &PENETGENPU,QENETGENPU,PENETRNPU,QENETRNPU,ENETIPFPU,ENETGPFPU,
     &ENETLPFPU,ENETLOSPU,PENETPHLPU(MPHAS),QENETPHGPU(MPHAS),
     &PENETPHGPU(MPHAS),QENETPHLPU(MPHAS),PENETPHIPU(MPHAS),
     &QENETPHIPU(MPHAS)

C Type casting (all varaiables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NECON,ECONO,
     &CONPHTYP,CONPH,CCNO,SENOD,EENOD,CSNOD,CSNODM,CENODM
     
C Type casting
      REAL ENODBASEV,ENODBASEP
      
      REAL PENODPU,QENODPU,PENODLPU,QENODLPU,PENODGPU,QENODGPU,PENODTPU,
     &QENODTPU,PENODIMPU,QENODIMPU,ENODPFPU,ENODLPFPU,ENODGPFPU,
     &ENODIPFPU,PECONTRNPU,QECONTRNPU,ECONLOSPU,ECONPFPU

      REAL PENETIMPPU,QENETIMPPU,PENETLODPU,QENETLODPU,
     &PENETGENPU,QENETGENPU,PENETRNPU,QENETRNPU,ENETIPFPU,ENETGPFPU,
     &ENETLPFPU,ENETLOSPU,PENETPHLPU,QENETPHLPU,PENETPHIPU,QENETPHIPU,
     &PENETPHGPU,QENETPHGPU
     
      COMPLEX YADM,VOLTPUP,VOLTPUF,SUMVA,SUMYDV,CONTRNIPU
      
C Set all network variables to 0.0
        PENETIMPPU=0.0
        QENETIMPPU=0.0
        PENETGENPU=0.0
        QENETGENPU=0.0
        PENETLODPU=0.0
        QENETLODPU=0.0
        PENETRNPU=0.0
        QENETRNPU=0.0   
        ENETIPFPU=0.0
        ENETGPFPU=0.0
        ENETLPFPU=0.0
        ENETLOSPU=0.0
      DO 3 IPH=1,MPHAS
        PENETPHLPU(IPH)=0.0
        QENETPHLPU(IPH)=0.0
        PENETPHGPU(IPH)=0.0
        QENETPHGPU(IPH)=0.0
        PENETPHIPU(IPH)=0.0
        QENETPHIPU(IPH)=0.0
   3  CONTINUE

C Nodal variables: 
      DO 10 INOD=1,NENOD

C Firstly set all derived nodal variables to 0.0
        PENODIMPU(INOD)=0.0
        QENODIMPU(INOD)=0.0
        PENODTPU(INOD)=0.0
        QENODTPU(INOD)=0.0
        ENODPFPU(INOD)=0.0

C Calculate nodal transmitted power flows for all nodes and also       
C calculate the imported power for fixed V nodes =Vi*SUM(Yij*Vj)'; this
C exactly balances the transmitted power associated with the fixed node. 

C Calculate the net transmitted power at the node.
        SUMVA=CMPLX(0.0,0.0)
        DO 21 ICNOD=1,NENOD
          SUMVA=SUMVA+
     &(VOLTPUP(INOD)*CONJG(VOLTPUP(ICNOD)*YADM(INOD,ICNOD)))
  21    CONTINUE
        IF( ENODTYP(INOD).GE.2 .and. ENODTYP(INOD).LE.4 )THEN
          PENODIMPU(INOD)=-PENODPU(INOD)+REAL(SUMVA)
          QENODIMPU(INOD)=-QENODPU(INOD)+AIMAG(SUMVA)
          PENODPU(INOD)=PENODPU(INOD)+PENODIMPU(INOD)
          QENODPU(INOD)=QENODPU(INOD)+QENODIMPU(INOD)
          PENODTPU(INOD)=REAL(SUMVA)
          QENODTPU(INOD)=AIMAG(SUMVA)
        ELSE
          PENODIMPU(INOD)=0.0
          QENODIMPU(INOD)=0.0
          PENODTPU(INOD)=REAL(SUMVA)
          QENODTPU(INOD)=AIMAG(SUMVA)
        ENDIF

C Calculate nodal power factors
        IF(ABS(QENODPU(INOD)).GT.0..OR.ABS(PENODPU(INOD)).GT.0.)THEN
          ENODPFPU(INOD)=
     &ABS(PENODPU(INOD))/SQRT((QENODPU(INOD)*QENODPU(INOD))+
     &(PENODPU(INOD)*PENODPU(INOD)))
        ELSE
          ENODPFPU(INOD)=0.0
        ENDIF

        IF(ABS(QENODLPU(INOD)).GT.0..OR.ABS(PENODLPU(INOD)).GT.0.)THEN
          ENODLPFPU(INOD)=
     &ABS(PENODLPU(INOD))/SQRT((QENODLPU(INOD)*QENODLPU(INOD))+
     &(PENODLPU(INOD)*PENODLPU(INOD)))
        ELSE
          ENODLPFPU(INOD)=0.0
        ENDIF

        IF(ABS(QENODGPU(INOD)).GT.0..OR.ABS(PENODGPU(INOD)).GT.0.)THEN
          ENODGPFPU(INOD)=
     &ABS(PENODGPU(INOD))/SQRT((QENODGPU(INOD)*QENODGPU(INOD))+
     &(PENODGPU(INOD)*PENODGPU(INOD)))
        ELSE
          ENODGPFPU(INOD)=0.0
        ENDIF

        IF(ABS(QENODIMPU(INOD)).GT.0..OR.ABS(PENODIMPU(INOD)).GT.0.)THEN
          ENODIPFPU(INOD)=
     &ABS(PENODIMPU(INOD))/SQRT((QENODIMPU(INOD)*QENODIMPU(INOD))+
     &(PENODIMPU(INOD)*PENODIMPU(INOD)))
        ELSE
          ENODIPFPU(INOD)=0.0
        ENDIF


C Calculate the node-based network paramters 
C Total imported power 
      PENETIMPPU=PENETIMPPU+PENODIMPU(INOD)
      QENETIMPPU=QENETIMPPU+QENODIMPU(INOD)

C Total generated power
      PENETGENPU=PENETGENPU+PENODGPU(INOD)
      QENETGENPU=QENETGENPU+QENODGPU(INOD)

C Total demand
      PENETLODPU=PENETLODPU+PENODLPU(INOD)
      QENETLODPU=QENETLODPU+QENODLPU(INOD)

C Total transmitted
      PENETRNPU=PENETRNPU+ABS(PENODTPU(INOD))
      QENETRNPU=QENETRNPU+ABS(QENODTPU(INOD))

C Phase loadings
      PENETPHLPU(ENODPH(INOD))=PENETPHLPU(ENODPH(INOD))+PENODLPU(INOD)
      QENETPHLPU(ENODPH(INOD))=QENETPHLPU(ENODPH(INOD))+QENODLPU(INOD)

C Phase generation
      PENETPHGPU(ENODPH(INOD))=PENETPHGPU(ENODPH(INOD))+PENODGPU(INOD)
      QENETPHGPU(ENODPH(INOD))=QENETPHGPU(ENODPH(INOD))+QENODGPU(INOD)

C Phase import
      PENETPHIPU(ENODPH(INOD))=PENETPHIPU(ENODPH(INOD))+PENODIMPU(INOD)
      QENETPHIPU(ENODPH(INOD))=QENETPHIPU(ENODPH(INOD))+QENODIMPU(INOD)

  10  CONTINUE

C Calculate the network power factors
C Imported pf
      IF(ABS(PENETIMPPU).GT.0..OR.ABS(QENETIMPPU).GT.0.)THEN
        ENETIPFPU=PENETIMPPU/SQRT((PENETIMPPU*PENETIMPPU)+
     &(QENETIMPPU*QENETIMPPU))
      ELSE
        ENETIPFPU=0.0
      ENDIF

C Generation pf
      IF(ABS(PENETGENPU).GT.0..OR.ABS(QENETGENPU).GT.0.)THEN
        ENETGPFPU=PENETGENPU/SQRT((PENETGENPU*PENETGENPU)+
     &(QENETGENPU*QENETGENPU))
      ELSE
        ENETGPFPU=0.0
      ENDIF

C Demand pf
      IF(ABS(PENETLODPU).GT.0..OR.ABS(QENETLODPU).GT.0.)THEN
        ENETLPFPU=PENETLODPU/SQRT((PENETLODPU*PENETLODPU)+
     &(QENETLODPU*QENETLODPU))
      ELSE
        ENETLPFPU=0.0
      ENDIF

C Connection and connecting component variables:
      DO 20 ICON=1,NECON

C Set all connection variables to 0.0
        DO 23 IPH=1,MPHAS
          CONTRNIPU(ICON,IPH)=CMPLX(0.0,0.0)
          PECONTRNPU(ICON,IPH)=0.0
          QECONTRNPU(ICON,IPH)=0.0
          ECONLOSPU(ICON,IPH)=0.0
          ECONPFPU(ICON,IPH)=0.0
  23    CONTINUE

C Calculate connection power flows including power flows due to mutual
C couplings. 
        DO 25 IPH=1,MPHAS
          SUMYDV=CMPLX(0.0,0.0)
          CSNOD=SENOD(ICON,IPH)
          IF(CSNOD.GT.0)THEN

C Get mutually coupled start and end nodes.
            DO 27 IPHM=1,MPHAS
              CSNODM=SENOD(ICON,IPHM)
              CENODM=EENOD(ICON,IPHM)
              IF(CONPH(ICON,IPHM).GT.0.AND.CSNOD.NE.0.AND.
     &CSNODM.NE.0)THEN

C Calculate the power flow for this connection
                IF(CABS(YADM(CSNOD,CENODM)).GT.0.0.AND.CENODM.NE.0)THEN

C Calculate the current flow to/from the node.
                  SUMYDV=SUMYDV+
     &(-YADM(CSNOD,CENODM)*(VOLTPUF(CSNODM)-VOLTPUF(CENODM)))
                ENDIF 
              ENDIF
  27        CONTINUE
C Calculate the current in the connection
            CONTRNIPU(ICON,IPH)=SUMYDV

C Calculate the power flow in the connection.
            PECONTRNPU(ICON,IPH)=
     &REAL(VOLTPUF(CSNOD)*CONJG(SUMYDV))
            QECONTRNPU(ICON,IPH)=
     &AIMAG(VOLTPUF(CSNOD)*CONJG(SUMYDV))

C Calculate connection real power losses (I2*R)
            ECONLOSPU(ICON,IPH)=
     &REAL(CONTRNIPU(ICON,IPH)*CONJG(CONTRNIPU(ICON,IPH)))/
     &REAL(YADM(CSNOD,CSNOD))

C Add the connection loss the total system loss.
            ENETLOSPU=ENETLOSPU+ECONLOSPU(ICON,IPH)

C Calculate connection power factors
            IF(ABS(PECONTRNPU(ICON,IPH)).GT.0..OR.
     &ABS(QECONTRNPU(ICON,IPH)).GT.0.)THEN
              ECONPFPU(ICON,IPH)=ABS(PECONTRNPU(ICON,IPH))/
     &SQRT((PECONTRNPU(ICON,IPH)*PECONTRNPU(ICON,IPH))+
     &(QECONTRNPU(ICON,IPH)*QECONTRNPU(ICON,IPH)))  
            ELSE
              ECONPFPU(ICON,IPH)=0.0
            ENDIF
          ENDIF
  25    CONTINUE  
  20  CONTINUE

C Trace
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN 
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Subroutine ENETDVAR '
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Derived (PER UNIT) Nodal Parameters: '
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Transmitted power and overall pf:'
        WRITE(ITU,*)
     &' Node  |    Real Power   |  Reactive Power |     pf     '
        WRITE(ITU,*)
     &'       |      (pu)       |      (pu)       |            ' 
        DO 30 INOD=1,NENOD
          WRITE(ITU,'(1X,I3,4X,2(F17.6,1X),F12.6)') INOD,PENODTPU(INOD),
     &QENODTPU(INOD),ENODPFPU(INOD)
  30    CONTINUE 
        WRITE(ITU,*)'  '  
        WRITE(ITU,*)' Derived (PER UNIT) Connection Parameters: '
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Connection: '   

        DO 40 ICON=1,NECON
          DO 41 IPHC=1,MPHAS
          IF(CONPH(ICON,IPHC).GT.0)THEN
              IPH=CONPH(ICON,IPHC)
              WRITE(ITU,*)' Node  ',SENOD(ICON,IPH),' to ','Node  ',
     &EENOD(ICON,IPH)
              WRITE(ITU,*)' Real Power     | Reactive Power |   pf    '
              WRITE(ITU,*)'    (pu)        |     (pu)       |         '
              WRITE(ITU,'(2(F16.5,1X),F11.5)')PECONTRNPU(ICON,IPH),
     &QECONTRNPU(ICON,IPH),ECONPFPU(ICON,IPH)
              WRITE(ITU,*)'          Current (pu)        '
              WRITE(ITU,'(2(F16.5,1X),F11.5)')CONTRNIPU(ICON,IPH)
              WRITE(ITU,*)'          loss (pu)        '
              WRITE(ITU,*)ECONLOSPU(ICON,IPH)
            ENDIF
  41      CONTINUE
  40    CONTINUE
        WRITE(ITU,*)'  '
        WRITE(ITU,*)' Derived (PER UNIT) Network Parameters: '
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Real phase loadings (pu): ' 
        WRITE(ITU,*)' Phase - 1   |   Phase - 2   |   Phase - 3   '
        WRITE(ITU,'(3(F11.6,4X))')(PENETPHLPU(J),J=1,MPHAS)
        WRITE(ITU,*)' Reactive phase loadings (pu): ' 
        WRITE(ITU,*)' Phase - 1   |   Phase - 2   |   Phase - 3   '
        WRITE(ITU,'(3(F11.6,4X))')(QENETPHLPU(J),J=1,MPHAS)
        WRITE(ITU,*)' Real Power Import (pu): '
        WRITE(ITU,*)' Real (pu)   |   Reactive (pu)  ' 
        WRITE(ITU,'(2(F11.6,4X))')PENETIMPPU,QENETIMPPU
        WRITE(ITU,*)' Total Loss (pu): ',ENETLOSPU
      ENDIF

      RETURN
      END
C  ************************************************************************************
C Subroutine ENETPU2SI converts the calculated per-unit network values to their SI 
C equivalents. 
C Variables introduced in this routine are: 
C
C ENODVSI - the solved nodal voltage (SI values). 

      SUBROUTINE ENETPU2SI

      INCLUDE 'power.h'
      INCLUDE 'building.h'

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C Derived System and Nodal Values
      COMMON/ENETSMC/LNPWRBAS,ENODZBAS(MENOD),ENODIBAS(MENOD)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The PER-UNIT electrical network state variables
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

C The electrical network solved voltages (SI)
      COMMON/ENODSLV/ENODVLT(MENOD)


C The load, generated and transmitted nodal power flows (per-unit)
      COMMON/ENODLGTPU/PENODLPU(MENOD),QENODLPU(MENOD),
     &PENODGPU(MENOD),QENODGPU(MENOD),PENODTPU(MENOD),QENODTPU(MENOD)

C The load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGT/PENODL(MENOD),QENODL(MENOD),PENODG(MENOD),
     &QENODG(MENOD),PENODT(MENOD),QENODT(MENOD)

C The derived nodal variables (per-unit)
      COMMON/ENODDERPU/PENODIMPU(MENOD),QENODIMPU(MENOD),
     &ENODPFPU(MENOD),ENODLPFPU(MENOD),ENODGPFPU(MENOD),ENODIPFPU(MENOD)

C The derived nodal variables (SI)
      COMMON/ENODDER/PENODIM(MENOD),QENODIM(MENOD),ENODPF(MENOD),
     &ENODLPF(MENOD),ENODGPF(MENOD),ENODIPF(MENOD)

C The derived connection variables (per-unit)
      COMMON/ECONDERPU/PECONTRNPU(MECON,MPHAS),QECONTRNPU(MECON,MPHAS),
     &ECONLOSPU(MECON,MPHAS),ECONPFPU(MECON,MPHAS),
     &CONTRNIPU(MECON,MPHAS)

C The derived connection variables (SI)
      COMMON/ECONDER/PECONTRN(MECON,MPHAS),QECONTRN(MECON,MPHAS),
     &ECONLOS(MECON,MPHAS),ECONPF(MECON,MPHAS),CONTRNI(MECON,MPHAS)

C The derived network variables (per-unit).
      COMMON/ENETDERPU/PENETIMPPU,QENETIMPPU,PENETLODPU,QENETLODPU,
     &PENETGENPU,QENETGENPU,PENETRNPU,QENETRNPU,ENETIPFPU,ENETGPFPU,
     &ENETLPFPU,ENETLOSPU,PENETPHLPU(MPHAS),QENETPHGPU(MPHAS),
     &PENETPHGPU(MPHAS),QENETPHLPU(MPHAS),PENETPHIPU(MPHAS),
     &QENETPHIPU(MPHAS)

C The derived network variables (SI).
      COMMON/ENETDER/PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,
     &QENETGEN,PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,
     &PENETPHL(MPHAS),QENETPHL(MPHAS),PENETPHG(MPHAS),
     &QENETPHG(MPHAS),PENETPHI(MPHAS),QENETPHI(MPHAS)

      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NECON,ECONO,
     &CONPHTYP,CONPH,CCNO,SENOD,EENOD

      REAL ENODBASEV,ENODBASEP,ENODZBAS,LNPWRBAS,ENODIBAS

      REAL PENODLPU,QENODLPU,PENODGPU,QENODGPU,PENODTPU,QENODTPU,
     &PENODIMPU,QENODIMPU,ENODPFPU,ENODLPFPU,ENODGPFPU,ENODIPFPU,
     &PECONTRNPU,QECONTRNPU,ECONLOSPU,ECONPFPU

      REAL PENETIMPPU,QENETIMPPU,PENETLODPU,QENETLODPU,
     &PENETGENPU,QENETGENPU,PENETRNPU,QENETRNPU,ENETIPFPU,ENETGPFPU,
     &ENETLPFPU,ENETLOSPU,PENETPHLPU,QENETPHLPU,PENETPHGPU,QENETPHGPU,
     &PENETPHIPU,QENETPHIPU

      REAL PENODL,QENODL,PENODG,QENODG,PENODT,QENODT,PENODIM,QENODIM,
     &ENODPF,ENODLPF,ENODGPF,ENODIPF,PECONTRN,QECONTRN,ECONLOS,ECONPF

      REAL PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,QENETGEN,
     &PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,PENETPHL,QENETPHL,
     &PENETPHG,QENETPHG,PENETPHI,QENETPHI

      COMPLEX VOLTPUP,VOLTPUF,ENODVLT,CONTRNI,CONTRNIPU

C Trace
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine ENETPU2SI'
      ENDIF

      DO 10 INOD=1,NENOD

C Calculate the SI nodal variables: 
        ENODVLT(INOD)=VOLTPUF(INOD)*ENODBASEV(INOD)
        PENODIM(INOD)=PENODIMPU(INOD)*LNPWRBAS    
        QENODIM(INOD)=QENODIMPU(INOD)*LNPWRBAS           
        ENODPF(INOD)=ENODPFPU(INOD)   
        PENODL(INOD)=PENODLPU(INOD)*LNPWRBAS    
        QENODL(INOD)=QENODLPU(INOD)*LNPWRBAS    
        PENODG(INOD)=PENODGPU(INOD)*LNPWRBAS    
        QENODG(INOD)=QENODGPU(INOD)*LNPWRBAS    
        PENODT(INOD)=PENODTPU(INOD)*LNPWRBAS    
        QENODT(INOD)=QENODTPU(INOD)*LNPWRBAS   
        ENODLPF(INOD)=ENODLPFPU(INOD)  
        ENODGPF(INOD)=ENODGPFPU(INOD) 
        ENODIPF(INOD)=ENODIPFPU(INOD)  

C Trace
        IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
          WRITE(ITU,*)' '
          WRITE(ITU,*)' Calculated SI nodal parameters: '
          WRITE(ITU,*)' Node ',INOD,' Base Voltage: ',ENODBASEV(INOD)
          WRITE(ITU,*)' Voltage       :',ENODVLT(INOD)

          WRITE(ITU,*)' Imported kW   | Imported kVA  '
          WRITE(ITU,'(2(F11.6,4X))')
     &PENODIM(INOD)/1000.,QENODIM(INOD)/1000.
          WRITE(ITU,*)' Load kW       | Load kVA       '
          WRITE(ITU,'(2(F11.6,4X))')
     &PENODL(INOD)/1000.,QENODL(INOD)/1000.
          WRITE(ITU,*)' Generated kW  |  Generated kVA '
          WRITE(ITU,'(2(F11.6,4X))') 
     &PENODG(INOD)/1000.,QENODG(INOD)/1000.
          WRITE(ITU,*)' Transmit  kW  | Transmit  kVA  '
          WRITE(ITU,'(2(F11.6,4X))')
     &PENODT(INOD)/1000.,QENODT(INOD)/1000.
        ENDIF
  10  CONTINUE

      DO 20 ICON=1,NECON
        DO 30 IPH=1,MPHAS

C Calculate the SI connection variables:
          PECONTRN(ICON,IPH)=PECONTRNPU(ICON,IPH)*LNPWRBAS
          QECONTRN(ICON,IPH)=QECONTRNPU(ICON,IPH)*LNPWRBAS
          ECONLOS(ICON,IPH)=ECONLOSPU(ICON,IPH)*LNPWRBAS
          ECONPF(ICON,IPH)=ECONPFPU(ICON,IPH)   
          CONTRNI(ICON,IPH)=CONTRNIPU(ICON,IPH)*
     &ENODIBAS(SENOD(ICON,IPH))  
  30    CONTINUE

C Trace
        IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
          WRITE(ITU,*)' '
          WRITE(ITU,*)' Calculated SI connection parameters: '
          WRITE(ITU,*)' Connection ',ICON
          WRITE(ITU,*)' Start Node(s) :',(SENOD(ICON,J),J=1,MPHAS)
          WRITE(ITU,*)' End Node(s)   :',(EENOD(ICON,J),J=1,MPHAS)
          WRITE(ITU,*)' Transmit kW     |    Transmit kVA  '
          WRITE(ITU,*)'  1  |  2  |  3  |  1  |  2  |  3  |'
          WRITE(ITU,*) 
     &(PECONTRN(ICON,J)/1000.,J=1,MPHAS),
     &(QECONTRN(ICON,K)/1000.,K=1,MPHAS)
          WRITE(ITU,*)' Loss W        '
          WRITE(ITU,*)'  1  |  2  |  3'
          WRITE(ITU,*) (-ECONLOS(ICON,J),J=1,MPHAS)
          WRITE(ITU,*)' Power fact.   '
          WRITE(ITU,*)'  1  |  2  |  3'
          WRITE(ITU,*) (ECONPF(ICON,J),J=1,MPHAS)
          WRITE(ITU,*)'     Current   '
          WRITE(ITU,*)'  1  |  2  |  3'
          WRITE(ITU,*)(CONTRNI(ICON,J),J=1,MPHAS)
        ENDIF
  20  CONTINUE

C Network variables      
      PENETIMP=PENETIMPPU*LNPWRBAS
      QENETIMP=QENETIMPPU*LNPWRBAS
      PENETLOD=PENETLODPU*LNPWRBAS
      QENETLOD=QENETLODPU*LNPWRBAS
      PENETGEN=PENETGENPU*LNPWRBAS
      QENETGEN=QENETGENPU*LNPWRBAS
      PENETRN=PENETRNPU*LNPWRBAS
      QENETRN=QENETRNPU*LNPWRBAS
      ENETIPF=ENETIPFPU
      ENETGPF=ENETGPFPU
      ENETLPF=ENETLPFPU
      ENETLOS=ENETLOSPU*LNPWRBAS
      DO 40 IPH=1,MPHAS
        PENETPHL(IPH)=PENETPHLPU(IPH)*LNPWRBAS
        QENETPHL(IPH)=QENETPHLPU(IPH)*LNPWRBAS
        PENETPHG(IPH)=PENETPHGPU(IPH)*LNPWRBAS
        QENETPHG(IPH)=QENETPHGPU(IPH)*LNPWRBAS
        PENETPHI(IPH)=PENETPHIPU(IPH)*LNPWRBAS
        QENETPHI(IPH)=QENETPHIPU(IPH)*LNPWRBAS
  40  CONTINUE

C Trace
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Calculated SI network parameters: '
        WRITE(ITU,*)' Imported (kW)   :',PENETIMP/1000.
        WRITE(ITU,*)' Imported (kVA)  :',QENETIMP/1000.
        WRITE(ITU,*)' Net load (kW)   :',PENETLOD/1000.
        WRITE(ITU,*)' Net load (kVA)  :',QENETLOD/1000.
        WRITE(ITU,*)' Net genr. (kW)  :',PENETGEN/1000.
        WRITE(ITU,*)' Net genr. (kVA) :',QENETGEN/1000.
        WRITE(ITU,*)' Net tran. (kW)  :',PENETRN/1000.
        WRITE(ITU,*)' Net tran. (kVA) :',QENETRN/1000.
        WRITE(ITU,*)' Loss (W)        :',-ENETLOS
        WRITE(ITU,*)' Import pf       :',ENETIPF
        WRITE(ITU,*)' Generation pf   :',ENETGPF
        WRITE(ITU,*)' Load pf         :',ENETLPF
        WRITE(ITU,*)' Phase loading (kW) :',
     &(PENETPHL(J)/1000.,J=1,MPHAS)
        WRITE(ITU,*)' Phase loading (kVA):',
     &(QENETPHL(J)/1000.,J=1,MPHAS)
        WRITE(ITU,*)' Phase generation (kW) :',
     &(PENETPHG(J)/1000.,J=1,MPHAS)
        WRITE(ITU,*)' Phase generation (kVA):',
     &(QENETPHG(J)/1000.,J=1,MPHAS)
        WRITE(ITU,*)' Phase import (kW) :',
     &(PENETPHI(J)/1000.,J=1,MPHAS)
        WRITE(ITU,*)' Phase import (kVA):',
     &(QENETPHI(J)/1000.,J=1,MPHAS)
      ENDIF

      RETURN
      END
C  ************************************************************************************
C Subroutine ENETLIBSV saves the electrical network results to the electrical network
C results library, opened on IFIL+MADFIL. The routine can work in averaging and non-
C averaging mode. After the electrical network results are written to the library all
C calculated network variables are shited to previous-time-step arrays (the variable
C name is the same but with a 'P' appended. Ditto for the averaged values - in this
C case and 'A' is appended.

      SUBROUTINE ENETLIBSV

      INCLUDE 'power.h'
      INCLUDE 'building.h'
      INCLUDE 'plant.h'

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SUS/ISSNY

      COMMON/C6/INDCFG

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTSTP/NTSTPP

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C The electrical network solved voltages (SI)
      COMMON/ENODSLV/ENODVLT(MENOD)

C The average electrical network solved voltages (SI)
      COMMON/ENODSLVA/ENODVLTA(MENOD)

C The previous electrical network solved voltages (SI)
      COMMON/ENODSLVP/ENODVLTP(MENOD)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C Zone lighting and power commons, alter the second parameter if the
C number of casual gain types changes!
      COMMON/ENETZONE/PEZON(MCOM,MGTY),QEZON(MCOM,MGTY)

C Plant
      COMMON/ENETPCOMP/PPCMP(MPCOM),QPCMP(MPCOM),
     &VCMP(MPCOM)

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)

C Power only components
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

C Power only components - averaged values
      COMMON/ENETPOWCA/PPOWOCA(MPOWCOM),QPOWOCA(MPOWCOM),
     &VPOWCA(MPOWCOM)

C Power only components - previous values
      COMMON/ENETPOWCP/PPOWOCP(MPOWCOM),QPOWOCP(MPOWCOM),
     &VPOWCP(MPOWCOM)

C The load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGT/PENODL(MENOD),QENODL(MENOD),PENODG(MENOD),
     &QENODG(MENOD),PENODT(MENOD),QENODT(MENOD)

C The averaged load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGTA/PENODLA(MENOD),QENODLA(MENOD),PENODGA(MENOD),
     &QENODGA(MENOD),PENODTA(MENOD),QENODTA(MENOD)

C The previous load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGTP/PENODLP(MENOD),QENODLP(MENOD),PENODGP(MENOD),
     &QENODGP(MENOD),PENODTP(MENOD),QENODTP(MENOD)

C The derived nodal variables (SI)
      COMMON/ENODDER/PENODIM(MENOD),QENODIM(MENOD),ENODPF(MENOD),
     &ENODLPF(MENOD),ENODGPF(MENOD),ENODIPF(MENOD)

C The average derived nodal variables (SI)
      COMMON/ENODDERA/PENODIMA(MENOD),QENODIMA(MENOD),ENODPFA(MENOD),
     &ENODLPFA(MENOD),ENODGPFA(MENOD),ENODIPFA(MENOD)

C The previous derived nodal variables (SI)
      COMMON/ENODDERP/PENODIMP(MENOD),QENODIMP(MENOD),ENODPFP(MENOD),
     &ENODLPFP(MENOD),ENODGPFP(MENOD),ENODIPFP(MENOD)

C The derived connection variables (SI)
      COMMON/ECONDER/PECONTRN(MECON,MPHAS),QECONTRN(MECON,MPHAS),
     &ECONLOS(MECON,MPHAS),ECONPF(MECON,MPHAS),CONTRNI(MECON,MPHAS)

C The averaged derived connection variables (SI)
      COMMON/ECONDERA/PECONTRNA(MECON,MPHAS),QECONTRNA(MECON,MPHAS),
     &ECONLOSA(MECON,MPHAS),ECONPFA(MECON,MPHAS),CONTRNIA(MECON,MPHAS)

C The previous derived connection variables (SI)
      COMMON/ECONDERP/PECONTRNP(MECON,MPHAS),QECONTRNP(MECON,MPHAS),
     &ECONLOSP(MECON,MPHAS),ECONPFP(MECON,MPHAS),CONTRNIP(MECON,MPHAS)


C The derived network variables (SI).
      COMMON/ENETDER/PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,
     &QENETGEN,PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,
     &PENETPHL(MPHAS),QENETPHL(MPHAS),PENETPHG(MPHAS),
     &QENETPHG(MPHAS),PENETPHI(MPHAS),QENETPHI(MPHAS)

C The averaged derived network variables (SI).
      COMMON/ENETDERA/PENETIMPA,QENETIMPA,PENETLODA,QENETLODA,PENETGENA,
     &QENETGENA,PENETRNA,QENETRNA,ENETIPFA,ENETGPFA,ENETLPFA,ENETLOSA,
     &PENETPHLA(MPHAS),QENETPHLA(MPHAS),PENETPHGA(MPHAS),
     &QENETPHGA(MPHAS),PENETPHIA(MPHAS),QENETPHIA(MPHAS)

C The previous derived network variables (SI).
      COMMON/ENETDERP/PENETIMPP,QENETIMPP,PENETLODP,QENETLODP,PENETGENP,
     &QENETGENP,PENETRNP,QENETRNP,ENETIPFP,ENETGPFP,ENETLPFP,ENETLOSP,
     &PENETPHLP(MPHAS),QENETPHLP(MPHAS),PENETPHGP(MPHAS),
     &QENETPHGP(MPHAS),PENETPHIP(MPHAS),QENETPHIP(MPHAS)


C Results averaging
      COMMON/AVRAGE/IDAVER

      DIMENSION ENODIM(MPHAS),ENODIA(MPHAS)

      DIMENSION PHYB(MHYCOM),QHYB(MHYCOM),PHYBA(MHYCOM),QHYBA(MHYCOM),
     &PHYBP(MHYCOM),QHYBP(MHYCOM)

      INTEGER IELFIL,IELIBHL,IELIBW,IELSTREC,NENOD,ENODNO,ENODPHTYP,
     &ENODPH,ENODTYP,NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD,
     &NHYBCOM,HYBCOMNO,HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT

      INTEGER NPOWCOM,POWCOMNO,POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,
     &NPOWCDATS
      
      REAL ENODBASEV,ENODBASEP,PENODL,QENODL,PENODG,QENODG,PENODT,
     &QENODT,PENODIM,QENODIM,ENODPF,PECONTRN,QECONTRN,ECONLOS,ECONPF,
     &PHYB,QHYB,HYBDAT,POWCDAT,PEZON,QEZON,PPOWOC,QPOWOC,QPCMP,PPCMP,
     &PSPM,QSPM,ENODIM,ENODIA,ENODVM,ENODVA

      REAL PENODLA,QENODLA,PENODGA,QENODGA,PENODTA,
     &QENODTA,PENODIMA,QENODIMA,ENODPFA,PECONTRNA,QECONTRNA,ECONLOSA,
     &ECONPFA,PHYBA,QHYBA,PPOWOCA,QPOWOCA

      REAL PENODLP,QENODLP,PENODGP,QENODGP,PENODTP,
     &QENODTP,PENODIMP,QENODIMP,ENODPFP,PECONTRNP,QECONTRNP,ECONLOSP,
     &ECONPFP,PHYBP,QHYBP,PPOWOCP,QPOWOCP

      REAL PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,
     &QENETGEN,PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,
     &PENETPHL,QENETPHL

      REAL PENETIMPP,QENETIMPP,PENETLODP,QENETLODP,PENETGENP,
     &QENETGENP,PENETRNP,QENETRNP,ENETIPFP,ENETGPFP,ENETLPFP,ENETLOSP,
     &PENETPHLP,QENETPHLP,PENETPHGP,QENETPHGP,PENETPHIP,QENETPHIP

   
      COMPLEX ENODVLT,ENODVLTA,ENODVLTP,VCMP,VSPM,VPOWC,CONTRNI,
     &CONTRNIA,CONTRNIP

      LOGICAL ERESAVE,bInStartup

      PI=3.1415927

C Establish if still in start-up period; if so then return with no save.
      ERESAVE=.TRUE.
      if (bInStartup())then
         ERESAVE=.FALSE.
      endif

C Check if simulation spans the New Year period.
C      IF(ISSNY.EQ.1) THEN
C         IF(IDYP.EQ.365.AND.IHRP.EQ.24.AND.ISDS.EQ.1) ERESAVE=.TRUE.
C         IF(IDYP.EQ.1) THEN
C            ISSNY=0
C         ELSE
C            ERESAVE=.FALSE.
C         ENDIF
C      ENDIF
C      IF(ISSNY.EQ.0.AND.(IDYP.GE.ISDS.OR.
C     &   (IDYP.EQ.ISDS-1.AND.IHRP.EQ.24))) THEN
C         ERESAVE=.TRUE.
C      ELSE
C         ERESAVE=.FALSE.
C      ENDIF 

      IF(ERESAVE) THEN
C Save the electrical network results. 

C Recover the last written record no.
        IREC=IELSTREC

        IF(IREC.EQ.IELIBHL-1)THEN
          
          IF(INDCFG.EQ.1)THEN
            NETS=NTSTEP
          ELSEIF(INDCFG.EQ.2)THEN
            NETS=NTSTPP
          ELSEIF(INDCFG.EQ.3.AND.NTSTPP.GT.0)THEN
            NETS=NTSTEP*NTSTPP
          ELSE
            NETS=NTSTEP
          ENDIF

C Write out the time step data to the results library header. 
          IREC=IREC+1
          WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)NTSTEP,NTSTPP,NETS
        ENDIF
        IF(NENOD.GT.0)THEN

C Save the nodal data.
          DO 10 INOD=1,NENOD
            IREC=IREC+1
            IF(IDAVER.EQ.0)THEN

C Average the timestep results
              ENODVLTA(INOD)=0.5*(ENODVLT(INOD)+ENODVLTP(INOD))
              ENODPFA(INOD)=0.5*(ENODPF(INOD)+ENODPFP(INOD))
              PENODLA(INOD)=0.5*(PENODL(INOD)+PENODLP(INOD))
              QENODLA(INOD)=0.5*(QENODL(INOD)+QENODLP(INOD))
              PENODGA(INOD)=0.5*(PENODG(INOD)+PENODGP(INOD))
              QENODGA(INOD)=0.5*(QENODG(INOD)+QENODGP(INOD))
              PENODTA(INOD)=0.5*(PENODT(INOD)+PENODTP(INOD))
              QENODTA(INOD)=0.5*(QENODT(INOD)+QENODTP(INOD))
              PENODIMA(INOD)=0.5*(PENODIM(INOD)+PENODIMP(INOD))
              QENODIMA(INOD)=0.5*(QENODIM(INOD)+QENODIMP(INOD))
              ENODLPFA(INOD)=0.5*(ENODLPF(INOD)+ENODLPFP(INOD))
              ENODGPFA(INOD)=0.5*(ENODGPF(INOD)+ENODGPFP(INOD))
              ENODIPFA(INOD)=0.5*(ENODIPF(INOD)+ENODIPFP(INOD))
            ELSE
              ENODVLTA(INOD)=ENODVLT(INOD)
              ENODPFA(INOD)=ENODPF(INOD)
              PENODLA(INOD)=PENODL(INOD)
              QENODLA(INOD)=QENODL(INOD)
              PENODGA(INOD)=PENODG(INOD)
              QENODGA(INOD)=QENODG(INOD)
              PENODTA(INOD)=PENODT(INOD)
              QENODTA(INOD)=QENODT(INOD)
              PENODIMA(INOD)=PENODIM(INOD)
              QENODIMA(INOD)=QENODIM(INOD)
              ENODLPFA(INOD)=0.5*ENODLPF(INOD)
              ENODGPFA(INOD)=0.5*ENODGPF(INOD)
              ENODIPFA(INOD)=0.5*ENODIPF(INOD)
            ENDIF

            ENODVM=
     &SQRT(REAL(ENODVLTA(INOD))**2.+AIMAG(ENODVLTA(INOD))**2.)
            IF(ENODPH(INOD).EQ.1)THEN
            ENODVA=
     &ATAN(AIMAG(ENODVLTA(INOD))/REAL(ENODVLTA(INOD)))*(180./PI)
            ELSEIF(ENODPH(INOD).EQ.2)THEN
              ENODVA=180.+
     &(ATAN(AIMAG(ENODVLTA(INOD))/REAL(ENODVLTA(INOD)))*(180./PI))
            ELSE
              ENODVA=180.+
     &(ATAN(AIMAG(ENODVLTA(INOD))/REAL(ENODVLTA(INOD)))*(180./PI))
            ENDIF
            WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &ENODVM,ENODVA,ENODPFA(INOD),
     &PENODLA(INOD),QENODLA(INOD),PENODGA(INOD),QENODGA(INOD),
     &PENODTA(INOD),QENODTA(INOD),PENODIMA(INOD),QENODIMA(INOD),
     &ENODLPFA(INOD),ENODGPFA(INOD),ENODIPFA(INOD)
  10      CONTINUE
        ENDIF

C Save the connection data.
        IF(NECON.GT.0)THEN
          DO 20 ICON=1,NECON
          IREC=IREC+1
            IF(IDAVER.EQ.0)THEN
              DO 13 J=1,MPHAS
                PECONTRNA(ICON,J)=0.5*(PECONTRN(ICON,J)
     &+PECONTRNP(ICON,J))
                QECONTRNA(ICON,J)=0.5*(QECONTRN(ICON,J)
     &+QECONTRNP(ICON,J))
                ECONLOSA(ICON,J)=0.5*(ECONLOS(ICON,J)+ECONLOSP(ICON,J))
                ECONPFA(ICON,J)=0.5*(ECONPF(ICON,J)+ECONPFP(ICON,J))
                CONTRNIA(ICON,J)=0.5*(CONTRNI(ICON,J)+CONTRNIP(ICON,J))
 13           CONTINUE
            ELSE
              DO 15 J=1,MPHAS
                PECONTRNA(ICON,J)=PECONTRN(ICON,J)
                QECONTRNA(ICON,J)=QECONTRN(ICON,J)
                ECONLOSA(ICON,J)=ECONLOS(ICON,J)
                ECONPFA(ICON,J)=ECONPF(ICON,J)
                CONTRNIA(ICON,J)=CONTRNI(ICON,J)
 15           CONTINUE
            ENDIF
            
            DO 17 J=1,MPHAS
              ENODIM(J)=
     &SQRT(REAL(CONTRNIA(ICON,J))**2.+AIMAG(CONTRNIA(ICON,J))**2.)
              IF(REAL(CONTRNIA(ICON,J)).GT.0.)THEN
                IF(CONPH(ICON,J).EQ.1)THEN
                  ENODIA(J)=
     &ATAN(AIMAG(CONTRNIA(ICON,J))/REAL(CONTRNIA(ICON,J)))*(180./PI)
                ELSEIF(CONPH(ICON,J).EQ.2)THEN 
                   ENODIA(J)=180.+
     &(ATAN(AIMAG(CONTRNIA(ICON,J))/REAL(CONTRNIA(ICON,J)))*(180./PI))       
                ELSE
                  ENODIA(J)=180.+
     &(ATAN(AIMAG(CONTRNIA(ICON,J))/REAL(CONTRNIA(ICON,J)))*(180./PI))
 
                ENDIF
              ELSE
                ENODIA(J)=0.0
              ENDIF
 17         CONTINUE
            WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &(PECONTRNA(ICON,J),QECONTRNA(ICON,J),-ECONLOSA(ICON,J),
     &ECONPFA(ICON,J),ENODIM(J),ENODIA(J),J=1,MPHAS)
  20      CONTINUE
        ENDIF

C Save the power only components data.
        IF(NPOWCOM.GT.0) THEN
          DO 30 IPW=1,NPOWCOM
            IF(IDAVER.EQ.0)THEN
              PPOWOCA(IPW)=0.5*(PPOWOC(IPW)+PPOWOCP(IPW))
              QPOWOCA(IPW)=0.5*(QPOWOC(IPW)+QPOWOCP(IPW))
            ELSE
              PPOWOCA(IPW)=PPOWOC(IPW)
              QPOWOCA(IPW)=QPOWOC(IPW)
            ENDIF
  30      CONTINUE
          IF(MOD(2*NPOWCOM,IELIBW).NE.0)THEN
            NREC=INT((2*NPOWCOM)/IELIBW)+1
          ELSE
            NREC=INT((2*NPOWCOM)/IELIBW)
          ENDIF
          DO 35 I=1,NREC

C Pack the power only component power flows into NRECs. Checking first
C that array limits are not exceeded.
            IF(IELIBW/2+(IELIBW*(I-1)/2).GT.MPOWCOM)THEN
              ILIM=MPOWCOM-(IELIBW*(I-1)/2)
            ELSE
              ILIM=IELIBW/2
            ENDIF
            IREC=IREC+1 
            WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &(PPOWOCA(J+(IELIBW*(I-1)/2)),QPOWOCA(J+(IELIBW*(I-1)/2))
     &,J=1,ILIM)
  35      CONTINUE
        ENDIF

C Save the hybrid components data.
        IF(NHYBCOM.GT.0) THEN
          IF(MOD(2*NHYBCOM,IELIBW).NE.0)THEN
            NREC=INT((2*NHYBCOM)/IELIBW)+1
          ELSE
            NREC=INT((2*NHYBCOM)/IELIBW)
          ENDIF

C Place the hybrid component power data in temporary  P&Q arrays for writing 
C results. 
          DO 40 IHYB=1,NHYBCOM
              PHYB(IHYB)=0.0
              QHYB(IHYB)=0.0
            IF(HYCOMTYP(IHYB).EQ.1) THEN 
              PHYB(IHYB)=PEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))
              QHYB(IHYB)=QEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))   
            ELSEIF(HYCOMTYP(IHYB).EQ.2) THEN
              PHYB(IHYB)=PSPM(HYLOC(IHYB,1))
              QHYB(IHYB)=QSPM(HYLOC(IHYB,1))
            ELSEIF(HYCOMTYP(IHYB).EQ.3) THEN
              PHYB(IHYB)=PPCMP(HYLOC(IHYB,1))
              QHYB(IHYB)=QPCMP(HYLOC(IHYB,1))
            ENDIF
            IF(IDAVER.EQ.0)THEN
              PHYBA(IHYB)=0.5*(PHYB(IHYB)+PHYBP(IHYB))
              QHYBA(IHYB)=0.5*(QHYB(IHYB)+QHYBP(IHYB))     
            ELSE
              PHYBA(IHYB)=PHYB(IHYB)
              QHYBA(IHYB)=QHYB(IHYB) 
            ENDIF     
  40      CONTINUE

C Pack the hybrid component power flows into NRECs
          DO 50 I=1,NREC
            IREC=IREC+1 

C Check that array limits are not exceeded.
            IF(IELIBW/2+(IELIBW*(I-1)/2).GT.MHYCOM)THEN
              ILIM=MHYCOM-(IELIBW*(I-1)/2)
            ELSE
              ILIM=IELIBW/2
            ENDIF
            WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)
     &(PHYBA(J+(IELIBW*(I-1)/2)),QHYBA(J+(IELIBW*(I-1)/2)),J=1,ILIM)
  50      CONTINUE
        ENDIF      

C Save the global network values. 
        IREC=IREC+1
        IF(IDAVER.EQ.0)THEN
          PENETIMPA=0.5*(PENETIMP+PENETIMPP)
          QENETIMPA=0.5*(QENETIMP+QENETIMPP)
          PENETLODA=0.5*(PENETLOD+PENETLODP)
          QENETLODA=0.5*(QENETLOD+QENETLODP)
          PENETGENA=0.5*(PENETGEN+PENETGENP)
          QENETGENA=0.5*(QENETGEN+QENETGENP)
          PENETRNA=0.5*(PENETRN+PENETRNP)
          QENETRNA=0.5*(QENETRN+QENETRNP)
          ENETIPFA=0.5*(ENETIPF+ENETIPFP)
          ENETGPFA=0.5*(ENETGPF+ENETGPFP)
          ENETLPFA=0.5*(ENETLPF+ENETLPFP)
          ENETLOSA=0.5*(ENETLOS+ENETLOSP)
          DO 53 J=1,MPHAS
            PENETPHLA(J)=0.5*(PENETPHL(J)+PENETPHLP(J))
            QENETPHLA(J)=0.5*(QENETPHL(J)+QENETPHLP(J))
            PENETPHGA(J)=0.5*(PENETPHG(J)+PENETPHGP(J))
            QENETPHGA(J)=0.5*(QENETPHG(J)+QENETPHGP(J))
            PENETPHIA(J)=0.5*(PENETPHI(J)+PENETPHIP(J))
            QENETPHIA(J)=0.5*(QENETPHI(J)+QENETPHIP(J))
  53      CONTINUE
        ELSE
          PENETIMPA=PENETIMP
          QENETIMPA=QENETIMP
          PENETLODA=PENETLOD
          QENETLODA=QENETLOD
          PENETGENA=PENETGEN
          QENETGENA=QENETGEN
          PENETRNA=PENETRN
          QENETRNA=QENETRN
          ENETIPFA=ENETIPF
          ENETGPFA=ENETGPF
          ENETLPFA=ENETLPF
          ENETLOSA=ENETLOS
          DO 55 J=1,MPHAS
            PENETPHLA(J)=PENETPHL(J)
            QENETPHLA(J)=QENETPHL(J)
            PENETPHGA(J)=PENETPHG(J)
            QENETPHGA(J)=QENETPHG(J)
            PENETPHIA(J)=PENETPHI(J)
            QENETPHIA(J)=QENETPHI(J)
  55      CONTINUE

        ENDIF

        WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)PENETIMPA,QENETIMPA,
     &PENETLODA,QENETLODA,PENETGENA,QENETGENA,PENETRNA,QENETRNA,
     &ENETIPFA,ENETGPFA,ENETLPFA,-ENETLOSA

C Save the network phase quantities
        IREC=IREC+1
        WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999)(PENETPHLA(IPH),
     &QENETPHLA(IPH),PENETPHGA(IPH),QENETPHGA(IPH),PENETPHIA(IPH),
     &QENETPHIA(IPH),IPH=1,MPHAS)

C Save the last written record.
        IELSTREC=IREC
      ENDIF

C For all time steps move the calculated future time step  value 
C to the present time step. 

C Shift the nodal variables
      DO 60 INOD=1,NENOD
        ENODVLTP(INOD)=ENODVLT(INOD)
        ENODPFP(INOD)=ENODPF(INOD)
        PENODLP(INOD)=PENODL(INOD)
        QENODLP(INOD)=QENODL(INOD)
        PENODGP(INOD)=PENODG(INOD)
        QENODGP(INOD)=QENODG(INOD)
        PENODTP(INOD)=PENODT(INOD)
        QENODTP(INOD)=QENODT(INOD)
        PENODIMP(INOD)=PENODIM(INOD)
        QENODIMP(INOD)=QENODIM(INOD)
        ENODLPFP(INOD)=ENODLPF(INOD)
        ENODGPFP(INOD)=ENODGPF(INOD)
        ENODIPFP(INOD)=ENODIPF(INOD)
  60  CONTINUE

C Shift the connection variables
      DO 70 ICON=1,NECON
        DO 75 J=1,MPHAS
          PECONTRNP(ICON,J)=PECONTRN(ICON,J)
          QECONTRNP(ICON,J)=QECONTRN(ICON,J)
          ECONLOSP(ICON,J)=ECONLOS(ICON,J)
          ECONPFP(ICON,J)=ECONPF(ICON,J)
          CONTRNIP(ICON,J)=CONTRNI(ICON,J)
  75    CONTINUE

  70  CONTINUE

C Shift the network variables.
      PENETIMPP=PENETIMP
      QENETIMPP=QENETIMP
      PENETLODP=PENETLOD
      QENETLODP=QENETLOD
      PENETGENP=PENETGEN
      QENETGENP=QENETGEN
      PENETRNP=PENETRN
      QENETRNP=QENETRN
      ENETIPFP=ENETIPF
      ENETGPFP=ENETGPF
      ENETLPFP=ENETLPF
      ENETLOSP=ENETLOS
      DO 80 J=1,MPHAS
        PENETPHLP(J)=PENETPHL(J)
        QENETPHLP(J)=QENETPHL(J)
        PENETPHGP(J)=PENETPHG(J)
        QENETPHGP(J)=QENETPHG(J)
        PENETPHIP(J)=PENETPHI(J)
        QENETPHIP(J)=QENETPHI(J)
  80  CONTINUE 

C Shift the hybrid component data
      DO 90 IHYB=1,NHYBCOM
        PHYBP(IHYB)=PHYB(IHYB)
        QHYBP(IHYB)=QHYB(IHYB)
  90  CONTINUE

C Shift the power only component data
      DO 100 IPOW=1,NPOWCOM
        PPOWOCP(IPOW)=PPOWOC(IPOW)
        QPOWOCP(IPOW)=QPOWOC(IPOW)
 100  CONTINUE
   
      RETURN

  999 WRITE(IUOUT,*)' ENETLIBSV ERROR: problem writing results.'
      STOP 

      END
