C Network parameters:
C Number of electrical network types 1-d.c. 2-1-ph ... 4-3ph 5-balanced
      integer MENTYP
      PARAMETER (MENTYP=5)

C The number of phase types 1-d.c. 2-1-ph ... 4-3-ph 5-balanced
      integer MPHATY
      PARAMETER (MPHATY=5)

C The maximum mumber of phases
      INTEGER MPHAS
      PARAMETER (MPHAS=3)

C The maximum number of electrical nodes
      INTEGER MENOD
      PARAMETER (MENOD=100)

C The maximum number of connected hybrid nodes and data items
      INTEGER MHYCOM, MHYDAT
      PARAMETER (MHYCOM=100)
      PARAMETER (MHYDAT=30)

C The maximum number of connected power components and data items
      INTEGER MPOWCOM,MPWDAT,MPCDS
      PARAMETER (MPOWCOM=30)
      PARAMETER (MPWDAT=100)
      PARAMETER (MPCDS=30)

C The maximum number of connecting components and data items
      INTEGER MCONECOM, MCCDAT
      PARAMETER (MCONECOM=100)
      PARAMETER (MCCDAT=30)

C The maximum number of connections
      INTEGER MECON
      PARAMETER (MECON=100)

C Maximim number of actual and PU Z array elements
      INTEGER MZARRY
      PARAMETER (MZARRY=18)

C Maximim number of primitive Y array elements
      INTEGER MYARRY
      PARAMETER (MYARRY=18)

C Solution parameters:
C Matrix parameters equals 2XMENOD.
      INTEGER MJROW
      PARAMETER (MJROW=200)
      INTEGER MJCOL
      PARAMETER (MJCOL=200)

C Iteration and convergence criteria
      INTEGER MENRITER
      PARAMETER (MENRITER=30)

C Library parameter, electrical library number (IFIL+MADFIL) and width.
      INTEGER MADFIL
      PARAMETER (MADFIL=21)

C The library width should be an even number.
      INTEGER MELIBW
      PARAMETER (MELIBW=20)

C The electrical results library header.
      INTEGER MELIBH
      PARAMETER (MELIBH=3)

C The maximum number of recovered data items associated with one electrical
C network entity.
      INTEGER MERPOS
      PARAMETER (MERPOS=6)

C The 1-D revover array width.
      INTEGER MERAWD
      PARAMETER (MERAWD=200)

C Maximum number of additional variables requiring time-row transport that
C can be assocaited with a power-only component.
      INTEGER MEnet_Time_Row_MAX
      PARAMETER ( MEnet_Time_Row_MAX = 30 )

C========================================================================
C Electrical network commons
C========================================================================

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

      character  ENTFLNAM*72
      integer    IENTXIST     ! Elec netowrks flag. True if > 0

C The electrical network
      COMMON/ENET/ENTYPE

      integer ENTYPE

C Simulation time and timestep.
      COMMON/ENETDELTAT/IETSET,ENETSTP
      
      integer ietset
      real enetstp

C More network-related info
      COMMON/ENETMSC/PHASEANG(MPHAS)

      real PHASEANG


C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

      integer NECON
      integer ECONO
      integer CONPHTYP
      integer CONPH
      integer CCNO
      integer SENOD
      integer EENOD

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

      INTEGER NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,NCONECOMDAT
      REAL CONECOMDAT


C The descriptive strings
      COMMON/ECDESC/ENTYPESTR(MENTYP),PHTYPSTR(MPHATY),ENDESC,
     &ENODTYPSTR(MENOD),HYDESC(MHYCOM),CONECOMDESC(MCONECOM),
     &POWCOMDESC(MPOWCOM)

      CHARACTER*12 ENTYPESTR,PHTYPSTR,ENODTYPSTR
      character*72 ENDESC,HYDESC,CONECOMDESC,POWCOMDESC


C The array of driving port and transfer admittances (per-unit)
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

      integer NYELM
      complex YADM


C The PER-UNIT electrical network state variables.
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

      complex VOLTPUP, VOLTPUF

C---------
C The derived nodal variables (SI)
      COMMON/ENODDER/PENODIM(MENOD),QENODIM(MENOD),ENODPF(MENOD),
     &ENODLPF(MENOD),ENODGPF(MENOD),ENODIPF(MENOD)
      real PENODIM
      real QENODIM
      real ENODPF
      real ENODLPF
      real ENODGPF
      real ENODIPF

C The average derived nodal variables (SI)
      COMMON/ENODDERA/PENODIMA(MENOD),QENODIMA(MENOD),ENODPFA(MENOD),
     &ENODLPFA(MENOD),ENODGPFA(MENOD),ENODIPFA(MENOD)
      real PENODIMA
      real QENODIMA
      real ENODPFA
      real ENODLPFA
      real ENODGPFA
      real ENODIPFA

C The previous derived nodal variables (SI)
      COMMON/ENODDERP/PENODIMP(MENOD),QENODIMP(MENOD),ENODPFP(MENOD),
     &ENODLPFP(MENOD),ENODGPFP(MENOD),ENODIPFP(MENOD)
      real PENODIMP
      real QENODIMP
      real ENODPFP
      real ENODLPFP
      real ENODGPFP
      real ENODIPFP


C The derived nodal variables (per-unit)
      COMMON/ENODDERPU/PENODIMPU(MENOD),QENODIMPU(MENOD),
     &ENODPFPU(MENOD),ENODLPFPU(MENOD),ENODGPFPU(MENOD),ENODIPFPU(MENOD)

      real PENODIMPU
      real QENODIMPU
      real ENODPFPU
      real ENODLPFPU
      real ENODGPFPU
      real ENODIPFPU


C The derived connection variables (SI)
      COMMON/ECONDER/PECONTRN(MECON,MPHAS),QECONTRN(MECON,MPHAS),
     &ECONLOS(MECON,MPHAS),ECONPF(MECON,MPHAS),CONTRNI(MECON,MPHAS)

      real PECONTRN
      real QECONTRN
      real ECONLOS
      real ECONPF
      complex CONTRNI

C The averaged derived connection variables (SI)
      COMMON/ECONDERA/PECONTRNA(MECON,MPHAS),QECONTRNA(MECON,MPHAS),
     &ECONLOSA(MECON,MPHAS),ECONPFA(MECON,MPHAS),CONTRNIA(MECON,MPHAS)
      real PECONTRNA
      real QECONTRNA
      real ECONLOSA
      real ECONPFA
      complex CONTRNIA

C The previous derived connection variables (SI)
      COMMON/ECONDERP/PECONTRNP(MECON,MPHAS),QECONTRNP(MECON,MPHAS),
     &ECONLOSP(MECON,MPHAS),ECONPFP(MECON,MPHAS),CONTRNIP(MECON,MPHAS)
      real PECONTRNP
      real QECONTRNP
      real ECONLOSP
      real ECONPFP
      complex CONTRNIP


C The derived connection variables (per-unit)
      COMMON/ECONDERPU/PECONTRNPU(MECON,MPHAS),QECONTRNPU(MECON,MPHAS),
     &ECONLOSPU(MECON,MPHAS),ECONPFPU(MECON,MPHAS),
     &CONTRNIPU(MECON,MPHAS)

      real PECONTRNPU
      real QECONTRNPU
      real ECONLOSPU
      real ECONPFPU
      complex CONTRNIPU

C The derived network variables (SI).
      COMMON/ENETDER/PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,
     &QENETGEN,PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,
     &PENETPHL(MPHAS),QENETPHL(MPHAS),PENETPHG(MPHAS),
     &QENETPHG(MPHAS),PENETPHI(MPHAS),QENETPHI(MPHAS)

      real PENETIMP
      real QENETIMP
      real PENETLOD
      real QENETLOD
      real PENETGEN
      real QENETGEN
      real PENETRN
      real QENETRN
      real ENETIPF
      real ENETGPF
      real ENETLPF
      real ENETLOS
      real PENETPHL
      real QENETPHG
      real PENETPHG
      real QENETPHL
      real PENETPHI
      real QENETPHI

C The averaged derived network variables (SI).
      COMMON/ENETDERA/PENETIMPA,QENETIMPA,PENETLODA,QENETLODA,PENETGENA,
     &QENETGENA,PENETRNA,QENETRNA,ENETIPFA,ENETGPFA,ENETLPFA,ENETLOSA,
     &PENETPHLA(MPHAS),QENETPHLA(MPHAS),PENETPHGA(MPHAS),
     &QENETPHGA(MPHAS),PENETPHIA(MPHAS),QENETPHIA(MPHAS)
      real PENETIMPA
      real QENETIMPA
      real PENETLODA
      real QENETLODA
      real PENETGENA
      real QENETGENA
      real PENETRNA
      real QENETRNA
      real ENETIPFA
      real ENETGPFA
      real ENETLPFA
      real ENETLOSA
      real PENETPHLA
      real QENETPHGA
      real PENETPHGA
      real QENETPHLA
      real PENETPHIA
      real QENETPHIA

C The previous derived network variables (SI).
      COMMON/ENETDERP/PENETIMPP,QENETIMPP,PENETLODP,QENETLODP,PENETGENP,
     &QENETGENP,PENETRNP,QENETRNP,ENETIPFP,ENETGPFP,ENETLPFP,ENETLOSP,
     &PENETPHLP(MPHAS),QENETPHLP(MPHAS),PENETPHGP(MPHAS),
     &QENETPHGP(MPHAS),PENETPHIP(MPHAS),QENETPHIP(MPHAS)
      real PENETIMPP
      real QENETIMPP
      real PENETLODP
      real QENETLODP
      real PENETGENP
      real QENETGENP
      real PENETRNP
      real QENETRNP
      real ENETIPFP
      real ENETGPFP
      real ENETLPFP
      real ENETLOSP
      real PENETPHLP
      real QENETPHGP
      real PENETPHGP
      real QENETPHLP
      real PENETPHIP
      real QENETPHIP


C The derived network variables (per-unit).
      COMMON/ENETDERPU/PENETIMPPU,QENETIMPPU,PENETLODPU,QENETLODPU,
     &PENETGENPU,QENETGENPU,PENETRNPU,QENETRNPU,ENETIPFPU,ENETGPFPU,
     &ENETLPFPU,ENETLOSPU,PENETPHLPU(MPHAS),QENETPHGPU(MPHAS),
     &PENETPHGPU(MPHAS),QENETPHLPU(MPHAS),PENETPHIPU(MPHAS),
     &QENETPHIPU(MPHAS)

      real PENETIMPPU
      real QENETIMPPU
      real PENETLODPU
      real QENETLODPU
      real PENETGENPU
      real QENETGENPU
      real PENETRNPU
      real QENETRNPU
      real ENETIPFPU
      real ENETGPFPU
      real ENETLPFPU
      real ENETLOSPU
      real PENETPHLPU
      real QENETPHGPU
      real PENETPHGPU
      real QENETPHLPU
      real PENETPHIPU
      real QENETPHIPU


C---------------------------------------------------------------------------------
C Common holding the power-only component description read from the power flow
C network input file.
C
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)

C Declarations for common block variables.
      integer npowcom        !- number of power-only components
      integer powcomno       !- the power-only component reference number
      integer powcomid       !- power-only component database id number
      integer powcomphtyp    !- power-only component phase type
      integer powconen       !- nodes connected to the power-only component
      integer npowcdat       !- number of data items associated with the power-only component
      integer npowcdats      !- number of string data items associated with the power-only component
      real    powcdat        !- power-only component numerical data item

C---------------------------
C Power-only component "Battery" type signatures
C---------------------------
C Lead acid
      INTEGER pba
      PARAMETER (pba = 17)
C Vanadium redox
      INTEGER vrb
      PARAMETER (vrb = 21)
C Lithium-ion
      INTEGER lion
      PARAMETER (lion = 22)

C-------------------------------------------------
C Battery constants

C   Faraday constant
      REAL Faraday_constant
      PARAMETER (Faraday_constant = 96485.)

C   Standard potential of Vanadium Redox Battery (VRB), V
      REAL VRB_E0
      PARAMETER (VRB_E0 = 1.259)

C    Gas constant
      REAL gas_constant
      PARAMETER (gas_constant = 8.314)

C---------------------------------------------------------------------------------
C Common holding the node and component names defined in the electric
C network input file.
C
      COMMON/ECNAMES/ENODNAM(MENOD),HYCOMNAM(MHYCOM),
     &HYCOMTYPSTR(MHYCOM),POWCOMNAM(MPOWCOM),POWCDATS(MPOWCOM,MPCDS),
     &CONECOMNAM(MCONECOM)

C Declarations for common block variables.
      character ENODNAM*12        !-string holding electrical node name
      character HYCOMNAM*12       !-string holding hybrid component name
      character HYCOMTYPSTR*12    !-string containing the hybrid component type
      character POWCOMNAM*12      !-string holding the power-only component name
      character POWCDATS*72       !-string data item for a power-only component
      character CONECOMNAM*12     !-string containing connecting component name




C---------------------------------------------------------------------------------
C Commons for connected components
C Zone lighting and power commons, alter the second parameter if the
C number of casual gain types changes!
      COMMON/ENETZONE/PEZON(MCOM,MGTY),QEZON(MCOM,MGTY)

      real PEZON    !  Real power (W)
      real QEZON    !  Reactive power (W)

C Plant
      COMMON/ENETPCOMP/PPCMP(MPCOM),QPCMP(MPCOM),
     &VCMP(MPCOM)

      REAL    PPCMP  !  Real power (W)
      REAL    QPCMP  !  Reactive power (W)
      complex VCMP   !  Complex power vector

C Plant component node connections
C Used for plant components connected to a DC and an AC node.
C iplt_conn_nodes holds the number of nodes the plant component is connected to
C   (1 or 2). If connected to two nodes it is assumed one is AC and the other DC
C   variable set to zero for hybrid components that are not plant components.
      common/pltnds/iplt_conn_nodes(MHYCOM),iplt_dcnode_id(MHYCOM),
     &              iplt_acnode_id(MHYCOM)
      integer  iplt_conn_nodes
      integer  iplt_dcnode_id        !- node index of the DC node the plant component connects to
      integer  iplt_acnode_id        !- node index of the AC node the plant component connects to

C For plant components connected to a DC and AC node
       COMMON/enetpcomp2/pdcpower(mpcom),pacpower(mpcom)
       real    pdcpower      !- dc power load/generation of plant component
       real    pacpower      !- ac power load/generation of plant component


C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)

      real pspm    !- real power of special material
      real qspm    !- reactive power load/generation of special material
      complex vspm !- voltage of special material node

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

      integer NHYBCOM     !  Number of Hybrid Components
      integer HYBCOMNO    !  index of component
      integer HYCOMTYP    !  Type of component
      integer HYCOMPHTYP  !  Phase type of component
      integer HYCONEN     !  Nodes connected to component
      integer HYLOC       !  Location of component
      integer NHYBDAT     !  Number of data for component
      real    HYBDAT      !  misc. data for component

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

      integer NENOD       !  Number of nodes in electrical network
      integer ENODNO
      integer ENODPHTYP
      integer ENODPH
      integer ENODTYP
      real    ENODBASEV
      real    ENODBASEP

C The sum of nodal boundary condition power flows (SI boundary conditions)
      COMMON/ENODBC/PENOD(MENOD),QENOD(MENOD)

      real PENOD
      real QENOD

C The per-unit nodal boundary conditions
      COMMON/ENODBCPU/PENODPU(MENOD),QENODPU(MENOD)

      real PENODPU
      real QENODPU


C Derived System and Nodal Values
      COMMON/ENETSMC/LNPWRBAS,ENODZBAS(MENOD),ENODIBAS(MENOD)

      real LNPWRBAS
      real ENODZBAS
      real ENODIBAS


C Common block storing electrical domain time-row data for additional
C (hybrid component?) data items.
      common / Enet_Power_Time_Row_Data /
     &    iPow_Time_Row_Count,
     &    fPow_Time_Row_Present,
     &    fPow_Time_Row_Future
C.....# of additional data items associated with power-only component.
      integer iPow_Time_Row_Count ( MPOWCOM )
C.....Component present time-row data
      real fPow_Time_Row_Present ( MPOWCOM, MEnet_Time_Row_MAX )
C.....Component future time-row data
      real fPow_Time_Row_Future ( MPOWCOM, MEnet_Time_Row_MAX )

C---------------------------------------------------------------------------------
C Common holding power generation and loads data for power-only components.
C
      COMMON/ENETPOWC/PPOWOC(MPOWCOM),QPOWOC(MPOWCOM),
     &VPOWC(MPOWCOM)

C Declarations for common block variables.
      real    ppowoc         !-real power generated/load for power-only component
      real    qpowoc         !-reactive power generated/load for power-only component
      complex vpowc          !-voltage of power only component

C Power only components - averaged values
      COMMON/ENETPOWCA/PPOWOCA(MPOWCOM),QPOWOCA(MPOWCOM),
     &VPOWCA(MPOWCOM)
      real    ppowoca        
      real    qpowoca        
      complex vpowca         

C Power only components - previous values
      COMMON/ENETPOWCP/PPOWOCP(MPOWCOM),QPOWOCP(MPOWCOM),
     &VPOWCP(MPOWCOM)
      real    PPOWOCP
      real    QPOWOCP
      complex VPOWCP


C---------------------------------------------------------------------------------
C The load, generated and transmitted nodal power flows (SI)
C
      COMMON/ENODLGT/PENODL(MENOD),QENODL(MENOD),
     &PENODG(MENOD),QENODG(MENOD),PENODT(MENOD),QENODT(MENOD)
      real    penodl         !- real power load on node
      real    qenodl         !- reactive power load on node
      real    penodg         !- real power generation at node
      real    qenodg         !- reactive power generation at node
      real    penodt         !- real transmitted power at node
      real    qenodt         !- reactive transmitted power at node

C The averaged load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGTA/PENODLA(MENOD),QENODLA(MENOD),PENODGA(MENOD),
     &QENODGA(MENOD),PENODTA(MENOD),QENODTA(MENOD)
      real    penodla
      real    qenodla
      real    penodga
      real    qenodga
      real    penodta
      real    qenodta

C The previous load, generated and transmitted nodal power flows (SI)
      COMMON/ENODLGTP/PENODLP(MENOD),QENODLP(MENOD),PENODGP(MENOD),
     &QENODGP(MENOD),PENODTP(MENOD),QENODTP(MENOD)
      real    penodlp
      real    qenodlp
      real    penodgp
      real    qenodgp
      real    penodtp
      real    qenodtp


C The load, generated and transmitted nodal power flows (per-unit)
      COMMON/ENODLGTPU/PENODLPU(MENOD),QENODLPU(MENOD),
     &PENODGPU(MENOD),QENODGPU(MENOD),PENODTPU(MENOD),QENODTPU(MENOD)

      real PENODLPU
      real QENODLPU
      real PENODGPU
      real QENODGPU
      real PENODTPU
      real QENODTPU


      COMMON/ENODSLV/ENODVLT(MENOD)
      complex enodvlt        !- the electrical network solved voltages (SI)

C The average electrical network solved voltages (SI)
      COMMON/ENODSLVA/ENODVLTA(MENOD)
      complex ENODVLTA

C The previous electrical network solved voltages (SI)
      COMMON/ENODSLVP/ENODVLTP(MENOD)
      complex ENODVLTP


C---------------------------------------------------------------------------------
C Electrical model time
      COMMON/ENETIME/ENETIMEF,ENETIMEP

      REAL         ENETIMEF, ENETIMEP



