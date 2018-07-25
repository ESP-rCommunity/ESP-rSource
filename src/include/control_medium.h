C control.h

C Include file which defines/describes control specific PARAMETERs and COMMONs.
C Re-compile all control-related modules if this file is altered. If MDTY or
C MCOM are changed in building.h then MCDT and MCF should be updated.

      INTEGER MNONID !Maximum number of non-ideal sensor/actuator loops
      PARAMETER (MNONID=2)
      INTEGER MCDT ! Max. day types (should match MDTY in building.h)
      PARAMETER (MCDT=15)
      INTEGER MCF ! Max. control loops (allow one per zone MCOM in building.h)
      PARAMETER (MCF=82)
      INTEGER MCDP ! Max. periods per day
      PARAMETER (MCDP=24)
      INTEGER MISCC ! Max. miscellaneous data items
      PARAMETER (MISCC=41)

C Domain specific control descriptions
      common/ctlnm/znctldoc,plctldoc,flctldoc,elctldoc,glctldoc,
     &             opticdoc, CFCctldoc
      character znctldoc*248,plctldoc*248,flctldoc*248,elctldoc*248
      character glctldoc*248,opticdoc*248,CFCctldoc*248     
      
C Domain specific commons follow, detailed explaination in econtrol.F
C Building control
      COMMON/BCTL/NCF,IBSN(MCF,4),IBAN(MCF,3),NBCDT(MCF),
     &     IBCDV(MCF,MCDT,2),NBCDP(MCF,MCDT),TBCPS(MCF,MCDT,MCDP),
     &     IBCTYP(MCF,MCDT,MCDP),IBCLAW(MCF,MCDT,MCDP),
     &     BMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NCF,IBSN,IBAN,NBCDT,IBCDV,NBCDP,IBCTYP,IBCLAW
      REAL TBCPS,BMISCD

C Mass flow network control
      COMMON/FCTL/NCC,IFSN(MCF,4),IFAN(MCF,3),NFCDT(MCF),
     &       IFCDV(MCF,MCDT,2),NFCDP(MCF,MCDT),TFCPS(MCF,MCDT,MCDP),
     &       IFCTYP(MCF,MCDT,MCDP),IFCLAW(MCF,MCDT,MCDP),
     &       FMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NCC,IFSN,IFAN,NFCDT,IFCDV,NFCDP,IFCTYP,IFCLAW
      REAL TFCPS,FMISCD

C Plant control
      COMMON/PCTL/NCL,IPSN(MCF,5),IPAN(MCF,4),NPCDT(MCF),
     &       IPCDV(MCF,MCDT,2),NPCDP(MCF,MCDT),TPCPS(MCF,MCDT,MCDP),
     &       IPCTYP(MCF,MCDT,MCDP),IPCLAW(MCF,MCDT,MCDP),
     &       PMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NCL,IPSN,IPAN,NPCDT,IPCDV,NPCDP,IPCTYP,IPCLAW
      REAL TPCPS,PMISCD          

C Global control
      COMMON/GCTL/NGF,IGSN(MCF,4),IGAN(MCF,3),NGCDT(MCF),
     &       IGCDV(MCF,MCDT,2),NGCDP(MCF,MCDT),TGCPS(MCF,MCDT,MCDP),
     &       IGCTYP(MCF,MCDT,MCDP),IGCLAW(MCF,MCDT,MCDP),
     &       GMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NGF,IGSN,IGAN,NGCDT,IGCDV,NGCDP,IGCTYP,IGCLAW
      REAL TGCPS,GMISCD          

C Optical controls
      COMMON/OCTL/NOF,IOSN(MCF,4),IOAN(MCF,3),NOCDT(MCF),
     &       IOCDV(MCF,MCDT,2),NOCDP(MCF,MCDT),TOCPS(MCF,MCDT,MCDP),
     &       IOCTYP(MCF,MCDT,MCDP),IOCLAW(MCF,MCDT,MCDP),
     &       OMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NOF,IOSN,IOAN,NOCDT,IOCDV,NOCDP,IOCTYP,IOCLAW
      REAL TOCPS,OMISCD          

C Electrical control.
      COMMON/ECTL/NECL,IESN(MCF,3),IEAN(MCF,3),NECDT(MCF),
     &            IECDV(MCF,MCDT,2),NECDP(MCF,MCDT),
     &            TECPS(MCF,MCDT,MCDP),IECTYP(MCF,MCDT,MCDP),
     &            IECLAW(MCF,MCDT,MCDP),EMISCD(MCF,MCDT,MCDP,MISCC)
      INTEGER NECL,IESN,IEAN,NECDT,IECDV,NECDP,IECTYP,IECLAW
      REAL TECPS,EMISCD          

C Complex fenestration control. Data structure of CFC control
C is summarized variables is documented in econtrol.F
      COMMON/CFCCTL/nCFCctlloops,iCFCsensor(MCF,4),
     &              iCFCactuator(MCF,3),nCFCctldaytypes(MCF),
     &              iCFCctldatevalid(MCF,MCDT,2),
     &              nCFCdayctlperiods(MCF,MCDT),
     &              CFCctlperiodstart(MCF,MCDT,MCDP),
     &              iCFCctltype(MCF,MCDT,MCDP),
     &              iCFCctllaw(MCF,MCDT,MCDP),
     &              CFCmiscdata(MCF,MCDT,MCDP,MISCC)
      INTEGER nCFCctlloops, iCFCactuator, iCFCsensor, nCFCctldaytypes
      INTEGER iCFCctldatevalid, nCFCdayctlperiods
      INTEGER iCFCctltype,iCFCctllaw
      REAL    CFCctlperiodstart,CFCmiscdata
