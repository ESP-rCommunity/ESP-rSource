C NCM Array sizing integers
      integer MFT
      integer MPT
      integer MHT
      integer MHC
      integer MCS
      integer MSS
      integer MBT
      integer MREG
      integer MSTG
      integer MSBT
      integer MDW
      integer MWS
      integer MNS
      PARAMETER (MFT=12)    ! max. fuel types
      PARAMETER (MPT=32)    ! max. system types
      PARAMETER (MHT=25)    ! max. HVAC system types
      PARAMETER (MHC=3)     ! max. system cooling types
      PARAMETER (MCS=MHC*8) ! max. cooling mechanism COP MHC*8
      PARAMETER (MSS=3)     ! max. building services strategies
      PARAMETER (MBT=29)    ! max. building types 
      PARAMETER (MREG=4)    ! max. building regulations to follow
      PARAMETER (MSTG=2)    ! max. building design stages
      PARAMETER (MSBT=3)    ! max. Scottish building types
      PARAMETER (MDW=5)     ! max. DHW generators
      PARAMETER (MWS=6)     ! max. fuel types for DHW generators
      PARAMETER (MNS=28)    ! max. HVAC and DHW systems allowed in model
                            ! max. activities allowed in the model
                            ! This is kept equal to max zones normally allowed
      PARAMETER (MAC=65)    ! max. activity types 

C NCM common blocks for reading SBEM database
C Integers
      COMMON/SBEM01/IBRUKLH(MFT,MPT,MHT),ISYSAPP(MPT,MHT),
     &IATYPNDX(MAC,MBT),IBRUKLC(MHC,MHT),IBRUKLW(MDW,MWS),IBRUKLF(MHT),
     &IBTYPNDX(MBT),IBTYP

C Strings
      COMMON/SBEM02/FUELNAME(MFT),SYSNAME(MPT),HSYSNAME(MHT),BLDSS(MSS),
     &DHWGEN(MDW),BTYPNAME(MBT),SBTYP(MSBT),BLDREG(MREG),BLDSTG(MSTG),
     &ATYPNAME(MAC)

C Real numbers
      COMMON/SBEM03/FUELCO2(MFT),SYSEFFC(MCS,MHT),DHWEFF(MDW,2),
     &SYSEFF(MPT),BLDIF(MSS),BLDLZC(MSS),VERSBEM,SFPDEF

C NCM common blocks for read/write of *.ncm project specific file and
C for BRUKL input file (*.inp)
C Integers
      COMMON/SBEM04/IBRUKH(MNS),IBRUKC(MNS),IBRUKW(MNS),IBRUKF(MNS),
     &IHGEF(MNS),ICGEF(MNS),IFTYP(MNS),IHLZ(MNS),IDHWS(MNS),IDHFL(MNS),
     &INCMSYS(MNS),IDHWLZ(MNS),IACTYTYP(MNS),ISBEM,IBUSERTYP,IBSS,
     &IRGG,ISTG,ISBT,NCMSYS,NDHWSYS

C Strings
      COMMON/SBEM05/DHWNAME(MNS),HVACNAME(MNS),LASBEM,SBREF,APCHK,
     &PJNAME,BADDRESS,BCITY,OWNERNAME,OWNERTEL,BUILDINGPC,OWNERADDR,
     &OWNERCITY,OWNERPC,CERTIFNAME,CERTTEL,CERTADDRESS,CERTCITY,CERTPC

C Real numbers
      COMMON/SBEM06/ROOFFR,WALLFR,BINF50,HGEF(MNS),CGEF(MNS),SFPHS(MNS),
     &HWEF(MNS)

C Project specific *.ncm file variables
      INTEGER ISBEM     ! a value of 1 signals that isbem data exists
      INTEGER IBUSERTYP ! this matches the list of buildings in isbem.
      INTEGER IBSS      ! building service strategy
      INTEGER IRGG      ! building regulations to follow
      INTEGER ISTG      ! building design stage
      INTEGER ISBT      ! Scottish building types
      INTEGER INCMSYS   ! Index of HVAC system type (read from SBEM db)
      INTEGER NCMSYS    ! total number of HVAC system types in model
      INTEGER NDHWSYS   ! total number of DHW generators in model
      INTEGER IHGEF     ! index of heat generator (read from SBEM db)
      INTEGER ICGEF     ! index of cool generator (read from SBEM db)
      INTEGER IFTYP     ! index number of fuel type for heat generator
      INTEGER IHLZ      ! zone to HVAC system coupling index
      INTEGER IDHWS     ! index of DHW generators in the model
      INTEGER IDHFL     ! index of fuel type for DHW generator
      INTEGER IDHWLZ    ! zone to DHW coupling index, 0 if no DHW in that zone
                        ! +N if coupled with DHW system number N
                        ! -N if coupled with HVAC system number N
      INTEGER IACTYTYP  ! activity type index for each zone
                        
      CHARACTER*72 LASBEM   ! SBEM project specific file name (*.ncm)
      CHARACTER*3  SBREF    ! Scottish Accredited Construction Details followed (yes/no)
      CHARACTER*3  APCHK    ! Compliance of air permeabilty to be checked (yes/no)
      CHARACTER*30 DHWNAME  ! Names of DHW generators in the model
      CHARACTER*30 HVACNAME ! Names of HVAC systems in the model
      CHARACTER*64 PJNAME       ! Project name
      CHARACTER*64 BADDRESS     ! Building address
      CHARACTER*64 BCITY        ! Building in which city?
      CHARACTER*64 OWNERNAME    ! Owner name
      CHARACTER*64 OWNERTEL     ! Owner telephone number
      CHARACTER*64 BUILDINGPC   ! Building post code
      CHARACTER*64 OWNERADDR    ! Owner address
      CHARACTER*64 OWNERCITY    ! Owner in which city?
      CHARACTER*64 OWNERPC      ! Owner post code
      CHARACTER*64 CERTIFNAME   ! Certifier name
      CHARACTER*64 CERTTEL      ! Certifier telephone number
      CHARACTER*64 CERTADDRESS  ! Certifier address
      CHARACTER*64 CERTCITY     ! Certifier in which city?
      CHARACTER*64 CERTPC       ! Certifier post code

      REAL ROOFFR   ! roof glazing fraction required
      REAL WALLFR   ! wall glazing fraction required
      REAL BINF50   ! building's permeability at 50Pa in m3/m2.hour
      REAL HGEF     ! heat generator efficiency/COP
      REAL CGEF     ! cool generatr efficiency/COP
      REAL SFPHS    ! Specific fan power SFP for ventilation W/l/s
      REAL HWEF     ! DHW generator efficiency

C SBEM database variables
      INTEGER IBTYP     ! this matches the index value in NCM guide table 4
      INTEGER ISYSAPP   ! hash table with elements equal to 1 if that heating or
                        ! cooling generator is applicable to that HVAC system
      INTEGER IBRUKLH   ! BRUKL HVAC-HEAT index number
      INTEGER IBRUKLC   ! BRUKL HVAC-COOL index number
      INTEGER IBRUKLW   ! BRUKL HVAC-DHW index number (domestic hot water)
      INTEGER IBRUKLF   ! BRUKL HVAC-SFP index number (specific fan power)
      INTEGER IBTYPNDX  ! Building type index number
      INTEGER IATYPNDX  ! Activity type index number hash table

      CHARACTER*42 FUELNAME ! Name of fuel used
      CHARACTER*60 SYSNAME  ! heating or cooling system name
      CHARACTER*70 HSYSNAME ! HVAC system name
      CHARACTER*25 DHWGEN   ! DHW generator name
      CHARACTER*40 BLDSS    ! building services strategy
      CHARACTER*50 BTYPNAME ! Building type name
      CHARACTER*40 SBTYP    ! Scottish building type
      CHARACTER*50 BLDREG   ! Building regulations to follow
      CHARACTER*12 BLDSTG   ! Building design stage (as built or as designed)
      CHARACTER*40 ATYPNAME ! Activity type name

      REAL VERSBEM  ! SBEM db version no.
      REAL SFPDEF   ! default specific fan power (units?)
      REAL FUELCO2  ! CO2 emission rating for fuel
      REAL SYSEFFC  ! cooling mechanism COP for cooling system
      REAL SYSEFF   ! heating or cooling system seasonal efficiency or
                    ! coefficient of performance
      REAL DHWEFF   ! DHW generator efficiency before 1998 array index 1
                    ! and after 1998 (array index 2)
      REAL BLDIF    ! building improvement factor
      REAL BLDLZC   ! building low and zero carbon benchmark

