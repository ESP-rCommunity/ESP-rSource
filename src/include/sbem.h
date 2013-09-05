C Project specific *.ncm file variables

C NCM Array sizing integers
      integer MFT, MPT, MHT, MHC, MCS, MSS, MBT, MREG, MSTG, MSBT
      integer MDW, MWS, MNS, MAC, MACL, MILS, ITWOMNS, MREN, MCLM
      PARAMETER (MFT=12)    ! max. fuel types
      PARAMETER (MPT=32)    ! max. system types
      PARAMETER (MHT=26)    ! max. HVAC system types

      PARAMETER (MHC=5)     ! max. system cooling types

C max. cooling mechanism COP 8 values for the first three coolers and 
C one each for the last two
      PARAMETER (MCS=3*8+2) 
      PARAMETER (MSS=3)     ! max. building services strategies

      PARAMETER (MBT=29)    ! max. building types 
      PARAMETER (MREG=3)    ! max. building regulations to follow
      PARAMETER (MSTG=2)    ! max. building design stages
      PARAMETER (MSBT=3)    ! max. Scottish building types
      PARAMETER (MDW=5)     ! max. DHW generators
      PARAMETER (MWS=6)     ! max. fuel types for DHW generators
      
C WARNING: MNS should be at least as big as MCOM in building.h.
      PARAMETER (MNS=72)    ! max. HVAC and DHW systems allowed in model
                            ! max. activities allowed in the model
                            ! This is kept equal to max zones normally allowed
      PARAMETER (MAC=65)    ! max. activity types 
      PARAMETER (MACL=530)  ! max. activity types (global list) 
      PARAMETER (MILS=10)   ! max. lighting types
      PARAMETER (ITWOMNS=2*MNS) ! Two times the max. activities allowed
      PARAMETER (MREN=10)   ! max. renewable energy systems in model
      PARAMETER (MCLM=16)   ! max. climate files insolation information 
                            ! to be held

C Integers
      INTEGER IBRUKLH   ! BRUKL HVAC-HEAT index number
      INTEGER ISYSAPP   ! hash table with elements equal to 1 if that heating or
                        ! cooling generator is applicable to that HVAC system
      INTEGER IATYPNDX  ! Activity type index number hash table
      INTEGER IBRUKLC   ! BRUKL HVAC-COOL index number
      INTEGER IBRUKLW   ! BRUKL HVAC-DHW index number (domestic hot water)
      INTEGER IBRUKLF   ! BRUKL HVAC-SFP index number (specific fan power)
      INTEGER IBTYPNDX  ! Building type index number
      INTEGER IBTYP     ! this matches the index value in NCM guide table 4

C Following variables are used for inferring air changes per hour
C from air permeability values using CIBSE data
      INTEGER ICIBSESTOR ! Number of storeys
      INTEGER ICIBSEAREA ! Floor area per storey

      COMMON/SBEM01/IBRUKLH(MFT,MPT,MHT),ISYSAPP(MPT,MHT),
     & IATYPNDX(MAC,MBT),IBRUKLC(MHC,MHT),IBRUKLW(MDW,MWS),
     & IBRUKLF(MHT),IBTYPNDX(MBT),IBTYP,ICIBSESTOR(8,4),
     & ICIBSEAREA(8,4)

C Strings
      CHARACTER*42 FUELNAME ! Name of fuel used
      CHARACTER*60 SYSNAME  ! heating or cooling system name
      CHARACTER*60 TEMPZS   ! heating or cooling system name as written in BRUKL and EPCGen input
                            ! files (slight modification of SYSNAME in certain cases)
      CHARACTER*70 HSYSNAME ! HVAC system name
      CHARACTER*25 DHWGEN   ! DHW generator name
      CHARACTER*40 BLDSS    ! building services strategy
      CHARACTER*50 BTYPNAME ! Building type name
      CHARACTER*40 SBTYP    ! Scottish building type
      CHARACTER*50 BLDREG   ! Building regulations to follow
      CHARACTER*12 BLDSTG   ! Building design stage (as built or as designed)
      CHARACTER*48 ATYPNAME ! Activity type name
      CHARACTER*70 TLIGHT   ! Lighting type name
      CHARACTER*42 SCLMTNM  ! Climate location/file name

C Following variables are used for inferring air changes per hour
C from air permeability values using CIBSE data
      CHARACTER*14 CIBSEBLDN ! CIBSE table building name
      CHARACTER*34 CIBSEBLDD ! CIBSE table building description
      CHARACTER*54 CIBSESTR1 ! concatenation of above two

      COMMON/SBEM02/FUELNAME(MFT),SYSNAME(MPT),HSYSNAME(MHT),BLDSS(MSS),
     & DHWGEN(MDW),BTYPNAME(MBT),SBTYP(MSBT),BLDREG(MREG),BLDSTG(MSTG),
     & ATYPNAME(MAC),TLIGHT(MILS),SCLMTNM(MCLM),TEMPZS(MPT),
     & CIBSEBLDN(8),CIBSEBLDD(8),CIBSESTR1(8)

C Real numbers
      REAL FUELCO2  ! CO2 emission rating for fuel
      REAL SYSEFFC  ! cooling mechanism COP for cooling system
      REAL DHWEFF   ! DHW generator efficiency before 1998 array index 1
                    ! and after 1998 (array index 2)
      REAL SYSEFF   ! heating or cooling system seasonal efficiency or
                    ! coefficient of performance
      REAL BLDIF    ! building improvement factor
      REAL BLDLZC   ! building low and zero carbon benchmark
      REAL PDRL     ! power density (W/m2)/100lux for lighting types PDRL(?,1)
                    ! for commercial & PDRL(?,2) for industrial use
      REAL VERSBEM  ! SBEM db version no.
      REAL SFPDEF   ! default specific fan power (units?)
      REAL SLRINSO  ! Solar insolation at various inclinations/orientations
      REAL SAWS     ! sum of cubes of hourly wind speed data 
      REAL AAWS     ! Average annual wind speed

C Following variable is used for inferring air changes per hour
C from air permeability values using CIBSE data. The array is sized by number of
C tables in CIBSE guide by number of permeability values given in the same table
      REAL CIBSEACH ! air change per hour value

      COMMON/SBEM03/FUELCO2(MFT),SYSEFFC(MCS,MHT),DHWEFF(MDW,2),
     & SYSEFF(MPT),BLDIF(MSS),BLDLZC(MSS),PDRL(MILS,2),VERSBEM,SFPDEF,
     & SLRINSO(MCLM,56),SAWS(MCLM),AAWS(MCLM),CIBSEACH(8,4,5)

C Integers
      INTEGER IBRUKH    ! BRUKL number of HVAC system (model specific 
      INTEGER IBRUKC    ! as opposed to being read from database)
      INTEGER IBRUKW    ! 
      INTEGER IBRUKF    !
      INTEGER IHGEF     ! index of heat generator (read from SBEM db)
      INTEGER ICGEF     ! index of cool generator (read from SBEM db)
      INTEGER IFTYP     ! index number of fuel type for heat generator
      INTEGER IHLZ      ! zone to HVAC system coupling index
      INTEGER IDHWS     ! index of DHW generators in the model
      INTEGER IDHFL     ! index of fuel type for DHW generator
      INTEGER INCMSYS   ! Index of HVAC system type (read from SBEM db)
      INTEGER IDHWLZ    ! zone to DHW coupling index, 0 if no DHW in that zone
                        ! +N if coupled with DHW system number N
                        ! -N if coupled with HVAC system number N
      INTEGER IACTYTYP  ! activity type index for each zone
      INTEGER ILITYP    ! lighting type index for each zone
      INTEGER IDHWSS    ! 0 if no DHW storage system or circulation loop are present
                        ! 1 if DHW storage system but no circulation loop are present
                        ! 2 if DHW storage system and circulation loop are present
      INTEGER ILIGHTUSER ! flag for user defined lighting wattage
      INTEGER IVENT     ! Ventilation strategy for this system 
                        ! 0 = natural
                        ! 1 = centralised balanced mech vent system (notional SFP=2W/l/s)
                        ! 2 = zonal supply system with remote fan (notional SFP=1.2)
                        ! 3 = zonal extract system with remote fan (notional SFP=0.8)
                        ! 10+IVENT = local ventilation only units (notional SFP=0.5) present
                        ! Default value 0. See NCM modelling guide 2008 version 
                        ! tables 7 and 12 for more details 
      INTEGER ISBEM     ! a value of 1 signals that isbem db exists 2 signals NCM file exists
      INTEGER IBUSERTYP ! this matches the list of buildings in isbem.
      INTEGER IBSS      ! building service strategy
      INTEGER IRGG      ! building regulations to follow
      INTEGER ISTG      ! building design stage
      INTEGER ISBT      ! Scottish building types
      INTEGER NCMSYS    ! total number of HVAC system types in model
      INTEGER INOTI     ! if set to 1 this is a notional model 
                        ! if set to 2 this is a reference model 
                        ! if set to 3 this is a typical model 
                        ! if set to 4 this is a stripped model 
      INTEGER NDHWSYS   ! total number of DHW generators in model
      integer theactivityindex   ! the long list (up to 525) activity index for each zone.
      INTEGER NREN      ! Number of renewable energy systems in model
      INTEGER NRENTYPE  ! Type of renewable energy system
                        !  1 is solar thermal collector (only used to feed DHW)
                        !  2 is solar photovoltaic collector
                        !  3 is wind turbine
                        !  4 is CHP
      INTEGER Y_inspect ! year for which the energy assessor inspected the building
      INTEGER M_inspect ! month for which the energy assessor inspected the building
      INTEGER D_inspect ! day for which the energy assessor inspected the building
      INTEGER S_Yinsur  !Specifies the year for which the energy assessor's 
                        !insurance policy number effectively started
      INTEGER S_Minsur  !Specifies the month for which the energy assessor's 
                        !insurance policy number effectively started                         
      INTEGER S_Dinsur  !Specifies the day for which the energy assessor's 
                        !insurance policy number effectively started
      INTEGER E_Yinsur  !Specifies the year for which the energy assessor's 
                        !insurance policy number effectively started
      INTEGER E_Minsur  !Specifies the month for which the energy assessor's 
                        !insurance policy number effectively started                         
      INTEGER E_Dinsur  !Specifies the day for which the energy assessor's 
                        !insurance policy number effectively started
      INTEGER pi_limit  !Specifies the the energy assessor's insurance
                        !policy cover limit
      COMMON/SBEM04/IBRUKH(MNS),IBRUKC(MNS),IBRUKW(MNS),IBRUKF(MNS),
     &IHGEF(MNS),ICGEF(MNS),IFTYP(MNS),IHLZ(MNS),IDHWS(MNS),IDHFL(MNS),
     &INCMSYS(MNS),IDHWLZ(MNS),IACTYTYP(MNS),ILITYP(MNS),IDHWSS(MNS),
     &ILIGHTUSER(MNS),IVENT(MNS),
     &ISBEM,IBUSERTYP,IBSS,IRGG,ISTG,ISBT,NCMSYS,INOTI,
     &NDHWSYS,theactivityindex(MNS),NREN,NRENTYPE(MREN),
     &Y_inspect,M_inspect,D_inspect,S_Yinsur,S_Minsur,S_Dinsur,E_Yinsur,
     &E_Minsur,E_Dinsur,pi_limit

C Strings
      CHARACTER*30 DHWNAME  ! Names of DHW generators in the model
      CHARACTER*30 HVACNAME ! Names of HVAC systems in the model
      CHARACTER*72 LASBEM       ! SBEM project specific file name (*.ncm)
      CHARACTER*3  SBREF        ! Scottish Accredited Construction Details followed (yes/no)
      CHARACTER*3  APCHK        ! Compliance of air permeabilty to be checked (yes/no)
      CHARACTER*20 PJNAME       ! Project name
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
      CHARACTER*24 RENNAME      ! Names of renewable energy systems
      CHARACTER*64 UPRN         ! Unique Property Reference Number
      CHARACTER*64 Accr_Scheme  ! Accreditation Scheme of the energy 
                                !assessor (E&W and NI only)
      CHARACTER*64 assessRegNumber   !Assessor registration number (E&W and NI only)
      CHARACTER*64 empl_Trading_name !Employer or trading name of energy
                                     !assessor (E&W and NI only)
      CHARACTER*64 addr_empl_Trading !Employer or trading address of energy
                                     !assessor (E&W and NI only)
      CHARACTER*64 party_disclosure  !Specifies any related party disclosure
                                     ! by the energy assessor (E&W and NI only)    
      CHARACTER*64 qualifications_assessor !Specifies the qualifications of
                                           !the energy assessor. There are 3choices:
                                           !NOS3, NOS4 and NOS5 (E&W and NI only)           
      CHARACTER*64 insurer_Company !Specifies the energy assessor's insurance company
      CHARACTER*64 policyNumberInsurance !Specifies the energy assessor's insurance 
                                         !policy number                              
      CHARACTER*64 pjLevComplexity !Specifies the level of complexity of the project
                                   !4 choices: 'Undefined','Level 3','Level 4' and 'Level 5'   
      COMMON/SBEM05/DHWNAME(MNS),HVACNAME(MNS),LASBEM,SBREF,APCHK,
     & PJNAME,BADDRESS,BCITY,OWNERNAME,OWNERTEL,BUILDINGPC,OWNERADDR,
     & OWNERCITY,OWNERPC,CERTIFNAME,CERTTEL,CERTADDRESS,CERTCITY,
     & CERTPC,RENNAME(MREN),UPRN,Accr_Scheme,assessRegNumber,
     & empl_Trading_name,addr_empl_Trading,party_disclosure,
     & qualifications_assessor,insurer_Company,policyNumberInsurance,
     & pjLevComplexity 

C Real numbers
      REAL ROOFFR   ! roof glazing fraction required
      REAL WALLFR   ! wall glazing fraction required
      REAL BINF50   ! building's permeability at 50Pa in m3/m2.hour
      REAL ROOFFRFR ! roof window frame fraction required
      REAL WALLFRFR ! wall window frame fraction required
      REAL HGEF     ! heat generator efficiency/COP
      REAL CGEF     ! cool generator efficiency/COP
      REAL SFPHS    ! Specific fan power SFP for ventilation W/l/s
      REAL HWEF     ! DHW generator efficiency
      REAL ACH      ! Air changes per zone
      REAL LIGHTWATTAGE ! Heat gains (Watts) per zone in the cases of user specifies them
      REAL RENDATA  ! Renewable energy system supplementary information
                    ! See mksbem.F for detail of individual system types
      REAL DEADLEG  ! Deadleg length of water draw off point in each zone
      REAL DHWSPD   ! DHW system supplementary data
                    ! 1 -> volume of storage tank (m3)
                    ! 2 -> system losses (MJ/month)
                    ! 3 -> circulation losses (W/m)
                    ! 4 -> pump power (kW)
                    ! 5 -> loop length (m)
      COMMON/SBEM06/ROOFFR,WALLFR,BINF50,ROOFFRFR,WALLFRFR,HGEF(MNS),
     & CGEF(MNS),SFPHS(MNS),HWEF(MNS),ACH(MNS),LIGHTWATTAGE(MNS),
     & RENDATA(MREN,6),DEADLEG(MNS),DHWSPD(MNS,5)

      REAL BERHEAT  ! heating energy of stripped building
      REAL BERCOOL  ! cooling energy of stripped building
      REAL BERLIGHT ! lighting energy of stripped building 
      REAL BERDHW   ! DHW energy of stripped building (kWh)
      REAL BERAUX   ! auxiliary energy of stripped building
      REAL AERHEAT  ! heating energy of notional building
      REAL AERCOOL  ! cooling energy of notional building
      REAL AERLIGHT ! lighting energy of notional building 
      REAL AERDHW   ! DHW energy of notional building
      REAL AERAUX   ! auxiliary energy of notional building
      REAL BERHEATM ! monthly heating energy of stripped building
      REAL AERHEATM ! monthly heating energy of notional building
      REAL BERCOOLM ! monthly cooling energy of stripped building
      REAL AERCOOLM ! monthly cooling energy of notional building
      REAL BERF     ! total annual energy for stripped building as
                    ! supplied by each fuel type
      REAL AERF     ! total annual energy for notional building as
                    ! supplied by each fuel type
      REAL RERF     ! total annual energy for reference building as
                    ! supplied by each fuel type
      REAL TyERF     ! total annual energy for typical building as
                    ! supplied by each fuel type
      REAL BERSH    ! Heating energy for stripped building as
                    ! supplied by each HVAC system
      REAL AERSH    ! Heating energy for notional building as
                    ! supplied by each HVAC system
      REAL BERSC    ! Cooling energy for stripped building as
                    ! supplied by each HVAC system
      REAL AERSC    ! Cooling energy for notional building as
                    ! supplied by each HVAC system
      REAL BERSA    ! Auxiliary energy for stripped building as
                    ! supplied by each HVAC system
      REAL AERSA    ! Auxiliary energy for notional building as
                    ! supplied by each HVAC system
      REAL BERSH_DEMAND ! As BERSH * heat generator efficiency
      REAL BERSC_DEMAND ! As BERSC * cool generator efficiency
      REAL AERSH_DEMAND ! As AERSH * heat generator efficiency
      REAL AERSC_DEMAND ! As AERSC * cool generator efficiency
      REAL RERSH    ! Heating energy for reference building as
                    ! supplied by each HVAC system
      REAL RERSC    ! Cooling energy for reference building as
                    ! supplied by each HVAC system
      REAL RERSA    ! Auxiliary energy for reference building as
                    ! supplied by each HVAC system
      REAL TyERSH    ! Heating energy for typical building as
                     ! supplied by each HVAC system
      REAL TyERSC    ! Cooling energy for typical building as
                     ! supplied by each HVAC system
      REAL TyERSA    ! Auxiliary energy for typical building as
                     ! supplied by each HVAC system  
      REAL BERHEAT_afterefficiency !Same as BERHEAT but by applying efficiency
      REAL BERCOOL_afterefficiency !Same as BERCOOL but by applying efficiency     
      REAL BERDHW_afterefficiency !Same as BERDHW but by applying efficiency     
      REAL AERHEAT_afterefficiency !Same as AERHEAT but by applying efficiency
      REAL AERCOOL_afterefficiency !Same as AERCOOL but by applying efficiency     
      REAL AERDHW_afterefficiency !Same as AERDHW but by applying efficiency    
      REAL RERHEAT_afterefficiency !Same as RERHEAT but by applying efficiency
      REAL RERCOOL_afterefficiency !Same as RERCOOL but by applying efficiency     
      REAL RERDHW_afterefficiency !Same as RERDHW but by applying efficiency    
      REAL TyERHEAT_afterefficiency !Same as TyERHEAT but by applying efficiency
      REAL TyERCOOL_afterefficiency !Same as TyERCOOL but by applying efficiency     
      REAL TyERDHW_afterefficiency !Same as TyERDHW but by applying efficiency        
      COMMON/SBEM07/BERHEAT(MNS),BERCOOL(MNS),
     & BERLIGHT(MNS),BERDHW(MNS),BERAUX(MNS),AERHEAT(MNS),
     & AERCOOL(MNS),AERLIGHT(MNS),AERDHW(MNS),AERAUX(MNS),
     & BERHEATM(MNS,12),AERHEATM(MNS,12),BERCOOLM(MNS,12),
     & AERCOOLM(MNS,12),BERF(MFT),AERF(MFT),RERF(MFT),TyERF(MFT),
     & BERSH(MNS),AERSH(MNS),BERSC(MNS),AERSC(MNS),BERSA(MNS),
     & AERSA(MNS),BERSH_DEMAND(MNS),BERSC_DEMAND(MNS),AERSH_DEMAND(MNS),
     & AERSC_DEMAND(MNS),RERSH(MNS),RERSC(MNS),RERSA(MNS),TyERSH(MNS),
     & TyERSC(MNS),TyERSA(MNS),BERHEAT_afterefficiency(MNS),
     & BERCOOL_afterefficiency(MNS),BERDHW_afterefficiency(MNS),
     & AERHEAT_afterefficiency(MNS),AERCOOL_afterefficiency(MNS),
     & AERDHW_afterefficiency(MNS),RERHEAT_afterefficiency(MNS),
     & RERCOOL_afterefficiency(MNS),RERDHW_afterefficiency(MNS),
     & TyERHEAT_afterefficiency(MNS),TyERCOOL_afterefficiency(MNS),
     & TyERDHW_afterefficiency(MNS)
        
C BER_* is energy in kWh/m^2 (after including efficiency of HVAC systems) and 
C CO2 emissions associated with stripped building

C AER_* is energy (after including efficiency of HVAC systems) and 
C CO2 emissions associated with notional building

C RER_* is energy (after including efficiency of HVAC systems) and 
C CO2 emissions associated with reference building. Most of these
C are local variables in mksbem.F subroutine BERTER.

C TyER_* is energy (after including efficiency of HVAC systems) and 
C CO2 emissions associated with typical building. Most of these
C are local variables in mksbem.F subroutine BERTER.
      REAL BER_HEAT,BER_HEATCO2,BER_COOL,BER_COOLCO2,
     & BER_AUX,BER_AUXCO2,BER_LIGHT,BER_LIGHTCO2,BER_DHW,BER_DHWCO2,
     & AER_HEAT,AER_HEATCO2,AER_COOL,AER_COOLCO2,AER_AUX,AER_AUXCO2,
     & AER_LIGHT,AER_LIGHTCO2,AER_DHW,AER_DHWCO2,DHWCO2RATE,
     &RER_HEAT,RER_COOL,RER_DHW,RER_AUX,RER_HEATCO2,RER_COOLCO2,
     &RER_AUXCO2,TyER_HEAT,TyER_COOL,TyER_AUX,TyER_LIGHT,
     &TyER_DHW,TyER_HEATCO2,TyER_COOLCO2,TyER_AUXCO2,TyER_LIGHTCO2,
     &TyER_DHWCO2   
      COMMON/SBEM08/BER_HEAT,BER_HEATCO2,BER_COOL,BER_COOLCO2,
     & BER_AUX,BER_AUXCO2,BER_LIGHT,BER_LIGHTCO2,BER_DHW,BER_DHWCO2,
     & AER_HEAT,AER_HEATCO2,AER_COOL,AER_COOLCO2,AER_AUX,AER_AUXCO2,
     & AER_LIGHT,AER_LIGHTCO2,AER_DHW,AER_DHWCO2,DHWCO2RATE,
     & RER_HEAT,RER_COOL,RER_DHW,RER_AUX,RER_HEATCO2,RER_COOLCO2,
     & RER_AUXCO2,TyER_HEAT,TyER_COOL,TyER_AUX,TyER_LIGHT,TyER_DHW,
     & TyER_HEATCO2,TyER_COOLCO2,TyER_AUXCO2,TyER_LIGHTCO2,TyER_DHWCO2
      
      CHARACTER*72 LCFGF_O      ! Original cfg file name
      CHARACTER*24 CFGROOT_O    ! original cfg root name
      CHARACTER*24 CFGROOT_S    ! stripped cfg root name
      CHARACTER*24 CFGROOT_N    ! notional cfg root name
      CHARACTER*24 CFGROOT_R    ! referemce cfg root name
      CHARACTER*24 CFGROOT_T    ! typical cfg root name
      CHARACTER*72 LGEOM_O      ! Original geometry file names
      CHARACTER*72 LCFGF_S      ! Stripped cfg file name
      CHARACTER*72 LCFGF_N      ! Notional cfg file name
      CHARACTER*72 LCFGF_R      ! Reference cfg file name
      CHARACTER*72 LCFGF_T      ! Typical cfg file name
      CHARACTER*72 LSNAM_O      ! Original model title
      COMMON/SBEM09/LCFGF_O,CFGROOT_O,CFGROOT_S,CFGROOT_N,CFGROOT_R,
     & CFGROOT_T,LGEOM_O(MNS),LCFGF_S,LCFGF_N,LCFGF_R,LCFGF_T,LSNAM_O

      INTEGER NAR    ! Building asset rating
      INTEGER MHF    ! Main heating fuel index in stripped model
      INTEGER NHF    ! Heating fuel index in notional model
      COMMON/SBEM10/NAR,MHF,NHF

      REAL DHWNGE    ! DHW efficiency for notional model
      REAL CoolSSEER_N,CoolSSEER_T,CoolSSEER_R ! Cooling COP for notional/typ/ref model
      REAL HeatSCoP_N,HeatSCoP_T,HeatSCoP_R    ! Heating efficiency for notional/typ/ref model
      COMMON/SBEM11/DHWNGE,CoolSSEER_N(MNS),CoolSSEER_T(MNS),
     & CoolSSEER_R(MNS),HeatSCoP_N(MNS),HeatSCoP_T(MNS),HeatSCoP_R(MNS)

      REAL BER       ! CO2 emission for stripped building
      REAL AER       ! CO2 emission for notional building
      REAL TER       ! Target CO2 emission 
      REAL RER       ! Reference CO2 emission 
      REAL SER       ! Standard CO2 emission 
      REAL BERE      ! total annual energy for stripped building
      REAL AERE      ! total annual energy for notional building
      REAL FXIF      ! Area weighted improvement factor
      REAL XLZC      ! Area weighted low and zero carbon technologies benchmark
      REAL TyER      ! Typical CO2 emission 
      COMMON/SBEM12/BER,AER,TER,RER,SER,BERE,AERE,FXIF,XLZC,TyER

      character*72 roomactname ! the quoted string naming the activity
      character*124 roomactdoc ! a quote description of activity
      common/actglobtext/roomactname(MACL),roomactdoc(MACL)

      REAL RERCOOL   ! cooling energy of reference building
      REAL RERCOOLM  ! monthly cooling energy of reference building
      REAL RERHEAT   ! heating energy of reference building
      REAL RERHEATM  ! monthly heating energy of reference building
      REAL RERAUX    ! auxiliary energy of reference building
      REAL RERDHW    ! hot water energy of reference building
      REAL TyERHEAT  ! heating energy of stripped building
      REAL TyERCOOL  ! cooling energy of stripped building
      REAL TyERLIGHT ! lighting energy of stripped building 
      REAL TyERDHW   ! DHW energy of stripped building
      REAL TyERAUX   ! auxiliary energy of stripped building
      REAL TyERHEATM ! monthly heating energy of stripped building
      REAL TyERCOOLM ! monthly cooling energy of stripped building
      COMMON/SBEM14/RERCOOL(MNS),RERCOOLM(MNS,12),RERHEAT(MNS),
     & RERHEATM(MNS,12),RERAUX(MNS),RERDHW(MNS),TyERHEAT(MNS),
     & TyERCOOL(MNS),TyERLIGHT(MNS),TyERDHW(MNS),TyERAUX(MNS),
     & TyERHEATM(MNS,12),TyERCOOLM(MNS,12)

C Recommendation report integer
      INTEGER iRecControls !flags to link HVAC and control systems
      COMMON/SBEMMISCREC/iRecControls(5,MNS)

C Recommendations report variables 
      REAL EFFORIGINAL    !Variable to temporary pass the original values (e.g. efficiency) before recalculating BER
      REAL BERORIGINAL    !Variable to temporary pass the original values of BER before recalculating a new BER
      REAL EFFDHWORIGINAL !Variable to temporary pass the original values of DHW 
                          !efficiency before recalculating a new BER
      REAL LIGHTORIGINAL  !Variable to temporary pass the original values of BERLIGHT 
                          !before recalculating a new BER. This should be actually sized
                          !per zone and not for MNS as it is for the moment
      REAL COOLORIGINAL   !Variable to temporary pass the original values of bersc(icmsys)
                          !before recalculating a new BER.                      
      COMMON/SBEM13REC/EFFORIGINAL(MNS),BERORIGINAL,
     & EFFDHWORIGINAL(MNS),LIGHTORIGINAL(MNS),COOLORIGINAL(MNS)
     
C Reals: used for the calculation of the auxiliary energy of the actual
C (stripped) building
      REAL duct_tDLd  ! constant for ductwork leakage: needed to calculate Auxiliary energy
      REAL AHU_tDLd   ! constant for AHU leakage: needed to calculate Auxiliary energy
      REAL totLeakage ! this is the sum of: duct_tDLd + AHU_tDLd , used in EPCCAL for recommendation C3
      REAL tAEnergy   ! Reference Auxiliary energy needed for systems that are in
                      ! operation 3255 hours per year (kWh/m2 per annum). Linear interpolation
                      ! is used to adjust this to the different activities of spaces
      REAL tASFPb     ! constant for SFP: needed to calculate Auxiliary energy
      REAL tASFPa     ! constant for SFP: needed to calculate Auxiliary energy
      REAL tADLb      ! constant for duct and AHU leakage: needed to calculate Auxiliary energy
      REAL tADLa      ! constant for duct and AHU leakage: needed to calculate Auxiliary energy 
      COMMON/AUXLEAKAGE/duct_tDLd(MNS),AHU_tDLd(MNS),totLeakage(MNS),
     & tAEnergy(MNS),tASFPb(MNS),tASFPa(MNS),tADLb(MNS),tADLa(MNS)

C Strings: names related to the classes for the leakage of the duct and AHU
      CHARACTER*24 ductwork    !ductwork leakage testing class. Available options are 4:
                               !1) Class Worse than A, 2) Class B, 3) Class A and
                               !4) No use the default leakage                                                                       
      CHARACTER*24 AHUleakage  !AHU leakage testing class. Available options are 5:
                               !1) Class Worse than L3, 2) Class L3, 3) Class L2
                               !4) Class L1 and 5) No use the default leakage      
      COMMON/AUXCLASSLEAKAGE/ductwork(MNS),AHUleakage(MNS)

C U Value maximums - calculated in BRUKL and it is also used in EPCGen
      REAL WALL_MAX_U !Maximum (for the whole building) U value for external walls
      REAL FLOR_MAX_U !Maximum (for the whole building) U value for ground floors
      REAL ROOF_MAX_U !Maximum (for the whole building) U value for roofs 
      REAL WNRF_MAX_U !Maximum (for the whole building) U value for windows or skylights
      REAL P_DR_MAX_U !Maximum (for the whole building) U value for personnel doors
      REAL V_DR_MAX_U !Maximum (for the whole building) U value for vehicle doors
      REAL H_DR_MAX_U !Maximum (for the whole building) U value for high-usage doors
      REAL flat_ROOF_MAX_U !Maximum (for the whole building) U value for flat roofs
      REAL pitched_ROOF_MAX_U !Maximum (for the whole building) U value for pitched roofs      
      REAL EXP_AREA   !Exposed area of the building (m^2)
      REAL tot_floor_area !Total floor area of the building (m^2)
      COMMON/MAX_UVALUES/WALL_MAX_U,FLOR_MAX_U,ROOF_MAX_U,WNRF_MAX_U,
     & P_DR_MAX_U,V_DR_MAX_U,H_DR_MAX_U,flat_ROOF_MAX_U,
     & pitched_ROOF_MAX_U,EXP_AREA,tot_floor_area


C Activities global list
      INTEGER bld_order_index ! building order indices from the *bldg_type_start section of the file
      INTEGER loc_man_sw      ! local manual switching flag (if 1 then local manual switching not allowed)
      INTEGER metabolic_rate  ! metabolic rate W/person/m2
      INTEGER lighting_lux    ! lighting lux
      INTEGER latent_ocup_percent  ! occupant latent percentage (of the whole gain)
      INTEGER latent_equip_percent ! equipment latent percentage (of the whole gain)
      common/actglobint/bld_order_index(MACL),loc_man_sw(MACL),
     &  metabolic_rate(MACL),lighting_lux(MACL),
     &  latent_ocup_percent(MACL),latent_equip_percent(MACL)

      REAL occupant_dens      ! occupant density people/m2
      REAL fresh_air          ! outside air per person litres/sec/person
      REAL equip_gain         ! equipment W/m2 
      REAL dhw_litres         ! domestic hot water litres/day/m2
      REAL display_lighting        ! lighting display W/m2
      REAL Hmainsetpoint,Cmainsetpoint  
      REAL fNotionalLighting
      REAL fTypicalLighting
      REAL fMonthEstSysHrs       ! for each month assumed hours of env sys use
                                 ! 13th value is annual assumed hours of env systems use      
      REAL nonzerohours          ! hours of year with non-zero occupancy/lights/equipemt
      REAL atleastonehour        ! days which have (at least some) occupancy/lights/equipment
      REAL casualfracsum         ! summation of the hourly occupant/lighting/equipment fractions
      common/actglobreal/occupant_dens(MACL),fresh_air(MACL),
     &  equip_gain(MACL),dhw_litres(MACL),display_lighting(MACL),
     &  Hmainsetpoint(MACL),Cmainsetpoint(MACL),fNotionalLighting(MACL),
     &  fTypicalLighting(MACL),fMonthEstSysHrs(MACL,13),
     &  nonzerohours(MACL,3),atleastonehour(MACL,3),
     &  casualfracsum(MACL,3)
                
C Reals not in common
C      REAL TyERE    ! total annual energy for typical building
C      REAL RERE     ! total annual energy for reference building

      real build_UA !stripped/actual building UA value for Brukl and EPCgen
      real build_UAN !notional building UA value for Brukl and EPCgen
      real b_totheatloss !buildings total steady state heat loss
                         !Calculated by adding totheatloss(izone) 
                         !as taken from geometry files                         
      common/BUILDTHRBRDG/build_UA,build_UAN,b_totheatloss

      real prcnt_Overheat !Reports the reading of save level 6 for percentage of occupied
                          !hours above 27degC
      common/prcOverhBRUKL/prcnt_Overheat(MNS)
      
      integer iDsmTestingFlag !default 0: stripped does NOT follow the rules of the notional/reference
                              !if equal to 1: stripped follows the rules of the notional building
                              !if equal to 2: stripped follows the rules of the reference building
      integer iNatVentilatedFlag !flag to indicate that the building is naturally ventilated
                                 ! =1 means naturally ventilated
      common/dsmtests/iDsmTestingFlag,iNatVentilatedFlag

C Common blocks for lighting control
      real PE_sensor_PP       ! Photoelectric sensor parasitic power W/m2
      real Occ_sensor_PP      ! Occupancy sensor parasitic power W/m2
      real DFFront            ! Front daylight factor
      real DFRear             ! Rear daylight factor
      common/sbem15/PE_sensor_PP(MNS),Occ_sensor_PP(MNS),
     &DFFront(MNS),DFRear(MNS)


      integer IOcc_sensing    ! Occupancy sensing flag 
      integer Ilightcontrol   ! lighting control type
      integer Idaylightzoning ! daylighting zoning for control
      integer Ipe_control     ! photoelectric control (switching or dimming)
      common/sbem16/IOcc_sensing(MNS),Ilightcontrol(MNS),
     &Idaylightzoning(MNS),Ipe_control(MNS)
      
      
      
      
      
      
      
      
      
      
      
      
