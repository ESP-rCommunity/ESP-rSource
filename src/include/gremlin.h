C *** PARAMETERS

C Latest gremlin specification file version.
      INTEGER, PARAMETER :: GRMVSN=1

C Maximum number of gremlins.
      INTEGER, PARAMETER :: MGRM=10

C Number of gremlin models.
            INTEGER, PARAMETER :: NGRMMOD=2

C Descriptions.
      CHARACTER*30, PARAMETER :: GRMDESC(NGRMMOD)=
     &  (/'boiler failure prototype      ',
     &    'extreme weather prototype     '/)

C Event details.
      CHARACTER*30, PARAMETER :: GRMEDTL(NGRMMOD)=
     &  (/'basic controller capacity = 0 ',
     &    'outside temperature +/-10 C   '/)

C Average frequency.
      CHARACTER*30, PARAMETER :: GRMFREQ(NGRMMOD)=
     &  (/'once per year                 ',
     &    '1 hot and 1 cold per year     '/)
     
C Criteria for event occuring.
      CHARACTER*30, PARAMETER :: GRMCRI(NGRMMOD)=
     &  (/'outside temp. < 12 C          ',
     &    'none                          '/)

C Event duration.
      CHARACTER*30, PARAMETER :: GRMDUR(NGRMMOD)=
     &  (/'1 day                         ',
     &    '3 days                        '/)

C Allowable level.
C 0  = building only
C 1  = zone only
C -1 = both
      INTEGER, PARAMETER :: GRMALVL(NGRMMOD)=
     &  (/0,0/)

C *** MISCHIEF

C Gremlin mischief types.
C 1  - boiler failure (basic control heating capacity in all zones = 0)
C 2  - heat wave (external dry bulb temperature + 10 deg. C)
C 3  - cold snap (external dry bulb temperature - 10 deg. C)

C Number of mischief types.
      INTEGER, PARAMETER :: MMCF=3 

C Complaint descriptions.
      CHARACTER*30, PARAMETER :: MCFDESC(MMCF)=
     &  (/'basic controller capacity = 0 ',
     &    'outside temperature +10 C     ',
     &    'outside temperature -10 C     '/)

C *** COMMONS

C IS_GRM - Logical flag indicating if there are valid gremlins.
C GRMLGU - Gremlin log file unit.
C GRMFIL - Path to gremlin specification file.
C GRMLOG - Path to gremlin log file.
      common/grmflg/IS_GRM,GRMLGU,GRMFIL,GRMLOG
      logical IS_GRM
      integer GRMLGU
      character GRMFIL*72
      character GRMLOG*72

C NGRM    - number of gremlins
C GRMTYP  - gremlin type
C GRMZON  - zone that the gremlin is in (0 = omnipresent)
C GRMMCF  - mischief index 
C           0 if not causing mischief
C           -2 if waiting to cause mischief
      common/gremlin/NGRM,GRMTYP(MGRM),GRMZON(MGRM),GRMMCF(MGRM)
      integer NGRM,GRMTYP,GRMZON,GRMMCF

C NMCF   - number of gremlins currently causing mischief
C MCFGRM - indices of gremlins currently causing mischief
C MCFTYP - mischief type
C MCFSTS - mischief start time step
      common/grmschf/NMCF,MCFGRM(MGRM),MCFTYP(MGRM),MCFSTS(MGRM)
      integer NMCF,MCFGRM,MCFTYP,MCFSTS

C PROB1PS - probability such that an event should occur once per simulation.
      common/grmprob/PROB1PS
      real PROB1PS


