C *** PARAMETERS

C Latest agent specification file version.
      INTEGER, PARAMETER :: AGTVSN=1

C Maximum number of agents.
      INTEGER, PARAMETER :: MAGT=10

C Number of agent models.
      INTEGER, PARAMETER :: NAGTMOD=2

C Is it a management agent.
      LOGICAL, PARAMETER :: AGTMAN(NAGTMOD)=
     &  (/.false.,
     &    .true./)

C Descriptions.
      CHARACTER*30, PARAMETER :: AGTDESC(NAGTMOD)=
     &  (/'prototype occupant agent      ',
     &    'prototype management agent    '/)

C Criteria description.
      CHARACTER*30, PARAMETER :: AGTCRI(NAGTMOD)=
     &  (/'dry bulb temp. < 18 or > 25   ',
     &    '50 complaints of one type     '/)

C Allowable level.
C 0  = building only
C 1  = zone only
C -1 = both
      INTEGER, PARAMETER :: AGTALVL(NAGTMOD)=
     &  (/1,0/)

C *** COMPLAINTS

C Complaints.
C 1 - too warm
C 2 - too cold
C 3 - too many complaints

C Number of complaint types.
      INTEGER, PARAMETER :: MPROB=3 

C Complaint descriptions.
      CHARACTER*30, PARAMETER :: PROBDESC(MPROB)=
     &  (/'too warm                      ',
     &    'too cold                      ',
     &    'too many complaints           '/)

C *** COMMONS

C IS_AGT - Logical flag indicating if there are valid agents.
C AGTLGU - Agent log file unit.
C AGTFIL - Path to agent specification file.
C AGTLOG - Path to agent log file.
      common/agtflg/IS_AGT,AGTLGU,AGTFIL,AGTLOG
      logical IS_AGT
      integer AGTLGU
      character AGTFIL*72
      character AGTLOG*72

C NAGT   - number of agents
C AGTTYP - agent type (person model)
C AGTZON - zone that the agent is in (0 = omnipresent)
      common/agent/NAGT,AGTTYP(MAGT),AGTZON(MAGT)
      integer NAGT,AGTTYP,AGTZON

C AGTACT - whether an agent is management (i.e. can stop the simulation)
C CMPLTS - the number of complaints of each type a management agent has recieved
      common/actagt/CMPLTS(MAGT,MPROB)
      integer CMPLTS

C AGTNUM - agent number that has called the halt
C PRBNUM - problem number for the halt
C SUPDAT - supplementary data for the halt, e.g. what problem number 
C          the agent has recieved too many of
      common/agtout/AGTNUM,PRBNUM,SUPDAT
      integer AGTNUM,PRBNUM
      character*30 SUPDAT


