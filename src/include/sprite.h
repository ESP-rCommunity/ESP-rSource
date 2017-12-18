C *** PARAMETERS

C Maximum number of sprites.
      INTEGER, PARAMETER :: MSPR=10

C *** ENUMERATORS

C Sprite complaints.
C 1 - too warm
C 2 - too cold
C 3 - too many complaints
      INTEGER, PARAMETER :: MPROB=3

C *** COMMONS

C NSPR   - number of sprites
C SPRTYP - sprite type (person model)
C SPRZON - zone that the sprite is in (0 = omnipresent)
      common/sprite/NSPR,SPRTYP(MSPR),SPRZON(MSPR)
      integer NSPR,SPRTYP,SPRZON

C SPRACT - whether a sprite is active (i.e. can stop the simulation)
C CMPLTS - the number of complaints of each type an active sprite has recieved
      common/actspr/SPRACT(MSPR),CMPLTS(MSPR,MPROB)
      logical SPRACT
      integer CMPLTS

C SPRNUM - sprite number that has called the halt
C PRBNUM - problem number for the halt
C SUPDAT - supplementary data for the halt, e.g. what problem number 
C          the sprite has recieved too many of
      common/sprout/SPRNUM,PRBNUM,SUPDAT
      integer SPRNUM,PRBNUM,SUPDAT


