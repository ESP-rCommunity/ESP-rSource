C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2010.

C profile.h holds common block for the profiles database. References
C to profile.h should follow esprdbfile.h to ensure that the
C parameters MGIT MPRFDBPER have been defined. 

      COMMON/PDBITM/NPDBITEMS,lastrec,NCG(MGIT),NO(MGIT),PDBDESC(MGIT)
      integer NPDBITEMS ! the number of profiles in the database.
      integer lastrec ! the next available record in the database.
      integer NCG  ! the number of periods in a profile.
      integer NO   ! record where profile X starts.
      character PDBDESC*40 ! description of the profile.

      COMMON/PDBELM/ICGS1(MGIT,MPRFDBPER),ICGF1(MGIT,MPRFDBPER),
     &             CGS1(MGIT,MPRFDBPER),CGL1(MGIT,MPRFDBPER)
      integer ICGS1  ! the start hour of the profile period.
      integer ICGF1  ! the finish hour of the profile period.
      real CGS1      ! the first item (e.g. sensible gain) of the period.
      real CGL1      ! the 2nd item (e.g. latent gain) of the period.


C end of profile.h
