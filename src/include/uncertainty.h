C The following is a parameter file for sensitivity related facilites.
C Used in the ESP suite of programs written by staff of the
C Energy Simulation Research Unit of the University of Strathclyde.
C For the building:
      INTEGER MNCNG ! max uncertainty distributions
      INTEGER MNIL  ! max uncertainty locations
      INTEGER MNACT ! max uncertainty distr-loc linkages
      PARAMETER (MNCNG=50)
      PARAMETER (MNIL=50)
      PARAMETER (MNACT=50)

      character*72 LUALF  ! uncertainty library file
      character*24 LCNG   ! uncertainty distribution name
      character*15 LLOC   ! uncertainty location name
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)

C Common blocks associated with sensitivity studies.
      integer NILOC  ! number of uncertainty locations
      integer NZGU   ! for each location how many zones
      integer NZNOGU ! for each location array of associated zones
      integer NSGU   ! for each associated zone how many surfaces
      integer NSNOG  ! for each location array of associated surfaces
      integer NTGU   ! for each location blind TMC type and period
      COMMON/UA2/NILOC,NZGU(MNIL),NZNOGU(MNIL,MCOM),NSGU(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS),NTGU(MNIL,MCOM,2)

      integer NICNG  ! number of uncertainty distributions (e.g. +-)
      integer IDTYPU ! distribution type: 1=materials, 2=MLC, 
                     ! 3=weather, 4=operations, 5=hc coef, 101=geometry
      integer IDMAT  ! for each distribution type (?,1) & (?,2) specific focus,
                     ! (?,3) change (%; +/-; explicit)
      real DATU      ! for each distribution the low and high value 
      COMMON/UA3/NICNG,IDTYPU(MNCNG),IDMAT(MNCNG,3),DATU(MNCNG,2)

      integer NIACT  ! number of links between distributions and locations
      integer IACTD  ! for each distribution the associated location
      COMMON/UA4/NIACT,IACTD(MNACT,2)
