C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2010.

C Schedule.h holds common block for project scheduled demands
C i.e. those not associated with any particular zone.  It may
C also later hold zone operations common blocks.
C References to schedule.h should follow building.h so that
C the parameters MDTY and MC have been defined.

C Project (dispersed) demands (lifts/fans/DHW).
      COMMON/BL1/dmdsdesc,bdmds
      character  dmdsdesc*248  ! documentation of dispersed demands
      character bdmds*72       ! file name for dispersed demands

C Demand data with support for up to MDTY calendar day types
      COMMON/BL3data/NBCAS(MDTY),IBCGS(MDTY,MC),IBCGF(MDTY,MC),
     &  CBMGS(MDTY,MC),CBMGL(MDTY,MC),BRADC(MDTY,MC),BCONC(MDTY,MC),
     &  IBCGT(MDTY,MC)
      integer NBCAS       ! number of period for each day type
      integer IBCGS,IBCGF ! start and finish hours of each demand
      real CBMGS,CBMGL    ! sensible and latent magnitude (Watts) of each demand
      real BRADC,BCONC    ! radiant and convective portions of each demand
      integer IBCGT
C ibcgt is type of gain:
C  1. Occup, 2. Lighting, 3. Small power, 4. Fans, 5. Pumps, 6. Lifts, 7. DHW
C -1. Occup w/m2, -2. Lighting w/m2, -3. Sml power w/m2, -4 Fan per w/m2,
C -5. Pumps per w/m2  -6. Lifts w/m^2, -7. DHW w/m^2

C Descriptive label for a zone load or casual gain types.
C blodlabel(1)='Occup',(2)='Lights',(3)='SmPowr',(4)='Fans',(5)='Pumps',(6)='Lifts',(7)='DHW'     
      common/bloadlabel/blodlabel(7)
      character*6 blodlabel

      common/b3ver/idmdver
      integer idmdver   ! version of the demands file.
                        ! idmdver =0 standard, idmdver =1 with header

C end of schedule.h
