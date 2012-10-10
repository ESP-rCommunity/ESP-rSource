C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2010-2012.

C Schedule.h holds common block for project scheduled demands
C i.e. those not associated with any particular zone.  It also
C hold zone operations common blocks.
C References to schedule.h should follow building.h so that
C the parameters MDTY, MGTY and MC have been defined.

C Project (dispersed) demands (lifts/fans/DHW).
      COMMON/BL1/dmdsdesc,bdmds
      character  dmdsdesc*248  ! documentation of dispersed demands
      character bdmds*72       ! file name for dispersed demands

C Demand data with support for up to MDTY calendar day types
      integer NBCAS       ! number of period for each day type
      integer IBCGS,IBCGF ! start and finish hours of each demand
      real CBMGS,CBMGL    ! sensible and latent magnitude (Watts) of each demand
      real BRADC,BCONC    ! radiant and convective portions of each demand
      integer IBCGT       ! ibcgt is type of gain:
C  1. Occup, 2. Lighting, 3. Small power, 4. Fans, 5. Pumps, 6. Lifts, 7. DHW
C -1. Occup w/m2, -2. Lighting w/m2, -3. Sml power w/m2, -4 Fan per w/m2,
C -5. Pumps per w/m2  -6. Lifts w/m^2, -7. DHW w/m^2
      COMMON/BL3data/NBCAS(MDTY),IBCGS(MDTY,MC),IBCGF(MDTY,MC),
     &  CBMGS(MDTY,MC),CBMGL(MDTY,MC),BRADC(MDTY,MC),BCONC(MDTY,MC),
     &  IBCGT(MDTY,MC)

C Descriptive label for a zone load or casual gain types.
C blodlabel(1)='Occup',(2)='Lights',(3)='SmPowr',(4)='Fans',(5)='Pumps',(6)='Lifts',(7)='DHW'     
      character*6 blodlabel
      common/bloadlabel/blodlabel(MGTY)

C Does each gain type on each day:
C   start at zero dmdload24(gaintype,daytype,1)=.true.
C   and end at 24 hours (dmdload24(gaintype,daytype,2)=.true.
      logical dmdload24
      common/dmdloadall/dmdload24(MGTY,MDTY,2)

C dmdloadmixed(gaintype,daytype) - if zero then nothing, if one
C   then all non-zero are absolute, if two then all (non-zero) are in
C   W/m2 or person/m2, if three then non-zero are mixed.

C How many of each dispersed demands for each gaintype/daytype.
      integer dmdloadcount   ! nb of each gain type (whether absolute or W/m2).
      integer dmdloadm2count ! nb of each m2 gain types
      integer dmdloadmixed   ! zero is nothing, one is all non-zero are absolute, 
                             ! two then all (non-zero) are in W/m2 or person/m2, 
                             ! three then non-zero are mixed.
      common/dmdsloadcnt/dmdloadcount(MGTY,MDTY),
     &  dmdloadm2count(MGTY,MDTY),dmdloadmixed(MGTY,MDTY)

      common/b3ver/idmdver
      integer idmdver   ! version of the demands file.
                        ! idmdver =0 standard, idmdver =1 with header

C Version of zone operations file. ip3ver=0 legacy, =1 sorted with header,
C =2 with more than 3 day types allowed and periods sorted,
C =3 with key words for casual gain types.
      integer ip3ver
      common/p3ver/ip3ver(MCOM)

      character oprdesc*248  ! notes for casual gain
      character ventdesc*248 ! notes for air schedules
      character ctlstr*24    ! summary of air control scheme
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

      character lodlabel*6  ! use label for each zone casual gain
      common/loadlabel/lodlabel(mcom,MGTY)

C Users can track a number of different casual gains up to MGTY and ICGT 
C is this index for each period in each calendar day type. Older files 
C implied a purpose to this index
C 1=occupancy W, 2=lighting W, 3=small power W, 4=other W, 5=external electrical
C -1=floor area/person -2 Light W/m2 -3=small power W/m2
      integer ICGT  ! position within array of casual gains (overloaded see above)
      COMMON/P3TYPEN/ICGT(MDTY,MC)

C end of schedule.h
