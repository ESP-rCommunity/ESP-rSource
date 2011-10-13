C This header relates to the legacy plant database management
C facilities in ESP-r.
C It is dependant on plantdb.h and include statements to
C pdb_data.h should be placed after plantdb.h.

C Names and documentation.
      CHARACTER*40 ZGTYPE  ! generic component type e.g. AC junction
      CHARACTER*16 ZCDATE  ! date of insertion into db
      CHARACTER*80 ZCDESC  ! component description e.g. air mixing box
      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC

C Location data.
      integer NPC    ! number of components in database
      integer ITMLOC ! for each component 1 the start record in the
                     ! database and 2 the component code
      common/PCDATC/NPC,ITMLOC(MAXPC,2)
