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
      
      integer NXTREC ! next available record for components
      common/PCCTL/NXTREC

C Lists.
      integer INDXPR  ! array of plant component indices to sort
                      ! or list
      COMMON/PL/INDXPR(MAXPC)

C Current component inter-connection data.
      integer ictype  ! zero is single component one is meta component
                      ! two is TRNSYS component
      integer mncomp  ! for a META component the nb of components used
      integer mncon   ! for a META component the nb of comp inter-connections
      integer micode  ! for a META component the index of each component used
      integer icndat  ! for each META inter-connection five values defining
                      ! the connection ??
      real rcndat     ! for each META inter-connection three real data
      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Current component descriptors. 
C << some of this might become arrays for all components? >>
      integer ICODE  ! the current component code
      integer INDXPC ! position within array of all plant components
      integer IRC    ! not used
      integer NNODE  ! number of nodes in the component
      integer NMATX  ! number of non-zero matrix coefficients
      integer NMISC  ! total of adata and bdata varaibles
      integer nadata ! number of A data variables
      integer nbdata ! number of B data variables
      integer ncdata ! number of C data variables
      integer napout ! number of additional ouput variables
      integer NDCON  ! for each node a connectivity value ??
      integer ISV    ! for each node signals water or air or solid etc
      integer NCPOS  ! position of coefficient within matrix
      character dtdesc*68 ! description of A data or B data variable
      real DATAMS    ! value of A data or B data variables
      real range     ! minimum and maximum for A data or B data variables
      character adodsc*30  ! additional output variable description
      integer noutyp ! additional output variable type ??
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)

