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

C Mass flow components data
      integer itpcmp  ! for each external component connection the
                      ! typical flow component type used
      integer isdcmp  ! for each external component connection the
                      ! flow total nb of supplemental data including
                      ! flow control data
      integer isdcnn  ! for each external component connection the
                      ! flow number of connections
      real supcmp  ! for each external component connection the flow
                   ! supplemental data e.g. fluid type
      character mfsdsc*68  ! description of each supplemental flow data
      character ltpcmp*60  ! text associated with each external comp conn
      integer isdifc  ! for each external component connection the flow
                      ! initial supplemental data defining flow comp
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C TRNSYS types data
      integer ntypes ! number of TRNSYS types in the database
      integer ittype ! TRNSYS type number for each
      integer ntadat ! number of A data in TRNSYS type
      integer ntbdat ! number of B data in TRNSYS type
      integer ntcdat ! number of C data in TRNSYS type
      integer ntapot ! number of additional output parameters
      integer ntmisc ! total of adata and bdata varaibles
      character tdtdsc*68 ! description of each supplemental data
      real tdatms    ! value of each supplemental data
      real trange    ! minimum and maximum for each supplemental data
      character*30 tadods  ! additional output variable description
      integer ntaotp ! additional output variable type ??
      integer ittpcm ! for each external component connection the
                     ! typical flow component type used
      integer itsdcm ! for each external component connection the
                     ! flow total nb of supplemental data including
                     ! flow control data
      integer itsdcn ! for each external component connection the
                     ! flow number of connections
      real tsupcm    ! for each external component connection the flow
                     ! supplemental data e.g. fluid type
      character tmfsds*68 ! description of each supplemental flow data
      character tltpcm*60 ! text associated with each external comp conn
      integer itsdif ! for each external component connection the flow
                     ! initial supplemental data defining flow comp
      common/trnsys/ntypes,ittype(MXTSYS),ntadat(MXTSYS),ntbdat(MXTSYS),
     &              ntcdat(MXTSYS),ntapot(MXTSYS),ntmisc(MXTSYS),
     &              tdtdsc(MXTSYS,maxmsc),tdatms(MXTSYS,maxmsc),
     &              trange(MXTSYS,maxmsc,2),tadods(MXTSYS,mxaout),
     &              ntaotp(MXTSYS,mxaout),ittpcm(MXTSYS,maxcon),
     &              itsdcm(MXTSYS,maxcon),itsdcn(MXTSYS,maxcon),
     &              tsupcm(MXTSYS,maxcon,maxflw),
     &              tmfsds(MXTSYS,maxcon,maxflw),
     &              tltpcm(MXTSYS,maxcon),itsdif(MXTSYS,maxcon)
