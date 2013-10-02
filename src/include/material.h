C This header relates to materials data structures in ESP-r. It is
C dependant on building.h and should follow building.h and esprdbfile.h so that
C parameters will have been defined.

C Materials header common blocks:
      real matver         ! version of the database 1.1 basic 1.2 has gap resistances
      integer matcats     ! number of categories
      integer matdbitems  ! number of items (overall) in database
      integer matcatitems ! number of items in each category
      logical origmatwasbin ! if true then initial materials file was binary
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  origmatwasbin

C Materials category commons
      character matdbdate*24 ! date stamp for the database
      character matdbdoc*248 ! documentation for the materials database
      character matcatname*32 ! identity/name of category
      character matcatdoc*248  ! documentation for the category
      common/matheadertxt/matdbdate,matdbdoc,matcatname(MGCL),
     &  matcatdoc(MGCL)

C Strings associated with a material
C matname for legacy materials the name is a combination of
C the characters 'mat_' followed by the legacy index in i3.3 format.
C Otherwise it is a user supplied short phrase (spaces are allowed).
      character matname*32 ! name for material
      character matdoc*248 ! documentation (initially filled with the 72 char from
                           ! the older name of the material.
C The variable matopaq indicates whether the material was opaque/transparent
C and/or a gas (various types). Set lower range to -1 so that the older
C convention of zero meaning air and confused materials do not result in
C out-of-bounds seg-fault.
C 'o' is opaque, 't' is transparent, '-' imported from legacy
C 'g' is a gap resistance layer, 'h' depreciated gas
      character matopaq*1  ! tag for opaque/transparent/gas
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(-1:MGIT)

C Data for each material:
      integer matlegindex  ! is the original material index
      integer matcatindex  ! is pointer to index of the material category
      real matdbcon        ! is the conductivity ()
      real matdbden        ! is the density ()
      real matdbsht        ! is the specific heat ()

C Traditionally emissivity and solar abs have been assumed to be the same
C on both faces of a material. Proposal to expand data structure to allow
C for differing values on each face.  Initially, both would be set to the
C same value. Later the source could be updated to support differing
C assumptions for each face.
      real matdboute      ! is the outside face long wave emissivity (-)
      real matdbine       ! is the inside face long wave emissivity (-)
      real matdbouta      ! is the outside face short wave absorbtivity (-)
      real matdbina       ! is the inside face short wave absorbtivity (-)
      real matdbdrv       ! is the diffusion resistance (MNs g^-1m^-1)'
      real matdbthick     ! default thickness (mm) for item (useful where
                           ! there are standard thickness of products
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

C For air gaps WIS additional data (ignored if found).
C If there are values after the tag then these might be air gap resistances.
      real matgapares ! air gap resistance (i.e. DRAIR) 1=vert 2=floor/ceil 3=other
      common/matgaparray/matgapares(MGIT,3)

C 'G-value' calculations require single layer optical properties
C (dependant on thickness vis-a-vis matdbthick). 
      real matirtran     ! IR transmittance
      real matsoldrtrn   ! solar direct transmittance
      real matsoldrotrfl ! outer solar direct reflectance
      real matsoldrinrfl ! inner solar direct reflectance
      real matvistran    ! visable transmittance
      real matvisotrfl   ! outer visable reflectance
      real matvisinrfl   ! inner visable reflectance
      real matrender     ! colour rendering index
      common/matwisarray/matirtran(MGIT),matsoldrtrn(MGIT),
     &  matsoldrotrfl(MGIT),matsoldrinrfl(MGIT),matvistran(MGIT),
     &  matvisotrfl(MGIT),matvisinrfl(MGIT),matrender(MGIT)

C mathash is a pointer to the array index in matdatarray for a
C   given legacy index e.g array_index = mathash(legacy_index)
C   if negative then there is no pointer or zero to represent
C   the implied standard air gap material.
      integer mathash
      common/mathasharray/mathash(0:600)

C To record equivalence between IPR and materials arrays.
      integer IPRMAT  ! similar to IPR but with the index to
                      ! matdatarray array rather than DB position
      common/MLCMA/IPRMAT(MMLC,ME)


C CFC layers header common blocks:
      real cfcver         ! version of the database (current version is 1.1)
      integer cfccats     ! number of categories
      integer cfcdbitems  ! number of items (overall) in database
      integer cfccatitems ! number of items in each category
      common/cfcheader/cfcver,cfccats,cfcdbitems,cfccatitems(MGCL_CFC)

C CFC layers category commons
      character cfcdbdate*24 ! date stamp for the database
      character cfcdbdoc*248 ! documentation for the CFC layers database
      character cfccatname*124 ! identity/name of category
      character cfccatdoc*248  ! documentation for the category
      common/cfcheadertxt/cfcdbdate,cfcdbdoc,cfccatname(MGCL_CFC),
     &  cfccatdoc(MGCL_CFC)

C Strings associated with a CFC layer
      character cfcname*32 ! name for CFC layer
      character cfcdoc*248 ! documentation
      common/cfcnamearray/cfcname(MGIT_CFC),cfcdoc(MGIT_CFC)

C Data for each CFC layer:
      integer cfccatindex  ! is pointer to index of the material category
      integer cfcitmindex  ! is pointer to item index
      real cfcdbcon        ! is the conductivity ()
      real cfcdbden        ! is the density ()
      real cfcdbsht        ! is the specific heat ()
      real cfcdbthick      ! default thickness (mm) for item
      common/cfcdatarray/cfccatindex(MGIT_CFC), cfcitmindex(MGIT_CFC),
     &  cfcdbcon(MGIT_CFC),cfcdbden(MGIT_CFC),cfcdbsht(MGIT_CFC),
     &  cfcdbthick(MGIT_CFC)      

C Stores the CFC layers database item index for CFCs in the MLC
C database.
C If -1 then not a CFC
C If 0 then confused or missing layer
C If gt 0 then legitimate db index
      integer ITMCFCDB
      common/MLCCFC/ITMCFCDB(MMLC,ME)     
     
C Data structures associated with CFC layers
      integer CFCshdtp        ! CFC shade type (defined in CFC_common.h)
      real CFCsolreflout      ! CFC fabric/material solar reflectance, outside
      real CFCsolreflin       ! CFC fabric/material solar reflectance, inside
      real CFCsoltrandir      ! CFC fabric/material solar direct transmittance
      real CFCsoltrantotout   ! CFC fabric/material solar total transmittance, outside
      real CFCsoltrantotin    ! CFC fabric/material solar total transmittance, inside
      real CFCemissout        ! CFC fabric/material emissivity, outside
      real CFCemissin         ! CFC fabric/material emissivity, inside
      real CFClwtran          ! CFC fabric/material longwave transmittance      
      real CFCdrpwidth        ! CFC pleated drape width (mm)
      real CFCdrpspacing      ! CFC pleated drape spacing (mm)
      real CFCwireemiss       ! CFC insect screen wire emissivity
      real CFCwirediam        ! CFC insect screen wire diameter (mm)
      real CFCwirespace       ! CFC insect screen mesh spacing (mm)
      real CFCslattran        ! CFC venetian blind slat beam-diffuse transmittance
      real CFCslatwidth       ! CFC venetian blind slat width (mm)
      real CFCslatspacing     ! CFC venetian blind slat spacing (mm)
      real CFCslatangle       ! CFC venetian blind initial slat angle (deg, 0 =slats are open )
      character CFCslatorient*4      ! CFC venetian blind slat orientation (HORZ or VERT)
      real CFCslatcrown       ! CFC venetian blind slat crown (mm)
      real CFCslatwr          ! CFC venetian blind slat width/radius of curvature ratio
      real CFCslatthk         ! CFC venetian blind slat thickness (mm)
      integer CFCfillAir      ! CFC fill gas % mole fraction, Air
      integer CFCfillAr       ! CFC fill gas % mole fraction, argon
      integer CFCfillKr       ! CFC fill gas % mole fraction, krypton
      integer CFCfillXe       ! CFC fill gas % mole fraction, xenon
      integer CFCfillSF6      ! CFC fill gas % mole fraction, SF6

      common/dbCFC/CFCshdtp(MGIT_CFC),CFCsolreflout(MGIT_CFC),
     &  CFCsolreflin(MGIT_CFC),CFCsoltrandir(MGIT_CFC),
     &  CFCsoltrantotout(MGIT_CFC), CFCsoltrantotin(MGIT_CFC),
     &  CFCemissout(MGIT_CFC), CFCemissin(MGIT_CFC), 
     &  CFClwtran(MGIT_CFC),
     &  CFCdrpwidth(MGIT_CFC), CFCdrpspacing(MGIT_CFC),
     &  CFCwireemiss(MGIT_CFC), CFCwirediam(MGIT_CFC),
     &  CFCwirespace(MGIT_CFC), CFCslattran(MGIT_CFC), 
     &  CFCslatwidth(MGIT_CFC),
     &  CFCslatspacing(MGIT_CFC), CFCslatangle(MGIT_CFC),
     &  CFCslatorient(MGIT_CFC), CFCslatcrown(MGIT_CFC), 
     &  CFCslatwr(MGIT_CFC), CFCslatthk(MGIT_CFC),
     &  CFCfillAir(MGIT_CFC), CFCfillAr(MGIT_CFC),
     &  CFCfillKr(MGIT_CFC), CFCfillXe(MGIT_CFC), 
     &  CFCfillSF6(MGIT_CFC)   
     
C The construction database includes text after the thickness of each layer
C which is based on the name and documentation of the material from the
C materials database. To assist in recovering materials for which there is
C no legacy index (-1) this text is used.
      character LAYDESC*124  ! first 124 char of layer text
C      character DESC*48 ! string which holds 12 char name, 4 char opaque tag, 
C                        ! 12 char optical tag and SYMMETRIC or NONSYMMETRIC tag
C      common/MLCTXT/LAYDESC(MMLC,ME),DESC(MMLC)
      common/MLCTXT/LAYDESC(MMLC,ME)

C Constructions header common blocks:
      integer mlcver      ! version of the database 0 and 1 with verbose names
      integer mlccats     ! number of categories
      integer mlcdbitems  ! number of items (overall) in database (replaces NMLC)
      integer mlccatitems ! number of items in each category
      common/mlcheader/mlcver,mlccats,mlcdbitems,mlccatitems(MGCL)

C Constructions categories
      character mlcdbdate*24 ! date stamp for the constructions file
      character mlcdbdoc*248 ! documentation for the constructions file
      character mlccatname*32 ! identity/name of category
      character mlccatdoc*248  ! documentation for the category
      common/mlcheadertxt/mlcdbdate,mlcdbdoc,mlccatname(MGCL),
     &  mlccatdoc(MGCL)

C Data structures for verbose names and documentation associated with MLC
      character mlcname*32     ! name of MLC
      character mlcdoc*248     ! documentation
      character mlctype*4      ! type (OPAQ | TRAN | CFC)
      character mlcoptical*32  ! associated optical properties name
      character mlcsymetric*32 ! associated name of reversed MLC or key words
                               ! SYMMETRIC or NONSYMMERTIC
      common/mlcnamearray/mlcname(MMLC),mlcdoc(MMLC),mlctype(MMLC),
     &  mlcoptical(MMLC),mlcsymetric(MMLC)

      integer mlccatindex  ! is pointer to index of the MLC category
      integer matsymindex  ! pointer to index of reversed MLC
      common/mlcdatarray/mlccatindex(MMLC),matsymindex(MMLC)

C Data structures associated with multi-layer constructions and their
C references to materials (index within materials database).
      integer NMLC      ! number of MLC in database
      integer IPR       ! index of each layer material (legacy)
      integer LAYERS    ! number of layers in a MLC
      real DTHK         ! thickness of each layer (m)
      real DRAIR        ! air gap resistance at 1=vert 2=floor/ceil 3=other
      real THKMLC       ! thickness (m) of the MLC
      COMMON/MLC/NMLC,DTHK(MMLC,ME),IPR(MMLC,ME),LAYERS(MMLC),
     &           DRAIR(MMLC,ME,3),THKMLC(MMLC)