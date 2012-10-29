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
C The variable matopaq indicates whether the material
C was opaque/transparent and/or a gas (various types).
C 'o' is opaque, 't' is transparent, '-' imported from legacy
C 'g' is a gap resistance layer, 'h' depreciated gas
      character matopaq*1  ! tag for opaque/transparent/gas
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

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

C The construction database includes text after the thickness of each layer
C which is based on the name and documentation of the material from the
C materials database. To assist in recovering materials for which there is
C no legacy index (-99) this text is used.  This array is filled when the
C MLC database is scanned and should be updated when new materials are selected.
C The string buffer DESC is included in MLCTXT so that MLC is all data.

C << todo increase DESC buffer to 124 char >>
      character LAYDESC*124  ! first 124 char of layer text
      character DESC*48 ! string which holds 12 char name, 4 char opaque tag, 
                        ! 12 char optical tag and SYMMETRIC or NONSYMMETRIC tag
      common/MLCTXT/LAYDESC(MMLC,ME),DESC(MMLC)

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
