C This header relates to predefined geometric entities in ESP-r. It is
C dependant on building.h and include statements to predefined.h should
C follow building.h so that parameters will have been defined.

C << parameters for predefined db classes >>

C << parameters for entities in predefined db >>

C Parameters for predefined objects. As objects might be sourced from
C existing zone geometry files allow surfaces to be modarately complex
C e.g. MV vertices/surface but limit the total number of surfaces and
C vertices in an object.
      integer MOMS,MOBS,MOTV,MOMB,MOMVB,MITM,MCATS
      PARAMETER (MOMS=20)       !- Mass surfaces/obj (10 pairs).
      PARAMETER (MOBS=36)       !- Boundary surfaces/obj.
      PARAMETER (MOTV=96)       !- Vertices/obj. -> 96
      PARAMETER (MOMB=32)       !- Simple visual shapes
      PARAMETER (MOMVB=14)      !- Compound visual shapes
      PARAMETER (MITM=63)       !- Items in the database
      PARAMETER (MCATS=10)      !- Categories

C Commons for simple visual objects within a predefined object:
      character objname*12,objdesc*32  ! name and menu string
      character objectcat*12,objnotes*72 ! category it is in and a text block
      character objsource*72             ! source or provinance of the object
      character msurname*12,bsurname*12  ! names for mass and boundary surfaces
      character msurmat*32,bsurmat*32    ! materials for mass and boundary surfaces
      character msuropt*32,bsuropt*32    ! optics for mass and boundary surfaces
      common/predefstr/objname,objdesc,objectcat,objnotes(6),
     &  objsource,msurname(MOMS),msurmat(MOMS),msuropt(MOMS),
     &  bsurname(MOBS),bsurmat(MOBS),bsuropt(MOBS)
      real objorg         ! origin of selected (derive).
      real objbnds        ! width, depth, height for object (derive)
      integer nbobjnotes  ! how many lines of notes  
      integer nbvertmass  ! how many vertices
      integer nbmass      ! how many mass surfaces
      integer nbbound     ! how many bounding surfaces
      real vertmass       ! vertices associated surfaces
      integer nbmassv     ! how many vertices per mass
      integer nbboundv    ! how many vertices per bounding
      integer masjvn      ! list of associated vertices
      integer boundjvn    ! list of associated vertices
      common/predefnum/objorg(3),objbnds(3),nbobjnotes,nbvertmass,
     &  nbmass,nbbound,vertmass(MOTV,3),nbmassv(MOMS),nbboundv(MOBS),
     &  masjvn(MOMS,MV),boundjvn(MOBS,MV)

C Facade entities can have bounding edges (anti-clockwise) to assist in
C merging into parent surfaces.
      integer nbbedge  ! number of edge vertices
      integer edgejvn  ! list of vertices
      common/predefbnd/nbbedge,edgejvn(MV)

C Similar to visual commons in geometry.h 
      character objvisname*12,objvismat*32,objvistyp*4 ! visual name material type
      character objvobjname*12,objvobjdesc*32  ! visual object collection name & menu
      character objvobjlist*12  ! list of associated visuals 
      common/predefvisstr/objvisname(MOMB),objvismat(MOMB),
     &  objvistyp(MOMB),objvobjname(MOMVB),objvobjdesc(MOMVB),
     &  objvobjlist(MOMVB,14)

      integer nbobjvis  ! number of visuals
      integer nbobjvisobj ! number of visual objects
      integer objnbvobjlist ! how many visuals in each visual object
      real objvisopaq   ! opacity of each
      real objbangov    ! three rotation angles
      real objxov,objyov,objzov  ! object visual origin
      real objdxov,objdyov,objdzov  ! object visual axis dimensions
      real objxvp,objyvp,objzvp  ! object visual polygon xyz
      common/predefvis/nbobjvis,nbobjvisobj,objnbvobjlist(MOMB),
     &  objvisopaq(MOMB),objbangov(MOMB,3),objxov(MOMB),objyov(MOMB),
     &  objzov(MOMB),objdxov(MOMB),objdyov(MOMB),objdzov(MOMB),
     &  objxvp(MOMB,8),objyvp(MOMB,8),objzvp(MOMB,8)

C Commons for shading obstructions in a predefined object.
      integer objnbobs  ! number of obstructions
      real objxob,objyob,objzob  ! obstruction origin
      real objdxob,objdyob,objdzob ! obstruction dimensions
      real objbangob  ! rotations
      real objopob    ! opacity
      real objxbp,objybp,objzbp     ! polygon coordinates
      common/predefobs/objnbobs,objxob(MOMB),objyob(MOMB),
     &  objzob(MOMB),objdxob(MOMB),objdyob(MOMB),objdzob(MOMB),
     &  objbangob(MOMB,3),objopob(MOMB),objxbp(MOMB,8),objybp(MOMB,8),
     &  objzbp(MOMB,8)
      character objblockname*12,objblockmat*32,objblocktyp*4
      common/predefobstr/objblockname(MOMB),objblockmat(MOMB),
     &  objblocktyp(MOMB)


