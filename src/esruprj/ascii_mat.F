C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C Version 1.1 of materials database
C Implementation requires the following code facilties:
C  a) subroutine to scan legacy ASCII material databases into
C     common blocks.  At least for a transition period there
C     would also be a way to scan legacy binary materials
C     databases into the common blocks.

C  b) modification of current material editing facilities to
C     work with the new common blocks

C  c) modification fo current code that requests material data
C     to get it from common block rather than read of binary file.
C     (also to recognise an older file and perform an automatic
C     conversion).

C  d) modification of the databases folder to include a new
C     ASCII material database and the Install script to use the
C     new file

C  e) scan of exemplar models changing references from the older
C     materials databases to the new. 

C Materials common blocks:
C matver (real) version of the database
C matcats (integer) number of categories
C matdbitems (integer) number of items in database
C matcatitems (integer) number of items in each category
C matdbdate (24 char) date stamp for the database
C matdbdoc (248 char) documentation for the materials database
C matcatname (32 char) for menu selection of categories
C matcatdoc (248 char) documentation for the category
C      real matver
C      integer matcats,matcatitems,matdbitems
C      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
C      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
C     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

C It would be good for purposes of sorting to provide a flag indicating
C whether the material was opaque/transparent and/or a gas. This would
C allow additional data to be scanned from the file:
C matopaq (1 char) 'o' is opaque, 't' is transparent,
C                  'g' is gas (data+temp correction),
C                  'h' is gas (data at 4 temps),
C                  '-' imported from legacy

C matname (32 char) for leacy materials the name is a combination of
C   the characters 'mat_' followed by the legacy index in i3.3 format.
C matdoc (248? char) is initially filled with the 72 char from
C   the older name of the material.
C    character matname*32,matdoc*248,matopaq*1
C    common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

C matlegindex (integer) is the original material index
C matcatindex (integer) is pointer to index of the material category
C matdbcon (real) is the conductivity ()
C matdbden (real) is the density ()
C matdbsht (real) is the specific heat ()

C Traditionally emissivity and solar abs have been assumed to be the same
C on both faces of a material. Proposal to expand data structure to allow
C for differing values on each face.  Initially, both would be set to the
C same value. Later the source could be updated to support differing
C assumptions for each face.
C matdboute (real) is the outside face long wave emissivity (-)
C matdbine (real) is the inside face long wave emissivity (-)
C matdbouta (real) is the outside face short wave absorbtivity (-)
C matdbina (real) is the inside face short wave absorbtivity (-)
C matdbdrv (real) is the diffusion resistance (?)
C matdbthick (real) default thickness (mm) for item (this is a new attribute
C   which might be useful in regions where there are standard thickness
C   of products
C      real matdbcon,matdbden,matdbsht,matdboute,matdbine
C      real matdbouta,matdbina,matdbdrv,matdbthick
C      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
C     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
C     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
C     &  matdbthick(MGIT)

C So called 'G-value' calculations require the sorts of single layer optical
C properties which ESP-r does not current hold. Also the lack of this data
C makes it difficult to work with optical tools such as WIS

C Based on WIS single layer data the following data could be held based
C on the assumtion that the default thickness was used by the construction:
C matirtran        IR transmittance
C matsoldrtrn    solar direct transmittance
C matsoldrotrfl outer solar direct reflectance
C matsoldrinrfl inner solar direct reflectance
C matvistran    visable transmittance
C matvisotrfl   outer visable reflectance
C matvisinrfl   inner visable reflectance
C matrender     colour rendering index
C      real matirtran,matsoldrtrn,matsoldrotrfl,matsoldrinrfl
C      real matvistran,matvisotrfl,matvisinrfl,metrendering
C      common/matwisarray/matirtran(MGIT),matsoldrtrn(MGIT),
C     &  matsoldrotrfl(MGIT),matsoldrinrfl(MGIT),matvistran(MGIT),
C     &  matvisotrfl(MGIT),matvisinrfl(MGIT),matrender(MGIT)

C For air gaps WIS supplies some additional data.
C matgapcnd(4)  gap conduction at -10C 0C 10C 20C
C matvisco(4) gap viscosity at -10C 0C 10C 20C
C matgapden(4)  gap density at -10C 0C 10C 20C
C matprandtl(2) prandtl number and extra field for T correct
C matgapsht(2) gap specific heat and T correct.
C UK EPBD method uses the 1st array index for the value and
C the 2nd array index for T derivitive.
C      real matgapcnd,matvisco,matgapden,matprandtl,matgapsht
C      common/matgaparray/matgapcnd(MGIT,4),matvisco(MGIT,4),
C     &  matgapden(MGIT,4),matprandtl(MGIT,2),matgapsht(MGIT,2)

C mathash is a pointer to the array index in matdatarray for a
C   given legacy index e.g array_index = mathash(legacy_index)
C   if return is negative then there is no pointer. Index includes
C   zero to accommodate the implied air material.
C    integer mathash
C    common/mathasharray/mathash(0:600)

C << Idea: have something like parsemlcdesc that goes and looks
C << for a material that matches either the legacy index or
C << the short material name.

C << What further information differences are there with
C << EnergyPlus and DOE-2?

C << What further information differences are there with
C << WIS and Window 5.1

C Categories for materials implied by UK national calculation method.
C   Air Layer
C   Asbestos Cement
C   Asphalt
C   Brick
C   Building Paper
C   Concrete dense
C   Concrete lightweight
C   Concrete medium density
C   Cement Mortar
C   Felt
C   Floor finish
C   Glazing not coated not tinted
C   Glazing not coated tinted
C   Glazing low-e coated not tinted
C   Glazing low-e coated tinted
C   Insulation (batts)
C   Insulation (board)
C   Insulation (fill)
C   Insulation (quilt)
C   Insulation (miscellaneous)
C   Metal
C   Particle Board
C   Plaster
C   Plasterboard
C   Render
C   Roof Gravel
C   Rubber Tile
C   Soil
C   Stone
C   Tiles / slate
C   Wood
C   Other
C   

C rlegacymat: scan legacy ASCII file and fill common blocks.
C mkascimat:  creates ascii material file (version 1.1) based on
C rascimat:   fills common matdatarray and matnamearray from

C ************* rlegacymat 
C rlegacymat: Fills common matdatarray and matnamearray from
C legacy ASCII material database LASCI opened on unit IFA. 
      subroutine rlegacymat(ITRU,IFA,LASCI,IER)
#include "esprdbfile.h"

      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      real matirtran,matsoldrtrn,matsoldrotrfl,matsoldrinrfl
      real matvistran,matvisotrfl,matvisinrfl,matrender
      common/matwisarray/matirtran(MGIT),matsoldrtrn(MGIT),
     &  matsoldrotrfl(MGIT),matsoldrinrfl(MGIT),matvistran(MGIT),
     &  matvisotrfl(MGIT),matvisinrfl(MGIT),matrender(MGIT)

      real matgapcnd,matvisco,matgapden,matprandtl,matgapsht
      common/matgaparray/matgapcnd(MGIT,4),matvisco(MGIT,4),
     &  matgapden(MGIT,4),matprandtl(MGIT,2),matgapsht(MGIT,2)

      integer mathash
      common/mathasharray/mathash(0:600)

      DIMENSION VAL(6)
      CHARACTER IDESC*72,NDESC*72,OUTSTR*124,LASCI*72

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)RETURN

C Get date stamp for the materials arrays.
      call dstamp(matdbdate)

C Fill the hash table with not included marker (-1)
      do 42 ih=0,600
        mathash(ih)=-1
  42  continue

C Need subroutine to clear the material arrays.
C << >>

      matver = 1.1   ! set initial version of database.

C Initiate documentation for the database (new concept).
      write(matdbdoc,'(4a)')
     & 'ESP-r Materials based on scan of a legacy ASCII materials',
     & 'database. Information in ASCII file is recorded as found. ',
     & 'Additional infomation and documentation uses standard ',
     & 'assumptions which should be updated as required.' 

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no classes',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,matcats,1,MGCL,'W','nb mat classes',IER)

      matdbitems = 0   ! reset the total number of items.
      DO 10 I=1,matcats
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'IC NE NDESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,IC,0,MGCL,'F','class',IER)
        CALL EGETWI(OUTSTR,K,NE,0,19,'-','no items',IER)
        matcatitems(I)=NE
        CALL EGETRM(OUTSTR,K,NDESC,'-','description',IER)
        write(matcatname(I),'(a)') NDESC(1:32)

C Create initial classification documentation
        write(matcatdoc(I),'(a,i2,3a)')
     &    'Category (',I,') was initially named ',
     &    NDESC(1:lnblnk(NDESC)),'. No other documentation (yet).'

C Loop through each of the items in this category, incrementing the
C matdbitems counter and creating a new name.
        DO 20 J=1,matcatitems(I)
          matdbitems=matdbitems+1   ! increment counter
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'leg index and text',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,NCODE,-99,600,'F','legacy index',IER)
          matlegindex(matdbitems)=NCODE
          matcatindex(matdbitems)=I

C If later the order of the array changes then it will probably
C be necessary to rebuild the mathash array. << ?? >>

          if(NCODE.ge.0.and.NCODE.le.600)then
            mathash(NCODE)=matdbitems  ! set up pointer from legacy index to array
          endif
          write(matname(matdbitems),'(a,i3.3)') 'mat_',ncode

          CALL EGETRM(OUTSTR,K,IDESC,'W','mat description',IER)
          write(matdoc(matdbitems),'(a)') IDESC(1:lnblnk(IDESC))

          CALL STRIPC(IFA,OUTSTR,0,ND,1,'the raw data line',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(OUTSTR,K,VAL(1),0.,0.,'-','db con',IER)
          CALL EGETWR(OUTSTR,K,VAL(2),0.,0.,'-','db den',IER)
          CALL EGETWR(OUTSTR,K,VAL(3),0.,0.,'-','db sht',IER)
          CALL EGETWR(OUTSTR,K,VAL(4),0.,0.,'-','db emis',IER)
          CALL EGETWR(OUTSTR,K,VAL(5),0.,0.,'-','db absor',IER)
          CALL EGETWR(OUTSTR,K,VAL(6),0.,0.,'-','db difus',IER)
          matdbcon(matdbitems)=VAL(1)
          matdbden(matdbitems)=VAL(2)
          matdbsht(matdbitems)=VAL(3)
          matdboute(matdbitems)=VAL(4)
          matdbine(matdbitems)=VAL(4)
          matdbouta(matdbitems)=VAL(5)
          matdbina(matdbitems)=VAL(5)
          matdbdrv(matdbitems)=VAL(6)

C Default thickness does not exist in legacy file so set a place
C holder based on typcial classes (for databases with 15-17 classes)
          if(matcats.ge.15.and.matcats.le.17)then
            matopaq(matdbitems)='-'
            if(I.eq.1.or.I.eq.2)then
              matdbthick(matdbitems)=100.0
            elseif(I.eq.3)then
              matdbthick(matdbitems)=5.0
            elseif(I.eq.4)then
              matdbthick(matdbitems)=25.0
            elseif(I.eq.5)then
              matdbthick(matdbitems)=200.0
            elseif(I.eq.6)then
              matdbthick(matdbitems)=12.5
            elseif(I.eq.7)then
              matdbthick(matdbitems)=50.0
            elseif(I.eq.8.or.I.eq.9.or.I.eq.10)then
              matdbthick(matdbitems)=6.0
            elseif(I.eq.11)then
              matdbthick(matdbitems)=100.0
            elseif(I.eq.12)then
              matdbthick(matdbitems)=5.0
            elseif(I.eq.13)then

C Assume that this category is glass. ?? WIS data
              write(matdoc(matdbitems),'(2a)') IDESC(1:lnblnk(IDESC)),
     &          ' with placeholder single layer optics'
              matdbthick(matdbitems)=6.0
              matopaq(matdbitems)='t'
              matirtran(matdbitems)=0.0
              matsoldrtrn(matdbitems)=0.0
              matsoldrotrfl(matdbitems)=0.0
              matsoldrinrfl(matdbitems)=0.0
              matvistran(matdbitems)=0.0
              matvisotrfl(matdbitems)=0.0
              matvisinrfl(matdbitems)=0.0
              matrender(matdbitems)=0.0
            elseif(I.eq.14)then
              matdbthick(matdbitems)=200.0
            elseif(I.eq.15)then
              matdbthick(matdbitems)=100.0
            else
              matdbthick(matdbitems)=100.0
            endif
          else
            matdbthick(matdbitems)=100.0
            matopaq(matdbitems)='o'
          endif

  20    CONTINUE
  10  CONTINUE

C If there has been no air material read in then add in one more
C category for air and fill it with the default information that
C the standard call to ERPCDB would have returned.
      if(mathash(0).eq.-1)then
        matcats=matcats+1
        matcatitems(matcats)=1
        write(matcatname(matcats),'(a)') 'GAPS'
        write(matcatdoc(matcats),'(a,i2,a)')
     &    'Category (',matcats,') holds the implied air material '
        matdbitems=matdbitems+1   ! increment counter
        matlegindex(matdbitems)=0
        matcatindex(matdbitems)=matcats
        mathash(0)=matdbitems  ! set up pointer from legacy index to array
        write(matname(matdbitems),'(a)') 'mat_000'
        write(matdoc(matdbitems),'(a)') 
     &    'Air layer with default properties'
        matdbcon(matdbitems)=0.0
        matdbden(matdbitems)=0.0
        matdbsht(matdbitems)=0.0
        matdboute(matdbitems)=0.99
        matdbine(matdbitems)=0.99
        matdbouta(matdbitems)=0.99
        matdbina(matdbitems)=0.99
        matdbdrv(matdbitems)=1.0
        matdbthick(matdbitems)=25.0
        matopaq(matdbitems)='-'
        matirtran(matdbitems)=0.0
        matsoldrtrn(matdbitems)=0.0
        matsoldrotrfl(matdbitems)=0.0
        matsoldrinrfl(matdbitems)=0.0
        matvistran(matdbitems)=0.0
        matvisotrfl(matdbitems)=0.0
        matvisinrfl(matdbitems)=0.0
        matrender(matdbitems)=0.0
      endif

C Debug.
C      write(6,*) 'matdbitems ',matdbitems
C      write(6,*) 'matcatitems ',matcatitems
C      write(6,*) 'mathash ',mathash
C      write(6,*) 'matlegindex ',matlegindex
      
      CALL ERPFREE(IFA,ISTAT)
      RETURN
    
      END

C ************* mkascimat 
C mkascimat: creates ascii material file (version 1.1) based on
C current information in common matdatarray and matnamearray.
C ASCII file LASCI opened on unit IFA. 
      subroutine mkascimat(ITRU,IFA,LASCI,IER)
#include "esprdbfile.h"

C Notes on variables at top of file.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      real matirtran,matsoldrtrn,matsoldrotrfl,matsoldrinrfl
      real matvistran,matvisotrfl,matvisinrfl,matrender
      common/matwisarray/matirtran(MGIT),matsoldrtrn(MGIT),
     &  matsoldrotrfl(MGIT),matsoldrinrfl(MGIT),matvistran(MGIT),
     &  matvisotrfl(MGIT),matvisinrfl(MGIT),matrender(MGIT)

      real matgapcnd,matvisco,matgapden,matprandtl,matgapsht
      common/matgaparray/matgapcnd(MGIT,4),matvisco(MGIT,4),
     &  matgapden(MGIT,4),matprandtl(MGIT,2),matgapsht(MGIT,2)

      integer mathash
      common/mathasharray/mathash(0:600)
      character tab*1,outs*248,outsd*248
      character LASCI*72
      integer matcount,matcatcount  ! used to increment arrays
      integer lncat     ! for length of string

      tab=','       ! create tab separator.
      matcount=0    ! setup initial overall counter
      matcatcount=0 ! setup initial category counter

C The ASCII file will be overwritten if it exists.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)RETURN

      write(IFA,'(A,F3.1)') '*Materials ',matver
      write(IFA,'(3A)') '*date',tab,matdbdate
      write(IFA,'(2A)') '# materials database defined in ',
     &  LASCI(1:lnblnk(LASCI))
      write(IFA,'(3A)') '*doc',tab,matdbdoc(1:lnblnk(matdbdoc))
      write(IFA,'(i2,a)') matcats,'  # number of classifications'
      write(IFA,'(A)') '#  '
      write(IFA,'(A)') '# Materials have the following attributes:'
      write(IFA,'(2A)') '#  conductivity (W/(m-K), density (kg/m**3)',
     &  ' specific heat (J/(kg-K)'
      write(IFA,'(2A)') '#  emissivity out (-) emissivity in (-)',
     &  '#   absorptivity out (-) absorptivity in (-)'
      write(IFA,'(A)') '#  diffusion rest (?) default thickness (mm)'
      write(IFA,'(A)') '#  flag [-] legacy [o] opaque [t] transparent'
      write(IFA,'(A)') '#       [g] gas data+T cor [h] gas data at 4T'
      write(IFA,'(A)') '#  '
      write(IFA,'(A)') '# Transparent material attributes:'
      write(IFA,'(2A)')'#  longwave tran (-) solar direct tran (-)',
     &  ' solar reflec out (-) solar refled in (-)'
      write(IFA,'(2A)')'#  visable tran (-) visable reflec out (-)',
     &  ' visable reflec in (-) colour rendering (-)'
      write(IFA,'(A)') '#  '

C Loop through each classification (index I) and write out contents.
      do 10 I=1,matcats
        matcatcount=0 ! zero items in category counter
        write(IFA,'(A)') 
     &    '# class index |nb items|description (32 char)'
        lncat=lnblnk(matcatname(I))
        write(IFA,'(2A,i2,a,i2,2a)') '*class',tab,I,tab,matcatitems(I),
     &    tab,matcatname(I)(1:lncat)
        lncat=lnblnk(matcatdoc(I))
        write(IFA,'(a)') matcatdoc(I)(1:lncat)

C Loop through each of the database items (index J) for items that fit the current
C category.  This has the effect of packing the file and sorting the
C common block arrays so that future reads are more efficient.
        do 20 J=1,matdbitems
          if(matcatindex(J).eq.I)then
            matcatcount=matcatcount+1 ! increment items in category counter
            if(matcatcount.gt.matcatitems(I))then
              call usrmsg('Number of items linked with a category ',
     &                    'exceeds expected count.','W')
            endif
            matcount=matcount+1   ! increment counter
            if(matcount.gt.matdbitems)then
              call usrmsg('Number of materials scanned exceeds',
     &                    'expected count for database.','W')
            endif
            lnna=lnblnk(matname(J))
            lndo=lnblnk(matdoc(J))
            write(IFA,'(4a,i3,a,i2,2a)') '*item',tab,
     &        matname(J)(1:lnna),tab,matlegindex(J),
     &        tab,matcatindex(J),tab,matdoc(J)(1:lndo)
            if(matopaq(J)(1:1).eq.'-')then
              write(outs,'(f13.3,2F9.3,4f6.3,f10.3,F6.1,2a)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J)
            elseif(matopaq(J)(1:1).eq.'o')then
              write(outs,'(f13.3,2F9.3,4f6.3,f10.3,F6.1,2a)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J)
            elseif(matopaq(J)(1:1).eq.'t')then
              write(outs,'(f13.3,2F9.3,4f6.3,f10.3,F6.1,2a,8F6.3)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J),
     &          matirtran(J),matsoldrtrn(J),matsoldrotrfl(J),
     &          matsoldrinrfl(J),matvistran(J),matvisotrfl(J),
     &          matvisinrfl(J),matrender(J)
            elseif(matopaq(J)(1:1).eq.'g')then
              write(outs,'(f13.3,2F9.3,4f6.3,f10.3,F6.1,2a,
     &          f8.5,f9.6,2f11.8,f8.4,f9.5,2f9.6,f9.3,f9.6)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J),
     &          matgapcnd(J,1),matgapcnd(J,2),matvisco(J,1),
     &          matvisco(J,2),matgapden(J,1),matgapden(J,2),
     &          matprandtl(J,1),matprandtl(J,2),matgapsht(J,1),
     &          matgapsht(J,2)
            elseif(matopaq(J)(1:1).eq.'h')then
              write(outs,'(f13.3,2F9.3,4f6.3,f10.3,F6.1,2a,4f8.5,
     &          4f11.8,4f9.6,f9.6,f9.3)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J),
     &          matgapcnd(J,1),matgapcnd(J,2),matgapcnd(J,3),
     &          matgapcnd(J,4),matvisco(J,1),matvisco(J,2),
     &          matvisco(J,3),matvisco(J,4),matgapden(J,1),
     &          matgapden(J,2),matgapden(J,3),matgapden(J,4),
     &          matprandtl(J,1),matgapsht(J,1)
            endif
            call SDELIM(outs,outsd,'C',IW)
            write(IFA,'(a)') outsd(1:lnblnk(outsd))
          endif
  20    continue
  10  continue
      write(IFA,'(a)') '*end'
      CALL ERPFREE(IFA,ISTAT)
      RETURN
      end

C ************* rascimat 
C rascimat: Fills common matdatarray and matnamearray from
C current ASCII material database LASCI opened on unit IFA. 
      subroutine rascimat(ITRU,IFA,LASCI,IER)
#include "esprdbfile.h"

      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      real matirtran,matsoldrtrn,matsoldrotrfl,matsoldrinrfl
      real matvistran,matvisotrfl,matvisinrfl,matrender
      common/matwisarray/matirtran(MGIT),matsoldrtrn(MGIT),
     &  matsoldrotrfl(MGIT),matsoldrinrfl(MGIT),matvistran(MGIT),
     &  matvisotrfl(MGIT),matvisinrfl(MGIT),matrender(MGIT)

      real matgapcnd,matvisco,matgapden,matprandtl,matgapsht
      common/matgaparray/matgapcnd(MGIT,4),matvisco(MGIT,4),
     &  matgapden(MGIT,4),matprandtl(MGIT,2),matgapsht(MGIT,2)

      integer mathash
      common/mathasharray/mathash(0:600)

      DIMENSION VAL(16)
      CHARACTER IDESC*72,NDESC*72,OUTSTR*124,LASCI*72,WORD*24
      character WORD32*32,WORD248*248,LOUTSTR*248

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)RETURN

      matver = 1.1   ! set initial version of database.
      matdbitems = 0   ! reset the total number of items.

C Proceed with header.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'material header',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(OUTSTR,K,WORD,'-','*material',IER)
      if(WORD(1:10).eq.'*Materials'.or.WORD(1:10).eq.'*materials')then

C Fill the hash table with not included marker (-1)
        do 42 ih=0,600
          mathash(ih)=-1
  42    continue
      else

C Did not find the correct header so this is probably a legacy file.
        ier=-2
        CALL ERPFREE(IFA,ISTAT)
        return
      endif

  41  CALL STRIPC(IFA,OUTSTR,0,ND,1,'header lines',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(OUTSTR,K,WORD,'-','*date',IER)
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(OUTSTR,K,matdbdate,'W','date stamp',IER)
        goto 41
      elseif(WORD(1:4).eq.'*Doc'.or.WORD(1:4).eq.'*doc')then
        CALL EGETRM(OUTSTR,K,matdbdoc,'W','database doc',IER)

C and the next line is number of classes...
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'nb of classes',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,matcats,1,MGCL,'W','nb of classes',IER)
        goto 41
      elseif(WORD(1:6).eq.'*Class'.or.WORD(1:6).eq.'*class')then

C Get class index, nb of items and class name. Then read the next line
C as documentation for the class. Untill the next *class line the
C class index IC is used for subsequent items.
        CALL EGETWI(OUTSTR,K,IC,0,MGCL,'F','class',IER)
        CALL EGETWI(OUTSTR,K,NE,0,19,'-','no items',IER)
        matcatitems(IC)=NE
        CALL EGETRM(OUTSTR,K,NDESC,'-','description',IER)
        write(matcatname(IC),'(a)') NDESC(1:lnblnk(NDESC))

        CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'class documentation',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETRM(LOUTSTR,K,WORD248,'W','categ doc',IER)
        write(matcatdoc(IC),'(a)') WORD248(1:lnblnk(WORD248))
        goto 41
      elseif(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then
        matdbitems=matdbitems+1   ! increment counter
        CALL EGETP(OUTSTR,K,WORD32,'-','item name',IER) ! name might contain a space
        write(matname(matdbitems),'(a)') WORD32(1:lnblnk(WORD32))
        CALL EGETWI(OUTSTR,K,NCODE,-99,600,'F','legacy index',IER)
        matlegindex(matdbitems)=NCODE
        if(NCODE.ge.0.and.NCODE.le.600)then
          mathash(NCODE)=matdbitems  ! set up pointer from legacy index to array
        endif
        CALL EGETWI(OUTSTR,K,matcatindex(matdbitems),1,MGCL,'F',
     &    'category index',IER)
        CALL EGETRM(OUTSTR,K,WORD248,'-','item doc',IER)
        write(matdoc(matdbitems),'(a)') WORD248(1:lnblnk(WORD248))

        CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'the raw data line',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-','db con',IER)
        CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-','db den',IER)
        CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-','db sht',IER)
        CALL EGETWR(LOUTSTR,K,VAL(4),0.,0.,'-','db out emis',IER)
        CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-','db in emis',IER)
        CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-','db out absorb',IER)
        CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-','db in absorb',IER)
        CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','db difusn',IER)
        CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-','db def thick',IER)
        CALL EGETW(LOUTSTR,K,WORD32,'-','tag for opaque transp',IER)
        matdbcon(matdbitems)=VAL(1)
        matdbden(matdbitems)=VAL(2)
        matdbsht(matdbitems)=VAL(3)
        matdboute(matdbitems)=VAL(4)
        matdbine(matdbitems)=VAL(5)
        matdbouta(matdbitems)=VAL(6)
        matdbina(matdbitems)=VAL(7)
        matdbdrv(matdbitems)=VAL(8)
        matdbthick(matdbitems)=VAL(9)
        if(WORD32(1:1).eq.'o')then
          matopaq(matdbitems)='o'
        elseif(WORD32(1:1).eq.'-')then
          matopaq(matdbitems)='-'
        elseif(WORD32(1:1).eq.'t')then

C Read single layer optics if there are more than 
          matopaq(matdbitems)='t'
          if(ND.gt.10)then
            CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-','db ir tran',IER)
            CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-','db sol dir tr',IER)
            CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-','db sol refl',IER)
            CALL EGETWR(LOUTSTR,K,VAL(4),0.,0.,'-','db sol refl',IER)
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-','db vis tran',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-','db vis refl',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-','db vis refl',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','db vis render',IER)
            matirtran(matdbitems)=VAL(1)
            matsoldrtrn(matdbitems)=VAL(2)
            matsoldrotrfl(matdbitems)=VAL(3)
            matsoldrinrfl(matdbitems)=VAL(4)
            matvistran(matdbitems)=VAL(5)
            matvisotrfl(matdbitems)=VAL(6)
            matvisinrfl(matdbitems)=VAL(7)
            matrender(matdbitems)=VAL(8)
          endif
        elseif(WORD32(1:1).eq.'g')then   ! gas (data+temp correction)
          matopaq(matdbitems)='g'
          CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-','db gas cond',IER)
          CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-','db cond T deriv',IER)
          CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-','db gas visc',IER)
          CALL EGETWR(LOUTSTR,K,VAL(4),0.,0.,'-','db visc T deriv',IER)
          CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-','db gas density',IER)
          CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-','db dens T deriv',IER)
          CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-','db prandtl',IER)
          CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','db prandtl der',IER)
          CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-','db spec ht',IER)
          CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-','db spec ht der',IER)
          matgapcnd(matdbitems,1)=VAL(1)
          matgapcnd(matdbitems,2)=VAL(2)
          matvisco(matdbitems,1)=VAL(3)
          matvisco(matdbitems,2)=VAL(4)
          matgapden(matdbitems,1)=VAL(5)
          matgapden(matdbitems,2)=VAL(6)
          matprandtl(matdbitems,1)=VAL(7)
          matprandtl(matdbitems,2)=VAL(8)
          matgapsht(matdbitems,1)=VAL(9)
          matgapsht(matdbitems,2)=VAL(10)
        elseif(WORD32(1:1).eq.'h')then   ! gas (data at 4 temps)
          matopaq(matdbitems)='h'
          CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-','gas cond -10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-','gas cond 0',IER)
          CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-','gas cond 10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(4),0.,0.,'-','gas cond 20',IER)
          CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-','gas visc -10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-','gas visc 0',IER)
          CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-','gas visc 10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','gas visc 20',IER)
          CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-','gas dens -10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-','gas dens 0',IER)
          CALL EGETWR(LOUTSTR,K,VAL(11),0.,0.,'-','gas dens 10',IER)
          CALL EGETWR(LOUTSTR,K,VAL(12),0.,0.,'-','gas dens 20',IER)
          CALL EGETWR(LOUTSTR,K,VAL(13),0.,0.,'-','db prandtl',IER)
          CALL EGETWR(LOUTSTR,K,VAL(14),0.,0.,'-','db gas sp ht',IER)
          matgapcnd(matdbitems,1)=VAL(1)
          matgapcnd(matdbitems,2)=VAL(2)
          matgapcnd(matdbitems,3)=VAL(3)
          matgapcnd(matdbitems,4)=VAL(4)
          matvisco(matdbitems,1)=VAL(5)
          matvisco(matdbitems,2)=VAL(6)
          matvisco(matdbitems,3)=VAL(7)
          matvisco(matdbitems,4)=VAL(8)
          matgapden(matdbitems,1)=VAL(9)
          matgapden(matdbitems,2)=VAL(10)
          matgapden(matdbitems,3)=VAL(11)
          matgapden(matdbitems,4)=VAL(12)
          matprandtl(matdbitems,1)=VAL(13)
          matgapsht(matdbitems,1)=VAL(14)
        endif

        goto 41
      elseif(WORD(1:4).eq.'*End'.or.WORD(1:4).eq.'*end')then

C End of file marker.
        CALL ERPFREE(IFA,ISTAT)
        RETURN
      else

C << did not find the correct header.... >>
        write(word248,'(2a)') 'Unrecognised in line ',
     &    outstr(1:lnblnk(outstr))
        call edisp248(iuout,WORD248,100)
        CALL ERPFREE(IFA,ISTAT)
        ier=-2
        return
      endif
      
  99  CALL ERPFREE(IFA,ISTAT)
      RETURN
    
      END

C ************* getnextascislot 
C getnextascislot: Finds the next empty slot in the 0-600 list
C of legacy material indices.
C Where: ip is point to start looking from
C        inext is slot found.
      subroutine getnextascislot(ip,inext)

C mathash is a pointer to the array index in matdatarray for a
C   given legacy index e.g array_index = mathash(legacy_index)
C   if return is negative then there is no pointer. The array
C   starts at zero to account for the implied air magerial.
      integer mathash
      common/mathasharray/mathash(0:600)
      integer ip,inext,ilook

C Set inext to 601 in case nothing found. If mathash array item
C is negative then it has not yet been used. Calling code should
C be sure to instanciate mathash with the material array index.
      inext=601
      if(ip.gt.1.and.ip.le.599)then
        do 42 ilook=ip,600
          if(mathash(ilook).lt.0)then
            inext=ilook
            return
          endif
  42    continue
      endif
      return
      end
