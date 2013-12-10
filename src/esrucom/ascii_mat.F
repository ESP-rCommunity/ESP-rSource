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

C Version 1.1 or 1.2 of materials database
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

C Materials common blocks see documentation withing material.h file.

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

C rlegacymat: scan legacy ASCII mat db file and fill common blocks.
C mkascimat:  creates ascii material file (version 1.1/2) based on
C             information in common matdatarray and matnamearray.
C rascimat:   fills common matdatarray and matnamearray from
C             current ASCII material database. 
C MATFROMBIN: Fills materials common blocks via scan of binary materials
C             database.
C scananymat: scans any materials database and fills common blocks.

C ************* rlegacymat 
C rlegacymat: Fills common matdatarray and matnamearray from
C legacy ASCII material database LASCI opened on unit IFA. 
      subroutine rlegacymat(IFA,LASCI,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144  ! ascii file name
      integer IER         ! error return where zero is ok, 
                          ! ier=-1 file not found, ier=-2 no classes
      COMMON/OUTIN/IUOUT,IUIN
      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

C Name of current application - only invoke editing if prj.
      character cAppName*12
      common/APPNAME/cAppName
      character h*72
      common/pophelp/h(60)

      character tmode*8
      CHARACTER DOIT*300
      DIMENSION VAL(6)
      CHARACTER IDESC*72,NDESC*72,OUTSTR*124
      character namelist*32,nametocheck*32  ! to check for duplicate names
      character outs*142,lltmp*144,longtfile*144,longtfiledos*144
      dimension namelist(600)
      integer loop,ih  ! loop indices
      integer iln,iln2 ! length of name strings
      logical dupfound,concat,unixok,ok,dok

C Open the ASCII file for reading. Reset ier state prior to efopseq.
      ier=0
      dupfound = .false.  ! assume no duplicate names
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)RETURN

      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

C Get date stamp for the materials arrays.
      call dstamp(matdbdate)

C Initiate documentation for the database (new concept).
      write(matdbdoc,'(4a)')
     & 'ESP-r Materials based on scan of a legacy ASCII materials',
     & 'database. Information in ASCII file is recorded as found. ',
     & 'Additional infomation and documentation uses standard ',
     & 'assumptions which should be updated as required.' 

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no classes',IER)
      if(IER.NE.0)then
        ier=-2
        RETURN
      endif

C Clear the materials common blocks.
      matdbitems = 0   ! reset the total number of items.
      do ij=1,MGCL
        matcatname(ij)=' '
        matcatdoc(ij)=' '
        matcatitems(ij)=0
      end do

C Fill the hash table with not included marker (-1) and clear the
C array of duplicate checking names and materials data.
      nametocheck=' '
      matopaq(-1)=' '  ! clear the confused material slot
      do ih=0,600
        mathash(ih)=-1
        if(ih.gt.0)namelist(ih)=' '
        if(ih.gt.0)matname(ih)=' '
        if(ih.gt.0)matdoc(ih)=' '
        matopaq(ih)=' '
        if(ih.gt.0)matcatindex(ih)=0
      end do

      K=0
      CALL EGETWI(OUTSTR,K,matcats,1,MGCL,'W','nb mat classes',IER)
      IF(IER.NE.0.or.matcats.le.0)RETURN  ! if no categories return

      DO 10 I=1,matcats
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'IC NE NDESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,IC,0,MGCL,'F','class',IER)
        CALL EGETWI(OUTSTR,K,NE,0,19,'-','no items',IER)
        IF(IER.NE.0.or.NE.le.0)RETURN  ! if no items return
        matcatitems(I)=NE
        CALL EGETRM(OUTSTR,K,NDESC,'-','description',IER)
        IF(IER.NE.0)RETURN
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

          if(NCODE.ge.0.and.NCODE.le.600)then
            mathash(NCODE)=matdbitems  ! set up pointer from legacy index to array
          endif

          CALL EGETRM(OUTSTR,K,IDESC,'W','mat description',IER)

C The MLC database will expect to find the name of the material matching the
C description NDESC initial characters. Write the initial 32 char of NDESC 
C as the material name.
          lex=MIN0(lnblnk(IDESC),32)

C Record the name and then check and see if it is a duplicate.
          write(namelist(matdbitems),'(a)') IDESC(1:lex)
          write(nametocheck,'(a)') IDESC(1:lex)
          iln=lnblnk(nametocheck)  ! length of name to test
          if(matdbitems.gt.1)then
            loop=matdbitems-1
            do 43 ih=1,loop
              iln2=lnblnk(namelist(ih))  ! length of prior name to test
              if(iln.eq.iln2)then
                if(namelist(ih)(1:iln2).eq.nametocheck(1:iln))then
                  write(outs,'(3a,i4,3a,i4,a)') 'Duplicate names ',
     &              nametocheck(1:iln),' (',matdbitems,') vs ',
     &              namelist(ih)(1:iln2),' (',ih,
     &              '). Must be unique!'
                  call edisp(iuout,outs)
                  dupfound = .true.  ! a duplicate name found
                endif
              else
                continue
              endif
  43        continue
          endif
 
          write(matname(matdbitems),'(a)') IDESC(1:lex)
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

C Assume that category 13 is glass.
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
C category for air and fill it with default information.
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
        write(matname(matdbitems),'(a)') 'air'
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
        matopaq(matdbitems)='g'  ! set as gap type
        matirtran(matdbitems)=0.0
        matsoldrtrn(matdbitems)=0.0
        matsoldrotrfl(matdbitems)=0.0
        matsoldrinrfl(matdbitems)=0.0
        matvistran(matdbitems)=0.0
        matvisotrfl(matdbitems)=0.0
        matvisinrfl(matdbitems)=0.0
        matrender(matdbitems)=0.0
        matgapares(matdbitems,1)=0.17
        matgapares(matdbitems,2)=0.17
        matgapares(matdbitems,3)=0.17
      endif

      matver = 1.2   ! set version of database.

C Debug.
C      write(6,*) 'matdbitems ',matdbitems
C      write(6,*) 'matcatitems ',matcatitems
C      write(6,*) 'mathash ',mathash
C      write(6,*) 'matlegindex ',matlegindex
      
      CALL ERPFREE(IFA,ISTAT)

C If it is a local materials file and there has been a duplicate
C name discovered and the application is 'prj' then offer the user
C the option of editing the materials files.
      if(cAppName(1:3).eq.'prj')then
        continue
      else
        return
      endif
      if(dupfound)then
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          dok=.false.
          h(1)='Each material should have a unique name. Duplicates'
          h(2)='were found and should be rectified as soon as possible'
          h(3)='to prevent confusion in the model. After editing the '
          h(4)='materials file you should exit prj and re-open the '
          h(5)='model. '
          nbhelp=5
          CALL ASKOK(
     &      'Edit materials file to address duplicate material names?',
     &      '(be sure to save file and re-open the model)',
     &      OK,dok,nbhelp)
          if(OK)then
            lltmp=LASCI  ! file name to edit

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
            call isunix(unixok)
            if(unixok)then
              call addpath(lltmp,longtfile,concat)
            else
              call addpath(lltmp,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            tmode='graph'
            if(teditor(1:2).eq.'vi')tmode='text'
            write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile))
            call runit(doit,tmode)
          endif
        endif
      endif
      RETURN
    
      END

C ************* mkascimat 
C mkascimat: creates ascii material file (version 1.1 or 1.2) based on
C current information in common matdatarray, matnamearray and matgaparray.
C ASCII file LASCI opened on unit IFA. 
      subroutine mkascimat(IFA,LASCI,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144  ! ascii file name
      integer IER         ! error return where zero is ok

      character tab*1,outs*248,outsd*248

      integer matcount,matcatcount  ! used to increment arrays
      integer lncat     ! for length of string
      logical closemat1,closemat2

      tab=','       ! create tab separator.
      matcount=0    ! setup initial overall counter
      matcatcount=0 ! setup initial category counter

C The ASCII file will be overwritten if it exists.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)RETURN

C Set closemat for the version.
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)
      
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
      write(IFA,'(A)') '#  diffusion resistance (MNs g^-1m^-1)'
      write(IFA,'(A)') '#  default thickness (mm)'
      write(IFA,'(A)') '#  flag [-] legacy [o] opaque [t] transparent'
      write(IFA,'(A)') '#       [g] gas or air gap'
      write(IFA,'(A)') '#  '
      write(IFA,'(A)') '# Transparent material additonal attributes:'
      write(IFA,'(2A)')'#  longwave tran (-) solar direct tran (-)',
     &  ' solar reflec out (-) solar refled in (-)'
      write(IFA,'(2A)')'#  visable tran (-) visable reflec out (-)',
     &  ' visable reflec in (-) colour rendering (-)'
      write(IFA,'(A)') '#  '
      write(IFA,'(A)') '# Gas material additional attributes:'
      write(IFA,'(A)') '#  air gap resistance for vert horiz other'
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

C Loop through each of the items (index J) to check if in the current
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
              write(outs,'(f13.4,2F10.3,4f6.3,f10.3,F6.1,2a)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J)
            elseif(matopaq(J)(1:1).eq.'o')then
              write(outs,'(f13.4,2F10.3,4f6.3,f10.3,F6.1,2a)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J)
            elseif(matopaq(J)(1:1).eq.'t')then
              write(outs,'(f13.4,2F10.3,4f6.3,f10.3,F6.1,2a,8F6.3)')
     &          matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &          matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &          matdbthick(J),' ',matopaq(J),
     &          matirtran(J),matsoldrtrn(J),matsoldrotrfl(J),
     &          matsoldrinrfl(J),matvistran(J),matvisotrfl(J),
     &          matvisinrfl(J),matrender(J)
            elseif(matopaq(J)(1:1).eq.'g')then

C If version 1.1 write out standard attributes, if version 2+ also
C include the three air gap resistance values.
              if(closemat1)then
                write(outs,'(f13.4,2F9.3,4f6.3,f10.3,F6.1,2a)')
     &            matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &            matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &            matdbthick(J),' ',matopaq(J)
              elseif(closemat2)then
                write(outs,'(f13.4,2F9.3,4f6.3,f10.3,F6.1,2a,3f7.4)')
     &            matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &            matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &            matdbthick(J),' ',matopaq(J),matgapares(J,1),
     &            matgapares(J,2),matgapares(J,2)
              endif
            elseif(matopaq(J)(1:1).eq.'h')then
              if(closemat1)then
                write(outs,'(f13.4,2F9.3,4f6.3,f10.3,F6.1,2a)')
     &            matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &            matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &            matdbthick(J),' ',matopaq(J)
              elseif(closemat2)then
                write(outs,'(f13.4,2F9.3,4f6.3,f10.3,F6.1,2a,3f7.4)')
     &            matdbcon(J),matdbden(J),matdbsht(J),matdboute(J),
     &            matdbine(J),matdbouta(J),matdbina(J),matdbdrv(J),
     &            matdbthick(J),' ',matopaq(J),matgapares(J,1),
     &            matgapares(J,2),matgapares(J,2)
              endif
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
      subroutine rascimat(IFA,LASCI,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144  ! ascii file name
      integer IER         ! error return where zero is ok and -2 signals
                          ! that the expected header was not found
                          ! ier=-1 file not found, ier=-2 no classes
      COMMON/OUTIN/IUOUT,IUIN
      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

C Name of current application - only invoke editing if prj.
      common/APPNAME/cAppName
      character cAppName*12

      character tmode*8
      CHARACTER DOIT*300

      DIMENSION VAL(19)
      CHARACTER NDESC*72,OUTSTR*124,WORD*24, WORD4*4
      character WORD32*32,WORD248*248,LOUTSTR*248
      logical closemat1,closemat2
      character namelist*32,nametocheck*32  ! to check for duplicate names
      character outs*132,lltmp*144,longtfile*144,longtfiledos*144
      dimension namelist(600)
      integer loop,ih  ! loop indices
      integer iln,iln2 ! length of name strings
      logical dupfound,concat,unixok,ok,dok
      integer iCFCtype, ival

C Open the ASCII file for reading.
      dupfound = .false.  ! assume no duplicate names
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)RETURN
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

      closemat1=.false.
      closemat2=.false.

C Proceed with header.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'material header',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(OUTSTR,K,WORD,'-','*material',IER)
      if(WORD(1:10).eq.'*Materials'.or.WORD(1:10).eq.'*materials')then

C Clear the materials common blocks.
        matdbitems = 0   ! reset the total number of items.
        do ij=1,MGCL
          matcatname(ij)=' '
          matcatdoc(ij)=' '
          matcatitems(ij)=0
        end do

C Fill the hash table with not included marker (-1) and clear the
C array of duplicate checking names and materials data.
        nametocheck=' '
        do ih=0,600
          mathash(ih)=-1
          if(ih.gt.0)namelist(ih)=' '
          if(ih.gt.0)matname(ih)=' '
          if(ih.gt.0)matdoc(ih)=' '
          matopaq(ih)=' '
          if(ih.gt.0)matcatindex(ih)=0
        end do

        matver = 1.1   ! set initial version of database.
        CALL EGETWR(OUTSTR,K,VAL(1),1.0,1.2,'-','db mat version',IER)
        if(VAL(1).gt.1.0) matver = VAL(1)
        call eclose(matver,1.1,0.001,closemat1)
        call eclose(matver,1.2,0.001,closemat2)
      else

C Did not find the correct header so this is probably a legacy file.
        ier=-2
        CALL ERPFREE(IFA,ISTAT)
        return
      endif

  41  CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'header lines',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'-','*date',IER)
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(LOUTSTR,K,matdbdate,'W','date stamp',IER)
        goto 41
      elseif(WORD(1:4).eq.'*Doc'.or.WORD(1:4).eq.'*doc')then
        CALL EGETRM(LOUTSTR,K,matdbdoc,'W','database doc',IER)

C and the next line is number of classes...
        CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'nb of classes',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(LOUTSTR,K,matcats,1,MGCL,'W','nb of classes',IER)
        goto 41
      elseif(WORD(1:6).eq.'*Class'.or.WORD(1:6).eq.'*class')then

C Get class index, nb of items and class name. Then read the next line
C as documentation for the class. Untill the next *class line the
C class index IC is used for subsequent items.
        CALL EGETWI(LOUTSTR,K,IC,0,MGCL,'F','class',IER)
        CALL EGETWI(LOUTSTR,K,NE,0,19,'-','no items',IER)
        matcatitems(IC)=NE
        CALL EGETRM(LOUTSTR,K,NDESC,'-','description',IER)
        write(matcatname(IC),'(a)') NDESC(1:lnblnk(NDESC))

        CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'class documentation',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETRM(LOUTSTR,K,WORD248,'W','categ doc',IER)
        write(matcatdoc(IC),'(a)') WORD248(1:lnblnk(WORD248))
        goto 41
      elseif(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then

C The item line includes a 'legacy index' for the record number in the
C binary materials file. Traditionally this would be a positive number
C for a non-gas material, zero for a gap/gas material or a -99 if there
C was some confusion associated with the material or it could not have
C been represented in the binary materials file.

C << the overloading of -99 is an issue to deal with! >>

        matdbitems=matdbitems+1   ! increment counter
        CALL EGETP(LOUTSTR,K,WORD32,'-','item name',IER) ! name might contain a space

C Record the name and then check and see if it is a duplicate.
        lex=MIN0(lnblnk(WORD32),32)
        write(namelist(matdbitems),'(a)') WORD32(1:lex)
        write(nametocheck,'(a)') WORD32(1:lex)
        iln=lnblnk(nametocheck)  ! length of name to test
        if(matdbitems.gt.1)then
          loop=matdbitems-1
          do 43 ih=1,loop
            iln2=lnblnk(namelist(ih))  ! length of prior name to test
            if(iln.eq.iln2)then
              if(namelist(ih)(1:iln2).eq.nametocheck(1:iln))then
                write(outs,'(3a,i4,3a,i4,a)') 'Duplicate names ',
     &            nametocheck(1:iln),' (',matdbitems,') vs ',
     &            namelist(ih)(1:iln2),' (',ih,
     &            '). Material names must be unique!'
                call edisp(iuout,outs)
                dupfound = .true.  ! a duplicate name found
              endif
            else
              continue
            endif
  43      continue
        endif

        write(matname(matdbitems),'(a)') WORD32(1:lnblnk(WORD32))
        CALL EGETWI(LOUTSTR,K,NCODE,-99,600,'F','legacy index',IER)
        matlegindex(matdbitems)=NCODE
        if(NCODE.ge.0.and.NCODE.le.600)then
          mathash(NCODE)=matdbitems  ! set up pointer from legacy index to array
        endif
        CALL EGETWI(LOUTSTR,K,matcatindex(matdbitems),1,MGCL,'F',
     &    'category index',IER)
        CALL EGETRM(LOUTSTR,K,WORD248,'-','item doc',IER)
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

C Read single layer optics if there are more than 10 items in line.
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
        elseif(WORD32(1:1).eq.'g')then   ! gas

C If marked with a 'g' and there are 13 ir more than 20 items on the
C line  then read air gap resistance but otherwise skip the rest of the line.
          matopaq(matdbitems)='g'
          if(closemat1.or.closemat2)then
            if(ND.eq.13.or.ND.gt.20)then
              CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-',
     &          'db air res vert',IER)
              CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-',
     &          'db air res floor:ceil',IER)
              CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-',
     &          'db air res other',IER)
              matgapares(matdbitems,1)=VAL(1)
              matgapares(matdbitems,2)=VAL(2)
              matgapares(matdbitems,3)=VAL(3)
            endif
          endif
        elseif(WORD32(1:1).eq.'h')then   ! gas

C If marked with a 'h' save matopaq as a 'g'. And if there are 13 or more 
C than 20 items on the line  then read air gap resistance but otherwise skip
C the rest of the line.
          matopaq(matdbitems)='g'
          if(closemat1.or.closemat2)then
            if(ND.eq.13.or.ND.gt.20)then
              CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-',
     &          'db air res vert',IER)
              CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-',
     &          'db air res floor:ceil',IER)
              CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-',
     &          'db air res other',IER)
              matgapares(matdbitems,1)=VAL(1)
              matgapares(matdbitems,2)=VAL(2)
              matgapares(matdbitems,3)=VAL(3)
            endif
          endif

        endif

        goto 41
      elseif(WORD(1:4).eq.'*End'.or.WORD(1:4).eq.'*end')then

C End of file marker.
        CALL ERPFREE(IFA,ISTAT)
      else

C << did not find the correct header.... >>
        write(word248,'(2a)') 'Unrecognised in line ',
     &    outstr(1:lnblnk(outstr))
        call edisp248(iuout,WORD248,100)
        CALL ERPFREE(IFA,ISTAT)
        ier=-2
        return
      endif
      
      CALL ERPFREE(IFA,ISTAT)

C If it is a local materials file and there has been a duplicate
C name discovered and the application is 'prj' then offer the user
C the option of editing the materials files.
      if(cAppName(1:3).eq.'prj')then
        continue
      else
        return
      endif
      if(dupfound)then
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          dok=.false.
          CALL ASKOK(
     &      'Edit materials file to address duplicate material names?',
     &      '(be sure to save file and re-open the model)',
     &      OK,dok,nbhelp)
          if(OK)then
            lltmp=LASCI  ! file name to edit

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
            call isunix(unixok)
            if(unixok)then
              call addpath(lltmp,longtfile,concat)
            else
              call addpath(lltmp,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            tmode='graph'
            if(teditor(1:2).eq.'vi')tmode='text'
            write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile))
            call runit(doit,tmode)
          endif
        endif
      endif

      RETURN
    
      END

C ************* getnextascislot 
C getnextascislot: Finds the next empty slot in the 0-600 list
C of legacy material indices.
C Where: ip is point to start looking from
C        inext is slot found.
      subroutine getnextascislot(ip,inext)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C mathash is a pointer to the array index in matdatarray for a
C   given legacy index e.g array_index = mathash(legacy_index)
C   if return is negative then there is no pointer. The array
C   starts at zero to account for the implied air magerial.
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

C ************* MATFROMBIN 
C MATFROMBIN: Fills materials common blocks via scan of a legacy
C binary materials database.  It should be called after an initial 
C scan of the binary file establishes that the file is ok for scanning.
C Other code blocks can check the value of natdbitems is non-zero.
C If the returned value of IER is zero then scan was ok.

C << this could be part of a legacy file conversion facility >>

      SUBROUTINE MATFROMBIN(IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"

C Parameters
      integer IER   ! error return where zero is ok, 
                    ! ier=-1 file not found, ier=-2 no classes
                    ! ier=-3 read error

      integer lnblnk  ! function definition
      integer icln
      real val
      DIMENSION ICLN(30),VAL(6)
      CHARACTER IDESC*72,NDESC*72
      character lworking*144,fs*1
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Open the binary materials database (in clase it was closed elsewhere).
      CALL ERPFREE(IFMAT,ISTAT)

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use eforan. If in
C the standard location then create lworking which has path
C prepended.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        ier=0
        call EFOPRAN(IFMAT,LFMAT,40,1,IER)
      elseif(ipathmat.eq.2)then
        ier=0
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMAT(1:lnblnk(LFMAT))
        call EFOPRAN(IFMAT,lworking,40,1,IER)
      endif

      if(ier.ne.0)then
        CALL ERPFREE(IFMAT,ISTAT)  ! free unit if we failed
        ier=-1
        return
      endif
      write(currentfile,'(a)') LFMAT(1:lnblnk(LFMAT))

C Reset ier state prior to reading data.
      ier=0

C Get date stamp for the materials arrays.
      call dstamp(matdbdate)

C Initiate documentation for the database (new concept).
      write(matdbdoc,'(4a)')
     & 'ESP-r Materials based on scan of a binary materials',
     & 'database. Information in the file is recorded as found. ',
     & 'Additional infomation and documentation uses standard ',
     & 'assumptions which should be updated as required.' 

C Check binary contents. As this subroutine will often be passed
C the ASCII version of the file and a read fail will indicate that
C the file is not binary there is no need to inform the user.
      IREC=601
      READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=999)NCLASS,(ICLN(K),K=1,30)
      IF(NCLASS.LE.0)THEN
        CALL USRMSG('binary materials database empty!',
     &    'checking further...','-')
        CALL ERPFREE(IFMAT,ISTAT)  ! free unit if we failed
        IER=-2
        RETURN
      ENDIF

C Clear the materials common blocks.
      matdbitems = 0   ! reset the total number of items.

      do ij=1,MGCL
        matcatname(ij)=' '
        matcatdoc(ij)=' '
        matcatitems(ij)=0
      end do

C Fill the hash table with not included marker (-1)
      do ih=0,600
        mathash(ih)=-1
        if(ih.gt.0)matname(ih)=' '
        if(ih.gt.0)matdoc(ih)=' '
        matopaq(ih)=' '  ! allow zero so that iprmat of zero is ok
        if(ih.gt.0)matcatindex(ih)=0
      end do

C Proceed with the assumption that ICLN increments in the same order as
C 1 -> NCLASS increments. ICLN is not held in the common blocks.
      matcats=NCLASS
      DO 10 I=1,NCLASS
        IC=ICLN(I)
        IREC=IC*20-19
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC

C Save number of entities in the class and the 1st 32 char as a name for class.
        matcatitems(I)=NE
        write(matcatname(I),'(a)') IDESC(1:32)

C Create initial classification documentation
        write(matcatdoc(I),'(a,i2,3a)')
     &    'Category (',I,') was initially named ',
     &    IDESC(1:lnblnk(IDESC)),'. No other documentation (yet).'

C If the class has some entries loop.
        IF(NE.GT.0)then
          DO 20 J=1,NE
            matdbitems=matdbitems+1   ! increment counter
            IREC=IREC+1
            READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1001)(VAL(IL),IL=1,6),
     &        NDESC
            NCODE=IREC-1
            matlegindex(matdbitems)=NCODE
            matcatindex(matdbitems)=I
            if(NCODE.ge.0.and.NCODE.le.600)then
              mathash(NCODE)=matdbitems  ! set up pointer from legacy index to array
            endif

C The MLC database will expect to find the name of the material matching the
C description NDESC initial characters. Write the initial 32 char of NDESC 
C as the name.
            lex=MIN0(lnblnk(NDESC),32)
            write(matname(matdbitems),'(a)') NDESC(1:lex)
            write(matdoc(matdbitems),'(a)') NDESC(1:lnblnk(NDESC))
            matdbcon(matdbitems)=VAL(1)
            matdbden(matdbitems)=VAL(2)
            matdbsht(matdbitems)=VAL(3)
            matdboute(matdbitems)=VAL(4)
            matdbine(matdbitems)=VAL(4)
            matdbouta(matdbitems)=VAL(5)
            matdbina(matdbitems)=VAL(5)
            matdbdrv(matdbitems)=VAL(6)
            matopaq(matdbitems)='-'

C Default thickness does not exist in legacy file so set a place
C holder based on typcial classes (for databases with 15-17 classes)
            if(matcats.ge.15.and.matcats.le.17)then
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

C Assume that category 13 is glass.
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
   20     CONTINUE
        ENDIF
   10 CONTINUE

C If there has been no air material read in then add in one more
C category for air and fill it with default information.
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
        matgapares(matdbitems,1)=0.17
        matgapares(matdbitems,2)=0.17
        matgapares(matdbitems,3)=0.17
      endif

      matver = 1.2   ! set version of materials structures.

C Debug.
C      write(6,*) 'b matdbitems ',matdbitems
C      write(6,*) 'b matcatitems ',matcatitems
C      write(6,*) ' '
C      write(6,*) 'b mathash ',mathash
C      write(6,*) ' '
C      write(6,*) 'b matlegindex ',matlegindex
C      write(6,*) ' '

      CALL ERPFREE(IFMAT,ISTAT)  ! free unit before return
      RETURN

C File errors.
  999 IER= -3
      matver = 0.0   ! signal that we have not filled commons.
      return

 1000 CALL USRMSG('MATFROMBIN: NE,NDESC read error in ',LFMAT,'W')
      matver = 0.0   ! signal that we have not filled commons.
      IER= -3
      return

 1001 CALL USRMSG('MATFROMBIN: Layer data read error in ',LFMAT,'W')
      matver = 0.0   ! signal that we have not filled commons.
      IER= -3
      return

      END

C ********** scananymat
C scananymat scans any materials database and fills common blocks. First
C assume the current string in LFMAT it is a binary file, check
C its contents and if a problem then scan new ascii format and 
C if that does not work try the older ascii materials file to
C fill the materials data arrays.
C If sucessful the material common blocks will be filled and ier
C will be returned as zero.

      subroutine scananymat(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer ier

      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2
      character outs*248
      character lworking*144,fs*1
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C << revise initial assumption that materials file was binary >>

C Scan the binary file data into materials commons and if this was
C sucessful and matver was set to 1.1 in matformbin then we can
C carry on using the materials common blocks for subsequent access.
      call MATFROMBIN(IER)  ! fill materials common blocks
      if(ier.eq.0)then
        origmatwasbin=.true.   ! remember this for subsequent update
        call eclose(matver,1.1,0.001,closemat1)
        call eclose(matver,1.2,0.001,closemat2)
        if(closemat1.or.closemat2)then
          MATDBOK=.TRUE.
          goto 139  !  commons filled carry on.
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
      elseif(ier.eq.-2.or.ier.eq.-3)then
        origmatwasbin=.false.  ! the material file was not binary
        ier=0  ! clear error state prior to rascimat call
        goto 1000
      elseif(ier.eq.-1)then
        origmatwasbin=.false.  ! the material file was not binary
        write(outs,'(3a)') ' Materials db ',
     &    LFMAT(1:lnblnk(LFMAT)),' not found!'
        call edisp248(iuout,outs,100)
        call edisp(iuout,' ')
        MATDBOK=.FALSE.
        ier=0  ! clear error state before continuing
        goto 139  !  try the other databases.
      endif

C Check for ascii versions of the materials database.
 1000 CALL ERPFREE(IFMAT,ISTAT)       ! if a read error look at alternatives.
      if(ipathmat.eq.0.or.ipathmat.eq.1)then
        call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
      elseif(ipathmat.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMAT(1:lnblnk(LFMAT))
        call rascimat(IFMAT,lworking,IER)  ! try current ascii format
      endif
      if(ier.eq.-2.or.ier.eq.-3)then               ! expected header not found
        ier=0
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))
          call rlegacymat(IFMAT,lworking,ier)  ! try older ascii format
        endif

        if(ier.ne.0)then
          call usrmsg('No readable materials database was not found',
     &                'or file was corrupt','W')
          MATDBOK=.FALSE.
        else
          call eclose(matver,1.1,0.001,closemat1)
          call eclose(matver,1.2,0.001,closemat2)
          if(closemat1.or.closemat2)then
            MATDBOK=.TRUE.
          else
            call usrmsg(
     &        'No readable Materials database was found!',
     &        ' ','W')
            MATDBOK=.FALSE.
          endif
        endif
      elseif(ier.eq.0)then
        call eclose(matver,1.1,0.001,closemat1)
        call eclose(matver,1.2,0.001,closemat2)
        if(closemat1.or.closemat2)then
          MATDBOK=.TRUE.
        endif
      endif

C Scan complete so return.
 139  CALL ERPFREE(IFMUL,ISTAT)
      return

      end

C ********** scancfcdb
C scancfcdb scans any CFC layers database and fills common blocks. 
C The new ascii materials db format is assumed.
C If sucessful the material common blocks will be filled and ier
C will be returned as zero.

      subroutine scancfcdb(ier)
#include "building.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer ier

      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      character outs*248
      character lworking*144,fs*1
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Check for ascii versions of the CFC layers database.
      if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
        call rascicfc(ICFCDB,LCFCDB,IER)  ! try current ascii format
      elseif(ipathcfc.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LCFCDB(1:lnblnk(LCFCDB))
        call rascicfc(ICFCDB,lworking,IER)  ! try current ascii format
      endif
      if(ier.ne.0)then
        call usrmsg('No readable CFC layers database was found',
     &                'or file was corrupt','W')
        CFCDBOK=.FALSE.
      elseif(ier.eq.0)then
        CFCDBOK=.TRUE.
      endif

C Scan complete so return.
      return

      end

C ************* rascicfc
C rascicfc: Fills common cfcdatarray and cfcnamearray from
C current ASCII CFC layers database LASCI opened on unit IFA. 
      subroutine rascicfc(IFA,LASCI,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144  ! ascii file name
      integer IER         ! error return where zero is ok and -2 signals
                          ! that the expected header was not found
                          ! ier=-1 file not found, ier=-2 no classes
      COMMON/OUTIN/IUOUT,IUIN
      common/texted/tedlbl,teditor
      character tedlbl*20,teditor*20

C Name of current application - only invoke editing if prj.
      common/APPNAME/cAppName
      character cAppName*12

      character tmode*8
      CHARACTER DOIT*300

      DIMENSION VAL(19)
      CHARACTER NDESC*124,OUTSTR*124,WORD*24, WORD4*4, WORD8*8
      character WORD32*32,WORD248*248,LOUTSTR*248
      character namelist*32,nametocheck*32  ! to check for duplicate names
      character outs*132,lltmp*144,longtfile*144,longtfiledos*144
      dimension namelist(6000)
      integer loop,ih  ! loop indices
      integer iln,iln2 ! length of name strings
      logical dupfound,concat,unixok,ok,dok
      integer iCFCtype, ival

C Open the ASCII file for reading.
      dupfound = .false.  ! assume no duplicate names
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)RETURN
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

C Proceed with header.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'CFC layers header',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(OUTSTR,K,WORD,'-','*CFClayers',IER)
      if(WORD(1:10).eq.'*CFClayers'.or.WORD(1:10).eq.'*cfclayers')then

C Clear the CFC layers common blocks.
        cfcdbitems = 0   ! reset the total number of items.
        do ij=1,MGCL_CFC
          cfccatname(ij)=' '
          cfccatdoc(ij)=' '
          cfccatitems(ij)=0
        end do

        cfcver = 1.1   ! set initial version of database.
        CALL EGETWR(OUTSTR,K,VAL(1),1.0,1.2,'-','db cfc version',IER)
        if(VAL(1).gt.1.0) cfcver = VAL(1)
      else

C Did not find the correct header so quit with error.
        ier=-2
        CALL ERPFREE(IFA,ISTAT)
        return
      endif

  41  CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'header lines',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'-','*date',IER)
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(LOUTSTR,K,cfcdbdate,'W','date stamp',IER)
        goto 41
      elseif(WORD(1:4).eq.'*Doc'.or.WORD(1:4).eq.'*doc')then
        CALL EGETRM(LOUTSTR,K,cfcdbdoc,'W','database doc',IER)

C and the next line is number of classes...
        CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'nb of classes',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(LOUTSTR,K,cfccats,1,MGCL_CFC,
     &               'W','nb of classes',IER)
        goto 41
      elseif(WORD(1:6).eq.'*Class'.or.WORD(1:6).eq.'*class')then

C Get class index, nb of items and class name. Then read the next line
C as documentation for the class. Untill the next *class line the
C class index IC is used for subsequent items.
        CALL EGETWI(LOUTSTR,K,IC,0,MGCL_CFC,'F','class',IER)
        CALL EGETWI(LOUTSTR,K,NE,0,19,'-','no items',IER)
        cfccatitems(IC)=NE
        CALL EGETRM(LOUTSTR,K,NDESC,'-','description',IER)
        write(cfccatname(IC),'(a)') NDESC(1:lnblnk(NDESC))

        CALL LSTRIPC(IFA,LOUTSTR,0,ND,1,'class documentation',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETRM(LOUTSTR,K,WORD248,'W','categ doc',IER)
        write(cfccatdoc(IC),'(a)') WORD248(1:lnblnk(WORD248))
        goto 41
      elseif(WORD(1:5).eq.'*Item'.or.WORD(1:5).eq.'*item')then

        cfcdbitems=cfcdbitems+1   ! increment counter
        CALL EGETP(LOUTSTR,K,WORD32,'-','item name',IER) ! name might contain a space

C Record the name and then check and see if it is a duplicate.
        lex=MIN0(lnblnk(WORD32),32)
        write(namelist(cfcdbitems),'(a)') WORD32(1:lex)
        write(nametocheck,'(a)') WORD32(1:lex)
        iln=lnblnk(nametocheck)  ! length of name to test
        if(cfcdbitems.gt.1)then
          loop=cfcdbitems-1
          do 43 ih=1,loop
            iln2=lnblnk(namelist(ih))  ! length of prior name to test
            if(iln.eq.iln2)then
              if(namelist(ih)(1:iln2).eq.nametocheck(1:iln))then
                write(outs,'(3a,i4,3a,i4,a)') 'Duplicate names ',
     &            nametocheck(1:iln),' (',cfcdbitems,') vs ',
     &            namelist(ih)(1:iln2),' (',ih,
     &            '). CFC layer names must be unique!'
                call edisp(iuout,outs)
                dupfound = .true.  ! a duplicate name found
              endif
            else
              continue
            endif
  43      continue
        endif

        write(cfcname(cfcdbitems),'(a)') WORD32(1:lnblnk(WORD32))
        CALL EGETWI(LOUTSTR,K,cfcitmindex(cfcdbitems),1,MGIT_CFC,'F',
     &    'item index',IER)
        CALL EGETWI(LOUTSTR,K,cfccatindex(cfcdbitems),1,MGCL_CFC,'F',
     &    'category index',IER)
        CALL EGETRM(LOUTSTR,K,WORD248,'-','item doc',IER)
        write(cfcdoc(cfcdbitems),'(a)') WORD248(1:lnblnk(WORD248))

        CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'the raw data line',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWR(LOUTSTR,K,VAL(1),0.,0.,'-','db con',IER)
        CALL EGETWR(LOUTSTR,K,VAL(2),0.,0.,'-','db den',IER)
        CALL EGETWR(LOUTSTR,K,VAL(3),0.,0.,'-','db sht',IER)
        CALL EGETWR(LOUTSTR,K,VAL(4),0.,0.,'-','db def thick',IER)
        cfcdbcon(cfcdbitems)=VAL(1)
        cfcdbden(cfcdbitems)=VAL(2)
        cfcdbsht(cfcdbitems)=VAL(3)
        cfcdbthick(cfcdbitems)=VAL(4)

        CALL EGETWI(LOUTSTR,K,iCFCtype,0,0,'-','shade type',IER)
        CFCshdtp(cfcdbitems) = iCFCtype
        if (CFCshdtp(cfcdbitems).eq.iGlazing)then
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-',
     &                    'sol refl out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-',
     &                    'sol refl in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-',
     &                    'sol tran dir',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','emiss out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-','emiss in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-','lw trans',IER)
            CFCsolreflout(cfcdbitems) = VAL(5)
            CFCsolreflin(cfcdbitems) = VAL(6)
            CFCsoltrandir(cfcdbitems) =  VAL(7)
            CFCemissout(cfcdbitems) = VAL(8)
            CFCemissin(cfcdbitems) = VAL(9)
            CFClwtran(cfcdbitems) = VAL(10)
            if(ND.eq.12)then
              CALL EGETW(LOUTSTR,K,WORD8,'-','IGDB ID',IER)
              CFC_IGDB_ID(cfcdbitems) = WORD8              
            else
              CFC_IGDB_ID(cfcdbitems) = ''           
            endif
        else if(CFCshdtp(cfcdbitems).eq.iVenBlind)then
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-',
     &                    'sol refl out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-',
     &                    'sol refl in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-','slat tran',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-','emiss out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-','emiss in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-','lw tran',IER)
            CALL EGETWR(LOUTSTR,K,VAL(11),0.,0.,'-',
     &                    'slat width',IER)
            CALL EGETWR(LOUTSTR,K,VAL(12),0.,0.,'-','slat space',IER)
            CALL EGETWR(LOUTSTR,K,VAL(13),0.,0.,'-',
     &                    'slat angle',IER)
            CALL EGETW(LOUTSTR,K,WORD4,'-','slat orient',IER)
            CALL EGETWR(LOUTSTR,K,VAL(14),0.,0.,'-',
     &                    'slat crown',IER)
            CALL EGETWR(LOUTSTR,K,VAL(15),0.,0.,'-',
     &                    'slat wr ratio',IER)
            CALL EGETWR(LOUTSTR,K,VAL(16),0.,0.,'-','slat thk',IER)
            CFCsolreflout(cfcdbitems) = VAL(5)
            CFCsolreflin(cfcdbitems) = VAL(6)
            CFCslattran(cfcdbitems) = VAL(7)
            CFCemissout(cfcdbitems) = VAL(8)
            CFCemissin(cfcdbitems) = VAL(9)
            CFClwtran(cfcdbitems) = VAL(10)
            CFCslatwidth(cfcdbitems) = VAL(11)
            CFCslatspacing(cfcdbitems) = VAL(12)
            CFCslatangle(cfcdbitems) = VAL(13)
            CFCslatorient(cfcdbitems) = WORD4
            CFCslatcrown(cfcdbitems)  = VAL(14)
            CFCslatwr(cfcdbitems) = VAL(15)
            CFCslatthk(cfcdbitems) = VAL(16)
        else if (CFCshdtp(cfcdbitems).eq.iPleatedDrape)then 
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-',
     &                    'sol refl out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-',
     &                    'sol refl in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-',
     &                    'sol tran dir',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-',
     &                    'sol tran tot out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-',
     &                    'sol tran tot in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-',
     &                    'pleat width',IER)
            CALL EGETWR(LOUTSTR,K,VAL(11),0.,0.,'-',
     &                    'pleat space',IER)
            CFCsolreflout(cfcdbitems) = VAL(5)
            CFCsolreflin(cfcdbitems) = VAL(6)
            CFCsoltrandir(cfcdbitems) =  VAL(7)
            CFCsoltrantotout(cfcdbitems) = VAL(8)
            CFCsoltrantotin(cfcdbitems) = VAL(9)
            CFCdrpwidth(cfcdbitems) = VAL(10)
            CFCdrpspacing(cfcdbitems) = VAL(11)
        else if (CFCshdtp(cfcdbitems).eq.iRollerBlind)then
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-',
     &                    'sol refl out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-',
     &                    'sol refl in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-',
     &                    'sol tran dir',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-',
     &                    'sol tran tot out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-',
     &                    'sol tran tot in',IER)
            CFCsolreflout(cfcdbitems) = VAL(5)
            CFCsolreflin(cfcdbitems) = VAL(6)
            CFCsoltrandir(cfcdbitems) =  VAL(7)
            CFCsoltrantotout(cfcdbitems) = VAL(8)
            CFCsoltrantotin(cfcdbitems) = VAL(9)
        else if (CFCshdtp(cfcdbitems).eq.iInsectScreen)then
            CALL EGETWR(LOUTSTR,K,VAL(5),0.,0.,'-',
     &                    'sol refl out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(6),0.,0.,'-',
     &                    'sol refl in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(7),0.,0.,'-',
     &                    'sol tran dir',IER)
            CALL EGETWR(LOUTSTR,K,VAL(8),0.,0.,'-',
     &                    'sol tran tot out',IER)
            CALL EGETWR(LOUTSTR,K,VAL(9),0.,0.,'-',
     &                    'sol tran tot in',IER)
            CALL EGETWR(LOUTSTR,K,VAL(10),0.,0.,'-','wire emiss',IER)
            CALL EGETWR(LOUTSTR,K,VAL(11),0.,0.,'-','wire diam',IER)
            CALL EGETWR(LOUTSTR,K,VAL(12),0.,0.,'-',
     &                    'wire space',IER)
            CFCsolreflout(cfcdbitems) = VAL(5)
            CFCsolreflin(cfcdbitems) = VAL(6)
            CFCsoltrandir(cfcdbitems) =  VAL(7)
            CFCsoltrantotout(cfcdbitems) = VAL(8)
            CFCsoltrantotin(cfcdbitems) = VAL(9)
            CFCwireemiss(cfcdbitems) = VAL(10)
            CFCwirediam(cfcdbitems) = VAL(11)
            CFCwirespace(cfcdbitems) = VAL(12)
        else if (CFCshdtp(cfcdbitems).eq.iGasGap)then
            CALL EGETWI(LOUTSTR,K,ival,0,100,'-',
     &           '% mole frac. air',IER)
            CFCfillAir(cfcdbitems) = ival
            CALL EGETWI(LOUTSTR,K,ival,0,100,'-',
     &           '% mole frac. Ar',IER)
            CFCfillAr(cfcdbitems) = ival
            CALL EGETWI(LOUTSTR,K,ival,0,100,'-',
     &           '% mole frac. Kr',IER)
            CFCfillKr(cfcdbitems) = ival
            CALL EGETWI(LOUTSTR,K,ival,0,100,'-',
     &           '% mole frac. Xe',IER)
            CFCfillXe(cfcdbitems)  = ival
            CALL EGETWI(LOUTSTR,K,ival,0,100,'-',
     &           '% mole frac. SF6',IER)
            CFCfillSF6(cfcdbitems) = ival
        end if

        goto 41
      elseif(WORD(1:4).eq.'*End'.or.WORD(1:4).eq.'*end')then

C End of file marker.
        CALL ERPFREE(IFA,ISTAT)
      else

C << did not find the correct header.... >>
        write(word248,'(2a)') 'Unrecognised in line ',
     &    outstr(1:lnblnk(outstr))
        call edisp248(iuout,WORD248,100)
        CALL ERPFREE(IFA,ISTAT)
        ier=-2
        return
      endif

C If it is a local CFC layers file and there has been a duplicate
C name discovered and the application is 'prj' then offer the user
C the option of editing the CFC layers files.
      if(cAppName(1:3).eq.'prj')then
        continue
      else
        return
      endif
      if(dupfound)then
        if(ipathcfc.eq.0.or.ipathcfc.eq.1)then
          dok=.false.
          CALL ASKOK(
     &      'Edit CFC layers file to address duplicate names?',
     &      '(be sure to save file and re-open the model)',
     &      OK,dok,nbhelp)
          if(OK)then
            lltmp=LASCI  ! file name to edit

C Depending on whether Unix or DOS based setup paths. If DOS
C then check for spaces in name and change / to \.
            call isunix(unixok)
            if(unixok)then
              call addpath(lltmp,longtfile,concat)
            else
              call addpath(lltmp,longtfile,concat)
              call cmdfiledos(longtfile,longtfiledos,ier)
              longtfile=' '
              longtfile=longtfiledos
            endif
            tmode='graph'
            if(teditor(1:2).eq.'vi')tmode='text'
            write(doit,'(a,2x,a)') teditor(1:lnblnk(teditor)),
     &        longtfile(1:lnblnk(longtfile))
            call runit(doit,tmode)
          endif
        endif
      endif

      RETURN
    
      END


C ************* mkascicfc 
C mkascicfc: creates ascii CFC layers file (version 1.1) based on
C current information in common cfcdatarray, cfcnamearray.
C ASCII file LASCI opened on unit IFA. 
      subroutine mkascicfc(IFA,LASCI,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144  ! ascii file name
      integer IER         ! error return where zero is ok

      character tab*1,outs*248,outsd*248

      integer cfccount,cfccatcount  ! used to increment arrays
      integer lncat     ! for length of string
      logical closemat1

      tab=','       ! create tab separator.
      cfccount=0    ! setup initial overall counter
      cfccatcount=0 ! setup initial category counter

C The ASCII file will be overwritten if it exists.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)RETURN

C Set closemat for the version.
      call eclose(cfcver,1.1,0.001,closemat1)
      
      write(IFA,'(A,F3.1)') '*CFClayers ',cfcver
      write(IFA,'(3A)') '*date',tab,cfcdbdate
      write(IFA,'(2A)') '# CFC layers database defined in ',
     &  LASCI(1:lnblnk(LASCI))
      write(IFA,'(3A)') '*doc',tab,cfcdbdoc(1:lnblnk(cfcdbdoc))
      write(IFA,'(i2,a)') cfccats,'  # number of classifications'
      write(IFA,'(A)') '#  '
      write(IFA,'(A)') '# CFC layers have the following attributes:'
      write(IFA,'(2A)') '#  conductivity (W/(m-K), density (kg/m**3)',
     &  ' specific heat (J/(kg-K)'
      write(IFA,'(A)') '#  default thickness (mm)'
      write(IFA,'(A)') '#  '

C Loop through each classification (index I) and write out contents.
      do 10 I=1,cfccats
        cfccatcount=0 ! zero items in category counter
        write(IFA,'(A)') 
     &    '# class index |nb items|description (32 char)'
        lncat=lnblnk(cfccatname(I))
        write(IFA,'(2A,i2,a,i2,2a)') '*class',tab,I,tab,cfccatitems(I),
     &    tab,cfccatname(I)(1:lncat)
        lncat=lnblnk(cfccatdoc(I))
        write(IFA,'(a)') cfccatdoc(I)(1:lncat)

C Loop through each of the items (index J) to check if in the current
C category.  This has the effect of packing the file and sorting the
C common block arrays so that future reads are more efficient.
        do 20 J=1,cfcdbitems
          if(cfccatindex(J).eq.I)then
            cfccatcount=cfccatcount+1 ! increment items in category counter
            if(cfccatcount.gt.cfccatitems(I))then
              call usrmsg('Number of items linked with a category ',
     &                    'exceeds expected count.','W')
            endif
            cfccount=cfccount+1   ! increment counter
            if(cfccount.gt.cfcdbitems)then
              call usrmsg('Number of CFC layers scanned exceeds',
     &                    'expected count for database.','W')
            endif
            lnna=lnblnk(cfcname(J))
            lndo=lnblnk(cfcdoc(J))
            write(IFA,'(4a,i4,a,i4,2a)') '*item',tab,
     &        cfcname(J)(1:lnna),tab,cfcitmindex(J),
     &        tab,cfccatindex(J),tab,cfcdoc(J)(1:lndo)

            if(CFCshdtp(J).eq.iGasGap)then
              write(outs,'(f13.3,3F10.3,6i4)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCfillAir(J),CFCfillAr(J),CFCfillKr(J),
     &          CFCfillXe(J),CFCfillSF6(J)
            elseif(CFCshdtp(J).eq.iGlazing)then
              write(outs,'(f13.3,3F10.3,i2,6F8.3,a8)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCsolreflout(J),CFCsolreflin(J),CFCsoltrandir(J),
     &          CFCemissout(J),CFCemissin(J),CFClwtran(J), 
     &          CFC_IGDB_ID(J)
            elseif(CFCshdtp(J).eq.iVenBlind)then
              write(outs,'(f13.3,3F10.3,i2,9F8.3,a5,3F8.3)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCsolreflout(J),CFCsolreflin(J),CFCslattran(J),
     &          CFCemissout(J),CFCemissin(J),CFClwtran(J),
     &          CFCslatwidth(J),CFCslatspacing(J),CFCslatangle(J),
     &          CFCslatorient(J),CFCslatcrown(J),CFCslatwr(J),
     &          CFCslatthk(J)
            elseif(CFCshdtp(J).eq.iPleatedDrape)then
              write(outs,'(f13.3,3F10.3,i2,7F8.3)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCsolreflout(J),CFCsolreflin(J),CFCsoltrandir(J),
     &          CFCsoltrantotout(J),CFCsoltrantotin(J),
     &          CFCdrpwidth(J),CFCdrpspacing(J)
            elseif(CFCshdtp(J).eq.iRollerBlind)then
              write(outs,'(f13.3,3F10.3,i2,5F8.3)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCsolreflout(J),CFCsolreflin(J),CFCsoltrandir(J),
     &          CFCsoltrantotout(J),CFCsoltrantotin(J)
            elseif(CFCshdtp(J).eq.iInsectScreen)then
              write(outs,'(f13.3,3F10.3,i2,8F8.3)')
     &          cfcdbcon(J),cfcdbden(J),cfcdbsht(J),cfcdbthick(J),
     &          CFCshdtp(J),
     &          CFCsolreflout(J),CFCsolreflin(J),CFCsoltrandir(J),
     &          CFCsoltrantotout(J),CFCsoltrantotin(J),
     &          CFCwireemiss(J),CFCwirediam(J),
     &          CFCwirespace(J)
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
