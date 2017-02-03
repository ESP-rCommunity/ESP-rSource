C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C The routines for reading and writing a predefined objects file are 
C contained in this file:
C A light scan just returns a list of objects, list of menu items
C list of bounding boxes which can be used for initial selection.

C A deep scan is passed the name of the item to recover, the zone
C to place the information in and it should manage the merge of
C objects into the zone.

C At which point does the user needs to specify the transform?
 
C   choosepredef - selects a predefined object and returns its name
C   predefscan - scans a predefined objects file and can be called with 
C             mode 'L' - light scan silent and mode 'R' - reporting mode.
C   predefembed - read tag-data version of predefined objects file.
C   geo2obj - scans ESP-r geometry file and creates initial text block
C             for manual editing and inclusion in predefined.db1
C   opjqa - produces a QA report based on contents of common blocks
C           for a single predefined object.
C   rpredefcom - scans an item in predefined database and fills the
C           common blocks.

C ******************************** CHOOSEPREDEF
C choosepredef - selects a predefined object and returns its name
C and bounding box. It calls predefscan to recover information to 
C present in menus.
      subroutine choosepredef(objectname,objmenu,objbb,ier)
#include "building.h"
#include "esprdbfile.h"
#include "predefined.h"

      integer lnblnk  ! function definition

C Parameters
      character objectname*12   ! the object name to pass to PREDEFEMBED
      character objmenu*32
      real objbb(3) ! width, depth, height
      integer ier   ! return 0 if one selected 1 if problem 2 if none
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Predefined entity variables.
      integer numcat      ! how many categoris found
      integer numitems    ! how many items found
      character catn*12   ! category name to pass to PREDEFEMBED
      character catname*12 ! category names
      character cats*32   ! category menu entries
      character names*12  ! list of names of objects
      character menus*32  ! list of menu entries
      character itemcat*12 ! category for each object

C << use parameters when available >>
      dimension catname(6),cats(6)
      dimension names(42),menus(42),itemcat(42)
      integer pointback(30) ! to point back to item in list
      real bounding(42,3) ! width, depth, height
      character predef*144,fs*1
      integer loop
      logical unixok
      DIMENSION IVALS(MS),ITEMSS(MS)
      CHARACTER ITEMSS*32
     
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='choosepredef'  ! set for subroutine

C Help text for this menu.
      helptopic='select_predefined'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Take into account path to the db.
      lndbp=lnblnk(standarddbpath)
      if(ipathpredef.eq.0.or.ipathpredef.eq.1)then
        predef=LPREDEF  ! use as is
      elseif(ipathpredef.eq.2)then
        write(predef,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPREDEF(1:lnblnk(LPREDEF))  ! prepend db folder path
      endif

      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        CALL EASKF(predef,'Predefined objects file ?',
     &    ' ',96,'predefined.db1','predefined file',IER,nbhelp)
      else
        CALL EASKF(predef,'Predefined objects file ?',
     &    ' ',96,'predefined.db1','predefined file',IER,nbhelp)
      endif

C Scan predefined database returning list of categories & items
      numcat=0; numitems=0
      CALL PREDEFSCAN(IFIL+2,predef,numcat,catname,cats,numitems,
     &  names,menus,itemcat,bounding,IER)
      if(ier.ne.0)then
        objectname='UNKNOWN'
        objmenu='UNKNOWN'
        return
      endif

C Loop to present categories (menus).
      do loop=1,numcat
        write(ITEMSS(loop),'(A)')cats(loop)
      enddo
      inpick=1
      CALL EPICKS(INPICK,IVALS,'Categories of predefined objects',
     &  'Select none to abort.',32,numcat,ITEMSS,
     &  'Predefined categories.',IER,nbhelp)
      if(inpick.gt.0)then
        ifoc=IVALS(inpick)
        if(ifoc.eq.0)then
          ier=2
          objectname='UNKNOWN'
          objmenu='UNKNOWN'
          return
        endif
        write(catn,'(a)') catname(ifoc)
      else
        ier=2
        objectname='UNKNOWN'
        objmenu='UNKNOWN'
        return
      endif

C Loop through items which match the selected category.
      matching=0
      lncatn=lnblnk(catn)
      do loop=1,numitems
        if(itemcat(loop)(1:lncatn).eq.catn(1:lncatn))then
          matching=matching+1
          pointback(matching)=loop
          write(ITEMSS(matching),'(A)')menus(loop)
        endif
      enddo
      if(matching.eq.0)then
        ier=1
        objbb(1)=0.0; objbb(2)=0.0; objbb(3)=0.0
        objectname='UNKNOWN'
        objmenu='UNKNOWN'
        return
      endif

C Now select from matching entries.
      inpick=1
      CALL EPICKS(INPICK,IVALS,'Available predefined objects',
     &  'Select none to abort.',32,matching,ITEMSS,
     &  'Available predefined objects.',IER,nbhelp)
      if(inpick.gt.0)then

C Decode which one and set name and ier.
        ifoc=IVALS(1); ifoc2=pointback(ifoc)
        write(6,*) 'ifoc ',ifoc,ifoc2,menus(ifoc2),names(ifoc2)
        write(objmenu,'(a)') menus(ifoc2)  ! menu entry to pass across
        write(objectname,'(a)') names(ifoc2)  ! name to pass across
        objbb(1)=bounding(ifoc2,1)
        objbb(2)=bounding(ifoc2,2)
        objbb(3)=bounding(ifoc2,3)
        ier=0
        return
      else
        ier=2
        objbb(1)=0.0; objbb(2)=0.0; objbb(3)=0.0
        objectname='UNKNOWN'
      endif
      return
      end

C ******************************** PREDEFSCAN
C Read tag-data version of predefined objects file for high level
C tags to assist user selection.
      SUBROUTINE PREDEFSCAN(IFA,LASCI,numcat,catname,cats,numitems,
     &  names,menus,itemcat,bounding,IER)

#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.

C << Support more categories
C << Support more entities in database

      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144 ! ascii file name
      integer numcat      ! how many categoris found
      integer numitems    ! how many items found

C << use parameters when available >>
      character catname*12 ! category names
      character cats*32   ! category menu entries
      character names*12  ! list of names of objects
      character menus*32  ! list of menus for objects
      character itemcat*12 ! category for each object
      dimension catname(6),cats(6)
      dimension names(42),menus(42),itemcat(42)
      real bounding(42,3) ! width, depth, height for each    
      integer IER         ! error return where zero is ok, 
                          ! ier=-1 file not found, ier=-2 no objects

C Graphics and ESP-r default commons
      COMMON/OUTIN/IUOUT,IUIN

      character OUTS*124,OUTSTR*124,loutstr*248
      character WORD*24,ectime*24,phrase*32
      LOGICAL CONT
      integer loop    ! for looping
      real verpre     ! version of file

C Open the file.
      call EFOPSEQ(IFA,LASCI,1,IER)
      if(IER.EQ.-301) then
        call edisp(IUOUT,'Warning: filename was blank')
        CALL ERPFREE(IFA,ISTAT)
        return
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        WRITE(OUTS,'(3A)')
     &    ' Problem opening ',LASCI(1:LNBLNK(LASCI)),'.'
        call edisp(IUOUT,outs)
        CALL ERPFREE(IFA,ISTAT)
        return
      endif
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))

C Clear the arrays to be returned.
C << use parameters when ready >>
      numcat=0
      do loop=1,6
        cats(loop)=' '
        catname(loop)=' '
      enddo    
      numitems=0
      do loop=1,42
        names(loop)='  '
        menus(loop)='  '
        itemcat(loop)='  '
        bounding(loop,1)=0.0     
        bounding(loop,2)=0.0     
        bounding(loop,3)=0.0
      enddo    
      CONT=.TRUE.
      
C Read the file header and check for first-line tag.
      CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'1st line of pre',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(LOUTSTR(1:11).EQ.'*PREDEFINED')THEN
        verpre=0.0
        if(ND.gt.1)then
          K=11
          CALL EGETWR(LOUTSTR,K,verpre,0.,2.,'-','version',IER)
        endif
        CALL EDISP(IUOUT,' ')    
        WRITE(OUTS,'(3A)') 
     &    'Opened predefined objects file: ',LASCI(1:LNBLNK(LASCI)),'.' 
        CALL USRMSG(OUTS,' ','-') 
      else
        WRITE(OUTS,'(3A)') 'File: ',LASCI(1:LNBLNK(LASCI)), 
     &    ' is not a predefined objects file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        return
      endif 
   
C Read in the header lines of the file, look for key tags.
  20  CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'header lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','predefined tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then

C Read date stamp << not yet used >>.
        CALL EGETRM(LOUTSTR,K,ectime,'W','date stamp',IER)
        IF(IER.NE.0) CONT=.FALSE.
      elseif(WORD(1:5).EQ.'*Text')then
  21    CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'text lines',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','text line',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:9).EQ.'*End_text')then
          continue  ! look for more key words
        else
          goto 21  ! get another line of text
        endif
      elseif(WORD(1:9).EQ.'*End_text')then
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*Category')then
        numcat=numcat+1
        K=9
        CALL EGETW(LOUTSTR,K,WORD,'W','cat name',IER)
        write(catname(numcat),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETRM(LOUTSTR,K,phrase,'W','category menu',IER)
        IF(IER.NE.0) CONT=.FALSE.
        write(cats(numcat),'(a)') phrase(1:lnblnk(phrase))
        continue  ! look for more key words
      elseif(WORD(1:13).EQ.'*End_category')then
        continue  ! look for more key words
      elseif(WORD(1:15).EQ.'*End_predefined')then
        CALL ERPFREE(IFA,ISTAT)
        return  ! reached the end of the file
      elseif(WORD(1:5).EQ.'*item')then
        numitems=numitems+1
        CALL EGETW(LOUTSTR,K,WORD,'W','item name',IER)
        write(names(numitems),'(a)') WORD(1:lnblnk(WORD))
        CALL EGETRM(LOUTSTR,K,phrase,'W','category',IER)
        write(menus(numitems),'(a)') phrase(1:lnblnk(phrase))
        IF(IER.NE.0) CONT=.FALSE.
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*end_item')then
        continue  ! look for more key words
      elseif(WORD(1:6).EQ.'*incat')then
        CALL EGETW(LOUTSTR,K,WORD,'W','assoc category',IER)
        write(itemcat(numitems),'(a)') WORD(1:lnblnk(WORD))
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*includes')then

C work-in-progress.
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*sourced')then
       continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*origin')then
        continue  ! look for more key words
      elseif(WORD(1:13).EQ.'*bounding_box')then
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bb X',IER)
        bounding(numitems,1)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bb Y',IER)
        bounding(numitems,2)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bb Z',IER)
        bounding(numitems,3)=VAL
        IF(IER.NE.0) CONT=.FALSE.
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*bounds')then
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*vertex')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*mass')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*surf')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*visp')then

C visp has two additional lines of data.
        CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'visp lines',IER)
        CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'visp lines',IER)
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*vis3')then

C work-in-progress.
        continue  ! look for more key words
      elseif(WORD(1:4).EQ.'*vis')then

C work-in-progress.
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*vobject')then

C work-in-progress.
        continue  ! look for more key words
      else
        write(outs,'(3a)') 'Unknown tag ',WORD,' continuing...'
        call edisp(iuout,outs)
        continue  ! look for more key words
      endif

C If there were no errors in reading header line then read another.
      if(CONT)then
        goto 20
      else
        call usrmsg('Error reading predefined file @',loutstr,'W')
        ier=1
        CLOSE(IFA)
        RETURN
      endif

      END
       
C ******************************** PREDEFEMBED
C Read tag-data version of predefined objects file.
C Currently entity is found in predefined.h 
      SUBROUTINE PREDEFEMBED(IFA,LASCI,ICOMP,DX,DY,DZ,DA,name,prec,IER)
      
      IMPLICIT NONE
      
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "predefined.h"
#include "espriou.h"
C espriou.h provides currentfile.

      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144 ! ascii file name
      integer icomp       ! zone to embed
      real DX,DY,DZ,DA    ! transform and rotation to apply
      character name*12   ! item to embed
      character prec*1    ! character to pre-pend to names so unique
      integer IER         ! error return where zero is ok, 
                          ! ier=-1 file not found, ier=-2 no objects
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character OUTS*124,loutstr*248,outstr*144
      character WORD*24,phrase*32
      logical CONT,foundit
      logical close
      integer loop,i,j,ibe,ix,ixx,nbv   ! for looping
      real verpre     ! version of file

      integer nbprevious  ! how many visual entities aleady in zone
      integer nbprevnztv  ! how many vertices in zone prior to import
      real A,CA,PI,SA,VAL,VAL1,VX,VY,VZ,YR
      real X1,XR,Y1,XX1,YY1,XXX,YYY
      integer icon,iflag,istat,iv,k,nd
      integer ibvo,nbvo

C Additional data structures to consider:
C character predef_notes*72  ! up to 6 lines of documentation (for popup?)
C dimension predef_notes(6)
C variables signalling shading oiaplic and insolation of interest
      integer oiaplic(2)

C variables signalling linear thermal bridges are of interest
C variables signalling viewfactors are of interest?

C Open the file.
      call EFOPSEQ(IFA,LASCI,1,IER)
      if(IER.EQ.-301) then
        call edisp(IUOUT,'Warning: filename was blank')
        CALL ERPFREE(IFA,ISTAT)
        return
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        WRITE(OUTS,'(3A)')
     &    ' Problem opening ',LASCI(1:LNBLNK(LASCI)),'.'
        call edisp(IUOUT,outs)
        CALL ERPFREE(IFA,ISTAT)
        return
      endif
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))
      foundit=.false.

C Clear variables.
      call clearobjcommons()
      CONT=.TRUE.  ! assume reads are ok

C Remember how many visual entities already exist
C in the zone (so we known which added ones to rotate.
      nbprevious=nbvis(icomp)
      nbprevnztv=NZTV(icomp)
 
C Read the file header and check for first-line tag.
      CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'1st line of pre',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(LOUTSTR(1:11).EQ.'*PREDEFINED')THEN
        verpre=0.0
        if(ND.gt.1)then
          K=11
          CALL EGETWR(LOUTSTR,K,verpre,0.,2.,'-','version',IER)
        endif
        CALL EDISP(IUOUT,' ')    
        WRITE(OUTS,'(3A)') 
     &    'Opened predefined objects file: ',LASCI(1:LNBLNK(LASCI)),'.' 
        CALL USRMSG(OUTS,' ','-') 
      else
        WRITE(OUTS,'(3A)') 'File: ',LASCI(1:LNBLNK(LASCI)), 
     &    ' is not a predefined objects file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        return
      endif 
   
C Read in the header lines of the file, look for key tags.
  20  CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'header lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','predefined tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*Text')then
  21    CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'text lines',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','text line',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:9).EQ.'*End_text')then
          continue  ! look for more key words
        else

C << Save the line of text NEED DATA STRUCTURE >> 
          goto 21  ! get another line of text
        endif
      elseif(WORD(1:9).EQ.'*End_text')then
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*Category')then
        continue  ! look for more key words
      elseif(WORD(1:13).EQ.'*End_category')then
        continue  ! look for more key words
      elseif(WORD(1:15).EQ.'*End_predefined')then
        CALL ERPFREE(IFA,ISTAT)
        if(foundit)then
          goto 42  ! process what has been found
        endif
      elseif(WORD(1:5).EQ.'*item')then
        CALL EGETW(LOUTSTR,K,WORD,'W','item name',IER)
        if(WORD(1:12).ne.name(1:12))then
          continue
        else

C Located the object requested.
          write(objname,'(a)') WORD(1:lnblnk(WORD))
          CALL EGETRM(LOUTSTR,K,phrase,'W','menu',IER)
          write(objdesc,'(a)') phrase(1:lnblnk(phrase))
          foundit=.true.
        endif
      elseif(WORD(1:9).EQ.'*end_item')then
        if(foundit)then
          goto 42  ! process what has been found
        endif
      elseif(WORD(1:6).EQ.'*incat')then
        if(foundit)then
          CALL EGETW(LOUTSTR,K,WORD,'W','item cat',IER)
          write(objectcat,'(a)') WORD(1:lnblnk(WORD))
        endif
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*includes')then

C Check topics included. << work to be done >>
        if(foundit)then

        endif
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*sourced')then
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*origin')then

C Read origin and add transforms (will need this to later
C apply rotation about lower left corner.
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org X',IER)
        if(foundit)objorg(1)=VAL+DX
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org Y',IER)
        if(foundit)objorg(2)=VAL+DY
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org Z',IER)
        if(foundit)objorg(3)=VAL+DZ
        IF(IER.NE.0) CONT=.FALSE.
        continue  ! look for more key words
      elseif(WORD(1:13).EQ.'*bounding_box')then

C Bounding box of the object.
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*vertex')then

C Remember vertices associated with mass and add transform.
        if(foundit)then
          nbvertmass=nbvertmass+1
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass X',IER)
          vertmass(nbvertmass,1)=VAL+DX
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass Y',IER)
          vertmass(nbvertmass,2)=VAL+DY
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass Z',IER)
          vertmass(nbvertmass,3)=VAL+DZ
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*mass')then

C Remember mass definitions. Apply prec to mass surface names.
        if(foundit)then
          nbmass=nbmass+1
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','mass name',IER)
          write(msurname(nbmass),'(2a)') prec,WORD(1:11)
          CALL EGETP(LOUTSTR,K,phrase,'W','mass mat',IER)
          write(msurmat(nbmass),'(a)') phrase(1:lnblnk(phrase))
          CALL EGETW(LOUTSTR,K,WORD,'W','mass optics',IER)
          write(msuropt(nbmass),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETWI(LOUTSTR,K,iv,4,MV,'F','nb assoc v',IER)
          nbmassv(nbmass)=iv
          IF(IER.NE.0) CONT=.FALSE.
          do loop=1,nbmassv(nbmass)
            CALL EGETWI(LOUTSTR,K,iv,1,MOTV,'F','assoc v',IER)
            masjvn(nbmass,loop)=iv
          enddo
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*surf')then

C Remember boundary surface definitions. Apply prec to surface names.
        if(foundit)then
          nbbound=nbbound+1
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','surf name',IER)
          write(bsurname(nbbound),'(2a)') prec,WORD(1:11)
          CALL EGETP(LOUTSTR,K,phrase,'W','surf mat',IER)
          write(bsurmat(nbbound),'(a)') phrase(1:lnblnk(phrase))
          CALL EGETW(LOUTSTR,K,WORD,'W','surf optics',IER)
          write(bsuropt(nbbound),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETWI(LOUTSTR,K,iv,3,MV,'F','nb assoc v',IER)
          nbboundv(nbbound)=iv
          IF(IER.NE.0) CONT=.FALSE.
          do loop=1,nbboundv(nbbound)
            CALL EGETWI(LOUTSTR,K,iv,1,MOTV,'F','assoc v',IER)
            boundjvn(nbbound,loop)=iv
          enddo
        endif
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*bounds')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*visp')then

C Scan visp definition and instanciate model entity.
        if(foundit)then
          nbvis(icomp)=nbvis(icomp)+1
          nbv=nbvis(icomp)
          CALL EGETWI(LOUTSTR,K,iv,8,8,'F','vis nb vertices',IER)
          CALL EGETWI(LOUTSTR,K,iv,6,6,'F','vis nb faces',IER)
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          OPOV(icomp,nbv)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Prepend prec to name and update the compound object reference.
          write(VISNAME(icomp,nbv),'(2a)') prec,WORD(1:11)
          LNVISNAME(icomp,nbv)=lnblnk(VISNAME(icomp,nbv))
          CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
          write(VISMAT(icomp,nbv),'(a)') WORD(1:lnblnk(WORD))
          LNVISMAT(icomp,nbv)=lnblnk(VISMAT(icomp,nbv))
          VISTYP(icomp,nbv)='visp'
          IF(IER.NE.0) CONT=.FALSE.

C Grab all of the coordinates and transform.
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'first 4 coord',IER)
          K=0
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 1',IER)
          XVP(icomp,nbv,1)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 1',IER)
          YVP(icomp,nbv,1)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 1',IER)
          ZVP(icomp,nbv,1)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 2',IER)
          XVP(icomp,nbv,2)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 2',IER)
          YVP(icomp,nbv,2)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 2',IER)
          ZVP(icomp,nbv,2)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 3',IER)
          XVP(icomp,nbv,3)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 3',IER)
          YVP(icomp,nbv,3)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 3',IER)
          ZVP(icomp,nbv,3)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 4',IER)
          XVP(icomp,nbv,4)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 4',IER)
          YVP(icomp,nbv,4)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 4',IER)
          ZVP(icomp,nbv,4)=val1+DZ
          IF(IER.NE.0) CONT=.FALSE.
          
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
          K=0
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 5',IER)
          XVP(icomp,nbv,5)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 5',IER)
          YVP(icomp,nbv,5)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 5',IER)
          ZVP(icomp,nbv,5)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 6',IER)
          XVP(icomp,nbv,6)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 6',IER)
          YVP(icomp,nbv,6)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 6',IER)
          ZVP(icomp,nbv,6)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 7',IER)
          XVP(icomp,nbv,7)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 7',IER)
          YVP(icomp,nbv,7)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 7',IER)
          ZVP(icomp,nbv,7)=val1+DZ
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 8',IER)
          XVP(icomp,nbv,8)=val1+DX
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 8',IER)
          YVP(icomp,nbv,8)=val1+DY
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 8',IER)
          ZVP(icomp,nbv,8)=val1+DZ
          IF(IER.NE.0) CONT=.FALSE.
        else

C Even if we are not focuse on it need to jump 2 lines.
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'first 4 coord',IER)
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*vis3')then

        if(foundit)then
          nbvis(icomp)=nbvis(icomp)+1
          nbv=nbvis(icomp)
          CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
          CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
          CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
          XOV(icomp,nbv)=VX+DX
          YOV(icomp,nbv)=VY+DY
          ZOV(icomp,nbv)=VZ+DZ

          CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
          CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
          CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
          DXOV(icomp,nbv)=VX
          DYOV(icomp,nbv)=VY
          DZOV(icomp,nbv)=VZ

          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang a',IER)
          BANGOV(icomp,nbv,1)=VX  ! add rotation later
          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang b',IER)
          BANGOV(icomp,nbv,2)=VX
          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang c',IER)
          BANGOV(icomp,nbv,3)=VX
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          OPOV(icomp,nbv)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Prepend prec to the name.
          write(VISNAME(icomp,nbv),'(2a)') prec,WORD(1:11)
          LNVISNAME(icomp,nbv)=lnblnk(VISNAME(icomp,nbv))

C The name of the construction might contain spaces so use EGETP.
          CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
          write(VISMAT(icomp,nbv),'(a)') WORD(1:lnblnk(WORD))
          LNVISMAT(icomp,nbv)=lnblnk(VISMAT(icomp,nbv))
          VISTYP(icomp,nbv)='vis3'
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:4).EQ.'*vis')then

C Visual entity with only one rotation.
        if(foundit)then
          nbvis(icomp)=nbvis(icomp)+1
          nbv=nbvis(icomp)
          CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
          CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
          CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
          XOV(icomp,nbv)=VX+DX
          YOV(icomp,nbv)=VY+DY
          ZOV(icomp,nbv)=VZ+DZ

          CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
          CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
          CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
          DXOV(icomp,nbv)=VX
          DYOV(icomp,nbv)=VY
          DZOV(icomp,nbv)=VZ

          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang a',IER)
          BANGOV(icomp,nbv,1)=VX  ! add rotation later
          BANGOV(icomp,nbv,2)=0.0   ! there is no 2nd rotation
          BANGOV(icomp,nbv,3)=0.0   ! there is no 3rd rotation
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          OPOV(icomp,nbv)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Prepend prec to the name.
          write(VISNAME(icomp,nbv),'(2a)') prec,WORD(1:11)
          LNVISNAME(icomp,nbv)=lnblnk(VISNAME(icomp,nbv))

C The name of the visual material might contain spaces so use EGETP.
          CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
          write(VISMAT(icomp,nbv),'(a)') WORD(1:lnblnk(WORD))
          LNVISMAT(icomp,nbv)=lnblnk(VISMAT(icomp,nbv))
          VISTYP(icomp,nbv)='vis '
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*vobject')then

C Collection of entities making up an object. Update
C limit if geometry.h changes.
        if(foundit)then
          if(NBVOBJ(icomp)+1.gt.20)then
            call usrmsg('Maximum number of visual objects reached',
     &        'sorry about that.','W')
            ier=2
            CLOSE(IFA)
            RETURN
          endif
          NBVOBJ(icomp)=NBVOBJ(icomp)+1
          nbvo=NBVOBJ(icomp)
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis obj name',IFLAG)

          write(VOBJNAME(icomp,nbvo),'(2a)') prec,WORD(1:11) ! adjust name
          LNVOBJNAME(icomp,nbvo)=lnblnk(VOBJNAME(icomp,nbvo))
          CALL EGETP(LOUTSTR,K,phrase,'W','vis obj desc',IFLAG)
          write(VOBJDESC(icomp,nbvo),'(a)') phrase(1:lnblnk(phrase))
          LNVOBJDESC(icomp,nbvo)=lnblnk(VOBJDESC(icomp,nbvo))
          CALL EGETWI(LOUTSTR,K,iv,1,MOMVB,'F','nb vis entities',
     &      IER)
          NBVOBJLIST(icomp,nbvo)=iv
          do ibvo = 1,iv
            WORD='            ' 
            CALL EGETW(LOUTSTR,K,WORD,'W','entity name',IFLAG)

C Prepend prec to the name.
            write(VOBJLIST(icomp,nbvo,ibvo),'(2a)') prec,WORD(1:11)
            LNVOBJLIST(icomp,nbvo,ibvo)=
     &        lnblnk(VOBJLIST(icomp,nbvo,ibvo))
          enddo  ! ibvo
C Debug.
C          write(6,*) 'vis obj ',icomp,nbvo,iv,VOBJNAME(icomp,nbvo),
C     &      LNVOBJNAME(icomp,nbvo),VOBJDESC(icomp,nbvo),
C     &      LNVOBJDESC(icomp,nbvo),
C     &      VOBJLIST(icomp,nbvo,1),LNVOBJLIST(icomp,nbvo,1),
C     &      VOBJLIST(icomp,nbvo,2),LNVOBJLIST(icomp,nbvo,2)
        endif
        continue  ! look for more key words
      else
        call edisp248(iuout,LOUTSTR,100)
        write(outs,'(3a)') 'has unknown tag ',WORD,' continuing...'
        call edisp(iuout,outs)
        continue  ! look for more key words
      endif

C If there were no errors in reading header line then read another.
      if(CONT)then
        goto 20
      else
        write(outstr,'(a)')loutstr(1:100)
        call usrmsg('Error reading predefined file @',outstr,'W')
        ier=1
        CLOSE(IFA)
        RETURN
      endif

   42 continue
      CLOSE(IFA)  ! close so can use again

C If there is a rotation to apply to the visual objects
C that have just been added do this.
      call eclose(DA,0.0,0.01,close)
      if(close)then
        continue  ! no rotation to apply
      else
        if(nbvis(icomp).gt.nbprevious)then
          PI = 4.0 * ATAN(1.0)
          A=-DA*PI/180.
          CA=COS(A)
          SA=SIN(A)
          X1=objorg(1); Y1=objorg(2)  ! rotate around the new origin
          do K=nbprevious+1,nbvis(icomp)
            XXX=XOV(ICOMP,K)-X1; YYY=YOV(ICOMP,K)-Y1
            XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
            XOV(ICOMP,K)=XR+X1; YOV(ICOMP,K)=YR+Y1
            BANGOV(ICOMP,K,1)=BANGOV(ICOMP,K,1)+DA
            do ibe=1,8
              XXX=XVP(icomp,K,ibe)-X1
              YYY=YVP(icomp,K,ibe)-Y1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              XVP(icomp,K,ibe)=XR+X1
              YVP(icomp,K,ibe)=YR+Y1
            enddo  ! of ibe
          enddo  ! of K
        endif
      endif
      
C For each of the vertices apply rotation.
      if(nbvertmass.gt.0)then
        call eclose(DA,0.0,0.01,close)
        if(close)then
          continue  ! no rotation to apply
        else
          PI = 4.0 * ATAN(1.0)
          A=-DA*PI/180.
          CA=COS(A)
          SA=SIN(A)
          XX1=objorg(1); YY1=objorg(2)  ! rotate around the new origin
          DO I=1,nbvertmass
            XXX=vertmass(I,1)-XX1
            YYY=vertmass(I,2)-YY1
            XR=XXX*CA+YYY*SA
            YR=YYY*CA-XXX*SA
            vertmass(I,1)=XR+XX1
            vertmass(I,2)=YR+YY1
          ENDDO
        endif

C Insert vertices associated with the entity into the zone.
C Use code similar to line 1665 of insert.F subroutine ADDVERT.
C Warn if close to an existing vertex.
        do ix = 1,nbvertmass
          if(NTV+1.le.MTV)then
            do ixx = 1,NTV
              call eclose3(vertmass(ix,1),vertmass(ix,2),
     &          vertmass(ix,3),X(ixx),Y(ixx),Z(ixx),0.004,close)
              if(close)then
                write(outs,'(a,i3,a,3f8.3,a,i3,a,3f8.3)')
     &            'New vertex ',ix,' @',vertmass(ix,1),vertmass(ix,2),
     &            vertmass(ix,3),' is close to existing vertex',ix,
     &            ' @',X(ixx),Y(ixx),Z(ixx)
                call edisp(iuout,outs)
              endif
            enddo  ! of ixx

C Remember what their new index (within the zone is.
            NTV=NTV+1
            NZTV(icomp)=NTV
            X(NTV)=vertmass(ix,1)
            Y(NTV)=vertmass(ix,2)
            Z(NTV)=vertmass(ix,3)
            VCOORD(ICOMP,ntv,1)=vertmass(ix,1)
            VCOORD(ICOMP,ntv,2)=vertmass(ix,2)
            VCOORD(ICOMP,ntv,3)=vertmass(ix,3)
          endif
        enddo  ! of ix

C Apply the updated indices to the bounding surface list.
        do ix=1,nbbound
          do loop=1,nbboundv(ix)
            boundjvn(ix,loop)=boundjvn(ix,loop)+nbprevnztv
          enddo
        enddo  ! of ix

C Apply the updated indices to the mass surface list.
        do ix=1,nbmass
          do loop=1,nbmassv(ix)
            masjvn(ix,loop)=masjvn(ix,loop)+nbprevnztv
          enddo
        enddo  ! of ix

C Add the bounding surfaces to the zone. However if this is
C a new zone the only the 1st connection will be known.
C << this logic needs to be adapted >>
        do ix=1,nbbound
          if(izstocn(icomp,nsur).eq.0)then
            icon=NCON+1   ! used next free connection
          else
            ICON=IZSTOCN(icomp,nsur)+1
          endif
          NSUR=NSUR+1
          NZSUR(ICOMP)=NSUR
          NVER(NSUR)=nbboundv(ix)
          do J=1,nbboundv(ix)
            JVN(NSUR,J)=boundjvn(ix,J)
          enddo
          SNAME(ICOMP,NSUR)=bsurname(ix)
          SOTF(NSUR)=bsuropt(ix)
          SMLCN(NSUR)=bsurmat(ix)
          SVFC(NSUR)='UNKN'
          SPARENT(NSUR)='-'
          SUSE(nsur,1)='WALL'
          SUSE(nsur,2)='- '
          SOTHER(NSUR,1)='UNKNOWN'
          SOTHER(NSUR,2)='0'
          SOTHER(NSUR,3)='0'

C Update the connection list. Move all others up and then insert with
C moderate moderate trace (3rd parameter in addedsurf).
          call addedsurf(icomp,icon,1,ier)

C Save geometry file.
          call geowrite(IFA,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        enddo  ! of ix

C Add the mass surfaces to the zone. Use code similar to
C line 1044 of insert.F of the subroutine ADDSUR.
        do ix=1,nbmass
          ICON=IZSTOCN(icomp,nsur)+1
          NSUR=NSUR+1
          NZSUR(ICOMP)=NSUR
          NVER(NSUR)=nbmassv(ix)
          do J=1,nbmassv(ix)
            JVN(NSUR,J)=masjvn(ix,J)
          enddo
          SNAME(ICOMP,NSUR)=msurname(ix)
          SOTF(NSUR)=msuropt(ix)
          SMLCN(NSUR)=msurmat(ix)
          SVFC(NSUR)='UNKN'
          SPARENT(NSUR)='-'
          SUSE(nsur,1)='FURNI'  ! predefined mass
          SUSE(nsur,2)='- '

C Set each pair to be back-to-back. Logic for up to 6.
          SOTHER(NSUR,1)='ANOTHER'
          if(ix.eq.1.or.ix.eq.3.or.ix.eq.5)then
            write(SOTHER(NSUR,2),'(i2.2)')INT(icomp)
            write(SOTHER(NSUR,3),'(i2.2)')INT(NSUR+1)
          else
            write(SOTHER(NSUR,2),'(i2.2)')INT(icomp)
            write(SOTHER(NSUR,3),'(i2.2)')INT(NSUR-1)
          endif

C Update the connection list. Move all others up and then insert with
C moderate moderate trace (3rd parameter in addedsurf).
          call addedsurf(icomp,icon,1,ier)

C Save geometry file.
          call geowrite(IFA,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        enddo  ! of ix
      endif

C Re-display the zone on exit.

      return
      end

 
C ******************** GEO2OBJ 
C GEO2OBJ reads V1.1 zone geometry data and to fill predefined object
C common blocks.

      SUBROUTINE GEO2OBJ(IUNIT,LGEOMF,ITRU,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "predefined.h"

C espriou.h provides currentfile.
#include "espriou.h"

      integer lnblnk  ! function definition

C Parameters
      integer IUNIT  ! file unit to read
      CHARACTER LGEOMF*72 ! geometry file to scan
      integer ITRU   ! file unit for feedback
      integer IER    ! zero is ok

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN

      CHARACTER WORD*32
      CHARACTER tmpvfc*4,tother*15,tother1*15
      character ZN*12,phrase*64,outs*124,outs2*124
      character loutstr*248
      character dstmp*24
      character OPT*32 ! for use with optical attribute
      integer lsn   ! length of currentfile
      integer lno,lnow     ! length of optics name
      integer lstrs,lstrd  ! length of mlc names
      real VX,VY,VZ ! local values for XOB YOB ZOB
      integer iflag ! for read error state

C Size this to acommodate normal geometry file complexity.
      character anysurname*12,anysurmat*32,anysuropt*32
      dimension anysurname(MS),anysurmat(MS),anysuropt(MS)
      integer nbanyv(MS)     ! for initial scan of edges
      integer anyjvn(MS,MV)  ! for the initial scan of edges
      real VAL1
      integer istat,k,nd
      integer ibvo,loop

      character OCTYPE*3

C Set initial values.
      IER=0; iflag=0; itrct=0
      ZN=' '; phrase=' '
      NEDGE=0  ! counter for surfaces in the edge list, assume no surfaces
      NS=0     ! temporary array for counting surfaces.
      objxmax=-1.E+7; objymax=-1.E+7; objzmax=-1.E+7
      objxmin=1.E+7; objymin=1.E+7; objzmin=1.E+7

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LGEOMF,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Geometry file ',LGEOMF(1:lnblnk(LGEOMF)),
     &      ' could not be opened.'
        call edisp(itru,outs)
        IER=1
        RETURN
      endif
      write(currentfile,'(a)') LGEOMF(1:lnblnk(LGEOMF))

C Clear predefined object variables.
      call clearobjcommons()
      do loop=1,MS
        anysurname(loop)=' '
        anysurmat(loop)=' '
        anysuropt(loop)=' '
        nbanyv(loop)=0
      enddo

C Indicate we got it from an ESP-r geometry file.
      write(objsource,'(a)') 'Derived from an ESP-r geometry file.'

C Read header lines from file, the 1.1 version looks like: 
C *Geometry 1.1,GEN,manager  # tag version, format, zone name (tbd allow spaces)
      CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'geo line 1',IER)
      IF(IER.NE.0)goto 1002
      if(LOUTSTR(1:13).eq.'*Geometry 1.1')then

C Decode first line of version 1.1 geometry file.
        K=13
        CALL EGETW(LOUTSTR,K,WORD,'W','OCTYPE',IFLAG)
        write(OCTYPE,'(a)') WORD(1:lnblnk(WORD))
        IF(OCTYPE(1:3).EQ.'GEN')THEN
          continue
        else
          CALL ERPFREE(IUNIT,ios)
          call usrmsg('CTYPE is not GEN.','Version 1.1 required.','W')
          return
        endif
        CALL EGETW(LOUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:12)
        call st2name(ZN,objname)
      else

C If we have reached this position then it is an older geometry
C file warn user.
        CALL ERPFREE(IUNIT,ios)
        call usrmsg('Older geometry file detected.',
     &    'Version 1.1 required.','W')
        return
      endif

C Zone is of type GEN. First read the date stamp line followed
C by the zone description on the next line. For now zdesc string
C is short, but will be extended to 248 char.
      CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'date stamp',IER)
      IF(IER.NE.0) goto 1002
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','header tags',IER)
      IF(IER.NE.0) goto 1002
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        CALL EGETRM(LOUTSTR,K,dstmp,'W','date stamp',IER)
      endif

C Convert the geometry description in to initial object descr & note.
      CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'description',IER)
      IF(IER.NE.0) goto 1002
      write(objdesc,'(a)') LOUTSTR(1:32)
      write(objnotes(1),'(a)') LOUTSTR(1:64)
      nbobjnotes=1

C Next look for *vertex lines, incrementing the value of nbvertmass
C as each of these lines is scanned. If the tag is *edges then
C hold in temporary array until the attributes are known.
   62 CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,0,'*vertex *edges tags',IER)
      if(IER.EQ.2)then

C End of file sensed, however this is not an error at this point.
        IER=0
        goto 44
      elseif(IER.eq.0)then
        continue
      else
        goto 1002
      endif
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','*vertex or *surface tags',IER)
      if(WORD(1:7).eq.'*vertex')then
        nbvertmass=nbvertmass+1
        if(nbvertmass.le.MOTV)then
          CALL EGETWR(LOUTSTR,K,vertmass(nbvertmass,1),0.,0.,'-',
     &      'X coord',IER)
          CALL EGETWR(LOUTSTR,K,vertmass(nbvertmass,2),0.,0.,'-',
     &      'Y coord',IER)
          CALL EGETWR(LOUTSTR,K,vertmass(nbvertmass,3),0.,0.,'-',
     &      'Z coord',IER)
        else
          call edisp(iuout,
     &      'Exceeded number of vertices. Skipping input line.')
        endif

C Gather minimum and maximums on each axis.
        objxmin=AMIN1(objxmin,vertmass(nbvertmass,1))
        objymin=AMIN1(objymin,vertmass(nbvertmass,2))
        objzmin=AMIN1(objzmin,vertmass(nbvertmass,3))
        objxmax=AMAX1(objxmax,vertmass(nbvertmass,1))
        objymax=AMAX1(objymax,vertmass(nbvertmass,2))
        objzmax=AMAX1(objzmax,vertmass(nbvertmass,3))
        goto 62
      elseif(WORD(1:7).eq.'*rotate')then
        goto 62
      elseif(WORD(1:16).eq.'*previous_rotate')then
        goto 62
      elseif(WORD(1:6).eq.'*edges')then

C We won't know if this is for mass or boundary until later. Increment.
       NEDGE=NEDGE+1 
       nbanyv(nedge)=0
       if(NEDGE.le.(MOMS+MOBS))then
         CALL EGETWI(LOUTSTR,K,J,3,MV,'F','nb assoc vertices',IERV)
         nbanyv(nedge)=J

C Now proceed to read vertices on one or more lines.
         DO 12 KV=1,nbanyv(nedge)
           CALL EGETWI(LOUTSTR,K,IVAL,0,MOTV,'F','vertex',IERV)
           IF(IERV.NE.0) THEN
             call edisp(ITRU,' reading continuation line...')
             CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,0,'vertex XYZ',IER)
             IF(IER.NE.0)goto 1002
             K=0
             CALL EGETWI(LOUTSTR,K,IVAL,0,MOTV,'F','vertex',IERV)
           ENDIF
           IF(IERV.NE.0) GOTO 1002
           anyjvn(NEDGE,KV)=IVAL
   12     CONTINUE
        else
          call edisp(iuout,
     &      'Exceeded number of zone surfaces. Skipping input line.')
        endif
        goto 62
      elseif(WORD(1:5).eq.'*surf')then

C Increment counter for surfaces (NS) up to MOBS+MOMS in source file.
        NS=NS+1
        if(NS.le.(MOBS+MOMS))then

C Surface attributes in a typical line:
C *surf,door,VERT,-,DOOR,UNDERCUT,door,OPAQUE,ANOTHER,3,6 # >|< door in coridor      

C Surface name, allow for future spaces in name.
          CALL EGETP(LOUTSTR,K,WORD,'W','surface name',IER)
          write(anysurname(NS),'(a)') WORD(1:lnblnk(WORD))

C Surface position can be ignored.
          CALL EGETW(LOUTSTR,K,tmpvfc,'W','surface position',IER)

C Parent name can be ignored.
          CALL EGETP(LOUTSTR,K,WORD,'W','surface parent',IER)

C Surface usage - two tokens.  ?? any use in predefined ??
          CALL EGETW(LOUTSTR,K,WORD,'W','surface use 1',IER)
          CALL EGETW(LOUTSTR,K,WORD,'W','surface use 2',IER)

C Surface construction name, allow for spaces.
          CALL EGETP(LOUTSTR,K,WORD,'W','surface construction',IER)
          write(anysurmat(NS),'(a)') WORD(1:lnblnk(WORD))

C Check if there is a matching MLC name find out which one it is.
C Because many names may start similarly, check against actual widths.
          ICF=-1
          if(NMLC.gt.0)then                
            lstrs=lnblnk(anysurmat(NS))  ! surface attribute length
            DO 20 IC=1,NMLC
              if(mlcname(ic)(1:lnmlcname(ic)).EQ.
     &           anysurmat(NS)(1:lstrs))ICF=IC
  20        CONTINUE
          endif

C << If ICF is still -1 then did not find the construction.
          if(ICF.le.0)then
            write(outs,'(2a)') 'Did not find ',anysurmat(ns)
            call edisp(iuout,outs)
          endif

C Surface optics set name or OPAQ/TRAN, allow for spaces.
          CALL EGETP(LOUTSTR,K,WORD,'W','surface optics',IER)
          if(WORD(1:4).eq.'OPAQ')then
            write(anysuropt(NS),'(a)') 'OPAQUE'  ! write OPAQUE
          elseif(WORD(1:4).eq.'CFC ')then
            write(anysuropt(NS),'(a)') 'CFC '  ! write CFC
          elseif(WORD(1:4).eq.'CFC2')then
            write(anysuropt(NS),'(a)') 'CFC2'  ! write CFC2
          elseif(WORD(1:4).ne.'OPAQ'.and.WORD(1:3).ne.'CFC')then

C This token could be the name of the optical property. Find out if it
C matches then name of the optical set for the construction.
C If the construction database has been scanned then attempt to discover
C the name of the optical property. Otherwise leave SOFT as TRAN
            if(NMLC.gt.0)then

C Find the optical name within the common constructions file.
              if(ICF.gt.0)then
                write(OPT,'(a)') mlcoptical(ICF)
              else
                OPT='OPAQUE'
              endif

C When scanning geometry warn users if the optical description in the
C geometry file is different from that of the optical property associated
C with the construction (TRAN is an ok mis-match).
              if(ICF.gt.0)then
                lnow=lnblnk(word)   ! length for token in file
                lno=lnblnk(OPT)     ! length for mlc optics
                if(WORD(1:lnow).ne.OPT(1:lno))then
                  if(WORD(1:4).eq.'TRAN')then
                    continue
                  else
                    write(outs,*) 'Optics ',WORD(1:lnow),
     &                ' in geo file does not match optics of the MLC ',
     &                  mlcname(ICF)(1:lnmlcname(ICF)),' ',OPT(1:lno)
                    call edisp(iuout,outs)
                  endif
                endif
              endif

C The matched optical property is saved to memory. The geometry file
C would need to be written to record the matched property.
              write(anysuropt(NS),'(a)') OPT
            else

C During initial scan of configuration file the databases will
C not yet have been scanned and so this block of code will be
C active. Lets assume that this token really is the name of the
C optical set.
              write(anysuropt(NS),'(a)') WORD(1:lnblnk(WORD))
            endif
          endif

C Surface other side - three tokens. The last is index of surface within  
C the zone. Depending on what we find, instantiate the edge list arrays.
C Internal mass assumed to be two subsequent surfaces in this zone back-to-back
          CALL EGETW(LOUTSTR,K,tother,'W','surface other 1',IER)
          CALL EGETW(LOUTSTR,K,tother1,'W','surface other 2',IER)
          CALL EGETWI(LOUTSTR,K,IEE,0,0,'-','conn other surf',IER)
          if(tother(1:7).eq.'UNKNOWN'.or.
     &       tother(1:9).eq.'ADIABATIC'.or.
     &       tother(1:7).eq.'SIMILAR'.or.
     &       tother(1:8).eq.'CONSTANT')then

C Ask the user what to do.
             call edisp248(iuout,LOUTSTR,100)
             CALL EASKABC('Options for the surface: ',' ',
     &         'set as back-to-back mass','set as boundary surface',
     &         'ignore it',IW,nbhelp)
             if (IW.eq.1) then
               nbmass=nbmass+1
               msurname(nbmass)=anysurname(NS)
               msurmat(nbmass)=anysurmat(ns)
               msuropt(nbmass)=anysuropt(ns)
               nbmassv(nbmass)=nbanyv(ns)
               do loop = 1,nbmassv(nbmass)
                 masjvn(nbmass,loop)=anyjvn(ns,loop)
               enddo
             elseif (IW.eq.2) then
               nbbound=nbbound+1
               bsurname(nbbound)=anysurname(NS)
               bsurmat(nbbound)=anysurmat(ns)
               bsuropt(nbbound)=anysuropt(ns)
               nbboundv(nbbound)=nbanyv(ns)
               do loop = 1,nbboundv(nbbound)
                 boundjvn(nbbound,loop)=anyjvn(ns,loop)
               enddo
             endif

           elseif(tother(1:8).eq.'EXTERIOR')then
             nbbound=nbbound+1
             bsurname(nbbound)=anysurname(NS)
             bsurmat(nbbound)=anysurmat(ns)
             bsuropt(nbbound)=anysuropt(ns)
             nbboundv(nbbound)=nbanyv(ns)
             do loop = 1,nbboundv(nbbound)
               boundjvn(nbbound,loop)=anyjvn(ns,loop)
             enddo
           elseif(tother(1:8).eq.'BASESIMP')then
             continue  ! probably no meaning
           elseif(tother(1:6).eq.'GROUND')then

C Consider GROUND as boundary.
             nbbound=nbbound+1
             bsurname(nbbound)=anysurname(NS)
             bsurmat(nbbound)=anysurmat(ns)
             bsuropt(nbbound)=anysuropt(ns)
             nbboundv(nbbound)=nbanyv(ns)
             do loop = 1,nbboundv(nbbound)
               boundjvn(nbbound,loop)=anyjvn(ns,loop)
             enddo
           elseif(tother(1:9).eq.'IDENT_CEN')then
             continue  ! probably no meaning
           elseif(tother(1:7).eq.'ANOTHER')then

C Consider a back-to-back.
             if(IEE.eq.NS+1.or.IEE.eq.NS-1)then
               nbmass=nbmass+1
               msurname(nbmass)=anysurname(NS)
               msurmat(nbmass)=anysurmat(ns)
               msuropt(nbmass)=anysuropt(ns)
               nbmassv(nbmass)=nbanyv(ns)
               do loop = 1,nbmassv(nbmass)
                 masjvn(nbmass,loop)=anyjvn(ns,loop)
               enddo
             endif
           else

C Assume partition so setup sother arrays with this in mind.
            if(IEE.eq.NS+1.or.IEE.eq.NS-1)then
              nbmass=nbmass+1
              msurname(nbmass)=anysurname(NS)
              msurmat(nbmass)=anysurmat(ns)
              msuropt(nbmass)=anysuropt(ns)
              nbmassv(nbmass)=nbanyv(ns)
              do loop = 1,nbmassv(nbmass)
                masjvn(nbmass,loop)=anyjvn(ns,loop)
              enddo
            endif
          endif
        else
          call edisp(iuout,
     &      'Exceeded number of surfaces. Skipping input line.')
        endif
        goto 62
      elseif(WORD(1:10).eq.'*base_list')then

C The list-based definition of base surfaces is of no interest. And
C if we get this far we have the information needed to compute the
C object bounds.
        objbnds(1)=objxmax-objxmin
        objbnds(2)=objymax-objymin
        objbnds(3)=objzmax-objzmin
        goto 62
      elseif(WORD(1:10).eq.'*shad_calc')then

C Shading calculation instructions.
C << would this be of any interest in an object ?? >>
        CALL EGETW(LOUTSTR,K,WORD,'W','shad directive',IFLAG)
        if(WORD(1:4).eq.'none')then
C          objiaplic(1)=0
          else
            if(WORD(1:14).eq.'all_applicable')then
C              objiaplic(1)=1
            endif
          endif
          goto 62
        elseif(WORD(1:11).eq.'*insol_calc')then

C Insolation calculation instructions.
C << would this be of any interest for predefined objects ?? >>
          CALL EGETW(LOUTSTR,K,WORD,'W','insolation directive',IFLAG)
          if(WORD(1:4).eq.'none')then
C            objiaplic(2)=0
          else
            if(WORD(1:14).eq.'all_applicable')then
C              objiaplic(2)=1
            endif
          endif
          goto 62
        elseif(WORD(1:7).eq.'*insol ')then
          goto 62
        elseif(WORD(1:13).eq.'*bridge_start')then

C << would this be of any interest in a predefined object ?? >>
          goto 62
        elseif(WORD(1:11).eq.'*ukt_bridge')then

C << would this be of any interest in a predefined object ?? >>
          goto 62
        elseif(WORD(1:11).eq.'*end_bridge')then
          goto 62
        elseif(WORD(1:12).eq.'*block_start')then

C A number of block shapes can be associated with a model as follows:
C *obs - solar obstruction blocks and

C << What about obstructions as part of a facade element ?? >>
C << do this later. Sort visual stuff first.

C Read another line to get the actual blocks. There are slightly different
C formats depending on whether the key is '*obs' or '*obs3'.
   72     CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,0,'*obs tags',IER)
          k=0
          CALL EGETP(LOUTSTR,K,phrase,'W','block tag',IER)
          if(phrase(1:5).eq.'*obs3')then
            objnbobs=objnbobs+1
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','obs X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','obs Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','obs Z org',IER)
            OBJXOB(objnbobs)=VX
            OBJYOB(objnbobs)=VY
            OBJZOB(objnbobs)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','obs X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','obs Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','obs Z dis',IER)
            OBJDXOB(objnbobs)=VX
            OBJDYOB(objnbobs)=VY
            OBJDZOB(objnbobs)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang a',IER)
            OBJBANGOB(objnbobs,1)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang b',IER)
            OBJBANGOB(objnbobs,2)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang c',IER)
            OBJBANGOB(objnbobs,3)=VX
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
            OBJOPOB(objnbobs)=VX
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            OBJBLOCKNAME(objnbobs)=WORD(1:12)

C The name of the construction might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(OBJBLOCKMAT(objnbobs),'(a)') WORD(1:lnblnk(WORD))
            OBJBLOCKTYP(objnbobs)='obs3'
            goto 72

          elseif(phrase(1:5).eq.'*obsp')then

C A general polygon obstruction to be associated with the current zone.
            objnbobs=objnbobs+1
            CALL EGETWI(LOUTSTR,K,ival,8,8,'F','obs nb vertices',IER)
            CALL EGETWI(LOUTSTR,K,ival,6,6,'F','obs nb faces',IER)
            if(ND.ge.6)then  ! if enough items for opacity
              CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
              OBJOPOB(objnbobs)=VX
            else
              OBJOPOB(objnbobs)=1.0  ! set to opaque if not specified
            endif
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            write(OBJBLOCKNAME(objnbobs),'(a)') WORD(1:lnblnk(WORD))
            CALL EGETP(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(OBJBLOCKMAT(objnbobs),'(a)') WORD(1:lnblnk(WORD))
            OBJBLOCKTYP(objnbobs)='obsp'

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'first 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 1',IER)
            OBJXBP(objnbobs,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 1',IER)
            OBJYBP(objnbobs,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 1',IER)
            OBJZBP(objnbobs,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 2',IER)
            OBJXBP(objnbobs,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 2',IER)
            OBJYBP(objnbobs,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 2',IER)
            OBJZBP(objnbobs,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 3',IER)
            OBJXBP(objnbobs,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 3',IER)
            OBJYBP(objnbobs,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 3',IER)
            OBJZBP(objnbobs,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 4',IER)
            OBJXBP(objnbobs,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 4',IER)
            OBJYBP(objnbobs,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 4',IER)
            OBJZBP(objnbobs,4)=val1

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 5',IER)
            OBJXBP(objnbobs,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 5',IER)
            OBJYBP(objnbobs,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 5',IER)
            OBJZBP(objnbobs,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 6',IER)
            OBJXBP(objnbobs,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 6',IER)
            OBJYBP(objnbobs,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 6',IER)
            OBJZBP(objnbobs,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 7',IER)
            OBJXBP(objnbobs,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 7',IER)
            OBJYBP(objnbobs,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 7',IER)
            OBJZBP(objnbobs,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XBP 8',IER)
            OBJXBP(objnbobs,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YBP 8',IER)
            OBJYBP(objnbobs,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZBP 8',IER)
            OBJZBP(objnbobs,8)=val1
            goto 72

          elseif(phrase(1:4).eq.'*obs')then

            objnbobs=objnbobs+1
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','obs X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','obs Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','obs Z org',IER)
            OBJXOB(objnbobs)=VX
            OBJYOB(objnbobs)=VY
            OBJZOB(objnbobs)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','obs X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','obs Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','obs Z dis',IER)
            OBJDXOB(objnbobs)=VX
            OBJDYOB(objnbobs)=VY
            OBJDZOB(objnbobs)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','obs rot ang',IER)
            OBJBANGOB(objnbobs,1)=VX
            OBJBANGOB(objnbobs,2)=0.0   ! there is no 2nd rotation
            OBJBANGOB(objnbobs,3)=0.0   ! there is no 3rd rotation
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','obs opacity',IER)
            OBJOPOB(objnbobs)=VX
            CALL EGETW(LOUTSTR,K,WORD,'W','obs blk name',IFLAG)
            OBJBLOCKNAME(objnbobs)=WORD(1:12)

C The name of the construction might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','obs mat name',IFLAG)
            write(OBJBLOCKMAT(objnbobs),'(a)') WORD(1:lnblnk(WORD))
            OBJBLOCKTYP(objnbobs)='obs '
            goto 72
          elseif(phrase(1:4).eq.'*mrt')then

C << not yet implemented >>
            goto 72
          elseif(phrase(1:6).eq.'*photo')then

C << not yet implemented >>
            goto 72
          elseif(phrase(1:10).eq.'*end_block')then
            goto 62
          endif
        elseif(WORD(1:13).eq.'*visual_start')then

C Read another line to get the visual entities. There are slightly different
C formats depending on whether the key is '*vis', '*vis3', 'visp'.
   73     CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,0,'*vis tags',IER)
          k=0
          CALL EGETP(LOUTSTR,K,phrase,'W','visual tag',IER)
          if(phrase(1:5).eq.'*vis3')then
            nbobjvis=nbobjvis+1
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
            OBJXOV(nbobjvis)=VX
            OBJYOV(nbobjvis)=VY
            OBJZOV(nbobjvis)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
            OBJDXOV(nbobjvis)=VX
            OBJDYOV(nbobjvis)=VY
            OBJDZOV(nbobjvis)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang a',IER)
            OBJBANGOV(nbobjvis,1)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang b',IER)
            OBJBANGOV(nbobjvis,2)=VX
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang c',IER)
            OBJBANGOV(nbobjvis,3)=VX
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
            objvisopaq(nbobjvis)=VX
            CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)
            objvisname(nbobjvis)=WORD(1:12)

C The name of the construction might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
            write(objvismat(nbobjvis),'(a)') WORD(1:lnblnk(WORD))
            objvistyp(nbobjvis)='vis3'
C Debug.
            write(6,*) 'vis3',nbobjvis,objvisname(nbobjvis),
     &        objvismat(nbobjvis),objvisopaq(nbobjvis),
     &        OBJBANGOV(nbobjvis,1),OBJBANGOV(nbobjvis,2),
     &        OBJBANGOV(nbobjvis,3),OBJDXOV(nbobjvis),
     &        OBJDYOV(nbobjvis),OBJDZOV(nbobjvis),OBJXOV(nbobjvis),
     &        OBJYOV(nbobjvis),OBJZOV(nbobjvis)
            goto 73    ! check if there is another

          elseif(phrase(1:5).eq.'*visp')then

C A general polygon visual to be associated with the current zone.
C The first line includes (current fixed) integer number of vertices
C followed by number of faces and the name and material.
C The 2nd line has the first 4 coordinates and the 3rd line has the 
C next 4 coordinates. The surface and edge ordering is as obsp. 
            nbobjvis=nbobjvis+1
            CALL EGETWI(LOUTSTR,K,ival,8,8,'F','vis nb vertices',IER)
            CALL EGETWI(LOUTSTR,K,ival,6,6,'F','vis nb faces',IER)
            if(ND.ge.6)then  ! if enough items for opacity
              CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
              objvisopaq(nbobjvis)=VX
            else
              objvisopaq(nbobjvis)=1.0  ! set to opaque if not specified
            endif
            CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)
            objvisname(nbobjvis)=WORD(1:12)
            CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
            write(objvismat(nbobjvis),'(a)') WORD(1:lnblnk(WORD))
            objvistyp(nbobjvis)='visp'

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'first 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 1',IER)
            OBJXVP(nbobjvis,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 1',IER)
            OBJYVP(nbobjvis,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 1',IER)
            OBJZVP(nbobjvis,1)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 2',IER)
            OBJXVP(nbobjvis,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 2',IER)
            OBJYVP(nbobjvis,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 2',IER)
            OBJZVP(nbobjvis,2)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 3',IER)
            OBJXVP(nbobjvis,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 3',IER)
            OBJYVP(nbobjvis,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 3',IER)
            OBJZVP(nbobjvis,3)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 4',IER)
            OBJXVP(nbobjvis,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 4',IER)
            OBJYVP(nbobjvis,4)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 4',IER)
            OBJZVP(nbobjvis,4)=val1

            CALL LSTRIPC(IUNIT,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
            IF(IER.NE.0)goto 1002
            K=0
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 5',IER)
            OBJXVP(nbobjvis,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 5',IER)
            OBJYVP(nbobjvis,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 5',IER)
            OBJZVP(nbobjvis,5)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 6',IER)
            OBJXVP(nbobjvis,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 6',IER)
            OBJYVP(nbobjvis,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 6',IER)
            OBJZVP(nbobjvis,6)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 7',IER)
            OBJXVP(nbobjvis,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 7',IER)
            OBJYVP(nbobjvis,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 7',IER)
            OBJZVP(nbobjvis,7)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 8',IER)
            OBJXVP(nbobjvis,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 8',IER)
            OBJYVP(nbobjvis,8)=val1
            CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 8',IER)
            OBJZVP(nbobjvis,8)=val1
            goto 73

          elseif(phrase(1:4).eq.'*vis')then
            nbobjvis=nbobjvis+1
            CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
            CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
            CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
            OBJXOV(nbobjvis)=VX
            OBJYOV(nbobjvis)=VY
            OBJZOV(nbobjvis)=VZ
            CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
            CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
            CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
            OBJDXOV(nbobjvis)=VX
            OBJDYOV(nbobjvis)=VY
            OBJDZOV(nbobjvis)=VZ
            CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang',IER)
            OBJBANGOV(nbobjvis,1)=VX
            OBJBANGOV(nbobjvis,2)=0.0   ! there is no 2nd rotation
            OBJBANGOV(nbobjvis,3)=0.0   ! there is no 3rd rotation
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
            objvisopaq(nbobjvis)=VX
            CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)
            objvisname(nbobjvis)=WORD(1:12)

C The name of the visual material might contain spaces so use EGETP.
            CALL EGETP(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
            write(objvismat(nbobjvis),'(a)') WORD(1:lnblnk(WORD))
            objvistyp(nbobjvis)='vis '
C Debug.
            write(6,*) 'vis ',nbobjvis,objvisname(nbobjvis),
     &        objvismat(nbobjvis),objvisopaq(nbobjvis),
     &        OBJBANGOV(nbobjvis,1),OBJBANGOV(nbobjvis,2),
     &        OBJBANGOV(nbobjvis,3),OBJDXOV(nbobjvis),
     &        OBJDYOV(nbobjvis),OBJDZOV(nbobjvis),OBJXOV(nbobjvis),
     &        OBJYOV(nbobjvis),OBJZOV(nbobjvis)
            goto 73

          elseif(phrase(1:8).eq.'*vobject')then

C Collection of entities making up an object.
C *vobject,rubish-bskt,square wood rubish bin,4,basket_bk,basket_fr,basket_lf,basket_rt
            nbobjvisobj=nbobjvisobj+1
            CALL EGETW(LOUTSTR,K,WORD,'W','vis obj name',IFLAG)
            objvobjname(nbobjvisobj)=WORD(1:12)
            CALL EGETP(LOUTSTR,K,phrase,'W','vis obj desc',IFLAG)
            write(objvobjdesc(nbobjvisobj),'(a)') 
     &        phrase(1:lnblnk(phrase))
            CALL EGETWI(LOUTSTR,K,ival,1,12,'F','nb vis entities',IER)
            objnbvobjlist(nbobjvisobj)=ival
            do ibvo = 1,ival 
             CALL EGETW(LOUTSTR,K,WORD,'W','entity name',IFLAG)
             write(objvobjlist(nbobjvisobj,ibvo),'(a)')
     &         WORD(1:lnblnk(WORD))
            enddo  ! ibvo
C Debug.
C            write(6,*) 'vis obj ',nbobjvisobj,ival,
C     &        objvobjname(nbobjvisobj),objvobjdesc(nbobjvisobj),
C     &        objvobjlist(nbobjvisobj,1),objvobjlist(nbobjvisobj,2)
            goto 73
          elseif(phrase(1:11).eq.'*end_visual')then
            goto 62
          endif

          goto 62   ! not recognised so jump

        elseif(WORD(1:22).eq.'*zone_viewfactor_start')then

C << to be done >> 
          goto 62
        elseif(WORD(1:24).eq.'*sensor_viewfactor_start')then

C  << to be done >>
          goto 62
        elseif(WORD(1:24).eq.'*sensor_viewfactor_start')then

C  << to be done >>
        else

C Fall through position. Warn and loop back to see if more.
          write(outs,'(2a)') ' Unknown tag in geometry file ',word
          call edisp(iuout,outs)
          goto 62
        endif

C << to this point in the logic... >>

C Now close geometry data file.
   44 CALL ERPFREE(IUNIT,ios)
      RETURN

C Errors for loutstr reads.
 1002 write(outs,'(3a)') 'GEO2OBS: conversion error in...',
     &  LOUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      call edisp(iuout,outs)
      call edisp(iuout,outs2)
      IER=1
      CALL ERPFREE(IUNIT,ios)
      RETURN

      END

 
C ******************** OBJQA 
C OBJQ generates a QA report on a predefined object or writes a block
C of text to patch into a predefined database.

      SUBROUTINE OBJQA(ifilg,ASCIIF,ACT,IER)
#include "building.h"
C #include "model.h"
C #include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "predefined.h"

C espriou.h provides currentfile.
C #include "espriou.h"

      integer lnblnk  ! function definition

C Parameters
      integer ifilg  ! file unit to write
      character ASCIIF*72 ! geometry file to create
      character ACT*2 ! action to take QA or EX
      integer IER    ! zero is ok

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN

      character tab*1    ! separator
      character outs*124
      character outs2*144,outsd*144
      integer lno     ! length of optics name

      logical hasmass,hasbound,hasvis

      tab=','    ! make the separator a comma.
C      objectcat='UNKNOWN'

      if(ACT(1:2).eq.'QA')then

C If act is QA then generate report.
        call edisp(iuout,'Predefined item name and menu entry:')
        write(outs,'(4a)') objname(1:lnblnk(objname)),'   ',
     &    objdesc(1:lnblnk(objdesc))
        call edisp(iuout,outs)
C        write(outs,'(2a)') 'Predefined object category:',objectcat
C        call edisp(iuout,outs)
        hasmass=.false.; hasbound=.false.; hasvis=.false.
        if(nbmass.gt.0) hasmass=.true.
        if(nbbound.gt.0) hasbound=.true.
        if(nbobjvis.gt.0) hasvis=.true.
        if(hasmass.and.hasbound.and.hasvis)then
          write(outs,'(a)') 'Includes visual,mass,boundary'
          call edisp(iuout,outs)
        elseif(hasmass.and.(.NOT.hasbound).and.(.NOT.hasvis))then
          write(outs,'(a)') 'Includes mass'
          call edisp(iuout,outs)
        elseif((.NOT.hasmass).and.hasbound.and.(.NOT.hasvis))then
          write(outs,'(a)') 'Includes boundary'
          call edisp(iuout,outs)
        elseif((.NOT.hasmass).and.hasbound.and.hasvis)then
          write(outs,'(a)') 'Includes visual,boundary'
          call edisp(iuout,outs)
        elseif(hasmass.and.hasbound.and.(.NOT.hasvis))then
          write(outs,'(a)') 'Includes mass,boundary'
          call edisp(iuout,outs)
        elseif((.NOT.hasmass).and.(.NOT.hasbound).and.hasvis)then
          write(outs,'(a)') 'Includes visual'
          call edisp(iuout,outs)
        endif
        write(outs,'(2a)') 'Sourced from: ',
     &    objsource(1:lnblnk(objsource))
        call edisp(iuout,outs)
        write(outs,'(a,3F7.3)') 'Object extents ',objbnds(1),
     &    objbnds(2),objbnds(3)
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,'Notes included:')
        do loop=1,nbobjnotes
          write(outs,'(a)') objnotes(loop)(1:lnblnk(objnotes(loop)))
          call edisp(iuout,outs)
        enddo
        call edisp(iuout,' ')
        call edisp(iuout,'Vertices:')
        DO I = 1,nbvertmass
          WRITE(outs,'(a,3F12.5)',IOSTAT=ios,ERR=13)'*vertex ',
     &      vertmass(I,1),vertmass(I,2),vertmass(I,3)
          call edisp(iuout,outs)
        ENDDO
        if(nbmass.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Surfaces representing mass:')
          do ib=1,nbmass
            XS=0.0; YS=0.0; ZS=0.0
            DO J=1,nbmassv(ib)
              K=J+1
              IF(J.EQ.nbmassv(ib))K=1
              IP1=masjvn(ib,J)
              IP2=masjvn(ib,K)
              XS=XS+vertmass(IP1,2)*vertmass(IP2,3)-
     &           vertmass(IP1,3)*vertmass(IP2,2)
              YS=YS+vertmass(IP1,3)*vertmass(IP2,1)-
     &           vertmass(IP1,1)*vertmass(IP2,3)
              ZS=ZS+vertmass(IP1,1)*vertmass(IP2,2)-
     &           vertmass(IP1,2)*vertmass(IP2,1)
            enddo
            ZAREA=0.5*SQRT(XS*XS+YS*YS+ZS*ZS)
    
            lnsm=lnblnk(msurmat(ib))
            lnsn=lnblnk(msurname(ib))
            lnopt=lnblnk(msuropt(ib))
            WRITE(outs,'(8a,i2,a,f6.2)',IOSTAT=ios,ERR=13)
     &        '*mass',tab,msurname(ib)(1:lnsn),tab,
     &        msurmat(ib)(1:lnsm),tab,msuropt(ib)(1:lnopt),
     &        tab,nbmassv(ib),tab,ZAREA
            call edisp(iuout,outs)
          enddo
        endif
        if(nbbound.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Surfaces at object bounds:')
          do ib=1,nbbound
            lnsm=lnblnk(bsurmat(ib))
            lnsn=lnblnk(bsurname(ib))
            lnopt=lnblnk(bsuropt(ib))
            WRITE(outs,'(8a,i2,2a)',IOSTAT=ios,ERR=13)
     &        '*surf',tab,bsurname(ib)(1:lnsn),tab,
     &        bsurmat(ib)(1:lnsm),tab,bsuropt(ib)(1:lnopt),
     &        tab,nbboundv(ib)
            call edisp(iuout,outs)
          enddo  ! of ib
        endif
        if(nbbedge.gt.0)then
          WRITE(outs,'(a,12I3)',IOSTAT=ios,ERR=13)'Bounding edges:',
     &      (edgejvn(J),J=1,nbbedge)
          call edisp(iuout,outs)
        endif
        if(objnbobs.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Solar obstructions:')
          do ib=1,objnbobs
            WRITE(outs,'(5a)',IOSTAT=ios,ERR=13) 
     &        objblocktyp(ib),tab,OBJBLOCKNAME(ib),tab,OBJBLOCKMAT(ib)
            call edisp(iuout,outs)
          enddo  ! of ib
        endif
        if(nbobjvis.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Visual (simple) entities:')
          do ib=1,nbobjvis
            WRITE(outs,'(5a)',IOSTAT=ios,ERR=13) 
     &        objvistyp(ib),' ',OBJVISNAME(ib),' ',OBJVISMAT(ib)
            call edisp(iuout,outs)
          enddo  ! of ib
        endif
        if(nbobjvisobj.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Visual objects (collections):')
          do ib=1,nbobjvisobj
            lno=lnblnk(OBJVOBJNAME(ib))
            lnd=lnblnk(OBJVOBJDESC(ib))
            WRITE(outs,'(6a,i2)',IOSTAT=ios,ERR=13) 
     &        '*vobject',tab,OBJVOBJNAME(ib)(1:lno),tab,
     &        OBJVOBJDESC(ib)(1:lnd),tab,objnbvobjlist(ib)
            call edisp(iuout,outs)
          enddo  ! of ib
        endif

      elseif(ACT(1:2).eq.'EX')then

C If act is EX then export a block of text.
        CALL EFOPSEQ(IFILG,ASCIIF,4,IER)
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
        write(ifilg,'(5a)') '*item,',objname(1:lnblnk(objname)),',',
     &    objdesc(1:lnblnk(objdesc)),' # tag name menu entry'
        write(ifilg,'(2a)') '*incat,',objectcat
        hasmass=.false.; hasbound=.false.; hasvis=.false.
        if(nbmass.gt.0) hasmass=.true.
        if(nbbound.gt.0) hasbound=.true.
        if(nbobjvis.gt.0) hasvis=.true.
        if(hasmass.and.hasbound.and.hasvis)then
          write(ifilg,'(a)') '*includes,visual,mass,boundary'
        elseif(hasmass.and.(.NOT.hasbound).and.(.NOT.hasvis))then
          write(ifilg,'(a)') '*includes,mass'
        elseif((.NOT.hasmass).and.hasbound.and.(.NOT.hasvis))then
          write(ifilg,'(a)') '*includes,boundary'
        elseif((.NOT.hasmass).and.hasbound.and.hasvis)then
          write(ifilg,'(a)') '*includes,visual,boundary'
        elseif(hasmass.and.hasbound.and.(.NOT.hasvis))then
          write(ifilg,'(a)') '*includes,mass,boundary'
        elseif((.NOT.hasmass).and.(.NOT.hasbound).and.hasvis)then
          write(ifilg,'(a)') '*includes,visual'
        endif
        write(ifilg,'(2a)') '*sourced,',
     &    objsource(1:lnblnk(objsource))
        write(ifilg,'(a)') '*origin,0.0,0.0,0.0  # local origin'
        write(ifilg,'(a,3F7.3,a)') '*bounding_box,',objbnds(1),
     &    objbnds(2),objbnds(3),'  # extents of object'

        write(ifilg,'(a)') '*Text'
        write(ifilg,'(a)') objnotes(1)
        write(ifilg,'(a)') '*End_text'

C Write vertices with minimal white space comma separated.
        write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
        DO I = 1,nbvertmass
          WRITE(outs,'(a,3F12.5)',IOSTAT=ios,ERR=13)'*vertex ',
     &      vertmass(I,1),vertmass(I,2),vertmass(I,3)
          call SDELIM(outs,outsd,'C',IW)
          write(ifilg,'(2a,i3)',IOSTAT=IOS,ERR=13) 
     &      outsd(1:lnblnk(outsd)),'  # ',I
        ENDDO

C If there are mass surfaces write.
        if(nbmass.gt.0)then
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
          do ib=1,nbmass
            lnsm=lnblnk(msurmat(ib))
            lnsn=lnblnk(msurname(ib))
            lnopt=lnblnk(msuropt(ib))
            WRITE(outs,'(8a,i2,a,32i3)',IOSTAT=ios,ERR=13)
     &        '*mass',tab,msurname(ib)(1:lnsn),tab,
     &        msurmat(ib)(1:lnsm),tab,msuropt(ib)(1:lnopt),
     &        tab,nbmassv(ib),tab,(masjvn(ib,J),J=1,nbmassv(ib))
            write(ifilg,'(2a,i3)',IOSTAT=IOS,ERR=13) 
     &        outs(1:lnblnk(outs)),'  # ',ib
          enddo
        endif

C If there are boundary surface write.
        if(nbbound.gt.0)then
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
          do ib=1,nbbound
            lnsm=lnblnk(bsurmat(ib))
            lnsn=lnblnk(bsurname(ib))
            lnopt=lnblnk(bsuropt(ib))
            WRITE(outs,'(8a,i2,a,32i3)',IOSTAT=ios,ERR=13)
     &        '*surf',tab,bsurname(ib)(1:lnsn),tab,
     &        bsurmat(ib)(1:lnsm),tab,bsuropt(ib)(1:lnopt),
     &        tab,nbboundv(ib),tab,(boundjvn(ib,J),J=1,nbboundv(ib))
            write(ifilg,'(2a,i3)',IOSTAT=IOS,ERR=13) 
     &        outs(1:lnblnk(outs)),'  # ',ib
          enddo  ! of ib
        endif

C If there is an edge around a facade entity include this info.
        if(nbbedge.gt.0)then
          WRITE(outs,'(2a,12I3)',IOSTAT=ios,ERR=13)'*bounds',
     &      tab,(edgejvn(J),J=1,nbbedge)
          write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &      outs(1:lnblnk(outs)),'  # edges around the facade'
        endif

C If predefined included shading obstructions print.
        if(objnbobs.gt.0)then
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
          do ib=1,objnbobs
            if(objblocktyp(ib)(1:4).eq.'obs3')then
              WRITE(outs,'(2a,9F9.4,F6.2,1X,A)',IOSTAT=ios,
     &          ERR=13) 
     &          '*obs3',tab,OBJXOB(ib),OBJYOB(ib),OBJZOB(ib),
     &          OBJDXOB(ib),OBJDYOB(ib),OBJDZOB(ib),OBJBANGOB(ib,1),
     &          OBJBANGOB(ib,2),OBJBANGOB(ib,3),OBJOPOB(ib),
     &          OBJBLOCKNAME(ib)
              call SDELIM(outs,outsd,'C',IW)
              lnbm=lnblnk(OBJBLOCKMAT(ib))
              write(ifilg,'(4a,i3)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',OBJBLOCKMAT(ib)(1:lnbm),
     &          '  # block ',ib
            elseif(objblocktyp(ib)(1:4).eq.'obsp')then
              WRITE(outs,'(2a,F6.2,1X,A)',IOSTAT=ios,ERR=13) 
     &          '*obsp',' 8 6 ',OBJOPOB(ib),OBJBLOCKNAME(ib)
              call SDELIM(outs,outsd,'C',IW)
              lnbm=lnblnk(OBJBLOCKMAT(ib))
              write(ifilg,'(4a,i3,a)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',OBJBLOCKMAT(ib)(1:lnbm),
     &          '  # block ',ib,' coords follow:'

              WRITE(outs,'(12F9.4)',IOSTAT=ios,ERR=13) 
     &          OBJXBP(ib,1),OBJYBP(ib,1),OBJZBP(ib,1),
     &          OBJXBP(ib,2),OBJYBP(ib,2),OBJZBP(ib,2),
     &          OBJXBP(ib,3),OBJYBP(ib,3),OBJZBP(ib,3),
     &          OBJXBP(ib,4),OBJYBP(ib,4),OBJZBP(ib,4)
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),'  # 1-4 '

              WRITE(outs,'(12F9.4)',IOSTAT=ios,ERR=13) 
     &          OBJXBP(ib,5),OBJYBP(ib,5),OBJZBP(ib,5),
     &          OBJXBP(ib,6),OBJYBP(ib,6),OBJZBP(ib,6),
     &          OBJXBP(ib,7),OBJYBP(ib,7),OBJZBP(ib,7),
     &          OBJXBP(ib,8),OBJYBP(ib,8),OBJZBP(ib,8)
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),'  # 5-8 '

            elseif(objblocktyp(ib)(1:4).eq.'obs ')then
              WRITE(outs,'(2a,7F9.4,F6.2,1X,A)',IOSTAT=ios,
     &          ERR=13) 
     &          '*obs',tab,OBJXOB(ib),OBJYOB(ib),OBJZOB(ib),
     &          OBJDXOB(ib),OBJDYOB(ib),OBJDZOB(ib),
     &          OBJBANGOB(ib,1),objopob(ib),OBJBLOCKNAME(ib)
              call SDELIM(outs,outsd,'C',IW)
              lnbm=lnblnk(OBJBLOCKMAT(ib))
              write(ifilg,'(4a,i3)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',OBJBLOCKMAT(ib)(1:lnbm),
     &          '  # block ',ib
            endif
          enddo  !  ib
        endif

C Visual entities are in the next section.
        if(nbobjvis.gt.0)then
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
          do ib=1,nbobjvis
            if(objvistyp(ib)(1:4).eq.'vis3')then
              WRITE(outs,'(2a,9F10.4,F6.2,2A)',IOSTAT=ios,
     &          ERR=13) 
     &          '*vis3',tab,OBJXOV(ib),OBJYOV(ib),OBJZOV(ib),
     &          OBJDXOV(ib),OBJDYOV(ib),OBJDZOV(ib),
     &          OBJBANGOV(ib,1),OBJBANGOV(ib,2),OBJBANGOV(ib,3),
     &          objvisopaq(ib),' ',OBJVISNAME(ib)
              write(6,*) outs(1:lnblnk(outs))
              lnvm=lnblnk(objvismat(ib))
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(4a,i3)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',objvismat(ib)(1:lnvm),
     &          '  # visual block ',ib
            elseif(objvistyp(ib)(1:4).eq.'visp')then
              WRITE(outs,'(2a,F6.2,2A)',IOSTAT=ios,ERR=13) 
     &          '*visp',' 8 6 ',objvisopaq(ib),' ',OBJVISNAME(ib)
              lnvm=lnblnk(objvismat(ib))
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(4a,i3,a)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',objvismat(ib)(1:lnvm),
     &          '  # visual ',ib,' coords follow:'

              WRITE(outs,'(12F9.4)',IOSTAT=ios,ERR=13) 
     &          OBJXVP(ib,1),OBJYVP(ib,1),OBJZVP(ib,1),
     &          OBJXVP(ib,2),OBJYVP(ib,2),OBJZVP(ib,2),
     &          OBJXVP(ib,3),OBJYVP(ib,3),OBJZVP(ib,3),
     &          OBJXVP(ib,4),OBJYVP(ib,4),OBJZVP(ib,4)
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),'  # 1-4 '

              WRITE(outs,'(12F9.4)',IOSTAT=ios,ERR=13) 
     &          OBJXVP(ib,5),OBJYVP(ib,5),OBJZVP(ib,5),
     &          OBJXVP(ib,6),OBJYVP(ib,6),OBJZVP(ib,6),
     &          OBJXVP(ib,7),OBJYVP(ib,7),OBJZVP(ib,7),
     &          OBJXVP(ib,8),OBJYVP(ib,8),OBJZVP(ib,8)
              write(6,*) outs(1:lnblnk(outs))
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(2A)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),'  # 5-8 '

            elseif(objvistyp(ib)(1:4).eq.'vis ')then
              WRITE(outs,'(2a,7F10.4,F6.2,2A)',IOSTAT=ios,ERR=13) 
     &          '*vis',tab,OBJXOV(ib),OBJYOV(ib),OBJZOV(ib),
     &          OBJDXOV(ib),OBJDYOV(ib),OBJDZOV(ib),
     &          OBJBANGOV(ib,1),objvisopaq(ib),' ',OBJVISNAME(ib)
              write(6,*) outs
              call SDELIM(outs,outsd,'C',IW)
              write(ifilg,'(4a,i3)',IOSTAT=IOS,ERR=13) 
     &          outsd(1:lnblnk(outsd)),' ',objvismat(ib)(1:lnvm),
     &          '  # visual ',ib
            endif
          enddo   ! ib
        endif     ! visual entities

C If there are objects (collections of entities) write them.
        if(nbobjvisobj.gt.0)then
          write(ifilg,'(a)',IOSTAT=IOS,ERR=13) '# '
          do ib=1,nbobjvisobj
            lno=lnblnk(OBJVOBJNAME(ib))
            lnd=lnblnk(OBJVOBJDESC(ib))
            WRITE(outs,'(6a,i2,a)',IOSTAT=ios,ERR=13) 
     &        '*vobject',tab,OBJVOBJNAME(ib)(1:lno),tab,
     &        OBJVOBJDESC(ib)(1:lnd),tab,objnbvobjlist(ib),tab

C Append the list of visual entities comma separated. Similar to logic
C in esru_lib.F subroutine aslist.
            outs2=' '; ix=1; ixl=0
            do ibo=1,objnbvobjlist(ib)
              lna=lnblnk(objvobjlist(ib,ibo))
              if(lna.eq.1)then
                ixl=ix
              else
                ixl=ix+(lna-1)
              endif
              write(outs2(ix:ixl),'(a)')objvobjlist(ib,ibo)(1:lna)
              if(ibo.lt.objnbvobjlist(ib))then
                write(outs2(ixl+1:ixl+1),'(a)') ','
                ix=ix+lna+1
              else
                ix=ix+lna+1
              endif
            enddo  ! of ibo
            write(ifilg,'(2a)',IOSTAT=IOS,ERR=13) 
     &        outs(1:lnblnk(outs)),outs2(1:lnblnk(outs2))
          enddo    ! of ib
        endif      ! of nbvobj

      endif

      return

C Error messages.
   13 if(IOS.eq.2)then
        CALL USRMSG('No permission to write ',ASCIIF,'W')
      else
        CALL USRMSG('File write error in ',ASCIIF,'W')
      endif
      IER=1

      end
       
C ******************************** RPREDEFCOM
C Read tag-data version of predefined objects file item into commons.
      SUBROUTINE RPREDEFCOM(IFA,LASCI,name,IER)
      
      IMPLICIT NONE
      
#include "building.h"
C #include "model.h"
C #include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
#include "predefined.h"
#include "espriou.h"
C espriou.h provides currentfile.

      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144 ! ascii file name
      character name*12   ! item to embed
      integer IER         ! error return where zero is ok, 
                          ! ier=-1 file not found, ier=-2 no objects
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN
      character OUTS*124,OUTSTR*124,loutstr*248
      character WORD*24,phrase*32
      logical CONT,foundit
      integer loop  ! for looping
      real verpre     ! version of file
      real VAL,VAL1,VX,VY,VZ
      integer iflag,istat,iv,k,nd
      integer ibvo

C Open the file.
      call EFOPSEQ(IFA,LASCI,1,IER)
      if(IER.EQ.-301) then
        call edisp(IUOUT,'Warning: filename was blank')
        CALL ERPFREE(IFA,ISTAT)
        return
      ELSEIF(IER.NE.0.AND.IER.NE.-301) THEN
        WRITE(OUTS,'(3A)')
     &    ' Problem opening ',LASCI(1:LNBLNK(LASCI)),'.'
        call edisp(IUOUT,outs)
        CALL ERPFREE(IFA,ISTAT)
        return
      endif
      write(currentfile,'(a)') LASCI(1:lnblnk(LASCI))
      foundit=.false.

C Clear common blocks prior to reading new object.
      call clearobjcommons()
      CONT=.TRUE.  ! assume reads are ok
 
C Read the file header and check for first-line tag.
      CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'1st line of pre',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      IF(LOUTSTR(1:11).EQ.'*PREDEFINED')THEN
        verpre=0.0
        if(ND.gt.1)then
          K=11
          CALL EGETWR(LOUTSTR,K,verpre,0.,2.,'-','version',IER)
        endif
        CALL EDISP(IUOUT,' ')    
        WRITE(OUTS,'(3A)') 
     &    'Opened predefined objects file: ',LASCI(1:LNBLNK(LASCI)),'.' 
        CALL USRMSG(OUTS,' ','-') 
      else
        WRITE(OUTS,'(3A)') 'File: ',LASCI(1:LNBLNK(LASCI)), 
     &    ' is not a predefined objects file.'
        CALL USRMSG(OUTS,' ','W') 
        ier=1
        return
      endif 
   
C Read in the header lines of the file, look for key tags.
  20  CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'header lines',IER)
      IF(IER.NE.0) CONT=.FALSE. 
      K=0
      CALL EGETW(LOUTSTR,K,WORD,'W','predefined tags',IER)
      IF(IER.NE.0) CONT=.FALSE.
      if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*Text')then
  21    CALL LSTRIPC(IFA,LOUTSTR,99,ND,0,'text lines',IER)
        IF(IER.NE.0) CONT=.FALSE. 
        K=0
        CALL EGETW(LOUTSTR,K,WORD,'W','text line',IER)
        IF(IER.NE.0) CONT=.FALSE.
        if(WORD(1:9).EQ.'*End_text')then
          continue  ! look for more key words
        else

C Save the line of text if in the focused item.
          if(foundit)then
            nbobjnotes=nbobjnotes+1
            write(objnotes(nbobjnotes),'(a)') LOUTSTR(1:72)
          endif
          goto 21  ! get another line of text
        endif
      elseif(WORD(1:9).EQ.'*End_text')then
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*Category')then
        if(foundit)then
          CALL EGETW(LOUTSTR,K,WORD,'W','item cat',IER)
          write(objectcat,'(a)') WORD(1:lnblnk(WORD))
        endif 
      elseif(WORD(1:13).EQ.'*End_category')then
        continue  ! look for more key words
      elseif(WORD(1:15).EQ.'*End_predefined')then
        CALL ERPFREE(IFA,ISTAT)
        if(foundit)then
          goto 42  ! process what has been found
        endif
      elseif(WORD(1:5).EQ.'*item')then
        CALL EGETW(LOUTSTR,K,WORD,'W','item name',IER)
        if(WORD(1:12).ne.name(1:12))then
          continue
        else

C Located the object requested.
          write(objname,'(a)') WORD(1:lnblnk(WORD))
          CALL EGETRM(LOUTSTR,K,phrase,'W','menu',IER)
          write(objdesc,'(a)') phrase(1:lnblnk(phrase))
          foundit=.true.
        endif
      elseif(WORD(1:9).EQ.'*end_item')then
        if(foundit)then
          goto 42  ! process what has been found
        endif
      elseif(WORD(1:6).EQ.'*incat')then
        if(foundit)then
          CALL EGETW(LOUTSTR,K,WORD,'W','item cat',IER)
        endif
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*includes')then

C Check topics included. << work to be done >>
        if(foundit)then

        endif
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*sourced')then
        if(foundit)then
          CALL EGETRM(LOUTSTR,K,phrase,'W','source',IER)
          write(objsource,'(a)') phrase(1:lnblnk(phrase))
        endif
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*origin')then

C Read origin.
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org X',IER)
        if(foundit)objorg(1)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org Y',IER)
        if(foundit)objorg(2)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','org Z',IER)
        if(foundit)objorg(3)=VAL
        IF(IER.NE.0) CONT=.FALSE.
        continue  ! look for more key words
      elseif(WORD(1:13).EQ.'*bounding_box')then

C Bounding box of the object.
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bnd X',IER)
        if(foundit)objbnds(1)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bnd Y',IER)
        if(foundit)objbnds(2)=VAL
        CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','bnd Z',IER)
        if(foundit)objbnds(3)=VAL
        IF(IER.NE.0) CONT=.FALSE.
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*vertex')then

C Remember vertices associated with mass and add transform.
        if(foundit)then
          nbvertmass=nbvertmass+1
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass X',IER)
          vertmass(nbvertmass,1)=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass Y',IER)
          vertmass(nbvertmass,2)=VAL
          CALL EGETWR(LOUTSTR,K,VAL,0.0,999.,'W','mass Z',IER)
          vertmass(nbvertmass,3)=VAL
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*mass')then

C Remember mass definitions.
        if(foundit)then
          nbmass=nbmass+1
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','mass name',IER)
          write(msurname(nbmass),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETP(LOUTSTR,K,phrase,'W','mass mat',IER)
          write(msurmat(nbmass),'(a)') phrase(1:lnblnk(phrase))
          CALL EGETW(LOUTSTR,K,WORD,'W','mass optics',IER)
          write(msuropt(nbmass),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETWI(LOUTSTR,K,iv,3,MV,'F','nb assoc v',IER)
          nbmassv(nbmass)=iv
          IF(IER.NE.0) CONT=.FALSE.
          do loop=1,nbmassv(nbmass)
            CALL EGETWI(LOUTSTR,K,iv,1,MOTV,'F','assoc v',IER)
            masjvn(nbmass,loop)=iv
          enddo
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*surf')then

C Remember boundary surface definitions.
        if(foundit)then
          nbbound=nbbound+1
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','surf name',IER)
          write(bsurname(nbbound),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETP(LOUTSTR,K,phrase,'W','surf mat',IER)
          write(bsurmat(nbbound),'(a)') phrase(1:lnblnk(phrase))
          CALL EGETW(LOUTSTR,K,WORD,'W','surf optics',IER)
          write(bsuropt(nbbound),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETWI(LOUTSTR,K,iv,3,MV,'F','nb assoc v',IER)
          nbboundv(nbbound)=iv
          IF(IER.NE.0) CONT=.FALSE.
          do loop=1,nbboundv(nbbound)
            CALL EGETWI(LOUTSTR,K,iv,1,MOTV,'F','assoc v',IER)
            boundjvn(nbbound,loop)=iv
          enddo
        endif
        continue  ! look for more key words
      elseif(WORD(1:7).EQ.'*bounds')then

C Facade objects might have edges for use with parent surface.
        CALL EGETWI(LOUTSTR,K,iv,3,16,'F','nb assoc edge v',IER)
        nbbedge=iv
        IF(IER.NE.0) CONT=.FALSE.
        do loop=1,nbbedge
          CALL EGETWI(LOUTSTR,K,iv,1,32,'F','assoc edge v',IER)
          edgejvn(loop)=iv
        enddo
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*visp')then

C Scan visp definition and instanciate model entity.
        if(foundit)then
          nbobjvis=nbobjvis+1
          CALL EGETWI(LOUTSTR,K,iv,8,8,'F','vis nb vertices',IER)
          CALL EGETWI(LOUTSTR,K,iv,6,6,'F','vis nb faces',IER)
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          objvisopaq(nbobjvis)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Update the compound object reference.
          write(objvisname(nbobjvis),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETP(LOUTSTR,K,phrase,'W','vis mat name',IFLAG)
          write(objvismat(nbobjvis),'(a)') phrase(1:lnblnk(phrase))
          objvistyp(nbobjvis)='visp'
          IF(IER.NE.0) CONT=.FALSE.

C Grab all of the coordinates and transform.
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'first 4 coord',IER)
          K=0
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 1',IER)
          objxvp(nbobjvis,1)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 1',IER)
          objyvp(nbobjvis,1)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 1',IER)
          objzvp(nbobjvis,1)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 2',IER)
          objxvp(nbobjvis,2)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 2',IER)
          objyvp(nbobjvis,2)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 2',IER)
          objzvp(nbobjvis,2)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 3',IER)
          objxvp(nbobjvis,3)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 3',IER)
          objyvp(nbobjvis,3)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 3',IER)
          objzvp(nbobjvis,3)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 4',IER)
          objxvp(nbobjvis,4)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 4',IER)
          objyvp(nbobjvis,4)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 4',IER)
          objzvp(nbobjvis,4)=val1
          IF(IER.NE.0) CONT=.FALSE.
          
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
          K=0
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 5',IER)
          objxvp(nbobjvis,5)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 5',IER)
          objyvp(nbobjvis,5)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 5',IER)
          objzvp(nbobjvis,5)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 6',IER)
          objxvp(nbobjvis,6)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 6',IER)
          objyvp(nbobjvis,6)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 6',IER)
          objzvp(nbobjvis,6)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 7',IER)
          objxvp(nbobjvis,7)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 7',IER)
          objyvp(nbobjvis,7)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 7',IER)
          objzvp(nbobjvis,7)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','XVP 8',IER)
          objxvp(nbobjvis,8)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','YVP 8',IER)
          objyvp(nbobjvis,8)=val1
          CALL EGETWR(LOUTSTR,K,val1,-999.,998.,'W','ZVP 8',IER)
          objzvp(nbobjvis,8)=val1
          IF(IER.NE.0) CONT=.FALSE.
        else

C Even if we are not focused on it need to jump 2 lines.
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'first 4 coord',IER)
          CALL LSTRIPC(IFA,LOUTSTR,99,ND,1,'2nd 4 coord',IER)
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*vis3')then

        if(foundit)then
          nbobjvis=nbobjvis+1
          CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
          CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
          CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
          objxov(nbobjvis)=VX
          objyov(nbobjvis)=VY
          objzov(nbobjvis)=VZ

          CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
          CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
          CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
          OBJDXOV(nbobjvis)=VX
          OBJDYOV(nbobjvis)=VY
          OBJDZOV(nbobjvis)=VZ

          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang a',IER)
          objbangov(nbobjvis,1)=VX
          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang b',IER)
          objbangov(nbobjvis,2)=VX
          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang c',IER)
          objbangov(nbobjvis,3)=VX
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          objvisopaq(nbobjvis)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)
C Set name.
          write(objvisname(nbobjvis),'(a)') WORD(1:lnblnk(WORD))

C The name of the construction might contain spaces so use EGETP.
          CALL EGETP(LOUTSTR,K,phrase,'W','vis mat name',IFLAG)
          write(objvismat(nbobjvis),'(a)') phrase(1:lnblnk(phrase))
          objvistyp(nbobjvis)='vis3'
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:4).EQ.'*vis')then

C Visual entity with only one rotation.
        if(foundit)then
          nbobjvis=nbobjvis+1
          CALL EGETWR(LOUTSTR,K,VX,-999.,998.,'W','vis X org',IER)
          CALL EGETWR(LOUTSTR,K,VY,-999.,998.,'W','vis Y org',IER)
          CALL EGETWR(LOUTSTR,K,VZ, -99., 99.,'W','vis Z org',IER)
          objxov(nbobjvis)=VX
          objyov(nbobjvis)=VY
          objzov(nbobjvis)=VZ

          CALL EGETWR(LOUTSTR,K,VX,0.,150.,'W','vis X dis',IER)
          CALL EGETWR(LOUTSTR,K,VY,0.,150.,'W','vis Y dis',IER)
          CALL EGETWR(LOUTSTR,K,VZ,0.,150.,'W','vis Z dis',IER)
          OBJDXOV(nbobjvis)=VX
          OBJDYOV(nbobjvis)=VY
          OBJDZOV(nbobjvis)=VZ

          CALL EGETWR(LOUTSTR,K,VX,-359.,359.,'W','vis rot ang a',IER)
          objbangov(nbobjvis,1)=VX
          objbangov(nbobjvis,2)=0.0
          objbangov(nbobjvis,3)=0.0
          CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
          objvisopaq(nbobjvis)=VX
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Remember the name.
          write(objvisname(nbobjvis),'(a)') WORD(1:lnblnk(WORD))

C The name of the visual material might contain spaces so use EGETP.
          CALL EGETP(LOUTSTR,K,phrase,'W','vis mat name',IFLAG)
          write(objvismat(nbobjvis),'(a)') phrase(1:lnblnk(phrase))
          objvistyp(nbobjvis)='vis '
          IF(IER.NE.0) CONT=.FALSE.
        endif
        continue  ! look for more key words
      elseif(WORD(1:8).EQ.'*vobject')then

C Collection of entities making up an object.
        if(foundit)then
          nbobjvisobj=nbobjvisobj+1
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis obj name',IFLAG)

          write(objvobjname(nbobjvisobj),'(a)') WORD(1:lnblnk(WORD))
          CALL EGETP(LOUTSTR,K,phrase,'W','vis obj desc',IFLAG)
          write(objvobjdesc(nbobjvisobj),'(a)') 
     &      phrase(1:lnblnk(phrase))
          CALL EGETWI(LOUTSTR,K,iv,1,14,'F','nb vis entities',IER)
          objnbvobjlist(nbobjvisobj)=iv
          do ibvo = 1,iv
            WORD='            ' 
            CALL EGETW(LOUTSTR,K,WORD,'W','entity name',IFLAG)
            write(objvobjlist(nbobjvisobj,ibvo),'(a)') WORD(1:12)
          enddo  ! ibvo
        endif
        continue  ! look for more key words
      else
        call edisp248(iuout,LOUTSTR,100)
        write(outs,'(3a)') 'has unknown tag ',WORD(1:lnblnk(WORD)),
     &    ' continuing...'
        call edisp(iuout,outs)
        continue  ! look for more key words
      endif

C If there were no errors in reading header line then read another.
      if(CONT)then
        goto 20
      else
        write(outstr,'(a)')loutstr(1:100)
        call usrmsg('Error reading predefined file @',outstr,'W')
        ier=1
        CLOSE(IFA)
        RETURN
      endif

   42 continue
      CLOSE(IFA)  ! close so can use again

      return
      end

C clearobjcommons resets and clears the common blocks in predefined.h
      subroutine clearobjcommons()
#include "building.h"
#include "predefined.h"

C Clear predefined object variables.
      objname=' '; objdesc=' '; objectcat=' '; objsource=' '
      nbobjnotes=0
      do loop=1,6
        objnotes(loop)='  '
      enddo
      objorg(1)=0.0; objorg(2)=0.0; objorg(3)=0.0
      objbnds(1)=0.0; objbnds(2)=0.0; objbnds(3)=0.0
      nbvertmass=0; nbmass=0; nbmassv=0; nbbound=0
      nbobjvis=0; nbobjvisobj=0; objnbobs=0
      nbbedge=0
      edgejvn(1)=0; edgejvn(2)=0
      do loop=1,MOTV
        vertmass(loop,1)=0.0; vertmass(loop,2)=0.0
        vertmass(loop,3)=0.0
      enddo
      do loop=1,MOMS
        msurname(loop)=' '; msurmat(loop)=' '
        msuropt(loop)=' '; nbmassv(loop)=0
      enddo
      do loop=1,MOBS
        bsurname(loop)=' '; bsurmat(loop)=' '
        bsuropt(loop)=' '; nbboundv(loop)=0
      enddo
      do loop=1,MOMB
        objvisname(loop)=' '; objvismat(loop)=' '
        objvistyp(loop)=' '; objblockname(loop)=' ' 
        objblockmat(loop)=' '; objblocktyp(loop)=' '
        objxob(loop)=0.0; objyob(loop)=0.0; objzob(loop)=0.0
        objdxob(loop)=0.0; objdyob(loop)=0.0; objdzob(loop)=0.0
      enddo
      do loop=1,MOMVB
        objvobjname(loop)=' '
        objvobjdesc(loop)=' '; objnbvobjlist(loop)=0
      enddo

      return
      end


