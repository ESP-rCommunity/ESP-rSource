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
 
C   predefscan - scans a predefined objects file and can be called with 
C             mode 'L' - light scan silent and mode 'R' - reporting mode.

C ******************************** PREDEFSCAN
C Read tag-data version of predefined objects file.
      SUBROUTINE PREDEFSCAN(IFA,LASCI,numcat,cats,numitems,
     &  names,menus,bounding,IER)

#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.

      integer lnblnk  ! function definition

C Parameters
      integer IFA         ! ascii file unit number
      character LASCI*144 ! ascii file name
      integer numcat      ! how many categoris found
      integer numitems    ! how many items found
      character cats*32   ! category menu entries
      character names*12  ! list ofnames of objects
      character menus*32  ! list of menu entries
      dimension cats(2),names(32),menus(32)
      real bounding(32,3) ! width, depth, height     
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
      numcat=0
      cats(1)=' '; cats(2)=' '
      numitems=0
      do loop=1,32
        names(loop)='  '
        menus(loop)='  '
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
        CALL EGETW(LOUTSTR,K,WORD,'W','cat tags',IER)
        CALL EGETW(LOUTSTR,K,WORD,'W','cat tags',IER)
        CALL EGETRM(LOUTSTR,K,phrase,'W','category',IER)
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

C work-in-progress.
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*includes')then

C work-in-progress.
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
      elseif(WORD(1:7).EQ.'*vertex')then
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*mass')then
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
      SUBROUTINE PREDEFEMBED(IFA,LASCI,ICOMP,DX,DY,DZ,DA,name,prec,IER)
      
      IMPLICIT NONE
      
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
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
      character OUTS*124,OUTSTR*124,loutstr*248
      character WORD*24,phrase*32
      logical CONT,foundit
      logical close
      integer loop,i,j,ibe,ix,ixx,nbv   ! for looping
      real verpre     ! version of file
      character msurname*12
      character msurmat*32
      character msuropt*32
      dimension msurname(4),msurmat(4),msuropt(4)
      integer nbvertmass  ! how many mass vertices
      integer nbmass      ! now many mass surfaces
      real objorg(3)      ! origin of selected.
      real vertmass(8,3)  ! vertices associated with mass
      integer nbmassv(4)  ! how many vertices per mass
      integer masjvn(4,12)! list of associated vertices
      integer nbprevious  ! how many visual entities aleady in zone
      integer nbprevnztv  ! how many vertices in zone prior to import
      real A,CA,PI,SA,VAL,VAL1,VX,VY,VZ,YR
      real X1,XR,Y1,XX1,YY1,XXX,YYY
      integer icon,iflag,istat,iv,k,nd
      integer ibvo,nbvo

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
      objorg(1)=0.0; objorg(2)=0.0; objorg(3)=0.0
      nbvertmass=0; nbmass=0; nbmassv=0
      do loop=1,8
        vertmass(loop,1)=0.0; vertmass(loop,2)=0.0
        vertmass(loop,3)=0.0
      enddo
      do loop=1,4
        msurname(loop)=' '; msurmat(loop)=' '
        msuropt(loop)=' '
      enddo
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
          foundit=.true.
        endif
      elseif(WORD(1:9).EQ.'*end_item')then
        if(foundit)then
          goto 42  ! process what has been found
        endif
      elseif(WORD(1:6).EQ.'*incat')then
        continue  ! look for more key words
      elseif(WORD(1:9).EQ.'*includes')then

C Check topics included. << work to be done >>
        if(foundit)then

        endif
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
          CALL EGETWI(LOUTSTR,K,iv,4,12,'F','nb assoc v',IER)
          nbmassv(nbmass)=iv
          IF(IER.NE.0) CONT=.FALSE.
          do loop=1,nbmassv(nbmass)
            CALL EGETWI(LOUTSTR,K,iv,1,8,'F','assoc v',IER)
            masjvn(nbmass,loop)=iv
          enddo
        endif
        continue  ! look for more key words
      elseif(WORD(1:5).EQ.'*visp')then

C Scan visp definition and instanciate model entity.
        if(foundit)then
          nbvis(icomp)=nbvis(icomp)+1
          nbv=nbvis(icomp)
          CALL EGETWI(LOUTSTR,K,iv,8,8,'F','vis nb vertices',IER)
          CALL EGETWI(LOUTSTR,K,iv,6,6,'F','vis nb faces',IER)
          if(ND.ge.6)then  ! if enough items for opacity
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
            OPOV(icomp,nbv)=VX
          else
            OPOV(icomp,nbv)=1.0  ! set to opaque if not specified
          endif
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis blk name',IFLAG)

C Prepend prec to name and update the compound object reference.
          write(VISNAME(icomp,nbv),'(2a)') prec,WORD(1:11)
          LNVISNAME(icomp,nbv)=lnblnk(VISNAME(icomp,nbv))
          CALL EGETW(LOUTSTR,K,WORD,'W','vis mat name',IFLAG)
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
          if(ND.ge.13)then  ! if enough items for opacity
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
            OPOV(icomp,nbv)=VX
          else
            OPOV(icomp,nbv)=1.0  ! set to opaque if not specified
          endif
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
          if(ND.ge.11)then  ! if enough items for opacity
            CALL EGETWR(LOUTSTR,K,VX,0.,1.,'W','vis opacity',IER)
            OPOV(icomp,nbv)=VX
          else
            OPOV(icomp,nbv)=1.0  ! set to opaque if not specified
          endif
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

C Collection of entities making up an object.
        if(foundit)then
          NBVOBJ(icomp)=NBVOBJ(icomp)+1
          nbvo=NBVOBJ(icomp)
          WORD='            ' 
          CALL EGETW(LOUTSTR,K,WORD,'W','vis obj name',IFLAG)

          write(VOBJNAME(icomp,nbvo),'(2a)') prec,WORD(1:11) ! adjust name
          LNVOBJNAME(icomp,nbvo)=lnblnk(VOBJNAME(icomp,nbvo))
          CALL EGETP(LOUTSTR,K,WORD,'W','vis obj desc',IFLAG)
          write(VOBJDESC(icomp,nbvo),'(a)') WORD(1:lnblnk(WORD))
          LNVOBJDESC(icomp,nbvo)=lnblnk(VOBJDESC(icomp,nbvo))
          CALL EGETWI(LOUTSTR,K,iv,1,12,'F','nb vis entities',IER)
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
        write(outs,'(3a)') 'Unknown tag ',WORD,' continuing...'
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
      
C For each of the mass vertices apply rotation.
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
          DO 100 I=1,nbvertmass
            XXX=vertmass(I,1)-XX1
            YYY=vertmass(I,2)-YY1
            XR=XXX*CA+YYY*SA
            YR=YYY*CA-XXX*SA
            vertmass(I,1)=XR+XX1
            vertmass(I,2)=YR+YY1
  100     continue
        endif

C Insert vertices associated with the mass into the zone.
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

C Apply the updated indices to the mass surface list.
        do ix=1,nbmass
          do loop=1,nbmassv(ix)
            masjvn(ix,loop)=masjvn(ix,loop)+nbprevnztv
          enddo
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
          SUSE(nsur,1)='PARTN'
          SUSE(nsur,2)='- '

C Set each pair to be back-to-back. Logic for up to 4.
          SOTHER(NSUR,1)='ANOTHER'
          if(ix.eq.1.or.ix.eq.3)then
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
