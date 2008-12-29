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

C This file contains the following routines:
C (related to vector visualization interface)

C VISOUT   define the parameters for the visualization of results.
C VISMAK   create images for specified views.
C FTV      control routine for flow and temperature visualization.
C GENVEC   Calculates velocity vectors at `P' grid points and prints results
C          in format for use in the flow visualization routines.
C GETVEC   Reads in the flow vector file produced by dfs.
C SETRES   Sets number of pixels based on resolution specified and geometry.
C rdFVP    Reads contents of flow-visualization parameters file.
C wrFVP    Writes contents of flow-visualization parameters file.
C ICOLOUR  Returns a colour based on given temperatures.

C ********************* VISOUT *********************
C VISOUT - define the parameters for the visualization of results.

      subroutine VISOUT(ICOMP)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      common/flvimg/imgtyp,IMOPTS
      common/flvpar/Lflpar(MCOM)
      common/flvnam/LBITrt(MCOM,6)
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax

      CHARACTER H*72,KEY*1,ITEM(35)*26,outs*124
      character LBITrt*12,t12*12,t24*24,Lflpar*72

C Check for visualidsation data.
      if (Lflpar(ICOMP)(1:7).ne.'UNKNOWN') call rdFVP(ICOMP,ier)

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this item, allowing editing.
C Setup for multi-page menu.
 3    MHEAD=2
      MCTL=7
      ILEN=Nview(ICOMP)
      INO=-3

C First entry in menu.
C imgtyp determines the format of the generated image:
C  imgtyp = 0 : 2d slice drawn on screen.
C  imgtyp = 1 : 2d slice in Xbitmap format to file.
C  imgtyp = 2 : 2d slice in Xpixmap format to file.
C  imgtyp = 3 : 3d image drawn on screen.
      if (imgtyp.eq.0) then
        ITEM(1)='a Image format >>screen2d'
      elseif (imgtyp.eq.1) then
        ITEM(1)='a Image format >> Xbitmap'
      elseif (imgtyp.eq.2) then
        ITEM(1)='a Image format >> XPM    '
      elseif (imgtyp.eq.3) then
        ITEM(1)='a Image format >>screen3d'
      endif
      ITEM(2)=' ----------------------- '

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 IM=1,ILEN
        M=M+1
        CALL EMKEY(M,KEY,IER)
        WRITE(ITEM(M),22) KEY,LBITrt(ICOMP,IM)
  22    FORMAT(A1,1X,A12)
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTL
      ITEM(M+1)=' ----------------------- '
      ITEM(M+2)='* add/ delete view '
      ITEM(M+3)='! list current '
      ITEM(M+4)='> save view information'
      ITEM(M+5)='= create image(s)   '
      ITEM(M+6)='? help     '
      ITEM(M+7)='- exit this menu  '
      INO=-4

C Instanciate h() strings for the menu.
 80   h(1) ='This facility is used to generate images of the'
      h(2) ='flow distribution predicted by the CFD calculations.'
      h(3) ='The flow at any 2-dimensional slice of the zone can'
      h(4) ='be viewed.'
      h(5) =' '
      h(6) ='Images are generated in various formats and can be'
      h(7) ='viewed and post-processed with 3rd-party tools such'
      h(8) ='as xv.'
      h(9) =' '
      h(10)='You must specify the name of the file to hold the'
      h(11)='image data and you must select a view prior to'
      h(12)='generating the image. The other items are for'
      h(13)='altering default settings, and their use is'
      h(14)='optional.'

      CALL EMENU('Flow visualization',ITEM,NITMS,INO)
      if (INO.EQ.0)THEN

C Try again.
        INO=-1
        GOTO 80
      elseif (INO.EQ.(NITMS))THEN
        return
      elseif (INO.EQ.(NITMS-1))THEN
        CALL PHELPD('flow vis facility',14,'-',0,0,IER)
      elseif (INO.EQ.(NITMS-2)) THEN

C Generate images.
        IFRAME=-1
        do 48 IM=1,Nview(ICOMP)
          ISOPT(IM)=-1
 48     continue
        call VISMAK(ICOMP,IFRAME)
      elseif (INO.EQ.(NITMS-3)) THEN

C Save imaging info.
        call wrFVP(ICOMP,ier)
      elseif (INO.EQ.(NITMS-4)) THEN

C List current.
        if (Nview(ICOMP).eq.0) then
          write (outs,'(a)') 'No view definitions to list yet !'
        else
          write (outs,'(a)') 'Current view definitions:'
          call edisp(iuout,outs)
          write (outs,'(a,a)') 'Image root |View  |Layer| Res  |',
     &            '     Arrow scaling factors           | Freq'
          call edisp(iuout,outs)
          write (outs,'(a,a)') ' name      |direct|     |      |',
     &            'Shaft len|Shaft thk|Head len|Head thk|'
          call edisp(iuout,outs)
          outs='  '
          do 100 I=1,Nview(ICOMP)
            write (outs(1:13),'(a)') LBITrt(ICOMP,I)

C Viewing direction.
            if (ISURvw(ICOMP,I).eq.1) then
              write (outs(15:21),'(a)') 'North '
            elseif (ISURvw(ICOMP,I).eq.2) then
              write (outs(15:21),'(a)') 'East  '
            elseif (ISURvw(ICOMP,I).eq.3) then
              write (outs(15:21),'(a)') 'South '
            elseif (ISURvw(ICOMP,I).eq.4) then
              write (outs(15:21),'(a)') 'West  '
            elseif (ISURvw(ICOMP,I).eq.5) then
              write (outs(15:21),'(a)') 'Top   '
            elseif (ISURvw(ICOMP,I).eq.6) then
              write (outs(15:21),'(a)') 'Bottom'
            endif

C Layer.
            write (outs(22:25),'(i3)') ILAYvw(ICOMP,I)

C Image quality.
            if (IRESvw(ICOMP,I).eq.1) then
              write (outs(26:32),'(a)') 'Low   '
            elseif (IRESvw(ICOMP,I).eq.2) then
              write (outs(26:32),'(a)') 'Medium'
            elseif (IRESvw(ICOMP,I).eq.3) then
              write (outs(26:32),'(a)') 'High  '
            endif

C Arrow style.
            write(outs(33:71),'(2(f6.1,4x),2(f5.1,4x))')VLSCvw(ICOMP,I),
     &              VTSCvw(ICOMP,I),HLSCvw(ICOMP,I),HTSCvw(ICOMP,I)

C Image frequency (dynamic flow viualisation only).
            write (outs(71:75),'(i4)') IFRQvw(ICOMP,I)
            call edisp(iuout,outs)
 100      continue
        endif
      elseif (INO.EQ.(NITMS-5)) THEN

C Add/ delete image definition.
        h(1) ='Define a new view, or delete a current view.'
        call EASKABC(' ','View definitions:',
     &                   'add another','delete one','continue',IW,1)
        if (IW.eq.1) then

C Add new image definition.
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
 101      h(1)='Please supply a root name for the image.  '
          h(2)='In the case of a dynamic viualisation the frame number'
          h(3)='will be appended to this name.'
          h(4)='Depending on the image format a .??? will be added to'
          h(5)='the name also e.g. for a GIF image .gif would be added.'
          t12=' new_view'
          CALL EASKS(t12,' ',' Name for image ? ',
     &                                12,'flowvis','image name',IER,5)
          if (t12(1:2).eq.'  ') goto 101
          LBITrt(ICOMP,NV)=t12
          h(1)='Please supply the direction from which the view is'
          h(2)='made e.g. Chose North if you wist a view from the North'
          h(3)='looking South.'
          CALL EASKATOG('View direction:',' ','South','East',
     &                    'North','West','Top','Bottom','  ',IWM,3)
          ISURvw(ICOMP,NV)=IWM
 102      IER=0
          IF(IWM.EQ.1)THEN

C View from north.
            CALL EASKI(ilayer,'  ',' Which Y layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.2)THEN

C View from east.
            CALL EASKI(ilayer,'  ',' Which X layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.3)THEN

C View from south.
            CALL EASKI(ilayer,'  ',' Which Y layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.4)THEN

C View from west.
            CALL EASKI(ilayer,'  ',' Which X layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.5)THEN

C View from top.
            CALL EASKI(ilayer,'  ',' Which Z layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          ELSEIF(IWM.EQ.6)THEN

C View from bottom.
            CALL EASKI(ilayer,'  ',' Which Z layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          endif
          ILAYvw(ICOMP,NV) = ilayer + 1

C Image resolution.
          H(1)='Higher resolution requires greater processing time'
          H(2)='and disk space. `Low` quality images are often'
          H(3)='sufficient, although `medium` and `high` quality'
          H(4)='may be desirable for inclusion in publications.'
          call EASKABC(' ','Image resolution?','Low','Medium','High',
     &                                             IWR,4)
          IRESvw(ICOMP,NV)=IWR

C Set defaults for customisation and frequency variables.
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4
          call usrmsg ('Setting defaults for arrow scaling factors and',
     &      'generation frequency. Edit via image pick. ','-')
          CALL EPWAIT
        elseif (IW.eq.2) then

C Remove an image definition.
          call usrmsg(' ','Facility not available. ','P')
        endif

C Image format.
      ELSEIF(INO.eq.1) then
        imgtyp=imgtyp+1
        if (imgtyp.gt.3) imgtyp=0

C Existing image selected.
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete or copy scene previously selected then new selection should 
C be deleted or copied, respectively.
        NV=INO-MHEAD
 200    write (outs,'(a,a,a)') 'For image definition ',
     &        LBITrt(ICOMP,NV)(1:lnblnk(LBITrt(ICOMP,NV))),' edit:'
        call EASKATOG(outs,' ','Name','View dir & layer','Resolution',
     &           'Customise arrows','Frequency','Continue','  ',IWE,3)
        if (IWE.eq.1) then

C Image root name.
 201      h(1)='Please supply a root name for the image.  '
          h(2)='In the case of a dynamic viualisation the frame number'
          h(3)='will be appended to this name.'
          h(4)='Depending on the image format a .??? will be added to'
          h(5)='the name also e.g. for a GIF image .gif will be added.'
          CALL EASKS(t12,' ',' Name for image ? ',
     &                                12,'flowvis','image name',IER,5)
          if (t12(1:2).eq.'  ') goto 201
          LBITrt(ICOMP,NV)=t12
        elseif (IWE.eq.2) then

C View direction and layer.
          h(1)='Please supply the direction from which the view is'
          h(2)='made e.g. Chose North if you wist a view from the North'
          h(3)='looking South.'
          CALL EASKATOG('View direction:',' ','South','East',
     &                    'North','West','Top','Bottom','  ',IWM,3)
          ISURvw(ICOMP,NV)=IWM
 202      IER=0
          IF(IWM.EQ.1)THEN

C View from north.
            CALL EASKI(ilayer,'  ',' Which Y layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.2)THEN

C View from east.
            CALL EASKI(ilayer,'  ',' Which X layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.3)THEN

C View from south.
            CALL EASKI(ilayer,'  ',' Which Y layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.4)THEN

C View from west.
            CALL EASKI(ilayer,'  ',' Which X layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.5)THEN

C View from top.
            CALL EASKI(ilayer,'  ',' Which Z layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          ELSEIF(IWM.EQ.6)THEN

C View from bottom.
            CALL EASKI(ilayer,'  ',' Which Z layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,4)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          endif
          ILAYvw(ICOMP,NV) = ilayer + 1

C Image resolution.
        elseif (IWE.eq.3) then
          H(1)='Higher resolution requires greater processing time'
          H(2)='and disk space. `Low` quality images are often'
          H(3)='sufficient, although `medium` and `high` quality'
          H(4)='may be desirable for inclusion in publications.'
          call EASKABC(' ','Image resolution?','Low','Medium','High',
     &                                             IWR,4)
          IRESvw(ICOMP,NV)=IWR
        elseif (IWE.eq.4) then

C Customise arrows.
 203      write (t24,'(6f6.1)') VLSCvw(ICOMP,NV),VTSCvw(ICOMP,NV),
     &                          HLSCvw(ICOMP,NV),HTSCvw(ICOMP,NV)
          H(1)='By altering these settings, you can control'
          H(2)='the appearance of the flow vectors.'
          call EASKS(t24,
     &      'Scale factors: Shaft len; Shaft thK; Head len; Head thk',
     &                  '  ',24,' 1.0 1.0 1.0 1.0','scale factrs',IER,2)
          call CHITMS(t24,ISC)
          if (ISC.ne.4) goto 203
          K=0
          call EGETWR(t24,K,SIL,0.,0.,'-','shaft length',IER)
          VLSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','shaft thickness',IER)
          VTSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','head length',IER)
          HLSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','head thickness',IER)
          HTSCvw(ICOMP,NV)=SIL
        elseif (IWE.eq.5) then

C Frequency of images.
          H(1)='For a conflated simulation images can be created at'
          H(2)='the required frequency.'
          CALL EASKI(IFR,' ',' Number of timesteps between images?',
     &      1,'F',1,'-',4,'image freq',IER,2)
          IFRQvw(ICOMP,NV)=IFR
        elseif (IWE.eq.6) then
          goto 3
        endif
        goto 200
      endif

      goto 3
      end

C ********************* VISMAK *********************
C VISMAK - create images for specified views and frames in an animation,
C  IFRAME=-1 if cfd stand alone.

      subroutine VISMAK(ICOMP,IFRAME)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      common/flvimg/imgtyp,IMOPTS
      common/flvnam/LBITrt(MCOM,6)
      COMMON/VISFIL/LVEC,LBIT
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)

      character LBITrt*12,outs*124,TLBIT*16
      character name*12,h*72,t12*12,LBIT*72,LVEC*72
      logical dok,GENVEW

C Any views.
      if (Nview(ICOMP).eq.0) then
        call edisp(iuout,' No images to create!')
        return
      endif

C For each view generate image.
      do 10 IM=1,Nview(ICOMP)
        GENVEW=.true.
        if (imgtyp.eq.0.and.Nview(ICOMP).gt.1) then
          write (outs,'(3a,i1,a)') 'View image ',
     &        LBITrt(ICOMP,IM)(1:lnblnk(LBITrt(ICOMP,IM))),' (',IM,') ?'
          dok=.true.
          h(1)='You have more than one view defined. '
          h(2)='Do you want to view this one? '
          call ASKOK('  ',outs,GENVEW,dok,2)
        elseif (imgtyp.eq.3.and.Nview(ICOMP).gt.1) then
          write (outs,'(a,f5.2,a,a,a,i1,a)') 
     &        'Use vector length scale factor (',
     &          VLSCvw(ICOMP,IM),') from view ',
     &        LBITrt(ICOMP,IM)(1:lnblnk(LBITrt(ICOMP,IM))),' (',IM,') ?'
          dok=.true.
          h(1)='You have more than one view defined. Do you '
          h(2)='want to use the scale parameters from this one? '
          call ASKOK(
     &    'The 3d image uses the scale factors from the listed views.',
     &     outs,GENVEW,dok,1)
        endif

C Generate name of image depending on type and allow 3 digits for
C the frame number.
        if (GENVEW) then
          write (t12,'(a,i3.3)') LBITrt(ICOMP,IM)(1:9),IFRAME
          call st2name(t12,name)
          if (imgtyp.eq.1) then
            write (TLBIT,'(a,a4)') name(1:lnblnk(name)),'.bmp'
            write (outs,'(2a)') 'Creating bmp image ',TLBIT(1:16)
          elseif (imgtyp.eq.2) then
            write (TLBIT,'(a,a4)') name(1:lnblnk(name)),'.xpm'
            write (outs,'(2a)') 'Creating xpm image ',TLBIT(1:16)
          endif
          call edisp(iuout,outs)
          write(LBIT,'(a)') TLBIT(1:16)

C Set common data (and parameters passed directly).
          ISUR=ISURvw(ICOMP,IM)
          ilayer=ILAYvw(ICOMP,IM)
          imgqua=IRESvw(ICOMP,IM)
          vLscal=VLSCvw(ICOMP,IM)
          tscal=VTSCvw(ICOMP,IM)
          hLscal=HLSCvw(ICOMP,IM)
          hWscal=HTSCvw(ICOMP,IM)

C Create image.
          call FTV(ICOMP,IM,ISUR,ilayer,name)
        endif
 10   continue

      return
      end


C ********************* FTV *********************
C FTV - control routine for flow and temperature visualization.
C ISUR    Viewing direction: 3=north, 2=east, 1=south, 4=west, 5=top, 6=bottom.
C ilayer  Layer of CFD cells to view.
C imgtyp  Output image type.

      subroutine FTV(ICOMP,IM,ISUR,ilayer,name)
#include "building.h"
#include "cfd.h"
      
      integer lnblnk  ! function definition

      COMMON/ICFNOD/ICFD,ICP
      common/pophelp/h(60)
      common/flvnam/LBITrt(MCOM,6)
      common/flvimg/imgtyp,IMOPTS
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
      LOGICAL CALPOL,CALLMA
      character name*12,BITITL*5,LBITrt*12,H*72,outs*124,t24*24
      CHARACTER POLNAM*12,ITEM(MCTM+2)*18

C Set the geometry for the CFD domain in terms of ESP-r's standard
C geometry model.
      CALL SETGEO

C Set the number of pixels required for the image.  This is a function
C of the resolution specified by the user and the geometry of the model.
      CALL SETRES(ISUR)

C Translate and rotate the site co-ordinate system to a face co-ordinate
C system for the viewing surface.
      CALL TRNSF1(ISUR)

C Translate and rotate the start and end points of each flow vector to the face
C co-ordinate system of the face under consideration.
      CALL TRNSF2

C Image type dependent code.
      if (imgtyp.eq.0) then

C Default to screen.
        call MAKS2d(ISUR,ilayer,name)

      elseif (imgtyp.eq.1.or.imgtyp.eq.2) then 
C Grid based image types: XBM and XPM
C Superimipose a mesh on the viewing surface.  This will be used for point
C containment tests on the flow vectors.
C NOX and NOZ are number of mesh points in the x and z directions, which
C correspond to the resolution defined by the user.
C NOX and NOZ must be whole-number multiples of 8.
        CALL MESH(ISUR)

C Ask for scalar to asign to colour.
        ISCALAR=-1
        if (imgtyp.eq.2) then
          if (ISOPT(IM).eq.-1) then
            H(1)='The colour of the vector will depend on the chosen'
            H(2)='metric (variable).'
            write (outs,'(4a,i1,a)') 'Vector colour represents which ',
     &         'metric for view ',
     &         LBITrt(ICOMP,IM)(1:lnblnk(LBITrt(ICOMP,IM))),' (',IM,')?'
            CALL EASKATOG(outs,' ','Air speed','Temperature',
     &                 'Local mean age of air','Concentration',
     &                 ' ',' ',' ',ISCALAR,2)

C Write title for pixmap scale.
            ISCALAR=ISCALAR-1
            call EASKAB('  ','Apply colour to:','Vector','Background',
     &                 IBR,0)

C Ask which contaminant to plot
            IF(ISCALAR.eq.3)THEN
              IF(NCTM.LE.0)GOTO 332
              DO 333 ICTM=1,NCTM
                WRITE(ITEM(ICTM),'(A,1X,A)')
     &               CHAR(96+ICTM),POLNAM(ICTM,ICFD)
 333          CONTINUE
              ITEM(NCTM+1)='--------------    '
              ITEM(NCTM+2)='- exit            '
              NITMS=NCTM+2
              INO=-1

C Help text for the menu.
 331          h(1)='The contaminant selection menu does not yet have'
              h(2)='any help associated with it. '
              CALL EMENU('Choose Contaminant',ITEM,NITMS,INO)
              IF(INO.EQ.NITMS)RETURN
              IF(INO.GT.NITMS.OR.INO.LT.1.OR.INO.EQ.(NCTM+1))GOTO 331
            ENDIF

C Remember selections (used by res for animations).
 332        ISOPT(IM)=ISCALAR
            IBGOPT(IM)=IBR
            
C Set scale (should scan res lib in future)
            t24='30.0 0.0'
 203        call EASKS(t24,'  ',
     &         'max and min values:',24,'30.0 0.0','scale',IER,0)
            call CHITMS(t24,ISC)
            if (ISC.ne.2) goto 203
            K=0
            call EGETWR(t24,K,SCmax,0.,0.,'-','maximum',IER)
            call EGETWR(t24,K,SCmin,0.,0.,'-','minimum',IER)
            if (SCmax.lt.SCmin) goto 203
          else
            ISCALAR=ISOPT(IM)
            IBR=IBGOPT(IM)
          endif
          if (ISCALAR.eq.0) then
            BITITL='SPEED'
          elseif (ISCALAR.eq.1) then
            BITITL='TEMP '
          elseif (ISCALAR.eq.2) then
            BITITL='LMA  '
          elseif (ISCALAR.eq.3) then
            BITITL=POLNAM(INO,ICFD)(1:5)
          else
            BITITL='     '
          endif
        endif

C Remember which contaminant to plot
        IF(ISCALAR.EQ.3)ISCALAR=ISCALAR+INO

C Examine each mesh point to determine whether it is covered by
C a flow vector.  
        CALL PbyP(ICOMP,IM,ISUR,ilayer,ISCALAR,IBR)
        IF(ISCALAR.GT.3)ISCALAR=ISCALAR-INO

C Create a X-bitmap file. 
        if (imgtyp.eq.1) then
          CALL MAKBIT
        elseif (imgtyp.eq.2) then
          CALL MAKXPM(SCmin,SCmax)
        endif
      elseif (imgtyp.eq.3) then 

C Create a screen3d image
        call MAKS3d
      endif

      return
      end

C ********************* GENVEC *********************
C GENVEC - Calculates velocity vectors at `P' grid points and prints 
C  results in format for use in the flow visualization routines.
      SUBROUTINE GENVEC(vecfil)
#include "cfd.h"

      COMMON/FILEP/IFIL
      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      common/veccol/IVRED(ntcelx,ntcely,ntcelz),
     &              IVGRN(ntcelx,ntcely,ntcelz),
     &              IVBLU(ntcelx,ntcely,ntcelz)

      character vecfil*72,lfil*72
      logical wrtvec,veclib,XST

C Should vectors be written to file?
      wrtvec=.FALSE.
      veclib=.FALSE.
      if (vecfil(1:7).ne.'UNKNOWN') wrtvec=.TRUE.
      if (vecfil(1:8).eq.'TIMESTEP') then
        veclib=.TRUE.
        wrtvec=.FALSE.
      endif

      if (veclib) then
        XST=.false.
        lfil='test_vec.lib'
        call FINDFIL(lfil,XST)
        if (.not.XST) then
          call efopseq(ICFLIB,lfil,4,ier)
          write(ICFLIB,'(3i4)') NIM1-1, NJM1-1, NKM1-1
        endif
      endif

C Open file and write header.
      if (wrtvec) then
        IUNIT=IFIL+1
        call efopseq(IUNIT,vecfil,4,ier)
        write(IUNIT,'(a)') '# dfs flow-vectors'
        write(IUNIT,'(a)') '# Flow vectors for each CFD grid point'
        write(IUNIT,'(a,a)') '# i, j, k : X start & end, Y start &',
     &                                  ' end, Z start & end, colour'
      endif

C Calculate the i, j, and k components of the velocity vectors at each
C `P' point.  This requires interpolation, as the CFD solution results
C in velocities at the `U', `V', and `W' points.
C The `P' points are located mid-distance between the `U', `V', and `W' points,
C so the velocities at the `P' points are calculated by averaging velocities
C at neighbouring points.
      do 10 I=2,NIM1
        do 101 J=2,NJM1
          do 102 K=2,NKM1
            UatP = 0.5*(Uf(i,j,k)+Uf(i+1,j,k))
            VatP = 0.5*(Vf(i,j,k)+Vf(i,j+1,k))
            WatP = 0.5*(Wf(i,j,k)+Wf(i,j,k+1))
            if (veclib) then
              write (ICFLIB,*) UatP,VatP,WatP,Tf(i,j,k)
            endif

C Create a flow vector for each `P' point, centred on the `P' point.
C The start and end points of each flow vector are given in units of
C metres and measured from the origin of the CFD grid.
C The start and end points of the vector are given in X,Y,Z co-ordinates,
C so there are six values associated with each vector.
C The six components of each flow vector are written on a single line
C of the flow-vector output file.
C Note that the origin is located at the bottom southwest corner
C of the CFD domain.
C The length of each flow vectors is equal to the distance the flow
C travels in one second.  These length of the vectors will be altered
C by the user during the display of the flow.
            VECXbeg(i,j,k) = XP(i) - 0.5*UatP
            VECXend(i,j,k) = XP(i) + 0.5*UatP
            VECYbeg(i,j,k) = YP(j) - 0.5*VatP
            VECYend(i,j,k) = YP(j) + 0.5*VatP
            VECZbeg(i,j,k) = ZP(k) - 0.5*WatP
            VECZend(i,j,k) = ZP(k) + 0.5*WatP

C Calculate colour of vector (prop to temperature)
            call ICOLOUR(25.0,15.0,Tf(i,j,k),IR,IG,IB)
            IVRED(i,j,k)=IR
            IVGRN(i,j,k)=IG
            IVBLU(i,j,k)=IB
            if (wrtvec) write(IUNIT,1011) i,j,k,vecXbeg(i,j,k),
     &                    vecXend(i,j,k),vecYbeg(i,j,k),vecYend(i,j,k),
     &                    vecZbeg(i,j,k),vecZend(i,j,k),IVRED(i,j,k),
     &                    IVGRN(i,j,k),IVBLU(i,j,k)
 102      continue
 101    continue
 10   continue

 1011 format(3(i3,1x),' : ',6(E13.6,2x),3i4)

C Remember to close file.
      if (wrtvec) CALL ERPFREE(IUNIT,ISTAT)
      if (veclib) then
        write (ICFLIB,'(a)') '# ---------- next time step ----------'
      endif

      return
      end


C ********************* GETVECLIB *********************
C GETVECLIB - Calculates velocity vectors at `P' grid points and prints 
C  results in format for use in the flow visualization routines.
      SUBROUTINE GETVECLIB(vecfil)
#include "cfd.h"

      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      common/veccol/IVRED(ntcelx,ntcely,ntcelz),
     &              IVGRN(ntcelx,ntcely,ntcelz),
     &              IVBLU(ntcelx,ntcely,ntcelz)

      character vecfil*72,OUTSTR*124,t24*24

C Ask for temperature scale max and min
        t24='  25.0  15.0'
        CALL EASKS(t24,
     &          'Maximum and minium temperature for colour scale?',
     &                    '  ',24,'25.0  15.0 ','temp max/min',IER,0)
        K=0
        CALL EGETWR(t24,K,VAL,0.,0.,'-','Max T',IER)
        Tmax = VAL
        CALL EGETWR(t24,K,VAL,0.,Tmax,'W','Max T',IER)
        Tmin = VAL

C Open library file.
      ier=0
      call efopseq(ICFLIB,vecfil,1,ier)
      if (ier.ne.0)  then
        call usrmsg(' Error in reading vector file:',vecfil,'W')
        return
      endif

C Read ?atP (velocity) data and temperature data.
      do 10 I=2,NIM1
        do 101 J=2,NJM1
          do 102 K=2,NKM1
            Kp=0
            call STRIPC(ICFLIB,OUTSTR,0,ITEMS,0,'cell data',IER)
            CALL EGETWR(OUTSTR,Kp,UatP,0.,0.,'-','UatP read',IER)
            CALL EGETWR(OUTSTR,Kp,VatP,0.,0.,'-','VatP read',IER)
            CALL EGETWR(OUTSTR,Kp,WatP,0.,0.,'-','WatP read',IER)
            CALL EGETWR(OUTSTR,Kp,VAL,0.,0.,'-','Tf read',IER)
            Tf(i,j,k) = VAL

C Create a flow vector for each `P' point, centred on the `P' point.
C The start and end points of each flow vector are given in units of
C metres and measured from the origin of the CFD grid.
C The start and end points of the vector are given in X,Y,Z co-ordinates,
C so there are six values associated with each vector.
C The six components of each flow vector are written on a single line
C of the flow-vector output file.
C Note that the origin is located at the bottom southwest corner
C of the CFD domain.
C The length of each flow vectors is equal to the distance the flow
C travels in one second.  These length of the vectors will be altered
C by the user during the display of the flow.
            VECXbeg(i,j,k) = XP(i) - 0.5*UatP
            VECXend(i,j,k) = XP(i) + 0.5*UatP
            VECYbeg(i,j,k) = YP(j) - 0.5*VatP
            VECYend(i,j,k) = YP(j) + 0.5*VatP
            VECZbeg(i,j,k) = ZP(k) - 0.5*WatP
            VECZend(i,j,k) = ZP(k) + 0.5*WatP

C Calculate colour of vector (prop to temperature)
            call ICOLOUR(Tmax,Tmin,Tf(i,j,k),IR,IG,IB)
            IVRED(i,j,k)=IR
            IVGRN(i,j,k)=IG
            IVBLU(i,j,k)=IB
 102      continue
 101    continue
 10   continue

C Remember to close file.
      CALL ERPFREE(ICFLIB,ISTAT)

      return
      end

C ********************* GETVEC *********************
C GETVEC - reads in the flow vector file produced by dfs.
      SUBROUTINE GETVEC(vecfil)
#include "cfd.h"

      common/FILEP/IFIL

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      common/veccol/IVRED(ntcelx,ntcely,ntcelz),
     &              IVGRN(ntcelx,ntcely,ntcelz),
     &              IVBLU(ntcelx,ntcely,ntcelz)

      character vecfil*72,OUTSTR*124

C Open file.
      IUNIT=IFIL+1
      ier=0
      call efopseq(IUNIT,vecfil,1,ier)
      if (ier.ne.0)  then
        call usrmsg(' Error in reading vector file:',vecfil,'W')
        return
      endif

C Read in the flow vectors.
C The start and end points of each flow vector are given in units of
C metres and measured from the origin of the CFD grid.
C The start and end points of the vector are given in X,Y,Z co-ordinates,
C so there are six values associated with each vector.
C Note that the origin is located at the bottom southwest corner
C of the CFD domain.
C The length of each flow vectors is equal to the distance the flow
C travels in one second.  These length of the vectors will be altered
C by the user during the display of the flow.
      do 20 I=2,NIM1
        do 201 J=2,NJM1
          do 202 K=2,NKM1
            call STRIPC(IUNIT,OUTSTR,0,ITEMS,0,'vector data',IER)
            KDI=15
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecXbeg',IER)
            vecXbeg(i,j,k)=VAL
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecXend',IER)
            vecXend(i,j,k)=VAL
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecYbeg',IER)
            vecYbeg(i,j,k)=VAL
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecYend',IER)
            vecYend(i,j,k)=VAL
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecZbeg',IER)
            vecZbeg(i,j,k)=VAL
            CALL EGETWR(OUTSTR,KDI,VAL,0.,0.,'-','vecZend',IER)
            vecZend(i,j,k)=VAL
            CALL EGETWI(OUTSTR,KDI,IVAL,0,255,'W','red frac',IER)
            IVRED(i,j,k)=IVAL
            CALL EGETWI(OUTSTR,KDI,IVAL,0,255,'W','grn frac',IER)
            IVGRN(i,j,k)=IVAL
            CALL EGETWI(OUTSTR,KDI,IVAL,0,255,'W','blu frac',IER)
            IVBLU(i,j,k)=IVAL
 202      continue
 201    continue
 20   continue

      return
      end

C **************** SETRES
C SETRES establishes the number of pixels required in the bitmap, in the X and Z
C        directions. The number of pixels depends on the resolution specified by
C        the user and the geometry of the zone.

      SUBROUTINE SETRES(ISUR)
#include "cfd.h"
#include "flowvis.h"

      COMMON/VISSET/imgqua,NOX,NOZ,tscal,hLscal,hWscal,vLscal
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      common/flvimg/imgtyp,IMOPTS

C Xmax,Ymax,Zmax are the dimensions of the model in the i, j, and k directions.
      Xmax = XU(NI)
      Ymax = YV(NJ)
      Zmax = ZW(NK)

C Calculate the aspect ratio, the horizontal dimension of the image divided
C by the vertical dimension. This depends on the viewing direction and the 
C geometry of the model.
C South or north:
      if(ISUR.eq.1 .or. ISUR.eq.3) aspec=Xmax/Zmax

C East or west:
      if(ISUR.eq.2 .or. ISUR.eq.4) aspec=Ymax/Zmax

C Top or bottom:
      if(ISUR.eq.5 .or. ISUR.eq.6) aspec=Xmax/Ymax

C Set the number of pixels required in the horizontal direction according to the
C user's selection.
C `high' quality:
 10   if(imgqua.eq.3) then
        NOX = MGRXZ-60

C `medium' quality:
      elseif(imgqua.eq.2) then
        NOX = (MGRXZ-60)*2/3

C `low' quality:
      else
        NOX = (MGRXZ-60)*4/9
      endif

C NOX should be an integer multiple of 8.
      NOX = (NOX/8) * 8

C Calculate the number of pixels required in the vertical direction.
      NOZ = int(NOX/aspec)

C Swap the pixel in the horizontal and vertical directions, if the image is taller
C than it is wide. NOX should be an integer multiple of 8.
      if(NOZ.gt.NOX) then
        NOZ = NOX
        NOX = (int(NOZ*aspec)/8) * 8
      endif

C If the arrays cannot accommodate this many pixels, give warning and reduce
C to a lower resolution.
C Check only needed for bitmaps.
      if (imgtyp.eq.1.or.imgtyp.eq.2) then
        if(NOX.gt.MGRXZ .or. NOZ.gt.MGRXZ) then
          CALL EDISP(IUOUT,
     &             ' Modify flowvis.h to accommodate this resolution.')
          CALL EDISP(IUOUT,' Reducing resolution.')
          imgqua = imgqua-1
          goto 10
        endif
      endif

      return
      end


C **************** rdFVP
C rdFVP reads in the contents of the flow-visualization parameters file, which
C       contains specifications on bitmap images to generate during the progress
C       of bps-dfs conflated simulations.

      SUBROUTINE rdFVP(ICOMP,ier)
#include "cfd.h"
#include "building.h"

      common/FILEP/IFIL

      common/flvpar/Lflpar(MCOM)
      common/flvimg/imgtyp,IMOPTS
      common/flvnam/LBITrt(MCOM,6)
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)

      character*124 outs,OUTSTR,WORD
      character Lflpar*72
      character LBITrt*12

C Set default image type to screen 2d.
      imgtyp=0

C Open flowvis parameters file.
      ier=0
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,Lflpar(ICOMP),1,IER)
      IF(ier.ne.0) goto 1000

C Read the number of views for which images will be generated.
      CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,0,'image data',IER)
      K=0
      CALL EGETWI(OUTSTR,K,N,1,6,'F','number of images',IER)
      Nview(icomp) = N
      CALL EGETWI(OUTSTR,K,N,0,3,'W','image type',IER)
      imgtyp = N

C Iterate through each view.
      DO 10 iview=1,Nview(icomp)
        CALL STRIPC(IUNIT,OUTSTR,9,ITEMS,1,'flow vis data',IER)
        if(IER.ne.0) goto 1000
        K=0

C Root of file name for bitmaps.
        CALL EGETW(OUTSTR,K,WORD,'W','image file root name',IER)
        LBITrt(ICOMP,iview) = WORD(1:12)

C Viewing direction.
        CALL EGETWI(OUTSTR,K,IVAL,1,6,'F','view direction',IER)
        ISURvw(ICOMP,iview) = IVAL

C Cell layer (slice) to view.
        CALL EGETWI(OUTSTR,K,IVAL,1,MCEL1D,'F','slice',IER)
        ILAYvw(ICOMP,iview) = IVAL

C Image resolution.
        CALL EGETWI(OUTSTR,K,IVAL,1,3,'F','resolution',IER)
        IRESvw(ICOMP,iview) = IVAL

C Shaft length scaling factor.
        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F','arrow length',IER)
        VLSCvw(ICOMP,iview) = VAL

C Shaft thickness scaling factor.
        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F','arrow thick',IER)
        VTSCvw(ICOMP,iview) = VAL

C Head length scaling factor.
        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F','head length',IER)
        HLSCvw(ICOMP,iview) = VAL

C Head thickness scaling factor.
        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F','head thick',IER)
        HTSCvw(ICOMP,iview) = VAL

C Frequency of image generation, expressed as integer multiple of
C simulation time step.
        CALL EGETWI(OUTSTR,K,IVAL,1,1000,'F','frequency',IER)
        IFRQvw(ICOMP,iview) = IVAL

   10 CONTINUE

C Read was successful.
      goto 2000

C Error reading flowvis parameters file.
 1000 write(outs,'(a,i3,a)') 'Zone',ICOMP,
     &                              ' flowvis parameters file error.'
      call USRMSG(' ',outs,'W')
      ier=1

C Close flowvis parameters file and return.
 2000 CALL ERPFREE(IUNIT,ISTAT)
      return
      end

C **************** wrFVP
C wrFVP writes the contents of the flow-visualization parameters file, 
C   which contains specifications on bitmap images to generate.

      SUBROUTINE wrFVP(ICOMP,ier)
#include "building.h"

      common/FILEP/IFIL

      common/flvpar/Lflpar(MCOM)
      common/flvimg/imgtyp,IMOPTS
      common/flvnam/LBITrt(MCOM,6)
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)

      character Lflpar*72,t72*72,LBITrt*12,outs*124

C Check that we have a file name.
      if (Lflpar(ICOMP)(1:7).eq.'UNKNOWN'.or.
     &    Lflpar(ICOMP)(1:2).eq.' ') then
        t72='CFDvis.par'
 1      call EASKS(t72,'File name for flow visualization parameters ?',
     &                  '  ',72,' CFDvis.par','vis par file',IER,0)
        if (IER.ne.0) goto 1
        Lflpar(ICOMP)=t72
      endif

C Open flowvis parameters file.
      IUNIT=IFIL+1
      t72=Lflpar(ICOMP)
      IER=0
 5    CALL EFOPSEQ(IUNIT,t72,4,IER)
      IF(ier.ne.0) goto 1000
      if (t72(1:2).eq.'  ') goto 5
      Lflpar(ICOMP)=t72

C Write the number of views for which images will be generated.
      write (IUNIT,'(2I3,a)') Nview(icomp),imgtyp,
     &                               ' # No of views and image format'

C Documentation.
      write (IUNIT,'(a,a)') '#Image root|View|Layer|Res|',
     &              '        Arrow scaling factors        |Freq'
      write (IUNIT,'(a,a)') '# name     |dir |     |   |',
     &              'Shaft len|Shaft thk|Head len|Head thk|(trns only)'

C Iterate through each view.
      DO 10 iview=1,Nview(icomp)
        write (IUNIT,'(a12,3i5,4f10.3,i4)') LBITrt(ICOMP,iview),
     &     ISURvw(ICOMP,iview),ILAYvw(ICOMP,iview),IRESvw(ICOMP,iview),
     &     VLSCvw(ICOMP,iview),VTSCvw(ICOMP,iview),HLSCvw(ICOMP,iview),
     &     HTSCvw(ICOMP,iview),IFRQvw(ICOMP,iview)
 10   CONTINUE

C Write was successful.
      goto 2000

C Error writing flowvis parameters file.
 1000 write(outs,'(a,i3,a)') 'Zone',ICOMP,
     &                              ' flowvis parameters file error.'
      call USRMSG(' ',outs,'W')
      ier=1

C Close flowvis parameters file and return.
 2000 CALL ERPFREE(IUNIT,ISTAT)
      return
      end

C ********************* ICOLOUR *********************
C ICOLOUR - determine colour scale point.
C Calculates colour as a hue first (assuming a saturation and 
C value of 1.) This HSV format is then translated into RGB fromat
C with 0,0,0 being black and 255,255,255 being white.
      SUBROUTINE ICOLOUR(TMAX,TMIN,TCHK,IRED,IGRN,IBLU)

C Copy temperature to local variable.
      T=TCHK

C First check that there is a range of temperatures.
      if (ABS(TMAX-TMIN).lt.0.001) then
        IRED=0
        IGRN=0
        IBLU=0
        return
      endif

C Colour scale is linear in hue.  Therefore work out point on colour 
C scale then translate to RGB value.
C Assume that saturation and value are 1.0
      if (T.lt.TMIN) T=TMIN
      if (T.gt.TMAX) T=TMAX
      h=240.0-(((T-TMIN)/(TMAX-TMIN))*240.0)
      s=1.
      v=1.

      if (abs(h+1.0).lt.0.0001.or.abs(s).lt.0.0001) then
        r=v
        g=v
        b=v
      else
        if (h.gt.360.0) h=h-360.
        h=h/60.0
        j=int(h)

C Check for neg value of j.
        if (j.lt.0) j=0
        f=h-float(j)
        p=v*(1.-s)
        q=v*(1.-(s*f))
        t=v*(1.-(s*(1.-f)))
        if (j.eq.0) then
          r=v
          g=t
          b=p
        elseif (j.eq.1) then
          r=q
          g=v
          b=p
        elseif (j.eq.2) then
          r=p
          g=v
          b=t
        elseif (j.eq.3) then
          r=p
          g=q
          b=v
        elseif (j.eq.4) then
          r=t
          g=p
          b=v
        elseif (j.eq.5) then
          r=v
          g=p
          b=q
        else
          r=v
          g=t
          b=p
        endif
      endif

      ired= int(r*255. + 0.5)
      igrn= int(g*255. + 0.5)
      iblu= int(b*255. + 0.5)
      RETURN
      END
