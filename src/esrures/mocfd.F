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

C mocfd.F contains the following:

C ******************** MOCFD ********************
C MOCFD 

      SUBROUTINE MOCFD
      INCLUDE 'cfd.h'

      integer igraphiclib  ! external definition

      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX

      character LTMP*72,H*72, POLNAM*12
      character path*72,LCMDFL*144,ltmpc*144
      logical XST,CALPOL,CALLMA

C Ask for CFD results library.
      H(1)='A CFD results library is created by the simulator. It will'
      H(2)='contain the time series results of one or more CFD'
      H(3)='simulations. '
      H(4)='A file called UNKNOWN will allow return to main menu.'
      LTMP='UNKNOWN'
      ltmpc='UNKNOWN'
      llt=lnblnk(ltmpc)

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(ltmpc,'CFD library name?',' ',96,LTMP,
     &      'CFDresfl name',IER,4)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(ltmpc,'CFD library name?',' ',124,LTMP,
     &      'CFDresfl name',IER,4)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(ltmpc,'CFD library name?',' ',144,LTMP,
     &      'CFDresfl name',IER,4)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(ltmpc,'CFD library name?',' ',144,LTMP,
     &    'CFDresfl name',IER,4)
      else
        CALL EASKF(ltmpc,'CFD library name?',' ',96,LTMP,
     &    'CFDresfl name',IER,4)
      endif

      if(ltmpc(1:2).ne.'  '.and.ltmpc(1:4).ne.'UNKN')then
        LCMDFL=ltmpc
      else
        return
      endif
      IER=0

C Find the path and local file name.
C      write(6,*) 'MOCFD: current path is ',path
      iunit=IFIL+14
      call fdroot(LCMDFL,path,LTMP)
      CALL ERPFREE(iunit,ISTAT)
      call FINDFIL(LTMP,XST)
      IF(XST)THEN

C Check that its a CFD results library.
        call EFOPRAN(iunit,LTMP,MCEL1D,1,IER)
        if(ier.eq.0) then
          irec=1
          read(iunit,rec=irec,iostat=istat) 
     &      IRLEN,ICFDSV,(IEQSV(J),J=1,5+NCTM)
C          write (6,*) IRLEN,ICFDSV,(IEQSV(J),J=1,5+NCTM)
          if(IRLEN.ne.MCEL1D) return
        endif
      ELSE
        call usrmsg(' Could not find: ',LTMP,'W')
        return
      ENDIF

C Read header data and set common block data.
      call RCFDLIBH(IER)
      if (IER.ne.0) return

C Display menu options << should be icomp?? >>.
      IX=1
      call MCFDM(IX)

      return
      end

C ******************** RCFDLIBH ********************
C RCFDLIBH - read CFD library 

      SUBROUTINE RCFDLIBH(IER)
      INCLUDE 'building.h'
      INCLUDE 'cfd.h'

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/c1/ncomp,ncon
      COMMON/ICFNOD/ICFD,ICP

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      
C NCDOM - number of CFD domains
C ICDOM - CFD domain selected for output
C ICFDZ - thermal zone associated with each CFD domain
      COMMON/cfddoms/NCDOM,ICDOM,ICFDZ(MNZ)

      LOGICAL CALPOL,CALLMA
      CHARACTER LCFD*72,POLNAM*12

C Set unit number.
      IER=0
      iunit=IFIL+14

C Read records 2 and 3 (active CFD domains/ simulation period).
      IREC=2
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (IFCFD(I),I=1,NCOMP)
C      write (6,*) (IFCFD(I),I=1,NCOMP)
      IREC=3
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICDYS,ICDYF,CFTS,CFTF
C      write (6,*) IREC,ICDYS,ICDYF,CFTS,CFTF
      call CFDPER(1)

C Read number of Contaminants and their names (assume zone 1)
      ICF=1
      IREC=4
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) NCTM

C Read which variables are saved
      IREC=1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)II,II,
     &    (IEQSV(I),I=1,5+NCTM)
      IREC=4

      DO 33 ICTM=1,NCTM
        IREC=IREC+1
        read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) POLNAM(ICTM,ICF)
 33   CONTINUE
      
C Work out which thermal zone the CFD zone is conflated to.
C If more than one then ask which one to use.
      ICDOM=0
      NCDOM=0
      NCFDSZ=0
      do 10 IC=1,NCOMP
        if (IFCFD(IC).ne.0) then
          NCDOM=NCDOM+1
          ICFDZ(NCDOM)=IC
          ICFD=ICFDZ(NCDOM)

C Read domain size.
          IREC=5+NCTM+(4*(IC-1))
          read (iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) 
     &                                           IDRSZE,NIM2,NJM2,NKM2
C          write (6,*) IREC,IDRSZE,NIM2,NJM2,NKM2

C NRCFDOM is the number of records for each CFD domain.
          NRCFDOM(NCDOM)=IDRSZE

C NCFDSZ is the number of records for each timestep (all CFD domains).
          NCFDSZ=NCFDSZ+IDRSZE
        endif
 10   continue

C Set up contaminants in correct CFD domain
      DO 1234 ICTM=1,NCTM
        POLNAM(ICTM,ICFDZ(NCDOM))=POLNAM(ICTM,ICF)
 1234 CONTINUE
      if (NCDOM.eq.0) then
        write (6,*) 'No CFD zones detected'
      elseif (NCDOM.eq.1) then
        ICDOM=1
      else
 99     IER=0
        CALL EASKI(I,' There is more than 1 CFD domain.',
     &    ' Output results for which domain? ',
     &    1,'F',NCDOM,'F',1,' CFD domain',IER,0)
        if (IER.ne.0) goto 99
        ICDOM=I
      endif

C Read domain size of selected domain.
      IREC=5+(4*(ICFDZ(ICDOM)-1))+NCTM
      read (iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) IDRSZE,NIM2,NJM2,NKM2
C      write (6,*) IREC,IDRSZE,NIM2,NJM2,NKM2

C Set grid variables
      NI=NIM2+2
      NJ=NJM2+2
      NK=NKM2+2
      NIM1=NIM2+1
      NJM1=NJM2+1
      NKM1=NKM2+1

C Read domain geometry.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)
C      write (6,*) IREC,(XU(I),I=1,NI)
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)
C      write (6,*) IREC,(YV(I),I=1,NJ)
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)
C      write (6,*) IREC,(ZW(I),I=1,NK)

C Set geometric variables.
      call GRIDGEO

      return

 1000 IER=-1
      call edisp(iuout,'CFD lib header fault. Cannot read file')
C      write (6,*) 'IREC ',IREC
      call erpfree(iunit,istat)
      return
      end

C ******************** CFDHASH ********************
C CFDHASH - return record and column jump from CFD hash table to first 
C metric in results listing (i.e. U velocity; V velocity for the same 
C control volume will be at NRJ+1; Wvel at NRJ+2; T at NRJ+3 etc)

      SUBROUTINE CFDHASH(ITIM,IDOM,IX,IY,IZ,NRJ,NCJ)
      INCLUDE 'cfd.h'

      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)

C Calculate initial record jump due to days and domain.
      NRJ=(ITIM*NCFDSZ)+NRCFDOM(IDOM-1)

C Calculate additional record jump.
      NRJ=NRJ+(IY-1)*(IZ-1)

C Which element is required?
      NCJ=IX

      return
      end

C ********************* MCFDM *********************
C MCFDM - define the parameters for the visualization of results.

      subroutine MCFDM(ICOMP)
      INCLUDE 'building.h'

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvimg/imgtyp,IMOPTS
      common/flvnam/LBITrt(MCOM,6)
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax

      CHARACTER H*72,KEY*1,ITEM(35)*26,outs*124
      character LBITrt*12,t12*12,t24*24

C Set vector colour option to velocity and temperature only.
      IMOPTS=0

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this item, allowing editing.
C Setup for multi-page menu.
      ICOMP=1
 3    MHEAD=3
      MCTL=6
      ILEN=Nview(ICOMP)
      INO=-3

C imgtyp determines the format of the generated image:
C  imgtyp = 0 : 2d slice drawn on screen.
C  imgtyp = 1 : 2d slice in Xbitmap format to file.
C  imgtyp = 2 : 2d slice in Xpixmap format to file.
C  imgtyp = 3 : 3d image drawn on screen.
C  imgtyp = 4 : TECplot data file.
      ITEM(1)='a Output period'
      if (imgtyp.eq.0) then
        ITEM(2)='b Image format >>screen2d'
      elseif (imgtyp.eq.1) then
        ITEM(2)='b Image format >> Xbitmap'
      elseif (imgtyp.eq.2) then
        ITEM(2)='b Image format >> XPM    '
      elseif (imgtyp.eq.3) then
        ITEM(2)='b Image format >>screen3d'
      elseif (imgtyp.eq.4) then
        ITEM(2)='b Output format >>TECplot'
      endif
      ITEM(3)=' ----------------------- '

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
      ITEM(M+4)='> create image(s)   '
      ITEM(M+5)='? help     '
      ITEM(M+6)='- exit this menu  '
      INO=-4

C Instanciate h() strings for this menu.
      h(1) ='This facility is used to generate images of the'
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
 
  80  CALL EMENU('Flow visualization',ITEM,NITMS,INO)
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
C Loop through each timestep. 
C Calculate number of time steps for output, set output frame and
C redefine name of image.
        NCTS=int((float(ICDYOF-ICDYOS)*24.0+CFTOF-CFTOS)*float(NTS))
        if (NCTS.gt.0) then
          do 48 IM=1,Nview(ICOMP)
            ISOPT(IM)=-1
 48       continue
          do 50 IFRAME=1,NCTS
            call RDCFDAT(IFRAME)
            if (imgtyp.eq.4) then
              call tecplotend
            else
              call VISMAK(ICOMP,IFRAME)
            endif
 50       continue
        endif
      elseif (INO.EQ.(NITMS-3)) THEN

C List current.
        if (Nview(ICOMP).eq.0) then
          write (outs,'(a)') 'No view definitions to list yet !'
          call edisp(iuout,outs)
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
      elseif (INO.EQ.(NITMS-4)) THEN

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
          write (outs,'(2a)')' Defaults set for arrow scaling factors ',
     &                'and generation frequency. Edit via image pick. '
          call edisp(iuout,outs)
        elseif (IW.eq.2) then

C Remove an image definition.
          call usrmsg(' ','Facility not available. ','P')
        endif

C Output period.
      ELSEIF(INO.eq.1) then
        CALL CFDPER(3)

C Image format.
      ELSEIF(INO.eq.2) then
        imgtyp=imgtyp+1
        if (imgtyp.gt.4) imgtyp=0

C Existing image selected.
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete or copy scene previously selected then new selection should 
C be deleted or copied, respectively.
        NV=INO-MHEAD
 200    write (outs,'(a,a,a)') 'For image definition ',
     &        LBITrt(ICOMP,NV)(1:lnblnk(LBITrt(ICOMP,NV))),' edit:'
        call EASKATOG(outs,' ','Name','View dir & layer','Resolution',
     &           'Customise arrows','Continue','  ','  ',IWE,3)
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
          goto 3
        elseif (IWE.eq.6) then
          goto 3
        endif
        goto 200
      endif

      goto 3
      end

C ******************** CFDPER ********************
C CFDPER sets the CFD output period. (IOPT=1 then whole period)

      SUBROUTINE CFDPER(IOPT)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF

      CHARACTER MTHNAM(12)*3,outs*124,PDESCR*60

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

      if (IOPT.eq.1) then

C In default mode then set output period to whole period.
        ICDYOS=ICDYS
        ICDYOF=ICDYF
        CFTOS=CFTS
        CFTOF=CFTF
      elseif (IOPT.eq.2) then
        write (outs,'(2a)') 'Please pick a single timestep for flow ',
     &          'visualisation.'
        call edisp(iuout,outs)
        CALL EDAYR(ICDYS,IDs,IMs)
        CALL EDAYR(ICDYF,IDf,IMf)
        write(PDESCR,'(2(i3,a4,a,f5.2,a))')IDs,MTHNAM(IMs),', hr ',CFTS,
     &              ' to',IDf,MTHNAM(IMf),', hr ',CFTF,'.'
        write (outs,'(2a)')
     &     'Current result set contains CFD data for the period:',PDESCR
        call edisp(iuout,outs)

C Set default to first timestep.
        IMO=IMs
        IDO=IDs
        IDAY=ICDYS
        TIME=CFTS

C Select a single timestep to plot output for.
 10     IFDAY=1
        call ASKRTIM(IFDAY,IMO,IDO,IDAY,TIME,IT,IER)

C Check that CFD active at this time.
        if (IDAY.lt.ICDYS.or.IDAY.gt.ICDYF) then
          call edisp(iuout,' Outside simulated period.')
          goto 10
        elseif (IDAY.eq.ICDYS.and.IDAY.eq.ICDYF.and.
     &          TIME.lt.CFTS.and.TIME.ge.CFTF) then
          call edisp(iuout,' Outside simulated period.')
          goto 10
        elseif (IDAY.eq.ICDYS.and.TIME.lt.CFTS) then
          call edisp(iuout,' Before start time.')
          goto 10
        elseif (IDAY.eq.ICDYF.and.TIME.ge.CFTF) then
          call edisp(iuout,' After finish time.')
          goto 10
        endif
        ICDYOS=IDAY
        ICDYOF=IDAY
        CFTOS=TIME
        CFTOF=TIME
      else
        ICDYOS=ICDYS
        ICDYOF=ICDYF
        CFTOS=CFTS
        CFTOF=CFTF
        CALL EDAYR(ICDYS,IDs,IMs)
        CALL EDAYR(ICDYF,IDf,IMf)
        write(PDESCR,'(2(i3,a4,a,f5.2,a))')IDs,MTHNAM(IMs),', hr ',CFTS,
     &              ' to',IDf,MTHNAM(IMf),', hr ',CFTF,'.'
        write (outs,'(2a)')
     &     'Current result set contains CFD data for the period:',PDESCR
        call edisp(iuout,outs)
 20     IFDAY=1
        call ASKRTIM(IFDAY,IMs,IDs,ICDYOS,CFTOS,IT,IER)
        call ASKRTIM(IFDAY,IMf,IDf,ICDYOF,CFTOF,IT,IER)

C Check that CFD active at this time.
        if (ICDYOS.lt.ICDYS.or.ICDYOF.gt.ICDYF) then
          call edisp(iuout,' Outside simulated period.')
          goto 20
        elseif (ICDYOS.eq.ICDYS.and.CFTOS.lt.CFTS) then
          call edisp(iuout,' Before start time.')
          goto 20
        elseif (ICDYOF.eq.ICDYF.and.CFTOF.ge.CFTF) then
          call edisp(iuout,' After finish time.')
          goto 20
        elseif (ICDYOS.eq.ICDYS.and.ICDYOF.eq.ICDYF.and.
     &          CFTOS.lt.CFTS.and.CFTOF.ge.CFTF) then
          call edisp(iuout,' Outside simulated period.')
          goto 20
        endif
      endif

      RETURN
      END

C ******************** RDCFDAT ********************
C RDCFDAT reads a timesteps worth of data into standard arrays.
C   ICFTS - CFD timestep to read (use this value if +ive otherwise
C      use first timestep of output period)

      SUBROUTINE RDCFDAT(ICFTS)
      INCLUDE 'building.h'
      INCLUDE 'cfd.h'

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
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
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz) 
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),NCTM,
     &      JHUMINDX
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      COMMON/cfddoms/NCDOM,ICDOM,ICFDZ(MNZ)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)

      CHARACTER  POLNAM*12
      LOGICAL CALPOL,CALLMA

C Set unit number.
      iunit=IFIL+14

C Calculate number of time steps into CFD results lib.
      if (ICFTS.gt.0) then
        ITS=ICFTS
      else
        ITS=int((float(ICDYOS-ICDYS)*24.0+CFTOS-CFTS)*float(NTS))+1
      endif
C      write (6,*) 'ITS ',ITS

C Read data from res file.
      ISTREC=5+((MCOM-1)*4)+ NCFDSZ*(ITS-1)+NCTM
      if (ICDOM.gt.1) then
        do 10 I=1,ICDOM-1
          ISTREC=ISTREC+NRCFDOM(I)
 10     continue
      endif
      
C      write (6,*) 'ISTREC ',ISTREC,NCFDSZ,ITS,NRCFDOM(1),NRCFDOM(2)
C      write(6,*)'ieqsv ',(ieqsv(i),i=1,5+nctm)
      IREC=ISTREC
      do 100 K=2,NK
        do 101 J=2,NJ

C Read data from library. Adjust trace to file unit 6 if ntcelz>48.
          if (ICFDSV.eq.1) then
            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Uf(I,J,K),I=2,NI)
C            write (6,'(i5,48f9.5)') IREC,(Uf(I,J,K),I=2,NI)
            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Vf(I,J,K),I=2,NI)
C            write (6,'(i5,48f9.5)') IREC,(Vf(I,J,K),I=2,NI)
            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Wf(I,J,K),I=2,NI)
C            write (6,'(i5,48f9.5)') IREC,(Wf(I,J,K),I=2,NI)
            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Tf(I,J,K),I=2,NI)
C            write (6,'(i5,48f9.5)') IREC,(Tf(I,J,K),I=2,NI)
          else
          
C Results library version 2.  Each metric is individually flagged if saved.
            if (IEQSV(1).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Uf(I,J,K),I=2,NI)
C              write (6,*) IREC,' Uf ',(Uf(I,J,K),I=2,NI)
            endif
            if (IEQSV(2).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Vf(I,J,K),I=2,NI)
C              write (6,*) IREC,' Vf ',(Vf(I,J,K),I=2,NI)
            endif
            if (IEQSV(3).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Wf(I,J,K),I=2,NI)
C              write (6,*) IREC,' Wf ',(Wf(I,J,K),I=2,NI)
            endif
            if (IEQSV(4).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Tf(I,J,K),I=2,NI)
C              write (6,*) IREC,' Tf ',(Tf(I,J,K),I=2,NI)
            endif
            if (IEQSV(5).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                           (AGEf(I,J,K),I=2,NI)
C              write (6,*) IREC,' Age ',(AGEf(I,J,K),I=2,NI)
            endif
            DO 121 ICTM=1,NCTM
            if (IEQSV(5+NCTM).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                     (POLCONCf(ICTM,I,J,K),I=2,NI)
C                  write (6,*)IREC,' pol ',(POLCONCf(ICTM,I,J,K),I=2,NI)
            endif
 121        CONTINUE
          endif
 101    continue
 100  continue

C Create vector info.
      do 200 K=2,NKM1
        do 201 J=2,NJM1
          do 202 I=2,NIM1
            hlfUatP=0.25*(Uf(I,J,K)+Uf(I+1,J,K))
            hlfVatP=0.25*(Vf(I,J,K)+Vf(I,J+1,K))
            hlfWatP=0.25*(Wf(I,J,K)+Wf(I,J,K+1))
            VECXbeg(I,J,K) = XP(I) - hlfUatP
            VECXend(I,J,K) = XP(I) + hlfUatP
            VECYbeg(I,J,K) = YP(J) - hlfVatP
            VECYend(I,J,K) = YP(J) + hlfVatP
            VECZbeg(I,J,K) = ZP(K) - hlfWatP
            VECZend(I,J,K) = ZP(K) + hlfWatP
 202      continue
 201    continue
 200  continue

      RETURN
 1000 call edisp(iuout,'CFD read data fault. Cannot read file.')
C      write (6,*) 'IREC ',IREC
      call erpfree(iunit,istat)
      return
      END
