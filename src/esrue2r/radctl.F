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

C RADCTL.F contains routines to control the creation of radiance files 
C and the control the visulaisation process depending on the value of 
C RIFPURP. Contains:
C  EMKRAD  - make radiance descriptions.
C  MKVREND - render/ calculate radiance scenes.
C  CHITMSL - a copy of CHITMS but for longer strings (300 characters).


C *********************** EMKRAD ***********************
C EMKRAD controls the creation and editing of radiance scene descriptions.
      SUBROUTINE EMKRAD
      INCLUDE 'building.h'

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      common/expath/runpath

      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               RIFDESC(MCOM+1),RIFPURP(MCOM+1)
      common/raddata2/NRIF,IRIFFOC
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)
      common/radgrid/LDFGRID
      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)

      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      LOGICAL MODIFY,MODLEN,MODBND
      LOGICAL XST,ok,dok

      DIMENSION ITEM(11)

      character SCENE*28,RIFNAME*72,SCENERT*28,RIFDESC*72,RIFPURP*12
      character*72 rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
      CHARACTER H*72,ITEM*30,ltmp*72,LDFGRID*72,OUTSTR*124,LBSRIF*72
      character tfile*72,runpath*72

C Create and display menu.
      ITA1 = IFIL+6
      ITA2 = IFIL+7

10    INO = -4
      write(ITEM(1),'(A,A)')   '  scene  : ',SCENE(IRIFFOC)(1:18)
      write(ITEM(2),'(A,A)')   '  purpose: ',RIFPURP(IRIFFOC)
      ITEM(3)=                 '  __________________________ '
      ITEM(4) =                'a sky type, location & time  '
      ITEM(5) =                'b zone & outside composition '
      ITEM(6) =                'c other (furniture&fittings) '
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(7) =              'd edit/display grid          '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(7) =              'd                            '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(7) =              'd                            '
      else
        ITEM(7) =              'd scene view points          '
      endif
      ITEM(8) =                '  __________________________ '
      ITEM(9)=                 '? help                       '
      ITEM(10)=                '- return to main menu        '
      NITEMS = 10

C Help text for the menu.
 20   H(1)= 'A Radiance scene is constructed in several sections:'
      H(2)= '  '
      H(3)= '* Sky type, location & date. Allows the selection of'
      H(4)= '  one sky condition, date and location for use with'
      H(5)= '  this scene. To analyse the same model under another'
      H(6)= '  sky copy this scene and recreate the new sky file.'
      H(7)= '  '
      H(8)= '* Zone & outside composition. Creates a Radiance  '
      H(9)= '  description of the ESP-r model, inc. obstruction'
      H(10)='  blocks and a ground topology if defined.'
      H(11)='  '
      H(12)='* Other (furniture & fittings):  This facility'
      H(13)='  allows manual editing of Radiance files only.'
      H(14)='  '
      H(15)='* Set views:  Depending on the portion of the ESP-r'
      H(16)='  problem you have included, you can setup views'
      H(17)='  (eye position, viewing direction & angle)'
      H(18)='  which can be passed as initial viewing data to'
      H(19)='  the interactive or static image creation modules.'
      H(20)='  Multiple viewpoints can be associated with a scene.'

C Present the main menu.
      CALL EMENU('Radiance descriptions',ITEM,NITEMS,INO)
      IF(INO.lt.4.or.INO.gt.NITEMS) then

C First check for illegal menu choice.
        goto 10
      ELSEIF(INO.EQ.NITEMS)THEN

C Return to main menu - update rcf file.
        ICHK=3
        call radcfgout(ICHK)
        return
      ELSEIF(INO.EQ.NITEMS-1)THEN

C Help message.
        CALL PHELPD('Scene generation',20,'-',0,0,IER)
      ELSEIF(INO.EQ.4)THEN

C Sky and Time.....
        call SKYFORM(IER)
        call edisp(iuout,' updating RIF file to account for zones.')
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif
      ELSEIF(INO.EQ.5)THEN

C Zone & Outside definition, updating rif file upon return.
        call E2RFORM('i',IER)
        call edisp(iuout,' updating RIF file to account for zones.')
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif
      ELSEIF(INO.EQ.6)then

C Miscel (furniture & fittings) definitions.
        H(1)='In order not to disturb files generated from a thermal'
        H(2)='description it is sometimes useful to place miscel '
        H(3)='furniture and fixtures in a separate file. '
        ltmp=rmfil
        CALL EASKS(ltmp,' Miscel (furniture & fittings file ?',
     &    '  ',72,rmfil,'misc fixt file name',IER,3)
        INQUIRE (FILE=ltmp,EXIST=XST)
        if(XST)rmfil = ltmp

        H(1)='Browsing lists contents in the text feedback'
        H(2)='area, editing spawns an editor with the file name.'
        CALL EASKABC(' Miscel (furniture & fittings description:',' ',
     &     'browse file','edit file','ignore',IW,2)
        if(IW.eq.1)then
          call vifile(ITA1,rmfil,'b',ier)
        elseif(IW.eq.2)then
          call vifile(ITA1,rmfil,'e',ier)
        endif
      ELSEIF(INO.EQ.7)THEN
        if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
          call ERPFREE(ITA1,ISTAT)
          write(tfile,'(a,a)')runpath(1:lnblnk(runpath)),
     &                                      LDFGRID(1:lnblnk(LDFGRID))
          call FPOPEN(ITA1,ISTAT,1,1,tfile)
          if(ISTAT.eq.0)then
            NDFP=0
  42        CALL STRIPC(ITA1,OUTSTR,0,ND,0,'grid points',IFER)
            if (IFER.eq.0) then
              NDFP=NDFP+1
              K=0
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid x',IER)
              DFPTS(NDFP,1)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid y',IER)
              DFPTS(NDFP,2)=XTMP
              CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid z',IER)
              DFPTS(NDFP,3)=XTMP
              CALL EGETWR(OUTSTR,K,DFDIR(1),0.,0.,'-','vec x',IER)
              CALL EGETWR(OUTSTR,K,DFDIR(2),0.,0.,'-','vec y',IER)
              CALL EGETWR(OUTSTR,K,DFDIR(3),0.,0.,'-','vec z',IER)
              goto 42
            endif
            CALL DFGRIDLBL('g')
          endif
          call ERPFREE(ITA1,ISTAT)
          dok=.false.
          h(1)='If you want to alter the grid points say yes.'
          CALL ASKOK('Edit grid points?','  ',OK,dok,1)
          izone=0
          if(OK)call setdfgrid('-',izone,ier)
        else

C Edit view points.  Save off current views.
          teye1=EYEM(1)
          tviewm1=VIEWM(1)
          teye2=EYEM(2)
          tviewm2=VIEWM(2)
          teye3=EYEM(3)
          tviewm3=VIEWM(3)
          tang=ANG
          thang=HANG
          itbndt=ITBND
          call VEWFORM(IER)
          call edisp(iuout,' updating RIF file to account for views.')
          IFC=3
          call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
          if (NBSRIF(IRIFFOC).eq.1) then
            call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
          endif

C Because of the site plan, recalculate perspective view.
C Recover previous view.
          EYEM(1)=teye1
          VIEWM(1)=tviewm1
          EYEM(2)=teye2
          VIEWM(2)=tviewm2
          EYEM(3)=teye3
          VIEWM(3)=tviewm3
          ANG=tang
          HANG=thang
          ITBND=itbndt
          MODIFY=.TRUE.
          MODLEN=.TRUE.
        endif
      ELSE
        INO=-2
        GOTO 20
      ENDIF
      GOTO 10

      END

C *********************** MKVREND ***********************
C MKVREND controls the creation and editing of visualisations and renderings.
      SUBROUTINE MKVREND
      INCLUDE 'building.h'

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew
      common/rad1/rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil

      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               RIFDESC(MCOM+1),RIFPURP(MCOM+1)
      common/raddata2/NRIF,IRIFFOC
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)
      common/radgrpts/NDFP,DFPTS(500,3),DFDIR(3),DFVALS(500)
      common/expath/runpath

      character SCENE*28,RIFNAME*72,SCENERT*28,RIFDESC*72,RIFPURP*12
      character*72 rofil,rzfil,rskyfil,octfil,picfil,rmfil,glzfil
      CHARACTER H*72,ITEM*30,LBSRIF*72,runpath*72,ltmp*72,OUTSTR*124
      character pf*72,gf*72,df*72,VALT*28,vewcmds*124,rvewsh*10,outs*124

      DIMENSION ITEM(10),IVALV(20),VALT(20),IPVALV(20)

      LOGICAL XST

C Set file numbers.
      ITA1 = IFIL+6
      ITA2 = IFIL+7

10    INO=-4
      write(ITEM(1),'(A,A)')   '  scene  : ',SCENE(IRIFFOC)(1:18)
      write(ITEM(2),'(A,A)')   '  purpose: ',RIFPURP(IRIFFOC)
      ITEM(3)=                 '  __________________________ '
      ITEM(4) =                'a scene parameter options    '
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(5) =              'b calculate daylight factors '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(5) =              'b calculate sensor illum.    '
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(5) =              'b calculate daylight coeffs  '
      else
        ITEM(5) =              'b render the scene           '
      endif
      if (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        ITEM(6) =              'c view daylight factor result'
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
        ITEM(6) =              'c view calculated illuminance'
      elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef') then
        ITEM(6) =              'c view daylight coeffs       '
      else
        ITEM(6) =              'c view visualisation results '
      endif
      ITEM(7) =                '  __________________________ '
      ITEM(8) =                '> save scene information     '
      ITEM(9)=                 '? help                       '
      ITEM(10)=                '- return to main menu        '
      NITEMS = 10

C Reset local file names.
      gf=' '
      pf=' '

C Help text for the menu.
      H(1)='Radiance visualisation/ calculation options available:'
      H(2)= '  '
      H(3)= '* Image setup & calc options '
      H(4)= ' Check the parameter settings (and perhaps edit them)'
      H(5)= ' before starting the calculation. '
      H(6)= '  '
      H(7)= '* Render the scene  '
      H(8)= ' Radiance is started, using the parameter settings '
      H(9)= ' defined, to generate all the information required '
      H(10)= ' to satisfy the scene purpose. '
      H(11)= '  '
      H(12)= '* Visualisation results '
      H(13)= ' View the results of the Radiance calculations. '

C Present the main menu.
      CALL EMENU('Visualisation',ITEM,NITEMS,INO)
      IF(INO.EQ.NITEMS)THEN

C Return to main menu.
        return
      ELSEIF(INO.EQ.NITEMS-1)THEN

C Help message.
        CALL PHELPD('Scene generation',13,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEMS-2)THEN

C Save rif file.
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif
      ELSEIF(INO.EQ.4)THEN

C Setup image and calculation options.
        call RRSKY(ITA2,rskyfil,ga,IER)
        if(IER.LT.0)then 
          CALL USRMSG(' ',' Please define the sky first!','W')
          GOTO 10
        endif
        call RIFFORM(ier)
      ELSEIF(INO.EQ.5)THEN

C Generate image/ visualisation data/ daylight factors.
C Check and see if there is an existing octree file.  Probably best
C to remove it before proceeding.
        CALL ERPFREE(ITA2,ISTAT)
        call FINDFIL(octfil,XST)
        if(XST)then
          H(1)='The octree is a intermediate step between the '
          H(2)='Radiance input files and the rendered image. '
          H(3)='Radiance should be able to decide if the octree '
          H(4)='needs recreated or not, although if you have a '
          H(5)='new sky file then you must force Radiance to  '
          H(6)='recreate the octree (ie choose option 1).'
          CALL EASKABC(' The octree exists, do you want to',' ',
     &      'force update','let Radiance decide (default)','cancel',
     &                                                        IOCTFL,6)
          if (IOCTFL.eq.3) then
            goto 10
          endif
        endif

C Pull up a list of views and ask user for which one.
        if (RIFPURP(IRIFFOC)(1:8).eq.'External'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Internal'.or.
     &      RIFPURP(IRIFFOC)(1:5).eq.'Glare') then
          if (indxvew.gt.0) then
            H(1)='Please select one view.'
            IX=1
            do 333 iv=1,indxvew
              write(valt(iv),'(a)') rvewsh(iv)
  333       continue
            CALL EPICKS(IX,IVALV,' ',' Select a view: ',
     &                  28,indxvew,VALT,'View options',IER,3)
            if(IX.ne.0)then
              ipckvew=IVALV(1)
            endif
          endif
        endif

        if (RIFPURP(IRIFFOC)(1:8).eq.'External'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Internal') then

C Generate image if external or internal.
          call genimage(1)
        elseif (RIFPURP(IRIFFOC)(1:5).eq.'Glare') then

C Extra calculations for glare.
          call genimage(2)
          call getglr
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then

C Daylight factors.
          call getdf
        endif

C Display results of calculations.
      ELSEIF(INO.EQ.6)THEN
        if (RIFPURP(IRIFFOC)(1:8).eq.'Day_coef'.or.
     &      RIFPURP(IRIFFOC)(1:8).eq.'Coupling') then
          call usrmsg('No results display for this option.','  ','W')
          goto 10
        elseif (RIFPURP(IRIFFOC)(1:8).eq.'Day_fact') then
        
C Check for daylight factor results.
          write(df,'(2a)')SCENERT(NRIF)(1:lnblnk(SCENERT(NRIF))),'.df'
          write(ltmp,'(2a)')runpath(1:lnblnk(runpath)),df(1:lnblnk(df))
          call ERPFREE(ITA1,ISTAT)
          call FINDFIL(ltmp,XST)
          if(XST)then
            call FPOPEN(ITA1,ISTAT,1,1,ltmp)
            NDFP=0
            AVEDF=0.
            DFMIN=10000.
            if(ISTAT.eq.0)then
  43          CALL STRIPC(ITA1,OUTSTR,0,ND,0,'DF results',IFER)
              if (IFER.eq.0) then
                NDFP=NDFP+1
                K=0
                CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid x',IER)
                DFPTS(NDFP,1)=XTMP
                CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid y',IER)
                DFPTS(NDFP,2)=XTMP
                CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','grid z',IER)
                DFPTS(NDFP,3)=XTMP
                CALL EGETWR(OUTSTR,K,XTMP,0.,0.,'-','DF',IER)
                DFVALS(NDFP)=XTMP
                AVEDF=AVEDF+XTMP
                DFMIN=amin1(DFMIN,XTMP)
                goto 43
              endif
            endif
            call ERPFREE(ITA1,ISTAT)
            AVEDF=AVEDF/real(NDFP)
            write(outs,'(a,f5.2,a)')'Average daylight factor ',AVEDF,'%'
            call edisp (iuout,outs)
            write(outs,'(a,f5.2)')'Uniformity ratio (min/ave) ',
     &                                  DFMIN/AVEDF
            call edisp (iuout,outs)
            call DFGRIDLBL('d')
          endif
          goto 10
        endif

C Check for existing file names based on available view and picture info.
        IX=1
        npics=0
        do 3331 iv=1,indxvew
          XST=.false.
          write(pf,'(5a)') runpath(1:lnblnk(runpath)),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(iv)(1:lnblnk(rvewsh(iv))),'.pic'
          INQUIRE (FILE=pf,EXIST=XST)
          if (XST) then
            npics=npics+1
            write(valt(npics),'(a)') rvewsh(iv)
            IPVALV(npics)=iv
          else
            write(6,*) 'file ',pf(1:lnblnk(pf)),' not found...'
          endif
 3331   continue
        npics=npics+1
        write (valt(npics),'(a)') 'Other '

C Now have list of possible images, ask which one to display.
C If there are no images then re-display the menu.
        if (npics.gt.1) then
          H(1)='Please select one view.'
          CALL EPICKS(IX,IVALV,' ',' Select a view point: ',
     &                28,npics,VALT,'View options',IER,1)
          if(IX.eq.0) goto 10
        else
          goto 10
        endif
        if (IVALV(1).eq.(npics+1)) then
          write(pf,'(a)') 'UNKNOWN.pic'
          write(gf,'(a)') 'UNKNOWN.glr'
        else
          ipckvew=IPVALV(IVALV(1))

C Generate default file names.
          write(pf,'(5a)') runpath(1:lnblnk(runpath)),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),'.pic'
          write(gf,'(5a)') runpath(1:lnblnk(runpath)),
     &      picfil(1:lnblnk(picfil)),'_',
     &      rvewsh(ipckvew)(1:lnblnk(rvewsh(ipckvew))),'.glr'
        endif

C Check chosen image file name.
        CALL ERPFREE(ITA2,ISTAT)
        call FINDFIL(pf,XST)
        if (.not.XST) then
          call usrmsg('Cannot find picture file: ',pf(1:lnblnk(pf)),'W')
          goto 10
        endif

C Get glare info file name (if necessary) and display image.
        if (RIFPURP(IRIFFOC)(1:5).eq.'Glare') then
          CALL ERPFREE(ITA2,ISTAT)
          call FINDFIL(gf,XST)
          if (.not.XST) then
            call usrmsg('Cannot find glare file: ',gf(1:lnblnk(gf)),'W')
            goto 10
          endif
          call dispimage(2)
        else
          call dispimage(1)
        endif
      ENDIF
      goto 10

      END

C ********************* CHITMSL *********************
C CHITMSL checks a character string (A), returning the number of data
C items (IW) separated by ' ' tab or ','.  Note to keep from 
C overwriting the string passed, deal with a copy.
C CHITMSL is similar to CHITMS but allows checking of longer strings and
C is particularly useful for Radiance.
      SUBROUTINE CHITMSL(A,IW)
      CHARACTER*(*) A
      CHARACTER B*254,C*1


      ISLENG=LNBLNK(A)
      if (ISLENG.lt.254) ISLENG=ISLENG+1

      WRITE(B,'(A)') A(1:ISLENG)

C Loop through B and convert all ',' and 'tabs' to blanks.
      DO 100 I=1,LNBLNK(B)+1
        C=B(I:I)
        IF(C.EQ.','.OR.ICHAR(C).EQ.9) B(I:I)=' '
  100 CONTINUE

C Loop through B and see how many 'words'.
      K=0
      IW=0
  102 IW=IW+1

C Start by skipping blanks before the word.
   10 K=K+1
      C=B(K:K)
      IF(C.EQ.' ') GOTO 10

C Loop character by character until separator is found.
   20 K=K+1
      C=B(K:K)
      IF(K.GE.LNBLNK(B))RETURN
      IF(C.EQ.' ') GOTO 102
      GOTO 20
 
      END

C ********************* RADPAR *********************
C RADPAR - check and update render options.
C If VAL is < 0 then return current setting otherwise set par=VAL.
      SUBROUTINE RADPAR(PAR,VAL)

      COMMON/FILEP/IFIL
      common/expath/runpath
      common/e2rf/rambfil,optnfil

      common/e2rc/crenrp,coconv

      character crenrp*160,coconv*40,flag*8,PAR*2
      character rambfil*72,optnfil*72,runpath*72,OUTSTR*124,ltmp*72

      ITA2 = IFIL+7

C Set all values to -1 (i.e. unset).
C Direct values.
      xdp=-1.
      xds=-1.
      xdj=-1.
      xdt=-1.
      xdc=-1.
      xdr=-1.

C Specular values.
      xsj=-1.
      xst=-1.

C Ambient values.
      xab=-1.
      xaa=-1.
      xad=-1.
      xas=-1.
      xar=-1.
      xav=-1.

C Reflections.
      xlr=-1.
      xlw=-1.

C Mist. - not active yet
C      xms=-1.
      

C Check for defined items - if found then decode.
      call CHITMSL(crenrp,IW)
      K=0
 10   if (IW.gt.1) then
        call EGETW(crenrp,K,flag,'W','radiance parameter',IER)
        if (flag(2:3).eq.'dp') then
          call EGETWR(crenrp,K,xdp,0.0,2048.0,'W','dir pretest den',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ds') then
          call EGETWR(crenrp,K,xds,0.0,1.0,'W','direct sampling',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dj') then
          call EGETWR(crenrp,K,xdj,0.0,1.0,'W','direct jitter',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dt') then
          call EGETWR(crenrp,K,xdt,0.0,1.0,'W','direct threshold',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dc') then
          call EGETWR(crenrp,K,xdc,0.0,1.0,'W','direct certainty',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dr') then
          call EGETWR(crenrp,K,xdr,0.0,10.0,'W','direct relays',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'sj') then
          call EGETWR(crenrp,K,xsj,0.0,1.0,'W','specular jitter',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'st') then
          call EGETWR(crenrp,K,xst,0.0,1.0,'W','specular threshold',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ab') then
          call EGETWR(crenrp,K,xab,0.0,7.0,'W','ambient bounces',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'aa') then
          call EGETWR(crenrp,K,xaa,0.0,0.5,'W','ambient accuracy',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ad') then
          call EGETWR(crenrp,K,xad,0.0,4096.0,'W','ambient divs',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'as') then
          call EGETWR(crenrp,K,xas,0.0,2048.0,'W','amb super-smpls',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ar') then
          call EGETWR(crenrp,K,xar,0.0,1024.0,'W','amb resolution',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'av') then
          call EGETWR(crenrp,K,xavr,0.0,30.0,'W','amb value red',IER)
          call EGETWR(crenrp,K,xavg,0.0,30.0,'W','amb value green',IER)
          call EGETWR(crenrp,K,xavb,0.0,30.0,'W','amb value blue',IER)
          xav=(xavr+xavg+xavb)/3.
          IW=IW-4
        elseif (flag(2:3).eq.'lr') then
          call EGETWR(crenrp,K,xlr,0.0,12.0,'W','limit reflection',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'lw') then
          call EGETWR(crenrp,K,xlw,0.0,1.0,'W','limit weight',IER)
          IW=IW-2
        else
          write (6,*) 'Unknown radiance parameter (1): ',flag
        endif

C Check other parameters.
        goto 10
      endif

      if (VAL.lt.0.0) then

C Return current value of parameter.
        if (PAR.eq.'dp') then
          VAL=xdp
        elseif (PAR.eq.'ds') then
          VAL=xds
        elseif (PAR.eq.'dj') then
          VAL=xdj
        elseif (PAR.eq.'dt') then
          VAL=xdt
        elseif (PAR.eq.'dc') then
          VAL=xdc
        elseif (PAR.eq.'dr') then
          VAL=xdr
        elseif (PAR.eq.'sj') then
          VAL=xsj
        elseif (PAR.eq.'st') then
          VAL=xst
        elseif (PAR.eq.'ab') then
          VAL=xab
        elseif (PAR.eq.'aa') then
          VAL=xaa
        elseif (PAR.eq.'ad') then
          VAL=xad
        elseif (PAR.eq.'as') then
          VAL=xas
        elseif (PAR.eq.'ar') then
          VAL=xar
        elseif (PAR.eq.'av') then
          VAL=xav
        elseif (PAR.eq.'lr') then
          VAL=xlr
        elseif (PAR.eq.'lw') then
          VAL=xlw
        else
          write (6,*) 'Unknown radiance parameter (2): ',PAR
        endif

C If no value has been found for the parameter then scan options file.
        if (VAL.lt.0.0) then
          write (ltmp,'(2a)') runpath(1:lnblnk(runpath)),
     &                                       optnfil(1:lnblnk(optnfil))
          call FPOPEN(ITA2,ISTAT,1,1,ltmp)
 25       call STRIPC(ITA2,OUTSTR,99,ND,0,'rad options',IER)
          if (IER.eq.0) then
            K=0
            call EGETW(OUTSTR,K,FLAG,'W','radiance parameter',IER)
            if (FLAG(2:3).eq.PAR(1:2)) then
              XV=0.
              do 20 IP=1,ND-1
                call EGETWR(OUTSTR,K,XVA,0.0,0.0,'-','rad par val',IER)
                XV=XV+XVA
 20           continue
              VAL=XV/real(ND-1)
            endif
            goto 25
          endif
          CALL ERPFREE(ITA2,ISTAT)
        endif
      else

C Set new/ Update parameter.
        if (PAR.eq.'dp') then
          xdp=VAL
        elseif (PAR.eq.'ds') then
          xds=VAL
        elseif (PAR.eq.'dj') then
          xdj=VAL
        elseif (PAR.eq.'dt') then
          xdt=VAL
        elseif (PAR.eq.'dc') then
          xdc=VAL
        elseif (PAR.eq.'dr') then
          xdr=VAL
        elseif (PAR.eq.'sj') then
          xsj=VAL
        elseif (PAR.eq.'st') then
          xst=VAL
        elseif (PAR.eq.'ab') then
          xab=VAL
        elseif (PAR.eq.'aa') then
          xaa=VAL
        elseif (PAR.eq.'ad') then
          xad=VAL
        elseif (PAR.eq.'as') then
          xas=VAL
        elseif (PAR.eq.'ar') then
          xar=VAL
        elseif (PAR.eq.'av') then
          xav=VAL
        elseif (PAR.eq.'lr') then
          xlr=VAL
        elseif (PAR.eq.'lw') then
          xlw=VAL
        else
          write (6,*) 'Unknown radiance parameter (3): ',PAR
        endif

C Write out new string.
        crenrp=' '
        IPOSE=0
        IPOS=IPOSE+1
        if (xdp.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-dp',int(xdp)
          IPOS=IPOSE+1
        endif
        if (xds.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-ds',xds
          IPOS=IPOSE+1
        endif
        if (xdj.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dj',xdj
          IPOS=IPOSE+1
        endif
        if (xdt.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dt',xdt
          IPOS=IPOSE+1
        endif
        if (xdc.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dc',xdc
          IPOS=IPOSE+1
        endif
        if (xdr.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-dc',int(xdr)
          IPOS=IPOSE+1
        endif
        if (xsj.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-sj',xsj
          IPOS=IPOSE+1
        endif
        if (xst.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-st',xst
          IPOS=IPOSE+1
        endif
        if (xab.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-ab',int(xab)
          IPOS=IPOSE+1
        endif
        if (xaa.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-aa',xaa
          IPOS=IPOSE+1
        endif
        if (xad.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-ad',int(xad)
          IPOS=IPOSE+1
        endif
        if (xas.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-as',int(xas)
          IPOS=IPOSE+1
        endif
        if (xar.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-ar',int(xar)
          IPOS=IPOSE+1
        endif
        if (xav.ge.0.0) then
          IPOSE=IPOS+19
         write(crenrp(IPOS:IPOSE),'(1x,a3,3(1x,f4.1))')'-av',xav,xav,xav
          IPOS=IPOSE+1
        endif
        if (xlr.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-lr',int(xlr)
          IPOS=IPOSE+1
        endif
        if (xlw.ge.0.0) then
          IPOSE=IPOS+10
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f5.3)') '-lw',int(xlw)
          IPOS=IPOSE+1
        endif
      endif

      return
      end
