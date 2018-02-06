C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C tdfedit.F includes high level subroutines for access to
C   temporal file data.

C tdfedit: is the controlling subroutine for access to
C   temporal file data. 
C CLRTABC: clears temporal commons TDFFH, tdset, tdaid,GDPER.
C glbset(foc): global questions about TDF file. Foc is the topic.
C EXTRACTS: writes an ascii exchange file beginning at TIMEJD.
C TDFUPD: updates TDF file from an exchange file.

      subroutine tdfedit(act,ier)
#include "building.h"
#include "model.h"
#include "epara.h"
#include "net_flow.h"
#include "tdf2.h"
      
      integer lnblnk  ! function definition

C The following parameters are defined as integers in epara.h:
C CREATE,PREV,NEXT,EDIT,LIST,NONE

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/FILEP/IFIL
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

C Related to inport and export files.
      CHARACTER LIMP*96,LEXP*96
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP

C Logical flags for file openings.
      COMMON/TDFOPEN/OPTDF

      LOGICAL OPTDF,OK,dok
      logical FOUND,XST,unixok,silent

      DIMENSION ITEM(31),ikeybk(31),itemsel(MIT),itemval(MIT)
      CHARACTER tdaide*72
      character afil*96,tfile*96,TTAG*12
      CHARACTER LTMPFL*72
      CHARACTER ITEM*50,itemsel*50,KEY*1,FOCUS*8,DFILE*96
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      character path*72,outs*124,fs*1
      character act*16,actf*72,action*16,msgl2*124

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer numberofzones ! to pass to updwire rather than ncomp
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer iicapture,iiazi    ! to pass to updcapt updazi
#else
      integer*8 numberofzones
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 iicapture,iiazi    ! to pass to updcapt updazi
#endif
      integer IRE  ! for radio button
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='tdfedit'  ! set for subroutine

C For debugging set itrc =2
      ITRC=1

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Clear temporal commons TDFFH, tdset, tdaid,GDPER.
      call clrtabc

C File Units, default names & types as follows: 
C ASCII file to import data from: IUIMP=unit, LIMP=file name.
C Import file unknown (ITIMP=0), ASCII column listing (ITIMP=1).
      afil='  '
      OPTDF=.FALSE.
      IUIMP=IFIL+5
C     LTDFA is used if not empty
      LIMP ='tdf.import'
      ITIMP=1

C ASCII export file: IUEXP=unit, LEXP=file name.
C Export file unknown (ITEXP=0), ASCII column listing (ITEXP=1).
      IUEXP=IFIL+6
      LEXP ='tdf.export' 
      ITEXP=1

C Instanciate the standard temporal entities.
      FOCUS='ALL'
      CALL GENDAT(FOCUS,0,FOUND,IER)

C Reset width of menu. Remember so can restore when returning.
      lastmenuchw=menuchw
C      menuchw = 53
      menuchw = 48

C Open the text display box equal to LIMTTY if MMOD = 8.
      if(MMOD.EQ.8)then
        if(iglib.eq.1)then
          call winclr
          CALL feedbox(menuchw,2,igfw,igfh)
          CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)

C Setup and pass in parameters to win3d.
          iiw1=9; iiw2=12; iiw3=7; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          call opencpw
          call opensetup
        elseif(iglib.eq.2)then
          continue
        endif
      endif


C Action parameter. 
C << we might be passing it an ascii temporal file though and >>
C << so a conversion might be required first >>
      if(act(1:1).ne.' ')then
        action=act
        write(outs,'(a,a)')'TDF: the input action is: ',action
        call tstamp('>',outs)
        write(outs,'(a,a)')' the action is: ',action
        if(action(1:7).eq.'extract')then
          call edisp(iuout,outs)
          silent=.true.
        elseif(action(1:6).eq.'export')then
          call edisp(iuout,outs)
          silent=.true.
        elseif(action(1:6).eq.'import')then
          call edisp(iuout,outs)
          silent=.true.
        endif
      else
        action='-'
        silent=.false.
      endif

C If tdf file has been specified then load it.
      if (LTDFA(1:4).eq.'UNKN'.or.LTDFA(1:2).eq.'  ') then
        XST=.false.
      else
        INQUIRE (FILE=LTDFA,EXIST=XST)
      endif
      if(XST)then
        IER=0
        call supplyandcheck(LTDFA,'T',ier)
        if(ier.ne.0)then
          write(outs,*) 'non-zero error after call to supplyandcheck'
          call edisp(iuout,outs)
        endif
      else

C The file did not exist.
        call edisp(iuout,'The command line file did not exist. If you')
        call edisp(iuout,'want to create this file use menu option 1.')
      endif

      if(action(1:6).eq.'import')then

C Convert from ascii temporal file to scratch file. Ask for import file
C name, confirm TDF scratch file name, Open scratch file and then
C import data items. 
        msgl2='  '
        call USRMSG('Importing temporal data...',msgl2,'-')
        call TDFAIMPT(ITRC,actf,IUIMP,'i',IER)
        write(action,'(a)') '-'
        CALL ERPFREE(IUTDF,ISTAT)
        msgl2=' End of TDF'
        CALL USRMSG('Importing temporal data...done.',msgl2,'P')
        menuchw = lastmenuchw
        return
      endif

C Convert scratch file data and attributes into temporal ascii file.
      if(action(1:6).eq.'export')then
        msgl2='  '
        call USRMSG('Exporting temporal to ASCII file...',msgl2,'-')
        call ewtdfa(itrc,actf,IUEXP,0,ier)
        write(action,'(a)') '-'
        CALL ERPFREE(IUTDF,ISTAT)
        msgl2=' End of TDF'
        CALL USRMSG('Exporting temporal to ASCII file...done.',msgl2,
     &    'P')
        menuchw = lastmenuchw
        return
      endif
      if(action(1:7).eq.'extract')then

C Extract a period of information from the scratch file into a
C text file in 'extract' format (a cut down version of ascii temporal file).

C Help text for this menu.
        helptopic='tdf_time_format'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(outs,'(a,i4,a)')'Currently there are ',NTSPH,' ts/h. '
        call edisp(iuout,outs)
        write(outs,'(a,i4,a,i4)')'and data is available from day ',
     &     itdbdoy,' to ',itdedoy
        call edisp(iuout,outs)
        TIMEJD=real(itdbdoy)
        CALL EASKR(TIMEJD,' Time (as julian day) to start recovery? ',
     &    '  ',real(itdbdoy),'F',real(itdedoy),'F',1.5,
     &    'time=decimal day',IER,nbhelp)
        incr=NDBSTP - INT(NTSPH*24*(TIMEJD-real(itdbdoy)))
        CALL EASKI(incr,' Number of steps to recover ',' ? ',
     &    1,'F',NDBSTP,'F',1,'steps to recover',IER,nbhelp)
        msgl2='  '
        call USRMSG('Extracting timestep data...',msgl2,'-')
        call extracts(itrc,actf,IUEXP,TIMEJD,incr,ier)
        write(action,'(a)') '-'
        goto 44
      endif

C Reset pending actions on entry to the menu. See notes (above)
C on menu common blocks. Initialise explor menu size variables based on 
C text window size. 
 44   MHEAD=10
      MCTL=7
      ILEN=NITDF
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Display startup menu which includes only request for setup, save,
C maintain db, system scheduler, trace, help & finish.
    2 IER=0
      ILEN=NITDF

C Get Date strings
C Set the viewing period to the same as the TDF file period.
      IVBTIM=1
      IVETIM=NTSPH*24
      CALL EPERSTR(itdyear,itdbdoy,IVBTIM,itdedoy,IVETIM,
     &           NTSPH,IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
      igbdoy=itdbdoy
      igedoy=itdedoy

  3   INO=-2
      if(OPTDF)then
        WRITE(ITEM(1),'(A,A)')  '1 TDF file: ',LTDFA(1:37)
        WRITE(ITEM(2),'(A,A36)')'2 1st descr: ',tdaide(1)(1:36)
        WRITE(ITEM(3),'(a,A36)')'3 2nd descr: ',tdaide(2)(1:36)
      else
        WRITE(ITEM(1),'(A)')    '1 TDF file: not yet defined'
        WRITE(ITEM(2),'(A)')    '2 1st descr: -'
        WRITE(ITEM(3),'(a)')    '3 2nd descr: -'
      endif
      WRITE(ITEM(4),'(A,A40)')'4 ',PERST3(1:40)
      WRITE(ITEM(5),'(A,I2)') '5 Timesteps per hour : ',NTSPH
      WRITE(ITEM(6),'(a,I2,a)')
     &        '  ___entries (',NITDF,')______________________________'
      ITEM(7)='   entry name  |description '
      MHEAD=7
      MCTL=7
      M=MHEAD

C Loop through the TDF items until the page to be displayed. ikeybk
C equates index of TMENU and INO position (m).  
      IF(NITDF.GT.0)THEN
        DO 10 L=1,NITDF
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(L,KEY,IER)
            WRITE(ITEM(M),14)KEY,TAG(L),TAIDE(L)(1:30)
   14       FORMAT(A,2X,A,1X,A)
            ikeybk(m)=L
          ENDIF
   10   CONTINUE
      ELSE
        msgl2=' Or if no file opened select option 1.'
        CALL USRMSG(' No items yet, how about adding one!',msgl2,'W')
      endif
      ILEN=NITDF

C Length of menu. If creating a long list setup the paging vi EKPAGE
C and include page facility text as appropriate.
      MITEM=M+7
      IF(IPACT.EQ.CREATE)THEN
        CALL EKPAGE(IPACT)
        IPACT=NONE
      ENDIF

      IF(IPFLG.EQ.0)THEN
        ITEM(MITEM-6)='  _____________________________________'
      ELSE
        WRITE(ITEM(MITEM-6),15)IPM,MPM 
   15   FORMAT       ('0 Page ---- Part: ',I2,' of ',I2,' -----')
      ENDIF

C Set the menu control text.
      if(OPTDF)then
        ITEM(MITEM-5)  ='* add, delete, copy temporal entry     '
      else
        ITEM(MITEM-5)  ='* add temporal entry                   '
      endif
      IF(ITRC.EQ.0)THEN
        ITEM(MITEM-4)='r Reporting >> OFF                     '
      ELSEIF(ITRC.EQ.1)THEN
        ITEM(MITEM-4)='r Reporting >> minimal                 '
      ELSEIF(ITRC.EQ.2)THEN
        ITEM(MITEM-4)='r Reporting >> verbose                 '
      ENDIF
      ITEM(MITEM-3)  ='! Preferences                          '
      ITEM(MITEM-2)  ='> Save|import|export|extract TDF file  '
      ITEM(MITEM-1)  ='? Help                                 '
      ITEM(MITEM)    ='- Exit menu                            '


C Help text for this menu.
      helptopic='tdf_overview_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Main tdf menu.
      CALL EMENU('  Temporal definitions',ITEM,MITEM,INO)
      IF(INO.EQ.1)THEN

C Define TDF file name then read the header information should be 
C accompanied by refresh of the gereric arrays GTYPES,GMENUS
        helptopic='tdf_file_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        DFILE='tdf.dba'
        write(tfile,'(a)') LTDFA(1:lnblnk(LTDFA))
        CALL EASKS(tfile,' Temporal file name? ',
     &   ' ',96,DFILE,'TDF file name',IER,nbhelp)

C Check if the file exists and if it does use supplyandcheck
C to see if it is binary (make an ascii version) or ascii (make
C a scratch file). If it does not exist offer to create one.
        if(tfile(1:2).ne.'  '.and.tfile(1:4).ne.'UNKN')then
          INQUIRE (FILE=tfile,EXIST=XST)
          if(XST)then
            IER=0
            call supplyandcheck(tfile,'T',ier)
          else

C File not found so offer option to create a minimal file.
            WRITE(OUTS,'(2X,A,A)') tfile(1:lnblnk(tfile)),' not found!'
            dok=.true.
            CALL ASKOK(outs,' Create a new scratch & asci file?',
     &        OK,dok,nbhelp)
            IF(.NOT.OK) goto 3
            call isunix(unixok)
            if(unixok)then
              fs = char(47)
            else
              fs = char(92)
            endif
            if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
              WRITE(LTMPFL,'(2a)')cfgroot(1:lnblnk(cfgroot)),
     &          '.temporalscratch'
            else
              WRITE(LTMPFL,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &          cfgroot(1:lnblnk(cfgroot)),'.temporalscratch'
            endif
            call st2file(LTMPFL,LTDF)
            ITWPR=20
            NWPR=ITWPR-1
            NUWPR=0
            call EFOPRAN(iutdf,LTDF,ITWPR,3,IER)
            WRITE(OUTS,'(A,A)')' Opened ',LTDF(1:lnblnk(LTDF))       
            CALL EDISP(IUOUT,OUTS)
            OPTDF=.TRUE.
            call usrmsg(' ','Updating the scratch file...','-')
            call ewtdf(ier)
            call usrmsg(' ','Updating the scratch file...done.','-')

C Write out an equivalent ascii file.
            call usrmsg(' ','Updating the ascii file...','-')
            write(ltdfa,'(a)') tfile(1:lnblnk(tfile))
            call ewtdfa(itrc,ltdfa,iutdfa,0,ier)
            if(ier.eq.0)then
              write(msgl2,'(2a)') ltdfa(1:lnblnk(ltdfa)),
     &          ' with your model.'
              call usrmsg(
     &          'An ascii version has been created & you should use',
     &          msgl2,'W')
            endif
          endif
        endif
        if(NITDF.eq.0)then
          CALL EASKAB(' Empty file. Options:',' ',
     &     'setup initial data','continue',IRT,nbhelp)
          if(IRT.eq.1)then
           call glbset('init')
          endif
        endif
      ELSEIF(INO.EQ.2.or.INO.eq.3)THEN

C Overall documentation.
        call glbset('aide')
      ELSEIF(INO.EQ.4)THEN

C Define the beginning and ending dates for the TDF file.
        call glbset('date')
      ELSEIF(INO.EQ.5)then

C Define the number of timestep per hour.
        call glbset('tsph')
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(MITEM-MCTL+1))THEN

C No action pending pull up details of item for editing. Save current
C menu status. Also read in the generic arrays which match this instance
C and recover it`s layout variables via GENDAT.
        ifoc=ikeybk(ino)
        if(ifoc.gt.0)then
          CALL EPMENSV
          CALL GENDAT(TTYPE(IFOC),IFOC,FOUND,IER)
          CALL EDTITEM(ITRC,IFOC,IER)
          CALL EPMENRC
        endif
        msgl2=' Problem detected...'
        IF(IER.EQ.1)CALL USRMSG(' ',msgl2,'-')
        INO=-4
        GOTO 3
      ELSEIF(INO.EQ.MITEM)THEN

C Exit gracefully from menu.
C Free the scratch file. If the file name ends with temporalscratch remove it.
        CALL ERPFREE(IUTDF,ISTAT)
        lnts=lnblnk(LTDF)
        if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
          call usrmsg('Deleting scratch file...',LTDF,'P')
          call EFOPRAN(iutdf,LTDF,ITWPR,1,IER)

C << for some reason efdelet does not actually remove the file?? >>
          if(ier.eq.0)then
            CALL EFDELET(iutdf,ISTAT)
            call usrmsg('Deleting scratch file...done.',LTDF,'P')
          endif
        endif
        menuchw = lastmenuchw
        if(MMOD.EQ.8)then
          call winclr
          CALL feedbox(menuchw,2,igfw,igfh)
          CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)

C Setup and pass in parameters to win3d.
          iiw1=4; iiw2=1; iiw3=1; iiw4=3; iimenu=menuchw
          iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
          CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &      iigl,iigr,iigt,iigb,iigw,iigwh)
          igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
          igw=int(iigw); igwh=int(iigwh)
          if(iglib.eq.1)then
            call opencpw
            call opensetup
            numberofzones=0
            call updwire(numberofzones)  ! pass local integer
            iicapture=1; iiazi=1
            call updcapt(iicapture)
            call updazi(iiazi)
          elseif(iglib.eq.2)then
            continue
          endif
        endif
        return
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help about temporal facility.
        helptopic='tdf_overview_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('tdf opening',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Save TDF data. May have to move to the last record before exiting.
        helptopic='tdf_save_data'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRE=1
        CALL EASKATOG(' File options:',' ','save ascii file',
     &   'ascii temporal>binary','binary>ascii temporal',
     &   'extract data','continue',' ',' ',IRE,nbhelp)
        if(ire.eq.1)then
          CALL EWTDF(IER)
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,' saved current static data')

C Double check the actual number of columns of data (itnwpr) in case we can
C save the asci file more compactly as well as the last column that
C is referenced (itnwprc) by any of the items (some older files might have a
C column of zeros after the time column).
          itnwpr=0
          itnwprc=0
          do 201 L=1,NITDF
            itnwpr = itnwpr + NTBITS(L)
            do 202 LL=1,NTBITS(L)
              if(ITCOL(L,LL).gt.itnwprc) itnwprc=ITCOL(L,LL)
  202       continue
  201     continue
          if(itnwprc.gt.itnwpr) itnwpr=itnwprc
          if(itnwpr.lt.nuwpr)then
            idif=itnwpr-nuwpr

C Debug.
C            write(6,*) 'compacting from ',nuwpr,' to ',itnwpr

            call ewtdfa(itrc,ltdfa,iutdfa,idif,ier)
          else
            call ewtdfa(itrc,ltdfa,iutdfa,0,ier)
          endif
          if(ier.eq.0)then
            write(msgl2,'(2a)') ltdfa(1:lnblnk(ltdfa)),
     &        ' for your model.'
            call usrmsg('The ascii version has also been updated ',
     &        msgl2,'W')
          endif
        elseif(ire.eq.2)then
          call usrmsg('Use option 1 to import an ascii version of the',
     &      'temporal file.','W')
          goto 44
        elseif(ire.eq.3)then
          afil='  '
          CALL EASKS(afil,' Ascii export file name? ',' ',
     &      96,LEXP,'ascii export',IER,nbhelp)
          call ewtdfa(itrc,afil,IUEXP,0,ier)
        elseif(ire.eq.4)then
          isteps=((itdedoy-itdbdoy)+1)*(NTSPH*24)
          write(outs,'(a,i4,a)')'Currently there are ',NTSPH,' ts/h. '
          call edisp(iuout,outs)
          write(outs,'(a,i4,a,i4,a,i5,a)')
     &      'and data is available from day ',
     &      itdbdoy,' to ',itdedoy,' e.g. ',isteps,' timesteps.'
          call edisp(iuout,outs)
          TIMEJD=real(itdbdoy)
          CALL EASKR(TIMEJD,' Time (as julian day) to start recovery?',
     &     '  ',real(itdbdoy),'F',real(itdedoy),'F',1.5,
     &     'time=decimal day',IER,nbhelp)
          incr=1
          CALL EASKI(incr,' Number of steps to recover ',' ? ',
     &      1,'F',10000,'F',1,'steps to recover',IER,nbhelp)
          CALL EASKS(afil,' Ascii export file name? ',' ',
     &      96,LEXP,'ascii export',IER,nbhelp)
          msgl2='  '
          call USRMSG('Extracting from file...',msgl2,'-')
          call extracts(itrc,afil,IUEXP,TIMEJD,incr,ier)
          call USRMSG('Extracting from file...done.',msgl2,'-')
        endif
      ELSEIF(INO.EQ.MITEM-3)THEN

C Preferences menu for temporal data.
        CALL EPMENSV
        CALL SETUPTDF(IER)
        CALL EPMENRC
      ELSEIF(INO.EQ.MITEM-4)THEN

C Set the trace verbosity within the TDF user interface.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO.EQ.MITEM-5)THEN

C Perform list management on the explore menu. Use MLMOD to get the
C action. If insert then move lower items down using logic similar to
C that of the help facility and then allow the user to select a 
C generic type for the inserted slot via __. If delete move the
C lower items up.  NOTE: some translation of IFOC may be required.
        IF(NITDF.EQ.0)THEN

C No instances created yet so do first one.
          IFOC=1
          CALL EPMENSV
          CALL ITINSRT(ITRC,IFOC,IER)
          CALL EPMENRC
         INO=-4
          GOTO 2
        ENDIF

C Manage list. Begin by creating selection list.
        IF(NITDF.GT.0)THEN
          INPIC=NITDF
          DO 101 L=1,NITDF
            WRITE(itemsel(l),'(A,1X,A)')TAG(L),TAIDE(L)(1:30)
  101     continue
        endif

        helptopic='tdf_add_an_item'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Entry options:',' ',
     &   'add entry','delete entry','copy entry',IRT,nbhelp)
        if(IRT.eq.1)then
          IFOC=NITDF+1
          CALL EPMENSV
          CALL ITINSRT(ITRC,IFOC,IER)
          CALL EPMENRC
          IF(IER.EQ.1) then
            msgl2=' Problem detected while adding item.'
            CALL USRMSG(' ',msgl2,'-')
          elseif(IER.EQ.2)then
            msgl2=' Could not expand the current file.'
            CALL USRMSG(' ',msgl2,'-')
          else
            TTAG=TAG(IFOC)
            CALL EASKS(TTAG,' ','Name for entry? ',
     &         12,'new_item','item id',IER,nbhelp)
            if(TTAG(1:2).ne.'  ')then
              TAG(IFOC)=TTAG
            endif
          endif
        elseif(IRT.eq.2)then
          CALL EPMENSV
          inpic=1
          CALL EPICKS(INPIC,itemval,' ',' Which item to delete?',
     &      50,NITDF,itemsel,' item list',IER,nbhelp)
          CALL EPMENRC
          if(inpic.eq.1)then
            ifoc=itemval(1)
            MITEM=MITEM-1
            CALL ITDEL(ITRC,IFOC,IER)
            msgl2=' Problem detected during delete.'
            IF(IER.EQ.1)CALL USRMSG(' ',msgl2,'-')
          endif
        elseif(IRT.eq.3)then

C Perform copy of static and period data from one instance to
C another (destination must already exist)..
          CALL EPMENSV
          inpic=1
          CALL EPICKS(INPIC,itemval,' ',' Which item to copy?',
     &      50,NITDF,itemsel,' item list',IER,nbhelp)
          if(inpic.eq.1)then
            ifoc=itemval(1)
          endif
          inpic=1
          CALL EPICKS(INPIC,itemval,
     &      '(destination item must exist and be the same type)',
     &      ' Which item as destination?',
     &      50,NITDF,itemsel,' item list',IER,nbhelp)
          if(inpic.eq.1)then
            ifoc2=itemval(1)
          endif
          CALL EPMENRC
          IF(IFOC.NE.0.OR.IFOC2.NE.0)THEN
            CALL ITCOPY(ITRC,IFOC,IFOC2,IER)
            msgl2=' Problem detected during copy.'
            IF(IER.EQ.1)CALL USRMSG(' ',msgl2,'-')
          ENDIF
        endif
        ILEN=NITDF
        IPACT=CREATE
        CALL EKPAGE(IPACT)
        INO=-4
        GOTO 3
      ELSEIF(INO.EQ.MITEM-6)THEN

C If there are enough items to warrent it, allow paging control - first
C get the action via EKPAGE then re-establish top of page key character
C and array index.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

      ELSE
C Not one of the legal menu choices.
        INO=-1
        goto 2
      ENDIF
      INO=-4
      goto 2

      END

C ************** CLRTABC
C CLRTABC clears temporal commons TDFFH, tdset, tdaid,GDPER.
      SUBROUTINE CLRTABC
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

      character tdaide*72

C Initial temporal defaults.
      tdaide(1)='- '
      tdaide(2)='- '
      itdyear=2007
      itdbdoy=1
      itdedoy=1
      igbdoy=itdbdoy
      igedoy=itdedoy
      NTSPH=1
      NEXTRC=1

C In a new scratch file, data can start in column 1
      NEXTCL=1

C LSTREC is the last recored used in the TDF scratch file.
      LSTREC=1

C NDBSPT is the initial number of TABU timesteps.
C Set record width to fixed maximum.
      NWPR=MTABC
      NDBSTP=NTSPH*24*(itdedoy-itdbdoy+1)

      RETURN

      END

C ********************* glbset
C glbset: Global questions about TDF file. Foc is the topic.
C If foc='init' do initial setup. 
      subroutine glbset(foc)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/TRC/ITRC
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

      character foc*4,TTAG*12
      CHARACTER*72 tdaide,tmp72

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='glbset'  ! set for subroutine
      
      if(foc.eq.'init'.or.foc.eq.'date')then

C Define the beginning and ending dates for the TDF file.
        helptopic='tdf_global_questions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(foc.eq.'init')CALL PHELPD('setup period',nbhelp,'-',0,0,IER)
        CALL EASKI(itdyear,' ',' What is the year?',
     &             1900,'W',2051,'W',2007,'tdf year',IER,nbhelp)

        CALL EASKPER('Period:',itdbdoy,itdedoy,IFDAY,IER)

C Need to update the total number of timesteps in the file.
C Note this will affect the tabular data!
        NDBSTP=NTSPH*24*(itdedoy-itdbdoy+1)
      endif
      if(foc.eq.'init'.or.foc.eq.'tsph')then

C Define the number of timestep per hour.
        helptopic='tdf_global_frequency'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(NTSPH,' ','No of timesteps/hour ? ',
     &     1,'F',60,'F',1,'no of timesteps/hour',IER,nbhelp)
        NDBSTP=NTSPH*24*(itdedoy-itdbdoy+1)
        igbdoy=itdbdoy
        igedoy=itdedoy
      endif
      if(foc.eq.'init'.or.foc.eq.'aide')then

C Allow user to imput up to 72 char describing this file. (Print out
C the entire text first.)
        tmp72=tdaide(1)
        CALL EASKS(tmp72,' 1st line of documentation ?',
     &    ' ',72,'no comment','aide',IER,nbhelp)
        tdaide(1)=tmp72
        tmp72=tdaide(2)
        CALL EASKS(tmp72,' 2nd line of documentation ?',
     &    ' ',72,'no comment','aide',IER,nbhelp)
        tdaide(2)=tmp72
      endif
      if(foc.eq.'init') then

C Set up initial temporal data set.
        IFOC=NITDF+1
        CALL ITINSRT(ITRC,IFOC,IER)
        IF(IER.EQ.1)CALL USRMSG(' ',' Problem detected...','-')

C Ask for entry name.
        TTAG=TAG(IFOC)
        CALL EASKS(TTAG,' ','Name for entry? ',
     &     12,'new_item','item id',IER,nbhelp)
        if(TTAG(1:2).ne.'  ')then
          TAG(IFOC)=TTAG
        endif
      endif

      return
      end

C ************* extracts 
C extracts writes an ascii exchange file with a header and
C selected header information along with incr timesteps
C of data beginning at TIMEJD. ITRC is the trace level, 
C IER=0 OK, IER=1 problem. 
C This facility supports the esp-r module b2e. 
      SUBROUTINE extracts(ITRC,AFIL,IAFIL,TIMEJD,incr,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/HDAT/IHREC(MIT)
      LOGICAL CLOSE,found

      CHARACTER AFIL*72,FSTR*80,HDR*12
      CHARACTER tab*1,outs*124,msgl2*48
      CHARACTER*5 DESCRH,DESCRD

      tab=','
      HDR  ='TDF exchange'
      noofstep = incr

C If not already opened free unit and check if file exists.
      IER=0
      CALL ERPFREE(iafil,ISTAT)
      CALL EFOPSEQ(iafil,AFIL,4,IER)
      IF(ier.ne.0)THEN
        msgl2=' error creating export file.'
        CALL USRMSG(' ',msgl2,'W')
        IER=1
        RETURN
      ENDIF

C Free up pointer to start of each item header.
      NDATA=NITDF

C Write the header information.
      IREC=1
      WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)HDR
      WRITE(iafil,'(a,a1,a)',IOSTAT=ios,ERR=101)'*task',tab,'extract'
      WRITE(iafil,'(a,a1,i2)',IOSTAT=ios,ERR=101)'*stepperhour',tab,
     &  NTSPH
      WRITE(iafil,'(a,a1,F9.5)',IOSTAT=ios,ERR=101)'*starting',tab,
     &  TIMEJD
      WRITE(iafil,'(a,a1,i3)',IOSTAT=ios,ERR=101)'*noofstep',tab,
     &  noofstep

C Count up number of fields.
      itfld=0
      do 9 I=1,NDATA
        IFIELD=NTSTAT(I)+NTBITS(I)
        DO 59 J=1,IFIELD
          IC=ITCOL(I,J)
          if(IC.gt.0)itfld=itfld+1
  59    continue
   9  continue
      WRITE(iafil,'(a,a1,i4)',IOSTAT=ios,ERR=101)'*nooffield',tab,itfld

C For each item write its attributes.
      WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)'# column,description,item'
      IREC=4
      DO 10 I=1,NDATA
        IREC=IREC+1
        IHREC(I)=IREC
        IFIELD=NTSTAT(I)+NTBITS(I)

C For each of the fields build up text strings which will hold the data.
C This is done so that the ordering of the fields and their variant 
C record structures can be accommodated.
        DO 60 J=1,IFIELD
          IC=ITCOL(I,J)
          IF(ITDFTR(I,J).EQ.1)THEN
            WRITE(FSTR,62)IC,tab,TDFDES(I,J),tab,TAG(I)
   62       FORMAT(I2,a1,a,a1,a)
          ELSEIF(ITDFTR(I,J).EQ.2)THEN
            WRITE(FSTR,63)IC,tab,TDFDES(I,J),tab,TAG(I)
   63       FORMAT(I2,a1,a,a1,a)
          ELSEIF(ITDFTR(I,J).EQ.3)THEN
            WRITE(FSTR,64)IC,tab,TDFDES(I,J),tab,TAG(I)
   64       FORMAT(I2,a1,a,a1,a)
          ENDIF
          IREC=IREC+1
          if(IC.gt.0)WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)FSTR
  60    CONTINUE
  10  CONTINUE

C If there are tabular items, loop through each step until within
C one minute of the given time and then get the data and write.
C Take into account NTSPH when dealing with tollerance below.
      IF(NITDF.gt.0)THEN
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)'*data'
        found=.FALSE.
        DO 46 IP=1,NDBSTP
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          if(ITRC.gt.1)then
            write(outs,*)'cday itime rdoty',cday,itime,rdoty
            call edisp(iuout,outs)
          endif

C If RDOTY is greater than TIMEJD then set close TRUE, otherwise
C check if RDOTY and TIMEJD are close.
          if(.NOT.found)then
            if(RDOTY.gt.TIMEJD)then
              close=.true.
            else
              if(NTSPH.le.6)then
                call eclose(RDOTY,TIMEJD,0.0006,close)
              else
                call eclose(RDOTY,TIMEJD,0.00034,close)
              endif
            endif
            if(CLOSE)then
              ITREC=IP+49
              CALL ERTABU(ITRC,ITREC,IER)
              CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
              write(outs,'(a,F10.6,3a)') 'Extracting data for ',
     &          RDOTY,' (@ ',DESCRH,')'
              if(ITRC.gt.1)call edisp(iuout,outs)
              CALL EWTABA(ITREC,RDOTY,iafil,IER)
              IF(IER.EQ.1)then
                RETURN
              else
                found=.TRUE.
                incrdone=1
              endif
            endif
          else

C Write subsequent records if requested.
            if(incrdone.lt.incr)then
              ITREC=IP+49
              CALL ERTABU(ITRC,ITREC,IER)
              CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
              write(outs,'(a,F10.6,3a)') 'Extracting data for ',
     &          RDOTY,' (@ ',DESCRH,')'
              if(ITRC.gt.1)call edisp(iuout,outs)
              CALL EWTABA(ITREC,RDOTY,iafil,IER)
              IF(IER.EQ.1)then
                RETURN
              else
                incrdone=incrdone+1
              endif
            endif
          endif
   46   CONTINUE
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=102) '*end_data'
      endif
  99  CALL ERPFREE(iafil,ISTAT)
      RETURN

C Export file write errors.
 101  msgl2='  '
      CALL USRMSG('Could not write exchange header',msgl2,'W')
      IER=1
      goto 99
 102  msgl2='  '
      CALL USRMSG('Could not write exchange header record 1',msgl2,'W')
      IER=1
      goto 99
 103  msgl2='  '
      CALL USRMSG('Could not write exchange item data.',msgl2,'W')
      IER=1
      goto 99

      END

C ********************* SETUPTDF 
C SETUPTDF provides the menu's and control logic for the temporal
C data preference menu. This source used to be found in esrutdf/setup.F
      SUBROUTINE SETUPTDF(IER)

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      CHARACTER LIMP*96,LEXP*96
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
      LOGICAL XST

      DIMENSION SETUPM(7)
      CHARACTER SETUPM*50,DFILE*96,tfile*96
      integer NITMS,ISETUP ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub=' SETUPTDF'  ! set for subroutine

C Present the preference menu, ISETUP is the menu index returned. Generate
C the menu text from current common block into via UPSET.
    2 IER=0
      ISETUP=-2

C Import file.
      IF(ITIMP.EQ.0)THEN
        WRITE(SETUPM(1),'(A,A20)')'h Import file: UNKNWN Name: ',
     &        LIMP(1:20)
      ELSEIF(ITIMP.EQ.1)THEN
        WRITE(SETUPM(1),'(A,A20)')'h Import column text  Name: ',
     &        LIMP(1:20)
      ENDIF

C Export file.
      IF(ITEXP.EQ.0)THEN
        WRITE(SETUPM(2),'(A,A20)')'i Export type: UNKNWN Name: ',
     &        LEXP(1:20)
      ELSEIF(ITEXP.EQ.1)THEN
        WRITE(SETUPM(2),'(A,A20)')'i Export ASCII temporal Name: ',
     &        LEXP(1:19)
      ENDIF
      SETUPM(3)  ='  _____________________________ '

C Date display format.
      IF(IFDAY.EQ.0)THEN
        SETUPM(4)='j Display format for date>> DOY  10             '
      ELSEIF(IFDAY.EQ.1)THEN
        SETUPM(4)='j Display format for date>> 10 Jan              '
      ELSEIF(IFDAY.EQ.2)THEN
        SETUPM(4)='j Display format for date>> Fri 10 Jan          '
      ENDIF

C Display of time of day.
      IF(IFTIME.EQ.0)THEN
        SETUPM(5)='k Display format for time>> 10h30               '
      ELSEIF(IFTIME.EQ.1)THEN
        SETUPM(5)='k Display format for time>> 10.50               '
      ELSEIF(IFTIME.EQ.2)THEN
        SETUPM(5)='k Display format for time>> 0.4166 (day frac)   '
      ENDIF

      SETUPM(6)=  '? Help                                          '
      SETUPM(7)=  '- Exit ^Main Menu^                              '

C Help text for this menu.
      helptopic='tdf_inport_options'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      NITMS=7
      CALL EMENU('Preferences',SETUPM,NITMS,ISETUP)

      IF(ISETUP.EQ.1)THEN

C Define a standard import file name & format for use by other parts of TDF.
        CALL EASKABC('Import options:',' ',
     &    'UNKNOWN','ASCII column text','continue',IWM,nbhelp)
        if(IWM.ge.3)goto 2
        ITIMP=IWM-1

        DFILE='data.imp'
        write(tfile,'(a)') LIMP(1:lnblnk(LIMP))
        CALL EASKS(tfile,' Import file name? ',
     &   ' ',96,DFILE,'import file name',IER,nbhelp)
        if(tfile(1:2).ne.'  ')then
          write(LIMP,'(a)') tfile(1:lnblnk(tfile))
        endif
        IF(ITIMP.EQ.1)THEN
          CALL ERPFREE(IUIMP,ISTAT)
          call findfil(LIMP,XST)
          IF(XST)THEN
            CALL USRMSG(' this file exists',' ','-')
          ELSE
            CALL USRMSG(' this file will be created',' ','-')
          ENDIF
        ENDIF
      ELSEIF(ISETUP.EQ.2)THEN

C Define a standard export file name and format for use by other parts
C of TDF.  Ask for the type then see if it exists.
        CALL EASKI(ITEXP,' Several export formats are available: ',
     &    '  0)UNKNOWN, 1)ASCII, 2)BINARY, 3)GRTOOL ? ',
     &       0,'F',3,'F',1,'export format',IER,nbhelp)

        DFILE='data.exp'
        write(tfile,'(a)') LEXP(1:lnblnk(LEXP))
        CALL EASKS(tfile,' Export file name? ',
     &   ' ',96,DFILE,'export file name',IER,nbhelp)
        if(tfile(1:2).ne.'  ')then
          write(LEXP,'(a)') tfile(1:lnblnk(tfile))
        endif

        IF(ITEXP.EQ.1)THEN
          CALL ERPFREE(IUEXP,ISTAT)
          call findfil(LEXP,XST)
          IF(XST)THEN
            CALL USRMSG(' export file exists ',' ','-')
          ELSE
            CALL USRMSG(' new export file will be opened',' ','-')
          ENDIF
        ENDIF
      ELSEIF(ISETUP.EQ.4)THEN

C Allow user to toggle between 'Fri 10 Jan'/'10 Jan'/'DOY 124' format.
        IFDAY=IFDAY+1
        IF(IFDAY.GT.2)IFDAY=0
      ELSEIF(ISETUP.EQ.5)THEN

C Allow user to toggle between '10h00'/'10.00'/'0.41666' format. Only
C allow decimal day representation if day format is DOY
        IFTIME=IFTIME+1
        IF(IFDAY.EQ.0.AND.IFTIME.GT.2)IFTIME=0
        IF(IFDAY.GE.1.AND.IFTIME.GT.1)IFTIME=0
      ELSEIF(ISETUP.EQ.6)THEN

C Respond to help requests.
        helptopic='tdf_inport_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('tdf preferences',nbhelp,'-',0,0,IER)
      ELSEIF(ISETUP.EQ.7)THEN
        RETURN
      ELSE

C Not one of the legal menu choices.
        ISETUP=-1
        goto 2
      ENDIF
      ISETUP=-4
      goto  2

      END

