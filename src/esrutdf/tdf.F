C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-6.

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

C MAIN TDF.F
C The TDF program provides facilities to create and edit system level
C timestep data which is compatible with the ESP-r suite of programs.  It
C makes use of library functions written by the staff of ESRU, Faculty
C of Engineering, University of Strathclyde.

C CLRTABC: clears temporal commons TDFFH, tdset, tdaid,GDPER.
C glbset(foc): global questions about TDF file. Foc is the topic.
C EXTRACTS: writes an ascii exchange file beginning at TIMEJD.
C TDFUPD: updates TDF file from an exchange file.

      program tdf
      INCLUDE 'epara.h'
      INCLUDE 'tdf2.h'

C The following parameters are defined as integers in epara.h:
C CREATE,PREV,NEXT,EDIT,LIST,NONE

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTPCH/ICOUT
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      common/appw/iappw,iappx,iappy
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/tdset/itdyear,itdbdoy,itdedoy
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

C Common blocks unique to TDF:
C Related to TDF ITEMS db.
C ITDFLG is zero if no file, one if older version, two if pre-2005
C        and three if 2005 version.
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA

C Related to TDF header.
C NWPR  : Number of words per record in the TAB file.
C NITDF : Total number of instances in the TDF db.
C NTSPH : Number of time steps per hour, 1 to 60 allowed.
C NEXTCL: Next column available in tabular file.
C NEXTRC: Next record available in << >>
C LASTHD: Last record used by the TDF header section.
C NDBSTP: Total number of timesteps during the db tabular period.
C NUWPR : Current number of data columns used (not counting time column)
C         i.e. this is the sum of ntbits() for current contents.
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR

      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)

C Related to inport and export files.
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP

C Path to problem
      common/rpath/path
      common/uhome/upath
      common/udot/esprc

C Logical flags for file openings.
      COMMON/FOPEN/OPTDF,OPIMP,OPEXP
      LOGICAL      OPTDF,OPIMP,OPEXP,OK,dok
      logical FOUND,XST,unixok,silent

      DIMENSION ITEM(30),ikeybk(30),itemsel(MIT),itemval(MIT)
      CHARACTER LTDF*72,LTDFA*72,LIMP*72,LEXP*72,H*72,tdaide*72
      character afil*72,tfile*72,TTAG*12
      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36,LTMPFL*72
      CHARACTER ITEM*50,itemsel*50,inf*144,KEY*1,FOCUS*8,DFILE*72
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      character path*72,upath*72,esprc*72,outs*124,fs*1
      character act*16,actf*72,action*16,msgl2*124,outs248*248

C Assume defaults: trace off (ITRC=0), date format is DOY,
C time format is 10h00.
      call ezero
      call curmodule('tdf ')
      IUOUT=6
      IUIN=5
      ICOUT=0
      LIMTTY=24
      LIMIT =24
      IFIL=11

C For debugging set itrc =2
      ITRC=1
      IFDAY=1
      IFTIME=1

C Fonts
      IFS=1
      ITFS=0
      IMFS=1
      call parsfact(MODL,iappw,iappx,iappy,inf,act,actf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

C Clear temporal commons TDFFH, tdset, tdaid,GDPER.
      call clrtabc

C File Units, default names & types as follows: 
C TDF binary (scratch) file: IUTDF=unit, LTDF=file name (72 char), 
C TDF ascii temporal file LTDFA (72 char) on unit IUTDFA.
      afil='  '
      LTDF='UNKNOWN'
      LTDFA='UNKNOWN'
      IUTDF=IFIL
      IUTDFA=IFIL+3
      OPTDF=.FALSE.

C ASCII file to import data from: IUIMP=unit, LIMP=file name.
C Import file unknown (ITIMP=0), ASCII column listing (ITIMP=1).
      IUIMP=IFIL+5 
      LIMP ='tdf.import' 
      ITIMP=1
      OPIMP=.FALSE.

C ASCII export file: IUEXP=unit, LEXP=file name.
C Export file unknown (ITEXP=0), ASCII column listing (ITEXP=1).
      IUEXP=IFIL+6
      LEXP ='tdf.export' 
      ITEXP=1
      OPEXP=.FALSE.

C Instanciate the standard temporal entities.
      FOCUS='ALL'
      CALL GENDAT(FOCUS,0,FOUND,IER)

C Initialise terminal and begin program. Note the tdf module graphic
C layout is slightly wider than other ESP-r modules so as to deal
C with more columns of timestep data.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappwi= 840
        iapphi= 550
        iappx=20
        iappy=30
      else
        if(iappx.le.0)iappx=20
        if(iappy.le.0)iappy=30
        if(iappw.le.200)then
          iappwi=int(840*iappw*0.01)
          iapphi=int(550*iappw*0.01)
        elseif(iappw.gt.200)then
          continue
        endif
      endif

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(53*iappw*0.01),28)
        LIMTTY=8
        LIMIT =8
      else
        menuchw = 53
        LIMTTY=8
        LIMIT =8
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for graphs, IFS is for dialog & text feedback).
        IMFS=1
        IFS=1
        ITFS=1
        call userfonts(IFS,ITFS,IMFS)
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &'ESP-r Temporal Definitions: enquiries to esru@strath.ac.uk')

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN
        CALL win3d(menuchw,9,12,7,3,igl,igr,igt,igb,igw,igwh)
#ifdef X11
        call opencpw
        call opentutorial
        call opensetup
#endif
        call setzscale()
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      IF(MMOD.NE.-6)ICOUT=IUOUT

      CALL EDISP(iuout,' ')
      write(outs,'(2a)')
     &  ' ESP-r Temporal data: Version 1.16b of March 2006.',
     &  ' Copyright 2001-6 Energy'
      call edisp(IUOUT,outs)
      write(outs,'(2a)')
     & ' Systems Research Unit, University of',
     & ' Strathclyde, Glasgow Scotland.'
      call edisp(IUOUT,outs)
      call edisp(IUOUT,' ')

C Find the user's home folder then get users custom settings.
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IFIL+1,0,IIER)

C Take command line file name and save this to LTDF and LTDFA.
C << note inf is 144 char but none of the other code in tdf
C << has been converted to work with longer file names
      if(inf(1:2).ne.'  '.and.inf(1:4).ne.'UNKN')then
        linf=lnblnk(inf)
        if(linf.le.72)then
          write(LTDF,'(a)') inf(1:linf)
          write(LTDFA,'(a)') inf(1:linf)
          write(outs248,'(a,a)')' the input file is: ',inf
          call edisp(iuout,outs248)
        else
          LTDF=inf(1:72)
          LTDFA=inf(1:72)
        endif
      endif

C Take command line action parameter. If paction = 'update' then
C after the file is read in, update with data in the action file.
C << we might be passing it an ascii temporal file though and >>
C << so a conversion might be required first >>
      if(act(1:1).ne.' ')then
        action=act
        write(outs,'(a,a)')'TDF: the input action is: ',action
        call tstamp('>',outs)
        write(outs,'(a,a)')' the action is: ',action
        if(action(1:6).eq.'update')then
          call edisp(iuout,outs)
          silent=.true.
        elseif(action(1:7).eq.'extract')then
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

C If an input file has been specified then load it.
      if (LTDFA(1:4).eq.'UNKN'.or.LTDFA(1:2).eq.'  ') then
        XST=.false.
      else
        INQUIRE (FILE=LTDFA,EXIST=XST)
      endif
      if(XST)then
        IER=0
        call supplyandcheck(inf,'T',ier)
        if(ier.ne.0)then
          write(6,*) 'non-zero error after call to supplyandcheck'
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
        call epwait
        CALL EPAGEND
        STOP
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
        call epwait
        CALL EPAGEND
        STOP
      endif
      if(action(1:7).eq.'extract')then

C Extract a period of information from the scratch file into a
C text file in 'extract' format (a cut down version of ascii temporal file).
        H(1)='The selected format for the input of time is currently'
        H(2)='set as a fraction of a day 0.4271 (eq 10h15).'
        write(outs,'(a,i4,a)')'Currently there are ',NTSPH,' ts/h. '
        call edisp(iuout,outs)
        write(outs,'(a,i4,a,i4)')'and data is available from day ',
     &     itdbdoy,' to ',itdedoy
        call edisp(iuout,outs)
        TIMEJD=real(itdbdoy)
        CALL EASKR(TIMEJD,' Time (as julian day) to start recovery? ',
     &    '  ',real(itdbdoy),'F',real(itdedoy),'F',1.5,
     &    'time=decimal day',IER,2)
        incr=NDBSTP - INT(NTSPH*24*(TIMEJD-real(itdbdoy)))
        CALL EASKI(incr,' Number of steps to recover ',' ? ',
     &    1,'F',NDBSTP,'F',1,'steps to recover',IER,2)
        msgl2='  '
        call USRMSG('Extracting timestep data...',msgl2,'-')
        call extracts(itrc,actf,IUEXP,TIMEJD,incr,ier)
        write(action,'(a)') '-'
        goto 44
      endif
      if(action(1:6).eq.'update')then

C Update current temporal file with period information from a
C file in 'extract' format.
        msgl2='  '
        call USRMSG('Updating file...',msgl2,'-')
        call TDFUPD(ITRC,actf,IUIMP,IER)
        write(action,'(a)') '-'
        CALL EWTDF(IER)
        CALL ERPFREE(IUTDF,ISTAT)
        msgl2=' End of TDF'
        CALL USRMSG('Updating file...done.',msgl2,'P')
        call epwait
        CALL EPAGEND
        STOP
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
        ITEM(MITEM-5)  ='*                                      '
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
      ITEM(MITEM)    ='- Finish TDF                           '

C Use askabout to instanciate the initial help messages (2nd parameter is one).
      call askabout('tdf ',1)

C Main tdf menu.
      CALL EMENU('  Temporal definitions',ITEM,MITEM,INO)
      IF(INO.EQ.1)THEN

C Define TDF file name then read the header information should be 
C accompanied by refresh of the gereric arrays GTYPES,GMENUS
        H(1)='The TDF file is the name of an ascii file which holds'
        H(2)='the attributes and temporal (short timestep) data of '
        H(3)='casual gains, boundary conditions, flow measurements etc.'
        H(4)=' '
        h(5)='While editing temporal data in this module or using the'
        h(6)='data during a simulation or as results are being'
        h(7)='recovered, a scratch (binary) file is created. '
        h(8)=' '
        h(9)='I you supply the name of a binary version of the '
        h(10)='temporal file it will be translated into an ascii '
        h(10)='version and a new scratch file created. '
        h(11)=' '
        h(12)='Note: if you need to create a new binary temporal file'
        h(13)='from an ESP-r ascii temporal file use the command line'
        h(14)='option to do the conversion: '
        h(15)='tdf -file xx.tdf -act import -actf yy.tdf.a '
        h(16)='  where xx.tdf is the name of the new binary file to'
        h(17)='  create and yy.tdf.a is the asci temporal file to'
        h(18)='  be read in.'
        DFILE='tdf.dba'
        tfile=LTDFA
        CALL EASKS(tfile,' Temporal file name? ',
     &   ' ',72,DFILE,'TDF file name',IER,18)

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
            h(1)='No temporal file was found so a minimal file '
            h(2)='will be created if you say yes. '
            h(3)=' '
            h(4)='If you say no, you can create one later. '
            CALL ASKOK(outs,' Create a new scratch & asci file?',
     &        OK,dok,4)
            IF(.NOT.OK) goto 3
            call isunix(unixok)
            if(unixok)then
              fs = char(47)
              call esppid(ipid)
              LTMPFL=' '
              write(LTMPFL,'(2a,i7,a)') '/tmp',fs,ipid,
     &          '.temporalscratch'
            else
              fs = char(92)
              call esppid(ipid)
              LTMPFL=' '
              write(LTMPFL,'(4a,i7,a)') 'C:',fs,'temp',fs,ipid,
     &          '.temporalscratch'
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
          h(1)='Initial setup prompts you for the necessary data to'
          h(2)='setup a new temporal definition. You can also do it'
          h(3)='manually. '
          CALL EASKAB(' Empty file. Options:',' ',
     &     'setup initial data','continue',IRT,3)
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
        CALL EPMENSV
        ifoc=ikeybk(ino)
        CALL GENDAT(TTYPE(IFOC),IFOC,FOUND,IER)
        CALL EDTITEM(ITRC,IFOC,IER)
        CALL EPMENRC
        msgl2=' Problem detected...'
        IF(IER.EQ.1)CALL USRMSG(' ',msgl2,'-')
        INO=-4
        GOTO 3
      ELSEIF(INO.EQ.MITEM)THEN

C Exit gracefully from TDF.
        call usrmsg(' Closing temporal definition facility...',' ','P')

C Free the scratch file. If the file name ends with temporalscratch remove it.
        CALL ERPFREE(IUTDF,ISTAT)
        lnts=lnblnk(LTDF)
        if(LTDF(lnts-15:lnts).eq.'.temporalscratch')then
          call usrmsg('Deleting scratch file...',LTDF,'P')
          call EFOPRAN(iutdf,LTDF,ITWPR,1,IER)
          CALL EFDELET(iutdf,ISTAT)
          call usrmsg('Deleting scratch file...done.',LTDF,'P')
        endif
        CALL EPAGEND
        msgl2=' End of TDF'
        CALL USRMSG(' ',msgl2,'-')
        STOP
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help via the askabout facility in esru_ask.F The zero requests display
        call askabout('tdf ',0)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Save TDF data. May have to move to the last record before exiting.
        h(1)='You may save the current binary file or'
        h(2)='import or export a ASCII version. '
        CALL EASKATOG(' File options:',' ','save ascii file',
     &   'ascii temporal>binary','binary>ascii temporal',
     &   'extract data','continue',' ',' ',IRE,2)
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
            write(6,*) 'compacting from ',nuwpr,' to ',itnwpr
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
          h(1)='File name for ascii version of the temporal file.'
          afil='  '
          CALL EASKS(afil,' Ascii export file name? ',' ',
     &      72,LEXP,'ascii export',IER,1)
          call ewtdfa(itrc,afil,IUEXP,0,ier)
        elseif(ire.eq.4)then
          H(1)='The selected format for the input of time is currently'
          H(2)='set as a fraction of a day 0.4271 (eq 10h15).'
          write(outs,'(a,i4,a)')'Currently there are ',NTSPH,' ts/h. '
          call edisp(iuout,outs)
          write(outs,'(a,i4,a,i4)')'and data is available from day ',
     &      itdbdoy,' to ',itdedoy
          call edisp(iuout,outs)
          TIMEJD=real(itdbdoy)
          CALL EASKR(TIMEJD,' Time (as julian day) to start recovery?',
     &     '  ',real(itdbdoy),'F',real(itdedoy),'F',1.5,
     &     'time=decimal day',IER,2)
          incr=1
          CALL EASKI(incr,' Number of steps to recover ',' ? ',
     &      1,'F',100,'F',1,'steps to recover',IER,2)
          CALL EASKS(afil,' Ascii export file name? ',' ',
     &      72,LEXP,'ascii export',IER,1)
          msgl2='  '
          call USRMSG('Extracting from file...',msgl2,'-')
          call extracts(itrc,afil,IUEXP,TIMEJD,incr,ier)
          call USRMSG('Extracting from file...done.',msgl2,'-')
        endif
      ELSEIF(INO.EQ.MITEM-3)THEN

C Preferences menu.
        CALL SETUP(IER)
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
          CALL ITINSRT(ITRC,IFOC,IER)
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

        H(1)='Adding an item - placed at the end of the list. '
        H(2)='Deleting an item (irreversable) shifts subsequent '
        H(3)=' items up in the list. '
        H(4)='Copy of item will overwrite the contents of the  '
        H(5)=' destination item. '
        CALL EASKABC('Entry options:',' ',
     &   'add entry','delete entry','copy entry',IRT,5)
        if(IRT.eq.1)then
          IFOC=NITDF+1
          CALL ITINSRT(ITRC,IFOC,IER)
          IF(IER.EQ.1) then
            msgl2=' Problem detected while adding item.'
            CALL USRMSG(' ',msgl2,'-')
          elseif(IER.EQ.2)then
            msgl2=' Could not expand the current file.'
            CALL USRMSG(' ',msgl2,'-')
          else
            H(1)='In order to find items in the TDF db a unique name'
            H(2)='must be assigned to each entry. This name is a '
            H(3)='string up to 12 characters long with no blanks.'
            TTAG=TAG(IFOC)
            CALL EASKS(TTAG,' ','Name for entry? ',
     &         12,'new_item','item id',IER,3)
            if(TTAG(1:2).ne.'  ')then
              TAG(IFOC)=TTAG
            endif
          endif
        elseif(IRT.eq.2)then
          CALL EPMENSV
          H(1)='Choose an item to delete.'
          inpic=1
          CALL EPICKS(INPIC,itemval,' ',' Which item to delete?',
     &      50,NITDF,itemsel,' item list',IER,1)
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
          H(1)='Choose an item to copy. You will then be asked'
          H(2)='for a destination (which must already exist).'
          inpic=1
          CALL EPICKS(INPIC,itemval,' ',' Which item to copy?',
     &      50,NITDF,itemsel,' item list',IER,2)
          if(inpic.eq.1)then
            ifoc=itemval(1)
          endif
          H(1)='Choose a destination item.'
          inpic=1
          CALL EPICKS(INPIC,itemval,
     &      '(destination item must exist and be the same type)',
     &      ' Which item as destination?',
     &      50,NITDF,itemsel,' item list',IER,1)
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
      INCLUDE 'tdf2.h'
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      common/tdset/itdyear,itdbdoy,itdedoy
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

      character tdaide*72

C Initial temporal defaults.
      tdaide(1)='- '
      tdaide(2)='- '
      itdyear=2000
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
      INCLUDE 'tdf2.h'

      COMMON/TRC/ITRC
      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/tdset/itdyear,itdbdoy,itdedoy
      common/tdaid/tdaide(2)
      COMMON/GDPER/igbdoy,igedoy

C Related to TDF header.
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)

      character foc*4,TTAG*12,TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER*72 H,tdaide,tmp72

      if(foc.eq.'init'.or.foc.eq.'date')then

C Define the beginning and ending dates for the TDF file.
        H(1)= 'Period of the temporal file:'
        H(2)= 'The year is required. It should match that of '
        H(3)= 'the climate file used in a simulation and can hold'
        H(4)= 'data for a few days up to a year. '
        H(5)= ' '
        H(6)= 'The start day applies to all of the items in the '
        H(7)= 'file.  Example:'
        H(8)= ' '
        H(9)= ' A week long simulation in July (say 7-14th) requires'
        H(10)=' a 4 day startup period. A minumum TDF db would be'
        H(11)=' 3-14 July.'
        H(12)=' '
        H(13)='If you are planning on importing timestep data'
        H(14)='from an external file then the TDF start date '
        H(15)='includes the startup period. SORT THIS OUT BEFORE'
        H(16)='importing data (or it might be truncated).  '
        if(foc.eq.'init')CALL PHELPD('setup period',16,'-',0,0,IER)
        h(1)='Check initial help on dates. '
        CALL EASKI(itdyear,' ',' What is the year?',
     &             1900,'W',2010,'W',2000,'tdf year',IER,1)

        CALL EASKPER('Period:',itdbdoy,itdedoy,IFDAY,IER)

C Need to update the total number of timesteps in the file.
C Note this will affect the tabular data!
        NDBSTP=NTSPH*24*(itdedoy-itdbdoy+1)
      endif
      if(foc.eq.'init'.or.foc.eq.'tsph')then

C Define the number of timestep per hour.
        H(1)='Frequency of data:'
        H(2)='Timestep data in your file will be at a fixed'
        H(3)='timestep which should match that of the'
        H(4)='simulation timestep.'
        H(5)=' '
        H(6)='If you are planning on importing TABULAR (timestep) '
        H(7)='data then the TDF timestep and that of the other '
        H(8)='file should match. '
        CALL EASKI(NTSPH,' ','No of timesteps/hour ? ',
     &     1,'F',60,'F',1,'no of timesteps/hour',IER,8)
        NDBSTP=NTSPH*24*(itdedoy-itdbdoy+1)
        igbdoy=itdbdoy
        igedoy=itdedoy
      endif
      if(foc.eq.'init'.or.foc.eq.'aide')then

C Allow user to imput up to 72 char describing this file. (Print out
C the entire text first.)
        H(1)='The file holds two documentation lines.  Such '
        H(2)='documentation can save a lot of grief later.....'
        tmp72=tdaide(1)
        CALL EASKS(tmp72,' 1st line of documentation ?',
     &    ' ',72,'no comment','aide',IER,2)
        tdaide(1)=tmp72
        tmp72=tdaide(2)
        CALL EASKS(tmp72,' 2nd line of documentation ?',
     &    ' ',72,'no comment','aide',IER,2)
        tdaide(2)=tmp72
      endif
      if(foc.eq.'init') then

C Set up initial temporal data set.
        IFOC=NITDF+1
        CALL ITINSRT(ITRC,IFOC,IER)
        IF(IER.EQ.1)CALL USRMSG(' ',' Problem detected...','-')

C Ask for entry name.
        H(1)='In order to find items in the TDF db a unique name must'
        H(2)='be assigned to each entry. This name is a character'
        H(3)='string up to 12 characters long with no blanks.'
        TTAG=TAG(IFOC)
        CALL EASKS(TTAG,' ','Name for entry? ',
     &     12,'new_item','item id',IER,3)
        if(TTAG(1:2).ne.'  ')then
          TAG(IFOC)=TTAG
        endif
      endif

      return
      end

C ************* TDFUPD
C TDFUPD updates TDF file from an exchange file (similar in format
C to extract exchange file). This facility is used by the esp-r
C module b2e and the code which generates the file is in daproc.F
C as routine TDFUDWR. Only the tdf module needs routine tdfupd.
C ITRC is the trace level, IER=0 OK, IER=1 problem. 
      SUBROUTINE tdfupd(ITRC,AFIL,IAFIL,IER)
      INCLUDE 'tdf2.h'

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      common/tdset/itdyear,itdbdoy,itdedoy
      logical FOUND,CLOSE,CLOSET,foundit

      CHARACTER AFIL*72,FOCUS*8,tail*8
      character outstr*248,WORD*20,task*16,outs*124,msgl2*48
      CHARACTER*5 DESCRH,DESCRD

C If not already opened free unit and check if file exists.
      IER=0
      CALL ERPFREE(iafil,ISTAT)
      CALL EFOPSEQ(iafil,AFIL,1,IER)
      IF(ier.ne.0)THEN
        msgl2='Error opening exchange file.'
        CALL USRMSG(' ',msgl2,'W')
        IER=1
        RETURN
      ENDIF

C Proceed as if loading a standard file.
      msgl2=' Scanning supplied file...'
      call usrmsg(' ',msgl2,'-')
      FOCUS='ALL'
      CALL GENDAT(FOCUS,0,FOUND,IER)

C Might need to reset the common blocks?

C Check header of file.
      CALL LSTRIPC(iafil,outstr,99,ND,1,'header',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:12).ne.'TDF exchange')then
        call usrmsg('Format of exchange file is incorrect.',
     &    afil,'W')
        ier=1
        CALL ERPFREE(iafil,ISTAT)
        return
      endif

C Read the header information.
  34  CALL LSTRIPC(iafil,outstr,99,ND,1,'line 1',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','header tags',IFLAG)
      if(WORD(1:5).eq.'*task')then
        CALL EGETRM(OUTSTR,K,task,'W','task',IER)
        write(outs,'(a,a)') 'The task is ',task
        if(ITRC.gt.1)call edisp(iuout,outs)
        IF(IER.NE.0)goto 1
        goto 34
      elseif(WORD(1:12).eq.'*stepperhour')then
        CALL EGETWI(OUTSTR,K,INTSPH,0,0,'-','NTSPH',IER)
        if(INTSPH.ne.NTSPH)then
          msgl2='that of the current file.'
          call usrmsg('exchange file NTSPH is different from',msgl2,'W')
          ier=1
          return
        endif
        goto 34
      elseif(WORD(1:9).eq.'*starting')then
        CALL EGETWR(OUTSTR,K,STARTNG,real(itdbdoy),real(itdedoy),
     &    'W','start',IER)
        goto 34
      elseif(WORD(1:9).eq.'*noofstep')then
        CALL EGETWI(OUTSTR,K,noofstep,0,0,'-','noofstep',IER)
        goto 34
      elseif(WORD(1:10).eq.'*nooffield')then
        CALL EGETWI(OUTSTR,K,itfld,0,0,'-','nooffield',IER)

C Skip past the field descriptors.
        DO 45 IIT=1,itfld
          CALL LSTRIPC(iafil,outstr,99,ND,1,'fields',IER)
   45   CONTINUE
        goto 34
      elseif(WORD(1:5).eq.'*data')then

C Loop until the time in the db is at STARTNG and then read new
C data and write to the relevant record, repeating as necessary.
        foundit=.false.
        noread=0

C Set a tolerance on the time stamp of +/- 30% of one time increment
        toler=(1./(24*float(NTSPH)))*0.5
        DO 46 IP=1,NDBSTP
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          RDOTYP=RDOTY+toler
          RDOTYM=RDOTY-toler
          if(.NOT.foundit)then

C Check if the read record falls within the tolerance band. 
            if(startng.le.(RDOTYP).and.startng.gt.(RDOTYM))then
              close=.true.
            endif
          else
            close=.TRUE.
          endif
          if(CLOSE.and.(noread.lt.noofstep))then
            foundit=.true.
            CALL CLRTAB
            CALL LSTRIPC(iafil,outstr,99,ND,1,'tabular data',IER)
            inwpr=ND-1
            K=0
            CALL EGETWR(OUTSTR,K,TIMEJD,0.,0.,'-','TIMEJD',IER)
            do 47 j=1,inwpr
              CALL EGETWR(OUTSTR,K,TABU(J),0.0,0.0,'-','TABU',IER)
              if(TABU(J).lt.TABUMN(J))TABUMN(J)=TABU(J)
              if(TABU(J).gt.TABUMX(J))TABUMX(J)=TABU(J)
  47        continue

C Check if the read record falls within the tolerance band. 
            if(TIMEJD.le.(RDOTYP).and.TIMEJD.gt.(RDOTYM))then
              closet=.true.
            endif
            if(.NOT.closet)then
              write(outs,'(a,i5,3F9.3)')'RDOTY != TIMEJDIP',ip,RDOTY,
     &          TIMEJD,STARTNG
	      call edisp(iuout,outs)
              ier=1
              goto 99
            else
              ITREC=IP+49
              noread=noread+1
            endif
            CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
            write(outs,'(a,F4.0,2a)') 'Updating data for ',RDOTY,
     &        ' @ ',DESCRH
            if(ITRC.gt.1)call edisp(iuout,outs)
            CALL EWTABU(ITRC,ITREC,IER)
          endif
   46   CONTINUE

C Now write the maximum for each column and then the last file record.
        IREC=48
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) (TABUMX(J),J=1,NWPR)
        IREC=49
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) (TABUMN(J),J=1,NWPR)
        IREC=50+NDBSTP
        tail='ENDTDF  '
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) tail
        goto 34
      elseif(WORD(1:9).eq.'*end_data')then
         CALL ERPFREE(iafil,ISTAT)
         if(foundit)then
           call edisp(iuout,'Updating data finished.')
         else
           call edisp(iuout,'Warning: no data imported!.')
         endif 
         return
      else
        goto 34
      endif
      goto 34

  99  CALL ERPFREE(iafil,ISTAT)
      RETURN

C Export file write errors.
 1    CALL LUSRMSG('Error in importing data',OUTSTR,'W')
      IER=1
      goto 99
 103  CALL LUSRMSG('Error in writing imported data',OUTSTR,'W')
      IER=1
      goto 99

      END

C ************* extracts 
C extracts writes an ascii exchange file with a header and
C selected header information along with incr timesteps
C of data beginning at TIMEJD. ITRC is the trace level, 
C IER=0 OK, IER=1 problem. 
C This facility supports the esp-r module b2e. 
      SUBROUTINE extracts(ITRC,AFIL,IAFIL,TIMEJD,incr,IER)
      INCLUDE 'tdf2.h'

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/HDAT/IHREC(MIT)
      common/tdset/itdyear,itdbdoy,itdedoy
      LOGICAL CLOSE,found

      CHARACTER AFIL*72,FSTR*80,HDR*12
      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER TDFDES*32,tab*1,outs*124,msgl2*48
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
      IF(NITDF.gt.0)THEN
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)'*data'
        found=.FALSE.
        DO 46 IP=1,NDBSTP
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          if(ITRC.gt.1)write(6,*)'cday itime rdoty',cday,itime,rdoty
          if(.NOT.found)then
            call eclose(RDOTY,TIMEJD,0.0006,CLOSE)
            if(CLOSE)then
              ITREC=IP+49
              CALL ERTABU(ITRC,ITREC,IER)
              CALL ESTIME(NTSPH,1,ITIME,DESCRH,DESCRD,TIMER)
              write(outs,'(a,F10.4,3a)') 'Extracting data for ',
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
              write(outs,'(a,F4.0,3a)') 'Extracting data for ',
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


C ***** dummy routine for c code active descriptions buttons.
      subroutine cfgpk(act)
      character act*1

      return
      end

C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(ITRC,IER)
      ier=0
      return
      end

      subroutine chgazi(icazi)
      return
      end

      subroutine chgelev(icelev)
      return
      end

      SUBROUTINE EMKVIEW(ITRC,IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(IUF,direc,ier)
      CHARACTER direc*1
      ier=0
      return
      end
