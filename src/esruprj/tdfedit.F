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

C tdfedit.F includes high level subroutines for access to
C   temporal file data.

C tdfedit: is the controlling subroutine for access to
C   temporal file data. 
C CLRTABC: clears temporal commons TDFFH, tdset, tdaid,GDPER.
C glbset(foc): global questions about TDF file. Foc is the topic.
C EXTRACTS: writes an ascii exchange file beginning at TIMEJD.
C TDFUPD: updates TDF file from an exchange file.

      subroutine tdfedit(act,ier)
#include "epara.h"
#include "tdf2.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

C The following parameters are defined as integers in epara.h:
C CREATE,PREV,NEXT,EDIT,LIST,NONE

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
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

C Logical flags for file openings.
      COMMON/FOPEN/OPTDF,OPIMP,OPEXP
      
      LOGICAL      OPTDF,OPIMP,OPEXP,OK,dok
      logical FOUND,XST,unixok,silent

      DIMENSION ITEM(30),ikeybk(30),itemsel(MIT),itemval(MIT)
      CHARACTER LTDF*72,LTDFA*72,LIMP*72,LEXP*72,H*72,tdaide*72
      character afil*72,tfile*72,TTAG*12
      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36,LTMPFL*72
      CHARACTER ITEM*50,itemsel*50,KEY*1,FOCUS*8,DFILE*72
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      character path*72,outs*124,fs*1
      character act*16,actf*72,action*16,msgl2*124

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

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
          CALL win3d(menuchw,9,12,7,3,igl,igr,igt,igb,igw,igwh)
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

      H(1)='The ESRU Temporal Definition Facility TDF supports the'
      H(2)='creation and management of temporal data from test'
      H(3)='cells, experiments or 3rd party software.  '
      H(4)=' '
      H(5)='TDF can also hold scheduling specifications for short'
      H(6)='timestep casual gains, air movement and boundary '
      H(7)='conditions for use by the ESP-r suite. It is also useful'
      H(8)='for data that changes at each timestep or in ways that'
      H(9)='are difficult for normal schedules. '
      H(10)=' '
      H(11)='Preferences : file names & types, periods, import/export'
      H(12)='             files & date display.'
      H(13)='Save : Update the TDF file and/or convert between ascii.'
      H(14)='       and binaray versions. '
      H(15)=' '
      H(16)='Currently the following item types are offered: '
      H(17)=' ALLCLMT - all climate data (includes next 6 items)'
      H(18)=' BDTEXTT - ambient temperature'
      H(19)=' WINDVLT - wind velocity'
      H(20)=' WINDDRT - wind direction'
      H(21)=' RELHUMT - ambient relative humidity'
      H(22)=' DIFHSLT - diffuse horizonal radiation'
      H(23)=' GLOHSLT - global horizonal radiation'
      H(24)=' DIRSOLT - direct normal solar radiation'
      H(25)=' SETPTTT - control setpoint temperature'
      H(26)=' CASUALT - zone casual gains (combined gains)'
      H(27)=' CASUAL3T -zone casual gains (Occup/Lights/SM Pwr)'
      H(28)=' ACTIVITY -global database: gains,setpoints, etc.'
      H(29)=' BIDIRSET -alternate bidirectional datasets'
      H(30)=' '
      H(31)='To create a new file, select option 1 and follow the'
      H(32)='instructions given. You will be asked for the year '        
      H(33)='and the period and the timesteps per hour and then '        
      H(34)='for an initial temporal type. '        
      H(35)=' '        
      H(36)='Insert by selecting an insertion point, the list will'
      H(37)='expand and you can select a generic type. '
      H(38)=' '        
      H(39)='Delete by selecting a slot and the list will contract.'
      H(40)=' '        
      H(41)='Copy by selecting source and then destination.'
      H(42)='The destination data will be overwritten - so you need'
      H(43)='to have created a destination item to overwrite before'        
      H(44)='you ask for a copy. '
      H(45)=' '
      H(46)='The TDF file is the main temporal file.'
      H(47)='The period shown is related to all tabular data'
      H(48)='and includes any `startup` time required by the'
      H(49)='simulation.  Example: for a simulation at half hour '
      H(50)='intervals over 10 days and with 2 startup days the '
      H(51)='temporal file should include 288 timesteps. '
      h(52)=' '
      h(53)='Note on file headers:'
      h(54)='Older asci temporal files may start with `TDFdatabase2`'
      h(55)='before using them please change 1st line to use the'
      h(56)='identifier ASCIITDF2'
      h(57)='Newer ascii files will start with `ASCIITDF3`.'
      h(58)='Also note that binary/scratch temporal files are'
      h(59)='machine dependant and are not always safe for use on'
      h(60)='another computer.'

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
          CALL win3d(menuchw,4,1,1,3,igl,igr,igt,igb,igw,igwh)
          if(iglib.eq.1)then
            call opencpw
            call opensetup
            call updwire(0)
            call updcapt(1)
            call updazi(1)
          elseif(iglib.eq.2)then
            continue
          endif
        endif
        return
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help about temporal facility.
        CALL PHELPD('tdf opening',60,'-',0,0,IER)
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

C Preferences menu for temporal data.
        CALL SETUPTDF(IER)
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
#include "tdf2.h"
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
#include "tdf2.h"

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

C ************* extracts 
C extracts writes an ascii exchange file with a header and
C selected header information along with incr timesteps
C of data beginning at TIMEJD. ITRC is the trace level, 
C IER=0 OK, IER=1 problem. 
C This facility supports the esp-r module b2e. 
      SUBROUTINE extracts(ITRC,AFIL,IAFIL,TIMEJD,incr,IER)
#include "tdf2.h"

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

C ********************* SETUPTDF 
C SETUPTDF provides the menu's and control logic for the temporal
C data preference menu. This source used to be found in esrutdf/setup.F
      SUBROUTINE SETUPTDF(IER)

      common/pophelp/h(60)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/TDFIO/LIMP,IUIMP,ITIMP,LEXP,IUEXP,ITEXP
      LOGICAL XST

      DIMENSION SETUPM(7)
      CHARACTER*72 LIMP,LEXP
      CHARACTER SETUPM*50,H*72,DFILE*72,tfile*72

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
      H(1)='The import facility is available to allow users with '
      H(2)='tabular data in a text file to select one or more '
      H(3)='columns of such information and associate it with a '
      H(4)='DB item which is of type TABULAR.  First define the '
      H(5)='import file and its format in this menu, then select '
      H(6)='the item which is to be filled and issue an import  '
      H(7)='command. '
      H(8)=' '
      H(9)='The export command... '
      H(10)=' '
      H(11)='The date and time formats may be used to alter the '
      H(12)='display as well as that of any exported data. '

C Menu control.
      CALL EMENU('Preferences',SETUPM,7,ISETUP)

      IF(ISETUP.EQ.1)THEN

C Define a standard import file name & format for use by other parts of TDF.
        H(1)='It is possible to import temporal information via an'
        H(2)='ASCII file with columns of data.'
        CALL EASKABC('Import options:',' ',
     &    'UNKNOWN','ASCII column text','continue',IWM,2)
        if(IWM.ge.3)goto 2
        ITIMP=IWM-1

        H(1)='One import source file will be assumed throughout this'
        H(2)='session. If this needs to be altered then the user'
        H(3)='should return to this menu to give the new name.'
        DFILE='data.imp'
        tfile=LIMP
        CALL EASKS(tfile,' Import file name? ',
     &   ' ',72,DFILE,'import file name',IER,3)
        if(tfile(1:2).ne.'  ')LIMP=tfile
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
        H(1)='It is possible to export information from the db via'
        H(2)='a number of file formats (ASCII currently available).'
        CALL EASKI(ITEXP,' Several export formats are available: ',
     &    '  0)UNKNOWN, 1)ASCII, 2)BINARY, 3)GRTOOL ? ',
     &       0,'F',3,'F',1,'export format',IER,2)

        H(1)='One export file will be used as the default name which'
        H(2)='the user may override.  Return to this menu to change'
        H(3)='the default name.'
        DFILE='data.exp'
        tfile=LEXP
        CALL EASKS(tfile,' Export file name? ',
     &   ' ',72,DFILE,'export file name',IER,3)
        if(tfile(1:2).ne.'  ')LEXP=tfile

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
        CALL PHELPD('tdf preferences',12,'-',0,0,IER)
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

