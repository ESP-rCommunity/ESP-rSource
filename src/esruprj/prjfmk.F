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

C Subroutines in prjfmk.F
C PRJFMK: Creates and edits a zone operations file.
C EDAFCTL: Edits thermostatic air flow control description.
C EDAIR2: Edits v1.0 & v2.0 air flow portion of a zone operations file.
C EDAIR3: Edits v3.0 air flow portion of a zone operations file.
C EDAIRFL - edit scheduled air flow data.
C ADDAFP add air flow period in correct location (time based).
C CDAIRFL - copy/ delete air flow data.
C COPYAIRFL - import an air flow regime from zone isource to
C             zone idest based on afact, iflow, vflow
C CHKAIRFL - check air flow data for overlapping periods.
C PLTAIR - plot air flow data.
C EDCAS: Edits casual gains portion of a zone operations file.
C SCASL: Import a profile from a profiles database to build casual
C        gain patterns.
C EDCASGN - edit casual gain data.
C CDCASGN - copy/ delete casual gain data.
C IMPORTZCASG - import a casual gain regime from zone isource to
C               zone idest based on user instructions.
C PLTCASGN - plot casual gain data.
C Inscasper - inserts or deletes a casual gain period into the
C             current P3 & elp3 data structure.
C initcasday - does initial setup of a casual day type.
C initcasper - supports setting up initial periods for schedules.

C ********************* PRJFMK *********************
C PRJFMK: Creates and edits a zone operations file.
C ITRC is the trace level, ITRU is the ouput channel, IUO is the
C file unit for the operations file. IER=0 is OK.
C If IVER is zero then support interactive use, if IVER is
C non-zero then it has been called from the versioning
C facility just after the common blocks have been filled
C and should just write out this data.
      SUBROUTINE PRJFMK(ITRC,ITRU,IUO,ICOMP,IER,IVER)

#include "building.h"  
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      common/pophelp/h(60)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NAC,IACS,IACF,IPT,ICGT
      REAL ACI,ACV,TA
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF
      REAL CMGS,CMGL,RADC,CONC
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem

C Flag noting whether file was browsed for (so that cfg can be
C updated when the user exits).
      logical gotviabrowse

      CHARACTER tcname*248
      CHARACTER ITEM(14)*33,choices(11)*32,H*72,LTMP*72,GFILE*72
      CHARACTER*6 lodlabel
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1,outs*124
      character act*8,iwatt*8,afact*8,iflow*8,vflow*8

      LOGICAL OK,DOK,XST,MODOPR,browse,unixok,needtoask,clkok
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
       
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Initially assume periods are not sorted.
      sorted=.false.
      needtoask=.true.
      gotviabrowse=.false.

C If version facility invoked this subroutine then 
C just write operations file
      IF(IVER.NE.0)then
        ICOMP=IVER
        if(cfgok.and.(.NOT.browse))then
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
          IF(IER.LT.0)THEN
            call edisp(iuout,
     &        'Problem saving current operations to the new file.')
            call edisp(iuout,'Check file permissions and disk space.')
            IER=1
            RETURN
          endif
          RETURN
        endif
      endif

C GFILE is the default file name for any operations file.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      else
        WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.opr'
      endif

C LPROJ() might already have a name, but not yet exist. Check for this. 
      if(LPROJ(ICOMP)(1:4).eq.'UNKN'.or.LPROJ(ICOMP)(1:2).eq.'  ')then
        needtoask=.true.
        LTMP=GFILE
      else
        call FINDFIL(LPROJ(ICOMP),XST)
        IF(XST)THEN
          needtoask=.false.
          LTMP=LPROJ(ICOMP)
        ELSE
          needtoask=.true.
          LTMP=LPROJ(ICOMP)
        ENDIF
      endif

C Operations file not known, allow user to give name or browse.
      if(needtoask)then
        H(1)='The zone operations file was not found or has not'
        H(2)='yet been created. The file contains schedules of air'
        H(3)='flow and casual gains. Air flows may have thermostatic'
        H(4)='or wind speed control. If electrical details are'
        H(5)='included then casual gains and electrical loads are'
        H(6)='combined into a `zone load`.'
        H(7)=' '
        H(8)='The browse option presents a list of files in the'
        H(9)='project `zones` folder in case you want to share.'
        H(10)='the same definition as used by another zone.'

        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.2)then
          CALL EASKSCMD(LTMP,' ','Zone operations file options:',
     &      'browse',clkok,72,GFILE,'operations file',ISER,10)
          if(clkok)iclkok=1   ! notice browse button
          if(iser.eq.-3)iclkok=2  ! notice cancel button
        else
          CALL EASKS2CMD(LTMP,' ','Zone operations file options:',
     &      'browse','cancel',iclkok,72,GFILE,
     &      'operations file',IER,10)
        endif
        call usrmsg(' ',' ','-')   ! clear the display
      else
        H(1)='Operations details have already been defined for this'
        H(2)='zone so if you want to use and/or modify the current'
        H(3)='definition accept the presented file.'
        H(4)=' '
        H(5)='A zone operations file contains schedules of air flow'
        H(6)='and casual gains. Air flows may have thermostatic or'
        H(7)='wind speed control. If electrical details are included'
        H(8)='then casual gains and electrical loads are combined'
        H(9)='into a `zone load`.'
        H(10)=' '
        H(11)='The browse option presents a list of files in the'
        H(12)='project `zones` folder in case you want to associate'
        H(13)='another operations file with the current zone In that'
        H(14)='case you the file will be shared between zones so that'
        H(15)='any change to its contents will affect more than one'
        H(16)='zone.'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.2)then
          CALL EASKSCMD(LTMP,' ','Confirm zone operations file:',
     &      'browse another',clkok,72,GFILE,'operations file',ISER,16)
          if(clkok)iclkok=1   ! notice browse button
          if(iser.eq.-3)iclkok=2  ! notice cancel button
        else
          CALL EASKS2CMD(LTMP,' ','Confirm zone operations file:',
     &      'browse another','cancel',iclkok,72,GFILE,
     &      'operations file',IER,16)
        endif
        call usrmsg(' ',' ','-')   ! clear the display
      endif
      if(iclkok.eq.1)then
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','zon','opr',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','zon','opr',sfile,snpfile,nfile,iier)
          if(snpfile(1:2).ne.'  ')then
            write(LTMP,'(3a)')zonepth(1:lnblnk(zonepth)),fs,
     &        snpfile(1:lnblnk(snpfile))

C Debug.
C            write(6,*) 'the browsed file is ',ltmp(1:lnblnk(ltmp))
C            write(6,*) 'the zonepth is ',zonepth(1:lnblnk(zonepth))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

            gotviabrowse=.true.
          else
            LTMP=GFILE
            CALL EASKS(LTMP,' ','Zone operations file name?',
     &            72,GFILE,'operations file',IER,5)
          endif
        else
          LTMP=GFILE
          CALL EASKS(LTMP,' ','Zone operations file name?',
     &            72,GFILE,'operations file',IER,5)
        endif

C Display a bit of information about this browsed file.
        IUO=IFIL+1
        call FINDFIL(LTMP,XST)
        IF(XST) call ZSCHEDHINT(IUO,LTMP,IER)

      elseif(iclkok.eq.2)then
        call edisp(iuout,'No file selected so returning.')
        return
      endif
      LPROJ(ICOMP)=LTMP

C Debug.
C      write(6,*) 'lproj is ',lproj(icomp)

  5   MODOPR=.FALSE.
 
C Instanciate for current day types prior to scanning the source operation file.
      DO 400 IDTY=1,NBDAYTYPE
        NAC(IDTY)=0
        NCAS(IDTY)=0
        ctlstr(icomp,IDTY)='no control of air flow  '
 400  CONTINUE
      oprdesc(icomp)='no operations notes (yet)'
      IER=0

C Set the default type labels for this zone
      lodlabel(icomp,1)='Occupt'
      lodlabel(icomp,2)='Lights'
      lodlabel(icomp,3)='Equipt'  
      lodlabel(icomp,4)='N/A   '  
      lodlabel(icomp,5)='Ann.El'  
      lodlabel(icomp,6)='N/A   ' 
      lodlabel(icomp,7)='N/A   '      
 
C Read file for editing or listing. If there is no file then begin with
C default data (and set ip3ver to 2 in case user does nothing else).
      IUO=IFIL+1
      call FINDFIL(LPROJ(ICOMP),XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)

C Do cursory check to see if the file is sorted.
        sorted=.true.
        problem=.false.
        DO 401 IDTY=1,NBDAYTYPE
          call checksort(icomp,IDTY,problem,ier)
          if(problem)then
            sorted=.false.
            MODOPR=.true.
            WRITE(OUTS,'(2A)')CALENTAG(IDTY),
     &      ' casual gains might be unsorted.'
            call edisp(iuout,OUTS)
          endif
 401    CONTINUE
      ELSE

C The choices for the user - sometimes they want to skip
C all of this definition because they will want to use
C the definition already defined in another zone.
  19    h(1)='Options: '
        h(2)=' define from scratch - you are asked to define all'
        h(3)='   casual gain periods for each day type as well as'
        h(4)='   schedules of infiltration and ventilation.'
        h(5)=' '
        h(6)=' import air flow schedules - you select a zone with an'
        h(7)='   air flow schedule pattern and it is copied into this'
        h(8)='   zone, optionally with adjustments if the zone'
        h(9)='   volumes are different. You can then define the'
        h(10)='   casual gains.'
        h(11)=' '
        h(12)=' import casual gains - you select a zone with casual'
        h(13)='   gain patterns and they are copied into this zone,'
        h(14)='   optionally with adjustments if the zone base areas'
        h(15)='   differ.'
        h(16)=' '
        h(17)=' import air flows and casual gains - both of the above'
        h(18)='   and you will be given the option to import air'
        h(19)='   flow schedules casual gains from different zones'
        h(20)='   (You may wish to note which zones you used and'
        h(21)='   include this in the documentation of the zone'
        h(22)='   operations file.)'
        h(23)=' '
        h(24)=' import < pattern - has the same functionality as above'
        h(25)='   except that the list of operations files are taken'
        h(26)='   from a folder within the standard ESP-r distribution'
        h(27)='   located on your computer. Files placed in this'
        h(28)='   folder serve as a resource for often-used patterns'
        h(29)='   of casual gains and scheduled air flows.'
        h(30)=' '
        h(31)=' nothing happens in the zone - does just this. It fills'
        h(32)='   the file with zero occupants, lights, small power'
        h(33)='   and no infiltration or ventilation for all day types.'
        h(34)=' '
        h(35)='After initiating one of the above actions you can edit'
        h(36)='the data as required. '
        choices(1) ='a define from scratch '
        choices(2) ='b air flow     < another zone'
        choices(3) ='c casual gains < another zone'
        choices(4) ='d air & gains  < another zone'
        choices(5) ='e air flow     < from pattern'
        choices(6) ='f casual gains < from pattern'
        choices(7) ='g air & gains  < from pattern'
        choices(8) ='h nothing happens in the zone'
        choices(9) =' ________________________'
        choices(10)='! cancel                 '
        choices(11)='? help                   '
        NIGU=11
        IGU=-2
        if(mmod.eq.8)then
          CALL EMENU('Operations File Options',choices,NIGU,IGU)
        else
          CALL EMENU('Options',choices,NIGU,IGU)
        endif

        if(igu.eq.0)then
          goto 19
        elseif(igu.eq.8)then

C Setup minimal description (single period for each type of casual
C gain set to zeros with no infiltration and ventilation) and exit.
          DO 399 IDTY=1,NBDAYTYPE
            ctlstr(icomp,IDTY)='no control of air flow  '
            NAC(IDTY)=1
            IACS(IDTY,1)=0
            IACF(IDTY,1)=24
            ACI(IDTY,1)=0.0
            ACV(IDTY,1)=0.0
            IPT(IDTY,1)=0
            TA(IDTY,1)=0.0
            NCAS(IDTY)=3
            ICGS(IDTY,1)=0
            ICGF(IDTY,1)=24
            CMGS(IDTY,1)=0.0
            CMGL(IDTY,1)=0.0
            RADC(IDTY,1)=0.5
            CONC(IDTY,1)=0.5
            ICGT(IDTY,1)=1
            ICGS(IDTY,2)=0
            ICGF(IDTY,2)=24
            CMGS(IDTY,2)=0.0
            CMGL(IDTY,2)=0.0
            RADC(IDTY,2)=0.5
            CONC(IDTY,2)=0.5
            ICGT(IDTY,2)=2
            ICGS(IDTY,3)=0
            ICGF(IDTY,3)=24
            CMGS(IDTY,3)=0.0
            CMGL(IDTY,3)=0.0
            RADC(IDTY,3)=0.5
            CONC(IDTY,3)=0.5
            ICGT(IDTY,3)=3
 399      CONTINUE

C Instanciate for initial weekday/saturday/sunday day types
C prior to scanning the source operation file.
          write(oprdesc(icomp),'(2a)')
     &    'nothing happens in this zone in terms of occupants lights',
     &    ' and small power. There is not infiltration or ventilation.'

C Set the default type labels for this zone
          lodlabel(icomp,1)='Occupt'
          lodlabel(icomp,2)='Lights'
          lodlabel(icomp,3)='Equipt'  
          lodlabel(icomp,4)='N/A   '  
          lodlabel(icomp,5)='Ann.El'  
          lodlabel(icomp,6)='N/A   ' 
          lodlabel(icomp,7)='N/A   '
          ip3ver(icomp)=2
          MODOPR=.true.
          sorted=.true.

C Write file and return.
          LTMP=LPROJ(ICOMP)
          H(1)='A zone operation file contains schedules of air flow'
          H(2)='and casual gains.  Air flows may have thermostatic'
          H(3)='or wind speed control. '
          CALL EASKS(LTMP,' ','Zone operations file name?',
     &           72,GFILE,'operations file',IER,3)
          IF(LTMP.NE.' ')THEN
            call usrmsg(
     &        'Updating model to reflect changes',
     &        'in zone operations data or file name...','-')
            LPROJ(ICOMP)=LTMP
            CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
            CALL EMKCFG('-',ier)
            call usrmsg(
     &        'Updating model to reflect changes',
     &        'in zone operations data or file name...done.','-')
          ENDIF
         return  
        elseif(igu.eq.9)then
          goto 19
        elseif(igu.eq.10)then

C User said to cancel so return from the subroutine without
C doing anything.
          return
        elseif(igu.eq.11)then

C Explain
          CALL PHELPD('operation choices',36,'-',0,0,IER)
          goto 19
        endif

C Section dealing with importing air flows from whatever source.
        if(igu.eq.2.or.igu.eq.4.or.igu.eq.5.or.igu.eq.7)then
          if(igu.eq.4.or.igu.eq.7)then

C Warn the user that the selection list is going to come up twice.
            call edisp(iuout,
     &        'You will be asked to select the source zone twice. The')
            call edisp(iuout,
     &        'first for air flow patterns, then for casual gains.')
          endif

C Import flow schedules from another zone (or in the case of `e`
C choice from standard pattern folder. Ask which zone, then
C confirm how much to copy and whether infiltration and ventilation
C flows are to preserve ach or volume of flow.
          if(igu.eq.2.or.igu.eq.4)then
 38         isource=0
            izdef=0
            call askzone(isource,izdef,
     &        'Air flow pattern taken from which zone?',
     &        'Source Zone','-','Air flow import requires a zone.',
     &        ier)
            if (IER.ne.0.or.isource.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 38
            endif

C Display a bit of information about this browsed file.
            IUO=IFIL+1
            write(LTMP,'(a)') LPROJ(isource)
            call FINDFIL(LTMP,XST)
            if(XST)then
              call ZSCHEDHINT(IUO,LTMP,IER)
            else
              call USRMSG('The selected file could not be found.',
     &          'Please try again.','W')
              goto 38
            endif
            sfile=' '
          elseif(igu.eq.5.or.igu.eq.7)then

C Bring up a list of files ending in .opr within the pattern folder
C of the distribution and let the user select one.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','pat','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','pat','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
C Debug.
C            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 19
              endif

C Display a bit of information about this browsed file.
              IUO=IFIL+1
              call FINDFIL(sfile,XST)
              if(XST)then
                call ZSCHEDHINT(IUO,sfile,IER)
              else
                call USRMSG('The selected file could not be found.',
     &            'Please try again.','W')
                goto 19
              endif
            else

C Nothing selected treat this as a return state.
              call usrmsg('No files available, try again.',' ','W')
              goto 19
            endif
          endif

C Only ask these questions if the source file exists.
          CALL EASKABC(' ','Options:','all flow schedules & ctl',
     &     'infiltration only','cancel',ivopt,21)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'preserve volume flow rate','cancel',infv,21)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'preserve volume flow rate','cancel',ivntv,21)
            if(ivntv.eq.1) vflow='ach-ach '
            if(ivntv.eq.2) vflow='vol-vol '
            if(ivntv.eq.3) goto 5
          else
            vflow='ach-ach '
          endif
          idest=icomp
          itrc=1

C We have all of the directives, do the import.
          call COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &      itrc,'-',ier)
        endif
        if(igu.eq.1.or.igu.eq.2.or.igu.eq.5)then

C Each of these leaves the user to define the casual gains manually.
C The from scratch should intially be version 2.0
          ip3ver(icomp)=2
          h(1)='New operations for this zone. Please check your notes'
          h(2)='and provide initial casual gain period start times'
          h(3)='for each day type (Weekday, Saturday, Sunday etc.) and'
          h(4)='for each casual gain type (occupants, lighting and'
          h(5)='small power). Do not include periods to be imported'
          h(6)='from events profiles.'
          h(7)=' '
          h(8)='Example of a schedule including 6 periods:'
          h(9)=' '
          H(10)='       |       ________               '
          H(11)=' Watts |       |      |___            '
          H(12)='       |    ___|         |____        '
          H(13)='       |   |   schedule       |       '
          H(14)='       |___|____..____________|_______'
          H(15)='       0   7  10..    13  14  18    24'
          H(16)='                   Time'
          H(17)=' '
          H(18)='The default assumption is that there is one period'
          H(19)='(from 0h00 to 24h00) with zero sensible and latent'
          H(20)='gains for each casual gain type on each day type.'
          H(21)='This ensures that nothing is happening in the zone'
          H(22)='until you provide relevant information (e.g. insert'
          H(23)='at least one period and define a casual gain).'
          CALL PHELPD('operation setup',23,'-',0,0,IER)
          call initcasday(ier)

C Debug.
C          write(6,*) 'after ncas1 ncas2 ncas3 ',ncas1,ncas2,ncas3

          MODOPR=.true.
          sorted=.true.
        elseif(igu.eq.3.or.igu.eq.4.or.igu.eq.6.or.igu.eq.7)then
          if(igu.eq.4.or.igu.eq.7)then
            call edisp(iuout,
     &    'You are being asked for the casual gains source zone.')
          endif

          if(igu.eq.3.or.igu.eq.4)then

C If igu is 3 or 4 import casual gains from another zone.
C Ask which zone, then confirm whether absolute W values are to be scaled.
 39         isource=0
            izdef=0
            call askzone(isource,izdef,'Casual gains from which zone?',
     &        'Source Zone','-','Casual gain import requires a zone.',
     &        ier)
            if (IER.ne.0.or.isource.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 39
            endif

C Display a bit of information about this browsed file.
            IUO=IFIL+1
            write(LTMP,'(a)') LPROJ(isource)
            call FINDFIL(LTMP,XST)
            if(XST)then
              call ZSCHEDHINT(IUO,LTMP,IER)
            else
              call USRMSG('The selected file could not be found.',
     &          'Please try again.','W')
              goto 39
            endif
            sfile=' '
          elseif(igu.eq.6.or.igu.eq.7)then

C If igu is 6 or 7 bring up a list of files ending in .opr
C within the pattern folder of the distribution and let the
C user select one.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','pat','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','pat','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
C Debug.
C            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 19
              endif

C Display a bit of information about this browsed file.
              IUO=IFIL+1
              call FINDFIL(sfile,XST)
              if(XST)then
                call ZSCHEDHINT(IUO,sfile,IER)
              else
                call USRMSG('The selected file could not be found.',
     &            'Please try again.','W')
                goto 19
              endif
            else

C Nothing selected treat this as a return state.
              call usrmsg('No files available, try again.',' ','W')
              goto 19
            endif
          endif

          CALL EASKABC(' ','Casual gain import:','preserve Watts',
     &       'convert by base area','cancel',ivntv,23)
          if(ivntv.eq.1) iwatt='W-W    '
          if(ivntv.eq.2) iwatt='W-m2-W '
          if(ivntv.eq.3) goto 5
          idest=icomp
          act='allcas'
          itrc=1
          call IMPORTZCASG(isource,idest,act,iwatt,sfile,itrc,'-',ier)

C Remind users to check their data.
      h(1)='An automated process has been used to manage the imported'
      h(2)='schedules. It is recommended that you confirm that the'
      h(3)='resulting file is correct. '
      h(4)=' '
      h(5)='One approach is to generate a new QA report and check this'
      h(6)='against the previous model and then run a short assessment'
      h(7)='to confirm that performance is as expected.'
          CALL PHELPD('operation import confirm',7,'-',0,0,IER)
        endif
      
      ENDIF

C Display different options depending on the version of the file.
  20  write(ITEM(1),'(2a)')   '  zone: ',zname(ICOMP)
      if(ip3ver(icomp).le.2)then
        write(ITEM(2),'(3a)') 'a description: ',oprdesc(ICOMP)(1:16),'-'
        write(ITEM(3),'(2x,2a)')   oprdesc(ICOMP)(17:45),'-'
        write(ITEM(4),'(2x,2a)')   oprdesc(ICOMP)(46:74),'-'
        write(ITEM(5),'(2x,2a)')   oprdesc(ICOMP)(75:103),'-'
        ITEM(6)=              '  ----------------------------- '
        ITEM(7)=              'c edit scheduled air flows      '
        ITEM(8)=              'd edit casual gains             '
        ITEM(9)=             '  ----------------------------- '    
        ITEM(10)=             '! list air flow & casual gains  '
        ITEM(11)=             '  ----------------------------- '
        ITEM(12)=             '> save operations file          '
        ITEM(13)=             '? help                          '
        ITEM(14)=             '- exit this menu                '
        NITEMS=14
      elseif(ip3ver(icomp).eq.3)then
        write(ITEM(2),'(3a)') 'a inf vent notes: ',
     &    ventdesc(ICOMP)(1:13),'-'
        write(ITEM(3),'(2x,a)')    ventdesc(ICOMP)(14:44)
        write(ITEM(4),'(3a)') 'b casual notes: ',
     &    oprdesc(ICOMP)(1:15),'-'
        write(ITEM(5),'(2x,a)')    oprdesc(ICOMP)(16:44)
        ITEM(6)=              '  ----------------------------- '
        ITEM(7)=              'c edit scheduled air flows      '
        ITEM(8)=              'd edit casual gains             '
        ITEM(9)=             '  ----------------------------- '    
        ITEM(10)=             '! list air flow & casual gains  '
        ITEM(11)=             '  ----------------------------- '
        ITEM(12)=             '> save operations file          '
        ITEM(13)=             '? help                          '
        ITEM(14)=             '- exit this menu                '
        NITEMS=14
      endif

C Help text for this menu.
      H(1) ='A summary of the data held within a zone operations file'
      H(2) ='follows.'
      H(3) =' '
      H(4) =' 1) Operations description - up to 248 characters'
      H(5) ='    summarising what is happening in the zone.'
      H(6) ='  '
      H(7) =' 2) Weekday, Saturday, Sunday (& other day types if '
      H(8) ='    defined) air flow data:  start and finish hours '
      H(9) ='    for each period infiltration and ventilation rates '
      H(10)='    (ACH) coupling index: '
      H(11)='         > 0 is coupled zone number'
      H(12)='         = 0 source at specified temperature'
      H(13)='  '
      H(14)=' 3) Air flow thermostatic control definition:'
      H(15)='       sensed zone number '
      H(16)='       upper and lower limits'
      H(17)='       upper and lower air flow rates'
      H(18)=' '
      H(19)='The relationship between control and schedules is'
      H(20)='as follows.                          '
      H(21)=' '
      H(22)='      |       ________               '
      H(23)=' ac/h |       |      |___            '
      H(24)='      |    ___|         |____        '
      H(25)='      |   |   scheduled rate |       '
      H(26)='      |___|____:_____________|_______'
      H(27)='               : <- time ->          '
      H(28)='      |        :                     '
      H(29)=' ac/h |flow |  V     | flow | flow | '
      H(30)='      |__x__|________|___y__|__z___| '
      H(31)='       low   default   high  extreme '
      H(32)='         <-  temperature ranges  ->  '
      H(33)=' '
      H(34) =' 4) Weekday, Saturday and Sunday (& other day types if '
      H(35) ='    defined) casual gains data: start and finish hours '
      H(36) ='    for each period casual gain type:'
      H(37) ='         1: usually occupants'
      H(38) ='         2: usually lighting'
      H(39) ='         3: usually equipment'
      H(40) ='       sensible and latent load magnitudes'
      H(41) ='       radiant and convective portions'
      H(42) =' '
      H(43)='Where electrical data are associated with a particular'
      H(44)='zone load (to enable electrical simulation), the'
      H(45)='required data items are: power factor,load type'
      H(46)='(inductive or capacative), real power consumption,'
      H(47)='operational voltage and the generator phase to which'
      H(48)='the load is connected.'

      INO=-2
      if(mmod.eq.8)then
        CALL EMENU('Zone Operations',ITEM,NITEMS,INO)
      else
        CALL EMENU('Operations',ITEM,NITEMS,INO)
      endif

      IF(INO.EQ.NITEMS)THEN
        if(.NOT.browse)then

C If not currently sorted then warn the user. Sometimes this happens
C even if modopr has not been set.
          if(.NOT.sorted)then
            DOK=.true.
            h(1)='You are asked this question because periods'
            h(2)='might have been altered and require re-sorting. '
            CALL ASKOK(
     &        'Before exiting do you wish to sort possible changes',
     &        'to the casual gain periods?',OK,DOK,2)
            if(ok)then

C Sort the gains and then updated the file and mark MODOPR.
              call PROCESSOLDCAS(ICOMP,0,iuout,IER)
              IUO=IFIL+1
              CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
              MODOPR=.false.
            endif
          endif
        endif
        if(MODOPR.or.gotviabrowse)then
          if(browse)return

          DOK=.true.
          h(1)='You are asked this question because changes were'
          h(2)='detected in the zone operations schedules and '
          h(3)='you might loose this information if you exit without'
          h(4)='saving the changes.'
          CALL ASKOK('Before exiting do you wish to',
     &      'save any changes made?',OK,DOK,4)
          IF(.not.OK) return

C So save the changes to the file and then exit.
          IUO=IFIL+1
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
        endif
        RETURN
      ELSEIF(INO.EQ.2)THEN
        if(ip3ver(icomp).le.2)then

C Edit operation description.
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          tcname=oprdesc(icomp)
          CALL EASKS248(tcname,' ','Zone operations notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')oprdesc(icomp)=tcname
          MODOPR=.true.
        elseif(ip3ver(icomp).eq.3)then
          H(1)='Notes (<248 char) for schedules of zone air flow can' 
          H(2)='be included in the model. Use this facility to'
          H(3)='clarify what is happening in the zone.'
          tcname=ventdesc(icomp)
          CALL EASKS248(tcname,' ','Zone scheduled air notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')ventdesc(icomp)=tcname
          MODOPR=.true.
        endif
      ELSEIF(INO.EQ.4)THEN
        if(ip3ver(icomp).eq.3)then
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          tcname=oprdesc(icomp)
          CALL EASKS248(tcname,' ','Zone operations notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')oprdesc(icomp)=tcname
          MODOPR=.true.
        endif
      ELSEIF(INO.EQ.7)THEN

C Edit air flows.
        if(ip3ver(icomp).le.2)then
          CALL EDAIR2(ICOMP,IER)
        elseif(ip3ver(icomp).eq.3)then
          CALL EDAIR3(ICOMP,IER)
        endif
        MODOPR=.true.
      ELSEIF(INO.EQ.8)THEN

C Edit casual gains.
        CALL EDCAS(ICOMP,MODOPR,sorted,IER)
      ELSEIF(INO.EQ.10)THEN

C List flows and gains.
        CALL VENTINF(ICOMP,ITRU)
        call CASINF(ICOMP,ITRU)
      ELSEIF(INO.EQ.(NITEMS-2))THEN

C Edit or browse for operations file name.
        call edisp(ITRU,' ')
        if(LPROJ(ICOMP)(1:2).EQ.'  '.or.LPROJ(ICOMP)(1:4).eq.'UNKN')then
          LTMP=GFILE
        else
          LTMP=LPROJ(ICOMP)
        endif

    8   H(1)='A zone operations file contains schedules of air flow'
        H(2)='and casual gains.  Air flows may have thermostatic'
        H(3)='or wind speed control. If electrical details are  '
        H(4)='included then casual gains and electrical loads are'
        H(5)='combined into a single zone load.'
        CALL EASKS(LTMP,' ','Zone operations file name?',
     &        72,GFILE,'operations file',IER,5)
        IF(LTMP(1:2).NE.'  ')THEN
          LPROJ(ICOMP)=LTMP
        ELSE
          GOTO 8
        ENDIF

C Save current information into a descriptive file
C and rewrite the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
   11     CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
          IF(IER.LT.0)THEN
            DOK=.true.
            h(1)='You were presented with this message because a'
            h(2)='problem was detected while trying to update the'
            h(3)='zone operations file. Check that you have sufficient'
            h(4)='disk space and/or permission to update the model'
            CALL ASKOK('Problem encountered saving data!',
     &      'Try again?',OK,DOK,4)
            IF(OK)THEN
              GOTO 11
            ELSE
              IER=1
              RETURN
            ENDIF
          else
            MODOPR=.false.  ! signal that we have saved the file.
          endif
          CALL EMKCFG('-',IER)
        else
          call usrmsg('Cannot save problem while in browse',
     &                'mode. You must first `own` the model!','W')
          goto 20
        endif
        MODOPR=.false.
      ELSEIF(INO.EQ.(NITEMS-1))THEN

C Explain
        CALL PHELPD('operation help',48,'-',0,0,IER)
      ENDIF
      GOTO 20
      END

C ********************* EDAFCTL *********************
C EDAFCTL: Edits thermostatic air flow control description.
C ITRC is the trace level, IER=0 is OK.

C << must be revised for multiple day types and periods. >>

      SUBROUTINE EDAFCTL(ICOMP,IER)

#include "building.h"
#include "geometry.h"
      common/pophelp/h(60)
      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      common/p3ver/ip3ver(MCOM)

      DIMENSION IVAL(4), IVALS(3), ALT(4),ALTS(3)
      CHARACTER hold*40,outs*80,outs2*80
      CHARACTER ALT*32,ALTS*31,H*72
      LOGICAL OK,DOK

C Flow control browse & editing.
      DOK=.false.
      h(1)='This facility supports basic scheduled control of air'
      h(2)='infiltration and ventilation based on temperatures or'
      h(3)='wind velocity.'
      CALL ASKOK(' ','Is air flow thermostatically controlled?',
     &           OK,DOK,3)
      IF(.NOT.OK)THEN
        ITCTL(ICOMP)=0
        THI(ICOMP)=100.
        TUP(ICOMP)=0.
        TLO(ICOMP)=0.
        ctlstr(ICOMP,1)='no control of air flow  '
        ctlstr(ICOMP,2)='no control of air flow  '
        ctlstr(ICOMP,3)='no control of air flow  '
        return
      endif

      ALT(1)='zone air temperature (default) '
      ALT(2)='adjacent zone air temperature  '
      ALT(3)='external air temperature       '
      ALT(4)='wind velocity                  '

      H(1)='Control of air flow can be based on one of the'
      H(2)='following parameters: '
      H(3)=' '
      H(4)='* air temperature within the current zone;'
      H(5)='* air temperature within another modelled zone;'
      H(6)='* external air temperature;'
      H(7)='* external wind velocity.'
      H(8)=' '
      H(9)='Note that thermostatic or wind velocity control'
      H(10)='applies to all days within a simulation.'
      IX=1
      CALL EPICKS(IX,IVAL,' ','Air Flow Control Options',
     &         32,4,ALT,'air flow control',IER,10)

      IF(IX.EQ.0) RETURN
      ITCTL(icomp)=IVAL(1)

      ALTS(1)='infiltration only'
      ALTS(2)='ventilation only'
      ALTS(3)='infiltration & ventilation'

      H(1)='Air flow control can be restricted to infiltration'
      H(3)='(outside), to ventilation (from another zone or at'
      H(3)='specified temperature) or to both.'
      IX=1
   12 CALL EPICKS(IX,IVALS,' ','Air flow control scope:',
     &         31,3,ALTS,'air flow scope',IER,3)

      IF(IX.EQ.0) goto 12
      ISCOPE=IVALS(1)
      IF(ISCOPE.EQ.1)THEN
        IMULTI=-10
      ELSE IF(ISCOPE.EQ.2)THEN
        IMULTI=-1
      ELSE
        IMULTI=1
      END IF
      IF(IMULTI.NE.-10)ITCTL(ICOMP)=ITCTL(ICOMP)*IMULTI
      IF(IMULTI.EQ.-10.AND.ITCTL(ICOMP).LT.4)
     &     ITCTL(ICOMP)=IMULTI-ITCTL(ICOMP)
      IF(IMULTI.EQ.-10.AND.ITCTL(ICOMP).EQ.4)
     &     ITCTL(ICOMP)=-ITCTL(ICOMP)


C Generate a brief description.
C << this does not yet take into account day type >>
      if(ip3ver(icomp).le.2)then
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          ctlstr(icomp,1)='infil only: wind speed  '
        elseif(IY.eq.-13)then
          ctlstr(icomp,1)='infil only: ext db temp '
        elseif(IY.eq.-12)then
          ctlstr(icomp,1)='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          ctlstr(icomp,1)='infil only: zone temp   '
        elseif(IY.eq.-4)then
          ctlstr(icomp,1)='vent only: wind speed   '
        elseif(IY.eq.-3)then
          ctlstr(icomp,1)='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          ctlstr(icomp,1)='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          ctlstr(icomp,1)='vent only: zone temp    '
        elseif(IY.eq.4)then
          ctlstr(icomp,1)='infil & vent: wind speed'
        elseif(IY.eq.3)then
          ctlstr(icomp,1)='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          ctlstr(icomp,1)='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          ctlstr(icomp,1)='infil & vent: zone T    '
        elseif(IY.eq.0)then
          ctlstr(icomp,1)='no control of air flow  '
        else
          ctlstr(icomp,1)='unknown flow control   '
        endif
      elseif(ip3ver(icomp).eq.3)then

C << to be done >>

      endif

      H(1)='For temperature control the limit is in degC.'
      H(2)='For wind speed control the limit is in m/s.'
      h(3)=' '
      h(4)='Lower limit: point below which an alternative flow'
      h(5)='  rate takes effect (e.g. closing windows when the'
      h(6)='  lower limit is rached).'
      h(7)=' '
      h(8)='Middle limit: point above which the 1st stage of an'
      h(9)='  alternative flow regime takes effect).'
      h(10)=' '
      h(11)='High limit: point above which a 2nd stage flow'
      h(12)='  regime takes effect. (Set high, e.g. 100degC,'
      h(13)='  to prevent use.)'
 100  WRITE(HOLD,'(3f8.2,a)')TLO(ICOMP),TUP(ICOMP),THI(ICOMP),'  '
      CALL EASKS(HOLD,' ',
     &  'Lower, middle and high set points?',
     &  40,' 15.0  22.0  26.0  ','flow stages ',IER,13)
      K=0
      CALL EGETWR(HOLD,K,VALL,-10.,200.,'F','low',IER)
      CALL EGETWR(HOLD,K,VALU,VALL,200.,'F','middle',IER)
      CALL EGETWR(HOLD,K,VALH,VALU,200.,'F','high',IER)
      if(ier.ne.0)goto 100
      TLO(ICOMP)=VALL
      TUP(ICOMP)=VALU
      THI(ICOMP)=VALH

c Define air flow when the sensed variable is below
c the lower range.
      IF(ISCOPE.EQ.2)goto 304

      h(1)='Infiltration is defined in terms of air changes per'
      h(2)='hour. This value is used below the lower set point.'
      VAL=ACIL(ICOMP)
      CALL EASKR(VAL,' ','Lower range infiltration rate?',
     &        0.0,'F',200.0,'W',0.0,'lower infiltration',IER,2)
      ACIL(ICOMP)=VAL

  304 IF(ISCOPE.NE.1)THEN
        h(1)='Ventilation is defined in air changes per hour. The'
        h(2)='source can be another modelled zone or a constant'
        h(3)='temperature source. In both cases the volumer flow'
        h(4)='rate is relative to the current zone volume.'
        CALL EASKABC(' ','Ventilation options:',
     &      'none','from modelled zone','at constant temperature',j,4)
        if(j.eq.1)then
          ACVL(ICOMP)=0.
          IVL(ICOMP)=0
          TAL(ICOMP)=0.0
        elseif(j.eq.2)then
          IV=IVL(ICOMP)
          call askzone(iv,IVL(icomp),'Ventilation from which zone?',
     &     'Source Zone','-','Ventilation definition requires a zone.',
     &      ier)
          IVL(ICOMP)=IV
          VAL=ACVL(ICOMP)
          ratio = VOL(ICOMP)/VOL(IV)
          write(outs,'(a,F7.3,a)') 
     &      ' Low range ventilation. (1ac/h in current zone =',
     &       ratio,' ac/h in source zone.)'
          write(outs2,'(a,F8.2,a,F8.2,a)') '(vol. current=',VOL(icomp),
     &      ' vol. source=',VOL(iv),')'
          CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'lwr vnt',
     &      IER,5)
          ACVL(ICOMP)=VAL
          TAL(ICOMP)=0.0
        elseif(j.eq.3)then
          IVL(ICOMP)=0
          VAL=ACVL(ICOMP)
          CALL EASKR(VAL,' ','Lower range ventilation rate?',
     &        0.0,'F',200.0,'W',1.0,'lower ventilation',IER,4)
          ACVL(ICOMP)=VAL
          V=TAL(ICOMP)
          CALL EASKR(V,' ',
     &      'Lower range ventilation source temperature?',
     &      0.0,'-',0.0,'-',18.0,'lower source t',IER,4)
          TAL(ICOMP)=V
        endif
      ENDIF
      call usrmsg(' ',' ','-')

C Define air flow when the sensed variable is above the middle range.
      IF(ISCOPE.NE.2)THEN
        h(1)='Infiltration is defined in air changes per hour. This'
        h(2)='value is used if the temperature is above the mid-leve'
        h(3)='set point but below the high level set point.'
        V=ACIU(ICOMP)
        CALL EASKR(V,' ','1st stage infiltration rate?',
     &      0.0,'F',200.0,'W',2.0,'middle infiltration',IER,3)
        ACIU(ICOMP)=V
      ENDIF
      call usrmsg(' ',' ','-')
      IF(ISCOPE.NE.1)THEN
        h(1)='Mid level ventilation is imposed when the sensed'
        h(2)='condition is between the middle and high set points.'
        h(3)='Ventilation (zone coupled air flow) is defined as'
        h(4)='air changes per hour. The source can be another'
        h(5)='zone or at a specified constant temperature. In both'
        h(6)='cases the volume flow rate is relative to the volume'
        h(7)='of the current zone.'
        CALL EASKABC(' ','First stage ventilation options:',
     &      'none','from modelled zone','at constant temperature',j,7)
        if(j.eq.1)then
          ACVU(ICOMP)=0.
          IVU(ICOMP)=0
          TAU(ICOMP)=0.0
        elseif(j.eq.2)then
          IV=IVU(ICOMP)
          call askzone(iv,IVU(icomp),'Ventilation from which zone?',
     &     'Source Zone','-','Ventilation definition requires a zone.',
     &     ier)
          IVU(ICOMP)=IV
          VAL=ACVU(ICOMP)
          ratio = VOL(ICOMP)/VOL(IV)
          write(outs,'(a,F7.3,a)') 
     &      ' Mid-range ventil. (ac/h where 1ac/h in current =',
     &      ratio,' ac/h in source)'
          write(outs2,'(a,F8.2,a,F8.2,a)') '(vol current=',VOL(icomp),
     &      ' vol source=',VOL(iv),')'
          CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'mid vnt',
     &       IER,7)
          ACVU(ICOMP)=VAL
          TAU(ICOMP)=0.0
        elseif(j.eq.3)then
          IVU(ICOMP)=0
          VAL=ACVU(ICOMP)
          CALL EASKR(VAL,' ','Mid-range ventilation rate?',
     &        0.0,'F',200.0,'W',1.0,'mid ventilation',IER,7)
          ACVU(ICOMP)=VAL
          V=TAU(ICOMP)
          CALL EASKR(V,' ','Mid range ventilation source temperature?',
     &        0.0,'-',0.0,'-',18.0,'mid source t',IER,7)
          TAU(ICOMP)=V
        endif
      ENDIF
      call usrmsg(' ',' ','-')

C Define air flow when the sensed variable is above the high range.
      IF(ISCOPE.NE.2)THEN
        h(1)='Infiltration is defined in air changes per hour and is'
        h(2)='used when conditions exceed the high set point. If not'
        h(3)='needed make the set point high, e.g. 100degC.'
        V=ACIU(ICOMP)
        CALL EASKR(V,' ','High stage infiltration rate?',
     &      0.0,'F',200.0,'W',2.0,'high infiltration',IER,3)
        ACIH(ICOMP)=V
      ENDIF

      IF(ISCOPE.NE.1)THEN
        h(1)='High level ventilation is imposed when the sensed'
        h(2)='condition exceeds the high set point. Ventilation'
        h(3)='(zone coupled air flow) is defined as air changes per'
        h(4)='hour. The source can be another zone or at a specified'
        h(5)='constant temperature. In both cases the volume flow '
        h(6)='rate is relative to the volume of the current zone.'

        CALL EASKABC(' ','High stage ventilation options:',
     &      'none','from modelled zone','at constant temperature',j,6)
        if(j.eq.1)then
          ACVH(ICOMP)=0.
          IVH(ICOMP)=0
          TAH(ICOMP)=0.0
        elseif(j.eq.2)then
          IV=IVH(ICOMP)
          call askzone(iv,IVH(icomp),'Ventilation from which zone?',
     &     'Source Zone','-','Ventilation definition requires a zone.',
     &     ier)
          IVH(ICOMP)=IV
          VAL=ACVH(ICOMP)
          ratio = VOL(ICOMP)/VOL(IV)
          write(outs,'(a,F7.3,a)') 
     &      ' High-range ventil. (ac/h where 1ac/h in current =',
     &      ratio,' ac/h in source)'
          write(outs2,'(a,F8.2,a,F8.2,a)') '(vol current=',VOL(icomp),
     &      ' vol source=',VOL(iv),')'
          CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'high vent',
     &      IER,7)
          ACVH(ICOMP)=VAL
          TAH(ICOMP)=0.0
        elseif(j.eq.3)then
          IVH(ICOMP)=0
          VAL=ACVH(ICOMP)
          CALL EASKR(VAL,' ','High-range ventilation rate?',
     &        0.0,'F',200.0,'W',1.0,'high ventilation',IER,6)
          ACVH(ICOMP)=VAL
          V=TAH(ICOMP)
          CALL EASKR(V,' ',
     &        'High range ventilation source temperature?',
     &        0.0,'-',0.0,'-',18.0,'hi source t',IER,6)
          TAH(ICOMP)=V
        endif
      ENDIF
      call usrmsg(' ',' ','-')

      RETURN
      END

C ********************* EDAIR2 *********************
C EDAIR2: Edits air flow portion of a zone operations file.
C This version toggles between day types and expands the
C editing options.
C Returns: NAC*,IACS*,IACF*,ACI*,ACV*,IPT*,TA* data in common.
C IER=0 is OK.
      SUBROUTINE EDAIR2(ICOMP,IER)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
      
C      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL

      common/pophelp/h(60)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      character vert(35)*33,H*72,KEY*1,title*33
      character LTMP*72
      character afact*8,iflow*8,vflow*8,outs*124,sfile*72,snpfile*72
      logical XST,closezero,closeone,closetwo
      integer KSTR(MDTY),ix,iy

C Initial menu entry setup.
      idtyp = 1
 5    MHEAD=3
      MCTL=5
      ILEN=NAC(IDTYP)*3
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Check and plot air flows. 
      call CHKAIRFL
      if (MMOD.eq.8) call PLTAIR(ICOMP,idtyp)

C Set up top of menu.
  44  if (ITCTL(ICOMP).eq.0) then 
        write(VERT(1),'(A)') '1 air flow control (none)'
      else
        write(VERT(1),'(2A)') '1 control: ',ctlstr(icomp,1)(1:22)
      endif
      vert(2) =              '  -----------------------------'
      write(vert(3),'(3a,i2,a)') '> air flow>> ',calentag(idtyp),' (',
     &NAC(IDTYP),')'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

C Day type loop
      istartcount=1
      iendcount=0
      do 10 L=1,NAC(idtyp)

C Ensure that all three lines can be written. If on subsequent
C pages then check loosely in order to catch an item that 
C would have otherwise spanned the page divide.
        iendcount=istartcount+2
        if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
          continue
        elseif(istartcount+2.GE.IST.AND.
     &        (iendcount.LE.(IST+MIFULL)))then
          continue
        else
          istartcount=iendcount+1
          goto 10
        endif
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &    IACS(idtyp,L),'-',IACF(idtyp,L)
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,f6.2)') KEY,'  infil rate:',ACI(idtyp,L)
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        if (IPT(idtyp,L).gt.0) then
          write (VERT(M),'(2a,f6.2,1x,a)') KEY,'  vent rate:',
     &      ACV(idtyp,L),zname(IPT(idtyp,L))(1:10)
        elseif (ACV(idtyp,L).lt.0.0001) then 
          write (VERT(M),'(2a,f6.2,a)') KEY,'  vent rate:',
     &      ACV(idtyp,L),'  n/a'
        else
          write (VERT(M),'(2a,f6.2,a,f6.2)') KEY,'  vent rate:',
     &      ACV(idtyp,L),'@temp:',TA(idtyp,L)
        endif
        istartcount=iendcount+1
 10   continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=           '  -----------------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT             ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2) = '+ add/delete/copy/import flows '
      VERT(M+3) = '! list current information     '
      VERT(M+4) = '? help                         '
      VERT(M+5) = '- exit this menu               '

C Help text for this menu.
      H(1)='Air flows are defined via one or more sequential'
      H(2)='periods, the magnitude at a given time being the'
      H(3)='flow specified a period that includes that time.'
      H(4)=' '
      H(5)='In cases where ventilation will occur with more'
      H(6)='than one modelled zone then a fluid flow network'
      H(7)='or temporal definition file should be employed.'
      H(8)=' '
      H(9)='Note that large changes in flow rates between'
      H(10)='periods can result in unacceptable interpolation'
      H(11)='errors.'
      H(12)=' '
      H(13)='The add/delete/copy/import option are as follows.'
      H(14)=' add - adds a period to one or all day types. BE SURE'
      H(15)='   to read the extended help messages in this dialog'
      H(16)='   to understand how to use the add function to insert'
      H(17)='   a period into a profile.'  
      H(18)=' '
      H(19)=' delete - deletes one or more periods (within a list'
      H(20)='   of all periods in all days).'
      H(21)=' '
      H(22)=' copy - copies Weekdays to Saturdays or Weekdays -> '
      H(23)='   Sundays or Saturdays -> Sundays'
      H(24)=' '
      H(25)=' import - copies infiltration, ventilation and control'
      H(26)='   (or a subset of these data) from another zone.'
      H(27)=' '
      H(28)='When importing you can specify whether to:'
      H(29)=' a) preserve the ac/h being imported, e.g. 1.5 ac/h in'
      H(30)='    the source zone is 1.5 ac/h in the destination zone;'
      H(31)=' b) preserve the volume of air implied by the ac/h in'
      H(32)='    the source zone, e.g. 1.5 ac/h in a 50m^3 source'
      H(33)='    zone becomes 1.07 ac/h in a 70m^3 destination zone.'
 
C Now display the menu.
      if(mmod.eq.8)then
        write (title,'(2a)') 'Air flow in ',
     &    zname(icomp)(1:lnzname(icomp))
      else
        write (title,'(2a)') 'Ventilation in ',
     &    zname(icomp)(1:lnzname(icomp))
      endif
      CALL EMENU(title,VERT,MVERT,IVERT)

      if (IVERT.EQ.MVERT) then
        return
      elseif (IVERT.EQ.(MVERT-1)) then
        CALL PHELPD('flows menu',33,'-',0,0,IER)
      elseif (IVERT.EQ.(MVERT-2)) then

C List current.
        call VENTINF(ICOMP,IUOUT)
      elseif (IVERT.EQ.(MVERT-3)) then

C Add delete copy period or import from another zone.
        H(1)='The options: '
        H(2)='The add/delete/copy/import option are as follows.'
        H(3)=' add - adds a period to one or all day types. IF'
        H(4)='   there is already a defined period which overlaps'
        H(5)='   this period then the volumes of air flow will be '
        H(6)='   summed.  For example: if there is a period from'
        H(7)='   8h00-24h00 with 1ach infiltration and you want to '
        H(8)='   add a period 20h00-24h00 with 0.5 the time from '
        H(9)='   20h00-24h00 will become 1.5 ach.'
        H(10)=' '
        H(11)='   If you want to adjust the profile so the inserted'
        H(12)='   period has a different value then initailly'
        H(13)='   give 0.0 for the flows. After the periods have'
        H(14)='   been revised then edit the newly created period.'
        H(15)='  '
        H(16)='   The alternative approach is to edit the start and'
        H(17)='   end time of the existing period so that a gap is'
        H(18)='   created and then use the add facility to fill in'
        H(19)='   the gap.'
        H(20)=' '
        H(21)=' delete - deletes one or more periods (within a list'
        H(22)='   of all periods in all days).'
        H(23)=' '
        H(24)=' copy - copies Weekdays to Saturdays or Weekdays -> '
        H(25)='   Sundays or Saturdays -> Sundays'
        H(26)=' '
        H(27)=' import - copies infiltration, ventilation and control'
        H(28)='   (or a subset of these data) from another zone.'
        H(29)=' '
        H(30)='When importing you can specify whether to:'
        H(31)=' a) preserve the ac/h being imported, e.g. 1.5 ac/h in'
        H(32)='    the source zone is 1.5 ac/h in the destination zone;'
        H(33)=' b) preserve the volume of air implied by the ac/h in'
        H(34)='    the source zone, e.g. 1.5 ac/h in a 50m^3 source'
        H(35)='    zone becomes 1.07 ac/h in a 70m^3 destination zone.'
        CALL EASKATOG(' ','Options (see help):',
     &    'add period','delete period','copy period',
     &    'import from other zone','import pattern','cancel',' ',IW,35)

C Add period
        if (IW.EQ.1) then 
          CALL EASKAB(' ','Add period to ','one day type',
     &     'all day types',IY,35)
          IF(IY.EQ.1)THEN
            IX=1
            CALL EPICKS(IX,KSTR,' ','choose daytype to add period in'
     &      ,12,nbdaytype,calentag,'Choose daytype',IER,35)
            call edisp(iuout,'choose daytype to add period in')
            IDT=KSTR(1)
            idtyp=idt
            IF(NAC(IDT).EQ.MA)THEN
              CALL USRMSG('Sorry, too many periods in day type ',
     &        calentag(idt),'W')
            ELSE
              IPER=0
              CALL EDAIRFL(ICOMP,IDT,IPER,-1)
            ENDIF
          ELSEIF(IY.EQ.2)THEN
            IDT=-1
            DO 400 IDTY=1,NBDAYTYPE
              IF(NAC(IDTY).EQ.MA)THEN
                CALL USRMSG('Sorry, too many periods in day type ',
     &            calentag(idt),'W')
                CALL EDISP(IUOUT,
     &            'Add periods by individually choosing each day type')
                IDT=0
              ENDIF
 400        CONTINUE
            IF(IDT.NE.0)THEN
              IDT=-1
              IPER=0
              call EDAIRFL(ICOMP,IDT,IPER,-1)
            ENDIF
          ENDIF
        elseif (IW.EQ.4.or.IW.eq.5) then

C Import flow schedules from another zone. Ask which zone, then
C confirm how much to copy and whether infiltration and ventilation
C flows are to preserve ac/h or volume of flow.
          if(iw.eq.4)then
 38         isource=0
            izdef=0
            call askzone(isource,izdef,
     &        'Air flow pattern from which zone?',
     &        'Source Zone','-','Ventilation import requires a zone.',
     &        ier)
            if (IER.ne.0.or.isource.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current zone (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 38
            endif

C Display a bit of information about this browsed file.
            IUO=IFIL+1
            write(LTMP,'(a)') LPROJ(isource)
            call FINDFIL(LTMP,XST)
            if(XST)then
              call ZSCHEDHINT(IUO,LTMP,IER)
            else
              call USRMSG('The selected file could not be found.',
     &          'Please try again.','W')
              goto 38
            endif
            sfile=' '
          elseif(iw.eq.5)then

C File browse from patterns.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','pat','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','pat','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
C Debug.
C            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 5
              endif

C Display a bit of information about this browsed file.
              IUO=IFIL+1
              call FINDFIL(sfile,XST)
              if(XST)then
                call ZSCHEDHINT(IUO,sfile,IER)
              else
                call USRMSG('The selected file could not be found.',
     &            'Please try again.','W')
                goto 5
              endif
            else

C Nothing selected treat this as a return state.
              call usrmsg('No files available, try again.',' ','W')
              goto 5
            endif
          endif

C Should only ask users these questions if the source file has been found.
          CALL EASKABC(' ','Import:','all flow schedules & ctl',
     &     'infiltration only','cancel',ivopt,21)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'convert to zone volume','cancel',infv,16)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'convert to zone vvolume','cancel',ivntv,16)
            if(ivntv.eq.1) vflow='ach-ach '
            if(ivntv.eq.2) vflow='vol-vol '
            if(ivntv.eq.3) goto 5
          else
            vflow='ach-ach '
          endif
          idest=icomp
          itrc=1

C We have all of the directives, do the import.
          call COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &      itrc,'-',ier)
        endif
      elseif (IVERT.EQ.(MVERT-4)) then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          ILEN=NAC(IDTYP)*3
          goto 44
        ENDIF
      elseif (IVERT.EQ.1) then

C Control of air flow.
        call EDAFCTL(ICOMP,IER)
      elseif (IVERT.EQ.3) then

C Increment the daytype focus.
        idtyp = idtyp + 1
        if(idtyp.gt.nbdaytype) idtyp = 1
        goto 5
      elseif (IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) then

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)

C Depending on currently defined gains work out day type of selected
C item. Also calculate period index in day type. Adjust for 2nd page
        if(ipm.eq.2.and.I.gt.1) I=I-1  ! adjust for 2nd page
        if(ipm.eq.3.and.I.gt.1) I=I-2  ! adjust for 3rd page
        if(ipm.eq.4.and.I.gt.1) I=I-3  ! adjust for 4th page

C Items are in groups of 3. If i/3 has no remainder I is 3,6,9,etc.
C so the period is just the nearest integer to I/3.0. If there is
C a remainder then I is 1,2 or 4,5 and it is one more than the
C nearest integer.
        trymod=amod(real(I),3.0)
        call eclose(trymod,0.0,0.001,closezero)
        call eclose(trymod,1.0,0.001,closeone)
        call eclose(trymod,2.0,0.001,closetwo)
        if(closezero.or.closetwo)then
          try=real(I)/3.0
          iper=nint(try)
        endif
        if(closeone)then
          try=real(I)/3.0
          iper=nint(try)+1
        endif
        IDT=idtyp
        if(closeone)then
          call EDAIRFL(ICOMP,IDT,IPER,1)
        endif
        if(closetwo)then
          call EDAIRFL(ICOMP,IDT,IPER,2)
        endif
        if(closezero)then
          call EDAIRFL(ICOMP,IDT,IPER,0)
        endif
      endif

C Redisplay menu.
      IVERT=-4
      GOTO 5
      END


C ********************* EDAIR3 *********************
C EDAIR3: Edits air flow portion of a v3.0 zone operations file.
C Returns: NAC*,IACS*,IACF*,ACI*,ACV*,IPT*,TA* data in common.
C IER=0 is OK.
      SUBROUTINE EDAIR3(ICOMP,IER)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
      
C      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL

      common/pophelp/h(60)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)

      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

C Common block P2UNIT defines the units for infiltration and ventilation
C for each zone and day type. The default value of zero is the same as
C the traditional (implied) unit in the version (ip3ver 0, 1, 2). A value
C of one signals m3/h and a value of 2 signals m3/s.
      integer INFUNIT,IVENTUNIT
      COMMON/P2UNIT/INFUNIT(MCOM,MDTY),IVENTUNIT(MCOM,MDTY)
      
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character vert(35)*33,H*72,KEY*1,title*33
      character LTMP*72
      character afact*8,iflow*8,vflow*8,outs*124,sfile*72,snpfile*72
      logical XST,closezero,closeone,closetwo
      integer KSTR(MDTY)

C Return if the version is not three.
      if(ip3ver(icomp).lt.3)then
        call usrmsg('EDAIR3 called with older version of zone ',
     &    'operation file.','-')
        return
      endif

C Initial menu entry setup.
      idtyp = 1
 5    MHEAD=4
      MCTL=5
      ILEN=NAC(IDTYP)*3
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Check and plot air flows. 
      call CHKAIRFL
      if (MMOD.eq.8) call PLTAIR(ICOMP,idtyp)

C Set up top of menu.
  44  if (ITCTL(ICOMP).eq.0) then 
        write(VERT(1),'(A)') '1 air flow control (none)'
      else
        write(VERT(1),'(2A)') '1 control: ',ctlstr(icomp,1)(1:22)
      endif
      vert(2) =              '  -----------------------------'
      write(vert(3),'(3a,i2,a)') '> air flow>> ',calentag(idtyp),' (',
     &  NAC(IDTYP),')'
      write(vert(4),'(a,i2,a,i2)') '* infil unit',INFUNIT(icomp,1),
     &  ' vent unit',IVENTUNIT(icomp,1)

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

C Day type loop
      istartcount=1
      iendcount=0
      do 10 L=1,NAC(idtyp)

C Ensure that all three lines can be written. If on subsequent
C pages then check loosely in order to catch an item that 
C would have otherwise spanned the page divide.
        iendcount=istartcount+2
        if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
          continue
        elseif(istartcount+2.GE.IST.AND.
     &        (iendcount.LE.(IST+MIFULL)))then
          continue
        else
          istartcount=iendcount+1
          goto 10
        endif
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &    IACS(idtyp,L),'-',IACF(idtyp,L)
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,f6.2)') KEY,'  infil rate:',ACI(idtyp,L)
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        if (IPT(idtyp,L).gt.0) then
          write (VERT(M),'(2a,f6.2,1x,a)') KEY,'  vent rate:',
     &      ACV(idtyp,L),zname(IPT(idtyp,L))(1:10)
        elseif (ACV(idtyp,L).lt.0.0001) then 
          write (VERT(M),'(2a,f6.2,a)') KEY,'  vent rate:',
     &      ACV(idtyp,L),'  n/a'
        else
          write (VERT(M),'(2a,f6.2,a,f6.2)') KEY,'  vent rate:',
     &      ACV(idtyp,L),'@temp:',TA(idtyp,L)
        endif
        istartcount=iendcount+1
 10   continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=           '  -----------------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT             ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2) = '+ add/delete/copy/import flows '
      VERT(M+3) = '! list current information     '
      VERT(M+4) = '? help                         '
      VERT(M+5) = '- exit this menu               '

C Help text for this menu.
      H(1)='Air flows are defined via one or more sequential'
      H(2)='periods, the magnitude at a given time being the'
      H(3)='flow specified a period that includes that time.'
      H(4)=' '
      H(5)='In cases where ventilation will occur with more'
      H(6)='than one modelled zone then a fluid flow network'
      H(7)='or temporal definition file should be employed.'
      H(8)=' '
      H(9)='Note that large changes in flow rates between'
      H(10)='periods can result in unacceptable interpolation'
      H(11)='errors.'
      H(12)=' '
      H(13)='The add/delete/copy/import option are as follows.'
      H(14)=' add - adds a period to one or all day types. BE SURE'
      H(15)='   to read the extended help messages in this dialog'
      H(16)='   to understand how to use the add function to insert'
      H(17)='   a period into a profile.'  
      H(18)=' '
      H(19)=' delete - deletes one or more periods (within a list'
      H(20)='   of all periods in all days).'
      H(21)=' '
      H(22)=' copy - copies periods for different days e.g. Weekdays ->'
      H(23)='   Sundays or Saturdays -> Sundays'
      H(24)=' '
      H(25)=' import - copies infiltration, ventilation and control'
      H(26)='   (or a subset of these data) from another zone.'
      H(27)=' '
      H(28)='When importing you can specify whether to:'
      H(29)=' a) preserve the ac/h being imported, e.g. 1.5 ac/h in'
      H(30)='    the source zone is 1.5 ac/h in the destination zone;'
      H(31)=' b) preserve the volume of air implied by the ac/h in'
      H(32)='    the source zone, e.g. 1.5 ac/h in a 50m^3 source'
      H(33)='    zone becomes 1.07 ac/h in a 70m^3 destination zone.'
 
C Now display the menu.
      if(mmod.eq.8)then
        write (title,'(2a)') 'Air flow in ',
     &    zname(icomp)(1:lnzname(icomp))
      else
        write (title,'(2a)') 'Ventilation in ',
     &    zname(icomp)(1:lnzname(icomp))
      endif
      CALL EMENU(title,VERT,MVERT,IVERT)
      if (IVERT.EQ.MVERT) then
        return
      elseif (IVERT.EQ.(MVERT-1)) then
        CALL PHELPD('flows menu',33,'-',0,0,IER)
      elseif (IVERT.EQ.(MVERT-2)) then

C List current.
        call VENTINF(ICOMP,IUOUT)
      elseif (IVERT.EQ.(MVERT-3)) then

C Add delete copy period or import from another zone.
        H(1)='The options: '
        H(2)='The add/delete/copy/import option are as follows.'
        H(3)=' add - adds a period to one or all day types. IF'
        H(4)='   there is already a defined period which overlaps'
        H(5)='   this period then the volumes of air flow will be '
        H(6)='   summed.  For example: if there is a period from'
        H(7)='   8h00-24h00 with 1ach infiltration and you want to '
        H(8)='   add a period 20h00-24h00 with 0.5 the time from '
        H(9)='   20h00-24h00 will become 1.5 ach.'
        H(10)=' '
        H(11)='   If you want to adjust the profile so the inserted'
        H(12)='   period has a different value then initailly'
        H(13)='   give 0.0 for the flows. After the periods have'
        H(14)='   been revised then edit the newly created period.'
        H(15)='  '
        H(16)='   The alternative approach is to edit the start and'
        H(17)='   end time of the existing period so that a gap is'
        H(18)='   created and then use the add facility to fill in'
        H(19)='   the gap.'
        H(20)=' '
        H(21)=' delete - deletes one or more periods (within a list'
        H(22)='   of all periods in all days).'
        H(23)=' '
        H(24)=' copy - copies periods for different days e.g. Weekdays'
        H(25)='   -> Sundays or Saturdays -> Sundays'
        H(26)=' '
        H(27)=' import - copies infiltration, ventilation and control'
        H(28)='   (or a subset of these data) from another zone.'
        H(29)=' '
        H(30)='When importing you can specify whether to:'
        H(31)=' a) preserve the ac/h being imported, e.g. 1.5 ac/h in'
        H(32)='    the source zone is 1.5 ac/h in the destination zone;'
        H(33)=' b) preserve the volume of air implied by the ac/h in'
        H(34)='    the source zone, e.g. 1.5 ac/h in a 50m^3 source'
        H(35)='    zone becomes 1.07 ac/h in a 70m^3 destination zone.'
        CALL EASKATOG(' ','Options (see help):',
     &    'add period','delete period','copy period',
     &    'import from other zone','import pattern','cancel',' ',IW,35)

C Add period
        if (IW.EQ.1) then 
          CALL EASKAB(' ','Add period to ','one day type',
     &     'all day types',IY,35)
          IF(IY.EQ.1)THEN
            IX=1
            CALL EPICKS(IX,KSTR,' ','choose daytype to add period in'
     &      ,12,nbdaytype,calentag,'Choose daytype',IER,35)
            call edisp(iuout,'choose daytype to add period in')
            IDT=KSTR(1)
            idtyp=idt
            IF(NAC(IDT).EQ.MA)THEN
              CALL USRMSG('Sorry, too many periods in day type ',
     &        calentag(idt),'W')
            ELSE
              IPER=0
              CALL EDAIRFL(ICOMP,IDT,IPER,-1)
            ENDIF
          ELSEIF(IY.EQ.2)THEN
            IDT=-1
            DO 400 IDTY=1,NBDAYTYPE
            IF(NAC(IDTY).EQ.MA)THEN
              CALL USRMSG('Sorry, too many periods in day type ',
     &        calentag(idt),'W')
              CALL EDISP(IUOUT,
     &        'Add periods by individually choosing each day type')
              IDT=0
            ENDIF
 400        CONTINUE
            IF(IDT.NE.0)THEN
              IDT=-1
              IPER=0
              call EDAIRFL(ICOMP,IDT,IPER,-1)
            ENDIF
          ENDIF
        elseif (IW.EQ.2) then 
          call CDAIRFL(1)    ! delete one or more periods
        elseif (IW.EQ.3) then
          call CDAIRFL(2)    ! copy a period
        elseif (IW.EQ.4.or.IW.eq.5) then

C Import flow schedules from another zone. Ask which zone, then
C confirm how much to copy and whether infiltration and ventilation
C flows are to preserve ac/h or volume of flow.
          if(iw.eq.4)then
 38         isource=0
            izdef=0
            call askzone(isource,izdef,
     &        'Air flow pattern from which zone?',
     &        'Source Zone','-','Ventilation import requires a zone.',
     &        ier)
            if (IER.ne.0.or.isource.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current zone (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 38
            endif

C Display a bit of information about this browsed file.
            IUO=IFIL+1
            write(LTMP,'(a)') LPROJ(isource)
            call FINDFIL(LTMP,XST)
            if(XST)then
              call ZSCHEDHINT(IUO,LTMP,IER)
            else
              call USRMSG('The selected file could not be found.',
     &          'Please try again.','W')
              goto 38
            endif
            sfile=' '
          elseif(iw.eq.5)then

C File browse from patterns.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','pat','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','pat','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
C Debug.
C            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 5
              endif

C Display a bit of information about this browsed file.
              IUO=IFIL+1
              call FINDFIL(sfile,XST)
              if(XST)then
                call ZSCHEDHINT(IUO,sfile,IER)
              else
                call USRMSG('The selected file could not be found.',
     &            'Please try again.','W')
                goto 5
              endif
            else

C Nothing selected treat this as a return state.
              call usrmsg('No files available, try again.',' ','W')
              goto 5
            endif
          endif

C Should only ask users these questions if the source file has been found.
          CALL EASKABC(' ','Import:','all flow schedules & ctl',
     &     'infiltration only','cancel',ivopt,21)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'convert to zone volume','cancel',infv,16)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'convert to zone vvolume','cancel',ivntv,16)
            if(ivntv.eq.1) vflow='ach-ach '
            if(ivntv.eq.2) vflow='vol-vol '
            if(ivntv.eq.3) goto 5
          else
            vflow='ach-ach '
          endif
          idest=icomp
          itrc=1

C We have all of the directives, do the import.
          call COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &      itrc,'-',ier)
        endif
      elseif (IVERT.EQ.(MVERT-4)) then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          ILEN=NAC(IDTYP)*3
          goto 44
        ENDIF
      elseif (IVERT.EQ.1) then

C Control of air flow.
        call EDAFCTL(ICOMP,IER)
      elseif (IVERT.EQ.3) then

C Increment the daytype focus.
        idtyp = idtyp + 1
        if(idtyp.gt.nbdaytype) idtyp = 1
        goto 5
      elseif (IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) then

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)

C Depending on currently defined gains work out day type of selected
C item. Also calculate period index in day type. Adjust for 2nd page
        if(ipm.eq.2.and.I.gt.1) I=I-1  ! adjust for 2nd page
        if(ipm.eq.3.and.I.gt.1) I=I-2  ! adjust for 3rd page
        if(ipm.eq.4.and.I.gt.1) I=I-3  ! adjust for 4th page

C Items are in groups of 3. If i/3 has no remainder I is 3,6,9,etc.
C so the period is just the nearest integer to I/3.0. If there is
C a remainder then I is 1,2 or 4,5 and it is one more than the
C nearest integer.
        trymod=amod(real(I),3.0)
        call eclose(trymod,0.0,0.001,closezero)
        call eclose(trymod,1.0,0.001,closeone)
        call eclose(trymod,2.0,0.001,closetwo)
        if(closezero.or.closetwo)then
          try=real(I)/3.0
          iper=nint(try)
        endif
        if(closeone)then
          try=real(I)/3.0
          iper=nint(try)+1
        endif
        IDT=idtyp
        if(closeone)then
          call EDAIRFL(ICOMP,IDT,IPER,1)
        endif
        if(closetwo)then
          call EDAIRFL(ICOMP,IDT,IPER,2)
        endif
        if(closezero)then
          call EDAIRFL(ICOMP,IDT,IPER,0)
        endif
      endif

C Redisplay menu.
      IVERT=-4
      GOTO 5

      END

C ********************* EDAIRFL *********************
C EDAIRFL - edit scheduled air flow data.
C  ICOMP zone number
C  IDT day type
C  IPER period (=0 then new)
C  itype is what attributes to edit (=-1 all, =1 period,
C     =2 infil, =0 ventilation attributes.

      subroutine EDAIRFL(ICOMP,IDT,IPER,itype)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA

      character OUTS*124,HOLD*16,h*72

C Working variables
      real ACIX,ACVX,TAX,XTA,XACH
      integer IACSX,IACFX,IPTX

C << This subroutine will need further code changes to implement
C << user cancel actions.

C If new period then set defaults otherwise get data from common.
      if (IPER.eq.0) then
        IACSX=0
        IACFX=24
        ACIX=0.
        ACVX=0.
        IPTX=0
        TAX=0.
      ELSEIF(IDT.GT.0)THEN
        IACSX=IACS(IDT,IPER)
        IACFX=IACF(IDT,IPER)
        ACIX=ACI(IDT,IPER)
        ACVX=ACV(IDT,IPER)
        IPTX=IPT(IDT,IPER)
        TAX=TA(IDT,IPER)
      endif

C Ask for start and finish times.
      if(itype.eq.-1.or.itype.eq.1)then
 10     write (OUTS,'(A)') 'Start and finish hours for period?'
        write (HOLD,'(2i5,a)') IACSX,IACFX,'  '
        h(1)='Period during which air flow applies.'
        CALL EASKS(HOLD,OUTS,' ',16,' 0 24 ','air st-fn hr',IER,1)
        K=0
        CALL EGETWI(HOLD,K,IVS,0,24,'F','period start',IER)
        CALL EGETWI(HOLD,K,IVF,IVS+1,24,'F','period end',IER)
        if (IER.ne.0) goto 10
        IACSX=IVS
        IACFX=IVF
      endif

C Ask for infiltration and ventilation flow rates.
C Can be specified as:ac/h or m^3/s.
      if(itype.eq.-1.or.itype.eq.2.or.itype.eq.0)then
        h(1)='An air flow rate can be specified in air change per '
        h(4)='hour or m^3/s.'
        IGU=1
        CALL EASKAB(' ','Air flow specified in:','AC/h',
     &     'm^3/s',IGU,2)
        if (IGU.eq.1) then

          if(itype.eq.-1.or.itype.eq.2)then
C Flow as ac/h.
 20         XACH=ACIX
            h(1)='Supply the infiltration rate in ac/h.'
            call EASKR(XACH,' ','Infiltration rate?',
     &        0.0,'W',10.0,'W',0.5,'infil rate ach',IERI,1)
            if(IERI.eq.0)then
              ACIX=XACH
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 20
            endif
          endif
        
          if(itype.eq.-1.or.itype.eq.0)then
 22         XACH=ACVX
            h(1)='Supply the ventilation rate in ac/h relative'
            h(2)='to the volume of the receiving zone.'
            call EASKR(XACH,' ','Ventilation rate?',
     &        0.0,'W',10.0,'W',0.0,'vent rate ach',IERI,2)
            if(IERI.eq.0)then
              ACVX=XACH
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 22
            endif
          endif
        elseif (IGU.eq.2) then

C Flow as m^3/s.
 30       if(itype.eq.-1.or.itype.eq.2)then
            XACH=ACIX*VOL(ICOMP)/3600.
            h(1)='Supply the infiltration rate in m^3/s.'
            call EASKR(XACH,' ','Infiltration rate?',
     &        0.0,'W',10.0,'W',0.0,'inf rate m3s',IERI,1)
            if(IERI.eq.0)then
              ACIX=XACH*3600.0/VOL(ICOMP)
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 30
            endif
          endif

          if(itype.eq.-1.or.itype.eq.0)then
            XACH= ACVX*VOL(ICOMP)/3600.
            h(1)='Supply the ventilation rate in m^3/s relative.'
            h(2)='to the volume of the receiving zone.'
            call EASKR(XACH,' ','Ventilation rate?',
     &        0.0,'W',10.0,'W',0.0,'vent rate m3s',IERI,2)
            if(IERI.eq.0)then
              ACVX=XACH*3600.0/VOL(ICOMP)
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 30
            endif
          endif
        endif
      endif

C If the ventilation rate is greater than zero then further
C checks are carried out.
      if(itype.eq.-1.or.itype.eq.0)then
        if (ACVX.gt.0.0) then

C Ask for source zone/ temperature.
          CALL EASKABC(' ','Ventilation air temperature:',
     &      'other modelled zone','constant',
     &      'ambient',IGU,0)
          if (IGU.eq.1) then

C Present zone list.
 38         IV=IPTX
            call askzone(IV,IPTX,'Ventilation from which zone?',
     &     'Source Zone','-','Ventilation definition requires a zone.',
     &       ier)
            if (IER.ne.0.or.IV.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current zone (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 38
            endif
            ratio = VOL(ICOMP)/VOL(IV)
            write (OUTS,'(a,F8.3,a)') 
     &        'Ventilation (ac/h where 1 ac/h in current zone =',
     &        ratio,' ac/h in source zone)'
            call EDISP(IUOUT,OUTS)
            write (OUTS,'(a,F8.2,a,F8.2,a)') 
     &        '(Current zone volume =',VOL(icomp),
     &        'Source zone volume =',VOL(iv),')'
            call EDISP(IUOUT,OUTS)
            IPTX=IV
            TAX=0.
          elseif (IGU.eq.2) then

C Ask for source temperature.
 40         XTA=  TAX
            h(1)='Supply the ventilation air temperature in degC.'
            call EASKR(XTA,' ','Supply temperature?',
     &        0.0,'F',50.0,'W',0.5,'vent src temp',IERI,1)
            if(IERI.eq.0)then
              IPTX=0
              TAX=XTA
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 40
            endif
          elseif (IGU.eq.3) then

C Set vent to zero and increase infiltration.
            call USRMSG('Supply of outside air being treated as',
     &       'infiltration. Updating infiltration flow rate.','W')
            ACIX=ACIX+ACVX
            ACVX=0
            IPTX=0
            TAX=0.0
          endif
        endif
      endif

C If day type -1 then add data to all day types.
      if (IDT.eq.-1) then
        ILOOP=NBDAYTYPE
      else
        ILOOP=1
      endif

C If new period then work out where to insert data.
      do 400 ILDT=1,ILOOP

C If looping through day types then set day type (IDT) to current 
C value of loop counter (ILDT).  Otherwise IDT will have the value 
C that was passed to the subroutine.
        if (ILOOP.NE.1) IDT=ILDT
        call ADDAFP(IPER,IDT,IACSX,IACFX,ACIX,ACVX,IPTX,TAX)
 400  continue

      return
      end

C ************************ ADDAFP ************************
C ADDAFP add air flow period in correct location (time based).
C Other routines assume that periods have been added in the order 
C resulting from the sorting done here.
C IPER=0 if its a new period
C IDT=day type wkday=1, sat=2, sun=3, other types if specified
C IACS,IACF period start and finish time
C ACI,ACV infiltration and ventilation rates
C IPT,TA ventilation source zone, if IPT=0 then TA is source temperature

      subroutine ADDAFP(IPER,IDT,IACSX,IACFX,ACIX,ACVX,IPTX,TAX)
#include "building.h"

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA

      if (IPER.eq.0) then
        ILPER=1
        DO 401 I=1,NAC(IDT)
          if (iabs(IACS(IDT,I)).le.iabs(IACSX)) then
            ILPER=I
            if (IACS(IDT,I).lt.IACSX) then
              ILPER=I+1
            elseif(IACS(IDT,I).eq.IACSX.and.IACF(IDT,I).gt.IACFX)then
              ILPER=I
            endif
          endif
 401    CONTINUE
        NAC(IDT)=NAC(IDT)+1
        INC=NAC(IDT)
      else
        ILPER=IPER
        INC=IPER
      endif

C Now update common blocks.
      do 300 IX=INC,ILPER,-1
        if (IX.eq.ILPER) then
          IACS(IDT,IX)=IACSX
          IACF(IDT,IX)=IACFX
          ACI(IDT,IX)=ACIX
          ACV(IDT,IX)=ACVX
          IPT(IDT,IX)=IPTX
          TA(IDT,IX)=TAX
        else
          IACS(IDT,IX)=IACS(IDT,IX-1)
          IACF(IDT,IX)=IACF(IDT,IX-1)
          ACI(IDT,IX)=ACI(IDT,IX-1)
          ACV(IDT,IX)=ACV(IDT,IX-1)
          IPT(IDT,IX)=IPT(IDT,IX-1)
          TA(IDT,IX)=TA(IDT,IX-1)
        endif
 300  continue

      return
      end

C ********************* CDAIRFL *********************
C CDAIRFL - copy/ delete air flow data.
C  ICD - copy (=2) delete (=1) flag

      subroutine CDAIRFL(ICD)
#include "building.h"
#include "geometry.h"

      common/pophelp/h(60)

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA

      character h*72,VERT(3*MA)*35,OUTS*124,OUT*4
      INTEGER IGOON,ILET,IDT,IDTY,IAFP,IX,KSTR(MDTY),ilistlen,ilm

      dimension IVERT(3*MA),ITEMP(3*MA)

      ilistlen=0

C Set up menu of current definitions.
      DO 399 IDTY=1,NBDAYTYPE
        ilistlen=ilistlen+NAC(IDTY)
 399  CONTINUE

C Loop through the items until the page to be displayed.
C Build up text strings for the menu. 
      M=0
      DO 400 IDTY=1,NBDAYTYPE
        DO 10 L=1,NAC(IDTY)
          M=M+1
          write(out,'(A)')calentag(idty)(1:4)
          if (IPT(IDTY,L).gt.0) then
            write(VERT(M),303)OUT,IACS(IDTY,L),IACF(IDTY,L),ACI(IDTY,L),
     &      ACV(IDTY,L),zname(IPT(IDTY,L))(1:9)
          else
            if (ACV(IDTY,L).lt.0.0001) then 
              write(VERT(M),323)OUT,IACS(IDTY,L),IACF(IDTY,L),
     &        ACI(IDTY,L),ACV(IDTY,L)
            else
              write(VERT(M),313)OUT,IACS(IDTY,L),IACF(IDTY,L),
     &        ACI(IDTY,L),ACV(IDTY,L),TA(IDTY,L)
            endif
          endif
  303     FORMAT(A,' ',2i4,1x,2f6.2,1x,a)
  313     FORMAT(A,' ',2i4,1x,2f6.2,1x,f6.1)
  323     FORMAT(A,' ',2i4,1x,2f6.2,1x,'  n/a')
 10     continue
 400  CONTINUE

C To delete period(s) redisplay the menu and allow user to indicate
C which one, then collapse the period list.
      if (ICD.eq.1) then
        H(1)='One or more of the periods may be deleted.'
        call EPICKS(ilistlen,IVERT,' ','Air flow period(s) to delete:',
     &         35,M,VERT,'Air flow period to delete',IER,1)

C Delete items in IVERT array. Sort IVERT first.
        if (ilistlen.gt.0) then
          KFLAG=-1
          call SORTI(IVERT,ITEMP,NBDAYTYPE*MA,KFLAG)
          do 100 IJ=1,ilistlen
            IAFP=IVERT(IJ)
            IGOON=1
            ILET=0
            ILM=0
            DO 401 IDT=1,NBDAYTYPE
              IF(IGOON.EQ.1)THEN
                ILET=ILET+NAC(IDT)
                IF(IAFP.GT.ILET)THEN
                  IGOON=1
                  ILM=ILM+NAC(IDT)
                ELSE
                  IGOON=0
                ENDIF
                IDTY=IDT
              ENDIF
 401        CONTINUE
            DO 402 IX=IAFP-ILM,NAC(IDTY)
              if (IX.ne.MA) then
                IACS(IDTY,IX)=IACS(IDTY,IX+1)
                IACF(IDTY,IX)=IACF(IDTY,IX+1)
                ACI(IDTY,IX)=ACI(IDTY,IX+1)
                ACV(IDTY,IX)=ACV(IDTY,IX+1)
                IPT(IDTY,IX)=IPT(IDTY,IX+1)
                TA(IDTY,IX)=TA(IDTY,IX+1)
              endif
 402        CONTINUE
            NAC(IDTY)=NAC(IDTY)-1
            M=M-1
 100      continue
        endif
      else

C Copy periods.
        h(1)='All the casual gains associated with a specified day type'
        h(2)='can be copied to another day type.'
        IX=1
        CALL EDISP(IUOUT,'choose daytype to copy periods from')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods from',
     &  12,nbdaytype,calentag,'Choose daytype',IER,35)
        IDTY=KSTR(1)
        CALL EDISP(IUOUT,'choose daytype to copy periods to')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods to',
     &  12,nbdaytype,calentag,'Choose daytype',IER,35)
        IDT=KSTR(1)
     	do 210 IX=1,NAC(IDTY)
     	  if (NAC(IDT).lt.MA) then
     	    IPER=0
     	    IACSX=IACS(IDTY,IX)
     	    IACFX=IACF(IDTY,IX)
     	    ACIX=ACI(IDTY,IX)
     	    ACVX=ACV(IDTY,IX)
     	    IPTX=IPT(IDTY,IX)
     	    TAX=TA(IDTY,IX)
     	    call ADDAFP(IPER,IDT,IACSX,IACFX,ACIX,ACVX,IPTX,TAX)
     	    call CHKAIRFL
          ELSE
            WRITE(OUTS,'(2a)')'Sorry, too many periods in day type ',
     &      calentag(idt)
            call usrmsg(' ',outs,'W')
     	  endif
 210 	continue
      endif
      return
      end

C ********************* COPYAIRFL *********************
C COPYAIRFL - copy an air flow regime from zone isource to
C zone idest based on afact, iflow , vflow
C afact (8 char) = 'all' copies both infiltration and ventilation
C       descriptions as well as any control actions (this
C       will need a QA by the user in case of conflicts).
C afact = 'infil' copies only the infiltration data to the
C       destination zone file.
C iflow (8 char) = 'ach-ach' preserves infiltration air change rates
C   e.g. 1.5 ach in source zone is also 1.5 ach in destination zone.
C iflow = 'vol-vol' preserves volume represented by the infiltration
C   air change rate in the source to an ach in the destination that
C   represents the same volume flow rate.
C vflow (8 char) = 'ach-ach' preserves ventialtoin air change rates
C   e.g. 1.5 ach in source zone is also 1.5 ach in destination zone.
C vflow = 'vol-vol' preserves volume represented by the ventilation
C   air change rate in the source to an ach in the destination that
C   represents the same volume flow rate.
C sfile (char*(*)) name of operations file outwith the model which
C   is used if isource is zero.
C itrc - if zero then work silently.
C act = '-' for normal, 's' for silent.

C << issue to be resolved later...if the user copies a pattern and
C << later on revises the zone geometry this might imply that a
C << different flow rate is required. Do we need to remember the
C << initial intent to guide later revisions?

      subroutine COPYAIRFL(isource,idest,afact,iflow,vflow,sfile,
     &  itrc,act,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)
      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      COMMON/P1C/oprdescc,ctlstrc
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Scratch copy of air flow pattern from an operations file.
      COMMON/P2CN/NACC(MDTY),IACSC(MDTY,MA),IACFC(MDTY,MA),
     &ACIC(MDTY,MA),ACVC(MDTY,MA),IPTC(MDTY,MA),TAC(MDTY,MA)
      INTEGER NACC,IACSC,IACFC,IPTC
      REAL ACIC,ACVC,TAC

      COMMON/P2CTLC/ITCTLC,TLOC,TUPC,THIC,ACILC,ACVLC,IVLC,TALC,
     &   ACIUC,ACVUC,IVUC,TAUC,ACIHC,ACVHC,IVHC,TAHC

      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

      character*(*) sfile
      character h*72
      CHARACTER oprdescc*248,ctlstrc*24
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,ltmp*72
      character fs*1,GFILE*72,afact*8,iflow*8,vflow*8
      character act*1   ! s for silent, - for normal
      LOGICAL XST,unixok
      INTEGER IDTY
      character actflatfile*1

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Clear the operations data structures related to air prior
C to reading in source.
      IER=0
      DO 400 IDTY=1,NBDAYTYPE
        NAC(IDTY)=0
 400  CONTINUE
      ctlstr(idest,1)='no control of air flow  '
      ctlstr(idest,2)='no control of air flow  '
      ctlstr(idest,3)='no control of air flow  '
      oprdesc(idest)='Flow schedules imported. No other notes (yet).'

C First scan the source zone operation file if there is a problem
C reading or it does not exist then return an error state.
C If the user has asked for a verbosity list out the details of
C the source zones air flows.
      IUO=IFIL+1
      if(isource.eq.0)then
        CALL EFOPSEQ(IUO,sfile,1,IER)
        if(ier.eq.0)then
          XST = .true.
          CALL ERPFREE(IUO,ISTAT)
        endif
      else
        call FINDFIL(LPROJ(isource),XST)
      endif
      if(XST)then
        CALL ERPFREE(IUO,ISTAT)
        if(isource.eq.0)then
          if(ifullysilent.eq.1)then
            actflatfile='-'
          else
            actflatfile='W'            
          endif
          
C Use ERZSCHED to fill P2C and P2CTLC common blocks and
C then jump to 32 to read in the destination operations file.
          CALL ERZSCHED(IUO,sfile,actflatfile,IER)
          goto 32
        else

C Read the source operations file from within the model and
C if this is successful the instanciate the P2C and P2CTLC
          CALL EROPER(ITRC,IUOUT,IUO,isource,IER)
        endif
        if(ier.ne.0)then
          call edisp(iuout,'Source operation file not clean.')
          ier=1
          return
        endif
      else
        call edisp(iuout,'Source operation file not found.')
        ier=1
        return
      endif

C Copy the air flow and controls information into the local arrays
      DO 401 IDTY=1,NBDAYTYPE
        NACC(IDTY)=NAC(IDTY)
        if(NAC(IDTY).gt.0)then
          do 10 L=1,NAC(IDTY)
            IACSC(IDTY,L)=IACS(IDTY,L)
            IACFC(IDTY,L)=IACF(IDTY,L)
            ACIC(IDTY,L)=ACI(IDTY,L)
            ACVC(IDTY,L)=ACV(IDTY,L)
            IPTC(IDTY,L)=IPT(IDTY,L)
            TAC(IDTY,L)=TA(IDTY,L)
  10      continue
        endif
 401  CONTINUE

C Copy air flow control into local variables.
C << needs to be updated for separate days and periods >>

      ctlstrc=ctlstr(isource,1)
      ITCTLC=ITCTL(isource)
      TLOC=TLO(isource)
      TUPC=TUP(isource)
      THIC=THI(isource)
      ACILC=ACIL(isource)
      ACVLC=ACVL(isource)
      IVLC=IVL(isource)
      TALC=TAL(isource)
      ACIUC=ACIU(isource)
      ACVUC=ACVU(isource)
      IVUC=IVU(isource)
      TAUC=TAU(isource)
      ACIHC=ACIH(isource)
      ACVHC=ACVH(isource)
      IVHC=IVH(isource)
      TAHC=TAH(isource)

C Scan in the destination operations file, if it does not exist
C then clear casual gain data structure and create a new file.
  32  continue
      call FINDFIL(LPROJ(idest),XST)
      if(XST)then
        CALL ERPFREE(IUO,ISTAT)
        CALL EROPER(ITRC,iuout,IUO,idest,IER)
        if(ier.ne.0)then
          call edisp(iuout,'Destination operation file not clean.')
          ier=1
          return
        endif
      else

C IF asked to copy all then 
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(GFILE,'(2a)') zname(idest)(1:lnzname(idest)),'.opr'
        else
          WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(idest)(1:lnzname(idest)),'.opr'
        endif
        write(ltmp,'(a)') LPROJ(idest)(1:lnblnk(LPROJ(idest)))
        if(act(1:1).eq.'s')then
          LPROJ(idest)=ltmp
        else
          call edisp(iuout,'Destination operation file not found.')
          h(1)='This is the suggested name for the file that will'
          h(2)='hold the imported air flow schedules data. '
          CALL EASKS(ltmp,' New zone operations file: (confirm) ',
     &      ' ',72,GFILE,'new operations file',IER,2)
          LPROJ(idest)=ltmp
        endif

C And any new file will be in the sorted format.
        ip3ver(idest)=2
      endif
      if(iflow(1:7).eq.'ach-ach')then
        ratioif=1.0
      elseif(iflow(1:7).eq.'vol-vol')then

C Copy the local array data into the common blocks overwriting
C the data implied by 'afact'. Also act on the iflow vflow directives.
        if(isource.eq.0)then
          VAL=10.0
          h(1)='Because this is a pattern file there is no associated'
          h(2)='zone and to do a ratio we need an assumed volume. '
          CALL EASKR(VAL,' ','Assumed volume of source?',
     &        0.10,'F',9999.0,'W',0.0,'assumed volume',IER,2)
          ratioif=VAL/VOL(idest)
        else
          ratioif=VOL(isource)/VOL(idest)
        endif
      endif
      if(vflow(1:7).eq.'ach-ach')then
        ratiovf=1.0
      elseif(vflow(1:7).eq.'vol-vol')then
        if(isource.eq.0)then
          ratiovf=VAL/VOL(idest)
        else
          ratiovf=VOL(isource)/VOL(idest)
        endif
      endif
      if(afact(1:5).eq.'all  '.or.afact(1:5).eq.'infil')then
        DO 403 IDTY=1,NBDAYTYPE
          if(NACC(IDTY).gt.0)then
            do 11 L=1,NACC(IDTY)
              NAC(IDTY)=NACC(IDTY)
              IACS(IDTY,L)=IACSC(IDTY,L)
              IACF(IDTY,L)=IACFC(IDTY,L)
              ACI(IDTY,L)=ACIC(IDTY,L)/ratioif
              ACV(IDTY,L)=ACVC(IDTY,L)/ratiovf
              IPT(IDTY,L)=IPTC(IDTY,L)
              TA(IDTY,L)=TAC(IDTY,L)
  11        continue
          endif
 403    CONTINUE
        if(afact(1:5).eq.'all  ')then

C Apply ach or vol conversion to controls as well.
C << update for control days and periods >>
          ctlstr(idest,1)=ctlstrc
          ITCTL(idest)=ITCTLC
          TLO(idest)=TLOC
          TUP(idest)=TUPC
          THI(idest)=THIC
          ACIL(idest)=ACILC/ratioif
          ACVL(idest)=ACVLC/ratiovf
          IVL(idest)=IVLC
          TAL(idest)=TALC
          ACIU(idest)=ACIUC/ratioif
          ACVU(idest)=ACVUC/ratiovf
          IVU(idest)=IVUC
          TAU(idest)=TAUC
          ACIH(idest)=ACIHC/ratioif
          ACVH(idest)=ACVHC/ratiovf
          IVH(idest)=IVHC
          TAH(idest)=TAHC
        endif
      endif

C If the user has asked for verbosity list out the result.
      if(itrc.gt.0)then
        call edisp(iuout,' ')
        call edisp(iuout,'Updated data.')
        call edisp(iuout,' ')
        CALL VENTINF(idest,iuout)
        call edisp(iuout,' ')
      endif

C Save the destination operation file.
      call usrmsg(
     &  'Updating model to reflect changes in destination',
     &  'operations data or file name...','-')
      CALL EMKOPER(IUO,LPROJ(idest),idest,IER)
      CALL EMKCFG('-',ier)
      call usrmsg(
     &  'Updating model to reflect changes in destination',
     &  'operations data or file name...done.','-')

      return
      end

C ********************* CHKAIRFL *********************
C CHKAIRFL - check air flow data for overlapping periods.

      subroutine CHKAIRFL
#include "building.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA

      logical RECHECK

 5    RECHECK=.false.
      Ifrom=0
      Ito=0
      Inew=0

C Check all daytypes.
      DO 400 IDTY=1,NBDAYTYPE

C I is the earlier of the two periods being checked.
C J is the later of the two periods.
C If periods I and J overlap (start time for J within period I) then
C copy data for all periods up one slot (if room) and insert new period.
C Therefore the data in period J becomes J+1 and the new period J is the 
C overlapping period of the two initial periods.
      do 10 I=1,NAC(IDTY)
        if (I+1.le.NAC(IDTY)) then
        do 15 J=I+1,NAC(IDTY)
          if (IACS(IDTY,J).ge.IACS(IDTY,I).and.
     &    IACS(IDTY,J).lt.IACF(IDTY,I)) then

C Start time of period J is between start and finish times of period I.
C Create a new period by splitting period J, only if start or finish time 
C of overlapping periods in not equal.
C     |-------------------------|
C     |                         |
C     ||.or.|-----------| ..or.||.or.| 
C     ||    |           |      ||    |
C  -----------------------------

            if (IACS(IDTY,J).eq.IACS(IDTY,I).and.
     &      IACF(IDTY,J).gt.IACF(IDTY,I)) then
C
C     |----I------------|
C     |                 |
C     ||---J------- ----|--| J
C     ||                |  |
C  -----------------------------
              Ifrom=J
              Ito=I
              Inew=I
              IACS(IDTY,J)=IACF(IDTY,I)
            elseif (IACS(IDTY,J).eq.IACS(IDTY,I).and.
     &      IACF(IDTY,J).lt.IACF(IDTY,I)) then
C
C     |----I------------|
C     |                 |
C     ||---J-------|    |
C     ||           |    |  
C  -----------------------------
              Ifrom=J
              Ito=I
              Inew=I

C By starting period I at finish of J periods will be out of order,
C so swap period data from I to J and J to I.
              IACS(IDTY,I)=IACF(IDTY,J)
              IACStmp=IACS(IDTY,I)
              IACFtmp=IACF(IDTY,I)
              ACItmp=ACI(IDTY,I)
              ACVtmp=ACV(IDTY,I)
              IPTtmp=IPT(IDTY,I)
              TAtmp=TA(IDTY,I)
              IACS(IDTY,I)=IACS(IDTY,J)
              IACF(IDTY,I)=IACF(IDTY,J)
              ACI(IDTY,I)=ACI(IDTY,J)
              ACV(IDTY,I)=ACV(IDTY,J)
              IPT(IDTY,I)=IPT(IDTY,J)
              TA(IDTY,I)=TA(IDTY,J)
              IACS(IDTY,J)=IACStmp
              IACF(IDTY,J)=IACFtmp
              ACI(IDTY,J)=ACItmp
              ACV(IDTY,J)=ACVtmp
              IPT(IDTY,J)=IPTtmp
              TA(IDTY,J)=TAtmp
            elseif (IACF(IDTY,J).eq.IACF(IDTY,I).and.
     &      IACS(IDTY,J).ne.IACS(IDTY,I)) then

C     |---I------------|
C     |                |
C     |   |---J-------||
C     |.  |           ||
C  -----------------------------
C Finishes are the same and starts are different.
              Ifrom=I
              Ito=J
              Inew=J
              IACF(IDTY,I)=IACS(IDTY,J)
            elseif (IACS(IDTY,J).gt.IACS(IDTY,I).and.
     &      IACF(IDTY,J).lt.IACF(IDTY,I)) then

C     |---I------------|
C     |                |
C     |...|---J-----|  |
C     |.  |         |  |
C  -----------------------------
C Later period starts after earlier period and later period finishes before
C the earlier period.
              if (NAC(IDTY).lt.MA) then
                Ifrom=I
                Ito=J
                Inew=J
                do 16 K=NAC(IDTY),J,-1
                  IACS(IDTY,K+1)=IACS(IDTY,K)
                  IACF(IDTY,K+1)=IACF(IDTY,K)
                  ACI(IDTY,K+1)=ACI(IDTY,K)
                  ACV(IDTY,K+1)=ACV(IDTY,K)
                  IPT(IDTY,K+1)=IPT(IDTY,K)
                  TA(IDTY,K+1)=TA(IDTY,K)
 16             continue
                NAC(IDTY)=NAC(IDTY)+1
                IACF(IDTY,J+1)=IACF(IDTY,I)
                IACF(IDTY,I)=IACS(IDTY,J)
                IACS(IDTY,J+1)=IACF(IDTY,J)
                ACI(IDTY,J+1)=ACI(IDTY,I)
                ACV(IDTY,J+1)=ACV(IDTY,I)
                IPT(IDTY,J+1)=IPT(IDTY,I)
                TA(IDTY,J+1)=TA(IDTY,I)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined. ','W')
              endif
            elseif (IACS(IDTY,J).gt.IACS(IDTY,I).and.
     &      IACF(IDTY,J).gt.IACF(IDTY,I)) then

C     |---I------------|
C     |                |
C     |...|---J--------|--|
C     |.  |            |  |
C  -----------------------------
C If later period starts after earlier period and latter period finishes
C after the earlier period.
              if (NAC(IDTY).lt.MA) then
                Ifrom=J+1
                Ito=I
                Inew=J
                do 17 K=NAC(IDTY),J,-1
                  IACS(IDTY,K+1)=IACS(IDTY,K)
                  IACF(IDTY,K+1)=IACF(IDTY,K)
                  ACI(IDTY,K+1)=ACI(IDTY,K)
                  ACV(IDTY,K+1)=ACV(IDTY,K)
                  IPT(IDTY,K+1)=IPT(IDTY,K)
                  TA(IDTY,K+1)=TA(IDTY,K)
 17             continue
                NAC(IDTY)=NAC(IDTY)+1
                IACF(IDTY,J)=IACF(IDTY,I)
                IACF(IDTY,I)=IACS(IDTY,J)
                IACS(IDTY,J+1)=IACF(IDTY,J)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined. ','W')
              endif
            else
              call USRMSG(
     &      'New air flow period defined for same period as existing',
     &      'period. Combining data, please check/ edit existing.','W')
              Ifrom=0
              ACI(IDTY,I)=ACI(IDTY,I)+ACI(IDTY,J)
              ACV(IDTY,I)=ACV(IDTY,I)+ACV(IDTY,J)
              do 18 K=I,NAC(IDTY)
                if ((K+1).gt.MA) then
                  IACS(IDTY,K)=0
                  IACF(IDTY,K)=0
                  ACI(IDTY,K)=0.
                  ACV(IDTY,K)=0.
                  IPT(IDTY,K)=0
                  TA(IDTY,K)=0.
                else
                  IACS(IDTY,K)=IACS(IDTY,K+1)
                  IACF(IDTY,K)=IACF(IDTY,K+1)
                  ACI(IDTY,K)=ACI(IDTY,K+1)
                  ACV(IDTY,K)=ACV(IDTY,K+1)
                  IPT(IDTY,K)=IPT(IDTY,K+1)
                  TA(IDTY,K)=TA(IDTY,K+1)
                endif
 18           continue
              NAC(IDTY)=NAC(IDTY)-1
            endif

C Try and set data in new period.
            if (Ifrom.gt.0) then
              ACI(IDTY,Inew)=ACI(IDTY,Ifrom)+ACI(IDTY,Ito)
              ACV(IDTY,Inew)=ACV(IDTY,Ifrom)+ACV(IDTY,Ito)
              if(ACV(IDTY,Ifrom).gt.0.0.and.ACV(IDTY,Ito).lt.0.0001)then

C Ventilation defined for period Ifrom but not for period Ito, use defn from Ifrom
                IPT(IDTY,Inew)=IPT(IDTY,Ifrom)
                TA(IDTY,Inew)=TA(IDTY,Ifrom)
              elseif
     &        (ACV(IDTY,Ito).gt.0.0.and.ACV(IDTY,Ifrom).lt.0.0001) then

C Ventilation defined for period I but not for period Ifrom, use defn from Ito
                IPT(IDTY,Inew)=IPT(IDTY,Ito)
                TA(IDTY,Inew)=TA(IDTY,Ito)
              elseif 
     &        (ACV(IDTY,Ito).gt.0.0.and.ACV(IDTY,Ifrom).gt.0.0) then

C Two ventilation sources defined, default to use defn from Ifrom
                IPT(IDTY,Inew)=IPT(IDTY,Ifrom)
                TA(IDTY,Inew)=TA(IDTY,Ifrom)
                if (IPT(IDTY,Ito).ne.IPT(IDTY,Ifrom)) then
                call USRMSG('Could not resolve ventilation source for',
     &                 'overlapping period, please check sources.','W')
                endif
              else

C No vent defined.
                IPT(IDTY,Inew)=0
                TA(IDTY,Inew)=0.
              endif

C Jump out of loop and start checking again.
              RECHECK=.true.
              goto 10
            endif
          endif
 15     continue
        endif
 10   continue
 400  CONTINUE

C If changes have been made then recheck all definitions.
      if (RECHECK) goto 5
      return
      end

C ********************* PLTAIR *********************
C PLTAIR - plot air flow data.
C  ICOMP zone number

      subroutine PLTAIR(ICOMP,idtyp)
#include "building.h"
#include "geometry.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT,IDTY,IDAY,JDAY,KDAY
      REAL ACI,ACV,TA

      character outs*124
      dimension TACI(24),TACV(24)
#ifdef OSI
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2
#endif

      IDTY=IDTYP
      IDAY=IDTY-1
      JDAY=IDTY
      KDAY=IDTY+1
      IF(IDAY.LE.0)IDAY=NBDAYTYPE
      IF(KDAY.GT.NBDAYTYPE)KDAY=1

C Scan each days gains and find maximum flow.
      YMAX=1.
      MXPERS=max(NAC(IDAY),NAC(JDAY),NAC(KDAY))
      do 10 I=1,MXPERS
       YMAX=amax1(YMAX,ACI(IDAY,I),ACV(IDAY,I),ACI(JDAY,I),ACV(JDAY,I),
     & ACI(KDAY,I),ACV(KDAY,I))
 10   continue

C Clear and setup plot area.
      CALL startbuffer()
      CALL win3d(menuchw,10,10,8,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igheight=igwh

C Calculate plotting box locations.
      ipltwd=int(igw/3)-6
      iglsat=igl + int(ipltwd) + 9
      iglsun=igl+2*(int(ipltwd)+9)
      plxmin=0.
      plxmax=24.
      YMAX=YMAX*1.1
      YMIN=0.
      YMAXF=YMAX*VOL(ICOMP)/3600.

C Scale and draw right hand (m^3/s) axis first.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAXF,
     &                                         xsc,ysc,sca,xadd,yadd)
      call vrtaxis(YMIN,YMAXF,igr,igb,igt,yadd,ysc,0,1,'Flow (m^3/s)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAX,
     &                                         xsc,ysc,sca,xadd,yadd)

C Scale and draw for first day type requested
      CALL horaxis(plxmin,plxmax,igl,igr,igb,xadd,xsc,0,'Time (hrs)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,yadd,ysc,0,0,'Flow (ac/hr)')
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)

C Scale and draw second day type requested
      CALL horaxis(plxmin,plxmax,iglsat,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMIM,iupdown,isym)

C Scale and draw third day type requested
      CALL horaxis(plxmin,plxmax,iglsun,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmin,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmax,YMAX,iupdown,isym)

C Display air flow curves.
C first day type requested
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      do 50 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 50   continue
      do 60 IP=1,NAC(IDAY)
        do 65 IHR=IACS(IDAY,IP),IACF(IDAY,IP)-1
          TACI(IHR+1)=ACI(IDAY,IP)
          TACV(IHR+1)=ACV(IDAY,IP)
 65     continue
 60   continue
      do 70 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 70   continue

C second day type requested
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      do 150 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 150  continue
      do 160 IP=1,NAC(JDAY)
        do 165 IHR=IACS(JDAY,IP),IACF(JDAY,IP)-1
          TACI(IHR+1)=ACI(JDAY,IP)
          TACV(IHR+1)=ACV(JDAY,IP)
 165    continue
 160  continue
      do 170 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 170  continue
      call forceflush()

C third day type requested
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      do 250 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 250  continue
      do 260 IP=1,NAC(KDAY)
        do 265 IHR=IACS(KDAY,IP),IACF(KDAY,IP)-1
          TACI(IHR+1)=ACI(KDAY,IP)
          TACV(IHR+1)=ACV(KDAY,IP)
 265    continue
 260  continue
      do 270 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 270  continue

C Title and labels.
      write (outs,'(2a)') 'Scheduled air flows for ',ZNAME(ICOMP)
      iside=1
      isize=1
      ifont=1
      call viewtext(outs,iside,isize,ifont)
      iid1=igl+20; iid2=igt-65;
      call wstxpt(iid1,iid2,'Infiltration')
      iid1=igr-100; iid2=igt-65;
      call wstxpt(iid1,iid2,'Ventilation')
      call pixel2u(igl+120,igt-69,gx,gy)
      iupdown=0
      isym=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igl+180,igt-69,gx,gy)
      iupdown=1
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-120,igt-69,gx,gy)
      iupdown=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-180,igt-69,gx,gy)
      iupdown=-4
      CALL etplot(gx,gy,iupdown,isym)

      ioff=(int(ipltwd)/3)
      iid1=igl+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(IDAY))
      iid1=iglsat+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(JDAY))
      iid1=iglsun+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(KDAY))
      call forceflush()

      return
      end


C ********************* EDCAS *********************
C EDCAS: Edits casual gains portion of a zone operations file.
C Returns: ICGS*(),ICGF*(),CMGS*(),CMGL*(),RADC*(),CONC*() common.
C ITRC is the trace level, IER=0 is OK.
      SUBROUTINE EDCAS(ICOMP,modopr,sorted,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/user/browse

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)

C How many of each gain type is there each zone/gaintype/daytype.
      common/loadcnt/loadcount(mcom,7,MDTY),loadm2count(mcom,7,MDTY),
     &               loadmixed(mcom,7,MDTY)
      logical browse,ok,silent,dok,XST
      logical sorted,modopr

      CHARACTER vert(35)*33,outs*124,KEY*1,sfile*72,snpfile*72
      character lodlabel*6,typstr*8,H*72
      character title*28

C iwatt (8 char) = 'W-W' for absolute W values preserves value
C   e.g. 123W in source zone is also 123W in destination zone.
C iwatt= 'W-m2-W' preserves implied W/m^2 base area implied by
C   absolute W in source zone in destination zone which has a
C   different base area. e.g. 123W in source zone of 2.3m^2 becomes
C   267W in a 5.0m^2 base zone.
      character act*8,iwatt*8
      integer kstr,ILLEN,IW,IQ,IX,I,IDU,IDT,IOPT,ICGTS,IDTYP
      dimension kstr(MDTY),ILLEN(MDTY)

C If not browsing, take the information from loadcnt
C and decide if the periods need to be updated.
      if(.NOT.browse)then
        if(ip3ver(ICOMP).eq.0)then
          dok=.true.
          h(1)='You were presented with this message because the'
          h(2)='file is in an older format which might have overlaps'
          h(3)='in the casual gain periods. The current definition'
          h(4)='of casual gain periods does not support overlapping'
          h(5)='periods and stores the periods by time and day type.'
          h(6)=' '
          h(7)='It is recommended that you say YES and then check that'
          h(8)='the updated periods are correct.'
          call askok(' ','Sort and remove overlaps from casual gains?',
     &                 OK,DOK,8)
        else
          ok=.false.
        endif
        if(OK)then

C Display initial gain profiles and then sort and remove overlaps and
C ensure that periods start at zero and end with 24. Sorted files can
C be saved as version 3.0
C          call edisp(iuout,'Before sorting')
          call CASINF(ICOMP,iuout)
          if(ip3ver(ICOMP).eq.0)then
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru,IER)
          else
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru,IER)
          endif
          ip3ver(ICOMP)=2
          sorted=.true.
          modopr=.true.
        endif
      endif

C Initial menu entry setup.
      idtyp = 1
 5    MHEAD=5
      MCTL=8
      ILEN=NCAS(IDTYP)
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Plot casual gains.
      if (MMOD.eq.8) call PLTCASGN(ICOMP,idtyp)

C Set up top of menu.
 44   VERT(1) = '1 import from profiles database '
      VERT(3) = '  ----------------------------- '
      WRITE(VERT(4),'(3a,i2,a)') '3 loads >> ',calentag(idtyp),' (',
     &NCAS(IDTYP),')'
      IF(IELF(ICOMP).EQ.1) THEN
        VERT(2) = '2 electrical data >> included'
        VERT(5) = ' Start End  Type     Power  p.f.'
      ELSE
        VERT(2) = '2 electrical data >> not included'
        VERT(5) = ' Start End  Type  Sensib  Latent'
      ENDIF

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

C loop for each day type
      DO 10 L=1,NCAS(IDTYP)
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
        M=M+1
        CALL EMKEY(M-MHEAD,KEY,IER)
        if(ICGT(IDTYP,L).gt.0)then
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' W  ',
     &        PWR(IDTYP,L),PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' W   no power use'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &      lodlabel(icomp,ICGT(IDTYP,L)),' W  ',CMGS(IDTYP,L),
     &      CMGL(IDTYP,L)
          endif
  303     FORMAT(a,2i4,1x,2a,2f6.0)
  313     FORMAT(a,2i4,1x,2a)
        elseif(ICGT(IDTYP,L).eq.-1)then
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' m2p',PWR(IDTYP,L),
     &        PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' m2p no power use'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS(IDTYP,L),CMGL(IDTYP,L)
          endif
        else
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' Wm2',PWR(IDTYP,L),
     &        PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,ICGT(IDTYP,L)),' Wm2 no power use'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' Wm2',CMGS(IDTYP,L),CMGL(IDTYP,L)
          endif
        endif
        ENDIF
 10   continue

C Set up remainder of menu.
C Number of actual items displayed (update M for additional descriptive lines).
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ----------------------------- '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='@ edit type labels            '
      VERT(M+3)  ='+ add/delete/copy/import gains'
      VERT(M+4)  ='* scale existing gains        '
      VERT(M+5)  ='# check/remove overlaps       '
      VERT(M+6)  ='! list current information    '
      VERT(M+7)  ='? help                        '
      VERT(M+8)  ='- exit this menu              '

C Help text for this menu.
      H(1)='Casual gains are defined via one or more periods, the'
      H(2)='magnitude at a given time being the summation of the '
      H(3)='gains in all periods which include that time.  In '
      H(4)='this way complex patterns may be described. '
      H(5)=' '
      H(6)='You may use an `additive` or `sequential` approach or'
      H(7)='a combination as you choose.  The total gain is '
      H(8)='displayed in the graphic feedback window.'
      H(9)='  '
      H(10)='If electrical definitions are included the menu will'
      H(11)='show the power consumption of the load and the power'
      H(12)='factor. The graphs will continue to show the casual'
      H(13)='gains to the zone.'
      H(14)='  '
      h(15)='Import casul gains - you select a zone with casual'
      h(16)='gains patterns and they are copied into this zone,'
      h(17)='optionally with adjustments if the zone base areas'
      h(18)='differ.'

C Now display the menu.
      title = ' '
      if(mmod.eq.8)then
        write(title,'(a,a)') 'Casual gains in ',zname(ICOMP)
      else
        write(title,'(a,a)') 'Gains: ',zname(ICOMP)(1:lnzname(ICOMP))
      endif
      CALL EMENU(title,VERT,MVERT,IVERT)
      if (IVERT.EQ.1)THEN

C Import profile.
        IX=1
        CALL EPICKS(IX,KSTR,' ','Import a profile for which day type?',
     &  12,NBDAYTYPE,CALENTAG,
     &  'Import a profile for which day type?',IER,18)
        IW=KSTR(1)
        IN=NCAS(IW)
        CALL SCASL(IN,IW,IER,ICOMP)
        sorted=.false.
      elseif (IVERT.eq.2) then

C Include/ exclude electrical definitions.
        H(1)='If you choose to incorporate electrical data into '
        H(2)='the operations file you will be asked to specify ' 
        H(3)='additional data items for each casual gain. With '
        H(4)='the extra data items casual gains are now zone '
        H(5)='loads - zone loads contain both thermal and '
        H(6)='electrical information. Type definitions remain '
        H(7)='the same as for casual gains. '
        if(IELF(ICOMP).EQ.0.or.IELF(ICOMP).EQ.2) then
         call easkabc('Do you wish to include definition of electrical', 
     &   'data in the operations description?','Yes','No','Cancel',IW,7)
          if(IW.eq.1) then

C Set flag and default values.
            ielf(ICOMP)=1
            DO 609 IDTY=1,NBDAYTYPE
              do 34 i=1,NCAS(IDTY)
                PF(IDTY,I)=0.0
                IPF(IDTY,I)=0
                PWR(IDTY,I)=0.0
                BVOLT(IDTY,I)=0.0
                IPHAS(IDTY,I)=1
 34           continue
 609        CONTINUE
          elseif(IW.eq.2) then
            ielf(ICOMP)=2
          endif
        elseif(IELF(ICOMP).EQ.1) then
          call easkab(' Do you wish to remove definition of  ', 
     &    ' electrical data in operations file ?','Yes','No',IW,7)
          if(IW.eq.1) then
            ielf(ICOMP)=2
          endif
        endif
      elseif (IVERT.eq.4) then

C Increment the daytype focus.
        idtyp = idtyp + 1
        if(idtyp.gt.NBDAYTYPE) idtyp = 1
        goto 5
      ELSEIF(IVERT.EQ.MVERT)THEN
        return
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('gains menu',18,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List current.
        call CASINF(ICOMP,IUOUT)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Check & remove overlaps in periods in the current day type. As the
C number of periods might have changed, rebuild the menus.
        ILLEN(IDTYP)=NCAS(IDTYP)
        call edisp(iuout,'Before sorting')
        call CASINF(ICOMP,iuout)
        call PROCESSOLDCAS(ICOMP,0,iuout,IER)
        if(ier.ne.0)then
          call edisp(iuout,'Problem while sorting gains.')
        endif
        sorted=.true.
        modopr=.true.

C If different period, update the menu length.
        if(ILLEN(IDTYP).ne.NCAS(IDTYP))
     &  call edisp(iuout,'Periods have changed.')
        ILEN=NCAS(IDTYP)
        goto 44
      ELSEIF(IVERT.EQ.(MVERT-4))THEN
        h(1)='You can apply a scaling factor to all entries of a '
        h(2)='particular casual gain type (useful for parametric '
        h(3)='studies on profiles which have been imported).'
        H(4)='If you are not scaling gains choose all gains and then'
        H(5)='choose a factor of 1.0'
        CALL EASKATOG('Apply scaling factor to which casual gain type?',
     &     ' ',lodlabel(icomp,1),lodlabel(icomp,2),lodlabel(icomp,3),
     &     'All gain types','Cancel',' ',' ',ICGTS,5)
        if (ICGTS.ne.5) then
          if (ICGTS.ne.4) then
            write (outs,'(3a)') 'Scale ',lodlabel(icomp,ICGTS),
     &                          ' gain for day type: '
          else
            write (outs,'(a)') 'Scale all gains for day type: '
          endif
          call EASKABC('Import profile for one or all day types',
     &          ' ','One day type','All day types','Cancel',IOPT,1)
          IF(IOPT.EQ.1)THEN
            IW=0
            IX=1
            CALL EPICKS(IX,KSTR,' ',
     &      'Import a profile for which day type?',12,NBDAYTYPE,
     &      CALENTAG,'Import a profile for which day type?',IER,1)
            IW=KSTR(1)
            write (outs,'(2a)') outs(1:lnblnk(outs)),CALENTAG(IW)
          ELSEIF(IOPT.EQ.2)THEN
            IW=0
            write (outs,'(2a)') outs(1:lnblnk(outs)),'ALL'
          ELSE
            GOTO 5
          ENDIF
          CALL EASKR(VAL,outs,'(typically between 0.1 and 5.0)',
     &    0.1,'W',5.0,'W',1.0,'scaling factor',IER,3)
          DO 760 IDTY=1,NBDAYTYPE
            do 33 i=1,NCAS(IDTY)
              IF(IDTY.EQ.IW.OR.IW.EQ.0)THEN
                if(IABS(ICGT(IDTY,I)).eq.ICGTS.or.ICGTS.eq.4)then
                  CMGS(IDTY,I)=CMGS(IDTY,I)*VAL
                  CMGL(IDTY,I)=CMGL(IDTY,I)*VAL
                endif
              ENDIF
 33         continue
 760      CONTINUE
        ELSE
          GOTO 5
        endif
        goto 5
      elseif (IVERT.eq.(MVERT-5)) then

C Add delete copy period or import full set of casual gains.
        H(1) ='The options: '
        H(2) =' add - adds a period to one or all day types'
        H(3) =' '
        H(4) =' delete - deletes one or more periods (within a list'
        H(5) ='   of all periods in all days).'
        H(6) =' '
        H(7) =' copy - copies periods from one day type to another  '
        H(8) ='   Currently it adds the copied periods to the '
        H(9) ='   destination in addition to existing periods already'
        H(10)='   present in the destination day type '
        H(11)=' '
        H(12)=' import - imports casual gain patterns for all gains'
        H(13)='   and day types from another zone (overwrites).'
        H(14)=' '
        H(15)=' import pattern - imports casual gain patterns for all'
        H(16)='   gains and day types from file outwith the model '
        H(17)='   (overwrites)'
        H(18)=' '
        H(19)='When importing you can specify whether to:'
        H(20)=' a) preserve the absolute W being imported e.g. 123W'
        H(21)='    in the source zone is also 123W in destination'
        H(22)=' b) preserves W/m^2 implied by the source zone and'
        H(23)='    destination zone base areas e.g. 123W in source'
        H(24)='    zone of 2.3m^2 becomes 267W in a 5.0m^2 base area'
        H(25)='    zone.'
        CALL EASKATOG('Casual gain period options:','(see help)',
     &    'add period','delete period','copy period',
     &    'import from other zone','import pattern ','cancel',' ',IW,25)
        if (IW.EQ.1) then 

C Add new casual gain period.
          CALL EASKABCD('New casual gain of which type?',' ',
     &        lodlabel(icomp,1),lodlabel(icomp,2),lodlabel(icomp,3),
     &        'Cancel',ICGTS,3)
          if (ICGTS.ne.4) then
            IQ=0
            if(.NOT.browse)call checkcascount(icomp)
            call EASKABC('Add period for one or all day types',
     &          ' ','One day type','All day types','Cancel',IOPT,1)
            IF(IOPT.EQ.1)THEN
              write (outs,'(3a)') 'New ',lodlabel(icomp,ICGTS),
     &                          ' gain for day type: '
              IQ=0
              IX=1
              CALL EPICKS(IX,KSTR,' ',outs,12,NBDAYTYPE,
     &        CALENTAG,outs,IER,1)
              IQ=KSTR(1)
            ELSEIF(IOPT.EQ.2)THEN
              IQ=0
            ELSE
              GOTO 5
            ENDIF
            IF(IQ.EQ.0)THEN
              ITRK=0
              DO 619 IDU=1,NBDAYTYPE
                IF(NCAS(IDU).EQ.MC)THEN
                  WRITE(OUTS,'(2A)')
     &            'Sorry, too many periods for day type ',CALENTAG(IDT)
                  CALL USRMSG(' ',OUTS,'W')
                  ITRK=1
                ENDIF
 619          CONTINUE
              IF(ITRK.EQ.0)THEN
                IDT=-1
                IPER=0
                silent=.false.
                izs=0
                izf=24
                call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
              ENDIF
            ELSE
              IDT=IQ
              IF(NCAS(IDT).EQ.MC)THEN
                WRITE(OUTS,'(2A)')
     &          'Sorry, too many periods for day type ',CALENTAG(IDT)
                CALL USRMSG(' ',OUTS,'W')
              ELSE
                IPER=0
                if(loadcount(icomp,ICGTS,IDT).eq.0)then

C If there are no loads of this type make a default one.
                  silent=.true.
                  izs=0
                  izf=24
                  call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                endif
                silent=.false.
                izs=8
                izf=18
                call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
              ENDIF
            ENDIF
          ELSE
            GOTO 5
          endif
          sorted=.false.
          modopr=.true.
          goto 5
        elseif (IW.EQ.2) then 
          call CDCASGN(ICOMP,1)
          sorted=.false.
          modopr=.true.
          goto 5
        elseif (IW.EQ.3) then
          call CDCASGN(ICOMP,2)
          sorted=.false.
          modopr=.true.
          goto 5
        elseif (IW.EQ.4.or.IW.EQ.5) then

C Import casual gains from another zone. Ask which zone, then
C confirm whether absolute W values are to be scaled.
          if(IW.eq.4)then
 39         isource=0
            izdef=0
            call askzone(isource,izdef,'Casual gains from which zone?',
     &        'Source Zone','-','Casual import requires a zone.',
     &      ier)
            if (IER.ne.0.or.isource.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current zone (',
     &          zname(ICOMP)(1:lnzname(ICOMP)),').'
              call USRMSG('The selected zone should be different from',
     &          outs,'W')
              goto 39
            endif
            sfile=' '
          elseif(IW.EQ.5) then

C File browsing from pattern folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','pat','opr',sfile,snpfile,nfile,
     &        iier)
            if(nfile.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','pat','opr',sfile,snpfile,nfile,
     &          iier)
              if(snpfile(1:2).ne.'  ')then

C Debug.
C            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
C            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))

                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again...',' ','W')
                goto 5
              endif

C Display a bit of information about this browsed file.
              IUO=IFIL+1
              call FINDFIL(sfile,XST)
              if(XST)then
                call ZSCHEDHINT(IUO,sfile,IER)
              else
                call USRMSG('The selected file could not be found.',
     &            'Please try again.','W')
                goto 5
              endif
            else

C Nothing selected treat this as a return state.
              call usrmsg('No files available, try again...',' ','W')
              goto 5
            endif
          endif

C User should only get to this point if there is a source file to use.
          CALL EASKABC('Casual import:',' ','preserve absolute W',
     &       'convert abs W for base area','continue',ivntv,21)
          if(ivntv.eq.1) iwatt='W-W    '
          if(ivntv.eq.2) iwatt='W-m2-W '
          if(ivntv.eq.3) goto 5
          idest=icomp
          act='allcas'
          itrc=1
          call IMPORTZCASG(isource,idest,act,iwatt,sfile,itrc,'-',ier)

C Remind users to check their data.
      h(1)='An automated process has been used to manage the imported'
      h(2)='schedules. It usually does it as intended and, as with all'
      h(3)='automated processes it is recommended that you confirm '
      h(4)='that the resulting file is correct. '
      h(5)=' '
      h(6)='One approach is to generate a new QA report and check this'
      h(7)='against the previous model and then run a short assessment'
      h(8)='to confirm that the predictions are as expected. '
          CALL PHELPD('operation import confirm',8,'-',0,0,IER)
        elseif(iw.eq.-3)then
          goto 44  ! re-display the menu
        endif
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C Provide a bit of editing space (8 char) for the  char string.
        dok=.false.
        h(1)='Casual gains have initial type labels which may not'
        h(2)='be appropriate for your project. You can adapt the '
        h(3)='labels if it clarifies your model. '
        call askok(' ','Change type labels?',OK,dok,3)
        if (OK) then 
          h(1)='Select a label which describes the gain/load type'
          h(2)='i.e. equip - equipment, light - lighting etc..   '
          DO 567 ITYPE=1,3
            write(outs,'(a,i2)') 'Label for type ',ITYPE
            write(TYPSTR,'(2a)') lodlabel(icomp,ITYPE),'  '
            call easks(TYPSTR,' ',outs,8,
     &                 lodlabel(icomp,ITYPE),' Type label',IER,2)
            write(lodlabel(icomp,ITYPE),'(a)') TYPSTR(1:6)
 567      CONTINUE
          goto 5
        endif
      ELSEIF(IVERT.EQ.(MVERT-7))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          ILEN=NCAS(IDTYP)
          goto 44
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)
        IDT=IDTYP
        IPER=I
        ICGTX=ICGT(IDTYP,IPER)
        silent=.false.
        izs=0
        izf=0
        call EDCASGN(ICOMP,ICGTX,IDT,IPER,silent,izs,izf)
        sorted=.false.
        modopr=.true.
      endif

C Redisplay menu.
      IVERT=-4
      GOTO 5
      END

C ********************* SCASL *********************
C SCASL: Import a profile from a profiles database to build casual
C gain patterns.
      SUBROUTINE  SCASL(IN,IDAY,IER,ICOMP)

C << introduce concept of profiles with multiple day types as well
C << as multiple casual gain types

C << also consider that periods can be defined as reals for later
C << use in the zone operation file timings

#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "profile.h"

      common/pophelp/h(60)

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

C Profile database commons via profile.h.

      DIMENSION  DESC(MGIT),IPRVAL(MGIT)
      CHARACTER*40 DESC
      CHARACTER H*72,hold*40,lodlabel*6
      character*8 ll1,ll2,ll3
      character lltmp*144
      character fs*1  ! file separator
      logical OK,DOK

      integer IPPFT,IPPHAST  ! for local editing.
      logical unixok  ! to check for database path file separators

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Labels for loads.
      write(ll1,'(1x,a6,1x)') lodlabel(icomp,1)
      write(ll2,'(1x,a6,1x)') lodlabel(icomp,2)
      write(ll3,'(1x,a6,1x)') lodlabel(icomp,3)

      IER=0
      ICGTX=1
      PS=0.
      PL=0.
      RAD=0.5
      CON=0.5
      IPPFT=0
      IPPF=0

C Scan the event profiles database. If an error state is
C returned then it might be an ASCII file, try and scan that.
      lndbp=lnblnk(standarddbpath)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif
      call TRIPROBIN(IPRODB,lltmp,IER)
      if(ier.ne.0)then
        CALL ERPFREE(IPRODB,ISTAT)
        CALL USRMSG(' opened profiles db...',' ','-')
        ier=0   ! reset prior to call to triprocom
        call TRIPROCOM(IPRODB,lltmp,IER)
        if(ier.ne.0)then
          call usrmsg('Error opening the db. Please use one',
     &                'of the other options.','W')
          return
        else
          CALL ERPFREE(IPRODB,ISTAT)
          CALL USRMSG(' opened profiles db...ok',' ','-')
        endif
      else
        CALL USRMSG(' opened profiles db...ok',' ','-')
      endif
      
C Copy the string list of event profiles to local string variable.
      DO 50 I = 1,NPDBITEMS
        DESC(I)=PDBDESC(I)
   50 CONTINUE
      
C Present a list of profiles to select from.
      H(1)='Any one of the profiles may be selected.'
      IX=1
      NITEMS=NPDBITEMS
      CALL EPICKS(IX,IPRVAL,' ','Profiles in events db: ',
     &         40,NITEMS,DESC,'db profiles',IER,1)
      IF(IX.EQ.0)RETURN

      INDEX=IPRVAL(1)

C Identify nature of casual gain
      H(1)='Casual gains within a zone can be be designated to be '
      H(2)='of one of three user-defined types so that for example '
      H(3)='occupant gains can be separately tracked from that of '
      H(4)='lighting or equipment.  The types listed can have any'
      H(5)='meaning that the user chooses (type labels can be edited'
      h(6)='in the main operations menu.'
      CALL EASKABC(' ','Type of casual gain: ',ll1,ll2,ll3,ICGTX,6)

 100  WRITE(HOLD,'(2f9.0,2f8.2,a)')PS,PL,RAD,CON,'  '

C << this would be a good place to expand the logic so that lighting
C << and small power can be entered in terms of W/m2.
      H(1) ='In order to expand the profile into energy values the'
      H(2) ='sensible and latent load in Watts equivalent to 100% is'
      h(3) =' required. '
      h(4) =' '
      h(5) ='Also supply the radiant and convective fraction for the'
      h(6) ='sensible load (these normally should sum to 1.00) '
      h(7) =' '
      h(8) ='The radiant and convective split of lighting fixtures is'
      h(9) ='listed below(CIBSE Consice Handbook Table A6.5) : '
      h(10)='  fluorescent radiant = 30  convective = 70'
      h(11)='  tungsten    radiant = 85  convective = 15'
      h(12)='  HP mercury sodium radiant = 50 convective = 50'
      h(13)='  low pressure sodium radiant = 43 convective = 57'
      h(14)=' '
      h(15)='The position of lamps determines the distribution of '
      h(16)='heat into room or ceiling void (CIBSE Table A6.4) '
      h(17)='  recessed open fixture  upwards = 38 downwards = 62'
      h(18)='  recessed with louvre   upwards = 45 downwards = 55'
      h(19)='  recessed with diffuser upwards = 53 downwards = 47'
      h(20)='  surface mount open fixt upwards = 12 downwards = 88'
      h(21)='  surface mount enclosed prismatic up= 22 down = 78'
      h(22)='  suspended near ceiling enclosed prismatic upwards = 6'
      h(23)='     downwards = 94'
      h(24)='  suspended or wall mounted or desk lamp upwards = 0'
      h(25)=' '
      h(26)='The radiant and convective split for office equipement'
      h(27)='is typically: (CIBSE Table A6.9) '
      h(28)=' desktop computer radiant = 14 convective = 86'
      h(29)=' monitor (type not defined) radiant = 35 conv = 65'
      h(30)=' laser printer  radiant 33 convective 67'
      CALL EASKS(HOLD,
     &  ' 100% sensible & latent loads, radiant & convective fraction:',
     &        ' ',40,' 1000   0  0.5  0.5  ','profile info',IER,30)
      K=0
      CALL EGETWR(HOLD,K,PS,0.0,999999.,'F','sensible 100%',IER)
      CALL EGETWR(HOLD,K,PL,0.0,999999.,'F','latent 100%',IER)
      CALL EGETWR(HOLD,K,RAD,0.0,1.0,'F','rad frac',IER)
      CALL EGETWR(HOLD,K,CON,0.0,1.0,'F','conv frac',IER)

      SUM = RAD + CON
      IF (SUM .LT. 0.98 .OR. SUM .GT. 1.02)then
        dok=.false.
        h(1)='If the sum is less than 0.98 or greater than 1.00 then'
        h(2)='the sensible load may not be well represented in your '
        h(3)='model. Tweaking this is for experts only! '
        call askok(' The Sum of radiant and convective fractions is',
     &    ' not close to 1.0! Are you sure about this? ',OK,dok,2)
       if(.NOT.ok)goto 100
      ENDIF
      IF(IELF(ICOMP).GT.0) THEN
        dok=.false.
        h(1)='Electrical data (e.g. power factor, leading/lagging'
        h(2)='loads, real power, phase) might be used by electrical'
        h(3)='engineers and in models where other power components'
        h(4)='are being defined. If not just say no. '
        CALL ASKOK('Include electrical data with this profile?',
     &    '(see help)',OK,dok,4)
        IF(.NOT.OK) THEN
          PPF=0.0
          IPPF=0
          PBVOLT=0.0
          IPPHAS=1
          PE=0.0
        ELSE
          H(1)='Enter a power factor between 0.-1.'
          H(2)='Power factor is the ratio of real to apparent'
          H(3)='power drawn by the load.'
          CALL EASKR(PPF,' ','Power factor? [0-1] ? ',
     &      0.,'F',1.,'F',0.8,'Power factor ',IER,3)

          H(1)='A lagging load is one with an inductive'
          H(2)='component i.e. flourescent lighting, a '
          H(3)='leading load is one with a capacative  '
          H(4)='element. Unity loads are purely resistive'
          H(5)='i.e. incandescent lighting.'
          H(6)=' '
          H(7)='A cancel leaves the model without electrical data'
          H(8)='for this item. '
          ippft=ippf
          CALL EASKI(ippft,'Lagging -1, Leading 1 or Unity 0?',
     &      '(see help) ',-1,'F',1,'F',0,'casual gain type',IERI,8) 
          if(ieri.eq.-3)then
            PPF=0.0
            IPPF=0
            PBVOLT=0.0
            IPPHAS=1
            PE=0.0
            goto 42
          else
            ippf=ippft
          endif

          H(1)='To expand the profile into energy values it'
          H(2)='is necessary to supply the power consumption'
          H(3)='equivalent to 100%.'
          PE=PS
          CALL EASKR(PE,' ','Power consumption? (W) ? ',
     &      0.,'F',99.,'-',PS,'Power consumption ',IER,3)

          PBVOLT=240.
          H(1)='At what voltage does the load operate ?'
          CALL EASKR(PBVOLT,' ','Operational voltage ? (W) ? ',
     &      0.,'F',99.,'-',240.,'Operational volatage ',IER,1)

          IPPHAST=1
          H(1)='Which phse of the three phse supply is the load'
          H(2)='connected to ? (1-3) or 4 - all three i.e. for'
          H(3)='a large induction motor.'
          H(4)=' '
          H(5)='A cancel leaves the model without electrical data'
          H(6)='for this item. '
          ipphast=ipphas
          CALL EASKI(IPPHAST,'Supply phase 1-3 or 4 ?','(see help)',
     &       1,'F',4,'F',1,'casual gain type',IERI,6) 
          if(ieri.eq.-3)then
            PPF=0.0
            IPPF=0
            PBVOLT=0.0
            IPPHAS=1
            PE=0.0
          else
            ipphas=ipphast
          endif            
        ENDIF
      ENDIF
 42   call usrmsg(' ',' ','-')

C Instanciate the casual gain commons with data from the profile.
      NCAS(IDAY) = NCAS(IDAY) + NCG(INDEX)
      DO 140 I = 1,NCG(INDEX)
	ICGS(IDAY,I+IN) = ICGS1(INDEX,I)
	ICGF(IDAY,I+IN) = ICGF1(INDEX,I)
	CMGS(IDAY,I+IN) = CGS1(INDEX,I)*PS/100.0
	CMGL(IDAY,I+IN) = CGL1(INDEX,I)*PL/100.0
	RADC(IDAY,+IN) = RAD
	CONC(IDAY,I+IN) = CON
	ICGT(IDAY,I+IN) = ICGTX
	IF(IELF(ICOMP).GT.0) THEN
	  PF(IDAY,I+IN)=PPF
	  IPF(IDAY,I+IN)=IPPF
	  PWR(IDAY,I+IN)=CGS1(INDEX,I)*PE/100.
	  BVOLT(IDAY,I+IN)=PBVOLT
	  IPHAS(IDAY,I+IN)=IPPHAS
	ENDIF
140   CONTINUE

      CALL ERPFREE(IPRODB,ISTAT)
      RETURN

      END


C ********************* EDCASGN *********************
C EDCASGN - edit casual gain data.
C  ICOMP zone number
C  ICGTX casual gain type index
C  IDT day type (=-1 then all day types)
C  IPER period (=0 then new)
C  silent true if no user interaction requested.
C If silent is true then use icgs & icgf parameters.

      subroutine EDCASGN(ICOMP,ICGTX,IDT,IPER,silent,icgsX,icgfX)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)

      logical OK,DOK,CLOSE,silent

      character OUTS*124,hold*24,h*72

      integer IV,ipft,IPHAST  ! for local editing.
      real pft,pwrt,bvoltt,xfla,zb   ! for local editing.
      integer IALL

C If new period then set defaults otherwise get data from common.
      if (IPER.eq.0) then
        CMGSX=0.
        CMGLX=0.
        RADCX=0.5
        CONCX=0.5
        IPFX=0
        IPHASX=1
        PFX=0.0
        PWRX=0.0
        BVOLTX=0.0
      ELSEIF(IDT.GT.0)THEN
        call CPYCASIT(icomp,idt,iper,icgtX,icgsX,icgfX,cmgsX,cmglX,
     &    radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX)
      endif

C If silent jump to insertion logic.
      if(silent) goto 112

C Ask for start and finish times.
 10   write (OUTS,'(A)') 'Start and finish hours for period?'
      write (hold,'(2i5,a)') ICGSX,ICGFX,'  '
      h(1)='Period during which casual gains applies.'
      CALL EASKS(hold,outs,' ',24,' 0 24 ','cas st-fn hr',IER,1)
      if(ier.eq.-3) return  ! user canceled
      K=0
      CALL EGETWI(hold,K,IVS,0,24,'F','period start',IER)
      CALL EGETWI(hold,K,IVF,IVS+1,24,'F','period end',IER)
      if (IER.ne.0) goto 10
      ICGSX=IVS
      ICGFX=IVF

C Warn user if the zone base area is still zero.
      call eclose(ZBASEA(ICOMP),0.0,0.01,close)
      if(close)then
        call edisp(iuout,
     &    'Zone base area is ~zero so 1m^2 assumed.')
        zb=1.0
      else
        zb=ZBASEA(ICOMP)
        write(outs,'(a,f7.3)') 'The current zone base area is ',zb
        call edisp(iuout,outs)
      endif

C Ask for sensible and latent gain.
C Can be specified as: W, W/obj, W/m^2, m^2/obj
      h(1)='A gain can be specified as:'
      h(2)=' Watts - explicitly set the gain.'
      h(3)=' Watts per square meter - will use the floor area of the '
      h(4)='  zone to calculate the total gain.'
      h(5)=' Watts per object - gain/object and number of objects are'
      h(6)='  required (an object could be a person or computer).'
      h(7)=' Square meters per object - will use the floor area of the'
      h(8)='  zone to calculate the total gain. For the particular'
      h(9)='  case of an occupant there is an assumption that each'
      h(10)=' person is 95W sensible and 45W latent.'
      h(11)=' '
      h(12)='ESP-r asks for fixed sensible and latent values. The'
      h(13)='values for occupants are really a function of the current'
      h(14)='zone temperature (CIBSE Concise Handbook Table 6.1)'
      h(15)='If possible pick a value for the likely temperature rnage.'
      h(16)='For an adult and average mix '
      h(17)='                   Tot 15C sen lat 20C sen lat 24C sen lat'
      h(18)='Seated inactive    115     100  15     90  25      75  40'
      h(19)='       avg mix     100      87  13     78  22      65  35'
      h(20)='Seated light work  140     110  30    100  40      80  60'
      h(21)='       avg mix     126      99  27     90  36      72  54'
      h(22)='Seated office work 140     110  30    100  40      80  60'
      h(23)='       avg mix     130     102  28     93  37      74  56'
      h(24)='Standing light work 160    120  40    110  50      85  75'
      h(25)='       avg mix      141    106  35     97  44      75  66'
      h(26)='Light factory work 235     150  85    130 105     100 135'
      h(27)='       avg mix     209     133  76    116  93      89 121'
      h(28)='Medium factory work 265    160 105    140 125     105 160'
      h(29)='       avg mix      249    150  99    132 117      99 150'
      h(30)='Heavy factory work 440     220 220    190 250     135 305'
      h(31)='       avg mix     440     220 220    190 250     135 305'
      h(32)=' '
      IGU=1
      CALL EASKABCD('Gain to be specified as: (see help)',' ','Watts',
     &     'Watts per square meter','Watts per object',
     &     'Square meters per object',IGU,32)

C Common portion of help message for detailed dialogs.
      h(4)=' '
      h(5)='ESP-r asks for fixed sensible and latent values. The'
      h(6)='values for occupants are really a function of the current'
      h(7)='zone temperature (CIBSE Concise Handbook Table 6.1)'
      h(8)='If possible pick a value for the likely temperature rnage.'
      h(9)='For an adult and average mix '
      h(10)='              Total W 15C sen lat 20C sen lat 24C sen lat'
      h(11)='Seated inactive   115     100  15     90  25      75  40'
      h(12)='       avg mix    100      87  13     78  22      65  35'
      h(13)='Seated light work 140     110  30    100  40      80  60'
      h(14)='       avg mix    126      99  27     90  36      72  54'
      h(15)='Seated office work 140    110  30    100  40      80  60'
      h(16)='       avg mix     130    102  28     93  37      74  56'
      h(17)='Standing light work 160   120  40    110  50      85  75'
      h(18)='       avg mix      141   106  35     97  44      75  66'
      h(19)='Light factory work 235    150  85    130 105     100 135'
      h(20)='       avg mix     209    133  76    116  93      89 121'
      h(21)='Medium factory work 265   160 105    140 125     105 160'
      h(22)='       avg mix      249   150  99    132 117      99 150'
      h(23)='Heavy factory work 440    220 220    190 250     135 305'
      h(24)='       avg mix     440    220 220    190 250     135 305'
      if (IGU.eq.1) then

C Gain as W - require gain only.
 20     if (ICGTX.lt.-1) then
          write (HOLD,'(2f8.1,a)') CMGSX*zb,CMGLX*zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGSX,CMGLX,'  '
        endif
        h(1)='Supply the sensible and latent gain in Watts.'
        h(2)=' '
        h(3)=' '
        call EASKS(HOLD,'Sensible and latent gain (W):',
     &          ' ',24,'0.   0. ','casual gn info',IER,24)
        if(ier.eq.-3) goto 10  ! user canceled
        K=0
        call EGETWR(HOLD,K,XSEN,0.,100000.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLATGN,0.,100000.0,'W','latent gain',IER)
        if (IER.ne.0) goto 20
        if (ICGTX.lt.0) ICGTX=ICGTX*(-1)
        CMGSX=XSEN
        CMGLX=XLATGN
      elseif (IGU.eq.2) then

C Gain as W/m^2 - require gain per square meter.
        if (ICGTX.gt.0) then
          write (HOLD,'(2f8.1,a)') CMGSX/zb,CMGLX/zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGSX,CMGLX,'  '
        endif
 30     h(1)='Supply the sensible and latent gain in Watts/m^2.'
        h(2)=' '
        h(3)=' '
        call EASKS(HOLD,'Sensible and latent gain (W/m^2):',
     &          ' ',24,'0.   0. ','casual gn info',IER,24)
        if(ier.eq.-3) goto 10  ! user canceled
        K=0
        call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLATGN,0.,50.0,'W','latent gain',IER)
        if (IER.ne.0) goto 30
        if (ICGTX.eq.1) then
          CMGSX=XSEN*zb
          CMGLX=XLATGN*zb
        else
          if (ICGTX.gt.1) ICGTX=ICGTX*(-1)
          CMGSX=XSEN
          CMGLX=XLATGN
        endif
        write(outs,'(a,f9.1,a,f9.1)') 'Sensible Watts are now ',CMGSX,
     &    ' & latent Watts ',CMGLX
        call edisp(iuout,outs)
      elseif (IGU.eq.3) then

C Gain as W/object - require no of objects and gain per object. If
C user cancels go back to initial question.
C << an actual cancel option not yet implemented >>
        NOBJ=1
 40     h(1)='The number of objects for the current gain type is '
        h(2)='required. For example the number of computers.'
        h(3)=' '
        CALL EASKI(NOBJ,' ','Number of objects (e.g. people)?',
     &    0,'W',100,'W',1,'no. objects',IERI,24)
        if(ieri.eq.0)then
          continue
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 40
        endif

        write (HOLD,'(2f8.1,a)') CMGSX/real(NOBJ),CMGLX/real(NOBJ),'  '
        h(1)='Supply the sensible and latent gain in Watts.'
        h(2)=' '
        h(3)=' '
        call EASKS(HOLD,'Object sensible and latent gain (W):',
     &          ' ',24,'  0.   0. ','casual gn info',IER,24)
        K=0
        if(ier.eq.-3) goto 10  ! user canceled
        call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLATGN,0.,50.0,'W','latent gain',IER)
        if (IER.ne.0) goto 40
        if (ICGTX.lt.0) ICGTX=ICGTX*(-1)
        CMGSX=XSEN*real(NOBJ)
        CMGLX=XLATGN*real(NOBJ)
        write(outs,'(a,f9.1,a,f9.1)') 'Sensible Watts are now ',CMGSX,
     &    ' & latent Watts ',CMGLX
        call edisp(iuout,outs)
      elseif (IGU.eq.4) then

C Gain as m^2/object - floor area of an object and gain per object.
C If user cancels go back to initial question.
        NOBJ=1
 50     if (ICGTX.eq.-1) then
          FLA=CMGSX
        else
          FLA=10.
        endif
        XFLA=FLA
        h(1)='The floor area per object for the current gain type is '
        h(2)='required. For example the area per person assuming that'
        h(3)='for people the sensible is 95W and latent is 45W. '
        call EASKR(XFLA,'Floor area per object (e.g. people)?','  ',
     &    0.1,'F',100.0,'W',1.0,'area per object',IERI,24)
        if(IERI.eq.0)then
          FLA=XFLA
          if (iabs(ICGTX).eq.1) then

C If dealing with occupants treat this as a special case which assumes
C that each person is 95W sensible and 45W latent.
            ICGTX=-1
            CMGSX=FLA  ! Set sensible to the m^2 per person entered
            CMGLX=0.   ! Why is CMGLX set to zero?
            VS=(ZBASEA(ICOMP)/CMGSX)*95.  ! find sensible watts
            VL=(ZBASEA(ICOMP)/CMGSX)*45.  ! find latent watts
            write(outs,'(a,f9.1,a,f9.1)') 'Sensible Watts now ',VS,
     &        ' & latent Watts ',VL
            call edisp(iuout,outs)
          else
            write (HOLD,'(2f8.1,a)') CMGSX/(zb/FLA),
     &                               CMGLX/(zb/FLA),'  '
            h(1)='Supply the sensible and latent gain in Watts/object.'
            h(2)=' '
            h(3)=' '
            call EASKS(HOLD,'Sensible and latent gain (W/object):',
     &          ' ',24,'0.   0. ','casual gn info',IER,24)
            K=0
            call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
            call EGETWR(HOLD,K,XLATGN,0.,50.0,'W','latent gain',IER)
            if (IER.ne.0) goto 50
            if (ICGTX.gt.1) ICGTX=ICGTX*(-1)
            CMGSX=XSEN*zb/FLA
            CMGLX=XLATGN*zb/FLA
            write(outs,'(a,f9.1,a,f9.1)') 'Sensible Watts now ',CMGSX,
     &        ' & latent Watts ',CMGLX
            call edisp(iuout,outs)
          endif
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 50
        endif
      elseif (IGU.eq.-3) then
        goto 10  ! user hit the cancel button
      endif

C Ask for rad/ conv split.
 70   write (HOLD,'(2f7.3,a)') RADCX,CONCX,'  '
      h(1)='Supply the radiant and convective fraction for'
      h(2)='the sensible load (should be <=1.00) '
      h(3) =' '
      h(4) ='The radiant and convective split of lighting fixtures is'
      h(5) ='listed below(CIBSE Consice Handbook Table A6.5) : '
      h(6) ='  fluorescent radiant = 30  convective = 70'
      h(7) ='  tungsten    radiant = 85  convective = 15'
      h(8) ='  HP mercury sodium radiant = 50 convective = 50'
      h(9) ='  low pressure sodium radiant = 43 convective = 57'
      h(10)=' '
      h(11)='The position of lamps determines the distribution of '
      h(12)='heat into room or ceiling void (CIBSE Table A6.4) '
      h(13)='  recessed open fixture  upwards = 38 downwards = 62'
      h(14)='  recessed with louvre   upwards = 45 downwards = 55'
      h(15)='  recessed with diffuser upwards = 53 downwards = 47'
      h(16)='  surface mount open fixt upwards = 12 downwards = 88'
      h(17)='  surface mount enclosed prismatic up= 22 down = 78'
      h(18)='  suspended near ceiling enclosed prismatic upwards = 6'
      h(19)='     downwards = 94'
      h(20)='  suspended or wall mounted or desk lamp upwards = 0'
      h(21)=' '
      h(22)='The radiant and convective split for office equipement'
      h(23)='is typically: (CIBSE Table A6.9) '
      h(24)=' desktop computer radiant = 14 convective = 86'
      h(25)=' monitor (type not defined) radiant = 35 conv = 65'
      h(26)=' laser printer  radiant 33 convective 67'
      call EASKS(HOLD,'Radiant & convective fraction: (see help)',
     &        ' ',24,' 0.5  0.5  ','casual gn info',IER,26)
      K=0
      call EGETWR(HOLD,K,RAD,0.,1.0,'F','rad frac',IER)
      CMAX=(1.00-RAD)+0.02  ! allow for small rounding errors
      call EGETWR(HOLD,K,CON,0.,CMAX,'F','conv frac',IER)
      if (IER.ne.0) goto 70
      if(rad+con.lt.0.9)then
        write(outs,'(a,f7.3)') 
     &    'The gains radiant & convective parts add up to ',rad+con
        DOK=.false.
        h(1)='Setting the total radiant and convective split of a'
        h(2)='casual gain to less than 0.9 can lead to confusion'
        h(3)='(600W with 0.4 rad and 0.4 conv is 480W total) and '
        h(4)='should only be done by experts.'
        CALL ASKOK(outs,
     &    ' This is less than 0.9. Are you sure?',OK,DOK,4)
        IF(.NOT.OK)GOTO 70
      endif
      RADCX=RAD
      CONCX=CON
      
C Electrical details.
      if (IELF(ICOMP).EQ.1) then
        DOK=.false.
        h(1)='Electrical data (e.g. power factor, leading/lagging'
        h(2)='loads, real power, phase) might be used by electrical'
        h(3)='engineers and in models where other power components'
        h(4)='are being defined. If not just say no. '
        call ASKOK(' ','Does this load need electrical data?',OK,DOK,4)
        if(OK) then

C Get the electrical details of the load from the user
          H(1)=' Power factor will vary between 0 - 1.0'
 80       pft=PFX
          call EASKR(pft,' ','Load power factor?',
     &           0.0,'F',1.0,'F',1.0,'power factor',IER,1)
          if (IER.ne.0) goto 80

          H(1)='The load will either be capacitive, leading current,'
          H(2)='or reactive with a lagging current, most loads take '
          H(3)='a lagging current.'
          H(4)=' '
          H(5)='A (*) flags an existing value. '

          call ECLOSE(pft,1.0,0.0001,CLOSE)
          if (CLOSE) then
            IPFT=0
          else
            if(ipfX.eq.-1)then
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading (*)','Lagging',IOPT,5)
            elseif(ipfX.eq.1)then
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading','Lagging (*)',IOPT,5)
            else
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading','Lagging',IOPT,5)
            endif
            if (IOPT.EQ.1) then
              IPFT=-1
            else
              IPFT=1
            endif
          endif

          H(1)='What is the real power consumption in watts?'
          H(2)='This might be the same as the sensible load for'
          H(3)='some devices (please check). '
 90       pwrt=PWRX
          call EASKR(pwrt,'Real power consumption ?','  ',
     &          0.0,'-',10000.0,'-',CMGSX,'power consumption',IER,3)
          if (IER.ne.0) goto 90

          H(1)=' What voltage does the load operate at i.e. 220V'
 100      bvoltt=BVOLTX
          call EASKR(bvoltt,' ','Operational voltage?',
     &          0.0,'-',10000.0,'-',220.0,'op voltage',IER,1)
          if (IER.ne.0) goto 100

          H(1)='To which phase of the 3-phase supply is the load '
          H(2)='connected, 1-3 or 4-all three.'
          H(3)=' '
          H(4)='A cancel leaves the electrical data unchanged.'
 110      IPHAST=IPHASX
          call EASKI(IPHAST,'Which phase is load connected to?',
     &      '(see help)',1,'F',4,'F',1,'phase ',IERI,4)
          if(ieri.eq.-3)then
            continue
          elseif (ieri.gt.0)then
            goto 110
          else

C No more cancels so instanciate the data for this period.
            BVOLTX=bvoltt
            PWRX=PWRT
            IPFX=IPFT
            PFX=pft
            IPHASX=IPHAST
          endif
        endif
      endif

C If idt=-1 then do for all day types      
  112 IALL=0
      IF(IDT.EQ.-1)THEN
        IALL=1
        IDT=1
      ENDIF

C Start insertion
 113  if (IDT.gt.0) then
        IF(IPER.EQ.0)THEN
          NCG=NCAS(IDT)
          IF(NCG.LE.0)NCG=1
          do 210 I=1,NCG
            if(iabs(ICGT(IDT,I)).le.iabs(ICGTX))then
              if(ICGS(IDT,I).lT.ICGSX)then
                ILPER=I+1
              elseif(ICGS(IDT,I).eq.ICGSX)then!.and.ICGF(IDT,I).gt.ICGFX)then
                ILPER=I
              endif
            endif
 210      continue
          NCAS(IDT)=NCAS(IDT)+1
          INC=NCAS(IDT)
        else
          ILPER=IPER
          INC=IPER
        endif

C Now update common blocks. Loop backwards from 
        do 300 IX=INC,ILPER,-1
          if (IX.eq.ILPER) then
            call CPYCASTI(icomp,idt,ix,icgtX,icgsX,icgfX,cmgsX,cmglX,
     &      radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX)
          else
            call CPYCASIJ(icomp,IDT,IX,IX-1)
          endif
 300    continue
      ENDIF

C Loop back if more than one day type requested
      IF(IALL.EQ.1)THEN
        IDT=IDT+1
        IGB=0
        IF(IDT.LE.NBDAYTYPE)GOTO 113
      ENDIF
      return
      end

C ********************* CDCASGN *********************
C CDCASGN - copy/ delete casual gain data.
C  ICOMP zone number
C  ICD - copy (=2) delete (=1) flag

      subroutine CDCASGN(ICOMP,ICD)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC
      INTEGER IX,IDT,IDTT

C Electrical data flag and elctrical data.
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)

      character lodlabel*6,h*72,VERT(3*MC)*35
      dimension IVERT(MC),ITEMP(3*MC),kstr(MDTY)
      integer ilistlen

C Choose day type to delete/copy period from
      IX=1
      IF(ICD.EQ.1)THEN
        call edisp(iuout,'Choose daytype to delete period from')
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to delete period from',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to delete period from',IER,1)
      ELSEIF(ICD.EQ.2)THEN
        call edisp(iuout,'Choose daytype to copy period from')
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to copy period from',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to copy period from',IER,1)
      ENDIF

      IDT=kstr(1)
      M=0
      ilistlen=NCAS(IDT)
      DO 10 L=1,NCAS(IDT)
        M=M+1
        if(ICGT(IDT,L).gt.0)then
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &        lodlabel(icomp,ICGT(IDT,L)),' W  ',CMGS(IDT,L),CMGL(IDT,L)
  303     FORMAT('Wkd ',2i4,1x,2a,2f6.0)
        elseif(ICGT(IDT,L).eq.-1)then
          jicgt=iabs(ICGT(IDT,L))
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS(IDT,L),CMGL(IDT,L)
        else
          jicgt=iabs(ICGT(IDT,L))
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &        lodlabel(icomp,jicgt),' Wm2',CMGS(IDT,L),CMGL(IDT,L)
        endif
 10   continue

      IF(ICD.EQ.1)THEN
        H(1)='One or more of the periods may be deleted (removed).'
        call EPICKS(ilistlen,IVERT,' ',
     &    'Casual gain period(s) to delete:',
     &    35,M,VERT,'Casual gain period to delete',IER,1)

C Delete items in IVERT array. Sort IVERT first.
        if (ilistlen.gt.0) then
          KFLAG=-1
          call SORTI(IVERT,ITEMP,MC,KFLAG)
          do 100 IJ=1,ilistlen
            ICGP=IVERT(IJ)
            call insdelcasper(icomp,IDT,'d',icgp,ier)
            M=M-1
 100      continue
        endif
      else

C Copy periods.
        h(1)='All the casual gains associated with a specified day type'
        h(2)='can be copied to another day type.'
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to copy period to',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to copy period to',IER,1)
        IDTT=kstr(1)
        M=NCAS(IDT)
        IXX=M
        call EPICKS(IXX,IVERT,' ','Casual gain period(s) to copy:',
     &         35,M,VERT,'Casual gain period to copy',IER,1)
        do 210 IX=1,NCAS(IDT)
         if (NCAS(IDTT).lt.MC) then
            ICOPY=0
            DO 220 IJ=1,IXX
              IF(IX.EQ.IVERT(IJ))ICOPY=1
 220        CONTINUE
            IF(ICOPY.EQ.1)THEN
              NCAS(IDTT)=NCAS(IDTT)+1
              ICGT(IDTT,(NCAS(IDTT)))=ICGT(IDT,IX)
              ICGS(IDTT,(NCAS(IDTT)))=ICGS(IDT,IX)
              ICGF(IDTT,(NCAS(IDTT)))=ICGF(IDT,IX)
              CMGS(IDTT,(NCAS(IDTT)))=CMGS(IDT,IX)
              CMGL(IDTT,(NCAS(IDTT)))=CMGL(IDT,IX)
              RADC(IDTT,(NCAS(IDTT)))=RADC(IDT,IX)
              CONC(IDTT,(NCAS(IDTT)))=CONC(IDT,IX)
              pf(IDTT,NCAS(IDTT))=PF(IDT,IX)
              ipf(IDTT,NCAS(IDTT))=IPF(IDT,IX)
              pwr(IDTT,NCAS(IDTT))=PWR(IDT,IX)
              bvolt(IDTT,NCAS(IDTT))=BVOLT(IDT,IX)
              iphas(IDTT,NCAS(IDTT))=IPHAS(IDT,IX)
            ENDIF
          endif
 210    continue
      ENDIF
      return
      end

C ********************* IMPORTZCASG *********************
C IMPORTZCASG - import a casual gain regime from zone isource to
C zone idest based on act and iwatt
C actcas (8 char) = 'allcas' copies all casual gain types
C       descriptions (this will need a QA by the user in case of conflicts).

C iwatt (8 char) = 'W-W' for absolute W values preserves value
C   e.g. 123W in source zone is also 123W in destination zone.
C iwatt= 'W-m2-W' preserves implied W/m^2 base area implied by
C   absolute W in source zone in destination zone which has a
C   different base area. e.g. 123W in source zone of 2.3m^2 becomes
C   267W in a 5.0m^2 base zone.
C sfile (char*(*)) is the name of operations file outwith the model
C   which is used is isource is zero.
C itrc - if zero then work silently.
C act = '-' normal actions, 's' silent actions.

C This subroutine does not alter the air flow schedules in the
C destination zone, only the casual gains.

C << issue to be resolved later...if the user copies a pattern and
C << later on revises the zone geometry this might imply that a
C << different flow rate is required. Do we need to remember the
C << initial intent to guide later revisions?

      subroutine IMPORTZCASG(isource,idest,actcas,iwatt,sfile,itrc,
     &  act,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)
      CHARACTER oprdesc*248,ventdesc*248,ctlstr*24
      COMMON/P1/oprdesc(MCOM),ventdesc(MCOM),ctlstr(MCOM,MDTY)
      COMMON/P1C/oprdescc,ctlstrc
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT
      REAL CMGS,CMGL,RADC,CONC
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)
      common/loadlabelc/lodlabelc(7)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

      COMMON/P3NC/NCASC(MDTY),ICGSC(MDTY,MC),ICGFC(MDTY,MC),
     &CMGSC(MDTY,MC),CMGLC(MDTY,MC),RADCC(MDTY,MC),CONCC(MDTY,MC)
      COMMON/P3TYPENC/ICGTC(MDTY,MC)
      INTEGER NCASC,ICGSC,ICGFC,ICGTC
      REAL CMGSC,CMGLC,RADCC,CONCC

      common/elecflgc/ielfc
      COMMON/ELP3NC/NELC(MDTY),PFC(MDTY,MC),IPFC(MDTY,MC),PWRC(MDTY,MC),
     &BVOLTC(MDTY,MC),IPHASC(MDTY,MC)
      INTEGER NELC,IPFC,IPHASC
      REAL PFC,PWRC,BVOLTC

      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

      character*(*) sfile
      CHARACTER oprdescc*248,ctlstrc*24
      character h*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,ltmp*72
      character lodlabel*6,lodlabelc*6
      character fs*1,GFILE*72,iwatt*8
      character actcas*8
      character act*1   ! '-' normal, 's' silent
      LOGICAL XST,unixok,dok,ok
      character actflatfile*1
      integer idty

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C It is assumed that a prior call to COPYAIRFL has been made
C so there is no need to clear nac
      IER=0
      DO 600 IDTY=1,NBDAYTYPE
        NCAS(IDTY)=0
 600  CONTINUE
      lodlabel(idest,1)='Occupt'
      lodlabel(idest,2)='Lights'
      lodlabel(idest,3)='Equipt'  
      lodlabel(idest,4)='NA    '  
      lodlabel(idest,5)='AnnEl '  
      lodlabel(idest,6)='NA    ' 
      lodlabel(idest,7)='NA    '      

C First scan the source zone operation file if there is a problem
C reading or it does not exist then return an error state.
C If the user has asked for a verbosity list out the details of
C the source zones air flows.
      IUO=IFIL+1
      if(isource.eq.0)then
        CALL EFOPSEQ(IUO,sfile,1,IER)
        if(ier.eq.0)then
          XST = .true.
          CALL ERPFREE(IUO,ISTAT)
        endif
      else
        call FINDFIL(LPROJ(isource),XST)
      endif
      if(XST)then
        CALL ERPFREE(IUO,ISTAT)
        if(isource.eq.0)then
          if(ifullysilent.eq.1)then
            actflatfile='-'
          else
            actflatfile='W'            
          endif
C Use ERZSCHED to fill P2C and P2CTLC common blocks and
C then jump to 32 to read in the destination operations file.
          CALL ERZSCHED(IUO,sfile,act,IER)
          if(ier.ne.0)then
            call edisp(iuout,'Source operation file not clean...')
            ier=1
            return
          endif
          goto 32
        else

C Read the source operations file from within the model and
C if this is successful the instanciate the P2C and P2CTLC
          CALL EROPER(ITRC,iuout,IUO,isource,IER)
          if(ier.ne.0)then
            call edisp(iuout,'Source operation file not clean...')
            ier=1
            return
          endif
        endif
      else
        call edisp(iuout,'Source operation file not found...')
        ier=1
        return
      endif

C Copy the casual gains into the local arrays 
      ielfc=ielf(isource)
      DO 601 IDTY=1,NBDAYTYPE
        nelc(idty)=nel(IDTY)
        NCASC(idty)=NCAS(IDTY)
        if(NCAS(IDTY).gt.0)then
          do 10 L=1,NCAS(IDTY)
            ICGSC(IDTY,L)=ICGS(IDTY,L)
            ICGFC(IDTY,L)=ICGF(IDTY,L)
            CMGSC(IDTY,L)=CMGS(IDTY,L)
            CMGLC(IDTY,L)=CMGL(IDTY,L)
            RADCC(IDTY,L)=RADC(IDTY,L)
            CONCC(IDTY,L)=CONC(IDTY,L)
            ICGTC(IDTY,L)=ICGT(IDTY,L)
            if(ielfc.ne.0)then
              pfc(IDTY,L)=pf(IDTY,L)
              ipfc(IDTY,L)=ipf(IDTY,L)
              pwrc(IDTY,L)=pwr(IDTY,L)
              bvoltc(IDTY,L)=bvolt(IDTY,L)
              iphasc(IDTY,L)=iphas(IDTY,L)
            endif
  10      continue
        endif
 601  CONTINUE
      oprdescc=oprdesc(isource)

C Scan in the destination operations file, if it does not exist
C then clear casual gain data structure and create a new file.
  32  continue
      call FINDFIL(LPROJ(idest),XST)
      if(XST)then
        CALL ERPFREE(IUO,ISTAT)
        CALL EROPER(ITRC,iuout,IUO,idest,IER)
        if(ier.ne.0)then
          call edisp(iuout,'Destination operation file not clean...')
          ier=1
          return
        endif
      else

C Usually if the user has already nominated a new name for the
C zone operations file nothing will have been written to it and
C so this dialog is brought up to remind the user. The string
C buffer ltmp holds the nominated new name and GFILE the standard
C name based on the zone name.
        call edisp(iuout,'Destination operation file not found...')
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          WRITE(GFILE,'(2a)') 
     &      zname(idest)(1:lnzname(idest)),'.opr'
        else
          WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(idest)(1:lnzname(idest)),'.opr'
        endif
        write(ltmp,'(a)') LPROJ(idest)(1:lnblnk(LPROJ(idest)))
        if(act(1:1).eq.'s')then
          LPROJ(idest)=ltmp
        else
          h(1)='This is the suggested name for the file that will'
          h(2)='hold the imported casual gain data. '
          CALL EASKS(ltmp,' New zone operations file: (confirm) ',
     &      ' ',72,GFILE,'new operations file',IER,2)
          LPROJ(idest)=ltmp
        endif

C This subroutine does not deal with importing air schedules or
C controls so, in the case of a new file ensure nothing but a default
C initial setup is done for these topics.
        ITCTL(idest)=0
        THI(idest)=100.
        TUP(idest)=0.
        TLO(idest)=0.
        ctlstr(idest,1)='no control of air flow  '
        ctlstr(idest,2)='no control of air flow  '
        ctlstr(idest,3)='no control of air flow  '
        DO 400 IDTY=1,NBDAYTYPE
          NAC(IDTY)=0
 400    CONTINUE

C New file will be in sorted format with multiple day types allowed
        ip3ver(idest)=2
      endif

C Copy the local array data into the common blocks overwriting
C the data implied by 'actcas'. Also act on the iwatt directive.
C Note: if the casual gain type is negative then the units are
C already per m2 so no need for conversion.  If isource is zero
C then we do not know the ratio set to 1.0 and inform user.
      if(iwatt(1:6).eq.'W-m2-W')then
        if(isource.eq.0)then
          VAL=10.0
          h(1)='Because this is a pattern file there is no associated'
          h(2)='zone and to do a ratio we need an assumed area. '
          CALL EASKR(VAL,' ',' Assumed floor area of source ? ',
     &        0.10,'F',9999.0,'W',0.0,'assumed floor area',IER,2)
          if(ZBASEA(idest).gt.0.01)then
            ratioarea=VAL/ZBASEA(idest)
          else
            ratioarea=1.0
          endif
        else
          if(ZBASEA(idest).gt.0.01)then
            ratioarea=ZBASEA(isource)/ZBASEA(idest)
          else
            ratioarea=1.0
          endif
        endif
      else
        ratioarea=1.0
      endif
      if(actcas(1:7).eq.'allcas ')then
        ielf(idest)=ielfc
        DO 602 IDTY=1,NBDAYTYPE
          NCAS(IDTY)=NCASC(IDTY)
          nel(IDTY)=nelc(IDTY)
          if(NCASC(idty).gt.0)then
            do 11 L=1,NCASC(IDTY)
              ICGS(IDTY,L)=ICGSC(IDTY,L)
              ICGF(IDTY,L)=ICGFC(IDTY,L)
              if(ICGTC(IDTY,L).lt.0)then
                CMGS(IDTY,L)=CMGSC(IDTY,L)
                CMGL(IDTY,L)=CMGLC(IDTY,L)
              else
                CMGS(IDTY,L)=CMGSC(IDTY,L)/ratioarea
                CMGL(IDTY,L)=CMGLC(IDTY,L)/ratioarea
              endif
              RADC(IDTY,L)=RADCC(IDTY,L)
              CONC(IDTY,L)=CONCC(IDTY,L)
              ICGT(IDTY,L)=ICGTC(IDTY,L)
              if(ielfc.ne.0)then
                pf(IDTY,L)=pfc(IDTY,L)
                ipf(IDTY,L)=ipfc(IDTY,L)
                pwr(IDTY,L)=pwrc(IDTY,L)
                bvolt(IDTY,L)=bvoltc(IDTY,L)
                iphas(IDTY,L)=iphasc(IDTY,L)
              endif
  11        continue
          endif
 602    CONTINUE

C << could also offer this copy process when importing air flow
C << patterns (however it would probably work better to have a
C << separate documentation string for air flows and casual gains.
        if(act(1:1).eq.'-')then
          call edisp(iuout,' ')
          call edisp(iuout,' Current operation notes:')
          call edisp248(iuout,oprdesc(idest),80)
          call edisp(iuout,' ')
          call edisp(iuout,' Source operation notes:')
          call edisp248(iuout,oprdescc,80)
          call edisp(iuout,' ')
          dok=.true.
          call askok('Use the source documentation?',' ',OK,DOK,0)
          if(ok) oprdesc(idest)=oprdescc
        elseif(act(1:1).eq.'s')then
          oprdesc(idest)=oprdescc
        endif

        if(act(1:1).eq.'-')then
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          CALL EASKS248(oprdescc,'Operations notes (please edit):',
     &      ' ',72,'base_case','operation notes',IER,3)
          if(oprdescc(1:2).ne.'  ')oprdesc(idest)=oprdescc
        endif
        if(isource.eq.0)then
          lodlabel(idest,1)=lodlabelc(1)
          lodlabel(idest,2)=lodlabelc(2)
          lodlabel(idest,3)=lodlabelc(3)
          lodlabel(idest,4)=lodlabelc(4)
          lodlabel(idest,5)=lodlabelc(5)
          lodlabel(idest,6)=lodlabelc(6)
          lodlabel(idest,7)=lodlabelc(7)
        else
          lodlabel(idest,1)=lodlabel(isource,1)
          lodlabel(idest,2)=lodlabel(isource,2)
          lodlabel(idest,3)=lodlabel(isource,3)
          lodlabel(idest,4)=lodlabel(isource,4)
          lodlabel(idest,5)=lodlabel(isource,5)
          lodlabel(idest,6)=lodlabel(isource,6)
          lodlabel(idest,7)=lodlabel(isource,7)
        endif
      endif

C If the user has asked for verbosity list out the result.
      if(act(1:1).eq.'-')then
        if(itrc.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'Updated data.... ')
          call edisp(iuout,' ')
          CALL CASINF(idest,iuout)
          call edisp(iuout,' ')
        endif
      endif

C Because there is a small chance that the source file lacked fully
C sorted casual gains (e.g. was an older file) force a sorting.
      call edisp(iuout,'Sorting imported casual gains...')
      call PROCESSOLDCAS(idest,0,iuout,IER)
      call edisp(iuout,'Sorting imported casual gains...completed.')

C Save the destination operation file.
      if(act(1:1).eq.'-')then
        call usrmsg(
     &    'Updating model to reflect changes',
     &    'in destination operation data or file name...','-')
      endif
      CALL EMKOPER(IUO,LPROJ(idest),idest,IER)
      CALL EMKCFG('s',ier)
      if(act(1:1).eq.'-')then
        call usrmsg(
     &    'Updating model to reflect changes',
     &    'in destination operation data or file name...done.','-')
      endif

      return
      end

C ********************* PLTCASGN *********************
C PLTCASGN - plot casual gain data.
C  ICOMP zone number

      subroutine PLTCASGN(ICOMP,IDTYP)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT,IDTY
      REAL CMGS,CMGL,RADC,CONC

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)

      dimension CGAINS(25,7),CGAINL(25,7),CGAINTS(25),CGAINTL(25)

      character lodlabel*6, outs*124

#ifdef OSI
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to etplot
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2
#endif

      logical COLOK
      IDTY=IDTYP
      IDAY=IDTY-1
      JDAY=IDTY
      KDAY=IDTY+1
      IF(IDAY.LE.0)IDAY=NBDAYTYPE
      IF(KDAY.GT.NBDAYTYPE)KDAY=1

C Check if can draw in colour.
C Use one colour for each gain type.
      COLOK=.false.
      if(nzonec.ge.8) COLOK=.true.

C Scan each days gains and calculate maximum gain.
      YMAXWKD=0.
      YMAXSAT=0.
      YMAXSUN=0.
      do 10 I=1,24
        CGNS=0.
        CGNL=0.
        do 20 J=1,NCAS(IDAY)
          if (I.gt.ICGS(IDAY,J).and.I.le.ICGF(IDAY,J)) then
            if (ICGT(IDAY,J).eq.-1) then

C For case of m2/person CMGL1()J remains zero so use cmgs1(J) which
C is the m^2 per person instead. Base graphed value on fractional 
C persons per m^2.
              CGNS=CGNS+(ZBASEA(ICOMP)/CMGS(IDAY,J))*95.
              CGNL=CGNL+(ZBASEA(ICOMP)/CMGS(IDAY,J))*45.
            elseif (ICGT(IDAY,J).lt.-1) then
              CGNS=CGNS+CMGS(IDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(IDAY,J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS(IDAY,J)
              CGNL=CGNL+CMGL(IDAY,J)
            endif
          endif
 20     continue
        YMAXWKD=AMAX1(YMAXWKD,CGNS,CGNL)
 10   continue
      do 12 I=1,24
        CGNS=0.
        CGNL=0.
        do 22 J=1,NCAS(JDAY)
          if (I.gt.ICGS(JDAY,J).and.I.le.ICGF(JDAY,J)) then
            if (ICGT(JDAY,J).eq.-1) then
              CGNS=CGNS+real(int((ZBASEA(ICOMP)/CMGS(JDAY,J))))*95.
              CGNL=CGNL+real(int((ZBASEA(ICOMP)/CMGS(JDAY,J))))*45.
            elseif (ICGT(JDAY,J).lt.-1) then
              CGNS=CGNS+CMGS(JDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(JDAY,J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS(JDAY,J)
              CGNL=CGNL+CMGL(JDAY,J)
            endif
          endif
 22     continue
        YMAXSAT=AMAX1(YMAXSAT,CGNS,CGNL)
 12   continue
      do 14 I=1,24
        CGNS=0.
        CGNL=0.
        do 24 J=1,NCAS(KDAY)
          if (I.gt.ICGS(KDAY,J).and.I.le.ICGF(KDAY,J)) then
            if (ICGT(KDAY,J).eq.-1) then
              CGNS=CGNS+real(int((ZBASEA(ICOMP)/CMGS(KDAY,J))))*95.
              CGNL=CGNL+real(int((ZBASEA(ICOMP)/CMGS(KDAY,J))))*45.
            elseif (ICGT(KDAY,J).lt.-1) then
              CGNS=CGNS+CMGS(KDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(KDAY,J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS(KDAY,J)
              CGNL=CGNL+CMGL(KDAY,J)
            endif
          endif
 24     continue
        YMAXSUN=AMAX1(YMAXSUN,CGNS,CGNL)
 14   continue

C Clear and setup plot area.
      CALL startbuffer()
      CALL win3d(menuchw,10,10,8,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igheight=igwh

C Calculate plotting box locations.
      ipltwd=int(igw/3)-6
      iglsat=igl + int(ipltwd) + 9
      iglsun=igl+2*(int(ipltwd)+9)
      plxmin=0.
      plxmax=24.
      YMAX=AMAX1(YMAXWKD,YMAXSAT,YMAXSUN,10.0)*1.1
      YMIN=0.
      if(ZBASEA(ICOMP).gt.0.0)then
        YMAXF=YMAX/ZBASEA(ICOMP)
      else
        YMAXF=YMAX/1.0
      endif

C Scale and draw right hand (W/m^2) axis first.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAXF,
     &                                         xsc,ysc,sca,xadd,yadd)
      call vrtaxis(YMIN,YMAXF,igr,igb,igt,yadd,ysc,0,1,'Gain (W/m^2)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAX,
     &                                         xsc,ysc,sca,xadd,yadd)

C Scale and draw first day type requested
      CALL horaxis(plxmin,plxmax,igl,igr,igb,xadd,xsc,0,'Time (hrs)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,yadd,ysc,0,0,'Gain (W)')
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)

C Scale and draw second day type requested
      CALL horaxis(plxmin,plxmax,iglsat,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMIM,iupdown,isym)

C Scale and draw third day type requested
      CALL horaxis(plxmin,plxmax,iglsun,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmin,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmax,YMAX,iupdown,isym)

C Scan gains and generate total curves.
C first day type requested
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      do 50 IP=1,7
        do 55 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 55     continue
 50   continue
      do 60 IP=1,NCAS(IDAY)
        ICG=iabs(ICGT(IDAY,IP))
        do 65 IHR=ICGS(IDAY,IP),ICGF(IDAY,IP)-1
          if (ICGT(IDAY,IP).eq.-1) then

C For case of m2/person CMGL is zero so use CMGS to get CGNL
            CGNS=real(int((ZBASEA(ICOMP)/CMGS(IDAY,IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS(IDAY,IP))))*45.
          elseif (ICGT(IDAY,IP).lt.-1) then
            CGNS=CMGS(IDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(IDAY,IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS(IDAY,IP)
            CGNL=CMGL(IDAY,IP)
          endif
          CGAINS(IHR+1,ICG)=CGAINS(IHR+1,ICG)+CGNS
          CGAINL(IHR+1,ICG)=CGAINL(IHR+1,ICG)+CGNL
          CGAINTS(IHR+1)=CGAINTS(IHR+1)+CGNS
          CGAINTL(IHR+1)=CGAINTL(IHR+1)+CGNL
 65     continue
 60   continue

C Plot curves.
      do 70 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        do 75 I=1,3
          if (COLOK) call winscl('z',I)
          iupdown=0
          isym=0
          CALL etplot(XST,CGAINS(IHR,I),iupdown,isym)
          iupdown=1
          CALL etplot(XFN,CGAINS(IHR,I),iupdown,isym)
          if (IHR.lt.24)then
            CALL etplot(XFN,CGAINS(IHR+1,I),iupdown,isym)
          endif
          iupdown=0
          CALL etplot(XST,CGAINL(IHR,I),iupdown,isym)
          iupdown=-4
          CALL etplot(XFN,CGAINL(IHR,I),iupdown,isym)
          if (IHR.lt.24)then
            CALL etplot(XFN,CGAINL(IHR+1,I),iupdown,isym)
          endif
          if (COLOK) call winscl('-',0)
 75     continue
        iupdown=0
        isym=0
        CALL etplot(XST,CGAINTS(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,CGAINTS(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTS(IHR+1),iupdown,isym)
        iupdown=0
        CALL etplot(XST,CGAINTL(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,CGAINTL(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTL(IHR+1),iupdown,isym)
 70   continue

C second day type requested
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      do 150 IP=1,7
        do 155 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 155    continue
 150  continue
      do 160 IP=1,NCAS(JDAY)
        ICG=iabs(ICGT(JDAY,IP))
        do 165 IHR=ICGS(JDAY,IP),ICGF(JDAY,IP)-1
          if (ICGT(JDAY,IP).eq.-1) then
            CGNS=real(int((ZBASEA(ICOMP)/CMGS(JDAY,IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS(JDAY,IP))))*45.
          elseif (ICGT(JDAY,IP).lt.-1) then
            CGNS=CMGS(JDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(JDAY,IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS(JDAY,IP)
            CGNL=CMGL(JDAY,IP)
          endif
          CGAINS(IHR+1,ICG)=CGAINS(IHR+1,ICG)+CGNS
          CGAINL(IHR+1,ICG)=CGAINL(IHR+1,ICG)+CGNL
          CGAINTS(IHR+1)=CGAINTS(IHR+1)+CGNS
          CGAINTL(IHR+1)=CGAINTL(IHR+1)+CGNL
 165    continue
 160  continue

C Plot curves.
      do 170 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        do 175 I=1,3
          if (COLOK) call winscl('z',I)
          iupdown=0
          isym=0
          CALL etplot(XST,CGAINS(IHR,I),iupdown,isym)
          iupdown=1
          CALL etplot(XFN,CGAINS(IHR,I),iupdown,isym)
          if (IHR.lt.24)then
            CALL etplot(XFN,CGAINS(IHR+1,I),iupdown,isym)
          endif
          iupdown=0
          CALL etplot(XST,CGAINL(IHR,I),iupdown,isym)
          iupdown=-4
          CALL etplot(XFN,CGAINL(IHR,I),iupdown,isym)
          if (IHR.lt.24)then
            CALL etplot(XFN,CGAINL(IHR+1,I),iupdown,isym)
          endif
          if (COLOK) call winscl('-',0)
 175    continue
        iupdown=0
        isym=0
        CALL etplot(XST,CGAINTS(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,CGAINTS(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTS(IHR+1),iupdown,isym)
        iupdown=0
        CALL etplot(XST,CGAINTL(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,CGAINTL(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTL(IHR+1),iupdown,isym)
 170  continue

C third day type requested
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      do 250 IP=1,7
        do 255 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 255    continue
 250  continue
      do 260 IP=1,NCAS(KDAY)
        ICG=iabs(ICGT(KDAY,IP))
        do 265 IHR=ICGS(KDAY,IP),ICGF(KDAY,IP)-1
          if (ICGT(KDAY,IP).eq.-1) then
            CGNS=real(int((ZBASEA(ICOMP)/CMGS(KDAY,IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS(KDAY,IP))))*45.
          elseif (ICGT(KDAY,IP).lt.-1) then
            CGNS=CMGS(KDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(KDAY,IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS(KDAY,IP)
            CGNL=CMGL(KDAY,IP)
          endif
          CGAINS(IHR+1,ICG)=CGAINS(IHR+1,ICG)+CGNS
          CGAINL(IHR+1,ICG)=CGAINL(IHR+1,ICG)+CGNL
          CGAINTS(IHR+1)=CGAINTS(IHR+1)+CGNS
          CGAINTL(IHR+1)=CGAINTL(IHR+1)+CGNL
 265    continue
 260  continue

C Plot curves.
      do 270 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        do 275 I=1,3
          if (COLOK) call winscl('z',I)
          iupdown=0
          isym=0
          CALL etplot(XST,CGAINS(IHR,I),iupdown,isym)
          iupdown=1
          CALL etplot(XFN,CGAINS(IHR,I),iupdown,isym)
          if (IHR.lt.24) CALL etplot(XFN,CGAINS(IHR+1,I),iupdown,isym)
          iupdown=0
          CALL etplot(XST,CGAINL(IHR,I),iupdown,isym)
          iupdown=-4
          CALL etplot(XFN,CGAINL(IHR,I),iupdown,isym)
          if (IHR.lt.24) CALL etplot(XFN,CGAINL(IHR+1,I),iupdown,isym)
          if (COLOK) call winscl('-',0)
 275    continue
        iupdown=0
        isym=0
        CALL etplot(XST,CGAINTS(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,CGAINTS(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTS(IHR+1),iupdown,isym)
        iupdown=0
        CALL etplot(XST,CGAINTL(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,CGAINTL(IHR),iupdown,isym)
        CALL etplot(XFN,CGAINTL(IHR+1),iupdown,isym)
 270  continue

C Title and labels.
      write (outs,'(2a)') 'Casual gains for ',ZNAME(ICOMP)
      iside=1
      isize=1
      ifont=1
      call viewtext(outs,iside,isize,ifont)
      iid1=igl+20; iid2=igt-65;
      call wstxpt(iid1,iid2,' Total sensible')
      iid1=igr-100; iid2=igt-65;
      call wstxpt(iid1,iid2,'Total latent')
      iid1=igl+120; iid2=igt-69;
      call pixel2u(igl+120,igt-69,gx,gy)
      iupdown=0
      isym=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igl+180,igt-69,gx,gy)
      iupdown=1
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-120,igt-69,gx,gy)
      iupdown=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-180,igt-69,gx,gy)
      iupdown=-4
      CALL etplot(gx,gy,iupdown,isym)
      do 300 I=1,3
        write(outs,'(2a)')
     &        lodlabel(ICOMP,I)(1:lnblnk(lodlabel(ICOMP,I))),' sensible'
        IY=igt-(12*I)-18
        iid1=igl+20; iid2=IY;
        call wstxpt(iid1,iid2,outs)
        write(outs,'(2a)')
     &          lodlabel(ICOMP,I)(1:lnblnk(lodlabel(ICOMP,I))),' latent'
        iid1=igr-100; iid2=IY;
        call wstxpt(iid1,iid2,outs)
        if (COLOK) call winscl('z',I)
        call pixel2u(igl+120,IY-4,gx,gy)
        iupdown=0
        isym=0
        CALL etplot(gx,gy,iupdown,isym)
        call pixel2u(igl+180,IY-4,gx,gy)
        iupdown=1
        CALL etplot(gx,gy,iupdown,isym)
        call pixel2u(igr-120,IY-4,gx,gy)
        iupdown=0
        CALL etplot(gx,gy,iupdown,isym)
        call pixel2u(igr-180,IY-4,gx,gy)
        iupdown=-4
        CALL etplot(gx,gy,iupdown,isym)
        if (COLOK) call winscl('-',0)
 300  continue

      ioff=(int(ipltwd)/3)
      iid1=igl+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(IDAY))
      iid1=iglsat+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(JDAY))
      iid1=iglsun+ioff; iid2=igt-10;
      call wstxpt(iid1,iid2,CALENTAG(KDAY))
      call forceflush()

      return
      end

C ************* insdelcasper
C Inscasper inserts or deletes a casual gain period into the
C current P3N & elp3N data structure.
C Parameters:
C   icomp - current zone.
C   idaytype - day type (weekdays = 1, sat = 2, sun = 3 
C   idp - point of insertion or deletion. 
C   ier - set to 1 if there was a problem
      subroutine insdelcasper(icomp,idaytype,act,idp,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,IDTY
      REAL CMGS,CMGL,RADC,CONC

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      character act*1
      IDTY=IDAYTYPE

C For insertion set loop limites depending on the day type.
      if(act.eq.'i')then
        NCASX=NCAS(IDTY)+1
        if(ncasX.gt.MC)then
          call edisp(iuout,'No additional periods can be inserted.')
          ier=1
          return
        endif

C Use the same 234 logic as found in insert.F to shift P3 and elp3
C data up.
        ix = ncasX+1
 234    continue
        ix = ix-1
        call CPYCASIJ(icomp,IDTY,ix,ix-1)
        ncas(IDTY)=ncas(IDTY)+1
        if(ielf(icomp).ne.0)then
          nel(IDTY)=nel(IDTY)+1
        endif
        IF(IX.GT.idp+1)GOTO 234
        return
      elseif(act.eq.'d')then

C Delete period idp by copying data above it down one and then
C decrementing the appropraite counter. If the period to delete
C is the last possible period then only decrement counters.
        if(idp.eq.MC)then
          NCAS(IDTY)=NCAS(IDTY)-1
          if(ielf(icomp).ne.0)nel(IDTY)=nel(IDTY)-1
        else
          do 110 IX=IDP,NCAS(IDTY)
            call CPYCASIJ(icomp,IDTY,ix,ix+1)
            if(ielf(icomp).ne.0)then
              nel(IDTY)=nel(IDTY)-1
            endif
  110     continue
          NCAS(IDTY)=NCAS(IDTY)-1
        endif
        return
      endif
      end

C ******* initcasday
C initcasday does initial setup of all casual day types. 
      subroutine initcasday(ier)

#include "building.h"
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/P3N/NCAS(MDTY),ICGS(MDTY,MC),ICGF(MDTY,MC),
     &CMGS(MDTY,MC),CMGL(MDTY,MC),RADC(MDTY,MC),CONC(MDTY,MC)
      COMMON/P3TYPEN/ICGT(MDTY,MC)
      INTEGER NCAS,ICGS,ICGF,ICGT,ILN
      REAL CMGS,CMGL,RADC,CONC
      dimension iper(24),iperf(24)
      character hold*48,H*72,OUTS*12,LOUTS*24,VOUTS*50

      h(1)='For the current day type specify how many periods'
      h(2)='for each casual gain type (occupants, lights and'
      h(3)='small power). Do not include periods for gain types'
      h(4)='which you will be importing from an Events Profiles'
      h(5)='database.'
      h(6)=' '
      h(7)='If you set the number of periods for a type to zero'
      h(8)='you can update it later. The number of periods should'
      h(9)='cover the whole day (i.e. from 0h00 to 24h00). For '
      h(10)='example, if there are gains between 8h00 and 18h00'
      h(11)='and nothing at other times, define this as three'
      h(12)='periods - 0h00 to 8h00 with no gains, 8h00 to 18h00'
      h(13)='with gains and 18h00 to 24h00 with no gains.'
      h(14)='Period start times should be in ascending order.'

      DO 600 IDTY=1,NBDAYTYPE  
        nbocper=1
        nbltper=1
        nbspper=1
        hold='                                    '
        write(hold,'(3i5,a)') nbocper,nbltper,nbspper,'  '
        OUTS=CALENTAG(IDTY)
        ILN=LNBLNK(OUTS)
        WRITE(VOUTS,'(3A)')'No. of ',OUTS(1:ILN),
     &  ' casual gain periods?'
        CALL EASKS(HOLD,VOUTS,
     &  'occupants / lights / equipment or small power',
     &  24,'  0  0  0  ','no. CG periods',IER,14)
        K=0
        CALL EGETWI(HOLD,K,nbocper,0,24,'F','occup perds',IER)
        CALL EGETWI(HOLD,K,nbltper,0,24,'F','light perds',IER)
        CALL EGETWI(HOLD,K,nbspper,0,24,'F','sm pwr perds',IER)

C Debug.
C        write(6,*) 'occup light sm pwr per ',nbocper,nbltper,hbspper

C Get user to specify the start times for occup periods if non-zero.
C Increment ipcount for each new period.
        ipcount=NCAS(IDTY)
        if(nbocper.gt.0)then
          WRITE(LOUTS,'(2A)')OUTS(1:ILN),' occupant'
          call initcasper(nbocper,iper,iperf,'i',LOUTS)
          do 71 ij=1,nbocper
            ipcount=ipcount+1
            ICGS(IDTY,ipcount) = iper(ij)
            ICGF(IDTY,ipcount) = iperf(ij)
            CMGS(IDTY,ipcount) = 0.0
            CMGL(IDTY,ipcount) = 0.0
            RADC(IDTY,ipcount) = 0.5
            CONC(IDTY,ipcount) = 0.5
            ICGT(IDTY,ipcount) = 1
  71      continue
        endif
        if(nbltper.gt.0)then
          WRITE(LOUTS,'(2A)')OUTS(1:ILN),' lighting'
          call initcasper(nbltper,iper,iperf,'i',LOUTS)
          do 72 ij=1,nbltper
            ipcount=ipcount+1
            ICGS(IDTY,ipcount) = iper(ij)
            ICGF(IDTY,ipcount) = iperf(ij)
            CMGS(IDTY,ipcount) = 0.0
            CMGL(IDTY,ipcount) = 0.0
            RADC(IDTY,ipcount) = 0.5
            CONC(IDTY,ipcount) = 0.5
            ICGT(IDTY,ipcount) = 2
  72      continue
        endif
        if(nbspper.gt.0)then
          WRITE(LOUTS,'(2A)')OUTS(1:ILN),' small power'
          call initcasper(nbspper,iper,iperf,'i',LOUTS)
          do 73 ij=1,nbspper
            ipcount=ipcount+1
            ICGS(IDTY,ipcount) = iper(ij)
            ICGF(IDTY,ipcount) = iperf(ij)
            CMGS(IDTY,ipcount) = 0.0
            CMGL(IDTY,ipcount) = 0.0
            RADC(IDTY,ipcount) = 0.4
            CONC(IDTY,ipcount) = 0.6
            ICGT(IDTY,ipcount) = 3
  73      continue
        endif
        NCAS(IDTY)=NCAS(IDTY)+nbocper+nbltper+nbspper
 600  CONTINUE
      return
      end

C ******* initcasper
C initcasper supports setting up initial periods for
C schedules. It is passed nper and returns an array iper(24)
C with the start times of each period and iperf(24) with finish
C times for each period. If act='e' the allow
C editing of current array values. If act='i' initialise and 
C edit.
C << Note change to real when casual gains data structure improves >>
      subroutine initcasper(nper,iper,iperf,act,label)
      common/pophelp/h(60)
      character act*1,cstr*72,h*72,msg*72
      character*(*) label
      dimension iper(24),iperf(24)
      real incr

      if(nper.lt.24)incr=1.
      if(nper.lt.12)incr=2.
      if(nper.lt.8)incr=3.
      if(nper.lt.4)incr=6.
      if(act.eq.'i')then
        DO 100 I = 1,nper
          if(I.eq.1)then
            iper(I)=0
          else
            iper(I)=iperf(I-1)
          endif
          iperf(I)=iper(I)+nint(incr)
          IF(iperf(I).gt.24) iperf(I)=24
100     CONTINUE
      endif

C Make up an editing string for the start of each period.
      CSTR='                                                        '
      K=1
      DO 60 J=1,nper
        K1=K+5
        WRITE(CSTR(K:K1),'(i5)') iper(J)
        K=K1+1
   60 CONTINUE
      h(1)='If you define actual start times the subsequent'
      h(2)='editing tasks will be easier.'
      write(msg,'(3a)') 'Start time for each ',
     &  label(1:lnblnk(label)),' period.'
      CALL EASKS(CSTR,' ',msg,72,' 0  5  6  8 ','time starts',IER,2)

C Extract the start timings and then sort out the finishes.
      K=0
      DO 61 J=1,nper
        CALL EGETWI(cstr,K,IR,0,23,'W','event start',IER)
        iper(J)=IR
   61 CONTINUE
      DO 62 J=1,nper
        if(j.ne.nper)then
          iperf(J)=iper(J+1)
        else
          iperf(J)=24
        endif
   62 CONTINUE

      return
      end
