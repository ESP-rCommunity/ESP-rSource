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
C EDAIR: Edits air flow portion of a zone operations file.
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

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C LPROJ   - zone scheduling (operation file)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      common/pophelp/h(60)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver
      COMMON/P1/oprdesc,ctlstr
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
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

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER oprdesc*248,ctlstr*24,zname*12,zdesc*64,tcname*248
      character ventdesc*248
      CHARACTER ITEM(14)*33,choices(12)*32,H*72,LTMP*72,GFILE*72
      CHARACTER*6 lodlabel
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72,fs*1,outs*124
      character act*8,iwatt*8,afact*8,iflow*8,vflow*8

      LOGICAL OK,DOK,XST,MODOPR,browse,unixok,needtoask

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      ventdesc=' '

C Initially assume periods are not sorted.
      sorted=.false.
      needtoask=.true.
      gotviabrowse=.false.

C If version facility invoked this subroutine then 
C just write operations file
      IF(IVER.NE.0)then
        ICOMP=IVER
        if(cfgok.and.(.NOT.browse))then
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
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
        WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &    '.opr'
      else
        WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.opr'
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
        H(10)='the same definition as used by another zone. '
        CALL EASKS2CMD(LTMP,' ','Zone operations file options:',
     &    'browse','cancel',iclkok,72,GFILE,
     &    'operations file',IER,13)
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
        CALL EASKS2CMD(LTMP,' ','Confirm zone operations file:',
     &         'browse','cancel',iclkok,72,GFILE,
     &          'operations file',IER,16)
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

      NAC1=0
      NAC2=0
      NAC3=0
      NCAS1=0
      NCAS2=0
      NCAS3=0

C Instanciate prior to scanning the source operation file.
      ctlstr='no control of air flow  '
      oprdesc='no operations notes (yet)'
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
C default data (and set ip3ver to 1 in case user does nothing else).
      IUO=IFIL+1
      call FINDFIL(LPROJ(ICOMP),XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)

C Do cursory check to see if the file is sorted.
        sorted=.true.
        problem=.false.
        call checksort(icomp,1,problem,ier)
        if(problem)then
          sorted=.false.
          MODOPR=.true.
          call edisp(iuout,'Weekday casual gains might be unsorted.')
        endif
        problem=.false.
        call checksort(icomp,2,problem,ier)
        if(problem)then
          sorted=.false.
          MODOPR=.true.
          call edisp(iuout,'Saturday casual gains might be unsorted.')
        endif
        problem=.false.
        call checksort(icomp,3,problem,ier)
        if(problem)then
          sorted=.false.
          MODOPR=.true.
          call edisp(iuout,'Sunday casual gains might be unsorted.')
        endif
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
        h(31)='After initiating one of the above actions you can edit'
        h(32)='the data as required. '
        choices(1) ='a define from scratch '
        choices(2) ='b air flow     < another zone'
        choices(3) ='c casual gains < another zone'
        choices(4) ='d air & gains  < another zone'
        choices(5) ='e air flow     < from pattern'
        choices(6) ='f casual gains < from pattern'
        choices(7) ='g air & gains  < from pattern'
        choices(8) =' ________________________'
        choices(9) ='! cancel operations     '
        choices(10)='? help                  '
        NIGU=10
        IGU=-2
        if(mmod.eq.8)then
          CALL EMENU('Operations File Options',choices,NIGU,IGU)
        else
          CALL EMENU('Options',choices,NIGU,IGU)
        endif

        if(igu.eq.0)then
          goto 19
        elseif(igu.eq.8)then
          goto 19
        elseif(igu.eq.9)then

C User said to cancel so return from the subroutine without
C doing anything.
C          call edisp(iuout,
C     &      'You asked to cancel so returning without changes.')
          return
        elseif(igu.eq.10)then

C Explain
          CALL PHELPD('operation choices',32,'-',0,0,IER)
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
     &          ZNAME(ICOMP)(1:lnblnk(ZNAME(ICOMP))),').'
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
            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))
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
C The from scratch should intially be version 2.
          ip3ver=2
          h(1)='New operations for this zone. Please check your notes'
          h(2)='and provide initial casual gain period start times'
          h(3)='for each day type (Weekday, Saturday & Sunday) and'
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

          call initcasday('wk',ier)
          call initcasday('sa',ier)
          call initcasday('su',ier)

C debug
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
     &          ZNAME(ICOMP)(1:lnblnk(ZNAME(ICOMP))),').'
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
            write(6,*) 'the browsed file is ',sfile(1:lnblnk(sfile))
            write(6,*) 'the snpfile is ',snpfile(1:lnblnk(snpfile))
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
      if(ip3ver.le.2)then
        write(ITEM(2),'(3a)') 'a description: ',oprdesc(1:16),'-'
        write(ITEM(3),'(2x,2a)')   oprdesc(17:45),'-'
        write(ITEM(4),'(2x,2a)')   oprdesc(46:74),'-'
        write(ITEM(5),'(2x,2a)')   oprdesc(75:103),'-'
        ITEM(6)=              '  ----------------------------- '
        ITEM(7)=              'c edit scheduled air flows      '
        ITEM(8)=              'd edit casual gains             '
        ITEM(9)=              '  ----------------------------- '    
        ITEM(10)=             '! list air flow & casual gains  '
        ITEM(11)=             '  ----------------------------- '
        ITEM(12)=             '> save air flow & casual gains  '
        ITEM(13)=             '? help                          '
        ITEM(14)=             '- exit this menu                '
        NITEMS=14
      elseif(ip3ver.eq.3)then
        write(ITEM(2),'(3a)') 'a inf vent notes: ',
     &    ventdesc(1:13),'-'
        write(ITEM(3),'(2x,a)')    ventdesc(14:44)
        write(ITEM(4),'(3a)') 'b casual notes: ',
     &    oprdesc(1:15),'-'
        write(ITEM(5),'(2x,a)')    oprdesc(16:44)
        ITEM(6)=              '  ----------------------------- '
        ITEM(7)=              'c edit scheduled air flows      '
        ITEM(8)=              'd edit casual gains             '
        ITEM(9)=              '  ----------------------------- '    
        ITEM(10)=             '! list air flow & casual gains  '
        ITEM(11)=             '  ----------------------------- '
        ITEM(12)=             '> save air flow & casual gains  '
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
      H(7) =' 2) Weekday, Saturday and Sunday air flow data:'
      H(8) ='       start and finish hours for each period'
      H(9) ='       infiltration and ventilation rates (ACH)'
      H(10)='       coupling index: '
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
      H(34) =' 4) Weekday, Saturday and Sunday casual gains data:'
      H(35) ='       start and finish hours for each period'
      H(36) ='       casual gain type:'
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
               call PROCESSOLDCAS(ICOMP,0,iuout,IER)
               MODOPR=.true.
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
          LTMP=LPROJ(ICOMP)
   91     H(1)='A zone operation file contains schedules of air flow'
          H(2)='and casual gains.  Air flows may have thermostatic'
          H(3)='or wind speed control. '
          CALL EASKS(LTMP,' ','Zone operations file name?',
     &           72,GFILE,'operations file',IER,3)
          IF(LTMP.NE.' ')THEN
            call usrmsg(
     &        'Updating model to reflect changes',
     &        'in zone operations data or file name...','-')
            LPROJ(ICOMP)=LTMP
            CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
            CALL EMKCFG('-',ier)
            call usrmsg(
     &        'Updating model to reflect changes',
     &        'in zone operations data or file name...done.','-')
          ELSE
            GOTO 91
          ENDIF
        endif
        RETURN
      ELSEIF(INO.EQ.2)THEN
        if(ip3ver.le.2)then

C Edit operation description.
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          tcname=oprdesc
          CALL EASKS248(tcname,' ','Zone operations notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')oprdesc=tcname
          MODOPR=.true.
        elseif(ip3ver.eq.3)then
          H(1)='Notes (<248 char) for schedules of zone air flow can' 
          H(2)='be included in the model. Use this facility to'
          H(3)='clarify what is happening in the zone.'
          tcname=ventdesc
          CALL EASKS248(tcname,' ','Zone scheduled air notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')ventdesc=tcname
          MODOPR=.true.
        endif
      ELSEIF(INO.EQ.4)THEN
        if(ip3ver.eq.3)then
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          tcname=oprdesc
          CALL EASKS248(tcname,' ','Zone operations notes:',
     &          72,'base_case','operation notes',IER,3)
          if(tcname(1:2).ne.'  ')oprdesc=tcname
          MODOPR=.true.
        endif
      ELSEIF(INO.EQ.7)THEN

C Edit air flows.
        CALL EDAIR2(ICOMP,IER)
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
   11     CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,ITRU,IER)
          IF(IER.LT.0)THEN
            DOK=.true.
            h(1)='You were presented with this message because a'
            h(2)='problem was detected while trying to update the'
            h(3)='zone operations file. Check that you have sufficient'
            h(4)='disk space and/or permission to update the mode.'
            CALL ASKOK('Problem encountered saving data!',
     &      'Try again?',OK,DOK,4)
            IF(OK)THEN
              GOTO 11
            ELSE
              IER=1
              RETURN
            ENDIF
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
      SUBROUTINE EDAFCTL(ICOMP,IER)

#include "building.h"
      common/pophelp/h(60)
      COMMON/PREC2/VOL(MCOM)
      CHARACTER oprdesc*248,ctlstr*24
      COMMON/P1/oprdesc,ctlstr
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      common/p3ver/ip3ver

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
        ctlstr='no control of air flow  '
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
      if(ip3ver.le.2)then
        IY=ITCTL(ICOMP)
        if(IY.eq.-14)then
          ctlstr='infil only: wind speed  '
        elseif(IY.eq.-13)then
          ctlstr='infil only: ext db temp '
        elseif(IY.eq.-12)then
          ctlstr='infil only: adj zone tmp'
        elseif(IY.eq.-11)then
          ctlstr='infil only: zone temp   '
        elseif(IY.eq.-4)then
          ctlstr='vent only: wind speed   '
        elseif(IY.eq.-3)then
          ctlstr='vent only: ext db temp  '
        elseif(IY.eq.-2)then
          ctlstr='vent only: adj zone temp'
        elseif(IY.eq.-1)then
          ctlstr='vent only: zone temp    '
        elseif(IY.eq.4)then
          ctlstr='infil & vent: wind speed'
        elseif(IY.eq.3)then
          ctlstr='infil & vent: ext db T  '
        elseif(IY.eq.2)then
          ctlstr='infil & vent: adj zone T'
        elseif(IY.eq.1)then
          ctlstr='infil & vent: zone T    '
        elseif(IY.eq.0)then
          ctlstr='no control of air flow  '
        else
          ctlstr='unknown flow control   '
        endif
      elseif(ip3ver.eq.3)then

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
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL

      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      common/pophelp/h(60)

      CHARACTER oprdesc*248,ctlstr*24
      COMMON/P1/oprdesc,ctlstr
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)

      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character zname*12,zdesc*64,vert(35)*33,H*72,KEY*1,title*33
      character LTMP*72
      character afact*8,iflow*8,vflow*8,outs*124,sfile*72,snpfile*72
      logical XST,closezero,closeone,closetwo

C Initial menu entry setup.
      idtyp = 1
 5    MHEAD=3
      MCTL=5
      if(idtyp.eq.1)then
        ILEN=NAC1*3
      elseif(idtyp.eq.2)then 
        ILEN=NAC2*3
      elseif(idtyp.eq.3)then
        ILEN=NAC3*3
      endif
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Check and plot air flows. 
      call CHKAIRFL
      if (MMOD.eq.8) call PLTAIR(ICOMP)

C Set up top of menu.
  44  if (ITCTL(ICOMP).eq.0) then 
        write(VERT(1),'(A)') '1 air flow control (none)'
      else
        write(VERT(1),'(2A)') '1 control: ',ctlstr(1:22)
      endif
      vert(2) =              '  -----------------------------'
      if(idtyp.eq.1)then
        write(vert(3),'(a,i2,a)') '> air flow>> Weekdays (',NAC1,')'
      elseif(idtyp.eq.2)then 
        write(vert(3),'(a,i2,a)') '> air flow>> Saturdays (',NAC2,')'
      elseif(idtyp.eq.3)then
        write(vert(3),'(a,i2,a)') '> air flow>> Sundays (',NAC3,')'
      endif

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

C Weekday loop
      if(idtyp.eq.1)then
        istartcount=1
        iendcount=0
        do 10 L=1,NAC1

C Ensure that all three lines can be written. If on subsequent
C pages then check loosely in order to catch an item that 
C would have otherwise spanned the page divide.
          iendcount=istartcount+2
C          if (istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL))) then
          if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
            continue
          elseif(istartcount+2.GE.IST.AND.
     &          (iendcount.LE.(IST+MIFULL)))then
            continue
          else
            istartcount=iendcount+1
            goto 10
          endif
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &      IACS1(L),'-',IACF1(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,f6.2)') KEY,'  infil rate:',ACI1(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          if (IPT1(L).gt.0) then
            write (VERT(M),'(2a,f6.2,1x,a)') KEY,'  vent rate:',
     &        ACV1(L),zname(IPT1(L))(1:10)
          elseif (ACV1(L).lt.0.0001) then 
            write (VERT(M),'(2a,f6.2,a)') KEY,'  vent rate:',
     &        ACV1(L),'  n/a'
          else
            write (VERT(M),'(2a,f6.2,a,f6.2)') KEY,'  vent rate:',
     &        ACV1(L),'@temp:',TA1(L)
          endif
          istartcount=iendcount+1
 10     continue

C Saturday loop
      elseif(idtyp.eq.2)then 
        istartcount=1
        iendcount=0
        do 20 L=1,NAC2

C Ensure that all three lines can be written. If on subsequent
C pages then check back two in order to catch an item that 
C would have otherwise spanned the page divide.
          iendcount=istartcount+2
          if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
            continue
          elseif(istartcount+2.GE.IST.AND.
     &          (iendcount.LE.(IST+MIFULL)))then
            continue
          else
            istartcount=iendcount+1
            goto 20
          endif
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &      IACS2(L),'-',IACF2(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,f6.2)') KEY,'  infil rate:',ACI2(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          if (IPT1(L).gt.0) then
            write (VERT(M),'(2a,f6.2,1x,a)') KEY,'  vent rate:',
     &        ACV2(L),zname(IPT2(L))(1:10)
          elseif (ACV1(L).lt.0.0001) then 
            write (VERT(M),'(2a,f6.2,a)') KEY,'  vent rate:',
     &        ACV2(L),'  n/a'
          else
            write (VERT(M),'(2a,f6.2,a,f6.2)') KEY,'  vent rate:',
     &        ACV2(L),'@temp:',TA2(L)
          endif
          istartcount=iendcount+1
 20     continue

C Sunday loop.
      elseif(idtyp.eq.3)then
        istartcount=1
        iendcount=0
        do 30 L=1,NAC3

C Ensure that all three lines can be written.
          iendcount=istartcount+2
          if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
            continue
          elseif(istartcount+2.GE.IST.AND.
     &          (iendcount.LE.(IST+MIFULL)))then
            continue
          else
            istartcount=iendcount+1
            goto 30
          endif
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &      IACS3(L),'-',IACF3(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          write (VERT(M),'(2a,f6.2)') KEY,'  infil rate:',ACI3(L)
          M=M+1
          call EMKEY(M-MHEAD,KEY,IER)
          if (IPT1(L).gt.0) then
            write (VERT(M),'(2a,f6.2,1x,a)') KEY,'  vent rate:',
     &        ACV3(L),zname(IPT3(L))(1:10)
          elseif (ACV1(L).lt.0.0001) then 
            write (VERT(M),'(2a,f6.2,a)') KEY,'  vent rate:',
     &        ACV3(L),'  n/a'
          else
            write (VERT(M),'(2a,f6.2,a,f6.2)') KEY,'  vent rate:',
     &        ACV3(L),'@temp:',TA3(L)
          endif
          istartcount=iendcount+1
 30     continue
      endif

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
     &    zname(icomp)(1:lnblnk(zname(icomp)))
      else
        write (title,'(2a)') 'Ventilation in ',
     &    zname(icomp)(1:lnblnk(zname(icomp)))
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
        if (IW.EQ.1) then 
          call EASKATOG(' ','New air flow period is for day type:',
     &      'Weekdays','Saturdays','Sundays','All','Cancel',
     &      ' ',' ',IW,35)
          if (IW.ne.5) then
            if (IW.eq.1) then
              if(NAC1.eq.MA)then
                call usrmsg(' ','Sorry, too many weekday periods.','W')
              else
                IDT=1
                IPER=0
                call EDAIRFL(ICOMP,IDT,IPER,-1)
              endif
            elseif (IW.eq.2) then
              if(NAC2.eq.MA)then
               call usrmsg(' ','Sorry, too many Saturday periods.','W')
              else
                IDT=2
                IPER=0
                call EDAIRFL(ICOMP,IDT,IPER,-1)
              endif
            elseif (IW.eq.3) then
              if(NAC3.eq.MA)then
                call usrmsg(' ','Sorry, too many Sunday periods.','W')
              else
                IDT=3
                IPER=0
                call EDAIRFL(ICOMP,IDT,IPER,-1)
              endif
            elseif (IW.eq.4) then

C Add a period to all day types.
              if(NAC1.eq.MA)then
                call usrmsg(' ','Sorry, too many Weekday periods.','W')
              elseif(NAC2.eq.MA)then
                call usrmsg(' ','Sorry, too many Saturday periods.','W')
              elseif(NAC3.eq.MA)then
                call usrmsg(' ','Sorry, too many Sunday periods.','W')
              else
                IDT=4
                IPER=0
                call EDAIRFL(ICOMP,IDT,IPER,-1)
              endif
            elseif (IW.eq.5) then
              continue
            endif
          endif
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
     &          ZNAME(ICOMP)(1:lnblnk(ZNAME(ICOMP))),').'
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
          if(idtyp.eq.1)then
            ILEN=NAC1*3
          elseif(idtyp.eq.2)then 
            ILEN=NAC2*3
          elseif(idtyp.eq.3)then
            ILEN=NAC3*3
          endif
          goto 44
        ENDIF
      elseif (IVERT.EQ.1) then

C Control of air flow.
        call EDAFCTL(ICOMP,IER)
      elseif (IVERT.EQ.3) then

C Increment the daytype focus.
        idtyp = idtyp + 1
        if(idtyp.gt.3) idtyp = 1
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
C     =2 infil, =0 ventilation.

      subroutine EDAIRFL(ICOMP,IDT,IPER,itype)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/PREC2/VOL(MCOM)
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)

      character OUTS*124,HOLD*16,h*72,zname*12,zdesc*64

C Working variables
      real ACI,ACV,TA,XTA,XACH
      integer IACS,IACF,IPT

C << This subroutine will need further code changes to implement
C << user cancel actions.

C If new period then set defaults otherwise get data from common.
      if (IPER.eq.0) then
        IACS=0
        IACF=24
        ACI=0.
        ACV=0.
        IPT=0
        TA=0.
      elseif (IDT.eq.1) then
        IACS=IACS1(IPER)
        IACF=IACF1(IPER)
        ACI=ACI1(IPER)
        ACV=ACV1(IPER)
        IPT=IPT1(IPER)
        TA=TA1(IPER)
      elseif (IDT.eq.2) then
        IACS=IACS2(IPER)
        IACF=IACF2(IPER)
        ACI=ACI2(IPER)
        ACV=ACV2(IPER)
        IPT=IPT2(IPER)
        TA=TA2(IPER)
      elseif (IDT.eq.3) then
        IACS=IACS3(IPER)
        IACF=IACF3(IPER)
        ACI=ACI3(IPER)
        ACV=ACV3(IPER)
        IPT=IPT3(IPER)
        TA=TA3(IPER)
      endif

C Ask for start and finish times.
      if(itype.eq.-1.or.itype.eq.1)then
 10     write (OUTS,'(A)') 'Start and finish hours for period?'
        write (HOLD,'(2i5,a)') IACS,IACF,'  '
        h(1)='Period during which air flow applies.'
        CALL EASKS(HOLD,OUTS,' ',16,' 0 24 ','air st-fn hr',IER,1)
        K=0
        CALL EGETWI(HOLD,K,IVS,0,24,'F','period start',IER)
        CALL EGETWI(HOLD,K,IVF,IVS+1,24,'F','period end',IER)
        if (IER.ne.0) goto 10
        IACS=IVS
        IACF=IVF
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
 20         XACH=ACI
            h(1)='Supply the infiltration rate in ac/h.'
            call EASKR(XACH,' ','Infiltration rate?',
     &        0.0,'W',10.0,'W',0.5,'infil rate ach',IERI,1)
            if(IERI.eq.0)then
              ACI=XACH
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 20
            endif
          endif
        
          if(itype.eq.-1.or.itype.eq.0)then
 22         XACH=ACV
            h(1)='Supply the ventilation rate in ac/h relative'
            h(2)='to the volume of the receiving zone.'
            call EASKR(XACH,' ','Ventilation rate?',
     &        0.0,'W',10.0,'W',0.0,'vent rate ach',IERI,2)
            if(IERI.eq.0)then
              ACV=XACH
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 22
            endif
          endif
        elseif (IGU.eq.2) then

C Flow as m^3/s.
 30       if(itype.eq.-1.or.itype.eq.2)then
            XACH=ACI*VOL(ICOMP)/3600.
            h(1)='Supply the infiltration rate in m^3/s.'
            call EASKR(XACH,' ','Infiltration rate?',
     &        0.0,'W',10.0,'W',0.0,'inf rate m3s',IERI,1)
            if(IERI.eq.0)then
              ACI=XACH*3600.0/VOL(ICOMP)
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 30
            endif
          endif

          if(itype.eq.-1.or.itype.eq.0)then
            XACH= ACV*VOL(ICOMP)/3600.
            h(1)='Supply the ventilation rate in m^3/s relative.'
            h(2)='to the volume of the receiving zone.'
            call EASKR(XACH,' ','Ventilation rate?',
     &        0.0,'W',10.0,'W',0.0,'vent rate m3s',IERI,2)
            if(IERI.eq.0)then
              ACV=XACH*3600.0/VOL(ICOMP)
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
        if (ACV.gt.0.0) then

C Ask for source zone/ temperature.
          CALL EASKABC(' ','Ventilation air temperature:',
     &      'other modelled zone','constant',
     &      'ambient',IGU,0)
          if (IGU.eq.1) then

C Present zone list.
 38         IV=IPT
            call askzone(IV,IPT,'Ventilation from which zone?',
     &     'Source Zone','-','Ventilation definition requires a zone.',
     &       ier)
            if (IER.ne.0.or.IV.eq.ICOMP) then
              write (OUTS,'(3a)')  'the current zone (',
     &          ZNAME(ICOMP)(1:lnblnk(ZNAME(ICOMP))),').'
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
            IPT=IV
            TA=0.
          elseif (IGU.eq.2) then

C Ask for source temperature.
 40         XTA=  TA
            h(1)='Supply the ventilation air temperature in degC.'
            call EASKR(XTA,' ','Supply temperature?',
     &        0.0,'F',50.0,'W',0.5,'vent src temp',IERI,1)
            if(IERI.eq.0)then
              IPT=0
              TA=XTA
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 40
            endif
          elseif (IGU.eq.3) then

C Set vent to zero and increase infiltration.
            call USRMSG('Supply of outside air being treated as',
     &       'infiltration. Updating infiltration flow rate.','W')
            ACI=ACI+ACV
            ACV=0
            IPT=0
            TA=0.0
          endif
        endif
      endif

C If day type 4 then add data to all day types.
      if (IDT.eq.4) then
        ILOOP=3
      else
        ILOOP=1
      endif

C If new period then work out where to insert data.
      do 400 ILDT=1,ILOOP

C If looping through day types then set day type (IDT) to current 
C value of loop counter (ILDT).  Otherwise IDT will have the value 
C that was passed to the subroutine.
        if (ILOOP.eq.3) IDT=ILDT
        call ADDAFP(IPER,IDT,IACS,IACF,ACI,ACV,IPT,TA)
 400  continue

      return
      end

C ************************ ADDAFP ************************
C ADDAFP add air flow period in correct location (time based).
C Other routines assume that periods have been added in the order 
C resulting from the sorting done here.
C IPER=0 if its a new period
C IDT=day type wkday=1, sat=2, sun=3
C IACS,IACF period start and finish time
C ACI,ACV infiltration and ventilation rates
C IPT,TA ventilation source zone, if IPT=0 then TA is source temperature

      subroutine ADDAFP(IPER,IDT,IACS,IACF,ACI,ACV,IPT,TA)
#include "building.h"

      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)

      if (IPER.eq.0) then
        if (IDT.eq.1) then
          ILPER=1
          do 210 I=1,NAC1
            if (iabs(IACS1(I)).le.iabs(IACS)) then
              ILPER=I
              if (IACS1(I).lt.IACS) then
                ILPER=I+1
              elseif (IACS1(I).eq.IACS.and.IACF1(I).gt.IACF) then
                ILPER=I
              endif
            endif
 210      continue
          NAC1=NAC1+1
          INC=NAC1
        elseif (IDT.eq.2) then
          ILPER=1
          do 220 I=1,NAC2
            if (iabs(IACS2(I)).le.iabs(IACS)) then
              ILPER=I
              if (IACS2(I).lt.IACS) then
                ILPER=I+1
              elseif (IACS2(I).eq.IACS.and.IACF2(I).gt.IACF) then
                ILPER=I
              endif
            endif
 220      continue
          NAC2=NAC2+1
          INC=NAC2
        elseif (IDT.eq.3) then
          ILPER=1
          do 230 I=1,NAC3
            if (iabs(IACS3(I)).le.iabs(IACS)) then
              ILPER=I
              if (IACS3(I).lt.IACS) then
                ILPER=I+1
              elseif (IACS3(I).eq.IACS.and.IACF3(I).gt.IACF) then
                ILPER=I
              endif
            endif
 230      continue
          NAC3=NAC3+1
          INC=NAC3
        endif
      else
        ILPER=IPER
        INC=IPER
      endif

C Now update common blocks.
      do 300 IX=INC,ILPER,-1
        if (IX.eq.ILPER) then
          if(IDT.eq.1) then
            IACS1(IX)=IACS
            IACF1(IX)=IACF
            ACI1(IX)=ACI
            ACV1(IX)=ACV
            IPT1(IX)=IPT
            TA1(IX)=TA
          endif
          if(IDT.eq.2) then
            IACS2(IX)=IACS
            IACF2(IX)=IACF
            ACI2(IX)=ACI
            ACV2(IX)=ACV
            IPT2(IX)=IPT
            TA2(IX)=TA
          endif
          if(IDT.eq.3) then
            IACS3(IX)=IACS
            IACF3(IX)=IACF
            ACI3(IX)=ACI
            ACV3(IX)=ACV
            IPT3(IX)=IPT
            TA3(IX)=TA
          endif
        else
          if(IDT.eq.1) then
            IACS1(IX)=IACS1(IX-1)
            IACF1(IX)=IACF1(IX-1)
            ACI1(IX)=ACI1(IX-1)
            ACV1(IX)=ACV1(IX-1)
            IPT1(IX)=IPT1(IX-1)
            TA1(IX)=TA1(IX-1)
          endif
          if(IDT.eq.2) then
            IACS2(IX)=IACS2(IX-1)
            IACF2(IX)=IACF2(IX-1)
            ACI2(IX)=ACI2(IX-1)
            ACV2(IX)=ACV2(IX-1)
            IPT2(IX)=IPT2(IX-1)
            TA2(IX)=TA2(IX-1)
          endif
          if(IDT.eq.3) then
            IACS3(IX)=IACS3(IX-1)
            IACF3(IX)=IACF3(IX-1)
            ACI3(IX)=ACI3(IX-1)
            ACV3(IX)=ACV3(IX-1)
            IPT3(IX)=IPT3(IX-1)
            TA3(IX)=TA3(IX-1)
          endif
        endif
 300  continue

      return
      end

C ********************* CDAIRFL *********************
C CDAIRFL - copy/ delete air flow data.
C  ICD - copy (=1) delete (=2) flag

      subroutine CDAIRFL(ICD)
#include "building.h"

      common/pophelp/h(60)

      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)

      character zname*12,zdesc*64,h*72,VERT(3*MA)*35

      dimension IVERT(3*MA),ITEMP(3*MA)

C Set up menu of current definitions.
      ILEN=NAC1+NAC2+NAC3

C Loop through the items until the page to be displayed.
C Build up text strings for the menu. 

C Weekday loop.
      M=0
      DO 10 L=1,NAC1
        M=M+1
        if (IPT1(L).gt.0) then
          write (VERT(M),303) IACS1(L),IACF1(L),ACI1(L),
     &      ACV1(L),zname(IPT1(L))(1:9)
        else
          if (ACV1(L).lt.0.0001) then 
            write (VERT(M),323) IACS1(L),IACF1(L),ACI1(L),ACV1(L)
          else
            write (VERT(M),313) IACS1(L),IACF1(L),ACI1(L),ACV1(L),TA1(L)
          endif
        endif
  303   FORMAT('Wkd ',2i4,1x,2f6.2,1x,a)
  313   FORMAT('Wkd ',2i4,1x,2f6.2,1x,f6.1)
  323   FORMAT('Wkd ',2i4,1x,2f6.2,1x,'  n/a')
 10   continue

C Saturday loop.
      DO 20 L=1,NAC2
        M=M+1
        if (IPT2(L).gt.0) then
          write (VERT(M),403) IACS2(L),IACF2(L),ACI2(L),
     &      ACV2(L),zname(IPT2(L))(1:9)
        else
          if (ACV2(L).lt.0.0001) then 
            write (VERT(M),423) IACS2(L),IACF2(L),ACI2(L),ACV2(L)
          else
            write (VERT(M),413) IACS2(L),IACF2(L),ACI2(L),ACV2(L),TA2(L)
          endif
        endif
  403   FORMAT('Sat ',2i4,1x,2f6.2,1x,a)
  413   FORMAT('Sat ',2i4,1x,2f6.2,1x,f6.1)
  423   FORMAT('Sat ',2i4,1x,2f6.2,1x,'  n/a')
 20   continue

C Sunday loop.
      DO 30 L=1,NAC3
        M=M+1
        if (IPT3(L).gt.0) then
          write (VERT(M),503) IACS3(L),IACF3(L),ACI3(L),
     &      ACV3(L),zname(IPT3(L))(1:9)
        else
          if (ACV3(L).lt.0.0001) then 
            write (VERT(M),523) IACS3(L),IACF3(L),ACI3(L),ACV3(L)
          else
            write (VERT(M),513) IACS3(L),IACF3(L),ACI3(L),ACV3(L),TA3(L)
          endif
        endif
  503   FORMAT('Sun ',2i4,1x,2f6.2,1x,a)
  513   FORMAT('Sun ',2i4,1x,2f6.2,1x,f6.1)
  523   FORMAT('Sun ',2i4,1x,2f6.2,1x,'  n/a')
 30   continue

C To delete period(s) redisplay the menu and allow user to indicate
C which one, then collapse the period list.
      if (ICD.eq.1) then
        H(1)='One or more of the periods may be deleted.'
        call EPICKS(ILEN,IVERT,' ','Air flow period(s) to delete:',
     &         35,M,VERT,'Air flow period to delete',IER,1)

C Delete items in IVERT array. Sort IVERT first.
        if (ILEN.gt.0) then
          KFLAG=-1
          call SORTI(IVERT,ITEMP,3*MA,KFLAG)
          do 100 IJ=1,ILEN
            IAFP=IVERT(IJ)
            if (IAFP.gt.(NAC1+NAC2)) then

C Sunday air flow selected.
              IAFP=IAFP-NAC1-NAC2
              do 110 IX=IAFP,NAC3
                if (IX.ne.MA) then
                  IACS3(IX)=IACS3(IX+1)
                  IACF3(IX)=IACF3(IX+1)
                  ACI3(IX)=ACI3(IX+1)
                  ACV3(IX)=ACV3(IX+1)
                  IPT3(IX)=IPT3(IX+1)
                  TA3(IX)=TA3(IX+1)
                endif
 110          continue
              NAC3=NAC3-1
            elseif (IAFP.gt.NAC1) then

C Saturday casual gain selected.
              IAFP=IAFP-NAC1
              do 120 IX=IAFP,NAC2
                if (IX.ne.MA) then
                  IACS2(IX)=IACS2(IX+1)
                  IACF2(IX)=IACF2(IX+1)
                  ACI2(IX)=ACI2(IX+1)
                  ACV2(IX)=ACV2(IX+1)
                  IPT2(IX)=IPT2(IX+1)
                  TA2(IX)=TA2(IX+1)
                endif
 120          continue
              NAC2=NAC2-1
            else

C Weekday casual gain selected.
              do 130 IX=IAFP,NAC1
                if (IX.ne.MA) then
                  IACS1(IX)=IACS1(IX+1)
                  IACF1(IX)=IACF1(IX+1)
                  ACI1(IX)=ACI1(IX+1)
                  ACV1(IX)=ACV1(IX+1)
                  IPT1(IX)=IPT1(IX+1)
                  TA1(IX)=TA1(IX+1)
                endif
 130          continue
              NAC1=NAC1-1
            endif
            M=M-1
 100      continue
        endif
      else

C Copy periods.
        h(1)='All the casual gains associated with a specified day type'
        h(2)='can be copied to another day type.'
        call EASKABCD(' ','Copy casual gains:','Weekdays->Saturdays',
     &     'Weekdays->Sundays','Saturdays->Sundays',
     &     'cancel',IGU,2)
        if (IGU.eq.1) then
          do 210 IX=1,NAC1
            if (NAC2.lt.MA) then
              IPER=0
              IDT=2
              IACS=IACS1(IX)
              IACF=IACF1(IX)
              ACI=ACI1(IX)
              ACV=ACV1(IX)
              IPT=IPT1(IX)
              TA=TA1(IX)
              call ADDAFP(IPER,IDT,IACS,IACF,ACI,ACV,IPT,TA)
              call CHKAIRFL
            endif
 210      continue
        elseif (IGU.eq.2) then
          do 220 IX=1,NAC1
            if (NAC3.lt.MA) then
              IPER=0
              IDT=3
              IACS=IACS1(IX)
              IACF=IACF1(IX)
              ACI=ACI1(IX)
              ACV=ACV1(IX)
              IPT=IPT1(IX)
              TA=TA1(IX)
              call ADDAFP(IPER,IDT,IACS,IACF,ACI,ACV,IPT,TA)
              call CHKAIRFL
            endif
 220      continue
        elseif (IGU.eq.3) then
          do 230 IX=1,NAC2
            if (NAC3.lt.MA) then
              IPER=0
              IDT=3
              IACS=IACS2(IX)
              IACF=IACF2(IX)
              ACI=ACI2(IX)
              ACV=ACV2(IX)
              IPT=IPT2(IX)
              TA=TA2(IX)
              call ADDAFP(IPER,IDT,IACS,IACF,ACI,ACV,IPT,TA)
              call CHKAIRFL
            endif
 230      continue
        endif
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

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C LPROJ   - zone scheduling (operation file)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/PREC2/VOL(MCOM)

      COMMON/precz/zname(MCOM),zdesc(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver
      CHARACTER oprdesc*248,ctlstr*24
      COMMON/P1/oprdesc,ctlstr
      COMMON/P1C/oprdescc,ctlstrc
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Scratch copy of air flow pattern from an operations file.
      COMMON/P2C/NAC1C,IACS1C(MA),IACF1C(MA),ACI1C(MA),ACV1C(MA),
     &   IPT1C(MA),TA1C(MA),NAC2C,IACS2C(MA),IACF2C(MA),ACI2C(MA),
     &   ACV2C(MA),IPT2C(MA),TA2C(MA),NAC3C,IACS3C(MA),IACF3C(MA),
     &   ACI3C(MA),ACV3C(MA),IPT3C(MA),TA3C(MA)

      COMMON/P2CTLC/ITCTLC,TLOC,TUPC,THIC,ACILC,ACVLC,IVLC,TALC,
     &   ACIUC,ACVUC,IVUC,TAUC,ACIHC,ACVHC,IVHC,TAHC

      character*(*) sfile
      character zname*12,zdesc*64,h*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER oprdescc*248,ctlstrc*24
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,ltmp*72
      character fs*1,GFILE*72,afact*8,iflow*8,vflow*8
      character act*1   ! s for silent, - for normal
      LOGICAL XST,unixok

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
      NAC1=0
      NAC2=0
      NAC3=0
      ctlstr='no control of air flow  '
      oprdesc='Flow schedules imported. No other notes (yet).'

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

C Use ERZSCHED to fill P2C and P2CTLC common blocks and
C then jump to 32 to read in the destination operations file.
          CALL ERZSCHED(IUO,sfile,IER)
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
C for weekdays, saturdays, sundays.
      NAC1C=NAC1
      if(NAC1.gt.0)then
        do 10 L=1,NAC1
          IACS1C(L)=IACS1(L)
          IACF1C(L)=IACF1(L)
          ACI1C(L)=ACI1(L)
          ACV1C(L)=ACV1(L)
          IPT1C(L)=IPT1(L)
          TA1C(L)=TA1(L)
  10    continue
      endif
      NAC2C=NAC2
      if(NAC2.gt.0)then
        do 20 L=1,NAC2
          IACS2C(L)=IACS2(L)
          IACF2C(L)=IACF2(L)
          ACI2C(L)=ACI2(L)
          ACV2C(L)=ACV2(L)
          IPT2C(L)=IPT2(L)
          TA2C(L)=TA2(L)
  20    continue
      endif
      NAC3C=NAC3
      if(NAC3.gt.0)then
        do 30 L=1,NAC3
          IACS3C(L)=IACS3(L)
          IACF3C(L)=IACF3(L)
          ACI3C(L)=ACI3(L)
          ACV3C(L)=ACV3(L)
          IPT3C(L)=IPT3(L)
          TA3C(L)=TA3(L)
  30    continue
      endif

C Copy air flow control into local variables.

      ctlstrc=ctlstr
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
          WRITE(GFILE,'(2a)') 
     &      zname(idest)(1:lnblnk(zname(idest))),'.opr'
        else
          WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(idest)(1:lnblnk(zname(idest))),'.opr'
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
        ip3ver=1
      endif

C Copy the local array data into the common blocks overwriting
C the data implied by 'afact'. Also act on the iflow vflow directives.
      if(isource.eq.0)then
        VAL=10.0
        h(1)='Because this is a pattern file there is no associated'
        h(2)='zone and to do a ratio we need an assumed volume. '
        CALL EASKR(VAL,' ','Assumed volume of source?',
     &      0.10,'F',9999.0,'W',0.0,'assumed volume',IER,2)
      endif
      if(iflow(1:7).eq.'ach-ach')then
        ratioif=1.0
      elseif(iflow(1:7).eq.'vol-vol')then
        if(isource.eq.0)then
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
        if(NAC1C.gt.0)then
          do 11 L=1,NAC1C
            NAC1=NAC1C
            IACS1(L)=IACS1C(L)
            IACF1(L)=IACF1C(L)
            ACI1(L)=ACI1C(L)/ratioif
            ACV1(L)=ACV1C(L)/ratiovf
            IPT1(L)=IPT1C(L)
            TA1(L)=TA1C(L)
  11      continue
        endif
        if(NAC2C.gt.0)then
          do 21 L=1,NAC2C
            NAC2=NAC2C
            IACS2(L)=IACS2C(L)
            IACF2(L)=IACF2C(L)
            ACI2(L)=ACI2C(L)/ratioif
            ACV2(L)=ACV2C(L)/ratiovf
            IPT2(L)=IPT2C(L)
            TA2(L)=TA2C(L)
  21      continue
        endif
        if(NAC3C.gt.0)then
          do 31 L=1,NAC3C
            NAC3=NAC3C
            IACS3(L)=IACS3C(L)
            IACF3(L)=IACF3C(L)
            ACI3(L)=ACI3C(L)/ratioif
            ACV3(L)=ACV3C(L)/ratiovf
            IPT3(L)=IPT3C(L)
            TA3(L)=TA3C(L)
  31      continue
        endif
        if(afact(1:5).eq.'all  ')then

C Apply ach or vol conversion to controls as well.
          ctlstr=ctlstrc
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
      CALL EMKOPER(IUO,LPROJ(idest),idest,iuout,IER)
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

      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)

      logical RECHECK

 5    RECHECK=.false.
      Ifrom=0
      Ito=0
      Inew=0

C Check all daytypes.
C I is the earlier of the two periods being checked.
C J is the later of the two periods.
C If periods I and J overlap (start time for J within period I) then
C copy data for all periods up one slot (if room) and insert new period.
C Therefore the data in period J becomes J+1 and the new period J is the 
C overlapping period of the two initial periods.
C Weekdays.
      do 10 I=1,NAC1
        if (I+1.le.NAC1) then
        do 15 J=I+1,NAC1
          if (IACS1(J).ge.IACS1(I).and.IACS1(J).lt.IACF1(I)) then

C Start time of period J is between start and finish times of period I.
C Create a new period by splitting period J, only if start or finish time 
C of overlapping periods in not equal.
C     |-------------------------|
C     |                         |
C     ||.or.|-----------| ..or.||.or.| 
C     ||    |           |      ||    |
C  -----------------------------

            if (IACS1(J).eq.IACS1(I).and.IACF1(J).gt.IACF1(I)) then
C
C     |----I------------|
C     |                 |
C     ||---J------- ----|--| J
C     ||                |  |
C  -----------------------------
              Ifrom=J
              Ito=I
              Inew=I
              IACS1(J)=IACF1(I)
            elseif (IACS1(J).eq.IACS1(I).and.IACF1(J).lt.IACF1(I)) then
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
              IACS1(I)=IACF1(J)
              IACS1tmp=IACS1(I)
              IACF1tmp=IACF1(I)
              ACI1tmp=ACI1(I)
              ACV1tmp=ACV1(I)
              IPT1tmp=IPT1(I)
              TA1tmp=TA1(I)
              IACS1(I)=IACS1(J)
              IACF1(I)=IACF1(J)
              ACI1(I)=ACI1(J)
              ACV1(I)=ACV1(J)
              IPT1(I)=IPT1(J)
              TA1(I)=TA1(J)
              IACS1(J)=IACS1tmp
              IACF1(J)=IACF1tmp
              ACI1(J)=ACI1tmp
              ACV1(J)=ACV1tmp
              IPT1(J)=IPT1tmp
              TA1(J)=TA1tmp
            elseif (IACF1(J).eq.IACF1(I).and.IACS1(J).ne.IACS1(I)) then

C     |---I------------|
C     |                |
C     |   |---J-------||
C     |.  |           ||
C  -----------------------------
C Finishes are the same and starts are different.
              Ifrom=I
              Ito=J
              Inew=J
              IACF1(I)=IACS1(J)
            elseif (IACS1(J).gt.IACS1(I).and.IACF1(J).lt.IACF1(I)) then

C     |---I------------|
C     |                |
C     |...|---J-----|  |
C     |.  |         |  |
C  -----------------------------
C Later period starts after earlier period and later period finishes before
C the earlier period.
              if (NAC1.lt.MA) then
                Ifrom=I
                Ito=J
                Inew=J
                do 16 K=NAC1,J,-1
                  IACS1(K+1)=IACS1(K)
                  IACF1(K+1)=IACF1(K)
                  ACI1(K+1)=ACI1(K)
                  ACV1(K+1)=ACV1(K)
                  IPT1(K+1)=IPT1(K)
                  TA1(K+1)=TA1(K)
 16             continue
                NAC1=NAC1+1
                IACF1(J+1)=IACF1(I)
                IACF1(I)=IACS1(J)
                IACS1(J+1)=IACF1(J)
                ACI1(J+1)=ACI1(I)
                ACV1(J+1)=ACV1(I)
                IPT1(J+1)=IPT1(I)
                TA1(J+1)=TA1(I)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined. ','W')
              endif
            elseif (IACS1(J).gt.IACS1(I).and.IACF1(J).gt.IACF1(I)) then

C     |---I------------|
C     |                |
C     |...|---J--------|--|
C     |.  |            |  |
C  -----------------------------
C If later period starts after earlier period and latter period finishes
C after the earlier period.
              if (NAC1.lt.MA) then
                Ifrom=J+1
                Ito=I
                Inew=J
                do 17 K=NAC1,J,-1
                  IACS1(K+1)=IACS1(K)
                  IACF1(K+1)=IACF1(K)
                  ACI1(K+1)=ACI1(K)
                  ACV1(K+1)=ACV1(K)
                  IPT1(K+1)=IPT1(K)
                  TA1(K+1)=TA1(K)
 17             continue
                NAC1=NAC1+1
                IACF1(J)=IACF1(I)
                IACF1(I)=IACS1(J)
                IACS1(J+1)=IACF1(J)
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
              ACI1(I)=ACI1(I)+ACI1(J)
              ACV1(I)=ACV1(I)+ACV1(J)
              do 18 K=I,NAC1
                if ((K+1).gt.MA) then
                  IACS1(K)=0
                  IACF1(K)=0
                  ACI1(K)=0.
                  ACV1(K)=0.
                  IPT1(K)=0
                  TA1(K)=0.
                else
                  IACS1(K)=IACS1(K+1)
                  IACF1(K)=IACF1(K+1)
                  ACI1(K)=ACI1(K+1)
                  ACV1(K)=ACV1(K+1)
                  IPT1(K)=IPT1(K+1)
                  TA1(K)=TA1(K+1)
                endif
 18           continue
              NAC1=NAC1-1
            endif

C Try and set data in new period.
            if (Ifrom.gt.0) then
              ACI1(Inew)=ACI1(Ifrom)+ACI1(Ito)
              ACV1(Inew)=ACV1(Ifrom)+ACV1(Ito)
              if (ACV1(Ifrom).gt.0.0.and.ACV1(Ito).lt.0.0001) then

C Ventilation defined for period Ifrom but not for period Ito, use defn from Ifrom
                IPT1(Inew)=IPT1(Ifrom)
                TA1(Inew)=TA1(Ifrom)
              elseif (ACV1(Ito).gt.0.0.and.ACV1(Ifrom).lt.0.0001) then

C Ventilation defined for period I but not for period Ifrom, use defn from Ito
                IPT1(Inew)=IPT1(Ito)
                TA1(Inew)=TA1(Ito)
              elseif (ACV1(Ito).gt.0.0.and.ACV1(Ifrom).gt.0.0) then

C Two ventilation sources defined, default to use defn from Ifrom
                IPT1(Inew)=IPT1(Ifrom)
                TA1(Inew)=TA1(Ifrom)
                if (IPT1(Ito).ne.IPT1(Ifrom)) then
                call USRMSG('Could not resolve ventilation source for',
     &                 'overlapping period, please check sources.','W')
                endif
              else

C No vent defined.
                IPT1(Inew)=0
                TA1(Inew)=0.
              endif

C Jump out of loop and start checking again.
              RECHECK=.true.
              goto 10
            endif
          endif
 15     continue
        endif
 10   continue

C Saturdays (for comments see weekdays, above).
      do 20 I=1,NAC2
        if (I+1.le.NAC2) then
        do 25 J=I+1,NAC2
          if (IACS2(J).ge.IACS2(I).and.IACS2(J).lt.IACF2(I)) then
            if (IACS2(J).eq.IACS2(I).and.IACF2(J).gt.IACF2(I)) then
              Ifrom=J
              Ito=I
              Inew=I
              IACS2(J)=IACF2(I)
            elseif (IACS2(J).eq.IACS2(I).and.IACF2(J).lt.IACF2(I)) then
              Ifrom=J
              Ito=I
              Inew=I

C By starting period I at finish of J periods will be out of order,
C so swap period data from I to J and J to I.
              IACS2(I)=IACF2(J)
              IACS2tmp=IACS2(I)
              IACF2tmp=IACF2(I)
              ACI2tmp=ACI2(I)
              ACV2tmp=ACV2(I)
              IPT2tmp=IPT2(I)
              TA2tmp=TA2(I)
              IACS2(I)=IACS2(J)
              IACF2(I)=IACF2(J)
              ACI2(I)=ACI2(J)
              ACV2(I)=ACV2(J)
              IPT2(I)=IPT2(J)
              TA2(I)=TA2(J)
              IACS2(J)=IACS2tmp
              IACF2(J)=IACF2tmp
              ACI2(J)=ACI2tmp
              ACV2(J)=ACV2tmp
              IPT2(J)=IPT2tmp
              TA2(J)=TA2tmp
            elseif (IACF2(J).eq.IACF2(I).and.IACS2(J).ne.IACS2(I)) then
              Ifrom=I
              Ito=J
              Inew=J
              IACF2(I)=IACS2(J)
            elseif (IACS2(J).gt.IACS2(I).and.IACF2(J).lt.IACF2(I)) then
              if (NAC2.lt.MA) then
                Ifrom=I
                Ito=J
                Inew=J
                do 26 K=NAC2,J,-1
                  IACS2(K+1)=IACS2(K)
                  IACF2(K+1)=IACF2(K)
                  ACI2(K+1)=ACI2(K)
                  ACV2(K+1)=ACV2(K)
                  IPT2(K+1)=IPT2(K)
                  TA2(K+1)=TA2(K)
 26             continue
                NAC2=NAC2+1
                IACF2(J+1)=IACF2(I)
                IACF2(I)=IACS2(J)
                IACS2(J+1)=IACF2(J)
                ACI2(J+1)=ACI2(I)
                ACV2(J+1)=ACV2(I)
                IPT2(J+1)=IPT2(I)
                TA2(J+1)=TA2(I)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined.','W')
              endif
            elseif (IACS2(J).gt.IACS2(I).and.IACF2(J).gt.IACF2(I)) then
              if (NAC2.lt.MA) then
                Ifrom=J+1
                Ito=I
                Inew=J
                do 27 K=NAC2,J,-1
                  IACS2(K+1)=IACS2(K)
                  IACF2(K+1)=IACF2(K)
                  ACI2(K+1)=ACI2(K)
                  ACV2(K+1)=ACV2(K)
                  IPT2(K+1)=IPT2(K)
                  TA2(K+1)=TA2(K)
 27             continue
                NAC2=NAC2+1
                IACF2(J)=IACF2(I)
                IACF2(I)=IACS2(J)
                IACS2(J+1)=IACF2(J)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined.','W')
              endif
            else
              call USRMSG(
     &      'New air flow period defined for same period as existing',
     &      'period. Combining data, please check/edit existing.','W')
              Ifrom=0
              ACI2(J)=ACI2(I)+ACI2(J)
              ACV2(J)=ACV2(I)+ACV2(J)
              IPT2(J)=IPT2(I)
              TA2(J)=TA2(I)
              do 28 K=I,NAC2
                if ((K+1).gt.MA) then
                  IACS2(K)=0
                  IACF2(K)=0
                  ACI2(K)=0.
                  ACV2(K)=0.
                  IPT2(K)=0
                  TA2(K)=0.
                else
                  IACS2(K)=IACS2(K+1)
                  IACF2(K)=IACF2(K+1)
                  ACI2(K)=ACI2(K+1)
                  ACV2(K)=ACV2(K+1)
                  IPT2(K)=IPT2(K+1)
                  TA2(K)=TA2(K+1)
                endif
 28           continue
              NAC2=NAC2-1
            endif
            if (Ifrom.gt.0) then
              ACI2(Inew)=ACI2(Ifrom)+ACI2(Ito)
              ACV2(Inew)=ACV2(Ifrom)+ACV2(Ito)
              if (ACV2(Ifrom).gt.0.0.and.ACV2(Ito).lt.0.0002) then
                IPT2(Inew)=IPT2(Ifrom)
                TA2(Inew)=TA2(Ifrom)
              elseif (ACV2(Ito).gt.0.0.and.ACV2(Ifrom).lt.0.0002) then
                IPT2(Inew)=IPT2(Ito)
                TA2(Inew)=TA2(Ito)
              elseif (ACV2(Ito).gt.0.0.and.ACV2(Ifrom).gt.0.0) then
                IPT2(Inew)=IPT2(Ifrom)
                TA2(Inew)=TA2(Ifrom)
                if (IPT2(Ito).ne.IPT2(Ifrom)) then
                call USRMSG('Could not resolve ventilation source for',
     &                 'overlapping period; please check sources.','W')
                endif
              else
                IPT2(Inew)=0
                TA2(Inew)=0.
              endif
              RECHECK=.true.
              goto 20
            endif
          endif
 25     continue
        endif
 20   continue

C Sundays (for comments see weekdays, above).
      do 30 I=1,NAC3
        if (I+1.le.NAC3) then
        do 35 J=I+1,NAC3
          if (IACS3(J).ge.IACS3(I).and.IACS3(J).lt.IACF3(I)) then
            if (IACS3(J).eq.IACS3(I).and.IACF3(J).gt.IACF3(I)) then
              Ifrom=J
              Ito=I
              Inew=I
              IACS3(J)=IACF3(I)
            elseif (IACS3(J).eq.IACS3(I).and.IACF3(J).lt.IACF3(I)) then
              Ifrom=J
              Ito=I
              Inew=I

C By starting period I at finish of J periods will be out of order,
C so swap period data from I to J and J to I.
              IACS3(I)=IACF3(J)
              IACS3tmp=IACS3(I)
              IACF3tmp=IACF3(I)
              ACI3tmp=ACI3(I)
              ACV3tmp=ACV3(I)
              IPT3tmp=IPT3(I)
              TA3tmp=TA3(I)
              IACS3(I)=IACS3(J)
              IACF3(I)=IACF3(J)
              ACI3(I)=ACI3(J)
              ACV3(I)=ACV3(J)
              IPT3(I)=IPT3(J)
              TA3(I)=TA3(J)
              IACS3(J)=IACS3tmp
              IACF3(J)=IACF3tmp
              ACI3(J)=ACI3tmp
              ACV3(J)=ACV3tmp
              IPT3(J)=IPT3tmp
              TA3(J)=TA3tmp
            elseif (IACF3(J).eq.IACF3(I).and.IACS3(J).ne.IACS3(I)) then
              Ifrom=I
              Ito=J
              Inew=J
              IACF3(I)=IACS3(J)
            elseif (IACS3(J).gt.IACS3(I).and.IACF3(J).lt.IACF3(I)) then
              if (NAC3.lt.MA) then
                Ifrom=I
                Ito=J
                Inew=J
                do 36 K=NAC3,J,-1
                  IACS3(K+1)=IACS3(K)
                  IACF3(K+1)=IACF3(K)
                  ACI3(K+1)=ACI3(K)
                  ACV3(K+1)=ACV3(K)
                  IPT3(K+1)=IPT3(K)
                  TA3(K+1)=TA3(K)
 36             continue
                NAC3=NAC3+1
                IACF3(J+1)=IACF3(I)
                IACF3(I)=IACS3(J)
                IACS3(J+1)=IACF3(J)
                ACI3(J+1)=ACI3(I)
                ACV3(J+1)=ACV3(I)
                IPT3(J+1)=IPT3(I)
                TA3(J+1)=TA3(I)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined.','W')
              endif
            elseif (IACS3(J).gt.IACS3(I).and.IACF3(J).gt.IACF3(I)) then
              if (NAC3.lt.MA) then
                Ifrom=J+1
                Ito=I
                Inew=J
                do 37 K=NAC3,J,-1
                  IACS3(K+1)=IACS3(K)
                  IACF3(K+1)=IACF3(K)
                  ACI3(K+1)=ACI3(K)
                  ACV3(K+1)=ACV3(K)
                  IPT3(K+1)=IPT3(K)
                  TA3(K+1)=TA3(K)
 37             continue
                NAC3=NAC3+1
                IACF3(J)=IACF3(I)
                IACF3(I)=IACS3(J)
                IACS3(J+1)=IACF3(J)
              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined.','W')
              endif
            else
              call USRMSG(
     &      'New air flow period defined for same period as existing',
     &      'period. Combining data, please check/edit existing.','W')
              Ifrom=0
              ACI3(J)=ACI3(I)+ACI3(J)
              ACV3(J)=ACV3(I)+ACV3(J)
              IPT3(J)=IPT3(I)
              TA3(J)=TA3(I)
              do 38 K=I,NAC3
                if ((K+1).gt.MA) then
                  IACS3(K)=0
                  IACF3(K)=0
                  ACI3(K)=0.
                  ACV3(K)=0.
                  IPT3(K)=0
                  TA3(K)=0.
                else
                  IACS3(K)=IACS3(K+1)
                  IACF3(K)=IACF3(K+1)
                  ACI3(K)=ACI3(K+1)
                  ACV3(K)=ACV3(K+1)
                  IPT3(K)=IPT3(K+1)
                  TA3(K)=TA3(K+1)
                endif
 38           continue
              NAC3=NAC3-1
            endif
            if (Ifrom.gt.0) then
              ACI3(Inew)=ACI3(Ifrom)+ACI3(Ito)
              ACV3(Inew)=ACV3(Ifrom)+ACV3(Ito)
              if (ACV3(Ifrom).gt.0.0.and.ACV3(Ito).lt.0.0003) then
                IPT3(Inew)=IPT3(Ifrom)
                TA3(Inew)=TA3(Ifrom)
              elseif (ACV3(Ito).gt.0.0.and.ACV3(Ifrom).lt.0.0003) then
                IPT3(Inew)=IPT3(Ito)
                TA3(Inew)=TA3(Ito)
              elseif (ACV3(Ito).gt.0.0.and.ACV3(Ifrom).gt.0.0) then
                IPT3(Inew)=IPT3(Ifrom)
                TA3(Inew)=TA3(Ifrom)
                if (IPT3(Ito).ne.IPT3(Ifrom)) then
                call USRMSG('Could not resolve ventilation source for',
     &                 'overlapping period; please check sources.','W')
                endif
              else
                IPT3(Inew)=0
                TA3(Inew)=0.
              endif
              RECHECK=.true.
              goto 30
            endif
          endif
 35     continue
        endif
 30   continue

C If changes have been made then recheck all definitions.
      if (RECHECK) goto 5

      return
      end

C ********************* PLTAIR *********************
C PLTAIR - plot air flow data.
C  ICOMP zone number

      subroutine PLTAIR(ICOMP)
#include "building.h"

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/PREC2/VOL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)

      character zname*12,zdesc*64,outs*124

      dimension TACI(24),TACV(24)

C Scan each days gains and find maximum flow.
      YMAX=1.
      MXPERS=max(NAC1,NAC2,NAC3)
      do 10 I=1,MXPERS
       YMAX=amax1(YMAX,ACI1(I),ACV1(I),ACI2(I),ACV2(I),ACI3(I),ACV3(I))
 10   continue

C Clear and setup plot area.
      CALL startbuffer()
      CALL win3d(menuchw,10,10,8,3,igl,igr,igt,igb,igw,igwh)

C Calculate plotting box locations.
      ipltwd=int(igw/3)-6
      iglsat=igl+ipltwd+9
      iglsun=igl+2*(ipltwd+9)
      plxmin=0.
      plxmax=24.
      YMAX=YMAX*1.1
      YMIN=0.
      YMAXF=YMAX*VOL(ICOMP)/3600.

C Scale and draw right hand (m^3/s) axis first.
      CALL axiscale(ipltwd,igwh,plxmin,plxmax,YMIN,YMAXF,
     &                                         xsc,ysc,sca,xadd,yadd)
      call vrtaxis(YMIN,YMAXF,igr,igb,igt,yadd,ysc,0,1,'Flow (m^3/s)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igwh,plxmin,plxmax,YMIN,YMAX,
     &                                         xsc,ysc,sca,xadd,yadd)

C Scale and draw Weekday plot.
      CALL horaxis(plxmin,plxmax,igl,igr,igb,xadd,xsc,0,'Time (hrs)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,yadd,ysc,0,0,'Flow (ac/hr)')
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmax,YMIN,0,0)
      CALL etplot(plxmax,YMAX,1,0)
      CALL etplot(plxmin,YMAX,1,0)

C Scale and draw Saturday plot.
      CALL horaxis(plxmin,plxmax,iglsat,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmax,YMIN,0,0)
      CALL etplot(plxmax,YMAX,1,0)
      CALL etplot(plxmin,YMAX,1,0)
      CALL etplot(plxmin,YMIM,1,0)

C Scale and draw Sunday plot.
      CALL horaxis(plxmin,plxmax,iglsun,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmin,YMIN,0,0)
      CALL etplot(plxmin,YMAX,1,0)
      CALL etplot(plxmax,YMAX,1,0)

C Display air flow curves.
C Weekdays.
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      do 50 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 50   continue
      do 60 IP=1,NAC1
        do 65 IHR=IACS1(IP),IACF1(IP)-1
          TACI(IHR+1)=ACI1(IP)
          TACV(IHR+1)=ACV1(IP)
 65     continue
 60   continue
      do 70 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        CALL etplot(XST,TACI(IHR),0,0)
        CALL etplot(XFN,TACI(IHR),1,0)
        if (IHR.ne.24) CALL etplot(XFN,TACI(IHR+1),1,0)
        CALL etplot(XST,TACV(IHR),0,0)
        CALL etplot(XFN,TACV(IHR),-4,0)
        if (IHR.ne.24) CALL etplot(XFN,TACV(IHR+1),-4,0)
 70   continue

C Saturdays.
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      do 150 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 150  continue
      do 160 IP=1,NAC2
        do 165 IHR=IACS2(IP),IACF2(IP)-1
          TACI(IHR+1)=ACI2(IP)
          TACV(IHR+1)=ACV2(IP)
 165    continue
 160  continue
      do 170 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        CALL etplot(XST,TACI(IHR),0,0)
        CALL etplot(XFN,TACI(IHR),1,0)
        if (IHR.ne.24) CALL etplot(XFN,TACI(IHR+1),1,0)
        CALL etplot(XST,TACV(IHR),0,0)
        CALL etplot(XFN,TACV(IHR),-4,0)
        if (IHR.ne.24) CALL etplot(XFN,TACV(IHR+1),-4,0)
 170  continue
      call forceflush()

C Sundays.
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      do 250 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 250  continue
      do 260 IP=1,NAC3
        do 265 IHR=IACS3(IP),IACF3(IP)-1
          TACI(IHR+1)=ACI3(IP)
          TACV(IHR+1)=ACV3(IP)
 265    continue
 260  continue
      do 270 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        CALL etplot(XST,TACI(IHR),0,0)
        CALL etplot(XFN,TACI(IHR),1,0)
        if (IHR.ne.24) CALL etplot(XFN,TACI(IHR+1),1,0)
        CALL etplot(XST,TACV(IHR),0,0)
        CALL etplot(XFN,TACV(IHR),-4,0)
        if (IHR.ne.24) CALL etplot(XFN,TACV(IHR+1),-4,0)
 270  continue

C Title and labels.
      write (outs,'(2a)') 'Scheduled air flows for ',ZNAME(ICOMP)
      CALL viewtext(outs,1,1,1)

      call wstxpt(igl+20,igt-65,'Infiltration')
      call wstxpt(igr-100,igt-65,'Ventilation')
      call pixel2u(igl+120,igt-69,gx,gy)
      CALL etplot(gx,gy,0,0)
      call pixel2u(igl+180,igt-69,gx,gy)
      CALL etplot(gx,gy,1,0)
      call pixel2u(igr-120,igt-69,gx,gy)
      CALL etplot(gx,gy,0,0)
      call pixel2u(igr-180,igt-69,gx,gy)
      CALL etplot(gx,gy,-4,0)

      ioff=(ipltwd/3)
      call wstxpt(igl+ioff,igt-10,'Weekday')
      call wstxpt(iglsat+ioff,igt-10,'Saturday')
      call wstxpt(iglsun+ioff,igt-10,'Sunday')
      call forceflush()

      return
      end


C ********************* EDCAS *********************
C EDCAS: Edits casual gains portion of a zone operations file.
C Returns: ICGS*(),ICGF*(),CMGS*(),CMGL*(),RADC*(),CONC*() common.
C ITRC is the trace level, IER=0 is OK.
      SUBROUTINE EDCAS(ICOMP,modopr,sorted,IER)
#include "building.h"
#include "epara.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/user/browse

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &         RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &         CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &         CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)

C How many of each gain type is there each zone/gaintype/daytype.
      common/loadcnt/loadcount(mcom,7,3),loadm2count(mcom,7,3),
     &               loadmixed(mcom,7,3)
      common/loadall/load24(7,3,2)
      logical load24,browse,ok,silent,dok,XST
      logical sorted,modopr

      CHARACTER vert(35)*33,outs*124,KEY*1,sfile*72,snpfile*72
      character lodlabel*6,typstr*8,H*72
      character zname*12,zdesc*64,title*28

C iwatt (8 char) = 'W-W' for absolute W values preserves value
C   e.g. 123W in source zone is also 123W in destination zone.
C iwatt= 'W-m2-W' preserves implied W/m^2 base area implied by
C   absolute W in source zone in destination zone which has a
C   different base area. e.g. 123W in source zone of 2.3m^2 becomes
C   267W in a 5.0m^2 base zone.
      character act*8,iwatt*8

C If not browsing, take the information from loadcnt
C and decide if the periods need to be updated.
      if(.NOT.browse)then
        if(ip3ver.eq.0)then
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
C be saved as version 1.0.
C          call edisp(iuout,'Before sorting')
          call CASINF(ICOMP,iuout)
          if(ip3ver.eq.0)then
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru,IER)
          else
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru,IER)
          endif
          ip3ver=1
          sorted=.true.
          modopr=.true.
        endif
      endif

C Initial menu entry setup.
      idtyp = 1
 5    MHEAD=5
      MCTL=8
      if(idtyp.eq.1)then
        ILEN=NCAS1
      elseif(idtyp.eq.2)then 
        ILEN=NCAS2
      elseif(idtyp.eq.3)then
        ILEN=NCAS3
      endif
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Plot casual gains.
      if (MMOD.eq.8) call PLTCASGN(ICOMP)

C Set up top of menu.
 44   VERT(1) = '1 import from profiles database '
      VERT(3) = '  ----------------------------- '
      if(idtyp.eq.1)then
        write(VERT(4),'(a,i2,a)')'3 loads >> Weekdays (',NCAS1,')'
      elseif(idtyp.eq.2)then 
        write(VERT(4),'(a,i2,a)')'3 loads >> Saturdays (',NCAS2,')'
      elseif(idtyp.eq.3)then
        write(VERT(4),'(a,i2,a)')'3 loads >> Sundays (',NCAS3,')'
      endif
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

C Weekday loop.
      if(idtyp.eq.1)then
        DO 10 L=1,NCAS1
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(ICGT1(L).gt.0)then
          if (IELF(ICOMP).EQ.1) then
              if (PWR1(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' W  ',PWR1(L),PF1(L)
              else
                write (VERT(M),313)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' W   no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &          lodlabel(icomp,ICGT1(L)),' W  ',CMGS1(L),CMGL1(L)
            endif
  303       FORMAT(a,2i4,1x,2a,2f6.0)
  313       FORMAT(a,2i4,1x,2a)
          elseif(ICGT1(L).eq.-1)then
            jicgt=iabs(ICGT1(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR1(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' m2p',PWR1(L),PF1(L)
              else
                write (VERT(M),313)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' m2p no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &          lodlabel(icomp,jicgt),' m2p',CMGS1(L),CMGL1(L)
            endif
          else
            jicgt=iabs(ICGT1(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR1(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' Wm2',PWR1(L),PF1(L)
              else
                write (VERT(M),313)KEY,ICGS1(L),ICGF1(L),
     &            lodlabel(icomp,ICGT1(L)),' Wm2 no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS1(L),ICGF1(L),
     &          lodlabel(icomp,jicgt),' Wm2',CMGS1(L),CMGL1(L)
            endif
          endif
        ENDIF
 10     continue
      endif

C Saturday loop.
      if(idtyp.eq.2)then
        DO 20 L=1,NCAS2
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(ICGT2(L).gt.0)then
            if (IELF(ICOMP).EQ.1) then
              if (PWR2(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' W  ',PWR2(L),PF2(L)
              else
                write (VERT(M),313)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' W   no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &          lodlabel(icomp,ICGT2(L)),' W  ',CMGS2(L),CMGL2(L)
            endif
          elseif(ICGT2(L).eq.-1)then
            jicgt=iabs(ICGT2(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR2(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' m2p',PWR2(L),PF2(L)
              else
                write (VERT(M),313)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' m2p no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &          lodlabel(icomp,jicgt),' m2p',CMGS2(L),CMGL2(L)
            endif
          else
            jicgt=iabs(ICGT2(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR2(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' Wm2',PWR2(L),PF2(L)
              else
                write (VERT(M),313)KEY,ICGS2(L),ICGF2(L),
     &            lodlabel(icomp,ICGT2(L)),' Wm2 no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS2(L),ICGF2(L),
     &          lodlabel(icomp,jicgt),' Wm2',CMGS2(L),CMGL2(L)
            endif
          endif
        ENDIF
 20     continue
      endif

C Sunday loop. 
      if(idtyp.eq.3)then
        DO 30 L=1,NCAS3
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M-MHEAD,KEY,IER)
          if(ICGT3(L).gt.0)then
            if (IELF(ICOMP).EQ.1) then
              if (PWR3(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' W ',PWR3(L),PF3(L)
              else
                write (VERT(M),313)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' W   no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &          lodlabel(icomp,ICGT3(L)),' W  ',CMGS3(L),CMGL3(L)
            endif
          elseif(ICGT3(L).eq.-1)then
            jicgt=iabs(ICGT3(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR3(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' m2p',PWR3(L),PF3(L)
              else
                write (VERT(M),313)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' m2p no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &          lodlabel(icomp,jicgt),' m2p',CMGS3(L),CMGL3(L)
            endif
          else
            jicgt=iabs(ICGT3(L))
            if (IELF(ICOMP).EQ.1) then
              if (PWR3(L).gt.0.0001) then
                write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' Wm2',PWR3(L),PF3(L)
              else
                write (VERT(M),313)KEY,ICGS3(L),ICGF3(L),
     &            lodlabel(icomp,ICGT3(L)),' Wm2 no power use'
              endif
            else
              write (VERT(M),303)KEY,ICGS3(L),ICGF3(L),
     &          lodlabel(icomp,jicgt),' Wm2',CMGS3(L),CMGL3(L)
            endif
          endif
        ENDIF
 30     continue
      endif

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
      H(12)='factor.  The graphs will continue to show the casual'
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
        write(title,'(a,a)') 'Gains: ',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP)))
      endif
      CALL EMENU(title,VERT,MVERT,IVERT)
      if (IVERT.EQ.1)THEN

C Import profile.
        call EASKABCD('Import a profile for which day type?', 
     &   '  ','Weekdays','Saturdays','Sundays','Cancel',IW,0)
        if (IW.EQ.1) then
          IN=NCAS1
          CALL SCASL(IN,IW,IER,ICOMP)
          sorted=.false.
        elseif (IW.EQ.2) then
          IN=NCAS2
          CALL SCASL(IN,IW,IER,ICOMP)
          sorted=.false.
        elseif (IW.EQ.3) then
          IN=NCAS3
          CALL SCASL(IN,IW,IER,ICOMP)
          sorted=.false.
        endif
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
            do 34 i=1,NCAS1
              PF1(I)=0.0
              IPF1(I)=0
              PWR1(I)=0.0
              BVOLT1(I)=0.0
              IPHAS1(I)=1
 34         continue
            do 35 i=1,NCAS2
              PF2(I)=0.0
              IPF2(I)=0
              PWR2(I)=0.0
              BVOLT2(I)=0.0
              IPHAS2(I)=1
 35         continue
            do 36 i=1,NCAS3
              PF3(I)=0.0
              IPF3(I)=0
              PWR3(I)=0.0
              BVOLT3(I)=0.0
              IPHAS3(I)=1
 36         continue
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
        if(idtyp.gt.3) idtyp = 1
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
        if(idtyp.eq.1)then
          ILLEN=NCAS1
        elseif(idtyp.eq.2)then 
          ILLEN=NCAS2
        elseif(idtyp.eq.3)then
          ILLEN=NCAS3
        endif
        call edisp(iuout,'Before sorting')
        call CASINF(ICOMP,iuout)
        call PROCESSOLDCAS(ICOMP,0,iuout,IER)
        if(ier.ne.0)then
          call edisp(iuout,'Problem while sorting gains.')
        endif
        sorted=.true.
        modopr=.true.

C If different period, update the menu length.
        if(idtyp.eq.1)then
          if(ILLEN.ne.NCAS1)call edisp(iuout,'Periods have changed.')
          ILEN=NCAS1
        elseif(idtyp.eq.2)then 
          if(ILLEN.ne.NCAS2)call edisp(iuout,'Periods have changed.')
          ILEN=NCAS2
        elseif(idtyp.eq.3)then
          if(ILLEN.ne.NCAS3)call edisp(iuout,'Periods have changed.')
          ILEN=NCAS3
        endif
        goto 44
      ELSEIF(IVERT.EQ.(MVERT-4))THEN
        h(1)='You can apply a scaling factor to all entries of a '
        h(2)='particular casual gain type (useful for parametric '
        h(3)='studies on profiles which have been imported).'
        CALL EASKATOG('Apply scaling factor to which casual gain type?',
     &     ' ',lodlabel(icomp,1),lodlabel(icomp,2),lodlabel(icomp,3),
     &     'All gain types','Cancel',' ',' ',ICGTS,3)
        if (ICGTS.ne.5) then
          if (ICGTS.ne.4) then
            write (outs,'(3a)') 'Scale ',lodlabel(icomp,ICGTS),
     &                          ' gain for day type: '
          else
            write (outs,'(a)') 'Scale all gains for day type: '
          endif
          call EASKATOG(outs,'  ','Weekdays','Saturdays',
     &           'Sundays','All day types','Cancel',' ',' ',IW,0)
          if (IW.ne.5) then
            VAL=1.0
            if (ICGTS.ne.4) then
              write (outs,'(2a)') 'Scaling factor for ',
     &                            lodlabel(icomp,ICGTS)
            else
              write (outs,'(a)') 'Scaling factor for all '
            endif
            if (IW.eq.1) then
              write (outs,'(2a)') outs(1:lnblnk(outs)),
     &                            ' gains during weekdays?'
            elseif (IW.eq.2) then
              write (outs,'(2a)') outs(1:lnblnk(outs)),
     &                            ' gains during Saturdays?'
            elseif (IW.eq.3) then
              write (outs,'(2a)') outs(1:lnblnk(outs)),
     &                            ' gains during Sundays?'
            else
              write (outs,'(2a)') outs(1:lnblnk(outs)),
     &                            ' gains during all day types?'
            endif
            CALL EASKR(VAL,outs,
     &             '(typically between 0.1 and 5.0)',
     &             0.1,'W',5.0,'W',1.0,'scaling factor',IER,3)
            if (IW.eq.1.or.IW.eq.4) then
              do 33 i=1,NCAS1
                if(IABS(ICGT1(I)).eq.ICGTS.or.ICGTS.eq.4)then
                  CMGS1(I)=CMGS1(I)*VAL
                  CMGL1(I)=CMGL1(I)*VAL
                endif
 33           continue
            endif
            if (IW.eq.2.or.IW.eq.4) then
              do 43 i=1,NCAS2
                if(IABS(ICGT2(I)).eq.ICGTS.or.ICGTS.eq.4)then
                  CMGS2(I)=CMGS2(I)*VAL
                  CMGL2(I)=CMGL2(I)*VAL
                endif
 43           continue
            endif
            if (IW.eq.3.or.IW.eq.4) then
              do 53 i=1,NCAS3
                if(IABS(ICGT3(I)).eq.ICGTS.or.ICGTS.eq.4)then
                  CMGS3(I)=CMGS3(I)*VAL
                  CMGL3(I)=CMGL3(I)*VAL
                endif
 53           continue
            endif
          endif
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
        H(7) =' copy - copies weekdays -> saturdays or weekdays -> '
        H(8) ='   sundays or saturdays -> sundays. Currently it'
        H(9) ='   adds the copied periods to the desination in'
        H(10)='   addition to existing periods.'
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
        H(21)='    in the source zone is is also 123W in destination'
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
            if(.NOT.browse)call checkcascount(icomp,ier)

            write (outs,'(3a)') 'New ',lodlabel(icomp,ICGTS),
     &                          ' gain for day type: '
            call EASKATOG(outs,'  ','Weekdays','Saturdays',
     &           'Sundays','All day types','Cancel',' ',' ',IW,0)
            if (IW.ne.5) then
              if (IW.eq.1) then
                if(NCAS1.eq.MC)then
                  call usrmsg(' ',
     &                         'Sorry, too many weekday periods.','W')
                else
                  IDT=1
                  IPER=0
                  if(loadcount(icomp,ICGTS,IDT).eq.0)then

C If there are no weekday loads of this type make a default one.
                    silent=.true.
                    izs=0
                    izf=24
                    call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                  endif
                  silent=.false.
                  izs=8
                  izf=18
                  call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                endif
              elseif (IW.eq.2) then
                if(NCAS2.eq.MC)then
                  call usrmsg(' ',
     &                         'Sorry, too many Saturday periods.','W')
                else
                  IDT=2
                  IPER=0
                  if(loadcount(icomp,ICGTS,IDT).eq.0)then

C If there are no saturday loads of this type make a default one.
                    silent=.true.
                    izs=0
                    izf=24
                    call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                  endif
                  silent=.false.
                  izs=8
                  izf=18
                  call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                endif
              elseif (IW.eq.3) then
                if(NCAS3.eq.MC)then
                  call usrmsg(' ','Sorry, too many Sunday periods.','W')
                else
                  IDT=3
                  IPER=0
                  if(loadcount(icomp,ICGTS,IDT).eq.0)then

C If there are no sunday loads of this type make a default one.
                    silent=.true.
                    izs=0
                    izf=24
                    call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                  endif
                  silent=.false.
                  izs=8
                  izf=18
                  call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                endif
              elseif (IW.eq.4) then
                if(NCAS1.eq.MC)then
                  call usrmsg(' ',
     &                         'Sorry, too many weekday periods.','W')
                elseif(NCAS2.eq.MC)then
                  call usrmsg(' ',
     &                         'Sorry, too many Saturday periods.','W')
                elseif(NCAS3.eq.MC)then
                  call usrmsg(' ','Sorry, too many Sunday periods.','W')
                else
                  IDT=4
                  IPER=0
                  silent=.false.
                  izs=0
                  izf=24
                  call EDCASGN(ICOMP,ICGTS,IDT,IPER,silent,izs,izf)
                endif
              endif
            endif
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
     &          ZNAME(ICOMP)(1:lnblnk(ZNAME(ICOMP))),').'
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
          if(idtyp.eq.1)then
            ILEN=NCAS1
          elseif(idtyp.eq.2)then 
            ILEN=NCAS2
          elseif(idtyp.eq.3)then
            ILEN=NCAS3
          endif
          goto 44
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)
        if(idtyp.eq.1)then
          IDT=1
          IPER=I
          ICGTX=ICGT1(IPER)
        elseif(idtyp.eq.2)then 
          IDT=2
          IPER=I
          ICGTX=ICGT2(IPER)
        elseif(idtyp.eq.3)then
          IDT=3
          IPER=I
          ICGTX=ICGT3(IPER)
        endif
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

#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain types.
      common/loadlabel/lodlabel(mcom,7)

      DIMENSION  ICGS(MC),ICGF(MC),CGS(MC),CGL(MC)
      DIMENSION  NO(100),DESC(100),IPRVAL(100)
      CHARACTER*40 DESC
      CHARACTER H*72,hold*40,lodlabel*6
      character*8 ll1,ll2,ll3
      logical OK,DOK

      integer IPPFT,IPPHAST  ! for local editing.

C Labels for loads.
      write(ll1,'(1x,a6,1x)') lodlabel(icomp,1)
      write(ll2,'(1x,a6,1x)') lodlabel(icomp,2)
      write(ll3,'(1x,a6,1x)') lodlabel(icomp,3)

      IER=0
      ICGT=1
      PS=0.
      PL=0.
      RAD=0.5
      CON=0.5
      call efopran(IPRODB,LPRFDB,10,1,IER)
      IF(ier.ne.0)return
      CALL edisp(iuout,' opened profiles db')

      IREC = 1
      READ(IPRODB,REC=IREC)  NITEMS
      DO 50 I = 1,NITEMS
        IREC = IREC + 1
        NO(I) = IREC
        READ(IPRODB,REC=IREC)  NCG
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=180)  DESC(I)
        IREC = IREC + NCG
   50 CONTINUE

C Present a list of profiles to select from.
      H(1)='Any one of the profiles may be selected.'
      IX=1
      CALL EPICKS(IX,IPRVAL,' ',' Profiles in db: ',
     &         40,NITEMS,DESC,'db profiles',IER,1)
      IF(IX.EQ.0)RETURN

      N=IPRVAL(1)
      IREC = NO(N)
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=181)  NCG
      IREC = IREC + 2

C There are NCG gains to be added
      DO 70 I = 1,NCG
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=183)  ICGS(I),ICGF(I),
     &       CGS(I),CGL(I)
        IREC = IREC + 1
70    CONTINUE

C Identify nature of casual gain
      H(1)='Casual gains within a zone can be be designated to be '
      H(2)='of one of three user-defined types so that for example '
      H(3)='occupant gains can be separately tracked from that of '
      H(4)='lighting or equipment.  The types listed can have any'
      H(5)='meaning that the user chooses (type labels can be edited'
      h(6)='in the main operations menu.'
      CALL EASKABC(' ','Type of casual gain: ',ll1,ll2,ll3,ICGT,6)

 100  WRITE(HOLD,'(2f9.0,2f8.2,a)')PS,PL,RAD,CON,'  '

C << this would be a good place to expand the logic so that lighting
C << and small power can be entered in terms of W/m2.
      H(1)='In order to expand the profile into energy values the'
      H(2)='sensible and latent load in Watts equivalent to 100% is'
      h(3)=' required. '
      h(4)=' '
      h(5)='Also supply the radiant and convective fraction for the'
      h(6)='sensible load (these normally should sum to 1.00) '
      CALL EASKS(HOLD,
     &  ' 100% sensible & latent loads, radiant & convective fraction:',
     &        ' ',40,' 1000   0  0.5  0.5  ','profile info',IER,3)
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
          
      if(IDAY .EQ. 1)then

C Weekday gains.
        NCAS1 = NCAS1 + NCG
        DO 140 I = 1,NCG
          ICGS1(I+IN) = ICGS(I)
          ICGF1(I+IN) = ICGF(I)
          CMGS1(I+IN) = CGS(I)*PS/100.0
          CMGL1(I+IN) = CGL(I)*PL/100.0
          RADC1(I+IN) = RAD
          CONC1(I+IN) = CON
          ICGT1(I+IN) = ICGT
          IF(IELF(ICOMP).GT.0) THEN
            PF1(I+IN)=PPF
            IPF1(I+IN)=IPPF
            PWR1(I+IN)=CGS(I)*PE/100.
            BVOLT1(I+IN)=PBVOLT
            IPHAS1(I+IN)=IPPHAS
          ENDIF
140     CONTINUE
      elseif(IDAY .EQ. 2)then

C Saturday gains.
        NCAS2 = NCAS2 + NCG
        DO 141 I = 1,NCG
          ICGS2(I+IN) = ICGS(I)
          ICGF2(I+IN) = ICGF(I)
          CMGS2(I+IN) = CGS(I)*PS/100.0
          CMGL2(I+IN) = CGL(I)*PL/100.0
          RADC2(I+IN) = RAD
          CONC2(I+IN) = CON
          ICGT2(I+IN) = ICGT
          IF(IELF(ICOMP).GT.0) THEN
            PF2(I+IN)=PPF
            IPF2(I+IN)=IPPF
            PWR2(I+IN)=CGS(I)*PE/100.
            BVOLT2(I+IN)=PBVOLT
            IPHAS2(I+IN)=IPPHAS
          ENDIF
141     CONTINUE
      elseif(IDAY .EQ. 3)then

C Sunday gains.
        NCAS3 = NCAS3 + NCG
        DO 142 I = 1,NCG
          ICGS3(I+IN) = ICGS(I)
          ICGF3(I+IN) = ICGF(I)
          CMGS3(I+IN) = CGS(I)*PS/100.0
          CMGL3(I+IN) = CGL(I)*PL/100.0
          RADC3(I+IN) = RAD
          CONC3(I+IN) = CON
          ICGT3(I+IN) = ICGT
          IF(IELF(ICOMP).GT.0) THEN
            PF3(I+IN)=PPF
            IPF3(I+IN)=IPPF
            PWR3(I+IN)=CGS(I)*PE/100.
            BVOLT3(I+IN)=PBVOLT
            IPHAS3(I+IN)=IPPHAS
          ENDIF
142     CONTINUE
      endif

  999 CALL ERPFREE(IPRODB,ISTAT)
      RETURN

180   call edisp(iuout,' Casual gain database read error.')
      IER=1
      goto 999
181   call edisp(iuout,' Casual gain db NCG read error.')
      IER=1
      goto 999
183   call edisp(iuout,' Casual gain db data read error.')
      IER=1
      goto 999

      END


C ********************* EDCASGN *********************
C EDCASGN - edit casual gain data.
C  ICOMP zone number
C  ICGT casual gain type index
C  IDT day type
C  IPER period (=0 then new)
C  silent true if no user interaction requested.
C If silent is true then use icgs & icgf parameters.

      subroutine EDCASGN(ICOMP,ICGT,IDT,IPER,silent,icgs,icgf)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Base area for zones.
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)

      logical OK,DOK,CLOSE,silent

      character OUTS*124,hold*24,h*72

      integer IV,ipft,IPHAST  ! for local editing.
      real pft,pwrt,bvoltt,xfla,zb   ! for local editing.

C If new period then set defaults otherwise get data from common.
      if (IPER.eq.0) then
        CMGS=0.
        CMGL=0.
        RADC=0.5
        CONC=0.5
        PF=0.0
        IPF=0
        PWR=0.0
        BVOLT=0.0
        IPHAS=1
      elseif(IDT.eq.1) then
        call CPYCASIT(icomp,idt,iper,icgt,icgs,icgf,cmgs,cmgl,
     &    radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
      elseif(IDT.eq.2) then
        call CPYCASIT(icomp,idt,iper,icgt,icgs,icgf,cmgs,cmgl,
     &    radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
      elseif(IDT.eq.3) then
        call CPYCASIT(icomp,idt,iper,icgt,icgs,icgf,cmgs,cmgl,
     &    radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
      endif

C If silent jump to insertion logic.
      if(silent) goto 112

C Ask for start and finish times.
 10   write (OUTS,'(A)') 'Start and finish hours for period?'
      write (hold,'(2i5,a)') ICGS,ICGF,'  '
      h(1)='Period during which casual gains applies.'
      CALL EASKS(hold,outs,' ',24,' 0 24 ','cas st-fn hr',IER,1)
      K=0
      CALL EGETWI(hold,K,IVS,0,24,'F','period start',IER)
      CALL EGETWI(hold,K,IVF,IVS+1,24,'F','period end',IER)
      if (IER.ne.0) goto 10
      ICGS=IVS
      ICGF=IVF

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
      h(8)='  zone to calculate the total gain.'
      IGU=1
      CALL EASKABCD('Gain to be specified as:',' ','Watts',
     &     'Watts per square meter','Watts per object',
     &     'Square meters per object',IGU,8)
      if (IGU.eq.1) then

C Gain as W - require gain only.
 20     if (ICGT.lt.-1) then
          write (HOLD,'(2f8.1,a)') CMGS*zb,CMGL*zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGS,CMGL,'  '
        endif
        h(1)='Supply the sensible and latent gain in Watts.'
        call EASKS(HOLD,'Sensible and latent gain (W):',
     &          ' ',24,'0.   0. ','casual gn info',IER,1)
        K=0
        call EGETWR(HOLD,K,XSEN,0.,100000.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLAT,0.,100000.0,'W','latent gain',IER)
        if (IER.ne.0) goto 20
        if (ICGT.lt.0) ICGT=ICGT*(-1)
        CMGS=XSEN
        CMGL=XLAT
      elseif (IGU.eq.2) then

C Gain as W/m^2 - require gain per square meter.
        if (ICGT.gt.0) then
          write (HOLD,'(2f8.1,a)') CMGS/zb,CMGL/zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGS,CMGL,'  '
        endif
 30     h(1)='Supply the sensible and latent gain in Watts/m^2.'
        call EASKS(HOLD,'Sensible and latent gain (W/m^2):',
     &          ' ',24,'0.   0. ','casual gn info',IER,1)
        K=0
        call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLAT,0.,50.0,'W','latent gain',IER)
        if (IER.ne.0) goto 30
        if (ICGT.eq.1) then
          CMGS=XSEN*zb
          CMGL=XLAT*zb
        else
          if (ICGT.gt.1) ICGT=ICGT*(-1)
          CMGS=XSEN
          CMGL=XLAT
        endif
      elseif (IGU.eq.3) then

C Gain as W/object - require no of objects and gain per object. If
C user cancels go back to initial question.
C << an actual cancel option not yet implemented >>
        NOBJ=1
 40     h(1)='The number of objects for the current gain type is '
        h(2)='required. For example the number of people or computers.'
        CALL EASKI(NOBJ,' ','Number of objects (e.g. people)?',
     &    0,'W',100,'W',1,'no. objects',IERI,2)
        if(ieri.eq.0)then
          continue
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 40
        endif

        write (HOLD,'(2f8.1,a)') CMGS/real(NOBJ),CMGL/real(NOBJ),'  '
        h(1)='Supply the sensible and latent gain in Watts.'
        call EASKS(HOLD,'Object sensible and latent gain (W):',
     &          ' ',24,'  0.   0. ','casual gn info',IER,1)
        K=0
        call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLAT,0.,50.0,'W','latent gain',IER)
        if (IER.ne.0) goto 40
        if (ICGT.lt.0) ICGT=ICGT*(-1)
        CMGS=XSEN*real(NOBJ)
        CMGL=XLAT*real(NOBJ)
      elseif (IGU.eq.4) then

C Gain as m^2/object - floor area of an object and gain per object.
C If user cancels go back to initial question.
        NOBJ=1
 50     if (ICGT.eq.-1) then
          FLA=CMGS
        else
          FLA=10.
        endif
        XFLA=FLA
        h(1)='The floor area per object for the current gain type is '
        h(2)='required. For example the area per person.'
        call EASKR(XFLA,'Floor area per object (e.g. people)?','  ',
     &    0.1,'F',100.0,'W',1.0,'area per object',IERI,2)
        if(IERI.eq.0)then
          FLA=XFLA
          if (iabs(ICGT).eq.1) then
            ICGT=-1
            CMGS=FLA
            CMGL=0.   ! Why is CMGL set to zero?
          else
            write (HOLD,'(2f8.1,a)') CMGS/(zb/FLA),
     &                               CMGL/(zb/FLA),'  '
            h(1)='Supply the sensible and latent gain in Watts/object.'
            call EASKS(HOLD,'Sensible and latent gain (W/object):',
     &          ' ',24,'0.   0. ','casual gn info',IER,1)
            K=0
            call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
            call EGETWR(HOLD,K,XLAT,0.,50.0,'W','latent gain',IER)
            if (IER.ne.0) goto 50
            if (ICGT.gt.1) ICGT=ICGT*(-1)
            CMGS=XSEN*zb/FLA
            CMGL=XLAT*zb/FLA
          endif
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 50
        endif
      endif

C Ask for rad/ conv split.
 70   write (HOLD,'(2f7.3,a)') RADC,CONC,'  '
      h(1)='Supply the radiant and convective fraction for'
      h(2)='the sensible load (should be <=1.00) '
      call EASKS(HOLD,' Radiant & convective fraction:',
     &        ' ',24,' 0.5  0.5  ','casual gn info',IER,2)
      K=0
      call EGETWR(HOLD,K,RAD,0.,1.0,'F','rad frac',IER)
      CMAX=1.00-RAD
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
      RADC=RAD
      CONC=CON
      
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
 80       pft=PF
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
            if(ipf.eq.-1)then
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading (*)','Lagging',IOPT,5)
            elseif(ipf.eq.1)then
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
 90       pwrt=PWR
          call EASKR(pwrt,'Real power consumption ?','  ',
     &          0.0,'-',10000.0,'-',CMGS,'power consumption',IER,3)
          if (IER.ne.0) goto 90

          H(1)=' What voltage does the load operate at i.e. 220V'
 100      bvoltt=BVOLT
          call EASKR(bvoltt,' ','Operational voltage?',
     &          0.0,'-',10000.0,'-',220.0,'op voltage',IER,1)
          if (IER.ne.0) goto 100

          H(1)='To which phase of the 3-phase supply is the load '
          H(2)='connected, 1-3 or 4-all three.'
          H(3)=' '
          H(4)='A cancel leaves the electrical data unchanged.'
 110      IPHAST=IPHAS
          call EASKI(IPHAST,'Which phase is load connected to?',
     &      '(see help)',1,'F',4,'F',1,'phase ',IERI,4)
          if(ieri.eq.-3)then
            continue
          elseif (ieri.gt.0)then
            goto 110
          else

C No more cancels so instanciate the data for this period.
            BVOLT=bvoltt
            PWR=PWRT
            IPF=IPFT
            PF=pft
            IPHAS=IPHAST
          endif
        endif
      endif

C If day type 4 then add data to all day types.
  112 if (IDT.eq.4) then
        ILOOP=3
      else
        ILOOP=1
      endif

C If new period then work out where to insert data.
      do 400 ILDT=1,ILOOP

C If looping through day types then set day type (IDT) to current 
C value of loop counter (ILDT).  Otherwise IDT will have the value 
C that was passed to the subroutine.
        if (ILOOP.eq.3) IDT=ILDT
        if (IPER.eq.0) then
          if (IDT.eq.1) then
            ILPER=1
            do 210 I=1,NCAS1
              if (iabs(ICGT1(I)).le.iabs(ICGT)) then
                if (ICGS1(I).lt.ICGS) then
                  ILPER=I+1
                elseif (ICGS1(I).eq.ICGS.and.ICGF1(I).gt.ICGF) then
                  ILPER=I
                endif
              endif
 210        continue
            NCAS1=NCAS1+1
            INC=NCAS1
          elseif (IDT.eq.2) then
            ILPER=1
            do 220 I=1,NCAS2
              if (iabs(ICGT2(I)).le.iabs(ICGT)) then
                if (ICGS2(I).lt.ICGS) then
                  ILPER=I+1
                elseif (ICGS2(I).eq.ICGS.and.ICGF2(I).gt.ICGF) then
                  ILPER=I
                endif
              endif
 220        continue
            NCAS2=NCAS2+1
            INC=NCAS2
          elseif (IDT.eq.3) then
            ILPER=1
            do 230 I=1,NCAS3
              if (iabs(ICGT3(I)).le.iabs(ICGT)) then
                if (ICGS3(I).lt.ICGS) then
                  ILPER=I+1
                elseif (ICGS3(I).eq.ICGS.and.ICGF3(I).gt.ICGF) then
                  ILPER=I
                endif
              endif
 230        continue
            NCAS3=NCAS3+1
            INC=NCAS3
          endif
        else
          ILPER=IPER
          INC=IPER
        endif

C Now update common blocks. Loop backwards from 
        do 300 IX=INC,ILPER,-1
          if (IX.eq.ILPER) then
            if(IDT.eq.1) then
              call CPYCASTI(icomp,idt,ix,icgt,icgs,icgf,cmgs,cmgl,
     &          radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
            endif
            if(IDT.eq.2) then
              call CPYCASTI(icomp,idt,ix,icgt,icgs,icgf,cmgs,cmgl,
     &          radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
            endif
            if(IDT.eq.3) then
              call CPYCASTI(icomp,idt,ix,icgt,icgs,icgf,cmgs,cmgl,
     &          radc,conc,ipf,iphas,pf,pwr,bvolt,ier)
            endif
          else
            if(IDT.eq.1) then
              call CPYCASIJ(icomp,1,IX,IX-1,ier)
            endif
            if(IDT.eq.2) then
              call CPYCASIJ(icomp,2,IX,IX-1,ier)
            endif
            if(IDT.eq.3) then
              call CPYCASIJ(icomp,3,IX,IX-1,ier)
            endif
          endif
 300    continue
 400  continue

      return
      end

C ********************* CDCASGN *********************
C CDCASGN - copy/ delete casual gain data.
C  ICOMP zone number
C  ICD - copy (=1) delete (=2) flag

      subroutine CDCASGN(ICOMP,ICD)
#include "building.h"

      common/pophelp/h(60)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)

      character lodlabel*6,h*72,VERT(3*MC)*35
      dimension IVERT(3*MC),ITEMP(3*MC)

C Set up menu of current definitions.
      ILEN=NCAS1+NCAS2+NCAS3

C Loop through the items until the page to be displayed.
C Build up text strings for the menu. 

C Weekday loop.
      M=0
      DO 10 L=1,NCAS1
        M=M+1
        if(ICGT1(L).gt.0)then
          WRITE(VERT(M),303)ICGS1(L),ICGF1(L),
     &        lodlabel(icomp,ICGT1(L)),' W  ',CMGS1(L),CMGL1(L)
  303     FORMAT('Wkd ',2i4,1x,2a,2f6.0)
        elseif(ICGT1(L).eq.-1)then
          jicgt=iabs(ICGT1(L))
          WRITE(VERT(M),303)ICGS1(L),ICGF1(L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS1(L),CMGL1(L)
        else
          jicgt=iabs(ICGT1(L))
          WRITE(VERT(M),303)ICGS1(L),ICGF1(L),
     &        lodlabel(icomp,jicgt),' Wm2',CMGS1(L),CMGL1(L)
        endif
 10   continue

C Saturday loop.
      DO 20 L=1,NCAS2
        M=M+1
        if(ICGT2(L).gt.0)then
          WRITE(VERT(M),313)ICGS2(L),ICGF2(L),
     &        lodlabel(icomp,ICGT2(L)),' W  ',CMGS2(L),CMGL2(L)
  313     FORMAT('Sat ',2i4,1x,2a,2f6.0)
        elseif(ICGT2(L).eq.-1)then
          jicgt=iabs(ICGT2(L))
          WRITE(VERT(M),313)ICGS2(L),ICGF2(L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS2(L),CMGL2(L)
        else
          jicgt=iabs(ICGT2(L))
          WRITE(VERT(M),313)ICGS2(L),ICGF2(L),
     &        lodlabel(icomp,jicgt),' Wm2',CMGS2(L),CMGL2(L)
        endif
 20   continue

C Sunday loop.
      DO 30 L=1,NCAS3
        M=M+1
        if(ICGT3(L).gt.0)then
          WRITE(VERT(M),323)ICGS3(L),ICGF3(L),
     &        lodlabel(icomp,ICGT3(L)),' W  ',CMGS3(L),CMGL3(L)
  323     FORMAT('Sun ',2i4,1x,2a,2f6.0)
        elseif(ICGT3(L).eq.-1)then
          jicgt=iabs(ICGT3(L))
          WRITE(VERT(M),323)ICGS3(L),ICGF3(L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS3(L),CMGL3(L)
        else
          jicgt=iabs(ICGT3(L))
          WRITE(VERT(M),323)ICGS3(L),ICGF3(L),
     &        lodlabel(icomp,jicgt),' Wm2',CMGS3(L),CMGL3(L)
        endif
 30   continue

C To delete period(s) redisplay the menu and allow user to indicate
C which one, then collapse the period list.
      if (ICD.eq.1) then
        H(1)='One or more of the periods may be deleted (removed).'
        call EPICKS(ILEN,IVERT,' ','Casual gain period(s) to delete:',
     &         35,M,VERT,'Casual gain period to delete',IER,1)

C Delete items in IVERT array. Sort IVERT first.
        if (ILEN.gt.0) then
          KFLAG=-1
          call SORTI(IVERT,ITEMP,3*MC,KFLAG)
          do 100 IJ=1,ILEN
            ICGP=IVERT(IJ)
            if (ICGP.gt.(NCAS1+NCAS2)) then

C Sunday casual gain selected.
              ICGP=ICGP-NCAS1-NCAS2
              idaytype=3
              call insdelcasper(icomp,idaytype,'d',icgp,ier)
            elseif (ICGP.gt.NCAS1) then

C Saturday casual gain selected.
              ICGP=ICGP-NCAS1
              idaytype=2
              call insdelcasper(icomp,idaytype,'d',icgp,ier)
            else

C Weekday casual gain selected. Delet point ICGP.
              idaytype=1
              call insdelcasper(icomp,idaytype,'d',icgp,ier)
            endif
            M=M-1
 100      continue
        endif
      else

C Copy periods.
        h(1)='All the casual gains associated with a specified day type'
        h(2)='can be copied to another day type.'
        call EASKABCD('Copy casual gains:',' ','Weekdays->Saturdays',
     &     'Weekdays->Sundays','Saturdays->Sundays',
     &     'cancel',IGU,2)
        if (IGU.eq.1) then
          do 210 IX=1,NCAS1
            if (NCAS2.lt.MC) then
              NCAS2=NCAS2+1
              ICGT2(NCAS2)=ICGT1(IX)
              ICGS2(NCAS2)=ICGS1(IX)
              ICGF2(NCAS2)=ICGF1(IX)
              CMGS2(NCAS2)=CMGS1(IX)
              CMGL2(NCAS2)=CMGL1(IX)
              RADC2(NCAS2)=RADC1(IX)
              CONC2(NCAS2)=CONC1(IX)
              pf2(NCAS2)=PF1(IX)
              ipf2(NCAS2)=IPF1(IX)
              pwr2(NCAS2)=PWR1(IX)
              bvolt2(NCAS2)=BVOLT1(IX)
              iphas2(NCAS2)=IPHAS1(IX)
            endif
 210      continue
        elseif (IGU.eq.2) then
          do 220 IX=1,NCAS1
            if (NCAS3.lt.MC) then
              NCAS3=NCAS3+1
              ICGT3(NCAS3)=ICGT1(IX)
              ICGS3(NCAS3)=ICGS1(IX)
              ICGF3(NCAS3)=ICGF1(IX)
              CMGS3(NCAS3)=CMGS1(IX)
              CMGL3(NCAS3)=CMGL1(IX)
              RADC3(NCAS3)=RADC1(IX)
              CONC3(NCAS3)=CONC1(IX)
              pf3(NCAS3)=PF1(IX)
              ipf3(NCAS3)=IPF1(IX)
              pwr3(NCAS3)=PWR1(IX)
              bvolt3(NCAS3)=BVOLT1(IX)
              iphas3(NCAS3)=IPHAS1(IX)
            endif
 220      continue
        elseif (IGU.eq.3) then
          do 230 IX=1,NCAS2
            if (NCAS3.lt.MC) then
              NCAS3=NCAS3+1
              ICGT3(NCAS3)=ICGT2(IX)
              ICGS3(NCAS3)=ICGS2(IX)
              ICGF3(NCAS3)=ICGF2(IX)
              CMGS3(NCAS3)=CMGS2(IX)
              CMGL3(NCAS3)=CMGL2(IX)
              RADC3(NCAS3)=RADC2(IX)
              CONC3(NCAS3)=CONC2(IX)
              pf3(NCAS3)=PF2(IX)
              ipf3(NCAS3)=IPF2(IX)
              pwr3(NCAS3)=PWR2(IX)
              bvolt3(NCAS3)=BVOLT2(IX)
              iphas3(NCAS3)=IPHAS2(IX)
            endif
 230      continue
        endif
      endif

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

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C LPROJ   - zone scheduling (operation file)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      COMMON/precz/zname(MCOM),zdesc(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
      common/p3ver/ip3ver
      CHARACTER oprdesc*248,ctlstr*24
      COMMON/P1/oprdesc,ctlstr
      COMMON/P1C/oprdescc,ctlstrc
      COMMON/P2/NAC1,IACS1(MA),IACF1(MA),ACI1(MA),ACV1(MA),
     &          IPT1(MA),TA1(MA),NAC2,IACS2(MA),IACF2(MA),ACI2(MA),
     &          ACV2(MA),IPT2(MA),TA2(MA),NAC3,IACS3(MA),IACF3(MA),
     &          ACI3(MA),ACV3(MA),IPT3(MA),TA3(MA)
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)
      common/loadlabelc/lodlabelc(7)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Base area for zones.
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)

C Storage arrays for weekdays, saturdays, sundays
      common/P3WK/NCAS1C,ICGS1C(MC),ICGF1C(MC),CMGS1C(MC),CMGL1C(MC),
     &          RADC1C(MC),CONC1C(MC)
      common/P3SA/NCAS2C,ICGS2C(MC),ICGF2C(MC),CMGS2C(MC),CMGL2C(MC),
     &          RADC2C(MC),CONC2C(MC)
      common/P3SU/NCAS3C,ICGS3C(MC),ICGF3C(MC),CMGS3C(MC),CMGL3C(MC),
     &          RADC3C(MC),CONC3C(MC)
      COMMON/P3TYPEC/ICGT1C(MC),ICGT2C(MC),ICGT3C(MC)

      common/elecflgc/ielfc
      common/elp3wk/nel1c,pf1c(mc),ipf1c(mc),pwr1c(mc),bvolt1c(mc),
     &  iphas1c(mc)
      common/elp3sa/nel2c,pf2c(mc),ipf2c(mc),pwr2c(mc),bvolt2c(mc),
     &  iphas2c(mc)
      common/elp3su/nel3c,pf3c(mc),ipf3c(mc),pwr3c(mc),bvolt3c(mc),
     &  iphas3c(mc)

      character*(*) sfile
      CHARACTER oprdescc*248,ctlstrc*24
      character h*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,ltmp*72
      character lodlabel*6,lodlabelc*6
      character zname*12,zdesc*64,fs*1,GFILE*72,iwatt*8
      character actcas*8
      character act*1   ! '-' normal, 's' silent
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      LOGICAL XST,unixok,dok,ok

C Flags noting whether casual gain periods are currently sorted.
      logical problem,MODOPR

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C It is assumed that a prior call to COPYAIRFL has been made
C so there is no need to clear nac1 nac2 nac3.
      IER=0
      NCAS1=0
      NCAS2=0
      NCAS3=0
      lodlabel(idest,1)='Occupt'
      lodlabel(idest,2)='Lights'
      lodlabel(idest,3)='Equipt'  
      lodlabel(idest,4)='NA    '  
      lodlabel(idest,5)='AnnEl '  
      lodlabel(idest,6)='NA    ' 
      lodlabel(idest,7)='NA    '      
      MODOPR=.FALSE.

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

C Use ERZSCHED to fill P2C and P2CTLC common blocks and
C then jump to 32 to read in the destination operations file.
          CALL ERZSCHED(IUO,sfile,IER)
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

C Copy the casual gains into the local arrays for weekdays, saturdays, sundays.
      ielfc=ielf(isource)
      NCAS1C=NCAS1
      nel1c=nel1
      if(NCAS1.gt.0)then
        do 10 L=1,NCAS1
          ICGS1C(L)=ICGS1(L)
          ICGF1C(L)=ICGF1(L)
          CMGS1C(L)=CMGS1(L)
          CMGL1C(L)=CMGL1(L)
          RADC1C(L)=RADC1(L)
          CONC1C(L)=CONC1(L)
          ICGT1C(L)=ICGT1(L)
          if(ielfc.ne.0)then
            pf1c(L)=pf1(L)
            ipf1c(L)=ipf1(L)
            pwr1c(L)=pwr1(L)
            bvolt1c(L)=bvolt1(L)
            iphas1c(L)=iphas1(L)
          endif
  10    continue
      endif
      NCAS2C=NCAS2
      nel2c=nel2
      if(NCAS2.gt.0)then
        do 20 L=1,NCAS2
          ICGS2C(L)=ICGS2(L)
          ICGF2C(L)=ICGF2(L)
          CMGS2C(L)=CMGS2(L)
          CMGL2C(L)=CMGL2(L)
          RADC2C(L)=RADC2(L)
          CONC2C(L)=CONC2(L)
          ICGT2C(L)=ICGT2(L)
          if(ielfc.ne.0)then
            pf2c(L)=pf2(L)
            ipf2c(L)=ipf2(L)
            pwr2c(L)=pwr2(L)
            bvolt2c(L)=bvolt2(L)
            iphas2c(L)=iphas2(L)
          endif
  20    continue
      endif
      NCAS3C=NCAS3
      nel3c=nel3
      if(NCAS3.gt.0)then
        do 30 L=1,NCAS3
          ICGS3C(L)=ICGS3(L)
          ICGF3C(L)=ICGF3(L)
          CMGS3C(L)=CMGS3(L)
          CMGL3C(L)=CMGL3(L)
          RADC3C(L)=RADC3(L)
          CONC3C(L)=CONC3(L)
          ICGT3C(L)=ICGT3(L)
          if(ielfc.ne.0)then
            pf3c(L)=pf3(L)
            ipf3c(L)=ipf3(L)
            pwr3c(L)=pwr3(L)
            bvolt3c(L)=bvolt3(L)
            iphas3c(L)=iphas3(L)
          endif
  30    continue
      endif
      oprdescc=oprdesc

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
     &      zname(idest)(1:lnblnk(zname(idest))),'.opr'
        else
          WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &      zname(idest)(1:lnblnk(zname(idest))),'.opr'
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
        ctlstr='no control of air flow  '
        NAC1=0
        NAC2=0
        NAC3=0

C And any new file will be in the sorted format.
        ip3ver=1
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
        NCAS1=NCAS1C
        nel1=nel1c
        if(NCAS1C.gt.0)then
          do 11 L=1,NCAS1C
            ICGS1(L)=ICGS1C(L)
            ICGF1(L)=ICGF1C(L)
            if(ICGT1C(L).lt.0)then
              CMGS1(L)=CMGS1C(L)
              CMGL1(L)=CMGL1C(L)
            else
              CMGS1(L)=CMGS1C(L)/ratioarea
              CMGL1(L)=CMGL1C(L)/ratioarea
            endif
            RADC1(L)=RADC1C(L)
            CONC1(L)=CONC1C(L)
            ICGT1(L)=ICGT1C(L)
            if(ielfc.ne.0)then
              pf1(L)=pf1c(L)
              ipf1(L)=ipf1c(L)
              pwr1(L)=pwr1c(L)
              bvolt1(L)=bvolt1c(L)
              iphas1(L)=iphas1c(L)
            endif
  11      continue
        endif
        NCAS2=NCAS2C
        nel2=nel2c
        if(NCAS2C.gt.0)then
          do 21 L=1,NCAS2C
            ICGS2(L)=ICGS2C(L)
            ICGF2(L)=ICGF2C(L)
            if(ICGT2C(L).lt.0)then
              CMGS2(L)=CMGS2C(L)
              CMGL2(L)=CMGL2C(L)
            else
              CMGS2(L)=CMGS2C(L)/ratioarea
              CMGL2(L)=CMGL2C(L)/ratioarea
            endif
            RADC2(L)=RADC2C(L)
            CONC2(L)=CONC2C(L)
            ICGT2(L)=ICGT2C(L)
            if(ielfc.ne.0)then
              pf2(L)=pf2c(L)
              ipf2(L)=ipf2c(L)
              pwr2(L)=pwr2c(L)
              bvolt2(L)=bvolt2c(L)
              iphas2(L)=iphas2c(L)
            endif
  21      continue
        endif
        NCAS3=NCAS3C
        nel3=nel3c
        if(NCAS3C.gt.0)then
          do 31 L=1,NCAS3C
            ICGS3(L)=ICGS3C(L)
            ICGF3(L)=ICGF3C(L)
            if(ICGT3C(L).lt.0)then
              CMGS3(L)=CMGS3C(L)
              CMGL3(L)=CMGL3C(L)
            else
              CMGS3(L)=CMGS3C(L)/ratioarea
              CMGL3(L)=CMGL3C(L)/ratioarea
            endif
            RADC3(L)=RADC3C(L)
            CONC3(L)=CONC3C(L)
            ICGT3(L)=ICGT3C(L)
            if(ielfc.ne.0)then
              pf3(L)=pf3c(L)
              ipf3(L)=ipf3c(L)
              pwr3(L)=pwr3c(L)
              bvolt3(L)=bvolt3c(L)
              iphas3(L)=iphas3c(L)
            endif
  31      continue
        endif

C << could also offer this copy process when importing air flow
C << patterns (however it would probably work better to have a
C << separate documentation string for air flows and casual gains.
        if(act(1:1).eq.'-')then
          call edisp(iuout,' ')
          call edisp(iuout,' Current operation notes:')
          call edisp248(iuout,oprdesc,80)
          call edisp(iuout,' ')
          call edisp(iuout,' Source operation notes:')
          call edisp248(iuout,oprdescc,80)
          call edisp(iuout,' ')
          dok=.true.
          call askok('Use the source documentation?',' ',OK,DOK,0)
          if(ok) oprdesc=oprdescc
        elseif(act(1:1).eq.'s')then
          oprdesc=oprdescc
        endif

        if(act(1:1).eq.'-')then
          H(1)='Notes (<248 char) for schedules of zone air flow and' 
          H(2)='casual gains can be included in the model. Use this'
          H(3)='facility to clarify what is happening in the zone.'
          CALL EASKS248(oprdescc,'Operations notes (please edit):',
     &      ' ',72,'base_case','operation notes',IER,3)
          if(oprdescc(1:2).ne.'  ')oprdesc=oprdescc
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
      CALL EMKOPER(IUO,LPROJ(idest),idest,ITRU,IER)
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

      subroutine PLTCASGN(ICOMP)
#include "building.h"

      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Descriptive label for a zone load or casual gain.
      common/loadlabel/lodlabel(mcom,7)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)

      dimension CGAINS(25,7),CGAINL(25,7),CGAINTS(25),CGAINTL(25)

      character lodlabel*6, outs*124
      character zname*12,zdesc*64

      logical COLOK

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
        do 20 J=1,NCAS1
          if (I.gt.ICGS1(J).and.I.le.ICGF1(J)) then
            if (ICGT1(J).eq.-1) then

C For case of m2/person CMGL1()J remains zero so use cmgs1(J) instead.
              CGNS=CGNS+real(int((ZBASEA(ICOMP)/CMGS1(J))))*95.
              CGNL=CGNL+real(int((ZBASEA(ICOMP)/CMGS1(J))))*45.
            elseif (ICGT1(J).lt.-1) then
              CGNS=CGNS+CMGS1(J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL1(J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS1(J)
              CGNL=CGNL+CMGL1(J)
            endif
          endif
 20     continue
        YMAXWKD=AMAX1(YMAXWKD,CGNS,CGNL)
 10   continue
      do 12 I=1,24
        CGNS=0.
        CGNL=0.
        do 22 J=1,NCAS2
          if (I.gt.ICGS2(J).and.I.le.ICGF2(J)) then
            if (ICGT2(J).eq.-1) then
              CGNS=CGNS+real(int((ZBASEA(ICOMP)/CMGS2(J))))*95.
              CGNL=CGNL+real(int((ZBASEA(ICOMP)/CMGS2(J))))*45.
            elseif (ICGT2(J).lt.-1) then
              CGNS=CGNS+CMGS2(J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL2(J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS2(J)
              CGNL=CGNL+CMGL2(J)
            endif
          endif
 22     continue
        YMAXSAT=AMAX1(YMAXSAT,CGNS,CGNL)
 12   continue
      do 14 I=1,24
        CGNS=0.
        CGNL=0.
        do 24 J=1,NCAS3
          if (I.gt.ICGS3(J).and.I.le.ICGF3(J)) then
            if (ICGT3(J).eq.-1) then
              CGNS=CGNS+real(int((ZBASEA(ICOMP)/CMGS3(J))))*95.
              CGNL=CGNL+real(int((ZBASEA(ICOMP)/CMGS3(J))))*45.
            elseif (ICGT3(J).lt.-1) then
              CGNS=CGNS+CMGS3(J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL3(J)*ZBASEA(ICOMP)
            else
              CGNS=CGNS+CMGS3(J)
              CGNL=CGNL+CMGL3(J)
            endif
          endif
 24     continue
        YMAXSUN=AMAX1(YMAXSUN,CGNS,CGNL)
 14   continue

C Clear and setup plot area.
      CALL startbuffer()
      CALL win3d(menuchw,10,10,8,3,igl,igr,igt,igb,igw,igwh)

C Calculate plotting box locations.
      ipltwd=int(igw/3)-6
      iglsat=igl+ipltwd+9
      iglsun=igl+2*(ipltwd+9)
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
      CALL axiscale(ipltwd,igwh,plxmin,plxmax,YMIN,YMAXF,
     &                                         xsc,ysc,sca,xadd,yadd)
      call vrtaxis(YMIN,YMAXF,igr,igb,igt,yadd,ysc,0,1,'Gain (W/m^2)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igwh,plxmin,plxmax,YMIN,YMAX,
     &                                         xsc,ysc,sca,xadd,yadd)

C Scale and draw Weekday plot.
      CALL horaxis(plxmin,plxmax,igl,igr,igb,xadd,xsc,0,'Time (hrs)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,yadd,ysc,0,0,'Gain (W)')
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmax,YMIN,0,0)
      CALL etplot(plxmax,YMAX,1,0)
      CALL etplot(plxmin,YMAX,1,0)

C Scale and draw Saturday plot.
      CALL horaxis(plxmin,plxmax,iglsat,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmax,YMIN,0,0)
      CALL etplot(plxmax,YMAX,1,0)
      CALL etplot(plxmin,YMAX,1,0)
      CALL etplot(plxmin,YMIM,1,0)

C Scale and draw Sunday plot.
      CALL horaxis(plxmin,plxmax,iglsun,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      CALL etplot(plxmin,YMIN,0,0)
      CALL etplot(plxmin,YMAX,1,0)
      CALL etplot(plxmax,YMAX,1,0)

C Scan gains and generate total curves.
C Weekdays.
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      do 50 IP=1,7
        do 55 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 55     continue
 50   continue
      do 60 IP=1,NCAS1
        ICG=iabs(ICGT1(IP))
        do 65 IHR=ICGS1(IP),ICGF1(IP)-1
          if (ICGT1(IP).eq.-1) then

C For case of m2/person CMGL is zero so use CMGS to get CGNL
            CGNS=real(int((ZBASEA(ICOMP)/CMGS1(IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS1(IP))))*45.
          elseif (ICGT1(IP).lt.-1) then
            CGNS=CMGS1(IP)*ZBASEA(ICOMP)
            CGNL=CMGL1(IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS1(IP)
            CGNL=CMGL1(IP)
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
          CALL etplot(XST,CGAINS(IHR,I),0,0)
          CALL etplot(XFN,CGAINS(IHR,I),1,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINS(IHR+1,I),1,0)
          CALL etplot(XST,CGAINL(IHR,I),0,0)
          CALL etplot(XFN,CGAINL(IHR,I),-4,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINL(IHR+1,I),-4,0)
          if (COLOK) call winscl('-',0)
 75     continue
        CALL etplot(XST,CGAINTS(IHR),0,0)
        CALL etplot(XFN,CGAINTS(IHR),1,0)
        CALL etplot(XFN,CGAINTS(IHR+1),1,0)
        CALL etplot(XST,CGAINTL(IHR),0,0)
        CALL etplot(XFN,CGAINTL(IHR),-4,0)
        CALL etplot(XFN,CGAINTL(IHR+1),-4,0)
 70   continue

C Saturdays.
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      do 150 IP=1,7
        do 155 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 155    continue
 150  continue
      do 160 IP=1,NCAS2
        ICG=iabs(ICGT2(IP))
        do 165 IHR=ICGS2(IP),ICGF2(IP)-1
          if (ICGT2(IP).eq.-1) then
            CGNS=real(int((ZBASEA(ICOMP)/CMGS2(IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS2(IP))))*45.
          elseif (ICGT2(IP).lt.-1) then
            CGNS=CMGS2(IP)*ZBASEA(ICOMP)
            CGNL=CMGL2(IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS2(IP)
            CGNL=CMGL2(IP)
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
          CALL etplot(XST,CGAINS(IHR,I),0,0)
          CALL etplot(XFN,CGAINS(IHR,I),1,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINS(IHR+1,I),1,0)
          CALL etplot(XST,CGAINL(IHR,I),0,0)
          CALL etplot(XFN,CGAINL(IHR,I),-4,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINL(IHR+1,I),-4,0)
          if (COLOK) call winscl('-',0)
 175    continue
        CALL etplot(XST,CGAINTS(IHR),0,0)
        CALL etplot(XFN,CGAINTS(IHR),1,0)
        CALL etplot(XFN,CGAINTS(IHR+1),1,0)
        CALL etplot(XST,CGAINTL(IHR),0,0)
        CALL etplot(XFN,CGAINTL(IHR),-4,0)
        CALL etplot(XFN,CGAINTL(IHR+1),-4,0)
 170  continue

C Sundays.
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      do 250 IP=1,7
        do 255 IHR=1,25
          CGAINS(IHR,IP)=0.
          CGAINL(IHR,IP)=0.
          CGAINTS(IHR)=0.
          CGAINTL(IHR)=0.
 255    continue
 250  continue
      do 260 IP=1,NCAS3
        ICG=iabs(ICGT3(IP))
        do 265 IHR=ICGS3(IP),ICGF3(IP)-1
          if (ICGT3(IP).eq.-1) then
            CGNS=real(int((ZBASEA(ICOMP)/CMGS3(IP))))*95.
            CGNL=real(int((ZBASEA(ICOMP)/CMGS3(IP))))*45.
          elseif (ICGT3(IP).lt.-1) then
            CGNS=CMGS3(IP)*ZBASEA(ICOMP)
            CGNL=CMGL3(IP)*ZBASEA(ICOMP)
          else
            CGNS=CMGS3(IP)
            CGNL=CMGL3(IP)
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
          CALL etplot(XST,CGAINS(IHR,I),0,0)
          CALL etplot(XFN,CGAINS(IHR,I),1,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINS(IHR+1,I),1,0)
          CALL etplot(XST,CGAINL(IHR,I),0,0)
          CALL etplot(XFN,CGAINL(IHR,I),-4,0)
          if (IHR.lt.24) CALL etplot(XFN,CGAINL(IHR+1,I),-4,0)
          if (COLOK) call winscl('-',0)
 275    continue
        CALL etplot(XST,CGAINTS(IHR),0,0)
        CALL etplot(XFN,CGAINTS(IHR),1,0)
        CALL etplot(XFN,CGAINTS(IHR+1),1,0)
        CALL etplot(XST,CGAINTL(IHR),0,0)
        CALL etplot(XFN,CGAINTL(IHR),-4,0)
        CALL etplot(XFN,CGAINTL(IHR+1),-4,0)
 270  continue

C Title and labels.
      write (outs,'(2a)') 'Casual gains for ',ZNAME(ICOMP)
      CALL viewtext(outs,1,1,1)

      call wstxpt(igl+20,igt-65,' Total sensible')
      call wstxpt(igr-100,igt-65,'Total latent')
      call pixel2u(igl+120,igt-69,gx,gy)
      CALL etplot(gx,gy,0,0)
      call pixel2u(igl+180,igt-69,gx,gy)
      CALL etplot(gx,gy,1,0)
      call pixel2u(igr-120,igt-69,gx,gy)
      CALL etplot(gx,gy,0,0)
      call pixel2u(igr-180,igt-69,gx,gy)
      CALL etplot(gx,gy,-4,0)
      do 300 I=1,3
        write(outs,'(2a)')
     &        lodlabel(ICOMP,I)(1:lnblnk(lodlabel(ICOMP,I))),' sensible'
        IY=igt-(12*I)-18
        call wstxpt(igl+20,IY,outs)
        write(outs,'(2a)')
     &          lodlabel(ICOMP,I)(1:lnblnk(lodlabel(ICOMP,I))),' latent'
        call wstxpt(igr-100,IY,outs)
          if (COLOK) call winscl('z',I)
          call pixel2u(igl+120,IY-4,gx,gy)
          CALL etplot(gx,gy,0,0)
          call pixel2u(igl+180,IY-4,gx,gy)
          CALL etplot(gx,gy,1,0)
          call pixel2u(igr-120,IY-4,gx,gy)
          CALL etplot(gx,gy,0,0)
          call pixel2u(igr-180,IY-4,gx,gy)
          CALL etplot(gx,gy,-4,0)
          if (COLOK) call winscl('-',0)
 300  continue

      ioff=(ipltwd/3)
      call wstxpt(igl+ioff,igt-10,'Weekday')
      call wstxpt(iglsat+ioff,igt-10,'Saturday')
      call wstxpt(iglsun+ioff,igt-10,'Sunday')
      call forceflush()

      return
      end

C ************* insdelcasper
C Inscasper inserts or deletes a casual gain period into the
C current P3 & elp3 data structure.
C Parameters:
C   icomp - current zone.
C   idaytype - day type (weekdays = 1, sat = 2, sun = 3 
C   idp - point of insertion or deletion. 
C   ier - set to 1 if there was a problem
      subroutine insdelcasper(icomp,idaytype,act,idp,ier)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      common/elp3/nel1,pf1(mc),ipf1(mc),pwr1(mc),bvolt1(mc),iphas1(mc),
     &            nel2,pf2(mc),ipf2(mc),pwr2(mc),bvolt2(mc),iphas2(mc),
     &            nel3,pf3(mc),ipf3(mc),pwr3(mc),bvolt3(mc),iphas3(mc)

      character act*1

C For insertion set loop limites depending on the day type.
      if(act.eq.'i')then
        if(idaytype.eq.1)then
          ncas=ncas1+1
        elseif(idaytype.eq.2)then
          ncas=ncas2+1
        elseif(idaytype.eq.3)then
          ncas=ncas3+1
        endif
        if(ncas.gt.MC)then
          call edisp(iuout,'No additional periods can be inserted.')
          ier=1
          return
        endif

C Use the same 234 logic as found in insert.F to shift P3 and elp3
C data up.
        ix = ncas+1
 234    continue
        ix = ix-1
        if(idaytype.eq.1)then
          call CPYCASIJ(icomp,idaytype,ix,ix-1,ier)
          ncas1=ncas1+1
          if(ielf(icomp).ne.0)then
            nel1=nel1+1
          endif
        elseif(idaytype.eq.2)then
          call CPYCASIJ(icomp,idaytype,ix,ix-1,ier)
          ncas2=ncas2+1
          if(ielf(icomp).ne.0)then
            nel2=nel2+1
          endif
        elseif(idaytype.eq.3)then
          call CPYCASIJ(icomp,idaytype,ix,ix-1,ier)
          ncas3=ncas3+1
          if(ielf(icomp).ne.0)then
            nel3=nel3+1
          endif
        endif
        IF(IX.GT.idp+1)GOTO 234
        return
      elseif(act.eq.'d')then

C Delete period idp by copying data above it down one and then
C decrementing the appropraite counter. If the period to delete
C is the last possible period then only decrement counters.
        if(idp.eq.MC)then
          if(idaytype.eq.1)then
            NCAS1=NCAS1-1
            if(ielf(icomp).ne.0)nel1=nel1-1
          elseif(idaytype.eq.2)then
            NCAS2=NCAS2-1
            if(ielf(icomp).ne.0)nel2=nel2-1
          elseif(idaytype.eq.3)then
            NCAS3=NCAS3-1
            if(ielf(icomp).ne.0)nel3=nel3-1
          endif
        else
          if(idaytype.eq.1)then
            do 110 IX=IDP,NCAS1
              call CPYCASIJ(icomp,idaytype,ix,ix+1,ier)
              if(ielf(icomp).ne.0)then
                nel1=nel1-1
              endif
  110       continue            
            NCAS1=NCAS1-1
          elseif(idaytype.eq.2)then
            do 111 IX=IDP,NCAS2
              call CPYCASIJ(icomp,idaytype,ix,ix+1,ier)
              if(ielf(icomp).ne.0)then
                nel2=nel2-1
              endif
  111       continue            
            NCAS2=NCAS2-1
          elseif(idaytype.eq.3)then
            do 112 IX=IDP,NCAS3
              call CPYCASIJ(icomp,idaytype,ix,ix+1,ier)
              if(ielf(icomp).ne.0)then
                nel3=nel3-1
              endif
  112       continue            
            NCAS3=NCAS3-1
          endif
        endif
        return
      endif

      end

C ******* initcasday
C initcasday does initial setup of a casual day type. It
C is passed act = 'wk' for weekday, 'sa' for saturday and
C 'su' for sunday.
      subroutine initcasday(act,ier)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)
      common/loadlabel/lodlabel(mcom,7)
      dimension iper(24),iperf(24)
      CHARACTER*6 lodlabel
      character hold*48,H*72,act*2
      LOGICAL OK,DOK,XST,MODOPR,browse,clkok,unixok

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

      if(act(1:2).eq.'wk')then
        nbocper=1
        nbltper=1
        nbspper=1
        hold='                                    '
        write(hold,'(3i5,a)') nbocper,nbltper,nbspper,'  '
        CALL EASKS(HOLD,' ',
     &    'No. of Wweekday casual gain periods?',48,'  0  0  0  ',
     &    'no. weekday CG periods',IER,14)
        K=0
        CALL EGETWI(HOLD,K,nbocper,0,24,'F','occup perds',IER)
        CALL EGETWI(HOLD,K,nbltper,0,24,'F','light perds',IER)
        CALL EGETWI(HOLD,K,nbspper,0,24,'F','sm pwr perds',IER)
C        write(6,*) 'occup light sm pwr per ',nbocper,nbltper,hbspper

C Get user to specify the start times for occup periods if non-zero.
C Increment ipcount for each new period.
        ipcount=NCAS1
        if(nbocper.gt.0)then
          call initcasper(nbocper,iper,iperf,'i','weekday occupant')
          do 71 ij=1,nbocper
            ipcount=ipcount+1
            ICGS1(ipcount) = iper(ij)
            ICGF1(ipcount) = iperf(ij)
            CMGS1(ipcount) = 0.0
            CMGL1(ipcount) = 0.0
            RADC1(ipcount) = 0.5
            CONC1(ipcount) = 0.5
            ICGT1(ipcount) = 1
  71      continue
        endif
        if(nbltper.gt.0)then
          call initcasper(nbltper,iper,iperf,'i','weekday lighting')
          do 72 ij=1,nbltper
            ipcount=ipcount+1
            ICGS1(ipcount) = iper(ij)
            ICGF1(ipcount) = iperf(ij)
            CMGS1(ipcount) = 0.0
            CMGL1(ipcount) = 0.0
            RADC1(ipcount) = 0.5
            CONC1(ipcount) = 0.5
            ICGT1(ipcount) = 2
  72      continue
        endif
        if(nbspper.gt.0)then
          call initcasper(nbspper,iper,iperf,'i','weekday small power')
          do 73 ij=1,nbspper
            ipcount=ipcount+1
            ICGS1(ipcount) = iper(ij)
            ICGF1(ipcount) = iperf(ij)
            CMGS1(ipcount) = 0.0
            CMGL1(ipcount) = 0.0
            RADC1(ipcount) = 0.4
            CONC1(ipcount) = 0.6
            ICGT1(ipcount) = 3
  73      continue
        endif
        NCAS1=NCAS1+nbocper+nbltper+nbspper
      elseif(act(1:2).eq.'sa')then
        nbocper=1
        nbltper=1
        nbspper=1
        hold='                                    '
        write(hold,'(3i5,a)') nbocper,nbltper,nbspper,'  '
        CALL EASKS(HOLD,' ',
     &    'No. of Saturday casual gain periods?',48,'  0  0  0   ',
     &    'nb saturday ocup lts sm pwr periods',IER,14)
        K=0
        CALL EGETWI(HOLD,K,nbocper,0,24,'F','occup perds',IER)
        CALL EGETWI(HOLD,K,nbltper,0,24,'F','light perds',IER)
        CALL EGETWI(HOLD,K,nbspper,0,24,'F','sm pwr perds',IER)
C        write(6,*) 'occup light sm pwr per ',nbocper,nbltper,hbspper

C Get user to specify the start times for occup periods if non-zero.
C Increment ipcount for each new period.
        ipcount=NCAS2
        if(nbocper.gt.0)then
          call initcasper(nbocper,iper,iperf,'i','saturday occupant')
          do 74 ij=1,nbocper
            ipcount=ipcount+1
            ICGS2(ipcount) = iper(ij)
            ICGF2(ipcount) = iperf(ij)
            CMGS2(ipcount) = 0.0
            CMGL2(ipcount) = 0.0
            RADC2(ipcount) = 0.5
            CONC2(ipcount) = 0.5
            ICGT2(ipcount) = 1
  74      continue
        endif
        if(nbltper.gt.0)then
          call initcasper(nbltper,iper,iperf,'i','saturday lighting')
          do 75 ij=1,nbltper
            ipcount=ipcount+1
            ICGS2(ipcount) = iper(ij)
            ICGF2(ipcount) = iperf(ij)
            CMGS2(ipcount) = 0.0
            CMGL2(ipcount) = 0.0
            RADC2(ipcount) = 0.5
            CONC2(ipcount) = 0.5
            ICGT2(ipcount) = 2
  75      continue
        endif
        if(nbspper.gt.0)then
          call initcasper(nbspper,iper,iperf,'i','saturday small power')
          do 76 ij=1,nbspper
            ipcount=ipcount+1
            ICGS2(ipcount) = iper(ij)
            ICGF2(ipcount) = iperf(ij)
            CMGS2(ipcount) = 0.0
            CMGL2(ipcount) = 0.0
            RADC2(ipcount) = 0.4
            CONC2(ipcount) = 0.6
            ICGT2(ipcount) = 3
  76      continue
        endif
        NCAS2=NCAS2+nbocper+nbltper+nbspper
      elseif(act(1:2).eq.'su')then
        nbocper=1
        nbltper=1
        nbspper=1
        hold='                                    '
        write(hold,'(3i5,a)') nbocper,nbltper,nbspper,'  '
        CALL EASKS(HOLD,' ',
     &    'No. ofs Sunday casual gain periods?',48,'  0  0  0   ',
     &    'nb sunday ocup lts sm pwr periods',IER,14)
        K=0
        CALL EGETWI(HOLD,K,nbocper,0,24,'F','occup perds',IER)
        CALL EGETWI(HOLD,K,nbltper,0,24,'F','light perds',IER)
        CALL EGETWI(HOLD,K,nbspper,0,24,'F','sm pwr perds',IER)
C        write(6,*) 'occup light sm pwr per ',nbocper,nbltper,hbspper

C Get user to specify the start times for occup periods if non-zero.
C Increment ipcount for each new period.
        ipcount=NCAS3
        if(nbocper.gt.0)then
          call initcasper(nbocper,iper,iperf,'i','sunday occupant')
          do 77 ij=1,nbocper
            ipcount=ipcount+1
            ICGS3(ipcount) = iper(ij)
            ICGF3(ipcount) = iperf(ij)
            CMGS3(ipcount) = 0.0
            CMGL3(ipcount) = 0.0
            RADC3(ipcount) = 0.5
            CONC3(ipcount) = 0.5
            ICGT3(ipcount) = 1
  77      continue
        endif
        if(nbltper.gt.0)then
          call initcasper(nbltper,iper,iperf,'i','sunday lighting')
          do 78 ij=1,nbltper
            ipcount=ipcount+1
            ICGS3(ipcount) = iper(ij)
            ICGF3(ipcount) = iperf(ij)
            CMGS3(ipcount) = 0.0
            CMGL3(ipcount) = 0.0
            RADC3(ipcount) = 0.5
            CONC3(ipcount) = 0.5
            ICGT3(ipcount) = 2
  78      continue
        endif
        if(nbspper.gt.0)then
          call initcasper(nbspper,iper,iperf,'i','sunday small power')
          do 79 ij=1,nbspper
            ipcount=ipcount+1
            ICGS3(ipcount) = iper(ij)
            ICGF3(ipcount) = iperf(ij)
            CMGS3(ipcount) = 0.0
            CMGL3(ipcount) = 0.0
            RADC3(ipcount) = 0.4
            CONC3(ipcount) = 0.6
            ICGT3(ipcount) = 3
  79      continue
        endif
        NCAS3=NCAS3+nbocper+nbltper+nbspper
      endif
C      write(6,*) 'ncas1 ncas2 ncas3 ',ncas1,ncas2,ncas3
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
      character act*1,cstr*72,h*72,label*24,msg*72
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
