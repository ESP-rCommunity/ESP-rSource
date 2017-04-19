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
C EDCASGN - edit casual gain data for one period.
C CDCASGN - copy/ delete casual gain data.
C IMPORTZCASG - import a casual gain regime from zone isource to
C               zone idest based on user instructions.
C PLTCASGN - plot casual gain data.
C Inscasper - inserts or deletes a casual gain period into the
C             current P3 & elp3 data structure.
C initcasday - does initial setup of a casual day type.
C initcasday21 - does initial setup of one or more casual day labels types.
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
#include "schedule.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      common/user/browse

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK      
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Flags noting whether casual gain periods are currently sorted.
      logical sorted,problem

C Flag noting whether file was browsed for (so that cfg can be
C updated when the user exits).
      logical gotviabrowse

      CHARACTER tcname*248
      CHARACTER ITEM(14)*33,choices(11)*32,LTMP*72,GFILE*72
      character sfile*72,snpfile*72,fs*1,outs*124,louts*248
      character priorname*72
      integer lnprior,lnoprf
      character act*8,iwatt*8,afact*8,iflow*8,vflow*8

      LOGICAL OK,DOK,XST,MODOPR,browse,unixok,needtoask,clkok
      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer NIGU,IGU,NITEMS,INO ! max items and current menu item
      integer islot, iatr1   ! used when defining from scratch
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PRJFMK'  ! set for subroutine
       
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

C Remember the file name in case user browses to something else.
      write(priorname,'(a)') LPROJ(ICOMP)(1:lnblnk(LPROJ(ICOMP)))
      lnprior=lnblnk(priorname)

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
        helptopic='operations_need_to_ask'
        call gethelptext(helpinsub,helptopic,nbhelp)
      
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.2)then
          CALL EASKSCMD(LTMP,' ','Zone operations file options:',
     &      'browse',clkok,72,GFILE,'operations file',ISER,nbhelp)
          if(clkok)iclkok=1   ! notice browse button
          if(iser.eq.-3)iclkok=2  ! notice cancel button
        else
          CALL EASKS2CMD(LTMP,' ','Zone operations file options:',
     &      'browse','cancel',iclkok,72,GFILE,
     &      'operations file',IER,nbhelp)
        endif
        lnoprf=lnblnk(LTMP)
        call usrmsg(' ',' ','-')   ! clear the display
      else
        helptopic='operations_need_confirm'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.2)then
          CALL EASKSCMD(LTMP,'Zone operation file name','Confirm:',
     &      'browse another',clkok,72,GFILE,'operations file',ISER,
     &       nbhelp)
          if(clkok)iclkok=1   ! notice browse button
          if(iser.eq.-3)iclkok=2  ! notice cancel button
        else
          CALL EASKS2CMD(LTMP,'Zone operation file name','Confirm:',
     &      'browse another','cancel',iclkok,72,GFILE,
     &      'operations file',IER,nbhelp)
        endif
        call usrmsg(' ',' ','-')   ! clear the display
        lnoprf=lnblnk(LTMP)
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
     &            72,GFILE,'operations file',IER,nbhelp)
          endif
          lnoprf=lnblnk(LTMP)
        else
          LTMP=GFILE
          CALL EASKS(LTMP,' ','Zone operations file name?',
     &            72,GFILE,'operations file',IER,nbhelp)
          lnoprf=lnblnk(LTMP)
        endif

C Display contents of this browsed file.
        IUO=IFIL+1
        call FINDFIL(LTMP,XST)
        IF(XST) call ZSCHEDHINT(IUO,LTMP,IER)
        dok=.true.
        call askok(' ','Use this information?',OK,dok,nbhelp)
        if(.not.ok)return

      elseif(iclkok.eq.2)then
        call edisp(iuout,'No file selected so returning.')
        return
      endif

      LPROJ(ICOMP)=LTMP

C If the file name has changed then remember this in cfg file.
C Debug.
      if(priorname(1:lnprior).eq.ltmp(1:lnoprf))then
        continue  ! name is the same
      else
        call usrmsg('Updating model to reflect changes',
     &        'in zone operations file name...','-')
        CALL EMKCFG('-',ier)
        call usrmsg('Updating model to reflect changes',
     &        'in zone operations file name...done.','-')
      endif

  5   MODOPR=.FALSE.
 
C Instanciate for current day types prior to scanning the source operation file.
      DO 400 IDTY=1,NBDAYTYPE
        NAC(IDTY)=0
        NCAS(IDTY)=0
        ctlstr(icomp,IDTY)='no control of air flow  '
 400  CONTINUE
      write(oprdesc(icomp),'(a)') 'no casual gain notes (yet)'
      write(ventdesc(icomp),'(a)')
     &  'no infiltration or venitlation notes (yet)'
      IER=0

C Set the default casual labels for this zone.

C << should *type also be set here prior to reading file? >>
C << do we need any of this >>
      lodlabel(icomp,1)='Occupants '
      lodlabel(icomp,2)='Lights    '
      lodlabel(icomp,3)='SmallPower'  
      lodlabel(icomp,4)='Otherstuff'  
      lodlabel(icomp,5)='Ann.El    '  
      lodlabel(icomp,6)='Metabolic ' 
      lodlabel(icomp,7)='N/A       '      
 
C Read file for editing or listing. If there is no file then begin with
C default data (and set ip3ver to 2 in case user does nothing else).
      IUO=IFIL+1
      call FINDFIL(LPROJ(ICOMP),XST)
      IF(XST)THEN
        CALL ERPFREE(IUO,ISTAT)
        CALL EROPER(ITRC,ITRU,IUO,ICOMP,IER)

C Do cursory check to see if the file is sorted.
C << if newer ip3ver then do not bother with this >>
        sorted=.true.
        problem=.false.
        DO 401 IDTY=1,NBDAYTYPE
          call checksort(icomp,IDTY,problem)
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
  19    helptopic='operations_creation'
        call gethelptext(helpinsub,helptopic,nbhelp)
  
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

C Setup minimal description (single period for the initial 3 slots of
C casual gain arrays to zeros with no infiltration and ventilation).
C << expand for additional gain types >>
          DO 399 IDTY=1,NBDAYTYPE
            ctlstr(icomp,IDTY)='no control of air flow  '
            NAC(IDTY)=1
            IACS(IDTY,1)=0; IACF(IDTY,1)=24
            ACI(IDTY,1)=0.0; ACV(IDTY,1)=0.0
            IPT(IDTY,1)=0; TA(IDTY,1)=0.0
            NCAS(IDTY)=3
            ICGS(IDTY,1)=0; ICGF(IDTY,1)=24; ICGUnit(IDTY,1)=0
            CMGS(IDTY,1)=0.0; CMGL(IDTY,1)=0.0
            RADC(IDTY,1)=0.6; CONC(IDTY,1)=0.4  ! CIBSE Guide A 6.3
            ICGT(IDTY,1)=1
            ICGS(IDTY,2)=0; ICGF(IDTY,2)=24; ICGUnit(IDTY,2)=0
            CMGS(IDTY,2)=0.0; CMGL(IDTY,2)=0.0
            RADC(IDTY,2)=0.3; CONC(IDTY,2)=0.7  ! CIBSE Table A 6.5
            ICGT(IDTY,2)=2 
            ICGS(IDTY,3)=0; ICGF(IDTY,3)=24; ICGUnit(IDTY,3)=0
            CMGS(IDTY,3)=0.0; CMGL(IDTY,3)=0.0
            RADC(IDTY,3)=0.4; CONC(IDTY,3)=0.6
            ICGT(IDTY,3)=3 
 399      CONTINUE

C Instanciate initial documentation for air schedules and casual gains.
          write(oprdesc(icomp),'(2a)')
     &    'Nothing happens in this zone in terms of occupants lights',
     &    ' and small power. Initial period of 0-24 hour for each.'
          write(ventdesc(icomp),'(2a)')
     &    'The zone has no infiltration or ventilation from other ',
     &    'zones in the model and no control imposed on air movement.'

C Set the default labels, slot, type labels for this zone. Only set
C the lodslot to non-zero for the initial 3 casual gain slots.

C << it might not be necessary to set lodslot??? >>
          lodlabel(icomp,1)='Occupants'
          caskeytype(icomp,1)='people      '
          lodslot(icomp,1)=1; lodatr1(icomp,1)=0;  lodatr2(icomp,1)=0
          lodlabel(icomp,2)='Lights   '
          caskeytype(icomp,2)='lighting    '
          lodslot(icomp,2)=2; lodatr1(icomp,2)=0;  lodatr2(icomp,2)=0
          lodlabel(icomp,3)='SmallPower'
          caskeytype(icomp,3)='equipment   '  
          lodslot(icomp,3)=3; lodatr1(icomp,3)=0;  lodatr2(icomp,3)=0
          lodlabel(icomp,4)='Otherstuff'  
          caskeytype(icomp,4)='other       '  
          lodslot(icomp,4)=0; lodatr1(icomp,4)=0;  lodatr2(icomp,4)=0
          lodlabel(icomp,5)='Ann.El    '  
          caskeytype(icomp,5)='net_utilities'  
          lodslot(icomp,5)=0; lodatr1(icomp,5)=0;  lodatr2(icomp,5)=0
          lodlabel(icomp,6)='Metabolic ' 
          caskeytype(icomp,6)='dynamicpeople'  
          lodslot(icomp,6)=0; lodatr1(icomp,6)=0;  lodatr2(icomp,6)=0
          lodlabel(icomp,7)='N/A       '
          caskeytype(icomp,7)='-           '  
          lodslot(icomp,7)=0; lodatr1(icomp,7)=0;  lodatr2(icomp,7)=0
          ip3ver(icomp)=21   ! set version for testing 2.1
          MODOPR=.true.
          sorted=.true.

C Write file and return.
          LTMP=LPROJ(ICOMP)
          CALL EASKS(LTMP,'Zone operations file name','Confirm:',
     &      72,GFILE,'operations file',IER,nbhelp)
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
          helptopic='operations_creation'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('operation choices',nbhelp,'-',0,0,IER)
          goto 19
        endif

C Section dealing with importing air flows from whatever source.
        if(igu.eq.2.or.igu.eq.4.or.igu.eq.5.or.igu.eq.7)then
          if(igu.eq.4.or.igu.eq.7)then

C Warn the user that the selection list is going to come up twice.
C << this seems overly complex, try to just as for file once >>
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

C Display contents  this browsed file.
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
                write(louts,*) 'you selected ',sfile(1:lnblnk(sfile))
                call edisp248(iuout,louts,100)
                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 19
              endif

C Display contents of this browsed file.
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
     &     'infiltration only','cancel',ivopt,nbhelp)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'preserve volume flow rate','cancel',infv,nbhelp)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5

C << probably dangerious to import ventilation data unless we ask what
C << zone in the model is the source
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'preserve volume flow rate','cancel',ivntv,nbhelp)
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
C The from scratch should intially be version 2.1
          ip3ver(icomp)=21
          helptopic='operations_synopsys'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('operation setup',nbhelp,'-',0,0,IER)
          islot=0  ! first time use of initcasday21
          call initcasday21(icomp,islot,ier)

          MODOPR=.true.
          sorted=.true.
        elseif(igu.eq.3.or.igu.eq.4.or.igu.eq.6.or.igu.eq.7)then
          if(igu.eq.4.or.igu.eq.7)then
            call edisp(iuout,
     &      'You are being asked for the casual gains source zone.')
          endif

          if(igu.eq.3.or.igu.eq.4)then

C If igu is 3 or 4 import casual gains from another zone.
C Ask which zone, then confirm whether absolute W values are to be scaled.
            if(isource.ne.0)then
              write(outs,*) 'Previous zone selected was ',zname(isource)
              call edisp(iuout,outs)
            endif
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

C Display contents of this other zone file.
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
                write(louts,*) 'you selected ',sfile(1:lnblnk(sfile))
                call edisp248(iuout,louts,100)
                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 19
              endif

C Display contents of this browsed file.
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
     &       'convert by base area','cancel',ivntv,nbhelp)
          if(ivntv.eq.1) iwatt='W-W    '
          if(ivntv.eq.2) iwatt='W-m2-W '
          if(ivntv.eq.3) goto 5
          idest=icomp
          act='allcas'
          itrc=1
          call IMPORTZCASG(isource,idest,act,iwatt,sfile,itrc,'-',ier)

C Remind users to check their data.
          helptopic='check_automatic'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('operation import confirm',nbhelp,'-',0,0,IER)
        endif
      
      ENDIF

C Display different options depending on the version of the file.
C If ip3ver is < 21 then there is a single string for both air
C and casual descriptions.
  20  write(ITEM(1),'(2a)')   '  zone: ',zname(ICOMP)
      if(ip3ver(icomp).lt.21)then
        write(ITEM(2),'(3a)') 'a notes: ',oprdesc(ICOMP)(1:23),'-'
        write(ITEM(3),'(2x,2a)')   oprdesc(ICOMP)(24:53),'-'
        write(ITEM(4),'(2x,2a)')   oprdesc(ICOMP)(54:83),'-'
        write(ITEM(5),'(2x,2a)')   oprdesc(ICOMP)(84:112),'-'
      else
        write(ITEM(2),'(3a)') 'a air schedule notes: ',
     &    ventdesc(ICOMP)(1:10),'-'
        write(ITEM(3),'(2x,a)')    ventdesc(ICOMP)(11:41)
        write(ITEM(4),'(3a)') 'b casual gain notes: ',
     &    oprdesc(ICOMP)(1:11),'-'
        write(ITEM(5),'(2x,a)')    oprdesc(ICOMP)(12:42)
      endif
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

C Help text for this menu.
      helptopic='operations_summary'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
            if(ip3ver(icomp).eq.21)then
              continue  ! avoid attempting to source v21 file.
            else
              DOK=.true.
              CALL ASKOK(
     &          'Before exiting do you wish to sort possible changes',
     &          'to the casual gain periods?',OK,DOK,nbhelp)
              if(ok)then

C Sort the gains and then update the file and mark MODOPR.
                call PROCESSOLDCAS(ICOMP,0,iuout)
                IUO=IFIL+1
                CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
                MODOPR=.false.
              endif
            endif
          endif
        endif
        if(MODOPR.or.gotviabrowse)then
          if(browse)return

          DOK=.true.
          CALL ASKOK('Before exiting do you wish to',
     &      'save any changes made?',OK,DOK,nbhelp)
          IF(.not.OK) return

C So save the changes to the file and then exit.
          IUO=IFIL+1
          CALL EMKOPER(IUO,LPROJ(ICOMP),ICOMP,IER)
        endif
        RETURN

      ELSEIF(INO.EQ.2)THEN

        if(ip3ver(icomp).lt.21)then

C Edit combined operation description or scheduled air description.
          tcname=oprdesc(icomp)
          CALL EASKS248(tcname,' ','Zone operation notes:',
     &          72,'base_case','operation notes',IER,nbhelp)
          if(tcname(1:2).ne.'  ')oprdesc(icomp)=tcname
          MODOPR=.true.
        else
          tcname=ventdesc(icomp)
          CALL EASKS248(tcname,' ','Zone scheduled air notes:',
     &          72,'base_case','operation notes',IER,nbhelp)
          if(tcname(1:2).ne.'  ')ventdesc(icomp)=tcname
          MODOPR=.true.
        endif

      ELSEIF(INO.EQ.4)THEN

C Edit the casual documentation
        if(ip3ver(icomp).ge.21)then
          tcname=oprdesc(icomp)
          CALL EASKS248(tcname,' ','Zone casual gains notes:',
     &          72,'base_case','operation notes',IER,nbhelp)
          if(tcname(1:2).ne.'  ')oprdesc(icomp)=tcname
          MODOPR=.true.
        endif
      ELSEIF(INO.EQ.7)THEN

C Edit air flows depending on the file version.
        if(ip3ver(icomp).le.21)then
          CALL EDAIR2(ICOMP,IER)
        elseif(ip3ver(icomp).ge.30)then
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

    8   CALL EASKS(LTMP,' ','Zone operations file name?',
     &        72,GFILE,'operations file',IER,nbhelp)
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
            helptopic='operations_save_error'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK('Problem encountered saving data!',
     &      'Try again?',OK,DOK,nbhelp)
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
        helptopic='operations_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('operation help',nbhelp,'-',0,0,IER)
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
#include "schedule.h"

      DIMENSION IVALS(3), ALTS(3)
      CHARACTER hold*40,outs*80,outs2*80
      CHARACTER ALTS*31
      integer iwhich,iwhich2  ! for user choices
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDAFCTL'  ! set for subroutine
       
C Flow control browse & editing.
      helptopic='zone_flow_menu_summary'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC('Control of scheduled air flows:',' ',
     &  'none','thermostatic control','wind-speed control',iwhich,
     &   nbhelp)
      if(iwhich.eq.1)then
        ITCTL(ICOMP)=0
        THI(ICOMP)=100.
        TUP(ICOMP)=0.
        TLO(ICOMP)=0.
        ctlstr(ICOMP,1)='no control of air flow  '
        ctlstr(ICOMP,2)='no control of air flow  '
        ctlstr(ICOMP,3)='no control of air flow  '
        return
      elseif(iwhich.eq.2)then
        CALL EASKABCD('Thermostatic control options:',' ',
     &    'zone air T','other zone air T','ambient air T',
     &    'Cancel',iwhich2,nbhelp)
        if(iwhich2.eq.1)then
           ITCTL(icomp)=1
        elseif(iwhich2.eq.2)then
           ITCTL(icomp)=2
        elseif(iwhich2.eq.3)then
           ITCTL(icomp)=3
        elseif(iwhich2.eq.4)then
          return
        endif
      elseif(iwhich.eq.3)then
        ITCTL(icomp)=4 ! wind velocity control
      endif

      ALTS(1)='infiltration only'
      ALTS(2)='ventilation only'
      ALTS(3)='infiltration & ventilation'
      IX=1
   12 CALL EPICKS(IX,IVALS,' ','Air flow control scope:',
     &  31,3,ALTS,'air flow scope',IER,nbhelp)

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
      if(ip3ver(icomp).le.21)then
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
      elseif(ip3ver(icomp).ge.30)then

C << to be done >>

      endif

 100  WRITE(HOLD,'(3f8.2,a)')TLO(ICOMP),TUP(ICOMP),THI(ICOMP),'  '
      CALL EASKS(HOLD,' ',
     &  'Lower, middle & high set points (see help)?',
     &  40,' 15.0  22.0  26.0  ','flow stages ',IER,nbhelp)
      K=0
      CALL EGETWR(HOLD,K,VALL,-10.,200.,'F','low',IER)
      CALL EGETWR(HOLD,K,VALU,VALL,200.,'F','middle',IER)
      CALL EGETWR(HOLD,K,VALH,VALU,200.,'F','high',IER)
      if(ier.ne.0)goto 100
      TLO(ICOMP)=VALL
      TUP(ICOMP)=VALU
      THI(ICOMP)=VALH

      if(ISCOPE.EQ.1.or.ISCOPE.EQ.3)then

C If the scope is infiltraiton only or both then ask for all three ranges
C of infiltraiton.

 101    WRITE(HOLD,'(3f8.2,a)')ACIL(ICOMP),ACIU(ICOMP),ACIH(ICOMP),'  '
        CALL EASKS(HOLD,' ',
     &    'Lower, middle & high infiltration rates (see help)?',
     &    40,' 0.1  1.0  2.0  ','flow rates',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,VALL,0.,100.,'F','low ach',IER)
        CALL EGETWR(HOLD,K,VALU,VALL,100.,'F','middle ach',IER)
        CALL EGETWR(HOLD,K,VALH,VALU,100.,'F','high ach',IER)
        if(ier.ne.0)goto 101
        ACIL(ICOMP)=VALL
        ACIU(ICOMP)=VALU
        ACIH(ICOMP)=VALH
        if(ISCOPE.eq.1) return
      endif

c Define ventilation air flow when the sensed variable is below
c the lower range.
      CALL EASKABC('Low range ventilation options:',' ',
     &  'none','from another zone','at constant temperature',j,nbhelp)
      if(j.eq.1)then
        ACVL(ICOMP)=0.
        IVL(ICOMP)=0
        TAL(ICOMP)=0.0
      elseif(j.eq.2)then
        IV=IVL(ICOMP)
        call askzone(iv,IVL(icomp),'Ventilation from which zone?',
     &    'Source Zone','-','Ventilation definition requires a zone.',
     &    ier)
        IVL(ICOMP)=IV
        VAL=ACVL(ICOMP)
        ratio = VOL(ICOMP)/VOL(IV)
        write(outs,'(a,F7.3,a)') 
     &    ' Low range ventilation. (1ac/h in current zone =',
     &     ratio,' ac/h in source zone.)'
        write(outs2,'(a,F8.2,a,F8.2,a)') '(vol. current=',VOL(icomp),
     &    ' vol. source=',VOL(iv),')'
        CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'lwr vnt',
     &    IER,nbhelp)
        ACVL(ICOMP)=VAL
        TAL(ICOMP)=0.0
      elseif(j.eq.3)then
        IVL(ICOMP)=0
        VAL=ACVL(ICOMP)
        CALL EASKR(VAL,' ','Lower range ventilation rate?',
     &    0.0,'F',200.0,'W',1.0,'lower ventilation',IER,nbhelp)
        ACVL(ICOMP)=VAL
        V=TAL(ICOMP)
        CALL EASKR(V,' ',
     &    'Lower range ventilation source temperature?',
     &    0.0,'-',0.0,'-',18.0,'lower source t',IER,nbhelp)
        TAL(ICOMP)=V
      endif
      call usrmsg(' ',' ','-')

C Define air flow when the sensed variable is above the middle range.
      CALL EASKABC('Mid range ventilation options: ',' ',
     &  'none','from another zone','at constant temperature',j,nbhelp)
      if(j.eq.1)then
        ACVU(ICOMP)=0.
        IVU(ICOMP)=0
        TAU(ICOMP)=0.0
      elseif(j.eq.2)then
        IV=IVU(ICOMP)
        call askzone(iv,IVU(icomp),'Ventilation from which zone?',
     &    'Source Zone','-','Ventilation definition requires a zone.',
     &    ier)
        IVU(ICOMP)=IV
        VAL=ACVU(ICOMP)
        ratio = VOL(ICOMP)/VOL(IV)
        write(outs,'(a,F7.3,a)') 
     &    ' Mid-range ventil. (ac/h where 1ac/h in current =',
     &    ratio,' ac/h in source)'
        write(outs2,'(a,F8.2,a,F8.2,a)') '(vol current=',VOL(icomp),
     &    ' vol source=',VOL(iv),')'
        CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'mid vnt',
     &   IER,nbhelp)
        ACVU(ICOMP)=VAL
        TAU(ICOMP)=0.0
      elseif(j.eq.3)then
        IVU(ICOMP)=0
        VAL=ACVU(ICOMP)
        CALL EASKR(VAL,' ','Mid-range ventilation rate?',
     &    0.0,'F',200.0,'W',1.0,'mid ventilation',IER,nbhelp)
        ACVU(ICOMP)=VAL
        V=TAU(ICOMP)
        CALL EASKR(V,' ','Mid range ventilation source temperature?',
     &    0.0,'-',0.0,'-',18.0,'mid source t',IER,nbhelp)
        TAU(ICOMP)=V
      endif
      call usrmsg(' ',' ','-')

C Define air flow when the sensed variable is above the high range.
      CALL EASKABC(' ','High stage ventilation options:',
     &  'none','from another zone','at constant temperature',j,nbhelp)
      if(j.eq.1)then
        ACVH(ICOMP)=0.
        IVH(ICOMP)=0
        TAH(ICOMP)=0.0
      elseif(j.eq.2)then
        IV=IVH(ICOMP)
        call askzone(iv,IVH(icomp),'Ventilation from which zone?',
     &    'Source Zone','-','Ventilation definition requires a zone.',
     &    ier)
        IVH(ICOMP)=IV
        VAL=ACVH(ICOMP)
        ratio = VOL(ICOMP)/VOL(IV)
        write(outs,'(a,F7.3,a)') 
     &    ' High-range ventil. (ac/h where 1ac/h in current =',
     &    ratio,' ac/h in source)'
        write(outs2,'(a,F8.2,a,F8.2,a)') '(vol current=',VOL(icomp),
     &    ' vol source=',VOL(iv),')'
        CALL EASKR(VAL,outs,outs2,0.0,'F',200.0,'W',1.0,'high vent',
     &    IER,nbhelp)
        ACVH(ICOMP)=VAL
        TAH(ICOMP)=0.0
      elseif(j.eq.3)then
        IVH(ICOMP)=0
        VAL=ACVH(ICOMP)
        CALL EASKR(VAL,' ','High-range ventilation rate?',
     &    0.0,'F',200.0,'W',1.0,'high ventilation',IER,nbhelp)
        ACVH(ICOMP)=VAL
        V=TAH(ICOMP)
        CALL EASKR(V,' ',
     &    'High range ventilation source temperature?',
     &    0.0,'-',0.0,'-',18.0,'hi source t',IER,nbhelp)
        TAH(ICOMP)=V
      endif
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
#include "schedule.h"
#include "epara.h"
      
C      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character vert(35)*33,KEY*1,title*33
      character LTMP*72,louts*248
      character afact*8,iflow*8,vflow*8,outs*124,sfile*72,snpfile*72
      logical XST,closezero,closeone,closetwo
      integer KSTR(MDTY),ix,iy
      integer IW  ! for radio button
      integer MVERT,IVERT ! max items and current menu item
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDAIR2'  ! set for subroutine
      
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
      helptopic='air_flow_period_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
        helptopic='air_flow_period_setup'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('flows menu',nbhelp,'-',0,0,IER)
      elseif (IVERT.EQ.(MVERT-2)) then

C List current.
        call VENTINF(ICOMP,IUOUT)
      elseif (IVERT.EQ.(MVERT-3)) then

C Add delete copy period or import from another zone.
        helptopic='air_flow_period_copy'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IW=1
        CALL EASKATOG(' ','Options (see help):',
     &    'add period','delete period','copy period',
     &    'import from other zone','import pattern','cancel',' ',
     &    IW,nbhelp)

C Add period
        if (IW.EQ.1) then 
          CALL EASKAB(' ','Add period to ','one day type',
     &     'all day types',IY,nbhelp)
          IF(IY.EQ.1)THEN
            IX=1
            CALL EPICKS(IX,KSTR,' ','choose daytype to add period in'
     &      ,12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
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

C Display contents of the other zone file.
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
                write(louts,*) 'you selected ',sfile(1:lnblnk(sfile))
                call edisp248(iuout,louts,100)
                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 5
              endif

C Display contents of this browsed file.
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
     &     'infiltration only','cancel',ivopt,nbhelp)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'convert to zone volume','cancel',infv,nbhelp)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'convert to zone vvolume','cancel',ivntv,nbhelp)
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
#include "schedule.h"
#include "epara.h"
      
C      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL

C Common block P2UNIT defines the units for infiltration and ventilation
C for each zone and day type. The default value of zero is the same as
C the traditional (implied) unit in the version (ip3ver 0,1,2,21,30). A value
C of one signals m3/h and a value of 2 signals m3/s.
      integer INFUNIT,IVENTUNIT
      COMMON/P2UNIT/INFUNIT(MCOM,MDTY),IVENTUNIT(MCOM,MDTY)
      
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character vert(35)*33,KEY*1,title*33
      character LTMP*72,louts*248
      character afact*8,iflow*8,vflow*8,outs*124,sfile*72,snpfile*72
      logical XST,closezero,closeone,closetwo
      integer KSTR(MDTY)
      integer IW   ! for radio button
      integer MVERT,IVERT ! max items and current menu item
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDAIR3'  ! set for subroutine
      
C Return if the version is not three.
      if(ip3ver(icomp).lt.30)then
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
      helptopic=' air_flow_period_setup'
      call gethelptext(helpinsub,helptopic,nbhelp)

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

C Add delete copy period or import air schedules from another zone or from a
C file in the pattern folder.
        helptopic='air_flow_period_copy'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IW=1
        CALL EASKATOG(' ','Options (see help): ',
     &    'add period','delete period','copy period',
     &    'import from other zone','import from pattern','cancel',' ',
     &    IW,nbhelp)

C Add period
        if (IW.EQ.1) then 
          CALL EASKAB(' ','Add period to ','one day type',
     &     'all day types',IY,nbhelp)
          IF(IY.EQ.1)THEN
            IX=1
            CALL EPICKS(IX,KSTR,' ','choose daytype to add period in'
     &      ,12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
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

C Display contents of this other zone file.
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

C File browse from patterns folder.
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
                write(louts,*) 'you selected ',sfile(1:lnblnk(sfile))
                call edisp248(iuout,louts,100)
                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again.',' ','W')
                goto 5
              endif

C Display contents of this browsed file.
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
     &     'infiltration only','cancel',ivopt,nbhelp)
          if(ivopt.eq.1) afact='all  '
          if(ivopt.eq.2) afact='infil'
          if(ivopt.eq.3) goto 5
          CALL EASKABC(' ','Infiltration import:','preserve ac/h',
     &     'convert to zone volume','cancel',infv,nbhelp)
          if(infv.eq.1) iflow='ach-ach '
          if(infv.eq.2) iflow='vol-vol '
          if(infv.eq.3) goto 5
          if(ivopt.eq.1)then
            CALL EASKABC(' ','Ventilation import:','preserve ac/h',
     &       'convert to zone vvolume','cancel',ivntv,nbhelp)
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
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character OUTS*124,HOLD*16

C Working variables
      real ACIX,ACVX,TAX,XTA,XACH
      integer IACSX,IACFX,IPTX
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDAIRFL'  ! set for subroutine

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

C Help for the dialogs.
      helptopic='operations_air_details'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Ask for start and finish times.
      if(itype.eq.-1.or.itype.eq.1)then
 10     write (OUTS,'(A)') 'Start and finish hours for period?'
        write (HOLD,'(2i5,a)') IACSX,IACFX,'  '
        CALL EASKS(HOLD,OUTS,' ',16,' 0 24 ','air st-fn hr',IER,nbhelp)
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
        IGU=1
        CALL EASKAB(' ','Air flow specified in:','AC/h',
     &     'm^3/s',IGU,nbhelp)
        if (IGU.eq.1) then

          if(itype.eq.-1.or.itype.eq.2)then
C Flow as ac/h.
 20         XACH=ACIX
            call EASKR(XACH,' ','Infiltration rate ac/h ?',
     &        0.0,'W',10.0,'W',0.5,'infil rate ach',IERI,nbhelp)
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
            call EASKR(XACH,' ','Ventilation ac/h rate?',
     &        0.0,'W',10.0,'W',0.0,'vent rate ach',IERI,nbhelp)
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
            call EASKR(XACH,' ','Infiltration rate m^3/s ?',
     &        0.0,'W',10.0,'W',0.0,'inf rate m3s',IERI,nbhelp)
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
            call EASKR(XACH,' ','Ventilation rate m^3/s ?',
     &        0.0,'W',10.0,'W',0.0,'vent rate m3s',IERI,nbhelp)
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
     &      'ambient',IGU,nbhelp)
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
            call EASKR(XTA,' ','Supply temperature (degC) ?',
     &        0.0,'F',50.0,'W',0.5,'vent src temp',IERI,nbhelp)
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
#include "schedule.h"

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
#include "schedule.h"

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      character VERT(3*MA)*35,OUTS*124,OUT*4
      INTEGER IGOON,ILET,IDT,IDTY,IAFP,IX,KSTR(MDTY),ilistlen,ilm

      dimension IVERT(3*MA),ITEMP(3*MA)
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CDAIRFL'  ! set for subroutine

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

      helptopic='operations_copy_flow'
      call gethelptext(helpinsub,helptopic,nbhelp)

C To delete period(s) redisplay the menu and allow user to indicate
C which one, then collapse the period list.
      if (ICD.eq.1) then
        call EPICKS(ilistlen,IVERT,' ','Air flow period(s) to delete:',
     &         35,M,VERT,'Air flow period to delete',IER,nbhelp)

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
        IX=1
        CALL EDISP(IUOUT,'choose daytype to copy periods from')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods from',
     &    12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
        IDTY=KSTR(1)
        CALL EDISP(IUOUT,'choose daytype to copy periods to')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods to',
     &    12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
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
 210    continue
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
#include "schedule.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      CHARACTER oprdescc*248,ventdescc*248,ctlstrc*24
      COMMON/P1C/oprdescc,ventdescc,ctlstrc
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Scratch copy of air flow pattern from an operations file.
      COMMON/P2NC/NACC(MDTY),IACSC(MDTY,MA),IACFC(MDTY,MA),
     &ACIC(MDTY,MA),ACVC(MDTY,MA),IPTC(MDTY,MA),TAC(MDTY,MA)
      INTEGER NACC,IACSC,IACFC,IPTC
      REAL ACIC,ACVC,TAC

      COMMON/P2CTLC/ITCTLC,TLOC,TUPC,THIC,ACILC,ACVLC,IVLC,TALC,
     &   ACIUC,ACVUC,IVUC,TAUC,ACIHC,ACVHC,IVHC,TAHC

      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

      character*(*) sfile
      character ltmp*72
      character fs*1,GFILE*72,afact*8,iflow*8,vflow*8
      character act*1   ! s for silent, - for normal
      LOGICAL XST,unixok
      INTEGER IDTY
      character actflatfile*1  ! parameter to pass
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='COPYAIRFL'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      helptopic='copy_air_via_import'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
      ventdesc(idest)='Flow schedules imported. No other notes (yet).'

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
          CALL EASKS(ltmp,' New zone operations file: (confirm) ',
     &      ' ',72,GFILE,'new operations file',IER,nbhelp)
          LPROJ(idest)=ltmp
        endif

C And any new zone operation file will use the sorted format.
        ip3ver(idest)=2
      endif
      if(iflow(1:7).eq.'ach-ach')then
        ratioif=1.0
      elseif(iflow(1:7).eq.'vol-vol')then

C Copy the local array data into the common blocks overwriting
C the data implied by 'afact'. Also act on the iflow vflow directives.
        if(isource.eq.0)then
          VAL=10.0
          CALL EASKR(VAL,' ','Assumed volume of source?',
     &        0.10,'F',9999.0,'W',0.0,'assumed volume',IER,nbhelp)
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
#include "schedule.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

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
#include "schedule.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      INTEGER IDTY,IDAY,JDAY,KDAY

      character outs*124
      dimension TACI(24),TACV(24)
#ifdef OSI
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2,iglsat,iglsun
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2,iglsat,iglsun
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
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

C Setup and pass in parameters to win3d.
      iiw1=10; iiw2=10; iiw3=8; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
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
      call vrtaxis(YMIN,YMAXF,iigr,iigb,iigt,yadd,ysc,0,1,
     &  'Flow (m^3/s)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAX,
     &                                         xsc,ysc,sca,xadd,yadd)

C Scale and draw for first day type requested
      CALL horaxis(plxmin,plxmax,iigl,iigr,iigb,xadd,xsc,0,
     &  'Time (hrs)')
      call vrtaxis(YMIN,YMAX,iigl,iigb,iigt,yadd,ysc,0,0,
     &  'Flow (ac/hr)')
      call linescale(iigl,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)

C Scale and draw second day type requested
      CALL horaxis(plxmin,plxmax,iglsat,iigr,iigb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMIM,iupdown,isym)

C Scale and draw third day type requested
      CALL horaxis(plxmin,plxmax,iglsun,iigr,iigb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmin,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmax,YMAX,iupdown,isym)

C Display air flow curves.
C first day type requested
      call linescale(iigl,xadd,xsc,iigb,yadd,ysc)
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
      call linescale(iglsat,xadd,xsc,iigb,yadd,ysc)
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
      call linescale(iglsun,xadd,xsc,iigb,yadd,ysc)
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
      iid1=igl+100; iid2=igt-69;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=0
      isym=0
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igl+180; iid2=igt-69;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=1
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igr-120; iid2=igt-69;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=0
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igr-180; iid2=igt-69;
      call pixel2u(iid1,iid2,gx,gy)
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
#include "schedule.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/user/browse

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C How many of each gain type is there each zone/gaintype/daytype.
      common/loadcnt/loadcount(mcom,MGTY,MDTY),
     &  loadm2count(mcom,MGTY,MDTY),loadmixed(mcom,MGTY,MDTY)
      logical browse,ok,silent,dok,XST
      logical sorted,modopr
      integer ICGTS,IW  ! casual slot for radio buttons
      integer MVERT,IVERT ! max items and current menu item

      CHARACTER vert(35)*36,outs*144,KEY*1,sfile*72,snpfile*72
      character typstr*14,caskeyt*16
      character title*28,louts*248
      character*12 ll1,ll2,ll3,ll4

C iwatt (8 char) = 'W-W' for absolute W values preserves value
C   e.g. 123W in source zone is also 123W in destination zone.
C iwatt= 'W-m2-W' preserves implied W/m^2 base area implied by
C   absolute W in source zone in destination zone which has a
C   different base area. e.g. 123W in source zone of 2.3m^2 becomes
C   267W in a 5.0m^2 base zone.
      character act*8,iwatt*8
      integer kstr,ILLEN,IQ,IX,I,IDU,IDT,IOPT,IDTYP
      dimension kstr(MDTY),ILLEN(MDTY)
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDCAS'  ! set for subroutine

C If not browsing, take the information from loadcnt
C and decide if the periods need to be updated.
      if(.NOT.browse)then
        if(ip3ver(ICOMP).eq.0)then
          dok=.true.
          helptopic='operations_warn_overlap'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(' ','Sort and remove overlaps from casual gains?',
     &                 OK,DOK,nbhelp)
        else
          ok=.false.
        endif
        if(OK)then

C Display initial gain profiles and then sort and remove overlaps and
C ensure that periods start at zero and end with 24. Sorted files can
C be saved as version 2.0
C          call edisp(iuout,'Before sorting')
          call CASINF(ICOMP,iuout)
          if(ip3ver(ICOMP).eq.0)then
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru)
          else
            itrc=0
            itru=6
            call PROCESSOLDCAS(ICOMP,itrc,itru)
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
        VERT(5) = ' Period  Label    Unit Power  p.f.'
      ELSE
        VERT(2) = '2 electrical data >> not included'
        VERT(5) = ' Period  Label    Unit Sensib Latent'
      ENDIF

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD

C Loop for each casual gain to make up menu entries (taking into 
C account ICGUnit and IELF attributes.
      DO 10 L=1,NCAS(IDTYP)
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
        M=M+1
        CALL EMKEY(M-MHEAD,KEY,IER)
        if(ICGUnit(IDTYP,L).eq.0)then
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),304)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &          lodlabel(icomp,jicgt),' W  ',PWR(IDTYP,L),PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &          lodlabel(icomp,jicgt),' W   no power'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' W  ',CMGS(IDTYP,L),
     &        CMGL(IDTYP,L)
          endif
  303     FORMAT(a,i3,i3,1x,2a,2f6.0)
  304     FORMAT(a,i3,i3,1x,2a,f6.0,f5.1)
  313     FORMAT(a,i3,i3,1x,2a)
        elseif(ICGUnit(IDTYP,L).eq.2)then
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' m2p',PWR(IDTYP,L),
     &        PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' m2p no power'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' m2p',CMGS(IDTYP,L),CMGL(IDTYP,L)
          endif
        elseif(ICGUnit(IDTYP,L).eq.3)then  ! for dynamicpeople
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' ocu',PWR(IDTYP,L),
     &        PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' ocu no power'
            endif
          else
            write (VERT(M),303)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' ocu',tnbmen(IDTYP,L),
     &        metabolic(IDTYP,L)
          endif
         elseif(ICGUnit(IDTYP,L).eq.1)then
          jicgt=iabs(ICGT(IDTYP,L))
          if (IELF(ICOMP).EQ.1) then
            if (PWR(IDTYP,L).gt.0.0001) then
              write (VERT(M),304)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' Wm2',PWR(IDTYP,L),
     &        PF(IDTYP,L)
            else
              write (VERT(M),313)KEY,ICGS(IDTYP,L),ICGF(IDTYP,L),
     &        lodlabel(icomp,jicgt),' Wm2 no power'
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
      VERT(M+2)  ='@ edit casual gain labels:types'
      VERT(M+3)  ='+ add/delete/copy/import gains '
      VERT(M+4)  ='* scale existing gains         '
      if(ip3ver(icomp).ge.21)then
        VERT(M+5)='# Not applicable               '
      else
        VERT(M+5)='# check/remove overlaps        '
      endif
      VERT(M+6)  ='! list current information     '
      VERT(M+7)  ='? help                         '
      VERT(M+8)  ='- exit this menu               '

C Help text for this menu.
      helptopic='operations_casual_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      title = ' '
      if(mmod.eq.8)then
        write(title,'(2a)') 'Casual gains in ',zname(ICOMP)
      else
        write(title,'(2a)') 'Gains: ',zname(ICOMP)(1:lnzname(ICOMP))
      endif
      CALL EMENU(title,VERT,MVERT,IVERT)
      if (IVERT.EQ.1)THEN

C Import profile.
        IX=1
        CALL EPICKS(IX,KSTR,' ','Import a profile for which day type?',
     &  12,NBDAYTYPE,CALENTAG,
     &  'Import a profile for which day type?',IER,nbhelp)
        IW=KSTR(1)
        IN=NCAS(IW)
        CALL SCASL(IN,IW,IER,ICOMP)
        sorted=.false.
      elseif (IVERT.eq.2) then

C Include/ exclude electrical definitions.
        if(IELF(ICOMP).EQ.0.or.IELF(ICOMP).EQ.2) then
          call easkabc('Do you wish to include electrical data', 
     &      'in the operations description?','Yes','No','Cancel',
     &      IW,nbhelp)
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
          call easkab('Do you wish to remove electrical data ', 
     &    'from the operations file?','Yes','No',IW,nbhelp)
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

C List current casual gains.
        call CASINF(ICOMP,IUOUT)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Check & remove overlaps in periods in the current day type. As the
C number of periods might have changed, rebuild the menus.
        ILLEN(IDTYP)=NCAS(IDTYP)
        call edisp(iuout,'Before sorting')
        call CASINF(ICOMP,iuout)
        call PROCESSOLDCAS(ICOMP,0,iuout)
        if(ier.ne.0)then
          call edisp(iuout,'Problem while sorting gains.')
        endif
        sorted=.true.
        modopr=.true.

C If different period, update the menu length and also need to update
C the menu strings.
        if(ILLEN(IDTYP).ne.NCAS(IDTYP))
     &  call edisp(iuout,'Periods have changed.')
        ILEN=NCAS(IDTYP)
        goto 44

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Offer casual gains to scale. User can cancel request.
        ll1='-'; ll2='-'; ll3='-'; ll4='-'
        if(lodslot(icomp,1).gt.0) write(ll1,'(a)') lodlabel(icomp,1)
        if(lodslot(icomp,2).gt.0) write(ll2,'(a)') lodlabel(icomp,2)
        if(lodslot(icomp,3).gt.0) write(ll3,'(a)') lodlabel(icomp,3)
        if(lodslot(icomp,4).gt.0) write(ll4,'(a)') lodlabel(icomp,4)
        ICGTS=1
        CALL EASKATOG('Apply scaling factor to which casual gain?',
     &     ' ',ll1,ll2,ll3,ll4,
     &     'All gains','Cancel ',' ',ICGTS,nbhelp)
        if (ICGTS.eq.6) goto 5  ! cancel requested
        if (ICGTS.ne.5)then
           write (outs,'(3a)') 'Scale ',lodlabel(icomp,ICGTS),
     &                         ' gain for day type: '
           call edisp(iuout,outs)
        endif
        call EASKABC('Import profile for one or all day types',
     &    ' ','One day type','All day types','Cancel',IOPT,nbhelp)
        IF(IOPT.EQ.1)THEN
          IW=0
          IX=1
          CALL EPICKS(IX,KSTR,' ',
     &      'Import a profile for which day type?',12,NBDAYTYPE,
     &      CALENTAG,'Import a profile for which day type?',IER,nbhelp)
          IW=KSTR(1)
          write (outs,'(2a)') outs(1:lnblnk(outs)),CALENTAG(IW)
        ELSEIF(IOPT.EQ.2)THEN
          IW=0
          write (outs,'(2a)') outs(1:lnblnk(outs)),'ALL'
        ELSE
          GOTO 5  ! cancel request
        ENDIF
        CALL EASKR(VAL,outs,'(typically between 0.1 and 5.0)',
     &    0.1,'W',5.0,'W',1.0,'scaling factor',IER,nbhelp)

C Loop through each day type and gain and factor matching values.
        DO 760 IDTY=1,NBDAYTYPE
          do 33 i=1,NCAS(IDTY)
            IF(IDTY.EQ.IW.OR.IW.EQ.0)THEN
              if(IABS(ICGT(IDTY,I)).eq.ICGTS)then
                CMGS(IDTY,I)=CMGS(IDTY,I)*VAL
                CMGL(IDTY,I)=CMGL(IDTY,I)*VAL
              endif
            ENDIF
 33       continue
 760    CONTINUE
        goto 5

      elseif (IVERT.eq.(MVERT-5)) then

C Add delete copy period or import full set of casual gains.
        IW=1
        CALL EASKATOG('Casual gain period options:(see help)','  ',
     &    'add period','delete period','copy period',
     &    'import from other zone','import from pattern ','cancel',
     &    ' ',IW,nbhelp)
        if (IW.EQ.1) then 

C Add new casual gain period.
          ll1='-'; ll2='-'; ll3='-'; ll4='-'
          if(lodslot(icomp,1).gt.0) write(ll1,'(a)') lodlabel(icomp,1)
          if(lodslot(icomp,2).gt.0) write(ll2,'(a)') lodlabel(icomp,2)
          if(lodslot(icomp,3).gt.0) write(ll3,'(a)') lodlabel(icomp,3)
          if(lodslot(icomp,4).gt.0) write(ll4,'(a)') lodlabel(icomp,4)

C << revise to offer selection of day types to apply new periods >>
C << should be a menu which pairs the current user labels and types
C << with the option to add another slot (label:type).

C Check to see what the current units for for the selected
C type and echo which type the user selected (easily forgotten)
          CALL EASKATOG('New casual period for...','Confirm:',
     &      ll1,ll2,ll3,ll4,'Cancel',' ',' ',ICGTS,nbhelp)
          if (ICGTS.eq.5) goto 5  ! cancel request
          write(outs,'(5a)') 'Requested a new ',
     &      lodlabel(icomp,icgts),' ',caskeytype(icomp,icgts),
     &      ' period. Please supply details...'
          call edisp(iuout,outs)
          caskeyt=caskeytype(icomp,icgts)  ! remember key word
          IQ=0
          if(.NOT.browse)call checkcascount(icomp)
          call EASKABC('Add period for one or all day types',
     &      ' ','One day type','All day types','Cancel',IOPT,nbhelp)
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
            GOTO 5   ! cancel request
          ENDIF
          IF(IQ.EQ.0)THEN

C For all day types. 
            ITRK=0
            DO 619 IDU=1,NBDAYTYPE
              IF(NCAS(IDU).EQ.MC)THEN
                WRITE(OUTS,'(2A)')
     &            'Sorry, too many periods for day type ',CALENTAG(IDT)
                  CALL USRMSG(' ',OUTS,'W')
                ITRK=1
              ENDIF
 619        CONTINUE
            IF(ITRK.EQ.0)THEN

C Ask about units prior to calling EDCASGN and reset icgts & icgus if required.
              igu=1; icgus=0
              CALL EASKATOG('For new gains the typical units will be',
     &          'Confirm: ','Watts','Watts/m2','objects/m2->Watts',
     &          'Square meters per object','Dynamic People',
     &          ' ',' ',IGU,nbhelp)
              if(IGU.eq.1)then
                icgus=0
              elseif(IGU.eq.2)then
                icgus=1
                ICGTS= (-1*ICGTS)  ! invert the selection slot index
              elseif(IGU.eq.3)then
                icgus=0    ! not yet available so use Watts
              elseif(IGU.eq.4)then
                icgus=2
                ICGTS= (-1*ICGTS)  ! invert the selection slot index
              elseif(IGU.eq.5)then
                icgus=3  ! number of peple
              endif
              IDT=-1
              IPER=0
              silent=.false.
              izs=0
              izf=24
              call EDCASGN(ICOMP,ICGTS,icgus,caskeyt,IDT,IPER,silent,
     &          izs,izf)
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

C If there are no loads using this slot make a default one for the whole
C day and then a (suggested) 8-18 period within. Ask about units prior
C to calling EDCASGN.
                igu=1
                CALL EASKATOG('For new gains the typical units will be',
     &          'Confirm: ','Watts','Watts/m2','objects/m2->Watts',
     &          'Square meters per object','Dynamic People',
     &          ' ',' ',IGU,nbhelp)
                if(IGU.eq.1)then
                  icgus=0
                elseif(IGU.eq.2)then
                  icgus=1
                  ICGTS= (-1*ICGTS)  ! invert the selection slot index
                elseif(IGU.eq.3)then
                  icgus=0    ! not yet available so use Watts
                elseif(IGU.eq.4)then
                  icgus=2
                  ICGTS= (-1*ICGTS)  ! invert the selection slot index
                elseif(IGU.eq.5)then
                  icgus=3  ! number of peple
                endif
                silent=.true.
                izs=0
                izf=24
                call EDCASGN(ICOMP,ICGTS,icgus,caskeyt,IDT,IPER,
     &            silent,izs,izf)
              endif
              silent=.false.
              izs=8
              izf=18
              call EDCASGN(ICOMP,ICGTS,icgus,caskeyt,IDT,IPER,
     &          silent,izs,izf)
            ENDIF
          ENDIF
          sorted=.false.
          modopr=.true.
          goto 5

        elseif (IW.EQ.2) then

C Delete a casual gain.
          call CDCASGN(ICOMP,1)
          sorted=.false.
          modopr=.true.
          goto 5

        elseif (IW.EQ.3) then

C Copy a casual gain.
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
                write(louts,*) 'you selected ',sfile(1:lnblnk(sfile))
                call edisp248(iuout,louts,100)
                isource=0
              else

C Nothing selected treat this as a return state.
                call usrmsg('No file selected, try again...',' ','W')
                goto 5
              endif

C Display contents of this browsed file.
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
          CALL EASKABC('Casual import:',' ','preserve Watts or W/m2',
     &      'adjust Watts for different base area','cancel import',
     &      ivntv,nbhelp)
          if(ivntv.eq.1) iwatt='W-W    '
          if(ivntv.eq.2) iwatt='W-m2-W '
          if(ivntv.eq.3) goto 5
          idest=icomp
          act='allcas'
          itrc=1
          call IMPORTZCASG(isource,idest,act,iwatt,sfile,itrc,'-',ier)

C Remind users to check their data.
          CALL PHELPD('operation import confirm',nbhelp,'-',0,0,IER)
        elseif(iw.eq.-3)then
          goto 44  ! re-display the menu
        endif

      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C Provide editing space (12 char) for the  char string. For identified slot
C ask for user label and confirmation of the type of gain. Use a dash if
C a slot has not been used yet.
        ll1='-'; ll2='-'; ll3='-'; ll4='-'
        if(lodslot(icomp,1).gt.0) write(ll1,'(a)') lodlabel(icomp,1)
        if(lodslot(icomp,2).gt.0) write(ll2,'(a)') lodlabel(icomp,2)
        if(lodslot(icomp,3).gt.0) write(ll3,'(a)') lodlabel(icomp,3)
        if(lodslot(icomp,4).gt.0) write(ll4,'(a)') lodlabel(icomp,4)
        helptopic='operations_casual_labels'
        call gethelptext(helpinsub,helptopic,nbhelp)
        dok=.false.
        islot=1
        CALL EASKATOG('Edit user label and type for...',' ',
     &     ll1,ll2,ll3,ll4,'Add another','Cancel',' ',islot,nbhelp)
        if (islot.eq.6) goto 5  ! cancel requested
        if (islot.eq.5)then

C Assign the next available slot.
          if(lodslot(icomp,4).eq.0) islot=4
          if(lodslot(icomp,3).eq.0) islot=3
          if(lodslot(icomp,2).eq.0) islot=2
          if(lodslot(icomp,1).eq.0) islot=1
          call initcasday21(icomp,islot,ier)
          ip3ver(icomp)=21  ! upgrade file to remember types
          modopr=.true.
          goto 5
        else
          write(outs,'(a,i1,2a)') 'Label for gains in slot ',islot,
     &      ' & type ',caskeytype(icomp,islot)
          write(TYPSTR,'(2a)') lodlabel(icomp,islot),'  '
          call easks(TYPSTR,outs,' ',12,
     &      lodlabel(icomp,islot),'Gain label',IER,2)
          write(lodlabel(icomp,islot),'(a)') TYPSTR(1:12)

C Type to go with this casual gain.
          idno=islot
          isw=0
          call MENUATOL('  ','Available types',
     &      'a -        not yet defined W & W/m2',
     &      'b people   basic occupant W & m2/p',
     &      'c lighting basic lighting W & W/m2',
     &      'd equipment basic small power W & W/m2',
     &      'e other    non-specific gains W & W/m2',
     &      'f dynamicpeople nb people & metabolic',
     &      'g net_utilities as legacy icgtyp = 5',
     &      'h ALOtherElectric H3K other appliances',
     &      'i ALStoveNG H3K natural gas stoves',
     &      'j ALStoveElectric H3K electrical stoves',
     &      'k ALDryer H3K clothes dryers',
     &      ' ',isw,idno,nbhelp)
          if(isw.eq.1)then
            caskeytype(icomp,islot)='-           '
          elseif(isw.eq.2)then
            caskeytype(icomp,islot)='people      '
          elseif(isw.eq.3)then
            caskeytype(icomp,islot)='lighting    '
          elseif(isw.eq.4)then
            caskeytype(icomp,islot)='equipment   '
          elseif(isw.eq.5)then
            caskeytype(icomp,islot)='other       '  
          elseif(isw.eq.6)then
            caskeytype(icomp,islot)='dynamicpeople'  
          elseif(isw.eq.7)then
            caskeytype(icomp,islot)='net_utilities'  
          elseif(isw.eq.8)then
            caskeytype(icomp,islot)='ALOtherElectric'  
          elseif(isw.eq.9)then
            caskeytype(icomp,islot)='ALStoveNG   '  
          elseif(isw.eq.10)then
            caskeytype(icomp,islot)='ALStoveElectric'  
          elseif(isw.eq.11)then
            caskeytype(icomp,islot)='ALDryer     '  
          endif
          lodslot(icomp,islot)=islot; lodatr1(icomp,islot)=iatr1;
          lodatr2(icomp,islot)=0
          ip3ver(icomp)=21  ! upgrade file to remember types
          modopr=.true.
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

C Edit casual gain identified by KEYIND. Set local variables to pass to
C edcasgn (commons are updated within edcasgn if the user makes changes).
        CALL KEYIND(MVERT,IVERT,I,IO)
        IDT=IDTYP
        IPER=I
        ICGTX=ICGT(IDT,IPER)
        ICGUX=ICGUnit(IDT,IPER)
        caskeyt=caskeytype(icomp,iabs(icgtx))
        silent=.false.
        izs=0
        izf=0
        call EDCASGN(ICOMP,ICGTX,ICGUX,caskeyt,IDT,IPER,silent,
     &    izs,izf)
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
C << as multiple casual gain slots

C << also consider that periods can be defined as reals for later
C << use in the zone operation file timings

#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "profile.h"
#include "schedule.h"

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Profile database commons via profile.h.

      DIMENSION  DESC(MGIT),IPRVAL(MGIT)
      CHARACTER*40 DESC
      CHARACTER hold*40
      character*12 ll1,ll2,ll3,ll4
      character lltmp*144
      character fs*1  ! file separator
      logical OK,DOK

      integer IPPFT,IPPHAST  ! for local editing.
      logical unixok  ! to check for database path file separators
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SCASL'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Labels for loads.
      ll1='-'; ll2='-'; ll3='-'; ll4='-'
      if(lodslot(icomp,1).gt.0) write(ll1,'(a)') lodlabel(icomp,1)
      if(lodslot(icomp,2).gt.0) write(ll2,'(a)') lodlabel(icomp,2)
      if(lodslot(icomp,3).gt.0) write(ll3,'(a)') lodlabel(icomp,3)
      if(lodslot(icomp,4).gt.0) write(ll4,'(a)') lodlabel(icomp,4)

C Initial assumptions.
      ICGTX=1
      PS=0.0; PL=0.; RAD=0.5; CON=0.5; IPPFT=0; IPPF=0

C Scan the event profiles file into common blocks.
      lndbp=lnblnk(standarddbpath)
      if(ipathprodb.eq.0.or.ipathprodb.eq.1)then
        lltmp=LPRFDB  ! use as is
      elseif(ipathprodb.eq.2)then
        write(lltmp,'(3a)') standarddbpath(1:lndbp),fs,
     &    LPRFDB(1:lnblnk(LPRFDB))  ! prepend db folder path
      endif
      ier=0   ! reset prior to call to triprocom
      CALL ERPFREE(IPRODB,ISTAT)
      call TRIPROCOM(IPRODB,lltmp,IER)
      if(ier.ne.0)then
        call usrmsg('Error opening the db. Please use one',
     &              'of the other options.','W')
        return
      else
        CALL ERPFREE(IPRODB,ISTAT)
      endif
      
C Copy the string list of event profiles to local string variable.
      DO 50 I = 1,NPDBITEMS
        DESC(I)=PDBDESC(I)
   50 CONTINUE

      helptopic='operations_event_import'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Present a list of profiles to select from.
      IX=1
      NITEMS=NPDBITEMS
      CALL EPICKS(IX,IPRVAL,' ','Profiles in events db: ',
     &         40,NITEMS,DESC,'db profiles',IER,nbhelp)
      IF(IX.EQ.0)RETURN

      INDEX=IPRVAL(1)

C Identify which casual gain.
      CALL EASKABCD(' ','Which casual gain: ',ll1,ll2,ll3,ll4,ICGTX,
     &  nbhelp)

 100  WRITE(HOLD,'(2f9.0,2f8.2,a)')PS,PL,RAD,CON,'  '

C << this would be a good place to expand the logic so that lighting
C << and small power can be entered in terms of W/m2.
      CALL EASKS(HOLD,
     &  ' 100% sensible & latent loads, radiant & convective fraction:',
     &  ' ',40,' 1000   0  0.5  0.5  ','profile info',IER,nbhelp)
      K=0
      CALL EGETWR(HOLD,K,PS,0.0,999999.,'F','sensible 100%',IER)
      CALL EGETWR(HOLD,K,PL,0.0,999999.,'F','latent 100%',IER)
      CALL EGETWR(HOLD,K,RAD,0.0,1.0,'F','rad frac',IER)
      CALL EGETWR(HOLD,K,CON,0.0,1.0,'F','conv frac',IER)

      SUM = RAD + CON
      IF (SUM .LT. 0.98 .OR. SUM .GT. 1.02)then
        dok=.false.
        call askok(' The Sum of radiant and convective fractions is',
     &    ' not close to 1.0! Are you sure about this? ',OK,dok,nbhelp)
       if(.NOT.ok)goto 100
      ENDIF
      IF(IELF(ICOMP).GT.0) THEN
        dok=.false.
        helptopic='operations_electrical'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK('Include electrical data with this profile?',
     &    '(see help)',OK,dok,nbhelp)
        IF(.NOT.OK) THEN
          PPF=0.0; IPPF=0; PBVOLT=0.0;  IPPHAS=1; PE=0.0
        ELSE
          CALL EASKR(PPF,' ','Power factor? [0-1] ? ',
     &      0.,'F',1.,'F',0.8,'Power factor ',IER,nbhelp)
          ippft=ippf
          CALL EASKI(ippft,'Lagging -1, Leading 1 or Unity 0?',
     &      '(see help) ',-1,'F',1,'F',0,'casual gain lagging',IERI,
     &      nbhelp) 
          if(ieri.eq.-3)then
            PPF=0.0; IPPF=0;  PBVOLT=0.0; IPPHAS=1; PE=0.0
            goto 42
          else
            ippf=ippft
          endif

          PE=PS
          CALL EASKR(PE,' ','Power consumption? (W) ? ',
     &      0.,'F',99.,'-',PS,'Power consumption ',IER,nbhelp)

          PBVOLT=240.
          CALL EASKR(PBVOLT,' ','Operational voltage ? (W) ? ',
     &      0.,'F',99.,'-',240.,'Operational volatage ',IER,nbhelp)

          IPPHAST=1
          ipphast=ipphas
          CALL EASKI(IPPHAST,'Supply phase 1-3 or 4 ?','(see help)',
     &       1,'F',4,'F',1,'casual gain phase',IERI,6) 
          if(ieri.eq.-3)then
            PPF=0.0; IPPF=0; PBVOLT=0.0; IPPHAS=1; PE=0.0
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
C EDCASGN - edit casual gain data for one period. After user input
C the local parameters are copied into common blocks via a call to CPYCASTI.

C  ICOMP zone number
C  ICGTX casual gain slot index (still overloaded )
C  ICGUX units 0=W 1=W/m2 2=m2/person 3 numb of dynamic people
C  caskeyt  gain type key word
C  IDT day type (=-1 then all day types)
C  IPER period (=0 then new)
C  silent true if no user interaction requested.
C If silent is true then use icgs & icgf parameters.

      subroutine EDCASGN(ICOMP,ICGTX,ICGUX,caskeyt,IDT,IPER,silent,
     &  icgsX,icgfX)
#include "building.h"
#include "geometry.h"
#include "schedule.h"

C Passed parameters
      integer ICOMP,ICGTX,ICGUX,IDT,IPER,icgsX,icgfX
      character caskeyt*16 ! key word

      COMMON/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Electrical data flag and elctrical data.
      common/elecflg/ielf(mcom)

      logical OK,DOK,CLOSE,silent

      character OUTS*124,hold*36
      character msg*48  ! for the dialog

      integer ipft,IPHAST  ! for local editing.
      real pft,pwrt,bvoltt,xfla,zb   ! for local editing.
      real airvelx,tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox
      real xairvel,xtnbmen,xtnbwomen,xtnbchild,xmetabolic,xclo
      integer IALL
      real LGAIN
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDCASGN'  ! set for subroutine

C If new period then set defaults based on the key word, otherwise 
C get data from common.
      if (IPER.eq.0) then
        CMGSX=0.
        CMGLX=0.
        if(caskeyt(1:6).eq.'people')then
          RADCX=0.6; CONCX=0.4
        elseif(caskeyt(1:13).eq.'dynamicpeople')then
          RADCX=0.6; CONCX=0.4
        elseif(caskeyt(1:8).eq.'lighting')then
          RADCX=0.3; CONCX=0.7
        elseif(caskeyt(1:9).eq.'equipment')then
          RADCX=0.4; CONCX=0.6
        elseif(caskeyt(1:5).eq.'other')then
          RADCX=0.5; CONCX=0.5
        elseif(caskeyt(1:13).eq.'net_utilities')then
          RADCX=0.5; CONCX=0.5
        elseif(caskeyt(1:15).eq.'ALOtherElectric')then
          RADCX=0.5; CONCX=0.5
        elseif(caskeyt(1:9).eq.'ALStoveNG')then
          RADCX=0.5; CONCX=0.5
        elseif(caskeyt(1:15).eq.'ALStoveElectric')then
          RADCX=0.5; CONCX=0.5
        elseif(caskeyt(1:7).eq.'ALDryer')then
          RADCX=0.5; CONCX=0.5
        endif
        IPFX=0
        IPHASX=1
        PFX=0.0
        PWRX=0.0
        BVOLTX=0.0
      ELSEIF(IDT.GT.0)THEN

C << cpycasit also needs caskeyt >>
        call CPYCASIT(icomp,idt,iper,icgtX,icguX,icgsX,icgfX,cmgsX,
     &    cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX,
     &    tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox,airvelx)
      endif

      helptopic='operations_casual_data'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If silent jump to insertion logic.
      if(silent) goto 112

C Ask for start and finish times.
 10   write (OUTS,'(A)') 'Start and finish hours for period?'
      write (hold,'(2i5,a)') ICGSX,ICGFX,'  '
      CALL EASKS(hold,outs,' ',24,' 0 24 ','cas st-fn hr',IER,nbhelp)
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
      IGU=1
      if(icguX.eq.0)then
        msg='Gain to be specified (currently Watts)'
      elseif(icguX.eq.1)then
        msg='Gain to be specified (currently W/m2)'
      elseif(icguX.eq.2)then
        msg='Gain to be specified (currently m2/person)'
      elseif(icguX.eq.3)then
        msg='Gain to be specified (currently nb of person)'
      endif
      CALL EASKATOG(msg,'Confirm: ',
     &  'Watts','Watts/m2','objects/m2->Watts',
     &  'Square meters per object','Dynamic People',
     &  ' ',' ',IGU,nbhelp)

C Common portion of help message for detailed dialogs.
      if (IGU.eq.1) then

C User requests gain as W - require gain only. If existing is in W/m2 then multiply
C up to get W and revise icgtx to be positive and set icgux.
 20     if (icguX.eq.1) then
          call edisp(iuout,'Converting from W/m2 to Watts')
          write (HOLD,'(2f8.1,a)') CMGSX*zb,CMGLX*zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGSX,CMGLX,'  '
        endif
        call EASKS(HOLD,'Sensible and latent gain (W):',
     &    ' ',24,'0.   0. ','casual gn info',IER,nbhelp)
        if(ier.eq.-3) goto 10  ! user canceled
        K=0
        call EGETWR(HOLD,K,XSEN,0.,100000.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLATGN,0.,100000.0,'W','latent gain',IER)
        if (IER.ne.0) goto 20
        if (ICGTX.lt.0) ICGTX=ICGTX*(-1)  ! reset to positive
        icguX=0   ! mark as Watts
        CMGSX=XSEN
        CMGLX=XLATGN
      elseif (IGU.eq.2) then

C User requests gain as W/m^2 - require gain per square meter. If original was in
C W divide by base area to reset value. 
        if (icguX.eq.0) then
          call edisp(iuout,'Converting from Watts -> W/m2...')
          write (HOLD,'(2f8.1,a)') CMGSX/zb,CMGLX/zb,'  '
        else
          write (HOLD,'(2f8.1,a)') CMGSX,CMGLX,'  '
        endif
 30     call EASKS(HOLD,'Sensible and latent gain (W/m^2):',
     &          ' ',24,'0.   0. ','casual gn info',IER,nbhelp)
        if(ier.eq.-3) goto 10  ! user canceled
        K=0
        call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
        call EGETWR(HOLD,K,XLATGN,0.,50.0,'W','latent gain',IER)
        if (IER.ne.0) goto 30
        if(caskeyt(1:6).eq.'people')then
          CMGSX=XSEN*zb   ! occupants so convert back to W
          CMGLX=XLATGN*zb
          icguX=0   ! mark as Watts
          write(outs,'(a,f9.1,a,f9.1)') 'Sensible W are now ',CMGSX,
     &    ' & latent W ',CMGLX
        else
          if (ICGTX.gt.1) ICGTX=ICGTX*(-1)
          CMGSX=XSEN
          CMGLX=XLATGN
          icguX=1   ! mark as Watts/m2
          write(outs,'(a,f9.1,a,f9.1)') 'Sensible W/m2 are now ',CMGSX,
     &    ' & latent W/m2 ',CMGLX
        endif
        call edisp(iuout,outs)

      elseif (IGU.eq.3) then

C Gain as W/object - require number of objects and gain per object. If
C user cancels go back to initial question otherwise user input is converted
C into equivalent Watts.
C << an actual cancel option not yet implemented >>
        NOBJ=1
 40     CALL EASKI(NOBJ,' ','Number of objects (e.g. lamps)?',
     &    0,'W',100,'W',1,'number of objects',IERI,nbhelp)
        if(ieri.eq.0)then
          continue
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 40
        endif

C Display value is current sensible and latent divided by the number
C of objects given above. Use the user values to derive equivalent
C Watts and reset both icgtx and icgux to the equivalent Watts.

C << later find a way to actually hold the number of objects >>

        write (HOLD,'(2f8.1,a)') CMGSX/real(NOBJ),CMGLX/real(NOBJ),'  '
        call EASKS(HOLD,'Object sensible and latent gain (W):',
     &          ' ',24,'  0.   0. ','casual gn info',IER,nbhelp)
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
        icguX=0   ! mark as Watts

      elseif (IGU.eq.4) then

C Gain as m^2/object - floor area of an object and gain per object.
C If user cancels go back to initial question. For the particular
C case of people the current logic converts into W using 95W & 45W.
C If occupants and a zero entered this is interpreted as no people
C for this timestep.  For gains other than people convert the users
C data back into W/m2.
        NOBJ=1
 50     if(caskeyt(1:6).eq.'people')then
          FLA=CMGSX  ! assumes occupants already in m2/p
        else
          FLA=10.
        endif
        XFLA=FLA
        call EASKR(XFLA,
     &   'Floor area per object (with zero meaning none)?',
     &   '  ',0.0,'W',100.0,'W',1.0,'area per object',IERI,nbhelp)
        if(IERI.eq.0)then
          FLA=XFLA

          if(caskeyt(1:6).eq.'people')then

C If dealing with occupants treat this as a special case which assumes
C that each person is 95W sensible and 45W latent.
            ICGTX=-1   ! overload slot
            icguX=2    ! mark as m2/person
            CMGSX=FLA  ! Set sensible to the m^2 per person entered
            CMGLX=0.   ! Why is CMGLX set to zero?
            call eclose(FLA,0.00,0.001,close)
            if(close)then
              write(outs,'(a)') 'Assuming no loads for this period.'
            else
              VS=(ZBASEA(ICOMP)/CMGSX)*95.  ! find sensible watts
              VL=(ZBASEA(ICOMP)/CMGSX)*45.  ! find latent watts
              write(outs,'(a,f9.1,a,f9.1,a)') 
     &          'Sensible now equivalent to',VS,
     &          'W & latent to ',VL,'W.'
            endif
            call edisp(iuout,outs)
          else
            write (HOLD,'(2f8.1,a)') CMGSX/(zb/FLA),
     &                               CMGLX/(zb/FLA),'  '
            call EASKS(HOLD,'Sensible and latent gain (W/object):',
     &          ' ',24,'0.   0. ','casual gn info',IER,nbhelp)
            K=0
            call EGETWR(HOLD,K,XSEN,0.,50.0,'W','sensible gain',IER)
            call EGETWR(HOLD,K,XLATGN,0.,50.0,'W','latent gain',IER)
            if (IER.ne.0) goto 50
            if (ICGTX.gt.1) ICGTX=ICGTX*(-1)  ! overload
            CMGSX=XSEN*zb/FLA
            CMGLX=XLATGN*zb/FLA
            write(outs,'(a,f9.1,a,f9.1)') 'Sensible W/m2 now ',CMGSX,
     &        ' & latent W/m2 ',CMGLX
            call edisp(iuout,outs)
            icguX=1   ! mark as Watts/m2
          endif
        elseif(ieri.eq.-3)then
          goto 10
        else
          goto 50
        endif

      elseif (IGU.eq.5) then

C Describe dynamic people:
C number of men number of women, number
C of children, avg metabolic rate, avg clothing level.

 51     if(caskeyt(1:13).eq.'dynamicpeople')then
          xtnbmen=tnbmenx
          xtnbwomen=tnbwomenx
          xtnbchild=tnbchildx
          xmetabolic=metabolicx
          xclo=clox
          xairvel=airvelx
        else
          xtnbmen=1.0
          xtnbwomen=1.0
          xtnbchild=0.0
          xmetabolic=1.2
          xclo=1.0
          xairvel=0.1
        endif

        write (HOLD,'(3f6.1)') xtnbmen,xtnbwomen,xtnbchild
        call EASKS(HOLD,
     &    'Total nb men nb women nb children:',
     &    ' ',36,'0.   0. ','occupant info',IER,nbhelp)
        K=0
        call EGETWR(HOLD,K,xtnbmen,0.,50.0,'W','Number of men',IER)
        call EGETWR(HOLD,K,xtnbwomen,0.,50.0,'W','Number of women',IER)
        call EGETWR(HOLD,K,xtnbchild,0.,50.0,'W','Number of child',IER)
        if(IER.eq.0)then
          tnbmenx=xtnbmen
          tnbwomenx=xtnbwomen
          tnbchildx=xtnbchild
          icguX=3    ! mark as number of person
          CMGSX=0.0  ! not used
          CMGLX=0.0  ! not used
        elseif(ier.eq.-3)then
          goto 10 
        else 
          goto 51
        endif

C Other questions to enable testing the equations. << TODO >>
        call EASKR(xmetabolic,
     &   'Average metabolic rate (with zero meaning none)?',
     &   '  ',0.0,'W',10.0,'W',1.0,'metabolic rate',IERI,nbhelp)
        metabolicx=xmetabolic
        call EASKR(xclo,
     &   'Average cloting (clo) value (with zero meaning none)?',
     &   '  ',0.0,'W',3.0,'W',1.0,'clo value',IERI,nbhelp)
        clox=xclo
C<< air velocity it may be used to calculate convective heat transfer from occupant
c        call EASKR(xairvel,
c     &   'Air velocity(m/s) value (with zero meaning none)?',
c     &   '  ',0.0,'W',3.0,'W',1.0,'clo value',IERI,nbhelp)
c        airvelx=xairvel
C Ask about testing.
        CALL EASKABC('Test this','Options:','DBT 20 MRT 20 RH 50',
     &     'user defined MRT RH','continue',ivopt,nbhelp)
        if(ivopt.eq.1)then
C Tmix = CONV*Tair + RAD*Tmrt
          TMIX=CONCX*20.0+RADCX*22.0
          TOP=TMIX 
          if (metabolicx.GT.1.6)then  
            SGAIN=(-0.0471*TOP**2-3.3346*TOP+155.86)      ! When metabolic rate> 1.6 met
            LGAIN=(0.0729*TOP**2+1.7747*TOP+32.646)
          else if (metabolicx.LT.1.2) then
            SGAIN=(-0.0558*TOP**2-2.99*TOP+174.3)      ! When metabolic rate< 1.2 
            LGAIN=(0.1544*TOP**2-4.6528*TOP+57.37)
          else
            SGAIN=(-0.0577*TOP**2-2.8642*TOP+172.16)  ! When metabolic rate> 1.2 and <1.6 met
            LGAIN=(0.1231*TOP**2-1.8358*TOP+25.452)
          end if
          write(6,*) 'dynamic sensible is ',SGAIN,' latent is',LGAIN,
     &      ' for dbt mrt TMIX 20. 22. ',TMIX
        elseif(ivopt.eq.2)then
          call EASKR(v1,'Test dry bulb temperature?',
     &   '  ',0.0,'W',30.0,'W',1.0,'test drybulb',IERI,nbhelp)
          call EASKR(v2,'Test MRT?',
     &   '  ',0.0,'W',30.0,'W',1.0,'test mrt',IERI,nbhelp)
          TMIX=CONCX*v1+RADCX*v2
          TOP=TMIX 
          if (metabolicx.GT.1.6)then  
            SGAIN=(-0.0471*TOP**2-3.3346*TOP+155.86)      ! When metabolic rate> 1.6 met
            LGAIN=(0.0729*TOP**2+1.7747*TOP+32.646)
          else if (metabolicx.LT.1.2) then
            SGAIN=(-0.0558*TOP**2-2.99*TOP+174.3)      ! When metabolic rate< 1.2 
            LGAIN=(0.1544*TOP**2-4.6528*TOP+57.37)
          else
            SGAIN=(-0.0577*TOP**2-2.8642*TOP+172.16)  ! When metabolic rate> 1.2 and <1.6 met
            LGAIN=(0.1231*TOP**2-1.8358*TOP+25.452)
          end if
          write(6,*) 'dynamic sensible is ',SGAIN,' latent is',LGAIN,
     &      ' for dbt mrt TMIX',v1,v2,TMIX
        elseif(ivopt.eq.3)then
          continue
        endif
      elseif (IGU.eq.-3) then
        goto 10  ! user hit the cancel button
      endif

C Ask for rad/ conv split. radcs and concs will already have been set
C based on the key word.
 70   write (HOLD,'(2f7.3,a)') RADCX,CONCX,'  '
      helptopic='operations_lighting_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call EASKS(HOLD,'Radiant & convective fraction: (see help)',
     &        ' ',24,' 0.5  0.5  ','casual gn info',IER,nbhelp)
      K=0
      call EGETWR(HOLD,K,RAD,0.,1.0,'F','rad frac',IER)
      CMAX=(1.00-RAD)+0.02  ! allow for small rounding errors
      call EGETWR(HOLD,K,CON,0.,CMAX,'F','conv frac',IER)
      if (IER.ne.0) goto 70
      if(rad+con.lt.0.9)then
        write(outs,'(a,f7.3)') 
     &    'The gains radiant & convective parts add up to ',rad+con
        DOK=.false.
        CALL ASKOK(outs,
     &    ' This is less than 0.9. Are you sure?',OK,DOK,nbhelp)
        IF(.NOT.OK)GOTO 70
      endif
      RADCX=RAD
      CONCX=CON
      
C Electrical details.
      if (IELF(ICOMP).EQ.1) then
        DOK=.false.
        call ASKOK(' ','Does this load need electrical data?',
     &    OK,DOK,nbhelp)
        if(OK) then

C Get the electrical details of the load from the user
 80       pft=PFX
          call EASKR(pft,' ','Load power factor?',
     &      0.0,'F',1.0,'F',1.0,'power factor',IER,nbhelp)
          if (IER.ne.0) goto 80

          call ECLOSE(pft,1.0,0.0001,CLOSE)
          if (CLOSE) then
            IPFT=0
          else
            if(ipfX.eq.-1)then
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading (*)','Lagging',IOPT,nbhelp)
            elseif(ipfX.eq.1)then
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading','Lagging (*)',IOPT,nbhelp)
            else
              call EASKAB('Is the load current leading or lagging?',
     &          '(see help)','Leading','Lagging',IOPT,nbhelp)
            endif
            if (IOPT.EQ.1) then
              IPFT=-1
            else
              IPFT=1
            endif
          endif

 90       pwrt=PWRX
          call EASKR(pwrt,'Real power consumption ?','  ',
     &      0.0,'-',10000.0,'-',CMGSX,'power consumption',IER,nbhelp)
          if (IER.ne.0) goto 90

 100      bvoltt=BVOLTX
          call EASKR(bvoltt,' ','Operational voltage?',
     &      0.0,'-',10000.0,'-',220.0,'op voltage',IER,nbhelp)
          if (IER.ne.0) goto 100

 110      IPHAST=IPHASX
          call EASKI(IPHAST,'Which phase is load connected to?',
     &      '(see help)',1,'F',4,'F',1,'phase ',IERI,nbhelp)
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

C Start insertion of casual gain. Loop until slot matches.
 113  if (IDT.gt.0) then
        IF(IPER.EQ.0)THEN
          NCG=NCAS(IDT)
          IF(NCG.LE.0)NCG=1
          ilper=1  ! ensure ilper is not zero for new gain
          do 210 I=1,NCG
            if(iabs(ICGT(IDT,I)).le.iabs(ICGTX))then
              if(ICGS(IDT,I).lt.ICGSX)then
                ILPER=I+1
              elseif(ICGS(IDT,I).eq.ICGSX)then !.and.ICGF(IDT,I).gt.ICGFX)then
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

C Now update common blocks. Loop backwards from INC to ILPER.
        do 300 IX=INC,ILPER,-1
          if (IX.eq.ILPER) then
            call CPYCASTI(icomp,idt,ix,icgtX,icguX,icgsX,icgfX,cmgsX,
     &        cmglX,radcX,concX,ipfX,iphasX,pfX,pwrX,bvoltX,
     &        tnbmenx,tnbwomenx,tnbchildx,metabolicx,clox,airvelx)
          else
            call CPYCASIJ(icomp,IDT,IX,IX-1)
          endif
 300    continue
      ENDIF

C Loop back if more than one day type requested
      IF(IALL.EQ.1)THEN
        IDT=IDT+1
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
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      INTEGER IX,IDT,IDTT

C Electrical data flag and elctrical data.
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

      character VERT(3*MC)*42
      dimension IVERT(MC),ITEMP(3*MC),kstr(MDTY)
      integer ilistlen
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CDCASGN'  ! set for subroutine
      helptopic='casual_copy_delete'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Choose day type to delete/copy period from
      IX=1
      IF(ICD.EQ.1)THEN
        call edisp(iuout,'Choose daytype to delete period from')
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to delete period from',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to delete period from',IER,nbhelp)
      ELSEIF(ICD.EQ.2)THEN
        call edisp(iuout,'Choose daytype to copy period from')
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to copy period from',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to copy period from',IER,nbhelp)
      ENDIF

      IDT=kstr(1)
      M=0
      ilistlen=NCAS(IDT)

C Generate menu text for each gain on the current day.
      DO 10 L=1,NCAS(IDT)
        M=M+1
        if(ICGUnit(IDT,L).eq.0)then
          jicgt=ICGT(IDT,L)
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &      lodlabel(icomp,jicgt),' W  ',CMGS(IDT,L),CMGL(IDT,L)
  303     FORMAT('period ',2i3,1x,2a,2f6.0)
        elseif(ICGUnit(IDT,L).eq.2)then
          jicgt=iabs(ICGT(IDT,L))   ! assumes occupant
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &      lodlabel(icomp,jicgt),' m2p',CMGS(IDT,L),CMGL(IDT,L)
        elseif(ICGUnit(IDT,L).eq.3)then
          jicgt=iabs(ICGT(IDT,L))   ! assumes metabolic
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &      lodlabel(icomp,jicgt),' ocu',CMGS(IDT,L),CMGL(IDT,L)
        else
          jicgt=iabs(ICGT(IDT,L))
          WRITE(VERT(M),303)ICGS(IDT,L),ICGF(IDT,L),
     &      lodlabel(icomp,jicgt),' Wm2',CMGS(IDT,L),CMGL(IDT,L)
        endif
 10   continue

      IF(ICD.EQ.1)THEN
        call EPICKS(ilistlen,IVERT,' ',
     &    'Casual gain period(s) to delete:',
     &    42,M,VERT,'Casual gain period to delete',IER,nbhelp)

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
        CALL EPICKS(IX,KSTR,' ',
     &  'Choose day type to copy period to',12,NBDAYTYPE,CALENTAG,
     &  'Choose daytype to copy period to',IER,nbhelp)
        IDTT=kstr(1)
        M=NCAS(IDT)
        IXX=M
        call EPICKS(IXX,IVERT,' ','Casual gain period(s) to copy:',
     &         42,M,VERT,'Casual gain period to copy',IER,nbhelp)
        do 210 IX=1,NCAS(IDT)
         if (NCAS(IDTT).lt.MC) then
            ICOPY=0
            DO 220 IJ=1,IXX
              IF(IX.EQ.IVERT(IJ))ICOPY=1
 220        CONTINUE
            IF(ICOPY.EQ.1)THEN
              NCAS(IDTT)=NCAS(IDTT)+1
              ICGT(IDTT,(NCAS(IDTT)))=ICGT(IDT,IX)
              ICGUnit(IDTT,(NCAS(IDTT)))=ICGUnit(IDT,IX)
              ICGS(IDTT,(NCAS(IDTT)))=ICGS(IDT,IX)
              ICGF(IDTT,(NCAS(IDTT)))=ICGF(IDT,IX)
              CMGS(IDTT,(NCAS(IDTT)))=CMGS(IDT,IX)
              CMGL(IDTT,(NCAS(IDTT)))=CMGL(IDT,IX)
              RADC(IDTT,(NCAS(IDTT)))=RADC(IDT,IX)
              CONC(IDTT,(NCAS(IDTT)))=CONC(IDT,IX)
              tnbmen(IDTT,(NCAS(IDTT)))=tnbmen(IDT,IX)
              tnbwomen(IDTT,(NCAS(IDTT)))=tnbwomen(IDT,IX)
              tnbchild(IDTT,(NCAS(IDTT)))=tnbchild(IDT,IX)
              metabolic(IDTT,(NCAS(IDTT)))=metabolic(IDT,IX)
              clov(IDTT,(NCAS(IDTT)))=clov(IDT,IX)
              airvel(IDTT,(NCAS(IDTT)))=airvel(IDT,IX)
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
C   which is used if isource is zero.

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
#include "schedule.h"
      
      integer lnblnk  ! function definition

C Passed parameters
      integer  isource  ! index of source zone
      integer  idest    ! index of destination zone
      character actcas*8  ! instruction 'allcas' copies all
      character iwatt*8   ! instruction see above
      character*(*) sfile ! source operations file name if isource is zero
      integer  itrc       ! if zero work silently
      character act*1   ! '-' normal, 's' silent
      integer ier       ! if non-zero there was a problem

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      CHARACTER oprdescc*248,ventdescc*248,ctlstrc*24
      COMMON/P1C/oprdescc,ventdescc,ctlstrc
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      common/elecflg/ielf(mcom)
      COMMON/ELP3N/NEL(MDTY),PF(MDTY,MC),IPF(MDTY,MC),PWR(MDTY,MC),
     &BVOLT(MDTY,MC),IPHAS(MDTY,MC)
      INTEGER NEL,IPF,IPHAS
      REAL PF,PWR,BVOLT

C Descriptive label for a zone load & casual gain types.
      character lodlabelc*12,caskeytypec*16
      common/loadlabelc/lodlabelc(MGTY),caskeytypec(MGTY)

      integer lodslotc    ! same meaning as loadkey common
      integer lodatr1c,lodatr2c
      common/loadkeyc/lodslotc(MGTY),lodatr1c(MGTY),
     &                lodatr2c(MGTY)

C Scratch version of P3N
      INTEGER NCASC,ICGSC,ICGFC,ICGUnitC
      REAL CMGSC,CMGLC,RADCC,CONCC
      REAL tnbmenC,tnbwomenC,tnbchildC,metabolicC,cloC,airvelC
      COMMON/P3NC/NCASC(MDTY),ICGSC(MDTY,MC),ICGFC(MDTY,MC),
     & ICGUnitC(MDTY,MC),CMGSC(MDTY,MC),CMGLC(MDTY,MC),
     & RADCC(MDTY,MC),CONCC(MDTY,MC),
     & tnbmenC(MDTY,MC),tnbwomenC(MDTY,MC),tnbchildC(MDTY,MC),
     & metabolicC(MDTY,MC),cloC(MDTY,MC),airvelC(MDTY,MC)
      INTEGER ICGTC
      COMMON/P3TYPENC/ICGTC(MDTY,MC)

      common/elecflgc/ielfc
      COMMON/ELP3NC/NELC(MDTY),PFC(MDTY,MC),IPFC(MDTY,MC),PWRC(MDTY,MC),
     &BVOLTC(MDTY,MC),IPHASC(MDTY,MC)
      INTEGER NELC,IPFC,IPHASC
      REAL PFC,PWRC,BVOLTC

      common/SLNTFULL/ifullysilent
      integer ifullysilent !flag to drive the silent model creation without
                           !questions about the operation files data

      character ltmp*72
      character fs*1,GFILE*72
      LOGICAL XST,unixok,dok,ok
      character actflatfile*1
      integer idty
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='IMPORTZCASG'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C It is assumed that a prior call to COPYAIRFL has been made
C so there is no need to clear nac. The task is not to setup
C the casual gain portion of the file.
      IER=0
      DO 600 IDTY=1,NBDAYTYPE
        NCAS(IDTY)=0
 600  CONTINUE

C << is it actually necessary to set these up now? >>
      lodlabel(idest,1)='Occupants'
      caskeytype(idest,1)='people      '
      lodlabel(idest,2)='Lights'
      caskeytype(idest,2)='lighting    '
      lodlabel(idest,3)='SmallPower'  
      caskeytype(idest,3)='equipment   '
      lodlabel(idest,4)='Otherstuff'  
      caskeytype(idest,4)='other       '  
      lodlabel(idest,5)='Ann.El '  
      caskeytype(idest,5)='net_utilities'  
      lodlabel(idest,6)='Metabolic '  
      caskeytype(idest,6)='dynamicpeople'  
      lodlabel(idest,7)='N/A    ' 
      caskeytype(idest,7)='-           '  
  
      helptopic='oper_import_dialogs'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
          if(ier.ne.0)then
            call edisp(iuout,'Source operation file not clean...')
            ier=1
            return
          endif
          goto 32
        else

C Read the source operations file from within the model and
C if this is successful then instanciate the P2C and P2CTLC
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

C Copy the casual gains into the local arrays.

C << logic needed to copy *type attributes >>

      ielfc=ielf(isource)
      DO 601 IDTY=1,NBDAYTYPE
        nelc(idty)=nel(IDTY)
        NCASC(idty)=NCAS(IDTY)
        if(NCAS(IDTY).gt.0)then
          do 10 L=1,NCAS(IDTY)
            ICGSC(IDTY,L)=ICGS(IDTY,L)
            ICGFC(IDTY,L)=ICGF(IDTY,L)
            ICGUnitC(IDTY,L)=ICGUnit(IDTY,L)
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
C << ?? ventdescc ?? >>

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
          CALL EASKS(ltmp,' New zone operations file: (confirm) ',
     &      ' ',72,GFILE,'new operations file',IER,nbhelp)
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

C New file will be in sorted format with multiple day types allowed.
C << consider when this should be 21? >>
        ip3ver(idest)=2
      endif

C Copy the local array data into the common blocks overwriting
C the data implied by 'actcas'. Also act on the iwatt directive.
C Note: if the casual gain type is negative then the units are
C already per m2 so no need for conversion.  If isource is zero
C then we do not know the ratio so set it to 1.0 and inform user.
      if(iwatt(1:6).eq.'W-m2-W')then
        if(isource.eq.0)then
          VAL=10.0
          CALL EASKR(VAL,' ',' Assumed floor area of source ? ',
     &        0.10,'F',9999.0,'W',0.0,'assumed floor area',IER,nbhelp)
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
              ICGUnit(IDTY,L)=ICGUnitC(IDTY,L)  ! overloaded icgtc
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
          call edisp248(iuout,ventdesc(idest),80)
          call edisp(iuout,' ')
          call edisp(iuout,' Source operation notes:')
          call edisp248(iuout,oprdescc,80)
          call edisp(iuout,' ')
          call edisp248(iuout,ventdescc,80)
          call edisp(iuout,' ')
          dok=.true.
          call askok('Use the source documentation?',' ',OK,DOK,0)
          if(ok) oprdesc(idest)=oprdescc
        elseif(act(1:1).eq.'s')then
          oprdesc(idest)=oprdescc
        endif

        if(act(1:1).eq.'-')then
          CALL EASKS248(oprdescc,'Operations notes (please edit):',
     &      ' ',72,'base_case','operation notes',IER,nbhelp)
          if(oprdescc(1:2).ne.'  ')oprdesc(idest)=oprdescc
        endif

        if(isource.eq.0)then
          do il=1,7
            lodlabel(idest,il)=lodlabelc(il)
            caskeytype(idest,il)=caskeytypec(il)
            lodslot(idest,il)=lodslotc(il)
            lodatr1(idest,il)=lodatr1c(il)
            lodatr2(idest,il)=lodatr2c(il)
          enddo
        else
          do il=1,7
            lodlabel(idest,il)=lodlabel(isource,il)
            caskeytype(idest,il)=caskeytype(isource,il)
            lodslot(idest,il)=lodslot(isource,il)
            lodatr1(idest,il)=lodatr1(isource,il)
            lodatr2(idest,il)=lodatr2(isource,il)
          enddo
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
      if(ip3ver(isource).ge.21)then
        continue
      else
        call edisp(iuout,'Sorting imported casual gains...')
        call PROCESSOLDCAS(idest,0,iuout)
        call edisp(iuout,'Sorting imported casual gains...completed.')
      endif

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

C << for the case of dynamicpeople plot right axis as
C << number of people rather than W/m2 (to be done)
C << perhaps also a slightly different header

C  ICOMP zone number

      subroutine PLTCASGN(ICOMP,IDTYP)
#include "building.h"
#include "geometry.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      INTEGER IDTY,lnlb
      Real METAOC,SGAIN,LGAIN
      dimension CGAINS(25,7),CGAINL(25,7),CGAINTS(25),CGAINTL(25)

      character outs*124

#ifdef OSI
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2,iicol,iglsat,iglsun
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to etplot
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2,iicol,iglsat,iglsun
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      logical COLOK
      IDTY=IDTYP  ! the day type passed in
      IDAY=IDTY-1 ! the previous day type
      JDAY=IDTY   ! the focus day type
      KDAY=IDTY+1 ! the next day type
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

C Loop throught the prior day.
      do 10 I=1,24
        CGNS=0.
        CGNL=0.
        do 20 J=1,NCAS(IDAY)
          if (I.gt.ICGS(IDAY,J).and.I.le.ICGF(IDAY,J)) then
            if (ICGUnit(IDAY,J).eq.2) then

C For case of m2/person CMGL1()J remains zero so use cmgs1(J) which
C is the m^2 per person instead. Base graphed value on fractional 
C persons per m^2. For case of CMGS zero assume no people.
              if (CMGS(IDAY,J).gt.0.0) then
                CGNS=CGNS+(ZBASEA(ICOMP)/CMGS(IDAY,J))*95.
                CGNL=CGNL+(ZBASEA(ICOMP)/CMGS(IDAY,J))*45.
              else
                CGNS=0.0
                CGNL=0.0
              endif
            elseif (ICGUnit(IDAY,J).eq.1) then
              CGNS=CGNS+CMGS(IDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(IDAY,J)*ZBASEA(ICOMP)
            elseif (ICGUnit(IDAY,J).eq.3) then
              METAOC=metabolic(IDAY,J)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=CGNS+SGAIN*tnbmen(IDAY,J)+
     &              0.85*SGAIN*tnbwomen(IDAY,J)+
     &              0.75*SGAIN*tnbchild(IDAY,J)
              CGNL=CGNL+LGAIN*tnbmen(IDAY,J)+
     &              0.85*LGAIN*tnbwomen(IDAY,J)+
     &              0.75*LGAIN*tnbchild(IDAY,J)
            else
              CGNS=CGNS+CMGS(IDAY,J)
              CGNL=CGNL+CMGL(IDAY,J)
            endif
          endif
 20     continue
        YMAXWKD=AMAX1(YMAXWKD,CGNS,CGNL)
 10   continue

C Loop through the focus day.
      do 12 I=1,24
        CGNS=0.
        CGNL=0.
        do 22 J=1,NCAS(JDAY)
          if (I.gt.ICGS(JDAY,J).and.I.le.ICGF(JDAY,J)) then
            if (ICGUnit(JDAY,J).eq.2) then
              if (CMGS(JDAY,J).gt.0.0) then
                CGNS=CGNS+(ZBASEA(ICOMP)/CMGS(JDAY,J))*95.0
                CGNL=CGNL+(ZBASEA(ICOMP)/CMGS(JDAY,J))*45.0
              else
                CGNS=0.0
                CGNL=0.0
              endif
            elseif (ICGUnit(JDAY,J).eq.1) then
              CGNS=CGNS+CMGS(JDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(JDAY,J)*ZBASEA(ICOMP)
            elseif (ICGUnit(JDAY,J).eq.3) then
              METAOC=metabolic(IDAY,J)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=CGNS+SGAIN*tnbmen(JDAY,J)+
     &              0.85*SGAIN*tnbwomen(JDAY,J)+
     &              0.75*SGAIN*tnbchild(JDAY,J)
              CGNL=CGNL+LGAIN*tnbmen(JDAY,J)+
     &              0.85*LGAIN*tnbwomen(JDAY,J)+
     &              0.75*LGAIN*tnbchild(JDAY,J)
            else
              CGNS=CGNS+CMGS(JDAY,J)
              CGNL=CGNL+CMGL(JDAY,J)
            endif
          endif
 22     continue
        YMAXSAT=AMAX1(YMAXSAT,CGNS,CGNL)
 12   continue

C Loop through the next day.
      do 14 I=1,24
        CGNS=0.
        CGNL=0.
        do 24 J=1,NCAS(KDAY)
          if (I.gt.ICGS(KDAY,J).and.I.le.ICGF(KDAY,J)) then
            if (ICGUnit(KDAY,J).eq.2) then
              if (CMGS(KDAY,J).gt.0.0) then
                CGNS=CGNS+(ZBASEA(ICOMP)/CMGS(KDAY,J))*95.0
                CGNL=CGNL+(ZBASEA(ICOMP)/CMGS(KDAY,J))*45.0
              else
                CGNS=0.0  ! avoid divide by zero 
                CGNL=0.0
              endif
            elseif (ICGUnit(KDAY,J).eq.1) then
              CGNS=CGNS+CMGS(KDAY,J)*ZBASEA(ICOMP)
              CGNL=CGNL+CMGL(KDAY,J)*ZBASEA(ICOMP)
            elseif (ICGUnit(KDAY,J).eq.3) then
              METAOC=metabolic(IDAY,J)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=CGNS+SGAIN*tnbmen(KDAY,J)+
     &              0.85*SGAIN*tnbwomen(KDAY,J)+
     &              0.75*SGAIN*tnbchild(KDAY,J)
              CGNL=CGNL+LGAIN*tnbmen(KDAY,J)+
     &              0.85*LGAIN*tnbwomen(KDAY,J)+
     &              0.75*LGAIN*tnbchild(KDAY,J)
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

C Setup and pass in parameters to win3d. Offset 11 char on left,
C 13 char on right, 9 lines on top and 3 below the graphs.
      iiw1=11; iiw2=13; iiw3=9; iiw4=3; iimenu=menuchw
      iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
      CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &  iigl,iigr,iigt,iigb,iigw,iigwh)
      igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
      igw=int(iigw); igwh=int(iigwh)
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
     &  xsc,ysc,sca,xadd,yadd)
      call vrtaxis(YMIN,YMAXF,iigr,iigb,iigt,yadd,ysc,0,1,
     &  'Gain (W/m^2)')

C Re-set scale for gain (W) axis.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAX,
     &  xsc,ysc,sca,xadd,yadd)

C Scale and draw first day type requested.
      CALL horaxis(plxmin,plxmax,iigl,iigr,iigb,xadd,xsc,0,
     &  'Time (hrs)')
      call vrtaxis(YMIN,YMAX,iigl,iigb,iigt,yadd,ysc,0,0,'Gain (W)')
      call linescale(iigl,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)

C Scale and draw second day type requested.
      CALL horaxis(plxmin,plxmax,iglsat,iigr,iigb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMIM,iupdown,isym)

C Scale and draw third day type requested.
      CALL horaxis(plxmin,plxmax,iglsun,iigr,iigb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,iigb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmin,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmax,YMAX,iupdown,isym)

C Scan gains and generate total curves.
C first (previous) day type requested
      call linescale(iigl,xadd,xsc,iigb,yadd,ysc)
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
          if (ICGUnit(IDAY,IP).eq.2) then

C For case of m2/person CMGL is zero so use CMGS to get CGNL.
C << can this be generalized >>
            if (CMGS(IDAY,IP).gt.0.0) then
              CGNS=(ZBASEA(ICOMP)/CMGS(IDAY,IP))*95.0
              CGNL=(ZBASEA(ICOMP)/CMGS(IDAY,IP))*45.0
            else
              CGNS=0.0  ! to avoid divide by zero
              CGNL=0.0
            endif
          elseif (ICGUnit(IDAY,IP).eq.1) then
            CGNS=CMGS(IDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(IDAY,IP)*ZBASEA(ICOMP)
          elseif (ICGUnit(IDAY,IP).eq.3) then
              METAOC=metabolic(IDAY,J)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=SGAIN*tnbmen(IDAY,IP)+
     &              0.85*SGAIN*tnbwomen(IDAY,IP)+
     &              0.75*SGAIN*tnbchild(IDAY,IP)
              CGNL=LGAIN*tnbmen(IDAY,IP)+
     &              0.85*LGAIN*tnbwomen(IDAY,IP)+
     &              0.75*LGAIN*tnbchild(IDAY,IP)
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

C Plot curves. If the 4th slot filled do that as well.
      do 70 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        if(lodslot(icomp,1).ne.0) loop=1
        if(lodslot(icomp,2).ne.0) loop=2
        if(lodslot(icomp,3).ne.0) loop=3
        if(lodslot(icomp,4).ne.0) loop=4
        do 75 I=1,loop
          if(COLOK)then
            iicol=i
            call winscl('z',iicol)
          endif
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
          if(COLOK)then
            iicol=0
            call winscl('-',iicol)
          endif
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

C Second (focus) day type requested.
      call linescale(iglsat,xadd,xsc,iigb,yadd,ysc)
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
          if (ICGUnit(JDAY,IP).eq.2) then
            if (CMGS(JDAY,IP).gt.0.0) then
              CGNS=(ZBASEA(ICOMP)/CMGS(JDAY,IP))*95.0
              CGNL=(ZBASEA(ICOMP)/CMGS(JDAY,IP))*45.0
            else
              CGNS=0.0  ! to avoid division by zero
              CGNL=0.0
            endif
          elseif (ICGUnit(JDAY,IP).eq.1) then
            CGNS=CMGS(JDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(JDAY,IP)*ZBASEA(ICOMP)
          elseif (ICGUnit(JDAY,IP).eq.3) then
              METAOC=metabolic(JDAY,IP)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=SGAIN*tnbmen(JDAY,IP)+
     &              0.85*SGAIN*tnbwomen(JDAY,IP)+
     &              0.75*SGAIN*tnbchild(JDAY,IP)
              CGNL=LGAIN*tnbmen(JDAY,IP)+
     &              0.85*LGAIN*tnbwomen(JDAY,IP)+
     &              0.75*LGAIN*tnbchild(JDAY,IP)
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
        if(lodslot(icomp,1).ne.0) loop=1
        if(lodslot(icomp,2).ne.0) loop=2
        if(lodslot(icomp,3).ne.0) loop=3
        if(lodslot(icomp,4).ne.0) loop=4
        do 175 I=1,loop
          if (COLOK)then
            iicol=i
            call winscl('z',iicol)
          endif
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
          if (COLOK)then
            iicol=0
            call winscl('-',iicol)
          endif
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

C Third (subsequent) day type requested.
      call linescale(iglsun,xadd,xsc,iigb,yadd,ysc)
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
          if (ICGUnit(KDAY,IP).eq.2) then
            if (CMGS(KDAY,IP).gt.0.0) then
              CGNS=(ZBASEA(ICOMP)/CMGS(KDAY,IP))*95.0
              CGNL=(ZBASEA(ICOMP)/CMGS(KDAY,IP))*45.0
            else
              CGNS=0.0   ! avoiding divide by zero
              CGNL=0.0
            endif
          elseif (ICGUnit(KDAY,IP).eq.1) then
            CGNS=CMGS(KDAY,IP)*ZBASEA(ICOMP)
            CGNL=CMGL(KDAY,IP)*ZBASEA(ICOMP)
          elseif (ICGUnit(KDAY,IP).eq.3) then
              METAOC=metabolic(KDAY,IP)*1.8*58
              SGAIN=198.42617-3.80901*20-0.05419*20**2-0.42472*
     &             METAOC+0.00171*METAOC**2+0.01287*20*METAOC-
     &             0.00004*20*METAOC**2+0.00002*METAOC*20**2-
     &             0.00000038516*METAOC**2*20**2    
              LGAIN=227.89011-14.95202*20+0.24884*20**2-
     &             2.56734 *METAOC+0.00715*METAOC**2+
     &             0.132*20*METAOC-0.00017*20*METAOC**2-
     &             0.00107*METAOC*20**2
              CGNS=SGAIN*tnbmen(KDAY,IP)+
     &              0.85*SGAIN*tnbwomen(KDAY,IP)+
     &              0.75*SGAIN*tnbchild(KDAY,IP)
              CGNL=LGAIN*tnbmen(KDAY,IP)+
     &              0.85*LGAIN*tnbwomen(KDAY,IP)+
     &              0.75*LGAIN*tnbchild(KDAY,IP)
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
        if(lodslot(icomp,1).ne.0) loop=1
        if(lodslot(icomp,2).ne.0) loop=2
        if(lodslot(icomp,3).ne.0) loop=3
        if(lodslot(icomp,4).ne.0) loop=4
        do 275 I=1,loop
          if (COLOK)then
            iicol=i
            call winscl('z',iicol)
          endif
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
          if (COLOK)then
            iicol=0
            call winscl('-',iicol)
          endif
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
      call viewtext(outs,iside,isize,ifont) ! 65 becomes 78 69 becomes 82
      iid1=igl+20; iid2=iigt-78;
      call wstxpt(iid1,iid2,' Total sensible')
      iid1=igr-100; iid2=iigt-78;
      call wstxpt(iid1,iid2,'Total latent')
      iid1=igl+120; iid2=iigt-82;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=0
      isym=0
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igl+180; iid2=iigt-82;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=1
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igr-120; iid2=iigt-82;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=0
      CALL etplot(gx,gy,iupdown,isym)
      iid1=igr-180; iid2=iigt-82;
      call pixel2u(iid1,iid2,gx,gy)
      iupdown=-4
      CALL etplot(gx,gy,iupdown,isym)

C For each of the currently used slots draw legend.
      if(lodslot(icomp,1).ne.0) loop=1
      if(lodslot(icomp,2).ne.0) loop=2
      if(lodslot(icomp,3).ne.0) loop=3
      if(lodslot(icomp,4).ne.0) loop=4
      do 300 I=1,loop
        if(lodslot(icomp,i).gt.0)then
          lnlb=lnblnk(lodlabel(ICOMP,I))
          write(outs,'(2a)') lodlabel(ICOMP,I)(1:lnlb),' sensible'
          IY=igt-(12*I)-18
          iid1=igl+20; iid2=IY;
          call wstxpt(iid1,iid2,outs)
          write(outs,'(2a)') lodlabel(ICOMP,I)(1:lnlb),' latent'
          iid1=igr-100; iid2=IY;
          call wstxpt(iid1,iid2,outs)
          if (COLOK)then
            iicol=i
            call winscl('z',iicol)
          endif
          iid1=igl+120; iid2=IY-4;
          call pixel2u(iid1,iid2,gx,gy)
          iupdown=0
          isym=0
          CALL etplot(gx,gy,iupdown,isym)
          iid1=igl+180; iid2=IY-4;
          call pixel2u(iid1,iid2,gx,gy)
          iupdown=1
          CALL etplot(gx,gy,iupdown,isym)
          iid1=igr-120; iid2=IY-4;
          call pixel2u(iid1,iid2,gx,gy)
          iupdown=0
          CALL etplot(gx,gy,iupdown,isym)
          iid1=igr-180; iid2=IY-4;
          call pixel2u(iid1,iid2,gx,gy)
          iupdown=-4
          CALL etplot(gx,gy,iupdown,isym)
          if (COLOK)then
            iicol=0
            call winscl('-',iicol)
          endif
        endif
 300  continue

      ioff=(int(ipltwd)/3)
      iid1=igl+ioff; iid2=iigt-10;
      call wstxpt(iid1,iid2,CALENTAG(IDAY))
      iid1=iglsat+ioff; iid2=iigt-10;
      call wstxpt(iid1,iid2,CALENTAG(JDAY))
      iid1=iglsun+ioff; iid2=iigt-10;
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
#include "schedule.h"

      COMMON/OUTIN/IUOUT,IUIN

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

C << Has built in assumptions about the number of casual gains and their
C << order and the units. Consider making more flexible.
C << ?? 4th slot >>
      subroutine initcasday(ier)

#include "building.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      INTEGER ILN
      dimension iper(24),iperf(24)
      character hold*48,OUTS*12,LOUTS*24,VOUTS*50
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='initcasday'  ! set for subroutine
      helptopic='operations_initial'
      call gethelptext(helpinsub,helptopic,nbhelp)

      DO 600 IDTY=1,NBDAYTYPE  
        nbocper=1
        nbltper=1
        nbspper=1
        hold='                                    '
        write(hold,'(3i5,a)') nbocper,nbltper,nbspper,'  '
        OUTS=CALENTAG(IDTY)
        ILN=LNBLNK(OUTS)
        WRITE(VOUTS,'(3A)')'Number of ',OUTS(1:ILN),
     &  ' casual gain periods?'
        CALL EASKS(HOLD,VOUTS,
     &  'occupants / lights / equipment or small power',
     &  24,'  0  0  0  ','no. CG periods',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,nbocper,0,24,'F','occupant periods',IER)
        CALL EGETWI(HOLD,K,nbltper,0,24,'F','light periods',IER)
        CALL EGETWI(HOLD,K,nbspper,0,24,'F','small power periods',IER)

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
            RADC(IDTY,ipcount) = 0.6
            CONC(IDTY,ipcount) = 0.4  ! CIBSE Guide A 6.3
            ICGT(IDTY,ipcount) = 1
            ICGUnit(IDTY,ipcount) = 0 ! initial assumption of W
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
            RADC(IDTY,ipcount) = 0.3
            CONC(IDTY,ipcount) = 0.7 ! CIBSE Table A6.5
            ICGT(IDTY,ipcount) = 2
            ICGUnit(IDTY,ipcount) = 0 ! initial assumption of W
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
            ICGUnit(IDTY,ipcount) = 0
  73      continue
        endif
        NCAS(IDTY)=NCAS(IDTY)+nbocper+nbltper+nbspper
 600  CONTINUE
      return
      end

C ******* initcasday21
C initcasday21 does initial setup of one or more casual day types or
C adds a new casual gain and periods to an existing regime.
      subroutine initcasday21(icomp,islot,ier)

#include "building.h"
#include "schedule.h"
      
      integer lnblnk  ! function definition
      integer icomp   ! which zone
      integer islot   ! zero to performe initial setup otherwise add one
      integer ier     ! error state
      integer iatr1,xiatr1
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      INTEGER ILN,nbper,igu
      dimension iper(24),iperf(24)
      character OUTS*12,LOUTS*24,VOUTS*50
      character typstr*14
      real rf,cf
      logical ok,dok
      logical firsttime
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='initcasday'  ! set for subroutine
      helptopic='operations_initial'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if(islot.eq.0)then
        firsttime=.true.
      else
        firsttime=.false.
      endif
 42   typstr='  '
      if(firsttime) islot=islot+1
      call easks(TYPSTR,
     &  'User label for new casual gain <= 12 characters ','  ',14,
     &  'object',' Gain label',IER,nbhelp)
      write(lodlabel(icomp,islot),'(a)') TYPSTR(1:12)

C Explific type to go with this casual gain.
      idno=islot+1
      isw=0
      igu=1  ! assume watts as unit
      call MENUATOL('  ','Available types',
     &  'a -        not yet defined W & W/m2',
     &  'b people   basic occupant W & m2/p',
     &  'c lighting basic lighting W & W/m2',
     &  'd equipment basic small power W & W/m2',
     &  'e other    non-specific gains W & W/m2',
     &  'f dynamicpeople nb people & metabolic',
     &  'g net_utilities as legacy icgtyp = 5',
     &  'h ALOtherElectric H3K other appliances',
     &  'i ALStoveNG H3K natural gas stoves',
     &  'j ALStoveElectric H3K electrical stoves',
     &  'k ALDryer H3K clothes dryers',
     &  ' ',isw,idno,nbhelp)
      if(isw.eq.1)then
        igu=1
        rf=0.5 ; cf=0.5 
        continue  ! ?? what to do here
      elseif(isw.eq.2)then
        caskeytype(icomp,islot)='people      '
        rf=0.6; cf=0.4  ! CIBSE Guide A 6.3
        CALL EASKABCD('For people gains the typical units will be',
     &    'Confirm: ','Watts','not applicable','not applicable',
     &    'Square meters per object',IGU,nbhelp)
      elseif(isw.eq.3)then
        caskeytype(icomp,islot)='lighting    '
        rf=0.3; cf=0.7  ! CIBSE Table A6.5
        CALL EASKABCD('For lighiting gains the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'object/m2->Watts','not applicable',IGU,nbhelp)
      elseif(isw.eq.4)then
        caskeytype(icomp,islot)='equipment   '
        rf=0.4 ; cf=0.6 
        CALL EASKABCD('For equipment gains the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'objects/m2->Watts','not applicable',IGU,nbhelp)
      elseif(isw.eq.5)then
        caskeytype(icomp,islot)='other       '  
        rf=0.5 ; cf=0.5 
        CALL EASKABCD('For other gains the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'object/m2->Watts','not applicable',IGU,nbhelp)
      elseif(isw.eq.6)then
        caskeytype(icomp,islot)='dynamicpeople'
        rf=0.6; cf=0.4  ! CIBSE Guide A 6.3
          xiatr1=iatr1
        CALL EASKI(xiatr1,' ', 
     &    'Number of model (1. Cibze  2. E+   3. 2nodes)?'
     &     ,0,'W',100,'W',1,'number of equation'
     &    ,IERI,nbhelp)
          iatr1=xiatr1
        CALL EASKABCD('For metabolic dynamicpeople gains the units',
     &    'Confirm: ','not applicable','not applicable',
     &    'not applicable','number of people',IGU,nbhelp)
      elseif(isw.eq.7)then
        caskeytype(icomp,islot)='net_utilities'  
        rf=0.5 ; cf=0.5 
        CALL EASKABCD(
     &    'For network utilities the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'not applicable','not applicable',IGU,nbhelp)
       elseif(isw.eq.8)then
        caskeytype(icomp,islot)='ALOtherElectric'  
        rf=0.4 ; cf=0.6 
        CALL EASKABCD(
     &    'For appliances other electric the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'not applicable','not applicable',IGU,nbhelp)
      elseif(isw.eq.9)then
        caskeytype(icomp,islot)='ALStoveNG   '  
        rf=0.5 ; cf=0.5 
        CALL EASKABCD(
     &    'For appliance stove natural gas the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'not applicable','not applicable',IGU,nbhelp)
      elseif(isw.eq.10)then
        caskeytype(icomp,islot)='ALStoveElectric'  
        rf=0.4 ; cf=0.6 
        CALL EASKABCD(
     &    'For appliance stove electric the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'not applicable','not applicable',IGU,nbhelp)
      elseif(isw.eq.11)then
        caskeytype(icomp,islot)='ALDryer     '  
        rf=0.5 ; cf=0.5 
        CALL EASKABCD(
     &    'For appliance dryer the typical unit will be',
     &    'Confirm: ','Watts','Watts per square meter',
     &    'not applicable','not applicable',IGU,nbhelp)
      endif
      lodslot(icomp,islot)=islot; lodatr1(icomp,islot)=iatr1;
      lodatr2(icomp,islot)=0

C If adding to existing zone operation file ask user if they want to
C create periods for each day type.
      if(.NOT.firsttime)then
        DOK=.false.
        CALL ASKOK('Also define periods in each day type?',
     &    '  ',OK,DOK,nbhelp)
        if(.NOT.ok) return
      endif

      DO 600 IDTY=1,NBDAYTYPE  
        nbper=1
        OUTS=CALENTAG(IDTY)
        ILN=LNBLNK(OUTS)
        WRITE(VOUTS,'(4A)')'Number of ',TYPSTR,' periods on ',
     &     OUTS(1:ILN)
        CALL EASKI(nbper,VOUTS,'Confirm:',0,'F',24,'F',1,
     &    'number of CG periods',IER,nbhelp)

C Get user to specify the start times for occup periods if non-zero.
C Increment ipcount for each new period.
        ipcount=NCAS(IDTY)
        if(nbper.gt.0)then
          WRITE(LOUTS,'(3A)')OUTS(1:ILN),' ',TYPSTR
          call initcasper(nbper,iper,iperf,'i',LOUTS)
          do 71 ij=1,nbper
            ipcount=ipcount+1
            ICGS(IDTY,ipcount) = iper(ij)
            ICGF(IDTY,ipcount) = iperf(ij)
            CMGS(IDTY,ipcount) = 0.0
            CMGL(IDTY,ipcount) = 0.0
            RADC(IDTY,ipcount) = rf
            CONC(IDTY,ipcount) = cf
            ICGT(IDTY,ipcount) = islot  ! set to the current slot

C Assigne perferred unit for each type and period. Also impose overload
C of the ICGT (for now) until ICGUnit is fully implemented.
            if(igu.eq.1)then
              ICGUnit(IDTY,ipcount) = 0
            elseif(igu.eq.2)then
              if(isw.eq.1)then
                ICGT(IDTY,ipcount) = (-1*islot)  ! set negative current slot
                ICGUnit(IDTY,ipcount) = 2        ! patch for people
              else
                ICGT(IDTY,ipcount) = (-1*islot)  ! set negative current slot
                ICGUnit(IDTY,ipcount) = 1        ! true W/m2
              endif
            elseif(igu.eq.3)then
              ICGUnit(IDTY,ipcount) = 0        ! not yet implemented use W
            elseif(igu.eq.4)then
              if(isw.eq.6)then
                ICGUnit(IDTY,ipcount) = 3        ! number of persons
              else
                ICGT(IDTY,ipcount) = (-1*islot)  ! set negative current slot
                ICGUnit(IDTY,ipcount) = 2        ! m2/person
              endif
            endif
  71      continue
        endif
        NCAS(IDTY)=NCAS(IDTY)+nbper
 600  CONTINUE

C Check if user wants to defin another casual gain. Revise the limit once
C res can deal with 4 filled slots.
      if(.NOT.firsttime)then
        return   ! task completed
      else
        if(islot.ge.4)then
          call usrmsg(
     &       'Four slots have been defined. This is the current',
     &       'limit. You next task is to define magnitudes.','W')
          return
        else
          DOK=.false.
          CALL ASKOK('Define another casual gain label/type/periods?',
     &      '  ',OK,DOK,nbhelp)
          if(ok) goto 42   ! loop again
          return
        endif
      endif
      end

C ******* initcasper
C initcasper supports setting up initial periods for
C schedules. It returns an array iper(24)
C with the start times of each period and iperf(24) with finish
C times for each period.
C << Note change to real when casual gains data structure improves >>
      subroutine initcasper(nper,iper,iperf,act,label)

C Passed parameters
      integer nper  ! user supplied number of periods
      integer iper(24)  ! array of start times
      integer iperf(24) ! array of finish times
      character act*1   ! i is initialise and edit e is edit current
      character*(*) label

      character cstr*72,msg*72
      real incr
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='initcasper'  ! set for subroutine
      helptopic='operations_allocate_per'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
      write(msg,'(3a)') 'Start time for each ',
     &  label(1:lnblnk(label)),' period.'
      CALL EASKS(CSTR,msg,'Confirm:',72,' 0  5  6  8 ','time starts',
     &  IER,nbhelp)

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
