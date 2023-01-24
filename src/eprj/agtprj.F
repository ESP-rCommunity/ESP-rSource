C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following subroutines:
C AGT_WT     - Write agent specification file.
C AGT_MNG    - Controlling routine for defining and editing agents.
C AGT_ED     - Edit a agent.
C AGT_PIKTYP - Pick a agent model from a list.

C ******************** AGT_WT ********************
C Write agent data to file unit IUNIT.
C Data format is *tag value.

      subroutine AGT_WT(IUNIT,IER)

#include "agent.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      character*124 outs

C Open file.
      CALL EFOPSEQ(IUNIT,AGTFIL,4,IER)
      if (IER.ne.0) goto 666

C Write header line.
      write(IUNIT,'(a)',ERR=666)'# Agent specification file'

C Write file version.
      write(IUNIT,'(a,i2)',ERR=666)'*version ',AGTVSN

C Write number of agents.
      write(IUNIT,'(a,i2)',ERR=666)'*numAgents ',NAGT

C Write log file path.
      write(IUNIT,'(2a)',ERR=666)'*logFile ',AGTLOG(1:lnblnk(AGTLOG))

C For each agent ...
      do iagt=1,NAGT

C ... write spacers.
        write(IUNIT,'(a)',ERR=666)'#'
        write(IUNIT,'(a,i2,a)',ERR=666)'# - agent ',iagt,' -'

C ... write model.
        write(IUNIT,'(a,i2)',ERR=666)'*type ',AGTTYP(iagt)
        
C ... write zone.
        write(IUNIT,'(a,i2)',ERR=666)'*zone ',AGTZON(iagt)

      enddo

C Close file and exit.
  999 call ERPFREE(IUNIT,IER)
      RETURN

C Error.
  666 outs='AGT_WT: Error while writing agent specification file.'
      call edisp(IUOUT,outs)
      goto 999

      end
  
C ******************** AGT_MNG
C Manages the definition and editing of agents.
C ILOC is the location; 0 for building, >0 for a specific zone.

      SUBROUTINE AGT_MNG(ILOC,IER)

#include "agent.h"
#include "espriou.h"
#include "epara.h"
#include "building.h"
#include "model.h"
#include "help.h"

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/GFONT/IFS,ITFS,IMFS
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      
      DIMENSION ITEMS(50)
      character ITEMS*39,item_tmp*56,outs*124,fs*1,ctmp*72,agtfil_tmp*72
      character key*1,isact*3,snpfile*72,sfile*72
      logical unixok,xst,saved,isnone,get_pagstr
      integer pagstr(2),indagt(MAGT)
      
      helpinsub='agtprj'
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Ask for agent file name.
    3 write(ctmp,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.agt'
      itmp=0
      if (AGTFIL(1:7).eq.'UNKNOWN') then
        xst=.false.
        agtfil_tmp=ctmp
        CALL EASKS2CMD(agtfil_tmp,'agent file',' ',
     &    'cancel','browse',itmp,72,ctmp,'agent file',IER,0)
      else
        xst=.true.
        agtfil_tmp=AGTFIL
        CALL EASKS2CMD(agtfil_tmp,'agent file',' ',
     &    'dereference','browse',itmp,72,ctmp,'agent file',IER,0)
      endif      
      CALL USRMSG(' ',' ','-')

C Cancelled or dereferenced.
C If dereferenced, deactivate agents.
      if (itmp.eq.1) then
        if (xst) then
          CALL AGT_INIT
          CALL EMKCFG('-',IER)
        endif
        goto 999

C Browse cfg directory.
      elseif (itmp.eq.2) then
        sfile='  '
        snpfile='  '
        call browsefilelist('?','cfg','fil',sfile,snpfile,nfile,IER)
        if (nfile.gt.0) then
          sfile='  '
          snpfile='  '
          call browsefilelist('b','cfg','fil',sfile,snpfile,nfile,IER)
          if (snpfile(1:2).ne.'  ') then
            write(agtfil_tmp,'(3a)')snpfile(1:lnblnk(snpfile))
          else
            CALL edisp(iuout,' No file selection detected.')
            goto 3
          endif
        else
          CALL edisp(iuout,' No files in cfg directory.')
          goto 3
        endif
      endif

C File name given, see if it exists. If it does, open and read file.
C If it doesn't, offer user choice to choose another or open a new file.
      iuf=IFIL+1
      CALL ERPFREE(iuf,istat)
      CALL FINDFIL(agtfil_tmp,xst)
      if (xst) then
        write(currentfile,'(a)')agtfil_tmp(1:lnblnk(agtfil_tmp))
        CALL EFOPSEQ(IUF,agtfil_tmp,1,istat)
        if (istat.ne.0) then
          call edisp(iuout,'Error opening file.')
          goto 3
        endif
        CALL AGT_RD(IUF,IER)
      else
        CALL EASKAB('File not found.','Options:',
     &    'specify another','make new file',itmp,0)
        if (itmp.eq.1) goto 3
        write(outs,'(3a)')'New file ',
     &    agtfil_tmp(1:lnblnk(agtfil_tmp)),' will be created.'
        CALL edisp(iuout,outs)
      endif

C If we've made it this far, we should have a valid file name.
C Save this to common.
      AGTFIL=agtfil_tmp

C Switch to monospace menu font.
      iimfs=IMFS
      if (IMFS.ge.4 .and. IMFS.le.7) IMFS=IMFS-4
      call userfonts(IFS,ITFS,IMFS)

C Set up (possibly) multi-page menu.
      get_pagstr=.false.
      saved=.true.
  6   MHEAD=3
      MCTL=5
      nagtl=0
      do i=1,NAGT
        if (ILOC.eq.AGTZON(i)) then
          nagtl=nagtl+1
          indagt(nagtl)=i
        endif
      enddo
      if (nagtl.eq.0) then
        isnone=.true.
      else
        isnone=.false.
      endif
      ILEN=nagtl
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      if (get_pagstr) then
        IST=pagstr(1)
        IPM=pagstr(2)
        get_pagstr=.false.
      endif
    4 ipick=-4
      IER=0
      if (ILOC.eq.0) then
        ITEMS(1)=' location: whole building'
      else
        write(ITEMS(1),'(a,i2)')'1 location: zone ',ILOC
      endif
      if (lnblnk(AGTLOG).gt.27) then
        write(ITEMS(2),'(3a)') '2 log file: ',AGTLOG(1:24),'...'
      else
        write(ITEMS(2),'(2a)') '2 log file: ',AGTLOG(1:lnblnk(AGTLOG))
      endif

      if (isnone) then
        ITEMS(3)=' no agents at this location'
        ITEMS(4)=' add one to proceed'
        ITEMS(5)=' --------------------------------------'
        ITEMS(6)='+ add/delete/copy'
        ITEMS(7)='> save'
        ITEMS(8)='? help'
        ITEMS(9)='- exit menu'
        NITEMS=9
      else
  
C Header.
        ITEMS(3)=' index | model'
        m=MHEAD

C Paginated part.
        im=0
        do i=1,NAGT
          if (ILOC.eq.AGTZON(i)) then
            if (i.ge.IST.and.i.le.(IST+MIFULL)) then
              m=m+1
              im=im+1
              CALL EMKEY(im,key,IER)
              write(ITEMS(m),'(A1,1X,I4,3X,A30)')
     &          key,im,AGTDESC(AGTTYP(i))(1:30)
            endif
          endif
        enddo

C Control options.
        if (IPFLG.gt.0) then
          write(ITEMS(m+1),'(A7,I2,A4,I2,A24)')
     &      '0 page ',IPM,' of ',MPM,' -----------------------'
        else
          ITEMS(m+1)=' --------------------------------------'
        endif
        ITEMS(m+2)='+ add/delete/copy'
        ITEMS(m+3)='> save'
        ITEMS(m+4)='? help'
        ITEMS(m+5)='- exit'
        NITEMS=m+MCTL
      endif

      CALL EMENU('Agents',ITEMS,NITEMS,IPICK)

C Exit, offering to save unsaved data.
      if (IPICK.eq.NITEMS) then
        if (.not.saved) then
          CALL EASKABC('Modified data not saved!','Options:',
     &      'save and exit','exit without saving','cancel',itmp,nbhelp)
          if (itmp.eq.1) then
            CALL AGT_WT(IUF,IER)
            if (NAGT.gt.0) then
              IS_AGT=.true.
            else
              IS_AGT=.false.
            endif
          elseif (itmp.eq.3) then
            goto 4
          endif
        else
          if (NAGT.gt.0) then
            IS_AGT=.true.
          else
            IS_AGT=.false.
          endif
        endif
        goto 999

C Show help.
      elseif (IPICK.eq.NITEMS-1) then
        if (ILOC.eq.0) then
          helptopic='man_agents'
        else
          helptopic='occ_agents'
        endif
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('agents help',nbhelp,'-',0,0,IER)

C Save.
      elseif (IPICK.eq.NITEMS-2) then
        CALL AGT_WT(IUF,IER)
        saved=.true.

C Manage agents.
      elseif (IPICK.eq.NITEMS-3) then
        CALL EASKABCD(' ','Options:','add','delete','copy','cancel',
     &    itmp,nbhelp)

C Add an agent at end of list. First check that it doesn't exceed the
C maximum.
        if (itmp.eq.1) then
          if (NAGT+1.gt.MAGT) then
            call USRMSG('cannot add another','maximum reached',
     &        '-')
            goto 4
          endif
          ityp=0
          call AGT_PIKTYP(ILOC,ityp,ier)
          NAGT=NAGT+1
          if (ityp.gt.0.and.ier.eq.0) then
            AGTTYP(NAGT)=ityp
          else
            ier=0
            if (ILOC.eq.0) then
              AGTTYP(NAGT)=2
            else
              AGTTYP(NAGT)=1
            endif
          endif
          AGTZON(NAGT)=ILOC
          if (saved) saved=.false.

C If the menu is paged, make sure the menu returns to the current page
C after recalculating paging.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6

C Delete an agent.
        elseif (itmp.eq.2) then
          if (nagtl.eq.0) then
            CALL USRMSG('None to delete!',' ','-')
            goto 4
          endif
          ind=nagtl
          CALL EASKI(ind,' ','Which index to delete?',1,'F',
     &      nagtl,'F',nagtl,'agent number',IER,nbhelp)
          itmp=indagt(ind)
          if (itmp.lt.NAGT) then
            do i=itmp+1,NAGT
              AGTZON(i-1)=AGTZON(i)
              AGTTYP(i-1)=AGTTYP(i)
            enddo
          endif
          AGTZON(NAGT)=0
          AGTTYP(NAGT)=0
          NAGT=NAGT-1
          if (saved) saved=.false.
          goto 6

C Copy an agent.
        elseif (itmp.eq.3) then
          if (NAGT+1.gt.MAGT) then
            call USRMSG('Cannot add another agent','maximum reached',
     &        '-')
            goto 4
          endif
          if (nagtl.eq.0) then
            CALL USRMSG('None to copy!',' ','-')
            goto 4
          endif
          ind=nagtl
          CALL EASKI(ind,' ','Which index to copy?',1,'F',
     &      nagtl,'F',nagtl,'agent number',IER,nbhelp)
          itmp=indagt(ind)
          NAGT=NAGT+1
          AGTZON(NAGT)=AGTZON(itmp)
          AGTTYP(NAGT)=AGTTYP(itmp)
          if (saved) saved=.false.
          
C If the menu is paged, make sure the menu returns to the current page
C after recalculating paging.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6
        endif

C Control paging.
      elseif (IPICK.eq.NITEMS-4) then
        if (IPFLG.gt.0) then
          IPACT=EDIT
          ILEN=nagtl
          CALL EKPAGE(IPACT)
        endif

C Change zone.
      elseif (IPICK.eq.1) then
        if (ILOC.gt.0) then
          CALL EASKGEOF('Select a zone',CFGOK,ILOC,'-',34,IER)
        endif

C Set log file.
      elseif (IPICK.eq.2) then
        write(ctmp,'(a72)')'../tmp/agent.log'
        call EASKS(AGTLOG,' ','Agent log file?',72,ctmp,
     &    'agent log file',IER,nbhelp)

C Edit an agent.
      elseif (IPICK.gt.MHEAD .and. IPICK.le.(NITEMS-MCTL)) then

C Set variable font.
        IMFS=IMFS+4
        call userfonts(IFS,ITFS,IMFS) ! variable font

        CALL KEYIND(NITEMS,IPICK,ind,io)
        itmp=indagt(ind)
        CALL AGT_ED(itmp,saved,IER)

C Return to monospace menu font.
        IMFS=IMFS-4
        call userfonts(IFS,ITFS,IMFS)

C If the menu is paged, make sure the menu returns to the current page.
        if (IPFLG.gt.0) then
          pagstr(1)=IST
          pagstr(2)=IPM
          get_pagstr=.true.
        endif
        goto 6
      endif

      goto 6

C Return to original menu font.
  999 IMFS=iimfs
      call userfonts(IFS,ITFS,IMFS)
      RETURN

      END

C ******************** AGT_ED
C Edit an agent.
C IAGT is the agent index.
C SAVED is a flag for passing back changes to the previous menu.

      SUBROUTINE AGT_ED(IAGT,SAVED,IER)

#include "agent.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON

      logical SAVED

      dimension ITEMS(7)
      character ITEMS*41
      
      helpinsub='agtprj'

C Set up menu.
    1 write(ITEMS(1),'(2a)')'a model: ',AGTDESC(AGTTYP(IAGT))
      if (AGTMAN(AGTTYP(IAGT))) then
        ITEMS(2)=' type: management'
      else
        ITEMS(2)=' type: occupant'
      endif
      itmp=AGTZON(IAGT)
      if (itmp.eq.0) then
        write(ITEMS(3),'(a)')' location: whole building'
      elseif (itmp.gt.0.and.itmp.le.NCOMP) then
        write(ITEMS(3),'(a,i2)')' location: zone ',itmp
      else
        write(ITEMS(3),'(a)')' location: UNKNOWN'
      endif
      write(ITEMS(4),'(2a)')' criteria: ',AGTCRI(AGTTYP(IAGT))
      ITEMS(5)  =' ----------------------------------------'
      ITEMS(6)  ='? help'
      ITEMS(7) ='- exit'
      NITEMS=7

      ipick=-4
      CALL EMENU('Edit agent',ITEMS,NITEMS,ipick)

C Exit.
      if (ipick.eq.NITEMS) then
        goto 999

C Help.
      elseif (ipick.eq.NITEMS-1) then
        helptopic='edit_agent'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('edit agents help',nbhelp,'-',0,0,IER)

C Pick an agent type.
      elseif (ipick.eq.1) then
        itmp=AGTTYP(IAGT)
        CALL AGT_PIKTYP(AGTZON(IAGT),itmp,IER)
        if (itmp.gt.0.and.ier.eq.0) then
          AGTTYP(IAGT)=itmp
        else
          ier=0
        endif
      endif

      goto 1

  999 RETURN
  
      end


C ******************** AGT_PIKTYP
C Pick an agent model.
C ILOC is the current location; 0 = building, >0 = zone
C ITYP is the chosen model.

      SUBROUTINE AGT_PIKTYP(ILOC,ITYP,IER)

#include "agent.h"
#include "epara.h"
#include "help.h"

      dimension ITEMS(5)
      character ITEMS*32
      character key*1
      integer indagtmod(NAGTMOD)
      
      helpinsub='agtprj'

C Set up multi-page menu.
    1 MHEAD=0
      MCTL=3
      n=0
      do i=1,NAGTMOD
        if ((ILOC.eq.0 .and. AGTALVL(i).eq.0) .or.
     &      (ILOC.gt.0 .and. AGTALVL(i).eq.1)) then
          n=n+1
          indagtmod(n)=i
        endif
      enddo
      ILEN=n
      IPACT=CREATE
      CALL EKPAGE(IPACT)
    
C Paginated part.
      m=MHEAD
      im=0
      do i=1,NAGTMOD
        if (AGTALVL(i).eq.-1 .or.
     &      (ILOC.eq.0 .and. AGTALVL(i).eq.0) .or.
     &      (ILOC.gt.0 .and. AGTALVL(i).eq.1)) then
          if (i.ge.IST.and.i.le.(IST+MIFULL)) then
            m=m+1
            im=im+1
            CALL EMKEY(im,key,IER)
            write(ITEMS(m),'(A1,1X,A30)')key,AGTDESC(i)
          endif
        endif
      enddo

C Control options.
      if (IPFLG.gt.0) then
        write(ITEMS(m+1),'(A7,I2,A4,I2,A17)')
     &    '0 page ',IPM,' of ',MPM,' --------------- '
      else
        ITEMS(m+1)=' -------------------------------'
      endif
      ITEMS(m+2)='? help'
      ITEMS(m+3)='- exit'
      NITEMS=m+MCTL   

      ipick=-4
      CALL EMENU('Available agent models',ITEMS,NITEMS,ipick)

C Exit.
      if (ipick.eq.NITEMS) then
        goto 999
    
C Help.
      elseif (ipick.eq.NITEMS-1) then
        helptopic='pick_agent'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('pick an agent model',nbhelp,'-',0,0,IER)
        
C Control paging.
      elseif (ipick.eq.NITEMS-2) then
        if (IPFLG.gt.0) then
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        endif

C Picked a type.
      elseif (ipick.gt.MHEAD .and. ipick.le.(NITEMS-MCTL)) then
        CALL KEYIND(NITEMS,IPICK,ifoc,io)
        ITYP=indagtmod(ifoc)
        goto 999
      endif

      goto 1

  999 RETURN
  
      end
      

      
