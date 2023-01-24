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
C GRM_WT     - Write gremlin specification file.
C GRM_MNG    - Controlling routine for defining and editing gremlins.
C GRM_ED     - Edit a gremlin.
C GRM_PIKTYP - Pick a gremlin model from a list.

C ******************** GRM_WT ********************
C Write gremlin data to file unit IUNIT.
C Data format is *tag value.

      subroutine GRM_WT(IUNIT,IER)

#include "gremlin.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      character*124 outs

C Open file.
      CALL EFOPSEQ(IUNIT,GRMFIL,4,IER)
      if (IER.ne.0) goto 666

C Write header line.
      write(IUNIT,'(a)',ERR=666)'# Gremlin specification file'

C Write file version.
      write(IUNIT,'(a,i2)',ERR=666)'*version ',GRMVSN

C Write number of gremlins.
      write(IUNIT,'(a,i2)',ERR=666)'*numGremlins ',NGRM

C Write log file path.
      write(IUNIT,'(2a)',ERR=666)'*logFile ',GRMLOG(1:lnblnk(GRMLOG))

C For each gremlin ...
      do igrm=1,NGRM

C ... write spacers.
        write(IUNIT,'(a)',ERR=666)'#'
        write(IUNIT,'(a,i2,a)',ERR=666)'# - gremlin ',igrm,' -'

C ... write model.
        write(IUNIT,'(a,i2)',ERR=666)'*type ',GRMTYP(igrm)
        
C ... write zone.
        write(IUNIT,'(a,i2)',ERR=666)'*zone ',GRMZON(igrm)

      enddo

C Close file and exit.
  999 call ERPFREE(IUNIT,IER)
      RETURN

C Error.
  666 outs='GRM_WT: Error while writing gremlin specification file.'
      call edisp(IUOUT,outs)
      goto 999

      end
    
C ******************** GRM_MNG
C Manages the definition and editing of gremlins (called random
C perturbations in the interface).
C ILOC is the location; 0 for building, >0 for a specific zone.

      SUBROUTINE GRM_MNG(ILOC,IER)

#include "gremlin.h"
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
      character ITEMS*39,item_tmp*56,outs*124,fs*1,ctmp*72,grmfil_tmp*72
      character key*1,isact*3,snpfile*72,sfile*72
      logical unixok,xst,saved,isnone,get_pagstr
      integer pagstr(2),indgrm(MGRM)
      
      helpinsub='grmprj'
      
C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Ask for gremlin file name.
    3 write(ctmp,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.grm'
      itmp=0
      if (GRMFIL(1:7).eq.'UNKNOWN') then
        xst=.false.
        grmfil_tmp=ctmp
        CALL EASKS2CMD(grmfil_tmp,'random perturbation file',' ',
     &    'cancel','browse',itmp,72,ctmp,'gremlin file',IER,0)
      else
        xst=.true.
        grmfil_tmp=GRMFIL
        CALL EASKS2CMD(grmfil_tmp,'random perturbation file',' ',
     &    'dereference','browse',itmp,72,ctmp,'gremlin file',IER,0)
      endif      
      CALL USRMSG(' ',' ','-')

C Cancelled or dereferenced.
C If dereferenced, deactivate gremlins.
      if (itmp.eq.1) then
        if (xst) then
          CALL GRM_INIT
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
            write(grmfil_tmp,'(3a)')snpfile(1:lnblnk(snpfile))
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
      CALL FINDFIL(grmfil_tmp,xst)
      if (xst) then
        write(currentfile,'(a)')grmfil_tmp(1:lnblnk(grmfil_tmp))
        CALL EFOPSEQ(IUF,grmfil_tmp,1,istat)
        if (istat.ne.0) then
          call edisp(iuout,'Error opening file.')
          goto 3
        endif
        CALL GRM_RD(IUF,IER)
      else
        CALL EASKAB('File not found.','Options:',
     &    'specify another','make new file',itmp,0)
        if (itmp.eq.1) goto 3
        write(outs,'(3a)')'New file ',
     &    grmfil_tmp(1:lnblnk(grmfil_tmp)),' will be created.'
        CALL edisp(iuout,outs)
      endif

C If we've made it this far, we should have a valid file name.
C Save this to common.
      GRMFIL=grmfil_tmp

C Switch to monospace menu font.
      iimfs=IMFS
      if (IMFS.ge.4 .and. IMFS.le.7) IMFS=IMFS-4
      call userfonts(IFS,ITFS,IMFS)

C Set up (possibly) multi-page menu.
      get_pagstr=.false.
      saved=.true.
  6   MHEAD=3
      MCTL=5
      ngrml=0
      do i=1,NGRM
        if (ILOC.eq.GRMZON(i)) then
          ngrml=ngrml+1
          indgrm(ngrml)=i
        endif
      enddo
      if (NGRM.eq.0) then
        isnone=.true.
      else
        isnone=.false.
      endif
      ILEN=ngrml
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
      if (lnblnk(GRMLOG).gt.27) then
        write(ITEMS(2),'(3a)') '2 log file: ',GRMLOG(1:24),'...'
      else
        write(ITEMS(2),'(2a)') '2 log file: ',GRMLOG(1:lnblnk(GRMLOG))
      endif

      if (isnone) then
        ITEMS(3)=' no perturbations at this location'
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
        do i=1,NGRM
          if (ILOC.eq.GRMZON(i)) then
            if (i.ge.IST.and.i.le.(IST+MIFULL)) then
              m=m+1
              im=im+1
              CALL EMKEY(im,key,IER)
              write(ITEMS(m),'(A1,1X,I4,3X,A30)')
     &          key,im,GRMDESC(GRMTYP(i))(1:30)
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

      CALL EMENU('Random perturbations',ITEMS,NITEMS,IPICK)

C Exit, offering to save unsaved data.
      if (IPICK.eq.NITEMS) then
        if (.not.saved) then
          CALL EASKABC('Modified data not saved!','Options:',
     &      'save and exit','exit without saving','cancel',itmp,nbhelp)
          if (itmp.eq.1) then
            CALL GRM_WT(IUF,IER)
            if (NGRM.gt.0) then
              IS_GRM=.true.
            else
              IS_GRM=.false.
            endif
          elseif (itmp.eq.3) then
            goto 4
          endif
        else
          if (NGRM.gt.0) then
            IS_GRM=.true.
          else
            IS_GRM=.false.
          endif
        endif
        goto 999

C Show help.
      elseif (IPICK.eq.NITEMS-1) then
        helptopic='gremlins'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('random perturbations help',nbhelp,'-',0,0,IER)

C Save.
      elseif (IPICK.eq.NITEMS-2) then
        CALL GRM_WT(IUF,IER)
        saved=.true.

C Manage gremlins.
      elseif (IPICK.eq.NITEMS-3) then
        CALL EASKABCD(' ','Options:','add','delete','copy','cancel',
     &    itmp,nbhelp)

C Add a gremlin at end of list. First check that it doesn't exceed the
C maximum.
        if (itmp.eq.1) then
          if (NGRM+1.gt.MGRM) then
            call USRMSG('cannot add another','maximum reached',
     &        '-')
            goto 4
          endif
          ityp=0
          CALL GRM_PIKTYP(ILOC,ityp,ier)
          NGRM=NGRM+1
          if (ityp.gt.0.and.ier.eq.0) then
            GRMTYP(NGRM)=ityp
          else
            ier=0
            if (ILOC.eq.0) then
              GRMTYP(NGRM)=1
            else
              call USRMSG('Currently no zone-level',
     &                    'perturbations available','-')
              NGRM=NGRM-1
              goto 6
            endif
          endif
          GRMZON(NGRM)=ILOC
          if (saved) saved=.false.

C If the menu is paged, make sure the menu returns to the current page
C after recalculating paging.
          if (IPFLG.gt.0) then
            pagstr(1)=IST
            pagstr(2)=IPM
            get_pagstr=.true.
          endif
          goto 6

C Delete a gremlin.
        elseif (itmp.eq.2) then
          if (ngrml.eq.0) then
            CALL USRMSG('None to delete!',' ','-')
            goto 4
          endif
          ind=ngrml
          CALL EASKI(ind,' ','Which index to delete?',1,'F',
     &      ngrml,'F',ngrml,'gremlin number',IER,nbhelp)
          itmp=indgrm(ind)
          if (itmp.lt.NGRM) then
            do i=itmp+1,NGRM
              GRMZON(i-1)=GRMZON(i)
              GRMTYP(i-1)=GRMTYP(i)
            enddo
          endif
          GRMZON(NGRM)=0
          GRMTYP(NGRM)=0
          NGRM=NGRM-1
          if (saved) saved=.false.
          goto 6

C Copy a gremlin.
        elseif (itmp.eq.3) then
          if (NGRM+1.gt.MGRM) then
            call USRMSG('Cannot add another','maximum reached',
     &        '-')
            goto 4
          endif
          if (ngrml.eq.0) then
            CALL USRMSG('None to copy!',' ','-')
            goto 4
          endif
          ind=ngrml
          CALL EASKI(ind,' ','Which index to copy?',1,'F',
     &      ngrml,'F',ngrml,'gremlin number',IER,nbhelp)
          itmp=indgrm(ind)
          NGRM=NGRM+1
          GRMZON(NGRM)=GRMZON(itmp)
          GRMTYP(NGRM)=GRMTYP(itmp)
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
          ILEN=ngrml
          CALL EKPAGE(IPACT)
        endif

C Change zone.
      elseif (IPICK.eq.1) then
        if (ILOC.gt.0) then
          CALL EASKGEOF('Select a zone',CFGOK,ILOC,'-',34,IER)
        endif

C Set log file.
      elseif (IPICK.eq.2) then
        write(ctmp,'(a72)')'../tmp/rand_pert.log'
        call EASKS(GRMLOG,' ','Random perturbation log file?',72,ctmp,
     &    'gremlin log file',IER,nbhelp)

C Edit a gremlin.
      elseif (IPICK.gt.MHEAD .and. IPICK.le.(NITEMS-MCTL)) then

C Set variable font.
        IMFS=IMFS+4
        call userfonts(IFS,ITFS,IMFS) ! variable font

        CALL KEYIND(NITEMS,IPICK,ind,io)
        itmp=indgrm(ind)
        CALL GRM_ED(itmp,saved,IER)

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

C ******************** GRM_ED
C Edit a gremlin.
C IGRM is the gremlin index.
C SAVED is a flag for passing back changes to the previous menu.

      SUBROUTINE GRM_ED(IGRM,SAVED,IER)

#include "gremlin.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/C1/NCOMP,NCON

      logical SAVED

      dimension ITEMS(9)
      character ITEMS*42
      
      helpinsub='grmprj'

C Set up menu.
    1 write(ITEMS(1),'(2a)')'a model: ',GRMDESC(GRMTYP(IGRM))
      itmp=GRMZON(IGRM)
      if (itmp.eq.0) then
        write(ITEMS(2),'(a)')' location: whole building'
      elseif (itmp.gt.0.and.itmp.le.NCOMP) then
        write(ITEMS(2),'(a,i2)')' location: zone ',itmp
      else
        write(ITEMS(2),'(a)')' location: UNKNOWN'
      endif
      write(ITEMS(3),'(2a)')' event: ',GRMEDTL(GRMTYP(IGRM))
      write(ITEMS(4),'(2a)')' frequency: ',GRMFREQ(GRMTYP(IGRM))
      write(ITEMS(5),'(2a)')' criteria: ',GRMCRI(GRMTYP(IGRM))
      write(ITEMS(6),'(2a)')' duration: ',GRMDUR(GRMTYP(IGRM))
      ITEMS(7)  =' -----------------------------------------'
      ITEMS(8)  ='? help'
      ITEMS(9) ='- exit'
      NITEMS=9

      ipick=-4
      CALL EMENU('Edit random perturbation',ITEMS,NITEMS,ipick)

C Exit.
      if (ipick.eq.NITEMS) then
        goto 999

C Help.
      elseif (ipick.eq.NITEMS-1) then
        helptopic='edit_gremlin'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('edit perturbation help',nbhelp,'-',0,0,IER)

C Pick a gremlin type.
      elseif (ipick.eq.1) then
        itmp=GRMTYP(IGRM)
        CALL GRM_PIKTYP(GRMZON(IGT),itmp,IER)
        if (itmp.gt.0.and.ier.eq.0) then
          GRMTYP(IGRM)=itmp
        else
          ier=0
        endif
      endif

      goto 1

  999 RETURN
  
      end

C ******************** GRM_PIKTYP
C Pick a gremlin model.
C ILOC is the current level; 0 = building, >0 = zone
C ITYP is the chosen model.

      SUBROUTINE GRM_PIKTYP(ILOC,ITYP,IER)

#include "gremlin.h"
#include "epara.h"
#include "help.h"

      dimension ITEMS(5)
      character ITEMS*32
      character key*1
      integer indgrmmod(NGRMMOD)
      
      helpinsub='grmprj'

C Set up multi-page menu.
    1 MHEAD=0
      MCTL=3
      n=0
      do i=1,NGRMMOD
        if ((ILOC.eq.0 .and. GRMALVL(i).eq.0) .or.
     &      (ILOC.gt.0 .and. GRMALVL(i).eq.1)) then
          n=n+1
          indgrmmod(n)=i
        endif
      enddo
      ILEN=n
      IPACT=CREATE
      CALL EKPAGE(IPACT)
    
C Paginated part.
      m=MHEAD
      im=0
      do i=1,NGRMMOD
        if (GRMALVL(i).eq.-1 .or.
     &      (ILOC.eq.0 .and. GRMALVL(i).eq.0) .or.
     &      (ILOC.gt.0 .and. GRMALVL(i).eq.1)) then
          if (i.ge.IST.and.i.le.(IST+MIFULL)) then
            m=m+1
            im=im+1
            CALL EMKEY(im,key,IER)
            write(ITEMS(m),'(A1,1X,A30)')key,GRMDESC(i)
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
      CALL EMENU('Available perturbation models',ITEMS,NITEMS,ipick)

C Exit.
      if (ipick.eq.NITEMS) then
        goto 999
    
C Help.
      elseif (ipick.eq.NITEMS-1) then
        helptopic='pick_gremlin'
        CALL gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('pick a perturbation model',nbhelp,'-',0,0,IER)
        
C Control paging.
      elseif (ipick.eq.NITEMS-2) then
        if (IPFLG.gt.0) then
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        endif

C Picked a type.
      elseif (ipick.gt.MHEAD .and. ipick.le.(NITEMS-MCTL)) then
        CALL KEYIND(NITEMS,IPICK,ifoc,io)
        ITYP=indgrmmod(ifoc)
        goto 999
      endif

      goto 1

  999 RETURN
  
      end
      

      
