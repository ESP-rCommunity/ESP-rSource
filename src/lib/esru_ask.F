C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-7.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C Licence as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public Licence for more
C details.

C You should have received a copy of the GNU General Public
C Licence along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C esru_ask.F includes notices used by ESP-r modules.

C   cpwpk()    Displays a copyright notice via help text data structure.
C   askabout() Creates text for a popup notice for each module.

C ***** copyright notice.
C cpwpk displays a copyright notice via help text data structure.
C It saves and recover any current contextual help messages.
C The integer parameter il is not used.
      subroutine cpwpk(il)
#include "help.h"
      dimension dh(60)
      character dh*72

C First copy any existing help into dh array.
      do 50 i=1,60
        dh(i)=h(i)
   50 continue
      helpinsub='lib'  ! set for subroutine
      helptopic='GPL_license'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('ESP-r license',nbhelp,'-',0,0,IER)

C Recover previous help via dh array.
      do 51 i=1,60
        h(i)= dh(i)
   51 continue

      return
      end

C **** askabout() Creates text for a popup notice for each module. ***
C The intent of askabout is to have one place for the opening help
C message of each applicaton (and thus support an application independent
C static help menu under GTK).
C If initonly is 1 then only assign the h() array and do not display
C If initonly is 0 save prior h(), update h(), display and re-establish prior.
C module (char *4) identifies the ESP-r module.
      subroutine askabout(module,initonly)
#include "help.h"
      dimension dh(60)
      character dh*72,module*4

C First copy any existing help into dh array.
      if(initonly.eq.0)then

        do 50 i=1,60
          dh(i)=h(i)
   50   continue
      else

      endif
      helpinsub='lib'  ! set for subroutine

      if(module(1:3).eq.'aco')then
        helptopic='module_aco'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('aco opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'b2e')then
        helptopic='module_b2e'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('b2e opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'bps')then
        helptopic='module_bps'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('bps opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'cfg')then
        helptopic='module_cfg'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('cfg opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'cdb')then
        helptopic='module_cdb'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('cdb opening',nbhelp,'-',0,0,IER)
        endif
C      elseif(module(1:3).eq.'clm')then
C Help moved to esruclm.help
C        if(initonly.eq.0)then
C          CALL PHELPD('clm opening',24,'clm_dbitems ',0,0,IER)
C        endif
      elseif(module(1:3).eq.'dbm')then
        helptopic='module_dbm'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('dbm opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'dfs')then
        helptopic='module_dfs'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('dfs opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'dfv')then
        helptopic='module_dfv'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('dfv opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:4).eq.'ecnv')then
        helptopic='module_ecnv'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('ecnv opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'e2r')then
        helptopic='module_e2r'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('e2r opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'eco')then
        helptopic='module_eco'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('eco opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'grd')then
        helptopic='module_grd'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('grd opening',2,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'ish')then
        helptopic='module_ish'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('ish opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mfs')then
        helptopic='module_mfs'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('mfs opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mld')then
        helptopic='module_mld'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('mld opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mrt'.or.module(1:3).eq.'vwf')then
        helptopic='module_mrt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('vwf opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'net'.or.module(1:4).eq.'enet')then
        helptopic='module_net'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('enet opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'pdb')then
        helptopic='module_pdb'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('pdb opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'plt')then
        helptopic='module_plt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('plt opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'pro')then
        helptopic='module_pro'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('pro opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'res')then
        helptopic='module_res'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('res opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'tdf')then
        continue  ! tdf functionality has been moved into esruprj
      elseif(module(1:3).eq.'vew')then
        helptopic='module_vew'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('vew opening',nbhelp,'-',0,0,IER)
        endif
      elseif(module(1:4).eq.'wire')then
        helptopic='module_wire'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('wirefram ctl',nbhelp,'-',0,0,IER)
        endif
      else
        helptopic='module_unknown'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(initonly.eq.0)then
          CALL PHELPD('ask about mismatch',nbhelp,'-',0,0,IER)
        endif
      endif

C Recover previous help via dh array.
      if(initonly.eq.0)then
        do 51 i=1,60
          h(i)= dh(i)
   51   continue
      endif
      return
      end

C test subroutinhe to capture help text from external file (to be
C found in the ESP-r distribution lib folder) with file ending
C of *.help
      subroutine gethelptext(sub,topic,nbhelp)
#include "espriou.h"
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters.
      character sub*24   ! subroutine name
      character topic*24 ! string (unique) for topic

C Commons.
      integer ifil
      COMMON/FILEP/IFIL
      integer iuout,iuin,ieout
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Local variables.
      logical unixok
      integer ln,lna  ! length of install path, app name app folder
      integer lns,lnt,lnhl ! length of subroutine, topic, helpline
      integer lno          ! length of outstr
      integer K            ! position
      integer trywhich     ! if one then primary help file if two use secondary

      character helpfil*96 ! the help file to load
      dimension folder(4),lnf(4)  ! up to 3 subsequent folders to look in and their lengths
      character folder*8   ! the source folder for the application
      integer lnf
      character WORD*32    ! a token to read
      character outstr*124 ! for help phrases
      character outs*124 ! for messages
      character helpline*72 ! line of help text
      character sq*1        ! single quote
      character fs*1
      logical havesub       ! set true if subroutine matches
      logical havetopic     ! set true if topic matches
      character app*24      ! application name (local)
#include "espinstalldir.h"

      helpinsub='lib'      ! not currently used
      helptopic='gethelptext'
      write(outs,'(3a)') helpinsub,' ',helptopic

C Get to work.
      app = helpinapp      ! set local app
      trywhich = 1         ! assume primary help file
      call isunix(unixok)  ! which folder separator to use
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      ln=lnblnk(instpath)  ! where is ESP-r installed
      lna=lnblnk(app)      ! length of app name
      folder(1)='UNKNOWN'
      folder(2)='UNKNOWN'
      folder(3)='UNKNOWN'
      folder(4)='UNKNOWN'
      if(app(1:3).eq.'aco')then
        folder='esruaco'
        folder(2)='common'
      elseif(app(1:3).eq.'cdb')then
        folder(1)='esrucdb'
        folder(2)='common'
      elseif(app(1:3).eq.'clm')then
        folder(1)='esruclm'
        folder(2)='common'
      elseif(app(1:3).eq.'bps')then
        folder(1)='esrubps'
        folder(2)='common'
      elseif(app(1:3).eq.'dbm')then
        folder(1)='esrudbm'
        folder(2)='common'
      elseif(app(1:3).eq.'dfs')then
        folder(1)='esrudfs'
        folder(2)='common'
      elseif(app(1:3).eq.'dfv')then
        folder(1)='esrudfs'
        folder(2)='common'
      elseif(app(1:3).eq.'e2r')then
        folder(1)='esrue2r'
        folder(2)='common'
      elseif(app(1:3).eq.'eco')then
        folder(1)='esrueco'
        folder(2)='common'
      elseif(app(1:4).eq.'ecnv')then
        folder(1)='esrucnv'
        folder(2)='common'
      elseif(app(1:3).eq.'grd')then
        folder(1)='esrugrd'
        folder(2)='common'
      elseif(app(1:3).eq.'ish')then
        folder(1)='esruish'
        folder(2)='common'
      elseif(app(1:3).eq.'mfs')then
        folder(1)='esrumfs'
        folder(2)='common'
      elseif(app(1:3).eq.'mld')then
        folder(1)='esrumld'
        folder(2)='common'
      elseif(app(1:3).eq.'mrt')then
        folder(1)='esrumrt'
        folder(2)='common'
      elseif(app(1:3).eq.'net')then
        folder(1)='esrunet'
        folder(2)='common'
      elseif(app(1:3).eq.'pdb')then
        folder(1)='esrupdb'
        folder(2)='common'
      elseif(app(1:3).eq.'prj')then
        folder(1)='esruprj'
        folder(2)='esrumfs'
        folder(3)='esrudfs'
        folder(4)='common'
      elseif(app(1:3).eq.'res')then
        folder(1)='esrures'
        folder(2)='esrumfs'
        folder(3)='esrubps'
        folder(4)='common'
      elseif(app(1:3).eq.'vew')then
        folder(1)='esruvew'
        folder(2)='common'
      endif
      lnf(1)=lnblnk(folder(1))   ! length of folder name
      lnf(2)=lnblnk(folder(2))   ! length of folder name
      lnf(3)=lnblnk(folder(3))   ! length of folder name
      lnf(4)=lnblnk(folder(4))   ! length of folder name
      lns=lnblnk(sub)      ! length of subroutine
      lnt=lnblnk(topic)    ! length of topic
      sq=char(39)   ! assign single quote

C Assume that the installer has placed the help text file in the
C distribution lib folder.
  42  if(trywhich.eq.1)then
        write(helpfil,'(6a)') instpath(1:ln),fs,'lib',fs,
     &    folder(1)(1:lnf(1)),'.help'
C        call edisp(iuout,helpfil)   ! debug
      elseif(trywhich.ge.2)then
        write(helpfil,'(6a)') instpath(1:ln),fs,'lib',fs,
     &    folder(trywhich)(1:lnf(trywhich)),'.help'
C        call edisp(iuout,helpfil)   ! debug
      endif
      if(folder(1)(1:7).eq.'esrubps')then
        ihfil=ifil+8              ! set file unit number for bps
      elseif(folder(1)(1:7).eq.'esruprj')then
        ihfil=ifil+11              ! set file unit number for prj
      elseif(folder(1)(1:7).eq.'esrures')then
        ihfil=ifil+8              ! set file unit number for res
      elseif(folder(1)(1:7).eq.'esrumrt')then
        ihfil=ifil+8              ! set file unit number for mrt
      elseif(folder(1)(1:7).eq.'esruish')then
        ihfil=ifil+8              ! set file unit number for ish
      else
        ihfil=ifil+2              ! set file unit number
      endif
      CALL ERPFREE(ihfil,ISTAT) ! free it
      CALL EFOPSEQ(ihfil,helpfil,1,IER)
      if(ier.ne.0)then
        write(outs,'(2a )')' Could not find help text file: ',
     &    helpfil(1:lnblnk(helpfil))
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        nbhelp=-1
        return
      endif

      nbhelp=0  ! set initial assumption
      havesub= .false.
      havetopic= .false.

C Check the header to see if this is a help text file.
      read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan header
      if(ios.ne.0) goto 1   ! jump if EOF
      K=0
      CALL EGETW(outstr,K,WORD,'W','*help_text',IER)
      if(WORD(1:10).ne.'*help_text')then
        call edisp(iuout,'Not a help text file.')
        call edisp(iuout,helpfil)
        goto 1
      endif
      CALL EGETW(outstr,K,WORD,'W','application',IER)

C If we are looking in the primary help file check the app name.
C Subsequent help texts will not have a matching app line.
      if(trywhich.eq.1)then
        if(WORD(1:3).eq.'dfv'.and.app(1:3).eq.'dfs')then
          continue  ! dfv uses dfs help text
        elseif(WORD(1:3).eq.'dfs'.and.app(1:3).eq.'dfv')then
          continue  ! dfv uses dfs help text
        elseif(WORD(1:lna).ne.app(1:lna))then
          write(outs,'(3a)') WORD(1:lna),
     &      ' is not a help text file for ',app(1:lna)
          call edisp(iuout,outs)
          goto 1
        endif
      endif

C If we reached this point then we can go looking for a
C subroutine and topic match on each new line.
      scan: do
        read(ihfil,'(a)',IOSTAT=IOS,ERR=1) outstr   ! scan line
        if(ios.ne.0)  exit scan   ! jump if EOF
        lno=lnblnk(outstr)
        if(lno.le.1) cycle  ! jump if a blank line

        if(outstr(1:1).eq.'*')then  ! we had a key phrase
          K=0
          CALL EGETW(outstr,K,WORD,'W','*item',IER)
          if(WORD(1:5).eq.'*item')then
            CALL EGETW(outstr,K,WORD,'W','subroutine',IER)
            if(WORD(1:lns).eq.sub(1:lns))then

C We have a matching subroutine name remember this and if
C there are still more characters in the line then it
C probably is a topic.
              havesub = .true.  ! set true subroutine matches
              if(K.le.lno)then
                CALL EGETW(outstr,K,WORD,'W','topic',IER)
                if(WORD(1:lnt).eq.topic(1:lnt))then
                  havetopic = .true.     ! set true topic matches
                  cycle   ! keep scanning
                else
                  cycle   ! keep scanning
                endif
              else
                cycle   ! keep scanning
              endif
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:8).eq.'*enditem')then

C Reached end of item exit scan, if this was the correct sub and topic then
C set number of help lines.
            if(havesub.and.havetopic)then
              CALL ERPFREE(ihfil,ISTAT) ! free it
              return
            else
              cycle  ! keep scanning
            endif
          elseif(WORD(1:6).eq.'*topic')then

C In case there are some old *topic lines in the help f
            if(lno.le.7) cycle  ! jump if no other token
            K=7
            CALL EGETW(outstr,K,WORD,'W','topic',IER)
            if(WORD(1:lnt).eq.topic(1:lnt))then
              havetopic = .true.     ! set true topic matches
              cycle   ! keep scanning
            else
              cycle   ! keep scanning
            endif
          elseif(WORD(1:11).eq.'*subroutine')then
            cycle   ! keep scanning
          endif

        elseif(outstr(1:1).eq.sq)then

C We have a line that starts with a quote. If we have reached the
C correct topic then read it otherwise scan another line.
          if(havesub.and.havetopic)then
            K=0
            call EGETDQ(outstr,K,helpline,'W','phrase',IER)
            lnhl=lnblnk(helpline)
C Debug (to text feedback).
C            call edisp(iuout,helpline)
            if(nbhelp.lt.60)then
              nbhelp=nbhelp+1
              if(lnhl.le.1)then
                h(nbhelp)='  '
              else
                write(h(nbhelp),'(a)') helpline(1:lnhl)
              endif
            endif
            cycle   ! keep scanning
          else
            cycle   ! keep scanning
          endif
        else
 
C Does not match expectations.
          cycle   ! keep scanning
        endif
      end do scan

C The end of file reached. If there has not been a match try other
C likely help files for the current application. If that fails
C make up a standard message.
  1   close(ihfil)
      if(havesub.and.havetopic)then
        return
      else

C Check if there is an alternative help text file. If not give up.
        if(trywhich.eq.1)then
          if(folder(2)(1:7).eq.'UNKNOWN')then
            h(1)='No contextual help text was found for the topic'
            write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &        ' of application ',app(1:lna)
            nbhelp=2
            return
          endif
          trywhich=trywhich+1
          goto 42  ! try scaning the alternative file
        elseif(trywhich.eq.2)then
          if(folder(3)(1:7).eq.'UNKNOWN')then
            h(1)='No alt contextual help text was found for the topic'
            write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &        ' of application ',app(1:lna)
            nbhelp=2
            return
          endif
          trywhich=trywhich+1
          goto 42  ! try scaning the alternative file
        elseif(trywhich.eq.3)then
          if(folder(4)(1:7).eq.'UNKNOWN')then
            h(1)='No alt contextual help text was found for the topic'
            write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &        ' of application ',app(1:lna)
            nbhelp=2
            return
          endif
          trywhich=trywhich+1
          goto 42  ! try scaning the alternative file
        elseif(trywhich.eq.4)then
          continue
        endif
        h(1)='No help found for this topic!'
        write(h(2),'(5a)') topic(1:lnt),' in ',sub(1:lns),
     &    ' of application ',app(1:lna)
        nbhelp=2
        return
      endif

      end

C subroutine to generate line of help text for an integer dialog
C in terms of default and current value. 
      subroutine helpcurrentint(idef,icur,ermsg,nbhelp,newnbhelp,ier)
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters.
      integer idef         ! default value for this number
      integer icur         ! current value
      CHARACTER*(*) ERMSG  ! error message from dialog
      integer newnbhelp    ! number of help lines after additional line(s)
      integer ier          ! zero is ok

C Commons.
      integer iuout,iuin,ieout
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Local variables.
      integer ln           ! length of error message
      integer iw2,iw1      ! length of two integers
      character outs*124 ! for messages
      CHARACTER STR1*10,STR2*10

      helpinsub='lib'      ! not currently used
      helptopic='helpcurrentint'
      write(outs,'(3a)') helpinsub,' ',helptopic

C Get to work.
      CALL INTSTR(icur,STR2,IW2,IER)
      CALL INTSTR(idef,STR1,IW1,IER)
      IF(nbhelp.GT.0)THEN
        newnbhelp=nbhelp+1
        WRITE(H(nbhelp+1),65,iostat=ios,err=1)STR2(1:IW2),STR1(1:IW1)
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        newnbhelp=2
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'!')
        WRITE(H(2),65,iostat=ios,err=1)STR2(1:IW2),STR1(1:IW1)
   65   FORMAT('Current value is (',a,'); Default value is (',a,').')
      ENDIF

      return

   1  if(IOS.eq.2)then
        write(outs,*) 'helpcurretint: permission error composing help.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'helpcurretint: error composing help.'
        call edisp(iuout,outs)
      endif
      return
      end


C subroutine to generate line of help text for an real dialog
C in terms of default and current value. 
      subroutine helpcurrentreal(def,cur,ermsg,nbhelp,newnbhelp,ier)
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters.
      real def         ! default value for this number
      real cur         ! current value
      CHARACTER*(*) ERMSG  ! error message from dialog
C      integer nbhelp       ! original nb of help lines
      integer newnbhelp    ! number of help lines after additional line(s)
      integer ier          ! zero is ok

C Commons.
      integer iuout,iuin,ieout
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Local variables.
      integer ln           ! length of error message
      integer iw2,iw1      ! length of two integers
      character outs*124 ! for messages
      CHARACTER STR1*16,STR2*16

      helpinsub='lib'      ! not currently used
      helptopic='helpcurrentreal'
      write(outs,'(3a)') helpinsub,' ',helptopic

C Get to work.
      CALL REL16STR(def,STR1,IW1,IER)
      CALL REL16STR(cur,STR2,IW2,IER)
      IF(nbhelp.GT.0)THEN
        newnbhelp=nbhelp+1
        WRITE(H(nbhelp+1),65,iostat=ios,err=1)STR2(1:IW2),STR1(1:IW1)
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        newnbhelp=2
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'!')
        WRITE(H(2),65,iostat=ios,err=1)STR2(1:IW2),STR1(1:IW1)
   65   FORMAT('Current value is (',a,'); Default value is (',a,').')
      ENDIF

      return

   1  if(IOS.eq.2)then
        write(outs,*) 
     &    'helpcurrentreal: permission error composing help.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'helpcurrentreal: error composing help.'
        call edisp(iuout,outs)
      endif
      return
      end

C subroutine to generate line of help text for an string dialog
C if no lines of help have been defined in the calling code. 
      subroutine helpwithblank(ermsg,nbhelp,newnbhelp,ier)
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters.
      CHARACTER*(*) ERMSG  ! error message from dialog
      integer newnbhelp    ! number of help lines after additional line(s)
      integer ier          ! zero is ok

C Commons.
      integer iuout,iuin,ieout
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Local variables.
      integer ln           ! length of error message
      character outs*124   ! for messages

      helpinsub='lib'      ! not currently used
      helptopic='helpwithblank'
      write(outs,'(3a)') helpinsub,' ',helptopic

C Get to work.
      IF(nbhelp.GT.0)THEN
        newnbhelp=nbhelp  ! nothing to do
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        newnbhelp=1
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'!')
      ENDIF

      return

   1  if(IOS.eq.2)then
        write(outs,*) 
     &    'helpwithblank: permission error composing help.'
        call edisp(iuout,outs)
        ier=2
      else
        write(outs,*) 'helpwithblank: error composing help.'
        call edisp(iuout,outs)
        ier=1
      endif
      return
      end
