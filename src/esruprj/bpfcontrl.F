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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C Configuration control file definition.
C This file contains the following subroutines:
C CONTRL : Principal interfacing subroutine that builds menus and
C          governs user interaction
C TOTCTLPER : Finds current number of periods in all control loops
C             of type icfoc.
C EBCNTRL : Edit building control function
C EPCNTRL : Edit plant control function
C EFCNTRL : Edit flow control function
C EGCNTRL : Edit global control function
C EOCNTRL: Edit optical control function
C edit_CFC_control: Edit complex fenestration control function
C SENLOC : Returns sensor location depending upon control domain
C ACTLOC : Returns actuator location depending upon control domain
c ASKMFCTM : Asks for one or more contaminants
C Editctlperiod: generic menu for editing details of a control
C                period for a zone control (later flow control).
C STFMENU: Copies menu and prompts for period editing.  It expects the common
C          block sctl to have been filled for the current period.
C check_CFC_control_validity: Checks complex fenestration control function
C                             data for validity.

      SUBROUTINE CONTRL(ITRC,ITRU,icfoc,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"
#include "net_flow.h"
#include "control.h"
#include "CFC_common.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      integer ncomp,ncon
      common/c1/ncomp,ncon
      integer icascf
      common/cctl/icascf(mcom)
      common/cctlnm/ctldoc,lctlf
      common/fctl4/iasocc(MCF,mcmp),nfsup(MCF)

      common/user/browse
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER nbdaytype,nbcaldays,icalender
     
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C mptback is pointer from position in display list to the control loop.
C ipeerptbk is pointer from position in display to day type.
      dimension mptback(35),iperptbk(35)
      CHARACTER*56 VERT(35)
      CHARACTER CTLDOC*248,LCTLF*72,KEY*1
      CHARACTER outs*124,LASCI*72
      character t248*248,SSTR*96
      character sfile*72,snpfile*72,fs*1
      logical LINKED,OK,dok,XST,browse,unixok
      integer icomp
      integer IW   ! for radio button
      integer MVERT,IVERT ! max items and current menu item
      logical CFCfilexists,CFCcontrolOK

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CONTRL'  ! set for subroutine
      
C Zeroize all control data.
      CALL EZCTLI
      CTLDOC  ='no overall control description supplied'
      znctldoc='no zone control description supplied'
      plctldoc='no plant control description supplied'
      flctldoc='no flow control description supplied'
      glctldoc='no global control description supplied'
      elctldoc='no optics control description supplied'
      opticdoc='no optics control description supplied'
      CFCctldoc='no complex fen. control description supplied'

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If control focus is Complex Fenestration, read in *.cfc file
C to populate CFC commons which are referenced in setting up 
C complex fenestration controls.
      if(icfoc.eq.6)then
       CFCfilexists=.false.
       do 1001 icomp=1,ncomp
        if (icfc(icomp).eq.1)then
          iunit=IFIL+1
C Read construction file - common block T1 is needed for 
C subroutine read_in_cfc_file
          CALL ECONST(LTHRM(ICOMP),iunit,icomp,0,itru,IER)
          call read_in_cfc_file(0,itru,iunit,lcfcin(icomp),icomp,ier)
          CFCfilexists=.true.
        elseif(icomp.eq.ncomp.and.(.not.CFCfilexists))then
          call usrmsg('No CFC files were found. You must first define',
     &                'CFCs in zone composition.','W')
          return
        end if      
 1001  continue
      endif
      
C Attempt to read in the control file. If no current name then suggest
C one. If an existing name also provide option to deference it.
      helptopic='sys_ctl_file_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
 44   if(LCTLF(1:2).eq.'  '.or.LCTLF(1:4).eq.'UNKN')then
        if(browse)then
          call usrmsg('No control associated with this model,',
     &                'returning with no change.','W')
          return
        endif
        LN=max(1,LNBLNK(cfgroot))
        if(ctlpth(1:2).eq.'  '.or.ctlpth(1:2).eq.'./')then
          WRITE(LCTLF,'(2a)')cfgroot(1:ln),'.ctl'
        else
          WRITE(LCTLF,'(4a)') ctlpth(1:lnblnk(ctlpth)),fs,
     &         cfgroot(1:ln),'.ctl'
        endif

C Present current/default name and a browse option. If browse selected
C then set iclkok variable and jump to browse functions.
        LASCI=LCTLF
        CALL EASKS2CMD(LASCI,'Control file? (suggested name)',' ',
     &    'cancel','browse',iclkok,72,' ','Control file',IER,nbhelp)
        if(iclkok.eq.1)then
          return
        endif
      else
        LASCI=LCTLF
        if(browse)then
          ICTLF=IFIL+1
          CALL ERPFREE(ICTLF,ISTAT)
          call FINDFIL(LCTLF,XST)
          if(XST)then
C            call edisp(iuout,'Reading existing control file.')
            CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
            goto 54
          else
            call usrmsg('Control file not found. You are browsing',
     &        'so returning without change to the model.','W')
            return
          endif
        endif
        CALL EASKS2CMD(LASCI,'Control file? (see help)',
     &    ' ','dereference','browse',iclkok,72,' ','Control file',
     &    IER,nbhelp)
      endif
      call usrmsg(' ',' ','-')

C If user wishes to deselect current control reset file name to
C blank, call ezctli to clear commons and save the configuration file.
      if(iclkok.eq.1)then
        if(cfgok.and.(.NOT.browse))then
          call ezctli
          LCTLF='  '
          CALL EMKCFG('-',IER)
          call usrmsg(' ',' ','-')
        else
          call usrmsg('Cannot save control data while in browse',
     &                'mode, you must first `own` the model.','W')
          goto 92
        endif
        return
      elseif(iclkok.eq.2)then
        sfile=' '
        snpfile=' '
        call edisp(iuout,' ')
        call browsefilelist('?','ctl','fil',sfile,snpfile,nfile,iier)
        if(nfile.gt.0)then
          sfile=' '
          snpfile=' '
          call browsefilelist('b','ctl','fil',sfile,snpfile,nfile,
     &        iier)
          if(snpfile(1:2).ne.'  ')then
            write(LASCI,'(3a)')ctlpth(1:lnblnk(ctlpth)),fs,
     &        snpfile(1:lnblnk(snpfile))
          else

C If the user supplied a blank file then this is the same as
C de-referencing the file.
            call edisp(iuout,
     &        'A blank control file name returned. Clearing data.') 
            call ezctli
            LCTLF='  '
            CALL EMKCFG('-',IER)
            goto 44
          endif
        else

C If the user did not select a file then this is the same as
C de-referencing the file. 
          call edisp(iuout,'No control file selected. Clearing data.') 
          call ezctli
          LCTLF='  '
          CALL EMKCFG('-',IER)
          goto 44
        endif
      endif
      IF(LASCI(1:2).NE.'  ')LCTLF=LASCI
      ICTLF=IFIL+1
      CALL ERPFREE(ICTLF,ISTAT)
      call FINDFIL(LCTLF,XST)
      IF(XST)THEN
        CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
      else
        CALL EASKAB(' File not found.','Options:',
     &    'specify another','make new file',IW,0)
        if(IW.eq.1)goto 44

C Set defaults and ask user which control to focus on first.
        LINKED=.FALSE.
        call ADDCNTL(icfoc,'A')
      endif

C Setup for multi-page menu.
  54  MHEAD=8
      MCTL=6
      call totctlper(icfoc,ncper)
      ILEN=ncper
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup, rescan for length of the list.
   92 IER=0
      call totctlper(icfoc,ncper)
      ILEN=ncper
      IVERT=-3
      write(VERT(2),'(A,A27)')   'b description: ',ctldoc(1:27)
      if(icfoc.eq.0)then
        write(VERT(1),'(A)')     'a control focus >> zones'
        write(VERT(3),'(A,A27)') 'c description: ',znctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncf
        VERT(5)=                 'd link loops to zones'
      elseif(icfoc.eq.1)then
        write(VERT(1),'(A)')     'a control focus >> plant'
        write(VERT(3),'(A,A27)') 'c description: ',plctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncl
        VERT(5)=                 ' '
      elseif(icfoc.eq.2)then
        write(VERT(1),'(A)')     'a control focus >> vent/hydronic'
        write(VERT(3),'(A,A27)') 'c description: ',flctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ncc
        VERT(5)=                 ' '
      elseif(icfoc.eq.3)then
        write(VERT(1),'(A)')     'a control focus >> global'
        write(VERT(3),'(A,A27)') 'c description: ',glctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',ngf
        VERT(5)=                 '                                    '
      elseif(icfoc.eq.4)then
        write(VERT(1),'(A)')     'a control focus >> power'
        write(VERT(3),'(A,A27)') 'c description: ',elctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',necl
        VERT(5)=                 '                                    '
      elseif(icfoc.eq.5)then
        write(VERT(1),'(A)')     'a control focus >> optics'
        write(VERT(3),'(A,A27)') 'c description: ',opticdoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',nof
        VERT(5)=                 '                                    '
      elseif(icfoc.eq.6)then
        write(VERT(1),'(A)')     
     &  'a control focus >> complex fenestration'
        write(VERT(3),'(A,A27)') 'c description: ',CFCctldoc(1:27)
        write(VERT(4),'(A,I2)')  '  loops      : ',nCFCctlloops
        VERT(5)=                 '                                    '
      endif
      VERT(6)= ' ___________________________________________________'
      VERT(7)= ' cntl| sensor |actuator|    day     | valid  | periods'
      VERT(8)= ' loop|location|location|    type    | during | in day '
      m=MHEAD

C Keep track of how many items will be in the variable length section
C of the menu.
      listct=0
      if(icfoc.eq.0.and.ncf.gt.0)then
        do 88 II=1,ncf
          NN=nbcdt(ii)
          if(nbcdt(ii).eq.0)NN=nbdaytype  ! zero indicates all calendar day types
          do 87 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nbcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &            IBSN(II,1),IBSN(II,2),IBSN(II,3),IBAN(II,1),
     &            IBAN(II,2),IBAN(II,3),calentag(ij),ibcdv(II,IJ,1),
     &            ibcdv(II,IJ,2),nbcdp(II,1)
              elseif(nbcdt(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &          calentag(ij),ibcdv(II,IJ,1),ibcdv(II,IJ,2),nbcdp(II,IJ)
              endif
              if(nbcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &            IBSN(II,1),IBSN(II,2),IBSN(II,3),IBAN(II,1),
     &            IBAN(II,2),IBAN(II,3),'all daytypes',ibcdv(II,IJ,1),
     &            ibcdv(II,IJ,2),nbcdp(II,IJ)
              elseif(nbcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,'all daytypes',
     &          ibcdv(II,IJ,1),ibcdv(II,IJ,2),nbcdp(II,IJ)
              endif
            endif
  87      continue
  88    continue
      elseif(icfoc.eq.1.and.ncl.gt.0)then
        do 188 II=1,ncl
          NN=NPCDT(II)
          if(NPCDT(II).eq.0)NN=nbdaytype
          do 187 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(npcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &            IPSN(II,1),IPSN(II,2),IPSN(II,3),IPAN(II,1),
     &            IPAN(II,2),IPAN(II,3),calentag(ij),ipcdv(II,IJ,1),
     &            ipcdv(II,IJ,2),npcdp(II,1)
              elseif(npcdt(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &          calentag(ij),ipcdv(II,IJ,1),ipcdv(II,IJ,2),npcdp(II,IJ)
              endif
              if(npcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &            IPSN(II,1),IPSN(II,2),IPSN(II,3),IPAN(II,1),
     &            IPAN(II,2),IPAN(II,3),'all daytypes',ipcdv(II,IJ,1),
     &            ipcdv(II,IJ,2),npcdp(II,IJ)
              elseif(npcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,'all daytypes',
     &          ipcdv(II,IJ,1),ipcdv(II,IJ,2),npcdp(II,IJ)
              endif
            endif
 187      continue
 188    continue
      elseif(icfoc.eq.2.and.ncc.gt.0)then
        do 288 II=1,ncc
          NN=NFCDT(II)
          if(NFCDT(II).eq.0) NN=nbdaytype
          do 287 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II  ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nfcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &            IFSN(II,1),IFSN(II,2),IFSN(II,3),IFAN(II,1),
     &            IFAN(II,2),IFAN(II,3),calentag(ij),ifcdv(II,IJ,1),
     &            ifcdv(II,IJ,2),nfcdp(II,1)
              elseif(nfcdt(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &          calentag(ij),ifcdv(II,IJ,1),ifcdv(II,IJ,2),nfcdp(II,IJ)
              endif
              if(nfcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &            IFSN(II,1),IFSN(II,2),IFSN(II,3),IFAN(II,1),
     &            IFAN(II,2),IFAN(II,3),'all daytypes',ifcdv(II,IJ,1),
     &            ifcdv(II,IJ,2),nfcdp(II,IJ)
              elseif(nfcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,'all daytypes',
     &          ifcdv(II,IJ,1),ifcdv(II,IJ,2),nfcdp(II,IJ)
              endif
            endif
 287      continue
 288    continue
      elseif(icfoc.eq.3.and.ngf.gt.0)then
        do 388 II=1,ngf
          NN=NGCDT(II)
          if(NGCDT(II).eq.0)NN=nbdaytype
          do 387 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II  ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(ngcdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &            IGSN(II,1),IGSN(II,2),IGSN(II,3),IGAN(II,1),
     &            IGAN(II,2),IGAN(II,3),calentag(ij),igcdv(II,IJ,1),
     &            igcdv(II,IJ,2),ngcdp(II,1)
              elseif(ngcdt(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &          calentag(ij),igcdv(II,IJ,1),igcdv(II,IJ,2),ngcdp(II,IJ)
              endif
              if(ngcdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &            IGSN(II,1),IGSN(II,2),IGSN(II,3),IGAN(II,1),
     &            IGAN(II,2),IGAN(II,3),'all daytypes',igcdv(II,IJ,1),
     &            igcdv(II,IJ,2),ngcdp(II,IJ)
              elseif(ngcdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,'all daytypes',
     &          igcdv(II,IJ,1),igcdv(II,IJ,2),ngcdp(II,IJ)
              endif
            endif
 387      continue
 388    continue
      elseif(icfoc.eq.4.and.necl.gt.0)then

C Power controls.
      elseif(icfoc.eq.5.and.nof.gt.0)then

C Optical controls.
        do 488 II=1,nof
          NN=NOCDT(II)
          if(NOCDT(II).eq.0)NN=nbdaytype
          do 487 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II  ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nocdt(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &            IOSN(II,1),IOSN(II,2),IOSN(II,3),IOAN(II,1),
     &            IOAN(II,2),IOAN(II,3),calentag(ij),iocdv(II,IJ,1),
     &            iocdv(II,IJ,2),nocdp(II,1)
              elseif(nocdt(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &          calentag(ij),iocdv(II,IJ,1),iocdv(II,IJ,2),nocdp(II,IJ)
              endif
              if(nocdt(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &            IOSN(II,1),IOSN(II,2),IOSN(II,3),IOAN(II,1),
     &            IOAN(II,2),IOAN(II,3),'all daytypes',iocdv(II,IJ,1),
     &            iocdv(II,IJ,2),nocdp(II,IJ)
              elseif(nocdt(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,'all daytypes',
     &          iocdv(II,IJ,1),iocdv(II,IJ,2),nocdp(II,IJ)
              endif
            endif
 487      continue
 488    continue
      elseif(icfoc.eq.6.and.nCFCctlloops.gt.0)then

C Complex fenestration controls.
        do 489 II=1,nCFCctlloops
          NN=nCFCctldaytypes(ii)
          if(nCFCctldaytypes(ii).eq.0)NN=nbdaytype
          do 490 IJ=1,NN
            listct=listct+1
            if(listct.GE.IST.AND.(listct.LE.(IST+MIFULL)))then
              m=m+1
              mptback(m)=II  ! set pointer from position in display list to the control loop
              CALL EMKEY(M-MHEAD+4,KEY,IER)
              if(nCFCctldaytypes(ii).eq.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)') KEY,II,
     &          iCFCsensor(II,1),iCFCsensor(II,2),iCFCsensor(II,3),
     &          iCFCactuator(II,1),iCFCactuator(II,2),
     &          iCFCactuator(II,3),calentag(ij),
     &          iCFCctldatevalid(II,IJ,1),
     &          iCFCctldatevalid(II,IJ,2),nCFCdayctlperiods(II,1)
              elseif(nCFCctldaytypes(ii).eq.0.and.IJ.gt.1)then
                iperptbk(m)=ij
                WRITE(VERT(M),'(A1,23X,a12,3I4)') KEY,
     &            calentag(ij),iCFCctldatevalid(II,IJ,1),
     &            iCFCctldatevalid(II,IJ,2),nCFCdayctlperiods(II,IJ)
              endif
              if(nCFCctldaytypes(ii).ne.0.and.IJ.eq.1)then
                iperptbk(m)=1
                WRITE(VERT(M),'(A1,I4,3I3,3I3,1x,a12,3I4)')KEY,II,
     &          iCFCsensor(II,1),iCFCsensor(II,2),iCFCsensor(II,3),
     &          iCFCactuator(II,1),iCFCactuator(II,2),
     &          iCFCactuator(II,3),'all daytypes',
     &          iCFCctldatevalid(II,IJ,1),
     &          iCFCctldatevalid(II,IJ,2),nCFCdayctlperiods(II,1)
              elseif(nCFCctldaytypes(ii).ne.0.and.IJ.gt.1)then
                iperptbk(m)=IJ
                WRITE(VERT(M),'(A1,23X,a12,3I4)')KEY,
     &            'all daytypes',iCFCctldatevalid(II,IJ,1),
     &            iCFCctldatevalid(II,IJ,2),nCFCdayctlperiods(II,IJ)
              endif
            endif
 490      continue
 489    continue
      endif

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list, include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page ------- Part: ',I2,' of ',I2,
     &    ' -----------')
      ENDIF
      VERT(M+2)  ='+ add/delete/copy control loop            '
      VERT(M+3)  ='! list or check current control data      '
      VERT(M+4)  ='> save control data                       '
      VERT(M+5)  ='? help                                    '
      VERT(M+6)  ='- exit this menu                          '

C Help text for this menu.
      helptopic='sys_ctl_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Controls',VERT,MVERT,IVERT)

      IF(IVERT.EQ.1)THEN

C Toggle focus
        icfoc=icfoc+1
        if(icfoc.gt.6)icfoc=0
      ELSEIF(IVERT.EQ.2)THEN

C Edit project level description.
        t248=ctldoc
        CALL EASKS248(t248,' ','Control system description?',
     &    72,'Base case control','overall control desc',IER,nbhelp)
        if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')ctldoc=t248
        IF(ctldoc(1:5).EQ.'NONE ')GOTO 92
      ELSEIF(IVERT.EQ.3)THEN

C Edit building/plant/flow/global/complex fen. control description.
        if(icfoc.eq.0)then
          t248=znctldoc
          CALL EASKS248(t248,' ','Zone control description?',
     &      72,'Zone control','building control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')znctldoc=t248
        elseif(icfoc.eq.1)then
          t248=plctldoc
          CALL EASKS248(t248,' ','Plant control description?',
     &      72,'Plant control','plant control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')plctldoc=t248
        elseif(icfoc.eq.2)then
          t248=flctldoc
          CALL EASKS248(t248,' ','Flow control description?',
     &      72,'Flow control','flow control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')flctldoc=t248
        elseif(icfoc.eq.3)then
          t248=glctldoc
          CALL EASKS248(t248,' ','Global control description?',
     &      72,'Global control','global control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')glctldoc=t248
        elseif(icfoc.eq.4)then
          t248=elctldoc
          CALL EASKS248(t248,' ','Electrical control description?',
     &      72,'Electrical control','electrical control descr.',
     &      IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')elctldoc=t248
        elseif(icfoc.eq.5)then
          t248=opticdoc
          CALL EASKS248(t248,' ',' Optical control description?',
     &      72,'Optical control','optic control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')opticdoc=t248
        elseif(icfoc.eq.6)then
          t248=CFCctldoc
          CALL EASKS248(t248,' ',' Complex fen. control description?',
     &      72,'Complex fen. control',
     &      'complex fen. control descr.',IER,nbhelp)
          if(t248(1:2).ne.'  '.and.t248(1:4).ne.'UNKN')CFCctldoc=t248
        endif
      ELSEIF(IVERT.EQ.5)THEN

c Relate control functions to the appropriate zones
c from the configuration file.
        if(icfoc.eq.0)then
          NZ=NCOMP
          IF(NZ.GT.1.OR.NCF.GT.1)GOTO 304

c Match single zone with single control function.
          ICASCF(1)=1
          LINKED=.TRUE.
          GOTO 92

c Relate control function to associated zone.
c Remember zero implies no control.
  304     IF(NZ.EQ.1)then
            ica=ICASCF(1)
            CALL EASKI(ICA,'Associated control function',
     &               'for zone 1?',0,'F',MCOM,'F',0,
     &               'associated control',IERI,nbhelp)
            if(ieri.eq.-3) goto 92
            ICASCF(1)=ica
          else
            do 311 IJ=1,NZ
              write(outs,'(3A,I2,A)')'for ',zname(IJ),'(',IJ,')?'
              ica=ICASCF(IJ)
              CALL EASKI(ICA,'Associated control function',outs,
     &                 0,'F',MCOM,'F',0,
     &                 'associated control',IER,nbhelp)
              if(ieri.eq.-3)then
                goto 311
              else
                ICASCF(IJ)=ica
              endif
  311       continue
          endif

c Check for out-of-allowable range.
          IF(NZ.gt.0)then
           DO 300 I=1,NZ
            IF(ICASCF(I).LT.0.OR.ICASCF(I).GT.NCF)then
             write(outs,'(3A,I2,A)')' Zone ',zname(IJ),'(',I,
     &         ') refers to a non-existent'
             call usrmsg(outs,'control function. Respecify.','W')
             goto 304
            endif
  300      CONTINUE
          endif

C Flag that links made.
          LINKED=.TRUE.

        elseif(icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.3.or.
     &    icfoc.eq.4.or.icfoc.eq.5.or.icfoc.eq.6)then
          continue
        endif

      ELSEIF(IVERT.EQ.MVERT)THEN

C Local variable ireturn is non-zero to signal user wants to exit.
        IRETURN=0
        if(icfoc.eq.0)then
          if(.NOT.LINKED)then
            dok=.false.
            CALL ASKOK('Control functions have not yet been',
     &        'associated with zones. Exit anyway?',OK,dok,nbhelp)
            IF(OK)then
              IRETURN=1
            else
              goto 92
            endif
          else
            IRETURN=1
          endif
        elseif(icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.3.or.
     &         icfoc.eq.4.or.icfoc.eq.5)then
          IRETURN=1
        elseif(icfoc.eq.6)then
          call check_CFC_control_validity(CFCcontrolOK)
          if(.not.CFCcontrolOK)then
            IVERT=-1
            goto 92
          endif
        endif

C If not browsing the model ask user if they want to save any changes
C to control file.
        if(cfgok.and.(.NOT.browse))then
          IRETURN=1
          DOK=.TRUE.
          CALL ASKOK(' ','Save changes to control file?',OK,dok,nbhelp)
          IF(OK)THEN
            ICTLF=IFIL+1
            CALL CTLWRT(ICTLF,IER)
C            call edisp(IUOUT,'Saving system configuration file.')
            CALL EMKCFG('-',IER)
          ENDIF
        endif
        IF(IRETURN.EQ.1)RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Explanation
        CALL PHELPD('control help',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Save the control file.
        LASCI=LCTLF
        CALL EASKS(LASCI,'Control file?',' ',72,' ','Control file',
     &            IER,nbhelp)
        IF(LASCI(1:2).NE.'  ')LCTLF=LASCI

C Save current information into a descriptive file
C and save the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
          ICTLF=IFIL+1
          CALL CTLWRT(ICTLF,IER)
C          call edisp(IUOUT,'Saving system configuration file.')
          CALL EMKCFG('-',IER)
        else
          call usrmsg('Cannot save control data while in browse',
     &                'mode. You must first `own` the model.','W')
          goto 92
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        CALL EASKABC(' ','Options:',
     &    'list control loops','edit control loops','cancel',
     &    IRC,nbhelp)
        if(irc.eq.1)then

C List control data.
          call edisp(itru,' ')
          call edisp(itru,' Overall description:')
          call edisp248(itru,ctldoc,72)

          if(icfoc.eq.0.and.NCF.gt.0)then
            WRITE(outs,'(a,I2,a)')' Zones control includes ',
     &        NCF,' functions.'
            call edisp(itru,outs)
            call edisp248(itru,znctldoc,72)
            do 144 ii=1,NCF
              call LSTCNTL(itru,0,ii)
 144        continue
            call LSTCNTL(itru,0,0)
          endif
          if(icfoc.eq.1.and.NCL.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Plant control includes ',
     &        NCL,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,plctldoc,72)
            do 145 ii=1,NCL
              call LSTCNTL(itru,1,ii)
 145        continue
          endif
          if(icfoc.eq.2.and.NCC.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Flow control includes ',
     &        NCC,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,flctldoc,72)
            do 146 ii=1,NCC
              call LSTCNTL(itru,2,ii)
 146        continue
          endif
          if(icfoc.eq.3.and.NGF.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Global control includes ',
     &        NGF,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,flctldoc,72)
            do 147 ii=1,NGF
              call LSTCNTL(itru,3,ii)
 147        continue
          endif
          if(icfoc.eq.4.and.NECL.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Electrical control includes ',
     &        NECL,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,elctldoc,72)
            do 247 ii=1,NECL
              call LSTCNTL(itru,4,ii)
 247        continue
          endif
          if(icfoc.eq.5.and.nof.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')' Optical control includes ',
     &        nof,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,opticdoc,72)
            do 248 ii=1,nof
              call LSTCNTL(itru,5,ii)
 248        continue
          endif
          if(icfoc.eq.6.and.nCFCctlloops.gt.0)then
            call edisp(itru,' ')
            WRITE(outs,'(a,I2,a)')
     &      ' Complex fenestration control includes ',
     &      nCFCctlloops,' loops.'
            call edisp(itru,outs)
            call edisp248(itru,CFCctldoc,72)
            do 249 ii=1,nCFCctlloops
              call LSTCNTL(itru,6,ii)
 249        continue
          endif
        elseif(irc.eq.2)then
          call ezctlc(iier)
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Add or delete a zone control function.
C Warn user to update any connections to plant loops.
        IW=1
        CALL EASKATOG(' ','Options:','delete control loop',
     &     'add control loop','copy control loop',
     &     'cancel',' ',' ',' ',IW,nbhelp)
        if(IW.eq.1)then
          call ADDCNTL(icfoc,'D')
        elseif(IW.eq.2)then
          call ADDCNTL(icfoc,'A')
        elseif(IW.eq.3)then
          call ADDCNTL(icfoc,'C')
        elseif(IW.eq.4)then
          goto 92
        endif
        call edisp(iuout,
     &'You may need to update links to zones, plant or flow networks')
        call edisp(iuout,
     &'or any other existing control networks.')
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items, allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MHEAD+M+1))THEN

C Edit control function.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        ii=mptback(IVERT)    ! get back control loop from the position in display list
        idi=iperptbk(IVERT)  ! get back day type from the position in display list

C List out the current details of the control loop.
  64    CALL EVCNTRL(icfoc,II,1,1,'S',SSTR)
        call edisp(itru,' ')
        LN=max(1,LNBLNK(SSTR))
        WRITE(outs,65)II,SSTR(1:LN)
  65    FORMAT(' The sensor for function ',I2,' ',a)
        call edisp(itru,outs)

        CALL EVCNTRL(icfoc,II,1,1,'A',SSTR)
        LN=max(1,LNBLNK(SSTR))
        WRITE(outs,75)II,SSTR(1:LN)
  75    FORMAT(' The actuator for function ',I2,' is ',a)
        call edisp(itru,outs)

C Pull up a multi-choice dialog to see what should be edited.
        CALL EPMENSV
        if(icfoc.eq.0)then
          call LSTCNTLD(itru,0,ii,idi)
          write(outs,'(a,i2,a)') 'Zone control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.1)then
          call LSTCNTLD(itru,1,ii,idi)
          write(outs,'(a,i2,a)') 'Plant control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.2)then
          call LSTCNTLD(itru,2,ii,idi)
          write(outs,'(a,i2,a)') 'Flow control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.3)then
          call LSTCNTLD(itru,3,ii,idi)
          write(outs,'(a,i2,a)') 'Global control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.4)then
          call LSTCNTLD(itru,4,ii,idi)
          write(outs,'(a,i2,a)') 'Power control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.5)then
          call LSTCNTLD(itru,5,ii,idi)
          write(outs,'(a,i2,a)') 'Optical control function (',II,
     &      ') editing.'
        elseif(icfoc.eq.6)then
          call LSTCNTLD(itru,6,ii,idi)
          write(outs,'(a,i2,a)') 'Complex fen. control function (',II,
     &      ') editing.'
        endif
        idno=0
        isw=0
        if(icfoc.eq.0)then
          NN=NBCDT(II)
        elseif(icfoc.eq.1)then
          NN=NPCDT(II)
        elseif(icfoc.eq.2)then
          NN=NFCDT(II)
        elseif(icfoc.eq.3)then
          NN=NGCDT(II)
        elseif(icfoc.eq.4)then
          NN=NECDT(II)
        elseif(icfoc.eq.5)then
          NN=NOCDT(II)
        elseif(icfoc.eq.6)then
          NN=nCFCctldaytypes(II)
        endif

C The only time the sensor and actuator choices
C should be shown and accessed is at the initial day type of
C the list. If we pick one of the subsequent days then
C just jump into the period data (to save a step).

C If we are following the calendar day types then ignore the period
C of validity in the selections.
        if(idi.eq.1)then
          IF(NN.EQ.0.OR.NN.EQ.1)THEN
            call MENUATOL(outs,'Editing Options',
     &      'a sensor details','b actuator details',
     &      'c period data',' ',' ',' ',
     &      ' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)
            IF(ISW.EQ.3)ISW=4
          ELSE
            call MENUATOL(outs,'Editing Options',
     &     'a sensor details','b actuator details',
     &     'c period of validity','d period data',' ',' ',
     &     ' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)
          ENDIF
        else
          ISW=4  ! jump directly to the period data
        endif
        if(ISW.eq.0)then
          goto 92
        elseif(ISW.eq.1)then

C Do not edit sensor information if Nicol control algorithm
          IF(IFSN(II,2).EQ.7)THEN
            CALL USRMSG(
     &      'Sensor not required for behavioural control model',
     &      ' ','W')
            NCA=1
          ENDIF
          IF(NCA.EQ.1)GOTO 64

C Edit sensor location. 
          CALL EPMENSV
          if(icfoc.eq.0)then
            isn1=IBSN(II,1)
            isn2=IBSN(II,2)
            isn3=IBSN(II,3)
            isn4=IBSN(II,4)
            call senloc('Options:','Zone sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'zone sensor',IER)
            IBSN(II,1)=isn1
            IBSN(II,2)=isn2
            IBSN(II,3)=isn3
            IBSN(II,4)=isn4
          elseif(icfoc.eq.1)then
            isn1=IPSN(II,1)
            isn2=IPSN(II,2)
            isn3=IPSN(II,3)
            isn4=IPSN(II,4)
            isn5=IPSN(II,5)
            call senloc('Options:','Plant sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'plant sensor',IER)
            IPSN(II,1)=isn1
            IPSN(II,2)=isn2
            IPSN(II,3)=isn3
            IPSN(II,4)=isn4
            IPSN(II,5)=isn5
          elseif(icfoc.eq.2)then
            isn1=IFSN(II,1)
            isn2=IFSN(II,2)
            isn3=IFSN(II,3)
            isn4=IFSN(II,4)
            call senloc('Options:','Flow sensor',icfoc,II,
     &        isn1,isn2,isn3,isn4,isn5,isense,nsupl,'flow sensor',IER)
            IFSN(II,1)=isn1
            IFSN(II,2)=isn2
            IFSN(II,3)=isn3
            IFSN(II,4)=isn4

C Note isense is sensed flow condition used tp determine the flow controller type.    
            IFCTYP(II,1,1)=isense
            nfsup(ii)=nsupl

C Because changes in flow sensor usually require changes to actuation
C call the flow actuation interactions now.
            ian1=IFAN(II,1)
            ian2=IFAN(II,2)
            ian3=IFAN(II,3)
            call actloc('Options:','Flow actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'flow actuator',IER)
            IFAN(II,1)=ian1
            IFAN(II,2)=ian2
            IFAN(II,3)=ian3
          elseif(icfoc.eq.3)then
            ign1=IGSN(II,1)
            ign2=IGSN(II,2)
            ign3=IGSN(II,3)
            ign4=IGSN(II,4)
            call senloc('Options:','Global sensor',icfoc,II,
     &        ign1,ign2,ign3,ign4,ign5,isense,nsupl,'global sensor',IER)
            IGSN(II,1)=ign1
            IGSN(II,2)=ign2
            IGSN(II,3)=ign3
            IGSN(II,4)=ign4
          elseif(icfoc.eq.4)then
            ien1=IESN(II,1)
            ien2=IESN(II,2)
            ien3=IESN(II,3)
            call senloc('Options:','Electrical sensor',icfoc,II,
     &        ien1,ien2,ien3,ien4,ien5,isense,nsupl,'power sensor',IER)
            IESN(II,1)=ien1
            IESN(II,2)=ien2
            IESN(II,3)=ien3
          elseif(icfoc.eq.5)then
            isn1=IOSN(II,1)
            isn2=IOSN(II,2)
            isn3=IOSN(II,3)
            isn4=IOSN(II,4)
            call senloc('Options:','Optical sensor',icfoc,II,
     &       isn1,isn2,isn3,isn4,isn5,isense,nsupl,'optical sensor',IER)
            IOSN(II,1)=isn1
            IOSN(II,2)=isn2
            IOSN(II,3)=isn3
            IOSN(II,4)=isn4
          elseif(icfoc.eq.6)then
            isn1=iCFCsensor(II,1)
            isn2=iCFCsensor(II,2)
            isn3=iCFCsensor(II,3)
            isn4=iCFCsensor(II,4)
            call senloc('Options:','Complex fen. sensor',icfoc,II,
     &      isn1,isn2,isn3,isn4,isn5,isense,nsupl,
     &      'complex fen. sensor',IER)
            iCFCsensor(II,1)=isn1
            iCFCsensor(II,2)=isn2
            iCFCsensor(II,3)=isn3
            iCFCsensor(II,4)=isn4
          endif
          CALL EPMENRC
          if (ICFOC.ne.2) then
            call usrmsg('After editing the sensor location it may',
     &        'be necessary to update the actuator location.','W')
          endif
          goto 64
        elseif(ISW.eq.2)then

C Edit actuator location.
C If sensor location -99 ask for up to three surfaces where
C substitutions will occur.
          CALL EPMENSV
          if(icfoc.eq.0)then 
            isn1=IBSN(II,1)
            ian1=IBAN(II,1)
            ian2=IBAN(II,2)
            ian3=IBAN(II,3)
            call actloc('Options:','Zone actuator',icfoc,II,
     &        isn1,ian1,ian2,ian3,'zone actuator',IER)
            IBAN(II,1)=ian1
            IBAN(II,2)=ian2
            IBAN(II,3)=ian3
          elseif(icfoc.eq.1)then
            ian1=IPAN(II,1)
            ian2=IPAN(II,2)
            ian3=IPAN(II,3)
            call actloc('Options:','Plant actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'plant actuator',IER)
            IPAN(II,1)=ian1
            IPAN(II,2)=ian2
            IPAN(II,3)=ian3
          elseif(icfoc.eq.2)then
            ian1=IFAN(II,1)
            ian2=IFAN(II,2)
            ian3=IFAN(II,3)
            call actloc('Options:','Flow actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'flow actuator',IER)
            IFAN(II,1)=ian1
            IFAN(II,2)=ian2
            IFAN(II,3)=ian3
          elseif(icfoc.eq.3)then
            ign1=IGAN(II,1)
            ign2=IGAN(II,2)
            ign3=IGAN(II,3)
            call actloc('Options:','Global actuator',icfoc,II,
     &        0,ign1,ign2,ign3,'global actuator',IER)
            IGAN(II,1)=ign1
            IGAN(II,2)=ign2
            IGAN(II,3)=ign3
          elseif(icfoc.eq.4)then
            ien1=IEAN(II,1)
            ien2=IEAN(II,2)
            ien3=IEAN(II,3)
            call actloc('Options:','Power actuator',icfoc,II,
     &        0,ien1,ien2,ien3,'power actuator',IER)
            IEAN(II,1)=ien1
            IEAN(II,2)=ien2
            IEAN(II,3)=ien3
          elseif(icfoc.eq.5)then
            ian1=IOAN(II,1)
            ian2=IOAN(II,2)
            ian3=IOAN(II,3)
            call actloc('Options:','Optical actuator',icfoc,II,
     &        0,ian1,ian2,ian3,'optical actuator',IER)
            IOAN(II,1)=ian1
            IOAN(II,2)=ian2
            IOAN(II,3)=ian3
          elseif(icfoc.eq.6)then 
            isn1=iCFCsensor(II,1)
            ian1=iCFCactuator(II,1)
            ian2=iCFCactuator(II,2)
            ian3=iCFCactuator(II,3)
            call actloc('Options:','Complex fen. actuator',icfoc,II,
     &        isn1,ian1,ian2,ian3,'complex fen. actuator',IER)
            iCFCactuator(II,1)=ian1
            iCFCactuator(II,2)=ian2
            iCFCactuator(II,3)=ian3
          endif
          CALL EPMENRC
          call usrmsg(' ',' ','-')
          goto 64
        elseif(ISW.eq.3)then

C Edit date of validity (day type index is idi).
C Only applicable if Periods of validity are active.
   48     if(icfoc.eq.0)then 
            IBDOY=IBCDV(II,idi,1)
            IEDOY=IBCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IBCDV(II,idi,1)=IBDOY
            IBCDV(II,idi,2)=IEDOY
            IF(IBCDV(II,idi,2).LT.IBCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.1)then 
            IBDOY=IPCDV(II,idi,1)
            IEDOY=IPCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IPCDV(II,idi,1)=IBDOY
            IPCDV(II,idi,2)=IEDOY
            IF(IPCDV(II,idi,2).LT.IPCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.2)then 
            IBDOY=IFCDV(II,idi,1)
            IEDOY=IFCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IFCDV(II,idi,1)=IBDOY
            IFCDV(II,idi,2)=IEDOY
            IF(IFCDV(II,idi,2).LT.IFCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.3)then
            IBDOY=IGCDV(II,idi,1)
            IEDOY=IGCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IGCDV(II,idi,1)=IBDOY
            IGCDV(II,idi,2)=IEDOY
            IF(IGCDV(II,idi,2).LT.IGCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.4)then
            IBDOY=IECDV(II,idi,1)
            IEDOY=IECDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IECDV(II,idi,1)=IBDOY
            IECDV(II,idi,2)=IEDOY
            IF(IECDV(II,idi,2).LT.IECDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.5)then
            IBDOY=IOCDV(II,idi,1)
            IEDOY=IOCDV(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            IOCDV(II,idi,1)=IBDOY
            IOCDV(II,idi,2)=IEDOY
            IF(IOCDV(II,idi,2).LT.IOCDV(II,idi,1))GOTO 48
          elseif(icfoc.eq.6)then
            IBDOY=iCFCctldatevalid(II,idi,1)
            IEDOY=iCFCctldatevalid(II,idi,2)
            CALL EASKPER('Dates of validity:',IBDOY,IEDOY,IFDAY,IER)
            iCFCctldatevalid(II,idi,1)=IBDOY
            iCFCctldatevalid(II,idi,2)=IEDOY
            IF(iCFCctldatevalid(II,idi,2)
     &         .LT.iCFCctldatevalid(II,idi,1))GOTO 48
          endif 
          goto 64
        elseif(ISW.eq.4)then

C Edit period data (day type index is idi), first list the
C current day type data..
          CALL EPMENSV
          if(icfoc.eq.0)then
            call LSTCNTLD(itru,0,II,idi)
            CALL EBCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.1)then
            call LSTCNTLD(itru,1,II,idi)
            CALL EPCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.2)then
            call LSTCNTLD(itru,2,II,idi)
            CALL EFCNTRL(II,idi,IER)
          elseif(icfoc.eq.3)then
            call LSTCNTLD(itru,3,II,idi)
            CALL EGCNTRL(ITRC,II,idi,IER)
          elseif(icfoc.eq.4)then
            call LSTCNTLD(itru,4,II,idi)
            CALL edisp(iuout,'Electrical editing not available')
          elseif(icfoc.eq.5)then
            call LSTCNTLD(itru,5,II,idi)
            CALL EOCNTRL(II,idi,IER)
          elseif(icfoc.eq.6)then
            call LSTCNTLD(itru,6,II,idi)
            CALL edit_CFC_control(ITRC,II,idi,IER)
          endif
          CALL EPMENRC
          if(idi.eq.1)then
            goto 64   ! jump back to sensor/actuator/period choice
          else
            goto 92   ! jump to main menu
          endif
        elseif(ISW.eq.0)then
          GOTO 92
        endif
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

      END

C ***** totctlper
C Finds the current number of periods in all control loops
C of type icfoc.
      subroutine totctlper(icfoc,ncper)
#include "building.h"
#include "net_flow.h"
#include "control.h"
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER nbdaytype,nbcaldays,icalender

      if(icfoc.eq.0)then
        ncper=0
        if(ncf.gt.0)then
          do 888 II=1,ncf
            NN=nbcdt(ii)
            if(nbcdt(ii).eq.0)NN=nbdaytype
            do 887 IJ=1,NN
              ncper=ncper+1
  887       continue
  888     continue
        endif
      elseif(icfoc.eq.1)then
        ncper=0
        if(ncl.gt.0)then
          do 886 II=1,ncl
            NN=npcdt(ii)
            if(npcdt(ii).eq.0)NN=nbdaytype
            do 885 IJ=1,NN
              ncper=ncper+1
  885       continue
  886     continue
        endif
      elseif(icfoc.eq.2)then
        ncper=0
        if(ncc.gt.0)then
          do 884 II=1,ncc
            NN=nfcdt(ii)
            if(nfcdt(ii).eq.0) NN=nbdaytype
            do 883 IJ=1,NN
              ncper=ncper+1
  883       continue
  884     continue
        endif
      elseif(icfoc.eq.3)then
        ncper=0
        if(ngf.gt.0)then
          do 882 II=1,ngf
            NN=ngcdt(ii)
            if(ngcdt(ii).eq.0)NN=nbdaytype
            do 881 IJ=1,NN
              ncper=ncper+1
  881       continue
  882     continue
        endif
      elseif(icfoc.eq.4)then
        ncper=0
        if(necl.gt.0)then
          do 782 II=1,necl
            NN=necdt(ii)
            if(necdt(ii).eq.0)NN=nbdaytype
            do 781 IJ=1,NN
              ncper=ncper+1
  781       continue
  782     continue
        endif
      elseif(icfoc.eq.5)then
        ncper=0
        if(nof.gt.0)then
          do 784 II=1,nof
            NN=nocdt(ii)
            if(nocdt(ii).eq.0)NN=nbdaytype
            do 783 IJ=1,NN
              ncper=ncper+1
  783       continue
  784     continue
        endif
      endif
      return
      end

C *********** EBCNTRL
C Edit building control function II.
      SUBROUTINE EBCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "plant.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/sctl/tcps,ictyp,iclaw,cm(misc)
      
C Plant common required for building/plant flux linkage      
      common/pcdat/nnodes(mpcom),isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

      DIMENSION SALT(24),IVALS(24)
      CHARACTER*82 VERT(20)

      CHARACTER outs*124,SSTR*96,TMP*96,SALT*40,hold*40,KEY*1
      character outsa*84,outsb*84,outsc*84,ltmp*248,SSTRB*48,daytype*28
      logical close1,close2,useold

      integer iclawt   ! control law index passed back from editctlperiod
      dimension icsn(5)! for passing sensor location to
      integer icsn,nn
      integer lastndx  ! to keep track of last data position used.
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EBCNTRL'  ! set for subroutine

C Number of building functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN
      NN=NBCDT(II)

C Building control day types.
      J=IV
      JJ=J
      IF(NN.EQ.0)THEN ! All day types have individual controls
        outs=calentag(jj)
        write(daytype,'(3a)')' (active on ',outs(1:lnblnk(outs)),')'
      ELSEIF(NN.EQ.1)THEN ! All day types have same controls
        daytype=' (active on all daytypes)'
      ELSE ! Day types are not used but periods of validity are active
        write(daytype,'(a,i3,a,i3,a)')'(valid from day ',ibcdv(ii,jj,1),
     &   ' to ',ibcdv(ii,jj,2),')'
      ENDIF

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,1x,a)')
     &' control loop',II,daytype
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NBCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per|start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.|time |property|property |                 |     '
      LL=NBCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(0,II,JJ,KK)
        CALL EVCNTRL(0,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(0,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F6.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.8)then
            write(outsb,'(8F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(8F9.1,a)')(cm(L),L=2,9),'..'
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:46),outsc(1:32)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      helptopic='sys_bctl_periods'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        helptopic='sys_bctl_periods'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        CALL EASKABC(' ','Options:',
     &    'add period','delete period','cancel',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(0,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(0,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(0,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.10)then
            write(outs,'(10F12.4)')(cm(L),L=2,NITEMS)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(10F12.4)')(cm(L),L=2,10)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.20)then
              write(outs,'(10F12.4)')(cm(L),L=11,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(10F12.4)')(cm(L),L=11,20)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
              write(outs,'(10F12.4)')(cm(L),L=21,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif
        call edisp(iuout,' ')

C If thermophysical property substitution set type and law and jump
C to periods.
        if(IBSN(II,1).eq.-99)then
          ictyp=0
          iclaw=1
          goto 62
        endif

C If menu editing version available use it otherwise use the
C sequential question interface. If return from editctlperiod
C having selected a control law it does not deal with then
C this will be signaled by an ier condition equal to the control
C law that has been selected so further user selection of control
C law can be jumped around.
        useold=.false.
        iclawt=-1
  77    if(useold)then
          if(iclawt.ge.0.and.iclawt.le.23)then
            iclaw=iclawt
            IBCLAW(II,JJ,KK)=iclaw
            useold=.false.
            goto 62
          endif
        endif
        if((iclaw.ge.1.and.iclaw.lt.6).or.
     &      iclaw.eq.7.or.iclaw.eq.8.or.iclaw.eq.10.or.
     &      iclaw.eq.13.or.iclaw.eq.14.or.iclaw.eq.15.or.
     &      iclaw.eq.21.or.iclaw.eq.22.or.iclaw.eq.23.or.
     &      iclaw.eq.24)then

C Remember the previous start (prevstart) and the total number of
C period in this control loop (nper) and the number of day types
C included in the control loop defintion (loopdaytype) before
C starting up the menu based control period definition.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        endif

        ictyp=0
        SALT(1)= 'Basic control for heating/cooling      '
        SALT(2)= 'Free-float control                     '
        SALT(3)= 'Basic pre-heat or pre-cool control     '
        SALT(4)= 'Fixed heat injection and extraction    '
        SALT(5)= 'PID control action for heating/cooling '
        SALT(6)= 'Flux connection between zone & plant   '
        SALT(7)= 'Multi-stage control with hysteresis    '
        SALT(8)= 'Variable supply temp. with constraints '
        SALT(9)= 'Heat pipe                              '
        SALT(10)='Separate flux ON & OFF set points      '
        SALT(11)='Match sensed & recorded values (ideal) '
        SALT(12)='Match sensed & recorded values (on/off)'
        SALT(13)='Time-proportioning (on/off)            '
        SALT(14)='Floating three-position control        '
        SALT(15)='Optimum start control (with rewind)    '
        SALT(16)='Optimum stop control                   '
        SALT(17)='Fuzzy Logic PI or PD control           '
        SALT(18)='Null control.                          '
        SALT(19)='Multi-sensor heating/cooling           '
        SALT(20)='Evaporative source (surface)           '
        SALT(21)='Slave capacity control                 '
        SALT(22)='VAV cooling with CAV reheat            '
        SALT(23)='Match sensed values (ideal:2 setpoints)'
        SALT(24)='Adaptive human comfort model           '
        if(iclaw.ne.0)then
          write(outs,'(a,a)') 'Current control law: ',SALT(iclaw)
          call edisp(iuout,outs)
          IX=1
        else
          IX=iclaw
        endif
        helptopic='sys_bctl_types'
        call gethelptext(helpinsub,helptopic,nbhelp)
  63    CALL EPICKS(IX,IVALS,' ','Control law:',
     &    40,24,SALT,'control law (indented=old or obscure)',
     &    IER,nbhelp)
        if(ix.eq.0.or.IVALS(1).eq.0)goto 63
        iclaw=IVALS(1)
        IBCLAW(II,JJ,KK)=iclaw

  62    CALL EASKR(tcps,' ','Period start time?',
     &    0.,'F',24.,'F',0.,'control start',IER,nbhelp)
        if(KK.GT.1)then
          IF(TBCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order.',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif

C If thermophysical property substitution then ask for misc. data.
        if(IBSN(II,1).eq.-99)then
          helptopic='sys_bctl_thermo_sub'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(int(cm(2)).eq.0)then
            outs=' Current property substitution is OFF.'
          elseif(int(cm(2)).eq.1)then
            outs=' Current property substitution is ON.'
          endif
          cm(1)=4.0
          CALL EASKR(cm(2),outs,' Substitution: 0) off, 1) on?',
     &      0.,'F',1.,'F',0.,'subst on/off',IER,nbhelp)

  28      WRITE(HOLD,'(3f8.0)')cm(3),cm(4),cm(5)
          if(IBAN(II,1).ne.0)then
            CALL EASKS(HOLD,' ','MLC index for 1st/2nd/3rd surfaces?',
     &        40,'  0   0  0  ','index for subst',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,cm(3),1.,18.,'F','1st sur mlc',IER)
            CALL EGETWR(HOLD,K,cm(4),1.,18.,'F','2nd sur mlc',IER)
            CALL EGETWR(HOLD,K,cm(5),1.,18.,'F','3rd sur mlc',IER)
            if(ier.ne.0)goto 28
          else
            cm(3)=0.
            cm(4)=0.
            cm(5)=0.
          endif
          call extrctl(0,II,JJ,KK)
          goto 92
        endif

C Make assumptions about the additional data that will be required.
        if(IBCLAW(II,JJ,KK).ge.1.and.IBCLAW(II,JJ,KK).lt.6)then

C Basic control (1) or pre-heat/cool (3) is taken care of by a
C call to editctlperiod.
C Free-floating control (2) is taken care of by a call to editctlperiod.
C Ideal fixed injection (4) is taken care of by a call to editctlperiod.
C PID controller (5) is taken care of by a call to editctlperiod.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.6)then

C Zone - plant connector.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'Current zone-plant connector data is:')
            call edisp(iuout,
     &      ' Supply comp & node, Index, Max Heat W, Mac Cool W')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            call edisp(iuout,' Extract Cmpt & Var')
            write(outs,'(2F9.2)') cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif

          helptopic='sys_bctl_plant_link'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(cm(1),'Number of miscellaneous','data items?',
     &      5.,'F',7.,'F',7.,'no. of misc data items',IER,nbhelp)
     
C Display list of plant components:         
          CALL ASKPCMP(' Supply plant component ? ',
     &                 '-',ISELS,IER)
          cm(2)=ISELS
          if(ISELS.eq.0) return
  
          if(nnodes(ISELS).gt.1) then
            call getnod(ISELS,knode)
            if(knode.eq.0) return
            cm(3)=knode
          else
            cm(3)=1.0
          endif

          CALL EASKR(cm(4),'  Coupling type',
     &      ' 1.) mCpdT calc, 2.) heat flux transfer ? ',
     &      1.,'F',2.,'F',1.,'coupling type',IER,nbhelp)

C Check if coupling index = 1. If so determine if the most recent
C zone temperatures should be used when determining the zone 
C flux (Q=m'Cp(Ts' + Ta)) or if the flux should be calculated
C using zone temperatures from the previous timestep :
C (Q=m'Cp(Ts' + Ta'))
          call eclose(cm(4), 1.0, 0.01,close1)
          if ( close1 ) then
            helptopic='sys_bctl_plant_link'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKAB(
     &        'Zone heat injection calculation method.',
     &        'Options:',
     &        'sequential boundary conditions',
     &        'concurrent boundary conditions',IW,nbhelp)

            if (IW .eq. 2) cm(4) = 5.0

          endif
          CALL EASKR(cm(5),' ','Maximum heating flux (W)?',
     &      0.,'F',0.,'-',1000.,'max heat flux',IER,nbhelp)
          CALL EASKR(cm(6),' ','Maximum cooling flux (W)?',
     &      0.,'-',0.,'-',1000.,'max cool flux',IER,nbhelp)

          IF(int(cm(1)).EQ.7)THEN
           CALL ASKPCMP(' Extract plant component ? ',
     &                  '-',ISELE,IER)
           cm(7)=ISELE
           if(ISELE.eq.0) return

           if(nnodes(ISELE).gt.1) then
             call getnod(ISELE,knode)
             if(knode.eq.0) return
             cm(8)=knode
           else
             cm(8)=1.0
           endif
          ENDIF
  
        elseif(IBCLAW(II,JJ,KK).eq.7.or.IBCLAW(II,JJ,KK).eq.8)then

C Multi-stage (7) is taken care of by a call to editctlperiod.
C CV HAVC variable supply temperature with limit constraints (8) 
C   is taken care of by a call to editctlperiod.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.9)then

C Heat pipe.
          helptopic='sys_bctl_heat_pipe'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current heat pipe data is:')
            call edisp(iuout,
     &' Iter indx, Surf, Out node, In node, Crit T, Max iter, Toler W')
            write(outs,'(7F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),
     &        cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=9.0
          CALL EASKR(cm(2),' Iteration index',
     &      ' 0) no iteration,  1) iteration ? ',
     &      0.,'F',1.,'-',0.,'iteration index',IER,nbhelp)

          CALL EASKR(cm(3),' ',' Surface no with heat pipe ? ',
     &      1.,'F',32.,'F',1.,'surface with heatpipe',IER,nbhelp)
          CALL EASKR(cm(4),' ',' Outer node for heatpipe ? ',
     &      1.,'F',8.,'F',1.,'outer node',IER,nbhelp)
          CALL EASKR(cm(5),' ',' Inner node for heatpipe ? ',
     &      1.,'F',8.,'F',2.,'inner node',IER,nbhelp)
          CALL EASKR(cm(6),' ',' Critical temperature ? ',
     &      -100.,'F',100.,'F',25.,'critical temp',IER,nbhelp)
          CALL EASKR(cm(7),' ',' Maximum no of iterations ? ',
     &      1.,'F',100.,'F',1.,'max iterations',IER,nbhelp)
          CALL EASKR(cm(8),' ',' Flux difference tolerance ? ',
     &      1.,'F',1000.,'F',10.,'flux tolerance',IER,nbhelp)
          CALL EASKR(cm(9),' Trace flag...',
     &      ' 0) no trace, 1) trace ? ',0.,'F',1.,'F',0.,'trace',
     &      IER,nbhelp)
        elseif(IBCLAW(II,JJ,KK).eq.10)then

C Control with separate on/off setpoints for heating and cooling.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.11)then

C Match temp to a function of multi-sensor values (ideal mode).
C This logic is also used for the case of a setpoint temperature
C defined in a temporal file.  Note that users should first have
C setup the control via the context->temporal menu so it is usually
C not required that a user do subsequent editing to the zone control
C law ll details.
          helptopic='sys_bctl_11'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal Match Temp to Multi-Sen` data:')
            call edisp(iuout,
     &     ' Max H W, Min H W, Max C W, Min C W, No. Axy Sen, Sen mode')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
            
            I6=INT(cm(6))
            ndx=0
            if(I6.gt.2) I6=2
            DO 123 L=1,I6              

C Collect up the current information and generate a descriptive
C string to remind the user.
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxliary Sensor Details')              
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              icsn(5)=0
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
              if(INT(cm(7)).eq.4)then
                ndx=12+(L-1)+4*(I6-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
              endif
123         continue   
          endif

          CALL EASKR(cm(1),' ','Number of misc data items (see help)',
     &      10.,'F',18.,'F',12.,'no. of misc data items',IER,nbhelp)

  53      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
          CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0. 10000. 0. ',
     &      ' track another max min heat cool',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 53

          CALL EASKR(cm(6),' ','No of auxiliary sensors?',
     &      0.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,nbhelp)

          I6=INT(cm(6))
          if(I6.eq.1)then
            cm(7)=3.0
          else  
            CALL EASKR(cm(7),' ','Multi-sensor mode (see help)?',
     &        1.,'F',4.,'F',0.,'multi-sensor mode',IER,nbhelp)
          endif
          
C Auxiliary sensor details,
          helptopic='bctl_11_aux_sens'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DO 125 L=1,I6
            ndx=8+4*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            icsn(5)=0
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            CALL EASKR(cm(ndx),' ',
     &        ' Aux sensor detail 1st item (see help)?',
     &        -5.,'F',0.,'-',1.,' Aux sen detail 1st item',IER,nbhelp)
            CALL EASKR(cm(ndx+1),' ',' Aux sensor detail 2nd item?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,nbhelp)          
            CALL EASKR(cm(ndx+2),' ',' Aux sensor detail 3rd item?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,nbhelp)          
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sensor detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,nbhelp)
125       continue 
          lastndx=ndx+4         
          
          I7=INT(cm(7))
          if(I7.eq.4)then
            DO 126 L=1,I6
C Weighted auxiliary sensors mode,
              ndx=lastndx  ! set to next slot after aux senor data.        
              if(i6.eq.1) cm(ndx)=100.0  ! if only one sensor.
              CALL EASKR(cm(ndx),' ',
     &          'Sensor weighting (between 0.0 & 100.0)?',
     &          0.,'F',100.,'F',50.,' Aux sensor weighting',IER,nbhelp)
126         continue
          else
            ndx=nint(cm(1))  ! set this so scaling next to last entry.

C Debug.
C            write(6,*) 'lastndx and ndx ',lastndx,ndx,cm(1) 
  
          endif

C At end of data add in scaling factor and offset. Typically
C a scaling factor is 1.0
          if(cm(ndx).lt.0.01) cm(ndx+1)=1.0
          CALL EASKR(cm(ndx),' ','Sensor scaling factor?',
     &        0.,'F',10.,'F',1.,' scaling factor',IER,nbhelp)
          CALL EASKR(cm(ndx+1),' ','Sensor offset factor?',
     &        -10.,'F',10.,'F',1.,' offset factor',IER,nbhelp)

        elseif(IBCLAW(II,JJ,KK).eq.12)then

C Match temp to a function of multi-sensor values (on-off mode).
          helptopic='sys_bctl_12'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal Match Temp to Multi-Sen` data:')
            call edisp(iuout,
     &  ' Max Htg W, Max Clg W, Htg Dif, Clg Dif,No. Axy Sen, Sen mode')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
            
            I6=INT(cm(6))
            DO 133 L=1,I6              
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxiliary Sensor Details')
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)

C Collect up the current information and generate a descriptive
C string to remind the user.
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              icsn(5)=0
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
              if(INT(cm(7)).eq.4)then
                ndx=12+(L-1)+4*(I6-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
             endif
133        continue   
          endif

          CALL EASKR(cm(1),' ',' No. of misc data items ',
     &      10.,'F',16.,'F',10.,'no. of misc data items',IER,nbhelp)

 153      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
          CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0. 10000. 0. ',
     &      ' track another max min heat cool',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 153

          CALL EASKR(cm(6),' ',' No of auxiliary sensors ? ',
     &      0.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,nbhelp)

          CALL EASKR(cm(7),' ',' Multi-sensor mode ? ',
     &      1.,'F',4.,'F',0.,' multi-sensor mode',IER,nbhelp)

C Auxiliary sensor details,
          I6=INT(cm(6))          
          helptopic='bctl_12_aux_sens'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DO 135 L=1,I6
            ndx=8+4*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            icsn(5)=0
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            CALL EASKR(cm(ndx),' ',' Aux sen detail 1st item ?',
     &        -3.,'F',0.,'-',0.,' Aux sen detail 1st item',IER,nbhelp)
            CALL EASKR(cm(ndx+1),' ',' Aux sen detail 2nd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,nbhelp)          
            CALL EASKR(cm(ndx+2),' ',' Aux sen detail 3rd item ?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,nbhelp)
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sen detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,nbehlp)
135       continue 

          I7=INT(cm(7))
          if(I7.eq.4)then
           DO 136 L=1,I6

C Weighted auxiliary sensors mode,   
            ndx=12+(L-1)+4*(I6-1)
            CALL EASKR(cm(ndx),' ',' Aux sensor weighting ? ',
     &       0.,'F',100.,'F',50.,' Aux sensor weighting',IER,nbhelp)
136        continue
          endif 
        elseif(IBCLAW(II,JJ,KK).eq.13)then

C Time proportioning control with separate on/off setpoints for heating
C and cooling (initial part of the input is similar to control 10).
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.14)then

C Floating (`three-position') control.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92

        elseif(IBCLAW(II,JJ,KK).eq.15)then

C Logic Optimum Start Controller.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.16)then

C Logic Optimum Stop Controller.
          helptopic='sys_bctl_opt_stop'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current Logic Stop controller data is:')
            call edisp(iuout,
     &      ' Htg cpy, Clg cpy, Htg Set pt, Clg Set pt, Dsrd temp')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            call edisp(iuout,' Throt rng, Occy Dep, Tim Dif, Init Gues')
            write(outs,'(4F9.2)')cm(7),cm(8),cm(9),cm(10)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=9.0
          CALL EASKR(cm(2),' ',' Maximum heating capacity (W) ? ',
     &      0.,'F',0.,'-',1000.,' max htg capacity',IER,nbhelp)
          CALL EASKR(cm(3),' ',' Maximum cooling capacity (W) ? ',
     &      0.,'F',0.,'-',1000.,' max clg capacity',IER,nbhelp)
          CALL EASKR(cm(4),' ',' Htg Set pt ? ',
     &      0.,'F',0.,'-',20.,' Htg Set pt',IER,nbhelp)
          CALL EASKR(cm(5),' ',' Clg Set pt ? ',
     &      0.,'F',0.,'-',20.,' Clg Set pt',IER,nbhelp)

          CALL EASKR(cm(6),' ',' Desired temp ? ',
     &      0.,'F',0.,'-',20.,' Desired temp',IER,nbhelp)
          CALL EASKR(cm(7),' ',' Throtl range ? ',
     &      0.,'F',0.,'-',2.,' Throtl range',IER,nbhelp)
          CALL EASKR(cm(8),' ',' Occpy dept time ? ',
     &      0.,'F',0.,'-',17.,' Occpy dept time',IER,nbhelp)

          CALL EASKR(cm(9),' ',' Min time diff ? ',
     &      0.,'F',0.,'-',0.1,' Min time diff',IER,nbhelp)
          CALL EASKR(cm(10),' ',
     &      'Initial guess of time of optimal stop?',
     &      0.,'F',24.,'F',0.1,' Initial Guess',IER,nbhelp)

        elseif(IBCLAW(II,JJ,KK).eq.17)then

C Fuzzy Logic PI-PD Controller.
          helptopic='sys_bctl_fuzzy'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current Fuzzy Logic controller data is:')
            call edisp(iuout,
     &      ' Ref data set, Ctl mode, Set pt, Defuz meth')
            write(outs,'(4F9.2)')cm(2),cm(3),cm(4),cm(5)
            call edisp(iuout,outs)
            call edisp(iuout,' Scale(err), Scale(err rate), Scale(o/p)')
            write(outs,'(4F9.2)')cm(6),cm(7),cm(8)
            call edisp(iuout,outs)
            call edisp(iuout,' ')
          endif
          cm(1)=7.0
          CALL EASKR(cm(2),' ',' Referenced data set ? ',
     &      0.,'F',20.,'F',1.,' Ref data set',IER,nbhelp)

          CALL EASKR(cm(3),' ',' Control Mode ? ',
     &      1.,'F',2.,'F',1.,' Ctl mode',IER,nbhelp)

          CALL EASKR(cm(4),' ',' Set point ? ',
     &    -100.,'F',100.,'F',20.,' Set point',IER,nbhelp)

          CALL EASKR(cm(5),' ',' Defuzzification method ? ',
     &      1.,'F',2.,'F',1.,' Defuz method',IER,nbhelp)

          CALL EASKR(cm(6),' ',' Error scale ? ',
     &    -100.,'F',100.,'F',1.,' Error Scale',IER,nbhelp)

          CALL EASKR(cm(7),' ',' Change of Error scale ? ',
     &    -100.,'F',100.,'F',1.,' Change of Error Scale',IER,nbhelp)

          CALL EASKR(cm(8),' ',' Output scale ? ',
     &    -100.,'F',100.,'F',1.,' Output Scale',IER,nbhelp)

        elseif(IBCLAW(II,JJ,KK).eq.18)then

C Null controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Null controller has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IBCLAW(II,JJ,KK).eq.19)then

C Ideal multi-sensor ON/OFF heating/cooling controller. The logic of this
C controler is implemented in bcfunc.F in subroutine BCL19.
          helptopic='sys_bctl_multi_sensor'
          call gethelptext(helpinsub,helptopic,nbhelp)
 243      WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)

C First 7 data-items are equal to basic controller above.
          call edisp(iuout,' Ideal multi-sensor ON/OFF controller:')
          cm(1)=6.0
          CALL EASKS(HOLD,' Max & Min heat capacity, Max & Min cool ',
     &      ' capacity for basic control (see help): ',
     &      40,' 1000. 0. 1000. 0. ','max min heat cool',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
          if(ier.ne.0)goto 243

 244      WRITE(HOLD,'(2f8.2)')cm(6),cm(7)
          CALL EASKS(HOLD,
     &      'Heating & cooling setpoints within the zone where flux',
     &      'is injected or extracted (see help)',40,' 20. 24. ',
     &      'heat cool setpoints',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(6),-100.,100.,'F','Heat setp',IER)
          CALL EGETWR(HOLD,K,cm(7),-100.,100.,'F','Cool setp',IER)
          if(ier.ne.0)goto 244

C If no cooling capacity then set cooling set point to 100C.
          CALL ECLOSE(cm(4),0.0,0.001,close1)
          CALL ECLOSE(cm(5),0.0,0.001,close2)
          if(close1.and.close2)then
            call edisp(iuout,' ')
            call edisp(iuout,' No cooling capacity defined - setting')
            call edisp(iuout,' the cooling to (100 deg.C).')
            cm(7)=100.
          endif

          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current `Ideal multi-sensor controller` data:')
            call edisp(iuout,
     &     ' No. Aux Sen')
            write(outs,'(1F9.2)')cm(8)
            call edisp(iuout,outs)

C << check what kinds of sensors can actually be used here - probably should
C << only select which zone and set the other values to zero.
            I8=INT(cm(8))
            DO 140 L=1,I8              
              ndx=8+6*(L-1)
              call edisp(iuout,' Auxiliary Sensor Details')
              write(outs,'(6F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),
     &                             cm(ndx+3),cm(ndx+4),cm(ndx+5)
              call edisp(iuout,outs)

C Collect up the current information and generate a descriptive
C string to remind the user.
              icsn(1)=nint(cm(ndx))
              icsn(2)=nint(cm(ndx+1))
              icsn(3)=nint(cm(ndx+2))
              icsn(4)=nint(cm(ndx+3))
              icsn(5)=0
              SSTR=' '
              call decodesensor(0,icsn,SSTR,SSTRB,ier)
              call edisp(iuout,SSTR)
140         continue   
          endif

          I8=int(cm(8))
          CALL EASKI(I8,' ',' Number of aux sensors (see help)',
     &      0,'F',MSEN,'F',3,'no. of aux sensors',IERI,nbhelp)
          if(ieri.eq.-3) goto 92
          cm(1)=7.+6.*I8
          cm(8)=float(I8)

C Auxiliary sensor details,
          DO 141 L=1,I8
            ndx=9+6*(L-1)

C Collect up the current information and generate a descriptive
C string to remind the user.

C << again check if we really could use anything other than another
C << air node to sense.
            icsn(1)=nint(cm(ndx))
            icsn(2)=nint(cm(ndx+1))
            icsn(3)=nint(cm(ndx+2))
            icsn(4)=nint(cm(ndx+3))
            icsn(5)=0
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            call edisp(iuout,SSTR)
            call edisp(iuout,' ')
            WRITE(outs,'(a,i2,a)')' For aux sensor ',L,' define....'
            call edisp(iuout,outs)
            CALL EASKR(cm(ndx),' ',' Aux sensor detail 1st item ?',
     &        -3.,'F',0.,'-',1.,' Aux sensor detail 1st item',
     &        IER,nbhelp)
            CALL EASKR(cm(ndx+1),' ',' Aux sensor detail 2nd item ?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 2nd item',
     &        IER,nbhelp)
            CALL EASKR(cm(ndx+2),' ',' Aux sensor detail 3rd item ?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 3rd item',
     &        IER,nbhelp)
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sensor detail 4th item (always use zero)?',
     &        0.,'F',0.,'-',0.,' Aux sensor detail 4th item',
     &        IER,nbhelp)

            CALL EASKR(cm(ndx+4),' ',' Aux sensor heating set-point ?',
     &        0.,'F',0.,'-',0.,' Aux sensor heating set-point',
     &        IER,nbhelp)

            CALL EASKR(cm(ndx+5),' ',' Aux sensor cooling set-point ?',
     &        0.,'F',0.,'-',0.,' Aux sensor cooling set-point',
     &        IER,nbhelp)

C << probably a good place to re-iterate what the user said... >>

141       continue
 
        elseif(IBCLAW(II,JJ,KK).eq.20)then

C Latent/evaporative source, see bcl20 for details.
          helptopic='sys_bctl_wetted_surf'
          call gethelptext(helpinsub,helptopic,nbhelp)
          mode=int(cm(2))
          Abasin=cm(3)
          alpha= cm(4)
          alphar= cm(5)    
          tbas=  cm(6) 
          tbasex= cm(7) 
          fsupply= cm(8) 
          tsupply= cm(9)   
          abasw= cm(10)  
          ubasw= cm(11) 

C Ask for data.
          CALL EASKAB('Wetted surface options:',' ',
     &    'Wet opaque surface','Swimming pool surface',IW,nbhelp)
          mode = IW-1
          if(mode.eq.0)then
C Surface area and HTC picked up from ESP-r
            cm(1)=1.0
            cm(2)=0.0
          elseif(mode.eq.1)then
            call phelpd('pool control help',nbhelp,'-',0,0,IER)       

            write(hold,'(3(f10.4,2X))') Abasin,alpha,alphar
            CALL EASKS(HOLD,'Pool surface area(m^2), convective and',
     &        'radiant heat trans coefts (W/m^2.K)',40,
     &        '100.0   1.5   5.1',
     &        ' surf area and coefts',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,Abasin,0.,9999.,'W','Abasin',IER)
            CALL EGETWR(HOLD,K,alpha,0.,99.,'W','alpha',IER)
            CALL EGETWR(HOLD,K,alphar,0.,99.,'W','alphar',IER)
            cm(1)=10.0
            cm(2)=1.0
            cm(3)=Abasin
            cm(4)=alpha
            cm(5)=alphar
            write(hold,'(2(f10.4,2X))') tbas,tbasex
            CALL EASKS(HOLD,'Pool temperature and ground temperature',
     &        '(degC)',40,
     &        '27.0 12.0',
     &        ' pool and ground temperatures',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,tbas,0.,99.,'W','tbas',IER)
            CALL EGETWR(HOLD,K,Tbasex,0.,99.,'W','tbasex',IER)
            cm(6)=tbas
            cm(7)=tbasex
            write(hold,'(2(f10.4,2X))') fsupply,tsupply
            CALL EASKS(HOLD,'water supply rate (kg/s), water supply ',
     &        'temp (degC)',40,
     &        '5.0 25.0',
     &        ' water data ',IER,nbhelp)   
            k=0
            CALL EGETWR(HOLD,K,fsupply,0.,9999.,'W','fsupply',IER)
            CALL EGETWR(HOLD,K,tsupply,0.,9999.,'W','tsupply',IER)
            cm(8)=fsupply
            cm(9)=tsupply  
            write(hold,'(2(f10.4,2X))') abasw,ubasw
            CALL EASKS(HOLD,'submerged wall surface area (m^2), ',
     &        'submerged wall U-value (W/m^2K)',40,
     &        '100.0 0.3',
     &        ' water data ',IER,nbhelp)   
            k=0
            CALL EGETWR(HOLD,K,abasw,0.,9999.,'W','abasw',IER)
            CALL EGETWR(HOLD,K,ubasw,0.,9999.,'W','ubasw',IER)
            cm(10)=abasw
            cm(11)=ubasw                     
          endif
        
        elseif(IBCLAW(II,JJ,KK).eq.21.or.IBCLAW(II,JJ,KK).eq.22)then
 
C Slave Capacity Controller or VAV controller.
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.23)then  

C Read setpoints from temporal file. This control law was developed for
C the UK NCM and takes in setpoints from tdf data and is based on the
C logic on control law 1
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92
        elseif(IBCLAW(II,JJ,KK).eq.24)then  

C Adaptive human comfort model. (Same algorithm as used within the flow
C domain therefore refer to same reference publication)
          nper=NBCDP(II,JJ)
          loopdaytype=nbcdt(II)
          if(KK.GT.1)then
            prevstart=TBCPS(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Zone control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawT=ier
            goto 77
          endif
          goto 92
        else

C The fall-through (none of the above) control edit.
          helptopic='sys_bctl_fall_through'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKI(NITEMS,' ',' No of control law data items ?',
     &      0,'F',MISC,'F',1,'control law data items',IERI,nbhelp)
          if(ieri.eq.-3) goto 92
          cm(1)=FLOAT(NITEMS)
          if(NITEMS.GT.0)then
            do 33 MM=2,NITEMS+1
              write(outs,'(A,I3)')' Item ',MM-1
              CALL EASKR(cm(MM),outs,' value: ',
     &          0.,'-',0.,'-',1.,'miscel data',IER,nbhelp)
  33        continue
          endif
        endif  

C Extract from working array to bmiscd() common structure
        call extrctl(0,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permission issue writing miscellaneous data.','W')
      else
        CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      endif
      return

      END

C *********** EOCNTRL
C Edit optical control function II.
      SUBROUTINE EOCNTRL(II,IV,IER)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
C      COMMON/GPICK/GS(MGOPT),nopt

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      CHARACTER*84 VERT(20)
      CHARACTER*65 VERT2(20)
      CHARACTER outs*124,SSTR*96,TMP*96,KEY*1
      character outsa*84,outsb*84,outsc*84,ltmp*248
C      CHARACTER GS*52
C      DIMENSION IGVAL(60)
      logical close1
      logical logic_cminitial ! needed to initialise cmlocal array in order to save values              
      dimension cmlocal(misc)   ! for local editing of cm data.
      real cmlocal
      dimension sorting(6),bsorting(6),sort_tmp(6),bsort_tmp(6) ! for local sorting of cm bidirectional data.
      dimension sortset(6),sortmerge(6)   
      real sorting,bsorting,sort_tmp,bsort_tmp,sortset,sortmerge
      integer isortCounter,ibsortCounter
      integer LL
      logical bNumsAreEqual
      integer isetp_displayCM,idat_displayCM !for display purposes to be combatible with gcc4
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EOCNTRL'  ! set for subroutine

C Number of optical functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Optical control day types.
      J=IV
      JJ=J
      logic_cminitial=.false.
C Logical needed to initialiase cm arrays
C This initialiasation applies only to ioclaw=2. It will have to change in the future
      do 952 inmisc=1,6
        if(.not.logic_cminitial)then
          cm(2*inmisc)=-30.0
          cm(2*inmisc+1)=1.
        endif
  952 continue

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',nocdp(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      LL=nocdp(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(5,II,JJ,KK)
        CALL EVCNTRL(5,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(5,II,JJ,KK,TMP,LTMP)

        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.9)then
            write(outsb,'(9F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(9F9.1)')(cm(L),L=2,10)
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:47),outsc(1:34)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Setup help text for optical controls.
      helptopic='optical_ctl_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Optical control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(5,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(5,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(5,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then

C The writing pattern here is taken from the flow controls
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
        endif
        call edisp(iuout,outs)
        call edisp(iuout,' ')

C Optics controller.  Set type to same as initial for this optics
C function (ie. all periods are the same).
        ictyp=IOCTYP(II,1,1)
        IOCTYP(II,JJ,KK)=ictyp
        write(outs,'(a,i3)')' Current controller type: ',ictyp
        call edisp(iuout,outs)
        call edisp(iuout,
     &      'Note: to change this re-edit the optics sensor')

        ino=iclaw
        idno=2
        write(outs,'(a,i3,a)')'Select a control law (currently: ',
     &    iclaw,')'
        call MENUATOL(outs,' Available laws',
     &   'a switch optics above setpoint (NA)',
     &   'b switch bidirectional sets (setp. & set)',' ',' ',' ',' ',
     &   ' ',' ',' ',' ',' ',' ',ino,idno,nbhelp)
        iclaw=ino
        IOCLAW(II,JJ,KK)=iclaw

C Exclude here control law 1 as it is not available at the moment
        if(ioclaw(II,JJ,KK).ne.1)then
  62      CALL EASKR(tcps,' ',' Period start time ? ',
     &      0.,'F',24.,'F',0.,'control start',IER,nbhelp)
          if(KK.GT.1)then
            IF(tocps(II,JJ,KK-1).GE.tcps)then
              call usrmsg(' Periods out of order..',' ','W')
              GOTO 62
            endif
          else
            CALL ECLOSE(tcps,0.0,0.001,close1)
            IF(.NOT.close1)then
              call usrmsg(' ',' First control must begin @ 0','W')
              GOTO 62
            endif
          endif
        endif

C Ask for miscel. data.
        if(ioclaw(II,JJ,KK).eq.1)then
C          cm(1)=2.
C          cm(2)=10.0
C          CALL EASKR(cm(2),' Set point:',' ',
C     &      -1000.,'F',1000.,'F',10.,'setpoint',IER,nbhelp) 

C Similar logic as the EDWINO Subroutine
C          noptz=nopt        
C          IX=1
C          CALL EPICKS(IX,IGVAL,' ',' Glazing types: ',
C     &      52,noptz,GS,'raw opticals (to use above setpoint)',
C     &      IER,nbhelp)
C          cm(3)=IGVAL(1)
          WRITE(outs,'(a)')' This option is not currently available. '
          call edisp(iuout,outs)
        elseif(ioclaw(II,JJ,KK).eq.2)then
C          cm(1)=12.
C          cm(2)=10.0
C          CALL EASKR(cm(2),' Set point:',' ',
C     &      -1000.,'F',1000.,'F',10.,'setpoint',IER,nbhelp)
 
C Similar logic as the EDWINO Subroutine       
C          CM(3)=1.0
C          I3=int(cm(3))
C          CALL EASKI(I3,' ','Which bidirectional set to use (1-40)?',
C     &       1,'F',40,'F',1,'1st set is default',IERI,nbhelp)
C          cm(3)=float(I3)
C        endif

C<<<<<<<<attempt to introduce an emenu here
          logic_cminitial=.true.
          cm(1)=12.
  
          do 11 ij=1,misc
            cmlocal(ij)=cm(ij)
  11      continue

C Set it to 12 for the moment
         do 852 inmisc=1,6
            sorting(inmisc)=0.0
            bsorting(inmisc)=0.0
            sortmerge(inmisc)=0.0
            sortset(inmisc)=0.0
  852     continue

C Number of actual items displayed.
  741     IER=0
          M=6
          MVERT=9
          do 963 ibictl_item=1,M
            isetp_displayCM=2*ibictl_item
            idat_displayCM=isetp_displayCM+1     
            CALL EMKEY(ibictl_item,KEY,IER)
            WRITE(VERT2(ibictl_item),'(A1,a,F7.1,2a,F4.0)') KEY,
     &           ' Set-point: ',cmlocal(isetp_displayCM),' & ',
     &           'dataset to use (above set-point): ',
     &           cmlocal(idat_displayCM)
  963     continue     
          VERT2(M+1)='_______________________________________________'
          VERT2(M+2)  ='? help                                    '
          VERT2(M+3)  ='- exit this menu & save any changes       '

C Now display the menu.
          CALL EMENU('Control for bidirectional datasets',VERT2,MVERT,
     &               IVERT)
          if(IVERT.ge.1.and.IVERT.lt.MVERT-2)then
            CALL EASKR(cmlocal(ivert*2),'Bidirectional Set-point:',' ',
     &      -10000.,'F',10000.,'F',-30.,'setpoint',IER,nbhelp)            
            I3=int(cmlocal(ivert*2+1))
            CALL EASKI(I3,' ','Which bidirectional set to use (1-40)?',
     &       1,'F',40,'F',1,'1st set is default',IERI,nbhelp)
            cmlocal(ivert*2+1)=float(I3)
            goto 741
          elseif(IVERT.EQ.MVERT-1)then
            CALL PHELPD('bidirectional control help',nbhlelp,
     &        '-',0,0,IER)
            goto 741
          elseif(IVERT.EQ.MVERT)then

C Sort the setpoints in descending order.
C Use a temporary array to hold them for sorting
            do 147 ix=1,6           
              if(cmlocal(ix*2).ge.0.0)then

C Get the positives for sorting
                sorting(ix)=cmlocal(ix*2)
              else
C Now get the negatives for sorting              
                bsorting(ix)=cmlocal(ix*2)             
              endif            

C Make them positive for using them in SORTR
              bsorting(ix)=abs(bsorting(ix))                 
  147       continue
            KFLAG = -1
            call SORTR(sorting,sort_tmp,6,KFLAG)
            call SORTR(bsorting,bsort_tmp,6,KFLAG)
            isortCounter=0
            ibsortCounter=0

C assign (next line only) back the negative values as they were before the sorting
            do 159 irevert=1,6
              bsorting(irevert)=-bsorting(irevert)            
  159       continue
            do 146 ixreturn=1,6
              do 145 ixmatch=1,6
                CALL ECLOSE(sorting(ixreturn),cmlocal(ixmatch*2),0.01,
     &               bNumsAreEqual)
                if(bNumsAreEqual)then
                  CALL ECLOSE(sorting(ixreturn),sorting(ixreturn-1),
     &               0.01,bNumsAreEqual)                  
                  if(bNumsAreEqual)then
                    write(outs,'(a,F7.1,a)')' set-point: ',
     &                  cmlocal(ixmatch*2),' was found more than once.'
                    call edisp(iuout,outs)     
                    write(outs,'(3a)')'Datasets for this setpoint ',
     &                 'must be the same ',
     &                 '(will use the first one in the list otherwise).'
                    call edisp(iuout,outs)

C do not count when there is dublication
                    if(isortCounter.gt.0)then
                      isortCounter=isortCounter-1
                    endif
                  endif
                  isortCounter=isortCounter+1

C sortmerge will merge sorting and bsorting back together                  
                  sortmerge(isortCounter)=cmlocal(ixmatch*2)     
                  sortset(isortCounter)=cmlocal(ixmatch*2+1)
                endif
                CALL ECLOSE(bsorting(ixreturn),cmlocal(ixmatch*2),0.01,
     &               bNumsAreEqual)
                if(bNumsAreEqual)then
                  CALL ECLOSE(bsorting(ixreturn),bsorting(ixreturn-1),
     &                 0.01,bNumsAreEqual)                
                  if(bNumsAreEqual)then
                    write(outs,'(a,F7.1,a)')' set-point: ',
     &                  cmlocal(ixmatch*2),' was found more than once.'
                    call edisp(iuout,outs)     
                    write(outs,'(3a)')'Datasets for this setpoint ',
     &                 'must be the same ',
     &                 '(will use the first one in the list otherwise).'
                    call edisp(iuout,outs)

C do not count when there is dublication
                    if(ibsortCounter.gt.0)then
                      ibsortCounter=ibsortCounter-1
                    endif                  
                  endif             
                  ibsortCounter=ibsortCounter+1

C sortmerge will merge sorting and bsorting back together                  
C 6 is the total number of sortmerge (e.g. set-points). The aim is to place
C the negatives at the end of the array
                  if(ibsortCounter.gt.0)then
                    sortmerge(7-ibsortCounter)=cmlocal(ixmatch*2) 
                    sortset(7-ibsortCounter)=cmlocal(ixmatch*2+1)
                  endif
                endif
  145         continue                             
  146       continue
            do 144 ipassback=1,6 
              cmlocal(ipassback*2)=sortmerge(ipassback)
              cmlocal(ipassback*2+1)=sortset(ipassback)

C Debug.
C              write(6,*) sortmerge(ipassback),'  ',sortset(ipassback)

  144       continue             
            do 13 ij=1,misc
              cm(ij)=cmlocal(ij)
  13        continue      
C            logic_cminitial=.true.
          endif
        endif
C<<<<<<<<<<<END OF EMENU HERE>>>>>>        

C Extract from working array to omiscd() common structure
        call extrctl(5,II,JJ,KK)
        call usrmsg(' ',' ','-')
        logic_cminitial=.true.
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    CALL USRMSG(' ','Problem writing misc. data feedback.','W')
      return
      END

C ********************************************************************
C                         --edit_CFC_control--
C
C Created by: Bart Lomanowski
C Initial Creation Date: April 2009
C
C This subroutine provides an interface for editing a complex
C fenestration control function in prj. It is based on subroutine
C EBCNTRL. 
C
C ********************************************************************
      SUBROUTINE edit_CFC_control(ITRC,II,IV,IER)
#include "building.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN

      common/sctl/tcps,ictyp,iclaw,cm(misc)

      DIMENSION SALT(7),PSALT (2), IVALS(7), IPVALS(1)
      CHARACTER*82 VERT(20)

      CHARACTER outs*124,SSTR*96,TMP*96,SALT*40,KEY*1
      character PSALT*40
      character outsa*84,outsb*84,outsc*84,ltmp*248
      logical close1,useold

      integer iclawt   ! control law index passed back from editctlperiod
      integer icfoc
      integer icurrentctltype
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edit_CFC_control'  ! set for subroutine
      
      icfoc = 6
      
C Number of complex fenestration functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Complex fenestration control day types.
      J=IV
      JJ=J

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',
     &                          nCFCdayctlperiods(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per|start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.|time |property|property |                 |     '
      LL=nCFCdayctlperiods(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1

C determine control type and misc data items based on actuator information. 
C        if(iCFCactuator(II,1).eq.0)then
C            iCFCctltype(II,JJ,KK)=0
C            CFCmiscdata(II,JJ,KK,1)=2.
C        elseif(iCFCactuator(II,1).eq.1)then
C            iCFCctltype(II,JJ,KK)=1
C            CFCmiscdata(II,JJ,KK,1)=4.
C        else
C            iCFCctltype(II,JJ,KK)=0
C            CFCmiscdata(II,JJ,KK,1)=2.
C        endif
      
        call stfctl(icfoc,II,JJ,KK)
        CALL EVCNTRL(icfoc,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(icfoc,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F6.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.8)then
            write(outsb,'(8F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(8F9.1,a)')(cm(L),L=2,9),'..'
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:46),outsc(1:32)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      helptopic='optical_CFC_ctl_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
       CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        CALL EASKABC(' ','Options:',
     &    'add period','delete period','cancel',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(icfoc,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(icfoc,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the CFCctlperiodstart() iCFCctltype(),iCFCctllaw(), 
C CFCmiscdata(II,JJ,KK,?) common and stuff into working 
C common array 'cm' for editing.  At end of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(icfoc,II,JJ,KK)

C Remember current control type
        icurrentctltype=ictyp
        
        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.10)then
            write(outs,'(10F12.4)')(cm(L),L=2,NITEMS)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(10F12.4)')(cm(L),L=2,10)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.20)then
              write(outs,'(10F12.4)')(cm(L),L=11,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(10F12.4)')(cm(L),L=11,20)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
              write(outs,'(10F12.4)')(cm(L),L=21,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif
        call edisp(iuout,' ')

        SALT(1)= 'senses temp.      actuates shade ON/OFF'
        SALT(2)= 'senses temp.      actuates slat angle  '
        SALT(3)= 'senses sol-rad    actuates shade ON/OFF'
        SALT(4)= 'senses sol-rad    actuates slat angle  '
        SALT(5)= 'senses wind speed actuates shade ON/OFF'
        SALT(6)= 'senses wind dir.  actuates shade ON/OFF'
        SALT(7)= 'no sensor (schedule only)              '
        
        call edisp(iuout,' Current controller type: ')
        call edisp(iuout,SALT(iCFCctltype(II,JJ,KK)))
        
        IX=1
  64    CALL EPICKS(IX,IVALS,' ',' Controller type: (see help)',
     &         40,7,SALT,'controller type',IER,nbhelp)
        if(ix.eq.0.or.IVALS(1).eq.0)goto 64
        if(ix.ne.0)iCFCctltype(II,JJ,KK)=IVALS(1)
        ictyp=iCFCctltype(II,JJ,KK)

        if(ictyp.ne.icurrentctltype)then
C Provide some default values for control data based on the
C control type.
        if(ictyp.eq.1)then     ! senses temp., actuates shade ON/OFF
          cm(1)=2.0
          cm(2)=25.0       ! shade ON setpoint [C]
          cm(3)=22.0       ! shade OFF setpoint [C]
        elseif(ictyp.eq.2)then ! senses temp., actuates slat angle
          cm(1)=4.0
          cm(2)=25.0       ! slats closed setpoint [C]
          cm(3)=22.0       ! slats open setpoint [C]
          cm(4)=80.0       ! closed slat angle [deg]
          cm(5)=0.0        ! open slat angle [deg]
        elseif(ictyp.eq.3)then ! senses solar radiation, actuates shade ON/OFF
          cm(1)=2.0
          cm(2)=500.0      ! shade ON setpoint [W/m2]
          cm(3)=400.0      ! shade OFF setpoint [W/m2]
        elseif(ictyp.eq.4)then ! senses solar radiation, actuates slat angle
          cm(1)=4.0
          cm(2)=500.0      ! slats closed setpoint [W/m2]
          cm(3)=400.0      ! slats open setpoint [W/m2]
          cm(4)=80.0       ! closed slat angle [deg]
          cm(5)=0.0        ! open slat angle [deg]        
        elseif(ictyp.eq.5)then ! senses wind speed, actuates shade ON/OFF
          cm(1)=2.0
          cm(2)=10.0       ! shade ON setpoint [m/s]
          cm(3)=8.0        ! shade OFF setpoint [m/s]       
        elseif(ictyp.eq.6)then ! senses wind direction, actuates shade ON/OFF
          cm(1)=2.0
          cm(2)=180.0       ! wind direction band start point for shade OFF[deg]
          cm(3)=270.0       ! wind direction band end point for shade OFF [deg]               
        elseif(ictyp.eq.7)then ! no control, schedule only
          cm(1)=2.0
          cm(2)=1.0       ! 0 shade OFF, 1 shade ON
          cm(3)=45.0      ! default slat angle for current period[deg]  
        endif  
        endif
          
C Complex fenestration contol law.
        PSALT(1)= 'Basic control '
        PSALT(2)= 'Schedule      '
        write(outs,'(a,a)')' Current control law: ',
     &                      PSALT(iCFCctllaw(II,JJ,KK))
        call edisp(iuout,outs)
        IX=1
  65    CALL EPICKS(IX,IPVALS,' ',' Complex fen. control law: ',
     &         40,2,PSALT,'control law',IER,nbhelp)
        if(ix.eq.0.or.IPVALS(1).eq.0)goto 65
        if(ix.ne.0)iclaw=IPVALS(1)
        iCFCctllaw(II,JJ,KK)=iclaw
        
C If menu editing version available use it otherwise use the
C sequential question interface. If return from editctlperiod
C having selected a control law it does not deal with then
C this will be signaled by an ier condition equal to the control
C law that has been selected so further user selection of control
C law can be jumped around.
        useold=.false.
        iclawt=-1
  77    if(useold)then
          if(iclawt.ge.0.and.iclawt.le.2)then
            iclaw=iclawt
            iCFCctllaw(II,JJ,KK)=iclaw
            useold=.false.
            goto 62
          endif
        endif
        
        if(iclaw.ge.1)then

C Remember the previous start (prevstart) and the total number of
C period in this control loop (nper) and the number of day types
C included in the control loop defintion (loopdaytype) before
C starting up the menu based control period definition.
          nper=nCFCdayctlperiods(II,JJ)
          loopdaytype=nCFCctldaytypes(II)
          if(KK.GT.1)then
            prevstart=CFCctlperiodstart(II,JJ,KK-1)
          else
            prevstart=0.0
          endif
          call editctlperiod(' Complex fen. control period data',
     &      icfoc,II,JJ,KK,nper,loopdaytype,prevstart,'-',ier)
          if(ier.gt.1)then
            useold=.true.
            iclawt=ier
            goto 77
          endif
          goto 92
        endif

  62    CALL EASKR(tcps,' ','Period start time?',
     &    0.,'F',24.,'F',0.,'control start',IER,nbhelp)
        if(KK.GT.1)then
          IF(CFCctlperiodstart(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order.',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif

      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    if(IOS.eq.2)then
        CALL USRMSG(' ',
     &    'Permission issue writing miscellaneous data.','W')
      else
        CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      endif
      return

      END
      
C *********** EPCNTRL
C Edit plant control function II.
      SUBROUTINE EPCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      DIMENSION SALT(42),IVALS(42),PSALT(18),IPVALS(18)
      CHARACTER*84 VERT(20)

      CHARACTER outs*124,SSTR*96,TMP*96,SALT*40,PSALT*40,KEY*1
      character outsa*84,outsb*84,outsc*84,ltmp*248

      CHARACTER hold*70
      logical close1
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found
      integer iother
      helpinsub='EPCNTRL'  ! set for subroutine


C Number of plant loops and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J
      iother=1  ! index not yet assigned within the code near
                ! line 4026

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NPCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      LL=NPCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 90 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(1,II,JJ,KK)
        CALL EVCNTRL(1,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(1,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.9)then
            write(outsb,'(9F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(9F9.1)')(cm(L),L=2,10)
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:47),outsc(1:34)
  90  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      helptopic='plant_ctl_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        helptopic='plant_ctl_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(1,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(1,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        KK=IVERT-5
        call stfctl(1,II,JJ,KK)
        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.26)then
              write(outs,'(13F9.1)')(cm(L),L=14,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(13F9.1)')(cm(L),L=14,26)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        SALT(1)= 'senses dry bulb           actuates flux'
        SALT(2)= 'senses dry bulb           actuates flow'
        SALT(3)= 'senses enthalpy           actuates flux'
        SALT(4)= 'senses enthalpy           actuates flow'
        SALT(5)= 'senses 1st ph mass flow   actuates flux'
        SALT(6)= 'senses 1st ph mass flow   actuates flow'
        SALT(7)= 'senses 2nd ph mass flow   actuates flux'
        SALT(8)= 'senses 2nd ph mass flow   actuates flow'
        SALT(9)= 'senses adl plant output   actuates flux'
        SALT(10)='senses adl plant output   actuates flow'
        SALT(11)='senses RH                 actuates flux'
        SALT(12)='senses RH                 actuates flow'
        SALT(13)='senses dry bulb       actuates variable'
        SALT(14)='senses enthalpy       actuates variable'
        SALT(15)='senses 1st ph flow    actuates variable'
        SALT(16)='senses 2nd ph flow    actuates variable'
        SALT(17)='senses plant output   actuates variable'
        SALT(18)='senses RH             actuates variable'
        SALT(19)='senses dry bulb     actuates mass ratio'
        SALT(20)='senses enthalpy     actuates mass ratio'
        SALT(21)='senses 1st ph flow  actuates mass ratio'
        SALT(22)='senses 2nd ph flow  actuates mass ratio'
        SALT(23)='senses plant o/p    actuates mass ratio'
        SALT(24)='senses RH           actuates mass ratio'
        SALT(25)='senses temp diff          actuates flow'
        SALT(26)='senses abs temp diff      actuates flow'
        SALT(27)='senses pressure           actuates flow'
        SALT(28)='senses press diff         actuates flow'
        SALT(29)='senses abs press diff     actuates flow'
        SALT(30)='senses abs mass flow      actuates flow'
        SALT(31)='senses wind speed         actuates flow'
        SALT(32)='senses wind direction     actuates flow'
        SALT(33)='senses dir nor sol rad    actuates flow'
        SALT(34)='senses dif hor sol rad    actuates flow'
        SALT(35)='senses ext rel hum        actuates flow'
        SALT(36)='senses wind speed         actuates flux'
        SALT(37)='senses wind direction     actuates flux'
        SALT(38)='senses dir nor sol rad    actuates flux'
        SALT(39)='senses dif hor sol rad    actuates flux'
        SALT(40)='senses ext rel hum        actuates flux'
        SALT(41)='senses contaminant conc   actuates flow'
        SALT(42)='senses control loops    actuates either '

        call edisp(iuout,' Current controller type: ')
        call edisp(iuout,SALT(IPCTYP(II,JJ,KK)+1))
        IX=1
        CALL EPICKS(IX,IVALS,' ',' Controller type: (see help)',
     &         40,42,SALT,'controller type',IER,nbhelp)
        if(ix.ne.0)IPCTYP(II,JJ,KK)=IVALS(1)-1
        ictyp=IPCTYP(II,JJ,KK)

C Plant contol law.
        PSALT(1)= 'Period switch off control'
        PSALT(2)= 'P,PI,PID for type 0,2,4,6,8,10 controlr'
        PSALT(3)= 'P,PI,PID for type 1,3,5,7,9,11 controlr'
        PSALT(4)= 'Proportional action for type 12-17 cntr'
        PSALT(5)= 'Optimum start for flux/variable control'
        PSALT(6)= 'Proportional action for type 18-23 cntr'
        PSALT(7)= 'Null controller    '
        PSALT(8)= 'Duty cycle controller'
        PSALT(9)= 'On-off controller'
        PSALT(10)='Multi-sensor on-off controller'
C DG controller
        PSALT(11)='CCHP Cogeneration system controller'

C Following controllers are awaiting GUI support
C        PSALT(12)='Hydrogen demand controller'
C        PSALT(13)='Derive signal from boundary condition'
C        PSALT(14)='Adsorption storage unit controller'

C Following controller indices should be updated once the above get GUI
C support
        PSALT(12)='PI Room Control'
        PSALT(13)='Outside temperature compensation control'

C Following controllers sense one or more (already defined) control
C loops 
         PSALT(14)='Multi-sensor (senses loops)'
         PSALT(17)='Idealised optimum start control'
         PSALT(18)='Idealised optimum stop control'

        write(outs,'(a,a)')' Current control law: ',
     &                      PSALT(IPCLAW(II,JJ,KK))
        call edisp(iuout,outs)
        IX=1
        helptopic='sys_plant_types'
        call gethelptext(helpinsub,helptopic,nbhelp)

 910    IF(IPCTYP(II,JJ,KK).NE.41)THEN
          CALL EPICKS(IX,IPVALS,' ',' Plant control law: ',
     &         40,13,PSALT,'control law',IER,nbhelp)
          if(ix.ne.0)iclaw=IPVALS(1)-1

C Skip controllers (11-13) that are not included in the interface
          IF(ICLAW.GE.11)ICLAW=ICLAW+4

C Multisensor for loops (control type is 
C determined in loops that these sensors sense)
        ELSE
          PSALT(1)='Multi-sensor (senses loops)'
          PSALT(2)='Idealised optimal start control'
          PSALT(3)='Idealised optimal stop control'
          CALL EPICKS(IX,IPVALS,' ',' Plant control law: ',
     &         40,3,PSALT,'control law',IER,nbhelp)
          if(ix.ne.0)then
            if(IPVALS(1).eq.1)then
              iclaw=14
            elseif(IPVALS(1).eq.2)then
              iclaw=17
            elseif(IPVALS(1).eq.3)then
              iclaw=18
            endif
          endif
        ENDIF

C Get supplementary data items based on control laws
        IPCLAW(II,JJ,KK)=iclaw
        IF(IPCTYP(II,JJ,KK).NE.41.AND.IPCLAW(II,JJ,KK).EQ.14)THEN
          helptopic='plant_must_be_multi'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('control type help',nbhelp,'-',0,0,IER)
          IPCTYP(II,JJ,KK)=41
        ENDIF

C Jump if period start time is not required
        IF(ICLAW.LT.14.AND.ICLAW.GT.10)GOTO 63
        IF(ICLAW.EQ.14.OR.ICLAW.GE.17)GOTO 63

C Ask for period start time
  62    helptopic='plant_ctl_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'plant control start',IER,nbhelp)
        if(KK.GT.1)then
          IF(TPCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif

C Make assumptions about the additional data that will be required.
 63     if(IPCLAW(II,JJ,KK).eq.0)then
           
C Period switch off control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Period switch off control has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IPCLAW(II,JJ,KK).eq.1)then

C PID flux controller.
          helptopic='plant_ctl_pid_flux'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' Current PID flux control data is:')
            call edisp(iuout,' Mode, Max Htg/Clg W, Min Htg/Clg W,  
     &       Htg/Clg SP, Htg/Clg TR')
            write(outs,'(5F10.3)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            
            I2=INT(cm(2))
            if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
              I7=INT(cm(7))
              if(I7.eq.1)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(8)
                call edisp(iuout,outs)
                I9=INT(cm(9))
                if(I9.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(10)
                  call edisp(iuout,outs)
                endif
              else
                I8=INT(cm(8))
                if(I8.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(9)
                  call edisp(iuout,outs)
                endif
              endif              
            elseif(I2.eq.3.or.I2.eq.-3)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(7)
                call edisp(iuout,outs)                  
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
               call edisp(iuout,' Int act time [s], Drv act time[s] ')
               write(outs,'(2F9.2)')cm(7),cm(8)
               call edisp(iuout,outs)                  
             endif
          endif

C Define mode, number of items, std items, then mode specific items.
          CALL EASKR(cm(2),' ',' PID mode ? ',
     &      -5.,'F',5.,'F',1.,' PID mode',IER,nbhelp)
          
          CALL EASKR(cm(1),' ',' No. of misc data items ?',
     &      6.,'F',16.,'F',7.,'no. of misc data items',IER,nbhelp)

 11       WRITE(HOLD,'(2f8.0,2f7.2)')cm(3),cm(4),cm(5),cm(6)
          CALL EASKS(HOLD,
     &      ' Max htg/clg cap [W], Min htg/clg cap [W], setpoint,',
     &      ' throtl range for PID ctl: ',70,' 1000. 0. 20. 2. ',
     &      'PID max min stpnt throt',IER,nbhelp)
          K=0
         CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Mx htg/clg',IER)
         CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Min Ht/clg',IER)
         CALL EGETWR(HOLD,K,cm(5),-1001.,100.,'F','Set point',IER)
         CALL EGETWR(HOLD,K,cm(6),0.,999999.,'F','Throt rnge',IER)
          if(ier.ne.0)goto 11

          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
            CALL EASKR(cm(7),' ',' Integral action flag?',
     &        0.,'F',1.,'-',0.,' integral action flag',IER,nbhelp)
            I7=INT(cm(7))
            if(I7.eq.1)then
            CALL EASKR(cm(8),' ',' Integral action time [sec]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
            CALL EASKR(cm(9),' ',' derivative action flag?',
     &        0.,'F',1.,'-',0.,' derivative action flag',IER,nbhelp)
              I9=INT(cm(9))
              if(I9.eq.1)then
                CALL EASKR(cm(10),' ',' Derivative action time [sec]?',
     &            1.,'F',0.,'-',500.,' derivative action time',
     &            IER,nbhelp)
              endif
            else
              CALL EASKR(cm(8),' ',' derivative action flag?',
     &          0.,'F',1.,'-',0.,' derivative action flag',IER,nbhelp)
                I8=INT(cm(8))
                if(I8.eq.1)then
                  CALL EASKR(cm(9),' ',' Derivative action time [sec]?',
     &             1.,'F',0.,'-',500.,' derivative action time',
     &             IER,nbhelp)
                endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
            CALL EASKR(cm(7),' ',' Integral action time [secs]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
          elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
            CALL EASKR(cm(8),' ',' Integral action time [secs]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
            CALL EASKR(cm(11),' ',' Derivative action time [secs]?',
     &        1.,'F',0.,'-',500.,' derivative action time',IER,nbhelp)
          endif

        elseif(IPCLAW(II,JJ,KK).eq.2)then

C PID flow controller.
          helptopic='plant_ctl_pid_flow'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' Current PID flow control data is:')
            call edisp(iuout,' Mode, Max flow [kg/s], Min flow [kg/s],
     &           setpoint, throt rng')
            write(outs,'(5F10.3)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
            
            I2=INT(cm(2))
            if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
              I7=INT(cm(7))
              if(I7.eq.1)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(8)
                call edisp(iuout,outs)
                I9=INT(cm(9))
                if(I9.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(10)
                  call edisp(iuout,outs)
                endif
              else
                I8=INT(cm(8))
                if(I8.eq.1)then
                  call edisp(iuout,' Derivative action time [secs]')
                  write(outs,'(F9.2)')cm(9)
                  call edisp(iuout,outs)
                endif
              endif              
            elseif(I2.eq.3.or.I2.eq.-3)then
                call edisp(iuout,' Integral action time [secs]')
                write(outs,'(F9.2)')cm(7)
                call edisp(iuout,outs)                  
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
               call edisp(iuout,' Int act time [s], Drv act time[s] ')
               write(outs,'(2F9.2)')cm(7),cm(8)
            endif
          endif

C Define mode, number of items, std items, then mode specific items.
          CALL EASKR(cm(2),' ',' PID mode? ',
     &      -5.,'F',5.,'F',1.,' PID mode',IER,nbhelp)
          
          CALL EASKR(cm(1),' ',' No. of misc data items ?',
     &      6.,'F',16.,'F',7.,'no. of misc data items',IER,nbhelp)

 21      WRITE(HOLD,'(2f8.0,2f7.2)')cm(3),cm(4),cm(5),cm(6)
          CALL EASKS(HOLD,
     &      ' Max flow [kg/s], Min flow [kg/s], setpoint,',
     &      ' throtl range for PID ctl: ',70,' 0.01 0.0001 20. 2.',
     &      'PID max min stpnt throt',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(3),0.,100.,'F','Max flow',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,100.,'F','Min flow',IER)
          CALL EGETWR(HOLD,K,cm(5),-1001.,100.,'F','Set pt',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,100.,'F','Throt rnge',IER)
          if(ier.ne.0)goto 21

          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
            CALL EASKR(cm(7),' ',' Integral action flag?',
     &        0.,'F',1.,'-',0.,' integral action flag',IER,nbhelp)
            if(INT(cm(7)).eq.1)then
            CALL EASKR(cm(8),' ',' Integral action time [seconds]?',
     &        1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
            CALL EASKR(cm(9),' ',' derivative action flag?',
     &        0.,'F',1.,'-',0.,' derivative action flag',IER,nbhelp)
              if(INT(cm(9)).eq.1)then
                CALL EASKR(cm(10),' ',' Derivative action time [sec]?',
     &            1.,'F',0.,'-',500.,' derivative action time',
     &            IER,nbhelp)
              endif
            else
              CALL EASKR(cm(8),' ',' derivative action flag?',
     &          0.,'F',1.,'-',0.,' derivative action flag',IER,nbhelp)
                if(INT(cm(8)).eq.1)then
                  CALL EASKR(cm(9),' ',' Derivative action time [sec]?',
     &             1.,'F',0.,'-',500.,' derivative action time',
     &             IER,nbhelp)
                endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
              CALL EASKR(cm(7),' ',' Integral action time [seconds]?',
     &          1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
            elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
              CALL EASKR(cm(8),' ',' Integral action time [seconds]?',
     &          1.,'F',0.,'-',900.,' integral action time',IER,nbhelp)
              CALL EASKR(cm(11),' ',' Derivative action time [secs]?',
     &          1.,'F',0.,'-',500.,' derivative action time',
     &          IER,nbhelp)
            endif

          elseif(IPCLAW(II,JJ,KK).eq.3)then

C Numerical proportional controller.
          helptopic='plant_ctl_numeric_prop'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current numerical proportional controller data is:')
            call edisp(iuout,
     &      ' Max & min o/p, upper & lower setpoints, hysteresis ')
            write(outs,'(5F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
            call edisp(iuout,outs)
          endif
          cm(1)=5.0

  31      WRITE(HOLD,'(5f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
          CALL EASKS(HOLD,
     &    ' Max output, min output, upper setpoint, lower setpoint, ',
     &    ' output to overcome hysteresis',70,
     &      ' 2. 1. 2. 1. 0.1 ','num`l prop`l ctlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Max ouput',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Min output',IER)
          CALL EGETWR(HOLD,K,cm(4),-100.,100.,'F','Up stpt',IER)
          CALL EGETWR(HOLD,K,cm(5),-100.,100.,'F','Low stpt',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,100.,'F','Hystersis',IER)
          if(ier.ne.0)goto 31

        elseif(IPCLAW(II,JJ,KK).eq.4)then

C Optimum start controller.
          helptopic='plant_ctl_opti_start'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current optimum start controller data is:')
            call edisp(iuout,
     &      ' Max o/p, dtoa, desd temp, 1st B&J coft, 2nd B&J, 3rd B&J')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
          endif
          cm(1)=6.0

  41      WRITE(HOLD,'(6f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
          CALL EASKS(HOLD,
     &      ' Max output, desired time of arrival, desired temp,',
     &      ' 1st B&J coeft, 2nd B&J coeft, 3rd B&J coeft ',70,
     &      ' 1000.0 9.0 20. 1. 1. 1. ','optimum start ctlr',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Max output',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,24.,'F','Dsrd tm of Arv',IER)
          CALL EGETWR(HOLD,K,cm(4),-100.,100.,'F','Desrd temp',IER)
          CALL EGETWR(HOLD,K,cm(5),-100.,100.,'F','1st B&J cf',IER)
          CALL EGETWR(HOLD,K,cm(6),-100.,100.,'F','2nd B&J cf',IER)
          CALL EGETWR(HOLD,K,cm(7),-100.,100.,'F','3rd B&J cf',IER)
          if(ier.ne.0)goto 41

        elseif(IPCLAW(II,JJ,KK).eq.5)then

C Proportional damper controller.
          helptopic='plant_ctl_prop_damper'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current proportional damper controller data is:')
            call edisp(iuout,
     &      ' Max & min dpr pos, flow at max & min, 1st & 2nd conects ')
            write(outs,'(6F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
            call edisp(iuout,outs)
          endif
          cm(1)=6.0

  51      WRITE(HOLD,'(6f7.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7)
          CALL EASKS(HOLD,
     &    ' Max damp pos, Min damp pos, flow at max pos, flow at',
     &    ' min pos, 1st connection number, 2nd connection number',70,
     &      ' 1. 0.  ','optimum start ctlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,1.,'F','Max dmpr pos',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'F','Min dmpr pos',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,100.,'F','Flo at max',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,100.,'F','Flo at min',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,75.,'F','1st con',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,75.,'F','2nd con',IER)
          if(ier.ne.0)goto 51

        elseif(IPCLAW(II,JJ,KK).eq.6)then
           
C Null controller.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Null controller has no data.')
            call edisp(iuout,' ')
          endif
          cm(1)=0.0
        elseif(IPCLAW(II,JJ,KK).eq.7)then

C Duty Cycle controller.
          helptopic='plant_ctl_duty_cycle'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current duty cycle controller data is:')
            call edisp(iuout,' Dty cyc prd,  tim stps off, 1st off ts ')
            write(outs,'(3F9.2)')cm(2),cm(3),cm(4)
            call edisp(iuout,outs)
          endif
          cm(1)=3.0
 
  71      WRITE(HOLD,'(3f7.2)')cm(2),cm(3),cm(4)
          CALL EASKS(HOLD,
     &      ' Duty cycle prd, times steps off, 1st off time-step',
     &      'in cycle period',70,' 4. 1. 1. ','Duty cycle ctlr',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1440.,'F','Cycle perd',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1440.,'F','off t-stps',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1440.,'F','1st off ts',IER)
          if(ier.ne.0)goto 71

        elseif(IPCLAW(II,JJ,KK).eq.8)then

C On-off controller.
          helptopic='plant_ctl_on_off'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current on-off controller data is:')
            call edisp(iuout,' Mode, on setpt, off setpt, o/p at hi,
     &                         o/p at lo, sen lag, act lag')
            write(outs,'(7F10.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                           cm(8)
            call edisp(iuout,outs)
          endif
          cm(1)=7.0

  81      WRITE(HOLD,'(7f9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                            cm(8)
          CALL EASKS(HOLD,
     &    'Mode of operation, on set point, off set point, o/p at hi',
     &    ' o/p at lo, sen lag, act lag ',70,' 1. 2. 1. 1. 1. 1. 1.',
     &    'on-off ctl',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),-999.,999.,'W','Mode',IER)
          CALL EGETWR(HOLD,K,cm(3),-100.,100.,'W','on set pnt',IER)
          CALL EGETWR(HOLD,K,cm(4),-100.,100.,'W','off set pnt',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,999999.,'W','output @ hi',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,999999.,'W','output @ lo',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,1440.,'W','sensor lag',IER)
          CALL EGETWR(HOLD,K,cm(8),0.,1440.,'W','actu lag',IER)

          if(ier.ne.0)goto 81

        elseif(IPCLAW(II,JJ,KK).eq.9)then

C Multi-sensor on-off controller.
          helptopic='plant_multi_on_off'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Current multi-sen on-off ctlr data is:')
            call edisp(iuout,'O/p at hi, o/p at lo, Mode, No. axy sens, 
     &                        Auxy fnc, sen lag, act lag')
            write(outs,'(7F9.2)')cm(2),cm(3),cm(4),cm(5),cm(6),cm(7),
     &                           cm(8)
            call edisp(iuout,outs)

            I4=INT(cm(4))
            DO 91 L=1,I4             
              ndx=8+4*(L-1)
              call edisp(iuout,' Auxliary Sensor Details')              
              write(outs,'(4F9.2)')cm(ndx),cm(ndx+1),cm(ndx+2),cm(ndx+3)
              call edisp(iuout,outs)
              if(INT(cm(5)).eq.4)then
                ndx=12+(L-1)+4*(I4-1)         
                write(outs,'(a,F9.2)')' Weighting for sensor ',cm(ndx)
                call edisp(iuout,outs)
              endif
 91        continue   
          endif

          CALL EASKR(cm(1),' ',' No. of misc data items ',
     &      10.,'F',16.,'F',10.,'no. of misc data items',IER,nbhelp)

          CALL EASKR(cm(2),' ',' Output at `hi` status ? ',
     &      0.,'-',0.,'-',1.,'output at `hi` status',IER,nbhelp)
          CALL EASKR(cm(3),' ',' Output at `lo` status ? ',
     &      0.,'-',0.,'-',1.,'output at `lo` status',IER,nbhelp)

          CALL EASKR(cm(4),' ',' Output mode ? ',
     &      -1.,'F',1.,'F',1.,' output mode',IER,nbhelp)

          CALL EASKR(cm(5),' ',' No of auxiliary sensors ? ',
     &      1.,'F',2.,'F',1.,'no. of auxiliary sensors',IER,nbhelp)

          CALL EASKR(cm(6),' ',' Multi-sensor auxy function options?',
     &      1.,'F',4.,'F',1.,' multi-sensor aux func',IER,nbhelp)

          CALL EASKR(cm(7),' ',' Sensor lag? ',
     &      1.,'F',4.,'F',0.,' sensor lag',IER,nbhelp)

          CALL EASKR(cm(8),' ',' Actuator lag ? ',
     &      1.,'F',4.,'F',0.,' actuator lag',IER,nbhelp)

C Auxiliary sensor details,
          I4=INT(cm(4))  
          DO 93 L=1,I4
            ndx=8+4*(L-1)
            CALL EASKR(cm(ndx),' ',' Aux sen detail 1st item ? ',
     &        -3.,'F',0.,'-',1.,' Aux sen detail 1st item',IER,nbhelp)
            CALL EASKR(cm(ndx+1),' ',' Aux sen detail 2nd item ? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 2nd item',IER,nbhelp)          
            CALL EASKR(cm(ndx+2),' ',' Aux sen detail 3rd item ? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 3rd item',IER,nbhelp)          
            CALL EASKR(cm(ndx+3),' ',
     &        ' Aux sen detail 4th item (always use zero)? ',
     &        0.,'F',0.,'-',0.,' Aux sen detail 4th item',IER,nbhelp)
 93       CONTINUE    
          
          I5=INT(cm(5))
          if(I5.eq.4)then
            DO 95 L=1,I5
C Weighted auxiliary sensors mode,
              ndx=12+(L-1)+4*(I4-1)         
              CALL EASKR(cm(ndx),' ',' Aux sensor weighting ? ',
     &          0.,'F',100.,'F',50.,' Aux sensor weighting',
     &          IER,nbhelp)
 95         continue
          endif
        elseif(IPCLAW(II,JJ,KK).eq.10)then

          CALL DG_controller_menu(cm)

        elseif(IPCLAW(II,JJ,KK).GE.11.AND.IPCLAW(II,JJ,KK).LE.13)then

C Controllers not supported through the interface yet
          GOTO 910
        elseif(IPCLAW(II,JJ,KK).eq.14)then

C Multi-sensor of control loops
          helptopic='plant_multi_loops'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKI(NCLP,' ',' No. of control loops to sense ',
     &      2,'-',4,'-',2,'no. of misc data items',IER,nbhelp)
          cm(1)=real(NCLP*2+3)
          DO 1454 ICLP=1,NCLP
            CALL EASKI(ICLN,' ',' Select control loops number',
     &      1,'-',mcF,'-',1,'ctl loop number',IER,nbhelp)
            CM(ICLP*2)=ICLN
            IF(ICLP.NE.NCLP)THEN
              CALL EASKAB('Logical relationship with next loop',' ',
     &        '.OR.','.AND.',iad,nbhelp)
              CM(ICLP*2+1)=IAD
            ENDIF
 1454     CONTINUE
          TONV=CM(NCLP*2+1)
          CALL EASKR(TONV,' ',' Signal value at ON ',
     &        1.,'-',1.,'-',1.,'signal on value',IER,nbhelp)
          CM(NCLP*2+1)=TONV              

          TONV=CM(NCLP*2+2)
          CALL EASKR(TONV,' ',' Signal value at OFF ',
     &        1.,'-',1.,'-',1.,'signal off value',IER,nbhelp)
          CM(NCLP*2+2)=TONV              

          TONV=CM(NCLP*2+3)
          CALL EASKI(ICDAT,' ','Which control item to set at ON'
     &        ,1,'-',1,'-',1,'CDATA for component',IER,nbhelp)
          CM(NCLP*2+3)=REAL(ICDAT)

          TONV=CM(NCLP*2+4)
          CALL EASKR(TONV,' ',' Overrun time (min)',
     &        1.,'-',1.,'-',1.,' Overrun time (min)',IER,nbhelp)
          CM(NCLP*2+4)=TONV
        elseif(IPCLAW(II,JJ,KK).eq.15)then

C Proportional Integral Room controller
          helptopic='plant_wch_simple_PI'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CM(1)=10.0
          call edisp(iuout,' ')
          call edisp(iuout,' current control parameters are:')
          call edisp(iuout,
     &      ' nominal, max, min, prop band, BDATA, reset time')
 101      WRITE(HOLD,'(8(F5.2,1X))')CM(2),CM(3),CM(4),CM(5),CM(6),CM(7)
     &       ,CM(8),CM(9)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,
     &    ' nominal, max, min, prop band, BDATA, reset time (sec)',
     &    'optional control loop, static value (see help)',
     &    70,HOLD,'PI room ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1.,'-','Setpoint nominal ',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1.,'-','Setpoint max',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1.,'-','Setpoint min',IER)
          CALL EGETWR(HOLD,K,cm(5),1.E-6,1.E6,'F','Prop band',IER)
          CALL EGETWR(HOLD,K,cm(6),1.,1.,'-','BDATA item #',IER)
          CALL EGETWR(HOLD,K,cm(7),1.E-6,1.E6,'F','Int reset time',IER)
          CALL EGETWR(HOLD,K,cm(8),1.,1.,'-','optional cntrl loop',IER)
          CALL EGETWR(HOLD,K,cm(9),1.,1.,'-','BDATA static value',IER)
          if(ier.ne.0)goto 101
 102      write(HOLD,'(2(F5.2,1X))')CM(10),CM(11)
          CALL EASKS(HOLD,
     &    'Optional flow control connection numbers',
     &    'connection no.1 , connection no. 2 (see help)',
     &    70,HOLD,'PI room ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(10),1.,1.,'-','connection no. 1 ',IER)
          CALL EGETWR(HOLD,K,cm(11),1.,1.,'-','connection no. 2 ',IER)
          if(ier.ne.0)goto 102
        elseif(IPCLAW(II,JJ,KK).eq.16)then

C Outside temperature compensation controller
          helptopic='plant_outside_comp'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=12.
          call edisp(iuout,' ')
          call edisp(iuout,' current control parameters are:')
          call edisp(iuout,'alpha, beta, gamma')
  111     write(HOLD,'(3(F5.2,1X))')cm(2),cm(3),cm(4)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,'alpha, beta, gamma',
     &    '(see help)',70,HOLD,'OTC ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1.,'-','alpha ',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1.,'-','beta',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1.,'-','gamma',IER)
          call edisp(iuout,' further control parameters are:')
          call edisp(iuout,'nominal1, nominal, max, min, BDATA')
          IF(IER.NE.0)GOTO 111
  121     WRITE(HOLD,'(7(F5.2,1X))')CM(5),CM(6),CM(7),CM(8),
     &    CM(9),CM(10),CM(11)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,'nominal1, nominal, max, min, BDATA',
     &      'optional control loop, static value (see help)',
     &      70,HOLD,'OTC ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(5),1.,1.,'-','nominal1',IER)
          CALL EGETWR(HOLD,K,cm(6),1.,1.,'-','nominal',IER)
          CALL EGETWR(HOLD,K,cm(7),1.,1.,'-','max',IER)
          CALL EGETWR(HOLD,K,cm(8),1.,1.,'-','min',IER)
          CALL EGETWR(HOLD,K,cm(9),1.,1.,'-','BDATA item #',IER)
          CALL EGETWR(HOLD,K,cm(10),1.,1.,'-','optional cntrl loop',IER)
          CALL EGETWR(HOLD,K,cm(11),1.,1.,'-','BDATA static value',IER)
          IF(IER.NE.0)GOTO 121
 131      write(HOLD,'(2(F5.2,1X))')CM(10),CM(11)
          CALL EASKS(HOLD,
     &    'Optional flow control connection numbers',
     &    'connection no.1 , connection no. 2 (see help)',
     &    70,HOLD,'PI room ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(12),1.,1.,'-','connection no. 1 ',IER)
          CALL EGETWR(HOLD,K,cm(13),1.,1.,'-','connection no. 2 ',IER)
          if(ier.ne.0)goto 131

C Idealised optimum start controller
        elseif(IPCLAW(II,JJ,KK).eq.17)then
          helptopic='ideal_optimal_start'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nclp=nint(cm(1))-2
          write(HOLD,'(a,i3,a)')' currently sensing ',nclp,' loops'
          call edisp(iuout,HOLD)
          CALL EASKI(NCLP,' ',' No. of control loops to sense ',
     &      2,'-',4,'-',2,'no. of misc data items',IER,nbhelp)
          cm(1)=real(NCLP+3)
          call edisp(iuout,' ')
          call edisp(iuout,'current control parameters are:')
          call edisp(iuout,' sensed control loop(s)')
          do 1115 iclp=1,nclp
            write(HOLD,'(a,i3)')' control loop number ',nint(cm(1+ICLP))
            call edisp(iuout,HOLD)
 1115     continue
          write(hold,'(a,f5.2)')' period number to optimise ',cm(nclp+2)
          call edisp(iuout,HOLD)
          write(hold,'(a,f5.2)')' space comes up to temperature at ',
     &    cm(nclp+3)
          CALL EDISP(IUOUT,HOLD)
 112      DO 1114 iclp=1,NCLP
            write(hold,'(f5.2)')cm(iclp+1)
            CALL EASKS(HOLD,'Loop number to sense',
     &      '(see help)',70,HOLD,'ideal optimum start ctrlr',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,cm(1+ICLP),1.,1.,'-','loop no.',IER)
 1114     CONTINUE
          write(hold,'(2f5.2)')cm(nclp+2),cm(nclp+3)
          CALL EASKS(HOLD,
     &    'period no. to optimise, time to come up to temperature',
     &    'overriding loop number if any (zero otherwise) (see help)',
     &    70,HOLD,'ideal optimum start ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(NCLP+2),1.,1.,'-','period no.',IER)
          CALL EGETWR(HOLD,K,cm(NCLP+3),1.,1.,'-','time',IER)
          CALL EGETWR(HOLD,K,cm(NCLP+4),1.,1.,'-','overriding loop',IER)
          IF(IER.NE.0)GOTO 112

C Get sensor and actuator detail from loop that is to be optimised
          DO 113 IDIN=1,4 ! Skip 5th sensor item for now
            IPAN(II,IDIN)=IPAN(IOTHER,IDIN)
            IPSN(II,IDIN)=IPSN(IOTHER,IDIN)
 113      CONTINUE

C Idealised optimum stop controller
        elseif(IPCLAW(II,JJ,KK).eq.18)then
          helptopic='ideal_optimal_stop'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nclp=nint(cm(1))-2
          write(HOLD,'(a,i3,a)')' currently sensing ',nclp,' loops'
          call edisp(iuout,HOLD)
          CALL EASKI(NCLP,' ',' No. of control loops to sense ',
     &      2,'-',4,'-',2,'no. of misc data items',IER,nbhelp)
          cm(1)=real(NCLP+3)
          call edisp(iuout,' ')
          call edisp(iuout,'current control parameters are:')
          call edisp(iuout,' sensed control loop(s)')
          do 2115 iclp=1,nclp
            write(HOLD,'(a,i3)')' control loop number ',nint(cm(1+ICLP))
            call edisp(iuout,HOLD)
 2115     continue
          write(hold,'(a,f5.2)')' period number to optimise ',cm(nclp+2)
          call edisp(iuout,HOLD)
          write(hold,'(a,f5.2)')
     &   ' acceptable temperature change at the end of this period',
     &   cm(nclp+3)
          CALL EDISP(IUOUT,HOLD)
 212      DO 2114 iclp=1,NCLP
            write(hold,'(f5.2)')cm(iclp+1)
            CALL EASKS(HOLD,'Loop number to sense',
     &      '(see help)',70,HOLD,'ideal optimum start ctrlr',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,cm(1+ICLP),1.,1.,'-','loop no.',IER)
 2114     CONTINUE
          write(hold,'(2f5.2)')cm(nclp+2),cm(nclp+3)
          CALL EASKS(HOLD,
     &    'period no. to optimise, acceptable temperature revision at ',
     &    'the end of this period, overriding loop (zero if absent)',
     &    70,HOLD,'ideal optimum stop ctrlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(NCLP+2),1.,1.,'-','period no.',IER)
          CALL EGETWR(HOLD,K,cm(NCLP+3),1.,1.,'-','time',IER)
          CALL EGETWR(HOLD,K,cm(NCLP+4),1.,1.,'-','time',IER)
          IF(IER.NE.0)GOTO 212

C Get sensor and actuator detail from loop that is to be optimised
          DO 213 IDIN=1,4 ! Skip 5th sensor item for now
            IPAN(II,IDIN)=IPAN(IOTHER,IDIN)
            IPSN(II,IDIN)=IPSN(IOTHER,IDIN)
 213      CONTINUE
        endif

C Extract from working array to pmiscd() common structure
        call extrctl(1,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      RETURN
      END

C *********** EFCNTRL
C Edit flow control function II.
      SUBROUTINE EFCNTRL(II,IV,IER)
#include "building.h"
#include "net_flow.h"
#include "epara.h"
#include "control.h"      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)

      DIMENSION ICPK(MCNN),INPK(MNOD)
      CHARACTER*84 VERT(36)
      CHARACTER outs*124,SSTR*96,TMP*96,KEY*1,NDNAM*12
      character outsa*84,outsb*84,outsc*84,hold*42,LTMP*248
      character prmpt1*72,prmpt2*72

C Portions of multi-sensor feedback message.
      character msg1*24,msg2*24,msg3*24,msg4*24
      logical close1,OK,DOK
      real ocli
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EFCNTRL'  ! set for subroutine

C Number of functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J

C Initialise menu size variables based on windows size.
C IVERT is the menu position, MVERT the current
C number of menu lines.
 92   MHEAD=5
      MCTL=4
      ILEN=NFCDP(II,JJ)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NFCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      ILEN=NFCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=MHEAD
      DO 91 KP=1,ILEN
        IF(KP.GE.IST.AND.(KP.LE.(IST+MIFULL)))THEN
          KK=KP
          m=m+1
          CALL EMKEY(KP,KEY,IER)
          call stfctl(2,II,JJ,KK)
          CALL EVCNTRL(2,II,JJ,KK,'T',SSTR)
          CALL EVCNTRLAW(2,II,JJ,KK,TMP,LTMP)
          NITEMS=INT(cm(1))
          WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
          outsb=' '
          outsc=' '
          if(NITEMS.gt.0)then
            if(NITEMS.LE.9)then
              write(outsb,'(9F9.2)')(cm(L),L=2,NITEMS+1)
            else
              write(outsb,'(9F9.2)')(cm(L),L=2,10)
            endif
            call SDELIM(outsb,outsc,'S',IW)
          endif
          write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &      outsa(1:47),outsc(1:34)
        endif
  91  continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' --')
      ENDIF
      WRITE(vert(m+2),'(a)') '* add/ delete a period '
      WRITE(vert(m+3),'(a)') '? help '
      WRITE(vert(m+4),'(a)') '- exit  '

C Help text for this menu.
      helptopic='flow_ctl_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        helptopic='flow_ctl_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then

C Add or delete a period.
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(2,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(2,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        KK=IFOC
        call stfctl(2,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data...'
          call edisp(iuout,outs)
          if(NITEMS.LE.12)then
            write(outs,'(12F10.2)')(cm(L),L=2,NITEMS+1)
          else
            write(outs,'(12F10.2)')(cm(L),L=2,12)
          endif
          call SDELIM(outs,outsc,'S',IW)
          call edisp(iuout,outsc)
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        call edisp(iuout,' ')

C Flow controller.  Set type to same as initial for this flow
C function (ie. all periods are the same for flow).
        ictyp=IFCTYP(II,1,1)
        IFCTYP(II,JJ,KK)=ictyp
        write(outs,'(a,i3)')' Current controller type: ',ictyp
        call edisp(iuout,outs)
        call edisp(iuout,'Note: to change this re-edit the flow sensor')

C What is the equivalent for flow.....
        ino=iclaw+1
        ilno=ino
        idno=1
        write(outs,'(a,i2,a)')'Select a control type (currently: ',
     &    iclaw+1,')'
        call MENUATOL(outs,' Available laws','a on/off',
     &   'b proportional with hysteresis',
     &   'c range based (for 30 35 40 110 130)',
     &   'd multi-sensor on/off',
     &   'e human behaviour algorithm ver 1.0',
     &   'f proportional and integral ',' ',' ',' ',' ',' '
     &   ,' ',ino,idno,nbhelp)
        if(ino.eq.0)then
          continue
        elseif(ilno.ne.ino)then
          iclaw=ino-1
          IFCLAW(II,JJ,KK)=iclaw
        endif

  62    CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,nbhelp)
        if(KK.GT.1)then
          IF(TFCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0','W')
            GOTO 62
          endif
        endif
        TFCPS(II,JJ,KK)=tcps

        if(iclaw.eq.0)then

C Miscellaneous data for on / off controller.
          helptopic='flow_ctl_on_off'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=3.
  53      WRITE(HOLD,'(3f11.5)')cm(2),cm(3),cm(4)
          CALL EASKS(HOLD,
     &    ' On/Off setpoint, action (1=on above setpoint, -1=on below)',
     &      ' and fraction ON (see help):',42,' 30. 1. 1. ',
     &      ' flow on/off controller',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','set pt',IER)
          CALL EGETWR(HOLD,K,cm(3),-1.,1.,'F','action',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,1.,'F','fraction',IER)
          if(int(cm(3)).eq.0)goto 53
          if(ier.ne.0)goto 53
        elseif(iclaw.eq.1)then

C Miscellaneous data for proportional controller.
          helptopic='flow_ctl_prop_hyster'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=5.
  54      WRITE(HOLD,'(5f8.2)')cm(2),cm(3),cm(4),cm(5),cm(6)
          CALL EASKS(HOLD,
     &   ' Proportional: signal lower lmt, lower valve psn (%), signal',
     &   ' upper lmt, upper valve psn (%), hysteresis: ',42,
     &      ' 10.  0.  20. 100.  1.',' flow proportional controller',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','signal low',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,100.,'F','valve psn l',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','signal high',IER)
          CALL EGETWR(HOLD,K,cm(5),0.,100.,'F','valve psn h',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,9999.,'F','ds for hist',IER)
          if(ier.ne.0)goto 54
        elseif(iclaw.eq.2)then

C Miscellaneous data for range based controller.
          helptopic='flow_ctl_range_based'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=6.
  55      WRITE(HOLD,'(3f10.4)')cm(2),cm(3),cm(4)
          CALL EASKS(HOLD,
     &      ' Range setpoints: low, mid, high (see help)',
     &      ' : ',42,' 0. 10. 20. ',' flow range controller',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','low set pt',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','mid set pt',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','high set pt',IER)
          if(ier.ne.0)goto 55
 157      WRITE(HOLD,'(3f9.4)')cm(5),cm(6),cm(7)
          CALL EASKS(HOLD,
     &      ' Ratios of rate or area for: low, mid, high (see help)',
     &      ' : ',42,' 0.1 1.0 2.0 ',' flow range controller',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(5),0.,0.,'-','low ratio',IER)
          CALL EGETWR(HOLD,K,cm(6),0.,0.,'-','mid ratio',IER)
          CALL EGETWR(HOLD,K,cm(7),0.,0.,'-','high ratio',IER)
          if(ier.ne.0)goto 157

        elseif(iclaw.eq.3)then

C Miscellaneous data for multi sensor flow controller.
          helptopic='flow_ctl_multi_sensing'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=2.
  56      WRITE(HOLD,'(2f8.2)')cm(2),cm(3)

C<< To include feed back to text feedback area using USRMSG
          WRITE(OUTS,'(A,I4,A)')'number of sensors (max ',MSEN,' )?' 
          NSEN=INT(CM(3))
          CALL EASKI(NSEN,' ',OUTS,
     &      0,'F',MSEN,'-',1,'no of sensors',IERI,nbhelp)
          if(ieri.eq.-3) goto 56
          if(ieri.ne.0)goto 56
          cm(3)=real(NSEN)
          call edisp(iuout,' Ideal multi-sensor controller:')
          cm(1)=2.0+8.0*NSEN
          OK=.true.
          IF(NSEN.EQ.1)
     &    CALL ASKOK('You have chosen just one sensor, the only option',
     &    'for mulitsensing is the NOT/SLAVE option. Is this correct?'
     &    ,OK,dok,nbhelp)
          IF(.NOT.OK)THEN
            GOTO 56
          ELSEIF(NSEN.EQ.1)THEN
            INOT=1
            CALL EASKAB('Choose whether NOT or SLAVE',' ','NOT','SLAVE'
     &      ,INOT,nbhelp)
          ENDIF
          IF(NSEN.EQ.1)GOTO 69
 
C Default position: 1=open,0=closed
          CALL EASKAB('Default position? ',' ','closed','open',
     &      IW,nbhelp)
          cm(2)=IW-1

C Auxiliary sensor details,
          DO 358 ISEN=1,NSEN
            ndx=4+8*(ISEN-1)
            WRITE(OUTS,'(A,I3)')'What to sense for sensor number ',ISEN
            CALL EASKAB(OUTS,' ','Climate variable','Node temperature'
     &      ,IW,nbhelp)
            IF(IW.EQ.1)THEN
              CM(NDX)=-3.
              idno=1
              call MENUATOL(' ',' Climate variables','a temperature',
     &        'b wind speed','c wind direction',
     &        'd diffuse radiation','e direct radiation ',
     &        'e relative humidity ',' ',' ',' ',' ',' ',' ',
     &        ino,idno,nbhelp)
              IF(INO.EQ.1)THEN
                CM(NDX+1)=0.0
                msg1=' senses ambient T'
              elseif(INO.eq.2)then
                CM(NDX+1)= real(INO)
                msg1=' senses wind speed'
              elseif(INO.eq.3)then
                CM(NDX+1)= real(INO)
                msg1=' senses wind direction'
              elseif(INO.eq.4)then
                CM(NDX+1)= real(INO)
                msg1=' senses diffuse solar'
              elseif(INO.eq.5)then
                CM(NDX+1)= real(INO)
                msg1=' senses direct solar'
              elseif(INO.eq.6)then
                CM(NDX+1)= real(INO)
                msg1=' senses RH'
              ENDIF
            ELSEIF(IW.EQ.2)THEN
              CM(NDX)=-4.0
              call ASKRNOD('  sensed flow node','-',INOD,IER)
              CM(NDX+1)=INOD
              write(msg1,'(2a)') ' senses nd ',ndnam(inod)
            ENDIF
            CM(NDX+2)=0.0
            CM(NDX+3)=0.0
            CALL EASKR(cm(ndx+4),' ',' Aux sensor set-point ? ',
     &        -100.,'F',0.,'-',0.,' Aux sensor set-point',IER,nbhelp)
            write(msg2,'(a,f5.2)') ' setpoint ',cm(ndx+4)
            CALL EASKAB('Action Flag (see help)',' ','Direct',
     &      'Inverse',IW,nbhelp)
            IF(IW.EQ.1)THEN
              cm(ndx+5)=1.0
            ELSEIF(IW.EQ.2)THEN
              cm(ndx+5)=-1.0
            ENDIF
            if(int(cm(ndx+5)).eq.-1)msg3=' inverse action'
            if(int(cm(ndx+5)).eq.1) msg3=' direct action'

C<< To implement "fraction on" in a sensible way (setting it to diffent)
C values for different sensors may cause ambiguity if differnet sensors
C require a connection to open to different fractions
C            CALL EASKR(cm(ndx+6),' ',' Aux sensor "fraction ON"? ',
C     &        0.,'F',1.,'F',1.,' Aux sensor set-point',IER,nbhelp)
            cm(ndx+6)=0.
            IF(ISEN.LT.NSEN.OR.ISEN.EQ.1)
     &      CALL EASKABC('Logical action variable',
     &      'with next sensor','AND','OR','NOT',iad,nbhelp)
            IF(IAD.EQ.1)THEN
              CM(NDX+7)=1.0
              msg4=' AND'
            ELSEIF(IAD.EQ.2)THEN
              CM(NDX+7)=2.0
              msg4=' OR'
            ELSEIF(IAD.EQ.3)THEN
              CM(NDX+7)=3.0
              msg4=' NOT'
            ENDIF

C Echo back to the user what they just put in.
            write(outs,'(a,i1,4a)') 'For sensor ',isen,
     &        msg1(1:lnblnk(msg1)),msg2(1:lnblnk(msg2)),
     &        msg3(1:lnblnk(msg3)),msg4(1:lnblnk(msg4))
            call edisp(iuout,outs)
 358      CONTINUE
          GOTO 70
 
C For the NOT option allow to read in sensed flow connection
  69      INODE=0
          NNPK=1
          prmpt1='Choose connection'
          prmpt2=' '
          CALL ASKMFCON(INODE,NNPK,ICPK,prmpt1,prmpt2)
          IF(INOT.EQ.2)THEN
            cm(6)=-1.*real(icpk(1))
          ELSE
            cm(6)=real(icpk(1))
          ENDIF

        elseif(iclaw.eq.4)then

C Adaptive behavioural model as detailed in Comfort Driven Adaptive
C Window Opening Behaviour and the Influence of Building Design. Tuohy,
C P., Rijal, H. B., Humphreys, M. A., Nicol, J. F., Samuel, A. A.,
C Clarke, J. A. Proc. BS 2007. 10th IBPSA Conference and Exhibition.
          helptopic='flow_ctl_adaptive'
          call gethelptext(helpinsub,helptopic,nbhelp)
          cm(1)=13
          cm(2)=0.8
          CALL EASKR(cm(2),' ',' Running mean response parameter ? ',
     &      0.,'F',0.99,'F',0.8,'Running mean response parameter',
     &      IER,nbhelp)

C Uncomment following code for version 2 of algorithm
          CALL EASKR(cm(3),'  ','Fraction window openable in day ? ',
     &      0.,'F',1.0,'F',1.0,' Wposs',IER,nbhelp)
          CALL EASKR(cm(4),' ','Fraction fan available for day use?',
     &      0.,'F',1.0,'F',1.0,' Fposs',IER,nbhelp)
          CALL EASKR(cm(5),' ','Fraction window openable in night ? ',
     &      0.,'F',1.0,'F',1.0,' WNposs',IER,nbhelp)
          CALL EASKR(cm(6),' ','Fraction AC available for day use?',
     &      0.,'F',1.0,'F',1.0,' ACavail',IER,nbhelp)
          CALL EASKR(cm(7),' ','Fraction heating available ?',
     &      0.,'F',1.0,'F',1.0,' Havail',IER,nbhelp)
          CALL EASKR(cm(8),' ','Fraction daylight available?',
     &      0.,'F',1.0,'F',1.0,' Dposs',IER,nbhelp)
          CALL EASKR(cm(9),' ','Power rating of the fan W/m2?',
     &      0.,'F',100.,'F',10.0,' PWRfan',IER,nbhlep)
          CALL EASKR(cm(10),' ','Power rating of the lights W/m2?',
     &      0.,'F',100.,'F',10.0,' PWRlight',IER,nbhelp)
          CALL EASKR(cm(11),' ','Fan comfort temperature adjustment?',
     &      0.,'F',4.0,'F',2.0,' DTfan',IER,nbhelp)
          CALL EASKR(cm(12),' ','Deadband for window open?',
     &      0.,'F',8.0,'F',4.0,' WD',IER,nbhelp)
          CALL EASKR(cm(13),' ','Deadband for fan on?',
     &      0.,'F',8.0,'F',2.0,' FD',IER,nbhelp)
          CALL EASKR(cm(14),' ','Air intake surface ID?',
     &      0.,'F',20.,'F',1.0,' AIsurf',IER,nbhelp)

C Ask for node associated with operative temperature
          prmpt1='Which node is to be used to represent the '
          prmpt2='operative temperature?'
          call edisp(iuout,prmpt1)
          call edisp(iuout,prmpt2)
          INOPT=1
          NNPK=1
 545      call ASKMFNOD(INOPT,NNPK,INPK,prmpt1,prmpt2,nbhelp)
          IIN=0
          IF(NDTYP(INPK(1)).GT.2)THEN
            IIN=1
            CALL PHELPD('node selection help',nbhelp,'-',0,0,IER)
          ENDIF
          IF(IIN.EQ.1)GOTO 545
          IFSN(II,1)=-3
          IFSN(II,2)=7
          IFSN(II,3)=INPK(1)

        elseif(iclaw.eq.5)then

C PI controller
          helptopic='flow_ctl_adaptive'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ECLOSE(CM(1),0.0,0.001,close1)
          IF(CLOSE1)THEN
            CM(2)=21.0
            CM(3)=1.0
            CM(4)=0.3
            CM(5)=1.0
            CM(6)=1000.0
          ENDIF
          CM(1)=8.0
          call edisp(iuout,' ')
          call edisp(iuout,' current control parameters are:')
          call edisp(iuout,
     &      ' nominal, max flow , min flow , prop band, reset time')
 101      WRITE(HOLD,'(5F8.3)')CM(2),CM(3),CM(4),CM(5),CM(6)
          CALL EDISP(IUOUT,HOLD)
          CALL EASKS(HOLD,
     &    ' nominal, max flow, min flow, prop band, reset time (sec)',
     &    '(see help)',42,HOLD,'PI controller',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),1.,1.,'-','Setpoint nominal ',IER)
          CALL EGETWR(HOLD,K,cm(3),1.,1.,'-','Setpoint max',IER)
          CALL EGETWR(HOLD,K,cm(4),1.,1.,'-','Setpoint min',IER)
          CALL EGETWR(HOLD,K,cm(5),1.E-6,1.E6,'F','Prop band',IER)
          CALL EGETWR(HOLD,K,cm(6),1.E-6,1.E6,'F','Int reset time',IER)
          IAD=0
          CALL EASKAB('Action flag (see help)',' ',
     &    ' direct ',' inverse ',iad,nbhelp)
          IF(IAD.EQ.2)IAD=-1
          CM(7)=REAL(IAD)
          IAD=0
          CM(8)=0.
          CALL EASKABC('Which sensor to use',' ',
     &    'this loops sensor','multisensor loop','continue',iad,nbhelp)
          IF(IAD.EQ.2)THEN
            CALL EASKR(ocli,' ',' control loop index ',
     &      0.,'-',24.,'-',0.,'control loop index',IER,nbhelp)
            CM(8)=ocli
          ENDIF
          if(ier.ne.0)goto 101

C Miscellaneous data for additional flow controllers should follow here.
        endif
  70    continue

C Extract from working array to fmiscd() common structure
        call extrctl(2,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      return
      END

C *********** EGCNTRL
C Edit global control function II.
      SUBROUTINE EGCNTRL(ITRC,II,IV,IER)
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/sctl/tcps,ictyp,iclaw,cm(misc)

      CHARACTER outs*124,SSTR*96,TMP*96,LTMP*248
      character outsa*84,outsb*84,outsc*84,KEY*1
      character hold*42
      CHARACTER*84 VERT(20)
      logical close1
      integer IADR  ! for radio button
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EGCNTRL'  ! set for subroutine

C Number of global functions and day type must be > 0.
      IF(II.LE.0.OR.IV.LE.0)RETURN

C Control day types.
      J=IV
      JJ=J

C Initial menu entry setup.
   92 IER=0
      IVERT=-3
      WRITE(vert(1),'(a,i3,a,i3)')' function',II,' day type',JJ
      WRITE(vert(2),'(a,i3)')     ' number of periods:',NGCDP(II,JJ)
      WRITE(vert(3),'(a)') ' ________________________________________'
      WRITE(vert(4),'(a)')
     &' per| start|sensed  |actuated | control law     | data'
      WRITE(vert(5),'(a)')
     &' no.| time |property|property |                 |     '
      LL=NGCDP(II,JJ)

C Generate menu entry for each period in day type.
      m=5
      DO 91 KP=1,LL
        KK=KP
        m=m+1
        call stfctl(3,II,JJ,KK)
        CALL EVCNTRL(3,II,JJ,KK,'T',SSTR)
        CALL EVCNTRLAW(3,II,JJ,KK,TMP,LTMP)
        NITEMS=INT(cm(1))
        WRITE(outsa,'(i2,F7.2,1x,a,1x,a)')KK,tcps,SSTR(1:18),TMP(1:18)
        outsb=' '
        outsc=' '
        if(NITEMS.gt.0)then
          if(NITEMS.LE.9)then
            write(outsb,'(9F9.1)')(cm(L),L=2,NITEMS+1)
          else
            write(outsb,'(9F9.1)')(cm(L),L=2,10)
          endif
          call SDELIM(outsb,outsc,'S',IW)
        endif
        CALL EMKEY(M-5,KEY,IER)
        write(vert(m),'(1a,1x,a,1x,a)',IOSTAT=IOS,ERR=3) key,
     &    outsa(1:47),outsc(1:34)
  91  continue
      m=m+1
      WRITE(vert(m),'(a)') ' ________________________________________'
      m=m+1
      WRITE(vert(m),'(a)') '* add/ delete a period '
      m=m+1
      WRITE(vert(m),'(a)') '? help '
      m=m+1
      WRITE(vert(m),'(a)') '- exit  '
      mvert=m

C Help text for this menu.
      helptopic='global_ctl_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      CALL EMENU('Control periods',VERT,MVERT,IVERT)
      if(IVERT.EQ.mvert)then
        return
      elseif(IVERT.EQ.mvert-1)then

C Explanation
        CALL PHELPD('control period help',nbhelp,'-',0,0,IER)
      elseif(IVERT.EQ.mvert-2)then


C Add or delete a period.
        CALL EASKABC('Options:',' ',
     &    'add period','delete period','continue',iad,nbhelp)
        if(iad.eq.1)then
          call addctldp(3,II,JJ,'A')
        elseif(iad.eq.2)then
          call addctldp(3,II,JJ,'D')
        endif
        goto 92
      ELSEIF(IVERT.GT.5.AND.IVERT.LT.(mvert-3))THEN

C Take the t?cps() i?ctyp(),i?claw(), ?MISCD(II,JJ,KK,?) common and
C stuff into working common array 'cm' for editing.  At end
C of if:elseif:else recover.
        call stfctl(3,II,JJ,KK)

        NITEMS=INT(cm(1))
        if(NITEMS.gt.0)then
          WRITE(outs,'(a,i3,a)')' For period',KK,' current data is...'
          call edisp(iuout,outs)
          if(NITEMS.LE.13)then
            write(outs,'(13F9.1)')(cm(L),L=2,NITEMS+1)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
          else
            write(outs,'(13F9.1)')(cm(L),L=2,13)
            call SDELIM(outs,outsc,'S',IW)
            call edisp(iuout,outsc)
            if(NITEMS.LE.26)then
              write(outs,'(13F9.1)')(cm(L),L=14,NITEMS)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            else
              write(outs,'(13F9.1)')(cm(L),L=14,26)
              call SDELIM(outs,outsc,'S',IW)
              call edisp(iuout,outsc)
            endif
          endif
        else
          WRITE(outs,'(a,i3,a)')' For period',KK,' define...'
          call edisp(iuout,outs)
        endif

        call edisp(iuout,' ')
        call edisp(iuout,'Standard flux actuation enabled... ')
        ictyp=0

        ino=iclaw
        if(iclaw.eq.5)ino=4
        idno=1
        write(outs,'(a,i3,a)')'Select a control law (currently: ',
     &    ino,')'
        call MENUATOL(outs,' Available laws',
     &   'a Global on-off controller','b Global capacity controller',
     &   'c Global schedule on controller','d Global multisensor ',
     &   ' ',' ',' ',' ',' ',' ',' ',' ',ino,idno,nbhelp)
        if(ino.eq.0)then
          continue
        endif
        IF(INO.EQ.4)INO=5
        iclaw=ino
        IGCLAW(II,JJ,KK)=iclaw

  62    CALL EASKR(tcps,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,nbhelp)
        if(KK.GT.1)then
          IF(TGCPS(II,JJ,KK-1).GE.tcps)then
            call usrmsg('Periods out of order..',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcps,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif
        TGCPS(II,JJ,KK)=tcps

        if(IGCLAW(II,JJ,KK).eq.1)then
           
C Global on/off control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global on/off controller data is:')
            call edisp(iuout,
     &      ' Mode, Minimum loops ON [mode_1], setpoint [mode_2] ')
            write(outs,'(2F9.2)')cm(2),cm(3)
            call edisp(iuout,outs)
          endif
          cm(1)=2.0

  33      WRITE(HOLD,'(2f7.2)')cm(2),cm(3)
          CALL EASKS(HOLD,' Mode; Min ON [mode_1] or stpt [mode_2]',
     &      '  ',42,' 1. 1. ','global on-off ctlr',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Mode',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Min on |setpt',IER)
          if(ier.ne.0)goto 33
        elseif(IGCLAW(II,JJ,KK).eq.2)then

C Global capacity management control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global capacity management controller data is:')
            call edisp(iuout,
     &      ' Glb htg capy, Glb clg capy, No shed')
            write(outs,'(3F9.2)')cm(2),cm(3),cm(4)
            call edisp(iuout,outs)
          endif
          cm(1)=3.0

  35      WRITE(HOLD,'(3f7.2)')cm(2),cm(3),cm(4)
          CALL EASKS(HOLD,'Glb htg capy, Glb clg capy, No shed ',
     &      '  ',36,' 10000. 10000. 1.','global on-off ctlr',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','Htg capy',IER)
          CALL EGETWR(HOLD,K,cm(3),0.,0.,'-','Clg capy',IER)
          CALL EGETWR(HOLD,K,cm(4),0.,0.,'-','No. shed',IER)
          if(ier.ne.0)goto 35
        elseif(IGCLAW(II,JJ,KK).eq.3)then

C Global schedule on control.
          if(itrc.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Current global schedule on controller data is:')
            call edisp(iuout,
     &      ' No. of sched loops')
            write(outs,'(1F9.2)')cm(2)
            call edisp(iuout,outs)
          endif
          cm(1)=1.0

  37      WRITE(HOLD,'(1f7.2)')cm(2)
          CALL EASKS(HOLD,'No. of scheduled loops',
     &      '  ',22,' 2.','schedule on controller',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,cm(2),0.,0.,'-','No. sched lps',IER)
          if(ier.ne.0)goto 37
        elseif(IGCLAW(II,JJ,KK).eq.5)then

C Global multisensor controller
C This controller senses the state of other control loops and operates
C the actuator to either on or off condition based on a logical relation
C amongst the other control loops. It currently supports the mass flow
C and plant domains because there are available variables that
C explicitly define the state of a controller to be on/off in those
C domains. On the mass flow side this is accomplished by the ctlpos 
C variable that is a number from 0 to 1 and on the plant side this is 
C accomplished by the variable lastout which is also 0 to 1 depending
C upon whether the last output from the loop was on/off respectively.
          helptopic='global_ctl_multi_sens'
          call gethelptext(helpinsub,helptopic,nbhelp)
          ICTYP=5
          nclp=(nint(cm(1))-1)/3
          WRITE(OUTS,'(a,i4,a)')'Currently sensing ',nclp,' loops'
          CALL EDISP(IUOUT,OUTS(1:LNBLNK(OUTS)))
          CALL EASKI(nClp,' ','Number of loops to sense ?',
     &      0,'F',4,'-',4,'no. loops to sense',IERI,nbhelp)
          CM(1)=REAL(NCLP*3+1)
          IONE=0
          DO 5050 ICLP=1,NCLP
            IDOM=0
 1001       IF(NINT(CM(ICLP*3-1)).EQ.1)THEN
              outs='select domain (currently mass flow)'
            ELSEIF(NINT(CM(ICLP*3-1)).EQ.2)THEN
              outs='select domain (currently plant)'
            ELSE
              outs='select domain (currently none)'
            ENDIF
            iotsl=lnblnk(outs)
            call edisp(iuout,outs(1:iotsl))
            call MENUATOL(outs(1:iotsl),outs(1:iotsl),
     &        'a mass flow','b plant',
     &        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',idom,idno,nbhelp)
            IF(IDOM.LE.0)GOTO 1001
            CM(ICLP*3-1)=REAL(IDOM)
            NLPS=NINT(CM(ICLP*3))
            WRITE(OUTS,'(a,i4)')'Current sensed loop number = ',NLPS
            CALL EDISP(IUOUT,OUTS(1:LNBLNK(OUTS)))
            CALL EASKI(NLPS,' ','Loop number to sense ?',
     &        1,'-',1,'-',1,'no. loops to sense',IERI,nbhelp)
            CM(ICLP*3)=REAL(NLPS)
            IF(ICLP.NE.NCLP.OR.ICLP.EQ.1)THEN
              IF(IONE.EQ.0)THEN
                IADR=NINT(CM(ICLP*3+1))
                IF(IADR.EQ.1)THEN
                  CALL EDISP(IUOUT,'Current action is AND')
                ELSEIF(IADR.EQ.2)THEN
                  CALL EDISP(IUOUT,'Current action is OR')
                ELSEIF(IADR.EQ.3)THEN
                  CALL EDISP(IUOUT,'Current action is NOT')
                  IONE=1
                ELSEIF(IADR.EQ.4)THEN
                  CALL EDISP(IUOUT,'Current action is SLAVE')
                  IONE=1
                ELSEIF(IADR.EQ.5)THEN
                  CALL EDISP(IUOUT,'Current action is MAX')
                  IONE=1
                ELSEIF(IADR.EQ.6)THEN
                  CALL EDISP(IUOUT,'Current action is MIN')
                  IONE=1
                ENDIF
                IADR=1
                CALL EASKATOG('Logical action variable',
     &          'with next sensor','AND','OR','NOT','SLAVE',
     &          'MAX','MIN',' ',IADR,nbhelp)
                CM(ICLP*3+1)=REAL(IADR)
              ELSE
                CM(ICLP*3+1)=CM((ICLP-1)*3+1)
              ENDIF
            ENDIF
 5050     CONTINUE
          IDOM=0
 1002     IF(NINT(CM(NCLP*3+1)).EQ.1)THEN
            outs='select domain for actuator (currently mass flow)'
          ELSEIF(NINT(CM(ICLP*3+1)).EQ.2)THEN
            outs='select domain for actuator (currently plant)'
          ELSE
            outs='select domain for actuator (currently none)'
          ENDIF
          iotsl=lnblnk(outs)
          call edisp(iuout,outs(1:iotsl))
          call MENUATOL(outs(1:iotsl),'select actuation domain',
     &      'a mass flow','b plant',
     &      ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',idom,idno,nbhelp)
          IF(IDOM.LE.0)GOTO 1002
          CM(NCLP*3+1)=REAL(IDOM)
          IAPS=NINT(CM(NCLP*3+2))
          WRITE(OUTS,'(a,i4)')'Current actuated loop number = ',IAPS
          CALL EDISP(IUOUT,OUTS(1:LNBLNK(OUTS)))
          CALL EASKI(IAPS,' ','Loop number to actuate ?',
     &    1,'-',1,'-',1,'loop to actuate',IERI,nbhelp)
          CM(NCLP*3+2)=REAL(IAPS)
        endif

C Extract from working array to gmiscd() common structure
        call extrctl(3,II,JJ,KK)
        call usrmsg(' ',' ','-')
      ELSE
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-4
      call usrmsg(' ',' ','-')
      GOTO 92

 3    if(IOS.eq.2)then
        CALL USRMSG(' ','Permissions issue writing miscellaneous data.',
     &    'W')
      else
        CALL USRMSG(' ','Problem writing miscellaneous data.','W')
      endif
      RETURN
      END

C **************** senloc 
C senloc presents  a list of applicable sensor locations depending
C on the control domain and includes explicit zones list.
C It is passed a prompt, menu title and returns 4 sensor indicators
C (isn1,isn2,isn3,isn4). isense is the condition sensed in the case of 
C flow control, nsup is the number of supplemental items for associated
C flow connections.It is assumed that the user will make one selection only.
      subroutine senloc(prompt,title,icfoc,II,isn1,isn2,isn3,isn4,
     &  isn5,isense,nsup,errmsg,IER)
#include "building.h"
#include "net_flow.h"
#include "plant.h"
#include "epara.h"
#include "control.h"
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
     
      LOGICAL SELECT,OK,dok
      LOGICAL active_option
      CHARACTER*(*) prompt,title,errmsg

      DIMENSION SALT(3),IVALS(3),VERT(35),ICPK(MCONTM)
      CHARACTER VERT*40,KEY*1,prompt2*36
      character outs*124,SALT*40
      integer isn1T,isn2T,isn3T,isn4T,ilayt   ! temporary values for editing.
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='senloc'  ! set for subroutine

C Initialise zone menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
   3  SELECT=.FALSE.
      if(icfoc.eq.0)then
        MHEAD=14
      elseif(icfoc.eq.1)then
        MHEAD=13
      elseif(icfoc.eq.2)then
        MHEAD=12
      elseif(icfoc.eq.3)then
        MHEAD=2
      elseif(icfoc.eq.4)then
        MHEAD=2
      elseif(icfoc.eq.5)then
        MHEAD=6
      elseif(icfoc.eq.6)then
        MHEAD=12
      endif
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu. And scan for the relevant
C help text blocks for the current control domain.
   33 M=MHEAD
      if(icfoc.eq.0)then    ! zones focus
        helptopic='senslocation_bld'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a senses current zone db temp         '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') 'd   sol-air temperature (not for flow)'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j references temporal file item       '
        WRITE(VERT(12),'(a)')'k uses value from function generator  '
        WRITE(VERT(13),'(a)')'l changes thermophysical properties   '
        WRITE(VERT(14),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.1)then
        helptopic='senslocation_plant'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a senses output of a plant component  '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') 'd   sol-air temperature (not for flow)'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j references temporal file item       '
        WRITE(VERT(12),'(a)')'k uses value from function generator  '
        WRITE(VERT(13),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.2)then
        helptopic='senslocation_flow'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a senses plant component node T       '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') '    not applicable'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   external relative humidity        '
        WRITE(VERT(11),'(a)')'j senses flow node or connection      '
        WRITE(VERT(12),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.3)then
        helptopic='senslocation_nothing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a sensed condition not used           '
        WRITE(VERT(2),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.4)then
        helptopic='senslocation_nothing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a power control not yet implemented   '
        WRITE(VERT(2),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.5)then
        helptopic='senslocation_optical'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a senses current zone db temp         '
        WRITE(VERT(2),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(3),'(a)') 'b   external dry bulb temperature     '
        WRITE(VERT(4),'(a)') 'c   incident radiation at ext. surface'
        WRITE(VERT(5),'(a)') 'd senses lux levels                   '
        WRITE(VERT(6),'(a)') '  ------------------------------------'
      elseif(icfoc.eq.6)then    ! complex fenistration
        helptopic='senslocation_CFC'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)') 'a sensor not used (schedule only)     '
        WRITE(VERT(2),'(a)') 'b senses mix of zone db temp and MRT  '
        WRITE(VERT(3),'(a)') '  senses an ambient condition...      '
        WRITE(VERT(4),'(a)') 'c   dry bulb temperature              '
        WRITE(VERT(5),'(a)') 'd   sol-air temperature (not for flow)'
        WRITE(VERT(6),'(a)') 'e   wind speed                        '
        WRITE(VERT(7),'(a)') 'f   wind direction                    '
        WRITE(VERT(8),'(a)') 'g   diffuse horizontal solar radiation'
        WRITE(VERT(9),'(a)') 'h   direct normal solar radiation     '
        WRITE(VERT(10),'(a)')'i   direct sol rad incident on surface'
        WRITE(VERT(11),'(a)')'j senses lux levels (not working)     '
        WRITE(VERT(12),'(a)')'  ------------------------------------'
      endif
      M=M+1
      CALL EMKEY(M,KEY,IER)
      WRITE(VERT(M),'(2a)')KEY,' senses temp in a specific zone'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)= '  ------------------------------------' 
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF
      VERT(M+2)  ='? help                  '
      VERT(M+3)  ='- exit this menu        '
      write(prompt2,'(a)')' '

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then display error message.
        IF(.NOT.SELECT)then
          call usrmsg(errmsg,'Please select a menu option!','W')
          IVERT=-2
          goto 92
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text based on blocks of text related to the specific
C control domain.
        CALL PHELPD('control top section',nbhelp,'-',0,0,IER)
        goto 92
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          IVERT=-2
          goto 33
        ELSE
          goto 92
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(isn1,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)
        CALL EPMENRC
        SELECT=.TRUE.
        if(icfoc.ne.5)then
          CALL EASKABC('Specific location: ',' ','zone air point',
     &      'surface','continue',IW,nbhelp)
        elseif(icfoc.eq.5)then

C For optical controls, always assume zone air point
          call edisp(iuout,
     &      ' The location is at the specified zone air point')
          IW=1
        endif
        if(IW.eq.1)then
          isn2=0
          isn3=0
        elseif(IW.eq.2)then
          CALL EPMENSV
          call EASKSUR(isn1,isn2,'-','Available surfaces.',' ',IER)
          CALL EPMENRC
          CALL EASKABC('Specific location in surface: ',' ',
     &      'at inside face','node within surface (see help)',
     &      'continue',IWN,nbhelp)
          if(IWN.eq.1)then
            isn3=0
          elseif(IWN.eq.2)then

C Work with local variables and only instanciate if ier is zero.
            ilayt=0
            isn3t=isn3
            call asksnode(isn1,isn2,ilayt,isn3t,ier)
            if(ier.eq.0)then
              ilay=ilayt
              isn3=isn3t
            else
              goto 3
            endif
          elseif(IWN.eq.3)then
            goto 3
          endif
        elseif(IW.eq.3)then
          goto 3
        endif
        nsup=0

C In the case of mass flow this choice implies no supplemental
C items, but return isense as the control law.
        if(icfoc.eq.2)isense=1
      elseif(ivert.eq.1.and.(icfoc.eq.0.or.icfoc.eq.5))then

C Senses current zone db temp if ideal.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.1.and.(icfoc.eq.1.or.icfoc.eq.2))then

C Senses output (plant control) or temperature (flow control) of a plant component.
C Offer the user a cancel option.
        isn1T=-1
        isn2T=isn2
        CALL ASKPCMP('Plant component for sensor ? ','-',IP,IER)
        if(IP.eq.0) goto 3
        ISN2T=IP
        isn3T=isn3

        if(nnodes(ip).eq.1) then
          ISN3T=1
        elseif(nnodes(ip).gt.1) then
           call getnod(ip,knode)
           if(knode.eq.0) goto 3
           ISN3T=knode
        endif


C Optionally allow user to detect the temperature between
C two nodes. 
        call EAskAB('Does sensor measure the temperature ',
     &              'difference between two nodes?',
     &              'yes', 'no', iWhich, nbhelp )



C If user has requested measurement of temperature difference,
C determine second component and node. 
        if ( iWhich == 1 ) then
          
C Get plant component index
          CALL ASKPCMP('Plant component for sensor ? ','-',IP,IER)
          isn4T=IP

C Get node in plant component
          if(IP == 0 ) then
            ISN5T = 0
          elseif ( nnodes(ip) == 1) then
            ISN5T=1
          elseif(nnodes(ip) > 1) then
            call getnod(ip,knode)
            ISN5T=knode
          endif

        else
C User has declined to measure difference between nodes.
          ISN4T = 0
          ISN5T = 0

        endif

C If the user has not canceled instanciate from temporary values.
        nsup=0
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
        isn4=isn4T
        isn5=isn5T
        
      elseif((ivert.eq.1.and.icfoc.eq.3)
     &        .or.(ivert.eq.1.and.icfoc.eq.6))then

C Sensed condition does not matter.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.1.and.(icfoc.eq.4))then

C Power control sensor not yet implemented, return zeros.
        isn1=0
        isn2=0
        isn3=0
        nsup=0
      elseif(ivert.eq.2.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.6))then

C Senses mix of zone db temp and MRT.
        isn1T=-2
        CALL EPMENSV
        idef=0
        isn2T=isn2
        call askzone(isn2T,idef,'Zone for sensor','Select zone','-',
     &    'MRT sensor',ier)
        CALL EPMENRC
        isn3T=isn3
        CALL EASKI(isn3T,' ',' Convective weighting factor (%) ?',
     &      0,'F',100,'-',50,'convective weighting',IERI,nbhelp)
        if(ieri.eq.-3) goto 3

C In the case of mass flow this choice implies no supplemental
C items, but return isense as the control law.
        if(icfoc.eq.2)isense=1

C If the user has not canceled instanciate from temporary values.
        nsup=0
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      elseif(ivert.eq.4.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2
     &       .or.icfoc.eq.6))then

C Senses outside db.
        nsup=0
        isn1=-3
        isn2=0
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.3.and.icfoc.eq.5)then

C Senses outside db.
        nsup=0
        isn1=-3
        isn2=0
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.5.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.6))then

C Senses sol-air temperature.
        nsup=0
        isn1=-3
        isn2=1
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.4.and.icfoc.eq.5)then

C Senses incident solar radiation on a surface.
C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(isn1,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)
        CALL EPMENRC
        SELECT=.TRUE.
        CALL EPMENSV
        call EASKSUR(isn1,isn2,'-','Available surfaces.',' ',IER)
        CALL EPMENRC

C<<<<<need to confirm isn2 because it will need to be assigned to >>>>>>>>>>>
C<<<<<<<<<<the outside face of the surface >>>>>>>>>>>>>>>>>>
        isn3=0
        nsup=0
      elseif(ivert.eq.10.and.icfoc.eq.6)then
      
C Senses incident solar radiation on an external CFC surface. 
C Ask for the specific zone.
        isn1=-4
        CALL EPMENSV
        idef=0
        call askzone(isn2,idef,
     &    'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)
        CALL EPMENRC
        SELECT=.TRUE.
        CALL EPMENSV
        call EASKSUR(isn2,isn3,'-','Available surfaces.',' ',IER)
        CALL EPMENRC
        
      elseif((ivert.eq.5.and.icfoc.eq.5).or.
     &       (ivert.eq.11.and.icfoc.eq.6))then

C Sense lux via daylight coef method. 
C Ask which of the two existing ways to use: Light Switch (manual 
C blind control) or Lux level
        active_option=.true.
        do while(active_option)
        
          call EASKAB(' ','Available lux level controls:',
     &      'Light Switch (N/A)','Lux level',IW,nbehlp)
          if(IW.eq.1)then
            active_option=.true.
C Light Switch (manual blind control)
            isn1= -8
            call edisp(iuout,
     &        ' This is not currently available from this menu.')
            call edisp(iuout,
     &        ' Use the tranparent multilayer constructions file (TMC)')
            call edisp(iuout,
     &        ' for this type of control.')       
          elseif(IW.eq.2)then
            active_option=.false.

C Lux level control
            isn1=-9
C Senses incident solar radiation on a surface.
C Ask for the specific zone.
            CALL EPMENSV
            idef=0
            call askzone(isn2,idef,
     &    'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)
            CALL EPMENRC
            SELECT=.TRUE.
            CALL EPMENSV
            call EASKSUR(isn2,isn3,'-','Available surfaces.',' ',IER)
            CALL EPMENRC
            nsup=0
          endif
        end do

        isn2=0
        isn3=0
        isn4=0
      elseif(ivert.eq.6.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2
     &       .or.icfoc.eq.6))then

C Senses wind speed.
        nsup=0
        isn1=-3
        isn2=2
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.7.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2
     &       .or.icfoc.eq.6))then

C Senses wind direction.
        nsup=0
        isn1=-3
        isn2=3
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.8.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2
     &       .or.icfoc.eq.6))then

C Senses diffuse horizontal solar radiation.
        nsup=0
        isn1=-3
        isn2=4
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.9.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2.or.
     &       icfoc.eq.6))then

C Senses direct normal solar radiation.
        nsup=0
        isn1=-3
        isn2=5
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.10.and.
     &      (icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2))then

C Senses external relative humidity.
        nsup=0
        isn1=-3
        isn2=6
        isn3=0
        if(icfoc.eq.2)isense=0
      elseif(ivert.eq.11.and.(icfoc.eq.0.or.icfoc.eq.1))then

C Uses information in temporal file.
C << check whether temporal file associated with model. >>
        isn1T= -5
        isn2T=isn2
        CALL EASKI(isn2T,'Index of the temporal item to associate with',
     &    ' this sensor: ',0,'F',0,'-',1,'temporal file item',
     &    IERI,nbhelp)
        isn3T=0
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      elseif(ivert.eq.11.and.(icfoc.eq.2))then

C Senses network flow node or connection. 
C  isn1= -4  always
C  isn2  index node/connection
C  isn3  2nd index node (for differences), or contaminant number
C  isn4=  0 node, 1=connection (tie in with type later), 2 contaminant control
        CALL EASKABC(' ',' Flow sensor at:',' flow node (typical) ',
     &    ' specific flow connection (see help) ','continue',IW,nbhelp)
        if(IW.eq.1)then

C Flow controller. Note: there is only one `type` per flow function 
C if actuation is at a component.
C Note: isense will eventually be passed to IFCTYP(II,1,1)=isense
  93      isn1= -4
          isn4=0
          ino=IFCTYP(II,1,1)
          idno=1
          write(outs,'(2a,i3,a)')'Select a property to sense ',
     &     '(currently:',IFCTYP(II,1,1),')'
          call MENUATOL(outs,' Sensed Property',
     &     'a dry bulb temperature at node','b enthalpy at node',
     &     'c additional plant output','d delta T between nodes',
     &     'e absolute delta T between nodes','f pressure at node',
     &     'g delta P between nodes',
     &     'h abs delta P between nodes',
     &     'i contaminant concentration at node',' ',' ',' ',
     &     ino,idno,nbhelp)
          if(ino.eq.0)then
            call usrmsg('You did not select a valid control law.',
     &        'Please redefine.','W')
            goto 93
          endif

C Depening on the users selection set isense and nsup.
          nsup=1
          if(ino.eq.1)isense=1
          if(ino.eq.2)isense=3
          if(ino.eq.3)isense=9
          if(ino.eq.4)then
            isense=24
            nsup=2
          endif
          if(ino.eq.5)then
            isense=25
            nsup=2
          endif
          if(ino.eq.6)isense=26
          if(ino.eq.7)then
            isense=27
            nsup=2
          endif
          if(ino.eq.8)then
            isense=28
            nsup=2
          endif
          if(ino.eq.9)then
            NCPK=1
            NH=0
            CALL ASKMFCTM(NCPK,ICPK,'choose','contaminant',NH)
            CALL PHELPD('sensor location menu',nbhelp,'-',0,0,IER)
            isn3=icpk(1)
            isn4=2
            isense=41
          endif
        elseif(IW.eq.2)then
          isn1= -4
          SALT(1)='senses 1st ph mass flow at connection  ' 
          SALT(2)='senses 2nd ph mass flow at connection  ' 
          SALT(3)='senses abs mass flow rate at connection' 
          IX=1
          CALL EPICKS(IX,IVALS,' ',' Controller type: ',
     &         40,3,SALT,'controller type',IER,nbhelp)
          if(ix.ne.0)then
            nsup=2
            if(IVALS(1).eq.1)isense=5
            if(IVALS(1).eq.2)isense=7
            if(IVALS(1).eq.3)isense=29
          endif
          CALL EPMENSV
          call ASKCON('  flow connections','-',INS,INE,IC,IER)
          CALL EPMENRC
          if(IC.gt.0)then
            isn2=IC
            isn3=0
            isn4=1
          endif
        elseif(IW.eq.3)then
          return
        endif
      elseif(ivert.eq.12.and.(icfoc.eq.0.or.icfoc.eq.1))then

C Uses value from function generator. 
        isn1T= -6
        isn2T=isn2
        CALL EASKI(isn2T,' ',' Required mathematical function ?',
     &      1,'F',9,'F',1,'generated function',IER,nbhelp)
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
      elseif(ivert.eq.13.and.(icfoc.eq.0))then

C Changes thermophysical properties.
        isn1T= -99
        isn2T=isn2
        CALL EASKI(isn2T,' Upper zone air temperature limit',
     &    ' (integer) above which substitution occurs ? ',
     &    0,'-',0,'-',30,'upper air',IERI,nbhelp)
        if(ieri.eq.-3) goto 3

        isn3T=isn3
        CALL EASKI(isn3T,' Lower zone air temperature limit',
     &    ' (integer) below which substitution occurs ? ',
     &    0,'-',0,'-',0,'lower air',IERI,nbhelp)
        if(ieri.eq.-3) goto 3
        isn1=isn1T
        isn2=isn2T
        isn3=isn3T
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif

C Query user for any nested controls (unless thermo substitution).
      if(icfoc.eq.0.and.isn1.ne.-99)then
        dok=.false.
        CALL ASKOK(' Are there any nested control functions ?',
     &    ' (currently only property substitution allowed) ',OK,
     &    dok,nbhelp)
        if(OK)then
          isn4T=isn4
          CALL EASKI(isn4T,' ',
     &      ' Control function (index) to be nested: ',
     &        0,'F',0,'-',1,'nested control',IERI,nbhelp)
          if(ieri.eq.-3) then
            continue
          else
            isn4=isn4T
          endif
        else
          isn4=0
        endif
      endif
      return
      END


C **************** actloc 
C actloc presents  a list of applicable actuator locations depending
C on the control domain and includes explicit zones list.
C It is passed a prompt, menu title and returns 3 actuator indicators
C (isn1,isn2,isn3,isn4). isense is the condition sensed in the case of 
C flow control, nsup is the number of supplemental items for associated
C flow connections.It is assumed that the user will make one selection only.
      SUBROUTINE actloc(prompt,title,icfoc,II,isn1,ian1,ian2,ian3,
     &  errmsg,IER)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "plant.h"
#include "epara.h"
#include "control.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/fctl4/iasocc(MCF,MCMP),nfsup(MCF)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
       common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      LOGICAL SELECT
      CHARACTER*(*) prompt,title,errmsg

      DIMENSION VERT(35)
      CHARACTER VERT*40,KEY*1,prompt2*36
      character outs*124,LTPCMP*60,CMNAM*12,NDNAM*12
      integer ian1T,ian2T,ian3T,ilayt  ! local variable for editing.
      logical vb_xst ! local variable to check if slat type blind exists in CFC
      logical shd_xst ! local variable to check if shading alyer exists in CFC
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='actloc'  ! set for subroutine
      
C Initialise zone menu variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
   3  SELECT=.FALSE.
      if(icfoc.eq.0)then      ! Zone actuator
        MHEAD=3
      elseif(icfoc.eq.1)then  ! Plant actuator
        MHEAD=2
      elseif(icfoc.eq.2)then  ! Mass flow actuator
        MHEAD=3
      elseif(icfoc.eq.3)then  ! Global actuator
        MHEAD=2
      elseif(icfoc.eq.4)then  ! Power flow actuator
        MHEAD=2
      elseif(icfoc.eq.5)then  ! Optical actuator
        MHEAD=1
      elseif(icfoc.eq.6)then  ! CFC actuator
        MHEAD=3
      endif
      MCTL=3
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      if(icfoc.eq.0.and.isn1.eq.-99)then

C If sensor location -99 ask for up to three surfaces where substi-.
C tutions will occur. << revise to selection list at some point >>.
C Offer the user option to cancel.
        helptopic='actlocation_prop_sub'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ian1T=ian1
        CALL EASKI(ian1T,' ',' First surface index (see help)?',
     &     0,'F',MS,'F',1,'1st surf for subst',IERI,nbhelp)
        if(ieri.eq.-3) return
        ian2T=ian2
        CALL EASKI(ian2T,' ',' Second surface index (see help)?',
     &     0,'F',MS,'F',1,'2nd surf for subst',IERI,nbhelp)
        if(ieri.eq.-3) return
        ian3T=ian3
        CALL EASKI(ian3T,' ',' Third surface index (see help)?',
     &     0,'F',MS,'F',1,'3rd surf for subst',IERI,nbhelp)
        if(ieri.eq.-3) return
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
        return
      endif

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu. 
   33 M=MHEAD
      if(icfoc.eq.0)then
        helptopic='actlocation_bld'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a at current zone air point           '
        WRITE(VERT(2),'(a)')'b mix of convection and radiation     '
        WRITE(VERT(3),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.1)then
        helptopic='actlocation_plant'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a node within a plant component       '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.2)then
        helptopic='actlocation_flow'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a single flow connection              '
        WRITE(VERT(2),'(a)')'b flow component (& assoc connections)'
        if(IFCTYP(II,1,1).eq.41)vert(2)='b contaminant source      '
        WRITE(VERT(3),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.3)then
        helptopic='actlocation_nothing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a actuator not used                   '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.4)then
        helptopic='actlocation_nothing'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a power actuator not yet implemented  '
        WRITE(VERT(2),'(a)')'  ------------------------------------'
      elseif(icfoc.eq.5)then
        helptopic='actlocation_optical'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a optical properties (constructions)  '
      elseif(icfoc.eq.6)then
        helptopic='actlocation_CFC'
        call gethelptext(helpinsub,helptopic,nbhelp)
        WRITE(VERT(1),'(a)')'a shading layer ON/OFF                '
        WRITE(VERT(2),'(a)')'b slat angle of Venetian blind        '
        WRITE(VERT(3),'(a)')'c shade ON/OFF (& slat angle) schedule'
        WRITE(VERT(4),'(a)')'  ------------------------------------'
      endif
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          if(icfoc.eq.0)then
            M=M+1
            CALL EMKEY(M,KEY,IER)
            WRITE(VERT(M),'(3a)')KEY,' air point or surf in ',zname(L)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  ------------------------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF
      VERT(M+2)  ='? help                  '
      VERT(M+3)  ='- exit this menu        '
      write(prompt2,'(a)')' '

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.EQ.MVERT)THEN

C If no selection has been made before exit then display error message.
        IF(.NOT.SELECT)then
          call usrmsg(errmsg,' Please select a menu option. ','W')
          IVERT=-2
          goto 92
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the relevant domain.
        CALL PHELPD('control actuation point',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          IVERT=-2
          goto 33
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        SELECT=.TRUE.
        ian1 = IFOC
        CALL EASKABC('Specific location: ',' ','zone air point',
     &    'surface','continue',IW,nbhelp)
        if(IW.eq.1)then
          ian2=0
          ian3=0
        elseif(IW.eq.2)then
          CALL EPMENSV
          call EASKSUR(ian1,ian2,'-','Available surfaces.',' ',IER)
          CALL EPMENRC
          CALL EASKABC('Specific location in surface: ',' ',
     &      'at inside face','node within surface','continue',
     &      IWN,nbhelp)
          if(IWN.eq.1)then
            ian3=0
            goto 33
          elseif(IWN.eq.2)then

C Work with local variables and only instanciate if ier is zero.
            ilayt=0
            ian3t=ian3
            call asksnode(ian1,ian2,ilayt,ian3t,ier)
            if(ier.eq.0)then
              ilay=ilayt
              ian3=ian3t
              goto 33
            else
              goto 3   ! reset select
            endif
          elseif(IWN.eq.3)then
            goto 3   ! reset select
          endif
        elseif(IW.eq.3)then
          goto 3   ! reset select
        endif
      elseif(ivert.eq.1.and.(icfoc.eq.0))then 

C Actuates current zone air point (fall though to exit).
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.1.and.(icfoc.eq.1))then

C Within a plant component (offer the user a cancel option)
C otherwise fall through to exit.
        ian1T=-1
        IP=-1
        CALL ASKPCMP('Plant component for actuator ? ','-',IP,IER)
        if(IP.le.0) goto 3
        IAN2T=IP

        if(nnodes(ip).eq.1) then
          IAN3T=1
        elseif(nnodes(ip).gt.1) then
          knode=-1
           call getnod(ip,knode)
           if(knode.le.0) goto 3
           IAN3T=knode
        endif
 
        if(ieri.eq.-3) goto 3
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
      elseif(ivert.eq.1.and.(icfoc.eq.2))then

C At a flow connection.
        ian1= -3
        CALL EPMENSV
        call ASKCON(' Flow connection?','-',INS,INE,IC,IER)
        CALL EPMENRC
        write(outs,'(6A)') ' Selected ',NDNAM(NODPS(IC)),
     &    ' - ',NDNAM(NODNE(IC)),' via ',CMNAM(ITPCON(IC))
        call edisp(iuout,outs)
        if(IC.gt.0)then
          ian2=IC
          ian3=0
        endif

C If sensor is a -4 and connection type ask for specific
C supplemental nodes.
        if(ifsn(ii,1).eq.-4.and.ian1.eq.-3)then
          CALL EPMENSV
          call ASKRNOD('  sensed flow node','-',INOD,IER)
          CALL EPMENRC
          if(INOD.gt.0)then
            ifsn(ii,2)=INOD
            if(nfsup(ii).eq.2)then
              CALL EPMENSV
              call ASKRNOD(' 2nd sensed flow node','-',INOD,IER)
              CALL EPMENRC
              if(INOD.gt.0)then
C << pev was ifan(ii,3) which was probably a typo >>
                ifsn(ii,3)=INOD
                ian3=INOD
              endif
            else
              ian3=0
            endif
          endif
        endif
      elseif(ivert.eq.1.and.icfoc.eq.3)then

C Sensed condition does not matter.
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.1.and.(icfoc.eq.4))then

C Power control sensor not yet implemented, return zeros.
        ian1=0
        ian2=0
        ian3=0
      elseif(ivert.eq.1.and.(icfoc.eq.5))then

C Optics
C Make a suggestion to control specific construction types
C 1st item is -5, 2nd item number of constrction
C and the third can be 0
        call edisp(iuout,' ')
        write(outs,'(2A)') ' Avoid assigning more than one control ',
     &        'loop with different datasets to the same construction. '
        call edisp(iuout,outs)
        write(outs,'(2A)') ' If this mistake is made by users the ',
     &        'datasets of the first control loop are used.'
        call edisp(iuout,outs)
        CALL EPMENSV
        call epkmlc(iwhich,'Construction to control?',' ',ierr)
        CALL EPMENRC
        if(iwhich.ne.0)then
          ian1=-5
          ian2=iwhich
          ian3=0
        else
          ian1=0
          ian2=0
          ian3=0
        endif
      elseif(ivert.eq.1.and.(icfoc.eq.6))then
C Complex Fenestration      
C       ian1 = actuator type (0 - shade ON/OFF, 1 - slat angle, 2- both (for schedule)
C       ian2 = zone index
C       ian3 = CFC type index for selected zone

C Actuates shade ON/OFF status
        ian1=0
      
C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(ian2,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)

C Ask for CFC type in selected zone.   
        call askCFCtype(ian2,ian3,IER)

C Check that selected CFC type contains a shading layer.
        shd_xst=.false.
        do 1001 ilayer=1,ncfc_el(ian2, ian3)
          if(icfcltp(ian2,ian3,ilayer).gt.iGlazing)shd_xst=.true.
 1001   continue 
        if(.not.shd_xst)then
            SELECT=.false.
            CALL USRMSG(' Shading layer not found, ',
     &       'please select another CFC type','W')
            CALL EPMENRC
            goto 92
        endif       
        CALL EPMENRC

      elseif(ivert.eq.2.and.(icfoc.eq.0))then

C Actuates radiant convective mix.
        ian1T=-2
        CALL EPMENSV
        idef=0
        ian2T=ian2
        call askzone(ian2T,idef,'Zone for actuation','Select zone','-',
     &    'mixed injection',ier)
        CALL EPMENRC
        ian3T=ian3
        CALL EASKI(ian3T,' ',' Convective weighting factor (%) ?',
     &      0,'F',100,'-',50,'convective weighting',IERI,nbhelp)
        if(ieri.eq.-3) goto 3
        ian1=ian1T
        ian2=ian2T
        ian3=ian3T
      elseif(ivert.eq.2.and.(icfoc.eq.2))then

C At a flow component.
 742    ian1= -4
        CALL EPMENSV
        call ASKRCMP(' Flow component?','-',ICMP,IER)
        CALL EPMENRC
        if((ITPCMP(ICMP).EQ.220.OR.ITPCMP(ICMP).EQ.230.OR.
     &    ITPCMP(ICMP).EQ.240.OR.ITPCMP(ICMP).EQ.250))THEN
          WRITE(outs,'(a,i3,a)') ' Sorry component type ',ITPCMP(ICMP),
     &                           ' cannot be actuated.'
          call edisp(itru,outs)
          goto 742
        elseif(icmp.eq.0)then
          goto 742
        ENDIF
        ian2=ICMP

        call edisp(iuout,' ')
        call edisp(iuout,'A mass flow component (such as a orifice)')
        call edisp(iuout,'may be controlled.  For each instance of a')
        call edisp(iuout,'component that is controlled on a sensed')
        call edisp(iuout,'condition at another node you will be asked')
        call edisp(iuout,'to nominate the node(s). ')
        call edisp(iuout,'A node must be specified for each instance!')
        ian3=0

C Loop through each of the connections and test for a matching
C component type.
        do 842 icl=1,NCNN
          if(CMNAM(ICMP)(1:12).eq.CMNAM(ITPCON(ICL))(1:12))then
            write(outs,'(7A)') 'For connection ',
     &        NDNAM(NODPS(ICL)),' - ',NDNAM(NODNE(ICL)),' via ',
     &        CMNAM(ITPCON(ICL)),' ...'
            call edisp(iuout,outs)
            ian3=ian3+1
            iasocc(II,ian3)=icl

C if isn1.eq.-3 then no supplemental.
            if(nfsup(II).eq.0)then
              ISDCNN(ICL)=0
              NDSCNN(ICL,1)=0
              NDSCNN(ICL,2)=0
              call edisp(iuout,' No supplemental data is required.')
            elseif(nfsup(II).eq.1)then
              ISDCNN(ICL)=1
              CALL EPMENSV
              call ASKRNOD('control on node','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,1)=iv
              NDSCNN(ICL,2)=0
            elseif(nfsup(II).eq.2)then
              ISDCNN(ICL)=2
              CALL EPMENSV
              call ASKRNOD(' 1st sensed node name','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,1)=iv
              CALL EPMENSV
              call ASKRNOD(' 2nd sensed node name','-',iv,IER)
              CALL EPMENRC
              NDSCNN(ICL,2)=iv
            endif

          endif
 842    continue

      elseif(ivert.eq.2.and.(icfoc.eq.6))then
C Complex Fenestration      
C       ian1 = actuator type (0 - shade ON/OFF, 1 - slat angle 2- schedule )
C       ian2 = zone index
C       ian3 = CFC type index for selected zone

C Actuates slat angle of a slat-type shade
        ian1=1
      
C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(ian2,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)

C Ask for CFC type in selected zone.   
        call askCFCtype(ian2,ian3,IER)

C Check that slat-type shade exists for selected CFC type.
        vb_xst=.false.
        do 1002 ilayer=1,ncfc_el(ian2, ian3)
          if(icfcltp(ian2,ian3,ilayer).eq.iVenBlind)vb_xst=.true.
 1002   continue 
        if(.not.vb_xst)then
            SELECT=.false.
            CALL USRMSG(' Venetian blind shading layer not found, ',
     &       'please select another CFC type','W')
     
            CALL EPMENRC
            goto 92
        endif
        CALL EPMENRC
        
      elseif(ivert.eq.3.and.(icfoc.eq.6))then
      
C       ian1 = actuator type (0 - shade ON/OFF, 1 - slat angle 2- both (for schedule) )
C       ian2 = zone index
C       ian3 = CFC type index for selected zone

C Actuates slat angle of a slat-type shade
        ian1=2
      
C Ask for the specific zone.
        CALL EPMENSV
        idef=0
        call askzone(ian2,idef,
     &   'Specific zone associated with this sensor','Select zone','-',
     &    'Specific zone',ier)

C Ask for CFC type in selected zone.   
        call askCFCtype(ian2,ian3,IER)
      
C Check that shade exists for selected CFC type.
        shd_xst=.false.
        do 1003 ilayer=1,ncfc_el(ian2, ian3)
          if(icfcltp(ian2,ian3,ilayer).gt.iGlazing)shd_xst=.true.
 1003   continue 
        if(.not.shd_xst)then
            SELECT=.false.
            CALL USRMSG(' Shading layer not found, ',
     &       'please select another CFC type','W')
     
            CALL EPMENRC
            goto 92
        endif
        CALL EPMENRC
        
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif

C If fell though to this point exit the subroutine.
      return
      END

C ************************ ASKMFCTM ************************
C ASKMFCTM: Asks for one or more contaminants
C PROMPT1 and PROMPT2 are text prompt strings
C NH is number of help strings.

      SUBROUTINE ASKMFCTM(NCPK,ICPK,PROMPT1,PROMPT2,NH)
#include "net_flow.h"

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      DIMENSION ICPK(MCONTM)

      CHARACTER VERTC(MCONTM)*12, CONTMNAM*12
      CHARACTER*(*) PROMPT1, PROMPT2

C Set up array of menu strings.
      if(NCONTM.gt.0)then
        do 10 I=1,NCONTM
          VERTC(I)=CONTMNAM(I)
 10     continue
        call EPICKS(NCPK,ICPK,PROMPT1,PROMPT2,
     &    12,NCONTM,VERTC,'Contaminant select',IER,NH)
      else
        call usrmsg('There are no contaminants to select.',' ','W')
      endif
      return
      end

C ***************** editctlperiod
C Editctlperiod is a generic menu for editing details of a control
C period for a zone control (later flow control). 
C Now also works with complex fenestration control.
C Works with the common block sctl which is assumed to have 
C already been filled.
C  head (48 char) title for the menu
C  icfoc (int) is the control domain
C  icloop (int) is the index of the control loop
C  iday (int) is the current day index
C  iper (int) is the current period
C  nper (int) how many periods have been defined.
C  loopdaytype (int) the number of day types defined for this loop
C  prevstart (real) start time of previous period.
C  act (1 char) is ...
C  ier is greater than one if the user has selected a control law
C    that is not supported yet. The calling routine should then
C    deal with the dialogs using standard sequential method.
      subroutine editctlperiod(head,icfoc,icloop,iday,iper,nper,
     &  loopdaytype,prevstart,act,ier)
#include "building.h"
#include "control.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/sctl/tcps,ictyp,iclaw,cm(misc)

C Calendar data.
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

C menu (32 char) menu text for each item
C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display (if -1 then do not
C   display the data)
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
C nctlhelp (int) number of help lines
C nmenulines (int) one less than int(cm(1)) with exceptions
C   for control law one.
      common/sctlmenu/menu(misc),prompt1(misc),prompt2(misc)
      common/sctldata/ndec(misc),vmin(misc),vmax(misc),
     &  vdef(misc),nctlhelp,nmenulines

      character act*1
      DIMENSION VERT(35),SALT(24),IVALS(24)
      dimension cmlocal(misc)    ! for local editing of cm data.
      real cmlocal
      integer ivals              ! for return of selection array.

      CHARACTER VERT*45,KEY*1,SSTR*96,TMP*96,ltmp*248,SALT*42
      character menu*32,prompt1*72,prompt2*72
      character outs*124
      character*(*) head  ! the title passed from calling code

      integer icfoc,icloop,iday,iper,nper,loopdaytype  ! passed parameters
      logical close,close1,close2,close3,modval,OK,dok ! to test if variable has changed.
      real tcpst  ! for local editing.
      integer imasctlindex    ! index of master control used by slave controller.
      integer iclawcurrent    ! to remember the current control law
      integer ivalue,imini,imaxi,idefa  ! local values for editing as integer
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='editctlperiod'  ! set for subroutine

C Set initial logical values.
      close=.true.
      close1=.true.
      close2=.true.
      modval=.false.

C If user changed the control law or major aspects of the control then
C we need to return to this point and update the cm data prior to
C setting up the menu.
  90  if(modval)then
        do 14 ij=1,misc
          cm(ij)=cmlocal(ij)
  14    continue
      endif

C Fill the control period menu and editing range arrays.
      call stfmenu(icfoc)

C Copy cm data to cmlocal.
      tcpst=tcps
      do 11 ij=1,misc
        cmlocal(ij)=cm(ij)
  11  continue

C List current control data for this loop (unless changes are pending).
      if(.NOT.modval) call LSTCNTLD(iuout,icfoc,icloop,iday)

C Define the static portions of the menu.
   91 MHEAD=6
      MCTL=5
      ILEN=nmenulines   ! number of miscel items to edit set in stfmenu
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=nmenulines   ! number of miscel items to edit set in stfmenu
      IVERT=-3

      if(loopdaytype.eq.0)then

C Following calendar day types.
        write(VERT(1),'(a,i2,3a,i2)') '  Loop ',icloop,
     &   '  day: ',calentag(iday),' period: ',iper
      else
        write(VERT(1),'(a,i2,a,i2,a,i2)') '  Loop ',icloop,
     &    '  day type:',iday,' period: ',iper
      endif 
      write(VERT(2),'(a)')         '  Sensed & actuated property is... '
      CALL EVCNTRL(icfoc,icloop,iday,iper,'T',SSTR)
      write(VERT(3),'(2a)')        '    ',SSTR(1:40)
      write(VERT(4),'(a)')         '  _________________________________'
      write(VERT(5),'(a,f6.3)')    '1 Starting at: ',tcpst

C Display the control law from the file or if modval is true then
C use the value set when the user selected the other control.
      if(modval)then
        continue
      else
        CALL EVCNTRLAW(icfoc,icloop,iday,iper,TMP,LTMP)
      endif
      write(VERT(6),'(2a)')        '2 Law: ',TMP(1:38)

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
C Notestart at cm(1) (typically blank) unless a change in the
C number of items is needed to swap between minor control
C variants (e.g. PID controler).
    3 M=MHEAD
      if(ILEN.eq.1) goto 19   ! if nothing to edit jump around menu logic
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if(ndec(L).eq.-1)then
            WRITE(VERT(M),'(3a)')KEY,' ',menu(L)
          elseif(ndec(L).eq.0)then
            WRITE(VERT(M),'(4a,F9.0)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.1)then
            WRITE(VERT(M),'(4a,F9.1)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.2)then
            WRITE(VERT(M),'(4a,F9.2)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.3)then
            WRITE(VERT(M),'(4a,F9.3)')KEY,' ',menu(L),':',cmlocal(L)
          elseif(ndec(L).eq.4)then
            WRITE(VERT(M),'(4a,F9.4)')KEY,' ',menu(L),':',cmlocal(L)
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
   19 MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF

C Offer switch to previous or next period.
      if(iper.gt.1.and.iper.lt.nper)then
        VERT(M+2)='+ Shift to earlier or later period'
      elseif(iper.eq.1)then
        VERT(M+2)='+ Shift to later period          '
      elseif(iper.eq.nper)then
        VERT(M+2)='+ Shift to earlier period        '
      endif
      VERT(M+3)  ='! List details                   '
      VERT(M+4)  ='? Help                           '
      VERT(M+5)  ='- Exit                           '

C Now display the menu.
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.eq.1.or.IVERT.eq.2.or.IVERT.eq.3.or.IVERT.eq.4)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(icfoc.eq.6)modval=.true.   !always save for complex fen. data
        if(modval)then
          dok=.false.
          CALL ASKOK(' The control period data has changed.',
     &      ' Accept changes?',OK,dok,nctlhelp)
          if(ok)then
            tcps=tcpst
            do 13 ij=1,misc
              cm(ij)=cmlocal(ij)
  13        continue
            call extrctl(icfoc,icloop,iday,iper)
            call edisp(iuout,
     &        'Data accepted (but not yet saved to file)!')
            modval=.false.

C << consider this as a place to also update the control file >>

          endif
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        CALL PHELPD('period control',nctlhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List control details.
        if(modval)then
          call usrmsg(
     &      'Listing only shows data saved in file. While some data',
     &      'has changed these have not been saved to file.','W')
        else
          call LSTCNTLD(iuout,icfoc,icloop,iday)
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Shift to previous or next control period.
        call edisp(iuout,'Switch periods not yet implemented.')
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.EQ.5)THEN

C Period start time. Edit and if it changes then check and see if
C the previous period is before it otherwise warn user.
  62    CALL EASKR(tcpst,' ',' Period start time ? ',
     &    0.,'F',24.,'F',0.,'control start',IER,nctlhelp)
        if(iper.GT.1)then
          IF(prevstart.GE.tcpst)then
            call usrmsg('Periods out of order.',' ','W')
            GOTO 62
          endif
        else
          CALL ECLOSE(tcpst,0.0,0.001,close1)
          IF(.NOT.close1)then
            call usrmsg(' ','First control must begin @ 0.','W')
            GOTO 62
          endif
        endif
        CALL ECLOSE(tcpst,tcps,0.001,close)
        if(.NOT.close)then
          modval=.true.
          goto 92
        endif
      ELSEIF(IVERT.EQ.6)THEN

C Control law (depending on the control domain). Remember the current
C control law to trap changes.
        iclawcurrent=iclaw
       if(icfoc.eq.0)then
        if(ictyp.eq.0)then
          SALT(1)= 'Basic controller for heating/cooling   '
          SALT(2)= 'Free-float controller                  '
          SALT(3)= 'Basic pre-heat or pre-cool controller  '
          SALT(4)= 'Fixed heat injection and extraction    '
          SALT(5)= 'PID control action for heating/cooling '
          SALT(6)= ' Flux connection between zone & plant  '
          SALT(7)= 'Multi-stage control with hysteresis    '
          SALT(8)= 'CAV variable supply T with constraints '
          SALT(9)= ' Heat pipe from `outside` to inside    '
          SALT(10)='Separate flux ON & OFF set points      '
          SALT(11)=' Match sensed/recorded value (ideal)   '
          SALT(12)=' Match sensed/recorded value (on/off)  '
          SALT(13)='Time-proportioning separate (on/off)   '
          SALT(14)='Floating `three-position` control      '
          SALT(15)='Optimum start (with rewind) control    '
          SALT(16)=' Optimum stop control                  '
          SALT(17)=' Fuzzy Logic PI-PD control             '
          SALT(18)=' Null controller.                      '
          SALT(19)=' Multi-sensor heating/cooling          '
          SALT(20)=' Evaporative source (surface)          '
          SALT(21)='Slave Capacity Controller              '
          SALT(22)='VAV cooling with CAV reheat (BETA)     '
          SALT(23)='Match sensed values (ideal:2 setpoints)'
          SALT(24)='Adaptive human comfort model           '
          if(iclawcurrent.ne.0)then
            write(outs,'(2a)') ' Current control law: ',
     &        SALT(iclawcurrent)
            call edisp(iuout,outs)
            IX=1
          else
            IX=iclawcurrent
          endif

C Re-instanciate help specifically for summary of control laws.
          helptopic='bld_ctl_period_data'
          call gethelptext(helpinsub,helptopic,nbhelp)
  63      CALL EPICKS(IX,IVALS,' ',' Control law option:',
     &      42,24,SALT,'control law (indented=old or obscure)',
     &      IER,nbhelp)
          if(ix.eq.0.or.IVALS(1).eq.0)goto 63
          if(iclawcurrent.ne.IVALS(1))then

C Control law has changed so provide some default values for the
C new control law prior to re-forming the menu display.
C Note: for those control laws which are not supported with
C the new menu structure return to the calling subroutine.
            modval=.true.
            iclaw=IVALS(1)
            write(TMP,'(a)') SALT(iclaw)  ! update string for menu
            if(iclaw.eq.1)then
              cmlocal(1)=7.0        ! this swap is supported, major menu change.
              cmlocal(2)=1000.0     ! reset to default values
              cmlocal(3)=0.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=20.0
              cmlocal(7)=24.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.2)then
              cmlocal(1)=0.0        ! this swap is supported, major menu change.
              goto 90
            elseif(iclaw.eq.4)then
              cmlocal(1)=4.0        ! this swap is supported, major menu change.
              cmlocal(2)=1000.0     ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=20.0
              cmlocal(5)=24.0
              cmlocal(6)=0.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.5)then
              cmlocal(1)=10.0        ! varies (this is the smallest) no yet supported.
              cmlocal(2)=1.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=0.0
              cmlocal(5)=20.0
              cmlocal(6)=2.0
              cmlocal(7)=1000.0
              cmlocal(8)=0.0
              cmlocal(9)=24.0
              cmlocal(10)=2.0
              cmlocal(11)=600.0
              cmlocal(12)=600.0
              goto 90
            elseif(iclaw.eq.6)then
              cmlocal(1)=5.0        ! varies (this is the smallest)
              ier=6
              return
            elseif(iclaw.eq.7)then
              cmlocal(1)=12.0       ! this swap is supported, major menu change.
              cmlocal(2)=0.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=2000.0
              cmlocal(5)=3000.0
              cmlocal(6)=0.0
              cmlocal(7)=1000.0
              cmlocal(8)=2000.0
              cmlocal(9)=3000.0
              cmlocal(10)=20.0
              cmlocal(11)=2.0
              cmlocal(12)=24.0
              cmlocal(13)=2.0
              goto 90
            elseif(iclaw.eq.8)then
              cmlocal(1)=6.0
              cmlocal(2)=30.0        ! reset to default values
              cmlocal(3)=10.0
              cmlocal(4)=1.0
              cmlocal(5)=18.0
              cmlocal(6)=24.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.9)then
              cmlocal(1)=9.0
              ier=9
              return
            elseif(iclaw.eq.10)then
              cmlocal(1)=6.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=18.0
              cmlocal(5)=22.0
              cmlocal(6)=26.0
              cmlocal(7)=24.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.11)then
              cmlocal(1)=10.0        ! varies (this is the smallest)
              ier=11
              return
            elseif(iclaw.eq.12)then
              cmlocal(1)=10.0        ! varies (this is the smallest)
              ier=12
              return
            elseif(iclaw.eq.13)then
              cmlocal(1)=12.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=18.0
              cmlocal(5)=22.0
              cmlocal(6)=26.0
              cmlocal(7)=24.0
              cmlocal(8)=10.0
              cmlocal(9)=5.0
              cmlocal(10)=5.0
              cmlocal(11)=10.0
              cmlocal(12)=5.0
              cmlocal(13)=5.0
              goto 90
            elseif(iclaw.eq.14)then
              cmlocal(1)=14.0
              cmlocal(2)=20.0        ! reset to default values
              cmlocal(3)=2.0
              cmlocal(4)=1.0
              cmlocal(5)=2.0
              cmlocal(6)=24.0
              cmlocal(7)=2.0
              cmlocal(8)=1.0
              cmlocal(9)=1.0
              cmlocal(10)=1000.0
              cmlocal(11)=0.0
              cmlocal(12)=1.0
              cmlocal(13)=1000.0
              cmlocal(14)=0.0
              cmlocal(15)=1.0
              goto 90
            elseif(iclaw.eq.15)then
              cmlocal(1)=6.0        ! varies (this is the smallest) no yet supported.
              cmlocal(2)=1000.0      ! reset to default values
              cmlocal(3)=20.0
              cmlocal(4)=1.0
              cmlocal(5)=8.0
              cmlocal(6)=0.5
              cmlocal(7)=1.0
              cmlocal(8)=4.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              goto 90
            elseif(iclaw.eq.16)then
              cmlocal(1)=9.0
              ier=16
              return
            elseif(iclaw.eq.17)then
              cmlocal(1)=7.0
              ier=17
              return
            elseif(iclaw.eq.18)then
              cmlocal(1)=0.0
              ier=18
              return
            elseif(iclaw.eq.19)then
              cmlocal(1)=6.0
              ier=19
              return
            elseif(iclaw.eq.20)then
              cmlocal(1)=1.0       ! varies (this is the smallest)
              ier=20
              return
            elseif(iclaw.eq.21)then
              cmlocal(1)=3.0
              cmlocal(2)=1.0        ! reset to default values
              cmlocal(3)=1000.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=0.0
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90
            elseif(iclaw.eq.22)then
              cmlocal(1)=6.0
              cmlocal(2)=1000.0        ! reset to default values
              cmlocal(3)=12.0
              cmlocal(4)=20.0
              cmlocal(5)=1.0
              cmlocal(6)=0.20
              cmlocal(7)=0.0
              cmlocal(8)=0.0
              cmlocal(9)=0.0
              cmlocal(10)=0.0
              cmlocal(12)=0.0
              goto 90

C new controller for tdf 
C At the moment it does what the control law 11 does
            elseif(iclaw.eq.23)then
              cmlocal(1)=2.0    
              cmlocal(2)=5.0    
              cmlocal(3)=6.0
              ier=23
              goto 90
            elseif(iclaw.eq.24)then
              cmlocal(1)=8.0     
              cmlocal(2)=1000.0  
              cmlocal(3)=0.0
              cmlocal(4)=1000.0
              cmlocal(5)=0.0
              cmlocal(6)=20.0
              cmlocal(7)=24.0
              cmlocal(8)=0.8
              cmlocal(9)=2.0
              goto 90
            else

            endif
            goto 90
          else

C The user did not alter the control law.
            goto 91
          endif
          goto 92
        elseif(ictyp.eq.1)then

        elseif(ictyp.eq.2)then
        
        endif
      
       elseif(icfoc.eq.6)then
              
C Complex fenestration control law        
        SALT(1)= 'Basic control'
        SALT(2)= 'Schedule'
        if(iclawcurrent.ne.0)then
          write(outs,'(2a)') ' Current control law: ',
     &        SALT(iclawcurrent)
          call edisp(iuout,outs)
          IX=1
        else
          IX=iclawcurrent
        endif

C Re-ask for the control law for the period.
  64    CALL EPICKS(IX,IVALS,' ',' Control law option:',
     &      42,2,SALT,'control law ',IER,nbhelp)
        if(ix.eq.0.or.IVALS(1).eq.0)goto 64
        if(iclawcurrent.ne.IVALS(1))then

C Control law has changed so provide some default values for the
C new control law prior to re-forming the menu display.
C Note: for those control laws which are not supported with
C the new menu structure return to the calling subroutine.
            modval=.true.
            iclaw=IVALS(1)
            write(TMP,'(a)') SALT(iclaw)  ! update string for menu
            goto 90
        else

C The user did not alter the control law.
            goto 91
        endif
        goto 92
        endif
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit data item identified by KEYIND
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
        if(ndec(ifoc).eq.-1)then

C If ndec was a negative one then do not edit this number.
          call edisp(iuout,'Nothing to edit...')
          goto 92
        elseif(ndec(ifoc).eq.0)then

C If ndec was a zero then edit this number as an integer.
          ivalue=nint(cmlocal(ifoc))
          imini=nint(vmin(ifoc))
          imaxi=nint(vmax(ifoc))
          idefa=nint(vdef(ifoc))
          CALL EASKI(ivalue,prompt1(ifoc),prompt2(ifoc),
     &      imini,'F',imaxi,'F',idefa,menu(ifoc),IER,nctlhelp)
          if(ier.eq.-3) goto 92
          cmlocal(ifoc)=real(ivalue)
        else

C If ndec was greater than zero then edit this number as a real.
          CALL EASKR(cmlocal(ifoc),prompt1(ifoc),prompt2(ifoc),
     &      vmin(ifoc),'F',vmax(ifoc),'F',vdef(ifoc),menu(ifoc),
     &      IER,nctlhelp)
          if(ier.eq.-3) goto 92
        endif

C If there were no errors and the value has changed then update the
C menu (in case a different set of items is required). If cancel
C requested simply redisplay the menu.
        call eclose(cmlocal(ifoc),cm(ifoc),0.001,close)
        if(close)then
          goto 92
        endif


C The current value changed so do further processing depending on
C the specific logic of each control law. Typically warning messages
C are given if invalid items are found.
        if(icfoc.eq.0)then
         if(ictyp.eq.0)then
          if(iclaw.eq.1.or.iclaw.eq.3)then
            modval=.true.
            if(ifoc.eq.12)then

C If the user has altered the humidity control then revise the cmlocal() data.
C Switch to zero turns off RH control. Change to 2.0 clears two data slots
C previously used for g/s.
              if(int(cmlocal(ifoc)).eq.0)then
                cmlocal(1)=7.0
                cmlocal(8)=0.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                cmlocal(11)=0.0
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=11.0
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=11.0
                cmlocal(10)=0.0
                cmlocal(11)=0.0
                goto 90    ! major change so menu needs to change
              endif
            elseif(ifoc.eq.4.or.ifoc.eq.5)then

C User altered the cooling capacity so check to see if cooling setpoint needs resetting.
              modval=.true.
              CALL ECLOSE(cmlocal(4),0.0,0.001,close1)
              CALL ECLOSE(cmlocal(5),0.0,0.001,close2)
              if(close1.and.close2)then
                call edisp(iuout,' ')
                call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
                cmlocal(7)=100.0
                goto 91   ! minor change
              endif
            else
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.2)then
            continue    ! no post processing required.
          elseif(iclaw.eq.4)then
            continue    ! no post processing required.
            modval=.true.
          elseif(iclaw.eq.5)then
            if(ifoc.eq.2)then

C If the user has switched between P PI PD PID modes adjust.
              if(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=9.0
                cmlocal(11)=0.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=10.0
                cmlocal(11)=600.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.3)then
                cmlocal(1)=10.0
                cmlocal(11)=600.0
                cmlocal(12)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.4)then
                cmlocal(1)=11.0
                cmlocal(11)=600.0
                cmlocal(12)=600.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              endif
            elseif(ifoc.eq.7.or.ifoc.eq.9)then

C User altered the cooling capacity so check to see if cooling setpoint needs resetting.
              modval=.true.
              CALL ECLOSE(cmlocal(7),0.0,0.001,close1)
              CALL ECLOSE(cmlocal(9),0.0,0.001,close2)
              if(close1.and.close2)then
                call edisp(iuout,' ')
                call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
                cmlocal(9)=100.0
                goto 91   ! minor change
              endif
            else
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.7)then

C Warn user if stage capacities do not increment.
            cmlocal(1)=12.0
            if(cmlocal(5).lt.cmlocal(4))then
              call edisp(iuout,'Heating stage 3 less than stage 2!')
            endif
            if(cmlocal(4).lt.cmlocal(3))then
              call edisp(iuout,'Heating stage 2 less than stage 1!')
            endif
            if(cmlocal(3).lt.cmlocal(2))then
              call edisp(iuout,'Heating stage 1 less than base!')
            endif
            if(cmlocal(9).lt.cmlocal(8))then
              call edisp(iuout,'Cooling stage 3 less than stage 2!')
            endif
            if(cmlocal(8).lt.cmlocal(7))then
              call edisp(iuout,'Cooling stage 2 less than stage 1!')
            endif
            if(cmlocal(7).lt.cmlocal(6))then
              call edisp(iuout,'Cooling stage 1 less than base!')
            endif
 
C If no cooling capacity then set cooling set point to 100C.
            CALL ECLOSE(cmlocal(7),0.0,0.001,close1)
            CALL ECLOSE(cmlocal(8),0.0,0.001,close2)
            CALL ECLOSE(cmlocal(9),0.0,0.001,close3)
            if(close1.and.close2.and.close3)then
              call edisp(iuout,' ')
              call edisp(iuout,
     &    ' Zero cooling capacity - setting cooling setpoint to 100C.')
              cmlocal(12)=100.
              cmlocal(13)=1.0
              modval=.true.
              goto 91   ! minor change
            endif
          elseif(iclaw.eq.8)then
            continue    ! no post processing required.
          elseif(iclaw.eq.10)then

C Ensure that heating setpoint on if equal to or above heating setpoint off.
            if(cmlocal(4).ge.cmlocal(5)) call usrmsg(
     &          'The heating ON set-point must be lower than',
     &          'the heating OFF set-point.','W')
            if(cmlocal(6).le.cmlocal(7)) call usrmsg(
     &          'The cooling OFF set-point must be lower than',
     &          'the cooling ON set-point.','W')
          elseif(iclaw.eq.13)then

C Ensure that heating setpoint on if equal to or above heating setpoint off.
            if(cmlocal(4).ge.cmlocal(5)) call usrmsg(
     &        'The heating ON set-point must be lower than',
     &        'the heating OFF set-point.','W')
            if(cmlocal(6).le.cmlocal(7)) call usrmsg(
     &        'The cooling OFF set-point must be lower than',
     &        'the cooling ON set-point.','W')

C Ensure that ON + OFF cycle time is not greater than total cycle time.
            if(cmlocal(8).lt.(cmlocal(9)+cmlocal(10))) call usrmsg(
     &        'The heating ON and OFF cycle minutes are greater than',
     &        'the total heating cycle minutes.','W') 
            if(cmlocal(11).lt.(cmlocal(12)+cmlocal(13))) call usrmsg(
     &        'The cooling ON and OFF cycle minutes are greater than',
     &        'the total cooling cycle minutes.','W') 
          elseif(iclaw.eq.14)then
             continue    ! no postprocessing (yet)
          elseif(iclaw.eq.15)then
            if(ifoc.eq.7)then

C If the user has switched between start-up modes adjust.
              if(int(cmlocal(ifoc)).eq.1)then
                cmlocal(1)=6.0
                cmlocal(8)=4.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.2)then
                cmlocal(1)=7.0
                cmlocal(8)=4.0
                cmlocal(9)=0.0
                cmlocal(10)=0.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              elseif(int(cmlocal(ifoc)).eq.3)then
                cmlocal(1)=9.0
                cmlocal(8)=1.0
                cmlocal(9)=1.0
                cmlocal(10)=1.0
                modval=.true.
                goto 90    ! major change so menu needs to change
              endif
            endif
          elseif(iclaw.eq.21)then

C For master-slave ensure that sensor of this control is non-zero and that
C it points to the same zone as is used by the master controller.
            imasctlindex= int(cmlocal(2))
            if(imasctlindex.eq.0)then
              call usrmsg('You must select a master control loop!',
     &          ' ','W')
            elseif(imasctlindex.gt.ncf)then
              call usrmsg('You must select a existing master',
     &          'control loop!','W')
            endif
            if(ibsn(icloop,1).gt.0.and.imasctlindex.gt.0)then
              if(ibsn(icloop,1).eq.ibsn(imasctlindex,1))then
                modval=.true.
              else
                call usrmsg(
     &           'Slave sensor must sense the same zone as the',
     &           'master control loop!','W')
              endif
            else
              call usrmsg(
     &          'Slave sensor must sense the same zone as the',
     &          'master control loop!','W')
            endif
            if(iban(icloop,1).gt.0)then
              modval=.true.
            else
              call usrmsg(
     &          'Slave actuator must be in the slave zone!',
     &          ' ','W')
            endif
          elseif(iclaw.eq.22)then
            continue    ! no postprocessing (yet)
          elseif(iclaw.eq.23)then
            modval=.true.
          elseif(iclaw.eq.24)then
            modval=.true.
          else

C << other control laws where processing is required... >>
          endif
         elseif(ictyp.eq.1)then

         elseif(ictyp.eq.2)then

         endif
        
        elseif(icfoc.eq.6)then
         if(iclaw.ge.1)modval=.true.
C        ADD CHECKS FOR MISC DATA (e.g. close setpoint must be higher than open setpoint, etc.)         
        endif 
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END 

C ****** stfmenu
C Stfmenu copies menu and prompts for period editing.  It expects the common
C block sctl to have been filled for the current period.
      subroutine stfmenu(icfoc)
#include "building.h"
#include "net_flow.h"

C Parameter
      integer icfoc  ! index of control domain

      common/sctl/tcps,ictyp,iclaw,cm(misc)

C menu (32 char) menu text for each item
C prompt1 (72 char) first easkr prompt
C prompt2 (72 char) second easkr prompt
C ndec (int) number of decimal places to display (-1 indicates no display)
C vmin (real) minimum value acceptable
C vmax (real) maximum value acceptable
C vdef (real) initial/default value
C nctlhelp (int) number of help lines
C nmenulines (int) usually same as int(cm(1)) with exceptions
C   for control law one.
      common/sctlmenu/menu(misc),prompt1(misc),prompt2(misc)
      common/sctldata/ndec(misc),vmin(misc),vmax(misc),
     &  vdef(misc),nctlhelp,nmenulines

      character menu*32,prompt1*72,prompt2*72

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='stfmenu'  ! set for subroutine

      if(icfoc.eq.0)then

C Set first item of menu (this item holds number of supplementary data
C items for the loop and should not be edited
          menu(1)='Choose parameter to edit:'
        if(iclaw.eq.1.or.iclaw.eq.3)then

C Set up the help text for the control period menu as well as the
C individual items to be edited (in one big block).
          helptopic='sys_bctl_01_03'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=12    ! set to the last cm index used.

C Basic control or pre-heat/cool (first array items are never
C used to that the index matches the cm position.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum heating capacity (W)'
          prompt1(2)='Maximum heating capacity (W) which is available'
          prompt2(2)='to meet the demand. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Minimum heating capacity (W)'
          prompt1(3)='Minimum heating capacity (W) The flux injected'
          prompt2(3)='will never drop below this level. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=0.0
          menu(4)='Maximum cooling capacity (W)'
          prompt1(4)='Maximum cooling capacity (W) which is available'
          prompt2(4)='to meet the demand. '
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
          menu(5)='Minimum cooling capacity (W)'
          prompt1(5)='Minimum cooling capacity (W) The flux extracted'
          prompt2(5)='will never be less than this. '
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=0.0
          menu(6)='Heating setpoint (C)'
          prompt1(6)='Heating setpoint (C) (temperature to meet if'
          prompt2(6)='there is sufficient capacity) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=20.0
          menu(7)='Cooling setpoint (C)'
          prompt1(7)='Cooling setpoint (C (temperature to meet if)'
          prompt2(7)='there is sufficient capacity) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0

C Depending on the current number of miscel items also do humidity.
          if(int(cm(1)).eq.7.or.int(cm(1)).eq.6)then
            menu(8)=' '
            prompt1(8)=' '
            prompt2(8)=' '
            ndec(8)=-1; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=0.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=0.0
            menu(11)=' '
            prompt1(11)=' '
            prompt2(11)=' '
            ndec(11)=-1; vmin(11)=0.0; vmax(11)=100.0; vdef(11)=0.0
            menu(12)='RH control >> OFF '
            prompt1(12)=
     & 'RH control not enabled (value is 0.0) change to 1.0 for'
            prompt2(12)=
     & 'moisture injection or 2.0 for temperature control(see help).'
            ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            continue
          elseif(int(cm(1)).eq.11)then
            menu(8)='Upper humidity set point (%)'
            prompt1(8)='Upper humidity set point (%)'
            prompt2(8)=' '
            ndec(8)=3; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=40.0
            menu(9)='Lower humidity set point (%)'
            prompt1(9)='Lower humidity set point (%)'
            prompt2(9)=' '
            ndec(9)=3; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=40.0
            if(int(cm(12)).eq.1)then
              menu(10)='Max Humidification rate (g/s)'
              prompt1(10)='Max Humidification rate (g/s)'
              prompt2(10)=' '
              ndec(10)=3; vmin(10)=0.0; vmax(10)=1000.0; vdef(10)=2.0
              menu(11)='Max Dehumidification rate (g/s)'
              prompt1(11)='Max Dehumidification rate (g/s)'
              prompt2(11)=' '
              ndec(11)=3; vmin(11)=0.0; vmax(11)=1000.0; vdef(11)=2.0
              menu(12)='RH control >> via moisture inj'
              prompt1(12)=
     & 'Moisture injection enabled (value is 1.0) change to 2.0'
              prompt2(12)=
     & 'for temperature control or 0.0 for no RH control(see help).'
              ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            elseif(int(cm(12)).eq.2)then
              menu(10)=' '
              prompt1(10)=' '
              prompt2(10)=' '
              ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
              menu(11)='  '
              prompt1(11)=' '
              prompt2(11)=' '
              ndec(11)=-1; vmin(11)=0.0; vmax(11)=0.0; vdef(11)=0.0
              menu(12)='RH control >> via temp control'
              prompt1(12)=
     & 'Temperature control enabled (value is 2.0) change to 1.0'
              prompt2(12)=
     & 'for moisture injection or 0.0 for no RH control(see help).'
              ndec(12)=1; vmin(12)=0.0; vmax(12)=2.0; vdef(12)=0.0
            endif
          endif
        elseif(iclaw.eq.2)then
          helptopic='sys_bctl_02'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=1    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Free floating control (no data)'
          prompt1(2)=' '
          prompt2(2)=' '
          ndec(2)=-1; vmin(2)=0.0; vmax(2)=0.0; vdef(2)=0.0
        elseif(iclaw.eq.4)then
          helptopic='sys_bctl_04'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=5    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heat injection (fixed W) '
          prompt1(2)='Heat injection (fixed W) if temperature drops'
          prompt2(2)='below the heating setpoint. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=9999999.0; vdef(2)=1000.0
          menu(3)='Cooling extract (fixed W) '
          prompt1(3)='Cooling extract (fixed W) if temperature goes '
          prompt2(3)='over the cooling setpoint. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=9999999.0; vdef(3)=1000.0
          menu(4)='Heating setpoint (C) '
          prompt1(4)='Heating setpoint (C) below which heating will '
          prompt2(4)='be called for '
          ndec(4)=3; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=20.0
          menu(5)='Cooling setopoint (C) '
          prompt1(5)='Cooling setopoint (C) above which cooling will '
          prompt2(5)='be called for '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=24.0
        elseif(iclaw.eq.5)then
          helptopic='sys_bctl_05'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          if(nint(cm(2)).eq.1)then
            menu(2)='Mode >> Proportional'
            nmenulines=10    ! set to the last cm index used
          elseif(nint(cm(2)).eq.2)then
            menu(2)='Mode >> Proportional +integral'
            nmenulines=11    ! set to the last cm index used
          elseif(nint(cm(2)).eq.3)then
            menu(2)='Mode >> Proportional +derivitive'
            nmenulines=11    ! set to the last cm index used
          elseif(nint(cm(2)).eq.4)then
            menu(2)='Mode >>Proportional P+I+D'
            nmenulines=12    ! set to the last cm index used
          endif
          prompt1(2)='Proportional is one, for P+I use two,'
          prompt2(2)='for P+D use three, for P+I+D use four.'
          ndec(2)=0; vmin(2)=0.0; vmax(2)=4.0; vdef(2)=0.0
          menu(3)='Maximum heating capacity (W) '
          prompt1(3)='Maximum heating capacity (W) '
          prompt2(3)=' '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Minimum heating capacity (W) '
          prompt1(4)='Minimum heating capacity (W) (will not go below'
          prompt2(4)='this amount e.g. for uninsulated pipes)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=0.0
          menu(5)='Heating setpoint (C)'
          prompt1(5)='Heating setpoint (C) '
          prompt2(5)='(in the middle of the proportional band) '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=20.0
          menu(6)='Heating throttling range (C) '
          prompt1(6)='Heating throttling range (proportional band) (C)'
          prompt2(6)=' '
          ndec(6)=3; vmin(6)=0.0; vmax(6)=20.0; vdef(6)=2.0
          menu(7)='Maximum cooling capacity (W)'
          prompt1(7)='Maximum cooling capacity (W) '
          prompt2(7)=' '
          ndec(7)=1; vmin(7)=0.0; vmax(7)=999999.0; vdef(7)=1000.0
          menu(8)='Minimum cooling capacity (W)'
          prompt1(8)='Minimum cooling capacity (W) (will not drop below'
          prompt2(8)='this amount) '
          ndec(8)=1; vmin(8)=0.0; vmax(8)=999999.0; vdef(8)=0.0
          menu(9)='Cooling setpoint (C) '
          prompt1(9)='Cooling setpoint (C) '
          prompt2(9)='(in the middle of the proportional band) '
          ndec(9)=3; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=24.0
          menu(10)='Cooling throttling range (C) '
          prompt1(10)='Cooling throttling range (proportional band) (C)'
          prompt2(10)=' '
          ndec(10)=3; vmin(10)=0.0; vmax(10)=10.0; vdef(10)=2.0
          if(nint(cm(2)).eq.1)then
            menu(11)=' '
            prompt1(11)=' '
            prompt2(11)=' '
            ndec(11)=-1; vmin(11)=0.0; vmax(11)=0.0; vdef(11)=0.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=0.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.2)then
            menu(11)='Integral action time [secs] '
            prompt1(11)='Integral action time [secs] '
            prompt2(11)=' '
            ndec(11)=1; vmin(11)=0.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=1.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.3)then
            menu(11)='Derivative action time [secs] '
            prompt1(11)='Derivative action time [secs] '
            prompt2(11)='(see help) '
            ndec(11)=1; vmin(11)=1.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)=' '
            prompt1(12)=' '
            prompt2(12)=' '
            ndec(12)=-1; vmin(12)=0.0; vmax(12)=0.0; vdef(12)=0.0
          elseif(nint(cm(2)).eq.4)then
            menu(11)='Integral action time [secs] '
            prompt1(11)='Integral action time [secs] '
            prompt2(11)='(see help) '
            ndec(11)=1; vmin(11)=1.0; vmax(11)=10000.0; vdef(11)=600.0
            menu(12)='Derivative action time [secs] '
            prompt1(12)='Derivative action time [secs] '
            prompt2(12)='(see help) '
            ndec(12)=1; vmin(12)=1.0; vmax(12)=10000.0; vdef(12)=600.0
          endif
        elseif(iclaw.eq.6)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.7)then

C A multi-stage controller with hysteresis.
          helptopic='sys_bctl_07'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=13    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heat capacity at OFF state (W)'
          prompt1(2)='Heat capacity at OFF state (W) (typically zero'
          prompt2(2)='and a non-zero base load is allowed '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=0.0
          menu(3)='Heat capacity at 1st stage (W) '
          prompt1(3)='Heat capacity at 1st stage (W)'
          prompt2(3)='(must be greater than base load)'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heat capacity at 2nd stage (W)'
          prompt1(4)='Heat capacity at 2nd stage (W)'
          prompt2(4)='(must be greater than 1st stage)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=2000.0
          menu(5)='Heat capacity at 3rd stage (W)'
          prompt1(5)='Heat capacity at 3rd stage (W)'
          prompt2(5)='(must be greater than 2nd stage)'
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=3000.0
          menu(6)='Cool capacity at OFF state (W)'
          prompt1(6)='Cool capacity at OFF state (W) (typically zero'
          prompt2(6)='and a non-zero base load is allowed '
          ndec(6)=1; vmin(6)=0.0; vmax(6)=999999.0; vdef(6)=0.0
          menu(7)='Cool capacity at 1st stage (W)'
          prompt1(7)='Cool capacity at 1st stage (W)'
          prompt2(7)='(must be greater than base load) '
          ndec(7)=1; vmin(7)=0.0; vmax(7)=999999.0; vdef(7)=1000.0
          menu(8)='Cool capacity at 2nd stage (W)'
          prompt1(8)='Cool capacity at 2nd stage (W)'
          prompt2(8)='(must be greater than 1st stage)'
          ndec(8)=1; vmin(8)=0.0; vmax(8)=999999.0; vdef(8)=2000.0
          menu(9)='Cool capacity at 3rd stage (W)'
          prompt1(9)='Cool capacity at 3rd stage (W)'
          prompt2(9)='(must be greater than 2nd stage) '
          ndec(9)=1; vmin(9)=0.0; vmax(9)=999999.0; vdef(9)=3000.0
          menu(10)='Heating setpoint (C)'
          prompt1(10)='Heating setpoint (C)'
          prompt2(10)=' '
          ndec(10)=3; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=20.0
          menu(11)='Heating deadband (C)'
          prompt1(11)='Heating deadband (C)'
          prompt2(11)='(zero or greater)'
          ndec(11)=3; vmin(11)=0.0; vmax(11)=10.0; vdef(11)=1.0
          menu(12)='Cooling setpoint (C)'
          prompt1(12)='Cooling setpoint (C)'
          prompt2(12)=' '
          ndec(12)=3; vmin(12)=0.0; vmax(12)=100.0; vdef(12)=24.0
          menu(13)='Cooling deadband (C)'
          prompt1(13)='Cooling deadband (C)'
          prompt2(13)='(zero or greater)'
          ndec(13)=3; vmin(13)=0.0; vmax(13)=10.0; vdef(13)=1.0
        elseif(iclaw.eq.8)then

C Variable supply temperature with limit constraints.
          helptopic='sys_bctl_08'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=7    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum supply temperature (C)'
          prompt1(2)='Maximum (heating) supply temperature (C) '
          prompt2(2)=' '
          ndec(2)=3; vmin(2)=-10.0; vmax(2)=100.0; vdef(2)=30.0
          menu(3)='Minimum supply temperature (C)'
          prompt1(3)='Minimum (cooling) supply temperature (C)'
          prompt2(3)=' '
          ndec(3)=3; vmin(3)=-10.0; vmax(3)=100.0; vdef(3)=10.0
          menu(4)='Air supply flow rate (m^3/sec) '
          prompt1(4)='Air supply flow rate (m^3/sec) '
          prompt2(4)=' '
          ndec(4)=4; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=1.0
          menu(5)='Heating (room) setpoint (C)'
          prompt1(5)='Heating (room) setpoint (C) '
          prompt2(5)=' '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=18.0
          menu(6)='Cooling (room) setpoint (C)'
          prompt1(6)='Cooling (room) setpoint (C)'
          prompt2(6)=' '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=24.0
          if(int(cm(7)).eq.0)then
            menu(7)='Cooling >> enabled '
            prompt1(7)='Cooling is available in the supply (value is'
            prompt2(7)='a zero) change to a one to turn off cooling)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=1.0; vdef(7)=0.0
          elseif(int(cm(7)).eq.1)then
            menu(7)='Cooling >> not enabled '
            prompt1(7)='Cooling is not available in the supply (value'
            prompt2(7)='is a one) change to zero to turn on cooling)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=1.0; vdef(7)=0.0
          endif
        elseif(iclaw.eq.9)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.10)then
          
C Control with separate on/off setpoints for heating and cooling.
          helptopic='sys_bctl_10'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=7    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heating (fixed) capacity (W)'
          prompt1(2)='Heating capacity (W) used for ON state'
          prompt2(2)='(injection is at a fixed rate)'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Cooling (fixed) capacity (W)'
          prompt1(3)='Cooling capacity (W) used for ON state'
          prompt2(3)='(extraction is at a fixed rate) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heating ON setpoint (C)'
          prompt1(4)='Heating ON setpoint (C) (turn on heating if'
          prompt2(4)='the temperature falls below this setpoint)'
          ndec(4)=3; vmin(4)=-100.0; vmax(4)=100.0; vdef(4)=18.0
          menu(5)='Heating OFF setpoint (C)'
          prompt1(5)='Heating OFF setpoint (C) (turn off heating if'
          prompt2(5)='the temperature goes above this point) '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=22.0
          menu(6)='Cooling ON setpoint (C) '
          prompt1(6)='Cooling ON setpoint (C) (turn on cooling if'
          prompt2(6)='the temperature goes above this setpoint) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=26.0
          menu(7)='Cooling OFF setpoint (C) '
          prompt1(7)='Cooling OFF setpoint (C) (turn off cooling if'
          prompt2(7)='the temperature goes below this setpoint) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
        elseif(iclaw.eq.11)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.12)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.13)then
          
C Time proportioning control with separate on/off setpoints for heating
C and cooling (initial part of the input is similar to control 10).
          helptopic='sys_bctl_13'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=13    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Heating (fixed) capacity (W)'
          prompt1(2)='Heating capacity (W) used for ON state'
          prompt2(2)='(injection is at a fixed rate)'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Cooling (fixed) capacity (W)'
          prompt1(3)='Cooling capacity (W) used for ON state'
          prompt2(3)='(extraction is at a fixed rate) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Heating ON setpoint (C)'
          prompt1(4)='Heating ON setpoint (C) (turn on heating if'
          prompt2(4)='the temperature falls below this setpoint)'
          ndec(4)=3; vmin(4)=-100.0; vmax(4)=100.0; vdef(4)=18.0
          menu(5)='Heating OFF setpoint (C)'
          prompt1(5)='Heating OFF setpoint (C) (turn off heating if'
          prompt2(5)='the temperature goes above this point) '
          ndec(5)=3; vmin(5)=-100.0; vmax(5)=100.0; vdef(5)=22.0
          menu(6)='Cooling ON setpoint (C) '
          prompt1(6)='Cooling ON setpoint (C) (turn on cooling if'
          prompt2(6)='the temperature goes above this setpoint) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=26.0
          menu(7)='Cooling OFF setpoint (C) '
          prompt1(7)='Cooling OFF setpoint (C) (turn off cooling if'
          prompt2(7)='the temperature goes below this setpoint) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
          menu(8)='Total minutues in heating cycle'
          prompt1(8)='Total minutues in heating cycle (must be equal'
          prompt2(8)='or greater than the sum of ON and OFF cycles).'
          ndec(8)=2; vmin(8)=2.0; vmax(8)=60.0; vdef(8)=10.0
          menu(9)='Minimum heating ON minutes'
          prompt1(9)='Minimum ON minutes in heating cycle '
          prompt2(9)=' '
          ndec(9)=2; vmin(9)=1.0; vmax(9)=60.0; vdef(9)=5.0
          menu(10)='Minimum heating OFF minutes'
          prompt1(10)='Minimum OFF minutes in heating cycle'
          prompt2(10)=' '
          ndec(10)=2; vmin(10)=1.0; vmax(10)=60.0; vdef(10)=5.0
          menu(11)='Total minutues in cooling cycle'
          prompt1(11)='Total minutues in cooling cycle (must be equal'
          prompt2(11)='or greater than the sum of ON and OFF cycles)'
          ndec(11)=2; vmin(11)=2.0; vmax(11)=60.0; vdef(11)=10.0
          menu(12)='Minimum cooling ON minutes'
          prompt1(12)='Minimum ON minutes in cooling cycle'
          prompt2(12)=' '
          ndec(12)=2; vmin(12)=1.0; vmax(12)=60.0; vdef(12)=5.0
          menu(13)='Minimum cooling OFF minutes'
          prompt1(13)='Minimum OFF minutes in cooling cycle'
          prompt2(13)=' '
          ndec(13)=2; vmin(13)=1.0; vmax(13)=60.0; vdef(13)=5.0
        elseif(iclaw.eq.14)then
          helptopic='sys_bctl_14'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=15    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)='Heating set-point (C)'
          prompt1(2)='Heating set-point (deg.C) (remember there is'
          prompt2(2)='a deadband as well) '
          ndec(2)=3; vmin(2)=0.0; vmax(2)=100.0; vdef(2)=20.0
          menu(3)='Heating deadband (C)'
          prompt1(3)='Heating deadband (C) (suggest that heating'
          prompt2(3)='and cooling T bands do not cross) '
          ndec(3)=3; vmin(3)=0.0; vmax(3)=10.0; vdef(3)=2.0
          menu(4)='Heat shut switch differential C'
          prompt1(4)='heating shut switch differential (C) '
          prompt2(4)=' '
          ndec(4)=3; vmin(4)=0.0; vmax(4)=10.0; vdef(4)=1.0
          menu(5)='Heat open switch differential C'
          prompt1(5)='heat open switch differential (C)'
          prompt2(5)=' '
          ndec(5)=3; vmin(5)=0.0; vmax(5)=10.0; vdef(5)=1.0
          menu(6)='Cooling set-point (C)'
          prompt1(6)='Cooling set-point (deg.C) (remember there is'
          prompt2(6)='a deadband as well) '
          ndec(6)=3; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=24.0
          menu(7)='Cooling deadband (C) '
          prompt1(7)='Cooling deadband (deg.C) (suggest that heating'
          prompt2(7)='and cooling T bands do not cross) '
          ndec(7)=3; vmin(7)=0.0; vmax(7)=10.0; vdef(7)=2.0
          menu(8)='Cool open switch differential C'
          prompt1(8)='cooling open switch differential (C)'
          prompt2(8)=' '
          ndec(8)=3; vmin(8)=0.0; vmax(8)=10.0; vdef(8)=1.0
          menu(9)='Cool shut switch differential C'
          prompt1(9)='Cooling shut switch differential (C)'
          prompt2(9)=' '
          ndec(9)=3; vmin(9)=0.0; vmax(9)=10.0; vdef(9)=1.0
          menu(10)='Maximum heating flux (W)'
          prompt1(10)='Maximum heating flux (W)'
          prompt2(10)=' '
          ndec(10)=1; vmin(10)=0.0; vmax(10)=999999.0; vdef(10)=1000.0
          menu(11)='Minimum heating flux (W) '
          prompt1(11)='Minimum heating flux (W) (typically zero and'
          prompt2(11)='can be non-zero for uninsulated pipes etc.)'
          ndec(11)=1; vmin(11)=0.0; vmax(11)=999999.0; vdef(11)=0.0
          menu(12)='Rate of change heater actuator'
          prompt1(12)='Rate of change of heater actuator (in building'
          prompt2(12)='timesteps) moving from one state to another'
          ndec(12)=0; vmin(12)=0.0; vmax(12)=10.0; vdef(12)=1.0
          menu(13)='Maximum cooling flux (W) '
          prompt1(13)='Maximum cooling flux (W) '
          prompt2(13)=' '
          ndec(13)=1; vmin(13)=0.0; vmax(13)=999999.0; vdef(13)=1000.0
          menu(14)='Minimum cooling flux (W) '
          prompt1(14)='Minimum cooling flux (W) (typically zero and'
          prompt2(14)='can be non-zero for uninsulated pipes etc.)'
          ndec(14)=1; vmin(14)=0.0; vmax(14)=999999.0; vdef(14)=0.0
          menu(15)='Rate of change cooling actuator'
          prompt1(15)='Rate of change of cooling actuator (in building'
          prompt2(15)='timesteps) moving from one state to another.'
          ndec(15)=0; vmin(15)=0.0; vmax(15)=10.0; vdef(15)=1.0
        elseif(iclaw.eq.15)then
          helptopic='sys_bctl_15'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)='Heating capacity (W) '
          prompt1(2)='Heating capacity (W) available during the '
          prompt2(2)='startup period. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Heating setpoint (desired) (C) '
          prompt1(3)='Heating setpoint (C) which we desire to '
          prompt2(3)='reach by the desired time-of-arrival '
          ndec(3)=3; vmin(3)=0.0; vmax(3)=100.0; vdef(3)=20.0
          menu(4)='Temperature differential (C) '
          prompt1(4)='Temperature differential (C) to be applied to'
          prompt2(4)='the setpoint to create an acceptable band'
          ndec(4)=3; vmin(4)=0.0; vmax(4)=10.0; vdef(4)=1.0
          menu(5)='Desired time-of-arrival (hours) '
          prompt1(5)='Desired time-of-arrival (hours) when the '
          prompt2(5)='temperatrure should be within the accept band.'
          ndec(5)=2; vmin(5)=0.0; vmax(5)=24.0; vdef(5)=8.0
          menu(6)='Minimum time difference (hours) '
          prompt1(6)='Minimum time difference (hours) to try before '
          prompt2(6)='or after guess. '
          ndec(6)=3; vmin(6)=0.1; vmax(6)=2.0; vdef(6)=0.2
          if(nint(cm(7)).eq.1)then
            nmenulines=8    ! set to the last cm index used
            menu(7)='Start >> Default of 4.00 a.m.'
            prompt1(7)='Default of 4.00 a.m. (value one), User start'
            prompt2(7)='(value two) and Birtles (value three) '
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)=' '
            prompt1(8)=' '
            prompt2(8)=' '
            ndec(8)=-1; vmin(8)=0.0; vmax(8)=0.0; vdef(8)=0.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=0.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
          elseif(nint(cm(7)).eq.2)then
            nmenulines=8    ! set to the last cm index used
            menu(7)='Start >> User defined start'
            prompt1(7)='User start (value two ),4.00 a.m. (value one)'
            prompt2(7)='and Birtles (value three) '
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)='Initial guess for start (hr)'
            prompt1(8)='Initial guess for start time (hr)'
            prompt2(8)=' '
            ndec(8)=3; vmin(8)=0.0; vmax(8)=24.0; vdef(8)=4.0
            menu(9)=' '
            prompt1(9)=' '
            prompt2(9)=' '
            ndec(9)=-1; vmin(9)=0.0; vmax(9)=0.0; vdef(9)=0.0
            menu(10)=' '
            prompt1(10)=' '
            prompt2(10)=' '
            ndec(10)=-1; vmin(10)=0.0; vmax(10)=0.0; vdef(10)=0.0
          elseif(nint(cm(7)).eq.3)then
            nmenulines=10    ! set to the last cm index used
            menu(7)='Start >> Birtles & John Eqn.'
            prompt1(7)='Birtles & John Eqn. (value three),4.00 a.m.'
            prompt2(7)='(value one), User start (value two)'
            ndec(7)=0; vmin(7)=0.0; vmax(7)=3.0; vdef(7)=1.0
            menu(8)='Birtles & John Eqn. heating coef'
            prompt1(8)='Birtles & John Eqn. heating coef'
            prompt2(8)='(see the referenced paper) '
            ndec(8)=2; vmin(8)=0.0; vmax(8)=99999.0; vdef(8)=1.0
            menu(9)='Birtles & John Eqn. build coef'
            prompt1(9)='Birtles & John Eqn. build coef'
            prompt2(9)='(see the referenced paper) '
            ndec(9)=2; vmin(9)=0.0; vmax(9)=99999.0; vdef(9)=1.0
            menu(10)='Birtles & John Eqn. build coef'
            prompt1(10)='Birtles & John Eqn. building coef'
            prompt2(10)='(see the referenced paper) '
            ndec(10)=2; vmin(10)=0.0; vmax(10)=99999.0; vdef(10)=1.0
          endif
        elseif(iclaw.eq.16)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.17)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.18)then
          menu(1)='Null control has no data.'
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=0; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
        elseif(iclaw.eq.19)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.20)then
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          continue
        elseif(iclaw.eq.21)then
          helptopic='sys_bctl_21'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=4    ! set to the last cm index used
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Index of master control loop'
          prompt1(2)='Index of master control loop (this loop should'
          prompt2(2)='already exist) '
          ndec(2)=0; vmin(2)=1.0; vmax(2)=100.0; vdef(2)=1.0
          menu(3)='Max slave heating capacity (W)'
          prompt1(3)='Maximum slave heating capacity (W) (for when'
          prompt2(3)='the master control is at 100%) '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=1000.0
          menu(4)='Max slave cooling capacity (W) '
          prompt1(4)='Maximum slave cooling capacity (W) (for when'
          prompt2(4)='the master control is at 100%)'
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
        elseif(iclaw.eq.22)then

C Variable supply volume cooling control which reverts to a CAV system
C using reheat for heating mode.
          helptopic='sys_bctl_22'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=6    ! set to the last cm index used.
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Reheat maximum capacity (W)'
          prompt1(2)='Reheat maximum capacity (W) is injected into '
          prompt2(2)='the CAV supply stream. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=99999.0; vdef(2)=1000.0
          menu(3)='Supply air temperature (C)'
          prompt1(3)='Supply air temperature (C)'
          prompt2(3)=' '
          ndec(3)=3; vmin(3)=-10.0; vmax(3)=100.0; vdef(3)=12.0
          menu(4)='Desired room air temp (C)'
          prompt1(4)='Desired room air temperature (C)'
          prompt2(4)=' '
          ndec(4)=3; vmin(4)=-10.0; vmax(4)=100.0; vdef(4)=20.0
          menu(5)='Air supply flow max rate (m^3/s)'
          prompt1(5)='Air supply flow maximum rate (m^3/sec) '
          prompt2(5)=' '
          ndec(5)=4; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=1.0
          menu(6)='Air supply flow min rate (m^3/s)'
          prompt1(6)='Air supply flow minimum rate (m^3/sec) '
          prompt2(6)=' '
          ndec(6)=4; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=0.5
        elseif(iclaw.eq.23)then

C Reading heating and cooling setpoints from temporal file
          helptopic='sys_bctl_23'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=5.0
          menu(2)='Heating setpoint column number'
          prompt1(2)='Set the column number which contains heating'
          prompt2(2)='setpoint value in the temporal (tdf) file'
          ndec(2)=0; vmin(2)=1.0; vmax(2)=80.0; vdef(2)=5.0
          menu(3)='Cooling setpoint column number'
          prompt1(3)='Set the column number which contains cooling'
          prompt2(3)='setpoint value in the temporal (tdf) file'
          ndec(3)=0; vmin(3)=1.0; vmax(3)=80.0; vdef(3)=6.0
          nmenulines=3    ! set to the last cm index used
        elseif(iclaw.eq.24)then

C Adaptive human comfort model. A comfort temperature is calculated
C based on running mean ambient temperature. If the environment
C temperature of the controlled zone is now within 2 degrees of this
C comfort temperature the control is activated
          helptopic='sys_bctl_24'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Maximum heating capacity (W)'
          prompt1(2)='Maximum heating capacity (W) which is available'
          prompt2(2)='to meet the demand. '
          ndec(2)=1; vmin(2)=0.0; vmax(2)=999999.0; vdef(2)=1000.0
          menu(3)='Minimum heating capacity (W)'
          prompt1(3)='Minimum heating capacity (W) The flux injected'
          prompt2(3)='will never drop below this level. '
          ndec(3)=1; vmin(3)=0.0; vmax(3)=999999.0; vdef(3)=0.0
          menu(4)='Maximum cooling capacity (W)'
          prompt1(4)='Maximum cooling capacity (W) which is available'
          prompt2(4)='to meet the demand. '
          ndec(4)=1; vmin(4)=0.0; vmax(4)=999999.0; vdef(4)=1000.0
          menu(5)='Minimum cooling capacity (W)'
          prompt1(5)='Minimum cooling capacity (W) The flux extracted'
          prompt2(5)='will never be less than this. '
          ndec(5)=1; vmin(5)=0.0; vmax(5)=999999.0; vdef(5)=0.0
          menu(6)='Heating setpoint (C)'
          prompt1(6)='Heating setpoint (C) (temperature to meet if'
          prompt2(6)='there is sufficient capacity) '
          ndec(6)=3; vmin(6)=-100.0; vmax(6)=100.0; vdef(6)=20.0
          menu(7)='Cooling setpoint (C)'
          prompt1(7)='Cooling setpoint (C (temperature to meet if)'
          prompt2(7)='there is sufficient capacity) '
          ndec(7)=3; vmin(7)=-100.0; vmax(7)=100.0; vdef(7)=24.0
          menu(8)='Running mean response parameter'
          prompt1(8)='Running mean response parameter'
          prompt2(8)=' '
          ndec(8)=3; vmin(8)=0.0; vmax(8)=0.99; vdef(8)=0.8
          menu(9)='Comfort band '
          prompt1(9)='Comfort band (default is 2deg) '
          prompt2(9)=' '
          ndec(9)=3; vmin(8)=0.0; vmax(8)=0.99; vdef(8)=2.0
          nmenulines=9    ! set to the last cm index used
        else
          helptopic='sys_bctl_other'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          nmenulines=7    ! set to the last cm index used
          menu(1)=' '
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=2; vmin(1)=0.0; vmax(1)=100.0; vdef(1)=0.0
          menu(2)=' '
          prompt1(2)=' '
          prompt2(2)=' '
          ndec(2)=2; vmin(2)=0.0; vmax(2)=100.0; vdef(2)=0.0
          menu(3)=' '
          prompt1(3)=' '
          prompt2(3)=' '
          ndec(3)=2; vmin(3)=0.0; vmax(3)=100.0; vdef(3)=0.0
          menu(4)=' '
          prompt1(4)=' '
          prompt2(4)=' '
          ndec(4)=2; vmin(4)=0.0; vmax(4)=100.0; vdef(4)=0.0
          menu(5)=' '
          prompt1(5)=' '
          prompt2(5)=' '
          ndec(5)=2; vmin(5)=0.0; vmax(5)=100.0; vdef(5)=0.0
          menu(6)=' '
          prompt1(6)=' '
          prompt2(6)=' '
          ndec(6)=2; vmin(6)=0.0; vmax(6)=100.0; vdef(6)=0.0
          menu(7)=' '
          prompt1(7)=' '
          prompt2(7)=' '
          ndec(7)=2; vmin(7)=0.0; vmax(7)=100.0; vdef(7)=0.0
          menu(8)=' '
          prompt1(8)=' '
          prompt2(8)=' '
          ndec(8)=2; vmin(8)=0.0; vmax(8)=100.0; vdef(8)=0.0
          menu(9)=' '
          prompt1(9)=' '
          prompt2(9)=' '
          ndec(9)=2; vmin(9)=0.0; vmax(9)=100.0; vdef(9)=0.0
          menu(10)=' '
          prompt1(10)=' '
          prompt2(10)=' '
          ndec(10)=2; vmin(10)=0.0; vmax(10)=100.0; vdef(10)=0.0
          menu(11)=' '
          prompt1(11)=' '
          prompt2(11)=' '
          ndec(11)=2; vmin(11)=0.0; vmax(11)=100.0; vdef(11)=0.0
          menu(12)=' '
          prompt1(12)=' '
          prompt2(12)=' '
          ndec(12)=2; vmin(12)=0.0; vmax(12)=100.0; vdef(12)=0.0
          menu(13)=' '
          prompt1(13)=' '
          prompt2(13)=' '
          ndec(13)=2; vmin(13)=0.0; vmax(13)=100.0; vdef(13)=0.0
        endif
      elseif(icfoc.eq.1)then
        continue
      elseif(icfoc.eq.2)then
        continue
      elseif(icfoc.eq.3)then
        continue
      elseif(icfoc.eq.4)then
        continue
      elseif(icfoc.eq.5)then
        continue
      elseif(icfoc.eq.6)then

C Set first item of menu (this item holds number of supplementary data
C items for the loop and should not be edited
        menu(1)='Choose parameter to edit:'
        if(iclaw.ge.1)then

C Set up the help text for the control period menu as well as the
C individual items to be edited (in one big block).
          helptopic='sys_bctl_CFC'
          call gethelptext(helpinsub,helptopic,nbhelp)
          nctlhelp=nbhelp
          
C Create prompts, min/max values for complex fen. data based on control type
        
         if(ictyp.eq.1)then     ! senses temp., actuates shade ON/OFF
          nmenulines=3
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Shade ON (close) setpoint (C)'
          prompt1(2)='Temperature (C) above which shade is deployed'
          prompt2(2)='or slat angle is closed.'
          ndec(2)=1; vmin(2)=-100.0; vmax(2)=100.0; vdef(2)=25.0
          menu(3)='Shade OFF (open) setpoint (C)'
          prompt1(3)='Temperature (C) below which shade is retracted'
          prompt2(3)='or slat angle is opened.'
          ndec(3)=1; vmin(3)=-100.0; vmax(3)=100.0; vdef(3)=20.0
         elseif(ictyp.eq.2)then ! senses temp., actuates slat angle
          nmenulines=5
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Shade ON (close) setpoint (C)'
          prompt1(2)='Temperature (C) above which shade is deployed'
          prompt2(2)='or slat angle is closed.'
          ndec(2)=1; vmin(2)=-100.0; vmax(2)=100.0; vdef(2)=25.0
          menu(3)='Shade OFF (open) setpoint (C)'
          prompt1(3)='Temperature (C) below which shade is retracted'
          prompt2(3)='or slat angle is opened.'
          ndec(3)=1; vmin(3)=-100.0; vmax(3)=100.0; vdef(3)=20.0
          menu(4)='Slat angle (close) (deg)'
          prompt1(4)='Slat angle (deg) for shade ON '
          prompt2(4)='or closed condition. '
          ndec(4)=1; vmin(4)=-89.9; vmax(4)=89.9; vdef(4)=60.0
          menu(5)='Slat angle (open) (deg)'
          prompt1(5)='Slat angle (deg) for shade OFF '
          prompt2(5)='or open position.'
          ndec(5)=1; vmin(5)=-89.9; vmax(5)=89.9; vdef(5)=0.0
         elseif(ictyp.eq.3)then ! senses solar radiation, actuates shade ON/OFF
          nmenulines=3
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Shade ON (close) setpoint (W/m2)'
          prompt1(2)='Solar radiation (W/m2) above which'
          prompt2(2)='shade is deployed or slat angle is closed.'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=1200.0; vdef(2)=600.0
          menu(3)='Shade OFF (open) setpoint (W/m2)'
          prompt1(3)='Solar radiation (W/m2) below which shade is'
          prompt2(3)='retracted or slat angle is opened.'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=1200.0; vdef(3)=400.0
         elseif(ictyp.eq.4)then ! senses solar radiation, actuates slat angle
          nmenulines=5
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Shade ON (close) setpoint (W/m2)'
          prompt1(2)='Solar radiation (W/m2) above which'
          prompt2(2)='shade is deployed or slat angle is closed.'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=1200.0; vdef(2)=600.0
          menu(3)='Shade OFF (open) setpoint (W/m2)'
          prompt1(3)='Solar radiation (W/m2) below which shade is'
          prompt2(3)='retracted or slat angle is opened.'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=1200.0; vdef(3)=400.0  
          menu(4)='Slat angle (close) (deg)'
          prompt1(4)='Slat angle (deg) for shade ON '
          prompt2(4)='or closed condition. '
          ndec(4)=1; vmin(4)=-89.9; vmax(4)=89.9; vdef(4)=60.0
          menu(5)='Slat angle (open) (deg)'
          prompt1(5)='Slat angle (deg) for shade OFF '
          prompt2(5)='or open position.'
          ndec(5)=1; vmin(5)=-89.9; vmax(5)=89.9; vdef(5)=0.0          
         elseif(ictyp.eq.5)then ! senses wind speed, actuates shade ON/OFF
          nmenulines=3
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Shade ON (close) setpoint (m/s)'
          prompt1(2)='Wind speed (m/s) above which'
          prompt2(2)='shade is deployed.'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=100.0; vdef(2)=10.0
          menu(3)='Shade OFF (open) setpoint (m/s)'
          prompt1(3)='Wind speed (m/s) below which shade'
          prompt2(3)='is retracted.'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=100.0; vdef(3)=5.0    
         elseif(ictyp.eq.6)then ! senses wind direction, actuates shade ON/OFF
          nmenulines=3
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='Start angle for shade OFF (deg)'
          prompt1(2)='Start angle (deg) beyond which'
          prompt2(2)='shade is retracted.'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=360.0; vdef(2)=180.0
          menu(3)='End angle for shade OFF (deg)'
          prompt1(3)='End angle (deg) below which shade'
          prompt2(3)='is retracted.'
          ndec(3)=1; vmin(3)=0.0; vmax(3)=360.0; vdef(3)=270.0              
         elseif(ictyp.eq.7)then ! no control, schedule only
          nmenulines=3
          prompt1(1)=' '
          prompt2(1)=' '
          ndec(1)=-1; vmin(1)=0.0; vmax(1)=0.0; vdef(1)=0.0
          menu(2)='0 for shade OFF, 1 for shade ON'
          prompt1(2)='Flag for shade ON/OFF (1/0)'
          prompt2(2)='for current period.'
          ndec(2)=1; vmin(2)=0.0; vmax(2)=1.0; vdef(2)=1.0
          menu(3)='Slat angle (deg)'
          prompt1(3)='Slat angle for current period (deg).'
          prompt2(3)=''
          ndec(3)=1; vmin(3)=-89.9; vmax(3)=89.9; vdef(3)=45.0        
         endif  
       
        endif
      endif  
      return
      end

C ********************************************************************
C                      --check_CFC_control_validity--
C
C Created by: Bart Lomanowski
C Initial Creation Date: June 2009
C
C This subroutine checks complex fenestration control data for
C validity and ensures that sensor/actuator/control type/control law
C data have been assembled correctly. 
C
C Reference: data structure of CFC control variables is documented
C            in econtrol.F 
C
C OUTPUTS:
C       checkOK - return status used in subroutine CONTRL
C ********************************************************************      
      subroutine check_CFC_control_validity(checkOK)
      IMPLICIT NONE
#include "building.h"
#include "control.h"

C-----Working variables
      integer ndaytypes
      integer iact1, iact2, iact3
      integer isen1, isen2, isen3
      integer ictype, ilaw
C-----Loop indices
      integer ictlfun,idaytype,iperiod

      logical checkOK
      character msgout*124,ctype*44
      
      checkOK  = .false.

C -------------------------------------------------------------------
C Loop through control functions, daytypes and periods
C -------------------------------------------------------------------      
      if(nCFCctlloops.gt.0)then
        do 10 ictlfun=1, nCFCctlloops
        
          ndaytypes = nCFCctldaytypes(ictlfun)
C---------if ndaytypes is 0 we have weekday, saturday, sunday types. Set daytypes = 3
          if(ndaytypes.eq.0)ndaytypes = 3
          
          do 20 idaytype=1, ndaytypes
            do 30 iperiod=1, nCFCdayctlperiods(ictlfun,idaytype)
        
              iact1 = iCFCactuator(ictlfun,1)           ! 0=shade ON/OFF, 1=slat angle, 2=both (for schedule
              iact2 = iCFCactuator(ictlfun,2)           ! zone index
              iact3 = iCFCactuator(ictlfun,3)           ! CFC type

              isen1 = iCFCsensor(ictlfun,1)
              isen2 = iCFCsensor(ictlfun,2)
              isen3 = iCFCsensor(ictlfun,3)
        
              ictype = iCFCctltype(ictlfun,idaytype,iperiod)
              ilaw   = iCFCctllaw(ictlfun,idaytype,iperiod)        
              
C -------------------------------------------------------------------
C Check for control data validity.
C -------------------------------------------------------------------        
                if(ictype.eq.1)then        ! senses temperature, actuates shade ON/OFF
                  if((isen1.eq.-3.and.(isen2.eq.0.or.isen2.eq.1))
     &                .or.isen1.eq.-2.or.isen1.gt.0)then
                    if(iact1.eq.0.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif
                elseif(ictype.eq.2)then    ! senses temperature, actuates slat angle
                  if((isen1.eq.-3.and.(isen2.eq.0.or.isen2.eq.1))
     &                            .or.isen1.eq.-2.or.isen1.gt.0)then
                    if(iact1.eq.1.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                
                elseif(ictype.eq.3)then    ! senses solar radiation, actuates shade ON/OFF
                  if((isen1.eq.-3.and.(isen2.eq.4.or.isen2.eq.5))
     &                            .or.isen1.eq.-4)then               
                    if(iact1.eq.0.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                      
                elseif(ictype.eq.4)then    ! senses solar radiation, actuates slat angle
                  if((isen1.eq.-3.and.(isen2.eq.4.or.isen2.eq.5))
     &                            .or.isen1.eq.-4)then               
                    if(iact1.eq.1.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                  
                elseif(ictype.eq.5)then    ! senses wind speed, actuates shade ON/OFF
                  if(isen1.eq.-3.and.isen2.eq.2)then               
                    if(iact1.eq.0.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                 
                elseif(ictype.eq.6)then    ! senses wind direction, actuates shade ON/OFF
                  if(isen1.eq.-3.and.isen2.eq.3)then               
                    if(iact1.eq.0.and.ilaw.eq.1)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                 
                elseif(ictype.eq.7)then    ! schedule only
                  if(isen1.eq.0.and.isen2.eq.0.and.isen3.eq.0)then               
                    if(iact1.eq.2.and.ilaw.eq.2)then
                      checkOK=.true.
                    else
                      checkOK=.false.
                    endif
                  else
                    checkOK=.false.
                  endif                
                else
                  checkOK=.false.
                endif            

                if(.not.checkOK)then
C ----------------Construct user message            
                  if(ictype.eq.1)then
                    write(ctype,'(A)')
     &               ' senses temp. > actuates shade ON/OFF'
                  elseif(ictype.eq.2)then
                    write(ctype,'(A)')
     &               ' senses temp. > actuates slat angle'                  
                  elseif(ictype.eq.3)then
                    write(ctype,'(A)')
     &               ' senses sol-rad > actuates shade ON/OFF'                  
                  elseif(ictype.eq.4)then
                    write(ctype,'(A)')
     &               ' senses sol-rad > actuates slat angle'                  
                  elseif(ictype.eq.5)then
                    write(ctype,'(A)')
     &               ' senses wind speed > actuates shade ON/OFF'                  
                  elseif(ictype.eq.6)then
                    write(ctype,'(A)')
     &               ' senses wind dir. > actuates shade ON/OFF'                   
                  elseif(ictype.eq.7)then
                    write(ctype,'(A)')
     &               ' no sensor > schedule only'                 
                  else
                    write(ctype,'(A)')
     &               ' UNKNOWN'
                  endif
                  
                  write(msgout,'(A,A,A,i4)')
     &               'Control type:', ctype, ' - in function', ictlfun
     
                  call USRMSG(msgout,
     &   'does not match sensor/actuator/control law selection.','W')
                  return                
                endif
                
 30         continue     
 20       continue     
 10     continue

      else
        checkOK=.true.        
      endif
      
      return
      end
      
