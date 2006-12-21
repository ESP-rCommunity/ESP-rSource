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

C The file edtopol.f is a collection of support facilities for 
C creating and editing the model topology ie. connection lists:
C  EDCONN: Edit connection attributes in common block C3.
C  CONXMENU: Returns an English description of the inter-connection 
C            information.
C  anchor:   anchor point items (a list of connections & concept).
C  anchlist: manage list of anchors
C  easkconn: Select one or more connections from a list.
C  PROCGDST process GDST command sets (defined in common blocks
C           idsta and anchora.
C  Serchrpl handles search and replace of composition attributes.

C ************* EDCONN 
C Edit connection attributes in common block __ via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDCONN(IER)
#include "building.h"
#include "esprdbfile.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      common/appw/iappw,iappx,iappy

      LOGICAL concat,prob,dup,OK,dok,unixok

      DIMENSION VERT(6)
      CHARACTER VERT*34,CXSTR*78,H*72,outs*124
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LCFGF
      character DFILE*72,ltmp*72,longtfile*144,longtfiledos*144
      character doit*248,tmode*8,tfile*72,MODE*4,cfgroot*24

C Check if Unix-based or DOS based.
      call isunix(unixok)

C On first entry, see if the list is consistent with geometry.
  90  call usrmsg(
     &  'Checking zone surfaces against master list...',' ','-')
      call ckcurmatch(prob,iprob,ier)
      if(prob)then
        call tstamp('>','PRJ: found topology incinsistent')
        if(IPROB.gt.10)then
          write(outs,*) IPROB,' inconsistencies found!'
          call edisp(iuout,outs)
          call usrmsg(
     &    'Topology list dangerously inconsistent. Attempting to',
     &    'recover by checking against zone information...','W')
          IW=1
        else
          call usrmsg(
     &    'Topology list inconsistent. Either clear current',
     &    'topology and/or use topology tool to resolve this.','W')
          H(1)='Clearing resets all connections to `outside`.'
          H(2)='The topology tool will look for geometric matches'
          H(3)='as well as letting you specify contiguity. '
          CALL EASKABC('Choices: ',' ','Clear ALL contiguity ',
     &      'Use topology tool','continue',IW,3)
        endif
        if(IW.eq.1)then
          call tstamp('>','PRJ: do topology clear')
          ICC=0
          do 87 IZ=1,NCOMP
            CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,iuout,IER)
            nzsur(iz)=nsur
            do 88 IS=1,NSUR
              ICC=ICC+1
              IC1(ICC)=IZ
              IE1(ICC)=IS
              ICT(ICC)=0
              IC2(ICC)=0
              IE2(ICC)=0
   88       continue
   87     continue
          NCON=ICC
          write(outs,'(a,i4)')'No. of connections now = ',NCON
          call edisp(iuout,outs)

C Save cleared data to configuration file and then offer topology tool.
          call EMKCFG('-',IER)
          goto 44
        elseif(IW.eq.2)then
          goto 44
        elseif(IW.eq.3)then
          goto 43
        endif
      else
        call usrmsg(
     &    'Checking zone surfaces against master list...done.',' ','-')
      endif

C Set for redraw if image on first entry (if refresh set to after
C each edit). Set flag for updating geometry file (for changed
C surface attribute.
  43  write(VERT(1),'(a,i4,a)')' connections (',ncon,')'
      VERT(2)  ='a topology tool (update/check)    '
      VERT(3)  ='b anchor points - define & tag    '
      VERT(4)  ='! list model topology             '
      VERT(5)  ='? help                            '
      VERT(6)  ='- exit this menu                  '
      mvert=6
      ivert=-3

C Help text for this menu.
      H(1)='The model contiguity ie. the boundary conditions'
      H(2)='at surfaces are set in this menu.  Before a simulation'
      H(3)='can be performed it is necessary that it be complete.'
      H(4) =' '
      H(5) ='There are several means to define the topology: '
      H(6) =' a) attribute surfaces via the zone geometry facility'
      H(7) ='    as they are created,'
      H(8) ='    '
      H(9) =' b) manually edit them via the `cfg` facility, '
      H(10) ='    '
      H(11) =' c) search for matching vertices within the model'
      H(12)='    to fill in all partitions and then complete this'
      H(13)='    list by editing or importing surface attributes.'
      H(14) =' '
      H(15)='The automated facility is considerably faster than'
      H(16)='manual methods and best used when all the zones have'
      H(17)='been created. '

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('  Zone Boundary Conditions',VERT,MVERT,IVERT)
      else
        CALL EMENU('Topology',VERT,MVERT,IVERT)
      endif
      IF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help.
        CALL PHELPD('connection section',17,'surf_topo   ',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List connections.
        call tstamp('>','PRJ: list topology')
        WRITE(outs,'(A,I3)')' Number of connections = ',NCON
        CALL EDISP(iuout,' ')
        CALL EDISP(iuout,outs)
        CALL CONXINFO(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        DO 21 ICON=1,NCON
          CALL CONXINFO(1,ICON,CXSTR)
          CALL EDISP(iuout,CXSTR)
   21   CONTINUE
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Set and tag anchor points.
        call anchlist(iasel,ier)
      ELSEIF(IVERT.EQ.(MVERT-4))THEN
        goto 44
      ELSE

C Not one of the legal menu choices.
        goto 43
      ENDIF
      IVERT=-2
      goto 43

C Update via vertex check.
  44  continue
      H(1)='This facility is provided via the topology program'
      H(2)='cfg which reads the current model and then scans the'
      H(3)='model for matching (adjacent) surfaces as well as'
      H(4)='allowing you to clear some or all of the surface'
      H(5)='topology/boundary conditions. '
      dok=.true.
      CALL ASKOK('Proceed with topology tool?','(see help)',OK,dok,5)
      IF(OK)then

C Get logical name of terminal type, expand model name to include
C the path and create a string to drive the remote application.
        call tstamp('>','PRJ: start topology tool')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        if(unixok)then
          call addpath(LCFGF,longtfile,concat)
        else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
          call addpath(LCFGF,longtfile,concat)
          call cmdfiledos(longtfile,longtfiledos,ier)
          longtfile=' '
          longtfile=longtfiledos
        endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
        if(iappw.gt.0.and.iappw.le.200)then
          write(doit,'(3a,3i4,3a)') 'cfg -mode ',tmode,' -s ',
     &    iappw,iappx-20,iappy+50,' -file ',
     &    longtfile(1:lnblnk(longtfile)),' &'
        else
          write(doit,'(5a)') 'cfg -mode ',tmode,
     &      ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' &'
        endif
        call usrmsg('starting topology module via',doit,'-')
        call runit(doit,tmode)

C Update configuraton.
        H(1)='REMINDER: the utility program cfg will have created'
        H(2)='an revised model configuration/ description file'
        H(3)='which does not yet contain information on temporal'
        H(4)='databases.  Edit this file if necessary before'
        H(5)='requesting prj read it in to update the model. '
        H(6)='(Only by re-reading it will the revised topology'
        H(7)='be know).'
        dok=.true.
        CALL ASKOK('Read in the revised configuration?','(see help)',
     &    OK,dok,7)
        IF(OK)then
          H(1)='The revised model description file name'
          H(2)='resulting from the topology tool.'
          DFILE=' '
          ltmp=LCFGF
  188     CALL EASKS(ltmp,' Updated configuration file? ',
     &      ' ',72,DFILE,'updated config file',IER,2)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LCFGF=ltmp
          else
            goto 188
          endif
          MODE='NONE'
          IAPROB=IPRODB
          CALL ERSYS(LCFGF,IFCFG,IAPROB,MODE,0,IER)
          IF(IER.EQ.1)THEN
            dok=.true.
            h(1)='While scanning the model configuration file an error'
            h(2)='was detected. Review any other messages presented to'
            h(3)='see if they might be corrected. '
            CALL ASKOK(' ','Problem reading model. Try again?',OK,dok,3)
            IF(OK)GOTO 188
          ENDIF
          NZONES=NCOMP
          CALL ZDATA (ITRC,IER,NZONES,ISFSUM)
          goto 90
        endif
      endif
      goto 43

      END 

C **************** CONXMENU 
C CONXMENU returns an English description in CXITM of the inter-connection 
C information. 
      SUBROUTINE CONXMENU(ICON,CXITM)
#include "building.h"
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      CHARACTER CXITM*43,SST1*12,ZST1*12,SST2*12,ZST2*12,zdesc*64
      CHARACTER zname*12,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4

      CXITM=' '
      SST1=SNAME(IC1(ICON),IE1(ICON))
      lsn1=MIN0(lnblnk(SST1),8)
      lsn10=MIN0(lnblnk(SST1),10)
      ZST1=zname(IC1(ICON))
      lzn1=MIN0(lnblnk(ZST1),8)
      lzn10=MIN0(lnblnk(ZST1),10)
      IF(ICT(ICON).EQ.0)THEN
        if((lsn10+lzn10+18).lt.43)then
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< External'
        else
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< External'
        endif
      ELSEIF(ICT(ICON).EQ.1)THEN
        if((lsn10+lzn10+19).lt.43)then
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< Identical'
        else
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< Identical'
        endif
      ELSEIF(ICT(ICON).EQ.2)THEN
        if((lsn10+lzn10+25).lt.43)then
          WRITE(CXITM,'(I3,1x,4a,I3,a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< Constant @',IC2(ICON),'dC'
        else
          WRITE(CXITM,'(I3,1x,4a,I3,a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< Constant @',IC2(ICON),'dC'
        endif
      ELSEIF(ICT(ICON).EQ.3)THEN
        SST2=SNAME(IC2(ICON),IE2(ICON))
        lsn2=MIN0(lnblnk(SST2),8)
        lsn21=MIN0(lnblnk(SST2),10)
        ZST2=zname(IC2(ICON))
        lzn2=MIN0(lnblnk(ZST2),8)
        lzn21=MIN0(lnblnk(ZST2),10)
        if((lsn10+lzn10+lsn21+lzn21+11).lt.43)then
          WRITE(CXITM,'(I3,1x,7a)')ICON,SST1(1:lsn10),':',
     &          ZST1(1:lzn10),' >|< ',SST2(1:lsn21),':',ZST2(:lzn21)
        elseif((lsn1+lzn10+lsn2+lzn21+11).lt.43)then
          WRITE(CXITM,'(I3,1x,7a)')ICON,SST1(1:lsn1),':',
     &          ZST1(1:lzn10),' >|< ',SST2(1:lsn2),':',ZST2(1:lzn21)
        else
          WRITE(CXITM,'(I3,1x,7a)')ICON,SST1(1:lsn1),':',
     &          ZST1(1:lzn1),' >|< ',SST2(1:lsn2),':',ZST2(1:lzn2)
        endif
      ELSEIF(ICT(ICON).EQ.4)THEN
        IF(IC2(ICON).GT.0)THEN
          if((lsn10+lzn10+16).lt.43)then
            WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &        ZST1(1:lzn10),' >|< ground'
          else
            WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &        ZST1(1:lzn1),' >|< ground'
          endif
        ELSE
          if((lsn10+lzn10+23).lt.43)then
            WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &        ZST1(1:lzn10),' >|< user def grnd'
          else
            WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &        ZST1(1:lzn1),' >|< user def grnd'
          endif
        ENDIF
      ELSEIF(ICT(ICON).EQ.5)THEN
        if((lsn10+lzn10+19).lt.43)then
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< adiabatic'
        else
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< adiabatic'
        endif
      ELSEIF(ICT(ICON).EQ.6)THEN

C BASESIMP begin.
        if((lsn10+lzn10+19).lt.43)then
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< BASESIMP'
        else
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< BASESIMP'
        endif
C BASESIMP end.

C CEN 13791 boundary condition start.
      ELSEIF(ICT(ICON).EQ.7)THEN
        if((lsn10+lzn10+19).lt.43)then
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn10),':',
     &      ZST1(1:lzn10),' >|< IDENT_CEN'
        else
          WRITE(CXITM,'(I3,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< IDENT_CEN'
        endif
C CEN 13791 boundary condition end.

      ENDIF

      RETURN
      END

C **************** anchor 
C anchor defines and instanciates anchor points (a list of connections
C associated with a concept).
      subroutine anchor(ifoc,ier)
#include "building.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)

C Anchor commons:
C NALOC = nb of anchors,
C ALOC = substitution attribute used with the the anchor (usually
C   passed from a command to a function which manipulates the anchor.),
C ALOCLBL = user's name for the anchor,
C ALOCTYP = type (SURF|CTRL|LOAD) for the anchor,
C IALOC = nb of links (surfaces, etc.) associated with the anchor,
C lstanchr = links for each anchor
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION VERT(35),flag(mcon),iconnpk(60)
      CHARACTER VERT*54,KEY*1,H*72,outs*124
      character CXITM*43,flag*9
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,AL*12
      logical found,ok,dok,foundone,okd,MODIFY,MODLEN,MODBND

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
  31  MHEAD=7
      MCTL=4
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      do 2 i = 1,ncon
        flag(i)= '      '
  2   continue

C Force redraw of zones, highlighting anchor list.
      MODIFY=.TRUE.
      nzg=NCOMP
      if(nzg.gt.0)then
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE
        CALL INLNST(1)
        do 45 jj = 1,IALOC(ifoc)
          kk=lstanchr(ifoc,jj)
          if(kk.ne.0)LINSTY(kk)=2
  45    continue
        CALL ADJVIEW(ITRC,IER)
      endif

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Help text.
   3  h(1)='Anchor points allow a user to line a number of items '
      h(2)='(such as surfaces) to a concept so that global actions'
      h(3)='(such as attribution or performance enquiries) can be '
      h(4)='undertaken. An achor point consists of name (12 char),'
      h(5)='a list of item indices and a type indicator. '
      h(6)=' '
      h(7)='Anchor types set the context within which esp-r can'
      h(8)='manipulate items associated with an anchor. Details: '
      h(9)=' a general surface - supports actions such as changing'
      h(10)='   attributes or graphing temperatures of a '
      h(11)='   collection of surfaces).'
      h(12)=' '
      h(13)='Tagging surfaces as possible openings or leakage paths'
      h(14)='in a flow network:'
      h(15)=' b closed door - surface acts as or includes a closed'
      h(16)='   door (or crack) between two flow nodes.'
      h(17)=' c open(able) door - surface acts as or includes an '
      h(18)='   open door (perhaps controlled) between flow nodes.'
      h(19)=' d closed window - surface acts as/includes a closed'
      h(20)='   window (or crack) between two flow nodes.'
      h(21)=' e open(able) window - surface acts as/includes a'
      h(22)='   window (perhaps controlled) between flow nodes.'
      h(23)=' f closed grill - surface acts as/includes a closed'
      h(24)='   grill (or crack) between two flow nodes.'
      h(25)=' g open(able) grill - surface includes an grill or'
      h(26)='   vent (perhaps controlled) between two flow nodes.'
      h(27)=' h duct or pipe - surface inclues an duct or pipe'
      h(28)='   between two flow nodes.'
      h(29)=' i fan or pump - surface near a fan or pump between'
      h(30)='   two flow nodes.'

C Set menu header text. Update flag string with * depending on which
C anchor set(s) is/are associated with a connection.
      write(VERT(1),'(a)') '  anchor data: '
      write(VERT(2),'(a)') '  ____________________________________'
      write(VERT(3),'(a)') '  index  label    type       nb. links'
      write(VERT(4),'(a,i2,5a,i2,a)') '1 ',ifoc,' ',ALOCLBL(ifoc),' ',
     &  ALOCTYP(ifoc),' (',IALOC(ifoc),' links)'
      VERT(5)='  ___________________________________________   '
      VERT(6)=' conn|  connection      |    connection     |123456789'
      VERT(7)=' no. | inside face      | other side data   |anchor'
      do 5 i=1,NALOC
        if(IALOC(i).gt.0)then
          do 4 j = 1,IALOC(i)
            k=lstanchr(i,j)
            if(k.ne.0.and.i.eq.1)write(flag(k)(1:1),'(a)') '*'
            if(k.ne.0.and.i.eq.2)write(flag(k)(2:2),'(a)') '*'
            if(k.ne.0.and.i.eq.3)write(flag(k)(3:3),'(a)') '*'
            if(k.ne.0.and.i.eq.4)write(flag(k)(4:4),'(a)') '*'
            if(k.ne.0.and.i.eq.5)write(flag(k)(5:5),'(a)') '*'
            if(k.ne.0.and.i.eq.6)write(flag(k)(6:6),'(a)') '*'
            if(k.ne.0.and.i.eq.7)write(flag(k)(7:7),'(a)') '*'
            if(k.ne.0.and.i.eq.8)write(flag(k)(8:8),'(a)') '*'
            if(k.ne.0.and.i.ge.9)write(flag(k)(9:9),'(a)') '*'
   4      continue
        endif
   5  continue

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          WRITE(VERT(M),'(A1,1x,A42,1a,a)')KEY,CXITM(1:42),'|',flag(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='* modify list                         '
      VERT(M+3)  ='? help                                '
      VERT(M+4)  ='- exit this menu                      '

C Now display the menu.
      CALL EMENU('  Anchor points',VERT,MVERT,IVERT)

      if(ivert.eq.mvert)then

C Save and return. 
        if(NALOC.gt.0)CALL EMKCFG('-',IER)
        return
      elseif(ivert.eq.mvert-1)then
        CALL PHELPD('anchor points',30,'-',0,0,IER)
      elseif(ivert.eq.mvert-2)then
        if(IALOC(ifoc).eq.0)then
          inpic = 60
        else
          inpic = 60 - IALOC(ifoc)
        endif
        CALL EPMENSV
        call easkconn(inpic,iconnpk,60,
     &    'Select from the list of connections the',
     &    'surfaces associated with this anchor.',ALOCLBL(ifoc),
     &    ifoc,ier,30)
        CALL EPMENRC
        if(inpic.gt.0)then
          do 242 ij=1,inpic

C For each item from iconnpk that does not match an existing
C anchor update the list.
            found=.false.
            if(IALOC(ifoc).eq.0)then
              IALOC(ifoc)=IALOC(ifoc)+1
              lstanchr(ifoc,IALOC(ifoc))=iconnpk(ij)
            else
              do 243 ik=1,IALOC(ifoc)
                if(lstanchr(ifoc,ik).eq.iconnpk(ij))found=.true.
  243         continue
              if(.NOT.found)then
                IALOC(ifoc)=IALOC(ifoc)+1
                lstanchr(ifoc,IALOC(ifoc))=iconnpk(ij)
              endif
            endif
  242     continue
          goto 31
        else
          call edisp(iuout,'No surfaces associated with anchor.')
        endif
        goto 31
      elseif(ivert.eq.mvert-3)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(ivert.eq.4)then

C Force redraw of zones, highlighting anchor list.
        MODIFY=.TRUE.
        nzg=NCOMP
        if(nzg.gt.0)then
          DO 144 I=1,nzg
            nznog(I)=I
 144      CONTINUE
          CALL INLNST(1)
          do 145 jj = 1,IALOC(ifoc)
            kk=lstanchr(ifoc,jj)
            if(kk.ne.0)LINSTY(kk)=2
 145      continue
          CALL ADJVIEW(ITRC,IER)
        endif
        dok=.false.
        h(1)='A chance to abort incorrect selection. '
        call askok('Edit anchor name or type?',' ',ok,dok,1)
        if(ok)then
          ino=0
          if(ALOCTYP(ifoc)(1:4).eq.'SURF')ino=1
          if(ALOCTYP(ifoc)(1:4).eq.'CLDR')ino=2
          if(ALOCTYP(ifoc)(1:4).eq.'OPDR')ino=3
          if(ALOCTYP(ifoc)(1:4).eq.'CLWN')ino=4
          if(ALOCTYP(ifoc)(1:4).eq.'OPWN')ino=5
          if(ALOCTYP(ifoc)(1:4).eq.'CLGR')ino=6
          if(ALOCTYP(ifoc)(1:4).eq.'OPGR')ino=7
          if(ALOCTYP(ifoc)(1:4).eq.'DUCT')ino=8
          if(ALOCTYP(ifoc)(1:4).eq.'FANP')ino=9
          ilno=ino
          idno=1
          call MENUATOL('Select a type for the anchor:',
     &    'Anchor type (options)','a surface (general)','b closed door',
     &    'c open(able) door','d closed window','e open(able) window',
     &    'f closed grill','g openable grill','h duct or pipe',
     &    'i fan or pump',' ',' ',' ',ino,idno,30)
          if(ino.eq.0)then
            goto 31
          elseif(ino.eq.1)then
            AL=ALOCLBL(ifoc)
            if(AL(1:4).eq.'unde')AL='  '
            CALL EASKS(AL,' Label for anchor: ',
     &        ' ',12,'none','anchor label',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='SURF'
          elseif(ino.eq.2)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_door'
            CALL EASKS(AL,'Label:',' ',12,'none','closed dr',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLDR'
          elseif(ino.eq.3)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_door'
            CALL EASKS(AL,'Label:',' ',12,'none','open dr ',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPDR'
          elseif(ino.eq.4)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_windw'
            CALL EASKS(AL,'Label:',' ',12,'none','closd win',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLWN'
          elseif(ino.eq.5)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_window'
            CALL EASKS(AL,'Label:',' ',12,'none','open win',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPWN'
          elseif(ino.eq.6)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','closd grill',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLGR'
          elseif(ino.eq.7)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','open grill',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPGR'
          elseif(ino.eq.8)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='duct'
            CALL EASKS(AL,'Label:',' ',12,'none','ductorpipe',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='DUCT'
          elseif(ino.eq.9)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='fanorpump'
            CALL EASKS(AL,'Label:',' ',12,'none','fanorpump',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='FANP'
          endif
          CALL EMKCFG('-',IER)
        endif
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Enquire about connection identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOCC,IO)

C Display information about the anchors associated with connection.
        call CONXMENU(ifocc,CXITM)
        write(outs,'(a,a)') CXITM(1:lnblnk(CXITM)),
     &    ' is associated with the following anchors...'
        call edisp(iuout,outs)
        outs=' '
        foundone=.false.
        do 6 i=1,NALOC
          if(IALOC(i).gt.0)then
            do 7 j = 1,IALOC(i)
              k=lstanchr(i,j)
              if(k.eq.ifocc.and.i.gt.0)then
                foundone=.true.
                write(outs(1:12),'(a,a)') ALOCLBL(i)
                call edisp(iuout,outs)
              endif
   7        continue
          endif
   6    continue
        if(foundone)then
          dok=.false.
          h(1)='A chance to abort de-reference of link. '
          call askok('De-reference link(s) to this surface?',' ',
     &      ok,dok,1)
          if(ok)then

C Loop through all of the anchor lists and check if any reference this
C connection. If use requests deletion, shift list (lstanchr) and
C decrement IALOC.
            do 16 i=1,NALOC
              ikk=IALOC(i)
              if(ikk.gt.0)then
                do 17 j = 1,ikk
                  k=lstanchr(i,j)
                  if(k.eq.ifocc)then
                   write(outs,'(3a)')'Dereference ',ALOCLBL(i),'?'
                    dok=.true.
                    h(1)='A chance not to dereference this anchor. '
                    call askok(outs,' ',okd,dok,1)
                    if(okd)then
                      do 18 ik=j,ikk-1
                        lstanchr(i,j)=lstanchr(i,j+1)
  18                  continue
                      IALOC(i)=IALOC(i)-1
                    endif
                  endif
  17            continue
              endif
  16        continue
            goto 31
          endif
        endif
C << what additional functionality?? >>
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C **************** anchlist 
C anchlist allows selection and list management of anchor points (a list of connections
C associated with a concept).
      subroutine anchlist(iasel,ier)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION AVERT(27)
      CHARACTER AVERT*36,KEY*1,H*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,AL*12

      iasel=0

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      if(NALOC.eq.0)then
        call usrmsg('Currently there are no anchors defined. ',
     &    'Begin by selecting the + option. ','W')
      endif
  31  MHEAD=3
      MCTL=4
      write(6,*) 'naloc mhead mctl ',naloc,mhead,mctl

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Help text.
   3  h(1)='Anchor points allow a user to line a number of items '
      h(2)='(such as surfaces) to a concept so that global actions'
      h(3)='(such as attribution or performance enquiries) can be '
      h(4)='undertaken. An achor point consists of name (12 char),'
      h(5)='a list of item indices and a type indicator. '
      h(6)=' '
      h(7)='Anchor types set the context within which esp-r can'
      h(8)='manipulate items associated with an anchor. Details: '
      h(9)=' a general surface - supports actions such as changing'
      h(10)='   attributes or graphing temperatures of a '
      h(11)='   collection of surfaces).'
      h(12)=' '
      h(13)='Tagging surfaces as possible openings or leakage paths'
      h(14)='in a flow network:'
      h(15)=' b closed door - surface acts as or includes a closed'
      h(16)='   door (or crack) between two flow nodes.'
      h(17)=' c open(able) door - surface acts as or includes an '
      h(18)='   open door (perhaps controlled) between flow nodes.'
      h(19)=' d closed window - surface acts as/includes a closed'
      h(20)='   window (or crack) between two flow nodes.'
      h(21)=' e open(able) window - surface acts as/includes a'
      h(22)='   window (perhaps controlled) between flow nodes.'
      h(23)=' f closed grill - surface acts as/includes a closed'
      h(24)='   grill (or crack) between two flow nodes.'
      h(25)=' g open(able) grill - surface includes an grill or'
      h(26)='   vent (perhaps controlled) between two flow nodes.'
      h(27)=' h duct or pipe - surface inclues an duct or pipe'
      h(28)='   between two flow nodes.'
      h(29)=' i fan or pump - surface near a fan or pump between'
      h(30)='   two flow nodes.'

C Set menu header text.
      write(AVERT(1),'(a,i2)') '1 anchors: ',NALOC
      write(AVERT(2),'(a)')    '  ________________________________ '
      write(AVERT(3),'(a)')    '  index  label     type  links     '
      M=MHEAD
      do 5 L=1,NALOC
        M=M+1
        CALL EMKEY(L,KEY,IER)
        AVERT(M)=' '
        write(AVERT(M),'(2a,i2,5a,i2,a)') KEY,' ',L,'   ',ALOCLBL(L),
     &  ' ',ALOCTYP(L),' (',IALOC(L),' links)'
   5  continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      AVERT(M+1)  ='  ________________________________ '
      AVERT(M+2)  ='+ add/delete an anchor             '
      AVERT(M+3)  ='? help                             '
      AVERT(M+4)  ='- exit this menu                   '

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('  Current anchor points',AVERT,MVERT,IVERT)
      else
        CALL EMENU('  Anchor points',AVERT,MVERT,IVERT)
      endif

      if(ivert.eq.mvert)then

C Save and return. 
        if(NALOC.gt.0)CALL EMKCFG('-',IER)
        return
      elseif(ivert.eq.mvert-1)then
        CALL PHELPD('anchor points',30,'-',0,0,IER)
      elseif(ivert.eq.mvert-2)then
        CALL EASKABC('Anchor point options:',' ',
     &    'add an anchor','delete an anchor','contine',II,6)
        if(II.eq.1)then
          NALOC=NALOC+1
          ino=0
          ilno=0
          idno=3
          call MENUATOL('Select a type for the anchor:',
     &    'Anchor type (options)','a surface (general)','b closed door',
     &    'c open(able) door','d closed window','e open(able) window',
     &    'f closed grill','g openable grill','h duct or pipe',
     &    'i fan or pump',' ',' ',' ',ino,idno,30)
          if(ino.eq.0)then
            goto 31
          elseif(ino.eq.1)then
            AL=ALOCLBL(NALOC)
            CALL EASKS(AL,' Label for anchor: ',
     &        ' ',12,'none','anchor label',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='SURF'
          elseif(ino.eq.2)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_door'
            CALL EASKS(AL,'Label:',' ',12,'none','closed dr',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLDR'
          elseif(ino.eq.3)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_door'
            CALL EASKS(AL,'Label:',' ',12,'none','open dr ',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPDR'
          elseif(ino.eq.4)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_windw'
            CALL EASKS(AL,'Label:',' ',12,'none','closd win',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLWN'
          elseif(ino.eq.5)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_window'
            CALL EASKS(AL,'Label:',' ',12,'none','open win',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPWN'
          elseif(ino.eq.6)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','closd grill',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLGR'
          elseif(ino.eq.7)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','open grill',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPGR'
          elseif(ino.eq.8)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='duct'
            CALL EASKS(AL,'Label:',' ',12,'none','ductorpipe',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='DUCT'
          elseif(ino.eq.9)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='fanorpump'
            CALL EASKS(AL,'Label:',' ',12,'none','fanorpump',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='FANP'
          endif
          call usrmsg('You may now associate surfaces with this',
     &      'anchor... Begin by selecting from the list. ','W')
          iasel=NALOC
          if(NALOC.gt.0)CALL EMKCFG('-',IER)
          call anchor(iasel,ier)
          goto 31
        elseif(II.eq.2)then

C Deletion function. << to be done >>

        elseif(II.eq.3)then
          continue
        endif
        goto 31
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Pass one back for selection and manipulation.
        iasel=ivert-MHEAD
        call anchor(iasel,ier)
        goto 31
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C **************** easkconn 
C Select one or more connections from a list.
      subroutine easkconn(inpic,iconnpk,lconnpk,promp1,promp2,ermsg,
     &  ltype,ier,nhelp)
#include "building.h"
#include "epara.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/C1/NCOMP,NCON
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION iconnpk(lconnpk),VERT(35),flag(mcon)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER VERT*54,KEY*1,H*72,CXITM*43,flag*1,SEL*50
      logical found
    
C At this point bring up a menu with the string alternatives, assuming
C there is more than one alternative to pick from.
      IF(lconnpk.LT.1)THEN
        CALL USRMSG(PROMP1,' Nunber of items to small.','W')
        IER=1
        RETURN
      ENDIF
      CALL USRMSG(PROMP1,PROMP2,'-')

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=2
      MCTL=3
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPIC

C Clear iconnpk and INPIC and, if editing an existing anchor, point
C to those items already associated.
      IER=0
      INPIC=0
      DO 40 I=1,lconnpk
        iconnpk(I)=0
   40 CONTINUE
      if(IALOC(ltype).gt.0)then
        do 42 ia=1,IALOC(ltype)
          if(lstanchr(ltype,ia).ne.0)then
            iconnpk(ia)=lstanchr(ltype,ia)
          endif
  42    continue
      endif

C If previous list being edited, marke these.
      do 2 i = 1,ncon
        flag(i)= ' '
        if(IALOC(ltype).gt.0)then
          do 41 ia=1,IALOC(ltype)
            if(lstanchr(ltype,ia).eq.i)then
              flag(i)= '*'
            endif
  41      continue
        endif
  2   continue

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Set menu header text. Update flag string with * depending on which
C items have been selected. Generate text for title.
    3 lne=LNBLNK(ERMSG)
      IF(lne.GT.50)WRITE(SEL,'(1X,A)')ERMSG(1:50)
      IF(lne.LE.50)WRITE(SEL,'(1X,A)')ERMSG(1:lne)
      VERT(1)=  ' no. | inside face     | other side data   |anchor'
      VERT(2)=  ' conn|  connection     |    connection     |select'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          WRITE(VERT(M),'(A1,1x,A43,1x,a1)')KEY,CXITM,flag(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='? help                                '
      VERT(M+3)  ='- exit this menu                      '

C Help text for the menu.
      IF(NHELP.GT.0)then
        WRITE(H(NHELP+1),65)
      ELSEIF(NHELP.EQ.0)then
        WRITE(H(1),'(A,A)')' No help available for ',ERMSG
        WRITE(H(2),65)
      ENDIF

C Now display the menu.
      CALL EMENU(sel,VERT,MVERT,IVERT)

      if(IVERT.LE.MHEAD)then

C Within the header so skip request.
        IVERT=-1
        goto 3
      elseif(ivert.eq.mvert)then
        return
      elseif(ivert.eq.mvert-1)then

C Produce help text for the menu.
        IF(NHELP.GT.0)then
          CALL PHELPD('connection pick string',NHELP+1,'-',0,0,IER)
        ELSEIF(NHELP.EQ.0)then
          CALL PHELPD('connection pick string',2,'-',0,0,IER)
        ENDIF
  65    format(' Type key to select, repeat as',
     &         ' required. -Exit to return list.')
      elseif(ivert.eq.mvert-2)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Mark identified by KEYIND. Increment ialoc counter and then
C instanciate iconnpk with current connection number. If item
C already selected, treat second selection as a toggle OFF.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPIC.GT.0)THEN
          DO 44 J=1,INPIC
            if(iconnpk(J).EQ.IFOC)then
              FOUND=.TRUE.
              ljfoc=iconnpk(j)
              lj=j
            endif
  44      CONTINUE
          if(.NOT.FOUND)then
            INPIC=INPIC+1
            iconnpk(INPIC)=IFOC
            flag(ifoc)='*'
          else
            INPIC=INPIC-1
            iconnpk(lj)=0
            flag(ljfoc)=' '
          endif
        ELSEIF(INPIC.EQ.0)THEN
          INPIC=1
          iconnpk(INPIC)=IFOC
          flag(ifoc)='*'
        ENDIF
        IF(INPIC.EQ.IALLOW)THEN

C Have picked as many as are allowed.
          RETURN
        ELSE
          IVERT=-2
          goto 3
        ENDIF
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C ***** PROCGDST
C PROCGDST process GDST command sets (defined in common blocks
C idsta and anchora.
C << Note: this procedure is somewhat out of date. Its function
C << should be reviewed for current simulation demands
      subroutine procgdst(itrc,runmode,keepres,donegdst,ier)
#include "building.h"
#include "esprdbfile.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/appw/iappw,iappx,iappy
      COMMON/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C IDST tool commands.
      common/idsta/CMDCFG,CMDCLM,CMDCLMN,CMDNOTE,CMDRES,CMDLBL(10),
     &             CMDLNAM(10)
      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres
      common/clmlst/cdblbl,cdblfil

      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character CMDLNAM*80,CMDLBL*12,cdblbl*20,cdblfil*72
      character LCFGF*72,cfgroot*24,h*72
      character CXITM*43,DESC*48,outs*124,LTMP*72
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,clmdb
      CHARACTER SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SSPARENT*12
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character doit*248,tmode*8,tfile*72,aut*8,runmode*12,keepres*4
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,alias*32,donegdst*72
      character longtfile*144,longtfiledos*144
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      logical updatecon,QUIET,concat,unixok

C Set flag for zone construction file update.
      updatecon=.false.

C Check if Unix-based or DOS based.
      call isunix(unixok)

C If IDST climate is different update the configuration. Check
C against the climate list to see related db for climate name.
      iuf=IFIL+1
      write(alias,'(a)')CMDCLMN(1:lnblnk(CMDCLMN))
      call matchcdb(iuf,cdblfil,alias,clmdb,ier)
      if(clmdb(1:lnblnk(clmdb)).ne.LCLIM(1:lnblnk(LCLIM)))then
        if(clmdb(1:4).ne.'UNKN')then
          LCLIM=clmdb
          write(6,*)'using new climate db ',clmdb
          call tstamp('>','PRJ: IDST update climate in configuration')
          call tstamp('>',LCFGF)
          CALL EMKCFG('-',IER)
        endif
      endif

C Loop through anchor types and deal with known types.
      call edisp(iuout,' no. |zone|surf|type|   other side data')
      call edisp(iuout,' conn|reference|conn|      connection  ')
      do 32 m=1,nidst

C Now see if can match the IDST ext_window parameter. ALOC(m).
        imlcrepl=0
        do 33 k=1,NMLC
          if(ALOC(m)(1:12).eq.DESC(k)(1:12))then
            imlcrepl=k
          endif
  33    continue
        if(CMDLBL(m)(1:10).eq.'ext_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes.
          if(imlcrepl.ne.0)then
            do 34 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'ext_window')then
                call edisp(iuout,' for type ext_window...')
                do 243 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  LTMP=LGEOM(IZ)
                  CALL EGOMIN(IUF,LTMP,IZ,1,0,iuout,IER)
                  WRITE(SMLCN(is),'(A12)')DESC(imlcrepl)(1:12)
                  SSMLCN(ifoc)=SMLCN(is)
                  IF(DESC(imlcrepl)(15:18).EQ.'TRAN')SOTF(is)='TRAN'
                  IF(DESC(imlcrepl)(15:18).EQ.'OPAQ')SOTF(is)='OPAQ'
                  SSOTF(ifoc)=SOTF(is)
                  updatecon=.true.
                  CALL EMKGEO(IUF,LTMP,IZ,iuout,3,IER)
 243            continue
              endif
  34        continue
          endif
        elseif(CMDLBL(m)(1:10).eq.'int_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes.
          if(imlcrepl.ne.0)then
            do 44 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'int_window')then
                call edisp(iuout,' of type int_window...')
                do 244 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  LTMP=LGEOM(IZ)
                  CALL EGOMIN(IUF,LTMP,IZ,1,0,iuout,IER)
                  WRITE(SMLCN(is),'(A12)')DESC(imlcrepl)(1:12)
                  SSMLCN(ifoc)=SMLCN(is)
                  IF(DESC(imlcrepl)(15:18).EQ.'TRAN')SOTF(is)='TRAN'
                  IF(DESC(imlcrepl)(15:18).EQ.'OPAQ')SOTF(is)='OPAQ'
                  SSOTF(ifoc)=SOTF(is)
                  updatecon=.true.
                  CALL EMKGEO(IUF,LTMP,IZ,iuout,3,IER)
 244            continue
              endif
  44        continue
          endif
        elseif(CMDLBL(m)(1:10).eq.'atr_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes.
          if(imlcrepl.ne.0)then
            do 45 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'atr_window')then
                call edisp(iuout,' of type atr_window...')
                do 245 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  LTMP=LGEOM(IZ)
                  CALL EGOMIN(IUF,LTMP,IZ,1,0,iuout,IER)
                  WRITE(SMLCN(is),'(A12)')DESC(imlcrepl)(1:12)
                  SSMLCN(ifoc)=SMLCN(is)
                  IF(DESC(imlcrepl)(15:18).EQ.'TRAN')SOTF(is)='TRAN'
                  IF(DESC(imlcrepl)(15:18).EQ.'OPAQ')SOTF(is)='OPAQ'
                  SSOTF(ifoc)=SOTF(is)
                  updatecon=.true.
                  CALL EMKGEO(IUF,LTMP,IZ,iuout,3,IER)
 245            continue
              endif
  45        continue
          endif
        endif
  32  continue
       
      if(updatecon)then
        call tstamp('>','PRJ: IDST update zone constructions')
        QUIET=.TRUE.
        DO 2491, IZ=1,NCOMP
          CALL EDCON(0,iuout,IZ,QUIET,IER)
 2491   CONTINUE
        QUIET=.FALSE.
      endif

C Run the appropriate simulations.
      h(1)='Interactive mode uses simulation parameters as '
      h(2)='defaults. The silent running mode uses the'
      h(3)='parameters to run in automatic mode. '
      if(runmode(1:11).eq.'interactive')then
        CALL EASKAB('Simulation interaction options: (see help)',
     &    ' ','run interactively','silent running mode',isauto,3)
      else
        isauto=2
      endif
      call tstamp('>','PRJ: beginning IPV simulations.')
      doit = ' '
      call tchild(ICPMOD)
      call termode(ICPMOD,tmode)
      if(isauto.eq.1)aut=' default'
      if(isauto.eq.2)then
        aut=' silent '
        tmode='text'
      endif
      if(unixok)then
        call addpath(LCFGF,longtfile,concat)
      else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
        call addpath(LCFGF,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)
        longtfile=' '
        longtfile=longtfiledos
      endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(:lnblnk(longtfile)),' -p win',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &   longtfile(1:lnblnk(longtfile)),' -p win',aut
      endif
      call usrmsg('starting winter assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,' ')
      call edisp(iuout,'Winter season run complete.')
      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(1)(1:lnblnk(sblres(1))),' -act ipv_win ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(1)(1:lnblnk(sblres(1))),
     &   ' -act ipv_win ',aut
      endif
      call usrmsg('starting winter recovery via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Winter season results complete.')

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p trn',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p trn',aut
      endif
      call usrmsg('starting transition assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Transition season run complete.')

      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(2)(1:lnblnk(sblres(2))),' -act ipv_trn ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(2)(1:lnblnk(sblres(2))),
     &   ' -act ipv_trn ',aut
      endif
      call usrmsg('starting transition recovery via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Transition season results complete.')

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p sum',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &   longtfile(1:lnblnk(longtfile)),' -p sum',aut
      endif
      call usrmsg('starting summer assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Summer season run complete.')

      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(3)(1:lnblnk(sblres(3))),' -act ipv_sum ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(3)(1:lnblnk(sblres(3))),
     &   ' -act ipv_sum ',aut
      endif
      call edisp(iuout,doit)
      call runit(doit,tmode)
      call edisp(iuout,'Summer season results complete.')

      call edisp(iuout,'Annual IPV recovery starting...')
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(3)(1:lnblnk(sblres(3))),' -act ipv_ann ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(3)(1:lnblnk(sblres(3))),
     &   ' -act ipv_ann ',aut
      endif
      call edisp(iuout,doit)
      call runit(doit,tmode)
      call edisp(iuout,'Summer season results complete.')

C Remove the esp-r results files if requested.
C << only do this for the actual number of sets >>
      if(keepres(1:2).eq.'no')then
        idf=ifil+1
        nzrl = MZRL
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(1),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(2),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(3),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        call edisp(iuout,'ESP-r results libraries cleaned up.')
      endif

      call edisp(iuout,'GDST COMMANDS FINISHED.')
      return

 3    CALL ERPFREE(idf,ISTAT)
      return

      end

C ******** serchrpl
C Serchrpl handles search and replace of composition attributes.
      subroutine serchrpl(act,itrc,ier)
#include "building.h"
      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/RAY3/MODIFY,MODLEN,MODBND
      common/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      logical OK,DOK,MODIFY,MODLEN,MODBND,QUIET,apply

      dimension IVALS(MCOM),SNA(MS),PAZI(MS),PELV(MS)

      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,LTMP
      character H*72
      character SOTHER*15,SMLCN*12,SVFC*4,SOTF*4,DESC*48
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character zname*12,ZN*12,outs*124,SNAME*12,SN*12,SSPARENT*12
      character zdesc*64,DESCRC*25,act*1
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      character AL1*12,AL2*12,AL3*12,AL4*12,AL5*12,AL6*12,AL7*12
      character AL8*12,AL9*12,CXITM*43

      iuf=IFIL+1
      if(act.eq.'c')then
        H(1)='Often it is convenient to search a model for one  '
        H(2)='constructional attribute and replace this by'
        H(3)='another. You can do this by searching for occurances'
        H(4)='of a particular construction or by selecting an anchor'
        H(5)='point (with an associated list of surfaces) to apply'
        H(6)='the new attribute to.'
        H(7)=' '
        H(8)='In the first method, choose the construction to search'
        H(9)='for and then its replacement (you will be asked to '
        H(10)='confirm each replacement'
        H(11)=' '
        H(12)='In the 2nd method, choose an anchor point and then the'
        H(13)='substutute construction. '
        H(14)=' '
        H(15)='NOTE: After using the search & replace facility '
        H(16)='zone construction files should be rebuilt for the'
        H(17)='new attributes to take effect. '
        CALL PHELPD('global attributions',17,'-',0,0,IER)
        if(NALOC.gt.0)then
          CALL EASKABC(' Surfaces to update:',' ',
     &      'which match a construction','linked with an anchor',
     &      'cancel',ins9,17)
        else
          ins9=1
        endif
        if(ins9.eq.1)then

C Search and replace by finding matching constructions.
          CALL EPKMLC(ISEL,'Select a construction to SEARCH for',
     &      'or -Exit',IER)
          if(ISEL.eq.0)return
          CALL EPKMLC(ISELR,'Select a construction to REPLACE it',
     &      'with or -Exit',IER)
          if(ISELR.eq.0)return

          H(1)=' Pick one, several or all zones to update.'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ',' Search zones:',
     &      12,NCOMP,zname,' zone list',IER,1)
          IF(INPIC.EQ.0)return
          call tstamp('>','PRJ: global search & replace')
          DO 96 IZ=1,INPIC
            IF(IVALS(IZ).GT.0)THEN
              write(zn,'(A)') zname(IVALS(IZ))
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &           ' attributes...'
              CALL USRMSG(' ',outs,'-')
              LTMP=LGEOM(IVALS(IZ))
              CALL EGOMIN(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
              CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)
              WRITE(outs,93)
              call edisp(iuout,outs)
              WRITE(outs,94)
              call edisp(iuout,outs)
              do 91 i=1,nsur
                if(SMLCN(i)(1:12).eq.DESC(ISEL)(1:12))then
                  write(sn,'(a)') SNAME(IVALS(IZ),I)
                  WRITE(outs,'(a7,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &              1X,A4,1X,A12,1X,A13)')'Match: ',I,
     &              SNA(I),PAZI(I),PELV(I),SN,SOTF(I),SVFC(I),
     &              SMLCN(I),SOTHER(I)
                  call edisp(iuout,outs)
                  write(outs,'(5a)') ' Apply construction to ',
     &              zn(1:lnblnk(zn)),':',sn(1:lnblnk(sn)),'?'
                  dok=.true.
                  h(1)='Just in case you want to be selective...'
                  CALL ASKOK(' ',outs,OK,dok,1)
                  IF(OK)then
                    ioc=IZSTOCN(IVALS(IZ),i)
                    WRITE(SMLCN(I),'(A12)')DESC(ISELR)(1:12)
                    SSMLCN(ioc)=SMLCN(i)
                    IF(DESC(ISELR)(15:18).EQ.'TRAN')SOTF(I)='TRAN'
                    IF(DESC(ISELR)(15:18).EQ.'OPAQ')SOTF(I)='OPAQ'
                    SSOTF(ioc)=SOTF(i)
                  endif
                endif
 91           continue
              CALL EMKGEO(IUF,LTMP,IVALS(IZ),iuout,3,IER)
              QUIET=.TRUE.
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &           ' constructions...'
              CALL USRMSG(' ',outs,'P')
              call tstamp('>','PRJ: auto update zone construction')
              CALL EDCON(0,ITRU,IVALS(IZ),QUIET,IER)
              QUIET=.FALSE.
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &        '... done.'
              CALL USRMSG(' ',outs,'-')
            endif
  96      continue
          MODIFY=.TRUE.
          MODBND=.TRUE.
        elseif(ins9.eq.2)then

C Anchor point substitution.
          AL1=ALOCLBL(1)
          AL2=ALOCLBL(2)
          AL3=ALOCLBL(3)
          AL4=ALOCLBL(4)
          AL5=ALOCLBL(5)
          h(1)='Indicate which anchor to use. '
          CALL easkatog(' Which anchor:',' ',AL1,AL2,AL3,AL4,AL5,
     &      'other anchors','continue',ltype,1)
          if(ltype.eq.7)then
            call usrmsg('No anchor selected.',' ','W')
            return
          elseif(ltype.eq.6)then
            AL6=ALOCLBL(6)
            AL7=ALOCLBL(7)
            AL8=ALOCLBL(8)
            AL9=ALOCLBL(9)
            CALL easkatog(' Which anchor:',' ',AL6,AL7,AL8,AL9,
     &        'continue',' ',' ',ltype,1)
            if(ltype.gt.4)then
              call usrmsg('No anchor selected.',' ','W')
              return
            else
              ltype=ltype+5
            endif
          endif
          CALL EPKMLC(ISELR,'Select a construction to use with this',
     &      'surface list or -Exit',IER)
          if(ISELR.eq.0)return
          dok=.true.
          h(1)='A chance to abort if incorrect construction selected.'
          CALL ASKOK('Apply this construction?',' ',OK,dok,1)
          IF(.NOT.OK)return
          do 2243 j=1,IALOC(ltype)
            ifoc=lstanchr(ltype,j)
            call CONXMENU(ifoc,CXITM)
            call usrmsg(' Processing...',CXITM,'-')
            iz=ic1(ifoc)
            is=ie1(ifoc)
            LTMP=LGEOM(IZ)
            CALL EGOMIN(IUF,LTMP,IZ,1,0,iuout,IER)
            WRITE(SMLCN(is),'(A12)')DESC(ISELR)(1:12)
            WRITE(SSMLCN(ifoc),'(A12)')DESC(ISELR)(1:12)
            IF(DESC(ISELR)(15:18).EQ.'TRAN')SOTF(is)='TRAN'
            IF(DESC(ISELR)(15:18).EQ.'OPAQ')SOTF(is)='OPAQ'
            SSOTF(ifoc)=SOTF(is)
            CALL EMKGEO(IUF,LTMP,IZ,iuout,3,IER)
 2243     continue
          QUIET=.FALSE.
          dok=.true.
          h(1)='A search & replace only changes the surface attribute'
          h(2)='it does not update the raw thermophysical data held in'
          h(3)='the zone construction files. It is recommended that '
          h(4)='you also update the zone files. '
          CALL ASKOK(
     &      'All construction & related files should be updated...',
     &      'Do this now (recommended - see help) ?',QUIET,dok,4)
          if(.NOT.QUIET)return
          call tstamp('>','PRJ: auto update zone construction')
          DO 2492,IZ=1,NCOMP
            CALL EDCON(0,ITRU,IZ,QUIET,IER)
 2492     CONTINUE
          QUIET=.FALSE.
        endif
      elseif(act.eq.'a')then
        H(1)='Often it is convenient to supply surface construct-'
        H(2)='ional attribution by globally applying a material to'
        H(3)='a subset of surfaces and then attend to exceptions. '
        H(4)=' '
        H(5)='The following dialogue will allow you to define a '
        H(6)='subset of surfaces via filters for position,  '
        H(7)='orientation, opacity, etc. '
        H(8)=' '
        H(9)='NOTE: After using the global attribution facility '
        H(10)='you must then rebuild the zone construction files'
        H(11)='for the new attributes to take effect. '
        CALL PHELPD('global attributions',11,'-',0,0,IER)

        CALL EPKMLC(ISEL,'Select construction for matching surfaces',
     &    'or -Exit',IER)
        if(ISEL.eq.0)return

        H(1)='You may restrict updating to only opaque surfaces,'
        H(2)='transparent surfaces or apply to both. '
        CALL EASKABCD(' First filter--surface opacity:',' ',
     &    'opaque','transparent','both','cancel',ins1,2)
        if(ins1.eq.4)return

        H(1)='You may restrict updating to only surfaces which '
        H(2)='face the outside, partitions (internal) etc. '
        call EASKATOG(' Second filter--surface contiguity:',' ',
     &      'external','similar','constant','partitions','ground',
     &      'adiabatic','cancel',ins2,2)
        if(ins2.eq.7)then
          return
        endif

        H(1)='You may restrict updating to only surfaces which '
        H(2)='have a particular orientation. '
        call EASKATOG(' Third filter--surface orientation:',' ',
     &      'vertical','sloped','horizontal up','horizontal down',
     &      'any horizontal','any orien.','cancel',ins3,2)
        if(ins3.eq.7)then
          return
        endif

        H(1)='You may restrict update to avoid already attributed'
        H(2)='surfaces. '
        CALL EASKABCD(' Surfaces to update:',' ',
     &    'those already attributed','those marked UNKNOWN','both',
     &    'cancel',ins4,2)
        if(ins4.eq.4)then
          return
        endif

        H(1)=' Pick one, several or all zones to update.'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,' ',' Which zones to update: ',
     &      12,NCOMP,zname,' zone list',IER,1)
        IF(INPIC.EQ.0)return
        call tstamp('>','PRJ: global construction attribution')
        DO 95 IZ=1,INPIC
          IF(IVALS(IZ).GT.0)THEN
            write(zn,'(A)') zname(IVALS(IZ))
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &         '...'
            CALL USRMSG(' ',outs,'-')
            LTMP=LGEOM(IVALS(IZ))
            CALL EGOMIN(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
            CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)
            WRITE(outs,93)
   93     FORMAT('           Sur|  Area  |Azim|Elev| surface    |',
     &        'geometry| multilayer  |environment')
            call edisp(iuout,outs)
            WRITE(outs,94)
   94     FORMAT('              |  m^2   |deg |deg | name       |',
     &        'type|loc| constr name |other side ')
            call edisp(iuout,outs)
            do 11 i=1,nsur
              apply=.true.
              if(ins1.eq.1.and.SOTF(i)(1:4).ne.'OPAQ')apply=.false.
              if(ins1.eq.2.and.SOTF(i)(1:4).ne.'TRAN')apply=.false.

C Determine the match in the system topology. 
              CALL SURADJ(IVALS(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ins2.eq.1.and.IE.ne.0)then
                apply=.false.
              elseif(ins2.eq.2.and.IE.ne.1)then
                apply=.false.
              elseif(ins2.eq.3.and.IE.ne.2)then
                apply=.false.
              elseif(ins2.eq.4.and.IE.ne.3)then
                apply=.false.
              elseif(ins2.eq.5.and.IE.ne.4)then
                apply=.false.
              elseif(ins2.eq.6.and.IE.ne.5)then
                apply=.false.
              endif

C Orientation check.
C 'vertical','sloped','horizontal up','horizontal down',
C 'any horizontal','any orien.','cancel'.
              if(ins3.eq.1.and.SVFC(i)(1:4).ne.'VERT')then
                apply=.false.
              elseif(ins3.eq.2.and.SVFC(i)(1:4).ne.'SLOP')then
                apply=.false.
              elseif(ins3.eq.3.and.SVFC(i)(1:4).ne.'CEIL')then
                apply=.false.
              elseif(ins3.eq.4.and.SVFC(i)(1:4).ne.'FLOR')then
                apply=.false.
              elseif((ins3.eq.5.and.SVFC(i)(1:4).eq.'FLOR').or.
     &               (ins3.eq.5.and.SVFC(i)(1:4).eq.'CEIL'))then
                continue
              elseif(ins3.eq.6)then
                continue
              endif

              if(ins4.eq.2.and.SMLCN(i)(1:4).ne.'UNKN')apply=.false.

              if(apply)then
                write(sn,'(a)') SNAME(IVALS(IZ),I)
                WRITE(outs,'(a7,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &            1X,A4,1X,A12,1X,A13)')'Match: ',I,SNA(I),
     &            PAZI(I),PELV(I),SN,SOTF(I),SVFC(I),SMLCN(I),SOTHER(I)
                call edisp(iuout,outs)
                write(outs,'(5a)') ' Apply construction to ',
     &            zn(1:lnblnk(zn)),':',sn(1:lnblnk(sn)),'?'
                dok=.true.
                h(1)='The criter you set were matched. And you can'
                h(2)='still opt not to apply it in all cases. '
                CALL ASKOK(' ',outs,OK,dok,2)
                IF(OK)then
                  ioc=IZSTOCN(IVALS(IZ),i)
                  WRITE(SMLCN(I),'(A12)')DESC(ISEL)(1:12)
                  SSMLCN(ioc)=SMLCN(i)
                  IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(I)='TRAN'
                  IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(I)='OPAQ'
                  SSOTF(ioc)=SOTF(i)
                endif
              endif
 11         continue
            CALL EMKGEO(IUF,LTMP,IVALS(IZ),ITRU,3,IER)
            QUIET=.TRUE.
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &         'constructions...'
            CALL USRMSG(' ',outs,'P')
            call tstamp('>','PRJ: auto update zone construction')
            CALL EDCON(0,ITRU,IVALS(IZ),QUIET,IER)
            QUIET=.FALSE.
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &        '... done.'
            CALL USRMSG(' ',outs,'-')
          endif
  95    continue
        MODIFY=.TRUE.
        MODBND=.TRUE.
      else
        call usrmsg('Unknown action...returning. ',' ','P')
      endif
      return

      end
