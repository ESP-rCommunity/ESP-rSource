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

C PCEDIT of ESRUpdb

C This routine allows the user to edit certain items of a component.
C The following items can be edited:
C  Component generic type.
C  Component description.
C  Component code.
C  Component nodal description.
C  Miscellaneous data.

C 'ival'=0 , means that component is already in database.
C 'ival'=n , means that some values should be changed before inserting
C            component in dbase. In this case n represents code number
C            for this component.

      subroutine pcedit(ival)
#include "plantdb.h"
#include "pdb_data.h"

      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

      CHARACTER dtdesc*68,adodsc*30

      dimension ivals(MAXPC)
      character buffer*120,ltmp*80
      character zitem(11)*20
      LOGICAL OK,dok
      integer nitems,ino ! max items and current menu item
      integer iuchan     ! for listing details

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='pcedit' ! set for subroutine

C The following statement writes a dummy variable on the last record
C of the binary file so that end-of-file marker is anchored there. 
C This was found to be essential on SPARC systems.
C Note that if a new component was inserted then position of
C end-of-file marker will be adjusted accordingly.

      IUCHAN = IUOUT  ! list details to text feedback
      dummy=999.
      write(ifil,rec=nxtrec,iostat=istat,err=1) dummy
      
      zitem(1)='a Generic type      '
      zitem(2)='b Description       ' 
      zitem(3)='c Component code    '
      zitem(4)='d Nodal description '
      zitem(5)='e Miscellaneous data'
      zitem(6)=' ------------------ '
      zitem(7)='f Save changes      '
      zitem(8)=' ------------------ '
      zitem(9)='! list current data '
      zitem(10)='? help              '
      zitem(11)='- Exit              '

      if(ival.eq.0) then

C User did not select anything. Check if library empty.
 1010   IF (NPC .eq. 0) then
          call usrmsg(' ',' The library is empty','W')
          return
        endif

C Dsiplay components list and get selection.
        inpc=npc
        nitems=1
        call list(inpc,ivals,nitems,ix)
        if (ix.eq.0) return

C Read component data from data base.
        call pcget(ivals(1))
        if(indxpc.eq.0) goto 1010
      endif

C Initialise changes flag.
      ichang=0

C Display menu.
  20  nitems = 11
      ino = -5
  22  call emenu('Edit component',zitem,nitems,ino)
      if(ino.eq.0) then

C Error in menu selection
        ino = -1
        goto 22 

C Edit component generic type.
      elseif(ino.eq.1) then
 1020   call edisp(iuout,' ')
        write(buffer,'(a,a40)') ' Old generic type ',zgtype
        call edisp(iuout,buffer)
        helptopic='pdb_generic_type_descr'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(zgtype,' New generic type? ',' ',
     &    40,' ',' Generic type',IER,nbhelp)
        if(ier.ne.0)goto 1020
        ichang=1

C Edit component description.
      elseif(ino.eq.2) then
 1030    call edisp(iuout,' ')
         call edisp(iuout,' Old component description:')
         write(buffer,'(1x,a80)') zcdesc
         call edisp(iuout,buffer)
         helptopic='pdb_new_component_descr'
         call gethelptext(helpinsub,helptopic,nbhelp)
         ltmp=zcdesc(1:80)
         CALL EASKS(ltmp,' New component description? ',
     &     ' ',80,' ','Editing component description',IER,nbhelp)
         if(ier.ne.0)goto 1030
         zcdesc=ltmp
         ichang=1

C Edit component code.
       elseif(ino.eq.3) then
 1040    call edisp(iuout,' ')
         write(buffer,'(a,i6)') ' Old comput code: ',icode
         call edisp(iuout,buffer)
         helptopic='pdb_new_component_code'
         call gethelptext(helpinsub,helptopic,nbhelp)
         CALL EASKI(icode,' ',' New component code ? ',
     &     0,'-',0,'-',1,'component code',IER,nbhelp)
         if(ier.ne.0)goto 1040
         itmloc(indxpc,2)=icode
         ichang=1

C Edit component model nodal description.
      elseif(ino.eq.4) then

C Case for single component.
         if(ictype.eq.0) then
            dok=.false.
            helptopic='pdb_matrix_change_ok'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' Change position of ',
     &      ' non-zero matrix coefficients?',OK,dok,nbhelp)
            IF(.NOT.OK)GOTO 330

C Edit position of non-zero matrix coefficiensts.
C Display non-zero matrix coefficients.
            call edisp(iuout,' ')
            write(buffer,320)  nmatx
  320       format(' Number of non-zero coefficients :',i5)
            call edisp(iuout,buffer)
            call edisp(iuout,' ')
            do 312 i=1,nmatx
              write(buffer,322) i,ncpos(i)
              call edisp(iuout,buffer)
  322         format(' coefficient',I3,': position',i3)
  312       continue
            if(nmatx.eq.1) then
               item=1
               goto 1060
            endif
 1050       item=0
            helptopic='pdb_matrix_change_which'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKI(item,' ',' Which coefficient to change? ',
     &       1,'F',nmatx,'F',1,'non-zero coefficient',IER,nbhelp)
            if(ier.ne.0)goto 1050
1060        nnode2=nnode*nnode
            helptopic='pdb_matrix_change_list'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKI(ncpos(item),' ',' New position? ',
     &             1,'F',nnode2,'F',1,' coeff position',IER,nbhelp)
            ichang=1

C Edit external connectivity of node(s).
  330       dok=.false.
            helptopic='matrix_change_connect'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' Change external connectivity',
     &      ' of node(s)?',OK,dok,nbhelp)
            IF(.NOT.OK)GOTO 350
            call edisp(iuout,' ')
            write(buffer,'(a,i5)') ' Number of nodes:', nnode
            call edisp(iuout,buffer)
            call edisp(iuout,' ')
            do 346 i=1, nnode
              write(buffer,347) i,ndcon(i)
               call edisp(iuout,buffer)
  347          format(' node',i3,': connectivity value',i3)
  346       continue
            if(nnode.eq.1) then
               item=1
               goto 1080
            endif
 1070       item=0
            CALL EASKI(item,' ',' Which node to change? ',
     &        1,'F',nnode,'F',1,'node connectivity',IER,nbhelp)
            if(ier.ne.0)goto 1070
 1080       CALL EASKI(ndcon(item),' ',' New connectivity value? ',
     &             0,'-',0,'-',1,' connectivity value',IER,nbhelp)
            if(ier.ne.0)goto 1080
            ichang=1

C Edit state variable index of node(s).
  350       dok=.false.
            helptopic='pdb_matrix_var_index'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' Change state variable index ',
     &      ' of node(s)?',OK,dok,nbhelp)
            IF(.NOT.OK)GOTO 368
            call edisp(iuout,' ')
            write(buffer,'(a,i5)') ' Number of nodes:', nnode
            do 366 i=1,nnode
              write(buffer,367) i,isv(i)
              call edisp(iuout,buffer)
  367         format(' node',i3,': state variable index',i3)
  366       continue
            if(nnode.eq.1) then
              item=1
              goto 1100
            endif
 1090       item=0
            CALL EASKI(item,' ',' Which node to change? ',
     &        1,'F',nnode,'F',1,' node to change',IER,nbhelp)
            if(ier.ne.0)goto 1090
 1100       helptopic='pdb_state_variable_index'
            call gethelptext(helpinsub,helptopic,nbhelp)
            itemv=isv(item)
            CALL EASKI(itemv,' ',' New state variable index? ',
     &        0,'-',0,'-',1,' state variable index?',IER,nbhelp)
            if(ier.ne.0)goto 1100
            isv(item)=itemv
  368       continue
            ichang=1

C Case for meta component.
         elseif(ictype.eq.1) then

C Edit components code numbers.
            dok=.false.
            helptopic='pdb_meta_component_index'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' ',' Change components code? ',OK,dok,nbhelp)
            IF(.NOT.OK)GOTO 390
            call edisp(iuout,' ')
            write(buffer,'(a,i5)') ' Number of components:',mncomp
            call edisp(iuout,buffer)
            do 385 i=1, mncomp
              write(buffer,383) i,micode(i)
              call edisp(iuout,buffer)
  383         format(' component',i3,': code number',i6)
  385       continue
            if(mncomp.eq.1) then
              item=1
              goto 1120
            endif
 1110       item=0
            CALL EASKI(item,' ',' Which item to change? ',
     &        1,'F',mncomp,'F',1,' item number?',IER,0)
            if(ier.ne.0)goto 1110
 1120       CALL EASKI(micode(item),' ',' Code number value? ',0,'-',0,
     &        '-',0,' item value?',IER,nbhelp)
            if(ier.ne.0)goto 1120
            ichang=1

C Edit components inter-connections.
  390       dok=.false.
            helptopic='pdb_meta_connect_change'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' Do you wish to change components ',
     &      ' inter-connections?',OK,dok,nbhelp)
C << to here >>
            IF(.NOT.OK)GOTO 421
            call edisp(iuout,' ')
            write(buffer,'(a,i5)')' No. of inter-connections:', mncon
            call edisp(iuout,buffer)
            call edisp(iuout,' ')
            write(buffer,410)
            call edisp(iuout,buffer)
  410       format(
     &         6x,'C1',5x,'N1',5x,'CT',5x,'C2',5x,'N2',7x,'MDR',
     &         6x,'MISC1',5x,'MISC2')
            do 412 i=1,mncon
              write(buffer,415) (icndat(i,l),l=1,5),
     &          (rcndat(i,ll),ll=1,3)
              call edisp(iuout,buffer)
  415         format(5(2x,i5),2x,f10.3,2(2x,f8.3))
  412       continue
            if(mncon.eq.1) then
              item=1
              goto 1140
            endif
 1130       item=0
            CALL EASKI(item,' ',' Which connection to change? ',
     &        1,'F',mncon,'F',1,' Which connection?',IER,nbhelp)
            if(ier.ne.0)goto 1130
 1140       write(ltmp,416) (icndat(item,l),l=1,5),
     &        (rcndat(item,ll),ll=1,3)
  416         format(5(2x,i5),2x,f10.3,2(2x,f8.3))
            CALL EASKS(ltmp,' New values? ',
     &       ' ',80,' ','connection values?',IER,nbhelp)
            if(ier.ne.0)goto 1140
            k=0
            ierl=0
            do 450 i=1,5
              call egetwi(ltmp,k,icnx,0,0,'-','icndat data',ierl)
              if(ierl.ne.0) goto 1140
              icndat(item,i)=icnx
  450       continue

            ierl=0
            do 420 i=1,3
              call egetwr(ltmp,k,rc,0.,0.,'-','rcndat data',ierl)
              if(ierl.ne.0) goto 1140
              rcndat(item,i)=rc
  420       continue
            ichang=1
  421       continue
         endif

C Edit miscellaneous data and data description.
      elseif(ino.eq.5) then
         call edisp(iuout,' ')
         write(buffer,500) nadata,nbdata,ncdata
         call edisp(iuout,buffer)
  500    format ('    Adata items   : ',I3,
     &           '    Bdata items   : ',I3,
     &           '    Cdata items   : ',I3)
         call edisp(iuout,' ')

C Edit ADATA variables.
         if(nadata.gt.0) then
           dok=.false.
           helptopic='ADATA_changes_editing'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL ASKOK(' ','Do you wish to edit ADATA?',OK,dok,nbhelp)
           IF(.NOT.OK)GOTO 530
           call edisp(iuout,' ')
           call edisp(iuout,' ADATA variables:')
           do 520 i=1,nadata
             write(buffer,515) i,dtdesc(i)
             call edisp(iuout,buffer)
             write(buffer,517) datams(i)
             call edisp(iuout,buffer)
  515        format(i5,1x,a68)
  517        format(6x,'Value = ',g12.5)
  520      continue
 1150      if(nadata.eq.1) then
             item=1
             goto 1160
           endif
           item=0
           CALL EASKI(item,' ',' Which item to change? ',
     &        1,'F',nadata,'F',1,' ADATA item?',IER,nbhelp)
           if(ier.ne.0)goto 1150
 1160      CALL EASKS(dtdesc(item),' ',' Item description? ',
     &             68,' ',' Item description?',IER,nbhelp)
           if(ier.ne.0)goto 1160
 1170      CALL EASKR(datams(item),' ',' Item value? ',0.,'-',
     &        0.,'-',0.,' item value?',IER,nbhelp)
           if(ier.ne.0)goto 1170
           ichang=1 
  530      continue
         endif

C Edit BDATA variables.
         if(nbdata.gt.0) then
           dok=.false.
           helptopic='BDATA_changes_editing'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL ASKOK(' ','Do you wish to edit BDATA?',OK,dok,nbhelp)
           IF(.NOT.OK)GOTO 560
           call edisp(iuout,' ')
           call edisp(iuout,' BDATA variables:')
           do 550 j=1,nbdata
             i=j+nadata
             write(buffer,545) j,dtdesc(i)
             call edisp(iuout,buffer)
             write(buffer,547) datams(i)
             call edisp(iuout,buffer)
  545        format(i5,1x,a68)
  547        format(6x,'Value = ',g12.5)
  550      continue
 1180      if(nbdata.eq.1) then
             item=1
             item=item+nadata
             goto 1190
           endif
           item=0
           CALL EASKI(item,' ',' Which item to change? ',
     &         1,'F',nbdata,'F',1,' BDATA item?',IER,nbhelp)
           if(ier.ne.0)goto 1180
           item=item+nadata
 1190      CALL EASKS(dtdesc(item),' ',' Item description? ',
     &             68,' ',' Item description?',IER,nbhelp)
           if(ier.ne.0)goto 1190
 1200      CALL EASKR(datams(item),' ',' Item value? ',0.,'-',
     &        0.,'-',0.,' item value?',IER,nbhelp)
           if(ier.ne.0)goto 1200
           ichang=1 
  560      continue
         endif

C Edit CDATA variables (description only).
         if(ncdata.gt.0) then
           dok=.false.
           helptopic='CDATA_changes_editing'
           call gethelptext(helpinsub,helptopic,nbhelp)
           CALL ASKOK(' ','Edit CDATA description?',OK,dok,nbhelp)
           IF(.NOT.OK)GOTO 590
           call edisp(iuout,' ')
           call edisp(iuout,' CDATA variables:')
           do 580 j=1,ncdata
             i=j+nadata+nbdata
             write(buffer,575) j,dtdesc(i)
             call edisp(iuout,buffer)
  575        format(i5,1x,a68)
  580      continue
 1210      if(ncdata.eq.1) then
             item=1
             item=item+nadata+nbdata
             goto 1220
           endif
           CALL EASKI(item,' ',' Which item to change? ',
     &        1,'F',ncdata,'F',1,' CDATA item?',IER,nbhelp)
           if(ier.ne.0)goto 1210
           item=item+nadata+nbdata
 1220      CALL EASKS(dtdesc(item),' ',' Item description? ',
     &             68,' ',' Item description?',IER,nbhelp)
           if(ier.ne.0)goto 1220
           ichang=1 
  590      continue
         endif

      elseif(ino.eq.7) then

C Save changes if OK.
        if(ichang.eq.1.or.ival.ne.0) then
          call pcwrt(iuout)
          dok=.true.
          helptopic='pdb_DATA_changes_pause'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' ',' Is this OK? ',OK,dok,nbhelp)
          IF(.NOT.OK)GOTO 20
          call pcput(indxpc)
          return
        endif

      elseif(ino.eq.9) then

C List data for current component.
         CALL PCWRT (IUCHAN)

      elseif(ino.eq.10) then
         
C Help for menu.
        helptopic='pdb_generic_type_descr'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('pdb editing',nbhelp,'-',0,0,IER)

      elseif(ino.eq.11) then
         
C Exit. Warn user if changes were made.
        if(ichang.eq.1) then
          dok=.false.
          helptopic='pdb_DATA_changes_pause'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok(' ','Really discard changes?',ok,dok,nbhelp)
          if(.not.ok) goto 20
        endif
        return
      endif
      goto 20    ! redisplay the menu

  888 RETURN

C Error handling
   1  CALL usrmsg(' pdb:pcedit unable to write dummy rec',' ','W')
      goto 888
      end

