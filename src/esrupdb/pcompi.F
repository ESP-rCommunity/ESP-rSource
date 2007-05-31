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

C This file contains the following subroutines:
C PCRD takes user input for a plant component
C PCRDF takes file input for a plant component
C PCRDFGDB reads and writes a plant component from a legacy asci file

C ********************* PCRD *********************

C PCRD of ESRUpdb, reads, interactively, data for one plant
C component entry from the user and puts it into the common
C blocks PCBLKA and PCBLKB.

      SUBROUTINE PCRD
      INCLUDE 'plantdb.h'
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

      CHARACTER*40 ZGTYPE
      CHARACTER*16 ZCDATE
      CHARACTER*80 ZCDESC
      character*68 dtdesc
      character*30 adodsc

      character buffer*120, ltmp*80, text*20,H*72
      CHARACTER*24 TUNIX

C Reset component index
      INDXPC=0

C Set date of insertion.
      CALL fdate(TUNIX)
      ZCDATE=TUNIX(9:10)//TUNIX(4:8)//TUNIX(23:24)//TUNIX(11:16)

C Generic type of plant component.
   10 call edisp(iuout,' ')
      call edisp(iuout,' COMPONENT DESCRIPTION')
      call edisp(iuout,' ---------------------')
      h(1)='The generic type describes the group under which a '
      h(2)='component comes under. For instance, a water pump '
      h(3)='generic type is used for both centrifugal and '
      h(4)='reciprocating water pumps.'
      h(5)='A maximum of 40 characters may be used.'
      CALL EASKS(zgtype,' Generic type (<= 40 char.)? ',
     &    ' ',40,' ',' Generic type',IER,5)
      IF(ier.ne.0) GOTO 10
      call edisp(iuout,' ')
      write(buffer,'(a,a)') ' Generic type: ',zgtype
      call edisp(iuout,buffer)

C Description of plant component.
   20 h(1)='The component description gives an explanation of a '
      h(2)='component such as a name and any relevant information'
      h(3)='to uniquely identify a component.'
      h(4)='A maximum of 80 characters may be used.'
      CALL EASKS(zcdesc,' Description (<= 80 char.)? ',
     &    ' ',80,' ','Component description',IER,4)
      IF(ier.ne.0) GOTO 20
      write(buffer,'(a,a)') ' Description: ',zcdesc
      call edisp(iuout,buffer)

C Component type index.
   40 iv=4
      h(1)='The component index indicates which type of component'
      h(2)='is required. At present two types are available.'
      h(3)='A single component and a meta component. A meta '
      h(4)='component comprises a number of single components'
      h(5)='already entered in the database. An example of a meta'
      h(6)='component is a VAV terminal reheat consisting of'
      h(7)='a heating coil, dampers and duct arrangement.'
      ictype=0
      CALL EASKI(ictype,' ',
     & ' Component  type index (0 for single, 1 for meta)? ',
     &             0,'F',1,'F',0,' Comp type index',IER,7)
      IF(ier.ne.0) GOTO 40
      if(ictype.eq.0) text='Single component    '
      if(ictype.eq.1) text='Meta component      '

C Component code.
   30 h(1)='The component code is an integer value which is used'
      h(2)='in ESRUbps to point to corresponding coefficient'
      h(3)='generator.'
      icode=0
      CALL EASKI(icode,' ',' Component model code ? ',
     &             0,'-',0,'-',10,'component code',IER,3)
      IF(ier.ne.0) GOTO 30
      write(buffer,31) text,icode
   31 format('    Type: ',a20,15X,' Component code : ',I6)
      call edisp(iuout,buffer)

C Case for single component.
      if(ictype.eq.0) then

C Number of nodes for nodal scheme of component.
   50    call edisp(iuout,' ')
         call edisp(iuout,' Nodal Scheme Description')
         nnode=0
         CALL EASKI(nnode,' ',' Number of nodes ? ',
     &             1,'F',maxnod,'F',1,'no. of nodes',IER,0)
         IF(ier.ne.0) GOTO 50

C Number of non-zero matrix coefficients.
   60    nmatx=0
         CALL EASKI(nmatx,' ',' No. of non-zero matrix coefficients? ',
     &        1,'F',maxmtx,'F',1,'no. of coefficients',IER,0)
         IF(ier.ne.0) GOTO 60
        WRITE (buffer,61) NNODE, NMATX
   61   FORMAT ('    Number of nodes  : ',I3,5X,
     &          ' No of nonzero matrix elements : ',I6)
        call edisp(iuout,buffer)

C Position of non-zero matrix coefficients.
   70    write(buffer,1070) nmatx
 1070    format('Enter position of',i5,
     &          ' non-zero matrix coefficients?')
         h(1)='Give a list of integer values which represent the'
         h(2)='position of non-zero matrix coefficients. '
         h(3)='For example the following matrix contains 2 non-'
         h(4)='zero coefficients:'
         h(5)='        |  X  0  | '
         h(6)='        |  0  X  | '
         h(7)='the positions of the non-zero corfficients are'
         h(8)=' 1 and 4.'
         ltmp=' '
         CALL EASKS(ltmp,buffer,' ',80,' ','coeff position?',IER,8)
         IF(ier.ne.0) GOTO 70
         k=0
         ierl=0
         nnode2=nnode*nnode
         do 71 i=1,nmatx
            call egetwi(ltmp,k,ncpos(i),0,nnode2,'F','non-0',ierl)
            if(ierl.ne.0) goto 70
   71    continue

C Write position of non-zero matrix coefficients.
         nlines=((nmatx-1)/10)+1
         do 73 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nmatx
            if(jj.eq.1) WRITE (buffer,74) (NCPOS(J),J=jj,jjj)
   74       FORMAT ('    Matrix positions : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,75) (NCPOS(J),J=jj,jjj)
   75       FORMAT (21X,': ',10(I3,', '))
            call edisp(iuout,buffer)
   73    continue

C External connectivity of each node.
   80    h(1)='Give a list of integer values which represent the'
         h(2)='connectivity value of each node. '
         h(3)='For example if a node expects to recieve 2 connec-'
         h(4)='tions then a value of 2 is specified for it.'
         ltmp=' '
         CALL EASKS(ltmp,' Connectivity of node(s) ?',
     &       ' ',80,' ','connection values?',IER,4)
         IF(ier.ne.0) GOTO 80
         k=0
         ierl=0
         do 81 i=1,nnode
            call egetwi(ltmp,k,ndcon(i),0,4,'F',
     &        'ext connect',ierl)
            if(ierl.ne.0) goto 80
   81    continue

C Write external connections for each node.
         nlines=((nnode-1)/10)+1
         do 82 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,83) (NDCON(J),J=jj,jjj)
   83       FORMAT ('    Node connections : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,84) (NDCON(J),J=jj,jjj)
   84       FORMAT (21X,': ',10(I3,', '))
            call edisp(iuout,buffer)
   82    continue

C State variable definition.
   90    h(1)='The state variable index indicates the functionality'
         h(2)='of a plant component model and the nature of a node.'
         h(3)='The following index values are supported: '
         h(4)=' '
         h(5)='       Energy     Energy&1st phase     Energy&2 phase'
         h(6)='        only        mass balance        mass balances'
         h(7)='water    0              10                   20'
         h(8)='air      1              11                   21'
         h(9)='solid    9              19                   29'
         ltmp=' '
         CALL EASKS(ltmp,' State variable index of node(s) ?',
     &       ' ',80,' ','state variable index?',IER,9)
         IF(ier.ne.0) GOTO 90
         k=0
         ierl=0
         do 91 i=1,nnode
            call egetwi(ltmp,k,isv(i),0,29,'F','state var',ierl)
            if(ierl.ne.0) goto 90
   91    continue

C Write state variable index for each node.
         nlines=((nnode-1)/10)+1
         do 92 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,93) (ISV(J),J=jj,jjj)
   93       FORMAT ('    Variable type    : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,94) (ISV(J),J=jj,jjj)
   94       FORMAT (21X,': ',10(I3,', '))
            call edisp(iuout,buffer)
   92    continue

C Case for meta component.
      elseif(ictype.eq.1) then

C Number of components.
  100    mncomp=0
         CALL EASKI(mncomp,' ',
     &      ' Total number of components? ',1,'F',maxpc,'F',1,
     &        ' no. of components',IER,2)
         IF(ier.ne.0) GOTO 100
         call edisp(iuout,' ')
         call edisp(iuout,' Meta component description')
         write(buffer,101) mncomp
  101    format('    Number of components: ',i3)
         call edisp(iuout,buffer)

C Code number of each component.
  110    h(1)='A meta component comprises a number of components'
         h(2)='which exist in database. In order to identify'
         h(3)='these components, their code numbers must be'
         h(4)='specified.'
         ltmp=' '
         CALL EASKS(ltmp,' Code number of each component ?',
     &       ' ',80,' ','conde of each component',IER,8)
         IF(ier.ne.0) GOTO 110
         k=0
         ierl=0
         do 111 i=1,mncomp
            call egetwi(ltmp,k,micode(i),0,0,'-','code no',ierl)
            if(ierl.ne.0) goto 110
  111    continue

C Write code number of each component.
         nlines=((mncomp-1)/10)+1
         do 112 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=mncomp
            if(jj.eq.1) WRITE (buffer,113) (micode(J),J=jj,jjj)
  113       FORMAT ('    Components code : ',6(I3,', '))
            if(jj.ne.1) WRITE (buffer,114) (micode(J),J=jj,jjj)
  114       FORMAT (21X,': ',6(I3,', '))
            call edisp(iuout,buffer)
  112    continue

C Number of connections in meta component.
  120    mncon=0
         CALL EASKI(mncon,' ',
     &      ' Total number of connections? ',1,'F',maxcon,'F',1,
     &        ' no. of connections',IER,2)
         IF(ier.ne.0) GOTO 120
         write(buffer,121) mncon
  121    format('    Number of component inter-connections : ',i5)
         call edisp(iuout,buffer)
         call edisp(iuout,' ')

C Connections data.
  130    h(1)=' Describe each connection using the following format'
         h(2)=' C1  N1  CT  C2  N2  MDR  MISC1  MISC2'
         h(3)=' Where:'
         h(4)=' C1   : Sending component'
         h(5)=' N1   : Sending node'
         h(6)=' CT   : Connection type'
         h(7)=' C2   : Receiving component'
         h(8)=' N2   : Receiving node'
         h(9)=' MDR  : Mass diversion ratio'
         h(10)=' MISC1: First miscellaneous data'
         h(11)=' MISC2: Second miscellaneous data'
         do 131 i=1,mncon
            write(buffer,132) i
  132       format(' Data for connection',i5,' ?')
            ltmp=' '
            CALL EASKS(ltmp,buffer,
     &       ' ',80,' ','connection values?',IER,11)
           IF(ier.ne.0) GOTO 130
            k=0
            ierl=0
            do 133 j=1,5
              call egetwi(ltmp,k,icnx,0,0,'-','connec',ierl)
               if(ierl.ne.0) goto 130
               icndat(i,j)=icnx
  133       continue
            ierl=0
            do 134 j=1,3
              call egetwr(ltmp,k,rc,0.,0.,'-','connec',ierl)
               if(ierl.ne.0) goto 130
               rcndat(i,j)=rc
  134       continue
  131    continue

C Write component inter-connections.
         call edisp(iuout,' Description of component inter-connections')
         call edisp(iuout,' ')
         write(buffer,135)
  135    format(6x,'C1',5x,'N1',5x,'CT',5x,'C2',5x,'N2',7x,'MDR',
     &         6x,'MISC1',5x,'MISC2')
         call edisp(iuout,buffer)
         do 137 i=1,mncon
            write(buffer,136) (icndat(i,l),l=1,5),(rcndat(i,ll),ll=1,3)
  136       format(5(2x,i5),2x,f10.3,2(2x,f8.3))
            call edisp(iuout,buffer)
  137    continue

      endif

C Miscellaneous data items definition.
  140 h(1)='The ADATA array holds static data for a component and'
      h(2)='their values are assumed to be fixed throughout the'
      h(3)='simulation. The BDATA on the other hand holds dynamic'
      h(4)='data which is expected to change in a simulation.'
      h(5)='The CDATA values are specified in ESRUbps configuration'
      h(6)='file and are used for control on heat flux, mass flow '
      h(7)='rates, humidity ratio ..etc.'
      ltmp=' '
      CALL EASKS(ltmp,' Number of items for ADATA, BDATA and CDATA?',
     &       ' ',80,' ','ADATA, BDATA and CDATA',IER,7)
      IF(ier.ne.0) GOTO 140
      k=0
      ierl=0
      call egetwi(ltmp,k,nadata,0,0,'-','no. adata',ierl)
      call egetwi(ltmp,k,nbdata,0,0,'-','no. bdata',ierl)
      call egetwi(ltmp,k,ncdata,0,0,'-','no. cdata',ierl)
      if(ierl.ne.0) goto 140
      nmisc=nadata+nbdata
      if(nmisc.lt.0.or.nmisc.gt.maxmsc) goto 140

C Read ADATA.
      if(nadata.gt.0) then
         call edisp(iuout,' ADATA variables:')
         do 152 i=1,nadata
  151       write(buffer,1150) i
 1150       format(' Enter description (68 char max) for ADATA item',i5)
            ltmp=' '
            CALL EASKS(ltmp,buffer,' ',80,' ','item description',IER,0)
            dtdesc(i)=ltmp(1:68)
            vitem=0.0
            CALL EASKR(vitem,' ',' Item value? ',0.,'-',0.,'-',0.,
     &        ' item value?',IER,2)
            datams(i)=vitem
            ltmp=' '
            CALL EASKS(ltmp,' Range (Min Max)?',
     &       ' ',80,' ','range',IER,0)
           IF(ier.ne.0) GOTO 151
            k=0
            ierl=0
            do 153 j=1,2
               call egetwr(ltmp,k,ra,0.,0.,'-','adata',ierl)
               if(ierl.ne.0) goto 151
               range(i,j)=ra
  153       continue
            if(datams(i).lt.range(i,1).or.datams(i).gt.range(i,2))
     &          goto 151

C Write entered data in text window.
            write(buffer,154) i,dtdesc(i)
  154       format(i5,1x,a68)
            call edisp(iuout,buffer)
            write(buffer,155) datams(i),range(i,1),range(i,2)
  155       format(6x,'Value = ',g12.5,4x,
     &               'Range (Min Max): ',2(2x,g12.5))
            call edisp(iuout,buffer)
  152    continue
      endif

C Then BDATA.
      if(nbdata.gt.0) then
         call edisp(iuout,' BDATA variables:')
  160    do 162 i=1,nbdata
            ii=i+nadata
            write(buffer,1160) i
 1160       format(' Enter description (68 char max) for BDATA item',i5)
            ltmp=' '
            CALL EASKS(ltmp,buffer,' ',80,' ','item description',IER,0)
            dtdesc(ii)=ltmp(1:68)
            vitem=0.0
            CALL EASKR(vitem,' ',' Item value? ',0.,'-',0.,'-',0.,
     &        ' item value?',IER,2)
            datams(ii)=vitem
            ltmp=' '
            CALL EASKS(ltmp,' Range (Min Max)?',
     &       ' ',80,' ','range',IER,0)
           IF(ier.ne.0) GOTO 160
            k=0
            ierl=0
            do 163 j=1,2
               call egetwr(ltmp,k,ra,0.,0.,'-','bdata',ierl)
               if(ierl.ne.0) goto 160
               range(ii,j)=ra
  163       continue
            if(datams(ii).lt.range(ii,1).or.datams(ii).gt.range(ii,2))
     &          goto 160

C Write enterd data in text window.
            write(buffer,164) i,dtdesc(ii)
  164       format(i5,1x,a68)
            call edisp(iuout,buffer)
            write(buffer,165) datams(ii),range(ii,1),range(ii,2)
  165       format(6x,'Value = ',g12.5,4x,
     &               'Range (Min Max): ',2(2x,g12.5))
            call edisp(iuout,buffer)
  162    continue
      endif

C Then CDATA (description only).
      if(ncdata.gt.0) then
         call edisp(iuout,' CDATA variables:')
  170    do 172 i=1, ncdata
            write(buffer,1170) i
 1170       format(' Enter description (68 char max) for CDATA item',i5)
            ltmp=' '
            CALL EASKS(ltmp,buffer,' ',80,' ','item description',IER,0)
            IF(ier.ne.0) GOTO 170
            dtdesc(i+nmisc)=ltmp(1:68)

C Write enterd data in text window.
            write(buffer,174) i,dtdesc(i+nmisc)
  174       format(i5,1x,a68)
            call edisp(iuout,buffer)
  172    continue
      endif

C Read number of additional outputs and description of
C each output.
  180 napout=0
      CALL EASKI(napout,' ',
     &    ' Number of additional outputs? ',0,'F',20,'F',0,
     &     ' no. of outputs',IER,0)
      IF(ier.ne.0) GOTO 180
      write(buffer,181) napout
  181 format('    Number of additional outputs : ',i5)
      call edisp(iuout,buffer)
      if(napout.gt.0) then
         call edisp(iuout,' ADDOUT variables:')
         iv=19
         do 192 i=1, napout
            write(buffer,1190) i
 1190       format(' Enter description (30 char max) for ADDOUT item',
     &             i5)
            ltmp=' '
            CALL EASKS(ltmp,buffer,
     &       ' ',80,' ','item description',IER,0)
            adodsc(i)=ltmp(1:30)
            vitem=0.0
            CALL EASKR(vitem,' ',' output type? ',0.,'-',0.,'-',0.,
     &        ' output type?',IER,2)
            IF(ier.ne.0) GOTO 180
            noutyp(i)=int(vitem)

C Write enterd data in text window.
            write(buffer,194) i,adodsc(i),noutyp(i)
  194       format(i5,1x,a30,1x,' TYPE :',i5)
            call edisp(iuout,buffer)
  192    continue
      endif
      call edisp(iuout,' ')

C Set internal index
      INDXPC=NPC+1
      RETURN

      END

C ********************* PCRDF ********************
C PCRDF of ESRUpdb, reads, from file (unit=IUFIL), the data
C for one plant component entry and puts it into the common
C blocks PCBLKA and PCBLKB.
      SUBROUTINE PCRDF(IUFIL)
      INCLUDE 'plantdb.h' 
      common/OUTIN/IUOUT,IUIN
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Mass flow components data.
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C TRNSYS types data.
      common/trnsys/ntypes,ittype(MXTSYS),ntadat(MXTSYS),ntbdat(MXTSYS),
     &              ntcdat(MXTSYS),ntapot(MXTSYS),ntmisc(MXTSYS),
     &              tdtdsc(MXTSYS,maxmsc),tdatms(MXTSYS,maxmsc),
     &              trange(MXTSYS,maxmsc,2),tadods(MXTSYS,mxaout),
     &              ntaotp(MXTSYS,mxaout),ittpcm(MXTSYS,maxcon),
     &              itsdcm(MXTSYS,maxcon),itsdcn(MXTSYS,maxcon),
     &              tsupcm(MXTSYS,maxcon,maxflw),
     &              tmfsds(MXTSYS,maxcon,maxflw),
     &              tltpcm(MXTSYS,maxcon),itsdif(MXTSYS,maxcon)

      CHARACTER*40 ZGTYPE
      CHARACTER*16 ZCDATE
      CHARACTER*80 ZCDESC
      CHARACTER*23 ZDESC
      character*68 dtdesc, mfsdsc, tdtdsc, tmfsds
      character*30 adodsc, tadods
      character*60 ltpcmp, tltpcm

      character*20 ctype

C Reset component index.
      INDXPC=-1

C Skip to next entry.
   10 READ (IUFIL,1010,IOSTAT=ISTAT,ERR=1,END=900) ZDESC
 1010 FORMAT (A23)
      IF(ZDESC(1:22).NE.' Component Description') GOTO 10

C Generic type of plant component.
      READ(IUFIL,10201,IOSTAT=ISTAT,ERR=1) ZGTYPE
10201 FORMAT(23X,A40)

C Description of plant component.
      READ(IUFIL,10202,IOSTAT=ISTAT,ERR=1) ZCDESC
10202 FORMAT(23X,A80)

C Insertion date.
      READ(IUFIL,10203,IOSTAT=ISTAT,ERR=1) ZCDATE
10203 FORMAT(23X,A16)

C Component type and code.
      read(IUFIL,1100,IOSTAT=ISTAT,ERR=1) ctype,icode
 1100 format(10x,a20,33x,i6)

C Case for single component.
      if(ctype(1:20).eq.'Single component    '.or.
     &   ctype(1:20).eq.'TRNSYS component    ') then
         if(ctype(1:20).eq.'Single component    ') ictype=0
         if(ctype(1:20).eq.'TRNSYS component    ') ictype=2

C Read number of nodes for nodal scheme of component and
C number of matrix locations filled with coefficients.
         read(iufil,10204,iostat=istat,err=1) nnode, nmatx
10204    format(//,23x,i3,38x,i6)
         if(nnode.le.0.or.nnode.gt.maxnod) goto 1
         if(nmatx.le.0.or.nmatx.gt.maxmtx) goto 1

C Position of nonzero matrix locations.
         nlines=((nmatx-1)/10)+1
         do 30 k=1,nlines
            nl1=((k-1)*10)+1
            nl2=nl1+9
            if(k.eq.nlines) nl2=nmatx
            read(iufil,10301,iostat=istat,err=1) (ncpos(l),l=nl1,nl2)
   30    continue
10301    format(21x,10(2x,i3))
         nnode2=nnode*nnode
         do 32 i=1,nmatx
            if(ncpos(i).lt.0.or.ncpos(i).gt.nnode2) goto 1
   32    continue

C External connectivity of each node.
         nlines=((nnode-1)/10)+1
         do 34 k=1,nlines
         nl1=((k-1)*10)+1
         nl2=nl1+9
         if(k.eq.nlines) nl2=nnode
         read(iufil,10301,iostat=istat,err=1) (ndcon(l),l=nl1,nl2)
   34    continue

C State variable definition.
         nlines=((nnode-1)/10)+1
         do 36 k=1,nlines
            nl1=((k-1)*10)+1
            nl2=nl1+9
            if(k.eq.nlines) nl2=nnode
            read(iufil,10301,iostat=istat,err=1) (isv(l),l=nl1,nl2)
   36    continue

C Case for meta component.
      elseif(ctype(1:20).eq.'Meta component      ') then
         ictype=1

C Read number of components in meta component.
         read(iufil,1050,iostat=istat,err=1) mncomp
 1050    format(//,26x,i3)
         nlines=((mncomp-1)/6)+1
         do 200 k=1,nlines
            nl1=((k-1)*6)+1
            nl2=nl1+5
            if(k.eq.nlines) nl2=mncomp
            read(iufil,1060,iostat=istat,err=1) (micode(l),l=nl1,nl2)
 1060    format(26x,6(i6,2x))
  200    continue

C Read number of component inter-connections.
         read(iufil,1070,iostat=istat,err=1) mncon
 1070    format(44x,i5)
         read(iufil,1075,iostat=istat,err=1)
 1075    format(//)

C Read description of each connection.
         do 210 i=1,mncon
            read(iufil,1080,iostat=istat,err=1) (icndat(i,l),l=1,5)
     &         ,(rcndat(i,ll),ll=1,3)
 1080       format(5(2x,i5),2x,f10.3,2(2x,f8.3))
  210    continue
      endif

C Number of miscellaneous data items associated with this component.
      if(ictype.eq.0) then
         read(iufil,1038,iostat=istat,err=1) nadata,nbdata,ncdata
 1038    format (//,3(20x,i3))
         nmisc=nadata+nbdata
         if(nmisc.lt.0.or.nmisc.gt.maxmsc) goto 1

C Miscellaneous data.
C First read ADATA.
         if(nadata.gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 140 i=1,nadata
               read(iufil,1040,iostat=istat,err=1)
     &             dtdesc(i),datams(i),range(i,1),range(i,2)
 1040          format(6x,a68,/,15x,g12.5,21x,2(2x,g12.5))
  140       continue
         endif

C Then BDATA.
         if(nbdata.gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 150 j=1,nbdata
               i=j+nadata
               read(iufil,1040,iostat=istat,err=1)
     &             dtdesc(i),datams(i),range(i,1),range(i,2)
  150       continue
         endif

C Then CDATA (description only).
         if(ncdata.gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 160 j=1,ncdata
               i=j+nmisc
               read(iufil,1065,iostat=istat,err=1)
     &             dtdesc(i)
 1065          format(6x,a68)
  160      continue
         endif

C Read number of additional ouput parameters.
         read(iufil,*,iostat=istat,err=1)
         read(iufil,1067,iostat=istat,err=1) napout
         read(iufil,*,iostat=istat,err=1)
         if(napout.gt.0) then
            do 170 j=1,napout
               read(iufil,1085,iostat=istat,err=1)
     &             adodsc(j),noutyp(j)
 1085          format(6x,a30,10x,i5)
  170       continue
 1067       format(41x,i5)
            read(iufil,*,iostat=istat,err=1)
         endif

C For each external connection, read mass flow component data.
C First find total number of connections.
         nconns=0
         do 2000 inod=1,nnode
 2000      if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
         if(nconns.gt.0) then
            read(iufil,*)
            do 2010 icon=1, nconns
               read(iufil,'(a60)',iostat=istat,err=1)ltpcmp(icon)
               read(iufil,2038,iostat=istat,err=1)
     &          itpcmp(icon),isdifc(icon),isdcmp(icon),isdcnn(icon)
               do 2020 isup=1, isdcmp(icon)
                 read(iufil,2040,iostat=istat,err=1)
     &            mfsdsc(icon,isup), supcmp(icon,isup)
 2040            format(a68,1x,g11.5)
 2020          continue
 2010       continue
 2038       format ((7x,i4,20x,i4,20x,i4,18x,i4))
         endif
      endif

C Case for TRNSYS type components.
      if(ictype.eq.2) then
         read(iufil,'(25x,i5)',iostat=istat,err=1) ntypes
         read(iufil,'(26x,50(i5,1x))',iostat=istat,err=1) 
     &        (ittype(ityp),ityp=1,ntypes)
         do 3000 ityp=1, ntypes
            read(iufil,1038,iostat=istat,err=1) 
     &         ntadat(ityp),ntbdat(ityp),ntcdat(ityp)
            ntmisc(ityp)=ntadat(ityp)+ntbdat(ityp)
            if(ntmisc(ityp).lt.0.or.ntmisc(ityp).gt.maxmsc) goto 1

C Miscellaneous data.
C First read ADATA.
            if(ntadat(ityp).gt.0) then
               read(iufil,*,iostat=istat,err=1)
               do 3140 i=1,ntadat(ityp)
                  read(iufil,1040,iostat=istat,err=1)
     &              tdtdsc(ityp,i),tdatms(ityp,i),trange(ityp,i,1),
     &              trange(ityp,i,2)
 3140          continue
            endif

C Then BDATA.
            if(ntbdat(ityp).gt.0) then
               read(iufil,*,iostat=istat,err=1)
               do 3150 j=1,ntbdat(ityp)
                  i=j+ntadat(ityp)
                  read(iufil,1040,iostat=istat,err=1)
     &              tdtdsc(ityp,i),tdatms(ityp,i),trange(ityp,i,1),
     &              trange(ityp,i,2)
 3150          continue
            endif

C Then CDATA (description only).
            if(ntcdat(ityp).gt.0) then
               read(iufil,*,iostat=istat,err=1)
               do 3160 j=1,ntcdat(ityp)
                  i=j+ntmisc(ityp)
                  read(iufil,1065,iostat=istat,err=1)
     &               tdtdsc(ityp,i)
 3160         continue
            endif

C Read number of additional ouput parameters.
            read(iufil,*,iostat=istat,err=1)
            read(iufil,1067,iostat=istat,err=1) ntapot(ityp)
            read(iufil,*,iostat=istat,err=1)
            if(ntapot(ityp).gt.0) then
               do 3170 j=1,ntapot(ityp)
                  read(iufil,1085,iostat=istat,err=1)
     &              tadods(ityp,j),ntaotp(ityp,j)
 3170          continue
               read(iufil,*,iostat=istat,err=1)
            endif

C For each external connection, read mass flow component data.
C First find total number of connections.
            nconns=0
            do 4000 inod=1,nnode
 4000         if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
            if(nconns.gt.0) then
               read(iufil,*)
               do 4010 icon=1, nconns
                  read(iufil,'(a60)',iostat=istat,err=1)
     &              tltpcm(ityp,icon)
                  read(iufil,2038,iostat=istat,err=1)
     &              ittpcm(ityp,icon),itsdif(ityp,icon),
     &              itsdcm(ityp,icon),itsdcn(ityp,icon)
                  do 4020 isup=1, itsdcm(ityp,icon)
                    read(iufil,2040,iostat=istat,err=1)
     &               tmfsds(ityp,icon,isup),tsupcm(ityp,icon,isup)
 4020             continue
 4010          continue
            endif
 3000    continue
      endif
         
C Set index for this entry.
      INDXPC=NPC+1
  100 RETURN

C EOF encountered.
  900 call usrmsg(' ',' PCRDF: End-Of-File encountered','-')
      INDXPC=0
      GOTO 100

C Error handling.
  1   CALL usrmsg(' Pdb:pcrdf error reading db',' ','W')
      goto 100
      END

C ********************* PCRDFGDB ********************
C PCRDFGDB reads and writes a plant component from a legacy asci file
C (unit=IUFIL), and puts it into the common blocks PCBLKA and PCBLKB as
C well as writing a text file entry in generic db (unit=IUFIL+1 which
C is assumed to already be open)
C if act is 'h' write the header boilerplate
C if act is 'g' write generic item.
      SUBROUTINE PCRDFGDB(IUFIL,act)
      INCLUDE 'plantdb.h' 
      INCLUDE 'esprdbfile.h'
      common/OUTIN/IUOUT,IUIN
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)
      common/PCDATC/NPC,ITMLOC(MAXPC,2)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Mass flow components data.
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C TRNSYS types data.
      common/trnsys/ntypes,ittype(MXTSYS),ntadat(MXTSYS),ntbdat(MXTSYS),
     &              ntcdat(MXTSYS),ntapot(MXTSYS),ntmisc(MXTSYS),
     &              tdtdsc(MXTSYS,maxmsc),tdatms(MXTSYS,maxmsc),
     &              trange(MXTSYS,maxmsc,2),tadods(MXTSYS,mxaout),
     &              ntaotp(MXTSYS,mxaout),ittpcm(MXTSYS,maxcon),
     &              itsdcm(MXTSYS,maxcon),itsdcn(MXTSYS,maxcon),
     &              tsupcm(MXTSYS,maxcon,maxflw),
     &              tmfsds(MXTSYS,maxcon,maxflw),
     &              tltpcm(MXTSYS,maxcon),itsdif(MXTSYS,maxcon)

C Generic db commons.
      common/gendb1/idbwidth(13),lastr(13),nbdbdoc(13),nbunit(13),
     &              nbsrc(13),nbcat(13),nbitem(13),idbver(13)     

C Database tag string, db menu entry, data stamp, db log string, doc string(s).
      common/gendb2/dbtag(13),dbmenu(13),dbdate(13),dbnote(13),dbdoc(10)

C Units common.
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)

C Sources common.
      common/gendbs/srcchr(24),srcnote(24)

C Record starts for each cat and number of documents.
      common/gendb3/irscat(MGCL),nbcatdoc(MGCL),nbcatit(MGCL)

C Category tag string, menu entry, data stamp, doc string(s).
      common/gendb4/cattag(MGCL),cattype(MGCL),catmenu(MGCL),
     &              catdate(MGCL),catdoc(5)

      common/gendb6/ittag(MGIT),itmenu(MGIT)
      common/gendb8a/ituntstr,itsrcstr,itcnfstr,itdate(MGIT),itdoc
      common/gendb8c/nbitusc(MGIT),nbitdat(MGIT),
     &               nbitrep(MGIT),ibitusc(MGIT)

C To keep track of the number of components (this routine is called
C once per item until the end-of-file.
      common/totconv/itotconv
      common/SFIG/NSIGFIG

      CHARACTER ZGTYPE*40,ZCDATE*16,ZCDESC*80,ZDESC*23
      character dtdesc*68,mfsdsc*68,tdtdsc*68,tmfsds*68
      character adodsc*30,tadods*30,ltpcmp*60,tltpcm*60,ctype*20

C Generic strings.
      character tab*1,class*12,h*72,act*1,fmt*10,qu*1,uchr*1,t68*68
      character dbtag*12,dbmenu*32,dbdate*24,dbdoc*248,catdoc*248
      character cattag*12,cattype*8,catmenu*32,catdate*24,itdoc*248
      character ittag*12,itmenu*32,itdate*24
      character dbnote*72,outs*248,outsd*248,dstmp*24
      character ituntstr*42,itsrcstr*1,itcnfstr*42
      character unitchr*4,unitstr*12,unitnote*72
      character srcchr*4,srcnote*248
      character ihold*80,SIGSTR*12,sigstra*12,sigstrb*12

C Temporary place holder for units discovered as the component is
C read in.
      character tustr(72)*1
      logical start

C Setup a tab character and fill tustr with dashes.
      do 2 i=1,72
        tustr(i)='-'
  2   continue
      tab=','
      qu=CHAR(39)
      NSIGFIG=4

C If header needs to be written
C plant templates on IFIL+14, igendbf(12)=IFIL+14 so use
C array index 12.
      idbh=12
      ifa=IUFIL+1
      if(act.eq.'h')then
        itotconv=0
        idbver(idbh)=2
        write(IFA,'(a)') '*ESP-r ascii gdb 2'
        write(IFA,'(a)')
     &    '# db type, menu, width,nb of units:sources:cat:items, date'
        call dstamp(dstmp)
        write(dbdate(idbh),'(a)') dstmp
        dbtag(idbh)='plant_db'
        dbmenu(idbh)='Plant components db'
        idbwidth(idbh)=65
        nbunit(idbh)=19
        nbsrc(idbh)=2
        nbcat(idbh)=1
        nbitem(idbh)=99
        write(IFA,'(4a,i2,a,i2,a,i2,a,i2,a,i3,2a)')
     &    dbtag(idbh)(1:lnblnk(dbtag(idbh))),tab,
     &    dbmenu(idbh)(1:lnblnk(dbmenu(idbh))),tab,
     &    idbwidth(idbh),tab,nbunit(idbh),tab,nbsrc(idbh),tab,
     &    nbcat(idbh),tab,nbitem(idbh),tab,dbdate(idbh)

        dbnote(idbh)='Plant components translated from esp-r.'
        write(IFA,'(a,a1,a)')'*DBNOTE',tab,
     &    dbnote(idbh)(1:lnblnk(dbnote(idbh)))

        unitchr(1)='-'
        unitstr(1)='(-)'
        unitnote(1)='(-)'
        unitchr(2)='a'
        unitstr(2)='(kg)'
        unitnote(2)='(mass kg)'
        unitchr(3)='b'
        unitstr(3)='(J/kgK)'
        unitnote(3)='(specific heat J/kgK)'
        unitchr(4)='c'
        unitstr(4)='(W/K)'
        unitnote(4)='(UA W/K)'
        unitchr(5)='d'
        unitstr(5)='(m)'
        unitnote(5)='(length m)'
        unitchr(6)='e'
        unitstr(6)='(m^2)'
        unitnote(6)='(area m^2)'
        unitchr(7)='f'
        unitstr(7)='(m/s)'
        unitnote(7)='(velocity m/s)'
        unitchr(8)='g'
        unitstr(8)='(Pa/(m^3/s)'
        unitnote(8)='(coef Pa/(m^3/s)^?)'
        unitchr(9)='h'
        unitstr(9)='(kg/s/Pa^b)'
        unitnote(9)='(coef kg/s/Pa^b)'
        unitchr(10)='i'
        unitstr(10)='(kg/s)'
        unitnote(10)='(flow rate kg/s)'
        unitchr(11)='j'
        unitstr(11)='(m^3/s)'
        unitnote(11)='(flow rate m^3/s)'
        unitchr(12)='k'
        unitstr(12)='(%)'
        unitnote(12)='(percentage)'
        unitchr(13)='l'
        unitstr(13)='(C)'
        unitnote(13)='(deg C)'
        unitchr(14)='m'
        unitstr(14)='(K/W)'
        unitnote(14)='(Res K/W)'
        unitchr(15)='n'
        unitstr(15)='(Deg)'
        unitnote(15)='(Lat/Long/tilt degree)'
        unitchr(16)='o'
        unitstr(16)='(kg/m^3)'
        unitnote(16)='(density kg/m^3)'
        unitchr(17)='p'
        unitstr(17)='(W)'
        unitnote(17)='(Energy W)'
        unitchr(18)='q'
        unitstr(18)='(J/m^3)'
        unitnote(18)='(Gas value J/m^3)'
        unitchr(19)='r'
        unitstr(19)='(C/W)'
        unitnote(19)='(Res C/W)'
        do 4 inbu=1,nbunit(idbh)
          write(IFA,'(7a)')'*UNIT',tab,unitchr(inbu)(1:1),tab,
     &        unitstr(inbu)(1:lnblnk(unitstr(inbu))),tab,
     &        unitnote(inbu)(1:lnblnk(unitnote(inbu)))
  4     continue
        
        srcchr(1)='-'
        srcnote(1)='no source documentation supplied (yet)'
        srcchr(2)='a'
        srcnote(2)='from an esp-r components database'
        do 5 inbsrc=1,nbsrc(idbh)
          write(IFA,'(5a)')'*SOURCE',tab,srcchr(inbsrc)(1:1),tab,
     &      srcnote(inbsrc)(1:lnblnk(srcnote(inbsrc)))
  5     continue

        nbdbdoc(idbh)=1
        dbdoc(1)='Database documentation to be inserted here'
        write(IFA,'(2a)') '*DD ',dbdoc(1)(1:lnblnk(dbdoc(1)))

        icl=1
        cattag(1)='converted'
        cattype(1)='STD     '
        catmenu(1)='Converted plant components'
        nbcatit(1)=99
        write(catdate(1),'(a)') dstmp
        write(IFA,'(a)')'# CAT tag,id,type,menu,items_in_cat,date'
        write(IFA,'(8a,i2,2a)')'*CATEG',tab,
     &    cattag(icl)(1:lnblnk(cattag(icl))),tab,
     &    cattype(icl)(1:lnblnk(cattype(icl))),tab,
     &    catmenu(icl)(1:lnblnk(catmenu(icl))),tab,
     &    nbcatit(icl),tab,catdate(icl)

        nbcatdoc(1)=1
        catdoc(1)='Category documentation to be inserted here.'
        write(IFA,'(2a)') '*CD ',catdoc(1)(1:lnblnk(catdoc(1)))

        return
      endif

C Reset component index.
      INDXPC=-1

C Skip to next entry.
   10 READ (IUFIL,'(a23)',IOSTAT=ISTAT,ERR=1,END=900) ZDESC
      IF(ZDESC(1:22).NE.' Component Description') GOTO 10

C Generic type of plant component.
      READ(IUFIL,'(23X,A40)',IOSTAT=ISTAT,ERR=1) ZGTYPE

C Description of plant component.
      READ(IUFIL,'(23X,A80)',IOSTAT=ISTAT,ERR=1) ZCDESC

C Insertion date.
      READ(IUFIL,'(23X,A16)',IOSTAT=ISTAT,ERR=1) ZCDATE

C Component type and code.
      read(IUFIL,'(10x,a20,33x,i6)',IOSTAT=ISTAT,ERR=1) ctype,icode

C Case for single component.
      if(ctype(1:20).eq.'Single component    '.or.
     &   ctype(1:20).eq.'TRNSYS component    ') then
        if(ctype(1:20).eq.'Single component    ') ictype=0
        if(ctype(1:20).eq.'TRNSYS component    ') ictype=2

C Increment the generic items.
        itotconv=itotconv+1
        iit=itotconv

C Set position in unit string to zero
        iupos=0

C Read number of nodes for nodal scheme of component and
C number of matrix locations filled with coefficients.
        read(iufil,10204,iostat=istat,err=1) nnode, nmatx
10204   format(//,23x,i3,38x,i6)
        if(nnode.le.0.or.nnode.gt.maxnod) then
          write(6,*) 'Component ',ctype,icode,' has ',nnode,
     &      'which is not allowed.'
          goto 1
        endif
        if(nmatx.le.0.or.nmatx.gt.maxmtx) then
          write(6,*) 'Component ',ctype,icode,' has ',nmatx,
     &      ' matrix locations which is not allowed.'
          goto 1
        endif

C Position of nonzero matrix locations.
        nlines=((nmatx-1)/10)+1
        do 30 k=1,nlines
          nl1=((k-1)*10)+1
          nl2=nl1+9
          if(k.eq.nlines) nl2=nmatx
          read(iufil,10301,iostat=istat,err=1) (ncpos(l),l=nl1,nl2)
   30   continue
10301   format(21x,10(2x,i3))
        nnode2=nnode*nnode
        do 32 i=1,nmatx
          if(ncpos(i).lt.0.or.ncpos(i).gt.nnode2) then
            write(6,*) 'Component ',ctype,icode,i,' has ',ncpos(i),
     &      ' matrix position which is not allowed.'
            goto 1
          endif
   32   continue

C External connectivity of each node.
        nlines=((nnode-1)/10)+1
        do 34 k=1,nlines
          nl1=((k-1)*10)+1
          nl2=nl1+9
          if(k.eq.nlines) nl2=nnode
          read(iufil,10301,iostat=istat,err=1) (ndcon(l),l=nl1,nl2)
   34   continue

C State variable definition.
        nlines=((nnode-1)/10)+1
        do 36 k=1,nlines
          nl1=((k-1)*10)+1
          nl2=nl1+9
          if(k.eq.nlines) nl2=nnode
          read(iufil,10301,iostat=istat,err=1) (isv(l),l=nl1,nl2)
   36   continue

C Case for meta component.
      elseif(ctype(1:20).eq.'Meta component      ') then
        write(6,*) 'Component ',ctype,icode,' is meta ',
     &      ' which is not yet decoded.'
        ictype=1

C Read number of components in meta component.
        read(iufil,1050,iostat=istat,err=1) mncomp
 1050   format(//,26x,i3)
        nlines=((mncomp-1)/6)+1
        do 200 k=1,nlines
          nl1=((k-1)*6)+1
          nl2=nl1+5
          if(k.eq.nlines) nl2=mncomp
          read(iufil,1060,iostat=istat,err=1) (micode(l),l=nl1,nl2)
 1060     format(26x,6(i6,2x))
  200   continue

C Read number of component inter-connections.
        read(iufil,1070,iostat=istat,err=1) mncon
 1070   format(44x,i5)
        read(iufil,1075,iostat=istat,err=1)
 1075   format(//)

C Read description of each connection.
        do 210 i=1,mncon
          read(iufil,1080,iostat=istat,err=1) (icndat(i,l),l=1,5)
     &      ,(rcndat(i,ll),ll=1,3)
 1080     format(5(2x,i5),2x,f10.3,2(2x,f8.3))
  210   continue
      endif

C Number of miscellaneous data items associated with this component.
      if(ictype.eq.0) then
        read(iufil,1038,iostat=istat,err=1) nadata,nbdata,ncdata
 1038   format (//,3(20x,i3))
        nmisc=nadata+nbdata
        if(nmisc.lt.0.or.nmisc.gt.maxmsc) goto 1

C Miscellaneous data.
C First read ADATA.
        if(nadata.gt.0) then
          read(iufil,*,iostat=istat,err=1)
          do 140 i=1,nadata
            read(iufil,1040,iostat=istat,err=1)
     &        dtdesc(i),datams(i),range(i,1),range(i,2)
            iupos=iupos+1
            call whichunit(iit,dtdesc(i),uchr)
            if(uchr.ne.'-')write(tustr(iupos),'(a)') uchr
 1040       format(6x,a68,/,15x,g12.5,21x,2(2x,g12.5))
  140     continue
        endif

C Then BDATA.
        if(nbdata.gt.0) then
          read(iufil,*,iostat=istat,err=1)
          do 150 j=1,nbdata
            i=j+nadata
            read(iufil,1040,iostat=istat,err=1)
     &        dtdesc(i),datams(i),range(i,1),range(i,2)
            iupos=iupos+1
            call whichunit(iit,dtdesc(i),uchr)
            if(uchr.ne.'-')write(tustr(iupos),'(a)') uchr
  150     continue
        endif

C Then CDATA (description only) but note that a place holder
C for a default value and range will be included in generic file.
        if(ncdata.gt.0) then
          read(iufil,*,iostat=istat,err=1)
          do 160 j=1,ncdata
            i=j+nmisc
            read(iufil,1065,iostat=istat,err=1) dtdesc(i)
 1065       format(6x,a68)
            iupos=iupos+1
            call whichunit(iit,dtdesc(i),uchr)
            if(uchr.ne.'-')write(tustr(iupos),'(a)') uchr
  160     continue
        endif

C Read number of additional ouput parameters.
        read(iufil,*,iostat=istat,err=1)
        read(iufil,1067,iostat=istat,err=1) napout
        read(iufil,*,iostat=istat,err=1)
        if(napout.gt.0) then
          do 170 j=1,napout
            read(iufil,1085,iostat=istat,err=1) adodsc(j),noutyp(j)
 1085       format(6x,a30,10x,i5)
            iupos=iupos+1
            write(t68,'(a)') adodsc(j)
            call whichunit(iit,t68,uchr)
            if(uchr.ne.'-')write(tustr(iupos),'(a)') uchr
  170     continue
 1067     format(41x,i5)
          read(iufil,*,iostat=istat,err=1)
        endif

C For each external connection, read mass flow component data.
C First find total number of connections.
        nconns=0
        do 2000 inod=1,nnode
          if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
 2000   continue

C Now have sufficient information to write out the generic item.
        write(IFA,'(a)')'# item id, menu, date'
        if(icode.le.9)then
          write(ittag(iit),'(a,i1)') 'cnvcmp',icode
        elseif(icode.gt.9.and.icode.le.99)then
          write(ittag(iit),'(a,i2)') 'cnvcmp',icode
        elseif(icode.gt.99)then
          write(ittag(iit),'(a,i3)') 'cnvcmp',icode
        endif
        write(itmenu(iit),'(a)') ZGTYPE(1:32)
        write(itdate(iit),'(a)') dstmp
        write(IFA,'(7a)')'*ITEM',tab,
     &    ittag(iit)(1:lnblnk(ittag(iit))),tab,
     &    itmenu(iit)(1:lnblnk(itmenu(iit))),tab,
     &    itdate(iit)(1:lnblnk(itdate(iit)))

        write(IFA,'(6a)') '*ID',tab,ZCDESC(1:lnblnk(ZCDESC)),
     &    ' from ',ZCDATE,' insert in original file.'

C Scan the flow component attributes if there are external connections.
        if(nconns.gt.0) then
          nflowus=0
          read(iufil,*)
          do 2010 icon=1,nconns
            read(iufil,'(a60)',iostat=istat,err=1)ltpcmp(icon)
            read(iufil,2038,iostat=istat,err=1)
     &        itpcmp(icon),isdifc(icon),isdcmp(icon),isdcnn(icon)
            isd=isdifc(icon)

C Increment the number of characters in ituntstr by the number of
C flow attributes (isdifc) plus one for itpcmp()
            iupos=iupos+1
            nflowus=nflowus+isdifc(icon)+1

            do 2020 isup=1,isdcmp(icon)
              read(iufil,2040,iostat=istat,err=1)
     &          mfsdsc(icon,isup), supcmp(icon,isup)
 2040         format(a68,1x,g11.5)
              if(isup.le.isdifc(icon))then
                iupos=iupos+1
                call whichunit(iit,mfsdsc(icon,isup),uchr)
                if(uchr.ne.'-')write(tustr(iupos),'(a)') uchr
              endif
 2020       continue
 2010     continue
        else
          nflowus=0
          continue
        endif

C Dump out what we have to tustr
        write(6,*) iupos,tustr
        write(6,*) '  '

C Create a format statement on the fly - with dashes for each of the
C adata, bdata, cdata napout and flow fields.
        if(nadata+nbdata+ncdata+napout+nflowus.gt.0)then
          nbitusc(iit)=nadata+nbdata+ncdata+napout+nflowus
          write(6,*) iupos,nbitusc(iit)
          write(fmt,'(a,i2,5a)') '(',nbitusc(iit),'(',qu,'-',qu,'))'
          write(ituntstr,fmt)
          write(6,*) 'initial ituntstr is ',ituntstr
          if(iupos.eq.nbitusc(iit))then
            do 1009 ij=1,iupos
              write(ituntstr(ij:ij),'(a)') tustr(ij)
 1009       continue
            write(6,*) 'revised ituntstr is ',ituntstr
          else
            write(6,*) 'guess on units and nbitusc differ in comp ',
     &        iit,icon,iupos,nbitusc(iit)
          endif
          write(itcnfstr,fmt)
        else
          ituntstr='------------'
          itcnfstr='------------'
        endif
        itsrcstr='a'
        write(IFA,'(8a)') '*USC',tab,ituntstr(1:lnblnk(ituntstr)),
     &    tab,itsrcstr,tab,itcnfstr(1:lnblnk(itcnfstr)),
     &    ' # units srcs uncert'

C Write first *DAT line with the number of various data fields.
        write(outs,'(10i4)',IOSTAT=IOS,ERR=1) 
     &    ictype,icode,nnode,nmatx,nadata,nbdata,ncdata,nmisc,
     &    napout,nconns
        call SDELIM(outs,outsd,'C',IW)
        lo=lnblnk(outsd)
        write(IFA,'(7a)')'*DAT',tab,ctype(1:lnblnk(ctype)),tab,
     &    outsd(1:lo),'  # ictype icode nnode nmatx nadata nbdata ',
     &    'ncdata nmisc napout nconns'

C Write 2nd *DAT line with the non-zero matrix positions.
        write(outs,'(24i3)') (ncpos(j),j=1,nmatx)
        call SDELIM(outs,outsd,'C',IW)
        lo=lnblnk(outsd)
        write(IFA,'(5a)')'*DAT',tab,'*Non_zero',tab,outsd(1:lo)

C Write 3rd *DAT line with external connections.
        write(outs,'(24i3)') (ndcon(j),j=1,nmatx)
        call SDELIM(outs,outsd,'C',IW)
        lo=lnblnk(outsd)
        write(IFA,'(5a)')'*DAT',tab,'*Ext_cnn',tab,outsd(1:lo)

C Write 4th *DAT line with state variable definitions.
        write(outs,'(24i3)') (isv(j),j=1,nmatx)
        call SDELIM(outs,outsd,'C',IW)
        lo=lnblnk(outsd)
        write(IFA,'(5a)')'*DAT',tab,'*Isv',tab,outsd(1:lo)

C Write adata in clusters (dtdesc/datams/range/range) * 4
        if(nmisc.eq.0)then
          write(IFA,'(3a)')'*DAT',tab,'*No_ABdata'
        else
          icount=0
          icol=1
          ihold=' '
          ix=1
          ixl=0
          outs=' '
          start=.true.
          do 43 i=1,nmisc

C Write group of fields to a holding string ihold.
            call SIGFIG(datams(i),NSIGFIG,RNO,SIGSTR,LSTR)
            call SIGFIG(range(i,1),NSIGFIG,RNO,SIGSTRA,LSTRA)
            call SIGFIG(range(i,2),NSIGFIG,RNO,SIGSTRB,LSTRB)
            ldt=MIN0(lnblnk(dtdesc(i)),48)
            write(ihold,'(8a)') dtdesc(i)(1:ldt),tab,
     &        sigstr(1:lstr),tab,sigstra(1:lstra),tab,sigstrb(1:lstrb),
     &        tab
            lna=lnblnk(ihold)
            if(ix+lna.gt.248)then

C If writing the current ihold will take the text width past 248 char
C dump out outs first and reset position of ix.
              icol=0
              icount=icount+1
              if(start)then
                write(IFA,'(5a)') '*DAT',tab,'*ABdat',tab,
     &            outs(1:lnblnk(outs))
                start=.false.
              else
                write(IFA,'(3a)') '*DAT',tab,outs(1:lnblnk(outs))
              endif
              outs=' '
              ix=1
              ixl=ix+lna
              write(outs(ix:ixl),'(a)')ihold(1:lna)
              ix=ix+lna
              icol=icol+1

C If we have reached nmisc then write outs.
              if(i.eq.nmisc)then
                icol=0
                icount=icount+1
                if(start)then
                  write(IFA,'(5a)') '*DAT',tab,'*ABdat',tab,
     &              outs(1:lnblnk(outs))
                  start=.false.
                else
                  write(IFA,'(3a)') '*DAT',tab,outs(1:lnblnk(outs))
                endif
                outs=' '
                ix=1
              endif
            else

C We have room to write another cluster.
              ixl=ix+lna
              write(outs(ix:ixl),'(a)')ihold(1:lna)
              ix=ix+lna
              icol=icol+1

C If we have reached the 4th colum or nmisc then write outs.
              if(icol.gt.4.or.i.eq.nmisc)then
                icol=0
                icount=icount+1
                if(start)then
                  write(IFA,'(5a)') '*DAT',tab,'*ABdat',tab,
     &              outs(1:lnblnk(outs))
                  start=.false.
                else
                  write(IFA,'(3a)') '*DAT',tab,outs(1:lnblnk(outs))
                endif
                outs=' '
                ix=1
              endif
            endif
  43      continue
        endif

C Write C data.
        if(ncdata.eq.0)then
          write(IFA,'(3a)')'*DAT',tab,'*No_Cdata'
        else
          icount=0
          icol=1
          ihold=' '
          ix=1
          ixl=0
          outs=' '
          start=.true.
          do 44 j=1,ncdata

C Write group of fields to a holding string ihold. Reset i to match
C the logic in the read statement because the cdata description is
C appended to the end of the a and b description array. Note that
C place holder for a default value and range is included.
            i=j+nmisc
            ldt=MIN0(lnblnk(dtdesc(i)),48)
            write(ihold,'(8a)') dtdesc(i)(1:ldt),tab,'0.00',tab,
     &        '0.00',tab,'99999.',tab
            lna=lnblnk(ihold)
            ixl=ix+lna
            write(outs(ix:ixl),'(a)')ihold(1:lna)
            ix=ix+lna
            icol=icol+1

C If we have reached the 4th colum or ncdata then write outs.
            if(icol.gt.4.or.j.eq.ncdata)then
              icol=0
              icount=icount+1
              if(start)then
                write(IFA,'(5a)') '*DAT',tab,'*Cdat',tab,
     &            outs(1:lnblnk(outs))
                start=.false.
              else
                write(IFA,'(3a)') '*DAT',tab,outs(1:lnblnk(outs))
              endif
              outs=' '
              ix=1
            endif
  44      continue
        endif

C Write additional output in clusters (dtdesc/datams/range/range) * 4
        if(napout.eq.0)then
          write(IFA,'(3a)')'*REP',tab,'*No_Addoutp'
        else
          icount=0
          icol=1
          ihold=' '
          ix=1
          ixl=0
          outs=' '
          start=.true.
          do 45 i=1,napout

C Write group of fields to a holding string ihold.
            ldt=MIN0(lnblnk(adodsc(i)),48)
            write(ihold,'(2a,i2,a)') adodsc(i)(1:ldt),tab,noutyp(i),tab
            lna=lnblnk(ihold)
            ixl=ix+lna
            write(outs(ix:ixl),'(a)')ihold(1:lna)
            ix=ix+lna
            icol=icol+1

C If we have reached the 4th colum or napout then write outs.
            if(icol.gt.4.or.i.eq.napout)then
              icol=0
              icount=icount+1
              if(start)then
                write(IFA,'(5a)') '*REP',tab,'*Addoutp',tab,
     &            outs(1:lnblnk(outs))
                start=.false.
              else
                write(IFA,'(3a)') '*REP',tab,outs(1:lnblnk(outs))
              endif
              outs=' '
              ix=1
            endif
  45      continue
        endif

C Write flow fields.
        if(nconns.gt.0) then
          do 3010 icon=1,nconns

C Write flow fields. 1st line holds itpcmp & ltpcmp & isdfic etc.
            isd=isdifc(icon)
            write(outs,'(i3,a,i3,a,i3,a,i3,2a)',IOSTAT=IOS,ERR=1) 
     &        itpcmp(icon),tab,isdifc(icon),tab,isdcmp(icon),tab,
     &        isdcnn(icon),tab,ltpcmp(icon)(1:lnblnk(ltpcmp(icon)))           
            lo=lnblnk(outs)
            write(IFA,'(5a)')'*REP',tab,'*Flowc',tab,outs(1:lo)

C Subsequent lines are the flow attributes.
            if(isd.eq.0)then
              write(IFA,'(3a)')'*REP',tab,'*No_flowa'
            else

C Write the non-control flow attributes in clusters of 4.
              icount=0
              icol=1
              ihold=' '
              ix=1
              ixl=0
              outs=' '
              start=.true.
              do 3022 isu=1,isd
                call SIGFIG(supcmp(icon,isu),NSIGFIG,RNO,SIGSTR,LSTR)
                ldt=MIN0(lnblnk(mfsdsc(icon,isu)),48)
                write(ihold,'(4a)') mfsdsc(icon,isu)(1:ldt),tab,
     &            sigstr(1:lstr),tab
                lna=lnblnk(ihold)
                ixl=ix+lna
                write(outs(ix:ixl),'(a)')ihold(1:lna)
                ix=ix+lna
                icol=icol+1

C If we have reached the 4th colum or isdifc(icon) then write outs.
                if(icol.gt.4.or.isu.eq.isd)then
                  icol=0
                  icount=icount+1
                  if(start)then
                    write(IFA,'(5a)') '*REP',tab,'*Flowa',tab,
     &                outs(1:lnblnk(outs))
                    start=.false.
                  else
                    write(IFA,'(3a)') '*REP',tab,outs(1:lnblnk(outs))
                  endif
                  outs=' '
                  ix=1
                endif
 3022         continue
            endif
 3010     continue
 2038     format ((7x,i4,20x,i4,20x,i4,18x,i4))
        else
          write(IFA,'(3a)')'*REP',tab,'*No_flowc'
        endif
        write(IFA,'(a)')'*ENDITEM'
      endif

C Case for TRNSYS type components. << still to be done >>
C << similar to the single component logic, but the data will be
C << repeated for as many subcomponents as exist within the
C << TRNSYS type. 
C << Decision required on how to represent this because what
C << we really have is the *DAT and *REP followed by another
C << set of *DAT and *REP. 
      if(ictype.eq.2) then
        read(iufil,'(25x,i5)',iostat=istat,err=1) ntypes
        read(iufil,'(26x,50(i5,1x))',iostat=istat,err=1) 
     &    (ittype(ityp),ityp=1,ntypes)
        do 3000 ityp=1, ntypes
          read(iufil,1038,iostat=istat,err=1) 
     &      ntadat(ityp),ntbdat(ityp),ntcdat(ityp)
          ntmisc(ityp)=ntadat(ityp)+ntbdat(ityp)
          if(ntmisc(ityp).lt.0.or.ntmisc(ityp).gt.maxmsc) goto 1

C Miscellaneous data.
C First read ADATA.
          if(ntadat(ityp).gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 3140 i=1,ntadat(ityp)
              read(iufil,1040,iostat=istat,err=1)
     &          tdtdsc(ityp,i),tdatms(ityp,i),trange(ityp,i,1),
     &          trange(ityp,i,2)
 3140       continue
          endif

C Then BDATA.
          if(ntbdat(ityp).gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 3150 j=1,ntbdat(ityp)
              i=j+ntadat(ityp)
              read(iufil,1040,iostat=istat,err=1)
     &          tdtdsc(ityp,i),tdatms(ityp,i),trange(ityp,i,1),
     &          trange(ityp,i,2)
 3150       continue
          endif

C Then CDATA (description only).
          if(ntcdat(ityp).gt.0) then
            read(iufil,*,iostat=istat,err=1)
            do 3160 j=1,ntcdat(ityp)
              i=j+ntmisc(ityp)
              read(iufil,1065,iostat=istat,err=1)tdtdsc(ityp,i)
 3160       continue
          endif

C Read number of additional ouput parameters.
          read(iufil,*,iostat=istat,err=1)
          read(iufil,1067,iostat=istat,err=1) ntapot(ityp)
          read(iufil,*,iostat=istat,err=1)
          if(ntapot(ityp).gt.0) then
            do 3170 j=1,ntapot(ityp)
              read(iufil,1085,iostat=istat,err=1)
     &          tadods(ityp,j),ntaotp(ityp,j)
 3170       continue
            read(iufil,*,iostat=istat,err=1)
          endif

C For each external connection, read mass flow component data.
C First find total number of connections.
          nconns=0
          do 4000 inod=1,nnode
 4000       if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)

          if(nconns.gt.0) then
            read(iufil,*)
            do 4010 icon=1, nconns
              read(iufil,'(a60)',iostat=istat,err=1)tltpcm(ityp,icon)
              read(iufil,2038,iostat=istat,err=1)
     &          ittpcm(ityp,icon),itsdif(ityp,icon),
     &          itsdcm(ityp,icon),itsdcn(ityp,icon)
              do 4020 isup=1, itsdcm(ityp,icon)
                read(iufil,2040,iostat=istat,err=1)
     &            tmfsds(ityp,icon,isup),tsupcm(ityp,icon,isup)
 4020         continue
 4010       continue
          endif
 3000   continue
      endif
         
C Set index for this entry.
      INDXPC=NPC+1
  100 RETURN

C EOF encountered.
  900 call usrmsg(' ',' PCRDFgdb: End-Of-File encountered','-')
      INDXPC=0
      GOTO 100

C Error handling.
  1   CALL usrmsg(' Pdb:pcrdfgdb error reading db',' ','W')
      goto 100
      END

C ********** whichunit ********
C whichunit scans a message () to see if it matches a unit string
C and returns uchr (character matching the unit).
C itm is the current item index.
      subroutine whichunit(itm,string,uchr) 
 
C Units commons.
      common/gendbu/unitchr(24),unitstr(24),unitnote(24)
      common/gendb1/idbwidth(13),lastr(13),nbdbdoc(13),nbunit(13),
     &              nbsrc(13),nbcat(13),nbitem(13),idbver(13)     

      character unitchr*4,unitstr*12,unitnote*72
      character string*68,uchr*1

C For each known unit, search within the string from the
C first position until lu characters before the end.
      idbh=12
      uchr='-'
      lss=lnblnk(string)
      do 42 ij=1,nbunit(idbh)
        lu=lnblnk(unitstr(ij))
        loop=lss-lu+1
        ix=1
        ixl=ix+(lu-1)
        do 43 is=1,loop
          if(string(ix:ixl).eq.unitstr(ij)(1:lu))then
            write(uchr,'(a)') unitchr(ij)(1:1)
            write(6,*) 'match of ',unitstr(ij)(1:lu),' in ',
     &        string(1:lss),' with char of ',uchr
            return
          endif
          ix=ix+1
          ixl=ix+(lu-1)
 43    continue
  42  continue
      return
      end
