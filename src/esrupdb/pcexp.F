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

C PCWRT of ESRUpdb
        SUBROUTINE PCEXP (ICHAN)
#include "plantdb.h"

C Formatted output of a complete plant component entry.  The channel
C used for output is passed as an argument.

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL

      common/PCDATA/ZGTYPE, ZCDATE, ZCDESC
      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)

      common/pcdatd/ictype,mncomp,mncon,micode(maxcmp),icndat(maxcon,5),
     &              rcndat(maxcon,3)

C Mass flow components data
      common/mfcomp/itpcmp(maxcon), isdcmp(maxcon), isdcnn(maxcon),
     &              supcmp(maxcon,maxflw), mfsdsc(maxcon,maxflw),
     &              ltpcmp(maxcon), isdifc(maxcon)

C     common/translate_id/xlate_id_tag

C TRNSYS types data
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
      INTEGER xlate_id_tag,xlate_id_tag2
      character*68 dtdesc
      character*68  mfsdsc, tdtdsc, tmfsds
      character*30 adodsc, tadods
      character*60 ltpcmp, tltpcm
      character text*20, buffer*248, flstr*10
      character id_tag_str*16

        if(ichan.eq.(ifil+1)) mmod=-1

C Note xlate_id_tag has not initial value
        xlate_id_tag=xlate_id_tag+1
        xlate_id_tag2=xlate_id_tag+1000

C Output header
        call edisp(ichan,'#')
        buffer='  '
        call edisp(ichan,
     &  '# -------------------- ITEM ------------------')
        call edisp(ichan,'#')
        buffer='  '
C Output description data
        if(ictype.eq.0) text='Single component    '
        if(ictype.eq.1) text='Meta component      '
        if(ictype.eq.2) text='TRNSYS component    '

        write(id_tag_str,'(A12,I4)')'unique_tag__',xlate_id_tag2
        WRITE (buffer,3)
     &id_tag_str,ZCDESC(1:lnblnk(ZCDESC))
    3   FORMAT ('*Item,',A16,',',A,' # id and description')
        call edisp(ichan,buffer)
        buffer='  '

C Now write out all component data
        write(ichan,'(a)')'*Attribute   # attributes and data'
        WRITE (buffer,5) ZCDATE
    5   FORMAT 
     &('*thermal,text,-,-,',A16,',static,creation date')
        call edisp(ichan,buffer)
        buffer='  '
        write(buffer,100) text(1:lnblnk(text))
  100   FORMAT ('*thermal,text,-,-,',A,',static,meta type')
        call edisp(ichan,buffer)
        buffer='  '

        write(buffer,103) icode
  103   FORMAT ('*thermal,intg,-,-,',I4,',0,999,static,component code')
        call edisp(ichan,buffer)
        buffer='  '

C Output nodal data for single component type.
      if(ictype.eq.0) then
        WRITE (buffer,7) NNODE
    7   FORMAT ('*thermal,intg,-,-,',I3,',0,999,static,no. of nodes')
        call edisp(ichan,buffer)
        buffer='  '
        WRITE (buffer,77) NMATX
   77   FORMAT 
     &('*thermal,intg,-,-,',I3,
     &',0,999,static,no. non zero matrix elements')
        call edisp(ichan,buffer)
        buffer='  '

C Write position of non-zero matrix coefficients.
         do 19 k=1,nmatx
            WRITE (buffer,8) NCPOS(k)
    8       FORMAT 
     &('*thermal,intg,-,-,',I3,',0,999,static,matrix position')
           call edisp(ichan,buffer)
         buffer='  '
   19    continue


C Write external connections for each node.
         do 13 k=1,nnode
           WRITE (buffer,9) NDCON(k),K
    9      FORMAT
     &('*thermal,intg,-,-,',I3,
     &',0,999,static,no. connections to node ',I3)
            call edisp(ichan,buffer)
            buffer='  '
   13    continue

C Write state variable index for each node.
         do 15 k=1,nnode
            WRITE (buffer,14) ISV(k),K
   14       FORMAT 
     &('*thermal,intg,-,-,',I3,',0,999,static,ISV of node ',I3)
            call edisp(ichan,buffer)
            buffer='  '
   15    continue

C Write miscellaneous data.
        write(buffer,'(a,I3,a)')
     &'*thermal,intg,-,-,',nadata,',0,999,static,no. adata '
        call edisp(ichan,buffer)
        buffer='  '
        write(buffer,'(a,I3,a)')
     &'*thermal,intg,-,-,',nbdata,',0,999,static,no. bdata '
        call edisp(ichan,buffer)
        buffer='  '
        write(buffer,'(a,I3,a)')
     &'*thermal,intg,-,-,',ncdata,',0,999,static,no. cdata '
        call edisp(ichan,buffer)
        buffer='  '

C Write ADATA.
        if(nadata.gt.0) then
c           write(buffer,'(a)') ' ADATA variables:'
c           call edisp(ichan,buffer)
            buffer='  '
           do 160 i=1,nadata
              write(buffer,162)
     &datams(i),range(i,1),range(i,2),dtdesc(i)(1:lnblnk(dtdesc(i)))
  162         FORMAT
     &('*thermal,real,-,-,',g12.5,',',g12.5,',',g12.5,',user,',A)
              call edisp(ichan,buffer)
              buffer='  '
  160      continue
        endif

C Write BDATA.
        if(nbdata.gt.0) then
c           write(buffer,'(a)') ' BDATA variables:'
c           call edisp(ichan,buffer)
            buffer='  '
           do 170 i=1,nbdata
              j=i+nadata
              write(buffer,172)
     &datams(j),range(j,1),range(j,2),dtdesc(j)(1:lnblnk(dtdesc(j)))
  172         FORMAT
     &('*thermal,real,-,-,',g12.5,',',g12.5,',',g12.5,',user,',A)
              call edisp(ichan,buffer)
              buffer='  '
  170      continue
        endif

C Write CDATA.
        if(ncdata.gt.0) then
c           write(buffer,'(a)') ' CDATA variables:'
c           call edisp(ichan,buffer)
            buffer='  '
           do 180 i=1,ncdata
              j=i+nadata+nbdata
              write(buffer,182)dtdesc(j)(1:lnblnk(dtdesc(j)))
  182         FORMAT
     &('*control,real,-,-,0.0,0.0,999.0,user,',A)
              call edisp(ichan,buffer)
              buffer='  '
  180      continue
        endif

C Write additional output.
c        call edisp(ichan,' ')
c        WRITE (buffer,17) napout
c   17   FORMAT
c     &('*output,real,-,-,0.0,0.0,999.0,static,',I3,',# no. aout')
c        call edisp(ichan,buffer)
         buffer='  '
        
C Write additional output description and type.
        if(napout.gt.0) then
c           write(buffer,'(a)') ' Additional output variables:'
c           call edisp(ichan,buffer)
            buffer='  '
           do 190 i=1,napout
             write(buffer,192) noutyp(i),adodsc(i)(1:30)
  192        FORMAT('*output,real,-,-,',I3,',0.0,0.0,999.0,static,',A30)
           call edisp(ichan,buffer)
           buffer='  '
  190      continue
        endif

C For each external connection, write mass flow component data.
C First find total number of connections.
      nconns=0
      do 200 inod=1,nnode
 200    if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
      if(nconns.gt.0) then
         do 210 icon=1, nconns    
           write(buffer,'(A60,1X,I2)')
     &'*flow,intg,-,-, 1,0,999, hydraulic data for flow connection:',
     &icon
           call edisp(ichan,buffer)
           buffer='  '
           write(flstr,'(A6)')'*flow'
           write(buffer,211)flstr,ltpcmp(icon)(1:32)
  211      FORMAT(A,',text,-,-,',A,',static,comp. description')
           call edisp(ichan,buffer)
           buffer='  '
           write(buffer,212)flstr,itpcmp(icon)
  212      FORMAT(A,',intg,-,-,',I3,',0,999,static,comp. type')
           call edisp(ichan,buffer)
           buffer='  '
           write(buffer,213)flstr,isdifc(icon)
  213      FORMAT(A,',intg,-,-,',I3,',0,999,static,init data')
           call edisp(ichan,buffer)
           buffer='  '
           write(buffer,'(a,a)') flstr,
     &',intg,-,-,  0,0,999,static,total data'
           call edisp(ichan,buffer)
           buffer='  '
           write(buffer,'(a,a)') flstr,
     &',intg,-,-,0,0,999,static,flow connections'
            do 220 isup=1, isdifc(icon)
               write(buffer,240)
     &flstr,supcmp(icon,isup),
     &mfsdsc(icon,isup)(1:lnblnk(mfsdsc(icon,isup)))
 240           format(A,',real,-,-,',g12.5,',0.0,999.0,user,',A)
               call edisp(ichan,buffer)
               buffer='  '
 220        continue
 210     continue
      endif
      write(ichan,'(A)')'*End_attribute'
      write(ichan,'(A)')'*Text'
      write(ichan,'(A)')'Descriptive text for component goes here'
      write(ichan,'(A)')'*End_Text'

C Write out a default component icon
      write(ichan,'(A)')'*Graphics'
      write(ichan,'(A)')'*Vertex,0.25,0.15'
      write(ichan,'(A)')'*Vertex,0.75,0.15'
      write(ichan,'(A)')'*Vertex,0.75,0.85'
      write(ichan,'(A)')'*Vertex,0.25,0.85'
      write(ichan,'(A)')'*Vertex,0.4,0.25'
      write(ichan,'(A)')'*Vertex,0.6,0.25'
      write(ichan,'(A)')'*Vertex,0.5,0.45'
      write(ichan,'(A)')'*Vertex,0.5 -0.1'
      write(ichan,'(A)')'*Vertex,0.5 1.1'
      write(ichan,'(A)')'*Edge,1,2,black,solid'
      write(ichan,'(A)')'*Edge,2,3,black,solid'
      write(ichan,'(A)')'*Edge,3,4,black,solid'
      write(ichan,'(A)')'*Edge,4,1,black,solid'
      write(ichan,'(A)')'*Edge,5,6,red,solid'
      write(ichan,'(A)')'*Edge,6,7,red,solid'
      write(ichan,'(A)')'*Edge,7,5,red,solid'
      write(ichan,'(A)')'*Attach,8,2'
      write(ichan,'(A)')'*Attach,9,2'
      write(ichan,'(A)')'*Label,16,black,T'
      write(ichan,'(A)')'*End_graphics'
      write(ichan,'(A)')'*End_item'
C Output composition for meta component type.
      elseif(ictype.eq.1) then


C Case for TRNSYS type components.
      elseif(ictype.eq.2) then
      endif


C Pause if terminal, then return
      RETURN
      END
