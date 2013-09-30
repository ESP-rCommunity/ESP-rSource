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
        SUBROUTINE PCWRT (ICHAN)
#include "plantdb.h"
#include "pdb_data.h"

C Formatted output of a complete plant component entry.  The channel
C used for output is passed as an argument.

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/FILEP/IFIL

      character text*20, buffer*120

C Reset the display mode to act as a script.
        if(ichan.eq.(ifil+1)) mmod=-1

C Output header. If not in graphic mode include some ------- lines.
        if(mmod.ne.8.and.mmod.ne.9) then
           call edisp(ichan,' ')
           write(buffer,1111)
 1111      format(69('-'))
           call edisp(ichan,buffer)
        endif
        WRITE (buffer,1) INDXPC
    1   FORMAT (' ',' PLANT COMPONENT',43X,'No : ',I6)
        call edisp(ichan,buffer)
        call edisp(ichan,' ')

C Output description data
        if(ictype.eq.0) text='Single component    '
        if(ictype.eq.1) text='Meta component      '
        if(ictype.eq.2) text='TRNSYS component    '
        WRITE (buffer,2)
    2   FORMAT (' Component Description ')
        call edisp(ichan,buffer)
        WRITE (buffer,3) ZGTYPE
    3   FORMAT ('    Generic type     : ',A40)
        call edisp(ichan,buffer)
        WRITE (buffer,4) ZCDESC
    4   FORMAT ('    Description      : ',A80)
        call edisp(ichan,buffer)
        WRITE (buffer,5) ZCDATE
    5   FORMAT ('    Insertion date   : ',A16)
        call edisp(ichan,buffer)
        write(buffer,100) text,icode
  100   format('    Type: ',a20,15X,
     &          ' Component code : ',I6)
        call edisp(ichan,buffer)

C Output nodal data for single component type.
      if(ictype.eq.0) then
        call edisp(ichan,' ')
        call edisp(ichan,' Nodal Scheme Description')
        WRITE (buffer,7) NNODE, NMATX
    7   FORMAT ('    Number of nodes  : ',I3,5X,
     &          ' No of nonzero matrix elements : ',I6)
        call edisp(ichan,buffer)

C Write position of non-zero matrix coefficients.
         nlines=((nmatx-1)/10)+1
         do 19 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nmatx
            if(jj.eq.1) WRITE (buffer,8) (NCPOS(J),J=jj,jjj)
    8       FORMAT ('    Matrix positions : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,10) (NCPOS(J),J=jj,jjj)
   10       FORMAT (21X,': ',10(I3,', '))
            call edisp(ichan,buffer)
   19    continue

C Write external connections for each node.
         nlines=((nnode-1)/10)+1
         do 13 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,9) (NDCON(J),J=jj,jjj)
    9       FORMAT ('    Node connections : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,12) (NDCON(J),J=jj,jjj)
   12       FORMAT (21X,': ',10(I3,', '))
            call edisp(ichan,buffer)
   13    continue

C Write state variable index for each node.
         nlines=((nnode-1)/10)+1
         do 15 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,14) (ISV(J),J=jj,jjj)
   14       FORMAT ('    Variable type    : ',10(I3,', '))
            if(jj.ne.1) WRITE (buffer,16) (ISV(J),J=jj,jjj)
   16       FORMAT (21X,': ',10(I3,', '))
            call edisp(ichan,buffer)
   15    continue

C Write miscellaneous data.
        call edisp(ichan,' ')
        call edisp(ichan,' Miscellaneous Data')
        WRITE (buffer,11) nadata, nbdata, ncdata
   11   FORMAT ('    Adata items   : ',I3,
     &          '    Bdata items   : ',I3,
     &          '    Cdata items   : ',I3)
           call edisp(ichan,buffer)

C Write ADATA.
        if(nadata.gt.0) then
           write(buffer,'(a)') ' ADATA variables:'
           call edisp(ichan,buffer)
           do 160 i=1,nadata
              write(buffer,162) i,dtdesc(i)
  162         format(i5,1x,a68)
              call edisp(ichan,buffer)
              write(buffer,163) datams(i),range(i,1),range(i,2)
  163         format(6x,'Value =  ',g12.5,4x,
     &               'Range (Min Max): ',2(2x,g12.5))
              call edisp(ichan,buffer)
  160      continue
        endif

C Write BDATA.
        if(nbdata.gt.0) then
           write(buffer,'(a)') ' BDATA variables:'
           call edisp(ichan,buffer)
           do 170 i=1,nbdata
              j=i+nadata
              write(buffer,172) i,dtdesc(j)
  172         format(i5,1x,a68)
              call edisp(ichan,buffer)
              write(buffer,173) datams(j),range(j,1),range(j,2)
  173         format(6x,'Value =  ',g12.5,4x,
     &               'Range (Min Max): ',2(2x,g12.5))
              call edisp(ichan,buffer)
  170      continue
        endif

C Write CDATA.
        if(ncdata.gt.0) then
           write(buffer,'(a)') ' CDATA variables:'
           call edisp(ichan,buffer)
           do 180 i=1,ncdata
              j=i+nadata+nbdata
              write(buffer,182) i,dtdesc(j)
  182         format(i5,1x,a68)
           call edisp(ichan,buffer)
  180      continue
        endif

C Write additional output.
        call edisp(ichan,' ')
        WRITE (buffer,17) napout
   17   FORMAT (' Number of additional output parameters: ',I5)
        call edisp(ichan,buffer)

C Write additional output description and type.
        if(napout.gt.0) then
           write(buffer,'(a)') ' Additional output variables:'
           call edisp(ichan,buffer)
           do 190 i=1,napout
              write(buffer,192) i,adodsc(i),noutyp(i)
  192         format(i5,1x,a30,' Type :',i5)
           call edisp(ichan,buffer)
  190      continue
        endif

C For each external connection, write mass flow component data.
C First find total number of connections.
      nconns=0
      do 200 inod=1,nnode
 200    if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
      if(nconns.gt.0) then

C Write mass flow components data.
         call edisp(ichan,' ')
         call edisp(ichan,' Mass flow components data')
  211    FORMAT ('Type :',I4,
     &          '    Init suppl dat :',I4,
     &          '    Tot suppl dat :',I4,
     &          '    No. of conns :',I4)

         do 210 icon=1, nconns
            write(buffer,'(a60)') ltpcmp(icon)
            call edisp(ichan,buffer)
            write(buffer,211)
     &           itpcmp(icon),isdifc(icon),isdcmp(icon),isdcnn(icon)
            call edisp(ichan,buffer)
            do 220 isup=1, isdcmp(icon)
               write(buffer,240)
     &            mfsdsc(icon,isup), supcmp(icon,isup)
               call edisp(ichan,buffer)
 240           format(a68,1x,g11.5)
 220        continue
 210     continue
      endif

C Output composition for meta component type.
      elseif(ictype.eq.1) then
        call edisp(ichan,' ')
        call edisp(ichan,' Meta component description')
        write(buffer,110) mncomp
  110   format('    Number of components: ',i3)
        call edisp(ichan,buffer)

C Write code number of each component.
         nlines=((mncomp-1)/10)+1
         do 119 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=mncomp
            if(jj.eq.1) WRITE (buffer,118) (micode(J),J=jj,jjj)
  118       FORMAT ('    Components code : ',6(I3,', '))
            if(jj.ne.1) WRITE (buffer,120) (micode(J),J=jj,jjj)
  120       FORMAT (21X,': ',6(I3,', '))
            call edisp(ichan,buffer)
  119    continue

C Write component inter-connections.
        write(buffer,130) mncon
  130   format('    Number of component inter-connections : ',i5)
        call edisp(ichan,buffer)
        call edisp(ichan,' ')
        call edisp(ichan,' Description of component inter-connections')
        call edisp(ichan,' ')
        write(buffer,140)
  140   format(6x,'C1',5x,'N1',5x,'CT',5x,'C2',5x,'N2',7x,'MDR',
     &         6x,'MISC1',5x,'MISC2')
        call edisp(ichan,buffer)
        do 142 i=1,mncon
           write(buffer,150) (icndat(i,l),l=1,5),(rcndat(i,ll),ll=1,3)
  150      format(5(2x,i5),2x,f10.3,2(2x,f8.3))
           call edisp(ichan,buffer)
  142   continue

C Case for TRNSYS type components.
      elseif(ictype.eq.2) then
        call edisp(ichan,' ')
        call edisp(ichan,' Nodal Scheme Description')
        WRITE (buffer,7) NNODE, NMATX
        call edisp(ichan,buffer)

C Write position of non-zero matrix coefficients.
         nlines=((nmatx-1)/10)+1
         do 719 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nmatx
            if(jj.eq.1) WRITE (buffer,8) (NCPOS(J),J=jj,jjj)
            if(jj.ne.1) WRITE (buffer,10) (NCPOS(J),J=jj,jjj)
            call edisp(ichan,buffer)
  719    continue

C Write external connections for each node.
         nlines=((nnode-1)/10)+1
         do 713 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,9) (NDCON(J),J=jj,jjj)
            if(jj.ne.1) WRITE (buffer,12) (NDCON(J),J=jj,jjj)
            call edisp(ichan,buffer)
  713    continue

C Write state variable index for each node.
         nlines=((nnode-1)/10)+1
         do 715 k=1,nlines
            jj=((k-1)*10)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=nnode
            if(jj.eq.1) WRITE (buffer,14) (ISV(J),J=jj,jjj)
            if(jj.ne.1) WRITE (buffer,16) (ISV(J),J=jj,jjj)
            call edisp(ichan,buffer)
  715    continue

C Write number of TRNSYS types entered so far
C and type number for each.
        write(buffer,'(a,i5)') '    No. of TRNSYS types:',ntypes
        call edisp(ichan,buffer)

C Write type number for each TRNSYS model.
         nlines=((ntypes-1)/6)+1
         do 755 k=1,nlines
            jj=((k-1)*6)+1
            jjj=jj+9
            if(k.eq.nlines) jjj=ntypes
            if(jj.eq.1) WRITE (buffer,1114) (ittype(J),J=jj,jjj)
            if(jj.ne.1) WRITE (buffer,1116) (ittype(J),J=jj,jjj)
            call edisp(ichan,buffer)
 1114       FORMAT ('    List of TRNSYS types:',6(I5,', '))
 1116       FORMAT (26x,6(I5,','))
  755    continue

C Write miscellaneous data.
       do 2000 ityp=1, ntypes
        call edisp(ichan,' ')
        write(buffer,'(a,i5)')
     &    ' Miscellaneous Data for TRNSYS model type :', ittype(ityp)
        call edisp(ichan,buffer)
        WRITE (buffer,11) ntadat(ityp),ntbdat(ityp),ntcdat(ityp)
           call edisp(ichan,buffer)

C Write ADATA.
        if(ntadat(ityp).gt.0) then
           write(buffer,'(a)') ' ADATA variables:'
           call edisp(ichan,buffer)
           do 760 i=1,ntadat(ityp)
              write(buffer,162) i, tdtdsc(ityp,i)
              call edisp(ichan,buffer)
              write(buffer,163) tdatms(ityp,i),
     &                      trange(ityp,i,1),trange(ityp,i,2)
              call edisp(ichan,buffer)
  760      continue
        endif

C Write BDATA.
        if(ntbdat(ityp).gt.0) then
           write(buffer,'(a)') ' BDATA variables:'
           call edisp(ichan,buffer)
           do 770 i=1,ntbdat(ityp)
              j=i+ntadat(ityp)
              write(buffer,172) i,tdtdsc(ityp,j)
              call edisp(ichan,buffer)
              write(buffer,163) tdatms(ityp,j),
     &                      trange(ityp,j,1),trange(ityp,j,2)
              call edisp(ichan,buffer)
  770      continue
        endif

C Write CDATA.
        if(ntcdat(ityp).gt.0) then
           write(buffer,'(a)') ' CDATA variables:'
           call edisp(ichan,buffer)
           do 780 i=1,ntcdat(ityp)
              j=i+ntmisc(ityp)
              write(buffer,182) i,tdtdsc(ityp,j)
           call edisp(ichan,buffer)
  780      continue
        endif

C Write additional aoutput.
        call edisp(ichan,' ')
        WRITE (buffer,17) ntapot(ityp)
           call edisp(ichan,buffer)

C Write additional output description and type.
        if(ntapot(ityp).gt.0) then
           write(buffer,'(a)') ' Additional output variables:'
           call edisp(ichan,buffer)
           do 790 i=1, ntapot(ityp)
              write(buffer,192) i,tadods(ityp,i),ntaotp(ityp,i)
           call edisp(ichan,buffer)
  790      continue
        endif

C For each external connection, write mass flow component data.
C First find total number of connections.
      nconns=0
      do 800 inod=1,nnode
 800    if(ndcon(inod).gt.0) nconns=nconns+ndcon(inod)
      if(nconns.gt.0) then

C Write mass flow components data.
         call edisp(ichan,' ')
         call edisp(ichan,' Mass flow components data')
         do 810 icon=1, nconns
            write(buffer,'(a60)') tltpcm(ityp,icon)
            call edisp(ichan,buffer)
            write(buffer,211)
     &              ittpcm(ityp,icon),itsdif(ityp,icon),
     &              itsdcm(ityp,icon),itsdcn(ityp,icon)
            call edisp(ichan,buffer)
            do 820 isup=1, itsdcm(ityp,icon)
               write(buffer,240)
     &           tmfsds(ityp,icon,isup),tsupcm(ityp,icon,isup)
               call edisp(ichan,buffer)
 820        continue
 810     continue
        endif
 2000 continue
      endif

C Pause if terminal, then return
      RETURN
      END
