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

C ***************** SPMINIT
C This subroutine reads information for special building materials from the user 
C defined file SPMFIL. Data is read into the special material common blocks.
C 
C Details of common block variables for special materials:
C nspmnod  - number of user designated special materials node.
C ispmloc  - array holding location of material 1-zone, 2-surface, 3 -node.
C ispmtype - array holding details of special material type
C           1-type number      2-opaque/transparent flag.
C spmdat   - array holding miscellaneous data for special materials subroutines.
C spmlabel - text label describing special materal (max 16 chars).
C spmflg   - flag indicating whether special materials are active (0 or 1).
      subroutine SPMINIT

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C Special materials file flag.
      common/spmfxst/ispmxist,spflnam 
 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

C Path to problem and command line file (if any).
      common/rpath/path
      common/filep/ifil

      character outs*124,outstr*124,spmlabel*16
      character*72 path,spflnam,H,ltmp*72

      H(1)='The special materials file contains information'
      H(2)='relating to advanced materials within the existing'
      H(3)='building construction. Examples of such materials'
      H(4)='include; photovoltaics, electrochromics, phase-'
      H(5)='change etc .... If you have created such a file'
      H(6)='enter the name of the file; if not press ok and'
      H(7)='you will be returned to the main menu.'

C Set the number of special material data items per line
      nspmdatl=10

C Open special materials file on temporary file channel.
      ispmfil=ifil+1
      call erpfree(ispmfil,istat)
      call edisp(iuout,' ')
      write(outs,'(a,a)') ' Path is: ',path
      call edisp(iuout,outs)
      ltmp=spflnam
      if(ltmp(1:2).eq.'  '.or.ltmp(1:4).eq.'UNKN')then
      call easks(ltmp,' Special Materials file ?',
     &' ',72,' ','special materials file',IER,7)
      endif
      spflnam=ltmp
      if(IER.NE.0) then
        write(outs,'(a,a)') 'Could not find file ',spflnam
        call edisp(iuout,outs)      
        ispmxist=0
        return
      endif
      call efopseq(ispmfil,spflnam,1,IER)
      if(IER.NE.0) then
        write(outs,'(a,a)') 'Error opening file ',spflnam
        call edisp(iuout,outs)
        ispmxist=0
        return
      endif

      call edisp(iuout,'Reading special materials file ...')
      call edisp(iuout,spflnam) 

C Read data from opened special materials file to the common blocks.
C Get the number of entries in the file. 
      call stripc(ispmfil,outstr,1,ND,0,
     &'special materials file error..',IER)
      if(IER.ne.0) goto 999
      k=0
      call egetwi(outstr,K,nspmnod,1,mspmnod,'W',
     &'no spmatl node',IER)
      if(IER.ne.0) goto 999

C For each entry read the associated special materials data.
      do 10 I=1,nspmnod

C Firstly the material label.
        call stripc(ispmfil,outstr,1,ND,0,
     &'special materials file error..',IER)
        if(IER.ne.0) goto 999
        spmlabel(I)=outstr(1:16)

C Next read the material info.
        call stripc(ispmfil,outstr,5,ND,0,
     &'special materials file error..',IER)
        if(IER.ne.0) goto 999
        k=0
        call egetwi(outstr,K,ivar1,1,mcom,'W','-',IER)
        ispmloc(I,1)=ivar1
        call egetwi(outstr,K,ivar2,1,ms,'W','-',IER)
        ispmloc(I,2)=ivar2   
        call egetwi(outstr,K,ivar3,1,0,'-','-',IER)
        ispmloc(I,3)=ivar3
        call egetwi(outstr,K,ivar4,1,99,'-','-',IER)
        ispmtyp(I,1)=ivar4
        call egetwi(outstr,K,ivar5,0,1,'W','-',IER)
        ispmtyp(I,2)=ivar5
        if(IER.ne.0) goto 999

C Read the material miscellaneous data.
        call stripc(ispmfil,outstr,1,ND,0,
     &'special materials file error..',IER)
        if(IER.ne.0) goto 999
        K=0
        call egetwi(outstr,K,nspmdat,1,mspmdat,'W','-',IER)
        nnodat(I)=nspmdat
        if(IER.ne.0) goto 999
        istrt=1
        iend=nspmdatl

 7      call stripc(ispmfil,outstr,0,ND,0, 
     &'special materials file error..',IER)
        if(IER.ne.0) goto 999
        K=0
        if(nnodat(i).le.nspmdatl)then
          k=0
          do 20 J=1,nnodat(i)
            call egetwr(outstr,K,var,1.,1.,'-','-',IER)
            if(IER.ne.0) goto 999 
            spmdat(I,J)=var           
  20        continue
        else
          K=0
          do 21 J=istrt,iend
            call egetwr(outstr,K,var,1.,1.,'-','-',IER)
            if(IER.ne.0) goto 999 
            spmdat(I,J)=var           
  21      continue
          istrt=istrt+nspmdatl
          iend=iend+nspmdatl
          if(iend.gt.nnodat(i))then
            iend=nnodat(i)
            call stripc(ispmfil,outstr,0,ND,0, 
     &'special materials file error..',IER)
            k=0
            do 22 J=istrt,iend
              call egetwr(outstr,K,var,1.,1.,'-','-',IER)
              if(IER.ne.0) goto 999 
              spmdat(I,J)=var           
  22        continue
          else
            goto 7
          endif
        endif
  10  continue

  99  close(ispmfil)
      return

 999  write(outs,'(a)')'An error has occurred while reading the '
      call edisp(iuout,outs)
      write(outs,'(a)')'special materials file - check file format!'
      call edisp(iuout,outs)
      ispmxist=0
      goto 99

      end

C ********* SPMWRIT
C Subroutine SPMWRIT writes out a special materials file if the
C flag ispmxist is greater than 0 i.e. when special materials have been
C defined.

      subroutine SPMWRIT
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      common/FILEP/IFIL
      common/C21/IFCFG,cfgroot,LCFGF

C Special materials file flag
      common/spmfxst/ispmxist,spflnam  

      character outs*124,deflnam*72,spflnam*72,lcfgf*72,h*72
      character spmlabel*16,lfil*72,cfgroot*24
      logical ok,dok

C Set the number of special material data items per line
      nspmdatl=10

C Determine whether a special materials file is to be created
      if(ispmxist.eq.1) then
        dok=.true.
        h(1)='Special materials information has been'
        h(2)='defined but recent changes not yet saved!'
        call askok(' ','Update special materials file?',ok,dok,2)
        if(.not.ok) return
      endif
      deflnam='data.spm'
      lfil=spflnam
      h(1)='This file contains information about special materials'
      h(2)='associated with a problem.'
 4    call easks(lfil,' ',' Special materials filename?',72,
     &  deflnam,' SPM filename',IER,2)
      if(lfil.ne.' ')spflnam=lfil
      if(IER.GT.0) return

C Open spm file with filename 'spflnam' unit number IFIL+1.
      ispmfil=ifil+1
      call efopseq(ispmfil,spflnam,4,IER)
      if(IER.GT.0) then
        write(outs,'(a,a)') 'an error has occurred opening ',spflnam
        call edisp(iuout,outs)
        goto 4
      endif

C Write header label for file
      write(ispmfil,'(a)')'# ESP-r special materials file used in '
      write(ispmfil,'(a,a)')'# configuration file ',LCFGF(1:20)

C Write out nodal information
      write(ispmfil,'(i4,a)') nspmnod,
     &'# No. of special material nodes.'
      do 10 isp=1,nspmnod
        write(ispmfil,'(a,i3)') '# Node No:',isp
        write(ispmfil,'(a,a)') spmlabel(isp),' # label'
        write(ispmfil,'(a)')'# Zone Surf Node Type Opq/Trn'
        write(ispmfil,'(2x,i2,2x,i2,2x,i2,2x,i2,2x,i2)')
     &  ispmloc(isp,1),ispmloc(isp,2),ispmloc(isp,3),
     &ispmtyp(isp,1),ispmtyp(isp,2)
 
C Write node specific data.
        write(ispmfil,'(a)')'# No. of data items.'
        write(ispmfil,'(2x,i2)') nnodat(isp)
        write(ispmfil,'(a)')'# Data:'
        istrt=1
        iend=nspmdatl

 7      continue
        if(nnodat(isp).le.nspmdatl)then
          write(ispmfil,'(10F10.4)')(spmdat(isp,idat),idat=1,
     &    nnodat(isp))
        else
          write(ispmfil,'(10F10.4)')(spmdat(isp,idat),idat=istrt,
     &iend)
          istrt=istrt+nspmdatl
          iend=iend+nspmdatl
          if(iend.gt.nnodat(isp))then
            iend=nnodat(isp)
            write(ispmfil,'(10F10.4)')(spmdat(isp,idat),idat=istrt,
     &nnodat(isp))
          else
            goto 7
          endif
        endif
 10   continue

C Set spm file flag to 'saved' 
      ispmxist=1

C Close the special materials file.
      call erpfree(ispmfil,istat)
      return
      end
