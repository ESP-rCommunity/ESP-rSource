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

C ************* cfgmsc
C This file contains the following subroutine:
C       pcwrt

C*********************** pcwrt ************************
C This subroutine writes plant network data to configuration
C file.
      subroutine pcwrt
#include "plant.h"
#include "building.h"

C esprdbfile.h supplies the following:
C LPCDB/IPCDB (for plant template database)

      common/OUTIN/IUOUT,IUIN

C Plant network description.
      COMMON/C8/LPNAM
      common/C23/IFPNF,LPNF

C The first two variables in C13PS are redundunt.
C Only NPMTYP is used.
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP

C Mass flow network.
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Components variables.
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)

C These variables hold data related to new component
C parameters.
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)

C Connections variables.
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

C Containments variables
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)

C Electrical details for specified plant components
      common/pcelflg/ipcelf(mpcom)
      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

C Strings describing plant components.
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      common/pcnam/pcname(mpcom)

C Strings describing component input
      common/hfpar/hfpdsc(mpcom,madata)
      character hfpdsc*68

      character pcdesc*80, pcname*15,LPNF*72,dstmp*24
      character lpnam*72, laprob*72

C Common controlling format of plant network file
      common/PLNFMT/bPLN_format_long
      logical bPLN_format_long

C Open the file. 
      CALL EFOPSEQ(IFPNF,LPNF,4,IER)
      if(ier.gt.0) then
        call edisp(iuout,' Error opening file ....')
        return 
      endif

C Write plant file heading
      call dstamp(dstmp)
      write(IFPNF,'(2a)') 
     &'ESP-r plant file version 2 written on: ',dstmp

C Write lpnam of problem.
      write(IFPNF,'(a)') '# Project title:'
      write(IFPNF,'(a70)') lpnam

C Write components definitions.
      write(IFPNF,'(a)')
     &      '# Total no. of specified components and simulation type'
      write(IFPNF,'(2I5)') npcomp, npmtyp
      do 10 ipcomp=1, npcomp
         write(IFPNF,100) ipcomp,', ',pcdesc(npref(ipcomp))
  100    format('#-> ',I3,a2,a72)
         write(IFPNF,'(2x,A15,I5)') pcname(ipcomp),npref(ipcomp)
         write(IFPNF,110) nci(ipcomp), nci(ipcomp)
  110    format(I5,8x,'# Component has',i4,' control variable(s).')
         if(nci(ipcomp).gt.0)  write(IFPNF,*)
     &       (cdata(ipcomp,jj),jj=1,nci(ipcomp))
         if(ipcelf(ipcomp).eq.0) then
           write(IFPNF,'(I5)') npi(ipcomp)
         else
           write(IFPNF,'(i5,1x,i5)')npi(ipcomp),5
         endif
         if(npi(ipcomp).ne.0) then
C Flag for inclusion of comments in .pln file.
            if ( .not. bPLN_format_long ) then
C Write out data in 6-column block without comments...            
              write(IFPNF,112)
     &                 (tadata(ipcomp,jj),jj=1,npi(ipcomp))
  112         format(12(6(g12.5,1x),/))
              if(amod(float(npi(ipcomp)),6.0).eq.0.0) backspace IFPNF
            else
C ...or optionally dump comments along with parameters
              do jj=1, npi(ipcomp)
                write (IFPNF, 1120)
     &             '     ',
     &             tadata(ipcomp,jj),
     &             ' # ',
     &             jj,
     &             ' ',
     &             hfpdsc(ipcomp,jj)(1:64)
              enddo
 1120       format (A,g12.5,A,I3,A,A)              
            endif 
         endif
         if(ipcelf(ipcomp).gt.0) then
           write(IFPNF,'(a)')'# Component electrical details.'
           write(IFPNF,113)PFP(ipcomp),IPFP(ipcomp),PWRP(ipcomp),
     &BVOLTP(ipcomp),IPHP(ipcomp)
  113       format(2x,F5.3,1X,I3,1X,F10.3,1X,F10.3,1X,I3)
         endif
   10 continue

C Write connections definitions.
      if(npcon.ne.0) write(IFPNF,76)
      write(IFPNF,'(I3,10x,A)') npcon,'# Total number of connections'
      write(IFPNF,707)
      if(npcon.eq.0) goto 1000
      do 20 icon=1, npcon
         if(ipct(icon).eq.1.or.ipct(icon).eq.3) then
            write(IFPNF,152) pcname(ipc1(icon)),ipn1(icon),ipct(icon),
     &                    pcname(ipc2(icon)),ipn2(icon),pcondr(icon),
     &'#',icon
         elseif(ipct(icon).eq.2) then
            write(IFPNF,150) pcname(ipc1(icon)),ipn1(icon),ipct(icon),
     &                    pcname(ipc2(icon)),ipn2(icon),pcondr(icon),
     &                    pconsd(icon,1),pconsd(icon,2),'#',icon
         elseif(ipct(icon).eq.4) then
            write(IFPNF,154) pcname(ipc1(icon)),ipn1(icon),ipct(icon),
     &                    pcname(ipc2(icon)),ipn2(icon),pcondr(icon),
     &                    pconsd(icon,1),'#',icon
         endif
   20 continue
  707 format('# receiving       node  conncn sending          node  ',
     &  'diversion  suppl1   suppl2',/,
     &       '# component              type  component',
     &    '                ratio')
  150 format(2x,A15,I4,2x,I4,5x,A15,I4,4x,F5.3,2x,2F7.2,1X,A1,I3)
  152 format(2x,A15,I4,2x,I4,5x,A15,I4,4x,F5.3,17X,A1,I3)
  154 format(2x,A15,I4,2x,I4,5x,A15,I4,4x,F5.3,2x,F7.2,8X,A1,I3)
   76 format('# The following is a list of component connections.')

C Write containments data.
 1000 if(ncont.ne.0) then
         write(IFPNF,207)
         write(IFPNF,'(I3,10x,A)') ncont,
     &          '# Total number of containments'
         write(IFPNF,710)
      else
         write(IFPNF,'(A)') '# No containment temperatures defined'
         write(IFPNF,'(I3,10x,A)') ncont
      endif
 207  format('# The following is a list of containment temperatures.')

      if(ncont.ne.0) then
         do 30 icont=1, ncont
            if(indcp(icont).eq.1) then
               write(IFPNF,709) pcname(ipcc(icont)), indcp(icont),
     &             pcname(int(cntdat(icont,1))),(cntdat(icont,j),j=2,3)
            else
               write(IFPNF,708)
     &             pcname(ipcc(icont)), indcp(icont),
     &             (cntdat(icont,j),j=1,3)
            endif
   30    continue
  708 format(2x,A15,2x,I4,10x,3F10.5)
  709 format(2x,A15,2x,I4,5x,A15,2F10.5)
  710 format('# Component       cont type        suppl1     suppl2',
     &       '    suppl3')
      endif

C Write mass flow network information.
      if(iflwn.eq.0) then
         write(IFPNF,1001)
 1001    format('# No mass flow network defined.')
         write(IFPNF,'(I5)') iflwn
      else
         write(IFPNF,1002)
 1002    format('# Mass flow network definition.')
         write(IFPNF,'(I5)') iflwn
         write(IFPNF,1010)
 1010    format('Mapping of mass flow network connections to',/,
     &          'plant network connections.')
         write(IFPNF,1011)
     &               (icffs(jj),jj=1,npcon)
 1011       format(12(6(I4,1x),/))
            if(amod(float(icffs(jj)),6.0).eq.0.0) backspace IFPNF
      endif
      close(IFPNF)
      return
      end
