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

C This file contains a subroutine to write dfs results to
C an ASCII file.  Two other output formats are available:
C `matlab' format and `photon' format.  ASCII format will
C be the choice for users who do not have access to these
C post-processing packages.  ASCII format can also be useful
C for examining turbulence-model predictions, such
C as the turbulence energy, the dissipation rate of turbulence
C energy, and the turbulent viscosity.
C
C Note that ASCII output is only available for dfs-only
C simulations.  The default output format for bps-dfs conflated
C simulations is photon.
C

C Contains:
C PRNTXT - write dfs results to an ASCII file.


C ********************* PRNTXT *********************
C PRNTXT -write dfs results to an ASCII file.
      SUBROUTINE PRNTXT(ifl)
#include "cfd.h"

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1          DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2          BETA(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1          Wf(ntcelx,ntcely,ntcelz),
     2          P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3          TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1           RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1          DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2          DZHP(ntcelz),DZPL(ntcelz),
     3          SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4          XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      common/slicom/islice,jslice,kslice

      real tur2lam(ntcelx,ntcely,ntcelz)
      integer islice,jslice,kslice
      integer dummy(ntcely)
      integer jent,jmid,jend

C This is set up to output results for a given x cross-section.
C `islice' is set in dfs.F and should have the value of 2 for 2D
C problems.

C `dummy' is an array holding the index numbers in the j-direction.
C It is only used to get a decent-looking display of the j-coordinates
C in the output file.
      do 12 j=1,nj
        dummy(j) = j
 12   continue

C Start the output file.
      write(ifl,100) 'dfs ASCII output report'
 100  format('**** ',A40)

C Write out the grid.
      write(ifl,105) ni,nj,nk
 105  format(/,'*** Number of cells ***',/,'NI = ',i2,
     &       ' NJ = ',i2,' NK = ',i2)
      write(ifl,108) 'Y','j','YP','YV'
      write(ifl,107) (j,yp(j),yv(j),j=1,nj)
      write(ifl,108) 'Z','k','ZP','ZW'
      write(ifl,107) (k,zp(k),zw(k),k=1,nk)
 107  format(i3,3x,F5.3,3x,F5.3)
 108  format(/,'*** Mesh in ',A1,' direction ***',/,2x,A1,6x,A2,6x,A2)

C Format statements for field-variable outputs.
 106  format(/,A50,/,9x,15('[j=',i2,']',1x))
 110  FORMAT('[k=',i2,']',62(2x,f5.2))
 111  FORMAT('[k=',i2,']',62(1x,e7.2))

C Output the temperature distribution for islice.
      write(ifl,106) '******* temperature ********',(dummy(j),j=1,nj)
      DO 20 k=nk,1,-1
        WRITE(ifl,110) k,(Tf(islice,j,K),j=1,nj)
 20   CONTINUE

C Output the V-velocity distribution for islice.
      write(ifl,106) '******* V-velocity ********',(dummy(j),j=1,nj)
      DO 21 k=nk,1,-1
        WRITE(ifl,110) k,(Vf(islice,j,K),j=1,nj)
 21   CONTINUE

C Output the W-velocity distribution for islice.
      write(ifl,106) '******* W-velocity ********',(dummy(j),j=1,nj)
      DO 22 k=nk,1,-1
        WRITE(ifl,110) k,(Wf(islice,j,K),j=1,nj)
 22   CONTINUE

C Output the kinetic energy distribution for islice.
      write(ifl,106) '**** turbulence energy *****',(dummy(j),j=1,nj)
      DO 23 k=nk,1,-1
        WRITE(ifl,111) k,(TEf(islice,j,K),j=1,nj)
 23   CONTINUE

C Output the dispersion of kinetic energy for islice.
      write(ifl,106) '******* Epsilon ********',(dummy(j),j=1,nj)
      DO 24 k=nk,1,-1
        WRITE(ifl,111) k,(EDf(islice,j,K),j=1,nj)
 24   CONTINUE

C Calculate the ratio of the turbulent viscosity to the laminar
C viscosity, and print the results for each grid point.
      do 30 i=1,ni
        do 301 j=1,nj
          do 302 k=1,nk
            tur2lam(i,j,k) = ( vis(i,j,k)-viscos )/viscos
 302      continue
 301    continue
 30   continue

      write(ifl,106) '*** [VIS(i,j,k)-VISCOS]/VISCOS ***',
     &                                        (dummy(j),j=1,nj)
      DO 40 k=nk,1,-1
        WRITE(ifl,111) k,(tur2lam(islice,j,K),j=1,nj)
 40   CONTINUE

C Output for plotting V=f(Z) flow profiles.
      write(ifl,*)
      write(ifl,100) '*** V=f(Z) ***'

C Plot the flow profile near the entrance, the middle,
C and near the end of the plates.
      jent=3
      jmid=10
      jend=18

      write(ifl,120) Yv(jent),Yv(jmid),Yv(jend)
 120  format('#',2x,'Z(m)',4x,3('V(@Y=',f4.2,')',2x))
      do 50 k=2,nk-1
        write(ifl,121) zp(k),Vf(islice,jent,K),
     &             Vf(islice,jmid,K),Vf(islice,jend,K)
 121    format(f7.3,3(1x,f10.6))
 50   continue

C Plot $mu sub t$/$mu$ ratios at the same locations.
      write(ifl,*)
      write(ifl,100) '*** R=f(Z) ***'
      write(ifl,*) '# Note that R=$mu sub t over mu$'
      write(ifl,130) Yv(jent),Yv(jmid),Yv(jend)
 130  format('#',2x,'Z(m)',4x,3('R(@Y=',f4.2,')',2x))

      do 55 k=2,nk-1
        write(ifl,122) zp(k),tur2lam(islice,jent,K),
     &             tur2lam(islice,jmid,K),tur2lam(islice,jend,K)
 122    format(e9.3,3(1x,f10.6))
 55   continue

      RETURN
      END
