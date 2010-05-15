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

C This file contains the following routines:

C      mzwdar
C      mtxctl
C      mzmrx1
C      mzmrx2
C      mzmrx3
C      mzmrx4
C      mzcnf1
C      mzcnf2
C      mzsad1
C      mzsad2
C      mzcms1
C      mzcms2
C      mzcms3
C      mzcms4
C      mzcnb1
C      mzcnb2
C      mzpst1
C      mzback
C MZGSUP controls the simulation within the 3D ground start up period.
C FILL1G fills up the 1D ground temperature array.

C ******************** mzwdar

C Computes the total area of internal and external default windows
c for transfer to the results database.  This cannot
C be done prior to the time loop because the necessary adjacency
C information is not established until the simulation commences.

C << there is probably no reason to call this subroutine as there
C << are no longer any so-called default windows and doors.
      subroutine mzwdar(icomp)
#include "building.h"

      common/outin/iuout,iuin
      common/filep/ifil

      common/prec1/sna(mcom,ms)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/adjc/ie(mcom,ms),atp(mcom,ms),atf(mcom,ms),arp(mcom,ms),
     &            arf(mcom,ms)

      character outs*124

      iunit=ifil+2
      nn=nconst(icomp)
      zgae=0.
      zgai=0.
      zdae=0.
      zdai=0.

C Find appropriate record in library.
      irec=2
      read(iunit,rec=irec,iostat=istat,err=1000)nst
      iresrv=nst-ncomp+icomp-1

C Write dummy values for default doors and windows to results library.
      irec=iresrv
      write(iunit,rec=irec,iostat=istat,err=1000)zgae,zgai,zdae,zdai
    5 return

 1000 write(outs,'(A,I5,A)')
     &  'mzwdar: results library error at record',irec,
     &  ' while writing default window and door areas.'
      call edisp(iuout,outs)
      call epwait
      goto 5
      end

C *********************************************************************
C MTXCTL controls the zone heat matrix solving process according to the
C sensor location of the associated control function.
C *********************************************************************
      SUBROUTINE MTXCTL(ICOMP)
#include "building.h"
#include "control.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/cctl/icascf(mcom)
      COMMON/GRND110/GRNDSP

C CFD
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      LOGICAL        GRNDSP
      character LCFD*72

c      if(ncf.eq.0)then
c
C No control is imposed.
c        IF(.NOT.GRNDSP.AND.
c     &   ((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
c     &    (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS)))then
c
C Solve matrix considering inside airflow modelled by CFD.
c          call mzmrx5(icomp)
c        else
c          call mzmrx1(icomp)
c         endif
c      else
c        ictl=icascf(icomp)
c        isl1=ibsn(ictl,1)
c        isl2=ibsn(ictl,2)
c        isl3=ibsn(ictl,3)
c
C Control is on mixed air and surface temperatures.
c        if(isl1.eq.-2.and.isl2.eq.icomp)then
c          call mzmrx4(icomp)
c
C Control is on intra-constructional temperature.
c        elseif(isl1.gt.0.and.isl2.gt.0.and.isl3.gt.0)then
c          call mzmrx3(icomp)
c
C Control is on surface temperature.
c        elseif(isl1.gt.0.and.isl2.gt.0.and.isl3.eq.0)then
c          call mzmrx2(icomp)
c
C Control is on air point or external air temperature (or no control
C is imposed).
c        else
c          IF((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
c     &      (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS))then
c
C Solve matrix considering inside airflow modelled by CFD.
c            call mzmrx5(icomp)
c          else
c            call mzmrx1(icomp)
c          endif
c        endif
c      endif
c      return
c      end


      if(ncf.eq.0)goto 11
      ii=icascf(icomp)
      if(ii.eq.0)goto 11

      if(ibsn(ii,1).ge.0.and.ibsn(ii,2).eq.0)nn=1
      if(ibsn(ii,1).le.0)nn=1
      if(ibsn(ii,1).gt.0.and.ibsn(ii,2).gt.0.and.ibsn(ii,3).eq.0)nn=2
      if(ibsn(ii,1).gt.0.and.ibsn(ii,2).gt.0.and.ibsn(ii,3).gt.0)nn=3
      if(ibsn(ii,1).eq.-2.and.ibsn(ii,2).eq.icomp)nn=4
      goto (11,12,13,14),nn

C Control is on air point or external air temperature (or no control
C is imposed).
   11 continue
      if(.NOT.GRNDSP.AND.
     &  ((IFCFD(ICOMP).EQ.2.AND.NSINC.GT.1).OR.
     &   (IFCFD(ICOMP).EQ.-2.AND.IDYP.GE.ISDS)))then

C Solve matrix considering inside airflow modelled by CFD.
        call mzmrx5(icomp)
      else
        call mzmrx1(icomp)
      endif
      return

C Control is on surface temperature.
   12 call mzmrx2(icomp)
      return

C Control is on intra-constructional temperature.
   13 call mzmrx3(icomp)
      return

C Control is on mixed air and surface temperatures.
   14 call mzmrx4(icomp)
      return
      end

C ******************** mzmrx1

C The main controller for the matrix handling and
C solution relating to air point temperature control.

      subroutine mzmrx1(icomp)
#include "building.h"

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

C Forward reduce construction matrices.
      nc=nconst(icomp)
      do 10 i=1,nc
         ii=i
         call mzcnf1(icomp,ii)

C Adjust surface equation.
         call mzsad1(icomp,ii)
   10 continue

C Invert and solve intra-zone matrix.
      call mzcms1(icomp)

C Back substitute in reduced construction matrices.
      do 20 j=1,nc
         jj=j
         call mzcnb1(icomp,jj)
   20 continue

      return
      end

C ******************** mzmrx2

C The main controller for the matrix handling and
C solution relating to surface temperature control.

      subroutine mzmrx2(icomp)
#include "building.h"

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

C Forward reduce construction matrices.
      nc=nconst(icomp)
      do 10 i=1,nc
         ii=i
         call mzcnf1(icomp,ii)

C Adjust surface equation.
         call mzsad1(icomp,ii)
   10 continue

C Invert and solve intra-zone matrix.
      call mzcms2(icomp)

C Back substitute in reduced construction matrices.
      do 20 j=1,nc
         jj=j
         call mzcnb1(icomp,jj)
   20 continue

      return
      end

C ******************** mzmrx3

C This is the main controller for the matrix handling
C and solution relating to intra-constructional temperature
C control.

      subroutine mzmrx3(icomp)
#include "building.h"

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

C Forward reduce construction matrices.
      nc=nconst(icomp)
      do 10 i=1,nc
         ii=i
         call mzcnf2(icomp,ii)

C Adjust surface equation.
      call mzsad2(icomp,ii)
   10 continue

C Invert and solve intra-zone matrix.
      call mzcms3(icomp)

C Back substitute in reduced construction matrices.
      do 20 j=1,nc
         jj=j
         call mzcnb2(icomp,jj)
   20 continue

      return
      end

C ******************** mzmrx4

C This is the main controller for the matrix handling
C and solution relating to mixed air/ mean radiant
C temperature control.

      subroutine mzmrx4(icomp)
#include "building.h"

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

C Forward reduce construction matrices.
      nc=nconst(icomp)
      do 10 i=1,nc
         ii=i
         call mzcnf1(icomp,ii)

C Adjust surface equation.
         call mzsad1(icomp,ii)
   10 continue

C Invert and solve intra-zone matrix.
      call mzcms4(icomp)

C Back substitute in reduced construction matrices.
      do 20 j=1,nc
         jj=j
         call mzcnb1(icomp,jj)
   20 continue

      return
      end

C ******************** mzcnf1

C Reduce a partitioned construction matrix to the end of
C the forward reduction stage and pass across the adjusted
C coefficients relating to the internal surface (future) term,
C the equation right-hand-side (present) term, the next-to-internal
C surface term and the plant term.

C That is, for a multilayered construction represented by 5 nodes
C (2 homogeneous elements) with heat injection at the centre plane
C of the innermost element, the equation-set is:

C XX               X
C XXX              X   Matrix 'w' holds
C  XXX             X   the X's.
C   XXX          X X
C    X+++ etc

C ^1             ^2^3

C where:   ^1 is the nodal temperature future coefficients
C          ^2 is the plant coefficient
C          ^3 is the present time coefficient

C This coefficient-set is reduced to:

C **               *
C  **              *   Matrix 'wa' holds
C   **             *   the *'s.
C   (**          * *) <a1,a2,a3 & a4
C    X+++ etc

C and the bracketed terms (a1,a2,a3 & a4) are passed over to
C mzsad1 where the next-to-surface coefficient can be eliminated
C from the corresponding surface equation.

      subroutine mzcnf1(icomp,iconst)
#include "building.h"
#include "control.h"

      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/coneqn/w(ms,mnm,5)
      common/coeadj/a1,a2,a3,a4
      common/conadj/wa(ms,mn,6)

      common/cctl/icascf(mcom)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character outs*124

C Set control function associated with current zone.
      if(ncf.eq.0)goto 1
      ic=icascf(icomp)

C Set number of construction nodes excluding internal
C surface node.
    1 n=nndc(icomp,iconst)-1
      do 10 i=2,5
         wa(iconst,1,i)=w(iconst,1,i)
   10 continue

C Is plant interaction node intra-constructional ?
      if(ncf.eq.0.or.ic.eq.0)goto 2
      if(iban(ic,3).gt.0)goto 3

C No !
    2 wa(iconst,1,4)=0.
      goto 4

C Yes ! : is plant interaction node located at 'outside'
C surface ?
    3 if(iban(ic,2).eq.iconst.and.iban(ic,3).eq.1)goto 4
      goto 2

C Proceed with forward reduction.
    4 do 20 j=1,n
         x1=w(iconst,j+1,1)
         x2=wa(iconst,j,2)
         x3=x1/x2
         do 30 k=1,4
            wa(iconst,j,k)=wa(iconst,j,k+1)*x3
            if(j.eq.n)goto 30
            if(k.eq.3)goto 5
            if(k.eq.4)goto 6
            wa(iconst,j+1,k)=w(iconst,j+1,k)-wa(iconst,j,k)
            goto 30
    5       wa(iconst,j+1,k)=w(iconst,j+1,k)
    6       wa(iconst,j+1,k+1)=w(iconst,j+1,k+1)-wa(iconst,j,k)
   30    continue
   20 continue
      a1=wa(iconst,n,1)
      a2=wa(iconst,n,2)
      a3=wa(iconst,n,3)
      a4=wa(iconst,n,4)

C Trace output ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(27).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)')' Subroutine mzcnf1   Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

      write(outs,'(A,F15.5)') ' a1 = ',a1
      call edisp(itu,' ')
      call edisp(itu,outs)
      write(outs,'(A,F15.5)') ' a2 = ',a2
      call edisp(itu,outs)
      write(outs,'(A,F15.5)') ' a3 = ',a3
      call edisp(itu,outs)
      write(outs,'(A,F15.5)') ' a4 = ',a4
      call edisp(itu,outs)
 9999 return
      end

C ******************** mzcnf2

C  Reduces a partitioned construction matrix to the end of the
C  forward reduction stage and passes across the adjusted coefficients
C  (two sets) relating to the internal constructional control node
C  (future) term, right-hand side (present) term, surface term
C  (future), next-to-inside surface term (future) and the
C  intra-constructional plant term (future) if any.

C  That is, for a multilayered construction represented by 5 nodes
C  (2 homogeneous elements) where node 2 is the control point and plant
C  interaction point, the equation-set is:

C   X  X                      X
C   X  X' X                X  X      Matrix 'w' holds
C      X  X  X                X      the X's
C         X  X  X             X
C            X  X  +  +etc    X

C    ^1                    ^2 ^3

C  Where,   X' is the control node
C           ^1 is the nodal temperature future coefficients
C           ^2 is the plant coefficient
C           ^3 is the present coefficient

C  This coefficient set reduced to:

C   *  *                      *      Matrix 'wa' holds
C      *  *                *  *      the *'s
C     (*     *             *  *)  <  b1,b2,b3 & b4
C     (*        *          *  *)  <  a1,a2,a3 & a4
C            X  X  +  +etc

C  and the bracketed terms (a1,a2,a3,a4,b1,b2,b3 & b4) are passed over
C  to mzsad2 where the next-to-surface coefficient and the surface
C  node coefficient can be eliminated from the corresponding surface
C  equation and the control node coefficient introduced if appropriate.

      subroutine mzcnf2(icomp,iconst)
#include "building.h"
#include "control.h"

      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/coneqn/w(ms,mnm,5)
      common/coeadj/a1,a2,a3,a4
      common/cadj2/b1,b2,b3,b4
      common/conadj/wa(ms,mn,6)

      common/cctl/icascf(mcom)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character outs*124

C Set control function associated with current zone.
      a1=0.0
      a2=0.0
      a3=0.0
      a4=0.0
      b1=0.0
      b2=0.0
      b3=0.0
      b4=0.0

C  Set number of construction nodes excluding internal
C  surface node and establish first row `wa' values.
      n=nndc(icomp,iconst)-1

C Initialise wa matrix,
      do 22 i=1,mn
        do 33 k=1,6
         wa(iconst,i,k)=0.
33      continue
22    continue
      
      do 10 i=1,5
         wa(iconst,1,i)=w(iconst,1,i)
   10 continue

C  Proceed with forward reduction.
      icrow=0
      ic=icascf(icomp)
      if(ic.eq.0.or.ncf.eq.0)goto 2
      ipos=ibsn(ic,3)
      if(ibsn(ic,2).eq.iconst)icrow=ipos
    2 j=0
    6 j=j+1
      if(j.eq.n+1)goto 99
      if(j.eq.icrow)goto 3      
      x1=w(iconst,j+1,1)
      x2=wa(iconst,j,2)
      x3=x1/x2
      do 20 k=1,4
         wa(iconst,j,k)=wa(iconst,j,k+1)*x3
         if(j.eq.n)goto 20
         if(k.eq.3)goto 4
         if(k.eq.4)goto 5
         wa(iconst,j+1,k)=w(iconst,j+1,k)-wa(iconst,j,k)
         goto 20
    4    wa(iconst,j+1,k)=w(iconst,j+1,k)
    5    wa(iconst,j+1,k+1)=w(iconst,j+1,k+1)-wa(iconst,j,k)    
   20 continue
      goto 6

    3 x1=w(iconst,j+1,2)
      x2=wa(iconst,j,3)
      if(j.eq.n)then
         x1=w(iconst,j+1,1)  
         x2=wa(iconst,j,2)
      endif
      x3=x1/x2      
      do 30 k=1,4
         kk=k
         if(k.eq.1)kk=6
         wa(iconst,j,kk)=wa(iconst,j,k+1)*x3
         if(j.eq.n)goto 30
         if(k.eq.3)goto 7
         if(k.eq.4)goto 8
         wa(iconst,j+1,kk)=w(iconst,j+1,k)-wa(iconst,j,kk)
         goto 30
    7    wa(iconst,j+1,k)=w(iconst,j+1,k)
    8    wa(iconst,j+1,k+1)=w(iconst,j+1,k+1)-wa(iconst,j,k)    
   30 continue
      
      if(j.eq.n)goto 98
      x1=w(iconst,j+2,1)
      x2=wa(iconst,j,2)
      x3=x1/x2            
      do 40 k=1,4
         wa(iconst,j,k)=wa(iconst,j,k)*x3

C in the case of icrow=n-1,
         if(j.eq.n-1.and.k.eq.4)then
           do 35 i=2,4
              wa(iconst,j+1,i)=wa(iconst,j+1,i+1)
   35      continue
           wa(iconst,j,6)=wa(iconst,j,6)*x3 
         endif
         if(j.eq.n-1)goto 40
         if(k.eq.1)goto 9
         if(k.eq.2)goto 11
         goto 12
    9    wa(iconst,j,6)=wa(iconst,j,6)*x3
         wa(iconst,j+2,6)=-wa(iconst,j,6)
         goto 40
   11    wa(iconst,j+2,k-1)=w(iconst,j+2,k-1)-wa(iconst,j,k)
         wa(iconst,j+2,k)=w(iconst,j+2,k) 
         wa(iconst,j+2,k+1)=w(iconst,j+2,k+1)
         goto 40
   12    wa(iconst,j+2,k+1)=w(iconst,j+2,k+1)-wa(iconst,j,k)
   
   40 continue
   
   17 j=j+1
       if(j.eq.n.and.icrow.ne.n-1)then
        do 45 m=2,4
           wa(iconst,j,m)=wa(iconst,j,m+1)
45      continue
      endif
      if(j.eq.n)goto 97
      x1=wa(iconst,j+1,2)
      x2=wa(iconst,j,3)
      x3=x1/x2
      do 50 k=1,5
         if(k.eq.5)goto 13
         wa(iconst,j,k)=wa(iconst,j,k+1)*x3
         if(k.eq.3.or.k.eq.4)goto 14
         wa(iconst,j+1,k)=wa(iconst,j+1,k)-wa(iconst,j,k)
         goto 50
   14    wa(iconst,j+1,k+1)=wa(iconst,j+1,k+1)-wa(iconst,j,k)
         goto 50
   13    wa(iconst,j,6)=wa(iconst,j,6)*x3
         wa(iconst,j+1,6)=wa(iconst,j+1,6)-wa(iconst,j,6)
         
   50 continue
      x1=w(iconst,j+2,1)
      x2=wa(iconst,j,2)
      x3=x1/x2
      do 60 k=2,5
         if(k.eq.5)goto 15
         wa(iconst,j,k)=wa(iconst,j,k)*x3
         if(j.eq.n-1)goto 60
         if(k.eq.3.or.k.eq.4)goto 16
         wa(iconst,j+2,k-1)=w(iconst,j+2,k-1)-wa(iconst,j,k)
         wa(iconst,j+2,k)=w(iconst,j+2,k)
         wa(iconst,j+2,k+1)=w(iconst,j+2,k+1)
         goto 60
   16    wa(iconst,j+2,k+1)=w(iconst,j+2,k+1)-wa(iconst,j,k)
         goto 60
   15    wa(iconst,j,6)=wa(iconst,j,6)*x3 
         if(j.eq.n-1)goto 60
         wa(iconst,j+2,6)=-wa(iconst,j,6)
   60 continue
   
      goto 17

C Establish `a' and `b' coefficients as appropriate.  'a'
C coefficient only for the case of no intra-constructional
C control node.

   99 a1=wa(iconst,n,1)
      a2=wa(iconst,n,2)
      a3=wa(iconst,n,3)
      a4=wa(iconst,n,4)
      goto 96

C `a' and `b' coefficients.
   97 b1=wa(iconst,n-1,6)
      b2=wa(iconst,n-1,2)
      b3=wa(iconst,n-1,3)
      b4=wa(iconst,n-1,4)

   98 a1=wa(iconst,n,6)
      a2=wa(iconst,n,2)
      a3=wa(iconst,n,3)
      a4=wa(iconst,n,4)
   96 continue

C  Trace output ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(27).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)')' Subroutine MZCNF2    Trace output',ICNT
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output the `w' and `wa' matrices and a1,a2,a3,a4,b1,b2,
C b3 and b4.
      write(outs,'(15X,A,I3,A,I3)')'Zone',icomp,' Construction',iconst
      call edisp(itu,outs)
      call edisp(itu,'                      Initial Matrix')
      write(outs,99972)
99972 format(' Node',8x,'1',12x,'2',12x,'3',12x,'4',12x,'5')
      call edisp(itu,outs)
      call edisp(itu,' ')

      do 9990 i=1,n
         write(outs,9996)i,(w(iconst,i,j),j=1,6)
 9996    format( i4,5(1x,f12.3))
         call edisp(itu,outs)
 9990 continue
      call edisp(itu,'                     Matrix solution')
      write(outs,9995)
 9995 format(' Node',6x,'1',10x,'2',10x,'3',10x,'4',10x,'5',10x,'6')
      call edisp(itu,outs)
      call edisp(itu,' ')
      do 9980 i=1,n
         write(outs,9994)i,(wa(iconst,i,j),j=1,6)
 9994    format( i4,6(1x,f10.3))
         call edisp(itu,outs)
 9980 continue
      call edisp(itu,'  ')
      call edisp(itu,'  a`s')
      write(outs,9993)a1,a2,a3,a4
 9993 format(6x,'  a1=',f11.3,'  a2=',f11.3,
     &          '  a3=',f11.3,'  a4=',f11.3)
      call edisp(itu,outs)
      call edisp(itu,'  ')
      call edisp(itu,'  b`s')
      write(outs,9992)b1,b2,b3,b4
 9992 format(6x,'  b1=',f11.3,'  b2=',f11.3,
     &          '  b3=',f11.3,'  b4=',f11.3)
      call edisp(itu,outs)
      call edisp(itu,'  ')
 9999 continue

      return
      end

C ******************** mzsad1

C Adjust the appropriate surface node equation
C coefficients in terms of the results of mzcnf1.
C That is:

C * *              * *     ) adjusted next-to-surface node
C |                          coefficients
C |
C X X X X X X X    X X     ) surface coefficients for a
C |                          6-sided zone
C |
C |
C algebraic summation of these two terms must equal zero.

C This results in:

C 1 2 X X X X X    3 4     ) where 1,2,3 & 4 are adjusted
C                            coefficients and coefficient 1
C                            is zero.

      subroutine mzsad1(icomp,iconst)
#include "building.h"
#include "control.h"

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/outin/iuout,iuin

      common/tc/itc,icnt

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/cctl/icascf(mcom)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/zoneqn/e(meq,mtr)

      common/zoneqs/es(meq,mtr),acaps
      common/coeadj/a1,a2,a3,a4

      character outs*124

C Set number of constructions in this component.
      nc=nconst(icomp)

C Surface term adjustment.
      e(iconst,iconst)=e(iconst,iconst)-a2

C Plant term adjustment.
      n=nc+2
      e(iconst,n)=e(iconst,n)-a3

C Present term adjustment.
      n=nc+3
      e(iconst,n)=e(iconst,n)-a4

C Next-to-surface term elimination.
      n=nc+4
      smalln=abs(e(iconst,n)-a1)
      b=.01
      if(abs(e(iconst,n)).gt.1.0e+6)b=.1
      if(smalln.gt.b)goto 1

      if(icascf(icomp).ge.1)then
         if(iban(icascf(icomp),1).eq.-2)then
           n1=nc+1
           n2=nc+2
           n3=nc+3
           n4=nc+4
           do 300 i=1,n1
           do 400 j=1,n4
           es(i,j)=e(i,j)
  400      continue
  300      continue
         endif
      endif

C Trace output ?
      if(itrace(28).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      write(outs,9995)icnt
 9995 format(' Subroutine mzsad1     trace output',I4)
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output adjusted coefficients.
      write(outs,'(A,F20.3)')' e(iconst,iconst)=',e(iconst,iconst)
      call edisp(itu,' ')
      call edisp(itu,outs)
      n=nc+2
      write(outs,'(A,F20.3)')' e(iconst,nc+2)  =',e(iconst,n)
      call edisp(itu,outs)
      n=nc+3
      write(outs,'(A,F20.3)')' e(iconst,nc+3)  =',e(iconst,n)
      call edisp(itu,outs)
      n=nc+4
      write(outs,'(A,F20.3)')' e(iconst,nc+4)  =',e(iconst,n)
      call edisp(itu,outs)

 9999 return
    1 write(outs,2)nsinc
    2 format(' mzsad1: numerical error after',I6,' time-steps.')
      call edisp(iuout,outs)
      write(outs,21)ihrp,idyp
   21 format(' (Hour',I3,' on year day',I4,').')
      call edisp(iuout,outs)
      write(outs,'(A,F20.6)')' e(iconst,n)  =',e(iconst,n)
      call edisp(iuout,outs)
      write(outs,'(A,F20.6)')' a1           =',a1
      call edisp(iuout,outs)
      call epwait
      stop

      end

C ******************** mzsad2

C Adjust the appropriate surface node equation
C coefficients in terms of the results of mzcnf2.
C That is:

C *                * * *  ) adjusted next-to `next-to surface node' cofts
C |
C X X X X X X X    X X     ) surface coefficients for a
C |                          6-sided zone
C |
C |
C algebraic summation of these two terms must equal zero.

C This results in:

C 1 X X X X X X    2 3 4  ) where 1,2,3 are adjusted coefficients,
C                           and coefficient 4 is introduced.

      subroutine mzsad2(icomp,iconst)

#include "building.h"
#include "control.h"

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/outin/iuout,iuin

      common/tc/itc,icnt
      common/cctl/icascf(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/zoneqn/e(meq,mtr)

      common/zoneqs/es(meq,mtr),acaps
      common/coeadj/a1,a2,a3,a4
      common/cadj2/b1,b2,b3,b4

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character outs*124

C Set number of constructions in this component,
      nc=nconst(icomp)
C Set number of construction nodes excluding internal
C surface node,     
      nel=NNDC(icomp,iconst)-1

C Determine associated control function for current zone,
      ic=icascf(icomp)
      if(ic.ne.0.or.ncf.ne.0)then
C Construction with control point,
        ictlcn=ibsn(ic,2)
C Determine position of control node within the construction,
      ipos=ibsn(ic,3)
      endif
      
C Use `a` values if not construction with control,
C or ctl point is ntis,
      if(iconst.ne.ictlcn.or.(iconst.eq.ictlcn.and.ipos.eq.nel))then

C Surface term adjustment.
          e(iconst,iconst)=e(iconst,iconst)-a2

C Plant term adjustment.
          n=nc+2
          e(iconst,n)=e(iconst,n)-a3

C Present term adjustment.
          n=nc+3
          e(iconst,n)=e(iconst,n)-a4
          
          if(iconst.eq.ictlcn.and.ipos.eq.nel)then
C Control term,
            n=nc+5
            e(iconst,n)=e(iconst,n)-a1
            
C Also, for this case, set intra-con ctl row terms;
C Inside surface term,
            e(nc+2,ictlcn)=a2
C Plant term, 
            e(nc+2,nc+2)=a3
C Present term,
            e(nc+2,nc+3)=a4
C Ctl point term,
            e(nc+2,nc+5)=a1

          endif
          
      else
C Use `a` and `b` values,
C Plant term adjustment,
          n=nc+2
          e(iconst,n)=e(iconst,n)-b3
         
C Present term adjustment,
          n=nc+3
          e(iconst,n)=e(iconst,n)-b4

C Control term,
          n=nc+5
          e(iconst,n)=e(iconst,n)-b1

C Set intra-con control row terms:
C Inside surface term,
         e(nc+2,ictlcn)=a2
C Plant term, 
         e(nc+2,nc+2)=a3
C Present term,
         e(nc+2,nc+3)=a4
C Ctl point term,
         e(nc+2,nc+5)=a1
      
      endif 

C Next-to-surface term elimination.

      n=nc+4
      if(iconst.ne.ictlcn)smalln=abs(e(iconst,n)-a1)
      if(iconst.eq.ictlcn.and.ipos.eq.nel)smalln=abs(e(iconst,n)-a1)
      if(iconst.eq.ictlcn.and.ipos.ne.nel)smalln=abs(e(iconst,n)-b2)
      b=.01
      if(abs(e(iconst,n)).gt.1.0e+6)b=.1      
      
      if(smalln.gt.b)goto 1
      
      if(iban(icomp,1).eq.-2)then
        n1=nc+1
        n2=nc+2
        n3=nc+3
        n4=nc+4
        n5=nc+5
        do 300 i=1,n2
        do 400 j=1,n5
        es(i,j)=e(i,j)
  400   continue
  300   continue
      endif

C Trace output ?
      if(itrace(28).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      write(outs,9995)icnt
 9995 format(' Subroutine mzsad2     trace output',I4)
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output adjusted coefficients.
      write(outs,'(A,F20.3)')' e(iconst,iconst)=',e(iconst,iconst)
      call edisp(itu,' ')
      call edisp(itu,outs)
      n=nc+2
      write(outs,'(A,F20.3)')' e(iconst,nc+2)  =',e(iconst,n)
      call edisp(itu,outs)
      n=nc+3
      write(outs,'(A,F20.3)')' e(iconst,nc+3)  =',e(iconst,n)
      call edisp(itu,outs)
      n=nc+4
      write(outs,'(A,F20.3)')' e(iconst,nc+4)  =',e(iconst,n)
      call edisp(itu,outs)
      n=nc+5
      write(outs,'(A,F20.3)')' e(iconst,nc+5)  =',e(iconst,n)
      call edisp(itu,outs)

 9999 return
    1 write(outs,2)nsinc
    2 format(' mzsad2: numerical error after',I6,' time-steps.')
      call edisp(iuout,outs)
      write(outs,21)ihrp,idyp
   21 format(' (Hour',I3,' on year day',I4,').')
      call edisp(iuout,outs)
      write(outs,'(A,F20.6)')' e(iconst,n)  =',e(iconst,n)
      call edisp(iuout,outs)
      write(outs,'(A,F20.6)')' a1           =',a1
      call edisp(iuout,outs)
      call epwait
      stop

      end
C ******************** mzcms1

C mzcms1 reduces the coefficients of the surface and air point
C node equations to the end of the forward reduction stage. The
C air and plant (carried through) coefficients are then passed
C to 'mzpst1' where the air/plant equation is solved depending
C on the simulation information previously defined.  'mzcms1'
C continues and solves for the surface node temperatures.

C For example, for a 6-sided zone:

C X X X X X X X             X X     -
C X X X X X X X             X X      |
C X X X X X X X             X X      | Zone surface
C X X X X X X X             X X      | node equations
C X X X X X X X             X X      |
C X X X X X X X             X X     -

C X X X X X X X             X X        Air point equation
C |         | |             | |
C  ---------  |             | |        Matrix e holds the X's
C  Surface    |             | |
C coefficients|             | |
C             |             | |
C             |             | |
C             Air point     Plant term
C                             |
C                             |
C                             RHS

C This is reduced to:

C + + + + + + +             + +
C   + + + + + +             + +
C     + + + + +             + +
C       + + + +             + +
C         + + +             + +
C           + +             + +
C             +             + +    This last equation in the
C             |             | |    reduction relates the zone
C             b1           b2 b3   air and plant capacity terms.

C The last equation is solved in 'mzpst1' and the back
C substitution is completed.

      subroutine mzcms1(icomp)
#include "building.h"

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/tc/itc,icnt

      common/zoneqn/e(meq,mtr)

      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/fvala/tfa(mcom),qfa(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      character outs*144

C N1 relates to the air node
C N2 relates to the plant term
C N3 relates to the present term

      nc=nconst(icomp)
      n1=nc+1
      n2=nc+2
      n3=nc+3

C Commence forward reduction.
      n=1
      do 10 i=1,nc
        n=n+1
        m=n-1
        do 20 j=n,n1
          x1=e(j,m)
          x2=e(i,m)
          x3=x1/x2
          do 30 k=m,n3
            sub=e(i,k)*x3
            e(j,k)=e(j,k)-sub
   30     continue
   20   continue
   10 continue

C Air temperature coefficient.
      b1=e(n1,n1)

C Plant coefficient.
      b2=e(n1,n2)

C Present term coefficient.
      b3=e(n1,n3)

C Trace output 1 ?
      if(itc.le.0.or.nsinc.lt.itc)goto 999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 999
      write(outs,'(A,I4)')' Subroutine mzcms1/1     Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

      call edisp(itu,' ')
      write(outs,'(A,F20.3)')' b1 = ',b1
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b2 = ',b2
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b3 = ',b3
      call edisp(itu,outs)

C Now solve air point/plant equation as a function
C of the user-imposed control strategy.
  999 call mzpst1(b1,b2,b3,icomp,qf)

C Now commence back-substitution operation.
      do 40 i=1,nc
      ii=n1-i
      sum=e(ii,n3)-e(ii,n2)*qf
      do 50 j=ii,nc
      l=n1+ii-j
      if(l.eq.n1)goto 1
      sum=sum-e(ii,l)*tfs(icomp,l)
      goto 2
    1 sum=sum-e(ii,l)*tfa(icomp)
    2 if(j.lt.nc)goto 50
      tfs(icomp,ii)=sum/e(ii,ii)
   50 continue
   40 continue

C Trace output 2 ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &  izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)')' Subroutine mzcms1/2     Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output computed temperatures.
      write(outs,9994)icomp
 9994 format(' tfs(icomp,1) to tfs(icomp,nc) for component',i3)
      call edisp(itu,' ')
      call edisp(itu,outs)

C Show first 24 surface.
      nss=min0(24,nc)
      write(outs,'(24F6.2)')(tfs(icomp,i),i=1,nss)
      call edisp(itu,outs)
      call edisp(itu,' ')

 9999 return
      end

C ******************** mzcms2

C Mzcms2 reduces the coefficients of the surface and air
C point node equations to the end of the forward reduction
C stage.   The appropriate control surface and plant
C (carried through) coefficients are then solved in
C mzpst1 depending on the simulation information
C defined previously. Mzcms2 continues and solves for
C the surface node and air temperatures.

C For example, for a 6-sided zone:

C X X X X X X X           X X    -
C X X X X X X X           X X     |
C X X X # X X X           X X     |  Zone surface node
C X X X X X X X           X X     |  equations.
C X X X X X X X           X X     |
C X X X X X X X           X X    -

C X X X X X X X           X X        Air point equation
C |         | |           | |
C  ---------  |           | |        Matrix e holds the X's
C Surface     |           | |
C coefficients|           | |
C             |           | |
C             |           | |
C             Air         Plant term
C             point         |
C                           |
C                           RHS

C Note that surface 4 is the control point (marked #)

C This scheme reduces to:

C + + + + + + +           + +
C   + + + + + +           + +
C     + + + + +           + +
C       @ + + +           + +
C       +   + +           + +
C       +     +           + +
C       +                 + +        This last equation in the
C       |                 | |        reduction relates the zone
C       |                 | |        surface temperature and plant
C       b1               b2 b3       capacity terms.

C The last equation is solved in 'mzpst1' and the back
C substitution completed.

      subroutine mzcms2(icomp)
#include "building.h"
#include "control.h"

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/tc/itc,icnt

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/zoneqn/e(meq,mtr)

      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/fvala/tfa(mcom),qfa(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/cctl/icascf(mcom)

      character outs*144
      dimension t(meq)

C Set control function associated with current zone.
      if(ncf.eq.0)return
      ic=icascf(icomp)

C Determine which surface is control point.
      is=ibsn(ic,2)

C n1 relates to the air node.
C n2 relates to the plant term.
C n3 relates to the present term.
      nc=nconst(icomp)
      n1=nc+1
      n2=nc+2
      n3=nc+3

C Commence forward reduction.
      m=0
      n=1

C Row-by-row .....
      do 10 i=1,nc
      n=n+1
      m=m+1

C ..... in terms of all subsequent rows .....
      do 20 j=n,n1

C ..... jumping when (and thereafter) the row relating to
C the surface control node is reached.
      if(i.ge.is)goto 1
      x1=e(j,m)
      x2=e(i,m)
      x3=x1/x2

C Term-by-term subtraction.
      do 30 k=m,n3
      sub=e(i,k)*x3
      e(j,k)=e(j,k)-sub
   30 continue
      goto 20
    1 x1=e(j,m+1)
      x2=e(i,m+1)
      x3=x1/x2

C Term-by-term subtraction (including each time
C the control node coefficient).
      do 40 k=m,n3
      if(k.eq.m)goto 2
      sub=e(i,k)*x3
      e(j,k)=e(j,k)-sub
      goto 40
    2 sub=e(i,is)*x3
      e(j,is)=e(j,is)-sub
   40 continue
   20 continue
   10 continue

C Now solve control point node/plant interaction
C node equation in terms of the user imposed control function.

C b1 = surface control node temperature coefficient.
C b2 = plant interaction node injection coefficient.
C b3 = present term coefficient.
      b1=e(n1,is)
      b2=e(n1,n2)
      b3=e(n1,n3)

C Trace output 1 ?
      if(itc.le.0.or.nsinc.lt.itc)goto 999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 999
      write(outs,'(A,I4)') ' Subroutine Mzcms2/1   Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

      call edisp(itu,' ')
      write(outs,'(A,F20.3)')' b1 = ',b1
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b2 = ',b2
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b3 = ',b3
      call edisp(itu,outs)

C Now solve air point/plant equation as a function
C of the user-imposed control strategy.
  999 call mzpst1(b1,b2,b3,icomp,qf)

C Commence back-substitution operation.
      t(is)=tfs(icomp,is)
      do 50 i=1,nc
      ii=n1-i
      sum=e(ii,n3)-e(ii,n2)*qf-e(ii,is)*tfs(icomp,is)
      k=ii+1
      do 60 j=k,n1
      l=n1+k-j
      if(l.le.is)l=l-1
      if(j.eq.n1)goto 3
      sum=sum-e(ii,l)*t(l)
      goto 60
    3 t(l)=sum/e(ii,l)
      if(i.eq.1)goto 4
      tfs(icomp,l)=t(l)
      goto 60
    4 tfa(icomp)=t(l)
   60 continue
   50 continue

C Trace output 2 ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)') ' Subroutine Mzcms2/2   Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output computed temperatures.
      write(outs,9994)icomp
 9994 format(' tfs(icomp,1) to tfs(icomp,nc) for component',i3)
      call edisp(itu,' ')
      call edisp(itu,outs)

C Show first 24 surface.
      nss=min0(24,nc)
      write(outs,'(24F6.2)')(tfs(icomp,i),i=1,nss)
      call edisp(itu,' ')
      call edisp(itu,outs)

 9999 return
      end

C ******************** mzcms3

C mzcms3 reduces the coefficients of the surface and air point
C node equations to the end of the forward reduction stage. The
C surface, air and plant (carried through) coefficients are then 
C passed to 'mzpst1' where the air/plant equation is solved 
C depending on the simulation information previously defined.  
C 'mzcms3' continues and solves for the surface node temperatures.

C For example, for a 6-sided zone:

C X X X X X X X             X X     -
C X X X X X X X X"          X X      |
C X X X X X X X             X X      | Zone surface
C X X X X X X X             X X      | node equations
C X X X X X X X             X X      |
C X X X X X X X             X X     -
C X X X X X X X             X X        Air point equation
C   X           X           X X        Ctl point equation

C |         | | |           | |
C  ---------  | |           | |        Matrix e holds the X's
C  Surface    | |           | |
C coefficients| |           | |
C             | |           | |
C             | |           | |
C             Air point     Plant term
C               |             |
C               |             |
C               Ctl term   RHS

C Note that the control point (marked #) is
C within construction 2.

C This is reduced to:

C + + + + + + +             + +
C   + + + + + + +           + +
C       + + + + +           + +
C         + + + +           + +
C           + + +           + +
C             + +           + +
C               +           + +    This last equation in the
C               |           | |    reduction relates the zone
C               b1         b2 b3   air and plant capacity terms.

C The last equation is solved in 'mzpst1' and the back
C substitution is completed.

C N1 relates to the air node
C N2 relates to the plant term
C N3 relates to the present term
C N5 relates to the control term

      subroutine mzcms3(icomp)
#include "building.h"
#include "control.h"
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/outin/iuout,iuin

      common/tc/itc,icnt

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/zoneqn/e(meq,mtr)

      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/fvala/tfa(mcom),qfa(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)


      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/cctl/icascf(mcom)

      character outs*144

C Set control function associated with current zone.
      if(ncf.eq.0)return
      ic=icascf(icomp)

C Determine the construction within which control is imposed,
      is=ibsn(ic,2) 
C Determine the control node,
      in=ibsn(ic,3)
      
C Set number of constructions in current component,
      nc=nconst(icomp)

C n1 relates to the air node.
C n2 relates to the plant term.
C n3 relates to the present term.
C n4 relates to the ntis term,
C n5 relates to the control term.
      n1=nc+1
      n2=nc+2
      n3=nc+3
      n4=nc+4
      n5=nc+5            

C Commence forward reduction:
      n=1
C Row-by-row .....
      do 10 i=1,n1
        n=n+1
        m=n-1
C ..... in terms of all subsequent rows .....
        do 20 j=n,n2
          x1=e(j,m)
          x2=e(i,m)
          x3=x1/x2
                    
C Term-by-term subtraction.
          do 30 k=m,n5
             sub=e(i,k)*x3
             e(j,k)=e(j,k)-sub
30        continue
20      continue
10    continue

C Ctl coefficient.
      b1=e(n2,n5)
C Plant coefficient.
      b2=e(n2,n2)
C Present term coefficient.
      b3=e(n2,n3) 

C Now solve air point/plant equation as a function
C of the user-imposed control strategy.
      call mzpst1(b1,b2,b3,icomp,qf)

C Now commence back-substitution operation.
         tfa(icomp)=(e(n1,n3)-e(n1,n2)*qf
     &                             -e(n1,n5)*tfc(icomp,is,in))/e(n1,n1)
         do 40 i=1,nc
           ii=n1-i
           sum=e(ii,n3)-e(ii,n2)*qf-e(ii,n5)*tfc(icomp,is,in)    
           do 50 j=ii,nc
              l=n1+ii-j
              if(l.eq.n1)goto 1
              sum=sum-e(ii,l)*tfs(icomp,l)               
              goto 2
1             sum=sum-e(ii,l)*tfa(icomp)
2             if(j.lt.nc)goto 50
              tfs(icomp,ii)=sum/e(ii,ii)
              
50         continue
40       continue  
     
      do 123 i=1,n2
          e(i,n5)=0.0
123   continue
      do 125 k=1,n5
          e(n2,k)=0.0
125   continue

C Trace output 2 ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)') ' Subroutine Mzcms3/2   Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output computed temperatures.
      write(outs,9994)icomp
 9994 format(' tfs(icomp,1) to tfs(icomp,nc) for component',i3)
      call edisp(itu,' ')
      call edisp(itu,outs)

C Show first 24 surface.
      nss=min0(24,nc)
      write(outs,'(24F6.2)')(tfs(icomp,i),i=1,nss)
      call edisp(itu,outs)
      call edisp(itu,' ')

 9999 return
      end
      
C ******************** mzcms4

C Mzcms4 reduces the coefficients of the surface and air point
C node equations to the end of the forward reduction stage.   The
C air and plant (carried through) coefficients are then passed
C to 'mzpst1' where the air/plant equation is solved to give the
C present time-row values of surface temperatures and then
C iterating until the desired future time row value is obtained.

C For example, for a 6-sided zone:

C X X X X X X X             X X     -
C X X X X X X X             X X      |
C X X X X X X X             X X      | Zone surface
C X X X X X X X             X X      | node equations
C X X X X X X X             X X      |
C X X X X X X X             X X     -

C X X X X X X X             X X        Air point equation
C |         | |             | |
C  ---------  |             | |        Matrix e holds the X's
C  Surface    |             | |
C coefficients|             | |
C             |             | |
C             |             | |
C             Air point     Plant term
C                             |
C                             |
C                             RHS

C This is reduced to:

C + + + + + + +             + +
C   + + + + + +             + +
C     + + + + +             + +
C       + + + +             + +
C         + + +             + +
C           + +             + +
C             +             + +    This last equation in the
C             |             | |    reduction relates the zone
C             b1           b2 b3   air and plant capacity terms.

C The last equation is solved in 'mzpst1' where the back
C substitution is completed by a call to 'mzback'.

      subroutine mzcms4(icomp)
#include "building.h"

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      common/tc/itc,icnt

      common/zoneqn/e(meq,mtr)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      character outs*144

C n1 relates to the air node.
C n2 relates to the plant term.
C n3 relates to the present term.

      nc=nconst(icomp)
      n1=nc+1
      n2=nc+2
      n3=nc+3

C Commence forward reduction.
      n=1
      do 10 i=1,nc
      n=n+1
      m=n-1
      do 20 j=n,n1
      x1=e(j,m)
      x2=e(i,m)
      x3=x1/x2
      do 30 k=m,n3
      sub=e(i,k)*x3
      e(j,k)=e(j,k)-sub
   30 continue
   20 continue
   10 continue

C Air temperature coefficient.
      b1=e(n1,n1)

C Plant coefficient.
      b2=e(n1,n2)

C Present term coefficient.
      b3=e(n1,n3)

C Trace output ?
      if(itc.le.0.or.nsinc.lt.itc)goto 999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 999
      write(outs,'(A,I4)')' Subroutine Mzcms4     Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

      call edisp(itu,' ')
      write(outs,'(A,F20.3)')' b1 = ',b1
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b2 = ',b2
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' b3 = ',b3
      call edisp(itu,outs)

C Now solve air point/plant equation for mixed zone temperature
C as a function of the user-imposed control strategy.
  999 call mzpst1(b1,b2,b3,icomp,qdum)
      return
      end

C ******************** mzcnb1

C Mzcnb1 implements back substitution operation on the
C construction matrices - relating to the non intra-
C construction control point case - and solves for the
C intra-nodal temperatures.

      subroutine mzcnb1(icomp,iconst)
#include "building.h"

      common/prec1/sna(mcom,ms)
      common/conadj/wa(ms,mn,6)

      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      n=nndc(icomp,iconst)-1
      ts=tfs(icomp,iconst)
      do 10 i=1,n
      jn=n-i+1
      tfc(icomp,iconst,jn)=(wa(iconst,jn,4)-wa(iconst,jn,3)*
     &qfc(icomp)/sna(icomp,iconst)-wa(iconst,jn,2)*ts)/(wa(iconst,jn,1))
      ts=tfc(icomp,iconst,jn)
   10 continue
      return
      end

C ******************** mzcnb2

C Mzcnb2 implements back substitution operation on the
C construction matrices - relating to the intra-construction
C control point case - and solves for the intra-nodal
C temperatures.

      subroutine mzcnb2(icomp,iconst)
      
#include "building.h"
#include "control.h"
      
      common/prec1/sna(mcom,ms)
      common/conadj/wa(ms,mn,6)

      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      
      common/cctl/icascf(mcom)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

C Set control function associated with current zone.
      if(ncf.eq.0)return
      ic=icascf(icomp)

C Determine the construction within which control is imposed,
      is=ibsn(ic,2) 
C Determine the control node,
      icn=ibsn(ic,3)
C Set number of construction nodes (excluding internal
C surface node), for the construction with control node,
      n=NNDC(icomp,iconst)-1
C Determine surface temperature,
      ts=tfs(icomp,iconst)
      
      do 10 i=1,n
       jn=n-i+1

      if(iconst.eq.is.and.jn.gt.icn)then
       tcn=tfc(icomp,is,icn)
       tfc(icomp,iconst,jn)=(wa(iconst,jn-1,4)-wa(iconst,jn-1,3)*
     &qfc(icomp)/sna(icomp,iconst)-wa(iconst,jn-1,6)*tcn)/
     &(wa(iconst,jn-1,2))
     
      elseif(iconst.eq.is.and.jn.lt.icn)then
       tcn=tfc(icomp,is,jn+1)
       tfc(icomp,iconst,jn)=(wa(iconst,jn,4)-wa(iconst,jn,3)*
     &qfc(icomp)/sna(icomp,iconst)-wa(iconst,jn,2)*tcn)/
     &(wa(iconst,jn,1))
          
      elseif(iconst.ne.is)then
      tfc(icomp,iconst,jn)=(wa(iconst,jn,4)-wa(iconst,jn,3)*
     &qfc(icomp)/sna(icomp,iconst)-wa(iconst,jn,2)*ts)/(wa(iconst,jn,1))
      ts=tfc(icomp,iconst,jn)

      endif
         
   10 continue  
      return
      end

C ******************** mzpst1

C Mzpst1 solves the air temperature/plant capacity
C (perhaps carried through) equation (b1*t+b2*q=b3) in
C terms of the control function information previously
C defined.

      subroutine mzpst1(b1,b2,b3,icomp,qf)
#include "building.h"
#include "control.h"

      common/tc/itc,icnt

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/outin/iuout,iuin
      common/pophelp/h(60)

      common/prec1/sna(mcom,ms)
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef

      common/fvala/tfa(mcom),qfa(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)

      common/cctl/icascf(mcom)

      common/pstflg/icflg1(mcf),icflg2(mcf)

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut
      common/tpmodx/iperx,idtypx

      common/setuq/qpltp(mcom),qpltf(mcom),conv(mcom)
      COMMON/MBINFO/ZMBI(MCOM,4)

      integer ncomp,ncon
      common/c1/ncomp,ncon

      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)
      common/glbctl/global,glbrwd
      common/sysctl/ipassr  

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,NIN

      character outs*144,h*72
      logical OK,dok,global,glbrwd

C Zeroise moisture addition to zone from plant -
C required in event of BCL06 not being referenced 
C at current time-step.      
      ZMBI(ICOMP,3)=0.

      bb1=b1
      bb2=b2
      bb3=b3
      iicomp=icomp
      tnp=b3/b1

      if(nsinc.gt.1)goto 1

C Initialise first pass through and assume no control
C irrespective of control data.
      do 10 i=1,mcf
      icflg1(i)=0
      icflg2(i)=0
   10 continue
      goto 2

C Test for control.
    1 icf=icascf(icomp)
      if(icf.eq.0)goto 2
      conv(icomp)=float(iban(icf,3))/100.
      iday=idyp
      if(ihrp.eq.24)iday=idyf

C If number of building control day types are 0 then
C same control on all day types
      n=nbcdt(icf)
      NIN=0
      if(n.eq.0)then
        NIN=-1*NBDAYTYPE
        n=nbdaytype
      endif
      do 20 i=1,n
      idtyp=i
      if(iday.le.ibcdv(icf,i,2).and.iday.ge.ibcdv(icf,i,1))goto 4
   20 continue

C Error!
      if(icflg1(icf).eq.1)goto 2
      write(outs,'(a,i3,a)') ' Control loop',icf,
     &  ' is being referenced outside its dates of validity.'
      call edisp(iuout,outs)
      icflg1(icf)=1
      dok=.true.
      h(1)='If you say yes, the zone(s) associated with this'
      h(2)='control loop will free-float for any days outwith'
      h(3)='the period of validity. If you say no the simulator'
      h(4)='will stop. '
      CALL ASKOK('Continue with no control if this condition occurs?',
     &           '(see help)',OK,dok,4)
      IF(.NOT.OK)then
        call epwait
        STOP
      endif

    2 tfa(icomp)=tnp
      qfa(icomp)=0.
      qfc(icomp)=0.
      qfs(icomp)=0.
      qf=0.
      goto 9999

C Commence control solution.
    4 if(nin.le.-1.or.n.lt.1)idtyp=icalender(idyp)
      if(btimef.gt.24.)then
        btimef=btimef-24.
        idtyp=icalender(idyp+1)
        if(idyp.eq.365)idtyp=icalender(1)
      endif
      n=nbcdp(icf,idtyp)
      do 30 i=1,n
      iper=i
      if(i.eq.n)goto 9
      if(btimef.le.tbcps(icf,idtyp,i+1))goto 11
      goto 30
    9 if(btimef.gt.tbcps(icf,idtyp,n))goto 11
   30 continue

C Error!
      if(icflg2(icf).eq.1)goto 2
      write(outs,'(a,i3,a)') ' Control loop',icf,
     &  ' is being referenced outside its dates of validity.'
      call edisp(iuout,outs)
      dok=.true.
      h(1)='If you say yes, the zone(s) associated with this'
      h(2)='control loop will free-float for any days outwith'
      h(3)='the period of validity. If you say no the simulator'
      h(4)='will stop. '
      CALL ASKOK('Continue with no control if this condition occurs?',
     &           '(see help)',OK,dok,4)
      icflg2(icf)=1
      IF(.NOT.OK)then
        call epwait
        STOP
      endif
      goto 2

C Set up day type and period for nested control loop.    
   11 if(ibsn(icf,4).eq.0)goto 12
      jcf=ibsn(icf,4)
      if(nbcdt(jcf).eq.0)ndt=nbdaytype
      do 71 i=1,ndt
      idtypx=i
      if(iday.le.ibcdv(icf,i,2).and.iday.ge.ibcdv(icf,i,1))goto 72
   71 continue

C Error: issue warning and set day type 1.
      write(outs,74)jcf
   74 format(' Warning: Control function',I3,' is being referenced')
      call edisp(iuout,outs)
      call edisp(iuout,' outside its dates of validity.')
      call edisp(iuout,' ')
      idtypx=1

C Set period.
   72 if(btimef.gt.24.)btimef=btimef-24.
      n=nbcdp(jcf,idtypx)
      do 75 i=1,n
      iperx=i
      if(i.eq.n)goto 76
      if(btimef.le.tbcps(jcf,idtypx,i+1))goto 12
      goto 75
   76 if(btimef.gt.tbcps(jcf,idtypx,n))goto 12
   75 continue

C Error: issue warning and set period 1.
      write(outs,77)jcf
   77 format(' Warning: Control function',I3,' is being referenced')
      call edisp(iuout,outs)
      call edisp(iuout,' outside its time of validity.')
      call edisp(iuout,' ')
      iperx=1

C Invoke appropriate control law.      
   12 if(global)goto 85
      call mzbctl(ier,icomp)

C << if error detected need to pass to parent subroutine >>
      if(ier.eq.2)then
        return
      endif
      goto 87

   85 if(ipassr.eq.1)then
         call mzbctl(ier,icomp)

C << if error detected need to pass to parent subroutine >>
         if(ier.eq.2)then
           return
         endif
         qfutgb(icf)=qfut
         tfutgb(icf)=tfut
         tnpgb(icf)=tnp            
      endif
      call mzgctl

C Assign t and q values.
   87 isn1=ibsn(icf,1)
      if(isn1.eq.0)isn1=icomp
      isn2=ibsn(icf,2)
      isn3=ibsn(icf,3)
      if(isn1.gt.0.and.isn2.eq.0)tfa(icomp)=tfut
      if(isn1.le.0)tfa(icomp)=tfut
	if(isn1.ne.icomp.and.isn1.gt.0.and.isn2.gt.0)tfa(icomp)=tfut
	if(isn1.eq.icomp.and.isn2.gt.0.and.isn3.eq.0)tfs(icomp,isn2)=tfut
	if(isn1.eq.icomp.and.isn2.gt.0.and.isn3.gt.0)tfc(icomp,isn2,isn3)=
     &tfut
      qf=qfut
      qfa(icomp)=0.
      qfs(icomp)=0.
      qfc(icomp)=0.
      qpltf(icomp)=0.
      ian1=iban(icf,1)
      if(ian1.eq.0)ian1=icomp
      ian2=iban(icf,2)
      ian3=iban(icf,3)
      if((ian1.eq.-3.and.ian2.eq.0).or.ian1.eq.-1.or.
     &(ian1.eq.-2.and.ian2.ne.icomp).or.
     &(ian1.gt.0.and.ian1.ne.icomp))goto 14
      if(ian1.eq.-3.and.ian2.eq.0)qfa(icomp)=qfut
      if(ian2.eq.0.and.ian1.eq.icomp)qfa(icomp)=qfut
      if(ian1.eq.icomp.and.ian2.gt.0.and.ian3.eq.0)qfs(icomp)=qfut*
     &sna(icomp,ian2)
      if(ian1.eq.icomp.and.ian2.gt.0.and.ian3.gt.0)qfc(icomp)=qfut*
     &sna(icomp,ian2)

C Mixed injection - 
      if(ian1.eq.-2.and.ian2.eq.icomp)then
        qpltf(icomp)=qfut
        qf=qfut*conv(icomp)
        if(isn1.eq.-2)qf=0.
      endif

 9999 if(itc.le.0.or.nsinc.lt.itc)goto 9998
      if(itrace(30).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9998
      write(outs,'(A,I4)')' Subroutine Mzpst1     Trace Output ',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

      write(outs,9996)icomp,icf
 9996 format(' Component',I4,4X,' Control function',I4)
      call edisp(itu,' ')
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' tfa =',tfa(icomp)
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' qfa =',qfa(icomp)
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' qfs =',qfs(icomp)
      call edisp(itu,outs)
      write(outs,'(A,F20.3)')' qfc =',qfc(icomp)
      call edisp(itu,outs)

 9998 return
   14 call edisp(iuout,' mzpst1: problem with flux assignments.')
      call epwait
      stop

      end

C ******************** mzback

C Mzback implements back substitution in a zone matrix
C in the case of "mixed" temperature control. If the
C zone also has a mixed actuator then the e array must
C be re-established to include the radiant and convective
C input.

      subroutine mzback(q)
#include "building.h"
#include "control.h"

      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/tc/itc,icnt

      common/zoneqn/e(meq,mtr)
      common/zoneqs/es(meq,mtr),acaps

      common/fvals/tfs(mcom,ms),qfs(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &npgap(mcom,ms,mgp)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      common/prec13/c(mcom,ms,mn,2),qc(mcom,ms,mn)
      COMMON/PREC14/emarea(MCOM)

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/btime/btimep,btimef
      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      COMMON/GR1D04/GAM,RGAM

      character outs*144

      icomp=iicomp

C n1 relates to the air node.
C n2 relates to the plant term.
C n3 relates to the present term.

      nc=nconst(icomp)
      n1=nc+1
      n2=nc+2
      n3=nc+3
      qq=q

C Test for radiant/convective split actuator.
      if(iban(icf,1).ne.-2)goto 11

C Actuator mixed: establish effect of radiant/convective split.
      qfut=qfut+q
      convs=real(iban(icf,3))/100.0
      q=qfut
      qq=0.

C At this stage the e array has been used in the forward
C reduction process and has been corrupted. Using array es
C to re-assign the e array values due account can be take
C of radiant plant and a new forward reduction conducted.
      do 100 i=1,n1
      do 110 j=1,n3+1
      e(i,j)=es(i,j)
  110 continue
  100 continue

C Calculate the plant radiative and convective component.
      do 10 i=1,nc
      nn=nndc(icomp,i)

C Re-assign e variable.
      qrad=qfut*(1.0-convs)*ei(icomp,i)/emarea(icomp)
      e(i,n3)=e(i,n3)+qc(icomp,i,nn)*qrad*GAM
   10 continue
      qcon=qfut*convs
      e(n1,n3)=e(n1,n3)+acaps*qcon*GAM

C Commence forward reduction.
      n=1
      do 20 i=1,nc
      n=n+1
      m=n-1
      do 30 j=n,n1
      x1=e(j,m)
      x2=e(i,m)
      x3=x1/x2
      do 40 k=m,n3
      sub=e(i,k)*x3
      e(j,k)=e(j,k)-sub
   40 continue
   30 continue
   20 continue

C Air temperature coefficient.
      bb1=e(n1,n1)

C Plant coefficient.
      bb2=e(n1,n2)

C Present term coefficient.
      bb3=e(n1,n3)

      tfut=bb3/bb1

C Conduct backward substitution.
   11 do 210 i=1,nc
      ii=n1-i
      sum=e(ii,n3)-e(ii,n2)*qq
      do 220 j=ii,nc
      l=n1+ii-j
      if(l.eq.n1)goto 201
      sum=sum-e(ii,l)*tfs(icomp,l)
      goto 202
  201 sum=sum-e(ii,l)*tfut
  202 if(j.lt.nc)goto 220
      tfs(icomp,ii)=sum/e(ii,ii)
  220 continue
  210 continue

C Trace output ?
      if(itc.le.0.or.nsinc.lt.itc)goto 9999
      if(itrace(29).eq.0.or.nsinc.gt.itcf.or.
     &   izntrc(icomp).ne.1)goto 9999
      write(outs,'(A,I4)')' Subroutine mzback     Trace output',icnt
      call edisp(itu,outs)
      icnt=icnt+1
      call dayclk(idyp,btimef,itu)

C Output computed temperatures.
      write(outs,9994)icomp
 9994 format(' tfs(icomp,1) to tfs(icomp,nc) for component',i3)
      call edisp(itu,' ')
      call edisp(itu,outs)

C Show first 24 surface.
      nss=min0(24,nc)
      write(outs,'(24F6.2)')(tfs(icomp,i),i=1,nss)
      call edisp(itu,outs)
      write(outs,'(A,F10.3)')' Air temperature =',tfut
      call edisp(itu,outs)
 9999 return
      end

C ****************************    MZGSUP    ***************************
C MZGSUP controls the simulation within the 3D ground start up period.
C *********************************************************************
      SUBROUTINE MZGSUP(ISS)
#include "building.h"
#include "model.h"
#include "tdf2.h"

      common/outin/iuout,iuin
      common/filep/ifil
      common/trc/itrc

      integer ncomp,ncon
      common/c1/ncomp,ncon

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/shad1/iml

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf

      common/btime/btimep,btimef

C For each zone a pointer to relevant temporal db entry 0=not used.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS),IVERSOL(MCOM,MS),IHTCLSETP(MCOM),
     &       IDBTZNM(MCOM)


C Common for 3D building model.
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)

      character outs*124

C Set simulation start and finish days: actual start
C day is prior to the requested start day (as determined
C in mztcon).  This is necessary to eliminate the effects
C of the assumed starting conditions.
      IGRNDS=ISS-ITCNSTG
      IF(IGRNDS.LT.1)THEN
        NUMYRS=INT(-IGRNDS/365)+1
        IGRNDS=IGRNDS+NUMYRS*365
      ENDIF

      iunit=ifil

c set `iml' to zero to ensure shading file is always
C read at start of simulation.
      iml=0
      nsinc=0
      btimef=1.
      I=IGRNDS-1
      DO 10 IDAY=1,ITCNSTG
        I=I+1

C Set day `i' (the present day) and day `i+1' (the
C future day) actual day numbers.
        idyp=i
        idyf=i+1
        if(idyp.gt.365)THEN
          idyp=idyp-365
          idyf=idyp+1
        ENDIF
        if(idyf.eq.366)idyf=1

C What day of week is present and future day: Monday 1
C through Sunday 7.
        call edayr(idyp,id,im)
        call eweekd(id,im,iyear,idwp)
        call edayr(idyf,id,im)
        call eweekd(id,im,iyear,idwf)

C Establish climatic data for present day and first
C hour of future day: future day first.
        irec=idyf
        read(iunit,rec=irec,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)
        idiff=idif(1)
        itmpf=itmp(1)
        idnrf=idnr(1)
        ivelf=ivel(1)
        idirf=idir(1)
        ihumf=ihum(1)

C Present day.
        irec=idyp
        read(iunit,rec=irec,err=1000)
     &    (idif(j),itmp(j),idnr(j),ivel(j),idir(j),ihum(j),j=1,24)

C Establish shading/insolation information for present day
C only: hour 1 of future day is assumed equal to hour 1 of
C present day even if present and future days are within
C different months.
        call mzshdo(idyp)

C Continue simulation on an hour-by-hour basis.
        DO 20 J=1,24
          NSINC=NSINC+1

C Set present and future hours.
          ihrp=j
          ihrf=j+1
          if(ihrf.eq.25)ihrf=1

C Set the future time-row hour value.
          btimep=btimef
          btimef=float(ihrp)+float(its)/float(ntstep)
          if(btimef.gt.24.0)btimef=btimef-24.

C Determine climatic parameters at the present and future time row. 
          call mzclmpt(1)

C Determine for each component and surface, the defining
C index and the adjacent space temperature and radiation
C at the present and future time-rows.  Also set up plant
C input flux to zones due to plant component containment
C losses already established by the coefficient generators.
          call mzadjc

C Compute total external and internal window and door areas
C and transfer to results library; then initiate start-up
C nodal temperature assignments (at first time-step only).
          if(IDAY.EQ.1.AND.J.EQ.1)then
            do 7 l=1,ncomp
              icomp=l
              call mzwdar(icomp)
              call mznasg(icomp)
    7       continue
          endif

C And compute all inside and 'outside' convection coefficients.
          call mzconv

C Consider each zone in turn.
          DO 40 L=1,NCOMP
            ICOMP=L

C Set the thermal coupling terms.
            CALL MZITCF(ICOMP)

C Set the heat variables at the current time step "N" equal to 
C the future values "F".
            CALL SHTNEF(ICOMP)

C Compute all time-dependent heat injections and complete
C coefficient set-up for current zone.
            CALL MZCOE3(ICOMP)

C Call matrix handling routines depending on the location
C of the control point node.
            CALL MTXCTL(ICOMP)

C Set the present temperature and plant injection values equal to the 
C future values.
            CALL MZLS5(ICOMP)
   40     CONTINUE

C Perform ground simulation for the current time step.

C Update the temperature array.
          CALL UD1TMP(-1)
          CALL MZ3SU1(-1)
          CALL UD2TMP
          CALL FORW3D(-1,ISNSR,B1,B2,B3)
          TNP=B3/B1
          CALL SVQTMP(-1,ISNSR,TNP,0.)
          CALL BACK3D
          CALL FILL1G
   20   CONTINUE

C Day loop now complete.
   10 CONTINUE
      RETURN

 1000 write(outs,'(A,I3)') ' MZGSUP: climate file error @ rec ',irec
      call edisp(iuout,outs)
      CALL EPWAIT
      RETURN
      END

C **************************    FILL1G    *****************************
C FILL1G fills up the 1D ground temperature array.
C *********************************************************************
      SUBROUTINE FILL1G
#include "building.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/GR3D110/ICVS,ICNC,INDC,INDD,ITAQ,ITLW,ILWV,ITF3
      COMMON/GRND101/NNODTG,ITCNSTG,TEMP3G(MCOM),AREAGD(MCOM)
      COMMON/GR3D132/T3F(MNOD3)

      DO 10 ICOMP=1,NCOMP
        TEMP3G(ICOMP)=0.
   10 CONTINUE
      IC=0
   20 IC=IC+1
      READ(INDD,REC=IC,IOSTAT=ISTAT)ND1,AREC,IZON

C End of file reached.
      IF(ISTAT.LT.0)THEN
        DO 30 ICOMP=1,NCOMP
          IF(AREAGD(ICOMP).LT.1.E-8)THEN
            TEMP3G(ICOMP)=0.
          ELSE
            TEMP3G(ICOMP)=TEMP3G(ICOMP)/AREAGD(ICOMP)
          ENDIF
   30   CONTINUE
c          write(*,'(15F6.1)')(TEMP3G(J),J=1,NCOMP)
        RETURN
      ELSE
        TEMP3G(IZON)=TEMP3G(IZON)+T3F(ND1)*AREC
        GOTO 20
      ENDIF
      END
