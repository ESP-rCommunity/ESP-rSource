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

C      mzini1
C      mzini2
C      mznasg

C ******************** mzini1

C Mzini1 initialises climatic data and future air
C temperature variable.

      subroutine mzini1
#include "building.h"

      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/climi/qfp,qff,tp,tf,qdp,qdf,vp,vf,dp,df,hp,hf
      common/fvala/tfa(mcom),qfa(mcom)

      qff=0.
      tf=0.
      qdf=0.
      vf=0.
      df=0.
      hf=0.
      do 10 i=1,ncomp
      tfa(i)=0.
   10 continue
      return
      end

C ******************** mzini2

C Mzini2 initialises the nodal energy injection terms.

      subroutine mzini2(icomp)

#include "building.h"

      common/coe31s/hrs(mcom,ms,ms),zhrs(mcom,ms)
      common/coe32s/qslif(mcom,ms),qslef(mcom,ms),qtmcaf(mcom,ms,mn)
      common/coe35s/zgef(mcom),zgif(mcom),x1(mcom),zgimf(mcom)
      common/coe39s/cif(mcom),cvf(mcom),x3(mcom),cvm(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &npgap(mcom,ms,mgp)

C Special for blc09.
      common/bcl9/ihpzon,ihpcon,ihpndo,qnopf,qhpsv

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      qnopf=0.

      nc=nconst(icomp)
      zgef(icomp)=0.
      zgif(icomp)=0.
      x1(icomp)=0.
      cif(icomp)=0.
      cvf(icomp)=0.
      x3(icomp)=0.
      do 10 i=1,nc
        zhrs(icomp,i)=0.
        qslif(icomp,i)=0.
        qslef(icomp,i)=0.
        do 20 j=1,nc
          hrs(icomp,i,j)=0.
   20   continue
   10 continue
      do 50 i=1,mtmc
        node=NNDC(icomp,i)
        do 60 j=1,node
          qtmcaf(icomp,i,j)=0.0
   60   continue
   50 continue
      return
      end

C ******************** mznasg

C Mznasg assigns the starting values of zone temperature and energy
C injections to each node in the discretisation scheme.

      subroutine mznasg(icomp)

#include "building.h"
#include "model.h"

      common/pvalc/tpc(mcom,ms,mn),qpc(mcom)
      common/pvals/tps(mcom,ms),qps(mcom)
      common/pvala/tpa(mcom),qpa(mcom)
      common/pvalg/gpa(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/fvala/tfa(mcom),qfa(mcom)
      common/fvalg/gfa(mcom)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &npgap(mcom,ms,mgp)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)
      COMMON/ADJCS/ATFS(MCOM,MS),ARFS(MCOM,MS)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

C Zone casual gain control data.
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

C Total previous time increment zone electrical power consumption.
      COMMON/ENETZONP/ZONPPP(MCOM),ZONQPP(MCOM)
      
      COMMON/SOFDT/IDTM(MCOM,MLCOM)
      COMMON/Vld30/Tinitial

C First set all present time values of time dependent heat injections
C and related coefficients to zero.
      call mzini2(icomp)

C Set start values of all nodal temperatures and plant energy
C injection/extraction.
      nc=nconst(icomp)
      do 10 i=1,nc
        ATP(icomp,i)=Tinitial
        ATF(icomp,i)=Tinitial
        ATFS(icomp,i)=Tinitial
        ARP(icomp,i)=0.
        ARF(icomp,i)=0.
        ARFS(icomp,i)=0.
        nn=nndc(icomp,i)-1
        do 20 j=1,nn
          tpc(icomp,i,j)=Tinitial
          tfc(icomp,i,j)=Tinitial
   20   continue
        tps(icomp,i)=Tinitial
        tfs(icomp,i)=Tinitial
   10 continue
      tpa(icomp)=Tinitial
      tfa(icomp)=Tinitial
      qfc(icomp)=0.
      qfs(icomp)=0.
      qfa(icomp)=0.
      gpa(icomp)=0.003
      gfa(icomp)=0.003

C Set initial values to the lighting switch off delay memory:
      IF(ICGC(icomp).EQ.1) THEN
        DO 40 N=1,NLITZ(ICOMP)
          IDTM(ICOMP,N)=0
40      CONTINUE
      ENDIF

C Set the previous time step zone-side power consumption totals initial values.  
      ZONPPP(ICOMP)=0.0
      ZONQPP(ICOMP)=0.0

      return
      end
