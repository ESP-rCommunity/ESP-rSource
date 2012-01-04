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

C This file contains routines to (1) Read in the CFD domain configuration file 
C and (2) calculate static, derived data from this file.
C Contains:
C  CFDDTA - reads CFD configuration file.
C  IGETCN - Identify mass flow connection.
C  IGETND - Identify mass flow network node associated with a CFD node.
C  IDSURF - Identify building surface number.
C  INICNT - Initialize simulation constant parameters.
C  CFDSBREAD - Reads in data and sets variables describing CFD solid boundaries.
C  SETGEO - sets the geometry for the CFD domain in terms of ESP-r's standard
C           geometry model (needed for graphic feedback scaling).

C ********************* CFDDTA *********************
C CFDDTA - Reads the CFD configuration file.
      SUBROUTINE CFDDTA(itrc,itu,ICOMP,IBLD,IMFN,IER)
#include "cfd.h"
#include "net_flow.h"
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

C If IBLD=1 there is thermal conflation and mapping between
C           dfs and bps surfaces must be provided.
C If IMFN=1 there is air-flow conflation and mapping between
C           dfs and bps connections must be provided.

C Grid and Geometry definition:
C   NREG(?,ICFD) - Number of regions in each direction(?: 1- x, 2- y, 3- z).
C   NCELX(IREG,ICFD) - Number of cells in x-direction per region(IREG) per
C                      domain(ICFD).
C   NCELY(IREG,ICFD) - Number of cells in y-direction per region per domain(ICFD).
C   NCELZ(IREG,ICFD) - Number of cells in z-direction per region per domain(ICFD.
C   NCELZe(IREG,ICFD) - Number of cells in z (east)-direction.

C   XREG(IREG,ICFD) - x-dimension of the region IREG per domain (ICFD).
C   YREG(IREG,ICFD) - y-dimension of the region IREG per domain (ICFD).
C   ZREG(IREG,ICFD) - z-dimension of the region IREG per domain (ICFD).
C   ZREGe(IREG,ICFD) - z (east) -dimension of the region IREG per domain (ICFD).

C   Xplaw(IREG,ICFD) - x-power law coefficient of region IREG per domain (ICFD).
C   Yplaw(IREG,ICFD) - y-power law coefficient of region IREG per domain (ICFD). 
C   Zplaw(IREG,ICFD) - West wall z-power law coef of region IREG per domain (ICFD).
C   Zplawe(IREG,ICFD) - East wall z-power law coef of region IREG per domain (ICFD).
C      (used in the case of curvilinear Z)
      COMMON/FILEP/IFIL

      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
C      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
C     &              XREG(MNREG,MNZ),YREG(MNREG,MNZ),ZREG(MNREG,MNZ),
C     &              Xplaw(MNREG,MNZ),Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),
C     &              Zplawe(MNREG,MNZ),NREG(3,MNZ)
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),ICNACT(MCFND,MNZ)
      COMMON/HSrc/NHS(2,MNZ),IHSi(MNHS,2,MNZ),IHSf(MNHS,2,MNZ),
     &            JHSi(MNHS,2,MNZ),JHSf(MNHS,2,MNZ),KHSi(MNHS,2,MNZ),
     &            KHSf(MNHS,2,MNZ),HSID(MNHS,2,MNZ),SHS(MNHS,2,MNZ),
     &            BHS(MNHS,2,MNZ)
      COMMON/FDTRFC/FLSDTU(MNZ),FLSDTV(MNZ),FLSDTW(MNZ),FLSDTP(MNZ),
     &              FLSDTT(MNZ),FLSDTK(MNZ),FLSDTE(MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ)
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/PRSREF/IPRESF(MNZ),JPRESF(MNZ),KPRESF(MNZ)
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     1             IPPHI(MNZ),SRMAX(MNZ)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/cfdini/INITFL(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONST/GREAT,small,GRAV
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW7/LVALCM(MCMV)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFS/IMFACT,NOSTOP,MITMFS
      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/NSSWP/NSSWPU(MNZ),NSSWPV(MNZ),NSSWPW(MNZ),NSSWPP(MNZ),
     &             NSSWPT(MNZ),NSSWPK(MNZ),NSSWPE(MNZ)
      COMMON/RENORM/LRENORM
      COMMON/THETA/THETA1(MCFND,MNZ),THETA2(MCFND,MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR
      COMMON/ZTURB/rMOOT,nZtoKE
      common/varIC/NINIT,uic,vic,wic,pic,kic,eic,tic
      common/relax/nrfac,urlx,vrlx,wrlx,prlx,krlx,erlx,trlx
      common/eqnslv/solveqn
      common/grdmax/NTCX,NTCY,NTCZ

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Boundary condition commons.
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

C Flow visualization common blocks.
      COMMON/VISFIL/LVEC,LBIT
      common/flvpar/Lflpar(MCOM)

      LOGICAL LRENORM,LGRILL(MNZ),BHS
      LOGICAL INITFL,CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL PRSTYP,CALLK,CALLE
      logical uic,vic,wic,pic,kic,eic,tic
      logical urlx,vrlx,wrlx,prlx,krlx,erlx,trlx
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho

      character VOLNAME*12, VCsurf*12
      CHARACTER VAR*2
      Character*1 THSRC,RELFAC,OPNTYP
      Character*124 OUTSTR,WORD,WORD1,WORD2
      CHARACTER*72 TITLE,CFTRFL,HSID,LPHI,LCFD,Lflpar
      CHARACTER LVALCM*60,LTPCMP*60,CMNAM*12,NDNAM*12,outs*124
      CHARACTER*72 LVEC,LBIT
      character solveqn*80
      LOGICAL BUOYA,BOUSSA

      IFL=IFIL+1

C Check whether the file is a CFD-input file.      
      CALL STRIPC(IFL,OUTSTR,99,ND,1,'dfd line 1',IER)
      if(OUTSTR(1:15).eq.'DFS DESCRIPTION')then

C Note, the conflation type is not read at this point, it has
C already been scanned when the configuration file was read
C because the type of conflation determines how the data is read.
        CALL STRIPC(IFL,OUTSTR,99,ND,1,'dfd confla type',IER)
      else
        call usrmsg('Referenced file not DFD description',' ','W')
        return
      endif

C Reset gridding logicals.
      origin=.FALSE.
      xgrid=.FALSE.
      ygrid=.FALSE.
      zgrid=.FALSE.
      zegrid=.FALSE.
      ortho=.FALSE.

C Read in title.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,' Simulation title',IER)
      IF(IER.NE.0)goto 1000
      TITLE(ICFD)=OUTSTR(1:72)
      if(itrc.gt.0)then
        call edisp(itu,' ')
        if(ICOMP.gt.0)then
          write(outs,'(a,a)')'A CFD domain has been described for: ',
     &      zname(ICOMP)
          call edisp(itu,outs)
        else
          call edisp(itu,'A CFD domain has been included in the model')
        endif
        write(outs,'(a,a)')'Description is: ',OUTSTR(1:lnblnk(OUTSTR))
        call edisp(itu,outs)
      endif

C Read in gridding. If 4 items then curvilinear Z has been defined.
      CALL STRIPC(IFL,OUTSTR,99,ND,1,'Number of grid regions',IER)
      IF(IER.NE.0)goto 1000
      if(ND.eq.3)then
        ortho=.true.
        irloop=3
        if(itrc.gt.0)call edisp(itu,'It is an orthogonal domain with:')
      elseif(ND.eq.4)then
        ortho=.false.
        irloop=4
        if(itrc.gt.0)call edisp(itu,'It is a curvilinier domain with:')
      endif
      if(ND.eq.3.or.ND.eq.4)then
        K=0
        Do 10 i=1,irloop
          CALL EGETWI(OUTSTR,K,IVAL,0,MNREG,'F','Number of regions',IER)
          IF(IER.NE.0)then
            call edisp(IUOUT,' Cannot convert number of regions.')
            goto 999
          endif
          NREG(i,ICFD)=IVAL
          if(itrc.gt.0)then
            if(i.eq.1)write(outs,'(a,i2)') 'Number of X regions:',IVAL
            if(i.eq.2)write(outs,'(a,i2)') 'Number of Y regions:',IVAL
            if(i.eq.3)write(outs,'(a,i2)') 'Number of Zw regions:',IVAL
            if(i.eq.4)write(outs,'(a,i2)') 'Number of Ze regions:',IVAL
            call edisp(itu,outs)
          endif
   10   continue
      endif

C Reset gridding variables to default values.
      NTCX=0
      NTCY=0
      NTCZ=0
      NCELX(1,ICFD)=1
      NCELY(1,ICFD)=1
      NCELZ(1,ICFD)=1
      NCELZe(1,ICFD)=1
      XREG(1,ICFD)=1.0
      YREG(1,ICFD)=1.0
      ZREG(1,ICFD)=1.0
      ZREGe(1,ICFD)=1.0

C Read in gridding data.
      Do 30 i=1,irloop
        Do 20 j=1,NREG(I,ICFD)
          CALL STRIPC(IFL,OUTSTR,4,ND,1,' Grid definition.',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W',' axy',IER)
          IF(IER.NE.0)then
            call edisp(IUOUT,' Cannot convert axy')
            goto 999
          endif
          CALL EGETWI(OUTSTR,K,IVAL,-1000,1000,'F','No. of cells.',IER)
          IF(IER.NE.0)then
            call edisp(IUOUT,' Matrices must be re-dimensioned.')
            goto 999
          endif
          IF(WORD(1:2).EQ.'X '.OR.WORD(1:2).EQ.'x ')NCELX(J,ICFD)=IVAL
          IF(WORD(1:2).EQ.'Y '.OR.WORD(1:2).EQ.'y ')NCELY(J,ICFD)=IVAL
          IF(WORD(1:2).EQ.'Zw'.OR.WORD(1:2).EQ.'zw'.or.
     &       WORD(1:2).EQ.'Z '.OR.WORD(1:2).EQ.'z ')NCELZ(J,ICFD)=IVAL
          IF(WORD(1:2).EQ.'Ze'.OR.WORD(1:2).EQ.'ze')NCELZE(J,ICFD)=IVAL
          CALL EGETWR(OUTSTR,K,VAL,0.0001,1000.0,'F',
     &      ' Size of the region.',IER)
          IF(IER.NE.0)then
            call edisp(IUOUT,' Size of the region out of range.')
            goto 999
          endif
          if(WORD(1:2).EQ.'X ')XREG(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Y ')YREG(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Zw'.OR.WORD(1:2).EQ.'zw'.or.
     &       WORD(1:2).EQ.'Z '.OR.WORD(1:2).EQ.'z ')ZREG(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Ze')ZREGE(J,ICFD)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Power law coef',IER)
          IF(IER.NE.0)then
            call edisp(IUOUT,' Grid definiton. Power law coefficient.')
            goto 999
          endif
          if(WORD(1:2).EQ.'X ')Xplaw(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Y ')Yplaw(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Zw'.OR.WORD(1:2).EQ.'zw'.or.
     &       WORD(1:2).EQ.'Z '.OR.WORD(1:2).EQ.'z ')Zplaw(J,ICFD)=VAL
          if(WORD(1:2).EQ.'Ze')Zplawe(J,ICFD)=VAL
          if(itrc.gt.0)then
            if(WORD(1:2).EQ.'X ')then
              write(outs,'(a,i2,a,i3,a,F7.3,a,F6.3)') 'X Region ',J,
     &          ' cells:',NCELX(J,ICFD),' size:',XREG(J,ICFD),' plaw:',
     &           Xplaw(J,ICFD)
              xgrid=.TRUE.
            elseif(WORD(1:2).EQ.'Y ')then
              write(outs,'(a,i2,a,i3,a,F7.3,a,F6.3)') 'Y Region ',J,
     &          ' cells:',NCELY(J,ICFD),' size:',YREG(J,ICFD),' plaw:',
     &           Yplaw(J,ICFD)
              ygrid=.TRUE.
            elseif(WORD(1:2).EQ.'Zw'.or.WORD(1:2).EQ.'Z ')then
              write(outs,'(a,i2,a,i3,a,F7.3,a,F6.3)') 'Z Region ',J,
     &          ' cells:',NCELZ(J,ICFD),' size:',ZREG(J,ICFD),' plaw:',
     &           Zplaw(J,ICFD)
              zgrid=.TRUE.
            elseif(WORD(1:2).EQ.'Ze')then
              write(outs,'(a,i2,a,i3,a,F7.3,a,F6.3)')'Z-east Region ',
     &           J,' cells:',NCELZe(J,ICFD),' size:',ZREGe(J,ICFD),
     &           ' plaw:',Zplawe(J,ICFD)
              zegrid=.TRUE.
            endif
            call edisp(itu,outs)
          endif
   20   continue
   30 continue

C If one dimension does not exist make NREG=1
      IF(NREG(1,ICFD).EQ.0) NREG(1,ICFD)=1
      IF(NREG(2,ICFD).EQ.0) NREG(2,ICFD)=1
      IF(NREG(3,ICFD).EQ.0) NREG(3,ICFD)=1
      if(.NOT.ortho)then
        IF(NREG(4,ICFD).EQ.0) NREG(4,ICFD)=1
      endif

C Total number of cell in each direction.
      do 210 j=1,NREG(1,ICFD)
        NTCX=NTCX+ABS(NCELX(J,ICFD))
  210 continue
      do 211 j=1,NREG(2,ICFD)
        NTCY=NTCY+ABS(NCELY(J,ICFD))
  211 continue
      do 212 j=1,NREG(3,ICFD)
        NTCZ=NTCZ+ABS(NCELZ(J,ICFD))
  212 continue
      if(itrc.gt.0)then
        write(outs,'(a,3i3)')'Total cells in X,Y,Z directions: ',NTCX,
     &    NTCY,NTCZ
        call edisp(itu,outs)
      endif

C Note: ntcze is a local variable representing total number of East cells.
      ntcze=0
      if(.NOT.ortho)then
        do 213 j=1,NREG(4,ICFD)
          ntcze=ntcze+ABS(NCELZE(J,ICFD))
  213   continue
        if(ntcze.ne.NTCZ)then
          call usrmsg('Total number of cells on East and West',
     &      'face differ. Not allowed in curviliniear!','W')
        endif
      endif

      if(NTCX.GT.NTCELX)then
        write(outs,'(a,i3)') ' Current limit is ',NTCELX
        call usrmsg(' To many cells in x direction.',outs,'W')
        ier=1
        return
      endif
      if(NTCY.GT.NTCELY)then
        write(outs,'(a,i3)') ' Current limit is ',NTCELY
        call usrmsg(' To many cells in y direction.',outs,'W')
        ier=1
        return
      endif
      if(NTCZ.GT.NTCELZ)then
        write(outs,'(a,i3)') ' Current limit is ',NTCELZ
        call usrmsg(' To many cells in z direction.',outs,'W')
        ier=1
        return
      endif

C Read in `equations to be solved'.
      CALLU(ICFD)=.FALSE.
      CALLV(ICFD)=.FALSE.
      CALLW(ICFD)=.FALSE.
      CALLT(ICFD)=.FALSE.
      CALLC(ICFD)=.FALSE.
      CALLK=.FALSE.
      CALLE=.FALSE.
      BUOY(ICFD)=.FALSE.
      BOUSSI(ICFD)=.FALSE.
      ZEROT(ICFD)=.FALSE.
      ZandKE(ICFD)=.FALSE.
      LGRILL(ICFD)=.FALSE.
      MITzero(ICFD)=.FALSE.
      IBUOY(ICFD)=0

      CALL STRIPC(IFL,OUTSTR,99,ND,1,' Variables to be solved. ',IER)

C Save entire line for echoing back to CFD input file in the case
C that the user does not respecify the equations to be solved in
C the interface.
      lsn1=MIN0(lnblnk(OUTSTR),80)
      solveqn=OUTSTR(1:lsn1)
      if(itrc.gt.0)then
        write(outs,'(a,i2,a)')'There are ',ND,' variables to be solved:'
        call edisp(itu,outs)
      endif
      K=0
      DO 300 I=1,ND
        CALL EGETW(OUTSTR,K,WORD,'W',' variable  name. ',IER)
        IF(WORD(1:1).EQ.'U'.OR.WORD(1:1).EQ.'u')THEN
          CALLU(ICFD)=.TRUE.
          IF(NTCX.le.1)THEN
            CALL EDISP(IUOUT,
     &      ' U is to be solved but grid not defined in x direction.')
            GOTO 999
          ENDIF
          if(itrc.gt.0)call edisp(itu,' U: X-direction velocity')
        ELSEIF(WORD(1:1).EQ.'V'.OR.WORD(1:1).EQ.'v')THEN
          CALLV(ICFD)=.TRUE.
          IF(NTCY.le.1)THEN
            CALL EDISP(IUOUT,
     &      ' V is to be solved but grid not defined in y direction.')
            GOTO 999
          ENDIF
          if(itrc.gt.0)call edisp(itu,' V: Y-direction velocity')
        ELSEIF(WORD(1:1).EQ.'W'.OR.WORD(1:1).EQ.'w')THEN
          CALLW(ICFD)=.TRUE.
          IF(NTCZ.le.1)THEN
            CALL EDISP(IUOUT,
     &      ' W is to be solved but grid not defined in w direction.')
            GOTO 999
          ENDIF
          if(itrc.gt.0)call edisp(itu,' W: Z-direction velocity')
        ELSEIF(WORD(1:1).EQ.'T'.OR.WORD(1:1).EQ.'t')THEN
          CALLT(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,' T: Temperature')
        ELSEIF(WORD(1:1).EQ.'C'.OR.WORD(1:1).EQ.'c')THEN
          CALLC(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,' C: species concentration')
        ELSEIF(WORD(1:1).EQ.'K'.OR.WORD(1:1).EQ.'k')THEN
          CALLK=.TRUE.
          if(itrc.gt.0)call edisp(itu,' K: turbulent kinetic energy')
        ELSEIF(WORD(1:1).EQ.'E'.OR.WORD(1:1).EQ.'e')THEN
          CALLE=.TRUE.
          if(itrc.gt.0)call edisp(itu,
     &       ' E: dispersion of turbulent kinetic energy')
        ELSEIF(WORD(1:1).EQ.'B'.OR.WORD(1:1).EQ.'b')THEN
          IBUOY(ICFD)=1
          BUOY(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,' B: include buoyancy effects')
        ELSEIF(WORD(1:1).EQ.'Q'.OR.WORD(1:1).EQ.'q')THEN

C Boussinesq approximation for buoyancy.
          IBUOY(ICFD)=2
          BOUSSI(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,
     &      ' Q: apply Boussinesq approximation for buoyancy')
        ELSEIF(WORD(1:1).EQ.'Z'.OR.WORD(1:1).EQ.'z')THEN

C Zero-equation fixed-eddy-viscosity turbulence `model'.
          ZEROT(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,
     &      ' Z: apply constant and uniform eddy viscosity 0-eqn')
        ELSEIF(WORD(1:1).EQ.'G'.OR.WORD(1:1).EQ.'g')THEN

C Flow at inlet not perpendicular to wall containing inlet.
          LGRILL(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,
     &     ' G: Inlet flow not perpendicular to wall containing inlet')
        ELSEIF(WORD(1:1).EQ.'M'.OR.WORD(1:1).EQ.'m')THEN

C MIT zero-equation model.
          MITzero(ICFD)=.TRUE.
          if(itrc.gt.0)call edisp(itu,' M: use MIT zero-equation model')
          if(IFCFD(ICP).eq.0) goto 1001
        ELSE
          write(outs,'(3a)') ' Variable ',WORD(1:lnblnk(WORD)),
     &      ' not identified.'
          call edisp(itu,outs)
        ENDIF
 300  CONTINUE

C Determine whether the k-epsilon turbulence model is active.
      ITURB(ICFD)=0
      KEMDL(ICFD)=.FALSE.
      IF(CALLK.AND.CALLE)THEN
        KEMDL(ICFD)=.TRUE.
        ITURB(ICFD)=1
      ELSEIF(CALLK.AND.(.NOT.CALLE))THEN
        CALL EDISP(itu,' One turbulence model equation not supported.')
        CALL EDISP(itu,' The default k-epsilon model is assumed.')
        KEMDL(ICFD)=.TRUE.
        ITURB(ICFD)=1
      ELSEIF(.NOT.CALLK.AND.CALLE)THEN
        CALL EDISP(itu,' Both k and epsilon equations must be solved.')
        CALL EDISP(itu,' The default k-epsilon model is assumed.')
        KEMDL(ICFD)=.TRUE.
        ITURB(ICFD)=1
      ENDIF

C If the zero-equation fixed-eddy-viscosity turbulence model
C is specified, then get the value of the eddy-viscosity.  This is specified in
C terms of a multiple of the molecular viscosity.
      if(ZEROT(ICFD))then
        ITURB(ICFD)=2
        CALL STRIPC(IFL,OUTSTR,1,ND,1,'fixed eddy viscosity',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,500.,'W',
     &    ' Eddy viscosity for zero-eq turb model.',IER)
        IF(IER.NE.0)then
          call EDISP(IUOUT,
     &      ' Cannot convert eddy viscosity for zero-eq turb model.')
          goto 999
        endif
        rMOOT=VAL
      endif

C If both the zero-equation and k-epsilon turbulence models are specified, then
C the zero-equation model will be used initially, then after a fixed number of
C iterations it will be replaced by the k-epsilon model.  In this case, the user
C must specify at which iteration the changeover from the zero-equation to
C the k-epsilon model takes place.
      if(ZEROT(ICFD) .and. KEMDL(ICFD)) then
        ITURB(ICFD)=4
        ZEROT(ICFD)=.FALSE.
        KEMDL(ICFD)=.FALSE.
        ZandKE(ICFD)=.TRUE.
        CALL STRIPC(IFL,OUTSTR,1,ND,1,
     &          ' Iteration to change from zero-eq to k-epsilon.',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWI(OUTSTR,K,IVAL,0,10000,'W',
     &    ' Iteration to change from zero-eq to k-epsilon.',IER)
        IF(IER.NE.0)then
          call EDISP(IUOUT,
     &             ' Cannot convert changeover point for turb models.')
          goto 999
        endif
        nZtoKE=IVAL
      endif

C Set ITURB for MIT.
      if (MITzero(ICFD)) ITURB(ICFD)=3

C Ensure the choice of turbulence model is sensical.
      if( KEMDL(ICFD) .and. MITzero(ICFD) )then
        call EDISP(IUOUT,
     &     ' Cannot use k-e model in conjunction with MIT 0-eqn model.')
        goto 999
      endif
      if( ZEROT(ICFD) .and. MITzero(ICFD) )then
        call edisp(IUOUT,' Cannot use fixed eddy viscosity model in ')
        call edisp(IUOUT,' conjunction with MIT 0-eqn model.')
        goto 999
      endif
      if( ZandKE(ICFD) .and. MITzero(ICFD) )then
        call edisp(IUOUT,' Cannot use fixed eddy vis & k-e models in')
        call edisp(IUOUT,' conjunction with MIT 0-eqn model.')
        goto 999
      endif

C If Boussinesq approximation is employed, then get the reference temperature
C to use in the buoyancy calculations.
      if(BOUSSI(ICFD))then
        IBUOY(ICFD)=2
        CALL STRIPC(IFL,OUTSTR,1,ND,1,
     &              ' Reference temperature for buoyancy.',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETWR(OUTSTR,K,VAL,-100.,100.,'W',
     &    ' Reference temperature for buoyancy.',IER)
        IF(IER.NE.0)then
          call EDISP(IUOUT,
     &      ' Cannot convert reference temperature for buoyancy.')
          goto 999
        endif
        TBAR=VAL
        if(itrc.gt.0)then
          write(outs,'(a,F7.3)')' Reference T for buoyancy: ',TBAR
          call edisp(itu,outs)
        endif
      endif


C Read in air-flow openings.
      CALL STRIPC(IFL,OUTSTR,1,ND,1,' Number of openings.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,MCFND,'F','Nb of openings',IER)
      IF(IER.NE.0)goto 999

      NOPEN(ICFD)=IVAL
      IF(NOPEN(ICFD).EQ.0)THEN
        IPRESF(ICFD)=INT(NTCX/2)+2
        JPRESF(ICFD)=INT(NTCY/2)+2
        KPRESF(ICFD)=INT(NTCZ/2)+2
        GOTO 102
      ENDIf
      if(itrc.gt.0)then
        write(outs,'(a,i3,a)')' There are ',NOPEN(ICFD),' openings:'
        call edisp(itu,outs)
      endif
      DO 40 i=1,NOPEN(ICFD)

C Add this to list of key volumes and create default name.
        NVOL(ICFD)=NVOL(ICFD)+1
        if (NVOL(ICFD).lt.10) then
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i1)') 'Open_',NVOL(ICFD)
        else
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i2)') 'Open_',NVOL(ICFD)
        endif

C Read connection info.
        CALL STRIPC(IFL,OUTSTR,99,ND,1,' Connection definition',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W',' opening type',IER)
        IF(IER.NE.0)goto 101
        OPNTYP=WORD(1:1)
        CALL EGETW(OUTSTR,K,WORD,'W',' opening location',IER)
        IF(IER.NE.0)goto 101

C Pressure type.
        IF(OPNTYP.EQ.'P'.OR.OPNTYP.EQ.'p')THEN
          IWOPEN(I,ICFD)=0
          IVTYPE(NVOL(ICFD),ICFD)=10
          IVOLF(NVOL(ICFD),ICFD)=7

C Mass flow connection type.
        ELSEIF(OPNTYP.EQ.'M'.OR.OPNTYP.EQ.'m')THEN
          IWOPEN(I,ICFD)=99
          IVTYPE(NVOL(ICFD),ICFD)=13
          IVOLF(NVOL(ICFD),ICFD)=7

C Velocity of zero gradient type.
        ELSEIF(OPNTYP.EQ.'V'.OR.OPNTYP.EQ.'v'.OR.
     &    OPNTYP.EQ.'Z'.OR.OPNTYP.EQ.'z')THEN
          IF(OPNTYP.EQ.'V'.OR.OPNTYP.EQ.'v') then
            IVTYPE(NVOL(ICFD),ICFD)=11
          else
            IVTYPE(NVOL(ICFD),ICFD)=12
          endif
          IF(WORD(1:1).EQ.'W'.or.WORD(1:1).EQ.'w')THEN
            IWOPEN(I,ICFD)=1
          ELSEIF(WORD(1:1).EQ.'E'.or.WORD(1:1).EQ.'e')then
            IWOPEN(I,ICFD)=2
          ELSEIF(WORD(1:1).EQ.'S'.or.WORD(1:1).EQ.'s')then
            IWOPEN(I,ICFD)=3
          ELSEIF(WORD(1:1).EQ.'N'.or.WORD(1:1).EQ.'n')then
            IWOPEN(I,ICFD)=4
          ELSEIF(WORD(1:1).EQ.'L'.or.WORD(1:1).EQ.'l')then
            IWOPEN(I,ICFD)=5
          ELSEIF(WORD(1:1).EQ.'H'.or.WORD(1:1).EQ.'h')then
            IWOPEN(I,ICFD)=6
          ENDIF
          IVOLF(NVOL(ICFD),ICFD)=IWOPEN(I,ICFD)
        ENDIF

C Text feedback.
        if(itrc.gt.0)then
          write(outs,'(4a)')' Opening type: ',OPNTYP,' location:',
     &      WORD(1:1)
          call edisp(itu,outs)
        endif

C If fixed gradient of velocity =0 is chosen at the opening make IWOPEN < 0.
        IF(OPNTYP.EQ.'Z'.OR.OPNTYP.EQ.'z')IWOPEN(I,ICFD)=-IWOPEN(I,ICFD)

C Read cell geometry information.
C I direction.
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCX,'F','NIi opening position',IER)
        IF(IER.NE.0)goto 101
        IOPENi(I,ICFD)=IVAL+1
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCX,'F','NIf opening position',IER)
        IF(IER.NE.0)goto 101
        IOPENf(I,ICFD)=IVAL+1
        IF((IWOPEN(I,ICFD).EQ.1.OR.IWOPEN(I,ICFD).EQ.2).AND.
     &    (IOPENi(I,ICFD).NE.IOPENf(I,ICFD)))THEN
          CALL EDISP(IUOUT,
     &    ' NIi must equal NIf when opening at west or east planes. ')
          IER=-1
          GOTO 101
        ENDIF
        IVCELLS(NVOL(ICFD),ICFD,1)=IOPENi(I,ICFD)
        IVCELLS(NVOL(ICFD),ICFD,2)=IOPENf(I,ICFD)

C J direction.
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCY,'F','NJi opening position',IER)
        IF(IER.NE.0)goto 101
        JOPENi(I,ICFD)=IVAL+1
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCY,'F','NJf opening position',IER)
        IF(IER.NE.0)goto 101
        JOPENf(I,ICFD)=IVAL+1
        IF((IWOPEN(I,ICFD).EQ.3.OR.IWOPEN(I,ICFD).EQ.4).AND.
     &    (JOPENi(I,ICFD).NE.JOPENf(I,ICFD)))THEN
          CALL EDISP(IUOUT,
     &    ' NJi must equal NJf when opening at south or north planes.')
          IER=-1
          GOTO 101
        ENDIF
        JVCELLS(NVOL(ICFD),ICFD,1)=JOPENi(I,ICFD)
        JVCELLS(NVOL(ICFD),ICFD,2)=JOPENf(I,ICFD)

C K direction.
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCZ,'F','NKi opening position',IER)
        IF(IER.NE.0)goto 101
        KOPENi(I,ICFD)=IVAL+1
        CALL EGETWI(OUTSTR,K,IVAL,1,NTCZ,'F','NKf opening position',IER)
        IF(IER.NE.0)goto 101
        KOPENf(I,ICFD)=IVAL+1
        IF((IWOPEN(I,ICFD).EQ.5.OR.IWOPEN(I,ICFD).EQ.6).AND.
     &    (KOPENi(I,ICFD).NE.KOPENf(I,ICFD)))THEN
          CALL EDISP(IUOUT,
     &   ' NKi must equal NKf when opening is at low or high planes.')
          IER=-1
          GOTO 101
        ENDIF
        KVCELLS(NVOL(ICFD),ICFD,1)=KOPENi(I,ICFD)
        KVCELLS(NVOL(ICFD),ICFD,2)=KOPENf(I,ICFD)

C If MFS active read the following information.
        IF(IMFN.ne.0)then

C Connection in the mass flow network linked to the CFD domain.
          CALL EGETW(OUTSTR,K,WORD,'W','+ve node',IER)
          IF(IER.NE.0)goto 101
          MFNODE(I,ICFD)=IGETND(OUTSTR,WORD,IER)
          IF(IER.NE.0)goto 101
          CALL EGETW(OUTSTR,K,WORD1,'W','-ve node',IER)
          IF(IER.NE.0)goto 101
          CALL EGETW(OUTSTR,K,WORD2,'W','Component',IER)
          IF(IER.NE.0)goto 101
          ICFDCN(I,ICFD)=IGETCN(OUTSTR,WORD,WORD1,WORD2,IER)
          IF(IER.NE.0)goto 101
          if(itrc.gt.0)then
            write(outs,'(6a)')' Nodes: +ve ',WORD(1:lnblnk(WORD)),
     &        ' -ve ',WORD1(1:lnblnk(WORD1)),' cmp ',
     &         WORD2(1:lnblnk(WORD2))
            call edisp(itu,outs)
          endif

C Connection in the mass flow network which is not active when the CFD is active.
          CALL EGETW(OUTSTR,K,WORD,'W','+ve node',IER)
          IF(IER.NE.0)goto 101
          CALL EGETW(OUTSTR,K,WORD1,'W','-ve node',IER)
          IF(IER.NE.0)goto 101
          CALL EGETW(OUTSTR,K,WORD2,'W','Component',IER)
          IF(IER.NE.0)goto 101
          ICNACT(I,ICFD)=IGETCN(OUTSTR,WORD,WORD1,WORD2,IER)
          IF(IER.NE.0)goto 101
        ELSE

C If MFS is not active read the following information.
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-',
     &      ' Mass flow rate or pressure value',IER)
          FIXM(I,ICFD)=VAL
          if (IVTYPE(NVOL(ICFD),ICFD).eq.10) then
            VOLPres(NVOL(ICFD),ICFD)=VAL
          else
            VOLVel(NVOL(ICFD),ICFD)=VAL
          endif
          IF(CALLT(ICFD))THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Temperature value',IER)
            IF(IER.NE.0)goto 101
            FIXT(I,ICFD)=VAL
            VOLTemp(NVOL(ICFD),ICFD)=VAL
          ENDIF
          IF(CALLC(ICFD))THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Conc value',IER)
            IF(IER.NE.0)goto 101
            FIXC(I,ICFD)=VAL
          ENDIF

C Initialize angles Theta 1 and 2.
          THETA1(I,ICFD)=0.0
          THETA2(I,ICFD)=0.0

C Read angles Theta 1 and 2 if the openning is an inlet.
          IF (LGRILL(ICFD).AND.FIXM(I,ICFD).GT.0.) THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Theta 1.',IER)
            IF(IER.NE.0) GOTO 999
            THETA1(I,ICFD)=VAL
            VOLDir(NVOL(ICFD),ICFD,1)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Theta 2.',IER)
            IF(IER.NE.0) GOTO 999
            THETA2(I,ICFD)=VAL
            VOLDir(NVOL(ICFD),ICFD,2)=VAL
          ENDIF

        ENDIF
 40   CONTINUE

C If there is conflation between bps' air-flow solver and dfs
C (ie. type 3 conflation), then create a node to represent the
C space occupied by the dfs domain and a CFD component to connect
C this node to nodes at the dfs openings.  Then, make the appropriate
C connections between this new node and the nodes at the dfs openings.
C This must be done so that MFSOLV will call the correct subroutine
C (MF500C) to represent the CFD component.
      IF(IFCFD(ICP).eq.3) THEN
        ncfdnd=NOPEN(ICFD)

C Define a new node to represent the dfs domain.
        NNOD=NNOD+1
        NDNAM(NNOD)='cfd'
        NDFLD(NNOD)=1
        NDTYP(NNOD)=2
        HNOD(NNOD)=0.0
        ITND(NNOD)=0
        TNOD(NNOD)=0.0

C Define a CFD component.
        NCMP=NCMP+1
        CMNAM(NCMP)='CFD'
        ITPCMP(NCMP)=500
        LTPCMP(NCMP)=LVALCM(22)

C Create connections between the dfs-domain node and each
C dfs-opening node.  Include these new connections in
C ICFDCN so that they can be de-activated when not
C required/desired.
        DO 23 M=1,NOPEN(ICFD)
          ncfdnd=ncfdnd+1
          NCNN=NCNN+1
          NODPS(NCNN)=MFNODE(M,ICFD)
          NODNE(NCNN)=NNOD
          HGTPS(NCNN)=0.0
          HGTNE(NCNN)=0.0
          ITPCON(NCNN)=NCMP
          ICFDCN(ncfdnd,ICFD)=NCNN
   23   CONTINUE
      ENDIF

C Assume a position for reference pressure.
      PRSTYP=.FALSE.
      DO 302 L=1,NOPEN(ICFD)
        IF(IWOPEN(L,ICFD).EQ.0.OR.IWOPEN(L,ICFD).EQ.99)THEN
          IPRESF(ICFD)=IOPENi(L,ICFD)
          JPRESF(ICFD)=JOPENi(L,ICFD)
          KPRESF(ICFD)=KOPENi(L,ICFD)
          PRSTYP=.TRUE.
          GOTO 320
        ENDIF
302   CONTINUE
320   IF(.NOT.PRSTYP)THEN
        IPRESF(ICFD)=IOPENi(1,ICFD)
        JPRESF(ICFD)=JOPENi(1,ICFD)
        KPRESF(ICFD)=KOPENi(1,ICFD)
      ENDIF

102   CONTINUE

C Read in data on solid boundaries.
      CALL CFDSBREAD(IFL,IER)

C Read in heat sources and blockages.
      CALL STRIPC(IFL,OUTSTR,1,ND,1,'Heat sources def.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,MNHS,'F','No. of heat src.',IER)
      IF(IER.NE.0)goto 999
      NTHS=IVAL
      DO 45 I=1,2
        NHS(I,ICFD)=0
 45   continue

      IF (NTHS.gt.0) then
      DO 50 i=1,NTHS

C Add this to list of key volumes and create default name.
        NVOL(ICFD)=NVOL(ICFD)+1
        if (NVOL(ICFD).lt.10) then
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i1)') 'Source_',NVOL(ICFD)
        else
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i2)') 'Source_',NVOL(ICFD)
        endif

C Read data.
        CALL STRIPC(IFL,OUTSTR,9,ND,1,'Heat sources def.',IER)
        IF(IER.NE.0)goto 1000
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W',' Type of heat source',IER)
          THSRC=WORD(1:1)
          if(THSRC.EQ.'T'.or.THSRC.EQ.'t')then
            NHS(1,ICFD)=NHS(1,ICFD)+1
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCX,'F',
     &                         ' Heat source. NIi position',IER)
            IF(IER.NE.0)goto 103
            IHSi(NHS(1,ICFD),1,ICFD)=IVAL+1
            IVCELLS(NVOL(ICFD),ICFD,1)=IHSi(NHS(1,ICFD),1,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCX,'F',
     &                         ' Heat source. NIf position',IER)
            IF(IER.NE.0)goto 103
            IHSf(NHS(1,ICFD),1,ICFD)=IVAL+1
            IVCELLS(NVOL(ICFD),ICFD,2)=IHSf(NHS(1,ICFD),1,ICFD)
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCY,'F',
     &                         ' Heat source. NJi position',IER)
            IF(IER.NE.0)goto 103
            JHSi(NHS(1,ICFD),1,ICFD)=IVAL+1
            JVCELLS(NVOL(ICFD),ICFD,1)=JHSi(NHS(1,ICFD),1,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCY,'F',
     &                         ' Heat source. NJf position',IER)
            IF(IER.NE.0)goto 103
            JHSf(NHS(1,ICFD),1,ICFD)=IVAL+1
            JVCELLS(NVOL(ICFD),ICFD,2)=JHSf(NHS(1,ICFD),1,ICFD)
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCZ,'F',
     &                         ' Heat source. NKi position',IER)
            IF(IER.NE.0)goto 103
            KHSi(NHS(1,ICFD),1,ICFD)=IVAL+1
            KVCELLS(NVOL(ICFD),ICFD,1)=KHSi(NHS(1,ICFD),1,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCZ,'F',
     &                         ' Heat source. NKf position',IER)
            IF(IER.NE.0)goto 103
            KHSf(NHS(1,ICFD),1,ICFD)=IVAL+1
            KVCELLS(NVOL(ICFD),ICFD,2)=KHSf(NHS(1,ICFD),1,ICFD)
            CALL EGETW(OUTSTR,K,WORD,'W','Source temperature',IER)
            IF(IER.NE.0)goto 103
            IF(WORD.EQ.'APT'.OR.WORD.EQ.'apt')THEN
              HSID(NHS(1,ICFD),1,ICFD)=WORD(1:3)
            ELSE
              HSID(NHS(1,ICFD),1,ICFD)=WORD(1:3)
              READ(word,*,ERR=999)VAL
              SHS(NHS(1,ICFD),1,ICFD)=VAL
            ENDIF
            CALL EGETW(OUTSTR,K,WORD,'W','nature of source',IER)
            IF(IER.NE.0)goto 103
            if(word(1:1).EQ.'Y'.or.word(1:1).EQ.'y')then
              BHS(NHS(1,ICFD),1,ICFD)=.true.
            else if(word(1:1).EQ.'N'.or.word(1:1).EQ.'n')then
              BHS(NHS(1,ICFD),1,ICFD)=.false.
            else
              call edisp(IUOUT,' Blockage of heat source.')
              goto 999
            endif
          else if(THSRC.EQ.'H'.or.THSRC.EQ.'h')then
            NHS(2,ICFD)=NHS(2,ICFD)+1
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCX,'F',
     &        ' Heat source. NIi position',IER)
            IF(IER.NE.0)goto 103
            IHSi(NHS(2,ICFD),2,ICFD)=IVAL+1
            IVCELLS(NVOL(ICFD),ICFD,1)=IHSi(NHS(2,ICFD),2,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCX,'F',
     &        ' Heat source. NIf position',IER)
            IF(IER.NE.0)goto 103
            IHSf(NHS(2,ICFD),2,ICFD)=IVAL+1
            IVCELLS(NVOL(ICFD),ICFD,2)=IHSf(NHS(2,ICFD),2,ICFD)
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCY,'F',
     &        ' Heat source. NJi position',IER)
            IF(IER.NE.0)goto 103
            JHSi(NHS(2,ICFD),2,ICFD)=IVAL+1
            JVCELLS(NVOL(ICFD),ICFD,1)=JHSi(NHS(2,ICFD),2,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCY,'F',
     &        ' Heat source. NJf position',IER)
            IF(IER.NE.0)goto 103
            JHSf(NHS(2,ICFD),2,ICFD)=IVAL+1
            JVCELLS(NVOL(ICFD),ICFD,2)=JHSf(NHS(2,ICFD),2,ICFD)
            CALL EGETWI(OUTSTR,K,IVAL,1,NTCZ,'-',
     &        ' Heat source. NKi position',IER)
            IF(IER.NE.0)goto 103
            KHSi(NHS(2,ICFD),2,ICFD)=IVAL+1
            KVCELLS(NVOL(ICFD),ICFD,1)=KHSi(NHS(2,ICFD),2,ICFD)
            Ist=IVAL
            CALL EGETWI(OUTSTR,K,IVAL,Ist,NTCZ,'F',
     &        ' Heat source. NKf position',IER)
            IF(IER.NE.0)goto 103
            KHSf(NHS(2,ICFD),2,ICFD)=IVAL+1
            KVCELLS(NVOL(ICFD),ICFD,2)=KHSf(NHS(2,ICFD),2,ICFD)
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-',
     &        ' Heat supplied in the heat source (W).',IER)
              IF(IER.NE.0)goto 103
              SHS(NHS(2,ICFD),2,ICFD)=VAL
              VOLHeat(NVOL(ICFD),ICFD)=VAL
            CALL EGETW(OUTSTR,K,WORD,'W','nature of source',IER)
            IF(IER.NE.0)goto 103
            if(word(1:1).EQ.'Y'.or.word(1:1).EQ.'y')then
              BHS(NHS(2,ICFD),2,ICFD)=.true.
            else if(word(1:1).EQ.'N'.or.word(1:1).EQ.'n')then
              BHS(NHS(2,ICFD),2,ICFD)=.false.
            else
              call edisp(IUOUT,' Blockage of heat source.')
              goto 999
            endif
          else
            goto 103
          endif
   50   continue
      endif

      CALL STRIPC(IFL,OUTSTR,3,ND,1,
     &  ' Convergence parameters.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,0,'-',
     &    ' Maximum number of iterations.',IER)
      IF(IER.NE.0)then
        call EDISP(IUOUT,
     &    ' Cannot convert the maximum number of iterations.')
        goto 999
      endif
      MAXITR(ICFD)=IVAL
      CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-',
     &  ' Maximum source of residuals.',IER)
      IF(IER.NE.0)then
        call EDISP(IUOUT,
     &    ' Cannot convert the maximum source of residuals.')
        goto 999
      endif
      SRMAX(ICFD)=VAL

C Check if residuals should be renormalized or not.
      LRENORM=.TRUE.
      CALL EGETW(OUTSTR,K,WORD,'W',' Renormalized residuals?',IER)
      if(IER.NE.0)then
        call EDISP(IUOUT,
     &    ' Cannot convert Residual renormalization flag.')
        goto 999
      endif
      if(word(1:1).eq.'N'.or.word(1:1).eq.'n')LRENORM=.FALSE.
C read switch for non-convergence ignoring and maximum number of conflation mfs-dfs loops ...
      CALL EGETWI(OUTSTR,K,IVAL,0,4,'W','cfdat-nostop',IER)
      NOSTOP=IVAL
      CALL EGETWI(OUTSTR,K,IVAL,1,30,'W','cfdat-nostop',IER)
      MITMFS=IVAL
c  end insertion

C Relaxation factors.
      FLSDTU(ICFD)=1.0E+20
      FLSDTV(ICFD)=1.0E+20
      FLSDTW(ICFD)=1.0E+20
      FLSDTP(ICFD)=1.0E+20
      FLSDTT(ICFD)=1.0E+20
      FLSDTK(ICFD)=1.0E+20
      FLSDTE(ICFD)=1.0E+20
      URFCU(ICFD)=0.5
      URFCV(ICFD)=0.5
      URFCW(ICFD)=0.5
      URFCP(ICFD)=0.7
      URFCT(ICFD)=1.0
      URFCK(ICFD)=1.0
      URFCE(ICFD)=1.0
      URFCVS(ICFD)=1.0

C Relaxation factor for density (default).
      URFCD(ICFD)=1.0
      CALL STRIPC(IFL,OUTSTR,1,ND,1,'Relaxation factors def.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,15,'F','No of relax. fctrs',IER)
      IF(IER.NE.0)then
        call edisp(IUOUT,' Cannot convert number of relax. factors.')
        goto 999
      endif
      nrfac=IVAL

C Set flags to indicate that no relaxation factors have been defined.
      urlx=.false.
      vrlx=.false.
      wrlx=.false.
      prlx=.false.
      krlx=.false.
      erlx=.false.
      trlx=.false.
      if(nrfac.gt.0) then
        do 60 i=1,nrfac
          CALL STRIPC(IFL,OUTSTR,3,ND,1,' Relaxation factor.',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','Relax factor type.',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,'Cannot convert type of relaxation factor')
            goto 999
          endif
          RELFAC=WORD(1:1)
          CALL EGETW(OUTSTR,K,WORD,'W',' Variable type.',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot convert variable type.')
            goto 999
          endif
          VAR=WORD(1:2)
          CALL EGETWR(OUTSTR,K,VAL,-1.0E+20,1.0E+20,'F',
     &                            ' Value of relaxation factor.',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,'Cannot convert value of relax. factor')
            goto 999
          endif
          if(RELFAC.EQ.'I'.or.RELFAC.EQ.'i')then
            IF(ABS(VAL).LE.1.0E+20)then
              IF(VAR(1:1).EQ.'U'.or.VAR(1:1).EQ.'u')then
                FLSDTU(ICFD)=VAL
              ELSEIF(VAR(1:2).EQ.'V '.or.VAR(1:2).EQ.'v ')then
                FLSDTV(ICFD)=VAL
              ELSEIF(VAR(1:1).EQ.'W'.or.VAR(1:1).EQ.'w')then
                FLSDTW(ICFD)=VAL
              ELSEIF(VAR(1:1).EQ.'P'.or.VAR(1:1).EQ.'p')then
                FLSDTP(ICFD)=VAL
              ELSEIF(VAR(1:1).EQ.'T'.or.VAR(1:1).EQ.'t')then
                FLSDTT(ICFD)=VAL
              ELSEIF(VAR(1:1).EQ.'K'.or.VAR(1:1).EQ.'k')then
                FLSDTK(ICFD)=VAL
              ELSEIF(VAR(1:1).EQ.'E'.or.VAR(1:1).EQ.'e')then
                FLSDTE(ICFD)=VAL
              ELSE
                call EDISP(IUOUT,
     &           ' Variable type must be: U, V, W, P, T, K or Epsilon.')
                goto 999
              endif
            else
              call edisp(IUOUT,'False dt must be > zero and <= 1.0E+20')
              goto 999
            endif
          else if(RELFAC.EQ.'L'.or.RELFAC.EQ.'l')then
            IF(VAL.GT.0.0.and.VAL.LE.1.0)then
              IF(VAR(1:1).EQ.'U'.or.VAR(1:1).EQ.'u')then
                URFCU(ICFD)=VAL
                urlx=.true.
              ELSEIF(VAR(1:2).EQ.'V '.or.VAR(1:2).EQ.'v ')then
                URFCV(ICFD)=VAL
                vrlx=.true.
              ELSEIF(VAR(1:1).EQ.'W'.or.VAR(1:1).EQ.'w')then
                URFCW(ICFD)=VAL
                wrlx=.true.
              ELSEIF(VAR(1:1).EQ.'P'.or.VAR(1:1).EQ.'p')then
                URFCP(ICFD)=VAL
                prlx=.true.
              ELSEIF(VAR(1:1).EQ.'T'.or.VAR(1:1).EQ.'t')then
                URFCT(ICFD)=VAL
                trlx=.true.
              ELSEIF(VAR(1:1).EQ.'K'.or.VAR(1:1).EQ.'k')then
                URFCK(ICFD)=VAL
                krlx=.true.
              ELSEIF(VAR(1:1).EQ.'E'.or.VAR(1:1).EQ.'e')then
                URFCE(ICFD)=VAL
                erlx=.true.
              ELSEIF(VAR(1:2).EQ.'VI'.or.VAR(1:2).EQ.'vi'.or.
     &                                               VAR.EQ.'Vi')then
                URFCVS(ICFD)=VAL

C If one wants to have density relaxated, put 'DE' or 'de' at the
C list of relaxation factors.
              ELSEIF(VAR(1:2).EQ.'DE'.or.VAR(1:2).EQ.'de'.or.
     &                                               VAR.EQ.'De')then
                URFCD(ICFD)=VAL

              ELSE
               call EDISP(IUOUT,
     &        'Variable type must be: U, V, W, P, T, K, Epsil or Visc.')
               goto 999
              endif
            else
              call EDISP(IUOUT,
     &        'Linear under-relaxation factor must be > 0.0 and <= 1.0')
              goto 999
            endif
          else
            call EDISP(IUOUT,
     &          'Type of relaxation factor must be: inertia or linear.')
            goto 999
          endif
   60   continue
      endif

C Number of sweeps for each variable (default).
      NSSWPU(ICFD)=3
      NSSWPV(ICFD)=3
      NSSWPW(ICFD)=3
      NSSWPP(ICFD)=5
      NSSWPK(ICFD)=3
      NSSWPE(ICFD)=3
      NSSWPT(ICFD)=3
      CALL STRIPC(IFL,OUTSTR,1,ND,1,'Variable sweeping def.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,7,'F',
     &                         ' Number of variables to be swept.',IER)
      IF(IER.NE.0)then
        call EDISP(IUOUT,
     &               'Cannot convert number of variables to be swept.')
        goto 999
      endif
      nsweep=IVAL
      if(nsweep.gt.0)then
        do 70 i=1,nsweep
          CALL STRIPC(IFL,OUTSTR,2,ND,1,' Number of sweeps.',IER)
          IF(IER.NE.0)goto 1000
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W',' Variable name.',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot convert variable name.')
            goto 999
          endif
          VAR=WORD(1:1)
          CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',
     &                               ' Value of number of sweeps.',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,'Cannot convert value of no. of sweeps.')
            goto 999
          endif
          IF(IVAL.GE.1.and.IVAL.LE.10)then
            IF(VAR(1:1).EQ.'U'.or.VAR(1:1).EQ.'u')then
              NSSWPU(ICFD)=IVAL
            ELSEIF(VAR(1:2).EQ.'V '.or.VAR(1:2).EQ.'v ')then
              NSSWPV(ICFD)=IVAL
            ELSEIF(VAR(1:1).EQ.'W'.or.VAR(1:1).EQ.'w')then
              NSSWPW(ICFD)=IVAL
            ELSEIF(VAR(1:1).EQ.'P'.or.VAR(1:1).EQ.'p')then
              NSSWPP(ICFD)=IVAL
            ELSEIF(VAR(1:1).EQ.'T'.or.VAR(1:1).EQ.'t')then
              NSSWPT(ICFD)=IVAL
            ELSEIF(VAR(1:1).EQ.'K'.or.VAR(1:1).EQ.'k')then
              NSSWPK(ICFD)=IVAL
            ELSEIF(VAR(1:1).EQ.'E'.or.VAR(1:1).EQ.'e')then
              NSSWPE(ICFD)=IVAL
            ELSE
              call EDISP(IUOUT,
     &            ' Variable type must be: U, V, W, P, T, K or Epsil.')
              goto 999
            endif
          else
            call edisp(IUOUT,'Number of sweeps must be >= 1 and <= 10.')
            goto 999
          endif
   70   continue
      endif

C Initial Values.
      TINIT(ICFD)=SMALL
      UINIT(ICFD)=0.0
      VINIT(ICFD)=0.0
      WINIT(ICFD)=0.0
      PINIT(ICFD)=0.0
      TEINIT(ICFD)=SMALL
      EDINIT(ICFD)=SMALL
      CALL STRIPC(IFL,OUTSTR,1,ND,1,'No. of initialized vars',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,7,'F','No. of init vars.',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,'Cannot convert no. of initialized variables.')
        goto 999
      endif
      NINIT=IVAL

C Set flags to indicate that no initial conditions have been defined.
      uic=.false.
      vic=.false.
      wic=.false.
      pic=.false.
      kic=.false.
      eic=.false.
      tic=.false.
      do 200 i=1,NINIT
        CALL STRIPC(IFL,OUTSTR,2,ND,1,' Initial value def.',IER)
        IF(IER.NE.0)goto 1000
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W',' Initialized var name.',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot convert Initialized variable name.')
          goto 999
        endif
        VAR=WORD(1:1)
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0E+20,'F','Init value.',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot convert initial value.')
          goto 999
        endif
        IF(VAR(1:1).EQ.'U'.or.VAR(1:1).EQ.'u')then
          UINIT(ICFD)=VAL
          uic=.true.
        ELSEIF(VAR(1:1).EQ.'V'.or.VAR(1:1).EQ.'v')then
          VINIT(ICFD)=VAL
          vic=.true.
        ELSEIF(VAR(1:1).EQ.'W'.or.VAR(1:1).EQ.'w')then
          WINIT(ICFD)=VAL
          wic=.true.
        ELSEIF(VAR(1:1).EQ.'P'.or.VAR(1:1).EQ.'p')then
          PINIT(ICFD)=VAL
          pic=.true.
        ELSEIF(VAR(1:1).EQ.'T'.or.VAR(1:1).EQ.'t')then
          TINIT(ICFD)=VAL
          tic=.true.
        ELSEIF(VAR(1:1).EQ.'K'.or.VAR(1:1).EQ.'k')then
          TEINIT(ICFD)=VAL
          kic=.true.
        ELSEIF(VAR(1:1).EQ.'E'.or.VAR(1:1).EQ.'e')then
          EDINIT(ICFD)=VAL
          eic=.true.
        ELSE
          call EDISP(IUOUT,
     &     ' Variable name must be: U, V, W, P, T, K, Epsil or Visc.')
          goto 999
        endif
 200  CONTINUE

C If the temperature is not solved, make inlet and outlet temperatures
C equal to the initial value.
      IF(.NOT.CALLT(ICFD)) THEN
        DO 195 I=1,NOPEN(ICFD)
          FIXT(I,ICFD)=TINIT(ICFD)
 195    CONTINUE
      ENDIF

C Output, visualization, and monitoring files.
C Output file.
      CALL STRIPC(IFL,OUTSTR,2,ND,1,' Result file.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,1,24,'F',
     &                    ' Time-step interval to print results.',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot convert Interval of time-step.')
        goto 999
      endif
      IPPHI(ICFD)=IVAL
      CALL EGETW(OUTSTR,K,WORD,'W',' Result file name.',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot convert result file name.')
        goto 999
      endif
      LPHI(ICFD)=WORD(1:72)

C Flow vector file to be used for flow visualization.
      ND=0
      CALL STRIPC(IFL,OUTSTR,99,ND,1,' Flow vec& vis files',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W',' Flow vector file',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read name of flow vector file.')
        goto 999
      endif
      LVEC=WORD(1:72)
      if (ND.eq.2) then
        CALL EGETW(OUTSTR,K,WORD,'W',' Flow vis file',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read name of flow vis file.')
          goto 999
        endif
        Lflpar(ICOMP)=WORD(1:72)
      endif

C Monitoring file.
      IMONT(ICFD)=INT(NTCX/2)+1
      JMONT(ICFD)=INT(NTCY/2)+1
      KMONT(ICFD)=INT(NTCZ/2)+1
      CALL STRIPC(IFL,OUTSTR,4,ND,1,' Monitoring.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,1,NTCX,'F','I monitor pos.',IER)
      IF(IER.NE.0)goto 107
      IMONT(ICFD)=IVAL+1
      CALL EGETWI(OUTSTR,K,IVAL,1,NTCY,'F','J monitor pos.',IER)
      IF(IER.NE.0)goto 107
      JMONT(ICFD)=IVAL+1
      CALL EGETWI(OUTSTR,K,IVAL,1,NTCZ,'F','K monitor pos.',IER)
      IF(IER.NE.0)goto 107
      KMONT(ICFD)=IVAL+1
      CALL EGETW(OUTSTR,K,WORD,'W',' Monitoring file name.',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot convert monitoring file name.')
        goto 999
      endif
      CFTRFL(ICFD)=WORD(1:72)
      goto 108

  107 call edisp(IUOUT,' Cannot convert monitoring position.')
      goto 999

  108 INITFL(ICFD)=.false.
      CALL STRIPC(IFL,OUTSTR,1,ND,1,' CFD re-initialization.',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W',' Re-initialization?.',IER)
      if(IER.NE.0)then
        call EDISP(IUOUT,
     &    ' Cannot convert CFD re-initialization flag.')
        goto 999
      endif
      if(word(1:1).eq.'Y'.or.word(1:1).eq.'y')INITFL(ICFD)=.TRUE.
      goto 100

C Error trapping.
 1000 CALL USRMSG('CFDDTA Conversion error in',OUTSTR,'W')
      goto 100

  101 call edisp(IUOUT,' Connection Definition. Surface location.')
      goto 999
  103 call edisp(IUOUT,' Heat source definition.')
      goto 999
 1001 call edisp(IUOUT,' MIT turb model not available for CFD-only.')
      goto 999

C Error trap on read error.
  999 CALL edisp(IUOUT,'CFDDAT: error reading cfd file!')
      CALL EDISP(IUOUT,OUTSTR)

  100 RETURN
      END


C ********************* IGETCN *********************
C IGETCN - Identify mass flow connection. (used for coupling purpose)
      FUNCTION IGETCN(OUTSTR,WORD,WORD1,WORD2,IER)
#include "net_flow.h"

      common/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      CHARACTER WORD*72,WORD1*72,WORD2*72,NODLE*12,NODRI*12,CMPNM*12
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12,OUTSTR*124

      NODLE=WORD(1:12)
      NODRI=WORD1(1:12)
      CMPNM=WORD2(1:12)
      DO 10 ICNN=1,NCNN
        IF(NODLE.EQ.NDNAM(NODPS(ICNN)).AND.NODRI.EQ.NDNAM(NODNE(ICNN))
     &    .AND.CMPNM.EQ.CMNAM(ITPCON(ICNN)))THEN
          IGETCN=ICNN
          GOTO 100
        ENDIF
   10 CONTINUE
      
      call edisp(IUOUT,' could not find +ve node name...')
      
C Error trap on read error.
      IER=1
      CALL edisp(IUOUT,'GETCNN: error reading cfd file!')
      CALL EDISP(IUOUT,OUTSTR)

  100 RETURN
      END


C ********************* IGETND *********************
C IGETND - Identify mass flow network node associated with a CFD node.
C          (used for coupling purpose)
      FUNCTION IGETND(OUTSTR,WORD,IER)
#include "net_flow.h"

      common/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)

      CHARACTER WORD*72,NODE*12,NDNAM*12,OUTSTR*124

      NODE=WORD(1:12)
      DO 10 I=1,NNOD
        IF(NODE.EQ.NDNAM(I))THEN
          IGETND=I
          GOTO 100
        ENDIF
   10 CONTINUE
      
      call edisp(IUOUT,' could not find MF node name...')
      
C Error trap on read error
      IER=1
      CALL edisp(IUOUT,'IGETND: error reading cfd file!')
      CALL EDISP(IUOUT,OUTSTR)

  100 RETURN
      END


C ********************* IDSURF *********************
C IDSURF - Identify ESP-r building surface number (used for thermal conflation).
C   This function identify bld surface associated with cfd surface.
C   surf (12 char) is passed parameter. The current zone is taken
C   from common block variable icp.
      FUNCTION IDSURF(SURF)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER SURF*12
      integer lnssn,lnsurf

      do 10 IS=1,nzsur(icp)
        icon=IZSTOCN(icp,is)
        lnssn=lnblnk(SSNAME(icon))
        lnsurf=lnblnk(SURF)
        IF(SSNAME(icon)(1:lnssn).EQ.SURF(1:lnsurf))then
          IDSURF=IS
          GOTO 99
        ENDIF
   10 continue

   99 RETURN
      END


C ********************* INICNT *********************
C INICNT - Initialize some constant parameters.
      SUBROUTINE INICNT
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/TURB/GEN(ntcelx,ntcely,ntcelz),CD,CMU,C1,C2,C3,CAPPA,ELOG,
     1            TURBIN,ALAMDA,PRTE,PRED
      COMMON/CONST/GREAT,SMALL,GRAV

      GREAT=1.E+20
      SMALL=1.E-20
      GRAV=9.81

C Fluid properties.
C Molecular viscosity.
      VISCOS=18.464E-6

C Prandtl number.
      PRANDL=0.71

C Turbulent prandtl number.
      PRANDT=0.9

C Specific heat of the air.
      SH=1006.0

C Turbulence constants.
      CMU=0.09
      CD=1.00
      C1=1.44
      C2=1.92
      C3=1.0
      CAPPA=.4187
      ELOG=9.793
      ALAMDA=0.005
      TURBIN=0.03
      PRED=CAPPA*CAPPA/(C2-C1)/(CMU**.5)
      PRTE=1.0

C The "pee function".
      PFUN=PRANDL/PRANDT
      PFUN=9.24*(PFUN**0.75-1.0)*(1.0+0.28*EXP(-0.007*PFUN))

C Grid related data.
      NIM2=0
      NJM2=0
      NKM2=0

C Calculate the total number of cells in each direction.
      DO 10 I=1,NREG(1,ICFD)
        NIM2=NIM2+ABS(NCELX(I,ICFD))
   10 CONTINUE
      DO 20 I=1,NREG(2,ICFD)
        NJM2=NJM2+ABS(NCELY(I,ICFD))
   20 CONTINUE
      DO 30 I=1,NREG(3,ICFD)
        NKM2=NKM2+ABS(NCELZ(I,ICFD))
   30 CONTINUE
      NIM1=NIM2+1
      NJM1=NJM2+1
      NKM1=NKM2+1
      NI=NIM1+1
      NJ=NJM1+1
      NK=NKM1+1

      RETURN
      END


C ******************** CFDSBREAD ********************
C CFDSBREAD reads in the data and sets the variables describing the CFD
C solid boundaries.

      SUBROUTINE CFDSBREAD(IFL,IER)
#include "building.h"
#include "cfd.h"


      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      common/grdmax/NTCX,NTCY,NTCZ
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)

C Boundary condition commons.
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      character VOLNAME*12, VCsurf*12
      CHARACTER*124 OUTSTR,WORD,outs
      CHARACTER TSBND*1,SURFAC*1,SURF*12,LCFD*72
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI,ZEROT,
     &        ZandKE,MITzero

C Read number of solid boundaries. Return to calling routine if none.
      call STRIPC(IFL,OUTSTR,1,ND,1,'Number of solid boundaries.',IER)
      if(IER.NE.0) goto 1000
      K=0
      call EGETWI(OUTSTR,K,IVAL,0,MNSBZ,'F','No of solid bndaries',IER)
      if(IER.NE.0) goto 999
      NSB(ICFD) = IVAL
      if(NSB(ICFD).EQ.0) goto 100

C Initialize array holding BC values; initialize BC counter (for CFD-only).
      do 10 J=1,NSB(ICFD)
        SSB(J,ICFD)=0.
   10 continue
      ISBND = 0

C Read data for each solid boundary in turn. First check BC `type'.
      DO 20 I=1,NSB(ICFD)

C Add this to list of key volumes and create default name.
        NVOL(ICFD)=NVOL(ICFD)+1
        if (NVOL(ICFD).lt.10) then
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i1)') 'Solid_',NVOL(ICFD)
        else
          write (VOLNAME(NVOL(ICFD),ICFD),'(a,i2)') 'Solid_',NVOL(ICFD)
        endif

C Read data.
        call STRIPC(IFL,OUTSTR,0,ND,1,'Solid boundary definition.',IER)
        if(IER.NE.0) goto 1000
        K=0
        call EGETW(OUTSTR,K,WORD,'W',' type of boundary',IER)
        TSBND = WORD(1:1)

C Set IWSB array element based on BC type and location of surface.
        IF(TSBND.EQ.'T'.or.TSBND.EQ.'t')then

C `Prescribed temp' BC: not applicable for BSim-CFD thermal conflation.
          if( IFCFD(ICP).eq.1 .or. IFCFD(ICP).eq.2 .or.
     &       (IFCFD(ICP).ge.4.and.IFCFD(ICP).le.7) ) goto 998
          CALL EGETW(OUTSTR,K,WORD,'W',' Surf. location',IER)
          SURFAC=WORD(1:1)
          IF(SURFAC.EQ.'W'.or.SURFAC.EQ.'w')THEN
            IWSB(I,ICFD)=1
          ELSEIF(SURFAC.EQ.'E'.or.SURFAC.EQ.'e')THEN
            IWSB(I,ICFD)=2
          ELSEIF(SURFAC.EQ.'S'.or.SURFAC.EQ.'s')THEN
            IWSB(I,ICFD)=3
          ELSEIF(SURFAC.EQ.'N'.or.SURFAC.EQ.'n')THEN
            IWSB(I,ICFD)=4
          ELSEIF(SURFAC.EQ.'L'.or.SURFAC.EQ.'l')THEN
            IWSB(I,ICFD)=5
          ELSEIF(SURFAC.EQ.'H'.or.SURFAC.EQ.'h')THEN
            IWSB(I,ICFD)=6
          ENDIF
          IVTYPE(NVOL(ICFD),ICFD)=1
          IVOLF(NVOL(ICFD),ICFD)=IWSB(I,ICFD)
        ELSEIF(TSBND.EQ.'H'.or.TSBND.EQ.'h')THEN

C `Prescribed heat flow' BC: not applicable for BSim-CFD thermal conf.
          if( IFCFD(ICP).eq.1 .or. IFCFD(ICP).eq.2 .or.
     &       (IFCFD(ICP).ge.4.and.IFCFD(ICP).le.7) ) goto 998
          CALL EGETW(OUTSTR,K,WORD,'W',' Surf. location',IER)
          SURFAC=WORD(1:1)
          IF(SURFAC.EQ.'W'.or.SURFAC.EQ.'w')THEN
            IWSB(I,ICFD)=-1
          ELSEIF(SURFAC.EQ.'E'.or.SURFAC.EQ.'e')THEN
            IWSB(I,ICFD)=-2
          ELSEIF(SURFAC.EQ.'S'.or.SURFAC.EQ.'s')THEN
            IWSB(I,ICFD)=-3
          ELSEIF(SURFAC.EQ.'N'.or.SURFAC.EQ.'n')THEN
            IWSB(I,ICFD)=-4
          ELSEIF(SURFAC.EQ.'L'.or.SURFAC.EQ.'l')THEN
            IWSB(I,ICFD)=-5
          ELSEIF(SURFAC.EQ.'H'.or.SURFAC.EQ.'h')THEN
            IWSB(I,ICFD)=-6
          ENDIF
          IVTYPE(NVOL(ICFD),ICFD)=2
          IVOLF(NVOL(ICFD),ICFD)=IWSB(I,ICFD)
        ELSEIF(TSBND.EQ.'S'.or.TSBND.EQ.'s')THEN

C `Symmetrical' BC: not applicable for BSim-CFD thermal conflation.
          if( IFCFD(ICP).eq.1 .or. IFCFD(ICP).eq.2 .or.
     &       (IFCFD(ICP).ge.4.and.IFCFD(ICP).le.7) ) goto 998
          CALL EGETW(OUTSTR,K,WORD,'W',' Surf. location',IER)
          SURFAC=WORD(1:1)
          IF(SURFAC.EQ.'W'.or.SURFAC.EQ.'w')THEN
            IWSB(I,ICFD)=11
          ELSEIF(SURFAC.EQ.'E'.or.SURFAC.EQ.'e')THEN
            IWSB(I,ICFD)=12
          ELSEIF(SURFAC.EQ.'S'.or.SURFAC.EQ.'s')THEN
            IWSB(I,ICFD)=13
          ELSEIF(SURFAC.EQ.'N'.or.SURFAC.EQ.'n')THEN
            IWSB(I,ICFD)=14
          ELSEIF(SURFAC.EQ.'L'.or.SURFAC.EQ.'l')THEN
            IWSB(I,ICFD)=15
          ELSEIF(SURFAC.EQ.'H'.or.SURFAC.EQ.'h')THEN
            IWSB(I,ICFD)=16
          ELSE
            goto 998
          ENDIF
          IVTYPE(NVOL(ICFD),ICFD)=3
          IVOLF(NVOL(ICFD),ICFD)=IWSB(I,ICFD)
        ELSEIF(TSBND.EQ.'C'.or.TSBND.EQ.'c')THEN

C `Conflated' BC: only applicable when BSim-CFD are thermally conflated.
          if( IFCFD(ICP).ne.1 .and. IFCFD(ICP).ne.2
     &        .and. IFCFD(ICP).ne.4 .and. IFCFD(ICP).ne.5
     &        .and. IFCFD(ICP).ne.6 .and. IFCFD(ICP).ne.7 ) goto 998

C `Conflated' BC only applicable when one of the tubulence models is
C active; laminar flow can only be modelled for CFD-only problems.
          if(.not.(KEMDL(ICFD).or.ZEROT(ICFD).or.ZandKE(ICFD)
     &       .or.MITzero(ICFD)) ) goto 991

C Get flag indicating `thermal conflation type'.
          CALL EGETWI(OUTSTR,K,IVAL,1,14,'F',
     &                ' Thermal conflation type flag.',IER)
          IF(IER.NE.0)goto 997
          ITCtype(I,ICFD) = IVAL
          IVConfl(NVOL(ICFD),ICFD)=ITCtype(I,ICFD)

C Make preliminary IWSB assignment.
          CALL EGETW(OUTSTR,K,WORD,'W',' Surf. location',IER)
          SURFAC=WORD(1:1)
          IF(SURFAC.EQ.'W'.or.SURFAC.EQ.'w')THEN
            IWSB(I,ICFD)=1
          ELSEIF(SURFAC.EQ.'E'.or.SURFAC.EQ.'e')THEN
            IWSB(I,ICFD)=2
          ELSEIF(SURFAC.EQ.'S'.or.SURFAC.EQ.'s')THEN
            IWSB(I,ICFD)=3
          ELSEIF(SURFAC.EQ.'N'.or.SURFAC.EQ.'n')THEN
            IWSB(I,ICFD)=4
          ELSEIF(SURFAC.EQ.'L'.or.SURFAC.EQ.'l')THEN
            IWSB(I,ICFD)=5
          ELSEIF(SURFAC.EQ.'H'.or.SURFAC.EQ.'h')THEN
            IWSB(I,ICFD)=6
          ELSE
            goto 998
          ENDIF
          IVTYPE(NVOL(ICFD),ICFD)=4
          IVOLF(NVOL(ICFD),ICFD)=IWSB(I,ICFD)

C Now adjust IWSB for thermal conflation type. Set icTREF where applicable.
          if(ITCtype(I,ICFD).eq.1)then

C `One-way surface' conflation; k-epsilon model with log-law wall
C functions; Qsurf calculated by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD)
          elseif(ITCtype(I,ICFD).eq.2)then

C `One-way surface' conflation; MIT 0-eqn model; Qsurf calculated
C by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 50
          elseif(ITCtype(I,ICFD).eq.3)then

C `One-way surface' conflation; k-epsilon model with Yuan wall
C functions; Qsurf calculated by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 60
          elseif(ITCtype(I,ICFD).eq.4)then

C `One-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tref); Tref=TFA (ie. from BSim).
            IWSB(I,ICFD) = IWSB(I,ICFD) + 20
            icTREF(I,ICFD) = 1
          elseif(ITCtype(I,ICFD).eq.5)then

C `One-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tref); Tref=Tcfd (domain averaged).
            IWSB(I,ICFD) = IWSB(I,ICFD) + 20
            icTREF(I,ICFD) = 2
          elseif(ITCtype(I,ICFD).eq.6)then

C `One-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tref); Tref=TFA (ie. from BSim); temperature of
C next-to-wall points fixed using log-law wall functions.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 30
            icTREF(I,ICFD) = 1
          elseif(ITCtype(I,ICFD).eq.7)then

C `One-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tref); Tref=Tcfd (domain averaged); temperature
C of next-to-wall points fixed using log-law wall functions.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 30
            icTREF(I,ICFD) = 2
          elseif(ITCtype(I,ICFD).eq.8)then

C `One-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C HC treated as local value.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 40
          elseif(ITCtype(I,ICFD).eq.9)then

C `Two-way surface' conflation; k-epsilon model with log-law wall
C functions; Qsurf calculated by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD)
          elseif(ITCtype(I,ICFD).eq.10)then

C `Two-way surface' conflation; MIT 0-eqn model; Qsurf calculated
C by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 50
          elseif(ITCtype(I,ICFD).eq.11)then

C `Two-way surface' conflation; k-epsilon model with Yuan wall
C functions; Qsurf calculated by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 60
          elseif(ITCtype(I,ICFD).eq.12)then

C `Two-way surface' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C HC treated as local value.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 40
          elseif(ITCtype(I,ICFD).eq.13)then

C `Two-way integrated' conflation; k-epsilon model with log-law wall
C functions; Qsurf calculated by CFD.
            IWSB(I,ICFD) = IWSB(I,ICFD)
          elseif(ITCtype(I,ICFD).eq.14)then

C `Two-way integrated' conflation; k-epsilon model;
C Qsurf=A*HC*(Tsurf-Tp); Tp is temperature of next-to-wall grid points;
C HC treated as local value.
            IWSB(I,ICFD) = IWSB(I,ICFD) + 40
          else

C Not a valid thermal conflation type.
            goto 997
          endif
        ELSE

C Not a valid thermal BC type.
          goto 998
        ENDIF

C Set location of solid boundary in terms of cells covered.
C X-axis location.
        call EGETWI(OUTSTR,K,IVAL,1,NTCX,
     &              'F',' Solid boundary. NIi position.',IER)
        if(IER.NE.0) goto 996
        ISBi(I,ICFD) = IVAL+1
        IVCELLS(NVOL(ICFD),ICFD,1)=ISBi(I,ICFD)
        Ist = IVAL
        call EGETWI(OUTSTR,K,IVAL,Ist,NTCX,
     &              'F',' Solid boundary. NIf position.',IER)
        if(IER.NE.0) goto 996
        ISBf(I,ICFD) = IVAL+1
        IVCELLS(NVOL(ICFD),ICFD,2)=ISBf(I,ICFD)

C Ensure `east' and `west' boundaries properly placed.
        if( SURFAC.EQ.'W' .or. SURFAC.EQ.'w' .or.
     &      SURFAC.EQ.'E' .or. SURFAC.EQ.'e' )then
          if( ISBi(I,ICFD).ne.ISBf(I,ICFD) )then
            write(outs,'(a,a)') ' NIi must be equal to NIf when wall ',
     &                          'is placed on west or east plane. '
            call EDISP(IUOUT,outs)
            IER=-1
            goto 996
          endif
        endif

C Y-axis location.
        call EGETWI(OUTSTR,K,IVAL,1,NTCY,'F',
     &              ' Solid boundary. NJi position.',IER)
        if(IER.NE.0) goto 996
        JSBi(I,ICFD) = IVAL+1
        JVCELLS(NVOL(ICFD),ICFD,1)=JSBi(I,ICFD)
        Ist = IVAL
        call EGETWI(OUTSTR,K,IVAL,Ist,NTCY,
     &              'F',' Solid boundary. NJf position',IER)
        IF(IER.NE.0) goto 996
        JSBf(I,ICFD) = IVAL+1
        JVCELLS(NVOL(ICFD),ICFD,2)=JSBf(I,ICFD)

C Ensure `north' and `south' boundaries properly placed.
        if( SURFAC.EQ.'S' .or. SURFAC.EQ.'s' .or.
     &      SURFAC.EQ.'N' .or. SURFAC.EQ.'n' )then
          if( JSBi(I,ICFD).ne.JSBf(I,ICFD) )then
            write(outs,'(a,a)') ' NJi must be equal to NJf when wall ',
     &                          'is placed on south or north plane. '
            call EDISP(IUOUT,outs)
            IER=-1
            goto 996
          endif
        endif

C Z-axis location.
        call EGETWI(OUTSTR,K,IVAL,1,NTCZ,'F',
     &              ' Solid boundary. NKi position.',IER)
        if(IER.NE.0) goto 996
        KSBi(I,ICFD) = IVAL+1
        KVCELLS(NVOL(ICFD),ICFD,1)=KSBi(I,ICFD)
        Ist=IVAL
        call EGETWI(OUTSTR,K,IVAL,Ist,NTCZ,
     &              'F',' Solid boundary. NKf position',IER)
        if(IER.NE.0) goto 996
        KSBf(I,ICFD) = IVAL+1
        KVCELLS(NVOL(ICFD),ICFD,2)=KSBf(I,ICFD)

C Ensure `low' and `high' boundaries properly placed.
        if( SURFAC.EQ.'L' .or. SURFAC.EQ.'l' .or.
     &      SURFAC.EQ.'H' .or. SURFAC.EQ.'h' )then
          if( KSBi(I,ICFD).ne.KSBf(I,ICFD) )then
            write(outs,'(a,a)') ' NKi must be equal to NKf when wall ',
     &                          'is placed on low or high plane. '
            call EDISP(IUOUT,outs)
            IER=-1
            goto 996
          endif
        endif

C Read in value (numeric or ESP-r surface name) of boundary condition.
        if( IFCFD(ICP).ne.1 .and. IFCFD(ICP).ne.2
     &      .and. IFCFD(ICP).ne.4 .and. IFCFD(ICP).ne.5
     &      .and. IFCFD(ICP).ne.6 .and. IFCFD(ICP).ne.7 )then

C No thermal conflation: read prescribed temperature or heat flow.
          call EGETWR(OUTSTR,K,VAL,-10000.,10000.,'F',
     &                'Solid boundary BC value',IER)
          if(IER.NE.0) goto 995
          SSB(I,ICFD)=VAL
          ISBND=ISBND+1
          ISUFLC(I,ICFD)=ISBND
          if (IVTYPE(NVOL(ICFD),ICFD).eq.1) then
            VOLTemp(NVOL(ICFD),ICFD)=VAL
          elseif (IVTYPE(NVOL(ICFD),ICFD).eq.2) then
            VOLHeat(NVOL(ICFD),ICFD)=VAL
          endif
        else

C Thermal conflation: map to ESP-r surface.
          call EGETW(OUTSTR,K,WORD,'W',' Surface name',IER)
          if(IER.NE.0) goto 995
          SURF=WORD(1:12)
          VCsurf(NVOL(ICFD),ICFD)=SURF
          ISUFLC(I,ICFD)=IDSURF(SURF)
        endif

C Read data on next solid boundary.
   20 CONTINUE

C All solid boundaries set. Now check consistency of thermal conflation types.
C Some types may not be used on combination with others within the same zone.

C `Integrated' conflation cannot be mixed with `surface' conflation.
      iINTEG = 0
      DO 30 I=1,NSB(ICFD)
        if(ITCtype(I,ICFD).eq.13 .or. ITCtype(I,ICFD).eq.14) iINTEG = 1
   30 CONTINUE
      IF(iINTEG.eq.1)THEN
        do 31 I=1,NSB(ICFD)
          if(ITCtype(I,ICFD).ne.13 .or. ITCtype(I,ICFD).ne.14) goto 994
   31   continue
      ENDIF

C The MIT zero-equation model cannot be mixed with the k-epsilon model.
      iMIT = 0
      DO 40 I=1,NSB(ICFD)
        if(ITCtype(I,ICFD).eq.2 .or. ITCtype(I,ICFD).eq.10) iMIT = 1
   40 CONTINUE
      IF(iMIT.eq.1)THEN
        do 41 I=1,NSB(ICFD)
          if(ITCtype(I,ICFD).ne.2 .and. ITCtype(I,ICFD).ne.10) goto 993
   41   continue
      ENDIF

C `One-way' and `two-way' `surface' conflation cannot be mixed on the same
C ESP-r surface.
      i1or2way=0
      DO 50 ii=1,NSB(ICFD)

C Get ESP-r surface number for this CFD solid boundary. Determine whether
C the type of this CFD solid boundary is one-way or two-way.
        ISii=ISUFLC(ii,ICFD)
        if(ITCtype(ii,ICFD).ge.1.and.ITCtype(ii,ICFD).le.8) i1or2way=1
        if(ITCtype(ii,ICFD).ge.9.and.ITCtype(ii,ICFD).le.12)i1or2way=2

C Check whether any other CFD solid boundaries correspond to the same ESP-r
C surface.
        do 51 jj=1,NSB(ICFD)
          ISjj=ISUFLC(jj,ICFD)
          if(ISii.eq.ISjj)then

C Make sure the BC types are consistent (1-way or 2-way) for the two
C CFD solid boundaries sharing the same ESP-r surface.
            if(i1or2way.eq.1 .and.
     &.not.(ITCtype(jj,ICFD).ge.1.and.ITCtype(jj,ICFD).le.8)) goto 992
            if(i1or2way.eq.2 .and.
     &.not.(ITCtype(jj,ICFD).ge.9.and.ITCtype(jj,ICFD).le.12)) goto 992
          endif
   51   continue
   50 CONTINUE

C Successfully completed setting of solid boundaries.
      GOTO 100

C Traps for reading and data errors.
  991 call edisp(IUOUT,' Error: thermal conflation cannot be used with')
      call edisp(IUOUT,' laminar flow.')
      goto 999
  992 call edisp(IUOUT,' Error: One-way and two-way surface conflation')
      call edisp(IUOUT,' cannot be mixed on the same building surface.')
      goto 999
  993 call edisp(IUOUT,' Error: The MIT zero-equation turbulence model')
      call edisp(IUOUT,' cannot be used in the same zone as the')
      call edisp(IUOUT,' k-epsilon turbulence model.')
      goto 999
  994 call edisp(IUOUT,' Error: Two-way integrated conflation cannot')
      call edisp(IUOUT,' be mixed with other conflation types.')
      goto 999
  995 call edisp(IUOUT,' Value of thermal BC incorrect.')
      goto 999
  996 call edisp(IUOUT,' Location of solid boundary incorrect.')
      goto 999
  997 call edisp(IUOUT,' Thermal conflation type incorrect.')
      goto 999
  998 call edisp(IUOUT,' Solid boundary BC type incorrect.')
      goto 999
  999 CALL edisp(IUOUT,' CFDDAT error reading this line from cfd file:')
      CALL EDISP(IUOUT,OUTSTR)
      goto 100
 1000 CALL USRMSG(' Conversion error in',OUTSTR,'W')
      goto 100

  100 RETURN
      END


C ********************* SETGEO *********************
C SETGEO - sets the geometry for the CFD domain in terms of ESP-r's standard
C  geometry model.  By so doing, it fills the `G1' common block.
C  The geometry is given in the site co-ordinate system,
C  with the origin at the lower southwest corner of the CFD domain.
      SUBROUTINE SETGEO
#include "cfd.h"
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"

      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 

C This is currently hard-wired for cartesian domains.
C NTV is the number of vertex points.
C NSUR is the number of surfaces.
      NTV = 8
      NSUR = 6

C Xmax,Ymax,Zmax are the dimensions of the problem in the i, j, and 
C k directions.
      Xmax = XU(NI)
      Ymax = YV(NJ)
      Zmax = ZW(NK)

C Set values in RAY5
      XMN=0.
      XMX=Xmax
      YMN=0.
      YMX=Ymax
      ZMN=0.
      ZMX=Zmax
      ZCOG(1,1)=XMX/2.
      ZCOG(1,2)=YMX/2.
      ZCOG(1,3)=ZMX/2.

C Set view point to COG.
      VIEWM(1)=ZCOG(1,1)
      VIEWM(2)=ZCOG(1,2)
      VIEWM(3)=ZCOG(1,3)

C The origin is at the lower southwest corner of the CFD domain.  
C The remaining vertex points are calculated based on the dimensions 
C read from dfs' flow-vector output file.
      X(1) = 0.
      Y(1) = 0.
      Z(1) = 0.
      X(2) = Xmax
      Y(2) = 0.
      Z(2) = 0.
      X(3) = Xmax
      Y(3) = Ymax
      Z(3) = 0.
      X(4) = 0.
      Y(4) = Ymax
      Z(4) = 0.
      X(5) = 0.
      Y(5) = 0.
      Z(5) = Zmax
      X(6) = Xmax
      Y(6) = 0.
      Z(6) = Zmax
      X(7) = Xmax
      Y(7) = Ymax
      Z(7) = Zmax
      X(8) = 0.
      Y(8) = Ymax
      Z(8) = Zmax

C The south face is surface 1.
      NVER(1) = 4
      JVN(1,1) = 1
      JVN(1,2) = 2
      JVN(1,3) = 6
      JVN(1,4) = 5

C The east face is surface 2.
      NVER(2) = 4
      JVN(2,1) = 2
      JVN(2,2) = 3
      JVN(2,3) = 7
      JVN(2,4) = 6

C The north face is surface 3.
      NVER(3) = 4
      JVN(3,1) = 3
      JVN(3,2) = 4
      JVN(3,3) = 8
      JVN(3,4) = 7

C The west face is surface 4.
      NVER(4) = 4
      JVN(4,1) = 4
      JVN(4,2) = 1
      JVN(4,3) = 5
      JVN(4,4) = 8

C The ceiling is surface 5.
      NVER(5) = 4
      JVN(5,1) = 5
      JVN(5,2) = 6
      JVN(5,3) = 7
      JVN(5,4) = 8

C The floor is surface 6.
      NVER(6) = 4
      JVN(6,1) = 1
      JVN(6,2) = 2
      JVN(6,3) = 3
      JVN(6,4) = 4

      return
      end
