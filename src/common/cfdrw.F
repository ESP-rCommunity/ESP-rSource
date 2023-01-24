C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C DFDSV:     Saves the zone cfd description file (version 2).
C DFDREAD:   Reads the zone cfd description file (version 2,2.1 and 2.2)
C NEW2OLD:   Copies new data structure to old.
C CFMFSU:    Set up CFD to mass flow connections/components.
C MFLISTemp: List common block contents for debugging.
C CFDDEFLT:  Sets default values for all solution equation data items.
C SCHCHK:    Checks the turbulent Schmidt number of containments.
C BCEKCK:    Checks the boundary conditions of boundary cells  
C RESETCFD:  Clear CFD commons that are independent of domain and zone.
C CFDLIST:   Lists available CFD domain files and their zone 
C            associations.


C ******************** DFDSV ********************
C Saves the room air movement description file (version 2).
C IUF   - unit number for CFD input file.
C IZONE - the index of the zone with a CFD domain.
C IER=0 - indicates no error.

      SUBROUTINE DFDSV(IUF,IZONE,IER)
#include "building.h"
#include "cfd.h"

C Passed parameters.
      integer iuf,izone,ier

      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      common/CLVTXNO/ivxhigh(MV),ivxlow(MV),ihigh,ilow
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)
      
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ICFNOD/ICFD,ICP

C CALLU (logical) - U equation to be solved.
C CALLV (logical) - V equation to be solved.
C CALLW (logical) - W equation to be solved.
C CALLT (logical) - temperature equation to be solved.
C CALLC (logical) - depreciated variable, replaced by CALPOL.
C KEMDL (logical) - k-epsilon turbulence equations to be solved.
C BUOY (logical)  - buoyancy equation to be solved.
C BOUSSI (logical)- use Boussinesq approximation.
C ZEROT (logical) - use zero-equation, fixed-eddy-viscosity turbulence.
C ZandKE (logical)- use fixed eddy viscosity then k-epsilon.
C MITzero (logical)- use the zero-equation model.
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)

C CALPOL Calculate contaminant concentration
C CALLMA (logical) calculate local mean age of air.
C NCTM total number of contaminants in a model
C JHUMINDX contaminant number representing water (humidity)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C NVOL    - number of key volumes.
C IVOLF   - interesting region of volume (e.g. a face, whole volume).
C IVCELLS - start and end cell in x direction.
C JVCELLS - start and end cell in y direction.
C KVCELLS - start and end cell in z direction.
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)

C VOLNAME - name of key volume.
C VCsurf  - zone surface name for thermal conflation.
C BLKSURF - surface names for blockage conflation.
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12

C IVTYPE  - volume boundary condition (BC) type (e.g. source, velocity).
C VOLTemp - BC temperature if applicable.
C VOLHeat - BC heat flux if applicable.
C IVConfl - surface handshaking if applicable.
C VOLHum  - BC humidity if applicable.
C VOLCO2  - BC CO2 generation above ambient if applicable.
C VOLPol  - BC pollutant generation above ambient if applicable.
C VOLVel  - BC air velocity if applicable.
C VOLDir  - BC air velocity direction vectors if applicable.
C VOLArea - BC small opening area if applicable.
C VOLPres - BC volume pressure if applicable.
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)
      common/cfdini/INITFL(MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho

C ICFDNOD - mass flow network node representative of CFD domain.
C ICFDCNN - list of mfs connections representative of CFD domain openings.
      common/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)

C ICTDFAF - 0, constant source strengths are defined.
C         - 1, contamiant sources defined in contaminants network are
C           used as sources within CFD domain (it is still possible to 
C           fall back to static source strengths in CFD standalone mode).
C SRCE    - source name defined in contaminant network model that is 
C           used as source type BC within CFD domain.
C ICCSRC  - 1, opening type BC also will act as source of contaminants
C           in or out of CFD domain (only possible with mfs/dfs conflation).
C ICC2NC  - maps CFD contaminants to network contaminants.
C SRCFRC  - fraction of network source released at this BC.
      COMMON/CTDFAF/ICTDFAF(MNZ),SRCE(MNVLS,MCTM,MNZ),ICCSRC(MNZ),
     &              ICC2NC(MCTM,MNZ),SRCFRC(MNVLS,MCTM,MNZ)

C IDCasgn - identification number of casual gain type assocated with
C           key volume.
C FCasgn  - fraction of this casual gain emitted to the key volume.
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)

C SCHMT  - turbulent Schmidt number for contaminants (= 2*Schmidt number).
C GFM    - gram formula mass for contaminants.
C VCRIT  - specific volume at critical point for contaminants.
C TBOIL  - boiling point for contaminants.
C TCRIT  - critical point for contaminants.
C ISCHMT - 0, specified as a constant.
C        - 1, parametric specification for SCHMT, i.e. from critical
C          and other properties.     
      COMMON/SCHMTT/SCHMT(MCTM,MNZ),GFM(MCTM,MNZ),VCRIT(MCTM,MNZ),
     &       TBOIL(MCTM,MNZ),TCRIT(MCTM,MNZ),ISCHMT(MCTM,MNZ)

      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALPOL,CALLMA
      LOGICAL INITFL,PNAMWRT

      character LCFD*72,POLNAM*12,PNMSTR*256
      character PCCSTR*256,PCCSTRA*26,PCCSTRB*18
      CHARACTER*72 TITLE,CFTRFL,LPHI

      character FACE*6, SRCE*12

C Reset error condition flag.
      IER=0
      CALL EFOPSEQ(IUF,LCFD(izone),4,IER)

C File header and conflation.
      write(IUF,'(a)',IOSTAT=ios,ERR=3)
     &  '*DFS V2.3 # Zone CFD domain description.'
      if(IFCFD(izone).eq.0)then
        write(IUF,'(a,i2,a)',IOSTAT=ios,ERR=3)'*conflation',
     &    IFCFD(izone),
     &    '  # for use with stand-alone CFD solver'
      elseif(IFCFD(izone).eq.1)then
        write(IUF,'(a,i2,a)',IOSTAT=ios,ERR=3)'*conflation',
     &    IFCFD(izone),
     &    '  # zone surface convection from CFD solution'
      elseif(IFCFD(izone).eq.2)then
        write(IUF,'(a,i2,a)',IOSTAT=ios,ERR=3)'*conflation',
     &    IFCFD(izone),
     &    '  # zone surface convection and air temp. from CFD solution'
      elseif(IFCFD(izone).eq.3)then
        write(IUF,'(a,i2,a)',IOSTAT=ios,ERR=3)'*conflation',
     &    IFCFD(izone),
     &    '  # network flow and CFD solvers cooperate'
      else
        write(IUF,'(a,i2,a)',IOSTAT=ios,ERR=3)'*conflation',
     &    IFCFD(izone),
     &    '  # CFD model temporally adapted'
      endif

C Title, date and documentation.
      write(IUF,'(2a)',IOSTAT=ios,ERR=3) '*title ',TITLE(ICFD)
C     write(IUF,'(a)',IOSTAT=ios,ERR=3)
C     &  '# user supplied notes for this domain'
C      do 10 id=1,??
C        write(IUF,'(2a)',IOSTAT=ios,ERR=3)
C     &    '*doc ',??(id)(1:lnblnk(??(id)))
C 10    continue

C Geometry and gridding.
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*thermal_geom'
      if(ortho)then
        write(IUF,'(a,4i4)',IOSTAT=ios,ERR=3) 
     &    '*vrts ',IORG(ICFD),ixend(ICFD),iyend(ICFD),izend(ICFD)
      else
        write(IUF,'(a,4i4)',IOSTAT=ios,ERR=3)'*vrts ',IORG(ICFD),
     &    ixend(ICFD),iyend(ICFD),izend(ICFD),izende(ICFD)
        write(IUF,'(a,4i4)',IOSTAT=ios,ERR=3) 
     &    '*high ',(ivxhigh(IIX),IIX=1,ihigh)
        write(IUF,'(a,4i4)',IOSTAT=ios,ERR=3) 
     &    '*low ',(ivxlow(IIX),IIX=1,ilow)
      endif

      write(IUF,'(a)',IOSTAT=ios,ERR=3) '# Ze is 0 if orthogonal'
      write(IUF,'(a,4i4,a)',IOSTAT=ios,ERR=3) '*regions ',NREG(1,ICFD),
     &  NREG(2,ICFD),NREG(3,ICFD),NREG(4,ICFD),
     &  '  # regions in X Y Zw Ze directions'
      write(IUF,'(a)',IOSTAT=ios,ERR=3) 
     &  '# -ve cell number indicates symmetrical gridding.'
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*xregion'
      do 20 ix=1,NREG(1,ICFD)
        write(IUF,'(i4,2f8.3,a)') NCELX(ix,ICFD),XREG(ix,ICFD),
     &    Xplaw(ix,ICFD),' # no. cells, length, power law coefficient'
 20   continue
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*yregion'
      do 30 ix=1,NREG(2,ICFD)
        write(IUF,'(i4,2f8.3,a)') NCELY(ix,ICFD),YREG(ix,ICFD),
     &    Yplaw(ix,ICFD),' # no. cells, length, power law coefficient'
 30   continue
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*zwregion'
      do 40 ix=1,NREG(3,ICFD)
        write(IUF,'(i4,2f8.3,a)') NCELZ(ix,ICFD),ZREG(ix,ICFD),
     &    Zplaw(ix,ICFD),' # no. cells, length, power law coefficient'
 40   continue

      if(NREG(4,ICFD).gt.0)then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) '*zeregion'
        do 50 ix=1,NREG(4,ICFD)
          write(IUF,'(i4,2f8.3,a)') NCELZE(ix,ICFD),ZREGE(ix,ICFD),
     &     Zplawe(ix,ICFD),' # no. cells, length, power law coefficient'
 50     continue
      endif

C Volumes within the domain.
      write(IUF,'(a)',IOSTAT=ios,ERR=3) 
     &  '# Volumes within the CFD domain.'
      write(IUF,'(a,i4)',IOSTAT=ios,ERR=3)'*volumes ',NVOL(ICFD)
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'# name  type  Is If Js Jf Ks Kf'
      do 60 I=1,NVOL(ICFD)
        if (IVOLF(I,ICFD).eq.1) then
          FACE='West'
        elseif (IVOLF(I,ICFD).eq.2) then
          FACE='East'
        elseif (IVOLF(I,ICFD).eq.3) then
          FACE='South'
        elseif (IVOLF(I,ICFD).eq.4) then
          FACE='North'
        elseif (IVOLF(I,ICFD).eq.5) then
          FACE='Low'
        elseif (IVOLF(I,ICFD).eq.6) then
          FACE='High'
        elseif (IVOLF(I,ICFD).eq.7) then
          FACE='Whole'
        elseif (IVOLF(I,ICFD).eq.8) then
          FACE='Block'
        elseif (IVOLF(I,ICFD).eq.9) then
          FACE='Source'
        endif
        write(IUF,'(3a,6i4)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),' ',FACE,
     &         (IVCELLS(I,ICFD,L)-1,L=1,2),(JVCELLS(I,ICFD,L)-1,L=1,2),
     &         (KVCELLS(I,ICFD,L)-1,L=1,2)
 60   continue

C Boundary conditions.
      write(IUF,'(a)',IOSTAT=ios,ERR=3) 
     &  '# Solid boundary conditions.'
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'*solids '
      do 70 I=1,NVOL(ICFD)
        if (IVTYPE(I,ICFD).eq.1) then
          write(IUF,'(2a,f7.2)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),
     &           ' Temp',VOLTemp(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.2) then
          write(IUF,'(2a,f10.2)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),
     &           ' Heat',VOLHeat(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.3) then
          write(IUF,'(2a)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),
     &           ' Symmetrical'
        elseif (IVTYPE(I,ICFD).eq.4) then
          write(IUF,'(2a,f7.2,a,i3,1x,a)',IOSTAT=ios,ERR=3)
     &            VOLNAME(I,ICFD),' Temp',VOLTemp(I,ICFD),' | Confl ',
     &            IVConfl(I,ICFD),VCsurf(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.5) then
          write(IUF,'(2a,f10.2,a,i3,1x,a)',IOSTAT=ios,ERR=3)
     &            VOLNAME(I,ICFD),' Heat',VOLHeat(I,ICFD),' | Confl ',
     &            IVConfl(I,ICFD),VCsurf(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.6) then
          write(IUF,'(2a,i3,1x,a)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),
     &           ' Symmetrical | Confl ',IVConfl(I,ICFD),VCsurf(I,ICFD)
        endif
 70   continue

      write(IUF,'(a)',IOSTAT=ios,ERR=3) 
     &  '# Air flow boundary conditions.'
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'*air_flow '
      do 80 I=1,NVOL(ICFD)
        IF(ICFMFS(ICFD).EQ.0)THEN
          if (IVTYPE(I,ICFD).eq.10) then
            write(IUF,'(2a,f6.2)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),
     &             ' Pressure',VOLPres(I,ICFD)
          elseif (IVTYPE(I,ICFD).eq.11) then
            write(IUF,'(a,5(a,f7.3),f7.3)',IOSTAT=ios,ERR=3) 
     &           VOLNAME(I,ICFD),' Velocity',VOLVel(I,ICFD),
     &           ' Temp',VOLTemp(I,ICFD),' Hum',VOLHum(I,ICFD),
     &           ' Area',VOLArea(I,ICFD),' Ang',(VOLDir(I,ICFD,L),L=1,2)
          elseif (IVTYPE(I,ICFD).eq.12) then
            write(IUF,'(2a)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),' Zero'
          elseif (IVTYPE(I,ICFD).eq.13) then
            write(IUF,'(2a)',IOSTAT=ios,ERR=3) VOLNAME(I,ICFD),' Mass'
          endif
        ELSEIF(ICFMFS(ICFD).EQ.1)THEN
          if (IVTYPE(I,ICFD).eq.10) then
            write(IUF,'(2a,f6.2,1X,I4,1X,I4)',IOSTAT=ios,ERR=3) 
     &           VOLNAME(I,ICFD),' Pressure',VOLPres(I,ICFD),
     &           ICFDNOD(ICFD),ICFDCNN(I,ICFD)
          elseif (IVTYPE(I,ICFD).eq.11) then
            write(IUF,'(a,5(a,f7.3),f7.3,1X,I4,1X,I4)',IOSTAT=ios,ERR=3) 
     &           VOLNAME(I,ICFD),' Velocity',VOLVel(I,ICFD),
     &           ' Temp',VOLTemp(I,ICFD),' Hum',VOLHum(I,ICFD),
     &           ' Area',VOLArea(I,ICFD),' Ang',(VOLDir(I,ICFD,L),L=1,2)
     &           ,ICFDNOD(ICFD),ICFDCNN(I,ICFD)
          elseif (IVTYPE(I,ICFD).eq.12) then
            write(IUF,'(2a,1X,I4,1X,I4)',IOSTAT=ios,ERR=3) 
     &           VOLNAME(I,ICFD),' Zero'
     &           ,ICFDNOD(ICFD),ICFDCNN(I,ICFD)
          elseif (IVTYPE(I,ICFD).eq.13) then
            write(IUF,'(2a,1X,I4,1X,I4)',IOSTAT=ios,ERR=3) 
     &           VOLNAME(I,ICFD),' Mass'
     &           ,ICFDNOD(ICFD),ICFDCNN(I,ICFD)
          endif
        ENDIF
 80   continue
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '# Internal sources.'
      write(IUF,'(a,I3,a,I3,a)',IOSTAT=ios,ERR=3) '*contaminants( ',
     &  NCTM(ICFD),' contaminants, humdity is contaminant number ',
     &  JHUMINDX(ICFD),' )'
      DO 156 ICTM=1,NCTM(ICFD)
        IF(ISCHMT(ICTM,ICFD).EQ.0)THEN
          WRITE(IUF,'(A,1X,I3,1X,F9.4,1x,i1)')POLNAM(ICTM,ICFD),0,
     &      SCHMT(ICTM,ICFD),ICC2NC(ictm,ICFD)
        ELSEIF(ISCHMT(ICTM,ICFD).EQ.1)THEN
          WRITE(IUF,'(A,1X,I3,1X,4F6.1,1x,i1)')POLNAM(ICTM,ICFD),1,
     &         GFM(ICTM,ICFD),VCRIT(ICTM,ICFD),TBOIL(ICTM,ICFD),
     &         TCRIT(ICTM,ICFD),ICC2NC(ictm,ICFD)
        ENDIF
 156  CONTINUE
      PNAMWRT=.FALSE.
      do 90 I=1,NVOL(ICFD)
        if (IVTYPE(I,ICFD).eq.20) then
          IF(.NOT.PNAMWRT)THEN
            CALL ASLIST2(1,NCTM(ICFD),POLNAM,MCTM,MNZ,ICFD,' ',PNMSTR,
     &        ILP,ILT)
            write(IUF,'(4a)',IOSTAT=ios,ERR=3) '*volume name, ',
     &        'heat source/fraction, cas gain index, ',
     &        'occ type, source rates/fraction and names for: ',PNMSTR
            PNAMWRT=.TRUE.
          ENDIF
          NLEN=0
          PCCSTR=' '
          PCCSTRA=' '

C Assemble string of contaminant release directives.
          DO 91 ICTM=1,NCTM(ICFD)
            IF(SRCE(I,ICTM,ICFD)(:1).NE.' '.and.ICTDFAF(ICFD).EQ.1)THEN ! linked with network source
              CALL RELSTR(SRCFRC(I,ICTM,ICFD),PCCSTRA,ILN,IER)
              PCCSTRB=SRCE(I,ICTM,ICFD)
            elseif (VCsurf(I,ICFD).ne.'none' .and.
     &              ictm.eq.JHUMINDX(ICFD)) then ! occupant moisture release
              CALL RELSTR(1.0,PCCSTRA,ILN,IER)
              PCCSTRB='occupant'
            ELSE ! prescribed release rate
              CALL RELSTR(VOLPOL(ICTM,I,ICFD),PCCSTRA,ILN,IER)
              PCCSTRB='none'
            ENDIF
            WRITE(PCCSTRA,'(A,1X,A)')PCCSTRA(1:ILN),
     &        PCCSTRB(1:LNBLNK(PCCSTRB))
            WRITE(PCCSTR,'(A,1X,A)')PCCSTR(1:NLEN),PCCSTRA
            NLEN=NLEN+ILN+1+LNBLNK(PCCSTRB)
 91       CONTINUE
            
C Assemble string of heat release directives.
          if (IDCASGN(i,ICFD).gt.0) then
            write(PCCSTRB,'(F7.2,1X,I3)')FCASGN(i,ICFD),IDCASGN(i,ICFD)
          else
            write(PCCSTRB,'(F7.2,1X,I3)')VOLHEAT(i,ICFD),IDCASGN(i,ICFD)
          endif

C Write line.
          write(IUF,'(A,1X,A,1X,A,1X,A)',IOSTAT=ios,ERR=3)
     &      VOLNAME(I,ICFD),PCCSTRB(1:11),VCsurf(I,ICFD),
     &      PCCSTR(1:NLEN+1)
        endif
 90   continue
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '# Domain blockages.'
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*blockages'
      do 100 I=1,NVOL(ICFD)
        if (IVTYPE(I,ICFD).eq.30) then
          write(IUF,'(2a,f12.2)',IOSTAT=ios,ERR=3) 
     &      VOLNAME(I,ICFD),' Heat ',VOLHeat(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.31) then
          write(IUF,'(2a,f12.2)',IOSTAT=ios,ERR=3) 
     &      VOLNAME(I,ICFD),' Temp ',VOLTemp(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.32) then
          write(IUF,'(2a,f8.2,2a)',IOSTAT=ios,ERR=3)     
     &      VOLNAME(I,ICFD),' Temp ',VOLTemp(I,ICFD),' | Confl ',
     &      VCsurf(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.33) then
          write(IUF,'(2a,f8.2,7a)',IOSTAT=ios,ERR=3)     
     &      VOLNAME(I,ICFD),' Temp ',VOLTemp(I,ICFD),' | Confl ',
     &      BLKSURF(I,ICFD,1),BLKSURF(I,ICFD,2),BLKSURF(I,ICFD,3),
     &      BLKSURF(I,ICFD,4),BLKSURF(I,ICFD,5),BLKSURF(I,ICFD,6)
        elseif (IVTYPE(I,ICFD).eq.34) then
          write(IUF,'(2a,f12.2,a,i3,1x,f4.2)',IOSTAT=ios,ERR=3) 
     &      VOLNAME(I,ICFD),' Heat ',VOLHeat(I,ICFD),' | CasGn ',
     &      IDcasgn(I,ICFD),Fcasgn(I,ICFD)
        elseif (IVTYPE(I,ICFD).eq.35) then
          write(IUF,'(2a,f12.2,2a,1x,a)',IOSTAT=ios,ERR=3) 
     &      VOLNAME(I,ICFD),' Heat ',VOLHeat(I,ICFD),' | Persn ',
     &      BLKSURF(I,ICFD,1),BLKSURF(I,ICFD,2)
        endif
 100  continue

C Solution methods.
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'*solution_methods'
      if (ITURB(ICFD).eq.0) then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) 'Turbulence 0 # ignored'
      elseif (ITURB(ICFD).eq.1) then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) 'Turbulence 1 # k-e model'
      elseif (ITURB(ICFD).eq.2) then
        write(IUF,'(a,f6.2,a)',IOSTAT=ios,ERR=3) 'Turbulence 2 ',
     &                            rMOOT(ICFD),' # fixed eddy viscosity'
      elseif (ITURB(ICFD).eq.3) then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) 'Turbulence 3 # zero equation'
      elseif (ITURB(ICFD).eq.4) then
        write(IUF,'(a,f6.2,i5,a)',IOSTAT=ios,ERR=3) 'Turbulence 4 ',
     &        rMOOT(ICFD),nZtoKE(ICFD),' # fixed eddy viscosity -> k-e'
      endif
      if (IBUOY(ICFD).eq.0) then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) 'Buoyancy 0 # ignored'
      elseif (IBUOY(ICFD).eq.1) then
        write(IUF,'(a,f6.4,a)',IOSTAT=ios,ERR=3)'Buoyancy 1 ',
     &      URFCD(ICFD),' # included, density linear relax. factor'
      elseif (IBUOY(ICFD).eq.2) then
        write(IUF,'(a,f6.2,a)',IOSTAT=ios,ERR=3) 'Buoyancy 2 ',
     &                        Tbar(ICFD),' # Boussinesq approximation.'
      endif

C Equations to be solved, initial values and relaxation factors.
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'# Equations to be solved:'
      write(IUF,'(a)',IOSTAT=ios,ERR=3)'*solution_equations'
      write(IUF,'(a)',IOSTAT=ios,ERR=3)
     &  '#  Type  Initial value  Relaxation type and high/low factors'

C Pressure.
      write(IUF,'(a,f8.3,a,f6.3,1X,f6.3)',IOSTAT=ios,ERR=3) 
     &  'Pressure ',PINIT(ICFD),' Linear ',URFCP(ICFD),URFCP2(ICFD)

C Velocity.
      if(CALLU(ICFD))then
        write(IUF,'(a,G10.3,a,f6.3,1X,f6.3)',IOSTAT=ios,ERR=3) 
     &    'Vel U ',UINIT(ICFD),' Linear ',URFCU(ICFD),URFCU2(ICFD)
      endif
      if(CALLV(ICFD))then
        write(IUF,'(a,G10.3,a,f6.3,1X,f6.3)',IOSTAT=ios,ERR=3) 
     &    'Vel V ',VINIT(ICFD),' Linear ',URFCV(ICFD),URFCV2(ICFD)
      endif
      if(CALLW(ICFD))then
        write(IUF,'(a,G10.3,a,f6.3,1X,f6.3)',IOSTAT=ios,ERR=3) 
     &    'Vel W ',WINIT(ICFD),' Linear ',URFCW(ICFD),URFCW2(ICFD)
      endif

C Temperature.
      if(CALLT(ICFD))then
        write(IUF,'(a,f8.3,a,f6.3,1X,f6.3)',IOSTAT=ios,ERR=3) 
     &    'Temp ',TINIT(ICFD),' Linear ',URFCT(ICFD),URFCT2(ICFD)
      endif

C Turbulence.
      write(IUF,'(2(a,G10.3),1X,G10.3)',IOSTAT=ios,ERR=3) 
     &    'Ted ',TEINIT(ICFD),' Linear ',URFCK(ICFD),URFCK2(ICFD)
      write(IUF,'(2(a,G10.3),1X,G10.3)',IOSTAT=ios,ERR=3) 
     &    'Epd ',EDINIT(ICFD),' Linear ',URFCE(ICFD),URFCE2(ICFD)

C Concentration.
      DO 123 ICTM=1,NCTM(ICFD)
        IF(CALPOL(ICTM,ICFD))THEN
          write(IUF,'(a,1x,G10.3,a,G10.3,1X,G10.3)',IOSTAT=ios,ERR=3)
     &      POLNAM(ICTM,ICFD)(1:LNBLNK(POLNAM(ICTM,ICFD))),
     &      POLINIT(ICFD,ICTM),' Linear  ',URFCC(ICFD,ICTM),
     &      URFCC2(ICFD,ICTM)
        ENDIF
 123  CONTINUE

C Local mean age of air.
      if(CALLMA(ICFD))then
        write(IUF,'(a)',IOSTAT=ios,ERR=3)
     &       'Lmage  n/a Linear  n/a # currently 0.9 is always assumed'
      endif
     
C Convergance criteria.
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '# Iteration control.'
      write(IUF,'(a)',IOSTAT=ios,ERR=3) '*iteration '
      write(IUF,'(i6,G12.4,a)',IOSTAT=ios,ERR=3) MAXITR(ICFD),
     &                     SRMAX(ICFD),' No'
      write(IUF,'(a,3i4,a)',IOSTAT=ios,ERR=3)  '*monitor ',
     &  IMONT(ICFD)-1,JMONT(ICFD)-1,KMONT(ICFD)-1,'  ../tmp/CFD.mon'
      if (INITFL(ICFD)) then
        write(IUF,'(a)',IOSTAT=ios,ERR=3) '*reint  YES'
      else
        write(IUF,'(a)',IOSTAT=ios,ERR=3) '*reint  NO'
      endif
      write(IUF,'(a)',IOSTAT=ios,ERR=3) 
     &  '*svf  1  # frequency of results writing'

C Close file.
      CALL ERPFREE(IUF,ISTAT)

      return

 3    if(IOS.eq.2)then
        CALL USRMSG('File permission error in ',LCFD(izone),'W')
      else
        CALL USRMSG('File write error in ',LCFD(izone),'W')
      endif
      IER=1
      CALL ERPFREE(IUF,ISTAT)
      return

      end


C ******************** DFDREAD *********************
C Reads the CFD domain configuration file (versions 2,2.1 and 2.2).
C IZONE - index of the model zone.
C itrc  - signals verbose reporting if greater than zero.
C itu   - the unit to send reports to
C IER   - set to one if file cannot be read, otherwise negative if
C         there is an error while scanning file.

      SUBROUTINE DFDREAD(IZONE,itrc,itu,IER)
#include "building.h"
#include "cfd.h"
#include "net_flow.h"
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

C Passed parameters.
      integer izone,itrc,itu,ier

C Grid and geometry definition:
C  NREG(?,ICFD)      - Number of regions in each direction(?: 1- x, 2- y, 3- z).
C  NCELX(IREG,ICFD)  - Number of cells in x-direction per region(IREG) per
C                      domain(ICFD).
C  NCELY(IREG,ICFD)  - Number of cells in y-direction per region per domain(ICFD).
C  NCELZ(IREG,ICFD)  - Number of cells in z-direction per region per domain(ICFD.
C  NCELZe(IREG,ICFD) - Number of cells in z (east)-direction.
C  XREG(IREG,ICFD)   - x-dimension of the region IREG per domain (ICFD).
C  YREG(IREG,ICFD)   - y-dimension of the region IREG per domain (ICFD).
C  ZREG(IREG,ICFD)   - z-dimension of the region IREG per domain (ICFD).
C  ZREGe(IREG,ICFD)  - z (east) -dimension of the region IREG per domain (ICFD).
C  Xplaw(IREG,ICFD)  - x-power law coefficient of region IREG per domain (ICFD).
C  Yplaw(IREG,ICFD)  - y-power law coefficient of region IREG per domain (ICFD). 
C  Zplaw(IREG,ICFD)  - West wall z-power law coef of region IREG per domain (ICFD).
C  Zplawe(IREG,ICFD) - East wall z-power law coef of region IREG per domain (ICFD)
C                      (used in the case of curvilinear Z)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      common/GRIDFN/NCELX(MNREG,MNZ),NCELY(MNREG,MNZ),NCELZ(MNREG,MNZ),
     &  NCELZE(MNREG,MNZ),XREG(MNREG,MNZ),YREG(MNREG,MNZ),
     &  ZREG(MNREG,MNZ),ZREGE(MNREG,MNZ),Xplaw(MNREG,MNZ),
     &  Yplaw(MNREG,MNZ),Zplaw(MNREG,MNZ),Zplawe(MNREG,MNZ),NREG(4,MNZ)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/FDTRFC/FLSDTU(MNZ),FLSDTV(MNZ),FLSDTW(MNZ),FLSDTP(MNZ),
     &              FLSDTT(MNZ),FLSDTK(MNZ),FLSDTE(MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      common/cfdini/INITFL(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/CONST/GREAT,small,GRAV
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

C Conflation.
C  ICFBLD(ICFD)=building conflation mechanism (not used at present)
C  ICFMFS(ICFD)=mass flow conflation mechanism (0=none, 1=active)
      common/cfdconf/ICFBLD(MNZ),ICFMFS(MNZ)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/MFS/IMFACT
      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/NSSWP/NSSWPU(MNZ),NSSWPV(MNZ),NSSWPW(MNZ),NSSWPP(MNZ),
     &             NSSWPT(MNZ),NSSWPK(MNZ),NSSWPE(MNZ)
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      common/grdmax/NTCX,NTCY,NTCZ

C Domain geometry.
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Boundary conditions.  
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)
      common/KEYCASGN/IDcasgn(MNVLS,MNZ),Fcasgn(MNVLS,MNZ)

C Common blocks for blockages and small supply openings.
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)

C Common block for calculation of turbulent Schmidt number for contaminants.
      COMMON/SCHMTT/SCHMT(MCTM,MNZ),GFM(MCTM,MNZ),VCRIT(MCTM,MNZ),
     &       TBOIL(MCTM,MNZ),TCRIT(MCTM,MNZ),ISCHMT(MCTM,MNZ)
      COMMON/CTDFAF/ICTDFAF(MNZ),SRCE(MNVLS,MCTM,MNZ),ICCSRC(MNZ),
     &              ICC2NC(MCTM,MNZ),SRCFRC(MNVLS,MCTM,MNZ)
      character SRCE*12
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      character CONTMNAM*12
     
      common/dynamico/isdynamicocup(MCOM)

      common/APPNAME/cAppName
      character cAppName*12

      logical origin,xgrid,ygrid,zgrid,zegrid,ortho
      LOGICAL INITFL,CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero
      LOGICAL CALLMA,CALPOL
      Character OUTSTR*124,WORD*124,POLNAM*12
      CHARACTER TITLE*72,CFTRFL*72,LPHI*72,LCFD*72
      CHARACTER TYPE*6,phrase*64
      CHARACTER outs*124,outs_pre*124

C Set volume counters to zero.
      NSSO(ICFD)=0
      NBLK(ICFD)=0

C Reset default values before reading in file.
      call CFDDEFLT

C Assume no conflation.
      ICFBLD(ICFD)=0
      ICFMFS(ICFD)=0

C Set the CFD zone flag (ICP) to current thermal zone id.
      ICP=izone
      IER=0
      IFL=IFIL+1
      write(currentfile,'(a)') LCFD(ICP)(1:lnblnk(LCFD(ICP)))
      CALL EFOPSEQ(IFL,LCFD(ICP),1,IER)

C Check whether the file is a CFD-input file version 2 or 2.1 and get 
C conflation type.      
      CALL STRIPC(IFL,OUTSTR,99,ND,1,'dfd line 1',IER)
      if(OUTSTR(1:7).eq.'*DFS V2')VER=2.0
      if(OUTSTR(1:9).eq.'*DFS V2.1')VER=2.1
      if(OUTSTR(1:9).eq.'*DFS V2.2')VER=2.2
      if(OUTSTR(1:9).eq.'*DFS V2.3')VER=2.3
      if(OUTSTR(1:7).eq.'*DFS V2')then
        CALL STRIPC(IFL,OUTSTR,99,ND,1,'dfd confla type',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','conflation tag',IER)
        if (WORD(1:11).ne.'*conflation') then
          call usrmsg('Cannot read conflation type!',
     &                'Abandoning reading .dfd file.','W')
          IER=1
          CALL ERPFREE(IFL,ISTAT)
          return
        endif
        CALL EGETWI(OUTSTR,K,IVAL,0,5,'W','Conflation type',IER)
        if (IER.eq.0) IFCFD(IZONE)=IVAL
        if(IFCFD(ICP).eq.1) IBLD=1
        if(IFCFD(ICP).eq.2) IBLD=1
        if(IFCFD(ICP).eq.4) IBLD=1
        if(IFCFD(ICP).eq.5) IBLD=1
      else
        call usrmsg('Referenced file not DFD description',' ','W')
        IER=1
        CALL ERPFREE(IFL,ISTAT)
        return
      endif

C Read title.
      TITLE(ICFD)='  '
      CALL STRIPC(IFL,OUTSTR,0,ND,1,' DFD title',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','title',IER)
      if (WORD(1:6).eq.'*title') then
        call EGETRM(OUTSTR,K,phrase,'W','DFD title text',IER)
        TITLE(ICFD)=phrase
      endif

C Text feedback.
      if(itrc.gt.0)then
        call edisp(itu,' ')
        if(ICP.gt.0)then
          write(outs,'(a,a)')'A CFD domain has been described for: ',
     &      zname(ICP)
          call edisp(itu,outs)
        else
          call edisp(itu,'A CFD domain has been included in the model.')
        endif
        iln=max(1,lnblnk(TITLE(ICFD)))
        write(outs,'(a,a)')'Description is: ',TITLE(ICFD)(1:iln)
        call edisp(itu,outs)
        write(outs,'(a,i1)')'Building-CFD conflation type ',IFCFD(ICP)
        outs_pre=outs
        if(IFCFD(ICP).eq.0)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                                     ': CFD only (no conflation)'
          call edisp(itu,outs)
          if(cAppName.ne.'dfs')then
            write(outs,'(2a)')'  WARNING: A domain with this ',
     &       'conflation type will not run alongside a building model.'
            call edisp(itu,outs)
            write(outs,'(2a)')'           Please use the domain flow ',
     &                   'solver module (DFS) to simulate this domain.'
            call edisp(itu,outs)
          endif
        elseif(IFCFD(ICP).eq.1)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                                           ': thermal conflation'
          call edisp(itu,outs)
        elseif(IFCFD(ICP).eq.2)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                                ': integrated thermal conflation'
          call edisp(itu,outs)
        elseif(IFCFD(ICP).eq.3)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                                          ': air flow conflation'
          call edisp(itu,outs)
        elseif(IFCFD(ICP).eq.4)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                                  ': one-way adaptive conflation'
          call edisp(itu,outs)
        elseif(IFCFD(ICP).eq.5)then
          write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &                      ': conditional two-way adaptive conflation'
          call edisp(itu,outs)
        endif
        call edisp(itu,' ')  
      endif

C Read in vertex linking.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,' therm geom',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','thermal geom',IER)
      if (WORD(1:13).eq.'*thermal_geom') then
        CALL STRIPC(IFL,OUTSTR,0,ND,1,' vert numbers',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','verts',IER)
        if (WORD(1:5).eq.'*vrts') then
          CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','orig vertex no',IER)
          if (IER.eq.0) IORG(ICFD)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','x axis vertex no',IER)
          if (IER.eq.0) ixend(ICFD)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','y axis vertex no',IER)
          if (IER.eq.0) iyend(ICFD)=IVAL
          CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','z axis vertex no',IER)
          if (IER.eq.0) izend(ICFD)=IVAL
          if (ND.eq.6) then
            CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','ze axis vertex no',IER)
            if (IER.eq.0) izende(ICFD)=IVAL
          else
            izende(ICFD)=0
          endif
        else
          IER=1
        endif
        if (IER.eq.0) then
          origin=.true.
        else
          origin=.false.
        endif
      endif

C Read in gridding regions.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'gridding regions',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','gridding',IER)
      if (WORD(1:8).eq.'*regions') then
        CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','x regions',IER)
        if (IER.eq.0) NREG(1,ICFD)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','y regions',IER)
        if (IER.eq.0) NREG(2,ICFD)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','z regions',IER)
        if (IER.eq.0) NREG(3,ICFD)=IVAL
        CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'W','ze regions',IER)
        if (IER.eq.0) NREG(4,ICFD)=IVAL
      endif

C Set ORTHO and write feedback if required.
      if (NREG(4,ICFD).gt.0) then
        ORTHO=.false.
        if(itrc.gt.0) then
          call edisp(itu,'The current domain is curvilinear with:')
          write(outs,'(i4,a)') NREG(1,ICFD),' X regions'
          call edisp(itu,outs)
          write(outs,'(i4,a)') NREG(2,ICFD),' Y regions'
          call edisp(itu,outs)
          write(outs,'(i4,a)') NREG(3,ICFD),' Z regions'
          call edisp(itu,outs)
          write(outs,'(i4,a)') NREG(4,ICFD),' Ze regions'
          call edisp(itu,outs)
        endif
      else
        ORTHO=.true.
        if(itrc.gt.0) then
          call edisp(itu,'The current domain is orthogonal with:')
          write(outs,'(i4,a)') NREG(1,ICFD),' X regions'
          call edisp(itu,outs)
          write(outs,'(i4,a)') NREG(2,ICFD),' Y regions'
          call edisp(itu,outs)
          write(outs,'(i4,a)') NREG(3,ICFD),' Z regions'
          call edisp(itu,outs)
        endif
      endif

C Check for error flag.
      if (IER.ne.0) then
        call usrmsg('Error reading gridding regions!',
     &              'Abandoning file read.','W')
        IER=1
        CALL ERPFREE(IFL,ISTAT)
        return
      endif

C Read gridding geometry. First check if orthogonal.
      if (ORTHO) then
        NAX=3
      else
        NAX=4
      endif

C Set total number of cells in each direction.
      NTCX=0
      NTCY=0
      NTCZ=0
      NTCZe=0

C Set total domain length in each direction.
      XLEN=0.0
      YLEN=0.0
      ZLEN=0.0
      ZLENe=0.0

C Loop through each gridding direction.
      do 10 IAX=1,NAX
        CALL STRIPC(IFL,OUTSTR,0,ND,1,'grid axis',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','axis',IER)
        if (WORD(1:8).eq.'*xregion') then
          xgrid=.TRUE.

C For each region read: number of cells, section length and power law
C coefficient.
          do 20 IGR=1,NREG(1,ICFD)
            CALL STRIPC(IFL,OUTSTR,0,ND,1,'x region data',IER)
            K=0
            MIN=NTCELX*(-1)
            CALL EGETWI(OUTSTR,K,IVAL,MIN,NTCELX,'F',
     &                          'Number of x cells?',IER)
            NCELX(IGR,ICFD)=IVAL
            CALL EGETWR(OUTSTR,K,VAL,0.0001,1000.0,'W',
     &                           'Size of x region',IER)
            XREG(IGR,ICFD)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Power law coef',IER)
            Xplaw(IGR,ICFD)=VAL
            NTCX=NTCX+ABS(NCELX(IGR,ICFD))
            XLEN=XLEN+XREG(IGR,ICFD)
            if (itrc.gt.0) then
              write(outs,'(a,i4,a,i4,a,f6.2,a,f6.3)') 'X Region',IGR,
     &        ', cells:',ABS(NCELX(IGR,ICFD)),', size:',XREG(IGR,ICFD),
     &                             ', power law coeff.:',Xplaw(IGR,ICFD)
              call edisp(itu,outs)
            endif
 20       continue
          if (itrc.gt.0) then
            write(outs,'(a,10x,i4,6x,f6.2)') '  Totals',NTCX,XLEN
            call edisp(itu,outs)
          endif

C Only NTCELX-2 cells allowed, otherwise array bounds exceeded.
          if(NTCX.GT.(NTCELX-2))then
            write(outs,'(a,i4)') ' Current limit is ',NTCELX-2 
            call usrmsg(' Too many cells in X direction.',outs,'W')
            ier=1
            xgrid=.FALSE.
          endif
        elseif (WORD(1:8).eq.'*yregion') then
          ygrid=.TRUE.

C For each region, read: no. of cells, section length and power law coefficient.
          do 30 IGR=1,NREG(2,ICFD)
            CALL STRIPC(IFL,OUTSTR,0,ND,1,'y region data',IER)
            K=0
            MIN=NTCELY*(-1)
            CALL EGETWI(OUTSTR,K,IVAL,MIN,NTCELY,'F',
     &                          'Number of y cells?',IER)
            NCELY(IGR,ICFD)=IVAL
            CALL EGETWR(OUTSTR,K,VAL,0.0001,1000.0,'W',
     &                           'Size of y region',IER)
            YREG(IGR,ICFD)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Power law coeff.',IER)
            Yplaw(IGR,ICFD)=VAL
            NTCY=NTCY+ABS(NCELY(IGR,ICFD))
            YLEN=YLEN+YREG(IGR,ICFD)
            if (itrc.gt.0) then
              write(outs,'(a,i4,a,i4,a,F6.2,a,F6.3)') 'Y Region',IGR,
     &                       ', cells:',ABS(NCELY(IGR,ICFD)),', size:',
     &               YREG(IGR,ICFD),', power law coeff:',Yplaw(IGR,ICFD)
              call edisp(itu,outs)
            endif
 30       continue
          if (itrc.gt.0) then
            write(outs,'(a,10x,i4,6x,f6.2)') '  Totals',NTCY,YLEN
            call edisp(itu,outs)
          endif
          if(NTCY.GT.(NTCELY-2))then
            write(outs,'(a,i4)') 'Current limit is ',NTCELY-2
            call usrmsg('Too many cells in Y direction.',outs,'W')
            ier=1
            ygrid=.FALSE.
          endif
        elseif (WORD(1:9).eq.'*zwregion') then
          zgrid=.TRUE.


C For each region, read: no. of cells, section length and power law coefficient.
          do 40 IGR=1,NREG(3,ICFD)
            CALL STRIPC(IFL,OUTSTR,0,ND,1,'z region data',IER)
            K=0
            MIN=NTCELZ*(-1)
            CALL EGETWI(OUTSTR,K,IVAL,MIN,NTCELZ,'F',
     &                          'Number of z cells?',IER)
            NCELZ(IGR,ICFD)=IVAL
            CALL EGETWR(OUTSTR,K,VAL,0.0001,1000.0,'W',
     &                           'Size of z region',IER)
            ZREG(IGR,ICFD)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Power law coeff.',IER)
            Zplaw(IGR,ICFD)=VAL
            NTCZ=NTCZ+ABS(NCELZ(IGR,ICFD))
            ZLEN=ZLEN+ZREG(IGR,ICFD)
            if (itrc.gt.0) then
              write(outs,'(a,i3,a,i4,a,F6.2,a,F6.3)') 'Z Region',IGR,
     &                     ', cells:',ABS(NCELZ(IGR,ICFD)),', size:',
     &             ZREG(IGR,ICFD),', power law coeff.:',Zplaw(IGR,ICFD)
              call edisp(itu,outs)
            endif
 40       continue
          if (itrc.gt.0) then
            write(outs,'(a,10x,i4,6x,f6.2)') '  Totals',NTCZ,ZLEN
            call edisp(itu,outs)
          endif
          if(NTCZ.GT.(NTCELZ-2))then
            write(outs,'(a,i4)') ' Current limit is ',NTCELZ-2
            call usrmsg('Too many cells in Z direction.',outs,'W')
            ier=1
            zgrid=.FALSE.
          endif
        elseif (WORD(1:9).eq.'*zeregion') then
          zegrid=.TRUE.

C For each region, read: no. of cells, section length and power law coefficient.
          do 50 IGR=1,NREG(4,ICFD)
            CALL STRIPC(IFL,OUTSTR,0,ND,1,'ze region data',IER)
            K=0
            MIN=NTCELZ*(-1)
            CALL EGETWI(OUTSTR,K,IVAL,MIN,NTCELZ,'F',
     &                          'Number of ze cells?',IER)
            NCELZe(IGR,ICFD)=IVAL
            CALL EGETWR(OUTSTR,K,VAL,0.0001,1000.0,'W',
     &                           'Size of ze region',IER)
            ZREGe(IGR,ICFD)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Power law coef',IER)
            Zplawe(IGR,ICFD)=VAL
            NTCZe=NTCZe+ABS(NCELZe(IGR,ICFD))
            ZLENe=ZLENe+ZREGe(IGR,ICFD)
            if (itrc.gt.0) then
              write(outs,'(a,i3,a,i4,a,F6.2,a,F6.3)') 'ZeRegion',IGR,
     &          ' cells:',NCELZe(IGR,ICFD),' size:',ZREGe(IGR,ICFD),
     &           ' power law coef:',Zplawe(IGR,ICFD)
              call edisp(itu,outs)
            endif
 50       continue
          if (itrc.gt.0) then
            write(outs,'(a,10x,i4,6x,f6.2)') '  Totals',NTCZe,ZLENe
            call edisp(itu,outs)
          endif
          if(NTCZe.GT.(NTCELZ-2))then
            write(outs,'(a,i4)') ' Current limit is ',NTCELZ-2
            call usrmsg(' Too many cells in Ze direction.',outs,'W')
            ier=1
            zegrid=.FALSE.
          endif

C Check that total number of cells in both Z dirs are the same.
          if (NTCZ.ne.NTCZe) then
            call usrmsg('Total number of cells on East and West',
     &            'face differ; disallowed in curviliniear mode!','W')
            ier=1
            zgrid=.FALSE.
            zegrid=.FALSE.
          endif
        endif
 10   continue
      if(itrc.gt.0)then
        call edisp(itu,' ')
      endif

C Check for error flag.
      if (IER.ne.0) then
        call usrmsg(
     &    'Warning while reading the gridding data;',
     &    'please check. Continuing to read the file.','W')
        xgrid=.FALSE.
        ygrid=.FALSE.
        zgrid=.FALSE.
        zegrid=.FALSE.
        IER=0
      endif

C Check for number of defined dimensions and overall domain size.
C If one dimension does not exist make NREG=1, NCEL=1 and length=1.0
C Only do this for orthogonal directions (i.e. omit Ze axis)
      if (NREG(1,ICFD).EQ.0) then
        NREG(1,ICFD)=1
        NCELX(1,ICFD)=1
        XREG(1,ICFD)=1.0
      endif
      if (NREG(2,ICFD).EQ.0) then
        NREG(2,ICFD)=1
        NCELY(1,ICFD)=1
        YREG(1,ICFD)=1.0
      endif
      if (NREG(3,ICFD).EQ.0) then
        NREG(3,ICFD)=1
        NCELZ(1,ICFD)=1
        ZREG(1,ICFD)=1.0
      endif

C Calculate the total number of cells in each direction.
      NIM2=0
      NJM2=0
      NKM2=0
      DO I=1,NREG(1,ICFD)
        NIM2=NIM2+ABS(NCELX(I,ICFD))
      ENDDO
      DO I=1,NREG(2,ICFD)
        NJM2=NJM2+ABS(NCELY(I,ICFD))
      ENDDO
      DO I=1,NREG(3,ICFD)
        NKM2=NKM2+ABS(NCELZ(I,ICFD))
      ENDDO
      NIM1=NIM2+1
      NJM1=NJM2+1
      NKM1=NKM2+1
      NI=NIM1+1
      NJ=NJM1+1
      NK=NKM1+1

C Read in key volumes in domain.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'key volumes',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','volumes tag',IER)
      if (WORD(1:8).eq.'*volumes') then
        CALL EGETWI(OUTSTR,K,IVAL,0,MNVLS,'F',
     &                          'Number of volumes',IER)
        if (IER.eq.0) NVOL(ICFD)=IVAL
        do 60 IV=1,NVOL(ICFD)

C Read in volume name, type and geometry.
          CALL STRIPC(IFL,OUTSTR,0,ND,1,'volume data',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','volumes name',IER)
          VOLNAME(IV,ICFD)=WORD(1:12)
          IF(VOLNAME(IV,ICFD)(1:4).EQ.'SRC_')ICCSRC(ICFD)=1
          CALL EGETW(OUTSTR,K,WORD,'W','volumes name',IER)
          write (TYPE,'(a)') WORD(1:6)
          if (WORD(1:4).eq.'West') then
            IVOLF(IV,ICFD)=1
          elseif (WORD(1:4).eq.'East') then
            IVOLF(IV,ICFD)=2
          elseif (WORD(1:5).eq.'South') then
            IVOLF(IV,ICFD)=3
          elseif (WORD(1:5).eq.'North') then
            IVOLF(IV,ICFD)=4
          elseif (WORD(1:3).eq.'Low') then
            IVOLF(IV,ICFD)=5
          elseif (WORD(1:4).eq.'High') then
            IVOLF(IV,ICFD)=6
          elseif (WORD(1:5).eq.'Whole') then
            IVOLF(IV,ICFD)=7
          elseif (WORD(1:5).eq.'Block') then
            IVOLF(IV,ICFD)=8
          elseif (WORD(1:6).eq.'Source') then
            IVOLF(IV,ICFD)=9
          else
            TYPE='UNKN  '
            IVOLF(IV,ICFD)=0
            IER=1
            call edisp(itu,OUTSTR)
            call usrmsg('Did not recognise volume type:',WORD,'W')
          endif
          CALL EGETWI(OUTSTR,K,IVALS,0,NTCX,'F','I start cell',IER)
          if (IER.eq.0) IVCELLS(IV,ICFD,1)=IVALS+1
          CALL EGETWI(OUTSTR,K,IVALF,IVALS,NTCX,'F','I end cell',IER)
          if (IER.eq.0) IVCELLS(IV,ICFD,2)=IVALF+1
          CALL EGETWI(OUTSTR,K,JVALS,0,NTCY,'F','J start cell',IER)
          if (IER.eq.0) JVCELLS(IV,ICFD,1)=JVALS+1
          CALL EGETWI(OUTSTR,K,JVALF,JVALS,NTCY,'F','J end cell',IER)
          if (IER.eq.0) JVCELLS(IV,ICFD,2)=JVALF+1
          CALL EGETWI(OUTSTR,K,KVALS,0,NTCZ,'F','K start cell',IER)
          if (IER.eq.0) KVCELLS(IV,ICFD,1)=KVALS+1
          CALL EGETWI(OUTSTR,K,KVALF,KVALS,NTCZ,'F','K end cell',IER)
          if (IER.eq.0) KVCELLS(IV,ICFD,2)=KVALF+1
          if (itrc.gt.0) then
            write(outs,'(2a)') 'Volume ',VOLNAME(IV,ICFD)
            outs_pre=outs
            if ((TYPE.ne.'Whole ').and.(TYPE.ne.'Block ').and.
     &                                         (TYPE.ne.'Source')) then
              write(outs,'(4a)')outs_pre(1:LNBLNK(outs_pre)),
     &                       ' is on the ',TYPE(1:LNBLNK(TYPE)),' face'
            else
              write(outs,'(3a)')outs_pre(1:LNBLNK(outs_pre)),
     &                              ' is of type ',TYPE(1:LNBLNK(TYPE))
            endif
            outs_pre=outs
            write(outs,'(2a,6i4)')outs_pre(1:LNBLNK(outs_pre)),
     &                   ', cells:',IVALS,IVALF,JVALS,JVALF,KVALS,KVALF
            call edisp(itu,outs)
          endif
 60     continue
        if (itrc.gt.0) then
          write(outs,'(2a)')'NOTE: Face orientations (North, South, ',
     &            'East and West) are in terms of the CFD domain only.'
          call edisp(itu,outs)
          write(outs,'(a)')
     &       '      This does not necessarily match building geometry.'
          call edisp(itu,outs)
          call edisp(itu,' ')
        endif
      endif

C Check if all boundary cells have been defined as boundary conditions.
      CALL BCEKCK(ITRC)

C Read solid boundary conditions.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'solid volumes',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','solid volumes tag',IER)
      if (WORD(1:7).eq.'*solids') then 

C Read line until reach *air line.
 88     CALL STRIPC(IFL,OUTSTR,99,ND,1,'solid data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','solid data tag',IER)
        if (WORD(1:9).ne.'*air_flow') then 

C Read data and match to volume with same name.
          do 80 IV=1,NVOL(ICFD)
            if (VOLNAME(IV,ICFD).eq.WORD(1:12)) then
              CALL EGETW(OUTSTR,K,WORD,'W','solid type',IER)
              if (WORD(1:4).eq.'Temp') then
                IVTYPE(IV,ICFD)=1
                CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'W','solid temp',IER)
                VOLTemp(IV,ICFD)=VALS
              elseif (WORD(1:4).eq.'Heat') then
                IVTYPE(IV,ICFD)=2
                CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','solid heat',IER)
                VOLHeat(IV,ICFD)=VALS
              elseif (WORD(1:4).eq.'Symm') then
                IVTYPE(IV,ICFD)=3
              else
                IER=1
                call edisp(itu,OUTSTR)
                call usrmsg('Did not recognise BC type:',WORD,'W')
              endif
              if (ND.gt.3) then

C Read conflated data.
C Following line commented because vertical bar is considered a comment 
C from circa rev1700
C                CALL EGETW(OUTSTR,K,WORD,'W','vert bar',IER)
                CALL EGETW(OUTSTR,K,WORD,'W','conf',IER)
                CALL EGETWI(OUTSTR,K,IVALS,1,99,'-','conf type',IER)
                IVConfl(IV,ICFD)=IVALS
                CALL EGETW(OUTSTR,K,WORD,'W','surface',IER)
                VCsurf(IV,ICFD)=WORD(1:12)
                IVTYPE(IV,ICFD)=IVTYPE(IV,ICFD)+3
              endif
              if (itrc.gt.0) then
                write(outs,'(3a)')'Solid boundary condition ',
     &       VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),' is of type'
                if ((IVTYPE(IV,ICFD)-3).gt.0) then
C Is conflated.
                  IVTYPEtemp=-(IVTYPE(IV,ICFD)-3)
                else
C Is not conflated.
                  IVTYPEtemp=IVTYPE(IV,ICFD)
                endif
                outs_pre=outs
                if (abs(IVTYPEtemp).eq.1) then
                  write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &              ' Temp,'
                  outs_pre=outs
                  if (IVTYPEtemp.gt.0) then
                    write(outs,'(2a,f7.2,a)')
     &                outs_pre(1:LNBLNK(outs_pre)),
     &      ' with constant temperature ',VOLTemp(IV,ICFD),' degrees C'
                  else
                    write(outs,'(3a)')outs_pre(1:LNBLNK(outs_pre)),
     &           ' with temperature associated with building surface ',
     &                VCsurf(IV,ICFD)(1:LNBLNK(VCsurf(IV,ICFD)))
                  endif
                  call edisp(itu,outs)
                elseif (abs(IVTYPEtemp).eq.2) then
                  write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &              ' Heat,'
                  outs_pre=outs
                  if (IVTYPEtemp.gt.0) then
                    write(outs,'(2a,f7.2,a)')
     &                outs_pre(1:LNBLNK(outs_pre)),
     &                ' with constant heat flux ',VOLTemp(IV,ICFD),' W'
                    call edisp(itu,outs)
                  else
                    write(outs,'(3a)')outs_pre(1:LNBLNK(outs_pre)),
     &                ' associated with building surface ',
     &                VCsurf(IV,ICFD)(1:LNBLNK(VCsurf(IV,ICFD)))
                    call edisp(itu,outs)
                  endif
                elseif (abs(IVTYPEtemp).eq.3) then
                  write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &              ' Symmetrical'
                  outs_pre=outs
                  if (IVTYPEtemp.gt.0) then
                    call edisp(itu,outs)
                  else
                    write(outs,'(3a)')outs_pre(1:LNBLNK(outs_pre)),
     &                ', associated with building surface ',
     &                VCsurf(IV,ICFD)(1:LNBLNK(VCsurf(IV,ICFD)))
                    call edisp(itu,outs)
                  endif
                endif
              endif
            endif
 80       continue

C Loop back again for next solid BC.
          goto 88
        else
          if(itrc.gt.0)then
            call edisp(itu,' ')
          endif
        endif
      endif

C Read air flow opening boundary conditions.
C Read file until *sources line is reached.
 98   CALL STRIPC(IFL,OUTSTR,99,ND,1,'air flow data',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','air data tag',IER)
      if(WORD(1:13).ne.'*contaminants'.and.WORD(1:8).ne.'*sources')then 

C Read data and match to volume with same name.
        do 90 IV=1,NVOL(ICFD)
          if (VOLNAME(IV,ICFD).eq.WORD(1:12)) then
            CALL EGETW(OUTSTR,K,WORD,'W','air type',IER)
            if (WORD(1:8).eq.'Pressure') then
              IVTYPE(IV,ICFD)=10
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air press',IER)
              VOLPres(IV,ICFD)=VALS
              if (ND.gt.3) then

C Mass flow connection data for version 2
                IF(ABS(VER-2.).LT.0.01)THEN
C                  CALL EGETW(OUTSTR,K,LCND,'W','Nod in CFD domain',IER)
C                  CALL EGETW(OUTSTR,K,LXND,'W','Nod outside domain',IER)
C                  CALL EGETW(OUTSTR,K,LCNN,'W','Connecting comp',IER)

C                  write(6,*) 'Node in CFD domain ',LCND
C                  write(6,*) 'Node outside domain ',LXND
C                  write(6,*) 'Connecting comp ',LCNN

C                  call CFMFSU(IV,ITRC,LCND,LXND,LCNN)
C                  ICFMFS(ICFD)=1
                  IMFACT=1
                  CALL USRMSG('An older version of dfd file is in use',
     &            'redefine opening type boundary conditions','W')
                ELSEIF(VER.GT.2.05)THEN

C Mass flow connection data for version 2.1+.
                  CALL EGETWI(OUTSTR,K,INODE,0,MNOD,'w','CFD nod',IER)
                  CALL EGETWI(OUTSTR,K,ICNNE,0,MCNN,'w','conn. #',IER)
                  ICFMFS(ICFD)=1
                  IMFACT=1
                  ICFDNOD(ICFD)=INODE
                  ICFDCNN(IV,ICFD)=ICNNE
                  if (cAppName.ne.'dfs') then
                    CALL CFMFSU(IV,ITRC,INODE,ICNNE,VOLNAME(IV,ICFD))
                  endif
                ENDIF
                if (itrc.gt.0) then
                  write(outs,'(3a,i2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     & ' is of type Pressure, conflated to mass flow connection ',ICNNE
                  call edisp(itu,outs)
                endif
              else
                if (itrc.gt.0) then
                  write(outs,'(3a,f6.2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     &               ' is of type Pressure, pressure ',VOLPres(IV,ICFD) 
                  call edisp(itu,outs)
                endif
              endif
            elseif (WORD(1:8).eq.'Velocity') then

C Air flow is described as a mass flow across a face i.e. with a 
C given direction.
              IVTYPE(IV,ICFD)=11
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air vel',IER)
              VOLVel(IV,ICFD)=VALS

C Read remaining data assuming data in order.
              CALL EGETW(OUTSTR,K,WORD,'W','air temp',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air temp',IER)
              VOLTemp(IV,ICFD)=VALS
              CALL EGETW(OUTSTR,K,WORD,'W','air hum',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air hum',IER)
              VOLHum(IV,ICFD)=VALS
              CALL EGETW(OUTSTR,K,WORD,'W','area',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','area',IER)
              VOLArea(IV,ICFD)=VALS
              if(VALS.gt.0.0) then

C The opening presents a small supply opening 
                NSSO(ICFD)=NSSO(ICFD)+1 
                INSSO(NSSO(ICFD),ICFD)=IV 
              endif
              CALL EGETW(OUTSTR,K,WORD,'W','direction',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','dir 1',IER)
              VOLDir(IV,ICFD,1)=VALS
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','dir 2',IER)
              VOLDir(IV,ICFD,2)=VALS
              if (ND.gt.12) then

C Mass flow connection data for version 2
                IF(ABS(VER-2.).LT.0.01)THEN
C                  CALL EGETW(OUTSTR,K,LCND,'W','Nod in CFD domain',IER)
C                  CALL EGETW(OUTSTR,K,LXND,'W','Nod outside domain',IER)
C                  CALL EGETW(OUTSTR,K,LCNN,'W','Connecting comp',IER)

C                  write(6,*) 'Node in CFD domain ',LCND
C                  write(6,*) 'Node outside domain ',LXND
C                  write(6,*) 'Connecting comp ',LCNN

C                  call CFMFSU(IV,ITRC,LCND,LXND,LCNN)
C                  ICFMFS(ICFD)=1
                  IMFACT=1
                  CALL USRMSG('An older version of dfd file is in use',
     &            'redefine opening type boundary conditions','W')
                ELSEIF(VER.GT.2.05)THEN

C Mass flow connection data for version 2.1+.
                  CALL EGETWI(OUTSTR,K,INODE,0,MNOD,'w','CFD nod',IER)
                  CALL EGETWI(OUTSTR,K,ICNNE,0,MCNN,'w','conn. #',IER)
                  ICFMFS(ICFD)=1
                  IMFACT=1
                  ICFDNOD(ICFD)=INODE
                  ICFDCNN(IV,ICFD)=ICNNE
                  if (cAppName.ne.'dfs') then
                    CALL CFMFSU(IV,ITRC,INODE,ICNNE,VOLNAME(IV,ICFD))
                  endif
                ENDIF
                if (itrc.gt.0) then
                  write(outs,'(3a,i2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     & ' is of type Velocity, conflated to mass flow connection ',ICNNE
                  call edisp(itu,outs)
                endif
              else
                if (itrc.gt.0) then
                  write(outs,'(3a,f8.4,a,f6.2,a,f6.2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     &               ' is of type Velocity, velocity ',VOLVel(IV,ICFD), 
     &                               ', temperature ',VOLTemp(IV,ICFD),
     &                                     ', humdity ',VOLHum(IV,ICFD)
                  call edisp(itu,outs)
                  if (VOLArea(IV,ICFD).gt.0.0) then
                    write(outs,'(a,f8.4)')
     &             '  This is a small supply opening (SSO) with area ',
     &                                                 VOLArea(IV,ICFD)
                    call edisp(itu,outs)
                  endif
                endif
              endif
            elseif (WORD(1:4).eq.'Zero') then

C Zero gradient BC.
              IVTYPE(IV,ICFD)=12
              if (ND.gt.2) then
                IF(ABS(VER-2.).LT.0.01)THEN

C Mass flow connection data for version 2
C Update opening type to 11=velocity incase flow is entering 
C via this connection.
                  IVTYPE(IV,ICFD)=11

C                  CALL EGETW(OUTSTR,K,LCND,'W','Nod in CFD domain',IER)
C                  CALL EGETW(OUTSTR,K,LXND,'W','Nod outside domain',IER)
C                  CALL EGETW(OUTSTR,K,LCNN,'W','Connecting comp',IER)

C                  write(6,*) 'Node in CFD domain ',LCND
C                  write(6,*) 'Node outside domain ',LXND
C                  write(6,*) 'Connecting comp ',LCNN

C                  call CFMFSU(IV,ITRC,LCND,LXND,LCNN)
C                  ICFMFS(ICFD)=1
                  CALL USRMSG('An older version of dfd file is in use',
     &            'redefine opening type boundary conditions','W')
                ELSEIF(VER.GT.2.05)THEN

C Mass flow connection data for version 2.1+.
                  CALL EGETWI(OUTSTR,K,INODE,0,MNOD,'w','CFD nod',IER)
                  CALL EGETWI(OUTSTR,K,ICNNE,0,MCNN,'w','conn. #',IER)
                  ICFMFS(ICFD)=1
                  IMFACT=1
                  ICFDNOD(ICFD)=INODE
                  ICFDCNN(IV,ICFD)=ICNNE
                  if (cAppName.ne.'dfs') then
                    CALL CFMFSU(IV,ITRC,INODE,ICNNE,VOLNAME(IV,ICFD))
                  endif
                ENDIF
                if (itrc.gt.0) then
                  write(outs,'(3a,i2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     & ' is of type Zero-gradient, conflated to mass flow connection ',
     &                                                            ICNNE
                  call edisp(itu,outs)
                endif
              else
                if(itrc.gt.0)then
                  write(outs,'(3a)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     &                                      ' is of type Zero-gradient'
                  call edisp(itu,outs)
                endif
              endif
            elseif (WORD(1:4).eq.'Mass') then

C Mass flow type - no direction component.
              IVTYPE(IV,ICFD)=13
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air vel',IER)
              VOLVel(IV,ICFD)=VALS
              CALL EGETW(OUTSTR,K,WORD,'W','air temp',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air temp',IER)
              VOLTemp(IV,ICFD)=VALS
              CALL EGETW(OUTSTR,K,WORD,'W','air hum',IER)
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','air hum',IER)
              VOLHum(IV,ICFD)=VALS
              if (ND.gt.7) then

C Mass flow connection data for version 2
                IF(ABS(VER-2.).LT.0.01)THEN
C                  CALL EGETW(OUTSTR,K,LCND,'W','Nod in CFD domain',IER)
C                  CALL EGETW(OUTSTR,K,LXND,'W','Nod outside domain',IER)
C                  CALL EGETW(OUTSTR,K,LCNN,'W','Connecting comp',IER)

C                  write(6,*) 'Node in CFD domain ',LCND
C                  write(6,*) 'Node outside domain ',LXND
C                  write(6,*) 'Connecting comp ',LCNN

C                  call CFMFSU(IV,ITRC,LCND,LXND,LCNN)
C                  ICFMFS(ICFD)=1
                  CALL USRMSG('An older version of dfd file is in use',
     &            'redefine opening type boundary conditions','W')
                ELSEIF(VER.GT.2.05)THEN

C Mass flow connection data for version 2.1+.
C There is no provision for a mass type boundary condition in
C Version 2.1 onwards but any opening type BC can be defined to be 
C connected to the mass flow network. 
C Following code not really necessary.
                  CALL EGETWI(OUTSTR,K,INODE,0,MNOD,'w','CFD nod',IER)
                  CALL EGETWI(OUTSTR,K,ICNNE,0,MCNN,'w','conn. #',IER)
                  ICFMFS(ICFD)=1
                  IMFACT=1
                  ICFDNOD(ICFD)=INODE
                  ICFDCNN(IV,ICFD)=ICNNE
                  if (cAppName.ne.'dfs') then
                    CALL CFMFSU(IV,ITRC,INODE,ICNNE,VOLNAME(IV,ICFD))
                  endif
                ENDIF
                if (itrc.gt.0) then
                  write(outs,'(3a,i2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     & ' is of type Mass, conflated to mass flow connection ',ICNNE
                  call edisp(itu,outs)
                endif
              else
                if (itrc.gt.0) then
                  write(outs,'(3a,f8.4,a,f6.2,a,f6.2)')'Opening ',
     &                    VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),
     &                   ' is of type Mass, velocity ',VOLVel(IV,ICFD), 
     &                               ', temperature ',VOLTemp(IV,ICFD),
     &                                     ', humdity ',VOLHum(IV,ICFD)
                  call edisp(itu,outs)
                endif
              endif

            else
              IER=1
              call edisp(itu,OUTSTR)
              call usrmsg('Did not recognise BC type: ',WORD,'W')
            endif
          endif
 90     continue

C Loop back again for next air flow BC.
        goto 98
      ELSE
        if(itrc.gt.0)then
          call edisp(itu,' ')
        endif

C Read number of contaminants defined and which one is water (humidity).
        CALL EGETWI(OUTSTR,K,NCTM(ICFD),0,MCTM,'w','# of contaminants',
     &    IER)
        IF(VER.GT.2.15)K=63
        IF(VER.GT.2.05.AND.VER.LT.2.15)K=57
        CALL EGETWI(OUTSTR,K,JHUMINDX(ICFD),0,MCTM,'w','water index',
     &    IER)

C Read data to calculate turbulent Schmidt number.
        IF(VER.GT.2.15)THEN
          DO 156 ICTM=1,NCTM(ICFD)
            CALL STRIPC(IFL,OUTSTR,0,ND,1,'tur schmdt #',IER)
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','ctm name',IER)
            POLNAM(ICTM,ICFD)=WORD(1:12)
            CALPOL(ICTM,ICFD)=.TRUE.
            CALL EGETWI(OUTSTR,K,ISCHMT(ICTM,ICFD),0,1,'w','shmdt index'
     &        ,IER)         
            IF(ISCHMT(ICTM,ICFD).EQ.0)THEN
              CALL EGETWR(OUTSTR,K,VL1,0.1,2.5,'-','tur shmdt #',IER)
              SCHMT(ICTM,ICFD)=VL1
            ELSEIF(ISCHMT(ICTM,ICFD).EQ.1)THEN
              CALL EGETWR(OUTSTR,K,VL1,1.,1000.,'W','molar mass',IER)
              CALL EGETWR(OUTSTR,K,VL2,1.,1000.,'W','crit vol',IER)
              CALL EGETWR(OUTSTR,K,VL3,100.,500.,'W','T_boil',IER)
              CALL EGETWR(OUTSTR,K,VL4,100.,1000.,'W','T_crit',IER)
              GFM(ICTM,ICFD)=VL1
              VCRIT(ICTM,ICFD)=VL2
              TBOIL(ICTM,ICFD)=VL3
              TCRIT(ICTM,ICFD)=VL4
            ENDIF
            if (ver.gt.2.25) then ! read coupled network contaminant
              CALL EGETWI(outstr,k,ival,0,MCONTM,'W','cpld ntwk ctm',
     &          IER)
              ICC2NC(ictm,ICFD)=ival
              if (ival.gt.0) ICTDFAF(ICFD)=1
            else ! older file version
              if (NCTM(ICFD).eq.NCONTM) then
                ICC2NC(ictm,ICFD)=ictm
                ICTDFAF(ICFD)=1
              else
                ICC2NC(ictm,ICFD)=0
                ICTDFAF(ICFD)=0
                write(outs,'(a,i2.2,a)')
     &            'Contaminant network coupling in CFD domain ',ICFD,
     &            ' is corrupt.'
                CALL USRMSG(outs,'Please redefine.','W')
              endif
            endif
 156      CONTINUE
        ENDIF
      endif

C List conflated mfs if one is active.
      IF((ICFMFS(ICFD).GT.0).and.(cAppName.ne.'dfs').and.
     &                                                 (itrc.gt.0))THEN
        CALL EDISP(ITU,' Following conflated flow network will be used')
        CALL EDISP(ITU,' ')
        CALL MFLISTEMP
        CALL EDISP(ITU,' ')
      ENDIF

C Check and report contaminant schmidt numbers.
      IF(NCTM(ICFD).GE.1)THEN
        CALL SCHCHK
        if (itrc.gt.0) then
          do ICTM=1,NCTM(ICFD)
            write(outs,'(3a)')'Contaminant ',
     &        POLNAM(ICTM,ICFD)(1:LNBLNK(POLNAM(ICTM,ICFD))),
     &        ' has turbulent Schmidt number'
            outs_pre=outs
            if (ISCHMT(ICTM,ICFD).eq.1) then
              write(outs,'(2a)')outs_pre(1:LNBLNK(outs_pre)),
     &          ' calculated at each time step'
            else
              write(outs,'(a,1X,f8.4)')outs_pre(1:LNBLNK(outs_pre)),
     &          SCHMT(ICTM,ICFD)
            endif
            call edisp(itu,outs)
          enddo
          call edisp(itu,' ')
        endif
      endif

C Check for line specifying sources.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'src or blk line',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','src or blk tag',IER)
      IF(WORD(1:10).EQ.'*blockages') GOTO 116 ! not present, go straight to blockages.

C Read sources until we reach *blockages line.
 108  CALL STRIPC(IFL,OUTSTR,0,ND,1,'src data',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','src name',IER)
      if (WORD(1:10).ne.'*blockages') then 

C Read data and match to volume with same name.
        do 111 IV=1,NVOL(ICFD)
          if (VOLNAME(IV,ICFD).eq.WORD(1:12)) then
            IVTYPE(IV,ICFD)=20
            CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','src heat/frac',IER)
            VOLHeat(IV,ICFD)=VALS
            CALL EGETWI(OUTSTR,K,IVALS,0,MGTY,'-','cas gn id',IER)
            IDcasgn(IV,ICFD)=IVALS
            if (VER.lt.2.25) then ! old file format              
              CALL EGETWR(OUTSTR,K,VALS,0.,1.,'-','frac of gain',IER)
              Fcasgn(IV,ICFD)=VALS
            else ! previous value might be fraction
              if (IDCASGN(iv,ICFD).gt.0) then
                FCASGN(iv,ICFD)=vals
                VOLHeat(IV,ICFD)=0.0 ! will be dynamically updated
              else
                FCASGN(iv,ICFD)=0.0
              endif
            endif
            if (VER.lt.2.25) then ! old file format
              if (isdynamicocup(ICP).eq.4) then
                write(outs,'(a17,i2,a20,a12,a30)')'Warning: in zone ',
     &            ICP,' CFD domain, source ',VOLNAME(IV,ICFD),
     &            ' occupant type is not defined.'
                call edisp(iuout,outs)
                write(outs,'(2a)')
     &            'This is required for multi-segmented dynamic ',
     &            'occupants; please correct this before simulating.'
                call edisp(iuout,outs)
                write(outs,'(a)')
              endif
              VCsurf(IV,ICFD)='none'
            else
              CALL EGETW(OUTSTR,K,WORD,'-','occ type',IER)
              VCsurf(IV,ICFD)=WORD(1:12) ! use VCsurf for source occupant type
            endif
            DO ICTM=1,NCTM(ICFD)
              CALL EGETWR(OUTSTR,K,VALS,0.,1.,'-','ctm gen/frac',IER)
              VOLPOL(ICTM,IV,ICFD)=VALS
              IF(VER.GT.2.15)THEN
                CALL EGETW(OUTSTR,K,WORD,'W','ctm netwrk src',IER)
                IF(WORD(1:4).EQ.'none')THEN ! no link with network source
                  SRCE(IV,ICTM,ICFD)=' '
                  SRCFRC(IV,ICTM,ICFD)=0.0
                elseif (VCsurf(IV,ICFD).ne.'none' .and. 
     &                  ictm.eq.JHUMINDX(ICFD)) then ! occupant moisture release
                  SRCE(IV,ICTM,ICFD)=' ' ! linked through occupant type
                  SRCFRC(IV,ICTM,ICFD)=1.0
                  VOLPOL(ICTM,IV,ICFD)=0.0 ! will be dynamically updated
                else ! linked with network source
                  SRCE(IV,ICTM,ICFD)=WORD(1:12)

C Previous value read into VOLVOL is actually the fraction.
                  VOLPOL(ICTM,IV,ICFD)=0.0 ! will be dynamically updated
                  if (ver.gt.2.25) then                  
                    SRCFRC(IV,ICTM,ICFD)=vals
                  else ! for old files, assume fraction of 1.0
                    SRCFRC(IV,ICTM,ICFD)=1.0
                  endif
                ENDIF
              ENDIF
            ENDDO

            if (itrc.gt.0) then
              write(outs,'(3a)')'Source ',
     &          VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD))),' '
              outs_pre=outs
              if (IDcasgn(IV,ICFD).gt.0) then
                write(outs,'(2a,i3,a,f4.2)')outs_pre(1:LNBLNK(outs_pre))
     &            ,'is associated with casual gain ',IDcasgn(IV,ICFD),
     &            ', fraction ',Fcasgn(IV,ICFD)
              else
                write(outs,'(a,f7.3,a)')outs_pre(1:LNBLNK(outs_pre)),
     &            'has constant heat flux of ',VOLHeat(IV,ICFD),' W'
              endif
              call edisp(itu,outs)
              if (NCTM(ICFD).ne.0) then
                do 234 ICTM=1,NCTM(ICFD)
                  write(outs,'(3a)')'  For contaminant ',
     &              POLNAM(ICTM,ICFD)(1:LNBLNK(POLNAM(ICTM,ICFD))),
     &              ' this source '
                  outs_pre=outs
                  if (VCsurf(IV,ICFD).ne.'none' .and. 
     &                    ictm.eq.JHUMINDX(ICFD)) then
                    write(outs,'(3a)')outs_pre(1:LNBLNK(outs_pre)),
     &              'represents a ',VCsurf(IV,ICFD)
                  elseif (SRCE(IV,ICTM,ICFD).ne.' ') then
                    write(outs,'(2a,f5.3,2a)')
     &                outs_pre(1:LNBLNK(outs_pre)),
     &                'is associated with ',SRCFRC(IV,ICTM,ICFD)*100.,
     &                ' % of network source ',SRCE(IV,ICTM,ICFD)
                  else
                    write(outs,'(2a,f13.10,a)')
     &                outs_pre(1:LNBLNK(outs_pre)),
     &                'generates ',VOLPOL(ICTM,IV,ICFD),' kg/s'
                  endif
  234           continue
                call edisp(itu,outs)
              endif
            endif
          endif
 111    continue

C Loop back again for next source BC.
        goto 108
      else
        if(itrc.gt.0)then
          call edisp(itu,' ')
        endif
      endif

C Read line until reach *solutions line.
 116  CALL STRIPC(IFL,OUTSTR,99,ND,1,'blk data',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','blk name',IER)
      if (WORD(1:17).ne.'*solution_methods') then 

C Read data and match to volume with same name.
C read BLOCKAGES here.
        do 112 IV=1,NVOL(ICFD)
          if (VOLNAME(IV,ICFD).eq.WORD(1:12)) then
            NBLK(ICFD)=NBLK(ICFD)+1
            INBLK(NBLK(ICFD),ICFD)=IV

            CALL EGETW(OUTSTR,K,WORD,'W','blk type',IER)
            if (itrc.gt.0) then
              write(outs,'(2a)')'Blockage ',
     &          VOLNAME(IV,ICFD)(1:LNBLNK(VOLNAME(IV,ICFD)))
              outs_pre=outs
            endif
            if (WORD(1:4).eq.'Heat') then
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','blk heat',IER)
              VOLHeat(IV,ICFD)=VALS
              if (itrc.gt.0) then
                write(outs,'(2a)')outs_pre(1:lnblnk(outs_pre)),
     &            ' has heat flux '
                outs_pre=outs
              endif
              if (ND.eq.3) then
                IVTYPE(IV,ICFD)=30
                VCsurf(IV,ICFD)='NONE        '
                do i=1,6
                  BLKSURF(IV,ICFD,i)='NONE        '
                enddo
                if (itrc.gt.0) then
                  write(outs,'(a,f12.2,a)')outs_pre(1:lnblnk(outs_pre))
     &              ,VOLHeat(IV,ICFD),' W/m^2'
                  call edisp(itu,outs)
                endif
              elseif (ND.eq.7) then
                CALL EGETW(OUTSTR,K,WORD,'W','CasGn or Persn',IER)
                if (word(1:5).eq.'CasGn') then
                  CALL EGETWI(OUTSTR,K,ivals,0,MGTY,'-','cas gn id',IER)
                  IDcasgn(IV,ICFD)=ivals
                  CALL EGETWR(OUTSTR,K,vals,0.,1.,'-','frac of gain',
     &              IER)
                  Fcasgn(IV,ICFD)=vals
                  IVTYPE(IV,ICFD)=34
                  VCsurf(IV,ICFD)='NONE        '
                  do i=1,6
                    BLKSURF(IV,ICFD,i)='NONE        '
                  enddo
                  if (itrc.gt.0) then
                    write(outs,'(2a,i3,a,f4.2)')
     &                outs_pre(1:LNBLNK(outs_pre)),
     &                'associated with casual gain ',IDcasgn(IV,ICFD),
     &                ', fraction ',Fcasgn(IV,ICFD)
                    call edisp(itu,outs)
                  endif
                elseif (word(1:5).eq.'Persn') then
                  CALL EGETW(OUTSTR,K,word,'-','occ type',IER)
                  BLKSURF(IV,ICFD,1)=word(1:12)
                  CALL EGETW(OUTSTR,K,word,'-','occ part',IER)
                  BLKSURF(IV,ICFD,2)=word(1:12)
                  IVTYPE(IV,ICFD)=35
                  VCsurf(IV,ICFD)='NONE        '
                  do i=3,6
                    BLKSURF(IV,ICFD,i)='NONE        '
                  enddo
                  if (itrc.gt.0) then
                    write(outs,'(5a)')outs_pre(1:LNBLNK(outs_pre)),
     &                'representing the ',
     &                BLKSURF(IV,ICFD,2)(1:lnblnk(BLKSURF(IV,ICFD,2))),
     &                ' of a ',BLKSURF(IV,ICFD,1)
                    call edisp(itu,outs)
                  endif
                endif
              endif
            elseif (WORD(1:4).eq.'Temp') then
              CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','blk temp',IER)
              VOLTemp(IV,ICFD)=VALS
              if (itrc.gt.0) then
                write(outs,'(2a)')outs_pre(1:lnblnk(outs_pre)),
     &            ' has temperature '
                outs_pre=outs
              endif
              if (ND.eq.3) then
                IVTYPE(IV,ICFD)=31
                VCsurf(IV,ICFD)='NONE        '
                do i=1,6
                  BLKSURF(IV,ICFD,i)='NONE        '
                enddo
                if (itrc.gt.0) then
                  write(outs,'(a,f8.2,a)')outs_pre(1:lnblnk(outs_pre))
     &              ,VOLTemp(IV,ICFD),' C'
                  call edisp(itu,outs)
                endif
              elseif (ND.eq.6) then
                CALL EGETW(OUTSTR,K,WORD,'W','conf',IER)
                CALL EGETW(OUTSTR,K,WORD,'W','surface',IER)
                VCsurf(IV,ICFD)=WORD(1:12)
                IVTYPE(IV,ICFD)=32
                do i=1,6
                  BLKSURF(IV,ICFD,i)='NONE        '
                enddo
                if (itrc.gt.0) then
                  write(outs,'(2a)')outs_pre(1:lnblnk(outs_pre)),
     &              ' associated with surface ',WORD(1:12)
                  call edisp(itu,outs)
                endif
              elseif (ND.eq.11) then
                CALL EGETW(OUTSTR,K,WORD,'W','conf',IER)
                IVTYPE(IV,ICFD)=33
                VCsurf(IV,ICFD)='NONE        '
                if (itrc.gt.0) then
                  write(outs,'(2a)')outs_pre(1:lnblnk(outs_pre)),
     &              ' associated per face'
                  call edisp(itu,outs)
                endif
                do i=1,6
                  CALL EGETW(OUTSTR,K,WORD,'W','surface',IER)
                  BLKSURF(IV,ICFD,i)=WORD(1:12)
                  if (itrc.gt.0) then
                    if (i.eq.1) then
                      write(outs,'(2a)')'  east face is surface ',
     &                  WORD(1:12)
                    elseif (i.eq.2) then
                      write(outs,'(2a)')'  west face is surface ',
     &                  WORD(1:12)
                    elseif (i.eq.3) then
                      write(outs,'(2a)')'  north face is surface ',
     &                  WORD(1:12)
                    elseif (i.eq.3) then
                      write(outs,'(2a)')'  south face is surface ',
     &                  WORD(1:12)
                    elseif (i.eq.3) then
                      write(outs,'(2a)')'  high face is surface ',
     &                  WORD(1:12)
                    elseif (i.eq.3) then
                      write(outs,'(2a)')'  low face is surface ',
     &                  WORD(1:12)
                    endif
                    call edisp(itu,outs)
                  endif
                enddo
              endif
            endif
          endif
 112    continue

C Loop back again for next blockage.
        goto 116
      else
        if(itrc.gt.0)then
          call edisp(itu,' ')
        endif
      endif

C Read in `equations to be solved' this includes methods.
      CALLU(ICFD)=.FALSE.
      CALLV(ICFD)=.FALSE.
      CALLW(ICFD)=.FALSE.
      CALLT(ICFD)=.FALSE.
      CALLC(ICFD)=.FALSE.
      KEMDL(ICFD)=.FALSE.
      BUOY(ICFD)=.FALSE.
      BOUSSI(ICFD)=.FALSE.
      ZEROT(ICFD)=.FALSE.
      ZandKE(ICFD)=.FALSE.
      MITzero(ICFD)=.FALSE.
      DO 39 ICTM=1,NCTM(ICFD)
        CALPOL(ICTM,ICFD)=.FALSE.
 39   CONTINUE
      CALLMA(ICFD)=.FALSE.

C Fictitious time steps.
      FLSDTU(ICFD)=1.0E+20
      FLSDTV(ICFD)=1.0E+20
      FLSDTW(ICFD)=1.0E+20
      FLSDTP(ICFD)=1.0E+20
      FLSDTT(ICFD)=1.0E+20
      FLSDTK(ICFD)=1.0E+20
      FLSDTE(ICFD)=1.0E+20

C Number of sweeps for each variable (default).
      NSSWPU(ICFD)=3
      NSSWPV(ICFD)=3
      NSSWPW(ICFD)=3
      NSSWPP(ICFD)=5
      NSSWPK(ICFD)=3
      NSSWPE(ICFD)=3
      NSSWPT(ICFD)=3

C Solution methods.
 118  CALL STRIPC(IFL,OUTSTR,0,ND,1,'solution methods',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','solution mets tag',IER)
      if (WORD(1:19).ne.'*solution_equations') then 
        if (WORD(1:10).eq.'Turbulence') then
          CALL EGETWI(OUTSTR,K,IVAL,0,4,'F','turb method',IER)
          ITURB(ICFD)=IVAL

C Read extra data for specific turb models.
          if (ITURB(ICFD).eq.2) then
            CALL EGETWR(OUTSTR,K,VALS,0.,500.,'W','rMOOT',IER)
            rMOOT(ICFD)=VALS
          elseif (ITURB(ICFD).eq.4) then
            CALL EGETWR(OUTSTR,K,VALS,0.,500.,'W','rMOOT',IER)
            rMOOT(ICFD)=VALS
            CALL EGETWI(OUTSTR,K,IVAL,1,8000,'W','niter',IER)
            nZtoKE(ICFD)=IVAL
          endif

          if (itrc.gt.0) then
            if (ITURB(ICFD).eq.1) then
              write(outs,'(a)')'k-epsilon turbulence model active'
              call edisp(itu,outs)
            elseif (ITURB(ICFD).eq.2) then
              write(outs,'(a,f8.4)')
     & 'Fixed eddy viscosity turbulence model active, eddy viscosity ',
     &                                                      rMOOT(ICFD)
              call edisp(itu,outs)
            elseif (ITURB(ICFD).eq.3) then
              write(outs,'(a)')
     &                      'MIT zero-equation turbulence model active'
              call edisp(itu,outs)
            elseif (ITURB(ICFD).eq.4) then
              write(outs,'(2a,f8.4,a,i4)')
     &    'Fixed eddy viscosity -> k-epsilon turbulence model active,',
     &     ' eddy viscosity ',rMOOT(ICFD),', transition on iteration ',
     &                                                     nZtoKE(ICFD)
              call edisp(itu,outs)
            endif
          endif
        elseif (WORD(1:8).eq.'Buoyancy') then
          CALL EGETWI(OUTSTR,K,IVAL,0,2,'F','buoy method',IER)
          IBUOY(ICFD)=IVAL

C Read extra data for specific buoy models.
          if (IBUOY(ICFD).eq.1) then
            if (K.gt.lnblnk(OUTSTR)) then
              call usrmsg(
     &  'There is no linear relax factor for density. This should be',
     &  'specified on the Buoyancy line.  Setting to 0.5','W')
              URFCD(ICFD)=0.5
            else
              CALL EGETWR(OUTSTR,K,VALS,0.,1.,'W','Den lin relax',IER)
              URFCD(ICFD)=VALS
            endif
          elseif (IBUOY(ICFD).eq.2) then
            CALL EGETWR(OUTSTR,K,VALS,-10.,99.,'-','Tbar',IER)
            Tbar(ICFD)=VALS
          endif
          if (itrc.gt.0) then
            if (IBUOY(ICFD).eq.1) then
              write(outs,'(2a,f4.2)')
     &         'Solving buoyancy by full ideal gas law calculations, ',
     &                                 'relaxation factor ',URFCD(ICFD)
              call edisp(itu,outs)
            elseif (IBUOY(ICFD).eq.2) then
              write(outs,'(2a,f6.2)')
     &              'Solving buoyancy by Boussinesque approximation, ',
     &                              'reference temperature ',Tbar(ICFD)
              call edisp(itu,outs)
            endif
          endif
        else
          call usrmsg('Did not recognise solution method:',WORD,'W')
        endif
        goto 118
      endif

C Check that turbulence is active if conflated.
      if (IBLD.eq.1.and.ITURB(ICFD).eq.0) then
        call usrmsg('You must use a turbulence model with building',
     &           'conflation. Setting k-e as a default.','W')
        ITURB(ICFD)=1
        KEMDL(ICFD)=.true.
      endif

C Solution equations.
C If equations set to true above are not found then the defaults will 
C be used.  The next time the file is written, however, the full list 
C of equations will be included in the file.
 128  CALL STRIPC(IFL,OUTSTR,99,ND,1,'solution equations',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','solution eqns tag',IER)
      if (WORD(1:10).ne.'*iteration') then 

C Pressure equation.
        if (WORD(1:8).eq.'Pressure') then
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Pr init',IER)
          PINIT(ICFD)=VAL
          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
          if (WORD(1:6).eq.'Linear') then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Pr Lrx',IER)
            URFCP(ICFD)=VAL
          endif
          if (ND.eq.5) then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Pr Lrx 2',IER)
            URFCP2(ICFD)=VAL
          endif
          if (itrc.gt.0) then
            write(outs,'(a,a,f8.4,a,f4.2)') 'Solving pressure (P),',
     &                                   ' initial value ',PINIT(ICFD),
     &                               ', relaxation factor ',URFCP(ICFD)
            if (ND.eq.5) then
              outs_pre=outs
              write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCP2(ICFD)
            endif
            call edisp(itu,outs)
          endif
        elseif (WORD(1:3).eq.'Vel') then
          CALL EGETW(OUTSTR,K,WORD,'W','direction',IER)

C Velocity U equation.
          if (WORD(1:1).eq.'U') then
            CALLU(ICFD)=.TRUE.
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Vu init',IER)
            UINIT(ICFD)=VAL
            CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
            if (WORD(1:6).eq.'Linear') then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vu Lrx',IER)
              URFCU(ICFD)=VAL
            endif
            if (ND.eq.6) then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vu Lrx 2',IER)
              URFCU2(ICFD)=VAL
            endif
            if (itrc.gt.0) then
              write(outs,'(a,a,f8.4,a,f4.2)') 'Solving velocity in X ',
     &                    'direction (Vu), initial value ',UINIT(ICFD),
     &                               ', relaxation factor ',URFCU(ICFD)
              if (ND.eq.6) then
                outs_pre=outs
                write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCU2(ICFD)
              endif
              call edisp(itu,outs)
            endif

C Velocity V equation.
          elseif (WORD(1:1).eq.'V') then
            CALLV(ICFD)=.TRUE.
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Vv init',IER)
            VINIT(ICFD)=VAL
            CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
            if (WORD(1:6).eq.'Linear') then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vv Lrx',IER)
              URFCV(ICFD)=VAL
            endif
            if (ND.eq.6) then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vv Lrx 2',IER)
              URFCV2(ICFD)=VAL
            endif
            if (itrc.gt.0) then
              write(outs,'(a,a,f8.4,a,f4.2)') 'Solving velocity in Y ',
     &                    'direction (Vv), initial value ',VINIT(ICFD),
     &                               ', relaxation factor ',URFCV(ICFD)
              if (ND.eq.6) then
                outs_pre=outs
                write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCV2(ICFD)
              endif
              call edisp(itu,outs)
            endif

C Velocity W equation.
          elseif (WORD(1:1).eq.'W') then
            CALLW(ICFD)=.TRUE.
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Vw init',IER)
            WINIT(ICFD)=VAL
            CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
            if (WORD(1:6).eq.'Linear') then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vw Lrx',IER)
              URFCW(ICFD)=VAL
            endif
            if (ND.eq.6) then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Vw Lrx 2',IER)
              URFCW2(ICFD)=VAL
            endif
            if (itrc.gt.0) then
              write(outs,'(a,a,f8.4,a,f4.2)') 'Solving velocity in Z ',
     &                    'direction (Vw), initial value ',WINIT(ICFD),
     &                               ', relaxation factor ',URFCW(ICFD)
              if (ND.eq.6) then
                outs_pre=outs
                write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCW2(ICFD)
              endif
              call edisp(itu,outs)
            endif
          else
            call usrmsg('Did not recognise velocity dir:',WORD,'W')
          endif
        elseif (WORD(1:4).eq.'Temp') then

C Temperature.
          CALLT(ICFD)=.TRUE.
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Tmp init',IER)
          TINIT(ICFD)=VAL
          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
          if (WORD(1:6).eq.'Linear') then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Tmp Lrx',IER)
            URFCT(ICFD)=VAL
          endif
          if (ND.eq.5) then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Tmp Lrx 2',IER)
            URFCT2(ICFD)=VAL
          endif
          if (itrc.gt.0) then
            write(outs,'(a,a,f8.4,a,f4.2)') 'Solving temperature (T),',
     &                                   ' initial value ',TINIT(ICFD),
     &                               ', relaxation factor ',URFCT(ICFD)
            if (ND.eq.5) then
              outs_pre=outs
              write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCT2(ICFD)
            endif
            call edisp(itu,outs)
          endif
        elseif (WORD(1:3).eq.'Ted') then

C Turbulence intensity.
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Te init',IER)
          TEINIT(ICFD)=amax1(VAL,SMALL)
          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
          if (WORD(1:6).eq.'Linear') then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Te Lrx',IER)
            URFCK(ICFD)=VAL
          endif
          if (ND.eq.5) then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Te Lrx 2',IER)
            URFCK2(ICFD)=VAL
          endif
          if (itrc.gt.0) then
            write(outs,'(a,a,f8.4,a,f4.2)') 'Solving turbulence ',
     &                       'energy (k), initial value ',TEINIT(ICFD),
     &                               ', relaxation factor ',URFCK(ICFD)
            if (ND.eq.5) then
              outs_pre=outs
              write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCK2(ICFD)
            endif
            call edisp(itu,outs)
          endif
        elseif (WORD(1:3).eq.'Epd') then

C Turbulence dissipation.
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Ed init',IER)
          EDINIT(ICFD)=amax1(VAL,SMALL)
          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
          if (WORD(1:6).eq.'Linear') then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Ed Lrx',IER)
            URFCE(ICFD)=VAL
          endif
          if (ND.eq.5) then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Ed Lrx 2',IER)
            URFCE2(ICFD)=VAL
          endif
          if (itrc.gt.0) then
            write(outs,'(a,a,f8.4,a,f4.2)') 'Solving turbulence ',
     &      'energy dissipation rate (e), initial value ',EDINIT(ICFD),
     &                               ', relaxation factor ',URFCE(ICFD)
            if (ND.eq.5) then
              outs_pre=outs
              write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                            ', relaxation factor 2 ',URFCE2(ICFD)
            endif
            call edisp(itu,outs)
          endif

C Contaminant information.
        elseif (WORD(1:LNBLNK(WORD)).eq.
     &    POLNAM(1,ICFD)(1:LNBLNK(POLNAM(1,ICFD))))then
          CALPOL(1,ICFD)=.TRUE.
          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Contam init',IER)
          POLINIT(ICFD,1)=VAL
          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
          if (WORD(1:6).eq.'Linear') then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Contam Lrx',IER)
            URFCC(ICFD,1)=VAL
          endif
          if (ND.eq.5) then
            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Contam Lrx 2',IER)
            URFCC2(ICFD,1)=VAL
          endif
          if (itrc.gt.0) then
            write(outs,'(4a,f4.2)')'Solving ',
     &     POLNAM(1,ICFD)(1:LNBLNK(POLNAM(1,ICFD))),' concentration, ',
     &       'initial value not used, relaxation factor ',URFCC(ICFD,1)
            if (ND.eq.5) then
              outs_pre=outs
              write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                          ', relaxation factor 2 ',URFCC2(ICFD,1)
            endif
            call edisp(itu,outs)
          endif
          DO 41 ICTM=2,NCTM(ICFD)
            CALL STRIPC(IFL,OUTSTR,99,ND,1,
     &                                   'contaminant information',IER)
            K=0
            CALPOL(ICTM,ICFD)=.TRUE.
            CALL EGETW(OUTSTR,K,WORD,'-','Contam name',IER)
            CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Contam init',IER)
            POLINIT(ICFD,ICTM)=VAL
            CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
            if (WORD(1:6).eq.'Linear') then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Contam Lrx',IER)
              URFCC(ICFD,ICTM)=VAL
            endif
            if (ND.eq.5) then
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Contam Lrx 2',IER)
              URFCC2(ICFD,ICTM)=VAL
            endif
            if (itrc.gt.0) then
              write(outs,'(4a,f4.2)')'Solving ',
     &                  POLNAM(ICTM,ICFD)(1:LNBLNK(POLNAM(ICTM,ICFD))),
     &                                              ' concentration, ',
     &    'initial value not used, relaxation factor ',URFCC(ICFD,ICTM)
              if (ND.eq.5) then
                outs_pre=outs
                write(outs,'(2a,f4.2)')outs_pre(1:LNBLNK(outs_pre)),
     &                       ', relaxation factor 2 ',URFCC2(ICFD,ICTM)
              endif
              call edisp(itu,outs)
            endif
 41       CONTINUE
        elseif (WORD(1:5).eq.'Lmage') then

C Mean age of air.
           CALLMA(ICFD)=.TRUE.
C          CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','Lma init',IER)
C          EDINIT(ICFD)=VAL
C          CALL EGETW(OUTSTR,K,WORD,'W','relax type',IER)
C          if (WORD(1:6).eq.'Linear') then
C            CALL EGETWR(OUTSTR,K,VAL,0.,1.,'W','Lma Lrx',IER)
C            URFCE(ICFD)=VAL
C          endif
          if (itrc.gt.0) then
            write(outs,'(2a)') 'Solving local mean age of air, ',
     &'initial value not used, relaxation factor not used (always 0.9)'
            call edisp(itu,outs)
          endif
        ELSEIF(WORD(1:4).EQ.'Conc'.OR.WORD(1:3).EQ.'Hum')THEN
          call usrmsg('WARNING: Contaminants defined in old format',
     &    'Delete source type bc and define again using Project Manager'
     &    ,'W')
        else
          call usrmsg('Did not recognise equation type:',WORD,'W')
        endif
        goto 128
      endif
      
C Read iteration control data.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'iteration control',IER)
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,10,15000,'W','max iter',IER)
      MAXITR(ICFD)=IVAL
      CALL EGETWR(OUTSTR,K,VAL,0.,0.,'-','conv crit',IER)
      SRMAX(ICFD)=VAL
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'monitoring',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','monitor',IER)
      if (WORD(1:8).eq.'*monitor') then
        CALL EGETWI(OUTSTR,K,IVAL,1,1000,'W','Imon',IER)
        IMONT(ICFD)=IVAL+1
        CALL EGETWI(OUTSTR,K,IVAL,1,1000,'W','Jmon',IER)
        JMONT(ICFD)=IVAL+1
        CALL EGETWI(OUTSTR,K,IVAL,1,1000,'W','Kmon',IER)
        KMONT(ICFD)=IVAL+1
        CALL EGETW(OUTSTR,K,WORD,'W','CFD monitoring file',IER)
        write(CFTRFL(ICFD),'(2a,i2.2)')WORD(1:lnblnk(WORD)),'_',icfd
      else
        IMONT(ICFD)=INT(NTCX/2)+1
        JMONT(ICFD)=INT(NTCY/2)+1
        KMONT(ICFD)=INT(NTCZ/2)+1
        write(CFTRFL(ICFD),'(a8,i2.2)')'CFD_mon_',icfd
      endif
      if (itrc.gt.0) then
        write(outs,'(a,E10.3,a,i5,a)')
     &            'Convergence criterion is maximum sum of residuals ',
     &               SRMAX(ICFD),' within ',MAXITR(ICFD),' iterations.' 
        call edisp(itu,outs)
        write(outs,'(a,i2,a,i2,a,i2)')'Monitoring cell is ',
     &                      IMONT(ICFD),' ',JMONT(ICFD),' ',KMONT(ICFD)
        call edisp(itu,outs)
      endif
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'initilisation',IER)
      K=0
      if (WORD(1:6).eq.'*reint') then
        CALL EGETW(OUTSTR,K,WORD,'W','yes or no',IER)
        if (WORD(1:2).eq.'NO') then
          INITFL(ICFD)=.FALSE.
        else
          INITFL(ICFD)=.TRUE.
        endif
      endif

C Read frequency of result writing.
      CALL STRIPC(IFL,OUTSTR,0,ND,1,'svf',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','*svf',IER)
      if (WORD(1:4).eq.'*svf') then
        CALL EGETWI(OUTSTR,K,IVAL,1,1000,'W','results freq.',IER)
        IPPHI(ICFD)=IVAL
      else

C Unrecognised line, set error flag.
        IER=1
      endif
C << Removed hardcoding of results writing frequency >>
C      IPPHI(ICFD)=1
      LPHI(ICFD)='CFD_new_phi'

C Print message about version number 
      IF(VER.GT.2.05.AND.VER.LT.2.15)THEN
        CALL EDISP(iuout,'dfd file version 2.1 detected, if you have')
        CALL EDISP(iuout,'contaminants defined then only these need to')
        CALL EDISP(iuout,'be redefined. File will be updated to newest')
        CALL EDISP(iuout,'version when saved.')
      ENDIF

      call NEW2OLD

      RETURN
      END

C ********************* NEW2OLD *********************
C Copies new data structure to old.

      SUBROUTINE NEW2OLD
#include "building.h"
#include "cfd.h"

C Grid and Geometry definition:
C   NREG(?,ICFD)     - Number of regions in each direction (?: 1- x, 2- y, 3- z).
C   NCELX(IREG,ICFD) - Number of cells in x-direction per region (IREG) per
C                      domain (ICFD).
C   NCELY(IREG,ICFD) - Number of cells in y-direction per region per domain (ICFD).
C   NCELZ(IREG,ICFD) - Number of cells in z-direction per region per domain (ICFD.
C   NCELZe(IREG,ICFD)- Number of cells in z (east)-direction.

C   XREG(IREG,ICFD)  - x-dimension of the region IREG per domain (ICFD).
C   YREG(IREG,ICFD)  - y-dimension of the region IREG per domain (ICFD).
C   ZREG(IREG,ICFD)  - z-dimension of the region IREG per domain (ICFD).
C   ZREGe(IREG,ICFD) - z (east) -dimension of the region IREG per domain (ICFD).

C   Xplaw(IREG,ICFD) - x-power law coefficient of region IREG per domain (ICFD).
C   Yplaw(IREG,ICFD) - y-power law coefficient of region IREG per domain (ICFD). 
C   Zplaw(IREG,ICFD) - West wall z-power law coef of region IREG per domain (ICFD).
C   Zplawe(IREG,ICFD)- East wall z-power law coef of region IREG per domain (ICFD)
C                     (used in the case of curvilinear Z).
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &              IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &              JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &              KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &              FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &              FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &              IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &              ICNACT(MCFND,MNZ),IVOLNOP(MCFND,MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &              INSSO(MNVLS,MNZ),BLKTEMP(MNVLS,6)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &              CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &              ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/PRSREF/IPRESF(MNZ),JPRESF(MNZ),KPRESF(MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/grdmax/NTCX,NTCY,NTCZ

C Solution methods.
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)

C Boundary condition commons.  
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12, VCsurf*12, BLKSURF*12
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      LOGICAL CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero

C Turbulence model.
      if (ITURB(ICFD).eq.1) then
        KEMDL(ICFD)=.true.
      elseif (ITURB(ICFD).eq.2) then
        ZEROT(ICFD)=.true.
      elseif (ITURB(ICFD).eq.3) then
        MITzero(ICFD)=.true.
      elseif (ITURB(ICFD).eq.4) then
        ZandKE(ICFD)=.true.
      endif

C Buoyancy.
      if (IBUOY(ICFD).eq.1) then
        BUOY(ICFD)=.true.
      elseif (IBUOY(ICFD).eq.2) then
        BUOY(ICFD)=.true.
        BOUSSI(ICFD)=.true.
      endif

C Boundary conditions.
      ISB=0
      IOP=0
      IBK=0
      ISBND = 0

C Solid boundaries.
      do 10 IKV=1,NVOL(ICFD)
        if (IVTYPE(IKV,ICFD).eq.1) then
C        ltmp='Temperature'
          ISB=ISB+1
          IVOLNSB(ISB,ICFD)=IKV
          IWSB(ISB,ICFD)=IVOLF(IKV,ICFD)
          ISBi(ISB,ICFD)=IVCELLS(IKV,ICFD,1)
          ISBf(ISB,ICFD)=IVCELLS(IKV,ICFD,2)
          JSBi(ISB,ICFD)=JVCELLS(IKV,ICFD,1)
          JSBf(ISB,ICFD)=JVCELLS(IKV,ICFD,2)
          KSBi(ISB,ICFD)=KVCELLS(IKV,ICFD,1)
          KSBf(ISB,ICFD)=KVCELLS(IKV,ICFD,2)
          SSB(ISB,ICFD)=VOLTemp(IKV,ICFD)
          ISBND=ISBND+1
          ISUFLC(ISB,ICFD)=ISBND
        elseif (IVTYPE(IKV,ICFD).eq.2) then
C        ltmp='Heat flux'
          ISB=ISB+1
          IVOLNSB(ISB,ICFD)=IKV
          IWSB(ISB,ICFD)=-IVOLF(IKV,ICFD)
          ISBi(ISB,ICFD)=IVCELLS(IKV,ICFD,1)
          ISBf(ISB,ICFD)=IVCELLS(IKV,ICFD,2)
          JSBi(ISB,ICFD)=JVCELLS(IKV,ICFD,1)
          JSBf(ISB,ICFD)=JVCELLS(IKV,ICFD,2)
          KSBi(ISB,ICFD)=KVCELLS(IKV,ICFD,1)
          KSBf(ISB,ICFD)=KVCELLS(IKV,ICFD,2)
          SSB(ISB,ICFD)=VOLHeat(IKV,ICFD)
          ISBND=ISBND+1
          ISUFLC(ISB,ICFD)=ISBND
        elseif (IVTYPE(IKV,ICFD).eq.3) then
C        ltmp='Symmetrical'
          ISB=ISB+1
          IVOLNSB(ISB,ICFD)=IKV
          IWSB(ISB,ICFD)=IVOLF(IKV,ICFD)+10
          ISBi(ISB,ICFD)=IVCELLS(IKV,ICFD,1)
          ISBf(ISB,ICFD)=IVCELLS(IKV,ICFD,2)
          JSBi(ISB,ICFD)=JVCELLS(IKV,ICFD,1)
          JSBf(ISB,ICFD)=JVCELLS(IKV,ICFD,2)
          KSBi(ISB,ICFD)=KVCELLS(IKV,ICFD,1)
          KSBf(ISB,ICFD)=KVCELLS(IKV,ICFD,2)
          ISBND=ISBND+1
          ISUFLC(ISB,ICFD)=ISBND
        elseif (IVTYPE(IKV,ICFD).eq.4.or.IVTYPE(IKV,ICFD).eq.5.or.
     &          IVTYPE(IKV,ICFD).eq.6) then
C        ltmp='Conflated  '
          ISB=ISB+1
          IVOLNSB(ISB,ICFD)=IKV
          IWSB(ISB,ICFD)=IVOLF(IKV,ICFD)
          ITCtype(ISB,ICFD)=IVConfl(IKV,ICFD)

C Now adjust IWSB for thermal conflation type. Set icTREF where applicable.
            if(ITCtype(ISB,ICFD).eq.1)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD)
            elseif(ITCtype(ISB,ICFD).eq.2)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 50
            elseif(ITCtype(ISB,ICFD).eq.3)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 60
            elseif(ITCtype(ISB,ICFD).eq.4)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 20
              icTREF(ISB,ICFD) = 1
            elseif(ITCtype(ISB,ICFD).eq.5)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 20
              icTREF(ISB,ICFD) = 2
            elseif(ITCtype(ISB,ICFD).eq.6)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 30
              icTREF(ISB,ICFD) = 1
            elseif(ITCtype(ISB,ICFD).eq.7)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 30
              icTREF(ISB,ICFD) = 2
            elseif(ITCtype(ISB,ICFD).eq.8)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 40
            elseif(ITCtype(ISB,ICFD).eq.9)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD)
            elseif(ITCtype(ISB,ICFD).eq.10)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 50
            elseif(ITCtype(ISB,ICFD).eq.11)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 60
            elseif(ITCtype(ISB,ICFD).eq.12)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 40
            elseif(ITCtype(ISB,ICFD).eq.13)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD)
            elseif(ITCtype(ISB,ICFD).eq.14)then
              IWSB(ISB,ICFD) = IWSB(ISB,ICFD) + 40
            endif
          ISBi(ISB,ICFD)=IVCELLS(IKV,ICFD,1)
          ISBf(ISB,ICFD)=IVCELLS(IKV,ICFD,2)
          JSBi(ISB,ICFD)=JVCELLS(IKV,ICFD,1)
          JSBf(ISB,ICFD)=JVCELLS(IKV,ICFD,2)
          KSBi(ISB,ICFD)=KVCELLS(IKV,ICFD,1)
          KSBf(ISB,ICFD)=KVCELLS(IKV,ICFD,2)
          ISUFLC(ISB,ICFD)=IDSURF(VCsurf(IKV,ICFD))

C Air flow openings.
        elseif (IVTYPE(IKV,ICFD).eq.10) then
C        ltmp='Pressure'
          IOP=IOP+1
          IVOLNOP(IOP,ICFD)=IKV
          IWOPEN(IOP,ICFD)=0
          IOPENi(IOP,ICFD)=IVCELLS(IKV,ICFD,1)
          IOPENf(IOP,ICFD)=IVCELLS(IKV,ICFD,2)
          JOPENi(IOP,ICFD)=JVCELLS(IKV,ICFD,1)
          JOPENf(IOP,ICFD)=JVCELLS(IKV,ICFD,2)
          KOPENi(IOP,ICFD)=KVCELLS(IKV,ICFD,1)
          KOPENf(IOP,ICFD)=KVCELLS(IKV,ICFD,2)
          FIXM(IOP,ICFD)=VOLPres(IKV,ICFD)
          FIXT(IOP,ICFD)=VOLTemp(IKV,ICFD)
          FIXC(IOP,ICFD)=VOLCO2(IKV,ICFD)
        elseif (IVTYPE(IKV,ICFD).eq.11) then
C        ltmp='Velocity'
          IOP=IOP+1
          IVOLNOP(IOP,ICFD)=IKV
          IWOPEN(IOP,ICFD)=IVOLF(IKV,ICFD)
          IOPENi(IOP,ICFD)=IVCELLS(IKV,ICFD,1)
          IOPENf(IOP,ICFD)=IVCELLS(IKV,ICFD,2)
          JOPENi(IOP,ICFD)=JVCELLS(IKV,ICFD,1)
          JOPENf(IOP,ICFD)=JVCELLS(IKV,ICFD,2)
          KOPENi(IOP,ICFD)=KVCELLS(IKV,ICFD,1)
          KOPENf(IOP,ICFD)=KVCELLS(IKV,ICFD,2)
          FIXM(IOP,ICFD)=VOLVel(IKV,ICFD)
          FIXT(IOP,ICFD)=VOLTemp(IKV,ICFD)
          FIXC(IOP,ICFD)=VOLCO2(IKV,ICFD)
        elseif (IVTYPE(IKV,ICFD).eq.12) then
C        ltmp='Zero gradient'
          IOP=IOP+1
          IVOLNOP(IOP,ICFD)=IKV
          IWOPEN(IOP,ICFD)=-IVOLF(IKV,ICFD)
          IOPENi(IOP,ICFD)=IVCELLS(IKV,ICFD,1)
          IOPENf(IOP,ICFD)=IVCELLS(IKV,ICFD,2)
          JOPENi(IOP,ICFD)=JVCELLS(IKV,ICFD,1)
          JOPENf(IOP,ICFD)=JVCELLS(IKV,ICFD,2)
          KOPENi(IOP,ICFD)=KVCELLS(IKV,ICFD,1)
          KOPENf(IOP,ICFD)=KVCELLS(IKV,ICFD,2)
          FIXT(IOP,ICFD)=VOLTemp(IKV,ICFD)
        elseif (IVTYPE(IKV,ICFD).eq.13) then
C        ltmp='Mass flow'
          IOP=IOP+1
          IVOLNOP(IOP,ICFD)=IKV
          IWOPEN(IOP,ICFD)=99
          IOPENi(IOP,ICFD)=IVCELLS(IKV,ICFD,1)
          IOPENf(IOP,ICFD)=IVCELLS(IKV,ICFD,2)
          JOPENi(IOP,ICFD)=JVCELLS(IKV,ICFD,1)
          JOPENf(IOP,ICFD)=JVCELLS(IKV,ICFD,2)
          KOPENi(IOP,ICFD)=KVCELLS(IKV,ICFD,1)
          KOPENf(IOP,ICFD)=KVCELLS(IKV,ICFD,2)
          FIXM(IOP,ICFD)=VOLVel(IKV,ICFD)
          FIXT(IOP,ICFD)=VOLTemp(IKV,ICFD)
          FIXC(IOP,ICFD)=VOLCO2(IKV,ICFD)
        elseif (IVTYPE(IKV,ICFD).ge.30 .and. 
     &          IVTYPE(IKV,ICFD).le.35) then
          IBK=IBK+1
          INBLK(IBK,ICFD)=IKV
        endif
 10   continue

      NSB(ICFD)=ISB 
      NOPEN(ICFD)=IOP
      NBLK(ICFD)=IBK

C Pressure reference position if no openings.
      IF(NOPEN(ICFD).EQ.0)THEN
        IPRESF(ICFD)=INT(NTCX/2)+2
        JPRESF(ICFD)=INT(NTCY/2)+2
        KPRESF(ICFD)=INT(NTCZ/2)+2
      else

C If openings, assume a position for reference pressure.
        IPRESF(ICFD)=IOPENi(1,ICFD)
        JPRESF(ICFD)=JOPENi(1,ICFD)
        KPRESF(ICFD)=KOPENi(1,ICFD)

C If an opening is a pressure or mass type then use it in preference
C to the first opening.
        DO 302 L=1,NOPEN(ICFD)
          IF(IWOPEN(L,ICFD).EQ.0.OR.IWOPEN(L,ICFD).EQ.99)THEN
            IPRESF(ICFD)=IOPENi(L,ICFD)
            JPRESF(ICFD)=JOPENi(L,ICFD)
            KPRESF(ICFD)=KOPENi(L,ICFD)
            GOTO 321
          ENDIF
 302    CONTINUE
      endif
 321  continue

      RETURN
      END


C ********************* CFMFSU *********************
C Set up CFD to mass flow connections/ components.
C All connections to the dom_nod (ICFDNOD) are deactivated. 
C A new node (opn_nod) is created for this opening in the CFD domain. 
C A new connection from this node is made to the node, ICFDNOD was 
C connected to (ext_nod), using the same component.
C Consequentially the number of new nodes created will be equal to the
C number of openings in CFD domain. The nodes created here are internal
C and constant pressure (type 1) type nodes.
C IV is the index of the key volume number.
C ITRC signals verbose reporting if greater than zero.
C IND is the index of the node.
C ICN is the index of the connection.
C OPENNAM (char 12) name of the opening.

      subroutine CFMFSU(IV,ITRC,IND,ICN,OPENNAM)
#include "building.h"
#include "cfd.h"
#include "net_flow.h"
#include "net_flow_data.h"

C Passed parameters.
      integer iv,itrc,ind,icn
      character OPENNAM*12
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/ICFNOD/ICFD,ICP
      COMMON/MFCFD/NCNRMV(MNZ),ICNRMV(MCFND,MNZ),
     &             NCNON(MNZ),ICNON(2*MCFND,MNZ)
      COMMON/ACT/ACTCNN(MCNN)
      COMMON/CFDMFS2/MFNEWN(MNVLS,MNZ),NFNDN,NCFDOPNO(MNVLS,MNZ),
     &               NFNPDN(MNZ)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      character*12 LCFND,LEXND,LCOMPNT
      character outs*124,CHECKER1*12,OPNO*10,ICFDSTR*10
      LOGICAL OK,ACTCNN
      double precision FLW1,FLW2,PRES,RESID,SAFLW

C Get CFD opening number and CFD domain number as integer.
      IER=0
      CALL INTSTR(IV,OPNO,ILN,IER)
      IF(IER.NE.0)RETURN
      CALL INTSTR(ICFD,ICFDSTR,ICFDLN,IER)
      IF(IER.NE.0)RETURN

C Check if this subroutine was called previously for the same opening
C If yes then this subroutine should not run again (because it will make
C more nodes and connections each time it is run).
      OK=.FALSE.
      DO 1 I=1,NNOD
        write(CHECKER1,'(3a)')ICFDSTR(1:ICFDLN),OPNO(1:ILN),
     &    OPENNAM(1:(12-ILN-ICFDLN))
        IF(NDNAM(I).EQ.CHECKER1)OK=.TRUE.
 1    CONTINUE
      IF(OK)GOTO 2

      ICNN=ICN
      LCFND=NDNAM(IND)
      IF(NODPS(ICNN).EQ.IND)THEN
        LEXND=NDNAM(NODNE(ICNN))
        INDX=NODNE(ICNN)
      ELSEIF(NODNE(ICNN).EQ.IND)THEN
        LEXND=NDNAM(NODPS(ICNN))
        INDX=NODPS(ICNN)
      ENDIF
      ICMP=ITPCON(ICNN)
      LCOMPNT=CMNAM(ICMP)

C Remove all connections with original node
      OK=.TRUE.
      IF(ICNN.GT.NCNN)OK=.FALSE.
      if (.not.OK) then
        write (outs,*) 'Could not find connection, ',LCOMPNT,
     &                 'for nodes ',LCFND,' & ',LEXND,' returning.'
        call edisp(iuout,outs)
        return
      elseif (itrc.gt.0) then
        write(outs,'(6a)')'Found connection for nodes: ',LCFND,
     &        ' & ',LEXND,' component ',LCOMPNT
        call edisp(iuout,outs)
      endif

C This connection will be deactivated while CFD is active, remember this
C NCNRMV = number of connections to remove when CFD active.
C ICNRMV = list of connections to remove when CFD active.
      NCNRMV(ICFD)=NCNRMV(ICFD)+1
      ICNRMV(NCNRMV(ICFD),ICFD)=ICNN

C A new node must be created to represent the control volumes of the 
C opening.
      NNOD=NNOD+1
      NDNAM(NNOD)=ICFDSTR(1:ICFDLN)//OPNO(1:ILN)
     &  //OPENNAM(1:(12-ILN-ICFDLN))
      NDFLD(NNOD)=1
      NDTYP(NNOD)=1
      HNOD(NNOD,1)=HNOD(IND,1)
      HNOD(NNOD,2)=HNOD(IND,2)
      HNOD(NNOD,3)=HNOD(IND,3)
      ITND(NNOD)=IND
      TNOD(NNOD)=TNOD(IND)
      PRES(NNOD)=0.

C Remember information about this new node as well as the CFD
C opening type boundary condition that this node represents.
      NFNDN=NFNDN+1
      NFNPDN(ICFD)=NFNPDN(ICFD)+1
      MFNEWN(NFNPDN(ICFD),ICFD)=NNOD
      NCFDOPNO(NFNPDN(ICFD),ICFD)=IV

C Following code is commented out because nodes representative of
C openings will not have a volume SUPNOD(*,1)
C      SUPNOD(NNOD,1)=SUPNOD(IND,1)
C      SUPNOD(NNOD,2)=SUPNOD(IND,2)

C New connection between new node and node originally connected to
C CFD domain node, using same component i.e. replacement connection.
      NCNN=NCNN+1
      IF(NODPS(ICNN).EQ.IND)THEN
        NODPS(NCNN) = NNOD
        NODNE(NCNN) = INDX
      ELSEIF(NODNE(ICNN).EQ.IND)THEN
        NODPS(NCNN) = INDX
        NODNE(NCNN) = NNOD
      ENDIF
      HGTPS(NCNN) = HGTPS(ICNN)
      HGTNE(NCNN) = HGTNE(ICNN)
      ITPCON(NCNN)= ICMP
      DO 20 IQ=1,MCNS 
        NDSCNN(NCNN,IQ)=NDSCNN(ICNN,IQ)
 20   CONTINUE

C NCNON = number of connections to activate for CFD connections.
C ICNON = list of connections to activate for CFD connections.
      NCNON(ICFD)=NCNON(ICFD)+1
      ICNON(NCNON(ICFD),ICFD)=NCNN

C Deactivate CFD connection created.
      ACTCNN(ICNON(NCNON,ICFD))=.FALSE.

 2    return
      end

C ******************** MFLISTemp ********************
C List common block contents for debugging only.

      SUBROUTINE MFLISTemp
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      CHARACTER outs*124,fldtyp*6,nodtyp*20

C Number of nodes, components and interconnections.
      WRITE(outs,1010) NNOD,NCMP,NCNN,WRED
 1010 FORMAT(1X,I3,' nodes, ',I3,' components, ',I3,' connections;',
     &       5X,'wind reduction = ',F6.3)
      CALL EDISP(IUOUT,outs)

C Node data.
      IF(NNOD.EQ.0) GOTO 999
      WRITE(outs,'(A,A)')'    Node      Fluid   Node Type',
     &  '            Height    Temperature    Data_1       Data_2'
      CALL EDISP(IUOUT,outs)

      DO 30 INOD=1,NNOD
        if(NDFLD(INOD).eq.1)then
          fldtyp='air   '
        elseif(NDFLD(INOD).eq.2)then
          fldtyp='water '
        endif
        if(NDTYP(INOD).eq.0)then
          nodtyp='internal & unknown  '
        elseif(NDTYP(INOD).eq.1)then
          nodtyp='internal & known    '
        elseif(NDTYP(INOD).eq.2)then
          nodtyp='boundary & known    '
        elseif(NDTYP(INOD).eq.3)then
          nodtyp='boundary & wind ind '
        endif
        IF(ITND(INOD).EQ.0) THEN
          WRITE(outs,1030)
     &         NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD,3),
     &         TNOD(INOD),SUPNOD(INOD,1),SUPNOD(INOD,2)
        ELSE
          WRITE(outs,10301)
     &         NDNAM(INOD),fldtyp,nodtyp,HNOD(INOD,3),
     &         NDNAM(ITND(INOD)),SUPNOD(INOD,1),SUPNOD(INOD,2)
        END IF
        call edisp(iuout,outs)
   30 CONTINUE
 1030   FORMAT(1X,A12,1x,a,a,G12.5,1X,G12.5,   1X,G12.5,1X,G12.5)
10301   FORMAT(1X,A12,1x,a,a,G12.5,1X,3X,A6,3X,1X,G12.5,1X,G12.5)

C Component data.
      IF(NCMP.EQ.0) GOTO 999
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' Component   Type C+ L+ Description')

      DO 50 ICMP=1,NCMP
        call edisp(iuout,' ')
        WRITE(outs,1050)CMNAM(ICMP),ITPCMP(ICMP),
     &      ISDCMP(ICMP),ISDCNN(ICMP),LTPCMP(ICMP)
 1050   FORMAT(1X,A12,I5,I3,I3,1X,A60)
        CALL EDISP(IUOUT,outs)

C Provide a heading string for each different component type.
        IF(ITPCMP(ICMP).EQ. 10) THEN
          call edisp(iuout,'   Fluid, flow coef, flow exp.')
        ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
          call edisp(iuout,'   Fluid, flow coef, flow exp.')
        ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
          call edisp(iuout,'   Fluid, flow coef, flow exp.')
        ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
          call edisp(iuout,'   Fluid, flow coef, flow exp.')
        ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
          call edisp(iuout,'   Fluid, flow coef, flow exp.')
        ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
          call edisp(iuout,'   Fluid, flow rate (m^3/s).')
        ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
          call edisp(iuout,'   Fluid, flow rate (kg/s).')
        ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
          call edisp(iuout,'   Fluid, opening area, discharge factor.')
        ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
          call edisp(iuout,'   Fluid, pipe length, pipe radius.')
        ELSE IF(ITPCMP(ICMP).EQ.110) THEN
          call edisp(iuout,'   Fluid, opening area.')
        ELSE IF(ITPCMP(ICMP).EQ.120) THEN
          call edisp(iuout,'   Fluid, crack width, crack length.')
        ELSE IF(ITPCMP(ICMP).EQ.130) THEN
          call edisp(iuout,
     &  '   Fluid, door width & height, ref ht, discharge factor.')
        ELSE IF(ITPCMP(ICMP).EQ.210) THEN
          call edisp(iuout,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
        ELSE IF(ITPCMP(ICMP).EQ.220) THEN
          call edisp(iuout,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
          call edisp(iuout,' x-sect of junct, Ccp coef a0 - a5.')
        ELSE IF(ITPCMP(ICMP).EQ.230) THEN
          call edisp(iuout,
     &    ' Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
          call edisp(iuout,' x-sect of junct, Ccp coef a0 - a5.')
        ELSE IF(ITPCMP(ICMP).EQ.240) THEN
          call edisp(iuout,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
          call edisp(iuout,'   x-sect of junct, Ccp coef a0 - a9.')
        ELSE IF(ITPCMP(ICMP).EQ.250) THEN
          call edisp(iuout,
     &  '   Fluid, hydr diam, x-sect, conduit ln, roughness, loss fac.')
          call edisp(iuout,'   x-sect of junct, Ccp coef a0 - a9.')
        ELSE IF(ITPCMP(ICMP).EQ.310) THEN
          call edisp(iuout,
     &  '   Fluid, lower & upper vol rate, flow coef a0 - a3')
        ELSE IF(ITPCMP(ICMP).EQ.410) THEN
          call edisp(iuout,
     &  '   Fluid, density, pres dif, lin/log, open & closed vol flow,')
          call edisp(iuout,
     &  '   low validity limit')
        ELSE IF(ITPCMP(ICMP).EQ.420) THEN
          call edisp(iuout,
     &  '   Fluid, x-sec, coef a0-a3')
        ELSE IF(ITPCMP(ICMP).EQ.460) THEN
          call edisp(iuout,
     &  '   Fluid, flow when S < Ssp, flow when S > Ssp.')
        endif

        if(ISDCMP(ICMP).LE.9)then
          WRITE(outs,'(9F8.2)')(SUPCMP(ICMP,I), I=1,ISDCMP(ICMP))
          CALL EDISP(IUOUT,outs)
        elseif(ISDCMP(ICMP).GT.9)then
          WRITE(outs,'(9F8.2)')(SUPCMP(ICMP,I), I=1,9)
          CALL EDISP(IUOUT,outs)
          WRITE(outs,'(9F8.2)')(SUPCMP(ICMP,I), I=10,ISDCMP(ICMP))
          CALL EDISP(IUOUT,outs)
        endif
   50 CONTINUE

C Connections data.
      IF(NCNN.EQ.0) GOTO 999
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' Connections')
      WRITE(outs,'(2a)')' Index   +Node   dHght   -Node   dHght   ',
     &                                        'Component  Snod1  Snod2'
      CALL EDISP(IUOUT,outs)

      DO 70 ICNN=1,NCNN
        WRITE(outs,1070)
     &      ICNN,NDNAM(NODPS(ICNN)),HGTPS(ICNN),
     &      NDNAM(NODNE(ICNN)),HGTNE(ICNN),
     &      CMNAM(ITPCON(ICNN)),
     &      NDNAM(NDSCNN(ICNN,1)),NDNAM(NDSCNN(ICNN,2))
 1070   FORMAT(1X,I3,3X,A12,F7.3,3X,A12,F7.3,3X,A12,3X,A12,1X,A12)
        CALL EDISP(IUOUT,outs)
   70 CONTINUE

  100 RETURN

C Error trap.
  999 CALL USRMSG(' ',' MFLIST: no more information available!','W')
      GOTO 100

      END


C ********************* CFDDEFLT *********************
C Sets default values for all data items.

      SUBROUTINE CFDDEFLT
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/MFCFD/NCNRMV(MNZ),ICNRMV(MCFND,MNZ),
     &             NCNON(MNZ),ICNON(2*MCFND,MNZ)
      COMMON/FDTRFC/FLSDTU(MNZ),FLSDTV(MNZ),FLSDTW(MNZ),FLSDTP(MNZ),
     &              FLSDTT(MNZ),FLSDTK(MNZ),FLSDTE(MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      COMMON/LINRFC2/URFCU2(MNZ),URFCV2(MNZ),URFCW2(MNZ),URFCP2(MNZ),
     &              URFCT2(MNZ),URFCK2(MNZ),URFCE2(MNZ),URFCVS2(MNZ),
     &              URFCC2(MNZ,MCTM)
      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/INITIA/UINIT(MNZ),VINIT(MNZ),WINIT(MNZ),PINIT(MNZ),
     &              TINIT(MNZ),TEINIT(MNZ),EDINIT(MNZ),POLINIT(MNZ,MCTM)
      common/cfdini/INITFL(MNZ)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      CHARACTER*12 POLNAM
      LOGICAL CALLMA,CALPOL
      COMMON/NSSWP/NSSWPU(MNZ),NSSWPV(MNZ),NSSWPW(MNZ),NSSWPP(MNZ),
     &             NSSWPT(MNZ),NSSWPK(MNZ),NSSWPE(MNZ)
      common/METHDS/ITURB(MNZ),IBUOY(MNZ)
      COMMON/CONST/GREAT,small,GRAV
      common/grdmax/NTCX,NTCY,NTCZ
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     &             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/ZTURB/rMOOT(MNZ),nZtoKE(MNZ)
      common/fvisgo/gophrun
      LOGICAL gophrun
      COMMON/GGDH/GENB1(ntcelx,ntcely,ntcelz),GGDH
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      
      LOGICAL INITFL,CALLU,CALLV,CALLW,CALLT,CALLC,KEMDL,BUOY,BOUSSI
      LOGICAL ZEROT,ZandKE,MITzero,GGDH

C Default not adaptive control.
      gophrun=.false.
      
C Clear mass flow links.
      NCNRMV(ICFD)=0
      NCNON(ICFD)=0

C Equations to be solved, this includes methods.
      CALLT(ICFD)=.TRUE.
      DO 39 ICTM=1,NCTM(ICFD)
        CALPOL(ICTM,ICFD)=.FALSE.
 39   CONTINUE
      CALLMA(ICFD)=.FALSE.
      ITURB(ICFD)=1
      IBUOY(ICFD)=2
      TBAR(ICP)=20.0
      INITFL(ICFD)=.FALSE.
      GGDH=.FALSE.

C Relaxation factors.
      FLSDTU(ICFD)=GREAT
      FLSDTV(ICFD)=GREAT
      FLSDTW(ICFD)=GREAT
      FLSDTP(ICFD)=GREAT
      FLSDTT(ICFD)=GREAT
      FLSDTK(ICFD)=GREAT
      FLSDTE(ICFD)=GREAT
      URFCU(ICFD) = 0.5
      URFCV(ICFD) = 0.5
      URFCW(ICFD) = 0.5
      URFCP(ICFD) = 0.7
      URFCT(ICFD) = 1.0
      URFCK(ICFD) = 1.0
      URFCE(ICFD) = 1.0
      URFCVS(ICFD) = 1.0
      URFCD(ICFD) = 1.0
      do ICON=1,MCTM
        URFCC(ICFD,ICON) = 1.0
        URFCC2(ICFD,ICON) = 0.25
      enddo
      URFCU2(ICFD) = 0.05
      URFCV2(ICFD) = 0.05
      URFCW2(ICFD) = 0.05
      URFCP2(ICFD) = 0.05
      URFCT2(ICFD) = 0.25
      URFCK2(ICFD) = 0.05
      URFCE2(ICFD) = 0.05
      URFCVS2(ICFD) = 0.25

C Number of sweeps for each variable (default).
      NSSWPU(ICFD)=3
      NSSWPV(ICFD)=3
      NSSWPW(ICFD)=3
      NSSWPP(ICFD)=5
      NSSWPK(ICFD)=3
      NSSWPE(ICFD)=3
      NSSWPT(ICFD)=3

C Initial Values.
      TINIT(ICFD)=20.
      UINIT(ICFD)=0.001
      VINIT(ICFD)=0.001
      WINIT(ICFD)=0.001
      PINIT(ICFD)=0.
      TEINIT(ICFD)=0.005
      EDINIT(ICFD)=0.005
      rMOOT(ICFD)=90.0

C Monitoring cell.
      IMONT(ICFD)=1+NTCX/2
      JMONT(ICFD)=1+NTCY/2
      KMONT(ICFD)=1+NTCZ/2

C Convergence criteria
      MAXITR(ICFD)=5000
      SRMAX(ICFD)=0.01

      return
      end

C ************************ SCHCHK ************************
C Checks the turbulent schmidt number of all containments
C defined in the CFD domain to be non - zero. If any 
C contaminant is zero it is set to the default value unity.

      SUBROUTINE SCHCHK
#include "building.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      COMMON/SCHMTT/SCHMT(MCTM,MNZ),GFM(MCTM,MNZ),VCRIT(MCTM,MNZ),
     &       TBOIL(MCTM,MNZ),TCRIT(MCTM,MNZ),ISCHMT(MCTM,MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL ISCLOSE,CALLMA,CALPOL
      CHARACTER POLNAM*12

      ISCLOSE=.FALSE.
      DO 234 ICTM=1,NCTM(ICFD)
        CALL ECLOSE(SCHMT(ICTM,ICFD),0.0,0.01,ISCLOSE)
        IF(ISCLOSE)SCHMT(ICTM,ICFD)=1.0
 234  CONTINUE
      RETURN
      END

C ******************** BCEKCK ********************
C Checks the boundary conditions of all cells.
C itrc signals verbose reporting if greater than zero.

      SUBROUTINE BCEKCK(ITRC)
#include "building.h"
#include "cfd.h"
      
      integer lnblnk  ! function definition

C Passed parameter.
      integer itrc

      COMMON/ICFNOD/ICFD,ICP
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVOLN/VOLNAME(MNVLS,MNZ),VCsurf(MNVLS,MNZ),
     &               BLKSURF(MNVLS,MNZ,6)
      character VOLNAME*12,VCsurf*12,BLKSURF*12
      common/grdmax/NTCX,NTCY,NTCZ

      character OUTSTR*124,FACE*7,STARST*72
      INTEGER IXZST(NTCELX,NTCELZ),IXYHH(NTCELX,NTCELY)
      INTEGER IYZET(NTCELY,NTCELZ),IXZNT(NTCELX,NTCELZ)
      INTEGER IXYLL(NTCELX,NTCELY),IYZWT(NTCELY,NTCELZ)
      INTEGER IBLKG(NTCELX,NTCELY,NTCELZ)
      LOGICAL WARN
C      logical unixok

C Clear arrays for boundary conditions on north, south, east & west face. 
       DO 45 II=1,NTCX
         DO 46 KK=1,NTCZ
           IXZST(II,KK)=0
           IXZNT(II,KK)=0         
 46      CONTINUE
         DO 47 JJ=1,NTCY
           IXYHH(II,JJ)=0
           IXYLL(II,JJ)=0
 47      CONTINUE
 45    CONTINUE
       DO 48 JJ=1,NTCY
         DO 49 KK=1,NTCZ
           IYZET(JJ,KK)=0
           IYZWT(JJ,KK)=0         
 49      CONTINUE
 48    CONTINUE
      do II=1,NTCX
        do JJ=1,NTCY
          do KK=1,NTCZ
            IBLKG(II,JJ,KK)=0
          enddo
        enddo
      enddo

      WARN=.FALSE.
      NBC=NVOL(ICFD)      
      DO 1 IBC=1,NBC
        IF(IVOLF(IBC,ICFD).EQ.1)THEN      

C West facing boundary conditions.
          FACE='West'
          IF(IVCELLS(IBC,ICFD,2).EQ.2)THEN
            DO 101 IY=JVCELLS(IBC,ICFD,1)-1,JVCELLS(IBC,ICFD,2)-1
              DO 102 IZ=KVCELLS(IBC,ICFD,1)-1,KVCELLS(IBC,ICFD,2)-1
                IYZWT(IY,IZ)=IYZWT(IY,IZ)+1
 102          CONTINUE
 101        CONTINUE
          ELSE
            WARN=.TRUE.
            STARST='It should start at cell # 1 in x direction'
          ENDIF
        ELSEIF(IVOLF(IBC,ICFD).EQ.2)THEN      

C East facing boundary conditions.
          FACE='East'
          IF(IVCELLS(IBC,ICFD,1).EQ.NTCX+1)THEN
            DO 201 IY=JVCELLS(IBC,ICFD,1)-1,JVCELLS(IBC,ICFD,2)-1
              DO 202 IZ=KVCELLS(IBC,ICFD,1)-1,KVCELLS(IBC,ICFD,2)-1
                IYZET(IY,IZ)=IYZET(IY,IZ)+1
 202          CONTINUE
 201        CONTINUE
          ELSE
            WARN=.TRUE.
            WRITE(STARST,'(A,I3,A)')'It should stop at cell #'
     &      ,NTCX,' in x direction'
          ENDIF
        ELSEIF(IVOLF(IBC,ICFD).EQ.3)THEN      

C South facing boundary conditions.
          FACE='South'
          IF(JVCELLS(IBC,ICFD,1).EQ.2)THEN
            DO 301 IX=IVCELLS(IBC,ICFD,1)-1,IVCELLS(IBC,ICFD,2)-1
              DO 302 IZ=KVCELLS(IBC,ICFD,1)-1,KVCELLS(IBC,ICFD,2)-1
                IXZST(IX,IZ)=IXZST(IX,IZ)+1
 302          CONTINUE
 301        CONTINUE
          ELSE
            WARN=.TRUE.
            STARST='It should start at cell # 1 in y direction'
          ENDIF
        ELSEIF(IVOLF(IBC,ICFD).EQ.4)THEN      

C North facing boundary conditions.
          FACE='North'
          IF(JVCELLS(IBC,ICFD,2).EQ.NTCY+1)THEN
            DO 401 IX=IVCELLS(IBC,ICFD,1)-1,IVCELLS(IBC,ICFD,2)-1
              DO 402 IZ=KVCELLS(IBC,ICFD,1)-1,KVCELLS(IBC,ICFD,2)-1
                IXZNT(IX,IZ)=IXZNT(IX,IZ)+1
 402          CONTINUE
 401        CONTINUE
          ELSE
            WARN=.TRUE.
            WRITE(STARST,'(A,I3,A)')'It should stop at cell #'
     &      ,NTCY,' in y direction'
          ENDIF
        ELSEIF(IVOLF(IBC,ICFD).EQ.5)THEN      

C Low facing boundary conditions.
          FACE='Low'
          IF(KVCELLS(IBC,ICFD,1).EQ.2)THEN
            DO 501 IX=IVCELLS(IBC,ICFD,1)-1,IVCELLS(IBC,ICFD,2)-1
              DO 502 IY=JVCELLS(IBC,ICFD,1)-1,JVCELLS(IBC,ICFD,2)-1
                IXYLL(IX,IY)=IXYLL(IX,IY)+1
 502          CONTINUE
 501        CONTINUE
          ELSE
            WARN=.TRUE.
            STARST='It should start at cell #1 in z direction'
          ENDIF
        ELSEIF(IVOLF(IBC,ICFD).EQ.6)THEN      

C High facing boundary conditions.
          FACE='High'
          IF(KVCELLS(IBC,ICFD,2).EQ.NTCZ+1)THEN
            DO 601 IX=IVCELLS(IBC,ICFD,1)-1,IVCELLS(IBC,ICFD,2)-1
              DO 602 IY=JVCELLS(IBC,ICFD,1)-1,JVCELLS(IBC,ICFD,2)-1
                IXYHH(IX,IY)=IXYHH(IX,IY)+1
 602          CONTINUE
 601        CONTINUE
          ELSE
            WARN=.TRUE.
            WRITE(STARST,'(A,I3,A)')'It should stop at cell #'
     &      ,NTCZ,' in z direction'
          ENDIF
        
C Blockage boundary conditions.
        ELSEIF (IVOLF(ibc,ICFD).eq.8) THEN
          do ix=IVCELLS(ibc,ICFD,1)-1,IVCELLS(ibc,ICFD,2)-1
            do iy=JVCELLS(ibc,ICFD,1)-1,JVCELLS(ibc,ICFD,2)-1
              do iz=KVCELLS(ibc,ICFD,1)-1,KVCELLS(ibc,ICFD,2)-1
                IBLKG(ix,iy,iz)=IBLKG(ix,iy,iz)+1
              enddo
            enddo
          enddo

        ENDIF

C Warn user if cell number starts/stops wrongly.
        IF(WARN)THEN
          WRITE(OUTSTR,'(4A)')'Boundary condition ',
     &      VOLNAME(IBC,ICFD)(1:LNBLNK(VOLNAME(IBC,ICFD))),' faces '
     &      ,FACE(1:LNBLNK(FACE))
          CALL USRMSG(OUTSTR,STARST,'W')
          WARN=.FALSE.
        ENDIF
 1    CONTINUE

C Debug if itrc is greater than two and NON DOS.
C      call isunix(unixok)
C      IF(ITRC.GT.2.and.unixok)THEN
C        do iz=1,ntcz
C          write(6,*)'ixzst ',(ixzst(ix,iz),ix=1,ntcx)
C        enddo

C        do iz=1,ntcz
C          write(6,*)'ixznt ',(ixznt(ix,iz),ix=1,ntcx)
C        enddo

C        do iz=1,ntcz
C          write(6,*)'iyzet ',(iyzet(iy,iz),iy=1,ntcy)
C        enddo

C        do iz=1,ntcz
C          write(6,*)'iyzwt ',(iyzwt(iy,iz),iy=1,ntcy)
C        enddo
 
C        do ix=1,ntcx
C          write(6,*)'ixyhh ',(ixyhh(ix,iy),iy=1,ntcy)
C        enddo

C Debug.
C        do ix=1,ntcx
C          write(6,*)'ixyll ',(ixyll(ix,iy),iy=1,ntcy)
C        enddo
C      ENDIF

C Do checks and issue warning messages if any
C North and South facing boundary conditions have
C a problem.
      DO 311 IX=1,NTCX
        DO 312 IZ=1,NTCZ
          IF(IXZST(IX,IZ).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # x=',IX,
     &      ' and cell # z=',IZ,' are not defined as boundary ',
     &      'conditions facing South or defined more than once.'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
          IF(IXZNT(IX,IZ).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # x=',IX,
     &      ' and cell # z=',IZ,' are not defined as boundary ',
     &      'conditions facing North or defined more than once.'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
 312    CONTINUE
 311  CONTINUE

C East and West facing boundary conditions.
      DO 411 IY=1,NTCY
        DO 412 IZ=1,NTCZ
          IF(IYZWT(IY,IZ).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # y=',IY,
     &      ' and cell # z=',IZ,' are not defined as boundary ',
     &      'conditions facing West or defined more than once.'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
          IF(IYZET(IY,IZ).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # y=',IY,
     &      ' and cell # z=',IZ,' are not defined as boundary ',
     &      'conditions facing East or defined more than once'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
 412    CONTINUE
 411  CONTINUE

C High and Low facing boundary conditions.
      DO 511 IX=1,NTCX
        DO 512 IY=1,NTCY
          IF(IXYLL(IX,IY).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # x=',IX,
     &      ' and cell # y=',IY,' are not defined as boundary ',
     &      'conditions facing Low or defined more than once.'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
          IF(IXYHH(IX,IY).NE.1)THEN
            WRITE(OUTSTR,'(A,I3,A,I3,2A)')'Boundary cell # x=',IX,
     &      ' and cell # y=',IY,' are not defined as boundary ',
     &      'conditions facing High or defined more than once.'
            CALL EDISP(IUOUT,OUTSTR)
          ENDIF
 512    CONTINUE
 511  CONTINUE

C Blockages.
      do ix=1,NTCX
        do iy=1,NTCY
          do iz=1,NTCZ
            if (IBLKG(ix,iy,iz).GT.1) then
              WRITE(OUTSTR,'(A,I3,A,I3,A,I3,A)')'Cell # x=',ix,' y=',iy,
     &          ' z=',iz,' is defined as more than one blockage.'
              CALL EDISP(IUOUT,OUTSTR)
            endif
          enddo
        enddo
      enddo

      RETURN
      END

C ******************** RESETCFD ********************
C Sets all CFD common blocks, that are independent 
C of domain and zone, to initial values.

      SUBROUTINE RESETCFD(IER)
#include "building.h"
#include "cfd.h"

C Passed parameters.
      integer ier

      COMMON/ICFNOD/ICFD,ICP
      COMMON/MFS/IMFACT
      COMMON/BUOYAN/BUOYA,BOUSSA,TBAR(MNZ)
      LOGICAL BUOYA,BOUSSA
      common/grdmax/NTCX,NTCY,NTCZ
      common/GRIDEFN/origin,xgrid,ygrid,zgrid,zegrid,ortho
      logical origin,xgrid,ygrid,zgrid,zegrid,ortho

      ICFD=0
      ICP=0
      IMFACT=0
      BUOYA=.true.
      BOUSSA=.true.
      NTCX=0
      NTCY=0
      NTCZ=0
      origin=.false.
      xgrid=.false.
      ygrid=.false.
      zgrid=.false.
      zegrid=.false.
      ortho=.false.

      RETURN
      END


C ******************** CFDLIST ********************
C Writes to ITU a formatted list of all available CFD domain 
C files, along with the zone to which they are associated.

      SUBROUTINE CFDLIST(ITU,IER)
#include "building.h"      
#include "geometry.h"
#include "cfd.h"

      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)

      character LCFD*72,outs*124,zntmp*12,dntmp*72,cpld*14
      integer ITU,IER,zref,znln,dnln,cpldln

      write(outs,'(a)')'Available CFD domain files:'
      call edisp(ITU,outs)
      do i=1,NCONF
        zref=icfdnd(i)
        zntmp=zname(zref)
        znln=lnzname(zref)
        dntmp=LCFD(i)
        dnln=LNBLNK(dntmp)
        if (IFCFD(zref).eq.0) then
          cpld=' (not coupled)'
          cpldln=14
        else
          cpld=' (coupled)'
          cpldln=10
        endif
        write(outs,'(a,i2.2,4a,1X,a)')' Zone ',zref,' ("',
     &                zntmp(1:znln),'"): ',dntmp(1:dnln),cpld(1:cpldln)
        call edisp(itu,outs)
      enddo

      RETURN
      END


