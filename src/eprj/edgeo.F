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
C  EDZONE:  Main zone form editing control.
C  EDINSUL: Edit zone insolation distribution and shading directives.
C PICKSSUR  Selects surfaces for shading & insolation or copying.
C  EDSURA:  Edit zone surface attributes in common block G5
C  Edituse: Sets the value of SUSE based on context.
C Selectuse: Gets two strings for surface use based on context.
C  EDVERT:  Edit vertex attributes in common block G1.
C  EDVLIST: Edit surface-vertex list attributes in common block G1.
C  EPKVERT: Select one or more vertices from information in common G1.
C  Zsurfprm: returns the number of edges and total length of
C           the perimiter of the polygon via global variables.
C  EZIPIN:  Reads zip format geometry data into esp-r geometry common
C           blocks.
C LINTHBRDG: Manages linear thermal bridges for zone psi and lengths.
C scan_bridges: Detects polygon edge associations related to thermal bridges.


C ******************** EDZONE ********************
C Control editing of zone attributes and allow this to be saved to a
C geometry file. ianother is returned as +1 if jump to next zone,
C -1 if jump to prior zone, otherwise zero.

      SUBROUTINE EDZONE(ITRC,ICOMP,ianother,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "model.h"
#include "site.h"
#include "prj3dv.h"
#include "geometry.h"
#include "predefined.h"
#include "esprdbfile.h"
#include "material.h"
#include "schedule.h"
#include "help.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C Passed parameters
      integer itrc   ! reporting level
      integer icomp  ! zone index
      integer ianother  ! jump indicator
      integer ier    ! returned error state zero is ok

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      common/SFIG/NSIGFIG
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Obstruction blocks via geometry.h.

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)

C prec17 common is described in geometry.h.
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C Water filled zones.
      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      real zfldK,zfldD,zfldC,zfldA,zSWAp,zSWAf
      LOGICAL znotair

C Hc coefficients
      COMMON/HCFP/IHCDT,IHCFP(MDTY),ST(MDTY,MBP),EN(MDTY,MBP),
     &            HCI(MDTY,MBP,MS),HCE(MDTY,MBP,MS),ICTL(MDTY,MBP),
     &            IHCI(MDTY,MBP,MS),IHCE(MDTY,MBP,MS),
     &            CVdata(MDTY,MBP,MS,8)
      common/HCFPHI/hcfpdescr(MDTY,MBP)
      character hcfpdescr*72

C Thermal bridges.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal  ! estimated UA for exposed parts of building
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

      LOGICAL context
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      DIMENSION IVALS(MS),ITEMS(31),ITEMSS(MS),IVALSS(MS),jvn1(MV)
      DIMENSION SALT(10),trnalt(21),IVLST(MTV)
      CHARACTER HEAD*32,SALT*32,hold*36
      CHARACTER DESCRC*25
      CHARACTER D14*14,T14*14,D12*12,T12*12,LTMP*72,GFILE*72
      CHARACTER ITEMS*33,ITEMSS*32,trnalt*33,outs*124
      character T64*64,D64*64
      CHARACTER LAPROB*72
      character SN*12,act*1,msg*48
      character SIGSTR*12,fs*1,t16a*16,t16b*16,t16c*16,msgv*42
      character TOSMLCN*32           ! remember other side mlc name
      character TOOPT*32             ! remember other side optics
      character TOUSE1*12,TOUSE2*12  ! remember other side use
      character use1*12,use2*12      ! return strings from selectuse
      character guesstype*24         ! for multi selection of surface use
      character lltmp*144
      character HFILE*72             ! initial name for hc file
      logical MODGEO,bound,attribok,zbzero,close,bndry,nameok,XST
      logical showother,updoth,firstin,unixok
      logical newgeo                 ! to use for testing if new/old geometry file.
      logical forceupgrade           ! if any other zone is version 1.1
      logical silent                 ! signal interactive dependency resolution
      logical modmlc                 ! for selecting a MLC from V1 db
      logical FOUND
      logical goforit                ! transform a node or component

      real exposed,vexposed,areatran,areawall  ! local values for QA
      real areaslproof,areafltroof,areaskylt
      real uavgtran,uavgsky,uavwall,uavfltroof,uavslproof,wallper

      integer lna,lnb,lnbm,lnbn
      integer IRT                     ! for radio button selections
      integer NITMS,INO,INOR          ! max items and current menu item
      integer INPICK                  ! to use with epkvert
      integer iissmlci                ! index for other side mlc
      integer icob                    ! for passing to clickon facility
      integer itrcl                   ! local trace on first entry to zone
      integer lnssmlc,lnsym

C Predefined entity variables.
      character name*12               ! the object name to pass to PREDEFEMBED
      character objmenu*32            ! menu for selected object
      character prec*1                ! character to pre-pend to names

      real objbb(3),DX,DY,DZ          ! Variables for preview of predefined entities.
      real A,CA,PI,SA,YR,X1,XR,Y1,XXX,YYY
      character predef*144,temp*12
      real angr
      logical closeangr

      helpinsub='edgeo'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume that no alterations have been made to geometry.
      MODGEO=.FALSE.
      zbzero=.FALSE.
      newgeo=.false.  ! assume older format geometry.
      TOSMLCN=' '; TOOPT=' '; TOUSE1=' '; TOUSE2=' '
      predef=' '
      iissmlci=0

C GFILE is the default file name for any geom file to be created,
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
      else
        WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
      endif

C On entry to zone level determine its bounds for future comparison.
      write(outs,'(2a)')'PRJ: focus on ',zname(ICOMP)
      call tstamp('>',outs)

C Initialise logic to test for model contiguity changes.
      silent= .false.
      call sumrchg(ICOMP,'i',silent)
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ESCZONE(ICOMP)
      CALL BNDOBJ(0,IER)
      CALL ERCZONE(ICOMP)

C Set for redraw if image on first entry (if refresh set to after
C each edit).
      MODIFYVIEW=.TRUE.; MODLEN=.TRUE.; MODBND=.TRUE.
      firstin=.true.

C Begin with menu of zone/surface attribute facilities.
C If ICOMP is greater than NCOMP then this signals a new zone may have
C been created. Check to see if it has.
      IF(ICOMP.GT.NCOMP.AND.ICOMP.LT.MCOM)THEN
        NCOMP=NCOMP+1
        NCCODE(ICOMP)=ICOMP
      ELSEIF(ICOMP.GT.MCOM)THEN
        CALL USRMSG('There is not sufficient space for another',
     &    'zone within this model! ','W')
        ianother=0   ! signal no jump to previous or next.
        RETURN
      ENDIF

C Detect the version of the current file.
      forceupgrade=.false.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        continue
      else

C If this zone is 1.0 and any other zone is 1.1 then any save will
C be to 1.1 via setting iupgrade equal 2.
        do 777 iz=1,ncomp
          if(iz.ne.icomp)then
            if(gversion(iz).gt.1.0) forceupgrade=.true.
          endif
  777   continue
        if(forceupgrade.and.igupgrade.eq.0) igupgrade=2

C On first entry for version 1.0 several common blocks need to be
C filled in case the model is a mix of 1.0 and 1.1 versions. First
C figure out implied shading and insolation directives. If there
C is no shading file or obstruction files assume nothing. If there
C is a shading file an no obstruction set for insolation only.
C Otherwise assume all applicable for shading and insolation.

C The logic below should also work for version 1.1 geometry files
C which include solar obstructions. << needs to be tested >>
        if(ISI(ICOMP).eq.0.and.IOBS(ICOMP).eq.0)then
          iaplic(icomp,1)=0; iaplic(icomp,2)=0   ! reset for no implied shading or insolation
          nsurfcalc(icomp)=0; nsurfinso(icomp)=0
        elseif(ISI(ICOMP).gt.0.and.IOBS(ICOMP).eq.0)then
          iaplic(icomp,1)=0; iaplic(icomp,2)=1   ! reset for insolation only
          nsurfcalc(icomp)=0; nsurfinso(icomp)=0
          DO 778 I=1,NZSUR(icomp)
            ioc=IZSTOCN(icomp,i)
            if(SOTHER(icomp,i,1)(1:5).eq.'EXTER'.and.
     &            SOTF(icomp,i)(1:4).ne.'OPAQ')then
              nsurfinso(icomp)=nsurfinso(icomp)+1
              isurfinso(icomp,nsurfinso(icomp))=I
            endif
  778     continue
        elseif(ISI(ICOMP).gt.0.and.IOBS(ICOMP).gt.0)then
          iaplic(icomp,1)=1; iaplic(icomp,2)=1   ! reset for insolation only
          nsurfcalc(icomp)=0; nsurfinso(icomp)=0
          DO 779 I=1,NZSUR(icomp)
            ioc=IZSTOCN(icomp,i)
            if(SOTHER(icomp,i,1)(1:5).eq.'EXTER'.and.
     &         SOTF(icomp,i)(1:4).ne.'OPAQ')then
              nsurfinso(icomp)=nsurfinso(icomp)+1
              isurfinso(icomp,nsurfinso(icomp))=I
            endif
            if(SOTHER(icomp,i,1)(1:5).eq.'EXTER')then
              nsurfcalc(icomp)=nsurfcalc(icomp)+1
              lstsfcalc(icomp,nsurfcalc(icomp))=I
            endif
  779     continue
        endif
      endif

    3 INO=-4
      IER=0

C Find current derived zone geometric information, check location if user
C might have edited vertices or added a surface.
      itrcl=1
      call zgupdate(itrcl,icomp,ier)

      OPQ=0.; TRN=0.; CFC=0.

C Check if base area is non-zero and whether the ibases list is
C full of zeros. If it is a partial list find the last non-zero
C entry (in case a new floor surface gets added) and remember the
C initial zbasea() as FLA.
      if(IUZBASEA(icomp).eq.1)then
        FLA=ZBASEA(ICOMP)
      elseif(IUZBASEA(icomp).eq.2)then
        FLA=ZBASEA(ICOMP)
      else
        CALL ECLOSE(ZBASEA(ICOMP),0.0,0.001,zbzero)

C If version 1.1 then set lastlist to IZBASELIST()
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          lastlist= izbaselist(icomp)
        else
          lastlist=0
          if(ibases(icomp,1).ne.0) lastlist=1
          if(ibases(icomp,2).ne.0) lastlist=2
          if(ibases(icomp,3).ne.0) lastlist=3
          if(ibases(icomp,4).ne.0) lastlist=4
          if(ibases(icomp,5).ne.0) lastlist=5
          if(ibases(icomp,6).ne.0) lastlist=6
        endif
        FLA=ZBASEA(ICOMP); ZBASEA(ICOMP)=0.0
      endif
      attribok=.true.

C Check if SVFC needs to be updated.
      call updatesvfc(icomp,modgeo)

C Loop through each surface detecting base and surface area information.
      DO 41 I=1,NSUR
        ioc=IZSTOCN(icomp,i)
        if(ioc.lt.1) goto 41  ! is model corrupt skip surf

        if(SOTHER(icomp,I,1)(1:4).eq.'UNKN')attribok=.false.
        if(SMLCN(icomp,I)(1:4).eq.'UNKN')attribok=.false.

C If the user has edited the floor area (one), or selected it via a
C sub-set of applicable surfaces (two) then don't both bother detecting
C base information.
C Otherwise, get area by adding up ibases list, adding any `FLOR` and
C non-`FURNI` surfaces.
        if(IUZBASEA(icomp).eq.1)then
          continue
        elseif(IUZBASEA(icomp).eq.2)then
          continue
        else
          FOUND=.false.
          if(lastlist.gt.MBL) lastlist=MBL
          DO J=1,lastlist
            if (I.eq.IBASES(ICOMP,J)) then
              FOUND=.true.
              EXIT
            endif
          ENDDO
          if (FOUND) then
            ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(icomp,i)
          else
            if(SVFC(ICOMP,I).eq.'FLOR')then
              if(SUSE(icomp,i,1)(1:5).eq.'FURNI'.or.
     &           SUSE(icomp,i,1)(1:6).eq.'REVEAL')then
                 continue
              else
                if(lastlist.le.MBL)then
                  lastlist=lastlist+1
                  IBASES(ICOMP,lastlist)=I
                  ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(icomp,i)
                else
                  write(outs,'(a,I2,a)')
     &   'Warning: number of floor surfaces detected exceeds maximum (',
     &              MBL,')'
                  call edisp(iuout,outs)
                endif
              endif
            endif
          endif
        endif

C Detect surface area information.
        if(SOTF(icomp,I)(1:4).NE.'OPAQ'.AND.
     &    SOTF(icomp,I)(1:3).NE.'CFC')then
          TRN=TRN+SNA(icomp,i)
        elseif(SOTF(icomp,I)(1:4).EQ.'CFC ')then
          CFC=CFC+SNA(icomp,i)
        elseif(SOTF(icomp,I)(1:4).EQ.'CFC2')then
          CFC=CFC+SNA(icomp,I)
        else
          OPQ=OPQ+SNA(icomp,I)
        endif
   41 CONTINUE

C If previously calculated base area, compare with current area calculation
C and inform user if it has changed.
      if(IUZBASEA(icomp).eq.0)then
        CALL ECLOSE(ZBASEA(ICOMP),FLA,0.01,close)
        if(.NOT.close)then

C Debug.
          call SIGFIG(ZBASEA(ICOMP),NSIGFIG,RNO,SIGSTR,LSTR)
          write(outs,'(4a)')
     &      'Base/floor area is now',SIGSTR(1:LSTR),'m^2. ',
     &      'Menu option c to define manually.'
          call edisp(iuout,outs)
        endif
      endif
      call ckvert(0,icomp,bound,iub,inv,'-',ier)  ! needs G1 common

C Loop through constructions and see if partitions have a matching
C construction on the other side. Logic is the same as in EDSURA.
      DO 40 is=1,NSUR
        ioc=IZSTOCN(icomp,is)
        if(ioc.lt.1)then
          showother=.false.
          icoth=0  ! not a partition
          goto 40
        endif
        if(ICT(ioc).eq.3)then
          showother=.true.
          icoth=IZSTOCN(IC2(ioc),IE2(ioc))
        else
          showother=.false.
          icoth=0  ! not a partition
        endif

C Report on any duplicate surface names.
        write(SN,'(a12)')SNAME(icomp,is)
        call snamdup(SN,icomp,is,nameok)
        if(nameok)then
          continue
        else
          write(outs,'(3a)') 'Surface ',SN(1:lnblnk(SN)),
     &      ' has a duplicate name. Names must be unique!'
          call edisp(iuout,outs)
        endif

C Report on possible construction issues between zones.
C Use smlcindex to get MLC index for this surface.
        if(showother.and.icoth.ne.0.and.
     &     smlcindex(icomp,is).ne.0)then
          ii=smlcindex(icomp,is)
          updoth=.false.
          lnssmlc=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
          if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
            if(SMLCN(IC2(ioc),IE2(ioc))(1:lnssmlc).eq.
     &         mlcname(ii)(1:lnmlcname(ii)))then
              TOSMLCN=mlcname(ii)
              write(TOOPT,'(a)') mlcoptical(ii)
            elseif(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
              TOSMLCN=mlcname(ii)
              write(TOOPT,'(a)') mlcoptical(ii)
              updoth=.true.
            else
              TOSMLCN=mlcname(ii)
              write(TOOPT,'(a)') mlcoptical(ii)
              updoth=.true.
            endif
            iissmlci=ii
          elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric but is linked to
C the key phrase NONSYMMETRIC rather than a specific MLC name
C then it should not be used for a partition - inform the user.
            lnsmlcn=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
            write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &        ' has a nonsymmetric construction ',
     &        SMLCN(IC2(ioc),IE2(ioc))(1:lnblnk(SMLCN(IC2(ioc),
     &        IE2(ioc)))),'.'
            call edisp(iuout,outs)
            write(outs,'(5a)') 'It faces ',
     &        SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &        SMLCN(IC2(ioc),IE2(ioc))(1:lnsmlcn),
     &        ' (which may not match).'
            call edisp(iuout,outs)
            updoth=.false.
            iissmlci=matsymindex(ii)   ! returned value
          else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
            lnsym=lnblnk(mlcsymetric(ii))
            lnsmlcn=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
            if(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
              TOSMLCN=mlcsymetric(ii)
              write(TOOPT,'(a)') mlcoptical(ii)
              updoth=.true.
            elseif(SMLCN(IC2(ioc),IE2(ioc))(1:lnsmlcn).eq.
     &             mlcsymetric(ii)(1:lnsym))then
              updoth=.false.
            else
              TOSMLCN=mlcsymetric(ii)
              write(TOOPT,'(a)') mlcoptical(ii)
              updoth=.true.
            endif
            iissmlci=matsymindex(ii)   ! returned value
          endif

C A difference was discovered, report this.
          if(updoth)then
            CALL ESCZONE(ICOMP)
            lnicoth=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
            write(outs,'(6a)')
     &        'Updating `other side` composition of ',
     &         SN(1:lnblnk(SN)),' from ',SMLCN(IC2(ioc),
     &         IE2(ioc))(1:lnicoth),' > ',TOSMLCN
            call edisp(iuout,outs)
            call georead(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,iuout,IER)

C Assign new MLC name to the icoth surface as well as SOTF with the
C optical properties returned from material.h commons.
            if(iissmlci.ne.0)then
              SMLCN(IC2(ioc),IC2(ioc))=TOSMLCN
              smlcindex(IC2(ioc),IC2(ioc))=iissmlci ! update
            endif

            write(SOTF(IC2(ioc),IE2(ioc)),'(a)') TOOPT
            call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(IC2(ioc)) =1.1
              newgeo = .true.
            endif

C Logic to decide whether to upgrade other zone geometry format.
            if(.NOT.newgeo)then
              gversion(IC2(ioc)) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &        iuout,3,IER)
            IF(IER.NE.0)CALL USRMSG(
     &        'There was a problem updating the other surface ',
     &        'attribute (i.e. the other geometry file)...','W')

C Read current zone back in and then update the configuration file.
            CALL ERCZONE(ICOMP)
            updoth=.false.
          endif
          call usrmsg(' ',' ','-')
        endif
 40   continue

C Returning to this menu re-establish proportional font.
      if(IMFS.ge.0.and.IMFS.le.3)then
        if(IMFS.eq.0) IMFS=4
        if(IMFS.eq.1) IMFS=5
        if(IMFS.eq.2) IMFS=6
        if(IMFS.eq.3) IMFS=7
        call userfonts(IFS,ITFS,IMFS)
      endif

      WRITE(ITEMS(1),'(A,A12)')  'a name: ',zname(ICOMP)
      WRITE(ITEMS(2),'(A,A24)')  'b desc: ',zdesc(ICOMP)(1:24)
      ITEMC=2
      ITEMC=ITEMC+1
      if(.not.attribok)then
        ITEMS(ITEMC)=                   '   attribution incomplete!'
        ITEMC=ITEMC+1
      endif
      if(.not.bound)then
        iprb=MAX0(iub,inv)
        WRITE(ITEMS(ITEMC),'(a,I3,A)')  'u ',iprb,' PROBLEM EDGES!'
        ITEMC=ITEMC+1
      endif

      WRITE(ITEMS(ITEMC),'(A,3F6.1)')'   origin @ ',X(1),Y(1),Z(1)
      call SIGFIG(VOL(icomp),NSIGFIG,RNO,SIGSTR,LSTR)
      WRITE(ITEMS(ITEMC+1),'(3a)')'   volume:      ',SIGSTR(1:LSTR),
     &  ' m^3'
      call SIGFIG(ZBASEA(ICOMP),NSIGFIG,RNO,SIGSTR,LSTR)
      if(IUZBASEA(icomp).eq.0)then
        WRITE(ITEMS(ITEMC+2),'(3a)')'c  base/floor area: ',
     &      SIGSTR(1:LSTR),' m^2'
      elseif(IUZBASEA(icomp).eq.1)then
        WRITE(ITEMS(ITEMC+2),'(3a)')'c  edited base area:',
     &      SIGSTR(1:LSTR),' m^2'
      elseif(IUZBASEA(icomp).eq.2)then
        WRITE(ITEMS(ITEMC+2),'(3a)')'c  base area via list:',
     &      SIGSTR(1:LSTR),' m^2'
      endif
      call SIGFIG(OPQ,NSIGFIG,RNO,SIGSTR,LSTR)
      WRITE(ITEMS(ITEMC+3),'(3a)')'   opaque constr.:  ',
     &         SIGSTR(1:LSTR),' m^2'
      call SIGFIG(TRN,NSIGFIG,RNO,SIGSTR,LSTR)
      WRITE(ITEMS(ITEMC+4),'(3a)')'   transp. constr.: ',
     &         SIGSTR(1:LSTR),' m^2'
      if(CFC.gt.0.01)then  ! if non-trival CFC area report it instead of TMC
        call SIGFIG(CFC,NSIGFIG,RNO,SIGSTR,LSTR)
        WRITE(ITEMS(ITEMC+4),'(3a)')'   cfc constr.: ',
     &         SIGSTR(1:LSTR),' m^2'
      endif

C Report on associated with mass flow node.
      ITEMS(ITEMC+5)='  _____________________________'
      if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then ! Is there a flow network?
        if(IAIRN.eq.3)then                     ! Is it 3D?
          loop=ICAAS(ICOMP)
          if(NDTYP(loop).eq.0.or.NDTYP(loop).eq.1)then ! if internal
            write(ITEMS(ITEMC+5),'(2a)') '  & flow node ',
     &        NDNAM(loop)(1:lnblnk(NDNAM(loop)))

C Check if NODASSOC(loop,1) is still '-'. If so set it to the
C zone name and update the flow network file(s).
            if(NODASSOC(loop,1)(1:1).eq.'-')then
              NODASSOC(loop,1)=ZNAME(ICOMP); NODASSOC(loop,2)='-'
              HNOD(loop,1)=ZCOG(ICOMP,1); HNOD(loop,2)=ZCOG(ICOMP,2)
              HNOD(loop,3)=ZCOG(ICOMP,3)
              SUPNOD(loop,2)=VOL(ICOMP)
              call updatebothflownetworks(ier)
            endif
          endif
        endif
      endif

C does one of the NODASSOC(NNOD,1) match the zone name?
C Is NDTYP(NNOD) internal known (1) or internal unknown pressure (0)

      WRITE(ITEMS(ITEMC+6),'(A,i3,a)')
     &                  'd vertex coordinates     (',NTV,')'  ! ? NZTV
      WRITE(ITEMS(ITEMC+7),'(A,i3,a)')
     &                  'e surface list & edges   (',NSUR,')'
      ITEMS(ITEMC+8) ='f surface attributes           '
      ITEMS(ITEMC+9) ='  _____________________________'
      if(newgeo)then
        ITEMS(ITEMC+10)='g solar dist. & calc directives'
      else
        ITEMS(ITEMC+10)='g solar distribution           '
      endif
      ITEMS(ITEMC+11)='h solar obstruction            '
      ITEMS(ITEMC+12)='i rotation & transforms        '
      if(newgeo)then
        ITEMS(ITEMC+13)='j linear thermal bridges     '
      else
        ITEMS(ITEMC+13)='j thermal bridges (NA)         '
      endif

C Interface to BASESIMP and visual entity definitions for this zone.
      ITEMS(ITEMC+14)='k BASESIMP defiitions          '
      ITEMS(ITEMC+15)='l visual entities (Radiance)   '
      ITEMS(ITEMC+16)='m predefined entities          '
      if(znotair(ICOMP))then
        ITEMS(ITEMC+17)='n >> zone is water filled      '
      else
        ITEMS(ITEMC+17)='n >> zone is air filled        '
      endif
      ITEMS(ITEMC+18)='  _____________________________'
      ITEMS(ITEMC+19)='* list zone & surface details  '
      ITEMS(ITEMC+20)='! save                         '
      IF(ITRC.EQ.0)THEN
        ITEMS(ITEMC+21)='t reporting & menus >> silent  '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(ITEMC+21)='t reporting & menus >> summary '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(ITEMC+21)='t reporting & menus >> detailed'
      ENDIF
      if(icomp.gt.1.and.icomp.lt.NCOMP)then
        ITEMS(ITEMC+22)='< jump to previous zone      '
        ITEMS(ITEMC+23)='> jump to next zone          '
      elseif(icomp.eq.1)then
        ITEMS(ITEMC+22)='                             '
        ITEMS(ITEMC+23)='> jump to next zone          '
      elseif(icomp.eq.NCOMP)then
        ITEMS(ITEMC+22)='< jump to previous zone      '
        ITEMS(ITEMC+23)='                             '
      endif
      ITEMS(ITEMC+24)='? help                         '
      ITEMS(ITEMC+25)='- exit menu                    '

      WRITE(HEAD,'(A,I2,A)')'Zone ',ICOMP,' Geometry'

C Do a bound check on the current zone and update display.
C Set all surfaces to standard line width.

C Debug.
C      write(6,*)nzg,nznog(1),nznog(2),nznog(3)

      CALL INLNST(1)
      itsnm=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL redraw(IER)

C Place any surface attribute mass flow symbol on the wireframe if in graphic mode.
      if(MMOD.eq.8)then
        do loop=1,NZSUR(ICOMP)
          icon=IZSTOCN(icomp,loop)
          if(icon.gt.0)then
            call draw_surf_flow_symbol(icon,'-')
          endif
        enddo
      endif

C Test edge adjacencies and subsurfaces if it is the first time
C in to the zone or if the geometry has been modified.
      if(firstin.or.MODGEO)then
        act = 'c'
        call suredgeadj(0,act,icomp,ier) ! determine child surfaces
        firstin=.false.
      endif

C Help text for this menu.
      helptopic='geometry_main_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu for zone geometry.
      NITMS=ITEMC+25
      CALL EMENU(HEAD,ITEMS,NITMS,INO)

      IF(INO.EQ.ITEMC+25)THEN
        if(MODGEO)then
          CALL EASKOK(' ',
     &      'Save changes to zone composition and surface attributes?',
     &      OK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
          endif

C If there is an associated flow node then update its volume
C and position and update the flow network file(s).
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            nindex=ICAAS(ICOMP)
            SUPNOD(nindex,2)=VOL(ICOMP)
            HNOD(nindex,1)=ZCOG(ICOMP,1); HNOD(nindex,2)=ZCOG(ICOMP,2)
            HNOD(nindex,3)=ZCOG(ICOMP,3)
            if(znotair(icomp))then
              SUPNOD(nindex,1)=2  ! mark flow node as water
              NDFLD(nindex)=2
            else
              SUPNOD(nindex,1)=1  ! mark flow node as air
              NDFLD(nindex)=1
            endif
            call updatebothflownetworks(ier)
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        ianother=0   ! signal no jump to previous or next.
        RETURN

      ELSEIF(INO.EQ.1)THEN

C Zone name, make sure it has no illegal characters.
        write(msg,'(2a)') 'The current zone name is ',zname(ICOMP)
        call edisp(iuout,' ')
        call edisp(iuout,msg)
        T14=' '
        D14='new_zone'
        CALL EASKS(T14,'Descriptive name for zone?','(<12 characters)',
     &               14,D14,'zone name',IER,nbhelp)
        if(T14(1:2).NE.'  ')then
          write(T12,'(a)') T14(1:12)
          call st2name(T12,zname(ICOMP))
          lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update this string length

          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
          else
            WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
          endif
          call tstamp('>','PRJ: edited zone name')
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          LTMP=LGEOM(ICOMP)
          call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
          MODIFYVIEW=.TRUE.; MODGEO=.FALSE.

C If applicable, update the node name to match.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            loop=ICAAS(ICOMP)
            NDNAM(loop)=zname(ICOMP)
            if(NDTYP(loop).eq.0.or.NDTYP(loop).eq.1)then ! if internal
              NODASSOC(loop,1)=ZNAME(ICOMP); NODASSOC(loop,2)='-'
            endif
            call updatebothflownetworks(ier)
          endif
        endif

      ELSEIF(INO.EQ.2)THEN

C Zone description
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        T64=zdesc(icomp)
        D64='<user has provided no description>'
  64    CALL EASKS(T64,'Zone description?','(<64 characters)',
     &           64,D64,'zone descr',IER,nbhelp)
        if(T64(1:2).eq.'  ')goto 64
        zdesc(icomp)=T64
        lnzdesc(icomp)=lnblnk(T64)  ! update the length of this string.
        call tstamp('>','PRJ: edited zone description')
        LTMP=LGEOM(ICOMP)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
        MODIFYVIEW=.TRUE.; MODGEO=.FALSE.
      ELSEIF(INO.EQ.3)then
        if(.NOT.bound)then

C Offer attempt to correct unbound edges.
          CALL EASKOK(' ','Correct unbounded edges?',
     &      OK,nbhelp)
          if(OK)then
            call ckvert(1,icomp,bound,iub,inv,'r',ier)
            MODGEO=.TRUE.
            goto 3
          endif
        endif

      ELSEIF(INO.EQ.ITEMC+2)THEN

C Associate surfaces with base area, especially if there are more
C than one surface with orientation 'FLOR'.
C Present list of surfaces and allow user to select. Number
C of surfaces associated with base is derived from non-zero values
C in IBASES or IZBASELIST. If all slots are 0 then floor area is user defined.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          INPICK=MBL
        else
          INPICK=6
        endif
        ij=0
        DO 20 I=1,NSUR
          ITEMSS(I)=' '
          DO J=1,IZBASELIST(ICOMP)
            FOUND=.false.
            if (I.eq.IBASES(ICOMP,J)) then
              FOUND=.true.
              EXIT
            endif
          enddo
          if (FOUND) then
            WRITE(ITEMSS(I),'(A,1x,A,1x,A,A)')SNAME(ICOMP,I),
     &        SMLCN(ICOMP,I)(1:12),SVFC(ICOMP,I)(1:4),'*'
          else
            WRITE(ITEMSS(I),'(A,1x,A,1x,A)')SNAME(ICOMP,I),
     &        SMLCN(ICOMP,I)(1:12),SVFC(ICOMP,I)
          endif
   20   CONTINUE

C Present list of available surfaces with * adjacent to those which
C are already considered to be floor surfaces. Clear the list ibases
C and then either do manual editing or update ibases with surfaces
C the user selected.
        if(IUZBASEA(icomp).eq.0)then
          write(outs,'(a,f9.3,a)')
     &     'Surfaces automatically associated with base (current area ',
     &      ZBASEA(ICOMP),' m^2)'
        elseif(IUZBASEA(icomp).eq.1)then
          write(outs,'(a,f9.3,a)')
     &   'No surfaces associated with base (current user defined area ',
     &      ZBASEA(ICOMP),' m^2)'
        elseif(IUZBASEA(icomp).eq.2)then
          write(outs,'(a,f9.3,a)')
     &      'Surfaces associated with base (current area ',
     &      ZBASEA(ICOMP),' m^2)'
        endif
        if (INPICK.gt.NSUR) INPICK=NSUR
        CALL EPICKS(INPICK,IVALS,outs,
     &    'Select none for manual area edit.',32,NSUR,ITEMSS,
     &    'Surface Name/Construction/Orient.',IER,nbhelp)
        do 221 i=1,MBL
          IBASES(ICOMP,i)=0
 221    continue
        if(inpick.eq.0)then
          CALL EASKR(ZBASEA(ICOMP),' ',
     &       'Confirm area of zone base.',
     &       0.1,'F',99999.,'F',FLA,'base area',IER,nbhelp)
          IUZBASEA(icomp)=1; IZBASELIST(icomp)=0
          MODGEO=.TRUE.
          goto 3
        else
          ZBASEA(ICOMP)=0.0
          IZBASELIST(icomp)=inpick
          do 211 i=1,inpick
            iss = ivals(i)
            IBASES(ICOMP,i)=iss
            ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(icomp,iss)
 211      continue
          IUZBASEA(icomp)=2
          write(outs,'(a,f9.3,a)')
     &      'New base/floor area is ',ZBASEA(ICOMP),'m^2'
          call edisp(iuout,outs)
          MODGEO=.TRUE.
          goto 3
        endif

      ELSEIF(INO.EQ.ITEMC+6)THEN

C Vertex editing. After returning from editing if there have
C been changes check if user wants to save the changes and
C if so update the geometry file. Also update the volume of
C any flow network node associated with this zone.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call tstamp('>','PRJ: enter zone vertices')
        CALL EDVERT(iuout,ICOMP,MODGEO,IER)
        if(MODGEO)then
          CALL EASKOK(' ','Save vertex changes?',OK,nbhelp)
          LTMP=LGEOM(ICOMP)
          if(OK)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
            if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
              nindex=ICAAS(ICOMP)
              SUPNOD(nindex,2)=VOL(ICOMP)
              HNOD(nindex,1)=ZCOG(ICOMP,1); HNOD(nindex,2)=ZCOG(ICOMP,2)
              HNOD(nindex,3)=ZCOG(ICOMP,3)

C Not yet done: loop through surfaces and for those associated with components
C in the flow network and which have appropriate use types update their
C positions.

              call updatebothflownetworks(ier)
            endif
            MODGEO=.false.
          endif
        endif
        MODIFYVIEW=.TRUE.   ! set so zone is re-drawn

      ELSEIF(INO.EQ.ITEMC+7)THEN

C Vertex-surface association editing.
        call tstamp('>','PRJ: enter zone-surface topology')
        CALL EDVLIST(ITRC,iuout,ICOMP,IER)
        MODGEO=.TRUE.

      ELSEIF(INO.EQ.ITEMC+8)THEN

C Surface attributes.  Display a selection of surfaces available in the
C zone. Trap option of multiple surface attribution.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
   42   IS=0
        if(ITRC.gt.1)then
          CALL EASKSUR(ICOMP,IS,'V','Select surface(s) to attribute.',
     &      ' ',IER)
        else
          CALL EASKSUR(ICOMP,IS,'M','Select surface(s) to attribute.',
     &      ' ',IER)
        endif
        if(IS.lt.0)then

C Remind user of current surface boundary attributes.

C << revise -> 'impose boundary condition' & 'discover boundary condition'
C <<        helptopic='surface_global_attrib'
C <<        call gethelptext(helpinsub,helptopic,nbhelp)

          call tstamp('>','PRJ: enter global surface attribution')
          context=.true.
          if(ITRC.gt.1)CALL SURINFO(ICOMP,iuout,context)
          IRT=1
          CALL EASKATOG(' ','Surface attribution options:',
     &      'name','composition','impose boundary condition',
     &      'discover boundary condition','usage',
     &      'cancel',' ',IRT,nbhelp)

          if(IRT.eq.2)then

C Select a MLC to apply to the list of surfaces.
            if(mlcver.eq.0)then
              CALL EPKMLC(ISEL,'Select one of the constructions',
     &          'or exit.',IER)
            else
              call edisp(iuout,'Select one of the constructions')
              CALL EDMLDB2(modmlc,'-',ISEL,IER)
            endif
          elseif(IRT.eq.3)then
            SALT(1)  ='exterior                   '
            SALT(2)  ='similar to current         '
            SALT(3)  ='prescribed static          '
            SALT(4)  ='surface in other zone      '
            SALT(5)  ='ground (monthly profile)   '
            if(NGRDP.gt.0)then
              SALT(6)='ground (user defined profile)   '
            else
              SALT(6)='Ground (no user defined profile)'
            endif
            SALT(7)  ='adiabatic                   '
            SALT(8)  ='BASESIMP foundation calculation'
            SALT(9)  ='CEN 13791 partition         '
            SALT(10) ='unknown at this time       '

            helptopic='geometry_boundary_menu'
            call gethelptext(helpinsub,helptopic,nbhelp)
            IX=1
            CALL EPICKS(IX,IVALSS,' ','Surface boundary condition:',
     &           32,10,SALT,'surface boundary options',IER,nbhelp)
            IOS=IVALSS(1)
            if(IOS.EQ.2)then
              VALT=0.0; VALW=0.0
              CALL EASKR(VALT,' ','Temperature offset?',
     &          -99.,'F',700.,'F',0.0,'offset temp',IER,nbhelp)
              CALL EASKR(VALW,' ','Radiation offset?',
     &          0.0,'W',99999.,'W',0.0,'offset rad',IER,nbhelp)
            elseif(IOS.EQ.3)then
              CALL EASKR(VALT,' ','Constant temperature?',
     &         -99.,'F',700.,'F',20.,'adjacent temperature',IER,nbhelp)
              CALL EASKR(VALW,' ','Constant radiation?',
     &         0.0,'W',99999.,'W',0.0,'adjacent radiation source',
     &         IER,nbhelp)
            elseif(IOS.EQ.5)then

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
              CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
              CALL EDISP(iuout,'id Jan, Feb, Mar, Apr, May, Jun...')
              do 21 igrdp=1,mgrdp
                CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
                if(.NOT.close)then
                 WRITE(OUTS,'(I2,2a)')igrdp,' ',grdtmpname(IGRDP)
                 call edisp(iuout,outs)
                 WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
                 call edisp(iuout,outs)
                endif
 21           continue
              CALL EASKI(IIC2,' ','Standard ground profile number?',
     &          1,'F',mgrdp,'F',1,'ground profile',IERI,nbhelp)
              if(ieri.eq.-3) goto 3
            elseif(IOS.EQ.6)then

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
              if(NGRDP.gt.0)then
                CALL EDISP(iuout,' ')
                do 22 igrdp=1,NGRDP
                CALL EDISP(iuout,'User defined monthly ground profile:')
                 WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
                 call edisp(iuout,outs)
 22             continue
                CALL EASKI(IIE2,' ',
     &            'User defined ground profile number?',
     &            0,'F',9,'F',1,'user defined profile',IERI,nbhelp)
                if(ieri.eq.-3) goto 3
              else
                call usrmsg(
     &          'No user defined ground profiles have been found.',
     &          'Go to `Model Context` menu to define.','W')
                goto 3
              endif
            endif
          elseif(IRT.eq.4)then
           call edisp(iuout,' ')
           call edisp(iuout,
     &       'Select surfaces with an UNKNOWN boundary condition.')

          elseif(IRT.eq.5)then

C Select surface USE for one or more surface, later update the
C common blocks.
            guesstype='any_type'; use1=' '; use2=' '
            call selectuse(guesstype,use1,use2)

          elseif(IRT.eq.6)then
            goto 3
          endif

C Present list of surfaces and allow user to select up to all of them.
          INPICK=NSUR
          DO 10 I=1,NSUR
            lnl=lnblnk(SMLCN(icomp,i))
            lnsn=lnblnk(SNAME(icomp,i))
            if(lnsn.lt.10) lnsn=10
            if((lnl+lnsn+2).ge.32) lnl=32-(lnsn+2)  ! to prevent overflow
            ITEMSS(I)=' '
            if(IRT.eq.1)then
              WRITE(ITEMSS(I),'(A)')SNAME(icomp,i)(1:lnsn)
            elseif(IRT.eq.2)then
              WRITE(ITEMSS(I),'(A,2x,A)')SNAME(icomp,i)(1:lnsn),
     &          SMLCN(icomp,i)(1:15)
            elseif(IRT.eq.3.or.IRT.eq.4)then
              WRITE(ITEMSS(I),'(A,2x,A)')SNAME(icomp,i)(1:lnsn),
     &          SOTHER(icomp,i,1)(1:15)
            elseif(IRT.eq.5)then
              WRITE(ITEMSS(I),'(7A)')SNAME(icomp,i),' ',
     &          SOTHER(icomp,i,1)(1:6),' ',SUSE(icomp,i,1)(1:5),' ',
     &          SUSE(icomp,i,2)(1:5)
            endif
   10     CONTINUE
          if(IRT.eq.1)then
            CALL EPICKS(INPICK,IVALS,' ',' ',15,NSUR,ITEMSS,
     &          'Surface name',IER,nbhelp)
          elseif(IRT.eq.2)then
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name   composition',IER,nbhelp)
          elseif(IRT.eq.3.or.IRT.eq.4)then
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name    facing',IER,nbhelp)
          elseif(IRT.eq.5)then
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name    use',IER,nbhelp)
          endif

C For each of the selected surfaces get its connection, decide if
C it is a partition and then process based on value of IRT.
          do 11 i=1,inpick
            iss = ivals(i)
            ioc=IZSTOCN(ICOMP,iss)
            if(ICT(ioc).eq.3)then
              showother=.true.
              icoth=IZSTOCN(IC2(ioc),IE2(ioc))
            else
               showother=.false.
            endif
            if(IRT.eq.1)then  ! (re)name all selected surfaces.

C Update image befort each edit.
C Call INLNST(1) to reset all LINSTY to 1 (i.e. thin lines) then set
C LINSTY of selected surface to 2 (i.e. thick lines) and redraw.
              CALL INLNST(1)
              CALL SURADJ(ICOMP,ISS,IE,TMP,IZC,ISC,IC,DESCRC)
              LINSTY(IC)=2
              nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
              MODIFYVIEW=.TRUE.; MODGEO=.TRUE.
              CALL redraw(IER)

C Also check whether surface is associated with a boundary flow node.
              inode=0; icmp=0
              call doesflowrefsurface(ICOMP,ISS,inode,icmp)

C Edit surface name via a slightly wider editing box than the name. Take the string
C (T14) and ensure that there are no spaces or strange characters.
              T14='  '
              lnsmlcn=lnblnk(SMLCN(ICOMP,iss))
              write(D14,'(2a)')SNAME(ICOMP,iss),'  '
              write (outs,'(7a)')'Surface name (default=',D14,
     &          ') composed of ',smlcn(ICOMP,iss)(1:lnsmlcn),' facing ',
     &          SOTHER(ICOMP,iss,1)(1:12),' highlighted surface'
 52           CALL EASKS(T14,outs,'(<=12 chars, no spaces)',
     &                   14,D14,'surface name',IER,nbhelp)
              write(T12,'(a)') T14(1:12)
              call st2name(T12,D12)

              call snamdup(D12,icomp,ISS,nameok)
              if(nameok)then

C Update commons and update the geometry file.
                SNAME(ICOMP,ISS)=D12
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(.NOT.newgeo)then
                  gversion(icomp) =1.1
                  newgeo = .true.
                endif
                call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C If surface associated with a boundary flow node update NODASSOC. If
C surface is associated with a component also update NODASSOC and network file.
                if(inode.gt.0)then
                  NODASSOC(inode,1)=ZNAME(ICOMP)
                  NODASSOC(inode,2)=SNAME(ICOMP,ISS)
                endif
                if(icmp.gt.0)then
                  CMPASSOC(icmp,1)=ZNAME(ICOMP)
                  CMPASSOC(icmp,2)=SNAME(ICOMP,ISS)
                endif
                if(inode.gt.0.or.icmp.gt.0)then
                  call updatebothflownetworks(ier)
                endif
              else
                call edisp(iuout,
     &                  'Surface name is a duplicate of an existing')
                call edisp(iuout,
     &                  'surface. Please supply an alternative.')
                goto 52
              endif

C Reset all line widths to normal and redraw (if last surface in list).
              if (i.eq.inpick) then
                CALL INLNST(1)
                MODIFYVIEW=.TRUE.; MODGEO=.TRUE.
                CALL redraw(IER)
              endif
            elseif(IRT.eq.2.and.ISEL.EQ.0)then

C User selected the UNKNOWN MLC, reset variables.
              WRITE(SMLCN(icomp,ISS),'(A)') 'UNKNOWN'
              SOTF(ICOMP,ISS)='OPAQUE'
              smlcindex(icomp,iss)=0
            elseif(IRT.eq.2.and.ISEL.GT.0)then

C Associate mlc with each selected surface.
              WRITE(SMLCN(icomp,ISS),'(A)') mlcname(ISEL)
              smlcindex(icomp,iss)=ISEL       ! update array

C Find the optical name.
              IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
                SOTF(ICOMP,ISS)='OPAQUE'
              ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
                SOTF(ICOMP,ISS)='CFC '
              ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC2')then
                SOTF(ICOMP,ISS)='CFC2'
              ELSE
                WRITE(SOTF(ICOMP,ISS),'(A)') mlcoptical(ISEL)
              ENDIF

C Write the current zone geometry file before proceeding.
              if(.NOT.newgeo)then
                gversion(icomp) =1.1
                newgeo = .true.
              endif
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

              call warnmod(ICOMP,'sat')

C If showother remember the other side SUSE so it can be re-established.
              if(showother)then
                TOUSE1=SUSE(IC2(ioc),IE2(ioc),1)  ! record prior to edit
                TOUSE2=SUSE(IC2(ioc),IE2(ioc),2)
              endif

C If this is a partition find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.

C This logic should also included in subroutine serchrpl. So the
C global search and replace acts the same way.
              write(SN,'(a12)')SNAME(ICOMP,ISS)
              if(showother.and.icoth.ne.0.and.
     &           smlcindex(ICOMP,iss).ne.0)then
                ii=smlcindex(icomp,iss)
                updoth=.false.
                lnssmlc=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                  if(SMLCN(IC2(ioc),IE2(ioc))(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcname(ii)))then
                    TOSMLCN=mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  elseif(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
                    TOSMLCN=mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                    updoth=.true.
                  else
                    TOSMLCN=mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                    updoth=.true.
                  endif
                  iissmlci=ii
                elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                  lnssmlc=lnblnk(SMLCN(ICOMP,iss))
                  write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &              ' has a nonsymmetric construction ',
     &              SMLCN(ICOMP,iss)(1:lnssmlc),'.'
                  call edisp(iuout,outs)
                  lnssmlc=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                  write(outs,'(5a)') 'It faces ',
     &              SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &              SMLCN(IC2(ioc),IE2(ioc))(1:lnssmlc),
     &              ' (which may not match).'
                  call edisp(iuout,outs)
                  call edisp(iuout,
     &              'Please check that one construction has inverted')
                  call edisp(iuout,
     &              'layers or revise the construction database.')
                  updoth=.false.
                  iissmlci=matsymindex(ii)  ! use returned value
                else
                  lnssmlc=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                  if(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
                    TOSMLCN=mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                    updoth=.true.
                  elseif(SMLCN(IC2(ioc),IE2(ioc))(1:lnssmlc).eq.
     &                   mlcsymetric(ii)(1:lnssmlc))then
                    TOSMLCN=mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  else
                    TOSMLCN=mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                    updoth=.true.
                  endif
                  iissmlci=matsymindex(ii)  ! use returned value
                endif
                if(updoth)then
                  CALL ESCZONE(ICOMP)
                  call usrmsg(
     &              ' Updating other side construction (remember to',
     &              ' update the other side construction file).','P')
                  call georead(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &              0,iuout,IER)

C Assign new MLC name to the icoth surface and reset SUSE which might
C have been lost in the switch between zones.
                  if(iissmlci.ne.0)then
                    SMLCN(IC2(ioc),IE2(ioc))=TOSMLCN
                    smlcindex(IC2(ioc),IE2(ioc))=iissmlci ! update
                  endif
                  write(SOTF(IC2(ioc),IE2(ioc)),'(a)') TOOPT
                  SUSE(IC2(ioc),IE2(ioc),1)=TOUSE1
                  SUSE(IC2(ioc),IE2(ioc),2)=TOUSE2
                  call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                  if(.NOT.newgeo)then
                    gversion(IC2(ioc)) =1.1
                    newgeo = .true.
                  endif
                  call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &              iuout,3,IER)
                  IF(IER.NE.0)CALL USRMSG(
     &               'Problem updating other surface attribute... ',
     &               '(could not write the other geometry file)','W')

C Read current zone back in and then update the configuration file.
                  CALL ERCZONE(ICOMP)
                  call usrmsg(
     &              ' Updating other side construction...done.',
     &              ' ','-')
                  updoth=.false.
                endif
              endif
            elseif(IRT.eq.3.and.IX.gt.0)then

C Determine the match in the system topology and update it.
              CALL SURADJ(ICOMP,ISS,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ioc.ne.ic)then
                write(outs,*) 'edgeo mismatch ic ioc ',ic,ioc
                call edisp(iuout,outs)
              endif
              IC1(IC)=ICOMP
              IE1(IC)=ISS
              IF(IOS.EQ.1)THEN
                SOTHER(ICOMP,ISS,1)='EXTERIOR'
                SOTHER(ICOMP,ISS,2)='0'; SOTHER(ICOMP,ISS,3)='0'
                ICT(IC)=0; IC2(IC)=0; IE2(IC)=0
              ELSEIF(IOS.EQ.2)THEN
                SOTHER(ICOMP,ISS,1)='SIMILAR'
                if(INT(VALT).ge.0)then
                  write(SOTHER(ICOMP,ISS,2),'(i3.3)')INT(VALT)
                else
                  write(SOTHER(ICOMP,ISS,2),'(i3)')INT(VALT)
                endif
                if(INT(VALW).ge.0)then
                  write(SOTHER(ICOMP,ISS,3),'(i3.3)')INT(VALW)
                else
                  write(SOTHER(ICOMP,ISS,3),'(i3)')INT(VALW)
                endif
                ICT(IC)=1; IC2(IC)=INT(VALT); IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.3)THEN
                SOTHER(ICOMP,ISS,1)='CONSTANT'
                write(SOTHER(ICOMP,ISS,2),'(i3.3)')INT(VALT)
                write(SOTHER(ICOMP,ISS,3),'(i3.3)')INT(VALW)
                ICT(IC)=2; IC2(IC)=INT(VALT); IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.5)THEN
                SOTHER(ICOMP,ISS,1)='GROUND'
                write(SOTHER(ICOMP,ISS,2),'(i3.3)')iie2
                SOTHER(ICOMP,ISS,3)='0'
                ICT(IC)=4; IC2(IC)=iic2; IE2(IC)=0
              ELSEIF(IOS.EQ.6)THEN
                SOTHER(ICOMP,ISS,1)='GROUND'; SOTHER(ICOMP,ISS,2)='0'
                write(SOTHER(ICOMP,ISS,3),'(i3.3)')iie2
                ICT(IC)=4; IC2(IC)=0; IE2(IC)=iie2
              ELSEIF(IOS.EQ.7)THEN
                SOTHER(ICOMP,ISS,1)='ADIABATIC'
                SOTHER(ICOMP,ISS,2)='0'; SOTHER(ICOMP,ISS,3)='0'
                ICT(IC)=5; IE2(IC)=0; IC2(IC)=0
              ELSEIF(IOS.EQ.8)THEN
                SOTHER(ICOMP,ISS,1)='BASESIMP'
                write(SOTHER(ICOMP,ISS,2),'(i3.3)')INT(VALT)
                write(SOTHER(ICOMP,ISS,3),'(i3.3)')INT(VALW)
              ELSEIF(IOS.EQ.9)THEN
                SOTHER(ICOMP,ISS,1)='IDENT_CEN'
                write(SOTHER(ICOMP,ISS,2),'(i3.3)')INT(VALT)
                write(SOTHER(ICOMP,ISS,3),'(i3.3)')INT(VALW)
                ICT(IC)=7; IC2(IC)=INT(VALT); IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.10)THEN

C Reset to represent UNKNOWN
                SOTHER(ICOMP,ISS,1)='UNKNOWN'
                SOTHER(ICOMP,ISS,2)='0'; SOTHER(ICOMP,ISS,3)='0'
                ICT(IC)=-1; IE2(IC)=0; IC2(IC)=0
              ENDIF
              call warnmod(ICOMP,'sat')
              MODGEO=.TRUE.
            elseif(IRT.eq.4)then

C Call specify_other_face for each item in the array.
              call specify_other_face(icomp,iss,modgeo,showother,
     &          updoth,'d',ier)

            elseif(IRT.eq.5)then

C Update surface USE attributes for relevant zone surface and connection.
              if(USE1(1:7).eq.'FIXTURE')then
                if(nbofies.eq.0)then
                  call usrmsg(
     &              'There are no known IES data sets. Set these',
     &              'up via [advanced optics] menu.','W')
                  USE2='- '
                else
                  INPIC=1
                  CALL EPICKS(INPIC,IVALS,' ',' Known IES entities:',
     &              32,nbofies,iesmenu,' IES list',IER,nbhelp)
                  if(INPIC.EQ.0)then
                    USE2='- '
                  else
                    write(USE2,'(a)') iesname(ivals(1))
                  endif
                endif
              endif
              SUSE(ICOMP,ISS,1)=USE1; SUSE(ICOMP,ISS,2)=USE2

            endif
  11      continue

C Finally update the system configuration file.
          if(cfgok)then
            CALL EMKCFG('-',IER)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            silent= .false.
            call sumrchg(ICOMP,'r',silent)
          endif
          goto 42
        elseif(IS.gt.0)then

C Attribute a single surface, set flag to redraw.
          ianother=0
  443     call tstamp('>','PRJ: enter single surface attribution')
          CALL EDSURA(ICOMP,IS,ITRC,MODGEO,ianother,IER)
          MODIFYVIEW=.TRUE.

C If user asked for next or prior surface reset IS and call again.
          if(ianother.eq.0)then
            continue
          elseif(ianother.lt.0.and.IS.gt.1)then
            is=is-1
            goto 443
          elseif(ianother.gt.0.and.IS.lt.NZSUR(ICOMP))then
            is=is+1
            goto 443
          endif
          goto 42
        endif

      ELSEIF(INO.EQ.ITEMC+10)THEN

C Edit zone insolation distribution and/or user specified insolation distribution.
        call tstamp('>','PRJ: enter zone insolation distribution')
        CALL EDINSUL(ICOMP,IER)
        MODGEO=.TRUE.

      ELSEIF(INO.EQ.ITEMC+11)THEN

C Zone obstructions. If any unsaved zone geometry changes update
C the geometry file before entering the obstruction editing facility.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(MODGEO)then
          if(cfgok)then
            CALL EMKCFG('-',IER)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            silent = .false.
            call sumrchg(ICOMP,'r',silent)
          endif
          MODGEO=.false.
        endif
        call tstamp('>','PRJ: enter zone obstructions')
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.

C Re-establish proportional font in case it was reset elsewhere.
        if(IMFS.ge.0.and.IMFS.le.3)then
          if(IMFS.eq.0) IMFS=4
          if(IMFS.eq.1) IMFS=5
          if(IMFS.eq.2) IMFS=6
          if(IMFS.eq.3) IMFS=7
          call userfonts(IFS,ITFS,IMFS)
        endif
        if(iglib.eq.2)then
          call easkab(' ','Shading obstruction definition via:',
     &      'dimensional input','bitmap',IW,nbhelp)
          if(iw.eq.-3) goto 3   ! pay attention to cancel request.
        else
          call easkabc(' ','Shading obstruction definition via:',
     &      'dimensional input','bitmap','cancel',IW,nbhelp)
          if(iw.eq.-3) goto 3   ! pay attention to cancel request.
        endif
        if(iw.eq.1)then
          CALL EDOBS(ITRC,iuout,ICOMP,IER)
        elseif(iw.eq.2)then
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            call edisp(iuout,
     &        'Toggle to obstruction mode after you have set origin')
            call edisp(iuout,'and scale. ')
            icob=icomp   ! pass which zone obstr to focus on
            call clickonbitmap(icob,ier)
            call edisp(iuout,'You may further edit the obstructions...')
            CALL EDOBS(ITRC,iuout,ICOMP,IER)
          elseif(iglib.eq.2)then
            call usrmsg('The clickonbitmap facility is not yet working',
     &        'with GTK graphic library.','W')
          elseif(iglib.eq.3)then
            call usrmsg('The clickonbitmap facility requires the',
     &        'interface to be in graphic mode.','W')
          endif
        endif

      ELSEIF(INO.EQ.ITEMC+12)THEN

C Zone rotations and transforms.
  77    continue
        write(trnalt(1),'(a)') ' ...... Rotation ......    '
        write(trnalt(2),'(a,3f6.2,a)') 'a around v1 (',x(1),
     &                                 y(1),z(1),')'
        write(trnalt(3),'(a)') 'b around another vertex    '
        write(trnalt(4),'(a)') 'c around site origin       '
        write(trnalt(5),'(a)') 'd around specified X Y     '
        write(trnalt(6),'(a)') ' ...... Transform ......   '
        write(trnalt(7),'(a)') 'e to specified X Y Z       '
        write(trnalt(8),'(a)') 'f to vertex in this zone   '
        write(trnalt(9),'(a)') 'g to vertex in another zone'
        write(trnalt(10),'(a)')' ...... Mirror ......      '
        write(trnalt(11),'(a)')'h Y axis positive direction'
        write(trnalt(12),'(a)')'i Y axis negative direction'
        write(trnalt(13),'(a)')'j X axis positive direction'
        write(trnalt(14),'(a)')'k X axis negative direction'
        write(trnalt(15),'(a)')' ...... Scale ......       '
        write(trnalt(16),'(a)')'l scaling factor for X Y Z '
        write(trnalt(17),'(a)')' ...... Invert ......      '
        write(trnalt(18),'(a)')'m invert surface edge order'
        write(trnalt(19),'(a)')' __________________________'
        write(trnalt(20),'(a)')'? help                     '
        write(trnalt(21),'(a)')'- exit menu                '

C Help for rotation/transform/mirror options
        helptopic='geometry_rotation_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)

        WRITE(HEAD,'(2a)')'Transforms for Zone ',zname(ICOMP)

C Menu for zone rotations and transforms.
        NITMS=21
        CALL EMENU(HEAD,trnalt,NITMS,INOR)
        if(INOR.EQ.21)then
          goto 3
        elseif(INOR.EQ.20)then

C Produce help text for the menu.
          CALL PHELPD('rotation section',nbhelp,'-',0,0,IER)
          goto 77
        elseif(INOR.EQ.18)then

C Invert edge ordering of all surfaces in the zone. First warn
C user and allow exit.
C Reverse the ordering, redraw image and return..
          CALL EASKOK(' ',
     &       'Reverse the vertex ordering of zone surfaces?',
     &        OK,nbhelp)
          if(OK)then
            do ins=1,NSUR
              do iyy = 1,NVER(ins)
                jvn1(iyy)=JVN(ins,iyy)
              enddo
              JVN(ins,1)=jvn1(2)
              JVN(ins,2)=jvn1(1)
              do iyy = 3,NVER(ins)
                izz=NVER(ins)+3-iyy
                JVN(ins,iyy)=jvn1(izz)
              enddo

C If construction symmetric do nothing but if it
C references a non-symetric MLC then use an inverted MLC.
              call matchmlcdesc(SMLCN(icomp,ins),imlcindex)
              if(imlcindex.ne.0)then
                ii=imlcindex
                lnssmlc=lnblnk(SMLCN(ICOMP,ins))
                if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                  if(SMLCN(ICOMP,ins)(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcname(ii)))then
                    SMLCN(ICOMP,ins)=mlcname(ii)
                  elseif(SMLCN(ICOMP,ins)(1:4).eq.'UNKN')then
                    SMLCN(ICOMP,ins)=mlcname(ii)
                  else
                    SMLCN(ICOMP,ins)=mlcname(ii)
                  endif
                  iissmlci=ii
                elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (if db does not have a linked MLC, inform the user).
                  SMLCN(ICOMP,ins)=mlcsymetric(ii)
                  iissmlci=matsymindex(ii)  ! use returned value
                else
                  lnssmlc=lnblnk(SMLCN(ICOMP,ins))
                  if(SMLCN(ICOMP,ins)(1:4).eq.'UNKN')then
                    SMLCN(ICOMP,ins)=mlcsymetric(ii)
                  elseif(SMLCN(ICOMP,ins)(1:lnssmlc).eq.
     &                   mlcsymetric(ii)(1:lnssmlc))then
                    SMLCN(ICOMP,ins)=mlcsymetric(ii)
                  else
                    SMLCN(ICOMP,ins)=mlcsymetric(ii)
                  endif
                  iissmlci=matsymindex(ii)       ! use returned value
                endif
                if(iissmlci.ne.0)then
                  smlcindex(ICOMP,ins)=iissmlci ! update
                endif
              endif
            enddo ! of ins
            iZBFLG(ICOMP)=0   ! reset
            call zgupdate(1,icomp,ier)  ! update commons
            call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            MODIFYVIEW=.TRUE.
            CALL INLNST(1)
            nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
            CALL ADJVIEW(IER)
            goto 77
          endif
        elseif(INOR.EQ.2.or.INOR.eq.3.or.INOR.eq.4.or.INOR.eq.5)then

C Remember rotation request.
          VAL=0.
          CALL EASKR(VAL,' ','Rotation (+ = anticlockwise)?',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(VAL.LT.-.01.OR.VAL.GT..01)then
            if(INOR.eq.2)then
              x1=x(1)     ! get position of vertex 1.
              y1=y(1)
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0; rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
            elseif(INOR.eq.3)then

C Scan G1 for vertes in this zone and allow user to select one.
              inpick=1
              call EPKVERT(INPICK,IVLST,'Rotation Point',
     &          'Select vertex to rotate around.',' ',nbhelp,ier)
              if(inpick.eq.1)then
                ivc=IVLST(1)
                x1=x(ivc); y1=y(ivc)     ! get position of this vertex.
                rotateit(icomp,1)=0.0; rotateit(icomp,2)=0.0
                rotateit(icomp,3)=0.0
              else
                goto 77
              endif
              CALL ESCROT(VAL,x1,y1)
            elseif(INOR.eq.4)then
              x1=0.0; y1=0.      ! site origin is X=0. Y=0.
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0; rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
            elseif(INOR.eq.5)then
              x1=0.
              CALL EASKR(x1,' ','X-coord?',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              y1=0.
              CALL EASKR(y1,' ',' Y-coord?',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0; rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
            endif
          else
            goto 77
          endif

C Rotation of nodes and components associated with surfaces in the
C zone need to be dealt with.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &      'Rotating a zone may require updating of flow network',
     &      'connections to wind boundary nodes. Please check!','W')
            goforit=.false.
            PI = 4.0 * ATAN(1.0)
            A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
            do ij=1,nzsur(icomp)
              call doesflowrefsurface(icomp,ij,inod,icmp)
              if(inod.gt.0)then  ! relocate node and alter orientation
                XXX=HNOD(INOD,1)-X1
                YYY=HNOD(INOD,2)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                HNOD(INOD,1)=XR+X1
                HNOD(INOD,2)=YR+Y1
                SUPNOD(INOD,2)=SUPNOD(INOD,2)+VAL
                goforit=.true.
              endif
              if(icmp.gt.0)then  ! relocate component
                XXX=HCMP(ICMP,1,1)-X1
                YYY=HCMP(ICMP,1,2)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                HCMP(ICMP,1,1)=XR+X1
                HCMP(ICMP,1,2)=YR+Y1
                goforit=.true.
              endif
            enddo

C Also reflect change in zone COG.
            i=ICAAS(icomp)
            HNOD(i,1)=ZCOG(icomp,1); HNOD(i,2)=ZCOG(icomp,2)
            HNOD(i,3)=ZCOG(icomp,3)
            write(outs,'(3a,3f7.3)') 'Updating ',NDNAM(i),' @',
     &        HNOD(i,1),HNOD(i,2),HNOD(i,3)
            call usrmsg(outs,' ','P')
            if(goforit)then
              call updatebothflownetworks(ier)
            endif
          endif

          iZBFLG(ICOMP)=0

          if(nbvis(icomp).gt.0)then

C Visual object rotation.
            ANGR=VAL
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.0; CA=COS(A); SA=SIN(A)
            do ij=1,nbvis(icomp)
              XXX=XOV(ICOMP,ij)-X1; YYY=YOV(ICOMP,ij)-Y1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              XOV(ICOMP,ij)=XR+X1; YOV(ICOMP,ij)=YR+Y1
              BANGOV(ICOMP,ij,1)=BANGOV(ICOMP,ij,1)+ANGR
              do ibe=1,8
                XXX=XVP(icomp,ij,ibe)-X1
                YYY=YVP(icomp,ij,ibe)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                XVP(icomp,ij,ibe)=XR+X1
                YVP(icomp,ij,ibe)=YR+Y1
              enddo  ! of ibe
            enddo    ! of ij
          endif

          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Warn user if there is a flow node associated with this zone that
C zone rotation may require boundary nodes to be updated.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &        'Rotating a zone may require updating of flow network ',
     &        'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')

C Rotate any MRT sensors associated with this zone.
C Test the error return from ermrt call.
          if (IVF(ICOMP).eq.1) then
            call edisp(iuout,' Viewfactors prior to transform.')
            CALL ERMRT(ITRC,iuout,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
              call usrmsg('zone surface mrt surfs mismatch',' ','W')
            endif

            if (NCUB(ICOMP).gt.0) then
              call easkok(' ','Apply rotation to zone MRT sensors',
     &                   OK,nbhelp)
              if (OK) then

C Depending on which point of rotation for the zone the
C rotation point x1 and y1 will have been set above. Update
C the viewfactor file as well as the block orientation held
C in the zone geometry file.
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
                do 76 ij=1,NCUB(ICOMP)
                  XXX=XOC(ij)-X1; YYY=YOC(ij)-Y1
                  XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                  XOC(ij)=XR+X1; YOC(ij)=YR+Y1
                  CANG(ij)=CANG(ij)+VAL
  76            continue
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
                endif
              endif
            else
              call usrmsg(
     &          'A transform will not alter the view factors between',
     &          'the surfaces in the zone.','P')
            endif
          endif

C Rotate any obstructions associated with this zone. First do
C this for obs in separate file.
          if(IOBS(icomp).eq.1)then
            call easkok(' ','Apply rotation to zone obstructions?',
     &                 ok,nbhelp)
            if(ok)then
              call edisp(iuout,' ')
              call edisp(iuout,' Obstruction data prior to transform.')
              itru=iuout
              CALL EGOMST(IFIL+2,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
              do 86 ij=1,nbobs(icomp)
                XXX=XOB(icomp,ij)-X1; YYY=YOB(icomp,ij)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                if(BLOCKTYP(icomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(icomp,ij)(1:4).eq.'obs3')then
                  XOB(icomp,ij)=XR+X1; YOB(icomp,ij)=YR+Y1
                  BANGOB(icomp,ij,1)=BANGOB(icomp,ij,1)+VAL
                else
                  do ibe=1,8
                    XXX=XBP(icomp,ij,ibe)-X1
                    YYY=YBP(icomp,ij,ibe)-Y1
                    XR=XXX*CA+YYY*SA
                    YR=YYY*CA-XXX*SA
                    XBP(icomp,ij,ibe)=XR+X1
                    YBP(icomp,ij,ibe)=YR+Y1
                  enddo
                endif
  86          continue
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          elseif(iobs(icomp).eq.2)then

C The zone geometry has just been written with transformed verticies
C so now apply the transform to the obstructions (only) as well.
C as transform general polygon obstructions.
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            call easkok(' ','Apply rotation to zone obstructions?',
     &                 ok,nbhelp)
            if(ok.and.newgeo)then
              call edisp(iuout,' ')
              call edisp(iuout,' Obstruction data prior to transform.')
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.0; CA=COS(A); SA=SIN(A)
              do 87 ij=1,nbobs(icomp)
                XXX=XOB(icomp,ij)-X1; YYY=YOB(icomp,ij)-Y1
                XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                if(BLOCKTYP(icomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(icomp,ij)(1:4).eq.'obs3')then
                  XOB(icomp,ij)=XR+X1
                  YOB(icomp,ij)=YR+Y1
                  BANGOB(icomp,ij,1)=BANGOB(icomp,ij,1)+VAL
                else
                  do ibe=1,8
                    XXX=XBP(icomp,ij,ibe)-X1
                    YYY=YBP(icomp,ij,ibe)-Y1
                    XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
                    XBP(icomp,ij,ibe)=XR+X1
                    YBP(icomp,ij,ibe)=YR+Y1
                  enddo
                endif
  87          continue
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
          endif

        elseif(INOR.eq.7.or.INOR.eq.8.or.INOR.eq.9)then
          if(INOR.eq.7)then

C Ask the user what the transform for each axis should be.
            call tstamp('>','PRJ: do zone translation')
            hold = ' 0.000  0.000  0.000    '
  43        CALL EASKS(HOLD,'Transform (X Y Z metres) to apply:',' ',
     &        36,' 0. 0. 0.','transforms XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
            CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
            if(ier.ne.0)goto 43
            call easkok(' ','Apply transform to zone surfaces?',
     &           ok,nbhelp)
            if(.NOT.ok) goto 3
          elseif(INOR.eq.8)then

C Identify two points that define the vector and find the difference
C between the two X and two Y and two Z. This uses G1 commons.
            inpick=2
            call EPKVERT(INPICK,IVLST,
     &        'Define Vector',
     &        'Select start and finish vertex.',' ',nbhelp,ier)
            if(inpick.eq.2)then
              ivcs=IVLST(1); ivcf=IVLST(2)
              x1=x(ivcs); y1=y(ivcs); z1=z(ivcs)     ! get position of initial vertex.
              x2=x(ivcf); y2=y(ivcf); z2=z(ivcf)     ! get position of initial vertex.
              VALX=x2-x1; VALY=y2-y1; VALZ=z2-z1
              write(msgv,'(a,3F8.4)') 'Transform is ',
     &          VALX,VALY,VALZ
              call easkok(msgv,'Is this OK?',ok,nbhelp)
              if(.NOT.ok) goto 77
            else
              goto 77
            endif
          elseif(INOR.eq.9)then

C Ask the user for a start vertex in this zone and a final vertex in
C another zone. This will require saving and loading geometry common
C blocks (epkvert uses common G1).
            x1=0.0; y1=0.0; z1=0.0
            inpick=1
            call EPKVERT(INPICK,IVLST,'Start Point',
     &        'Select starting vertex.',' ',nbhelp,ier)
            if(inpick.eq.1)then
              ivcs=IVLST(1)
              x1=x(ivcs); y1=y(ivcs); z1=z(ivcs)     ! get position of this vertex.
            else
              goto 77
            endif

C Select the other zone, if it is a different zone then save any
C pending changes to the current zone and draw the other zone
C and temporarily fill the geometry common blocks in order to
C present the user with list of vertices to select from in the
C other zone.
            CALL EASKGEOF(
     &        'Select other zone for location to transform to:',
     &        CFGOK,IZ,'-',34,IER)
            if(iz.eq.icomp)goto 3
            if(iz.eq.0) goto 3
            call usrmsg(' updating current zone before proceeding...',
     &        ' ','P')
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            MODIFYVIEW=.TRUE.
            MODBND=.TRUE.
            CALL INLNST(1)
            itsnm=0
            nzg=1; nznog(1)=IZ; izgfoc=ICOMP
            CALL redraw(IER)
            MODIFYVIEW=.TRUE.; MODBND=.TRUE.

C Load and scan G1 for vertes in other zone and allow user to select one.
            call georead(IFIL+1,LGEOM(IZ),IZ,0,iuout,IER)
            inpick=1
            call EPKVERT(INPICK,IVLST,'Other Zone Vertex',
     &        'Select vertex in other zone.',' ',nbhelp,ier)
            if(inpick.eq.1)then
              ivcf=IVLST(1)
              x2=x(ivcf); y2=y(ivcf); z2=z(ivcf)     ! get position of this vertex.

C Re-establish the initial zone geometry.
              call georead(IFIL+1,LGEOM(icomp),icomp,0,iuout,IER)
              VALX=x2-x1; VALY=y2-y1; VALZ=z2-z1
              write(msgv,'(a,3F8.4)') 'Transform is ',
     &          VALX,VALY,VALZ
              call easkok(msgv,'Is this ok?',ok,nbhelp)
              if(.NOT.ok) goto 77
            else

C User aborted so scan the intial zone prior to returning.
              call georead(IFIL+1,LGEOM(icomp),icomp,0,iuout,IER)
              MODBND=.TRUE.; MODIFYVIEW=.TRUE.
              goto 77
            endif
         endif

C If user has not canceled then perform the transform.
          DO I=1,NTV
            X(I)=X(I)+VALX; Y(I)=Y(I)+VALY; Z(I)=Z(I)+VALZ
          enddo
          iZBFLG(ICOMP)=0

          if(nbvis(icomp).gt.0)then

C Visual object transforms.
            do ij=1,nbvis(icomp)
              XOV(icomp,ij)=XOV(icomp,ij)+VALX
              YOV(icomp,ij)=YOV(icomp,ij)+VALY
              ZOV(icomp,ij)=ZOV(icomp,ij)+VALZ
              do ibe=1,8
                XVP(icomp,ij,ibe)=XVP(icomp,ij,ibe)+VALX
                YVP(icomp,ij,ibe)=YVP(icomp,ij,ibe)+VALY
                ZVP(icomp,ij,ibe)=ZVP(icomp,ij,ibe)+VALZ
              enddo  ! of ibe
            enddo    ! of ij
          endif

          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            if(INOR.eq.7.or.INOR.eq.8.or.INOR.eq.9)then

C Apply the transform to the position of nodes and components
C associated with this zone. Loop through all the surfaces and
C items associated in the network apply this transform to.
              goforit=.false.
              do ij=1,nzsur(icomp)
                call doesflowrefsurface(icomp,ij,inod,icmp)
                if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &          'Transforming a zone may require updating of flow',
     &          'netork entities. Please check!','W')
                if(inod.gt.0)then
                  HNOD(INOD,1)=HNOD(INOD,1)+VALX
                  HNOD(INOD,2)=HNOD(INOD,2)+VALY
                  HNOD(INOD,3)=HNOD(INOD,3)+VALZ
                  goforit=.true.
                endif
                if(icmp.gt.0)then
                  HCMP(ICMP,1,1)=HCMP(ICMP,1,1)+VALX
                  HCMP(ICMP,1,2)=HCMP(ICMP,1,2)+VALY
                  HCMP(ICMP,1,3)=HCMP(ICMP,1,3)+VALZ
                  goforit=.true.
                endif
              enddo
              if(goforit)then
                call updatebothflownetworks(ier)
              endif
            endif
          endif
          call warnmod(ICOMP,'str')

C Transform any MRT sensors associated with this zone.
C << if newer geometry file also write new sensor data >>
C Test the error return from ermrt call.
          if (IVF(ICOMP).eq.1) then
            CALL ERMRT(ITRC,iuout,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
C              write(6,*) 'zone surface mrt surfs mismatch'
            endif
            if (NCUB(ICOMP).gt.0) then
              call easkok(' ','Apply transform to zone MRT sensors?',
     &                   ok,nbhelp)
              if (OK) then
                do ij=1,NCUB(ICOMP)
                  XOC(ij)=XOC(ij)+VALX
                  YOC(ij)=YOC(ij)+VALY
                  ZOC(ij)=ZOC(ij)+VALZ
                enddo

C Update the MRT block origin in the viewfactor file as well as
C in the zone geometry file if it is a current version.
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
                endif
              endif
            else
              call usrmsg(
     &          'A transform will not alter the view factors between',
     &          'the surfaces in the zone.','P')
            endif
          endif

C Transform any obstructions associated with this zone.
          if(iobs(icomp).eq.1)then
            call easkok(' ','Apply transform to zone obstructions?',
     &                 ok,nbhelp)
            if(ok)then
              call edisp(iuout,' ')
              call edisp(iuout,'Obstruction data prior to transform.')
              CALL EGOMST(IFIL+2,ICOMP,ZOBS(ICOMP),0,ITRC,iuout,IER)
              do ij=1,nbobs(icomp)
                if(BLOCKTYP(icomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(icomp,ij)(1:4).eq.'obs3')then
                  XOB(icomp,ij)=XOB(icomp,ij)+VALX
                  YOB(icomp,ij)=YOB(icomp,ij)+VALY
                  ZOB(icomp,ij)=ZOB(icomp,ij)+VALZ
                else
                  do ibe=1,8
                    XBP(icomp,ij,ibe)=XBP(icomp,ij,ibe)+VALX
                    YBP(icomp,ij,ibe)=YBP(icomp,ij,ibe)+VALY
                    ZBP(icomp,ij,ibe)=ZBP(icomp,ij,ibe)+VALZ
                  enddo
                endif
              enddo
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          elseif(iobs(icomp).eq.2)then

C The zone geometry has just been written with transformed verticies
C so now apply the transform to the obstructions (only).
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            call easkok(' ','Apply transform to zone obstructions?',
     &                 ok,nbhelp)
            if(ok.and.newgeo)then
              call edisp(iuout,' ')
              call edisp(iuout,'Obstruction data prior to transform.')
              do ij=1,nbobs(icomp)
                if(BLOCKTYP(icomp,ij)(1:4).eq.'obs '.or.
     &             BLOCKTYP(icomp,ij)(1:4).eq.'obs3')then
                  XOB(icomp,ij)=XOB(icomp,ij)+VALX
                  YOB(icomp,ij)=YOB(icomp,ij)+VALY
                  ZOB(icomp,ij)=ZOB(icomp,ij)+VALZ
                else
                  do ibe=1,8
                    XBP(icomp,ij,ibe)=XBP(icomp,ij,ibe)+VALX
                    YBP(icomp,ij,ibe)=YBP(icomp,ij,ibe)+VALY
                    ZBP(icomp,ij,ibe)=ZBP(icomp,ij,ibe)+VALZ
                  enddo
                endif
              enddo
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
          endif

        elseif(INOR.eq.11.or.INOR.eq.12.or.INOR.eq.13.or.INOR.eq.14)then

C Mirror a zone (cardinal directions only). Ask for NS SN EW WE, then
C the mirror point on axis, warn user to save first. Also invert
C each surface.
          helptopic='geometry_mirror_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('axis mirror',nbhelp,'-',0,0,IER)

          if(INOR.eq.11.or.INOR.eq.12)then

C Mirror along Y axis, either positive or negative.
            call tstamp('>','PRJ: do zone mirror')
            CALL EASKR(VALM,' ','Mirror point on Y-axis?',
     &           -50.0,'W',50.0,'W',0.0,'y mirror',IER,nbhelp)
            call easkok(' ','Apply mirror transform?',ok,nbhelp)
            if(ok)then
              do iwmv=1,NTV
                if(INOR.eq.11)then
                  VALY=VALM-Y(iwmv)
                  Y(iwmv)=VALM+VALY
                elseif(INOR.eq.12)then
                  VALY=Y(iwmv)-VALM
                  Y(iwmv)=VALM-VALY
                endif
              enddo
            else
              goto 77
            endif
          elseif(INOR.eq.13.or.INOR.eq.14)then

C Mirror along X axis, either positive or negative.
            call tstamp('>','PRJ: do zone mirror')
            CALL EASKR(VALM,' ','Mirror point on X-axis?',
     &           -50.0,'W',50.0,'W',0.0,'y mirror',IER,nbhelp)
            call easkok(' ','Apply mirror transform?',ok,nbhelp)
            if(ok)then
              do iwmv=1,NTV
                if(INOR.eq.13)then
                  VALX=VALM-X(iwmv)
                  X(iwmv)=VALM+VALX
                elseif(INOR.eq.14)then
                  VALX=X(iwmv)-VALM
                  X(iwmv)=VALM-VALX
                endif
              enddo
            else
              goto 77
            endif
          endif

C Now invert the ordering of the surfaces.
C <<  Note: this can cause problems when exporting the
C <<  model (e.g. for EnergyPlus) so more work is required).
          do 148, ins=1,NSUR
            do iyy = 1,NVER(ins)
              jvn1(iyy)=JVN(ins,iyy)
            enddo
            JVN(ins,1)=jvn1(2)
            JVN(ins,2)=jvn1(1)
            do iyy = 3,NVER(ins)
              izz=NVER(ins)+3-iyy
              JVN(ins,iyy)=jvn1(izz)
            enddo
  148     continue
          iZBFLG(ICOMP)=0

C Logic for deciding which format of geometry file to write.
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Warn user if there is a flow node associated with this zone that
C zone mirroring may require boundary nodes to be updated.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &        'Mirroring a zone will require manual updating of flow',
     &        'network items. Please remember to do this!','W')
          endif
          call warnmod(ICOMP,'str')
        elseif(INOR.eq.16)then

C Scale surfaces and origins of rectangular bodies.
          hold = ' 1.0  1.0  1.0'
  44      CALL EASKS(HOLD,'X Y & Z scaling factor?',' ',
     &      32,' 1.0  1.0   1.0 ','scaling XYZ',ISER,nbhelp)
          if(iser.eq.-3) goto 3
          K=0
          CALL EGETWR(HOLD,K,VALX,0.1,10.1,'W','X scale',IER)
          CALL EGETWR(HOLD,K,VALY,0.1,10.1,'W','Y scale',IER)
          CALL EGETWR(HOLD,K,VALZ,0.1,10.1,'W','Z scale',IER)
          if(ier.ne.0)goto 44
          DO I=1,NTV
            X(I)=X(I)*VALX; Y(I)=Y(I)*VALY; Z(I)=Z(I)*VALZ
          enddo
          iZBFLG(ICOMP)=0

          if(nbvis(icomp).gt.0)then

C Visual object scale origins.
            do ij=1,nbvis(icomp)
              XOV(icomp,ij)=XOV(icomp,ij)*VALX
              YOV(icomp,ij)=YOV(icomp,ij)*VALY
              ZOV(icomp,ij)=ZOV(icomp,ij)*VALZ
              do ibe=1,8
                XVP(icomp,ij,ibe)=XVP(icomp,ij,ibe)*VALX
                YVP(icomp,ij,ibe)=YVP(icomp,ij,ibe)*VALY
                ZVP(icomp,ij,ibe)=ZVP(icomp,ij,ibe)*VALZ
              enddo  ! of ibe
            enddo    ! of ij
          endif

          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          call warnmod(ICOMP,'str')

C Scale origins of any MRT sensors associated with this zone.
C If newer geometry file also write new sensor data to the zone
C geometry file as well as the viewfactor file.
C Test the error return from ermrt call.
          if (IVF(ICOMP).eq.1) then
            CALL ERMRT(ITRC,iuout,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if(ier.eq.3)then
C              write(6,*) 'zone surface mrt surfs mismatch'
            endif
            if (NCUB(ICOMP).gt.0) then
              call easkok(' ',
     &           'Apply scaling to zone MRT sensor origins?',
     &            ok,nbhelp)
              if (OK) then
                do ij=1,NCUB(ICOMP)
                  XOC(ij)=XOC(ij)*VALX
                  YOC(ij)=YOC(ij)*VALY
                  ZOC(ij)=ZOC(ij)*VALZ
                enddo
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
                endif
              endif
            else
              call usrmsg('Scaling might alter the view factors',
     &                    'between the surfaces in the zone.','P')
            endif
          endif

C Transform any obstructions associated with this zone.
          if(iobs(icomp).eq.1)then
            call easkok(' ',
     &        'Apply scaling to zone obstruction origins?',
     &                 ok,nbhelp)
            if(ok)then
              call edisp(iuout,' ')
              call edisp(iuout,'Obstruction data prior to transform.')
              CALL EGOMST(IFIL+2,ICOMP,ZOBS(ICOMP),0,ITRC,iuout,IER)
              do ij=1,nbobs(icomp)
                XOB(icomp,ij)=XOB(icomp,ij)*VALX
                YOB(icomp,ij)=YOB(icomp,ij)*VALY
                ZOB(icomp,ij)=ZOB(icomp,ij)*VALZ
              enddo
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          elseif(iobs(icomp).eq.2)then

C The zone geometry has just been written with transformed verticies
C so now apply scaling to obstruction origins (only).
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            call easkok(' ','Apply scaling to obstruction origins?',
     &                 ok,nbhelp)
            if(ok.and.newgeo)then
              call edisp(iuout,' ')
              call edisp(iuout,'Obstruction data prior to transform.')
              do ij=1,nbobs(icomp)
                XOB(icomp,ij)=XOB(icomp,ij)*VALX
                YOB(icomp,ij)=YOB(icomp,ij)*VALY
                ZOB(icomp,ij)=ZOB(icomp,ij)*VALZ
              enddo
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
          endif

        elseif(INOR.eq.1.or.INOR.eq.6.or.INOR.eq.10.or.INOR.eq.15.or.
     &         INOR.eq.17)then
          goto 77
        endif

C Ensure that the zone is redrawn and boundaries are checked.
        MODIFYVIEW=.TRUE.; MODBND=.TRUE.; MODGEO=.TRUE.

      ELSEIF(INO.EQ.ITEMC+13)THEN

C Check the file version, if new then allow thermal bridges. If
C there are existing bridges then do not scan until within
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          helptopic='scan_bridge_choice'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('bridge rescan',nbhelp,'-',0,0,IER)
          if(nbrdg(icomp).gt.0)then
            call easkabc(' ',
     &        'Zone has existing thermal bridge definitions',
     &        'use them','rescan geometry','cancel',IW,nbhelp)
            if(IW.eq.1)then
              continue
            elseif(IW.eq.2)then
              act = '-'
              call edisp(iuout,' ')
              call edisp(iuout,'Scanning the surface edges...')
              call scan_bridges(itrc,act,icomp,ier) ! Determine bridge relationships.
            else
              goto 3
            endif
          else
            act = '-'
            call edisp(iuout,' ')
            call edisp(iuout,'Scanning the surface edges...')
            call scan_bridges(itrc,act,icomp,ier) ! Determine bridge relationships.
          endif
          call edisp(iuout,' ')
          call edisp(iuout,'Options to manage thermal bridges...')
          CALL LINTHBRDG(ICOMP,'-',0.0)      ! Manage thermal bridges.
        else
          call usrmsg('Simple thermal bridges not suppored in older',
     &                'format geometry files.','W')
        endif

      ELSEIF(INO.EQ.ITEMC+14)THEN

C Definition of BASESIMP definitions for this zone. The user is also
C asked when the return from the surface attribute menu if they want
C to work on BASESIMP data if there are any of the surfaces in the
C zone what are associated with BASESIMP
        CALL BASESIMP_INPUTS(ICOMP,IER)

      ELSEIF(INO.EQ.ITEMC+15)THEN

C Visual entities.

C << Note if someone transforms an existing visual entity it might be
C << linked to mass surfaces which are not transformed. What to remember
C << in order also shift the associated surfaces.
C << What about another token in the visual line??

        CALL EDVIS(iuout,ICOMP,IER)
      ELSEIF(INO.EQ.ITEMC+16)THEN

C Predefined entities. To avoid name clashes adapt the names of visual entities and
C compound visuals that are being imported. If nothing was selected jump.
        call choosepredef(name,objmenu,objbb,ier)
        if(ier.eq.1)then
          call usrmsg('Predefined objects not found or the file',
     &                'was corrupt','W')
          goto 3
        elseif(ier.eq.2)then
          goto 3
        endif

C Take into account path to the db.
        lndbp=lnblnk(standarddbpath)
        if(ipathpredef.eq.0.or.ipathpredef.eq.1)then
          predef=LPREDEF  ! use as is
        elseif(ipathpredef.eq.2)then
          write(predef,'(3a)') standarddbpath(1:lndbp),fs,
     &      LPREDEF(1:lnblnk(LPREDEF))  ! prepend db folder path
        endif
        DX=0.0; DY=0.0; DZ=0.0  ! initial origin

C Ask where to place it (get offsets and rotation) and then use GB1
C to temporarily draw it.
        call RPREDEFCOM(IFA,predef,name,IER)
        call edisp(iuout,'  ')
        write(outs,'(3a)')'bounding box of ',objmenu,' is:'
        call edisp(iuout,outs)
        write(outs,'(3f7.3,a)') objbb(1),objbb(2),objbb(3),' XYZm'
        call edisp(iuout,outs)
        call edisp(iuout,objnotes(1))
        if(nbobjnotes.gt.1) call edisp(iuout,objnotes(2))
        if(nbobjnotes.gt.2) call edisp(iuout,objnotes(3))

        VALR=0.
 423    CALL EASKR(VALR,' Object rotation (+ = anticlockwise)',
     &    'around its lower left corner:',
     &    -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)

        call easkabc(' ','Origin choices:','edit object origin',
     &    'use zone vertex','abort',ibopt,nbhelp)
        if(ibopt.eq.1)then
          write(hold,'(3f8.4)') DX,DY,DZ
        elseif(ibopt.eq.2)then

C User current zone G1 common block to present list of points.
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &      'Select a vertex to define the origin...',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich1=IVLST(1)
            HOLD=' '
            WRITE(hold,'(1x,3f8.4)')X(iwhich1),Y(iwhich1),Z(iwhich1)
          else
            goto 3
          endif
        elseif(ibopt.eq.3)then
          goto 3
        endif
        CALL EASKS(HOLD,'Confirm object origin @ (X Y Z metres)',
     &    'i.e. within the room: ',
     &    36,' 0. 0. 0.','transforms XYZ',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,DX,-99.,99.,'W','X tr',IER)
        CALL EGETWR(HOLD,K,DY,-99.,99.,'W','Y tr',IER)
        CALL EGETWR(HOLD,K,DZ,-99.,99.,'W','Z tr',IER)
        CALL CNVBLK(DX,DY,DZ,objbb(1),objbb(2),objbb(3),VALR)

C Scan into commons and write out a QA report.
  424   call RPREDEFCOM(IFA,predef,name,IER)
        lltmp=' '
        CALL OBJQA(iunit,lltmp,'QA',IER)

        write(temp,'(A)')name  ! Peview the bounding box of the object via DRWBB.
        call drwbb(temp,ier)

C Now draw the visual entities use simple code if zero rotation.
        if(nbobjvis.gt.0)then
          DO 301 IB=1,nbobjvis
            call eclose(VALR,0.0,0.01,closeangr)
            if(closeangr)then
              ODX=OBJXOV(IB)+DX; ODY=OBJYOV(IB)+DY; ODZ=OBJZOV(IB)+DZ
            else
              PI = 4.0 * ATAN(1.0)
              A=-VALR*PI/180.; CA=COS(A); SA=SIN(A)
              X1=DX; Y1=DY                   ! The new origin 
              XXX=(OBJXOV(IB)+DX)-X1        ! object orign with offsets
              YYY=(OBJYOV(IB)+DY)-Y1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              ODX=XR+X1; ODY=YR+Y1; ODZ=OBJZOV(IB)+DZ ! computed transformed object origin
C              write(6,*) 
C     &'pi,DX,DY,IB,OBJXOV(IB),OBJYOV(IB),X1,Y1,XXX,YYY,XR,YR'
C              write(6,*) 
C     &'pi',DX,DY,IB,OBJXOV(IB),OBJYOV(IB),X1,Y1,XXX,YYY,XR,YR
            endif
            OANG=objbangov(IB,1)+VALR
            OANG2=objbangov(IB,2)+VALR
            OANG3=objbangov(IB,3)+VALR
            if(objvistyp(IB)(1:4).eq.'vis ')then
C              write(6,*) 'cnvblk ',ODX,ODY,ODZ,
C     &          OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),OANG
              CALL CNVBLK(ODX,ODY,ODZ,
     &          OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),OANG)
            elseif(objvistyp(IB)(1:4).eq.'vis3')then
              CALL CNVBLK3A(ODX,ODY,ODZ,
     &          OBJDXOV(IB),OBJDYOV(IB),OBJDZOV(IB),
     &          OANG,OANG2,OANG3)
            elseif(objvistyp(IB)(1:4).eq.'visp')then
C              call CNVOBJVISP(IB) ! cannot preview predefined visp type.
            endif
            write(temp,'(A)')objvisname(IB)
            write(temp,'(A)') '.'
            call drwbb(temp,ier)
            call forceflush()
            call pausems(20)
  301     CONTINUE
        endif

C << place to include a nudge option >>
        CALL EASKABCD(' ','Options for predefined objects:',
     &    'apply transform and merge','revise transform','nudge X Y Z',
     &    'cancel',IW,nbhelp)
        if (IW.eq.1) then

C Ask for single character to prepend to copied associated entities.
          CALL EASKATOG(' ','Character to pre-pend to copied entities:',
     &    ' a ',' b ',' c ',' d ',' e ',' f ','...',IBOPT,nbhelp)
          if(ibopt.eq.1)prec='a'
          if(ibopt.eq.2)prec='b'
          if(ibopt.eq.3)prec='c'
          if(ibopt.eq.4)prec='d'
          if(ibopt.eq.5)prec='e'
          if(ibopt.eq.6)prec='f'
          if(ibopt.eq.7)then
            CALL EASKATOG(' ',
     &      'Character to pre-pend to copied entities:',
     &      ' g ',' h ',' i ',' j ',' k ',' l ',' m',IBOPT,nbhelp)
            if(ibopt.eq.1)prec='g'
            if(ibopt.eq.2)prec='h'
            if(ibopt.eq.3)prec='i'
            if(ibopt.eq.4)prec='j'
            if(ibopt.eq.5)prec='k'
            if(ibopt.eq.6)prec='l'
          endif
          call PREDEFEMBED(IFIL+2,predef,ICOMP,DX,DY,DZ,VALR,name,
     &      prec,IER)
        elseif (IW.eq.2) then
          goto 423   ! adapt location
        elseif (IW.eq.3) then
          CALL EASKATOG(' ','Nudge origin:',
     &    'x -0.1 ','x +0.1','y -0.1','y +0.1','z -0.1','z +0.1',
     &    'rotate...',IBOPT,nbhelp)
          if(ibopt.eq.1) DX=DX-0.1
          if(ibopt.eq.2) DX=DX+0.1
          if(ibopt.eq.3) DY=DY-0.1
          if(ibopt.eq.4) DY=DY+0.1
          if(ibopt.eq.5) DZ=DZ-0.1
          if(ibopt.eq.6) DZ=DZ+0.1
          if(ibopt.eq.7)then
            CALL EASKATOG(' ','Nudge Z rotation:',
     &        '15deg anti','30deg anti','45deg anti','15deg clkw',
     &        '30deg clkw','45deg clkw',' ',IBOPT,nbhelp)
            if(ibopt.eq.1) VALR=VALR+15.
            if(ibopt.eq.2) VALR=VALR+30.
            if(ibopt.eq.3) VALR=VALR+45.
            if(ibopt.eq.4) VALR=VALR-15.
            if(ibopt.eq.5) VALR=VALR-30.
            if(ibopt.eq.6) VALR=VALR-45.
          endif
          CALL CNVBLK(DX,DY,DZ,objbb(1),objbb(2),objbb(3),VALR)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          MODIFYVIEW=.TRUE.; MODGEO=.TRUE.
          CALL redraw(IER)
          goto 424   ! jump back and redisplay nudgee location or rotation.
        elseif (IW.eq.4) then
          continue
        endif

C Signal that the wireframe image needs to be redrawn.
        MODIFYVIEW=.TRUE.; MODLEN=.TRUE.; MODBND=.TRUE.

C As mass surfaces would be added save the model cfd and cnn files.
        if(cfgok)then
          CALL EMKCFG('-',IER)
        endif
      ELSEIF(INO.EQ.ITEMC+17)THEN

C Toggle between air filled and water filled. Update zone hc
C file to reflect this change or create a zone hc file for water
C filled state.
        if(znotair(ICOMP))then
          CALL EASKOK(' ',
     &      'Switch from water to air filled zone?',OK,nbhelp)
          if(OK)then
            znotair(ICOMP)=.false.

C Scan any existing hc file, reset the inside coeff to -1 and write
C out the file.
            IF(IHC(ICOMP).EQ.1)THEN
              call FINDFIL(LHCCO(ICOMP),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(ICOMP),IFIL+2,IER)
                if(ier.eq.0)then

C Fill with default for inside face.
                  IHCDT=1; IHCFP(1)=1; ST(1,1)=0.0;EN(1,1)=24.0
                  DO I = 1,IHCFP(1)
                    ICTL(1,I) = 1
                    hcfpdescr(1,I) = 'Default convection coefficients'
                    do J = 1,NZSUR(icomp)
                      HCI(1,I,J) = -1.0
                      HCE(1,I,J) = -1.0
                      IHCI(1,I,J) = -1
                      IHCE(1,I,J) = -1
                      do jj=1,8
                        CVdata(1,I,J,jj) = 0.
                      enddo  ! of jj
                    enddo    ! of j
                  enddo      ! of i
                  CALL EMKHTC(LHCCO(ICOMP),ICOMP,IFIL+2,ITRU,IER)
                  call usrmsg('Updated zone hc file with default hc',
     &              'coefficients at inside and outside face.','W')
                else
                  call usrmsg('Problem reading conv regime for zone.',
     &                        'returning with no action taken.','W')
                  ier=0
                  goto 3
                endif
              else
                call usrmsg('Could not find conv regime for zone.',
     &                      'returning with no action taken.','W')
                ier=0
                goto 3
              endif
            endif
            if(cfgok)then
              CALL EMKCFG('-',IER)
            endif
          endif
        else
          CALL EASKOK(' ',
     &      'Switch from air to water filled zone?',OK,nbhelp)
          if(OK)then
            znotair(ICOMP)=.true.

C Scan any existing hc file, reset the inside coeff to 999 and write
C out the file.
            IF(IHC(ICOMP).EQ.1)THEN
              call FINDFIL(LHCCO(ICOMP),XST)
              IF(XST)THEN
                call ehtcff(LHCCO(ICOMP),IFIL+2,IER)
                if(ier.eq.0)then

C Fill with high hc for inside face.
                  IHCDT=1; IHCFP(1)=1; ST(1,1)=0.0;EN(1,1)=24.0
                  DO I = 1,IHCFP(1)
                   ICTL(1,I) = 1
                   hcfpdescr(1,I)='Water filled convection coefficients'
                   do J = 1,NZSUR(icomp)
                     HCI(1,I,J) = 999.0
                     HCE(1,I,J) = -1.0
                     IHCI(1,I,J) = -1
                     IHCE(1,I,J) = -1
                     do jj=1,8
                       CVdata(1,I,J,jj) = 0.
                     enddo  ! of jj
                   enddo    ! of j
                  enddo      ! of i
                  CALL EMKHTC(LHCCO(ICOMP),ICOMP,IFIL+2,ITRU,IER)
                  call usrmsg('Updated zone hc file with water hc',
     &              'coefficients at inside face.','W')
                else
                  call usrmsg('Problem reading conv regime for zone.',
     &                        'returning with no action taken.','W')
                  ier=0
                  goto 3
                endif
              else
                call usrmsg('Could not find conv regime for zone.',
     &                      'returning with no action taken.','W')
                ier=0
                goto 3
              endif
            else

C Create a hc file for this zone.
              IHC(ICOMP)=1
              IHCFP(1)=1
              if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
                WRITE(HFILE,'(2A)')zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
              else
                WRITE(HFILE,'(4A)') zonepth(1:lnblnk(zonepth)),'/',
     &          zname(ICOMP)(1:lnzname(ICOMP)),'.htc'
              endif

              LHCCO(ICOMP)=HFILE
              call usrmsg('Creating hc file for water filled.',HFILE,
     &          'P')

C Fill with high hc for inside face.
              DO I = 1,IHCFP(1)
                ICTL(1,I) = 1
                hcfpdescr(1,I)= 'Water filled convection coefficients'
                do J = 1,NZSUR(icomp)
                  HCI(1,I,J) = 999.0
                  HCE(1,I,J) = -1.0
                  IHCI(1,I,J) = -1
                  IHCE(1,I,J) = -1
                  do jj=1,8
                    CVdata(1,I,J,jj) = 0.
                  enddo  ! of jj
                enddo    ! of j
              enddo      ! of i
              CALL EMKHTC(LHCCO(ICOMP),ICOMP,IFIL+2,ITRU,IER)
              call usrmsg('Created zone hc file with water hc',
     &              'coefficients at inside face.','W')
            endif
            if(cfgok)then
              CALL EMKCFG('-',IER)
            endif
          endif
        endif

      ELSEIF(INO.EQ.ITEMC+19)THEN

C Surface summary: print header, followed by surface information.
C And also report other zone summary fields as in the QA report.
C Switch to fixed width font for the report.
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(ITFS.eq.4) ITFS=0
        if(ITFS.eq.5) ITFS=1
        if(ITFS.eq.6) ITFS=2
        if(ITFS.eq.7) ITFS=3
        call userfonts(IFS,ITFS,IMFS)

        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        exposed = 0.0; vexposed = 0.0
        areatran = 0.0; areawall = 0.0; areaslproof = 0.0
        areafltroof = 0.0; areaskylt = 0.0
        uavgtran = 0.0; uavgsky = 0.0; uavwall = 0.0
        uavfltroof = 0.0; uavslproof = 0.0; wallper =0.0

        call tstamp('>','PRJ: list zone surface summary')
        context=.true.
        CALL SURINFO(ICOMP,iuout,context)

C Display obstructions if newgeom is true. Write out report
C format for each type of obstruction.
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          if(iobs(icomp).eq.2)then
            if(nbobs(icomp).gt.0)then
              call edisp(iuout,' ')
              call edisp(iuout,'Details of obstruction blocks:')
              write(outs,'(a,i3,a,i3,a)')'Shading based on grids of ',
     &          NOX(icomp),' by ',NOZ(icomp),' for surfaces.'
              call edisp(iuout,outs)
              write(outs,'(2a)')
     &        'Block X-coord Y-coord Z-coord DX VAL. DY VAL. DZ VAL. ',
     &        'Orientation Name Material'
              call edisp(iuout,outs)
              DO 9995 I=1,nbobs(icomp)
                lnbn=LNBLOCKNAME(icomp,I)
                lnbm=LNBLOCKMAT(icomp,I)
                if(BLOCKTYP(icomp,I)(1:4).eq.'obs ')then
                  WRITE(outs,9994)I,XOB(icomp,I),YOB(icomp,I),
     &              ZOB(icomp,I),DXOB(icomp,I),DYOB(icomp,I),
     &              DZOB(icomp,I),BANGOB(icomp,I,1),OPOB(icomp,I),
     &              BLOCKNAME(icomp,I)(1:lnbn),BLOCKMAT(icomp,I)(1:lnbm)
 9994             FORMAT(I3,6F8.2,2F7.2,' ',a,' ',a)
                  call edisp(iuout,outs)
                elseif(BLOCKTYP(icomp,I)(1:4).eq.'obs3')then
                  WRITE(outs,9993)I,XOB(icomp,I),YOB(icomp,I),
     &              ZOB(icomp,I),DXOB(icomp,I),DYOB(icomp,I),
     &              DZOB(icomp,I),BANGOB(icomp,I,1),BANGOB(icomp,I,2),
     &              BANGOB(icomp,I,3),OPOB(icomp,I),
     &              BLOCKNAME(icomp,I)(1:lnbn),BLOCKMAT(icomp,I)(1:lnbm)
 9993             FORMAT(I3,6F8.2,4F7.2,' ',a,' ',a)
                  call edisp(iuout,outs)
                elseif(BLOCKTYP(icomp,I)(1:4).eq.'obsp')then
                  WRITE(outs,'(i3,5a)')I,' ',BLOCKNAME(icomp,I)(1:lnbn),
     &              ' ',BLOCKMAT(icomp,I)(1:lnbm),
     &              ' is a 6 sided polygon obstruction'
                  call edisp(iuout,outs)
                endif
 9995         CONTINUE
              call edisp(iuout,' ')
            endif
          endif
        endif

        bndry=.true.
        DO 1243 IS=1,NSUR
         icc=IZSTOCN(icomp,is)
         if(ICT(icc).eq.0.and.SOTHER(icomp,is,1)(1:5).ne.'EXTER')
     &     bndry=.false.
         if(ICT(icc).eq.1.and.SOTHER(icomp,is,1)(1:5).ne.'SIMIL')
     &     bndry=.false.
         if(ICT(icc).eq.2.and.SOTHER(icomp,is,1)(1:5).ne.'CONST')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SOTHER(icomp,is,1)(1:5).eq.'CONST')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SOTHER(icomp,is,1)(1:5).eq.'SIMIL')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SOTHER(icomp,is,1)(1:5).eq.'EXTER')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SOTHER(icomp,is,1)(1:5).eq.'GROUN')
     &     bndry=.false.
         if(ICT(icc).eq.4.and.SOTHER(icomp,is,1)(1:5).ne.'GROUN')
     &     bndry=.false.
         if(ICT(icc).eq.5.and.SOTHER(icomp,is,1)(1:5).ne.'ADIAB')
     &     bndry=.false.
         if(ICT(icc).eq.6.and.SOTHER(icomp,is,1)(1:5).ne.'BASES')
     &     bndry=.false.

         if(ICT(icc).eq.0)then

C Check the U value for each external surface.
C exposed walls and floors, pitched roofs, flat roofs
           UVH = 0.0; UVU = 0.0; UVD = 0.0
           lnssmlc=lnblnk(SMLCN(icomp,is))
           do 511 ii=1,nmlc
             if(SMLCN(icomp,is)(1:lnssmlc).eq.
     &          mlcname(ii)(1:lnmlcname(ii)))then

C Recover the ISO 6946 U values as in prjqa.F.
               call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI,UVG)
             else
               continue
             endif
  511      continue
           exposed = exposed + SNA(icomp,is)
           if(SOTF(icomp,is)(1:4).eq.'OPAQ')then
             if(SVFC(icomp,is)(1:4).eq.'VERT')then

C For vertical walls assume horizontal hc coef.
               areawall = areawall + SNA(icomp,is)
               uavwall = uavwall + (SNA(icomp,is) * UVH)
               vexposed = vexposed + SNA(icomp,is)
             elseif(SVFC(icomp,is)(1:4).eq.'SLOP')then

C For sloped surfaces.
               areaslproof = areaslproof + SNA(icomp,is)
               uavslproof = uavslproof + (SNA(icomp,is) * UVU)
             elseif(SVFC(icomp,is)(1:4).eq.'CEIL')then

C For ceilings.
               areafltroof = areafltroof + SNA(icomp,is)
               uavfltroof = uavfltroof + (SNA(icomp,i) * UVU)
             else

C For floors.
               areawall = areawall + SNA(icomp,is)
               uavwall = uavwall + (SNA(icomp,is) * UVD)
             endif

C At this point CFC and CFC2 are treated with opaque surfaces.
           elseif(SOTF(icomp,is)(1:4).ne.'OPAQ'.and.
     &            SOTF(icomp,is)(1:4).ne.'CFC '.and.
     &            SOTF(icomp,is)(1:4).ne.'CFC2')then
             if(SVFC(icomp,is)(1:4).eq.'CEIL'.or.
     &          SVFC(icomp,is)(1:4).eq.'SLOP')then

C Consider glazing on ceiling or sloped to be a skylight.
               areaskylt = areaskylt + SNA(icomp,is)
               uavgsky = uavgsky + (SNA(icomp,is) * UVU)
             elseif(SVFC(icomp,is)(1:4).eq.'VERT')then

C Consider glazing on walls to be a part of facade.
               areatran = areatran + SNA(icomp,is)
               uavgtran = uavgtran + (SNA(icomp,is) * UVH)
               vexposed = vexposed + SNA(icomp,is)
             else
               areatran = areatran + SNA(icomp,is)
               uavgtran = uavgtran + (SNA(icomp,is) * UVH)
               vexposed = vexposed + SNA(icomp,is)
             endif
           else
             continue
           endif
          endif
 1243   continue
        if(.NOT.bndry)then
          call usrmsg(
     &   'Some surface boundary attributions did not match the master',
     &   'connections list. A topology update may be required.','W')
        endif

        if(exposed.gt.0.1)then

C If there is external glazing or skylights then report.
          call rel16str(exposed,t16a,lna,ier)
          call rel16str(vexposed,t16b,lnb,ier)
          if(vexposed.gt.0.1)then
            write(outs,'(5a)')'There is ',t16a(1:lna),
     &        'm^2 of exposed surface area, ',
     &        t16b(1:lnb),'m^2 of which is vertical.'
          else
            write(outs,'(3a)')'There is ',
     &        t16a(1:lna),'m^2 of exposed surface area.'
          endif
          call edisp(iuout,outs)
        endif
        if(areawall.gt.0.1)then
          if(ZBASEA(icomp).gt.0.01)then
            wallper = (areawall/ZBASEA(icomp)) * 100.
          else
            wallper = 1.0
          endif
          call rel16str(wallper,t16a,lna,ier)
          call rel16str(uavwall,t16b,lnb,ier)
          write(outs,'(3a,F3.1,3a)')'Outside walls are ',
     &      t16a(1:lna),'% of floor area, with an average U-value of ',
     &      uavwall/areawall,' and UA value of ',t16b(1:lnb),'.'
          call edisp(iuout,outs)
        endif
        if(areaslproof.gt.0.1)then
          if(ZBASEA(icomp).gt.0.01)then
            slproofper = (areaslproof/ZBASEA(icomp))*100.
          else
            slproofper = 1.0
          endif
          call rel16str(slproofper,t16a,lna,ier)
          call rel16str(uavslproof,t16b,lnb,ier)
          write(outs,'(3a,F3.1,3a)')'Sloped roof is ',
     &      t16a(1:lna),'% of floor area, with an average U-value of ',
     &      uavslproof/areaslproof,' and UA value of ',
     &      t16b(1:lnb),'.'
          call edisp(iuout,outs)
        endif
        if(areafltroof.gt.0.1)then
          if(ZBASEA(icomp).gt.0.01)then
            flatroofper = (areafltroof/ZBASEA(icomp))*100.
          else
            flatroofper = 1.0
          endif
          call rel16str(flatroofper,t16a,lna,ier)
          call rel16str(uavfltroof,t16b,lnb,ier)
          write(outs,'(3a,F3.1,3a)')'Flat roof is ',
     &      t16a(1:lna),'% of floor area, with an average U-value of ',
     &      uavfltroof/areafltroof,' and UA value of ',
     &      t16b(1:lnb),'.'
          call edisp(iuout,outs)
        endif
        if(areatran.gt.0.1)then
          if(ZBASEA(icomp).gt.0.01)then
            tranper = (areatran/ZBASEA(icomp)) * 100.
          else
            tranper = 1.0
          endif
          vtranper = (areatran/vexposed) * 100.
          call rel16str(tranper,t16a,lna,ier)
          call rel16str(vtranper,t16b,lnb,ier)
          call rel16str(uavgtran,t16c,lnc,ier)
          write(outs,'(5a,F3.1,3a)')
     &      'Glazing is ',t16a(1:lna),'% of floor and ',t16b(1:lnb),
     &      '% of facade, with an average U-value of ',
     &      uavgtran/areatran,' and UA value of ',t16c(1:lnc),'.'
          call edisp(iuout,outs)
        endif
        if(areaskylt.gt.0.1)then
          if(ZBASEA(icomp).gt.0.01)then
            skyper = (areaskylt/ZBASEA(icomp)) * 100.
          else
            skyper = 1.0
          endif
          call rel16str(skyper,t16a,lna,ier)
          call rel16str(uavgsky,t16b,lnb,ier)
          write(outs,'(3a,F3.1,3a)')'Skylights are ',
     &      t16a(1:lna),'% of floor area, with an average U-value of ',
     &      uavgsky/areaskylt,' and UA value of ',t16b(1:lnb),'.'
          call edisp(iuout,outs)
        endif

C Check if user wishes to see construction details as well.
        if(LTHRM(ICOMP)(1:7).eq.'UNKNOWN'.or.
     &     LTHRM(ICOMP)(1:2).eq.'  ')then
          continue
        else
          XST=.FALSE.
          call FINDFIL(LTHRM(ICOMP),XST)
          if(XST)then
            call easkok(' ','View thermophysical properties?',
     &               ok,nbhelp)
            if(ok)then
              CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)
              CALL CONINF(ICOMP,0,iuout)
            endif
          endif
        endif
        call pauses(1)
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        call usrmsg(' ',' ','-') ! clear dialog box

      ELSEIF(INO.EQ.ITEMC+20)THEN

C Ask for name of file to put updated information into.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call tstamp('>','PRJ: update zone geometry')
        LTMP=LGEOM(ICOMP)
        CALL EASKS(LTMP,' ','Zone geometry file?',
     &                  72,GFILE,'geom file',IER,nbhelp)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call geowrite(IFIL+2,LTMP,ICOMP,iuout,4,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ','Problem creating file!','W')
          INO=-4
          GOTO 3
        ENDIF
        LGEOM(ICOMP)=LTMP
        CALL EMKCFG('-',IER)
        MODGEO=.FALSE.

      ELSEIF(INO.EQ.ITEMC+21)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0

      ELSEIF(INO.EQ.ITEMC+22)THEN

C Jump to previous zone after processing any remaining tasks.
        ianother=-1
        if(MODGEO)then
          CALL EASKOK(' ',
     &     'Save changes to zone composition and surface attributes?',
     &      OK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
          endif

C If there is an associated flow node then update its volume and ??
C Update the flow network file(s).
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            nindex=ICAAS(ICOMP)
            SUPNOD(nindex,2)=VOL(ICOMP)
            HNOD(nindex,1)=ZCOG(ICOMP,1); HNOD(nindex,2)=ZCOG(ICOMP,2)
            HNOD(nindex,3)=ZCOG(ICOMP,3)
            call updatebothflownetworks(ier)
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        return

      ELSEIF(INO.EQ.ITEMC+23)THEN

C Jump to next zone after processing any remaining tasks.
        ianother=1
        if(MODGEO)then
          CALL EASKOK(' ',
     &     'Save changes to zone composition and surface attributes?',
     &     OK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
          endif

C If there is an associated flow node then update its volume and ??
C Update the flow network file(s).
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            nindex=ICAAS(ICOMP)
            SUPNOD(nindex,2)=VOL(ICOMP)
            HNOD(nindex,1)=ZCOG(ICOMP,1); HNOD(nindex,2)=ZCOG(ICOMP,2)
            HNOD(nindex,3)=ZCOG(ICOMP,3)
            call updatebothflownetworks(ier)
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        return

      ELSEIF(INO.EQ.ITEMC+24)THEN

C Produce help text for the menu.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('geometry section',nbhelp,'-',0,0,IER)
      ELSE
        INO=-4
        GOTO 3
      ENDIF
      INO=-4
      GOTO 3

      END

C ************* EDINSUL
C Edit zone insolation distribution.
      SUBROUTINE EDINSUL(ICOMP,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C iaplic(1) toggle for shading; iaplic(2) toggle for insolation.
C   toggle for assessment where:
C   1 is 'all_applicable', 0 is manual selection of surfaces.
C nsurfcalc nb of shaded surfaces, lstsfcalc() list of applicable surfaces.
C nsurfinso nb of insolation sources, isurfinso() list of insolation sources.
      common/ishdirec/iaplic(MCOM,2),nsurfcalc(MCOM),lstsfcalc(MCOM,MS),
     &     nsurfinso(MCOM),isurfinso(MCOM,MS)

      common/shad0/ISIcalc,icalcD,icalcM
      integer ISIcalc,icalcD,icalcM

      common/pmchange/comold,tmcold,vwfold,ishold,cfcold

      DIMENSION IALT(8),IVAL(8),IVA(MS)
      dimension isd(16)
      CHARACTER IALT*36
      CHARACTER isd*33,SFIL*72,DSFIL*72
      character ZN*12
      LOGICAL comold,tmcold,vwfold,ishold,cfcold
      logical changed,newgeo
      logical unixok
      integer ISDN,INODA ! max items and current menu item

      helpinsub='edgeo'  ! set for subroutine

      IER=0
      changed=.false. ! user has not altered any directives yet
      newgeo=.false.  ! assume older format geometry.

      call isunix(unixok)

C << Note: if surfaces or added or deleted or changed from opaque to
C << transparent then the ish directives will need to be updated.

C << Note: after obstructions defined this routine needs to be called
C << to define directives - also code probably needs a way not to
C << clear all of the arrays.

C If there are no obstructions warn the user and reset nsurfcalc().
      if(IOBS(icomp).eq.0)then
        nsurfcalc(icomp)=0
        call usrmsg(
     &   'Without shading obstructions only insolation',
     &   'can be calculated.','W')
      endif

      call eclose(gversion(icomp),1.1,0.01,newgeo)

   13 zn=zname(icomp)
      WRITE(ISD(1),'(A,A12)')   '  zone: ',zn
      ISD(2)=                   'a specified insolation to:      '
      ioc1=0; ioc2=0
      if(IDPN(ICOMP,1).gt.0)then
        ioc1=IZSTOCN(icomp,IDPN(ICOMP,1))
      endif
      if(IDPN(ICOMP,2).gt.0)then
        ioc2=IZSTOCN(icomp,IDPN(ICOMP,2))
      endif

      if(NDP(ICOMP).eq.1)then
        if(ioc1.gt.0)then
          write(isd(3),'(4x,a)') sname(icomp,IDPN(ICOMP,1))
        else
          write(isd(3),'(4x,a)') 'unknown surface'
        endif
        write(isd(4),'(a)')     '                                '
      elseif(NDP(ICOMP).eq.2)then
        if(ioc1.gt.0)then
          write(isd(3),'(4x,a)') sname(icomp,IDPN(ICOMP,1))
        else
          write(isd(3),'(4x,a)') 'unknown surface'
        endif
        if(ioc2.gt.0)then
          write(isd(4),'(4x,a)') sname(icomp,IDPN(ICOMP,2))
        else
          write(isd(4),'(4x,a)') 'unknown surface'
        endif
      elseif(NDP(ICOMP).eq.3)then
        write(isd(3),'(a)')     '    diffuse insolation distrib  '
        write(isd(4),'(a)')     '                                '
      endif
      write(isd(5),'(a)')       '  _____________________________ '
      write(isd(6),'(a)')       'b calculated shading:           '
      if(nsurfcalc(icomp).gt.0)then
        if(iaplic(icomp,1).eq.0)then
            write(isd(7),'(a)') '   user defined list            '
        else
            write(isd(7),'(a)') '   all applicable surfaces      '
        endif
      else
        if(IOBS(icomp).eq.0)then
          write(isd(7),'(a)')   '  not applicable for this zone  '
        elseif(IOBS(icomp).eq.2)then
          write(isd(7),'(a)')   '  not requested for this zone   '
        else
          write(isd(7),'(a)')   '  not requested for this zone   '
        endif
      endif
      write(isd(8),'(a)')       'c calculated insolation:        '
      if(nsurfinso(icomp).gt.0)then
        if(iaplic(icomp,2).eq.0)then
            write(isd(9),'(a)') '   user defined list            '
        else
            write(isd(9),'(a)') '   all applicable surfaces      '
        endif
      else
        write(isd(9),'(a)')     '   not requested for this zone  '
      endif
      ISD(10)=                  '                                '
      ISD(11)=                  '                                '
      ISD(12)=                  'e invoke shade/insol analysis   '
      ISD(13)=                  '  _____________________________ '
      ISD(14)=                  '! list details                  '
      ISD(15)=                  '? help                          '
      ISD(16)=                  '- exit menu                     '
      ISDN=16
      INODA=-4

      helptopic='insolation_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      CALL USRMSG(' ',' ','-')
      CALL EMENU(' Zone shading and insolation',ISD,ISDN,INODA)
      if(INODA.EQ.ISDN)then

C If any changes update and then save geometry file prior to exiting.
        if(changed)then
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          changed=.false.
        endif
        return
      elseif(INODA.eq.ISDN-1)then

C Insolation, present a list of current default insolation choices.
        helptopic='insolation_choices'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('shad:insol',nbhelp,'-',0,0,IER)
      elseif(INODA.eq.ISDN-2)then
        CALL INSINFO(icomp,iuout)
      elseif(INODA.eq.ISDN-4)then

C If any changes update and then save geometry file before invoking ish.
        if(changed)then
          call usrmsg('Updating zone for changed directives.',' ','P')
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          changed=.false.
        endif

C Allow user to start shading analysis. If user does not cancel suggest
C a new file name, ask the user to confirm that name and update the model
C cfg file and then if the user says do it now call comission ish.
        CALL EASKAB(' ','Shading & insolation analysis options:',
     &   'do now','cancel',IW,nbhelp)

C If ISIcalc has not yet been set assign = 2 for monthly file.
        if(IW.eq.1)then
          if(ISIcalc.eq.0) ISIcalc = 2
          if(ISI(ICOMP).eq.0)then
            if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
              WRITE(SFIL,'(A,A4)')ZN(1:lnblnk(ZN)),'.shd'
            else
              WRITE(SFIL,'(A,A,A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &          ZN(1:lnblnk(ZN)),'.shd'
            endif
            DSFIL = 'new.shd'
            CALL EASKS(SFIL,' New zone shading/ insulation database?',
     &        ' ',72,DSFIL,'shd/ins db',IER,nbhelp)
            if(SFIL(1:2).ne.'  ')then
              LSHAD(ICOMP)=SFIL
              ISI(ICOMP)=1
              call tstamp('>','PRJ: adding shading to configuration')
              CALL EMKCFG('-',IER)
            endif
          endif
        else
          continue
        endif

C If existing db out of date the perform recalculation.
        if(IW.eq.1)then
          call tstamp('>','PRJ: start shading')
          if(ishold)then
            call comissionish(icomp,'sra',ier)
          else
            call comissionish(icomp,'in ',ier)
          endif
          ishold=.false.
        endif
      elseif(INODA.eq.2)then

C Set static insolation.
        IALT(1)='specified insolation to 1 surface   '
        IALT(2)='specified insolation to 2 surfaces  '
        IALT(3)='specified diffuse insol distribution'
        IX=1
        CALL EPICKS(IX,IVAL,' static insolation options:','  ',
     &    36,3,IALT,'insolation options',IER,nbhelp)
        IF(IX.EQ.0) goto 13
        IF(IVAL(1).EQ.1)THEN

C Insolation to a specific surface.
          IS=1
          CALL EASKSUR(ICOMP,IS,'A','Select surface for insolation.',
     &      ' ',IER)
          NDP(ICOMP)=1
          IDPN(ICOMP,1)=IS; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=IS
          changed=.true.
        ELSEIF(IVAL(1).EQ.2)THEN

C Insolation to two user selected surfaces.
          IS=1
          CALL EASKSUR(ICOMP,IS,'A','Select first insolated surface.',
     &      ' ',IER)
          NDP(ICOMP)=2
          IDPN(ICOMP,1)=IS
          IS=1
          CALL EASKSUR(ICOMP,IS,'A','Select 2nd insolated surface.',
     &      ' ',IER)
          IDPN(ICOMP,2)=IS; IDPN(ICOMP,3)=-1
          CALL USRMSG(' ',' ','-')
          changed=.true.
        ELSEIF(IVAL(1).EQ.3)THEN

C Diffuse distribution.
          NDP(ICOMP)=3
          IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0
          changed=.true.
        endif
      elseif(INODA.eq.6)then

C Set shading directives.
        if(IOBS(icomp).eq.0)then
          call usrmsg('Only insolation can be calculated because there',
     &      'are no shading obstructions associated with the zone.','W')
          goto 13
        endif

C Clear list arrays for surfaces to include in shading analysis.
        do 14 i=1,NZSUR(icomp)
          IVA(I)=0
          lstsfcalc(icomp,i)=0
  14    continue
        NP=0
        ilimit=NZSUR(icomp)
        CALL PICKSSUR(ICOMP,NP,'s',IVA,iap,ilimit,IER)
        if(ier.ne.0) goto 13
        NDP(ICOMP)=3
        IDPN(ICOMP,1)=0; IDPN(ICOMP,2)=0; IDPN(ICOMP,3)=0

        iaplic(icomp,1)=iap

C Set up lstsfcalc list (surfaces included in the shading analysis).
        nsurfcalc(icomp)=NP
        if(NP.gt.0)then
          DO 110 i=1,NP
            lstsfcalc(icomp,i)=IVA(i)
  110     CONTINUE
        endif
        changed=.true.
      elseif(INODA.eq.8)then

C Clear return array and select all applicable surfaces for insolation.
        do 15 i=1,NZSUR(icomp)
          IVA(I)=0
          isurfinso(icomp,i)=0
  15    continue
        ntmc=0
        ilimit=NZSUR(icomp)
        CALL PICKSSUR(ICOMP,ntmc,'i',IVA,iap,ilimit,IER)
        if(ier.ne.0) goto 13

C Set up nsurfinso isurfinso list (insolation sources).
        iaplic(icomp,2)=iap
        nsurfinso(icomp)=ntmc
        if(ntmc.gt.0)then
          DO 111 i=1,ntmc
            isurfinso(icomp,i)=IVA(i)
  111     CONTINUE
        endif
        changed=.true.
      elseif(INODA.eq.10)then
        continue
      else
        INODA=-4
        GOTO 13
      endif
      INODA=-4
      GOTO 13

      END

C ******************** PICKSSUR
C PICKSSUR selects surfaces for shading & insolation analysis.
C IZONE is the focus zone, NP is the number of surfaces selected,
C act = 's or S' shading, act = 'i or I' insolation
C act = 'c or C' for generating a list of surfaces to copy up to
C        the limit of ilimit surfaces.
C act = '-' returning a list of surfaces up to nzsur().
C iaplic toggle for shading or insolation where:
C   if = 1 then 'all_applicable', if = 0 then manual selection of surfaces.
C IVA array of selections is returned.

      SUBROUTINE PICKSSUR(IZONE,NP,act,IVA,iaplic,ilimit,IER)
#include "building.h"
#include "geometry.h"
#include "help.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      DIMENSION STMP(MS),IVA(MS),IVALS(MS)
      character STMP*46,act*1,msg1*32,msg2*32

      helpinsub='edgeo'  ! set for subroutine

      IER=0
      IF(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
        CALL USRMSG(' ','Zone number out of range!','W')
        IER=1
        RETURN
      ENDIF

C Debug.
C      write(6,*) 'pickssur ',izone,NP,act,IVA,iaplic,ilimit,IER

C Help messages.
      helptopic='picksurface_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)

 43   if(act.eq.'s'.or.act.eq.'S')then
        CALL EASKABCD(' ','Shading calculation options:',
     &     'all applicable surfaces','selected surfaces',
     &     'none','cancel',IIC,nbhelp)
      elseif(act.eq.'i'.or.act.eq.'I')then
         CALL EASKABCD(' ','Insolation calculation options:',
     &      'all applicable surfaces','selected surfaces',
     &      'none','cancel',IIC,nbhelp)
      else
        IIC=2
      endif

C Clear return array.
      do 14 i=1,MS
        IVA(I)=0
        stmp(i)=' '
  14  continue

C Process initial selection.
      if(IIC.eq.3)then
        NP=0
        return
      elseif(IIC.eq.4)then
        return
      elseif(IIC.eq.1)then
        if(act.eq.'s'.or.act.eq.'S')then
          NP=0
          DO I=1,NZSUR(IZONE)
            if(SOTHER(izone,i,1)(1:5).eq.'EXTER')then
              NP=NP+1
              IVA(NP)=I
            endif
          ENDDO  ! pf I
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found.',' ','W')
            goto 43
          endif
          iaplic=1
          return
        elseif(act.eq.'i'.or.act.eq.'I')then
          NP=0
          DO I=1,NZSUR(IZONE)
            if(SOTHER(izone,i,1)(1:5).eq.'EXTER'.and.
     &         SOTF(izone,i)(1:4).ne.'OPAQ')then
              NP=NP+1
              IVA(NP)=I
            endif
          ENDDO ! of I
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found.',' ','W')
            goto 43
          endif
          iaplic=1
          return
        endif
      elseif(IIC.eq.2)then

C If copy action set inpic to ilimit otherwise set to nzsur().
        if(act.eq.'c'.or.act.eq.'C')then
          inpic=min0(ilimit,nzsur(izone))
        else
          INPIC=NZSUR(IZONE)
        endif

C Loop through each surface in the zone and make up display list based on the
C attributes of the surface. For shading or insulation mark surfaces
C which are not applicable. The do loop needs to use nzsur so that all
C surfaces are presented even if only ilimit can be copied.
        DO I=1,NZSUR(IZONE)
         lnl=lnblnk(SMLCN(izone,i))
         if(lnl.gt.16) lnl=16  ! truncate MLC name in list
         if(act.eq.'s'.or.act.eq.'S')then
           if(SOTHER(izone,i,1)(1:5).eq.'EXTER')then
             write(STMP(I),'(5a)') SNAME(izone,i),'|',
     &         SOTF(izone,i)(1:6),' |',SOTHER(izone,i,1)(1:16)
           else
             write(STMP(I),'(2a)') SNAME(izone,i),' not applicable'
           endif
         elseif(act.eq.'-')then
           write(STMP(I),'(5a)') SNAME(izone,i),'|',
     &       SMLCN(izone,i)(1:lnl),
     &       '|',SOTHER(izone,i,1)(1:16)
         elseif(act.eq.'c'.or.act.eq.'C')then
           write(STMP(I),'(5a)') SNAME(izone,i),'|',
     &       SMLCN(izone,i)(1:lnl),
     &       '|',SOTHER(izone,i,1)(1:15)
         elseif(act.eq.'i'.or.act.eq.'I')then
           if(SOTHER(izone,i,1)(1:5).eq.'EXTER'.and.
     &        SOTF(izone,i)(1:4).ne.'OPAQ')then
             write(STMP(I),'(5a)') SNAME(izone,i),'|',
     &         SOTF(izone,i)(1:6),' |',SOTHER(izone,i,1)(1:16)
           else
             write(STMP(I),'(2a)') SNAME(izone,i),' not applicable'
           endif
         endif
       enddo

        if(act.eq.'c'.or.act.eq.'C')then

C Note: use display width of 41 char.
          write(msg1,'(a,i3,a)') '(up to ',ilimit,')'
          write(msg2,'(2a)') 'surfaces in ',zname(izone)
          CALL EPICKS(INPIC,IVALS,'Which surfaces?',msg1,
     &      41,NZSUR(IZONE),STMP,msg2,IER,nbhelp)
        else
          write(msg2,'(a)') ' name & type & exposure'
          CALL EPICKS(INPIC,IVALS,' ','Surfaces to Include',
     &      33,NZSUR(IZONE),STMP,msg2,IER,nbhelp)
        endif
        if(INPIC.eq.0)then
          if(act.eq.'s'.or.act.eq.'S')then
            iaplic=0
          elseif(act.eq.'i'.or.act.eq.'I')then
            iaplic=0
          endif
          RETURN
        else
          NP=INPIC
          DO I=1,NP
            IVA(I)=IVALS(I)
          ENDDO  ! of I
          if(act.eq.'s'.or.act.eq.'S')then
            iaplic=0
          elseif(act.eq.'i'.or.act.eq.'I')then
            iaplic=0
         endif
        endif

C Debug...
C        write(6,*) 'act np iva ',act,np,iva

      endif
      RETURN
      END


C ******************** EDSURA ********************
C Edit zone surface attributes in common block G5 and allow this to be
C saved to a geometry file ITRU unit number for user output, IER=0 OK,
C IER=1 problem. Make use of construction information in common MLC.
C ianother is returned as +1 if jump to next surface, -1 if jump to
C prior surface, otherwise zero.

C << add in additional menu line for external viewfactors >>
C << add in additional menu line for external diffuse shading >>
C << add in additional menu line for external hc correlation >>

C << it would clarify the surface if it was reported which convection >>
C << correlation is being used for inside face. And should it be >>
C << possible to alter the inside/outside hc from this interface? >>

      SUBROUTINE EDSURA(ICOMP,ISS,ITRC,MODGEO,ianother,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "site.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

      integer lnblnk  ! function definition

C Passed parameters.
      integer icomp   ! zone index
      integer iss     ! surface index
      integer itrc    ! reporting unit
      logical modgeo
      integer ianother ! set if user wants to jump to next/previous
      integer ier     ! for error state

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72

      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL nameok,showother
      logical modmlc             ! for selecting MLC

      logical dupedges,XST,OK,updoth
      logical newgeo             ! to use for testing if new/old geometry file.
      logical goforit            ! create a default flow component

      DIMENSION SALT(14),IVAL(MCOM)  ! ,bl(144)
      DIMENSION COG1(3),COG2(3)
      DIMENSION CG(3)            ! for warp checks
      DIMENSION VN(3),ipoints(6,2)
      dimension sperim(MS)       ! perimeter for each surface in zone
      integer icontinue          ! to control while loop
      CHARACTER*33 ISD(28)
      CHARACTER DESCRC*25,outs*124
      CHARACTER SALT*33,SN*12,SN2*12,T14*14
      character ozn*12           ! the name of the other zone
      character guesstype*24     ! pass back context of surface
      character TOSMLCN*32       ! to remember other side mlc name
      character TOOPT*32         ! to remember other side optics
      character TOUSE1*12,TOUSE2*12  ! to remember other side use

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*12,rsuse2*12
      character rsparent*12
      real XO1,ZO1               ! to prevent name clash in geometry.h
      integer ISDN,INODA         ! max items and current menu item
      integer mnulen,iwhich,iwhich2,numv   ! multi-column lines
      logical greyok,odd         ! for display of vertex info
      logical found_association  ! true if flow network has item with this surface name
      logical updatenetwork      ! if true then update the flow network files.
      integer ii,iloop           ! for loop
      integer issize
      character message*48,t248*496,stemp*13

      integer iissmlci           ! other side construction index

#ifdef OSI
      integer iix,iiy,iicol,iixc,iiyc,iid1,iid2,iid3,iid4
#else
      integer*8 iix,iiy,iicol,iixc,iiyc,iid1,iid2,iid3,iid4
#endif

      helpinsub='edgeo'          ! set for subroutine
      greyok=.false.             ! see if colour available
      if(nifgrey.gt.4)then
        greyok=.true.
      endif
      issize=4                   ! for surfaces

C Display a selection of surfaces available in the zone. If surface
C faces another zone, get the other surface connection index.
      IS=ISS; icoth=0; ii=0
      found_association=.false.
      updatenetwork=.false.
      ioc=IZSTOCN(icomp,iss)
      if(ICT(ioc).eq.3)then
        showother=.true.
        icoth=IZSTOCN(IC2(ioc),IE2(ioc))
      else
        showother=.false.
        icoth=0
      endif

C Echo back to the user additional information about the surface.
C start with the vertices associated with the surface.
      write(SN,'(a12)') SNAME(ICOMP,IS)
      write(message,'(2a)') ' Vertices (X Y Z) used by ',SN
      call edisp(iuout,message)
      numv=NVER(IS)

C See if an even or odd number of items in list.
      im=MOD(numv,2)
      odd=.false.
      if(im.eq.1) odd=.true.
      if(numv.lt.8)then
        do 960 i=1,numv
          iwhich=jvn(is,i)
          write(outs,'(a,i3,3f11.5)') ' vertex ',iwhich,X(iwhich),
     &      Y(iwhich),Z(iwhich)
          call edisp(iuout,outs)
  960   continue
      else
        MNULEN=(numv/2)
        DO 193 K=1,MNULEN
          iwhich=jvn(is,k)
          iwhich2=jvn(is,k+mnulen)
          if(iwhich.ge.0.and.iwhich2.ge.0)then
            WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') ' vertex ',iwhich,
     &        X(iwhich),Y(iwhich),Z(iwhich),'   vertex ',iwhich2,
     &        X(iwhich2),Y(iwhich2),Z(iwhich2)
          else
            WRITE(outs,'(a)') 'Detected zero vertex index.'
          endif
          call edisp(iuout,outs)
  193   CONTINUE
        IF(odd)THEN
          WRITE(outs,'(a,i3,3f11.5)') ' vertex ',numv,X(numv),
     &      Y(numv),Z(numv)
          call edisp(iuout,outs)
        ENDIF
      endif

C Now tell the user the edge list for the surface. This write
C statement should be kept up to date with MV changes.
      write(SN,'(a12)') SNAME(ICOMP,IS)
      write(message,'(2a)') ' Vertices associated with ',SN
      call edisp(iuout,message)
      WRITE(t248,'(124I4)')(JVN(is,j),J=1,numv)
      call edisp248(iuout,t248,100)

C Force redraw of zone on entry to highlight focus surface.
      MODIFYVIEW=.TRUE.
      newgeo=.false.  ! assume older format geometry.
      TOSMLCN=' '; TOOPT=' '; TOUSE1=' '; TOUSE2=' '
      iissmlci=0

      call eclose(gversion(icomp),1.1,0.01,newgeo)  ! Check which version.
      call zsurfprm(icomp,is,dupedges,perim)        ! Check perimeter.

      call getperimeter(icomp,sperim)  ! recover perimeters
   13 WRITE(ISD(1),'(A,A)')    'a surface name    : ',SNAME(icomp,iss)
      if(SOTF(icomp,iss)(1:4).eq.'OPAQ'.or.
     &   SOTF(icomp,iss)(1:4).eq.'TRAN'.or.
     &   SOTF(icomp,iss)(1:4).eq.'CFC '.or.
     &   SOTF(icomp,iss)(1:4).eq.'CFC2')then
        WRITE(ISD(2),'(A,A)')  'b surface type    : ',
     &    SOTF(icomp,iss)(1:12)
      else
        WRITE(ISD(2),'(A,A)')  'b optical set name: ',
     &    SOTF(icomp,iss)(1:12)
      endif
      WRITE(ISD(3),'(A,A4)')   'c surface location: ',SVFC(icomp,iss)
      WRITE(ISD(4),'(A,f8.3)') '  surface area m^2:',SNA(icomp,iss)
      WRITE(ISD(5),'(A,2f7.2)')'  azim & elevation:',SPAZI(icomp,iss),
     &  SPELV(icomp,iss)
      WRITE(ISD(6),'(A,f8.3)') '  perimeter length:',sperim(iss)
      if(newgeo)then
        isel=smlcindex(icomp,iss)
        if(isel.gt.0)then
          WRITE(ISD(7),'(A,F6.3)') '  thermophysical thickness ',
     &      THKMLC(isel)
        else
          WRITE(ISD(7),'(A)')      '  thermophysical thickness N/A'
        endif
      else
        WRITE(ISD(7),'(A,F7.4)') 'd surface indentation:',Y0S(IS)
      endif
      lnm=lnblnk(SMLCN(icomp,iss))  ! avoid truncation if possible
      if(lnm.le.17)then
        WRITE(ISD(8),'(2A)') 'e construction: ',SMLCN(icomp,iss)(1:17)
      elseif(lnm.gt.17.and.lnm.le.26)then
        WRITE(ISD(8),'(2A)') 'e MLC: ',SMLCN(icomp,iss)(1:lnm)
      elseif(lnm.ge.26)then
        WRITE(ISD(8),'(2A)') 'e MLC: ',SMLCN(icomp,iss)(1:26)
      endif
      WRITE(ISD(9),'(A,A)')   'f environment : ',
     &  SOTHER(icomp,iss,1)(1:17)

      if(newgeo)then   ! If newgeo include a line for sparent and one for suse.

C Info about children of this surface.
        if(nbchild(ioc).gt.0)then
          if(nbchild(ioc).eq.1.and.ichild(ioc,1).gt.0)then
            iclcon=ichild(ioc,1)
            WRITE(ISD(10),'(2A)') 'g parent of   : ',
     &        SNAME(IC1(iclcon),IE1(iclcon))
          elseif(nbchild(ioc).gt.1)then
            WRITE(ISD(10),'(A,i1,A)') 'g parent of    : ',
     &        nbchild(ioc),' surfaces'
          else
            WRITE(ISD(10),'(A)')  'g parent of   : -'
          endif
        else
          WRITE(ISD(10),'(A)')    'g parent of   : -'
        endif
        if(iparent(ioc).eq.0)then
          WRITE(ISD(11),'(A)')    'g child of    : - '
        else
          WRITE(ISD(11),'(2A)')   'g child of    : ',
     &      SPARENT(icomp,iss)(1:12)
        endif
        if(SUSE(icomp,iss,1)(1:2).eq.'- '.or.
     &     SUSE(icomp,iss,1)(1:2).eq.'  ')then
          WRITE(ISD(12),'(A)')  'h use type   : ordinary surface'
          WRITE(ISD(13),'(A)')  '  use subtype: '
        else
          WRITE(ISD(12),'(2A)') 'h use type   : ',SUSE(icomp,iss,1)
          WRITE(ISD(13),'(2A)') '  use subtype: ',SUSE(icomp,iss,2)
        endif

C Report on associated boundary flow node and/ or flow component.
C If SUSE set but no matching flow node or component then offer
C to select an existing node or component and then update the
C NODASSOC & CMPASSOC and their positions.
        found_association=.false.
        call doesflowrefsurface(ICOMP,ISS,inode,icmp)
        if(inode.gt.0)then
          found_association=.true.
          WRITE(ISD(14),'(2A)')     '  @ flow node: ',NDNAM(inode)
        else  ! If partition also check the other side.
          if(ICT(ioc).eq.3)then
            call doesflowrefsurface(IC2(ioc),IE2(ioc),inode,icmp)
            if(inode.gt.0)then
              found_association=.true.
              WRITE(ISD(14),'(2A)') '  @ flow node: ',NDNAM(inode)
            else
              WRITE(ISD(14),'(A)')  '  --            '
            endif
          else
            WRITE(ISD(14),'(A)')  '  --            '
          endif
        endif
        if(icmp.gt.0)then
          found_association=.true.
          WRITE(ISD(15),'(2A)')     '  @ flow cmp ',CMNAM(icmp)
        else
          if(ICT(ioc).eq.3)then
            call doesflowrefsurface(IC2(ioc),IE2(ioc),inode,icmp)
            if(inode.gt.0)then
              found_association=.true.
              WRITE(ISD(15),'(2A)') '  @ flow cmp ',CMNAM(icmp)
            else
              WRITE(ISD(15),'(A)')  '  --            '
            endif
          else
            WRITE(ISD(15),'(A)')  '  --            '
          endif
        endif

C If flow network had an item that matches this surfaces name but the
C surface use has not been set suggest this to the user.
        if(SUSE(icomp,iss,1)(1:2).eq.'- '.or.
     &     SUSE(icomp,iss,1)(1:2).eq.'  ')then
          if(found_association)then
            call usrmsg(
     & 'Flow entities associated with this surface were found in the',
     & 'network file but the surface USE does not reflect this.','W')

C Ask user if they want to set surface USE.
            CALL EASKOK(' ','Assign a USE attribute to this surface?',
     &              OK,nbhelp)
            if(OK)then
              write(outs,'(2a)') 'Component type: ',LTPCMP(icmp)
              call edisp(iuout,outs)
              iz=icomp; isur=iss; icon=ioc
              TOUSE1=SUSE(icomp,iss,1)  ! record prior to edit
              TOUSE2=SUSE(icomp,iss,2)
              call edituse(iz,isur,icon,guesstype)
              if(TOUSE1(1:12).eq.SUSE(icomp,iss,1)(1:12).and.
     &           TOUSE2(1:12).eq.SUSE(icomp,iss,2)(1:12))then
                continue  ! no change
              else

C Update the menu to reflect the users choice.
               WRITE(ISD(12),'(2A)') 'h use type   : ',SUSE(icomp,iss,1)
               WRITE(ISD(13),'(2A)') '  use subtype: ',SUSE(icomp,iss,2)
              endif
            endif
          endif
        else

C The USE might suggest creating a link to an existing flow
C component or creating a new component.
          if(SUSE(icomp,iss,1)(1:4).eq.'DOOR'.or.
     &       SUSE(icomp,iss,1)(1:6).eq.'P-DOOR'.or.
     &       SUSE(icomp,iss,1)(1:5).eq.'FRAME'.or.
     &       SUSE(icomp,iss,1)(1:7).eq.'F-FRAME'.or.
     &       SUSE(icomp,iss,1)(1:5).eq.'GRILL'.or.
     &       SUSE(icomp,iss,1)(1:6).eq.'WINDOW'.or.
     &       SUSE(icomp,iss,1)(1:8).eq.'D-WINDOW'.or.
     &       SUSE(icomp,iss,1)(1:8).eq.'S-WINDOW'.or.
     &       SUSE(icomp,iss,1)(1:8).eq.'C-WINDOW'.or.
     &       SUSE(icomp,iss,1)(1:4).eq.'FICT')then

C If the user has selected a USE that reflects a flow node or component
C and no entity in the flow network file points to this surface then
C offer to add such a default entity to the flow network.
C If connection NOT KNOWN then do not process flow!
            ioc=IZSTOCN(icomp,iss)
            if(IAIRN.ge.3.and.ICAAS(ICOMP).ne.0.and.ioc.ne.-1)then
              if(.NOT.found_association)then
                updatenetwork=.false.
                write(outs,'(5A)') SNAME(icomp,iss),' with USE ',
     &            SUSE(icomp,iss,1),' & ',SUSE(icomp,iss,2)
                call edisp(iuout,outs)
                INVT=0
                CALL EASKABC(
     &          'The surface USE attribute suggests an associated flow',
     &          'component. Options:','pick existing flow component',
     &          'create one to match',
     &          'ignore (already defined elsewhere)',INVT,nbhelp)
                if(INVT.eq.1)then
                  call edisp(iuout,' ')
                  write(outs,'(a,i3.3,a,i3.3)') 
     &            'If standard names used look for ????z',icomp,':',iss
                  call edisp(iuout,outs)
                  call ASKRCMP('select component','-',ICOMPN,IER)
                  if(ICOMPN.gt.0)then

C Update the association and also reset the position of that
C component based on the standard USE rules.
                    CMPASSOC(ICOMPN,1)=ZNAME(ICOMP)
                    write(SN,'(a12)') SNAME(ICOMP,ISS)
                    CMPASSOC(ICOMPN,2)=SN
                    call update_cmp_pos(icomp,iss,ICOMPN)
                    updatenetwork=.true.
                    WRITE(ISD(15),'(2A)') '  @ flow cmp ',CMNAM(ICOMPN)
                  endif
                elseif(INVT.eq.2)then
                  call create_surface_node_cmp(icomp,iss)
                  updatenetwork=.true.
                elseif(INVT.eq.3)then
                  continue
                endif
              else

C There is already an association noted in the flow network
C file and we have a USE so no need to do anything.
                continue
              endif
              if(updatenetwork)then
                call updatebothflownetworks(ier)
                updatenetwork=.false.
              endif
            endif
          endif
        endif
      else
        if(nbchild(ioc).gt.0)then
          if(nbchild(ioc).eq.1.and.ichild(ioc,1).gt.0)then
            iclcon=ichild(ioc,1)
            WRITE(ISD(10),'(2A)') 'g parent of    : ',
     &        SNAME(IC1(iclcon),IE1(iclcon))
          elseif(nbchild(ioc).gt.1)then
            WRITE(ISD(10),'(A,i1,A)') 'g parent of    : ',
     &        nbchild(ioc),' surfaces'
          else
            WRITE(ISD(10),'(A)')  'g parent of    : -'
          endif
        else
          WRITE(ISD(10),'(A)')  'g parent of    : -'
        endif
        WRITE(ISD(11),'(2A)')   '  child of     : ',
     &    SPARENT(icomp,iss)(1:12)
        WRITE(ISD(12),'(A)')    '  use type   : (NA)'
        WRITE(ISD(13),'(A)')    '  use subtype: (NA)'
        WRITE(ISD(14),'(A)')    '  flow node: (NA)      '
        WRITE(ISD(15),'(A)')    '  flow component: (NA) '
        found_association=.false.
      endif

C << report inside and outside hc regime here >>

      ISD(16)        ='  ___ @ other face____________ '
      if(showother)then
        if(IC2(ioc).ne.0.and.IE2(ioc).ne.0)then
          icoth=IZSTOCN(IC2(ioc),IE2(ioc))
          WRITE(ISD(17),'(2A)')'   surface name: ',
     &      SNAME(IC2(ioc),IE2(ioc))
          write(ozn,'(a)') zname(IC2(ioc))
          WRITE(ISD(18),'(2A)') '   surface type: ',
     &      SOTF(IC2(ioc),IE2(ioc))(1:12)
          WRITE(ISD(19),'(2A)') '   location    : ',
     &      SVFC(IC2(ioc),IE2(ioc))
          lnm=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))  ! avoid truncation if possible
          if(lnm.le.16)then
            WRITE(ISD(20),'(2A)') '   construction: ',
     &        SMLCN(IC2(ioc),IE2(ioc))(1:16)
          elseif(lnm.gt.16.and.lnm.le.25)then
            WRITE(ISD(20),'(2A)') '   MLC: ',
     &        SMLCN(IC2(ioc),IE2(ioc))(1:lnm)
          elseif(lnm.ge.25)then
            WRITE(ISD(20),'(2A)') '   MLC: ',
     &        SMLCN(IC2(ioc),IE2(ioc))(1:25)
          endif
          WRITE(ISD(21),'(2A)') '   in zone     : ',ozn(1:lnblnk(ozn))
          if(newgeo)then
           WRITE(ISD(22),'(2A)')'   use type    : ',
     &       SUSE(IC2(ioc),IE2(ioc),1)
          else
           WRITE(ISD(22),'(A)') '   use type    : Not applicable'
          endif
        else
          WRITE(ISD(17),'(A)') '   surface name: IS NOT KNOWN'
          write(ozn,'(a)') 'IS NOT KNOWN'
          WRITE(ISD(18),'(A)') '   surface type: IS NOT KNOWN'
          WRITE(ISD(19),'(A)') '   location    : IS NOT KNOWN'
          WRITE(ISD(20),'(A)') '   construction: IS NOT KNOWN'
          WRITE(ISD(21),'(A)') '   in zone     : IS NOT KNOWN'
          if(newgeo)then
           WRITE(ISD(22),'(2A)') '    use type   : ',
     &       SUSE(IC2(ioc),IE2(ioc),1)
          else
           WRITE(ISD(22),'(A)')  '    use type   : IS NOT KNOWN'
          endif
        endif

C << Report other faces inside and outside hc regime here. >>

        ISD(23)=      '  ____________________________ '
        ISD(24)=      '+ add glazing/door/opening     '
        if(is.gt.1.and.is.lt.nzsur(icomp))then
          ISD(25)=    '< jump to previous surface     '
          ISD(26)=    '> jump to next surface         '
        elseif(is.eq.1)then
          ISD(25)=    '                               '
          ISD(26)=    '> jump to next surface         '
        elseif(is.eq.nzsur(icomp))then
          ISD(25)=    '< jump to previous surface     '
          ISD(26)=    '                               '
        endif
        ISD(27)=      '? help                         '
        ISD(28)=      '- exit menu                    '
        ISDN=28
      else

C << add in additional menu line for external viewfactors >>
C << add in additional menu line for external diffuse shading >>
C << add in additional menu line for external hc correlation >>

        ISD(16)=      '  ____________________________ '
        ISD(17)=      '+ add glazing/door/opening     '
        if(is.gt.1.and.is.lt.nzsur(icomp))then
          ISD(18)=    '< jump to previous surface     '
          ISD(19)=    '> jump to next surface         '
        elseif(is.eq.1)then
          ISD(18)=    '                               '
          ISD(19)=    '> jump to next surface         '
        elseif(is.eq.nzsur(icomp))then
          ISD(18)=    '< jump to previous surface     '
          ISD(19)=    '                               '
        endif
        ISD(20)=      '? help                         '
        ISD(21)=      '- exit to zone description     '
        ISDN=21
      endif
      INODA=-4

C Update image after each edit. Take current common block info
C and update the image. ISFOC is the current surface being edited
C (for highlighting).
C Set all surfaces to standard line width and surface being edited to
C a thick line.
      CALL INLNST(1)

C Note IC is the connection at the other side of surface icom:is.
      CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
      LINSTY(IC)=2
      nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
      CALL redraw(IER)

C If in graphic mode draw the points on top of standard drawing.
      if(MMOD.lt.8)then
        continue
      else
        icontinue=1
        iloop=0
        numv=NVER(IS)
        if(numv.eq.0) goto 42 ! do not attempt to draw the surface
        do while (icontinue.ne.0)
          iloop=iloop+1  ! increment iloop until = numv then jump out
          if(iloop.eq.numv) icontinue=0
          iwhich=jvn(is,iloop)
          if(iwhich.gt.0.and.iwhich.le.MTV)then
            COG1(1)=X(iwhich); COG1(2)=Y(iwhich); COG1(3)=Z(iwhich)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
            if (iclp.eq.0) then
              call u2pixel(COG2(1),COG2(2),iix,iiy)
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              call VERTLBLRED(iix,iiy,COG2(3),iwhich,ier)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif
          endif
        enddo  ! of while loop

C Highlight the arrow of the focus surface. Set size of text.
        write(stemp,'(A)')SNAME(ICOMP,IS)
        if(itsnr.eq.0)then
          CG(1)=SURCOG(icomp,is,1); CG(2)=SURCOG(icomp,is,2)
          CG(3)=SURCOG(icomp,is,3)
          VN(1)=SURVN(icomp,is,1); VN(2)=SURVN(icomp,is,2)
          VN(3)=SURVN(icomp,is,3)
          CALL VECTRN(VN,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iixc,iiyc)
          CALL winfnt(issize)
          iicol=0
          if(greyok)call winscl('z',iicol)
          call arrow(CG,VN,0.3,0.1,ipoints,'a',2)

C Offset the surface name depending on orientation.
          IF(SVFC(ICOMP,IS).EQ.'VERT')THEN
            iixc=iixc+5
          ELSEIF(SVFC(ICOMP,IS).EQ.'CEIL')THEN
            iixc=iixc+5
          ELSEIF(SVFC(ICOMP,IS).EQ.'FLOR')THEN
            iixc=iixc+5
          ELSE
            iixc=iixc+5
          ENDIF
          call CLIPST(stemp,iixc,iiyc,COG2(3),iclp)
          if (iclp.eq.0) then
            call textatxy(iixc,iiyc,stemp,'-',iicol)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif
          CALL winfnt(IMFS)  ! set back to menu font
        else
          CG(1)=SURCOG(icomp,is,1); CG(2)=SURCOG(icomp,is,2)
          CG(3)=SURCOG(icomp,is,3)
          CALL VECTRN(CG,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
          if (iclp.ne.0) goto 42
          call u2pixel(COG2(1),COG2(2),iixc,iiyc)
          CALL winfnt(issize)
          iicol=0
          if(greyok)call winscl('z',iicol)
          IF(SVFC(ICOMP,IS).EQ.'VERT')THEN

C Draw arrow and horizontal line.
            iid1=iixc+3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc+3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc+7
            call eswline(iixc,iiyc,iid1,iiyc)
            iixc=iixc+7
          ELSEIF(SVFC(ICOMP,IS).EQ.'CEIL')THEN

C Draw arrow to surface then up and horizontal to the text.
            iid1=iixc+3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc-3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iiyc-5
            call eswline(iixc,iiyc,iixc,iid1)
            iid1=iiyc-5; iid2=iixc+7; iid3=iiyc-5
            call eswline(iixc,iid1,iid2,iid3)
            iiyc=iiyc-5; iixc=iixc+7
          ELSEIF(SVFC(ICOMP,IS).EQ.'FLOR')THEN

C Draw arrow to surface then down and horizontal to the text.
            iid1=iixc+3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc-3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iiyc+5
            call eswline(iixc,iiyc,iixc,iid1)
            iid1=iiyc+5; iid2=iixc+7; iid3=iiyc+5
            call eswline(iixc,iid1,iid2,iid3)
            iiyc=iiyc+5; iixc=iixc+7
          ENDIF
          iid4=iiyc+3
          call CLIPST(stemp,iixc,iid4,COG2(3),iclp)
          if (iclp.eq.0) then
            call textatxy(iixc,iid4,stemp,'z',iicol)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call draw_surf_flow_symbol(ic,'v')
            call forceflush()
          endif
          CALL winfnt(IMFS)
        endif
      endif

C Help text for the menu.
  42  helptopic='surface_attrib_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      CALL EMENU('Surface attributes',ISD,ISDN,INODA)
      IF(INODA.EQ.ISDN)THEN

C If an attribute has changed save the geometry file.
        ianother=0   ! signal no jump to previous or next.
        if(cfgok)then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
        endif

C Exit and choose another surface.
        return
      ELSEIF(INODA.EQ.ISDN-1)THEN

C Produce help text for the menu.
        helptopic='surface_attrib_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('surface attributes menu',nbhelp,'-',0,0,IER)

      ELSEIF(INODA.eq.ISDN-2)THEN

C Jump to next surface.
        ianother=1

C If an attribute has changed save the geometry file.
        if(cfgok)then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
        endif
        return

      ELSEIF(INODA.eq.ISDN-3)THEN

C Jump to previous surface.
        ianother=-1

C If an attribute has changed save the geometry file.
        if(cfgok)then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
        endif
        return

      ELSEIF(INODA.EQ.ISDN-4)THEN

C Add/insert a surface, return to higher level menu when done.
C The 'ii' request an interactive insertion, the parameters
C XO1,ZO1,XW,ZH are not used in an interactive insert.
        LASTS=NSUR
        call tstamp('>','PRJ: insert surface into another')
        XO1=0.; ZO1=0.
        XW=0.; ZH=0.
        rsname=' '; rsotf=' '; rsmlcn=' '
        rsuse1=' '; rsuse2=' '; rsparent='-'
        CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO1,ZO1,XW,ZH,
     &    rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,guesstype,IER)
        if(LASTS.ne.NSUR)then
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          CALL EMKCFG('-',IER)
          call warnmod(ICOMP,'sf+')
        endif
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL redraw(IER)

C Having updated the geometry for the new surface, ask user for USE.
        icon=IZSTOCN(icomp,nsur)
        call edituse(icomp,nsur,icon,guesstype)
        return
      ELSEIF(INODA.EQ.1)THEN

C Edit surface name, double check if it is assoiated with a flow component
C so that CMPASSOC can be updated after the name change.
        found_association=.false.
        call doesflowrefsurface(ICOMP,is,inode,icmp)
        write(T14,'(2a)')SNAME(icomp,is),'  '
 52     CALL EASKS(T14,' Surface name?','(<=12 characters, no spaces)',
     &    14,'new_surface','surface name',IER,nbhelp)
        write(SN,'(a)') T14(1:12)
        call st2name(SN,SN2)

        call snamdup(SN2,icomp,is,nameok)
        if(nameok)then
          SNAME(ICOMP,IS)=SN2

C Save names so they are not wiped out by wire-frame drawing
C code.
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          if(icmp.gt.0)then
            CMPASSOC(icmp,1)=ZNAME(ICOMP)
            write(SN,'(a12)') SNAME(ICOMP,IS)
            CMPASSOC(icmp,2)=SN
            call update_cmp_pos(icomp,is,icmp)
            updatenetwork=.true.
            WRITE(ISD(15),'(2A)') '  @ flow cmp ',CMNAM(icmp)
            call updatebothflownetworks(ier)
            updatenetwork=.false.
          endif
        else
          call usrmsg('Surface name is a duplicate of an existing',
     &                'surface. Please supply a different name.','W')
          goto 52
        endif
        MODIFYVIEW=.TRUE.; MODGEO=.TRUE.
      ELSEIF(INODA.EQ.2)THEN

C Specify whether surface multilayer construction is opaque, transparent,
C or fictitious (bookkeeping purposes only). If transparent ask user
C which set of optical properties to use.
        SALT(1)='all layers opaque                '
        SALT(2)='at least 1 layer transparent     '
        SALT(3)='n/a fictitious (mass flow detail)'
        SALT(4)='n/a fictitious (geometry detail) '
        SALT(5)='                                 '
        SALT(6)='                                 '

        IX=1
    9   CALL EPICKS(IX,IVAL,' ','Surface Type',
     &         33,6,SALT,'surface type',IER,nbhelp)

        IF(IX.EQ.0)GOTO 13
        IF(IVAL(1).EQ.1)THEN
          SOTF(ICOMP,IS)='OPAQUE'
        ELSEIF(IVAL(1).EQ.2)THEN

C If 1.1 geometry format the TRAN will be updated if/when
C the user defines the optical property set and/or the construction.
          SOTF(ICOMP,IS)='TRAN'
        ELSE
          goto 9
        ENDIF
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.3)THEN

C Specify surface location for daylighting calculations and floor area
C based casual gain calculations.
        if(SVFC(icomp,iss)(1:4).eq.'UNKN')ino=1
        if(SVFC(icomp,iss)(1:4).eq.'VERT')ino=2
        if(SVFC(icomp,iss)(1:4).eq.'CEIL')ino=3
        if(SVFC(icomp,iss)(1:4).eq.'FLOR')ino=4
        ilno=ino
        idno=2
        call MENUATOL(' ','Surface Orientation',
     &   'a unknown or sloped','b vertical (or nearly so)',
     &   'c facing down (e.g. ceiling) ','d facing up (e.g. floor)',
     &   ' ',' ',' ',' ',' ',' ',' ',' ',ino,idno,nbhelp)
        IF(ilno.eq.ino)GOTO 13
        IF(ino.eq.0)GOTO 13
        IF(ino.EQ.1)THEN
          SVFC(icomp,ISS)='UNKN'
        ELSEIF(ino.EQ.2)THEN
          SVFC(icomp,ISS)='VERT'
        ELSEIF(ino.EQ.3)THEN
          SVFC(icomp,ISS)='CEIL'
        ELSEIF(ino.EQ.4)THEN
          SVFC(icomp,ISS)='FLOR'
        ENDIF
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.7)THEN

C Version 1.1 geometry no longer has a concept of surface
C indentation so cannot be selected.
C Define the indentation for the current surface.
        helptopic='surface_indentation'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(SINDT,' ','Surface indentation?',
     &     0.0,'F',1.0,'W',0.,'Surface indentation',IER,5)
        Y0S(IS)=SINDT
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.8)THEN

C Check if user wishes to see construction details as well. If there is
C a zone construction file report its common block information, otherwise
C if SMLCN matches something in the constructions database then use
C that reporting mechanism.
        write(SN,'(a12)') SNAME(ICOMP,is)
        if(SMLCN(ICOMP,is)(1:4).ne.'UNKN')then

C << TODO - test if date of zone construction file is older than the
C << geometry file and if so do not use CONINF.

C Use smlcindex to get MLC index for surface.
          if(LTHRM(ICOMP)(1:7).eq.'UNKNOWN'.or.
     &       LTHRM(ICOMP)(1:2).eq.'  ')then
            if(smlcindex(icomp,is).ne.0)then
              write(outs,'(3a)') 'View thermo-physical properties of ',
     &          SN(1:lnblnk(SN)),'?'
              call easkok(' ',outs,ok,nbhelp)
              if(ok)then
                lnssmlc=lnblnk(SMLCN(icomp,is))
                do ii=1,nmlc
                  if(SMLCN(icomp,is)(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcname(ii)))then
                    call ETMLDB(1,iuout,ii,imerr)
                  endif
                enddo
              endif
            endif
          else

C A non-blank, possibly known construction.
            XST=.FALSE.
            call FINDFIL(LTHRM(ICOMP),XST)
            if(XST)then
              write(outs,'(3a)') 'View thermo-physical properties of ',
     &          SN(1:lnblnk(SN)),'?'
              call easkok(' ',outs,ok,nbhelp)
              if(ok)then
                CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)

C << This would also be a good place to list out optical
C << properties and the optical name.
                CALL CONINF(ICOMP,IS,iuout)
              endif
            endif
          endif
          write(outs,'(3a)') 'Modify thermo-physical properties of ',
     &      SN(1:lnblnk(SN)),'?'
          helptopic='surface_non_symetric'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call easkok(' ',outs,ok,nbhelp)
        else
          ok=.true.
        endif
        if(ok)then
          CALL EPMENSV
          if(mlcver.eq.0)then
            CALL EPKMLC(ISEL,'Select one of the constructions',
     &        'or exit.',IER)
          else
            call edisp(iuout,'Select one of the constructions')
            CALL EDMLDB2(modmlc,'-',ISEL,IER)
          endif
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(SMLCN(ICOMP,IS),'(A)') mlcname(ISEL)
            smlcindex(icomp,is)=isel   ! update the array

C Find the optical name.
            IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
              SOTF(ICOMP,IS)='OPAQUE'
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
              SOTF(ICOMP,IS)='CFC '
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC2')then
              SOTF(ICOMP,IS)='CFC2'
            ELSE
              WRITE(SOTF(ICOMP,IS),'(A)') mlcoptical(ISEL)
            ENDIF
            MODGEO=.TRUE.
            call warnmod(ICOMP,'sat')

C If showother remember the other side SUSE so it can be re-established.
            if(showother)then
              TOUSE1=SUSE(IC2(ioc),IE2(ioc),1)  ! record prior to edit
              TOUSE2=SUSE(IC2(ioc),IE2(ioc),2)
            endif

C If this is a partition find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.

C Use smlcindex to get MLC index for this surface and material.h
C commons to get information on the other partition.
            if(showother.and.icoth.ne.0.and.
     &        smlcindex(icomp,is).ne.0)then
              ii=smlcindex(icomp,is)
              updoth=.false.
              if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                lnssmlc=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                if(SMLCN(IC2(ioc),IE2(ioc))(1:lnssmlc).eq.
     &             mlcname(ii)(1:lnmlcname(ii)))then
                  TOSMLCN= mlcname(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                elseif(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
                  TOSMLCN= mlcname(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                  updoth=.true.
                else
                  TOSMLCN= mlcname(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                  updoth=.true.
                endif
                iissmlci=ii
              elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                lnsmlcn=lnblnk(SMLCN(icomp,iss))
                write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &            ' has a nonsymmetric construction ',
     &            SMLCN(icomp,iss)(1:lnsmlcn),'.'
                call edisp(iuout,outs)
                lnsmlcn=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                write(outs,'(5a)') 'It faces ',
     &            SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &            SMLCN(IC2(ioc),IE2(ioc))(1:lnsmlcn),'.'
                call edisp(iuout,outs)
                if(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
                  TOSMLCN= mlcname(ii)  ! replace UNKNOWN
                  write(TOOPT,'(a)') mlcoptical(ii)
                  iissmlci=smlcindex(icomp,iss)
                  updoth=.true.
                else
                  TOSMLCN= SMLCN(IC2(ioc),IE2(ioc))
                  write(TOOPT,'(a)') mlcoptical(ii)
                  iissmlci=smlcindex(IC2(ioc),IE2(ioc))  ! leave it alone
                  updoth=.false.
                endif
              else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
                lnsmlcn=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                if(SMLCN(IC2(ioc),IE2(ioc))(1:4).eq.'UNKN')then
                  TOSMLCN= mlcsymetric(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                  updoth=.true.
                elseif(SMLCN(IC2(ioc),IE2(ioc))(1:lnsmlcn).eq.
     &                 mlcsymetric(ii)(1:lnsmlcn))then
                  TOSMLCN= mlcsymetric(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                else
                  TOSMLCN= mlcsymetric(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                  updoth=.true.
                endif
                iissmlci=matsymindex(ii)  ! use returned value
              endif
              if(updoth)then

C First backup the current zones data before reading in the other
C zone.
                CALL ESCZONE(ICOMP)
                lnicoth=lnblnk(SMLCN(IC2(ioc),IE2(ioc)))
                write(outs,'(6a)')
     &            'Updating `other side` composition of ',
     &            SN(1:lnblnk(SN)),' from ',
     &            SMLCN(IC2(ioc),IE2(ioc))(1:lnicoth),
     &            ' > ',TOSMLCN
                if(SMLCN(IC2(ioc),IE2(ioc))(1:lnicoth).ne.
     &             TOSMLCN(1:lnblnk(TOSMLCN)))then
                  call usrmsg(outs,' ','P')
                endif
                call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                if(newgeo)then
                  call georead(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,
     &              iuout,IER)
                else
                  call egomin(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,0,
     &              iuout,IER)
                endif

C Assign new MLC name to the icoth surface as well as SOTF with the
C optical properties and MLC index from material.h commons.
                if(iissmlci.ne.0)then
                  SMLCN(IC2(ioc),IE2(ioc))=TOSMLCN
                  smlcindex(IC2(ioc),IE2(ioc))=iissmlci  ! update
                endif
                if(TOOPT(1:2).ne.' ')then
                  write(SOTF(IC2(ioc),IE2(ioc)),'(a)') TOOPT
                endif
                SUSE(IC2(ioc),IE2(ioc),1)=TOUSE1
                SUSE(IC2(ioc),IE2(ioc),2)=TOUSE2

C Logic to decide whether to update other zone geometry format.
                call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                if(.NOT.newgeo)then
                  gversion(IC2(ioc)) =1.1
                  newgeo = .true.
                endif
                call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &            iuout,3,IER)
                IF(IER.NE.0)CALL USRMSG(
     &            'Problem updating other surface attribute... ',
     &            '(could not write other geometry file).','W')

C Read current zone data back in to commons. But the backed up data
C might not include the name of the newly selected mlc name so just
C to be sure reset smlcn with smlcn so it will show up in the menu
C when it is re-freshed.
                CALL ERCZONE(ICOMP)
                call usrmsg(' Updating other side...done.',' ','-')
                updoth=.false.
              endif
            endif
          else

C User selected UNKNOWN MLC so update arrays.
C            ioc=IZSTOCN(ICOMP,is)
            WRITE(SMLCN(ICOMP,IS),'(A)') 'UNKNOWN'
            SOTF(ICOMP,IS)='OPAQUE'
            smlcindex(icomp,is)=0
            MODGEO=.TRUE.

          endif
          IF(IER.EQ.1)THEN
            CALL USRMSG(' ','A problem was encountered.','W')
          ENDIF
        endif
      ELSEIF(INODA.EQ.9)THEN

C Specify the environment of the 'other' face of the surface.
        call specify_other_face(icomp,is,modgeo,showother,
     &    updoth,'s',ier)

      ELSEIF(INODA.EQ.10)THEN

C Version 1.1 geometry holds information on child pararent
C relationship. What is shown here is result of geometric
C scan and mentions if there are children and the name of
C the first child.
C << manual editing facility to-be-done >>

      ELSEIF(INODA.EQ.11)THEN

C Version 1.1 geometry holds information on child pararent
C relationship. What is shown here is result of geometric
C scan and mentions the name of the parent surface.
C << manual editing to be done >>

      ELSEIF(INODA.EQ.12)THEN

C Version 1.1 geometry holds SEMANTIC tags for surfaces.
C If it changed and surface is a partition remember so
C can update other zone (as it done with SMLCN).
        if(newgeo)then
          guesstype='any_type'
          ic=IZSTOCN(ICOMP,iss)  ! the current connection
          iz=icomp; isur=iss; icon=ic
          TOUSE1=SUSE(ICOMP,iss,1)  ! record prior to edit
          TOUSE2=SUSE(ICOMP,iss,2)
          call edituse(iz,isur,icon,guesstype) ! with other side update
          if(TOUSE1(1:12).eq.SUSE(ICOMP,iss,1)(1:12).and.
     &       TOUSE2(1:12).eq.SUSE(ICOMP,iss,2)(1:12))then
            continue  ! no change
          else

C Place related symbol on the wireframe if in graphic mode.
C Cases to ignore.
            goforit=.true.; updatenetwork=.false.
            ic=IZSTOCN(ICOMP,iss)  ! the current connection
            if(SUSE(ICOMP,iss,1)(1:4).eq.'WALL'.or.
     &         SUSE(ICOMP,iss,1)(1:5).eq.'FLOOR'.or.
     &         SUSE(ICOMP,iss,1)(1:5).eq.'FURNI'.or.
     &         SUSE(ICOMP,iss,1)(1:6).eq.'REVEAL'.or.
     &         SUSE(ICOMP,iss,1)(1:7).eq.'ITEQUIP'.or.
     &         SUSE(ICOMP,iss,1)(1:5).eq.'PARTN'.or.
     &         SUSE(ICOMP,iss,1)(1:4).eq.'ROOF'.or.
     &         SUSE(ICOMP,iss,1)(1:5).eq.'STRUC'.or.
     &         SUSE(ICOMP,iss,1)(1:7).eq.'FIXTURE'.or.
     &         SUSE(ICOMP,iss,1)(1:6).eq.'PLANTS')then
              goforit=.false.
            endif

C If there is a flow network see if there is further work to do.
            if(IAIRN.ge.3.and.ICAAS(ICOMP).ne.0.and.goforit)then ! Proceed?

C If SUSE implies a flow component then....
              ic=IZSTOCN(ICOMP,iss)  ! the current connection
              if(SUSE(ICOMP,iss,1)(1:4).eq.'DOOR'.or.
     &           SUSE(ICOMP,iss,1)(1:6).eq.'P-DOOR'.or.
     &           SUSE(ICOMP,iss,1)(1:5).eq.'FRAME'.or.
     &           SUSE(ICOMP,iss,1)(1:7).eq.'F-FRAME'.or.
     &           SUSE(ICOMP,iss,1)(1:5).eq.'GRILL'.or.
     &           SUSE(ICOMP,iss,1)(1:6).eq.'WINDOW'.or.
     &           SUSE(ICOMP,iss,1)(1:8).eq.'D-WINDOW'.or.
     &           SUSE(ICOMP,iss,1)(1:8).eq.'S-WINDOW'.or.
     &           SUSE(ICOMP,iss,1)(1:8).eq.'C-WINDOW'.or.
     &           SUSE(ICOMP,iss,1)(1:4).eq.'FICT')then

C If the user has selected a USE that reflects a flow node or component
C and no entity in the flow network file points to this surface then
C offer to add such a default entity to the flow network.
                if(.NOT.found_association)then
                  INVT=0
                  CALL EASKABC(
     &              'The surface USE attribute suggests an associated',
     &              'flow component. Options:',
     &              'pick existing flow component',
     &              'create one to match',
     &              'ignore (defined elsewhere)',INVT,nbhelp)
                  if(INVT.eq.1)then
                    call ASKRCMP('select component','-',ICOMPN,IER)
                    if(ICOMPN.gt.0)then
                      CMPASSOC(ICOMPN,1)=ZNAME(ICOMP)
                      write(SN,'(a12)') SNAME(ICOMP,IS)
                      CMPASSOC(ICOMPN,2)=SN
                      updatenetwork=.true.
                    endif
                  elseif(INVT.eq.2)then
                    call create_surface_node_cmp(icomp,is)
                    updatenetwork=.true.
                  elseif(INVT.eq.3)then
                    continue
                  endif
                else

C If there is an entity in the flow network file that points to
C this surface then ?? perhaps its position needs to be updated?
                  call ASKRCMP('select component','-',ICOMPN,IER)
                  if(ICOMPN.gt.0)then
                    CMPASSOC(ICOMPN,1)=ZNAME(ICOMP)
                    write(SN,'(a12)') SNAME(ICOMP,IS)
                    CMPASSOC(ICOMPN,2)=SN
                    updatenetwork=.true.
                  endif
                endif
                if(updatenetwork)then
                  call updatebothflownetworks(ier)
                endif
              endif
            endif
          endif
        endif
      ELSEIF(INODA.EQ.13)THEN

C Usage subtype is handled by 11th menu item.
        continue

      ELSEIF(INODA.EQ.14)THEN
        continue  ! report on any associated flow node.
      ELSEIF(INODA.EQ.15)THEN
        continue  ! report on any associated flow component.
      ELSE
        INODA=-4
        goto 13
      ENDIF
      INODA=-4
      goto 13

      END


C ******************** specify_other_face ******************** 
C Called by edsura to support user choices about what happens
C on the 'other' face of a surface.

      subroutine specify_other_face(izone,isurf,modgeo,showother,
     &  updoth,act,ier)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "site.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

      integer lnblnk  ! function definition

C Passed parameters:
      integer izone,isurf  ! the current zone and surface
      logical modgeo,showother,updoth
      character act*1      ! 's' = specify 'd' = discover
      integer ier          ! non-zero is an error

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL close

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      logical newgeo  ! to use for testing if new/old geometry file.
      logical origisfloor,origisceiling ! close to horizontal
      logical testisfloor,testisceiling

      DIMENSION SALT(14),IVAL(MCOM)  ! ,bl(144)
      DIMENSION cog(3),cogo(3)
      DIMENSION VN(3)
      integer iocog                          ! loop counter for detecting close surfaces
      integer ifound,ifoundc                 ! how many close surfaces and which one
      CHARACTER DESCRC*25,outs*124,CXSTR*78
      CHARACTER SALT*33,SN*12
      character TOSMLCN*32                   ! remember other side mlc name
      character TOOPT*32                     ! to remember other side optics
      character TOUSE1*12, TOUSE2*12         ! remember other side USE
      character TSOTH1*24,TSOTH2*24,TSOTH3*24
      character choice1*76, choice2*76
      integer ico

      logical closecog,closea,closee,closeaz ! to check for nearby surfaces
      logical closeenough,closecog1m
      integer lnsa,lnsb                      ! lengths for surface names
      integer ii                             ! for loop

      integer iissmlci,IIC3                  ! other side construction index, surface index
      real vdist                             ! distance between cog of two surfaces

      helpinsub='edgeo'                      ! set for subroutine
      icoth=0; ii=0
      TOSMLCN=' '; TOOPT=' '; TOUSE1=' '; TOUSE2=' '
      TSOTH1=' '; TSOTH2=' '; TSOTH3=' '
      iissmlci=0
      icomp=izone                            ! local variables for zone index
      is=isurf                               ! surface index
      ioc=IZSTOCN(icomp,is)                  ! current connection
      ic=IZSTOCN(icomp,is)
      ifoundc=0; ifoundc2=0
      if(ICT(ioc).eq.3)then
        showother=.true.
        icoth=IZSTOCN(IC2(ioc),IE2(ioc))
      else
        showother=.false.
        icoth=0
      endif
      write(SN,'(a12)') SNAME(ICOMP,IS)

C Check which version.
      call eclose(gversion(icomp),1.1,0.01,newgeo)


C Specify the environment of the 'other' face of the surface.
      updoth=.false.
      call edisp(iuout,' ')
      call edisp(iuout,'The current model topology is...')
      CALL CONXINFO(1,0,CXSTR)
      write(outs,'(1X,A)') CXSTR
      CALL EDISP(iuout,outs)
      CALL CONXINFO(1,IC,CXSTR)
      write(outs,'(1X,A)') CXSTR
      CALL EDISP(iuout,outs)
      call edisp(iuout,' ')

      SALT(1)='exterior                         '
      SALT(2)='similar to current               '
      SALT(3)='prescribed static                '
      SALT(4)='surface in this zone             '
      SALT(5)='surface in other zone            '
      SALT(6)='ground (monthly profile)'
      if(NGRDP.gt.0)then
        SALT(7)='ground (user defined profile)  '
      else
        SALT(7)='ground (no user defined profile)'
      endif
      SALT(8)='ground (3D conduction model)    '
      SALT(9)='adiabatic                       '
      SALT(10)='BASESIMP foundation configuration'
      SALT(11)='CEN 13791 partition            '
      SALT(12)='unknown at this time           '
      SALT(13)='scan for nearby surfaces       '
      SALT(14)='no change at this time         '

      if(act(1:1).eq.'s')then      ! ask user
        helptopic='surface_boundary_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IX=1
        CALL EPICKS(IX,IVAL,' ','Other Side Boundary Condition',
     &    33,14,SALT,'surface boundary ',IER,nbhelp)
      elseif(act(1:1).eq.'d')then  ! auto choose scan for nearby surfaces
        IX=1; IVAL(1)=13
      endif

      IF(IX.EQ.0) return
      IF(IVAL(1).EQ.14) return

C Determine the match in the system topology and update it.
      CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
      IC1(IC)=ICOMP; IE1(IC)=IS
      IF(IVAL(1).EQ.1)THEN
        SOTHER(ICOMP,IS,1)='EXTERIOR'; SOTHER(ICOMP,IS,2)='0'
        SOTHER(ICOMP,IS,3)='0'
        ICT(IC)=0; IC2(IC)=0; IE2(IC)=0
        showother=.false.
        updoth=.false.
      ELSEIF(IVAL(1).EQ.2)THEN
        SOTHER(ICOMP,IS,1)='SIMILAR'
        ICT(IC)=1
        VALT= real(IC2(IC)); VALW= real(IE2(IC))  ! pick up existing value
        CALL EASKR(VALT,' ','Temperature offset?',
     &           -99.,'F',700.,'F',0.0,'offset temp',IER,nbhelp)
        CALL EASKR(VALW,' ','Radiation offset?',
     &           0.0,'W',99999.,'W',0.0,'offset rad',IER,nbhelp)
        if(INT(VALT).ge.0)write(SOTHER(ICOMP,IS,2),'(i3.3)')INT(VALT)
        if(INT(VALT).lt.0)write(SOTHER(ICOMP,IS,2),'(i3)')INT(VALT)
        write(SOTHER(ICOMP,IS,3),'(i3.3)')INT(VALW)
        IC2(IC)=INT(VALT); IE2(IC)=INT(VALW)
        showother=.false.
        updoth=.false.
      ELSEIF(IVAL(1).EQ.3)THEN
        SOTHER(ICOMP,IS,1)='CONSTANT'
        ICT(IC)=2
        VALT=0.0; VALW=0.0
        CALL EASKR(VALT,' ','Constant temperature?',
     &    -99.,'F',700.,'F',20.,'adjacent temperature',IER,nbhelp)
        CALL EASKR(VALW,' ','Constant radiation?',
     &    0.0,'W',99999.,'W',0.0,'adjacent radiation source',
     &    IER,nbhelp)
        IC2(IC)=INT(VALT)
        IE2(IC)=INT(VALW)
        if(INT(VALT).ge.0)write(SOTHER(ICOMP,IS,2),'(i3.3)')INT(VALT)
        if(INT(VALT).lt.0)write(SOTHER(ICOMP,IS,2),'(i3)')INT(VALT)
        write(SOTHER(ICOMP,IS,3),'(i3.3)')INT(VALW)
        showother=.false.
        updoth=.false.
      ELSEIF(IVAL(1).EQ.4)THEN

C Link to a surface in this zone (this is allowed as an alternative
C to adiabetic connections) and is what is used when setting up
C internal mass bodies.
        IZ=ICOMP

C General image option flags.
        ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
        ITVNO=1; ITORG=1; ITSNR=1; ITGRD=1
        GRDIS=0.0; ITPPSW=0

        ISO=1
        CALL EPMENSV
        CALL EASKSUR(IZ,ISO,'A','Select another surface in',
     &    'this zone.',IER)
        CALL EPMENRC
        if(ISO.GT.0)then

C Write ANOTHER followed by the indices of zone and surface.
          SOTHER(ICOMP,IS,1)='ANOTHER'
          write(SOTHER(ICOMP,IS,2),'(i3.3)') iz
          write(SOTHER(ICOMP,IS,3),'(i3.3)') iso
          ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO

C Sort out the other face attributes (within this zone).
          ico = izstocn(iz,iso)
          IC1(ICO)=IZ; IE1(ICO)=ISO
          ICT(ICO)=3
          IC2(ICO)=IC1(IC); IE2(ICO)=IE1(IC)

C Also check the other side construction and see if it matches. If
C current surface MLC index is known get isymindex for the other side
          if(smlcindex(icomp,is).gt.0) then
            ii=smlcindex(icomp,is)
            if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
              lnssmlc=lnblnk(SMLCN(iz,iso))
              if(SMLCN(iz,iso)(1:lnssmlc).eq.
     &           mlcname(ii)(1:lnmlcname(ii)))then
                TOSMLCN= mlcname(ii)
              elseif(SMLCN(iz,iso)(1:4).eq.'UNKN')then
                TOSMLCN= mlcname(ii)
              else
                TOSMLCN= mlcname(ii)
              endif
              iissmlci=ii
            elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition if db does not have a linked MLC - inform the user.
              lnssmlc=lnblnk(SMLCN(icomp,is))
              write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &          ' has a nonsymmetric construction ',
     &          SMLCN(icomp,is)(1:lnssmlc),'.'
              call edisp(iuout,outs)
              lnsmlc=lnblnk(SMLCN(iz,iso))
              write(outs,'(5a)') 'It faces ',
     &          SNAME(iz,iso),' composed of ',
     &          SMLCN(iz,iso)(1:lnsmlc),'.'
              call edisp(iuout,outs)
              iissmlci=matsymindex(ii)  ! use returned value
            else
              if(SMLCN(iz,iso)(1:4).eq.'UNKN')then
                TOSMLCN= mlcsymetric(ii)
                write(TOOPT,'(a)') mlcoptical(ii)
              elseif(SMLCN(iz,iso)(1:12).eq.mlcsymetric(ii)(1:12))then
                TOSMLCN= mlcsymetric(ii)
                write(TOOPT,'(a)') mlcoptical(ii)
              else
                TOSMLCN= mlcsymetric(ii)
                write(TOOPT,'(a)') mlcoptical(ii)
              endif
              iissmlci=matsymindex(ii)  ! use returned value
            endif
          else
            iissmlci=0  ! mark as unknown
            TOSMLCN='UNKNOWN'
          endif

C Update the attributes of the other surface in the zone so it will
C be saved along with the changes in the initial surface.
          if(iissmlci.ne.0)then
            SMLCN(iz,iso)=TOSMLCN
            smlcindex(iz,iso)=iissmlci  ! update if known
          endif
          if(TOOPT(1:2).ne.' ')then
            write(SOTF(iz,iso),'(a)') TOOPT
          endif
          SOTHER(iz,iso,1)='ANOTHER'
          write(SOTHER(iz,iso,2),'(i3.3)') IC2(ico)
          write(SOTHER(iz,iso,3),'(i3.3)') IE2(ico)
          TSOTH1='ANOTHER'
          write(TSOTH2,'(i3.3)') IC2(ico)
          write(TSOTH3,'(i3.3)') IE2(ico)
          updoth=.true.
          showother=.true.

C The temporary variabels TOSMLCN, TSOTH1 etc. will be acted on in the
C logic following on from line 5614 below.
        else
          continue          ! User did not select surface so continue without changing.
        endif
      ELSEIF(IVAL(1).EQ.5)THEN

C Ask for specific zone to find a matching partition in.
        CALL EASKGEOF('If other zone known select it, else exit.',
     &    CFGOK,IZ,'-',34,IER)
        if(IZ.GT.0)then

C General image option flags.
          ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
          ITVNO=1; ITORG=1; ITSNR=1; ITGRD=1
          GRDIS=0.0; ITPPSW=0

C Remember the COG of the current surface and then compare it with
C the cog of the surfaces in the other zone as well as their areas.
C Report if within 1m and 0.5m2 area difference.
          cog(1)=surcog(icomp,is,1)
          cog(2)=surcog(icomp,is,2)
          cog(3)=surcog(icomp,is,3)
          do 77 ij=1,nzsur(iz)
            closecog=.false.
            iocog=IZSTOCN(iz,ij)
            cogo(1)=surcog(iz,ij,1)
            cogo(2)=surcog(iz,ij,2)
            cogo(3)=surcog(iz,ij,3)
            call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &        cogo(3),COGCC,closecog)
            if(closecog)then
              CALL ECLOSE(SNA(iz,ij),sna(icomp,is),SNACC,closea)
              if(closea)then
                lnsa=lnblnk(sname(iz,ij)); lnsb=lnblnk(sname(icomp,is))
                write(outs,'(5a,f6.2,3a,f6.2)') 'Surface ',
     &            sname(iz,ij)(1:lnsa),' in ',
     &            zname(iz)(1:lnzname(iz)),
     &            ' with area ',SNA(iz,ij),' might match ',
     &            sname(icomp,is)(1:lnsb),' with area ',sna(icomp,is)
                call edisp(iuout,outs)
              endif
            endif
  77      continue

C Reset all surface lines to standard width except for current,
C and display both zones.
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          CALL INLNST(1)
          LINSTY(IC)=2
          nzg=2
          nznog(1)=ICOMP
          nznog(2)=IZ
          izgfoc=ICOMP
          CALL redraw(IER)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.

          ISO=1
          CALL EPMENSV
          CALL EASKSUR(IZ,ISO,'A','Select surface in',
     &      'adjacent zone.',IER)
          CALL EPMENRC
          if(ISO.GT.0)then

C Write ANOTHER followed by the indices of zone and surface.
            SOTHER(ICOMP,IS,1)='ANOTHER'
            write(SOTHER(ICOMP,IS,2),'(i3.3)') iz
            write(SOTHER(ICOMP,IS,3),'(i3.3)') iso
            ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO

C If applicable find other surface, return its connection number (ICO)
C and point it to the current surface.
            updoth=.false.
            CALL EASKOK(' ','Update other surface?',updoth,nbhelp)
            if(updoth)then
              CALL SURADJ(IZ,ISO,IE,TMP,IZC,ISC,ICO,DESCRC)
              IC1(ICO)=IZ; IE1(ICO)=ISO; ICT(ICO)=3
              IC2(ICO)=IC1(IC); IE2(ICO)=IE1(IC)

              SOTHER(IZ,ISO,1)='ANOTHER'
              write(SOTHER(IZ,ISO,2),'(i3.3)') IC2(ICO)
              write(SOTHER(IZ,ISO,3),'(i3.3)') IE2(ICO)
              TSOTH1='ANOTHER'
              write(TSOTH2,'(i3.3)') IC2(ICO)
              write(TSOTH3,'(i3.3)') IE2(ICO)

C Update information needed to display information on the other side
C of this surface (i.e. if we went from unknown to a partition).
              icoth=ICO

C If SUSE is currently 'WALL', reset to 'PARTN'.
              if(SUSE(ICOMP,is,1)(1:4).eq.'WALL')then
                SUSE(ICOMP,is,1)='PARTN'
                SUSE(ICOMP,is,2)='-'
              endif

C Reconcile the surface attributes between the focus surface and the
C other surface.
              call reconcileattributes(icomp,is,SN,iz,iso,showother,
     &           updoth,TOSMLCN,TOOPT,TOUSE1,TOUSE2,IISSMLCI)
  
              icoth=ICO  ! remember the connection for the other surface
              CALL EMKCFG('-',IER)  ! update connections file.

C Other zone surface attributes are now known. They will be
C written with code at ~line 3817 in the if(updoth)then section.
            endif
            showother=.true.
          else

C User did not select surface so continue without changing.
            continue
          endif
        else

C User did not select zone so continue without changing.
          continue
        endif

      ELSEIF(IVAL(1).EQ.6)THEN

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
        CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
        CALL EDISP(iuout,'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
        do 21 igrdp=1,mgrdp
          CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
          if(.NOT.close)then
            WRITE(OUTS,'(I2,2a)')igrdp,' ',grdtmpname(IGRDP)
            call edisp(iuout,outs)
            WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
            call edisp(iuout,outs)
          endif
 21     continue

C Ask for the standard monthly profile index and if a valid response
C set the common blocks. If out of range warn user. If user cancels
C the action make no changes and jump back to 13 (the menu creation code).
        iic2=ic2(IC)
  777   CALL EASKI(IIC2,' ','Standard ground profile number?',
     &    1,'F',mgrdp,'F',1,'ground profile',IERI,nbhelp)
        IF(IIC2.GT.0)THEN
          if(ieri.eq.0)then
            IC2(IC)=iic2; IE2(IC)=0
            SOTHER(ICOMP,IS,1)='GROUND'
            write(SOTHER(ICOMP,IS,2),'(i3.3)')iic2
            SOTHER(ICOMP,IS,3)='0'
            ICT(IC)=4
            showother=.false.
          elseif(ieri.eq.-3)then
            return
          else
            goto 777
          endif
        ELSE
          CALL USRMSG(' ','Unacceptable ground profile.','W')
          GOTO 777
        ENDIF
        updoth=.false.
      ELSEIF(IVAL(1).EQ.7)THEN

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
        if(NGRDP.gt.0)then
          CALL EDISP(iuout,' ')
          do 22 igrdp=1,NGRDP
            CALL EDISP(iuout,'User defined ground profile Jan-Dec:')
            WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
            call edisp(iuout,outs)
 22       continue

C Ask for the user defined monthly profile index and if a valid response
C set the common blocks. If out of range warn user. If user cancels
C the action make no changes and jump back to 13 (the menu creation code).
 778      CALL EASKI(IIE2,' ','User defined ground profile number?',
     &      0,'F',9,'F',1,'user defined profile',IERI,nbhelp)
          if(IIE2.gt.0)then
            if(ieri.eq.0)then
              IC2(IC)=0; IE2(IC)=iie2
              SOTHER(ICOMP,IS,1)='GROUND'; SOTHER(ICOMP,IS,2)='0'
              write(SOTHER(ICOMP,IS,3),'(i3.3)')iie2
              ICT(IC)=4
            elseif(ieri.eq.-3)then
              return
            else
              goto 778
            endif
          else
            call usrmsg(
     &        'User defined ground profile out of range. Retry',
     &        'or go to `Model Context` menu to define.','W')
            goto 778
          endif
        else
         call usrmsg(
     &      'No user defined ground profiles have been found.',
     &      'Go to `Model Context` menu to define.','W')
        endif
        updoth=.false.
      ELSEIF(IVAL(1).EQ.8)THEN

C Link to a 3D conduction model.
        SOTHER(ICOMP,IS,1)='GROUND'; SOTHER(ICOMP,IS,2)='-3'
        SOTHER(ICOMP,IS,3)='0'
        ICT(IC)=4; IC2(IC)=-3; IE2(IC)=0
        call edisp(iuout,'Link set to 3D ground conduction model.')
        showother=.false.
        updoth=.false.
      ELSEIF(IVAL(1).EQ.9)THEN

C Set at adiabatic.
        SOTHER(ICOMP,IS,1)='ADIABATIC'; SOTHER(ICOMP,IS,2)='0'
        SOTHER(ICOMP,IS,3)='0'
        ICT(IC)=5; IE2(IC)=0; IC2(IC)=0
        showother=.false.
        updoth=.false.
      ELSEIF(IVAL(1).EQ.10)THEN

C Get BASESIMP configuration via call to bsimtype.
        IBS=IC2(IC)
        call bsimtype(ibs)

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration. If the user cancels the
C action do not instantiate the data.
        Ifrac=IE2(IC)
        CALL EASKI(Ifrac,' ','% BASESIMP loss to this surface?',
     &      0,'F',100,'F',100,'BASESIMP heat loss percent',
     &      IERI,nbhelp)
        if(ieri.eq.0)then
          IE2(IC)=Ifrac; IC2(IC)=IBS
          SOTHER(ICOMP,IS,1)='BASESIMP'
          write(SOTHER(ICOMP,IS,2),'(i3.3)')Ifrac
          write(SOTHER(ICOMP,IS,3),'(i3.3)')IBS
          ICT(IC)=6
          showother=.false.
        elseif(ieri.eq.-3)then
          return
        endif
        updoth=.false.

      ELSEIF(IVAL(1).EQ.11)THEN
        SOTHER(ICOMP,IS,1)='IDENT_CEN'; SOTHER(ICOMP,IS,2)='0'
        SOTHER(ICOMP,IS,3)='0'
        ICT(IC)=7
        VALT=0.0; VALW=0.0
        IC2(IC)=INT(VALT); IE2(IC)=INT(VALW)
        showother=.false.
        updoth=.false.

      ELSEIF(IVAL(1).EQ.12)THEN

C Reset to zeros set ICT to -1 to signal UNKNOWN
        updoth=.false.
        SOTHER(ICOMP,IS,1)='UNKNOWN'; SOTHER(ICOMP,IS,2)='0'
        SOTHER(ICOMP,IS,3)='0'
        ICT(IC)=-1; IE2(IC)=0; IC2(IC)=0
        showother=.false.

      ELSEIF(IVAL(1).EQ.13)THEN

C Scan for neighbor surfaces in all zones. Report if within 1m
C and 0.5m2 area difference and also take into account difference
C in surface normals.
C << If one surface has additional vertices the COG will differ
C << even if all other parameters are close enough. What we
C << want is to allow, say one failure.
C << logic for that in edtopol.F ~line 1500. >>
        updoth=.false.
        icuriz=IC1(ic); icuris=IE1(ic)
        cog(1)=surcog(icuriz,icuris,1)
        cog(2)=surcog(icuriz,icuris,2)
        cog(3)=surcog(icuriz,icuris,3)
        ifound=0; ifoundc=0
        lnsb=lnblnk(sname(icuriz,icuris))
        write(outs,'(3a,f9.3,a,f7.2,a,f7.2,a)') 'For surface ',
     &    sname(icuriz,icuris)(1:lnsb),
     &    ' (area=',sna(icuriz,icuris),' azimuth=',
     &    spazi(icuriz,icuris),' elevation=',spelv(icuriz,icuris),
     &    '):'
        call edisp(iuout,outs)

C Remember if surface elevation is ~floor -90 or ceiling 90.
        call eclose(SPELV(icuriz,icuris),-90.0,1.0,origisfloor)
        call eclose(SPELV(icuriz,icuris),90.0,1.0,origisceiling)

C Loop through all surfaces in the model looking for matches
C using a tight logic.
        do 78 ij=1,ncon
          closecog=.false.; closecog1m=.false.;
          closea=.false.; closee=.false.
          closeaz=.false.; closeenough=.false.
          if(ic1(ij).ne.icomp)then  ! if in another zone
            cogo(1)=surcog(IC1(ij),IE1(ij),1)
            cogo(2)=surcog(IC1(ij),IE1(ij),2)
            cogo(3)=surcog(IC1(ij),IE1(ij),3)

C Tighter tollerances for small surfaces.
            if(sna(icuriz,icuris).lt.1.0)then
              call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3),0.2,closecog)
            elseif(sna(icuriz,icuris).gt.10.0)then
              call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3),0.6,closecog)
            else
              call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3),0.4,closecog)
            endif
            vdist= crowxyz(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &        cogo(3))
          elseif(ic1(ij).eq.icomp)then  ! if in same zone
            if(icuris.eq.IE1(ij))then
              continue                  ! exclude itself
            else
              cogo(1)=surcog(IC1(ij),IE1(ij),1)
              cogo(2)=surcog(IC1(ij),IE1(ij),2)
              cogo(3)=surcog(IC1(ij),IE1(ij),3)

C Tighter tollerances for small surfaces.
              if(sna(icuriz,icuris).lt.1.0)then
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.2,closecog)
              elseif(sna(icuriz,icuris).gt.10.0)then
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.6,closecog)
              else
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.4,closecog)
              endif
              vdist= crowxyz(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3))
            endif
          endif
          if(sna(icuriz,icuris).lt.1.0)then
            CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),0.1,
     &        closea)
          elseif(sna(icuriz,icuris).gt.10.0)then
            CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),0.5,
     &        closea)
          else
            CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),0.3,
     &        closea)
          endif

C Also check angle between surfaces are within ANGCC degrees orientation
C for surfaces which are not horizontal.
          lnsa=lnblnk(sname(IC1(ij),IE1(ij)))
          call eclose(SPELV(IC1(ij),IE1(ij)),-90.0,1.0,testisfloor)
          call eclose(SPELV(IC1(ij),IE1(ij)),90.0,1.0,testisceiling)
          SELV=SPELV(icuriz,icuris)+SPELV(IC1(ij),IE1(ij))
          DAZI=SPAZI(icuriz,icuris)-SPAZI(IC1(ij),IE1(ij))
          DAZI=ABS(DAZI)
          call eclose(DAZI,180.0,ANGCC,closeaz)
          call eclose(SELV,0.0,ANGCC,closee)
C Debug.
C          write(6,*) ic,ij,selv,dazi,closea,closeaz,closee,
C     &      origisfloor,origisceiling,testisfloor,testisceiling,
C     &      closecog,closecog1m

C If all match accept or if area/azimuth/elevation set closeenough.
          if(closecog.and.closea.and.closeaz.and.closee)then
            closeenough=.true.  ! tight COG
          elseif(closecog.and.closea.and.closee)then

C If horizontal relax the azimuth test.
            if(origisfloor.and.testisceiling)then
              closeenough=.true.
            endif
            if(origisceiling.and.testisfloor)then
              closeenough=.true.
            endif
          endif

          if(closeenough)then
            lnsa=lnblnk(sname(IC1(ij),IE1(ij)))
            write(outs,'(4a,i4,a,f7.3,a,f6.2,a,f6.2,a,f4.2,a)')
     &        sname(IC1(ij),IE1(ij))(1:lnsa),' in ',
     &        zname(ic1(ij))(1:lnzname(ic1(ij))),
     &        ' conn index ',ij,
     &        ' area=',SNA(IC1(ij),IE1(ij)),' azim=',
     &        spazi(IC1(ij),IE1(ij)),
     &        ' elev=',spelv(IC1(ij),IE1(ij)),' @ dist ',vdist,
     &        ' match?'
            call edisp(iuout,outs)
            ifound=ifound+1   ! increment number neighbors found
            if(ifound.eq.1)then
              ifoundc=ij        ! remember 1st one
              write(choice1,'(4a,i4,a,f7.3,a,f4.2)')
     &          sname(IC1(ij),IE1(ij))(1:lnsa),':',
     &          zname(ic1(ij))(1:lnzname(ic1(ij))),
     &          ' connection ',ij,
     &          ' area=',SNA(IC1(ij),IE1(ij)),' dist=',vdist
            elseif(ifound.eq.2)then
              ifoundc2=ij        ! remember 2nd one
              write(choice2,'(4a,i4,a,f7.3,a,f4.2)')
     &          sname(IC1(ij),IE1(ij))(1:lnsa),':',
     &          zname(ic1(ij))(1:lnzname(ic1(ij))),
     &          ' connection ',ij,
     &          ' area=',SNA(IC1(ij),IE1(ij)),' dist=',vdist
            endif
          endif
  78    continue

C If nothing found try again with looser tollerances.
        if(ifound.eq.0)then
          call edisp(iuout,'Trying with looser tollerances.')
          do 79 ij=1,ncon
            closecog=.false.; closea=.false.; closee=.false.
            closeaz=.false.; closeenough=.false.
            if(ic1(ij).ne.icomp)then  ! if in another zone
              cogo(1)=surcog(IC1(ij),IE1(ij),1)
              cogo(2)=surcog(IC1(ij),IE1(ij),2)
              cogo(3)=surcog(IC1(ij),IE1(ij),3)

C Tighter tollerances for small surfaces.
              if(sna(icuriz,icuris).lt.1.0)then
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.3,closecog)
              elseif(sna(icuriz,icuris).gt.10.0)then
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.9,closecog)
              else
                call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3),0.6,closecog)
              endif
              vdist= crowxyz(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3))
            elseif(ic1(ij).eq.icomp)then  ! if in same zone
              if(icuris.eq.IE1(ij))then
                continue                  ! exclude itself
              else
                cogo(1)=surcog(IC1(ij),IE1(ij),1)
                cogo(2)=surcog(IC1(ij),IE1(ij),2)
                cogo(3)=surcog(IC1(ij),IE1(ij),3)

C Tighter tollerances for small surfaces.
                if(sna(icuriz,icuris).lt.1.0)then
                  call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &              cogo(3),0.3,closecog)
                elseif(sna(icuriz,icuris).gt.10.0)then
                  call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &              cogo(3),0.9,closecog)
                else
                  call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &              cogo(3),0.6,closecog)
                endif
                vdist= crowxyz(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &            cogo(3))
              endif
            endif

            if(sna(icuriz,icuris).lt.1.0)then
              CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),
     &          0.1,closea)
            elseif(sna(icuriz,icuris).gt.10.0)then
              CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),
     &          0.5,closea)
            else
              CALL ECLOSE(SNA(IC1(ij),IE1(ij)),sna(icuriz,icuris),
     &          0.3,closea)
            endif

C Also check angle between surfaces are within ANGCC degrees orientation
C for surfaces which are not horizontal.
            lnsa=lnblnk(sname(IC1(ij),IE1(ij)))
            call eclose(SPELV(IC1(ij),IE1(ij)),-90.0,1.0,testisfloor)
            call eclose(SPELV(IC1(ij),IE1(ij)),90.0,1.0,testisceiling)
            SELV=SPELV(icuriz,icuris)+SPELV(IC1(ij),IE1(ij))
            DAZI=SPAZI(icuriz,icuris)-SPAZI(IC1(ij),IE1(ij))
            DAZI=ABS(DAZI)
            call eclose(DAZI,180.0,3.0,closeaz)
            call eclose(SELV,0.0,3.0,closee)
C Debug.
C            write(6,*) ic,ij,selv,dazi,closea,closeaz,closee,
C     &      origisfloor,origisceiling,testisfloor,testisceiling,
C     &      closecog,closecog1m

C If all match accept or if area/azimuth/elevation set closeenough.
            if(closecog.and.closea.and.closeaz.and.closee)then
              closeenough=.true.  ! tight COG
            elseif(closecog.and.closea.and.closee)then

C If horizontal relax the azimuth test.
              if(origisfloor.and.testisceiling)then
                closeenough=.true.
              endif
              if(origisceiling.and.testisfloor)then
                closeenough=.true.
              endif
            endif

            if(closeenough)then
              lnsa=lnblnk(sname(IC1(ij),IE1(ij)))
              write(outs,'(4a,i4,a,f6.2,a,f6.2,a,f6.2,a,f4.2,a)')
     &          sname(IC1(ij),IE1(ij))(1:lnsa),' in ',
     &          zname(ic1(ij))(1:lnzname(ic1(ij))),
     &          ' conn index ',ij,
     &          ' area=',SNA(IC1(ij),IE1(ij)),' azim=',
     &          spazi(IC1(ij),IE1(ij)),
     &          ' elev=',spelv(IC1(ij),IE1(ij)),' @ dist ',vdist,
     &          ' match?'
              call edisp(iuout,outs)
              ifound=ifound+1   ! increment number neighbors found
              if(ifound.eq.1)then
                ifoundc=ij        ! remember 1st one
                write(choice1,'(4a,i4,a,f7.3,a,f4.2)')
     &            sname(IC1(ij),IE1(ij))(1:lnsa),':',
     &            zname(ic1(ij))(1:lnzname(ic1(ij))),
     &            ' index ',ij,' area=',SNA(IC1(ij),IE1(ij)),
     &            ' d=',vdist
              elseif(ifound.eq.2)then
                ifoundc2=ij        ! remember 2nd one
                write(choice2,'(4a,i4,a,f7.3,a,f4.2)')
     &            sname(IC1(ij),IE1(ij))(1:lnsa),':',
     &            zname(ic1(ij))(1:lnzname(ic1(ij))),
     &            ' index ',ij,
     &            ' area=',SNA(IC1(ij),IE1(ij)),' d=',vdist
              endif
            endif
  79      continue
        endif
        if(ifound.ge.1)then    ! at least one possible match was found
          if(ifound.eq.1)then
            IIC3=ifoundc    ! if only one set index
          elseif(ifound.eq.2)then
            CALL EASKABC('Select one:',':',choice1,
     &        choice2,'cancel',INVT,nbhelp)
            if(INVT.eq.1)then
              IIC3=ifoundc
            elseif(INVT.eq.2)then
              IIC3=ifoundc2
            elseif(INVT.eq.3)then
              IIC3=0
            endif
          else
            IIC3=0
            CALL EASKI(IIC3,' ',
     &        'Connection number of matching surface (zero=skip)?',
     &        0,'F',ncon,'F',1,'connection index',IERI,nbhelp)
          endif
          if(IIC3.gt.0)then
            write(outs,'(3a)') 'Make thermophysical connection with ',
     &        sname(IC1(iic3),IE1(iic3)),'?'
            updoth=.false.
            CALL EASKOK(' ',outs,updoth,nbhelp)
            if(updoth)then
              iz=ic1(iic3); iso=ie1(iic3) ! decode other zone and surf
              ico=iic3
              SOTHER(ICOMP,IS,1)='ANOTHER'
              write(SOTHER(ICOMP,IS,2),'(i3.3)') iz
              write(SOTHER(ICOMP,IS,3),'(i3.3)') iso
              ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO
              IC1(ICO)=IZ; IE1(ICO)=ISO; ICT(ICO)=3
              IC2(ICO)=icuriz; IE2(ICO)=icuris
              TSOTH1='ANOTHER'
              write(TSOTH2,'(i3.3)') icuriz
              write(TSOTH3,'(i3.3)') icuris

C If current SUSE is 'WALL', reset to 'PARTN'.
              if(SUSE(ICOMP,is,1)(1:4).eq.'WALL')then
                SUSE(ICOMP,is,1)='PARTN'
                SUSE(ICOMP,is,2)='-'
              endif

C Reconcile the surface attributes between the focus surface and the
C other surface. The returned temporary variables TOSMLCN,TOUSE1, TSOTH1 etc.
C will be dealt with in the logic following line 5615.
              call reconcileattributes(icomp,is,SN,iz,iso,showother,
     &           updoth,TOSMLCN,TOOPT,TOUSE1,TOUSE2,IISSMLCI)
              showother=.true.
              updoth=.true.
              CALL EMKCFG('-',IER)  ! update connections file.
            else
              updoth=.false.
              return       ! user canceled so jump back
            endif
          else
            return         ! user declined
          endif
        else
          return           ! nothing was found go back to the main menu
        endif

      ENDIF
      MODGEO=.TRUE.

C Finally update the system configuration file if not browsing.
      if(cfgok)then
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C If user requested other side to be updated do this as well. Read
C in the zone pointed to by IC2, update SOTHER (of index IE2 in the
C other zone) and write out that zones geometry.
        if(updoth)then
          lnicoth=lnblnk(SMLCN(IC2(IC),IE2(IC)))
          write(outs,'(6a)')'Updating `other side` composition of ',
     &      SN(1:lnblnk(SN)),' from ',SMLCN(IC2(IC),IE2(IC))(1:lnicoth),
     &      ' > ',TOSMLCN
          if(SMLCN(IC2(ic),IE2(ic))(1:lnicoth).ne.
     &       TOSMLCN(1:lnblnk(TOSMLCN)))then
            call usrmsg(outs,' ','P')
          endif

C If the other side is in a different zone then do read and write.
          if(IC2(IC).ne.icomp)then
            CALL ESCZONE(ICOMP)
            call eclose(gversion(IC2(IC)),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IFIL+2,LGEOM(IC2(IC)),IC2(IC),0,iuout,IER)
            else
              call egomin(IFIL+2,LGEOM(IC2(IC)),IC2(IC),0,0,iuout,IER)
            endif

C Assign new MLC name, USE and connexion info to the icoth surface.
            if(iissmlci.ne.0)then
              SMLCN(IC2(IC),IE2(IC))=TOSMLCN
              smlcindex(IC2(IC),IE2(IC))=iissmlci  ! update if known
            endif
            if(TOOPT(1:2).ne.'  ')then
              write(SOTF(IC2(ic),IE2(ic)),'(a)') TOOPT
            endif
            SUSE(IC2(ic),IE2(ic),1)=TOUSE1
            SUSE(IC2(ic),IE2(ic),2)=TOUSE2
            SOTHER(IC2(ic),IE2(ic),1)=TSOTH1
            SOTHER(IC2(ic),IE2(ic),2)=TSOTH2
            SOTHER(IC2(ic),IE2(ic),3)=TSOTH3

            call eclose(gversion(IC2(IC)),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(IC2(IC)) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(IC2(IC)),IC2(IC),iuout,3,IER)
            IF(IER.NE.0)CALL USRMSG(' ',
     &        'Problem updating other surface attribute.','W')

C Read current zone back in and then update the configuration file.
            CALL ERCZONE(ICOMP)
            call usrmsg(' ',' ','-')
            updoth=.false.
          else                                           ! In same zone so assign and write.
            if(iissmlci.ne.0)then
              SMLCN(IC2(IC),IE2(IC))=TOSMLCN               ! Update smlcindex if known.
              smlcindex(IC2(IC),IE2(IC))=iissmlci
            endif
            SOTHER(IC1(icoth),IE1(icoth),1)='ANOTHER'
            write(SOTHER(IC1(icoth),IE1(icoth),2),'(i3.3)') IC2(icoth)
            write(SOTHER(IC1(icoth),IE1(icoth),3),'(i3.3)') IE2(icoth)
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            call usrmsg(' ',' ','-')
            updoth=.false.
          endif
        endif
        CALL EMKCFG('-',IER)
      endif

      return
      end  ! of specify_other_face


C ******************** EDITUSE ********************
C Edituse sets the value of SUSE based on context.
C   integer iz is the current zone
C   integer isur is the current surface
C   integer icon is the current connection
C   guesstype (char 24) provides the context so that a likely
C     subset of choices can be presented.

      subroutine edituse(iz,isur,icon,guesstype)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "sbem.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      CHARACTER outs*124
      character TOUSE1*12,TOUSE2*12  ! to remember other side use

      character guesstype*24  ! for context of surface addition
      dimension iuseopt(32)
      character*32 useopt(25)
      character*72 ltmp
      character SN*12
      integer IRT  ! for radio button
      logical changed,ok,newgeo ! to detect change
      logical showother,updoth
      dimension IVALS(10)

      helpinsub='edgeo'  ! set for subroutine

C Set local working variables.
      is=isur; ic=icon
      write(SN,'(a12)') SNAME(IZ,IS)
      changed=.false.; updoth=.false.
      if(ICT(icon).eq.3)then
        showother=.true.
      else
        showother=.false.
      endif

C Set use type to a subset of all usage if UK NCM model
C This superceeds the value of guesstype that was set in
C the calling code so inform the user.
      if(ISBEM.EQ.2)then
        if(guesstype(1:8).ne.'notional')then
          call edisp(iuout,'Switching to UK notional surface uses.')
        endif
        GUESSTYPE='notional'
      endif

C Tell the user the current use of the surface.
      write(outs,'(6a)') 'The current use/semantic of surface ',
     &  sname(iz,isur),' is ',SUSE(iz,isur,1),' and ',
     &  SUSE(iz,isur,2)
      call edisp(iuout,outs)

      write(outs,'(3a)') 'The use/semantic options for surface ',
     &  sname(iz,isur),' are...'
      call edisp(iuout,outs)

C Help for the various choices.
      helptopic='surface_use_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(guesstype(1:8).eq.'any_type'.or.guesstype(1:2).eq.'- ')then

C Setup array of menu strings.
        useopt(1) ='DOOR NORMAL (not code compliant)'
        useopt(2) ='DOOR PERSONEL (code compliant)  '
        useopt(3) ='BALCONY                         '
        useopt(4) ='REVEAL (at window or door)      '
        useopt(5) ='FICTitious (low mass & transp)  '
        useopt(6) ='FLOOR EXTGRND (extern or ground)'
        useopt(7) ='FLOOR OTHER (not code compliant)'
        useopt(8) ='FRAME FACADE (code complient)   '
        useopt(9) ='FRAME OTHER (not code compliant)'
        useopt(10)='FURNI - (mass within zone)      '
        useopt(11)='ITEQUIP - (servers NATS)        '
        useopt(12)='GRILL - (small inlet/extr/vent) '
        useopt(13)='PARTN - (inside wall/ceil/floor)'
        useopt(14)='ROOF PITCHED (>20deg slope)     '
        useopt(15)='ROOF FLAT (0deg-20deg slope)    '
        useopt(16)='PLANTS (vegitation <future use>)'
        useopt(17)='STRUCture - (heavy mass in zone)'
        useopt(18)='WALL - (part of the facade)     '
        useopt(19)='WINDOW  (facade code complient) '
        useopt(20)='WINDOW DISPLAY (code complient) '
        useopt(21)='WINDOW skylight (code complient)'
        useopt(22)='WINDOW OTHER (not code complnt) '
        useopt(23)='FIXTURE - (lighting fixture)    '
        useopt(24)='FIXTURE IES or Rad diffuse fixt '
        useopt(25)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,25,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.
        if(IOS.eq.1)then
          iuopt=1  ! DOOR NORMAL
        elseif(IOS.eq.2)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.3)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.4)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.5)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.6)then
          iuopt=6  ! FLOOR EXTGRND
        elseif(IOS.eq.7)then
          iuopt=7  ! FLOOR OTHER
        elseif(IOS.eq.8)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.9)then
          iuopt=9  ! FRAME OTHER
        elseif(IOS.eq.10)then
          iuopt=10 ! FURNI
        elseif(IOS.eq.11)then
          iuopt=11 ! ITEQUIP
        elseif(IOS.eq.12)then
          iuopt=12 ! GRILL
        elseif(IOS.eq.13)then
          iuopt=13 ! PARTN
        elseif(IOS.eq.14)then
          iuopt=14 ! ROOF PITCHED
        elseif(IOS.eq.15)then
          iuopt=15 ! ROOF FLAT
        elseif(IOS.eq.16)then
          iuopt=16 ! PLANTS
        elseif(IOS.eq.17)then
          iuopt=17 ! STRUCture
        elseif(IOS.eq.18)then
          iuopt=18 ! WALL
        elseif(IOS.eq.19)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.20)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.21)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.22)then
          iuopt=22 ! WINDOW OTHER
        elseif(IOS.eq.23)then
          iuopt=23 ! FIXTURE
        elseif(IOS.eq.24)then
          iuopt=24 ! FIXTURE IES
        elseif(IOS.eq.25)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:8).eq.'notional')then

C Setup array of menu strings.
        useopt(1) ='DOOR PERSONEL'
        useopt(2) ='BALCONY'
        useopt(3) ='REVEAL'
        useopt(4) ='ROOF PITCHED (>20deg slope)     '
        useopt(5) ='ROOF FLAT (0deg-20deg slope)    '
        useopt(6) ='WALL'
        useopt(7) ='WINDOW NORMAL'
        useopt(8) ='WINDOW DISPLAY '
        useopt(9) ='WINDOW SKYLIGHT'
        useopt(10)='WINDOW FRAME'
        useopt(11)='FLOOR (foundation/ground) '
        useopt(12)='something else'

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,12,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.
        if(IOS.eq.1)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.2)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.3)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.4)then
          iuopt=14 ! ROOF PITCHED
        elseif(IOS.eq.5)then
          iuopt=15 ! ROOF FLAT
        elseif(IOS.eq.6)then
          iuopt=18 ! WALL
        elseif(IOS.eq.7)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.8)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.9)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.10)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.11)then
          iuopt=6  ! FLOOR EXTGRND
        elseif(IOS.eq.12)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:18).eq.'window_grill_frame')then

C Setup array of menu strings.
        useopt(1) ='FICTitious (not code compliant) '
        useopt(2) ='FRAME FACADE (code compliant)   '
        useopt(3) ='FRAME OTHER (not code compliant)'
        useopt(4) ='GRILL (small inlet/extr/vent)   '
        useopt(5) ='WINDOW (facade code compliant)  '
        useopt(6) ='WINDOW DISPLAY (code compliant) '
        useopt(7) ='WINDOW skylight (code compliant)'
        useopt(8) ='WINDOW reveal                   '
        useopt(9) ='-   -  (something else)         '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,9,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.

        if(IOS.eq.1)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.2)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.3)then
          iuopt=9  ! FRAME OTHER
        elseif(IOS.eq.4)then
          iuopt=12 ! GRILL
        elseif(IOS.eq.5)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.6)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.7)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.8)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.9)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:16).eq.'door_grill_frame')then

C Setup array of menu strings.
        useopt(1) ='DOOR NORMAL (not code compliant)'
        useopt(2) ='DOOR PERSONEL (code compliant)  '
        useopt(3) ='BALCONY                         '
        useopt(4) ='REVEAL                          '
        useopt(5) ='FICTitious (not code compliant) '
        useopt(6) ='FRAME FACADE (code compliant)   '
        useopt(7) ='FRAME OTHER (not code compliant)'
        useopt(8) ='GRILL (small inlet/extr/vent)   '
        useopt(9) ='WINDOW  (facade code compliant) '
        useopt(10)='WINDOW DISPLAY (code compliant) '
        useopt(11)='WINDOW skylight (code compliant)'
        useopt(12)='WINDOW OTHER (not code complnt) '
        useopt(13)='-   -  (something else)         '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,13,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.

        if(IOS.eq.1)then
          iuopt=1  ! DOOR NORMAL
        elseif(IOS.eq.2)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.3)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.4)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.5)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.6)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.7)then
          iuopt=9  ! FRAME OTHER
        elseif(IOS.eq.8)then
          iuopt=12 ! GRILL
        elseif(IOS.eq.9)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.10)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.11)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.12)then
          iuopt=22 ! WINDOW OTHER
        elseif(IOS.eq.13)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:8).eq.'vertical')then

        useopt(1) ='DOOR NORMAL (not code compliant)'
        useopt(2) ='DOOR PERSONEL (code compliant)  '
        useopt(3) ='BALCONY                         '
        useopt(4) ='REVEAL (at window or door)      '
        useopt(5) ='FICTitious (not code compliant) '
        useopt(6) ='FURNI - (mass within zone)      '
        useopt(7) ='PARTN - (inside wall/ceil/floor)'
        useopt(8) ='STRUCture - (mass within zone)  '
        useopt(9) ='WALL - (part of the facade)     '
        useopt(10)='-   -  (something else)         '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,10,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.

        if(IOS.eq.1)then
          iuopt=1  ! DOOR NORMAL
        elseif(IOS.eq.2)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.3)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.4)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.5)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.6)then
          iuopt=10 ! FURNI
        elseif(IOS.eq.7)then
          iuopt=13 ! PARTN
        elseif(IOS.eq.8)then
          iuopt=17 ! STRUCture
        elseif(IOS.eq.9)then
          iuopt=18 ! WALL
        elseif(IOS.eq.10)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:10).eq.'horizontal')then

        useopt(1) ='FICTitious (not code compliant) '
        useopt(2) ='FLOOR EXTGRND (extr or ground)  '
        useopt(3) ='FLOOR OTHER (not code compliant)'
        useopt(4) ='FURNI - (mass within zone)      '
        useopt(5) ='PARTN - (inside wall/ceil/floor)'
        useopt(6) ='STRUCture - (mass within zone)  '
        useopt(7) ='REVEAL                          '
        useopt(8) ='-   -  (something else)         '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,7,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(showother)updoth=.true.

        if(IOS.eq.1)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.2)then
          iuopt=6  ! FLOOR EXTGRND
        elseif(IOS.eq.3)then
          iuopt=7  ! FLOOR OTHER
        elseif(IOS.eq.4)then
          iuopt=10 ! FURNI
        elseif(IOS.eq.5)then
          iuopt=13 ! PARTN
        elseif(IOS.eq.6)then
          iuopt=17 ! STRUCture
        elseif(IOS.eq.7)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.8)then
          iuopt=25 ! somthing else
        endif

      endif

C Common editing of variants - doors:
C If a partition then update the other surface use attribute.
      if(iuopt.eq.1)then
        SUSE(iz,IS,1)='DOOR'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='DOOR'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.2)then
        SUSE(iz,IS,1)='P-DOOR'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='P-DOOR'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.3)then
        SUSE(iz,IS,1)='BALCONY'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='BALCONY'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.4)then
        SUSE(iz,IS,1)='REVEAL'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='REVEAL'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      endif
      if(iuopt.eq.1.or.iuopt.eq.2)then
        helptopic='door_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface DOOR USE attribute air leakage:',
     &    'closed','undercut','open','bidirectional',
     &    'bi-dir ajar','undefined','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(iz,IS,2)='CLOSED'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CLOSED'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.2)then
          SUSE(iz,IS,2)='UNDERCUT'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='UNDERCUT'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.3)then
          SUSE(iz,IS,2)='OPEN'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='OPEN'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.4)then
          SUSE(iz,IS,2)='BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.5)then
          SUSE(iz,IS,2)='AJAR-BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='AJAR-BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.6)then
          SUSE(iz,IS,2)='-'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.7)then
          continue
        endif
      endif

C If a partition then update the other surface use attributes.
      if(iuopt.eq.5)then
        SUSE(iz,IS,1)='FICT'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='FICT'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif

        helptopic='fict_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface FICT USE air leakage choices:',
     &    'closed','crack','open','bidirectional',
     &    'bi-dir ajar','undefined','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(iz,IS,2)='CLOSED'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CLOSED'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.2)then
          SUSE(iz,IS,2)='CRACK'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CRACK'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.3)then
          SUSE(iz,IS,2)='OPEN'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='OPEN'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.4)then
          SUSE(iz,IS,2)='BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.5)then
          SUSE(iz,IS,2)='AJAR-BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='AJAR-BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.6)then
          SUSE(iz,IS,2)='-'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.7)then
          continue
        endif
      endif

      if(iuopt.eq.6)then
        SUSE(iz,IS,1)='FLOOR'
        SUSE(iz,IS,2)='EXTGRND'
      elseif(iuopt.eq.7)then
        SUSE(iz,IS,1)='FLOOR'
        SUSE(iz,IS,2)='- '
      endif

      if(iuopt.eq.8)then
        SUSE(iz,IS,1)='F-FRAME'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='F-FRAME'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.9)then
        SUSE(iz,IS,1)='FRAME'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='FRAME'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      endif

      if(iuopt.eq.8.or.iuopt.eq.9)then

C If a partition then update the other surface use attributes.
        helptopic='frame_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface FRAME USE air leakage choices:',
     &    'undefined','crack','trickle vent','cancel',
     &    ' ',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(iz,IS,2)='-'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.2)then
          SUSE(iz,IS,2)='CRACK'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CRACK'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.3)then
          SUSE(iz,IS,2)='VENT'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='VENT'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.4)then
          continue
        endif
      endif

      if(iuopt.eq.10)then
        SUSE(iz,IS,1)='FURNI'
        SUSE(iz,IS,2)='- '
      elseif(iuopt.eq.11)then
        SUSE(iz,IS,1)='ITEQUIP'
        SUSE(iz,IS,2)='- '
      elseif(iuopt.eq.12)then
        SUSE(iz,IS,1)='GRILL'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='GRILL'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
        helptopic='grill_use'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface GRILL USE air leakage choices:',
     &    'undefined','crack','mech vent inlet','mech vent extract',
     &    'natural air flow','duct','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(iz,IS,2)='-'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.2)then
          SUSE(iz,IS,2)='CRACK'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CRACK'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.3)then       ! Other side to '-'.
          SUSE(iz,IS,2)='INLET'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.4)then
          SUSE(iz,IS,2)='EXTRACT'  ! Other side to '-'.
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.5)then
          SUSE(iz,IS,2)='OPEN'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='OPEN'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.6)then
          SUSE(iz,IS,2)='DUCT'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='DUCT'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.7)then
          continue
        endif

      elseif(iuopt.eq.13)then

C If a partition then update the other surface use attributes.
        SUSE(iz,IS,1)='PARTN'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='PARTN'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
        SUSE(iz,IS,2)='- '
        if(showother)then
          SUSE(IC2(icon),IE2(icon),2)='- '
          TOUSE2=SUSE(IC2(icon),IE2(icon),2)
        endif
      elseif(iuopt.eq.14)then
        SUSE(iz,IS,1)='ROOF'
        SUSE(iz,IS,2)='PITCHED'
      elseif(iuopt.eq.15)then
        SUSE(iz,IS,1)='ROOF'
        SUSE(iz,IS,2)='FLAT'
      elseif(iuopt.eq.16)then
        SUSE(iz,IS,1)='PLANTS'
        SUSE(iz,IS,2)='- '
      elseif(iuopt.eq.17)then
        SUSE(iz,IS,1)='STRUC'
        SUSE(iz,IS,2)='- '
      elseif(iuopt.eq.18)then
        SUSE(iz,IS,1)='WALL'
        SUSE(iz,IS,2)='- '
      endif

C If a partition then update the other surface use attributes.
      if(iuopt.eq.19)then
        SUSE(iz,IS,1)='C-WINDOW'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='C-WINDOW'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.20)then
        SUSE(iz,IS,1)='D-WINDOW'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='D-WINDOW'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.21)then
        SUSE(iz,IS,1)='S-WINDOW'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='S-WINDOW'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.22)then
        SUSE(iz,IS,1)='WINDOW'
        if(showother)then
          SUSE(IC2(icon),IE2(icon),1)='WINDOW'
          TOUSE1=SUSE(IC2(icon),IE2(icon),1)
        endif
      elseif(iuopt.eq.23)then
        SUSE(iz,IS,1)='FIXTURE'
        SUSE(iz,IS,2)='- '
      elseif(iuopt.eq.24)then

C Marking as associated with and IES data set. If there are
C some IES entities in the model present list. If none advise
C the user and set 2nd parameter to '-'.
        if(nbofies.eq.0)then
          call usrmsg('There are no known IES data sets. Set these',
     &      'up via [advanced optics] menu.','W')
          SUSE(iz,IS,1)='FIXTURE'
          SUSE(iz,IS,2)='- ';
        else
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ',' Known IES entities:',
     &      32,10,iesmenu,' ies list',IER,nbhelp)
          if(INPIC.EQ.0)then
            SUSE(iz,IS,1)='FIXTURE'
            SUSE(iz,IS,2)='- '
          else
            SUSE(iz,IS,1)='FIXTURE'
            write(SUSE(iz,IS,2),'(a)') iesname(ivals(1))
          endif
        endif
      elseif(iuopt.eq.25)then
        SUSE(iz,IS,1)='-'
      endif

      if(iuopt.eq.19.or.iuopt.eq.20.or.iuopt.eq.21.or.iuopt.eq.22)then
C If a partition then update the other surface use attributes.
        helptopic='window_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface WINDOW USE air leakage choices:',
     &    'undefined','crack','open','sash ','bidirectional',
     &    'bi-dir ajar','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(iz,IS,2)='-'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='-'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.2)then
          SUSE(iz,IS,2)='CRACK'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='CRACK'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.3)then
          SUSE(iz,IS,2)='OPEN'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='OPEN'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.4)then
          SUSE(iz,IS,2)='SASH'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='SASH'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.5)then
          SUSE(iz,IS,2)='BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.6)then
          SUSE(iz,IS,2)='AJAR-BIDIR'
          if(showother)then
            SUSE(IC2(icon),IE2(icon),2)='AJAR-BIDIR'
            TOUSE2=SUSE(IC2(icon),IE2(icon),2)
          endif
        elseif(IRT.eq.7)then
          continue
        endif
      endif
      if(iuopt.eq.25)then
        SUSE(iz,IS,1)='- '
        SUSE(iz,IS,2)='- '
      endif

C If changed then save the zone geometry. If a partition save
C both zones.
      if(changed)then
        CALL EASKOK(' ','Save changes to surface attributes?',
     &    OK,nbhelp)
        if(showother.and.updoth.and.ok)then
          CALL ESCZONE(IZ)
          write(outs,'(6a)')
     &      'Updating `other side` USE of ',
     &      SN(1:lnblnk(SN)),' to ',TOUSE1,' & ',TOUSE2
          call edisp(iuout,outs)
          call eclose(gversion(IC2(icon)),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IFIL+2,LGEOM(IC2(icon)),IC2(icon),0,
     &        iuout,IER)
          else
            call egomin(IFIL+2,LGEOM(IC2(icon)),IC2(icon),0,0,
     &        iuout,IER)
          endif

C Apply USE to other side but make an exception for GRILL. If
C it is INLET or EXTRACT set it to '-' on the other side. If it
C is CLOSED or '-' on this side it might be an INLET or EXTRACT on the
C other side so if that is the case do not change it.
          if(TOUSE1(1:5).eq.'GRILL'.and.TOUSE2(1:5).eq.'INLET')then
            SUSE(IC2(icon),IE2(icon),1)=TOUSE1  ! apply to other zone
            SUSE(IC2(icon),IE2(icon),2)='-'
          elseif(TOUSE1(1:5).eq.'GRILL'.and.
     &           TOUSE2(1:7).eq.'EXTRACT')then
            SUSE(IC2(icon),IE2(icon),1)=TOUSE1  ! apply to other zone
            SUSE(IC2(icon),IE2(icon),2)='-'
          elseif(TOUSE1(1:5).eq.'GRILL'.and.
     &           (TOUSE2(1:1).eq.'-'.or.TOUSE2(1:6).eq.'CLOSED'))then
            if(SUSE(IC2(icon),IE2(icon),1)(1:5).eq.'GRILL')then
              continue  ! don't overwrite the other side
            else
              SUSE(IC2(icon),IE2(icon),1)=TOUSE1  ! apply to other zone
              SUSE(IC2(icon),IE2(icon),2)=TOUSE2
            endif
          else
            SUSE(IC2(icon),IE2(icon),1)=TOUSE1  ! apply to other zone
            SUSE(IC2(icon),IE2(icon),2)=TOUSE2
          endif
          call eclose(gversion(IC2(icon)),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(IC2(icon)) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(IC2(icon)),IC2(icon),
     &      iuout,3,IER)
          IF(IER.NE.0)CALL USRMSG(
     &      'Problem updating other surface attribute... ',
     &      '(could not write other geometry file).','W')

C Read current zone data back in to commons.
          CALL ERCZONE(IZ)
          call usrmsg(' Updating other side...done.',' ','-')
          updoth=.false.
        endif
        LTMP=LGEOM(IZ)

C Update the current zone attributes.
        IF(OK)then
          call eclose(gversion(iz),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(iz) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LTMP,IZ,iuout,3,IER)
          CALL EMKCFG('-',IER)
        endif
      endif

      return
      end


C ************* SELECTUSE
C Selectuse gets two strings for surface use based on context.
C   guesstype (char 24) provides the context so that a likely
C     subset of choices can be presented.
      subroutine selectuse(guesstype,use1,use2)
#include "building.h"
#include "sbem.h"
#include "help.h"

C Parameters
      character guesstype*24  ! for context of surface uses
      character use1*12        ! general use type
      character use2*12        ! details of use

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      dimension iuseopt(32)
      character*32 useopt(25)
      logical changed

      helpinsub='edgeo'  ! set for subroutine

      changed=.false.

C Set use type to a subset of all usage if UK NCM model
C This superceeds the value of guesstype that was set in
C the calling code so inform the user.
      if(ISBEM.EQ.2)then
        if(guesstype(1:8).ne.'notional')then
          call edisp(iuout,'Switching to UK notional surface uses.')
        endif
        GUESSTYPE='notional'
      endif

C Help for the various choices.
      helptopic='surface_use_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(guesstype(1:8).eq.'any_type'.or.guesstype(1:2).eq.'- ')then

C Setup array of menu strings.
        useopt(1) ='DOOR NORMAL (not code compliant)'
        useopt(2) ='DOOR PERSONEL (code compliant)  '
        useopt(3) ='BALCONY                         '
        useopt(4) ='REVEAL (at window or door)      '
        useopt(5) ='FICTitious (low mass & transp)  '
        useopt(6) ='FLOOR EXTGRND (extern or ground)'
        useopt(7) ='FLOOR OTHER (not code compliant)'
        useopt(8) ='FRAME FACADE (code complient)   '
        useopt(9) ='FRAME OTHER (not code compliant)'
        useopt(10)='FURNI - (mass within zone)      '
        useopt(11)='ITEQUIP - (servers NATS)        '
        useopt(12)='GRILL - (small inlet/extr/vent) '
        useopt(13)='PARTN - (inside wall/ceil/floor)'
        useopt(14)='ROOF PITCHED (>20deg slope)     '
        useopt(15)='ROOF FLAT (0deg-20deg slope)    '
        useopt(16)='PLANTS (vegitation <future use>)'
        useopt(17)='STRUCture - (heavy mass in zone)'
        useopt(18)='WALL - (part of the facade)     '
        useopt(19)='WINDOW  (facade code compliant) '
        useopt(20)='WINDOW DISPLAY (code compliant) '
        useopt(21)='WINDOW skylight (code compliant)'
        useopt(22)='WINDOW OTHER (not code complnt) '
        useopt(23)='FIXTURE - (lighting fixture)    '
        useopt(24)='FIXTURE IES (IES lighting fixt) '
        useopt(25)='-   -  (something else)        '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,25,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.

        if(IOS.eq.1)then
          iuopt=1  ! DOOR NORMAL
        elseif(IOS.eq.2)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.3)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.4)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.5)then
          iuopt=5  ! FICTitious
        elseif(IOS.eq.6)then
          iuopt=6  ! FLOOR EXTGRND
        elseif(IOS.eq.7)then
          iuopt=7  ! FLOOR OTHER
        elseif(IOS.eq.8)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.9)then
          iuopt=9  ! FRAME OTHER
        elseif(IOS.eq.10)then
          iuopt=10 ! FURNI
        elseif(IOS.eq.11)then
          iuopt=11 ! ITEQUIP
        elseif(IOS.eq.12)then
          iuopt=12 ! GRILL
        elseif(IOS.eq.13)then
          iuopt=13 ! PARTN
        elseif(IOS.eq.14)then
          iuopt=14 ! ROOF PITCHED
        elseif(IOS.eq.15)then
          iuopt=15 ! ROOF FLAT
        elseif(IOS.eq.16)then
          iuopt=16 ! PLANTS
        elseif(IOS.eq.17)then
          iuopt=17 ! STRUCture
        elseif(IOS.eq.18)then
          iuopt=18 ! WALL
        elseif(IOS.eq.19)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.20)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.21)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.22)then
          iuopt=22 ! WINDOW OTHER
        elseif(IOS.eq.23)then
          iuopt=23 ! FIXTURE
        elseif(IOS.eq.24)then
          iuopt=24 ! FIXTURE IES
        elseif(IOS.eq.25)then
          iuopt=25 ! somthing else
        endif

      elseif(guesstype(1:8).eq.'notional')then

C Setup array of menu strings.
        useopt(1) ='DOOR PERSONEL'
        useopt(2) ='BALCONY'
        useopt(3) ='REVEAL'
        useopt(4) ='ROOF PITCHED (>20deg slope)     '
        useopt(5) ='ROOF FLAT (0deg-20deg slope)    '
        useopt(6) ='WALL'
        useopt(7) ='WINDOW NORMAL'
        useopt(8) ='WINDOW DISPLAY '
        useopt(9) ='WINDOW SKYLIGHT'
        useopt(10)='WINDOW FRAME'
        useopt(11)='something else'

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,11,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return
        changed=.true.
        if(IOS.eq.1)then
          iuopt=2  ! DOOR PERSONEL
        elseif(IOS.eq.2)then
          iuopt=3  ! BALCONY
        elseif(IOS.eq.3)then
          iuopt=4  ! REVEAL
        elseif(IOS.eq.4)then
          iuopt=14 ! ROOF PITCHED
        elseif(IOS.eq.5)then
          iuopt=15 ! ROOF FLAT
        elseif(IOS.eq.6)then
          iuopt=18 ! WALL
        elseif(IOS.eq.7)then
          iuopt=19 ! WINDOW
        elseif(IOS.eq.8)then
          iuopt=20 ! WINDOW DISPLAY
        elseif(IOS.eq.9)then
          iuopt=21 ! WINDOW skylight
        elseif(IOS.eq.10)then
          iuopt=8  ! FRAME FACADE
        elseif(IOS.eq.11)then
          iuopt=25 ! somthing else
        endif

      endif

      if(iuopt.eq.1)then
        USE1='DOOR'
      elseif(iuopt.eq.2)then
        USE1='P-DOOR'
      elseif(iuopt.eq.3)then
        USE1='BALCONY'
      elseif(iuopt.eq.4)then
        USE1='REVEAL'
      endif

C Do not need second tag if generating notional model.
C Add USE2 'PERIM-CR'
      if(guesstype(1:8).eq.'notional')goto 55
      if(iuopt.eq.1.or.iuopt.eq.2)then
        helptopic='door_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface DOOR USE air leakage choices:',
     &    'closed','undercut','open','bidirectional',
     &    'bi-dir ajar','undefined','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='UNDERCUT'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='BIDIR'
        elseif(IRT.eq.5)then
          USE2='AJAR-BIDIR'
        elseif(IRT.eq.6)then
          USE2='-'
        elseif(IRT.eq.7)then
          continue
        endif
      endif
 55   if(iuopt.eq.5)then
        USE1='FICT'

        helptopic='fict_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface FICT USE air leakage choices:',
     &    'closed','crack','open','bidirectional',
     &    'bi-dir ajar','undefined','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='BIDIR'
        elseif(IRT.eq.5)then
          USE2='AJAR-BIDIR'
        elseif(IRT.eq.6)then
          USE2='-'
        elseif(IRT.eq.7)then
          continue
        endif
      endif
      if(iuopt.eq.6)then
        USE1='FLOOR'
        USE2='EXTGRND'
      elseif(iuopt.eq.7)then
        USE1='FLOOR'
        USE2='- '
      endif

      if(iuopt.eq.8)then
        USE1='F-FRAME'
      elseif(iuopt.eq.9)then
        USE1='FRAME'
      endif
      if(iuopt.eq.8.or.iuopt.eq.9)then
        helptopic='frame_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface FRAME USE air leakage choices:',
     &    'undefined','crack','trickle vent','cancel',
     &    ' ',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='-'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          continue
        endif
      endif

      if(iuopt.eq.10)then
        USE1='FURNI'; USE2='- '
      elseif(iuopt.eq.11)then
        USE1='ITEQUIP'; USE2='- '
      elseif(iuopt.eq.12)then
        helptopic='grill_use'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG('  ','Surface GRILL USE air leakage choices:',
     &    'undefined','crack','mech vent inlet','mech vent extract',
     &    'natural air flow','duct','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          USE1='GRILL'; USE2='-'
        elseif(IRT.eq.2)then
          USE1='GRILL'; USE2='CRACK'
        elseif(IRT.eq.3)then
          USE1='GRILL'; USE2='INLET'
        elseif(IRT.eq.4)then
          USE1='GRILL'; USE2='EXTRACT'
        elseif(IRT.eq.5)then
          USE1='GRILL'; USE2='OPEN'
        elseif(IRT.eq.6)then
          USE1='GRILL'; USE2='DUCT'
        elseif(IRT.eq.7)then
          continue
        endif
      elseif(iuopt.eq.13)then
        USE1='PARTN'; USE2='- '
      elseif(iuopt.eq.14)then
        USE1='ROOF'; USE2='PITCHED'
      elseif(iuopt.eq.15)then
        USE1='ROOF'; USE2='FLAT'
      elseif(iuopt.eq.16)then
        USE1='PLANTS'; USE2='- '
      elseif(iuopt.eq.17)then
        USE1='STRUC'; USE2='- '
      elseif(iuopt.eq.18)then
        USE1='WALL'; USE2='- '
      endif

      if(iuopt.eq.19)then
        USE1='C-WINDOW'
      elseif(iuopt.eq.20)then
        USE1='D-WINDOW'
      elseif(iuopt.eq.21)then
        USE1='S-WINDOW'
      elseif(iuopt.eq.22)then
        USE1='WINDOW'
      elseif(iuopt.eq.23)then
        USE1='FIXTURE'; USE2='- '
      elseif(iuopt.eq.24)then
        USE1='FIXTURE'; USE2='IES '
      elseif(iuopt.eq.25)then
        USE1='-'
      endif

C Do not need second tag if generating notional model
      if(guesstype(1:8).eq.'notional')goto 56
      if(iuopt.eq.19.or.iuopt.eq.20.or.iuopt.eq.21.or.iuopt.eq.22)then
        helptopic='window_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','Surface WINDOW USE air leakage choices:',
     &    'undefined','crack','open','sash ','bidirectional',
     &    'bi-dir ajar','cancel',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='-'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='SASH'
        elseif(IRT.eq.5)then
          USE2='BIDIR'
        elseif(IRT.eq.6)then
          USE2='AJAR-BIDIR'
        elseif(IRT.eq.7)then
          continue
        endif
      endif
56    if(iuopt.eq.25)then
        USE1='- '; USE2='- '
      endif
C      write(6,*) 'SELECTUSE noticed a change in SUSE ',
C     &  changed,USE1,USE2

      return
      end

C ************* reconcileattributes ************
C Reconcile surface attributes SUSE and MLC when users associate two
C surfaces as partitions.

      subroutine reconcileattributes(icomp,is,SN,iz,iso,showother,
     &   updoth,TOSMLCN,TOOPT,TOUSE1,TOUSE2,IISSMLCI)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

      integer icomp,is                ! focus zone and surface
      character SN*12                 ! focus surface name
      integer iz,iso                  ! other zone and surface
      logical showother,updoth        ! returned flags
      character TOSMLCN*32            ! pass back attributes for other side
      character TOOPT*32
      character TOUSE1*12, TOUSE2*12
      integer IISSMLCI
      character outs*124
      integer lnblnk

C Surface USE: check if there is a difference between the current
C and other surface use. If the same do nothing. If current is '-'
C and the other is in the following list then make it the same.
              if(SUSE(ICOMP,is,1)(1:1).eq.'-'.and.
     &           SUSE(IZ,iso,1)(1:1).eq.'-')then
                continue                                  ! Nothing to do.
              elseif(SUSE(ICOMP,is,1)(1:1).eq.'-'.and.    ! Use non-flow attributes
     &               SUSE(IZ,iso,1)(1:4).eq.'WALL'.or.    ! from the other side.
     &               SUSE(IZ,iso,1)(1:5).eq.'FLOOR'.or.
     &               SUSE(IZ,iso,1)(1:5).eq.'FURNI'.or.
     &               SUSE(IZ,iso,1)(1:7).eq.'ITEQUIP'.or.
     &               SUSE(IZ,iso,1)(1:5).eq.'PARTN'.or.
     &               SUSE(IZ,iso,1)(1:4).eq.'ROOF'.or.
     &               SUSE(IZ,iso,1)(1:5).eq.'STRUC'.or.
     &               SUSE(IZ,iso,1)(1:7).eq.'FIXTURE'.or.
     &               SUSE(IZ,iso,1)(1:6).eq.'PLANTS')then
                SUSE(ICOMP,is,1)=SUSE(IZ,iso,1)
                SUSE(ICOMP,is,2)=SUSE(IZ,iso,2)

              elseif(SUSE(ICOMP,is,1)(1:1).eq.'-'.and.    ! Use non-flow attributes.
     &               SUSE(IZ,iso,1)(1:4).eq.'DOOR'.or.    ! Flow attributes on other side.
     &               SUSE(IZ,iso,1)(1:6).eq.'P-DOOR'.or.
     &               SUSE(IZ,iso,1)(1:5).eq.'FRAME'.or.
     &               SUSE(IZ,iso,1)(1:7).eq.'F-FRAME'.or.
     &               SUSE(IZ,iso,1)(1:5).eq.'GRILL'.or.
     &               SUSE(IZ,iso,1)(1:6).eq.'WINDOW'.or.
     &               SUSE(IZ,iso,1)(1:8).eq.'D-WINDOW'.or.
     &               SUSE(IZ,iso,1)(1:8).eq.'S-WINDOW'.or.
     &               SUSE(IZ,iso,1)(1:8).eq.'C-WINDOW'.or.
     &               SUSE(IZ,iso,1)(1:4).eq.'FICT')then
                SUSE(ICOMP,is,1)=SUSE(IZ,iso,1)
                if(SUSE(IZ,iso,2)(1:5).eq.'CLOSED'.or.    ! Depending on 2nd attribute
     &             SUSE(IZ,iso,2)(1:8).eq.'UNDERCUT'.or.  ! also update the other surface.
     &             SUSE(IZ,iso,2)(1:4).eq.'OPEN'.or.
     &             SUSE(IZ,iso,2)(1:5).eq.'BIDIR'.or.
     &             SUSE(IZ,iso,2)(1:10).eq.'AJAR-BIDIR'.or.
     &             SUSE(IZ,iso,2)(1:5).eq.'CRACK'.or.
     &             SUSE(IZ,iso,2)(1:4).eq.'VENT'.or.
     &             SUSE(IZ,iso,2)(1:4).eq.'DUCT')then
                  SUSE(ICOMP,is,2)=SUSE(IZ,iso,2)
                endif
                if(SUSE(IZ,iso,2)(1:5).eq.'INLET'.or.     ! For mechanical set current to -
     &             SUSE(IZ,iso,2)(1:7).eq.'EXTRACT')then
                  SUSE(ICOMP,is,2)='-'
                endif

C If current SUSE is set and other surface is '-' then update other surface.
              elseif(SUSE(ICOMP,is,1)(1:4).eq.'WALL'.or.
     &               SUSE(ICOMP,is,1)(1:5).eq.'FLOOR'.or.
     &               SUSE(ICOMP,is,1)(1:5).eq.'FURNI'.or.
     &               SUSE(ICOMP,is,1)(1:7).eq.'ITEQUIP'.or.
     &               SUSE(ICOMP,is,1)(1:4).eq.'ROOF'.or.
     &               SUSE(ICOMP,is,1)(1:5).eq.'STRUC'.or.
     &               SUSE(ICOMP,is,1)(1:7).eq.'FIXTURE'.or.
     &               SUSE(ICOMP,is,1)(1:6).eq.'PLANTS')then
                if(SUSE(IZ,iso,1)(1:1).eq.'-')then
                  SUSE(IZ,iso,1)=SUSE(ICOMP,is,1)
                  SUSE(IZ,iso,2)=SUSE(ICOMP,is,2)
                endif

C If current SUSE has already been set to 'PARTN' and the other side
C is flow, relate then reasign current also as flow related.
              elseif(SUSE(ICOMP,is,1)(1:5).eq.'PARTN')then
                if(SUSE(IZ,iso,1)(1:1).eq.'-')then          ! Not set so copy PARTN.
                  SUSE(IZ,iso,1)=SUSE(ICOMP,is,1)
                  SUSE(IZ,iso,2)=SUSE(ICOMP,is,2)
                elseif(SUSE(IZ,iso,1)(1:4).eq.'DOOR'.or.    ! Flow attributes on other side.
     &             SUSE(IZ,iso,1)(1:6).eq.'P-DOOR'.or.
     &             SUSE(IZ,iso,1)(1:5).eq.'FRAME'.or.
     &             SUSE(IZ,iso,1)(1:7).eq.'F-FRAME'.or.
     &             SUSE(IZ,iso,1)(1:5).eq.'GRILL'.or.
     &             SUSE(IZ,iso,1)(1:6).eq.'WINDOW'.or.
     &             SUSE(IZ,iso,1)(1:8).eq.'D-WINDOW'.or.
     &             SUSE(IZ,iso,1)(1:8).eq.'S-WINDOW'.or.
     &             SUSE(IZ,iso,1)(1:8).eq.'C-WINDOW'.or.
     &             SUSE(IZ,iso,1)(1:4).eq.'FICT')then
                  SUSE(ICOMP,is,1)=SUSE(IZ,iso,1)
                  if(SUSE(IZ,iso,2)(1:5).eq.'CLOSED'.or.   ! Depending on 2nd attribute
     &               SUSE(IZ,iso,2)(1:8).eq.'UNDERCUT'.or. ! also update the other surface.
     &               SUSE(IZ,iso,2)(1:4).eq.'OPEN'.or.
     &               SUSE(IZ,iso,2)(1:5).eq.'BIDIR'.or.
     &               SUSE(IZ,iso,2)(1:10).eq.'AJAR-BIDIR'.or.
     &               SUSE(IZ,iso,2)(1:5).eq.'CRACK'.or.
     &               SUSE(IZ,iso,2)(1:4).eq.'VENT'.or.
     &               SUSE(IZ,is,2)(1:4).eq.'DUCT')then
                    SUSE(ICOMP,is,2)=SUSE(IZ,iso,2)
                  endif
                  if(SUSE(IZ,iso,2)(1:5).eq.'INLET'.or.    ! For mechanical set current to -
     &               SUSE(IZ,iso,2)(1:7).eq.'EXTRACT')then
                    SUSE(ICOMP,is,2)='-'
                  endif
                elseif(SUSE(IZ,iso,1)(1:4).eq.'WALL')then  ! PARTN trumps WALL.
                  SUSE(IZ,iso,1)=SUSE(ICOMP,is,1)
                  SUSE(IZ,iso,2)=SUSE(ICOMP,is,2)
                else                                       ! Non-flow other side so copy PARTN.
                  SUSE(IZ,iso,1)=SUSE(ICOMP,is,1)
                  SUSE(IZ,iso,2)=SUSE(ICOMP,is,2)
                endif

C If current surface SUSE is related to flow set the other face SUSE first
C attribute.
              elseif(SUSE(ICOMP,is,1)(1:4).eq.'DOOR'.or.
     &               SUSE(ICOMP,is,1)(1:6).eq.'P-DOOR'.or.
     &               SUSE(ICOMP,is,1)(1:5).eq.'FRAME'.or.
     &               SUSE(ICOMP,is,1)(1:7).eq.'F-FRAME'.or.
     &               SUSE(ICOMP,is,1)(1:5).eq.'GRILL'.or.
     &               SUSE(ICOMP,is,1)(1:6).eq.'WINDOW'.or.
     &               SUSE(ICOMP,is,1)(1:8).eq.'D-WINDOW'.or.
     &               SUSE(ICOMP,is,1)(1:8).eq.'S-WINDOW'.or.
     &               SUSE(ICOMP,is,1)(1:8).eq.'C-WINDOW'.or.
     &               SUSE(ICOMP,is,1)(1:4).eq.'FICT')then
                if(SUSE(IZ,iso,1)(1:1).eq.'-')then            ! Update other to flow.
                  SUSE(IZ,iso,1)=SUSE(ICOMP,is,1)
                  if(SUSE(ICOMP,is,2)(1:5).eq.'CLOSED'.or.    ! Depending on 2nd attribute
     &               SUSE(ICOMP,is,2)(1:8).eq.'UNDERCUT'.or.  ! also update the other surface.
     &               SUSE(ICOMP,is,2)(1:4).eq.'OPEN'.or.
     &               SUSE(ICOMP,is,2)(1:5).eq.'BIDIR'.or.
     &               SUSE(ICOMP,is,2)(1:10).eq.'AJAR-BIDIR'.or.
     &               SUSE(ICOMP,is,2)(1:5).eq.'CRACK'.or.
     &               SUSE(ICOMP,is,2)(1:4).eq.'VENT'.or.
     &               SUSE(ICOMP,is,2)(1:4).eq.'DUCT')then
                    SUSE(IZ,iso,2)=SUSE(ICOMP,is,2)
                  endif
                  if(SUSE(ICOMP,is,2)(1:5).eq.'INLET'.or.     ! For mechanical set other to -
     &               SUSE(ICOMP,is,2)(1:7).eq.'EXTRACT')then
                    SUSE(IZ,iso,2)='-'
                  endif
                endif
                showother=.true.  ! On re-display include info on SUSE.
              endif

C If logic fell through and SUSE on either side is a blank reset to '-'.
              if(SUSE(ICOMP,is,1)(1:1).eq.' ') SUSE(ICOMP,is,1)='-'
              if(SUSE(ICOMP,is,2)(1:1).eq.' ') SUSE(ICOMP,is,2)='-'
              if(SUSE(IZ,iso,1)(1:1).eq.' ') SUSE(IZ,iso,1)='-'
              if(SUSE(IZ,iso,2)(1:1).eq.' ') SUSE(IZ,iso,2)='-'
              TOUSE1=SUSE(IZ,iso,1)   ! Remember so can restore after zone read.
              TOUSE2=SUSE(IZ,iso,2)

C If this surface MLC is known, also check the other side construction matche
C and get isymindex for the other side.
              ii=smlcindex(icomp,is)
              if(ii.gt.0)then
                lnssmlc=lnblnk(SMLCN(IZ,iso))
                if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                  if(SMLCN(IZ,iso)(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcname(ii)))then
                    TOSMLCN= mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  elseif(SMLCN(IZ,iso)(1:4).eq.'UNKN')then
                    TOSMLCN= mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  else
                    TOSMLCN= mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  endif
                  iissmlci=ii
                elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition if db does not have a linked MLC - inform the user.
                  lnssmlc=lnblnk(SMLCN(icomp,is))
                  write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &              ' has a nonsymmetric construction ',
     &              SMLCN(icomp,is)(1:lnssmlc),'.'
                  call edisp(iuout,outs)
                  lnssmlc=lnblnk(SMLCN(IZ,iso))
                  write(outs,'(5a)') 'It faces ',
     &              SNAME(IZ,iso),' composed of ',
     &              SMLCN(IZ,iso)(1:lnssmlc),'.'
                  call edisp(iuout,outs)
                  TOSMLCN= SMLCN(IZ,iso)
                  iissmlci=smlcindex(IZ,iso)  ! leave it alone
                else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
                  ii=smlcindex(icomp,is)
                  lnssmlc=lnblnk(SMLCN(IZ,iso))
                  if(SMLCN(IZ,iso)(1:4).eq.'UNKN')then
                    TOSMLCN= mlcsymetric(ii)
                  elseif(SMLCN(IZ,iso)(1:lnssmlc).eq.
     &                   mlcsymetric(ii)(1:lnssmlc))then
                    TOSMLCN= mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  else
                    TOSMLCN= mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  endif
                  iissmlci=matsymindex(ii)  ! use returned value
                endif

C Mar the other zone surface attributes as known.
                showother=.true.
                updoth=.true.
              else

C This surface MLC is UNKNOWN, check other side. If symmetric 
C the same MLC name is applied to other zone. If nonsymmetric
C assign the other zones symmetric name to this zone. Otherwise
C leave the other MLC name the same and apply the symmetric name
C to this zone.
                iio=smlcindex(IZ,iso)
                if(iio.gt.0)then
                  lnssmlc=lnblnk(SMLCN(IZ,iso))
                  if(mlcsymetric(iio)(1:9).EQ.'SYMMETRIC')then
                    if(SMLCN(IZ,iso)(1:4).eq.'UNKN')then
                      iissmlci=0  ! Both surface and other unknown MLC.
                      TOSMLCN='UNKNOWN'
                      SMLCN(icomp,is)='UNKNOWN'
                      smlcindex(icomp,is)=0
                    else
                      TOSMLCN= mlcname(iio)
                      write(TOOPT,'(a)') mlcoptical(iio)
                      iissmlci=iio
                      SMLCN(icomp,is)=mlcname(iio)
                      smlcindex(icomp,is)=smlcindex(IZ,iso)
                    endif
                  elseif(mlcsymetric(iio)(1:12).EQ.'NONSYMMETRIC')then
                    TOSMLCN= mlcname(iio)
                    write(TOOPT,'(a)') mlcoptical(iio)
                    iissmlci=iio
                    SMLCN(icomp,is)=mlcsymetric(iio)
                    smlcindex(icomp,is)=matsymindex(iio)  ! use returned value
                  else
                    TOSMLCN= mlcname(iio)
                    SMLCN(icomp,is)=mlcsymetric(iio)
                    smlcindex(icomp,is)=matsymindex(iio)  ! use returned value
                    write(TOOPT,'(a)') mlcoptical(iio)
                    iissmlci=matsymindex(iio)  ! use returned value
                  endif
                endif
                showother=.true.
                updoth=.true.
              endif
      
      return
      end


C ************* EDVERT *************
C Edit vertex attributes in common block G1 via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.

      SUBROUTINE EDVERT(ITRU,ICOMP,MODGEO,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "epara.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/FILEP/IFIL
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      LOGICAL MODGEO,ok,match,matchver

C Tollerance for surface matching.
      real ANGCC               ! Angle between surfaces tolerance.
      real CACC                ! Tolerance between vertices.
      real DACC                ! Tolerance along line.
      real COGCC               ! Tolerance between surface COG.
      real SNACC               ! Tolerance between surface areas.
      integer IACC             ! Number of matching corners outside dist tolerance.
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC
      logical withinbox        ! True if point within bounding box.

      DIMENSION VERT(35),IVLST(MTV),ITEMP(MTV)
      DIMENSION VP(3),EQN(4),XX(MV),YY(MV),ZZ(MV)
      dimension IVALS(MCOM)
      DIMENSION ISASSO(MS)
      CHARACTER VERT*34,KEY*1,head*28,HOLD*36
      character outs*124,headv*48
      logical anotherone       ! Signal that another point will be requested.
      integer IRT              ! For radio button.
      integer MVERT,IVERT      ! Max items and current menu item.
      integer INPICK           ! Use with epkvert.
      integer iwhich1,lastv1   ! For 1st and 2nd user selections.
      integer iwhich2,lastv2
      integer iwhich3
      integer  iclkok          ! Allow jump to previous or next vertex.
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      real CX,CY,CZ            ! XYZ of selected verticies
      dimension CX(10),CY(10),CZ(10)
      integer im,k,mnulen      ! Multi-column lines.
      logical odd              ! Display of vertex info.
      logical newgeo           ! Test if new/old geometry file.
      logical writeagain       ! In case geometry file needs updating.
      logical exclude_surf     ! Intersection test excludes a surace.

      helpinsub='edgeo'        ! Set for subroutine.

      V1=1.0; AZ=0.0; EL=0.0
      x3=0.0; y3=0.0; z3=0.0
      anotherone=.false.; MODGEO=.false.
      iwhich=0; iwhich1=0; iwhich2=0
      lastv1=0; lastv2=0
      MODIFYVIEW=.true.

C Detect the version of the current file.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Write vertices with minimal white space comma separated.
C See if an even or odd number of items in list. Do this
C on first entry to the vertex editing facility.
      im=MOD(NZTV(icomp),2)
      odd=.false.
      if(im.eq.1) odd=.true.
      call edisp(itru,' Vertices ( index X Y Z) in the zone...')
      if(NZTV(icomp).lt.20)then
        DO 960 I = 1,NZTV(icomp)
          WRITE(outs,'(a,i4,3F10.4)')'*vertex ',I,
     &      X(I),Y(I),Z(I)
          call edisp(itru,outs)
960     CONTINUE
      else
        MNULEN=(NZTV(icomp)/2)
        DO 193 K=1,MNULEN
          WRITE(outs,'(a,i4,3f10.4,a,i4,3f10.4)') 'vertex ',k,
     &      X(k),Y(k),Z(k),'   vertex ',K+MNULEN,
     &      X(K+MNULEN),Y(K+MNULEN),Z(K+MNULEN)
          call edisp(itru,outs)
  193   CONTINUE

C Put odd vertex in right column.
        if(odd)then
          WRITE(outs,'(46x,a,i4,3f10.4)') 'vertex ',NZTV(icomp),
     &      X(NZTV(icomp)),Y(NZTV(icomp)),Z(NZTV(icomp))
         call edisp(itru,outs)
        endif
      endif

C Initialise zone vertex menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=6
      ILEN=NZTV(icomp)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NZTV(icomp)
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,L,X(L),Y(L),Z(L)
   14     FORMAT(A,I4,3F9.3)
        ENDIF
   10 CONTINUE

      VERT(1)    =' Index |X-coord|Y-coord|Z-coord'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='+ add/delete/copy/replicate/edit '
      VERT(M+3)  ='^ transforms                     '
      VERT(M+4)  ='! browse surface topology        '
      VERT(M+5)  ='? help                           '
      VERT(M+6)  ='- exit menu                      '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL redraw(IER)

C Help text for this menu.
      helptopic='surface_vertex_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(3A)')' Vertices in ',zname(ICOMP)
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        helptopic='surface_vertex_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('configuration section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        CALL VERINFO(ICOMP,ITRU)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Vertex transforms (start).
        idno=2
        call MENUATOL(' ',
     &   'Vertex Transforms','a vertices along a line',
     &   'b distance between two vertices',
     &   'c vertex @ angle & distance',
     &   'd angle between two lines','e intersect of a line & plane',
     &   'f intersect of LINES & plane',
     &   'g distance from vertex to a line',
     &   'h align vertex with a line','i find close vertices',
     &   'j move vertices along a line','k via offset from lower left',
     &   'l intersect from angle & dist',ino,idno,nbhelp)

C Based on users selection or jump back point for 'do another one'.
 243    if(ino.eq.0)then
          continue
        elseif(ino.eq.1)then

C Vertices along a line between two verts. Uses common G1 for the
C current zone.  The index to rememeber two vertices selected
C (iwhich1 and iwhich2) might be overwritten in latter parts of
C the code. Also hold them as lastv1 and lastv2.
          if(anotherone)then
            inpick=2
            iwhich1=lastv1   ! use rememebered 1st vertex
            iwhich2=lastv2
          else
            inpick=2
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Define Line',
     &        'Select two vertices to define line.',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.2)then
              iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
              iwhich2=IVLST(2); lastv2=iwhich2
            else
              goto 3
            endif
          endif

C Find distance and report.
          tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &      Y(IWHICH2),Z(IWHICH2))
          write(outs,'(a,i3,a,i3,a,f9.4)') ' Distance between v ',
     &      IWHICH1,' & v ',IWHICH2,' =',tdis
          call edisp(itru,outs)

          CALL EASKR(vdis,' ','Distance along line?',0.001,
     &      'F',99.999,'W',0.1,'dist along line',IER,nbhelp)

C Use ratio calculation.
          r2 = tdis - vdis
          r1 = vdis
          x3 = ((r2 * X(IWHICH1)) + (r1 * X(IWHICH2)))/tdis
          y3 = ((r2 * Y(IWHICH1)) + (r1 * Y(IWHICH2)))/tdis
          z3 = ((r2 * Z(IWHICH1)) + (r1 * Z(IWHICH2)))/tdis

          write(outs,'(a,3f10.4)') ' Vertex is at X,Y,Z:',x3,y3,z3
          call edisp(itru,outs)

C Show the new point on the current wireframe.
          if(MMOD.eq.8)call wiresymbol(x3,y3,z3,1,24)

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 6359.

        elseif(ino.eq.2)then

C Distance between two vertices (based on current G1 common block).
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Line',
     &      'Select two vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2)
            lastv2=iwhich2
            vdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            call ln2az(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),az,el)
            write(outs,'(a,i3,a,i3,a,f9.4,a,f8.3,a,f7.3)')
     &        ' Distance between v ',IWHICH1,' & v ',IWHICH2,' =',vdis,
     &        ' @ aimuth ',az,' & elev ',el
            call edisp(itru,outs)
            MODIFYVIEW=.true.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL redraw(IER)
          endif

        elseif(ino.eq.3)then

C Vertex at distance and angle from another (based on current G1 common).
          if(anotherone)then
            inpick=1; iwhich=lastv1   ! use rememebered 1st vertex
          else
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Origin',
     &        'Select origin vertex.',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich=IVLST(1)
              lastv1=iwhich  ! remember this
            else
              goto 3
            endif
          endif

C Present distance, azimuth and elevation then parse data from HOLD.
          write(hold,'(f10.4,f9.3,f8.3,a)') V1,AZ,EL,'   '
 342      CALL EASKS(HOLD,
     &      'Distance, azimuth an elevation?',
     &      ' ',36,' 1.0  0.0  0.0 ','dist azim elev',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,V1,-999.9,999.9,'W','dist',IER)
          CALL EGETWR(HOLD,K,AZ,-359.9,359.9,'W','azim',IER)
          CALL EGETWR(HOLD,K,EL,-90.0,90.0,'W','elev',IER)
          if(ier.ne.0)goto 342
          PI = 4.0 * ATAN(1.0)
          RAD = PI/180.0; RYAZI = AZ*RAD; RSALT = EL*RAD
          z3 = V1*SIN(RSALT)
          XYDIS = V1*COS(RSALT)
          IF (XYDIS .LT. 1E-6)THEN
            x3 = 0.0; y3 = 0.0
          ELSE
            x3 = XYDIS*SIN(RYAZI); y3 = XYDIS*COS(RYAZI)
          ENDIF
          write(outs,'(a,3f10.4)') 'Vertex calculated @ X,Y,Z:',
     &      x3+X(IWHICH),y3+Y(IWHICH),z3+Z(IWHICH)
          call edisp(itru,outs)
          x3=x3+X(IWHICH); y3=y3+Y(IWHICH); z3=z3+Z(IWHICH)

C Show the new point on the current wireframe.
          if(MMOD.eq.8)call wiresymbol(x3,y3,z3,1,24)

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 6386.

        elseif(ino.eq.4)then

C Angle between two lines (based on current G1 common block).
          inpick=3
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Angle',
     &      'Select three vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.3)then
            iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2); lastv2=iwhich2
            iwhich3=IVLST(3)
            call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &        ang3)
            write(outs,'(a,3i3,a,f9.4)') ' Angle between verts ',
     &        iwhich1,iwhich2,iwhich3,' is ',ang3
            call edisp(itru,outs)
          endif
          MODIFYVIEW=.true.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL redraw(IER)

        elseif(ino.eq.5)then

C Intersection line and plane (based on info in current G1 common).
C << add option for picking surface in another zone >>
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define the line',
     &      'Select two vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2); lastv2=iwhich2
            IS=1
            CALL EPMENSV
            CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &         ' ',IER)
            CALL EPMENRC
            N = NVER(IS)
            DO 150 J = 1,N
              XX(J) = X(JVN(IS,J)); YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
  150       CONTINUE
            call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
            CALL VECPLN(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &        X(IWHICH2),Y(IWHICH2),Z(IWHICH2),EQN, x3,y3,z3, IERR)
            write(outs,'(a,3f10.4)') 'Vertex at intersection X,Y,Z:',
     &        x3,y3,z3
            call edisp(itru,outs)
            call edisp(itru,' ')

C Show the new point on the current wireframe.
            if(MMOD.eq.8)call wiresymbol(x3,y3,z3,1,24)

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 6386.

          endif

        elseif(ino.eq.6)then

C Intersection LINES and a surface or plane. As for insersection surface.
          nbstart=NZTV(icomp)+1  ! Remember 1st new vertex position.
          invt=0
          CALL EASKATOG(' ','Intersection with:',
     &    'existing surface','horizontal plane','vertical (along X)',
     &    'vertical (along Y)','origin azimuth elevation','cancel ',
     &    ' ',INVT,nbhelp)
          if(invt.eq.1)then
            CALL EPMENSV
            IS=1
            CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &        ' ',IER)
            N = NVER(IS)
            CALL EPMENRC

C Setup definition of the plane to intersect.
            DO 149 J = 1,N
              XX(J) = X(JVN(IS,J)); YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
  149       CONTINUE
            call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
            exclude_surf=.true.
          elseif(invt.eq.2)then
            EQN(1)=0.0; EQN(2)=0.0; EQN(3)=1.0
            CALL EASKR(vdis,' ','Height Z value?',0.001,
     &      'F',99.999,'W',0.1,'Z value',IER,nbhelp)
            EQN(4)=vdis
            exclude_surf=.false.
          elseif(invt.eq.3)then
            EQN(1)=1.0; EQN(2)=0.0; EQN(3)=0.0
            CALL EASKR(vdis,' ','Point along X axis?',0.001,
     &      'F',99.999,'W',0.1,'X value',IER,nbhelp)
            EQN(4)=vdis
            exclude_surf=.false.
          elseif(invt.eq.4)then
            EQN(1)=0.0; EQN(2)=1.0; EQN(3)=0.0
            CALL EASKR(vdis,' ','Point along Y axis?',0.001,
     &      'F',99.999,'W',0.1,'Y value',IER,nbhelp)
            EQN(4)=vdis
            exclude_surf=.false.
          elseif(invt.eq.5)then
            call edisp(iuout,'Not yet implemented')
            goto 3
          endif
  
C Loop through each of the edge lists for each of the surfaces in
C the zone (except the identified surface).
          do 135 ivj=1,NSUR
            if(exclude_surf.and.ivj.eq.is) goto 135  ! do not test edges of the plane surface 'is'.
            ivjlimit=NVER(ivj)
            do 147 ivjj=1,ivjlimit
              if(ivjj.eq.ivjlimit)then
                iwhich1=JVN(ivj,ivjj); iwhich2=JVN(ivj,1)
              else
                iwhich1=JVN(ivj,ivjj); iwhich2=JVN(ivj,ivjj+1)
              endif
              x3=0.0; y3=0.0; z3=0.0
              CALL VECPLN(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &          X(IWHICH2),Y(IWHICH2),Z(IWHICH2),EQN, x3,y3,z3, IERR)
              if(ierr.eq.-1) goto 147   ! If error in vecpln.
              write(outs,'(a,i3,3f10.4,a,2i3)')
     &          ' Possible vert X,Y,Z:',ntv+1,
     &          x3,y3,z3,' related to edge ',iwhich1,iwhich2
              call edisp(itru,outs)
              call edisp(itru,' ')

C Test if the point is within the zone bounding box.
              withinbox=.true.
              if(x3.gt.ZXMX(icomp).or.x3.lt.ZXMN(icomp))then
                withinbox=.false.
              elseif(y3.gt.ZYMX(icomp).or.y3.lt.ZYMN(icomp))then
                withinbox=.false.
              elseif(z3.gt.ZZMX(icomp).or.z3.lt.ZZMN(icomp))then
                withinbox=.false.
              endif

C Does this point duplicate any other existing point in the zone?
              if(withinbox)then
                do 138 iwhich2=1,NTV
                  tdis= crowxyz(x3,y3,z3,X(IWHICH2),Y(IWHICH2),
     &                  Z(IWHICH2))
                  if(tdis.lt.CACC)then
                    goto 147
                  endif
  138           continue

C Show the new point on the current wireframe.
                if(MMOD.eq.8)call wiresymbol(x3,y3,z3,1,24)

C Ask if it should be added.
                if(NTV.lt.MTV)then
                  write(outs,'(a,i3,a)')'Make vertex ',NTV+1,' Options:'
                  invt=0
                  CALL EASKABC(' ',outs,'new vertex',
     &              'new vertex after editing','cancel',INVT,nbhelp)
                  if(INVT.eq.1)then
                    NTV=NTV+1; NZTV(icomp)=NTV
                    x(ntv)=x3; y(ntv)=y3; z(ntv)=z3
                    szcoords(ICOMP,ntv,1)=x3; szcoords(ICOMP,ntv,2)=y3
                    szcoords(ICOMP,ntv,3)=z3
                  elseif(INVT.eq.2)then
                    NTV=NTV+1
                    NZTV(icomp)=NTV
                    WRITE(HOLD,'(3f11.5,a)')x3,y3,z3,'  '
                    write(outs,'(a,i3,a)')' Vertex (',ntv,') X Y Z(m):'
  136               CALL EASKS(HOLD,outs,' ',36,' 0. 0. 0. ','vtx co',
     &                IER,nbhelp)
                    K=0
                    CALL EGETWR(HOLD,K,X(ntv),-999.,999.,'W','X cd',IER)
                    CALL EGETWR(HOLD,K,Y(ntv),-999.,999.,'W','Y cd',IER)
                    CALL EGETWR(HOLD,K,Z(ntv),-9.9,999.9,'W','Z cd',IER)
                    if(ier.ne.0)goto 136
                    szcoords(ICOMP,ntv,1)=x(ntv)
                    szcoords(ICOMP,ntv,2)=y(ntv)
                    szcoords(ICOMP,ntv,3)=z(ntv)
                  elseif(INVT.eq.3)then
                    goto 147
                  endif

C Update the geometry file.
                  call eclose(gversion(icomp),1.1,0.01,newgeo)
                  if(.NOT.newgeo)then
                    gversion(icomp) =1.1
                    newgeo = .true.
                  endif
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,
     &              3,IER)
                endif
              endif
  147       continue
  135     continue

C Go back through each of the newly created vertices and see if they
C should be included in existing edges. this separate loop ensures that
C the initial scanning through edges is not mucked up by revisions to
C the edges.
          if(nbstart.lt.NTV)then
            do 445 jwhich3=nbstart,NTV

C See if this vertext should be included in any existing surface edges.
C Use loop incrementors jivj and jivjj within 446 loop. Remember to
C skip over the surface being used to define the plane.
              if(MMOD.eq.8)then
                call wiresymbol(x(jwhich3),y(jwhich3),z(jwhich3),1,24)
              endif
              do 446 jivj=1,NSUR
                if(exclude_surf.and.jivj.eq.is) goto 446  ! Skip past surface 'is'.
                ivjlimit=NVER(jivj)
                do 447 jivjj=1,ivjlimit
                  if(jivjj.eq.ivjlimit)then
                    jwhich1=JVN(jivj,jivjj); jwhich2=JVN(jivj,1)
                  else
                    jwhich1=JVN(jivj,jivjj); jwhich2=JVN(jivj,jivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
C Considered close if less than CACC.
                  call pointtoline(jwhich3,jwhich1,jwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 447
                  if(offset.lt.CACC)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',jwhich3,' @',X(jwhich3),Y(jwhich3),
     &                Z(jwhich3),' is close (',offset,') to edge ',
     &                jWHICH1,' & ',jWHICH2,' of surface ',
     &                SNAME(icomp,jivj)
                    call edisp(itru,outs)

C If current surface (jivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (jivjj) and then
C inserting the new vertex index.
                    if(NVER(jivj)+1.le.MV)then
                      call easkok(' ',
     &                  'Insert this (close to edge) vertex?',
     &                  ok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then   ! update local and connection based arrays
                      NVER(jivj)=NVER(jivj)+1
                      isznver(icomp,jivj)=NVER(jivj)
                      IXV=NVER(jivj)+1
  348                 continue
                      IXV=IXV-1
                      JVN(jivj,IXV)=JVN(jivj,IXV-1)
                      iszjvn(icomp,jivj,IXV)=JVN(jivj,IXV-1)
                      IF(IXV.GT.jivjj+1)GOTO 348
                      JVN(jivj,jivjj+1)=jwhich3
                      iszjvn(icomp,jivj,jivjj+1)=jwhich3

C Update the geometry file.
                      call eclose(gversion(icomp),1.1,0.01,newgeo)
                      if(.NOT.newgeo)then
                        gversion(icomp) =1.1
                        newgeo = .true.
                      endif
                      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,
     &                  3,IER)

C Debug...
C                      write(6,'(a,124i4)') 'now jvn is ',
C     &                  (JVN(jivj,ii),ii=1,NVER(jivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 446
                    endif
                  endif
  447           continue
  446         continue
  445       continue
          endif

C And re-draw the zone to clean up the circles.
          ILEN=NTV
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          CALL redraw(IER)

        elseif(ino.eq.7)then

C Distance between point and a line (based on current G1 common block).
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Line',
     &      'Select two vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2); lastv2=iwhich2
          endif
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to Test',
     &      'Select a vertex.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',X(iwhich3),Y(iwhich3),
     &    Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' & ',IWHICH2
          call edisp(itru,outs)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          CALL redraw(IER)

        elseif(ino.eq.8)then

C Bring a point off a line into alignment. Begin by checking the
C distance between point and the line (based on current G1 common
C block data).
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Line',
     &      'Select two vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2); lastv2=iwhich2
          endif
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to Align',
     &      'Select a vertex.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',X(iwhich3),Y(iwhich3),
     &    Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' & ',IWHICH2
           call edisp(itru,outs)

C If the distance is greater than 1mm get the distance between the
C start of the line (iwhich1) and the test point (iwhich3) and do this
C for the end point on the line (iwhich2) and the test point.
          if(offset.gt.0.001)then
            tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            vdislsp=crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH3),
     &        Y(IWHICH3),Z(IWHICH3))
            write(outs,'(a,F8.4)')' Line start -> unaligned vertex is ',
     &        vdislsp
            call edisp(itru,outs)

C Use square root of (vdislsp^2 - offset^2)
            aligndis = SQRT((vdislsp * vdislsp) - (offset * offset))

C Use ratio calculation to make an aligned point vdislsp along the line.
            r2 = tdis - aligndis
            r1 = aligndis
            x3 = ((r2 * X(IWHICH1)) + (r1 * X(IWHICH2)))/tdis
            y3 = ((r2 * Y(IWHICH1)) + (r1 * Y(IWHICH2)))/tdis
            z3 = ((r2 * Z(IWHICH1)) + (r1 * Z(IWHICH2)))/tdis
            write(outs,'(a,3f10.5)')' Aligned vertex @ X,Y,Z:',x3,y3,z3
            call edisp(itru,outs)

            if(MMOD.eq.8)call wiresymbol(x3,y3,z3,2,24)
            call easkok(' ','Aligned vertex OK?',ok,nbhelp)
            if(ok)then

C Update the coordinates in common blocks and return.
              X(IWHICH3)=x3; Y(IWHICH3)=y3; Z(IWHICH3)=z3
              szcoords(ICOMP,IWHICH3,1)=x3; szcoords(ICOMP,IWHICH3,2)=y3
              szcoords(ICOMP,IWHICH3,3)=z3
            endif
          endif

        elseif(ino.eq.9)then

C Find close vertices.
          CALL EASKATOG(' ','Search options:',
     &    'close vertices','close to a vertex','to an X','to a Y',
     &    'to a Z','cancel ',' ',INVT,nbhelp)
          if(INVT.eq.1)then
            call usrmsg('Scanning for vertices within 50mm...',' ','P')
            do 102 iwhich1=1,NTV
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',X(IWHICH1),Y(IWHICH1),Z(IWHICH1)
              do 103 iwhich2=1,NTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &                  X(IWHICH2),Y(IWHICH2),Z(IWHICH2))
                  if(tdis.le.0.05)then
                  write(outs,'(2a,i3,a,f9.4,a)') headv(1:lnblnk(headv)),
     &              ' is close to vert ',IWHICH2,' (',tdis,'m).'
                  call edisp(itru,outs)
                  endif
                endif
  103         continue
  102       continue
            call edisp(itru,'Scanning for vertices within 100mm...')
            do 104 iwhich1=1,NTV
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',X(IWHICH1),Y(IWHICH1),Z(IWHICH1)
              do 105 iwhich2=1,NTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &                  X(IWHICH2),Y(IWHICH2),Z(IWHICH2))
                  if(tdis.gt.0.05.and.tdis.le.0.1)then
                  write(outs,'(2a,i3,a,f9.4,a)') headv(1:lnblnk(headv)),
     &              ' is close to vert ',IWHICH2,' (',tdis,'m).'
                  call edisp(itru,outs)
                  endif
                endif
  105         continue
  104       continue

            call edisp(itru,
     &      'Use the substitute vertex option in the vertex menu')
            call edisp(itru,
     &      'to search for instances and replace to get rid of close')
            call edisp(itru,
     &      'vertex issues e.g. change all vert 76 with vert 44.')
          elseif(INVT.eq.2)then
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Focus Vertex',
     &        'Select a vertex.',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',X(IWHICH1),Y(IWHICH1),Z(IWHICH1)
              do 101 iwhich2=1,NTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &                  X(IWHICH2),Y(IWHICH2),Z(IWHICH2))
                  if(tdis.lt.0.05)then
           write(outs,'(2a,i3,a,f9.4,a,3F10.4)') headv(1:lnblnk(headv)),
     &       ' close to v ',IWHICH2,' (',tdis,'m) @ XYZ:',X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2)
                    call edisp(itru,outs)
                  endif
                endif
  101         continue
            endif
            call edisp(itru,
     &      'Use the substitute vertex option in the vertex menu')
            call edisp(itru,
     &      'to search for instances and replace to get rid of close')
            call edisp(itru,
     &      'vertex issues e.g. change all vert 76 with vert 44.')
          elseif(INVT.eq.3)then  ! X
            CALL EASKR(VALT,' ','test point along X axis',
     &        -99.,'F',99.,'F',0.0,'test X',IER,nbhelp)
            call linesatz(icomp,'X',VALT)
          elseif(INVT.eq.4)then  ! Y
            CALL EASKR(VALT,' ','test point along Y axis',
     &        -99.,'F',99.,'F',0.0,'test Y',IER,nbhelp)
            call linesatz(icomp,'Y',VALT)
          elseif(INVT.eq.5)then  ! Z
            CALL EASKR(VALT,' ','test point along Z axis',
     &        -99.,'F',99.,'F',0.0,'test Z',IER,nbhelp)
            call linesatz(icomp,'Z',VALT)

C Test for a close vertex.
            HOLD=' 0.0  0.0  0.0'
            CALL EASKS(HOLD,'test point',' ',36,' 0. 0. 0. ','vtx co',
     &        IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,px,-999.,999.,'W','X cd',IER)
            CALL EGETWR(HOLD,K,py,-999.,999.,'W','Y cd',IER)
            CALL EGETWR(HOLD,K,pz,-9.9,999.9,'W','Z cd',IER)
            call pointmergewithinlinesatz(icomp,px,py,pz,rx,ry,rz,iok)
C            write(6,*) 'after pointmerge',px,py,pz,rx,ry,rz,iok
          else
            continue
          endif

        elseif(ino.eq.10)then

C Move an existing vertex along an existing line.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Line',
     &      'Select two vertices. The first',
     &      'one is the vertex that moves.',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1); lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2); lastv2=iwhich2
            tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            write(outs,'(a,i3,a,i3,a,f9.4)') ' Distance between v',
     &        IWHICH1,' & v',IWHICH2,' =',tdis
            call edisp(itru,outs)
            call edisp(itru,' ')

            CALL EASKR(vdis,' ','Movement distance?',
     &         -1.0,'W',99.999,'W',0.1,'dist along line',
     &        IER,nbehlp)

C Use ratio calculation.
            r2 = tdis - vdis
            r1 = vdis
            x3 = ((r2 * X(IWHICH1)) + (r1 * X(IWHICH2)))/tdis
            y3 = ((r2 * Y(IWHICH1)) + (r1 * Y(IWHICH2)))/tdis
            z3 = ((r2 * Z(IWHICH1)) + (r1 * Z(IWHICH2)))/tdis

            write(outs,'(a,3f10.4)') ' Moved vertex @ XYZ:',x3,y3,z3
            call edisp(itru,outs)

C Show the new point on the current wireframe.
            if(MMOD.eq.8)call wiresymbol(x3,y3,z3,2,24)
            call easkok(' ','Apply this move?',OK,nbhelp)
            if(OK)then
              x(IWHICH1)=x3; y(IWHICH1)=y3; z(IWHICH1)=z3
              szcoords(ICOMP,IWHICH1,1)=x3; szcoords(ICOMP,IWHICH1,2)=y3
              szcoords(ICOMP,IWHICH1,3)=z3
              call eclose(gversion(icomp),1.1,0.01,newgeo)
              if(.NOT.newgeo)then
                gversion(icomp) =1.1
                newgeo = .true.
              endif
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
            MODIFYVIEW=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL redraw(IER)  ! this call will clear the X Y Z
          else
            goto 3
          endif
        elseif(ino.eq.11)then

C Insert vertex in surface at an offset from its lower left corner.
C Set initial offset values for rectangle within surface.
          CALL EPMENSV
          IS=1
          CALL EASKSUR(ICOMP,IS,'-','Select surface to insert into:',
     &      ' ',IER)
          N = NVER(IS)
          nbstart=NZTV(icomp)  ! how many vertices did we have to start with
          IF(nbstart+1.GT.MTV)THEN
            CALL USRMSG('A new vertex could not be added as it',
     &      'will make the zone too complex!','W')
            goto 3
          ENDIF
          CALL EPMENRC

C Report parent surface length and height (bounding box).
          CALL ZSURLEHI(ICOMP,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos,
     &      DZLLFF)
          Write(outs,'(a,i3.3,1x,i3.3,f6.3,a,f6.3,a)')'Surface width=',
     &      ICOMP,IS,XYMAX,' m, surface height = ',ZMAX,
     &      ' m (bounding box).'
          call edisp(itru,outs)
          XO1=1.0; ZO1=1.0
  46      write(HOLD,'(2F9.4)') XO1,ZO1
          CALL EASKS(HOLD,
     &      'Offset from lower left corner of surface (outside view):',
     &      ' ',36,' 1. 1. ','offset from corner',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,XO1,-99.,99.,'W','X offset',IER)
          CALL EGETWR(HOLD,K,ZO1,-99.,99.,'W','Z offset',IER)

C Test if the opening is going to be bigger than the bounding box.
          if((XO1).gt.XYMAX)then
            call edisp(itru,'X offset beyond bounding box!')
            goto 46
          elseif((ZO1).gt.ZMAX)then
            call edisp(itru,'Z offset beyond bounding box!')
            goto 46
          endif

C Determine coordinate from X & Z offset.
          call insvertinsurf(ITRC,ICOMP,IS,XO1,ZO1,x3,y3,z3,IER)

C Show this point on the current wireframe.
          if(MMOD.eq.8)then
            call wiresymbol(x3,y3,z3,2,24)
            call forceflush()
          endif
          call easkok(' ','Apply this change?',OK,nbhelp)
          if(OK)then
            NTV=NTV+1; NZTV(icomp)=NTV; MODGEO=.true.
            x(NTV)=x3; y(NTV)=y3; z(NTV)=z3
            szcoords(ICOMP,NTV,1)=x3; szcoords(ICOMP,NTV,2)=y3
            szcoords(ICOMP,NTV,3)=z3
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          endif
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL redraw(IER)  ! this call will clear the X Y Z
        elseif(ino.eq.12)then

C Intersection of line at angle from a point with a surface.
          if(anotherone)then
            inpick=1; iwhich=lastv1   ! use rememebered 1st vertex
          else
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Origin',
     &        'Select origin vertex.',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich=IVLST(1)
              lastv1=iwhich  ! remember this
            else
              goto 3
            endif
          endif

C Present azimuth and elevation then parse data from HOLD.
          write(hold,'(f9.3,f8.3,a)') AZ,EL,'   '
 442      CALL EASKS(HOLD,
     &      'Azimuth and elevation defining a line?',
     &      ' ',36,' 0.0  0.0 ','azim elev',IER,nbhelp)
          K=0
          V1=1.0
          CALL EGETWR(HOLD,K,AZ,-359.9,359.9,'W','azim',IER)
          CALL EGETWR(HOLD,K,EL,-90.0,90.0,'W','elev',IER)
          if(ier.ne.0)goto 442
          PI = 4.0 * ATAN(1.0)
          RAD = PI/180.0; RYAZI = AZ*RAD; RSALT = EL*RAD
          z4 = V1*SIN(RSALT)
          XYDIS = V1*COS(RSALT)
          IF (XYDIS .LT. 1E-6)THEN
            x4 = 0.0; y4 = 0.0
          ELSE
            x4 = XYDIS*SIN(RYAZI); y4 = XYDIS*COS(RYAZI)
          ENDIF
          write(outs,'(a,3f10.4)') 'T vertex calculated @ X,Y,Z:',
     &      x4+X(IWHICH),y4+Y(IWHICH),z4+Z(IWHICH)
          call edisp(itru,outs)
          x4=x4+X(IWHICH); y4=y4+Y(IWHICH); z4=z4+Z(IWHICH)

          IS=1
          CALL EPMENSV
          CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &       ' ',IER)
          CALL EPMENRC
          N = NVER(IS)
          DO J = 1,N
              XX(J) = X(JVN(IS,J)); YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
          ENDDO
          call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
          CALL VECPLN(X(IWHICH),Y(IWHICH),Z(IWHICH),
     &      x4,y4,z4,EQN, x3,y3,z3, IERR)
          write(outs,'(a,3f10.4)') 'Vertex at intersection X,Y,Z:',
     &      x3,y3,z3
          call edisp(itru,outs)
          call edisp(itru,' ')

C Show the new point on the current wireframe.
          if(MMOD.eq.8)call wiresymbol(x3,y3,z3,1,24)

        endif
        call usrmsg(' ',' ','-')

C If possible to add another vertex ask if calculated point should
C be a new vertex and update the wireframe. This logic will be
C applied to the 'a' 'c' 'f' 'l' options.
        if(ino.eq.1.or.ino.eq.3.or.ino.eq.5.or.ino.eq.12)then
          if(NTV.lt.MTV)then
            write(outs,'(a,i3,a)') 'Make vertex ',NTV+1,' Options:'
            CALL EASKABC(' ',outs,'new vertex',
     &        'new vertex after editing','cancel',INVT,nbhelp)
            if(INVT.eq.1)then
              NTV=NTV+1; NZTV(icomp)=NTV
              x(ntv)=x3; y(ntv)=y3; z(ntv)=z3
              szcoords(ICOMP,ntv,1)=x3; szcoords(ICOMP,ntv,2)=y3
              szcoords(ICOMP,ntv,3)=z3
            elseif(INVT.eq.2)then
              NTV=NTV+1
              NZTV(icomp)=NTV
              WRITE(HOLD,'(3f11.5,a)')x3,y3,z3,'  '
              write(outs,'(a,i3,a)')' Vertex (',ntv,') X Y Z (m):'
  43          CALL EASKS(HOLD,outs,' ',36,' 0. 0. 0. ','vtx co',
     &          IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,X(ntv),-999.,999.,'W','X cd',IER)
              CALL EGETWR(HOLD,K,Y(ntv),-999.,999.,'W','Y cd',IER)
              CALL EGETWR(HOLD,K,Z(ntv),-9.9,999.9,'W','Z cd',IER)
              if(ier.ne.0)goto 43
              szcoords(ICOMP,ntv,1)=x(ntv); szcoords(ICOMP,ntv,2)=y(ntv)
              szcoords(ICOMP,ntv,3)=z(ntv)
            elseif(INVT.eq.3)then
              goto 3
            endif

C Update the geometry file.
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            ILEN=NTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODIFYVIEW=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
            CALL redraw(IER)

C Check to see if this new vertex should be included in any existing edges.
            writeagain=.false.
            if(invt.eq.1.or.invt.eq.2)then
              iwhich3=NTV
              do 246 ivj=1,NSUR
                ivjlimit=NVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JVN(ivj,ivjj); iwhich2=JVN(ivj,1)
                  else
                    iwhich1=JVN(ivj,ivjj); iwhich2=JVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
C Considered close if within 4mm.
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.CACC)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',X(NTV),Y(NTV),
     &                Z(NTV),' is close (',offset,') to edge ',
     &                IWHICH1,' & ',IWHICH2,' of surface ',
     &                SNAME(icomp,ivj)
                    call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      call easkok(' ','Insert this vertex?',
     &                  ok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then               ! update local and connection based arrays
                      NVER(ivj)=NVER(ivj)+1
                      isznver(icomp,ivj)=NVER(ivj)
                      IXV=NVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      iszjvn(icomp,ivj,IXV)=JVN(jivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JVN(ivj,ivjj+1)=iwhich3
                      iszjvn(icomp,ivj,jivjj+1)=jwhich3

C Debug...
C                     write(6,'(a,124i4)') 'now jvn is ',
C     &                 (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      writeagain=.true.
                      goto 246
                    endif
                  endif
  247           continue
  246         continue
              if(writeagain)then
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
                else
                  call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
                endif
              endif

C See if another one is required.
              if(ino.eq.1)then
                call easkok(' ','Another vertex along the line?',
     &            ok,nbhelp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              elseif(ino.eq.3)then
                call easkok(' ','Another vertex from this origin?',
     &            ok,nbehlp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              elseif(ino.eq.5)then
                call easkok(' ',
     &            'Another intersection of line and plane?',
     &            ok,nbehlp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              endif

C Tidy up the wireframe image.
              MODIFYVIEW=.true.
              CALL INLNST(1)
              nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
              CALL redraw(IER)
              goto 92
            endif
          endif  ! of NTV less than MTV
        endif    ! of the check whether it should be edited or added
        goto 92

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Alter vertex list by deleting/ copy / replicate / editing.
        writeagain=.false.
        write(outs,'(A,I3,A)')' Up to ',MTV-NTV,
     &    ' vertices may be added or copied.'
        call edisp(itru,outs)
        idno=1
        irt=1
        call MENUATOL('Vertex operations:','Vertex operations:',
     &    'a add vertex','b delete vertex','c copy vertex within zone',
     &    'd replicate vertex','e edit several vertex',
     &    'f delete unused vertices','g copy vertex from another zone',
     &    'h substitute vertices','i cancel ',' ',' ',' ',irt,idno,
     &    nbhelp)
        if(IRT.eq.1.or.IRT.eq.3.or.IRT.eq.4.or.IRT.eq.7)then

C Ask if fine tolerance to be used.
          write(outs,'(a,f7.4,a)')
     &      'Current tolerance for vertex matching is:',CACC,'m.'
          CALL EASKABC(outs,'Options:','accept tolerance',
     &      'use fine tolerances','cancel',INVT,nbhelp)
          if(INVT.eq.1)then
            continue
          elseif(INVT.eq.2)then
            CACC=0.002; ANGCC=1.0; DACC=1.0; COGCC=0.1; SNACC=0.1
          elseif(INVT.eq.3)then
            GOTO 92  ! jump
          endif
        endif
        if(IRT.eq.1)then

C If user cancels jump back to menu setup, otherwise add the requested
C number of vertices.  Note: ADDVERT will have saved the geometry file
C if it successfully added the vertex.
          IADD=1
          CALL EASKI(IADD,' ',' How many vertices to add ? ',
     &     1,'W',MTV-NTV,'F',1,'+ vertex',IERI,nbhelp)
          if(iadd.eq.0)then
            goto 92
          else
            if(ieri.eq.-3)then
              goto 92
            else
              CALL ADDVERT(icomp,IADD,'A',IER)
              ILEN=NTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              iZBFLG(ICOMP)=0
            endif
          endif
        elseif(IRT.eq.2)then

C Delete one or more vertices, get list and then sort in decending
C order so that compating of list works correctly. Note: ADDVERT
C will have saved the geometry file if it successfully deleted the vertex.
          jipm=IPM
          inpick=MIN0(12,NTV-1)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to Delete',
     &      'Select vertices from list.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.gt.0)then
            KFLAG = -1
            call SORTI(IVLST,ITEMP,MTV,KFLAG)
            do 142 ij=1,inpick
              iwhich=IVLST(ij)
              CALL ADDVERT(icomp,IWHICH,'D',IER)
  142       continue
            ILEN=NTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            IPACT= -1*jipm
            CALL EKPAGE(IPACT)
            MODIFYVIEW=.TRUE.; MODBND=.TRUE.; MODLEN=.TRUE.
            iZBFLG(ICOMP)=0
          endif
        elseif(IRT.eq.3)then

C In the case of copying existing vertices, process one or more,
C adding to zone data structure and updating the interface. Allow
C user to copy only as many vertices as there are in the zone but
C no more than will overrange MTV. Note: ADDVERT will have saved
C the geometry file if it successfully added the vertex.
          inpick=MIN0(NTV,MTV-NTV)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to Copy',
     &      'Select vertices from list.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.ge.1)then
            do 143 ij=1,inpick
              iwhich=IVLST(ij)
              CALL ADDVERT(icomp,IWHICH,'C',IER)

C << not that sometimes vertex which should be close is not >>
C << detected. This needs debugging... >>
  143       continue
            ILEN=NZTV(icomp)
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODIFYVIEW=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
          endif
        elseif(IRT.eq.4)then

C In the case of replicating an existing vertex, add it to zone
C data structure and updating the interface. Allow user to replicate
C only up to MTV.  Note: ADDVERT will have saved the geometry file
C if it successfully copied the vertex.
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to Replicate',
     &      'Select vertex from list.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich=IVLST(1)
            CALL EASKI(IADD,' ',' Replicate how many times ? ',
     &        1,'F',MTV-NTV,'F',1,'+ replicate vertex',IERI,nbhelp)
            if(ieri.eq.-3)then
              goto 92
            elseif(ieri.eq.0)then
              if(IADD.ge.1)then
                do 1144 ij=1,IADD
                  CALL ADDVERT(icomp,IWHICH,'C',IER)
 1144           continue
                ILEN=NZTV(icomp)
                IPACT=CREATE
                CALL EKPAGE(IPACT)
                MODIFYVIEW=.TRUE.; MODBND=.TRUE.
                iZBFLG(ICOMP)=0
              endif
            endif
          endif
        elseif(IRT.eq.5)then

C Select (possibly via mouse) and then edit.
          inpick=MIN0(NTV,MTV-NTV)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to Edit',
     &      'Select vertex from list.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.ge.1)then
            do 1145 ij=1,inpick
              ifoc=IVLST(ij)

C Show the point to be edited on the current wireframe.
              if(MMOD.eq.8)then
                call wiresymbol(X(IFOC),Y(IFOC),Z(IFOC),2,24)
              endif

C Present vertex coords for editing then parse data from HOLD.
              WRITE(HOLD,'(3f11.5,a)')X(IFOC),Y(IFOC),Z(IFOC),'  '
              write(outs,'(a,i3,a)')
     &          'Vertex (',ifoc,') X Y Z (in metres):'
 1146         CALL EASKS(HOLD,' ',outs,36,' 0. 0. 0. ','vertex coord',
     &          IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
              CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
              CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
              if(ier.ne.0)goto 1146
              szcoords(ICOMP,IFOC,1)=x(ifoc)
              szcoords(ICOMP,IFOC,2)=y(ifoc)
              szcoords(ICOMP,IFOC,3)=z(ifoc)
              call eclose(gversion(icomp),1.1,0.01,newgeo)
              if(.NOT.newgeo)then
                gversion(icomp) =1.1
                newgeo = .true.
              endif
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
              MODIFYVIEW=.TRUE.; MODBND=.TRUE.; MODLEN=.TRUE.
              iZBFLG(ICOMP)=0
              call warnmod(ICOMP,'str')
 1145       continue
          endif
        elseif(IRT.eq.6)then

C Delete unused vertices quietly, start at end and work back calling
C addvert silently. Note: ADDVERT will have saved the geometry file
C if it successfully deleted the vertex.
          do 139 ij=NZTV(icomp),1,-1
            CALL ADDVERT(icomp,ij,'Q',IER)
  139     continue
          ILEN=NZTV(icomp)
          NTV=NZTV(icomp)
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0

        elseif(IRT.eq.7)then

C Copy up to 10 verticies in another zone. Present a list of
C zone names, only allow for one zone to be selected.
          CALL EPMENSV
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ',' Source zone:',
     &      12,NCOMP,zname,' zone list',IER,nbhelp)
          CALL EPMENRC
          IF(INPIC.EQ.0) goto 92
          IZ=IVALS(1)  ! assign source zone index
          if(IZ.EQ.0)goto 92

C Ask user which vertices and return how many in NVC and the
C coordinates in CX CY CZ arrays.
          CALL EPMENSV
          call CPVERT(IZ,NVC,CX,CY,CZ,IER)
          CALL EPMENRC
          if(NVC.eq.0) goto 92

          do ix=1,NVC

C Merge point into existing lines and zone structure.
            call CKADDVERTINSURF(ITRU,ICOMP,'i',cx(ix),cy(ix),cz(ix),
     &        IER)
          enddo ! end of loop for copied verticies
          ILEN=NZTV(icomp)
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
        elseif(IRT.eq.8)then

C Scan all of the surfaces in this zone and look for vertex
C iwhich1 and replace the reference iwhich2.
          iwhich1=0
          iwhich2=0
          CALL EASKI(iwhich1,' ',
     &      'In zone surface search for vertex index?',
     &      1,'F',NZTV(icomp),'F',1,'find vertex index',IERI,nbhelp)
          CALL EASKI(iwhich2,' ',
     &      'And replace occurrances with vertex index? (zero cancel)',
     &      1,'F',NZTV(icomp),'F',1,'replace vertex index',IERI,nbhelp)
          if(iwhich1.ne.0.and.iwhich2.ne.0)then
            write(outs,'(a,i3,a,i3)') 'Scanning for vertex ',iwhich1,
     &        ' and replacing with ',iwhich2
            call edisp(itru,outs)
            do 3246 ivj=1,NSUR          ! for each surface
              ivjlimit=NVER(ivj)        ! and every vertex in edge list
              do 3247 ivjj=1,ivjlimit
                if(JVN(ivj,ivjj).eq.iwhich1)then  ! check for match
                  write(outs,'(3a,i3,a)') 'In ',sname(icomp,ivj),
     &              ' found vertex ',iwhich1,' in its edge list.'
                  call easkok(outs,'OK to substitute?',ok,nbhelp)
                  if(ok)then
                    JVN(ivj,ivjj)=iwhich2
                    if(newgeo)then
                      call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,
     &                  3,IER)
                    else
                      call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
                    endif
                    MODIFYVIEW=.TRUE.; MODBND=.TRUE.
                    iZBFLG(ICOMP)=0
                  endif
                endif
 3247         continue
 3246       continue
          endif
        endif
        call usrmsg(' ',' ','-')
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit vertex identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Show the point to be edited on the current wireframe. 42 is
C a jump back point if user requests a different vertex.
  42    if(MMOD.eq.8)then
          call wiresymbol(X(IFOC),Y(IFOC),Z(IFOC),1,24)
        endif

C Remind user what other surfaces reference this vertex.
        IHIT=0
        DO 8792 ISL=1,NZSUR(ICOMP)
          DO 8794 IVL=1,NVER(ISL)
            IF(IFOC.EQ.JVN(ISL,IVL))THEN
              IHIT=IHIT+1
              ISASSO(IHIT)=ISL
            ENDIF
 8794     CONTINUE
 8792   CONTINUE
        if(IHIT.lt.6)then

C If there are less than 6 surfaces write our their names.
          call edisp(itru,' ')
          call edisp(itru,'Surfaces which reference this vertex:')
          WRITE(outs,9994)(SNAME(icomp,ISASSO(IH)),IH=1,IHIT)
 9994     FORMAT('  ',6(A,' '))
          call edisp(itru,outs)
        else
          call edisp(itru,' ')
          call edisp(itru,'Surfaces which reference this vertex:')
          WRITE(outs,9993) (ISASSO(IH),IH=1,IHIT)
 9993     FORMAT(10(I3,', '))
          call edisp(itru,outs)
        endif

C Remind user if there are close (99mm or less) vertices.
        do 106 iwhich2=1,NZTV(icomp)
          if(ifoc.ne.iwhich2)then
            tdis= crowxyz(X(ifoc),Y(ifoc),Z(ifoc),
     &            X(IWHICH2),Y(IWHICH2),Z(IWHICH2))
            if(tdis.lt.0.1)then
              write(outs,'(a,i3,a,f9.4,a,3F10.4)')
     &          'And it is close to v ',
     &          IWHICH2,' (',tdis,'m) @ XYZ:',X(IWHICH2),
     &          Y(IWHICH2),Z(IWHICH2)
              call edisp(itru,outs)
            endif
          endif
  106   continue

C Present vertex coords for editing then parse data from HOLD.
        WRITE(HOLD,'(3f11.5,a)')X(IFOC),Y(IFOC),Z(IFOC),'  '
        WRITE(outs,'(a,i3,a,3f11.5)')' Vertex (',ifoc,') @ ',
     &    X(IFOC),Y(IFOC),Z(IFOC)
        call edisp(iuout,outs)
        write(outs,'(a,i3,a)')'Vertex (',ifoc,') X Y Z (in metres):'

C Depending on position within array of vertices adapt the dialog to
C allow for jump to previous or next.
        if(ifoc.eq.1)then
          call EASKS2CMD(HOLD,' ',outs,' ','next vertex',iclkok,36,
     &      ' 0. 0. 0. ','vertex coord',IER,nbhelp)
        elseif(ifoc.eq.NZTV(icomp))then
          call EASKS2CMD(HOLD,' ',outs,'prev vertex',' ',iclkok,36,
     &      ' 0. 0. 0. ','vertex coord',IER,nbhelp)
        else
          call EASKS2CMD(HOLD,' ',outs,'prev vertex','next vertex ',
     &      iclkok,36,' 0. 0. 0. ','vertex coord',IER,nbhelp)
        endif
        K=0
        CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
        CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
        if(ier.ne.0)goto 42
        szcoords(ICOMP,IFOC,1)=x(ifoc)
        szcoords(ICOMP,IFOC,2)=y(ifoc)
        szcoords(ICOMP,IFOC,3)=z(ifoc)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        MODIFYVIEW=.TRUE.; MODBND=.TRUE.; MODLEN=.TRUE.
        iZBFLG(ICOMP)=0
        call warnmod(ICOMP,'str')

C Jump previous or next or return.  Change just edited vertex symbol
C back to black.  If the user happened to edit a vertex and ask for
C a jump then the display will not have a chance to re-draw fully
C so at least draw the vertex in its new location.
        if(iclkok.eq.1.and.ifoc.gt.1) then
          call wiresymbol(X(IFOC),Y(IFOC),Z(IFOC),1,24)
          ifoc=ifoc-1
          goto 42
        elseif(iclkok.eq.2.and.ifoc.lt.NZTV(icomp)) then
          call wiresymbol(X(IFOC),Y(IFOC),Z(IFOC),1,24)
          ifoc=ifoc+1
          goto 42
        else
          continue
        endif
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END

C ************* EDVLIST
C Edit surface-vertex list attributes in common block G1 via a paging
C menu. Make use of VERINFO to display the current vertex surface
C connections.
      SUBROUTINE EDVLIST(ITRC,ITRU,ICOMP,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

C Parameters
      integer itrc  ! verbosity level
      integer itru  ! freeback unit
      integer icomp ! zone index
      integer ier   ! error state zero ok one is problem

      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      DIMENSION  COG1(3),COG2(3)

      LOGICAL OK,bound,nameok
      logical newgeo            ! to use for testing if new/old geometry file.
      integer ifoc              ! local variable for current surface index
      integer iwhich            ! for the worst off point (warped surface check)
      integer iadds             ! for use in addsur calls
      logical close             ! for checking if warp distance is zero
      logical foundit           ! if isolation of warped vertex was successful
      logical modmlc            ! to select MLC
      logical goforit           ! if true then update flow network files
      real ZZZ,ofby             ! the distance worst point is warped
      real XXS,YYS,ZZS          ! suggested point on the line.

      DIMENSION VERT(37),options(10)
      DIMENSION TMAT(4,4),RMAT(4,4),EQN(4),CG(3),EP(3)  ! for warp checks
      real XX,YY,ZZ,XT,YT,ZT
      DIMENSION XX(MV),YY(MV),ZZ(MV),XT(MV),YT(MV),ZT(MV),jvn1(MV)
      dimension ivlst(MTV)       ! for vertex selection
      dimension ISLIST(MS),ITEMP(MS)
      CHARACTER VERT*34,options*34,KEY*1,SN*12,T14*14,D14*14
      character head*32,HOLDS*36,outs*124
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16
      CHARACTER SNAME1*12,SNAMED*12
      character OPT*12
      character constr*32        ! to pass to addmass
      character guesstype*24     ! pass back context of surface
      character defvalstr*32     ! for editing defaults

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*12,rsuse2*12
      character rsparent*12,rname*8,mname*32
      real XO1,ZO1               ! to prevent name clash in geometry.h
      integer MVERT,IVERT,NITMS,IRT ! max items and current menu item
      integer icmpall            ! to signal request to compare all names
      integer ibopt              ! to specify which mass origin option
      logical greyok             ! for display of vertex info
      integer icount,iecount,ibaseedge,ibasecount ! for looping

#ifdef OSI
      integer iix,iiy,iicol
#else
      integer*8 iix,iiy,iicol
#endif

      helpinsub='edgeo'  ! set for subroutine

C Check if we have colour.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Switch to fixed width font for menu. Base size on current
C IMFS or ITFS sizes and remember so can restore.
      lastmenufont=IMFS
      if(IMFS.eq.4) IMFS=0
      if(IMFS.eq.5) IMFS=1
      if(IMFS.eq.6) IMFS=2
      if(IMFS.eq.7) IMFS=3
      lastbuttonfont=IFS
      lasttextfont=ITFS
      if(ITFS.eq.4) ITFS=0
      if(ITFS.eq.5) ITFS=1
      if(ITFS.eq.6) ITFS=2
      if(ITFS.eq.7) ITFS=3
      call userfonts(IFS,ITFS,IMFS)

C Initialise zone surface-vertex list menu size variables based on
C window size. IVERT is the menu position, MVERT the current
C number of menu lines.
      V1=0.0; AZ=0.0; EL=0.0           ! Initial values for editing.
      VALOX=0.0; VALOY=0.0; VALOZ=0.0
      AANG=180.0; DDX=1.0; DDZ=1.0
      newgeo=.false.                   ! Assume older format geometry.
      MHEAD=4
      MCTL=9
      ILEN=NSUR
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 ILEN=NSUR
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      call ckvert(0,icomp,bound,iub,inv,'-',ier)
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          NV=NVER(L)
          if(ntv.le.99)then
           IF(NV.LE.5)THEN
             WRITE(VERT(M),'(A1,1X,A12,I3,5(I3))') KEY,SNAME(ICOMP,L),
     &         NV,(JVN(L,J),J=1,NV)
           ELSE
             WRITE(VERT(M),'(A1,1X,A12,I3,5(I3),a)') KEY,
     &         SNAME(ICOMP,L),NV,(JVN(L,J),J=1,5),'..'
           ENDIF
          else
           IF(NV.LE.4)THEN
             WRITE(VERT(M),'(A1,1X,A12,I3,4(I4))') KEY,SNAME(ICOMP,L),
     &         NV,(JVN(L,J),J=1,NV)
           ELSE
             WRITE(VERT(M),'(A1,1X,A12,I3,4(I4),a)') KEY,
     &         SNAME(ICOMP,L),NV,(JVN(L,J),J=1,4),'.'
           ENDIF
          endif
        ENDIF
   10 CONTINUE

C Set menu header text.
      if(bound)then
        VERT(1)='  enclosure: properly bounded '
      else
        if(iub.gt.0.and.inv.eq.0)then
          WRITE(VERT(1),'(A,i3,a)')  '  enclosure:',iub,' PROBLEM EDGES'
        elseif(iub.eq.0.and.inv.gt.0)then
          WRITE(VERT(1),'(A,i3,a)')  '  enclosure:',inv,' REVERSED SURF'
        elseif(iub.gt.0.and.inv.gt.0)then
          WRITE(VERT(1),'(A,2i3,a)') '  enclosure:',iub,inv,' PROB/REV'
        endif
      endif
      VERT(2)=    '  _______________________________ '
      VERT(3)=    '  Surface   |No. |Verts (anti-clk '
      VERT(4)=    '  name      |vert|from outside)   '
C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  _______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT   ('0 Page --- Part: ',I2,' of ',I2,' --')
      ENDIF
      VERT(M+2)  ='+ add/insert/copy/extrude_from    '
      VERT(M+3)  ='* delete surface(s)               '
      VERT(M+4)  ='> surface transforms              '
      VERT(M+5)  ='< invert surface(s) edges         '
      VERT(M+6)  ='! browse surface-vertex topology  '
      VERT(M+7)  ='@ check surface-vertex topology   '
      VERT(M+8)  ='? help                            '
      VERT(M+9)  ='- exit menu                       '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL redraw(IER)

C Help text for this menu.
      helptopic='surface_edge_list'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(A,A)')'Surface topology of ',zname(ICOMP)
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        helptopic='surface_edge_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('vertex section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Check vertex topology.
        call tstamp('>','PRJ: check zone vertex topology')

C Echo the current tolerances and ask if small features.
        write(outs,'(2a,f7.4,a)') 'If import has small features...',
     &    ' Current tolerance for vertex matching is:',CACC,'m.'
        CALL EASKABC(outs,'Options:','accept tolerance',
     &    'use fine tolerances','cancel',INVT,nbhelp)
        if(INVT.eq.1)then
          continue
        elseif(INVT.eq.2)then
          CACC=0.002; ANGCC=1.0; DACC=1.0; COGCC=0.1; SNACC=0.1
        elseif(INVT.eq.3)then
          GOTO 3  ! skip the check
        endif
        call ckvert(1,icomp,bound,iub,inv,'r',ier)

C If not bounded and iub>0 then loop through each of the
C unbounded surfaces/edges and see if a vertex which is
C not part of its list would resolve this.

      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        CALL VERINFO(ICOMP,ITRU)
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Invert one or more surface edge lists.
        CALL EPMENSV
        CALL EASKMSUR(ICOMP,INPICK,ISLIST,
     &    'Select surfaces (up to 10) to invert.',' ',IER)
        CALL EPMENRC
        if(inpick.gt.0)then
          do loop=1,inpick
            IS=ISLIST(loop)
            do iyy = 1,NVER(IS)
              jvn1(iyy)=JVN(IS,iyy)
            enddo
            JVN(IS,1)=jvn1(2); JVN(IS,2)=jvn1(1)
            do iyy = 3,NVER(IS)
              izz=NVER(IS)+3-iyy
              JVN(IS,iyy)=jvn1(izz)
            enddo

C If construction is symmetric then do nothing but if it
C references a non-symmetric MLC then use an inverted MLC.
            call matchmlcdesc(SMLCN(icomp,is),imlcindex)
            if(imlcindex.ne.0)then
              ii=imlcindex
              lnssmlc=lnblnk(SMLCN(ICOMP,is))
              if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                if(SMLCN(ICOMP,is)(1:lnssmlc).eq.
     &             mlcname(ii)(1:lnmlcname(ii)))then
                  SMLCN(ICOMP,is)=mlcname(ii)
                elseif(SMLCN(ICOMP,is)(1:4).eq.'UNKN')then
                  SMLCN(ICOMP,is)=mlcname(ii)
                else
                  SMLCN(ICOMP,is)=mlcname(ii)
                endif
                iissmlci=ii
              elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is non-symmetric then it should not be
C used for a partition (if db does not have a linked MLC, inform the user).
                SMLCN(ICOMP,is)=mlcsymetric(ii)
                iissmlci=matsymindex(ii)  ! use returned value
              else
                lnssmlc=lnblnk(SMLCN(ICOMP,is))
                if(SMLCN(ICOMP,is)(1:4).eq.'UNKN')then
                  SMLCN(ICOMP,is)=mlcsymetric(ii)
                elseif(SMLCN(ICOMP,is)(1:lnssmlc).eq.
     &                 mlcsymetric(ii)(1:lnssmlc))then
                  SMLCN(ICOMP,is)=mlcsymetric(ii)
                else
                  SMLCN(ICOMP,is)=mlcsymetric(ii)
                endif
                iissmlci=matsymindex(ii)       ! use returned value
              endif
              if(iissmlci.ne.0)then
                smlcindex(ICOMP,is)=iissmlci   ! update
              endif
            endif
            call zgupdate(1,icomp,ier)
          enddo
          MODIFYVIEW=.TRUE.
          CALL INLNST(1)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          CALL redraw(IER)
          goto 92
        endif

      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Miscellaneous geometry transforms. Ask which surface and set up temporary array.
        CALL EPMENSV
        CALL EASKSUR(ICOMP,IS,'-','Select surface to act on.',
     &        ' ',IER)
        CALL EPMENRC
        if(is.eq.0)goto 92

        N = NVER(IS)
        DO J = 1,N
          XX(J) = X(JVN(IS,J))
          YY(J) = Y(JVN(IS,J))
          ZZ(J) = Z(JVN(IS,J))
        ENDDO

        call ZSID(icomp,IS,ZSDES,ZSDESC,ZSDESS)
        write(head,'(a,a)') 'Actions ',ZSDESS
        options(1)  ='a shift along the surface normal  '
        options(2)  ='b XYZ translate                   '
        options(3)  ='c rotate                          '
        options(4)  ='d invert (edge list)              '
        options(5)  ='e combination (shift/rotate/inv)  '
        options(6)  ='f identify cause of surface warp  '
        options(7)  ='g shift child to create reveals   '
        options(8)  ='? help                            '
        options(9)  ='- exit menu                       '
        NITMS=9
        CALL EMENU(head,options,NITMS,IRT)
        if(IRT.EQ.(9))then
          MODIFYVIEW=.FALSE.
          goto 92
        elseif(IRT.EQ.8)then

C List help text for the options menu.
          helptopic='surface_edge_list'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('transform section',nbhelp,'-',0,0,IER)
        elseif(IRT.EQ.7)then

C Create a reveal by relocating surface along its normal and then
C filling in with reveal surfaces. Request reveal depth positive
C or negative.
          CALL EASKR(RTK,
     &      'Surface will be shifted along its normal +outwards to',
     &      'create a reveal. Thickness of the reveal (m)?',
     &      0.0,'W',1.0,'W',0.2,'thickness',IER,nbhelp)
          write(rname,'(a)') sname(icomp,is)(1:5)
          CALL EASKS(rname,
     &      'Reveal surfaces will share a common root name.',
     &      'Name :',8,'reveal','reveal root',IER,nbhelp)

C Find the parent surface an use its MLC for the reveals.
          ioc=IZSTOCN(icomp,is)
          if(iparent(ioc).ne.0)then
            ipc=iparent(ioc)
            mname=smlcn(IC1(ipc),IE1(ipc))
          else
            mname=smlcn(icomp,is)
          endif
          call esurreveal(icomp,is,rtk,rname,mname,ier)

C          call zgupdate(1,icomp,ier)
          MODIFYVIEW=.TRUE.
          CALL INLNST(1)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          CALL redraw(IER)
          goto 92

        elseif(IRT.eq.1.or.IRT.eq.5)then

C Shift along normal and copy transformed points back to XX,YY,ZZ array.
          call tstamp('>','PRJ: do surface shift along normal')
          vdis=0.0
          CALL EASKR(vdis,' ','Distance along normal?',
     &         -99.999,'F',99.999,'W',0.0,'d along normal',IER,nbhelp)
          CALL TRANSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,ZSDES)
          DX=XX(1)-XT(1)  ! get the delta in each axis
          DY=YY(1)-YT(1)
          DZ=ZZ(1)-ZT(1)
          do nt1=1,N
            XX(nt1)=XT(nt1)
            YY(nt1)=YT(nt1)
            ZZ(nt1)=ZT(nt1)
          enddo

C Normal shift of any associated flow node will need a similar transform.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            goforit=.false.
            call doesflowrefsurface(icomp,is,inod,icmp)
            if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &        'A normal shift may require updating of flow network',
     &        'components. Please check!','W')
            if(inod.gt.0)then
              HNOD(INOD,1)=HNOD(INOD,1)+DX
              HNOD(INOD,2)=HNOD(INOD,2)+DY
              HNOD(INOD,3)=HNOD(INOD,3)+DZ
              goforit=.true.
            endif
            if(icmp.gt.0)then
              HCMP(ICMP,1,1)=HCMP(ICMP,1,1)+DX
              HCMP(ICMP,1,2)=HCMP(ICMP,1,2)+DY
              HCMP(ICMP,1,3)=HCMP(ICMP,1,3)+DZ
              goforit=.true.
            endif
            if(goforit)then
              call updatebothflownetworks(ier)
            endif
          endif
        endif
        if(IRT.eq.2.or.IRT.eq.5)then

C Transform all surface vertices.
          call tstamp('>','PRJ: do surface vertex tranform')
          HOLDS= ' 0.000  0.000  0.000    '
 152      CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &      36,' 0.00  0.00  0.00  ','offsets',IER,nbehlp)
          K=0
          CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
          CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
          CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
          if(ier.ne.0)goto 152
          DO I=1,N
            XX(I)=XX(I)+VALX; YY(I)=YY(I)+VALY; ZZ(I)=ZZ(I)+VALZ
          ENDDO

C If there is an associated flow component or node also translate.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            goforit=.false.
            call doesflowrefsurface(icomp,is,inod,icmp)
            if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &        'Transforming a surface may require updating of flow',
     &        'network components or nodes. Please check!','W')
            if(inod.gt.0)then
              HNOD(INOD,1)=HNOD(INOD,1)+VALX
              HNOD(INOD,2)=HNOD(INOD,2)+VALY
              HNOD(INOD,3)=HNOD(INOD,3)+VALZ
              goforit=.true.
            endif
            if(icmp.gt.0)then
              HCMP(ICMP,1,1)=HCMP(ICMP,1,1)+VALX
              HCMP(ICMP,1,2)=HCMP(ICMP,1,2)+VALY
              HCMP(ICMP,1,3)=HCMP(ICMP,1,3)+VALZ
              goforit=.true.
            endif
            if(goforit)then
              call updatebothflownetworks(ier)
            endif
          endif
        endif
        if(IRT.eq.3.or.IRT.eq.5)then

C Rotate around a point in the surface (offer user cancel option).
          call tstamp('>','PRJ: do surface rotation')
          CALL EASKI(IRV,'Rotate about which vertex index: ',
     &       '(ie. 1st, 2nd or 3rd in the surface) ',
     &       1,'F',N,'F',1,'rotation anchor',IERI,nbhelp)
          if(ieri.eq.-3)then
            MODIFYVIEW=.FALSE.
            goto 92
          endif
          ANGR=0.
          CALL EASKR(ANGR,' ','Rotation (+ = anticlockwise)?',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.0; CA=COS(A); SA=SIN(A)
            XX1=XX(IRV); YY1=YY(IRV)
            DO 100 I=1,N
              XXX=XX(I)-XX1; YYY=YY(I)-YY1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              XX(I)=XR+XX1; YY(I)=YR+YY1
  100       continue

C If there is an associated flow component or node also rotate.
            if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
              goforit=.false.
              PI = 4.0 * ATAN(1.0)
              A=-ANGR*PI/180.0; CA=COS(A); SA=SIN(A)
              call doesflowrefsurface(icomp,is,inod,icmp)
              if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &        'Transforming a surface may require updating of flow',
     &        'network components or nodes. Please check!','W')
              if(inod.gt.0)then  ! relocate node and alter orientation
                XXX=HNOD(INOD,1)-XX1
                YYY=HNOD(INOD,2)-YY1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                HNOD(INOD,1)=XR+XX1
                HNOD(INOD,2)=YR+YY1
                SUPNOD(INOD,2)=SUPNOD(INOD,2)+ANGR
                goforit=.true.
              endif
              if(icmp.gt.0)then  ! relocate component
                XXX=HCMP(ICMP,1,1)-XX1
                YYY=HCMP(ICMP,1,2)-YY1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                HCMP(ICMP,1,1)=XR+XX1
                HCMP(ICMP,1,2)=YR+YY1
                goforit=.true.
              endif
              if(goforit)then
                call updatebothflownetworks(ier)
              endif
            endif
          endif  ! of ANGR test
        endif
        if(IRT.eq.4)then

C Reverse the ordering, redraw image and return.
          call tstamp('>','PRJ: do surface invert vertex order')
          do iyy = 1,NVER(IS)
            jvn1(iyy)=JVN(IS,iyy)
          enddo
          JVN(IS,1)=jvn1(2); JVN(IS,2)=jvn1(1)
          do iyy = 3,NVER(IS)
            izz=NVER(IS)+3-iyy
            JVN(IS,iyy)=jvn1(izz)
          enddo
          call zgupdate(1,icomp,ier)
          MODIFYVIEW=.TRUE.
          CALL INLNST(1)
          nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
          CALL redraw(IER)
          goto 92
        endif
        if(IRT.eq.6)then

C Code for testing which vertex might be the problem. The brute force
C method for surfaces of more than 3 edges is to check if the surface
C if warped via call to CHECKWARP and then to call it again until
C it reports back no warps.
          if(nver(is).lt.4) goto 92
          DO KK=1,NVER(is)
            XX(KK) = X(JVN(is,KK))
            YY(KK) = Y(JVN(is,KK))
            ZZ(KK) = Z(JVN(is,KK))
          ENDDO
          N = NVER(is)
          call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

C See if any points are out of the plane.
          iwhich=0
          ofby=0.0
          call CHECKWARP(2,iuout,is,N,XX,YY,ZZ,iwhich,ofby,ivoff)
          if(ivoff.gt.0)then

C We need to loop through each edges. If at loop=kk then set the
C coordiantes to next vertex (e.g. ignore one). Then compute equation
C and use checkwarp until there is one of the variants
C which results in no deviation.
            foundit=.false.
            do 59 loop=1,NVER(is)
              DO 60 KK=1,NVER(is)
                if(loop.eq.kk)then
                  XX(KK) = X(JVN(is,KK+1))  ! take next coords
                  YY(KK) = Y(JVN(is,KK+1))
                  ZZ(KK) = Z(JVN(is,KK+1))

C Debug.
C                  write(6,*) 'Skipping vertex ',loop,kk,JVN(is,KK)

                elseif(loop.eq.NVER(is))then
                  if(kk.lt.NVER(is))then
                    XX(KK) = X(JVN(is,KK))  ! no need to skip
                    YY(KK) = Y(JVN(is,KK))
                    ZZ(KK) = Z(JVN(is,KK))
                  else
                    XX(KK) = X(JVN(is,1))   ! take first coords
                    YY(KK) = Y(JVN(is,1))
                    ZZ(KK) = Z(JVN(is,1))

C Debug.
C                    write(6,*) 'Skipping vertex ',loop,kk,JVN(is,KK)

                  endif
                else
                  XX(KK) = X(JVN(is,KK))    ! no need to skip
                  YY(KK) = Y(JVN(is,KK))
                  ZZ(KK) = Z(JVN(is,KK))
                endif
   60         CONTINUE
              N = NVER(is)
              call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

C See if any points are out of the plane.
              iwhich=0
              ofby=0.0
              call CHECKWARP(1,iuout,is,N,XX,YY,ZZ,iwhich,ofby,ivoff)

C Debug.
C              write(6,*) is,iwhich,ofby,ivoff

              if(ivoff.gt.0)then
                goto 59
              else
                lln=lnblnk(sname(icomp,is))
                write(outs,'(3a,i3)') ' Surface ',
     &            sname(icomp,is)(1:lln),' warp is probably @ vertex ',
     &            JVN(is,loop)
                call edisp(iuout,outs)

C Find distance from this warped vertext to the polygon that does not
C include this vertex.
                DO J = 1,3
                  EP(J) = CG(J) + EQN(J)
                ENDDO

C Call eyemat with 1m offset and then transform to 2D and for this
C warped vertex calculate how much it is off the plane.
                CALL  EYEMAT(EP,CG,1.0,TMAT,RMAT)
                CALL ORTTRN(X(JVN(is,loop)),Y(JVN(is,loop)),
     &            Z(JVN(is,loop)),TMAT,X1,Y1,ZZZ,IERR)
                call eclose(ZZZ,1.0,0.001,close)
                ofby=zzz
                diff=1.0-ZZZ
                WRITE(outs,'(a,F7.5,a,3F9.5)') ' Vertex is off by ',
     &            diff,'m @ ',X(JVN(is,loop)),
     &            Y(JVN(is,loop)),Z(JVN(is,loop))
                CALL EDISP(iuout,outs)
                foundit=.true.

C Suggest a set of coordinates which will be in the plane by
C Calling reverse transform with zzz as 1.0
                ZZZ=1.0
                CALL ORTTRN(X1,Y1,ZZZ,RMAT,XXS,YYS,ZZS,IERR)
                WRITE(outs,'(a,3F9.5)') 'Suggested coords are',
     &            XXS,YYS,ZZS
                CALL EDISP(iuout,outs)

C Ask user if they want to update the vertex with the new values.
                CALL EASKOK(outs,'Update vertex?',OK,nbhelp)
                if(OK)then
                  X(JVN(is,loop))=XXS
                  Y(JVN(is,loop))=YYS
                  Z(JVN(is,loop))=ZZS
                  call zgupdate(1,icomp,ier)
                  MODIFYVIEW=.TRUE.
                  CALL INLNST(1)
                  nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
                  CALL redraw(IER)
                  goto 92
                endif
              endif
   59       CONTINUE

C Report if no variant creates a flat surface.
            if(.NOT.foundit)then
              call edisp(iuout,'Probably more than one vertex warped.')
            endif
          endif
          goto 92
        endif

C Check bounds for the case of surface transform etc.
        do ix = 1,N
          XMN=AMIN1(XMN,XX(ix)); YMN=AMIN1(YMN,YY(ix))
          ZMN=AMIN1(ZMN,ZZ(ix)); XMX=AMAX1(XMX,XX(ix))
          YMX=AMAX1(YMX,YY(ix)); ZMX=AMAX1(ZMX,ZZ(ix))
        enddo

C Update the image.
        MODBND=.TRUE.
        MODIFYVIEW=.TRUE.
        CALL INLNST(1)
        nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
        CALL redraw(IER)

C Show these points as small circles...
        call edisp(iuout,' proposed points are shown as dots....')
        DO J = 1,N
          if(MMOD.lt.8)then
            continue
          else
            COG1(1)=XX(J); COG1(2)=YY(J); COG1(3)=ZZ(J)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
            if (iclp.eq.0) then
              call u2pixel(COG2(1),COG2(2),iix,iiy)
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
            endif
          endif
        ENDDO
        call forceflush()

C Then draw the revised and if acceptable then used transformed
C points within the zone in place of the originals....
        CALL EASKOK(' ','Apply transformed points to the surface',
     &             OK,nbhelp)
        if(OK)then
          MODIFYVIEW=.TRUE.
          DO J = 1,N
            X(JVN(IS,J)) = XX(J)
            Y(JVN(IS,J)) = YY(J)
            Z(JVN(IS,J)) = ZZ(J)
          ENDDO
          call warnmod(ICOMP,'str')
          call zgupdate(1,icomp,ier)

C << possible place for update_cmp_pos(izone,isurf,indexofcmp) >>

        else
          MODIFYVIEW=.FALSE.
        endif
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C Delete up to 6 surface(s) and update cfg.
C Multisurface selection.
        CALL EPMENSV
        CALL EASKMSUR(ICOMP,INPICK,ISLIST,
     &    'Select surface(s) to delete.',' ',IER)
        CALL EPMENRC
        if(inpick.gt.0)then
          KFLAG = -1
          call SORTI(ISLIST,ITEMP,MS,KFLAG)
          do loop=1,inpick
            iopt=0    ! offer editing box with mouse click.
            itrcl=0
            IS=ISLIST(loop)
            CALL ADDSUR(itrcl,ICOMP,IS,'D','V',iopt,IER)  ! Delete surface
            call zinfo(icomp,zoa,zvol,'q')                ! Re-derive values
            call zgupdate(0,icomp,ier)
            CALL EMKCFG('-',IER)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          enddo ! of loop
          call warnmod(ICOMP,'sf-')
        endif

        ILEN=NSUR
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFYVIEW=.TRUE.
        CALL INLNST(1)
        nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
        CALL redraw(IER)
      ELSEIF(IVERT.EQ.(MVERT-7))THEN

C Add/insert a surface.
   30   helptopic='surface_insertion'
        call gethelptext(helpinsub,helptopic,nbhelp)

C Note if running in GTK or in pure text mode then should not offer
C the mouse click option.
        irt=0
        ilrt=irt
        idrt=3
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call MENUATOL(' ','new surface options',
     &     'a made from existing vertices',
     &     'b made from existing vertices (mouse)',
     &     'c inserted into a surface',
     &     'd copy surface(s) in this zone',
     &     'e copy surface(s) from another zone ',
     &     'f vertical rectangle (origin&azim)',
     &     'g horizontal rectangle (origin&rot)',
     &     'h extrude sides/top from base surface',
     &     'i vertical rect mass (origin&azim)',
     &     'j horizontal rect mass (origin&azim)',
     &     ' ',' ',irt,idrt,nbhelp)
        elseif(iglib.eq.2.or.iglib.eq.3)then
          call MENUATOL(' ','new surface options',
     &     'a made from existing vertices',
     &     'b mouse option NOT available',
     &     'c inserted into a surface',
     &     'd copy surface(s) in this zone',
     &     'e copy surface(s) from another zone ',
     &     'f vertical rectangle (origin&azim)',
     &     'g horizontal rectangle (origin&rot)',
     &     'h extrude sides/top from base surface',
     &     'i vertical rect mass (origin&azim)',
     &     'j horizontal rect mass (origin&azim)',
     &     ' ',' ',irt,idrt,nbhelp)
        endif
        call usrmsg(' ',' ','-')
        IF(ilrt.eq.irt)GOTO 92
        IF(irt.eq.0.or.irt.eq.11)GOTO 92
        LASTS=NSUR
        if(IRT.eq.1)then
          call tstamp('>','PRJ: insert surface via vertex')
          iopt=0     ! offer editing box and mouse click option.
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'A','V',iopt,IER)
          call zinfo(icomp,zoa,zvol,'q')                  ! Re-derive values
          call zgupdate(0,icomp,ier)
        elseif(IRT.eq.2)then
          call tstamp('>','PRJ: insert surface via vertex')
          iopt=1     ! offer mouse click directly
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'A','V',iopt,IER) ! Add surface
          call zinfo(icomp,zoa,zvol,'q')                  ! Re-derive values
          call zgupdate(0,icomp,ier)
        elseif(IRT.eq.3)then
          CALL EPMENSV
          call tstamp('>','PRJ: insert surface into another')
          CALL EASKSUR(ICOMP,IS,'-','Select surface to insert into.',
     &        ' ',IER)
          CALL EPMENRC
          IF(IS.EQ.0)RETURN

C Add/insert a surface, return to higher level menu when done.
C The 'ii' request an interactive insertion, the parameters
C XO1,ZO1,XW,ZH are not used in an interactive insert. The string
C parameters are currently passed as blanks for interactive mode.
C << ?? make use of rsname etc in interactive use?? >>
          XO1=0.0; ZO1=0.0; XW=0.0; ZH=0.
          rsname=' '; rsotf=' '; rsmlcn=' '; rsuse1=' '; rsuse2=' '
          rsparent='-'
          CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,guesstype,IER)

C Establish USE for the new surface.
          icon=IZSTOCN(icomp,nsur)
          call edituse(icomp,nsur,icon,guesstype)
     
        elseif(IRT.eq.4)then
          call tstamp('>','PRJ: copy local surfaces')
          iopt=2      ! offer copy of local surfaces
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'C','V',iopt,IER) ! Copy surface
          call zinfo(icomp,zoa,zvol,'q')                  ! Re-derive values
          call zgupdate(0,icomp,ier)
        elseif(IRT.eq.5)then

C Copy surface(s) from another zone. Confirm tolerances.
          write(outs,'(2a,f7.4,a)') 'If import has small features...',
     &      ' Current tolerance for vertex matching is:',CACC,'m.'
          CALL EASKABC(outs,'Options:','accept tolerance',
     &      'use fine tolerances','cancel',INVT,nbhelp)
          if(INVT.eq.1)then
            continue
          elseif(INVT.eq.2)then
            CACC=0.002; ANGCC=1.0; DACC=1.0; COGCC=0.1; SNACC=0.1
          elseif(INVT.eq.3)then
            GOTO 92  ! do not copy any surfaces
          endif
          call tstamp('>','PRJ: copy another zone surfaces')
          iopt=1      ! offer mouse click directly ??
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'C','V',iopt,IER) ! Copy surface
          call zinfo(icomp,zoa,zvol,'q')                  ! Re-derive values
          call zgupdate(0,icomp,ier)
        elseif(IRT.eq.6.or.IRT.eq.9)then

C Get vertical rectangular surface orgin.
          IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the zone too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add vertical surface')

C Offer similar choices to setting up obstructions (see edobs.F)
          call easkabcd(' ','Origin choices:','edit',
     &      'use zone vertex','angle & distance from vertex','cancel',
     &      ibopt,nbhelp)
          if(ibopt.eq.1)then
            HOLDS= ' 0.000  0.000  0.000   '
          elseif(ibopt.eq.2)then

C User current zone G1 common block to present list of points.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &        'Select a vertex to define the origin.',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')X(iwhich1),Y(iwhich1),Z(iwhich1)
            else
              goto 92
            endif
          elseif(ibopt.eq.3)then

C Use current zone G1 common blocks to present list of points
C via epkvert.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertice in zone',
     &        'Select a vertex to begin from...',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              write(holds,'(f10.4,f9.3,f8.3)') V1,AZ,EL
 343          CALL EASKS(HOLDS,
     &    'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &    ' ',32,' 1. 0. 0. ','dist azim elev',IER,nbhelp)
              K=0
              CALL EGETWR(HOLDS,K,V1,-999.9,999.9,'W','dist',IER)
              CALL EGETWR(HOLDS,K,AZ,-359.9,359.9,'W','azim',IER)
              CALL EGETWR(HOLDS,K,EL,-90.0,90.0,'W','elev',IER)
              if(ier.ne.0)goto 343
              PI = 4.0 * ATAN(1.0)
              RAD = PI/180.0; RYAZI = AZ*RAD; RSALT = EL*RAD
              z3 = V1*SIN(RSALT)
              XYDIS = V1*COS(RSALT)
              IF (XYDIS .LT. 1E-6)THEN
                x3 = 0.0; y3 = 0.0
              ELSE
                x3 = XYDIS*SIN(RYAZI)
                y3 = XYDIS*COS(RYAZI)
              ENDIF
              write(outs,'(a,3f10.4)') ' Point @ X,Y,Z:',x3+X(iwhich1),
     &          y3+Y(iwhich1),z3+Z(iwhich1)
              call edisp(itru,outs)
              x3=x3+X(iwhich1); y3=y3+Y(iwhich1); z3=z3+Z(iwhich1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')x3,y3,z3
              MODIFYVIEW=.TRUE.
            else
              goto 92
            endif
          elseif(ibopt.eq.4)then
            goto 92
          endif

          write(defvalstr,'(3f7.3)') VALOX,VALOY,VALOZ
          CALL EASKS(HOLDS,' Vertical surface origin X Y & Z: ',' ',
     &      36,defvalstr,'vert origin XYZ',IER,nbehlp)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get azimuth and length and height.
          write(defvalstr,'(f8.2,2f7.3)') AANG,DDX,DDZ
          write(HOLDS,'(f8.2,2f7.3)') AANG,DDX,DDZ
          CALL EASKS(HOLDS,'Surface azimuth (deg) length & height:',
     &      ' ',36,defvalstr,'azim length height',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,AANG,0.0,360.0,'W','Azim',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

        elseif(IRT.eq.7.or.IRT.eq.10)then

C Get horizontal rectangular surface orgin.
          IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the zone or surface too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add horizontal surface')

C Offer similar choices to setting up obstructions (see edobs.F)
          call easkabcd(' ','Origin choices:','edit',
     &      'use zone vertex','angle & distance from vertex','cancel',
     &      ibopt,nbhelp)
          if(ibopt.eq.1)then
            HOLDS= ' 0.000  0.000  0.000   '
          elseif(ibopt.eq.2)then

C User current zone G1 common block to present list of points.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertices for origin',
     &        'Select a vertex to define the origin...',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')X(iwhich1),Y(iwhich1),Z(iwhich1)
            else
              goto 92
            endif
          elseif(ibopt.eq.3)then

C Use current zone G1 common blocks to present list of points
C via epkvert.
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Vertice in zone',
     &        'Select a vertex to begin from...',' ',nbhelp,ier)
            CALL EPMENRC
            if(inpick.eq.1)then
              iwhich1=IVLST(1)
              write(holds,'(f10.4,f9.3,f8.3)') V1,AZ,EL
 342          CALL EASKS(HOLDS,
     &    'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &    ' ',32,' 1. 0. 0. ','dist azim elev',IER,nbhelp)
              K=0
              CALL EGETWR(HOLDS,K,V1,-999.9,999.9,'W','dist',IER)
              CALL EGETWR(HOLDS,K,AZ,-359.9,359.9,'W','azim',IER)
              CALL EGETWR(HOLDS,K,EL,-90.0,90.0,'W','elev',IER)
              if(ier.ne.0)goto 342
              PI = 4.0 * ATAN(1.0)
              RAD = PI/180.0; RYAZI = AZ*RAD; RSALT = EL*RAD
              z3 = V1*SIN(RSALT)
              XYDIS = V1*COS(RSALT)
              IF (XYDIS .LT. 1E-6)THEN
                x3 = 0.0; y3 = 0.0
              ELSE
                x3 = XYDIS*SIN(RYAZI)
                y3 = XYDIS*COS(RYAZI)
              ENDIF
              write(outs,'(a,3f10.4)') ' Point @ X,Y,Z:',x3+X(iwhich1),
     &          y3+Y(iwhich1),z3+Z(iwhich1)
              call edisp(itru,outs)
              x3=x3+X(iwhich1); y3=y3+Y(iwhich1); z3=z3+Z(iwhich1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')x3,y3,z3
              MODIFYVIEW=.TRUE.
            else
              goto 92
            endif
          elseif(ibopt.eq.4)then
            goto 92
          endif
          write(defvalstr,'(3f7.3)') VALOX,VALOY,VALOZ
          CALL EASKS(HOLDS,' Horizontal surface origin X Y & Z: ',
     &      ' ',36,defvalstr,'horiz origin XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get rotation and length and height.
          write(defvalstr,'(f8.2,2f7.3)') AANG,DDX,DDZ
          write(HOLDS,'(a,2f7.3)') '  0.0 ',DDX,DDZ
          CALL EASKS(HOLDS,'Surface rotation (deg) length & height:',
     &      ' ',36,defvalstr,'rotation length height',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,AANG,0.0,360.0,'W','Rot',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

C Would be useful to be able to preview and revise the location
C without having to do it all again.

        elseif(IRT.eq.8)then

C Identify the source surface, find out how many edges it has and
C whether the zone can hold enough new surfaces.
          CALL EPMENSV
          call tstamp('>','PRJ: extrude from initial surface')
          CALL EASKSUR(ICOMP,IS,'-','Select surface .',
     &        ' ',IER)
          CALL EPMENRC
          IF(IS.EQ.0)RETURN
          vdis=0.0
          CALL EASKR(vdis,' ','Extrusion distance along normal?',
     &      -99.999,'F',99.999,'W',-2.0,'d along normal',IER,nbhelp)

C Loop once for the top and then again for each new edge.  .
          icount=0  ! icount keeps track of which surface is being composed
          iecount=0
          ibaseedge=NVER(IS)  ! nb of edges in the original surface
          ibasecount=NVER(IS) ! counter for original edges left to process
          itopcount = 1 ! counter for top edges to process
          iwcount=(NVER(IS))+1  ! iwcount is the total number of new surfaces
          itopsurf=NSUR+1  ! index of the top surface

 141      icount=icount+1
          if(icount.eq.1)then

C The top surface has the same number of verticies as the base surface
C edge-ordered in opposite direction. The new vertices are generated via
C filling XX YY ZZ arrays with points for surface IS then call to TRANSUR.
C vdis is the distance along the surface normal (e.g. if the initial
C surface is the base then vdis should be negative).
            N = ibaseedge
            DO J = 1,N
              XX(J) = X(JVN(IS,J))
              YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
            ENDDO
            CALL TRANSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,ZSDES)

C Add the transformed points to the zone (later check for unique).
            do ix = 1,ibaseedge
              if(NTV+1.le.MTV)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                X(NTV)=XT(ix); Y(NTV)=YT(ix); Z(NTV)=ZT(ix)
                szcoords(ICOMP,ntv,1)=XT(ix)
                szcoords(ICOMP,ntv,2)=YT(ix)
                szcoords(ICOMP,ntv,3)=ZT(ix)
                XMN=AMIN1(XMN,X(NTV)); YMN=AMIN1(YMN,Y(NTV))
                ZMN=AMIN1(ZMN,Z(NTV)); XMX=AMAX1(XMX,X(NTV))
                YMX=AMAX1(YMX,Y(NTV)); ZMX=AMAX1(ZMX,Z(NTV))
              endif
            enddo

C Assign edges for the top surface in reverse order of the original.
            iecount=NZTV(icomp)
            do ix = 1,ibaseedge
              JVN(itopsurf,ix)=iecount

C Debug.
C              write(6,*) ' top ',ix,iecount,JVN(itopsurf,ix),
C     &          X(iecount),Y(iecount),Z(iecount)

              iecount=iecount-1
            enddo
            NVER(itopsurf)=ibaseedge

C Generate a name for the top.
            write(SNAMED,'(2a)') 't_',SNAME(ICOMP,IS)(1:10)
            write(SNAME1,'(2a)') 't_',SNAME(ICOMP,IS)(1:10)
          else

C Begin with the last edge in the original surface and map that to
C the transformed points. For the last surface use ibaseedge rather
C than ibasecount and one rather than itopcount+1.
            iv1=JVN(is,ibasecount)
            if(ibasecount.eq.1)then
              iv2=JVN(is,ibaseedge)
            else
              iv2=JVN(is,ibasecount-1)
            endif
            if(itopcount.eq.ibaseedge)then
              iv3=JVN(itopsurf,1)
            else
              iv3=JVN(itopsurf,itopcount+1)
            endif
            iv4=JVN(itopsurf,itopcount)
            JVN(NSUR+1,1)=iv1; JVN(NSUR+1,2)=iv2
            JVN(NSUR+1,3)=iv3; JVN(NSUR+1,4)=iv4
            NVER(NSUR+1)=4

C Debug.
C            write(6,*) ' iv1 iv2 iv3 iv4 ',iv1,iv2,iv3,iv4

            if(icount.le.9)then
             write(SNAMED,'(a,i1,a)')'edg_',icount,SNAME(ICOMP,IS)(1:7)
             write(SNAME1,'(a,i1,a)')'edg_',icount,SNAME(ICOMP,IS)(1:7)
            else
             write(SNAMED,'(a,i2,a)')'edg_',icount,SNAME(ICOMP,IS)(1:6)
             write(SNAME1,'(a,i2,a)')'edg_',icount,SNAME(ICOMP,IS)(1:6)
            endif

C Update the counters (ibasecount decrements, itopcount increments).
            ibasecount=ibasecount-1
            itopcount=itopcount+1
          endif

C Check that suraface name is unique.
          icmpall=0 ! force compare of all surfaces
          call snamdup(SNAME1,icomp,icmpall,nameok)
          if(.NOT.nameok)then

C Copy to a slightly longer buffer for editing and ensure that
C the final name uses the allowed character set.
            write(T14,'(2a)') SNAME1(1:12),'  '
            write(D14,'(2a)') SNAMED(1:12),'  '
            CALL EASKS(T14,'Revised surface name',
     &        '(duplicate found)',14,D14,'surface name',IER,nbhelp)
            if(T14(1:2).NE.'  ')then
              write(SNAME1,'(a)') T14(1:12)
            else
              goto 141
            endif
          endif

C Up the surface count and zone surface attributes. Because the
C user cannot yet see the newly extruded surface it is pointless to
C ask them about its use and composition.
          ICONT=IZSTOCN(icomp,nsur)+1
          NSUR=NSUR+1
          NZSUR(ICOMP)=NZSUR(ICOMP)+1
          SNAME(ICOMP,NSUR)=SNAME1
          SMLCN(ICOMP,NSUR)='UNKNOWN'
          SVFC(ICOMP,NSUR)='UNKN'
          SOTF(ICOMP,NSUR)='OPAQUE'
          SOTHER(ICOMP,NSUR,1)='UNKNOWN'
          SOTHER(ICOMP,NSUR,2)='0'
          SOTHER(ICOMP,NSUR,3)='0'
          SPARENT(ICOMP,NSUR)='-'
          SUSE(ICOMP,NSUR,1)='-'
          SUSE(ICOMP,NSUR,2)='-'
C          icon=ICONT

C Add surface to the connection list (icont) assuming an unknown boundary.
          call addedsurf(icomp,icont,1,ier)

C If icount is less than iwcount loop back other wise jump to point
C where geometry and configuration is saved and re-display managed.
          if(icount.lt.iwcount)then
            goto 141
          else
            call usrmsg(
     &        'Note you must attribute the extruded surfaces and it',
     &        'is recommended that you turn on surface normals!','W')
            goto 77
          endif
        endif   ! of the new surface options (IRT)

C If user created surface via existing vertices then check if
C it is warped.
        if(IRT.eq.1.or.IRT.eq.2)then
          continue ! << code to be added here. >>
        endif

        if(IRT.eq.6.or.IRT.eq.7.or.IRT.eq.9.or.IRT.eq.10)then

C Ask for new surface name.
          T14=' '
          SN=' '
          CALL EASKS(T14,'Inserted surface name?','  ',
     &      14,'new_door','surface name',IER,nbhelp)
          write(SN,'(a)') T14(1:12)

C Ask for surface construction.
          CALL EPMENSV
          if(mlcver.eq.0)then
            CALL EPKMLC(ISEL,'Select one of the constructions for',
     &        'the inserted surface.',IER)
          else
            call edisp(iuout,
     &      'Select one of the constructions for the inserted surface')
            CALL EDMLDB2(modmlc,'-',ISEL,IER)
          endif
          CALL EPMENRC
          if(ISEL.GT.0)then
            WRITE(constr,'(A)') mlcname(ISEL)

C Find the optical name.
            OPT=' '
            IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
              OPT='OPAQUE'
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
              SOTF(ICOMP,NSUR)='CFC '
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC2')then
              SOTF(ICOMP,NSUR)='CFC2'
            ELSE
              WRITE(OPT,'(A)') mlcoptical(ISEL)(1:12)
              IF(OPT.EQ.' ')OPT='UNKNOWN'
            ENDIF
          else
            WRITE(constr,'(A)') 'UNKNOWN'
            OPT='OPAQUE'
          endif

C Instantiate the new surface via call addmass with all of the parameters.
C << addmass parameters for optical should be 32 characters >>
          if(IRT.eq.6)then
            call addmass(ICOMP,'VS','G',VALOX,VALOY,VALOZ,
     &        AANG,DDX,DDZ,SN,constr,OPT,INVT)
          elseif(IRT.eq.7)then
            call addmass(ICOMP,'HS','G',VALOX,VALOY,VALOZ,
     &        AANG,DDX,DDZ,SN,constr,OPT,INVT)
          elseif(IRT.eq.9)then
            call addmass(ICOMP,'VM','G',VALOX,VALOY,VALOZ,
     &        AANG,DDX,DDZ,SN,constr,OPT,INVT)
          elseif(IRT.eq.10)then
            call addmass(ICOMP,'HM','G',VALOX,VALOY,VALOZ,
     &        AANG,DDX,DDZ,SN,constr,OPT,INVT)
          endif

C Track users confirmation of new surface.
          if(INVT.eq.-1)then
            ITVNO=0
            ITSNM=0
            MODIFYVIEW=.TRUE.
            CALL INLNST(1)
            nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
            CALL redraw(IER)
            GOTO 30
          elseif(INVT.eq.-3)then
            return
          endif
        endif   ! of processing of vert or horiz rectangle.

C Record the recent changes and re-display
 77     if(LASTS.ne.NSUR)then
          CALL EMKCFG('-',IER)
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          call warnmod(ICOMP,'sf+')
        endif
        MHEAD=4
        MCTL=9
        ILEN=NSUR
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.; MODIFYVIEW=.TRUE.
        iZBFLG(ICOMP)=0
        call usrmsg(' ',' ','-')
        CALL INLNST(1)
        nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
        CALL redraw(IER)
      ELSEIF(IVERT.EQ.(MVERT-8))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND. Treat the list of vertices as a
C long string and then parse the data.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        call editedgelist(icomp,ifoc,ier)

      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-4
      goto  92

      END

C ************** editedgelist
C editedgelist supports editing of the edge list of a polygon
C and if in graphic mode it controls what is displayed.
      subroutine editedgelist(icomp,isurf,ier)
#include "building.h"
#include "net_flow.h"
!##include "net_flow_data.h"
#include "model.h"
#include "geometry.h"
!##include "epara.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

C Parameters
      integer icomp,isurf ! zone & focus surface index
      integer ier   ! error state zero ok one is problem

      integer lnblnk  ! function definition
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      DIMENSION  COG1(3),COG2(3)

      CHARACTER DESCRC*25
      integer ifoc    ! local variable for current surface index
      integer iwhich  ! for the worst off point (warped surface check)
      real ofby   ! the distance worst point is warped

      DIMENSION EQN(4),CG(3)  ! for warp checks
      DIMENSION VN(3),ipoints(6,2)
      real XX,YY,ZZ
      DIMENSION XX(MV),YY(MV),ZZ(MV)
      character outs*124
      character hold64*64,t248*496
      character message*48,stemp*13
      character guesstype*24     ! pass back context of surface

C Strings for surface attributes to pass to insrec.
      integer mnulen,iwhich2   ! multi column lines
      logical greyok,odd,nozeros   ! for display of vertex info
      integer istrw

#ifdef OSI
      integer iix,iiy,iicol,iixc,iiyc,iid1,iid2,iid3,iid4
#else
      integer*8 iix,iiy,iicol,iixc,iiyc,iid1,iid2,iid3,iid4
#endif

      helpinsub='edgeo'  ! set for subroutine

C Check if we have colour.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

      ifoc=isurf                  ! set the focus surface.

C Set all surfaces to standard line width and surface being edited to
C a thick line.
      MODIFYVIEW=.TRUE.
      CALL INLNST(1)
      CALL SURADJ(ICOMP,IFOC,IE,TMP,IZC,ISC,IC,DESCRC)
      if(IC.gt.0)LINSTY(IC)=2
      nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
      CALL redraw(IER)

   93 helptopic='surface_edge_order'
      call gethelptext(helpinsub,helptopic,nbhelp)
      NV=NVER(IFOC)

C If in graphic mode draw the points.
      if(MMOD.lt.8)then
        continue
      else
        do 961 i=1,nv
          iwhich=jvn(ifoc,i)
          if(iwhich.gt.0)then
            COG1(1)=X(iwhich); COG1(2)=Y(iwhich); COG1(3)=Z(iwhich)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
            if (iclp.eq.0) then
              call u2pixel(COG2(1),COG2(2),iix,iiy)

C Draw vertex symbol and index in red.
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              call VERTLBLRED(iix,iiy,COG2(3),iwhich,ier)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif
          endif
  961   continue

C Highlight the arrow of the focus surface.
        write(stemp,'(A)')SNAME(ICOMP,IFOC)
        if(itsnr.eq.0)then
          CG(1)=SURCOG(icomp,isurf,1); CG(2)=SURCOG(icomp,isurf,2)
          CG(3)=SURCOG(icomp,isurf,3)
          VN(1)=SURVN(icomp,isurf,1); VN(2)=SURVN(icomp,isurf,2)
          VN(3)=SURVN(icomp,isurf,3)
          CALL VECTRN(VN,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
          if (iclp.ne.0) goto 52
          call u2pixel(COG2(1),COG2(2),iixc,iiyc)
          CALL winfnt(issize)
          iicol=0
          if(greyok)call winscl('z',iicol)
          call arrow(CG,VN,0.3,0.1,ipoints,'a',2)

C Offset the surface name depending on orientation.
          IF(SVFC(icomp,isurf).EQ.'VERT')THEN
            iixc=iixc+5
          ELSEIF(SVFC(icomp,isurf).EQ.'CEIL')THEN
            iixc=iixc+5
          ELSEIF(SVFC(icomp,isurf).EQ.'FLOR')THEN
            iixc=iixc+5
          ELSE
            iixc=iixc+5
          ENDIF
          call CLIPST(stemp,iixc,iiyc,COG2(3),iclp)
          if (iclp.eq.0) then
            call textatxy(iixc,iiyc,stemp,'z',iicol)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif
          CALL winfnt(IMFS)
        else
          CG(1)=SURCOG(icomp,isurf,1); CG(2)=SURCOG(icomp,isurf,2)
          CG(3)=SURCOG(icomp,isurf,3)
          CALL VECTRN(CG,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
          if (iclp.ne.0) goto 52
          call u2pixel(COG2(1),COG2(2),iixc,iiyc)
          CALL winfnt(issize)
          iicol=0
          if(greyok)call winscl('z',iicol)
          IF(SVFC(icomp,isurf).EQ.'VERT')THEN

C Draw arrow and horizontal line.
            iid1=iixc+3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc+3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc+7
            call eswline(iixc,iiyc,iid1,iiyc)
            iixc=iixc+7
          ELSEIF(SVFC(icomp,isurf).EQ.'CEIL')THEN

C Draw arrow to surface then up and horizontal to the text.
            iid1=iixc+3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc-3; iid2=iiyc-3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iiyc-5
            call eswline(iixc,iiyc,iixc,iid1)
            iid1=iiyc-5; iid2=iixc+7; iid3=iiyc-5
            call eswline(iixc,iid1,iid2,iid3)
            iiyc=iiyc-5; iixc=iixc+7
          ELSEIF(SVFC(icomp,isurf).EQ.'FLOR')THEN

C Draw arrow to surface then down and horizontal to the text.
            iid1=iixc+3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iixc-3; iid2=iiyc+3
            call eswline(iixc,iiyc,iid1,iid2)
            iid1=iiyc+5
            call eswline(iixc,iiyc,iixc,iid1)
            iid1=iiyc+5; iid2=iixc+7; iid3=iiyc+5
            call eswline(iixc,iid1,iid2,iid3)
            iiyc=iiyc+5; iixc=iixc+7
          ENDIF

          iid4=iiyc+3
          call CLIPST(stemp,iixc,iid4,COG2(3),iclp)
          if (iclp.eq.0) then
            call textatxy(iixc,iid4,stemp,'z',iicol)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif
          CALL winfnt(IMFS)
        endif
      endif

C List out the vertices associated with this surface. To save
C space, use double column as in the emenu code in
C esru_libNonGTK.F and also for use in the model contents report.

  52  write(message,'(2a)') 'Vertices (X Y Z) used by ',
     &  SNAME(ICOMP,IFOC)
      call edisp(iuout,message)

C See if an even or odd number of items in list.
      im=MOD(NV,2)
      odd=.false.
      if(im.eq.1) odd=.true.
      if(nv.lt.8)then
        do 960 i=1,nv
          iwhich=jvn(ifoc,i)
          if(iwhich.gt.0)then
            write(outs,'(a,i3,3f11.5)') 'vertex ',iwhich,X(iwhich),
     &        Y(iwhich),Z(iwhich)
          else
            write(outs,'(a)') 'vertex with zero index'
          endif
          call edisp(iuout,outs)
  960   continue
      else
        MNULEN=(NV/2)
        DO 193 K=1,MNULEN
          iwhich=jvn(ifoc,k)
          iwhich2=jvn(ifoc,k+mnulen)
          if(iwhich.gt.0.and.iwhich2.gt.0)then
            WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') 'vertex ',iwhich,
     &        X(iwhich),Y(iwhich),Z(iwhich),'   vertex ',iwhich2,
     &        X(iwhich2),Y(iwhich2),Z(iwhich2)
          else
            WRITE(outs,'(a)') 'vertex with a zero index.'
          endif
          call edisp(iuout,outs)
  193   CONTINUE
        IF(odd)THEN
          WRITE(outs,'(a,i3,3f11.5)') 'vertex ',nv,X(nv),
     &      Y(nv),Z(nv)
          call edisp(iuout,outs)
        ENDIF
      endif

C Set up a long string buffer for editing vertex list for the
C surface. Include surface name in the dialog. This write statement
C should be kept up to date with MV changes. t248 is actually 496 char
      hold64='                                                      '
      write(t248,'(4a)') hold64,hold64,hold64,'                     '
      WRITE(t248,'(124I4)')(JVN(IFOC,j),J=1,nv)
      write(message,'(2a)') 'Vertices associated with ',
     &  SNAME(ICOMP,IFOC)
      istrw=72
      CALL EASKS496(t248,message,
     &  ' ',istrw,' 1  2  3  4  ','associated vertices',IIER,nbhelp)
      NV = iCountWords(t248)
      K=0
      DO 94 J=1,NV

C Read an index, check if within range and if so add to JVN().
        CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IIER)
        IF(IIER.NE.0)GOTO 93
        if(JV.eq.0.or.JV.gt.NTV)then
          write(outs,'(a,i3,a,i3,a)') 'Vertex index ',J,' value',JV,
     &    'is out of range!'
          call edisp(iuout,outs)
        endif
        JVN(IFOC,J)=JV
        iszjvn(ICOMP,IFOC,J)=JV
   94 CONTINUE
      NVER(IFOC)=NV
      isznver(ICOMP,IFOC)=NVER(ifoc)
      MODIFYVIEW=.TRUE.
      call warnmod(ICOMP,'str')

C Check to see if this surface is warped.
C Derive the equation of the polygon as long as it
C has no zero index vertices.
      nozeros=.true.
      DO 607 KK=1,NVER(ifoc)
        iwhich=JVN(ifoc,KK)
        if(iwhich.eq.0) nozeros=.false.
        XX(KK) = X(JVN(ifoc,KK))
        YY(KK) = Y(JVN(ifoc,KK))
        ZZ(KK) = Z(JVN(ifoc,KK))
  607 CONTINUE
      if(nozeros)then
        N = NVER(ifoc)
        call PLEQN(XX,YY,ZZ,N,CG,EQN,IERR)

C See if any points are out of the plane.
        iwhich=0
        ofby=0.0
        call CHECKWARP(1,iuout,ifoc,N,XX,YY,ZZ,iwhich,ofby,ivoff)
        if(ivoff.gt.0)then
          lln=lnblnk(sname(icomp,ifoc))
          write(outs,'(a,i3,2a,i3,a)') ' Surf ',ifoc,
     &      sname(icomp,ifoc)(1:lln),
     &      ' is warped. Please check via surface transform options.'
          call edisp(iuout,outs)
        endif
      endif

      return
      end

C ************* EPKVERT
C EPKVERT Select one or more vertices from information currently in
C common block G1.
C << zone index is not known but might be useful here >>
      SUBROUTINE EPKVERT(INPICK,IVLST,TITLE,PROMPT1,PROMPT2,NHELP,IER)
#include "building.h"
#include "geometry.h"
#include "epara.h"
#include "prj3dv.h"

C Parameters passed.
      integer inpick  ! passed in number of items that can be selected
                      ! and becomes actual number of items selected
      integer IVLST   ! array to hold items selected
      CHARACTER*(*) TITLE,PROMPT1,PROMPT2
      integer nhelp   ! number of context help lines
      integer ier     ! error state zero ok, one problem

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      DIMENSION  item(36)
      real COG1,COG2  ! position to check against
      DIMENSION  COG1(3),COG2(3),IVLST(MTV)
      CHARACTER item*33,outs*124,KEY*1
      logical greyok,found
      integer MVERT,IVERT ! max items and current menu item

C Trackview needs iixx,iiyy.
#ifdef OSI
      integer iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#else
      integer*8 iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#endif

C Initialise vertex list menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Also clear tagged items list (IVLST).
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Switch to fixed width font for menu. Base size on current
C IMFS or ITFS sizes and remember so can restore.
      lastmenufont=IMFS
      if(IMFS.eq.4) IMFS=0
      if(IMFS.eq.5) IMFS=1
      if(IMFS.eq.6) IMFS=2
      if(IMFS.eq.7) IMFS=3
      lastbuttonfont=IFS
      lasttextfont=ITFS
      IER=0
      MHEAD=1
      MCTL=4
      ILEN=NTV
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPICK
      INPICK=0
      DO 40 I=1,NTV
        IVLST(I)=0
   40 CONTINUE

C Initial menu entry setup.
C << is there something better to use then NTV ? >>
      CALL USRMSG(PROMPT1,PROMPT2,'-')
   92 ILEN=NTV
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          item(m)=' '
          WRITE(item(M),'(A1,I4,3F9.3)')KEY,L,X(L),Y(L),Z(L)
          do 20 K=1,INPICK
            if(IVLST(K).eq.L) then
              WRITE(item(M),'(A1,I4,3F9.3,A)')KEY,L,X(L),Y(L),
     &          Z(L),'*'
            endif
 20       continue
        ENDIF
   10 CONTINUE

      item(1)    =' Vertex | X Y Z coordinates      '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        item(M+1)='  ______________________________ '
      ELSE
        WRITE(item(M+1),15)IPM,MPM
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      if(MMOD.lt.8)then
        write(item(M+2),'(2x,a,i3,a)') '(',IALLOW,' items)'
      else
        write(item(M+2),'(a,i3,a)')'* select via mouse (',IALLOW,
     &    ' items)'
      endif
      item(M+3)  ='? help                           '
      item(M+4)  ='- exit                           '

C Help text for this menu is passed from calling routine.
      CALL EMENU(TITLE,item,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Return with updated IVLST().
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help strings setup in the calling function.
        CALL PHELPD('vertex pick',NHELP,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Use mouse to select existing vertices.
        if(MMOD.eq.8)then
          ijvn=0
          if(inpick.ne.2)then
            call edisp(iuout,
     &        'Select points via cursor...type `e` to finish.')
          else
            call edisp(iuout,
     &        'Select points or edge via cursor...type `e` to finish.')
          endif

C Return pixel position of mouse click, check if key `e` or `E` was
C hit and then loop through each of the vertices for something close.
C In this code block trackview is correctly using iixx and iiyy.
  46      CALL trackview(iik,iixx,iiyy)
          if(iik.eq.69.or.iik.eq.101)goto 47
          found=.false.
          do 45 i=1,NTV
            COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
            CALL VECTRN(COG1,TSMAT,COG2,IER)
            call u2pixel(COG2(1),COG2(2),iix,iiy)
            ixd=iix-iixx
            iyd=iiy-iiyy
C Debug
C            write(6,'(7i5)') i,iixx,iiyy,iix,iiy,ixd,iyd
            if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
              if(found)then
                call edisp(iuout,'Close points...try again.')
                goto 46
              endif
              WRITE(outs,'(a,i3,a,3F9.3)')' The point matches vertex',
     &          i,' @ XYZ ',X(I),Y(I),Z(I)
              call edisp(iuout,outs)
              found=.true.
              INPICK=INPICK+1
              ijvn=ijvn+1
              IVLST(ijvn)=i

C Highight the matching vertex.
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
              goto 46
            endif
  45      continue
          if(.NOT.found)then

C If there were only two points to find, check if user clicked on edge.
            if(inpick.eq.2)then
              call usrmsg(
     &          'Code for edge check is work-in-progress.',' ','P')
            endif
            goto 46
          endif
  47      continue
        endif
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVLST and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPICK.GT.0)THEN
          DO 44 J=1,INPICK
            IF(IVLST(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NTV) then
                IVLST(J)=0
              else
                IVLST(J)=IVLST(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPICK.lt.IALLOW) then
              INPICK=INPICK+1
              IVLST(INPICK)=IFOC
            endif
          ELSE
            INPICK=INPICK-1
          ENDIF
        ELSEIF(INPICK.EQ.0)THEN
          INPICK=1
          IVLST(INPICK)=IFOC
        ENDIF
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END


C ******* zsurfprm
C Zsurfprm returns the number of edges and total length of
C the perimiter of the polygon with global commons.
      subroutine zsurfprm(izone,isurf,dupedges,perim)
#include "building.h"
#include "geometry.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      logical dupedges

      perim=0.0
      dupedges=.false.
      if(izone.ne.0.and.izone.le.NCOMP)then
        if(isurf.le.NZSUR(izone).and.isurf.ne.0)then

C First confirm whether there are any duplicate references to vertices
c (indicates a polygon with a hole in it).
          do 40 m=1,isznver(izone,isurf)
            do 41 n=1,isznver(izone,isurf)
              IF(m.EQ.n)goto 41
              J1=iszjvn(izone,isurf,n)
              J2=iszjvn(izone,isurf,m)
              IF(J1.EQ.J2)dupedges=.true.
  41        continue
  40      continue

C Step through the vertices of each edge in turn and find distance.
          list=isznver(izone,isurf)-1
          do 42 i=1,list
            j=iszjvn(izone,isurf,i)
            k=iszjvn(izone,isurf,i+1)
            vdis=0.0
            vdis= crowxyz(szcoords(izone,j,1),szcoords(izone,j,2),
     &        szcoords(izone,j,3),szcoords(izone,k,1),
     &        szcoords(izone,k,2),szcoords(izone,k,3))
            perim=perim+vdis
  42      continue

C Link back to start vertex
          j=iszjvn(izone,isurf,isznver(izone,isurf))
          k=iszjvn(izone,isurf,1)
          vdis=0.0
          vdis= crowxyz(szcoords(izone,j,1),szcoords(izone,j,2),
     &      szcoords(izone,j,3),szcoords(izone,k,1),
     &      szcoords(izone,k,2),szcoords(izone,k,3))
          perim=perim+vdis
        endif
      endif
      return
      end

C ******************** EZIPIN
C EZIPIN reads zip format geometry data from a text file into standard
C esp-r common blocks. If possible it converts default windows and doors
C into surfaces.
C There are three levels of geometry data input corresponding
C to RECtangular (REC), REGular (REG) and GENeral (GEN) shaped zones
C using essentially the same logic as in EGOMIN (see notes in EGOMIN).
C If IR=1 then range checking is enabled, otherwise only minimal checking
C is performed on the data as read in.

C << For GEN type input the 'G1' common block is filled immediately.
C << With REC and REG type input the simpler input data is read and
C << passed to subroutines ERECC and EREGC respectively where
C << conversion to the 'G1' format takes place.

      SUBROUTINE EZIPIN(IUNIT,LZIP,ICOMP,IR,ITRC,ITRU,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "espriou.h"

      integer lnblnk  ! function definition

      common/FILEP/IFIL

C NGL (from geometry.h) - contains the nb of windows in each face
C   this is depreciated.

C SNAME (12 char) - surface name attribute and other variables are defined
C in geometry.h
C ZBASEA    - area of base (m^2)
C IBASES    - surfaces (up to 12) associated with base area. If all 0 then
C             base area has been supplied by the user.

C ZNAME (12 char) - the zone name from geometry.h.
C ZDESC (64 char) - zone notes from geometry.h.

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      DIMENSION XX(MS),YY(MS),IVA(MS)

C For each possible default window and default door the relevant data.
      DIMENSION VALX(MS),VALZ(MS),VALW(MS),VALH(MS)
      DIMENSION VALDX(MS),VALDW(MS),VALDH(MS)

C Pointer from default window or door to the parent surface it is to
C be inserted.
      dimension iwhichsurfw(MS), iwhichsurfd(MS)
      CHARACTER OUTSTR*124,WORD*20
      character lzip*72,dfile*72
      character phrasea*64,phraseb*64,outs*124
      character act*2
      character guesstype*24     ! pass back context of surface
      logical checkbase
      logical newgeo  ! to use for testing if new/old geometry file.

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*12,rsuse2*12
      character rsparent*12
      real ARX,Z1X,Z2X,DX1,DY1,DZ1  ! to avoid variable clash with geometry.h
      real XO1,YO1,ZO1

      IER=0
      phrasea=' '
      phraseb=' '
      checkbase=.false.
      newgeo=.false.  ! assume older format geometry.

C Initialise geometry data file. and set currentfile.
      CALL EFOPSEQ(IUNIT,LZIP,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Zip format file ',lzip(1:lnblnk(lzip)),
     &      ' could not be opened.'
        goto 1002
      ENDIF
      write(currentfile,'(a)') lzip(1:lnblnk(lzip))

C First line of a zip file starts with 'COM'. If so the 2nd line
C of the file is the tag 'NAME' followed by the name of the file
C on the 3rd line.
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 1',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','COM',IFLAG)
      if(WORD(1:3).ne.'COM')then
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &      ' is not in zip format.'
        goto 1002
      endif

C Recover the remainder of the line.
      call egetrm(outstr,K,phraseb,'W','Z comment',IER)
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'zip NAME',IER)
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'zip file name',IER)
      write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &  ' NAME or file name was misunderstood.'
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','COM',IFLAG)

C Debug.
C      write(6,*) 'the overall file name is ',WORD

C Debug.
C      lwd=lnblnk(WORD)
C      write(6,*) 'the file name is ',WORD(2:lwd-1)

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'zip date',IER)
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'blank line',IER)

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'file type',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','ESP',IFLAG)
      if(WORD(1:3).ne.'ESP')then
        write(outs,'(3a)') 'In file ',lzip(1:lnblnk(lzip)),
     &      ' expected an `ESP` line.'
        goto 1002
      endif

      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'edge ordering',IER)
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','ANT',IFLAG)
      if(WORD(1:3).ne.'ANT')then
        write(outs,'(3a)') 'In file ',lzip(1:lnblnk(lzip)),
     &      ' expected an `ANT` line.'
        goto 1002
      endif

C At this point the header of the file will have been written.
C Read a line and look for COLOUR, VISIB, LAYER or GEN.
 96   CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'edge colour',IER)
      write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &  ' COLOUR, VISIB, LAYER or GENNAME  was misunderstood.'
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W',
     &  'tag COLOUR VISIB LAYER or GEN',IFLAG)
      if(WORD(1:6).eq.'COLOUR')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'colour index',IER)
        goto 96
      elseif(WORD(1:5).eq.'VISIB')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'visib index',IER)
        goto 96
      elseif(WORD(1:5).eq.'LAYER')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'layer index',IER)
        goto 96
      elseif(WORD(1:3).eq.'GEN')then
        continue
      else
        write(outs,'(3a)') 'In file ',lzip(1:lnblnk(lzip)),
     &    ' expected a `COLOUR, VISIB, LAYER or GEN` line.'
        goto 1002
      endif

C Read lines from file, if one item then assumed to
C be CTYPE, if 2 then the second is the zone name which
C is tested for illegal characters.
 97   IF(IER.NE.0)goto 1001
      K=0
      IF(ND.EQ.1)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
        zname(ICOMP)=' '
        lnzname(ICOMP)=0
        zdesc(ICOMP)=' '
        lnzdesc(ICOMP)=0
      ELSEIF(ND.EQ.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
        call egetrm(outstr,K,phrasea,'W','Z description',IER)
        zdesc(ICOMP)=' '
        lnzdesc(ICOMP)=0
      ELSEIF(ND.gt.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        write(CTYPE(icomp),'(a)') WORD(1:lnblnk(WORD))
        call egetrm(outstr,K,phrasea,'W','Z description',IER)

C Combine the comment from the first line with the one after the zone CTYPE.
        ipra=lnblnk(phrasea)
        iprb=lnblnk(phraseb)
        iprc= 63 - ipra
        iwidth=ipra + iprb + 1
        if(iwidth.lt.64)then
          write(zdesc(ICOMP),'(3a)') phrasea(1:ipra),' ',
     &      phraseb(1:iprb)
          lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
        else
          write(zdesc(ICOMP),'(3a)') phrasea(1:ipra),' ',
     &      phraseb(1:iprc)
          lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
        endif
      ENDIF
      write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &  ' Comment lines or CTYPE was misunderstood.'
      IF(IFLAG.NE.0)GOTO 1001

C Fill in a default zone name and description if blank.
      if(zname(ICOMP)(1:2).EQ.'  '.OR.
     &     zname(ICOMP)(1:7).EQ.'UNKNOWN')then
        IF(ICOMP.LE.9)WRITE(zname(ICOMP),'(A5,I1)')'Zone-',ICOMP
        IF(ICOMP.GT.9)WRITE(zname(ICOMP),'(A5,I2)')'Zone-',ICOMP
        lnzname(ICOMP)=lnblnk(zname(ICOMP))  ! update the length of this string.
      endif
      if(zdesc(ICOMP)(1:1).EQ.' ')then
        write(zdesc(ICOMP),'(a,a)')
     &    zname(ICOMP)(1:lnzname(ICOMP)),' describes a '
        lnzdesc(ICOMP)=lnblnk(zdesc(ICOMP))  ! update the length of this string.
      endif

C Zone is of type REC, look for 7 items on one or two lines.
      IF(CTYPE(icomp)(1:3).EQ.'REC')THEN
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 2',IER)
        write(outs,'(3a)') 'In file ',lzip(1:lnblnk(lzip)),
     &    ' REC line origin was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWR(OUTSTR,K,XO1,0.,0.,'-','rec X origin',IER)
        CALL EGETWR(OUTSTR,K,YO1,0.,0.,'-','rec Y origin',IER)
        CALL EGETWR(OUTSTR,K,ZO1,0.,0.,'-','rec Z origin',IER)

C If only 3 items on first line read another OUTSTR and try to continue.
        IF(ND.EQ.3) THEN
          K=0
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 3',IER)
          IF(IER.NE.0)goto 1001
        ENDIF
        CALL EGETWR(OUTSTR,K,DX1,0.,0.,'-','length',IER)
        CALL EGETWR(OUTSTR,K,DY1,0.,0.,'-','width',IER)
        CALL EGETWR(OUTSTR,K,DZ1,0.,0.,'-','height',IER)
        CALL EGETWR(OUTSTR,K,ARX,-360.,360.,'W','rotation angle',IER)
        rotateit(icomp,1)=ARX
        rotateit(icomp,2)=XO1
        rotateit(icomp,3)=YO1

C Now convert to a gen description. Pass in critical dimensions
C and expect the data back via common G1.
        CALL ERECC(XO1,YO1,ZO1,DX1,DY1,DZ1,ARX)
        rotateit(ICOMP,1)=0.0
        rotateit(ICOMP,2)=0.0
        rotateit(ICOMP,3)=0.0
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE(icomp)='GEN '

C Zone is of type REG.
      ELSEIF(CTYPE(icomp)(1:3).EQ.'REG')THEN
        CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'line 2',IER)
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' REG line data was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NW,3,MS-2,'W','no of walls',IER)
        CALL EGETWR(OUTSTR,K,Z1X,0.,1000.,'W','floor height',IER)
        CALL EGETWR(OUTSTR,K,Z2X,Z1X,1000.,'F','ceiling ht',IER)
        CALL EGETWR(OUTSTR,K,ARX,-360.,360.,'W','rot angle',IER)
        rotateit(icomp,1)=ARX

C Read base vertex data.
        DO 60 IW=1,NW
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(OUTSTR,K,XX(IW),0.,0.,'-','Base X',IER)
          CALL EGETWR(OUTSTR,K,YY(IW),0.,0.,'-','Base Y',IER)
   60   CONTINUE

C Now convert to GEN body type and place into in common block.
        CALL EREGC(NW,Z1X,Z2X,XX,YY)
        IF(ARX.LT.-.01.OR.ARX.GT..01)then
          x1=X(1)
          y1=Y(1)
          rotateit(icomp,2)=x1
          rotateit(icomp,3)=y1
          CALL ESCROT(ARX,x1,y1)
          rotateit(ICOMP,1)=0.0
          rotateit(ICOMP,2)=0.0
          rotateit(ICOMP,3)=0.0
        endif
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE(icomp)='GEN '

C Zone is of type GEN.
      ELSEIF(CTYPE(icomp)(1:3).EQ.'GEN')THEN
        CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'NTV NSUR AR',IER)
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' GEN line NTV NSUR AR was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NTV,4,MTV,'F','no of vertices',IER)
        CALL EGETWI(OUTSTR,K,NSUR,3,MS,'F','no of surfaces',IER)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CALL EGETWR(OUTSTR,K,ARX,-360.,360.,'W','rot angle',IER)

C Read each vertex data line, strip any comments, see if 3 items and
C place in X(),Y(),Z().
        DO 62 I=1,NTV
          CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'vertex data',IER)
          write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' GEN line vertex data was misunderstood.'
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(OUTSTR,K,X(I),0.,0.,'-','X coord',IER)
          CALL EGETWR(OUTSTR,K,Y(I),0.,0.,'-','Y coord',IER)
          CALL EGETWR(OUTSTR,K,Z(I),0.,0.,'-','Z coord',IER)
   62   CONTINUE

C Read vertex list for each surface, strip comments, begin by finding
C the number of expected vertices (first item on list).
        DO 10 I=1,NSUR
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'vertex list',IER)
          write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' GEN line vertex list was misunderstood.'
          IF(IER.NE.0)goto 1001
          IF(ND.GE.4)THEN
            K=0
            CALL EGETWI(OUTSTR,K,J,3,MV,'F','nb assoc vertices',IERV)
            NVER(I)=J

C Now proceed to read vertices on one or more lines.
            DO 12 KV=1,NVER(I)
              CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              IF(IERV.NE.0) THEN
                call edisp(ITRU,' reading continuation line...')
                CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'vertex XYZ',IER)
                IF(IER.NE.0)goto 1001
                K=0
                CALL EGETWI(OUTSTR,K,IVAL,0,MTV,'F','vertex',IERV)
              ENDIF
              IF(IERV.NE.0) GOTO 1001
              JVN(I,KV)=IVAL
   12       CONTINUE
          ENDIF
   10   CONTINUE

C Rotate if required.  Shift pending rotate to prior rotate.
        if(ARX.LT.-.01.OR.ARX.GT..01)then
          x1=X(1)
          y1=Y(1)
          rotateit(ICOMP,1)=x1
          rotateit(ICOMP,2)=y1
          CALL ESCROT(ARX,x1,y1)
          rotateit(ICOMP,1)=0.0
          rotateit(ICOMP,2)=0.0
          rotateit(ICOMP,3)=0.0
        endif
      ELSE
        write(outs,'(2a)') ' Geometry shape type illegal in ',
     &    lzip(1:lnblnk(lzip))
        goto 1002
      ENDIF

C Read line of default indices (one per surface) if any index is
C non zero then it represents a depreciated default window, we can
C handle (easily if one per surface) and convert it to a transparent
C surface later on in the subroutine.
      IRVA=NSUR
      CALL EGETWIA(IUNIT,IVA,IRVA,0,3,'F','def window list',IER)

C Read the window dimension details and advise user (if IVA non-zero).
      nbdef=0
      DO 16 KS=1,NSUR
        IF(IVA(KS).GT.0)THEN
          DO 18 KW=1,IVA(KS)
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'window data',IER)
            write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &      ' Def win data was misunderstood.'
            IF(IER.NE.0)goto 1001
            IF(ND.GE.4)THEN
              nbdef=nbdef + 1
              VALX(nbdef)=0.0
              VALZ(nbdef)=0.0
              VALW(nbdef)=0.0
              VALH(nbdef)=0.0
              iwhichsurfw(nbdef)=KS
              K=0
              CALL EGETWR(OUTSTR,K,VALX(nbdef),0.,0.,'-',
     &          'win X off',IER)
              CALL EGETWR(OUTSTR,K,VALZ(nbdef),0.,0.,'-',
     &          'win Z off',IER)
              CALL EGETWR(OUTSTR,K,VALW(nbdef),0.,100.,'W',
     &          'win width',IER)
              CALL EGETWR(OUTSTR,K,VALH(nbdef),0.,100.,'W',
     &          'win ht',IER)
              write(outs,'(a,i3,a,f6.3,a,f6.3,a,f6.3,a,f6.3)')
     &          'Window for surface',ks,': X off ',VALX(nbdef),
     &          ' Z off ',VALZ(nbdef),' width ',VALW(nbdef),
     &          ' height ',VALH(nbdef)
              call edisp(itru,outs)
            ENDIF
   18     CONTINUE
        ENDIF
   16 CONTINUE

C Read default doors index (one per surface).
      IRVA=NSUR
      CALL EGETWIA(IUNIT,IVA,IRVA,0,2,'F','def door list',IER)
      nbdor=0
      DO 22 KS=1,NSUR
        IF(IVA(KS).GT.0)THEN
          DO 23 KW=1,IVA(KS)
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'door data',IER)
            write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &      ' Def door data was misunderstood.'
            IF(IER.NE.0)goto 1001
            IF(ND.GE.3)THEN
              nbdor=nbdor + 1
              VALDX(nbdor)=0.0
              VALDW(nbdor)=0.0
              VALDH(nbdor)=0.0
              iwhichsurfd(nbdor)=KS
              K=0
              CALL EGETWR(OUTSTR,K,VALDX(nbdor),0.,0.,'-',
     &          'door X off',IER)
              CALL EGETWR(OUTSTR,K,VALDW(nbdor),0.,100.,'W',
     &          'door width',IER)
              CALL EGETWR(OUTSTR,K,VALDH(nbdor),0.,100.,'W',
     &          'door ht',IER)
              write(outs,'(a,i3,a,f6.3,a,f6.3,a,f6.3)')
     &          'Door for surface',ks,': X off ',VALDX(nbdor),
     &          ' width ',VALDW(nbdor),
     &          ' height ',VALDH(nbdor)
              call edisp(itru,outs)
            ENDIF
   23     CONTINUE
        ENDIF
   22 CONTINUE

C Read default insolation surface numbers. NN = insolation defining
C index:  1; one plane, 2; two planes, 3; all planes (diffuse).
C IDPN defines the default plane numbers.
      CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'insolation data',IER)
      write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &  ' insolation data was misunderstood.'
      IF(IER.NE.0)goto 1001
      K=0
      CALL EGETWI(OUTSTR,K,IV,1,3,'W','Def insol index',IER)
      NDP(ICOMP)=IV
      CALL EGETWI(OUTSTR,K,IV,0,NSUR,'W','1st recv surf',IER)
      IDPN(ICOMP,1)=IV
      CALL EGETWI(OUTSTR,K,IV,0,NSUR,'W','2nd recv surf',IER)
      IDPN(ICOMP,2)=IV
      CALL EGETWI(OUTSTR,K,IV,-1,NSUR,'W','3rd insol data',IER)
      IDPN(ICOMP,3)=IV
      IF(IR.EQ.1)THEN
        DO 40 I=1,3
          IX=0
          IF(NDP(ICOMP).EQ.1.AND.I.EQ.1)IX=1
          IF(NDP(ICOMP).EQ.2.AND.I.LE.2)IX=1
          if(IDPN(ICOMP,I).NE.-1.AND.IDPN(ICOMP,I).NE.0)then
            if(IDPN(ICOMP,I).LT.IX.OR.IDPN(ICOMP,I).GT.NSUR)then
              CALL USRMSG(' Nonexistent insolation plane!',OUTSTR,'W')
            endif
          endif
   40   CONTINUE
      ENDIF

C Surface attributes:
C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
      CALL FILSUR(ICOMP,0)

C Estimate base area.
      checkbase=.true.
      lastlist=0
      IBASES(ICOMP,1)=0; IBASES(ICOMP,2)=0; IBASES(ICOMP,3)=0
      IBASES(ICOMP,4)=0; IBASES(ICOMP,5)=0; IBASES(ICOMP,6)=0
      IBASES(ICOMP,7)=0; IBASES(ICOMP,8)=0; IBASES(ICOMP,9)=0
      IBASES(ICOMP,10)=0; IBASES(ICOMP,11)=0; IBASES(ICOMP,12)=0
      IBASES(ICOMP,13)=0; IBASES(ICOMP,14)=0; IBASES(ICOMP,15)=0
      IBASES(ICOMP,16)=0; IBASES(ICOMP,17)=0; IBASES(ICOMP,18)=0
      ZBASEA(ICOMP)=0.00
      IER=0

C If base area has not yet been calculated, do this now and
C get surface areas via call to zinfo.
      if(checkbase)then
        call zgupdate(0,icomp,ier)
        call zinfo(icomp,zoa,zvol,'q')
        vol(icomp)=zvol
        zonetotsurfacearea(icomp)=zoa
        do 43 ijj=1,NSUR
          if(SVFC(icomp,ijj).eq.'FLOR')then

C If surface `flor` not included in the list add it if the
C USE is something other than FURNI.
            if(SUSE(icomp,ijj,1)(1:5).eq.'FURNI')then
              continue
            else
              lastlist=lastlist+1
              if(lastlist.le.6)then
                IBASES(ICOMP,lastlist)=ijj
                ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(icomp,ijj)
              endif
            endif
          endif
  43    continue
        write(outs,'(a,f6.2,3a)') 'Base area estimated at ',
     &     ZBASEA(ICOMP),'m^2 for ',zname(icomp)(1:lnzname(icomp)),
     &    ' (probably an older file).'
        call edisp(itru,outs)
        if(lastlist.gt.0)iuzbasea(icomp)=0
      endif

C Update the connection list and then the model configuration.
      NCOMP=NCOMP+1
      NZSUR(NCOMP)=NSUR
      NZTV(NCOMP)=NTV
      NCCODE(NCOMP)=NCOMP
      ICCC=NCON

C If surface attributes are adaibetic or similar then use these in
C the connections list, then remind user to double check topology.
      DO 132 ICC=1,NSUR
        ICCC=ICCC+1
        IC1(ICCC)=icomp
        IE1(ICCC)=ICC
        if(SOTHER(icomp,icc,1)(1:8).eq.'EXTERIOR')then
          ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
        elseif(SOTHER(icomp,icc,1)(1:7).eq.'SIMILAR')then
          ICT(ICCC)=1; IC2(ICCC)=0; IE2(ICCC)=0
        elseif(SOTHER(icomp,icc,1)(1:9).eq.'ADIABATIC')then
          ICT(ICCC)=5; IC2(ICCC)=0; IE2(ICCC)=0
        elseif(SOTHER(icomp,icc,1)(1:8).eq.'CONSTANT')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a CONSTANT connection. Use the'
          call usrmsg(outs,
     &      'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
        elseif(SOTHER(icomp,icc,1)(1:6).eq.'GROUND')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a GROUND connection. Use the'
          call usrmsg(outs,
     &     'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
        elseif(SOTHER(icomp,icc,1)(1:7).eq.'UNKNOWN')then
          ICT(ICCC)=-1; IC2(ICCC)=0; IE2(ICCC)=0
        else
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a possible partition. Confirm its'
          call usrmsg(outs,
     &      'surface attributes or use topology tool.','W')
          ICT(ICCC)=0; IC2(ICCC)=0; IE2(ICCC)=0
        endif
        smlcindex(ic1(iccc),ie1(iccc))=smlcindex(icomp,icc)
        isznver(icomp,icc)=NVER(icc)
        DO KV=1,isznver(icomp,icc)
          iszjvn(icomp,icc,KV)=JVN(icc,KV)
        ENDDO
        IZSTOCN(ncomp,icc)=iccc
  132 CONTINUE
      NCON=ICCC

C Update the G7 common blocks and then write out configuration file.
      call zgupdate(1,NCOMP,ier)

C Make up a zone geometry file name for this new zone.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
      else
        WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.geo'
      endif
      LGEOM(icomp)= DFILE
      write(outs,'(A,A)') 'Updating initial surfaces in ',zname(icomp)
      call usrmsg(' ',outs,'-')
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(.NOT.newgeo)then
        gversion(icomp) =1.1
        newgeo = .true.
      endif
      call geowrite(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)

C If there are default windows then attempt to insert them as
C transparent surfaces.
      if(nbdef.gt.0)then
        do 66 idw=1,nbdef
          isurf=iwhichsurfw(idw)
          act='sw'
          XO1=VALX(idw); ZO1=VALZ(idw)
          XW=VALW(idw); ZH=VALH(idw)
          rsname=' '; rsotf=' '; rsmlcn=' '
          rsuse1=' '; rsuse2=' '; rsparent='-'
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,guesstype,IER)
  66    continue
      endif

C If there are default doors, attempt to insert them as surfaces.
      if(nbdor.gt.0)then
        do 67 idd=1,nbdor
          isurf=iwhichsurfd(idd)
          act='sd'
          XO1=VALDX(idd); ZO1=0.0
          XW=VALDW(idd); ZH=VALDH(idd)
          rsname=' '; rsotf=' '; rsmlcn=' '
          rsuse1=' '; rsuse2=' '; rsparent='-'
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,guesstype,IER)
  67    continue
      endif

C Save zone if more surfaces have been added.
      if(nbdef.gt.0.or.nbdor.gt.0)then
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call geowrite(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)
      endif

C And there might be more than one room in a zip file. If we
C ever come across a GEN this signals a new zone.
C Read a line and look for COLOUR, VISIB, LAYER or GEN (in
C case there is more than one zone in the zip file). If an
C error at this point we have probably reached the end.
  196 CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'edge colour',IER)
      IF(IER.NE.0)then
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &  ' possible next zone was misunderstood or EOF reached.'
        CALL ERPFREE(IUNIT,ios)
        CALL EMKCFG('-',IER)
        RETURN
      endif
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W',
     &  'tag COLOUR VISIB LAYER or GEN',IFLAG)
      if(WORD(1:6).eq.'COLOUR')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'colour index',IER)
        goto 196
      elseif(WORD(1:5).eq.'VISIB')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'visib index',IER)
        goto 196
      elseif(WORD(1:5).eq.'LAYER')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'layer index',IER)
        goto 196
      elseif(WORD(1:3).eq.'GEN')then

C We have reached another GEN so increment ICOMP and loop back
C to 97 to read in further details.
         call edisp(iuout,'Scanning in another zone...')
         ICOMP=ICOMP+1
         NZSUR(ICOMP)=0
         NZTV(ICOMP)=0
         NSUR=0
         NTV=0
         phrasea=' '
         checkbase=.false.
         goto 97
      else
        write(outs,'(3a)') 'In file ',lzip(1:lnblnk(lzip)),
     &    ' expected a `COLOUR, VISIB, LAYER or GEN` line.'
        goto 196
      endif

C Close zip data file as the end of the file has been reached
      CALL ERPFREE(IUNIT,ios)
      CALL EMKCFG('-',IER)

      RETURN

C 1001 write(outs,'(3a)') 'EZIPIN: conversion error in...',
C     &  OUTSTR(1:50),'...'
 1001 continue
      call edisp(iuout,outs)
      call edisp(iuout,OUTSTR)
      IER=1
      CALL ERPFREE(IUNIT,ios)
      RETURN

C Found unexpected item.
 1002 call edisp(itru,outs)
      IER=1
      RETURN

      END


C ******************** LINTHBRDG ***************************************
C LINTHBRDG defines linear thermal bridges for zone psi and lengths.
C The extra fabric heat loss is calculated at the end
C Additional information is given in the BRE IP 1/06 document by T I Ward
C in tables 3 and 4.
C This subroutine may be called from the geometry definition menu using
C act='-' or when defining the notional model using act='s'. The latter
C is a silent call and assumes a thermal bridging of 10% (0.1)

      SUBROUTINE LINTHBRDG(ICOMP,act,THPER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
C #include "material.h"
#include "sbem.h"
#include "derived.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C bridgelen was calculated in subroutine suredgeadj
      real bridgelen ! Length of potential thermal bridges in each zone.
      common/gbridge/bridgelen(MCOM,16)

C Thermal bridge common block:
C nbrdg is the number of thermal bridge types in the zone
C psi is the linear thermal transmittance value,
C lngth is the length of the thermal bridge and
C ibrdg is an index defining the type of thermal bridge as follows:
C   1 is "roof-wall", 2 is "wall-ground floor"
C   3 is "wall-wall (convex corner)", 4 is "wall-wall (concave corner)"
C   5 is "wall-floor (exposed)", 6 is "lintel above window or door"
C   7 is "Sill below window", 8 is "jamb at window or door"
C   9 is "roof-wall-gable", 10 is "wall-parapet"
C  11 is "wall-interm-floor", 12 is "wall-partition"
C  13 is "glass-frame", 14 is "Sill below window-mc"
C  15 is "user-defined-a"
C  16 is "user-defined-b"

C losspercent is W/K for an alternative method  where the calculated heat flow
C   is augmented by a fixed percentage of the UA (fabric loss)
C totheatloss is W/K for all length*psi plus losspercent for the zone
C thbrpercent is the user defined fraction for thermal bridges used by
C   losspercent and is assumed to apply to all zones in the model.
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      real uavtotal  ! estimated UA for exposed parts of building
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent,uavtotal(MCOM)

      LOGICAL context

      DIMENSION TBRIDGE(30)
      integer IVALS(MCOM)  ! the array of zones to include
      CHARACTER outs*124
      character SIGSTR*12,t16b*16,act*1
      character TBRIDGE*58
      dimension phraselen(16),phrasepsi(16),phrasemenu(16)
      character phraselen*42,phrasepsi*42,phrasemenu*34
      character msg*48  ! to use for default prompt.

      real THPER

      DIMENSION xx(2),xy(2),xz(2)
      integer lnb
      real psiv,lenv  ! local variables for psi and length.
      integer ISDN,INODA ! max items and current menu item
      real defpsi     ! default value for psi
      dimension defpsi(16,4)
      real lenpsi     ! default value for length based on bridgelen.
      dimension lenpsi(16)
      real zerolen,zeropsi  ! for bridge types that have yet to be used.
      logical foundit
      logical greyok

#ifdef OSI
      integer iix,iiy,iix2,iiy2,iicol
#else
      integer*8 iix,iiy,iix2,iiy2,iicol
#endif

      helpinsub='edgeo'  ! set for subroutine

      if(nbrdg(icomp).gt.0)then
         CALL SIGFIG(psi(ICOMP,1),5,RNO,SIGSTR,LSTR)
         CALL SIGFIG(lngth(ICOMP,1),5,RNO,SIGSTR,LSTR)
      endif
      zerolen=0.0; zeropsi=0.0; lookfor=0; INVT=0

      greyok=.false.      ! see if colour available
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Do call to FINDUA for this zone in order to get derived areas.
      IVALS(1)=icomp
      NZ=1
      call FINDUA(IVALS,nz,'p')

C Report on estimated lengths of thermal bridges from suredgeadj
      if(act(1:1).eq.'-')then
        call edisp(iuout,' ')
        call edisp(iuout,'Based on surface form & boundary condition:')
        if(bridgelen(icomp,1).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length roof-wall:',
     &        bridgelen(icomp,1)
          else
            write(outs,'(a,f7.2,a)')'bridge length roof-wall:',
     &        bridgelen(icomp,1),' (midnight blue)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,2).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-ground:',
     &        bridgelen(icomp,2)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-ground:',
     &        bridgelen(icomp,2),' (lt brown)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,3).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-wall convex:',
     &        bridgelen(icomp,3)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-wall convex:',
     &        bridgelen(icomp,3),' (forest green)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,4).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-wall concave:',
     &        bridgelen(icomp,4)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-wall concave:',
     &        bridgelen(icomp,4),' (khaki)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,5).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length exposed floor:',
     &        bridgelen(icomp,5)
          else
            write(outs,'(a,f7.2,a)')'bridge length exposed floor:',
     &        bridgelen(icomp,5),' (turquoise)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,6).gt.0.0)then
          if(MMOD.lt.8)then
           write(outs,'(a,f7.2)')'bridge length door & window lintels ',
     &        bridgelen(icomp,6)
          else
            write(outs,'(a,f7.2,a)')
     &        'bridge length door & window lintels ',
     &        bridgelen(icomp,6),' (magenta)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,7).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length door & window sills ',
     &        bridgelen(icomp,7)
          else
           write(outs,'(a,f7.2,a)')'bridge length door & window sills ',
     &        bridgelen(icomp,7),' (firebrick)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,8).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length door & window jambs ',
     &        bridgelen(icomp,8)
          else
           write(outs,'(a,f7.2,a)')'bridge length door & window jambs ',
     &        bridgelen(icomp,8),' (cyan)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,9).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-gable ',
     &        bridgelen(icomp,9)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-gable ',
     &        bridgelen(icomp,9),' (khaki)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,10).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-parapet ',
     &        bridgelen(icomp,10)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-parapet ',
     &        bridgelen(icomp,10),' (grey)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,11).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-interm-floor ',
     &        bridgelen(icomp,11)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-interm-floor ',
     &        bridgelen(icomp,11),' (royal blue)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,12).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length wall-partition ',
     &        bridgelen(icomp,12)
          else
            write(outs,'(a,f7.2,a)')'bridge length wall-partition ',
     &        bridgelen(icomp,12),' (tomato)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,13).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length glass-frame ',
     &        bridgelen(icomp,13)
          else
            write(outs,'(a,f7.2,a)')'bridge length glass-frame ',
     &        bridgelen(icomp,13),' (grey)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,15).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length user-defined-a ',
     &        bridgelen(icomp,15)
          else
            write(outs,'(a,f7.2,a)')'bridge length user-defined-a',
     &        bridgelen(icomp,15),' (pale green)'
          endif
          call edisp(iuout,outs)
        endif
        if(bridgelen(icomp,16).gt.0.0)then
          if(MMOD.lt.8)then
            write(outs,'(a,f7.2)')'bridge length user-defined-b ',
     &        bridgelen(icomp,16)
          else
            write(outs,'(a,f7.2,a)')'bridge length user-defined-b',
     &        bridgelen(icomp,16),' (orange)'
          endif
          call edisp(iuout,outs)
        endif
      endif

C If in graphic mode draw the edges with thermal bridge attributes
C in a different colour for each type (to match colours listed above).
      if(MMOD.lt.8)then
        continue
      else
        CALL INLNST(1)
        itsnm=0
        nzg=1; nznog(1)=ICOMP; izgfoc=ICOMP
        CALL redraw(IER)
        do 778 I=1,NZSUR(icomp)
          ioc=IZSTOCN(icomp,i)
          if(ioc.eq.0)then
            continue  ! unknown surface
          else
            limit=NVER(i)-1
            do 779, iyy = 1,limit
              if(ibridgeshr(ioc,iyy).ne.0)then
                izva=JVN(i,iyy)
                izvb=JVN(i,iyy+1)
                call ORTTRN(szcoords(icomp,izva,1),
     &            szcoords(icomp,izva,2),szcoords(icomp,izva,3),
     &            TSMAT,xx(1),xy(1),xz(1),ier)
                call ORTTRN(szcoords(icomp,izvb,1),
     &            szcoords(icomp,izvb,2),szcoords(icomp,izvb,3),
     &            TSMAT,xx(2),xy(2),xz(2),ier)
                call CLIPLIN(xx,xy,xz,iclp)
                if (iclp.eq.1) then
                  goto 779
                elseif (iclp.eq.-1) then
                  call CUTLIN(xx,xy,xz,iclp)
                  if (iclp.eq.-1) goto 779
                endif
                call u2pixel(xx(1),xy(1),iix,iiy)
                call u2pixel(xx(2),xy(2),iix2,iiy2)
                iicol=ibridgeshr(ioc,iyy)
                if(greyok)call winscl('z',iicol)
                call edwline(iix,iiy,iix2,iiy2)
                iicol=0
                if(greyok)call winscl('-',iicol)
                call forceflush()
              endif
  779       continue

C Complete the polygon by going back to the origin.
            if(ibridgeshr(ioc,NVER(i)).ne.0)then
              izva=JVN(i,NVER(i))
              izvb=JVN(i,1)
              call ORTTRN(szcoords(icomp,izva,1),
     &          szcoords(icomp,izva,2),szcoords(icomp,izva,3),
     &          TSMAT,xx(1),xy(1),xz(1),ier)
              call ORTTRN(szcoords(icomp,izvb,1),
     &          szcoords(icomp,izvb,2),szcoords(icomp,izvb,3),
     &          TSMAT,xx(2),xy(2),xz(2),ier)
              call CLIPLIN(xx,xy,xz,iclp)
              if (iclp.eq.1) then
                goto 778
              elseif (iclp.eq.-1) then
                call CUTLIN(xx,xy,xz,iclp)
                if (iclp.eq.-1) goto 778
              endif
              call u2pixel(xx(1),xy(1),iix,iiy)
              call u2pixel(xx(2),xy(2),iix2,iiy2)
              iicol=ibridgeshr(ioc,NVER(i))
              if(greyok)call winscl('z',iicol)
              call edwline(iix,iiy,iix2,iiy2)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif
          endif
  778   continue
      endif

C Setup phrases for the menu and for editing length and psi values.
C The exact spelling of phraselen and phrasepsi is important. The
C psi values have been taken from Accredited Construction Details 
C used in UK building regulations. Default values are VERY conservative.
      phrasemenu(1) ='a roof-wall (eave)             :'
      phraselen(1)  ='roof-wall '
      phrasepsi(1)  ='roof-wall '
      defpsi(1,1)=0.12; defpsi(1,2)=0.06; defpsi(1,3)=0.097
      defpsi(1,4)=0.069
      lenpsi(1)=bridgelen(icomp,1)
      phrasemenu(2) ='b wall-ground floor            :'
      phraselen(2)  ='wall-ground floor '
      phrasepsi(2)  ='wall-ground floor '
      defpsi(2,1)=0.32; defpsi(2,2)=0.16; defpsi(2,3)=0.059
      defpsi(2,4)=0.06
      lenpsi(2)=bridgelen(icomp,2)
      phrasemenu(3) ='c wall-wall (convex corner)    :'
      phraselen(3)  ='wall-wall (convex corner) '
      phrasepsi(3)  ='wall-wall (convex corner) '
      defpsi(3,1)=0.18; defpsi(3,2)=0.09; defpsi(3,3)=0.046
      defpsi(3,4)=0.059
      lenpsi(3)=bridgelen(icomp,3)
      phrasemenu(4) ='d wall-wall (concave corner)   :'
      phraselen(4)  ='wall-wall (concave corner) '
      phrasepsi(4)  ='wall-wall (concave corner) '
      defpsi(4,1)= 0.0; defpsi(4,2)= -0.09; defpsi(4,3)= -0.082 
      defpsi(4,4)= -0.013
      lenpsi(4)=bridgelen(icomp,4)
      phrasemenu(5) ='e wall-floor (exposed)         :'
      phraselen(5)  ='wall-floor (exposed) '
      phrasepsi(5)  ='wall-floor (exposed) '
      defpsi(5,1)=0.32; defpsi(5,2)=0.16; defpsi(5,3)=0.1 ! only default specified
      defpsi(5,4)=0.1
      lenpsi(5)=bridgelen(icomp,5)
      phrasemenu(6) ='f lintel above window or door  :'
      phraselen(6)  ='lintel above window or door '
      phrasepsi(6)  ='lintel above window or door '
      defpsi(6,1)=1.0; defpsi(6,2)=0.5; defpsi(6,3)=0.17; 
      defpsi(6,4)=0.1
      lenpsi(6)=bridgelen(icomp,6)
      phrasemenu(7) ='g Sill below window            :'
      phraselen(7)  ='Sill below window '
      phrasepsi(7)  ='Sill below window '
      defpsi(7,1)=0.08; defpsi(7,2)=0.04; defpsi(7,3)=0.038
      defpsi(7,4)=0.032
      lenpsi(7)=bridgelen(icomp,7)
      phrasemenu(8) ='h jamb at window or door       :'
      phraselen(8)  ='jamb at window or door '
      phrasepsi(8)  ='jamb at window or door '
      defpsi(8,1)=0.10; defpsi(8,2)=0.05; defpsi(8,3)=0.041 
      defpsi(8,4)=0.045
      lenpsi(8)=bridgelen(icomp,8)
      phrasemenu(9) ='i wall-gable                   :'
      phraselen(9)  ='wall-gable'
      phrasepsi(9)  ='wall-gable'
      defpsi(9,1)=0.48; defpsi(9,2)=0.24; defpsi(9,3)=0.059 
      defpsi(9,4)=0.069
      lenpsi(9)=bridgelen(icomp,9)
      phrasemenu(10)='j wall-parapet                 :'
      phraselen(10) ='wall-parapet'
      phrasepsi(10) ='wall-parapet'
      defpsi(10,1)=0.56; defpsi(10,2)=0.28; defpsi(10,3)=0.16 
      defpsi(10,4)=0.16
      lenpsi(10)=bridgelen(icomp,10)
      phrasemenu(11)='k wall-intermediate-floor      :'
      phraselen(11) ='wall-intermediate-floor'
      phrasepsi(11) ='wall-intermediate-floor'
      defpsi(11,1)=0.14; defpsi(11,2)=0.07; defpsi(11,3)=0.01 
      defpsi(11,4)=0.01
      lenpsi(11)=bridgelen(icomp,11)
      phrasemenu(12)='l wall-partition               :'
      phraselen(12) ='wall-partition'
      phrasepsi(12) ='wall-partition'
      defpsi(12,1)=0.12; defpsi(12,2)=0.06; defpsi(12,3)=0.002 
      defpsi(12,4)=0.08
      lenpsi(12)=bridgelen(icomp,12)
      phrasemenu(13)='m glass-frame                  :'
      phraselen(13) ='glass-frame'
      phrasepsi(13) ='glass-frame'
      defpsi(13,1)=0.1; defpsi(13,2)=0.07; defpsi(13,3)=.04 
      defpsi(13,4)=0.04
      lenpsi(13)=bridgelen(icomp,13)
      phrasemenu(14)='n balcony                      :'
      phraselen(14) ='balcony'
      phrasepsi(14) ='balcony'
      defpsi(14,1)=1.0; defpsi(14,2)=0.5; defpsi(14,3)=0.4 
      defpsi(14,4)=0.4
      lenpsi(14)=bridgelen(icomp,14)
      phrasemenu(15)='o user-defined a               :'
      phraselen(15) ='user-defined-a'
      phrasepsi(15) ='user-defined-a'
      defpsi(15,1)=1.0; defpsi(15,2)=1.0; defpsi(15,3)=1.0
      defpsi(15,4)=1.0
      lenpsi(15)=bridgelen(icomp,15)
      phrasemenu(16)='p user-defined b               :'
      phraselen(16) ='user-defined-b'
      phrasepsi(16) ='user-defined-b'
      defpsi(16,1)=0.00; defpsi(16,2)=0.00; defpsi(16,3)=0.00 
      defpsi(16,4)=0.0
      lenpsi(16)=1.0

C Initial questions about initial psi values.
      helptopic='psi-value-groups'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKATOG('Initial thermal bridge (psi) values:','  ',
     &  'k1 default','general accredited','2021 PartL masonry',
     &  '2021 PartL timber','exit',' ',' ',INVT,nbhelp)
      helptopic='linear_thermal_bridge'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
      if(nbrdg(icomp).eq.0)then
        if(INVT.eq.1)then
          continue
        elseif(INVT.eq.2)then
          continue
        elseif(INVT.eq.3)then
          continue
        elseif(INVT.eq.4)then
          continue
        elseif(INVT.eq.5)then
          return
        endif

C Values for constructions depending on user choice above.
        call edisp(iuout,' ')
        call edisp(iuout,'Applying iniitial psi values...')
        do itbmenu=1,16
          if(bridgelen(icomp,itbmenu).gt.0.1)then
            nbrdg(icomp)=nbrdg(icomp)+1
            ibrdg(ICOMP,nbrdg(icomp))=itbmenu
            if(INVT.eq.1) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,1)
            if(INVT.eq.2) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,2)
            if(INVT.eq.3) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,3)
            if(INVT.eq.4) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,4)
            lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,itbmenu)
C Debug.
C            write(6,*) icomp,itbmenu,nbrdg(icomp),
C     &        ibrdg(ICOMP,nbrdg(icomp)),psi(ICOMP,nbrdg(icomp))
          endif
        enddo
      else

C There were existing thermal bridge values. As user if menu display should
C reflect the recently scanned values.
        helptopic='psi-length-update'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Zone has existing psi values & lengths.',' ',
     &    'update lengths','update lengths & psi','continue',IV,nbhelp)
        if(IV.eq.1)then
          act = '-'
          call edisp(iuout,' ')
          call edisp(iuout,'Scanning the surface edges...')
          call scan_bridges(itrc,act,icomp,ier) ! Determine bridge relationships.
          call edisp(iuout,' ')
          call edisp(iuout,'Applying scanned lengths...')
          do itbmenu=1,16
            foundit=.false.
            do itb=1,nbrdg(icomp)
              if(itbmenu.eq.ibrdg(icomp,itb))then
                lngth(ICOMP,itb)=bridgelen(icomp,itbmenu)
                WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)')
     &            phrasemenu(itbmenu),
     &            psi(icomp,itb),lngth(icomp,itb)
                foundit=.true.
                cycle
              endif
            enddo
            if(.NOT.foundit.and.bridgelen(icomp,itbmenu).gt.0.0)then  ! A bridge type not yet included.
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=itbmenu
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,itbmenu)
              psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,1)               ! Assign default psi
            endif
          enddo
        elseif(IV.eq.2)then     ! Reset all engths as well as the psi values.
          nbrdg(icomp)=0
          act = '-'
          call edisp(iuout,' ')
          call edisp(iuout,'Scanning the surface edges...')
          call scan_bridges(itrc,act,icomp,ier) ! Determine bridge relationships.
          call edisp(iuout,' ')
          call edisp(iuout,'Applying scanned lengths...')
          do itbmenu=1,16
            if(bridgelen(icomp,itbmenu).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=itbmenu
              if(INVT.eq.1) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,1)
              if(INVT.eq.2) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,2)
              if(INVT.eq.3) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,3)
              if(INVT.eq.4) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,4)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,itbmenu)
C Debug.
C              write(6,*) 'x ',icomp,itbmenu,nbrdg(icomp),
C     &          ibrdg(ICOMP,nbrdg(icomp)),psi(ICOMP,nbrdg(icomp))
            endif
          enddo
        elseif(IV.eq.3)then
          do itbmenu=1,16
            if(bridgelen(icomp,itbmenu).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=itbmenu
              if(INVT.eq.1) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,1)
              if(INVT.eq.2) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,2)
              if(INVT.eq.3) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,3)
              if(INVT.eq.4) psi(ICOMP,nbrdg(icomp))=defpsi(itbmenu,4)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,itbmenu)
C Debug.
C              write(6,*) 'x ',icomp,itbmenu,nbrdg(icomp),
C     &          ibrdg(ICOMP,nbrdg(icomp)),psi(ICOMP,nbrdg(icomp))
            endif
          enddo
        endif
      endif

C Setup the editing phrases.
   33 TBRIDGE(1)='Junctions . . . . . . . . psi (W/mK) length (m)'

C Loop through the variants seeing if any of the currentlist
C matches the type in each menu position. If not write text and place
C holder, otherwise write actual values.
      do itbmenu=1,16
        if(nbrdg(icomp).gt.0)then
          foundit=.false.
          do itb=1,nbrdg(icomp)
            if(itbmenu.eq.ibrdg(icomp,itb))then
              WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)')
     &          phrasemenu(itbmenu),
     &          psi(icomp,itb),lngth(icomp,itb)
              foundit=.true.
            endif
          enddo
          if(.NOT.foundit)then
            WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)') phrasemenu(itbmenu),
     &        zeropsi,zerolen
          endif
        else
          WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)') phrasemenu(itbmenu),
     &      zeropsi,zerolen
        endif
      enddo
      TBRIDGE(18)=
     & '      _________________________________________________ '


      TBRIDGE(19)='q include % of thermal bridges'
      TBRIDGE(20)='r update summation of psi*lenth'
      TBRIDGE(21)='? help                         '
      TBRIDGE(22)='- exit to zone description     '
      ISDN=22

      INODA=-4

C  142 FORMAT(2a,f5.3,a,f7.3)

      helptopic='linear_thermal_bridge'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      IF(ACT.EQ.'s')THEN
        INODA=ISDN-2
      ELSE
        CALL EMENU('Thermal bridges',TBRIDGE,ISDN,INODA)
      ENDIF

      IF(INODA.EQ.ISDN.or.INODA.eq.20)THEN
        totheatloss(icomp) = 0.0
        do i=1,nbrdg(icomp)

C totheatloss is the total heat loss when all linear thermal bridges are added
          totheatloss(icomp) = totheatloss(icomp) +
     &                         (psi(ICOMP,i) * lngth(ICOMP,i))
        enddo
        totheatloss(icomp) = totheatloss(icomp) + losspercent(ICOMP)
        write(outs,*) 'total losses: ',totheatloss(icomp),' W/K'
        call edisp(iuout,outs)

        call edisp(iuout,
     &    'And remember SAVE this zone to record the t bridge data.')

C Jump back to the main geometry menu.
        if(INODA.EQ.ISDN) return
      ELSEIF(INODA.EQ.ISDN-1)THEN

C Produce help text for the menu.
        CALL PHELPD('thermal bridge menu',nbhelp,'-',0,0,IER)

      ELSEIF(INODA.EQ.2)THEN

C Is any one of the current thermal bridges type 1 "roof-wall (eave)"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(1,1),
     &    defpsi(1,2),defpsi(1,3),defpsi(1,4)
        call edisp(iuout,outs)
        lookfor=1
        goto 42
      ELSEIF(INODA.EQ.3)THEN

C Is any one of the current thermal bridges type 2 "wall-ground floor"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(2,1),
     &    defpsi(2,2),defpsi(2,3),defpsi(2,4)
        call edisp(iuout,outs)
        lookfor=2
        goto 42
      ELSEIF(INODA.EQ.4)THEN

C Is any one of the current thermal bridges type 3 "wall-wall (convex corner)"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(3,1),
     &    defpsi(3,2),defpsi(3,3),defpsi(3,4)
        call edisp(iuout,outs)
        lookfor=3
        goto 42
      ELSEIF(INODA.EQ.5)THEN

C Is any one of the current thermal bridges type 4 "wall-wall (concave corner)"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(4,1),
     &    defpsi(4,2),defpsi(4,3),defpsi(4,4)
        call edisp(iuout,outs)
        lookfor=4
        goto 42
      ELSEIF(INODA.EQ.6)THEN

C Is any one of the current thermal bridges type 5 "wall-floor (exposed)"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(5,1),
     &    defpsi(5,2),defpsi(5,3),defpsi(5,4)
        call edisp(iuout,outs)
        lookfor=5
        goto 42
      ELSEIF(INODA.EQ.7)THEN

C Is any one of the current thermal bridges type 6 "lintel above window or door"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(6,1),
     &    defpsi(6,2),defpsi(6,3),defpsi(6,4)
        call edisp(iuout,outs)
        lookfor=6
        goto 42
      ELSEIF(INODA.EQ.8)THEN

C Is any one of the current thermal bridges type 7 "Sill below window"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(7,1),
     &    defpsi(7,2),defpsi(7,3),defpsi(7,4)
        call edisp(iuout,outs)
        lookfor=7
        goto 42
      ELSEIF(INODA.EQ.9)THEN

C Is any one of the current thermal bridges type 8 "jamb at window or door"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(8,1),
     &    defpsi(8,2),defpsi(8,3),defpsi(8,4)
        call edisp(iuout,outs)
        lookfor=8
        goto 42
      ELSEIF(INODA.EQ.10)THEN

C Is any one of the current thermal bridges type 9 "roof-wall-gable"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(9,1),
     &    defpsi(9,2),defpsi(9,3),defpsi(9,4)
        call edisp(iuout,outs)
        lookfor=9
        goto 42
      ELSEIF(INODA.EQ.11)THEN

C Is any one of the current thermal bridges type 10 "wall-parapet"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(10,1),
     &    defpsi(10,2),defpsi(10,3),defpsi(10,4)
        call edisp(iuout,outs)
        lookfor=10
        goto 42
      ELSEIF(INODA.EQ.12)THEN

C Is any one of the current thermal bridges type 11 "wall-interm-floor"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(11,1),
     &    defpsi(11,2),defpsi(11,3),defpsi(11,4)
        call edisp(iuout,outs)
        lookfor=11
        goto 42
      ELSEIF(INODA.EQ.13)THEN

C Is any one of the current thermal bridges type 12 "wall-partition"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(12,1),
     &    defpsi(12,2),defpsi(12,3),defpsi(12,4)
        call edisp(iuout,outs)
        lookfor=12
        goto 42
      ELSEIF(INODA.EQ.14)THEN

C Is any one of the current thermal bridges type 13 "glass-frame"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(13,1),
     &    defpsi(13,2),defpsi(13,3),defpsi(13,4)
        call edisp(iuout,outs)
        lookfor=13
        goto 42
      ELSEIF(INODA.EQ.15)THEN

C Is any one of the current thermal bridges type 14 'balcony'
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(14,1),
     &    defpsi(14,2),defpsi(14,3),defpsi(14,4)
        call edisp(iuout,outs)
        lookfor=14
        goto 42
      ELSEIF(INODA.EQ.16)THEN

C Is any one of the current thermal bridges type 15 "user-defned-a"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(15,1),
     &    defpsi(15,2),defpsi(15,3),defpsi(15,4)
        call edisp(iuout,outs)
        lookfor=15
        goto 42
      ELSEIF(INODA.EQ.17)THEN

C Is any one of the current thermal bridges type 16 "user-defined-b"
        write(outs,'(a,4f6.3)') 'Psi value range is ',defpsi(16,1),
     &    defpsi(16,2),defpsi(16,3),defpsi(16,4)
        call edisp(iuout,outs)
        lookfor=16
        goto 42

      ELSEIF(INODA.EQ.ISDN-3)THEN
        IF(ACT.EQ.'s')THEN
          THBRPERCENT=THPER
        ELSE
          CALL EASKR(thbrpercent,' ',
     &    'thermal bridges as a fraction of UA (values from 0 to 1)',
     &     -0.1,'F',1.0,'W',0.0,'fraction',IER,nbhelp)
          context=.true.
          CALL SURINFO(ICOMP,iuout,context)
        ENDIF

C Include the user defined percentage to get the zone's total fabric loss
        uavtotal(ICOMP) = uavgtran(ICOMP) + uavwall(ICOMP) +
     &    uavslproof(ICOMP) + uavfltroof(ICOMP) + uavgsky(ICOMP)
        losspercent(ICOMP) = thbrpercent * uavtotal(ICOMP)
        call rel16str(losspercent(ICOMP),t16b,lnb,ier)
        write(outs,'(3a)')' Total percentage loss: ',
     &        t16b(1:lnb),' W/K'
        call edisp(iuout,outs)
      ENDIF

C If called silently then return
      IF(ACT.EQ.'s')THEN
        totheatloss(icomp) = losspercent(ICOMP)
        RETURN
      ENDIF

C return to the thermal bridges menu
      goto 33

C Edit the length and psi values for the type selected. First look
C and see which item matches lookfor and edit that. If none matches
C increment nbrdg() and edit. If there are no bridges then increment
C nbrdg and edit.
  42  if(nbrdg(icomp).gt.0)then
        foundit=.false.
        do 31,ibj=1,nbrdg(icomp)
          if(ibrdg(ICOMP,ibj).eq.lookfor)then
            foundit=.true.
            psiv=psi(ICOMP,ibj)
            write(msg,'(a,F7.4,a)')'(accredited default psi:',
     &        defpsi(lookfor,1),' W/mK)'

            CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &        -2.0,'W',2.0,'W',defpsi(lookfor,1),'psi',IER,nbhelp)
            psi(ICOMP,ibj)=psiv

            write(msg,'(a,F7.4,a)')'(suggested length:',
     &        lenpsi(lookfor),' m)'
            if(lngth(ICOMP,ibj).lt.0.01)then
              lenv=lenpsi(lookfor)
            else
              lenv=lngth(ICOMP,ibj)
            endif
            CALL EASKR(lenv,phraselen(lookfor),msg,
     &        -0.1,'W',200.0,'W',0.0,'length',IER,nbhelp)
            lngth(ICOMP,ibj)=lenv
          endif
  31    continue
        if(.NOT.foundit)then
          nbrdg(icomp)=nbrdg(icomp)+1
          ibrdg(ICOMP,nbrdg(icomp))=lookfor
          psiv=0.0
          write(msg,'(a,F7.4,a)')'(accredited default psi:',
     &      defpsi(lookfor,1),' W/mK)'
          CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &      -2.0,'W',2.0,'W',defpsi(lookfor,1),'psi',IER,nbhelp)
          psi(ICOMP,nbrdg(icomp))=psiv

          write(msg,'(a,F7.4,a)')'(suggested length:',
     &      lenpsi(lookfor),' m)'
          lenv=lenpsi(lookfor)
          CALL EASKR(lenv,phraselen(lookfor),msg,
     &      -0.1,'W',200.0,'W',lenpsi(lookfor),'length',IER,nbhelp)
          lngth(ICOMP,nbrdg(icomp))=lenv

        endif
      else
        nbrdg(icomp)=nbrdg(icomp)+1
        ibrdg(ICOMP,nbrdg(icomp))=lookfor
        psiv=0.0
        write(msg,'(a,F7.4,a)')'(accredited default psi:',
     &    defpsi(lookfor,1),' W/mK)'
        CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &    -2.0,'W',2.0,'W',defpsi(lookfor,1),'psi',IER,nbhelp)
        psi(ICOMP,nbrdg(icomp))=psiv

        lenv=0.0
        CALL EASKR(lenv,phraselen(lookfor),
     &    '(default length: 0 m)',
     &    -0.1,'W',200.0,'W',0.0,'length',IER,nbhelp)
        lngth(ICOMP,nbrdg(icomp))=lenv

      endif
      goto 33
      END

C ********** updatesvfc **************
C Review surface azimuth and elevation and update SVFC
       subroutine updatesvfc(icomp,modgeo)

C It assumes that the zone geometry has just been read and that
C common blocks ?? are current.

C It is passed the current zone index (izone) and returns
C modgeo=.true. if a change to common blocks G5 and G6 have
C been made.

#include "building.h"
#include "net_flow.h"
#include "geometry.h"

      integer IZSTOCN  ! for each zone:surface a pointer to connection index.
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer IUOUT,IUIN,IEOUT
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      logical MODGEO
      character msg*42

C Loop through each surface and detect whether orientation of
C the surface has changed and needs to be recognised. If the surface
C USE attribute is furniture then do not include in base surface count.
      DO 41 I=1,NZSUR(icomp)
        ioc=IZSTOCN(icomp,i)
        if(ioc.lt.1)then
          write(msg,'(a,2i4)') 'No connection for zone surf',icomp,i
          call edisp(iuout,msg)
        else
          if(SPELV(icomp,i).GE.-1.5.AND.SPELV(icomp,i).LE.1.5)then
            if(SVFC(icomp,i).NE.'VERT')then
              MODGEO=.TRUE.
              SVFC(icomp,i)='VERT'
            endif
          elseif(SPELV(icomp,i).GE.88.5.AND.SPELV(icomp,i).LE.91.5)then
            if(SVFC(icomp,i).NE.'CEIL')then
              SVFC(icomp,i)='CEIL'
              MODGEO=.TRUE.
              if(SUSE(icomp,I,1)(1:5).eq.'FURNI')then
                continue           ! do not include in ceiling list
              elseif(SUSE(icomp,I,1)(1:6).eq.'REVEAL')then
                continue           ! do not include in ceiling list
              else
                izsceil(icomp)=I   ! identify as a ceiling
              endif
            endif
          elseif(SPELV(icomp,i).GE.-91.5.AND.
     &           SPELV(icomp,i).LE.-88.5)then
            if(SVFC(icomp,i).NE.'FLOR')then
              SVFC(icomp,i)='FLOR'
              MODGEO=.TRUE.
              if(SUSE(icomp,I,1)(1:5).eq.'FURNI')then
                continue           ! do not include in floor list
              elseif(SUSE(icomp,I,1)(1:6).eq.'REVEAL')then
                continue           ! do not include in floor list
              else
                izsfloor(icomp)=I  ! identify as a floor
              endif
            endif
          else
            if(SVFC(icomp,i).NE.'SLOP')then
              SVFC(icomp,i)='SLOP'
              MODGEO=.TRUE.
            endif
          endif
        endif
   41 CONTINUE

      return
      end

C ************* CPVERT
C Present list of vertex for copy via a paging menu. Up to 10
C verticies can be copied. No zone graphic information is
C displayed at this time, based on global data.
      SUBROUTINE CPVERT(ICOMP,NVC,CX,CY,CZ,IER)
#include "epara.h"
#include "building.h"
#include "geometry.h"
#include "help.h"

C Passed parameters
      integer icomp  ! zone to look in
      integer NVC    ! number of selected verticies
      real CX,CY,CZ  ! XYZ of selected verticies
      dimension CX(10),CY(10),CZ(10)
      integer IER    ! IER=0 OK, IER=1 problem.

      common/OUTIN/IUOUT,IUIN,IEOUT
      DIMENSION VERT(35)
      CHARACTER VERT*34,KEY*1,head*28
      character outs*124
      integer MVERT,IVERT ! max items and current menu item

      helpinsub='edgeo'  ! set for subroutine

      NVC=0    ! assume nothing selected yet

      do 42 iv=1,10  ! clear the return array
        CX(iv)=0.0; CY(iv)=0.0; CZ(iv)=0.0
  42  continue

C Initialise zone vertex menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=3
      ILEN=NZTV(ICOMP)
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NZTV(ICOMP)
      IVERT=-3

C Loop through the items until the page to be displayed. M is the
C current menu line index. Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)KEY,L,szcoords(ICOMP,L,1),
     &      szcoords(ICOMP,L,2),szcoords(ICOMP,L,3)
   14     FORMAT(A,I4,3F9.3)
        ENDIF
   10 CONTINUE

      VERT(1)    =' Index |X-coord|Y-coord|Z-coord'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='? help                           '
      VERT(M+3)  ='- exit menu                      '

C Help text for this menu.
      helptopic='surface_vertex_copy'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(3A)')' Vertices in ',zname(ICOMP)
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Pass back the relevant items to CX CY CZ.
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        helptopic='vert_copy_other'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('vertex copy',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Select vertex identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        if(IFOC.ne.0.and.NVC.lt.10)then
          NVC=NVC+1    ! increment selected
          CX(NVC)=szcoords(ICOMP,IFOC,1)
          CY(NVC)=szcoords(ICOMP,IFOC,2)
          CZ(NVC)=szcoords(ICOMP,IFOC,3)
          write(outs,'(A,A12,A)')'From zone `',zname(ICOMP),'`'
          call edisp(iuout,outs)
          write(outs,'(a,i3,a,3F9.4)') 'Selected vert ',IFOC,' @ ',
     &      CX(NVC),CY(NVC),CZ(NVC)
          call edisp(iuout,outs)
        endif
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      end

C linesatz ****************
C Scan current zone geometry for edges which are close to
C an axis X Y or Z and test value (atest). Store them in jvnatz.
      subroutine linesatz(icomp,axis,atest)
#include "building.h"
#include "model.h"
#include "geometry.h"

      integer icomp  ! the zone
      character axis*1
      real atest
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC
      integer jvnatz  ! jvn instantiated only for zlevel
      integer vertatz ! vertex index close to zlevel (zero ignore)
      common/closedge/jvnatz(MS,MV),vertatz(MTV)
      character outs*248
      integer loop,loop2
      logical close,surftoinclude
      real tx,ty,tz  ! current coord

      write(outs,'(3a,f6.3)') 'Edges close to ',axis,' @',atest
      call edisp(iuout,outs)
      do loop=1,NSUR
        surftoinclude=.false.
        DO loop2 = 1,NVER(loop)
          if(axis(1:1).eq.'X')then
            tx = X(JVN(loop,loop2))
            call eclose(atest,tx,CACC,close)
          elseif(axis(1:1).eq.'Y')then
            ty = Y(JVN(loop,loop2))
            call eclose(atest,ty,CACC,close)
          elseif(axis(1:1).eq.'Z')then
            tz = Z(JVN(loop,loop2))
            call eclose(atest,tz,CACC,close)
          endif
          if(close)then
            jvnatz(loop,loop2)=JVN(loop,loop2)
            vertatz(JVN(loop,loop2))=1 ! of interest
            surftoinclude=.true.
          else
            jvnatz(loop,loop2)=0       ! not of interest
            vertatz(JVN(loop,loop2))=0 ! not of interest
          endif
        enddo  ! of loop2
        if(axis(1:1).eq.'X')then
          write(outs,'(2a,99i4)') 'jvnx ',sname(icomp,loop),
     &      (jvnatz(loop,j),j=1,NVER(loop))
          if(surftoinclude)call edisp248(iuout,outs,100)
        elseif(axis(1:1).eq.'Y')then
          write(outs,'(2a,99i4)') 'jvny ',sname(icomp,loop),
     &      (jvnatz(loop,j),j=1,NVER(loop))
          if(surftoinclude)call edisp248(iuout,outs,100)
        elseif(axis(1:1).eq.'Z')then
          write(outs,'(2a,99i4)') 'jvnz ',sname(icomp,loop),
     &      (jvnatz(loop,j),j=1,NVER(loop))
          if(surftoinclude)call edisp248(iuout,outs,100)
        endif
      enddo  ! of loop
      write(outs,'(3a,f6.3)') 'Vertices close to axis ',axis,' @',atest
      call edisp(iuout,outs)
      outs=' '
      do loop=1,NTV
        if(vertatz(loop).ne.0)then
          write(outs,'(a,i3,3f7.3)')'vert',loop,X(loop),Y(loop),Z(loop)
          call edisp248(iuout,outs,100)
        endif
      enddo  ! of loop
      return
      end

C pointmergewithinlinesatz ****************
C After call to linesatz find if a point (px py pz) is close to
C one of the edges in jvnatz and calculate the coord (rx,ry,rz)
C if merged. Set ok 1 if px py pz was within ?mm of one of the
C edges.
      subroutine pointmergewithinlinesatz(icomp,px,py,pz,rx,ry,rz,ok)
#include "building.h"
#include "model.h"
#include "geometry.h"

      integer icomp  ! the zone
      real px,py,pz,rx,ry,rz
      integer ok
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer jvnatz  ! jvn instantiated only for zlevel
      integer vertatz ! vertex index close to zlevel (zero ignore)
      common/closedge/jvnatz(MS,MV),vertatz(MTV)
      character outs*248
      integer loop,loop2
      logical match

C For each surface and each non-zero edge pair. Also check
C last vert to first vert.
      do loop=1,NSUR
        DO loop2 = 1,NVER(loop)-1
          if(jvnatz(loop,loop2).ne.0.and.
     &       jvnatz(loop,loop2+1).ne.0)then
            xp1=X(jvnatz(loop,loop2)); yp1=Y(jvnatz(loop,loop2))
            zp1=Z(jvnatz(loop,loop2))
            xp2=X(jvnatz(loop,loop2+1)); yp2=Y(jvnatz(loop,loop2+1))
            zp2=Z(jvnatz(loop,loop2+1))
            call pointtoline3d(px,py,pz,xp1,yp1,zp1,xp2,yp2,zp2,
     &        offset,match)
            if(offset.lt.0.1.and.match)then
C Debug.
C              write(6,*) 'might be',px,py,pz,' and ',
C     &          jvnatz(loop,loop2),jvnatz(loop,loop2+1),offset

C If the distance is greater than 1mm get the distance between the
C start of the line (iwhich1) and the test point (iwhich3) and do this
C for the end point on the line (iwhich2) and the test point.
              tdis= crowxyz(xp1,yp1,zp1,xp2,yp2,zp2)
              vdislsp=crowxyz(xp1,yp1,zp1,px,py,pz)
              write(outs,'(a,F8.4)')'Line start->unaligned vertex is',
     &          vdislsp
              call edisp(itru,outs)

C Use square root of (vdislsp^2 - offset^2)
              aligndis = SQRT((vdislsp * vdislsp) - (offset * offset))

C Use ratio calculation to make an aligned point vdislsp along the line.
              r2 = tdis - aligndis
              r1 = aligndis
              rx = ((r2 * xp1) + (r1 * xp2))/tdis
              ry = ((r2 * yp1) + (r1 * yp2))/tdis
              rz = ((r2 * zp1) + (r1 * zp2))/tdis
              write(outs,'(a,3f10.5)')' Aligned vertex @ X,Y,Z:',
     &          rx,ry,rz
              call edisp(itru,outs)
              ok=1
            endif
          endif
        enddo  ! loop2
      enddo    ! loop
      return
      end


C POINTTOLINE3D: determines distance from a 3D point to a 3D line.
C where px,py,pz is the test vertex, iwhich1 is the index
C of the vertex at the start of the line, iwhich2 is the index of the
C index at the end of the line, offset is the distance (m), match is
C a logical set to true if close enough.
C Only returns match=true if point was found along the line between
C the two vertices (i.e. it discards matches beyond the end points.
C It assumes that calling code will decide whether the distance
C can be used.
      subroutine pointtoline3d(px,py,pz,xp1,yp1,zp1,xp2,yp2,zp2,
     &  offset,match)
#include "building.h"
#include "geometry.h"
      dimension vd(3),vd1(3),vd2(3)
      logical match

C Assme match=false.
      match=.false.

C Report length of line. Use method of Ward/Radiance in fvect.c
      vd(1)= xp2-xp1
      vd(2)= yp2-yp1
      vd(3)= zp2-zp1
      call dot3(vd,vd,vdis)
      vd1(1)= px-xp1
      vd1(2)= py-yp1
      vd1(3)= pz-zp1
      call dot3(vd1,vd1,vdis1)
      vd2(1)= px-xp2
      vd2(2)= py-yp2
      vd2(3)= pz-zp2
      call dot3(vd2,vd2,vdis2)
      if(vdis2.gt.vdis1)then
        if((vdis2 - vdis1).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      else
        if((vdis1 - vdis2).gt.vdis)then
          match=.false.
          offset=0.0
          return
        endif
      endif
      if(vdis.ne.0.0)then

C The original C code returned the square of distance
C so unpack via sqrt call.
        d2l=(vdis1-(vdis+vdis1-vdis2)*
     &      (vdis+vdis1-vdis2)/vdis/4.0)
        if(abs(d2l).lt.0.0003)then
          offset=d2l   ! if really small d2l
        else
          offset=SQRT(d2l)
        endif
        match=.true.
      else
        offset=0.0   ! vdis was zero so assume a match
        match=.true.
      endif
      return
      end

C ********************* ESURREVEAL 
C ESURREVEAL Offsets a child surface along surface normal and creates reveal
C surfaces as required. Assumes that common block G1,GS5,GS6 are current.
      SUBROUTINE ESURREVEAL(ICOMP,IRS,RTK,revroot,mat,IER)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "model.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer icomp   ! zone index
      integer irs     ! selected surface to transform
      real RTK        ! projection (m) of the reveal
      character revroot*8  ! root name of the set of reveals
      character mat*32  ! reveal construction
      integer IER     ! if zero ok

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer IZSTOCN
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72

      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      DIMENSION  XT(MV),YT(MV),ZT(MV)
      DIMENSION  jvnorig(MV)

      CHARACTER SNAME1*12,SNAMED*12
      CHARACTER ZSDES*28
      character t14*14,d14*14  ! for duplicat name editing
      logical newgeo  ! to use for testing if new/old geometry file.
      logical nameok
      logical goforit ! if true then update flow network files

      helpinsub='edgeo'  ! set for subroutine

C Check if ok to add a reveal surface for each edge.
      if(nzsur(icomp)+NVER(IRS).GT.MS)then
        call usrmsg('Reveal not added, run out of surfaces',
     &    'in this zone. ','W')
        return
      endif

      newgeo=.false.  ! assume older format geometry.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Create working vertices of the selected surface.
      helptopic='surface_reveal'
      call gethelptext(helpinsub,helptopic,nbhelp)

      vdis= RTK  ! Offset is thickness.
      snamed=' '
      DX=0.0; DY=0.0; DZ=0.0  ! reset the delta in each axis

C Use logic similar to shifting along surface normal near line
C 8400. Then re-asign the surface to the newly transformed vertices.
C And then create the edges needed for the reveal surfaces.

C Also look at logic for extruding edges from a surface near
C line 9074.
      N = NVER(IRS)
      DO J = 1,N
        jvnorig(j)=0 ! clear
      ENDDO

C Loop once for the shifted and then again for each new edge.  .
      icount=0  ! icount keeps track of which surface is being composed
      ibaseedge=NVER(IRS)  ! nb of edges in the original surface
      ibasecount=NVER(IRS) ! counter for original edges left to process
      irsedge=NVER(IRS)    ! nb of edges in the shifted surface
      irscount=NVER(IRS)   ! counter for shifted edges left to process
      iwcount=(NVER(IRS))+1  ! iwcount is the total number of new surfaces

 141  icount=icount+1
      if(icount.eq.1)then

C The shifted surface has the same number of verticies as IRS and
C edge-ordered in same direction. The new vertices are generated via
C filling XX YY ZZ arrays with points for surface IRS then call to TRANSUR.
C vdis is the distance along the surface normal (e.g. if the initial
C surface is the base then vdis should be negative).
        N = ibaseedge
        DO J = 1,N
          XX(J) = X(JVN(IRS,J))
          YY(J) = Y(JVN(IRS,J))
          ZZ(J) = Z(JVN(IRS,J))
          jvnorig(j)=jvn(IRS,J)  ! remember original edge list
        ENDDO

        CALL TRANSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,ZSDES)
        DX=XX(1)-XT(1)  ! get the delta in each axis
        DY=YY(1)-YT(1)
        DZ=ZZ(1)-ZT(1)

C Add the transformed points to the zone (later check for unique).
        do ix = 1,ibaseedge
          if(NTV+1.le.MTV)then
            NTV=NTV+1
            NZTV(icomp)=NTV
            X(NTV)=XT(ix); Y(NTV)=YT(ix); Z(NTV)=ZT(ix)
            szcoords(ICOMP,ntv,1)=XT(ix)
            szcoords(ICOMP,ntv,2)=YT(ix)
            szcoords(ICOMP,ntv,3)=ZT(ix)
            XMN=AMIN1(XMN,X(NTV)); YMN=AMIN1(YMN,Y(NTV))
            ZMN=AMIN1(ZMN,Z(NTV)); XMX=AMAX1(XMX,X(NTV))
            YMX=AMAX1(YMX,Y(NTV)); ZMX=AMAX1(ZMX,Z(NTV))
            JVN(IRS,ix)=ntv    ! re-asign
            iszjvn(icomp,IRS,ix)=JVN(irs,ix)
          endif
        enddo
        NVER(IRS)=ibaseedge
        isznver(icomp,irs)=NVER(irs)
        goto 141     ! jump to process the reveals.
      else

C Begin with the last edge in the original surface and map that to
C the transformed points. For the last surface use ibaseedge rather
C than ibasecount.
        if(ibasecount.eq.1)then
          iv1=jvnorig(ibaseedge)
        else
          iv1=jvnorig(ibasecount-1)
        endif
        if(ibasecount.eq.1)then
          iv2=jvnorig(1)
        else
          iv2=jvnorig(ibasecount)
        endif
        if(irscount.eq.1)then
          iv3=JVN(IRS,1)
        else
          iv3=JVN(IRS,irscount)
        endif
        if(irscount.eq.1)then
          iv4=JVN(IRS,irsedge)
        else
          iv4=JVN(IRS,irscount-1)
        endif
C Debug.
C        write(6,'(a,4i3)') ' iv1 iv2 iv3 iv4 ',iv1,iv2,iv3,iv4
        JVN(NSUR+1,1)=iv1; JVN(NSUR+1,2)=iv2
        JVN(NSUR+1,3)=iv3; JVN(NSUR+1,4)=iv4
        iszjvn(icomp,nsur+1,1)=JVN(nsur+1,1)
        iszjvn(icomp,nsur+1,2)=JVN(nsur+1,2)
        iszjvn(icomp,nsur+1,3)=JVN(nsur+1,3)
        iszjvn(icomp,nsur+1,4)=JVN(nsur+1,4)
        NVER(NSUR+1)=4
        isznver(icomp,nsur+1)=NVER(NSUR+1)

        if(icount-1.le.9)then
          write(SNAMED,'(a,i1,a)')'rev_',icount-1,revroot(1:7)
          write(SNAME1,'(a,i1,a)')'rev_',icount-1,revroot(1:7)
        else
          write(SNAMED,'(a,i2,a)')'rev_',icount-1,revroot(1:6)
          write(SNAME1,'(a,i2,a)')'rev_',icount-1,revroot(1:6)
        endif

C Update the counters (ibasecount irscount decrements).
        ibasecount=ibasecount-1
        irscount=irscount-1
      endif

C Check that suraface name is unique.
      icmpall=0 ! force compare of all surfaces
      call snamdup(SNAME1,icomp,icmpall,nameok)
      if(.NOT.nameok)then

C Copy to a slightly longer buffer for editing and ensure that
C the final name uses the allowed character set.
        write(T14,'(2a)') SNAME1(1:12),'  '
        write(D14,'(2a)') SNAMED(1:12),'  '
        CALL EASKS(T14,'Revised surface name',
     &    '(duplicate found)',14,D14,'surface name',IER,nbhelp)
        if(T14(1:2).NE.'  ')then
          write(SNAME1,'(a)') T14(1:12)
        else
          goto 141
        endif
      endif

C Up the surface count and zone surface attributes. Because the
C user cannot yet see the newly extruded surface it is pointless to
C ask them about its use and composition.
      ICONT=IZSTOCN(icomp,nsur)+1
      NSUR=NSUR+1
      NZSUR(ICOMP)=NZSUR(ICOMP)+1
      SNAME(ICOMP,NSUR)=SNAME1
      write(SMLCN(ICOMP,NSUR),'(a)') mat(1:lnblnk(mat))
      SVFC(ICOMP,NSUR)='UNKN'
      SOTF(ICOMP,NSUR)='OPAQUE'
      SOTHER(ICOMP,NSUR,1)='UNKNOWN'
      SOTHER(ICOMP,NSUR,2)='0'
      SOTHER(ICOMP,NSUR,3)='0'
      SPARENT(ICOMP,NSUR)='-'
      SUSE(ICOMP,NSUR,1)='REVEAL'
      SUSE(ICOMP,NSUR,2)='-'

C Add surface to the connection list (icont) assuming an unknown boundary.
      call addedsurf(icomp,icont,1,ier)

C If original surface faces the outside then set this for the
C reveals that have been created.
      icorig=IZSTOCN(ICOMP,IRS)
      if(ICT(icorig).eq.0)then
        ICT(icont)=0      
        SOTHER(ICOMP,NSUR,1)='EXTERIOR'
      endif

C If icount is less than iwcount loop back other wise jump to point
C where geometry and configuration is saved and re-display managed.
      if(icount.lt.iwcount)then
        goto 141
      else

C Completed all of the reveals, check if surface IRS has an
C associated flow component and if so transform.

C Normal shift of any associated flow node will need a similar transform.
        if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
          goforit=.false.
          call doesflowrefsurface(icomp,is,inod,icmp)
          if(inod.gt.0.or.icmp.gt.0)call usrmsg(
     &      'Surface shift may require updating of flow network',
     &      'components. Please check!','W')
          if(inod.gt.0)then
            HNOD(INOD,1)=HNOD(INOD,1)+DX
            HNOD(INOD,2)=HNOD(INOD,2)+DY
            HNOD(INOD,3)=HNOD(INOD,3)+DZ
            goforit=.true.
          endif
          if(icmp.gt.0)then
            HCMP(ICMP,1,1)=HCMP(ICMP,1,1)+DX
            HCMP(ICMP,1,2)=HCMP(ICMP,1,2)+DY
            HCMP(ICMP,1,3)=HCMP(ICMP,1,3)+DZ
            goforit=.true.
          endif
          if(goforit)then
            call updatebothflownetworks(ier)
          endif
        endif
        if(.NOT.newgeo)then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        call zgupdate(1,icomp,ier)  ! update commons
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

        call usrmsg(
     &    'Remember to attribute the reveal surfaces and it',
     &    'is recommended that you turn on surface normals!','W')
        return
      endif

      RETURN
      END   ! of ESURREVEAL


C ******* scan_bridges
C Detects polygon edge associations related to thermal bridges.
c Assumes that the surface is in the current zones common blocks.
C act is '-' derive thermal bridgege data.
C Assumes that suredgeadj has been recently called.

      subroutine scan_bridges(itrc,act,izon,ier)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

      real bridgelen ! Length (m) of potential thermal bridges in each zone.
                     ! See code below for what each means.
      common/gbridge/bridgelen(MCOM,16)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      CHARACTER outs*148,act*1
      character ZSDESC*20,ZSDESCO*20,ZSDESS*16,ZSDESSO*20
      character zsn*28,zsno*28
      logical closeazi,ok

C iwhich is local variable to sort most often referenced adjacent surf.
      dimension idedge(MV*MV,3)
      DIMENSION xx(2),xy(2),xz(2)
      real va,vb,pa,pb
      dimension va(3),vb(3),pa(3),pb(3),vaa(3),vbb(3)
      real ux,uy,uz  ! unit vector along the edge
      real dista,distc
      logical closeux,closeuy,closeuz
      logical greyok

#ifdef OSI
      integer iix,iiy,iix2,iiy2,iicol
#else
      integer*8 iix,iiy,iix2,iiy2,iicol
#endif

      h(1)='Logic looks at edges & constructions & use to determine'
      h(2)='likely categories of thermal bridges.'
      h(3)=' '
      h(4)='You are asked to supply opinions for ambigious cases.'
      h(5)='in the current zone.'
      nbhelp=5

      if(izon.eq.0.or.izon.gt.NCOMP)then
        ier=1
        return
      endif

C Zero the lengths of various thermal bridge type lengths.
      do ij=1,16
          bridgelen(izon,ij)=0.0
      enddo

C Rest the bookkeepping for whether edges have been dealt with.
      icuredge=0
      do ijj=1,MV*MV  ! clear idoneedge
        idedge(ijj,1)=0
        idedge(ijj,2)=0
        idedge(ijj,3)=0
      enddo

C If in graphic mode draw the edges with thermal bridge attributes
C in a different colour for each type (to match colours listed above).
      if(MMOD.lt.8)then
        continue
      else

        greyok=.false.      ! see if colour available
        if(nifgrey.gt.4)then
          greyok=.true.
        endif
        CALL INLNST(1)
        itsnm=0
        nzg=1; nznog(1)=izon; izgfoc=izon
        CALL redraw(IER)
      endif

      do 43 isurf=1,NZSUR(izon)
        icc=izstocn(izon,isurf)
        if(icc.eq.0)then
          write(outs,'(a,i2,a,i3,a)') ' Zone ',izon,' surface ',
     &      isurf,' is an unknown connection, skipping scan_bridges.'
          call edisp(iuout,outs)
          ier=1
          return
        endif

C Step through the vertices of each edge in turn and check to see
C what other surfaces are associated with edge. j & k
C are the vertices at the ends of the edge being tested. 
        list=NVER(isurf)
        do 42 i=1,list      ! loop through each vertex
          ibridgeshr(icc,i)=0 ! begin by assuming no bridge locations
          j=JVN(isurf,i)
          if(i.lt.NVER(isurf))then
            k=JVN(isurf,i+1)
          else
            k=JVN(isurf,1)
          endif

C Find screen location of the current edge.
          izva=j; izvb=k
          call ORTTRN(szcoords(izon,izva,1),
     &      szcoords(izon,izva,2),szcoords(izon,izva,3),
     &      TSMAT,xx(1),xy(1),xz(1),ier)
          call ORTTRN(szcoords(izon,izvb,1),
     &      szcoords(izon,izvb,2),szcoords(izon,izvb,3),
     &      TSMAT,xx(2),xy(2),xz(2),ier)
          call CLIPLIN(xx,xy,xz,iclp)

          do 44 iosurf=1,NZSUR(izon)
            icco=izstocn(izon,iosurf)
            if(icco.eq.0) goto 44
            if(iosurf.eq.isurf) goto 44

C For other surface, loop through each edge and check if they match.
C (by definition, they will be in reversed order so test jj against k).
C If an edge matches, test if other surface is facing the same way.
            listo=NVER(iosurf)
            do 45 mm=1,listo
              jj=JVN(iosurf,mm)
              if(mm.lt.NVER(iosurf))then
                kk=JVN(iosurf,mm+1)
              else
                kk=JVN(iosurf,1)
              endif
              if(j.eq.0.or.k.eq.0)then
                continue   ! skip past zero indices
              elseif(j.eq.kk.and.k.eq.jj)then

C Find length of the current edge. And remember which vertices.
                edgelen= crowxyz(X(j),Y(j),Z(j),X(k),Y(k),Z(k))
                icuredge=icuredge+1
                idedge(icuredge,1)=j
                idedge(icuredge,2)=k
                idedge(icuredge,3)=0

C Has this edge already been referenced?
                do iq=1,icuredge
                  if(idedge(iq,1).gt.0.and.idedge(iq,2).gt.0)then
                    if(idedge(iq,1).eq.k.and.idedge(iq,2).eq.j)then
                      if(idedge(iq,3).eq.1)then
C                       write(6,*) 'edge at v v ',j,k,' matches prior'
                        goto 45  ! skip to the next edge.
                      elseif(idedge(iq,3).eq.-1)then
C                        write(6,*) 'edge at v v ',j,k,' user declined'
                        goto 45  ! skip to the next edge.
                      endif
                    endif
                  endif
                enddo

C Tests for ibridgeshr (edges that might represent thermal bridges.
C The first filter is that both surfaces are facing outside
                if(ICT(icc).eq.0.and.ICT(icco).eq.0)then
                  call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                  call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                  if(itrc.ge.1)then
                    write(outs,'(14a,4i4)') 'facade ',
     &                zsn(1:lnblnk(zsn)),
     &                ' edge w ',zsno(1:lnblnk(zsno)),' p ',
     &                sparent(izon,isurf),' ',SMLCN(izon,isurf)(1:12),
     &                ' ',SMLCN(izon,iosurf)(1:12),' ',
     &                suse(izon,isurf,1),' ',suse(izon,iosurf,1),
     &                icuredge,idedge(icuredge,1),idedge(icuredge,2),
     &                idedge(icuredge,3)
                    call edisp(iuout,outs)
                  endif

                  if(SVFC(izon,isurf)(1:4).eq.'VERT'.and.(
     &               SVFC(izon,iosurf)(1:4).eq.'CEIL'.or.
     &               SVFC(izon,iosurf)(1:4).eq.'SLOP'))then

C ibridgeshr =1 is roof-wall (at eave), ibridgeshr =9 is wall-gable 
C ibridgeshr = 10 wall-parapet so both surfaces must face the outside, first must
C be roughly vertical and the other flat (up) or sloped.
                    if(MMOD.eq.8)then  ! highlight in red
                      call edge_redblack(xx,xy,iix,iiy,iix2,iiy2,'r')
                    endif
                    call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                    call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                    write(outs,'(3a)') zsn,' edge with ',zsno
                    CALL EASKATOG(outs,'  ','wall-eave','wall-gable',
     &                'wall-parapet or wall-flat roof','lintel',
     &                'balcony','not a bridge',' ',IV,nbhelp)
                    if(iv.eq.1)then
                      ibridgeshr(icc,i)=1
                      bridgelen(izon,1)= bridgelen(izon,1)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-eave'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.2)then
                      ibridgeshr(icc,i)=9
                      bridgelen(izon,9)= bridgelen(izon,9)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-gable'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.3)then
                      ibridgeshr(icc,i)=10
                      bridgelen(izon,10)= bridgelen(izon,10)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-parapett'
                      endif
                    elseif(iv.eq.4)then  ! A lintel.
                      ibridgeshr(icc,i)=6   ! lintel
                      bridgelen(izon,6)= bridgelen(izon,6)+
     &                  edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to lintel'
                      endif
                    elseif(iv.eq.5)then  ! A balcony
                      idedge(icuredge,3)=14
                      bridgelen(izon,14)= bridgelen(izon,14)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to balcony'
                      endif
                    elseif(iv.eq.6)then  ! User says not a bridge.
                      if(MMOD.eq.8)then  ! redraw in black
                      call edge_redblack(xx,xy,iix,iiy,iix2,iiy2,'b')
                      endif
                      idedge(icuredge,3)=-1  ! Mark declined.
                      goto 45  ! skip to the next edge.
                    endif
                    if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                      call u2pixel(xx(1),xy(1),iix,iiy)
                      call u2pixel(xx(2),xy(2),iix2,iiy2)
                      iicol=ibridgeshr(icc,i)
                      if(greyok)call winscl('z',iicol)
                      call edwline(iix,iiy,iix2,iiy2)
                      iicol=0
                      if(greyok)call winscl('-',iicol)
                      call forceflush()
                    endif
                  elseif(SVFC(izon,isurf)(1:4).eq.'VERT'.and.
     &                   SVFC(izon,iosurf)(1:4).eq.'VERT')then

C Test if azimuth are within two degrees. For the special case of the
C difference between the two being ~360deg call it closeazi
                    call eclose(SPAZI(izon,isurf),
     &                          SPAZI(izon,iosurf),2.0,closeazi)
                    if(.NOT.closeazi)then
                      zazi = abs(SPAZI(izon,isurf)-
     &                           SPAZI(izon,iosurf))
                      call eclose(zazi,360.0,2.0,closeazi)
                    endif
                    if(closeazi)then

C Find which of the unit vector components (ux uy uz) are near zero
C in order to see if edge if vertical or horizontal.
                      call uvxyz(X(j),Y(j),Z(j),X(k),Y(k),Z(k),
     &                  ux,uy,uz)
                      call eclose(ux,0.00,0.002,closeux)
                      call eclose(uy,0.00,0.002,closeuy)
                      call eclose(uz,0.00,0.002,closeuz)

C ibridgeshr =6 is lintel above window or door so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly horizontal and above the
C centre of the surface (and marked as a door or window)
C << ALSO check if parent is of FRAME type >>
                      if(closeuz)then
                        if(Z(j).gt.SURCOG(izon,isurf,3))then
                          if(sparent(izon,isurf)(1:2).eq.'-')then
                            goto 45
                          elseif(SMLCN(izon,isurf)(1:12).eq.
     &                           SMLCN(izon,iosurf)(1:12))then
                            goto 45  ! same MLC so cannot be a TB
                          else

C Check if either surface is a REVEAL.
                            if(SUSE(izon,isurf,1)(1:6).eq.'REVEAL'.or.
     &                         SUSE(izon,iosurf,1)(1:6).eq.'REVEAL')then
                              if(MMOD.eq.8)then  ! highlight in red
                                call edge_redblack(xx,xy,iix,iiy,
     &                            iix2,iiy2,'r')
                              endif
                            call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                            call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                            write(outs,'(3a)') zsn,' edge with ',zsno
                              CALL EASKAB(outs,
     &                          'One surface is a reveal:',
     &                          'corner thermal bridge','not a bridge',
     &                          IV,nbhelp)
                              if(IV.eq.2)then
                               if(MMOD.eq.8)then  ! Redraw as black line.
                                 call edge_redblack(xx,xy,iix,iiy,
     &                             iix2,iiy2,'b')
                                endif
                                idedge(icuredge,3)=-1  ! Mark declined.
                                goto 45
                              endif
                            endif

C Check various combinations. 1st opaque & 2nd opaque then lintel
C if 1st opaque & 2nd tran or 1st tran & 2nd opaque then glass-fame.
                            if(SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                         SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                              ibridgeshr(icc,i)=6   ! lintel
                              bridgelen(izon,6)= bridgelen(izon,6)+
     &                          edgelen
                              if(itrc.ge.1)then
C                                write(6,*) 'add ',edgelen,' to lintel'
                              endif
                              idedge(icuredge,3)=1
                            elseif(
     &                        SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                              ibridgeshr(icc,i)=13  ! glass-frame
                              bridgelen(izon,13)= bridgelen(izon,13)+
     &                          edgelen
                              if(itrc.ge.1)then
C                                write(6,*) 'add ',edgelen,' glass frame'
                              endif
                              idedge(icuredge,3)=1
                            elseif(
     &                        SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                              ibridgeshr(icc,i)=13  ! glass-frame
                              bridgelen(izon,13)= bridgelen(izon,13)+
     &                          edgelen
                            elseif(
     &                        SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                              if(MMOD.eq.8)then  ! redraw in black
                                call edge_redblack(xx,xy,iix,iiy,
     &                            iix2,iiy2,'b')
                              endif
                              goto 45  ! skip to the next edge.
                            endif
                            if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                              call u2pixel(xx(1),xy(1),iix,iiy)
                              call u2pixel(xx(2),xy(2),iix2,iiy2)
                              iicol=ibridgeshr(icc,i)
                              if(greyok)call winscl('z',iicol)
                              call edwline(iix,iiy,iix2,iiy2)
                              iicol=0
                              if(greyok)call winscl('-',iicol)
                              call forceflush()
                            endif
                          endif

C ibridgeshr =7 is Sill below window so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly horizontal and below the
C centre of the surface (and marked as a door or window)
                        else
                          if(sparent(izon,isurf)(1:2).eq.'-')then
                            goto 45
                          elseif(SMLCN(izon,isurf)(1:12).eq.
     &                           SMLCN(izon,iosurf)(1:12))then
                            goto 45  ! same MLC so cannot be a TB
                          else

C Check various combinations. 1st opaque & 2nd opaque then sill.
C if 1st opaque & 2nd tran or 1st tran & 2nd opaque then glass-fame.
                            if(SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                         SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                              ibridgeshr(icc,i)=7   ! sill
                              bridgelen(izon,7)= bridgelen(izon,7)+
     &                          edgelen
                              if(itrc.ge.1)then
C                                write(6,*) 'add ',edgelen,' to sill'
                              endif
                              idedge(icuredge,3)=1
                            elseif(
     &                        SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                              ibridgeshr(icc,i)=13  ! glass-frame
                              bridgelen(izon,13)= bridgelen(izon,13)+
     &                          edgelen
                              if(itrc.ge.1)then
C                                write(6,*) 'add ',edgelen,' glass frame'
                              endif
                              idedge(icuredge,3)=1
                            elseif(
     &                        SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                              ibridgeshr(icc,i)=13  ! glass-frame
                              bridgelen(izon,13)= bridgelen(izon,13)+
     &                          edgelen
                              if(itrc.ge.1)then
C                                write(6,*) 'add ',edgelen,' glass frame'
                              endif
                              idedge(icuredge,3)=1
                            elseif(
     &                        SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                        SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                              continue
                            endif
                            if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                              call u2pixel(xx(1),xy(1),iix,iiy)
                              call u2pixel(xx(2),xy(2),iix2,iiy2)
                              iicol=ibridgeshr(icc,i)
                              if(greyok)call winscl('z',iicol)
                              call edwline(iix,iiy,iix2,iiy2)
                              iicol=0
                              if(greyok)call winscl('-',iicol)
                              call forceflush()
                            endif
                          endif
                        endif
                      endif

C ibridgeshr =8 is jamb at window or door so both surfaces must face
C the outside and face the same direction and one must be the
C parent and the edge must be roughly vertical (and marked as
C a door or window)
C << ALSO check if parent is of FRAME type >>
                     if(closeux.and.closeuy)then
                       if(sparent(izon,isurf)(1:2).eq.'-')then
                          goto 45
                       elseif(SMLCN(izon,isurf)(1:12).eq.
     &                         SMLCN(izon,iosurf)(1:12))then
                          goto 45  ! same MLC so cannot be a TB
                       else
                         if(SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                       SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                            ibridgeshr(icc,i)=8  ! is vertical
                            bridgelen(izon,8)= bridgelen(izon,8)+
     &                        edgelen
                            if(itrc.ge.1)then
C                              write(6,*) 'add ',edgelen,' to jamb'
                            endif
                            idedge(icuredge,3)=1
                          elseif(SOTF(izon,isurf)(1:6).eq.'OPAQUE'.and.
     &                      SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                            ibridgeshr(icc,i)=13  ! glass-frame
                            bridgelen(izon,13)= bridgelen(izon,13)+
     &                        edgelen
                            if(itrc.ge.1)then
C                              write(6,*) 'add ',edgelen,' to gass frame'
                            endif
                            idedge(icuredge,3)=1
                          elseif(SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                           SOTF(izon,iosurf)(1:6).eq.'OPAQUE')then
                            ibridgeshr(icc,i)=13  ! glass-frame
                            bridgelen(izon,13)= bridgelen(izon,13)+
     &                         edgelen
                            if(itrc.ge.1)then
C                              write(6,*) 'add ',edgelen,' to gass frame'
                            endif
                            idedge(icuredge,3)=1
                          elseif(SOTF(izon,isurf)(1:6).ne.'OPAQUE'.and.
     &                           SOTF(izon,iosurf)(1:6).ne.'OPAQUE')then
                            continue
                          endif
                          if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                            call u2pixel(xx(1),xy(1),iix,iiy)
                            call u2pixel(xx(2),xy(2),iix2,iiy2)
                            iicol=ibridgeshr(icc,i)
                            if(greyok)call winscl('z',iicol)
                            call edwline(iix,iiy,iix2,iiy2)
                            iicol=0
                            if(greyok)call winscl('-',iicol)
                            call forceflush()
                          endif
                        endif
                      endif
                    else

C Determin concave or convex between the two surfaces based on the distance
C between points on the two surfaces and points a slight distance
C away from the two surraces.
                      PA(1)=SURCOG(izon,isurf,1)  ! point on plane
                      PA(2)=SURCOG(izon,isurf,2)
                      PA(3)=SURCOG(izon,isurf,3)
                      VA(1)=SURVN(izon,isurf,1)   ! point 1m away
                      VA(2)=SURVN(izon,isurf,2)
                      VA(3)=SURVN(izon,isurf,3)

C Use ratio calculation to get point 100mm away.
                      r2 = 1.0 - 0.1
                      r1 = 0.1
                      vaa(1) = ((r2 * PA(1)) + (r1 * VA(1)))/1.0
                      vaa(2) = ((r2 * PA(2)) + (r1 * VA(2)))/1.0
                      vaa(3) = ((r2 * PA(3)) + (r1 * VA(3)))/1.0

                      PB(1)=SURCOG(izon,iosurf,1)  ! point on plane
                      PB(2)=SURCOG(izon,iosurf,2)
                      PB(3)=SURCOG(izon,iosurf,3)
                      VB(1)=SURVN(izon,iosurf,1)   ! point 1m away
                      VB(2)=SURVN(izon,iosurf,2)
                      VB(3)=SURVN(izon,iosurf,3)
                      vbb(1) = ((r2 * PB(1)) + (r1 * VB(1)))/1.0
                      vbb(2) = ((r2 * PB(2)) + (r1 * VB(2)))/1.0
                      vbb(3) = ((r2 * PB(3)) + (r1 * VB(3)))/1.0
                      dista = crow(PA,PB)  ! dist cog-to-cog
C                      distb = crow(VA,VB)  ! dist offset-to-offset
                      distc = crow(VAA,VBB)! dist slight offset-to-offset

C Check if REVEAL << and FRAME >>
                      IV=1     ! Assume corner.
                      if(SUSE(izon,isurf,1)(1:6).eq.'REVEAL'.or.
     &                   SUSE(izon,iosurf,1)(1:6).eq.'REVEAL')then
                        if(MMOD.eq.8)then  ! highlight in red
                          call edge_redblack(xx,xy,iix,iiy,
     &                      iix2,iiy2,'r')
                        endif
                        call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                        call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                        write(outs,'(3a)') zsn,' edge with ',zsno
                        CALL EASKABC(outs,'One surface is a reveal',
     &                    'corner thermal bridge','jamb',
     &                    'not a bridge',IV,nbhelp)
                        if(IV.eq.3)then
                          if(MMOD.eq.8)then  ! Redraw as black line.
                            call edge_redblack(xx,xy,iix,iiy,
     &                        iix2,iiy2,'b')
                          endif
                          idedge(icuredge,3)=-1  ! Mark declined.
                          goto 45
                        endif
                      endif

C If dista is greater than distc then concave.
                      if(IV.eq.1)then
                        if(dista.lt.distc)then

C ibridgeshr =3 is wall-wall (convex corner) so both must face the outside
C and the angle between the surface normals is greater than zero.
                          ibridgeshr(icc,i)=3
                          bridgelen(izon,3)= bridgelen(izon,3)+
     &                      edgelen
                          if(itrc.ge.1)then
C                            write(6,*) 'add ',edgelen,' to corner conv'
                          endif
                          idedge(icuredge,3)=1
                        else

C ibridgeshr =4 is wall-wall (concave corner) so both must face the outside
C and the angle between the surface normals less than zero
                          ibridgeshr(icc,i)=4
                          bridgelen(izon,4)= bridgelen(izon,4)+
     &                      edgelen
                          if(itrc.ge.1)then
C                            write(6,*) 'add ',edgelen,' to corner concv'
                          endif
                          idedge(icuredge,3)=1
                        endif
                      elseif(IV.eq.2)then
                        ibridgeshr(icc,i)=8  ! is vertical
                        bridgelen(izon,8)= bridgelen(izon,8)+
     &                    edgelen
                        if(itrc.ge.1)then
C                          write(6,*) 'add ',edgelen,' to jamb'
                        endif
                        idedge(icuredge,3)=1
                      endif
                      if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                        call u2pixel(xx(1),xy(1),iix,iiy)
                        call u2pixel(xx(2),xy(2),iix2,iiy2)
                        iicol=ibridgeshr(icc,i)
                        if(greyok)call winscl('z',iicol)
                        call edwline(iix,iiy,iix2,iiy2)
                        iicol=0
                        if(greyok)call winscl('-',iicol)
                        call forceflush()
                      endif
                    endif

                  elseif(SVFC(izon,isurf)(1:4).eq.'VERT'.and.
     &                   SVFC(izon,iosurf)(1:4).eq.'FLOR')then

C Check if either surface is a REVEAL.
                    IV=1       ! assume it will be wall-floor exposed.
                    if(SUSE(izon,isurf,1)(1:6).eq.'REVEAL'.or.
     &                 SUSE(izon,iosurf,1)(1:6).eq.'REVEAL')then
                      if(MMOD.eq.8)then  ! highlight in red
                        call edge_redblack(xx,xy,iix,iiy,iix2,iiy2,'r')
                      endif
                      call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                      call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                      write(outs,'(3a)') zsn,' edge with ',zsno
                      CALL EASKABC(outs,'One surface is a reveal',
     &                  'wall-floor exposed','sill','not a bridge',
     &                  IV,nbhelp)
                      if(IV.eq.3)then
                        if(MMOD.eq.8)then  ! Redraw as black line.
                          call edge_redblack(xx,xy,iix,iiy,
     &                      iix2,iiy2,'b')
                        endif
                        idedge(icuredge,3)=-1  ! Mark declined.
                        goto 45
                      endif
                    endif

C ibridgeshr =5 is wall-floor (exposed) so both surfaces must face the
C outside one roughly vertical and the other flat (down).
                    if(IV.eq.1)then  ! Wall-floor exposed.
                      call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                      call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
C Debug.
C                      write(6,'(14a,4i4)') 'wall-floor-exposed ',
C     &                  zsn(1:lnblnk(zsn)),' edge with ',
C     &                  zsno(1:lnblnk(zsno)),' p ',
C     &                  sparent(izon,isurf),' ',
C     &                  SMLCN(izon,isurf)(1:12),
C     &                  ' ',SMLCN(izon,iosurf)(1:12),' ',
C     &                  suse(izon,isurf,1),' ',suse(izon,iosurf,1),
C     &                  icuredge,idedge(icuredge,1),idedge(icuredge,2),
C     &                  idedge(icuredge,3)
                        ibridgeshr(icc,i)=5
                        bridgelen(izon,5)= bridgelen(izon,5)+
     &                    edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to exposed floor'
                      endif
                      idedge(icuredge,3)=1
                    elseif(IV.eq.2)then
                      ibridgeshr(icc,i)=7   ! sill
                      bridgelen(izon,7)= bridgelen(izon,7)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to sill'
                      endif
                      idedge(icuredge,3)=1
                    endif
                    if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                      call u2pixel(xx(1),xy(1),iix,iiy)
                      call u2pixel(xx(2),xy(2),iix2,iiy2)
                      iicol=ibridgeshr(icc,i)
                      if(greyok)call winscl('z',iicol)
                      call edwline(iix,iiy,iix2,iiy2)
                      iicol=0
                      if(greyok)call winscl('-',iicol)
                      call forceflush()
                    endif
                  endif

                elseif(ICT(icc).eq.0.and.ICT(icco).eq.3.or.
     &                 ICT(icc).eq.0.and.ICT(icco).eq.1)then

C Edge between external and an internal surface or a surface to
C a similar condition. 
C ibridgeshr =11 if 2nd surface is flat
                  call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                  call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
C Debug.
C                  write(6,'(14a,4i4)') 'wall ptn ',zsn(1:lnblnk(zsn)),
C     &              ' edge with ',zsno(1:lnblnk(zsno)),' p ',
C     &              sparent(izon,isurf),' ',
C     &              SMLCN(izon,isurf)(1:12),
C     &              ' ',SMLCN(izon,iosurf)(1:12),' ',
C     &              suse(izon,isurf,1),' ',suse(izon,iosurf,1),
C     &              icuredge,idedge(icuredge,1),idedge(icuredge,2),
C     &              idedge(icuredge,3)
                  if(SVFC(izon,isurf)(1:4).eq.'VERT'.and.
     &               SVFC(izon,iosurf)(1:4).eq.'FLOR')then
                    ibridgeshr(icc,i)=11
                    bridgelen(izon,11)= bridgelen(izon,11)+
     &                edgelen
                    if(itrc.ge.1)then
C                      write(6,*) 'add ',edgelen,' to intermed floor'
                    endif
                    idedge(icuredge,3)=1
                  elseif(SVFC(izon,isurf)(1:4).eq.'VERT'.and.
     &                   SVFC(izon,iosurf)(1:4).eq.'CEIL')then
                    if(MMOD.eq.8)then  ! highlight in red
                      call edge_redblack(xx,xy,iix,iiy,iix2,iiy2,'r')
                    endif
                    call ZSID(izon,isurf,zsn,ZSDESC,ZSDESS)
                    call ZSID(izon,iosurf,zsno,ZSDESCO,ZSDESSO)
                    write(outs,'(3a)') zsn,' edge with ',zsno
                    CALL EASKATOG(outs,'  ','wall-eave','wall-gable',
     &                'wall-parapet or wall-flat roof',
     &                'intermediate floor','balcony','lintel',
     &                'not a bridge',IV,nbhelp)
                    if(iv.eq.1)then
                      ibridgeshr(icc,i)=1
                      bridgelen(izon,1)= bridgelen(izon,1)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-eave'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.2)then
                      ibridgeshr(icc,i)=9
                      bridgelen(izon,9)= bridgelen(izon,9)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-gable'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.3)then
                      ibridgeshr(icc,i)=10
                      bridgelen(izon,10)= bridgelen(izon,10)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to wall-parapet'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.4)then
                      ibridgeshr(icc,i)=11
                      bridgelen(izon,11)= bridgelen(izon,11)+
     &                  edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to intermed floor'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.5)then  ! A balcony
                      ibridgeshr(icc,i)=14
                      bridgelen(izon,14)= bridgelen(izon,14)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to balcony'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.6)then  ! A lintel
                      ibridgeshr(icc,i)=6   ! lintel
                      bridgelen(izon,6)= bridgelen(izon,6)+edgelen
                      if(itrc.ge.1)then
C                        write(6,*) 'add ',edgelen,' to lintel'
                      endif
                      idedge(icuredge,3)=1
                    elseif(iv.eq.7)then  ! User says not a bridge.
                      if(MMOD.eq.8)then  ! Redraw as black line.
                        call edge_redblack(xx,xy,iix,iiy,
     &                    iix2,iiy2,'b')
                      endif
                      idedge(icuredge,3)=-1  ! Mark declined.
                    endif
                  elseif(SVFC(izon,isurf)(1:4).eq.'VERT'.and.
     &                   SVFC(izon,iosurf)(1:4).eq.'VERT')then

C ibridgeshr =12 if 2nd (interior) surface is vertical.
                    ibridgeshr(icc,i)=12
                    bridgelen(izon,12)= bridgelen(izon,12)+
     &                edgelen
                    if(itrc.ge.1)then
C                      write(6,*) 'add ',edgelen,' to wall-ptn'
                    endif
                    idedge(icuredge,3)=1
                  elseif(SVFC(izon,isurf)(1:4).eq.'SLOP'.and.  ! Sloped roof to flat ceiling.
     &                   SVFC(izon,iosurf)(1:4).eq.'FLOR')then
                    ibridgeshr(icc,i)=9
                    bridgelen(izon,9)= bridgelen(izon,9)+edgelen
                    if(itrc.ge.1)then
C                      write(6,*) 'add ',edgelen,' to wall-gable'
                    endif
                    idedge(icuredge,3)=1
                  endif
                  if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                    call u2pixel(xx(1),xy(1),iix,iiy)
                    call u2pixel(xx(2),xy(2),iix2,iiy2)
                    iicol=ibridgeshr(icc,i)
                    if(greyok)call winscl('z',iicol)
                    call edwline(iix,iiy,iix2,iiy2)
                    iicol=0
                    if(greyok)call winscl('-',iicol)
                    call forceflush()
                  endif
 
                elseif(ICT(icc).eq.4.and.ICT(icco).eq.0)then

C ibridgeshr =2 is wall-ground floor so one surface must face outside and the
C other must face the ground.
                  if(SVFC(izon,isurf)(1:4).eq.'FLOR'.and.
     &                SVFC(izon,iosurf)(1:4).eq.'VERT')then
                    ibridgeshr(icc,i)=2
                    bridgelen(izon,2)= bridgelen(izon,2)+
     &                edgelen
                    if(itrc.ge.1)then
C                      write(6,*) 'add ',edgelen,' to wall-grnd'
                    endif
                    idedge(icuredge,3)=1
                    if(ibridgeshr(icc,i).ne.0.and.MMOD.eq.8)then
                      call u2pixel(xx(1),xy(1),iix,iiy)
                      call u2pixel(xx(2),xy(2),iix2,iiy2)
                      iicol=ibridgeshr(icc,i)
                      if(greyok)call winscl('z',iicol)
                      call edwline(iix,iiy,iix2,iiy2)
                      iicol=0
                      if(greyok)call winscl('-',iicol)
                      call forceflush()
                    endif
                  endif
                endif
              endif
  45        continue
  44      continue     ! loop surfaces in zone
  42    continue  ! loop vertices in surface
  43  continue    ! outer loop of surfaces in the zone

C Report on likely thermal bridge lengths. Reduce the concave and
C convex bridge lengths because they will have been counted from
C the edges going in both directions.
C       bridgelen(izon,3)=bridgelen(izon,3)*0.5
C       bridgelen(izon,4)=bridgelen(izon,4)*0.5
      if(itrc.ge.1)then
        write(outs,'(a,9f7.2)') 'bridge len ',bridgelen(izon,1),
     &    bridgelen(izon,2),bridgelen(izon,3),
     &    bridgelen(izon,4),bridgelen(izon,5),
     &    bridgelen(izon,6),bridgelen(izon,7),
     &    bridgelen(izon,8),bridgelen(izon,9)
        call edisp(iuout,' ')
        call edisp(iuout,outs)
        write(outs,'(a,7f7.2)') 'bridge len ',bridgelen(izon,10),
     &    bridgelen(izon,11),bridgelen(izon,12),
     &    bridgelen(izon,13),bridgelen(izon,14),
     &    bridgelen(izon,15),bridgelen(izon,16)
        call edisp(iuout,outs)
      endif
      return
      end

C ****************** edge_redblack *******************
C Draw a polygon edge in either red or black.

      subroutine edge_redblack(xx,xy,iix,iiy,iix2,iiy2,act)
C #include "building.h"

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      dimension xx(2),xy(2)
      character*1 act
      logical greyok

#ifdef OSI
      integer iix,iiy,iix2,iiy2,iicol
#else
      integer*8 iix,iiy,iix2,iiy2,iicol
#endif

C Check if we have colour.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

      if(act.eq.'b')then
        call u2pixel(xx(1),xy(1),iix,iiy)
        call u2pixel(xx(2),xy(2),iix2,iiy2)
        iicol=0
        if(greyok)call winscl('-',iicol)
        call edwline(iix,iiy,iix2,iiy2)
        call forceflush()
      else
        call u2pixel(xx(1),xy(1),iix,iiy)
        call u2pixel(xx(2),xy(2),iix2,iiy2)
        iicol=0
        if(greyok)call winscl('z',iicol)
        call edwline(iix,iiy,iix2,iiy2)
        iicol=0
        if(greyok)call winscl('-',iicol)
        call forceflush()
      endif
      return
      end
