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

C edgeo.f contains code to allow the editing of geometry files:
C  EDZONE:  Main zone form editing control.
C  EDINSUL: Edit zone insolation distribution and shading directives.
C PICKSSUR  Selects surfaces for shading & insolation or copying.
C  EDSURA:  Edit zone surface attributes in common block G5
C  Edituse: Sets the value of SSUSE() and SUSE based on context.
C  EDVERT:  Edit vertex attributes in common block G1.
C  EDVLIST: Edit surface-vertex list attributes in common block G1.
C  EPKVERT: Select one or more vertices from information in common G1.
C  Surfprm: returns the number of edges and total length of
C           the perimiter of the polygon.
C  EZIPIN:  Reads zip format geometry data into esp-r geometry common
C           blocks.
C LINTHBRDG: defines linear thermal bridges for zone psi and lengths.

C ************* EDZONE
C Control editing of zone attributes and allow this to be saved to a
C geometry file. ianother is returned as +1 if jump to next zone,
C -1 if jump to prior zone, otherwise zero.
      SUBROUTINE EDZONE(ITRC,ICOMP,ianother,IER)
#include "net_flow.h"
#include "building.h"
#include "model.h"
#include "site.h"
#include "prj3dv.h"

C geometry.h provides commons G0/G1/G2/G4/G5/G7/prec2/prec17/precz/c20/GS5.
#include "geometry.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

C Passed parameters
      integer itrc   ! reporting level
      integer icomp  ! zone index
      integer ianother  ! jump indicator
      integer ier    ! returned error state zero is ok

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/SFIG/NSIGFIG
      
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
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      common/user/browse

      integer ncub,ivfok
      real XOC,YOC,ZOC,DXC,DYC,DZC,CANG
      character CUBN*6
      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

      LOGICAL MODIFY,MODLEN,MODBND,browse,context
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL DOK,OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      DIMENSION IVALS(MS),ITEMS(28),ITEMSS(MS),IVALSS(MS),jvn1(MV)
      DIMENSION SALT(10),trnalt(17),IVLST(MTV)
      CHARACTER HEAD*32,SALT*33,hold*36
      CHARACTER DESCRC*25
      CHARACTER D14*14,T14*14,D12*12,T12*12,LTMP*72,GFILE*72
      CHARACTER ITEMS*33,ITEMSS*32,trnalt*33,outs*124
      character T64*64,D64*64
      CHARACTER LAPROB*72
      character SN*12,act*1
      character SIGSTR*12,fs*1,t16a*16,t16b*16,t16c*16,msgv*42
      character TOSMLCN*32  ! to remember other side mlc name
      character TOOPT*32    ! to remember other side optics
      character use1*8,use2*8 ! return strings from selectuse
      character guesstype*24  ! for multi selection of surface use

      logical MODGEO,bound,attribok,zbzero,close,bndry,nameok,XST
      logical showother,updoth,firstin,unixok
      logical newgeo  ! to use for testing if new/old geometry file.
      logical forceupgrade  ! if any other zone is version 1.1
      logical silent  ! to signal interactive dependency resolution

      real exposed,vexposed,areatran,areawall  ! local values for QA
      real areaslproof,areafltroof,areaskylt
      real uavgtran,uavgsky,uavwall,uavfltroof,uavslproof,wallper

      integer lna,lnb,lnbm,lnbn
      integer IRT  ! for radio button selections
      integer NITMS,INO,INOR ! max items and current menu item
      integer INPICK  ! to use with epkvert
      integer iissmlci  ! index for other side mlc
      integer icob    ! for passing to clickon facility
      integer itrcl   ! local trace on first entry to zone
      integer lnssmlc,lnmlcn,lnsym

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDZONE'  ! set for subroutine

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
      TOSMLCN=' '
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
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
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
          iaplic(icomp,1)=0   ! reset for no implied shading or insolation
          iaplic(icomp,2)=0
          nsurfcalc(icomp)=0
          nsurfinso(icomp)=0
        elseif(ISI(ICOMP).gt.0.and.IOBS(ICOMP).eq.0)then
          iaplic(icomp,1)=0   ! reset for insolation only
          iaplic(icomp,2)=1
          nsurfcalc(icomp)=0
          nsurfinso(icomp)=0
          DO 778 I=1,NZSUR(icomp)
            ioc=IZSTOCN(icomp,i)
            if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &            SSOTF(ioc)(1:4).ne.'OPAQ')then
              nsurfinso(icomp)=nsurfinso(icomp)+1
              isurfinso(icomp,nsurfinso(icomp))=I
            endif
  778     continue
        elseif(ISI(ICOMP).gt.0.and.IOBS(ICOMP).gt.0)then
          iaplic(icomp,1)=1   ! reset for insolation only
          iaplic(icomp,2)=1
          nsurfcalc(icomp)=0
          nsurfinso(icomp)=0
          DO 779 I=1,NZSUR(icomp)
            ioc=IZSTOCN(icomp,i)
            if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &         SSOTF(ioc)(1:4).ne.'OPAQ')then
              nsurfinso(icomp)=nsurfinso(icomp)+1
              isurfinso(icomp,nsurfinso(icomp))=I
            endif
            if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
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

      OPQ=0.
      TRN=0.
      CFC=0.

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
        FLA=ZBASEA(ICOMP)
        ZBASEA(ICOMP)=0.0
      endif
      attribok=.true.
      
C Check if SVFC and SSVFC needs to be updated.
      call updatesvfc(icomp,modgeo)

C Loop through each surface and detect base information. 
      DO 41 I=1,NSUR
        ioc=IZSTOCN(icomp,i)

C Debug
C        write(6,*) 'ssmlcindex ',ioc,ssmlcindex(ioc)
        if(SOTHER(I,1)(1:4).eq.'UNKN')attribok=.false.
        if(SMLCN(I)(1:4).eq.'UNKN')attribok=.false.
        if(SVFC(I).eq.'FLOR')then

C If the user has edited the floor area (one) then continue or selected it
C via a sub-set of applicable surfaces (two), otherwise
C get area by adding up ibases list, otherwise search for `flor` surfaces.
          if(IUZBASEA(icomp).eq.1)then
            continue
          elseif(IUZBASEA(icomp).eq.2)then
            continue
          else

C Check each of the surfaces in the ibases list. If surface `flor` not
C included in the list add it.
            if(i.eq.ibases(icomp,1).or.i.eq.ibases(icomp,2).or.
     &         i.eq.ibases(icomp,3).or.i.eq.ibases(icomp,4).or.
     &         i.eq.ibases(icomp,5).or.I.eq.ibases(icomp,6).or.
     &         i.eq.ibases(icomp,7).or.I.eq.ibases(icomp,8).or.
     &         i.eq.ibases(icomp,9).or.I.eq.ibases(icomp,10))then
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
            else
              lastlist=lastlist+1
              if(lastlist.le.10)then
                IBASES(ICOMP,lastlist)=I
                ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
              endif
            endif
          endif
        else

C Check to see if a non `FLOR` surface has been included in ibases.
          if(IUZBASEA(icomp).eq.1)then
            continue
          elseif(IUZBASEA(icomp).eq.2)then
            continue
          else
            if(i.eq.ibases(icomp,1).or.i.eq.ibases(icomp,2).or.
     &         i.eq.ibases(icomp,3).or.i.eq.ibases(icomp,4).or.
     &         i.eq.ibases(icomp,5).or.I.eq.ibases(icomp,6).or.
     &         i.eq.ibases(icomp,7).or.I.eq.ibases(icomp,8).or.
     &         i.eq.ibases(icomp,9).or.I.eq.ibases(icomp,10))then
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
            endif
          endif
        endif
        if(SOTF(I)(1:4).NE.'OPAQ'.AND.SOTF(I)(1:3).NE.'CFC')then
          TRN=TRN+SSNA(ioc)
        elseif(SOTF(I)(1:3).EQ.'CFC')then
          CFC=CFC+SSNA(ioc)
        else
          OPQ=OPQ+SSNA(ioc)
        endif
   41 CONTINUE

C If previously calculated base area, compare with current area calculation
C and inform user if it has changed.
      if(IUZBASEA(icomp).eq.0)then
        CALL ECLOSE(ZBASEA(ICOMP),FLA,0.001,close)
        if(.NOT.close)then

C Debug.
          call SIGFIG(ZBASEA(ICOMP),NSIGFIG,RNO,SIGSTR,LSTR)
          write(outs,'(3a)') 
     &        'Base/floor area is ',SIGSTR(1:LSTR),'m^2.'
          call edisp(iuout,outs)
        endif
      endif
      call ckvert(0,icomp,bound,iub,inv,ier)  ! needs G1 common

C Loop through constructions and see if partitions have a matching
C construction on the other side. Logic is the same as in EDSURA.
      if(.NOT.browse)then
        DO 40 is=1,NSUR
          ioc=IZSTOCN(icomp,is)
          if(ICT(ioc).eq.3)then
            showother=.true.
            icoth=IZSTOCN(IC2(ioc),IE2(ioc))
          else
            showother=.false.
            icoth=0  ! not a partition
          endif

C Report on any duplicate surface names.
          write(SN,'(a12)')SSNAME(ioc)
          call snamdup(SN,icomp,is,nameok)
          if(nameok)then
            continue
          else
            write(outs,'(3a)') 'Surface ',SN(1:lnblnk(SN)),
     &        ' has a duplicate name. Names must be unique!'
            call edisp(iuout,outs)
          endif

C Report on possible construction issues between zones.
C Use ssmlcindex to get MLC index for this surface.
          if(showother.and.icoth.ne.0.and.ssmlcindex(ioc).ne.0)then
            ii=ssmlcindex(ioc)
            updoth=.false.
            lnmlcn=lnblnk(mlcname(ii))
            lnssmlc=lnblnk(SSMLCN(icoth))
            if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
              if(SSMLCN(icoth)(1:lnssmlc).eq.mlcname(ii)(1:lnmlcn))then
                TOSMLCN=mlcname(ii)
                write(TOOPT,'(a)') mlcoptical(ii)
              elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
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
              write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &          ' has a nonsymmetric construction ',
     &          SSMLCN(ioc)(1:lnblnk(SSMLCN(ioc))),'.'
              call edisp(iuout,outs)
              write(outs,'(5a)') 'It faces ',
     &          SSNAME(icoth),' which is composed of ',
     &          SSMLCN(icoth)(1:12),' (which may not match).'
              call edisp(iuout,outs)
              updoth=.false.
              iissmlci=matsymindex(ii)   ! returned value
            else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
              lnsym=lnblnk(mlcsymetric(ii))
              if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                TOSMLCN=mlcsymetric(ii)
                write(TOOPT,'(a)') mlcoptical(ii)
                updoth=.true.
              elseif(SSMLCN(icoth)(1:lnssmlc).eq.
     &               mlcsymetric(ii)(1:lnsym))then
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
              lnicoth=lnblnk(SSMLCN(icoth))
              write(outs,'(6a)') 
     &          'Updating `other side` composition of ',
     &           SN(1:lnblnk(SN)),' from ',SSMLCN(icoth)(1:lnicoth),
     &          ' > ',TOSMLCN
              call edisp(iuout,outs)
              call georead(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,iuout,IER)

C Assign new MLC name to the icoth surface as well as SSOTF with the
C optical properties returned from material.h commons.
              SSMLCN(icoth)=TOSMLCN
              if(iissmlci.ne.0) ssmlcindex(icoth)=iissmlci ! update

              write(SSOTF(icoth),'(a)') TOOPT
              call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(IC2(ioc)) =1.1
                newgeo = .true.
              endif

C Logic to decide whether to upgrade other zone geometry format.
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(IC2(ioc)) =1.1
                newgeo = .true.
              endif
              if(newgeo)then
                call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &            iuout,3,IER)
              else
                call emkgeo(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &            3,IER)
              endif
              IF(IER.NE.0)CALL USRMSG(
     &          'There was a problem updating the other surface ',
     &          'attribute (i.e. the other geometry file)...','W')

C Read current zone back in and then update the configuration file.
              CALL ERCZONE(ICOMP)
              updoth=.false.
            endif
            call usrmsg(' ',' ','-')
          endif
 40     continue
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
        WRITE(ITEMS(ITEMC),'(3X,I3,A)')  iprb,' PROBLEM EDGES!'
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
      call SIGFIG(CFC,NSIGFIG,RNO,SIGSTR,LSTR)
      WRITE(ITEMS(ITEMC+4),'(3a)')'   cfc constr.: ',
     &         SIGSTR(1:LSTR),' m^2'
      ITEMS(ITEMC+5)='  _____________________________'
      WRITE(ITEMS(ITEMC+6),'(A,i3,a)')
     &                           'd vertex coordinates     (',NTV,')'
      WRITE(ITEMS(ITEMC+7),'(A,i3,a)')
     &                           'e surface list & edges   (',NSUR,')'
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
        ITEMS(ITEMC+13)='j define linear thermal bridges'
      else
        ITEMS(ITEMC+13)='j thermal bridges (NA)         '
      endif

C Interface to BASESIMP definitions for this zone.
      ITEMS(ITEMC+14)='k define/edit BASESIMP         '
      ITEMS(ITEMC+15)='  _____________________________'
      ITEMS(ITEMC+16)='* list zone & surface details  '
      ITEMS(ITEMC+17)='> save                         '
      IF(ITRC.EQ.0)THEN
        ITEMS(ITEMC+18)='t reporting >> silent  '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(ITEMC+18)='t reporting >> summary '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(ITEMC+18)='t reporting >> detailed'
      ENDIF
      if(icomp.gt.1.and.icomp.lt.NCOMP)then
        ITEMS(ITEMC+19)='< jump to previous zone      '
        ITEMS(ITEMC+20)='> jump to next zone          '
      elseif(icomp.eq.1)then
        ITEMS(ITEMC+19)='                             '
        ITEMS(ITEMC+20)='> jump to next zone          '        
      elseif(icomp.eq.NCOMP)then
        ITEMS(ITEMC+19)='< jump to previous zone      '
        ITEMS(ITEMC+20)='                             '
      endif
      ITEMS(ITEMC+21)='? help                         '
      ITEMS(ITEMC+22)='- exit this menu               '

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
      CALL ADJVIEW(IER)

C Test edge adjacencies and subsurfaces if it is the first time
C in to the zone or if the geometry has benn modified.
      if(.NOT.browse)then
        if(firstin.or.MODGEO)then
          act = 'c'
          call suredgeadj(0,act,icomp,ier) ! determine child surfaces
          firstin=.false.
        endif
      endif

C Help text for this menu.
      helptopic='geometry_main_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu for zone geometry.
      NITMS=ITEMC+22
      CALL EMENU(HEAD,ITEMS,NITMS,INO)

      IF(INO.EQ.ITEMC+22)THEN
        if(browse)return
        if(MODGEO)then
          DOK=.false.
          CALL ASKOK('Save changes to zone composition and surface',
     &         'attributes? (legacy and new format)',OK,DOK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LTMP,ICOMP,3,IER)
            endif
          endif

C If there is an associated flow node then update its volume and update
C the flow network.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            SUPNOD(ICAAS(ICOMP),2)=VOL(ICOMP)
            if(IPRODB.eq.IFIL+6)then
              IUM=IPRODB
            else
              IUM=IFIL+6
            endif
            CALL EFOPSEQ(IUM,LAPROB,3,IER)
            if(IER.eq.0)then

C << Note need a check so network flow file data is not written
C << to a graphic network flow file. Also need a way to update
C << graphic network flow file to reflect changes in zone volume.
              CALL MFWRIT(IUM)
              CALL ERPFREE(IUM,ISTAT)
            endif
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        ianother=0   ! signal no jump to previous or next.
        RETURN

      ELSEIF(INO.EQ.1)THEN

C Zone name, make sure it has no illegal characters.
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
          MODIFY=.TRUE.
          MODGEO=.TRUE.
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
        MODIFY=.TRUE.
        MODGEO=.TRUE.

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
          INPICK=10
        else
          INPICK=6
        endif
        ij=0
        DO 20 I=1,NSUR
          ITEMSS(I)=' '
          if(I.eq.IBASES(ICOMP,1).or.I.eq.IBASES(ICOMP,2).or.
     &       I.eq.IBASES(ICOMP,3).or.I.eq.IBASES(ICOMP,4).or.
     &       I.eq.IBASES(ICOMP,5).or.I.eq.IBASES(ICOMP,6).or.
     &       I.eq.IBASES(ICOMP,7).or.I.eq.IBASES(ICOMP,8).or.
     &       I.eq.IBASES(ICOMP,9).or.I.eq.IBASES(ICOMP,10))then
            WRITE(ITEMSS(I),'(A,1x,A,1x,A,A)')SNAME(ICOMP,I),
     &        SMLCN(I)(1:12),SVFC(I)(1:4),'*'
          else
            WRITE(ITEMSS(I),'(A,1x,A,1x,A)')SNAME(ICOMP,I),
     &        SMLCN(I)(1:12),SVFC(I)
          endif
   20   CONTINUE

C Present list of available surfaces with * adjacent to those which
C are already considered to be floor surfaces. Clear the list ibases
C and then either do manual editing or update ibases with surfaces
C the user selected.
        if(IUZBASEA(icomp).eq.0)then
          write(outs,'(a,f9.3,a)') 
     &      'Surfaces automatically associated with base (currently ',
     &      ZBASEA(ICOMP),'m^2)'
        elseif(IUZBASEA(icomp).eq.1)then
          write(outs,'(a,f9.3,a)') 
     &      'Surfaces associated with base (currently user edited ',
     &      ZBASEA(ICOMP),'m^2)'
        elseif(IUZBASEA(icomp).eq.2)then
          write(outs,'(a,f9.3,a)') 
     &      'Surfaces associated with base (current user list ',
     &      ZBASEA(ICOMP),'m^2)'
        endif
        CALL EPICKS(INPICK,IVALS,outs,
     &    'Select none for manual edit.',32,NSUR,ITEMSS,
     &    'Surface Name/Construction/Orient.',IER,nbhelp)
        do 221 i=1,10
          IBASES(ICOMP,i)=0
 221    continue
        if(inpick.eq.0)then
          CALL EASKR(ZBASEA(ICOMP),' ',
     &       'Confirm area of zone base.',
     &       0.1,'F',99999.,'F',FLA,'base area',IER,nbhelp)
          IUZBASEA(icomp)=1
          IZBASELIST(icomp)=0
          MODGEO=.TRUE.
          goto 3 
        else
          ZBASEA(ICOMP)=0.0
          IZBASELIST(icomp)=inpick
          do 211 i=1,inpick
            iss = ivals(i)
            IBASES(ICOMP,i)=iss
            ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(izstocn(icomp,iss))
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
          DOK=.true.
          CALL ASKOK(' ','Save vertex changes?',OK,DOK,nbhelp)
          LTMP=LGEOM(ICOMP)
          if(OK)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LTMP,ICOMP,3,IER)
            endif
            if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
              SUPNOD(ICAAS(ICOMP),2)=VOL(ICOMP)
              if(IPRODB.eq.IFIL+6)then
                IUM=IPRODB
              else
                IUM=IFIL+6
              endif
              CALL EFOPSEQ(IUM,LAPROB,3,IER)
              if(IER.eq.0)then

C << Note need a check so network flow file data is not written
C << to a graphic network flow file. Also need a way to update
C << graphic network flow file to reflect changes in zone volume.
                CALL MFWRIT(IUM)
                CALL ERPFREE(IUM,ISTAT)
              endif
            endif
            MODGEO=.false.
          endif
        endif
        MODIFY=.TRUE.   ! set so zone is re-drawn

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
        CALL EASKSUR(ICOMP,IS,'M','Select surface(s) to attribute.',
     &    ' ',IER)
        if(IS.lt.0)then

C Remind user of current surface boundary attributes.
          call tstamp('>','PRJ: enter global surface attribution')
          context=.true.
          if(ITRC.gt.1)CALL SURINFO(ICOMP,iuout,context)
          IRT=1
          CALL EASKATOG('Surface attribution: ',' ',
     &    'name','composition','boundary condition','usage',
     &    'cancel',' ',' ',IRT,nbhelp)

          if(IRT.eq.2)then
            CALL EPKMLC(ISEL,'Select one of the constructions',
     &        'or exit.',IER)

          elseif(IRT.eq.3)then
            SALT(1)='exterior                   '
            SALT(2)='similar to current         '
            SALT(3)='prescribed static          '
            SALT(4)='surface in other zone      '
            SALT(5)='ground (monthly profile)   '
            if(NGRDP.gt.0)then
              SALT(6)='ground (user defined profile)   '
            else
              SALT(6)='Ground (no user defined profile)'
            endif
            SALT(7)='adiabatic                   '
            SALT(8)='BASESIMP foundation calculation'
            SALT(9)='CEN 13791 partition         '
            SALT(10)='unknown at this time       '

            helptopic='geometry_boundary_menu'
            call gethelptext(helpinsub,helptopic,nbhelp)
            IX=1
            CALL EPICKS(IX,IVALSS,' ','Surface boundary condition:',
     &           33,10,SALT,'surface boundary options',IER,nbhelp)
            IOS=IVALSS(1)
            if(IOS.EQ.2)then
              VALT=0.0
              VALW=0.0
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

C Select surface use.
            guesstype='any_type'
            call selectuse(guesstype,use1,use2)

          elseif(IRT.eq.5)then
            goto 3
          endif

C Present list of surfaces and allow user to select up to all of them.
          INPICK=NSUR
          DO 10 I=1,NSUR
            ioc10=IZSTOCN(icomp,i)
            ITEMSS(I)=' '
            if(IRT.eq.1)then
              WRITE(ITEMSS(I),'(A)')SSNAME(ioc10)
            elseif(IRT.eq.2)then
              WRITE(ITEMSS(I),'(A,2x,A)')SSNAME(ioc10),
     &          SSMLCN(ioc10)(1:12)
            elseif(IRT.eq.3)then
              WRITE(ITEMSS(I),'(A,2x,A)')SSNAME(ioc10),
     &          SSOTHER(ioc10,1)(1:15)
            elseif(IRT.eq.4)then
              WRITE(ITEMSS(I),'(7A)')SSNAME(ioc10),' ',
     &          SSOTHER(ioc10,1)(1:6),' ',SSUSE(ioc10,1)(1:5),' ',
     &          SSUSE(ioc10,2)(1:5)
            endif
   10     CONTINUE
          if(IRT.eq.1)then
            CALL EPICKS(INPICK,IVALS,' ',' ',15,NSUR,ITEMSS,
     &          'Surface name',IER,nbhelp)
          elseif(IRT.eq.2)then
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name   composition',IER,nbhelp)
          elseif(IRT.eq.3)then
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name    facing',IER,nbhelp)
          elseif(IRT.eq.4)then
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
            if(IRT.eq.1)then

C (re)name all selected surfaces. 
C Update image befort each edit. 
C Call INLNST(1) to reset all LINSTY to 1 (i.e. thin lines) then set 
C LINSTY of selected surface to 2 (i.e. thick lines) and redraw.
              CALL INLNST(1)
              CALL SURADJ(ICOMP,ISS,IE,TMP,IZC,ISC,IC,DESCRC)
              LINSTY(IC)=2
              nzg=1
              nznog(1)=ICOMP
              izgfoc=ICOMP
              MODIFY=.TRUE.
              MODGEO=.TRUE.
              CALL ADJVIEW(IER)

C Use a slightly wider editing box than the name. Take the string
C (T14) and ensure that there are no spaces or strange characters.
              T14='  '
              write(D14,'(2a)')SSNAME(ioc),'  '
              write (outs,'(7a)')'Surface name (default=',D14,
     &          ') composed of ',ssmlcn(ioc)(1:12),' facing ',
     &          SSOTHER(ioc,1)(1:12),' highlighted surface'
 52           CALL EASKS(T14,outs,'(<=12 chars, no spaces)',
     &                   14,D14,'surface name',IER,nbhelp)
              write(T12,'(a)') T14(1:12)
              call st2name(T12,D12)

              call snamdup(D12,icomp,ISS,nameok)
              if(nameok)then

C Update commons and update the geometry file.
                SNAME(ICOMP,ISS)=D12
                SSNAME(ioc)=D12
                call eclose(gversion(icomp),1.1,0.01,newgeo)
                if(igupgrade.eq.2.and.(.NOT.newgeo))then
                  gversion(icomp) =1.1
                  newgeo = .true.
                endif
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
                else
                  call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
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
                MODIFY=.TRUE.
                MODGEO=.TRUE.
                CALL ADJVIEW(IER)
              endif
            elseif(IRT.eq.2.and.ISEL.EQ.0)then

C User selected the UNKNOWN MLC, reset variables.
              WRITE(SMLCN(ISS),'(A)') 'UNKNOWN'
              WRITE(SSMLCN(ioc),'(A)') 'UNKNOWN'
              SOTF(ISS)='OPAQUE'
              SSOTF(ioc)=SOTF(iss)
              ssmlcindex(ioc)=0
            elseif(IRT.eq.2.and.ISEL.GT.0)then

C Associate mlc with each selected surface.
              WRITE(SMLCN(ISS),'(A)') mlcname(ISEL)
              WRITE(SSMLCN(ioc),'(A)') mlcname(ISEL)
              ssmlcindex(ioc)=ISEL       ! update array
              ssthick(ioc)=THKMLC(isel)  ! update the thermophysical thickness

C Find the optical name.
              IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
                SOTF(ISS)='OPAQUE'
              ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
                SOTF(ISS)='CFC '
              ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC2')then
                SOTF(ISS)='CFC2'
              ELSE
                WRITE(SOTF(ISS),'(A)') mlcoptical(ISEL)
              ENDIF
              SSOTF(ioc)=SOTF(iss)
              call warnmod(ICOMP,'sat')

C If this is a partition find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.

C This logic should also included in subroutine serchrpl. So the
C global search and replace acts the same way.
              write(SN,'(a12)')SSNAME(ioc)
              if(showother.and.icoth.ne.0.and.ssmlcindex(ioc).ne.0)then
                ii=ssmlcindex(ioc)
                updoth=.false.
                lnmlcn=lnblnk(mlcname(ii))
                lnssmlc=lnblnk(SSMLCN(icoth))
                if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                  if(SSMLCN(icoth)(1:lnssmlc).eq.
     &               mlcname(ii)(1:lnmlcn))then
                    TOSMLCN=mlcname(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                  elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
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
                  write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &              ' has a nonsymmetric construction ',
     &              SSMLCN(ioc)(1:lnblnk(SSMLCN(ioc))),'.'
                  call edisp(iuout,outs)
                  write(outs,'(5a)') 'It faces ',
     &              SSNAME(icoth),' which is composed of ',
     &              SSMLCN(icoth)(1:12),' (which may not match).'
                  call edisp(iuout,outs)
                  call edisp(iuout,
     &              'Please check that one construction has inverted')
                  call edisp(iuout,
     &              'layers or revise the construction database.')
                  updoth=.false.
                  iissmlci=matsymindex(ii)  ! use returned value 
                else
                  if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                    TOSMLCN=mlcsymetric(ii)
                    write(TOOPT,'(a)') mlcoptical(ii)
                    updoth=.true.
                  elseif(SSMLCN(icoth)(1:12).eq.
     &                   mlcsymetric(ii)(1:12))then
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

C Assign new MLC name to the icoth surface.
                  SSMLCN(icoth)=TOSMLCN
                  if(iissmlci.ne.0) ssmlcindex(icoth)=iissmlci ! update
                  ssthick(icoth)=THKMLC(isel)  ! update the thermophysical thickness
                  write(SSOTF(icoth),'(a)') TOOPT
                  call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                  if(igupgrade.eq.2.and.(.NOT.newgeo))then
                    gversion(IC2(ioc)) =1.1
                    newgeo = .true.
                  endif
                  if(newgeo)then
                    call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &                iuout,3,IER)
                  else
                    call emkgeo(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &                  3,IER)
                  endif
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
                SOTHER(ISS,1)='EXTERIOR'
                SOTHER(ISS,2)='0'
                SOTHER(ISS,3)='0'
                SSOTHER(IC,1)='EXTERIOR'
                SSOTHER(IC,2)='0'
                SSOTHER(IC,3)='0'
                ICT(IC)=0
                IC2(IC)=0
                IE2(IC)=0
              ELSEIF(IOS.EQ.2)THEN
                SOTHER(ISS,1)='SIMILAR'
                write(SOTHER(ISS,2),'(i2.2)')INT(VALT)
                write(SOTHER(ISS,3),'(i2.2)')INT(VALW)
                SSOTHER(IC,1)='SIMILAR'
                write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
                write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
                ICT(IC)=1
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.3)THEN
                SOTHER(ISS,1)='CONSTANT'
                write(SOTHER(ISS,2),'(i2.2)')INT(VALT)
                write(SOTHER(ISS,3),'(i2.2)')INT(VALW)
                SSOTHER(IC,1)='CONSTANT'
                write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
                write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
                ICT(IC)=2
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.5)THEN
                SOTHER(ISS,1)='GROUND'
                write(SOTHER(ISS,2),'(i2.2)')iie2
                SOTHER(ISS,3)='0'
                SSOTHER(IC,1)='GROUND'
                write(SSOTHER(IC,2),'(i2.2)')iie2
                SSOTHER(IC,3)='0'
                ICT(IC)=4
                IC2(IC)=iic2
                IE2(IC)=0
              ELSEIF(IOS.EQ.6)THEN
                SOTHER(ISS,1)='GROUND'
                SOTHER(ISS,2)='0'
                write(SOTHER(ISS,3),'(i2.2)')iie2
                SSOTHER(IC,1)='GROUND'
                SSOTHER(IC,2)='0'
                write(SSOTHER(IC,3),'(i2.2)')iie2
                ICT(IC)=4
                IC2(IC)=0
                IE2(IC)=iie2
              ELSEIF(IOS.EQ.7)THEN
                SOTHER(ISS,1)='ADIABATIC'
                SOTHER(ISS,2)='0'
                SOTHER(ISS,3)='0'
                SSOTHER(IC,1)='ADIABATIC'
                SSOTHER(IC,2)='0'
                SSOTHER(IC,3)='0'
                ICT(IC)=5
                IE2(IC)=0
                IC2(IC)=0
              ELSEIF(IOS.EQ.8)THEN
                SOTHER(ISS,1)='BASESIMP'
                write(SOTHER(ISS,2),'(i2.2)')INT(VALT)
                write(SOTHER(ISS,3),'(i2.2)')INT(VALW)
                SSOTHER(IC,1)='BASESIMP'
                write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
                write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
              ELSEIF(IOS.EQ.9)THEN
                SOTHER(ISS,1)='IDENT_CEN'
                write(SOTHER(ISS,2),'(i2.2)')INT(VALT)
                write(SOTHER(ISS,3),'(i2.2)')INT(VALW)
                SSOTHER(IC,1)='IDENT_CEN'
                write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
                write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
                ICT(IC)=7
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.10)THEN

C Reset to represent UNKNOWN
                SOTHER(ISS,1)='UNKNOWN'
                SOTHER(ISS,2)='0'
                SOTHER(ISS,3)='0'
                SSOTHER(IC,1)='UNKNOWN'
                SSOTHER(IC,2)='0'
                SSOTHER(IC,3)='0'
                ICT(IC)=-1
                IE2(IC)=0
                IC2(IC)=0
              ENDIF
              call warnmod(ICOMP,'sat')
              MODGEO=.TRUE.
            elseif(IRT.eq.4)then

C Update surface USE attributes for relevant zone surface and connection.
              SUSE(ISS,1)=USE1
              SSUSE(ioc,1)=USE1
              SUSE(ISS,2)=USE2
              SSUSE(ioc,2)=USE2

            endif
  11      continue

C Finally update the system configuration file if not browsing.
          if(cfgok.and.(.NOT.browse))then
            CALL EMKCFG('-',IER)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
            endif
            silent= .false.
            call sumrchg(ICOMP,'r',silent)
          endif
          goto 42
        elseif(IS.gt.0)then

C Attribute a single surface, set flag to redraw.
          ianother=0
  443     call tstamp('>','PRJ: enter single surface attribution')
          CALL EDSURA(ICOMP,IS,ITRC,MODGEO,ianother,IER)
          MODIFY=.TRUE.

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

C Zone obstructions. If any unsaved zone geometry changes and not
C browsing update the geometry file before entering the obstruction
C editing facility.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(MODGEO)then
          if(cfgok.and.(.NOT.browse))then
            CALL EMKCFG('-',IER)
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
            endif
            silent = .false.
            call sumrchg(ICOMP,'r',silent)
          endif
          MODGEO=.false.
        endif
        call tstamp('>','PRJ: enter zone obstructions')
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
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
        write(trnalt(6),'(a)') ' ...... Translation ...... '
        write(trnalt(7),'(a)') 'e to specified X Y Z       '
        write(trnalt(8),'(a)') 'f to vertex in this zone   '
        write(trnalt(9),'(a)') 'g to vertex in another zone'
        write(trnalt(10),'(a)')' ...... Mirror ......      '
        write(trnalt(11),'(a)')'h Y axis positive direction'
        write(trnalt(12),'(a)')'i Y axis negative direction'
        write(trnalt(13),'(a)')'j X axis positive direction'
        write(trnalt(14),'(a)')'k X axis negative direction'
        write(trnalt(15),'(a)')' __________________________'
        write(trnalt(16),'(a)')'? help                     '
        write(trnalt(17),'(a)')'- exit this menu           '

C Help for rotation/transform/mirror options
        helptopic='geometry_rotation_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)

        WRITE(HEAD,'(2a)')'Transforms for Zone ',zname(ICOMP)

C Menu for zone rotations and transforms.
        NITMS=17
        CALL EMENU(HEAD,trnalt,NITMS,INOR)
        if(INOR.EQ.17)then
          goto 3
        elseif(INOR.EQ.16)then

C Produce help text for the menu.
          CALL PHELPD('rotation section',nbhelp,'-',0,0,IER)
          goto 77
        elseif(INOR.EQ.2.or.INOR.eq.3.or.INOR.eq.4.or.INOR.eq.5)then

C Remember what the user requested in prior rotateit structure and
C clear the pending rotateit structure.
          VAL=0.
          CALL EASKR(VAL,' ','Rotation (+ = anticlockwise)?',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(VAL.LT.-.01.OR.VAL.GT..01)then
            if(INOR.eq.2)then
              x1=x(1)     ! get position of vertex 1.
              y1=y(1)
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0
              rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
              rotateit(icomp,4)=VAL
              rotateit(icomp,5)=x1
              rotateit(icomp,6)=y1
            elseif(INOR.eq.3)then

C Scan G1 for vertes in this zone and allow user to select one.
              inpick=1
              call EPKVERT(INPICK,IVLST,'Rotation Point',
     &          'Select vertex to rotate around.',' ',nbhelp,ier)
              if(inpick.eq.1)then
                ivc=IVLST(1)
                x1=x(ivc)     ! get position of this vertex.
                y1=y(ivc)
                rotateit(icomp,1)=0.0
                rotateit(icomp,2)=0.0
                rotateit(icomp,3)=0.0
                rotateit(icomp,4)=VAL
                rotateit(icomp,5)=x1
                rotateit(icomp,6)=y1
              else
                goto 77
              endif
              CALL ESCROT(VAL,x1,y1)
            elseif(INOR.eq.4)then
              x1=0.         ! site origin is X=0. Y=0.
              y1=0.
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0
              rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
              rotateit(icomp,4)=VAL
              rotateit(icomp,5)=x1
              rotateit(icomp,6)=y1
            elseif(INOR.eq.5)then
              x1=0.
              CALL EASKR(x1,' ','X-coord?',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,nbhelp)
              y1=0.
              CALL EASKR(y1,' ',' Y-coord?',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,nbhelp)
              CALL ESCROT(VAL,x1,y1)
              rotateit(icomp,1)=0.0
              rotateit(icomp,2)=0.0
              rotateit(icomp,3)=0.0
              rotateit(icomp,4)=VAL
              rotateit(icomp,5)=x1
              rotateit(icomp,6)=y1
            endif
          else
            goto 77
          endif

          iZBFLG(ICOMP)=0
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif

C Warn user if there is a flow node associated with this zone that
C zone rotation may require boundary nodes to be updated.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &        'Rotating a zone may require updating of flow network ',
     &        'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')

C Rotate any MRT sensors associated with this zone.
          if (IVF(ICOMP).eq.1) then
            NCUB = 0
            call edisp(iuout,' Viewfactors prior to transform.')
            CALL ERMRT(ITRC,iuout,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if (NCUB.gt.0) then
              dok=.true.
              call askok('Apply this rotation to MRT sensors',
     &                   'associated with this zone? ',OK,dok,nbhelp)
              if (OK) then

C Depending on which point of rotation for the zone the
C rotation point x1 and y1 will have been set above.
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.
                CA=COS(A)
                SA=SIN(A)
                do 76 ij=1,NCUB
                  XXX=XOC(ij)-X1
                  YYY=YOC(ij)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XOC(ij)=XR+X1
                  YOC(ij)=YR+Y1
                  CANG(ij)=CANG(ij)+VAL
  76            continue
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
              endif
            else
              call edisp(iuout,
     &          'A transform will not alter the view factors between')
              call edisp(iuout,'the surfaces in the zone.')
            endif
          endif

C Rotate any obstructions associated with this zone.
          if(iobs(icomp).eq.1)then
            dok=.true.
            call askok('Apply this rotation to obstructions',
     &                 'associated with this zone? ',ok,dok,nbhelp)
            if(ok)then
              call edisp(iuout,' Obstruction data prior to transform.')
              itru=iuout
              CALL EGOMST(IFIL+2,ICOMP,ZOBS(ICOMP),0,ITRC,ITRU,IER)
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 86 ij=1,nbobs(icomp)
                XXX=XOB(icomp,ij)-X1
                YYY=YOB(icomp,ij)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XOB(icomp,ij)=XR+X1
                YOB(icomp,ij)=YR+Y1
                BANGOB(icomp,ij,1)=BANGOB(icomp,ij,1)+VAL
  86          continue
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          elseif(iobs(icomp).eq.2)then

C The zone geometry has just been written with transformed verticies
C so now apply the transform to the obstructions (only).
            dok=.true.
            call askok('Apply this rotation to obstructions',
     &                 'associated with this zone? ',ok,dok,nbhelp)
            if(ok.and.newgeo)then
              call edisp(iuout,' Obstruction data prior to transform.')
              PI = 4.0 * ATAN(1.0)
              A=-VAL*PI/180.
              CA=COS(A)
              SA=SIN(A)
              do 87 ij=1,nbobs(icomp)
                XXX=XOB(icomp,ij)-X1
                YYY=YOB(icomp,ij)-Y1
                XR=XXX*CA+YYY*SA
                YR=YYY*CA-XXX*SA
                XOB(icomp,ij)=XR+X1
                YOB(icomp,ij)=YR+Y1
                BANGOB(icomp,ij,1)=BANGOB(icomp,ij,1)+VAL
  87          continue
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            endif
          endif

        elseif(INOR.eq.7.or.INOR.eq.8.or.INOR.eq.9)then
          if(INOR.eq.7)then

C Ask the user what the transform for each axis should be.
            call tstamp('>','PRJ: do zone translation')
            hold = ' 0.000  0.000  0.000    '
  43        CALL EASKS(HOLD,'Transform (X Y Z metres):',' ',36,
     &      ' 0. 0. 0.','transforms XYZ',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
            CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
            CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
            if(ier.ne.0)goto 43
            DOK=.true.
            call askok('Apply this transform to zone surfaces?',' ',
     &        ok,dok,nbhelp)
            if(.NOT.ok) goto 3
          elseif(INOR.eq.8)then

C Identify two points that define the vector and find the difference
C between the two X and two Y and two Z. This uses G1 commons.
            inpick=2
            call EPKVERT(INPICK,IVLST,
     &        'Define Vector',
     &        'Select start and finish vertex.',' ',nbhelp,ier)
            if(inpick.eq.2)then
              ivcs=IVLST(1)
              ivcf=IVLST(2)
              x1=x(ivcs)     ! get position of initial vertex.
              y1=y(ivcs)
              z1=z(ivcs)
              x2=x(ivcf)     ! get position of initial vertex.
              y2=y(ivcf)
              z2=z(ivcf)
              VALX=x2-x1
              VALY=y2-y1
              VALZ=z2-z1
              write(msgv,'(a,3F8.4)') 'The transform is ',
     &          VALX,VALY,VALZ
              DOK=.true.
              call askok(msgv,' Is this ok?',ok,dok,nbhelp)
              if(.NOT.ok) goto 77
            else
              goto 77
            endif
          elseif(INOR.eq.9)then

C As the user for a start vertex in this zone and a final vertex in
C another zone. This will require saving and loading geometry common
C blocks (epkvert uses common G1).
            x1=0.0; y1=0.0; z1=0.0
            inpick=1
            call EPKVERT(INPICK,IVLST,'Start Point',
     &        'Select starting vertex.',' ',nbhelp,ier)
            if(inpick.eq.1)then
              ivcs=IVLST(1)
              x1=x(ivcs)     ! get position of this vertex.
              y1=y(ivcs)
              z1=z(ivcs)
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
     &        CFGOK,IZ,'-',IER)
            if(iz.eq.icomp)goto 3
            if(iz.eq.0) goto 3
            call edisp(iuout,
     &        ' updating current zone before proceeding...')
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+1,LGEOM(ICOMP),ICOMP,3,IER)
            endif
            MODIFY=.TRUE.
            MODBND=.TRUE.
            CALL INLNST(1)
            itsnm=0
            nzg=1
            nznog(1)=IZ
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
            MODIFY=.TRUE.
            MODBND=.TRUE.

C Load and scan G1 for vertes in other zone and allow user to select one.
            call georead(IFIL+1,LGEOM(IZ),IZ,0,iuout,IER)
            inpick=1
            call EPKVERT(INPICK,IVLST,'Other Zone Vertex',
     &        'Select vertex in other zone.',' ',nbhelp,ier)
            if(inpick.eq.1)then
              ivcf=IVLST(1)
              x2=x(ivcf)     ! get position of this vertex.
              y2=y(ivcf)
              z2=z(ivcf)

C Re-establish the initial zone geometry.
              call georead(IFIL+1,LGEOM(icomp),icomp,0,iuout,IER)
              VALX=x2-x1
              VALY=y2-y1
              VALZ=z2-z1
              write(msgv,'(a,3F8.4)') 'The transform is ',
     &          VALX,VALY,VALZ
              DOK=.true.
              call askok(msgv,' Is this ok?',ok,dok,nbhelp)
              if(.NOT.ok) goto 77
            else

C User aborted so scan the intial zone prior to returning.
              call georead(IFIL+1,LGEOM(icomp),icomp,0,iuout,IER)
              MODBND=.TRUE.
              MODIFY=.TRUE.
              goto 77
            endif
         endif

C If user has not canceled then perform the transform.
          DO 62 I=1,NTV
            X(I)=X(I)+VALX
            Y(I)=Y(I)+VALY
            Z(I)=Z(I)+VALZ
   62     continue
          iZBFLG(ICOMP)=0
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &      'Transforming a zone may require updating of flow network',
     &      'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')

C Transform any MRT sensors associated with this zone.
          if (IVF(ICOMP).eq.1) then
            NCUB = 0
            CALL ERMRT(ITRC,iuout,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if (NCUB.gt.0) then
              dok=.true.
              call askok('Apply this transform to MRT sensors',
     &                   'associated with this zone? ',ok,dok,nbhelp)
              if (OK) then
                do 73 ij=1,NCUB
                  XOC(ij)=XOC(ij)+VALX
                  YOC(ij)=YOC(ij)+VALY
                  ZOC(ij)=ZOC(ij)+VALZ
  73            continue
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
              endif
            else
              call edisp(iuout,
     &          'A transform will not alter the view factors between')
              call edisp(iuout,'the surfaces in the zone.')
            endif
          endif

C Transform any obstructions associated with this zone.
          if(iobs(icomp).eq.1)then
            dok=.true.
            call askok('Apply this transform to obstructions',
     &                 'associated with this zone? ',ok,dok,nbhelp)
            if(ok)then
              call edisp(iuout,' Obstruction data prior to transform.')
              CALL EGOMST(IFIL+2,ICOMP,ZOBS(ICOMP),0,ITRC,iuout,IER)
              do 63 ij=1,nbobs(icomp)
                XOB(icomp,ij)=XOB(icomp,ij)+VALX
                YOB(icomp,ij)=YOB(icomp,ij)+VALY
                ZOB(icomp,ij)=ZOB(icomp,ij)+VALZ
  63          continue
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          elseif(iobs(icomp).eq.2)then

C The zone geometry has just been written with transformed verticies
C so now apply the transform to the obstructions (only).
            dok=.true.
            call askok('Apply this transform to obstructions',
     &                 'associated with this zone? ',ok,dok,nbhelp)
            if(ok.and.newgeo)then
              call edisp(iuout,' Obstruction data prior to transform.')
               do 65 ij=1,nbobs(icomp)
                XOB(icomp,ij)=XOB(icomp,ij)+VALX
                YOB(icomp,ij)=YOB(icomp,ij)+VALY
                ZOB(icomp,ij)=ZOB(icomp,ij)+VALZ
  65          continue
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
            dok=.true.
            call askok(' ','Apply mirror transform?',ok,dok,nbhelp)
            if(ok)then
              do 342 iwmv=1,NTV
                if(INOR.eq.11)then
                  VALY=VALM-Y(iwmv)
                  Y(iwmv)=VALM+VALY
                elseif(INOR.eq.12)then
                  VALY=Y(iwmv)-VALM
                  Y(iwmv)=VALM-VALY
                endif
 342          continue
            else
              goto 77
            endif
          elseif(INOR.eq.13.or.INOR.eq.14)then

C Mirror along X axis, either positive or negative.
            call tstamp('>','PRJ: do zone mirror')
            CALL EASKR(VALM,' ','Mirror point on X-axis?',
     &           -50.0,'W',50.0,'W',0.0,'y mirror',IER,nbhelp)
            dok=.true.
            call askok(' ','Apply mirror transform?',ok,dok,nbhelp)
            if(ok)then
              do 343 iwmv=1,NTV
                if(INOR.eq.13)then
                  VALX=VALM-X(iwmv)
                  X(iwmv)=VALM+VALX
                elseif(INOR.eq.14)then
                  VALX=X(iwmv)-VALM
                  X(iwmv)=VALM-VALX
                endif
 343          continue
            else
              goto 77
            endif
          endif

C Now invert the ordering of the surfaces.
C <<  Note: this can cause problems when exporting the
C <<  model (e.g. for EnergyPlus) so more work is required).
          do 148, ins=1,NSUR
            do 146, iyy = 1,NVER(ins)
              jvn1(iyy)=JVN(ins,iyy)
  146       CONTINUE
            JVN(ins,1)=jvn1(2)
            JVN(ins,2)=jvn1(1)
            do 147, iyy = 3,NVER(ins)
              izz=NVER(ins)+3-iyy
              JVN(ins,iyy)=jvn1(izz)
  147       CONTINUE
  148     continue
          iZBFLG(ICOMP)=0

C Logic for deciding which format of geometry file to write.
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif

C Warn user if there is a flow node associated with this zone that
C zone mirroring may require boundary nodes to be updated.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &        'Mirroring a zone may require updating of flow network ',
     &        'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')
        elseif(INOR.eq.1.or.INOR.eq.6.or.INOR.eq.10.or.INOR.eq.15)then
          goto 77
        endif

C Ensure that the zone is redrawn and boundaries are checked.
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODGEO=.TRUE.

      ELSEIF(INO.EQ.ITEMC+13)THEN

C Check the file version, if new then allow thermal bridges.
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          CALL LINTHBRDG(ICOMP,'-',0.0)
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

      ELSEIF(INO.EQ.ITEMC+16)THEN

C Surface summary: print header, followed by surface information.
C And also report other zone summary fields as in the QA report.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        exposed = 0.0
        vexposed = 0.0
        areatran = 0.0
        areawall = 0.0
        areaslproof = 0.0
        areafltroof = 0.0
        areaskylt = 0.0
        uavgtran = 0.0
        uavgsky = 0.0
        uavwall = 0.0
        uavfltroof = 0.0
        uavslproof = 0.0
        wallper =0.0

        call tstamp('>','PRJ: list zone surface summary')
        context=.true.
        CALL SURINFO(ICOMP,iuout,context)

C Display obstructions if newgeom is true. Write out report
C format for each type of obstruction.
        if(newgeo)then
          if(iobs(icomp).eq.2)then
            if(nbobs(icomp).gt.0)then
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
         if(ICT(icc).eq.0.and.SSOTHER(icc,1)(1:5).ne.'EXTER')
     &     bndry=.false.
         if(ICT(icc).eq.1.and.SSOTHER(icc,1)(1:5).ne.'SIMIL')
     &     bndry=.false.
         if(ICT(icc).eq.2.and.SSOTHER(icc,1)(1:5).ne.'CONST')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'CONST')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'SIMIL')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'EXTER')
     &     bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc,1)(1:5).eq.'GROUN')
     &     bndry=.false.
         if(ICT(icc).eq.4.and.SSOTHER(icc,1)(1:5).ne.'GROUN')
     &     bndry=.false.
         if(ICT(icc).eq.5.and.SSOTHER(icc,1)(1:5).ne.'ADIAB')
     &     bndry=.false.
         if(ICT(icc).eq.6.and.SSOTHER(icc,1)(1:5).ne.'BASES')
     &     bndry=.false.

         if(ICT(icc).eq.0)then 

C Check the U value for each external surface.
C exposed walls and floors, pitched roofs, flat roofs
           UVH = 0.0
           UVU = 0.0
           UVD = 0.0
           do 511 ii=1,nmlc
             if(SMLCN(is)(1:12).eq.mlcname(ii)(1:12)) then

C Recover the ISO 6946 U values as in prjqa.F.
               call etmldbu(0,itu,ii,UVH,UVU,UVD,UVI,UVG)
             else
               continue
             endif
  511      continue
           exposed = exposed + SSNA(icc)
           if(SOTF(is)(1:4).eq.'OPAQ')then
             if(SVFC(is)(1:4).eq.'VERT')then

C For vertical walls assume horizontal hc coef.
               areawall = areawall + SSNA(icc)
               uavwall = uavwall + (SSNA(icc) * UVH)
               vexposed = vexposed + SSNA(icc)
             elseif(SVFC(is)(1:4).eq.'SLOP')then

C For sloped surfaces.
               areaslproof = areaslproof + SSNA(icc)
               uavslproof = uavslproof + (SSNA(icc) * UVU)
             elseif(SVFC(is)(1:4).eq.'CEIL')then

C For ceilings.
               areafltroof = areafltroof + SSNA(icc)
               uavfltroof = uavfltroof + (SSNA(icc) * UVU)
             else

C For floors.
               areawall = areawall + SSNA(icc)
               uavwall = uavwall + (SSNA(icc) * UVD)
             endif
           elseif(SOTF(is)(1:4).ne.'OPAQ'.and.
     &            SOTF(is)(1:3).ne.'CFC')then
             if(SVFC(is)(1:4).eq.'CEIL'.or.
     &          SVFC(is)(1:4).eq.'SLOP')then

C Consider glazing on ceiling or sloped to be a skylight.
               areaskylt = areaskylt + SSNA(icc)
               uavgsky = uavgsky + (SSNA(icc) * UVU) 
             elseif(SVFC(is)(1:4).eq.'VERT')then

C Consider glazing on walls to be a part of facade.
               areatran = areatran + SSNA(icc)
               uavgtran = uavgtran + (SSNA(icc) * UVH)
               vexposed = vexposed + SSNA(icc)
             else
               areatran = areatran + SSNA(icc)
               uavgtran = uavgtran + (SSNA(icc) * UVH)
               vexposed = vexposed + SSNA(icc)
             endif
           else
             continue
           endif
          endif
 1243   continue
        if(.NOT.bndry)then
          call edisp(iuout,
     &   'Some surface boundary attributions did not match the master')
          call edisp(iuout,
     &   'connections list. A topology update may be required.')
        endif

C Debug...
C        write(iuout,*) 'in zone ',icomp,' area wall = ',areawall
C        write(iuout,*) 'area flat roof = ',areafltroof
C        write(iuout,*) 'area sloped roof = ',areaslproof
C        write(iuout,*) 'area skylights = ',areaskylt
C        write(iuout,*) 'area windows = ',areatran
C        write(iuout,*) 'floor area = ',ZBASEA(icomp)
C        write(iuout,*) 'exposed = ',exposed
C        write(iuout,*) 'vert exposed = ',vexposed

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
            dok=.false.
            call askok(' ','Also view thermo-physical details?',
     &               ok,dok,nbhelp)
            if(ok)then
              CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)
              CALL CONINF(ICOMP,0,iuout)
            endif
          endif
        endif

      ELSEIF(INO.EQ.ITEMC+17)THEN

C Ask for name of file to put updated information into.
        helptopic='geometry_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(browse)then
          call usrmsg('In browse mode, therefore not saved.',' ','W')
          goto 3
        endif
        call tstamp('>','PRJ: update zone geometry')
        LTMP=LGEOM(ICOMP)
        CALL EASKS(LTMP,' ','Zone geometry file?',
     &                  72,GFILE,'geom file',IER,nbhelp)
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(igupgrade.eq.2.and.(.NOT.newgeo))then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        if(newgeo)then
          call geowrite(IFIL+2,LTMP,ICOMP,iuout,4,IER)
        else
          call emkgeo(IFIL+2,LTMP,ICOMP,4,IER)
        endif
        IF(IER.NE.0)THEN
          CALL USRMSG(' ','Problem creating file!','W')
          INO=-4
          GOTO 3
        ENDIF
        LGEOM(ICOMP)=LTMP
        CALL EMKCFG('-',IER)
        MODGEO=.FALSE.

      ELSEIF(INO.EQ.ITEMC+18)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0

      ELSEIF(INO.EQ.ITEMC+19)THEN

C Jump to previous zone after processing any remaining tasks.
        ianother=-1
        if(browse)return
        if(MODGEO)then
          DOK=.false.
          CALL ASKOK('Save changes to zone composition and surface',
     &         'attributes? (legacy and new format)',OK,DOK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LTMP,ICOMP,3,IER)
            endif
          endif

C If there is an associated flow node then update its volume and update
C the flow network.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            SUPNOD(ICAAS(ICOMP),2)=VOL(ICOMP)
            if(IPRODB.eq.IFIL+6)then
              IUM=IPRODB
            else
              IUM=IFIL+6
            endif
            CALL EFOPSEQ(IUM,LAPROB,3,IER)
            if(IER.eq.0)then

C << Note need a check so network flow file data is not written
C << to a graphic network flow file. Also need a way to update
C << graphic network flow file to reflect changes in zone volume.
              CALL MFWRIT(IUM)
              CALL ERPFREE(IUM,ISTAT)
            endif
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        return

      ELSEIF(INO.EQ.ITEMC+20)THEN

C Jump to next zone after processing any remaining tasks.
        ianother=1
        if(browse)return
        if(MODGEO)then
          DOK=.false.
          CALL ASKOK('Save changes to zone composition and surface',
     &         'attributes? (legacy and new format)',OK,DOK,nbhelp)
          LTMP=LGEOM(ICOMP)

C Test for saving current or new format geometry file.
          IF(OK)then
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LTMP,ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LTMP,ICOMP,3,IER)
            endif
          endif

C If there is an associated flow node then update its volume and update
C the flow network.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            SUPNOD(ICAAS(ICOMP),2)=VOL(ICOMP)
            if(IPRODB.eq.IFIL+6)then
              IUM=IPRODB
            else
              IUM=IFIL+6
            endif
            CALL EFOPSEQ(IUM,LAPROB,3,IER)
            if(IER.eq.0)then

C << Note need a check so network flow file data is not written
C << to a graphic network flow file. Also need a way to update
C << graphic network flow file to reflect changes in zone volume.
              CALL MFWRIT(IUM)
              CALL ERPFREE(IUM,ISTAT)
            endif
          endif
        endif

C Report on model contiguity changes.
        silent= .false.
        call sumrchg(ICOMP,'r',silent)
        return

      ELSEIF(INO.EQ.ITEMC+21)THEN

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

C Get common blocks g0/g2/g4/g6/precz from geometry.h
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
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

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDINSUL'  ! set for subroutine

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
        call edisp(iuout,
     &'Without shading obstructions only insolation can be calculated.')
      endif

      call eclose(gversion(icomp),1.1,0.01,newgeo)

   13 zn=zname(icomp)
      WRITE(ISD(1),'(A,A12)')   '  zone: ',zn     
      ISD(2)=                   'a specified insolation to:      '
      ioc1=0
      ioc2=0
      if(IDPN(ICOMP,1).gt.0)then
        ioc1=IZSTOCN(icomp,IDPN(ICOMP,1))
      endif
      if(IDPN(ICOMP,2).gt.0)then
        ioc2=IZSTOCN(icomp,IDPN(ICOMP,2))
      endif

      if(NDP(ICOMP).eq.1)then
        if(ioc1.gt.0)then
          write(isd(3),'(4x,a)') ssname(ioc1)
        else
          write(isd(3),'(4x,a)') 'unknown surface'
        endif
        write(isd(4),'(a)')     '                                '
      elseif(NDP(ICOMP).eq.2)then
        if(ioc1.gt.0)then
          write(isd(3),'(4x,a)') ssname(ioc1)
        else
          write(isd(3),'(4x,a)') 'unknown surface'
        endif
        if(ioc2.gt.0)then
          write(isd(4),'(4x,a)') ssname(ioc2) 
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
      ISD(16)=                  '- exit                          '
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
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
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
          call edisp(iuout,'Updating zone for changed directives.')
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          changed=.false.
        endif

C Allow user to start shading analysis. If user does not cancel suggest
C a new file name, ask the user to confirm that name and update the model
C cfg file and then if the user says do it now call comission ish.
        CALL EASKABC('Shading & insolation analysis options: ',' ',
     &   'do now','do later','cancel',IW,nbhelp)
        if(IW.eq.1.or.IW.eq.2)then
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
          IDPN(ICOMP,1)=IS
          IDPN(ICOMP,2)=0
          IDPN(ICOMP,3)=IS
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
          IDPN(ICOMP,2)=IS
          IDPN(ICOMP,3)=-1
          CALL USRMSG(' ',' ','-')
          changed=.true.
        ELSEIF(IVAL(1).EQ.3)THEN

C Diffuse distribution.
          NDP(ICOMP)=3
          IDPN(ICOMP,1)=0
          IDPN(ICOMP,2)=0
          IDPN(ICOMP,3)=0
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
        IDPN(ICOMP,1)=0
        IDPN(ICOMP,2)=0
        IDPN(ICOMP,3)=0

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
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      DIMENSION STMP(MS),IVA(MS),IVALS(MS)
      character STMP*42,act*1,msg1*32,msg2*32

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='PICKSSUR'  ! set for subroutine

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
         CALL EASKABCD(' ','Insolation calculationoptions:',
     &      'all applicable surfaces','selected surfaces',
     &      'none','cancel ? ',IIC,nbhelp)
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
          DO 13 I=1,NZSUR(IZONE)
            ioc=IZSTOCN(izone,i)
            if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
              NP=NP+1
              IVA(NP)=I
            endif
  13      continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found.',' ','W')
            goto 43
          endif
          iaplic=1
          return
        elseif(act.eq.'i'.or.act.eq.'I')then
          NP=0
          DO 15 I=1,NZSUR(IZONE)
            ioc=IZSTOCN(izone,i)
            if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &         SSOTF(ioc)(1:4).ne.'OPAQ')then
              NP=NP+1
              IVA(NP)=I
            endif
   15     continue
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
        DO 11 I=1,NZSUR(IZONE)
         ioc=IZSTOCN(izone,i)
         if(act.eq.'s'.or.act.eq.'S')then
           if(SSOTHER(ioc,1)(1:5).eq.'EXTER')then
             write(STMP(I),'(5a)') SSNAME(ioc),'|',SSOTF(ioc)(1:6),' |',
     &         SSOTHER(ioc,1)(1:16)
           else
             write(STMP(I),'(2a)') SSNAME(ioc),' not applicable'
           endif
         elseif(act.eq.'-')then
           write(STMP(I),'(5a)') SSNAME(ioc),'|',SSMLCN(ioc)(1:12),'|',
     &       SSOTHER(ioc,1)(1:16)
         elseif(act.eq.'c'.or.act.eq.'C')then
           write(STMP(I),'(5a)') SSNAME(ioc),'|',SSMLCN(ioc)(1:12),'|',
     &       SSOTHER(ioc,1)(1:15)
         elseif(act.eq.'i'.or.act.eq.'I')then
           if(SSOTHER(ioc,1)(1:5).eq.'EXTER'.and.
     &        SSOTF(ioc)(1:4).ne.'OPAQ')then
             write(STMP(I),'(5a)') SSNAME(ioc),'|',SSOTF(ioc)(1:6),
     &         ' |',SSOTHER(ioc,1)(1:16)
           else
             write(STMP(I),'(2a)') SSNAME(ioc),' not applicable'
           endif
         endif
   11   CONTINUE

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
          DO 12 I=1,NP
            IVA(I)=IVALS(I)
   12     CONTINUE
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

C ************* EDSURA 
C Edit zone surface attributes in common block G5 and allow this to be
C saved to a geometry file ITRU unit number for user output, IER=0 OK, 
C IER=1 problem. Make use of construction information in common MLC.
C ianother is returned as +1 if jump to next surface, -1 if jump to
C    prior surface, otherwise zero.

C << add in additional menu line for external viewfactors >>
C << add in additional menu line for external diffuse shading >>
C << add in additional menu line for external hc correlation >>

C << it would clarify the surface if it was reported which convection >>
C << correlation is being used for inside face. And should it be >>
C << possible to alter the inside/outside hc from this interface? >>

      SUBROUTINE EDSURA(ICOMP,ISS,ITRC,MODGEO,ianother,IER)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Pased parameters
      integer icomp   ! zone index
      integer iss     ! surface index
      integer itrc    ! reporting unit
      logical modgeo
      integer ianother ! set if user wants to jump to next/previous
      integer ier     ! for error state

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)

      common/user/browse
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL MODIFY,MODLEN,MODBND,browse,nameok,close,showother
      logical dupedges,XST,OK,DOK,updoth
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION SALT(14),IVAL(MCOM)  ! ,bl(144)
      DIMENSION cog(3),cogo(3),COG1(3),COG2(3),COG3(3)
      integer iocog  ! loop counter for detecting close surfaces
      integer ifound,ifoundc ! how many close surfaces and which one
      integer icontinue  ! to control while loop
      CHARACTER*33 ISD(25)
      CHARACTER DESCRC*25,outs*124,CXSTR*78
      CHARACTER SALT*33,SN*12,SN2*12,T14*14
      character ozn*12  ! the name of the other zone
      character guesstype*24
      character TOSMLCN*32  ! to remember other side mlc name
      character TOOPT*32    ! to remember other side optics

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*8,rsuse2*8
      character rsparent*12
      real XO1,ZO1  ! to prevent name clash in geometry.h
      integer ISDN,INODA ! max items and current menu item
      integer mnulen,iwhich,iwhich2,numv   ! multi column lines
      logical greyok,odd   ! for display of vertex info
      logical closecog,closea     ! to check for nearby surfaces
      integer lnsa,lnsb    ! lengths for surface names
      integer ii,iloop         ! for loop
      character message*48,t248*248

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      integer iissmlci,IIC3   ! other side construction index, surface index
      real vdist         ! distance between cog of two surfaces

#ifdef OSI
      integer iix,iiy,iicol
#else
      integer*8 iix,iiy,iicol
#endif

      helpinsub='EDSURA'  ! set for subroutine
      greyok=.false.      ! see if colour available
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Display a selection of surfaces available in the zone. If surface
C faces another zone, get the other surface connection index. 
      IS=ISS
      icoth=0
      ii=0
      ioc=IZSTOCN(icomp,is)
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
          WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') ' vertex ',iwhich,
     &      X(iwhich),Y(iwhich),Z(iwhich),'   vertex ',iwhich2,
     &      X(iwhich2),Y(iwhich2),Z(iwhich2)
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
      WRITE(t248,'(42I4)')(JVN(is,j),J=1,numv)
      call edisp248(iuout,t248,90)

C Force redraw of zone on entry to highlight focus surface.
      MODIFY=.TRUE.
      newgeo=.false.  ! assume older format geometry.
      TOSMLCN=' '
      TOOPT=' '
      iissmlci=0

C Check which version.
      call eclose(gversion(icomp),1.1,0.01,newgeo)

C Check perimeter.
      call surfprm(icomp,is,dupedges,perim)
      if(dupedges)then
        call edisp(iuout,
     &    'This surface probably wraps around another surface.')
      endif

   13 WRITE(ISD(1),'(A,A12)')  'a surface name    : ',SSNAME(ioc)
      if(SSOTF(ioc)(1:4).eq.'OPAQ'.or.SSOTF(ioc)(1:4).eq.'TRAN'.or.
     &                                SSOTF(ioc)(1:3).eq.'CFC')then     
        WRITE(ISD(2),'(A,A12)')'b surface type    : ',SSOTF(ioc)(1:12)
      else
        WRITE(ISD(2),'(A,A12)')'b optical set name: ',SSOTF(ioc)(1:12)
      endif
      WRITE(ISD(3),'(A,A4)')   'c surface location: ',SSVFC(ioc)
      WRITE(ISD(4),'(A,f8.3)') '  surface area m^2:',SSNA(ioc)
      WRITE(ISD(5),'(A,2f7.2)')'  azim & elevation:',SSPAZI(ioc),
     &  SSPELV(ioc)
      WRITE(ISD(6),'(A,f8.3)') '  perimeter length:',ssperim(ioc)
      if(newgeo)then
        WRITE(ISD(7),'(A,F6.3)') '  thermophysical thickness ',
     &    ssthick(ioc)
      else
        WRITE(ISD(7),'(A,F7.4)') 'd surface indentation:',Y0S(IS)
      endif
      WRITE(ISD(8),'(A,A12)')  'e construction    : ',SSMLCN(ioc)(1:12)     
      WRITE(ISD(9),'(A,A16)')  'f environment  : ',SSOTHER(ioc,1)(1:16)

C If newgeo include a line for ssparent and one for ssuse.
      if(newgeo)then

C Info about children of this surface.
        if(nbchild(ioc).gt.0)then
          if(nbchild(ioc).eq.1.and.ichild(ioc,1).gt.0)then
            WRITE(ISD(10),'(2A)') 'g parent of    : ',
     &        SSNAME(ichild(ioc,1))
          elseif(nbchild(ioc).gt.1)then
            WRITE(ISD(10),'(A,i1,A)') 'g parent of    : ',
     &        nbchild(ioc),' surfaces'
          else
            WRITE(ISD(10),'(A)')  'g parent of    : -'
          endif
        else
          WRITE(ISD(10),'(A)')  'g parent of    : -'
        endif
        WRITE(ISD(11),'(2A)')   'g child of     : ',SSPARENT(ioc)(1:12)
        if(SSUSE(ioc,1)(1:2).eq.'- '.or.
     &     SSUSE(ioc,1)(1:2).eq.'  ')then
          WRITE(ISD(12),'(A)')  'h use type   : ordinary surface'
          WRITE(ISD(13),'(A)')  '  use subtype: '
        else
          WRITE(ISD(12),'(2A)') 'h use type   : ',SSUSE(ioc,1)
          WRITE(ISD(13),'(2A)') '  use subtype: ',SSUSE(ioc,2)
        endif
      else
        if(nbchild(ioc).gt.0)then
          if(nbchild(ioc).eq.1.and.ichild(ioc,1).gt.0)then
            WRITE(ISD(10),'(2A)') 'g parent of    : ',
     &        SSNAME(ichild(ioc,1))
          elseif(nbchild(ioc).gt.1)then
            WRITE(ISD(10),'(A,i1,A)') 'g parent of    : ',
     &        nbchild(ioc),' surfaces'
          else
            WRITE(ISD(10),'(A)')  'g parent of    : -'
          endif
        else
          WRITE(ISD(10),'(A)')  'g parent of    : -'
        endif
        WRITE(ISD(11),'(2A)')   '  child of     : ',SSPARENT(ioc)(1:12)
        WRITE(ISD(12),'(A)')    '  use type   : (NA)'
        WRITE(ISD(13),'(A)')    '  use subtype: (NA)'
      endif

C << report inside and outside hc regime here >>

      ISD(14)        ='  ___ @ other face____________ '
      if(showother)then
        if(IC2(ioc).ne.0.and.IE2(ioc).ne.0)then
          icoth=IZSTOCN(IC2(ioc),IE2(ioc))
          WRITE(ISD(15),'(2A)')'   surface name: ',SSNAME(icoth)
          write(ozn,'(a)') zname(IC2(ioc))
          WRITE(ISD(16),'(2A)') '   surface type: ',SSOTF(icoth)(1:12)
          WRITE(ISD(17),'(2A)') '   location    : ',SSVFC(icoth)
          WRITE(ISD(18),'(2A)') '   construction: ',SSMLCN(icoth)(1:12)
          WRITE(ISD(19),'(2A)') '   in zone     : ',ozn(1:lnblnk(ozn))
        else
          WRITE(ISD(15),'(A)')'   surface name: IS NOT KNOWN'
          write(ozn,'(a)') 'IS NOT KNOWN'
          WRITE(ISD(16),'(A)') '   surface type: IS NOT KNOWN'
          WRITE(ISD(17),'(A)') '   location    : IS NOT KNOWN'
          WRITE(ISD(18),'(A)') '   construction: IS NOT KNOWN'
          WRITE(ISD(19),'(A)') '   in zone     : IS NOT KNOWN'
        endif

C << report other faces inside and outside hc regime here >>

        ISD(20)=      '  ____________________________ '
        ISD(21)=      '+ add glazing/door/opening     '
        if(is.gt.1.and.is.lt.nzsur(icomp))then
          ISD(22)=    '< jump to previous surface     '
          ISD(23)=    '> jump to next surface         '
        elseif(is.eq.1)then
          ISD(22)=    '                               '
          ISD(23)=    '> jump to next surface         '        
        elseif(is.eq.nzsur(icomp))then
          ISD(22)=    '< jump to previous surface     '
          ISD(23)=    '                               '
        endif
        ISD(24)=      '? help                         '
        ISD(25)=      '- exit this menu               '
        ISDN=25
      else

C << add in additional menu line for external viewfactors >>
C << add in additional menu line for external diffuse shading >>
C << add in additional menu line for external hc correlation >>

        ISD(14)=      '  ____________________________ '
        ISD(15)=      '+ add glazing/door/opening     '
        if(is.gt.1.and.is.lt.nzsur(icomp))then
          ISD(16)=    '< jump to previous surface     '
          ISD(17)=    '> jump to next surface         '
        elseif(is.eq.1)then
          ISD(16)=    '                               '
          ISD(17)=    '> jump to next surface         '        
        elseif(is.eq.nzsur(icomp))then
          ISD(16)=    '< jump to previous surface     '
          ISD(17)=    '                               '
        endif
        ISD(18)=      '? help                         '
        ISD(19)=      '- exit to zone description     '
        ISDN=19
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
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C If in graphic mode draw the points on top of standard drawing.
      if(MMOD.lt.8)then
        continue
      else
        icontinue=1
        iloop=0
        numv=NVER(IS)
        do while (icontinue.ne.0)
          iloop=iloop+1  ! increment iloop until = numv then jump out
          if(iloop.eq.numv) icontinue=0
          iwhich=jvn(is,iloop)
          if(iwhich.gt.0.and.iwhich.le.MTV)then
            COG1(1)=X(iwhich); COG1(2)=Y(iwhich); COG1(3)=Z(iwhich)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            iicol=0
            if(greyok)call winscl('z',iicol)
            call esymbol(iix,iiy,24,1)
            call VERTLBL(iix,iiy,iwhich,ier)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif
        enddo  ! of while loop
      endif

C Help text for the menu.
      helptopic='surface_attrib_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Menu control.
      CALL EMENU('Surface Attributes',ISD,ISDN,INODA)
      IF(INODA.EQ.ISDN)THEN

C If an attribute has changed save the geometry file.
        ianother=0   ! signal no jump to previous or next.
        if(cfgok.and.(.NOT.browse))then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
            endif
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
        if(cfgok.and.(.NOT.browse))then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
            endif
          endif
        endif
        return

      ELSEIF(INODA.eq.ISDN-3)THEN

C Jump to previous surface.
        ianother=-1
        
C If an attribute has changed save the geometry file.
        if(cfgok.and.(.NOT.browse))then
          if(MODGEO)then
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
            endif
          endif
        endif
        return

      ELSEIF(INODA.EQ.ISDN-4)THEN

C Add/insert a surface, return to higher level menu when done.
C The 'ii' request an interactive insertion, the parameters
C XO1,ZO1,XW,ZH are not used in an interactive insert.
        LASTS=NSUR
        call tstamp('>','PRJ: insert surface into another')
        XO1=0.
        ZO1=0.
        XW=0.
        ZH=0.
        rsname=' '
        rsotf=' '
        rsmlcn=' '
        rsuse1=' '
        rsuse2=' '
        rsparent='-'
        CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO1,ZO1,XW,ZH,
     &    rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)
        if(LASTS.ne.NSUR)then
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          CALL EMKCFG('-',IER)
          call warnmod(ICOMP,'sf+')
        endif
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
        return
      ELSEIF(INODA.EQ.1)THEN
        write(T14,'(2a)')SSNAME(ioc),'  '
 52     CALL EASKS(T14,' Surface name?','(<=12 characters, no spaces)',
     &    14,'new_surface','surface name',IER,nbhelp)
        write(SN,'(a)') T14(1:12)
        call st2name(SN,SN2)

        call snamdup(SN2,icomp,is,nameok)
        if(nameok)then
          SNAME(ICOMP,IS)=SN2
          SSNAME(ioc)=SN2

C Save names so they are not wiped out by wire-frame drawing
C code.
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
        else
          call edisp(iuout,'Surface name is a duplicate of an existing')
          call edisp(iuout,'surface. Please supply a different name.')
          goto 52
        endif
        MODIFY=.TRUE.
        MODGEO=.TRUE.
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
          SOTF(IS)='OPAQUE'
          SSOTF(ioc)='OPAQUE'
        ELSEIF(IVAL(1).EQ.2)THEN

C If 1.1 geometry format the TRAN will be updated if/when
C the user defines the optical property set and/or the construction.
          SOTF(IS)='TRAN'
          SSOTF(ioc)='TRAN'
        ELSE
          goto 9
        ENDIF
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.3)THEN

C Specify surface location for daylighting calculations and floor area
C based casual gain calculations.

C << the UK NCM also has a concept of a low slope roof so it might
C << be useful to extend the keywords included in SVFC

        if(SSVFC(ioc)(1:4).eq.'UNKN')ino=1
        if(SSVFC(ioc)(1:4).eq.'VERT')ino=2
        if(SSVFC(ioc)(1:4).eq.'CEIL')ino=3
        if(SSVFC(ioc)(1:4).eq.'FLOR')ino=4
        ilno=ino
        idno=2
        call MENUATOL(' ','Surface Orientation',
     &   'a unknown or sloped','b vertical (or nearly so)',
     &   'c facing down (e.g. ceiling) ','d facing up (e.g. floor)',
     &   ' ',' ',' ',' ',' ',' ',' ',' ',ino,idno,nbhelp)
        IF(ilno.eq.ino)GOTO 13
        IF(ino.eq.0)GOTO 13
        IF(ino.EQ.1)THEN
          SVFC(IS)='UNKN'
          SSVFC(ioc)='UNKN'
        ELSEIF(ino.EQ.2)THEN
          SVFC(IS)='VERT'
          SSVFC(ioc)='VERT'
        ELSEIF(ino.EQ.3)THEN
          SVFC(IS)='CEIL'
          SSVFC(ioc)='CEIL'
        ELSEIF(ino.EQ.4)THEN
          SVFC(IS)='FLOR'
          SSVFC(ioc)='FLOR'
        ENDIF
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.7)THEN

C << version 1.1 geometry no longer has a concept of surface
C << indentation so the menu needs to be revised and this
C << item might need to be disabled.

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
        ioc=IZSTOCN(ICOMP,is)
        write(SN,'(a12)') SSNAME(ioc)
        if(SSMLCN(ioc)(1:4).ne.'UNKN')then

C << TODO - test if date of zone construction file is older than the
C << geometry file and if so do not use CONINF.

C Use ssmlcindex to get MLC index for surface.
          if(LTHRM(ICOMP)(1:7).eq.'UNKNOWN'.or.
     &       LTHRM(ICOMP)(1:2).eq.'  ')then
            if(ssmlcindex(ioc).ne.0)then
              write(outs,'(2a)') SN(1:lnblnk(SN)),'?'
              dok=.false.
              call askok('View thermo-physical properties of ',
     &                    outs,ok,dok,nbhelp)
              if(ok)then
                call ETMLDB(1,iuout,ii,imerr)        
              endif
            endif
          else

C A non-blank, possibly known construction.
            XST=.FALSE.
            call FINDFIL(LTHRM(ICOMP),XST)
            if(XST)then
              write(outs,'(2a)') SN(1:lnblnk(SN)),'?'
              dok=.false.
              call askok('View thermo-physical properties of ',outs,
     &                   ok,dok,nbhelp)
              if(ok)then
                CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)

C << This would also be a good place to list out optical
C << properties and the optical name.
                CALL CONINF(ICOMP,IS,iuout)
              endif
            endif
          endif
          write(outs,'(2a)') SN(1:lnblnk(SN)),'?'
          dok=.false.
          helptopic='surface_non_symetric'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call askok('Modify thermo-physical data of',outs,
     &      ok,dok,nbhelp)
        else
          ok=.true.
        endif
        if(ok)then
          CALL EPMENSV
          CALL EPKMLC(ISEL,'Select one of the constructions.',' ',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            ioc=IZSTOCN(ICOMP,is)
            WRITE(SMLCN(IS),'(A12)') mlcname(ISEL)(1:12)
            SSMLCN(ioc)=SMLCN(is)
            ssmlcindex(ioc)=isel   ! update the array
            ssthick(ioc)=THKMLC(isel)  ! update the thermophysical thickness

C Find the optical name.
            IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
              SOTF(IS)='OPAQUE'
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
              SOTF(IS)='CFC '
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC2')then
              SOTF(IS)='CFC2'
            ELSE
              WRITE(SOTF(ISS),'(A)') mlcoptical(ISEL)
            ENDIF
            SSOTF(ioc)=SOTF(is)
            MODGEO=.TRUE.
            call warnmod(ICOMP,'sat')

C If this is a partition find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.

C Use ssmlcindex to get MLC index for this surface and material.h
C commons to get information on the other partition.
            if(showother.and.icoth.ne.0.and.ssmlcindex(ioc).ne.0)then
              ii=ssmlcindex(ioc)
              updoth=.false.
              if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                if(SSMLCN(icoth)(1:12).eq.mlcname(ii)(1:12))then
                  TOSMLCN= mlcname(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
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
                write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &            ' has a nonsymmetric construction ',
     &            SSMLCN(ioc)(1:lnblnk(SSMLCN(ioc))),'.'
                call edisp(iuout,outs)
                write(outs,'(5a)') 'It faces ',
     &            SSNAME(icoth),' which is composed of ',
     &            SSMLCN(icoth)(1:12),' (which may not match).'
                call edisp(iuout,outs)
                call edisp(iuout,
     &            'Please check that one construction has inverted')
                call edisp(iuout,
     &            'layers or revise the common constructions.')
                if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                  TOSMLCN= mlcname(ii)  ! replace UNKNOWN
                  iissmlci=ssmlcindex(ioc)
                  updoth=.true.
                else
                  TOSMLCN= SSMLCN(icoth)
                  iissmlci=ssmlcindex(icoth)  ! leave it alone
                  updoth=.false.
                endif
              else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
                if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                  TOSMLCN= mlcsymetric(ii)
                  write(TOOPT,'(a)') mlcoptical(ii)
                  updoth=.true.
                elseif(SSMLCN(icoth)(1:12).eq.
     &                 mlcsymetric(ii)(1:12))then
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
                lnicoth=lnblnk(SSMLCN(icoth))
                write(outs,'(6a)') 
     &            'Updating `other side` composition of ',
     &            SN(1:lnblnk(SN)),' from ',SSMLCN(icoth)(1:lnicoth),
     &            ' > ',TOSMLCN
                call edisp(iuout,outs)
                call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                if(newgeo)then
                  call georead(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,
     &              iuout,IER)
                else
                  call egomin(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,0,
     &              iuout,IER)
                endif

C Assign new MLC name to the icoth surface as well as SSOTF with the
C optical properties and MLC index from material.h commons.
                SSMLCN(icoth)=TOSMLCN
                if(iissmlci.ne.0) ssmlcindex(icoth)=iissmlci  ! update
                write(SSOTF(icoth),'(a)') TOOPT

C Logic to decide whether to update other zone geometry format.
                call eclose(gversion(IC2(ioc)),1.1,0.01,newgeo)
                if(igupgrade.eq.2.and.(.NOT.newgeo))then
                  gversion(IC2(ioc)) =1.1
                  newgeo = .true.
                endif
                if(newgeo)then
                  call geowrite(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &              iuout,3,IER)
                else
                  call emkgeo(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),
     &              3,IER)
                endif
                IF(IER.NE.0)CALL USRMSG(
     &            'Problem updating other surface attribute... ',
     &            '(could not write other geometry file).','W')

C Read current zone data back in to commons. But the backed up data
C might not include the name of the newly selected mlc name so just
C to be sure reset smlcn with ssmlcn so it will show up in the menu
C when it is re-freshed.
                CALL ERCZONE(ICOMP)
                SMLCN(is)=SSMLCN(ioc)
                SOTF(is)=SSOTF(ioc)
                call usrmsg(' Updating other side...done.',' ','-')
                updoth=.false.
              endif
            endif
          else

C User selected UNKNOWN MLC so update arrays.
            ioc=IZSTOCN(ICOMP,is)
            WRITE(SMLCN(IS),'(A)') 'UNKNOWN'
            SSMLCN(ioc)=SMLCN(is)
            SOTF(IS)='OPAQUE'
            SSOTF(ioc)=SOTF(is)
            ssmlcindex(ioc)=0
            MODGEO=.TRUE.

          endif
          IF(IER.EQ.1)THEN
            CALL USRMSG(' ','A problem was encountered.','W')
          ENDIF
        endif
      ELSEIF(INODA.EQ.9)THEN

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

        helptopic='surface_boundary_opt'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IX=1
        CALL EPICKS(IX,IVAL,' ','Other Side Boundary Condition',
     &    33,14,SALT,'surface boundary ',IER,nbhelp)

        IF(IX.EQ.0) goto 13
        IF(IVAL(1).EQ.14) goto 13

C Determine the match in the system topology and update it. 
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        IC1(IC)=ICOMP
        IE1(IC)=IS
        IF(IVAL(1).EQ.1)THEN
          SOTHER(IS,1)='EXTERIOR'; SOTHER(IS,2)='0'; SOTHER(IS,3)='0'
          SSOTHER(IC,1)='EXTERIOR'; SSOTHER(IC,2)='0'
          SSOTHER(IC,3)='0'
          ICT(IC)=0; IC2(IC)=0; IE2(IC)=0
          showother=.false.
          updoth=.false.
        ELSEIF(IVAL(1).EQ.2)THEN
          SOTHER(IS,1)='SIMILAR'; SSOTHER(IC,1)='SIMILAR'
          ICT(IC)=1
          VALT=0.0; VALW=0.0
          CALL EASKR(VALT,' ','Temperature offset?',
     &           -99.,'F',700.,'F',0.0,'offset temp',IER,nbhelp)
          CALL EASKR(VALW,' ','Radiation offset?',
     &           0.0,'W',99999.,'W',0.0,'offset rad',IER,nbhelp)
          write(SOTHER(IS,2),'(i2.2)')INT(VALT)
          write(SOTHER(IS,3),'(i2.2)')INT(VALW)
          write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
          write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
          IC2(IC)=INT(VALT)
          IE2(IC)=INT(VALW)
          showother=.false.
          updoth=.false.
        ELSEIF(IVAL(1).EQ.3)THEN
          SOTHER(IS,1)='CONSTANT'; SSOTHER(IC,1)='CONSTANT'
          ICT(IC)=2
          VALT=0.0; VALW=0.0
          CALL EASKR(VALT,' ','Constant temperature?',
     &      -99.,'F',700.,'F',20.,'adjacent temperature',IER,nbhelp)
          CALL EASKR(VALW,' ','Constant radiation?',
     &      0.0,'W',99999.,'W',0.0,'adjacent radiation source',
     &      IER,nbhelp)
          IC2(IC)=INT(VALT)
          IE2(IC)=INT(VALW)
          write(SOTHER(IS,2),'(i2.2)')INT(VALT)
          write(SOTHER(IS,3),'(i2.2)')INT(VALW)
          write(SSOTHER(IC,2),'(i2.2)')INT(VALT)
          write(SSOTHER(IC,3),'(i2.2)')INT(VALW)
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
     &      'this zone.',IER)
          CALL EPMENRC
          if(ISO.GT.0)then

C Write ANOTHER followed by the indices of zone and surface.
            SSOTHER(ic,1)='ANOTHER'
            write(SSOTHER(ic,2),'(i2.2)') iz
            write(SSOTHER(ic,3),'(i2.2)') iso
            ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO

C Sort out the other face attributes.
            ico = izstocn(iz,iso)
            IC1(ICO)=IZ
            IE1(ICO)=ISO
            ICT(ICO)=3
            IC2(ICO)=IC1(IC)
            IE2(ICO)=IE1(IC)

            SSOTHER(ico,1)='ANOTHER'
            write(SSOTHER(ICO,2),'(i2.2)') IC2(ICO)
            write(SSOTHER(ICO,3),'(i2.2)') IE2(ICO)

C Also check the other side construction and see if it matches. If
C current surface MLC index is known get isymindex for the other side
            if(ssmlcindex(ic).gt.0) then
              ii=ssmlcindex(ic)
              if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                if(SSMLCN(ico)(1:12).eq.mlcname(ii)(1:12))then
                  TOSMLCN= mlcname(ii)
                elseif(SSMLCN(ico)(1:4).eq.'UNKN')then
                  TOSMLCN= mlcname(ii)
                else
                  TOSMLCN= mlcname(ii)
                endif
                iissmlci=ii
              elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition if db does not have a linked MLC - inform the user.
                write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &            ' has a nonsymmetric construction ',
     &            SSMLCN(ic)(1:lnblnk(SSMLCN(ic))),'.'
                call edisp(iuout,outs)
                write(outs,'(5a)') 'It faces ',
     &            SSNAME(ico),' composed of ',
     &            SSMLCN(ico)(1:12),' (which may not match).'
                call edisp(iuout,outs)
                call edisp(iuout,
     &  'Please check attributes and/or revise the Cconstructions db.')
                iissmlci=matsymindex(ii)  ! use returned value 
              else
                if(SSMLCN(ico)(1:4).eq.'UNKN')then
                  TOSMLCN= mlcsymetric(ii)
                elseif(SSMLCN(ico)(1:12).eq.mlcsymetric(ii)(1:12))then
                  TOSMLCN= mlcsymetric(ii)
                else
                  TOSMLCN= mlcsymetric(ii)
                endif
                iissmlci=matsymindex(ii)  ! use returned value
              endif
            else
              iissmlci=0  ! mark as unknown
              TOSMLCN='UNKNOWN'
            endif

C Update the attributes of the other surface in the zone so it will
C be saved along with the changes in the initial surface.
            SSMLCN(ico)=TOSMLCN
            if(iissmlci.ne.0) ssmlcindex(ico)=iissmlci  ! update if known
            SSOTHER(ico,1)='ANOTHER'
            write(SSOTHER(ico,2),'(i2.2)') IC2(ico)
            write(SSOTHER(ico,3),'(i2.2)') IE2(ico)
            showother=.true.  ! when re-display include info on matched

          else

C User did not select surface so continue without changing.
            continue
          endif
        ELSEIF(IVAL(1).EQ.5)THEN

C Ask for specific zone to find a matching partition in.
          CALL EASKGEOF('If other zone known select it, else exit.',
     &      CFGOK,IZ,'-',IER)
          if(IZ.GT.0)then

C General image option flags.
            ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
            ITVNO=1; ITORG=1; ITSNR=1; ITGRD=1
            GRDIS=0.0; ITPPSW=0

C Remember the COG of the current surface and then compare it with
C the cog of the surfaces in the other zone as well as their areas.
C Report if within 1m and 0.5m2 area difference.
            cog(1)=ssurcog(ic,1)
            cog(2)=ssurcog(ic,2)
            cog(3)=ssurcog(ic,3)
            do 77 ij=1,nzsur(iz)
              closecog=.false.
              iocog=IZSTOCN(iz,ij)
              cogo(1)=ssurcog(iocog,1)
              cogo(2)=ssurcog(iocog,2)
              cogo(3)=ssurcog(iocog,3)
              call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3),1.0,closecog)
              if(closecog)then
                CALL ECLOSE(SSNA(iocog),ssna(ic),0.5,closea)
                if(closea)then
                  lnsa=lnblnk(ssname(iocog)); lnsb=lnblnk(ssname(ic))
                  write(outs,'(5a,f6.2,3a,f6.2)') 'Surface ',
     &              ssname(iocog)(1:lnsa),' in ',
     &              zname(iz)(1:lnzname(iz)),
     &              ' with area ',SSNA(iocog),' might match ',
     &              ssname(ic)(1:lnsb),' with area ',ssna(ic)
                  call edisp(iuout,outs)
                endif
              endif
  77        continue

C Reset all surface lines to standard width except for current,
C and display both zones.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            CALL INLNST(1)
            LINSTY(IC)=2
            nzg=2
            nznog(1)=ICOMP
            nznog(2)=IZ
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
            MODIFY=.TRUE.
            MODBND=.TRUE.

            ISO=1
            CALL EPMENSV
            CALL EASKSUR(IZ,ISO,'A','Select surface in',
     &        'adjacent zone.',IER)
            CALL EPMENRC
            if(ISO.GT.0)then

C Write ANOTHER followed by the indices of zone and surface.
              SSOTHER(ic,1)='ANOTHER'
              write(SSOTHER(ic,2),'(i2.2)') iz
              write(SSOTHER(ic,3),'(i2.2)') iso
              ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO

C If applicable find other surface, return its connection number and
C point it to the current surface.
              updoth=.false.
              dok=.true.
              CALL ASKOK(' ','Update other surface side?',
     &          updoth,dok,nbhelp)
              if(updoth)then
                CALL SURADJ(IZ,ISO,IE,TMP,IZC,ISC,ICO,DESCRC)
                IC1(ICO)=IZ; IE1(ICO)=ISO; ICT(ICO)=3
                IC2(ICO)=IC1(IC); IE2(ICO)=IE1(IC)

                SSOTHER(ico,1)='ANOTHER'
                write(SSOTHER(ICO,2),'(i2.2)') IC2(ICO)
                write(SSOTHER(ICO,3),'(i2.2)') IE2(ICO)

C Update information needed to display information on the other side
C of this surface (i.e. if we went from unknown to a partition).
                icoth=ICO

C Also check the other side construction and see if it matches. If
C current surface MLC index is known get isymindex for the other side
                if(ssmlcindex(ic).gt.0) then
                  ii=ssmlcindex(ic)
                  if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                    if(SSMLCN(icoth)(1:12).eq.mlcname(ii)(1:12))then
                      TOSMLCN= mlcname(ii)
                    elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      TOSMLCN= mlcname(ii)
                    else
                      TOSMLCN= mlcname(ii)
                    endif
                    iissmlci=ii
                  elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition if db does not have a linked MLC - inform the user.
                    write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &                ' has a nonsymmetric construction ',
     &                SSMLCN(ic)(1:lnblnk(SSMLCN(ic))),'.'
                    call edisp(iuout,outs)
                    write(outs,'(5a)') 'It faces ',
     &                SSNAME(icoth),' composed of ',
     &                SSMLCN(icoth)(1:12),' (which may not match).'
                    call edisp(iuout,outs)
                    call edisp(iuout,
     &  'Please check attributes and/or revise the Cconstructions db.')
                    TOSMLCN= mlcsymetric(ii)
                    iissmlci=matsymindex(ii)  ! use returned value 
                  else
                    if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      TOSMLCN= mlcsymetric(ii)
                    elseif(SSMLCN(icoth)(1:12).eq.
     &                     mlcsymetric(ii)(1:12))then
                      TOSMLCN= mlcsymetric(ii)
                    else
                      TOSMLCN= mlcsymetric(ii)
                    endif
                    iissmlci=matsymindex(ii)  ! use returned value
                  endif
                else
                  iissmlci=0  ! mark as unknown
                  TOSMLCN='UNKNOWN'
                endif
                icoth=ICO  ! remember the connection for the other surface

C At this point the other zone surface attributes are known. They
C be written out with code at ~line 3817 in the if(updoth)then section.
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
 21       continue

C Ask for the standard monthly profile index and if a valid response
C set the common blocks. If out of range warn user. If user cancels
C the action make no changes and jump back to 13 (the menu creation code).
          iic2=ic2(IC)
  777     CALL EASKI(IIC2,' ','Standard ground profile number?',
     &      1,'F',mgrdp,'F',1,'ground profile',IERI,nbhelp)
          IF(IIC2.GT.0)THEN
            if(ieri.eq.0)then
              IC2(IC)=iic2
              IE2(IC)=0
              SOTHER(IS,1)='GROUND'
              write(SOTHER(IS,2),'(i2.2)')iic2
              SOTHER(IS,3)='0'
              SSOTHER(IC,1)='GROUND'
              write(SSOTHER(IC,2),'(i2.2)')iic2
              SSOTHER(IC,3)='0'
              ICT(IC)=4
              showother=.false.
            elseif(ieri.eq.-3)then
              goto 13
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
 22         continue

C Ask for the user defined monthly profile index and if a valid response
C set the common blocks. If out of range warn user. If user cancels
C the action make no changes and jump back to 13 (the menu creation code).
 778        CALL EASKI(IIE2,' ','User defined ground profile number?',
     &        0,'F',9,'F',1,'user defined profile',IERI,nbhelp)
            if(IIE2.gt.0)then
              if(ieri.eq.0)then
                IC2(IC)=0
                IE2(IC)=iie2
                SOTHER(IS,1)='GROUND'
                SOTHER(IS,2)='0'
                write(SOTHER(IS,3),'(i2.2)')iie2
                SSOTHER(IC,1)='GROUND'
                SSOTHER(IC,2)='0'
                write(SSOTHER(IC,3),'(i2.2)')iie2
                ICT(IC)=4
              elseif(ieri.eq.-3)then
                goto 13
              else
                goto 778
              endif
            else
              call usrmsg(
     &          'User defined ground profile out of range. Retry',
     &          'or go to `Model Context` menu to define.','W')
              goto 778
            endif
          else
           call usrmsg(
     &        'No user defined ground profiles have been found.',
     &        'Go to `Model Context` menu to define.','W')
          endif
          updoth=.false.
        ELSEIF(IVAL(1).EQ.8)THEN

C Link to a 3D conduction model.
          SOTHER(IS,1)='GROUND'; SOTHER(IS,2)='-3'; SOTHER(IS,3)='0'
          SSOTHER(IC,1)='GROUND'; SSOTHER(IC,2)='-3'; SSOTHER(IC,3)='0'
          ICT(IC)=4; IC2(IC)=-3; IE2(IC)=0
          call edisp(iuout,'Link set to 3D ground conduction model.')
          showother=.false.
          updoth=.false.
        ELSEIF(IVAL(1).EQ.9)THEN

C Set at adiabatic.
          SOTHER(IS,1)='ADIABATIC'; SOTHER(IS,2)='0'; SOTHER(IS,3)='0'
          SSOTHER(IC,1)='ADIABATIC'; SSOTHER(IC,2)='0'
          SSOTHER(IC,3)='0'
          ICT(IC)=5; IE2(IC)=0; IC2(IC)=0
          showother=.false.
          updoth=.false.
        ELSEIF(IVAL(1).EQ.10)THEN

C Get BASESIMP configuration via call to bsimtype.
          IBS=IC2(IC)
          call bsimtype(ibs)

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration. If the user cancels the
C action do not instanciate the data.
          Ifrac=IE2(IC)
          CALL EASKI(Ifrac,' ','% BASESIMP loss to this surface?',
     &        0,'F',100,'F',100,'BASESIMP heat loss percent',
     &        IERI,nbhelp)
          if(ieri.eq.0)then
            IE2(IC)=Ifrac
            IC2(IC)=IBS
            SOTHER(IS,1)='BASESIMP'
            write(SOTHER(IS,2),'(i2.2)')Ifrac
            write(SOTHER(IS,3),'(i2.2)')IBS
            SSOTHER(IC,1)='BASESIMP'
            write(SSOTHER(IC,2),'(i2.2)')Ifrac
            write(SSOTHER(IC,3),'(i2.2)')IBS
            ICT(IC)=6
            showother=.false.
          elseif(ieri.eq.-3)then
            INODA=-4
            goto 13
          endif
          updoth=.false.

        ELSEIF(IVAL(1).EQ.11)THEN
          SOTHER(IS,1)='IDENT_CEN'; SOTHER(IS,2)='0'; SOTHER(IS,3)='0'
          SSOTHER(IC,1)='IDENT_CEN'; SSOTHER(IC,2)='0'
          SSOTHER(IC,3)='0'
          ICT(IC)=7
          VALT=0.0
          VALW=0.0
          IC2(IC)=INT(VALT)
          IE2(IC)=INT(VALW)
          showother=.false.
          updoth=.false.

        ELSEIF(IVAL(1).EQ.12)THEN

C Reset to zeros set ICT to -1 to signal UNKNOWN
          updoth=.false.
          SOTHER(IS,1)='UNKNOWN'; SOTHER(IS,2)='0'; SOTHER(IS,3)='0'
          SSOTHER(IC,1)='UNKNOWN'; SSOTHER(IC,2)='0'
          SSOTHER(IC,3)='0'
          ICT(IC)=-1; IE2(IC)=0; IC2(IC)=0
          showother=.false.

        ELSEIF(IVAL(1).EQ.13)THEN

C Scan for neighbor surfaces in all zones. Report if within 1m 
C and 0.5m2 area difference.
          updoth=.false.
          cog(1)=ssurcog(ic,1)
          cog(2)=ssurcog(ic,2)
          cog(3)=ssurcog(ic,3)
          ifound=0; ifoundc=0
          lnsb=lnblnk(ssname(ic))
          write(outs,'(3a,f9.3,a,f7.2,a,f7.2,a)') 'For surface ',
     &      ssname(ic)(1:lnsb),' (area=',ssna(ic),' azimuth=',
     &      sspazi(ic),' elevation=',sspelv(ic),'):'
          call edisp(iuout,outs)
          do 78 ij=1,ncon
            closecog=.false.
            if(ic1(ij).ne.icomp)then
              cogo(1)=ssurcog(ij,1)
              cogo(2)=ssurcog(ij,2)
              cogo(3)=ssurcog(ij,3)
              call eclose3(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3),1.5,closecog)
              vdist= crowxyz(cog(1),cog(2),cog(3),cogo(1),cogo(2),
     &          cogo(3))
              if(closecog)then
                CALL ECLOSE(SSNA(ij),ssna(ic),0.5,closea)
                if(closea)then
                  lnsa=lnblnk(ssname(ij))
                  write(outs,'(4a,i4,a,f6.2,a,f6.2,a,f6.2,a,f4.2,a)')
     &              ssname(ij)(1:lnsa),' in ',
     &              zname(ic1(ij))(1:lnzname(ic1(ij))),
     &              ' conn index ',ij,
     &              ' area=',SSNA(ij),' azim=',sspazi(ij),
     &              ' elev=',sspelv(ij),' @ dist ',vdist,
     &              ' match?'
                  call edisp(iuout,outs)
                  ifound=ifound+1   ! increment number neighbors found
                  if(ifound.eq.1) ifoundc=ij        ! remember first one
                endif
              endif
            endif
  78      continue
          if(ifound.ge.1)then    ! at least one possible match was found
            if(ifound.eq.1)then
              IIC3=ifoundc    ! if only one set index
            else
              IIC3=0
              CALL EASKI(IIC3,' ',
     &          'Connection number of matching surface (zero=skip)?',
     &          0,'F',ncon,'F',1,'connection index',IERI,nbhelp)
            endif
            if(IIC3.gt.0)then
              write(outs,'(3a)') 'Make thermophysical connection with ',
     &        ssname(IIC3),'?'
              INVT=0
              CALL EASKABC(outs,'  ','yes',
     &          'yes and update other side','cancel',INVT,nbhelp)
              if(INVT.eq.1)then
                iz=ic1(iic3); iso=ie1(iic3) ! decode other zone and surf
                SSOTHER(ic,1)='ANOTHER'
                write(SSOTHER(ic,2),'(i2.2)') iz
                write(SSOTHER(ic,3),'(i2.2)') iso
                ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO
                updoth=.false.
              elseif(INVT.eq.2)then
                iz=ic1(iic3); iso=ie1(iic3) ! decode other zone and surf
                ico=iic3
                SSOTHER(ic,1)='ANOTHER'
                write(SSOTHER(ic,2),'(i2.2)') iz
                write(SSOTHER(ic,3),'(i2.2)') iso
                ICT(IC)=3; IC2(IC)=IZ; IE2(IC)=ISO
                IC1(ICO)=IZ; IE1(ICO)=ISO; ICT(ICO)=3
                IC2(ICO)=IC1(IC); IE2(ICO)=IE1(IC)

                SSOTHER(ico,1)='ANOTHER'
                write(SSOTHER(ICO,2),'(i2.2)') IC2(ICO)
                write(SSOTHER(ICO,3),'(i2.2)') IE2(ICO)

C Update information needed to display information on the other side
C of this surface (i.e. if we went from unknown to a partition).
                icoth=ICO
                ii=ssmlcindex(ic)

C Also check the other side construction and see if it matches. If
C current surface MLC index is known get isymindex for the other side.
                if(ii.gt.0)then
                  lnmlcn=lnblnk(mlcname(ii))
                  lnssmlc=lnblnk(SSMLCN(icoth))
                  if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                    if(SSMLCN(icoth)(1:lnssmlc).eq.
     &                 mlcname(ii)(1:lnmlcn))then
                      TOSMLCN= mlcname(ii)
                    elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      TOSMLCN= mlcname(ii)
                    else
                      TOSMLCN= mlcname(ii)
                    endif
                    iissmlci=ii
                  elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition if db does not have a linked MLC - inform the user.
                    write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &                ' has a nonsymmetric construction ',
     &                SSMLCN(ic)(1:lnblnk(SSMLCN(ic))),'.'
                    call edisp(iuout,outs)
                    write(outs,'(5a)') 'It faces ',
     &                SSNAME(icoth),' composed of ',
     &                SSMLCN(icoth)(1:12),' (which may not match).'
                    call edisp(iuout,outs)
                    call edisp(iuout,
     &  'Please check attributes and/or revise common constructions.')
                    TOSMLCN= SSMLCN(icoth)
                    iissmlci=ssmlcindex(icoth)  ! leave it alone
                  else

C We have a non-symmetric MLC which does point to a reversed version
C so check to see if the name of the other MLC matches mlcsymetric.
                    ii=ssmlcindex(ic)
                    if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      TOSMLCN= mlcsymetric(ii)
                    elseif(SSMLCN(icoth)(1:12).eq.
     &                     mlcsymetric(ii)(1:12))then
                      TOSMLCN= mlcsymetric(ii)
                    else
                      TOSMLCN= mlcsymetric(ii)
                    endif
                    iissmlci=matsymindex(ii)  ! use returned value
                  endif

C At this point the other zone surface attributes are known.
                  icoth=ICO  ! remember the connection for the other surface
                  showother=.true.
                  updoth=.true.
                else
                  iissmlci=0  ! set to ignore MLC
                  TOSMLCN='UNKNOWN'
                  icoth=ICO  ! remember the connection for the other surface
                  showother=.true.
                  updoth=.true.
                endif
              elseif(INVT.eq.3)then
                goto 13    ! user canceled so jump back
              endif
            else
              goto 13  ! user declined
            endif
          else
            goto 13   ! nothing was found go back to the main menu
          endif

        ENDIF
        MODGEO=.TRUE.
        write(6,*) 'tosmlcn is ',iissmlci,' ',tosmlcn

C Finally update the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif

C If user requested other side to be updated do this as well. Read
C in the zone pointed to by IC2, update SOTHER (of index IE2 in the
C other zone) and write out that zones geometry.
          if(updoth)then
            CALL ESCZONE(ICOMP)
            lnicoth=lnblnk(SSMLCN(icoth))
            write(outs,'(6a)')'Updating `other side` composition of ',
     &        SN(1:lnblnk(SN)),' from ',SSMLCN(icoth)(1:lnicoth),
     &        ' > ',TOSMLCN
            call edisp(iuout,outs)
            call eclose(gversion(IC2(IC)),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IFIL+2,LGEOM(IC2(IC)),IC2(IC),0,iuout,IER)
            else
              call egomin(IFIL+2,LGEOM(IC2(IC)),IC2(IC),0,0,iuout,IER)
            endif

C Assign new MLC name and connexion info to the icoth surface.
            SSMLCN(icoth)=TOSMLCN
            if(iissmlci.ne.0) ssmlcindex(icoth)=iissmlci  ! update if known
            SSOTHER(icoth,1)='ANOTHER'
            write(SSOTHER(icoth,2),'(i2.2)') IC2(icoth)
            write(SSOTHER(icoth,3),'(i2.2)') IE2(icoth)

            call eclose(gversion(IC2(IC)),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(IC2(IC)) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IFIL+2,LGEOM(IC2(IC)),IC2(IC),iuout,3,IER)
            else
              call emkgeo(IFIL+2,LGEOM(IC2(IC)),IC2(IC),3,IER)
            endif
            IF(IER.NE.0)CALL USRMSG(' ',
     &           'Problem updating other surface attribute.','W')

C Read current zone back in and then update the configuration file.
            CALL ERCZONE(ICOMP)
            call usrmsg(' ',' ','-')
            updoth=.false.
          endif
          CALL EMKCFG('-',IER)
        endif

      ELSEIF(INODA.EQ.10)THEN

C Version 1.1 geometry holds information on child pararent
C relationship. What is shown here is result of geometric
C scan and mentions if there are children and the name of
C the first child.
C << manual editing to be done >>

      ELSEIF(INODA.EQ.11)THEN

C Version 1.1 geometry holds information on child pararent
C relationship. What is shown here is result of geometric
C scan and mentions the name of the parent surface.
C << manual editing to be done >>

      ELSEIF(INODA.EQ.12)THEN

C Version 1.1 geometry holds SEMANTIC tags for surfaces.
C << perhaps this will need to be called from several
C << locations in the code (when adding a surface) so
C << this code could be in a separate subroutine?

        if(newgeo)then
          guesstype='any_type'
          iz=icomp
          isur=is
          icon=ic
          call edituse(iz,isur,icon,guesstype)
        endif
      ELSEIF(INODA.EQ.13)THEN

C Usage subtype is handled by 11th menu item.
        continue
      ELSE
        INODA=-4
        goto 13
      ENDIF
      INODA=-4
      goto 13

      END 

C ************* EDITUSE
C Edituse sets the value of SSUSE() and SUSE based on context.
C   integer iz is the current zone
C   integer isur is the current surface
C   integer icon is the current connection
C   guesstype (char 24) provides the context so that a likely
C     subset of choices can be presented.
      subroutine edituse(iz,isur,icon,guesstype)
#include "building.h"
#include "geometry.h"
#include "sbem.h"
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER outs*124

      character guesstype*24  ! for context of surface addition
      dimension iuseopt(32)
      character*32 useopt(23)
      integer IRT  ! for radio button

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='edituse'  ! set for subroutine

C Set local working variables.
      is=isur
      ic=icon

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
     &  ssname(ic),' is ',SSUSE(ic,1),' and ',SSUSE(ic,2)
      call edisp(iuout,outs)
      
      write(outs,'(3a)') 'The use/semantic options for surface ',
     &  ssname(ic),' are...'
      call edisp(iuout,outs)

C Help for the various choices.
      helptopic='surface_use_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(guesstype(1:8).eq.'any_type'.or.guesstype(1:2).eq.'- ')then

C Setup array of menu strings.
        useopt(1) ='DOOR NORMAL (not code complient)'
        useopt(2) ='DOOR PERSONEL (code complient)  '
        useopt(3) ='DOOR ENTRANCE (high useage)     '
        useopt(4) ='DOOR VEHICLE (large access)     '
        useopt(5) ='FICTitious (not code complient) '
        useopt(6) ='FLOOR EXTGRND (extr or ground)  '
        useopt(7) ='FLOOR OTHER (not code complient)'
        useopt(8) ='FRAME FACADE (code complient)   '
        useopt(9) ='FRAME OTHER (not code complient)'
        useopt(10)='FURNI - (mass within zone)      '
        useopt(11)='GRILL SOURCE (conduit outlet)   '
        useopt(12)='GRILL EXTRACT (conduit inlet)   '
        useopt(13)='PARTN - (inside wall/ceil/floor)'
        useopt(14)='ROOF PITCHED (>20deg slope)     '
        useopt(15)='ROOF FLAT (0deg-20deg slope)    '
        useopt(16)='SMOKEVNT (in roof or wall)      '
        useopt(17)='STRUCture - (mass within zone)  '
        useopt(18)='WALL - (part of the facade)     '
        useopt(19)='WINDOW  (facade code complient) '
        useopt(20)='WINDOW DISPLAY (code complient) '
        useopt(21)='WINDOW skylight (code complient)'
        useopt(22)='WINDOW OTHER (not code complnt) '
        useopt(23)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,23,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=1
        elseif(IOS.eq.2)then
          iuopt=2
        elseif(IOS.eq.3)then
          iuopt=3
        elseif(IOS.eq.4)then
          iuopt=4
        elseif(IOS.eq.5)then
          iuopt=5
        elseif(IOS.eq.6)then
          iuopt=6
        elseif(IOS.eq.7)then
          iuopt=7
        elseif(IOS.eq.8)then
          iuopt=8
        elseif(IOS.eq.9)then
          iuopt=9
        elseif(IOS.eq.10)then
          iuopt=10
        elseif(IOS.eq.11)then
          iuopt=11
        elseif(IOS.eq.12)then
          iuopt=12
        elseif(IOS.eq.13)then
          iuopt=13
        elseif(IOS.eq.14)then
          iuopt=14
        elseif(IOS.eq.15)then
          iuopt=15
        elseif(IOS.eq.16)then
          iuopt=16
        elseif(IOS.eq.17)then
          iuopt=17
        elseif(IOS.eq.18)then
          iuopt=18
        elseif(IOS.eq.19)then
          iuopt=19
        elseif(IOS.eq.20)then
          iuopt=20
        elseif(IOS.eq.21)then
          iuopt=21
        elseif(IOS.eq.22)then
          iuopt=22
        elseif(IOS.eq.23)then
          iuopt=23
        endif

      elseif(guesstype(1:8).eq.'notional')then

C Setup array of menu strings.
        useopt(1) ='DOOR PERSONEL'
        useopt(2) ='DOOR ENTRANCE'
        useopt(3) ='DOOR VEHICLE'
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
        if(IOS.eq.1)then
          iuopt=2
        elseif(IOS.eq.2)then
          iuopt=3
        elseif(IOS.eq.3)then
          iuopt=4
        elseif(IOS.eq.4)then
          iuopt=14
        elseif(IOS.eq.5)then
          iuopt=15
        elseif(IOS.eq.6)then
          iuopt=18
        elseif(IOS.eq.7)then
          iuopt=19
        elseif(IOS.eq.8)then
          iuopt=20
        elseif(IOS.eq.9)then
          iuopt=21
        elseif(IOS.eq.10)then
          iuopt=8
        elseif(IOS.eq.11)then
          iuopt=6
        elseif(IOS.eq.12)then
          iuopt=23
        endif

      elseif(guesstype(1:18).eq.'window_grill_frame')then

C Setup array of menu strings.
        useopt(1) ='FICTitious (not code complient) '
        useopt(2) ='FRAME FACADE (code complient)   '
        useopt(3) ='FRAME OTHER (not code complient)'
        useopt(4)='GRILL SOURCE (conduit outlet)   '
        useopt(5)='GRILL EXTRACT (conduit inlet)   '
        useopt(6)='WINDOW  (facade code complient) '
        useopt(7)='WINDOW DISPLAY (code complient) '
        useopt(8)='WINDOW skylight (code complient)'
        useopt(9)='WINDOW OTHER (not code complnt) '
        useopt(10)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,10,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=5
        elseif(IOS.eq.2)then
          iuopt=8
        elseif(IOS.eq.3)then
          iuopt=9
        elseif(IOS.eq.4)then
          iuopt=11
        elseif(IOS.eq.5)then
          iuopt=12
        elseif(IOS.eq.6)then
          iuopt=19
        elseif(IOS.eq.7)then
          iuopt=20
        elseif(IOS.eq.8)then
          iuopt=21
        elseif(IOS.eq.9)then
          iuopt=22
        elseif(IOS.eq.10)then
          iuopt=23
        endif

      elseif(guesstype(1:16).eq.'door_grill_frame')then

C Setup array of menu strings.
        useopt(1) ='DOOR NORMAL (not code complient)'
        useopt(2) ='DOOR PERSONEL (code complient)  '
        useopt(3) ='DOOR ENTRANCE (high useage)     '
        useopt(4) ='DOOR VEHICLE (large access)     '
        useopt(5) ='FICTitious (not code complient) '
        useopt(6) ='FRAME FACADE (code complient)   '
        useopt(7) ='FRAME OTHER (not code complient)'
        useopt(8) ='GRILL SOURCE (conduit outlet)   '
        useopt(9) ='GRILL EXTRACT (conduit inlet)   '
        useopt(10)='WINDOW  (facade code complient) '
        useopt(11)='WINDOW DISPLAY (code complient) '
        useopt(12)='WINDOW skylight (code complient)'
        useopt(13)='WINDOW OTHER (not code complnt) '
        useopt(14)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,14,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=1
        elseif(IOS.eq.2)then
          iuopt=2
        elseif(IOS.eq.3)then
          iuopt=3
        elseif(IOS.eq.4)then
          iuopt=4
        elseif(IOS.eq.5)then
          iuopt=5
        elseif(IOS.eq.6)then
          iuopt=8
        elseif(IOS.eq.7)then
          iuopt=9
        elseif(IOS.eq.8)then
          iuopt=11
        elseif(IOS.eq.9)then
          iuopt=12
        elseif(IOS.eq.10)then
          iuopt=19
        elseif(IOS.eq.11)then
          iuopt=20
        elseif(IOS.eq.12)then
          iuopt=21
        elseif(IOS.eq.13)then
          iuopt=22
        elseif(IOS.eq.14)then
          iuopt=23
        endif

      elseif(guesstype(1:8).eq.'vertical')then

        useopt(1) ='DOOR NORMAL (not code complient)'
        useopt(2) ='DOOR PERSONEL (code complient)  '
        useopt(3) ='DOOR ENTRANCE (high useage)     '
        useopt(4) ='DOOR VEHICLE (large access)     '
        useopt(5) ='FICTitious (not code complient) '
        useopt(6)='FURNI - (mass within zone)      '
        useopt(7)='PARTN - (inside wall/ceil/floor)'
        useopt(8)='STRUCture - (mass within zone)  '
        useopt(9)='WALL - (part of the facade)     '
        useopt(10)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,10,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=1
        elseif(IOS.eq.2)then
          iuopt=2
        elseif(IOS.eq.3)then
          iuopt=3
        elseif(IOS.eq.4)then
          iuopt=4
        elseif(IOS.eq.5)then
          iuopt=5
        elseif(IOS.eq.6)then
          iuopt=10
        elseif(IOS.eq.7)then
          iuopt=13
        elseif(IOS.eq.8)then
          iuopt=17
        elseif(IOS.eq.9)then
          iuopt=18
        elseif(IOS.eq.10)then
          iuopt=23
        endif

      elseif(guesstype(1:10).eq.'horizontal')then

        useopt(1) ='FICTitious (not code complient) '
        useopt(2) ='FLOOR EXTGRND (extr or ground)  '
        useopt(3) ='FLOOR OTHER (not code complient)'
        useopt(4) ='FURNI - (mass within zone)      '
        useopt(5)='PARTN - (inside wall/ceil/floor)'
        useopt(6)='STRUCture - (mass within zone)  '
        useopt(7)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,7,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=5
        elseif(IOS.eq.2)then
          iuopt=6
        elseif(IOS.eq.3)then
          iuopt=7
        elseif(IOS.eq.4)then
          iuopt=10
        elseif(IOS.eq.5)then
          iuopt=13
        elseif(IOS.eq.6)then
          iuopt=17
        elseif(IOS.eq.7)then
          iuopt=23
        endif

      endif

C Common editing of variants - doors:
      if(iuopt.eq.1)then
        SUSE(IS,1)='DOOR'
        SSUSE(IC,1)='DOOR'
      elseif(iuopt.eq.2)then
        SUSE(IS,1)='P-DOOR'
        SSUSE(IC,1)='P-DOOR'
      elseif(iuopt.eq.3)then
        SUSE(IS,1)='H-DOOR'
        SSUSE(IC,1)='H-DOOR'
      elseif(iuopt.eq.4)then
        SUSE(IS,1)='V-DOOR'
        SSUSE(IC,1)='V-DOOR'
      endif

C Do not need second tag if generating notional model
      if(guesstype(1:8).eq.'notional')then
        SUSE(IS,2)='-'
        SSUSE(IC,2)='-'
        goto 55
      endif
      if(iuopt.eq.1.or.iuopt.eq.2.or.iuopt.eq.3.or.iuopt.eq.4)then
        helptopic='door_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','DOOR air leakage:',
     &    'closed','undercut','open','bidirectional',
     &    'cancel',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(IS,2)='CLOSED'
          SSUSE(IC,2)='CLOSED'
        elseif(IRT.eq.2)then
          SUSE(IS,2)='UNDERCUT'
          SSUSE(IC,2)='UNDERCUT'
        elseif(IRT.eq.3)then
          SUSE(IS,2)='OPEN'
          SSUSE(IC,2)='OPEN'
        elseif(IRT.eq.4)then
          SUSE(IS,2)='BIDIR'
          SSUSE(IC,2)='BIDIR'
        elseif(IRT.eq.5)then
          continue
        endif
      endif
 55   if(iuopt.eq.5)then
        SUSE(IS,1)='FICT'
        SSUSE(IC,1)='FICT'

        helptopic='fict_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','FICT surface air leakage:',
     &    'closed','crack','open','bidirectional',
     &    'cancel',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(IS,2)='CLOSED'
          SSUSE(IC,2)='CLOSED'
        elseif(IRT.eq.2)then
          SUSE(IS,2)='CRACK'
          SSUSE(IC,2)='CRACK'
        elseif(IRT.eq.3)then
          SUSE(IS,2)='OPEN'
          SSUSE(IC,2)='OPEN'
        elseif(IRT.eq.4)then
          SUSE(IS,2)='BIDIR'
          SSUSE(IC,2)='BIDIR'
        elseif(IRT.eq.5)then
          continue
        endif
      endif

      if(iuopt.eq.6)then
        SUSE(IS,1)='FLOOR'
        SSUSE(IC,1)='FLOOR'
        SUSE(IS,2)='EXTGRND'
        SSUSE(IC,2)='EXTGRND'
      elseif(iuopt.eq.7)then
        SUSE(IS,1)='FLOOR'
        SSUSE(IC,1)='FLOOR'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      endif

      if(iuopt.eq.8)then
        SUSE(IS,1)='F-FRAME'
        SSUSE(IC,1)='F-FRAME'
      elseif(iuopt.eq.9)then
        SUSE(IS,1)='FRAME'
        SSUSE(IC,1)='FRAME'
      endif
      if(guesstype(1:8).eq.'notional')then
        SUSE(IS,2)='-'
        SSUSE(IC,2)='-'
        goto 57
      endif
      if(iuopt.eq.8.or.iuopt.eq.9)then
        helptopic='frame_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','FRAME surface air leakage:',
     &    'closed','crack','vent','cancel',
     &    ' ',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(IS,2)='CLOSED'
          SSUSE(IC,2)='CLOSED'
        elseif(IRT.eq.2)then
          SUSE(IS,2)='CRACK'
          SSUSE(IC,2)='CRACK'
        elseif(IRT.eq.3)then
          SUSE(IS,2)='OPEN'
          SSUSE(IC,2)='OPEN'
        elseif(IRT.eq.4)then
          continue
        endif
      endif

  57  if(iuopt.eq.10)then
        SUSE(IS,1)='FURNI'
        SSUSE(IC,1)='FURNI'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      elseif(iuopt.eq.11)then
        SUSE(IS,1)='GRILL'
        SSUSE(IC,1)='GRILL'
        SUSE(IS,2)='SOURCE'
        SSUSE(IC,2)='SOURCE'
      elseif(iuopt.eq.12)then
        SUSE(IS,1)='GRILL'
        SSUSE(IC,1)='GRILL'
        SUSE(IS,2)='EXTRACT'
        SSUSE(IC,2)='EXTRACT'
      elseif(iuopt.eq.13)then
        SUSE(IS,1)='PARTN'
        SSUSE(IC,1)='PARTN'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      elseif(iuopt.eq.14)then
        SUSE(IS,1)='ROOF'
        SSUSE(IC,1)='ROOF'
        SUSE(IS,2)='PITCHED'
        SSUSE(IC,2)='PITCHED'
      elseif(iuopt.eq.15)then
        SUSE(IS,1)='ROOF'
        SSUSE(IC,1)='ROOF'
        SUSE(IS,2)='FLAT'
        SSUSE(IC,2)='FLAT'
      elseif(iuopt.eq.16)then
        SUSE(IS,1)='SMOKEVNT'
        SSUSE(IC,1)='SMOKEVNT'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      elseif(iuopt.eq.17)then
        SUSE(IS,1)='STRUC'
        SSUSE(IC,1)='STRUC'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      elseif(iuopt.eq.18)then
        SUSE(IS,1)='WALL'
        SSUSE(IC,1)='WALL'
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      endif

      if(iuopt.eq.19)then
        SUSE(IS,1)='C-WINDOW'
        SSUSE(IC,1)='C-WINDOW'
      elseif(iuopt.eq.20)then
        SUSE(IS,1)='D-WINDOW'
        SSUSE(IC,1)='D-WINDOW'
      elseif(iuopt.eq.21)then
        SUSE(IS,1)='S-WINDOW'
        SSUSE(IC,1)='S-WINDOW'
      elseif(iuopt.eq.22)then
        SUSE(IS,1)='WINDOW'
        SSUSE(IC,1)='WINDOW'
      elseif(iuopt.eq.23)then
        SUSE(IS,1)='-'
        SSUSE(IC,1)='-'
      endif

C Do not need second tag if generating notional model
      if(guesstype(1:8).eq.'notional')then
        SUSE(IS,2)='-'
        SSUSE(IC,2)='-'
        goto 56
      endif
      if(iuopt.eq.19.or.iuopt.eq.20.or.iuopt.eq.21.or.iuopt.eq.22)then
        helptopic='window_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','WINDOW surface air leakage:',
     &    'closed','crack','open','sash ','bidirectional',
     &    'cancel',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          SUSE(IS,2)='CLOSED'
          SSUSE(IC,2)='CLOSED'
        elseif(IRT.eq.2)then
          SUSE(IS,2)='CRACK'
          SSUSE(IC,2)='CRACK'
        elseif(IRT.eq.3)then
          SUSE(IS,2)='OPEN'
          SSUSE(IC,2)='OPEN'
        elseif(IRT.eq.4)then
          SUSE(IS,2)='SASH'
          SSUSE(IC,2)='SASH'
        elseif(IRT.eq.5)then
          SUSE(IS,2)='BIDIR'
          SSUSE(IC,2)='BIDIR'
        elseif(IRT.eq.6)then
          continue
        endif
      endif
56    if(iuopt.eq.23)then
        SUSE(IS,1)='- '
        SSUSE(IC,1)='- '
        SUSE(IS,2)='- '
        SSUSE(IC,2)='- '
      endif

      return
      end


C ************* SELECTUSE
C Sekectuse gets two strings for surface use based on context.
C   guesstype (char 24) provides the context so that a likely
C     subset of choices can be presented.
      subroutine selectuse(guesstype,use1,use2)
#include "building.h"
#include "sbem.h"

C Parameters
      character guesstype*24  ! for context of surface uses
      character use1*8        ! general use type
      character use2*8        ! details of use

      COMMON/OUTIN/IUOUT,IUIN

      dimension iuseopt(32)
      character*32 useopt(23)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='selectuse'  ! set for subroutine

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
        useopt(1) ='DOOR NORMAL (not code complient)'
        useopt(2) ='DOOR PERSONEL (code complient)  '
        useopt(3) ='DOOR ENTRANCE (high useage)     '
        useopt(4) ='DOOR VEHICLE (large access)     '
        useopt(5) ='FICTitious (not code complient) '
        useopt(6) ='FLOOR EXTGRND (extr or ground)  '
        useopt(7) ='FLOOR OTHER (not code complient)'
        useopt(8) ='FRAME FACADE (code complient)   '
        useopt(9) ='FRAME OTHER (not code complient)'
        useopt(10)='FURNI - (mass within zone)      '
        useopt(11)='GRILL SOURCE (conduit outlet)   '
        useopt(12)='GRILL EXTRACT (conduit inlet)   '
        useopt(13)='PARTN - (inside wall/ceil/floor)'
        useopt(14)='ROOF PITCHED (>20deg slope)     '
        useopt(15)='ROOF FLAT (0deg-20deg slope)    '
        useopt(16)='SMOKEVNT (in roof or wall)      '
        useopt(17)='STRUCture - (mass within zone)  '
        useopt(18)='WALL - (part of the facade)     '
        useopt(19)='WINDOW  (facade code complient) '
        useopt(20)='WINDOW DISPLAY (code complient) '
        useopt(21)='WINDOW skylight (code complient)'
        useopt(22)='WINDOW OTHER (not code complnt) '
        useopt(23)='-   -  (somthing else)          '

        IX=1
        CALL EPICKS(IX,iuseopt,' ','Surface USAGE attributes:',
     &         32,23,useopt,'surface USAGE options',IER,nbhelp)
        IOS=iuseopt(1)
        if(IOS.eq.0) return

        if(IOS.eq.1)then
          iuopt=1
        elseif(IOS.eq.2)then
          iuopt=2
        elseif(IOS.eq.3)then
          iuopt=3
        elseif(IOS.eq.4)then
          iuopt=4
        elseif(IOS.eq.5)then
          iuopt=5
        elseif(IOS.eq.6)then
          iuopt=6
        elseif(IOS.eq.7)then
          iuopt=7
        elseif(IOS.eq.8)then
          iuopt=8
        elseif(IOS.eq.9)then
          iuopt=9
        elseif(IOS.eq.10)then
          iuopt=10
        elseif(IOS.eq.11)then
          iuopt=11
        elseif(IOS.eq.12)then
          iuopt=12
        elseif(IOS.eq.13)then
          iuopt=13
        elseif(IOS.eq.14)then
          iuopt=14
        elseif(IOS.eq.15)then
          iuopt=15
        elseif(IOS.eq.16)then
          iuopt=16
        elseif(IOS.eq.17)then
          iuopt=17
        elseif(IOS.eq.18)then
          iuopt=18
        elseif(IOS.eq.19)then
          iuopt=19
        elseif(IOS.eq.20)then
          iuopt=20
        elseif(IOS.eq.21)then
          iuopt=21
        elseif(IOS.eq.22)then
          iuopt=22
        elseif(IOS.eq.23)then
          iuopt=23
        endif

      elseif(guesstype(1:8).eq.'notional')then

C Setup array of menu strings.
        useopt(1) ='DOOR PERSONEL'
        useopt(2) ='DOOR ENTRANCE'
        useopt(3) ='DOOR VEHICLE'
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
        if(IOS.eq.1)then
          iuopt=2
        elseif(IOS.eq.2)then
          iuopt=3
        elseif(IOS.eq.3)then
          iuopt=4
        elseif(IOS.eq.4)then
          iuopt=14
        elseif(IOS.eq.5)then
          iuopt=15
        elseif(IOS.eq.6)then
          iuopt=18
        elseif(IOS.eq.7)then
          iuopt=19
        elseif(IOS.eq.8)then
          iuopt=20
        elseif(IOS.eq.9)then
          iuopt=21
        elseif(IOS.eq.10)then
          iuopt=8
        elseif(IOS.eq.11)then
          iuopt=23
        endif

      endif

C Edit door variants.
      if(iuopt.eq.1)then
        USE1='DOOR'
      elseif(iuopt.eq.2)then
        USE1='P-DOOR'
      elseif(iuopt.eq.3)then
        USE1='H-DOOR'
      elseif(iuopt.eq.4)then
        USE1='V-DOOR'
      endif

C Do not need second tag if generating notional model
      if(guesstype(1:8).eq.'notional')goto 55
      if(iuopt.eq.1.or.iuopt.eq.2.or.iuopt.eq.3.or.iuopt.eq.4)then
        helptopic='door_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','DOOR air leakage:',
     &    'closed','undercut','open','bidirectional',
     &    'cancel',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='UNDERCUT'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='BIDIR'
        elseif(IRT.eq.5)then
          continue
        endif
      endif
 55   if(iuopt.eq.5)then
        USE1='FICT'

        helptopic='fict_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','FICT surface air leakage:',
     &    'closed','crack','open','bidirectional',
     &    'cancel',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='BIDIR'
        elseif(IRT.eq.5)then
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
        CALL EASKATOG(' ','FRAME surface air leakage:',
     &    'closed','crack','vent','cancel',
     &    ' ',' ',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          continue
        endif
      endif

      if(iuopt.eq.10)then
        USE1='FURNI'
        USE2='- '
      elseif(iuopt.eq.11)then
        USE1='GRILL'
        USE2='SOURCE'
      elseif(iuopt.eq.12)then
        USE1='GRILL'
        USE2='EXTRACT'
      elseif(iuopt.eq.13)then
        USE1='PARTN'
        USE2='- '
      elseif(iuopt.eq.14)then
        USE1='ROOF'
        USE2='PITCHED'
      elseif(iuopt.eq.15)then
        USE1='ROOF'
        USE2='FLAT'
      elseif(iuopt.eq.16)then
        USE1='SMOKEVNT'
        USE2='- '
      elseif(iuopt.eq.17)then
        USE1='STRUC'
        USE2='- '
      elseif(iuopt.eq.18)then
        USE1='WALL'
        USE2='- '
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
        USE1='-'
      endif

C Do not need second tag if generating notional model
      if(guesstype(1:8).eq.'notional')goto 56
      if(iuopt.eq.19.or.iuopt.eq.20.or.iuopt.eq.21.or.iuopt.eq.22)then
        helptopic='window_use_open_closed'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IRT=1
        CALL EASKATOG(' ','WINDOW surface air leakage:',
     &    'closed','crack','open','sash ','bidirectional',
     &    'cancel',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          USE2='CLOSED'
        elseif(IRT.eq.2)then
          USE2='CRACK'
        elseif(IRT.eq.3)then
          USE2='OPEN'
        elseif(IRT.eq.4)then
          USE2='SASH'
        elseif(IRT.eq.5)then
          USE2='BIDIR'
        elseif(IRT.eq.6)then
          continue
        endif
      endif
56    if(iuopt.eq.23)then
        USE1='- '
        USE2='- '
      endif

      return
      end


C ************* EDVERT 
C Edit vertex attributes in common block G1 via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDVERT(ITRU,ICOMP,MODGEO,IER)
#include "epara.h"
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      LOGICAL MODIFY,MODLEN,MODBND,MODGEO,ok,dok,match,matchver
      logical withinbox  ! true if point within bounding box.

      DIMENSION VERT(35),IVLST(MTV),ITEMP(MTV),COG1(3),COG2(3),COG3(3)
      DIMENSION VP(3),EQN(4),XX(MV),YY(MV),ZZ(MV)
      dimension IVALS(MCOM)
      DIMENSION ISASSO(MS)
      CHARACTER VERT*33,KEY*1,head*28,HOLD*36
      character outs*124,headv*48
      logical anotherone  ! to signal that another point will be requested
      integer IRT  ! for radio button
      integer MVERT,IVERT ! max items and current menu item
      integer INPICK  ! to use with epkvert
      integer iwhich1,lastv1  ! for 1st and 2nd user selections
      integer iwhich2,lastv2
      integer iwhich3
      integer  iclkok  ! to allow jump to previous or next vertex
      logical greyok,found,closex,closey,closez
      real CX,CY,CZ  ! XYZ of selected verticies
      dimension CX(10),CY(10),CZ(10)
      integer im,k,mnulen   ! multi column lines
      logical odd   ! for display of vertex info

#ifdef OSI
      integer iix,iiy,iicol
#else
      integer*8 iix,iiy,iicol
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDVERT'  ! set for subroutine

C Initial values for vertex at angle facility.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif
      V1=1.0; AZ=0.0; EL=0.0
      x3=0.0; y3=0.0; z3=0.0
      anotherone=.false.
      iwhich=0; iwhich1=0; iwhich2=0
      lastv1=0; lastv2=0

C Write vertices with minimal white space comma separated.
C See if an even or odd number of items in list. Do this
C on first entry to the vertex editing facility.
      im=MOD(NZTV(icomp),2)
      odd=.false.
      if(im.eq.1) odd=.true.
      call edisp(itru,' Vertices ( index X Y Z) in the zone...')
      if(NZTV(icomp).lt.20)then
        DO 960 I = 1,NZTV(icomp)
          WRITE(outs,'(a,i3,3F12.5,a,i3)')'*vertex ',I,
     &      X(I),Y(I),Z(I)
          call edisp(itru,outs)
960     CONTINUE
      else
        MNULEN=(NZTV(icomp)/2)
        DO 193 K=1,MNULEN
          WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') 'vertex ',k,
     &      X(k),Y(k),Z(k),'   vertex ',K+MNULEN,
     &      X(K+MNULEN),Y(K+MNULEN),Z(K+MNULEN)
          call edisp(itru,outs)
  193   CONTINUE

C Put odd vertex in right column.
        if(odd)then
          WRITE(outs,'(46x,a,i3,3f11.5)') 'vertex ',NZTV(icomp),
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
   14     FORMAT(A1,I4,3F9.3)
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
      VERT(M+6)  ='- exit this menu                 '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Help text for this menu.
      helptopic='surface_vertex_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(A,A12,A)')' Vertices in `',zname(ICOMP),'`'
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
     &   'j move vertices along a line','k do nothing',' ',
     &   ino,idno,nbhelp)

C Based on users selection or jump back point for 'do another one'.
 243     if(ino.eq.0.or.ino.eq.11)then
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
              iwhich1=IVLST(1)
              lastv1=iwhich1  ! remember this
              iwhich2=IVLST(2)
              lastv2=iwhich2
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
          if(MMOD.lt.8)then
            continue
          else
            COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            iicol=0
            if(greyok)call winscl('z',iicol)
            call esymbol(iix,iiy,24,1)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 5580.

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
            MODIFY=.true.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
          endif

        elseif(ino.eq.3)then

C Vertex at distance and angle from another (based on current G1 common).
          if(anotherone)then
            inpick=1
            iwhich=lastv1   ! use rememebered 1st vertex
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
          RAD = PI/180.
          RYAZI = AZ*RAD
          RSALT = EL*RAD
          z3 = V1*SIN(RSALT)
          XYDIS = V1*COS(RSALT)
          IF (XYDIS .LT. 1E-6)THEN
            x3 = 0.
            y3 = 0.
          ELSE
            x3 = XYDIS*SIN(RYAZI)
            y3 = XYDIS*COS(RYAZI)
          ENDIF
          write(outs,'(a,3f10.4)') 'Vertex calculated @ X,Y,Z:',
     &      x3+X(IWHICH),y3+Y(IWHICH),z3+Z(IWHICH)
          call edisp(itru,outs)
          x3=x3+X(IWHICH)
          y3=y3+Y(IWHICH)
          z3=z3+Z(IWHICH)

C Show the new point on the current wireframe.
          if(MMOD.lt.8)then
            continue
          else
            COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            iicol=0  ! make another pass to use iicol in winscl
            if(greyok)call winscl('z',iicol)
            call esymbol(iix,iiy,24,1)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
          endif

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 5580.

        elseif(ino.eq.4)then

C Angle between two lines (based on current G1 common block).
          inpick=3
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define Angle',
     &      'Select three vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.3)then
            iwhich1=IVLST(1)
            lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2)
            lastv2=iwhich2
            iwhich3=IVLST(3)
            call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &        ang3)
            write(outs,'(a,3i3,a,f9.4)') ' Angle between verts ',
     &        iwhich1,iwhich2,iwhich3,' is ',ang3
            call edisp(itru,outs)
          endif
          MODIFY=.true.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(IER)

        elseif(ino.eq.5)then

C Intersection line and plane (based on info in current G1 common).

C << add option for picking surface in another zone >>
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Define the line',
     &      'Select two vertices.',' ',nbhelp,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2)
            lastv2=iwhich2
            IS=1
            CALL EPMENSV
            CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &         ' ',IER)
            CALL EPMENRC
            N = NVER(IS)
            DO 150 J = 1,N
              XX(J) = X(JVN(IS,J))
              YY(J) = Y(JVN(IS,J))
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
            if(MMOD.lt.8)then
              continue
            else
              COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif

C Checking if it should be added into the list of vertices or
C inserted in another surface is done in code near line 5580.

          endif

        elseif(ino.eq.6)then

C Intersection LINES and a plane. As for insersection surface.
          CALL EPMENSV
          IS=1
          CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &       ' ',IER)
          N = NVER(IS)
          nbstart=NZTV(icomp)  ! how many vertices did we have to start with
          CALL EPMENRC

C Setup definition of the plane to intersect.
          DO 149 J = 1,N
            XX(J) = X(JVN(IS,J))
            YY(J) = Y(JVN(IS,J))
            ZZ(J) = Z(JVN(IS,J))
  149     CONTINUE
          call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)

C Loop through each of the edge lists for each of the surfaces in
C the zone (except the identified surface). 
          do 135 ivj=1,NSUR
            if(ivj.eq.is) goto 135  ! do not test edges of the plane surface 'is'.
            ivjlimit=NVER(ivj)
            do 147 ivjj=1,ivjlimit
              if(ivjj.eq.ivjlimit)then
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,1)
              else
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,ivjj+1)
              endif
              x3=0.0
              y3=0.0
              z3=0.0
              CALL VECPLN(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &          X(IWHICH2),Y(IWHICH2),Z(IWHICH2),EQN, x3,y3,z3, IERR)
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
                  if(tdis.lt.0.003)then
                    goto 147
                  endif
  138           continue

C Show the new point on the current wireframe.
                if(MMOD.lt.8)then
                  continue
                else
                  COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
                  CALL VECTRN(COG1,TEMAT,COG2,IER)
                  CALL VECTRN(COG2,ETSMAT,COG3,IER)
                  call u2pixel(COG3(1),COG3(2),iix,iiy)
                  iicol=0
                  if(greyok)call winscl('z',iicol)
                  call esymbol(iix,iiy,24,1)
                  iicol=0
                  if(greyok)call winscl('-',iicol)
                  call forceflush()
                endif

C Ask if it should be added.
                if(NTV.lt.MTV)then
                  write(outs,'(a,i3,a)') 'Make vertex ',NTV+1,' a:'
                  invt=0
                  CALL EASKABC(' ',outs,'new vertex',
     &              'new vertex after editing','cancel',INVT,nbhelp)
                  if(INVT.eq.1)then
                    NTV=NTV+1
                    NZTV(icomp)=NTV
                    x(ntv)=x3; y(ntv)=y3; z(ntv)=z3
                    VCOORD(ICOMP,ntv,1)=x3
                    VCOORD(ICOMP,ntv,2)=y3
                    VCOORD(ICOMP,ntv,3)=z3
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
                    VCOORD(ICOMP,ntv,1)=x3
                    VCOORD(ICOMP,ntv,2)=y3
                    VCOORD(ICOMP,ntv,3)=z3
                  elseif(INVT.eq.3)then
                    goto 147
                  endif
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
              if(MMOD.lt.8)then
                continue
              else
                COG1(1)=x(jwhich3);COG1(2)=y(jwhich3);COG1(3)=z(jwhich3)
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),iix,iiy)
                iicol=0
                if(greyok)call winscl('z',iicol)
                call esymbol(iix,iiy,24,1)
                iicol=0
                if(greyok)call winscl('-',iicol)
                call forceflush()
              endif
              do 446 jivj=1,NSUR
                if(jivj.eq.is) goto 446  ! skip past surface is
                icc=izstocn(icomp,jivj)
                ivjlimit=NVER(jivj)
                do 447 jivjj=1,ivjlimit
                  if(jivjj.eq.ivjlimit)then
                    jwhich1=JVN(jivj,jivjj)
                    jwhich2=JVN(jivj,1)
                  else
                    jwhich1=JVN(jivj,jivjj)
                    jwhich2=JVN(jivj,jivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(jwhich3,jwhich1,jwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 447
                  if(offset.lt.0.004)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',jwhich3,' @',X(jwhich3),Y(jwhich3),
     &                Z(jwhich3),' is close (',offset,') to edge ',
     &                jWHICH1,' &',jWHICH2,' of surface ',
     &                SSNAME(icc)
                    call edisp(itru,outs)

C If current surface (jivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (jivjj) and then
C inserting the new vertex index.
                   if(NVER(jivj)+1.le.MV)then
                      dok=.true.
                      call askok(' ',
     &                  'Insert this (close to edge) vertex?',
     &                  ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(jivj)=NVER(jivj)+1
                      IXV=NVER(jivj)+1
  348                 continue
                      IXV=IXV-1
                      JVN(jivj,IXV)=JVN(jivj,IXV-1)
                      IF(IXV.GT.jivjj+1)GOTO 348
                      JVN(jivj,jivjj+1)=jwhich3

C Debug...
C                      write(6,'(a,42i4)') 'now jvn is ',
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
          MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(IER)

        elseif(ino.eq.7)then

C Distance between point and a line (based on current G1 common block).
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
     &     Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',IWHICH2
          call edisp(itru,outs)
          MODIFY=.TRUE.; MODBND=.TRUE.
          iZBFLG(ICOMP)=0
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(IER)

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
            iwhich1=IVLST(1)
            lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2)
            lastv2=iwhich2
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
     &     Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',IWHICH2
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
            if(MMOD.lt.8)then
              continue
            else
              COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif
            dok=.true.
            call askok('Is this aligned vertex ok?',' ',ok,dok,nbhelp)
            if(ok)then
              X(IWHICH3)=x3; Y(IWHICH3)=y3; Z(IWHICH3)=z3
              MODGEO=.TRUE.
              iwhich3=NTV
              do 346 ivj=1,NSUR
                icc=izstocn(icomp,ivj)
                ivjlimit=NVER(ivj)
                do 347 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,1)
                  else
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 347
                  if(offset.lt.0.004)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',
     &                X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &                SSNAME(icc)
                    call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      dok=.true.
                      call askok(' ','Insert this vertex?',
     &                            ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(ivj)=NVER(ivj)+1
                      IXV=NVER(ivj)+1
  248                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 248
                      JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                     write(6,'(a,42i4)') 'now jvn is ',
C     &                (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 346 
                    endif
                  endif
  347           continue
  346         continue
            endif
          endif

        elseif(ino.eq.9)then
        
C Find close vertices.
          CALL EASKABC(' ','Options:','find close vertices',
     &      'find closest to a vertex','cancel',INVT,nbhelp)
          if(INVT.eq.1)then
            call edisp(itru,'Scanning for vertices within 50mm...')
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
            iwhich1=IVLST(1)
            lastv1=iwhich1  ! remember this
            iwhich2=IVLST(2)
            lastv2=iwhich2
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
            if(MMOD.lt.8)then
              continue
            else
              COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              iicol=0
              if(greyok)call winscl('z',iicol)
              call esymbol(iix,iiy,24,1)
              iicol=0
              if(greyok)call winscl('-',iicol)
              call forceflush()
            endif
            dok=.true.
            call askok('Apply this move?',' ',OK,dok,nbhelp)
            if(OK)then
              x(IWHICH1)=x3; y(IWHICH1)=y3; z(IWHICH1)=z3 ! place to update vcoord?
            endif
            MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
          else
            goto 3
          endif
        endif
        call usrmsg(' ',' ','-')

C If possible to add another vertex ask if calculated point should
C be a new vertex and update the wireframe. This logic will be 
C applied to the 'a' 'c' and 'f' options.
        if(ino.eq.1.or.ino.eq.3.or.ino.eq.5)then
          if(NTV.lt.MTV)then
            write(outs,'(a,i3,a)') 'Make vertex ',NTV+1,' a:'
            CALL EASKABC(' ',outs,'new vertex',
     &        'new vertex after editing','cancel',INVT,nbhelp)
            if(INVT.eq.1)then
              NTV=NTV+1
              NZTV(icomp)=NTV
              x(ntv)=x3; y(ntv)=y3; z(ntv)=z3
              VCOORD(ICOMP,ntv,1)=x3
              VCOORD(ICOMP,ntv,2)=y3
              VCOORD(ICOMP,ntv,3)=z3
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
              VCOORD(ICOMP,ntv,1)=x3
              VCOORD(ICOMP,ntv,2)=y3
              VCOORD(ICOMP,ntv,3)=z3
            elseif(INVT.eq.3)then
              goto 3
            endif
            ILEN=NTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(IER)

C Check to see if this new vertex should be included in any existing edges.
            if(invt.eq.1.or.invt.eq.2)then
              iwhich3=NTV
              do 246 ivj=1,NSUR
                icc=izstocn(icomp,ivj)
                ivjlimit=NVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,1)
                  else
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.0.004)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',X(NTV),Y(NTV),
     &                Z(NTV),' is close (',offset,') to edge ',
     &                IWHICH1,' &',IWHICH2,' of surface ',
     &                SSNAME(icc)
                    call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      dok=.true.
                      call askok(' ','Insert this vertex?',
     &                  ok,dok,nbhelp)
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(ivj)=NVER(ivj)+1
                      IXV=NVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                     write(6,'(a,42i4)') 'now jvn is ',
C     &                 (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 246 
                    endif
                  endif
  247           continue
  246         continue

C See if another one is required.
              if(ino.eq.1)then
                dok=.false.
                call askok('Another vertex along the line? ',' ',
     &            ok,dok,nbhelp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              elseif(ino.eq.3)then
                dok=.false.
                call askok('Another vertex from this origin? ',
     &            ' ',ok,dok,nbehlp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              elseif(ino.eq.5)then
                dok=.false.
                call askok('Another intersection of line and plane? ',
     &            ' ',ok,dok,nbehlp)
                if(ok)then
                  anotherone=.true.
                  goto 243
                else
                  anotherone=.false.
                endif
              endif

C Tidy up the wireframe image.
              MODIFY=.true.
              CALL INLNST(1)
              nzg=1
              nznog(1)=ICOMP
              izgfoc=ICOMP
              CALL ADJVIEW(IER)
              goto 92
            endif
          endif  ! of NTV less than MTV
        endif    ! of the check whether it should be edited or added
        goto 92

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Alter vertex list by deleting/ copy / replicate / editing.
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
        if(IRT.eq.1)then

C If user cancels jump back to menu setup, otherwise add the requested
C number of vertices.
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
              MODGEO=.TRUE.
              iZBFLG(ICOMP)=0
            endif
          endif
        elseif(IRT.eq.2)then

C Delete one or more vertices, get list and then sort in decending
C order so that compating of list works correctly.
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
            MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.; MODLEN=.TRUE.
            iZBFLG(ICOMP)=0
          endif
        elseif(IRT.eq.3)then

C In the case of copying existing vertices, process one or more,
C adding to zone data structure and updating the interface. Allow
C user to copy only as many vertices as there are in the zone but
C no more than will overrange MTV.
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
            ILEN=NTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
            iZBFLG(ICOMP)=0
          endif
        elseif(IRT.eq.4)then

C In the case of replicating an existing vertex, add it to zone
C data structure and updating the interface. Allow user to replicate
C only up to MTV.
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
                ILEN=NTV
                IPACT=CREATE
                CALL EKPAGE(IPACT)
                MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
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
              if(MMOD.lt.8)then
                continue
              else
                COG1(1)=X(IFOC); COG1(2)=Y(IFOC); COG1(3)=Z(IFOC)
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),iix,iiy)
                iicol=0
                if(greyok)call winscl('z',iicol)
                call esymbol(iix,iiy,24,1)
                iicol=0
                if(greyok)call winscl('-',iicol)
                call forceflush()
              endif

C Present vertex coords for editing then parse data from HOLD.
              WRITE(HOLD,'(3f11.5,a)')X(IFOC),Y(IFOC),Z(IFOC),'  '
              write(outs,'(a,i3,a)')
     &          ' Vertex (',ifoc,') X  Y  Z (in metres):'
 1146         CALL EASKS(HOLD,outs,' ',36,' 0. 0. 0. ','vertex coord',
     &          IER,nbhelp)
              K=0
              CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
              CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
              CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
              if(ier.ne.0)goto 1146
              MODIFY=.TRUE.; MODBND=.TRUE.; MODGEO=.TRUE.; MODLEN=.TRUE.
              iZBFLG(ICOMP)=0
              call warnmod(ICOMP,'str')
 1145       continue
          endif
        elseif(IRT.eq.6)then

C Delete unused vertices quietly, start at end and work back calling
C addvert silently.
          jipm=IPM
          do 139 ij=NTV,1,-1
            CALL ADDVERT(icomp,ij,'Q',IER)
  139     continue
          ILEN=NTV
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          IPACT= -1*jipm
          CALL EKPAGE(IPACT)
          MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
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

C Replicate logic from ADDVERT below.
          do 443 ix=1,NVC
            if(NTV+1.le.MTV)then
              found=.false.
              do 144 ixx = 1,NTV
                CALL ECLOSE(CX(ix),X(ixx),0.004,closex)
                CALL ECLOSE(CY(ix),Y(ixx),0.004,closey)
                CALL ECLOSE(CZ(ix),Z(ixx),0.004,closez)
                if(closex.and.closey.and.closez)then
                  found=.true.
                  goto 145
                endif
  144         continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list and update vcoord.
  145         if(found)then
                continue
              else
                NTV=NTV+1
                NZTV(icomp)=NTV
                X(NTV)=CX(ix); Y(NTV)=CY(ix); Z(NTV)=CZ(ix)
                VCOORD(ICOMP,ntv,1)=CX(ix)
                VCOORD(ICOMP,ntv,2)=CY(ix)
                VCOORD(ICOMP,ntv,3)=CZ(ix)

C For a unique vertex also test to see if it is close (+-0.003m) to
C one of the existing lines. Also check the last edge in the surface.
                iwhich3=NTV
                do 2246 ivj=1,NSUR
                  ivjlimit=NVER(ivj)
                  do 2247 ivjj=1,ivjlimit
                    if(ivjj.eq.ivjlimit)then
                      iwhich1=JVN(ivj,ivjj)
                      iwhich2=JVN(ivj,1)
                    else
                      iwhich1=JVN(ivj,ivjj)
                      iwhich2=JVN(ivj,ivjj+1)
                    endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                    call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &                matchver)
                    if(.NOT.matchver) goto 2247
                    if(offset.lt.0.004)then
                      write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                  'New vertex ',iwhich3,' @',
     &                  X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                  ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &                  sname(icomp,ivj)
                      call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                      if(NVER(ivj)+1.le.MV)then
                        if(offset.le.0.004)then
                          ok=.true.
                        else
                          dok=.true.
                          helptopic='vertex_near_edge'
                          call gethelptext(helpinsub,helptopic,nbhelp)
                          call askok(
     &                  'Insert this vertex into the adjacent surface?',
     &                  '(see help)',ok,dok,nbhelp)
                        endif
                      else
                        ok=.false.
                      endif
                      if(ok)then
                        NVER(ivj)=NVER(ivj)+1
                        IXV=NVER(ivj)+1
 1148                   continue
                        IXV=IXV-1
                        JVN(ivj,IXV)=JVN(ivj,IXV-1)
                        IF(IXV.GT.ivjj+1)GOTO 1148
                        JVN(ivj,ivjj+1)=iwhich3

C Debug.
C                         write(6,'(a,42i4)') 'now jvn is ',
C     &                     (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                        goto 2246 
                      endif
                    endif
 2247             continue
 2246           continue  ! end of loop of surfs in zone
              endif
            endif
  443     continue ! end of loop for copied verticies
          ILEN=NTV
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
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
                  dok=.true.
                  call askok(outs,'Ok to substitute?',ok,dok,nbhelp)
                  if(ok)then
                    JVN(ivj,ivjj)=iwhich2
                    MODGEO=.TRUE.; MODIFY=.TRUE.; MODBND=.TRUE.
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
  42    if(MMOD.lt.8)then
          continue
        else
          COG1(1)=X(IFOC); COG1(2)=Y(IFOC); COG1(3)=Z(IFOC)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          iicol=0
          if(greyok)call winscl('z',iicol)
          call esymbol(iix,iiy,24,1)
          iicol=0
          if(greyok)call winscl('-',iicol)
          call forceflush()
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
 9993     FORMAT(10(I2,', '))
          call edisp(itru,outs)
        endif

C Remind user if there are close vertices.
        do 106 iwhich2=1,NTV
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
        write(outs,'(a,i3,a)')' Vertex (',ifoc,') X  Y  Z (in metres):'

C Depending on position within array of vertices adapt the dialog to
C allow for jump to previous or next.
        if(ifoc.eq.1)then
          call EASKS2CMD(HOLD,outs,' ',' ','next vertex',iclkok,36,
     &      ' 0. 0. 0. ','vertex coord',IER,nbhelp)
        elseif(ifoc.eq.NZTV(icomp))then
          call EASKS2CMD(HOLD,outs,' ','prev vertex',' ',iclkok,36,
     &      ' 0. 0. 0. ','vertex coord',IER,nbhelp)
        else
          call EASKS2CMD(HOLD,outs,' ','prev vertex','next vertex ',
     &      iclkok,36,' 0. 0. 0. ','vertex coord',IER,nbhelp)
        endif
        K=0
        CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
        CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.; MODBND=.TRUE.; MODGEO=.TRUE.; MODLEN=.TRUE.
        iZBFLG(ICOMP)=0
        call warnmod(ICOMP,'str')

C Jump previous or next or return.  Change just edited vertex symbol
C back to black.  If the user happened to edit a vertex and ask for
C a jump then the display will not have a chance to re-draw fully
C so at least draw the vertex in its new location.
        if(iclkok.eq.1.and.ifoc.gt.1) then
          COG1(1)=X(IFOC); COG1(2)=Y(IFOC); COG1(3)=Z(IFOC)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          iicol=0
          if(greyok)call winscl('-',iicol)
          call esymbol(iix,iiy,24,1)
          call forceflush()
          ifoc=ifoc-1
          goto 42
        elseif(iclkok.eq.2.and.ifoc.lt.NZTV(icomp)) then
          COG1(1)=X(IFOC); COG1(2)=Y(IFOC); COG1(3)=Z(IFOC)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          iicol=0
          if(greyok)call winscl('-',iicol)
          call esymbol(iix,iiy,24,1)
          call forceflush()
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
#include "model.h"
#include "geometry.h"
#include "epara.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"

C Parameters
      integer itrc  ! verbosity level
      integer itru  ! freeback unit
      integer icomp ! zone index
      integer ier   ! error state zero ok one is problem
      
      integer lnblnk  ! function definition
      integer iCountWords

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)
      DIMENSION  COG1(3),COG2(3),COG3(3)

      CHARACTER DESCRC*25
      LOGICAL MODIFY,MODLEN,MODBND,OK,bound,DOK,nameok
      logical newgeo  ! to use for testing if new/old geometry file.
      integer ifoc    ! local variable for current surface index
      integer iwhich  ! for the worst off point (warped surface check)
      integer iadds   ! for use in addsur calls
      logical close   ! for checking if warp distance is zero
      logical foundit ! if isolation of warped vertex was successful
      real ZZZ,ofby   ! the distance worst point is warped
      real XXS,YYS,ZZS ! suggested point on the line.

      DIMENSION VERT(36),options(9)
      DIMENSION TMAT(4,4),RMAT(4,4),EQN(4),CG(3),EP(3)  ! for warp checks
      real XX,YY,ZZ,XT,YT,ZT
      DIMENSION XX(MV),YY(MV),ZZ(MV),XT(MV),YT(MV),ZT(MV),jvn1(MV)
      dimension ivlst(MTV)  ! for vertex selection
      CHARACTER VERT*34,options*34,KEY*1,SN*12,T14*14,D14*14
      character head*32,HOLDS*36,outs*124
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16 
      CHARACTER SNAME1*12,SNAMED*12
      character hold64*64,t248*248,OPT*12
      character constr*12     ! to pass to addmass
      character message*48

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*8,rsuse2*8
      character rsparent*12
      real XO1,ZO1  ! to prevent name clash in geometry.h
      integer MVERT,IVERT,NITMS,IRT ! max items and current menu item
      integer icmpall  ! to signal request to compare all names
      integer ibopt    ! to specify which mass origin option
      integer mnulen,iwhich2   ! multi column lines
      logical greyok,odd   ! for display of vertex info
      
#ifdef OSI
      integer iix,iiy,iicol
#else
      integer*8 iix,iiy,iicol
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDVLIST'  ! set for subroutine

C Check if we have colour.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Initialise zone surface-vertex list menu size variables based on
C window size. IVERT is the menu position, MVERT the current
C number of menu lines.
      V1=0.0; AZ=0.0; EL=0.0  ! initial values for editing
      newgeo=.false.  ! assume older format geometry.
      MHEAD=4
      MCTL=8
      ILEN=NSUR
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 ILEN=NSUR
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      call ckvert(0,icomp,bound,iub,inv,ier)
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
        iprb=MAX0(iub,inv)
        WRITE(VERT(1),'(A,i3,a)')  '  enclosure:',iprb,' PROBLEM EDGES'
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
      VERT(M+3)  ='* delete a surface                '
      VERT(M+4)  ='> transforms                      '
      VERT(M+5)  ='! browse surface-vertex topology  '
      VERT(M+6)  ='@ check surface-vertex topology   '
      VERT(M+7)  ='? help                            '
      VERT(M+8)  ='- exit this menu                  '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

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
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        helptopic='surface_edge_list'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('vertex section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Check vertex topology.
        call tstamp('>','PRJ: check zone vertex topology')
        call ckvert(1,icomp,bound,iub,inv,ier)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        CALL VERINFO(ICOMP,ITRU)
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Miscel. geometric transforms. Ask which surface and setup temp array.
        CALL EPMENSV
        CALL EASKSUR(ICOMP,IS,'-','Select surface to act on.',
     &        ' ',IER)
        CALL EPMENRC
        if(is.eq.0)goto 92

        N = NVER(IS)
        DO 150 J = 1,N
          XX(J) = X(JVN(IS,J))
          YY(J) = Y(JVN(IS,J))
          ZZ(J) = Z(JVN(IS,J))
  150   CONTINUE

        call ZSID(icomp,IS,ZSDES,ZSDESC,ZSDESS)
        write(head,'(a,a)') 'Actions ',ZSDESS
        options(1)  ='a shift along the surface normal  '
        options(2)  ='b XYZ translate                   '
        options(3)  ='c rotate                          '
        options(4)  ='d invert (edge list)              '
        options(5)  ='e combination (shift/rotate/inv)  '
        options(6)  ='f identify cause of surface warp  '
        options(7)  ='? help                            '
        options(8)  ='- exit this menu                  '
        NITMS=8
        CALL EMENU(head,options,NITMS,IRT)
        if(IRT.EQ.(8))then
          MODIFY=.FALSE.
          goto 92
        elseif(IRT.EQ.7)then

C List help text for the options menu.
          helptopic='surface_edge_list'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('transform section',nbhelp,'-',0,0,IER)
        elseif(IRT.eq.1.or.IRT.eq.5)then

C Shift along normal and copy transformed points back to XX,YY,ZZ array.
          call tstamp('>','PRJ: do surface shift along normal')
          vdis=0.0
          CALL EASKR(vdis,' ','Distance along normal?',
     &         -99.999,'F',99.999,'W',0.0,'d along normal',IER,nbhelp)
          CALL TRANSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,ZSDES)
          do 151 nt1=1,N
            XX(nt1)=XT(nt1)
            YY(nt1)=YT(nt1)
            ZZ(nt1)=ZT(nt1)
 151      continue
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
          DO 153 I=1,N
            XX(I)=XX(I)+VALX
            YY(I)=YY(I)+VALY
            ZZ(I)=ZZ(I)+VALZ
 153      continue
        endif
        if(IRT.eq.3.or.IRT.eq.5)then

C Rotate around a point in the surface (offer user cancel option).
          call tstamp('>','PRJ: do surface rotation')
          CALL EASKI(IRV,'Rotate about which vertex index: ',
     &       '(ie. 1st, 2nd or 3rd in the surface) ',
     &       1,'F',N,'F',1,'rotation anchor',IERI,nbhelp)
          if(ieri.eq.-3)then
            MODIFY=.FALSE.
            goto 92
          endif
          ANGR=0.
          CALL EASKR(ANGR,' ','Rotation (+ = anticlockwise)?',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANGR*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=XX(IRV)
            YY1=YY(IRV)
            DO 100 I=1,N
              XXX=XX(I)-XX1
              YYY=YY(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XX(I)=XR+XX1
              YY(I)=YR+YY1
  100       continue
          endif
        endif
        if(IRT.eq.4)then

C Reverse the ordering, redraw image and return.
          call tstamp('>','PRJ: do surface invert vertex order')
          do 146, iyy = 1,NVER(IS)
            jvn1(iyy)=JVN(IS,iyy)
  146     CONTINUE
          JVN(IS,1)=jvn1(2)
          JVN(IS,2)=jvn1(1)
          do 147, iyy = 3,NVER(IS)
            izz=NVER(IS)+3-iyy
            JVN(IS,iyy)=jvn1(izz)
  147     CONTINUE
          call zgupdate(1,icomp,ier)
          MODIFY=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(IER)
          goto 92
        endif
        if(IRT.eq.6)then

C Code for testing which vertex might be the problem. The brute force
C method for surfaces of more than 3 edges is to check if the surface
C if warped via call to CHECKWARP and then to call it again until
C it reports back no warps. 
          if(nver(is).lt.4) goto 92
          DO 606 KK=1,NVER(is)
            XX(KK) = X(JVN(is,KK))
            YY(KK) = Y(JVN(is,KK))
            ZZ(KK) = Z(JVN(is,KK))
  606     CONTINUE
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
                DO 250 J = 1,3
                  EP(J) = CG(J) + EQN(J)
  250           CONTINUE
      
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
                WRITE(outs,'(a,3F9.5)') ' Suggested coords are',
     &            XXS,YYS,ZZS
                CALL EDISP(iuout,outs)

C Ask user if they want to update the vertex with the new values.
                DOK=.true.
                CALL ASKOK(outs,'Update the vertex?',OK,DOK,nbhelp)
                if(OK)then
                  X(JVN(is,loop))=XXS
                  Y(JVN(is,loop))=YYS
                  Z(JVN(is,loop))=ZZS
                  call zgupdate(1,icomp,ier)
                  MODIFY=.TRUE.
                  CALL INLNST(1)
                  nzg=1
                  nznog(1)=ICOMP
                  izgfoc=ICOMP
                  CALL ADJVIEW(IER)
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
        do 143 ix = 1,N
          XMN=AMIN1(XMN,XX(ix))
          YMN=AMIN1(YMN,YY(ix))
          ZMN=AMIN1(ZMN,ZZ(ix))
          XMX=AMAX1(XMX,XX(ix))
          YMX=AMAX1(YMX,YY(ix))
          ZMX=AMAX1(ZMX,ZZ(ix))
  143   continue

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)

C Show these points as small circles...
        call edisp(iuout,' proposed points are shown as dots....')
        DO 154 J = 1,N
          if(MMOD.lt.8)then
            continue
          else
            COG1(1)=XX(J); COG1(2)=YY(J); COG1(3)=ZZ(J)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            iicol=0
            if(greyok)call winscl('z',iicol)
            call esymbol(iix,iiy,24,1)
            iicol=0
            if(greyok)call winscl('-',iicol)
          endif
  154   CONTINUE
        call forceflush()

C Then draw the revised and if acceptable then used transformed
C points within the zone in place of the originals....
        DOK=.true.
        CALL ASKOK('Apply the transformed points to the',
     &             'surface (and any attached surfaces)?',
     &             OK,DOK,nbhelp)
        if(OK)then
          MODIFY=.TRUE.
          DO 155 J = 1,N
            X(JVN(IS,J)) = XX(J) 
            Y(JVN(IS,J)) = YY(J)
            Z(JVN(IS,J)) = ZZ(J) 
  155     CONTINUE
          call warnmod(ICOMP,'str')
          call zgupdate(1,icomp,ier)
        else
          MODIFY=.FALSE.
        endif
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Delete a surface. If number of surfaces changed then update cfg.
        CALL EPMENSV
        CALL EASKSUR(ICOMP,IS,'-','Select surface to delete.',
     &        ' ',IER)
        CALL EPMENRC
        IF(IS.EQ.0)goto 92
        LASTS=NSUR
        iopt=0    ! offer editing box with mouse click.
        itrcl=0
        CALL ADDSUR(itrcl,ICOMP,IS,'D','V',iopt,IER)
        if(LASTS.ne.NSUR)then
          CALL EMKCFG('-',IER)
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          call warnmod(ICOMP,'sf-')
        endif
        ILEN=NSUR
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

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
        elseif(IRT.eq.2)then
          call tstamp('>','PRJ: insert surface via vertex')
          iopt=1     ! offer mouse click directly
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'A','V',iopt,IER)
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
          XO1=0.
          ZO1=0.
          XW=0.
          ZH=0.
          rsname=' '
          rsotf=' '
          rsmlcn=' '
          rsuse1=' '
          rsuse2=' '
          rsparent='-'
          CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)
        elseif(IRT.eq.4)then
          call tstamp('>','PRJ: copy local surfaces')
          iopt=2      ! offer copy of local surfaces
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'C','V',iopt,IER)
        elseif(IRT.eq.5)then
          call tstamp('>','PRJ: copy another zone surfaces')
          iopt=1      ! offer mouse click directly ??
          iadds=0; itrcl=0
          CALL ADDSUR(ITRCL,ICOMP,iadds,'C','V',iopt,IER)
        elseif(IRT.eq.6.or.IRT.eq.9)then

C Get vertical rectangular surface orgin.
          IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the zone too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add vertical surface')

C Offer similar choices to setting up obstructions (see edobs.F)
          call easkabcd('Origin choices:',' ','edit origin',
     &      'use zone vertex','@ angle&dist from vertex','continue',
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
 343          CALL EASKS(HOLDS,
     &    'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &    ' ',32,' 1. 0. 0. ','dist azim elev',IER,nbhelp)
              K=0
              CALL EGETWR(HOLDS,K,V1,-999.9,999.9,'W','dist',IER)
              CALL EGETWR(HOLDS,K,AZ,-359.9,359.9,'W','azim',IER)
              CALL EGETWR(HOLDS,K,EL,-90.0,90.0,'W','elev',IER)
              if(ier.ne.0)goto 343
              PI = 4.0 * ATAN(1.0)
              RAD = PI/180.
              RYAZI = AZ*RAD
              RSALT = EL*RAD
              z3 = V1*SIN(RSALT)
              XYDIS = V1*COS(RSALT)
              IF (XYDIS .LT. 1E-6)THEN
                x3 = 0.
                y3 = 0.
              ELSE
                x3 = XYDIS*SIN(RYAZI)
                y3 = XYDIS*COS(RYAZI)
              ENDIF
              write(outs,'(a,3f10.4)') ' Point @ X,Y,Z:',x3+X(iwhich1),
     &          y3+Y(iwhich1),z3+Z(iwhich1)
              call edisp(itru,outs)
              x3=x3+X(iwhich1)
              y3=y3+Y(iwhich1)
              z3=z3+Z(iwhich1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')x3,y3,z3
              MODIFY=.TRUE.
            else
              goto 92
            endif
          elseif(ibopt.eq.4)then
            goto 92
          endif
          
          CALL EASKS(HOLDS,' Vertical surface origin X Y & Z: ',' ',
     &      36,' 0.0  0.0   0.0   ','vert origin XYZ',IER,nbehlp)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get azimuth and length and height.
          HOLDS= ' 180.00   1.000  1.000   '
          CALL EASKS(HOLDS,' Surface azimuth (deg) length & height:',
     &      ' ',36,' 180.0 1.0 1.0  ','azim length height',IER,nbhelp)
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
          call easkabcd('Origin choices:',' ','edit origin',
     &      'use zone vertex','@ angle&dist from vertex','continue',
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
              RAD = PI/180.
              RYAZI = AZ*RAD
              RSALT = EL*RAD
              z3 = V1*SIN(RSALT)
              XYDIS = V1*COS(RSALT)
              IF (XYDIS .LT. 1E-6)THEN
                x3 = 0.
                y3 = 0.
              ELSE
                x3 = XYDIS*SIN(RYAZI)
                y3 = XYDIS*COS(RYAZI)
              ENDIF
              write(outs,'(a,3f10.4)') ' Point @ X,Y,Z:',x3+X(iwhich1),
     &          y3+Y(iwhich1),z3+Z(iwhich1)
              call edisp(itru,outs)
              x3=x3+X(iwhich1)
              y3=y3+Y(iwhich1)
              z3=z3+Z(iwhich1)
              HOLDS=' '
              WRITE(HOLDS,'(1x,3f9.4)')x3,y3,z3
              MODIFY=.TRUE.
            else
              goto 92
            endif
          elseif(ibopt.eq.4)then
            goto 92
          endif
          CALL EASKS(HOLDS,' Horizontal surface origin X Y & Z: ',
     &      ' ',36,' 0.00  0.00  0.00  ','horiz origin XYZ',IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-99.0,99.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-99.0,99.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-99.0,99.0,'W','Z org',IER)

C Get rotation and length and height.
          HOLDS= ' 0.0    1.000  1.000    '
          CALL EASKS(HOLDS,' Surface rotation (deg) length & height: ',
     &      ' ',36,' 180.0  1.0  1.0  ','rotation length height',
     &      IER,nbhelp)
          K=0
          CALL EGETWR(HOLDS,K,AANG,0.0,360.0,'W','Rot',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

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
          ibasecount=ibaseedge  ! counter for original edges left to process
          itopcount = 1 ! counter for top edges to process
          iwcount=(ibaseedge)+1  ! iwcount is the total number of new surfaces
          itopsurf=NSUR+1  ! index of the top surface

 141      icount=icount+1
          if(icount.eq.1)then

C The top surface has the same number of verticies as the base surface
C edge-ordered in opposite direction. The new vertices are generated via
C filling XX YY ZZ arrays with points for surface IS then call to TRANSUR.
C vdis is the distance along the surface normal (e.g. if the initial
C surface is the base then vdis should be negative).
            N = ibaseedge
            DO 241 J = 1,N
              XX(J) = X(JVN(IS,J))
              YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
  241       CONTINUE
            CALL TRANSUR(ITRC,ITRU,N,XX,YY,ZZ,vdis,XT,YT,ZT,ZSDES)

C Add the transformed points to the zone (later check for unique).
            do 242 ix = 1,ibaseedge
              if(NTV+1.le.MTV)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                X(NTV)=XT(ix)
                Y(NTV)=YT(ix)
                Z(NTV)=ZT(ix)
                VCOORD(ICOMP,ntv,1)=XT(ix)
                VCOORD(ICOMP,ntv,2)=YT(ix)
                VCOORD(ICOMP,ntv,3)=ZT(ix)
                XMN=AMIN1(XMN,X(NTV))
                YMN=AMIN1(YMN,Y(NTV))
                ZMN=AMIN1(ZMN,Z(NTV))
                XMX=AMAX1(XMX,X(NTV))
                YMX=AMAX1(YMX,Y(NTV))
                ZMX=AMAX1(ZMX,Z(NTV))
              endif
  242       continue

C Assign edges for the top surface in reverse order of the original.
            iecount=NTV
            do 144 ix = 1,ibaseedge
              JVN(itopsurf,ix)=iecount

C Debug.
C              write(6,*) ' top ',ix,iecount,JVN(itopsurf,ix),
C     &          X(iecount),Y(iecount),Z(iecount)

              iecount=iecount-1
  144       continue
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
            JVN(NSUR+1,1)=iv1
            JVN(NSUR+1,2)=iv2
            JVN(NSUR+1,3)=iv3
            JVN(NSUR+1,4)=iv4
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
C user cannot see the newly extruded surface it is pointless to
C ask them about its use and composition.
          ICONT=IZSTOCN(icomp,nsur)+1
          NSUR=NSUR+1
          NZSUR(ICOMP)=NZSUR(ICOMP)+1
          SNAME(ICOMP,NSUR)=SNAME1
          SMLCN(NSUR)='UNKNOWN'
          SVFC(NSUR)='UNKN'
          SOTF(NSUR)='OPAQUE'
          SOTHER(NSUR,1)='UNKNOWN'
          SOTHER(NSUR,2)='0'
          SOTHER(NSUR,3)='0'
          SPARENT(NSUR)='-'
          SUSE(NSUR,1)='-'
          SUSE(NSUR,2)='-'
          icon=ICONT
          SSUSE(icon,1)='-'
          SSUSE(icon,2)='-'

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
          CALL EASKS(T14,'Inserted surface name (12 char max):','  ',
     &      14,'new_door','surface name',IER,nbhelp)
          write(SN,'(a)') T14(1:12)

C Ask for surface construction.
          CALL EPMENSV
          CALL EPKMLC(ISEL,'Select one of the constructions for the',
     &      'inserted surface. ',IER)
          CALL EPMENRC
          if(ISEL.GT.0)then
            WRITE(constr,'(A12)') mlcname(ISEL)(1:12)

C Find the optical name.
            OPT=' '
            IF(mlctype(ISEL)(1:4).EQ.'OPAQ')then
              OPT='OPAQUE'
            ELSEIF(mlctype(ISEL)(1:4).EQ.'CFC ')then
              SOTF(NSUR)='CFC '
            ELSE
              WRITE(OPT,'(A)') mlcoptical(ISEL)(1:12)
              IF(OPT.EQ.' ')OPT='UNKNOWN'
            ENDIF
          else
            WRITE(constr,'(A)') 'UNKNOWN'
            OPT='OPAQUE'
          endif

C Instanciate the new surface via call addmass with all of the parameters.
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
            MODIFY=.TRUE.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(IER)
            GOTO 30
          elseif(INVT.eq.-3)then
            return
          endif
        endif   ! of processing of vert or horiz rectangle.

C Record the recent changes and re-display
 77     if(LASTS.ne.NSUR)then
          CALL EMKCFG('-',IER)
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
          call warnmod(ICOMP,'sf+')
        endif
        MHEAD=4
        MCTL=8
        ILEN=NSUR
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        iZBFLG(ICOMP)=0
        call usrmsg(' ',' ','-')
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
      ELSEIF(IVERT.EQ.(MVERT-7))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF

      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND. Treat the list of vertices as a
C long string and then parse the data.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

C Set all surfaces to standard line width and surface being edited to 
C a thick line.
        MODIFY=.TRUE.
        CALL INLNST(1)
        CALL SURADJ(ICOMP,IFOC,IE,TMP,IZC,ISC,IC,DESCRC)
        LINSTY(IC)=2
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)

   93   helptopic='surface_edge_order'
        call gethelptext(helpinsub,helptopic,nbhelp)
        NV=NVER(IFOC)

C If in graphic mode draw the points.
        if(MMOD.lt.8)then
          continue
        else
          do 961 i=1,nv
            iwhich=jvn(ifoc,i)
            COG1(1)=X(iwhich); COG1(2)=Y(iwhich); COG1(3)=Z(iwhich)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            iicol=0
            if(greyok)call winscl('z',iicol)
            call esymbol(iix,iiy,24,1)
            call VERTLBL(iix,iiy,iwhich,ier)
            iicol=0
            if(greyok)call winscl('-',iicol)
            call forceflush()
  961     continue
        endif

C List out the vertices associated with this surface. To save
C space, use double column as in the emenu code in
C esru_libNonGTK.F and also for use in the model contents report.

        write(message,'(2a)') 'Vertices (X Y Z) used by ',
     &    SNAME(ICOMP,IFOC)
        call edisp(iuout,message)

C See if an even or odd number of items in list.
        im=MOD(NV,2)
        odd=.false.
        if(im.eq.1) odd=.true.
        if(nv.lt.8)then
          do 960 i=1,nv
            iwhich=jvn(ifoc,i)
            write(outs,'(a,i3,3f11.5)') 'vertex ',iwhich,X(iwhich),
     &        Y(iwhich),Z(iwhich)
            call edisp(iuout,outs)
  960     continue
        else
          MNULEN=(NV/2)
          DO 193 K=1,MNULEN
            iwhich=jvn(ifoc,k)
            iwhich2=jvn(ifoc,k+mnulen)
            WRITE(outs,'(a,i3,3f11.5,a,i3,3f11.5)') 'vertex ',iwhich,
     &        X(iwhich),Y(iwhich),Z(iwhich),'   vertex ',iwhich2,
     &        X(iwhich2),Y(iwhich2),Z(iwhich2)
            call edisp(iuout,outs)
  193     CONTINUE
          IF(odd)THEN
            WRITE(outs,'(a,i3,3f11.5)') 'vertex ',nv,X(nv),
     &        Y(nv),Z(nv)
            call edisp(iuout,outs)
          ENDIF
        endif

C Set up a long string buffer for editing vertex list for the
C surface. Include surface name in the dialog. This write statement
C should be kept up to date with MV changes. 
        hold64='                                                      '
        write(t248,'(4a)') hold64,hold64,hold64,'                     '
        WRITE(t248,'(42I4)')(JVN(IFOC,j),J=1,nv)
        write(message,'(2a)') 'Vertices associated with ',
     &    SNAME(ICOMP,IFOC)
        CALL EASKS248(t248,message,
     &    ' ',72,' 1  2  3  4  ','associated vertices',IIER,nbhelp)
        NV = iCountWords(t248)
        K=0
        DO 94 J=1,NV

C Read an index, check if within range and if so add to JVN().
          CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IIER)
          IF(IIER.NE.0)GOTO 93
          if(JV.eq.0.or.JV.gt.NTV)then
            call edisp(iuout,
     &        'At least one of the vertex indices was out of range!')
            goto 93
          endif
          JVN(IFOC,J)=JV
   94   CONTINUE
        NVER(IFOC)=NV
        MODIFY=.TRUE.
        call warnmod(ICOMP,'str')

C Check to see if this surface is warped.
C Derive the equation of the polygon.
        DO 607 KK=1,NVER(ifoc)
          XX(KK) = X(JVN(ifoc,KK))
          YY(KK) = Y(JVN(ifoc,KK))
          ZZ(KK) = Z(JVN(ifoc,KK))
  607   CONTINUE
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

      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-4
      goto  92

      END 

C ************* EPKVERT 
C EPKVERT Select one or more vertices from information currently in
C common block G1.
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
      COMMON/OUTIN/IUOUT,IUIN
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      DIMENSION  item(36)
      real COG1,COG2,COG3  ! position to check against
      DIMENSION  COG1(3),COG2(3),COG3(3),IVLST(MTV)
      CHARACTER item*33,outs*124,KEY*1
      logical greyok,found
      integer MVERT,IVERT ! max items and current menu item

#ifdef OSI
      integer iix,iiy,ixd,iyd,iicol
#else
      integer*8 iix,iiy,ixd,iyd,iicol
#endif

C Initialise vertex list menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Also clear tagged items list (IVLST).
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif
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
          WRITE(item(M),'(A1,I3,3F9.3)')KEY,L,X(L),Y(L),Z(L)
          do 20 K=1,INPICK
            if(IVLST(K).eq.L) then
              WRITE(item(M),'(A1,I3,3F9.3,A)')KEY,L,X(L),Y(L),
     &          Z(L),' *'
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
  46      CALL trackview(ii,ixx,iyy)
          if(ii.eq.69.or.ii.eq.101)goto 47
          found=.false.
          do 45 i=1,NTV
            COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),iix,iiy)
            ixd=iix-ixx
            iyd=iiy-iyy
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
              call edisp(iuout,'code for edge check in progress')
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


C ******* surfprm
C Surfprm returns the number of edges and total length of
C the perimiter of the polygon. Assumes that the surface is
C in the current zones common blocks.
      subroutine surfprm(izone,isurf,dupedges,perim)
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
          do 40 m=1,NVER(isurf)
            do 41 n=1,NVER(isurf)
              IF(m.EQ.n)goto 41
              J1=JVN(isurf,n)
              J2=JVN(isurf,m)
              IF(J1.EQ.J2)dupedges=.true.
  41        continue
  40      continue

C Step through the vertices of each edge in turn and find distance.
          list=NVER(isurf)-1
          do 42 i=1,list
            j=JVN(isurf,i)
            k=JVN(isurf,i+1)
            vdis=0.0
            vdis= crowxyz(x(j),y(j),z(j),x(k),y(k),z(k))
            perim=perim+vdis
  42      continue

C Link back to start vertex
          j=JVN(isurf,NVER(isurf))
          k=JVN(isurf,1)
          vdis=0.0
          vdis= crowxyz(x(j),y(j),z(j),x(k),y(k),z(k))
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

      common/dllerr/dllsubr,dllmesg

      DIMENSION XX(MS),YY(MS),IVA(MS)
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)

C For each possible default window and default door the relevant data.
      DIMENSION VALX(MS),VALZ(MS),VALW(MS),VALH(MS)
      DIMENSION VALDX(MS),VALDW(MS),VALDH(MS)

C Pointer from default window or door to the parent surface it is to
C be inserted.
      dimension iwhichsurfw(MS), iwhichsurfd(MS)
      CHARACTER OUTSTR*124,WORD*20
      character lzip*72,dfile*72
      character phrasea*64,phraseb*64,outs*124
      character dllsubr*12,dllmesg*124,act*2
      logical dll,checkbase
      logical newgeo  ! to use for testing if new/old geometry file.

C Strings for surface attributes to pass to insrec.
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*8,rsuse2*8
      character rsparent*12
      real ARX,Z1X,Z2X,DX1,DY1,DZ1  ! to avoid variable clash with geometry.h
      real XO1,YO1,ZO1

      IER=0
      phrasea=' '
      phraseb=' '
      checkbase=.false.
      newgeo=.false.  ! assume older format geometry.

C Check if running in dll mode.
      call isadll(dll)

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
        rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! shift the pending rotation
        rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
        rotateit(ICOMP,6)=rotateit(ICOMP,3)
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
          rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! shift the pending rotation
          rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
          rotateit(ICOMP,6)=rotateit(ICOMP,3)
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
          rotateit(ICOMP,4)=rotateit(ICOMP,1)   ! remmeber the pending rotation
          rotateit(ICOMP,5)=rotateit(ICOMP,2)   ! and the origin of the zone
          rotateit(ICOMP,6)=rotateit(ICOMP,3)
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
              write(outs,'(a,i2,a,f6.3,a,f6.3,a,f6.3,a,f6.3)') 
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
              write(outs,'(a,i2,a,f6.3,a,f6.3,a,f6.3)') 
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
      IBASES(ICOMP,1)=0
      IBASES(ICOMP,2)=0
      IBASES(ICOMP,3)=0
      IBASES(ICOMP,4)=0
      IBASES(ICOMP,5)=0
      IBASES(ICOMP,6)=0
      ZBASEA(ICOMP)=0.00
      IER=0

C If base area has not yet been calculated, do this now and
C get surface areas via call to zinfo.
      if(checkbase)then
        call zinfo(sna,zoa,pazi,pelv,zvol)
        vol(icomp)=zvol
        zonetotsurfacearea(icomp)=zoa
        do 43 ijj=1,NSUR
          if(SVFC(ijj).eq.'FLOR')then

C If surface `flor` not included in the list add it.
            lastlist=lastlist+1
            if(lastlist.le.6)then
              IBASES(ICOMP,lastlist)=ijj
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SNA(ijj)
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
        if(SOTHER(icc,1)(1:8).eq.'EXTERIOR')then
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc,1)(1:7).eq.'SIMILAR')then
          ICT(ICCC)=1
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc,1)(1:9).eq.'ADIABATIC')then
          ICT(ICCC)=5
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc,1)(1:8).eq.'CONSTANT')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a CONSTANT connection. Use the'
          call usrmsg(outs,
     &      'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc,1)(1:6).eq.'GROUND')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a GROUND connection. Use the'
          call usrmsg(outs,
     &     'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc,1)(1:7).eq.'UNKNOWN')then
          ICT(ICCC)=-1
          IC2(ICCC)=0
          IE2(ICCC)=0
        else
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a possible partition. Confirm its'
          call usrmsg(outs,
     &      'surface attributes or use topology tool.','W')
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        endif
        SSNAME(iccc)=SNAME(NCOMP,icc)
        SSOTF(iccc)=SOTF(icc)
        SSMLCN(iccc)=SMLCN(icc)
        ssmlcindex(iccc)=ssmlcindex(icc)
        SSVFC(iccc)=SVFC(icc)
        SSOTHER(iccc,1)=SOTHER(icc,1)
        SSOTHER(iccc,2)=SOTHER(icc,2)
        SSOTHER(iccc,3)=SOTHER(icc,3)
        SSUSE(iccc,1)=SUSE(icc,1)
        SSUSE(iccc,2)=SUSE(icc,2)
        SSPARENT(iccc)=SPARENT(icc)
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
      if(igupgrade.eq.2.and.(.NOT.newgeo))then
        gversion(icomp) =1.1
        newgeo = .true.
      endif
      if(newgeo)then
        call geowrite(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)
      else
        call emkgeo(IFIL+2,LGEOM(icomp),icomp,4,IER)
      endif

C If there are default windows then attempt to insert them as
C transparent surfaces.
      if(nbdef.gt.0)then
        do 66 idw=1,nbdef
          isurf=iwhichsurfw(idw)
          act='sw'
          XO1=VALX(idw)
          ZO1=VALZ(idw)
          XW=VALW(idw)
          ZH=VALH(idw)
          rsname=' '
          rsotf=' '
          rsmlcn=' '
          rsuse1=' '
          rsuse2=' '
          rsparent='-'
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)
  66    continue
      endif

C If there are default doors, attempt to insert them as surfaces.
      if(nbdor.gt.0)then
        do 67 idd=1,nbdor
          isurf=iwhichsurfd(idd)
          act='sd'
          XO1=VALDX(idd)
          ZO1=0.0
          XW=VALDW(idd)
          ZH=VALDH(idd)
          rsname=' '
          rsotf=' '
          rsmlcn=' '
          rsuse1=' '
          rsuse2=' '
          rsparent='-'
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO1,ZO1,XW,ZH,
     &      rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,IER)
  67    continue
      endif

C Save zone if more surfaces have been added.
      if(nbdef.gt.0.or.nbdor.gt.0)then
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(igupgrade.eq.2.and.(.NOT.newgeo))then
          gversion(icomp) =1.1
          newgeo = .true.
        endif
        if(newgeo)then
          call geowrite(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)
        else
          call emkgeo(IFIL+2,LGEOM(icomp),icomp,4,IER)
        endif
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
      if(dll)then
        dllsubr='EZIPIN'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ios)
        return
      else
        call edisp(iuout,outs)
        call edisp(iuout,OUTSTR)
        IER=1
        CALL ERPFREE(IUNIT,ios)
        RETURN
      endif

C Found unexpected item.
 1002 if(dll)then
        dllsubr='EZIPIN'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(itru,outs)
        IER=1
        RETURN
      endif

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
C derived.h are derived areas and U values for zones and the model
#include "derived.h"
 
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      LOGICAL MODIFY,MODLEN,MODBND
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)

C bridgelen was calculated in subroutine suredgeadj
      real bridgelen ! Length of potential thermal bridges in each zone.
      common/gbridge/bridgelen(MCOM,10)

C Thermal bridge common block: 
C nbrdg is the number of thermal bridge types in the zone
C psi is the linear thermal transmittance value, 
C lngth is the length of the thermal bridge and
C ibrdg is an index defining the type of thermal bridge as follows:
C   1 to 8 (NOT metal cladding)
C   1 is "roof-wall", 2 is "wall-ground floor"
C   3 is "wall-wall (convex corner)", 4 is "wall-wall (concave corner)"
C   5 is "wall-floor (not ground floor)", 6 is "lintel above window or door"
C   7 is "Sill below window", 8 is "jamb at window or door"
C   9 to 15 (metal cladding)
C   9 is "roof-wall-mc", 10 is "wall-ground floor-mc"
C  11 is "wall-wall (corner)-mc", 12 is "wall-floor (not ground floor)-mc"
C  13 is "lintel above window or door-mc", 14 is "Sill below window-mc"
C  15 is "jamb at window or door-mc"
C  16 is for any other categories of thermal bridges

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

      DIMENSION COG1(3),COG2(3),COG3(3)
      integer lnb
      real psiv,lenv  ! local variables for psi and length.
      integer ISDN,INODA ! max items and current menu item
      real defpsi     ! default value for psi
      dimension defpsi(16)
      real lenpsi     ! default value for length based on bridgelen.
      dimension lenpsi(16)
      real zerolen,zeropsi  ! for bridge types that have yet to be used.
      logical foundit
      logical greyok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

#ifdef OSI
      integer iix,iiy,iix2,iiy2,iicol
#else
      integer*8 iix,iiy,iix2,iiy2,iicol
#endif

      helpinsub='LINTHBRDG'  ! set for subroutine

      if(nbrdg(icomp).gt.0)then
         CALL SIGFIG(psi(ICOMP,1),5,RNO,SIGSTR,LSTR)
         CALL SIGFIG(lngth(ICOMP,1),5,RNO,SIGSTR,LSTR)
      endif
      zerolen=0.0
      zeropsi=0.0
      lookfor=0
      INVT=0

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
        call edisp(iuout,'Based on surface form & boundary condition:')
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length roof-wall:',
     &      bridgelen(icomp,1)
        else
          write(outs,'(a,f7.2,a)')'bridge length roof-wall:',
     &      bridgelen(icomp,1),' (midnight blue)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length wall-ground:',
     &      bridgelen(icomp,2)
        else
          write(outs,'(a,f7.2,a)')'bridge length wall-ground:',
     &      bridgelen(icomp,2),' (lt brown)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length wall-wall convex:',
     &      bridgelen(icomp,3)
        else
          write(outs,'(a,f7.2,a)')'bridge length wall-wall convex:',
     &      bridgelen(icomp,3),' (forest green)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length wall-wall concave:',
     &      bridgelen(icomp,4)
        else
          write(outs,'(a,f7.2,a)')'bridge length wall-wall concave:',
     &      bridgelen(icomp,4),' (khaki)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length ext floor down facing:',
     &      bridgelen(icomp,5)
        else
          write(outs,'(a,f7.2,a)')
     &      'bridge length ext floor down facing:',
     &      bridgelen(icomp,5),' (turquoise)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length door & window lintels ',
     &      bridgelen(icomp,6)
        else
          write(outs,'(a,f7.2,a)')
     &      'bridge length door & window lintels ',
     &      bridgelen(icomp,6),' (magenta)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length door & window sills ',
     &      bridgelen(icomp,7)
        else
          write(outs,'(a,f7.2,a)')'bridge length door & window sills ',
     &      bridgelen(icomp,7),' (firebrick)'
        endif
        call edisp(iuout,outs)
        if(MMOD.lt.8)then
          write(outs,'(a,f7.2)')'bridge length door & window jambs ',
     &      bridgelen(icomp,8)
        else
          write(outs,'(a,f7.2,a)')'bridge length door & window jambs ',
     &      bridgelen(icomp,8),' (cyan)'
        endif
        call edisp(iuout,outs)
        call edisp(iuout,
     &    'Use these suggested lengths. You can also add more types.')
      endif

C If in graphic mode draw the edges with thermal bridge attributes
C in a different colour for each type (to match colours listed above).
      if(MMOD.lt.8)then
        continue
      else
        CALL INLNST(1)
        itsnm=0
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
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
                COG1(1)=VCOORD(icomp,izva,1)
                COG1(2)=VCOORD(icomp,izva,2)
                COG1(3)=VCOORD(icomp,izva,3)
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),iix,iiy)
                COG1(1)=VCOORD(icomp,izvb,1)
                COG1(2)=VCOORD(icomp,izvb,2)
                COG1(3)=VCOORD(icomp,izvb,3)
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),iix2,iiy2)
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
              COG1(1)=VCOORD(icomp,izva,1)
              COG1(2)=VCOORD(icomp,izva,2)
              COG1(3)=VCOORD(icomp,izva,3)
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix,iiy)
              COG1(1)=VCOORD(icomp,izvb,1)
              COG1(2)=VCOORD(icomp,izvb,2)
              COG1(3)=VCOORD(icomp,izvb,3)
              CALL VECTRN(COG1,TEMAT,COG2,IER)
              CALL VECTRN(COG2,ETSMAT,COG3,IER)
              call u2pixel(COG3(1),COG3(2),iix2,iiy2)
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
C The exact spelling of phraselen and phrasepsi is important.
      phrasemenu(1) ='a roof-wall                    :'
      phraselen(1)  ='roof-wall '
      phrasepsi(1)  ='roof-wall '
      defpsi(1)=0.12
      lenpsi(1)=bridgelen(icomp,1)
      phrasemenu(2) ='b wall-ground floor            :'
      phraselen(2)  ='wall-ground floor '
      phrasepsi(2)  ='wall-ground floor '
      defpsi(2)=0.16
      lenpsi(2)=bridgelen(icomp,2)
      phrasemenu(3) ='c wall-wall (convex corner)    :'
      phraselen(3)  ='wall-wall (convex corner) '
      phrasepsi(3)  ='wall-wall (convex corner) '
      defpsi(3)=0.09
      lenpsi(3)=bridgelen(icomp,3)
      phrasemenu(4) ='d wall-wall (concave corner)   :'
      phraselen(4)  ='wall-wall (concave corner) '
      phrasepsi(4)  ='wall-wall (concave corner) '
      defpsi(4)= -0.09
      lenpsi(4)=bridgelen(icomp,4)
      phrasemenu(5) ='e wall-floor (not ground floor):'
      phraselen(5)  ='wall-floor (not ground floor) '
      phrasepsi(5)  ='wall-floor (not ground floor) '
      defpsi(5)=0.07
      lenpsi(5)=bridgelen(icomp,5)
      phrasemenu(6) ='f lintel above window or door  :'
      phraselen(6)  ='lintel above window or door '
      phrasepsi(6)  ='lintel above window or door '
      defpsi(6)=0.3
      lenpsi(6)=bridgelen(icomp,6)
      phrasemenu(7) ='g Sill below window            :'
      phraselen(7)  ='Sill below window '
      phrasepsi(7)  ='Sill below window '
      defpsi(7)=0.04
      lenpsi(7)=bridgelen(icomp,7)
      phrasemenu(8) ='h jamb at window or door       :'
      phraselen(8)  ='jamb at window or door '
      phrasepsi(8)  ='jamb at window or door '
      defpsi(8)=0.05
      lenpsi(8)=bridgelen(icomp,8)
      phrasemenu(9) ='i roof-wall                    :'
      phraselen(9)  ='roof-wall-mc'
      phrasepsi(9)  ='roof-wall-mc'
      defpsi(9)=0.28
      lenpsi(9)=bridgelen(icomp,1)
      phrasemenu(10)='j wall-ground floor            :'
      phraselen(10) ='wall-ground floor-mc'
      phrasepsi(10) ='wall-ground floor-mc'
      defpsi(10)=1.15
      lenpsi(10)=bridgelen(icomp,2)
      phrasemenu(11)='k wall-wall (corner)-          :'
      phraselen(11) ='wall-wall (corner)-mc'
      phrasepsi(11) ='wall-wall (corner)-mc'
      defpsi(11)=0.25
      lenpsi(11)=bridgelen(icomp,3)+bridgelen(icomp,4)
      phrasemenu(12)='l wall-floor (not ground floor):'
      phraselen(12) ='wall-floor (not ground floor)-mc'
      phrasepsi(12) ='wall-floor (not ground floor)-mc'
      defpsi(12)=0.07
      lenpsi(12)=bridgelen(icomp,5)
      phrasemenu(13)='m lintel above window or door  :'
      phraselen(13) ='lintel above window or door-mc'
      phrasepsi(13) ='lintel above window or door-mc'
      defpsi(13)=1.27
      lenpsi(13)=bridgelen(icomp,6)
      phrasemenu(14)='n Sill below window            :'
      phraselen(14) ='Sill below window-mc'
      phrasepsi(14) ='Sill below window-mc'
      defpsi(14)=1.27
      lenpsi(14)=bridgelen(icomp,7)
      phrasemenu(15)='o jamb at window or door       :'
      phraselen(15) ='jamb at window or door-mc'
      phrasepsi(15) ='jamb at window or door-mc'
      defpsi(15)=1.27
      lenpsi(15)=bridgelen(icomp,8)
      phrasemenu(16)='p user-defined                 :'
      phraselen(16) ='user-defined'
      phrasepsi(16) ='user-defined'
      defpsi(16)=0.00
      lenpsi(16)=1.0

C If the UK NCM facility is active ask the user whether facade
C has metal cladding or not and then set the relevant lengths.
C If in silent model for the so-called stripped model then do
C not involve the user, jump straight to the 10%.
      helptopic='linear_thermal_bridge'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(act(1:1).eq.'-')then
        if(nbrdg(icomp).eq.0)then
          if(ISBEM.EQ.2)then
            CALL EASKABCD('This zone:','(see help)',
     &        'uses metal cladding','no metal cladding','fixed %',
     &        'exit',INVT,nbhelp)
            if(INVT.eq.1)then
              continue
            elseif(INVT.eq.2)then
              continue
            elseif(INVT.eq.3)then
              continue
            elseif(INVT.eq.4)then
              return
            endif
          else
            CALL EASKABCD('This zone:','(see help)',
     &        'uses metal cladding','no metal cladding','fixed %',
     &        'exit',INVT,nbhelp)
            if(INVT.eq.1)then
              continue
            elseif(INVT.eq.2)then
              continue
            elseif(INVT.eq.3)then
              continue
            elseif(INVT.eq.4)then
              return
            endif
          endif

C Fill in the initial values for metal clad constructions.
          if(INVT.eq.1)then
            if(bridgelen(icomp,1).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=9
              psi(ICOMP,nbrdg(icomp))=defpsi(9)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,1)
            endif
            if(bridgelen(icomp,2).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=10
              psi(ICOMP,nbrdg(icomp))=defpsi(10)
               lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,2)
            endif
            if((bridgelen(icomp,3)+bridgelen(icomp,4)).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=11
              psi(ICOMP,nbrdg(icomp))=defpsi(11)
              lngth(ICOMP,nbrdg(icomp))=
     &          bridgelen(icomp,3)+bridgelen(icomp,4)
            endif
            if(bridgelen(icomp,5).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=12
              psi(ICOMP,nbrdg(icomp))=defpsi(12)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,5)
            endif
            if(bridgelen(icomp,6).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=13
              psi(ICOMP,nbrdg(icomp))=defpsi(13)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,6)
            endif
            if(bridgelen(icomp,7).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=14
              psi(ICOMP,nbrdg(icomp))=defpsi(14)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,7)
            endif
            if(bridgelen(icomp,8).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=15
              psi(ICOMP,nbrdg(icomp))=defpsi(15)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,8)
            endif
          elseif(INVT.eq.2)then

C Values for constructions without metal cladding.
            if(bridgelen(icomp,1).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=1
              psi(ICOMP,nbrdg(icomp))=defpsi(1)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,1)
            endif
            if(bridgelen(icomp,2).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=2
              psi(ICOMP,nbrdg(icomp))=defpsi(2)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,2)
            endif
            if(bridgelen(icomp,3).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=3
              psi(ICOMP,nbrdg(icomp))=defpsi(3)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,3)
            endif
            if(bridgelen(icomp,4).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=4
              psi(ICOMP,nbrdg(icomp))=defpsi(4)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,4)
            endif
            if(bridgelen(icomp,5).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=5
              psi(ICOMP,nbrdg(icomp))=defpsi(5)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,5)
            endif
            if(bridgelen(icomp,6).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=6
              psi(ICOMP,nbrdg(icomp))=defpsi(6)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,6)
            endif
            if(bridgelen(icomp,7).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=7
              psi(ICOMP,nbrdg(icomp))=defpsi(7)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,7)
            endif
            if(bridgelen(icomp,8).gt.0.1)then
              nbrdg(icomp)=nbrdg(icomp)+1
              ibrdg(ICOMP,nbrdg(icomp))=8
              psi(ICOMP,nbrdg(icomp))=defpsi(8)
              lngth(ICOMP,nbrdg(icomp))=bridgelen(icomp,8)
            endif
          endif
        endif
      endif

C Setup the editing phrases.
   33 TBRIDGE(1)=
     &'Junctions without metal cladding   psi (W/mK) length (m)'

C Loop through the first 8 variants seeing if any of the currentlist
C matches the type in each menu position. If not write text and place
C holder, otherwise write actual values.
      do 89 itbmenu=1,8
        if(nbrdg(icomp).gt.0)then
          foundit=.false.    
          do 90 itb=1,nbrdg(icomp)
            if(itbmenu.eq.ibrdg(icomp,itb))then
              WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)') 
     &          phrasemenu(itbmenu),
     &          psi(icomp,itb),lngth(icomp,itb)
              foundit=.true.
            endif
  90      continue
          if(.NOT.foundit)then
            WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)') phrasemenu(itbmenu),
     &        zeropsi,zerolen
          endif
        else
          WRITE(TBRIDGE(itbmenu+1),'(a,2F10.4)') phrasemenu(itbmenu),
     &      zeropsi,zerolen
        endif
  89  continue
      TBRIDGE(10)=
     & '      _________________________________________________ '
      TBRIDGE(11)=
     &'Junctions with metal cladding      psi (W/mK) length (m)'

      do 189 itbmenu=9,16
        if(nbrdg(icomp).gt.0)then
          foundit=.false.    
          do 190 itb=1,nbrdg(icomp)
            if(itbmenu.eq.ibrdg(icomp,itb))then
              WRITE(TBRIDGE(itbmenu+3),'(a,2F10.4)') 
     &          phrasemenu(itbmenu),
     &          psi(icomp,itb),lngth(icomp,itb)
              foundit=.true.
            endif
 190      continue
          if(.NOT.foundit)then
            WRITE(TBRIDGE(itbmenu+3),'(a,2F10.4)') phrasemenu(itbmenu),
     &        zeropsi,zerolen
          endif
        else
          WRITE(TBRIDGE(itbmenu+3),'(a,2F10.4)') phrasemenu(itbmenu),
     &      zeropsi,zerolen
        endif
 189  continue

      TBRIDGE(20)=
     &'     __________________________________________________ '
      TBRIDGE(21)='q include % of thermal bridges'
      TBRIDGE(22)='? help                         '
      TBRIDGE(23)='- exit to zone description     '
      ISDN=23

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

      IF(INODA.EQ.ISDN)THEN
        totheatloss(icomp) = 0.0
        do 1221 i=1,nbrdg(icomp)

C totheatloss is the total heat loss when all linear thermal bridges are added
          totheatloss(icomp) = totheatloss(icomp) +
     &                         (psi(ICOMP,i) * lngth(ICOMP,i))
 1221   continue
        totheatloss(icomp) = totheatloss(icomp) + losspercent(ICOMP)
        write(outs,*) 'total losses: ',totheatloss(icomp),' W/K'
        call edisp(iuout,outs)

        call edisp(iuout,
     &    'And remember SAVE this zone to record the t bridge data.')

C jump back to the main geometry menu
        return
      ELSEIF(INODA.EQ.ISDN-1)THEN 

C Produce help text for the menu.
        CALL PHELPD('thermal bridge menu',nbhelp,'-',0,0,IER) 

      ELSEIF(INODA.EQ.2)THEN

C Is any one of the current thermal bridges type 1?
        lookfor=1
        goto 42
      ELSEIF(INODA.EQ.3)THEN

C Is any one of the current thermal bridges type 2?
        lookfor=2
        goto 42
      ELSEIF(INODA.EQ.4)THEN

C Is any one of the current thermal bridges type 3?
        lookfor=3
        goto 42
      ELSEIF(INODA.EQ.5)THEN

C Is any one of the current thermal bridges type 4?
        lookfor=4
        goto 42
      ELSEIF(INODA.EQ.6)THEN

C Is any one of the current thermal bridges type 5?
        lookfor=5
        goto 42
      ELSEIF(INODA.EQ.7)THEN

C Is any one of the current thermal bridges type 6?
        lookfor=6
        goto 42
      ELSEIF(INODA.EQ.8)THEN

C Is any one of the current thermal bridges type 7?
        lookfor=7
        goto 42
      ELSEIF(INODA.EQ.9)THEN

C Is any one of the current thermal bridges type 8?
        lookfor=8
        goto 42
      ELSEIF(INODA.EQ.12)THEN

C Is any one of the current thermal bridges type 9?
        lookfor=9
        goto 42
      ELSEIF(INODA.EQ.13)THEN

C Is any one of the current thermal bridges type 10?
        lookfor=10
        goto 42
      ELSEIF(INODA.EQ.14)THEN

C Is any one of the current thermal bridges type 11?
        lookfor=11
        goto 42
      ELSEIF(INODA.EQ.15)THEN

C Is any one of the current thermal bridges type 12?
        lookfor=12
        goto 42
      ELSEIF(INODA.EQ.16)THEN

C Is any one of the current thermal bridges type 13?
        lookfor=13
        goto 42
      ELSEIF(INODA.EQ.17)THEN

C Is any one of the current thermal bridges type 14?
        lookfor=14
        goto 42
      ELSEIF(INODA.EQ.18)THEN

C Is any one of the current thermal bridges type 15?
        lookfor=15
        goto 42
      ELSEIF(INODA.EQ.19)THEN

C Is any one of the current thermal bridges type 16 (other type)?
        lookfor=16
        goto 42

      ELSEIF(INODA.EQ.ISDN-2)THEN
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
     &        defpsi(lookfor),' W/mK)'

            CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &        -2.0,'W',2.0,'W',defpsi(lookfor),'psi',IER,nbhelp)
            psi(ICOMP,ibj)=psiv

            if(ISBEM.EQ.2)then
              continue  ! Do not allow editing of length if UK NCM
            else
              write(msg,'(a,F7.4,a)')'(suggested length:',
     &          lenpsi(lookfor),' m)'
              if(lngth(ICOMP,ibj).lt.0.01)then
                lenv=lenpsi(lookfor)
              else
                lenv=lngth(ICOMP,ibj)
              endif
              CALL EASKR(lenv,phraselen(lookfor),msg,
     &          -0.1,'W',200.0,'W',0.0,'length',IER,nbhelp)
              lngth(ICOMP,ibj)=lenv
            endif
          endif
  31    continue
        if(.NOT.foundit)then
          nbrdg(icomp)=nbrdg(icomp)+1
          ibrdg(ICOMP,nbrdg(icomp))=lookfor
          psiv=0.0
          write(msg,'(a,F7.4,a)')'(accredited default psi:',
     &      defpsi(lookfor),' W/mK)'
          CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &      -2.0,'W',2.0,'W',defpsi(lookfor),'psi',IER,nbhelp)
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
     &    defpsi(lookfor),' W/mK)'
        CALL EASKR(psiv,phrasepsi(lookfor),msg,
     &    -2.0,'W',2.0,'W',defpsi(lookfor),'psi',IER,nbhelp)
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
C Review surface azimuth and elevation and update SVFC, SSVFC.
       subroutine updatesvfc(icomp,modgeo)
       
C It assumes that the zone geometry has just been read and that
C common blocks ?? are current. 

C It is passed the current zone index (izone) and returns
C modgeo=.true. if a change to common blocks G5 and G6 have
C been made.

#include "net_flow.h"
#include "building.h"
#include "geometry.h"

      integer IZSTOCN  ! for each zone:surface a pointer to connection index.
      COMMON/C24/IZSTOCN(MCOM,MS)

      logical MODGEO

C Loop through each surface and detect whether orientation of
C the surface has changed and needs to be recognised. 
      DO 41 I=1,NZSUR(icomp)
        ioc=IZSTOCN(icomp,i)
        if(SSPELV(ioc).GE.-1.5.AND.SSPELV(ioc).LE.1.5)then
          if(SVFC(I).NE.'VERT')then
            MODGEO=.TRUE.
            SVFC(I)='VERT'
            SSVFC(ioc)='VERT'
          endif
        elseif(SSPELV(ioc).GE.88.5.AND.SSPELV(ioc).LE.91.5)then
          if(SVFC(I).NE.'CEIL')then
            SVFC(I)='CEIL'
            SSVFC(ioc)='CEIL'
            MODGEO=.TRUE.
            izsceil(icomp)=I   ! identify as a ceiling
          endif
        elseif(SSPELV(ioc).GE.-91.5.AND.SSPELV(ioc).LE.-88.5)then
          if(SVFC(I).NE.'FLOR')then
            SVFC(I)='FLOR'
            SSVFC(ioc)='FLOR'
            MODGEO=.TRUE.
            izsfloor(icomp)=I  ! identify as a floor
          endif
        else
          if(SVFC(I).NE.'SLOP')then
            SVFC(I)='SLOP'
            SSVFC(ioc)='SLOP'
            MODGEO=.TRUE.
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

C Passed parameters
      integer icomp  ! zone to look in
      integer NVC    ! number of selected verticies
      real CX,CY,CZ  ! XYZ of selected verticies
      dimension CX(10),CY(10),CZ(10)
      integer IER    ! IER=0 OK, IER=1 problem.

      common/OUTIN/IUOUT,IUIN
      DIMENSION VERT(35)
      CHARACTER VERT*33,KEY*1,head*28
      character outs*124
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='CPVERT'  ! set for subroutine

      NVC=0    ! assume nothing selected yet

      do 42 iv=1,10  ! clear the return array
        CX(iv)=0.0
        CY(iv)=0.0
        CZ(iv)=0.0
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
          WRITE(VERT(M),14)KEY,L,VCOORD(ICOMP,L,1),VCOORD(ICOMP,L,2),
     &      VCOORD(ICOMP,L,3)
   14     FORMAT(A1,I4,3F9.3)
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
      VERT(M+3)  ='- exit this menu                 '

C Help text for this menu.
      helptopic='surface_vertex_copy'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      write(head,'(A,A12,A)')' Vertices in `',zname(ICOMP),'`'
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
          CX(NVC)=VCOORD(ICOMP,IFOC,1)
          CY(NVC)=VCOORD(ICOMP,IFOC,2)
          CZ(NVC)=VCOORD(ICOMP,IFOC,3)
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
