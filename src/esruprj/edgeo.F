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
C  EDINSUL: Edit zone insolation distribution.
C PICKSSUR  Selects surfaces for shading & insolation or copying.
C  EDSURA:  Edit zone surface attributes in common block G5
C  EDVERT:  Edit vertex attributes in common block G1.
C  EDVLIST: Edit surface-vertex list attributes in common block G1.
C  EZIPIN:  Reads zip format geometry data into esp-r geometry common
C           blocks.

C ************* EDZONE
C Control editing of zone attributes and allow this to be saved to a
C geometry file, ITRU unit number for user output, IER=0 OK. 
      SUBROUTINE EDZONE(ITRC,ICOMP,IER)
#include "net_flow.h"
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      common/SFIG/NSIGFIG
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/ground/grdtmp(12,mgrdp)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/PREC2/VOL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user has defined
C the floor area (one) or it was calculated (zero).
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      common/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      common/user/browse


      COMMON/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

      LOGICAL MODIFY,MODLEN,MODBND,browse,context
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL DOK,OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION IVALS(MS),ITEMS(28),ITEMSS(MS),IVALSS(MS),jvn1(MV)
      DIMENSION SALT(10)
      CHARACTER H*72,zname*12,zdesc*64,HEAD*20,SNAME*12,SALT*33,hold*32
      CHARACTER SOTHER*15,SMLCN*12,SVFC*4,SOTF*4,DESC*48,DESCRC*25
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER D12*12,T12*12,LTMP*72,GFILE*72
      CHARACTER ITEMS*33,ITEMSS*32,outs*124
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,T64*64,D64*64
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      CHARACTER LAPROB*72
      character MLCN*12,OTF*4,OPT*12,MLCSYM*12,SN*12,CUBN*6,act*1
      character SIGSTR*12,fs*1

      logical MODGEO,bound,attribok,zbzero,close,bndry,nameok,XST
      logical showother,updoth,firstin,unixok

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

C GFILE is the default file name for any geom file to be created,
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &      '.geo'
      else
        WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),fs,
     &     zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.geo'
      endif

C On entry to zone level determine its bounds for future comparison.
      write(outs,'(2a)')'PRJ: focus on ',zname(ICOMP)
      call tstamp('>',outs)

C Initialise logic to test for model contiguity changes.`
      call sumrchg(ICOMP,'i')
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
     &    'zone within this configuration! ','W')
        RETURN
      ENDIF

    3 INO=-4
      IER=0

C Find current derived zone geometric information, check location if user
C might have edited vertices or added a surface.
      call zgupdate(itrc,icomp,ier)

      OPQ=0.
      TRN=0.

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
        lastlist=0
        if(ibases(icomp,1).ne.0) lastlist=1
        if(ibases(icomp,2).ne.0) lastlist=2
        if(ibases(icomp,3).ne.0) lastlist=3
        if(ibases(icomp,4).ne.0) lastlist=4
        if(ibases(icomp,5).ne.0) lastlist=5
        if(ibases(icomp,6).ne.0) lastlist=6
        FLA=ZBASEA(ICOMP)
        ZBASEA(ICOMP)=0.0
      endif
      attribok=.true.
      DO 41 I=1,NSUR
        ioc=IZSTOCN(icomp,i)
        if(SOTHER(I)(1:4).eq.'UNKN')attribok=.false.
        if(SMLCN(I)(1:4).eq.'UNKN')attribok=.false.
        if(SSPELV(ioc).GE.-1.5.AND.SSPELV(ioc).LE.1.5)then
          if(SVFC(I).NE.'VERT')then
            MODGEO=.TRUE.
            SVFC(I)='VERT'
            SSVFC(ioc)='VERT'
          endif
        elseif(SSPELV(ioc).GE.88.5.AND.SSPELV(ioc).LE.91.5)then
          if(SVFC(I).NE.'CEIL')then
            SVFC(I)='CEIL'
            SSVFC(ioc)='VERT'
            MODGEO=.TRUE.
          endif
        elseif(SSPELV(ioc).GE.-91.5.AND.SSPELV(ioc).LE.-88.5)then
          if(SVFC(I).NE.'FLOR')then
            SVFC(I)='FLOR'
            SSVFC(ioc)='VERT'
            MODGEO=.TRUE.
          endif
        else
          if(SVFC(I).NE.'SLOP')then
            SVFC(I)='SLOP'
            SSVFC(ioc)='VERT'
            MODGEO=.TRUE.
          endif
        endif
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
     &         i.eq.ibases(icomp,5).or.I.eq.ibases(icomp,6))then
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
            else
              lastlist=lastlist+1
              if(lastlist.le.6)then
                IBASES(ICOMP,lastlist)=I
                ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
              endif
            endif
          endif
        else

C Check to see if a non `flor` surface has been included in ibases.
          if(IUZBASEA(icomp).eq.1)then
            continue
          elseif(IUZBASEA(icomp).eq.2)then
            continue
          else
            if(i.eq.ibases(icomp,1).or.i.eq.ibases(icomp,2).or.
     &         i.eq.ibases(icomp,3).or.i.eq.ibases(icomp,4).or.
     &         i.eq.ibases(icomp,5).or.I.eq.ibases(icomp,6))then
              ZBASEA(ICOMP)=ZBASEA(ICOMP)+SSNA(ioc)
            endif
          endif
        endif
        if(SOTF(I).EQ.'TRAN')then
          TRN=TRN+SSNA(ioc)
        else
          OPQ=OPQ+SSNA(ioc)
        endif
   41 CONTINUE

C If previously calculated base area, compare with current area calculation
C and inform user if it has changed.
      if(IUZBASEA(icomp).eq.0)then
        CALL ECLOSE(ZBASEA(ICOMP),FLA,0.001,close)
        if(.NOT.close)then
          call SIGFIG(ZBASEA(ICOMP),NSIGFIG,RNO,SIGSTR,LSTR)
          write(outs,'(3a)') 
     &        'Recalculated base/floor area is ',SIGSTR(1:LSTR),'m^2'
          call edisp(iuout,outs)
        endif
      endif
      call ckvert(0,icomp,bound,iub,inv,ier)

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
          endif
          write(SN,'(a12)')SNAME(icomp,is)
          if(showother.and.icoth.ne.0)then
            do 17 ii=1,nmlc
              if(SMLCN(is)(1:12).eq.DESC(ii)(1:12)) then
                updoth=.false.
                call parsemlcdesc(DESC(ii),MLCN,OTF,OPT,MLCSYM)
                if(MLCSYM(1:9).EQ.'SYMMETRIC')then
                  if(SSMLCN(icoth)(1:12).eq.MLCN(1:12))then
                    continue
                  elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                    SSMLCN(icoth)=MLCN
                    updoth=.true.
                  else
                    SSMLCN(icoth)=MLCN
                    updoth=.true.
                  endif
                elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                  write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &              ' has a nonsymmetric construction ',
     &              SMLCN(is)(1:lnblnk(SMLCN(is))),'.'
                  call edisp(iuout,outs)
                  write(outs,'(5a)') 'It faces ',
     &              SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &              SSMLCN(icoth)(1:12),' (which may not match).'
                  call edisp(iuout,outs)
                  updoth=.false.
                else
                  if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                    SSMLCN(icoth)=MLCSYM
                    updoth=.true.
                  elseif(SSMLCN(icoth)(1:12).eq.MLCSYM(1:12))then
                    continue
                  else
                    SSMLCN(icoth)=MLCSYM
                    updoth=.true.
                  endif
                endif
                if(updoth)then
                  CALL ESCZONE(ICOMP)
                  write(outs,'(3a)') 
     &              'Updating `other side` composition of ',
     &              SN(1:lnblnk(SN)),'...'
                  call edisp(iuout,outs)
                  CALL EGOMIN(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,0,
     &              iuout,IER)
                  SMLCN(IE2(ioc))=SSMLCN(icoth)
                  CALL EMKGEO(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),iuout,3,
     &              IER)
                  IF(IER.NE.0)CALL USRMSG(
     &              'There was a problem updating the other surface ',
     &              'attribute (i.e. the other geometry file)...','W')

C Read current zone back in and then update the configuration file.
                  CALL ERCZONE(ICOMP)
                  updoth=.false.
                endif
              endif
  17        continue
            call usrmsg(' ',' ','-')
          endif
 40     continue
      endif

      WRITE(ITEMS(1),'(A,1X,A12)')  'a name:',zname(ICOMP)
      WRITE(ITEMS(2),'(A,1X,A24)')  'b desc:',zdesc(ICOMP)(1:24)
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

      WRITE(ITEMS(ITEMC),'(A,1X,3F6.1)')'   origin @',X(1),Y(1),Z(1)
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
      ITEMS(ITEMC+5)='  _____________________________'
      WRITE(ITEMS(ITEMC+6),'(A,i3,a)')
     &                           'd vertex coordinates     (',NTV,')'
      WRITE(ITEMS(ITEMC+7),'(A,i3,a)')
     &                           'e surface list & edges   (',NSUR,')'
      ITEMS(ITEMC+8) ='f surface attributes           '
      ITEMS(ITEMC+9) ='  _____________________________'
      ITEMS(ITEMC+10)='g solar distribution           '
      ITEMS(ITEMC+11)='h solar obstruction            '
      ITEMS(ITEMC+12)='i rotation & transforms        '
      ITEMS(ITEMC+13)='j                              '
      ITEMS(ITEMC+14)='  _____________________________'
      ITEMS(ITEMC+15)='* list surface details         '
      ITEMS(ITEMC+16)='> save                         '
      IF(ITRC.EQ.0)THEN
        ITEMS(ITEMC+17)='t reporting >> silent  '
      ELSEIF(ITRC.EQ.1)THEN
        ITEMS(ITEMC+17)='t reporting >> summary '
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS(ITEMC+17)='t reporting >> detailed'
      ENDIF
      ITEMS(ITEMC+18)='? help                         '
      ITEMS(ITEMC+19)='- exit this menu               '

      WRITE(HEAD,'(A,I2,A)')' Zone ',ICOMP,' Composition'

C Do a bound check on the current zone and update display.
C Set all surfaces to standard line width.
C      write(6,*)nzg,nznog(1),nznog(2),nznog(3)
      CALL INLNST(1)
      itsnm=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(ITRC,IER)

C Test edge adjacencies and subsurfaces if it is the first time
C in to the zone or if the geometry has benn modified. 
C << test check parent and child by changing act='c' >>
C << NOTE: parent - child not yet held in geometry file >>
      if(.NOT.browse)then
        if(firstin.or.MODGEO)then
C          act = 'c'
          act = '-'
          call suredgeadj(0,act,icomp,ier)
          firstin=.false.
        endif
      endif

C Help text for this menu.
      H(1) ='This menu allows the attributes of a thermal zone to'
      H(2) ='be specified:'
      H(3) =' '
      H(4) ='* Zone name: label used in lists and reports.'
      H(5) ='* Base area: list of surfaces associate with the base.'
      H(6) =' '
      H(7) ='* Vertex list & coordinates: X, Y, Z coords of vertices'
      H(8) ='  in the zone and related functions (add|delete|copy as'
      H(9) ='  well as transform functions).  '
      H(10)=' '
      H(11)='* Surface list: topology (edge ordering) of each surface'
      H(12)='  within related functions (add|delete|copy as well as'
      H(13)='  transform functions.  New surfaces can be composed via'
      H(14)='  point and click or import from other zones as well as '
      H(15)='  manual vertex editing.'
      H(16)=' '
      H(17)='* Surface attributes: name, composition, opacity and'
      H(18)='  boundary conditions are required attributes. This is'
      H(19)='  the preferred place to define composition and opacity.'
      H(20)='  '
      H(21) ='* Solar insolation: as a default and an alternative to'
      H(22)='  running a time-series insolation distribution analysis'
      H(23)='  you may specify that solar radiation entering a zone'
      H(24)='  falls on particular surfaces or diffusly distributed.'
      H(25)=' '
      H(26)='* Obstructions: Shading patterns are generated via the'
      H(27)='  use of obstruction blocks, and their definition is'
      H(28)='  contained in this menu pick.'
      H(29)=' '
      H(30)='* Browse displays a table about each of the surfaces'
      H(31)='  in a zone.'

C Menu for zone geometry.
      CALL EMENU(HEAD,ITEMS,ITEMC+19,INO)
      IF(INO.EQ.ITEMC+19)THEN
        if(browse)return
        if(MODGEO)then
          DOK=.false.
          h(1)='If you exit without saving changes then some of '
          h(2)='your work may be lost. '
          CALL ASKOK(' Save recent changes to the zone composition',
     &    ' and surface attributes?',OK,DOK,2)
          LTMP=LGEOM(ICOMP)
          IF(OK)CALL EMKGEO(IFIL+2,LTMP,ICOMP,iuout,3,IER)

C If there is an associated flow node then update its volume and update the
C flow network.
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
        call sumrchg(ICOMP,'r')
        RETURN
      ELSEIF(INO.EQ.1)THEN

C Zone name, make sure it has no illegal characters.
        H(1)='The name of the zone is used both for descriptive and'
        H(2)='bookkeeping purposes.  Each name should be unique.'
        T12=' '
        D12='new_zone'
        CALL EASKS(T12,' ',' Descriptive name for zone? ',
     &    12,D12,'zone name',IER,2)
        if(T12.NE.' ')then
          call st2name(T12,zname(ICOMP))
          if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
            WRITE(GFILE,'(2a)') zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &        '.geo'
          else
            WRITE(GFILE,'(4a)') zonepth(1:lnblnk(zonepth)),'/',
     &        zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.geo'
          endif
          call tstamp('>','PRJ: edited zone name')
          MODIFY=.TRUE.
          MODGEO=.TRUE.
        endif
      ELSEIF(INO.EQ.2)THEN

C Zone description
        H(1)='The zone represents something so here is a chance'
        H(2)='to clarify the description and assumptions. '
        T64=zdesc(icomp)
        D64='<user has provided no description>'
  64    CALL EASKS(T64,' Description of zone (<64 char)? ',' ',
     &    64,D64,'zone descr',IER,2)
        if(T64(1:2).eq.'  ')goto 64
        zdesc(icomp)=T64
        call tstamp('>','PRJ: edited zone description')
        MODIFY=.TRUE.
        MODGEO=.TRUE.
      ELSEIF(INO.EQ.ITEMC+2)THEN

C Associate surfaces with base area, especially if there are more
C than one surface with orientation 'FLOR'.
C Present list of surfaces and allow user to select up to 6. Number
C of surfaces associated with base is derived from non-zero values
C in IBASES. If all 6 slots are 0 then floor area is user defined.
        INPICK=6
        ij=0
        DO 20 I=1,NSUR
          ITEMSS(I)=' '
          if(I.eq.IBASES(ICOMP,1).or.I.eq.IBASES(ICOMP,2).or.
     &       I.eq.IBASES(ICOMP,3).or.I.eq.IBASES(ICOMP,4).or.
     &       I.eq.IBASES(ICOMP,5).or.I.eq.IBASES(ICOMP,6))then
            WRITE(ITEMSS(I),'(A,1x,A,1x,A,A)')SNAME(ICOMP,I),SMLCN(I),
     &        SVFC(I),'*'
          else
            WRITE(ITEMSS(I),'(A,1x,A,1x,A)')SNAME(ICOMP,I),SMLCN(I),
     &        SVFC(I)
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
          H(1)='These surfaces are associated with base area because'
          H(2)='they had attributes of `flor` or were previously '
          H(3)='picked by the user. You can edit this list or select'
          H(4)='none and then edit the value.'
        elseif(IUZBASEA(icomp).eq.1)then
          write(outs,'(a,f9.3,a)') 
     &      'Surfaces associated with base (currently user edited ',
     &      ZBASEA(ICOMP),'m^2)'
          H(1)='No surfaces are associated with base area because'
          H(2)='the user manually edited the floor area value.'
          H(3)='You can select surfaces (and floor area will be'
          H(4)='calculated) or select none and then edit the value.'
        elseif(IUZBASEA(icomp).eq.2)then
          write(outs,'(a,f9.3,a)') 
     &      'Surfaces associated with base (currently user list ',
     &      ZBASEA(ICOMP),'m^2)'
          H(1)='A subset of surfaces are associated with base area'
          H(2)='(user excluded some FLOR surfaces).'
          H(3)='You can select surfaces (and floor area will be'
          H(4)='calculated) or select none and then edit the value.'
        endif
        CALL EPICKS(INPICK,IVALS,outs,
     &    '(to manually edit area select none)',32,NSUR,ITEMSS,
     &    'Surf name composition orient',IER,4)
        do 221 i=1,6
          IBASES(ICOMP,i)=0
 221    continue
        if(inpick.eq.0)then
          CALL EASKR(ZBASEA(ICOMP),' ',
     &       ' Confirm area of zone base (m^2) ? ',
     &       0.1,'F',99999.,'F',FLA,'base area',IER,2)
          IUZBASEA(icomp)=1
          MODGEO=.TRUE.
          goto 3 
        else
          ZBASEA(ICOMP)=0.0
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
        call tstamp('>','PRJ: enter zone vertices')
        CALL EDVERT(ITRC,iuout,ICOMP,MODGEO,IER)
        if(MODGEO)then
          DOK=.true.
          h(1)='Changes in vertices may be lost if you continue working'
          h(2)='without saving the data. '
          CALL ASKOK(' Save recent vertex changes?',' ',OK,DOK,2)
          LTMP=LGEOM(ICOMP)
          if(OK)then
            CALL EMKGEO(IFIL+2,LTMP,ICOMP,iuout,3,IER)
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
        MODIFY=.TRUE.
      ELSEIF(INO.EQ.ITEMC+7)THEN

C Vertex-surface association editing.
        call tstamp('>','PRJ: enter zone-surface topology')
        CALL EDVLIST(ITRC,iuout,ICOMP,IER)
        MODGEO=.TRUE.
      ELSEIF(INO.EQ.ITEMC+8)THEN

C Surface attributes.  Display a selection of surfaces available in the
C zone. Trap option of multiple surface attribution. 
   42   IS=0
        CALL EASKSUR(ICOMP,IS,'M','Select surface(s) to attribute.',
     &    ' ',IER)
        if(IS.lt.0)then

C Remind user of current surface boundary attributes.
          call tstamp('>','PRJ: enter global surface attribution')
          context=.true.
          if(ITRC.gt.1)CALL SURINFO(ICOMP,iuout,context)
          CALL EASKABCD(' Attribution choices:',' ','surface name',
     &     'composition','boundary conditions','return',IRT,2)
          if(IRT.eq.2)then
            CALL EPKMLC(ISEL,'Select one of the constructions',
     &        'or exit.',IER)
          elseif(IRT.eq.3)then
            SALT(1)='External                         '
            SALT(2)='Dynamic (similar to this zone)   '
            SALT(3)='Constant (@specified temperature)'
            SALT(4)=' - - (Another zone - see help)   '
            SALT(5)='Ground (standard monthly profile)'
            if(NGRDP.gt.0)then
              SALT(6)='Ground (user defined profile)    '
            else
              SALT(6)='Ground (no user defined profiles)'
            endif
            SALT(7)='Adiabatic                        '
            SALT(8)=' - - BASESIMP - (see help) '
            SALT(9)='CEN 13791 partition '
            SALT(10)='Unknown (at this time)           '

            H(1)='The boundary conditions at the other face of this'
            H(2)='surface is specified via the selection of one of '
            H(3)='these choices.  The default is External.'
            H(4)=' '
            H(5)='Choices marked - - - must be attributed surface'
            H(6)='by surface. '
            IX=1
            CALL EPICKS(IX,IVALSS,' ','The other side faces:',
     &        33,10,SALT,'surface boundary options',IER,6)
            IOS=IVALSS(1)
            if(IOS.EQ.2)then
              H(1)='A similar (type 1) connection can include an offset'
              H(2)='temperature and radiation (W/m^2) from that of the'
              H(3)='current zone. For example, if the adjacent space'
              H(4)='which has not been geometrically defined tends to '
              H(5)='be 2degC cooler then use a -2 offset. '
              H(6)=' '
              H(7)='(units are in whole degC and whole W) '
              VALT=0.0
              VALW=0.0
              CALL EASKR(VALT,' Offset temperature? [default=0.0]',
     &          '(see help) ',-99.,'F',99.,'F',0.0,'offset temp',IER,7)
              CALL EASKR(VALW,' Offset radiation source (W/m^2)?',
     &          '(see help) ',0.0,'W',99999.,'W',0.0,'offset rad',IER,7)
            elseif(IOS.EQ.3)then
              H(1)='A constant (type 2) connection requires that a '
              H(2)='constant temperature and radiation (W/m^2).'
              CALL EASKR(VALT,' ',' Constant temperature? ',
     &         -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
              CALL EASKR(VALW,' ',' Constant radiation source (W/m^2)?',
     &        0.0,'W',99999.,'W',0.0,'adjacent radiation source',IER,2)
            elseif(IOS.EQ.5)then

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
              CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
              CALL EDISP(iuout,'id Jan, Feb, Mar, Apr, May, Jun...')
              do 21 igrdp=1,mgrdp
                CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
                if(.NOT.close)then
                 WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
                 call edisp(iuout,outs)
                endif
 21           continue
              H(1)='A ground connection allows either:'
              H(2)='> 0 a connection to a standard profile in'
              H(3)='    which case the profile number is required.'
              H(4)='= 0 a connection to a user defined profile in'
              H(5)='    which case a 0 followed by a profile number'
              H(6)='    is required.'
              H(7)='-3  a connection to a 3D ground model.'
              CALL EASKI(IIC2,' Monthly (standard) profile index? ',
     &                  ' ',1,'F',12,'F',1,'ground profile',IER,7)
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
                H(1)='One of the user defined profile numbers is'
                H(2)='required here.'
                CALL EASKI(IIE2,' User defined ground profile index ?',
     &            ' ',0,'F',9,'F',1,'user defined profile',IER,2)
              else
                call usrmsg(
     &          'No user defined ground profiles have been found.',
     &          'Go to `model context` to define these.','W')
                goto 3
              endif
            endif
          elseif(IRT.eq.4)then
            goto 3
          endif

C Present list of surfaces and allow user to select up to all of them.
          INPICK=NSUR
          DO 10 I=1,NSUR
            ITEMSS(I)=' '
            if(IRT.eq.1)then
              WRITE(ITEMSS(I),'(A)')SNAME(ICOMP,I)
            elseif(IRT.eq.2)then
              WRITE(ITEMSS(I),'(A,2x,A)')SNAME(ICOMP,I),SMLCN(I)
            elseif(IRT.eq.3)then
              WRITE(ITEMSS(I),'(A,2x,A)')SNAME(ICOMP,I),SOTHER(I)
            endif
   10     CONTINUE
          if(IRT.eq.1)then
            H(1)='Select the surfaces for which the name is to'
            H(2)='be defined.'
            CALL EPICKS(INPICK,IVALS,' ',' ',15,NSUR,ITEMSS,
     &          'Surface name',IER,2)
          elseif(IRT.eq.2)then
            H(1)='Select the surfaces for which the composition '
            H(2)='is to be defined.'
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name   composition',IER,2)
          elseif(IRT.eq.3)then
            H(1)='Select the surfaces for which the facing environment'
            H(2)='is to be defined.'
            CALL EPICKS(INPICK,IVALS,' ',' ',32,NSUR,ITEMSS,
     &          'Surface name    facing',IER,2)
          endif
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
              CALL ADJVIEW(ITRC,IER)
              T12='  '
              write(D12,'(a12)')SNAME(ICOMP,ISS)
              H(1)='Use this attribute to inform about the nature of '
              H(2)='the surface (make it easy for others to understand)'
              write (outs,'(4a)')'Surface name (default=',D12,') for ',
     &                           'highlighted surface'
 52           CALL EASKS(T12,outs,'(unique word <=12 char):',
     &                    12,D12,'surface name',IER,2)
              call st2name(T12,D12)
              call snamdup(D12,icomp,ISS,nameok)
              if(nameok)then
                SNAME(ICOMP,ISS)=D12
                SSNAME(ioc)=D12
              else
                call edisp(iuout,
     &                  'Surface name is a duplicate of an existing')
                call edisp(iuout,
     &                  'surface. Please supply a different name.')
                goto 52
              endif

C Reset all line widths to normal and redraw (if last surface in list).
              if (i.eq.inpick) then
                CALL INLNST(1)
                MODIFY=.TRUE.
                MODGEO=.TRUE.
                CALL ADJVIEW(ITRC,IER)
              endif
            elseif(IRT.eq.2.and.ISEL.GT.0)then

C Associate mlc with each selected surface.
              WRITE(SMLCN(ISS),'(A12)')DESC(ISEL)(1:12)
              WRITE(SSMLCN(ioc),'(A12)')DESC(ISEL)(1:12)
              IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(ISS)='TRAN'
              IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(ISS)='OPAQ'
              SSOTF(ioc)=SOTF(iss)
              call warnmod(ICOMP,'sat')

C If this is a partition loop and find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.
              write(SN,'(a12)')SNAME(ICOMP,ISS)
              if(showother.and.icoth.ne.0)then
                do 7 ii=1,nmlc
                  if(SMLCN(iss)(1:12).eq.DESC(ii)(1:12)) then
                    updoth=.false.
                    call parsemlcdesc(DESC(ii),MLCN,OTF,OPT,MLCSYM)
                    if(MLCSYM(1:9).EQ.'SYMMETRIC')then
                      if(SSMLCN(icoth)(1:12).eq.MLCN(1:12))then
                        continue
                      elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                        SSMLCN(icoth)=MLCN
                        updoth=.true.
                      else
                        SSMLCN(icoth)=MLCN
                        updoth=.true.
                      endif
                    elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                      write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &                  ' has a nonsymmetric construction ',
     &                  SMLCN(is)(1:lnblnk(SMLCN(is))),'.'
                      call edisp(iuout,outs)
                      write(outs,'(5a)') 'It faces ',
     &                SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &                SSMLCN(icoth)(1:12),' (which may not match).'
                      call edisp(iuout,outs)
                      call edisp(iuout,
     &               'Please check that one construction has inverted')
                      call edisp(iuout,
     &               'layers or revise the construction database.')
                      updoth=.false.
                    else
                      if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                        SSMLCN(icoth)=MLCSYM
                        updoth=.true.
                      elseif(SSMLCN(icoth)(1:12).eq.MLCSYM(1:12))then
                        continue
                      else
                        SSMLCN(icoth)=MLCSYM
                        updoth=.true.
                      endif
                    endif
                    if(updoth)then
                      CALL ESCZONE(ICOMP)
                    call usrmsg(
     &                ' Updating other side construction (remember to',
     &                ' update the other side construction file).','P')
                    CALL EGOMIN(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,0,
     &                iuout,IER)
                    SMLCN(IE2(ioc))=SSMLCN(icoth)
                    CALL EMKGEO(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),iuout,
     &                3,IER)
                    IF(IER.NE.0)CALL USRMSG(
     &               'Problem updating other surface attribute... ',
     &               '(could not write the other geometry file).','W')

C Read current zone back in and then update the configuration file.
                    CALL ERCZONE(ICOMP)
                    call usrmsg(
     &             ' Updating other side construction...done.',' ','-')
                    updoth=.false.
                  endif
                endif
   7          continue
            endif
            elseif(IRT.eq.3.and.IX.gt.0)then

C Determine the match in the system topology and update it. 
              CALL SURADJ(ICOMP,ISS,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ioc.ne.ic)write(6,*) 'edgeo mismatch ic ioc ',ic,ioc
              IC1(IC)=ICOMP
              IE1(IC)=ISS
              IF(IOS.EQ.1)THEN
                SOTHER(ISS)='EXTERIOR'
                SSOTHER(IC)='EXTERIOR'
                ICT(IC)=0
                IC2(IC)=0
                IE2(IC)=0
              ELSEIF(IOS.EQ.2)THEN
                SOTHER(ISS)='SIMILAR'
                SSOTHER(IC)='SIMILAR'
                ICT(IC)=1
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.3)THEN
                SOTHER(ISS)='CONSTANT'
                SSOTHER(IC)='CONSTANT'
                ICT(IC)=2
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.5)THEN
                SOTHER(ISS)='GROUND'
                SSOTHER(IC)='GROUND'
                ICT(IC)=4
                IC2(IC)=iic2
                IE2(IC)=0
              ELSEIF(IOS.EQ.6)THEN
                SOTHER(ISS)='GROUND'
                SSOTHER(IC)='GROUND'
                ICT(IC)=4
                IC2(IC)=0
                IE2(IC)=iie2
              ELSEIF(IOS.EQ.7)THEN
                SOTHER(ISS)='ADIABATIC'
                SSOTHER(IC)='ADIABATIC'
                ICT(IC)=5
                IE2(IC)=0
                IC2(IC)=0
              ELSEIF(IOS.EQ.8)THEN
                SOTHER(ISS)='BASESIMP'
                SSOTHER(IC)='BASESIMP'
              ELSEIF(IOS.EQ.9)THEN
                SOTHER(ISS)='IDENT_CEN'
                SSOTHER(IC)='IDENT_CEN'
                ICT(IC)=7
                IC2(IC)=INT(VALT)
                IE2(IC)=INT(VALW)
              ELSEIF(IOS.EQ.10)THEN
C Reset to zeros << actually should have a different ict
C to represent UNKNOWN >>
                SOTHER(ISS)='UNKNOWN'
                SSOTHER(IC)='UNKNOWN'
                ICT(IC)=0
                IE2(IC)=0
                IC2(IC)=0
              ENDIF
              call warnmod(ICOMP,'sat')
              MODGEO=.TRUE.
            endif
  11      continue

C Finally update the system configuration file if not browsing.
          if(cfgok.and.(.NOT.browse))then
            CALL EMKCFG('-',IER)
            CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            call sumrchg(ICOMP,'r')
          endif
          goto 42
        elseif(IS.gt.0)then

C Attribute a single surface, set flag to redraw.
          call tstamp('>','PRJ: enter single surface attribution')
          CALL EDSURA(ICOMP,IS,ITRC,MODGEO,IER)
          MODIFY=.TRUE.
          goto 42
        endif
      ELSEIF(INO.EQ.ITEMC+10)THEN

C Edit zone insolation distribution.
        call tstamp('>','PRJ: enter zone insolation distribution')
        CALL EDINSUL(ICOMP,IER)
        MODGEO=.TRUE.
      ELSEIF(INO.EQ.ITEMC+11)THEN

C Zone obstructions. If any unsaved zone geometry changes and not
C browsing update the geometry file before entering the obstruction
C editing facility.
        if(MODGEO)then
          if(cfgok.and.(.NOT.browse))then
            CALL EMKCFG('-',IER)
            CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            call sumrchg(ICOMP,'r')
          endif
          MODGEO=.false.
        endif
        call tstamp('>','PRJ: enter zone obstructions')
        h(1)='You can define shading obstructions by either supplying'
        h(2)='and origin and dimensions or clicking on a bitmap. '
        call easkabc('Shading obstruction options:',' ',
     &    'dimensional input','click on bitmap','continue',IW,2)
        if(iw.eq.1)then
          CALL EDOBS(ITRC,iuout,ICOMP,IER)
        elseif(iw.eq.2)then
#ifdef X11
          call edisp(iuout,
     &      'Toggle to obstruction mode after you have set origin')
          call edisp(iuout,'and scale. ')
          call clickonbitmap(0,ier)
          call edisp(iuout,'You may further edit the obstructions...')
          CALL EDOBS(ITRC,iuout,ICOMP,IER)
#else
          call usrmsg('The clickonbitmap facility is not yet working',
     &      'with new graphic library.','W')
#endif
        endif
      ELSEIF(INO.EQ.ITEMC+12)THEN

C Zone rotations and transforms.
        H(1)='This set of geometric transforms and rotations are'
        H(2)='applied to all surfaces in the zone. '
        CALL EASKABCD(' Adjust location of current zone:',' ',
     &    'rotate zone','transform X,Y or Z','mirror','continue',IW,2)
        if(IW.eq.1)then
          H(1)='The rotation is applied around vertex 1 and positive'
          H(2)='is anticlockwise. '
          VAL=0.
          CALL EASKR(VAL,' ',' Rotation (deg + = anticlockwise) ? ',
     &      -359.0,'W',359.0,'W',0.0,'rotation',IER,2)
          if(VAL.LT.-.01.OR.VAL.GT..01)then

C Rotation choices.
            call tstamp('>','PRJ: do zone rotation')
            H(1)='The traditional rotation is about vertex 1 of the'
            H(2)='zone with a positive angle being anticlockwise. '
            H(3)='You may wish to rotate a zone about the site '
            H(4)='origin X=0., Y=0. or at a user specified point. '
            CALL EASKABCD(' Rotation choices :',' ',
     &        'Vert 1 of zone','Site origin',
     &        'User specified X & Y','cancel  ?',IRT,4)
            if(IRT.eq.1)then
              x1=x(1)
              y1=y(1)
              CALL ESCROT(VAL,x1,y1)
            elseif(IRT.EQ.2)THEN
              x1=0.
              y1=0.
              CALL ESCROT(VAL,x1,y1)
            elseif(IRT.EQ.3)THEN
              H(1)='Point is in the site coordinate system.'
              x1=0.
              CALL EASKR(x1,' ',' X point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'x point',IER,1)
              y1=0.
              CALL EASKR(y1,' ',' Y point (metres) ? ',
     &          0.0,'-',0.0,'-',0.0,'y point',IER,1)
              CALL ESCROT(VAL,x1,y1)
              call usrmsg(' ',' ','-')
            endif
            ZBFLG(ICOMP)=0.
            CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Warn user if there is a flow node associated with this zone that
C zone rotation may require boundary nodes to be updated.
            if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
              call usrmsg(
     &          'Rotating a zone may require updating of flow network ',
     &          'connections to wind boundary nodes. Please check!','W')
            endif
            call warnmod(ICOMP,'str')

C Rotate any MRT sensors associated with this zone.
            if (IVF(ICOMP).eq.1) then
              NCUB = 0
              CALL ERMRT(ITRC,ITRU,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
              if (NCUB.gt.0) then
                dok=.true.
                h(1)='You have rotated the zone. You probably also '
                h(2)='need to rotate the MRT sensors.'
                call askok('Apply this rotation to MRT sensors',
     &                     'associated with this zone? ',OK,dok,2)
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
  76              continue
                  CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                        NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
                endif
              endif
            endif

C Rotate any obstructions associated with this zone.
            if(iobs(icomp).eq.1)then
              dok=.true.
              h(1)='You have already rotated the zone. Probably '
              h(2)='you will want to rotate associated obstructions.'
              call askok('Apply this rotation to obstructions',
     &                   'associated with this zone? ',ok,dok,2)
              if(ok)then
                CALL EGOMST(IFIL+2,ZOBS(ICOMP),0,ITRC,ITRU,IER)
                PI = 4.0 * ATAN(1.0)
                A=-VAL*PI/180.
                CA=COS(A)
                SA=SIN(A)
                do 86 ij=1,NB
                  XXX=XO(ij)-X1
                  YYY=YO(ij)-Y1
                  XR=XXX*CA+YYY*SA
                  YR=YYY*CA-XXX*SA
                  XO(ij)=XR+X1
                  YO(ij)=YR+Y1
                  BANG(ij)=BANG(ij)+VAL
  86            continue
                CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
              endif
            endif
          endif
        elseif(IW.eq.2)then
          call tstamp('>','PRJ: do zone translation')
          H(1)='Transform shifts the zone by a given amount in'
          H(2)='each axis.'
          hold = ' 0.0  0.0  0.0'
  43      CALL EASKS(HOLD,'Transform (X Y Z metres):',' ',32,
     &      ' 0. 0. 0.','transforms XYZ',IER,2)
          K=0
          CALL EGETWR(HOLD,K,VALX,-99.,99.,'W','X tr',IER)
          CALL EGETWR(HOLD,K,VALY,-99.,99.,'W','Y tr',IER)
          CALL EGETWR(HOLD,K,VALZ,-99.,99.,'W','Z tr',IER)
          if(ier.ne.0)goto 43
          DO 62 I=1,NTV
            X(I)=X(I)+VALX
            Y(I)=Y(I)+VALY
            Z(I)=Z(I)+VALZ
   62     continue
          ZBFLG(ICOMP)=0.
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &      'Transforming a zone may require updating of flow network',
     &      'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')

C Transform any MRT sensors associated with this zone.
          if (IVF(ICOMP).eq.1) then
            NCUB = 0
            CALL ERMRT(ITRC,ITRU,IFIL+2,LVIEW(ICOMP),ICOMP,IER)
            if (NCUB.gt.0) then
              dok=.true.
              h(1)='You have transformed the zone. You probably also '
              h(2)='want to transform its MRT sensors.'
              call askok('Apply this transform to MRT sensors',
     &                   'associated with this zone? ',ok,dok,2)
              if (OK) then
                do 73 ij=1,NCUB
                  XOC(ij)=XOC(ij)+VALX
                  YOC(ij)=YOC(ij)+VALY
                  ZOC(ij)=ZOC(ij)+VALZ
  73            continue
                CALL EMKMRT(LVIEW(ICOMP),LGEOM(ICOMP),
     &                      NZSUR(ICOMP),IFIL+2,ICOMP,'v',IER)
              endif
            endif
          endif

C Transform any obstructions associated with this zone.
          if(iobs(icomp).eq.1)then
            dok=.true.
            h(1)='You already transformed the zone. You probably also'
            h(2)='want to transform the associated obstructions. '
            call askok('Apply this transform to obstructions',
     &                 'associated with this zone? ',ok,dok,2)
            if(ok)then
              CALL EGOMST(IFIL+2,ZOBS(ICOMP),0,ITRC,ITRU,IER)
              do 63 ij=1,NB
                XO(ij)=XO(ij)+VALX
                YO(ij)=YO(ij)+VALY
                ZO(ij)=ZO(ij)+VALZ
  63          continue
              CALL MKGOMST(IFIL+2,ZOBS(ICOMP),ICOMP,IER)
            endif
          endif
        elseif(IW.eq.3)then

C Mirror a zone (cardinal directions only). Ask for NS SN EW WE, then
C the mirror point on axis, warn user to save first?? Also invert
C each surface.
          H(1)='The mirror facility flips one axis.  You specify '
          H(2)='which axis and the point along that axis where the '
          H(3)='mirror is to occur. A positive Y mirror is shown. '
          H(4)='A negative mirror is top to bottom. '
          H(5)= '             +Y'
          H(6)= '            |  .......'
          H(7)= '            |  :     :.......'
          H(8)= '            |  :     /\\     :'
          H(9)= '            |  :.....||.....:'
          H(10)='           -|- - - - || - - - - - -mirror line'
          H(11)='            |  ......||......'
          H(12)='            |  :     ||     :'
          H(13)='            |  :     .......:'
          H(14)='            |  :.....:'
          H(15)=' -X_________|______________________ +X '
          H(16)='            |'
          H(17)='            | -Y    Positive Y mirror'
          H(18)='  '
          H(19)='  '
          H(20)='  '
          H(21)='A positive X mirror is shown. The negative goes from'
          H(22)='right to left. '
          H(23)=' '
          H(24)= '             +Y'
          H(25)= '          |            mirror line'
          H(26)= '          |           |'
          H(27)= '          | ......... | ..........'
          H(28)= '          | :     ==========>    :'
          H(29)= '          | :     ..: | :..      :'
          H(30)='          | :.....:   |   :......:'
          H(31)=' -X_______|___________|__________ +X '
          H(32)='          |'
          H(33)='          | -Y    Positive X mirror'
          H(34)='                  '
          H(35)='                  '
          CALL PHELPD('axis mirror',35,'-',0,0,IER)
          h(1)='consult the previous help display'
          CALL EASKATOG('Mirror along which axis & direction (+-):',' ',
     &      'Y (+)','Y (-)','X (+)','X (-)','continue',' ',' ',IWM,1)
          if(IWM.eq.1.or.IWM.eq.2)then

C Mirror along Y axis, either positive or negative.
            call tstamp('>','PRJ: do zone mirror')
            H(1)='Please ensure the mirror point is at or outside '
            H(2)='of the zone. For +mirror the point is above the'
            H(3)='object.  For -mirror the point is below it. '
            CALL EASKR(VALM,'Mirror point on the Y axis:',' ',
     &           -50.0,'W',50.0,'W',0.0,'y mirror',IER,3)
            do 342 iwmv=1,NTV
              if(IWM.eq.1)then
                VALY=VALM-Y(iwmv)
                Y(iwmv)=VALM+VALY
              elseif(IWM.eq.2)then
                VALY=Y(iwmv)-VALM
                Y(iwmv)=VALM-VALY
              endif
 342        continue
          elseif(IWM.eq.3.or.IWM.eq.4)then

C Mirror along X axis, either positive or negative.
            call tstamp('>','PRJ: do zone mirror')
            H(1)='Please ensure the mirror point is at or outside of'
            H(2)='the zone. For +mirror the point is to the right of'
            H(3)='the zone.  For -mirror the point is on the left.'
            CALL EASKR(VALM,'Mirror point on the X axis:',' ',
     &           -50.0,'W',50.0,'W',0.0,'y mirror',IER,2)
            do 343 iwmv=1,NTV
              if(IWM.eq.3)then
                VALX=VALM-X(iwmv)
                X(iwmv)=VALM+VALX
              elseif(IWM.eq.4)then
                VALX=X(iwmv)-VALM
                X(iwmv)=VALM-VALX
              endif
 343        continue
          elseif(IWM.eq.5)then
            goto 3
          endif

C Now invert the ordering of the surfaces.
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
          ZBFLG(ICOMP)=0.
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Warn user if there is a flow node associated with this zone that
C zone mirroring may require boundary nodes to be updated.
          if(IAIRN.ge.1.and.ICAAS(ICOMP).ne.0)then
            call usrmsg(
     &        'Mirroring a zone may require updating of flow network ',
     &        'connections to wind boundary nodes. Please check!','W')
          endif
          call warnmod(ICOMP,'str')
        elseif(IW.eq.4)then
          goto 3
        endif
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODGEO=.TRUE.
      ELSEIF(INO.EQ.ITEMC+13)THEN

C << future position of linear thermophysical properties >>
      ELSEIF(INO.EQ.ITEMC+15)THEN

C Surface summary: print header, followed by surface information.
        call tstamp('>','PRJ: list zone surface summary')
        context=.true.
        CALL SURINFO(ICOMP,iuout,context)
        bndry=.true.
        DO 1243 IS=1,NSUR
         icc=IZSTOCN(icomp,is)
         if(ICT(icc).eq.0.and.SSOTHER(icc)(1:5).ne.'EXTER')bndry=.false.
         if(ICT(icc).eq.1.and.SSOTHER(icc)(1:5).ne.'SIMIL')bndry=.false.
         if(ICT(icc).eq.2.and.SSOTHER(icc)(1:5).ne.'CONST')bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc)(1:5).eq.'CONST')bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc)(1:5).eq.'SIMIL')bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc)(1:5).eq.'EXTER')bndry=.false.
         if(ICT(icc).eq.3.and.SSOTHER(icc)(1:5).eq.'GROUN')bndry=.false.
         if(ICT(icc).eq.4.and.SSOTHER(icc)(1:5).ne.'GROUN')bndry=.false.
         if(ICT(icc).eq.5.and.SSOTHER(icc)(1:5).ne.'ADIAB')bndry=.false.
         if(ICT(icc).eq.6.and.SSOTHER(icc)(1:5).ne.'BASES')bndry=.false.
 1243   continue
        if(.NOT.bndry)then
          call edisp(iuout,
     &   'Some surface boundary attributions did not match the master')
          call edisp(iuout,
     &   'connections list. A topology update may be required.')
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
            h(1)='In addition to surface attributes you can view the'
            h(2)='thermophysical properties of the zone. '
           call askok('Also view thermophysical details?',' ',ok,dok,2)
            if(ok)then
              CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)
              CALL CONINF(ICOMP,0,iuout)
            endif
          endif
        endif
      ELSEIF(INO.EQ.ITEMC+16)THEN

C Ask for name of file to put updated information into.
        if(browse)then
          call usrmsg(' In browse mode, not saved... ',' ','W')
          goto 3
        endif
        call tstamp('>','PRJ: update zone geometry')
        H(1)='The current geometric description will be placed into'
        H(2)='a file in the neutral GEN format.'
        LTMP=LGEOM(ICOMP)
        CALL EASKS(LTMP,' Zone geometry file ? ',' ',
     &    72,GFILE,'geom file',IER,2)
        CALL EMKGEO(IFIL+2,LTMP,ICOMP,iuout,4,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' Problem creating file...','W')
          INO=-4
          GOTO 3
        ENDIF
        LGEOM(ICOMP)=LTMP
        CALL EMKCFG('-',IER)
        MODGEO=.FALSE.
      ELSEIF(INO.EQ.ITEMC+17)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
      ELSEIF(INO.EQ.ITEMC+18)THEN

C Produce help text for the menu.
        CALL PHELPD('geometry section',31,'zone_geo_ed ',0,0,IER)
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
      common/pophelp/h(60)
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)
      DIMENSION IALT(8),IVAL(8)
      CHARACTER IALT*29,H*72

      IER=0
C Insolation, present a list of current default insolation choices.
      H(1)='In addition to an optional dynamic analysis of shading'
      H(2)='and insolation distribution via `ish` which pro-'
      H(3)='duces a shading/insolation database, the user must'
      H(4)='specify a static insolation distribution which will be'
      H(5)='used if no db exists or a simulation is outwith the'
      H(6)='period of the db.'
      H(7)='Unless the user has specific knowledge to the contrary,'
      H(8)='diffuse distribution is suggested. '

      IALT(1)='Insolation to 1 surface      '
      IALT(2)='Insolation to 2 surfaces     '
      IALT(3)='Diffuse distribution         '
      IX=1
      CALL EPICKS(IX,IVAL,' ',' user specified insolation: ',
     &  29,3,IALT,'user specified insolation',IER,8)
 
      IF(IX.EQ.0)RETURN
      IF(IVAL(1).EQ.1)THEN

C Insolation to 1 surface, including internal windows.
        IS=1
        CALL EASKSUR(ICOMP,IS,'A','Select surface for insolation.',
     &    ' ',IER)
        NDP(ICOMP)=1
        IDPN(ICOMP,1)=IS
        IDPN(ICOMP,2)=0
        IDPN(ICOMP,3)=IS
      ELSEIF(IVAL(1).EQ.2)THEN

C Insolation to 2 surf, including internal windows on both.
        IS=1
        CALL EASKSUR(ICOMP,IS,'A','Select first insolated surface.',
     &    ' ',IER)
        NDP(ICOMP)=2
        IDPN(ICOMP,1)=IS
        IS=1
        CALL EASKSUR(ICOMP,IS,'A','Select 2nd insolated surface.',
     &    ' ',IER)
        IDPN(ICOMP,2)=IS
        IDPN(ICOMP,3)=-1
        CALL USRMSG(' ',' ','-')
      ELSEIF(IVAL(1).EQ.3)THEN

C Diffuse distribution.
        NDP(ICOMP)=3
        IDPN(ICOMP,1)=0
        IDPN(ICOMP,2)=0
        IDPN(ICOMP,3)=0
      ENDIF

      RETURN
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
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      DIMENSION STMP(MS),IVA(MS),IVALS(MS)
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12,H*72,STMP*41,act*1,msg1*32,msg2*32
      CHARACTER zname*12,zdesc*64

      IER=0
      IF(IZONE.GT.NCOMP.OR.IZONE.EQ.0)THEN
        CALL USRMSG(' ',' Zone number out of range! ','W')
        IER=1
        RETURN
      ENDIF

 43   if(act.eq.'s'.or.act.eq.'S')then
        h(1) ='A shading analysis finds the temporal patterns of '
        h(2) ='shading on exterior surfaces.  '
        h(3) =' '
        h(4) ='You are being asked to select surfaces. If you select'
        h(5) ='All applicable then this is: '
        h(6) ='  shading - all outside facing opaque & tansparent '
        h(7)=' '
        h(8)='You may also manually select surfaces. '
        h(9)='Note: this process works best if the model topology '
        h(10)='(boundary conditions) and surface conposition (so '
        h(11)=' '
        CALL EASKABCD('Shading  options:',' ','all applicable surfaces',
     &    'manual surf selection','none','continue ? ',IIC,11)
      elseif(act.eq.'i'.or.act.eq.'I')then
        h(1) ='An insolation analysis finds the temporal patterns of '
        h(2) ='direct solar radiation entering a zone and its'
        h(3) ='distribution. '
        h(4) =' '
        h(5) ='You are being asked to select surfaces. If you select'
        h(6) ='All applicable then this is: '
        h(7) ='  insolation - all outside facing tansparent '
        h(8)=' '
        h(9)='You may also manually select surfaces. '
        h(10)='Note: this process works best if the model topology '
        h(11)='(boundary conditions) and surface conposition (so '
        h(12)=' '
         CALL EASKABCD('Insolation options:',' ',
     &    'all applicable surfaces','manual surf selection',
     &    'none','continue ? ',IIC,12)
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
            if(SSOTHER(ioc)(1:5).eq.'EXTER')then
              NP=NP+1
              IVA(NP)=I
            endif
  13      continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found:',' ','W')
            goto 43
          endif
          iaplic=1
          return
        elseif(act.eq.'i'.or.act.eq.'I')then
          NP=0
          DO 15 I=1,NZSUR(IZONE)
            ioc=IZSTOCN(izone,i)
            if(SSOTHER(ioc)(1:5).eq.'EXTER'.and.
     &         SSOTF(ioc)(1:4).eq.'TRAN')then
              NP=NP+1
              IVA(NP)=I
            endif
   15     continue
          if(NP.eq.0)then
            CALL usrmsg('No applicable surfaces found:',' ','W')
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
C which are not applicable.
C        DO 11 I=1,INPIC
        DO 11 I=1,NZSUR(IZONE)
         ioc=IZSTOCN(izone,i)
         if(act.eq.'s'.or.act.eq.'S')then
           if(SSOTHER(ioc)(1:5).eq.'EXTER')then
             write(STMP(I),'(5a)') SSNAME(ioc),'|',SSOTF(ioc),' |',
     &         SSOTHER(ioc)
           else
             write(STMP(I),'(2a)') SSNAME(ioc),' not applicable'
           endif
         elseif(act.eq.'-')then
           write(STMP(I),'(5a)') SSNAME(ioc),'|',SSMLCN(ioc),'|',
     &       SSOTHER(ioc)
         elseif(act.eq.'c'.or.act.eq.'C')then
           write(STMP(I),'(5a)') SSNAME(ioc),'|',SSMLCN(ioc),'|',
     &       SSOTHER(ioc)
         elseif(act.eq.'i'.or.act.eq.'I')then
           if(SSOTHER(ioc)(1:5).eq.'EXTER'.and.
     &        SSOTF(ioc)(1:4).eq.'TRAN')then
             write(STMP(I),'(5a)') SSNAME(ioc),'|',SSOTF(ioc),' |',
     &         SSOTHER(ioc)
           else
             write(STMP(I),'(2a)') SSNAME(ioc),' not applicable'
           endif
         endif
   11   CONTINUE

        if(act.eq.'c'.or.act.eq.'C')then

C Note: use display width of 41 char.
          H(1)=' Pick one or more surfaces from the zone (up to'
          H(2)=' the limit shown on the left of the dialog).'
          write(msg1,'(a,i2,a)') '(up to ',ilimit,')'
          write(msg2,'(2a)') 'surfaces in ',zname(izone)
          CALL EPICKS(INPIC,IVALS,'Which surfaces: ',msg1,
     &      41,NZSUR(IZONE),STMP,msg2,IER,2)
        else
          H(1)=' Pick one, several or All surfaces for inclusion'
          write(msg2,'(a)') ' name & type & exposure'
          CALL EPICKS(INPIC,IVALS,' ',' Which surfaces to include: ',
     &      33,NZSUR(IZONE),STMP,msg2,IER,1)
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
        write(6,*) 'act np iva ',act,np,iva
      endif
      RETURN
      END

C ************* EDSURA 
C Edit zone surface attributes in common block G5 and allow this to be
C saved to a geometry file ITRU unit number for user output, IER=0 OK, 
C IER=1 problem. Make use of construction information in common MLC.
      SUBROUTINE EDSURA(ICOMP,ISS,ITRC,MODGEO,IER)
#include "building.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

C Optical and geometric properties.
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/ground/grdtmp(12,mgrdp)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      common/user/browse
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND,MODGEO,browse,nameok,close,showother
      logical dupedges,XST,OK,DOK,updoth

      DIMENSION SALT(11),IVAL(MCOM),bl(144)
      CHARACTER*33 ISD(21)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER DESC*48,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      CHARACTER zname*12,zdesc*64,DESCRC*25,outs*124,CXSTR*78
      CHARACTER SALT*33,H*72,SN*12,SN2*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character MLCN*12,OTF*4,OPT*12,MLCSYM*12,SSPARENT*12

C Display a selection of surfaces available in the zone. If surface
C faces another zone, get the other surface connection index. 
      IS=ISS
      ioc=IZSTOCN(icomp,is)
      if(ICT(ioc).eq.3)then
        showother=.true.
        icoth=IZSTOCN(IC2(ioc),IE2(ioc))
      else
        showother=.false.
      endif

C Force redraw of zone on entry to highlight focus surface.
      MODIFY=.TRUE.

C Check perimeter.
      call surfprm(icomp,is,dupedges,perim)
      if(dupedges)then
        call edisp(iuout,
     &    'This surface probably wraps around another surface.')
      endif

   13 WRITE(ISD(1),'(A,A12)')  'a surface name    : ',SNAME(ICOMP,IS)     
      WRITE(ISD(2),'(A,A4)')   'b surface type    : ',SOTF(IS)
      WRITE(ISD(3),'(A,A4)')   'c surface location: ',SVFC(IS)
      WRITE(ISD(4),'(A,f8.3)') '  surface area m^2:',SSNA(ioc)
      WRITE(ISD(5),'(A,2f7.2)')'  azim & elevation:',SSPAZI(ioc),
     &  SSPELV(ioc)
      WRITE(ISD(6),'(A,f8.3)') '  perimeter length:',ssperim(ioc)
      WRITE(ISD(7),'(A,F7.4)') 'd surface indentation (m):',Y0S(IS)
      WRITE(ISD(8),'(A,A12)')  'e construction    : ',SMLCN(IS)     
      WRITE(ISD(9),'(A,A15)')  'f environment  : ',SOTHER(IS)
      ISD(10)        ='  ___ @ other face____________ '
      if(showother)then
        if(IC2(ioc).ne.0.and.IE2(ioc).ne.0)then
          WRITE(ISD(11),'(2A)')'   surf name   : ',SSNAME(icoth)
        else
          WRITE(ISD(11),'(A)') '   surf name   : IS NOT KNOWN'
        endif
        WRITE(ISD(12),'(2A)')  '   surf type   : ',SSOTF(icoth)
        WRITE(ISD(13),'(2A)')  '   location    : ',SSVFC(icoth)
        WRITE(ISD(14),'(2A)')  '   construction: ',SSMLCN(icoth)
        WRITE(ISD(15),'(2A)')  '   environment : ',SSOTHER(icoth)
        ISD(16)=      '  ____________________________ '
        ISD(17)=      '+ add glazing/door/opening     '
        ISD(18)=      '? help                         '
        ISD(19)=      '- exit to zone description     '
        ISDN=19
      else
        ISD(10)=      '  ____________________________ '
        ISD(11)=      '+ add glazing/door/opening     '
        ISD(12)=      '? help                         '
        ISD(13)=      '- exit to zone description     '
        ISDN=13
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
      CALL ADJVIEW(ITRC,IER)

C Help text for the menu.
      H(1)='This is a display of the surface attributes which may'
      H(2)='be edited.  Editing is accomplished by selecting one'
      H(3)='of the surfaces. '
      H(4)=' '
      H(5)='You can add a rectangular opening (window/door) to'
      H(6)='this surface by specifing its offsets from the lower'
      H(7)='left left hand corner of the surface (looking from '
      H(8)='the outside), its width and height.'
      H(9)=' '
      H(10)='To add non-rectangular surface, use the surface list'
      H(11)='& edges menu pick.'

C Menu control.
      CALL EMENU('Surface Attributes',ISD,ISDN,INODA)
      IF(INODA.EQ.ISDN)THEN

C Exit and choose another surface.
        return
      ELSEIF(INODA.EQ.ISDN-1)THEN

C Produce help text for the menu.
        CALL PHELPD('surface attributes menu',11,'-',0,0,IER)
      ELSEIF(INODA.EQ.ISDN-2)THEN

C Add/insert a surface, return to higher level menu when done.
C The 'ii' request an interactive insertion, the parameters
C XO,ZO,XW,ZH are not used in an interactive insert.
        H(1)='Insertion into an existing surface is a common way '
        H(2)='to create a door or window opening. '
        LASTS=NSUR
        call tstamp('>','PRJ: insert surface into another')
        XO=0.
        ZO=0.
        XW=0.
        ZH=0.
        CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO,ZO,XW,ZH,IER)
        if(LASTS.ne.NSUR)then
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          CALL EMKCFG('-',IER)
          call warnmod(ICOMP,'sf+')
        endif
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITRC,IER)
        return
      ELSEIF(INODA.EQ.1)THEN
        write(SN,'(a12)')SNAME(ICOMP,IS)
        H(1)='Use this attribute to inform about the nature of the '
        H(2)='surface (make it easy for others to understand). '
 52     CALL EASKS(SN,' Surface name ','(unique word <=12 char):',
     &    12,'new_surface','surface name',IER,2)
        call st2name(SN,SN2)
        call snamdup(SN2,icomp,is,nameok)
        if(nameok)then
          SNAME(ICOMP,IS)=SN2
          SSNAME(ioc)=SN2
        else
          call edisp(iuout,'Surface name is a duplicate of an existing')
          call edisp(iuout,'surface. Please supply a different name.')
          goto 52
        endif
        MODIFY=.TRUE.
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.2)THEN

C Specify whether surface multilayer construction is opaque, transparent,
C or fictitious ( bookkeeping purposes only). If transparent ask user
C which set of optical properties to use.
        SALT(1)='Opaque (all layers)              '
        SALT(2)='Transparent (at least 1 layer)   '
        SALT(3)='N/A Ficticious (mass flow detail)'
        SALT(4)='N/A Ficticious (geometric detail)'
        SALT(5)='                                 '
        SALT(6)='                                 '

        H(1)='If a surface is composed of at least one transparent'
        H(2)='layer then it is considered not to be opaque.'
        H(3)=' '
        H(4)='Ficticious surfaces have not yet been implemented. '
        IX=1
    9   CALL EPICKS(IX,IVAL,' ',' Surface type: ',
     &         33,6,SALT,'surface type',IER,4)
 
        IF(IX.EQ.0)GOTO 13
        IF(IVAL(1).EQ.1)THEN
          SOTF(IS)='OPAQ'
          SSOTF(ioc)='OPAQ'
        ELSEIF(IVAL(1).EQ.2)THEN
          SOTF(IS)='TRAN'
          SSOTF(ioc)='TRAN'
C        ELSEIF(IVAL(1).EQ.3)THEN
C          SOTF(IS)='FICA'
C        ELSEIF(IVAL(1).EQ.4)THEN
C          SOTF(IS)='FICB'
        ELSE
          goto 9
        ENDIF
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.3)THEN

C Specify surface location for daylighting calculations and floor area
C based casual gain calculations.
        H(1)='The surface location is an optional attribute which is'
        H(2)='used to assist in assessing surfaces for daylighting'
        H(3)='calculations and casual gains based on floor areas.'
        H(4)='Initial values are based on the azimuth and elevation'
        H(5)='calculated for each surface.'
        H(6)='The user may over-ride these and leaving an attri-'
        H(7)='bute as unknown is not currently detrimental.'
        if(SVFC(IS)(1:4).eq.'UNKN')ino=1
        if(SVFC(IS)(1:4).eq.'VERT')ino=2
        if(SVFC(IS)(1:4).eq.'CEIL')ino=3
        if(SVFC(IS)(1:4).eq.'FLOR')ino=4
        ilno=ino
        idno=2
        call MENUATOL('Select an orientation for the surface.',
     &   ' Surface orientation','a Unknown or sloped',
     &   'b Vertical (or nearly vertical)',
     &   'c Ceiling (facing down) ','d Floor (facing up)',
     &   ' ',' ',' ',' ',' ',' ',' ',' ',ino,idno,7)
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

C Define the indentation for the current surface.
        H(1)=' To enable 3D geometry definition which is needed '
        H(2)=' for 3D zone gridding, the distance between the'
        H(3)=' current surface and an imaginary external surface'
        H(4)=' (line) is required. One imaginary surface should'
        H(5)='  be assumed for surfaces with similar direction'
        H(6)='  angles.'
        CALL EASKR(SINDT,' ',' Surface indentation (m) : ',
     &     0.0,'F',1.0,'W',0.,'Surface indentation',IER,6)
        Y0S(IS)=SINDT
        MODGEO=.TRUE.
      ELSEIF(INODA.EQ.8)THEN

C Check if user wishes to see construction details as well. If there is
C a zone construction file report its common block information, otherwise
C if SMLCN matches something in the constructions database then use
C that reporting mechanism.
        if(SMLCN(is)(1:4).ne.'UNKN')then
          write(SN,'(a12)')SNAME(icomp,is)

C << TODO - test if date of zone construction file is older than the
C << geometry file and if so do not use CONINF.
          if(LTHRM(ICOMP)(1:7).eq.'UNKNOWN'.or.
     &       LTHRM(ICOMP)(1:2).eq.'  ')then
            do 5 ii=1,nmlc
              if(SMLCN(is)(1:12).eq.DESC(ii)(1:12)) then
                write(outs,'(3a)') 'View thermophysical details of ',
     &            SN(1:lnblnk(SN)),'?'
                dok=.false.
                h(1)='There is no zone construction file yet, however '
                h(2)='you can see details from the database. '
                call askok(outs,
     &       '(database data matching surface construction attribute) ',
     &            ok,dok,2)
                if(ok)then
                  call ETMLDB(1,iuout,IFMAT,ii,imerr)        
                endif
              endif
  5         continue
          else

C A non-blank, possibly known construction.
            XST=.FALSE.
            call FINDFIL(LTHRM(ICOMP),XST)
            if(XST)then
              write(outs,'(3a)') 'View thermophysical details of ',
     &          SN(1:lnblnk(SN)),'?'
              dok=.false.
              h(1)='You can see details from the existing zone'
              h(2)='construction file .'
              call askok(outs,'(as held in zone construction file)',
     &          ok,dok,2)
              if(ok)then
                CALL ECONST(LTHRM(ICOMP),IFIL+2,ICOMP,0,IUOUT,IER)

C << this would also be a good place to list out optical
C << properties and the optical name.
                CALL CONINF(ICOMP,IS,iuout)
              endif
            endif
          endif
          write(outs,'(3a)') 'Modify thermophysical data of ',
     &      SN(1:lnblnk(SN)),'?'
          dok=.false.
          h(1)='If this surface is a partition and the construction is'
          h(2)='non-symetrical and linked to a reversed construction '
          h(3)='you should check that the surface in the other room '
          h(4)='has also been properly updated.'
          h(5)=' '
          h(6)='If after reviewing the current details you decide not'
          h(7)='to alter the thermophysical properties say `no`. '
          call askok(outs,'(see help)',ok,dok,7)
        else
          ok=.true.
        endif
        if(ok)then
          CALL EPMENSV
          CALL EPKMLC(ISEL,'Select one of the constructions.',' ',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            ioc=IZSTOCN(ICOMP,is)
            WRITE(SMLCN(IS),'(A12)')DESC(ISEL)(1:12)
            SSMLCN(ioc)=SMLCN(is)
            IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(IS)='TRAN'
            IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(IS)='OPAQ'
            SSOTF(ioc)=SOTF(is)
            MODGEO=.TRUE.
            call warnmod(ICOMP,'sat')

C If this is a partition loop and find MLC index of current surface
C attribute and see if the surface in the adjacent zone is made of
C an equivalent construction. If the current construction is
C symmetric then expect to find the same construction name and if
C the name does not match or is UNKN get user to confirm change.
            if(showother.and.icoth.ne.0)then
              do 7 ii=1,nmlc
                if(SMLCN(is)(1:12).eq.DESC(ii)(1:12)) then
                  updoth=.false.
                  call parsemlcdesc(DESC(ii),MLCN,OTF,OPT,MLCSYM)
                  if(MLCSYM(1:9).EQ.'SYMMETRIC')then
                    if(SSMLCN(icoth)(1:12).eq.MLCN(1:12))then
                      continue
                    elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      SSMLCN(icoth)=MLCN
                      updoth=.true.
                    else
                      SSMLCN(icoth)=MLCN
                      updoth=.true.
                    endif
                  elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                    write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &                ' has a nonsymmetric construction ',
     &                SMLCN(is)(1:lnblnk(SMLCN(is))),'.'
                    call edisp(iuout,outs)
                    write(outs,'(5a)') 'It faces ',
     &                SNAME(IC2(ioc),IE2(ioc)),' which is composed of ',
     &                SSMLCN(icoth)(1:12),' (which may not match).'
                    call edisp(iuout,outs)
                    call edisp(iuout,
     &               'Please check that one construction has inverted')
                    call edisp(iuout,
     &               'layers or revise the construction database.')
                    updoth=.false.
                  else
                    if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                      SSMLCN(icoth)=MLCSYM
                      updoth=.true.
                    elseif(SSMLCN(icoth)(1:12).eq.MLCSYM(1:12))then
                      continue
                    else
                      SSMLCN(icoth)=MLCSYM
                      updoth=.true.
                    endif
                  endif
                  if(updoth)then
                    CALL ESCZONE(ICOMP)
                    write(outs,'(3a)') 
     &                'Updating `other side` composition of ',
     &                SN(1:lnblnk(SN)),'...'
                    call edisp(iuout,outs)
                    CALL EGOMIN(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),0,0,
     &                iuout,IER)
                    SMLCN(IE2(ioc))=SSMLCN(icoth)
                    CALL EMKGEO(IFIL+2,LGEOM(IC2(ioc)),IC2(ioc),iuout,
     &                3,IER)
                    IF(IER.NE.0)CALL USRMSG(
     &               'Problem updating other surface attribute... ',
     &               '(could not write other geometry file).','W')

C Read current zone back in and then update the configuration file.
                    CALL ERCZONE(ICOMP)
                    call usrmsg(' Updating other side...done.',' ','-')
                    updoth=.false.
                  endif
                endif
   7          continue
            endif
          endif
          IF(IER.EQ.1)THEN
            CALL USRMSG(' ',' A problem was encountered..','W')
          ENDIF
        endif
      ELSEIF(INODA.EQ.9)THEN

C Specify the environment of the 'other' face of the surface.
        call edisp(iuout,' ')
        call edisp(iuout,' The current model topology is...')
        CALL CONXINFO(1,0,CXSTR)
        write(outs,'(1X,A)') CXSTR
        CALL EDISP(iuout,outs)
        CALL CONXINFO(1,IC,CXSTR)
        write(outs,'(1X,A)') CXSTR
        CALL EDISP(iuout,outs)
        call edisp(iuout,' ')

        SALT(1)='Exterior                         '
        SALT(2)='Dynamic (similar) boundary       '
        SALT(3)='Static boundary conditions       '
        SALT(4)='Surface in another zone          '
        SALT(5)='Ground (standard monthly profile)'
        if(NGRDP.gt.0)then
          SALT(6)='Ground (user defined profile)    '
        else
          SALT(6)='Ground (no user defined profiles)'
        endif
        SALT(7)='Ground (3D conduction model)     '
        SALT(8)='Adiabatic (no heat flux)         '
        SALT(9)='BASESIMP foundation configuration'
        SALT(10)='CEN 13791 partition              '
        SALT(11)='Unknown (at this time)           '

        H(1)='The boundary conditions at the other face of this sur-'
        H(2)='face is specified via the selection of one of these'
        H(3)='choices.  The default is OUTSIDE.'
        H(4)='Within the system configuration editing section are'
        H(5)='facilities to ascertain the probable connection type'
        H(6)='for most surfaces.  The attribute in this display is'
        H(7)='both an aide memoire and an initial source of infor-'
        H(8)='mation for the system level connection analysis.'
        IX=1
        CALL EPICKS(IX,IVAL,' ',' The other side of surface faces:',
     &    33,11,SALT,'surface boundary ',IER,8)

        IF(IX.EQ.0)GOTO 13

C Determine the match in the system topology and update it. 
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        IC1(IC)=ICOMP
        IE1(IC)=IS
        IF(IVAL(1).EQ.1)THEN
          SOTHER(IS)='EXTERIOR'
          SSOTHER(IC)='EXTERIOR'
          ICT(IC)=0
          IC2(IC)=0
          IE2(IC)=0
          showother=.false.
        ELSEIF(IVAL(1).EQ.2)THEN
          SOTHER(IS)='SIMILAR'
          SSOTHER(IC)='SIMILAR'
          ICT(IC)=1
          H(1)='A similar (type 1) connection can include an offset'
          H(2)='temperature and radiation (W/m^2) from that of the'
          H(3)='current zone. For example, if the adjacent space'
          H(4)='which has not been geometrically defined tends to '
          H(5)='be 2degC cooler then use a -2 offset. '
          H(6)=' '
          H(7)='(units are in whole degC and whole W) '
          VALT=0.0
          VALW=0.0
          CALL EASKR(VALT,' Offset temperature? [default=0.0]',
     &      '(see help) ',-99.,'F',99.,'F',0.0,'offset temp',IER,7)
          CALL EASKR(VALW,' Offset radiation source (W/m^2)?',
     &      '(see help) ',0.0,'W',99999.,'W',0.0,'offset rad',IER,7)
          IC2(IC)=INT(VALT)
          IE2(IC)=INT(VALW)
          showother=.false.
        ELSEIF(IVAL(1).EQ.3)THEN
          SOTHER(IS)='CONSTANT'
          SSOTHER(IC)='CONSTANT'
          ICT(IC)=2
          H(1)='A type 2 connection requires that a constant temp-'
          H(2)='erature and radiation (W/m^2) be entered.'
          CALL EASKR(VAL,' ',' Constant temperature ? ',
     &      -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
          IC2(IC)=INT(VAL)

          CALL EASKR(VAL,' ',' Constant radiation source (W/m^2) ? ',
     &      0.0,'W',99999.,'W',0.0,'adjacent radiation source',IER,2)
          IE2(IC)=INT(VAL)
          showother=.false.
        ELSEIF(IVAL(1).EQ.4)THEN

C Ask for specific name.
          CALL EASKGEOF('If `other` zone known pick, otherwise exit.',
     &      CFGOK,IZ,'-',IER)
          if(IZ.GT.0)then

C General image option flags.
            ITDSP=1
            ITBND=1
            ITEPT=0
            ITZNM=0
            ITSNM=0
            ITVNO=1
            ITORG=1
            ITSNR=1
            ITGRD=1
            GRDIS=0.0
            ITPPSW=0

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
            CALL ADJVIEW(0,IER)
            MODIFY=.TRUE.
            MODBND=.TRUE.

            ISO=1
            CALL EPMENSV
            CALL EASKSUR(IZ,ISO,'A','Select surface in the adjacent',
     &        'zone. ',IER)
            CALL EPMENRC
            if(ISO.GT.0)then
              write(SOTHER(IS),'(a)') zname(IZ)
              write(SSOTHER(IC),'(a)') zname(IZ)
              ICT(IC)=3
              IC2(IC)=IZ
              IE2(IC)=ISO

C If applicable find other surface, return it`s connection number and
C point it to the current surface.
              updoth=.false.
              dok=.true.
              h(1)='You have modified an attribute of a surface which'
              h(2)='is a partition between two zones. You probably'
              h(3)='should attempt to update to associated surface'
              h(4)='in the adjacent zone to maintain consistency. '
              CALL ASKOK(' ','Update other side? (see help)',
     &          updoth,dok,4)
              if(updoth)then
                CALL SURADJ(IZ,ISO,IE,TMP,IZC,ISC,ICO,DESCRC)
                IC1(ICO)=IZ
                IE1(ICO)=ISO
                ICT(ICO)=3
                IC2(ICO)=IC1(IC)
                IE2(ICO)=IE1(IC)
                write(SSOTHER(ICO),'(a)') zname(ICOMP)

C Update information needed to display information on the other side
C of this surface (i.e. if we went from unknown to a partition).
                icoth=ICO

C Also check the other side construction and see if it matches.
                do 8 ii=1,nmlc
                  if(SMLCN(is)(1:12).eq.DESC(ii)(1:12)) then
                    call parsemlcdesc(DESC(ii),MLCN,OTF,OPT,MLCSYM)
                    if(MLCSYM(1:9).EQ.'SYMMETRIC')then
                      if(SSMLCN(icoth)(1:12).eq.MLCN(1:12))then
                        continue
                      elseif(SSMLCN(icoth)(1:4).eq.'UNKN')then
                        SSMLCN(icoth)=MLCN
                      else
                        SSMLCN(icoth)=MLCN
                      endif
                    elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is nonsymmetric then it should not be
C used for a partition (and db does not have a linked MLC - inform the user.
                      write(outs,'(5a)') 'Surface ',SN(1:lnblnk(SN)),
     &                 ' has a nonsymmetric construction ',
     &                 SMLCN(is)(1:lnblnk(SMLCN(is))),'.'
                      call edisp(iuout,outs)
                      write(outs,'(5a)') 'It faces ',
     &                 SNAME(IC2(ic),IE2(ic)),' composed of ',
     &                 SSMLCN(icoth)(1:12),
     &                 ' (which may not have matching layers).'
                      call edisp(iuout,outs)
                      call edisp(iuout,
     &     'Please check attributes and/or revise the construction db.')
                    else
                      if(SSMLCN(icoth)(1:4).eq.'UNKN')then
                        SSMLCN(icoth)=MLCSYM
                      elseif(SSMLCN(icoth)(1:12).eq.MLCSYM(1:12))then
                        continue
                      else
                        SSMLCN(icoth)=MLCSYM
                      endif
                    endif
                  endif
  8             continue
                icoth=ICO
              endif
              showother=.true.
            else
              SOTHER(IS)='ANOTHER_ZONE'
              SSOTHER(IC)='ANOTHER_ZONE'
            endif
          else 
            SOTHER(IS)='ANOTHER_ZONE'
            SSOTHER(IC)='ANOTHER_ZONE'
          endif
        ELSEIF(IVAL(1).EQ.5)THEN

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
          CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
          CALL EDISP(iuout,'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
          do 21 igrdp=1,mgrdp
            CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
            if(.NOT.close)then
              WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
            endif
 21       continue
          SOTHER(IS)='GROUND'
          SSOTHER(IC)='GROUND'
          ICT(IC)=4
          H(1)='A ground connection allows either:'
          H(2)='> 0 a connection to one of several standard profiles'
          H(3)='    in which case the profile number is required.'
          H(4)='= 0 a connection to a user defined profile in which'
          H(5)='    case a 0 followed by a profile number is '
          H(6)='    required.'
          H(7)='-3  a connection to a 3D ground model.'
          iic2=ic2(IC)
  777     CALL EASKI(IIC2,' Monthly (standard) profile index? ',
     &                  ' ',1,'F',12,'F',1,'ground profile',IER,7)
          IF(IIC2.GT.0)THEN
            IC2(IC)=iic2
            IE2(IC)=0
          ELSE
            CALL USRMSG(' ',' Unacceptable ground profile...','W')
            GOTO 777
          ENDIF
        ELSEIF(IVAL(1).EQ.6)THEN

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
          if(NGRDP.gt.0)then
            CALL EDISP(iuout,' ')
            do 22 igrdp=1,NGRDP    
              CALL EDISP(iuout,'User defined ground profile Jan-Dec:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
 22         continue
            SOTHER(IS)='GROUND'
            SSOTHER(IC)='GROUND'
            ICT(IC)=4
            H(1)='One of the user defined profile numbers is'
            H(2)='required here.'
            CALL EASKI(IIE2,' User defined ground profile index ?',
     &        ' ',0,'F',9,'F',1,'user defined profile',IER,2)
            IC2(IC)=0
            IE2(IC)=iie2
          else
           call usrmsg(
     &        'No user defined ground profiles have been found.',
     &        'Go to `model context` to define these.','W')
          endif
        ELSEIF(IVAL(1).EQ.7)THEN

C Link to a 3D conduction model..
          SOTHER(IS)='GROUND'
          SSOTHER(IC)='GROUND'
          ICT(IC)=4
          IC2(IC)=-3
          IE2(IC)=0
          call edisp(iuout,'Link set to 3D ground conduction model.')
        ELSEIF(IVAL(1).EQ.8)THEN
          SOTHER(IS)='ADIABATIC'
          SSOTHER(IC)='ADIABATIC'
          ICT(IC)=5
          IE2(IC)=0
          IC2(IC)=0
        ELSEIF(IVAL(1).EQ.9)THEN

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype.
          SOTHER(IS)='BASESIMP'
          SSOTHER(IC)='BASESIMP'
          ICT(IC)=6
          H(1)='Specify the BASESIMP configuration number (iconfig). '
          H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
          H(3)='correspondence between iconfig and standard BASESIMP '
          H(4)='descriptors (e.g. BCIN_1).'
          IBS=IC2(IC)
          call bsimtype(ibs)
          IC2(IC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
          Ifrac=IE2(IC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
          CALL EASKI(Ifrac,' Percent of BASESIMP loss to this surface?'
     &       ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',IER,6)
          IE2(IC)=Ifrac
C BASESIMP end.

        ELSEIF(IVAL(1).EQ.10)THEN
          SOTHER(IS)='IDENT_CEN'
          SSOTHER(IC)='IDENT_CEN'
          ICT(IC)=7

C          H(1)='This is like the SIMILAR but for CEN 13791 test'
C          H(2)='A similar (type 1) connection can include an offset'
C          H(3)='temperature and radiation (W/m^2) from that of the'
C          H(4)='current zone. This is not available for the CEN'
C          H(5)='13791 partition where these values are 0. '
C          H(6)='For details see section 4.4.2 CEN 13791 standard. '
          VALT=0.0
          VALW=0.0
          IC2(IC)=INT(VALT)
          IE2(IC)=INT(VALW)
          showother=.false.


        ELSEIF(IVAL(1).EQ.11)THEN

C Reset to zeros << actually should have a different ict
C to represent UNKNOWN >>
          SOTHER(IS)='UNKNOWN'
          SSOTHER(IC)='UNKNOWN'
          ICT(IC)=0
          IE2(IC)=0
          IC2(IC)=0
        ELSE
          goto 148
        ENDIF
        MODGEO=.TRUE.

C Finally update the system configuration file if not browsing.
        if(cfgok.and.(.NOT.browse))then
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C If user requested other side to be updated do this as well. Read
C in the zone pointed to by IC2, update SOTHER (of index IE2 in the
C other zone) and write out that zones geometry.
          if(updoth)then
            CALL ESCZONE(ICOMP)
            write(outs,'(3a)') 'Updating `other side` composition of ',
     &        SN(1:lnblnk(SN)),'...'
            call edisp(iuout,outs)
            CALL EGOMIN(IFIL+2,LGEOM(IC2(IC)),IC2(IC),0,0,iuout,IER)
            SOTHER(IE2(IC))=zname(IC1(IC))
            SMLCN(IE2(IC))=SSMLCN(icoth)
            CALL EMKGEO(IFIL+2,LGEOM(IC2(IC)),IC2(IC),iuout,3,IER)
            IF(IER.NE.0)CALL USRMSG(' ',
     &           ' Problem updating other surface attribute...','W')

C Read current zone back in and then update the configuration file.
            CALL ERCZONE(ICOMP)
            call usrmsg(' ',' ','-')
            updoth=.false.
          endif
          CALL EMKCFG('-',IER)
        endif
      ELSE
        INODA=-4
        goto 13
      ENDIF
      INODA=-4
      goto 13

  149 RETURN

  148 CALL USRMSG(' ',' Problem reading other zone name.','W')
      IER=1
      GOTO 149
      END 

C ************* EDVERT 
C Edit vertex attributes in common block G1 via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDVERT(ITRC,ITRU,ICOMP,MODGEO,IER)
#include "epara.h"
#include "building.h"
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      LOGICAL MODIFY,MODLEN,MODBND,MODGEO,ok,dok,match,matchver

      DIMENSION VERT(35),IVLST(MTV),ITEMP(MTV),COG1(3),COG2(3),COG3(3)
      DIMENSION VP(3),EQN(4),XX(MV),YY(MV),ZZ(MV)
      CHARACTER VERT*33,KEY*1,H*72,head*28,zname*12,zdesc*64,HOLD*32
      character outs*124,headv*48
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15

C Initial values for vertex at angle facility.
      V1=1.0
      AZ=0.0
      EL=0.0

C Initialise zone vertex menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=6
      ILEN=NTV
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NTV
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

      VERT(1)    =' Index|X coord |Y coord |Z coord'

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
      VERT(M+4)  ='! browse surface-vertex topology '
      VERT(M+5)  ='? help                           '
      VERT(M+6)  ='- exit                           '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(ITRC,IER)

C Help text for this menu.
      H(1)='This menu allows existing vertices to be viewed and'
      H(2)='edited. Edit by selecting an item or -Exit to escape.'
      H(3)=' '
      H(4)='The `> find vertex` option is useful to predict an'
      H(5)='intermediate vertex along an edge. '
      H(6)=' '
      H(7)='The `^ distance along` option is useful to confirm'
      H(8)='actual dimension of a surface.'

C Now display the menu.
      write(head,'(A,A12)')' Vertices in ',zname(ICOMP)
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        CALL PHELPD('configuration section',8,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN
        CALL VERINFO(ITRU)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        H(1)='*--x-* Vertex on a line: finds one or more points '
        H(2)='along a line (defined by specifying two vertices). '
        H(3)=' '
        H(4)='*<-?->* Distance between vertices: gives distance in '
        H(5)='metres given two vertices. '
        H(6)=' '
        H(7)='Vertex at angle: finds a (new) vertex relative to '
        H(8)='a given vertex at a specified distance, angle and '
        H(9)='elevation. '
        H(10)=' '
        H(11)='Angel between lines: finds angle A between two lines'
        H(12)='given 3 vertex as in:      * 1 '
        H(13)='                       a  /    '
        H(14)='                3 *------* 2   '
        idno=2
        call MENUATOL('available vertex transforms (see help):',
     &   ' Vertex transform options ','a vertices along a line',
     &   'b distance between two vertices',
     &   'c vertex @ angle & distance',
     &   'd angle between two lines','e intersect of a line & plane',
     &   'f distance from vertex to a line',
     &   'g align vertex with a line','h find close vertices',
     &   'i move vetices along a line',' ',' ',' ',ino,idno,14)
        if(ino.eq.0.or.ino.eq.10)then
          continue
        elseif(ino.eq.1)then

C Vertices along a line between two verts.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for *---x--*',
     &      'Select two vertices to define a line...',' ',14,ier)
          CALL EPMENRC
  72      if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            write(outs,'(a,i2,a,i2,a,f9.4)') ' Distance between v ',
     &        IWHICH1,' & v ',IWHICH2,' =',tdis
            call edisp(itru,outs)

            H(1)='Distance along the line in metres.'
            CALL EASKR(vdis,' Distance along the line (metres): ',
     &        ' : ',0.001,'F',99.999,'W',0.1,'dist along line',IER,1)

C Use ratio calculation.
            r2 = tdis - vdis
            r1 = vdis
            x3 = ((r2 * X(IWHICH1)) + (r1 * X(IWHICH2)))/tdis
            y3 = ((r2 * Y(IWHICH1)) + (r1 * Y(IWHICH2)))/tdis
            z3 = ((r2 * Z(IWHICH1)) + (r1 * Z(IWHICH2)))/tdis
  
            write(outs,'(a,3f10.4)') ' Vertex is at X,Y,Z:',x3,y3,z3
            call edisp(itru,outs)

C Show the new point on the current wireframe.
            COG1(1)=x3
            COG1(2)=y3
            COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            call ecirc(ix,iy,3,1)
            call forceflush()
            if(NTV.lt.MTV)then
              h(1)='This can become a new vertex. '
              write(outs,'(a,i2,a)') 'Make this (',NTV+1,') a:'
              CALL EASKABC(outs,' ','new vertex',
     &          'new vertex after editing','cancel',INVT,1)
              if(INVT.eq.1)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                x(ntv)=x3
                y(ntv)=y3
                z(ntv)=z3
              elseif(INVT.eq.2)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
                H(1)='The vertex coordinates should be in metres. '
                H(2)='It is not advisable for Z to be negative.'
                write(outs,'(a,i2,a)')' Vertex (',ntv,') X  Y  Z (m):'
 443            CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',IER,2)
                K=0
                CALL EGETWR(HOLD,K,X(ntv),-999.,999.,'W','X cd',IER)
                CALL EGETWR(HOLD,K,Y(ntv),-999.,999.,'W','Y cd',IER)
                CALL EGETWR(HOLD,K,Z(ntv),-9.9,999.9,'W','Z cd',IER)
                if(ier.ne.0)goto 443
              elseif(INVT.eq.3)then
                continue
              endif
              ILEN=NTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
              MODIFY=.TRUE.
              MODBND=.TRUE.
              ZBFLG(ICOMP)=0.
              call usrmsg(' ',' ','-')
              CALL INLNST(1)
              nzg=1
              nznog(1)=ICOMP
              izgfoc=ICOMP
              CALL ADJVIEW(ITRC,IER)

C Check to see if this new vertex should be included in any existing edges.
C << example of checking if should be inserted in another surface >>
              if(invt.eq.1.or.invt.eq.2)then
                iwhich3=NTV
                do 246 ivj=1,NSUR
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
     &                matchver)
                    if(.NOT.matchver) goto 247
                    if(offset.lt.0.004)then
                      write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &                  'New vertex ',iwhich3,' @',X(NTV),Y(NTV),
     &                  Z(NTV),' is close (',offset,') to edge ',
     &                  IWHICH1,' &',IWHICH2,' of surface ',
     &                  SNAME(ICOMP,ivj)
                      call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                      if(NVER(ivj)+1.le.MV)then
                        dok=.true.
                        h(1)='The new vertex is within 3mm of an'
                        h(2)='edge of another surface. Probably the'
                        h(3)='vertex should also be included in that'
                        h(4)='other surface (so the rules about'
                        h(5)='matching edges is followed). '
                        call askok('Insert this vertex?','(see help)',
     &                    ok,dok,5)
                      else
                        ok=.false.
                      endif
                      if(ok)then
                        NVER(ivj)=NVER(ivj)+1
                        IXV=NVER(ivj)+1
  148                   continue
                        IXV=IXV-1
                        JVN(ivj,IXV)=JVN(ivj,IXV-1)
                        IF(IXV.GT.ivjj+1)GOTO 148
                        JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                       write(6,'(a,30i3)') 'now jvn is ',
C     &                  (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                        goto 246 
                      endif
                    endif
  247             continue
  246           continue
              endif
            endif
            dok=.false.
            h(1)='If you have several vertices along one line you '
            h(2)='can click ok to continue defining them. '
            call askok('Another vertex along the line? ',' ',ok,dok,2)
            if(ok)goto 72
          else
            goto 3
          endif
        elseif(ino.eq.2)then

C Distance between two vertices.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for *<--?-->*',
     &      'Select two vertices...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            vdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            call ln2az(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),az,el)
            write(outs,'(a,i2,a,i2,a,f9.4,a,f8.3,a,f7.3)')
     &        ' Distance between v ',IWHICH1,' & v ',IWHICH2,' =',vdis,
     &        ' @ aimuth ',az,' & elev ',el
            call edisp(itru,outs)
            MODIFY=.true.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(ITRC,IER)
          endif
        elseif(ino.eq.3)then
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex for origin',
     &      'Select vertex to act as origin...',' ',14,ier)
          CALL EPMENRC
  73      if(inpick.eq.1)then
            iwhich=IVLST(1)

C Present distance, azimuth and elevation then parse data from HOLD.
            write(hold,'(f10.4,f9.3,f8.3)') V1,AZ,EL
            H(1)='Distance is in metres, azimuth assumes north=0 '
            H(2)='+ degrees clockwise, elevation 90 is vertical. '
 342        CALL EASKS(HOLD,
     &      'Distance (m), azimuth (north=0, east=90), elev (vert=90):',
     &      ' ',32,' 1. 0. 0. ','dist azim elev',IER,2)
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
            write(outs,'(a,3f10.4)') ' Vertex @ X,Y,Z:',x3+X(IWHICH),
     &        y3+Y(IWHICH),z3+Z(IWHICH)
            call edisp(itru,outs)
            x3=x3+X(IWHICH)
            y3=y3+Y(IWHICH)
            z3=z3+Z(IWHICH)

C Show the new point on the current wireframe.
            COG1(1)=x3
            COG1(2)=y3
            COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            call ecirc(ix,iy,3,1)
            call forceflush()
            if(NTV.lt.MTV)then
              h(1)='This can become a new vertex. '
              write(outs,'(a,i2,a)') 'Make this (',NTV+1,') a:'
              CALL EASKABC(outs,' ','new vertex',
     &          'new vertex after editing','cancel',INVT,1)
              if(INVT.eq.1)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                x(ntv)=x3
                y(ntv)=y3
                z(ntv)=z3
              elseif(INVT.eq.2)then
                NTV=NTV+1
                NZTV(icomp)=NTV
                WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
                H(1)='The vertex coordinates should be in metres. '
                H(2)='It is not advisable for Z to be negative.'
                write(outs,'(a,i2,a)')' Vertex (',ntv,') X  Y  Z (m):'
 543            CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',IER,2)
                K=0
                CALL EGETWR(HOLD,K,X(ntv),-999.,999.,'W','X cd',IER)
                CALL EGETWR(HOLD,K,Y(ntv),-999.,999.,'W','Y cd',IER)
                CALL EGETWR(HOLD,K,Z(ntv),-9.9,999.9,'W','Z cd',IER)
                if(ier.ne.0)goto 543
              elseif(INVT.eq.3)then
                continue
              endif
              ILEN=NTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
              MODIFY=.TRUE.
              MODBND=.TRUE.
              ZBFLG(ICOMP)=0.
              call usrmsg(' ',' ','-')
              CALL INLNST(1)
              nzg=1
              nznog(1)=ICOMP
              izgfoc=ICOMP
              CALL ADJVIEW(ITRC,IER)
            endif
            dok=.false.
            h(1)='If you have several vertices to create '
            h(2)='can click ok to continue defining them.'
            call askok('Another vertex from this origin?',' ',ok,dok,2)
            if(ok)goto 73
          endif
          MODIFY=.true.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(ITRC,IER)
        elseif(ino.eq.4)then

C Angle between two lines.
          inpick=3
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Angle between 3 vertices',
     &      'Select three vertices (see help for rules).',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.3)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            iwhich3=IVLST(3)
            call ang3vtx(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2),X(IWHICH3),Y(IWHICH3),Z(IWHICH3),
     &        ang)
            write(outs,'(a,3i3,a,f9.4)') ' Angle between verts ',
     &        iwhich1,iwhich2,iwhich3,' is ',ang
            call edisp(itru,outs)
          endif
          MODIFY=.true.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(ITRC,IER)
        elseif(ino.eq.5)then

C Intersection line and plane.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices defining line',
     &      'Select two vertices...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            IS=1
            CALL EASKSUR(ICOMP,IS,'-','Select surface to intersect.',
     &         ' ',IER)
            N = NVER(IS)
            DO 150 J = 1,N
              XX(J) = X(JVN(IS,J))
              YY(J) = Y(JVN(IS,J))
              ZZ(J) = Z(JVN(IS,J))
  150       CONTINUE
            call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
            CALL VECPLN(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &        X(IWHICH2),Y(IWHICH2),Z(IWHICH2),EQN, x3,y3,z3, IERR)
            write(outs,'(a,3f10.4)') ' Vertex is at X,Y,Z:',x3,y3,z3
            call edisp(itru,outs)

C Show the new point on the current wireframe.
            COG1(1)=x3
            COG1(2)=y3
            COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            call ecirc(ix,iy,3,1)
            call forceflush()
          endif
        elseif(ino.eq.6)then

C Distance between point and a line.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices defining line',
     &      'Select two vertices...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
          endif
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to test',
     &      'Select a vertex...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',X(iwhich3),Y(iwhich3),
     &     Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',IWHICH2
          call edisp(itru,outs)
          MODIFY=.TRUE.
          MODBND=.TRUE.
          ZBFLG(ICOMP)=0.
          call usrmsg(' ',' ','-')
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(ITRC,IER)
        elseif(ino.eq.7)then

C Bring a point off a line into alignment. Begin by checking the
C distance between point and the line.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices defining line',
     &      'Select two vertices...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
          endif
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to align',
     &      'Select a vertex...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.1)iwhich3=IVLST(1)

C Report length of line. Use method of Ward/Radiance in fvect.c
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,match)
          if(.NOT.match) goto 3
          write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3)')
     &     'Vertex ',iwhich3,' @',X(iwhich3),Y(iwhich3),
     &     Z(iwhich3),' is ',offset,'(m) to edge ',IWHICH1,' &',IWHICH2
           call edisp(itru,outs)

C If the distance is greater than 1mm get the distance between the
C start of the line (iwhich1) and the test point (iwhich3) and do this
C for the end point on the line (iwhich2) and the test point.
          if(offset.gt.0.002)then
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
            write(outs,'(a,3f10.4)')' Aligned vertex @ X,Y,Z:',x3,y3,z3
            call edisp(itru,outs)
            COG1(1)=x3
            COG1(2)=y3
            COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            call ecirc(ix,iy,3,1)
            call forceflush()
            dok=.true.
            h(1)='Last chance to change the vertex position. '
            call askok('Is this aligned vertex ok?',' ',ok,dok,1)
            if(ok)then
              X(IWHICH3)=x3
              Y(IWHICH3)=y3
              Z(IWHICH3)=z3
              MODGEO=.TRUE.
              iwhich3=NTV
              do 346 ivj=1,NSUR
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
                    write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',
     &                X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &                SNAME(ICOMP,ivj)
                    call edisp(itru,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      dok=.true.
                      h(1)='The new vertex is within 3mm of an'
                      h(2)='edge of another surface. Probably the'
                      h(3)='vertex should also be included in that'
                      h(4)='other surface (so the rules about'
                      h(5)='matching edges is followed). '
                      call askok('Insert this vertex?','(see help)',
     &                  ok,dok,5)
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
C                     write(6,'(a,30i3)') 'now jvn is ',
C     &                (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 346 
                    endif
                  endif
  347           continue
  346         continue
            endif
          endif
        elseif(ino.eq.8)then
        
C Find close vertices.
          h(1)='If there are vertices which are near duplicates, this'
          h(2)='function will list them all or find ones close to one.'
          CALL EASKABC('Options:',' ','find any close vertices',
     &          'find closest to one vertex','cancel',INVT,2)
          if(INVT.eq.1)then
            do 102 iwhich1=1,NTV
              write(headv,'(a,i3,a,3f10.4)') ' Vert',iwhich1,
     &          ' @ XYZ:',X(IWHICH1),Y(IWHICH1),Z(IWHICH1)
              do 103 iwhich2=1,NTV
                if(iwhich1.ne.iwhich2)then
                  tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),
     &                  X(IWHICH2),Y(IWHICH2),Z(IWHICH2))
                  if(tdis.lt.0.05)then
                  write(outs,'(2a,i2,a,f9.4,a)') headv(1:lnblnk(headv)),
     &              ' is close to vert ',IWHICH2,' (',tdis,'m).'
                  call edisp(itru,outs)
                  endif
                endif
  103         continue
  102       continue
          elseif(INVT.eq.2)then
            inpick=1
            CALL EPMENSV
            call EPKVERT(INPICK,IVLST,'Focus vertex',
     &        '(close vertices will be listed)',' ',14,ier)
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
           write(outs,'(2a,i2,a,f9.4,a,3F10.4)') headv(1:lnblnk(headv)),
     &       ' close to v ',IWHICH2,' (',tdis,'m) @ XYZ:',X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2)
                    call edisp(itru,outs)
                  endif
                endif
  101         continue
            endif
          else
            continue
          endif
        elseif(ino.eq.9)then

C Move an existing vertex along an existing line.
          inpick=2
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices for <-x->---*',
     &      'Select two vertices along a line...',
     &      '(first one is the vertex to move)',14,ier)
          CALL EPMENRC
          if(inpick.eq.2)then
            iwhich1=IVLST(1)
            iwhich2=IVLST(2)
            tdis= crowxyz(X(IWHICH1),Y(IWHICH1),Z(IWHICH1),X(IWHICH2),
     &        Y(IWHICH2),Z(IWHICH2))
            write(outs,'(a,i2,a,i2,a,f9.4)') ' Distance between v ',
     &        IWHICH1,' & v ',IWHICH2,' =',tdis
            call edisp(itru,outs)

            H(1)='Distance along the line in metres. Positive'
            H(2)='will be towards the 2nd vertex. Negavie will '
            H(3)='be away from 2nd vertex.'
            CALL EASKR(vdis,' Move along the line (metres): ',
     &        '(see help)',-1.0,'W',99.999,'W',0.1,'dist along line',
     &        IER,2)

C Use ratio calculation.
            r2 = tdis - vdis
            r1 = vdis
            x3 = ((r2 * X(IWHICH1)) + (r1 * X(IWHICH2)))/tdis
            y3 = ((r2 * Y(IWHICH1)) + (r1 * Y(IWHICH2)))/tdis
            z3 = ((r2 * Z(IWHICH1)) + (r1 * Z(IWHICH2)))/tdis
  
            write(outs,'(a,3f10.4)') ' Moved vertex @ XYZ:',x3,y3,z3
            call edisp(itru,outs)

C Show the new point on the current wireframe.
            COG1(1)=x3
            COG1(2)=y3
            COG1(3)=z3
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            call ecirc(ix,iy,3,1)
            call forceflush()
            dok=.true.
            h(1)='Last chance to abort the change. '
            call askok('Apply this move?',' ',OK,dok,1)
            if(OK)then
              x(IWHICH1)=x3
              y(IWHICH1)=y3
              z(IWHICH1)=z3
            endif
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            ZBFLG(ICOMP)=0.
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(ITRC,IER)
          else
            goto 3
          endif
        endif
        call usrmsg(' ',' ','-')

C If possible to add another vertex ask if calculated point should
C be a new vertex and update the wireframe.
        if(ino.eq.5)then
          if(NTV.lt.MTV)then
            h(1)='This can become a new vertex. '
            write(outs,'(a,i2,a)') 'Make this (',NTV+1,') a:'
            CALL EASKABC(outs,' ','new vertex',
     &        'new vertex after editing','cancel',INVT,1)
            if(INVT.eq.1)then
              NTV=NTV+1
              NZTV(icomp)=NTV
              x(ntv)=x3
              y(ntv)=y3
              z(ntv)=z3
            elseif(INVT.eq.2)then
              NTV=NTV+1
              NZTV(icomp)=NTV
              WRITE(HOLD,'(1x,3f10.5)')x3,y3,z3
              H(1)='The vertex coordinates should be in metres. '
              H(2)='It is not advisable for Z to be negative.'
              write(outs,'(a,i2,a)')' Vertex (',ntv,') X  Y  Z (m):'
  43          CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vtx co',IER,2)
              K=0
              CALL EGETWR(HOLD,K,X(ntv),-999.,999.,'W','X cd',IER)
              CALL EGETWR(HOLD,K,Y(ntv),-999.,999.,'W','Y cd',IER)
              CALL EGETWR(HOLD,K,Z(ntv),-9.9,999.9,'W','Z cd',IER)
              if(ier.ne.0)goto 43
            elseif(INVT.eq.3)then
              goto 3
            endif
            ILEN=NTV
            IPACT=CREATE
            CALL EKPAGE(IPACT)
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            ZBFLG(ICOMP)=0.
            call usrmsg(' ',' ','-')
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(ITRC,IER)
            goto 92
          endif
        endif
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Alter vertex list by deleting/ copy / replicate / editing.
        write(H(1),'(A,I2,A)')' Up to ',MTV-NTV,
     &    ' vertices may be added or copied.'
        H(2)='If a new vertex is a minor variation of an existing '
        H(3)='point then it may be easier to copy and then edit. '
        h(4)=' '
        h(5)='You may delete one or more vertices (but only those'
        h(6)='which are unlinked to surfaces may be deleted). '
        h(7)='In graphic mode click on the vertices (end with and `e`)'
        h(8)=' '
        h(9)='Replication can be used make several varitants of a'
        h(10)='single vertex (useful where only one coordinate needs'
        h(11)='to be altered). '
        h(12)=' '
        h(13)='In graphic mode you can edit a number of vertices by '
        h(14)='clicking on the wire-frame image. '
        CALL EASKATOG('Vertex operations:',' ','add','delete','copy',
     &    'replicate','edit (several)','do nothing',' ',IRT,14)
        if(IRT.eq.1)then
          CALL EASKI(IADD,' ',' How many vertices to add ? ',
     &     1,'F',MTV-NTV,'F',1,'+ vertex',IER,14)
          CALL ADDVERT(icomp,IADD,'A',IER)
          ILEN=NTV
          IPACT=CREATE
          CALL EKPAGE(IPACT)
          MODGEO=.TRUE.
          ZBFLG(ICOMP)=0.
        elseif(IRT.eq.2)then

C Delete one or more vertices, get list and then sort in decending
C order so that compating of list works correctly.
          jipm=IPM
          inpick=MIN0(12,NTV-1)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to delete',
     &      'Select vertices to delete from list...',' ',14,ier)
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
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            ZBFLG(ICOMP)=0.
          endif
        elseif(IRT.eq.3)then

C In the case of copying existing vertices, process one or more,
C adding to zone data structure and updating the interface. Allow
C user to copy only as many vertices as there are in the zone but
C no more than will overrange MTV.
          inpick=MIN0(NTV,MTV-NTV)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to copy',
     &      'Select vertices to copy from list...',' ',14,ier)
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
            MODGEO=.TRUE.
            MODIFY=.TRUE.
            MODBND=.TRUE.
            ZBFLG(ICOMP)=0.
          endif
        elseif(IRT.eq.4)then

C In the case of replicating an existing vertex, add it to zone
C data structure and updating the interface. Allow user to replicate
C only up to MTV.
          inpick=1
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertex to replicate',
     &      'Select vertex to replicate from list...',' ',14,ier)
          CALL EPMENRC
          if(inpick.eq.1)then
            iwhich=IVLST(1)
            CALL EASKI(IADD,' ',' Replicate how many times ? ',
     &        1,'F',MTV-NTV,'F',1,'+ replicate vertex',IER,1)
            if(IADD.ge.1)then
              do 144 ij=1,IADD
                CALL ADDVERT(icomp,IWHICH,'C',IER)
 144          continue
              ILEN=NTV
              IPACT=CREATE
              CALL EKPAGE(IPACT)
              MODGEO=.TRUE.
              MODIFY=.TRUE.
              MODBND=.TRUE.
              ZBFLG(ICOMP)=0.
            endif
          endif
        elseif(IRT.eq.5)then

C Select (possibly via mouse) and then edit.
          inpick=MIN0(NTV,MTV-NTV)
          CALL EPMENSV
          call EPKVERT(INPICK,IVLST,'Vertices to edit',
     &      'Select vertex to edit from list...',' ',14,ier)
          CALL EPMENRC
          if(inpick.ge.1)then
            do 145 ij=1,inpick
              ifoc=IVLST(ij)

C Present vertex coords for editing then parse data from HOLD.
              WRITE(HOLD,'(1x,3f10.5)')X(IFOC),Y(IFOC),Z(IFOC)
              H(1)='The vertex coordinates should be in metres. '
              H(2)='It is not advisable for Z to be negative.'
              write(outs,'(a,i2,a)')
     &          ' Vertex (',ifoc,') X  Y  Z (in metres):'
 146          CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vertex coord',
     &          IER,2)
              K=0
              CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
              CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
              CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
              if(ier.ne.0)goto 146
              MODIFY=.TRUE.
              MODBND=.TRUE.
              MODGEO=.TRUE.
              MODLEN=.TRUE.
              ZBFLG(ICOMP)=0.
              call warnmod(ICOMP,'str')
 145        continue
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

C Present vertex coords for editing then parse data from HOLD.
        WRITE(HOLD,'(1x,3f10.5)')X(IFOC),Y(IFOC),Z(IFOC)
        H(1)='The vertex coordinates should be in metres. '
        H(2)='It is not advisable for Z to be negative.'
        write(outs,'(a,i2,a)')' Vertex (',ifoc,') X  Y  Z (in metres):'
  42    CALL EASKS(HOLD,outs,' ',32,' 0. 0. 0. ','vertex coord',IER,2)
        K=0
        CALL EGETWR(HOLD,K,X(IFOC),-999.9,999.9,'W','X cord',IER)
        CALL EGETWR(HOLD,K,Y(IFOC),-999.9,999.9,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,Z(IFOC),-9.9,999.9,'W','Z cord',IER)
        if(ier.ne.0)goto 42
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODGEO=.TRUE.
        MODLEN=.TRUE.
        ZBFLG(ICOMP)=0.
        call warnmod(ICOMP,'str')
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
C menu. ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C Make use of VERINFO to display the current vertex surface connections.
      SUBROUTINE EDVLIST(ITRC,ITRU,ICOMP,IER)
#include "building.h"
#include "epara.h"

      PARAMETER  (MPL = 200 )
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G2/NGL(MS)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),ZBFLG(MCOM)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      DIMENSION  COG1(3),COG2(3),COG3(3)
      DIMENSION  XXW(4),YYW(4),ZZW(4)
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      CHARACTER SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4,DESCRC*25
      LOGICAL MODIFY,MODLEN,MODBND,OK,bound,DOK

      DIMENSION VERT(36)
      DIMENSION  XX(MV),YY(MV),ZZ(MV),XT(MV),YT(MV),ZT(MV),jvn1(MV)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER VERT*34,KEY*1,HOLD*72,H*72,DESC*48,SN*12
      character head*32,zname*12,zdesc*64,HOLDS*36,outs*124
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16,SSPARENT*12
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12

C Initialise zone surface-vertex list menu size variables based on
C window size. IVERT is the menu position, MVERT the current
C number of menu lines.
      MHEAD=4
      MCTL=8
      ILEN=NSUR
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      ILEN=NSUR
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
      VERT(M+2)  ='+ add / insert / copy surfaces    '
      VERT(M+3)  ='* delete a surface                '
      VERT(M+4)  ='> transforms                      '
      VERT(M+5)  ='! browse surface-vertex topology  '
      VERT(M+6)  ='@ check surface-vertex topology   '
      VERT(M+7)  ='? help                            '
      VERT(M+8)  ='- exit to zone definition menu    '

C Do a bound check on the current zone and update display.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(ITRC,IER)

C Help text for this menu.
      H(1)='This menu allows existing vertices to be viewed and'
      H(2)='edited. Edit by selecting an item or -Exit to escape.'
      H(3)=' '
      H(4)='The `@ check vertex topology` looks for unbounded '
      H(5)='edges, edges with an adjacent edge ordered in the same'
      H(6)='direction (one surface probably reversed) and for '
      H(7)='missing intermediate vertices (where one edge is has'
      H(8)='more than one adjacent surface). '
      H(9)=' '
      H(10)='The `>` command to shift a surface along its normal '
      H(11)='axis updates vertices associated with that surface '
      H(12)='and thus surfaces sharing these vertices.  Useful '
      H(13)='for expanding/shrinking a zone. '
      H(14)=' '
      H(15)='NOTE: it is suggested that the first surface edge be'
      H(16)='horizontal (or close to horizontal) if at all possible.'
      H(17)='The shading analysis application gets confused if the'
      H(18)='first edge of a surface is vertical. Nothing in esp-r'
      H(19)='warns you of this so get into a consistent habit of '
      H(20)='avoiding vertical first edges in surfaces. '

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
        CALL PHELPD('vertex section',20,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Check vertex topology.
        call tstamp('>','PRJ: check zone vertex topology')
        call ckvert(1,icomp,bound,iub,inv,ier)
      ELSEIF(IVERT.EQ.(MVERT-3))THEN
        CALL VERINFO(ITRU)
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

        H(1)='Actions: '
        H(2)=' Shift surface along (+=outwards) surface normal.'
        H(3)=' Transform vertices by X,Y,Z metres.'
        H(4)=' Rotate surface about one of its vertices.'
        H(5)=' Invert ordering (to swop front & back faces).'
        H(6)=' Do several operations to the surface.'
        H(7)=' Continue with no action.'
        call ZSID(icomp,IS,ZSDES,ZSDESC,ZSDESS)
        write(outs,'(a,a)') ' Actions to take on ',ZSDES
        CALL EASKATOG(outs,' :','shift along normal','transform xyz',
     &    'rotate','invert ','combination','continue ?',' ',IRT,7)
        if(IRT.eq.1.or.IRT.eq.5)then

C Shift along normal and copy transformed points back to XX,YY,ZZ array.
          call tstamp('>','PRJ: do surface shift along normal')
          H(1)='Distance along the normal (+=out) in metres.'
          vdis=0.0
          CALL EASKR(vdis,' Distance along the normal (metres): ',
     &      '(0.0 = none) :',-9.999,'F',9.999,'W',0.0,'d along normal',
     &      IER,1)
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
          HOLDS= ' 0.  0.  0. '
 152      CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &      36,' 0. 0. 0.  ','offsets',IER,9)
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

C Rotate around a point in the surface.
          call tstamp('>','PRJ: do surface rotation')
          H(1)='The rotation is applied around one vertex of'
          H(2)='the surface. The index is to the 1st, 2nd, 3rd,'
          H(3)='vertex in the surface. '
          CALL EASKI(IRV,'Rotate about which vertex index: ',
     &       '(ie. 1st, 2nd or 3rd in the surface) ',
     &       1,'F',N,'F',1,'rotation anchor',IER,3)
          H(1)='The rotation is applied with positive'
          H(2)='being anticlockwise. '
          ANG=0.
          CALL EASKR(ANG,' ',' Rotation (deg + = anticlockwise) ? ',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,2)
          if(ANG.LT.-.01.OR.ANG.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANG*PI/180.
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
          CALL ADJVIEW(ITRC,IER)
          goto 92
        endif
        if(IRT.eq.6)then
          MODIFY=.FALSE.
          goto 92
        endif

C Check bounds.
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
        CALL ADJVIEW(ITRC,IER)

C Show these points as small circles...
        call edisp(iuout,' proposed points are shown as dots....')
        DO 154 J = 1,N
          COG1(1)=XX(J)
          COG1(2)=YY(J)
          COG1(3)=ZZ(J)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),ix,iy)
          CALL ecirc(ix,iy,3,1)
  154   CONTINUE
        call forceflush()

C Then draw the revised and if acceptable then used transformed
C points within the zone in place of the originals....
        DOK=.true.
        h(1)='If you say no then no changes to the zone geometry.'
        CALL ASKOK('Apply the transformed points to the',
     &             'surface (and any attached surfaces)?',OK,DOK,1)
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
        iopt=0
        CALL ADDSUR(ITRC,ICOMP,IS,'D','V',iopt,IER)
        if(LASTS.ne.NSUR)then
          CALL EMKCFG('-',IER)
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
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
        CALL ADJVIEW(ITRC,IER)
      ELSEIF(IVERT.EQ.(MVERT-6))THEN

C Add/insert a surface.
   30   H(1)='Insertion into an existing surface is a common way '
        H(2)='to create a door or window opening. '
        H(3)=' '
        H(4)='You may copy (and then transform) a surface within this '
        H(5)='zone to make a new surface. An example an identical shape'
        H(6)='which is repeated in several locations. '
        H(7)=' '
        H(8)='You may copy (and then transform) a surface from another '
        H(9)='zone for use in this zone. For example, a partition which'
        H(10)='shared between two zones. '
        H(11)=' '
        H(12)='You may create a rectangular surface by specifying its'
        H(13)='origin and either an azimuth (for vertical) or a '
        H(14)='rotation between the X axis and its `length edge` for.'
        H(15)='a horizontal rectangle. '
        H(16)=' '
        H(17)='Creating a new surface with existing vertices can be '
        H(18)='done by either entering the indices of each vertex in '
        H(19)='turn (anticlockwise looking from the outside or clock-'
        H(20)='wise looking from the inside). '
        H(21)=' '
        H(22)='NOTE: it is suggested that the first surface edge be'
        H(23)='horizontal (or close to horizontal) if at all possible.'
        H(24)='The shading analysis application gets confused if the'
        H(25)='first edge of a surface is vertical. Nothing in esp-r'
        H(26)='warns you of this - so get into a consistent habit of'
        H(27)='avoiding vertical first edges in surfaces! '
        H(28)=' '
        H(29)='If you are working in a graphics display you can use'
        H(30)='mouse clicks on the existing vertices.'

        irt=0
        ilrt=irt
        idrt=3
        call MENUATOL(' ','new surface options',
     &   'a made from existing vertices',
     &   'b made from existing vertices (mouse)',
     &   'c inserted into a surface',
     &   'd copy surface(s) in this zone',
     &   'e copy surface(s) from another zone ',
     &   'f vertical rectangle (origin&azim)',
     &   'g horizontal rectangle (origin&rot)',' ',
     &   ' ',' ',' ',' ',irt,idrt,30)
        call usrmsg(' ',' ','-')
        IF(ilrt.eq.irt)GOTO 92
        IF(irt.eq.0.or.irt.eq.8)GOTO 92
        LASTS=NSUR
        if(IRT.eq.1)then
          call tstamp('>','PRJ: insert surface via vertex')
          iopt=0
          CALL ADDSUR(ITRC,ICOMP,0,'A','V',iopt,IER)
        elseif(IRT.eq.2)then
          call tstamp('>','PRJ: insert surface via vertex')
          iopt=1
          CALL ADDSUR(ITRC,ICOMP,0,'A','V',iopt,IER)
        elseif(IRT.eq.3)then
          CALL EPMENSV
          call tstamp('>','PRJ: insert surface into another')
          CALL EASKSUR(ICOMP,IS,'-','Select surface to insert into.',
     &        ' ',IER)
          CALL EPMENRC
          IF(IS.EQ.0)RETURN

C Add/insert a surface, return to higher level menu when done.
C The 'ii' request an interactive insertion, the parameters
C XO,ZO,XW,ZH are not used in an interactive insert.
          XO=0.
          ZO=0.
          XW=0.
          ZH=0.
          CALL INSREC(ITRC,ITRU,ICOMP,IS,'ii',XO,ZO,XW,ZH,IER)
        elseif(IRT.eq.4)then
          call tstamp('>','PRJ: copy local surfaces')
          iopt=2
          CALL ADDSUR(ITRC,ICOMP,0,'C','V',iopt,IER)
        elseif(IRT.eq.5)then
          call tstamp('>','PRJ: copy another zone surfaces')
          iopt=1
          CALL ADDSUR(ITRC,ICOMP,0,'C','V',iopt,IER)
        elseif(IRT.eq.6)then

C Get vertical rectangular surface orgin.
          IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the zone too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add vertical surface')
          H(1)='The origin of the surface will be its lower left hand '
          H(2)='corner when viewed from the outside. '
          HOLDS= ' 0.  0.  0. '
          CALL EASKS(HOLDS,' Surface origin X Y & Z: ',' ',
     &      36,' 0. 0. 0.  ','origin XYZ',IER,2)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-50.0,50.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-50.0,50.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-50.0,50.0,'W','Z org',IER)

C Get azimuth and length and height.
          HOLDS= ' 180.0   1.0  1.0 '
          h(1)='Azimuth 0.0 is north, 180 is south, 270 is west. '
          h(2)='Length and height are in metres.'
          CALL EASKS(HOLDS,' Surface azimuth (deg) length & height: ',
     &      ' ',36,' 180.0 1.0 1.0  ','azim length height',IER,2)
          K=0
          CALL EGETWR(HOLDS,K,VALA,0.0,360.0,'W','Azim',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

C Set first coord at origin (lower left).
          XXW(1) = VALOX
          YYW(1) = VALOY
          ZZW(1) = VALOZ

C Set second coord along DDX metres at VALA-90.
          PI = 4.0 * ATAN(1.0)
          RAD = PI/180.
          RYAZI = (VALA-90.0)*RAD
          RSALT = 0.00
          z3 = DDX*SIN(RSALT)
          XYDIS = DDX*COS(RSALT)
          IF (ABS(XYDIS) .LT. 1E-6)THEN
            x3 = 0.
            y3 = 0.
          ELSE
            x3 = XYDIS*SIN(RYAZI)
            y3 = XYDIS*COS(RYAZI)
          ENDIF
          XXW(2)=x3+XXW(1)
          YYW(2)=y3+YYW(1)
          ZZW(2)=z3+ZZW(1)

C Third point is as second with Z adjusted. Fourth point is as first with
C Z adjusted.
          XXW(3)=XXW(2)
          YYW(3)=YYW(2)
          ZZW(3)=ZZW(2)+DDZ
          XXW(4)=XXW(1)
          YYW(4)=YYW(1)
          ZZW(4)=ZZW(1)+DDZ
        elseif(IRT.eq.7)then

C Get horizontal rectangular surface orgin.
          IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV))THEN
            CALL USRMSG(' A new surface could not be added as',
     &        ' it will make the zone or surface too complex!','W')
            RETURN
          ENDIF
          call tstamp('>','PRJ: add horizontal surface')
          H(1)='The origin of the surface will be its lower left hand '
          H(2)='corner when viewed from the outside. '
          HOLDS= ' 0.  0.  0. '
          CALL EASKS(HOLDS,' Surface origin X Y & Z: ',' ',
     &      36,' 0. 0. 0.  ','origin XYZ',IER,2)
          K=0
          CALL EGETWR(HOLDS,K,VALOX,-50.0,50.0,'W','X org',IER)
          CALL EGETWR(HOLDS,K,VALOY,-50.0,50.0,'W','Y org',IER)
          CALL EGETWR(HOLDS,K,VALOZ,-50.0,50.0,'W','Z org',IER)

C Get rotation and length and height.
          HOLDS= ' 0.0    1.0  1.0 '
          h(1)='Rotation is in relation to the first edge along the '
          h(2)='`length` edge and the X axis (positive is anticlockwise'
          h(3)='and negative is clockwise.'
          CALL EASKS(HOLDS,' Surface rotation (deg) length & height: ',
     &      ' ',36,' 180.0 1.0 1.0  ','rotation length height',IER,3)
          K=0
          CALL EGETWR(HOLDS,K,ANG,0.0,360.0,'W','Rot',IER)
          CALL EGETWR(HOLDS,K,DDX,0.0,50.0,'W','length',IER)
          CALL EGETWR(HOLDS,K,DDZ,0.0,50.0,'W','width',IER)

C Set first coord at origin (lower left) and the unrotated other corners.
          XXW(1) = VALOX
          YYW(1) = VALOY
          ZZW(1) = VALOZ
          XXW(2) = VALOX+DDX
          YYW(2) = VALOY
          ZZW(2) = VALOZ
          XXW(3) = VALOX+DDX
          YYW(3) = VALOY+DDZ
          ZZW(3) = VALOZ
          XXW(4) = VALOX
          YYW(4) = VALOY+DDZ
          ZZW(4) = VALOZ

C Now rotate around the first coordinate.
          PI = 4.0 * ATAN(1.0)
          if(ANG.LT.-.01.OR.ANG.GT..01)then
            A=-ANG*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=XXW(1)
            YY1=YYW(1)
            DO 200 I=1,4
              XXX=XXW(I)-XX1
              YYY=YYW(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              XXW(I)=XR+XX1
              YYW(I)=YR+YY1
  200       continue
          endif
        endif
        if(IRT.eq.6.or.IRT.eq.7)then

C Display the new rectangle.
          IAPNT = 1
          DO 350 J = 1,4
            AX(J) = XXW(J)
            AY(J) = YYW(J)
            AZ(J) = ZZW(J)
            IANXT(J) = J + 1
  350     CONTINUE
          IANXT(4) = IAPNT
          CALL MATPOL(4,IAPNT,AX,AY,AZ,IANXT,TEMAT,
     &                SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
          CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
          IF(ISTAT .EQ. 1)goto 32
          IF(ISTAT .EQ.-1)CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)

C Draw the (perhaps clipped) rectangle.
          DO 302 JB = 1,NB
            IP = IABS(IBPNT)
            CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            call etplot(AAX,AAY,0,0)

C Draw succeeding points until last vertex.
            IP1 = IBNXT(IP)
 451        CONTINUE
            CALL ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
            call etplot(AAX,AAY,1,0)

            IP1 = IBNXT(IP1)
            IF(IP1 .NE. IP) GOTO 451

C Complete rec.
            CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
            call etplot(AAX,AAY,1,0)
  302     CONTINUE
   32     CONTINUE
          CALL EDISP(ITRU,
     &      ' New coords: (lower left, lower right, up right, up left')
          WRITE(OUTS,'(a,4F8.3)') ' X coords:',(XXW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)
          WRITE(OUTS,'(a,4F8.3)') ' Y coords:',(YYW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)
          WRITE(OUTS,'(a,4F8.3)') ' Z coords:',(ZZW(I),I=1,4)
          CALL EDISP(ITRU,OUTS)

C Confirm opening.
          h(1)='If you accept the rectangle it will be converted into '
          h(2)='a surface within the zone. If you elect to revise it'
          h(3)='you will be presented with several options. '
          CALL EASKABC('Options for this rectangle:',' ',
     &       'accept position','revise position','abort',INVT,2)
          if(INVT.eq.2)then
            ITVNO=0
            ITSNM=0
            MODIFY=.TRUE.
            CALL INLNST(1)
            nzg=1
            nznog(1)=ICOMP
            izgfoc=ICOMP
            CALL ADJVIEW(ITRC,IER)
            GOTO 30
          elseif(INVT.eq.3)then
            return
          endif

C Now update the appropriate common data for the new surface. The
C following code is similar to that in addsur in edzone.f.
C ICONT is the connection for the surface. Because the surface 
C to be added is not yet in the connections list find ICONT
C its future position via `one connection past the current last 
          ICONT=IZSTOCN(icomp,nsur)+1
          NZSUR(ICOMP)=NZSUR(ICOMP)+1
          NSUR=NSUR+1
          is=nsur
          NVER(NSUR)=4
          DO 251 J = 1,4
            X(NTV+J)=XXW(J)
            Y(NTV+J)=YYW(J)
            Z(NTV+J)=ZZW(J)
            JVN(NSUR,J)=NTV+J
  251     CONTINUE
          NTV=NTV+4
          NZTV(icomp)=NTV
          SN=' '
          CALL EASKS(SN,' ',' Inserted surface name: ',
     &      12,'new_door','surface name',IER,0)
          call st2name(SN,SNAME(ICOMP,NSUR))

          SOTF(NSUR)='OPAQ'
          if(IRT.eq.5) SVFC(NSUR)='VERT'
          if(IRT.eq.6) SVFC(NSUR)='CEIL'
          SOTHER(NSUR)='UNKNOWN'
          CALL EPMENSV
          CALL EPKMLC(ISEL,'Select one of the constructions for the',
     &      'inserted surface. ',IER)
          CALL EPMENRC
          IF(ISEL.GT.0)then
            WRITE(SMLCN(NSUR),'(A12)')DESC(ISEL)(1:12)
            IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(NSUR)='TRAN'
            IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(NSUR)='OPAQ'
          endif
          NGL(NSUR)=0

C Add surface to the connection list (icont) assuming an unknown boundary. 
          if(NCON.LT.MCON)then
            NCON=NCON+1
            IX=NCON+1

C Update common blocks G6 & G7
            call adjcbg6g7('+',ix,icont)

            IC1(ICONT)=ICOMP
            IE1(ICONT)=NSUR
            ICT(ICONT)=0
            IC2(ICONT)=0
            IE2(ICONT)=0
            SSNAME(ICONT)=SNAME(ICOMP,NSUR)
            SSOTF(ICONT)=SOTF(NSUR)
            SSMLCN(ICONT)=SMLCN(NSUR)
            SSVFC(ICONT)=SVFC(NSUR)
            SSOTHER(ICONT)=SOTHER(NSUR)
            IZSTOCN(icomp,nsur)=icont

C Now update the G7 common block to reflect additional surface.
            call zgupdate(1,icomp,ier)
          endif
        endif
        if(LASTS.ne.NSUR)then
          CALL EMKCFG('-',IER)
          CALL EMKGEO(IFIL+2,LGEOM(ICOMP),ICOMP,ITRU,3,IER)
          call warnmod(ICOMP,'sf+')
        endif
        ILEN=NSUR
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Update the image.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        ZBFLG(ICOMP)=0.
        call usrmsg(' ',' ','-')
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITRC,IER)
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
        CALL ADJVIEW(ITRC,IER)

        H(1)='The list of associated vertices should be given in'
        H(2)='anticlockwise order as one faces the outside of the'
        H(3)='surface.  If one is looking at the inside face then'
        H(4)='think of the list in clockwise order.  It is a con-'
        H(5)='vention that the first vertex is located at the'
        H(6)='bottom-left corner of the surface (looking from the'
        H(7)='outside) with the next vertex moving horizontally.'
        H(8)='It helps to follow a consistent pattern!'
   93   HOLD=' '
        NV=NVER(IFOC)

C Incrementally write out vertex information (within 72 chars).
C Use alternative logic if NTV >99
        ix=1
        lc=2
        do 42 j=1,nv
          ixl=ix+lc
          if(j.lt.NV.and.ixl.le.72)then
            write(hold(ix:ixl),'(i3)')JVN(IFOC,j)
            if(JVN(IFOC,j).le.99)then
              ix=ix+lc+1
            else
              ix=ix+lc+1+1
            endif
          elseif(j.eq.NV.and.ixl.le.72)then
            write(hold(ix:ixl),'(i3)')JVN(IFOC,j)
          else
            continue
          endif
  42    continue
  43    CALL EASKS(HOLD,' Associated vertices: ',
     &     '  ',72,' ','associated vertices',IIER,8)
        CALL CHITMS(hold,NV)
C debug        write(6,*) 'found nv ',NV
        K=0
        DO 94 J=1,NV

C Read an index, check if within range and if so add to JVN().
          CALL EGETWI(hold,K,JV,1,NTV,'W','vertex list',IIER)
          IF(IIER.NE.0)GOTO 93
          if(JV.eq.0.or.JV.gt.NTV)then
            call edisp(iuout,
     &        'At least one of the vertex indices was out of range!')
            goto 43
          endif
          JVN(IFOC,J)=JV
   94   CONTINUE
        NVER(IFOC)=NV
        MODIFY=.TRUE.
        call warnmod(ICOMP,'str')
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
C IER=0 OK, IER=1 problem.
      SUBROUTINE EPKVERT(INPICK,IVLST,TITLE,PROMPT1,PROMPT2,NHELP,IER)
#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      integer CREATE,EDIT
      PARAMETER (CREATE=1,EDIT=20)
      DIMENSION  item(36)
      DIMENSION  COG1(3),COG2(3),COG3(3),IVLST(MTV)
      CHARACTER*(*) TITLE,PROMPT1,PROMPT2
      CHARACTER item*33,outs*124,KEY*1
      logical found

C Initialise vertex list menu size variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
C Also clear tagged items list (IVLST).
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
   92 IER=0
      ILEN=NTV
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
        write(item(M+2),'(2x,a,i2,a)') '(',IALLOW,' items)'
      else
        write(item(M+2),'(a,i2,a)')'* select via mouse (',IALLOW,
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
            COG1(1)=X(I)
            COG1(2)=Y(I)
            COG1(3)=Z(I)
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call u2pixel(COG3(1),COG3(2),ix,iy)
            ixd=ix-ixx
            iyd=iy-iyy
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
              CALL ecirc(ix,iy,3,0)
              call forceflush()
              goto 46
            endif
  45      continue
          if(.NOT.found)then

C If there were only two points to find, check if user clicked on edge.
            if(inpick.eq.2)then
              write(6,*) 'code for edge check in progress'
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

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/C1/NCOMP,NCON
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
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
#include "espriou.h"

      common/FILEP/IFIL

C CTYPE (3 char) - zone shape type (REC, REG or GEN)
      COMMON/G0/CTYPE

C X,Y & Z   - coordinates of the vertices defining the zone
C NSUR      - Number of faces comprising zone
C JVN       - contains an ordered description (anticlockwise) of the
C             vertices in each face
C NTV       - total number of vertices in zone
C NVER      - contains the number of vertices in each face
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C NGL       - contains the number of windows in each face (set to zero)
C             this is depreciated as the default window data structure is no
C             longer used.        
      COMMON/G2/NGL(MS)

C Default solar distribution and shading directives.
      COMMON/G4/NDP(MCOM),IDPN(MCOM,3)

C SNAME (12 char) - surface name attribute
C ZBASEA    - area of base (m^2)
C IBASES    - surfaces (up to 6) associated with base area. If all 0 then
C             base area has been supplied by the user.
C SOTF (4 char) - surface attribute indicating OPAQ or TRAN.
C SVFC (4 char) - string indicating whether the surface is to be
C     counted as a FLOR (floor), VERT (wall), CEIL (ceiling),
C     SLOP (not vertical) for purposes of floor area and daylighting.
C SMLCN (12 char) surface construction attribute.
C SOTHER (15 char) surface attribute for `other` side. UNKNOWN indicates
C     that no attribute has been set. EXTERIOR or SIMILAR or ADIABATIC or
C     BASESIMP or GROUND or a zone name are allowed.
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)

C ZNAME (12 char) - the zone name.
C ZDESC ( char) - zone notes.
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C ZBASEA is the floor area of the zone, IBASES is a list of surfaces
C which make up the floor, IUZBASEA signals that the user selected
C specific surfaces (two), has edited the floor area (one) or it was
C calculated based on FLOR orientations (zero).
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
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
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12
      CHARACTER OUTSTR*124,WORD*20,CTYPE*3,zname*12,SOTHER*15
      character lzip*72,dfile*72
      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4 
      character ZN*12,phrasea*64,phraseb*64,zdesc*64,outs*124
      character dllsubr*12,dllmesg*124,act*2
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,msg*48
      logical dll,checkbase

      IER=0
      ZN=' '
      phrasea=' '
      phraseb=' '
      checkbase=.false.

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
      write(6,*) 'the overall file name is ',WORD
      lwd=lnblnk(WORD)
      write(6,*) 'the file name is ',WORD(2:lwd-1)
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
        CTYPE=WORD(1:3)
        zname(ICOMP)=' '
        zdesc(ICOMP)=' '
      ELSEIF(ND.EQ.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        CTYPE=WORD(1:3)
        call egetrm(outstr,K,phrasea,'W','Z description',IER)
        zdesc(ICOMP)=' '
      ELSEIF(ND.gt.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','CTYPE',IFLAG)
        CTYPE=WORD(1:3)
        call egetrm(outstr,K,phrasea,'W','Z description',IER)

C Combine the comment from the first line with the one after the zone CTYPE.
        ipra=lnblnk(phrasea)
        iprb=lnblnk(phraseb)
        iprc= 63 - ipra
        iwidth=ipra + iprb + 1
        if(iwidth.lt.64)then
          write(ZDESC(ICOMP),'(3a)') phrasea(1:ipra),' ',
     &      phraseb(1:iprb)
        else
          write(ZDESC(ICOMP),'(3a)') phrasea(1:ipra),' ',
     &      phraseb(1:iprc)
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
      endif
      if(zdesc(ICOMP)(1:1).EQ.' ')then
        write(zdesc(ICOMP),'(a,a)') 
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),' describes a...'
      endif

C Zone is of type REC, look for 7 items on one or two lines.
      IF(CTYPE.EQ.'REC')THEN
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 2',IER)
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' REC line origin was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWR(OUTSTR,K,XO,0.,0.,'-','X origin',IER)
        CALL EGETWR(OUTSTR,K,YO,0.,0.,'-','Y origin',IER)
        CALL EGETWR(OUTSTR,K,ZO,0.,0.,'-','Z origin',IER)

C If only 3 items on first line read another OUTSTR and try to continue.
        IF(ND.EQ.3) THEN
          K=0
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'line 3',IER)
          IF(IER.NE.0)goto 1001
        ENDIF
        CALL EGETWR(OUTSTR,K,DX,0.,0.,'-','length',IER)
        CALL EGETWR(OUTSTR,K,DY,0.,0.,'-','width',IER)
        CALL EGETWR(OUTSTR,K,DZ,0.,0.,'-','height',IER)
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rotation angle',IER)

C Now convert to a gen description.
        CALL ERECC(XO,YO,ZO,DX,DY,DZ,AR)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE='GEN'

C Zone is of type REG.
      ELSEIF(CTYPE.EQ.'REG')THEN
        CALL STRIPC(IUNIT,OUTSTR,4,ND,1,'line 2',IER)
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' REG line data was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NW,3,MS-2,'W','no of walls',IER)
        CALL EGETWR(OUTSTR,K,Z1,0.,1000.,'W','floor height',IER)
        CALL EGETWR(OUTSTR,K,Z2,Z1,1000.,'F','ceiling ht',IER)
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rot angle',IER)

C Read base vertex data.
        DO 60 IW=1,NW
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'vertex data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          CALL EGETWR(OUTSTR,K,XX(IW),0.,0.,'-','Base X',IER)
          CALL EGETWR(OUTSTR,K,YY(IW),0.,0.,'-','Base Y',IER)
   60   CONTINUE

C Now convert to GEN body type and place into in common block.
        CALL EREGC(NW,Z1,Z2,XX,YY)
        IF(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
        endif
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CTYPE='GEN'

C Zone is of type GEN.
      ELSEIF(CTYPE.EQ.'GEN')THEN
        CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'NTV NSUR AR',IER)
        write(outs,'(3a)') 'The file ',lzip(1:lnblnk(lzip)),
     &    ' GEN line NTV NSUR AR was misunderstood.'
        IF(IER.NE.0)goto 1001
        K=0
        CALL EGETWI(OUTSTR,K,NTV,4,MTV,'F','no of vertices',IER)
        CALL EGETWI(OUTSTR,K,NSUR,3,MS,'F','no of surfaces',IER)
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        CALL EGETWR(OUTSTR,K,AR,-360.,360.,'W','rot angle',IER)

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

C Rotate if required.
        if(AR.LT.-.01.OR.AR.GT..01)then
          x1=X(1)
          y1=Y(1)
          CALL ESCROT(AR,x1,y1)
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
      DO 14 KS=1,NSUR
        NGL(KS)=0
   14 CONTINUE

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
      CALL FILSUR(itru,ICOMP,0)

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
        call zinfo(iuout,sna,zoa,pazi,pelv,vol)
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
     &     ZBASEA(ICOMP),'m^2 for ',zname(icomp),
     &    ' (probably an older file).'
        call edisp(itru,outs)
        if(lastlist.gt.0)iuzbasea(icomp)=0
      endif

C Update the connection list and then the model configuration.
      NCOMP=NCOMP+1
      NZSUR(NCOMP)=NSUR
      NZTV(NCOMP)=NTV
      NCCODE(NCOMP)=NCOMP
      INDUTL(NCOMP)=0
      LUTIL(NCOMP)=' '
      ICCC=NCON

C If surface attributes are adaibetic or similar then use these in
C the connections list, then remind user to double check topology. 
      DO 132 ICC=1,NSUR
        ICCC=ICCC+1
        IC1(ICCC)=icomp
        IE1(ICCC)=ICC
        if(SOTHER(icc)(1:8).eq.'EXTERIOR')then
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc)(1:7).eq.'SIMILAR')then
          ICT(ICCC)=1
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc)(1:9).eq.'ADIABATIC')then
          ICT(ICCC)=5
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc)(1:8).eq.'CONSTANT')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a CONSTANT connection. Use the'
          call usrmsg(outs,
     &      'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc)(1:6).eq.'GROUND')then
          write(outs,'(3a)') 'Note: ',SNAME(NCOMP,icc),
     &      ' is a GROUND connection. Use the'
          call usrmsg(outs,
     &     'surface attribute facility to confirm its details.','W')
          ICT(ICCC)=0
          IC2(ICCC)=0
          IE2(ICCC)=0
        elseif(SOTHER(icc)(1:7).eq.'UNKNOWN')then
          ICT(ICCC)=0
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
        SSVFC(iccc)=SVFC(icc)
        SSOTHER(iccc)=SOTHER(icc)
        IZSTOCN(ncomp,icc)=iccc
  132 CONTINUE
      NCON=ICCC

C Update the G7 common blocks and then write out configuration file.
      call zgupdate(0,NCOMP,ier)

C Make up a zone geometry file name for this new zone.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DFILE,'(A,A4)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.geo'
      else
        WRITE(DFILE,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.geo'
      endif
      LGEOM(icomp)= DFILE
      write(outs,'(A,A)') ' updating initial surfaces in ',zname(icomp)
      call usrmsg('  ',outs,'-')
      CALL EMKGEO(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)

C If there are default windows then attempt to insert them as
C transparent surfaces.
      if(nbdef.gt.0)then
        do 66 idw=1,nbdef
          isurf=iwhichsurfw(idw)
          act='sw'
          XO=VALX(idw)
          ZO=VALZ(idw)
          XW=VALW(idw)
          ZH=VALH(idw)
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO,ZO,XW,ZH,IER)
  66    continue
      endif

C If there are default doors, attempt to insert them as surfaces.
      if(nbdor.gt.0)then
        do 67 idd=1,nbdor
          isurf=iwhichsurfd(idd)
          act='sd'
          XO=VALDX(idd)
          ZO=0.0
          XW=VALDW(idd)
          ZH=VALDH(idd)
          call INSREC(ITRC,ITRU,ICOMP,isurf,act,XO,ZO,XW,ZH,IER)
  67    continue
      endif

C Save zone if more surfaces have been added.
      if(nbdef.gt.0.or.nbdor.gt.0)then
        CALL EMKGEO(IFIL+2,LGEOM(icomp),icomp,iuout,4,IER)
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
         ZN=' '
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

C 1001 write(outs,'(3a)') 'Conversion error in...',OUTSTR(1:50),'...'
 1001 continue
      if(dll)then
        dllsubr='EGOMIN'
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

