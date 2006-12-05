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

C The file edtopol.f is a collection of support facilities for 
C creating and editing the problem topology ie. connection lists. 

C edtopol.f contains the following:

C EDACONN: Edit connection attributes in common block C3.
C CONXMENU: returns an English description of the inter-connection 
C           information. 

C ************* EDACONN 
C Edit connection attributes in common block __ via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDACONN(ITRC,IER)
#include "epara.h"
#include "building.h"
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      common/ground/grdtmp(12,mgrdp)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/user/browse

      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,dok
      LOGICAL MODIFY,MODLEN,MODBND,UPDGEO,browse,modcfg,close,updoth

      DIMENSION VERT(35),TOTHER(MS)
      CHARACTER VERT*46,KEY*1,CXSTR*78,H*72,outs*124,DESCRC*25
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      character zname*12,zdesc*64,ZN*12,SO*12,TOTHER*12
      character CXITM*43


C Set for redraw if image on first entry (if refresh set to after
C each edit). Set flag for updating geometry file (for changed
C surface attribute.
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      modcfg=.false.

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=2
      MCTL=3
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          WRITE(VERT(M),'(A1,1x,A43)')KEY,CXITM
        ENDIF
   10 CONTINUE

C Set menu header text.
      VERT(1)=    ' conn|reference|conn|      connection     | '
      VERT(2)=    ' no. |zone|surf|type| zone/data|surf/data | '

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ___________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='? help                                '
      VERT(M+3)  ='- exit this menu                      '

C Help text for this menu.
      H(1) ='The problem contiguity (boundary conditions at surfaces)'
      H(2) ='are set in this menu.  Before a simulation can be'
      H(3) ='performed it is necessary that it be complete.'
      H(4) =' '
      H(5) ='There are several means to define the topology: '
      H(6) =' a) attribute surfaces via the zone geometry facility'
      H(7) ='    as they are created,'
      H(8) ='    '
      H(9) =' b) manually edit them via this list, '
      H(10)='    '
      H(11)=' c) search for matching vertices within the problem to'
      H(12)='    fill in all partitions and then complete this list'
      H(13)='    by editing or importing surface attributes.'
      H(14)=' '
      H(15)='The automated facility is considerably faster than'
      H(16)='manual methods and best used when all the zones have'
      H(17)='been created. '

C Now display the menu.
      CALL EMENU('Zone Boundary Conditions',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the menu.
        CALL PHELPD('connection section',17,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit item identified by KEYIND after reporting on current values.
        call tstamp('>','PRJ: enter topology edit')
        CALL KEYIND(MVERT,IVERT,IFOC,IO)

        CALL EDISP(iuout,' ')
        CALL EDISP(iuout,' Current connection description...')
        CALL CONXINF(1,0,CXSTR)
        write(outs,'(1X,A)') CXSTR
        CALL EDISP(iuout,outs)
        CALL CONXINF(1,IFOC,CXSTR)
        write(outs,'(1X,A)') CXSTR
        CALL EDISP(iuout,outs)

C Remember current surface attribute.
        WRITE(SO,'(A)')SOTHER(IE1(IFOC))(1:12)

C Display the reference zone (if type 3 then display both).
C Remember the topology attributes for reference zone.
        IUO=IFIL+1
        CALL EGOMIN(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),0,0,iuout,IER)
        do 322 IJ=1,NSUR
          write(TOTHER(IJ),'(a)') SOTHER(IJ)(1:12)
  322   continue
        MODIFY=.TRUE.
        MODBND=.TRUE.
        UPDGEO=.FALSE.

C Reset all surface lines to standard width and then the specific 
C surface(s) under consideration to wide lines. If other side
C has not a type 3 then don`t highlight.
        CALL INLNST(1)
        if(ICT(IFOC).eq.3)then
          LINSTY(IFOC)=2
          nzg=2
          nznog(1)=IC1(IFOC)
          nznog(2)=IC2(IFOC)
          CALL SURADJ(IC1(IFOC),IC2(IFOC),IE,TMP,IZC,ISC,IC,DESCRC)
          LINSTY(IC)=2
          izgfoc=IC1(IFOC)
          CALL ADJVIEW(ITRC,IER)
        else
          LINSTY(IFOC)=2
          nzg=1
          nznog(1)=IC1(IFOC)
          izgfoc=IC1(IFOC)
          CALL ADJVIEW(ITRC,IER)
        endif

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

C Editing of items.
        H(1)='The connection types are:'
        H(2)='0 - Exterior'
        H(3)='1 - Dynamic (similar to this surface) boundary'
        H(4)='2 - Static boundary conditions'
        H(5)='3 - Another participating zone'
        H(6)='4 - Ground (monthly profile)'
        H(7)='5 - Adiabatic boundary'
        H(8)='6 - BASESIMP foundation configuration'
        H(9)='7 - CEN 13791 partition which is like the SIMILAR'
        H(10)='    but for CEN 13791 test. The offset temperauture'
        H(11)='    and radiation (W/m^2) in SIMILAR is not available'
        H(12)='    for the CEN 13791 partition type. For details see'
        H(13)='    section 4.4.2 CEN 13791 standard. '
        write(outs,'(a,a,a)')' Connection (currently ',
     &    CXSTR(1:lnblnk(CXSTR)),' )'
        idno=ICT(IFOC)
        iart=0
        call MENUATOL(outs,' Connection options ',
     &  'a exterior                  ','b dynamic (similar) boundary',
     &  'c static boundary conditions','d surface in another zone   ',
     &  'e ground (standard profile) ','f ground (user def profile) ',
     &  'g ground (3D conduction)    ','h adiabatic (no heat flux)  ',
     &  'i BASESIMP foundation       ','j CEN 13791 partition       ',
     &  'k continue ',' ',iart,idno,13)
        if(iart.eq.0)then
          continue
        elseif(iart.eq.1)then
          modcfg=.true.
          ICT(IFOC)=0
          IE2(IFOC)=0
          IC2(IFOC)=0
          if(SO(1:8).ne.'EXTERIOR')then
            TOTHER(IE1(IFOC))='EXTERIOR'
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.2)then 
          modcfg=.true.
          ICT(IFOC)=1
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
          IC2(IFOC)=INT(VALT)
          IE2(IFOC)=INT(VALW)
          if(SO(1:7).ne.'SIMILAR')then
            TOTHER(IE1(IFOC))='SIMILAR'
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.3)then 
          modcfg=.true.
          ICT(IFOC)=2
          H(1)='A type 2 connection requires that a constant '
          H(2)='temperature and radiation (Watts) be entered.'
          CALL EASKR(VAL,' ',' Constant temperature ? ',
     &      -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
          IC2(IFOC)=INT(VAL)

          CALL EASKR(VAL,' ',
     &    ' Constant radiation source (Watts) ? ',
     &      0.0,'W',99999.,'W',0.0,'adjacent radiation source',IER,2)
          IE2(IFOC)=INT(VAL)
          if(SO(1:8).ne.'CONSTANT')then
            TOTHER(IE1(IFOC))='CONSTANT'
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.4)then 

C Find out which other zone to connect to. Save the current state of the
C menu so that can return to where we were. Also connected surface.
          CALL EPMENSV
          CALL EASKGEOF('Select zone on other side:',CFGOK,IZ,'-',IER)
          CALL EPMENRC

C Reset all surface lines to std width except for current
C and display both zones.
          MODIFY=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          LINSTY(IFOC)=2
          nzg=2
          nznog(1)=IC1(IFOC)
          nznog(2)=IZ
          izgfoc=IC1(IFOC)
          CALL ADJVIEW(0,IER)
          MODIFY=.TRUE.
          MODBND=.TRUE.

          IS=1
          CALL EPMENSV
          CALL EASKSUR(IZ,IS,'-','Select surface in the adjacent',
     &        'zone. ',IER)
          CALL EPMENRC
          if(IS.GT.0.AND.IZ.GT.0)then
            modcfg=.true.
            ICT(IFOC)=3
            IC2(IFOC)=IZ
            IE2(IFOC)=IS
            ZN=zname(IZ)
            if(SO(:lnblnk(SO)).ne.ZN(:lnblnk(ZN)))then
              TOTHER(IE1(IFOC))=ZN
              UPDGEO=.TRUE.
            endif

C If applicable find other surface, return it`s connection number and
C point it to the current surface.
            updoth=.false.
            dok=.true.
            h(1)='Where a pair of surfaces form a partition between'
            h(2)='two zones changes in one sides attributes should'
            h(3)='be reflected in the other side to maintain model'
            h(4)='consistency. Usually you want to update the other'
            h(5)='side.  '
            CALL ASKOK(' ','Update other side?',updoth,dok,5)
            if(updoth)then
              CALL SURADJ(IZ,IS,IE,TMP,IZC,ISC,ICO,DESCRC)
              IC1(ICO)=IZ
              IE1(ICO)=IS
              ICT(ICO)=3
              IC2(ICO)=IC1(IFOC)
              IE2(ICO)=IE1(IFOC)
            endif
          else
            call edisp(iuout,' ')
            call edisp(iuout,
     &      ' Insufficient information given to complete')
            call edisp(iuout,' this connection.  Resetting to EXTERIOR')
            modcfg=.true.
            ICT(IFOC)=0
            IE2(IFOC)=0
            IC2(IFOC)=0
          endif
        elseif(iart.eq.5)then 

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
21        continue
          modcfg=.true.
          ICT(IFOC)=4
          H(1)='A ground connection allows either:'
          H(2)='> 0 a connection to one of several standard profiles'
          H(3)='    in which case the profile number is required.'
          H(4)='= 0 a connection to a user defined profile in which'
          H(5)='    case a 0 followed by a profile number is '
          H(6)='    required.'
          H(7)='-3  a connection to a 3D ground model.'
          iic2=ic2(IFOC)
  777     CALL EASKI(IIC2,' Monthly (standard) profile index? ',
     &                  ' ',1,'F',12,'F',1,'ground profile',IER,7)
          IF(IIC2.GT.0)THEN
            IC2(IFOC)=iic2
            IE2(IFOC)=0
            if(SO(1:6).ne.'GROUND')then
              TOTHER(IE1(IFOC))='GROUND'
              UPDGEO=.TRUE.
            endif
          ELSE
            CALL USRMSG(' ',' Unacceptable ground profile...','W')
            GOTO 777
          ENDIF
        elseif(iart.eq.6)then 

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
          if(NGRDP.gt.0)then
            CALL EDISP(iuout,' ')
            do 22 igrdp=1,NGRDP    
              CALL EDISP(iuout,'User defined ground profile Jan-Dec:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
 22         continue
            modcfg=.true.
            ICT(IFOC)=4
            H(1)='One of the user defined profile numbers is'
            H(2)='required at this point.'
            CALL EASKI(IIE2,' User defined ground profile index ?',
     &        ' ',0,'F',9,'F',1,'user defined profile',IER,2)
            IC2(IFOC)=0
            IE2(IFOC)=iie2
            if(SO(1:6).ne.'GROUND')then
              TOTHER(IE1(IFOC))='GROUND'
              UPDGEO=.TRUE.
            endif
          else
           call usrmsg(
     &        'No user defined ground profiles have been found.',
     &        'Go to `problem context` to define these.','W')
          endif
        elseif(iart.eq.7)then 

C Link to a 3D conduction model..
          modcfg=.true.
          ICT(IFOC)=4
          IC2(IFOC)=-3
          IE2(IFOC)=0
          if(SO(1:6).ne.'GROUND')then
            TOTHER(IE1(IFOC))='GROUND'
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.8)then 
          modcfg=.true.
          IE2(IFOC)=0
          IC2(IFOC)=0
          if(SO(1:9).ne.'ADIABATIC')then
            TOTHER(IE1(IFOC))='ADIABATIC'
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.9)then 

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype. Save state of current
C menu and recover after the call.
          ICT(IFOC)=6
          H(1)='Specify the BASESIMP configuration number (iconfig). '
          H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
          H(3)='correspondence between iconfig and standard BASESIMP '
          H(4)='descriptors (e.g. BCIN_1).'
          CALL EPMENSV
          IBS=IC2(IFOC)
          call bsimtype(ibs)
          CALL EPMENRC
          IC2(IFOC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
          Ifrac=IE2(IFOC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
          CALL EASKI(Ifrac,' Percent of BASESIMP loss to this surface?'
     &       ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',IER,6)
          IE2(IFOC)=Ifrac
          if(SO(1:8).ne.'BASESIMP')then
            TOTHER(IE1(IFOC))='BASESIMP'
            UPDGEO=.TRUE.
          endif
C BASESIMP end.

C CEN 13791 partition start
        elseif(iart.eq.10)then 
          modcfg=.true.
          ICT(IFOC)=7

          VALT=0.0
          VALW=0.0
          IC2(IFOC)=INT(VALT)
          IE2(IFOC)=INT(VALW)
          if(SO(1:7).ne.'IDENT_CEN')then
            TOTHER(IE1(IFOC))='IDENT_CEN'
            UPDGEO=.TRUE.
          endif
C CEN 13791 partition end

        elseif(iart.eq.11)then
          goto 3
        endif 

C Update zone geometry if attributes have changed.
        if(browse)goto 3
        if(UPDGEO)then
          call usrmsg(' Updating problem...',' ','-')
          CALL EGOMIN(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),0,0,iuout,IER)
          do 323 IJ=1,NSUR
            SOTHER(IJ)=TOTHER(IJ)
  323     continue
          CALL EMKGEO(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),iuout,3,IER)
          IF(IER.NE.0)CALL USRMSG(' ',
     &           ' Problem updating surface attribute...','W')
        endif

C If user requested other side to be updated do this as well. Read
C in the zone pointed to by IC2, update SOTHER (of index IE2 in the
C other zone) and write out that zones geometry.
        if(updoth)then
          call usrmsg(' Updating other side...',' ','-')
          CALL EGOMIN(IUO,LGEOM(IC2(IFOC)),IC2(IFOC),0,0,iuout,IER)
          SOTHER(IE2(IFOC))=zname(IC1(IFOC))
          CALL EMKGEO(IUO,LGEOM(IC2(IFOC)),IC2(IFOC),iuout,3,IER)
          IF(IER.NE.0)CALL USRMSG(' ',
     &           ' Problem updating other surface attribute...','W')
        endif
        if(modcfg)then
          CALL EMKCFG('-',IER)
          modcfg=.false.
          call usrmsg(' Updating problem...done.',' ','-')
        endif
      ELSE

C Not one of the legal menu choices.
        goto 92
      ENDIF
      IVERT=-2
      goto 3

      END 

C **************** CONXMENU 
C CONXMENU returns an English description in CXITM of the inter-connection 
C information. 
      SUBROUTINE CONXMENU(ICON,CXITM)
#include "building.h"
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      CHARACTER CXITM*43,SST1*12,ZST1*12,SST2*12,ZST2*12,zdesc*64
      CHARACTER zname*12,SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4

      CXITM=' '
      SST1=SNAME(IC1(ICON),IE1(ICON))
      lsn1=MIN0(lnblnk(SST1),8)
      ZST1=zname(IC1(ICON))
      lzn1=MIN0(lnblnk(ZST1),8)
      IF(ICT(ICON).EQ.0)THEN
        if((lsn1+lzn1+21).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< External'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< External'
        endif
      ELSEIF(ICT(ICON).EQ.1)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< Identical'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< Identical'
        endif
      ELSEIF(ICT(ICON).EQ.2)THEN
        if((lsn1+lzn1+28).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a,I3,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< Constant @',IC2(ICON),'dC'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a,I3,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< Constant @',IC2(ICON),'dC'
        endif
      ELSEIF(ICT(ICON).EQ.3)THEN
        SST2=SNAME(IC2(ICON),IE2(ICON))
        lsn2=MIN0(lnblnk(SST2),8)
        ZST2=zname(IC2(ICON))
        lzn2=MIN0(lnblnk(ZST2),8)
        if((lsn1+lzn1+lsn2+lzn2+17).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a,a,a,a)')ICON,SST1(:lsn1),' in ',
     &          ZST1(:lzn1),' >|< ',SST2(:lsn2),' in ',ZST2(:lzn2)
        elseif((lsn1+lzn1+lsn2+lzn2+15).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a,a,a,a)')ICON,SST1(:lsn1),' : ',
     &          ZST1(:lzn1),' >|< ',SST2(:lsn2),' : ',ZST2(:lzn2)
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a,a,a,a)')ICON,SST1(:lsn1),':',
     &          ZST1(:lzn1),' >|< ',SST2(:lsn2),':',ZST2(:lzn2)
        endif
      ELSEIF(ICT(ICON).EQ.4)THEN
        IF(IC2(ICON).GT.0)THEN
          if((lsn1+lzn1+19).lt.43)then
            WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &        ZST1(:lzn1),' >|< ground'
          else
            WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &        ZST1(:lzn1),' >|< ground'
          endif
        ELSE
          if((lsn1+lzn1+26).lt.43)then
            WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &        ZST1(:lzn1),' >|< user def grnd'
          else
            WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &        ZST1(:lzn1),' >|< user def grnd'
          endif
        ENDIF
      ELSEIF(ICT(ICON).EQ.5)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< adiabatic'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< adiabatic'
        endif
      ELSEIF(ICT(ICON).EQ.6)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< BASESIMP'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< BASESIMP'
        endif
      ELSEIF(ICT(ICON).EQ.7)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),' in ',
     &      ZST1(:lzn1),' >|< IDENT_CEN'
        else
          WRITE(CXITM,'(I3,1x,a,a,a,a)') ICON,SST1(:lsn1),':',
     &      ZST1(:lzn1),' >|< IDENT_CEN'
        endif
      ENDIF

      RETURN
      END

