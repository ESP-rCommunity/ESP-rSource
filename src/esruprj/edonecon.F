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
C SFCRNR:  finds the total number of corners in a surface and
C          stores their coordinates.
C LINDIR:  finds the angles associated with a line in space.

C ************* EDACONN 
C Edit connection attributes in common block __ via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDACONN(IER)
#include "epara.h"
#include "building.h"
#include "model.h"
#include "site.h"

C geometry.h provides commons G0 and G2.
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      common/user/browse

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,dok
      LOGICAL MODIFY,MODLEN,MODBND,UPDGEO,browse,modcfg,close,updoth

C TOTHER is a temporary array for manipulating SSOTHER.
C SO is a tempporary array to hold current surface SSOTHER.
      DIMENSION VERT(35),TOTHER(MS,3),SO(3)
      CHARACTER VERT*46,KEY*1,CXSTR*78,H*72,outs*124,DESCRC*25
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      character SO*24,TOTHER*24
      character CXITM*43
      logical newgeo  ! to use for testing if new/old geometry file.


C Set for redraw if image on first entry (if refresh set to after
C each edit). Set flag for updating geometry file (for changed
C surface attribute.
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.
      modcfg=.false.
      newgeo=.false.  ! assume older format geometry.

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
        ICN=IZSTOCN(IC1(IFOC),IE1(IFOC))
        WRITE(SO(1),'(A)')SSOTHER(icn,1)
        WRITE(SO(2),'(A)')SSOTHER(icn,2)
        WRITE(SO(3),'(A)')SSOTHER(icn,3)

C Display the reference zone (if type 3 then display both).
C Remember the topology attributes for reference zone.
        IUO=IFIL+1
        call georead(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),0,iuout,IER)
        do 322 IJ=1,NSUR
          ICN=IZSTOCN(IC1(IFOC),IJ)
          write(TOTHER(IJ,1),'(a)') SSOTHER(ICN,1)
          write(TOTHER(IJ,2),'(a)') SSOTHER(ICN,2)
          write(TOTHER(IJ,3),'(a)') SSOTHER(ICN,3)
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
          CALL ADJVIEW(IER)
        else
          LINSTY(IFOC)=2
          nzg=1
          nznog(1)=IC1(IFOC)
          izgfoc=IC1(IFOC)
          CALL ADJVIEW(IER)
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
        H(1) ='The boundary conditions at the other face of this sur-'
        H(2) ='face is specified via the selection of one of these'
        H(3) ='choices.  The default is OUTSIDE.'
        H(4) =' '
        H(5) =' Exterior - the other face of the surface is exposed to'
        H(6) ='  ambient air temperature, wind and solar radiation.'
        H(4) =' '
        H(5) =' Dynamic/similar - the other face of the surface is'
        H(6) ='  exposed to the same conditions as the inside face'
        H(7) ='  (except that when direct sunlight is falling on the'
        H(8) ='  inside face it cannot be falling on the other face'
        H(9) ='  only the diffuse component is used on the other face.'
        H(10)='  where both sides get the same radiation.'
        H(11)='  A dynamic/similar connection can include an offset'
        H(12)='  temperature (C) and radiation (W/m^2).'
        H(13)=' '
        H(14)=' Static - requires that a constant temperature and'
        H(15)='  radiation be supplied. You might use this to represent'
        H(16)='  a cold storage room.'
        H(17)=' '
        H(18)=' Surface in another zone - the other face of the surface'
        H(19)='  gets its boundary conditions from the last known state'
        H(20)='  of the other surface. There is a thermophysical con-'
        H(21)='  nection established between the two zones.'
        H(22)=' '
        H(23)=' Ground (standard profile) - the other face of the'
        H(24)='  surface is in direct contact with one of the standard'
        H(25)='  monthly temperature profiles embedded in ESP-r.'
        H(26)=' '
        H(27)=' Ground (user defined profile) - the other face of the'
        H(28)='  surface is in direct contact with a monthly profile'
        H(29)='  previously created by the user (in the model context)'
        H(30)=' '
        H(31)=' Ground (3D conduction) - the other face connects to a'
        H(32)='  3D ground representation. This is not often used.'
        H(33)=' '
        H(34)=' Adiabatic boundary - typically used for back-to-back'
        H(35)='  surfaces in a zone representing thermal mass. There'
        H(36)='  is no heat flux passed beyond the other-face. '
        H(37)=' '
        H(38)=' BASESIMP is used to represent one of several basement'
        H(39)='  configurations and precalculated patterns of heat'
        H(40)='  transfer. For this to be useful there must be a'
        H(41)='  basesimp file created for the zone giving a number'
        H(42)='  of dimensions and computational parameters.'
        H(43)=' '
        H(44)=' CEN 13791 partition which is like the SIMILAR'
        H(45)='  but for CEN 13791 test. The offset temperauture'
        H(46)='  and radiation (W/m^2) in SIMILAR is not available'
        H(47)='  for the CEN 13791 partition type. For details see'
        H(48)='  section 4.4.2 CEN 13791 standard. '
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
     &  'k continue ',' ',iart,idno,48)
        if(iart.eq.0)then
          continue
        elseif(iart.eq.1)then
          modcfg=.true.
          ICT(IFOC)=0
          IE2(IFOC)=0
          IC2(IFOC)=0
          if(SO(1)(1:8).ne.'EXTERIOR')then
            TOTHER(IE1(IFOC),1)='EXTERIOR'
            TOTHER(IE1(IFOC),2)='0'
            TOTHER(IE1(IFOC),3)='0'
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
          H(8)=' '
          H(9)='Use care when setting a high offset temperature.'
          VALT=0.0
          VALW=0.0
          CALL EASKR(VALT,' Offset temperature? [default=0.0]',
     &      '(see help) ',-99.,'F',700.,'F',0.0,'offset temp',IER,9)
          CALL EASKR(VALW,' Offset radiation source (W/m^2)?',
     &      '(see help) ',0.0,'W',99999.,'W',0.0,'offset rad',IER,9)
          IC2(IFOC)=INT(VALT)
          IE2(IFOC)=INT(VALW)
          if(SO(1)(1:7).ne.'SIMILAR')then
            TOTHER(IE1(IFOC),1)='SIMILAR'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.3)then 
          modcfg=.true.
          ICT(IFOC)=2
          H(1)='A type 2 connection requires that a constant '
          H(2)='temperature and radiation (Watts) be entered.'
          H(3)=' '
          H(4)='Use care when setting a high adjacent temperature.'
          CALL EASKR(VAL,' ',' Constant temperature ? ',
     &      -99.,'F',700.,'F',20.,'adjacent temperature',IER,4)
          IC2(IFOC)=INT(VAL)

          CALL EASKR(VAL,' ',
     &    ' Constant radiation source (Watts) ? ',
     &      0.0,'W',99999.,'W',0.0,'adjacent radiation source',IER,2)
          IE2(IFOC)=INT(VAL)
          if(SO(1)(1:8).ne.'CONSTANT')then
            TOTHER(IE1(IFOC),1)='CONSTANT'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
          CALL ADJVIEW(IER)
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
            TOTHER(IE1(IFOC),1)='ANOTHER'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
            UPDGEO=.TRUE.

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
            if(SO(1)(1:6).ne.'GROUND')then
              TOTHER(IE1(IFOC),1)='GROUND'
              write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
              write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
            if(SO(1)(1:6).ne.'GROUND')then
              TOTHER(IE1(IFOC),1)='GROUND'
              write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
              write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
          if(SO(1)(1:6).ne.'GROUND')then
            TOTHER(IE1(IFOC),1)='GROUND'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
            UPDGEO=.TRUE.
          endif
        elseif(iart.eq.8)then 
          modcfg=.true.
          IE2(IFOC)=0
          IC2(IFOC)=0
          if(SO(1)(1:9).ne.'ADIABATIC')then
            TOTHER(IE1(IFOC),1)='ADIABATIC'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
          if(SO(1)(1:8).ne.'BASESIMP')then
            TOTHER(IE1(IFOC),1)='BASESIMP'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
          if(SO(1)(1:7).ne.'IDENT_CEN')then
            TOTHER(IE1(IFOC),1)='IDENT_CEN'
            write(tother(IE1(IFOC),2),'(i2.2)') IC2(ifoc)
            write(tother(IE1(IFOC),3),'(i2.2)') IE2(ifoc)
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
          call georead(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),0,iuout,IER)
          do 323 IJ=1,NSUR
            SOTHER(IJ,1)=TOTHER(IJ,1)
            SOTHER(IJ,2)=TOTHER(IJ,2)
            SOTHER(IJ,2)=TOTHER(IJ,3)
            ICN=IZSTOCN(IC1(IFOC),ij)
            SSOTHER(ICN,1)=TOTHER(IJ,1)
            SSOTHER(ICN,2)=TOTHER(IJ,2)
            SSOTHER(ICN,3)=TOTHER(IJ,3)
  323     continue
          call eclose(gversion(IC1(IFOC)),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(IC1(IFOC)) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),iuout,3,IER)
          else
            call emkgeo(IUO,LGEOM(IC1(IFOC)),IC1(IFOC),3,IER)
          endif
          IF(IER.NE.0)CALL USRMSG(' ',
     &           ' Problem updating surface attribute...','W')
        endif

C If user requested other side to be updated do this as well. Read
C in the zone pointed to by IC2, update SOTHER (of index IE2 in the
C other zone) and write out that zones geometry.
        if(updoth)then
          call usrmsg(' Updating other side...',' ','-')
          call georead(IUO,LGEOM(IC2(IFOC)),IC2(IFOC),0,iuout,IER)

          ICN=IZSTOCN(IC2(IFOC),IE2(IFOC))
          SSOTHER(ICN,1)='ANOTHER'
          write(SSOTHER(ICN,2),'(i2.2)') IC1(IFOC)
          write(SSOTHER(ICN,3),'(i2.2)') IE1(IFOC)

          call eclose(gversion(IC2(IFOC)),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(IC2(IFOC)) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IUO,LGEOM(IC2(IFOC)),IC2(IFOC),iuout,3,IER)
          else
            call emkgeo(IUO,LGEOM(IC2(IFOC)),IC2(IFOC),3,IER)
          endif
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
C information. This is shorter than the string returned by CONXINFO. 
      SUBROUTINE CONXMENU(ICON,CXITM)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)
      CHARACTER CXITM*43,SST1*12,ZST1*12,SST2*12,ZST2*12

      integer ICM  ! connection for current surface.

      CXITM=' '
      SST1=SSNAME(ICON)
      lsn1=MIN0(lnblnk(SST1),8)
      ZST1=zname(IC1(ICON))
      lzn1=MIN0(lnblnk(ZST1),8)
      IF(ICT(ICON).EQ.0)THEN
        if((lsn1+lzn1+21).lt.43)then
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< External'
        else
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< External'
        endif
      ELSEIF(ICT(ICON).EQ.1)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< Identical'
        else
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< Identical'
        endif
      ELSEIF(ICT(ICON).EQ.2)THEN
        if((lsn1+lzn1+28).lt.43)then
          WRITE(CXITM,'(I4,1x,4a,I3,a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< Constant @',IC2(ICON),'dC'
        else
          WRITE(CXITM,'(I4,1x,4a,I3,a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< Constant @',IC2(ICON),'dC'
        endif
      ELSEIF(ICT(ICON).EQ.3)THEN
        ICM=IZSTOCN(IC2(ICON),IE2(ICON))
        SST2=SSNAME(ICM)
        lsn2=MIN0(lnblnk(SST2),8)
        ZST2=zname(IC2(ICON))
        lzn2=MIN0(lnblnk(ZST2),8)
        if((lsn1+lzn1+lsn2+lzn2+17).lt.43)then
          WRITE(CXITM,'(I4,1x,7a)')ICON,SST1(:lsn1),' in ',
     &          ZST1(1:lzn1),' >|< ',SST2(1:lsn2),' in ',ZST2(1:lzn2)
        elseif((lsn1+lzn1+lsn2+lzn2+15).lt.43)then
          WRITE(CXITM,'(I4,1x,7a)')ICON,SST1(1:lsn1),' : ',
     &          ZST1(1:lzn1),' >|< ',SST2(1:lsn2),' : ',ZST2(1:lzn2)
        else
          WRITE(CXITM,'(I4,1x,7a)')ICON,SST1(1:lsn1),':',
     &          ZST1(1:lzn1),' >|< ',SST2(1:lsn2),':',ZST2(1:lzn2)
        endif
      ELSEIF(ICT(ICON).EQ.4)THEN
        IF(IC2(ICON).GT.0)THEN
          if((lsn1+lzn1+19).lt.43)then
            WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &        ZST1(1:lzn1),' >|< ground'
          else
            WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &        ZST1(1:lzn1),' >|< ground'
          endif
        ELSE
          if((lsn1+lzn1+26).lt.43)then
            WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &        ZST1(1:lzn1),' >|< user def grnd'
          else
            WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &        ZST1(1:lzn1),' >|< user def grnd'
          endif
        ENDIF
      ELSEIF(ICT(ICON).EQ.5)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< adiabatic'
        else
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< adiabatic'
        endif
      ELSEIF(ICT(ICON).EQ.6)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< BASESIMP'
        else
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< BASESIMP'
        endif
      ELSEIF(ICT(ICON).EQ.7)THEN
        if((lsn1+lzn1+22).lt.43)then
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),' in ',
     &      ZST1(1:lzn1),' >|< IDENT_CEN'
        else
          WRITE(CXITM,'(I4,1x,4a)') ICON,SST1(1:lsn1),':',
     &      ZST1(1:lzn1),' >|< IDENT_CEN'
        endif
      ENDIF

      RETURN
      END

C ************ SFCRNR
C SFCRNR finds the total number of corners in a surface and
C stores their coordinates. A corner exists if two joined
C edges have different angles 'ALPHA' and 'THETA' where:
C         ALPHA is measured from the X axis on the XY plane.
C         THETA is measured from the XY plane.
      SUBROUTINE SFCRNR(DACC,IZN, JSURF, NAPICS, COORD)
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C24/IZSTOCN(MCOM,MS)
      DIMENSION COORD(MV,3)
      character outs*124

C ANACC is the tolerance in degrees before a corner is assumed.
      icc=IZSTOCN(IZN,JSURF)
      if(icc.eq.0)then
        write(outs,'(a,i3,a,i3,a)') 
     &    'When checking corners found surf ',jsurf,
     &    ' in zone ',izn,' was not in the master connections list.'
        call edisp(iuout,outs)
        return
      endif

      ANACC = DACC

C NAPICS counts number of corners located for surface.
      NAPICS=0

C NSV holds number of vertices for this surface.
      NSV=NZNVER(icc)

C Loop over each vertex to locate an apex.
      DO 10 I=1, NSV
         IND=I
         IVERT1= NZJVN(icc,IND)
         X1=VCOORD(IZN,IVERT1,1)
         Y1=VCOORD(IZN,IVERT1,2)
         Z1=VCOORD(IZN,IVERT1,3)

         IND=IND+1
         IF (IND.GT.NSV) IND=1
         IVERT2= NZJVN(icc,IND)
         X2=VCOORD(IZN,IVERT2,1)
         Y2=VCOORD(IZN,IVERT2,2)
         Z2=VCOORD(IZN,IVERT2,3)

         IND=IND+1
         IF (IND.GT.NSV) IND=1
         IVERT3= NZJVN(icc,IND)
         X3=VCOORD(IZN,IVERT3,1)
         Y3=VCOORD(IZN,IVERT3,2)
         Z3=VCOORD(IZN,IVERT3,3)

C Find angles of edge 1.
         CALL LINDIR(X1,Y1,Z1,X2,Y2,Z2,ALPHA1,THETA1)

C Find angles of edge 2.
         CALL LINDIR(X2,Y2,Z2,X3,Y3,Z3,ALPHA2,THETA2)

         ALDIFF=ABS(ALPHA2-ALPHA1)
         THDIFF=ABS(THETA2-THETA1)

C If angles difference greater than ANACC then apex located.
         IF (ALDIFF.GT.ANACC.OR.THDIFF.GT.ANACC) THEN
            NAPICS=NAPICS+1
            COORD(NAPICS,1)=X2
            COORD(NAPICS,2)=Y2
            COORD(NAPICS,3)=Z2
         ENDIF
   10 CONTINUE

      RETURN
      END


C ************ LINDIR
C LINDIR finds the angles associated with a line in
C space. The angles returned are the alpha and theta angles in degrees.
C Alpha lies in the XY plane and theta measured from the XY plane.
      subroutine lindir(X1,Y1,Z1,X2,Y2,Z2,ALPHA,THETA)

      logical close
      PI = 4.0 * ATAN(1.0)
      DIST=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
      THETA=ASIN((Z2-Z1)/DIST)
      call eclose(X1,X2,0.001,close)
      IF (close) THEN
         ALPHA=PI/2.
      ELSE
         ALPHA=ATAN((Y2-Y1)/(X2-X1))
      ENDIF
      THETA = THETA*180./PI
      ALPHA = ALPHA*180./PI

      RETURN
      END

C Find bounding box and center related to a surface. Dx is cushion
C to place around the box (m).
      subroutine surbox(iz,is,dx,cx,cy,cz,sxmx,sxmn,symx,symn,szmx,szmn)
#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      SXMX=-1.E+7
      SYMX=-1.E+7
      SXMN=1.E+7
      SYMN=1.E+7
      SZMX=-1.E+7
      SZMN=1.E+7
      N = NVER(IS)
      DO 150 J = 1,N
        SXMN=AMIN1(SXMN,X(JVN(IS,J)))
        SYMN=AMIN1(SYMN,Y(JVN(IS,J)))
        SZMN=AMIN1(SZMN,Z(JVN(IS,J)))
        SXMX=AMAX1(SXMX,X(JVN(IS,J)))
        SYMX=AMAX1(SYMX,Y(JVN(IS,J)))
        SZMX=AMAX1(SZMX,Z(JVN(IS,J)))
  150 CONTINUE
      SXMX=SXMX+dx
      SYMX=SYMX+dx
      SXMN=SXMN-dx
      SYMN=SYMN-dx
      SZMX=SZMX+dx
      SZMN=SZMN-dx
      cx=SXMN+((SXMX-SXMN)/2.0)
      cy=SYMN+((SYMX-SYMN)/2.0)
      cz=SZMN+((SZMX-SZMN)/2.0)
      return
      end
