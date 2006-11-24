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

C prescoef.F contains subroutines related to management of
C wind pressure coefficient data:

C  CPCDAT: reads data required by CPCALC for pressure coefficients.
C  EDCPFAC:  Edit coordinates that define at which facade location CPCALC
C            should calculate pressure coefficients.
C  CPCACAL:  Use CPCALC parameters, create an input file 
C            and calculate the pressure coeffcients
C  EMKAPCD: Creates an ascii pressure coef database from commons.
C  EDPCDB: Display pressure coeff db and allow changes.
C  EDPCST: Supports editing on one pressure coef set.
C  ERPRCDB: Read an ASCII pressure coefficient database.

C ************* CPCDAT
C The subroutine CPCDAT reads in general building data that is 
C required by CPCALC to calculate pressure coefficients around
C a building.
      subroutine cpcdat
#include "building.h"
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/pophelp/h(60)
      dimension ITEMS(15)
      character items*36,h*72,hold*32

      logical moddb,close

C If values are zero then set defaults.
      call eclose(ble,0.0,0.001,CLOSE)
      if(close)ble=XMX-XMN
      call eclose(bwi,0.0,0.001,CLOSE)
      if(close)bwi=YMX-YMN
      call eclose(bhi,0.0,0.001,CLOSE)
      if(close)bhi=ZMX-ZMN
      call eclose(blox,0.0,0.001,CLOSE)
      if(close)blox=XMN
      call eclose(bloy,0.0,0.001,CLOSE)
      if(close)bloy=YMN
      call eclose(bloz,0.0,0.001,CLOSE)
      if(close)bloz=ZMN

      moddb=.false.
C Display menue. 
  31  INO=-4
      
      IER=0
      items(1)                 =  '  Data required by CPCALC  -'
      WRITE(ITEMS(2),'(A,F5.1)')  'a  building width       :',ble
      WRITE(ITEMS(3),'(A,F5.1)')  'b  building depth       :',bwi
      WRITE(ITEMS(4),'(A,F5.1)')  'c  building hight       :',bhi
      WRITE(ITEMS(5),'(A,3F5.1)') 'd  origin :',blox,bloy,bloz
      WRITE(ITEMS(6),'(A,3F5.1)') 'e  orientation          :',orient
      if(irt.eq.0)then
        WRITE(ITEMS(7),'(A)')     'f  roof type            : flat'
      elseif(irt.eq.1)then
        WRITE(ITEMS(7),'(A)')     'f  roof type            : sngl slop'
      elseif(irt.eq.2)then
        WRITE(ITEMS(7),'(A)')     'f  roof type            : dbl slop '
      endif
      WRITE(ITEMS(8),'(A,F5.1)')  'g  roof tilt angle      :',ra
      WRITE(ITEMS(9),'(A,F5.1)')  'h  sur. buildings hight :',sbh
      WRITE(ITEMS(10),'(A,F5.1)') 'i  plan area density    :',pad
      WRITE(ITEMS(11),'(A,F5.2)') 'j  wind vel. prof. expo :',wvpe
      ITEMS(12)=                  '  _________________________        '
      ITEMS(13)=                  '? help                             '
      ITEMS(14)=                  '- exit this menu                   '
      nitms=14

C Help text for this menu.
  47  h(1)='The data is needed by CPCALC to calculate the pressure'
      h(2)='distribution around a building.  '
      h(3)=' '
      h(4)='Building width,depth and hight are the OVERALL '
      h(5)='dimensions if the building. Roof type can be a flat'
      h(6)='roof (type 0), a single sloped roof (type 1) or'
      h(7)='a double sloped roof (type 2).'
      h(8)='The surrounding buildings hight and plan area density '
      h(9)='are related to the average hight and the layout'
      h(10)='pattern of the buildings surrounding the building' 
      h(11)='under examination.'
      h(12)='The wind velocity profile defines the terrain'
      h(13)='roughness.'
      h(14)=' '
      h(15)='Detailed information about the points on the facade '
      h(16)='where pressure coefficients are calculated should be '
      h(17)='defined in the pressure coefficients database of the'
      h(18)='database maintenence facility. '

      CALL EMENU('CPCALC data',ITEMS,nitms,INO)
      if(moddb)then

C << Put redraw of the outline here >>
      endif
      if(INO.EQ.nitms)then

C Update configuration file before exiting.
        if(moddb)CALL EMKCFG('-',IER)
        RETURN
      elseif(INO.EQ.nitms-1)then

C Produce help text for the menu.
        CALL PHELPD('prim convers',18,'-',0,0,IER)

      elseif(ino.eq.2)then

C Building lenght.
        def=ble
        h(1)='The width (with no rotation) is along the x-axis.'
        h(2)='Note that for a tilted roof this is the facade'
        h(3)='with the roof ridge!'
        CALL EASKR(ble,' ','Overall building width:',
     &       0.0,'F',0.0,'-',def,'CPCALC-ble',IER,3)
        moddb=.true.
        icpcon=1
      elseif(ino.eq.3)then

C Building width.
        def=bwi
        h(1)='The depth (with no rotation) is along the y-axis.'
        h(2)='Note that for a tilted roof this is the facade'
        h(3)='with the gable!'
        CALL EASKR(bwi,' ','Overall building depth:',
     &       0.0,'F',0.0,'-',def,'CPCALC-bwi',IER,1)
        moddb=.true.
        icpcon=1
      elseif(ino.eq.4)then

C Building hight.
        def=bhi
        h(1)='Give the OVERALL hight of the building.'
        h(2)='Note that for a building with a tilted roof the hight '
        h(3)='of the building is defined from the bottom of the'
        h(4)='building to the bottom of the roof (eaves).'
        CALL EASKR(bhi,' ','Overall building hight:',
     &       0.0,'F',0.0,'-',def,'CPCALC-bhi',IER,4)
        moddb=.true.
        icpcon=1
      elseif(ino.eq.5)then

C Origin
        h(1)='This is the origin of the CPCALC domain that is used by'
        h(2)='CPCALC to calculate the pressure coefficients.'
        h(3)='It will define its south-westerly corner.'
        h(4)='With the definition of the orientation the box'
        h(5)='gets positioned at the exact place.'
        h(6)='(this is in the same coordinate system as the zones)'
        write(hold,'(3F7.2)') blox,bloy,bloz
        CALL EASKS(HOLD,'Origin of CPCALC domain (X Y Z metres):',
     &      ' ',32,' 0. 0. 0.','origin XYZ',IER,6)
        K=0
        CALL EGETWR(HOLD,K,blox,-999.,999.,'W','X org',IER)
        CALL EGETWR(HOLD,K,bloy,-999.,999.,'W','Y org',IER)
        CALL EGETWR(HOLD,K,bloz,-999.,999.,'W','Z org',IER)
        moddb=.true.
      elseif(ino.eq.6)then

C  Orientation
        def=0.
        h(1)= 'The orientation of the CPCALC domain is given in  '
        h(2)= 'a clockwise direction (i.e. 0 rotation is length  '
        h(3)= 'along X axis). The rotation point is x (see below).'
        h(4)= 'Rotation can be in between 0 and 180 degree.      '
        h(5)= '                                                  '
        h(6)= '   *****************                              '
        h(7)= '   *               *      NORTH                   '
        h(8)= '   *               *        ^                     '
        h(9)= '   *  <-length->   *        |                     '
        h(10)='   x****************        |                     '

        CALL EASKR(orient,' ','Orientation of the building:',
     &      0.0,'F',180.0,'F',def,'CPCALC-orient',IER,10)
        moddb=.true.
        icpcon=1
      elseif(ino.eq.7)then

C Roof type
        h(1)='Choose one roof type. '
        h(2)='The double slope roof is also known as a gable roof.'
        h(3)='Note that for a building with a tilted roof the facade'
        h(4)='along the length dimension (x-axis if no rotation) is '
        h(5)='the one with the roof ridge!'
        h(6)= '        ridge                                     '
        h(7)= '   **.............**                              '
        h(8)= '   *      or       *      NORTH                   '
        h(9)= '   * ...ridge..... *        ^                     '
        h(10)='   *  <-length->   *        |                     '
        h(11)='   x****************        |                     '
        CALL EASKABC('Choose one roof type: ',' ','flat roof',
     &  'single slope roof','double slope roof',IWHICH,11)
        if (IWHICH.EQ.1)then
          irt=0
          ra=0.0
        endif
        IF (IWHICH.EQ.2) irt=1
        IF (IWHICH.EQ.3) irt=2
        IF (IWHICH.EQ.2.or.IWHICH.eq.3)then
          def=10.
          h(1)='The roof tilt angle is measured from the horizontal'
          h(2)='plane containing the reference flat roof.' 
          h(3)='The roof tilt angle has to be in between 0 - 50 degree'
          CALL EASKR(ra,' ','Roof tilt angle:',
     &        0.0,'F',50.0,'F',def,'CPCALC-ra',IER,3)
        endif
        moddb=.true.
        icpcon=1
      elseif(ino.eq.8)then

C  Roof tilt angle.
        IF(irt.NE.0) THEN
          def=10.
          h(1)='The roof tilt angle is measured from the horizontal'
          h(2)='plane containing the reference flat roof.' 
          h(3)='The roof tilt angle has to be in between 0 - 50 degree'
          CALL EASKR(ra,' ','Roof tilt angle:',
     &        0.0,'F',50.0,'F',def,'CPCALC-ra',IER,3)
          moddb=.true.
          icpcon=1
        ENDIF
      elseif(ino.eq.9)then

C Surrounding buildings height.
        def=5.
        h(1)='The surrounding buildings hight is related to the '
        h(2)='average hight of the buildings surrounding the model'
        h(3)='zones under examination.'
        CALL EASKR(sbh,' ','Surrounding buildings hight:',
     &       0.0,'F',0.0,'-',def,'CPCALC-sbh',IER,3)
        moddb=.true.
        icpcon=1

C << Change formula R and give hight for examples to calculate R >>
      elseif(ino.eq.10)then

C Plan area density.
        def=8.0
        h(1)='The plan area density (PAD) is related to the layout'
        h(2)='pattern of the buildings surrounding the building under'
        h(3)='examination:'
        h(4)=' '
        h(5)='PAD=(Building Area/Total Area)*100'
        h(6)='[where total area is the overall site area (m^2) and '
        h(7)=' building area is the ground contact area of buildings'
        h(8)=' (m^2) within the overall site].'
        h(9)=' '
        h(10)='The extension of the radius to be considered can be'
        h(11)='calculated with the radius formula R:'
        h(12)=' '
        h(13)='R=(7.67-PAD)/2.7*building height'
        h(14)=' '
        h(15)='Wind tunnel test data shows that the radius of the'
        h(16)='built area surrounding the building, and affecting the'
        h(17)='wind pressure distribution on its envelop, is inversely'
        h(18)='proportional to the plan area density. This is why a'
        h(19)='greater PAD lead to a greater radius to be considered:'
        h(20)='A PAD of 12 results in a considered radius of 240'
        h(21)='and a PAD of 40 results in a considered radius of 135.' 
        h(22)=' '
        h(23)='If the area surrounding the buildings does not have an'
        h(24)='homogeneous density - as often happens - homogeneous'
        h(25)='sectors should be defined according to the average'
        h(26)='density of their building blocks. The PAD value to '
        h(27)='insert in the program is the average value of all' 
        h(28)='sectors.'
        h(29)=' '
        h(30)='This help message was extracted from the CPCALC user'
        h(31)='menual.'
        h(32)=' '
        CALL EASKR(pad,'Plan area density: ','(see help)',
     &       0.0,'F',50.0,'F',def,'CPCALC-pad',IER,32)
        moddb=.true.
        icpcon=1
      elseif(ino.eq.11)then

C Wind velocity profile.
        def=0.28
        h(1)='The wind velocity profile defines the terrain roughness.'
        h(2)=' ' 
        h(3)='The following list shows reference values taken from'
        h(4)='the CPCALC user menue:'
        h(5)=' ' 
        h(6)='Level surface, surface of water basin, grass land:' 
        h(7)='-> 0.10'
        h(8)='Flat open country few, very small and scattered'
        h(9)='obstructions:' 
        h(10)='-> 0.14'
        h(11)='Rolling or level surfaces broken by numerious'
        h(12)='obstructions such as trees or small houses:'
        h(13)='-> 0.22'
        h(14)='Heterogeneous surface with obstacles larger than'
        h(15)='one story:'
        h(16)='-> 0.28'
        h(17)='Low density suburban areas:'
        h(18)='-> 0.34'
        h(19)='Medium-high density urban areas:'
        h(20)='-> 0.40'
        h(21)='Very high density inner city areas:'
        h(22)='-> 0.45'
        h(23)=' ' 
        CALL EASKR(wvpe,' ',' Wind velocity profile:',
     &       0.0,'F',1.0,'F',def,'CPCALC-wvpe',IER,23)
        moddb=.true.
        icpcon=1
      else
        INO=-4
        GOTO 47
      endif
      INO=-4
      GOTO 31
      end

C ************* EDCPFAC
C Edit coordinates that define at which facade location CPCALC
C should calculate pressure coefficients.
      SUBROUTINE EDCPFAC(ITRC,IER)
#include "epara.h"
#include "building.h"
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)
      common/pophelp/h(60)
      common/FILEP/IFIL
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      common/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      common/CPCALC2/FICOCP

      LOGICAL MODIFY,MODLEN,MODBND,OK,dok
      logical CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      DIMENSION VERT(35),COMPS(MPCP),IVAL(MPCP)
      CHARACTER VERT*75,KEY*1,H*72,DESCRC*25
      CHARACTER SOTHER*15,hold2*40,naco*40,COMPS*72
      CHARACTER*12 SNAME, SMLCN
      CHARACTER*4 SVFC,SOTF,type
      character*72 LTMP,LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,FICOCP
      CHARACTER OUTSTR*124,RSTR*124
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16,t28*28

      DIMENSION COG1(3),COG2(3),COG3(3)
      DIMENSION XX(MV),YY(MV),ZZ(MV)
      DIMENSION VP(3),EP(3),EQN(4),TMAT(4,4),RMAT(4,4)

C Initialise facade coordinates menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      IUM=IFIL+7
 92   MHEAD=1
      MCTL=7
      ILEN=NUCO
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   3  IER=0
      ILEN=NUCO
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,NUCO
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(VERT(M),14)naco(L),xcoab(L),ycoab(L),zcoab(L)
   14     FORMAT(2X,A40,1X,3F9.3)
        ENDIF
   10 CONTINUE

      WRITE(VERT(1),'(a,a)')'|Description                          ',
     &  '       X co-ord|Y co-ord|Z co-ord|'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
      VERT(M+1)  ='  ________________________________________________'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      VERT(M+2)  ='+ add/generate coordinate           '
      VERT(M+3)  ='^ delete coordinate                 '
      VERT(M+4)  ='< open a temporary coordinates file '
      VERT(M+5)  ='> save coordinates to temporaty file'
      VERT(M+6)  ='? help                              '
      VERT(M+7)  ='- exit                              '
      IVERT=-1

C Help text for the menu.
      h(1)='Here you can define the coordinates for which'
      h(2)='CPCALC is going to calculate the pressure'
      h(3)='coefficients.'
      h(4)='"Add coordinates" allows a manual input of the '
      h(5)='coordinates, "generate coordinates" will search the '
      h(6)='model for windows and suggest a position.'
      h(7)='Delete coordinates allows to remove allready defined'
      h(8)='coordinates.'

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU(' Current coordinates used by CPCALC',VERT,
     &  MVERT,IVERT)
      else
        CALL EMENU(' CPCALC coordinates',VERT,MVERT,IVERT)
      endif
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Leave subroutine.
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu. 
        CALL PHELPD('input cpcalc coordinates',8,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Save coordinates.
        LTMP=FICOCP
 878    CALL EASKS(LTMP,' Facade coordinates file ? ',
     &    ' ',72,' ','facade coordinates file',IER,5)
        call usrmsg(' ',' ','-')
        IF(LTMP(1:2).NE.'  ')FICOCP=LTMP
        CALL EFOPSEQ(IUM,FICOCP,4,IER)
        IF(IER.NE.0)goto 878
        WRITE(IUM,'(I3,A)')NUCO,'   # no of facade coordinate sets'
        Write(IUM,'(A,A)')'#   x-coo   y-coo   z-coo   azim     elev',
     &      '  type  Name of coordinate set'
        DO 234 kl=1,NUCO
          Write(IUM,679)xcoab(kl),ycoab(kl),zcoab(kl),
     &                 azim(kl),elev(kl),type(kl),naco(kl)
 679      Format(3F8.2,F9.1,F8.1,2X,A4,2X,A40)
 234    CONTINUE
        CALL ERPFREE(IUM,ISTAT)
        CALL USRMSG('Since the facade coordinate file is only for ',
     &    'supporting purpose you must keep track of it yourself!','W')
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Open coordinate file.
        CALL EASKS(LTMP,' Facade coordinates file ? ',
     &    ' ',72,' ','read facade coordinates file',IER,5)
        call usrmsg(' ',' ','-')
        IF(LTMP(1:2).NE.'  ')FICOCP=LTMP
        CALL ERPFREE(IUM,ISTAT)
        CALL EFOPSEQ(IUM,FICOCP,1,IER)
        IF(IER.NE.0)goto 92

C Read the number of facade cordinates in file (NCOO),
        CALL STRIPC(IUM,OUTSTR,0,ND,1,'no. facade coord',IER)
        IF(IER.NE.0)goto 92
        K=0
        CALL EGETWI(OUTSTR,K,NCOO,0,MPRD,'F','no. coords',IER)
        IF(IER.NE.0)goto 92

C Read in coordinates
        DO 98 naqs=1,NCOO
          K=0
          CALL STRIPC(IUM,OUTSTR,0,ND,1,'facade coordd',IER)
          IF(IER.NE.0)goto 92
          DO 988 naqd=1,3
            CALL EGETWR(OUTSTR,K,FC,0.0,0.0,'-',
     &        'facade coord',IER) 
            IF(IER.NE.0)goto 92
            IF (naqd.eq.1) xcoab(naqs)=FC
            IF (naqd.eq.2) ycoab(naqs)=FC
            IF (naqd.eq.3) zcoab(naqs)=FC
 988      continue
          CALL EGETWR(OUTSTR,K,val,0.0,360.0,'F','azimuth',IER)
          IF(IER.NE.0)goto 92
          azim(naqs)=val
          CALL EGETWR(OUTSTR,K,val,-90.0,90.0,'F','elevation',IER)
          IF(IER.NE.0)goto 92
          elev(naqs)=val
          CALL EGETRM(OUTSTR,K,RSTR,'W','cpcalc co. desc.',IER)
          IF(IER.NE.0)goto 92
          type(naqs)=RSTR(1:4)
          naco(naqs)=RSTR(7:46)
 98     continue  
        CALL ERPFREE(IUM,ISTAT)

C Update the number of known coordinates.
        NUCO=NCOO
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C Delete coordinates.
        H(1)='You can delete one coordinate set'
        CALL EPMENSV
        DO 66 IWW=1,NUCO
          WRITE(COMPS(IWW),31)naco(IWW),xcoab(IWW),ycoab(IWW),zcoab(IWW)
   31     FORMAT(A40,1X,3F9.3)
  66    CONTINUE
        IX=1
        CALL EPICKS(IX,IVAL,'Delete which one?',' ',
     &  72,NUCO,COMPS,'coordinate sets',IER,1)
        CALL EPMENRC
        IF(IX.EQ.0)RETURN
        IWHICH=IVAL(1)

C Confirm choice
        dok=.false.
        h(1)='Chance to not delete this cooordinate set. '
        h(2)='(in case you picked the wrong set) '
        CALL ASKOK(' ',' Are you sure? ',OK,dok,2)
        IF(.NOT.OK)GOTO 3

C Create new numbering for pressure coefficient set
        DO 891 IVV=IWHICH,NUCO-1
          naco(IVV)=naco(IVV+1)
          xcoab(IVV)=xcoab(IVV+1)
          ycoab(IVV)=ycoab(IVV+1)
          zcoab(IVV)=zcoab(IVV+1)
          azim(IVV)=azim(IVV+1)
          type(IVV)=type(IVV+1)
          elev(IVV)=elev(IVV+1)
 891    CONTINUE

C Reduce number of pressure coefficient sets.
        NUCO=NUCO-1
        ILEN=NUCO
        GOTO 3
      ELSEIF(IVERT.EQ.(MVERT-5))THEN

C Add/generate coordinates. 
        h(1)='You can eiter provide the coordinates by manual input'
        h(2)='or search the model for windows.'
        h(3)='Windows are transparent external surfaces.'
        CALL EASKAB('Options: ',' ','manual input',
     &              'search for windows',iacc,3)
        IF(IACC.EQ.1) THEN

C Manual input of cordinates. 
          CALL EASKGEOF('Select a zone for coords:',CFGOK,IC,'-',IER)
          IUF=IFIL+1
          CALL EGOMIN(IUF,LGEOM(IC),IC,1,0,iuout,IER)
          nzg=1
          nznog(1)=IC
          izgfoc=IC
          MODIFY=.TRUE.
          MODLEN=.TRUE.
          MODBND=.TRUE.
          CALL ESCZONE(IC)
          CALL BNDOBJ(0,IER)
          CALL ERCZONE(IC)
          CALL INLNST(1)
          itsnm=0
          nzg=1
          nznog(1)=IC
          izgfoc=IC
          CALL ADJVIEW(ITRC,IER)
 256      CALL EASKSUR(IC,ISO,'-','with respect to which external ',
     &               'surface do you want to define coordinates? ',IER)
          call ZSID(IC,ISO,ZSDES,ZSDESC,ZSDESS) 
 
C Set all surfaces to standard line width and surface being 
C selected to a thick line.
          ITVNO=0
          ITSNM=0
          MODIFY=.TRUE.
          CALL INLNST(1)
          CALL SURADJ(IC,ISO,IE,TMP,IZC,ISC,ICXX,DESCRC)
          LINSTY(ICXX)=2
          nzg=1
          nznog(1)=IC
          izgfoc=IC
          izgfoc=IC
          CALL ADJVIEW(ITRC,IER)

C Ask for x offset.
          DX=1.0
          H(1)='The X offset is the horizontal distance from the '
          H(2)='lower left corner of the surface (looking from'
          H(3)='the outside) to the considered point.'
          CALL EASKR(DX,' ',' X offset: ? ',
     &       0.1,'-',99.9,'-',1.0,'X offset',IER,3)

C Ask for z offset.
          DZ=1.0
          H(1)='The Z offset is the vertical distance from the '
          H(2)='lower left corner of the surface (looking from'
          H(3)='the outside) to the considered point.'
          CALL EASKR(DZ,' ',' Z offset: ? ',
     &         0.1,'-',99.9,'-',1.0,'Z offset',IER,3)

C Make up XX,YY,ZZ to the transform routine.
          N = NVER(ISO)
          DO 150 J = 1,N
            XX(J) = X(JVN(ISO,J))
            YY(J) = Y(JVN(ISO,J))
            ZZ(J) = Z(JVN(ISO,J))
  150     CONTINUE

C Find transformation matrices that normalise face.
          call PLEQN(XX,YY,ZZ,N,VP,EQN,IERR)
          DO 250 J = 1,3
            EP(J) = VP(J) + EQN(J)
  250     CONTINUE

C Find matrix and reverse matrix via EYEMAT
          CALL  EYEMAT(EP,VP,1.0,TMAT,RMAT)

C Transform all points in surface and find lower left corner.
          XMIN=0.0
          YMIN=0.0
          DO 300 I=1,N
            CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
            IF(X1.LT.XMIN)XMIN=X1
            IF(Y1.LT.YMIN)YMIN=Y1
 300      CONTINUE
          XMIN=XMIN+DX
          YMIN=YMIN+DZ
          CALL ORTTRN(XMIN,YMIN,ZZZ,RMAT,XXDP,YYDP,ZZDP,IERR)
          COG1(1)=XXDP
          COG1(2)=YYDP
          COG1(3)=ZZDP
          CALL VECTRN(COG1,TEMAT,COG2,IER)           
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),ix,iy)
          CALL ecirc(ix,iy,3,1)
          call forceflush()
          write(t28,'(3(a,F6.2))')'x=',XXDP,', y=',YYDP,', z=',ZZDP
          dok=.true.
          h(1)='Chance to not use this cooordinate set.'
          h(2)='(in case the location is incorrect) '
          CALL ASKOK(t28,'Use this coordinate?',OK,dok,2)
          if(OK)then

C If yes than save coordinates ...
 350        write(HOLD2,'(a,a20,a)')'coord of ',ZSDESC,'...'
            nuco=nuco+1
          H(1)='Give a name for the cordinates. This names will then'
          H(2)='later also be used for the definition in the pressure'
          H(3)='coefficient database.'
            CALL EASKS(HOLD2,'Name cpcalc cordinates',
     &        ' ',40,' ','cpcalc co-name',IER,3)
            IF(IER.NE.0)GOTO 350 
            naco(nuco)=HOLD2
            xcoab(nuco)=XXDP
            ycoab(nuco)=YYDP
            zcoab(nuco)=ZZDP
            azim(nuco)=SSPAZI(izstocn(IC,ISO))
            if (azim(nuco).gt.359.9) azim(nuco)=0.0
            type(nuco)=SVFC(ISO)
            elev(nuco)=SSPELV(izstocn(IC,ISO))
          endif

C Ask whether user wants to define another point.
          dok=.true.
          h(1)='Respond with ok if you want another cooordinate set.'
          CALL ASKOK(' ','Define another point for this zone?',OK,dok,1)
          if(OK) goto 256
        ELSEIF(IACC.EQ.2) THEN
          
C Open geometry file for surface property check. 
          IUF=IFIL+1
          do 46 IZU=1,NCOMP
            CALL EGOMIN(IUF,LGEOM(IZU),IZU,1,0,iuout,IER)

C  Check whether zone has external transparent surface(s)
            do 42 IZW=1,NSUR
              if(SOTF(IZW)(1:4).eq.'TRAN'.and.SOTHER(IZW)(1:8).eq.
     &          'EXTERIOR') then

C  If so then display zone and centre point of the 
C  transparent surface of the zone 
                nzg=1
                nznog(1)=IZU
                izgfoc=IZU
                MODIFY=.TRUE.
                CALL ESCZONE(IZU)
                CALL BNDOBJ(0,IER)
                CALL ERCZONE(IZU)
                CALL INLNST(1)
                nzg=1
                nznog(1)=IZU
                izgfoc=IZU
                CALL ADJVIEW(ITRC,IER)

C Find maximum and minimum vertex coordinates of transparent surface
                xdebi=x(JVN(IZW,1))
                xdesm=x(JVN(IZW,1))
                ydebi=y(JVN(IZW,1))
                ydesm=y(JVN(IZW,1))
                zdebi=z(JVN(IZW,1))
                zdesm=z(JVN(IZW,1))
                do 47 ik=1,NVER(IZW)
                  if (x(JVN(IZW,ik)).GT.xdebi) xdebi=x(JVN(IZW,ik))
                  if (x(JVN(IZW,ik)).LT.xdesm) xdesm=x(JVN(IZW,ik))
                  if (y(JVN(IZW,ik)).GT.ydebi) ydebi=y(JVN(IZW,ik))
                  if (y(JVN(IZW,ik)).LT.ydesm) ydesm=y(JVN(IZW,ik))
                  if (z(JVN(IZW,ik)).GT.zdebi) zdebi=z(JVN(IZW,ik))
                  if (z(JVN(IZW,ik)).LT.zdesm) zdesm=z(JVN(IZW,ik))
 47             continue     

C Define the middle point of the window surfaces as the middle point
C between the min-max values. This is under the assumption that
C 96.5% of all the windows are rectangular.
                xpc=xdesm+(xdebi-xdesm)/2
                ypc=ydesm+(ydebi-ydesm)/2
                zpc=zdesm+(zdebi-zdesm)/2
     
C Draw the point.
                COG1(1)=xpc
                COG1(2)=ypc
                COG1(3)=zpc
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),ix,iy)
                CALL ecirc(ix,iy,3,1)
                call forceflush()

C Confirm whether user wants to use the coordinates 
                write(t28,'(3(a,F6.2))')'x=',xpc,', y=',ypc,', z=',zpc
                dok=.true.
                h(1)='Chance to not use this cooordinate set.'
                h(2)='(in case the location is incorrect) '
                CALL ASKOK(t28,'Use this coordinate?',OK,dok,2)
                if(OK)then

C If yes save cordinates. 
 360              continue
                  call ZSID(IZU,IZW,ZSDES,ZSDESC,ZSDESS)
                  write(HOLD2,'(a,a20,a)')'coord of ',ZSDESC,'...'
                  
                  nuco=nuco+1
                  H(1)='Give a name for the cordinates. This names'
                  H(2)='will then later also be used for the defi-'
                  H(3)='nition in the pressure coefficient database.'
                  CALL EASKS(HOLD2,'Name cpcalc cordinates',
     &                 ' ',40,' ','cpcalc co-name',IER,3)
                  IF(IER.NE.0)GOTO 360 
                  naco(nuco)=HOLD2
                  xcoab(nuco)=xpc
                  ycoab(nuco)=ypc
                  zcoab(nuco)=zpc
                  azim(nuco)=SSPAZI(izstocn(IZU,IZW))
                  if (azim(nuco).gt.359.9) azim(nuco)=0.0
                  type(nuco)=SVFC(IZW)
                  elev(nuco)=SSPELV(izstocn(IZU,IZW))
                endif
              endif               
 42         continue
 46       continue
        ENDIF
      ENDIF
      goto 92
      END
               
C ************* CPCACAL
C Take the parameters required by CPCALC, create an input file 
C and calculate the pressure coeffcients

      SUBROUTINE CPCACAL (IER)
#include "net_flow.h"
#include "building.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IFPRE (for window pressure databases)
      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/pophelp/h(60)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)
      common/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB

      CHARACTER naco*40,type*4,H*72,msga*124,msgb*124,tmode*8,t72*72
      CHARACTER LAPROB*72,LTMP*72
      character*72 DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character doit*248

      dimension Xdom(10),Ydom(10),Zdom(10),azdom(4)
      dimension XX(MV),YY(MV),ZZ(MV),VP(3),EQN(4)
      dimension xl(MPCP),zl(MPCP),ior(MPCP),isut(5),inum(5)
      logical ok,dok

C Parameters to see whether for a surface a coordinate set has been 
C defined
      ditol=0.01
      DO 334 ik=1,5
        isut(ik)=0
        inum(ik)=0
 334  CONTINUE

C Determine from the given absolute coordinates the relative coordinates
C required by CPCALC to calculate the pressure coefficients.
C Ask for the maximum tolerance between the the absolut cordinates and
C the block surfaces.
      def=0.1
      h(1)='ESP-r will check whether the coordinates that '
      h(2)='have been defines as locations to calculate the '
      h(3)='pressure coefficients are close to one of the surfaces '    
      h(4)='of the CPCALC domain.'  
      h(5)='Here you can give a tolerance that will be applied'
      h(6)='for this check.'
      CALL EASKR(ditol,'maximum tolerance between the absolut', 
     &  'cordinates and the block surfaces:',0.01,'F',1.0,'F',
     &   def,'CPCALC-ditol',IER,6)

C Ask for the tolerance between the azimuth of the suface from which
C the coordinates have been derived and the azimuth of the CPCALC 
C domain.      
      def=5.0
      aztol=5.0
      h(1)='ESP-r will project the coordinates that have been'
      h(2)='defines as locations to calculate the pressure'
      h(3)='coefficients on the surface of the CPCALC domain' 
      h(4)='with similar orentation and  elevation (projection '
      h(5)='direction normal to the surface of the CPCALC domain).'
      h(6)='As a control meachanism it compares the orentation of'
      h(7)='the zone surface from which these coordinates had been'
      h(8)='derived with the orentation of the surface of the'
      h(9)='CPCALC domain. Here you can define the maximum'
      h(10)='difference between these two values.'
      CALL EASKR(aztol,
     &  'Maximum tolerance between the orientation angle (degrees)', 
     &  'of zone surface and the surface of the CPCALC domain:',
     &  0.01,'F',5.0,'F',def,'CPCALC-aztol',IER,10)

C For a tilted roof: Ask for the maximum tolerance between the tilt of  
C the surface from which the coordinates have been derived and the  
C roof tilt of the CPCALC domain.
      IF (irt.eq.1.or.irt.eq.2) THEN 
        def=5.0
        eltol=5.0
        h(1)='ESP-r will project the coordinates that have been'
        h(2)='defines as locations to calculate the pressure'
        h(3)='coefficients on the surface of the CPCALC domain' 
        h(4)='with similar orentation and  elevation (projection '
        h(5)='direction normal to the surface of the CPCALC domain).'
        h(6)='As a control meachanism it compares the elevation of'
        h(7)='the zone surface from which these coordinates had been'
        h(8)='derived with the elevation of the tilted roof of the'
        h(9)='CPCALC domain. Here you can define the maximum'
        h(10)='difference between these two values.'
        CALL EASKR(eltol,
     &    'Maximum tolerance between the tilt angle of zone ', 
     &    'surface and roof tilt of the CPCALC domain:',
     &    0.01,'F',5.0,'F',def,'CPCALC-eltol',IER,10)
      ENDIF

C Determine the coordinates of the box defining the cpcalc domain.
C The naming convention is the same like for a rectangular 
C thermal zone.
      PI = 4.0 * ATAN(1.0)
      R=PI/180.
      SA=SIN(orient*R)
      CA=COS(orient*R)
      Xdom(1)=blox
      Xdom(2)=Xdom(1)+(ble*CA)
      Xdom(3)=Xdom(2)+(bwi*SA)
      Xdom(4)=Xdom(3)-(ble*CA)
      Ydom(1)=bloy
      Ydom(2)=Ydom(1)-(ble*SA)
      Ydom(3)=Ydom(2)+(bwi*CA)
      Ydom(4)=Ydom(3)+(ble*SA)
      DO 20 I=5,8
        Xdom(I)=Xdom(I-4)
        Ydom(I)=Ydom(I-4)
        Zdom(I-4)=bloz
        Zdom(I)=bloz+bhi
  20  CONTINUE

C Determine orientation of the four vertical surfaces.
      azdom(1)=orient+180.0
      if (azdom(1).GE.359.9) azdom(1)=0.0
      azdom(2)=orient+90.0
      azdom(3)=orient
      azdom(4)=orient+270.0
      if (azdom(4).GE.359.9) azdom(4)=azdom(4)-360.0

C Determine the coordinates for the roof (if tilted).
      if (irt.eq.1.or.irt.eq.2) then
        Xdom(9)=Xdom(5)
        Ydom(9)=Ydom(5)
        Zdom(9)=Zdom(5)
        Xdom(10)=Xdom(6)
        Ydom(10)=Ydom(6)
        Zdom(10)=Zdom(6)
        DO 202 I=9,10
          EL=abs(90.0-ra)         
          AZ=azdom(3)
          IF (irt.eq.1) THEN
            V1=bwi
          ELSE 
            V1=bwi/2
          ENDIF
          PI = 4.0 * ATAN(1.0)
          RAD = PI/180.
          RYAZI = AZ*RAD
          RSALT = EL*RAD
          z2 = V1/tan(RSALT)
          XYDIS=V1
          x2 = XYDIS*SIN(RYAZI)
          y2 = XYDIS*COS(RYAZI)
          Xdom(I)=Xdom(I)+x2
          Ydom(I)=Ydom(I)+y2
          Zdom(I)=Zdom(I)+z2
 202    CONTINUE
      ENDIF
      
C Compare orientation of thermal surface with which point has been
C created with orientation of the surface of CPCALC domain
C Start with vertical surfaces
      DO 157 JC = 1,nuco
      isurf=0
      diff1=azdom(1)-azim(JC)
      diff2=azdom(2)-azim(JC)
      diff3=azdom(3)-azim(JC)
      diff4=azdom(4)-azim(JC)
      if (abs(diff1).le.aztol) then 
        isurf=1
      elseif (abs(diff2).le.aztol) then 
        isurf=2
      elseif (abs(diff3).le.aztol) then 
        isurf=3
      elseif (abs(diff4).le.aztol) then 
        isurf=4
      endif
      IF (type(JC).EQ.'VERT') THEN 
        if(isurf.eq.0) then
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the orientation of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please solve this first !' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C In the next steps the coordinates get projected on domain surfaces with 
C similar orientation:
C First find the equation of plane for the surface
        if (isurf.eq.1) then
          call stuf4xyz(xx,yy,zz,Xdom(1),Ydom(1),Zdom(1),Xdom(2),
     &      Ydom(2),Zdom(2),Xdom(6),Ydom(6),Zdom(6),Xdom(5),Ydom(5),
     &      Zdom(5))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
        elseif (isurf.eq.2) then 
          call stuf4xyz(xx,yy,zz,Xdom(2),Ydom(2),Zdom(2),Xdom(3),
     &      Ydom(3),Zdom(3),Xdom(7),Ydom(7),Zdom(7),Xdom(6),Ydom(6),
     &      Zdom(6))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
        elseif (isurf.eq.3) then 
          call stuf4xyz(xx,yy,zz,Xdom(3),Ydom(3),Zdom(3),Xdom(4),
     &      Ydom(4),Zdom(4),Xdom(8),Ydom(8),Zdom(8),Xdom(7),Ydom(7),
     &      Zdom(7))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
        elseif (isurf.eq.4) then 
          call stuf4xyz(xx,yy,zz,Xdom(4),Ydom(4),Zdom(4),Xdom(1),
     &      Ydom(1),Zdom(1),Xdom(5),Ydom(5),Zdom(5),Xdom(8),Ydom(8),
     &      Zdom(8))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
         endif

C Find a second point that is in normal surface direction
C one meter away from the first point 
        XYDIS=1.0
        AZ=azdom(isurf)
        PI = 4.0 * ATAN(1.0)
        RAD = PI/180.
        RYAZI = AZ*RAD
        x2 = XYDIS*SIN(RYAZI)
        y2 = XYDIS*COS(RYAZI)
        x2=x2+xcoab(JC)
        y2=y2+ycoab(JC)
        z2=zcoab(JC)

C Use these two points to project first point on surface (point 3)
        CALL VECPLN(xcoab(JC),ycoab(JC),zcoab(JC),x2,y2,z2,EQN, 
     &             x3,y3,z3,IERR)

C Check whether point 3 is within distance limit to point 1
        vdist= crowxyz(xcoab(JC),ycoab(JC),zcoab(JC),x3,y3,z3)
        if (vdist.gt.ditol) then 
          Write (msga,'(a,a,I3)') 'Problem encountered with ',
     &          'the distance of coordinate definition number',JC
          Write (msgb,'(a,a,f4.2,a)') 'from the CPCALC domain ',
     &          '(Distance more than ',ditol,'m!).'
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C Get coordinates for CPCALC input file, first along x-axis wall. 
      vdist= crowxyz(Xdom(isurf),Ydom(isurf),Zdom(isurf),x3,y3,
     &   Zdom(isurf))
      xl(JC)=vdist

C Along the y-axis.     
      vdist=z3-Zdom(isurf)
      zl(JC)=vdist

C Remember for which surface a coordinate set has been defined, the overall numbers 
C of coordinate set for this surface and for which surface number it has been defined.
      isut(isurf)=1
      inum(isurf)=inum(isurf)+1
      ior(JC)=isurf

      ELSEIF (type(JC).EQ.'CEIL') THEN 
C Projection for roof - flat roof   
C In the next steps the coordinates get projected on roof surfaces 
C First find the equation of plane for the surface
          call stuf4xyz(xx,yy,zz,Xdom(5),Ydom(5),Zdom(5),Xdom(6),
     &      Ydom(6),Zdom(6),Xdom(7),Ydom(7),Zdom(7),Xdom(8),Ydom(8),
     &      Zdom(8))
        call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)

C Define a second point 1 metre below fist point
        x2=xcoab(JC)
        y2=ycoab(JC)
        z2=zcoab(JC)-1.0

C Use these two points to project first point on surface (point 3)
        CALL VECPLN(xcoab(JC),ycoab(JC),zcoab(JC),x2,y2,z2,EQN, 
     &             x3,y3,z3,IERR)

C Check whether point 3 is within distance limit to point 1
        vdist= crowxyz(xcoab(JC),ycoab(JC),zcoab(JC),x3,y3,z3)
        if (vdist.gt.ditol) then 
          Write (msga,'(a,a,I3)') 'Problem encountered with ',
     &          'the distance of coordinate definition number',JC
          Write (msgb,'(a,a,f4.2,a)') 'from the CPCALC domaine ',
     &          '(Distance more than ',ditol,'m!).'
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C  get coordinates for CPCALC input file, first along x-axis wall 
        vdist= crowxyz(Xdom(5),Ydom(5),Zdom(5),x3,Ydom(5),z3)
        xl(JC)=vdist

C along the y-axis     
        vdist= crowxyz(Xdom(5),Ydom(5),Zdom(5),Xdom(5),y3,z3)
        zl(JC)=vdist
      ELSEIF (type(JC).EQ.'SLOP'.and.irt.eq.1) THEN 

C Single sloped roof. Check orientation of roof surface in comparison
C with the thermal surface.
        diff1=azdom(1)-azim(JC)
        if (abs(diff1).gt.aztol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the orientation of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please solve this first !' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C Check the slope of the roof in comparison with the thermal surface.
        diff1=ra-(abs(elev(JC)-90.0))
        if (diff1.gt.eltol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the tilt of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please solve this first !' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C In the next steps the coordinates get projected on domain surfaces with 
C similar orientation:
C First find the equation of plane for the surface
          call stuf4xyz(xx,yy,zz,Xdom(5),Ydom(5),Zdom(5),Xdom(6),
     &      Ydom(6),Zdom(6),Xdom(10),Ydom(10),Zdom(10),Xdom(9),Ydom(9),
     &      Zdom(9))
        call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)

C Find a second point that is in normal surface direction
C one meter away from the first point 
        V1=1.0
        AZ=azdom(1)
        EL=abs(ra-90.0)
        PI = 4.0 * ATAN(1.0)
        RAD = PI/180.
        RYAZI = AZ*RAD
        RSALT = EL*RAD
        z2 = V1*SIN(RSALT)
        XYDIS = V1*COS(RSALT)
        x2 = XYDIS*SIN(RYAZI)
        y2 = XYDIS*COS(RYAZI)
        x2=x2+xcoab(JC)
        y2=y2+ycoab(JC)
        z2=z2+zcoab(JC)

C Use these two points to project first point on surface (point 3)
        CALL VECPLN(xcoab(JC),ycoab(JC),zcoab(JC),x2,y2,z2,EQN, 
     &             x3,y3,z3,IERR)

C Check whether point 3 is within distance limit to point 1
        vdist= crowxyz(xcoab(JC),ycoab(JC),zcoab(JC),x3,y3,z3)
        if (vdist.gt.ditol) then 
          Write (msga,'(a,a,I3)') 'Problem encountered with ',
     &          'the distance of coordinate definition number',JC
          Write (msgb,'(a,a,f4.2,a)') 'from the CPCALC domain ',
     &          '(Distance more than ',ditol,'m!).'
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C  get coordinates for CPCALC input file, first along x-axis wall 
        vdist= crowxyz(Xdom(5),Ydom(5),Zdom(5),x3,Ydom(5),Zdom(5))
        xl(JC)=vdist

C along the y-axis     
        vdist= crowxyz(Xdom(5),Ydom(5),Zdom(5),Xdom(5),y3,Zdom(5))
        zl(JC)=vdist

C Remember coordinate parameters (see above)
        isut(5)=1
        inum(5)=inum(5)+1
        ior(JC)=5
      ELSEIF (type(JC).EQ.'SLOP'.and.irt.eq.2) THEN

C Double sloped roof.  Check orientation of roof surface in comparison
C with the thermal surface.
        diff1=azdom(1)-azim(JC)
        diff2=azdom(3)-azim(JC)
        if (abs(diff1).gt.aztol.and.abs(diff2).gt.aztol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the orientation of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please solve this first !' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C Check the slope of the roof in comparison with the thermal surface.
        diff1=ra-(abs(elev(JC)-90.0))
        if (diff1.gt.eltol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the tilt of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please solve this first !' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C In the next steps the coordinates get projected on domain surfaces with 
C similar orientation:  First find the equation of plane for the surface
        if (isurf.eq.1) then 
          call stuf4xyz(xx,yy,zz,Xdom(5),Ydom(5),Zdom(5),Xdom(6),
     &      Ydom(6),Zdom(6),Xdom(10),Ydom(10),Zdom(10),Xdom(9),Ydom(9),
     &      Zdom(9))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
        endif
        if (isurf.eq.3) then 
          call stuf4xyz(xx,yy,zz,Xdom(7),Ydom(7),Zdom(7),Xdom(8),
     &      Ydom(8),Zdom(8),Xdom(9),Ydom(9),Zdom(9),Xdom(10),Ydom(10),
     &      Zdom(10))
          call PLEQN(XX,YY,ZZ,4,VP,EQN,IERR)
        endif

C Find a second point that is in normal surface direction
C one meter away from the first point. 
        V1=1.0
        AZ=azdom(isurf)
        EL=abs(ra-90.0)
        PI = 4.0 * ATAN(1.0)
        RAD = PI/180.
        RYAZI = AZ*RAD
        RSALT = EL*RAD
        z2 = V1*SIN(RSALT)
        XYDIS = V1*COS(RSALT)
        x2 = XYDIS*SIN(RYAZI)
        y2 = XYDIS*COS(RYAZI)
        x2=x2+xcoab(JC)
        y2=y2+ycoab(JC)
        z2=z2+zcoab(JC)

C Use these two points to project first point on surface (point 3).
        CALL VECPLN(xcoab(JC),ycoab(JC),zcoab(JC),x2,y2,z2,EQN, 
     &             x3,y3,z3,IERR)

C Check whether point 3 is within distance limit to point 1.
        vdist= crowxyz(xcoab(JC),ycoab(JC),zcoab(JC),x3,y3,z3)
        if (vdist.gt.ditol) then 
          Write (msga,'(a,a,I3)') 'Problem encountered with ',
     &          'the distance of coordinate definition number',JC
          Write (msgb,'(a,a,f4.2,a)') 'from the CPCALC domain ',
     &          '(Distance more than ',ditol,'m!).'
          CALL USRMSG(msga,msgb,'W')
          return
        endif      

C Get coordinates for CPCALC input file, first along x-axis wall. 
        if(isurf.eq.1) inp=5
        if(isurf.eq.3) inp=7
        vdist= crowxyz(Xdom(inp),Ydom(inp),Zdom(inp),x3,Ydom(inp),
     &    Zdom(inp))
        xl(JC)=vdist

C Along the y-axis.     
        vdist= crowxyz(Xdom(inp),Ydom(inp),Zdom(inp),Xdom(inp),y3,
     &    Zdom(inp))
        zl(JC)=vdist

C Remember coordinate parameters (see above).
        isut(5)=1
        inum(5)=inum(5)+1
        ior(JC)=5
      ENDIF
 157  CONTINUE

C Create input file for CPCALC, if error return. 
      IUM=IFIL+7
      nofi=1
      CALL EFOPSEQ(IUM,'ENVBUILD',3,IER)
      IF(IER.NE.0) THEN
        CALL USRMSG('problems creating CpCalc input file',' ','W')
        IER=1
        RETURN
      ENDIF
      Write(IUM,767)pad,sbh,ble,bwi,bhi,wvpe,irt,ra,'  # PaD,SbH,', 
     &               'BdX,BdY,BdZ,VeEXP,Rf type,Rf angle'
 767  Format(F4.1,F5.1,2F6.1,F5.1,F5.2,I2,F5.1,A,A)
      DO 189 JCC=1,5
        IF (isut(JCC).eq.1) THEN
          Write(IUM,'(I2,A)')JCC,'  # surface number'
          Write(IUM,'(I2,A,A)')inum(JCC),'  # number of coordiates',
     &       ' for surface'
          DO 333 JCCC=1,nuco
            IF (ior(JCCC).eq.JCC) then
              Write(IUM,'(I2,2F8.2,1X,A40,A,A,A)')nofi,xl(JCCC),
     &         zl(JCCC), naco(JCCC),'# Continous numbering, xl ',
     &         'and zl, description'
              nofi=nofi+1
            ENDIF
 333      CONTINUE
        ENDIF
 189  CONTINUE
      CALL ERPFREE(IUM,ISTAT)

C Run CPCALC. 
      CALL USRMSG('ESP-r will now run CpCalc. Please check in',
     & ' the text window for progress and error messages','W')
      doit = 'c2e'
      call tchild(ICPMOD)
      call termode(ICPMOD,tmode)
      call runit(doit,tmode)

C Save data to file.
      dok=.true.
      h(1)='You can either append the data to the currently used '
      h(2)='database or you can save it as a new database.'
      h(3)='If you are using the default pressure coefficent'
      h(4)='database ESP-r will suggest a file in your directory'
      h(5)='to save the data since you do not have writing'
      h(6)='permission to the default one. This file will then'
      h(7)='include the default pc-sets but also the ones you'
      h(8)='created.'
      CALL ASKOK(
     &  'Check the data in c2e.cpdb. Do you want to save the new',
     &  'pressure coefficients? (see help)',OK,dok,8)
      if(OK)then
        CALL EASKAB('How do you want to save the data?',' ',
     &    'new pc database','append to existing pc database',IWHICH,8)
        IF (IWHICH.EQ.1) THEN

C Read in pc from file c2e.cpdb
          t72='c2e.cpdb'
          CALL ERPRCDB(t72,0,3,IER)
          if (ier.ne.0) return

C Write data to new file. 
 777      LAPRES=' '
          LTMP=LAPRES
          H(1)='To which file do you want to write the data?'
          CALL EASKS(LTMP,' Pressure coefficients database?',
     &         ' ',72,DAPRES,'pressure coefficients database',IER,1)
          if(LTMP.ne.'  ')then
            call EMKAPCDB(LAPRES,IER)
          else
            goto 777
          endif
        ELSE

C Read in pc from file c2e.cpdb
          ndep=NPRE
          t72='c2e.cpdb'
          CALL ERPRCDB(t72,ndep,3,IER)
          if (ier.ne.0) return

C If LAPRES=DAPRES change LAPRES to local pressc.db1 and write data.
          if(LAPRES(1:lnblnk(LAPRES)).eq.
     &       DAPRES(1:lnblnk(DAPRES))) LAPRES='pressc.db1'
          call EMKAPCDB(LAPRES,IER)
        ENDIF
      endif
      return
      
      END


C *************** 
C EMKAPCD: Creates an ascii pressure coefficient database based on the
C current contents of pc-data in common block.
 
      SUBROUTINE EMKAPCDB(FINA,IER)
#include "net_flow.h"
#include "building.h"
#include "esprdbfile.h"

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/MFLDOC/DEPRE(MPRD)
      common/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

      character LAPROB*72,FINA*72
      character DEPRE*40

C Check the number of pressure coefficients and give a warning 
C message if the number is too big.
      IF(NPRE.GT.MPRD)THEN
        CALL USRMSG(' too many items to put in ',LAPRES,'W')
        IER=1
        RETURN
      ENDIF

C Open file, if error return.
      IUM=IFIL+1
      CALL EFOPSEQ(IUM,FINA,4,IER)
      IF(IER.NE.0) then
        CALL USRMSG('problems writing to press coef-db',' ','W')
        IER=1
        RETURN
      ENDIF

C Write data to the file and close.
      WRITE(IUM,'(I3,A)')NPRE,'     # no of pressure coefficient sets'
      DO 234 kl = 1,NPRE
        Write(IUM,'(10F7.3)')(FPRE(lk,kl),lk=1,10)
        Write(IUM,679)(FPRE(lk,kl),lk=11,16),
     &    DEPRE(kl)(1:lnblnk(DEPRE(kl))),'  # SET ',kl
 679    Format(6(F7.3),2X,A,A,I2)
  234 CONTINUE
      CALL ERPFREE(IUM,ISTAT)
      return
      END

C *************** EDPCDB
C EDPCDB Display a pressure coefficient db and allow changes
C IER=0 OK, IER=1 problem.

      SUBROUTINE EDPCDB(IER)
#include "net_flow.h"
#include "epara.h"
#include "building.h"
#include "esprdbfile.h"
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/MFLDOC/DEPRE(MPRD)
      common/pophelp/h(60)
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

      DIMENSION IVAL(MPRD),COMPS(MPRD)
      CHARACTER PCITM(35)*42
      character DEPRE*40,KEY*1,outwin*72,H*72,COMPS*40
      character*72 ITEMS(14)
      character LAPROB*72
      LOGICAL OK,dok,moddb

      IER=0
      moddb =.false.

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this set based on info
C in the pressure coefficient database. Setup for multi-page menu.
      MHEAD=0
      MCTL=6
      ILEN=NPRE
      IPACT=CREATE
      CALL EKPAGE(IPACT)

 3    IER=0
      ILEN=NPRE
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=0
      DO 20 IM=1,NPRE
        IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(M,KEY,IER)   
          WRITE(PCITM(M),'(A1,1X,A)')KEY,DEPRE(IM)
        ENDIF
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        PCITM(M+1)='  ________________________  '
      ELSE
        WRITE(PCITM(M+1),15)IPM,MPM 
 15   FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
      ENDIF
      PCITM(M+2)='1 add/delete/copy pres coef-set '
      PCITM(M+3)='! list db                '
      PCITM(M+4)='> update db              '
      PCITM(M+5)='? help                   '
      PCITM(M+6)='- exit this menu         '
      IVERT=-1

C Help text for the menu.
      H(1)='To enable the calculation of wind-induced surface'
      H(2)='pressures (as required by ESP-r`s fluid flow'
      H(3)='simulators), this database of angle dependent'
      H(4)='pressure coefficients is provided.'
      H(5)=' '
      H(6)='Wind pressure coefficients are pressure coefficients'
      H(7)='for surfaces in typical positions and within several'
      H(8)='different exposure categories. '
      H(9)=' '
      H(10)='The first 27 data sets in the standard pressure coef'
      H(11)='database are from the Air Infiltration and Ventilation'
      H(12)='Centre Handbook called <Air Infiltration Calculation'
      H(13)='Techniques - An Applications Guide>. The 27 data sets'
      H(14)='are only applicable for buildings up to 3 storeys and'
      H(15)='come with the usual warnings about usage of the data.'
      H(16)=' '
      H(17)='Each data set is described with the following terms: '
      H(18)=' a) facade or roof at different roof pitch angles'
      H(19)=' b) the ratio of the building length to width and'
      H(20)='    for a non square building there will be a long and'
      H(21)='    a short wall to select'
      H(22)=' c) site shielding as follows:'
      H(23)='   exposed - open countryside with no obstructions'
      H(24)='   simi-exposed - rural with some obstructions'
      H(25)='   sheltered - urban, surrounded on all sides by'
      H(26)='     obstructions of similar size.'
      H(27)=' '
      H(28)='Here you can change the data of an existing pressure'
      H(29)='coefficient set (by clicking on it), add/delete/copy'
      H(30)='a set, list the file contents of the existing pc-file'
      H(31)='or update changes you have made.'

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('Pressure Coefficient db',PCITM,NITMS,IVERT)
      else
        CALL EMENU('Pressure Coef.',PCITM,NITMS,IVERT)
      endif
      IF(IVERT.EQ.NITMS)THEN

C Check for changes/ask user to save.
        if(moddb)then
          dok=.true.
          h(1)='Recent changes to the coefficients might be lost if'
          h(2)='you do not save it to the database. '
          CALL ASKOK(' ',
     &      'Update the pressure coef. database for recent changes?',
     &      OK,dok,2)
          if(OK)then
            CALL EMKAPCDB(LAPRES,IER)
            moddb =.false.     
          endif      
        endif
        RETURN
      ELSEIF(IVERT.EQ.(NITMS-1))THEN

C The block with the help messages.
        CALL PHELPD('pc help menue',31,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(NITMS-2)) THEN

C Update pressure coefficient database. 
        CALL EMKAPCDB(LAPRES,IER)
        moddb =.false. 
      ELSEIF(IVERT.EQ.(NITMS-3))THEN

C List pressure coefficients. 
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,
     &    'A summary of the pressure coefficient sets follows:')
        CALL EDISP(IUOUT,' ')
        WRITE(OUTWIN,'(A,I2,A)')'The database contains ',NPRE,
     &    ' pressure coefficient sets.'
        CALL EDISP(IUOUT,OUTWIN)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,
     &    'The first pressure coefficient is NORMAL to the surface.')
        CALL EDISP(IUOUT,
     &    'Every following pc is at steps of 22.5 degree clockwise.')
        CALL EDISP(IUOUT,' ')
        DO 23 ig=1,NPRE
          WRITE(OUTWIN,'(a)')DEPRE(ig)
          CALL EDISP(IUOUT,OUTWIN)
          Write(OUTWIN,96)(FPRE(igg,ig),igg=1,8)
 96       Format(1X,8(F7.3))
          CALL EDISP(IUOUT,OUTWIN)
          Write(OUTWIN,96)(FPRE(igg,ig),igg=9,16)
          CALL EDISP(IUOUT,OUTWIN)
 23     CONTINUE
      ELSEIF(IVERT.EQ.(NITMS-4))THEN

C The next section is to add/delete/copy a set.
        H(1)='The continue option allows you to cancel choice. '
        call easkatog('Modify sets:',' ','add (manually)',
     &    'add (via caluclation)','delete','copy','continue',' ',' ',
     &    iw,1)
        if(IW.EQ.1)then
         
C Add set. 
          NPRE=NPRE+1
          DEPRE(NPRE)='new pressure cofficent set'
          DO 8315 IVW=1,16
            FPRE(IVW,NPRE)=0.000
 8315     CONTINUE
          CALL EDPCST(IER,moddb,NPRE)
        elseif(IW.EQ.2)then

C Calcualte a set.
 666      INOO=-4
          IER=0
          items(1)                 = ' Data used by CPCALC  -'
          WRITE(ITEMS(2),'(A,F5.1)') '  building width       :',ble
          WRITE(ITEMS(3),'(A,F5.1)') '  building depth       :',bwi
          WRITE(ITEMS(4),'(A,F5.1)') '  building hight       :',bhi
          WRITE(ITEMS(5),'(A,3F5.1)')'  origin :',blox,bloy,bloz
          WRITE(ITEMS(6),'(A,3F5.1)')'  orientation          :',orient
          WRITE(ITEMS(7),'(A,I3)')   '  roof type            :',irt
          WRITE(ITEMS(8),'(A,F5.1)') '  roof tilt angle      :',ra
          ITEMS(9 )=                 '  _________________________      '
          ITEMS(10)=                 'a define facade coordinates      '
          ITEMS(11)=                 'b calculate pressure coefficients'
          ITEMS(12)=                 '  _________________________      '
          ITEMS(13)=                 '? help                           '
          ITEMS(14)=                 '- exit this menu                 '
          nitms=14

C Help text for this menu.
          h(1)='Parameter a to j are general building parameters'
          h(2)='required by CPCALC and are defined in the model'
          h(3)='context.'
          h(4)='In "facade coordinates" the user can define for which'
          h(5)='coordinates he wants to calculate pressure'
          h(6)='coefficients.'
          h(7)='"Calculate pressure coefficients" runs CPCALC.'

          CALL EMENU('CPCALC data',ITEMS,nitms,INOO)
          if(inoo.ge.1.and.inoo.lt.9) then

C No general paramter input, refer to model context
            CALL USRMSG(' ',
     &       'Please change the parameter in the model context.','W')
          elseif(INOO.EQ.nitms) then 

C Exit the interface.
            goto 3
          elseif(INOO.EQ.nitms-1) then

C Produce help text. 
            CALL PHELPD('cpcalc in mfs',7,'-',0,0,IER)
          elseif(INOO.EQ.10) then

C Define facade coordinates. 

            CALL EPMENSV
            call EDCPFAC(ITRC,IER)
            CALL EPMENRC
          elseif(INOO.EQ.11) then

C  Calculate pressure coefficients. 
            call CPCACAL(IER)
          endif
          goto 666
        elseif(IW.EQ.3)then

C Delete set. 
          H(1)='You can delete one pressure coefficient set'

C Push the current state of menu before delete list.
          CALL EPMENSV
          DO 66 IWW=1,NPRE
            WRITE(COMPS(IWW),'(A40)')DEPRE(IWW)(1:40)
  66      CONTINUE
          IX=1
          CALL EPICKS(IX,IVAL,'Delete which one?',' ',
     &     40,NPRE,COMPS,'constructions',IER,1)
          CALL EPMENRC
          IF(IX.EQ.0)RETURN
          IWHICH=IVAL(1)

C Confirm choice
          dok=.false.
          h(1)='Chance to abort deletion of pressure coef set.'
          CALL ASKOK(' ',' Are you sure? ',OK,dok,1)
          IF(.NOT.OK)GOTO 3
          moddb =.true.

C Create new numbering for pressure coefficient set
          DO 891 IVV=IWHICH,NPRE-1
            DEPRE(IVV)=DEPRE(IVV+1)
            DO 831 IVW=1,16
              FPRE(IVW,IVV)=FPRE(IVW,IVV+1)
 831        CONTINUE
 891      CONTINUE

C Reduce number of pressure coefficient sets
          NPRE=NPRE-1
          ILEN=NPRE
          GOTO 3
        elseif(IW.EQ.4) then

C Copy set. 
          CALL EPMENSV
          DO 661 IWW=1,NPRE
            WRITE(COMPS(IWW),'(A36)')DEPRE(IWW)(1:36)
  661     CONTINUE
          IX=1
          CALL EPICKS(IX,IVAL,'Copy which one?',' ',
     &     36,NPRE,COMPS,'constructions',IER,1)
          CALL EPMENRC
          IF(IX.EQ.0)RETURN
          IWHICH=IVAL(1)
          NPRE=NPRE+1
          DEPRE(NPRE)='new pressure cofficent set'
          DO 8313 IVW=1,16
            FPRE(IVW,NPRE)=FPRE(IVW,IWHICH)
 8313     CONTINUE
          CALL EDPCST(IER,moddb,NPRE)
        endif
      ELSEIF(IVERT.EQ.NITMS-5)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Editing an existing pressure coefficient set
        CALL KEYIND(NITMS,IVERT,IFOC,IO)
        CALL EDPCST(IER,moddb,IFOC)
      else
        INO=-1
        GOTO 3 
      ENDIF
      INO=-4
      GOTO 3 
      END

C*******  SUBROUTINE EDPCST(IER,moddb,IFOC)
C EDPCST allows changes of the name of a pressure coefficient 
C set or the values of the pressure coefficients.
      SUBROUTINE EDPCST(IER,moddb,IFOC)
#include "net_flow.h"
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/OUTIN/IUOUT,IUIN
      common/MFLDOC/DEPRE(MPRD)
      common/pophelp/h(60)
      character DEPRE*40,outs*72,hold*72,H*72
      character hold2*40
      logical moddb

      IER=0
      moddb =.false.

C Change name of pressure cofficent set
 350  HOLD2=DEPRE(IFOC)
      H(1)='Give a name for the pressure coefficient set'
      CALL EASKS(HOLD2,'Name of pressure coefficient set',
     &    ' ',40,' ','pc name',IER,1)
      IF(IER.NE.0)GOTO 350
      DEPRE(IFOC)=HOLD2

C Present vertex coords for editing then parse data from HOLD
C First eight pressure coeficients ...
  35  HOLD=' '
      WRITE(HOLD,'(1x,8f7.3)')(FPRE(J,IFOC),J=1,8)
      H(1) ='The values for the pressure coefficients should be '
      H(2) ='between -1.5 and 1.5.'
      H(3) ='The first pressure coefficient is defined as NORMAL'
      H(4) ='to the surface (see drawing) and the following'
      H(5) ='coefficients are defined at steps of 22.5 degree'
      H(6) ='in a clockwise direction.'
      H(7) ='   _____ '
      H(8) ='        |' 
      H(9) ='        |'     
      H(10)='        |  <----'
      H(11)='        | The first pressure coefficient is defined'
      H(12)='   _____| as NORMAL to the surface'
      write(outs,'(a,a)')'PC 1 to 8 - ',DEPRE(IFOC)
      CALL EASKS(HOLD,outs,' ',72,
     &    ' 0.5 0.4 0.3 0.1 -0.1 -0.3 -0.4 -0.5 ','pc input',IER,12)
      IF(IER.NE.0)GOTO 35

C Check whether number of returned number is 8, if so extract data.
      CALL CHITMS(HOLD,NV)
      if(NV.ne.8)goto 35
      K=0
      DO 36 jt=1,8
        CALL EGETWR(HOLD,K,GV,-1.5,1.5,'W','pc input',IER)
        IF(IER.NE.0)GOTO 35
        FPRE(jt,IFOC)=GV
   36 CONTINUE

C Second eight pressure coeficients ...
  351 HOLD=' '
      outs=' '
      WRITE(HOLD,'(1x,8f7.3)')(FPRE(J,IFOC),J=9,16) 
      write(outs,'(a,a)')'PC 9 to 16 - ',DEPRE(IFOC)
      CALL EASKS(HOLD,outs,' ',72,
     &    ' -0.5 -0.4 -0.3 -0.1 0.1 0.3 0.4 0.5 ','pc input',IER,12)
      IF(IER.NE.0)GOTO 351

C Check whether number of returned numbers is 8, if so, extract data.
      CALL CHITMS(HOLD,NV)
      if(NV.ne.8)goto 351
      K=0
      DO 361 jt=9,16
        CALL EGETWR(HOLD,K,GV,-1.5,1.5,'W','pc input',IER)
        IF(IER.NE.0)GOTO 351
        FPRE(jt,IFOC)=GV
  361 CONTINUE
      moddb =.true.
      return
      END

C ************* ERPRCDB
C Given file name, read an ASCII pressure coefficient database.
C INPC is the number of pc sets that allready exist in case
C that the data that is read in should be appended to an existing file.
      SUBROUTINE ERPRCDB(FINA,INPC,ICO,IER)
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/MFLDOC/DEPRE(MPRD)

      CHARACTER OUTSTR*124,DEPRE*40,setn*40,FINA*72 

C Open the file, of problem return.
      IUM=IFIL+1
      CALL EFOPSEQ(IUM,FINA,ICO,IER)
      IF(IER.NE.0) THEN
        CALL USRMSG('problems reading press coef db',' ','W')
        IER=1
        RETURN
      ENDIF

C Read the number of pressure coefficient sets in the database,
      CALL STRIPC(IUM,OUTSTR,1,ND,1,'press db ln 1',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NPRET,0,MPRD,'W','no press item',IER)
      IF(IER.NE.0) GOTO 101

C Loop through all the pressure coefficient sets and descriptions.
      DO 98 na=1,NPRET

C Increase the value for NPRE by na in case the data that is read
C in should be appended to an existing file.
        NPRE=INPC+na
        K=0
        CALL STRIPC(IUM,OUTSTR,10,ND,1,'press db data 1st',IER)
        IF(IER.NE.0) GOTO 101
        do 56 naa=1,10
          CALL EGETWR(OUTSTR,K,PC,-1.5,1.5,'W','pres coef',IERV) 
          IF(IER.NE.0) GOTO 101
          FPRE(naa,NPRE)=PC 
  56    continue

C Read the second line of each set for remaining data and description.
        CALL STRIPC(IUM,OUTSTR,0,ND,1,'press db data 2nd',IER)
        IF(IER.NE.0) GOTO 101
        K=0
        do 65 naa=11,16
          CALL EGETWR(OUTSTR,K,PC,-1.5,1.5,'W','pres coef',IER) 
          IF(IER.NE.0) GOTO 101
          FPRE(naa,NPRE)=PC 
  65    continue
        CALL EGETRM(OUTSTR,K,setn,'W','pc set name',IER)
        DEPRE(NPRE)=setn
  98  CONTINUE
      CALL ERPFREE(IUM,ISTAT)

C If an error when reading in the pressure coefficient file exit.
C Assumes that calling routine closes the database.
 101  IF(IER.NE.0) THEN
        CALL ERPFREE(IUM,ISTAT)
        RETURN
      ENDIF
      return
      END


C ********  PCUPDT **********
C Update header block after ITMLOC has changed. If IFLG = 0, then
C zeroize header information
C This is similar to code in esrupdb/pc_manip.F but patched to use
C a unit number passed in as a parameter
      SUBROUTINE PCUPDT (IFIL,IFLG)
#include "plantdb.h"
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATC/NPC,ITMLOC(MAXPC,2)
      CHARACTER*25 ZDESC

C Write header.
      IF (IFLG .EQ. 0) THEN
        ZDESC = ' PLANT COMPONENT DATABASE'
        IREC = 1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) ZDESC

C Zeroize no of items.
        NPC = 0
        NXTREC = MHEADR+1

C Zeroize item pointers
        DO 100 I = 1, MAXPC
          ITMLOC(I,1) = 0
          ITMLOC(I,2) = 0
  100   CONTINUE
      ENDIF

C Update item pointers and count
      IREC = 2
      WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC, NXTREC
      NR = 1
      DO 110 I = 3, MHEADR
        NRR = NR + IRECLN - 1
        IREC = I
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(ITMLOC(J,1),J=NR,NRR)
        NR = NR + IRECLN
  110 CONTINUE

C Return
      RETURN

C Error handling
  1   CALL usrmsg('Error in prj eddb:pcupdt plant database creating.',
     &  ' a new plant database.','W')
      return
      END
