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


C prescoef.F contains subroutines related to management of
C wind pressure coefficient data.

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
C Reads in general building data as required by CPCALC to
C calculate external surface pressure coefficients.
      subroutine cpcdat

#include "building.h"
#include "prj3dv.h"
#include "help.h"

      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      dimension ITEMS(15)
      character items*36,hold*32
      integer nitms,INO ! max items and current menu item

      logical moddb,close

      helpinsub='prescoef'     ! set for subroutine

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
      WRITE(ITEMS(1),'(A,F5.1)')  'a  building width:',ble
      WRITE(ITEMS(2),'(A,F5.1)')  'b  building depth:',bwi
      WRITE(ITEMS(3),'(A,F5.1)')  'c  building height:',bhi
      WRITE(ITEMS(4),'(A,3F5.1)') 'd  origin:',blox,bloy,bloz
      WRITE(ITEMS(5),'(A,3F5.1)') 'e  orientation:',orient
      if(irt.eq.0)then
        WRITE(ITEMS(6),'(A)')     'f  roof type: flat'
      elseif(irt.eq.1)then
        WRITE(ITEMS(6),'(A)')     'f  roof type: single slope'
      elseif(irt.eq.2)then
        WRITE(ITEMS(6),'(A)')     'f  roof type: double slope'
      endif
      WRITE(ITEMS(7),'(A,F5.1)')  'g  roof tilt angle:',ra
      WRITE(ITEMS(8),'(A,F5.1)')  'h  surroundings height:',sbh
      WRITE(ITEMS(9),'(A,F5.1)') 'i  plan area density:',pad
      WRITE(ITEMS(10),'(A,F5.2)') 'j  wind prof. exponent:',wvpe
      ITEMS(11)=                  '  _________________________ '
      ITEMS(12)=                  '? help                      '
      ITEMS(13)=                  '- exit menu'
      nitms=13

C Help text for this menu.
  47  helptopic='cpc_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('CPCALC data',ITEMS,nitms,INO)
      if(moddb)then

C << Put redraw of the outline here >>
      endif

C Update configuration file before exiting.
      if(INO.EQ.nitms)then
        if(moddb)CALL EMKCFG('-',IER)
        RETURN

C Help message.
      elseif(INO.EQ.nitms-1)then
        CALL PHELPD('prim convers',nbhelp,'-',0,0,IER)

C Building width.
      elseif(ino.eq.1)then
        def=ble
        CALL EASKR(ble,' ','Overall building width?',
     &       0.0,'F',0.0,'-',def,'CPCALC-ble',IER,nbhelp)
        moddb=.true.
        icpcon=1

C Building depth.
      elseif(ino.eq.2)then
        def=bwi
        CALL EASKR(bwi,' ','Overall building depth?',
     &       0.0,'F',0.0,'-',def,'CPCALC-bwi',IER,nbhelp)
        moddb=.true.
        icpcon=1

C Building height.
      elseif(ino.eq.3)then
        def=bhi
        CALL EASKR(bhi,' ','Overall building height?',
     &       0.0,'F',0.0,'-',def,'CPCALC-bhi',IER,nbhelp)
        moddb=.true.
        icpcon=1

C Domain origin.
      elseif(ino.eq.4)then
        write(hold,'(3F7.2)') blox,bloy,bloz
        CALL EASKS(HOLD,' ','Origin (X Y Z) of CPCALC domain?',
     &        32,' 0. 0. 0.','origin XYZ',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,blox,-999.,999.,'W','X org',IER)
        CALL EGETWR(HOLD,K,bloy,-999.,999.,'W','Y org',IER)
        CALL EGETWR(HOLD,K,bloz,-999.,999.,'W','Z org',IER)
        moddb=.true.

C  Bulding orientation.
      elseif(ino.eq.5)then
        def=0.
        CALL EASKR(orient,' ','Building orientation?',
     &      0.0,'F',180.0,'F',def,'CPCALC-orient',IER,nbhep)
        moddb=.true.
        icpcon=1

C Roof type.
      elseif(ino.eq.6)then
        helptopic='cpc_roof_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' ','Roof type options:','flat',
     &  'single slope','double slope',IWHICH,nbhelp)
        if (IWHICH.EQ.1)then
          irt=0
          ra=0.0
        endif
        IF (IWHICH.EQ.2) irt=1
        IF (IWHICH.EQ.3) irt=2
        IF (IWHICH.EQ.2.or.IWHICH.eq.3)then
          def=10.
          CALL EASKR(ra,' ','Roof tilt angle?',
     &        0.0,'F',50.0,'F',def,'CPCALC-ra',IER,nbhelp)
        endif
        moddb=.true.
        icpcon=1

C  Roof tilt angle.
      elseif(ino.eq.7)then
        IF(irt.NE.0) THEN
          def=10.
          helptopic='cpc_roof_type'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(ra,' ','Roof tilt angle?',
     &        0.0,'F',50.0,'F',def,'CPCALC-ra',IER,nbhelp)
          moddb=.true.
          icpcon=1
        ENDIF

C Surroundings height.
      elseif(ino.eq.8)then
        def=5.
        helptopic='cpc_roof_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(sbh,' ','Surroundings height?',
     &       0.0,'F',0.0,'-',def,'CPCALC-sbh',IER,nbhelp)
        moddb=.true.
        icpcon=1

C Plan area density.
      elseif(ino.eq.9)then
        helptopic='cpc_plan_density'
        call gethelptext(helpinsub,helptopic,nbhelp)
        def=8.0
        CALL EASKR(pad,' ','Plan area density?',
     &       0.0,'F',50.0,'F',def,'CPCALC-pad',IER,nbhelp)
        moddb=.true.
        icpcon=1

C Wind velocity profile exponent.
      elseif(ino.eq.10)then
        def=0.28
        helptopic='cpc_velocity_profile'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(wvpe,' ',' Wind velocity profile exponent?',
     &       0.0,'F',1.0,'F',def,'CPCALC-wvpe',IER,nbhelp)
        moddb=.true.
        icpcon=1
      else
        INO=-4
        GOTO 47
      endif
      INO=-4
      GOTO 31
      end

C ************************** EDCPFAC **************************
C Edit coordinates that define at which facade location CPCALC
C should calculate pressure coefficients.

      SUBROUTINE EDCPFAC(IER)

#include "epara.h"
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "prj3dv.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)
      common/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      common/CPCALC2/FICOCP

      LOGICAL OK
      logical CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      DIMENSION VERT(35),COMPS(MPCP),IVAL(MPCP)
      CHARACTER VERT*75,KEY*1,DESCRC*25
      CHARACTER hold2*40,naco*40,COMPS*72
      CHARACTER*4 type
      character*72 LTMP,FICOCP
      CHARACTER OUTSTR*124,RSTR*124
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16,t28*28

      DIMENSION COG1(3),COG2(3)
      DIMENSION XX(MV),YY(MV),ZZ(MV)
      DIMENSION VP(3),EP(3),EQN(4),TMAT(4,4),RMAT(4,4)
      real DX1,DZ1  ! to avoid name clash in geometry.h
      integer MVERT,IVERT ! max items and current menu item
      
#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

      helpinsub='prescoef'     ! set for subroutine

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
      VERT(M+4)  ='< open a coordinates file           '
      VERT(M+5)  ='> save coordinates to file          '
      VERT(M+6)  ='? help                              '
      VERT(M+7)  ='- exit menu'
      IVERT=-1

C Help text for the menu.
      helptopic='cpc_coords'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('Coordinates used by CPCALC',VERT,
     &  MVERT,IVERT)
      else
        CALL EMENU('CPCALC coordinates',VERT,MVERT,IVERT)
      endif
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Leave subroutine.
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu. 
        CALL PHELPD('input cpcalc coordinates',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C Save coordinates.
        LTMP=FICOCP
 878    CALL EASKS(LTMP,' ','Facade coordinates file?',
     &      72,' ','facade coordinates file',IER,5)
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
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Open coordinate file.
        CALL EASKS(LTMP,' ','Facade coordinates file?',
     &    72,' ','read facade coordinates file',IER,nbhelp)
        call usrmsg(' ',' ','-')
        IF(LTMP(1:2).NE.'  ')FICOCP=LTMP
        CALL ERPFREE(IUM,ISTAT)
        CALL EFOPSEQ(IUM,FICOCP,1,IER)
        IF(IER.NE.0)goto 92

C Read the number of facade cordinates in file (NCOO).
        CALL STRIPC(IUM,OUTSTR,0,ND,1,'no. facade coord',IER)
        IF(IER.NE.0)goto 92
        K=0
        CALL EGETWI(OUTSTR,K,NCOO,0,MPRD,'F','no. coords',IER)
        IF(IER.NE.0)goto 92

C Read in coordinates.
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
        CALL EPMENSV
        DO 66 IWW=1,NUCO
          WRITE(COMPS(IWW),31)naco(IWW),xcoab(IWW),ycoab(IWW),zcoab(IWW)
   31     FORMAT(A40,1X,3F9.3)
  66    CONTINUE
        IX=1
        CALL EPICKS(IX,IVAL,' ','Delete which one?',
     &     72,NUCO,COMPS,'coordinate sets',IER,nbhelp)
        CALL EPMENRC
        IF(IX.EQ.0)RETURN
        IWHICH=IVAL(1)

C Confirm choice
        CALL EASKOK(' ','Are you sure?',OK,1)
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
        CALL EASKAB(' ','Options:','manual input',
     &              'search for windows',iacc,nbhelp)
        IF(IACC.EQ.1) THEN

C Manual input of cordinates. 
          CALL EASKGEOF('Select a zone for coords:',CFGOK,IC,'-',34,IER)
          IUF=IFIL+1
          call georead(IUF,LGEOM(IC),IC,1,iuout,IER)
          nzg=1
          nznog(1)=IC
          izgfoc=IC
          MODIFYVIEW=.TRUE.
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
          CALL redraw(IER)
 256      CALL EASKSUR(IC,ISO,'-','Define coordinates for',
     &               'which external surface?',IER)
          call ZSID(IC,ISO,ZSDES,ZSDESC,ZSDESS) 
 
C Set all surfaces to standard line width and surface being 
C selected to a thick line.
          ITVNO=0
          ITSNM=0
          MODIFYVIEW=.TRUE.
          CALL INLNST(1)
          CALL SURADJ(IC,ISO,IE,TMP,IZC,ISC,ICXX,DESCRC)
          LINSTY(ICXX)=2
          nzg=1
          nznog(1)=IC
          izgfoc=IC
          izgfoc=IC
          CALL redraw(IER)

C Ask for x offset.
          helptopic='cpc_offsets'
          call gethelptext(helpinsub,helptopic,nbhelp)
          DX1=1.0
          CALL EASKR(DX1,' ',' X offset: ? ',
     &       0.1,'-',99.9,'-',1.0,'X offset',IER,nbhelp)

C Ask for z offset.
          DZ1=1.0
          CALL EASKR(DZ1,' ','Z offset:?',
     &         0.1,'-',99.9,'-',1.0,'Z offset',IER,nbhelp)

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
          XMIN=XMIN+DX1
          YMIN=YMIN+DZ1
          CALL ORTTRN(XMIN,YMIN,ZZZ,RMAT,XXDP,YYDP,ZZDP,IERR)
          COG1(1)=XXDP
          COG1(2)=YYDP
          COG1(3)=ZZDP
          CALL VECTRN(COG1,TSMAT,COG2,IER) 
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          CALL ecirc(iix,iiy,3,1)
          call forceflush()
          write(t28,'(3(a,F6.2))')'x=',XXDP,', y=',YYDP,', z=',ZZDP
          CALL EASKOK(t28,'Use this coordinate?',OK,nbhelp)
          if(OK)then

C If yes than save coordinates ...
 350        write(HOLD2,'(a,a20,a)')'coord of ',ZSDESC,'...'
            nuco=nuco+1
            CALL EASKS(HOLD2,' ','Name cpcalc cordinates',
     &        40,' ','cpcalc co-name',IER,nbhelp)
            IF(IER.NE.0)GOTO 350 
            naco(nuco)=HOLD2
            xcoab(nuco)=XXDP
            ycoab(nuco)=YYDP
            zcoab(nuco)=ZZDP
            azim(nuco)=SPAZI(IC,ISO)
            if (azim(nuco).gt.359.9) azim(nuco)=0.0
            type(nuco)=SVFC(IC,ISO)
            elev(nuco)=SPELV(IC,ISO)
          endif

C Ask whether user wants to define another point.
          CALL EASKOK(' ','Define another point?',OK,nbhelp)
          if(OK) goto 256
        ELSEIF(IACC.EQ.2) THEN
          
C Open geometry file for surface property check. 
          IUF=IFIL+1
          do 46 IZU=1,NCOMP
            call georead(IUF,LGEOM(IZU),IZU,1,iuout,IER)

C  Check whether zone has external transparent surface(s)
            do 42 IZW=1,NZSUR(izu)
              if(SOTF(izu,izw)(1:4).eq.'TRAN'.and.
     &           SOTHER(izu,izw,1)(1:8).eq.'EXTERIOR') then

C  If so then display zone and centre point of the 
C  transparent surface of the zone 
                nzg=1
                nznog(1)=IZU
                izgfoc=IZU
                MODIFYVIEW=.TRUE.
                CALL ESCZONE(IZU)
                CALL BNDOBJ(0,IER)
                CALL ERCZONE(IZU)
                CALL INLNST(1)
                nzg=1
                nznog(1)=IZU
                izgfoc=IZU
                CALL redraw(IER)

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
                CALL VECTRN(COG1,TSMAT,COG2,IER)
                call u2pixel(COG2(1),COG2(2),iix,iiy)
                CALL ecirc(iix,iiy,3,1)
                call forceflush()

C Confirm whether user wants to use the coordinates 
                write(t28,'(3(a,F6.2))')'x=',xpc,', y=',ypc,', z=',zpc
                CALL EASKOK(t28,'Use this coordinate?',OK,nbhelp)
                if(OK)then

C If yes save cordinates. 
 360              continue
                  call ZSID(IZU,IZW,ZSDES,ZSDESC,ZSDESS)
                  write(HOLD2,'(a,a20,a)')'coord of ',ZSDESC,'...'
                  
                  nuco=nuco+1
                  CALL EASKS(HOLD2,' ','Name cpcalc cordinates',
     &                 40,' ','cpcalc co-name',IER,nbhelp)
                  IF(IER.NE.0)GOTO 360 
                  naco(nuco)=HOLD2
                  xcoab(nuco)=xpc
                  ycoab(nuco)=ypc
                  zcoab(nuco)=zpc
                  azim(nuco)=SPAZI(izu,izw)
                  if (azim(nuco).gt.359.9) azim(nuco)=0.0
                  type(nuco)=SVFC(izu,izw)
                  elev(nuco)=SPELV(izu,izw)
                endif
              endif               
 42         continue
 46       continue
        ENDIF
      ENDIF
      goto 92
      END
               
C ************************** CPCACAL **************************
C Take the parameters required by CPCALC, create an input file 
C and calculate the pressure coeffcients

      SUBROUTINE CPCACAL (IER)

#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      integer childterminal  ! picks up mmod from starting of prj
      common/childt/childterminal
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe
      common/cpcaco/nuco,naco(MPCP),xcoab(MPCP),ycoab(MPCP),
     &              zcoab(MPCP),azim(MPCP),TYPE(MPCP),elev(MPCP)

      CHARACTER naco*40,type*4,msga*124,msgb*124,tmode*8
      character t144*144
      CHARACTER LAPROB*72,LTMP*72,LLTMP*144,outs248*248
      character doit*248

      dimension Xdom(10),Ydom(10),Zdom(10),azdom(4)
      dimension XX(MV),YY(MV),ZZ(MV),VP(3),EQN(4)
      dimension xl(MPCP),zl(MPCP),ior(MPCP),isut(5),inum(5)
      logical ok
      integer iglib  ! are we running X11 or GTK
      integer ISTRW

      helpinsub='prescoef'     ! set for subroutine

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
      helptopic='cpc_cal_checks'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(ditol,'Maximum tolerance between the absolute', 
     &  'cordinates and the block surfaces?',0.01,'F',1.0,'F',
     &   def,'CPCALC-ditol',IER,nbhelp)

C Ask for the tolerance between the azimuth of the suface from which
C the coordinates have been derived and the azimuth of the CPCALC 
C domain.      
      def=5.0
      aztol=5.0
      CALL EASKR(aztol,
     &  'Maximum tolerance between the orientation angle', 
     &  'of zone surface and the surface of the CPCALC domain?',
     &  0.01,'F',5.0,'F',def,'CPCALC-aztol',IER,nbhelp)

C For a tilted roof: Ask for the maximum tolerance between the tilt of  
C the surface from which the coordinates have been derived and the  
C roof tilt of the CPCALC domain.
      IF (irt.eq.1.or.irt.eq.2) THEN 
        def=5.0
        eltol=5.0
        CALL EASKR(eltol,
     &    'Maximum tolerance between the tilt angle of the zone', 
     &    'surface and roof tilt of the CPCALC domain?',
     &    0.01,'F',5.0,'F',def,'CPCALC-eltol',IER,nbhelp)
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
     &          ' is derived. Please resolve this first!' 
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
     &          ' is derived. Please resolve this first!' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C Check the slope of the roof in comparison with the thermal surface.
        diff1=ra-(abs(elev(JC)-90.0))
        if (diff1.gt.eltol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the tilt of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please resolve this first!' 
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
     &          ' is derived. Please resolve this first!' 
          CALL USRMSG(msga,msgb,'W')
          return
        endif

C Check the slope of the roof in comparison with the thermal surface.
        diff1=ra-(abs(elev(JC)-90.0))
        if (diff1.gt.eltol) then 
          Write (msga,'(a,a)') 'Problem encountered with ',
     &          'the tilt of the zone surface from which' 
          Write (msgb,'(a,I2,a)') 'coordinate set ',nuco,
     &          ' is derived. Please resolve this first!' 
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
          Write(IUM,'(I3,A)')JCC,'  # surface number'
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
      call terminalmode(childterminal,tmode)
      call runit(doit,tmode)

C Save data to file.
      helptopic='cpc_append_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Save pressure coefficients?',OK,nbhelp)
      if(OK)then
        CALL EASKAB(' ','Save options:',
     &    'new PC file',
     &    'append to existing file',IWHICH,8)
        IF (IWHICH.EQ.1) THEN

C Read in pc from file c2e.cpdb rather than the standard pressure db.
          t144='c2e.cpdb'
          CALL ERPRCDB(t144,0,3,IER)
          if (ier.ne.0) return

 777      write(outs248,'(2a)') 'The current source file is:',
     &      LAPRES(1:lnblnk(LAPRES))
          call edisp248(iuout,outs248,100)
          lltmp=LAPRES
          llt=lnblnk(lltmp)
          iglib = igraphiclib()
          if(iglib.eq.1.or.iglib.eq.3)then
            if(llt.lt.96)then
              ISTRW=96
            elseif(llt.ge.96.and.llt.lt.124)then
              ISTRW=124
            elseif(llt.ge.124.and.llt.le.144)then
              ISTRW=144
            endif
          elseif(iglib.eq.2)then
            ISTRW=144
          else
            ISTRW=96
          endif
          CALL EASKF(lltmp,' ','Pressure coefficients file?',
     &      ISTRW,DAPRES,'pressure coef database',IER,nbhelp)

C Write data to new file. 
C 777      LAPRES=' '
C          LLTMP=LAPRES
C          CALL EASKS(LTMP,' Pressure coefficients database?',
C     &         ' ',72,DAPRES,'pressure coefficients database',IER,1)

C If user request cancel << logic needs to be added >>.
C          if(ier.eq.-3) return ! cancel detected.

          if(lltmp(1:2).ne.'  ')then
            call EMKAPCDB(LAPRES,IER)
          else
            goto 777
          endif
        ELSE

C Read in pc from file c2e.cpdb rather than the standard pressure db.
          ndep=NPRE
          t144='c2e.cpdb'
          CALL ERPRCDB(t144,ndep,3,IER)
          if (ier.ne.0) return

C If LAPRES=DAPRES change LAPRES to local pressc.db1 and write data.
          if(LAPRES(1:lnblnk(LAPRES)).eq.
     &       DAPRES(1:lnblnk(DAPRES))) LAPRES='pressc.db1'
          call EMKAPCDB(LAPRES,IER)
        ENDIF
      endif
      return
      
      END


C ******************** EMKAPCD ********************
C Creates an ascii pressure coefficient database based on the
C current pressure coefficients data held in common block.
 
      SUBROUTINE EMKAPCDB(FINA,IER)

#include "building.h"
#include "net_flow.h"

      integer lnblnk  ! function definition

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/MFLDOC/DEPRE(MPRD)
      common/FILEP/IFIL

      character FINA*144
      character DEPRE*40

C Check the number of pressure coefficients and give a warning 
C message if the number is too big.
      IF(NPRE.GT.MPRD)THEN
        CALL LUSRMSG(' too many items to put in ',FINA,'W')
        IER=1
        RETURN
      ENDIF

C Open file, if error return.
      IUM=IFIL+1
      CALL EFOPSEQ(IUM,FINA,4,IER)
      IF(IER.NE.0) then
        CALL USRMSG('Error writing to pressure coefficients database!',
     &              ' ','W')
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

C ******************** EDPCDB ********************
C Display a pressure coefficient database and allow changes.

      SUBROUTINE EDPCDB(IER)

#include "building.h"
#include "net_flow.h"
#include "epara.h"
#include "esprdbfile.h"
#include "help.h"

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      common/MFLDOC/DEPRE(MPRD)
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

      DIMENSION IVAL(MPRD),COMPS(MPRD)
      CHARACTER PCITM(35)*42
      character DEPRE*40,KEY*1,outwin*72,COMPS*40
      character*72 ITEMS(14)
      character LAPROB*72
      character lworking*144,fs*1
      LOGICAL OK,moddb
      integer lndbp   ! for length of standard database path
      integer lnwkg   ! for length of working file name
      integer iw      ! for radio button
      logical unixok  ! to check for database path file separators
      integer NITMS,IVERT,INOO ! max items and current menu item

      helpinsub='prescoef'     ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

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
      PCITM(M+2)='1 add/delete/copy PC sets      '
      PCITM(M+3)='! list contents                '

C Adapt save dialog if a common data file.
      if(ipathapres.eq.0.or.ipathapres.eq.1)then
        PCITM(M+4)='> save to file           '
      else
        PCITM(M+4)='> save to common file    '
      endif
      PCITM(M+5)='? help                   '
      PCITM(M+6)='- exit menu'
      IVERT=-1

C Help text for the menu.
      helptopic='wind_induced_flow'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('Pressure coeff. db',PCITM,NITMS,IVERT)
      else
        CALL EMENU('Pressure coeff.',PCITM,NITMS,IVERT)
      endif
      IF(IVERT.EQ.NITMS)THEN

C Check for changes/ask user to save.
        if(moddb)then
          CALL EASKOK(' ',
     &      'Save changes to the pressure coefficients database?',
     &      OK,nbhelp)
          if(OK)then

C << ipathapres >>
            CALL EMKAPCDB(LAPRES,IER)
            moddb =.false.     
          endif      
        endif
        RETURN
      ELSEIF(IVERT.EQ.(NITMS-1))THEN

C The block with the help messages.
        CALL PHELPD('pc help menue',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(NITMS-2)) THEN

C Update pressure coefficient database. 
C << ipathapres >>
        CALL EMKAPCDB(LAPRES,IER)
        moddb =.false. 
      ELSEIF(IVERT.EQ.(NITMS-3))THEN

C List pressure coefficients. 
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,
     &    'Pressure coefficients summary:')
        CALL EDISP(IUOUT,' ')
        WRITE(OUTWIN,'(A,I2,A)')'The database contains ',NPRE,
     &    ' pressure coefficient sets.'
        CALL EDISP(IUOUT,OUTWIN)
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,
     &    'The first pressure coefficient is NORMAL to the surface.')
        CALL EDISP(IUOUT,
     &    'Every PCs are at steps of 22.5 degrees clockwise.')
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
        iw=1
        call easkatog(' ','Options:','add (manually)',
     &    'add (via calculation)','delete','copy','cancel',' ',' ',
     &    iw,nbhelp)
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
          items(1)                 = ' Data used by CPCALC:'
          WRITE(ITEMS(2),'(A,F5.1)') '  building width:',ble
          WRITE(ITEMS(3),'(A,F5.1)') '  building depth:',bwi
          WRITE(ITEMS(4),'(A,F5.1)') '  building hight:',bhi
          WRITE(ITEMS(5),'(A,3F5.1)')'  origin:',blox,bloy,bloz
          WRITE(ITEMS(6),'(A,3F5.1)')'  orientation:',orient
          WRITE(ITEMS(7),'(A,I3)')   '  roof type:',irt
          WRITE(ITEMS(8),'(A,F5.1)') '  roof tilt angle:',ra
          ITEMS(9 )=                 '  _________________________      '
          ITEMS(10)=                 'a define facade coordinates      '
          ITEMS(11)=                 'b calculate pressure coefficients'
          ITEMS(12)=                 '  _________________________      '
          ITEMS(13)=                 '? help                           '
          ITEMS(14)=                 '- exit menu'
          nitms=14

C Help text for this menu.
          helptopic='cpc_parmater_j'
          call gethelptext(helpinsub,helptopic,nbhelp)

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
            CALL PHELPD('cpcalc in mfs',nbhelp,'-',0,0,IER)
          elseif(INOO.EQ.10) then

C Define facade coordinates. 

            CALL EPMENSV
            call EDCPFAC(IER)
            CALL EPMENRC
          elseif(INOO.EQ.11) then

C  Calculate pressure coefficients. 
            call CPCACAL(IER)
          endif
          goto 666
        elseif(IW.EQ.3)then

C Delete set. 
C Push the current state of menu before delete list.
          CALL EPMENSV
          DO 66 IWW=1,NPRE
            WRITE(COMPS(IWW),'(A40)')DEPRE(IWW)(1:40)
  66      CONTINUE
          IX=1
          CALL EPICKS(IX,IVAL,'Delete which one?',' ',
     &     40,NPRE,COMPS,'constructions',IER,nbhelp)
          CALL EPMENRC
          IF(IX.EQ.0)RETURN
          IWHICH=IVAL(1)

C Confirm choice
          CALL EASKOK(' ','Are you sure?',OK,nbhelp)
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
     &     36,NPRE,COMPS,'constructions',IER,nbhelp)
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
        INOO=-1
        GOTO 3 
      ENDIF
      INOO=-4
      GOTO 3 
      END

C ******************** EDPCST(IER,moddb,IFOC) ********************
C Allows changes of the name of a pressure coefficient 
C set or the values of the pressure coefficients.

      SUBROUTINE EDPCST(IER,moddb,IFOC)

#include "building.h"
#include "net_flow.h"
#include "help.h"

      integer iCountWords

      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/MFLDOC/DEPRE(MPRD)
      character DEPRE*40,outs*72,hold*72
      character hold2*40
      logical moddb

      helpinsub='prescoef'     ! set for subroutine

      IER=0
      moddb =.false.

      helptopic='cpc_set_manage'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Change name of pressure cofficent set
 350  HOLD2=DEPRE(IFOC)
      CALL EASKS(HOLD2,'Name of pressure coefficient set',
     &    ' ',40,' ','pc name',IER,nbhelp)
      IF(IER.NE.0)GOTO 350
      DEPRE(IFOC)=HOLD2

C Present vertex coords for editing then parse data from HOLD
C First eight pressure coeficients ...
  35  HOLD=' '
      WRITE(HOLD,'(1x,8f7.3)')(FPRE(J,IFOC),J=1,8)
      write(outs,'(a,a)')'PC 1 to 8 - ',DEPRE(IFOC)
      CALL EASKS(HOLD,outs,' ',72,
     &  ' 0.5 0.4 0.3 0.1 -0.1 -0.3 -0.4 -0.5 ','pc input',IER,
     &  nbhelp)
      IF(IER.NE.0)GOTO 35

C Check whether number of returned number is 8, if so extract data.
      NV = iCountWords(HOLD)
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
     &    ' -0.5 -0.4 -0.3 -0.1 0.1 0.3 0.4 0.5 ','pc input',IER,
     &    nbhelp)
      IF(IER.NE.0)GOTO 351

C Check whether number of returned numbers is 8, if so, extract data.
      NV = iCountWords(HOLD)
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

C ******************** ERPRCDB ********************
C Given file name, read an ASCII pressure coefficient database.
C If FINA is a blank then use the current lapres string (depending on
C its location).
C INPC is the number of pc sets that allready exist in case
C that the data that is read in should be appended to an existing file.

      SUBROUTINE ERPRCDB(FINA,INPC,ICO,IER)

#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      common/MFLDOC/DEPRE(MPRD)

      CHARACTER OUTSTR*124,DEPRE*40,setn*40,FINA*144 
      character lworking*144,fs*1
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If FINA is blank then use the current lapres variable.
      IUM=IFIL+1
      if(FINA(1:2).eq.'  ')then
       
C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use efopseq. If in
C the standard location then create lworking which has path
C prepended.
        if(ipathapres.eq.0.or.ipathapres.eq.1)then
          CALL EFOPSEQ(IUM,lapres,1,IER)
        elseif(ipathapres.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      lapres(1:lnblnk(lapres))

          CALL EFOPSEQ(IUM,lworking,1,IER)
        endif
      else
        CALL EFOPSEQ(IUM,FINA,1,IER)
      endif

C If problem return.
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

C ********************  PCUPDT ********************
C Update header block after ITMLOC has changed. If IFLG = 0
C then zeroise header information.
C <<Similar to code in esrupdb/pc_manip.F but patched to use
C a unit number passed in as a parameter.>>

      SUBROUTINE PCUPDT (IFIL,IFLG)

#include "plantdb.h"

      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCCTL/MPC, NXTREC
      common/PCDATC/NPC,ITMLOC(MAXPC,2)
      CHARACTER*25 PZDESC

C Write header.
      IF (IFLG .EQ. 0) THEN
        PZDESC = ' PLANT COMPONENT DATABASE'
        IREC = 1
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) PZDESC

C Zeroize no of items.
        NPC = 0
        NXTREC = MHEADR+1

C Zeroize item pointers.
        DO 100 I = 1, MAXPC
          ITMLOC(I,1) = 0
          ITMLOC(I,2) = 0
  100   CONTINUE
      ENDIF

C Update item pointers and count.
      IREC = 2
      WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1) NPC, NXTREC
      NR = 1
      DO 110 I = 3, MHEADR
        NRR = NR + IRECLN - 1
        IREC = I
        WRITE (IFIL,REC=IREC,IOSTAT=ISTAT,ERR=1)(ITMLOC(J,1),J=NR,NRR)
        NR = NR + IRECLN
  110 CONTINUE

      RETURN

C Error handling.
  1   CALL usrmsg('Error in prj eddb:pcupdt plant database creating.',
     &  ' a new plant database.','W')
      return
      END
