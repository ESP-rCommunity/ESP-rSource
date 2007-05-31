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

C This file contains the routines used in the interactive graphics display 
C for the network tool. 
C   NWKSLCTC Called from the c-library to determine if a component has been
C            selected.
C   NWKUPDTPOS Called from the c-library and updates component and connection
C              intermediate point positions  interaction.
C   GCONAD  The user define a network connection graphically.
C           called from the c-library.
C   GNWKQUERY Called by the c-library to get the current network viewing
C             parameters (for drawing scroll bars).
C   NETWSNAP  Calculates the position of grid point co-ordinates
C             when grid snappping is on.

C *************************** NWKSLCTC *************************
C Called from the c-side to check if an object has been selected
C IT is toggle of status for icon or connection.
C IS is the index of the icon.
C ISA is the index of the connection waypoint.
C ISELECT is a flag passed to the routine which determines whether a 
C component can be selected (ISELECT=1)
C IACTIVE is returned if ISELECT=0 but the component is already marked
C as selected.
      SUBROUTINE NWKSLCTC(IX,IY,IT,IS,ISA,ISELECT,IACTIVE)

      INCLUDE 'gnetwk.h'
C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)
   
C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      
      LOGICAL ISEL,CSEL,CLOSEX,CLOSEY

C Check if the pixels IX and IY passed from C match are close
C to a component or connection (+/-) 0.4 on the grid scale
C element.
      CALL PIXEL2U(IX,IY,GX,GY)

      CGX=GX
      CGY=GY

      CLOSEX=.FALSE.
      CLOSEY=.FALSE.
      IS=0
      ISA=0
      IT=0
      IACTIVE=0

C Hunt through the components
      DO 10 I=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(CGX,XYZICON(I,1),0.6,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,2),0.6,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(CGX,XYZICON(I,1),0.6,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,3),0.6,CLOSEY)
        ELSE
          CALL ECLOSE(CGX,XYZICON(I,2),0.6,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,3),0.6,CLOSEY)
        ENDIF
        IF(CLOSEX.AND.CLOSEY)THEN

C If select flag is on then allow selection or delection of the component
          IF(ISELECT.GT.0)THEN
            IF(ISEL(I))THEN
              ISEL(I)=.FALSE.
              IS=I
              ISA=0
              IT=1
           ELSE
              ISEL(I)=.TRUE.
              IS=I
              ISA=0
              IT=1
            ENDIF

C Otherwise just return the number of the selected component
          ELSE
            IF(ISEL(I))IACTIVE=1
            IS=I
            ISA=0
            IT=1
          ENDIF
        ENDIF
  10  CONTINUE


C Reset the search parameters
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.

      DO 20 I=1,NICNN

C Loop through each connection waypoint.
        DO 23 J=1,NCONWP(I)
          IF(IVIEW.EQ.1)THEN
            CALL ECLOSE(CGX,CNWNP(I,J,1),0.20,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(I,J,2),0.20,CLOSEY)
          ELSEIF(IVIEW.EQ.2)THEN
            CALL ECLOSE(CGX,CNWNP(I,J,1),0.20,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(I,J,3),0.20,CLOSEY)
          ELSE
            CALL ECLOSE(CGX,CNWNP(I,J,2),0.20,CLOSEX)
            CALL ECLOSE(CGY,CNWNP(I,J,3),0.20,CLOSEY)
          ENDIF
          IF(CLOSEX.AND.CLOSEY)THEN

C If the selection flag is on then allow the connection to be selected/deselected
            IF(ISELECT.GT.0)THEN
              IF(CSEL(I))THEN
                CSEL(I)=.FALSE.
                IS=I
                ISA=J
                IT=2
              ELSE
                CSEL(I)=.TRUE.
                IS=I
                ISA=J
                IT=2
              ENDIF
            ELSE
              IF(CSEL(I))IACTIVE=1
              IS=I
              ISA=J
              IT=2              
            ENDIF
          ENDIF
   23   CONTINUE
   20 CONTINUE

C Re draw the network
      CALL NETWDRW

      RETURN
      END

C ********************* NWKUPDTPOS *********************************
C Called from the c-library and updates component and connection
C intermediate point positions.
      SUBROUTINE NWKUPDTPOS(IS,ISA,IT,IX,IY)

      INCLUDE 'gnetwk.h'
C Icon commons
      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)
      character ATRTAG*12,ATRMENU*32,ATRICN*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12

      CALL PIXEL2U(IX,IY,X,Y)
      CALL NETWSNAP(X,Y,XS,YS)

C Store old icon position
      OLDPOSX=XYZICON(IS,1)
      OLDPOSY=XYZICON(IS,2)
      OLDPOSZ=XYZICON(IS,3)
      DELTAX=0.0
      DELTAY=0.0
      DELTAZ=0.0
      NVTX=NIVC(IS)
      NICP=NCONP(IS)
C Move the  icon
      IF(IT.EQ.1)THEN
        IF(IVIEW.EQ.1)THEN
          XYZICON(IS,1)=XS
          XYZICON(IS,2)=YS
          XYZICON(IS,3)=VIEWCEN(3)
          DELTAX=XYZICON(IS,1)-OLDPOSX
          DELTAY=XYZICON(IS,2)-OLDPOSY
          DELTAZ=XYZICON(IS,3)-OLDPOSZ

C Update the icon position
          DO 10 I=1,NVTX 
            VCICON(IS,I,1)=VCICON(IS,I,1)+DELTAX
            VCICON(IS,I,2)=VCICON(IS,I,2)+DELTAY
            VCICON(IS,I,3)=VCICON(IS,I,3)+DELTAZ
  10      CONTINUE

          DO 11 I=1,NICP
            CONCP(IS,I,1)=CONCP(IS,I,1)+DELTAX
            CONCP(IS,I,2)=CONCP(IS,I,2)+DELTAY
  11      CONTINUE

        ELSEIF(IVIEW.EQ.2)THEN
          XYZICON(IS,1)=XS
          XYZICON(IS,2)=VIEWCEN(2)
          XYZICON(IS,3)=YS
          DELTAX=XYZICON(IS,1)-OLDPOSX
          DELTAY=XYZICON(IS,2)-OLDPOSY
          DELTAZ=XYZICON(IS,3)-OLDPOSZ

C Update the icon position
          DO 12 I=1,NVTX
            VCICON(IS,I,1)=VCICON(IS,I,1)+DELTAX
            VCICON(IS,I,2)=VCICON(IS,I,2)+DELTAY
            VCICON(IS,I,3)=VCICON(IS,I,3)+DELTAZ
  12      CONTINUE
          DO 13 I=1,NICP
            CONCP(IS,I,1)=CONCP(IS,I,1)+DELTAX
            CONCP(IS,I,2)=CONCP(IS,I,2)+DELTAY
  13      CONTINUE
        ELSE
          XYZICON(IS,1)=VIEWCEN(1)
          XYZICON(IS,2)=XS
          XYZICON(IS,3)=YS

C Update the icon position
          DO 14 I=1,NVTX
            VCICON(IS,I,1)=VCICON(IS,I,1)+DELTAX
            VCICON(IS,I,2)=VCICON(IS,I,2)+DELTAY
            VCICON(IS,I,3)=VCICON(IS,I,3)+DELTAZ
  14      CONTINUE

          DO 15 I=1,NICP
            CONCP(IS,I,1)=CONCP(IS,I,1)+DELTAX
            CONCP(IS,I,2)=CONCP(IS,I,2)+DELTAY
  15      CONTINUE
        ENDIF

      ELSEIF(IT.EQ.2)THEN
        IF(IVIEW.EQ.1)THEN
          CNWNP(IS,ISA,1)=XS
          CNWNP(IS,ISA,2)=YS
          CNWNP(IS,ISA,3)=VIEWCEN(3)
        ELSEIF(IVIEW.EQ.2)THEN
          CNWNP(IS,ISA,1)=XS
          CNWNP(IS,ISA,2)=VIEWCEN(2)
          CNWNP(IS,ISA,3)=YS
        ELSE
          CNWNP(IS,ISA,1)=VIEWCEN(1)
          CNWNP(IS,ISA,2)=XS
          CNWNP(IS,ISA,3)=YS
        ENDIF
      ENDIF

      CALL NETWDRW
      RETURN
      END

C ********************* GRIDUPDT *********************************
C Called from the c-library and updates the network grid properties
C Zoom ,snap, etc, etc.
      SUBROUTINE GRIDUPDT(IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY)

      INCLUDE 'gnetwk.h'
C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      LOGICAL GON,SON

C Switch on/off the grid spacing.
      IF(IGRIDS.EQ.-1)THEN
          GON=.FALSE.
      ELSEIF(IGRIDS.EQ.1)THEN
          GON=.TRUE.

C Switch ON/OFF the grid snapping.
      ELSEIF(ISNP.EQ.-1)THEN
          SON=.FALSE.
      ELSEIF(ISNP.EQ.1)THEN
          SON=.TRUE.

C Adjust the grid size
      ELSEIF(IGRIDU.EQ.1)THEN
        IF(GRSPC(1).LT.GRMAX(1).AND.GRSPC(1).GE.1.)THEN
          GRSPC(1)=GRSPC(1)+1.0
          GRSPC(2)=GRSPC(2)+1.0
        ELSEIF(GRSPC(1).LT.1.)THEN
          GRSPC(1)=GRSPC(1)*2.0
          GRSPC(2)=GRSPC(2)*2.0
        ELSE
          GRSPC(1)=GRSPC(1)
          GRSPC(2)=GRSPC(2)
        ENDIF
      ELSEIF(IGRIDU.EQ.-1)THEN
        IF(GRSPC(1).GT.1.)THEN
          GRSPC(1)=GRSPC(1)-1.0
          GRSPC(2)=GRSPC(2)-1.0
        ELSEIF(GRSPC(1).LE.1.0.AND.GRSPC(1).GT.0.125)THEN
          GRSPC(1)=GRSPC(1)-0.5*GRSPC(1)
          GRSPC(2)=GRSPC(2)-0.5*GRSPC(2)
        ELSE
          GRSPC(1)=GRSPC(1)
          GRSPC(2)=GRSPC(2)
        ENDIF

C Adjust the zoom status
      ELSEIF(IZOOM.EQ.1)THEN
        IF(SCALF.LT.VIEWMZ)THEN
          SCALF=SCALF+0.25
        ELSE
          SCALF=VIEWMZ
        ENDIF
      ELSEIF(IZOOM.EQ.-1)THEN
        IF(SCALF.GT.VIEWMN)THEN
          SCALF=SCALF-0.25
        ELSE
          SCALF=VIEWMN
        ENDIF

C Pan the display (called by scrollbar routine)
      ELSEIF(IPAN.GT.0)THEN
        SDX=DX*(MGRXE)
        IF(IPAN.EQ.1)THEN
          SDY=-DY*(MGRYE)
        ELSE
          SDY=DY*(VIEWLIM(4)-VIEWLIM(3))
          SDX=DX*(VIEWLIM(2)-VIEWLIM(1))
        ENDIF
        IF(DX.GT.0.)THEN
          IF(VIEWLIM(2)+(SDX).LE.GRMAX(1))THEN
            VIEWCEN(1)=VIEWCEN(1)+(SDX)
          ENDIF
        ELSEIF(DX.LT.0.0)THEN
         IF(VIEWLIM(1)+(SDX).GE.0.)THEN
            VIEWCEN(1)=VIEWCEN(1)+(SDX)
          ENDIF
        ELSEIF(DY.GT.0.0)THEN
          IF(VIEWLIM(4)+(SDY).LE.GRMAX(2))THEN
            VIEWCEN(2)=VIEWCEN(2)+(SDY)
          ENDIF
        ELSEIF(DY.LT.0.0)THEN
          IF(VIEWLIM(3)+(SDY).GE.0.)THEN
            VIEWCEN(2)=VIEWCEN(2)+(SDY)
          ENDIF
        ENDIF
      ENDIF

C Re draw the network
      CALL NETWDRW

      RETURN
      END

C ****************************** GCONAD **********************************
C This code lets the user define a network connection graphically.
C Called from the C-side

      SUBROUTINE GCONAD(IX,IY,IXR,IYR,IB)

      INCLUDE 'gnetwk.h'
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/NWKTYP/INWKTYP,vergnf   
      
C Icon commons

      COMMON/NWKICN/NNICN,ICONTP(MICN),XYZICON(MICN,3),NICONATR(MICN),
     & ATRICN(MICN,MIATRB,3),ATRTAG(MICN,MIATRB,5),ATRMENU(MICN,MIATRB),
     & NCONP(MICN),CONCP(MICN,MCNP,2),ICNCT(MICN,MCNP),
     & VCICON(MICN,MICNV,3),IVEICN(MICN,MICNE,5),NIVC(MICN),
     & NIVE(MICN),NIVD(MICN),IVDOT(MICN,MICND,4),NIVA(MICN),
     & IVARC(MICN,MICND,7),NIVL(MICN),IVLBL(MICN,MICND,3),NIVT(MICN)

      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/NWKICS/NWICNM(MICN),NWICNHIS(MICN),NWICNLBL(MICN,MICND),
     & NWICNTXT(MICN,60)

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN)

C Connection commons
      COMMON/NWKCON/NICNN,ICNS(MNCNN),ICNE(MNCNN),ICNNT(MNCNN),
     & ICNSP(MNCNN),ICNEP(MNCNN),CNWNP(MNCNN,MCIP,3),
     & NCONWP(MNCNN),idatrdom(MNCNN),ddtagatr(MNCNN,MIATRB,5),
     & ddatrib(MNCNN,MIATRB,3),ddmenuatr(MNCNN,MIATRB)

      COMMON/TMPGCA/ICSTRT,ICSTRTP,CIP(MCIP,2),ICEND,IENDP,ICPC

C Signal that there is a new connection which might have attributes
C to edit.
      common/newconn/ihavenewcnn

C Data returned from initial scan of icon db.
      dimension idomain(MNWKTYP),nbcat(MNWKTYP),
     &  cattag(MNWKTYP,MICNCAT),catmenu(MNWKTYP,MICNCAT),
     &  nbicons(MNWKTYP,MICNCAT),icontag(MNWKTYP,MICNCAT,MICN),
     &  iconmenu(MNWKTYP,MICNCAT,MICN)
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      LOGICAL CLOSEX,CLOSEY,ISEL,CSEL,icnisnode,ok
      CHARACTER ATRTAG*12,ATRMENU*32,ATRICN*12
      CHARACTER NWICNM*12,NWICNHIS*40,NWICNLBL*4,ct*12
      character NWICNTXT*72,outs*124,outsd*124,VNPSTR*12
      character ddtagatr*12,ddmenuatr*32,ddatrib*12
      character cattag*12,catmenu*32,icontag*12,iconmenu*40
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12

      CALL PIXEL2U(IX,IY,X,Y)

C Hunt through the components
      INCOMP=0
      DO 10 I=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(X,XYZICON(I,1),1.2*XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,2),1.2*XSIZIC,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(X,XYZICON(I,1),1.2*XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,3),1.2*XSIZIC,CLOSEY)
        ELSE
          CALL ECLOSE(X,XYZICON(I,2),1.2*XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,3),1.2*XSIZIC,CLOSEY)
        ENDIF

        IF(CLOSEX.AND.CLOSEY)THEN
          INCOMP=I
        ENDIF
  10  CONTINUE

      IF(INCOMP.GT.0.AND.IB.EQ.1)THEN

C If inside a component and button == 1 then set the start point.
        ICSTRT=INCOMP
        DMIN=1.0E+20
        DO 13 J=1,NCONP(ICSTRT)
          DISTX=ABS(CONCP(ICSTRT,J,1)-X)
          DISTY=ABS(CONCP(ICSTRT,J,2)-Y)
          IF(((DISTX**2.+DISTY**2.)**0.5).LT.DMIN)THEN
            DMIN=(DISTX**2.+DISTY**2.)**0.5
            ICSTRTP=J
          ENDIF
  13    CONTINUE
        ICPC=0

        CALL U2PIXEL(CONCP(ICSTRT,ICSTRTP,1),
     &               CONCP(ICSTRT,ICSTRTP,2),ISX,ISY)
        IXR=ISX
        IYR=ISY

C Re set the connection start point to unselected
       ELSEIF(INCOMP.EQ.0.AND.IB.EQ.2)THEN

C If !inside a component and button == 2 set an intermediate
C point
        IF(ICPC.LT.MCIP)THEN
          ICPC=ICPC+1
          CALL NETWSNAP(X,Y,XS,YS)
          CIP(ICPC,1)=XS
          CIP(ICPC,2)=YS
          CALL U2PIXEL(CIP(ICPC,1),CIP(ICPC,2),IMX,IMY)
          IXR=IMX
          IYR=IMY
        ENDIF
      ELSEIF(INCOMP.GT.0.AND.IB.EQ.3)THEN

C If inside a component and button == 3 then end connection, update
C connection data and clear the temporary array
        call winscl('-',0)
        ICEND=INCOMP
        DMIN=1E+20
        DO 15 J=1,NCONP(ICEND)
          DISTX=ABS(CONCP(ICEND,J,1)-X)
          DISTY=ABS(CONCP(ICEND,J,2)-Y)
          IF(((DISTX**2+DISTY**2)**0.5).LT.DMIN)THEN
            DMIN=(DISTX**2+DISTY**2)**0.5
            ICENDP=J
          ENDIF
  15    CONTINUE

C Return snapped points to the calling routine.
        CALL U2PIXEL(CONCP(ICEND,ICENDP,1),
     &               CONCP(ICEND,ICENDP,2),IEX,IEY)
        IXR=IEX
        IYR=IEY

C << primitive parts ICPP testing code logic needs checking >>
C << logic should be loacted in a separate subroutine or be >>
C << placed in an external programme >>
C        IF((ICPP(ICSTRT,ICSTRTP,1)(6:6).NE.
C     &      ICPP(ICEND,ICENDP,1)(6:6)).OR.
C     &     (ICPP(ICSTRT,ICSTRTP,1)(6:6).EQ.'0').OR.
C     &     (ICPP(ICEND,ICENDP,1)(6:6).EQ.'0'))THEN
C          continue
C        else

C Upon succesful completion of a connection, set the connection data.
        NICNN=NICNN+1
        ICNS(NICNN)=ICSTRT
        ICNE(NICNN)=ICEND

C See if the type of attachments match, if so set ICNNT and if not
C give a warning to the user (reset to standard colours before giving
C the warning). Tell the user which components have been associated
C with this connection.
        if(ICNCT(ICSTRT,ICSTRTP).eq.ICNCT(ICEND,ICENDP))then
          ICNNT(NICNN)=ICNCT(ICSTRT,ICSTRTP)
          if(icnnt(nicnn).eq.0) ct='none'
          if(icnnt(nicnn).eq.1) ct='air'
          if(icnnt(nicnn).eq.2) ct='water'
          if(icnnt(nicnn).eq.3) ct='steam'
          if(icnnt(nicnn).eq.4) ct='refrigerant'
          if(icnnt(nicnn).eq.5) ct='fuel'
          if(icnnt(nicnn).eq.6) ct='comb_product'
          if(icnnt(nicnn).eq.7) ct='signal'
          lns=lnblnk(NWICNM(ICNS(nicnn)))
          lne=lnblnk(NWICNM(ICNE(nicnn)))
          lhs=lnblnk(NWICNHIS(ICNS(nicnn)))
          lhe=lnblnk(NWICNHIS(ICNE(nicnn)))
          write(outs,'(10a)') 'Between ',NWICNM(ICNS(nicnn))(1:lns),
     &      ' (',NWICNHIS(ICNS(nicnn))(1:lhs),') & ',
     &      NWICNM(ICNE(nicnn))(1:lne),
     &      ' (',NWICNHIS(ICNE(nicnn))(1:lhe),') via ',ct
          call sdelim(outs,outsd,'S',IW)
          call edisp(iuout,outsd)

C See comments in network.F in NETCDATR regarding height differences.
          if(icnisnode(ICNS(nicnn)))then
            delta=XYZICON(ICNE(nicnn),3)-XYZICON(ICNS(nicnn),3)
          else
            delta=XYZICON(ICNS(nicnn),3)-XYZICON(ICNE(nicnn),3)
          endif
          write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &      NWICNM(ICNS(nicnn))(1:lns),' is',XYZICON(ICNS(nicnn),3),
     &      ' & ',NWICNM(ICNE(nicnn))(1:lne),' is ',
     &      XYZICON(ICNE(nicnn),3),' & delta is ',delta
          call sdelim(outs,outsd,'S',IW)
          call edisp(iuout,outsd)

C Assign default attributes for this connection based on the current
C domain. Rescan the iconsdb to pick up each domain's connection
C attributes.
          call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
     &      catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)

C Loop through and find the relevant domain id to which to attribute the
C connections data.
C Debug.. 
C          write(6,*) 'Added a connection ...'
C          write(6,*) 'number of domains in db ',nbdomain
          do 42 id=1,nbdomain

C Debug..
C            write(6,*) 'network domain type ',inwktyp
C            write(6,*) 'domain ids ',idomain(id)
            if(idomain(id).eq.inwktyp)then
C Debug..
C              write(6,*)'number of data attributes ',iatrdom(id)
              idatrdom(NICNN)=iatrdom(id)
              if(iatrdom(id).gt.0)then
                do 43 ii=1,iatrdom(id)
                  ddtagatr(NICNN,ii,1)=dtagatr(id,ii,1)
                  ddtagatr(NICNN,ii,2)=dtagatr(id,ii,2)
                  ddtagatr(NICNN,ii,3)=dtagatr(id,ii,3)
                  ddtagatr(NICNN,ii,4)=dtagatr(id,ii,4)
                  ddtagatr(NICNN,ii,5)=dtagatr(id,ii,5)

C If dtagatr(id,ii,1) is location then datrib(id,ii,1) is delta
                  if(dtagatr(id,ii,1)(1:8).eq.'location')then
                    call relstr(delta,vnpstr,ilen,iflag)
                    write(ddatrib(nicnn,ii,1),'(a)') vnpstr
                  else
                    ddatrib(NICNN,ii,1)=datrib(id,ii,1)
                  endif
                  ddatrib(NICNN,ii,2)=datrib(id,ii,2)
                  ddatrib(NICNN,ii,3)=datrib(id,ii,3)
                  ddmenuatr(NICNN,ii)=dmenuatr(id,ii)
C Debug...
C                  write(6,*) 'data tag ',dtagatr(id,ii,1)
C                  write(6,*) 'data atr ',datrib(id,ii,1)
C                  write(6,*) 'data tag ',dtagatr(id,ii,2)
C                  write(6,*) 'data atr ',datrib(id,ii,2)
C                  write(6,*) 'data tag ',dtagatr(id,ii,3)
C                  write(6,*) 'data tag ',dtagatr(id,ii,4)
C                  write(6,*) 'data tag ',dtagatr(id,ii,5)
C                  write(6,*) 'data atr ',datrib(id,ii,3)
C                  write(6,*) 'menu atr ',dmenuatr(id,ii)
  43            continue
              endif
            endif
  42      continue
	else
	  write(outs,'(a,i2,a,i2,a)') 'Attachement types for icons',
     &      ICSTRT,' & ',ICEND,' do not match! Setting to zero.'
          call winscl('-',0)
          call usrmsg(outs,'Using type zero.','W')
	  ICNNT(NICNN)=0
	endif
        ICNSP(NICNN)=ICSTRTP
        ICNEP(NICNN)=ICENDP
        NCONWP(NICNN)=ICPC
        IF(NCONWP(NICNN).LT.1) THEN
          NCONWP(NICNN)=1
          IF(IVIEW.EQ.1)THEN 
            CIP(1,1)=0.5*(XYZICON(ICSTRT,1)+XYZICON(ICEND,1))
            CIP(1,2)=0.5*(XYZICON(ICSTRT,2)+XYZICON(ICEND,2))
          ELSEIF(IVIEW.EQ.2)THEN  
            CIP(1,1)=0.5*(XYZICON(ICSTRT,1)+XYZICON(ICEND,1))
            CIP(1,2)=0.5*(XYZICON(ICSTRT,3)+XYZICON(ICEND,3))
          ELSE   
            CIP(1,1)=0.5*(XYZICON(ICSTRT,2)+XYZICON(ICEND,2))
            CIP(1,2)=0.5*(XYZICON(ICSTRT,3)+XYZICON(ICEND,3)) 
          ENDIF
        ENDIF  
        DO 20 K=1,NCONWP(NICNN) 
          IF(IVIEW.EQ.1)THEN
            CNWNP(NICNN,K,1)= CIP(K,1)
            CNWNP(NICNN,K,2)= CIP(K,2)
            CNWNP(NICNN,K,3)= VIEWCEN(3)
          ELSEIF(IVIEW.EQ.2)THEN
            CNWNP(NICNN,K,1)= CIP(K,1)
            CNWNP(NICNN,K,2)= VIEWCEN(2)
            CNWNP(NICNN,K,3)= CIP(K,2)
          ELSE
            CNWNP(NICNN,K,1)= VIEWCEN(1)
            CNWNP(NICNN,K,2)= CIP(K,1)
            CNWNP(NICNN,K,3)= CIP(K,2)
          ENDIF
 20     CONTINUE

C Re set the start and end components to unselected.
        ISEL(ICSTRT)=.FALSE.
        ISEL(ICEND)=.FALSE.

C Clear the temporary array of connection variables.
        ICSTRT=0
        ICSTRTP=0
        DO 30 I=1,MCIP
          CIP(I,1)=0
          CIP(I,2)=0
  30    CONTINUE
        ICEND=0
        IENDP=0
        ICPC=0

C Note a new connection has been created so interface can ask if
C the user wishes to edit attributes.
        ihavenewcnn=nicnn

C Debug..
C        write(6,*) 'info ',nicnn,ICNSP(NICNN),ICNEP(NICNN),
C     &     NCONWP(NICNN),ihavenewcnn
      ELSE

C Clear the temporary array of connection variables if a complete connection
C has not been made.
        IXR=IX
        IYR=IY
        ICSTRT=0
        ICSTRTP=0
        DO 40 I=1,MCIP
          CIP(I,1)=0
          CIP(I,2)=0
  40    CONTINUE
        ICEND=0
        IENDP=0
        ICPC=0
      ENDIF

      RETURN
      END

C ******************* GNWKQUERY *********************
C Called by the c-side to get the current network viewing parameters
C
C IQ - query identifier
C PV - vertical view extents
C TV - vertical view limits
C CV - current vert. view centre
C
C PV - horizontal view extents
C TV - horizontal view limits
C CV - current horiz. view centre

      SUBROUTINE GNWKQUERY(IQ,PV,TV,CV,PH,TH,CH)

      INCLUDE 'gnetwk.h'
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      IQ=-1
      IF(IVIEW.EQ.1)THEN
C Standard X-Y view
        TH=FLOAT(MGRXL)
        PH=(VIEWLIM(2)-VIEWLIM(1))/TH
        CH=VIEWCEN(1)/TH

        TV=FLOAT(MGRYL)
        PV=(VIEWLIM(4)-VIEWLIM(3))/TV
        CV=VIEWCEN(2)/TV
        IQ=1
      ELSEIF(IVIEW.EQ.2)THEN
C X-Z view
        TH=FLOAT(MGRXL)
        PH=(VIEWLIM(2)-VIEWLIM(1))/TH
        CH=VIEWCEN(1)/TH

        TV=FLOAT(MGRZL)
        PV=(VIEWLIM(6)-VIEWLIM(5))/TV
        CV=VIEWCEN(3)/TV
        IQ=1
      ELSEIF(IVIEW.EQ.3)THEN
        TH=FLOAT(MGRYL)
        PH=(VIEWLIM(4)-VIEWLIM(3))/TH
        CH=VIEWCEN(2)/TV

        TV=FLOAT(MGRZL)
        PV=(VIEWLIM(6)-VIEWLIM(5))/TV
        CV=VIEWCEN(3)/TV
        IQ=1
      ENDIF

      RETURN
      END

C ************************ NETWSNAP *************************
C This routine calculates the position of grid point co-ordinates
C when grid snappping is on

      SUBROUTINE NETWSNAP(X,Y,XS,YS)

      INCLUDE 'gnetwk.h'
C Grid commons
      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      COMMON/NWKGRDL/GON,SON

      LOGICAL SON,GON

C If grid is on - then snap to nearest point.
      IF(SON.AND.GRSPC(1).GT.0)THEN
        IF(IVIEW.EQ.1)THEN
          REMX=AMOD(X,GRSPC(1))
          REMY=AMOD(Y,GRSPC(2))
          GRSPC1=GRSPC(1)
          GRSPC2=GRSPC(2)
        ELSEIF(IVIEW.EQ.2)THEN
          REMX=AMOD(X,GRSPC(1))
          REMY=AMOD(Y,GRSPC(3))
          GRSPC1=GRSPC(1)
          GRSPC2=GRSPC(3)
        ELSE
          REMX=AMOD(X,GRSPC(2))
          REMY=AMOD(Y,GRSPC(3))
          GRSPC1=GRSPC(2)
          GRSPC2=GRSPC(3)
        ENDIF

        IF(REMX.GT.GRSPC1/2.)THEN
          XS=X+(GRSPC1-REMX)
        ELSE
          XS=X-REMX
        ENDIF

        IF(REMY.GT.GRSPC2/2.)THEN
          YS=Y+(GRSPC2-REMY)
        ELSE
          YS=Y-REMY
        ENDIF
      ELSE

C If not use the returned co-ordinates
        XS=X
        YS=Y
      ENDIF

C End grid snapping routine
      RETURN
      END
