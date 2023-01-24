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


C ******************** NWKSLCTC ********************
C Called from the c-side to check if an object has been selected
C IT is toggle of status for icon (1) or connection (2).
C IS is the index of the icon.
C ISA is the index of the connection waypoint.
C ISELECT is a flag passed to the routine which determines whether a 
C component can be selected (ISELECT=1)
C IACTIVE is returned if ISELECT=0 but the component is already marked
C as selected.

      SUBROUTINE NWKSLCTC(IX,IY,IT,IS,ISA,ISELECT,IACTIVE)
#include "gnetwk.h"

C Icon commons
C Selected entity (component and connection) common
      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW
      
      LOGICAL ISEL,CSEL,CLOSEX,CLOSEY

#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

C Check if the pixels IX and IY passed from C match are close
C to a component or connection (+/-) 0.4m on the grid scale
C element.
      iix=ix; iiy=iy
      CALL PIXEL2U(IIX,IIY,GX,GY)

      CGX=GX; CGY=GY

C Reset all values prior to hunt.
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.
      IS=0
      ISA=0
      IT=0
      IACTIVE=0

C Hunt through the components.
      DO 10 I=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(CGX,XYZICON(I,1),0.4,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,2),0.4,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(CGX,XYZICON(I,1),0.4,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,3),0.4,CLOSEY)
        ELSE
          CALL ECLOSE(CGX,XYZICON(I,2),0.4,CLOSEX)
          CALL ECLOSE(CGY,XYZICON(I,3),0.4,CLOSEY)
        ENDIF
        IF(CLOSEX.AND.CLOSEY)THEN

C If select flag is on then allow selection or de-selection of the component.
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

C Otherwise just return the number of the selected component.
          ELSE
            IF(ISEL(I))IACTIVE=1
            IS=I
            ISA=0
            IT=1
          ENDIF
        ENDIF
  10  CONTINUE


C Reset the search parameters.
      CLOSEX=.FALSE.
      CLOSEY=.FALSE.

      DO 20 I=1,NICNN

C Loop through each connection waypoint looking for one within 0.2m.
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
              IF(CSEL(I)) IACTIVE=1
              IS=I
              ISA=J
              IT=2              
            ENDIF
          ENDIF
   23   CONTINUE
   20 CONTINUE

C Re-draw the network.
      CALL NETWDRW

      RETURN
      END

C ********************* NWKUPDTPOS *********************************
C NWKUPDTPOS is called from the c-library and updates component and connection
C intermediate point positions.
      SUBROUTINE NWKUPDTPOS(IS,ISA,IT,IX,IY)

#include "gnetwk.h"
C Icon commons
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

      iix=ix; iiy=iy
      CALL PIXEL2U(IIX,IIY,X,Y)
      CALL NETWSNAP(X,Y,XS,YS)

C Store old icon position.
      OLDPOSX=XYZICON(IS,1)
      OLDPOSY=XYZICON(IS,2)
      OLDPOSZ=XYZICON(IS,3)
      DELTAX=0.0
      DELTAY=0.0
      DELTAZ=0.0
      NVTX=NIVC(IS)
      NICP=NCONP(IS)

C Move the  icon.
      IF(IT.EQ.1)THEN
        IF(IVIEW.EQ.1)THEN
          XYZICON(IS,1)=XS
          XYZICON(IS,2)=YS
          XYZICON(IS,3)=VIEWCEN(3)
          DELTAX=XYZICON(IS,1)-OLDPOSX
          DELTAY=XYZICON(IS,2)-OLDPOSY
          DELTAZ=XYZICON(IS,3)-OLDPOSZ

C Update the icon position.
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

C Update the icon position.
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

C Update the icon position.
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
C Zoom ,snap, etc.

      SUBROUTINE GRIDUPDT(IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY)
#include "gnetwk.h"

      COMMON/NWKGRD/GRMAX(3),GRSPC(3),GRLYRH(MLYRS)
      COMMON/NWKGRDL/GON,SON
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      LOGICAL GON,SON

C Note: the integers were defined as long-int in the C code
C so might need #ifdef OSI for this to work correctly.

C Debug.
C      write(6,*) IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY

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

C Adjust the grid size positive IGRIDU increases.
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

C Adjust the zoom status. Ensure it is not beyond limits.
      ELSEIF(IZOOM.EQ.1)THEN
        IF(SCALF.LT.VIEWMZ)THEN
          SCALF=SCALF+0.2  ! increase the zoom by 0.2
        ELSE
          SCALF=VIEWMZ
        ENDIF
      ELSEIF(IZOOM.EQ.-1)THEN
        IF(SCALF.GT.VIEWMN)THEN
          SCALF=SCALF-0.2  ! decrease the zoom by 0.2
        ELSE
          SCALF=VIEWMN
        ENDIF

C If the C scrollbar calling code had set pan to a positive number then 
C pan the display. ipan = 1 when user clicked at the extents of the
C scrollbar and ipan = 2 if they were dragging the bar along. DX is
C positive if user clicked to the right, negative if clicked to the
C left.
      ELSEIF(IPAN.GT.0)THEN
        IF(IPAN.EQ.1)THEN
          SDX=DX*(MGRXE)
          SDY=-DY*(MGRYE)  ! ? negate for the Y
        ELSE
          SDX=DX*(VIEWLIM(2)-VIEWLIM(1))
          SDY=DY*(VIEWLIM(4)-VIEWLIM(3))
        ENDIF
        IF(DX.GT.0.0)THEN
          IF(VIEWLIM(2)+(SDX).LE.GRMAX(1))THEN
            VIEWCEN(1)=VIEWCEN(1)+(SDX)  ! increment right
          ENDIF
        ELSEIF(DX.LT.0.0)THEN
         IF(VIEWLIM(1)+(SDX).GE.-5.0)THEN
            VIEWCEN(1)=VIEWCEN(1)+(SDX)
          ENDIF
        ELSEIF(DY.GT.0.0)THEN
          IF(VIEWLIM(4)+(SDY).LE.GRMAX(2))THEN
            VIEWCEN(2)=VIEWCEN(2)+(SDY)
          ENDIF
        ELSEIF(DY.LT.0.0)THEN
          IF(VIEWLIM(3)+(SDY).GE.-5.0)THEN
            VIEWCEN(2)=VIEWCEN(2)+(SDY)
          ENDIF
        ENDIF
      ENDIF

C Re draw the network
      CALL NETWDRW

      RETURN
      END

C ****************************** GCONAD **********************************
C GCONAD lets the user define a network connection graphically.
C Called from the C-side

      SUBROUTINE GCONAD(IX,IY,IXR,IYR,IB)

#include "gnetwk.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer ix,iy,ixr,iyr,ib

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/NWKTYP/INWKTYP,vergnf   
      
C Icon commons
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)

C This common block holds information gathered between (frequent) calls
C from the C code.
      COMMON/TMPGCA/ICSTRT,ICSTRTP,CIP(MCIP,2),ICEND,ICENDP,ICPC

C Signal that there is a new connection which might have attributes
C to edit.
      common/newconn/ihavenewcnn

C Data (templates) returned from initial scan of icon db.
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      LOGICAL CLOSEX,CLOSEY,CLOSEAX,CLOSEAY,ISEL,CSEL,icnisnode
      CHARACTER ct*12
      character outs*124,outsd*124,VNPSTR*12
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      integer ICSTRT,ICEND,INCOMP

#ifdef OSI
      integer iicol,iisx,iisy,iix,iiy,iimx,iimy,iiex,iiey
#else
      integer*8 iicol,iisx,iisy,iix,iiy,iimx,iimy,iiex,iiey
#endif

      iix=ix; iiy=iy
      CALL PIXEL2U(IIX,IIY,X,Y)

C Hunt through the components. Look for a radius of XSIZIC
C (0.75m) of the centre of each icon. Note, as the mouse is
C moved this subroutine is called again and again. If the mouse
C is not near a component then INCOMP is zero, if it passes
C close to a component then it becomes non-zero. In cluttered
C networks this can lead to the initial point getting lost.
      INCOMP=0; CLOSEX=.FALSE.; CLOSEY=.FALSE.
      DO 10 I=1,NNICN
        IF(IVIEW.EQ.1)THEN
          CALL ECLOSE(X,XYZICON(I,1),XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,2),XSIZIC,CLOSEY)
        ELSEIF(IVIEW.EQ.2)THEN
          CALL ECLOSE(X,XYZICON(I,1),XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,3),XSIZIC,CLOSEY)
        ELSE
          CALL ECLOSE(X,XYZICON(I,2),XSIZIC,CLOSEX)
          CALL ECLOSE(Y,XYZICON(I,3),XSIZIC,CLOSEY)
        ENDIF

        IF(CLOSEX.AND.CLOSEY)THEN
          INCOMP=I
        ENDIF
  10  CONTINUE

C Debug.
C      write(6,*)'INCOMP IB ',INCOMP,IB,ICSTRT,ICSTRTP,ICEND,ICENDP,ICPC

      IF(INCOMP.GT.0.AND.IB.EQ.1)THEN

C If inside a component and within 0.25m of an attachment point
C and button == 1 then set the start point and attachment point.
        ICSTRT=INCOMP  ! remember the start component
        CLOSEAX=.FALSE.; CLOSEAY=.FALSE.; ICSTRTP=0
        DO 13 J=1,NCONP(ICSTRT)
          CALL ECLOSE(X,CONCP(ICSTRT,J,1),0.25,CLOSEAX)
          CALL ECLOSE(Y,CONCP(ICSTRT,J,2),0.25,CLOSEAY)
          IF(CLOSEAX.AND.CLOSEAY)THEN
            ICSTRTP=J  ! remember the start attachment point
          ENDIF
  13    CONTINUE
        if(ICSTRTP.eq.0) ICSTRTP=1  ! if did not find set to initial attachment
        ICPC=0   ! reset counter for way-points

        CALL U2PIXEL(CONCP(ICSTRT,ICSTRTP,1),
     &               CONCP(ICSTRT,ICSTRTP,2),iisx,iisy)
        IXR=int(iisx)
        IYR=int(iisy)

C Re set the connection start point to unselected
       ELSEIF(INCOMP.EQ.0.AND.IB.EQ.2)THEN

C If !inside a component and button == 2 set an way-point.
C There can be up to MCIP way-points.
        IF(ICPC.LT.MCIP)THEN
          ICPC=ICPC+1
          CALL NETWSNAP(X,Y,XS,YS)
          CIP(ICPC,1)=XS
          CIP(ICPC,2)=YS
C Debug.
C         write(6,*) 'waypoint ',icpc,XS,YS
          CALL U2PIXEL(CIP(ICPC,1),CIP(ICPC,2),iimx,iimy)
          IXR=int(iimx)
          IYR=int(iimy)
        ENDIF
      ELSEIF(INCOMP.GT.0.AND.IB.EQ.3)THEN

C If inside a component and button == 3 then end connection, update
C connection data and clear the temporary array.
        iicol=0
        call winscl('-',iicol)
        ICEND=INCOMP  ! remember the end component
        CLOSEAX=.FALSE.; CLOSEAY=.FALSE.
        DO 15 J=1,NCONP(ICEND)
          CALL ECLOSE(X,CONCP(ICEND,J,1),0.25,CLOSEAX)
          CALL ECLOSE(Y,CONCP(ICEND,J,2),0.25,CLOSEAY)
          IF(CLOSEAX.AND.CLOSEAY)THEN
            ICENDP=J  ! remember the end connection point
          ENDIF
  15    CONTINUE
        if(ICENDP.eq.0) ICENDP=1  ! if did not find set to initial attachment

C Return snapped points to the calling routine.
        CALL U2PIXEL(CONCP(ICEND,ICENDP,1),
     &               CONCP(ICEND,ICENDP,2),IIEX,IIEY)
        IXR=int(IIEX)
        IYR=int(IIEY)

C Test if both start and end components have been identified.
C If not reset data and return.
        if(icstrt.eq.0.or.icend.eq.0)then
          write(outs,'(a,2i3,a)') 'Both ends ',icstrt,icend,
     &      ' not defined so giving up'
          call edisp(iuout,outs)
          IXR=int(IX)
          IYR=int(IY)
          ICSTRT=0
          ICSTRTP=0
          DO 44 I=1,MCIP
            CIP(I,1)=0
            CIP(I,2)=0
  44      CONTINUE
          ICEND=0; ICENDP=0; ICPC=0
          return
        endif

C Upon succesful completion of a connection, set the connection data.
        iicol=0
        call winscl('-',iicol)  ! reset color again
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
C          call scanicondb(nbdomain,verdomain,idomain,nbcat,cattag,
C     &      catmenu,nbicons,icontag,iconmenu,iatrdom,dtagatr,
C     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)
          call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &      datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)

C Loop through and find the relevant domain id to which to attribute the
C connections data.

C Debug.
C          write(6,*) 'Added a connection ...'
C          write(6,*) 'number of domains in db ',nbdomain

          do 42 id=1,nbdomain

C Debug.
C           write(6,*) 'network domain type ',inwktyp
C           write(6,*) 'domain ids ',idomain(id)

            if(idomain(id).eq.inwktyp)then

C Debug.
C           write(6,*)'number of data attributes ',iatrdom(id)

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
                    call relstr(delta,vnpstr,ilena,iflag)
                    write(ddatrib(nicnn,ii,1),'(a)') vnpstr
                  else
                    ddatrib(NICNN,ii,1)=datrib(id,ii,1)
                  endif
                  ddatrib(NICNN,ii,2)=datrib(id,ii,2)
                  ddatrib(NICNN,ii,3)=datrib(id,ii,3)
                  ddmenuatr(NICNN,ii)=dmenuatr(id,ii)

C Debug.
C                  write(6,*) 'nicnn id ii ',nicnn,id,ii
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

C Seems the selected points were not clearly identified. Warn user
          write(outs,'(a,i2,a,i2,a)') 'Attachement types for icons',
     &      ICSTRT,' & ',ICEND,' do not match!'
          call usrmsg(outs,
     &      'Marking as type zero. Probably best to delete it.','W')
          ICNNT(NICNN)=0
        endif

        ICNSP(NICNN)=ICSTRTP  ! remember index of attachment points
        ICNEP(NICNN)=ICENDP
        NCONWP(NICNN)=ICPC    ! remember how many way points
        IF(NCONWP(NICNN).LT.1) THEN

C If there are no way points then create one half way between the
C two icons.
C << improve this: via call to crowxyz()
C << if distance is < ?? then create one way point ?? point on line method?
C << if distance is > ?? then create two way points ? ratio method?
C << revise interface to allow way point X to be reported/edited
C << important to have the Z along the line as well.
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
          CIP(I,1)=0.0
          CIP(I,2)=0.0
  30    CONTINUE
        ICEND=0
        ICENDP=0
        ICPC=0

C Note a new connection has been created so interface can ask if
C the user wishes to edit attributes.
        ihavenewcnn=nicnn

C Debug.
C        write(6,*) 'info ',nicnn,ICNSP(NICNN),ICNEP(NICNN),
C     &     NCONWP(NICNN),ihavenewcnn

      ELSE

C Clear the temporary array of connection variables if a complete connection
C has not been made.
        IXR=int(IX)
        IYR=int(IY)
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

C ****************************** MCONAD **********************************
C MCONAD lets the user define a network connection via menu.

      SUBROUTINE MCONAD(is,ie)

#include "gnetwk.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/NWKTYP/INWKTYP,vergnf   
      
C Icon commons
      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      COMMON/NWKSEL/ISEL(MICN),CSEL(MNCNN)

C icontoflow() is a pointer from icon to flow node (positive) or flow
C component (negative).
C icnisnode(MNCNN) toggle where .true. means icon index is
C associated with a flow node.
C inodetoicon() points from flow node to icon
      common/NWKFLW/icontoflow(MNCNN),icnisnode(MNCNN),
     &  inodetoicon(MNCNN),icomptoicon(MNCNN)

C This common block holds information gathered between (frequent) calls
C from the C code.
      COMMON/TMPGCA/ICSTRT,ICSTRTP,CIP(MCIP,2),ICEND,ICENDP,ICPC

C Signal that there is a new connection which might have attributes
C to edit.
      common/newconn/ihavenewcnn

C Data returned from initial scan of icon db.
      dimension iatrdom(MNWKTYP),dtagatr(MNWKTYP,MIATRB,5),
     &  datrib(MNWKTYP,MIATRB,3),dmenuatr(MNWKTYP,MIATRB)
      dimension igatrdom(MNWKTYP),dgtagatr(MNWKTYP,MIATRB,5),
     &  dgatrib(MNWKTYP,MIATRB,3),dgmenuatr(MNWKTYP,MIATRB)

      LOGICAL ISEL,CSEL,icnisnode
      CHARACTER ct*12
      character outs*124,outsd*124,VNPSTR*12
      character dtagatr*12,dmenuatr*32,datrib*12
      character dgtagatr*12,dgmenuatr*32,dgatrib*12
      integer ICSTRT,ICEND,INCOMP

#ifdef OSI
      integer iicol
#else
      integer*8 iicol
#endif

C Have to re-think. A single mass flow connection is actually
C two connections in enet. Do node to component and then do
C component to 2nd node creating two enet connections.
      ICSTRT=is; ICEND=ie
      ICSTRTP=1  ! set to initial attachment
      ICPC=0     ! reset counter for way-points
      ICENDP=1   ! end  attachment

C Set the connection data for the 1st half.
      iicol=0
      call winscl('-',iicol)  ! reset color again
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
        lns=lnblnk(NWICNM(ICSTRT))
        lne=lnblnk(NWICNM(ICEND))
        lhs=lnblnk(NWICNHIS(ICSTRT))
        lhe=lnblnk(NWICNHIS(ICEND))
        write(outs,'(10a)') 'Between ',NWICNM(ICSTRT)(1:lns),
     &    ' (',NWICNHIS(ICSTRT)(1:lhs),') & ',
     &    NWICNM(ICEND)(1:lne),
     &    ' (',NWICNHIS(ICEND)(1:lhe),') via ',ct
        call sdelim(outs,outsd,'S',IW)
        call edisp(iuout,outsd)

C See comments in network.F in NETCDATR regarding height differences.
        if(icnisnode(ICSTRT))then
          delta=XYZICON(ICEND,3)-XYZICON(ICSTRT,3)
        else
          delta=XYZICON(ICSTRT,3)-XYZICON(ICEND,3)
        endif
        write(outs,'(3a,f6.3,3a,f6.3,a,f6.3)') 'Height of ',
     &    NWICNM(ICSTRT)(1:lns),' is',XYZICON(ICSTRT,3),
     &    ' & ',NWICNM(ICNE(nicnn))(1:lne),' is ',
     &    XYZICON(ICEND,3),' & delta is ',delta
        call sdelim(outs,outsd,'S',IW)
        call edisp(iuout,outsd)

C Assign default attributes for this connection based on the current
C domain. Rescan the iconsdb to pick up each domain's connection
C attributes.
        call scanicondb(nbdomain,verdomain,iatrdom,dtagatr,
     &    datrib,dmenuatr,igatrdom,dgtagatr,dgatrib,dgmenuatr,IER)

C Loop through and find the relevant domain id to which to attribute the
C connections data.

C Debug. 
C        write(6,*) 'Added a connection ...'
C        write(6,*) 'number of domains in db ',nbdomain

        do 42 id=1,nbdomain

C Debug.
C          write(6,*) 'network domain type ',inwktyp
C          write(6,*) 'domain ids ',idomain(id)

          if(idomain(id).eq.inwktyp)then

C Debug.
C           write(6,*)'number of data attributes ',iatrdom(id)

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
                  call relstr(delta,vnpstr,ilena,iflag)
                  write(ddatrib(nicnn,ii,1),'(a)') vnpstr
                else
                  ddatrib(NICNN,ii,1)=datrib(id,ii,1)
                endif
                ddatrib(NICNN,ii,2)=datrib(id,ii,2)
                ddatrib(NICNN,ii,3)=datrib(id,ii,3)
                ddmenuatr(NICNN,ii)=dmenuatr(id,ii)

C Debug.
C                write(6,*) 'nicnn id ii ',nicnn,id,ii
C                write(6,*) 'data tag ',dtagatr(id,ii,1)
C                write(6,*) 'data atr ',datrib(id,ii,1)
C                write(6,*) 'ddata atr ',ddatrib(nicnn,ii,1)
C                write(6,*) 'data tag ',dtagatr(id,ii,2)
C                write(6,*) 'data atr ',datrib(id,ii,2)
C                write(6,*) 'data tag ',dtagatr(id,ii,3)
C                write(6,*) 'data tag ',dtagatr(id,ii,4)
C                write(6,*) 'data tag ',dtagatr(id,ii,5)
C                write(6,*) 'data atr ',datrib(id,ii,3)
C                write(6,*) 'menu atr ',dmenuatr(id,ii)
  43          continue
            endif
          endif
  42    continue
      endif

      ICNSP(NICNN)=ICSTRTP  ! remember index of attachment points
      ICNEP(NICNN)=ICENDP
      NCONWP(NICNN)=ICPC    ! remember how many way points
      IF(NCONWP(NICNN).LT.1) THEN

C If there are no way points then create one half way between the
C two icons.
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
 20   CONTINUE

C Re set the start and end components to unselected.
      ISEL(ICSTRT)=.FALSE.
      ISEL(ICEND)=.FALSE.

C Clear the temporary array of connection variables.
      ICSTRT=0
      ICSTRTP=0
      DO 30 I=1,MCIP
        CIP(I,1)=0.0
        CIP(I,2)=0.0
  30  CONTINUE
      ICEND=0
      ICENDP=0
      ICPC=0

C Note a new connection has been created so interface can ask if
C the user wishes to edit attributes.
      ihavenewcnn=nicnn

C Debug.
C      write(6,*) 'info ',nicnn,ICNSP(NICNN),ICNEP(NICNN),
C     &   NCONWP(NICNN),ihavenewcnn

      RETURN
      END


C ******************* GNWKQUERY *********************
C Called by the c-side to get the current network viewing parameters.
C IQ - query identifier
C PV - vertical view extents
C TV - vertical view limits
C CV - current vert. view centre
C
C PH - horizontal view extents
C TH - horizontal view limits
C CH - current horiz. view centre

      SUBROUTINE GNWKQUERY(IQ,PV,TV,CV,PH,TH,CH)
#include "gnetwk.h"

      COMMON/NWKVEW/SCALF,VIEWCEN(3),VIEWLIM(6),IVIEW

      IQ=-1
      IF(IVIEW.EQ.1)THEN

C Standard X-Y view. Assume maximum limits MGRXL MGRYL of the
C network domain so scroll bar use.
        TH=FLOAT(MGRXL)
        PH=(VIEWLIM(2)-VIEWLIM(1))/TH
        CH=VIEWCEN(1)/TH

        TV=FLOAT(MGRYL)
        PV=(VIEWLIM(4)-VIEWLIM(3))/TV
        CV=VIEWCEN(2)/TV
        IQ=1
      ELSEIF(IVIEW.EQ.2)THEN

C X-Z view.
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

C Debug.
C      write(6,*) 'gnwkquery ',IQ,PV,TV,CV,PH,TH,CH

      RETURN
      END


C ************************ NETWSNAP *************************
C Calculates the position of grid point co-ordinates
C when grid snappping is on.

      SUBROUTINE NETWSNAP(X,Y,XS,YS)
#include "gnetwk.h"

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

