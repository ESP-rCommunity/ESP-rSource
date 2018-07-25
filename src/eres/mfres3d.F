C Copy of several subroutines from mfprb3.F supporting the drawing
C of 3D flow networks.

C  mfwfdraw - draws 3D flow network overlay on wireframe from commons passed colour.

C ************************* MFWFDRAW 
C MFWFDRAW Fluid flow file: draw common block contents.
C colour is either 'r' red or 'g' grey50 for components.
      SUBROUTINE MFWFDRAW(col)

#include "net_flow.h"
#include "net_flow_data.h"
#include "building.h"
      
C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer net3ddelay  ! delay between timesteps in 3D network
      common/delay3d/net3ddelay
      character col*1
      CHARACTER name*12
      integer POS,NEG,CMPIS
      real xc,yc,zc,xw,yw,zw
      integer idms

      if(MMOD.lt.8) return  ! in text mode do not draw
      if(net3ddelay.eq.1)idms=40
      if(net3ddelay.eq.2)idms=20
      if(net3ddelay.eq.3)idms=10
      if(net3ddelay.eq.4)idms=5

C Node data.
      IF(NNOD.gt.0)then
        DO INOD=1,NNOD
          name=NDNAM(INOD)
          xw=HNOD(INOD,1);yw=HNOD(INOD,2); zw=HNOD(INOD,3)
          if(NDTYP(INOD).eq.0)then
            call symbol_flow_nd(name,'iu',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.1)then
            call symbol_flow_nd(name,'ik',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.2)then
            call symbol_flow_nd(name,'bk',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.3)then
            call symbol_flow_nd(name,'bw',col,xw,yw,zw)
            call pausems(idms)
          endif
        ENDDO
      endif

C Component data.
  
C << CONSIDER NWPCMP()  >>

      IF(NCMP.gt.0)then
        DO ICMP=1,NCMP
          name=CMNAM(ICMP)
          xc=HCMP(ICMP,1,1);yc=HCMP(ICMP,1,2); zc=HCMP(ICMP,1,3)

          IF(ITPCMP(ICMP).EQ. 10) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 11) THEN
            continue  ! Self regulating vent
          ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Common orifice flow
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Laminar pipe vol
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.110) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Specific air flow
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.120) THEN
            call symbol_flow_cmp(name,'cr',col,xc,yc,zc) ! Specific air flow crack 
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.130) THEN
            call symbol_flow_cmp(name,'bi',col,xc,yc,zc) ! Specific air flow door
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.210) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! General flow conduit
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.211) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Cowls and roof outlets (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.220) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 3 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.230) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 3 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.240) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 4 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.250) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 4 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.310) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow inducer (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.410) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow corrector (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.420) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Corrector polynomial (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.460) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Fixed flow rates controller (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.500) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! multi-configuration (box)
            call pausems(idms)
          endif
        ENDDO
      endif

C Connections drawn as dotted lines from node -> component -> node.
  
C << CONSIDER NWPCMP()  >>

      IF(NCNN.gt.0)then
        DO ICNN=1,NCNN
          POS=NODPS(ICNN); NEG=NODNE(ICNN)
          CMPIS=ITPCON(ICNN)
          xc=HCMP(CMPIS,1,1); yc=HCMP(CMPIS,1,2); zc=HCMP(CMPIS,1,3)
          call draw_flow_link(HNOD(POS,1),HNOD(POS,2),HNOD(POS,3),
     &       xc,yc,zc)
          call draw_flow_link(xc,yc,zc,
     &       HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
        ENDDO
      endif

  100 RETURN

      END


C ********* symbol_flow_nd
C Draw a symbol for a flow node with optional name and type.
C type='iu' internal unknown, 'ik' internal known
C type='bw' boundary wind, 'bk' boundary known

      subroutine symbol_flow_nd(name,type,col,xw,yw,zw)
#include "building.h"
#include "prj3dv.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER name*12,type*2,col*1
      real xw,yw,zw

      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      DIMENSION  COG1(3),COG2(3),COG3(3)
      character t13*13
#ifdef OSI
      integer iix,iiy,iid4,iicol
#else
      integer*8 iix,iiy,iid4,iicol
#endif

      if(MMOD.lt.8) return  ! in text mode do not draw

C Get pixel position to draw at.
      COG1(1)=xw; COG1(2)=yw; COG1(3)=zw
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix,iiy)

C Draw circles depending on type. (powderblue is 27) (navy blue is 34)
      if(col.eq.'r') iicol=0   ! red
      if(col.eq.'g') iicol=32  ! grey50
      call winscl('z',iicol)
      if(type.eq.'bw')then
        CALL ecirc(iix,iiy,5,0)
        CALL ecirc(iix,iiy,3,0)
      elseif(type.eq.'bk')then
        CALL ecirc(iix,iiy,5,0)
        CALL ecirc(iix,iiy,4,0)
        CALL ecirc(iix,iiy,3,0)
      elseif(type.eq.'iu')then
        CALL ecirc(iix,iiy+8,4,1)
      elseif(type.eq.'ik')then
        CALL ecirc(iix,iiy,5,0)
        CALL ecirc(iix,iiy,3,1)
      endif

C Write name for other than 'iu'.
      if(type.eq.'iu')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +8  ! offset to right of circle
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+8,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      else
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +8  ! offset to right of circle
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      endif
      iicol=0
      call winscl('-',iicol)
      
      return
      end


C ********* symbol_flow_cmp
C Draw a symbol for a flow component with name and symbol type.
C type='bx' box, type='cd' conduit type='cr' crack, 'or' orifice,
C type='bi' bi-directional
C col is 'r' red  'g' grey
      subroutine symbol_flow_cmp(name,type,col,xw,yw,zw)
#include "building.h"
#include "prj3dv.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER name*12,type*2,col*1
      real xw,yw,zw

      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      DIMENSION  COG1(3),COG2(3),COG3(3)
      character t13*13
#ifdef OSI
      integer iix,iiy,iid4,iicol
#else
      integer*8 iix,iiy,iid4,iicol
#endif

      if(MMOD.lt.8) return  ! in text mode do not draw

C Get pixel position to draw at.
      COG1(1)=xw; COG1(2)=yw; COG1(3)=zw
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix,iiy)

C Draw symbol depending on type.
      if(col.eq.'r') iicol=0   ! red
      if(col.eq.'g') iicol=32  ! grey50
      call winscl('z',iicol)
      if(type.eq.'bx')then
        call esymbol(iix,iiy,38,1) ! box symbol
      elseif(type.eq.'cr')then
        call esymbol(iix,iiy,34,1) ! crack symbol
      elseif(type.eq.'cd')then
        call esymbol(iix,iiy,37,1) ! conduit symbol
      elseif(type.eq.'or')then
        call esymbol(iix,iiy,36,1) ! orifice symbol
      elseif(type.eq.'bi')then
        call esymbol(iix,iiy+5,35,1) ! bidir symbol
      endif

C Write name to the right of the symbol.
      t13=' '
      if(type.eq.'bx')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +8
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      elseif(type.eq.'cr')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +10
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      elseif(type.eq.'cd')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +10
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      elseif(type.eq.'or')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +8
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      elseif(type.eq.'bi')then
        write(t13,'(A)')name(1:lnblnk(name))
        IF(IFS.GT.0)CALL winfnt(0)
        iid4 = iix +8
        if(col.eq.'r') iicol=0   ! red
        if(col.eq.'g') iicol=32  ! grey50
        call winscl('z',iicol)
        CALL wstxpt(iid4,iiy+5,t13)
        IF(IFS.GT.0)CALL winfnt(IFS)
        call forceflush()
      endif
      iicol=0
      call winscl('-',iicol)
      
      return
      end

C ********* draw_flow_link
C Draw_flow_link places dotted line between a flow node and
C component in such as way as to not obscure node and/or component.

      subroutine draw_flow_link(xs,ys,zs,xe,ye,ze)
#include "building.h"
#include "prj3dv.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
C      COMMON/OUTIN/IUOUT,IUIN
      real xs,ys,zs,xe,ye,ze

      real tdis
C      character outs*124
      DIMENSION COG1(3),COG2(3),COG3(3)
#ifdef OSI
      integer iix1,iiy1,iix2,iiy2,iicol
#else
      integer*8 iix1,iiy1,iix2,iiy2,iicol
#endif

      if(MMOD.lt.8) return  ! in text mode do not draw
      
C Find distance and report.
      tdis= crowxyz(xs,ys,zs,xe,ye,ze)
C      write(outs,'(a,3f7.3,a,3f7.3,a,f9.4)') ' Distance between ',
C     &   xs,ys,zs,' & ',xe,ye,ze,' =',tdis
C      call edisp(iuout,outs)
      if(tdis.gt.10)then
        vdis=0.3
      else
        vdis=0.2
      endif

C Use ratio calculation.
      r2 = tdis - vdis
      r1 = vdis
      x3 = ((r2 * xs) + (r1 * xe))/tdis
      y3 = ((r2 * ys) + (r1 * ye))/tdis
      z3 = ((r2 * zs) + (r1 * ze))/tdis
      COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix1,iiy1)

C Now do the other end.
      r2 = tdis - vdis
      r1 = vdis
      x3 = ((r2 * xe) + (r1 * xs))/tdis
      y3 = ((r2 * ye) + (r1 * ys))/tdis
      z3 = ((r2 * ze) + (r1 * zs))/tdis
      COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix2,iiy2)

C Draw a dotted line (in red=0) (powderblue 27) (navy blue 34) (midnight blue 1).
      iicol=0  ! red
      call winscl('z',iicol)
      call edline(iix1,iiy1,iix2,iiy2,2)
      call forceflush()
      iicol=0
      call winscl('-',iicol)
      return
      end


C ************************* MFWFDRAWNC 
C MFWFDRAW Fluid flow file: draw node & components from common block contents.
C colour is either 'r' red or 'g' grey50 for components.
      SUBROUTINE MFWFDRAWNC(col)

#include "net_flow.h"
#include "net_flow_data.h"
#include "building.h"
      
C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer net3ddelay  ! delay between timesteps in 3D network
      common/delay3d/net3ddelay
      character col*1
      CHARACTER name*12
      integer POS,NEG,CMPIS
      real xc,yc,zc,xw,yw,zw
      integer idms

      if(MMOD.lt.8) return  ! in text mode do not draw
      if(net3ddelay.eq.1)idms=20
      if(net3ddelay.eq.2)idms=10
      if(net3ddelay.eq.3)idms=5
      if(net3ddelay.eq.4)idms=3
      
C Node data.
      IF(NNOD.gt.0)then
        DO INOD=1,NNOD
          name=NDNAM(INOD)
          xw=HNOD(INOD,1);yw=HNOD(INOD,2); zw=HNOD(INOD,3)
          if(NDTYP(INOD).eq.0)then
            call symbol_flow_nd(name,'iu',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.1)then
            call symbol_flow_nd(name,'ik',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.2)then
            call symbol_flow_nd(name,'bk',col,xw,yw,zw)
            call pausems(idms)
          elseif(NDTYP(INOD).eq.3)then
            call symbol_flow_nd(name,'bw',col,xw,yw,zw)
            call pausems(idms)
          endif
        ENDDO
      endif

C Component data.
  
C << CONSIDER NWPCMP()  >>

      IF(NCMP.gt.0)then
        DO ICMP=1,NCMP
          name=CMNAM(ICMP)
          xc=HCMP(ICMP,1,1);yc=HCMP(ICMP,1,2); zc=HCMP(ICMP,1,3)

          IF(ITPCMP(ICMP).EQ. 10) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 11) THEN
            continue  ! Self regulating vent
          ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant vol flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant mass flow (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Common orifice flow
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Laminar pipe vol
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.110) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Specific air flow
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.120) THEN
            call symbol_flow_cmp(name,'cr',col,xc,yc,zc) ! Specific air flow crack 
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.130) THEN
            call symbol_flow_cmp(name,'bi',col,xc,yc,zc) ! Specific air flow door
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.210) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! General flow conduit
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.211) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Cowls and roof outlets (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.220) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 3 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.230) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 3 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.240) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 4 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.250) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 4 leg
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.310) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow inducer (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.410) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow corrector (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.420) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Corrector polynomial (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.460) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Fixed flow rates controller (box)
            call pausems(idms)
          ELSE IF(ITPCMP(ICMP).EQ.500) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! multi-configuration (box)
            call pausems(idms)
          endif
        ENDDO
      endif

  100 RETURN

      END

C ********* draw_flow_value
C Draw_flow_link places dotted line between a flow node and
C component in such as way as to not obscure node and/or component.

      subroutine draw_flow_value(word,xs,ys,zs,xe,ye,ze)
#include "building.h"
#include "prj3dv.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
C      COMMON/OUTIN/IUOUT,IUIN
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      character word*12
      real xs,ys,zs,xe,ye,ze

      real tdis,hdis
C      character outs*124
      character t13*13
      DIMENSION COG1(3),COG2(3),COG3(3)
#ifdef OSI
      integer iix1,iiy1,iix2,iiy2,iicol,iid4
#else
      integer*8 iix1,iiy1,iix2,iiy2,iicol,iid4
#endif

      if(MMOD.lt.8) return  ! in text mode do not draw
      
C Find distance and report.
      tdis= crowxyz(xs,ys,zs,xe,ye,ze)
      hdis=tdis*0.5
C      write(outs,'(a,3f7.3,a,3f7.3,a,2f9.4)') ' Distance between ',
C     &   xs,ys,zs,' & ',xe,ye,ze,' =',tdis,hdis
C      call edisp(iuout,outs)
      if(tdis.gt.10)then
        vdis=0.35
      else
        vdis=0.25
      endif

C Use ratio calculation.
      r2 = tdis - vdis
      r1 = vdis
      x3 = ((r2 * xs) + (r1 * xe))/tdis
      y3 = ((r2 * ys) + (r1 * ye))/tdis
      z3 = ((r2 * zs) + (r1 * ze))/tdis
      COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix1,iiy1)

C Now do the other end.
      r2 = tdis - vdis
      r1 = vdis
      x3 = ((r2 * xe) + (r1 * xs))/tdis
      y3 = ((r2 * ye) + (r1 * ys))/tdis
      z3 = ((r2 * ze) + (r1 * zs))/tdis
      COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix2,iiy2)

C Draw a thick line (in red=0) (powderblue 27) (navy blue 34) (midnight blue 1).
      iicol=0  ! red
      call winscl('z',iicol)
      call edwline(iix1,iiy1,iix2,iiy2)
      call forceflush()
      iicol=0
      call winscl('-',iicol)

C Draw the value word at half-way point.
      r2 = tdis - hdis
      r1 = hdis
      x3 = ((r2 * xs) + (r1 * xe))/tdis
      y3 = ((r2 * ys) + (r1 * ye))/tdis
      z3 = ((r2 * zs) + (r1 * ze))/tdis
      COG1(1)=x3; COG1(2)=y3; COG1(3)=z3
      CALL VECTRN(COG1,TEMAT,COG2,IER)
      CALL VECTRN(COG2,ETSMAT,COG3,IER)
      call u2pixel(COG3(1),COG3(2),iix1,iiy1)
      write(t13,'(A)')word(1:lnblnk(word))
      call textpixwidth(t13,ipixw)
      IF(IFS.GT.0)CALL winfnt(0)
      iid4 = iix1-(ipixw/2)
      iicol=0   ! red
      call winscl('z',iicol)
      CALL wstxpt(iid4,iiy1-5,t13)
      IF(IFS.GT.0)CALL winfnt(IFS)
      call forceflush()

      return
      end
