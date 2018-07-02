C mfprb3.F includes subroutines which either support the drawing of
C 3D flow entities or user dialogs establishing postion.

C  mfwfdraw - draws 3D flow network overlay on wireframe from commons passed colour.
C  symbol_flow_nd - draws a flow node symbol passed name type colour & position
C  symbol_flow_cmp - draws a flow component symbol passed name type colour & position
C  draw_flow_link - draws a flow connection between two coordinates
C  draw_surf_flow_symbol - places flow symbol after user chooses surface USE 

C mfcompposition - user interactions eastablishing flow component position.
C mfnodeposition - user interactions for flow node position.
C scan_flow_use - scans surface USE attributes related to mass flows,
C   extracts position information, fills flow common blocks and creates
C   both legacy and a 3D flow files while drawing wireframe overlay.

C ************************* MFWFDRAW 
C MFWFDRAW Fluid flow file: draw common block contents.
C colour is either 'r' red or 'g' grey50 for components.
      SUBROUTINE MFWFDRAW(col)

#include "net_flow.h"
#include "net_flow_data.h"
#include "building.h"
      
C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      character col*1
      CHARACTER name*12
      integer POS,NEG,CMPIS
      real xc,yc,zc,xw,yw,zw

      if(MMOD.lt.8) return  ! in text mode do not draw

C Node data.
      IF(NNOD.gt.0)then
        DO INOD=1,NNOD
          name=NDNAM(INOD)
          xw=HNOD(INOD,1);yw=HNOD(INOD,2); zw=HNOD(INOD,3)
          if(NDTYP(INOD).eq.0)then
            call symbol_flow_nd(name,'iu',col,xw,yw,zw)
            call pausems(100)
          elseif(NDTYP(INOD).eq.1)then
            call symbol_flow_nd(name,'ik',col,xw,yw,zw)
            call pausems(100)
          elseif(NDTYP(INOD).eq.2)then
            call symbol_flow_nd(name,'bk',col,xw,yw,zw)
            call pausems(100)
          elseif(NDTYP(INOD).eq.3)then
            call symbol_flow_nd(name,'bw',col,xw,yw,zw)
            call pausems(100)
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
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 11) THEN
            continue  ! Self regulating vent
          ELSE IF(ITPCMP(ICMP).EQ. 12) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law vol flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 15) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 17) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! power law mass flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 20) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law vol flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 25) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! quadratic law mass flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 30) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant vol flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 35) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! constant mass flow (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 40) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Common orifice flow
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ. 50) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Laminar pipe vol
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.110) THEN
            call symbol_flow_cmp(name,'or',col,xc,yc,zc) ! Specific air flow
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.120) THEN
            call symbol_flow_cmp(name,'cr',col,xc,yc,zc) ! Specific air flow crack 
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.130) THEN
            call symbol_flow_cmp(name,'bi',col,xc,yc,zc) ! Specific air flow door
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.210) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! General flow conduit
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.211) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Cowls and roof outlets (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.220) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 3 leg
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.230) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 3 leg
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.240) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit ending in conv 4 leg
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.250) THEN
            call symbol_flow_cmp(name,'cd',col,xc,yc,zc) ! Conduit starts in diverg 4 leg
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.310) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow inducer (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.410) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! General flow corrector (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.420) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Corrector polynomial (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.460) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! Fixed flow rates controller (box)
            call pausems(100)
          ELSE IF(ITPCMP(ICMP).EQ.500) THEN
            call symbol_flow_cmp(name,'bx',col,xc,yc,zc) ! multi-configuration (box)
            call pausems(100)
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

C  
C Based on the surface USE attributes draw relevant symbol
C on the wire-frame. If act 'v' show names.
      subroutine draw_surf_flow_symbol(loop,act)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "prj3dv.h"

C Passed parameters.
      integer loop     ! the connection number for the surface
      character act*1  ! '-' try not to show component name

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      character LAPROB*72

      CHARACTER outs*124,name*12
      CHARACTER SN*12,ZN*12
      logical dupedges
      logical inthenetwork
      integer loop2
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      real edgelower,edgeupper  ! Z of current surface lower & upper edges.
      real xc,yc,zc

C Cases to ignore.
      if(SSUSE(loop,1)(1:1).eq.'-'.or.
     &   SSUSE(loop,1)(1:5).eq.'FLOOR'.or.
     &   SSUSE(loop,1)(1:5).eq.'FURNI'.or.
     &   SSUSE(loop,1)(1:7).eq.'ITEQUIP'.or.
     &   SSUSE(loop,1)(1:5).eq.'PARTN'.or.
     &   SSUSE(loop,1)(1:4).eq.'ROOF'.or.
     &   SSUSE(loop,1)(1:5).eq.'STRUC'.or.
     &   SSUSE(loop,1)(1:7).eq.'FIXTURE'.or.
     &   SSUSE(loop,1)(1:6).eq.'PLANTS')then
        return
      endif

C Under what circumstances do we not want to draw symbols?  << NOT YET DECIDED >>
C      if(IAIRN.eq.3) return  

      PI = 4.0 * ATAN(1.0)

      iz=IC1(loop); is=IE1(loop)  ! get current zone & surface.
      write(SN,'(a)') SSNAME(loop)
      write(ZN,'(a)') ZNAME(iz)
      name='  '
      lnsn=lnblnk(SN)
      lnzn=lnblnk(ZN)
      inthenetwork=.false.
      xc=0.0; yc=0.0; zc=0.0
      if(IAIRN.eq.3)then

C If IAIRN = 3 loop through the components looking for one
C which references the same zone and surface and use the
C name and position in the flow network. Otherwise use
C the approximate location and truncated name.
C Jump out when inthenetwork becomes true.
        do loop2=1,NCMP
          lnczn=lnblnk(CMPASSOC(loop2,1))
          lncsn=lnblnk(CMPASSOC(loop2,2))
          if(ZN(1:lnzn).eq.CMPASSOC(loop2,1)(1:lnczn).and.
     &       SN(1:lnsn).eq.CMPASSOC(loop2,2)(1:lncsn))then
            write(name,'(a)') CMNAM(loop2)(1:lnblnk(CMNAM(loop2)))
            xc=HCMP(loop2,1,1); yc=HCMP(loop2,1,2) ! recover
            zc=HCMP(loop2,1,3) 
            write(6,*) 'match ',iz,loop,loop2,name,xc,yc,zc
            inthenetwork=.true.
            EXIT
          endif
        enddo  ! of loop2
      endif

C Note: llpos, lrpos etc are the position in the nzjvn list at each
C of the corners of the surface.
      CALL ZSURLEHI(IZ,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
      edgelower=vcoord(iz,nzjvn(loop,llpos),3)
      edgeupper=vcoord(iz,nzjvn(loop,ulpos),3)

C For DOOR variants.
      if(SSUSE(loop,1)(1:4).eq.'DOOR'.or.
     &   SSUSE(loop,1)(1:6).eq.'P-DOOR'.or.
     &   SSUSE(loop,1)(1:6).eq.'H-DOOR'.or.
     &   SSUSE(loop,1)(1:6).eq.'H-DOOR')then
        if(SSUSE(loop,2)(1:6).eq.'CLOSED')then
          continue   ! no flow
        elseif(SSUSE(loop,2)(1:8).eq.'UNDERCUT')then

C Undercut at door - crack component, assume width = 5mm, length = door width
C at lowest horizontal edge.  Ok.
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'DoCr'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &    ' door undercut 5mm x ',XYMAX,'m @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cr','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Open door - common orifice component, (type 40) IVALCM(10) MF040I 
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'DoOP'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,f6.1,a,f7.3,a,3f6.2)') name,
     &      ' door open width ',XYMAX,' height',ZMAX,
     &      ' area',SSNA(loop),'m2 COG @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Bidirectional door - door component,  IVALCM(14)=130 MF130I
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'DoBI'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          VAL3=ABS(ZCOG(iz,3)-edgelower)  ! base to zone COG
          write(outs,'(2a,f6.1,a,f6.1,a,3f7.3,a,f6.3)') name,
     &      ' door bidir width ',XYMAX,
     &      ' height',ZMAX,' coef 0.5 base @XYZ',xc,yc,zc,
     &      ' delta base to zone COG',VAL3
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'bi','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'bi','g',xc,yc,zc)
          endif
          call pausems(50)
        endif
      elseif(SSUSE(loop,1)(1:5).eq.'FRAME'.or.
     &       SSUSE(loop,1)(1:7).eq.'F-FRAME')then

C For FRAME vairants.
        if(SSUSE(loop,2)(1:6).eq.'CLOSED')then
          continue   ! no flow
        elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Frame crack - crack component, assume width = 1mm,
C length = 0.5* frame inside perimeter, on centre line. OK
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'FrCr'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' frame crack width 1mm len',perim*0.5,' COG @XYZ',
     &      xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cr','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'VENT')then

C Frame trickle vent - air flow opening, assume width = 0.2* frame inside width,
C height = 10mm, at highest horizontal edge. OK
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'FrVe'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgeupper
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' frame trickle vent width ',XYMAX*0.2,' height 10mm @XYZ',
     &      xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        endif
      elseif(SSUSE(loop,1)(1:5).eq.'GRILL')then

C For GRILLs.
        if(SSUSE(loop,2).eq.'CLOSED')then
          continue   ! no flow
        elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Grill crack - crack component, assume width = 1mm,
C length = 0.5* frame inside perimeter, on centre line.
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'GrCR'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' grill crack width 1mm length',perim*0.5,' COG @XYZ',
     &      xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cr','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:5).eq.'INLET')then

C Grill inlet - a conduit (type 210) IVALCM(15), as in MF210I
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'GrIN'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' grill inlet hydr diam',
     &      4.0*SSNA(loop)/perim,' COG @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cd','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cd','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:7).eq.'EXTRACT')then

C Grill extract - constant vol flow comp. (type 30) IVALCM(8), as in MF030I
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'GrEX'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' grill extract hydr diam',
     &      4.0*SSNA(loop)/perim,' COG @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'bx','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'bx','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Grill opening - common orifice component, (type 40) IVALCM(10) MF040I 
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'GrOP'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f6.2)') name,
     &      ' grill orifice area',SSNA(loop),' COG @XYZ',
     &      xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        endif

      elseif(SSUSE(loop,1)(1:6).eq.'WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'D-WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'S-WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'C-WINDOW')then
        if(SSUSE(loop,2).eq.'CLOSED')then
          continue   ! no flow
        elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Window crack - crack component, assume width = 1mm,
C length = window width, at surface COG.
          call zsurfprm(iz,is,dupedges,perim)
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'WiCr'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' window crack width 1mm length',XYMAX,' @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cr','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Window open - common orifice component, (type 40) IVALCM(10) MF040I 
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'WiOp'
          endif
          if((XYMAX/ZMAX).gt.1.0)then
            if(inthenetwork)then
              continue
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
            endif
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &        ' window open width ',XYMAX*0.25,' height',ZMAX,
     &        ' area',((XYMAX*0.25)*ZMAX),'m2 COG @Z',xc,yc,zc
          else
            if(inthenetwork)then
              continue
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
            endif
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &        ' window open width ',XYMAX,' height',ZMAX*0.25,
     &        ' area',(XYMAX*(ZMAX*0.25)),'m2 COG @Z',xc,yc,zc
          endif
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'SASH')then

C Window sash - 2 common orifice component, (type 40) IVALCM(10) MF040I
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'WiSL'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
          endif
          write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f6.2)') name,
     &      ' width ',XYMAX,' height',ZMAX*0.125,
     &      ' area',(XYMAX*(ZMAX*0.125)),'m2 lower @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
    
C Now create the upper orifice.
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'WiSU'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgeupper
          endif
          write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f6.2)') name,
     &      ' width ',XYMAX,' height',ZMAX*0.125,
     &      ' area',(XYMAX*(ZMAX*0.125)),'m2 upper @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Window bidirectional - specific door component width & height 
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'WiBi'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
          endif
          VAL3=ABS(ZCOG(iz,3)-edgelower)  ! base to zone COG
          write(outs,'(2a,f6.1,a,f6.1,a,3f7.3,a,f6.3)') name,
     &      ' bidir width ',XYMAX,' height',ZMAX,
     &      ' coef 0.6 @XYZ',xc,yc,zc,
     &      ' delta base to zone COG',VAL3
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'bi','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'bi','g',xc,yc,zc)
          endif
          call pausems(50)
        endif

      elseif(SSUSE(loop,1)(1:4).eq.'FICT')then
        if(SSUSE(loop,2).eq.'CLOSED')then
          continue   ! no flow
        elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Fict crack - crack component, assume width = 1mm,
C length = window width, at lowest horizontal edge.
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'FiCR'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
          endif
          write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' fict crack width ',XYMAX,' height 1mm @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'cr','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Fict opening - common orifice component, (type 40) IVALCM(10) MF040I 
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'FiOP'
          endif
          if((XYMAX/ZMAX).gt.1.0)then
            if(inthenetwork)then
              continue
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
            endif
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &      ' fict open width ',XYMAX*0.25,' height',ZMAX,
     &      ' area',((XYMAX*0.25)*ZMAX),'m2 COG @Z',xc,yc,zc
          else
            if(inthenetwork)then
              continue
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
            endif
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &      ' fict open width ',XYMAX,' height',ZMAX*0.25,
     &      ' area',(XYMAX*(ZMAX*0.25)),'m2 COG @Z',xc,yc,zc
          endif
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
          else
            call symbol_flow_cmp(name,'or','g',xc,yc,zc)
          endif
          call pausems(50)
        elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Fict bi-directional - door component IVALCM(14)=130 MF130I
          if(inthenetwork)then
            continue
          else
            write(name,'(a)') 'FiBI'
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
          endif
          write(outs,'(2a,f6.1,a,f6.1,a,3f7.3)') name,
     &      ' fict bidir width ',XYMAX*0.75,' height',ZMAX*0.75,
     &      ' coef 0.6 base @XYZ',xc,yc,zc
          if(act.eq.'v') call edisp(iuout,outs)
          if(inthenetwork)then
            call symbol_flow_cmp(name,'bi','r',xc,yc,VAL3)
          else
            call symbol_flow_cmp(name,'bi','g',xc,yc,VAL3)
          endif
          call pausems(50)
        endif
      endif
      return
      end

C ***************** mfcompposition 
C mfcompposition does user interactions for flow component position.
C passed component index and returns associated zone & surface, the
C position and several useful surface derived attributes.
      SUBROUTINE mfcompposition(ICMP,iz,iso,cx,cy,cz,carea,cperim,
     &  cwidth,cheight,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "prj3dv.h"

C Parameters passed
      integer ICMP        ! index of component
      real cx,cy,cz       ! returned position
      real carea,cperim   ! returned surface area and perimeter
      real cwidth,cheight ! returned surface width & height
      integer IER  ! 0=use all returns, 1 use only position,  3 not 3D file

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/gzonpik/izgfoc,nzg,nznog(mcom)

      CHARACTER outs*124,HOLD*36
      real edgelower,edgeupper  ! Z of current surface lower & upper edges.
      integer loop
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      logical dupedges

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='mfcompposition'  ! set for subroutine

      IER=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='comp_position'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If not 3D network exit.
      if(IAIRN.lt.3)then
        ier=3
        return
      endif

C Position in space if IAIRN=3.
      write(hold,'(f9.3,f9.3,f8.3,a)') HCMP(ICMP,1,1),HCMP(ICMP,1,2),
     &  HCMP(ICMP,1,3),'   '
 342  CALL EASKS2CMD(HOLD,'Component: position in space:',' ',
     &  'accept current','via a specific surface',iclkok,36,
     &  ' 1. 0. 0.','position',iser,nbhelp)
      if(iclkok.eq.1)then    ! notice accept current so just return
        cx=HCMP(ICMP,1,1)
        cy=HCMP(ICMP,1,2)
        cz=HCMP(ICMP,1,3)
        carea=0.; cperim=0.; cwidth=0.; cheight=0.
        ier=1
        call usrmsg(' ',' ','-')
        return
      elseif(iclkok.eq.2)then

C Ask for zone and surface.
        CALL EASKGEOF('Select an existing zone, else exit.',
     &    CFGOK,IZ,'-',IER)
        if(IZ.GT.0)then

C General image option flags. Rescan the zone geometry file.
          call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)
          CALL ESCZONE(IZ)
          CALL BNDOBJ(0,IER)
          CALL ERCZONE(IZ)
          ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
          ITVNO=1; ITORG=1; ITSNR=1; ITGRD=1
          GRDIS=0.0; ITPPSW=0

C Reset all surface lines to standard width and display the zone.
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=IZ
          izgfoc=IZ
          CALL ADJVIEW(IER)

          ISO=1
          CALL EPMENSV
          CALL EASKSUR(IZ,ISO,'A','Select surface ',
     &      'to associate with this flow entity.',IER)
          CALL EPMENRC
          loop=izstocn(iz,iso)

C Note: llpos, lrpos etc are the position in the nzjvn list at each
C of the corners of the surface.
          call zsurfprm(iz,iso,dupedges,perim)
          CALL ZSURLEHI(IZ,ISO,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
          edgelower=vcoord(iz,nzjvn(loop,llpos),3)
          edgeupper=vcoord(iz,nzjvn(loop,ulpos),3)
          write(outs,'(a,3f8.3)') 'Surface COG is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),SSURCOG(loop,3)
          call edisp(iuout,outs)
          write(outs,'(a,3f8.3)') 'Surface upper edge is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),edgeupper
          call edisp(iuout,outs)
          write(outs,'(a,3f8.3)') 'Surface lower edge is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),edgelower
          call edisp(iuout,outs)
          write(outs,'(a,f7.1,f6.2)') 'Surface area & perimeter:',
     &      SSNA(loop),perim 
          call edisp(iuout,outs)
          write(outs,'(a,f7.1,f6.2)') 'Surface width & height:',
     &      XYMAX,ZMAX 
          call edisp(iuout,outs)
          carea=SSNA(loop)  ! instanciate suggested values
          cperim=perim
          cwidth=XYMAX
          cheight=ZMAX

C Present options COG, upper edge, lower edge or manual edit.
          CALL EASKABCD('Position options:',' ',
     &      'surface COG','surface upper edge',
     &      'surface lower edge','manual edit',IW,nbhelp)
          if(iw.eq.1)then
            cx=SSURCOG(loop,1)
            cy=SSURCOG(loop,2)
            cz=SSURCOG(loop,3)
            ier=0
            call usrmsg(' ',' ','-')
            return
          elseif(iw.eq.2)then
            cx=SSURCOG(loop,1)
            cy=SSURCOG(loop,2)
            cz=edgeupper
            ier=0
            call usrmsg(' ',' ','-')
            return
          elseif(iw.eq.3)then
            cx=SSURCOG(loop,1) 
            cy=SSURCOG(loop,2)
            cz=edgelower
            ier=0
            call usrmsg(' ',' ','-')
            return
          else
            CALL EASKS(HOLD,'Position in space?',
     &        ' ',36,' 1.0  0.0  0.0 ','position',IER,nbhelp)
          endif
        else

C No selection so just return current position.
          cx=HCMP(ICMP,1,1)
          cy=HCMP(ICMP,1,2)
          cz=HCMP(ICMP,1,3)
          carea=0.; cperim=0.; cwidth=0.; cheight=0.
          ier=1
          call usrmsg(' ',' ','-')
          return
        endif

C Extract XYZ values from the string.
        K=0
        CALL EGETWR(HOLD,K,cx,-999.9,999.9,'W','X',IER)
        CALL EGETWR(HOLD,K,cy,-99.9,999.9,'W','Y',IER)
        CALL EGETWR(HOLD,K,cz,-99.9,999.0,'W','Z',IER)
        if(ier.ne.0)goto 342
        ier=0
        call usrmsg(' ',' ','-')
        return
      else

C Extract XYZ values from the string.
        K=0
        CALL EGETWR(HOLD,K,cx,-999.9,999.9,'W','X',IER)
        CALL EGETWR(HOLD,K,cy,-99.9,999.9,'W','Y',IER)
        CALL EGETWR(HOLD,K,cz,-99.9,999.0,'W','Z',IER)
        if(ier.ne.0)goto 342
        ier=1  ! signal only position changed
      endif
      call usrmsg(' ',' ','-')
      RETURN
      END

C ***************** mfnodeposition 
C mfnodeposition does user interactions for flow node position.
C passed node index and returns associated zone & surface, the
C position and several useful surface derived attributes.
C If boundary wind or fixed pressure offset from the surface.

      SUBROUTINE mfnodeposition(INOD,iz,iso,cx,cy,cz,carea,cperim,
     &  cwidth,cheight,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "prj3dv.h"

C Parameters passed
      integer INOD        ! index of node
      real cx,cy,cz       ! returned position
      real carea,cperim   ! returned surface area and perimeter
      real cwidth,cheight ! returned surface width & height
      integer IER  ! 0=use all returns, 1 use only position,  3 not 3D file

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/gzonpik/izgfoc,nzg,nznog(mcom)

      CHARACTER outs*124,HOLD*36
      real edgelower,edgeupper  ! Z of current surface lower & upper edges.
      integer loop
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      logical dupedges

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='mfnodeposition'  ! set for subroutine

      IER=0

C Synopsis of flow node.
      if(NDTYP(INOD).eq.0)then
        WRITE(outs,'(2A)') NDNAM(INOD),' internal unknown pressure'
      elseif(NDTYP(INOD).eq.1)then
        WRITE(outs,'(2A)') NDNAM(INOD),' internal known pressure'
      elseif(NDTYP(INOD).eq.2)then
        WRITE(outs,'(2A)') NDNAM(INOD),' external known pressure'
      elseif(NDTYP(INOD).eq.3)then
        WRITE(outs,'(2A)') NDNAM(INOD),' external wind pressure'
      endif
      call edisp(iuout,outs)
      helptopic='node_position'
      call gethelptext(helpinsub,helptopic,nbhelp)

C If not 3D network exit.
      if(IAIRN.lt.3)then
        ier=3
        return
      endif

C For calculating offset.
      PI = 4.0 * ATAN(1.0); RAD = PI/180.0;

C Position in space if IAIRN=3.
      write(hold,'(f9.3,f9.3,f8.3,a)') HNOD(INOD,1),HNOD(INOD,2),
     &  HNOD(INOD,3),'   '
 342  CALL EASKS2CMD(HOLD,'Node: position in space:',' ',
     &  'accept current','via a specific zone or surface',iclkok,36,
     &  ' 1. 0. 0.','position',iser,nbhelp)
      if(iclkok.eq.1)then    ! notice accept current so just return
        cx=HNOD(INOD,1)
        cy=HNOD(INOD,2)
        cz=HNOD(INOD,3)
        carea=0.; cperim=0.; cwidth=0.; cheight=0.
        ier=1
        call usrmsg(' ',' ','-')
        return
      elseif(iclkok.eq.2)then

C Ask for zone and optionally for surface.
        CALL EASKGEOF('Select an existing zone, else exit.',
     &    CFGOK,IZ,'-',IER)
        if(IZ.GT.0)then

C General image option flags. Rescan the zone geometry file.
          call georead(IFIL+1,LGEOM(IZ),IZ,1,iuout,IER)
          CALL ESCZONE(IZ)
          CALL BNDOBJ(0,IER)
          CALL ERCZONE(IZ)
          ITDSP=1; ITBND=1; ITEPT=0; ITZNM=0; ITSNM=0
          ITVNO=1; ITORG=1; ITSNR=1; ITGRD=1
          GRDIS=0.0; ITPPSW=0

C Reset all surface lines to standard width and display the zone.
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=IZ
          izgfoc=IZ
          CALL ADJVIEW(IER)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.

          if(NDTYP(INOD).eq.0)then

C If node is internal unknown then set the zone COG and return.
            cx=ZCOG(iz,1); cy=ZCOG(iz,2); cz=ZCOG(iz,3)
            carea=0.; cperim=0.; cwidth=0.; cheight=0.
            ier=1
            call usrmsg(' ',' ','-')
            return
          elseif(NDTYP(INOD).eq.1)then

C If node is internal known then set the zone COG and return.
            cx=ZCOG(iz,1); cy=ZCOG(iz,2); cz=ZCOG(iz,3)
            carea=0.; cperim=0.; cwidth=0.; cheight=0.
            ier=1
            call usrmsg(' ',' ','-')
            return
          endif

C For boundary known and wind pressure request a surface.
          ISO=1
          CALL EPMENSV
          CALL EASKSUR(IZ,ISO,'A','Select surface ',
     &      'to associate with this node.',IER)
          CALL EPMENRC
          loop=izstocn(iz,iso)  ! the connection

C Calculate offset.
          V1=0.9; AZ=sspazi(loop); EL=sspelv(loop)
          RYAZI = AZ*RAD; RSALT = EL*RAD
          z3 = V1*SIN(RSALT)
          XYDIS = V1*COS(RSALT)
          IF (XYDIS .LT. 1E-6)THEN
            x3 = 0.0; y3 = 0.0
          ELSE
            x3 = XYDIS*SIN(RYAZI); y3 = XYDIS*COS(RYAZI)
          ENDIF

C Note: llpos, lrpos etc are the position in the nzjvn list at each
C of the corners of the surface.
          call zsurfprm(iz,iso,dupedges,perim)
          CALL ZSURLEHI(IZ,ISO,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
          edgelower=vcoord(iz,nzjvn(loop,llpos),3)
          edgeupper=vcoord(iz,nzjvn(loop,ulpos),3)
          write(outs,'(a,3f8.3)') 'Surface COG is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),SSURCOG(loop,3)
          call edisp(iuout,outs)
          write(outs,'(a,3f8.3)') 'Surface upper edge is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),edgeupper
          call edisp(iuout,outs)
          write(outs,'(a,3f8.3)') 'Surface lower edge is:',
     &      SSURCOG(loop,1),SSURCOG(loop,2),edgelower
          call edisp(iuout,outs)
          write(outs,'(a,f7.1,f6.2)') 'Surface area & perimeter:',
     &      SSNA(loop),perim 
          call edisp(iuout,outs)
          write(outs,'(a,f7.1,f6.2)') 'Surface width & height:',
     &      XYMAX,ZMAX 
          call edisp(iuout,outs)
          carea=SSNA(loop)  ! instanciate suggested values
          cperim=perim
          cwidth=XYMAX
          cheight=ZMAX

C Present options COG, upper edge, lower edge or manual edit.
          CALL EASKABCD(
     &      'Position options (node will be 1m away from surface:',
     &      ' ','surface COG','surface upper edge',
     &      'surface lower edge','manual edit',IW,nbhelp)
          if(iw.eq.1)then  
            cx=x3+SSURCOG(loop,1)
            cy=y3+SSURCOG(loop,2)
            cz=z3+SSURCOG(loop,3)
            ier=0
            call usrmsg(' ',' ','-')
            return
          elseif(iw.eq.2)then  
            cx=x3+SSURCOG(loop,1)
            cy=y3+SSURCOG(loop,2)
            cz=z3+edgeupper
            ier=0
            call usrmsg(' ',' ','-')
            return
          elseif(iw.eq.3)then  
            cx=x3+SSURCOG(loop,1) 
            cy=y3+SSURCOG(loop,2)
            cz=z3+edgelower
            ier=0
            call usrmsg(' ',' ','-')
            return
          else
            CALL EASKS(HOLD,'Node: position in space (no offset)?',
     &        ' ',36,' 1.0  0.0  0.0 ','position',IER,nbhelp)
          endif
        else

C No selection so just return current position.
          cx=HNOD(INOD,1)
          cy=HNOD(INOD,2)
          cz=HNOD(INOD,3)
          carea=0.; cperim=0.; cwidth=0.; cheight=0.
          ier=1
          call usrmsg(' ',' ','-')
          return
        endif

C Extract XYZ values from the string.
        K=0
        CALL EGETWR(HOLD,K,cx,-999.9,999.9,'W','X',IER)
        CALL EGETWR(HOLD,K,cy,-99.9,999.9,'W','Y',IER)
        CALL EGETWR(HOLD,K,cz,-99.9,999.0,'W','Z',IER)
        if(ier.ne.0)goto 342
        ier=0
        call usrmsg(' ',' ','-')
        return
      else

C Extract XYZ values from the string.
        K=0
        CALL EGETWR(HOLD,K,cx,-999.9,999.9,'W','X',IER)
        CALL EGETWR(HOLD,K,cy,-99.9,999.9,'W','Y',IER)
        CALL EGETWR(HOLD,K,cz,-99.9,999.0,'W','Z',IER)
        if(ier.ne.0)goto 342
        ier=1  ! signal only position changed
      endif
      call usrmsg(' ',' ','-')
      RETURN
      END

C ***********************************
C scan_flow_use scanns surface USE attributes related to mass flows,
C extracts position information, fills flow common blocks and creates
C both legacy and a 3D flow file while drawing over wireframe.

C << TODO: 
C << add trace verbosity directives for feedback
      subroutine scan_flow_use()
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)
#include "prj3dv.h"
      INTEGER, PARAMETER :: MNWKTYP=6  ! from gnetwk.h
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      character LAPROB*72
      COMMON/MFLOWIT/fndegc,imix 

C To signal to MFWRIT that globals are available from graphic network.
C Currently set .FALSE.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

C Documentation shared with graphic flow network.
      COMMON/NWKSTR/NWKNAM,NWKDSC,NWKTYPSTR(MNWKTYP)
      CHARACTER NWKNAM*72,NWKDSC*72,NWKTYPSTR*12
      COMMON/NWKTYP/INWKTYP,vergnf
      INTEGER inwktyp
      REAL vergnf   ! 1.0 is 3D variant of ASCII network

      DIMENSION IVALS(MCOM),IVALS2(MCOM)
      DIMENSION  COG1(3),COG2(3),COG3(3)
      DIMENSION linkatpartition(MCON)
      CHARACTER outs*124,name*12
      character CXSTR*78
      CHARACTER prompt*72,hold*32,fs*1,SN*12
      CHARACTER LTMP*72
      logical ok,dok,unixok
      logical dupedges
      integer loop
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      integer CMPIS  ! index of component we just created
      integer POS,NEG
      logical useit
      real edgelower,edgeupper  ! Z of current surface lower & upper edges.
#ifdef OSI
      integer iix,iiy,iid4
#else
      integer*8 iix,iiy,iid4
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='scan_flow_use'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

      WRITE(OUTS,'(A)') 
     &  'Which zones to include in the fluid flow network?'
      IZN=NCOMP

C Clear zone->node mapping and then generate their names & Z values.
      NNOD=0; NCMP=0; NCNN=0  ! Reset
      DO ICZ=1,IZN
        ICAAS(ICZ)=0
      ENDDO  ! of ICZ
      DO loop=1,NCON
        linkatpartition(loop)=0  ! connection not yet done
      ENDDO ! of loop
      CALL EPICKS(IZN,IVALS,' ',OUTS,12,NCOMP,zname,'zone list',
     &  IER,nbhelp)

      IF(IZN.EQ.0)THEN
        CALL USRMSG('No zones selected, returning.',' ','W')
        ier=1
        return
      ENDIF

C Temporarily use same file unit as profiles db.
      NDNAM(0)='    '
      fndegc=20.0
      imix=1
      vergnf=1.0  ! signal 3D flow file variant
      inwktyp=2   ! to match graphic flow network type
      IF(INWKTYP.EQ.0)THEN

C Set the network type (default is plant).
        INWKTYP=1
        NWKTYPSTR(1)='HVAC'
      ENDIF
      NWKTYPSTR(2)='Flow'
      NWKTYPSTR(3)='Electrical'
      NWKTYPSTR(4)='Control'
      NWKTYPSTR(5)='Hygroscopic'
      NWKTYPSTR(6)='PrimitivePt'

      if(IPRODB.eq.IFIL+6)then
        IUM=IPRODB
      else
        IUM=IFIL+6
      endif
      CALL ERPFREE(IUM,ISTAT)
      CALL MFCDAT

C Attempt to read in a flow file. If file has yet to be named then base
C it on cfgroot and place it in the netpth folder (differienciate between
C unix and non-unix machine types).
      if(LAPROB(1:2).eq.'  '.or.LAPROB(1:4).eq.'UNKN')then
        if(unixok)then
          if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
            WRITE(LAPROB,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.afn'
            WRITE(NWKNAM,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          else
            WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.afn'
            WRITE(NWKNAM,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          endif
        else
          if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46.and.
     &       ichar(netpth(2:2)).eq.92))then
            WRITE(LAPROB,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.afn'
            WRITE(NWKNAM,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          else
            WRITE(LAPROB,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.afn'
            WRITE(NWKNAM,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          endif
        endif
      else
        if(unixok)then
          if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
            WRITE(NWKNAM,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          else
            WRITE(NWKNAM,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          endif
        else
          if(netpth(1:2).eq.'  '.or.(ichar(netpth(1:1)).eq.46.and.
     &       ichar(netpth(2:2)).eq.92))then
            WRITE(NWKNAM,'(2A)')cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          else
            WRITE(NWKNAM,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &      cfgroot(1:lnblnk(cfgroot)),'.3dafn'
          endif
        endif
      endif
   78 LTMP=LAPROB
      CALL EASKS(LTMP,'Fluid flow network file?',
     &  '(non-grapic version)',72,' ','mass flow file',IER,nbhelp)
      if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
        LAPROB=LTMP
      else
        goto 78
      endif
   79 LTMP=NWKNAM
      CALL EASKS(LTMP,'3D Fluid flow network file?',
     &  '(non-grapic 3D version)',72,' ','mass flow file',IER,nbhelp)
      if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
        NWKNAM=LTMP
      else
        goto 79
      endif
   80 LTMP=NWKDSC
      CALL EASKS(LTMP,'3D Fluid flow description?',
     &  '(<72 char)',72,' ','mass flow descr',IER,nbhelp)
      if(LTMP(1:2).ne.'  '.and.LTMP(1:4).ne.'UNKN')then
        NWKDSC=LTMP
      else
        goto 80
      endif
      CALL EFOPSEQ(IUM,LAPROB,4,IER)
      CALL USRMSG(
     &  'Generating flow nodes for selected zones ...',' ','-')  
      DO IZT=1,IZN
        IZ=IVALS(IZT)
        NNOD=NNOD+1

C Get zone information. For the zone associated with the current
C surface instanciate pointer from zone to the node.
        ICAAS(IZ)=NNOD

C Set node type to internal and fill in the data on volume, position 
C and temperature, setting the node name to the zone name.
        NDTYP(NNOD)=0; NDFLD(NNOD)=1 ! internal unkn air
        HNOD(NNOD,1)=ZCOG(IZ,1); HNOD(NNOD,2)=ZCOG(IZ,2)
        HNOD(NNOD,3)=ZCOG(IZ,3)
        SUPNOD(NNOD,2)=VOL(IZ); SUPNOD(NNOD,1)=0.0; TNOD(NNOD)=fndegc
        NDNAM(NNOD)=ZNAME(IZ)
        NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)='-'
        WRITE(OUTS,'(A,I3,3A)')'Node :',NNOD,' name: ',
     &    NDNAM(NNOD), ' type: internal, unknown pressure '
        CALL edisp(iuout,OUTS)
        write(6,*) outs(1:lnblnk(outs))
        WRITE(OUTS,'(A,3F9.3,A,F10.3)') ' XYZ(m): ',
     &    ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3), ' volume (m^3): ',
     &    SUPNOD(NNOD,2)
        CALL edisp(iuout,OUTS)
        write(6,*) outs(1:lnblnk(outs))
        write(name,'(a)') NDNAM(NNOD)
        call symbol_flow_nd(name,'iu','r',
     &    ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3))
        call pausems(100)
      ENDDO

C Loop through all the surfaces in the model. Based on the use
C attributes build up a list of likely components.
      call edisp(iuout,
     & 'Scanning surface attributes for boundary nodes & connections.')
      PI = 4.0 * ATAN(1.0); RAD = PI/180.0;
      do loop=1,NCON
        iz=IC1(loop); is=IE1(loop)  ! get current zone & surface.
        write(SN,'(a)') SSNAME(loop)

C Check if partition and linkatpartition already set.
C If so we do not need to create this component.
        if(ICT(loop).eq.3.and.linkatpartition(loop).gt.0)then
          CYCLE  ! no need to create anything for this surface
        endif
        useit=.false.
        name='  '
        call conxinfo(1,loop,CXSTR)

C Note: llpos, lrpos etc are the position in the nzjvn list at each
C of the corners of the surface.
        CALL ZSURLEHI(IZ,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
        edgelower=vcoord(iz,nzjvn(loop,llpos),3)
        edgeupper=vcoord(iz,nzjvn(loop,ulpos),3)

C Figure out the delta for the boundary node would be if
C 0.9m along normal vector.
        V1=0.9; AZ=sspazi(loop); EL=sspelv(loop)
        RYAZI = AZ*RAD; RSALT = EL*RAD
        z3 = V1*SIN(RSALT)
        XYDIS = V1*COS(RSALT)
        IF (XYDIS .LT. 1E-6)THEN
          x3 = 0.0; y3 = 0.0
        ELSE
          x3 = XYDIS*SIN(RYAZI); y3 = XYDIS*COS(RYAZI)
        ENDIF

C Cases to ignore.
        if(SSUSE(loop,1)(1:1).eq.'-'.or.
     &     SSUSE(loop,1)(1:5).eq.'FLOOR'.or.
     &     SSUSE(loop,1)(1:5).eq.'FURNI'.or.
     &     SSUSE(loop,1)(1:7).eq.'ITEQUIP'.or.
     &     SSUSE(loop,1)(1:5).eq.'PARTN'.or.
     &     SSUSE(loop,1)(1:4).eq.'ROOF'.or.
     &     SSUSE(loop,1)(1:5).eq.'STRUC'.or.
     &     SSUSE(loop,1)(1:7).eq.'FIXTURE'.or.
     &     SSUSE(loop,1)(1:6).eq.'PLANTS')then
          CYCLE
        endif

C For DOOR variants.
        if(SSUSE(loop,1)(1:4).eq.'DOOR'.or.
     &     SSUSE(loop,1)(1:6).eq.'P-DOOR'.or.
     &     SSUSE(loop,1)(1:6).eq.'H-DOOR'.or.
     &     SSUSE(loop,1)(1:6).eq.'H-DOOR')then
          if(SSUSE(loop,2)(1:6).eq.'CLOSED')then
            continue   ! no flow
          elseif(SSUSE(loop,2)(1:8).eq.'UNDERCUT')then

C Undercut at door - crack component, assume width = 5mm, length = door width
C at lowest horizontal edge.  Ok.
            write(name,'(a,i2.2,a,i3.3)') 'DoCrz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' door undercut 5mm x ',XYMAX,'m @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF120I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(13); LTPCMP(NCMP)=LVALCM(13)  !crack (120)
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=5.0/1000.0
            SUPCMP(NCMP,3)=XYMAX
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf lower.
            write(outs,'(a,f7.3)') 'Delta Z with surf lower is',
     &        edgelower-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then

C Define boundary point ~1m from facade.
              write(name,'(a,i2.2,a,i3.3)') 'BW-Cr',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+edgelower
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary undercut azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the component
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with component
              HGTNE(NCNN)=edgelower-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)

C Draw from boundary to component and then from component to the
C associated zone.
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1
                
C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif
    
          elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Open door - common orifice component, (type 40) IVALCM(10) MF040I 
            write(name,'(a,i2.2,a,i3.3)') 'DoOPz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,f6.1,a,f7.3,a,3f6.2)') name,
     &        ' door open width ',XYMAX,' height',ZMAX,
     &        ' area',SSNA(loop),'m2 COG @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF110I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10) ! common orifice (40)
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=SSNA(loop)
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.6
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)
            if(ICT(loop).eq.0)then

C Define boundary point 1m from facade.
              write(name,'(a,i2.2,a,i3.3)') 'BW-Op',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary opening azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with component
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0
              NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)

C Draw from boundary to component and then from component to the
C associated zone.
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif
  
          elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Bidirectional door - door component,  IVALCM(14)=130 MF130I
C assume width = door width, height = door height, discharge factor = 0.5.
C Need to draw component at surf COG but the real base is at bottom
C edge.
            write(name,'(a,i2.2,a,i3.3)') 'DoBIz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            VAL1=XYMAX; VAL2=ZMAX
            VAL3=ABS(ZCOG(iz,3)-edgelower)  ! base to zone COG
            VAL4=SSURCOG(loop,3)-ZCOG(IZ,3)        ! surf COG to zone COG
            write(outs,'(2a,f6.1,a,f6.1,a,3f7.3,a,f6.3)') name,
     &        ' door bidir width ',XYMAX,
     &        ' height',ZMAX,' coef 0.5 base @XYZ',xc,yc,zc,
     &        ' delta base to zone COG',VAL3
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(14); LTPCMP(NCMP)=LVALCM(14) ! bi-directional (130)
            ISDCMP(NCMP)=5; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL1
            SUPCMP(NCMP,3)=VAL2
            SUPCMP(NCMP,4)=VAL3
            if(SUPCMP(NCMP,5).lt.0.001) SUPCMP(NCMP,5)=0.5
            write(prompt,'(3a)') 'The discharge coefficient of ',
     &        CMNAM(NCMP),':'
            VAL2=SUPCMP(NCMP,5)
            CALL EASKR(VAL2,prompt,'  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,5)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'bi','r',xc,yc,zc)
            call pausems(100)
    
C Report on Delta Height between surface COG and zone COG.
            write(outs,'(a,f7.3)') 'Delta Z with base is',VAL4
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Bi',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary bidir azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the bi-directional
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with component
              HGTNE(NCNN)=VAL4   ! bi-dir assume surf COG to zone COG
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          endif
        elseif(SSUSE(loop,1)(1:5).eq.'FRAME'.or.
     &    SSUSE(loop,1)(1:7).eq.'F-FRAME')then

C For FRAME vairants.
          if(SSUSE(loop,2)(1:6).eq.'CLOSED')then
            continue   ! no flow
          elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Frame crack - crack component, assume width = 1mm,
C length = 0.5* frame inside perimeter, on centre line. OK
            call zsurfprm(iz,is,dupedges,perim)
            write(name,'(a,i2.2,a,i3.3)') 'FrCrz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' frame crack width 1mm len',perim*0.5,' COG @XYZ',
     &        xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF120I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(13); LTPCMP(NCMP)=LVALCM(13)  !crack
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=1.0/1000.0
            SUPCMP(NCMP,3)=perim*0.5
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Cr',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary crack azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the component
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:4).eq.'VENT')then

C Frame trickle vent - air flow opening, assume width = 0.2* frame inside width,
C height = 10mm, at highest horizontal edge. OK
            write(name,'(a,i2.2,a,i3.3)') 'FrVez',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgeupper
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &      ' frame trickle vent width ',XYMAX*0.2,' height 10mm @XYZ',
     &        xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF110I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(12); LTPCMP(NCMP)=LVALCM(12) ! Specific air op (110)
            ISDCMP(NCMP)=2; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=(XYMAX*0.2)*0.01
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf upper.
            write(outs,'(a,f7.3)') 'Delta Z with surf top is',
     &        edgeupper-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Ve',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+edgeupper
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the vent
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=edgeupper-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          endif
        elseif(SSUSE(loop,1)(1:5).eq.'GRILL')then

C For GRILLs.
          if(SSUSE(loop,2).eq.'CLOSED')then
            continue   ! no flow
          elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Grill crack - crack component, assume width = 1mm,
C length = 0.5* frame inside perimeter, on centre line.
            call zsurfprm(iz,is,dupedges,perim)
            write(name,'(a,i2.2,a,i3.3)') 'GrCRz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' grill crack width 1mm length',perim*0.5,' COG @XYZ',
     &        xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF120I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(13); LTPCMP(NCMP)=LVALCM(13)  !crack (120)
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=1.0/1000.0
            SUPCMP(NCMP,3)=perim*0.5
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Cr',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary crack azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the crack
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0
              NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:5).eq.'INLET')then

C Grill inlet - a conduit (type 210) IVALCM(15), as in MF210I
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)
C     SUPCMP(ICMP,4) - conduit length (m)  ?? get from distance ??
C     SUPCMP(ICMP,5) - absolute wall roughness (m)
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)

C << deal with start/end points likely being different >>

            call zsurfprm(iz,is,dupedges,perim)
            write(name,'(a,i2.2,a,i3.3)') 'GrINz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' grill inlet hydr diam',
     &        4.0*SSNA(loop)/perim,' COG @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF110I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(15); LTPCMP(NCMP)=LVALCM(15) ! conduit (210)
            ISDCMP(NCMP)=6; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=4.0*SSNA(loop)/perim
            SUPCMP(NCMP,3)=SSNA(loop)
            SUPCMP(NCMP,4)=1.0
            SUPCMP(NCMP,5)=1.00000E-04
            SUPCMP(NCMP,6)=6.00000E-03
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cd','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-IN',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary inlet azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0
              NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:7).eq.'EXTRACT')then

C Grill extract - constant vol flow comp. (type 30) IVALCM(8), as in MF030I
            write(name,'(a,i2.2,a,i3.3)') 'GrEXz',iz,':',is
            call zsurfprm(iz,is,dupedges,perim)
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' grill extract hydr diam',
     &        4.0*SSNA(loop)/perim,' COG @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF110I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(8); LTPCMP(NCMP)=LVALCM(8) ! constant vol (30)
            ISDCMP(NCMP)=2; ISDCNN(NCMP)=0
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            SUPCMP(NCMP,1)=1.

C Offer shortcut definition.
  42        write(prompt,'(3a)') 'Derive flow through ',CMNAM(NCMP),
     &        ' (m^3/s) via:'
            CALL EASKABCD(prompt,' ','direct edit','l/s/person',
     &        'm^3/hr','ach (zone volume)',IW,nbhelp)
            if(iw.eq.1)then

C Flow m^3/s (zero flow allowed).
              VAL=SUPCMP(NCMP,2)
              write(prompt,'(3a)') 'Flow through ',CMNAM(NCMP),
     &          ' (m^3/s) ?'
   20         CALL EASKR(VAL,prompt,'(help for synopsis)',
     &          0.,'W',0.,'-',1.,
     &          'flow in comp',IER,nbhelp)
              IF(IER.NE.0) GOTO 20
              SUPCMP(NCMP,2)=VAL
            elseif(iw.eq.2)then

C Derive from litres/second/person and nb of people.
              write(prompt,'(3a,F8.5,a)') ' The current flow of ',
     &          CMNAM(NCMP),' (m^3/s) is ',SUPCMP(NCMP,2),'.'
              call edisp(iuout,prompt)
              hold = '  6.0    1.0 '
  43          CALL EASKS(hold,' litres/sec/person & number of people?',
     &          '(help for synopsis)',32,' 6.0 1 ','l/s/p',IER,nbhelp)
              K=0
              CALL EGETWR(hold,K,VAL2,0.1,99.,'W','l/s/p',IER)
              CALL EGETWR(hold,K,VAL3,0.1,999.,'W','number people',IER)
              IF(IER.NE.0) GOTO 43

              VAL= (val2/1000.0) * val3
              write(prompt,'(3a,F8.5,a)') ' The flow through ',
     &          CMNAM(NCMP),' (m^3/s) is now ',VAL,'.'
              dok=.true.
              call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
              if (.NOT.OK) goto 42
              SUPCMP(NCMP,2)=VAL
            elseif(iw.eq.3)then

C Convert any existing value to m3/hr
              write(prompt,'(3a,F8.5,a)') ' The current flow of ',
     &          CMNAM(NCMP),' (m^3/s) is ',SUPCMP(NCMP,2),'.'
              call edisp(iuout,prompt)
              VAL2=SUPCMP(NCMP,2)*3600.0
              write(prompt,'(3a)') 'Flow through ',CMNAM(NCMP),
     &          ' (m^3/hr) ?'
   21         CALL EASKR(VAL2,prompt,'(help for synopsis)',
     &          0.,'W',0.,'-',10.,
     &          'flow m3/hr in comp',IER,nbhelp)
              IF(IER.NE.0) GOTO 21
              if(val2.gt.0.0) VAL = (val2/3600.0)
              write(prompt,'(3a,F8.5,a)') ' The flow through ',
     &          CMNAM(NCMP),' (m^3/s) is now ',VAL,'.'
              dok=.true.
              call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
              if (.NOT.OK) goto 42
              SUPCMP(NCMP,2)=VAL
            elseif(iw.eq.4)then

C Based on ach against the volume of a particular zone.
              write(prompt,'(3a,F8.5,a)') ' The current flow of ',
     &          CMNAM(NCMP),' (m^3/s) is ',SUPCMP(NCMP,2),'.'
              call edisp(iuout,prompt)
              CALL EPICKS(IZN,IVALS2,' ',OUTS,12,NCOMP,zname,
     &          'zone list',IER,nbhelp)
              write(prompt,*)'Associated volume is ',VOL(IVALS2(1))
              call edisp(iuout,prompt)
              if(SUPCMP(NCMP,2).lt.0.001) val2=1.0
              write(prompt,'(3a)') 'Air changes for ',CMNAM(NCMP),
     &          ' (ach) ?'
   22         CALL EASKR(VAL2,prompt,'(help for synopsis)',
     &          0.,'W',0.,'-',1.,'flow ach in comp',IER,nbhelp)
              IF(IER.NE.0) GOTO 22
              VAL = (SUPNOD(NCMP,2) * VAL2)/3600.0
              write(prompt,'(3a,F8.5,a)') ' The flow through ',
     &          CMNAM(NCMP),' (m^3/s) is now ',VAL,'.'
              dok=.true.
              call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
              if (.NOT.OK) goto 42
              SUPCMP(NCMP,2)=VAL
            endif
            call symbol_flow_cmp(name,'bx','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-EX',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary extract azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from zone to this boundary node via extract
C and then to the adjacent zone.
              POS=ICAAS(IZ); NEG=NNOD; CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! an extract has zero height differences
              HGTNE(NCNN)=0.0   ! an extract has zero height differences
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Grill opening - common orifice component, (type 40) IVALCM(10) MF040I 
            call zsurfprm(iz,is,dupedges,perim)
            write(name,'(a,i2.2,a,i3.3)') 'GrOPz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f6.2)') name,
     &        ' grill orifice area',SSNA(loop),' COG @XYZ',
     &        xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10) ! common orifice
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=SSNA(loop)
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.65
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Op',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary opening azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          endif
        elseif(SSUSE(loop,1)(1:6).eq.'WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'D-WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'S-WINDOW'.or.
     &    SSUSE(loop,1)(1:8).eq.'C-WINDOW')then
          if(SSUSE(loop,2).eq.'CLOSED')then
            continue   ! no flow
          elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Window crack - crack component, assume width = 1mm,
C length = window width, at surface COG.
            call zsurfprm(iz,is,dupedges,perim)
            write(name,'(a,i2.2,a,i3.3)') 'WiCrz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' window crack width 1mm length',XYMAX,' @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF120I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(13); LTPCMP(NCMP)=LVALCM(13) !crack
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=1.0/1000.0
            SUPCMP(NCMP,3)=XYMAX
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf lower.
            write(outs,'(a,f7.3)') 'Delta Z with surf lower is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Cr',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary crack azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the crack
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Window open - common orifice component, (type 40) IVALCM(10) MF040I 
C If width/height >1: 
C wider than it is tall, assume width = 0.25* window width,
C height = 1* window height, on the centre line
C If width/height <=1: taller than it is wide
C assume width = 1* window width,
C height = 0.25* window height, at lowest horizontal edge. OK.
            write(name,'(a,i2.2,a,i3.3)') 'WiOpz',iz,':',is
            if((XYMAX/ZMAX).gt.1.0)then
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &          ' window open width ',XYMAX*0.25,' height',ZMAX,
     &          ' area',((XYMAX*0.25)*ZMAX),'m2 COG @Z',xc,yc,zc
              VAL=(XYMAX*0.25)*ZMAX
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &          ' window open width ',XYMAX,' height',ZMAX*0.25,
     &          ' area',(XYMAX*(ZMAX*0.25)),'m2 COG @Z',xc,yc,zc
              VAL=XYMAX*(ZMAX*0.25)
            endif
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10) ! common orifice
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.5
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            call edisp(iuout,outs)
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)
    
C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Op',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary opening azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:4).eq.'SASH')then

C Window sash - 2 common orifice component, (type 40) IVALCM(10) MF040I
C Ror first, assume width = window width,
C height = 0.125* window height, at highest horizontal edge
C For second, assume width = window width,
C height = 0.125* window height, at lowest horizontal edge
            write(6,*) 'lower sash W H llpos lrpos ulpos urpos',
     &         xymax,zmax,llpos,lrpos,ulpos,urpos
            write(name,'(a,i2.2,a,i3.3)') 'WiSLz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f6.2)') name,
     &        ' width ',XYMAX,' height',ZMAX*0.125,
     &        ' area',(XYMAX*(ZMAX*0.125)),'m2 lower @XYZ',xc,yc,zc
            VAL=XYMAX*(ZMAX*0.125)
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10)  ! common orifice opening
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.5
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)
    
C Report on Delta Height with associated surf lower.
            write(outs,'(a,f7.3)') 'Delta Z with surf lower is',
     &        edgelower-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)

C If at boundary setup the lower boundary node.
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-SL',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+edgelower
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary sash azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the lower orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with orifice
              HGTNE(NCNN)=edgelower-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined lower orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

C Now create the upper orifice.
            write(6,*) 'upper sash W H llpos lrpos ulpos urpos',
     &         xymax,zmax,llpos,lrpos,ulpos,urpos
            write(name,'(a,i2.2,a,i3.3)') 'WiSUz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgeupper
            write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f6.2)') name,
     &        ' width ',XYMAX,' height',ZMAX*0.125,
     &        ' area',(XYMAX*(ZMAX*0.125)),'m2 upper @XYZ',xc,yc,zc
            VAL=XYMAX*(ZMAX*0.125)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10)  ! common orifice opening
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.5
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)
    
C Report on Delta Height with associated surf upper.
            write(outs,'(a,f7.3)') 'Delta Z with surf upper is',
     &        edgeupper-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)

            if(ICT(loop).eq.0)then

C Now the boundary at the upper orifice.
              write(name,'(a,i2.2,a,i3.3)') 'BW-SU',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+edgeupper
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary sash azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the upper orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with orifice
              HGTNE(NCNN)=edgeupper-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined lower orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Window bidirectional - specific door component width & height 
C based on the surface width & height. discharge factor = 0.6,
C Draw on the centre line. The VAL4 is Surf COG to Zone COG.
            write(name,'(a,i2.2,a,i3.3)') 'WiBiz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=SSURCOG(loop,3)
            VAL1=XYMAX; VAL2=ZMAX
            VAL3=ABS(ZCOG(iz,3)-edgelower)  ! base to zone COG
            VAL4=SSURCOG(loop,3)-ZCOG(IZ,3)        ! surf COG to zone COG
            write(outs,'(2a,f6.1,a,f6.1,a,3f7.3,a,f6.3)') name,
     &        ' bidir width ',XYMAX,' height',ZMAX,
     &        ' coef 0.6 @XYZ',xc,yc,zc,
     &        ' delta base to zone COG',VAL3
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(14); LTPCMP(NCMP)=LVALCM(14) ! bi-directional
            ISDCMP(NCMP)=5; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL1
            SUPCMP(NCMP,3)=VAL2
            SUPCMP(NCMP,4)=VAL3
            if(SUPCMP(NCMP,5).lt.0.001) SUPCMP(NCMP,5)=0.6
            write(prompt,'(3a)') 'The discharge coefficient of ',
     &        CMNAM(NCMP),':'
            VAL2=SUPCMP(NCMP,5)
            CALL EASKR(VAL2,prompt,'  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,5)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'bi','r',xc,yc,zc)
            call pausems(100)
    
C Report on Delta Height with associated surf lower.
            write(outs,'(a,f7.3)') 'Delta Z with base is',VAL4
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Bi',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary opening azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the bi-directional
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with component
              HGTNE(NCNN)=VAL4   ! bi-dir assume surf COG to zone COG
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! <<  for similar creat another node ....
            endif

          endif
        elseif(SSUSE(loop,1)(1:4).eq.'FICT')then
          if(SSUSE(loop,2).eq.'CLOSED')then
            continue   ! no flow
          elseif(SSUSE(loop,2)(1:5).eq.'CRACK')then

C Fict crack - crack component, assume width = 1mm,
C length = window width, at lowest horizontal edge.
            write(name,'(a,i2.2,a,i3.3)') 'FiCRz',iz,':',is
            xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
            zc=edgelower
            write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' fict crack width ',XYMAX,' height 1mm @XYZ',xc,yc,zc
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF120I
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(13); LTPCMP(NCMP)=LVALCM(13)  !crack
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=1.0/1000.0
            SUPCMP(NCMP,3)=XYMAX
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'cr','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf lower.
            write(outs,'(a,f7.3)') 'Delta Z with surf lower is',
     &        edgelower-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Cr',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+edgelower
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary fict azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the crack
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with crack
              HGTNE(NCNN)=edgelower-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop))  ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:4).eq.'OPEN')then

C Fict opening - common orifice component, (type 40) IVALCM(10) MF040I 
C If width/height >1: wider than it is tall, assume
C width = 0.25* window width,
C height = 1* window height, on the centre line.
C If width/height <=1: taller than it is wide
C assume width = 1* window width,
C height = 0.25* window height, at lowest horizontal edge
            write(name,'(a,i2.2,a,i3.3)') 'FiOPz',iz,':',is
            if((XYMAX/ZMAX).gt.1.0)then
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &        ' fict open width ',XYMAX*0.25,' height',ZMAX,
     &        ' area',((XYMAX*0.25)*ZMAX),'m2 COG @Z',xc,yc,zc
              VAL=(XYMAX*0.25)*ZMAX
            else
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,f6.1,a,f6.2,a,3f7.3)') name,
     &        ' fict open width ',XYMAX,' height',ZMAX*0.25,
     &        ' area',(XYMAX*(ZMAX*0.25)),'m2 COG @Z',xc,yc,zc
              VAL=XYMAX*(ZMAX*0.25)
            endif
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(10); LTPCMP(NCMP)=LVALCM(10) ! common orifice
            ISDCMP(NCMP)=3; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL
            if(SUPCMP(NCMP,3).lt.0.001) SUPCMP(NCMP,3)=0.5
            write(prompt,'(3a,F8.5,a)') 
     &        'The current discharge coef of ',
     &        CMNAM(NCMP),' is ',SUPCMP(NCMP,3),'.'
            call edisp(iuout,prompt)
            VAL2=SUPCMP(NCMP,3)
            CALL EASKR(VAL2,'Discharge coefficient (see help)',
     &        '  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,3)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'or','r',xc,yc,zc)
            call pausems(100)

C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with surf COG is',
     &        SSURCOG(loop,3)-ZCOG(IZ,3)
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-Op',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary opening azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the orifice
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with orifice
              HGTNE(NCNN)=SSURCOG(loop,3)-ZCOG(IZ,3)
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined crack
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,zc)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif

          elseif(SSUSE(loop,2)(1:5).eq.'BIDIR')then

C Fict bi-directional - door component IVALCM(14)=130 MF130I
C Make is slightly smaller than the opening.
C width = 0.75* width, height = 0.75*height, discharge factor = 0.4
C Need to draw component at surf COG but the real base is at bottom
C edge.
            write(name,'(a,i2.2,a,i3.3)') 'FiBIz',iz,':',is
              xc=SSURCOG(loop,1); yc=SSURCOG(loop,2) ! remember
              zc=edgelower
              write(outs,'(2a,f6.1,a,f6.1,a,3f7.3,a,f6.3)') name,
     &        ' fict bidir width ',XYMAX*0.75,' height',ZMAX*0.75,
     &        ' coef 0.6 base @XYZ',xc,yc,zc,
     &        ' delta base to zone COG',VAL3
            VAL1=XYMAX*0.75; VAL2=ZMAX*0.75
            VAL3=ABS(ZCOG(iz,3)-edgelower)  ! base to zone COG
            VAL4=SSURCOG(loop,3)-ZCOG(IZ,3)        ! surf COG to zone COG
            useit=.true.
            write(6,*) CXSTR(1:lnblnk(CXSTR))
            write(6,*) outs(1:lnblnk(outs))
            call edisp(iuout,CXSTR)
            call edisp(iuout,outs)
            NCMP=NCMP+1 ! use logic in MF040I 
            CMNAM(NCMP)=name
            CMPASSOC(NCMP,1)=ZNAME(IZ); CMPASSOC(NCMP,2)=SN
            ITPCMP(NCMP)=IVALCM(14); LTPCMP(NCMP)=LVALCM(14) ! bi-directional
            ISDCMP(NCMP)=5; ISDCNN(NCMP)=0
            SUPCMP(NCMP,1)=1.
            SUPCMP(NCMP,2)=VAL1
            SUPCMP(NCMP,3)=VAL2
            SUPCMP(NCMP,4)=VAL3
            if(SUPCMP(NCMP,5).lt.0.001) SUPCMP(NCMP,5)=0.5
            write(prompt,'(3a)') 'The discharge coefficient of ',
     &        CMNAM(NCMP),':'
            VAL2=SUPCMP(NCMP,5)
            CALL EASKR(VAL2,prompt,'  ',0.,'W',0.,'-',10.,
     &        'discharge coef in comp',IER,nbhelp)
            SUPCMP(NCMP,5)=VAL2
            NWPCMP(NCMP)=0  ! start and end the same
            HCMP(NCMP,1,1)=xc; HCMP(NCMP,1,2)=yc; HCMP(NCMP,1,3)=zc
            call symbol_flow_cmp(name,'bi','r',xc,yc,VAL3)
            call pausems(100)
    
C Report on Delta Height with associated surf COG.
            write(outs,'(a,f7.3)') 'Delta Z with zone COG is',VAL4
            call edisp(iuout,outs)
            call pausems(100)
            if(ICT(loop).eq.0)then
              write(name,'(a,i2.2,a,i3.3)') 'BW-BI',iz,':',is
              xw=x3+SSURCOG(loop,1);yw=y3+SSURCOG(loop,2)
              zw=z3+SSURCOG(loop,3)
              write(outs,'(2a,f6.1,a,3f7.3)') name,
     &        ' Wind boundary fict azim',sspazi(loop),
     &        ' @XYZ',xw,yw,zw
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              NNOD=NNOD+1; NDTYP(NNOD)=3; NDFLD(NNOD)=1
              HNOD(NNOD,1)=xw; HNOD(NNOD,2)=yw; HNOD(NNOD,3)=zw
              SUPNOD(NNOD,2)=sspazi(loop)
              SUPNOD(NNOD,1)=1.0
              TNOD(NNOD)=0.0
              write(NDNAM(NNOD),'(a)') name
              NODASSOC(NNOD,1)=ZNAME(IZ); NODASSOC(NNOD,2)=SN
              call symbol_flow_nd(name,'bw','r',xw,yw,zw)
              call pausems(100)

C Establish connection from this boundary node to the bi-directional
C and then to the adjacent zone.
              POS=NNOD; NEG=ICAAS(IZ); CMPIS=NCMP; NCNN=NCNN+1

C Reality checks here...
              NODPS(NCNN)=POS; NODNE(NCNN)=NEG
              HGTPS(NCNN)=0.0   ! boundary always in line with component
              HGTNE(NCNN)=VAL4   ! bi-dir assume surf COG to zone COG
              NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
              ITPCON(NCNN)=CMPIS ! use the just-defined orifice
              write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &          NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
              write(6,*) outs(1:lnblnk(outs))
              call edisp(iuout,outs)
              call draw_flow_link(xw,yw,zw,xc,yc,VAL3)
              call draw_flow_link(xc,yc,zc,
     &          HNOD(NEG,1),HNOD(NEG,2),HNOD(NEG,3))
            elseif(ICT(loop).eq.3)then

C Define connection with adjacent zone. Check if linkatpartition is still
C zero. If so mark this connection and update the other surface as well. 
              if(linkatpartition(loop).eq.0)then
                POS=ICAAS(IC1(loop))  ! the current zone node is positive
                NEG=ICAAS(IC2(loop)) ! the other zone 
                CMPIS=NCMP; NCNN=NCNN+1
                linkatpartition(loop)=1
                icoth=IZSTOCN(IC2(loop),IE2(loop))
                linkatpartition(icoth)=1

C Reality checks here...
                NODPS(NCNN)=POS; NODNE(NCNN)=NEG
                HGTPS(NCNN)= HCMP(NCMP,1,3)-ZCOG(IZ,3)  ! diff comp & zone
                HGTNE(NCNN)= HCMP(NCMP,1,3)-ZCOG(IC2(loop),3)  ! diff comp & other zone
                NDSCNN(NCNN,1)=0; NDSCNN(NCNN,2)=0                 
                ITPCON(NCNN)=CMPIS ! use the just-defined crack
                write(outs,'(a,i3,6a)') 'add connection ',ncnn,' +nd',
     &            NDNAM(POS),' to ',NDNAM(NEG),' via cmp',CMNAM(CMPIS)
                write(6,*) outs(1:lnblnk(outs))
                call edisp(iuout,outs)
                call draw_flow_link(ZCOG(IZ,1),ZCOG(IZ,2),ZCOG(IZ,3),
     &            HCMP(NCMP,1,1),HCMP(NCMP,1,2),HCMP(NCMP,1,3))
                call draw_flow_link(ZCOG(IC2(loop),1),ZCOG(IC2(loop),2),
     &            ZCOG(IC2(loop),3),HCMP(NCMP,1,1),HCMP(NCMP,1,2),
     &            HCMP(NCMP,1,3))
              endif
            else
              continue  ! << for similar creat another node ....
            endif
          endif
        endif
        if(useit)then
          CALL ZSURLEHI(IZ,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
          Write(outs,'(a,2i3.3,a,f7.3,a,f7.3,a)') 'Z Surface width ',
     &      iz,IS,' =',XYMAX,'m, surface height=',ZMAX,
     &      'm (bounding box).'
          write(6,*) outs(1:lnblnk(outs))
          call edisp(iuout,outs)
        endif
      enddo
      haveglobal=.FALSE.  ! global preferences not yet available
      CALL MFWRIT(IUM)
      CALL ERPFREE(IUM,ISTAT)

C List what we have thus far.
      CALL MFLIST(iuout)

C Write alternative version of flow network here.
      CALL ERPFREE(IUM,ISTAT)
      CALL EFOPSEQ(IUM,NWKNAM,4,IER)
      call MF3DWRIT(IUM)

      call edisp(iuout,
     &  'Next task is to confirm links between the nodes & zones.')

C Link nodes and zones.
      DO 33 IZ=1,NCOMP
        if(ICAAS(IZ).gt.0)then
          write(outs,'(3A)')zname(IZ)(1:lnzname(IZ)),
     &      ' is currently linked to: ',NDNAM(ICAAS(IZ))
          helptopic='link_node_to_zone'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(outs,' ','accept',
     &      'select another','free link',iacc,nbhelp)
          if(iacc.eq.1)then
            goto 33
          elseif(iacc.eq.2)then
            IC=0
            call ASKRNOD(' available nodes','-',IC,IER)
            ICAAS(IZ)=IC
          else
            ICAAS(IZ)=0
          endif
        else
          helptopic='link_node_to_zone'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(outs,'(A,A)') zname(IZ)(1:lnzname(IZ)),
     &                        ' has no mass flow node.'
          CALL EASKAB(outs,' ','accept','select a node',iacc,nbhelp)
          if(iacc.eq.1)then
            ICAAS(IZ)=0
          elseif(iacc.eq.2)then
            IC=0
            call ASKRNOD(' available nodes','-',IC,IER)
            ICAAS(IZ)=IC
          endif
        endif
  33  CONTINUE
      IAIRN=3  ! so it shows up in cfg file.

C Update the configuration file so that connections and mass flow
C network are know.
      call edisp(iuout,' ')
      call edisp(iuout,' Updating configuration mass flow links...')
      CALL EMKCFG('-',IER)
      
      return
      end


