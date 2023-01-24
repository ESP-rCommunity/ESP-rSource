C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2010.

C Note this include file must be referenced after building.h
C (it uses some parameters from building.h).

      integer MPL !  Max. number of vertices in polygon structure
      PARAMETER  (MPL = 200)

C Viewing transformation matrices (all 4x4)
      real TEMAT  ! model to eye
      real TSMAT  ! model to screen
      real ETMMAT ! eye to model 
      real ETSMAT ! eye to screen
      real STMMAT ! screen to model
      COMMON/DRWMAT/TEMAT(4,4),TSMAT(4,4),ETMMAT(4,4),ETSMAT(4,4),
     &              STMMAT(4,4)

C View frustrum clip flags
      real TPCLIP    ! ?? clip in 3 axis
      integer IFCLIP ! flag denoting whether clipping is active
      real CLIPLN    ! equation for each of the 6 planes of the frustrum
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

C View coordinates
      integer IMT    ! does not seem to be used
      real EYEM      ! X Y Z of the eye point
      real VIEWM     ! X Y Z of the viewed point
      real HITH      ! nearest clipping distance
      real YON       ! farthest clipping distance
      real ANG       ! angle of view (degrees)
      real HANG      ! half angle of view (degrees)
      real WIDE      ! does not seem to be used
      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

      integer ITDSP  ! labels: all surf+obs+vis = 0, all surf = 1, ext = 2,
                     ! partn = 3, similar = 4, surfs+obs+ground = 5, ground only = 6
                     ! surf+obs = 7, surf+vis = 8
      integer ITBND  ! zone bounds toggle: static = 0, optimum = 1,
                     ! zone focus = 2
      integer ITEPT  ! is not yet used.
      integer ITZNM  ! zone name: display = 0, hidden = 1
      integer ITSNM  ! surface name: display = 0, hidden = 1
      integer ITORG  ! site origin: display = 0, hidden = 1
      integer ITSNR  ! surf normal: display = 0, hidden = 1
      integer ITOBS  ! obstruction: display = 0, hidden = 1
      integer ITVIS  ! visual entities: highlight if non-zero
      integer ITVOBJ ! visual compound objects: highlight if non-zero
      integer ITHLS  ! highlight: normal 0, constr 1, trans/opaq 2, part atrib 3
      integer ITHLZ  ! additional qualifier for ITHLS.
      integer ITGRD  ! grid: display = 0, hidden = 1
      integer ITVNO  ! vertex: display = 0, hidden = 1
      integer ITPPSW ! current view - perspective/plan/south/west
      real GRDIS     ! grid distance
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITVIS,ITVOBJ,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

C Keep track of when wireframe view needs to be updated or adjusted.
      logical MODIFYVIEW ! modify the view
      logical MODLEN ! modify the viewing lens
      logical MODBND ! modify the view boundary
      COMMON/RAY3/MODIFYVIEW,MODLEN,MODBND

C Zone bounds.
      real ZCOG   ! zone centre of gravity
      real XMN,YMN,ZMN  ! minimum in each axis
      real XMX,YMX,ZMX  ! maximum in each axis
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX

      integer LINSTY    ! line type for each surface in the model
      COMMON/RAY6/LINSTY(MCON)

C Zone bounds (minimum and maximum for each axis.
      real ZXMN,ZYMN,ZZMN ! the zone minimum bounds (m)  for X Y Z axis.
      real ZXMX,ZYMX,ZZMX ! the zone maximum bounds (m)  for X Y Z axis.
      integer iZBFLG  ! zero bounds must be recalculated, ZBFLG is one use current value.
      COMMON/RAY7/ZXMN(MCOM),ZYMN(MCOM),ZZMN(MCOM),ZXMX(MCOM),
     &            ZYMX(MCOM),ZZMX(MCOM),iZBFLG(MCOM)
