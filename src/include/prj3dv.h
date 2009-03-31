C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2009.

      integer MPL !  Max. number of vertices in polygon structure
      PARAMETER  (MPL = 200)

C Viewing transformation matrices
      real TEMAT  ! model to eye matrix(4x4).
      real TSMAT  ! 4x4 transform
      real ETMMAT ! model to eye reverse matrix(4x4).
      real ETSMAT ! Eye to Screen Matrix.
      COMMON/DRWMAT/TEMAT(4,4),TSMAT(4,4),ETMMAT(4,4),ETSMAT(4,4)

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
