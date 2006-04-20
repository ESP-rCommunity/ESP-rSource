C------------------------------------------------------------
C Version 12 August 1992.

C     MG_RAY  -  PARAMETERS AND COMMON BLOCKS

C     MSUR  =  Max. number of surfaces in view
C     MIP   =  Max. number of polygons in polygon structure
C     MPL   =  Max. number of vertices in polygon structure

C      PARAMETER  (MSUR = 500)
      PARAMETER  (MIP = 10,  MPL = 200 )

C Model parameters and file totals
C      COMMON/TOTALS/NSURF

C surface plane equations.
C      COMMON/PLANE/PEQN(4,MSUR)

C Viewing transformation matrices
C TEMAT is 
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

C View frustrum clip flags
      COMMON/CLIP/TPCLIP(3),IFCLIP(6),CLIPLN(6,4)

      COMMON/IMAGE/IMT,EYEM(3),VIEWM(3),HITH,YON,ANG,HANG,WIDE

C------------------------------------------------------------
