C This header relates to geometric entities in ESP-r. It is
C dependant on building.h and include statements to geometry.h should
C follow building.h so that parameters will have been defined.

C ctype (4 char) - zone shape type (REC, REG or GEN) see egomin for definition
C gversion (real) version of the geometry file (1.0 legacy, 1.1 current).
C igupgrade (integer) 0 = do nothing, 1 = leave at 1.0, 2 = upgrade to 1.1
      character CTYPE*4
      real gversion
      integer igupgrade
      common/g0/CTYPE(MCOM),gversion(MCOM),igupgrade

C NGL - contains the number of default windows in each face. As this
C       data structure is depreciated it should be removed.
      integer ngl        
      common/g2/ngl(MS)


