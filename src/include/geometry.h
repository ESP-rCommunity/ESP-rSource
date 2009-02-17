C This header relates to geometric entities in ESP-r. It is
C dependant on building.h and include statements to geometry.h should
C follow building.h so that parameters will have been defined.

C NGL - contains the number of default windows in each face. As this
C       data structure is depreciated it should be removed.
      integer ngl        
      common/g2/ngl(MS)


