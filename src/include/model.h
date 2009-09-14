C This header relates to high level model entities in ESP-r. It is
C dependant on building.h and should follow building.h so that
C parameters will have been defined.

C The title and summary of the model
      character modeltitle*72  ! title of the model (to replace LSNAM)
      character modeldocblock*248   ! text block of documentation for model
      common/modeltd/modeltitle,modeldocblock
