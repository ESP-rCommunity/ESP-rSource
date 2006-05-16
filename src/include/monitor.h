C Building plot parameters:
C mdph is the max data block per hour dumped to library file.
C Should equal MCOM*60. Where 60 is maximum number of time-steps
C per hour for the building.
      integer mdph
      parameter (mdph=1500)
