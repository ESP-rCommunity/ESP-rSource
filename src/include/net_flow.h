C This include must be placed after building.h in the source code.
C Include file for network flow.
C Updated December 2005 for additional nodes, components and control laws.
C Updated January 2007 for additional connections.
C Updated June 2008 for "implicit none" callability
C Updated August 2019 to use building.h parameters.
C Updated September 2020 to use support more flow items to include an graphs.

C Type declarations:
      integer MNOD, MCMP, MCNN, MNDS, MCMV, MCMS, MCNS
      integer MPOS, MPRD, MPCP,    NMRL
      integer MFCNLST, MFCNEL, MFRS, MCONTM, MPRODI, MCSD
      integer MSPMNO, MSRC

C Maximum number of nodes, components, connections.
      PARAMETER (MNOD=MCOM*4,MCMP=MCOM*4,MCNN=MCOM*4)
C Maximum number of node supplementary data items
      PARAMETER (MNDS=2)
C Number of valid fluid flow component types
      PARAMETER (MCMV=25)
C Maximum number of component supplementary data items
      PARAMETER (MCMS=17)
C Maximum number of connection level component supplementary items
      PARAMETER (MCNS=2)
C Number of wind pressure coefficients in one set
      PARAMETER (MPOS=16)
C Maximum number of wind pressure distribution sets
      PARAMETER (MPRD=50)
C Maximum number of CPCALC derived pressure distribution sets
      PARAMETER (MPCP=20)
C Record width of binary results file (should be larger of MNOD or MCMS + 6)
      PARAMETER (NMRL=MCOM*3)
C Results analysis:
C Max number of connection lists and number of connections allowed.
      PARAMETER (MFCNLST=20)
      PARAMETER (MFCNEL=12)
C Max items to display
      PARAMETER (MFRS=24)
C Max number of contaminants, this is equal to parameter MCTM in
C cfd.h if MCONTM is changed change MCTM accordingly
      PARAMETER (MCONTM=4)
      PARAMETER (MPRODI=MCONTM*MNOD)
C Max number of contaminant supplementary data items
      PARAMETER (MCSD=9)
C Max number of sources/sinks
      PARAMETER (MSPMNO=30)
C Max number of contaminant source/sink types
      PARAMETER (MSRC=6)
