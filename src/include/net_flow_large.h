C Include file for network flow.
C Updated December 2005 for additional nodes, components and control laws.
C Updated January 2007 for additional connections.
C Updated June 2008 for "implicit none" callability

C Type declarations:
      integer MNOD, MCMP, MCNN, MNDS, MCMV, MCMS, MCNS
      integer MPOS, MPRD, MPCP,    NMRL
      integer MFCNLST, MFCNEL, MFRS, MCONTM, MPRODI, MCSD
      integer MSPMNO, MSRC

C Maximum number of nodes, components, connections.
      PARAMETER (MNOD=80,MCMP=80,MCNN=199)
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
      PARAMETER (NMRL=70)
C Results analysis:
C Max number of connection lists and number of connections allowed.
      PARAMETER (MFCNLST=10)
      PARAMETER (MFCNEL=5)
C Max items to display
      PARAMETER (MFRS=20)
C Max number of contaminants, this is equal to parameter MCTM in
C cfd.h if MCONTM is changed change MCTM accordingly
      PARAMETER (MCONTM=4)
      PARAMETER (MPRODI=MCONTM*MNOD)
C Max number of contaminant supplementary data items
      PARAMETER (MCSD=9)
C Max number of sources/sinks
      PARAMETER (MSPMNO=20)
C Max number of contaminant source/sink types
      PARAMETER (MSRC=6)
