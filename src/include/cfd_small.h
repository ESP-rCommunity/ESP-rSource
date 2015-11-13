C  NTCELX, NTCELY, NTCELZ, and MCEL1D define the resolution of the CFD
C  domain. These variables significantly influence the size of dfs and
C  bps. Most practical problems require at least 32 cells in each
C  direction. If CFD is not being used, these can be reduced to 12 to
C  reduce the size of bps. Note that a number of exemplars require
C  these variables to be sized to 32. The other variables have little
C  impact of the size of dfs and bps.
C Small version jwh may 2013
C
C  Maximum number of cells in x direction (usual setting NTCELX=14)
      INTEGER, PARAMETER :: NTCELX=18
C  Maximum number of cells in y direction (usual setting NTCELY=14)
      INTEGER, PARAMETER :: NTCELY=18
C  Maximum number of cells in z direction (usual setting NTCELZ=14)
      INTEGER, PARAMETER :: NTCELZ=18
C  Maximum number between ntcelx, ntcely and ntcelz (usual setting MCEL1D=14)
      INTEGER, PARAMETER :: MCEL1D=18
C  Maximum number of new mfs connections created for CFD domain (usual setting MCFND=20)
      INTEGER, PARAMETER :: MCFND=20
C  Maximum number of gridding regions per axis (usual setting MNREG=20)
      INTEGER, PARAMETER :: MNREG=20
C  Maximum number of heat source (usual setting MNHS=36)
      INTEGER, PARAMETER :: MNHS=36
C  Maximum number of solid boundaries per zone
C (set equal to 2* or greater than MS in building.h, because of current
C method of specification of solid boundary conditions from building
C surfaces)
      INTEGER, PARAMETER :: MNSBZ=160
C  Maximum number of zones with CFD (usual setting MNZ=4)
      INTEGER, PARAMETER :: MNZ=2
C  Maximum number of key volumes (usual setting MNVLS=120)
      INTEGER, PARAMETER :: MNVLS=120
C  Maximum number of contaminants that can be modelled (Set equal to
C  INTEGER, PARAMETER :: MCONTM in net_flow.h, if this is changed also change MCONTM)
      INTEGER, PARAMETER :: MCTM=4
C Maximum frequency of residuals plotting during CFD solutions
      INTEGER, PARAMETER :: MFRP=1000

