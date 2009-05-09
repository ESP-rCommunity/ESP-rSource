C  NTCELX, NTCELY, NTCELZ, and MCEL1D define the resolution of the CFD
C  domain. These variables significantly influence the size of dfs and
C  bps. Most practical problems require at least 32 cells in each
C  direction. If CFD is not being used, these can be reduced to 12 to
C  reduce the size of bps. Note that a number of exemplars require
C  these variables to be sized to 32. The other variables have little
C  impact of the size of dfs and bps.
C
C Type declarations.
      integer NTCELX,NTCELY,NTCELZ,MCEL1D,MCFND,MNREG,MNHS
	  integer MNSBZ,MNZ,MNVLS,MCTM,MFRP

C  Maximum number of cells in x direction (usual setting NTCELX=32)
      PARAMETER (NTCELX=32)
C  Maximum number of cells in y direction (usual setting NTCELY=32)
      PARAMETER (NTCELY=32)
C  Maximum number of cells in z direction (usual setting NTCELZ=32)
      PARAMETER (NTCELZ=32)
C  Maximum number between ntcelx, ntcely and ntcelz (usual setting MCEL1D=32)
      PARAMETER (MCEL1D=32)
C  Maximum number of new mfs connections created for CFD domain (usual setting MCFND=10)
      PARAMETER (MCFND=10)
C  Maximum number of gridding regions per axis (usual setting MNREG=20)
      PARAMETER (MNREG=20)
C  Maximum number of heat source (usual setting MNHS=3)
      PARAMETER (MNHS=3)
C  Maximum number of solid boundaries per zone 
C (set equal to or greater than MS in building.h, because of current
C method of specification of solid boundary conditions from building
C surfaces)
      PARAMETER (MNSBZ=85)
C  Maximum number of zones with CFD (usual setting MNZ=1)
      PARAMETER (MNZ=1)
C  Maximum number of key volumes (usual setting MNVLS=120)
      PARAMETER (MNVLS=120)
C  Maximum number of contaminants that can be modelled (Set equal to
C  parameter MCONTM in net_flow.h, if this is changed also change MCONTM)
      PARAMETER (MCTM=4)
C Maximum frequency of residuals plotting during CFD solutions    
      PARAMETER (MFRP=100)

