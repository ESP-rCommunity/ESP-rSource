! This include file should follow building.h in the source code.
! NTCELX, NTCELY, NTCELZ, and MCEL1D define the resolution of the CFD
! domain. These variables significantly influence the size of dfs and
! bps and are set in building.h. The other variables have little
! impact on the size of dfs and bps.

! Maximum number of new mfs connections created for CFD domain (usual setting MCFND=20).
      INTEGER, PARAMETER :: MCFND=20
! Maximum number of heat source (usual setting MNHS=36).
      INTEGER, PARAMETER :: MNHS=36
! Maximum number of solid boundaries per zone
! (set equal to 2* or greater than MS in building.h, because of current
! method of specification of solid boundary conditions from building
! surfaces).
      INTEGER, PARAMETER :: MNSBZ=MS*2
! Maximum number of zones with CFD (usual setting MNZ=4).
      INTEGER, PARAMETER :: MNZ=4
! Maximum number of key volumes (usual setting MNVLS=120).
      INTEGER, PARAMETER :: MNVLS=350
! Maximum number of contaminants that can be modelled (Set equal to
! INTEGER, PARAMETER :: MCONTM in net_flow.h, if this is changed
! also change MCONTM).
      INTEGER, PARAMETER :: MCTM=4
! Maximum frequency of residuals plotting during CFD solutions.
      INTEGER, PARAMETER :: MFRP=1000
! Maximum number of cells to potentially refine gridding.
      INTEGER, PARAMETER :: MRFN=NTCELX*NTCELY*NTCELZ/1000
! Maximum number of times we might refine in total.
      INTEGER, PARAMETER :: MRFNT=NTCELX*NTCELY*NTCELZ


