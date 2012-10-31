C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains the following routines for the MIT zero-eqn turbulence model:
C  MITlenV  Calculates local mean velocity and length scale.


C ********************* MITlenV *********************
C MITlenV  Calculates local mean velocity and length scale for the MIT 0-eqn model.
      SUBROUTINE MITlenV(I,J,K,rleng,Vmean)

#include "cfd.h"
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz) 
      COMMON/CONST/GREAT,small,GRAV
      common/INCALC/INCALU,INCALV,INCALW,INCALK,INCALD,INCALT,
     1              IZEROT,IZanKE,IMITZ

      LOGICAL INCALU,INCALV,INCALW,INCALT,INCALK,INCALD
      LOGICAL IZEROT,IZanKE,IMITZ

C This subroutine calculates the local mean velocity and the `length scale'
C for the CFD cell specified in the calling statement. These quantities are
C required to calculate the local eddy viscosity in the MIT zero-equation
C turbulence model. The following references describe the MIT zero-equation
C model:
C
C Chen Q. and Xu W. (1998), `A Zero-Equation Turbulence Model for Indoor
C Airflow Simulation', Energy and Buildings, 28 137-144.
C
C Xu W. (1998), `New Turbulence Models for Indoor Airflow Simulation',
C PhD Thesis, Massachusetts Institute of Technology, USA.


C Calculate velocity at `P' point. Interpolation required due to staggered grid.
C `P' point located mid-distance between the `U', `V', and `W' points, so
C velocity components at `P' point calculated by averaging velocities at
C neighbouring points. Then get magnitude of velocity at `P' using three
C components.
      UatP = 0.5*Uf(i,j,k) + 0.5*Uf(i+1,j,k)
      VatP = 0.5*Vf(i,j,k) + 0.5*Vf(i,j+1,k)
      WatP = 0.5*Wf(i,j,k) + 0.5*Wf(i,j,k+1)
      Vmean = SQRT(UatP**2 + VatP**2 + WatP**2)


C Calculate `length scale', equal to distance to nearest wall (Xu thesis, p.140).
C Note that proximity to "significant objects" (Chen, 1999, personal communication)
C should also be considered in determining the length scale. Therefore, this
C coding should be revised once blockages are fully implemented.
C First, determine distances from west, east, south, north, low, and high walls.
      dwest = XP(i)
      deast = XU(NI)-XP(i)
      dsouth = YP(j)
      dnorth = YV(NJ)-YP(j)
      dlow = ZP(k)
      dhigh = ZW(NK)-ZP(k)
C If we are in a fictitious cell, one or more of the distances will be negative.
C Set this length to zero, effectively giving an eddy viscosity of zero.
      if(dwest.lt.0.) dwest=0.
      if(deast.lt.0.) deast=0.
      if(dsouth.lt.0.) dsouth=0.
      if(dnorth.lt.0.) dnorth=0.
      if(dlow.lt.0.) dlow=0.
      if(dhigh.lt.0.) dhigh=0.
C If we are in a fictitious cell, one or more of the distances will be greater
C than the dimensions of the domain. Restrict the distances to the dimensions
C of the domain.
      if(dwest.gt.XU(NI)) dwest=XU(NI)
      if(deast.gt.XU(NI)) deast=XU(NI)
      if(dsouth.gt.YV(NJ)) dsouth=YV(NJ)
      if(dnorth.gt.YV(NJ)) dnorth=YV(NJ)
      if(dlow.gt.ZW(NK)) dlow=ZW(NK)
      if(dhigh.gt.ZW(NK)) dhigh=ZW(NK)
C If the problem is two-dimensional, do not consider the distances to the
C walls in the non-participating direction.
      if(.not.INCALU)then
        dwest = GREAT
        deast = GREAT
      endif
      if(.not.INCALV)then
        dsouth = GREAT
        dnorth = GREAT
      endif
      if(.not.INCALW)then
        dlow = GREAT
        dhigh = GREAT
      endif
C Now determine the distance to the nearest wall.
      rleng = AMIN1(dwest,deast,dsouth,dnorth,dlow,dhigh)

      RETURN
      END
