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

C This file contains dummy subroutines which may be used by the
C Makefile of bld when this is NOT coupled to dfs, in order to meet 
C its needs without taking on board the complete counterparts of:
C  DFSPER,  MZMRX5,  CFDDTA,  CFMNGE

C Contains:
C DFSPER - sets up the period in which the bps-dfs calculation is active.
C MZMRX5 - solver for the simultaneous thermal coupling of bps and dfs.
C CFDDTA - reads a dfs configuration file.
C CFMNGE - is the manager of the dfs calculations.
C SURFCONF - Calls CFMNGE each time step there is surface conflation.


C ********************* DFSPER *********************
C DFSPER - This version is a dummy for incorporation in bps-dfs' Makefile
C In its complete version, DFSPER sets up the period in which the 
C bps-dfs calculation is active.
      SUBROUTINE DFSPER

      RETURN
      END


C ********************* MZMRX5 *********************
C MZMRX5 - This version is a dummy for incorporation in bps-dfs' Makefile
C In its complete version, MZMRX5 is the solver for the simultaneous thermal 
C coupling of bps and dfs.
      SUBROUTINE MZMRX5(ic)

      WRITE(*,*) ' MZMRX5: CFD calculation is not supported in this',
     &                   ' bps version.'
 
      STOP ' MZMRX5: '

      END


C ********************* CFDDTA *********************
C CFDDTA - This version is a dummy for incorporation in bps-dfs' Makefile
C In its complete version, CFDDTA reads a dfs configuration file.
      SUBROUTINE CFDDTA(itrc,itu,ICOMP,IBLD,IMFN,IER)

      call edisp(itu,
     &  ' CFDDTA: CFD calculation is not supported in this version.')
      STOP ' CFDDTA '

      END


C ********************* CFMNGE *********************
C CFMNGE - This version is a dummy for incorporation in bps-dfs' Makefile
C In its complete version, CFMNGE is the manager of the dfs calculations.
      SUBROUTINE CFMNGE(IC,icfd)

      WRITE(*,*) ' CFMNGE: CFD calculation is not supported in this',
     &                   ' bps version.'
 
      STOP ' CFMNGE '

      END


C ********************* SURFCONF *********************
C SURFCONF - This version is a dummy for incorporation in bld's Makefile
C In its complete version, SURFCONF calls CFMNGE each time step there is
C surface conflation.
      SUBROUTINE SURFCONF(ICOMP,mHCcfd)

      WRITE(*,*) ' SURFCONF: CFD calculation is not supported in this',
     &                   ' bld version.'
 
      STOP ' SURFCONF '

      END
