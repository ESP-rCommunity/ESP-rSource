C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file contains parameters and common blocks for the FMI
C implementation in ESP-r.

C NOTE: This header requires "building.h" to be imported first.

      include "FMIrefs.h"

C Parameter type declarations.
      integer MFMU,MFMUI,MFMUO,MFMUIS,MFMUSUP

C Parameter values.
      PARAMETER (MFMU=1)     !- Maximum number of FMUs
      PARAMETER (MFMUI=110)  !- Maximum number of FMU inputs per FMU
      PARAMETER (MFMUO=110)  !- Maximum number of FMU outputs per FMU
      PARAMETER (MFMUIS=82)  !- Maximum number of instances for each FMU.
      PARAMETER (MFMUSUP=1)  !- Maximum number of supplementary data items
C                               for an input or output.

C Common blocks.
C     IS_FMU - Logical flag indicating if there is at least one valid FMU.
C     FMISFL - Path to FMI specification file.
      common/FMIFLG/IS_FMU,FMISFL
      logical IS_FMU
      character FMISFL*72

C FMIFMU contains general data for coupled FMUs.
C     FMUFIL - Path to .fmu files.
C     FMUTO  - FMU timeout in ms.
C     FMULOG - Integer flag indicating whether debug logging is active.
C     FMUNOF - Number of FMUs.
C     FMUNIS - Number of instances needed for each fmu.
C     FMUINS - Names of FMU instances.
C     FMUDSC - Brief description string.
      common/FMIFMU/FMUNOF,FMUTO(MFMU),FMULOG(MFMU),FMUNIS(MFMU),
     &              FMUFIL(MFMU),FMUINS(MFMU,MFMUIS),FMUDSC(MFMU)
      character FMUFIL*72,FMUINS*30,FMUDSC*30
      integer FMULOG,FMUNIS,FMUNOF
      real FMUTO

C FMII contains data for FMU input directives; variables to be passed
C from ESP-r to an FMU.
C     FMUNUMI - Number of FMU inputs
C     FMUIZON - ESP zone number (0 = ambient)
C     FMUIVAR - ESP variable reference (see FMIrefs.h)
C     FMUIINS - FMU instance name
C     FMUIVNM - FMU variable name
C     FMUISUP - Supplementary data 
C     FMUIRFN - Reference number.
      common/FMII/FMUNUMI(MFMU),FMUIZON(MFMU,MFMUI),FMUIVAR(MFMU,MFMUI),
     &            FMUIINS(MFMU,MFMUI),FMUIVNM(MFMU,MFMUI),
     &            FMUISUP(MFMU,MFMUI,MFMUSUP),FMUIRFN(MFMU,MFMUI)
      integer FMUNUMI,FMUIZON,FMUIVAR,FMUIRFN
      character FMUIINS*30,FMUIVNM*32
      real FMUISUP

C FMIO contains data for FMU output directives; variables to be passed
C from an FMU to ESP-r.
C     FMUNUMO - Number of FMU outputs
C     FMUOZON - ESP zone number (0 = ambient)
C     FMUOVAR - ESP variable reference (see FMIrefs.h)
C     FMUOINS - FMU instance name
C     FMUOVNM - FMU variable name
C     FMUOSUP - Supplementary data
C     FMUORFN - Reference number.
      common/FMIO/FMUNUMO(MFMU),FMUOZON(MFMU,MFMUO),FMUOVAR(MFMU,MFMUO),
     &            FMUOINS(MFMU,MFMUO),FMUOVNM(MFMU,MFMUO),
     &            FMUOSUP(MFMU,MFMUO,MFMUSUP),FMUORFN(MFMU,MFMUI)
      integer FMUNUMO,FMUOZON,FMUOVAR,FMUORFN
      character FMUOINS*30,FMUOVNM*32
      real FMUOSUP

C FMIT contains data relating to the simulation time.
C     FMUTSA - Start time in seconds
C     FMUTSO - Stop time in seconds
C     FMUTSE - Step time in seconds
C     FMUTCU - Current time in seconds
      common/FMIT/FMUTSA(MFMU),FMUTSO(MFMU),FMUTSE(MFMU),FMUTCU(MFMU)
      real FMUTSA,FMUTSO,FMUTSE,FMUTCU

C FMICTL contains data for the implementation of FMU outputs. They are
C referenced by zone and by the output variable references defined in
C FMIrefs.h.
C     FMUCTL    - Control values suitable for implementation in ESP-r.
C     FMUDOCTL  - Logical specifying whether to implement control.
C     FMUCTLSUP - Supplementary data.
      COMMON/FMICTL/FMUCTL(MCOM,MFMIOREFS),FMUDOCTL(MCOM,MFMIOREFS),
     &              FMUCTLSUP(MCOM,MFMIOREFS,MFMUSUP)
      real FMUCTL,FMUCTLSUP
      logical FMUDOCTL

