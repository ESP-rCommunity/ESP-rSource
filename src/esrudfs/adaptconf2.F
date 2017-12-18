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

C This file contains subroutines related to the `adaptive conflation controller'
C which controls how CFD is used by the thermal domain.

C ASSESS - Decides whether to pass CFD-calc conv coeff to thermal domain.

C ******************** ASSESS ********************
C ASSESS - Decides whether to pass the CFD-calculated convection coefficients
C to the thermal domain for inclusion in the zone matrix.

      SUBROUTINE ASSESS(ICOMP,ISUR)
#include "building.h"
#include "cfd.h"
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      common/htccfd/HCICFD(MCOM,MS)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),IVOLNSB(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)
      COMMON/ICFNOD/ICFD,ICP
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ypave/ypavg(MS)
      COMMON/ACCrec/IACC(MNZ)

      CHARACTER*72 LCFD
      LOGICAL laccept

C When conditional two-way adaptive conflation control is active, this
C subroutine examines the CFD predictions to decide whether or not to pass
C the CFD-predicted convection coefficients to the thermal domain. The routine
C is called once for each surface in the thermal model.
C When uncontrolled two-way surface conflation is active, it simply accepts the
C CFD-predicted convection coefficients and passes them to the thermal domain.
C When one-way surface conflation is active it takes no action, thus preserving
C the convection coefficients calculated by adaptive convection algorithm.

C First, make sure that CFD is active in the zone under consideration.

C Debug.
C      write(6,*) '5 ',ICP,ICOMP,icfd,IFCFD(ICP)

      IF( icfdnd(icfd).eq.ICOMP )THEN

C Determine which CFD solid boundary corresponds to the ESP-r surface under
C consideration. It is not a problem if numerous CFD solid boundaries correspond
C to the ESP-r surface, as consistency in BC types was ensured when the CFD
C data were read in and by the adaptive conflation controller.
        DO 10 L=1,NSB(ICFD)
          IS=ISUFLC(L,ICFD)
          IF(IS.eq.ISUR) THEN
            IF(abs(IFCFD(ICP)).eq.1)THEN
            
C Uncontrolled surface conflation. If one-way conflation on this CFD surface, then
C do nothing. If two-way, then accept CFD HCs.
              if(ITCtype(L,ICFD).ge.9 .and. ITCtype(L,ICFD).le.12)then
                HCIF(ICOMP,ISUR) = HCICFD(ICOMP,ISUR)
              endif
            ELSEIF(abs(IFCFD(ICP)).eq.4)THEN
            
C One-way adaptive conflation: do nothing as CFD HCs will not be used.
            ELSEIF(abs(IFCFD(ICP)).eq.5)THEN
            
C Conditional two-way adaptive conflation. Use three-stage screening process to
C decide whether to accept CFD HC for this surface.
              laccept = .true.

C If the co-operative 1-way Neumann condition was used, then cannot
C use CFD-calculated convection coefficient since convective heat
C transfer was imposed.
              if(ITCtype(L,ICFD).eq.5) laccept=.false.

C If the log-law thermal wall function with the Dirichlet conditions
C was used for the CFD surfaces corresponding
C to this thermal surface, then check gridding to see if next-to-wall points in a
C range that can give feasible surface convection estimates.
              if(ITCtype(L,ICFD).eq.9)then
                if(ypavg(ISUR).gt.10. .and. ypavg(ISUR).lt.30.)then
                  laccept=.true.
                else
                  laccept=.false.
                endif
              endif
              
C If CFD HC not within same range as HC calculated by adaptive
C convection algorithm, then reject CFD result. At this point HCIF has been
C calculated by the adaptive convection algorithm.
              ratio = HCICFD(ICOMP,ISUR) / HCIF(ICOMP,ISUR)
              if( ratio.lt.0.1 .or. ratio.gt.10.0 ) laccept=.false.
              
C Accept CFD HC if above conditions met.
              if(laccept)then
                HCtemp = HCIF(ICOMP,ISUR)
                HCIF(ICOMP,ISUR) = HCICFD(ICOMP,ISUR)              
              endif
              
C Record ACC actions (begin).
              write(IACC(icfd),*)
              write(IACC(icfd),*)
     &          '  ASSESS has accepted HC (true/false)  at ',
     &                         'surface?: ',ISUR,' : ',laccept
              write(IACC(icfd),*) '   HCIF = ',HCtemp,' HCICFD = ',
     &                          HCICFD(ICOMP,ISUR),' ratio = ',ratio
C Record ACC actions (end).

            ELSE
            
C Should not be in this subroutine for other conflation types.
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
      
      RETURN
      END

