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

C ***************** MFCDAT
C Fills valid fluid flow component type arrays
      SUBROUTINE MFCDAT

#include "net_flow.h"
      COMMON/MFLOW6/IVALCM(MCMV)
      COMMON/MFLOW7/LVALCM(MCMV)

      CHARACTER*60 LVALCM
C String sizer:
C      123456789 123456789 123456789 123456789 123456789 1234567890
      IVALCM(1)=10
      LVALCM(1)=
     &'Power law vol. flow component       m = rho.a.dP^b          '
      IVALCM(2)=15
      LVALCM(2)=
     &'Power law mass flow component       m = a.dP^b              '
      IVALCM(3)=17
      LVALCM(3)=
     &'Power law mass flow component       m = a.rho^.5.dP^b       '
      IVALCM(4)=20
      LVALCM(4)=
     &'Quadratic law vol. flow component   dP= a.m/rho+b.(m/rho)^2 '
      IVALCM(5)=25
      LVALCM(5)=
     &'Quadratic law mass flow component   dP= a.m+b.m^2           '
      IVALCM(6)=30
      LVALCM(6)=
     &'Constant vol. flow rate component   m = rho.a               '
      IVALCM(7)=35
      LVALCM(7)=
     &'Constant mass flow rate component   m = a                   '
      IVALCM(8)=40
      LVALCM(8)=
     &'Common orifice vol. flow rate comp. m = rho.f(Cd,A,rho,dP)  '
      IVALCM(9)=50
      LVALCM(9)=
     &'Laminar pipe vol. flow rate comp.   m = rho.f(L,R,mu,dP)    '

      IVALCM(10)=110
      LVALCM(10)=
     &'Specific air flow opening           m = rho.f(A,dP)         '
      IVALCM(11)=120
      LVALCM(11)=
     &'Specific air flow crack             m = rho.f(W,L,dP)       '
      IVALCM(12)=130
      LVALCM(12)=
     &'Specific air flow door              m = rho.f(W,H,dP)       '
      IVALCM(13)=210
      LVALCM(13)=
     &'General flow conduit component      m = rho.f(D,A,L,k,SCi)  '
      IVALCM(14)=220
      LVALCM(14)=
     &'Conduit ending in converging 3-leg junction & Ccp = f(q/qc) '
      IVALCM(15)=230
      LVALCM(15)=
     &'Conduit starts in diverging  3-leg junction & Ccp = f(q/qc) '
      IVALCM(16)=240
      LVALCM(16)=
     &'Conduit ending in converging 4-leg junction & Ccp = f(q/qc) '
      IVALCM(17)=250
      LVALCM(17)=
     &'Conduit starts in diverging  4-leg junction & Ccp = f(q/qc) '
      IVALCM(18)=310
      LVALCM(18)=
     &'General flow inducer component      dP= a0+Sai(m/rho)^i     '
      IVALCM(19)=410
      LVALCM(19)=
     &'General flow corrector component    m = rho.f(comp, signal) '
      IVALCM(20)=420
      LVALCM(20)=
     &'Corrector with polynomial flow resistance     C = f(H/H100) '
      IVALCM(21)=460
      LVALCM(21)=
     &'Fixed flow rates controller                                 '
      IVALCM(22)=500
      LVALCM(22)=
     &'CFD component                                               '

      RETURN
      END
