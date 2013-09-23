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
      IVALCM(2)=11
      LVALCM(2)=
     &'Self regulating vent for 15 or 30 m3/h at 20 Pa             '
      IVALCM(3)=15
      LVALCM(3)=
     &'Power law mass flow component       m = a.dP^b              '
      IVALCM(4)=17
      LVALCM(4)=
     &'Power law mass flow component       m = a.rho^.5.dP^b       '
      IVALCM(5)=20
      LVALCM(5)=
     &'Quadratic law vol. flow component   dP= a.m/rho+b.(m/rho)^2 '
      IVALCM(6)=25
      LVALCM(6)=
     &'Quadratic law mass flow component   dP= a.m+b.m^2           '
      IVALCM(7)=30
      LVALCM(7)=
     &'Constant vol. flow rate component   m = rho.a               '
      IVALCM(8)=35
      LVALCM(8)=
     &'Constant mass flow rate component   m = a                   '
      IVALCM(9)=40
      LVALCM(9)=
     &'Common orifice flow component m = rho.f(Cd,A,rho,dP)        '
      IVALCM(10)=50
      LVALCM(10)=
     &'Laminar pipe vol. flow rate comp.   m = rho.f(L,R,mu,dP)    '

      IVALCM(11)=110
      LVALCM(11)=
     &'Specific air flow opening           m = rho.f(A,dP)         '
      IVALCM(12)=120
      LVALCM(12)=
     &'Specific air flow crack             m = rho.f(W,L,dP)       '
      IVALCM(13)=130
      LVALCM(13)=
     &'Specific air flow door              m = rho.f(W,H,dP)       '
      IVALCM(14)=210
      LVALCM(14)=
     &'General flow conduit component      m = rho.f(D,A,L,k,SCi)  '
      IVALCM(15)=211
      LVALCM(15)=
     &'Cowls and roof outlets (typical ceramic unit)               '
      IVALCM(16)=220
      LVALCM(16)=
     &'Conduit ending in converging 3-leg junction & Ccp = f(q/qc) '
      IVALCM(17)=230
      LVALCM(17)=
     &'Conduit starts in diverging  3-leg junction & Ccp = f(q/qc) '
      IVALCM(18)=240
      LVALCM(18)=
     &'Conduit ending in converging 4-leg junction & Ccp = f(q/qc) '
      IVALCM(19)=250
      LVALCM(19)=
     &'Conduit starts in diverging  4-leg junction & Ccp = f(q/qc) '
      IVALCM(20)=310
      LVALCM(20)=
     &'General flow inducer component      dP= a0+Sai(m/rho)^i     '
      IVALCM(21)=410
      LVALCM(21)=
     &'General flow corrector component    m = rho.f(comp, signal) '
      IVALCM(22)=420
      LVALCM(22)=
     &'Corrector with polynomial flow resistance     C = f(H/H100) '
      IVALCM(23)=460
      LVALCM(23)=
     &'Fixed flow rates controller                                 '
      IVALCM(24)=500
      LVALCM(24)=
     &'Multi configuration component                               '
     
      IVALCM(25)=600
      LVALCM(25)=
     &'AIM-2 model coupling component                              '
      IVALCM(26)=610
      LVALCM(26)=
     &'Simple circulation fan coupling component                   '     
      RETURN
      END
