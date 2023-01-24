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

C ***************** MFCDAT
C Fills valid fluid flow component type arrays
      SUBROUTINE MFCDAT
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"

C String sizer:
C      123456789 123456789 123456789 123456789 123456789 1234567890
      IVALCM(1)=10
      LVALCM(1)=
     &'Power law vol. flow component       m = rho.a.dP^b          '
      LTVALCM(1)='Power law volume flow          '
      IVALCM(2)=11
      LVALCM(2)=
     &'Self regulating vent for 15 or 30 m3/h at 20 Pa             '
      LTVALCM(2)='Self regulating vent at 20 Pa  '
      IVALCM(3)=12
      LVALCM(3)=
     &'Pwr law vol. flow cmp w/ max flw. or dp max m = rho.a.dP^b  '
      LTVALCM(3)='Power law volume flow '
      IVALCM(4)=15
      LVALCM(4)=
     &'Power law mass flow component       m = a.dP^b              '
      LTVALCM(4)='Power law mass flow (c15)      '
      IVALCM(5)=17
      LVALCM(5)=
     &'Power law mass flow component       m = a.rho^.5.dP^b       '
      LTVALCM(5)='Power law mass flow (c17)      '
      IVALCM(6)=20
      LVALCM(6)=
     &'Quadratic law vol. flow component   dP= a.m/rho+b.(m/rho)^2 '
      LTVALCM(6)='Quadratic law volume flow      '
      IVALCM(7)=25
      LVALCM(7)=
     &'Quadratic law mass flow component   dP= a.m+b.m^2           '
      LTVALCM(7)='Quadratic law mass flow (c25)  '
      IVALCM(8)=30
      LVALCM(8)=
     &'Constant vol. flow rate component   m = rho.a               '
      LTVALCM(8)='Constant volume flow rate      '
      IVALCM(9)=35
      LVALCM(9)=
     &'Constant mass flow rate component   m = a                   '
      LTVALCM(9)='Constant mass flow rate        '
      IVALCM(10)=40
      LVALCM(10)=
     &'Common orifice flow component m = rho.f(Cd,A,rho,dP)        '
      LTVALCM(10)='Common orifice flow           '
      IVALCM(11)=50
      LVALCM(11)=
     &'Laminar pipe vol. flow rate comp.   m = rho.f(L,R,mu,dP)    '
      LTVALCM(11)='Laminar pipe volume flow rate '

      IVALCM(12)=110
      LVALCM(12)=
     &'Specific air flow opening           m = rho.f(A,dP)         '
      LTVALCM(12)='Specific air flow opening     '
      IVALCM(13)=120
      LVALCM(13)=
     &'Specific air flow crack             m = rho.f(W,L,dP)       '
      LTVALCM(13)='Specific air flow crack       '
      IVALCM(14)=130
      LVALCM(14)=
     &'Specific air flow door              m = rho.f(W,H,dP)       '
      LTVALCM(14)='Specific air flow door        '
      IVALCM(15)=210
      LVALCM(15)=
     &'General flow conduit component      m = rho.f(D,A,L,k,SCi)  '
      LTVALCM(15)='General flow conduit          '
      IVALCM(16)=211
      LVALCM(16)=
     &'Cowls and roof outlets (typical ceramic unit)               '
      LTVALCM(16)='Cowls and roof outlets        '
      IVALCM(17)=220
      LVALCM(17)=
     &'Conduit ending in converging 3-leg junction & Ccp = f(q/qc) '
      LTVALCM(17)='Conduit ending conv 3-leg jct '
      IVALCM(18)=230
      LVALCM(18)=
     &'Conduit starts in diverging  3-leg junction & Ccp = f(q/qc) '
      LTVALCM(18)='Conduit starts divg 3-leg jct '
      IVALCM(19)=240
      LVALCM(19)=
     &'Conduit ending in converging 4-leg junction & Ccp = f(q/qc) '
      LTVALCM(19)='Conduit ending conv 4-leg jct '
      IVALCM(20)=250
      LVALCM(20)=
     &'Conduit starts in diverging  4-leg junction & Ccp = f(q/qc) '
      LTVALCM(20)='Conduit starts divg 4-leg jct '
      IVALCM(21)=310
      LVALCM(21)=
     &'General flow inducer component      dP= a0+Sai(m/rho)^i     '
      LTVALCM(21)='General flow inducer component'
      IVALCM(22)=410
      LVALCM(22)=
     &'General flow corrector component    m = rho.f(comp, signal) '
      LTVALCM(22)='General flow corrector comp   '
      IVALCM(23)=420
      LVALCM(23)=
     &'Corrector with polynomial flow resistance     C = f(H/H100) '
      LTVALCM(23)='Corrector polynomial flow rest'
      IVALCM(24)=460
      LVALCM(24)=
     &'Fixed flow rates controller                                 '
      LTVALCM(24)='Fixed flow rates controller   '
      IVALCM(25)=500
      LVALCM(25)=
     &'Multi configuration component                               '
      LTVALCM(25)='Multi configuration component '

      RETURN
      END
