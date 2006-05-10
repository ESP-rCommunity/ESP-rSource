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

C ************** MFMODE 
C MFMODE lets the user, when
C     ICHNG=1  alter the iteration parameters
C     ICHNG=2  use a different stack pressure calculation model
C     ICHNG=3  use a different matrix solver
C     ICHNG=4  toggle the trace facility

      SUBROUTINE MFMODE(ICHNG)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      character outs*124,H*72
      logical OK,dok

      IF(ICHNG.LT.1.OR.ICHNG.GT.4) STOP ' MFMODE: unresolvable error !'
      IF(ICHNG.EQ.1) GOTO 10
      IF(ICHNG.EQ.2) GOTO 70
      IF(ICHNG.EQ.3) GOTO 80
      IF(ICHNG.EQ.4) GOTO 90

C Show and possibly reset iteration control parameters
   10 CALL EPAGE
      call edisp(iuout,
     &' There are a number of parameters which control')
      call edisp(iuout,
     &' the iterative fluid flows calculation process. They are:')
      call edisp(iuout,
     &' MAXITF : maximum number of iterations allowed for one time')
      write(outs,'(A,I4)')  '          step; currently:',MAXITF
      call edisp(iuout,outs)
      call edisp(iuout,
     &' FERREL : largest percentage residual flow error allowed in any')
      write(outs,'(A,F6.3,A)')
     &'          node; currently:',FERREL*100.0,'%'
      call edisp(iuout,outs)
      call edisp(iuout,
     &' FERMFL : largest absolute residual flow error allowed in any ')
      write(outs,'(A,F9.5,A)')
     &'          node; current value :',FERMFL,' kg/s. Iteration stops'
      call edisp(iuout,outs)
      call edisp(iuout,
     &'          if the largest percentage error < FERREL, or when ')
      call edisp(iuout,'          absolute error < FERMFL ')
      call edisp(iuout,
     &' PMAX   : maximum absolute pressure correction applied ')
      write(outs,'(A,F9.2,A)')
     &'          to any node; current value :',PMAX,' Pa'
      call edisp(iuout,outs)
      call edisp(iuout,
     &' STEFFR : when the ratio of successive pressure corrections ')
      call edisp(iuout,
     &'          for a node < STEFFR then Steffensen`s relaxation ')
      WRITE(outs,'(A,F9.2)') 
     &'          is applied to that node; current value :',STEFFR
      call edisp(iuout,outs)
      call edisp(iuout,' ')

C Test if user wishes to reset any of these values.
      dok=.false.
      h(1)='If after reviewing the current values you wish to change'
      h(2)='any of them, respond with a yes. '
      CALL ASKOK(' ','Reset any of these parameters?',OK,dok,2)
      IF(.not.OK)GOTO 100

C Reset MAXITF
   20 IV=1
      WRITE(outs,'(A,I5,A)') ' MAXITF [currently = ',MAXITF,' ] '
      H(1)='This is the maximum number of iterations allowed '
      H(2)='before solution fails. '
      IV=MAXITF
      CALL EASKI(IV,outs,' MAXITF value?',
     &  0,'F',1000,'W',0,'MAXITF ',IER,2)
      IF(IER.NE.0) GOTO 20
      MAXITF=IV

C Reset FERREL (%)
   30 WRITE(outs,'(A,F9.3,A)') ' FERREL [currently = ',FERREL*100.0,
     &                         ' %] '
      H(1)='This is the percentage error allowed. '
      V=FERREL*100.
      CALL EASKR(V,outs,' FERREL value (%) ? ',
     &  0.,'F',100.,'F',0.,'FERREL ',IER,1)
      IF(IER.NE.0) GOTO 30
      FERREL=V*.01

C Reset FERMFL (kg/s)
   40 WRITE(outs,'(A,F9.5,A)') ' FERMFL [currently = ',FERMFL,' kg/s] '
      H(1)='This value is the largest absolute residual flow '
      H(2)='error which is allowed in any node.'
      V=FERMFL
      CALL EASKR(V,outs,' FERMFL value (kg/s) ? ',
     &  0.,'F',1000.,'F',0.,'FERMFL ',IER,2)
      IF(IER.NE.0) GOTO 40
      FERMFL=V

C Reset PMAX (Pa)
   50 WRITE(outs,'(A,F9.2,A)') ' PMAX [currently = ',PMAX,' Pa] '
      H(1)='This value is.... '
      V=PMAX
      CALL EASKR(V,outs,' PMAX value (Pa) ? ',
     &  0.,'F',0.,'-',0.,'PMAX ',IER,1)
      IF(IER.NE.0) GOTO 50
      PMAX=V

C Reset STEFFR
   60 WRITE(outs,'(A,F9.2,A)') ' STEFFR [currently = ',STEFFR,'] '
      H(1)='This value is.... '
      V=STEFFR
      CALL EASKR(V,outs,' STEFFR: ',
     &  0.,'-',0.,'-',0.,'STEFFR',IER,1)
      IF(IER.NE.0) GOTO 60
      STEFFR=V
      GOTO 100

C Test if user wishes to use a different stack pressure calculation model
   70 CALL EPAGE
      call edisp(iuout,' ')
      call edisp(iuout,' Stack pressure effect calculation options:')
      call edisp(iuout,'   1: fluid density of node which is sending ')
      call edisp(iuout,'      according to most recently computed flow')
      call edisp(iuout,'      direction')
      call edisp(iuout,'   2: average fluid density of connected nodes')
      call edisp(iuout,' ')
      WRITE(outs,'(A,I1,A)')' IPSMOD value [currently = ',IPSMOD,'] '
      IV=IPSMOD
      CALL EASKI(IV,outs,' Stack pressure method: ',
     &        1,'F',2,'F',-1,'stack pressure type',IER,0)
      IF(IER.NE.0) GOTO 70
      IPSMOD=IV
      GOTO 100

C Test if user wishes to alter the matrix solver
   80 CALL EPAGE
      call edisp(iuout,' ')
      call edisp(iuout,' Currently supported matrix solver options:')
      call edisp(iuout,'  1: Gaussian elimination with backsubsti-')
      call edisp(iuout,'     tution and no pivoting')
      call edisp(iuout,'  2: LU decomposition with implicit pivoting')
      call edisp(iuout,'     (Crout)')
      call edisp(iuout,' ')
      WRITE(outs,'(A,I2,A)')' MSLVTP value [currently = ',MSLVTP,'] '
      IV=MSLVTP
      CALL EASKI(IV,outs,' Solver type: ',
     &        1,'F',2,'F',-1,'solver type',IER,0)
      IF(IER.NE.0) GOTO 80
      MSLVTP=IV
      GOTO 100

C Test if user wishes to reset trace facility
   90 CALL EPAGE
      call edisp(iuout,' ')
      call edisp(iuout,' Currently supported trace output options are:')
      call edisp(iuout,'  -1: no trace output at (for use with bps) ')
      call edisp(iuout,'   0: no trace, only indicate no of iterations')
      call edisp(iuout,'   1: pressure, residual & relative error at ')
      call edisp(iuout,'      worst (relative) node')
      call edisp(iuout,'   2: as 1 + pressure & residual at all nodes ')
      call edisp(iuout,'           + pointer(s) at worst relative and ')
      call edisp(iuout,'             absolute node(s)')
      call edisp(iuout,'   3: as 2 + network matrix solving info')
      call edisp(iuout,' ')
      WRITE(outs,'(A,I2,A)') ' MFTRAC value [currently =',MFTRAC,'] '
      IV=MFTRAC
      CALL EASKI(IV,outs,' Trace option: ',
     &        -1,'F',3,'F',-1,'trace option',IER,0)
      IF(IER.NE.0) GOTO 90
      MFTRAC=IV
      GOTO 100

  100 RETURN

      END

