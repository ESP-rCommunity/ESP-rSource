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

C Dummy routine for modules which do not require interactive graphics
C  netwdrw  dummy drawing routine for esrunet/network.F
C  nwkslctc() dummy routine for esrunet/netwkc2f.F
C  gnwkquery() dummy routine for esrunet/netwkc2f.F
C  netwsnap() dummy routine for esrunet/netwkc2f.F
C  netwmic() dummy routine for esrunet/network.F
C  gconad() dummy routine for esrunet/netwkc2f.F
C  gridupdt() dummy routine for esrunet/netwkc2f.F
C  nwkupdtpos() dummy routine for esrunet/netwkc2f.F
C  icntfm() dummy routine for esrunet/netutil.F
C  netwmco() dummy routine for esrunet/network.F

C NETWDRW is the main drawing routine for the network  and is called
C when the workspace needs to be resized or redrawn. 
      SUBROUTINE NETWDRW
      RETURN
      END

C   NWKSLCTC Called from the c-library to determine if a component has been
C            selected.
      SUBROUTINE NWKSLCTC(IX,IY,IT,IS,ISA)
      character outs*124
      write(outs,'(5i4)')'NWKSLCTC ',IX,IY,IT,IS,ISA 
      write(6,*) outs
      RETURN
      END

C   GNWKQUERY Called by the c-library to get the current network viewing
C             parameters (for drawing scroll bars).
      SUBROUTINE GNWKQUERY(IQ,PV,TV,CV,PH,TH,CH)
      character outs*124
      write(outs,*)'GNWKQUERY ',IQ,PV,TV,CV,PH,TH,CH 
      write(6,*) outs
      RETURN
      END

C   NETWSNAP  Calculates the position of grid point co-ordinates
C             when grid snappping is on.
      SUBROUTINE NETWSNAP(X,Y,XS,YS)
      character outs*124
      write(outs,*)'NETWSNAP ',X,Y,XS,YS
      write(6,*) outs
      RETURN
      END

C NETWMIC manages the network icons, can be called from the C-side
C to handle deletion of icons.
      SUBROUTINE NETWMIC(IMODE,IMOPT)
      character outs*124
      write(outs,*)'NETWMIC ',IMODE,IMOPT
      write(6,*) outs
      RETURN
      END

C   GCONAD  The user define a network connection graphically.
C           called from the c-library.
      SUBROUTINE GCONAD(IX,IY,IXR,IYR,IB)
      character outs*124
      write(outs,*)'GCONAD ',IX,IY,IXR,IYR,IB
      write(6,*) outs
      RETURN
      END

C GRIDUPDT is called from the c-library and updates the network grid properties
C Zoom ,snap, etc, etc.
      SUBROUTINE GRIDUPDT(IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY)
      character outs*124
      write(outs,*)'GRIDUPDT ',IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY
      write(6,*) outs
      RETURN
      END

C   NWKUPDTPOS Called from the c-library and updates component and connection
C              intermediate point positions  interaction.
      SUBROUTINE NWKUPDTPOS(IS,ISA,IT,IX,IY)
      character outs*124
      write(outs,*)'NWKUPDTPOS ',IS,ISA,IT,IX,IY
      write(6,*) outs
      RETURN
      END

C ICNTFM is called from C code interface.
      SUBROUTINE ICNTFM(ITFM)
      RETURN
      END

      SUBROUTINE NETWMCO(IMODE,IMOPT)     
      character outs*124
      write(outs,*)'NETWMCO ',IMODE,IMOPT
      write(6,*) outs
      RETURN
      END
