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
      SUBROUTINE NETWDRW
      RETURN
      END

      SUBROUTINE NWKSLCTC(IX,IY,IT,IS,ISA)
      character outs*124
      write(outs,'(5i4)')'NWKSLCTC ',IX,IY,IT,IS,ISA 
      write(6,*) outs
      RETURN
      END

      SUBROUTINE GNWKQUERY(IQ,PV,TV,CV,PH,TH,CH)
      character outs*124
      write(outs,*)'GNWKQUERY ',IQ,PV,TV,CV,PH,TH,CH 
      write(6,*) outs
      RETURN
      END

      SUBROUTINE NETWSNAP(X,Y,XS,YS)
      character outs*124
      write(outs,*)'NETWSNAP ',X,Y,XS,YS
      write(6,*) outs
      RETURN
      END

      SUBROUTINE NETWMIC(IMODE,IMOPT)
      character outs*124
      write(outs,*)'NETWMIC ',IMODE,IMOPT
      write(6,*) outs
      RETURN
      END

      SUBROUTINE GCONAD(IX,IY,IXR,IYR,IB)
      character outs*124
      write(outs,*)'GCONAD ',IX,IY,IXR,IYR,IB
      write(6,*) outs
      RETURN
      END

      SUBROUTINE GRIDUPDT(IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY)
      character outs*124
      write(outs,*)'GRIDUPDT ',IZOOM,IGRIDU,IGRIDS,ISNP,IPAN,DX,DY
      write(6,*) outs
      RETURN
      END

      SUBROUTINE NWKUPDTPOS(IS,ISA,IT,IX,IY)
      character outs*124
      write(outs,*)'NWKUPDTPOS ',IS,ISA,IT,IX,IY
      write(6,*) outs
      RETURN
      END

      SUBROUTINE ICNTFM(ITFM)
      RETURN
      END

      SUBROUTINE NETWMCO(IMODE,IMOPT)     
      character outs*124
      write(outs,*)'NETWMCO ',IMODE,IMOPT
      write(6,*) outs
      RETURN
      END
