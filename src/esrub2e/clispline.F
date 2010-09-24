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

C Climate interpolation code (prototype).
C datain is an array of ndatain items,
C idofrom is the index of the array to begin fitting,
C howmany is the number of points beyond idofrom to fit,
C act character toggle defining method to use ( ?, ?, ?)
C actdat1,actdat2,actdat3 are data in support of the method,
C   for act = '-' the array is passed without modification,
C   for act = 'd' the points after idofrom are equivalent to
C   the data actdat1=points previous with an offset of actdat2. 
C   for act = '?' actdat1=?,actdat2=?,actdat3=? 
C dataout is the array of data after the fit has been done.
      subroutine clspline(datain,ndatain,idofrom,howmany,actdat1,
     &  actdat2,actdat3,act,datout,ier)

# include "b2e.h"

      COMMON/OUTIN/IUOUT,IUIN
      real datain(mbrow),datout(mbrow)
      integer howmany
      character act*1

C Begin by clearing datout and copying in datain up to the
C point idofrom.
      do 42 i = 1,ndatain
        if(i.le.idofrom)then
          datout(i)=datain(i)
        else
          datout(i)=0.0
        endif
  42  continue

C Depending on 'act' define what items after idofrom should be.
C If act = '-' then just pass the data from datain to datout.
      if(act(1:1).eq.'-')then
        do 43 j = idofrom,ndatain
          datout(j)=datain(j)
  43    continue

C Difference method based on earlier sequence with offset.
      elseif(act(1:1).eq.'d')then
        k = nint(actdat1)
        do 44 j = idofrom,ndatain
          if(k.lt.j.and.k.ge.1)then
            datout(j)=datain(j-k) + actdat2
          endif
  44    continue

C << other methods and act tags follow from here.... >>
      elseif(act(1:1).eq.'?')then

      else
        call edisp(iuout,'unknown request to clspline')
        ier = 1
        return
      endif
      return
      end
