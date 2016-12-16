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

C Definition routines for distributions:
C   SDDEF001 - Thermophysical properties
C   SDDEF002 - Composite constructions
C   SDDEF003 - Weather
C   SDDEF004 - Operations
C   SDDEF005 - Convection coefficients
C   SDDEF101 - Zone geometry
C   SDDEF1001 - Zone controls
C   SDDBOUND - ask for numerical data with range checking.

C ****************** SDDEF001 ******************
C SDDEF001 - Material properties distribution definition.
      subroutine SDDEF001(ICDEF,IER)
#include "building.h"
C Common blocks UA1 UA3 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      logical closemat1,closemat2

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      CHARACTER NAM*72
      CHARACTER t24*24,message*48
      CHARACTER ITEM(8)*16, LELM(MMLC*ME)*36

      DIMENSION IVALP(6),IVALE(MMLC*ME),IELM(MMLC*ME)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF001'  ! set for subroutine
      
      IER=0

C Establish if material data arrays have been filled. If not return.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so admit-',
     &    'tance of construction attributes cannot be calculated.','W')
        ier=1
        return
      endif

C Materials - find and display unique elements.
      call edisp(iuout,' Material selection.')

C This returns the number of unique materials and array of
C materials array indices.
      call UPELM(NPRIM,IELM,IER)
      write(6,*) 'nprim ',nprim
      write(6,*) 'ielm ',ielm

C Now find the names of the materials.
      DO 20 II=1,NPRIM
        matarrayindex=IELM(II)   ! which material
 
C And if matarrayindex is zero then re-establish NAM.
        if(matarrayindex.eq.0)then
          NAM='AIR'
        elseif(matarrayindex.lt.0)then
          NAM='Confused'
        else
          write(NAM,'(a)') matname(matarrayindex)(1:32)
        endif

C Check for errors - add to the total error counter if error has occured.
        if (IER.ne.0) then
          IER=101
          return
        endif
        LELM(II)=NAM(1:36)
 20   CONTINUE

      helptopic='uncert_material'
      call gethelptext(helpinsub,helptopic,nbhelp)
      IEPIC=NPRIM
      CALL EPICKS(IEPIC,IVALE,' ',' Which material : ',
     &   36,NPRIM,LELM,' Material',IERR,nbhelp)

C Check for errors - add to the total error counter if error has occured.
      if (IERR.ne.0) then
        IER=101
        return
      endif

C If no type has been selected then return to main SA menu.
      if (IEPIC.eq.0) then
        call edisp(iuout,' Returning to main menu. ')
        call edisp(iuout,
     &            ' * No data saved from distribution specification! ')
        IER=101
        return
      endif


C Now determine the property of the element which is uncertain.
      call edisp(iuout,' Material property selection. ')
      call edisp(iuout,'  ')
      ITEM(1) ='Conductivity '
      ITEM(2) ='Density      '
      ITEM(3) ='Specific Heat'
      ITEM(4) ='Emissivity   '
      ITEM(5) ='Absorptivity '
      ITEM(6) ='Diffusion res'
      NITMS=6

      IPPIC=6
      CALL EPICKS(IPPIC,IVALP,' ',' Which property : ',
     &  16,NITMS,ITEM,LELM(IVALE(1)),IERR,nbhelp)

C Check for errors - add to the total error counter if error has occured.
      if (IERR.ne.0) then
        IER=101
        return
      endif

C If no type has been selected then return to main SA menu.
      if (IPPIC.eq.0) then
        call edisp(iuout,' Returning to main menu: ')
        call edisp(iuout,
     &             ' * No data saved for distribution specification! ')
        IERR=101
        return
      endif

C Ask for value of uncertainty.
      if(IDFLTR(1,IVALP(1)).eq.1)then
        write(message,'(a,f5.2)') 
     &    'The current default is % and ',DFLTRANGE(1,IVALP(1))
        call edisp(iuout,message)
      else
        write(message,'(a,f5.2)') 
     &    'The current default is +- ',DFLTRANGE(1,IVALP(1))
        call edisp(iuout,message)
      endif
      call EASKAB('Define parameter bounds:','  ',
     &  'from default definitions','by data entry',IANS,0)
      if (IANS.eq.1) then
        continue  ! work done in next block of code
      elseif (IANS.eq.2) then
        IALLOW=2  ! use to select either % or abs.

C Use the general topic defaults as initial values.
        XMPERC=10.0  ! default %
        XMABS=10.0
        if(IDFLTR(1,IVALP(1)).eq.1)XMPERC=DFLTRANGE(1,IVALP(1))
        if(IDFLTR(1,IVALP(1)).eq.2)XMABS=DFLTRANGE(1,IVALP(1))
        XLOW=0.0
        XHIGH=100.0
        call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      endif

C Write info to common for each of the material and property
C selections that have been made.
      do 99 I=1,IEPIC
        do 66 J=1,IPPIC
          if (I.gt.1.or.J.gt.1) then
            ICDEF=ICDEF+1
            NICNG=NICNG+1
          endif

C Material properties.
          IDTYPU(ICDEF)=1

C Add material selections to common.
          IDMAT(ICDEF,1)=IELM(IVALE(I))

C Add property to common.
          IDMAT(ICDEF,2)=IVALP(J)

C Add distribution data to common. If user selected the default
C use that otherwise apply what was returned from SDDBOUND.
          if (IANS.eq.1) then

C Recover the material property and hence uncertainty.
            IDMAT(ICDEF,3)=IDFLTR(1,IVALP(J))
            DATU(ICDEF,1)=DFLTRANGE(1,IVALP(J))
            write (t24,'(3a)')ITEM(IVALP(J))(1:6),':',
     &        LELM(IVALE(I))(1:16)
            call st2name(t24,LCNG(ICDEF))
          else
            IDMAT(ICDEF,3)=IALLOW
            DATU(ICDEF,1)=XLOW
            if (IALLOW.eq.3) then
              DATU(ICDEF,2)=XHIGH
            else
              DATU(ICDEF,2)=0.
            endif
          endif
 66     continue
 99   continue

      return
      end

C ****************** SDDEF002 ******************
C SDDEF002 - Composite constructions distribution definition.
      subroutine SDDEF002(ICDEF,IER)
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA31/MATNAM(MNCNG,2)

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2,modmlc

      CHARACTER NAM*72
      CHARACTER LELM(MMLC*ME)*36
      CHARACTER MATNAM*32

      DIMENSION IVALE(MMLC*ME)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF002'  ! set for subroutine
      
      IER=0

C Establish if material data arrays have been filled. If not return.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so distri-',
     &    'bution of constructions not available.','W')
        ier=102
        return
      endif

C Composite constructions.
      if(mlcver.eq.0)then
        call EPKMLC(ISEL,'Which construction?','  ',IERR)
      else
        call edisp(iuout,'Which construction?')
        CALL EDMLDB2(modmlc,'-',ISEL,IER)
      endif
      
      helptopic='uncert_MLC'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
C Check for errors - add to the total error counter if error has occured.
      if (IERR.ne.0) then
        IER=101
        return
      endif
      write (MATNAM(ICDEF,1),'(a)') mlcname(ISEL)
      call EASKAB('Which composite construction parameter? ','  ',
     &  'A materials thickness','Whole composite',IRT,nbhelp)
      if (IRT.eq.1) then

C Pick a material in that MLC.
        II=0
        do 10 IL=1,LAYERS(ISEL)
          matarrayindex=IPRMAT(ISEL,IL)   ! which array index
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h'.or.
     &       matarrayindex.eq.0)then
            NAM='AIR'
          elseif(matarrayindex.lt.0)then
            IER=101  ! can no deal with this
            return
          else
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif

          II=II+1
          LELM(II)=NAM(1:36)
 10     CONTINUE
        NPRIM=II
        INPIC=1
        CALL EPICKS(INPIC,IVALE,' ',' Which material: ',
     &    36,NPRIM,LELM,' material',IERR,nbhelp)

C Check for errors - add to the total error counter if error has occured.
        if (IERR.ne.0) then
          IER=101
          return
        endif

C If no type has been selected then return to main SA menu.
        if (INPIC.eq.0) then
          call edisp(iuout,' Returning to main menu. ')
          call edisp(iuout,
     &            ' * No data saved from distribution specification! ')
          IER=101
          return
        endif

C Everything is OK re selection therefore add type selection to common.
        IDMAT(ICDEF,1)=IPRMAT(ISEL,IVALE(1))
          
      else
        call edisp(iuout,'Not enabled yet.')
        IER=101
        return
      endif

      IALLOW=3
      XMPERC=10.0  ! default %
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DATU(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DATU(ICDEF,2)=XHIGH
      else
        DATU(ICDEF,2)=0.
      endif
      return

C << if there is not yet a location which matches this offer
C << the user one that is a good fit (via a scan for MLC or
C << materials.
      end

C ****************** SDDEF003 ******************
C SDDEF003 - Weather parameters distribution definition.
      subroutine SDDEF003(ICDEF,IER)
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      CHARACTER ITEM(6)*20

      DIMENSION IVALP(6)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF003'  ! set for subroutine

      helptopic='uncert_climate'
      call gethelptext(helpinsub,helptopic,nbhelp)  
      
C Now determine the climatic parameter which is uncertain.
      call edisp(iuout,' Climatic parameter selection. ')
      call edisp(iuout,'  ')
      ITEM(1) ='Dry bulb temperature'
      ITEM(2) ='Direct normal solar '
      ITEM(3) ='Diffuse horiz solar '
      ITEM(4) ='Wind speed          '
      ITEM(5) ='Wind direction      '
      ITEM(6) ='Relative humidity   '
      NITMS=6

      INPIC=1
      CALL EPICKS(INPIC,IVALP,' ','Which parameter:',
     &  20,NITMS,ITEM,'Weather parameters',IERR,nbhelp)
      if (IERR.ne.0.or.IVALP(1).eq.0) then
        IER=101
        return
      endif
      IDMAT(ICDEF,1)=IVALP(1)

      IALLOW=2
      XMPERC=10.0  ! default %
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DATU(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DATU(ICDEF,2)=XHIGH
      else
        DATU(ICDEF,2)=0.
      endif

C << if a location for weather has not yet been defined then
C << offer to make one for the user.

      return
      end

C ****************** SDDEF004 ******************
C SDDEF004 - Operations distribution definition. Where
C IDMAT 1=sensible, 2=latent, 3=radiant frac, 4=convective frac,
C 5= total Q, 6= period start, 8 RC split.
C << could be simplified 4 & 8 are redundant, and then add one
C << for period end.
      subroutine SDDEF004(ICDEF,IER)
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      integer IRT  ! for radio button

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF004'  ! set for subroutine
      helptopic='uncert_operations'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      IER=0
C Operations file data.
      call edisp(iuout,' Operations parameter selection. ')
      IRT=0
      call EASKAB('  ','Which scheduled parameter? ',
     &  'Casual gains','Air flow',IRT,nbhelp)
      if (IRT.eq.1) then
        call EASKABC('  ','Which casual gain?',
     &               'occupants','lights','small power',IRT,nbhelp)
        IDMAT(ICDEF,1)=IRT
        call EASKATOG('Which property?',' ','Sensible','Latent',
     &    'Radiant frac','Convective frac','Total Q','Period start',
     &    'RC split  ',IRT,nbhelp)
        if(IRT.eq.7)IRT=8  ! to enable different RC split
        IDMAT(ICDEF,2)=IRT
      else
        IDMAT(ICDEF,1)=0
        call EASKATOG('Which property?',' ','Infiltration',
     &    'Ventilation','N/A','Source temp.','  ',
     &    '  ','  ',IRT,nbhelp)
        IDMAT(ICDEF,2)=IRT
      endif

      IALLOW=3
      XMPERC=10.0  ! default %
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DATU(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DATU(ICDEF,2)=XHIGH
      else
        DATU(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDEF005 ******************
C SDDEF005 - Convection coefficients distribution definition.
C << has not yet been tested >>
      subroutine SDDEF005(ICDEF,IER)
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      IDMAT(ICDEF,1)=1
      IDMAT(ICDEF,2)=1

C Check for htc file - can use scaling on calculated coefficievts that way.

      call edisp(iuout,' Convection coefficient selection. ')
      call EASKAB('Only Halcrow time invarient enabled ! ','  ',
     &                'Continue and define distribution','Cancel',IRT,0)
      if (IRT.eq.2) then
        IER=101
        return
      endif
      IDMAT(ICDEF,3)=4
      DATU(ICDEF,1)=3.
      DATU(ICDEF,2)=2.

C      IALLOW=3
C      XMPERC=10.0
C      XMABS=10.0
C      XLOW=0.0
C      XHIGH=100.0
C      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
C      IDMAT(ICDEF,3)=IALLOW
C      DATU(ICDEF,1)=XLOW
C      if (IALLOW.eq.3) then
C        DATU(ICDEF,2)=XHIGH
C      else
C        DATU(ICDEF,2)=0.
C      endif
      return
      end

C ****************** SDDEF101 ******************
C SDDEF101 - Zone geometry distribution definition.
C Zone air volume supported IDMAT(ICDEF,1)=1
C Surface area if IDMAT(ICDEF,1)=2
      subroutine SDDEF101(ICDEF,IER)
#include "building.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      CHARACTER ITEM(2)*16
      character message*48
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF101'  ! set for subroutine
       helptopic='uncert_air_volume'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      ITEM(1) ='Zone air volume '
      ITEM(2) ='Surface area    '
      NITMS=2

      call edisp(iuout,'Zone geometry parameter distribution def.')
      call EASKAB('Options ','  ',
     &  'Zone air volume','Surface area',IRT,nbhelp)
      if (IRT.eq.1) then
        IDMAT(ICDEF,1)=1
        IDMAT(ICDEF,2)=1
        if(IDFLTR(6,1).eq.1)then
          write(message,'(a,f5.2)') 
     &    'The current default is % and ',DFLTRANGE(6,1)
          call edisp(iuout,message)
        else
          write(message,'(a,f5.2)') 
     &    'The current default is +- ',DFLTRANGE(6,1)
          call edisp(iuout,message)
        endif
      elseif (IRT.eq.2) then
        IDMAT(ICDEF,1)=2
        IDMAT(ICDEF,2)=1
        if(IDFLTR(6,2).eq.1)then
          write(message,'(a,f5.2)') 
     &    'The current default is % and ',DFLTRANGE(6,2)
          call edisp(iuout,message)
        else
          write(message,'(a,f5.2)') 
     &    'The current default is +- ',DFLTRANGE(6,2)
          call edisp(iuout,message)
        endif
      endif

      IALLOW=2
      XMPERC=10.0  ! default %
      XMABS=1.0    ! ? 1m^3
      if (IRT.eq.1) then
        if(IDFLTR(6,1).eq.1)XMPERC=DFLTRANGE(6,1)
        if(IDFLTR(6,1).eq.2)XMABS=DFLTRANGE(6,1)
      elseif (IRT.eq.2) then
        if(IDFLTR(6,2).eq.1)XMPERC=DFLTRANGE(6,2)
        if(IDFLTR(6,2).eq.2)XMABS=DFLTRANGE(6,2)
      endif
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DATU(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DATU(ICDEF,2)=XHIGH
      else
        DATU(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDEF1001 ******************
C SDDEF1001 - Control distribution definition. 
C << needs to be tested ! >>
      subroutine SDDEF1001(ICDEF,IER)
#include "building.h"
#include "model.h"
#include "schedule.h"
C Common blocks UA3 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)

      integer ICDEF  ! current index
      COMMON/UA32/IMISCD(MNCNG,2)

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      integer KSTR(MDTY)
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDEF1001'  ! set for subroutine
      helptopic='uncert_control'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      IER=0
C Control file data.
      call edisp(iuout,' Control parameter selection. ')

C Ask for the control function index ->IDMAT(?,1)
      IDST=1
      call EASKI(IDST,' ','Control function index?',1,'F',100,'F',1,
     &  'control function index',IERI,nbhelp)
      IDMAT(ICDEF,1)=IDST
      
C Ask for the control function day type ->IDMAT(?,2)
      IX=1
      CALL EPICKS(IX,KSTR,' ','choose daytype to add period in',
     &  12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
      IDMAT(ICDEF,2)=KSTR(1)

C Ask for the control function period ->IMISCD(?,1)
      IDST=2
      call EASKI(IDST,' ','Control function period?',1,'F',24,'F',2,
     &  'control function period',IERI,nbhelp)
      IMISCD(ICDEF,1)=IDST

C Ask for the control function specific parameter ->IMISCD(?,2)
C IMISCD 1=Period start time, 2=max heating, 3=min heating,
C        4=max cooling, 5=min cooling, 6=heating SP, 7=cooling SP.
      IRT=0
      call EASKATOG('Which control property?',' ','Period Start',
     &  'Max heat','Min heat','Max cool','Min cool','Heating SP',
     &  'Cooling SP',IRT,nbhelp)
      IMISCD(ICDEF,2)=IRT

      IALLOW=2
      XMPERC=10.0  ! default %
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DATU(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DATU(ICDEF,2)=XHIGH
      else
        DATU(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDBOUND ******************
C SDDBOUND - Operations distribution definition. Returned value
C of IALLOW specifies whether % or abs or explicit.
      subroutine SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      integer IALLOW  ! passed in 3= %, abs and explicit changes;
                      ! 2= % and abs only; 1=exp only;
      real XMPERC     ! default %
      real XMABS      ! default absolute
      real XLOW       ! returned % or absolute

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)
      
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SDDBOUND'  ! set for subroutine

      if (IALLOW.eq.3) then

        helptopic='uncert_bounds_3'
        call gethelptext(helpinsub,helptopic,nbhelp)
        
C Now ask for the numerical data.
        call EASKABC('Define parameter bounds',' via:',
     &   'percentage change','absolute change',
     &   'explicit alternatives',ICNG,nbhelp)
        PERC=0.0; ABS=0.0; EXPU=0.0; EXPL=0.0
        if (ICNG.eq.1) then
          call EASKR(PERC,' ','Percentage change value?',
     &      0.,'F',100.,'W',XMPERC,'Percentage change',IER,nbhelp)
          XLOW=PERC
        elseif (ICNG.eq.2) then
          call EASKR(ABS,' ','Absolute change value?',
     &      0.,'F',XMABS*2,'W',XMABS,'Absolute change',IER,nbhelp)
          XLOW=ABS
        elseif (ICNG.eq.3) then

C << not clear what this does >>
22        call EASKR(EXPU,' ','Explicit definition of upper value?',
     &      XLOW,'W',XHIGH,'W',1.,'Explicit def upper value',IER,
     &      nbhelp)
          call EASKR(EXPL,' ','Explicit definition of lower value?',
     &      XLOW,'W',XHIGH,'W',1.,'Explicit def lower value',IER,
     &      nbhelp)
          if (EXPL.ge.EXPU) goto 22
          XLOW=EXPU
          XHIGH=EXPL
        endif
      elseif (IALLOW.eq.2) then
        helptopic='uncert_bounds_3'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKAB(' Define parameter bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        PERC=0.
        ABS=0.
        if (ICNG.eq.1) then
          call EASKR(PERC,' ','Percentage change value?',
     &      0.,'F',100.,'W',XMPERC,'Percentage change',IER,nbhelp)
          XLOW=PERC
        elseif (ICNG.eq.2) then
          call EASKR(ABS,' ','Absolute change value?',
     &      0.,'F',XMABS*2,'W',XMABS,'Absolute change ',IER,nbhelp)
          XLOW=ABS
        endif
      endif

      IALLOW=ICNG  ! pass back user method choice

      return
      end
