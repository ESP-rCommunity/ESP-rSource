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

C This file contains the generic menus used within res.
C They are:

C GOMSETUP: Clear arrays/ set defaults.
C GOMENU :  The main menu type used for: interogation, misc data 
C           listings, graphs, frequency distributions.
C GOGET :   The main menu type used for: interogation, misc data 
C           listings, graphs, frequency distributions.

C MENUTYPE's are:
C MENUTYPE 1: not used
C MENUTYPE 2: Frequency histogram
C MENUTYPE 3: Time series plot
C MENUTYPE 4: Performance metrics
C MENUTYPE 5: Summary statistics
C MENUTYPE 6: Sensitivity
C MENUTYPE 7: Hours above query point
C MENUTYPE 8: Hours below query point
C MENUTYPE 9: Zone energy balance
C MENUTYPE 10: Energy demands
C MENUTYPE 11: Zone casual gains distribution
C MENUTYPE 12: Surface energy balance
C MENUTYPE 13: Monthly miscel gains and losses
C MENUTYPE 14: Surface condensation

C IGETNO contains all the information needed for specifying which data to recover
C IGETNO(*,1): The IGET number - metric identifier
C IGETNO(*,2): The chosen zone
C IGETNO(*,3): Variable - Surface number, casual gain type.
C IGETNO(*,4): Generally node number
C IGETNO(*,5): Result set number - default=1 ie the currently chosen set
C IGETNO(*,6): Line type (see below).
C IGETNO(*,7): Symbol index (see below).
C IGETNO(*,8): Associated axis (1=temp,2=Load(W),3=Intensity(W/m2),4=Misc.).
C IGETNO(*,9): Associated label width (for Menu type 5)

C This list is copied from the old tgraph for current settings examine 
C each entry below call to EMENU in subroutine GOMENU
C Item    Line type,  Axis,  Symbol  existing Label (assuming zone name is office).
C zone air  :  1        1       6     office db Tmp
C zone dp   :  1        1       1     zn  2 dew p T
C zone resul:  1        1       5     zn  2 resl T
C zone MRT  :  1        1       8     zn  2 MRT
C zone p.f  :  1        ?       7     zn  2 p.f
C zone rh   :  -202     4       2     zn  2 RH %
C ext air db:  -302     1       4     Ambient db Tmp
C int surf t:  1        1       2     zn  2 InSfT 3
C ext surf t:  1        1       3     zn  2 ExSfT 3
C surf node :  1        1       7     zn  2 Node T
C surf conv : -3        2       7     zn  2 InSfC 3  or zn 2 ExSfC 3
C partn conv: -3        2       8     zn  2 PrtCnv
C exter conv: -3        2       9     zn  2 ExtCnv
C plant     : -3        2       6     zn  2 Plant
C infiltr   : -3        2       4     zn  2 Infil
C ventila   : -3        2       9     zn  2 Ventil
C real pwr  : -4        2       6     zn  2 R Pwr    or zn 2 Q Pwr
C cas gn cnv: -3        2       10    zn  2 Cas con
C cas gn rad: -3        2       11    zn  2 Cas rad
C ext surf sol: -104    2       6     zn  2 ExSfA 3
C int surf sol: -104    2       4     zn  2 InSfA 3
C dir norm sol: -104    3       9     Dir N solar
C dif horz sol: -104    3       5     Diff solar
C wind spd  :  -203     4       6     Wind Speed
C wind dir  :  -203     4       4     Wind Direc
C amb rh    :  -202     4       7     RH %
C condens   :  -203     ?       1     zn  2 Conden

C All menus subscribe to this list of IGET values:

C IGET =  1 Zone temperature
C IGET =  2 Ambient temperature
C IGET =  3 Control point temperature
C IGET =  4 Internal surface temperature
C IGET =  5 Outside surface temperature (relative to zone)
C IGET =  6 Resultant temperature
C IGET =  7 Node temperature (MLC)

C All fluxes saved as value in watts, reporting in kW.
C IGET =  8 Zone sensible heating
C IGET =  9 Zone sensible cooling 
C IGET = 10 Zone total sensible load 

C IGET = 11 Infiltration
C IGET = 12 Ventilation

C IGET = 13 Zone RH
C IGET = 14 Dew point temperature

C IGET = 15 Casual gains (Total/Convec/Radiant/Type)
C IGET = 16 Surface convection (Int: (IGET,4)=1; Ext: (IGET,4)=2; Total int (IGET,4)=3)
C IGET = 17 Solar absorbed on external surfaces
C IGET = 18 Solar absorbed on internal surfaces

C IGET = 19 Direct solar (climate parameter)
C IGET = 20 Diffuse solar (climate parameter)
C IGET = 21 Wind speed (climate parameter)
C IGET = 22 Ambient RH (climate parameter)

C IGET = 23 Comfort (max and min; ICI= 1,2 or 3 = cmf index)
C IGET = 24 Mean radiant temperature
C IGET = 25 Surface flux (various IGET,4) where 1=conduction (inside),
C   2=convec (inside), 3=LW radiation (inside), 4=SW rad absorb inside,
C   5=radiant casual occup, 6=radiant casual light
C   7=radiant casual equip, 8=contrld casual gains, 9=heat storage (inside)
C   10=plant inj/extr (ins), 11=conduction (other fc), 12=convec (other),
C   13=long wave > buildings
C   14=long wave > sky, 15=long wave > ground, 16= SW radiation (othr),
C   17=heat storage (othr)
C IGET = 26 External lux level

C IGET = 27 Real electrical power consumption
C IGET = 28 Reactive electrical power consumption

C IGET = 29 Delta T: zone dbT - ambient T

C IGET = 30 Zone latent load - dehumidification (kW)
C IGET = 31 Zone latent load - humidification (kW)
C IGET = 32 Zone total latent load (kW)
C IGET = 33 Zone sensible + latent load (kW)
C IGET = 34 Wind direction (climate parameter)
C IGET = 35 Delta T: surf T - dewpoint T

C IGET = 36 Total conv @ interior/similar/adiab/constant surfaces
C IGET = 37 Total conv @ outside connected surfaces
C IGET = 38 Solar entering zone from outside (W)
C IGET = 39 Solar entering zone from adj zones (W)
C IGET = 40 Solar absorbed in zone (W)
C IGET = 41 Delta T: zone T - other zone T
C IGET = 42 Zone balance opaque exterior conv.
C IGET = 43 Zone balance opaque partitions conv.
C IGET = 44 Zone balance transparent exterior conv.
C IGET = 45 Zone balance transparent partitions conv.

C IGET = 46 Zone special materials/components output 1
C IGET = 47 Zone special materials/components output 2

C Slots 50 through 59 reserved for local comfort (checks made else where 
C assuming this is the case)
C IGET = 50 Local PPD - delta T head/ foot
C IGET = 51 Local PPD - floor T
C IGET = 52 Local PPD - floor/ ceiling radiant T asymmetry
C IGET = 53 Local PPD - wall/ wall radiant T asymmetry
C Assume other local comfort gets will occupy slots up to and including 59.

C IGET = 60 Energy demands
C IGET = 61 Zone energy balances (Integr: (IGET,4)=1; Snapshot: (IGET,4)=2)
C IGET = 62 Surface energy balances (Integr: (IGET,4)=1; Snapshot: (IGET,4)=2)
C IGET = 63 Zone casual gains distribution
C IGET = 64 Monthly gains and losses
C IGET = 65 Surface condensation
C IGET = 66 free
C IGET = 70 IPV demands set (heating/cooling/lights/small_power/fans/DHW/PV)
C IGET = 71 IPV distributed demands (light (unctld)/light (ctld)/ fans
C            /pumps/lifts/small_power/DHW/PV
C IGET = 72 IPV aggregate demands W/m2 for whole model
C IGET = 73 IPV visual comfort (various IGET,4) where 1=Guth, 2=glare,
C           3=daylight factors
C IGET = 74 IPV metric maximum power capacity (W/m^2)
C IGET = 75 IPV metric pollutant emissions (kg/m^2/yr)
C IGET = 76 IPV metric overall thermal comfort distribution

C IGET = 80 Measured zone db T (for display only)
C IGET = 81 Measured surface T (for display only)


C ************************ GOMSETUP ************************
C Set default values for IGETNO array and associated plotting/ labeling arrays.

      SUBROUTINE GOMSETUP
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      character SLABEL*32,GLABEL*20

      integer YAXSET, ADDLIN

C Reset axis max and min values to 1.0 and 0.0 resp.
C YAXSET: Axis max min set =1 automatically scalled, 2= user set.
      do 8 I=1,5
        YMAX(I)=1.0
        YMIN(I)=0.0
        YAXSET(I)=0
 8    continue

C Set add profile label ADDLIN=0 i.e. no add
      ADDLIN=0

C Reset to no choices and current result set.
      NGET=0
      do 10 I=1,MZS
        IGETNO(I,1)=0
        IGETNO(I,2)=0
        IGETNO(I,3)=0
        IGETNO(I,4)=0
        IGETNO(I,5)=ISIM
        IGETNO(I,6)=1
        IGETNO(I,7)=0
        IGETNO(I,8)=0
        IGETNO(I,9)=0
        SLABEL(I)='  '
        GLABEL(I)='  '
 10   continue
      ialstused=0

      return
      end

C ************************ EDGOGET ************************
C EDGOGET allows removal of output requests.
      SUBROUTINE EDGOGET
#include "epara.h"
#include "building.h"

      common/pophelp/h(60)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)

      character SLABEL*32,GLABEL*20
      character VERT(MZS+3)*28,KEY*1,h*72
      integer ORDER,OGET
      dimension INCGET(MZS),ORDER(MZS),OGET(MZS)

C Sort ORDER of IGETNO array for display purposes.
      do 100 I=1,NGET
        ORDER(I)=I
        OGET(I)=IGETNO(I,1)*10000 + IGETNO(I,2)*100 + IGETNO(I,3)
 100  continue
      call SORTI(OGET,ORDER,NGET,2)

C Set all values of incget to 1
      do 5 I=1,NGET
        INCGET(I)=1
 5    continue

C Initialise menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=1
      MCTL=3
      ILEN=NGET
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
      IER=0
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      VERT(1)='  Metric             Active'
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if (INCGET(ORDER(L)).eq.0) then
            WRITE(VERT(M),'(a,1x,2a)')KEY,GLABEL(ORDER(L)),'  '
          else
            WRITE(VERT(M),'(a,1x,2a)')KEY,GLABEL(ORDER(L)),' *'
          endif
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ------------------------ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2)  ='? help                     '
      VERT(M+3)  ='- exit and activate changes'

C Instanciate help messages for this menu.
      H(1)='This menu allows existing metric choices to be '
      H(2)='removed from the list to display. '
      H(3)='Each item can be toggled on/ off untill you are'
      H(4)='happy with the list as all items which are marked'
      H(5)='off are removed from the list when you exit.'

C Display menu.
      CALL EMENU('Metric selection',VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN

C Within the header so skip request.
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

C Remove inactive elements.
 21     do 20 I=1,NGET
          if (INCGET(I).eq.0) then
            do 30 K=I,NGET
              if (K.lt.NGET) then
                IGETNO(K,1)=IGETNO(K+1,1)
                IGETNO(K,2)=IGETNO(K+1,2)
                IGETNO(K,3)=IGETNO(K+1,3)
                IGETNO(K,4)=IGETNO(K+1,4)
                IGETNO(K,5)=IGETNO(K+1,5)
                IGETNO(K,6)=IGETNO(K+1,6)
                IGETNO(K,7)=IGETNO(K+1,7)
                IGETNO(K,8)=IGETNO(K+1,8)
                IGETNO(K,9)=IGETNO(K+1,9)
                SLABEL(K)=SLABEL(K+1)
                GLABEL(K)=GLABEL(K+1)
                INCGET(K)=INCGET(K+1)
              else
                IGETNO(K,1)=0
                IGETNO(K,2)=0
                IGETNO(K,3)=0
                IGETNO(K,4)=0
                IGETNO(K,5)=ISIM
                IGETNO(K,6)=1
                IGETNO(K,7)=0
                IGETNO(K,8)=0
                IGETNO(K,9)=0
                SLABEL(K)='  '
                GLABEL(K)='  '
                INCGET(K)=1
              endif
 30         continue
            NGET=NGET-1
            goto 21
          endif
 20     continue
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        CALL PHELPD('metric selection',5,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit vertex identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        INCGET(ORDER(IFOC))=INCGET(ORDER(IFOC))+1
        if (INCGET(ORDER(IFOC)).gt.1) INCGET(ORDER(IFOC))=0
      endif
      goto 3

      end


C ************************ GOMENU ************************
C The choices are put into the array IGETNO in the columns listed
C above.

      SUBROUTINE GOMENU
#include "building.h"
#include "geometry.h"
#include "tdf2.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/loadlabel/lodlabel(mcom,7)
      common/recov01/pifltog,lpifl
      COMMON/DNORGH/IRTYPE

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

C IGETFLG - flags for menu set up:
C IOCUPF current state of occupancy filter (0=none; 1=filtering on).
C IROC calculate rate of change of metric.
C ialstused is the anchor point used for selection (0 if unused)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

C Optional title for graph and up to 5 other labels and their positions.
      common/grextras/graftitle,grlbl(5),ngrlbl,lblpx(5),lblpy(5)

      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

C Pre calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)

      CHARACTER ITEM(35)*24, MTITLE*22, values*20
      CHARACTER ITEMS(18)*23
      CHARACTER ITEMC(21)*24
      character H*72,xfile*144,tg*1,delim*1
      character SLABEL*32,GLABEL*20,SUFFIX*8,lodlabel*6
      character ll1*8,ll2*8,ll3*8
      CHARACTER outs*124,pifltog*4,lpifl*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16
      character graftitle*64,grlbl*24,t64*64,t24*24

C Note: 60 is the maximum number of items associated with an anchor point.
      dimension ISP(MS),IZNSEL(MCON),ISFSEL(MCON)
      dimension IVALS(17),IVALC(21)

      integer YAXSET, ADDLIN
      integer nbofhelp   ! how many help lines to display for different menu types.
      integer loop       ! for do loops
      integer IAC        ! for user selection

      logical SLIST,NLIST,MRTLIST,OKL,dok,ok
#ifdef OSI
      integer iid1,iid2
#else
      integer*8 iid1,iid2
#endif

C The logical variables are required so as user is asked for additional 
C information about the choice of metric if necessary. NLIST=.true. implies 
C SLIST=.true. which are asked for all active zones (listed in NZNO).
      SLIST=.FALSE.   ! do we need to ask about surfaces
      NLIST=.FALSE.

C MNGET is the maximum number of metrics allowed to be chosen from the menu.
      if (MENUTYPE.eq.1) then
        return
C        MTITLE='  '
C        MNGET=1
      elseif (MENUTYPE.eq.2) then
        MTITLE='Frequency histogram'
        MNGET=1
      elseif (MENUTYPE.eq.3) then
        MTITLE='Time series plot'
        MNGET=MZS-1
      elseif (MENUTYPE.eq.4) then
        MTITLE='Performance metrics'
        MNGET=13
      elseif (MENUTYPE.eq.5) then
        MTITLE='Summary statistics'
        MNGET=1
      elseif (MENUTYPE.eq.6) then
        MTITLE='Sensitivity'
        MNGET=1
      elseif (MENUTYPE.eq.7) then
        MTITLE='Hrs above query point'
        MNGET=1
      elseif (MENUTYPE.eq.8) then
        MTITLE='Hrs below query point'
        MNGET=1
      elseif (MENUTYPE.eq.9) then
        MTITLE='Zone energy balance'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=61
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Zone energy balance (kWhr)'
      elseif (MENUTYPE.eq.10) then

C IGET = 60 and MENUTYPE 10:  Energy demands.
        MTITLE='Energy demands'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=60
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Zone energy demands (kWhrs)'
      elseif (MENUTYPE.eq.11) then

C IGET = 63 and MENUTYPE 11: Zone casual gains distribution.
        MTITLE='Casual gains distrib.'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=63
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Casual gains distribution (kWhr)'
      elseif (MENUTYPE.eq.12) then

C IGET = 62 and MENUTYPE 12: Surface energy balance flux paths.
        slist=.true.
        MTITLE='Surface energy balance'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=62
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Surface energy balance (Whrs)'
      elseif (MENUTYPE.eq.13) then

C IGET = 64 Monthly gains & losses. Use IGETNO(NGET,4) = 1 as 100Wh & 2 as kWh.
        CALL EASKAB(' When displaying monthly values round',
     &    ' :','to nearest 100Wh (default)','to nearest kWh',IR,0)
        MTITLE='Monthly gains/losses'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=64
        IGETNO(NGET,4)=IR
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Monthly zonal gains/losses'
      elseif (MENUTYPE.eq.14) then

C IGET = 65 Surface condensation. Use IGETNO(NGET,3) to denote fixed
C RH (number greater than zero) or current RH (zero). Use 
C IGETNO(NGET,4)=1 denote verbose and 0 to be summary.
        MTITLE='Surface condensation'
        MNGET=MZS-1
        NGET=1
        h(1)='Use a specified RH to test sensitivity to condensation.'
        CALL EASKAB(' Evaluate condensation using:',
     &    '  ','current zone RH','specified RH',IR,1)
        if(IR.eq.1)then
          IGETNO(NGET,3)=0
        else
          iv=70
          h(1)='This humidity will be used in the evaluation of'
          h(2)='condensation at surfaces.'
          CALL EASKI(iv,'Specified humidity (%) ? ',' ',
     &      1,'-',100,'-',70,'Specified RH',IER,2)
          IGETNO(NGET,3)=iv
        endif
        h(1)='The summary report includes totals for each surface. '
        CALL EASKAB(' Condensation report options:',
     &    '  ','summary','show each timestep',IR,1)
        if(IR.eq.1)then
          IGETNO(NGET,4)=0
        elseif(IR.eq.2)then
          IGETNO(NGET,4)=1
        endif
        IGETNO(NGET,1)=64
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=20
        SLABEL(NGET)='Surface condensation'
      else
        MNGET=1
        MTITLE='  '
      endif

C If energy demand or energy balance etc, save commands if recording and
C skip past menu to code that copies data into IGETNO array for each zone.
      if (MENUTYPE.ge.9.and.MENUTYPE.le.14) then
        if(pifltog(1:3).eq.'ON ')call WRITEPIF(MENUTYPE)
        goto 99
      endif

C Setup menu items.   
 1    ITEM(1) ='2 Result set           '
      ITEM(2) ='3 Display period       '
      ITEM(3) ='4 Select zones         '
      ITEM(4) ='  ------------------   '
      ITEM(5) ='a Climate              '
      ITEM(6) ='b Temperatures         '
      ITEM(7) ='c Comfort metrics      '
      ITEM(8) ='d Solar processes      '
      ITEM(9) ='  ------------------   '
      ITEM(10)='f Zone flux            '
      ITEM(11)='g Surface flux         '
      ITEM(12)='h Heat/cool/humidify   '
      ITEM(13)='i Zone RH              '
      ITEM(14)='j Casual gains         '
      ITEM(15)='k Electrical demand    '
      ITEM(16)='  ------------------   '
      ITEM(17)='m Renewables/adv. comp.'
      ITEM(18)='n Network air/wtr flow '
      ITEM(19)='o CFD metrics          '
      ITEM(20)='p Measured:temporal    '
      ITEM(21)='  ------------------   '
      if(NGET.eq.0)then
        ITEM(22)='                     '
      else
        ITEM(22)='q Edit selections    '
      endif
      if(ixopen.eq.1)then
        ITEM(23)='> Display to >> file '
      elseif(ixopen.eq.0)then
        ITEM(23)='> Display to >> screen'
      endif
      if(IROC.eq.0)then
        ITEM(24)='& Data: as values       '
      elseif(IROC.eq.1)then
        ITEM(24)='& Data: rate of change  '
      elseif(IROC.eq.-1)then
        ITEM(24)='& Data: integrated      '
      endif
      if (MENUTYPE.eq.3) then
        ITEM(25)='/ Clear all selections  '
      else
        if(iocupf.eq.0)then
          ITEM(25)='+ Filter >> none      '
        else
          ITEM(25)='+ Filter >> occup only'
        endif
      endif
      if (MENUTYPE.eq.3) then
        ITEM(26)='= Set axis scale'
      else
        if(IHFLAG.eq.0)then
          ITEM(26)='* Time >> 10h30   '
        elseif(IHFLAG.eq.1)then
          ITEM(26)='* Time >> 0.4375  '
        endif

C << place ILFLAG here to define tabular labels >>
      endif
      if (MENUTYPE.eq.3) then
        ITEM(27)='+ Add another profile'
      else
        if(delim.eq.'-')then
          ITEM(27)='^ Delim >> normal '
        elseif(delim.eq.'T')then
          ITEM(27)='^ Delim >> TAB    '
        elseif(delim.eq.'C')then
          ITEM(27)='^ Delim >> comma  '
        elseif(delim.eq.'S')then
          ITEM(27)='^ Delim >> space  '
        elseif(delim.eq.'X')then
          ITEM(27)='^ delim >> tagged '
        endif
      endif
      if(MENUTYPE.eq.3) then
        ITEM(28)='! Draw graph    '
        ITEM(29)='* Labels        '
        nl=2
      elseif(MENUTYPE.eq.4)then
        ITEM(28)='! List data    '
        nl=1
      else
        nl=0
      endif
      ITEM(28+nl)='? Help           '
      ITEM(29+nl)='- Exit           '

      NITMS=29+nl
      INO=-2

C Instanciate the help text for this menu.
      H(1) ='  '
      write(H(1),'(10x,a)') MTITLE(1:lnblnk(MTITLE))
      H(2) ='  '
      H(3) ='Select the required for output.  Some metrics require'
      H(4) ='extra information e.g. zone number, surface number. '
      H(5) ='  '
      write (H(6),'(a,i2,a)')'You have made ',NGET,
     &  ' selections, therefore you have a'
      NLEFT=(MNGET-NGET)
      write(H(7),'(a,i2,a)')'maximum of ',NLEFT,' selections left.'
      H(8) ='  '
      H(9) ='Display can be redirected to a file; be carefull not '
      H(10)='  to confuse graphical and text output.  '
      H(11)='Filtering is based on the definition of casual gains.'
      H(12)='  It is usual to filter on occupancy.'
      H(13)='Time can be displayed as hours and minutes (hh,mm) or'
      H(14)='  as a decimal (Day of year.Fraction of day).'
      H(15)='Delimiters between tabular output can be changed; '
      H(16)='  useful when exporting data to 3rd party software.'
      H(17)='Data can be displayed as raw values or as rate of '
      H(18)='  change of the value. '
      if(MENUTYPE.eq.1) then
        H(19)=' '
        nbofhelp=19
      elseif (MENUTYPE.eq.2) then
        H(19)=' '
        H(20)='A frequency histogram is based on a scan of the current'
        H(21)='performance metric along with some user preferences for'
        H(22)='the number of binns and frequency.  '
        nbofhelp=22
      elseif (MENUTYPE.eq.3) then
        H(19)=' '
        H(20)='A time series plot graphs one or more performance '
        H(21)='metrics over a user defined period. Typically each '
        H(22)='type of data is given a different line type and/or '
        H(23)='symbol and each zone is given a different colour. '
        nbofhelp=23
      elseif (MENUTYPE.eq.4) then
        H(19)=' '
        H(20)='The selection of `Performance metrics` produces reports'
        H(21)='of the statistics of one type of data over the period'
        H(22)='of the assessment or a user defined period. '
        nbofhelp=22
      elseif (MENUTYPE.eq.5) then
        H(19)=' '
        H(20)='The selection of `Summary statistics` produces reports'
        H(21)='of the statistics of one type of data over the period'
        H(22)='of the assessment or a user defined period. '
        nbofhelp=22
      elseif (MENUTYPE.eq.6) then
        H(19)=' '
        H(20)='A sensitivity report is based on data extracted from a'
        H(21)='sequence of assessments. '
        nbofhelp=21
      elseif (MENUTYPE.eq.7) then
        H(19)=' '
        H(20)='A `Hrs above query point` report provides additonal'
        H(21)='statistical information about the number of occurrances'
        H(22)='that the performance data exceeds a particular value.'
        nbofhelp=22
      elseif (MENUTYPE.eq.8) then
        H(19)=' '
        H(20)='A `Hrs below query point` report provides additonal'
        H(21)='statistical information about the number of occurrances'
        H(22)='that the performance data is less than a value.'
        nbofhelp=22
      elseif (MENUTYPE.eq.9) then
        H(19)=' '
        H(20)='A zone energy balance reports the flux gains and losses'
        H(21)='for infiltration, ventilation, occupant/lighting/small'
        H(22)='power and controlled casual gains as well as convection'
        H(23)='at the inside face of surfaces which have an outside'
        H(24)='exposure and convection at the inside face of surfaces'
        H(25)='which are connected to other zones or the ground. It'
        H(26)='also includes the convection portion of environmental'
        H(27)='control systems associated with the zone. '
        H(28)=' '
        H(29)='There is a TOTAL line which reports the sum of all '
        H(30)='gains and the sum of all losses and these should '
        H(31)='balance each other closely if the model is correct. '
        H(32)=' '
        H(33)='An energy balance (from the point of view of the air '
        H(34)='node of the zone) of all of the flux entering & leaving'
        H(35)='the air node should balance at each timestep of the '
        H(36)='simulation. The report is an integration of each of the'
        H(37)='above types of flux over the period of the assessment. '
        H(38)=' '
        H(39)='A flux is positive if it adds heat to the air node of '
        H(40)='the zone and is negative if it removes heat from the '
        H(41)='air node of the zone.  For example, if exterior surfaces'
        H(42)='which are opaque are always colder than the air temp '
        H(43)='then the report will include losses. '
        nbofhelp=43
      elseif (MENUTYPE.eq.10) then
        H(19)=' '
        H(20)='Energy demands help to be written. '
        nbofhelp=20
      elseif (MENUTYPE.eq.11) then
        H(19)=' '
        H(20)='Zone casual gain distribution help to be written. '
        nbofhelp=20
      elseif (MENUTYPE.eq.12) then
        H(19)=' '
        H(20)='A surface energy balance reports the flux gains & losses'
        H(21)='at the inside and `other-side` face of the surface for:'
        H(22)='Conduction (just below the surface), convection, long-'
        H(23)='wave radiation to other surfaces and at the outside face'
        H(24)='longwave radiation to other buildings/ground/sky.'
        H(25)='It also includes the radiant portion of occupants,'
        H(26)='lighting and small power gains as well as heat stored'
        H(27)='in the construction and the radiant portion of any'
        H(28)='environmental control systems associated with the zone.'
        H(29)=' '
        H(30)='There is a TOTAL line which reports the sum of all '
        H(31)='gains and the sum of all losses and these should '
        H(32)='balance each other closely if the model is correct. '
        H(33)=' '
        H(33)='An energy balance (from the point of view of a surface)'
        H(34)='of all of the flux entering and leaving at each face'
        H(35)='should balance at each timestep of the simulation.'
        H(36)='The report is an integration of each of the'
        H(37)='above types of flux over the period of the assessment.'
        H(38)=' '
        H(39)='A flux is positive if it adds heat to surface and is '
        H(40)='negative if it removes heat from the surface.'
        H(41)='For example, if exterior surfaces which are opaque'
        H(42)='are always colder than the air temp the the surface'
        H(43)='energy balance report will indicate gains at the inside'
        H(44)='face.'
        nbofhelp=44
      elseif (MENUTYPE.eq.13) then
        H(19)=' '
        H(20)='Monthly gains and losses are a summary of SOME of the'
        H(21)='gains and losses associated with each zone. This is '
        H(22)='NOT a complete energy balance. '
        nbofhelp=22
      elseif (MENUTYPE.eq.14) then
        H(19)=' '
        H(20)='Surface condensation - to be written. '
        nbofhelp=19
      endif

      CALL EMENU(MTITLE,ITEM,NITMS,INO)

C Check for ADDLIN in time series plot - must choose metric if ADDLIN=1.
      if (MENUTYPE.eq.3.and.ADDLIN.eq.1) then
        if (INO.lt.5.or.INO.gt.26) goto 1
      endif

      IF(INO.EQ.1)then
        CALL MORESS
        INO=-1
      elseif(INO.EQ.2)then
        CALL MOOPER
        INO=-1
      elseif(INO.EQ.3)then
        CALL MOZDFN
        INO=-1
      elseif(INO.EQ.5)then

C Climate sub-list. Note: on first entry only the solar direct
C toggle from the climate file will be known. If there are any
C temporal climate data these would not yet have been scanned so
C do this now. Use logic similar to that in moclim.F
        if(IDIRSOL.ne.0)then

C Direct normal solar from temporal file.
          IRTYPE=0
        elseif(IGLOHSL.ne.0)then

C Global horizontal.
          IRTYPE=1
        elseif(IALLCLM.ne.0)then

C From the allclm type check the 1st static item value.
          itdi=IALLCLM
          IACC1=ITMAR(itdi,IATDF(itdi,1))
          if(IACC1.eq.0)then
            IRTYPE=0
          elseif(IACC1.eq.123)then
            IRTYPE=1
          endif
        endif

        H(1)='Climate options:'
        H(2)=' << >>'
        H(3)=' << >>'
        inoc=0
        idnoc=0
        if(irtype.eq.0)then
          call MENUATOL('Select a climate metric',' Climate choices:',
     &      'a Ambient temperature','b Solar Dir N','c Solar diffuse',
     &      'd Wind speed','e Wind direction','f Ambient RH',
     &      'g Sky illumunance',' ',' ',' ',' ',' ',inoc,idnoc,3)
        else
          call MENUATOL('Select a climate metric',' Climate choices:',
     &      'a Ambient temperature','b Solar GlobH','c Solar diffuse',
     &      'd Wind speed','e Wind direction','f Ambient RH',
     &      'g Sky illumunance',' ',' ',' ',' ',' ',inoc,idnoc,3)
        endif
        call usrmsg('  ','  ','-')
        if(inoc.eq.0)goto 1
        if((NGET+1).GT.MZS)then
          call usrmsg('Number of graph items exceeded.',' ','P')
          goto 1
        endif
        NGET=NGET+1
        if(inoc.eq.1)then

C IGET = 2 outside temperature.
          IGETNO(NGET,1)=2
          IGETNO(NGET,6)=-302; IGETNO(NGET,7)=4
          IGETNO(NGET,8)=1;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Ambient db Tmp'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ambient db temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ambient db temp change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ambient db temp hours (degC.hr)'
          endif
        elseif(inoc.eq.2)then

C IGET = 19 Direct solar. Set text based on current value of irtype.
          IGETNO(NGET,1)=19
          IGETNO(NGET,6)=-104; IGETNO(NGET,7)=9
          IGETNO(NGET,8)=3;    IGETNO(NGET,9)=14
          if(irtype.eq.0)then
            GLABEL(NGET)='Direct nor solar'
            if(IROC.eq.0)then
              SLABEL(NGET)='DN solar intensity (W/m^2)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='DN solar intens (W/m^2/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='DN solar intens (Whr/m^2)'
            endif
          else
            GLABEL(NGET)='Glo horiz solar'
            if(IROC.eq.0)then
              SLABEL(NGET)='Glo hor solar intensity (W/m^2)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Glo hor solar intens (W/m^2/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Glo hor solar intens (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.3)then
          IGETNO(NGET,1)=20
          IGETNO(NGET,6)=-104; IGETNO(NGET,7)=5
          IGETNO(NGET,8)=3;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Diffuse solar'
          if(IROC.eq.0)then
            SLABEL(NGET)='Diffuse solar intensity (W/m^2)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Diffuse solar intens (W/m^2/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Diffuse solar intens (Whr/m^2)'
          endif
        elseif(inoc.eq.4)then
          IGETNO(NGET,1)=21
          IGETNO(NGET,6)=-203; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Wind speed'
          if(IROC.eq.0)then
            SLABEL(NGET)='Wind speed (m/sec)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wind speed change (m/sec/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wind speed hours (m.hr/sec)'
          endif
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=34
          IGETNO(NGET,6)=-203; IGETNO(NGET,7)=4
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Wind direc'
          if(IROC.eq.0)then
            SLABEL(NGET)='Wind direction (deg)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wind direction change (deg/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wind direction hours (deg.hr)'
          endif
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=22
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=7
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Ambient RH'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ambient RH (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ambient RH change (%/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ambient RH hours (%.hr)'
          endif
        elseif(inoc.eq.7)then
          IGETNO(NGET,1)=26
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=8
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=10
          GLABEL(NGET)='Sky illum'
          if(IROC.eq.0)then
            SLABEL(NGET)='Sky illuminance (lux)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Sky illuminance change (lux/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Sky illuminance hours (lux.hr)'
          endif
        endif
      elseif(INO.EQ.6)then

C Temperatures sub-menu.
        inoc=0
        idnoc=0
        H(1)='Temperature options:'
        H(2)=' << >>'
        H(3)=' << >>'
        H(4)='The reported MRT can either be area weighted (default)'
        H(5)='or for a specific sensor defined in the model.'
        H(6)=' '
        H(7)='The terms Inside and Outside are defined with respect '
        H(8)='to the current zone.  Therefore the Outside face of a '
        H(9)='surface is not necessarily an exterior surface.'
        H(10)='Delta T dewpoint is difference between inside surface T'
        H(11)='and zone dew point temperature (Surf T - Dewpoint).'
        call MENUATOL('Select a temperature metric.',
     &    ' Temperature choices:','a Zone db T',
     &    'b Zone db T - ambient db T','c Zone db T - other zone db T',
     &    'd Zone control point T','e Zone Resultant T',
     &    'f Mean Radiant T (area wtd)','g Mean Radiant T (at sensor)',
     &    'h Dew point T ','i Surf inside face T',
     &    'j Surf T - dewpoint T','k Surf outside face T',
     &    'l Surf node T',inoc,idnoc,11)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        NGET=NGET+1
        if(inoc.eq.1)then
          IGETNO(NGET,1)=1
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' db T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone db temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone db temp change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone db temp hours (degC.hr)'
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,1)=29
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Del dbT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Delta db temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Delta db temp change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Delta db temp hours (degC.hr)'
          endif
        elseif(inoc.eq.3)then

C IGET = 41 db T - another zone db T. Ask for other zone.
          call ASKZON(IZONE)
          IGETNO(NGET,3)=IZONE
          IGETNO(NGET,1)=41
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Del znT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Delta zn T - other zn T (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chg D znT - other znT (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='D znT - other znT hrs(degC.hr)'
          endif
        elseif(inoc.eq.4)then

C IGET = 3 Control point temperature. 
          IGETNO(NGET,1)=3
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=9
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Cpt T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Control pt temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Control pt temp change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Control pt temp hours (degC.hr)'
          endif
        elseif(inoc.eq.5)then

C IGET = 6  Resultant temperature.
          IGETNO(NGET,1)=6
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=5
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Res T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Resultant temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Resultant temp change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Resultant temp hours (degC.hr)'
          endif
        elseif(inoc.eq.6)then

C IGET = 24, average MRT.
          MRTLIST=.false.
          IGETNO(NGET,1)=24
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=8
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' MRT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Mean Radiant Temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Mean radiant Tmp change(degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Mean radiant Tmp hours(degC.hr)'
          endif
        elseif(inoc.eq.7)then

C IGET = 24, MRT at sensor.
          MRTLIST=.true.
          IGETNO(NGET,1)=24
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=8
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX=' MRT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Mean Radiant Temperature (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Mean radiant Tmp change(degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Mean radiant Tmp hours (degC.hr)'
          endif
        elseif(inoc.eq.8)then

C IGET = 14  Zone dew point.
          IGETNO(NGET,1)=14
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=2
          IGETNO(NGET,8)=1;    IGETNO(NGET,9)=12
          SUFFIX=' dew ptT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone dew point (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone dew point change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone dew point hours (degC.hr)'
          endif
        elseif(inoc.eq.9)then
          slist=.true.
          IGETNO(NGET,1)=4
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=2
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX=' InSurT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Surface temp inside (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Surface temp inside (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Surf temp inside hours (degC.hr)'
          endif
        elseif(inoc.eq.10)then
          slist=.true.
          IGETNO(NGET,1)=35
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX='Dsur-dpT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Delta surf T - zone dp T (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Delta surf - zn dp T (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Delta surfT - zn dpT hr(degC.hr)'
          endif
        elseif(inoc.eq.11)then
          IF(ISAVE.lt.3.)then
            call edisp(iuout,' This selection only available with')
            call edisp(iuout,' a save option 3-4 result-set.')
          endif
          slist=.true.
          IGETNO(NGET,1)=5
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=3
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX=' ExSurT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Surface temp outside (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Surface temp outside (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Surface out temp hrs (degC.hr)'
          endif
        elseif(inoc.eq.12)then
          if(ISAVE.ne.3)then
            call edisp(iuout,' This selection only available with')
            call edisp(iuout,' a save option 3 result-set.')
          else
            slist=.true.
            nlist=.true.
            IGETNO(NGET,1)=7
            IGETNO(NGET,6)=1; IGETNO(NGET,7)=7
            IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
            SUFFIX=' NodeT'
            if(IROC.eq.0)then
              SLABEL(NGET)='Surface node temp (degC)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Surface node temp (degC/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Surf node temp hours (degC.hr)'
            endif
          endif
        endif
      elseif(INO.EQ.7)then

C IGET = 23  Comfort, ask which index and initialise.
        H(1) ='Comfort indicies are: Predicted Mean Vote (PMV), PMV'
        H(2) ='using efective temperature (PMV set) and Predicted '
        H(3) ='Percentage Dissatisfied (PPD). '
        H(3) ='Local comfort indicies are:  '
        H(4) ='Head/ foot dT: PPD due to delta Tair between head  '
        H(5) ='   and feet.'
        H(6) ='Floor T: PPD due to warm floor.'
        H(7) ='Ceiling: percentage dissatisfied due to warm or'
        H(8) ='   cool ceiling.  Note this is not a PPD value.'
        H(9) ='Wall: percentage dissatisfied due to warm or'
        H(10)='   cool wall. Note this is not a PPD value.'
        H(11)='NB: ceiling and wall assume delta T between zone air'
        H(12)='    point and surface.'
        inoc=0
        idnoc=0
        call MENUATOL('Select a comfort metric',' Comfort choices:',
     &    'a Predicted Mean Vote (PMV)','b PMV using SET',
     &    'c Percentage Dissatisfied (PPD)','d Local delta T head-foot',
     &    'e Dissatisfied due to floor T','f Diss. warm/ cool ceiling',
     &    'g Diss. wall rad T assymetry','  ','  ','  ','  ','  ',
     &    inoc,idnoc,12)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        NGET=NGET+1
        IGETNO(NGET,1)=23
        IGETNO(NGET,9)=12
        if(inoc.eq.1)then
          IGETNO(NGET,3)=1
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PMV'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PMV (-)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PMV change/hr (-)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PMV hours) (-)'
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,3)=2
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PMV'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PMV (set)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PMV (set) (change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PMV set hours)'
          endif
        elseif(inoc.eq.3)then
          IGETNO(NGET,3)=3
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PPD (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PPD (% change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PPD % hours)'
          endif
        elseif(inoc.eq.4) then
          IGETNO(NGET,1)=50
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PD head/foot (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PD hd/ft (% change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort PD hd/ft (% hours)'
          endif
        elseif(inoc.eq.5) then
          IGETNO(NGET,1)=51
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PD floor (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PD floor (% change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort PD floor (% hours)'
          endif
          slist=.true.
        elseif(inoc.eq.6) then
          IGETNO(NGET,1)=52
          IGETNO(NGET,4)=5
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PDceil'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ceiling discomf PD (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ceiling discomf PD (% change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ceiling discomf PD (% hours)'
          endif
          MRTLIST=.true.
        elseif(inoc.eq.7) then
          IGETNO(NGET,1)=53
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          MRTLIST=.true.
          H(1) ='The question assumes that the sensor has not been '
          H(2) ='rotated.   '
          CALL EASKABCD('Sensor facing which direction:',' ','South',
     &               'East','North','West',ICF,2)
          IGETNO(NGET,4)=ICF
          SUFFIX=' PDwall'
          if(IROC.eq.0)then
            SLABEL(NGET)='Wall discomf PD (%)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wall discomf PD (% change/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wall discomf PD (% hours)'
          endif
        endif
        idd=1
        izz=1
        CALL MOCMFT(idd,izz,'I')
      elseif(INO.EQ.8)then

C Solar processes.
        inoc=0
        idnoc=0
        h(1)='Solar radiation distribution within a zone is reported'
        h(2)='as solar radiation (W) entering from all outside-facing'
        h(3)='glazing, solar radiation (W) entering from adjacent zones'
        h(4)='and solar radiation absorbed by opaque surfaces within '
        h(5)='the zone. '
        h(6)=' '
        h(7)='Further solar details can be found in the surface energy'
        h(8)='balance reports. '
        call MENUATOL('Select a solar flux metric',' Solar choices:',
     &    'a Solar entering from outside','b Solar entering from adj',
     &    'c Solar absorbed in zone','  ','  ','  ','  ','  ',
     &    '  ','  ','  ','  ',inoc,idnoc,8)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        
C        dok=.false.
C        h(1)='The normal report is for the whole surface. '
C        call askok('Report per square meter of surface',' ',OK,dok,1)
C        if(ok)ISM=1
C        slist=.true.
C        if (ISM.eq.0) then

C Report flux.
C          IGETNO(NGET,4)=inoc
C        else

C Report flux/m^2.
C          IGETNO(NGET,4)=-inoc
C        endif
        
        NGET=NGET+1
        IGETNO(NGET,6)=-104; IGETNO(NGET,8)=2; IGETNO(NGET,9)=12
        if(inoc.eq.1)then

C IGET = 38 Solar entering zone from outside (W)
          IGETNO(NGET,1)=38
          IGETNO(NGET,7)=7
          SUFFIX=' SolinEx'
          if(IROC.eq.0)then
            SLABEL(NGET)='Solar entering from outside (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng solar enter outside (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Solar entering from outside(Whr)'
          endif
        elseif(inoc.eq.2)then

C IGET = 39 Solar entering zone from adj zones (W)
          IGETNO(NGET,1)=39
          IGETNO(NGET,7)=8
          SUFFIX=' SolinAd'
          if(IROC.eq.0)then
            SLABEL(NGET)='Solar entering from adj zn (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng solar enter adj zn (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Solar entering from adj zn (Whr)'
          endif
        elseif(inoc.eq.3)then

C IGET = 40 Solar absorbed in zone (W)
          IGETNO(NGET,1)=40
          IGETNO(NGET,7)=9
          SUFFIX=' TSolabs'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total solar absorbed in zn (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng zn solar absorbed (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total solar absorbed in zn (Whr)'
          endif
        endif
      elseif(INO.EQ.10)then

C Zone flux paths. Additional items could be included if we went
C to an emenu selection for izver >=4. << to be done >>
        inoc=0
        idnoc=0
        h(1)='The zone flux options comprise each of the flux paths '
        h(2)='included in the zone energy balance report. '
        h(3)=' '
        h(4)='Convective exchange with the inside face of surfaces in '
        h(5)='the zone are subdivided as follows: '
        h(6) =' g -opaque surfaces which are connected to ambient air,'
        h(7) =' h -opaque partitions (i.e. other boundary conditions)'
        h(8) =' i -transp surfaces which are connected to ambient air,'
        h(9) =' j -transp partitions (i.e. other boundary conditions)'
        h(10)=' '
        h(11)=' k -is convection at the inside face of all surfaces in'
        h(12)='   the zone.'
        h(13)=' '
        h(14)='Note: there are more options for viewing casual gains'
        h(15)='under the `Casual gains` menu option. What is shown'
        h(16)='here is actually the sensible convective + radiant.'
        call MENUATOL('Select a zone flux metric','Zone flux options:',
     &    'a Infiltration (from outside)','b Ventilation (adj zones)',
     &    'c Occupant casual gains (R+C)',
     &    'd Lighting casual gains (R+C)',
     &    'e Small power casual gains (R+C)',
     &    'f Controlled casual gains (R+C',
     &    'g Opaq surf conv @extrn','h Opaq surf conv @partns',
     &    'i Tran surf conv @extrn','j Tran surf conv @partns',
     &    'k Total surface conv',' ',inoc,idnoc,15)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        NGET=NGET+1
        IGETNO(NGET,8)=2
        IGETNO(NGET,6)=-3
        IGETNO(NGET,9)=12
        if(inoc.eq.1)then

C IGET = 11  Infiltration.
          IGETNO(NGET,1)=11
          IGETNO(NGET,7)=4
          SUFFIX=' Inf'
          if(IROC.eq.0)then
            SLABEL(NGET)='Infiltration (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Infiltration change (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Infiltration (Whr)'
          endif
        elseif(inoc.eq.2)then

C IGET = 12  Ventilation.
          IGETNO(NGET,1)=12
          IGETNO(NGET,7)=9
          SUFFIX=' Vent'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ventilation (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ventilation change (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ventilation (Whr)'
          endif
        elseif(inoc.eq.3)then
C IGET = 15  Casual gains by type.
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=4
          IGETNO(NGET,7)=13
          SUFFIX=' OccCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total occupant gain (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total occupant gain chg (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total occupant gain (Whr)'
          endif
        elseif(inoc.eq.4)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=5
          IGETNO(NGET,7)=14
          SUFFIX=' LitCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total lighting gain (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total lighting gain chg (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total lighting gain (Whr)'
          endif
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=6
          IGETNO(NGET,7)=15
          SUFFIX=' EqpCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total small power gain (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total small power gain (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total small power gain (Whr)'
          endif
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=7
          IGETNO(NGET,7)=16
          SUFFIX=' CtlCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total controled casual gain (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total ctld casual gain (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total ctld casual gain (Whr)'
          endif
        elseif(inoc.eq.7)then

C IGET = 42 Zone balance opaque exterior conv.
          IGETNO(NGET,1)=42
          IGETNO(NGET,7)=17
          SUFFIX=' OpqcvE'
          if(IROC.eq.0)then
            SLABEL(NGET)='Opaque surf exterior conv (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Opaque surf exterior conv (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Opaque surf exterior conv (Whr)'
          endif
        elseif(inoc.eq.8)then

C IGET = 43 Zone balance opaque partitions conv.
          IGETNO(NGET,1)=43
          IGETNO(NGET,7)=7
          SUFFIX=' OpqcvP'
          if(IROC.eq.0)then
            SLABEL(NGET)='Opaque partitions conv (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Opaque partitionns conv (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Opaque partitionns conv (Whr)'
          endif
        elseif(inoc.eq.9)then

C IGET = 44 Zone balance transparent exterior conv.
          IGETNO(NGET,1)=44
          IGETNO(NGET,7)=8
          SUFFIX=' TrncvE'
          if(IROC.eq.0)then
            SLABEL(NGET)='Transp surf exterior conv (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Transp surf exterior conv (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Transp surf exterior conv (Whr)'
          endif
        elseif(inoc.eq.10)then

C IGET = 45 Zone balance transparent partitions conv.
          IGETNO(NGET,1)=45
          IGETNO(NGET,7)=9
          SUFFIX=' TrncvP'
          if(IROC.eq.0)then
            SLABEL(NGET)='Transp partitions conv (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Transp partitions conv (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Transp partitions conv (Whr)'
          endif
        elseif(inoc.eq.11)then
          IGETNO(NGET,1)=16
          IGETNO(NGET,4)=3
          IGETNO(NGET,7)=10
          IGETNO(NGET,8)=2
          SUFFIX=' TotCnv'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total surf convection (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total surf conv (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total surf conv (Whr)'
          endif
        endif
      elseif(INO.EQ.11)then

C IGET = 16,17,18 Surface fluxes
C IGET = 25 Surface flux (various IGET,4) where 1=conduction (inside),
C   2=convec (inside), 3=LW radiation (inside), 4=SW rad absorb inside,
C   5=radiant casual occup, 6=radiant casual light
C   7=radiant casual equip, 8=contrld casual gains, 9=heat storage (inside)
C   10=plant inj/extr (ins), 11=conduction (other fc), 12=convec (other),
C   13=long wave > buildings
C   14=long wave > sky, 15=long wave > ground, 16= SW radiation (othr),
C   17= SW incident radiation (othr),18=heat storage (othr)
C Set up surface flux selection items.
        H(1)='The terms Inside and other side are defined with respect '
        H(2)='to the current zone.  Therefore the other face of a '
        H(3)='surface is not necessarily an exterior surface.'
        H(4)='<< make this clear >> '
        ITEMS(1) ='conduction (inside)    '
        ITEMS(2) ='convection (inside)    '
        ITEMS(3) ='LW radiation (inside)  '
        ITEMS(4) ='SW radiation (inside)  '
        ITEMS(5) ='radiant casual occup   '
        ITEMS(6) ='radiant casual light   '
        ITEMS(7) ='radiant casual equip   '
        ITEMS(8) ='contrld casual gains   '
        ITEMS(9) ='heat storage (inside)  '
        ITEMS(10)='plant inj/extr (inside)'
        ITEMS(11)='conduction (other face)'
        ITEMS(12)='convection (other face)'
        ITEMS(13)='long wave > buildings  '
        ITEMS(14)='long wave > sky        '
        ITEMS(15)='long wave > ground     '
        ITEMS(16)='SW rad abs (other fc)'
        ITEMS(17)='SW rad incid (other fc)'
        ITEMS(18)='heat storage (other fc)'
        IX=1
        CALL EPICKS(IX,IVALS,' ',' Surface fluxes:',
     &         23,18,ITEMS,'surface fluxes',IER,4)
        if(IVALS(1).eq.0)goto 1
        dok=.false.
        h(1)='The normal report is for the whole surface. '
        call askok('Report per square meter of surface',' ',OK,dok,1)
        ism=0
        if(ok)ism=1
        inoc=IVALS(1)
        slist=.true.
        NGET=NGET+1
        if (ISM.eq.0) then

C Report flux.
          IGETNO(NGET,4)=inoc
        else

C Report flux/m^2.
          IGETNO(NGET,4)=-inoc
        endif
        IGETNO(NGET,8)=2
        IGETNO(NGET,6)=-3
        IGETNO(NGET,9)=20
        if(inoc.eq.1)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=17
          SUFFIX=' InSfCnd'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Conduction @ an inside face (W)'
            else
              SLABEL(NGET)='Conduction @an ins. face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then         
              SLABEL(NGET)='Cond chng @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Cond chng @ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Cond chng @ an ins face (Whr)'
            else
              SLABEL(NGET)='Cond chng @an ins face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=8
          SUFFIX=' InSfCnv'
          if(IROC.eq.0)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Convection @ an inside face (W)'
            else
              SLABEL(NGET)='Convection @ inside face (W/m^2)'
            endif          
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Conv chng @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Conv chng @ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Convection @ an ins face (Whr)'
            else
              SLABEL(NGET)='Convection @ ins face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.3)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' InSfLW'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad @ an inside face (W)'
            else
              SLABEL(NGET)='LW rad @ an inside face (W/m^2)'
            endif            
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW chng @ an ins face (W/hr)'
            else
              SLABEL(NGET)='LW chng @ ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad @ an ins face (Whr)'
            else
              SLABEL(NGET)='LW rad @ an ins face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.4)then
          slist=.true.
          IGETNO(NGET,1)=25
          IGETNO(NGET,6)=-104
          IGETNO(NGET,7)=10
          SUFFIX=' InSolAb'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs inside (W)'
            else
              SLABEL(NGET)='Surface solar abs inside (W/m^2)'
            endif       
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs inside (W/hr)'
            else
              SLABEL(NGET)='Surf solar abs ins (W/(hr*m^2))'
            endif    
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs inside (Whr)'
            else
              SLABEL(NGET)='Surf solar abs inside (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=11
          SUFFIX=' InSfOcc'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an inside face (W)'
            else
              SLABEL(NGET)='Occup rad @ an ins face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Occup rad @ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an ins face (Whr)'
            else
              SLABEL(NGET)='Occup rad @an ins face (Whr/m^2)'
            endif         
          endif
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=12
          SUFFIX=' InSfLts'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Lights rad @ an inside face (W)'
            else
              SLABEL(NGET)='Lights rad @ inside face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Lights rad @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Lights rad@ins face (W/(hr*m^2))'
            endif           
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Lights rad @ an ins face (Whr)'
            else
              SLABEL(NGET)='Lights rad @ ins face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.7)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=13
          SUFFIX=' InSfEqp'
          if(IROC.eq.0)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Sm Pwr rad @ an inside face (W)'
            else
              SLABEL(NGET)='Sm Pwr rad @ inside face (W/m^2)'            
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Sm Pwr rad @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Sm Pwr rad@ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Sm Pwr rad @ an ins face (Whr)'
            else
              SLABEL(NGET)='Sm Pwr rad @ ins face (Whr/m^2)'            
            endif
          endif
        elseif(inoc.eq.8)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=14
          SUFFIX=' InSfCtC'
          if(IROC.eq.0)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Ctld cas @ an inside face (W)'
            else
              SLABEL(NGET)='Ctld cas @an inside face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ctld cas @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Ctld cas @ ins face (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ctld cas @ an ins face (Whr)'
            else
              SLABEL(NGET)='Ctld cas @ an ins face (Whr/m^2)'
            endif           
          endif
        elseif(inoc.eq.9)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=15
          SUFFIX=' InSfHtS'
          if(IROC.eq.0)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Heat stored @ an inside face (W)'
            else
              SLABEL(NGET)='Heat stored @an ins face (W/m^2)'            
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ht str chng @ an ins face (W/hr)'
            else
              SLABEL(NGET)='Ht str chng @ins face W/(hr*m^2)'
            endif          
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Ht str chng @ an ins face (Whr)'
            else
              SLABEL(NGET)='Ht str chng @ ins face (Whr/m^2)'
            endif         
          endif
        elseif(inoc.eq.10)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=16
          SUFFIX=' InSfPlt'
          if(IROC.eq.0)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Rad plant @ an inside face (W)'
            else
              SLABEL(NGET)='Rad plant @ inside face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Rad plt chng @ ins face (W/hr)'
            else
              SLABEL(NGET)='Rad plt chng@ins face W/(hr*m^2)'
            endif           
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Rad plant @ inside face (Whr)'
            else
              SLABEL(NGET)='Rad plant @inside face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.11)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=17
          SUFFIX=' ExSfCnd'
          if(IROC.eq.0)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Conduction @ other face (W)'
            else
              SLABEL(NGET)='Conduction @ other face (W/m^2)'              
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Cond chng @ other face (W/hr)'
            else
              SLABEL(NGET)='Cond chng @other face W/(hr*m^2)'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Conduction @ other face (Whr)'
            else            
              SLABEL(NGET)='Conduction @other face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.12)then
          slist=.true.
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=16
          SUFFIX=' ExSfCnv'
          if(IROC.eq.0)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Convection @ other face (W)'
            else
              SLABEL(NGET)='Convection @ other face (W/m^2)'
            endif      
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Conv chng @other face (W/hr)'
            else
              SLABEL(NGET)='Conv chng @other face W/(hr*m^2)'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Convection @other face (Whr)'
            else
              SLABEL(NGET)='Convection @other face (Whr/m^2)'
            endif           
          endif
        elseif(inoc.eq.13)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWb'
          if(IROC.eq.0)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='LW rad > bld @ other face (W)'
            else
              SLABEL(NGET)='LW rad > bld @other face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='LW>bld chng @ other face (W/hr)'
            else
              SLABEL(NGET)='LW>bld chng @oth face W/(hr*m^2)'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > bld @ other face (Whr)'
            else
              SLABEL(NGET)='LW rad >bld@other face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.14)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWs'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > sky @ other face (W)'
            else
              SLABEL(NGET)='LW rad > sky @other face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW>sky chng @ other face (W/hr)'
            else
              SLABEL(NGET)='LW>sky chng @oth face W/(hr*m^2)'
            endif            
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='LW rad > sky @ other face (Whr)'
            else
              SLABEL(NGET)='LW rad >sky@other face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.15)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWg'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > grnd @ other face (W)'
            else            
              SLABEL(NGET)='LW rad > grnd@other face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW>grnd chng @ other face (W/hr)'
            else
              SLABEL(NGET)='LW>grnd chng@oth face W/(hr*m^2)'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > grnd @ other face (Whr)'
            else
              SLABEL(NGET)='LW rad>grnd@other face (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.16)then
          slist=.true.
          IGETNO(NGET,1)=25
          IGETNO(NGET,6)=-104
          IGETNO(NGET,7)=10
          SUFFIX=' ExSolAb'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs outside (W)'
            else
              SLABEL(NGET)='Surf solar abs outside (W/m^2)'              
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs outside (W/hr)'
            else
              SLABEL(NGET)='Surf solar abs outs (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs outside (Whr)' 
            else
              SLABEL(NGET)='Surf solar abs outside (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.17)then
          slist=.true.
          IGETNO(NGET,1)=25
          IGETNO(NGET,6)=-104
          IGETNO(NGET,7)=10
          SUFFIX=' ExSolAb'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar incid outside (W)'
            else
              SLABEL(NGET)='Surf solar incid outside (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar incid outs (W/hr)'
            else
              SLABEL(NGET)='Surf solar inc outs (W/(hr*m^2))'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar incid outs (Whr)'
            else
              SLABEL(NGET)='Surf solar incid outs (Whr/m^2)'
            endif
          endif
        elseif(inoc.eq.18)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=15
          SUFFIX=' InSfHtS'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Heat stored @ other face (W)'  
            else                    
              SLABEL(NGET)='Heat stored @ other face (W/m^2)'
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Ht str chng @ other face (W/hr)'
            else                   
              SLABEL(NGET)='Ht str chng @oth face W/(hr*m^2)'
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Heat stored @ other face (Whr)'
            else                      
              SLABEL(NGET)='Heat str @ other face (Whr/m^2)'
            endif
          endif
        endif
      elseif(INO.EQ.12)then
        H(1)='You can display either the sensible heating or'
        H(2)='cooling load for the selected zone (kW). '
        H(3)='You can also display either the dehumidification'
        H(4)='or humidification load for the selected zone (kW).'
        H(5)='  '
        H(6)='You can also display :  '
        H(7)=' All sensible loads (heating and cooling) '
        H(8)=' All latent loads (humidification and dehumidification)'
        H(9)=' The total load (all sensible plus all latent). '
        H(10)=' '
        H(11)=' All values are in kW. '
        inoc=0
        idnoc=0
        call MENUATOL('Select an HVAC load metric',' Load choices:',
     &    'a Sensible heating load','b Sensible cooling load',
     &    'c Dehumidification load','d Humidification load',
     &    'e All sensible loads','f All latent loads',
     &    'g All Sensible + latent load','  ',
     &    '  ','  ','  ','  ',inoc,idnoc,11)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        NGET=NGET+1
        IGETNO(NGET,8)=2
        IGETNO(NGET,9)=12
        if(inoc.eq.1)then

C IGET = 8   Zone-side sensible heating load.
          IGETNO(NGET,1)=8
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' HeatInj'
          if(IROC.eq.0)then
            SLABEL(NGET)='Sensible heating load (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Sensible heating load ch (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Sensible heating load (kWhr)'
          endif
        elseif(inoc.eq.2)then

C IGET = 9 Zone-side sensible cooling load.
          IGETNO(NGET,1)=9
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' CoolInj'
          if(IROC.eq.0)then
            SLABEL(NGET)='Sensible cooling load (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Sensible cooling load ch (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Sensible cooling load (kWhr)'
          endif
        elseif(inoc.eq.3)then

C IGET = 30   Dehumidification load.
          IGETNO(NGET,1)=30
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' LatExt'
          if(IROC.eq.0)then
            SLABEL(NGET)='Latent dehumidification (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Latent dehumidification (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Latent dehumidification (kWhr)'
          endif
        elseif(inoc.eq.4)then

C IGET = 31   Humidification load.
          IGETNO(NGET,1)=31
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' LatInj'
          if(IROC.eq.0)then
            SLABEL(NGET)='Latent humidification (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Latent humidification (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Latent humidification (kWhr)'
          endif
        elseif(inoc.eq.5)then

C IGET = 10  Total sensible zone loads. 
          IGETNO(NGET,1)=10
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotSens'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone sensible load (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone sensible load chg (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone sensible load (kWhr)'
          endif
        elseif(inoc.eq.6)then

C IGET = 32  All latent zone loads.
          IGETNO(NGET,1)=32
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotLat'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone latent load (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone latent load change (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone latent load (kWhr)'
          endif
        elseif(inoc.eq.7)then

C IGET = 33  All sensible+latent zone loads.
          IGETNO(NGET,1)=33
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotSenL'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone sensible + latent load (kW)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zn sens+latent load chng (kW/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zn sens+latent load (kWhr)'
          endif
        endif
      elseif(INO.EQ.13)then

C IGET = 13  Zone RH.
        NGET=NGET+1
        IGETNO(NGET,1)=13
        IGETNO(NGET,6)=-202; IGETNO(NGET,7)=4
        IGETNO(NGET,8)=4;    IGETNO(NGET,9)=12
        SUFFIX=' RH'
        if(IROC.eq.0)then
          SLABEL(NGET)='Zone relative humidity (%)'
        elseif(IROC.eq.1)then
          SLABEL(NGET)='Zn relative humidity chg (%/hr)'
        elseif(IROC.eq.-1)then
          SLABEL(NGET)='Zn relative humidity (%hr)'
        endif
      elseif(INO.EQ.14)then

C IGET = 15  Casual gains.
        if(izver.eq.4)then
          H(1)='Casual gains are reported separately in terms of their'
          H(2)='convective radiant and latent contributions. The first'
          H(3)='casual gain type is labeled Occupant, the 2nd casual '
          H(4)='gain type is labeled Lighting, the 3rd is Equipment.'
          H(5)=' '
          H(6)='The casual gains reported include any controls that '
          H(7)='that have been applied. If you want to see only the '
          H(8)='part of a casual gain that is controlled use the '
          H(9)='options labeled control. '
          H(10)=' '
          H(11)=' * All sensible casual gains (convective + radiant)'
          H(12)=' * All sensible convective casual gains'
          H(13)=' * All sensible radiant casual gains'
          H(14)=' * All latent gains'
          H(15)=' * Occupant sensible gains (convective + radiant)'
          H(16)=' * Occupant sensible convective gains'
          H(17)=' * Occupant sensible radiant gains'
          H(18)=' * Occupant latent gains'
          H(19)=' * Lighting sensible gains (convective + radiant)'
          H(20)=' * Lighting sensible convective gains'
          H(21)=' * Lighting sensible radiant gains'
          H(22)=' * Lighting latent gains'
          H(23)=' * Equipment sensible gains (convective + radiant)'
          H(24)=' * Equipment sensible convective gains'
          H(25)=' * Equipment sensible radiant gains'
          H(26)=' * Equipment latent gains'
          H(27)=' * Controlled fraction (typically of lights)'
          H(28)=' * Controlled sensible gains (convective + radiant)'
          H(29)=' * Controlled sensible convective gains'
          H(30)=' * Controlled sensible radiant gains'
          H(31)=' * Controlled latent gains'
          ITEMC(1)='all sensible (conv+rad)'
          ITEMC(2)='all sensible convective'
          ITEMC(3)='all sensible radiant '
          ITEMC(4)='all latent '
          ITEMC(5)='occupant (conv+rad) '
          ITEMC(6)='occupant convective '
          ITEMC(7)='occupant radiant '
          ITEMC(8)='occupant latent '
          ITEMC(9)='lighting (conv+rad) '
          ITEMC(10)='lighting convective '
          ITEMC(11)='lighting radiant '
          ITEMC(12)='lighitng latent '
          ITEMC(13)='equipment (conv+rad) '
          ITEMC(14)='equipment convective '
          ITEMC(15)='equipment radiant '
          ITEMC(16)='equipment latent '
          ITEMC(17)='controlled fraction '
          ITEMC(18)='controlled (conv+rad) '
          ITEMC(19)='controlled convective '
          ITEMC(20)='controlled radiant'
          ITEMC(21)='controlled latent'
          IX=1
          CALL EPICKS(IX,IVALC,' ',' Casual sensible and latent:',
     &      24,21,ITEMC,'Casuaul fluxes',IER,31)
          if(IVALC(1).eq.0)goto 1
          inoc=IVALC(1)
          call usrmsg('  ','  ','-')
          NGET=NGET+1
          IGETNO(NGET,1)=15
          IGETNO(NGET,8)=2
          IGETNO(NGET,9)=12
          if(inoc.eq.1)then
            IGETNO(NGET,3)=1   ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=12
            SUFFIX=' TotCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total casual gain change (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain (Whr)'
            endif
          elseif(inoc.eq.2)then
            IGETNO(NGET,3)=2   ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=10
            SUFFIX=' CnvCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain convective (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gain conv chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Tot casual gain convective (Whr)'
            endif
          elseif(inoc.eq.3)then
            IGETNO(NGET,3)=3   ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' RadCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain radiant (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn radiant chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain radiant (Whr)'
            endif
          elseif(inoc.eq.4)then
            IGETNO(NGET,3)=9   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' LatCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain latent (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn latent chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain latent (Whr)'
            endif
          elseif(inoc.eq.5)then
            IGETNO(NGET,3)=4  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total occupant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total occupant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total occupant gain (Whr)'
            endif
          elseif(inoc.eq.6)then
            IGETNO(NGET,3)=10   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant convec gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant convec gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant convec gain (Whr)'
            endif
          elseif(inoc.eq.7)then
            IGETNO(NGET,3)=11   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant radiant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant radiant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant radiant gain (Whr)'
            endif
          elseif(inoc.eq.8)then
            IGETNO(NGET,3)=12   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant latent gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant latent gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant latent gain (Whr)'
            endif
          elseif(inoc.eq.9)then
            IGETNO(NGET,3)=5  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total lighting gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total lighting gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total lighting gain (Whr)'
            endif
          elseif(inoc.eq.10)then
            IGETNO(NGET,3)=13   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting convec gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting convec gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting convec gain (Whr)'
            endif
          elseif(inoc.eq.11)then
            IGETNO(NGET,3)=14   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting radiant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting radiant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting radiant gain (Whr)'
            endif
          elseif(inoc.eq.12)then
            IGETNO(NGET,3)=15   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting latent gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting latent gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting latent gain (Whr)'
            endif
          elseif(inoc.eq.13)then
            IGETNO(NGET,3)=6  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total small power gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total small power gain (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total small power gain (Whr)'
            endif
          elseif(inoc.eq.14)then
            IGETNO(NGET,3)=16   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr convec gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Small pwr convec gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr convec gain (Whr)'
            endif
          elseif(inoc.eq.15)then
            IGETNO(NGET,3)=17   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr radiant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Sml pwr radiant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr radiant gain (Whr)'
            endif
          elseif(inoc.eq.16)then
            IGETNO(NGET,3)=18   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr latent gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Small pwr latent gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr latent gain (Whr)'
            endif
          elseif(inoc.eq.17)then
            IGETNO(NGET,3)=8  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Frac of ctld casual gain (%)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Frac of ctld casual gain (%/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Frac of ctld casual gain (%hr)'
            endif
          elseif(inoc.eq.18)then
            IGETNO(NGET,3)=7  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total controled casual gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total ctld casual gain (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total ctld casual gain (Whr)'
            endif
          elseif(inoc.eq.19)then
            IGETNO(NGET,3)=19   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Controlled convec gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Cntrld convec gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Controlled convec gain (Whr)'
            endif
          elseif(inoc.eq.20)then
            IGETNO(NGET,3)=20   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Controlled radiant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Cntrld radiant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Controlled radiant gain (Whr)'
            endif
          elseif(inoc.eq.21)then
            IGETNO(NGET,3)=21   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Controlled latent gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Cntrld latent gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Controlled latent gain (Whr)'
            endif
          endif
        else
          H(1)='Casual gains are reported separately in terms of their'
          H(2)='convective and radiant contributions. '
          H(3)='The total gain (rad + conv) for a single type, or '
          H(4)='for the controlled type is given.  The fraction of'
          H(5)='the controlled type (delivered/maximum *100%) can '
          H(6)='also be reported.'
          H(7)='<< expand the list if explicit values available >> '
          inoc=0
          idnoc=0
          call MENUATOL('Select a casual gain metric',' Choices:',
     &      'a All gains','b Convective portion','c Radiant portion',
     &      'd Total occupant gain','e Total lighting gain',
     &      'f Total small power gain','g Total controlled gain',
     &      'h Controlled fraction','  ','  ','  ','  ',inoc,idnoc,6)
          if(inoc.eq.0)goto 1
          call usrmsg('  ','  ','-')
          NGET=NGET+1
          IGETNO(NGET,1)=15
          IGETNO(NGET,8)=2; IGETNO(NGET,9)=12
          if(inoc.eq.1)then
            IGETNO(NGET,3)=1
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=12
            SUFFIX=' TotCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total casual gain change (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain (Whr)'
            endif
          elseif(inoc.eq.2)then
            IGETNO(NGET,3)=2
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=10
            SUFFIX=' CnvCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain convective (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gain conv chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Tot casual gain convective (Whr)'
            endif
          elseif(inoc.eq.3)then
            IGETNO(NGET,3)=3
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' RadCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain radiant (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn radiant chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain radiant (Whr)'
            endif
          elseif(inoc.eq.4)then
            IGETNO(NGET,3)=4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total occupant gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total occupant gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total occupant gain (Whr)'
            endif
          elseif(inoc.eq.5)then
            IGETNO(NGET,3)=5
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total lighting gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total lighting gain chg (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total lighting gain (Whr)'
            endif
          elseif(inoc.eq.6)then
            IGETNO(NGET,3)=6
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total small power gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total small power gain (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total small power gain (Whr)'
            endif
          elseif(inoc.eq.7)then
            IGETNO(NGET,3)=7
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total controled casual gain (W)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total ctld casual gain (W/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total ctld casual gain (Whr)'
            endif
          elseif(inoc.eq.8)then
            IGETNO(NGET,3)=8
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Frac of ctld casual gain (%)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Frac of ctld casual gain (%/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Frac of ctld casual gain (%hr)'
            endif
          elseif(inoc.eq.9)then

C Test.
            IGETNO(NGET,3)=9
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Various casual gains (%)'
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Various casual gains (%/hr)'
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Various casual gain (%hr)'
            endif
          endif
        endif
      elseif(INO.EQ.15)then

C Electrical power flow.
C IGET = 27 Real power flow.
C IGET = 28 Reactive power flow.
         H(1)='To select more than one item, e.g. real+reactive '  
         H(2)='reselect the electrical option.'
        CALL EASKAB(' ','Power flow type:','Real','Reactive',jpwr,2)

C Should set logicals to ask for cas gain type and phase - see surface 
C and node logic below.
        NGET=NGET+1
        IGETNO(NGET,8)=2
        IGETNO(NGET,9)=12
        if (jpwr.eq.1) then
          IGETNO(NGET,1)=27
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=5
          SUFFIX=' RealPwr'
          if(IROC.eq.0)then
            SLABEL(NGET)='Real power consumption (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Real power consump chg (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Real power consumption (Whr)'
          endif
        elseif (jpwr.eq.2) then
          IGETNO(NGET,1)=28
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' ReacPwr'
          if(IROC.eq.0)then
            SLABEL(NGET)='Reactive power consumption (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Reactive power consmp chg (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Reactive power consmption (Whr)'
          endif
        endif

      elseif(INO.EQ.17)then

C Renewables/special components output.
C IGET = 46 component main output.
C IGET = 47 component secondary output.
         H(1)='This option displays the ouput from embedded renewables'
         H(2)='or advanced materials. Choose to display the main '
         H(3)='output, usually heat or power (W) or a secondary '
         H(4)='output. Note this option will display a maximum of '
         H(5)='ONE renewable or material metric per zone.'
        CALL EASKAB(' ','Choose a component performance metric',
     &'Heat/Power output','Misc. output',ispm,5)

C Should set logicals to ask for cas gain type and phase - see surface 
C and node logic below.
        NGET=NGET+1

        IGETNO(NGET,9)=12
        if (ispm.eq.1) then
          IGETNO(NGET,8)=4
          IGETNO(NGET,1)=46
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=5
          SUFFIX=' Output'
          if(IROC.eq.0)then
            SLABEL(NGET)=' Output (W)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)=' Output chg (W/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)=' Output (Whr)'
          endif
        elseif (ispm.eq.2) then
          IGETNO(NGET,8)=4
          IGETNO(NGET,1)=47
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' Secndry'
          if(IROC.eq.0)then
            SLABEL(NGET)=' Misc. output (-)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)=' Misc. output chg (-/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)=' Misc. output hours (-.hr)'
          endif
        endif

      elseif(INO.EQ.18)then

C Network flow.
        call mfoutp('-')
        goto 1
      elseif(INO.EQ.19)then

C CFD flow.
        call edisp(iuout,'Not yet available here.')
        goto 1
      elseif(INO.EQ.20)then

C Measured data.
C Measured zone db T, select associated zone or if measured
C surface T select the zone and surface.
C << give the users clues about which zones have measured temps >>
C << make this into a selection list, but if there is only one
C << then it could be automatically selected
        do 188 loop=1,NZ
          if(IDBTZNM(loop).ne.0)then
            write(outs,'(2a)') 'measured db T in ',
     &        ZNAME(loop)(1:lnzname(loop))
            call edisp(iuout,outs)
          endif
          do 189 kk=1,nzsur(loop)
            if(ISURTMEAS(loop,kk).ne.0)then
              write(outs,'(3a,i2)') 'measured surf T in ',
     &          ZNAME(loop)(1:lnzname(loop)),' surf ',kk
              call edisp(iuout,outs)
            endif
  189     continue
  188   continue
        CALL EASKAB('Which measured data type',
     &    ' :','zone db T','surface T',IR,0)
        NGET=NGET+1
        if(ir.eq.1)then
          call ASKZON(IZONE)
          IGETNO(NGET,1)=80
          IGETNO(NGET,2)=IZONE
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' db T'
          GLABEL(NGET)='Measured db Tmp'
          if(IROC.eq.0)then
            SLABEL(NGET)='Measured zone db T (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Meas zone db T change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Meas zone db T hours (degC.hr)'
          endif
        elseif(ir.eq.2)then
          call ASKZON(IZONE)
          call ASKSUR(IZONE,ISFN)
          IGETNO(NGET,1)=81
          IGETNO(NGET,2)=IZONE
          IGETNO(NGET,3)=ISFN
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' surf T'
          GLABEL(NGET)='Measured surf T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Measured surf T (degC)'
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Meas surf T change (degC/hr)'
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Meas surf T hours (degC.hr)'
          endif

        endif

C << ****** >>
      elseif(INO.EQ.22)then

C Edit selections.
        if(NGET.gt.0)then
          call edgoget
          goto 1
        endif
      elseif(INO.EQ.23)then

C Output redirect.
        if(MENUTYPE.eq.3)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Graphics',IER)
        elseif(MENUTYPE.eq.4)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
        else
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Text',IER)
        endif

C If user request cancel then return after resetting ixopen to zero.
        if(ier.eq.-3)then
          ixopen=0
          goto 1
        endif
      elseif(INO.EQ.24)then

C Data presentation format (actual or rate of change).
        if(IROC.eq.0)then
          write(outs,'(a)')'(currently variable)'
        elseif(IROC.eq.1)then
          write(outs,'(a)')'(currently rate of change)'
        elseif(IROC.eq.-1)then
          write(outs,'(a)')'(currently integrated)'
        endif
        h(1)='Typical results recovery are of the values within the'
        h(2)='results library. An alternative is to report the rate '
        h(3)='of change of a variable. The rate of change is expressed'
        h(4)='in units per hour (of the normal unit for the variable).'
        h(5)=' '

C The following lines do not allow casual access to the 'integrate data' 
C functionality.  The commented out lines just below activate this feature.
C        h(6)='Note: all selections cleared when this option is altered.'
C        CALL EASKABC('Data display options:',outs,
C     &    'variable (default)','rate of change of variable','continue',
C     &    II,6)
C        if(II.ne.3.and.(IROC.ne.(II-1)))then 
C         IROC=II-1
        h(6)='If integrating data the value is espressed as units.hour '
        h(7)='For example a flux (W) will become energy (Wh). '
        h(8)=' '
        h(9)='Note: all selections cleared when this option is altered.'
        CALL EASKABCD('Data display options:',outs,'integrated',
     &    'variable (default)','rate of change of variable','continue',
     &    II,9)
        if(II.ne.4.and.(IROC.ne.(II-2)))then 
          IROC=II-2
          call usrmsg ('Clearing current selections....','  ','W')
          call GOMSETUP
        endif
      elseif(INO.EQ.25)then

C Clear all selections (time series plot).
        if(MENUTYPE.eq.3) then
          call GOMSETUP
        else

C Occupancy filter (all types bar time series plot).
          if(iocupf.eq.0)then

C Casual gain type.  Ask user which constitutes occupancy.
C Labels for loads, use first zones labels as examples. If time
C based set iocut=-1, if always occup set iocut=0.
            write(ll1,'(1x,a6,1x)') lodlabel(NZNO(1),1)
            write(ll2,'(1x,a6,1x)') lodlabel(NZNO(1),2)
            write(ll3,'(1x,a6,1x)') lodlabel(NZNO(1),3)
            call edisp(iuout,' ')
            H(1)='Select a casual gain type to base occupancy on.'
            H(2)='Labels show are from the first zone in the current'
            H(3)='list. Type 1 usually represents casual gains from '
            H(4)='people,although any of the three casual gain types'
            H(5)='can be used. '
            CALL EASKATOG('Which casual gain represents occupancy:',
     &         '  ','Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,5)
            if (IOT.gt.1.and.IOT.lt.5) then
              call usrmsg('  ','  ','-')
              iocupf=1
              iocut=IOT-1
              call scnocup(iier)
              if (iocut.ne.IFILT)then
                call usrmsg('Rescanning database... ',' ','-')
                call MMAPER
                call usrmsg('Rescanning database...done.',' ','P')
                call usrmsg('  ','  ','-')
              endif
              IFILT=iocut
            elseif(IOT.eq.5)then
              iocupf=1
              iocut=-1
              call scnocup(iier)
              if (iocut.ne.IFILT)then
                call usrmsg('Rescanning database... ',' ','-')
                call MMAPER
                call usrmsg('Rescanning database...done.',' ','P')
                call usrmsg('  ','  ','-')
              endif
              IFILT=iocut
            elseif(IOT.eq.1)then
              iocupf=0
              iocut=0
              call scnocup(iier)
            endif
          else
            iocupf=0
          endif
        endif
      elseif(INO.EQ.26)then
        if(MENUTYPE.eq.3) then

C Set axis limits.
          H(1)='Enter the minimum and maximum value for the y-axis.'
          do 555 Iaxis=1,4
            if (YAXSET(Iaxis).gt.0) then
 501          write (values,'(8(f9.2,1x))') YMIN(Iaxis),YMAX(Iaxis)
              if (Iaxis.eq.1) then
                call EASKS(values,'  ','Min &max for temperature axis?',
     &                                 20,' -10 30 ','temp axis',IER,1)
              elseif (Iaxis.eq.2) then
                call EASKS(values,'  ','Min & max for loads axis?',
     &                               20,' -100 100 ','temp axis',IER,1)
              elseif (Iaxis.eq.3) then
                call EASKS(values,'  ','Min & max for intensity axis?',
     &                                 20,' 0 1000 ','temp axis',IER,1)
              elseif (Iaxis.eq.4) then
                call EASKS(values,'  ','Min & max for misc axis?',
     &                                 20,' 0 100 ','temp axis',IER,1)
              endif
              call usrmsg('  ','  ','-')
              K=0
              CALL EGETWR(values,K,YN,0.,0.,'-','Y min',IER)
              CALL EGETWR(values,K,YM,0.,0.,'-','Y max',IER)
              if (YN.gt.YM) then
                call usrmsg('Maximum value is less than minimum value.',
     &                    ' Please respecify. ','W')
                goto 501
              endif
              YMAX(Iaxis)=YM
              YMIN(Iaxis)=YN
              YAXSET(Iaxis)=2
            endif
 555      continue

C Redraw graph (if applicable).
          if (NGET.gt.0) then
            if (YAXSET(1).eq.2.or.YAXSET(2).eq.2.or.YAXSET(3).eq.2.or.
     &                                           YAXSET(4).eq.2) return
          endif
        else

C Toggle time format and day separators.
          IHFLAG=IHFLAG+1
          if(IHFLAG.GT.1)IHFLAG=0
          if(IHFLAG.eq.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'standard display time = 10h30 set ')
          elseif(IHFLAG.eq.1)then
            call edisp(iuout,' ')
            call edisp(iuout,'3rd party graphing time=day fraction set')
          endif
          H(1)='Places a # separator between days of tabular data.'
          CALL EASKAB('Include mark between days when ',
     &                'displaying or writing data :','no','yes',IDH,1)
          IDHFLG=IDH-1
        endif
      elseif(INO.EQ.27)then
        if(MENUTYPE.eq.3) then

C Add another selection - time series plot only.
          INO=-4
          ADDLIN=1
        else

C Toggle delimeter.
          H(1) ='Tabular data can be sent to file with various '
          H(2) ='delimiters: '
          H(3) ='  spaces (format using spaces to lineup columns)'
          H(4) ='  single space between columns'
          H(5) ='  comma separator (for excel)'
          H(6) ='  tab separator (for excel)'
          H(7) ='  tagged - marked up data'
          CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &      'normal spaces','single space','tab','comma','tagged',
     &      'continue',' ',IWM,7)
          if(iwm.eq.1)then
            delim = '-'
          elseif(iwm.eq.2)then
            delim = 'S'
          elseif(iwm.eq.3)then
            delim = 'T'
          elseif(iwm.eq.4)then
            delim = 'C'
          elseif(iwm.eq.5)then
            delim = 'X'
          endif
        endif
      elseif(INO.eq.28.and.nl.ne.0) then

C Debug...
C        do 999 I=1,NGET
C          write(6,*) (IGETNO(I,K),K=1,9)
C          write(6,*) SLABEL(I)
C          write(6,*) GLABEL(I)
C          write(6,*) '  '
C 999    continue

        if (NGET.lt.1) then
          call usrmsg('No data to list/ plot!',
     &                'Please choose a metric.','W')
          INO=-3
        else

C Write GOGET info to file (so that current data recovery can be
C replicated).
          if(pifltog(1:3).eq.'ON ')call WRITEPIF(MENUTYPE)
          return
        endif
      elseif(INO.EQ.(NITMS-2).and.MENUTYPE.eq.3)then

C Add in a title for this graph 64 char string graftitle.
 290    H(1)='A title (up to 64 characters) can be included. '
        t64=graftitle
        CALL EASKS(t64,' Title for graph ? ',
     &     ' ',64,'  ','graph title',IER,1)
        if(t64(1:2).ne.'  '.and.t64(1:4).ne.'UNKN')then
          graftitle=t64
        else
          goto 290
        endif

C Add or clear labels within a graph.
        if(ngrlbl.eq.0)then
          CALL EASKAB('Labels within graph options:',' ',
     &    'edit/add labels','contine',igopt,0)
          if(igopt.eq.1)then
            igrl=0
            grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '
            grlbl(4)=' '; grlbl(5)=' '
          elseif(igopt.eq.2)then
            goto 1
          endif
        elseif(ngrlbl.gt.0)then
          CALL EASKABC('Labels within graph options:',' ',
     &    'clear existing labels','edit/add labels','contine',igopt,0)
          if(igopt.eq.1)then
            ngrlbl=0
            igrl=0
            grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '
            grlbl(4)=' '; grlbl(5)=' '
            goto 1
          elseif(igopt.eq.2)then
            igrl=0
          elseif(igopt.eq.3)then
            goto 1
          endif
        endif
 291    continue

C If there are no labels or some label start at first, if more than 5 exit.
        if(ngrlbl.eq.0)then
          ngrlbl=1
          igrl=1
        elseif(ngrlbl.gt.5)then
          call usrmsg('Array of labels is full.',' ','W')
          goto 1
        else
          igrl=igrl+1
        endif
        t24=grlbl(igrl)
 292    CALL EASKS(t24,
     &   'Label within graph (AFTER editing select a point on graph',
     &   'for the label position).',24,'x','graph label',IER,1)
        if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
          grlbl(igrl)=t24
          if(igrl.gt.ngrlbl)ngrlbl=igrl
        else
          goto 292
        endif

C Find out point where text should be drawn.
        CALL trackview(ii,ix,iy)
        lblpx(igrl)=ix
        lblpy(igrl)=iy
        t24=grlbl(igrl)
        iid1=lblpx(igrl); iid2=lblpy(igrl);
        call wstxpt(iid1,iid2,t24)
        dok=.false.
        h(1)='If you say yes you can add another label and if you say'
        h(2)='no you will be returned to the menu. NOTE you might need'
        h(3)='to re-draw the graph to see the labels and titles. '
        CALL ASKOK('Include another user supplied label?',' ',OKL,dok,3)
        if(OKL) goto 291
      elseif(INO.EQ.(NITMS-1))then

C Show help message for the current menu type.
        CALL PHELPD(MTITLE,nbofhelp,'-',0,0,IER)
      elseif(INO.EQ.NITMS)then
        call usrmsg('  ','  ','-')
        MENUTYPE=-1
        return
      else
        goto 1
      endif

C << ******* >>
C Check for new pick.
      if (INO.lt.0.or.NGET.lt.1) goto 1

C If sensitivity allow only one zone.
      if (MENUTYPE.eq.6.and.NZ.gt.1) then
        call usrmsg('Only one zone can be analysed. ','  ','-')
        CALL MOZDFN
      endif

C Check at least one zone selected.
 99   if (NZ.lt.1) then
        call usrmsg('One or more zones must be selected. ','  ','-')
        CALL MOZDFN
      endif

C Reset local zone and surface selections, NSEL is total number selected.
      do 5 loop=1,MCON
        IZNSEL(loop)=0
        ISFSEL(loop)=0
 5    continue
      NSEL=0

C Copy data for multi zones, surfaces and nodes. In the
C case of climate data or measured data GLABEL has already been set.
      if (IGETNO(NGET,1).eq.2.or.IGETNO(NGET,1).eq.19.or.
     &    IGETNO(NGET,1).eq.20.or.IGETNO(NGET,1).eq.21.or.
     &    IGETNO(NGET,1).eq.22.or.IGETNO(NGET,1).eq.26.or.
     &    IGETNO(NGET,1).eq.34.or.IGETNO(NGET,1).eq.80.or.
     &    IGETNO(NGET,1).eq.81) then

C Do nothing.
        continue
      else

C Create a list of zones and surfaces from current information, asking 
C user to fill in data where necessary.
        IER=0
        if (SLIST.and.(.not.NLIST)) then

C Check if anchor points defined.
          IAC=0
          if (NALOC.gt.0) then
            H(1)='`one at a time` presents a list of surfaces for each'
            H(2)='of the currently selected zones. '
            H(3)='The anchor points method finds all surfaces related'
            H(4)='to an anchor point. '
            CALL EASKAB('Options for surface selection:','  ',
     &        'one at a time','via anchor point list',IAC,4)
          else
            IAC=1
          endif

C Either ask for surfaces zone by zone or ask for a specific anchor point
C depending on value of IAC.
          if (IAC.eq.1) then
            do 600 I=1,NZ
              IZONE=NZNO(I)
              call PICKSUR(IZONE,NSP,ISP,IER)
              if (NSP.gt.0) then
                do 610 IX=1,NSP

C If IGET=25 and not outside then need to exclude some selections.
                  icc=IZSTOCN(IZONE,ISP(IX))
                  if(IGETNO(NGET,1).eq.25.and.IGETNO(NGET,4).gt.10
     &              .and.ICT(icc).ne.0)then
                    call usrmsg(
     &                'Surface does not face ambient condition so an',
     &                'other-face flux path cannot be reported.','W')
                    continue
                  else
                    NSEL=NSEL+1
                    IZNSEL(NSEL)=IZONE
                    ISFSEL(NSEL)=ISP(IX)
                  endif
 610            continue
              endif
 600        continue
          else
            call PICKANC('SURF',IZNSEL,ISFSEL,NSEL,IER)
          endif
        elseif (SLIST.and.NLIST) then

C Need to ask for specific surface and node.
          NSEL=NSEL+1
          call ASKZON(IZONE)
          call ASKSUR(IZONE,ISFN)
          call ASKICN(IZONE,ISFN,INODE)
          IZNSEL(NSEL)=IZONE
          ISFSEL(NSEL)=ISFN
        elseif (MRTLIST) then

C Need to ask for MRT sensors.
          call PIKMRTS(IZNSEL,ISFSEL,NSEL,IER)
          call usrmsg('  ','  ','-')
        else

C Copy current zone selection.
          do 620 I=1,NZ
            NSEL=NSEL+1
            IZNSEL(NSEL)=NZNO(I)
 620      continue
        endif

C Copy new data into IGETNO array if a selection has been made.
        if (NSEL.gt.0.AND.IER.eq.0) then
          do 650 IG=1,NSEL
            if((NGET+1).EQ.MZS)then
              call usrmsg('Maximum number of selected items exceeded',
     &          '(list will be truncated). ','P')
            else
              IZ=IZNSEL(IG)
              if (IG.eq.1) then
                IGETNO(NGET,2)=IZ
                if (SLIST.and.(.NOT.NLIST)) then
                  IGETNO(NGET,3)=ISFSEL(IG)
                elseif (MRTLIST) then
                  IGETNO(NGET,3)=ISFSEL(IG)
                endif
                if (SLIST.and.NLIST)then
                  IGETNO(NGET,3)=ISFN
                  IGETNO(NGET,4)=INODE
                endif
                if (MENUTYPE.eq.3.or.MENUTYPE.eq.4) then
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    IZLEN=lnblnk(ZSDESS)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.20) IZLEN=20-ISLEN
                    write (GLABEL(NGET),'(a,a)')
     &                                  ZSDESS(1:IZLEN),SUFFIX(1:ISLEN)
                  elseif (MRTLIST) then
                    IZLEN=lnzname(IZ)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.16) IZLEN=16-ISLEN
                    write (GLABEL(NGET),'(a,a,i2,a)')ZNAME(IZ)(1:IZLEN),
     &                                  ' S',ISFSEL(IG),SUFFIX(1:ISLEN)
                  else
                    write(GLABEL(NGET),'(2a)')ZNAME(IZ)(1:lnzname(IZ)),
     &                SUFFIX
                  endif
                else
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    IZLEN=lnblnk(ZSDESC)
                    write (GLABEL(NGET),'(a)') ZSDESC(1:IZLEN)
                  elseif (MRTLIST) then
                    IZLEN=lnzname(IZ)
                    write(GLABEL(NGET),'(a,a,i2)')ZNAME(IZ)(1:IZLEN),
     &                ' S',ISFSEL(IG)
                  else
                    write(GLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                  endif
                endif
              else

C Copy data for all selected zones/ surfaces/ sensors/ etc and fill in 
C specific information.
                NGET=NGET+1
                IGETNO(NGET,1)=IGETNO((NGET-1),1)
                IGETNO(NGET,2)=IZ
                if (SLIST.and.(.NOT.NLIST)) then
                  IGETNO(NGET,3)=ISFSEL(IG)
                elseif (MRTLIST) then
                  IGETNO(NGET,3)=ISFSEL(IG)
                else
                  IGETNO(NGET,3)=IGETNO((NGET-1),3)
                endif
                if (SLIST.and.NLIST) then
                  IGETNO(NGET,3)=ISFSEL(IG)
                  IGETNO(NGET,4)=INODE
                else
                  IGETNO(NGET,4)=IGETNO((NGET-1),4)
                endif
                IGETNO(NGET,5)=IGETNO((NGET-1),5)
                IGETNO(NGET,6)=IGETNO((NGET-1),6)
                IGETNO(NGET,7)=IGETNO((NGET-1),7)
                IGETNO(NGET,8)=IGETNO((NGET-1),8)
                IGETNO(NGET,9)=IGETNO((NGET-1),9)
                SLABEL(NGET)=SLABEL(NGET-1)
                if (MENUTYPE.eq.3.or.MENUTYPE.eq.4) then
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    IZLEN=lnblnk(ZSDESS)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.20) IZLEN=20-ISLEN
                    write (GLABEL(NGET),'(a,a)')
     &                                  ZSDESS(1:IZLEN),SUFFIX(1:ISLEN)
                  elseif (MRTLIST) then
                    IZLEN=lnzname(IZ)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.16) IZLEN=16-ISLEN
                    write (GLABEL(NGET),'(a,a,i2,a)')ZNAME(IZ)(1:IZLEN),
     &                                  ' S',ISFSEL(IG),SUFFIX(1:ISLEN)
                  else
                    write (GLABEL(NGET),'(a,a)')
     &                ZNAME(IZ)(1:lnzname(IZ)),SUFFIX
                  endif
                else
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    IZLEN=lnblnk(ZSDESC)
                    write (GLABEL(NGET),'(a)') ZSDESC(1:IZLEN)
                  elseif (MRTLIST) then
                    IZLEN=lnzname(IZ)
                    write (GLABEL(NGET),'(a,a,i2)')ZNAME(IZ)(1:IZLEN),
     &                                                  ' S',ISFSEL(IG)
                  else
                    write(GLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                  endif
                endif
              endif
            endif
 650      continue
        else

C No data to copy therefore cancel request.
          NGET=NGET-1
        endif
      endif

C Reset logical variables.
      SLIST=.false.
      NLIST=.false.
      MRTLIST=.false.

C Check for duplicate information.
 901  if (NGET.gt.1) then 
        do 900 I=1,(NGET-1)
          do 910 J=I+1,NGET
            if (IGETNO(J,1).eq.IGETNO(I,1).and.
     &          IGETNO(J,2).eq.IGETNO(I,2).and.
     &          IGETNO(J,3).eq.IGETNO(I,3).and.
     &          IGETNO(J,4).eq.IGETNO(I,4).and.
     &          IGETNO(J,5).eq.IGETNO(I,5).and.
     &          IGETNO(J,6).eq.IGETNO(I,6).and.
     &          IGETNO(J,7).eq.IGETNO(I,7).and.
     &          IGETNO(J,8).eq.IGETNO(I,8)) then

C Debug.
C              write(6,*) ' found match, NGET = ',I,J

              do 920 K=J,(NGET-1)
                IGETNO(K,1)=IGETNO(K+1,1)
                IGETNO(K,2)=IGETNO(K+1,2)
                IGETNO(K,3)=IGETNO(K+1,3)
                IGETNO(K,4)=IGETNO(K+1,4)
                IGETNO(K,5)=IGETNO(K+1,5)
                IGETNO(K,6)=IGETNO(K+1,6)
                IGETNO(K,7)=IGETNO(K+1,7)
                IGETNO(K,8)=IGETNO(K+1,8)
                IGETNO(K,9)=IGETNO(K+1,9)
                SLABEL(K)=SLABEL(K+1)
                GLABEL(K)=GLABEL(K+1)
 920          continue
              IGETNO(NGET,1)=0; IGETNO(NGET,2)=0
              IGETNO(NGET,3)=0; IGETNO(NGET,4)=0
              IGETNO(NGET,5)=ISIM
              IGETNO(NGET,6)=1; IGETNO(NGET,7)=0
              IGETNO(NGET,8)=0; IGETNO(NGET,9)=0
              SLABEL(NGET)='  '
              GLABEL(NGET)='  '
              NGET=NGET-1
              goto 901
            endif
 910      continue
 900    continue
      endif

C If energy demand or energy balance return.
      if (MENUTYPE.ge.9.and.MENUTYPE.le.14) return

C Check that there are still picks left and not adding a line.
      if (NGET.lt.MNGET.and.ADDLIN.eq.0) goto 1
      write(outs,'(a,i2,a)') 'All available data columns (',MNGET,
     &  ') are now filled.'
      if(MNGET.gt.3)call edisp(iuout,outs)
C *** user selections have now finished ***

C Debug.
C        do 9999 I=1,NGET
C          write(6,*) (IGETNO(I,K),K=1,9)
C          write(6,*) SLABEL(I)
C          write(6,*) GLABEL(I)
C          write(6,*) '  '
C 9999   continue

C Write GOGET info to tmp file.
      if(pifltog(1:3).eq.'ON ')call WRITEPIF(MENUTYPE)

      RETURN
      END

C ************************ GOGET ************************
C GOGET: Retrieves data from the result library according to array IGETNO.
C All NGET metrics are recovered by repeatedly calling GOGETIT.

      SUBROUTINE GOGET(IDAY)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      dimension XVAL(MZS),XVALOLD(MZS)

C Clear GET arrays of old data:
      do 10 I=1,MTS
        do 20 J=1,MZS
          VAL1(J,I)=0.0
          VAL2(J,I)=0.0
          if(J.le.MZRL)VAL3(J,I)=0.0
 20     continue
        XDUM(I)=0.0
        XDUM1(I)=0.0
        GVAL(I)=0.0
 10   continue
      do 30 I=1,MZS
        XVALOLD(I)=0.
 30   continue

C Recover a days data.
      ISTART=1
      IEND=24*NTS

C All recovered data is placed in array GVAL initially (COMMON/GET2/)
C ** For some unknown reason the climate data is also placed in VAL3 ! **
C Loop through the chosen metrics.
      do 200 KGET=1,NGET
        call GOGETIT(KGET,IDAY,ISTART,IEND)
 200  continue

C Average and copy the contents of VAL1 into VAL2.
C If IROC=1 the calc rate of change (xx per hour).
C If IROC=-1 then integrate data (xx hours).
      do 600 IX=ISTART,IEND,NOUT
        call VALIAV(NGET,IX,XVAL)
        do 610 KX=1,NGET
          VAL2(KX,IX)=XVAL(KX)
          if (IROC.eq.1) then
            if (IX.eq.1) then

C Should realy use previous value (if available) - assume no rate of change.
              XV=0.0
            else

C debug.        write(6,*) 'KX:',KX,' IX:',IX,' VAL2(KX,IX):',
C     &               VAL2(KX,IX),' XVALOLD(KX):',XVALOLD(KX)

              XV=(VAL2(KX,IX)-XVALOLD(KX))*float(NTS)/float(NOUT)
            endif
            XVALOLD(KX)=VAL2(KX,IX)
            VAL2(KX,IX)=XV
          elseif (IROC.eq.-1) then
            VAL2(KX,IX)=XVAL(KX)*float(NOUT)/float(NTS)
          endif
 610    continue
 600  continue

C Next day...
      RETURN
      END

C ************************ GOGETIT ************************
C GOGETIT Retrieves data from the result library according to array IGETNO and KGET.
C This recovers one day.

      SUBROUTINE GOGETIT(KGET,IDAY,ISTART,IEND)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever

      COMMON/RECV5/SNA(MCOM,MS)
      common/PREC18/ZTRANA(MCOM)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
           
C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types
      common/p3ver/ip3ver(MCOM)
      
      character outs*124,CSTR*25
      dimension CQ(MTS),GC(MTS)

      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

      if(IGETNO(KGET,1).eq.1)then

C IGETNO(KGET,1) =  1 Zone temperature
        CALL GZAIRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.2)then

C IGETNO(KGET,1) =  2 Ambient temperature
        CALL MOCLIM (IDAY,2)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.3)then

C IGETNO(KGET,1) =  3 Control point temperature
        CALL GTMCON(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.4)then

C IGETNO(KGET,1) =  4 Internal surface temperature
        CALL GTMS(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.5)then

C IGETNO(KGET,1) =  5 Outside surface temperature (relative to zone)
        CALL GTMSO(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.6)then

C IGETNO(KGET,1) =  6 Resultant temperature
        CALL CZRESL(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(XDUM1,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.7)then

C IGETNO(KGET,1) =  7 Node temperature (MLC)
        CALL GTMC(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,4),
     &                                               IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.8)then

C IGETNO(KGET,1) =  8 Sensible heating load and convert to KW.
        CALL GZQMH(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=24*NTS
        do 30 I=1,N
          GVAL(I)=GVAL(I)/1000.
 30     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.9)then

C IGETNO(KGET,1) =  9 Sensible cooling load and convert to KW.
        CALL GZQMC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=24*NTS
        do 31 I=1,N
          GVAL(I)=GVAL(I)/1000.
 31     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.10)then

C IGETNO(KGET,1) = 10 Total plant flux and convert to KW.
        CALL GZQM(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=24*NTS
        do 32 I=1,N
          GVAL(I)=GVAL(I)/1000.
 32     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.11)then

C IGETNO(KGET,1) = 11 Infiltration
        CALL GQV1(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.12)then

C IGETNO(KGET,1) = 12 Ventilation
        CALL GQV2(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.13)then

C IGETNO(KGET,1) = 13 Zone RH (%)
        CALL GZRH(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.14)then

C IGETNO(KGET,1) = 14 Zone dew point.
C Get zone air temperatureand RH, use xdum and xdum1 for processing.
        CALL GZDP(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.15)then

C IGETNO(KGET,1) = 15 Casual gains. Total, convective, radiant or by
C type.
        iicrcg=IGETNO(KGET,3)
        if(izver.eq.4)then

C If izver >= 4 the read the data directly.
          IZ=IGETNO(KGET,2)
          ISET=IGETNO(KGET,5)
          DO  202 J2=ISTART,IEND
            call getallcas(IDAY,IZ,ISET,J2,QCASR,QCASC,QCASL,FRAC,
     &        perocupc,perlightc,perequipc,otherc,perocupr,
     &        perlightr,perequipr,otherr,perocupl,perlightl,
     &        perequipl,otherl,theonectld)
            if(IICRCG.eq.1)then
              GC(J2)=QCASR+QCASC  ! Total casual gain (W)
            elseif(IICRCG.eq.2)then
              GC(J2)=QCASC        ! Total casual gain convective (W)
            elseif(IICRCG.eq.3)then
              GC(J2)=QCASR        ! Total casual gain radiant (W)
            elseif(IICRCG.eq.4)then
              GC(J2)=perocupc+perocupr ! Total occupant gain (W)
            elseif(IICRCG.eq.5)then
              GC(J2)=perlightc+perlightr ! Total lighting gain (W)
            elseif(IICRCG.eq.6)then
              GC(J2)=perequipc+perequipr ! Total small power gain (W)
            elseif(IICRCG.eq.7)then
              if(theonectld.eq.0)then ! Total controled casual gain (W)
                GC(J2)=0.0
              elseif(theonectld.eq.1)then
                GC(J2)=perocupc+perocupr
              elseif(theonectld.eq.2)then
                GC(J2)=perlightc+perlightr
              elseif(theonectld.eq.3)then
                GC(J2)=perequipc+perequipr
              endif
            elseif(IICRCG.eq.8)then
              GC(J2)=FRAC*100.     ! Frac of ctld casual gain (%)
            elseif(IICRCG.eq.9)then
              GC(J2)=perocupl+perlightl+perequipl  ! Total casual gain latent (W)
            elseif(IICRCG.eq.10)then
              GC(J2)=perocupc ! Occupant convec gain (W)
            elseif(IICRCG.eq.11)then
              GC(J2)=perocupr ! Occupant radiant gain (W)
            elseif(IICRCG.eq.12)then
              GC(J2)=perocupl ! Occupant latent gain (W)
            elseif(IICRCG.eq.13)then
              GC(J2)=perlightc ! Lighting convec gain (W)
            elseif(IICRCG.eq.14)then
              GC(J2)=perlightr ! Lighting radiant gain (W)
            elseif(IICRCG.eq.15)then
              GC(J2)=perlightl ! Lighting latent gain (W)
            elseif(IICRCG.eq.16)then
              GC(J2)=perequipc    ! Small pwr convec gain (W)
            elseif(IICRCG.eq.17)then
              GC(J2)=perequipr    ! Small pwr radiant gain (W)
            elseif(IICRCG.eq.18)then
              GC(J2)=perequipl    ! Small pwr latent gain (W)
            elseif(IICRCG.eq.19)then
              if(theonectld.eq.0)then ! Controlled convec gain (W)
                GC(J2)=0.0
              elseif(theonectld.eq.1)then
                GC(J2)=perocupc
              elseif(theonectld.eq.2)then
                GC(J2)=perlightc
              elseif(theonectld.eq.3)then
                GC(J2)=perequipc
              endif
            elseif(IICRCG.eq.20)then
              if(theonectld.eq.0)then ! Controlled radiant gain (W) 
                GC(J2)=0.0
              elseif(theonectld.eq.1)then
                GC(J2)=perocupr
              elseif(theonectld.eq.2)then
                GC(J2)=perlightr
              elseif(theonectld.eq.3)then
                GC(J2)=perequipr
              endif
            elseif(IICRCG.eq.21)then
              if(theonectld.eq.0)then ! Controlled latent gain (W)
                GC(J2)=0.0
              elseif(theonectld.eq.1)then
                GC(J2)=perocupl
              elseif(theonectld.eq.2)then
                GC(J2)=perlightl
              elseif(theonectld.eq.3)then
                GC(J2)=perequipl
              endif
            endif
  202     CONTINUE
          CALL XTVAL1(GC,ISTART,IEND,KGET)
        else
          if(IICRCG.eq.1)then

C Combined radiant and convective. If the
C operation file is old, sort the periods via processoldcas.
            DO  204 J2=ISTART,IEND
              GC(J2)=0.0
  204       CONTINUE
            IUNIT=IFIL+1
            CALL EROPER(0,iuout,iunit,IGETNO(KGET,2),IER) ! for older results files.
            if(ip3ver(IGETNO(KGET,2)).eq.0)then
              write(outs,'(a,i2,a)') 
     &          'Operation file for zone ',IGETNO(KGET,2),
     &          ' periods being sorted!'
              call edisp(iuout,outs)
              call PROCESSOLDCAS(IGETNO(KGET,2),0,iuout,IER)
            endif
            IF(ICGC(IGETNO(KGET,2)).EQ.1)then
              CALL ercgcf(0,iuout,LCGCIN(IGETNO(KGET,2)),
     &            IGETNO(KGET,2),ier)
            endif
            CALL ZCASC(IGETNO(KGET,2),IDAY)
            DO 51, JC2=ISTART,IEND
              GC(JC2)=VAL3(15,JC2)+VAL3(16,JC2)+VAL3(17,JC2)+
     &                VAL3(18,JC2)+VAL3(19,JC2)+VAL3(20,JC2)
  51        continue
            CALL CASRT(IGETNO(KGET,2),IDAY)
            DO 54, JC2=ISTART,IEND
              GC(JC2)=GC(JC2)+VAL3(15,JC2)+VAL3(16,JC2)+VAL3(17,JC2)+
     &                VAL3(18,JC2)+VAL3(19,JC2)+VAL3(20,JC2)
  54        continue
            CALL XTVAL1(GC,ISTART,IEND,KGET)
          elseif(IICRCG.eq.2)then
            CALL GCASC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5)) ! for older versions.
            CALL XTVAL1(GVAL,ISTART,IEND,KGET)
          elseif(IICRCG.eq.3)then
            CALL GCASR(IDAY,IGETNO(KGET,2),IGETNO(KGET,5)) ! for older versions.
            CALL XTVAL1(GVAL,ISTART,IEND,KGET)
          elseif(IICRCG.gt.3.and.IICRCG.le.7)then

C Casual by type. Use GC to hold sum at each timestep. First read the
C zone operation file and possible casual gain control file. If the
C operation file is old, sort the periods.
            DO  203 J2=ISTART,IEND
              GC(J2)=0.0
  203       CONTINUE
            IUNIT=IFIL+1
            CALL EROPER(0,iuout,iunit,IGETNO(KGET,2),IER) ! for older results files.
            if(ip3ver(IGETNO(KGET,2)).eq.0)then
              write(outs,'(a,i2,a)') 
     &          'Operation file for zone ',IGETNO(KGET,2),
     &          ' periods being sorted!'
              call edisp(iuout,outs)
              call PROCESSOLDCAS(IGETNO(KGET,2),0,iuout,IER)
            endif
            IF(ICGC(IGETNO(KGET,2)).EQ.1)then
              CALL ercgcf(0,iuout,LCGCIN(IGETNO(KGET,2)),
     &          IGETNO(KGET,2),ier)
            endif
            CALL ZCASC(IGETNO(KGET,2),IDAY)
            DO 50, JC2=ISTART,IEND
              if(iicrcg.eq.4)then
                GC(JC2)=VAL3(15,JC2)
              elseif(iicrcg.eq.5)then
                GC(JC2)=VAL3(16,JC2)
              elseif(iicrcg.eq.6)then
                GC(JC2)=VAL3(17,JC2)
              elseif(iicrcg.eq.7)then
                GC(JC2)=VAL3(18,JC2)+VAL3(19,JC2)+VAL3(20,JC2)
              endif
   50       CONTINUE

C Use XDUM1 for radiant portion, sum depending on gain type.
            CALL CASRT(IGETNO(KGET,2),IDAY)
            DO 52, JC2=ISTART,IEND
              if(iicrcg.eq.4)then
                GC(JC2)=GC(JC2)+VAL3(15,JC2)
              elseif(iicrcg.eq.5)then
                GC(JC2)=GC(JC2)+VAL3(16,JC2)
              elseif(iicrcg.eq.6)then
                GC(JC2)=GC(JC2)+VAL3(17,JC2)
              elseif(iicrcg.eq.7)then
                GC(JC2)=GC(JC2)+VAL3(18,JC2)+VAL3(19,JC2)+VAL3(20,JC2)
              endif
   52       CONTINUE
            CALL XTVAL1(GC,ISTART,IEND,KGET)

C Casual gain fraction (controlled).
          elseif(IICRCG.eq.8)then
            CALL GCFRAC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
            CALL XTVAL1(GVAL,ISTART,IEND,KGET)
          endif
        endif
      elseif(IGETNO(KGET,1).eq.16)then

C IGETNO(KGET,1) = 16 Surface convection.
        if(IGETNO(KGET,4).eq.1)then

C Internal surface convection.
C Need to add set information to this call.
          CALL CSCONV(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),CQ)
          CALL XTVAL1(CQ,ISTART,IEND,KGET)
        elseif(IGETNO(KGET,4).eq.2)then

C External surface convection.
          if(ISAVE.eq.4)then
            IG=9
            CALL G4FLUX(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IG,CQ)
          else
            call usrmsg('Current save level does not support the ',
     &       'recovery of convection at surfaces. Use save level 4.',
     &       '-')
            do 900 JTS=ISTART,IEND
              CQ(JTS)=0.0
  900       continue
          endif
          CALL XTVAL1(CQ,ISTART,IEND,KGET)
        elseif(IGETNO(KGET,4).eq.3)then

C Total inside surface convection.
          call GSURIC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          CALL XTVAL1(XDUM,ISTART,IEND,KGET)
        endif
      elseif(IGETNO(KGET,1).eq.17)then

C IGETNO(KGET,1) = 17 Solar absorbed on external surfaces
        CALL GQSE(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.18)then

C IGETNO(KGET,1) = 18 Solar absorbed on internal surfaces
        CALL GQSI(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.19)then

C IGETNO(KGET,1) = 19 Direct solar (climate parameter)
        CALL MOCLIM (IDAY,24)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.20)then

C IGETNO(KGET,1) = 20 Diffuse solar (climate parameter)
        CALL MOCLIM (IDAY,25)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.21)then

C IGETNO(KGET,1) = 21 Wind speed (climate parameter)
        CALL MOCLIM (IDAY,26)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.22)then

C IGETNO(KGET,1) = 22 Ambient RH (climate parameter)
        CALL MOCLIM (IDAY,28)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.23)then

C IGETNO(KGET,1) = 23 Comfort (IGETNO(KGET,3)= 1,2 or 3 - PMV/PPD..)
        if(IGETNO(KGET,3).eq.1)then
          CALL MOCMFT(IDAY,IGETNO(KGET,2),'M')
        elseif(IGETNO(KGET,3).eq.2)then
          CALL MOCMFT(IDAY,IGETNO(KGET,2),'E')
        elseif(IGETNO(KGET,3).eq.3)then
          CALL MOCMFT(IDAY,IGETNO(KGET,2),'D')
        endif
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.24)then

C IGETNO(KGET,1) = 24 Mean radiant temperature
C Get area weighted MRT or if sensors defined (KGET,3) > 0 then 
C recover data for sensor.
        if(IGETNO(KGET,3).eq.0)then
          CALL CZMRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          CALL XTVAL1(XDUM1,ISTART,IEND,KGET)
        else
        CALL SENMRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5),0)
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        endif
      elseif(IGETNO(KGET,1).eq.25)then

C IGET = 25 Surface flux (various IGET,4) where 1=conduction (inside),
C   2=convec (inside), 3=LW radiation (inside), 4=SW rad absorb inside,
C   5=radiant casual occup, 6=radiant casual light
C   7=radiant casual equip, 8=contrld casual gains, 9=heat storage (inside)
C   10=plant inj/extr (ins), 11=conduction (other fc), 12=convec (other),
C   13=long wave > buildings, 14=long wave > sky, 15=long wave > ground,
C   16= SW rad (other), 17=heat storage (other fc)
        iz=IGETNO(KGET,2)
        is=IGETNO(KGET,3)
        IPK=IGETNO(KGET,4)

C Check if reporting per m^2.  If so set surface area, if not set surface 
C area to 1.
        if (IPK.lt.0) then
          IPK=-IPK
          SAREA=SNA(IZ,IS)
        else
          SAREA=1.
        endif
          
C Read in the zone geometry, construction data, zone operational data,
C and casual gain control file. << Work towards not reading zone files >>
        IUNIT=IFIL+1
        call georead(IUNIT,LGEOM(IZ),IZ,0,IUOUT,IER)

        CALL ECONST(LTHRM(IZ),IUNIT,IZ,0,IUOUT,IER)

C If izver is 4 or greater then assume direct read of data, otherwise
C read the zone operation file.
        if(izver.eq.4)then
          continue
        else
          CALL EROPER(0,iuout,iunit,iz,IER) ! for older results files.
          if(ip3ver(iz).eq.0)then
            write(outs,'(a,i2,a)') 
     &        'Operation file for zone ',IGETNO(KGET,2),
     &        ' periods being sorted!'
            call edisp(iuout,outs)
            call PROCESSOLDCAS(IGETNO(KGET,2),0,iuout,IER)
          endif
          IF(ICGC(IZ).EQ.1)CALL ercgcf(0,iuout,LCGCIN(iz),IZ,ier)
        endif

C Set default surface absorbtivity.
        absorb=1.0
        absorb=ABSE(IS)

C Radiant plant is area AND emissivity weighted.
        emarea=0.0
        do 80 kk=1,NZSUR(iz)
          emarea=emarea+sna(iz,kk)*EMISI(kk)
  80    continue
        if(IPK.GE.5.AND.IPK.LE.8)then

C Get inside casual radiant for this surface and store
C requested uncontrolled and controlled types 1,2,3
C from temporary storage in VAL3(15,?) to VAL3(20,?).

C << is there a way to use data from GSURFB for this ?? >>

          CALL ZCASR(IZ,IS,IDAY)
          DO 199 JTS=ISTART,IEND
            IF(IPK.EQ.5)GVAL(JTS)=VAL3(15,JTS)
            IF(IPK.EQ.6)GVAL(JTS)=VAL3(16,JTS)
            IF(IPK.EQ.7)GVAL(JTS)=VAL3(17,JTS)
            IF(IPK.EQ.8)GVAL(JTS)=VAL3(18,JTS)+VAL3(19,JTS)+VAL3(20,JTS)
            GVAL(JTS)=GVAL(JTS)/SAREA
  199     CONTINUE
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        elseif(IPK.EQ.10)then

C Get the plant injection at each time step, return in GVAL.
          CALL GZQM(IDAY,IZ,IGETNO(KGET,5))
          DO 201 JTS=ISTART,IEND
            GVAL(JTS)=GVAL(JTS)/SAREA
  201     CONTINUE

C Determine the radiant portion of the plant via checking IBAN.
C Radiant only to one surface node, mixed plant injection, or
C no radiant plant component.
          ICF=ICASCF(IZ)
          IF(ICF.GT.0)THEN
            IF(IBAN(ICF,1).EQ.IZ.AND.IBAN(ICF,2).EQ.IS.AND.
     &                                  IBAN(ICF,3).EQ.0)then
              CALL XTVAL1(GVAL,ISTART,IEND,KGET)
            ELSEIF(IBAN(ICF,1).EQ.-2.AND.IBAN(ICF,2).EQ.IZ)then
              xfac=sna(IZ,IS)*EMISI(IS)/emarea
              DO 136 I2=ISTART,IEND
                 XDUM(I2)=(xfac)*GVAL(I2)
     &                    *(1.-(FLOAT(IBAN(ICF,3))*0.01))
  136         CONTINUE
              CALL XTVAL1(XDUM,ISTART,IEND,KGET)
            ELSE
              DO 37 I2=ISTART,IEND
                XDUM(I2)=0.
   37         CONTINUE
              CALL XTVAL1(XDUM,ISTART,IEND,KGET)
            ENDIF
          ELSE
            DO 137 I2=ISTART,IEND
              XDUM(I2)=0.
  137       CONTINUE
            CALL XTVAL1(XDUM,ISTART,IEND,KGET)
          ENDIF
        else
          CALL SURADJ(IZ,IS,IE,T,IZC,ISC,ICN,CSTR)
          DO 198 JTS=ISTART,IEND
            CALL GSURFB(IDAY,IZ,IS,JTS,TSout,qswrdi,
     &         qlwrdi,qconvi,qcondi,qstori,qstore,qconde,qconve,
     &         qlwrde,qlwrbd,qlwrsk,qswrde,avqcrioc,avqcrili,
     &         avqcrisp,avqcrioth)
            if(IPK.eq.1)then
              GVAL(JTS)=qcondi/SAREA
            elseif(IPK.eq.2)then
              GVAL(JTS)=qconvi/SAREA
            elseif(IPK.eq.3)then
              GVAL(JTS)=qlwrdi/SAREA
            elseif(IPK.eq.4)then
              GVAL(JTS)=qswrdi/SAREA
            elseif(IPK.eq.9)then
              GVAL(JTS)=qstori/SAREA
            elseif(IPK.eq.11)then
              GVAL(JTS)=qconde/SAREA
            elseif(IPK.eq.12)then
              GVAL(JTS)=qconve/SAREA
            elseif(IPK.eq.13)then
              GVAL(JTS)=qlwrbd/SAREA
            elseif(IPK.eq.14)then
              GVAL(JTS)=qlwrsk/SAREA
            elseif(IPK.eq.15)then
              GVAL(JTS)=qlwrde/SAREA
            elseif(IPK.eq.16)then
              GVAL(JTS)=qswrde/SAREA
            elseif(IPK.eq.17)then

C Determine if the surface is transparent or opaque.
              if(SSOTF(ICN)(1:4).EQ.'OPAQ'.OR.
     &           SSOTF(ICN)(1:4).EQ.'UNKN')then
                GVAL(JTS)=qswrde/(SAREA*absorb)
              else
                call edisp(iuout,
     & ' Surface solar abs is only available for opaque surfaces.')
                GVAL(JTS)=0.0
              endif
            elseif(IPK.eq.18)then
              GVAL(JTS)=qstore/SAREA
            else
            endif
  198     continue
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        endif
      elseif(IGETNO(KGET,1).eq.26)then

C IGETNO(KGET,1) = 26 External lux level
        call gillum(IDAY)
        CALL XTVAL1(XDUM,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.27)then

C IGETNO(KGET,1) = 27 Real electrical power consumption.
        CALL GPCR1(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(XDUM,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.28)then

C IGETNO(KGET,1) = 28 Reactive electrical power consumption.
        CALL GPCQ2(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(XDUM,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.29)then

C IGETNO(KGET,1) = 29 Delta T zone - ambient.
        CALL MOCLIM (IDAY,2)
        CALL GZAIRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1203 J2=ISTART,IEND
          GC(J2)=GVAL(J2)-VAL3(2,J2)
 1203   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.30)then

C IGETNO(KGET,1) = 30 Latent load (dehumidification) and convert to KW.
        CALL GZLLC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=24*NTS
        do 33 I=1,N
          GVAL(I)=GVAL(I)/1000.
 33     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.31)then

C IGETNO(KGET,1) = 31 Latent load (humidification) and convert to KW.
        CALL GZLLH(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=34*NTS
        do 34 I=1,N
          GVAL(I)=GVAL(I)/1000.
 34     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.32)then

C IGETNO(KGET,1) = 32 All latent load and convert to KW.
        CALL GZLL(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=35*NTS
        do 35 I=1,N
          GVAL(I)=GVAL(I)/1000.
 35     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.33)then

C IGETNO(KGET,1) = 33 Sensible plus latent load and convert to KW. 
        CALL GZQSL(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=36*NTS
        do 36 I=1,N
          GVAL(I)=GVAL(I)/1000.
 36     continue
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.34)then

C IGETNO(KGET,1) = 34 Wind direction (climate parameter)
        CALL MOCLIM (IDAY,27)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.35)then

C IGETNO(KGET,1) = 35 Delta T surf (use CQ) - dewpoint.
        CALL GTMS(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        DO  1204 J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
 1204   CONTINUE
        CALL GZDP(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1205 J2=ISTART,IEND
          GC(J2)=CQ(J2)-GVAL(J2)
 1205   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.36)then

C IGET = 36 Total conv @ interior/similar/adiab/constant surfaces.
C Add opaque and transparent at each timestep.
        CALL GOPQIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1206 J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
 1206   CONTINUE
        CALL GTRNIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1207 J2=ISTART,IEND
          CQ(J2)=CQ(J2)+GVAL(J2)
 1207   CONTINUE
        CALL XTVAL1(CQ,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.37)then

C IGET = 37 Total conv @ outside connected surfaces. Add opaque and
C transparent at each timestep.
        CALL GOUTOPQ(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1208 J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
 1208   CONTINUE
        CALL GOUTTRN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1209 J2=ISTART,IEND
          CQ(J2)=CQ(J2)+GVAL(J2)
 1209   CONTINUE
        CALL XTVAL1(CQ,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.38)then

C IGET = 38 Solar entering zone from outside (W)
        iz=IGETNO(KGET,2)
        IPK=IGETNO(KGET,4)
        
C Check if reporting per m^2.  If so set surface area to total transparent 
C area (=1) or floor area (=2) connected to external type BC, if not set surface area 
C to 1.
        if (IPK.eq.2) then
        
C Floor area.
          SAREA=ZBASEA(IZ)
        elseif (IPK.eq.1) then
        
C Transparent exterior area.
          SAREA=ZTRANA(IZ)
        else
          SAREA=1.
        endif
        call GSOLX(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO 1250 JTS=ISTART,IEND
          GVAL(JTS)=GVAL(JTS)/SAREA
 1250   CONTINUE
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.39)then

C IGET = 39 Solar entering zone from adj zones (W)
        call GSOLADJ(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.40)then

C IGET = 40 Solar absorbed in zone (W)
        call GSOLABS(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.41)then

C IGET = 41 db T - another zone db T.
        CALL GZAIRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO  1304 J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
 1304   CONTINUE
        CALL GZAIRT(IDAY,IGETNO(KGET,3),IGETNO(KGET,5))
        DO  1305 J2=ISTART,IEND
          GC(J2)=CQ(J2)-GVAL(J2)
 1305   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.42)then

C IGET = 42 Zone balance opaque exterior conv.
        CALL GOUTOPQ(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.43)then

C IGET = 43 Zone balance opaque partitions conv.
        CALL GOPQIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.44)then

C IGET = 44 Zone balance transparent exterior conv.
        CALL GOUTTRN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.45)then

C IGET = 45 Zone balance transparent partitions conv.
        CALL GTRNIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.46)then

C IGET = 46 Renewables/advanced materials primary output.
        CALL GZSPM1(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.47)then

C IGET = 47 Renewables/advanced materials secondary output.
        CALL GZSPM2(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.50)then

C IGETNO(KGET,1) = 50 Local PPD: delta T head/ feet.
        call edisp(iuout,
     &    'Missing information on local air T, setting dT to zero.')
        DO  1406 J2=ISTART,IEND
          iflag=1
          dT=0.
          call DISCOMF(iflag,dT,PPD)
          if (iflag.eq.-1) PPD=101.
          GC(J2)=PPD
 1406   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.51)then

C IGETNO(KGET,1) = 51 Local PPD: floor T.
        CALL GTMS(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        DO  1404 J2=ISTART,IEND
          iflag=2
          call DISCOMF(iflag,GVAL(J2),PPD)

C Debug.
C          write(6,*) '41 ',IGETNO(KGET,2),GVAL(J2),PPD

          if (iflag.eq.-1) PPD=101.
          GC(J2)=PPD
 1404   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.52)then

C IGETNO(KGET,1) = 52 Local PD: ceiling T, assume dT between air and surface.
        CALL SENMRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),
     &                                  IGETNO(KGET,5),IGETNO(KGET,4))
        DO  1306 J2=ISTART,IEND
          if (GVAL(J2).lt.0.0) then
            iflag=5
          else
            iflag=3
          endif
          call DISCOMF(iflag,GVAL(J2),PD)

C Debug.
C          write(6,*) '42 ',IGETNO(KGET,2),GVAL(J2),PD

          if (iflag.eq.-1) PD=101.
          GC(J2)=PD
 1306   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.53)then

C IGETNO(KGET,1) = 53 Local PD: assymetric dTrad
        CALL SENMRT(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),
     &                                  IGETNO(KGET,5),IGETNO(KGET,4))
        DO  1308 J2=ISTART,IEND
          if (GVAL(J2).lt.0.0) then
            iflag=6
          else
            iflag=4
          endif
          call DISCOMF(iflag,GVAL(J2),PD)

C Debug.
C          write(6,*) '42 ',IGETNO(KGET,2),GVAL(J2),PD

          if (iflag.eq.-1) PD=101.
          GC(J2)=PD
 1308   CONTINUE
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.80)then

C IGETNO(KGET,1) = 80 Zone measured db temperature
        CALL MOMEASURED('DBZNM   ',IDAY,IGETNO(KGET,2),0)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.81)then

C IGETNO(KGET,1) = 81 Surface measured db temperature
        CALL MOMEASURED('SURTM   ',IDAY,IGETNO(KGET,2),IGETNO(KGET,3))
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      endif

      RETURN
      END


C ******************** MMACHK ********************
C MMACHK returns the maximum, minimum, and average value of the 
C requested metric.  The time of occurance of the maximum and minimum 
C values is also returned.
C In the case of integrated values (IROC=-1) DAVE is the total value.hours,
C e.g. for a flux (W) the energy for the given period (Wh).
C For IROC=0 or 1 the value DAVE is the average value for the period.
C Memory is referenced if the data is held there otherwise MMACAL is 
C called to calculate the values.

      subroutine MMACHK(KGET,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
#include "building.h"
#include "geometry.h"

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      common/MMADAT/RDMAX(MCOM,3,MZRL),RDMIN(MCOM,3,MZRL),
     &       RDAVE(MCOM,3,MZRL),RDTMAX(MCOM,3,MZRL),RDTMIN(MCOM,3,MZRL)
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

C If calculating a rate of change or integrating then always scan db.
      if (IROC.ne.0) then
        call MMACAL(KGET,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
        return
      endif

C Check to see if data held in memory if not then call calculation routine.
      if (IGETNO(KGET,1).eq.1) then

C IGETNO(KGET,1) =  1 Zone temperature
        IZ=IGETNO(KGET,2)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,1)
          DMIN=RDFMIN(IZ,1,1)
          DAVE=RDFAVE(IZ,1,1)
          TMAX=RDTFMAX(IZ,1,1)
          TMIN=RDTFMIN(IZ,1,1)
        else
          DMAX=RDMAX(IZ,1,1)
          DMIN=RDMIN(IZ,1,1)
          DAVE=RDAVE(IZ,1,1)
          TMAX=RDTMAX(IZ,1,1)
          TMIN=RDTMIN(IZ,1,1)
        endif
      elseif(IGETNO(KGET,1).eq.3)then

C IGETNO(KGET,1) =  3 Control point temperature
        IZ=IGETNO(KGET,2)
        IC = NZSUR(IGETNO(KGET,2))+5
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,IC)
          DMIN=RDFMIN(IZ,1,IC)
          DAVE=RDFAVE(IZ,1,IC)
          TMAX=RDTFMAX(IZ,1,IC)
          TMIN=RDTFMIN(IZ,1,IC)
        else
          DMAX=RDMAX(IZ,1,IC)
          DMIN=RDMIN(IZ,1,IC)
          DAVE=RDAVE(IZ,1,IC)
          TMAX=RDTMAX(IZ,1,IC)
          TMIN=RDTMIN(IZ,1,IC)
        endif
      elseif(IGETNO(KGET,1).eq.4)then

C IGETNO(KGET,1) =  4 Internal surface temperature
        IZ=IGETNO(KGET,2)
        IC = IGETNO(KGET,3)+2
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,IC)
          DMIN=RDFMIN(IZ,1,IC)
          DAVE=RDFAVE(IZ,1,IC)
          TMAX=RDTFMAX(IZ,1,IC)
          TMIN=RDTFMIN(IZ,1,IC)
        else
          DMAX=RDMAX(IZ,1,IC)
          DMIN=RDMIN(IZ,1,IC)
          DAVE=RDAVE(IZ,1,IC)
          TMAX=RDTMAX(IZ,1,IC)
          TMIN=RDTMIN(IZ,1,IC)
        endif
      elseif(IGETNO(KGET,1).eq.10)then

C IGETNO(KGET,1) = 10 Total plant flux
C Reporting of all plant fluxes in kW but data stored as W
        IZ=IGETNO(KGET,2)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,2)/1000.
          DMIN=RDFMIN(IZ,1,2)/1000.
          DAVE=RDFAVE(IZ,1,2)/1000.
          TMAX=RDTFMAX(IZ,1,2)
          TMIN=RDTFMIN(IZ,1,2)
        else
          DMAX=RDMAX(IZ,1,2)/1000.
          DMIN=RDMIN(IZ,1,2)/1000.
          DAVE=RDAVE(IZ,1,2)/1000.
          TMAX=RDTMAX(IZ,1,2)
          TMIN=RDTMIN(IZ,1,2)
        endif
      elseif(IGETNO(KGET,1).eq.11)then

C IGETNO(KGET,1) = 11 Infiltration
        IZ=IGETNO(KGET,2)
        IC = NZSUR(IGETNO(KGET,2))+3
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,IC)
          DMIN=RDFMIN(IZ,1,IC)
          DAVE=RDFAVE(IZ,1,IC)
          TMAX=RDTFMAX(IZ,1,IC)
          TMIN=RDTFMIN(IZ,1,IC)
        else
          DMAX=RDMAX(IZ,1,IC)
          DMIN=RDMIN(IZ,1,IC)
          DAVE=RDAVE(IZ,1,IC)
          TMAX=RDTMAX(IZ,1,IC)
          TMIN=RDTMIN(IZ,1,IC)
        endif
      elseif(IGETNO(KGET,1).eq.12)then

C IGETNO(KGET,1) = 12 Ventilation
        IZ=IGETNO(KGET,2)
        IC = NZSUR(IGETNO(KGET,2))+4
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,1,IC)
          DMIN=RDFMIN(IZ,1,IC)
          DAVE=RDFAVE(IZ,1,IC)
          TMAX=RDTFMAX(IZ,1,IC)
          TMIN=RDTFMIN(IZ,1,IC)
        else
          DMAX=RDMAX(IZ,1,IC)
          DMIN=RDMIN(IZ,1,IC)
          DAVE=RDAVE(IZ,1,IC)
          TMAX=RDTMAX(IZ,1,IC)
          TMIN=RDTMIN(IZ,1,IC)
        endif
      elseif(IGETNO(KGET,1).eq.13)then

C IGETNO(KGET,1) = 13 Zone RH (%)
        IZ=IGETNO(KGET,2)
        IC = NZSUR(IGETNO(KGET,2))+3
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,3,IC)
          DMIN=RDFMIN(IZ,3,IC)
          DAVE=RDFAVE(IZ,3,IC)
          TMAX=RDTFMAX(IZ,3,IC)
          TMIN=RDTFMIN(IZ,3,IC)
        else
          DMAX=RDMAX(IZ,3,IC)
          DMIN=RDMIN(IZ,3,IC)
          DAVE=RDAVE(IZ,3,IC)
          TMAX=RDTMAX(IZ,3,IC)
          TMIN=RDTMIN(IZ,3,IC)
        endif
      elseif(IGETNO(KGET,1).eq.15)then

C IGETNO(KGET,1) = 15 Casual gains. Total, convective, radiant or by
C type.
        iicrcg=IGETNO(KGET,3)
        IZ=IGETNO(KGET,2)
        if(iicrcg.eq.2)then       ! Total casual gain convective (W)
          IC = NZSUR(IGETNO(KGET,2))+2
          if (IOCUPF.eq.1) then
            DMAX=RDFMAX(IZ,3,IC)
            DMIN=RDFMIN(IZ,3,IC)
            DAVE=RDFAVE(IZ,3,IC)
            TMAX=RDTFMAX(IZ,3,IC)
            TMIN=RDTFMIN(IZ,3,IC)
          else
            DMAX=RDMAX(IZ,3,IC)
            DMIN=RDMIN(IZ,3,IC)
            DAVE=RDAVE(IZ,3,IC)
            TMAX=RDTMAX(IZ,3,IC)
            TMIN=RDTMIN(IZ,3,IC)
          endif
        elseif(iicrcg.eq.3)then       ! Total casual gain radiant (W)
          IC = NZSUR(IGETNO(KGET,2))+1
          if (IOCUPF.eq.1) then
            DMAX=RDFMAX(IZ,3,IC)
            DMIN=RDFMIN(IZ,3,IC)
            DAVE=RDFAVE(IZ,3,IC)
            TMAX=RDTFMAX(IZ,3,IC)
            TMIN=RDTFMIN(IZ,3,IC)
          else
            DMAX=RDMAX(IZ,3,IC)
            DMIN=RDMIN(IZ,3,IC)
            DAVE=RDAVE(IZ,3,IC)
            TMAX=RDTMAX(IZ,3,IC)
            TMIN=RDTMIN(IZ,3,IC)
          endif
        elseif(iicrcg.eq.8)then    ! Frac of ctld casual gain (%)
          IC = NZSUR(IGETNO(KGET,2))+5
          if (IOCUPF.eq.1) then
            DMAX=RDFMAX(IZ,3,IC)*100.
            DMIN=RDFMIN(IZ,3,IC)*100.
            DAVE=RDFAVE(IZ,3,IC)*100.
            TMAX=RDTFMAX(IZ,3,IC)
            TMIN=RDTFMIN(IZ,3,IC)
          else
            DMAX=RDMAX(IZ,3,IC)*100.
            DMIN=RDMIN(IZ,3,IC)*100.
            DAVE=RDAVE(IZ,3,IC)*100.
            TMAX=RDTMAX(IZ,3,IC)
            TMIN=RDTMIN(IZ,3,IC)
          endif
        else
          call MMACAL(KGET,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
        endif
      elseif(IGETNO(KGET,1).eq.17)then

C IGETNO(KGET,1) = 17 Solar absorbed on external surfaces
        IZ=IGETNO(KGET,2)
        IC = IGETNO(KGET,3)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,3,IC)
          DMIN=RDFMIN(IZ,3,IC)
          DAVE=RDFAVE(IZ,3,IC)
          TMAX=RDTFMAX(IZ,3,IC)
          TMIN=RDTFMIN(IZ,3,IC)
        else
          DMAX=RDMAX(IZ,3,IC)
          DMIN=RDMIN(IZ,3,IC)
          DAVE=RDAVE(IZ,3,IC)
          TMAX=RDTMAX(IZ,3,IC)
          TMIN=RDTMIN(IZ,3,IC)
        endif
      elseif(IGETNO(KGET,1).eq.18)then

C IGETNO(KGET,1) = 18 Solar absorbed on internal surfaces
        IZ=IGETNO(KGET,2)
        IC = IGETNO(KGET,3)+5
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,IC)
          DMIN=RDFMIN(IZ,2,IC)
          DAVE=RDFAVE(IZ,2,IC)
          TMAX=RDTFMAX(IZ,2,IC)
          TMIN=RDTFMIN(IZ,2,IC)
        else
          DMAX=RDMAX(IZ,2,IC)
          DMIN=RDMIN(IZ,2,IC)
          DAVE=RDAVE(IZ,2,IC)
          TMAX=RDTMAX(IZ,2,IC)
          TMIN=RDTMIN(IZ,2,IC)
        endif
      elseif(IGETNO(KGET,1).eq.27)then

C IGETNO(KGET,1) = 27 Real electrical power consumption.
        IZ=IGETNO(KGET,2)
        IC = 3
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,IC)
          DMIN=RDFMIN(IZ,2,IC)
          DAVE=RDFAVE(IZ,2,IC)
          TMAX=RDTFMAX(IZ,2,IC)
          TMIN=RDTFMIN(IZ,2,IC)
        else
          DMAX=RDMAX(IZ,2,IC)
          DMIN=RDMIN(IZ,2,IC)
          DAVE=RDAVE(IZ,2,IC)
          TMAX=RDTMAX(IZ,2,IC)
          TMIN=RDTMIN(IZ,2,IC)
        endif
      elseif(IGETNO(KGET,1).eq.28)then

C IGETNO(KGET,1) = 28 Reactive electrical power consumption.
        IZ=IGETNO(KGET,2)
        IC = 4
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,IC)
          DMIN=RDFMIN(IZ,2,IC)
          DAVE=RDFAVE(IZ,2,IC)
          TMAX=RDTFMAX(IZ,2,IC)
          TMIN=RDTFMIN(IZ,2,IC)
        else
          DMAX=RDMAX(IZ,2,IC)
          DMIN=RDMIN(IZ,2,IC)
          DAVE=RDAVE(IZ,2,IC)
          TMAX=RDTMAX(IZ,2,IC)
          TMIN=RDTMIN(IZ,2,IC)
        endif
      elseif(IGETNO(KGET,1).eq.32)then

C IGETNO(KGET,1) = 32 All latent load 
C Reporting of all plant fluxes in kW but data stored as W
        IZ=IGETNO(KGET,2)
        IC = NZSUR(IGETNO(KGET,2))+4
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,3,IC)/1000.
          DMIN=RDFMIN(IZ,3,IC)/1000.
          DAVE=RDFAVE(IZ,3,IC)/1000.
          TMAX=RDTFMAX(IZ,3,IC)
          TMIN=RDTFMIN(IZ,3,IC)
        else
          DMAX=RDMAX(IZ,3,IC)/1000.
          DMIN=RDMIN(IZ,3,IC)/1000.
          DAVE=RDAVE(IZ,3,IC)/1000.
          TMAX=RDTMAX(IZ,3,IC)
          TMIN=RDTMIN(IZ,3,IC)
        endif
      elseif(IGETNO(KGET,1).eq.38)then

C IGET = 38 Solar entering zone from outside (W)
        IZ=IGETNO(KGET,2)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,1)
          DMIN=RDFMIN(IZ,2,1)
          DAVE=RDFAVE(IZ,2,1)
          TMAX=RDTFMAX(IZ,2,1)
          TMIN=RDTFMIN(IZ,2,1)
        else
          DMAX=RDMAX(IZ,2,1)
          DMIN=RDMIN(IZ,2,1)
          DAVE=RDAVE(IZ,2,1)
          TMAX=RDTMAX(IZ,2,1)
          TMIN=RDTMIN(IZ,2,1)
        endif
      elseif(IGETNO(KGET,1).eq.39)then

C IGET = 39 Solar entering zone from adj zones (W)
        IZ=IGETNO(KGET,2)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,2)
          DMIN=RDFMIN(IZ,2,2)
          DAVE=RDFAVE(IZ,2,2)
          TMAX=RDTFMAX(IZ,2,2)
          TMIN=RDTFMIN(IZ,2,2)
        else
          DMAX=RDMAX(IZ,2,2)
          DMIN=RDMIN(IZ,2,2)
          DAVE=RDAVE(IZ,2,2)
          TMAX=RDTMAX(IZ,2,2)
          TMIN=RDTMIN(IZ,2,2)
        endif
      elseif(IGETNO(KGET,1).eq.40)then

C IGET = 40 Solar absorbed in zone (W)
        IZ=IGETNO(KGET,2)
        if (IOCUPF.eq.1) then
          DMAX=RDFMAX(IZ,2,5)
          DMIN=RDFMIN(IZ,2,5)
          DAVE=RDFAVE(IZ,2,5)
          TMAX=RDTFMAX(IZ,2,5)
          TMIN=RDTFMIN(IZ,2,5)
        else
          DMAX=RDMAX(IZ,2,5)
          DMIN=RDMIN(IZ,2,5)
          DAVE=RDAVE(IZ,2,5)
          TMAX=RDTMAX(IZ,2,5)
          TMIN=RDTMIN(IZ,2,5)
        endif

C If not included above must calculate.
      else
        call MMACAL(KGET,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
      endif
      return
      end

C ******************** MMACAL ********************
C MMACAL calculates the data not directly available in MMACHK.
C In the case of integrated values (IROC=-1) DTOT is the total value.hours,
C e.g. for a flux (W) the energy for the given period (Wh).
C For IROC=0 or 1 the value DAVE is the average value for the period.

      subroutine MMACAL(KGET,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      dimension XVAL(MZS),XVALOLD(MZS)

C Reset return values.
      DMAX=-1.E+10
      DMIN=1.E+10
      DAVE=0.
      DTOT=0.
      TMAX=0.
      TMIN=0.

C Initialise
      XVALOLD(KGET)=0.

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
C NDTS - the number of timesteps in a day.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)
      NDTS=24*NTS
      NITMS=0
      IZONE=IGETNO(KGET,2)
      do 10 IDAY=IODS,IODF
        call GOGETIT(KGET,IDAY,1,NDTS)
        DO 421 J = 1,NDTS,NOUT
          call VALIAV(KGET,J,XVAL)

C Check for invalid use of comfort algorithms.
          if (IGETNO(KGET,1).ge.50.AND.IGETNO(KGET,1).le.59) then
            if (XVAL(KGET).gt.100.0) goto 421
          endif

C Calculate rate of change if flag set.
          if (IROC.eq.1) then
            if (J.eq.1) then

C Should realy use previous value (if available).
              XV=0.0
            else
              XV=(XVAL(KGET)-XVALOLD(KGET))*float(NTS)/float(NOUT)
            endif
            XVALOLD(KGET)=XVAL(KGET)
            XVAL(KGET)=XV
          elseif (IROC.eq.-1) then
            XVAL(KGET)=XVAL(KGET)*float(NOUT)/float(NTS)
          endif

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C Add half a timestep to TIME if data averaged.
          IHRD=(IDAY-IODS)*24
          call DATIME(J,ATIME)
          TIME=float(IHRD)+ATIME

C Within requested output period?
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 421

C If there is occupancy filter and occupancy then include in check.
C Assume fully occupied.
          ih=int(ATIME+1.)
          ioc=1
          if(iocupf.eq.1)then
            if(menutype.ne.3) call getocup(IZONE,IDAY,ih,ioc,ier)
          endif
          if(ioc.ne.0) then
            if (XVAL(KGET).gt.DMAX) then
              DMAX=XVAL(KGET)
              TMAX=TIME
            endif
            if (XVAL(KGET).lt.DMIN) then
              DMIN=XVAL(KGET)
              TMIN=TIME
            endif
            DAVE=DAVE+XVAL(KGET)
            NITMS=NITMS+1
          endif
 421    CONTINUE
 10   continue

C Calculate average.
      DTOT=DAVE
      if (ABS(DAVE).gt.1.E-10.and.NITMS.ne.0)then
        DAVE=DAVE/float(NITMS)
      else
        DAVE=0.
      endif

      return
      END

