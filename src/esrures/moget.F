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
C GOGETIT:  Retrieves data for one day from the result library according
C           to array IGETNO and KGET. It sues GET1 & GET2 commons.
C GETSTATS: Efficient routine to scan the zone results records for save
C           levels 2-4 finding the stats for a single topic.
C MMACAL:   Calculates stats for topics via calls to gogetit, valiav etc.

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

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      integer YAXSET,ADDLIN,horaxisdiv

C Reset axis max and min values to 1.0 and 0.0 resp.
C YAXSET: Axis max min set =1 automatically scalled, 2= user set.
      do I=1,5
        YMAX(I)=1.0
        YMIN(I)=0.0
        YAXSET(I)=0
      end do

C Set add profile label ADDLIN=0 i.e. no add
      ADDLIN=0
      horaxisdiv=0  ! assume default horizontal divisions

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
        LNSLABEL(I)=0
        GLABEL(I)='  '
        LNGLABEL(I)=0
        TABLABEL(I)='  '
        LNTABLABEL(I)=0
 10   continue
      ialstused=0

      return
      end

C ************************ EDGOGET ************************
C EDGOGET allows removal of output requests.
      SUBROUTINE EDGOGET
#include "epara.h"
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/GETPIK/NGET,IGETNO(MZS,9)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)

      character VERT(MZS+3)*36,KEY*1
      integer ORDER,OGET
      dimension INCGET(MZS),ORDER(MZS),OGET(MZS)
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDGOGET'  ! set for subroutine

C Sort ORDER of IGETNO array for display purposes.
      do I=1,NGET
        ORDER(I)=I
        OGET(I)=IGETNO(I,1)*10000 + IGETNO(I,2)*100 + IGETNO(I,3)
      end do
      call SORTI(OGET,ORDER,NGET,2)

C Set all values of incget to 1
      do I=1,NGET
        INCGET(I)=1
      end do

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
C current menu line index. Build up text strings for the menu based
C on the non-truncated TABLABEL rather than the graphs GLABEL.
      VERT(1)='  Metric             Active'
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          if (INCGET(ORDER(L)).eq.0) then
C            WRITE(VERT(M),'(a,1x,2a)')KEY,GLABEL(ORDER(L)),'  '
            WRITE(VERT(M),'(a,1x,2a)')KEY,TABLABEL(ORDER(L))(1:32),'  '
          else
C            WRITE(VERT(M),'(a,1x,2a)')KEY,GLABEL(ORDER(L)),' *'
            WRITE(VERT(M),'(a,1x,2a)')KEY,TABLABEL(ORDER(L))(1:32),' *'
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
      helptopic='res_metric_list_edit'
      call gethelptext(helpinsub,helptopic,nbhelp)

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
                LNSLABEL(K)=LNSLABEL(K+1)
                GLABEL(K)=GLABEL(K+1)
                LNGLABEL(K)=LNGLABEL(K+1)
                TABLABEL(K)=TABLABEL(K+1)
                LNTABLABEL(K)=LNTABLABEL(K+1)
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
                LNSLABEL(K)=0
                GLABEL(K)='  '
                LNGLABEL(K)=0
                TABLABEL(K)='  '
                LNTABLABEL(K)=0
                INCGET(K)=1
              endif
 30         continue
            NGET=NGET-1
            goto 21
          endif
 20     continue
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN
        helptopic='res_metric_list_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
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
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "net_flow.h"
#include "tdf2.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/recov01/pifltog,lpifl
      COMMON/DNORGH/IRTYPE

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C IGETFLG - flags for menu set up:
C IOCUPF current state of occupancy filter (0=none; 1=filtering on).
C IROC calculate rate of change of metric.
C ialstused is the anchor point used for selection (0 if unused)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)

C Optional title for graph and up to 10 other labels and their positions.
      common/grextras/graftitle,grlbl(10),ngrlbl,lblpx(10),lblpy(10)

      character SLABEL*32,GLABEL*20,TABLABEL*36
      COMMON/GETLABEL/SLABEL(MZS),GLABEL(MZS),TABLABEL(MZS)
      integer LNSLABEL,LNGLABEL,LNTABLABEL  ! lengths for label strings
      COMMON/LNGETLABEL/LNSLABEL(MZS),LNGLABEL(MZS),LNTABLABEL(MZS)

      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv

      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      integer ireportunit ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit

C Pre calculated data (filtered).
      common/MMAFDAT/IFILT,RDFMAX(MCOM,3,MZRL),RDFMIN(MCOM,3,MZRL),
     &    RDFAVE(MCOM,3,MZRL),RDTFMAX(MCOM,3,MZRL),RDTFMIN(MCOM,3,MZRL)

C Comfort parameters and hash table.
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset,ioucpz(MCOM,72)
      
C MRT Sensors.
      common/MRTC/NCUB,XOC(MCUB),YOC(MCUB),ZOC(MCUB),DXC(MCUB),
     &            DYC(MCUB),DZC(MCUB),CANG(MCUB),IVFOK(MCUB),CUBN(MCUB)

      CHARACTER ITEM(35)*24, MTITLE*22, values*20, CUBN*6
      CHARACTER ITEMS(18)*23
      CHARACTER ITEMC(25)*24
      character xfile*144,tg*1,delim*1
      character SUFFIX*8
      character ll1*8,ll2*8,ll3*8
      CHARACTER outs*124,pifltog*4,lpifl*72
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16
      character graftitle*64,grlbl*24,t64*64,t24*24

C Note: 60 is the maximum number of items associated with an anchor point.
      dimension ISP(MS),IZNSEL(MCON),ISFSEL(MCON)
      dimension IVALS(17),IVALC(21)

      integer YAXSET,ADDLIN,horaxisdiv
      integer nbofhelp   ! how many help lines to display for different menu types.
      integer loop       ! for do loops
      integer IAC        ! for user selection
      integer IOT,IWM    ! for radio buttons
      integer NITMS,INO  ! max items and current menu item
      integer IZLEN      ! length of ZSDESS
      integer ISLEN      ! length of SUFFIX
      integer ITLEN      ! length of ZSDES
      integer lastireportunit  ! to detect change in unit
      integer NSEL       ! nb of items selected
      integer IZONE      ! user selected zone
      integer ISFN       ! user selected surface

      logical SLIST,NLIST,MRTLIST,AGRLIST,OKL,dok,ok
#ifdef OSI
      integer iid1,iid2
#else
      integer*8 iid1,iid2
#endif

      logical dmdsok,xst

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='GOMENU'  ! set for subroutine

C The logical variables are required so as user is asked for additional 
C information about the choice of metric if necessary. NLIST=.true. implies 
C SLIST=.true. which are asked for all active zones (listed in NZNO).
      SLIST=.FALSE.   ! do we need to ask about surfaces
      NLIST=.FALSE.
      MRTLIST=.FALSE.
      AGRLIST=.FALSE. ! assume heating & cooling & lighting loads separately drawn
      IAC=0; NSEL=0; IZONE=0; ISFN=0  ! initial values
      ireportunit=0 ! assume normal units for reports

C Check existance of demands file (it will have been scanned elsewhere).
      call FINDFIL(bdmds,XST)
      if(XST)then
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

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

C Assume we are starting with feedback to text area of interface.
C Set initial width to 22 columns. If writing to file MNGET is
C reset later in the code to 42 columns.
        MTITLE='Performance metrics'
        MNGET=22  ! number of columns 11 char wide for 248 char buffer
      elseif (MENUTYPE.eq.5) then
        MTITLE='Summary statistics'
        MNGET=1
        ireportunit=0 ! assume normal units for stats
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
        SLABEL(NGET)='Zone energy balance (kWhr)'; LNSLABEL(NGET)=26
      elseif (MENUTYPE.eq.10) then

C IGET = 60 and MENUTYPE 10:  Energy demands.
        MTITLE='Energy demands'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=60
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Zone energy demands (kWhrs)'; LNSLABEL(NGET)=27
      elseif (MENUTYPE.eq.11) then

C IGET = 63 and MENUTYPE 11: Zone casual gains distribution.
        MTITLE='Casual gains distrib.'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=63
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Casual gains distribution (kWhr)'
        LNSLABEL(NGET)=32
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
        LNSLABEL(NGET)=29
      elseif (MENUTYPE.eq.13) then

C IGET = 64 Monthly gains & losses. Use IGETNO(NGET,4) = 1 as 100Wh & 2 as kWh.
        helptopic='res_display_100wh'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' When displaying monthly values round',
     &    ' :','to nearest 100Wh (default)','to nearest kWh',IR,nbhelp)
        MTITLE='Monthly gains/losses'
        MNGET=MZS-1
        NGET=1
        IGETNO(NGET,1)=64
        IGETNO(NGET,4)=IR
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=12
        SLABEL(NGET)='Monthly zonal gains/losses'; LNSLABEL(NGET)=26
      elseif (MENUTYPE.eq.14) then

C IGET = 65 Surface condensation. Use IGETNO(NGET,3) to denote fixed
C RH (number greater than zero) or current RH (zero). Use 
C IGETNO(NGET,4)=1 denote verbose and 0 to be summary.
        MTITLE='Surface condensation'
        MNGET=MZS-1
        NGET=1
        helptopic='res_rh_for_condensation'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' Evaluate condensation using:',
     &    '  ','current zone RH','specified RH',IR,nbhelp)
        if(IR.eq.1)then
          IGETNO(NGET,3)=0
        else
          iv=70
          CALL EASKI(iv,'Specified humidity (%) ? ',' ',
     &      1,'-',100,'-',70,'Specified RH',IER,nbhelp)
          IGETNO(NGET,3)=iv
        endif
        CALL EASKAB(' Condensation report options:',
     &    '  ','summary','show each timestep',IR,nbhelp)
        if(IR.eq.1)then
          IGETNO(NGET,4)=0
        elseif(IR.eq.2)then
          IGETNO(NGET,4)=1
        endif
        IGETNO(NGET,1)=64
        IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
        IGETNO(NGET,8)=4;  IGETNO(NGET,9)=20
        SLABEL(NGET)='Surface condensation'; LNSLABEL(NGET)=20
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
        if (MENUTYPE.eq.4) then
          MNGET=42  ! number of columns 11 char wide for 800 char buffer
                    ! is ~70 but must also acommodate heading text
          call usrmsg('Resetting max columns to 42',' ','-')
        endif
      elseif(ixopen.eq.0)then
        ITEM(23)='> Display to >> screen'
        if (MENUTYPE.eq.4) then
          MNGET=22  ! number of columns 11 char wide for 248 char buffer
          call usrmsg('Resetting max columns to 22',' ','-')
        endif
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

C Make up text for 27th and 28th menu slots.
      if (MENUTYPE.eq.3) then
        ITEM(27)='+ Add another profile'
        if(ireportunit.eq.0)then
          ITEM(28)='" Units >> normal '
        elseif(ireportunit.eq.1)then
          ITEM(28)='" Units >> W or Whr'
        elseif(ireportunit.eq.2)then
          ITEM(28)='" Units >> kW or kWhr'
        else
          ITEM(28)='" Units >> unknown'
        endif
      else

C Set delimiter, and for tabular lists allow unit choice. Stats is supposed
C to be normal units ??
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
        if (MENUTYPE.eq.4) then
          if(ireportunit.eq.0)then
            ITEM(28)='" Units >> normal '
          elseif(ireportunit.eq.1)then
            ITEM(28)='" Units >> W or Whr'
          elseif(ireportunit.eq.2)then
            ITEM(28)='" Units >> kW or kWhr'
          else
            ITEM(28)='" Units >> unknown'
          endif
        elseif (MENUTYPE.eq.5) then
          if(ireportunit.eq.0)then
            ITEM(28)='" Units >> normal '
          else
            ITEM(28)='                     '
          endif
        endif
      endif

      if(MENUTYPE.eq.3) then
        ITEM(29)='! Draw graph    '
        ITEM(30)='* Labels        '
        nl=2
      elseif(MENUTYPE.eq.4)then
        ITEM(29)='! List data    '
        nl=1
      else
        nl=0
      endif
      ITEM(29+nl)='? Help           '
      ITEM(30+nl)='- Exit           '

      NITMS=30+nl
      INO=-2

C Instanciate the help text for this menu.
      helptopic='res_select_metrics_list'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(MENUTYPE.eq.1) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.2) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.3) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.4) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.5) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.6) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.7) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.8) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.9) then
        helptopic='select_energy_bal'
        call gethelptext(helpinsub,helptopic,nbhelp)
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.10) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.11) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.12) then
        helptopic='select_surface_bal'
        call gethelptext(helpinsub,helptopic,nbhelp)
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.13) then
        nbofhelp=nbhelp
      elseif (MENUTYPE.eq.14) then
        nbofhelp=nbhelp
      endif

      CALL EMENU(MTITLE,ITEM,NITMS,INO)

C Check for ADDLIN in time series plot - must choose metric if ADDLIN=1.
      if (MENUTYPE.eq.3.and.ADDLIN.eq.1) then
        if (INO.lt.5.or.INO.gt.26) goto 1
      endif

C Depending on the option (INO) selected process the request.
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

        inoc=0
        idnoc=0
        if(irtype.eq.0)then
          call MENUATOL('Select a climate metric',' Climate choices:',
     &      'a Ambient temperature','b Solar Dir N','c Solar diffuse',
     &      'd Wind speed','e Wind direction','f Ambient RH',
     &      'g Sky illumunance',' ',' ',' ',' ',' ',inoc,idnoc,nbhelp)
        else
          call MENUATOL('Select a climate metric',' Climate choices:',
     &      'a Ambient temperature','b Solar GlobH','c Solar diffuse',
     &      'd Wind speed','e Wind direction','f Ambient RH',
     &      'g Sky illumunance',' ',' ',' ',' ',' ',inoc,idnoc,nbhelp)
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
          GLABEL(NGET)='Ambient db Tmp'; LNGLABEL(NGET)=14
          if(IROC.eq.0)then
            SLABEL(NGET)='Ambient db temperature (degC)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ambient db temp change (degC/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ambient db temp hours (degC.hr)'
            LNSLABEL(NGET)=31
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.2)then

C IGET = 19 Direct solar. Set text based on current value of irtype.
          IGETNO(NGET,1)=19
          IGETNO(NGET,6)=-104; IGETNO(NGET,7)=9
          IGETNO(NGET,8)=3;    IGETNO(NGET,9)=14
          if(irtype.eq.0)then
            GLABEL(NGET)='Direct nor solar'; LNGLABEL(NGET)=16
            if(IROC.eq.0)then
              SLABEL(NGET)='DN solar intensity (W/m^2)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.1)then
              SLABEL(NGET)='DN solar intens (W/m^2/hr)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='DN solar intens (Whr/m^2)'
              LNSLABEL(NGET)=25
            endif
          else
            GLABEL(NGET)='Glo horiz solar'; LNGLABEL(NGET)=15
            if(IROC.eq.0)then
              SLABEL(NGET)='Glo hor solar intensity (W/m^2)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Glo hor solar intens (W/m^2/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Glo hor solar intens (Whr/m^2)'
              LNSLABEL(NGET)=30
            endif
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.3)then
          IGETNO(NGET,1)=20
          IGETNO(NGET,6)=-104; IGETNO(NGET,7)=5
          IGETNO(NGET,8)=3;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Diffuse solar'; LNGLABEL(NGET)=13
          if(IROC.eq.0)then
            SLABEL(NGET)='Diffuse solar intensity (W/m^2)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Diffuse solar intens (W/m^2/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Diffuse solar intens (Whr/m^2)'
            LNSLABEL(NGET)=30
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.4)then
          IGETNO(NGET,1)=21
          IGETNO(NGET,6)=-203; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Wind speed'; LNGLABEL(NGET)=10
          if(IROC.eq.0)then
            SLABEL(NGET)='Wind speed (m/sec)'
            LNSLABEL(NGET)=18
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wind speed change (m/sec/hr)'
            LNSLABEL(NGET)=28
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wind speed hours (m.hr/sec)'
            LNSLABEL(NGET)=27
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=34
          IGETNO(NGET,6)=-203; IGETNO(NGET,7)=4
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Wind direc'; LNGLABEL(NGET)=10
          if(IROC.eq.0)then
            SLABEL(NGET)='Wind direction (deg)'
            LNSLABEL(NGET)=20
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wind direction change (deg/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wind direction hours (deg.hr)'
            LNSLABEL(NGET)=29
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=22
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=7
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=14
          GLABEL(NGET)='Ambient RH'; LNGLABEL(NGET)=10
          if(IROC.eq.0)then
            SLABEL(NGET)='Ambient RH (%)'; LNSLABEL(NGET)=14
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ambient RH change (%/hr)'; LNSLABEL(NGET)=24
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ambient RH hours (%.hr)'; LNSLABEL(NGET)=23
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        elseif(inoc.eq.7)then
          IGETNO(NGET,1)=26
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=8
          IGETNO(NGET,8)=4;    IGETNO(NGET,9)=10
          GLABEL(NGET)='Sky illum'; LNGLABEL(NGET)=9
          if(IROC.eq.0)then
            SLABEL(NGET)='Sky illuminance (lux)'; LNSLABEL(NGET)=21
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Sky illuminance change (lux/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Sky illuminance hours (lux.hr)'
            LNSLABEL(NGET)=30
          endif
          write(TABLABEL(NGET),'(2a)')GLABEL(NGET)(1:LNGLABEL(NGET))
          LNTABLABEL(NGET)=LNSLABEL(NGET)
        endif

      elseif(INO.EQ.6)then

C Temperatures sub-menu.
        inoc=0
        idnoc=0
        helptopic='res_temperature_metrics'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call MENUATOL('Select a temperature metric.',
     &    ' Temperature choices:','a Zone db T',
     &    'b Zone db T - ambient db T','c Zone db T - other zone db T',
     &    'd Zone control point T','e Zone Resultant T',
     &    'f Mean Radiant T (area wtd)','g Mean Radiant T (at sensor)',
     &    'h Dew point T ','i Surf inside face T',
     &    'j Surf T - dewpoint T','k Surf outside face T',
     &    'l Surf node T',inoc,idnoc,nbhelp)
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
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone db temp change (degC/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone db temp hours (degC.hr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,1)=29
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Del dbT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Delta db temperature (degC)'
            LNSLABEL(NGET)=27
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Delta db temp change (degC/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Delta db temp hours (degC.hr)'
            LNSLABEL(NGET)=29
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
            LNSLABEL(NGET)=30
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chg D znT - other znT (degC/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='D znT - other znT hrs(degC.hr)'
            LNSLABEL(NGET)=30
          endif
        elseif(inoc.eq.4)then

C IGET = 3 Control point temperature. 
          IGETNO(NGET,1)=3
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=9
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Cpt T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Control pt temperature (degC)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Control pt temp change (degC/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Control pt temp hours (degC.hr)'
            LNSLABEL(NGET)=31
          endif
        elseif(inoc.eq.5)then

C IGET = 6  Resultant temperature.
          IGETNO(NGET,1)=6
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=5
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' Res T'
          if(IROC.eq.0)then
            SLABEL(NGET)='Resultant temperature (degC)'
            LNSLABEL(NGET)=28
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Resultant temp change (degC/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Resultant temp hours (degC.hr)'
            LNSLABEL(NGET)=30
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
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Mean radiant Tmp change(degC/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Mean radiant Tmp hours(degC.hr)'
            LNSLABEL(NGET)=31
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
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Mean radiant Tmp change(degC/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Mean radiant Tmp hours (degC.hr)'
            LNSLABEL(NGET)=32
          endif
        elseif(inoc.eq.8)then

C IGET = 14  Zone dew point.
          IGETNO(NGET,1)=14
          IGETNO(NGET,6)=-202; IGETNO(NGET,7)=2
          IGETNO(NGET,8)=1;    IGETNO(NGET,9)=12
          SUFFIX=' dew ptT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone dew point (degC)'
            LNSLABEL(NGET)=21
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone dew point change (degC/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone dew point hours (degC.hr)'
            LNSLABEL(NGET)=30
          endif
        elseif(inoc.eq.9)then
          slist=.true.
          IGETNO(NGET,1)=4
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=2
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX=' InSurT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Surface temp inside (degC)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Surface temp inside (degC/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Surf temp inside hours (degC.hr)'
            LNSLABEL(NGET)=32
          endif
        elseif(inoc.eq.10)then
          slist=.true.
          IGETNO(NGET,1)=35
          IGETNO(NGET,6)=1; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=20
          SUFFIX='Dsur-dpT'
          if(IROC.eq.0)then
            SLABEL(NGET)='Delta surf T - zone dp T (degC)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Delta surf - zn dp T (degC/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Delta surfT - zn dpT hr(degC.hr)'
            LNSLABEL(NGET)=32
          endif
        elseif(inoc.eq.11)then
          IF(ISAVE.lt.3)then
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
            LNSLABEL(NGET)=27
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Surface temp outside (degC/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Surface out temp hrs (degC.hr)'
            LNSLABEL(NGET)=30
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
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Surface node temp (degC/hr)'
              LNSLABEL(NGET)=27
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Surf node temp hours (degC.hr)'
              LNSLABEL(NGET)=30
            endif
          endif
        endif

      elseif(INO.EQ.7)then

C IGET = 23  Comfort, ask which index and initialise.
        helptopic='res_comfort_metrics'
        call gethelptext(helpinsub,helptopic,nbhelp)
        inoc=0
        idnoc=0
        call MENUATOL('Select a comfort metric',' Comfort choices:',
     &    'a Predicted Mean Vote (PMV)','b PMV using SET',
     &    'c Percentage Dissatisfied (PPD)','d Local delta T head-foot',
     &    'e Dissatisfied due to floor T','f Diss. warm/ cool ceiling',
     &    'g Diss. wall rad T assymetry','  ','  ','  ','  ','  ',
     &    inoc,idnoc,nbhelp)
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
            SLABEL(NGET)='Comfort PMV (-)'; LNSLABEL(NGET)=15
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PMV change/hr (-)'; LNSLABEL(NGET)=25
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PMV hours) (-)'; LNSLABEL(NGET)=23
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,3)=2
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PMV'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PMV (set)'; LNSLABEL(NGET)=17
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PMV (set) (change/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PMV set hours)'; LNSLABEL(NGET)=23
          endif
        elseif(inoc.eq.3)then
          IGETNO(NGET,3)=3
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PPD (%)'; LNSLABEL(NGET)=15
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PPD (% change/hr)'
            LNSLABEL(NGET)=25
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort (PPD % hours)'; LNSLABEL(NGET)=21
          endif
        elseif(inoc.eq.4) then
          IGETNO(NGET,1)=50
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PD head/foot (%)'
            LNSLABEL(NGET)=24
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PD hd/ft (% change/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort PD hd/ft (% hours)'
            LNSLABEL(NGET)=26
          endif
        elseif(inoc.eq.5) then
          IGETNO(NGET,1)=51
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PPD'
          if(IROC.eq.0)then
            SLABEL(NGET)='Comfort PD floor (%)'; LNSLABEL(NGET)=20
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Comfort PD floor (% change/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Comfort PD floor (% hours)'
            LNSLABEL(NGET)=26
          endif
          slist=.true.
        elseif(inoc.eq.6) then
          IGETNO(NGET,1)=52
          IGETNO(NGET,4)=5
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          SUFFIX=' PDceil'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ceiling discomf PD (%)'; LNSLABEL(NGET)=22
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ceiling discomf PD (% change/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ceiling discomf PD (% hours)'
            LNSLABEL(NGET)=28
          endif
          MRTLIST=.true.
        elseif(inoc.eq.7) then
          IGETNO(NGET,1)=53
          IGETNO(NGET,6)=-305; IGETNO(NGET,7)=8; IGETNO(NGET,8)=4
          MRTLIST=.true.
          helptopic='comfort_sensor_direction'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABCD('Sensor facing which direction:',' ','South',
     &      'East','North','West',ICF,nbhelp)
          IGETNO(NGET,4)=ICF
          SUFFIX=' PDwall'
          if(IROC.eq.0)then
            SLABEL(NGET)='Wall discomf PD (%)'; LNSLABEL(NGET)=19
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Wall discomf PD (% change/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Wall discomf PD (% hours)'
            LNSLABEL(NGET)=25
          endif
        endif
        idd=1
        izz=1
        CALL MOCMFT(idd,izz,'I')

      elseif(INO.EQ.8)then

C Solar processes.
        inoc=0
        idnoc=0
        helptopic='res_menu_solar_select'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call MENUATOL('Select a solar flux metric',' Solar choices:',
     &    'a Solar entering from outside','b Solar entering from adj',
     &    'c Solar absorbed in zone','  ','  ','  ','  ','  ',
     &    '  ','  ','  ','  ',inoc,idnoc,nbhelp)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
                
        NGET=NGET+1
        IGETNO(NGET,6)=-104; IGETNO(NGET,8)=2; IGETNO(NGET,9)=12
        if(inoc.eq.1)then

C IGET = 38 Solar entering zone from outside (W)
          IGETNO(NGET,1)=38
          IGETNO(NGET,7)=7
          SUFFIX=' SolinEx'
          if(IROC.eq.0)then
            SLABEL(NGET)='Solar entering from outside (W)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng solar enter outside (W/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Solar entering from outside(Whr)'
            LNSLABEL(NGET)=32
          endif
        elseif(inoc.eq.2)then

C IGET = 39 Solar entering zone from adj zones (W)
          IGETNO(NGET,1)=39
          IGETNO(NGET,7)=8
          SUFFIX=' SolinAd'
          if(IROC.eq.0)then
            SLABEL(NGET)='Solar entering from adj zn (W)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng solar enter adj zn (W/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Solar entering from adj zn (Whr)'
            LNSLABEL(NGET)=32
          endif
        elseif(inoc.eq.3)then

C IGET = 40 Solar absorbed in zone (W)
          IGETNO(NGET,1)=40
          IGETNO(NGET,7)=9
          SUFFIX=' TSolabs'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total solar absorbed in zn (W)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Chng zn solar absorbed (W/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total solar absorbed in zn (Whr)'
            LNSLABEL(NGET)=32
          endif
        endif

      elseif(INO.EQ.10)then

C Zone flux paths. Additional items could be included if we went
C to an emenu selection for izver >=4. << to be done >>
        inoc=0
        idnoc=0
        helptopic='res_menu_zone_flux'
        call gethelptext(helpinsub,helptopic,nbhelp)
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
            SLABEL(NGET)='Infiltration (W)'; LNSLABEL(NGET)=16
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Infiltration change (W/hr)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Infiltration (Whr)'; LNSLABEL(NGET)=18
          endif
        elseif(inoc.eq.2)then

C IGET = 12  Ventilation.
          IGETNO(NGET,1)=12
          IGETNO(NGET,7)=9
          SUFFIX=' Vent'
          if(IROC.eq.0)then
            SLABEL(NGET)='Ventilation (W)'; LNSLABEL(NGET)=15
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Ventilation change (W/hr)'
            LNSLABEL(NGET)=25
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Ventilation (Whr)'; LNSLABEL(NGET)=17
          endif
        elseif(inoc.eq.3)then
C IGET = 15  Casual gains by type.
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=4
          IGETNO(NGET,7)=13
          SUFFIX=' OccCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total occupant gain (W)'; LNSLABEL(NGET)=23
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total occupant gain chg (W/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total occupant gain (Whr)'
            LNSLABEL(NGET)=25
          endif
        elseif(inoc.eq.4)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=5
          IGETNO(NGET,7)=14
          SUFFIX=' LitCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total lighting gain (W)'; LNSLABEL(NGET)=23
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total lighting gain chg (W/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total lighting gain (Whr)'; LNSLABEL(NGET)=25
          endif
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=6
          IGETNO(NGET,7)=15
          SUFFIX=' EqpCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total small power gain (W)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total small power gain (W/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total small power gain (Whr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=15
          IGETNO(NGET,3)=7
          IGETNO(NGET,7)=16
          SUFFIX=' CtlCasG'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total controled casual gain (W)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total ctld casual gain (W/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total ctld casual gain (Whr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.7)then

C IGET = 42 Zone balance opaque exterior conv.
          IGETNO(NGET,1)=42
          IGETNO(NGET,7)=17
          SUFFIX=' OpqcvE'
          if(IROC.eq.0)then
            SLABEL(NGET)='Opaque surf exterior conv (W)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Opaque surf exterior conv (W/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Opaque surf exterior conv (Whr)'
            LNSLABEL(NGET)=31
          endif
        elseif(inoc.eq.8)then

C IGET = 43 Zone balance opaque partitions conv.
          IGETNO(NGET,1)=43
          IGETNO(NGET,7)=7
          SUFFIX=' OpqcvP'
          if(IROC.eq.0)then
            SLABEL(NGET)='Opaque partitions conv (W)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Opaque partitionns conv (W/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Opaque partitionns conv (Whr)'
            LNSLABEL(NGET)=29
          endif
        elseif(inoc.eq.9)then

C IGET = 44 Zone balance transparent exterior conv.
          IGETNO(NGET,1)=44
          IGETNO(NGET,7)=8
          SUFFIX=' TrncvE'
          if(IROC.eq.0)then
            SLABEL(NGET)='Transp surf exterior conv (W)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Transp surf exterior conv (W/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Transp surf exterior conv (Whr)'
            LNSLABEL(NGET)=31
          endif
        elseif(inoc.eq.10)then

C IGET = 45 Zone balance transparent partitions conv.
          IGETNO(NGET,1)=45
          IGETNO(NGET,7)=9
          SUFFIX=' TrncvP'
          if(IROC.eq.0)then
            SLABEL(NGET)='Transp partitions conv (W)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Transp partitions conv (W/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Transp partitions conv (Whr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.11)then
          IGETNO(NGET,1)=16
          IGETNO(NGET,4)=3
          IGETNO(NGET,7)=10
          IGETNO(NGET,8)=2
          SUFFIX=' TotCnv'
          if(IROC.eq.0)then
            SLABEL(NGET)='Total surf convection (W)'
            LNSLABEL(NGET)=25
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Total surf conv (W/hr)'; LNSLABEL(NGET)=22
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Total surf conv (Whr)'; LNSLABEL(NGET)=21
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
        helptopic='res_menu_surface_flux'
        call gethelptext(helpinsub,helptopic,nbhelp)
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
     &    23,18,ITEMS,'surface fluxes',IER,nbhelp)
        if(IVALS(1).eq.0)goto 1
        dok=.false.
        call askok('Report per square meter of surface',' ',OK,dok,
     &    nbhelp)
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
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Conduction @an ins. face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then         
              SLABEL(NGET)='Cond chng @ an ins face (W/hr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Cond chng @ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Cond chng @ an ins face (Whr)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Cond chng @an ins face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif
          endif
        elseif(inoc.eq.2)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=8
          SUFFIX=' InSfCnv'
          if(IROC.eq.0)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Convection @ an inside face (W)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Convection @ inside face (W/m^2)'
              LNSLABEL(NGET)=32
            endif          
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Conv chng @ an ins face (W/hr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Conv chng @ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Convection @ an ins face (Whr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Convection @ ins face (Whr/m^2)'
              LNSLABEL(NGET)=31
            endif
          endif
        elseif(inoc.eq.3)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' InSfLW'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad @ an inside face (W)'
              LNSLABEL(NGET)=27
            else
              SLABEL(NGET)='LW rad @ an inside face (W/m^2)'
              LNSLABEL(NGET)=31
            endif            
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW chng @ an ins face (W/hr)'
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='LW chng @ ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=31
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad @ an ins face (Whr)'
              LNSLABEL(NGET)=26
            else
              SLABEL(NGET)='LW rad @ an ins face (Whr/m^2)'
              LNSLABEL(NGET)=30
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
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='Surface solar abs inside (W/m^2)'
              LNSLABEL(NGET)=32
            endif       
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs inside (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Surf solar abs ins (W/(hr*m^2))'
              LNSLABEL(NGET)=31
            endif    
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs inside (Whr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Surf solar abs inside (Whr/m^2)'
              LNSLABEL(NGET)=31
            endif
          endif
        elseif(inoc.eq.5)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=11
          SUFFIX=' InSfOcc'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an inside face (W)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Occup rad @ an ins face (W/m^2)'
              LNSLABEL(NGET)=31
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an ins face (W/hr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Occup rad @ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Occup rad @ an ins face (Whr)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Occup rad @an ins face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif         
          endif
        elseif(inoc.eq.6)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=12
          SUFFIX=' InSfLts'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Lights rad @ an inside face (W)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Lights rad @ inside face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Lights rad @ an ins face (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Lights rad@ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif           
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Lights rad @ an ins face (Whr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Lights rad @ ins face (Whr/m^2)'
              LNSLABEL(NGET)=31
            endif
          endif
        elseif(inoc.eq.7)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=13
          SUFFIX=' InSfEqp'
          if(IROC.eq.0)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Sm Pwr rad @ an inside face (W)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Sm Pwr rad @ inside face (W/m^2)'            
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Sm Pwr rad @ an ins face (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Sm Pwr rad@ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Sm Pwr rad @ an ins face (Whr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Sm Pwr rad @ ins face (Whr/m^2)'            
              LNSLABEL(NGET)=31
            endif
          endif
        elseif(inoc.eq.8)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=14
          SUFFIX=' InSfCtC'
          if(IROC.eq.0)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='Ctld cas @ an inside face (W)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Ctld cas @an inside face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ctld cas @ an ins face (W/hr)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Ctld cas @ ins face (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ctld cas @ an ins face (Whr)'
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='Ctld cas @ an ins face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif           
          endif
        elseif(inoc.eq.9)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=15
          SUFFIX=' InSfHtS'
          if(IROC.eq.0)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Heat stored @ an inside face (W)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='Heat stored @an ins face (W/m^2)'            
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Ht str chng @ an ins face (W/hr)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='Ht str chng @ins face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif          
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Ht str chng @ an ins face (Whr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Ht str chng @ ins face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif         
          endif
        elseif(inoc.eq.10)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=16
          SUFFIX=' InSfPlt'
          if(IROC.eq.0)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Rad plant @ an inside face (W)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Rad plant @ inside face (W/m^2)'
              LNSLABEL(NGET)=31
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='Rad plt chng @ ins face (W/hr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Rad plt chng@ins face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif           
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Rad plant @ inside face (Whr)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Rad plant @inside face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif
          endif
        elseif(inoc.eq.11)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=17
          SUFFIX=' ExSfCnd'
          if(IROC.eq.0)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Conduction @ other face (W)'
              LNSLABEL(NGET)=27
            else
              SLABEL(NGET)='Conduction @ other face (W/m^2)'              
              LNSLABEL(NGET)=31
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then           
              SLABEL(NGET)='Cond chng @ other face (W/hr)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Cond chng @other face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Conduction @ other face (Whr)'
              LNSLABEL(NGET)=29
            else            
              SLABEL(NGET)='Conduction @other face (Whr/m^2)'
              LNSLABEL(NGET)=32
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
              LNSLABEL(NGET)=27
            else
              SLABEL(NGET)='Convection @ other face (W/m^2)'
              LNSLABEL(NGET)=31
            endif      
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Conv chng @other face (W/hr)'
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='Conv chng @other face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then  
              SLABEL(NGET)='Convection @other face (Whr)'
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='Convection @other face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif           
          endif
        elseif(inoc.eq.13)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWb'
          if(IROC.eq.0)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='LW rad > bld @ other face (W)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='LW rad > bld @other face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then 
              SLABEL(NGET)='LW>bld chng @ other face (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='LW>bld chng @oth face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > bld @ other face (Whr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='LW rad >bld@other face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif
          endif
        elseif(inoc.eq.14)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWs'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > sky @ other face (W)'
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='LW rad > sky @other face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW>sky chng @ other face (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='LW>sky chng @oth face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif            
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then          
              SLABEL(NGET)='LW rad > sky @ other face (Whr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='LW rad >sky@other face (Whr/m^2)'
              LNSLABEL(NGET)=32
            endif
          endif
        elseif(inoc.eq.15)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=9
          SUFFIX=' ExSfLWg'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > grnd @ other face (W)'
              LNSLABEL(NGET)=30
            else            
              SLABEL(NGET)='LW rad > grnd@other face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW>grnd chng @ other face (W/hr)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='LW>grnd chng@oth face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='LW rad > grnd @ other face (Whr)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='LW rad>grnd@other face (Whr/m^2)'
              LNSLABEL(NGET)=32
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
              LNSLABEL(NGET)=29
            else
              SLABEL(NGET)='Surf solar abs outside (W/m^2)'              
              LNSLABEL(NGET)=30
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs outside (W/hr)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='Surf solar abs outs (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar abs outside (Whr)' 
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Surf solar abs outside (Whr/m^2)'
              LNSLABEL(NGET)=32
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
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Surf solar incid outside (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar incid outs (W/hr)'
              LNSLABEL(NGET)=31
            else
              SLABEL(NGET)='Surf solar inc outs (W/(hr*m^2))'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Surface solar incid outs (Whr)'
              LNSLABEL(NGET)=30
            else
              SLABEL(NGET)='Surf solar incid outs (Whr/m^2)'
              LNSLABEL(NGET)=31
            endif
          endif
        elseif(inoc.eq.18)then
          IGETNO(NGET,1)=25
          IGETNO(NGET,7)=15
          SUFFIX=' InSfHtS'
          if(IROC.eq.0)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Heat stored @ other face (W)'  
              LNSLABEL(NGET)=28
            else                    
              SLABEL(NGET)='Heat stored @ other face (W/m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Ht str chng @ other face (W/hr)'
              LNSLABEL(NGET)=31
            else                   
              SLABEL(NGET)='Ht str chng @oth face W/(hr*m^2)'
              LNSLABEL(NGET)=32
            endif
          elseif(IROC.eq.-1)then
            if (ISM.eq.0) then
              SLABEL(NGET)='Heat stored @ other face (Whr)'
              LNSLABEL(NGET)=30
            else                      
              SLABEL(NGET)='Heat str @ other face (Whr/m^2)'
              LNSLABEL(NGET)=31
            endif
          endif
        endif

      elseif(INO.EQ.12)then

        helptopic='res_menu_sensible_latent'
        call gethelptext(helpinsub,helptopic,nbhelp)
        inoc=0
        idnoc=0
        call MENUATOL('Select an HVAC load metric',' Load choices:',
     &    'a Sensible heating load','b Sensible cooling load',
     &    'c Dehumidification load','d Humidification load',
     &    'e Sensible H+C loads','f Latent H+C loads',
     &    'g All Sensible + latent load',
     &    'h Aggregate heating load','i Aggregate cooling load',
     &    'j Aggregate dehumidification','k Aggregate humidification',
     &    '  ',inoc,idnoc,nbhelp)
        if(inoc.eq.0)goto 1
        call usrmsg('  ','  ','-')
        NGET=NGET+1
        IGETNO(NGET,8)=2
        IGETNO(NGET,9)=12
        if(inoc.eq.1)then

C IGET = 8   Zone-side sensible heating load.
          IGETNO(NGET,1)=8; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' HeatInj'; AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              SLABEL(NGET)='Sensible heating load (kW)'
              LNSLABEL(NGET)=26
            else
              SLABEL(NGET)='Sensible heating load (W)'
              LNSLABEL(NGET)=25
            endif
          elseif(IROC.eq.1)then
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              SLABEL(NGET)='Sensible heating load ch (kW/hr)'
              LNSLABEL(NGET)=32
            else
              SLABEL(NGET)='Sensible heating load ch (W/hr)'
              LNSLABEL(NGET)=31
            endif
          elseif(IROC.eq.-1)then
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              SLABEL(NGET)='Sensible heating load (kWhr)'
              LNSLABEL(NGET)=28
            else
              SLABEL(NGET)='Sensible heating load (Whr)'
              LNSLABEL(NGET)=27
            endif
          endif
        elseif(inoc.eq.2)then

C IGET = 9 Zone-side sensible cooling load.
          IGETNO(NGET,1)=9; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' CoolInj'; AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Sensible cooling load (kW)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Sensible cooling load ch (kW/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Sensible cooling load (kWhr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.3)then

C IGET = 30   Dehumidification load.
          IGETNO(NGET,1)=30; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' LatExt';  AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Latent dehumidification (kW)'
            LNSLABEL(NGET)=28
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Latent dehumidification (kW/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Latent dehumidification (kWhr)'
            LNSLABEL(NGET)=30
          endif
        elseif(inoc.eq.4)then

C IGET = 31   Humidification load.
          IGETNO(NGET,1)=31; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' LatInj';  AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Latent humidification (kW)'
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Latent humidification (kW/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Latent humidification (kWhr)'
            LNSLABEL(NGET)=28
          endif
        elseif(inoc.eq.5)then

C IGET = 10  Total sensible zone loads. 
          IGETNO(NGET,1)=10; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotSens'; AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone sensible load (kW)'
            LNSLABEL(NGET)=23
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone sensible load chg (kW/hr)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone sensible load (kWhr)'
            LNSLABEL(NGET)=25
          endif
        elseif(inoc.eq.6)then

C IGET = 32  All latent zone loads.
          IGETNO(NGET,1)=32; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotLat';  AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone latent load (kW)'; LNSLABEL(NGET)=21
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zone latent load change (kW/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zone latent load (kWhr)'; LNSLABEL(NGET)=23
          endif
        elseif(inoc.eq.7)then

C IGET = 33  All sensible+latent zone loads.
          IGETNO(NGET,1)=33; IGETNO(NGET,4)=1 ! signal individual
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          SUFFIX=' TotSenL'; AGRLIST=.FALSE.   ! draw separately
          if(IROC.eq.0)then
            SLABEL(NGET)='Zone sensible + latent load (kW)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Zn sens+latent load chng (kW/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Zn sens+latent load (kWhr)'
            LNSLABEL(NGET)=26
          endif
        elseif(inoc.eq.8)then

C IGET = 8  Aggregate sensible heating load for all currently selected zones.
          IGETNO(NGET,1)=8; IGETNO(NGET,4)=2 ! signal aggregate
          IGETNO(NGET,6)=-302; IGETNO(NGET,7)=6
          SUFFIX=' AgrHeat'; AGRLIST=.TRUE.   ! draw aggregate
          if(IROC.eq.0)then
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              SLABEL(NGET)='Agr Sensible heating (kW)'
              LNSLABEL(NGET)=25
            else
              SLABEL(NGET)='Agr Sensible heating (W)'
              LNSLABEL(NGET)=24
            endif
          elseif(IROC.eq.-1)then
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              SLABEL(NGET)='Agr Sensible heating (kWhr)'
              LNSLABEL(NGET)=27
            else
              SLABEL(NGET)='Agr Sensible heating (Whr)'
              LNSLABEL(NGET)=26
            endif
          endif
        elseif(inoc.eq.9)then

C IGET = 9 Aggregate sensible cooling load for all currently selected zones.
          IGETNO(NGET,1)=9; IGETNO(NGET,4)=2 ! signal aggregate
          IGETNO(NGET,6)=-302; IGETNO(NGET,7)=6
          SUFFIX=' AgrCool'; AGRLIST=.TRUE.   ! draw aggregate
          if(IROC.eq.0)then
            SLABEL(NGET)='Agr Sensible cooling (kW)'
            LNSLABEL(NGET)=25
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Agr Sensible cooling (kWhr)'
            LNSLABEL(NGET)=27
          endif
        elseif(inoc.eq.10)then

C IGET = 30  Aggregate Dehumidification load.
          IGETNO(NGET,1)=30; IGETNO(NGET,4)=2 ! signal aggregate
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=6
          SUFFIX=' AgrLtDH';  AGRLIST=.TRUE.   ! draw aggregate
          if(IROC.eq.0)then
            SLABEL(NGET)='Agr Latent dehumid (kW)'
            LNSLABEL(NGET)=23
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Agr Latent dehumid (kWhr)'
            LNSLABEL(NGET)=25
          endif
        elseif(inoc.eq.11)then

C IGET = 31 Aggregate  Humidification load.
          IGETNO(NGET,1)=31; IGETNO(NGET,4)=2 ! signal aggregate
          IGETNO(NGET,6)=-304; IGETNO(NGET,7)=6
          SUFFIX=' AgrLatH';  AGRLIST=.TRUE.   ! draw aggregate
          if(IROC.eq.0)then
            SLABEL(NGET)='Agr Latent humidif (kW)'
            LNSLABEL(NGET)=23
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Agr Latent humidif (kWhr)'
            LNSLABEL(NGET)=25
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
          LNSLABEL(NGET)=26
        elseif(IROC.eq.1)then
          SLABEL(NGET)='Zn relative humidity chg (%/hr)'
          LNSLABEL(NGET)=31
        elseif(IROC.eq.-1)then
          SLABEL(NGET)='Zn relative humidity (%hr)'
          LNSLABEL(NGET)=26
        endif

      elseif(INO.EQ.14)then

C IGET = 15  Casual gains.
        if(izver.eq.4)then
          helptopic='res_menu_casual_select'
          call gethelptext(helpinsub,helptopic,nbhelp)
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
          ITEMC(21)='aggregate occupant (c+r)'
          ITEMC(22)='aggregate lighting (c+r)'
          ITEMC(23)='aggregate equipment(c+r)'
          ITEMC(24)='aggregate lights+equip'
          if(dmdsok)then
            ITEMC(25)='dispersed demands'
          else
            ITEMC(25)='                 '
          endif
          IX=1
          CALL EPICKS(IX,IVALC,' ','Casual sensible and latent:',
     &      24,25,ITEMC,'Casual fluxes',IER,nbhelp)
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
              SLABEL(NGET)='Total casual gain (W)'; LNSLABEL(NGET)=21
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total casual gain change (W/hr)'
              LNSLABEL(NGET)=31
              elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain (Whr)'
              LNSLABEL(NGET)=23
            endif
          elseif(inoc.eq.2)then
            IGETNO(NGET,3)=2   ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=10
            SUFFIX=' CnvCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain convective (W)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gain conv chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Tot casual gain convective (Whr)'
              LNSLABEL(NGET)=32
            endif
          elseif(inoc.eq.3)then
            IGETNO(NGET,3)=3   ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' RadCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain radiant (W)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn radiant chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain radiant (Whr)'
              LNSLABEL(NGET)=31
            endif
          elseif(inoc.eq.4)then
            IGETNO(NGET,3)=9   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' LatCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain latent (W)'
              LNSLABEL(NGET)=28
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn latent chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain latent (Whr)'
              LNSLABEL(NGET)=30
            endif
          elseif(inoc.eq.5)then
            IGETNO(NGET,3)=4  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total occupant gain (W)'
              LNSLABEL(NGET)=23
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total occupant gain chg (W/hr)'
              LNSLABEL(NGET)=30
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total occupant gain (Whr)'
              LNSLABEL(NGET)=25
            endif
          elseif(inoc.eq.6)then
            IGETNO(NGET,3)=10   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant convec gain (W)'
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant convec gain chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant convec gain (Whr)'
              LNSLABEL(NGET)=26
            endif
          elseif(inoc.eq.7)then
            IGETNO(NGET,3)=11   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant radiant gain (W)'
              LNSLABEL(NGET)=25
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant radiant gain chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant radiant gain (Whr)'
              LNSLABEL(NGET)=27
            endif
          elseif(inoc.eq.8)then
            IGETNO(NGET,3)=12   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Occupant latent gain (W)'
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Occupant latent gain chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Occupant latent gain (Whr)'
              LNSLABEL(NGET)=26
            endif
          elseif(inoc.eq.9)then
            IGETNO(NGET,3)=5  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total lighting gain (W)'
              LNSLABEL(NGET)=23
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total lighting gain chg (W/hr)'
              LNSLABEL(NGET)=30
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total lighting gain (Whr)'
              LNSLABEL(NGET)=25
            endif
          elseif(inoc.eq.10)then
            IGETNO(NGET,3)=13   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting convec gain (W)'
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting convec gain chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting convec gain (Whr)'
              LNSLABEL(NGET)=26
            endif
          elseif(inoc.eq.11)then
            IGETNO(NGET,3)=14   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting radiant gain (W)'
              LNSLABEL(NGET)=25
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting radiant gain chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting radiant gain (Whr)'
              LNSLABEL(NGET)=27
            endif
          elseif(inoc.eq.12)then
            IGETNO(NGET,3)=15   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Lighting latent gain (W)'
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Lighting latent gain chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Lighting latent gain (Whr)'
              LNSLABEL(NGET)=24
            endif
          elseif(inoc.eq.13)then
            IGETNO(NGET,3)=6  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total small power gain (W)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total small power gain (W/hr)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total small power gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.14)then
            IGETNO(NGET,3)=16   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr convec gain (W)'
              LNSLABEL(NGET)=25
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Small pwr convec gain chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr convec gain (Whr)'
              LNSLABEL(NGET)=27
            endif
          elseif(inoc.eq.15)then
            IGETNO(NGET,3)=17   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr radiant gain (W)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Sml pwr radiant gain chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr radiant gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.16)then
            IGETNO(NGET,3)=18   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpLatG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Small pwr latent gain (W)'
              LNSLABEL(NGET)=25
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Small pwr latent gain chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Small pwr latent gain (Whr)'
              LNSLABEL(NGET)=27
            endif
          elseif(inoc.eq.17)then
            IGETNO(NGET,3)=8  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Frac of ctld casual gain (%)'
              LNSLABEL(NGET)=28
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Frac of ctld casual gain (%/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Frac of ctld casual gain (%hr)'
              LNSLABEL(NGET)=30
            endif
          elseif(inoc.eq.18)then
            IGETNO(NGET,3)=7  ! same as for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total controled casual gain (W)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total ctld casual gain (W/hr)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total ctld casual gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.19)then
            IGETNO(NGET,3)=19   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCnvG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Controlled convec gain (W)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Cntrld convec gain chg (W/hr)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Controlled convec gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.20)then
            IGETNO(NGET,3)=20   ! not avail for izver <4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlRadG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Controlled radiant gain (W)'
              LNSLABEL(NGET)=27
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Cntrld radiant gain chg (W/hr)'
              LNSLABEL(NGET)=30
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Controlled radiant gain (Whr)'
              LNSLABEL(NGET)=29
            endif
          elseif(inoc.eq.21)then

C Aggregate occupant (r+c) for all currently selected zones. 
            IGETNO(NGET,3)=21;  IGETNO(NGET,4)=2 ! signal aggregate
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' AgrOcup'; AGRLIST=.TRUE.   ! draw aggregate
            if(IROC.eq.0)then
              SLABEL(NGET)='Agr occupant gain (W)'
              LNSLABEL(NGET)=21
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Agr occupant gain (Whr)'
              LNSLABEL(NGET)=23
            endif
          elseif(inoc.eq.22)then

C Aggregate lighting (r+c) for all currently selected zones.
            IGETNO(NGET,3)=22;  IGETNO(NGET,4)=2 ! signal aggregate
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' AgrLtng'; AGRLIST=.TRUE.   ! draw aggregate
            if(IROC.eq.0)then
              SLABEL(NGET)='Agr lighting gain (W)'
              LNSLABEL(NGET)=21
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Agr lighting gain (Whr)'
              LNSLABEL(NGET)=23
            endif
          elseif(inoc.eq.23)then
            IGETNO(NGET,3)=23; IGETNO(NGET,4)=2 ! signal aggregate
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' AgrSmPw'; AGRLIST=.TRUE.   ! draw aggregate
            if(IROC.eq.0)then
              SLABEL(NGET)='Agr small pwr gain (W)'
              LNSLABEL(NGET)=22
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Agr small pwr gain (Whr)'
              LNSLABEL(NGET)=24
            endif
          elseif(inoc.eq.24)then
            IGETNO(NGET,3)=24; IGETNO(NGET,4)=2 ! signal aggregate
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' AgrLtPw'; AGRLIST=.TRUE.   ! draw aggregate
            if(IROC.eq.0)then
              SLABEL(NGET)='Agr light+small pwr (W)'
              LNSLABEL(NGET)=23
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Agr light+small pwr (Whr)'
              LNSLABEL(NGET)=25
            endif
          elseif(inoc.eq.25)then
            IGETNO(NGET,3)=25; IGETNO(NGET,4)=2 ! signal aggregate
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' Dispers'; AGRLIST=.TRUE.   ! draw aggregate
            if(IROC.eq.0)then
              SLABEL(NGET)='Dispersed demands (W)'
              LNSLABEL(NGET)=21
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Dispersed demands (Whr)'
              LNSLABEL(NGET)=23
            endif
          endif
        else
          helptopic='casual_convective_select'
          call gethelptext(helpinsub,helptopic,nbhelp)
          inoc=0
          idnoc=0
          call MENUATOL('Select a casual gain metric',' Choices:',
     &      'a All gains','b Convective portion','c Radiant portion',
     &      'd Total occupant gain','e Total lighting gain',
     &      'f Total small power gain','g Total controlled gain',
     &      'h Controlled fraction','  ','  ','  ','  ',inoc,idnoc,
     &      nbhelp)
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
              SLABEL(NGET)='Total casual gain (W)'; LNSLABEL(NGET)=21
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total casual gain change (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain (Whr)'
              LNSLABEL(NGET)=23
            endif
          elseif(inoc.eq.2)then
            IGETNO(NGET,3)=2
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=10
            SUFFIX=' CnvCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain convective (W)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gain conv chg (W/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Tot casual gain convective (Whr)'
              LNSLABEL(NGET)=32
            endif
          elseif(inoc.eq.3)then
            IGETNO(NGET,3)=3
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=11
            SUFFIX=' RadCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total casual gain radiant (W)'
              LNSLABEL(NGET)=28
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Tot casual gn radiant chg (W/hr)'
              LNSLABEL(NGET)=32
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total casual gain radiant (Whr)'
              LNSLABEL(NGET)=31
            endif
          elseif(inoc.eq.4)then
            IGETNO(NGET,3)=4
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=13
            SUFFIX=' OccCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total occupant gain (W)'
              LNSLABEL(NGET)=23
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total occupant gain chg (W/hr)'
              LNSLABEL(NGET)=30
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total occupant gain (Whr)'
              LNSLABEL(NGET)=25
            endif
          elseif(inoc.eq.5)then
            IGETNO(NGET,3)=5
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=14
            SUFFIX=' LitCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total lighting gain (W)'
              LNSLABEL(NGET)=23
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total lighting gain chg (W/hr)'
              LNSLABEL(NGET)=30
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total lighting gain (Whr)'
              LNSLABEL(NGET)=25
            endif
          elseif(inoc.eq.6)then
            IGETNO(NGET,3)=6
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=15
            SUFFIX=' EqpCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total small power gain (W)'
              LNSLABEL(NGET)=26
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total small power gain (W/hr)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total small power gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.7)then
            IGETNO(NGET,3)=7
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16
            SUFFIX=' CtlCasG'
            if(IROC.eq.0)then
              SLABEL(NGET)='Total controled casual gain (W)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Total ctld casual gain (W/hr)'
              LNSLABEL(NGET)=29
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Total ctld casual gain (Whr)'
              LNSLABEL(NGET)=28
            endif
          elseif(inoc.eq.8)then
            IGETNO(NGET,3)=8
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Frac of ctld casual gain (%)'
              LNSLABEL(NGET)=28
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Frac of ctld casual gain (%/hr)'
              LNSLABEL(NGET)=31
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Frac of ctld casual gain (%hr)'
              LNSLABEL(NGET)=30
            endif
          elseif(inoc.eq.9)then

C Test.
            IGETNO(NGET,3)=9
            IGETNO(NGET,6)=-3; IGETNO(NGET,7)=16; IGETNO(NGET,8)=4
            SUFFIX=' CtlCasF'
            if(IROC.eq.0)then
              SLABEL(NGET)='Various casual gains (%)'
              LNSLABEL(NGET)=24
            elseif(IROC.eq.1)then
              SLABEL(NGET)='Various casual gains (%/hr)'
              LNSLABEL(NGET)=27
            elseif(IROC.eq.-1)then
              SLABEL(NGET)='Various casual gain (%hr)'
              LNSLABEL(NGET)=25
            endif
          endif
        endif

      elseif(INO.EQ.15)then

C Electrical power flow.
C IGET = 27 Real power flow.
C IGET = 28 Reactive power flow.
        helptopic='casual_electrical_select'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Power flow type:','Real','Reactive',jpwr,
     &    nbhelp)

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
            LNSLABEL(NGET)=26
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Real power consump chg (W/hr)'
            LNSLABEL(NGET)=29
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Real power consumption (Whr)'
            LNSLABEL(NGET)=28
          endif
        elseif (jpwr.eq.2) then
          IGETNO(NGET,1)=28
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' ReacPwr'
          if(IROC.eq.0)then
            SLABEL(NGET)='Reactive power consumption (W)'
            LNSLABEL(NGET)=30
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Reactive power consmp chg (W/hr)'
            LNSLABEL(NGET)=32
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Reactive power consmption (Whr)'
            LNSLABEL(NGET)=31
          endif
        endif

      elseif(INO.EQ.17)then

C Renewables/special components output.
C IGET = 46 component main output.
C IGET = 47 component secondary output.
        helptopic='embedded_renew_select'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB(' ','Choose a component performance metric',
     &    'Heat/Power output','Misc. output',ispm,nbhelp)

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
            SLABEL(NGET)=' Output (W)'; LNSLABEL(NGET)=11
          elseif(IROC.eq.1)then
            SLABEL(NGET)=' Output chg (W/hr)'; LNSLABEL(NGET)=18
          elseif(IROC.eq.-1)then
            SLABEL(NGET)=' Output (Whr)'; LNSLABEL(NGET)=13
          endif
        elseif (ispm.eq.2) then
          IGETNO(NGET,8)=4
          IGETNO(NGET,1)=47
          IGETNO(NGET,6)=-4; IGETNO(NGET,7)=6
          SUFFIX=' Secndry'
          if(IROC.eq.0)then
            SLABEL(NGET)=' Misc. output (-)'; LNSLABEL(NGET)=17
          elseif(IROC.eq.1)then
            SLABEL(NGET)=' Misc. output chg (-/hr)'
            LNSLABEL(NGET)=24
          elseif(IROC.eq.-1)then
            SLABEL(NGET)=' Misc. output hours (-.hr)'
            LNSLABEL(NGET)=26
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
        helptopic='res_measured_data_select'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKAB('Which measured data type',
     &    ' :','zone db T','surface T',IR,nbhelp)
        NGET=NGET+1
        if(ir.eq.1)then
          call ASKZON(IZONE)
          IGETNO(NGET,1)=80
          IGETNO(NGET,2)=IZONE
          IGETNO(NGET,6)=-3; IGETNO(NGET,7)=6
          IGETNO(NGET,8)=1; IGETNO(NGET,9)=12
          SUFFIX=' db T'
          GLABEL(NGET)='Measured db Tmp'; LNGLABEL(NGET)=15
          if(IROC.eq.0)then
            SLABEL(NGET)='Measured zone db T (degC)'
            LNSLABEL(NGET)=25
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Meas zone db T change (degC/hr)'
            LNSLABEL(NGET)=31
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Meas zone db T hours (degC.hr)'
            LNSLABEL(NGET)=30
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
          GLABEL(NGET)='Measured surf T'; LNGLABEL(NGET)=15
          if(IROC.eq.0)then
            SLABEL(NGET)='Measured surf T (degC)'
            LNSLABEL(NGET)=22
          elseif(IROC.eq.1)then
            SLABEL(NGET)='Meas surf T change (degC/hr)'
            LNSLABEL(NGET)=28
          elseif(IROC.eq.-1)then
            SLABEL(NGET)='Meas surf T hours (degC.hr)'
            LNSLABEL(NGET)=27
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
        helptopic='data_vs_rate_of_change'
        call gethelptext(helpinsub,helptopic,nbhelp)

C The following lines do not allow casual access to the 'integrate data' 
C functionality.  The commented out lines just below activate this feature.
C        CALL EASKABC('Data display options:',outs,
C     &    'variable (default)','rate of change of variable','continue',
C     &    II,6)
C        if(II.ne.3.and.(IROC.ne.(II-1)))then 
C         IROC=II-1
        CALL EASKABCD('Data display options:',outs,'integrated',
     &    'variable (default)','rate of change of variable','continue',
     &    II,nbhelp)
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

C << revise for additional casual gain types >>
            write(ll1,'(1x,a6,1x)') lodlabel(NZNO(1),1)
            write(ll2,'(1x,a6,1x)') lodlabel(NZNO(1),2)
            write(ll3,'(1x,a6,1x)') lodlabel(NZNO(1),3)
            call edisp(iuout,' ')
            helptopic='which_casual_gain_type'
            call gethelptext(helpinsub,helptopic,nbhelp)
            IOT=1
            CALL EASKATOG('Which casual gain represents occupancy:',
     &         '  ','Always occupied',ll1,ll2,ll3,'time',' ',' ',
     &         IOT,nbhelp)
            if (IOT.gt.1.and.IOT.lt.5) then
              call usrmsg('  ','  ','-')
              iocupf=1
              iocut=IOT-1
              call scnocup(iier)
              IFILT=iocut
            elseif(IOT.eq.5)then
              iocupf=1
              iocut=-1
              call scnocup(iier)
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

C Set axis limits or the format of time in tabular lists.
        if(MENUTYPE.eq.3) then

C Set axis limits.
          helptopic='res_menu_3_axis_min_max'
          call gethelptext(helpinsub,helptopic,nbhelp)
          do 555 Iaxis=1,4
            if (YAXSET(Iaxis).gt.0) then
 501          write (values,'(8(f9.2,1x))') YMIN(Iaxis),YMAX(Iaxis)
              if (Iaxis.eq.1) then
                call EASKS(values,'  ','Min &max for temperature axis?',
     &            20,' -10 30 ','temp axis',IER,nbhelp)
              elseif (Iaxis.eq.2) then
                call EASKS(values,'  ','Min & max for loads axis?',
     &            20,' -100 100 ','temp axis',IER,nbhelp)
              elseif (Iaxis.eq.3) then
                call EASKS(values,'  ','Min & max for intensity axis?',
     &            20,' 0 1000 ','temp axis',IER,nbhelp)
              elseif (Iaxis.eq.4) then
                call EASKS(values,'  ','Min & max for misc axis?',
     &            20,' 0 100 ','temp axis',IER,nbhelp)
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

C Ask user preference for horizontal axis.
          CALL EASKABCD('Horizontal axis divisions:',' ','default',
     &      'hours','days','weeks',horaxisdiv,nbhelp)

C Redraw graph (if applicable).
          if (NGET.gt.0) then
            if (YAXSET(1).eq.2.or.YAXSET(2).eq.2.or.
     &          YAXSET(3).eq.2.or.YAXSET(4).eq.2) return
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
          helptopic='export_hash_separator'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB('Include mark between days when ',
     &      'displaying or writing data :','no','yes',IDH,nbhelp)
          IDHFLG=IDH-1
        endif

      elseif(INO.EQ.27)then

        if(MENUTYPE.eq.3) then

C Add another selection - time series plot only.
          INO=-4
          ADDLIN=1
        else

C Toggle delimeter.
          helptopic='tabular_delimiter_choice'
          call gethelptext(helpinsub,helptopic,nbhelp)
          IWM=1
          CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &      'normal spaces','single space','tab','comma','tagged',
     &      'continue',' ',IWM,nbhelp)
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

      elseif(INO.EQ.28)then

C Toggle flux units for all menu types.

C << if there is something already in the graph then we need
C << to modify the range in axis 2 (flux).

        helptopic='tabular_delimiter_choice'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IWM=0
        lastireportunit=ireportunit
        CALL EASKATOG('Units to use when reporting flux:',' ',
     &    'normal unit','W or Whr ','kW or kWhr','continue',' ',
     &    ' ',' ',IWM,nbhelp)
        if(iwm.eq.1)then
          ireportunit = 0
        elseif(iwm.eq.2)then
          ireportunit = 1
        elseif(iwm.eq.3)then
          ireportunit = 2
        endif
C Debug.
C        write(6,*) 'ireportunit ',ireportunit,lastireportunit,
C     &    YMAX(2),YMIN(2)

C If something drawn AND it included loads axis and the value
C of ireportunit changed then reset YMIN(2) and YMAX(2) units
C as well.
        if(ireportunit.ne.lastireportunit)then
          if (NGET.gt.0) then
            if (YAXSET(2).gt.0) then
              if((ireportunit.eq.0.or.ireportunit.eq.2).and.
     &            lastireportunit.eq.1)then
                YMAX(2)=YMAX(2)/1000.  ! W to kW
                YMIN(2)=YMIN(2)/1000.
              elseif((lastireportunit.eq.0.or.
     &                lastireportunit.eq.2).and.ireportunit.eq.1)then
                YMAX(2)=YMAX(2)*1000.  ! kW to W
                YMIN(2)=YMIN(2)*1000.
              endif
            endif
          endif
C Debug.
C          write(6,*) 'new ireportunit ',ireportunit,lastireportunit,
C     &      YMAX(2),YMIN(2)
        endif
      elseif(INO.eq.29.and.nl.ne.0) then

C Debug...
C        do 999 I=1,NGET
C          write(6,'(12i3)') (IGETNO(I,K),K=1,9)
C          write(6,*) LNSLABEL(I),' ',SLABEL(I)(1:lnslabel(I))
C          write(6,*) LNGLABEL(I),' ',GLABEL(I)(1:lnglabel(I))
C          write(6,*) LNTABLABEL(I),' ',TABLABEL(I)(1:lntablabel(I))
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
 290    helptopic='res_graph_title_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        t64=graftitle
        CALL EASKS(t64,' Title for graph ? ',
     &     ' ',64,'  ','graph title',IER,nbhelp)
        if(t64(1:2).ne.'  '.and.t64(1:4).ne.'UNKN')then
          graftitle=t64
        else
          goto 290
        endif

C Add or clear labels within a graph. First setup the help
C messages for the related topics.
        helptopic='res_graph_label_manage'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ngrlbl.eq.0)then
          CALL EASKAB('Labels within graph options:',' ',
     &    'edit/add labels','contine',igopt,nbhelp)
          if(igopt.eq.1)then
            igrl=0
            grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '; grlbl(4)=' '
            grlbl(5)=' '; grlbl(6)=' '; grlbl(7)=' '; grlbl(8)=' '
            grlbl(9)=' '; grlbl(10)=' '
          elseif(igopt.eq.2)then
            goto 1
          endif
        elseif(ngrlbl.gt.0)then
          CALL EASKABC('Labels within graph options:',' ',
     &    'clear existing labels','edit/add labels','contine',
     &    igopt,nbhelp)
          if(igopt.eq.1)then
            ngrlbl=0
            igrl=0
            grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '; grlbl(4)=' '
            grlbl(5)=' '; grlbl(6)=' '; grlbl(7)=' '; grlbl(8)=' '
            grlbl(9)=' '; grlbl(10)=' '
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
        elseif(ngrlbl.gt.9)then
          call usrmsg('Array of labels is full.',' ','W')
          goto 1
        else
          igrl=igrl+1
        endif
        t24=grlbl(igrl)
 292    CALL EASKS(t24,
     &   'Label within graph (AFTER editing select a point on graph',
     &   'for the label position).',24,'x','graph label',IER,nbhelp)
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
        CALL ASKOK('Include another user supplied label?',' ',OKL,dok,
     &    nbhelp)
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
      do loop=1,MCON
        IZNSEL(loop)=0
        ISFSEL(loop)=0
      end do
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
            helptopic='options_for_surf_select'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKAB('Options for surface selection:','  ',
     &        'one at a time','via anchor point list',IAC,nbhelp)
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
        elseif (AGRLIST) then

C If we have asked for an aggregate of heating or cooling or lighting
C or small power or lighitng+small power set the label.
          IGETNO(NGET,2)=0    ! no specific zone associated with aggregate
          ISLEN=lnblnk(SUFFIX)
          write (GLABEL(NGET),'(a)') SUFFIX(1:ISLEN)
          LNGLABEL(NGET)=ISLEN
          write (TABLABEL(NGET),'(a)') SUFFIX(1:ISLEN)
          LNTABLABEL(NGET)=ISLEN
          NSEL=NSEL+1  ! increment because it will be decremented later
        else

C Copy current zone selection.
          do I=1,NZ
            NSEL=NSEL+1
            IZNSEL(NSEL)=NZNO(I)
          end do
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
                    ITLEN=lnblnk(ZSDES)
                    IZLEN=lnblnk(ZSDESS)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.20) IZLEN=20-ISLEN
                    write (GLABEL(NGET),'(a,a)')
     &                ZSDESS(1:IZLEN),SUFFIX(1:ISLEN)
                    LNGLABEL(NGET)=IZLEN+ISLEN
                    write (TABLABEL(NGET),'(a,a)')
     &                ZSDES(1:ITLEN),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=ITLEN+ISLEN
                  elseif (MRTLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(CUBN(ISFSEL(IG)))
                    IZLEN=lnzname(IZ)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.16) IZLEN=16-ISLEN
                    write (GLABEL(NGET),'(3a)')ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN),SUFFIX(1:ISLEN)
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write (TABLABEL(NGET),'(3a)') ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=ITLEN+ISLEN
                  elseif (AGRLIST) then  ! aggregate does not need a zone name
                    ISLEN=lnblnk(SUFFIX)
                    write(GLABEL(NGET),'(2a)') 'All',SUFFIX
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(2a)') 'All ',SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  else
                    ISLEN=lnblnk(SUFFIX)
                    write(GLABEL(NGET),'(2a)')ZNAME(IZ)(1:lnzname(IZ)),
     &                SUFFIX
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(2a)')
     &                ZNAME(IZ)(1:lnzname(IZ)),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  endif
                else
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(ZSDES)
                    IZLEN=lnblnk(ZSDESC)
                    write (GLABEL(NGET),'(a)') ZSDESC(1:IZLEN)
                    LNGLABEL(NGET)=IZLEN
                    write (TABLABEL(NGET),'(a)') ZSDES(1:ITLEN)
                    LNTABLABEL(NGET)=ITLEN
                  elseif (MRTLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(CUBN(ISFSEL(IG)))
                    IZLEN=lnzname(IZ)
                    write (GLABEL(NGET),'(2a)')ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN)
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write (TABLABEL(NGET),'(2a)') ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN)
                    LNTABLABEL(NGET)=ITLEN
                  elseif (AGRLIST) then  ! aggregate does not need a zone name
                    ISLEN=lnblnk(SUFFIX)
                    write(GLABEL(NGET),'(2a)') 'All',SUFFIX
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(2a)') 'All ',SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  else
                    write(GLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
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
                LNSLABEL(NGET)=LNSLABEL(NGET-1)
                if (MENUTYPE.eq.3.or.MENUTYPE.eq.4) then
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(ZSDES)
                    IZLEN=lnblnk(ZSDESS)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.20) IZLEN=20-ISLEN
                    write (GLABEL(NGET),'(a,a)')
     &                ZSDESS(1:IZLEN),SUFFIX(1:ISLEN)
                    LNGLABEL(NGET)=IZLEN+ISLEN
                    write (TABLABEL(NGET),'(a,a)')
     &                ZSDES(1:ITLEN),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=ITLEN+ISLEN
                  elseif (MRTLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(CUBN(ISFSEL(IG)))
                    IZLEN=lnzname(IZ)
                    ISLEN=lnblnk(SUFFIX)
                    if ((IZLEN+ISLEN).gt.16) IZLEN=16-ISLEN
                    write (GLABEL(NGET),'(3a)')ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN),SUFFIX(1:ISLEN)
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write (TABLABEL(NGET),'(3a)') ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=ITLEN+ISLEN
                  elseif (AGRLIST) then  ! aggregate does not need a zone name
                    ISLEN=lnblnk(SUFFIX)
                    write(GLABEL(NGET),'(2a)') 'All',SUFFIX
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(2a)') 'All ',SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  else
                    ISLEN=lnblnk(SUFFIX)
                    write (GLABEL(NGET),'(a,a)')
     &                ZNAME(IZ)(1:lnzname(IZ)),SUFFIX(1:ISLEN)
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(2a)')
     &                ZNAME(IZ)(1:lnzname(IZ)),SUFFIX(1:ISLEN)
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  endif
                else
                  if (SLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(ZSDES)
                    IZLEN=lnblnk(ZSDESC)
                    write (GLABEL(NGET),'(a)') ZSDESC(1:IZLEN)
                    LNGLABEL(NGET)=IZLEN
                    write (TABLABEL(NGET),'(a)') ZSDES(1:ITLEN)
                    LNTABLABEL(NGET)=ITLEN
                  elseif (MRTLIST) then
                    call ZSID(IZ,ISFSEL(IG),ZSDES,ZSDESC,ZSDESS)
                    ITLEN=lnblnk(CUBN(ISFSEL(IG)))
                    IZLEN=lnzname(IZ)
                    write (GLABEL(NGET),'(2a)')ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN)
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write (TABLABEL(NGET),'(2a)') ZNAME(IZ)(1:IZLEN),
     &                        CUBN(ISFSEL(IG))(1:ITLEN)
                    LNTABLABEL(NGET)=ITLEN
C                  elseif (AGRLIST) then  ! aggregate does not need a zone name
C                    ISLEN=lnblnk(SUFFIX)
C                    write(GLABEL(NGET),'(2a)') 'All',SUFFIX
C                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
C                    write(TABLABEL(NGET),'(2a)') 'All ',SUFFIX(1:ISLEN)
C                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  else
                    write(GLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                    LNGLABEL(NGET)=lnblnk(GLABEL(NGET))
                    write(TABLABEL(NGET),'(a)') ZNAME(IZ)(1:lnzname(IZ))
                    LNTABLABEL(NGET)=lnblnk(TABLABEL(NGET))
                  endif
                endif
              endif
            endif
 650      continue
        else

C No data to copy therefore cancel request.  Note: if aggregate then it will
C already have had nget incremented one extra so that this subtraction will
C have no impact.
          NGET=NGET-1
        endif
      endif

C Reset logical variables.
      SLIST=.false.
      NLIST=.false.
      MRTLIST=.false.
      AGRLIST=.false.

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
                LNSLABEL(K)=LNSLABEL(K+1)
                GLABEL(K)=GLABEL(K+1)
                LNGLABEL(K)=LNGLABEL(K+1)
                TABLABEL(K)=TABLABEL(K+1)
                LNTABLABEL(K)=LNTABLABEL(K+1)
 920          continue
              IGETNO(NGET,1)=0; IGETNO(NGET,2)=0
              IGETNO(NGET,3)=0; IGETNO(NGET,4)=0
              IGETNO(NGET,5)=ISIM
              IGETNO(NGET,6)=1; IGETNO(NGET,7)=0
              IGETNO(NGET,8)=0; IGETNO(NGET,9)=0
              SLABEL(NGET)='  '
              LNSLABEL(NGET)=0
              GLABEL(NGET)='  '
              LNGLABEL(NGET)=0
              TABLABEL(NGET)='  '
              LNTABLABEL(NGET)=0
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
C          write(6,'(12i3)') (IGETNO(I,K),K=1,9)
C          write(6,*) LNSLABEL(I),' ',SLABEL(I)(1:lnslabel(I))
C          write(6,*) LNGLABEL(I),' ',GLABEL(I)(1:lnglabel(I))
C          write(6,*) LNTABLABEL(I),' ',TABLABEL(I)(1:lntablabel(I))
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

      integer ireportunit ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit

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
      do KGET=1,NGET
        call GOGETIT(KGET,IDAY,ISTART,IEND)
      end do

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
#include "schedule.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever

      COMMON/RECV5/SNA(MCOM,MS)
      common/PREC18/ZTRANA(MCOM)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      COMMON/CCTL/ICASCF(MCOM)
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      common/getmenu/menutype,igetind(65),igetflux(65)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      integer ireportunit ! zero is default one is W two is kW three Joules
      common/repunit/ireportunit
      
C      character outs*124
      character CSTR*25
      dimension CQ(MTS),GC(MTS),AGR(MTS)

      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.
      integer iset    ! IGETNO(*,5)
      logical dmdsok,xst

C Check existance of demands file (it will have been scanned elsewhere).
      call FINDFIL(bdmds,XST)
      if(XST)then
        dmdsok=.true.
      else
        dmdsok=.false.
      endif

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

C IGETNO(KGET,1) =  8 Sensible heating load and convert to KW
C taking into account ireportunit. If individual items use the
C following logic.
        if(IGETNO(KGET,4).eq.0.or.IGETNO(KGET,4).eq.1)then
          CALL GZQMH(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          N=24*NTS
          if(ireportunit.eq.0.or.ireportunit.eq.2)then
            do I=1,N
              GVAL(I)=GVAL(I)/1000. ! selective conversion
            end do
          endif
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        else

C For each of the currently selected zones call GZQMH and add to
C the aggregate figure.
          N=24*NTS
          do i=1,N
            AGR(i)=0.0  ! clear the AGR array
          end do
          do 60 izz=1,NZ
            CALL GZQMH(IDAY,NZNO(izz),IGETNO(KGET,5))
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              do I=1,N
                AGR(I)=AGR(I)+(GVAL(I)/1000.0) ! selective conversion and sum
              end do
            else
              do I=1,N
                AGR(I)=AGR(I)+GVAL(I) ! sum to AGR
              end do
            endif
  60      continue
          CALL XTVAL1(AGR,ISTART,IEND,KGET)
        endif

      elseif(IGETNO(KGET,1).eq.9)then

C IGETNO(KGET,1) =  9 Sensible cooling load and convert to KW
C taking into account ireportunit. If individual items use the
C following logic.
        if(IGETNO(KGET,4).eq.0.or.IGETNO(KGET,4).eq.1)then
          CALL GZQMC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          N=24*NTS
          if(ireportunit.eq.0.or.ireportunit.eq.2)then
            do I=1,N
              GVAL(I)=GVAL(I)/1000. ! selective conversion
            end do
          endif
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        else

C For each of the currently selected zones call GZQMC and add to
C the aggregate figure.
          N=24*NTS
          do i=1,N
            AGR(i)=0.0  ! clear the AGR array
          end do
          do 65 izz=1,NZ
            CALL GZQMC(IDAY,NZNO(izz),IGETNO(KGET,5))
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              do I=1,N
                AGR(I)=AGR(I)+(GVAL(I)/1000.0) ! selective conversion and sum
              end do
            else
              do I=1,N
                AGR(I)=AGR(I)+GVAL(I) ! sum to AGR
              end do
            endif
  65      continue
          CALL XTVAL1(AGR,ISTART,IEND,KGET)
        endif

      elseif(IGETNO(KGET,1).eq.10)then

C IGETNO(KGET,1) = 10 Total plant flux and convert to KW
C taking into account ireportunit.
        CALL GZQM(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=24*NTS
        if(ireportunit.eq.0.or.ireportunit.eq.2)then
          do I=1,N
            GVAL(I)=GVAL(I)/1000. ! selective conversion
          end do
        endif
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

C IGETNO(KGET,1) = 15 Casual gains. Total, convective, radiant or by type.
C IGETNO(KGET,3) 1=all c+r, 2=all conv, 3=all rad, 4=ocup, 5=light, 6=sm pwr, 7=ctld
C (also check if aggregate occupant, lighting or small power or lighitng+small
C power requested.
        iicrcg=IGETNO(KGET,3)
        if(izver.ge.4)then

C If izver >= 4 the read the data directly for individual items.
          IZ=IGETNO(KGET,2)
          ISET=IGETNO(KGET,5)
          if(IGETNO(KGET,4).eq.0.or.IGETNO(KGET,4).eq.1)then
            DO J2=ISTART,IEND
              call getallcas(IDAY,IZ,ISET,J2,QCASR,QCASC,QCASL,FRAC,
     &          perocupc,perlightc,perequipc,otherc,perocupr,
     &          perlightr,perequipr,otherr,perocupl,perlightl,
     &          perequipl,otherl,theonectld)
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
              endif
            ENDDO  ! of J2 loop
            CALL XTVAL1(GC,ISTART,IEND,KGET)
          elseif(IGETNO(KGET,4).eq.2)then

C For each of the currently selected zones call getallcas and add to
C aggregate figure for occupant, lights, small power or lights+smallpower.
            N=24*NTS
            do i=1,N
              AGR(i)=0.0  ! clear the AGR array
            end do
            if(IICRCG.ge.21.and.IICRCG.le.24)then
              do izz=1,NZ
                DO J2=ISTART,IEND
                  call getallcas(IDAY,NZNO(izz),ISET,J2,QCASR,QCASC,
     &              QCASL,FRAC,
     &              perocupc,perlightc,perequipc,otherc,perocupr,
     &              perlightr,perequipr,otherr,perocupl,perlightl,
     &              perequipl,otherl,theonectld)
                  if(IICRCG.eq.21)then
                    AGR(J2)=AGR(J2)+perocupc+perocupr ! occupant convec+r gain (W)
                  elseif(IICRCG.eq.22)then
                    AGR(J2)=AGR(J2)+perlightc+perlightr ! Lighting convec+r gain (W)
                  elseif(IICRCG.eq.23)then
                    AGR(J2)=AGR(J2)+perequipc+perequipr ! sm power convec+r gain (W)
                  elseif(IICRCG.eq.24)then
                    AGR(J2)=AGR(J2)+
     &                perlightc+perlightr+perequipc+perequipr ! lights+small power gain (W)
                  endif
                enddo  ! of j2 loop
              enddo    ! of izz loop
              CALL XTVAL1(AGR,ISTART,IEND,KGET)
            elseif(IICRCG.eq.25)then

C If dispersed demands (via VAL3 arrays) sum them to VAL(14).
C As dispersed demands are for whole model in the reporting
C for each zone factor it by the base area of that zone.
              if(dmdsok)call DDMDS(IDAY)
              DO J2=ISTART,IEND
                if(dmdsok)then
                  AGR(J2)=VAL3(15,J2)+VAL3(16,J2)+VAL3(17,J2)+
     &              VAL3(18,J2)+VAL3(19,J2)+VAL3(20,J2)+VAL3(21,J2)
                else
                  AGR(J2)=0.0
                endif
              ENDDO  ! of j2 loop
              CALL XTVAL1(AGR,ISTART,IEND,KGET)
            endif
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
            do JTS=ISTART,IEND
              CQ(JTS)=0.0
            end do
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
          
C Read in the zone geometry, construction data.
C << Work towards not reading zone files >>
        IUNIT=IFIL+1
          call georead(IUNIT,LGEOM(IZ),IZ,0,IUOUT,IER)

        CALL ECONST(LTHRM(IZ),IUNIT,IZ,0,IUOUT,IER)

C Set default surface absorbtivity.
        absorb=1.0
        absorb=ABSE(IS)

C Radiant plant is area AND emissivity weighted.
        emarea=0.0
        do kk=1,NZSUR(iz)
          emarea=emarea+sna(iz,kk)*EMISI(kk)
        end do
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
          DO JTS=ISTART,IEND
            GVAL(JTS)=GVAL(JTS)/SAREA
          END DO

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
              DO I2=ISTART,IEND
                 XDUM(I2)=(xfac)*GVAL(I2)
     &                    *(1.-(FLOAT(IBAN(ICF,3))*0.01))
              END DO
              CALL XTVAL1(XDUM,ISTART,IEND,KGET)
            ELSE
              DO I2=ISTART,IEND
                XDUM(I2)=0.
              END DO
              CALL XTVAL1(XDUM,ISTART,IEND,KGET)
            ENDIF
          ELSE
            DO I2=ISTART,IEND
              XDUM(I2)=0.
            END DO
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
        DO J2=ISTART,IEND
          GC(J2)=GVAL(J2)-VAL3(2,J2)
        END DO
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.30)then

C IGETNO(KGET,1) = 30 Latent load (dehumidification) and convert to KW
C taking into account ireportunit. If individual items use the
C following logic.
        if(IGETNO(KGET,4).eq.0.or.IGETNO(KGET,4).eq.1)then
          CALL GZLLC(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          N=24*NTS
          if(ireportunit.eq.0.or.ireportunit.eq.2)then
            do I=1,N
              GVAL(I)=GVAL(I)/1000. ! selective conversion
            end do
          endif
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        else

C For each of the currently selected zones call GZLLC and add to
C the aggregate figure.
          N=24*NTS
          do i=1,N
            AGR(i)=0.0  ! clear the AGR array
          end do
          do 66 izz=1,NZ
            CALL GZLLC(IDAY,NZNO(izz),IGETNO(KGET,5))
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              do I=1,N
                AGR(I)=AGR(I)+(GVAL(I)/1000.0) ! selective conversion and sum
              end do
            else
              do I=1,N
                AGR(I)=AGR(I)+GVAL(I) ! sum to AGR
              end do
            endif
  66      continue
          CALL XTVAL1(AGR,ISTART,IEND,KGET)
        endif

      elseif(IGETNO(KGET,1).eq.31)then

C IGETNO(KGET,1) = 31 Latent load (humidification) and convert to KW
C taking into account ireportunit. If individual items use the
C following logic.
        if(IGETNO(KGET,4).eq.0.or.IGETNO(KGET,4).eq.1)then
          CALL GZLLH(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
          N=34*NTS
          if(ireportunit.eq.0.or.ireportunit.eq.2)then
            do I=1,N
              GVAL(I)=GVAL(I)/1000. ! selective conversion
            end do
          endif
          CALL XTVAL1(GVAL,ISTART,IEND,KGET)
        else

C For each of the currently selected zones call GZLLH and add to
C the aggregate figure.
          N=24*NTS
          do i=1,N
            AGR(i)=0.0  ! clear the AGR array
          end do
          do 67 izz=1,NZ
          CALL GZLLH(IDAY,NZNO(izz),IGETNO(KGET,5))
            if(ireportunit.eq.0.or.ireportunit.eq.2)then
              do I=1,N
                AGR(I)=AGR(I)+(GVAL(I)/1000.0) ! selective conversion and sum
              end do
            else
              do I=1,N
                AGR(I)=AGR(I)+GVAL(I) ! sum to AGR
              end do
            endif
  67      continue
          CALL XTVAL1(AGR,ISTART,IEND,KGET)
        endif

      elseif(IGETNO(KGET,1).eq.32)then

C IGETNO(KGET,1) = 32 All latent load and convert to KW
C taking into account ireportunit.
        CALL GZLL(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=35*NTS
        if(ireportunit.eq.0.or.ireportunit.eq.2)then
          do I=1,N
            GVAL(I)=GVAL(I)/1000. ! selective conversion
          end do
        endif
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.33)then

C IGETNO(KGET,1) = 33 Sensible plus latent load and convert to KW 
C taking into account ireportunit.
        CALL GZQSL(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        N=36*NTS
        if(ireportunit.eq.0.or.ireportunit.eq.2)then
          do I=1,N
            GVAL(I)=GVAL(I)/1000. ! selective conversion
          end do
        endif
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.34)then

C IGETNO(KGET,1) = 34 Wind direction (climate parameter)
        CALL MOCLIM (IDAY,27)
        CALL XTVAL1(GVAL,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.35)then

C IGETNO(KGET,1) = 35 Delta T surf (use CQ) - dewpoint.
        CALL GTMS(IDAY,IGETNO(KGET,2),IGETNO(KGET,3),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
        END DO
        CALL GZDP(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          GC(J2)=CQ(J2)-GVAL(J2)
        END DO
        CALL XTVAL1(GC,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.36)then

C IGET = 36 Total conv @ interior/similar/adiab/constant surfaces.
C Add opaque and transparent at each timestep.
        CALL GOPQIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
        END DO
        CALL GTRNIN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          CQ(J2)=CQ(J2)+GVAL(J2)
        END DO
        CALL XTVAL1(CQ,ISTART,IEND,KGET)
      elseif(IGETNO(KGET,1).eq.37)then

C IGET = 37 Total conv @ outside connected surfaces. Add opaque and
C transparent at each timestep.
        CALL GOUTOPQ(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
        END DO
        CALL GOUTTRN(IDAY,IGETNO(KGET,2),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          CQ(J2)=CQ(J2)+GVAL(J2)
        END DO
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
        DO JTS=ISTART,IEND
          GVAL(JTS)=GVAL(JTS)/SAREA
        END DO
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
        DO J2=ISTART,IEND
          CQ(J2)=GVAL(J2)
        END DO
        CALL GZAIRT(IDAY,IGETNO(KGET,3),IGETNO(KGET,5))
        DO J2=ISTART,IEND
          GC(J2)=CQ(J2)-GVAL(J2)
        END DO
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
        DO J2=ISTART,IEND
          iflag=1
          dT=0.
          call DISCOMF(iflag,dT,PPD)
          if (iflag.eq.-1) PPD=101.
          GC(J2)=PPD
        END DO
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

C ******************** GETSTATS ********************
C GETSTATS is an efficient routine to scan the zone results records for save
C levels 2, 3 and 4 finding the max, min and average values and times of 
C max and min occurances as offsets from the output period start day in 
C hours for one topic (defined by KGET). It uses a combination of logic
C from MMAPER (the genereal scan to memory), and the row and column
C offsets from the low level subroutines in rlibget.F such as GTMS
C calls to GFIXED. It is a more efficient implementation (less overhead
C less passing of information) which recovers statistical data on
C a given topic. Getstats is called by tgraph.F frqdis.F and stats.F.
C If the stats can be generated for a topic HAVEIT is set to true. If
C GETSTATS cannot deal with a topic HAVEIT is set to false and MMACAL
C will be called.

      SUBROUTINE GETSTATS(KGET,HAVEIT,DMAX,DMIN,DAVE,DTOT,TMAX,TMIN)
#include "building.h"
#include "geometry.h"

C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/GETPIK/NGET,IGETNO(MZS,9)
      COMMON/IGETFLG/IOCUPF,ialstused,IROC

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/HASH/IHASHT(MTS),IHASHZ(MCOM),IHASHD(365),IHASHS(MNRS)

      dimension DATA(MZRL),XD(MZRL)
      character outs*124
      integer metric ! based on IGETNO(KGET,1)
      integer izone  ! based on IGETNO(KGET,2)
      integer isurf  ! based on IGETNO(KGET,3)
      integer inode  ! based on IGETNO(KGET,5)
      integer iaggr  ! based on IGETNO(KGET,4)
      logical haveit ! set true if we can do it

C Set unit number.
      IUNIT=IFIL

C Clear returned values.
      DMAX=-1.E+10; DMIN=1.E+10; DAVE=0.; DTOT=0.
      TMAX=0.; TMIN=0.
      IRJUMP = -1; ICJUMP = -1  ! offset state until specifically set

C TSTART and TFINISH - start and finish times in hours from 0000 on the 
C first day of output.
      TSTART=FLOAT(IOH1)
      TFINSH=FLOAT(((IODF)*24+IOH2)-(IODS)*24)

C NDTS - the number of timesteps in a day.
      NDTS=24*NTS

C In general only save 2 3 4 handled here.
      if(ISAVE.GT.1.AND.ISAVE.LE.4)then
        continue
      else
        haveit=.false.
        return
      endif

C If calculating a rate of change or integrating then use MMACAL.
      if (IROC.ne.0) then
        haveit=.false.
        return
      endif


C Depending on the metric set the column and row offsets for the
C read.  If we can no deal with it continue and trap return if
C ICJUMP is still -1.
C << It may be possible to extend the logic to deal with more
C << topics in the future.
      metric=IGETNO(KGET,1)  ! remember the topic
      izone =IGETNO(KGET,2)  ! remember the zone
      isurf =IGETNO(KGET,3)  ! remember the surface (if applicable)
      iaggr =IGETNO(KGET,4)  ! remember is separate one or aggregate two
      inode =IGETNO(KGET,5)  ! remember the node (if applicable)
      if(metric.eq.1)then
        IRJUMP = 0; ICJUMP = 1  ! zone db temperature
      elseif(metric.eq.2)then
        continue                ! ambient temperature
      elseif(metric.eq.3)then
        IRJUMP = 0              ! Control point temperature GTMCON
        if(ISAVE.GE.2.AND.ISAVE.LE.4) ICJUMP=NZSUR(IZONE)+5
      elseif(metric.eq.4)then 
        if(ISAVE.EQ.3.OR.ISAVE.EQ.4)then
          IRJUMP = 0; ICJUMP = isurf+2 ! internal surface temperature
        endif
      elseif(metric.eq.5)then
        if(ISAVE.EQ.3.OR.ISAVE.EQ.4)then  ! other side surface temp
          IRJUMP = 3+(isurf-1); ICJUMP = 1
        endif
      elseif(metric.eq.6)then
        continue              !  resultant temperature
      elseif(metric.eq.7)then
        if(ISAVE.EQ.3)then    !  node temperature (MLC)
          IRJUMP = 3+(isurf-1); ICJUMP = inode
        endif
      elseif(metric.eq.8)then
        if(iaggr.eq.1)then
          IRJUMP = 0; ICJUMP=2  ! sensible heating load (non aggregate)
        endif
      elseif(metric.eq.9)then
        if(iaggr.eq.1)then
          IRJUMP = 0; ICJUMP=2  ! sensible cooling load (non aggregate)
        endif
      elseif(metric.eq.10)then
        IRJUMP = 0; ICJUMP=2  ! total plant flux
      elseif(metric.eq.11)then
        IRJUMP = 0; ICJUMP = NZSUR(IZONE)+3 ! infiltration
      elseif(metric.eq.12)then
        IRJUMP = 0; ICJUMP = NZSUR(IZONE)+4 ! ventilation
      elseif(metric.eq.13)then
        IRJUMP = 2; ICJUMP = NZSUR(IZONE)+3 ! zone RH (%)
      elseif(metric.eq.14)then
        continue                ! zone dew point
      elseif(metric.eq.15)then
        continue                ! casual gains
      elseif(metric.eq.16)then
        continue                ! surface convection
      elseif(metric.eq.17)then
        IRJUMP = 2;  ICJUMP=isurf   ! solar absorbed on external surfaces
      elseif(metric.eq.18)then
        IRJUMP = 1; ICJUMP = isurf+5 ! solar absorbed on internal surfaces
      elseif(metric.eq.19.or.metric.eq.20.or.metric.eq.21.or.
     &       metric.eq.22)then
        continue   ! direct solar diffuse solar window speed ambient rh 
      elseif(metric.eq.23)then
        continue   ! comfort
      elseif(metric.eq.24)then
        continue   ! mean radiant temperature
      elseif(metric.eq.25)then
        continue   ! surface flux 
      elseif(metric.eq.26)then
        continue   ! external lux level
      elseif(metric.eq.27)then
        IRJUMP = 1; ICJUMP = 3  ! real electrical power consumption
      elseif(metric.eq.28)then
        IRJUMP = 1; ICJUMP = 4  ! reactive electrical power consumption
      elseif(metric.eq.29)then
        continue  ! delta T zone - ambient
      elseif(metric.eq.30)then  ! latent load (non aggregage dehumidification)
        if(iaggr.eq.1)then
          IRJUMP = 2; ICJUMP = NZSUR(IZONE)+4
        endif
      elseif(metric.eq.31)then  ! latent load (non aggregate humidification)
        if(iaggr.eq.1)then
          IRJUMP = 2; ICJUMP = NZSUR(IZONE)+4
        endif
      elseif(metric.eq.32)then  ! all latent load and convert to KW
        IRJUMP = 2; ICJUMP = NZSUR(IZONE)+4
      elseif(metric.eq.33)then
        continue  ! sensible plus latent load
      elseif(metric.eq.34)then
        continue  ! wind direction
      elseif(metric.eq.35)then
        continue  ! delta T surf - dewpoint
      elseif(metric.eq.36)then
        continue  ! total conv @ non-exterior surface
      elseif(metric.eq.37)then
        continue  ! total conv @ outside
      elseif(metric.eq.38)then
        continue  ! solar entering zone from outside 
      elseif(metric.eq.39)then
        continue  ! solar entering zone from adj zones
      elseif(metric.eq.40)then
        continue  ! solar absorbed in zone
      elseif(metric.eq.41)then
        continue  ! db T - another zone db T
      elseif(metric.eq.42.or.metric.eq.43.or.metric.eq.44.or.
     &       metric.eq.45)then
        continue  ! zone balance 
      elseif(metric.eq.46)then
        continue  ! renewables/advanced materials primary output
      elseif(metric.eq.47)then
        continue  ! renewables/advanced materials secondary output.
      elseif(metric.eq.48)then
        continue
      elseif(metric.eq.49)then
        continue
      elseif(metric.eq.50)then
        continue  ! local PPD: delta T head/ feet
      elseif(metric.eq.51)then
        continue  ! local PPD: floor T
      elseif(metric.eq.52)then
        continue  ! local PD: ceiling T
      elseif(metric.eq.53)then
        continue  ! local PD: assymetric dTrad
      elseif(metric.eq.80)then
        continue  ! zone measured db temperature
      elseif(metric.eq.81)then
        continue  ! surface measured db temperature
      else
      endif

      if(ICJUMP.eq.-1.or.ICJUMP.eq.0)then
        haveit=.false.
        return  ! we can not deal with it here
      else
        haveit=.true.
      endif
      
C For current zone loop days then minutes and average if required.
C ICJUMP - number of zone data to read in the record to get to the
C item we want.
      NZI = NZSUR(IZONE)+12  ! width of actual data in 1st 3 records
      NITMS=0
      do 200 IDAY=IODS,IODF
        do 300 ITS=1,NDTS,NOUT

C Compute current time.
C IHRD - number of days since start of plotting period in hours.
C TIME - time in hours since start of first day plotted.
C Add half a timestep to TIME if data averaged.
          IHRD=(IDAY-IODS)*24
          call DATIME(ITS,ATIME)
          TIME=float(IHRD)+ATIME

C Within requested output period.
          IF(TIME.LT.(TSTART-1.0).or.TIME.GT.TFINSH)goto 300

C Depending on the requested data type read the relevant data
C fields (the minimum required).
C Read first record and set values, make up potential error messages.
          IREC=IHASHD(IDAY)+IHASHT(ITS)+IHASHZ(IZONE)+IHASHS(ISIM)+
     &         IRJUMP
          if(irec.eq.0) goto 999

          READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=99)(DATA(K),K=1,ICJUMP)

C Average over required time steps. E.g if there are 2 tsph then read
C the subsequent records for the hour into XD and sum into DATA.
          if (NOUT.gt.1) then
            do 400 IV=2,NOUT
              J=ITS+IV-1
              IREC=IHASHD(IDAY)+IHASHT(J)+IHASHZ(IZONE)+IHASHS(ISIM)+
     &             IRJUMP
              if(irec.eq.0) goto 999

C Read up to ICJUMP and sum data at icjumpet determined.
              READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=98)(XD(K),K=1,ICJUMP)
              DATA(ICJUMP)=DATA(ICJUMP)+XD(ICJUMP)
 400        CONTINUE
          endif

C If this is an item that needs to be filtered for positive values do it here.
          if(metric.eq.8.or.metric.eq.31)then
            if(DATA(ICJUMP).lt.0.) DATA(ICJUMP)=0.0
          endif

C If this item need to be filtered for negative values do it here.
          if(metric.eq.9.or.metric.eq.30)then
            if(DATA(ICJUMP).gt.0.) DATA(ICJUMP)=0.0
          endif

C Average filtered and non filtered data and compare to current
C max/min values. If there is no occupancy filter is there any
C nneed to call getocup??
          ih=int(ATIME+1.)
          ioc=1
          if(iocupf.eq.1) call getocup(IZONE,IDAY,ih,ioc,ier)
          if (NOUT.gt.1) DATA(ICJUMP)=DATA(ICJUMP)/float(NOUT)
          if(ioc.ne.0) then
            if (DATA(ICJUMP).gt.DMAX) then
              DMAX=DATA(ICJUMP)
              TMAX=TIME
            endif
            if (DATA(ICJUMP).lt.DMIN) then
              DMIN=DATA(ICJUMP)
              TMIN=TIME
            endif
            DAVE=DAVE+DATA(ICJUMP)
            NITMS=NITMS+1    ! number of timesteps
          endif
 300    CONTINUE
 200  CONTINUE
 
C Remember totals.
      DTOT=DAVE

C Calculate average for period.
      xNITMS=float(NITMS)
      DAVE=DAVE/xNITMS
 
C If items needs to be converted into KW do this here.

C << ?? do we need to pay attention to ireportunit ?? >>

      if(metric.eq.8.or.metric.eq.9.or.metric.eq.10.or.metric.eq.32)then
        DMAX=DMAX/1000.; DMIN=DMIN/1000.; DAVE=DAVE/1000.
        DTOT=DTOT/1000.
      endif

      return

  99  if(IOS.eq.2)then
        write(outs,'(a,4i6)') 
     &    'getstats: no permission to scan data @ rec ',
     &    IREC,NZI,IZONE,ISIM
      else
        write(outs,'(a,4i6)') 'getstats: scanning data @ rec ',
     &    IREC,NZI,IZONE,ISIM
      endif
      call usrmsg(outs,' cannot continue results recovery.','F')
      return

  98  if(IOS.eq.2)then
        write(outs,'(a,4i6)') 
     &    'getstats: no permission to scan xd @ rec ',
     &    IREC,NZI,IZONE,ISIM
      else
        write(outs,'(a,4i6)') 'getstats: scanning xd @ rec ',
     &    IREC,NZI,IZONE,ISIM
      endif
      call usrmsg(outs,' cannot continue results recovery.','F')
      return

 999  write(outs,'(a,4i6)') 
     & 'getstats: record out of range (day? zone?) scanning xd @ rec ',
     &  IREC,IDAY,IZONE,ISIM
      call usrmsg(outs,' cannot continue results recovery.','W')
      return
      end

C ******************** MMACAL ********************
C MMACAL calculates the data for topics not processed via GETSTATS.
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

C For each day in the period recover data to topic kget
C and then for each timestep recover data in array XVAL.
      do 10 IDAY=IODS,IODF
        call GOGETIT(KGET,IDAY,1,NDTS)
        DO 421 J = 1,NDTS,NOUT
          call VALIAV(KGET,J,XVAL)

C Check for invalid use of comfort algorithms.
          if (IGETNO(KGET,1).ge.50.AND.IGETNO(KGET,1).le.59) then
            if (XVAL(KGET).gt.100.0) goto 421
          endif

C Calculate rate of change if IROC flag set to +1 and if
C IROC is -1 then integrate the data.
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

