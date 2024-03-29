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


C EBRSTAB: searches the item defs for tabular generic choices.
C GENDAT:  holds cases of generic data.

C ************* EBRSTAB 
C EBRSITM searches the item definition and produces a menu of temporal 
C type choices available and returns its index as IFG.  It uses a call
C to subroutine gendat to populate the TDFGH2 common blocks.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EBRSTAB(IFG,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
#include "help.h"

      COMMON/TDFGH1/NITEMS
      COMMON/TDFGH2/GTYPES(MTI),GMENUS(MTI)
      DIMENSION ISGGT(MTI+4),IVAL(MTI+4),IVALSS(MTI)
      LOGICAL FOUND
      CHARACTER GMENUS*24,GTYPES*8
      CHARACTER KEY*1,ISGGT*36,FOCUS*8
      integer nitms,INO ! max items and current menu item

      helpinsub='item'  ! set for subroutine
      helptopic='scan_temporal_items'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Read the generic descriptions to see what is available.
      IER=0
      FOCUS='ALL'
      FOUND=.false.
      CALL GENDAT(FOCUS,0,FOUND,IER)
C      M=1     ! for the title of the list
C      DO 3 IT=1,NITEMS
C        M=M+1
C        CALL EMKEY(M,KEY,IER)
C        IVAL(M)=IT   ! remember mapping between item and menu position
C        WRITE(ISGGT(M),72)KEY,GMENUS(IT),GTYPES(IT)
C   72   FORMAT(A1,1X,A,1x,A)
C    3 CONTINUE
C      ISGGT(1)  ='  Description       TAG     '
C      ISGGT(M+1)='? Help                      '
C      ISGGT(M+2)='- Exit                      '
      DO 3 IT=1,NITEMS
        WRITE(ISGGT(IT),'(3a)')GMENUS(IT),' ',GTYPES(IT)
    3 CONTINUE

C Make a menu of available generic items.
      INPICK=1
      CALL EPICKS(INPICK,IVALSS,'Available temporal types',
     &  'Description       TAG',32,NITEMS,ISGGT,
     &  'types',IER,nbhelp)
      INO=IVALSS(1)
      IFG=IVALSS(1)

      RETURN
      END

C ************* GENDAT 
C GENDAT holds data on generic items to place into common blocks 
C TDFG1 TDFG2 TDFG3 TDFG5.
C ITRU is the unit number for user output, IER=0 OK, IER=1 problem.
C FOCUS is the generic type string, IFOC is the instance array.
C In the case of FOCUS='ALL' IFOC is not used. 
      SUBROUTINE GENDAT(FOCUS,IFOC,FOUND,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

C Related to generic items. See __ for details.
C  GTYPES(): 8 char generic type tag for type of processing and info.
C  GMENUS(): 24 char descriptive text for use in selection menus.
      COMMON/TDFGH1/NITEMS
      COMMON/TDFGH2/GTYPES(MTI),GMENUS(MTI)

C Related to generic ITEMS.
C  GAIDE: 40 char description of the generic item.
C  GHELP(): Help text.
C  GDHDRT: 72 char string array, first two used for heading of item 
C     data display and the third used as prompt in the case of all
C     numerical editing of timestep data.
C  GMH: 32 char string with misc data:
C       NGMAT: No of text attributes used.
C       NGMAR : No of real attributes used.
C       NHELP: No of help lines associated with this type.
C       NBITS : No data bits in a period ie. time, data, data.
      COMMON/TDFG1/GAIDE,GHELP(MHEL),GDHDRT(3),GMH

C  MGMA(): 32 char menu/prompt text for each field.
C  IALT:   Number of text alternates for each field (used only if
C          field is text).
C Example GMH string with miscellaneous definitions included.
C         NHELP,NBITS,NSTATIC,NGMAT,NGMAR,NGMAI,GERR
C       GMH='2,1,0,0,1,0,-999.0'

      COMMON/TDFG5/GMF(MBITS),MGMA(MBITS)
      COMMON/TDFG6/IALT(MBITS)

      CHARACTER GMENUS*24,GTYPES*8
      CHARACTER GAIDE*36,GHELP*72,GDHDRT*72,GMH*32
      CHARACTER GMF*32,MGMA*32
      CHARACTER FOCUS*8,WORD*20
      LOGICAL FOUND

C Pick up the arrays used for displaying current generic items.
      NITEMS=MTI
      GTYPES(1) ='ALLCLMT'
      GMENUS(1) ='Climate data (all six):'
      GTYPES(2) ='DBTEXTT'
      GMENUS(2) ='External Dry Bulb T:   '
      GTYPES(3) ='WINDVLT'
      GMENUS(3) ='Wind velocity :        '
      GTYPES(4) ='WINDDRT'
      GMENUS(4) ='Wind direction:        '
      GTYPES(5) ='RELHUMT'
      GMENUS(5) ='External rel humidity :'
      GTYPES(6) ='DIFHSLT'
      GMENUS(6) ='Diffuse horiz solar:   '
      GTYPES(7) ='GLOHSLT'
      GMENUS(7) ='Global horiz solar:    '
      GTYPES(8) ='DIRSOLT'
      GMENUS(8) ='Direct normal solar:   '
      GTYPES(9) ='VERTSOL'
      GMENUS(9) ='Vertical solar @ surf: '     
      GTYPES(10)='CASUALT'
      GMENUS(10)='Casual sum of OccLtEq: '
      GTYPES(11)='CASUAL3T'
      GMENUS(11)='Casual separate OccLtEq'
      GTYPES(12)='ZIVENTT'
      GMENUS(12)='Zone infil & vent rate:'
      GTYPES(13)='RAIRVLT'
      GMENUS(13)='Room air velocity (NA):'  ! not yet implemented
      GTYPES(14)='SETPTTT'
      GMENUS(14)='Control set point T:   '
      GTYPES(15)='HTCLSETP'
      GMENUS(15)='Heat & Cool set points:'
      GTYPES(16)='SKYTMPT'
      GMENUS(16)='Sky temp (degC):       '
      GTYPES(17)='OBJROTT'
      GMENUS(17)='Object rotate:         '
      GTYPES(18)='ACTIVITY'
      GMENUS(18)='Activity Type:         '
      GTYPES(19)='BIDIRSET'
      GMENUS(19)='Optical bi-direc set:  '       
      GTYPES(20)='BAMSNSET'
      GMENUS(20)='BAM wireless sensor:   '  ! this is work in progress   

C DBTZNOBS is not used by the simulator, res will graph/list it.  
      GTYPES(21)='DBTZNOBS'
      GMENUS(21)='Zone db T (observed):  '

C SURTOBS is not used by the simulator, res will graph/list it.
      GTYPES(22)='SURTOBS'
      GMENUS(22)='Surface T (observed):'
      GTYPES(23)='NODPRESS'
      GMENUS(23)='Flow node press Pa:    '
      GTYPES(24)='ZNRHOBS'   ! use this for observed RH
      GMENUS(24)='Zone RH (observed): '   ! use this for observed RH
      GTYPES(25)='PERCTRL'
      GMENUS(25)='Pervasive ctrl:'
      GTYPES(26)='CTRLCAP'
      GMENUS(26)='Plant capacity:'

      GTYPES(27)='ELECPWR'
      GMENUS(27)='Real & reactive power: '

      GTYPES(28)='CFCCTL'
      GMENUS(28)='On/off and slat angle: '

C RAININD is currently only used to pass to external control via FMI.
      GTYPES(29)='RAININD'
      GMENUS(29)='Binary rain indicator: '

C Observed zone heating for display in res
      GTYPES(30)='ZNHTOBS'   ! use this for observed heating
      GMENUS(30)='Zone heat (observed): '

C Observed zone cooling for display in res
      GTYPES(31)='ZNCLOBS'   ! use this for observed cooling
      GMENUS(31)='Zone cool (observed): '

      GTYPES(32)='GRNDRFL'   ! use this for measured albedo
      GMENUS(32)='Ground reflectance: '      

      GTYPES(33)='SUPPLWT'   ! use this for measured supply water temperatures in plant water loops (e.g. for underfloor heating)
      GMENUS(33)='Supply water temperat: ' 

      GTYPES(34)='PUMPVFR'   ! use this for measured volume flow rates in plant water loops (e.g. for pumps in underfloor heating)
      GMENUS(34)='Pump volume flow rate: '
      
      GTYPES(35)='PRCSIG'    ! use this for price signal, for example for smart controls
      GMENUS(35)='Price signal: '
      
C If not looking for a specific item then return.
      IF(FOCUS(1:7).EQ.'ALLCLMT')THEN

C All climate data in tabular format.
C Data are: difuse hor (W/m2), dry bulb (C), direct normal or global
C horizontal (W/m2), wind speed (m/s), wind direction (deg from north)
C Relative humidity (%).  Static item flags direct normal (0) or global
C horizontal (123). Note: MGMA(4) cannot be a specific description because
C the static item will not be known in advance.
        GAIDE   ='Site climate '
        GHELP(1)='Climate: difuse hor (W/m2), dry bulb (C), direct'
        GHELP(2)='normal or global hor (W/m2), wind speed (m/s), wind'
        GHELP(3)='direction (deg from north), Relative humidity (%).'
        GMH='3,6,1,0,6,1,-999.0'
        GDHDRT(1)='   time  | difuse  dry   direct wind  wind  RH'
        GDHDRT(2)='   of day| solar   bulb  solar  speed direc % ' 
        GDHDRT(3)='Diff sol, db, dir sol, wind spd, wind dir, RH'
        GMF(1)='INTG,1,0,0,123'
        MGMA(1)='Radiation flag (0=DN 123=GH):   '
        GMF(2)='REAL,2,0.0,0.0,600.0'
        MGMA(2)='Diffuse hor solar rad (W/m2):   '
        GMF(3)='REAL,3,10.0,-49.0,49.0'
        MGMA(3)='Ambient DB temperature (C):     '
        GMF(4)='REAL,4,0.0,0.0,700.0'
        MGMA(4)='Direct solar (W/m2):            '
        GMF(5)='REAL,5,0.0,0.0,49.0'
        MGMA(5)='Wind velocity (m/s):            '
        GMF(6)='REAL,6,0.0,0.0,360.0'
        MGMA(6)='Wind direction (deg clockwise): '
        GMF(7)='REAL,7,50.0,0.0,100.0'
        MGMA(7)='Relative humidity (%):          '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'DBTEXTT')THEN

C External air temperature: one data item.
        GAIDE   ='External DB temp  '
        GHELP(1)='Dry bulb temperature deg C. This type used for '
        GHELP(2)='climate data NOT for control temperatures.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | external db '
        GDHDRT(2)='   of day| temperature '
        GDHDRT(3)='Exterior DB temperature:'
        GMF(1)='REAL,1,0.0,-49.0,49.0'
        MGMA(1)='Ext DB temperature (C):         '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'WINDVLT')THEN
        GAIDE   ='Wind velocity    '
        GHELP(1)='Wind velocity at the site in metres/sec.'
        GMH='1,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | wind velo-   '
        GDHDRT(2)='   of day| city m/s     ' 
        GDHDRT(3)='Wind velocity (m/s):'
        GMF(1)='REAL,1,0.0,0.0,49.0'
        MGMA(1)='Wind velocity (m/s) :           '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'WINDDRT')THEN
        GAIDE   ='Wind direction    '
        GHELP(1)='Wind direction at the site in degrees clockwise'
        GHELP(2)='from North. '
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | wind direc-  '
        GDHDRT(2)='   of day| tion (deg)   ' 
        GDHDRT(3)='Wind direction (deg clockwise):'
        GMF(1)='REAL,1,0.0,0.0,360.0'
        MGMA(1)='Wind direction (deg clockwise): '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'RELHUMT')THEN
        GAIDE   ='Rel humidity (%) '
        GHELP(1)='Relative humidity at the site in percent.'
        GMH='1,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | ext relative '
        GDHDRT(2)='   of day| humidity (%) ' 
        GDHDRT(3)='External relative humidity (%):'
        GMF(1)='REAL,1,50.0,0.0,100.0'
        MGMA(1)='Relative humidity (%):          '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'DIFHSLT')THEN
        GAIDE   ='Diffuse hor solar radiation'
        GHELP(1)='Diffuse horizontal solar radiation in Watts/m2'
        GMH='1,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | dif horiz  '
        GDHDRT(2)='   of day| solar rad  ' 
        GDHDRT(3)='Diffuse hor solar radiation (W/m2):'
        GMF(1)='REAL,1,0.0,0.0,600.0'
        MGMA(1)='Diffuse horiz solar rad (W/m2): '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'GLOHSLT')THEN
        GAIDE   ='Global hor solar radiation'
        GHELP(1)='Global horizontal solar radiation in Watts/m2'
        GMH='1,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | glob horiz '
        GDHDRT(2)='   of day| solar rad  ' 
        GDHDRT(3)='Global hor solar radiation (W/m2):'
        GMF(1)='REAL,1,0.0,0.0,600.0'
        MGMA(1)='Global horiz solar rad (W/m2):  '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'DIRSOLT')THEN
        GAIDE   ='Direct solar radiation '
        GHELP(1)='Direct normal solar radiation in Watts/m2. '
        GHELP(2)='If you want global horizontal use another.'
        GHELP(3)='generic type.'
        GMH='3,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | direct nor '
        GDHDRT(2)='   of day| solar rad  ' 
        GDHDRT(3)='Direct normal solar radiation (W/m2):'
        GMF(1)='REAL,1,0.0,0.0,600.0'
        MGMA(1)='Direct normal solar rad (W/m2): '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'VERTSOL')THEN

C Control state: one data item.
        GAIDE   ='Vertical solar radiation in W/m2'
        GHELP(1)='Global solar radiation data for surfaces in the'
        GHELP(2)='facade. Values in Watts/m^2'
        GMH='2,1,0,0,1,0,999.9'
        GDHDRT(1)='   time  | vertical solar  '
        GDHDRT(2)='   of day| radiation       '
        GDHDRT(3)='Vertical solar radiation (0 to 999):'
        GMF(1)='REAL,1,0.0,0.0,999.9'
        MGMA(1)='Vertical solar rad (W/m2)   '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'CASUALT')THEN

C Lumped zone casual loads -  in tabular format.
C << ? what about an attribute to tag which casual gain type this
C << relates to? (so other gains assumed to come from operation files?)
        GAIDE   ='Casual gains '
        GHELP(1)='Casual gains are lumped together in the form of '
        GHELP(2)='sensible conv W, sensible radiant W, and latent W.'
        GMH='2,3,0,0,3,0,-999.0'
        GDHDRT(1)='   time  |       sensible      | latent'
        GDHDRT(2)='   of day| convective  radiant |       ' 
        GDHDRT(3)='Sensible convective, radiant, latent loads:'

C Non-static convective, radiation, latent values.
        GMF(1)='REAL,1,0.0,-10000.0,100000.0'
        MGMA(1)='Sensible convec gain (W):       '
        GMF(2)='REAL,2,0.0,-10000.0,100000.0'
        MGMA(2)='Sensible radiant gain (W):      '
        GMF(3)='REAL,3,0.0,0.0,100000.0'
        MGMA(3)='Latent gain (W):                '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:8).EQ.'CASUAL3T')THEN

C Data for separate zone casual loads occup/lights/small power.
        GAIDE   ='Casual gains OccLtgEqp'
        GHELP(1)='Casual gains are in the form of sensible conv W, '
        GHELP(2)='sensible radiant W, laten W for occupants, lighting'
        GHELP(3)='and small power separately.'
        GMH='3,9,0,0,9,0,-999.0'
        write(GDHDRT(1),'(a)')
     &   ' time  |     ocupant     |    lights       |   small power   '
        write(GDHDRT(2),'(a)')
     &   ' of day| conv radnt latnt| conv radnt latnt| conv radnt latnt' 
        GDHDRT(3)='Sensible convective, radiant, latent loads:'

C Non-static convective, radiation, latent values.
        GMF(1) ='REAL,1,0.0,-10000.0,100000.0'
        MGMA(1)='Occup convec gain (W):       '
        GMF(2) ='REAL,2,0.0,-10000.0,100000.0'
        MGMA(2)='Occup radiant gain (W):      '
        GMF(3) ='REAL,3,0.0,0.0,100000.0'
        MGMA(3)='Occup latent gain (W):       '
        GMF(4) ='REAL,1,0.0,-10000.0,100000.0'
        MGMA(4)='Lights convec gain (W):      '
        GMF(5) ='REAL,2,0.0,-10000.0,100000.0'
        MGMA(5)='Lights radiant gain (W):     '
        GMF(6) ='REAL,3,0.0,0.0,100000.0'
        MGMA(6)='Lights latent gain (W):      '
        GMF(7) ='REAL,1,0.0,-10000.0,100000.0'
        MGMA(7)='Equip convec gain (W):       '
        GMF(8) ='REAL,2,0.0,-10000.0,100000.0'
        MGMA(8)='Equip radiant gain (W):      '
        GMF(9) ='REAL,3,0.0,0.0,100000.0'
        MGMA(9)='Equip latent gain (W):       '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'RAIRVLT')THEN
        GAIDE   ='Air velocity    '
        GHELP(1)='Air velocity in a zone in metres/sec.'
        GHELP(2)='NOT YET IMPLEMENTED IN SIMULATOR'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | air velo-    '
        GDHDRT(2)='   of day| city m/s     ' 
        GDHDRT(3)='Zone air velocity (m/s):'
        GMF(1)='REAL,1,0.0,0.0,49.0'
        MGMA(1)='Air velocity (m/s):             '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'ZIVENTT')THEN

C Data for zone ventilation and infiltration in ac/h - work in progress.
        GAIDE   ='Zone Vent:Infil'
        GHELP(1)='Zone infiltration and zone coupled ventilation in ' 
        GHELP(2)='ac/h (in terms of the receiving zone volume) set '
        GHELP(3)='units = 0 or m^3/sec set units = 1. '
        GHELP(4)='Static index of 1st & 2nd associated zone.'

C String with miscellaneous definitions included.
C         NHELP,NBITS,NSTATIC,NGMAT,NGMAR,NGMAI,GERR
        GMH='4,3,3,0,3,3,-999.0'
        GDHDRT(1)='   time  | infiltration|     ventilation'
        GDHDRT(2)='   of day| rate        | 1st src   2nd src' 
        GDHDRT(3)='Infiltration, ventilation (m^3/sec or ac/h):'

C Static field INTG : 0=ach 1-m3^hr associated zones.
C             type, position, default, range
        GMF(1)='INTG,1,0,0,1'
        MGMA(1)='Units: 0 = ac/h and 1 = m^3/sec'
        GMF(2)='INTG,2,0,0,82'
        MGMA(2)='Ventilation source 1st zone:    '
        GMF(3)='INTG,3,0,0,82'
        MGMA(3)='Ventilation source 2nd zone:    '

C Non-static infiltration and 1st and 2nd zone ventilation.
        GMF(4)='REAL,4,0.0,0.0,10000.0'
        MGMA(4)='Infiltration (m^3/sec):         '
        GMF(5)='REAL,5,0.0,0.0,10000.0'
        MGMA(5)='Ventilation 1st zone (m^3/sec): '
        GMF(6)='REAL,6,0.0,0.0,10000.0'
        MGMA(6)='Ventilation 2nd zone (m^3/sec):'
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'SETPTTT')THEN

C Control set point temperature (single) : one data item.
        GAIDE   ='Set point temp    '
        GHELP(1)='Set point temperature deg C. This type used for '
        GHELP(2)='control purposes with a single target temperature.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | set point   '
        GDHDRT(2)='   of day| temperature '
        GDHDRT(3)='Set point temperature (C):'
        GMF(1)='REAL,1,0.0,-101.0,101.0'
        MGMA(1)='Set point temperature (C) :     '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:6).EQ.'CFCCTL')THEN

C Control set point on/off and slat angle : two data items.
        GAIDE   ='On/off state & slat angle (-, deg)'
        GHELP(1)='Assign on/off state and slat angle in'
        GHELP(2)='two columns (-, deg). TDF control (CFCctl??)'
        GHELP(3)='uses this information for its setpoints.'        
        GMH='3,2,0,0,2,0,-999.9'
        GDHDRT(1)='   time  | On/off  |  Slat   '
        GDHDRT(2)='   of day| state   |  angle  '
        GDHDRT(3)='On/off state (-) & slat angle (deg):'
        GMF(1)='INTG,1,0,0,1'
        MGMA(1)='Slat angle (deg): '
        GMF(2)='REAL,2,0.0,-90.0,90.0'
        MGMA(2)='Slat angle (deg): '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'HTCLSETP')THEN

C Control state: two data item.
        GAIDE   ='Heating & Cooling setp. (degC)'
        GHELP(1)='Assign Heating and cooling set-points in'
        GHELP(2)='two columns (degC). Ideal control (BCL23)'
        GHELP(3)='uses this information for its setpoints.'        
        GMH='3,2,0,0,2,0,-999.9'
        GDHDRT(1)='   time  | Heating   |  Cooling   '
        GDHDRT(2)='   of day| set-point |  set-point '
        GDHDRT(3)='Heating set-points:'
        GMF(1)='REAL,1,0.0,-300.0,300.0'
        MGMA(1)='Heating set-points (degC): '
        GMF(2)='REAL,2,0.0,-300.0,300.0'
        MGMA(2)='Cooling set-points (degC): '
        FOUND=.TRUE.
        
      ELSEIF(FOCUS(1:7).EQ.'SKYTMPT')THEN
        GAIDE   ='Sky temp (C)    '
        GHELP(1)='Sky temperature (C) (measured).'
        GHELP(2)='WORK IN PROGRESS'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | sky temper- '
        GDHDRT(2)='   of day| ature (C)   ' 
        GDHDRT(3)='Sky temperature (C):'
        GMF(1)='REAL,1,0.0,-49.0,49.0'
        MGMA(1)='Sky temperature (C):            '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'SKYLUXT')THEN
        GAIDE   ='Sky lux     '
        GHELP(1)='Sky lux (measured).'
        GHELP(2)='NOT YET IMPLEMENTED IN SIMULATOR'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | sky lux '
        GDHDRT(2)='   of day|         ' 
        GDHDRT(3)='Sky lux:'
        GMF(1)='REAL,1,0.0,0.0,25000.0'
        MGMA(1)='Sky lux level:                  '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'OBJROTT')THEN
        GAIDE   ='Obj rotation'
        GHELP(1)='Object rotation (degrees from default orientation).'
        GMH='1,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | object  '
        GDHDRT(2)='   of day| rotation' 
        GDHDRT(3)='Obj rot:'
        GMF(1)='REAL,1,0.0,-360.0,360.0'
        MGMA(1)='Object rotation (deg):          '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'ACTIVITY')THEN
        GAIDE   ='Activity db (DOE/SBEM)'
        GHELP(1)='Data based on activity profiles'
        GHELP(2)='(e.g. for UK NCM compliance)'
        GMH='2,5,11,0,5,11,-999.0'
        write(GDHDRT(1),'(a)')
     &   ' time  | ocupant | lights| equipm.| Heati.|Cooli. '
        write(GDHDRT(2),'(a)')
     &   ' of day| fraction| fract.| fract. | Setp. | Setp. ' 
        GDHDRT(3)='activities database:'

C Need: fraction-reals: occupants, lights, equipment
C                reals: setpoint H, setpoint C
C       single entries: .....airflow
        GMF(1)='REAL,1,0.0,0.0,100.0'
        MGMA(1)='Occupant density (pers/m2):     '
        GMF(2)='INTG,2,0,0,999'
        MGMA(2)='Metabolic rate (W/pers):        '
        GMF(3)='INTG,3,0,0,100'
        MGMA(3)='Occup. Latent gain % (0-100):   '
        GMF(4)='REAL,4,0.0,0.0,400.0'
        MGMA(4)='Equipment (W/m2):               '
        GMF(5)='INTG,5,0,0,100'
        MGMA(5)='Equip. Latent gain % (0-100):   '
        GMF(6)='INTG,6,0,0,999'
        MGMA(6)='Number of luxes (lux):          '
        GMF(7)='REAL,7,0.0,0.0,400.0'
        MGMA(7)='Display Lighting (W/m2):        '
        GMF(8)='REAL,8,0.0,0.0,400.0'
        MGMA(8)='Dom. Hot Water (l/d/m2):        '
        GMF(9)='REAL,9,0.0,0.0,400.0'
        MGMA(9)='Outdoor air (l/s/pers.):       '
        GMF(10)='INTG,10,0,0,100'
        MGMA(10)='Min Humidity levels (%):       '
        GMF(11)='INTG,11,0,0,100'
        MGMA(11)='Max Humidity levels (%):       '
        GMF(12)='REAL,12,0.0,0.0,1.0'
        MGMA(12)='Occupant fraction:             '
        GMF(13)='REAL,13,0.0,0.0,1.0'
        MGMA(13)='Lights fraction:               '
        GMF(14)='REAL,14,0.0,0.0,1.0'
        MGMA(14)='Equipment fraction:            '
        GMF(15)='REAL,15,0.0,-101.0,101.0'
        MGMA(15)='Heating setpoint (C):          '
        GMF(16)='REAL,16,0.0,-101.0,101.0'
        MGMA(16)='Cooling setpoint (C):          '                        
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'BIDIRSET')THEN

C Control state: one data item.
        GAIDE   ='Bidirectional data set index'
        GHELP(1)='The index of this entity will specify the set'
        GHELP(2)='of bidirectional data to be used (i.e. in solar.F)'
        GMH='2,1,0,0,1,0,40'
        GDHDRT(1)='   time  | bidirectional  '
        GDHDRT(2)='   of day| dataset        '
        GDHDRT(3)='Bidirectional set (1 to 40):'
        GMF(1)='INTG,1,0,0,100'
        MGMA(1)='Bidirectional data set:   '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'BAMSNSET')THEN

C BAM wireless sensor data (to be done).
        GAIDE   ='BAM wireless sensor         '
        GHELP(1)='A BAM wireless sensor records temperature, light, RH'
        GHELP(2)='movement, audio level.'
        GMH='2,5,1,0,5,0,-999'
        GDHDRT(1)=
     &'   time  | temp | light |relative| PIR   |audio'
        GDHDRT(2)=
     &'   of day| degC | lux   |humidity| value |level' 
        GDHDRT(3)='Temp (degC)  Light (lux)  RH%  PIR(-) Audio(-):'
        GMF(1)='INTG,1,0,0,900'    ! static
        MGMA(1)='Sensor ID: (0-900)'
        GMF(2)='REAL,2,0.0,-100.0,100.0'
        MGMA(2)='Temp (degC):                '
        GMF(3)='REAL,3,0.0,0.0,20000.0'
        MGMA(3)='Light (lux):                '
        GMF(4)='REAL,4,0.0,0.0,100.0'
        MGMA(4)='RH (%):                     '
        GMF(5)='REAL,5,0.0,0.0,1000.0'
        MGMA(5)='Movement (-):               '
        GMF(6)='REAL,5,0.0,0.0,1000.0'
        MGMA(6)='Audio (-):                  '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'DBTZNOBS')THEN

C Observed zone air temperature: one data item.
        GAIDE   ='Observed zone DB temp '
        GHELP(1)='Zone dry bulb temperature degree C observed to'
        GHELP(2)='to display in res for comparison with predictions.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | obs zone db '
        GDHDRT(2)='   of day| temperature '
        GDHDRT(3)='Obs Zn DB temperature:'
        GMF(1)='REAL,1,0.0,-49.0,49.0'
        MGMA(1)='Obs Zn DB temperature (C): '
        FOUND=.TRUE.
        
      ELSEIF(FOCUS(1:7).EQ.'SURTOBS')THEN

C Observed surface temperature (for reporting in res): one data item.
        GAIDE   ='Observed surface temp '
        GHELP(1)='Zone surf temperature degree C observed to'
        GHELP(2)='display in res for comparison with predictions.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | obs surface'
        GDHDRT(2)='   of day| temperature '
        GDHDRT(3)='Obs Surf temperature:'
        GMF(1)='REAL,1,0.0,-49.0,49.0'
        MGMA(1)='Obs surf temperature (C): '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'ZNRHOBS')THEN
        GAIDE   ='Observed zone RH (%) '
        GHELP(1)='Zone RH % observed to display in res for'
        GHELP(2)='comparison with predictions.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | obs relative '
        GDHDRT(2)='   of day| humidity (%) ' 
        GDHDRT(3)='Obs relative humidity (%):'
        GMF(1)='REAL,1,50.0,0.0,100.0'
        MGMA(1)='Obs zone RH (%):          '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'ZNHTOBS')THEN
        GAIDE   ='Observed zone heat (%) '
        GHELP(1)='Zone heating observed to display in res'
        GHELP(2)='for comparison with predictions.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | obs heating '
        GDHDRT(2)='   of day| (W) ' 
        GDHDRT(3)='Obs zone heating (W):'
        GMF(1)='REAL,1,50.0,0.0,100.0'
        MGMA(1)='Obs zone heat (W):          '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'ZNCLOBS')THEN
        GAIDE   ='Observed zone cool (%) '
        GHELP(1)='Zone cooling observed to display in res'
        GHELP(2)='for comparison with predictions.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | obs coolinging '
        GDHDRT(2)='   of day| (W) ' 
        GDHDRT(3)='Obs zone cooling (W):'
        GMF(1)='REAL,1,50.0,0.0,100.0'
        MGMA(1)='Obs zone cool (W):          '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:8).EQ.'NODPRESS')THEN

C Measured node pressure (for reporting in res): one data item.
        GAIDE   ='Measured flow node pressure Pa'
        GHELP(1)='Fluid flow network node pressure (Pa)'
        GHELP(2)='measured data'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | measured node'
        GDHDRT(2)='   of day| pressure '
        GDHDRT(3)='Meas Node Pressure:'
        GMF(1)='REAL,1,0.0,-50.0,50.0'
        MGMA(1)='Measured node press(Pa): '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'PERCTRL')THEN

C Pervasive control: mnps data items (defined in bcl32)
        GAIDE   ='State of control parameters 0/1'
        GHELP(1)='State of control parameters '
        GHELP(2)='(on/off)'
        GMH='2,5,0,0,5,0,-999.0'
        GDHDRT(1)='   time  |sensor|sensor|sensor|sensor|sensor|'
        GDHDRT(2)='   of day|  1   |  2   |  3   |  4   |  5   |'
        GDHDRT(3)='Pervasive controller'
        GMF(1)='REAL,1,0.0,0.0,1.0'
        GMF(2)='REAL,2,0.0,0.0,1.0'
        GMF(3)='REAL,3,0.0,0.0,1.0'
        GMF(4)='REAL,4,0.0,0.0,1.0'
        GMF(5)='REAL,5,0.0,0.0,1.0'
        MGMA(1)='State of sensor 1  (0/1): '
        MGMA(2)='State of sensor 2  (0/1): '
        MGMA(3)='State of sensor 3  (0/1): '
        MGMA(4)='State of sensor 4  (0/1): '
        MGMA(5)='State of sensor 5  (0/1): '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'CTRLCAP')THEN

C Explicit Storage Heater for NINES project charge schedule 
        GAIDE   ='Grid charging schedule'
        GHELP(1)='Electric power input to  '
        GHELP(2)='storage heater zone'
        GMH='2,3,4,0,3,0,-999.0'
        GDHDRT(1)='   time  |sched-|Upper |Lower '
        GDHDRT(2)='   of day|ule   |set pt|set pt'
        GDHDRT(3)='Grid charge schedule'
        GMF(1)='REAL,1,0.0,0.0,5000.0'
        GMF(2)='REAL,2,0.0,0.0,10.0'
        GMF(3)='REAL,3,0.0,0.0,50.0'
        GMF(4)='REAL,4,0.0,0.0,50.0'
        GMF(5)='REAL,5,0.0,0.0,5000.0'
        GMF(6)='REAL,6,0.0,0.0,800.0'
        GMF(7)='REAL,7,0.0,0.0,100.0'
        MGMA(1)='Maximum charge input (Watts) '
        MGMA(2)='Number of cores'
        MGMA(3)='Dead band on upper set point'
        MGMA(4)='Dead band on lower set point'
        MGMA(5)='Grid charge input (watts)'
        MGMA(6)='Upper temperature set point'
        MGMA(7)='Lower temperature set point'
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'ELECPWR')THEN

C Real & reactive power for electrical networks: two data items.
        GAIDE   ='Real & reactive power (W)'
        GHELP(1)='Electrical equipment has both a real and reactive'
        GHELP(2)='power value depending on its power factor. You can'
        GHELP(3)='specify sources or demands via this item.'        
        GMH='3,2,0,0,2,0,-999.9'
        GDHDRT(1)='   time  | Real power|Reactive pwr'
        GDHDRT(2)='   of day| (W)       |  (W)       '
        GDHDRT(3)='Real:react power (W):'
        GMF(1)='REAL,1,0.0,0.0,900000.0'
        MGMA(1)='Real power (W): '
        GMF(2)='REAL,2,0.0,0.0,900000.0'
        MGMA(2)='Reactive power (W): '
        FOUND=.TRUE.

      ELSEIF(FOCUS(1:7).EQ.'RAININD')THEN

C Rain indicator, currently only used for FMI: one data item.
        GAIDE   ='Binary rain indicator (0 or 1)'
        GHELP(1)='This is currently not used in simulations, only to'
        GHELP(2)='pass to external models via FMI.'
        GMH='2,1,0,0,0,1,-999.0'
        GDHDRT(1)='   time  | Rain     |'
        GDHDRT(2)='   of day| indicator|'
        GDHDRT(3)='Rain indicator (0 or 1):'
        GMF(1)='INTG,1,0,0,1'
        MGMA(1)='Indicator value (0 or 1): '
        FOUND=.TRUE.


      ELSEIF(FOCUS(1:7).EQ.'GRNDRFL')THEN
        GAIDE   ='Ground reflectance (0 to 1)'
        GHELP(1)='Ground reflectance (0 to 1) '
        GHELP(2)='(measured). '
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | ground     |'
        GDHDRT(2)='   of day| reflectance|' 
        GDHDRT(3)='Ground reflectance (-):'
        GMF(1)='REAL,1,0.2,0.,1.0'
        MGMA(1)='Ground reflectance (-): '
        FOUND=.TRUE.      
      
      ELSEIF(FOCUS(1:7).EQ.'SUPPLWT')THEN
        GAIDE   ='Supply temperature (C)'
        GHELP(1)='Measured supply water '
        GHELP(2)='temperature (degC) '
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | supply     |'
        GDHDRT(2)='   of day| temperature|' 
        GDHDRT(3)='Supply temperature (C):'
        GMF(1)='REAL,1,35,-300.0,300.0'
        MGMA(1)='Supply temperature (C): '
        FOUND=.TRUE.    

      ELSEIF(FOCUS(1:7).EQ.'PUMPVFR')THEN
        GAIDE   ='Pump flow rate (l/h)'
        GHELP(1)='Measured volume flow '
        GHELP(2)='rate (l/h) '
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | water flow |'
        GDHDRT(2)='   of day| rate l/h   |' 
        GDHDRT(3)='Water flow rate (l/h):'
        GMF(1)='REAL,1,0.0036,0.0036,800.0'
        MGMA(1)='Water flow rate (l/h): '
        FOUND=.TRUE.   

      ELSEIF(FOCUS(1:6).EQ.'PRCSIG')THEN
        GAIDE   ='Price signal'
        GHELP(1)='Price signal for'
        GHELP(2)='smart control'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | price      |'
        GDHDRT(2)='   of day| signal     |' 
        GDHDRT(3)='Price signal (-):'
        GMF(1)='REAL,1,1.0,-999.9,999.9'
        MGMA(1)='Price signal (-): '
        FOUND=.TRUE.   

      ELSEIF(FOCUS(1:4).EQ.'ALL ')THEN
        RETURN

      ELSE

C Add in additional generic data definitions here.
C    ......
      ENDIF

C If specific search item found then return, otherwise loop back.
      IF(FOCUS.NE.'ALL')THEN
        IF(FOUND)THEN

C Strip general info in case beginning from scratch rather
C than reading from TDF file.
C            NHELP,NBITS,NSTATIC,NGMAT,NGMAR,NGMAI,GERR
C        GMH='2,2,8,1,7,2,-999.0'
          K=0
          CALL EGETWI(GMH,K,NHELP,0,MHEL,'F','nb help',IER)
          CALL EGETWI(GMH,K,NTBITS(IFOC),1,MBITS,'F',
     &      'total timestep fields',IER)
          CALL EGETWI(GMH,K,NTSTAT(IFOC),0,MGAR,'F',
     &      'nb of static data fields',IER)
          CALL EGETWI(GMH,K,NTMAT(IFOC),0,MGAT,'F',
     &      'nb text fields',IER)
          CALL EGETWI(GMH,K,NTMAR(IFOC),0,MGAR,'F',
     &      'nb real data fields',IER)
          CALL EGETWI(GMH,K,NTMAI(IFOC),0,MGAI,'F',
     &      'nb integer data fields',IER)

C Loop through the field definitions (in GMF) to get patterns.
C IEFLDS is the total number of fields in this instance.
          IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
          DO 55 J=1,IEFLDS
            K=0
            CALL EGETW(GMF(J),K,WORD,'F','REAL or TEXT',IER)
            IF(WORD(1:4).EQ.'REAL')THEN
              ITDFTR(IFOC,J)=1
              CALL EGETWI(GMF(J),K,IA,1,MGAR,'F','array ind',IER)
              IATDF(IFOC,J)=IA
              IF(IER.NE.0)RETURN
            ELSEIF(WORD(1:4).EQ.'TEXT')THEN

C Strip from typical string.  GMF(?)='TEXT,4,1
              ITDFTR(IFOC,J)=2
              CALL EGETWI(GMF(J),K,IALT(J),1,MIT,'F','alts',IER)
              CALL EGETWI(GMF(J),K,IA,1,MGAI,'F','array ind',IER)
              IATDF(IFOC,J)=IA
            ELSEIF(WORD(1:4).EQ.'INTG')THEN
              ITDFTR(IFOC,J)=3
              CALL EGETWI(GMF(J),K,IA,1,MGAI,'F','array',IER)
              IATDF(IFOC,J)=IA
              IF(IER.NE.0)RETURN
            ENDIF
   55     CONTINUE
          RETURN
        ELSE
          IER=1
          RETURN
        ENDIF
      ENDIF
      RETURN

      END


C ******* EXGMAT
C Extract text alternatives from gmat in the string array salt.
      subroutine exgmat(ia,SALT,ier)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      integer iCountWords

      COMMON/TDFG2/GMAT(MGAT)
      DIMENSION SALT(6)
      CHARACTER GMAT*52,line*52,SALT*16

      write(line,'(a52)') GMAT(IA)
      n = iCountWords(line)  ! get nb of tokens in the line of text
      salt(1)=' '
      salt(2)=' '
      salt(3)=' '
      salt(4)=' '
      salt(5)=' '
      salt(6)=' '
      k = 0
      IF(n.GE.1)call egetp(line,k,SALT(1),'W','1st alt',ier)
      IF(n.GE.2)call egetp(line,k,SALT(2),'W','2nd alt',ier)
      IF(n.GE.3)call egetp(line,k,SALT(3),'W','3rd alt',ier)
      IF(n.GE.4)call egetp(line,k,SALT(4),'W','4th alt',ier)
      IF(n.GE.5)call egetp(line,k,SALT(5),'W','5th alt',ier)
      IF(n.EQ.6)call egetp(line,k,SALT(6),'W','6th alt',ier)
      return
      end
