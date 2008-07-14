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

C EBRSTAB: searches the item defs for tabular generic choices.
C GENDAT:  holds cases of generic data.

C ************* EBRSTAB 
C EBRSITM searches the item definition and produces a menu of generic 
C TABULAR choices available and returns its index INO.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EBRSTAB(IFG,IER)
      common/pophelp/h(60)
      COMMON/TDFGH1/NITEMS
      COMMON/TDFGH2/GTYPES(20),GMENUS(20)
      DIMENSION ISGGT(23),IVAL(23)
      LOGICAL FOUND
      CHARACTER GMENUS*16,GTYPES*8
      CHARACTER KEY*1,ISGGT*30,FOCUS*8,h*72

C Read the generic descriptions to see what is available.
      IER=0
      FOCUS='ALL'
      CALL GENDAT(FOCUS,0,FOUND,IER)
      M=1
      DO 3 IT=1,NITEMS
        M=M+1
        CALL EMKEY(M,KEY,IER)
        IVAL(M)=IT
        WRITE(ISGGT(M),72)KEY,GMENUS(IT),GTYPES(IT)
   72   FORMAT(A1,1X,A16,1x,A8)
    3 CONTINUE
      ISGGT(1)  ='  Description       TAG     '
      ISGGT(M+1)='? Help                      '
      ISGGT(M+2)='- Exit                      '

C Instanciateh() strings for this menu.
      h(1)='The tab type menu does not yet have'
      h(2)='any help associated with it. '

C Make a menu of available generic items.
      INO=-2
   14 CALL EMENU('Available TAB types',ISGGT,M+2,INO)
      IF(INO.EQ.M+2)THEN
        INO=0
        RETURN
      ELSEIF(INO.EQ.M+1)THEN

C Help text...
      ELSEIF(INO.GE.2.AND.INO.LE.M+1)THEN
        IFG=IVAL(INO)
        write(6,*) 'picked ',ifg,ino
        RETURN
      ELSE
        INO=-1
        GOTO 14
      ENDIF

      END

C ************* GENDAT 
C GENDAT holds data on generic items to place into common blocks 
C TDFG1 TDFG2 TDFG3 TDFG5.
C ITRU is the unit number for user output, IER=0 OK, IER=1 problem.
C FOCUS is the generic type string, IFOC is the instance array.
C In the case of FOCUS='ALL' IFOC is not used. 
      SUBROUTINE GENDAT(FOCUS,IFOC,FOUND,IER)
#include "tdf2.h"
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)

C Related to generic items. See __ for details.
C  GTYPES(): 8 char generic type tag for type of processing and info.
C  GMENUS(): 16 char descriptive text for use in selection menus.
      COMMON/TDFGH1/NITEMS
      COMMON/TDFGH2/GTYPES(20),GMENUS(20)

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

      CHARACTER GMENUS*16,GTYPES*8
      CHARACTER GAIDE*36,GHELP*72,GDHDRT*72,GMH*32
      CHARACTER GMF*32,MGMA*32
      CHARACTER FOCUS*8,WORD*20
      LOGICAL FOUND

C Pick up the arrays used for displaying current generic items.
      NITEMS=20
      GTYPES(1) ='ALLCLMT'
      GMENUS(1) ='Climate data:  '
      GTYPES(2)='CASUALT'
      GMENUS(2)='Casual gains(W):'
      GTYPES(3)='ZIVENTT'
      GMENUS(3)='Zone infil_vent:'
      GTYPES(4)='RAIRVLT'
      GMENUS(4)='Air (room) velo:'
      GTYPES(5)='SETPTTT'
      GMENUS(5)='Set point temp: '
      GTYPES(6)='CTLSTAT'
      GMENUS(6)='Control state: '
      GTYPES(7)='SKYLUXT'
      GMENUS(7)='Sky lux        :'
      GTYPES(8)='DBTEXTT'
      GMENUS(8)='Extn Dry Bulb T:'
      GTYPES(9)='WINDVLT'
      GMENUS(9)='Wind velocity  :'
      GTYPES(10)='WINDDRT'
      GMENUS(10)='Wind direction :'
      GTYPES(11)='RELHUMT'
      GMENUS(11)='Ext Rel humidty:'
      GTYPES(12)='DIFHSLT'
      GMENUS(12)='Diff horz solar:'
      GTYPES(13)='GLOHSLT'
      GMENUS(13)='Global H solar :'
      GTYPES(14)='DIRSOLT'
      GMENUS(14)='Direct N solar :'
      GTYPES(15)='SKYTMPT'
      GMENUS(15)='Sky temperature:'
      GTYPES(16)='GLOVRTT'
      GMENUS(16)='Global Vt solar:'
      GTYPES(17)='OBJROTT'
      GMENUS(17)='Object rotation:'
      GTYPES(18)='CASUAL3T'
      GMENUS(18)='Casual OccLtEqp:'
      GTYPES(19)='ACTIVITY'
      GMENUS(19)='Activity Type:'
      GTYPES(20)='BIDIRSET'
      GMENUS(20)='Optical set:'       

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
      ELSEIF(FOCUS(1:7).EQ.'SETPTTT')THEN

C Set point temperature: one data item.
        GAIDE   ='Set point temp    '
        GHELP(1)='Set point temperature deg C. This type used for '
        GHELP(2)='control purposes.'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | set point   '
        GDHDRT(2)='   of day| temperature '
        GDHDRT(3)='Set point temperature (C):'
        GMF(1)='REAL,1,0.0,-101.0,101.0'
        MGMA(1)='Set point temperature (C) :     '
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'CTLSTAT')THEN

C Control state: one data item.
        GAIDE   ='Control state   '
        GHELP(1)='Control state (-1.0 to 1.0). This type used for'
        GHELP(2)='control purposes. NOT YET IMPLEMENTED IN SIMULATOR'
        GMH='2,1,0,0,1,0,-999.0'
        GDHDRT(1)='   time  | control  '
        GDHDRT(2)='   of day| state    '
        GDHDRT(3)='Control state (0.0 to 1.0):'
        GMF(1)='REAL,1,0.0,0.0,1.0'
        MGMA(1)='Control state (0.0 to 1.0):     '
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
      ELSEIF(FOCUS(1:7).EQ.'CASUALT')THEN

C Lumped zone casual loads -  in tabular format.
C << ? what about an attribute to tag which casual gain type this
C << relates to? (so other gains assumed to come from operation files?)
        GAIDE   ='Casual gains '
        GHELP(1)='Casual gains are in the form of sensible conv W, '
        GHELP(2)='sensible radiant W, laten W.'
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
        GHELP(3)='and small power'
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
        GMF(2)='INTG,2,0,0,24'
        MGMA(2)='Ventilation source 1st zone:    '
        GMF(3)='INTG,3,0,0,24'
        MGMA(3)='Ventilation source 2nd zone:    '

C Non-static infiltration and 1st and 2nd zone ventilation.
        GMF(4)='REAL,4,0.0,0.0,10000.0'
        MGMA(4)='Infiltration (m^3/sec):         '
        GMF(5)='REAL,5,0.0,0.0,10000.0'
        MGMA(5)='Ventilation 1st zone (m^3/sec): '
        GMF(6)='REAL,6,0.0,0.0,10000.0'
        MGMA(6)='Ventilation 2nd zone (m^3/sec):'
        FOUND=.TRUE.
      ELSEIF(FOCUS(1:7).EQ.'SKYTMPT')THEN
        GAIDE   ='Sky temp (C)    '
        GHELP(1)='Sky temperature (C) (measured).'
        GHELP(2)='NOT YET IMPLEMENTED IN SIMULATOR'
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
          CALL EGETWI(GMH,K,NHELP,0,MHEL,'F','help',IER)
          CALL EGETWI(GMH,K,NTBITS(IFOC),1,MBITS,'F','bits',IER)
          CALL EGETWI(GMH,K,NTSTAT(IFOC),0,MGAR,'F','nstat',IER)
          CALL EGETWI(GMH,K,NTMAT(IFOC),0,MGAT,'F','tx fld',IER)
          CALL EGETWI(GMH,K,NTMAR(IFOC),0,MGAR,'F','rl fld',IER)
          CALL EGETWI(GMH,K,NTMAI(IFOC),0,MGAI,'F','in fld',IER)

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
#include "tdf2.h"
      COMMON/TDFG2/GMAT(MGAT)
      DIMENSION SALT(6)
      CHARACTER GMAT*52,line*52,SALT*16

      write(line,'(a52)') GMAT(IA)
      CALL CHITMS(line,n)
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
