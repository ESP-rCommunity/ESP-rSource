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


C This file contains the following routines:
C      VWLIN
C      VGMXN
C      VGGET
C      VGDRW
C      VAXIS
C      VGHED

C ******************** VWLIN ********************

C Main controller routine for variable-v-variable
C graph construction. This output mode allows any pair of
C time-dependent variables to be plotted for one or more zones.

      SUBROUTINE VWLIN
#include "building.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/VGRAF1/YMAXV,YMINV,YMAXH,YMINH
      COMMON/VGRAF2/NPICK,IVV,IVH,IVCON(MCOM),IVELT(MCOM),
     &              IVPOS(MCOM),IVNOD(MCOM),VPSAT(MCOM)
      COMMON/VGRAF3/CONVER
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      CHARACTER*26 ITEM(42)
      character*12 vaxistitle(2)
      character VITEM*26,HITEM*26,xfile*144,tg*1,delim*1
      integer NITMS,INO ! max items and current menu item
      logical showedhelp

      helpinsub='vgraph'  ! set for subroutine

      MODIFYVIEW=.FALSE.
      showedhelp=.false.
      VITEM=' '
      HITEM=' '

      ITEM(1) ='2 result set             '
      ITEM(2) ='3        period          '
      ITEM(3) ='4 zones                  '
      ITEM(4) ='  ___________________    '
      ITEM(5) ='a outside air temp.      '
      ITEM(7) ='c control point temp.    '
      ITEM(8) ='d inside surface temp.   '
      ITEM(9) ='e outside surface temp.  '
      ITEM(10)='f resultant temp.        '
      ITEM(11)='g mean radiant temp.     '
      ITEM(12)='h intra-constr. temp.    '
      ITEM(13)='i                        '
      ITEM(14)='j                        '
      ITEM(15)='  ___________________    '
      ITEM(16)='k plant flux             '
      ITEM(17)='l infiltration           '
      ITEM(18)='m ventilation            '
      ITEM(19)='n                        '
      ITEM(20)='o                        '
      ITEM(21)='p                        '
      ITEM(22)='q casual gains           '
      ITEM(23)='r surface convection     '
      ITEM(24)='s surface longwave rad.  '
      ITEM(25)='t solar at external surf.'
      ITEM(26)='u solar at internal surf '
      ITEM(27)='v zone RH                '
      ITEM(28)='w zone condensation      '
      ITEM(29)='  ___________________    '
      ITEM(30)='5 direct solar radiation '
      ITEM(31)='6 diffuse solar radiation'
      ITEM(32)='7 wind speed             '
      ITEM(33)='8 wind direction         '
      ITEM(34)='9 ambient RH             '
      item(35)='+ dew point temp.        '
      item(36)='  ___________________    '
      item(37)='! draw                   '
      item(38)='= axis scale             '
      item(39)='/ clear                  '
      item(40)='# units                  '
      if(ixopen.eq.1)then
        item(41)='> echo >> file        '
      elseif(ixopen.eq.0)then
        item(41)='> echo >> screen      '
      endif
      item(42)='- exit menu              '

      CONVER=1000.

      YMAXV=-1.E+9; YMINV=1.E+9
      YMAXH=-1.E+9; YMINH=1.E+9
      NPICK=0
      NITMS=42

C Menu display.
   51 if(MMOD.eq.8)then
        INO=-1
      else
        INO=-2
      endif

C In case the user has resized the display redraw
C the image before presenting the command menu.
    5 if(MODIFYVIEW)then
        CALL VGHED(VITEM,HITEM)
        CALL VGDRW(vaxistitle,HITEM)
        INO=-2
        MODIFYVIEW=.FALSE.
      endif

C Instantiate help string arrays for this menu.
      helptopic='res_var_var_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(.NOT.showedhelp)then
        CALL PHELPD('var var chart',nbhelp,'-',0,0,IER)
        showedhelp=.true.
      endif

      CALL EMENU(' Var.vs.Var',ITEM,NITMS,INO)

C Test for illegal menu pick.
      IF(INO.EQ.0.OR.INO.EQ.4.OR.INO.EQ.15.OR.
     &   INO.EQ.29.OR.INO.EQ.36)THEN
        INO=-1
        goto  5
      ENDIF

      IF(ISAVE.EQ.1.AND.((INO.GE.8.AND.INO.LE.14).OR.
     &  (INO.GE.17.AND.INO.LE.28)))THEN
        call edisp(iuout,' Save option 1 does not contain sufficient')
        call edisp(iuout,' information to support this selection. ')
        goto 51
      ENDIF

      IF(ISAVE.EQ.2.AND.(INO.EQ.9.OR.INO.EQ.12.OR.INO.EQ.23))THEN
        call edisp(iuout,' Save option 2 does not contain sufficient')
        call edisp(iuout,' information to support this selection. ')
        goto 51
      ENDIF

      IF(INO.EQ.24)THEN
        call edisp(iuout,' This option not available in this version.')
        goto 51
      ENDIF

      IF(INO.EQ.1)CALL MORESS
      IF(INO.EQ.2)CALL MOOPER
      IF(INO.EQ.3)CALL MOZDFN

      IF(INO.LE.35.AND.INO.GE.5)THEN

C Register this pick and compute maximum and minimum
C values (to modify YMAX? and YMIN?).
        NPICK=NPICK+1
        IF(NPICK.EQ.1)THEN
          IVV=INO
          if(INO.eq.5)then
            vaxistitle(1)='Temp air'
            vaxistitle(2)='inside'
          elseif(INO.eq.6)then
            vaxistitle(1)='Temp air'
            vaxistitle(2)='outside'
          elseif(INO.eq.7)then
            vaxistitle(1)='Temp'
            vaxistitle(2)='control pnt'
          elseif(INO.eq.8)then
            vaxistitle(1)='Temp surface'
            vaxistitle(2)='inside face'
          elseif(INO.eq.9)then
            vaxistitle(1)='Temp surface'
            vaxistitle(2)='other face'
          elseif(INO.eq.10)then
            vaxistitle(1)='Resultant'
            vaxistitle(2)='temperature'
          elseif(INO.eq.11)then
            vaxistitle(1)='Mean radiant'
            vaxistitle(2)='temperature'
          elseif(INO.eq.12)then
            vaxistitle(1)='Temperature'
            vaxistitle(2)='intra-surf'
          elseif(INO.eq.16)then
            vaxistitle(1)='Plant inject'
            vaxistitle(2)='/extract'
          elseif(INO.eq.17)then
            vaxistitle(1)='Infiltration'
            vaxistitle(2)=''
          elseif(INO.eq.18)then
            vaxistitle(1)='Ventilation'
            vaxistitle(2)='(between zn)'
          elseif(INO.eq.22)then
            vaxistitle(1)='Casual'
            vaxistitle(2)='gains'
          elseif(INO.eq.23)then
            vaxistitle(1)='Surface'
            vaxistitle(2)='convection'
          elseif(INO.eq.24)then
            vaxistitle(1)='Surface lng-'
            vaxistitle(2)='wave radiatn'
          elseif(INO.eq.25)then
            vaxistitle(1)='Solar at'
            vaxistitle(2)='other face'
          elseif(INO.eq.26)then
            vaxistitle(1)='Solar at'
            vaxistitle(2)='inside face'
          elseif(INO.eq.27)then
            vaxistitle(1)='Zone'
            vaxistitle(2)='RH%'
          elseif(INO.eq.28)then
            vaxistitle(1)='Zone'
            vaxistitle(2)='condensation'
          elseif(INO.eq.30)then
            vaxistitle(1)='Solar direct'
            vaxistitle(2)='radiation'
          elseif(INO.eq.31)then
            vaxistitle(1)='Solar diff'
            vaxistitle(2)='radiation'
          elseif(INO.eq.32)then
            vaxistitle(1)='Wind'
            vaxistitle(2)='speed'
          elseif(INO.eq.33)then
            vaxistitle(1)='Wind'
            vaxistitle(2)='direction'
          elseif(INO.eq.34)then
            vaxistitle(1)='Ambient'
            vaxistitle(2)='RH%'
          elseif(INO.eq.35)then
            vaxistitle(1)='Dew point'
            vaxistitle(2)='temperature'
          else
            vaxistitle(1)='-'
            vaxistitle(2)='-'
          endif
          write(VITEM,'(A)')ITEM(INO)(3:lnblnk(ITEM(INO)))
        ELSEIF(NPICK.EQ.2)THEN
          IVH=INO
          write(HITEM,'(A)')ITEM(INO)(3:lnblnk(ITEM(INO)))
        ELSEIF(NPICK.EQ.3)THEN
          call edisp(iuout,' Two variables already defined. Select  ')
          call edisp(iuout,' draw (!) or clear (/) to re-define.')
          NPICK=2
          goto 51
        ENDIF

C Compute maximum and minimum values for this pick.
        CALL VGMXN(INO)
        goto 5
      ENDIF

      IF(INO.EQ.37)goto 11
      IF(INO.EQ.38)goto 13
      IF(INO.EQ.39)goto 14
      IF(INO.EQ.40)goto 15
      IF(INO.EQ.41)then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Var vs Var',IER)

C If user request cancel then redisplay menu after resetting ixopen to zero.
        if(ier.eq.-3)then
          ixopen=0
          goto 51
        endif

      endif
      IF(INO.EQ.42)goto 16
      goto 5

C !DRAW selected: plot paired variables over time.
   11 IF(NPICK.NE.2)THEN
        call edisp(iuout,' Two variables required!')
        goto 51
      ENDIF

      CALL VGHED(VITEM,HITEM)
      CALL VGDRW(vaxistitle,HITEM)
      goto 51

C '=SCLE>' command selected: set maximum and minimum
C axis values.
   13 CONTINUE
   32 call edisp(iuout,' Maximum and minimum values for the,')
      call edisp(iuout,' vertical and horizontal axes in turn ?')
      CALL EPROMPT
      READ(IUIN,*,IOSTAT=IOS,ERR=26)YMV,YNV,YMH,YNH

C Check for max. < min.
      IF(YMV.LT.YNV.OR.YMH.LT.YNH)goto 27

      YMAXV=YMV
      YMINV=YNV
      YMAXH=YMH
      YMINH=YNH
      goto 11

   26 call edisp(iuout,' Illegal number(s). ')
   33 call edisp(iuout,' Sorry, please respecify.')
      goto 32
   27 call edisp(iuout,' Maximum value is less than minimum value.')
      goto 33

C '/CLEAR' command selected.
   14 NPICK=0
      YMAXV=-1.E+10
      YMINV=1.E+10
      YMAXH=-1.E+10
      YMINH=1.E+10
      goto 5

C Load/intensity output can be WATTS or KILOWATTS.
   15 IF(NPICK.NE.0)then
        call edisp(iuout,' Units cannot be changed in middle of graph')
        call edisp(iuout,' content definition (you must first select')
        call edisp(iuout,' the CLEAR command).')
        goto 51
      endif
      CALL EASKAB(' ','Load/intensity variable in:',
     &            'Watts','Kilowatts',IANS,nbhelp)
      CONVER=1.
      IF(IANS.EQ.2)CONVER=1000.
      goto 51

   16 RETURN
      END

C ******************** VGMXN ********************

C VGMXN determines the maximum and minimum value of the
C chosen parameter, over the requested output period and
C for the selected zones. These values are then used to
C compute the scale of the vertical and horizontal axes
C at the time of graph construction.

      SUBROUTINE VGMXN(JPICK)
#include "building.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/VGRAF1/YMAXV,YMINV,YMAXH,YMINH
      COMMON/VGRAF2/NPICK,IVV,IVH,IVCON(MCOM),IVELT(MCOM),
     &              IVPOS(MCOM),IVNOD(MCOM),VPSAT(MCOM)

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION XVAL1(MS)
      character outs*124

      helpinsub='vgraph'  ! set for subroutine

C For each zone selected.
      DO 999 IZ=1,NZ
      II=NZNO(IZ)

C Establish associated information if JPICK = 8,9,12,23,25,26 or 35.
      IF(JPICK.EQ.35.OR.JPICK.GE.37)goto 2
      GOTO(2,2,2,2,1,1,1,3,3,1,1,5,7,7,2,1,1,1,7,7,1,8,
     &3,2,3,3,1,1,2,1,1,1,2,1,6),JPICK

    2 WRITE(outs,'(A,I3)')' MOGMXN: error menu pick was = ',JPICK
      call edisp(iuout,outs)
      CALL EPWAIT
      RETURN

C Which surface ?
    3 WRITE(outs,'(A,I3,A)')' Zone ',II,': which surface for output ?'
      call edisp(iuout,outs)
      CALL ASKSUR(II,ISUR)
      IVCON(IZ)=ISUR
      IF(JPICK.EQ.23)goto 7
      goto 1

C Construction node: which one ?
    5 WRITE(outs,'(A,I3,A)')' Zone ',II,
     &                       ': which construction for output ?'
      call edisp(iuout,outs)
      CALL ASKSUR(II,ISUR)
      IVCON(IZ)=ISUR

C Ask for node number.
      call ASKICN(II,ISUR,INN)
      IVNOD(IZ)=INN
      goto 1

C Dew point temperature: what % saturation ?
    6 helptopic='res_inside_saturation'
      call gethelptext(helpinsub,helptopic,nbhelp)
      WRITE(outs,'(A,I3,A)')' Zone ',II,': inside % saturation :'
      CALL EASKR(V,outs,' ?',0.,'F',100.,'F',50.,
     &    '% saturation',IER,nbhelp)
      VPSAT(IZ)=V
      goto 1

C Inside or outside ?
    7 helptopic='res_inside_face'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(' ','Which side :',
     &  'external (other side)','internal',IANS,nbhelp)
      IF(JPICK.EQ.23)IVELT(IZ)=IANS
      IF(JPICK.NE.23)IVCON(IZ)=IANS
      goto 1

C Radiant or convective casual gain ?
    8 helptopic='res_gains_rad_conv'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(' ','Casual gains :',
     &            'radiant','convective',IANS,nbhelp)
      IVCON(IZ)=IANS
      goto 1

    1 CONTINUE

C Finish zone loop.
  999 CONTINUE

C Compute max/min values for chosen parameter.
      DO 998 IZ=1,NZ
      II=NZNO(IZ)

      DO 130 I=IODS,IODF
      ID=I

C For the current zone and for every computational
C time-step, get results (for complete day) for requested
C parameter.

C First compute IGET: this defines the parameter to be recovered
C from the results library.
      IGET=JPICK
      IF(IGET.LT.15.AND.IGET.GT.4)IGET=IGET-4
      IF(IGET.LT.29.AND.IGET.GT.15)IGET=IGET-5
      IF(IGET.LE.35.AND.IGET.GT.29)IGET=IGET-6

      IF(IZ.EQ.1)THEN
        IF(IGET.EQ.2)THEN
          CALL MOCLIM(ID,1)
        ELSEIF(IGET.GE.25)THEN
          CALL MOCLIM(ID,2)
        ELSEIF(IGET.GE.24)THEN
          CALL MOCLIM(ID,3)
        ELSEIF(IGET.GE.26)THEN
          CALL MOCLIM(ID,5)
        ELSEIF(IGET.GE.27)THEN
          CALL MOCLIM(ID,6)
        ELSEIF(IGET.GE.28)THEN
          CALL MOCLIM(ID,7)
        ENDIF
      ENDIF
      CALL VGGET(II,ID,IGET,1)

C Stepping at defined output interval.
      N=24*NTS
      DO 140 J=1,N,NOUT
      JT=J

C Compute current time.
      call DATIME(JT,TIME)

C Within requested output period.
      TT1=FLOAT(IOH1)
      TT2=FLOAT(IOH2)
      IF(I.EQ.IODS.AND.TIME.LT.TT1)goto 140
      IF(I.EQ.IODF.AND.TIME.GT.TT2)goto 140

C Set value, computing average if required.
      XVAL1(1)=VAL1(1,J)
      IF(IAV.EQ.0)goto 102

      K1=J
      K2=J+NOUT
      XVAL1(1)=0.

      DO 160 L=K1,K2
      XMULT=1.
      IF(L.EQ.K1.OR.L.EQ.K2)XMULT=.5
      XVAL1(1)=XVAL1(1)+VAL1(1,L)*XMULT
  160 CONTINUE
      DIV=FLOAT(K2-K1)
      XVAL1(1)=XVAL1(1)/DIV

C Vertical axis.
  102 IF(NPICK.EQ.1)THEN
         YMAXV=AMAX1(YMAXV,XVAL1(1))
         YMINV=AMIN1(YMINV,XVAL1(1))
      ENDIF

C Horizontal axis.
      IF(NPICK.EQ.2)THEN
         YMAXH=AMAX1(YMAXH,XVAL1(1))
         YMINH=AMIN1(YMINH,XVAL1(1))
      ENDIF

  140 CONTINUE
  130 CONTINUE

  998 CONTINUE
      RETURN
      END

C ******************** VGGET ********************

C VGGET recovers from the results library any requested
C parameter for any specified zone and day.

C IGET values correspond to the following quantities:

C IGET = 1  - internal air temperature
C      = 2  - external air temperature
C      = 3  - control point temperature
C      = 4  - internal surface temperature
C      = 5  - external surface temperature
C      = 6  - resultant temperature
C      = 7  - mean radiant temperature
C      = 8  - construction node temperature
C      = 9  - window inside surface temperature
C      =10  - N/A ....
C      =11  - plant capacity
C      =12  - infiltration
C      =13  - ventilation
C      =14  - internal and external window conduction
C      =15  - N/A ....
C      =16  - window solar absorption
C      =17  - casual gains: convective and radiant
C      =18  - surface convection: internal and external
C      =19  - surface lw radiation: internal and external
C      =20  - external surface solar absorption
C      =21  - internal surface solar absorption
C      =22  - zone relative humidity
C      =23  - condensation value
C      =24  - direct normal solar intensity
C      =25  - diffuse horizontal solar intensity
C      =26  - wind speed
C      =27  - wind direction
C      =28  - relative humidity
C      =29  - dew point temperature

      SUBROUTINE VGGET(IZONE,IDAY,IGET,I12)
#include "building.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/RECV3/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/VGRAF2/NPICK,IVV,IVH,IVCON(MCOM),IVELT(MCOM),
     &              IVPOS(MCOM),IVNOD(MCOM),VPSAT(MCOM)
      COMMON/VGRAF3/CONVER

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION XD(MS),YDUM(MS)
      character outs*124

      IUNIT=IFIL

      DO 99 I=1,NZ
        IZ=NZNO(I)
        IF(IZ.EQ.IZONE)IP=I
   99 CONTINUE

C Compute start record address for requested recovery day.
      CALL MOREC1(IDAY,IREC)

C Recover appropriate value.
      N=24*NTS
      IF(IDAY.LT.ISDF.AND.NOUT.GT.1)N=N+NOUT
      DO 20 I=1,N

C Zone by zone until required zone is located.
      DO 30 J=1,NCOMP
      IC=J
      NN=NZSUR(J)
      IF(J.EQ.IZONE)goto 5

C Increment IREC depending on save option.
   34 CALL MOREC2(NN,IREC)
      goto 301

C Zone located: read appropriate values.
    5 goto (6,7,8,9,11,12,13,14,15,16,17,18,19,21,22,
     &23,24,25,34,26,27,331,332,28,29,31,32,33,333),IGET

C Internal air temperature.
    6 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)XD(1)
      IF(I12.EQ.1)VAL1(1,I)=XD(1)
      IF(I12.EQ.2)VAL2(1,I)=XD(1)
      goto 34

C External air temperature.
    7 IF(I12.EQ.1)VAL1(1,I)=VAL3(2,I)
      IF(I12.EQ.2)VAL2(1,I)=VAL3(2,I)
      goto 34

C Control point temperature.
    8 goto (35,37,37,37),ISAVE
   35 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)XD(1),XD(2)
      IF(I12.EQ.1)VAL1(1,I)=XD(2)
      IF(I12.EQ.2)VAL2(1,I)=XD(2)
      goto 34
   37 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)XD(1),XD(2),
     &(XD(K),K=1,NN),XD(1),XD(2),XD(3)
      IF(I12.EQ.1)VAL1(1,I)=XD(3)
      IF(I12.EQ.2)VAL2(1,I)=XD(3)
      goto 34

C Internal surface temperature.
    9 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,2),
     &(XD(K),K=1,NN)
      KK=IVCON(IP)
      IF(I12.EQ.1)VAL1(1,I)=XD(KK)
      IF(I12.EQ.2)VAL2(1,I)=XD(KK)
      goto 34

C External surface temperature.
   11 IREC=IREC+3
      DO 60 K=1,NN
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)XD(K)
      IREC=IREC+1
   60 CONTINUE
      KK=IVCON(IP)
      IF(I12.EQ.1)VAL1(1,I)=XD(KK)
      IF(I12.EQ.2)VAL2(1,I)=XD(KK)
      goto 30

C Resultant temperature.
   12 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)T,XD(1),
     &(XD(K),K=1,NN)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(YDUM(K),K=1,5)
      CALL MORESL(IC,T,XD,YDUM,NN,TRESL)
      IF(I12.EQ.1)VAL1(1,I)=TRESL
      IF(I12.EQ.2)VAL2(1,I)=TRESL
      IREC=IREC-1
      goto 34

C Mean radiant temperature.
   13 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)T,XD(1),(XD(K),
     &K=1,NN)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(YDUM(K),K=1,5)
      CALL MOMNRD(IC,T,XD,YDUM,NN,TMNRD)
      IF(I12.EQ.1)VAL1(1,I)=TMNRD
      IF(I12.EQ.2)VAL2(1,I)=TMNRD
      IREC=IREC-1
      goto 34

C Construction node temperature.
   14 ISUR=IVCON(IP)
C      IELM=IVELT(IP)
      INODE=IVNOD(IP)
      IREC=IREC+3
      DO 40 K=1,NN
      IF(K.EQ.ISUR)goto 42
      IREC=IREC+1
      goto 40
   42 NNODE=2*NELTS(J,K)
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(KK),KK=1,NNODE)
      IF(I12.EQ.1)VAL1(1,I)=XD(INODE)
      IF(I12.EQ.2)VAL2(1,I)=XD(INODE)
   40 CONTINUE
      IREC=IREC+1
      goto 30

C Window inside surface temperature for external and internal windows.
   15 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)T
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)QE,QI

      DIVE=1.
      DIVI=1.

C Internal surface temperature for external windows.
      IF(IVCON(IP).EQ.2)THEN
         WSTMP=T+((QE*0.123)/DIVE)
         WSTMP=0.
         IF(I12.EQ.1)VAL1(1,I)=WSTMP
         IF(I12.EQ.2)VAL2(1,I)=WSTMP
      ENDIF

C Internal surface temperature for internal windows.
      IF(IVCON(IP).EQ.1)THEN
         WSTMP=T+((QI*0.123)/DIVI)
         IF(I12.EQ.1)VAL1(1,I)=WSTMP
         IF(I12.EQ.2)VAL2(1,I)=WSTMP
      ENDIF

      IREC=IREC-1
      goto 34

   16 DIVE=1.
      DIVI=1.
      VAL1(1,I)=0.
      VAL2(1,I)=0.
      goto 34

C Plant capacity.
   17 IF(ISAVE.EQ.1)goto 53
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,2)
      IF(I12.EQ.1)VAL1(1,I)=XD(2)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=XD(2)/CONVER
      goto 34
   53 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,2),XD(1)
      IF(I12.EQ.1)VAL1(1,I)=XD(1)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=XD(1)/CONVER
      goto 34

C Infiltration.
   18 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,2),
     &(XD(K),K=1,NN),XD(1)
      IF(I12.EQ.1)VAL1(1,I)=XD(1)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=XD(1)/CONVER
      goto 34

C Ventilation.
   19 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,2),
     &(XD(K),K=1,NN),XD(1),XD(1)
      IF(I12.EQ.1)VAL1(1,I)=XD(1)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=XD(1)/CONVER
      goto 34

C Window conduction for external and internal windows.
   21 VAL1(1,I)=0.
      VAL2(1,I)=0.
      goto 34

C Door conduction for external and internal doors.
   22 VAL1(1,I)=0.
      VAL2(1,I)=0.
      goto 34

C Window solar absorption.
   23 VAL1(1,I)=0.0
      VAL2(1,I)=0.0
      goto 34

C Casual gains: convective and radiant.
   24 IREC=IREC+2
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,NN),
     &XD(1),XD(2)
      IF(IVCON(IP).EQ.2)XD(1)=XD(1)/CONVER
      IF(IVCON(IP).EQ.1)XD(1)=XD(2)/CONVER
      IF(I12.EQ.1)VAL1(1,I)=XD(1)
      IF(I12.EQ.2)VAL2(1,I)=XD(1)
      IREC=IREC-2
      goto 34

C Surface convection: internal and external surfaces.
C << This should only be available for save level 4 and
C << for the case of aggregate values in version 2 of
C << results library.
   25 VAL1(1,I)=0.0
      VAL2(1,I)=0.0
      goto 30

C External surface solar absorption.
   26 IREC=IREC+2
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,NN)
      KK=IVCON(IP)
      IF(I12.EQ.1)VAL1(K,I)=XD(KK)/CONVER
      IF(I12.EQ.2)VAL2(K,I)=XD(KK)/CONVER
      IREC=IREC-2
      goto 34

C Internal surface solar absorption.
   27 IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,5),
     &(XD(K),K=1,NN)
      KK=IVCON(IP)
      IF(I12.EQ.1)VAL1(K,I)=XD(KK)/CONVER
      IF(I12.EQ.2)VAL2(K,I)=XD(KK)/CONVER
      IREC=IREC-1
      goto 34

C Direct normal solar radiation.
   28 IF(I12.EQ.1)VAL1(1,I)=VAL3(3,I)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=VAL3(3,I)/CONVER
      goto 34

C Diffuse horizontal solar radiation.
   29 IF(I12.EQ.1)VAL1(1,I)=VAL3(1,I)/CONVER
      IF(I12.EQ.2)VAL2(1,I)=VAL3(1,I)/CONVER
      goto 34

C Wind speed.
   31 IF(I12.EQ.1)VAL1(1,I)=VAL3(4,I)
      IF(I12.EQ.2)VAL2(1,I)=VAL3(4,I)
      goto 34

C Wind direction.
   32 IF(I12.EQ.1)VAL1(1,I)=VAL3(5,I)
      IF(I12.EQ.2)VAL2(1,I)=VAL3(5,I)
      goto 34

C Relative humidity.
   33 IF(I12.EQ.1)VAL1(1,I)=VAL3(6,I)
      IF(I12.EQ.2)VAL2(1,I)=VAL3(6,I)
      goto 34

C Zone relative humidity.
  331 IREC=IREC+2
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,NN),
     &XD(1),XD(2),XD(3)
      IF(I12.EQ.1)VAL1(1,I)=XD(3)
      IF(I12.EQ.2)VAL2(1,I)=XD(3)
      IREC=IREC-2
      goto 34

C Zone condensation value.
  332 IREC=IREC+2
      READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)(XD(K),K=1,NN),
     &(XD(K),K=1,2),XD(1),XD(2)
      IF(I12.EQ.1)VAL1(1,I)=XD(2)
      IF(I12.EQ.2)VAL2(1,I)=XD(2)
      IREC=IREC-2
      goto 34

C Zone dew point temperature.
  333 READ(IUNIT,REC=IREC,IOSTAT=IOS,ERR=1000)XD(1)
      CALL MODEWP(XD(1),VPSAT(IP),DPT)
      IF(I12.EQ.1)VAL1(1,I)=DPT
      IF(I12.EQ.2)VAL2(1,I)=DPT
      goto 34

  301 IF(IDAY.EQ.ISDF.AND.NOUT.GT.1)goto 302
      goto 30
  302 DO 303 I1=1,NOUT
        VAL1(1,I1)=VAL1(1,I)
        VAL2(1,I1)=VAL2(1,I)
  303 CONTINUE
   30 CONTINUE
   20 CONTINUE

   56 RETURN
 1000 if(IOS.eq.2)then
        WRITE(outs,'(a,i5)')
     &    ' VGGET: no permission to read library at record',IREC
      else
        WRITE(outs,'(a,i5)')' VGGET: library error at record',IREC
      endif
      call edisp(iuout,outs)
      goto 56
      END

C ******************** VGDRW ********************
C Initiates axis drawing and displays all requested profiles.

      SUBROUTINE VGDRW(vaxistitle,HITEM)
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/VGRAF2/NPICK,IVV,IVH,IVCON(MCOM),IVELT(MCOM),
     &              IVPOS(MCOM),IVNOD(MCOM),VPSAT(MCOM)

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      character VITEM*26,HITEM*26
      character*12 vaxistitle(2)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

C Initiate graph axis drawing.
      CALL VAXIS(vaxistitle,HITEM)

C Zone loop.
      DO 999 IZ=1,NZ
        IZONE=NZNO(IZ)

C Consider requested output period.
        DO 20 II=IODS,IODF
          ID=II

C For current zone and for every computational time-step
C in this day, GET results for both parameters.
          IGET1=IVV
          IF(IGET1.LT.15.AND.IGET1.GT.4)IGET1=IGET1-4
          IF(IGET1.LT.29.AND.IGET1.GT.15)IGET1=IGET1-5
          IF(IGET1.LE.35.AND.IGET1.GT.29)IGET1=IGET1-6
          IF(IZ.EQ.1)THEN
            IF(IGET1.EQ.2)THEN
              CALL MOCLIM(ID,1)
            ELSEIF(IGET1.GE.25)THEN
              CALL MOCLIM(ID,2)
            ELSEIF(IGET1.GE.24)THEN
              CALL MOCLIM(ID,3)
            ELSEIF(IGET1.GE.26)THEN
              CALL MOCLIM(ID,5)
            ELSEIF(IGET1.GE.27)THEN
              CALL MOCLIM(ID,6)
            ELSEIF(IGET1.GE.28)THEN
              CALL MOCLIM(ID,7)
            ENDIF
          ENDIF
          CALL VGGET(IZONE,ID,IGET1,1)
          IGET2=IVH
          IF(IGET2.LT.15.AND.IGET2.GT.4)IGET2=IGET2-4
          IF(IGET2.LT.29.AND.IGET2.GT.15)IGET2=IGET2-5
          IF(IGET2.LE.35.AND.IGET2.GT.29)IGET2=IGET2-6
          IF(IZ.EQ.1)THEN
            IF(IGET2.EQ.2)THEN
              CALL MOCLIM(ID,1)
            ELSEIF(IGET2.GE.25)THEN
              CALL MOCLIM(ID,2)
            ELSEIF(IGET2.GE.24)THEN
              CALL MOCLIM(ID,3)
            ELSEIF(IGET2.GE.26)THEN
              CALL MOCLIM(ID,5)
            ELSEIF(IGET2.GE.27)THEN
              CALL MOCLIM(ID,6)
            ELSEIF(IGET2.GE.28)THEN
              CALL MOCLIM(ID,7)
            ENDIF
          ENDIF
          CALL VGGET(IZONE,ID,IGET2,2)

C Stepping at defined output interval. ITS not used.
C         ITS=NOUT/NTS
          N=24*NTS
          DO 30 J=1,N,NOUT
            JT=J

C Compute current time.
            IHR=(II-IODS)*24
            call DATIME(JT,ATIME)
            TIME=float(IHR)+ATIME

C Within requested output period.
            TSTART=FLOAT(IOH1)
            TFINSH=TSTART+FLOAT(((IODF-1)*24+IOH2)-((IODS-1)*24+IOH1))
            TSTA=TSTART-1.0
            IF(TIME.LT.TSTA)goto 30
            IF(TIME.GT.TFINSH)goto 30

C Set all possible values, computing average if requested.
            YVAL1=VAL1(1,J)
            YVAL2=VAL2(1,J)
            IF(IAV.EQ.0)goto 1
            K1=J
            K2=J+NOUT
            YVAL1=0.
            YVAL2=0.
            DO 50 L=K1,K2
              XMULT=1.
              IF(L.EQ.K1.OR.L.EQ.K2)XMULT=.5
              YVAL1=YVAL1+VAL1(1,L)*XMULT
              YVAL2=YVAL2+VAL2(1,L)*XMULT
   50       CONTINUE
            DIV=FLOAT(K2-K1)
            YVAL1=YVAL1/DIV
            YVAL2=YVAL2/DIV

    1       MARK=6
            IPEN=0
            IF(TIME.LE.TSTART)THEN
              iupdown=0
              isym=0
              call etplot(YVAL2,YVAL1,iupdown,isym)
            ELSE
              iupdown=IPEN
              isym=MARK
              call etplot(YVAL2,YVAL1,iupdown,isym)
            ENDIF
   30     CONTINUE
   20   CONTINUE
  999 CONTINUE

      RETURN
      END

C ******************** VAXIS ********************
C Draws the graphical axes relating to the requested profiles.

      SUBROUTINE VAXIS(vaxistitle,HITEM)

      COMMON/VGRAF1/YMAXV,YMINV,YMAXH,YMINH
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character HITEM*26,HIT*26
      character*12 vaxistitle(2)
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iicol,ibsize,ipixw,ipixh
      integer iid1,iid2,iid3
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iicol,ibsize,ipixw,ipixh
      integer*8 iid1,iid2,iid3
#endif

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Interval on vertical axis.
      A=YMAXV-YMINV
      IF(A.LT.0.2)THEN
        YMAXV=YMAXV+.2
        YMINV=YMINV-.2
      ENDIF

C Interval on horizontal axis.
      B=YMAXH-YMINH
      IF(B.LT.0.2)THEN
        YMAXH=YMAXH+.2
        YMINH=YMINH-.2
      ENDIF

C Determine the scaling for both axis and place in static 
C memory for access by plotting code.
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,YMINH,YMAXH,YMINV,YMAXV,xsc,ysc,sca,
     &              Xadd,Yadd)
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)

C Copy axis labels into local variable and then display the 
C horizontal and vertical axis.
      HIT=' '
      HIT=HITEM
      call dintervalf(YMINH,YMAXH,ddy1,ny,0)
      call horaxisdd(YMINH,YMAXH,iigl,iigr,iigb,Xadd,xsc,0,ddy1,ny,HIT)
      call textpixwidth(vaxistitle(1),ipixw,ipixh)    ! get its width
      iid1=15; iid2=igt-(ipixh); iid3=igt-1  ! Do left justified.
      CALL dintervalf(YMINV,YMAXV,ddy1,ny,0)
      call vrtaxisdd(YMINV,YMAXV,iigl,iigb,iigt,Yadd,ysc,0,
     &  ddy1,ny,0,'  ')
      iicol=0; ibsize=0
      call textsizeatxy(iid1,iid2,vaxistitle(1),ibsize,'-',iicol)
      call textsizeatxy(iid1,iid3,vaxistitle(2),ibsize,'-',iicol)

      RETURN

      END


C ******************** VGHED ********************
C Outputs the appropriate header information corresponding
C to graphical result display.

      SUBROUTINE VGHED(VITEM,HITEM)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES

      CHARACTER ETEXT*94
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER PDESCR*64,SDESCR*44,VITEM*26,HITEM*26
      CHARACTER outsd*124,outsd1*126
      integer lsn1

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
      integer ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
      integer*8 ilf,igfw,igfh,ild,igdw,igdh
#endif

C Generate a descriptive string for the time period to be displayed.
C and the simulation and output timesteps.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
C        iiw1=10; iiw2=8; iiw3=5; iiw4=3; iimenu=menuchw
        iiw1=7; iiw2=8; iiw3=7; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          ilf=2; ild=LIMTTY
          call feedbox(iimenu,ilf,igfw,igfh)
          call opengdisp(iimenu,ild,ilf,igdw,igdh)
        endif
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        call startbuffer()
      ENDIF

      lsn1=MIN0(lnblnk(RFILE),42)
      WRITE(ETEXT,'(1X,A,A,A)')SDESCR,' Results library:',RFILE(1:lsn1)
      iside=1; isize=0; ifont=1
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

      WRITE(ETEXT,'(1X,A)')PDESCR
      iside=2
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

      call znlist(outsd,len,ierr) 
      if(ierr.ne.1)then
        write(outsd1,'(1x,a)')outsd
        iside=3
        if(mmod.eq.8)then
          call viewtext(outsd1,iside,isize,ifont)
        else
          call viewtextwwc(outsd1,iside,isize,ifont)
        endif
      endif

      WRITE(ETEXT,'(3a)')
     &  VITEM(1:lnblnk(VITEM)),' vs ',HITEM(1:lnblnk(HITEM))
      iside=5; isize=1
      if(mmod.eq.8)then
        call viewtext(etext,iside,isize,ifont)
      else
        call viewtextwwc(etext,iside,isize,ifont)
      endif

      RETURN
      END
