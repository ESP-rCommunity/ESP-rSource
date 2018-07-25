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

C This file contains the following routines:
C  MO3DPL: Plot a 3-D surface representation of a variable.
C  SURF3:  Plot a 3D surface representation of 2D arrays of time
C          series data ie. timesteps-vs-day-vs-data. 
C  RWRAND: Read and write binary random access file for drawing data.
C  COEF3D: Calculate rotation coefficients for a 2D representation
C          of a 3D figure with rotations about the X & Y axis.
C  FRAMER: Draw the frame (context of the surface plot).
C  MASKED: Enters edge of figure into array mask as the figure is drawn.
C  ROTN3D: Scale and rotate a 3D array to produce 2D representation.
C  SETQUD: Set default for drawing frame.


C ******************** MO3DPL ********************

C MO3DPL plots a 3-D surface representation of a
C user-specified variable.

      SUBROUTINE MO3DPL
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      common/recver/izver,ipver,iever

      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV

      COMMON/PLT3D/ISUR,NCNOUT,NELOUT,NPSOUT,NNDOUT,INEXWC,
     &             ICRCG,INEXSC,ICI

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      CHARACTER*23 ITEM(28)
      CHARACTER*10 ITITLX,ITITLY,ITITLZ
      CHARACTER*40 ITITLE
      DIMENSION DATA(10),CQ(MTS)
      CHARACTER LNAME*72,outs*124,etxt*10
      logical zlist
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real equipc,equipr,equipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.
      integer jsur,jclm,ICI  ! for radio buttons
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found
      integer IUNIT      ! for the scratch file used in drawing

      helpinsub='MO3DPL'  ! set for subroutine

      DATA ITITLX/'Time-steps'/
      DATA ITITLZ/'Year day  '/
      DATA ITITLY/'  Variable'/
      DATA LNAME/'E9S7P3'/

C Set chosen result set.
      ISET=ISIM

      ITEM(1) ='2 select result set   '
      ITEM(2) ='3 define period       '
      ITEM(3) ='4 select zones        '
      ITEM(4) ='  ___________________ '
      ITEM(5) ='a zone db temperature '
      ITEM(6) ='b ambient temperature '
      ITEM(7) ='c control point temp  '
      ITEM(8) ='d resultant temp      '
      ITEM(9) ='e MRT                 '
      ITEM(10)='  ___________________ '
      ITEM(11)='f surface temps       '
      ITEM(12)='g surface flux        '
      ITEM(13)='  ___________________ '
      ITEM(14)='i heating flux        '
      ITEM(15)='j cooling flux        '
      ITEM(16)='k total plant flux    '
      ITEM(17)='  ___________________ '
      ITEM(18)='l infiltration        '
      ITEM(19)='m ventilation         '
      ITEM(20)='q zone casual gains   '
      ITEM(21)='  ___________________ '
      ITEM(22)='r climate             '
      ITEM(23)='s comfort metrics     '
      ITEM(24)='  ___________________ '
      ITEM(25)='1 rotate view         '
      ITEM(26)='2 scale data          ' 
      ITEM(27)='! draw 3d plot        ' 
      ITEM(28)='- exit                '

      IUNIT=IFIL+6
      XR=45.
      ZR=45.
      YS=0.
      II=NZNO(1)

   61 NITMS=28
      INO=-2
    3 continue

C Instanciate help array for the menu (including each of the
C sub-dialogs).
      helptopic='res_3D_plot_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('  3D profile',ITEM,NITMS,INO)
      write(outs,'(a,a,a)')'In zone ',zname(II)(1:lnzname(II)),
     &      ' please select a surface...'

C Test for illegal menu pick.
      IF(INO.EQ.0.OR.INO.EQ.4.OR.INO.EQ.10.OR.INO.EQ.13.OR.INO.EQ.
     &         17.OR.INO.EQ.21.OR.INO.EQ.24)then
        INO=-1
        goto 3
      endif

      IF(INO.EQ.1)then
        CALL MORESS
        goto 61
      elseif(INO.EQ.2)then
        CALL MOOPER
        goto 61
      elseif(INO.EQ.3)then
        CALL MOZDFN
        II=NZNO(1)
        goto 61
      elseif(INO.EQ.5)then

C IGET = 1 zone temperature.
        zlist=.true.
        IGET=1
        ITITLE = 'Zone db temperature'
      elseif(INO.EQ.6)then

C IGET = 2 outside temperature.
        zlist=.false.
        IGET=2
        ITITLE = 'Outside db temperature'
      elseif(INO.EQ.7)then

C IGET = 3 control point temperaturee.
        zlist=.true.
        IGET=3
        ITITLE = 'Control pt temperature'
      elseif(INO.EQ.8)then

C IGET = 6  Resultant temperature.
        zlist=.true.
        IGET=6
        ITITLE = 'Resultant temperature'
      elseif(INO.EQ.9)then
        zlist=.true.
        IGET=24
        ITITLE = 'Mean Radiant temperature'
      elseif(INO.EQ.11)then
        jsur=1
        CALL EASKATOG('Surface temperatures:',' ',
     &    'T inside','T outside','T node',' ',' ',' ',' ',jsur,nbhelp)
        zlist=.true.
        if(jsur.eq.1)then
          IGET=4
          CALL ASKSUR(II,NCNOUT)
          ITITLE = 'Surface temp inside'
        elseif(jsur.eq.2)then
          IF(ISAVE.lt.3.)then
            call edisp(iuout,' This selection only available with')
            call edisp(iuout,' a save option 3-4 result-set.')
            INO=-1
            goto 3
          endif
          IGET=5
          CALL ASKSUR(II,NCNOUT)
          ITITLE = 'Surface temp outside'
        elseif(jsur.eq.3)then
          IF(ISAVE.ne.3)then
            call edisp(iuout,' This selection only available with')
            call edisp(iuout,' a save option 3 result-set.')
            INO=-1
            goto 3
          endif
          IGET=7
          CALL ASKSUR(II,NCNOUT)
          call ASKICN(II,NCNOUT,NNDOUT)
          ITITLE = 'Surface node temp'
        endif
      elseif(INO.EQ.12)then
        jsur=1
        CALL EASKATOG('Surface fluxes:',' ',
     &    'Convection int','Convection ext','Total convec int',
     &    'Solar abs ext','Solar abs int',' ',' ',jsur,nbhelp)
        zlist=.true.
        if(jsur.eq.1)then
          if(ISAVE.lt.4.and.izver.lt.2)then
            call edisp(iuout,' This selection requires save 4 or')
            call edisp(iuout,' version 2 of results library.')
            INO=-1
            goto 3
          endif
          IGET=16
          INEXSC = 1
          ITITLE = 'Surface convection inside'
          CALL ASKSUR(II,NCNOUT)
        elseif(jsur.eq.2)then
          if(ISAVE.lt.4.and.izver.lt.2)then
            call edisp(iuout,' This selection requires save 4 or')
            call edisp(iuout,' version 2 of results library.')
            INO=-1
            goto 3
          endif
          IGET=16
          INEXSC = 2
          ITITLE = 'Surface convection outside'
          CALL ASKSUR(II,NCNOUT)
        elseif(jsur.eq.3)then
          if(ISAVE.lt.4.and.izver.lt.2)then
            call edisp(iuout,' This selection requires save 4 or')
            call edisp(iuout,' version 2 of results library.')
            INO=-1
            goto 3
          endif
          IGET=16
          INEXSC = 3
          ITITLE = 'Total convection inside (W)'
        elseif(jsur.eq.4)then
          IF(ISAVE.ne.3)then
            call edisp(iuout,' This selection only available with')
            call edisp(iuout,' a save option 3 result-set.')
            INO=-1
            goto 3
          endif
          IGET=17
          ITITLE = 'Surface solar abs outside'
          CALL ASKSUR(II,NCNOUT)
        elseif(jsur.eq.5)then
          IGET=18
          ITITLE = 'Surface solar abs inside'
          CALL ASKSUR(II,NCNOUT)
        endif
      elseif(INO.EQ.14)then

C IGET = 8   Heating flux (also by multi-zone)
        zlist=.true.
        IGET=8
        ITITLE = 'Heating injection (w)'
      elseif(INO.EQ.15)then

C IGET = 9   Cooling flux (also by multi-zone)
        zlist=.true.
        IGET=9
        ITITLE = 'Cooling (w) '
      elseif(INO.EQ.16)then

C IGET = 10  Total plant flux
        zlist=.true.
        IGET=10
        ITITLE = 'Total plant flux (w)'
      elseif(INO.EQ.18)then

C IGET = 11  Infiltration
        zlist=.true.
        IGET=11
        ITITLE = 'Infiltration (w)'
      elseif(INO.EQ.19)then

C IGET = 12  Ventilation
        zlist=.true.
        IGET=12
        ITITLE = 'Ventilation (w)'
      elseif(INO.EQ.20)then

C IGET = 15  Casual gains  << ? expand, perhaps to total and perhaps by type >>
        zlist=.true.
        IGET=15
        CALL EASKAB(' ','Casual gains:',
     &    'Convective ','Radiant ',ICRCG,nbhelp)
        if(ICRCG.eq.1)ITITLE = 'Casual gain convective(w)'
        if(ICRCG.eq.2)ITITLE = 'Casual gain radiant (w)'
      elseif(INO.EQ.22)then
        jclm=1
        CALL EASKATOG('Climate data:',' ',
     &    'Solar direct','Solar diffuse','Wind speed','Ambient rh',' ',
     &    ' ',' ',jclm,nbhelp)
        zlist=.false.
        if(jclm.eq.1)then
          IGET=19
          ITITLE = 'Direct solar intensity'
        elseif(jclm.eq.2)then
          IGET=20
          ITITLE = 'Diffuse solar intensity'
        elseif(jclm.eq.3)then
          IGET=21
          ITITLE = 'Wind speed'
        elseif(jclm.eq.4)then
          IGET=22
          ITITLE = 'Ambient RH'
        endif
      elseif(INO.EQ.23)then

C IGET = 23  Comfort, ask which index and initialise.
        zlist=.true.
        IGET=23
        ICI=1
        CALL EASKATOG('Comfort indices:',' ',
     &    'PMV','PMV (set)','PPD',' ',' ',' ',' ',ICI,nbhelp)
        if(ICI.eq.1)ITITLE = 'Comfort PMV'
        if(ICI.eq.2)ITITLE = 'Comfort PMV (set)'
        if(ICI.eq.3)ITITLE = 'Comfort PPD'
        CALL MOCMFT(IODS,NZNO(1),0,'I')
      elseif(INO.EQ.25)then

C Set rotations.
        CALL EASKR(XR,' ',' X-axis rotation ?',
     &    0.,'F',90.,'F',0.,'x-axis rotation',IER,nbhelp)

        CALL EASKR(ZR,' ',' Z-axis rotation ?',
     &    0.,'F',360.,'F',0.,'z-axis rotation',IER,nbhelp)
        goto 61
      elseif(INO.EQ.26)then

C Set scale.
        CALL EASKR(YS,' ',' Data scale factor ?',
     &    0.,'F',0.,'-',0.,'data scale',IER,nbhelp)
        goto 61
      elseif(INO.EQ.27)then
        goto 2
      elseif(INO.EQ.28)then
        return
      else
        INO=-1
        goto 3
      endif
      goto 61

C Assign user-defined display file and write display information
C to file.
 2    CALL FPRAND(IUNIT,ISTAT,12,3,LNAME)
      IF(ISTAT.GE.0)goto 5
      goto 61
    5 IREC=1

C Make up title of plot with the menu text selected.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ITITLE
      IREC=IREC+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ITITLX,ITITLY,ITITLZ
      IREC=IREC+1
      NXPNTS=24*NTS
      if(NXPNTS.gt.100)then
        call edisp(iuout,
     &    'More than 100 data for X axis (timestep less than 15min)')
        INO=-1
        goto 3
      endif
      NZPNTS=IODF-IODS+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NXPNTS,NZPNTS
      IREC=IREC+1
      IC=0
      DO 10 I=1,NXPNTS
        IC=IC+1
        DATA(IC)=I
        IF(IC.EQ.10)goto 7
        goto 10
    7   WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(J),J=1,10)
        IREC=IREC+1
        IC=0
   10 CONTINUE
      IREM=MOD(NXPNTS,10)
      IF(IREM.GT.0)goto 8
      goto 9
    8 WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(J),J=1,IREM)
      IREC=IREC+1
    9 IC=0
      DO 20 I=IODS,IODF
        IC=IC+1
        DATA(IC)=I
        IF(IC.EQ.10)goto 11
        goto 20
   11   WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(J),J=1,10)
        IREC=IREC+1
        IC=0
   20 CONTINUE
      IREM=MOD(NZPNTS,10)
      IF(IREM.GT.0)goto 12
      goto 13
   12 WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(J),J=1,IREM)
      IREC=IREC+1

C For requested zone and for every computational time-step,
C get results (for complete day) for requested parameter.
   13 II=NZNO(1)

C Day by day recovery of results and transfer to
C display file.
      call usrmsg('Scanning data for range of values... ',' ','-')
      ISET=ISIM
      INIT = 0
      DO 30 I=IODS,IODF
        ID=I
        CALL CHKTIME(ID,ISTART,IEND)
        N = NTS * 24
        if(IGET.eq.1)then
          CALL GZAIRT(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.2)then
          CALL MOCLIM (ID,2)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.3)then
          CALL GTMCON(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.4)then
          CALL GTMS(ID,NZNO(1),NCNOUT,ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.5)then
          CALL GTMSO(ID,NZNO(1),NCNOUT,ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.6)then
          CALL CZRESL(ID,NZNO(1),ISET)
          CALL XTVAL1(XDUM1,ISTART,IEND,1)
        elseif(IGET.eq.7)then
          CALL GTMC(ID,NZNO(1),NCNOUT,NNDOUT,ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.8)then
          CALL GZQMH(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.9)then
          CALL GZQMC(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.10)then
          CALL GZQM(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.11)then
          CALL GQV1(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.12)then
          CALL GQV2(ID,NZNO(1),ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.15)then
          if(izver.eq.4)then

C If izver >= 4 the read the data directly.
            DO 202 J2=ISTART,IEND
              call getallcas(ID,NZNO(1),ISET,J2,QCASR,QCASC,QCASL,
     &          FRAC,perocupc,perlightc,equipc,otherc,perocupr,
     &          perlightr,equipr,otherr,perocupl,perlightl,equipl,
     &          otherl,theonectld)
              if(ICRCG.eq.1)then
                GVAL(J2)=QCASC        ! Total casual gain convective (W)
              elseif(ICRCG.eq.2)then
                GVAL(J2)=QCASR        ! Total casual gain radiant (W)
              endif
  202       continue
            CALL XTVAL1(GVAL,ISTART,IEND,1)
          else
            if(ICRCG.eq.1)then
              CALL GCASC(ID,NZNO(1),ISET)  ! for older versions
              CALL XTVAL1(GVAL,ISTART,IEND,1)
            elseif(ICRCG.eq.2)then
              CALL GCASR(ID,NZNO(1),ISET)  ! for older versions
              CALL XTVAL1(GVAL,ISTART,IEND,1)
            endif
          endif
        elseif(IGET.eq.16)then

C Surface convection inside.
          if(INEXSC.eq.1)then
            CALL CSCONV(ID,NZNO(1),NCNOUT,CQ)
            CALL XTVAL1(CQ,ISTART,IEND,1)
C Surface convection outside (only if save level 4).
          elseif(INEXSC.eq.2)then
            if(ISAVE.eq.4)then
              IG=9
              CALL G4FLUX(ID,NZNO(1),NCNOUT,IG,CQ)
            else
              call usrmsg('Current save level does not support the ',
     &         'recovery of convection at surfaces. Use save level 4.',
     &         '-')
              do 401 JTS=ISTART,IEND
                CQ(JTS)=0.0
  401         continue
            endif
            CALL XTVAL1(CQ,ISTART,IEND,1)
          elseif(INEXSC.eq.3)then

C Total convection inside.
            call GSURIC(ID,NZNO(1),ISET)
            CALL XTVAL1(XDUM,ISTART,IEND,1)
          endif
        elseif(IGET.eq.17)then
          CALL GQSE(ID,NZNO(1),NCNOUT,ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.18)then
          CALL GQSI(ID,NZNO(1),NCNOUT,ISET)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.19)then
          CALL MOCLIM (ID,19)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.20)then
          CALL MOCLIM (ID,20)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.21)then
          CALL MOCLIM (ID,21)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.22)then
          CALL MOCLIM (ID,22)
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.23)then

C Get comfort.
          if(ICI.eq.1)then
            CALL MOCMFT(ID,NZNO(1),0,'M')
          elseif(ICI.eq.2)then
            CALL MOCMFT(ID,NZNO(1),0,'E')
          elseif(ICI.eq.3)then
            CALL MOCMFT(ID,NZNO(1),0,'D')
          endif
          CALL XTVAL1(GVAL,ISTART,IEND,1)
        elseif(IGET.eq.24)then
          CALL CZMRT(ID,NZNO(1),ISET)
          CALL XTVAL1(XDUM1,ISTART,IEND,1)
        endif

C Provide an indication of data range.
        N = NTS * 24
        DO 310 J=1,N,NOUT
          IF (ID.LE.IODS.AND.(FLOAT(J)/NTS).LT.IOH1)goto 906
          IF(INIT.NE.0)then
            IF (VAL1(1,J) .GT. XXMAX) XXMAX = VAL1(1,J)
            IF (VAL1(1,J) .LT. XXMIN) XXMIN = VAL1(1,J)
          else
            XXMAX = VAL1(1,J)
            XXMIN = XXMAX
            INIT = 1
          endif
  906     CONTINUE
  310   CONTINUE

C ********

C Stepping at defined output interval.
        IC=0
        N=24*NTS
        DO 40 J=1,N
        IC=IC+1
        DATA(IC)=VAL1(1,J)
        IF(IC.EQ.10)goto 58
        goto 40
   58   WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(K),K=1,10)
        IC=0
        IREC=IREC+1
   40   CONTINUE
        IREM=MOD(N,10)
        IF(IREM.GT.0)goto 59
        goto 30
   59     WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(DATA(K),K=1,IREM)
      IREC=IREC+1
   30 CONTINUE

C Terminate the temporary file with one record beyond standard data.
      etxt = 'end record'
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000) etxt

      write(outs,'(a,f9.2,a,f9.2)') 'The data range is ',
     &  XXMIN,' to ',XXMAX
      call usrmsg('Scanning data for range of values...done.',outs,'P')
      call usrmsg('  ','  ','-')
      CALL EPWAIT

C Invoke drawing routines.
      CALL SURF3(IUNIT,XR,ZR,YS,1.,xxmin,xxmax,zlist)
      CALL EPWAIT
      CALL EFDELET(IUNIT,ISTAT)    
      call usrmsg(' ',' ','-')
      goto 61
 1000 WRITE(outs,62)IREC
   62 FORMAT(' MO3DPL: display file write error in record',I6)
      call edisp(iuout,outs)
      CALL EPWAIT

      RETURN
      END


C ******************** SURF3 ********************

C Plot a 3D surface representation of 2D arrays of time
C series data ie. timesteps-vs-day-vs-data. 
      SUBROUTINE SURF3(LU1,ROTNXX,ROTNYY,YS,ZS,xxmin,xxmax,zlist)
#include "building.h"
#include "geometry.h"

C Parameters passed
      integer LU1        ! file unit to read and write from
      real ROTNXX,ROTNYY ! rotation in X and Y axis
      real YS,ZS         ! scale factor for X and Y
      real xxmin,xxmax   ! data range minimum and maximum
      logical zlist      ! if true indicate zone name

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/ZONPIK/NZ,NZNO(MCOM)

C << Question about sizing of arrays. 3D plot crashes if there are many
C << points (e.g. the period exceeds some number of timesteps). Perhaps
C << the 100 below needs to be bigger? 
      REAL MARGIN
      PARAMETER ( MARGIN=75.0,BMARGN=55.0,IRANDR=1,
     &            NPR=10,IRSTART=3,ARRLEN=25.0,ARRWID=12.0 )
      INTEGER MASK(2000),IVER(12),IAXIS(3),IPOS(3)
      REAL XDATA(100),YDATA(100),ZDATA(100),VERTEX(16),
     1     YDATA1(1000),XLT(2),YLT(2),XYZARW(6,3),COEFF(5)

      CHARACTER LNAME*72,ETEXT*72,PDESCR*60,SDESCR*44
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER*40 TITLE,NAME
      CHARACTER*10 AXIS(3),ANAME,ANAME1
      character detail*72
      logical close
      real z1,x1  ! to avoid conflicts with G1 common

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont,iposi     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont,iposi     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      DATA LNAME/'MASK.DAT'/

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to viewtext.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call winclr
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif

C Setup and pass in parameters to win3d.
        iiw1=9; iiw2=16; iiw3=7; iiw4=2; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        igwid=igw
        igheight=igwh
        call startbuffer()
        call usrmsg(' ',' ','-')
      ENDIF

      WRITE(ETEXT,'(1X,A44,A,A20)')SDESCR,' Lib:',RFILE(1:20)
      iside=1
      isize=0
      ifont=1
      call viewtext(etext,iside,isize,ifont)

      WRITE(ETEXT,'(1x,A60)')PDESCR
      iside=2
      call viewtext(etext,iside,isize,ifont)

      ETEXT=' '
      if(zlist)then
        WRITE(ETEXT,'(A,I3,A,A,a,f9.2,a,f9.2)')' Zone (',NZNO(1),') ',
     &    zname(NZNO(1)),'    Data: ',  XXMIN,' to ',XXMAX
      else
        write(ETEXT,'(a,f9.2,a,f9.2)') ' The data range is ',
     &  XXMIN,' to ',XXMAX
      endif
      iside=4
      call viewtext(etext,iside,isize,ifont)

C Scaling happens here...
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,0.0,700.0,0.0,750.,xsc,ysc,sca,
     &  Xadd,Yadd)
      call linescale(iigl,Xadd,xsc,iigb,Yadd,ysc)

      READ(LU1,REC=1) TITLE
      READ(LU1,REC=2) (AXIS(I),I=1,3)
      READ(LU1,REC=3) NXPNTS,NZPNTS

      NAME=TITLE(1:40)
      iside=6
      isize=1
      ifont=2
      call viewtext(name,iside,isize,ifont)

C Make sure all rotations are positive.
      ROTNX=ABS(ROTNXX)
      ROTNY=ABS(ROTNYY)

      XSCALE=1.0

C Check to see if YS and ZS are non-zero.
      call eclose(YS,0.0,0.001,close)
      IF(close) GOTO 55
      call eclose(ZS,0.0,0.001,close)
      IF(close) GOTO 55

      YSCALE=YS
      ZSCALE=ABS(ZS)

C Calculate 3D rotation coefficients.
55    CALL COEF3D(ROTNX,ROTNY,COEFF)


C Open an ascii temporary file MASK.DAT to hold the first mask.
C Use logical unit LU2 for MASK.DAT.
      LU2=LU1+1
      CALL FPOPEN(LU2,ISTAT,3,3,LNAME)

C First figure. Generate data running parallel to X-axis.
      IREC=IRSTART+1

      CALL RWRAND(LU1,IRANDR,NPR,1,IREC,NXPNTS,XDATA)

C Store X 3D coordinates for axis arrow heads.
      XYZARW(1,1)=XDATA(NXPNTS)
      XYZARW(3,1)=XDATA(1)
      XYZARW(4,1)=XDATA(1)
      XYZARW(5,1)=XDATA(1)
      XYZARW(6,1)=XDATA(1)

      CALL RWRAND(LU1,IRANDR,NPR,1,IREC,NZPNTS,ZDATA)

C Store Z 3D coordinates for axis arrow heads.
      XYZARW(1,3)=ZDATA(1)
      XYZARW(2,3)=ZDATA(1)
      XYZARW(3,3)=ZDATA(1)
      XYZARW(4,3)=ZDATA(1)
      XYZARW(5,3)=ZDATA(NZPNTS)

C Set index to previous element.
      NX=NXPNTS/NPR
      NLEFT=NXPNTS-NX*NPR
      IF(NLEFT.NE.0) NX=NX+1

      NZ=NZPNTS/NPR
      NLEFT=NZPNTS-NZ*NPR
      IF(NLEFT.NE.0) NZ=NZ+1

      IREC1=IRSTART+NX+NZ+1

C If YS=0.0 then setup a reasonable scale.
      call eclose(YS,0.0,0.001,close)
      IF(.NOT.close) GOTO 59

      YMAX=0.0

      DO 58 ILINE=1,NZPNTS
        IREC=(ILINE-1)*NX+IREC1
        CALL RWRAND(LU1,IRANDR,NPR,1,IREC,NXPNTS,YDATA)

        DO 57 IPOINT=1,NXPNTS
          AYDATA=ABS(YDATA(IPOINT))
          IF(YMAX.LT.AYDATA) YMAX=AYDATA
57      CONTINUE
58    CONTINUE

      ZSCALE=ABS(XDATA(NXPNTS)/ZDATA(NZPNTS))
      IF(ZSCALE.GT.4.0) ZSCALE=4.0
      IF(ZSCALE.LT.0.25) ZSCALE=0.25

      YSCALE=ABS(XDATA(NXPNTS)/YMAX)
      IF(YSCALE.GT.4.0) YSCALE=4.0
      IF(YSCALE.LT.0.25) YSCALE=0.25

C Display rotation parameters & yscale and zscale.
59    WRITE(detail,'(A,F5.1,A,F5.1,A,F7.3,A,F7.3)')
     &   ' Rotation about X=',ROTNXX,', about Y=',ROTNYY,
     &   '  Yscale=',YSCALE,' Zscale=',ZSCALE
      iside=3
      isize=0
      ifont=1
      call viewtext(detail,iside,isize,ifont)

C Set up the plotting direction to produce the proper mask.
C Set vertices to be plotted by the frame routine into the
C array IVER().
      CALL SETQUD(ROTNY,IAXIS,IPOS,IZFRON,IZBACK,IZSTEP,
     &            IXFRON,IXBACK,IXSTEP,NX,NZPNTS,IV,IVER)

C Set variable IFIRST=PT on the line to be accessed first.
      IFIRST=1
      IF(IZSTEP.EQ.-1) IFIRST=NZPNTS

C Do two passes through lines drawn parallel to the X axis.
C Firstly, rotate and scale points on the line, storing max. 
C and min. X & Y values.
C Secondly, rotate, scale, mask and plot.
      DO 75 IPASS=1,2
        IALT=1
        DO 70 ILINE = IZFRON,IZBACK,IZSTEP
          IREC=(ILINE-1)*NX+IREC1
          CALL RWRAND(LU1,IRANDR,NPR,1,IREC,NXPNTS,YDATA)

          IF(ILINE.NE.1) GOTO 60

C Store 3D coordinates for axis arrow heads.
          XYZARW(1,2)=0.0
          XYZARW(2,2)=0.0
          XYZARW(3,2)=YDATA(1)
          XYZARW(5,2)=0.0
          XYZARW(6,2)=0.0

60        Z1=ZDATA(ILINE)

C Plot each line on the second pass only.
C Note: Z1 in PLOT3e should be an array of 100 elements but does not
C work if done that way.
          CALL PLOT3E(1110,XDATA,YDATA,Z1,XSCALE,YSCALE,ZSCALE,
     1                IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
     2                ILINE,IFIRST,NXPNTS,COEFF,MASK,VERTEX,
     3                XLT,YLT,IALT,XLAST,YLAST)
70      CONTINUE

        IF(IPASS.EQ.2) GOTO 78

C Include vertex coordinates lying in XZ plane
C in MAX & MIN coords.
        DO 72 J=9,15,2
          CALL MAXMIN(VERTEX(J),VERTEX(J+1),XMAX,XMIN,YMAX,YMIN)
72      CONTINUE

        BREDTH=XMAX-XMIN
        HEIGHT=YMAX-YMIN
        RATIOB=(1000.0-2.0*MARGIN)/BREDTH
        RATIOH=(780.0-2.0*MARGIN)/HEIGHT
        IF(RATIOB.GT.RATIOH) SCALE=RATIOH
        IF(RATIOH.GT.RATIOB) SCALE=RATIOB

        XMIN=XMIN-MARGIN/SCALE
        YMIN=YMIN-BMARGN/SCALE

C Scale arrow heads.
        ARRW=ARRWID/SCALE
        ARRL=ARRLEN/SCALE

        XYZARW(2,1)=XYZARW(1,1)-ARRL
        XYZARW(4,2)=XYZARW(3,2)-ARRL/YSCALE
        XYZARW(6,3)=XYZARW(5,3)-ARRL/ZSCALE
75    CONTINUE

78    DO 79 K=1,1999,2
        WRITE(LU2,*) MASK(K),MASK(K+1)
79    CONTINUE

C Second figure - data running parallel to Z axis.
      NPNTS=NZPNTS*NPR

      IFIRST=1
      IF(IXSTEP.EQ.-1) IFIRST=NXPNTS

      IALT=1

      DO 90 IX=IXFRON,IXBACK,IXSTEP
        IREC=IX-1+IREC1

        CALL RWRAND(LU1,IRANDR,NPR,NX,IREC,NPNTS,YDATA1)

        IX1=(IX-1)*NPR+1
        IX2=IX*NPR
        IX3=IX1-1
        IF(IX.EQ.NX) IX2=NXPNTS
        IF(IXSTEP.EQ.1) GOTO 82
        IX3=IX1
        IX1=IX2
        IX2=IX3
        IX3=IX2-1

82      DO 85 ILINE=IX1,IX2,IXSTEP
          IL=ILINE-IX3
          DO 83 I=1,NZPNTS
            YDATA(I)=YDATA1(IL)
            IL=IL+NPR
83        CONTINUE

C Call to plot3e, X1 below should be an array.
          X1=XDATA(ILINE)

C Plot each line.
          CALL PLOT3E(11,X1,YDATA,ZDATA,XSCALE,YSCALE,ZSCALE,
     1                IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
     2                ILINE,IFIRST,NZPNTS,COEFF,MASK,VERTEX,
     3                XLT,YLT,IALT,XLAST,YLAST)

C Next 4 lines are correct accoring to forchk, but do not seem to
C work so use original code above.
C      CALL  PLOT3E(11,XDATA,YDATA,ZDATA,XSCALE,YSCALE,ZSCALE,
C     1         IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
C     2         ILINE,IFIRST,NZPNTS,COEFF,MASK,VERTEX,
C     3         XLT,YLT,IALT,XLAST,YLAST)
85      CONTINUE
90    CONTINUE

C Create complete mask before drawing the frame.
      REWIND LU2

      DO 100 K=1,1999,2
        READ(LU2,*) MHIGH,MLOW
        IF(MHIGH.GT.MASK(K)) MASK(K)=MHIGH
        IF(MLOW.LT.MASK(K+1)) MASK(K+1)=MLOW
        IF(MASK(K+1).EQ.-1) MASK(K+1)=MLOW
100   CONTINUE

C Delete MASK.DAT
      CALL EFDELET(LU2,ISTAT)

C Draw a frame on the figure.
      CALL  FRAMER(IVER,VERTEX,MASK,IPASS,XMAX,XMIN,YMAX,YMIN,
     1             SCALE,COEFF)

C Return rotations to those input.
      CALL COEF3D(ROTNX,ROTNY,COEFF)

C Label axis.
      DO 150 I=1,3
        IY=2*IAXIS(I)
        IX=IY-1
        WRITE(ANAME,'(A10)') AXIS(I)
        ANAME1=ANAME
        iposi=ipos(i)
        isize=1
        call etlabel(ANAME1,VERTEX(IX),VERTEX(IY),iposi,isize)

C Draw arrow on axis.
        CALL FLARROW(XYZARW,2*I-1,ARRW,COEFF,SCALE,XMIN,YMIN,
     1            YSCALE,ZSCALE)

150   CONTINUE
      RETURN

      END

C ******************** RWRAND ********************

C Read and write binary random access file for drawing data.
      SUBROUTINE RWRAND(LU,IRW,NPR,ISTEP,IREC,NPNTS,ARRAY)

C Parameters:-
C  LU     = logical unit no.
C  IRW    = 1 for read and 2 for write
C  NPR    = no. of points per record
C  ISTEP  = step between each record
C  IREC   = record no. to be accessed first
C  NPNTS  = no. of points or elements to be transferred
C  ARRAY  = real array to hold transferred data

      DIMENSION ARRAY(1)

      NACCES=NPNTS/NPR
      NLEFT=NPNTS-NACCES*NPR

      IF(NLEFT.NE.0) NACCES=NACCES+1

      K1=1
      K2=NPR

      IF(IRW.EQ.1)THEN
        DO 150 IREAD=1,NACCES
          READ(LU,REC=IREC) (ARRAY(K),K=K1,K2)
          IREC=IREC+ISTEP
          K1=K2+1
          K2=K2+NPR
150     CONTINUE
        RETURN
      ELSEIF(IRW.EQ.2)THEN
        DO 250 IWRITE=1,NACCES
          WRITE(LU,REC=IREC) (ARRAY(K),K=K1,K2)
          IREC=IREC+ISTEP
          K1=K2+1
          K2=K2+NPR
250     CONTINUE
        RETURN
      ELSE
        call edisp(6,' irw parameter (1 or 2) not specified in cal ')
        RETURN
      ENDIF

      END

C ******************** COEF3D ********************

C Calculate rotation coefficients for a 2D representation
C of a 3D figure with rotations about the X & Y axis.
      SUBROUTINE COEF3D(ROTNX,ROTNY,COEFF)

      PARAMETER ( PID180=0.0174532925 )

      REAL COEFF(5)

C Convert rotation angles to radians.
      XRAD=PID180*ROTNX
      YRAD=PID180*ROTNY

      COEFF(1)=COS(YRAD)
      COEFF(2)=-SIN(YRAD)
      COEFF(3)=SIN(XRAD)*SIN(YRAD)
      COEFF(4)=COS(XRAD)
      COEFF(5)=SIN(XRAD)*COS(YRAD)

      RETURN
      END

C ******************** FRAMER ********************

C Draw the frame (context of the surface plot).
      SUBROUTINE FRAMER(IVER,VERTEX,MASK,
     &           IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,COEFF)

      INTEGER DRAW,MOVE,NOMARK
      PARAMETER ( DRAW=1,MOVE=0,NOMARK=0 )
      REAL COEFF(5),XLT(2),YLT(2)

      DIMENSION  VERTEX(16),MASK(2000),ARRAY(100),IVER(12)
      dimension  zdat(100)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      DO 2 I=1,5
        ARRAY(I)=VERTEX(2*IVER(I)-1)
        ARRAY(I+5)=VERTEX(2*IVER(I))
2     CONTINUE
      do 3 I=1,100
        zdat(I)=0.0
3     continue

      IPASS=2

C Set rotations to zero.
      CALL COEF3D(0.0,0.0,COEFF)
      IALT=0
      CALL PLOT3E(110,ARRAY,ARRAY(6),zdat,1.0,1.0,0.0,
     1     IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
     2          -1,0,5,COEFF,MASK,VERTEX,XLT,YLT,IALT,XLAST,YLAST)

C Should work with ydat, but doesn`t
C      CALL PLOT3E(110,ARRAY,ydat,zdat,1.0,1.0,0.0,
C     1     IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
C     2          -1,0,5,COEFF,MASK,VERTEX,XLT,YLT,IALT,XLAST,YLAST)

      DO 5 K=1,15,2
        VERTEX(K)=(VERTEX(K)-XMIN)*SCALE
        VERTEX(K+1)=(VERTEX(K+1)-YMIN)*SCALE
5     CONTINUE

      IY=2*IVER(6)
      IX=IY-1
      iupdown=MOVE
      isym=NOMARK
      call etplot(VERTEX(IX),VERTEX(IY),iupdown,isym)
      iupdown=DRAW
      DO 10 J = 7,10
        IY=2*IVER(J)
        IX=IY-1
        call etplot(VERTEX(IX),VERTEX(IY),iupdown,isym)
   10 CONTINUE

      IY=2*IVER(11)
      IX=IY-1
      iupdown=MOVE
      call etplot(VERTEX(IX),VERTEX(IY),iupdown,isym)

      IY=2*IVER(12)
      IX=IY-1
      iupdown=DRAW
      call etplot(VERTEX(IX),VERTEX(IY),iupdown,isym)

      RETURN
      END

C ******************** MASKED ********************

C Enters edge of figure into array mask as the figure is drawn.
      SUBROUTINE MASKED(XL,YLAST,XN,YN,MASK)
      logical close
      INTEGER HIGH,MASK(2000)

      call eclose(XN,XL,0.001,close)
      IF(close) RETURN
      YL=YLAST

      YINC=(YN-YL)/ABS(XN-XL)
      ISTEP=int((XN-XL)/ABS(XN-XL))

      JX1=INT(XL)+ISTEP
      JX2=INT(XN)-ISTEP

      DO 50 JXX=JX1,JX2,ISTEP
        LOW=JXX+JXX
        HIGH=LOW-1

        YL=YL+YINC
        LY=int(YL+0.5)

C Check to see if any mask exists.
        IF(MASK(LOW).LT.0) GOTO 40

        IF(MASK(LOW).GT.LY) MASK(LOW)=LY
        IF(MASK(HIGH).LT.LY) MASK(HIGH)=LY
        GOTO 50

40      MASK(LOW)=LY
        MASK(HIGH)=LY
50    CONTINUE

      RETURN
      END

C ******************** MAXMIN ********************

C Return maximum and minimum X & Y coords.
      SUBROUTINE MAXMIN(X,Y,XMAX,XMIN,YMAX,YMIN)
      logical close

      dx=xmin-x
      if(dx.lt.0.)goto 172
      call eclose(dx,0.0,0.001,close)
      if(close)goto 172
      if(dx.gt.0.)goto 171

C >0 SET
171     XMIN=X
        GOTO 174
172   dx=xmax-x
      if(dx.lt.0.)goto 173
      call eclose(dx,0.0,0.001,close)
      if(close)goto 174
      if(dx.gt.0.)goto 174

C <0 SET
173     XMAX=X
174   dy=ymin-y
      if(dy.lt.0.)goto 176
      call eclose(dy,0.0,0.001,close)
      if(close)goto 176
      if(dy.gt.0.)goto 175

C >0 SET
175     YMIN=Y
        GOTO 178
176   dy=ymax-y
      if(dy.lt.0.)goto 177
      call eclose(dy,0.0,0.001,close)
      if(close)goto 178
      if(dy.gt.0.)goto 178

C <0 SET
177     YMAX=Y

178   RETURN

      END

C ******************** PLOT3E ********************

      SUBROUTINE  PLOT3E(IVXYZ,XDATA,YDATA,ZDATA,XSCALE,YSCALE,ZSCALE,
     1              IPASS,XMAX,XMIN,YMAX,YMIN,SCALE,II,JVXYZ,INCI,
     2              NLINE,IFIRST,NPNTS,COEFF,MASK,VERTEX,
     3              XLT,YLT,IALT,XLAST,YLAST)
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      INTEGER DRAW,HIGH,MOVE,NOMARK
      PARAMETER ( DRAW=1,MOVE=0,NOMARK=0 )

      REAL XLT(2),YLT(2),COEFF(5)
      logical close
      DIMENSION  XDATA(100),YDATA(100),ZDATA(100),MASK(2000),VERTEX(16)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      IF(NLINE.EQ.-1) GOTO 15
      IF (NLINE .NE. IFIRST)  GOTO  20
      LIMITX =1000
      I = LIMITX + LIMITX
      DO 10 K = 1,I
        MASK(K) = -1
   10 CONTINUE

15    INCI = -1
      JVXYZ=-1
      II = 0
   20 IF (JVXYZ .EQ. IVXYZ)  GOTO  80
      JVXYZ = IVXYZ
      INDX = 1
      INDY = 1
      INDZ = 1
      INDV = 1
      IF (JVXYZ .LT. 1000)  GOTO  30
      INDV = 2
      JVXYZ = JVXYZ -1000

   30 IF (JVXYZ .LT. 100)  GOTO  40
      INDX = 2
      JVXYZ = JVXYZ -100

   40 IF (JVXYZ .LT. 10)  GOTO  50
      INDY = 2
      JVXYZ = JVXYZ -10

   50 IF (JVXYZ .LT. 1)  GOTO  60
      INDZ = 2
   60 JVXYZ = IVXYZ
   80 INCI = -INCI
      IF (II .NE. 0)  II = NPNTS + 1

C Need to test limits for the XDATA and YDATA array.
      DO 530 K = 1,NPNTS
        II=II + INCI
        GOTO  (90,100),INDX
   90   X = XDATA(1)*XSCALE
        GOTO  110
  100   X = XDATA(II)*XSCALE
  110   GOTO (120,130),INDY
  120   Y = YDATA(1)*YSCALE
        GOTO  140
  130   Y = YDATA(II)*YSCALE
  140   GOTO (150,160),INDZ
  150   Z = ZDATA(1)*ZSCALE
        GOTO  170
  160   Z = ZDATA(II)*ZSCALE
  170   XXX = COEFF(1)*X + COEFF(2)*Z
        XX = XXX
        YYY = COEFF(3)*X + COEFF(5)*Z
        YY = YYY + COEFF(4)*Y

        IF(IPASS.EQ.2) GOTO 180

        IF(NLINE.NE.IFIRST) GOTO 172
        IF(K.GT.1) GOTO 172
        XMAX=XX
        XMIN=XX
        YMAX=YY
        YMIN=YY
        GOTO 245

172     CALL MAXMIN(XX,YY,XMAX,XMIN,YMAX,YMIN)

        IF(K.NE.1) GOTO 530
        GOTO 245

180     IX=int((XX-XMIN)*SCALE+0.5)
        IY=int((YY-YMIN)*SCALE+0.5)

        IF (K .NE. 1)  GOTO  250
        LOW = IX + IX
        HIGH = LOW -1
        MLOW = MASK(LOW)
        MHIGH = MASK(HIGH)

        X=IX
        Y=IY
        LASTPT=0

        IF(MHIGH.LT.0) GOTO 230

C Mask exists.
        IF(MHIGH.LT.IY) GOTO 220
        IF(MLOW.LE.IY) GOTO 232

C New mask - low.
        MASK(LOW)=IY
        LOC=0
        GOTO 235

C New mask - high.
220     MASK(HIGH)=IY
        LOC=1
        GOTO 235

C No mask at this point.
230     MASK(HIGH)=IY
        MASK(LOW)=IY
        GOTO 235

C Inside mask.
232     LASTPT=-2

235     IF(NLINE.EQ.-1) GOTO 240
        IALT=1-IALT*IALT
        JALT=IALT+1
        XLT(JALT)=X
        YLT(JALT)=Y

        IF(LASTPT.EQ.-2) GOTO 242

        IF(NLINE.NE.IFIRST) CALL MASKED(XLAST,YLAST,X,Y,MASK)

        GOTO 240

C Remember latest point.
240     ML=MLOW
        MH=MHIGH
242     XLAST=IX
        YLAST=IY

C Move back to first point on the line and store that point.
        iupdown=MOVE
        isym=NOMARK
        call etplot(X,Y,iupdown,isym)
        JX=IX
        JY=IY

        IF (INDV .EQ. 1)  GOTO  530
245     INDEX = INCI + 6
        VERTEX(INDEX) = XX
        VERTEX(INDEX+1) = YY
        VERTEX(INDEX+8) = XXX
        VERTEX(INDEX+9) = YYY

        IF(NLINE.NE.IFIRST) GOTO 530
        VERTEX(1) = XX
        VERTEX(2) = YY
        VERTEX(9) = XXX
        VERTEX(10) = YYY
        GOTO  530

250     INCX=IX-JX
        INCY=IY-JY
        INCXA=IABS(INCX)
        INCYA=IABS(INCY)
        XINC=0.0
        YINC=0.0

        IF(INCXA.EQ.0) GOTO 260

        XINC=INCX/FLOAT(INCXA)
        YINC=INCY/FLOAT(INCXA)
260     XJ=JX
        YJ=JY
        NINC=INCXA

        HALFX=0.0
        HALFY=0.5
        IF(INCYA.EQ.0) HALFY=0.0

        IF(INCXA.GE.INCYA) GOTO 270
        YINC=INCY/FLOAT(INCYA)
        XINC=INCX/FLOAT(INCYA)
        NINC=INCYA

        HALFX=0.5
        IF(INCXA.EQ.0) HALFX=0.0
        HALFY=0.0

270     DO 525 K1=1,NINC

        XJ=XJ+XINC
        JX=int(XJ+HALFX)
        YJ=YJ+YINC
        JY=int(YJ+HALFY)

        LOW = JX+JX
        HIGH = LOW -1
        MLOW = MASK(LOW)
        MHIGH = MASK(HIGH)

        IF(MHIGH.LT.0) GOTO 290

C Mask exists.
        IF(MHIGH.LT.JY) GOTO 280
        IF(MLOW.LE.JY) GOTO 400

C New mask - low.
        MASK(LOW)=JY
        LOC=0
        GOTO 300

C New mask - high.
280     MASK(HIGH)=JY
        LOC=1
        GOTO 300

C No mask exists at this point.
290     MASK(HIGH)=JY
        MASK(LOW)=JY

C Check to see if just emerged from mask.
300     ML=MLOW
        MH=MHIGH
        if(LASTPT.EQ.-2)then
          iupdown=MOVE
          isym=NOMARK
          call etplot(XLAST,YLAST,iupdown,isym)
        endif
        LASTPT=0

C Outside of the mask.
        GOTO 520

C Inside the mask, but check in case point has disappeared.
400     IF(LASTPT.EQ.-2) GOTO 520
        X=JX
        call eclose(xlast,x,0.001,close)
        IF(.NOT.close) GOTO 450

C Masking has just occurred by drawing vertically towards the mask.
        IF(LOC.NE.1) GOTO 430

C Between new HIGH mask and old HIGH mask.
        IF(MH.EQ.JY) GOTO 450
        GOTO 520

C Between new LOW mask and old LOW mask.
430     IF(ML.GT.JY) GOTO 520

C Just entered the mask.
450     Y=JY
        iupdown=DRAW
        isym=NOMARK
        call etplot(X,Y,iupdown,isym)
        LASTPT=-2

520     XLAST=JX
        YLAST=JY

525     CONTINUE

C Finish drawing the line if required.
        IF(LASTPT.EQ.-2) GOTO 530
        iupdown=DRAW
        isym=NOMARK
        call etplot(XLAST,YLAST,iupdown,isym)

530   CONTINUE

      IF(NLINE.EQ.IFIRST) GOTO 535
      IF(IPASS.EQ.1) GOTO 535

      IF(NLINE.EQ.-1) GOTO 535

      IALT=1-IALT*IALT
      JALT=IALT+1

      CALL MASKED(XLT(JALT),YLT(JALT),XLAST,YLAST,MASK)

      IALT=1-IALT*IALT

535   IF (INDV .EQ. 1)  GOTO  540
      INDEX = -INCI + 6

      VERTEX(INDEX) = XX
      VERTEX(INDEX+1) = YY
      VERTEX(INDEX+8) = XXX
      VERTEX(INDEX+9) = YYY

      IF(NLINE.NE.IFIRST) GOTO 540
      VERTEX(3) = XX
      VERTEX(4) = YY
      VERTEX(11) = XXX
      VERTEX(12) = YYY
540   II=II-1

      RETURN

      END

C ******************** ROTN3D ********************

C Scale and rotate a 3D array to produce 2D representation.
      SUBROUTINE ROTN3D(YSCALE,ZSCALE,COEFF,XYZ,N)

      REAL COEFF(5),XYZ(10,3)

      IF(N.GT.10)THEN
        call edisp(6,'0 error in call to ROTN3D :- ')
        call edisp(6,'5th. parameter is > 10 ')
        RETURN
      ENDIF

      DO 100 I=1,N
        XYZ(I,2)=XYZ(I,2)*YSCALE
        XYZ(I,3)=XYZ(I,3)*ZSCALE

        XX=COEFF(1)*XYZ(I,1)+COEFF(2)*XYZ(I,3)
        YY=COEFF(3)*XYZ(I,1)+COEFF(4)*XYZ(I,2)+COEFF(5)*XYZ(I,3)

        XYZ(I,1)=XX
        XYZ(I,2)=YY
100   CONTINUE
      RETURN
      END

C ******************** SETQUD ********************

C Set default for drawing frame.
      SUBROUTINE SETQUD(ROTNY,IAXIS,IPOS,IZFRON,IZBACK,IZSTEP,
     1                  IXFRON,IXBACK,IXSTEP,NX,NZPNTS,IV,IVER)

      INTEGER IAXIS(3),IPOS(3),IVER(12),HIDEDG

      IV=0
      IAXIS(1)=7
      IAXIS(2)=4
      IAXIS(3)=5

      IZFRON=1
      IZBACK=NZPNTS
      IZSTEP=1
      IXFRON=1
      IXBACK=NX
      IXSTEP=1

      HIDEDG=3

      IPOS(1)=3
      IPOS(2)=3
      IPOS(3)=1

      II = int(ABS(ROTNY)/90.+1.)
      IF(II.EQ.1)THEN
        IAXIS(1)=6
        IAXIS(2)=1
        IAXIS(3)=8
      ELSEIF(II.EQ.2)THEN
        IZFRON=NZPNTS
        IZBACK=1
        IZSTEP=-1
        IPOS(1)=4
        IPOS(2)=3
        IPOS(3)=2
      ELSEIF(II.EQ.3)THEN
        IZFRON=NZPNTS
        IZBACK=1
        IZSTEP=-1
        HIDEDG=4
        IXFRON=NX
        IXBACK=1
        IXSTEP=-1
        IV=1
        IPOS(1)=1
        IPOS(2)=4
        IPOS(3)=3
      ELSEIF(II.EQ.4)THEN
        IXFRON=NX
        IXBACK=1
        IXSTEP=-1
        HIDEDG=4
        IV=1
        IPOS(1)=2
        IPOS(2)=1
        IPOS(3)=4
        IAXIS(1)=6
        IAXIS(2)=1
        IAXIS(3)=8
      ENDIF

      IVER(1)=3+IV
      IVER(2)=7+IV
      IVER(3)=6+IV
      IVER(4)=7+IV
      IVER(5)=8-3*IV

      IVER(6)=2+IV
      IVER(7)=6+IV
      IVER(8)=5+IV
      IVER(9)=8-3*IV
      IVER(10)=4-3*IV

      IVER(11)=1+IV
      IVER(12)=5+IV

      RETURN
      END

C ******************** FLARROW ********************
C FLARROW Draw flat arrows. ie a 2D representation of a 3D figure.
      SUBROUTINE FLARROW(XYZARW,JAXIS,ARRW,COEFF,SCALE,XMIN,YMIN,
     1           YSCALE,ZSCALE)

      INTEGER MOVE,DRAW,NOMARK
      logical close
      PARAMETER ( MOVE=0,DRAW=1,NOMARK=0 )

      REAL XYZARW(6,3),XYZ(10,3),COEFF(5)
#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

C Define arrow head.
      JPLUS1=JAXIS+1
      XDIFF=XYZARW(JAXIS,1)-XYZARW(JPLUS1,1)
      call eclose(XDIFF,0.0,0.001,close)
      IF(close) GOTO 10
      A=ATAN((XYZARW(JAXIS,3)-XYZARW(JPLUS1,3))/XDIFF)
      GOTO 20

10    A=90.0*0.0174532925

20    W=ARRW/2.0

      XYZ(1,1)=XYZARW(JAXIS,1)
      XYZ(1,2)=XYZARW(JAXIS,2)
      XYZ(1,3)=XYZARW(JAXIS,3)

      XYZ(2,1)=XYZARW(JPLUS1,1)+W*SIN(A)
      XYZ(2,2)=XYZARW(JPLUS1,2)
      XYZ(2,3)=XYZARW(JPLUS1,3)+W*COS(A)/ZSCALE

      XYZ(3,1)=XYZARW(JPLUS1,1)-W*SIN(A)
      XYZ(3,2)=XYZARW(JPLUS1,2)
      XYZ(3,3)=XYZARW(JPLUS1,3)-W*COS(A)/ZSCALE

      XYZ(4,1)=XYZARW(JAXIS,1)
      XYZ(4,2)=XYZARW(JAXIS,2)
      XYZ(4,3)=XYZARW(JAXIS,3)

C Scale and then rotate array xyz.
      CALL ROTN3D(YSCALE,ZSCALE,COEFF,XYZ,4)

C Convert to full screen size.
      DO 50 I=1,4
        XYZ(I,1)=(XYZ(I,1)-XMIN)*SCALE
        XYZ(I,2)=(XYZ(I,2)-YMIN)*SCALE
50    CONTINUE

C Move to point of arrow.
      iupdown=MOVE
      isym=NOMARK
      call etplot(XYZ(1,1),XYZ(1,2),iupdown,isym)

C Draw rest of arrow.
      iupdown=DRAW
      DO 70 I=2,4
        call etplot(XYZ(I,1),XYZ(I,2),iupdown,isym)
70    CONTINUE

      RETURN
      END
