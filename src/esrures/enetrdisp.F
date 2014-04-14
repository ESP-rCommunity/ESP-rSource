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

C netgrph.F contains generic graph plotting routines, allowing data from non-building 
C simulations to be plotted. These routines are as follows: 
C 
C ENETGRPH - deals with all aspects of graph drawing.
C ENETTBL  - displays tabular output, up to 11 columns of data can be listed. 
C ENETSTAT - displays statistical output. <to be added ...>
C ENETPQAL - displays a power quality assessment. <to be added ...>
C MOGEHED  - writes out header information for graphs.
C ENRFILT  - alows filtering of selected data for the graphical display.
C
C All the routines are called in various modes. These are as follows:
C 'I' - initialise the graph: initialise the axis, prescan data to get the minimum 
C       and maximum values for each axis and then draw then. 
C 'U' - updates the graph with data read from the results library.
C
C
      SUBROUTINE ENETGRPH(MODE)

#include "power.h"
      
      integer lnblnk  ! function definition

C View parameters
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/MENUCHW,IGL,IGR,IGT,IGB,IGW,IGWH
      COMMON/GRAF1/YMAX(5),YMIN(5),YAXSET(5),ADDLIN,horaxisdiv
      COMMON/NETPLOT/XSTRT,XFIN,ILINST(MERAWD),AXSC(5)

C Array of recovered data
      COMMON/ERECOV/NVAL1,NVAL2,EVAL1(MERAWD),EVAL2(MERAWD)
      COMMON/ERECOVD/EVAXIS(MERAWD),EVMARK(MERAWD),EVLINE(MERAWD)
      COMMON/ERECOVL/EVLABS(MERAWD),EVLABU(MERAWD)

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C Data filters
      COMMON/ENFILT/EFILT(MERAWD)

C The period details
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/NETTS/ITSTPS,ITSTPF,NTSTP

      DIMENSION YSC(5),YOLD(MERAWD),TIMEOLD(MERAWD),IGX(5),
     &ILR(5),NETXLAB(5),YADD(5)

      CHARACTER*1 MODE
      CHARACTER*12 NETXLAB
      CHARACTER EVLABS*16,EVLABU*4,STR*20,EXLAB*5

      REAL  VNEG,VPOS,YOLD,TIMEOLD
      INTEGER YAXSET,ADDLIN,horaxisdiv,EVAXIS,EVMARK,EVLINE,EFILT
#ifdef OSI
      integer igx             ! see above array
      integer igwid,igheight  ! for use with axiscale
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
      integer iid1,iid2
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igx             ! see above array
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iid1,iid2
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C Set up scaling and graph parameters
      VPOS=1E+15
      VNEG=-1E+15

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Convert the start and finish times into timesteps and therefore record numbers. 
C Firstly calculate the period start and finish days.
      CALL EDAY(IOD1,IOM1,IOPS)
      CALL EDAY(IOD2,IOM2,IOPF)
      IF(IOH1.NE.0)THEN
        ITSTPS=1+((IOPS-ISDS)*24+IOH1-1)*NETS
      ELSE
        ITSTPS=1+((IOPS-ISDS)*24+IOH1)*NETS
      ENDIF
      ITSTPF=1+((IOPF-ISDS)*24+IOH2)*NETS 
      NTSTP=ITSTPF-ITSTPS

      IF(MODE.EQ.'I') THEN
        IF(ADDLIN.EQ.0)THEN

C Initialise the axes of the graph (skip if the addlin flag is active).      
          DO 10 I=1,5
            YAXSET(I)=0
            AXSC(I)=1.
            YMIN(I)=VPOS
            YMAX(I)=VNEG
            NETXLAB(I)=' '
  10      CONTINUE
        ENDIF 
        DO 20 I=1,MERAWD
          YOLD(I)=0.0
          TIMEOLD(I)=0.0
          ILINST(I)=0
  20    CONTINUE


        IF(ADDLIN.EQ.0)THEN
C Change the graph scaling based on the currently read data
          CALL USRMSG('Scanning data for range of values...',' ','-')

C Loop through each timestep and recover the data into the EVAL1 array, scale
C the graph's axes according to this prescan.
          IAVERF=1
          DO 30 ITS=ITSTPS,ITSTPF

C Read the selected data from the library at each time step.
            IGT=ITS
            CALL ENETGET(IGT,'-')

C Average the data if required.
            DO 40 J=1,NVAL1
              IF(EFILT(J).EQ.1)THEN
                IF(NOUT.EQ.1) THEN

C No averaging.
                  EVAL2(J)=EVAL1(J)
              ELSE
                  IF(IAVERF.EQ.1) THEN

C Add half of first data period to the average. 
                    EVAL2(J)=EVAL1(J)/2.
                  ELSEIF(IAVERF.EQ.NOUT+1)THEN

C Add half of the last data period to the average and take the average
C or if the averaging flag is not set use the timestep value.
                    EVAL2(J)=EVAL2(J)+EVAL1(J)/2.
                    EVAL2(J)=EVAL2(J)/FLOAT(NOUT+1)
                    IF(IAV.EQ.1) EVAL2(J)=EVAL1(J)              
                  ELSE

C For intermediate periods add the whole value.
                    EVAL2(J)=EVAL2(J)+EVAL1(J)
                  ENDIF
                ENDIF
              ENDIF       
  40        CONTINUE
            IF(IAVERF.LT.NOUT+1.AND.NOUT.NE.1)THEN
              IAVERF=IAVERF+1 
            ELSE

C Reset the averaging flag.
              IAVERF=1
              DO 50 J=1,NVAL1
                IF(EFILT(J).EQ.1)THEN

C Maximum value for each axis. 
                  IF(EVAL2(J).GT.YMAX(EVAXIS(J)))THEN
                    YMAX(EVAXIS(J))=EVAL2(J)
                  ENDIF

C Mininmum value for each axis. 
                  IF(EVAL2(J).LT.YMIN(EVAXIS(J)))THEN
                    YMIN(EVAXIS(J))=EVAL2(J)
                  ENDIF 
                ENDIF
  50          CONTINUE
            ENDIF 
  30      CONTINUE

C Loop through the selection data and if a data item is associated with an 
C axis then turn the axis flag on also if the axis was prexiously off then
C increment the axis counter. 
          DO 60 I=1,NVAL1
            IF(EFILT(I).EQ.1)THEN
              AYX=ABS(YMAX(EVAXIS(I)))
              AYM=ABS(YMIN(EVAXIS(I)))
              AMY=MAX(AYX,AYM)
              IF(EVAXIS(I).EQ.1.AND.YAXSET(1).EQ.0)THEN

C Initialise axes for voltage display
                YAXSET(1)=1
                NETXLAB(1)='Voltage V'
                IF(AMY.GT.1000..AND.AMY.LE.1000000.)THEN
                  AXSC(1)=1000.
                  NETXLAB(1)='Voltage kV'
                ELSEIF(AMY.GT.1000000.)THEN
                  AXSC(1)=1000000.  
                  NETXLAB(1)='Voltage MV'           
                ENDIF
                YMAX(1)=YMAX(1)/AXSC(1)
                YMIN(1)=YMIN(1)/AXSC(1)  
              ELSEIF(EVAXIS(I).EQ.2.AND.YAXSET(2).EQ.0)THEN

C Initialise axes for current display
                YAXSET(2)=1
                NETXLAB(2)='Current A'
                IF(AMY.GT.1000..AND.AMY.LE.1000000.)THEN
                  AXSC(2)=1000.
                  NETXLAB(2)='Current kA'
                ELSEIF(AMY.GT.1000000.)THEN
                  AXSC(2)=1000000.  
                  NETXLAB(2)='Current MA'           
                ENDIF
                YMAX(2)=YMAX(2)/AXSC(2)
                YMIN(2)=YMIN(2)/AXSC(2)  
              ELSEIF(EVAXIS(I).EQ.3.AND.YAXSET(3).EQ.0)THEN

C Initialise axes for power display
                YAXSET(3)=1
                NETXLAB(3)='Power W/VAR'
                IF(AMY.GT.1000..AND.AMY.LE.1000000.)THEN
                  AXSC(3)=1000.
                  NETXLAB(3)='Pwr kW/kVAR'
                ELSEIF(AMY.GT.1000000.)THEN
                  AXSC(3)=1000000.  
                  NETXLAB(3)='Pwr MW/MVAR'            
                ENDIF
                YMAX(3)=YMAX(3)/AXSC(3)
                YMIN(3)=YMIN(3)/AXSC(3)  
              ELSEIF(EVAXIS(I).EQ.4.AND.YAXSET(4).EQ.0)THEN

C Initialise axes for misc. display
                YAXSET(4)=1
                NETXLAB(4)='Misc'
                IF(AMY.GT.1000..AND.AMY.LE.1000000.)THEN
                  AXSC(4)=1000.
                  NETXLAB(4)='Misc/1E+03'
                ELSEIF(AMY.GT.1000000.)THEN
                  AXSC(4)=1000000.  
                  NETXLAB(4)='Misc/1E+06'          
                ENDIF
                YMAX(4)=YMAX(4)/AXSC(4)
                YMIN(4)=YMIN(4)/AXSC(4) 
              ENDIF
            ENDIF
  60      CONTINUE
        ENDIF

C Draw the graph's axes.
        CALL MOGEHED

C Set the axis side voltage on the left, current, power and pf on the right. 
        ILR(1)=0
        ILR(2)=1
        ILR(3)=1
        ILR(4)=1 

        IGX(1)=IGL
        IGX(2)=IGR+140
        IGX(3)=IGR+80
        IGX(4)=(IGL+IGR)/2

C Draw the graph with the scaled axes. Set the axis positions.
        IF (YAXSET(2).GT.0) THEN
          IF (YAXSET(3).GT.0) THEN

C Axis 2,3 active, 4 either active or not still in center.
            IGX(2)=IGR+140
            IGX(3)=IGR+80
            IGX(4)=(IGL+IGR)/2
          ELSE

C Axis 2,4 active.
            IF (YAXSET(4).GT.0) THEN
              IGX(2)=IGR+90
              IGX(4)=IGR+145
            ELSE

C Axis 2 active.
              IGX(2)=IGR+120
            ENDIF
          ENDIF
        ELSE
          IF (YAXSET(3).GT.0) THEN
            IF (YAXSET(4).GT.0) THEN

C Axis 3, 4 active.
              IGX(3)=IGR+90
              IGX(4)=IGR+145
            ELSE

C Axis 3 only active.
              IGX(3)=IGR+120
            ENDIF
          ELSE

C Axis 4 only active or not. 
            IGX(4)=IGR+120
          ENDIF
        ENDIF

C Plot the X-axis, firstly get the start and finish hours. 
        IF(IOH1.GT.0) THEN
          XSTRT=FLOAT(IOH1-1)
        ELSE
          XSTRT=FLOAT(IOH1)
        ENDIF
        XFIN=FLOAT(((IODF)*24+IOH2)-(IODS)*24)


C Plot the Yaxes
        DO 70 I=1,5
          IF(YAXSET(I).GT.0)THEN
            IF(ADDLIN.EQ.0)THEN
              IF (YMAX(I).LT.0.) THEN
                YMAX(I)=YMAX(I)*0.985
              ELSE
                YMAX(I)=YMAX(I)*1.015
              ENDIF
              IF (YMIN(I).LT.0.) then
                YMIN(I)=YMIN(I)*1.015
              ELSE
                YMIN(I)=YMIN(I)*0.985
              ENDIF

C Round YMIN to a sensible value.
              IF (YMIN(I).LT.0.0) THEN
                IF (YMIN(I).GT.-1.0) then
                  YMIN(I)=FLOAT(INT(YMIN(I)*10.)-1)/10.
                ELSEIF (YMIN(I).GT.-10.0) THEN
                  YMIN(I)=FLOAT(INT(YMIN(I))-1)
                ELSEIF (YMIN(I).GT.-100.0)THEN
                  YMIN(I)=10.*FLOAT((INT(YMIN(I))/10)-1)
                ELSEIF (YMIN(I).GT.-1000.0) THEN
                  YMIN(I)=100.*FLOAT((INT(YMIN(I))/100)-1)
                ELSE
                  YMIN(I)=1000.*FLOAT((INT(YMIN(I))/1000)-1)
                ENDIF
              ELSE
                IF (ABS(YMAX(I)-YMIN(I)).LT.10.0) THEN
                  YMIN(I)=10.*FLOAT(INT(YMIN(I))/10)-1.
                ELSEIF (ABS(YMAX(I)-YMIN(I)).LT.100.0) THEN
                  YMIN(I)=10.*FLOAT(INT(YMIN(I))/10)-10.
                ELSEIF (ABS(YMAX(I)-YMIN(I)).LT.1000.0) then
                  YMIN(I)=100.*FLOAT(INT(YMIN(I))/100)-100.
                ELSE
                  YMIN(I)=1000.*FLOAT(INT(YMIN(I))/1000)
                ENDIF
              ENDIF
              IF (ABS(YMAX(I)-YMIN(I)).LT.0.00001) YMAX(I)=YMAX(I)+0.5
            ENDIF
 
C Set-up frequency of labels (DT), number of decimal places (NDEC) and 
C draw vertical axis.
            GMIN=YMIN(I)
            GMAX=YMAX(I)
            CALL DINTERVAL(XSTRT,XFIN,DT,NDEC,1)
            igwid=igw
            igheight=igwh
            call axiscale(igwid,igheight,XSTRT,XFIN,GMIN,GMAX,
     &                           TSC,YSC(I),SCA,XADD,YADD(I))
            IF (ILR(I).EQ.0) then
              CALL VRTAXIS(GMIN,GMAX,IGX(I),iigb,iigt,YADD(I),
     &                             YSC(I),0,ILR(I),NETXLAB(I))
            ELSE
              CALL VRTAXIS(GMIN,GMAX,IGX(I),iigb,iigt,YADD(I),
     &                         YSC(I),0,ILR(I),'  ')
              iid1=IGX(I); iid2=igt-5;
              CALL WSTXPT(iid1,iid2,NETXLAB(I))
            ENDIF           
          ENDIF

C Reset the scaling flag if required
          IF(I.EQ.4.AND.ADDLIN.EQ.1) ADDLIN=0  
  70    CONTINUE 

C Draw horizontal (time) axis.
        CALL HORAXIS(XSTRT,XFIN,iigl,iigr,iigb,XADD,TSC,1,
     &    'Time Hrs')
     
      ELSEIF(MODE.EQ.'U')THEN

C Calculate a spacing for the plotting of symbols.
        IF(NETS.GE.NOUT)THEN
          NOPHR=NETS/NOUT
        ELSE
          NOPHR=1
        ENDIF
        LSSPAC=NOPHR*3

C Set averaging and symbol plotting flags
        ISSPAC=0
        IAVERF=1

C Loop through each timestep and plot the data.
        DO 80 ITS=ITSTPS,ITSTPF

C Increment the symbol plotting flag
          IF(ISSPAC.EQ.LSSPAC.OR.ISSPAC.EQ.0)THEN
            ISSPAC=1
          ELSE
            ISSPAC=ISSPAC+1
          ENDIF


C Read the selected data from the library at each time step.
          IGT=ITS
          CALL ENETGET(IGT,'-')

C Average the data if required.
          DO 90 I=1,NVAL1
            IF(EFILT(I).EQ.1)THEN
              IF(NOUT.EQ.1) THEN

C No averaging.
                EVAL2(I)=EVAL1(I)/AXSC(EVAXIS(I))
              ELSE
                IF(IAVERF.EQ.1) THEN

C Add half of first data period to the average. 
                  EVAL2(I)=EVAL1(I)/(2.*AXSC(EVAXIS(I)))
                ELSEIF(IAVERF.EQ.NOUT+1)THEN

C Add half of the last data period to the value and take the average.
                  EVAL2(I)=EVAL2(I)+EVAL1(I)/(2.*AXSC(EVAXIS(I)))
                  EVAL2(I)=EVAL2(I)/FLOAT(NOUT+1) 
                  IF(IAV.EQ.1) EVAL2(I)=EVAL1(I)/AXSC(EVAXIS(I))               
                ELSE

C For intermediate periods add the whole value.
                  EVAL2(I)=EVAL2(I)+EVAL1(I)/AXSC(EVAXIS(I))
                ENDIF
              ENDIF 
            ENDIF       
  90      CONTINUE

          IF(IAVERF.LT.NOUT+1.AND.NOUT.NE.1)THEN
            IAVERF=IAVERF+1 
          ELSE

C Reset the averaging flag. 
            IAVERF=1
            DO 100 I=1,NVAL1
              IF(EFILT(I).EQ.1)THEN
C Scale the recovered data to the appropriate axis.
                IAX=EVAXIS(I)
                CALL LINESCALE(iigl,XADD,TSC,iigb,YADD(IAX),YSC(IAX))

C Calculate the current time (day fraction). 
                HOURF=1.0/FLOAT(NETS)
                IF(NOUT.GT.1)THEN
                  SINC=FLOAT(NOUT)/2.
                ELSE
                  SINC=0.5
                ENDIF 
                TIMEC=((FLOAT(ITS)-SINC)*HOURF)

C Move the pen to the last plotted point for the recovered variable, 
C unless the first time increment is being plotted.
                iupdown=0
                isym=0
                IF(ILINST(I).EQ.0) THEN
                  CALL ETPLOT(TIMEC,EVAL2(I),iupdown,isym)
                  ILINST(I)=1
                ELSE
                  CALL ETPLOT(TIMEOLD(I),YOLD(I),iupdown,isym) 
                ENDIF

C Check if a symbol needs to be plotted at this point and draw the point on the graph. 
                IPEN=EVLINE(I)
                IF(ISSPAC.EQ.1)THEN
                  IMARK=EVMARK(I) 
                ELSE
                  IMARK=0
                ENDIF
                iupdown=IPEN
                isym=IMARK
                CALL ETPLOT(TIMEC,EVAL2(I),iupdown,isym)

C Store the plotted point and the time.
                YOLD(I)=EVAL2(I)
                TIMEOLD(I)=TIMEC
              ENDIF
  100       CONTINUE
          ENDIF

  80    CONTINUE

C Plot the label at the end of the data line.
        DO 110 I=1,NVAL1
          IF(EFILT(I).EQ.1)THEN
            IAX=EVAXIS(I)
            CALL LINESCALE(iigl,XADD,TSC,iigb,YADD(IAX),YSC(IAX))
            STR=' '

C Make sure the label is within the string bounds (20 chars).
            LSTR=LNBLNK(EVLABS(I))
            IF(LSTR.GT.14)LSTR=14

            EXLAB=' '
            IF(INT(AXSC(EVAXIS(I))).EQ.1)THEN
              WRITE(EXLAB(1:4),'(A4)')EVLABU(I)
            ELSEIF(INT(AXSC(EVAXIS(I))).EQ.1000)THEN
              WRITE(EXLAB(1:1),'(A1)')'k'
              WRITE(EXLAB(2:5),'(A4)')EVLABU(I)
            ELSEIF(INT(AXSC(EVAXIS(I))).EQ.1000000)THEN
              WRITE(EXLAB(1:1),'(A1)')'M'
              WRITE(EXLAB(2:5),'(A4)')EVLABU(I) 
            ENDIF  

C Combine the data decriptor and units to form the complete label. 
            WRITE(STR,'(A,1X,A)') 
     &        EVLABS(I)(1:LSTR),EXLAB(1:LNBLNK(EXLAB))
            ipos=3
            isize=0
            CALL ETLABEL(STR,TIMEOLD(I),YOLD(I),ipos,isize)
          ENDIF
 110    CONTINUE

      ENDIF
      RETURN
      END

C ************************************************************************************
      SUBROUTINE ENETTBL(MODE)

#include "power.h"
      
      integer lnblnk  ! function definition

C Array of recovered data
      COMMON/ERECOV/NVAL1,NVAL2,EVAL1(MERAWD),EVAL2(MERAWD)
      COMMON/ERECOVL/EVLABS(MERAWD),EVLABU(MERAWD)
      COMMON/ENFILT/EFILT(MERAWD)
      COMMON/ERECOVH/HDR1,HDR2
      COMMON/ELRFIL/ELRFLNAM
      character ELRFLNAM*144

C Export files and unit numbers.
      COMMON/EXPORTI/IXOPEN,IXUNIT,IXPUNIT
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      COMMON/EXPORTTG/XFILE,TG,DELIM
      COMMON/OUTPCH/ICOUT

C The period details
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/NETTS/ITSTPS,ITSTPF,NTSTP

C Electrical network results library details.
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC


      REAL EVAL1,EVAL2

      INTEGER NVAL1,NVAL2,EFILT
  
      CHARACTER MODE*1,EVLABU*4,EVLABS*16,OUTPUTS*216,SOUTP*18,OUTS*124,
     &DELIM*1,XFILE*144,HDR1*216,HDR2*216,DESCR*7,DESCR1*10,PDESCR*60,
     &SDESCR*44,SSTR*3,TG*1

C Initialise the output string 
      OUTPUTS=' '
      OUTS=' '
      SOUTP=' '
      HDR1=' '
      HDR2=' '

C Convert the start and finish times into timesteps and therefore record numbers. 
C Firstly calculate the period start and finish days.
      CALL EDAY(IOD1,IOM1,IOPS)
      CALL EDAY(IOD2,IOM2,IOPF)
      IF(IOH1.NE.0)THEN
        ITSTPS=1+((IOPS-ISDS)*24+IOH1-1)*NETS
      ELSE
        ITSTPS=1+((IOPS-ISDS)*24+IOH1)*NETS
      ENDIF
      ITSTPF=1+((IOPF-ISDS)*24+IOH2)*NETS 


C Make up the output header using the header elements held in in EVLABS
C and EVLABU. 
      IF(MODE.EQ.'I')THEN

        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)

C Firstly write the text header
        IF(IHFLAG.EQ.0)THEN
          CALL EDISP(ICOUT,'# Timestep miscellaneous information.')
          WRITE(OUTS,'(A,A)')'# Electrical results lib: ',ELRFLNAM(1:20)
          CALL EDISP(ICOUT,OUTS)
          WRITE(OUTS,'(2A)')'# ',PDESCR
          CALL EDISP(ICOUT,OUTS)
          WRITE(outs,'(2A)')'# ',SDESCR
          CALL EDISP(ICOUT,OUTS)
          IS=1
          WRITE(OUTPUTS(IS:IS+17),'(A)')'#      Time      |'
          IS=IS+18
        ELSE
          IS=1
          WRITE(OUTPUTS(IS:IS+17),'(A)')'       Time      |'
          IS=IS+18
        ENDIF

C Make sure the string bounds are not exceeded. 
        IF(NVAL1.GT.11) THEN
          NV=11
        ELSE
          NV=NVAL1
        ENDIF
        DO 10 I=1,NV 
          IF(EFILT(I).EQ.1)THEN
            SOUTP=' '
            WRITE(SOUTP(1:1),'(A)')' ' 
            WRITE(SOUTP(2:17),'(A)') EVLABS(I)
            WRITE(SOUTP(18:18),'(A)')'|'
            WRITE(OUTPUTS(IS:IS+17),'(A)') SOUTP
            IS=IS+18
          ENDIF
  10    CONTINUE

C Save the header string and write the output string to the output file. 
        HDR1=OUTPUTS
        IF(IXOPEN.EQ.1)THEN
          CALL EDISP(ICOUT,' Output being directed to file... ')
          WRITE(IXUNIT,'(A)') OUTPUTS(1:LNBLNK(OUTPUTS))
        ELSE
          WRITE(OUTS,'(A)') OUTPUTS(1:124)
          CALL EDISP(ICOUT,OUTS)
        ENDIF

C Write second header output line
        IS=1
        IF(IHFLAG.EQ.0)THEN
          WRITE(OUTPUTS(IS:IS+17),'(A)')'#    hr:mm:ss    |'
        ELSE
          WRITE(OUTPUTS(IS:IS+17),'(A)')'     hr (aa.bb)  |'
        ENDIF
        IS=IS+18
        DO 20 I=1,NV
          IF(EFILT(I).EQ.1)THEN
            SOUTP=' '
            IF(IHFLAG.EQ.0)THEN
              WRITE(SOUTP(1:1),'(A)')'#'
            ELSE
              WRITE(SOUTP(1:1),'(A)')' ' 
            ENDIF
            WRITE(SOUTP(2:17),'(6X,A4,6X)') EVLABU(I)
            WRITE(SOUTP(18:18),'(A)')'|'
            WRITE(OUTPUTS(IS:IS+17),'(A)') SOUTP
            IS=IS+18
          ENDIF
  20    CONTINUE

C Save the header string and write the output string to the output file. 
        HDR2=OUTPUTS
        IF(IXOPEN.EQ.1)THEN
          WRITE(IXUNIT,'(A)') OUTPUTS(1:LNBLNK(OUTPUTS))
        ELSE
          WRITE(OUTS,'(A)') OUTPUTS(1:124)
          CALL EDISP(ICOUT,OUTS)
        ENDIF

C List out the time series data. 
      ELSEIF(MODE.EQ.'U')THEN

C Loop through each timestep and plot the data.
        IAVERF=1
        IDAYC=0
        DO 80 ITS=ITSTPS,ITSTPF

C Increment the day counter Check if the header string needs written 
          IDAYC=IDAYC+1
          IF(IDHFLG.EQ.1)THEN
            IF(IDAYC.EQ.(NETS*24))THEN

C Calculate the day
              IDAY=IOD1+(ITS/(24*NETS))
              CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
              IF(IXOPEN.EQ.1)THEN
                OUTS='# '
                CALL EDISP(IXUNIT,OUTS)
                WRITE(OUTS,'(3A)')'# ',DESCR1,' timestep misc. listing'
                CALL EDISP(IXUNIT,OUTS)
                OUTS='# '
                WRITE(IXUNIT,'(A)') HDR1(1:LNBLNK(HDR1))
                WRITE(IXUNIT,'(A)') HDR2(1:LNBLNK(HDR2))
              ELSE
                OUTS='# '
                CALL EDISP(ICOUT,OUTS)
                WRITE(OUTS,'(3A)')'# ',DESCR1,' timestep misc. listing'
                CALL EDISP(ICOUT,OUTS)
                OUTS='# '
                WRITE(OUTS,'(A)') HDR1(1:124)
                CALL EDISP(ICOUT,OUTS)
                WRITE(OUTS,'(A)') HDR2(1:124)
                CALL EDISP(ICOUT,OUTS)
              ENDIF 

C Reset the day counter.
              IDAYC=0               
            ENDIF
          ENDIF

C Read the selected data from the library at each time step.
          IGT=ITS
          CALL ENETGET(IGT,'-')

C Average the data if required.
          DO 90 I=1,NVAL1
            IF(EFILT(I).EQ.1)THEN
              IF(NOUT.EQ.1) THEN

C No averaging.
                EVAL2(I)=EVAL1(I)
              ELSE
                IF(IAVERF.EQ.1) THEN

C Add half of first data period to the average. 
                  EVAL2(I)=EVAL1(I)/2.
                ELSEIF(IAVERF.EQ.NOUT+1)THEN

C Add half of the last data period to the average and take the average.
                  EVAL2(I)=EVAL2(I)+EVAL1(I)/2.
                  EVAL2(I)=EVAL2(I)/FLOAT(NOUT+1)                
                ELSE

C For intermediate periods add the whole value.
                  EVAL2(I)=EVAL2(I)+EVAL1(I)
                ENDIF
              ENDIF 
            ENDIF       
  90      CONTINUE

C Set the data delimiters.
          IF(DELIM.EQ.'-') THEN
            SSTR(1:3)='   '
          ELSEIF(DELIM.EQ.'S')THEN
            SSTR(1:1)=' '
          ELSEIF(DELIM.EQ.'T')THEN
            SSTR(1:1)=CHAR(9)
          ELSEIF(DELIM.EQ.'C')THEN
            SSTR(1:1)=','
          ENDIF

          IF(IAVERF.LT.NOUT+1.AND.NOUT.NE.1)THEN
            IAVERF=IAVERF+1 
          ELSE

C Reset the averaging flag. 
            IAVERF=1

C Calculate the current time (day fraction). 
            HOURF=1.0/FLOAT(NETS)
            IF(NOUT.GT.1)THEN
              SINC=FLOAT(NOUT)/2.
            ELSE
              SINC=0.5
            ENDIF 
            TIMEC=((FLOAT(ITS)-SINC)*HOURF)

C Calculate the time for the current time increment based on the 
C specified time flag XXXXX.YYYYYY or XXhYYmZZs
            IF(IHFLAG.EQ.0)THEN
              IHOUR=INT(TIMEC)
              IMIN=INT((TIMEC-FLOAT(IHOUR))*60.)
              ISEC=INT((TIMEC-(FLOAT(IHOUR)+(FLOAT(IMIN)/60.)))*3600.)
            ENDIF
            IS=1
            IF(DELIM.EQ.'-')THEN

C Format time for a three character delimiter
              IF(IHFLAG.EQ.0)THEN
                WRITE(OUTPUTS(IS:IS+14),'(3X,I2,A1,I2,A1,I2,4X)')
     &IHOUR,':',IMIN,':',ISEC
              WRITE(OUTPUTS(IS+15:IS+17),'(A)') SSTR(1:3)
              ELSE
                WRITE(OUTPUTS(IS:IS+14),'(3X,F10.5,2X)')TIMEC
                WRITE(OUTPUTS(IS+15:IS+17),'(A)') SSTR(1:3)
              ENDIF
            ELSE

C Format time for a single character spacing
              IF(IHFLAG.EQ.0)THEN
                WRITE(OUTPUTS(IS:IS+16),'(5X,I2,A1,I2,A1,I2,5X)')
     &IHOUR,':',IMIN,':',ISEC
                WRITE(OUTPUTS(IS+17:IS+17),'(A)') SSTR(1:1)  
              ELSE
                WRITE(OUTPUTS(IS:IS+16),'(3X,F10.5,4X)')TIMEC
                WRITE(OUTPUTS(IS+17:IS+17),'(A)') SSTR(1:1)    
              ENDIF
            ENDIF
            IS=IS+18

C Make sure the string bounds are not exceeded. 
            IF(NVAL1.GT.11) THEN
              NV=11
            ELSE
              NV=NVAL1
            ENDIF

C Write the selected data to the output string with the appropriate delimiter.
            DO 110 I=1,NV 
              IF(EFILT(I).EQ.1)THEN
                SOUTP=' '
                IF(DELIM.EQ.'-')THEN
                  WRITE(SOUTP(1:15),'(F15.5)') EVAL2(I)
                  WRITE(SOUTP(16:18),'(A)')SSTR(1:3)
                  WRITE(OUTPUTS(IS:IS+17),'(A)') SOUTP
                  IS=IS+18
                ELSE
                  WRITE(SOUTP(1:17),'(F17.6)') EVAL2(I)
                  WRITE(SOUTP(18:18),'(A)')SSTR(1:1)
                  WRITE(OUTPUTS(IS:IS+17),'(A)') SOUTP
                  IS=IS+18
                ENDIF
              ENDIF
  110       CONTINUE

C Write the output string to the file or screen (not that the output string is
C shortened for the screen display.
C Write the output string to the output file. 
            IF(IXOPEN.EQ.1)THEN
              WRITE(IXUNIT,'(A)') OUTPUTS(1:LNBLNK(OUTPUTS))
            ELSE
              WRITE(OUTS,'(A)') OUTPUTS(1:124)
              CALL EDISP(ICOUT,OUTS)
            ENDIF  
          ENDIF        
  80    CONTINUE

      ENDIF
      RETURN
      END

C ******************** MOGEHED ********************
C MOGEHED outputs the appropriate header information for the electrical
C network corresponding to graphical result display.

      SUBROUTINE MOGEHED
#include "power.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/ELRFIL/ELRFLNAM
      character ELRFLNAM*144
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C Array of recovered data
      COMMON/ERECOV/NVAL1,NVAL2,EVAL1(MERAWD),EVAL2(MERAWD)
      COMMON/ERECOVL/EVLABS(MERAWD),EVLABU(MERAWD)
      COMMON/ERECOVD/EVAXIS(MERAWD),EVMARK(MERAWD),EVLINE(MERAWD)

      COMMON/ENFILT/EFILT(MERAWD)

      COMMON/NETPLOT/XSTRT,XFIN,ILINST(MERAWD),AXSC(5)

      REAL EVAL1,EVAL2
      INTEGER NVAL1,NVAL2,EVAXIS,EVMARK,EVLINE,EFILT
      CHARACTER ETEXT*124,LOUT*92,SLOUT*22
      CHARACTER PDESCR*60,SDESCR*44
      CHARACTER EVLABS*16,EVLABU*4,EXLAB*5

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer lnerl
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

C Generate a descriptive string for the time period to be displayed
C and the simulation and output timesteps. Use opengdisp to get
C pixel references for calls to VIEWTEXT.
      CALL HDDATE(PDESCR)
      CALL HDSTEP(SDESCR)

      IF(MMOD.EQ.8)THEN
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call feedbox(menuchw,2,igfw,igfh)
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif

C Setup and pass in parameters to win3d.
        iiw1=9; iiw2=30; iiw3=5; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        call startbuffer()
      ENDIF

C Prevent buffer overflow.
      lnerl=MIN0(LNBLNK(ELRFLNAM),107)
      WRITE(ETEXT,'(A,1X,A)') 'Electrical from ',ELRFLNAM(1:lnerl)
      iside=1
      isize=0
      ifont=1
      call viewtext(etext,iside,isize,ifont)

      WRITE(ETEXT,'(A)')PDESCR
      iside=2
      call viewtext(etext,iside,isize,ifont)

      WRITE(ETEXT,'(A)')SDESCR
      iside=3
      call viewtext(etext,iside,isize,ifont)


C Write a maximum of four of the selected data entities to screen
      IPOS=1
      LOUT=' '
      IF(NVAL1.GT.5)THEN
        NOUT=5
      ELSE
        NOUT=NVAL1
      ENDIF
      DO 4 I=1,NOUT 
        IF(EFILT(I).EQ.1)THEN
          SLOUT=' '

C Scale scratch unit labels to match the axis scaling.
          EXLAB=' '
          IF(INT(AXSC(EVAXIS(I))).EQ.1)THEN
            WRITE(EXLAB(1:4),'(A4)')EVLABU(I)
          ELSEIF(INT(AXSC(EVAXIS(I))).EQ.1000)THEN
            WRITE(EXLAB(1:1),'(A1)')'k'
            WRITE(EXLAB(2:5),'(A4)')EVLABU(I)
          ELSEIF(INT(AXSC(EVAXIS(I))).EQ.1000000)THEN
            WRITE(EXLAB(1:1),'(A1)')'M'
            WRITE(EXLAB(2:5),'(A4)')EVLABU(I) 
          ENDIF  
          
          WRITE (SLOUT,'(A,1X,A)')EVLABS(I)(1:LNBLNK(EVLABS(I))),EXLAB
          ISLEN=LNBLNK(SLOUT)
          WRITE(LOUT(IPOS:IPOS+ISLEN+1),
     &'(A,A)')SLOUT(1:LNBLNK(SLOUT)),','
          IPOS=IPOS+ISLEN+2
        ENDIF
 4    CONTINUE
      IF(NVAL1.GT.4)THEN
        WRITE(LOUT(IPOS:IPOS+3),'(A)')'...'
      ENDIF        
      iside=4
      call viewtext(lout,iside,isize,ifont)
      RETURN
      END

C ***********************************************************************
C Subroutine ENRFILT allows the user to choose which selected data items 
C they want to display on the current graph. The filters are as follows:
C Filters for display of electrical data
C Variables introduced in this routine:
C 
C EFILT(*)- switch for filtering of data 1-show 0-don't show.
C  ***********************************************************************

      SUBROUTINE ENRFILT

#include "power.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/ENFILT/EFILT(MERAWD)
      COMMON/ERECOVL/EVLABS(MERAWD),EVLABU(MERAWD)
      COMMON/ERECOV/NVAL1,NVAL2,EVAL1(MERAWD),EVAL2(MERAWD)

      INTEGER EFILT

      CHARACTER ELEND*5,VERT*30,HEAD*30,SLOUT*22,EVLABS*16,
     &EVLABU*4,KEY*1
      DIMENSION VERT(MERAWD)
      integer MVERT,IVERT ! max items and current menu item

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ENRFILT'  ! set for subroutine

C Ensure that the selected data information is avalaible
      II=1
      CALL ENETGET(II,'I')

C Set up the menu display

C Loop through the selected data items and form a menu

  3   HEAD='Currently selected data'
      IER=0
      MHEAD=1
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      ILEN=NVAL1
      IVERT=-4
      MCTL=4
      M=MHEAD
      VERT(1)='  Description         |  View'
      DO 10 I=1,NVAL1
        IF(EFILT(I).EQ.1)THEN
          ELEND='>>ON '
        ELSE
          ELEND='>>OFF'
        ENDIF

C Write the selected data labels to the menu
        M=M+1
        VERT(M)=' '
        SLOUT=' '
        CALL EMKEY(I,KEY,IER)
        WRITE (SLOUT,'(1X,A,1X,A)')EVLABS(I)(1:LNBLNK(EVLABS(I))),
     &EVLABU(I)(1:LNBLNK(EVLABU(I)))
        WRITE(VERT(M),'(A,1X,A)')KEY,SLOUT(1:LNBLNK(SLOUT))

C Append the appropriate switch ending
        WRITE(VERT(M)(25:29),'(A)')ELEND

 10   CONTINUE

      MVERT=M+MCTL

C Add the control data to the menu.
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  _________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ----')
      ENDIF
      VERT(M+2)='  '
      VERT(M+3)='? Help                   '
      VERT(M+4)='- Exit                   '
      IVERT=-4

C Help text for menu.
      helptopic='res_data_entry_onoff'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(HEAD,VERT,MVERT,IVERT)
      IF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) THEN
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        IV=IFOC
        IF(EFILT(IV).EQ.1)THEN
          EFILT(IV)=0
        ELSE
          EFILT(IV)=1
        ENDIF
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.MVERT-1)THEN
        helptopic='res_data_entry_onoff'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(HEAD,nbhelp,'-',0,0,IER)    
        IVERT=-1  
        GOTO 3 
      ELSE
        IVERT=-1
        GOTO 3
      ENDIF
 
      END
