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
C   SDDEF003 - Climate
C   SDDEF004 - Operations
C   SDDEF005 - Convection coefficients
C   SDDEF101 - Zone geometry
C   SDDBOUND - ask for numerical data with range checking.

C ****************** SDDEF001 ******************
C SDDEF001 - Material properties distribution definition.
      subroutine SDDEF001(ICDEF,IER)
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      common/UADFLT/MATTYPE(30),DFLTMAT(5,6)

      logical closemat1,closemat2

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      CHARACTER NAM*72,TITL*72
      CHARACTER LUALF*72,LCNG*15,LLOC*15,t15*15
      CHARACTER H*72, ITEM(8)*16, LELM(MMLC*ME)*36

      DIMENSION IVALP(6),IVALE(MMLC*ME),IELM(MMLC*ME)
      
      IER=0
C Materials - find and display unique elements.
      call edisp(iuout,' Material selection. ')

C This returns the number of unique elements and there db ref.
      call UPELM(NPRIM,IELM,IER)

C Now find the names of the materials.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      DO 20 II=1,NPRIM
        IDB=IELM(II)
        IER=0
        if(closemat1.or.closemat2)then
          legmat=IELM(II)   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
            if(matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
              E=0.99; A=0.99; DRV=1.0
              TITL='GAPS'; NAM='AIR'
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          endif
        else
          CALL ERPCDB(IFMAT,IDB,iuout,IERR,DBCON,DBDEN,DBSHT,E,A,
     &      DRV,TITL,NAM)
        endif

C Check for errors - add to the total error counter if error has occured.
        if (IERR.ne.0) then
          IER=101
          return
        endif
        LELM(II)=NAM(1:36)
 20   CONTINUE

      H(1)='Pick the required material. '
      H(2)='   '
      H(3)='Exit will return to the main menu '
      H(4)='without saving any data. '
      IEPIC=NPRIM
      CALL EPICKS(IEPIC,IVALE,' ',' Which material : ',
     &                   36,NPRIM,LELM,' Material',IERR,4)

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
      ITEM(1) ='Conductivity'
      ITEM(2) ='Density '
      ITEM(3) ='Specific Heat'
      ITEM(4) ='Emissivity'
      ITEM(5) ='Absorptivity'
      ITEM(6) ='Diffusion res'
      NITMS=6

      H(1)='Pick the required property. '
      H(2)='   '
      H(3)='Exit will return to the main menu '
      H(4)='without saving any data. '
      IPPIC=6
 6    CALL EPICKS(IPPIC,IVALP,' ',' Which property : ',
     &                       16,NITMS,ITEM,LELM(IVALE(1)),IERR,4)

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
      call EASKAB('Define parameter bounds:','  ',
     &               'from default definitions','by data entry',IANS,0)
      if (IANS.eq.2) then
        IALLOW=3

C These limits should be set in a more considered way.
        XMPERC=10.0
        XMABS=10.0
        XLOW=0.0
        XHIGH=100.0
        call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      endif

C Write info to common.
      do 99 I=1,IEPIC
        do 66 J=1,IPPIC
          if (I.gt.1.or.J.gt.1) then
            ICDEF=ICDEF+1
            NICNG=NICNG+1
          endif

C Material properties.
          IDTYP(ICDEF)=1

C Add type selection to common.
          IDMAT(ICDEF,1)=IELM(IVALE(I))

C Add property to common.
          IDMAT(ICDEF,2)=IVALP(J)

C Add distribution data to common.
          if (IANS.eq.1) then
            IDMAT(ICDEF,3)=1

C Work out material category and property and hence uncertainty.
            ICAT=MATTYPE((IELM(IVALE(I))/20)+1)
            DAT(ICDEF,1)=DFLTMAT(ICAT,IVALP(J))
            write (t15,'(3a)')ITEM(IVALP(J))(1:4),':',
     &                                 LELM(IVALE(I))(1:10)
            call st2name(t15,LCNG(ICDEF))
          else
            IDMAT(ICDEF,3)=IALLOW
            DAT(ICDEF,1)=XLOW
            if (IALLOW.eq.3) then
              DAT(ICDEF,2)=XHIGH
            else
              DAT(ICDEF,2)=0.
            endif
          endif
 66     continue
 99   continue


      return
      end

C ****************** SDDEF002 ******************
C SDDEF002 - Composite constructions dietribution definition.
      subroutine SDDEF002(ICDEF,IER)
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA31/MATNAM(MNCNG,2)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      CHARACTER NAM*72,TITL*72
      CHARACTER H*72, LELM(MMLC*ME)*36
      CHARACTER MATNAM*12

      DIMENSION IVALE(MMLC*ME)

      IER=0
C Composite constructions.
      call EPKMLC(ISEL,'Which construction?','  ',IERR)

C Check for errors - add to the total error counter if error has occured.
      if (IERR.ne.0) then
        IER=101
        return
      endif
      write (MATNAM(ICDEF,1),'(a)') DESC(ISEL)(1:12)
      call EASKAB('Which composite construction parameter? ','  ',
     &                 'A materials thickness','Whole composite',IRT,0)
      if (IRT.eq.1) then

C Pick a material in that MLC.
        call eclose(matver,1.1,0.01,closemat1)
        call eclose(matver,1.2,0.01,closemat2)
        II=0
        do 10 IL=1,LAYERS(ISEL)
          if(closemat1.or.closemat2)then
            matarrayindex=IPRMAT(ISEL,IL)   ! which array index
            if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
              if(matarrayindex.eq.0)then
                DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                E=0.99; A=0.99; DRV=1.0
                TITL='GAPS'; NAM='AIR'
              else
                DBCON=matdbcon(matarrayindex)
                DBDEN=matdbden(matarrayindex)
                DBSHT=matdbsht(matarrayindex)
                E=matdbine(matarrayindex)
                A=matdbina(matarrayindex)
                DRV=matdbdrv(matarrayindex)
                write(TITL,'(a)') 'placeholder'
                write(NAM,'(a)') matname(matarrayindex)(1:32)
              endif
            endif
          else
            CALL ERPCDB(IFMAT,IPR(ISEL,IL),iuout,IERR,DBCON,DBDEN,
     &        DBSHT,E,A,DRV,TITL,NAM)
          endif

C Check for errors - add to the total error counter if error has occured.
          if (IERR.ne.0) then
            IER=101
            return
          endif
          II=II+1
          LELM(II)=NAM(1:36)
 10     CONTINUE
        H(1)='Pick the required material. '
        H(2)='   '
        H(3)='Exit will return to the main menu '
        H(4)='without saving any data. '
        NPRIM=II
        INPIC=1
        CALL EPICKS(INPIC,IVALE,' ',' Which material : ',
     &                   36,NPRIM,LELM,' material',IERR,4)

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
        IDMAT(ICDEF,1)=IPR(ISEL,IVALE(1))
          
      else
        call edisp(iuout,'Not enabled yet.')
        IER=101
        return
      endif

      IALLOW=3
      XMPERC=10.0
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DAT(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DAT(ICDEF,2)=XHIGH
      else
        DAT(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDEF003 ******************
C SDDEF003 - Climatic parameters distribution definition.
      subroutine SDDEF003(ICDEF,IER)
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      CHARACTER ITEM(6)*20, H*72

      DIMENSION IVALP(6)

C Now determine the climatic parameter which is uncertain.
      call edisp(iuout,' Climatic parameter selection. ')
      call edisp(iuout,'  ')
      ITEM(1) ='Dry bulb temperature'
      ITEM(2) ='Direct normal solar '
      ITEM(3) ='Diffuse horiz solar'
      ITEM(4) ='Wind speed'
      ITEM(5) ='Wind direction'
      ITEM(6) ='Relative humidity'
      NITMS=6

      H(1)='Pick the required parameter. '
      H(2)='   '
      H(3)='Exit will return to the main menu '
      H(4)='without saving any data. '
      INPIC=1
      CALL EPICKS(INPIC,IVALP,' ',' Which parameter : ',
     &                     20,NITMS,ITEM,'Climatic parameters',IERR,4)
      if (IERR.ne.0.or.IVALP(1).eq.0) then
        IER=101
        return
      endif
      IDMAT(ICDEF,1)=IVALP(1)

      IALLOW=2
      XMPERC=10.0
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DAT(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DAT(ICDEF,2)=XHIGH
      else
        DAT(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDEF004 ******************
C SDDEF004 - Operations distribution definition.
      subroutine SDDEF004(ICDEF,IER)
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      CHARACTER H*72

      IER=0
C Operations file data.
      call edisp(iuout,' Operations parameter selection. ')
      IRT=0
      call EASKAB('  ','Which scheduled parameter? ',
     &                                'Casual gains','Air flow',IRT,0)
      if (IRT.eq.1) then
        call EASKABC('  ','Which casual gain? ',
     &                               'Type 1','Type 2','Type 3',IRT,0)
        IDMAT(ICDEF,1)=IRT
        call EASKATOG('Which property? ',' ','Sensible','Latent',
     &       'Radiant frac','Convective frac','Total','  ','  ',IRT,0)
        IDMAT(ICDEF,2)=IRT
      else
        IDMAT(ICDEF,1)=0
        call EASKATOG('Which property? ',' ','Infiltration',
     &       'Ventilation','Zone coupled index','Source temp.','  ',
     &                                               '  ','  ',IRT,0)
        IDMAT(ICDEF,2)=IRT
      endif

      IALLOW=3
      XMPERC=10.0
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DAT(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DAT(ICDEF,2)=XHIGH
      else
        DAT(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDEF005 ******************
C SDDEF005 - Convection coefficients distribution definition.
      subroutine SDDEF005(ICDEF,IER)
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      CHARACTER H*72

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
      DAT(ICDEF,1)=3.
      DAT(ICDEF,2)=2.

C      IALLOW=3
C      XMPERC=10.0
C      XMABS=10.0
C      XLOW=0.0
C      XHIGH=100.0
C      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
C      IDMAT(ICDEF,3)=IALLOW
C      DAT(ICDEF,1)=XLOW
C      if (IALLOW.eq.3) then
C        DAT(ICDEF,2)=XHIGH
C      else
C        DAT(ICDEF,2)=0.
C      endif
      return
      end

C ****************** SDDEF101 ******************
C SDDEF101 - Zone geometry distribution definition.
      subroutine SDDEF101(ICDEF,IER)
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)

      CHARACTER H*72

      IDMAT(ICDEF,1)=1
      IDMAT(ICDEF,2)=1

      call edisp(iuout,'Zone geometry parameter distribution def.')
      call EASKAB('Only zone air volume enabled ! ','  ',
     &                'Continue and define distribution','Cancel',IRT,0)
      if (IRT.eq.2) then
        IER=101
        return
      endif

      IALLOW=3
      XMPERC=10.0
      XMABS=10.0
      XLOW=0.0
      XHIGH=100.0
      call SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)
      IDMAT(ICDEF,3)=IALLOW
      DAT(ICDEF,1)=XLOW
      if (IALLOW.eq.3) then
        DAT(ICDEF,2)=XHIGH
      else
        DAT(ICDEF,2)=0.
      endif
      return
      end

C ****************** SDDBOUND ******************
C SDDBOUND - Operations distribution definition.
C IALLOW - 3=perc, abs and explicit changes; 2=perc and abs only; 1=exp only
      subroutine SDDBOUND(IALLOW,XMPERC,XMABS,XLOW,XHIGH,IER)

      common/pophelp/h(60)

      CHARACTER H*72

      if (IALLOW.eq.3) then

C Now ask for the numerical data.
        H(1)='The upper and lower values that the parameter can '
        H(2)='take are defined via one of three processes:'
        H(3)='1) Percentage change: the upper and lower values will'
        H(4)='   be the base case value +/- the percentage change.'
        H(5)='2) Absolute change: the upper and lower values will be'
        H(6)='   the base case value +/- the absolute change.'
        H(7)='3) Explicitly: the upper and lower values will be '
        H(8)='   defined explicitly.'
        call EASKABC(' Define parameter bounds',' via:',
     &       'percentage change','absolute change','explicitly',ICNG,8)
        PERC=0.
        ABS=0.
        EXPU=0.
        EXPL=0.
        if (ICNG.eq.1) then
          H(1)=' A value greater than 0.00 is required. '
          call EASKR(PERC,' ',' Percentage change value? ',
     &        0.,'F',XMPERC,'W',1.,' Percantage change ',IER,1)
          XLOW=PERC
        elseif (ICNG.eq.2) then
          H(1)=' A value greater than 0.00 is required. '
          call EASKR(ABS,' ',' Absolute change value? ',
     &        0.,'F',XMABS,'W',1.,' Absolute change ',IER,1)
          XLOW=ABS
        elseif (ICNG.eq.3) then
          H(1)=' The upper and lower values are required. '
          H(2)=' There is no checking of these values, except that '
          H(3)=' the upper value is greater than the lower. '
22        call EASKR(EXPU,' ',' Explicit definition of upper value? ',
     &         XLOW,'W',XHIGH,'W',1.,' Explicit def upper value',IER,3)
          call EASKR(EXPL,' ',' Explicit definition of lower value? ',
     &         XLOW,'W',XHIGH,'W',1.,' Explicit def lower value',IER,3)
          if (EXPL.ge.EXPU) goto 22
          XLOW=EXPU
          XHIGH=EXPL
        endif
      elseif (IALLOW.eq.2) then
        H(1)='The upper and lower values that the parameter can '
        H(2)='take are defined via one of three processes:'
        H(3)='1) Percentage change: the upper and lower values will'
        H(4)='   be the base case value +/- the percentage change.'
        H(5)='2) Absolute change: the upper and lower values will be'
        H(6)='   the base case value +/- the absolute change.'
        call EASKAB(' Define parameter bounds',' via:',
     &       'percentage change','absolute change',ICNG,6)
        PERC=0.
        ABS=0.
        if (ICNG.eq.1) then
          H(1)=' A value greater than 0.00 is required. '
          call EASKR(PERC,' ',' Percentage change value? ',
     &        0.,'F',XMPERC,'W',1.,' Percantage change ',IER,1)
          XLOW=PERC
        elseif (ICNG.eq.2) then
          H(1)=' A value greater than 0.00 is required. '
          call EASKR(ABS,' ',' Absolute change value? ',
     &        0.,'F',XMABS,'W',1.,' Absolute change ',IER,1)
          XLOW=ABS
        endif
      endif

      IALLOW=ICNG

      return
      end
