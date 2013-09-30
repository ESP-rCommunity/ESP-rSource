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

C *********************************************************************
C This file contains the following subroutines and functions:

C GRAAPH displays the appropriate graph in the graphical display.
C SUPORT displays the multi-D gridding support facilities menu.
C INGRD3 initialize the multi-D gridding variables.
C INTCNC initializes the multi-D connection surfaces variables.
C INDCNC initializes the connection surfaces indentations.
C CHCK3D check the 3D configuration information and generates warning
C        massege when un-acceptable information is detected.
C ADDSSY checks if no interval is defined at the rquired y-level in 
C        srf-srf, it will create one and updates the associated arrays.
C ADDSSZ checks if no interval is defined at the rquired z-level in 
C        srf-srf, it will create one and updates the associated arrays.
C ADDSEY checks if no interval is defined at the rquired y-level in
C        srf-edg, it will create one and updates the associated arrays.
C ADDSEZ checks if no interval is defined at the rquired z-level in
C        srf-edg, it will create one and updates the associated arrays.
C ADDEEX checks if no interval is defined at the rquired x-level in 
C        edg-edg, it will create one and updates the associated arrays.
C ADDEEY checks if no interval is defined at the rquired y-level in 
C        edg-edg, it will create one and updates the associated arrays.
C ADDECX checks if no interval is defined at the rquired x-level in 
C        edg-crn, it will create one and updates the associated arrays.
C ADDECY checks if no interval is defined at the rquired y-level in 
C        edg-crn, it will create one and updates the associated arrays.
C CNCLNE defines connection surface type for a control volume 
C        (ICL > 0 : boundary line, ICL < 0 : another control volume).
C FDSCV2 finds the adjacent control volume for the current one.
C ISEFNS finds the type and number of the adjacent connection surface
C        for the current surface.
C ISEFNE Function finds the number of the srf-edge connection surface.
C NEDEDG define the number of the coolinear edge-edge connection. If
C        it does not exist, a new one will be created.
C IECFNE Function finds the number of the edge-corner connection surface.
C IEECFN finds the type (edge-edge or edge-corner) and number of the 
C        adjacent connection surface for the current edge.
C CMVRTS determines if a vertex is included in a surface.
C PROJCT determines the projection of a node defined by (XC,ZC) on 
C        a line defined by (IV1,IV2).
C RELPOS determines the relative location of a node defined by (XC,ZC)
C        with respect to the connection surface defined by two vertices 
C        (IV1,IV2).
C EFDIST Function returns the effective distance between two nodes of
C        which one belonges to srf-edge connection surface.
C PNTLNE checks if a point is on a line.
C RORDER reorders the vertex list for all the lines within a surface.
C ANGL2G defines the angle between the +ve x-axes and a line defined 
C        by the zone geometry file.
C ANGL2P determines the angle between a line between two points and the
C        positive x axes in the transformed plane.
C PLNORM defines the direction cosines for a plane in space.
C LINORM defines the direction cosines for a line in space.
C ANGCOS defines the angle between two vectors in space from cosine law.
C GCOORD determines the global coordinate.
C *********************************************************************

C ****************************  GRAAPH  ******************************
C GRAAPH displays the appropriate graph in the graphical display.

C      IDRW1         IDRW2          DOMAIN
C      ------        ------         ------
C         1             0           building
C         2             i           i-th zone
C         3             j           j-th surface
C         4             k           k-th edge
C         5             l           l-th corner
C         6             m           m-th srf-srf
C         7             n           n-th srf-edg
C         8             o           o-th edg-edg
C         9             p           p-th edg-crn
C        10             0           ground
C        11             q           q-th ground cross section
C        12             r           r-th ground boundary surface
C *********************************************************************
      SUBROUTINE GRAAPH(IDRW1,IDRW2)
#include "building.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      LOGICAL MODIFY,MODLEN,MODBND

      IF(MMOD.NE.8)RETURN
      IF(IDRW1.EQ.1)THEN
        CALL startbuffer()
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        nzg=NCOMP
        DO 10 IP=1,nzg
          nznog(IP)=IP
  10    CONTINUE
        CALL INLNST(1)
        izgfoc=0
        CALL ADJVIEW(IER)
      ELSEIF(IDRW1.EQ.2)THEN
        CALL startbuffer()
        MODIFY=.TRUE.
        MODBND=.TRUE.
        MODLEN=.TRUE.
        nzg=1
        nznog(1)=IDRW2
        izgfoc=IDRW2
        CALL INLNST(1)
        CALL ADJVIEW(IER)
      ELSEIF(IDRW1.EQ.3)THEN
        CALL DRWSRF(IDRW2)
      ELSEIF(IDRW1.EQ.4)THEN
        CALL DRWEDG(IDRW2)
      ELSEIF(IDRW1.EQ.5)THEN
        CALL DRWCRN(IDRW2)
      ELSEIF(IDRW1.EQ.10)THEN
        CALL DRWGND
      ELSEIF(IDRW1.EQ.11)THEN
        CALL FHCGND(IDRW2)
      ELSEIF(IDRW1.EQ.12)THEN
        CALL FHCBSF(IDRW2)
      ENDIF
      call forceflush()

      RETURN
      END

C *************************     SUPORT      ***************************
C SUPORT displays the multi-D gridding support facilities menu.
C *********************************************************************
      SUBROUTINE SUPORT(IZ)
#include "building.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GR3D108/L3DCVS(MCOM),L3DCNC(MCOM),L3DNDC(MCOM),L3DTAQ(MCOM)
      COMMON/GRND108/LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD

      CHARACTER*72 L3DCVS,L3DCNC,L3DNDC,L3DTAQ
      CHARACTER*72 LGDCVS,LGDCNC,LGDNDC,LGDTAQ,LGDNDD
      CHARACTER*72 AFLCVS,BFLCVS,AFLCNC,BFLCNC
      CHARACTER*72 AFLNDC,BFLNDC,AFLTAQ,BFLTAQ
      CHARACTER ITEM(11)*41,MSG*72,FLTMP*65
      integer ier
      integer MITEM,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SUPORT'  ! set for subroutine

      IF(IZ.EQ.-1)THEN
        BFLCVS=LGDCVS
        BFLCNC=LGDCNC
        BFLTAQ=LGDTAQ
        BFLNDC=LGDNDC
      ELSE
        BFLCVS=L3DCVS(IZ)
        BFLCNC=L3DCNC(IZ)
        BFLTAQ=L3DTAQ(IZ)
        BFLNDC=L3DNDC(IZ)
      ENDIF
      FLTMP=BFLCVS(1:65)
      WRITE(AFLCVS,'(A,A6)')FLTMP(1:lnblnk(FLTMP)),'_ASCII'
      FLTMP=BFLCNC(1:65)
      WRITE(AFLCNC,'(A,A6)')FLTMP(1:lnblnk(FLTMP)),'_ASCII'
      FLTMP=BFLTAQ(1:65)
      WRITE(AFLTAQ,'(A,A6)')FLTMP(1:lnblnk(FLTMP)),'_ASCII'
      FLTMP=BFLNDC(1:65)
      WRITE(AFLNDC,'(A,A6)')FLTMP(1:lnblnk(FLTMP)),'_ASCII'
   10 INO=-3
      ITEM(1)='1 Binary --> ASCII  ( control volumes )  '
      ITEM(2)='2 ASCII  --> Binary ( control volumes )  '
      ITEM(3)='3 Binary --> ASCII  ( connection file )  '
      ITEM(4)='4 ASCII  --> Binary ( connection file )  '
      ITEM(5)='5 Binary --> ASCII  (nodes temperature)  '
      ITEM(6)='6 ASCII  --> Binary (nodes temperature)  '
      ITEM(7)='7 Binary --> ASCII  (nodes coordinates)  '
      ITEM(8)='8 ASCII  --> Binary (nodes coordinates)  '
      ITEM(9)='  ---------------------------------------'
      ITEM(10)='? help                                   '
      ITEM(11)='- Exit                                   '
      MITEM=11

C Help text for this menu.
      helptopic='multi_bin_ascii_conv'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EMENU('        Support facilities',ITEM,MITEM,INO)

C Binary --> ASCII  ( control volumes ).
      IF(INO.EQ.1)THEN
        CALL EASKS(BFLCVS,'the control volumes file (Binary-version) ?',
     &    ' ',72,' ','binary file',IER,nbhelp)
        IU1=IFIL+1
        IF(IZ.EQ.-1)THEN
          ier=0
          CALL EFOPRAN(IU1,BFLCVS,3,1,IER)
          IF(IER.NE.0)GOTO 10
        ELSE
          ier=0
          CALL EFOPRAN(IU1,BFLCVS,5,1,IER)
          IF(IER.NE.0)GOTO 10
        ENDIF
        CALL EASKS(AFLCVS,'the control volumes file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU2=IFIL+2
        CALL EFOPSEQ(IU2,AFLCVS,4,IER)
        IF(IER.NE.0)GOTO 10
        IF(IZ.EQ.-1)THEN
          WRITE(IU2,'(A)')'      ICV    IND   VOLUME    DENS*HC'
          NCV=0
   20     NCV=NCV+1
          READ(IU1,REC=NCV,IOSTAT=ISTAT,ERR=1)ND,VOLM,DNHC
          IF(ISTAT.LT.0)THEN
            CALL EDISP(IUOUT,' End of file reached.')
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          WRITE(IU2,'(2I8,2G11.3)')NCV,ND,VOLM,DNHC
          GOTO 20
        ELSE
          WRITE(IU2,'(A35,A12)')'      ICV    IND   VOLUME    DENS*',
     &                'HC  IP1 IP2'
          NCV=0
   30     NCV=NCV+1
          READ(IU1,REC=NCV,IOSTAT=ISTAT,ERR=1)ND,VOLM,DNHC,ISF,IL
          IF(ISTAT.LT.0)THEN
            CALL EDISP(IUOUT,' End of file reached.')
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          WRITE(IU2,'(2I8,2G11.3,2I5)')NCV,ND,VOLM,DNHC,ISF,IL
          GOTO 30
        ENDIF
      ELSEIF(INO.EQ.2)THEN

C ASCII  --> Binary ( control volumes ).
        CALL EASKS(AFLCVS,'the control volumes file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU1=IFIL+1
        CALL EFOPSEQ(IU1,AFLCVS,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(BFLCVS,'the control volumes file (Binary-version) ?',
     &     ' ',72,' ','binary file',IER,nbhelp)
        IU2=IFIL+2
        IF(IZ.EQ.-1)THEN
          ier=0
          CALL EFOPRAN(IU2,BFLCVS,3,4,IER)
          IF(IER.NE.0)GOTO 10
          CALL USRMSG(' ',' ','-')
          READ(IU1,*)
          IREC=0
   40     READ(IU1,*,IOSTAT=ISTAT)NCV,ND,VOLM,DNHC
          IF(ISTAT.LT.0)THEN
            CALL EDISP(IUOUT,' End of file reached.')
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          IREC=IREC+1
          IF(IREC.NE.NCV)THEN
            WRITE(MSG,'(A,I8)')
     &        'wronge control volume number at line:',IREC+1
            CALL EDISP(IUOUT,MSG)
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          WRITE(IU2,REC=NCV,ERR=1)ND,VOLM,DNHC
          GOTO 40
        ELSE
          ier=0
          CALL EFOPRAN(IU2,BFLCVS,5,4,IER)
          IF(IER.NE.0)GOTO 10
          CALL USRMSG(' ',' ','-')
          READ(IU1,*)
          IREC=0
   50     READ(IU1,*,IOSTAT=ISTAT)NCV,ND,VOLM,DNHC,ISF,IL
          IF(ISTAT.LT.0)THEN
            CALL EDISP(IUOUT,' End of file reached.')
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          IREC=IREC+1
          IF(IREC.NE.NCV)THEN
            WRITE(MSG,'(A,I8)')
     &                   'wronge control volume number at line:',IREC+1
            CALL EDISP(IUOUT,MSG)
            CALL ERPFREE(IU1,ISTAT)
            CALL ERPFREE(IU2,ISTAT)
            GOTO 10
          ENDIF
          WRITE(IU2,REC=NCV,ERR=1)ND,VOLM,DNHC,ISF,IL
          GOTO 50
        ENDIF
      ELSEIF(INO.EQ.3)THEN

C Binary --> ASCII  ( connection file ).
        CALL EASKS(BFLCNC,'the connection file (Binary-version) ?',
     &     ' ',72,' ','binary file',IER,nbhelp)
        IU1=IFIL+1
        ier=0
        CALL EFOPRAN(IU1,BFLCNC,7,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(AFLCNC,'the connection file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU2=IFIL+2
        CALL EFOPSEQ(IU2,AFLCNC,4,IER)
        IF(IER.NE.0)GOTO 10
        WRITE(IU2,'(2A)')'     ICNC    ND1    DIST       AR',
     &              'EA     COND    ITYP ISP1 ISP2'
        IC=0
   60   IC=IC+1
        READ(IU1,REC=IC,IOSTAT=ISTAT,ERR=1)
     &                    ND1,DIST,ARECV,COND,ITYP,ISP1,ISP2
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        WRITE(IU2,'(2I8,3G11.3,2I4,I6)')
     &    IC,ND1,DIST,ARECV,COND,ITYP,ISP1,ISP2
        GOTO 60
      ELSEIF(INO.EQ.4)THEN

C ASCII  --> Binary ( connection file ).
        CALL EASKS(AFLCNC,'the connection file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU1=IFIL+1
        CALL EFOPSEQ(IU1,AFLCNC,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(BFLCNC,'the connection file (Binary-version) ?',
     &     ' ',72,' ','binary file',IER,nbhelp)
        IU2=IFIL+2
        ier=0
        CALL EFOPRAN(IU2,BFLCNC,7,4,IER)
        IF(IER.NE.0)GOTO 10
        READ(IU1,*)
        IREC=0
   70   READ(IU1,*,IOSTAT=ISTAT)IC,ND1,DIST,ARECV,COND,ITYP,ISP1,ISP2
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        IREC=IREC+1
        IF(IREC.NE.IC)THEN
          WRITE(MSG,'(2A,I8)')' WARNING: wronge connection number ',
     &                        'at line:',IREC+1
          CALL EDISP(IUOUT,MSG)
        ENDIF
        WRITE(IU2,REC=IREC,ERR=1)ND1,DIST,ARECV,COND,ITYP,ISP1,ISP2
        GOTO 70
      ELSEIF(INO.EQ.5)THEN

C Binary --> ASCII  ( nodes temperature ).
        CALL EASKS(BFLTAQ,'the temperature file (Binary-version) ?',
     &    ' ',72,' ','binary file',IER,nbhelp)
        IU1=IFIL+1
        ier=0
        CALL EFOPRAN(IU1,BFLTAQ,2,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(AFLTAQ,'the temperature file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU2=IFIL+2
        CALL EFOPSEQ(IU2,AFLTAQ,4,IER)
        IF(IER.NE.0)GOTO 10
        WRITE(IU2,'(A)')'     NODE    Temp_P   Temp_F'
        IC=0
   80   IC=IC+1
        READ(IU1,REC=IC,IOSTAT=ISTAT,ERR=1)TMPP,TMPF
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        WRITE(IU2,'(I8,2(3X,F7.2))')IC,TMPP,TMPF
        GOTO 80
      ELSEIF(INO.EQ.6)THEN

C ASCII  --> Binary ( nodes temperature ).
        CALL EASKS(AFLTAQ,'the temperature file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU1=IFIL+1
        CALL EFOPSEQ(IU1,AFLTAQ,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(BFLTAQ,'the temperature file (Binary-version) ?',
     &       ' ',72,' ','binary file',IER,nbhelp)
        IU2=IFIL+2
        ier=0
        CALL EFOPRAN(IU2,BFLTAQ,3,4,IER)
        IF(IER.NE.0)GOTO 10
        READ(IU1,*)
        IREC=0
   90   READ(IU1,*,IOSTAT=ISTAT)IND,TMPP,TMPF
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        IREC=IREC+1
        IF(IREC.NE.IND)THEN
          WRITE(MSG,'(A,I8)')'wronge node number at line:',IREC+1
          CALL EDISP(IUOUT,MSG)
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        WRITE(IU2,REC=IND,ERR=1)TMPP,TMPF
        GOTO 90
      ELSEIF(INO.EQ.7)THEN

C Binary --> ASCII  ( nodes coordinates ).
        CALL EASKS(BFLNDC,'the coordinates file (Binary-version) ?',
     &    ' ',72,' ','binary file',IER,nbhelp)
        IU1=IFIL+1
        ier=0
        CALL EFOPRAN(IU1,BFLNDC,3,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(AFLNDC,'the coordinates file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU2=IFIL+2
        CALL EFOPSEQ(IU2,AFLNDC,4,IER)
        IF(IER.NE.0)GOTO 10
        WRITE(IU2,'(A)')'     NODE     X         Y           Z'
        IC=0
  100   IC=IC+1
        READ(IU1,REC=IC,IOSTAT=ISTAT,ERR=1)XND,YND,ZND
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        WRITE(IU2,'(I8,3G13.5)')IC,XND,YND,ZND
        GOTO 100
      ELSEIF(INO.EQ.8)THEN

C ASCII  --> Binary ( nodes coordinates ).
        CALL EASKS(AFLNDC,'the coordinates file (ASCII-version) ?',
     &     ' ',72,' ','ascii file',IER,nbhelp)
        IU1=IFIL+1
        CALL EFOPSEQ(IU1,AFLNDC,1,IER)
        IF(IER.NE.0)GOTO 10
        CALL EASKS(BFLNDC,'the coordinates file (Binary-version) ?',
     &    ' ',72,' ','binary file',IER,nbhelp)
        IU2=IFIL+2
        ier=0
        CALL EFOPRAN(IU2,BFLNDC,3,4,IER)
        IF(IER.NE.0)GOTO 10
        READ(IU1,*)
        IREC=0
  110   READ(IU1,*,IOSTAT=ISTAT)IND,XND,YND,ZND
        IF(ISTAT.LT.0)THEN
          CALL EDISP(IUOUT,' End of file reached.')
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        IREC=IREC+1
        IF(IREC.NE.IND)THEN
          WRITE(MSG,'(A,I8)')'wronge node number at line:',IREC+1
          CALL EDISP(IUOUT,MSG)
          CALL ERPFREE(IU1,ISTAT)
          CALL ERPFREE(IU2,ISTAT)
          GOTO 10
        ENDIF
        WRITE(IU2,REC=IND,ERR=1)XND,YND,ZND
        GOTO 110
      ELSEIF(INO.EQ.MITEM-1)THEN

C Help.
        CALL PHELPD('grd opening',5,'-',0,0,IER)
      ELSEIF(INO.EQ.MITEM)THEN

C Exit program.
        RETURN
      ENDIF
      GOTO 10
    1 CALL USRMSG(' ',
     &  'Error detected while reading/writing to a file.','W')
      END

C *****************************  INGRD3  ******************************
C INGRD3 initialize the multi-D gridding variables.
C *********************************************************************
      SUBROUTINE INGRD3
#include "building.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D05/NVRTB(MS),NVRTD(MS),NVRTA(MS)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D27/NDZE(MEZ),DZE(MEZ,MZE)
      COMMON/GR3D28/ITPEDG(MEZ,MZE,MZE,MZE)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D38/ITPCRN(MCZ,MZE,MZE,MZE)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D52/NDZSS(MSSZ),DZSS(MSSZ,MZSS)
      COMMON/GR3D53/NDYSS(MSSZ),DYSS(MSSZ,MYSS)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D57/NDZSE(MSEZ),DZSE(MSEZ,MZSE)
      COMMON/GR3D58/NDYSE(MSEZ),DYSE(MSEZ,MYSE)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D62/NDXEE(MEEZ),DXEE(MEEZ,MXEE)
      COMMON/GR3D63/NDYEE(MEEZ),DYEE(MEEZ,MYEE)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D67/NDXEC(MECZ),DXEC(MECZ,MXEC)
      COMMON/GR3D68/NDYEC(MECZ),DYEC(MECZ,MYEC)

      NUMEDG=0
      NUMCRN=0

C For all surfaces.
      DO 10 IS=1,MS
        INDXS(IS)=1
        NLINB(IS)=0
        NLIND(IS)=0
        NLINA(IS)=0
        NVRTB(IS)=0
        NVRTD(IS)=0
        NVRTA(IS)=0
        DO 20 IV=1,MVS
          XVRT(IS,IV)=0.
          ZVRT(IS,IV)=0.
   20   CONTINUE
        DO 30 IL=1,MLS
          ILINE(IS,IL,1)=0
          ILINE(IS,IL,2)=0
   30   CONTINUE
   10 CONTINUE

C For all edges.
      DO 40 IEG=1,MEZ
        INDXE(IEG)=0
        IVXEDG(IEG,1)=0
        IVXEDG(IEG,2)=0
        ISFEDG(IEG,1)=0
        ISFEDG(IEG,2)=0
        NDZE(IEG)=1
        DO 50 IDZ=1,MZE
          DZE(IEG,IDZ)=0.
   50   CONTINUE
        DO 60 IEZ=1,MZE
        DO 60 IEX=1,ME
        DO 60 IEY=1,ME
          ITPEDG(IEG,IEX,IEY,IEZ)=23
   60   CONTINUE
   40 CONTINUE

C For all corners.
      DO 80 ICR=1,MCZ
        INDXC(ICR)=0
        IVXCRN(ICR)=0
        ISFCRN(ICR,1)=0
        ISFCRN(ICR,2)=0
        ISFCRN(ICR,3)=0
        DO 90 IEZ=1,ME
        DO 90 IEX=1,ME
        DO 90 IEY=1,ME
          ITPCRN(ICR,IEX,IEY,IEZ)=23
   90   CONTINUE
   80 CONTINUE

C For all surface-surface connections.
      NUMSS=0
      DO 100 ISS=1,MSSZ
        INDXSS(ISS)=0
        DO 110 I=1,4
          IBDYSS(ISS,I)=0
  110   CONTINUE
        ILNBSS(ISS,1)=0
        ILNBSS(ISS,2)=0
        NDZSS(ISS)=0
        DO 120 I=1,MZSS
          DZSS(ISS,I)=0.
  120   CONTINUE
        NDYSS(ISS)=0
        DO 130 I=1,MYSS
          DYSS(ISS,I)=0.
  130   CONTINUE
  100 CONTINUE

C For all surface-edge connections.
      NUMSE=0
      DO 150 ISE=1,MSEZ
        INDXSE(ISE)=0
        DO 160 I=1,3
          IBDYSE(ISE,I)=0
  160   CONTINUE
        ILNBSE(ISE)=0
        NDZSE(ISE)=0
        DO 170 I=1,MZSE
          DZSE(ISE,I)=0.
  170   CONTINUE
        NDYSE(ISE)=0
        DO 180 I=1,MYSE
          DYSE(ISE,I)=0.
  180   CONTINUE
  150 CONTINUE

C For all edge-edge connections.
      NUMEE=0
      DO 200 IEE=1,MEEZ
        INDXEE(IEE)=0
        IVRTEE(IEE)=0
        IBDYEE(IEE,1)=0
        IBDYEE(IEE,2)=0
        NDXEE(IEE)=0
        DO 210 I=1,MXEE
          DXEE(IEE,I)=0.
  210   CONTINUE
        NDYEE(IEE)=0
        DO 220 I=1,MYEE
          DYEE(IEE,I)=0.
  220   CONTINUE
  200 CONTINUE

C For all edge-corner connections.
      NUMEC=0
      DO 250 IEC=1,MECZ
        INDXEC(IEC)=0
        IVRTEC(IEC)=0
        IBDYEC(IEC,1)=0
        IBDYEC(IEC,2)=0
        NDXEC(IEC)=0
        DO 260 I=1,MXEC
          DXEC(IEC,I)=0.
  260   CONTINUE
        NDYEC(IEC)=0
        DO 270 I=1,MYEC
          DYEC(IEC,I)=0.
  270   CONTINUE
  250 CONTINUE

      RETURN
      END

C *************************     INTCNC     ****************************
C INTCNC initializes the multi-D connection surfaces variables.
C *********************************************************************
      SUBROUTINE INTCNC
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D90/DCOSSS(MSSZ,3,3),DCOSSE(MSEZ,3,3)
      COMMON/GR3D91/DCOSEE(MEEZ,3,3),DCOSEC(MECZ,3,3)

      DIMENSION XNORM(3),YNORM(3),ZNORM(3)

      NUMSS=0
      NUMSE=0
      NUMEE=0
      NUMEC=0
      DO 10 IS1=1,NSUR

C Define the surface-surface connections.
        DO 20 IS2=IS1+1,NSUR
          DELTA=0.
          DO 30 I123=1,3
            DELTA=DELTA+ABS(DCOSS(IS2,2,I123)-DCOSS(IS1,2,I123))
   30     CONTINUE

C The two surfaces are not in the same plane (bypass).
          IF(DELTA.GT.0.01)GOTO 20

C The two surfaces are in the same plane. Check if they are adjacent.
          DO 40 IL1=1,NLINB(IS1)
            DO 50 IL2=1,NLINB(IS2)
              IV11=ILINB(IS1,IL1,1)
              IV12=ILINB(IS1,IL1,2)
              IV21=ILINB(IS2,IL2,1)
              IV22=ILINB(IS2,IL2,2)
              IF(IV11.EQ.IV22.AND.IV12.EQ.IV21)THEN
                NUMSS=NUMSS+1
                IF(NUMSS.GT.MSSZ)
     &          STOP "error (501): maximum number of srf-srf exceeded."
                IBDYSS(NUMSS,1)=IS1
                IBDYSS(NUMSS,2)=IS2
                ILNBSS(NUMSS,1)=IL1
                ILNBSS(NUMSS,2)=-IL2
                IVRTSS(NUMSS,1)=IV11
                IVRTSS(NUMSS,2)=IV12
                DO 60 I123=1,3
                  YNORM(I123)=DCOSS(IS1,2,I123)
   60           CONTINUE
                CALL LINORM(IV11,IV12,ZNORM)
                CALL CROSS(YNORM,ZNORM,XNORM)
                DIST=SQRT(XNORM(1)**2+XNORM(2)**2+XNORM(3)**2)
                DO 55 I123=1,3
                  DCOSSS(NUMSS,1,I123)=XNORM(I123)/DIST
                  DCOSSS(NUMSS,2,I123)=YNORM(I123)
                  DCOSSS(NUMSS,3,I123)=ZNORM(I123)
   55           CONTINUE
                GOTO 40
              ENDIF
   50       CONTINUE
   40     CONTINUE
   20   CONTINUE

C Define the surface-edge connections.
        DO 70 IL=1,NLINB(IS1)
          DO 80 IEG=1,NUMEDG
            IF(ILINB(IS1,IL,1).EQ.IVXEDG(IEG,1).AND.
     &         ILINB(IS1,IL,2).EQ.IVXEDG(IEG,2))THEN
               NUMSE=NUMSE+1
               IF(NUMSE.GT.MSEZ)
     &         STOP "error (502): maximum number of srf-edge exceeded."
               IBDYSE(NUMSE,1)=IS1
               IBDYSE(NUMSE,2)=IEG
               ILNBSE(NUMSE)=IL
               DO 65 I123=1,3
                 YNORM(I123)=DCOSS(IS1,2,I123)
                 ZNORM(I123)=DCOSE(IEG,3,I123)
   65          CONTINUE
               CALL CROSS(YNORM,ZNORM,XNORM)
               DIST=SQRT(XNORM(1)**2+XNORM(2)**2+XNORM(3)**2)
               DO 75 I123=1,3
                 DCOSSE(NUMSE,1,I123)=XNORM(I123)/DIST
                 DCOSSE(NUMSE,2,I123)=YNORM(I123)
                 DCOSSE(NUMSE,3,I123)=ZNORM(I123)
   75          CONTINUE
               GOTO 70
            ELSEIF(ILINB(IS1,IL,1).EQ.IVXEDG(IEG,2).AND.
     &         ILINB(IS1,IL,2).EQ.IVXEDG(IEG,1))THEN
               NUMSE=NUMSE+1
               IF(NUMSE.GT.MSEZ)
     &         STOP "error (503): maximum number of srf-edge exceeded."
               IBDYSE(NUMSE,1)=IS1
               IBDYSE(NUMSE,2)=IEG
               ILNBSE(NUMSE)=-IL
               DO 66 I123=1,3
                 YNORM(I123)=DCOSS(IS1,2,I123)
                 ZNORM(I123)=DCOSE(IEG,3,I123)
   66          CONTINUE
               CALL CROSS(YNORM,ZNORM,XNORM)
               DIST=SQRT(XNORM(1)**2+XNORM(2)**2+XNORM(3)**2)
               DO 77 I123=1,3
                 DCOSSE(NUMSE,1,I123)=XNORM(I123)/DIST
                 DCOSSE(NUMSE,2,I123)=YNORM(I123)
                 DCOSSE(NUMSE,3,I123)=ZNORM(I123)
   77          CONTINUE
               GOTO 70
            ENDIF
   80     CONTINUE
   70   CONTINUE
   10 CONTINUE

C Define the edge-edge connections.
      DO 90 IEG1=1,NUMEDG-1
        IV11=IVXEDG(IEG1,1)
        IV12=IVXEDG(IEG1,2)
      DO 90 IEG2=IEG1+1,NUMEDG
        IV21=IVXEDG(IEG2,1)
        IV22=IVXEDG(IEG2,2)
        IF(IV11.EQ.IV21)THEN
          CALL NEDEDG(-1,IEG1,IEG2,IV12,IV11,IV22,NUM)
        ELSEIF(IV11.EQ.IV22)THEN
          CALL NEDEDG(1,IEG1,IEG2,IV21,IV22,IV12,NUM)
        ELSEIF(IV12.EQ.IV21)THEN
          CALL NEDEDG(1,IEG1,IEG2,IV11,IV12,IV22,NUM)
        ELSEIF(IV12.EQ.IV22)THEN
          CALL NEDEDG(-1,IEG1,IEG2,IV11,IV12,IV21,NUM)
        ENDIF
        DO 95 IXYZ=1,3
        DO 95 I123=1,3
          DCOSEE(NUM,IXYZ,I123)=DCOSE(IEG1,IXYZ,I123)
   95   CONTINUE
   90 CONTINUE

C Define the edge-corner connections.
      DO 100 ICR=1,NUMCRN
        IS1=ISFCRN(ICR,1)
        IS2=ISFCRN(ICR,2)
        IS3=ISFCRN(ICR,3)
        IV1=IVXCRN(ICR)

C Define the three edges connected to this corner.
        CALL EDGNUM(IS2,IS3,IV1,IV1,IEG)
        NUMEC=NUMEC+1
        IF(NUMEC.GT.MECZ)
     &  STOP "error (504): maximum number of edge-crn exceeded."
        IVRTEC(NUMEC)=IV1
        IBDYEC(NUMEC,1)=IEG
        DO 105 IXYZ=1,3
        DO 105 I123=1,3
          DCOSEC(NUMEC,IXYZ,I123)=DCOSE(IEG,IXYZ,I123)
  105   CONTINUE
        IF(IS3.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=-ICR
        ELSEIF(IS2.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=ICR
        ELSE
          STOP "error (505): while gridding edge-corner."
        ENDIF
        CALL EDGNUM(IS3,IS1,IV1,IV1,IEG)
        NUMEC=NUMEC+1
        IF(NUMEC.GT.MECZ)
     &  STOP "error (506): maximum number of edge-crn exceeded."
        IVRTEC(NUMEC)=IV1
        IBDYEC(NUMEC,1)=IEG
        DO 106 IXYZ=1,3
        DO 106 I123=1,3
          DCOSEC(NUMEC,IXYZ,I123)=DCOSE(IEG,IXYZ,I123)
  106   CONTINUE
        IF(IS1.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=-ICR
        ELSEIF(IS3.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=ICR
        ELSE
          STOP "error (507): while gridding edge-corner."
        ENDIF
        CALL EDGNUM(IS1,IS2,IV1,IV1,IEG)
        NUMEC=NUMEC+1
        IF(NUMEC.GT.MECZ)
     &  STOP "error (508): maximum number of edge-crn exceeded."
        IVRTEC(NUMEC)=IV1
        IBDYEC(NUMEC,1)=IEG
        DO 107 IXYZ=1,3
        DO 107 I123=1,3
          DCOSEC(NUMEC,IXYZ,I123)=DCOSE(IEG,IXYZ,I123)
  107   CONTINUE
        IF(IS2.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=-ICR
        ELSEIF(IS1.EQ.ISFEDG(IEG,1))THEN
          IBDYEC(NUMEC,2)=ICR
        ELSE
          STOP "error (509): while gridding edge-corner ."
        ENDIF
  100 CONTINUE
      RETURN
      END

C *************************    INDCNC    *****************************
C INDCNC Initializes the connection surfaces indentations.
C *********************************************************************
      SUBROUTINE INDCNC
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)

C Define the surfaces internal indentations.
      DO 10 IS=1,NSUR
        Y0S(IS)=Y1S(IS)+THKS(IS)
   10 CONTINUE

C Define the srf-srf connections internal and external indentations.
      DO 20 ISS=1,NUMSS
        IS1=IBDYSS(ISS,1)
        IS2=IBDYSS(ISS,2)
        Y0SS(ISS)=AMAX1(Y0S(IS1),Y0S(IS2))
        Y1SS(ISS)=AMIN1(Y1S(IS1),Y1S(IS2))
   20 CONTINUE

C Define the srf-edg connections internal and external indentations.
      DO 30 ISE=1,NUMSE
        IS1=IBDYSE(ISE,1)
        Y0SE(ISE)=Y0S(IS1)
        Y1SE(ISE)=0.
        DO 40 IS2=1,NSUR
          IF(IS1.NE.IS2)THEN
            DELTA=0.
            DO 50 I123=1,3
              DELTA=DELTA+ABS(DCOSS(IS2,2,I123)-DCOSS(IS1,2,I123))
   50       CONTINUE

C The two surfaces are in the same plane.
            IF(DELTA.LT.0.01)THEN
              Y0SE(ISE)=AMAX1(Y0SE(ISE),Y0S(IS2))
            ENDIF
          ENDIF
   40   CONTINUE
   30 CONTINUE
      RETURN
      END

C **************************     CHCK3D    ****************************
C CHCK3D check the 3D configuration information and generates warning
C massege when un-acceptable information is detected.
C *********************************************************************
      SUBROUTINE CHCK3D(IWRNG,IERRR)
#include "building.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D06/INDXSS(MSSZ),INDXSE(MSEZ),INDXEE(MEEZ),INDXEC(MECZ)
      COMMON/GR3D07/Y0S(MS),Y0SS(MSSZ),Y0SE(MSEZ)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)

      CHARACTER MSG*124

      IWRNG=0
      IERRR=0
      DO 10 ISS=1,NUMSS
        IF(INDXSS(ISS).EQ.3)THEN
          DO 20 I=1,2
            IS=IBDYSS(ISS,I)
            IF(INDXS(IS).NE.3)THEN
              IERRR=IERRR+1
              WRITE(MSG,'(A,I2,3A)')'ERROR: surface (',IS,') is not ',
     &                  'discretized; However, it is connected by a ',
     &                  'surface-surface connection.'
              CALL EDISP(IUOUT,MSG)
            ENDIF
   20     CONTINUE
        ENDIF

C First, check that the given surfaces indentations are acceptable.
        IS1=IBDYSS(ISS,1)
        IS2=IBDYSS(ISS,2)
        IF(Y1S(IS1).GT.Y0S(IS2).OR.Y1S(IS2).GT.Y0S(IS1))THEN
            IERRR=IERRR+1
            WRITE(MSG,'(2A,2(I2,A))')' The indentations for the ',
     &            'two adjacent and co-planer surfaces (',IS1,',',IS2,
     &            ') are not acceptable.'
            CALL EDISP(IUOUT,MSG)
        ENDIF
   10 CONTINUE
      DO 30 ISE=1,NUMSE
        IF(INDXSE(ISE).EQ.3)THEN
          IS=IBDYSE(ISE,1)
          IEG=IBDYSE(ISE,2)
          IF(INDXS(IS).NE.3)THEN
            IERRR=IERRR+1
            WRITE(MSG,'(A,I2,3A)')'ERROR: surface (',IS,') is not ',
     &                'discretized; However, it is connected by a ',
     &                'surface-edge connection.'
            CALL EDISP(IUOUT,MSG)
          ENDIF
          IF(INDXE(IEG).EQ.0)THEN
            IERRR=IERRR+1
            WRITE(MSG,'(A,I2,3A)')'ERROR: edge (',IEG,') is not ',
     &                'defined; However, it is connected by a ',
     &                'surface-edge connection.'
            CALL EDISP(IUOUT,MSG)
          ENDIF
        ENDIF
   30 CONTINUE
      DO 40 IEE=1,NUMEE
        IF(INDXEE(IEE).EQ.3)THEN
          DO 50 I=1,2
            IEG=ABS(IBDYEE(IEE,I))
            IF(INDXE(IEG).EQ.0)THEN
              IERRR=IERRR+1
              WRITE(MSG,'(A,I2,3A)')'ERROR: edge (',IEG,') is not ',
     &                  'defined; However, it is connected by an ',
     &                  'edge-edge connection.'
              CALL EDISP(IUOUT,MSG)
            ENDIF
   50     CONTINUE
        ENDIF
   40 CONTINUE
      DO 60 IEC=1,NUMEC
        IF(INDXEC(IEC).EQ.3)THEN
          IEG=ABS(IBDYEC(IEC,1))
          ICR=ABS(IBDYEC(IEC,2))
          IF(INDXE(IEG).EQ.0)THEN
            IERRR=IERRR+1
            WRITE(MSG,'(A,I2,3A)')'ERROR: edge (',IEG,') is not ',
     &                'defined; However, it is connected by an ',
     &                'edge-corner connection.'
            CALL EDISP(IUOUT,MSG)
          ENDIF
          IF(INDXC(ICR).EQ.0)THEN
            IERRR=IERRR+1
            WRITE(MSG,'(A,I2,3A)')'ERROR: corner (',ICR,') is not ',
     &                'defined; However, it is connected by a ',
     &                'edge-corner connection.'
            CALL EDISP(IUOUT,MSG)
          ENDIF
        ENDIF
   60 CONTINUE
      RETURN
      END

C **************************   ADDSSY    ******************************
C ADDSSY checks if no interval is defined at the rquired level in Y
C it will create one and updates the associated arrays. [srf-srf]
C *********************************************************************
      SUBROUTINE ADDSSY(ISS,YVALU)
#include "building.h"


      COMMON/GR3D53/NDYSS(MSSZ),DYSS(MSSZ,MYSS)

      IF(YVALU.LT.1.E-3)RETURN
      DO 10 IDY=1,NDYSS(ISS)
        IF(ABS(DYSS(ISS,IDY)-YVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DYSS(ISS,IDY)-YVALU).GT.1.E-2)THEN
          DO 20 IDY2=NDYSS(ISS),IDY,-1
            DYSS(ISS,IDY2+1)=DYSS(ISS,IDY2)
   20     CONTINUE
          DYSS(ISS,IDY)=YVALU
          NDYSS(ISS)=NDYSS(ISS)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((YVALU-DYSS(ISS,NDYSS(ISS))).GT.0.01)
     &STOP "error (510): while gridding srf-srf connection surfaces."
      RETURN
      END

C **************************   ADDSSZ    ******************************
C ADDSSZ checks if no interval is defined at the rquired z-level in 
C srf-srf, it will create one and updates the associated arrays.
C *********************************************************************
      SUBROUTINE ADDSSZ(ISS,ZVALU)
#include "building.h"


      COMMON/GR3D52/NDZSS(MSSZ),DZSS(MSSZ,MZSS)

      IF(ZVALU.LT.1.E-3)RETURN
      DO 10 IDZ=1,NDZSS(ISS)
        IF(ABS(DZSS(ISS,IDZ)-ZVALU).LT.5.E-2)THEN
          RETURN
        ELSEIF((DZSS(ISS,IDZ)-ZVALU).GT.5.E-2)THEN
          DO 20 IDZ2=NDZSS(ISS),IDZ,-1
            DZSS(ISS,IDZ2+1)=DZSS(ISS,IDZ2)
   20     CONTINUE
          DZSS(ISS,IDZ)=ZVALU
          NDZSS(ISS)=NDZSS(ISS)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((ZVALU-DZSS(ISS,NDZSS(ISS))).GT.0.01)
     &STOP "error (511): while gridding srf-srf connection surfaces."
      RETURN
      END

C **************************   ADDSEY    ******************************
C ADDSEY checks if no interval is defined at the rquired y-level in
C srf-edg, it will create one and updates the associated arrays.
C *********************************************************************
      SUBROUTINE ADDSEY(ISE,YVALU)
#include "building.h"

      COMMON/GR3D58/NDYSE(MSEZ),DYSE(MSEZ,MYSE)

      IF(YVALU.LT.1.E-3)RETURN
      DO 10 IY=1,NDYSE(ISE)
        IF(ABS(DYSE(ISE,IY)-YVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DYSE(ISE,IY)-YVALU).GT.1.E-2)THEN
          DO 20 IY2=NDYSE(ISE),IY,-1
            DYSE(ISE,IY2+1)=DYSE(ISE,IY2)
   20     CONTINUE
          DYSE(ISE,IY)=YVALU
          NDYSE(ISE)=NDYSE(ISE)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((YVALU-DYSE(ISE,NDYSE(ISE))).GT.0.01)
     &STOP "error (512): while gridding surface-edge."
      RETURN
      END

C **************************   ADDSEZ    ******************************
C ADDSEZ checks if no interval is defined at the rquired level in Z
C it will create one and updates the associated arrays.
C *********************************************************************
      SUBROUTINE ADDSEZ(ISE,ZVALU)
#include "building.h"


      COMMON/GR3D57/NDZSE(MSEZ),DZSE(MSEZ,MZSE)

      IF(ZVALU.LT.1.E-3)RETURN
      DO 10 IDZ=1,NDZSE(ISE)
        IF(ABS(DZSE(ISE,IDZ)-ZVALU).LT.5.E-2)THEN
          RETURN
        ELSEIF((DZSE(ISE,IDZ)-ZVALU).GT.5.E-2)THEN
          DO 20 IDZ2=NDZSE(ISE),IDZ,-1
            DZSE(ISE,IDZ2+1)=DZSE(ISE,IDZ2)
   20     CONTINUE
          DZSE(ISE,IDZ)=ZVALU
          NDZSE(ISE)=NDZSE(ISE)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((ZVALU-DZSE(ISE,NDZSE(ISE))).GT.0.01)
     &STOP "error (513): while gridding surface-edge."
      RETURN
      END

C **************************   ADDEEX    ******************************
C ADDEEX checks if no interval is defined at the rquired level in X
C it will create one and updates the associated arrays. [edge-edge]
C *********************************************************************
      SUBROUTINE ADDEEX(IEE,XVALU)
#include "building.h"

      COMMON/GR3D62/NDXEE(MEEZ),DXEE(MEEZ,MXEE)

      IF(XVALU.LT.1.E-3)RETURN
      DO 10 IX1=1,NDXEE(IEE)
        IF(ABS(DXEE(IEE,IX1)-XVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DXEE(IEE,IX1)-XVALU).GT.1.E-2)THEN
          DO 20 IX2=NDXEE(IEE),IX1,-1
            DXEE(IEE,IX2+1)=DXEE(IEE,IX2)
   20     CONTINUE
          DXEE(IEE,IX1)=XVALU
          NDXEE(IEE)=NDXEE(IEE)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((XVALU-DXEE(IEE,NDXEE(IEE))).GT.0.01)
     &STOP "error (514): while gridding edge-edge."
      RETURN
      END

C **************************   ADDEEY    ******************************
C ADDEEY checks if no interval is defined at the rquired level in Y
C it will create one and updates the associated arrays. [edge-edge]
C *********************************************************************
      SUBROUTINE ADDEEY(IEE,YVALU)
#include "building.h"

      COMMON/GR3D63/NDYEE(MEEZ),DYEE(MEEZ,MYEE)

      IF(YVALU.LT.1.E-3)RETURN
      DO 10 IY=1,NDYEE(IEE)
        IF(ABS(DYEE(IEE,IY)-YVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DYEE(IEE,IY)-YVALU).GT.1.E-2)THEN
          DO 20 IY2=NDYEE(IEE),IY,-1
            DYEE(IEE,IY2+1)=DYEE(IEE,IY2)
   20     CONTINUE
          DYEE(IEE,IY)=YVALU
          NDYEE(IEE)=NDYEE(IEE)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((YVALU-DYEE(IEE,NDYEE(IEE))).GT.0.01)
     &STOP "error (515): while gridding edge-edge."
      RETURN
      END

C **************************   ADDECX    ******************************
C ADDECX checks if no interval is defined at the rquired level in X
C it will create one and updates the associated arrays. [edge-corner]
C *********************************************************************
      SUBROUTINE ADDECX(IEC,XVALU)
#include "building.h"

      COMMON/GR3D67/NDXEC(MECZ),DXEC(MECZ,MXEC)

      IF(XVALU.LT.1.E-3)RETURN
      DO 10 IX1=1,NDXEC(IEC)
        IF(ABS(DXEC(IEC,IX1)-XVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DXEC(IEC,IX1)-XVALU).GT.1.E-2)THEN
          DO 20 IX2=NDXEC(IEC),IX1,-1
            DXEC(IEC,IX2+1)=DXEC(IEC,IX2)
   20     CONTINUE
          DXEC(IEC,IX1)=XVALU
          NDXEC(IEC)=NDXEC(IEC)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((XVALU-DXEC(IEC,NDXEC(IEC))).GT.0.01)
     &STOP "error (516): while gridding edge-corner."
      RETURN
      END

C **************************   ADDECY    ******************************
C ADDECY checks if no interval is defined at the rquired level in Y
C it will create one and updates the associated arrays. [edge-corner]
C *********************************************************************
      SUBROUTINE ADDECY(IEC,YVALU)
#include "building.h"

      COMMON/GR3D68/NDYEC(MECZ),DYEC(MECZ,MYEC)

      IF(YVALU.LT.1.E-3)RETURN
      DO 10 IY=1,NDYEC(IEC)
        IF(ABS(DYEC(IEC,IY)-YVALU).LT.1.E-2)THEN
          RETURN
        ELSEIF((DYEC(IEC,IY)-YVALU).GT.1.E-2)THEN
          DO 20 IY2=NDYEC(IEC),IY,-1
            DYEC(IEC,IY2+1)=DYEC(IEC,IY2)
   20     CONTINUE
          DYEC(IEC,IY)=YVALU
          NDYEC(IEC)=NDYEC(IEC)+1
          RETURN
        ENDIF
   10 CONTINUE
      IF((YVALU-DYEC(IEC,NDYEC(IEC))).GT.0.01)
     &STOP "error (517): while gridding edge-corner."
      RETURN
      END

C *************************      CNCLNE     ***************************
C CNCLNE defines connection surface type for a control volume 
C ( ICL > 0 : boundary line, ICL < 0 : another control volume ).
C *********************************************************************
      SUBROUTINE CNCLNE(IS,I,J,ICL)
#include "building.h"

      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)

      DO 10 IL=1,NLINB(IS)
      DO 10 IV=1,NVLN(IS,IL)-1
        IV1=IVLN(IS,IL,IV)
        IV2=IVLN(IS,IL,IV+1)
        IF((I.EQ.IV1.AND.J.EQ.IV2).OR.(I.EQ.IV2.AND.J.EQ.IV1))THEN
          ICL=IL
          RETURN
        ENDIF
   10 CONTINUE
      ICL=0
      RETURN
      END

C **************************     FDSCV2     ***************************
C FDSCV2 finds the adjacent control volume for the current one.
C *********************************************************************
      SUBROUTINE FDSCV2(IS,ICV1,I,J,ICV2)
#include "building.h"

      COMMON/GR3D44/NCVSF(MS),NCVV(MS,MCVS),ICVV(MS,MCVS,MCVV)

      DO 10 ICV=1,NCVSF(IS)
        IF(ICV.EQ.ICV1)GOTO 10
        DO 20 IV=1,NCVV(IS,ICV)
          IV1=ICVV(IS,ICV,IV)
          IF(IV.EQ.NCVV(IS,ICV))THEN
            IV2=ICVV(IS,ICV,1)
          ELSE
            IV2=ICVV(IS,ICV,IV+1)
          ENDIF
          IF((I.EQ.IV1.AND.J.EQ.IV2).OR.(I.EQ.IV2.AND.J.EQ.IV1))THEN
            ICV2=ICV
            RETURN
          ENDIF
   20   CONTINUE
   10 CONTINUE
      STOP "error (518): while gridding a surface."
      END

C **************************     ISEFNS     ***************************
C ISEFNS finds the type and number of the adjacent connection surface
C for the current surface.
C *********************************************************************
      SUBROUTINE ISEFNS(IS,ICL,ISSE,NSSE,ILN)
#include "building.h"

      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)

      DO 10 ISE=1,NUMSE
        IF(IS.EQ.IBDYSE(ISE,1).AND.ICL.EQ.ABS(ILNBSE(ISE)))THEN
          ISSE=2
          NSSE=ISE
          ILN=ILNBSE(ISE)
          RETURN
        ENDIF
   10 CONTINUE
      DO 20 ISS=1,NUMSS
        IF(IS.EQ.IBDYSS(ISS,1).AND.ICL.EQ.ABS(ILNBSS(ISS,1)))THEN
          ISSE=1
          NSSE=ISS
          ILN=ILNBSS(ISS,1)
          RETURN
        ELSEIF(IS.EQ.IBDYSS(ISS,2).AND.ICL.EQ.ABS(ILNBSS(ISS,2)))THEN
          ISSE=1
          NSSE=ISS
          ILN=ILNBSS(ISS,2)
          RETURN
        ENDIF
   20 CONTINUE
      STOP "error (519): while gridding a surface."
      END

C *****************************   ISEFNE   ****************************
C ISEFNE finds the number of the srf-edge connection surface.
C *********************************************************************
      FUNCTION ISEFNE(IEG,IS)
#include "building.h"

      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)

      DO 10 ISE=1,NUMSE
        IF(IS.EQ.IBDYSE(ISE,1).AND.IEG.EQ.IBDYSE(ISE,2))THEN
          ISEFNE=ISE
          RETURN
        ENDIF
   10 CONTINUE
      STOP "error (520): undefined surface-edge connection surface."
      END

C ****************************   NEDEDG   *****************************
C NEDEDG define the number of the coolinear edge-edge connection. If
C it does not exist, a new one will be created.
C *********************************************************************
      SUBROUTINE NEDEDG(ITYP,IEG1,IEG2,IV1,IV2,IV3,NUM)
#include "building.h"

      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)

C Check if a co-linear edge-edge connection exists at IV2.
      DO 10 IEE=1,NUMEE
        IF(IVRTEE(IEE).EQ.IV2)THEN
          NUM=IEE
          RETURN
        ENDIF
   10 CONTINUE

C No edge-edge connection found and creation of new one is not required.
      IF(ITYP.EQ.0.OR.IV1.EQ.IV2.OR.IV1.EQ.IV3.OR.IV2.EQ.IV3.OR.
     &   IEG1.EQ.IEG2)THEN
        NUM=0
        RETURN
      ENDIF

C Create new co-linear edge-edge connection the these two edges are
C co-linear.
      CALL ANGCOS(IV1,IV2,IV3,THETA)
      IF(ABS(THETA-180.).LT.20.)THEN
        NUMEE=NUMEE+1
        IF(NUMEE.GT.MEEZ)
     &  STOP "error (521): maximum number of edge-edge exceeded."
        NUM=NUMEE
        IVRTEE(NUM)=IV2
        IBDYEE(NUM,1)=IEG1
        IF(ITYP.EQ.1)THEN
          IBDYEE(NUM,2)=IEG2
        ELSEIF(ITYP.EQ.-1)THEN
          IBDYEE(NUM,2)=-IEG2
        ELSE
          STOP "error (522): undefined edge-edge connection surface."
        ENDIF
      ENDIF
      RETURN
      END

C ****************************   IECFNE   *****************************
C IECFNE finds the number of the edge-corner connection surface.
C *********************************************************************
      FUNCTION IECFNE(ICN,IS1,IS2)
#include "building.h"

      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)

      DO 10 IEC=1,NUMEC
        IF(IVXCRN(ICN).EQ.IVRTEC(IEC))THEN
          IEDG=IBDYEC(IEC,1)
          IF((IS1.EQ.ISFEDG(IEDG,1).AND.IS2.EQ.ISFEDG(IEDG,2)).OR.
     &       (IS1.EQ.ISFEDG(IEDG,2).AND.IS2.EQ.ISFEDG(IEDG,1)))THEN
             IECFNE=IEC
             RETURN
          ENDIF
        ENDIF
   10 CONTINUE
      STOP "error (523): undefined edge-corner connection surface."
      END

C ***************************     IEECFN    ***************************
C IEECFN finds the type (edge-edge or edge-corner) and number of the 
C adjacent connection surface for the current edge.
C *********************************************************************
      SUBROUTINE IEECFN(IEG,IV,IEEC,NEEC)
#include "building.h"

      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)

C Check if there is a corner at the current vertex.
      DO 10 ICN=1,NUMCRN
        IF(IVXCRN(ICN).EQ.IV)THEN
          ICR=ICN
          GOTO 20
        ENDIF
   10 CONTINUE
      ICR=0

C Edge-edge connection is detected.
   20 IF(ICR.EQ.0)THEN
        IEEC=1
        CALL NEDEDG(0,IEG,IEG,IV,IV,IV,NEEC)
        RETURN

C The connection surface is edge-corner connection.
      ELSE
        IEEC=2
        NEEC=IECFNE(ICR,ISFEDG(IEG,1),ISFEDG(IEG,2))
        RETURN
      ENDIF
      STOP "error (524): undefined connection surface."
      END

C **************************     CMVRTS    ****************************
C CMVRTS determines if a vertex is included in a surface.
C *********************************************************************
      SUBROUTINE CMVRTS(IV1,IS2,YES)
#include "building.h"
#include "geometry.h"

      LOGICAL YES

      DO 10 J=1,NVER(IS2)
        IV2=JVN(IS2,J)
        IF(IV1.EQ.IV2)THEN
          YES=.TRUE.
          RETURN
        ENDIF
   10 CONTINUE
      YES=.FALSE.
      RETURN
      END

C *****************************  PROJCT   *****************************
C PROJCT determines the projection of a node defined by (XC,ZC)
C on a line defined by (IV1,IV2).
C *********************************************************************
      SUBROUTINE PROJCT(IS,IV1,IV2,XC,ZC,XPRJ,ZPRJ)
#include "building.h"

      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      XA=XINT(IS,IV1)
      ZA=ZINT(IS,IV1)
      XB=XINT(IS,IV2)
      ZB=ZINT(IS,IV2)
      DOM=(XB-XA)**2+(ZB-ZA)**2
      IF(DOM.LT.1.E-6)STOP "error (525): devision by zero."
      DUMM=(XB-XA)*XA+(ZB-ZA)*ZA-XB*XC+XA*XC-ZB*ZC+ZA*ZC
      XPRJ=XA-DUMM*(XB-XA)/DOM
      ZPRJ=ZA-DUMM*(ZB-ZA)/DOM
      RETURN
      END

C *****************************  RELPOS   *****************************
C RELPOS determines the relative location of a node defined by (XC,ZC)
C with respect to the connection surface defined by two vertices 
C (IV1,IV2).
C *********************************************************************
      SUBROUTINE RELPOS(IS,IV1,IV2,XC,ZC,XREL,ZREL)
#include "building.h"

      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      XA=XINT(IS,IV1)
      ZA=ZINT(IS,IV1)
      CALL PROJCT(IS,IV1,IV2,XC,ZC,XPRJ,ZPRJ)
      XREL=SQRT((XC-XPRJ)**2+(ZC-ZPRJ)**2)
      ZREL=SQRT((XA-XPRJ)**2+(ZA-ZPRJ)**2)
      RETURN
      END

C *****************************  EFDIST   *****************************
C EFDIST Function returns the effective distance between two nodes of
C which one belonges to srf-edge connection surface.
C *********************************************************************
      FUNCTION EFDIST(ISP,NSP,XA,YA,ZA,XB,YB,ZB)
#include "building.h"

      COMMON/GR3D90/DCOSSS(MSSZ,3,3),DCOSSE(MSEZ,3,3)
      COMMON/GR3D91/DCOSEE(MEEZ,3,3),DCOSEC(MECZ,3,3)

      DIMENSION VECT1(3),VECT2(3)

      IF(ISP.EQ.-4)THEN
        ISS=NSP
        VECT1(1)=DCOSSS(ISS,1,1)
        VECT1(2)=DCOSSS(ISS,1,2)
        VECT1(3)=DCOSSS(ISS,1,3)
      ELSEIF(ISP.EQ.-5)THEN
        ISE=NSP
        VECT1(1)=DCOSSE(ISE,1,1)
        VECT1(2)=DCOSSE(ISE,1,2)
        VECT1(3)=DCOSSE(ISE,1,3)
      ELSEIF(ISP.EQ.-6)THEN
        IEE=NSP
        VECT1(1)=DCOSEE(IEE,3,1)
        VECT1(2)=DCOSEE(IEE,3,2)
        VECT1(3)=DCOSEE(IEE,3,3)
      ELSEIF(ISP.EQ.-7)THEN
        IEC=NSP
        VECT1(1)=DCOSEC(IEC,3,1)
        VECT1(2)=DCOSEC(IEC,3,2)
        VECT1(3)=DCOSEC(IEC,3,3)
      ENDIF
      RDIST=SQRT((XA-XB)**2+(YA-YB)**2+(ZA-ZB)**2)
      VECT2(1)=(XB-XA)/RDIST
      VECT2(2)=(YB-YA)/RDIST
      VECT2(3)=(ZB-ZA)/RDIST
      CDIS=VECT1(1)*VECT2(1)+VECT1(2)*VECT2(2)+VECT1(3)*VECT2(3)
      IF(ABS(CDIS).LT.1.E-8)
     &STOP "error (526): zero perpendicular heat flow area."
      EFDIST=ABS(RDIST/CDIS)
      RETURN
      END

C **************************   PNTLNE   *******************************
C PNTLNE checks if a point is on a line.
C *********************************************************************
      SUBROUTINE PNTLNE(IS,IL,IV,IER)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      IER=-1
      IA=ILINE(IS,IL,1)
      IB=ILINE(IS,IL,2)
      AX=XVRT(IS,IA)
      AZ=ZVRT(IS,IA)
      BX=XVRT(IS,IB)
      BZ=ZVRT(IS,IB)
      VX=XINT(IS,IV)
      VZ=ZINT(IS,IV)
      XMX=AMAX1(AX,BX)+1.E-3
      XMN=AMIN1(AX,BX)-1.E-3
      ZMX=AMAX1(AZ,BZ)+1.E-3
      ZMN=AMIN1(AZ,BZ)-1.E-3
      IF(VX.GT.XMX.OR.VX.LT.XMN.OR.VZ.GT.ZMX.OR.VZ.LT.ZMN)RETURN
      IF(ABS(BX-AX).GT.1.E-2)THEN
        Z=(VX-AX)*(BZ-AZ)/(BX-AX)+AZ
        IF(ABS(Z-VZ).LT.1.E-3)IER=0
      ELSEIF(ABS(BZ-AZ).GT.1.E-2)THEN
        X=(VZ-AZ)*(BX-AX)/(BZ-AZ)+AX
        IF(ABS(X-VX).LT.1.E-3)IER=0
      ENDIF
      RETURN
      END

C *****************************   RORDER   ****************************
C RORDER  reorders the vertex list for all the lines within a surface.
C *********************************************************************
      SUBROUTINE RORDER(IS)
#include "building.h"

      COMMON/GR3D03/NLINB(MS),NLIND(MS),NLINA(MS)
      COMMON/GR3D04/ILINE(MS,MLS,2),ILINB(MS,MLBS,2)
      COMMON/GR3D42/NVLN(MS,MLS),IVLN(MS,MLS,MLS)
      COMMON/GR3D43/XINT(MS,MGV),ZINT(MS,MGV),NCVS(MS,MGV)

      DIMENSION DEL(MGV),IVLNTP(MGV)

      LOGICAL COUNTOK(MGV)

C Re-order the vertex list for the lines.
      DO 10 IL=1,NLINA(IS)

C save the old order in a temporary storage.
        DO 20 IV=1,NVLN(IS,IL)
          IVLNTP(IV)=IVLN(IS,IL,IV)
   20   CONTINUE
        IV1=ILINE(IS,IL,1)
        DO 30 IV=1,NVLN(IS,IL)
          IV2=IVLN(IS,IL,IV)
          DELTA=SQRT((XINT(IS,IV2)-XINT(IS,IV1))**2+
     &                 (ZINT(IS,IV2)-ZINT(IS,IV1))**2)
          DEL(IV)=DELTA
          COUNTOK(IV)=.TRUE.
   30   CONTINUE
        ICOUNT=0
        DO 40 ICNT=1,NVLN(IS,IL)
          ICOUNT=ICOUNT+1
          DELMIN=1.E+10
          DO 50 IV=1,NVLN(IS,IL)
            IF(COUNTOK(IV).AND.DEL(IV).LT.DELMIN)THEN
              INDX=IV
              DELMIN=DEL(IV)
            ENDIF
   50     CONTINUE
          IVLN(IS,IL,ICOUNT)=IVLNTP(INDX)
          COUNTOK(INDX)=.FALSE.
   40   CONTINUE
   10 CONTINUE
      RETURN
      END

C *******************************  ANGL2G  ****************************
C ANGL2G determines the angle between two lines in space via cosine law.
C *********************************************************************
      SUBROUTINE ANGL2G(ISP1,IFP1,THETA2D)
#include "building.h"

      COMMON/G1T/XFT(MV),ZFT(MV)

      PI=3.1415927
      DX=XFT(IFP1)-XFT(ISP1)
      DZ=ZFT(IFP1)-ZFT(ISP1)
      DR=SQRT(DX**2+DZ**2)
      THETA2D=ACOS(DX/DR)*180./PI
      IF(DZ.LT.0.)THETA2D=360.0-THETA2D
      RETURN
      END

C ******************************   ANGL2P   ***************************
C ANGL2P determines the angle between a line between two points and the
C positive x axes in the transformed plane.
C *********************************************************************
      SUBROUTINE ANGL2P(IS,ISP1,IFP1,THETA2D)
#include "building.h"

      COMMON/GR3D02/XVRT(MS,MVS),ZVRT(MS,MVS)

      PI=3.1415927
      DX=XVRT(IS,IFP1)-XVRT(IS,ISP1)
      DZ=ZVRT(IS,IFP1)-ZVRT(IS,ISP1)
      DR=SQRT(DX**2+DZ**2)
      THETA2D=ACOS(DX/DR)*180./PI
      IF(DZ.LT.0.)THETA2D=360.0-THETA2D
      RETURN
      END

C ***************************    PLNORM    ****************************
C PLNORM defines the direction cosines for a plane in space.
C *********************************************************************
      SUBROUTINE PLNORM(IV1,IV2,IV3,PLNRM)
#include "building.h"
#include "geometry.h"

      DIMENSION VECT1(3),VECT2(3),PLNRM(3)

      VECT1(1)=X(IV2)-X(IV1)
      VECT1(2)=Y(IV2)-Y(IV1)
      VECT1(3)=Z(IV2)-Z(IV1)
      VECT2(1)=X(IV3)-X(IV1)
      VECT2(2)=Y(IV3)-Y(IV1)
      VECT2(3)=Z(IV3)-Z(IV1)
      CALL CROSS(VECT1,VECT2,PLNRM)
      RPLN=SQRT(PLNRM(1)**2+PLNRM(2)**2+PLNRM(3)**2)
      PLNRM(1)=PLNRM(1)/RPLN
      PLNRM(2)=PLNRM(2)/RPLN
      PLNRM(3)=PLNRM(3)/RPLN
      RETURN
      END

C ***************************    LINORM     ***************************
C LINORM defines the direction cosines for a line in space.
C *********************************************************************
      SUBROUTINE LINORM(IV1,IV2,XYZLIN)
#include "building.h"
#include "geometry.h"

      DIMENSION XYZLIN(3)

      XYZLIN(1)=X(IV2)-X(IV1)
      XYZLIN(2)=Y(IV2)-Y(IV1)
      XYZLIN(3)=Z(IV2)-Z(IV1)
      RLINE=SQRT(XYZLIN(1)**2+XYZLIN(2)**2+XYZLIN(3)**2)
      XYZLIN(1)=XYZLIN(1)/RLINE
      XYZLIN(2)=XYZLIN(2)/RLINE
      XYZLIN(3)=XYZLIN(3)/RLINE
      RETURN
      END

C ***************************     ANGCOS     **************************
C ANGCOS defines the angle between two vectors in space from cosine law.
C *********************************************************************
      SUBROUTINE ANGCOS(IV1,IV2,IV3,THETA)
#include "building.h"
#include "geometry.h"

      PI=3.1415927
      DX1=X(IV1)-X(IV2)
      DY1=Y(IV1)-Y(IV2)
      DZ1=Z(IV1)-Z(IV2)
      DX2=X(IV3)-X(IV2)
      DY2=Y(IV3)-Y(IV2)
      DZ2=Z(IV3)-Z(IV2)
      R1=SQRT(DX1**2.+DY1**2.+DZ1**2.)
      R2=SQRT(DX2**2.+DY2**2.+DZ2**2.)
      COSTHETA=(DX1*DX2+DY1*DY2+DZ1*DZ2)/(R1*R2)
      THETA=ACOS(COSTHETA)*180./PI
      RETURN
      END

C *************************    GCOORD    ******************************
C GCOORD determines the global coordinate.
C *********************************************************************
      SUBROUTINE GCOORD(ISP,NSP,XND,YND,ZND,GX,GY,GZ)
#include "building.h"
#include "geometry.h"

      COMMON/GR3D01/INDXS(MS),THKS(MS),DCOSS(MS,3,3)
      COMMON/GR3D08/Y1S(MS),Y1SS(MSSZ),Y1SE(MSEZ)
      COMMON/GR3D21/NUMEDG,INDXE(MEZ),DCOSE(MEZ,3,3)
      COMMON/GR3D24/ISFEDG(MEZ,2),IVXEDG(MEZ,2)
      COMMON/GR3D31/NUMCRN,INDXC(MCZ),DCOSC(MCZ,3,3)
      COMMON/GR3D34/ISFCRN(MCZ,3),IVXCRN(MCZ)
      COMMON/GR3D51/NUMSS,IBDYSS(MSSZ,2),ILNBSS(MSSZ,2),IVRTSS(MSSZ,2)
      COMMON/GR3D56/NUMSE,IBDYSE(MSEZ,2),ILNBSE(MSEZ)
      COMMON/GR3D61/NUMEE,IBDYEE(MEEZ,2),IVRTEE(MEEZ)
      COMMON/GR3D66/NUMEC,IBDYEC(MECZ,2),IVRTEC(MECZ)
      COMMON/GR3D90/DCOSSS(MSSZ,3,3),DCOSSE(MSEZ,3,3)
      COMMON/GR3D91/DCOSEE(MEEZ,3,3),DCOSEC(MECZ,3,3)

      IF(ISP.GT.0)THEN
        IS=ISP
        GX=XND*DCOSS(IS,1,1)+YND*DCOSS(IS,2,1)+ZND*DCOSS(IS,3,1)
        GY=XND*DCOSS(IS,1,2)+YND*DCOSS(IS,2,2)+ZND*DCOSS(IS,3,2)
        GZ=XND*DCOSS(IS,1,3)+YND*DCOSS(IS,2,3)+ZND*DCOSS(IS,3,3)
        IP1=JVN(IS,1)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-2)THEN
        IEG=NSP
        GX=XND*DCOSE(IEG,1,1)+YND*DCOSE(IEG,2,1)+ZND*DCOSE(IEG,3,1)
        GY=XND*DCOSE(IEG,1,2)+YND*DCOSE(IEG,2,2)+ZND*DCOSE(IEG,3,2)
        GZ=XND*DCOSE(IEG,1,3)+YND*DCOSE(IEG,2,3)+ZND*DCOSE(IEG,3,3)
        IP1=IVXEDG(IEG,1)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-3)THEN
        ICR=NSP
        GX=XND*DCOSC(ICR,1,1)+YND*DCOSC(ICR,2,1)+ZND*DCOSC(ICR,3,1)
        GY=XND*DCOSC(ICR,1,2)+YND*DCOSC(ICR,2,2)+ZND*DCOSC(ICR,3,2)
        GZ=XND*DCOSC(ICR,1,3)+YND*DCOSC(ICR,2,3)+ZND*DCOSC(ICR,3,3)
        IP1=IVXCRN(ICR)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-4)THEN
        ISS=NSP
        YND=YND+Y1SS(ISS)
        GX=XND*DCOSSS(ISS,1,1)+YND*DCOSSS(ISS,2,1)+ZND*DCOSSS(ISS,3,1)
        GY=XND*DCOSSS(ISS,1,2)+YND*DCOSSS(ISS,2,2)+ZND*DCOSSS(ISS,3,2)
        GZ=XND*DCOSSS(ISS,1,3)+YND*DCOSSS(ISS,2,3)+ZND*DCOSSS(ISS,3,3)
        IP1=IVRTSS(ISS,1)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-5)THEN
        ISE=NSP
        GX=XND*DCOSSE(ISE,1,1)+YND*DCOSSE(ISE,2,1)+ZND*DCOSSE(ISE,3,1)
        GY=XND*DCOSSE(ISE,1,2)+YND*DCOSSE(ISE,2,2)+ZND*DCOSSE(ISE,3,2)
        GZ=XND*DCOSSE(ISE,1,3)+YND*DCOSSE(ISE,2,3)+ZND*DCOSSE(ISE,3,3)
        IEG=IBDYSE(ISE,2)
        IP1=IVXEDG(IEG,1)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-6)THEN
        IEE=NSP
        GX=XND*DCOSEE(IEE,1,1)+YND*DCOSEE(IEE,2,1)+ZND*DCOSEE(IEE,3,1)
        GY=XND*DCOSEE(IEE,1,2)+YND*DCOSEE(IEE,2,2)+ZND*DCOSEE(IEE,3,2)
        GZ=XND*DCOSEE(IEE,1,3)+YND*DCOSEE(IEE,2,3)+ZND*DCOSEE(IEE,3,3)
        IP1=IVRTEE(IEE)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ELSEIF(ISP.EQ.-7)THEN
        IEC=NSP
        GX=XND*DCOSEC(IEC,1,1)+YND*DCOSEC(IEC,2,1)+ZND*DCOSEC(IEC,3,1)
        GY=XND*DCOSEC(IEC,1,2)+YND*DCOSEC(IEC,2,2)+ZND*DCOSEC(IEC,3,2)
        GZ=XND*DCOSEC(IEC,1,3)+YND*DCOSEC(IEC,2,3)+ZND*DCOSEC(IEC,3,3)
        IP1=IVRTEC(IEC)
        GX=GX+X(IP1)
        GY=GY+Y(IP1)
        GZ=GZ+Z(IP1)
      ENDIF
      RETURN
      END

C *************************    AREACN    ******************************
C AREACN determines the area and centroid location for a set of vertices.
C *********************************************************************
      SUBROUTINE AREACN(NCRD,XCRD,ZCRD,AREC,XCNT,ZCNT)
#include "building.h"

      DIMENSION XCRD(MGV),ZCRD(MGV)

      SUM=0.
      XAREA=0.
      ZAREA=0.
      DO 10 I=1,NCRD
        IF(I.EQ.NCRD)THEN
          J=1
        ELSE
          J=I+1
        ENDIF
        AREA1=0.5*(XCRD(I)*ZCRD(J)-ZCRD(I)*XCRD(J))
        IF(AREA1.GT.1.E-12.OR.AREA1.LT.-1.E-12)THEN
          SUM=SUM+AREA1

C Find the median coordinates for line I,J
          XM12=(XCRD(I)+XCRD(J))/2.0
          ZM12=(ZCRD(I)+ZCRD(J))/2.0

C Find the median coordinates for line 0,I
          XM01=XCRD(I)/2.0
          ZM01=ZCRD(I)/2.0
          AX=0.0
          AZ=0.0
          BX=XM12
          BZ=ZM12
          CX=XCRD(J)
          CZ=ZCRD(J)
          DX=XM01
          DZ=ZM01
          CALL INTRSC(AX,AZ,BX,BZ,CX,CZ,DX,DZ,IER,EX,EZ)
          IF(IER.NE.0)STOP "Error (1243):in subroutine AREACN."
          XAREA=XAREA+EX*AREA1
          ZAREA=ZAREA+EZ*AREA1
        ENDIF
   10 CONTINUE
      AREC=SUM
      XCNT=XAREA/AREC
      ZCNT=ZAREA/AREC
      RETURN
      END

C *************************    INTRSC    ******************************
C INTRSC determines the intersection of two lines defined by their 
C        coordinates.
C *********************************************************************
      SUBROUTINE INTRSC(AX,AZ,BX,BZ,CX,CZ,DX,DZ,IER,EX,EZ)

      IER=-1
      DOMAIN=CZ*AX-CZ*BX+DZ*BX-DZ*AX+AZ*DX-AZ*CX-BZ*DX+BZ*CX
      IF(ABS(DOMAIN-1E-10).LT.1.E-6)RETURN
      EX=(BX*DZ*CX-BX*CZ*DX-AX*DZ*CX+AX*CZ*DX-BZ*AX*DX+BZ*AX*CX+
     &       AZ*BX*DX-AZ*BX*CX)/DOMAIN
      EZ=(CZ*BZ*AX-CZ*AZ*BX-DZ*BZ*AX+DZ*AZ*BX-AZ*DZ*CX+AZ*CZ*DX+
     &       BZ*DZ*CX-BZ*CZ*DX)/DOMAIN

C Check if the intersection point is belongs to both lines.
      ABXMX=AMAX1(AX,BX)+1.E-3
      ABXMN=AMIN1(AX,BX)-1.E-3
      ABZMX=AMAX1(AZ,BZ)+1.E-3
      ABZMN=AMIN1(AZ,BZ)-1.E-3
      CDXMX=AMAX1(CX,DX)+1.E-3
      CDXMN=AMIN1(CX,DX)-1.E-3
      CDZMX=AMAX1(CZ,DZ)+1.E-3
      CDZMN=AMIN1(CZ,DZ)-1.E-3
      IF(EX.LT.ABXMN.OR.EX.LT.CDXMN.OR.EX.GT.ABXMX.OR.EX.GT.CDXMX.OR.
     &   EZ.LT.ABZMN.OR.EZ.LT.CDZMN.OR.EZ.GT.ABZMX.OR.EZ.GT.CDZMX)RETURN
      IER=0
      RETURN
      END
