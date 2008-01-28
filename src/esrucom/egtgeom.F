C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2004.

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

C ******************** EGTGEOM
C EGTGEOM contains the following:
C  EGRNIN - reads a GRN description file
C  EGROUT - writes a GRN description to file
C  EGTROT:  Rotate ground about a user specified point by ANG degrees.
C  GTINFO - list of surface info..

C ******************** EGRNIN
C the file type GRN will consist of:
C       the total number of vertices in the body, the number of
C       surfaces, the X, Y and Z coordinates of each vertex (in any
C       convenient order), an associated ordered list of the vertex 
C       numbers which comprise each surface.

C There are no windows or doors in a GRN description.

C Common block variables are:
C GTTYPE    - zone shape type (GRN)
C XGT,YGT & ZGT - coordinates of the vertices defining the ground 
C NGT     - Number of faces comprising ground
C JGVN      - contains an ordered description (anticlockwise) of the
C             vertices in each face
C NGTV      - total number of vertices in ground
C NGVER     - vertices per surface (max 6)
C GTGEOM    - the ground topology file name
C GTNAME    - description name of the ground topology.

      SUBROUTINE EGRNIN(IUNIT,GTGEOM,ITRC,ITRU,IER)
#include "building.h"
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

C GTNAME (15 char) - description of the ground topology.
      COMMON/GT/GTNAME

C GSNAME (6 char) - ground topology surface names
C GMLCN (12 char) - ground topology surface construction attribute.
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER OUTSTR*124,GTGEOM*72,WORD*20,GTTYPE*3,GTNAME*15
      CHARACTER GMLCN*12,GSNAME*6,outs*124
      character ZN*15

      IER=0
      ZN=' '

C Initialise groung description data file. The file will be similar 
C to the current .geo file structure 
      CALL EFOPSEQ(IUNIT,GTGEOM,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Read lines from file, if one item then assumed to be GTTYPE, 
C if 2 then the second is the ground topology name which
C is tested for illegal characters.
C We will add a new GTTYPE called GRN for the ground description file
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'line 1',IER)
      IF(IER.NE.0)RETURN
      K=0
      IF(ND.EQ.1)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','GTTYPE',IFLAG)
        GTTYPE=WORD(1:3)
        GTNAME=' '
      ELSEIF(ND.EQ.2)THEN
        CALL EGETW(OUTSTR,K,WORD,'W','GTTYPE',IFLAG)
        GTTYPE=WORD(1:3)
        CALL EGETW(OUTSTR,K,WORD,'W','Z name',IFLAG)
        ZN=WORD(1:15)
        call st2name(ZN,GTNAME)
      ENDIF
      IF(IFLAG.NE.0)GOTO 1001

C Fill in a default ground name if blank.
      IF(GTNAME(1:2).EQ.'  ')WRITE(GTNAME,'(A)')'grnd_topo'

C Zone is of type GRN, look for number of vertices and surfaces
      IF(GTTYPE.EQ.'GRN')THEN
        CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'NGTV NGT',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NGTV,3,MGTV,'F','no of vertices',IER)
        CALL EGETWI(OUTSTR,K,NGT,0,MGRT,'W','no of surfaces',IER)

C Descriptive output.
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,8996)GTNAME(1:LNBLNK(GTNAME))
 8996   FORMAT(' Ground ',a,' is a set of general polygons')
        CALL EDISP(ITRU,OUTS)
        WRITE(OUTS,'(a,i3,a,i3,a)')' composed of ',NGT,
     &    ' surfaces and ',NGTV,' vertices.'
        CALL EDISP(ITRU,OUTS)
      endif

C Read each vertex data line, strip any comments, see if 3 items and
C place in XGT(),YGT(),ZGT().
        DO 62 I=1,NGTV
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'vertex data',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWR(OUTSTR,K,XGT(I),0.,0.,'-','X coord',IER)
          CALL EGETWR(OUTSTR,K,YGT(I),0.,0.,'-','Y coord',IER)
          CALL EGETWR(OUTSTR,K,ZGT(I),0.,0.,'-','Z coord',IER)
          ivedge(i)=0
          if(ND.eq.4)then
            CALL EGETW(OUTSTR,K,WORD,'W','edge',IFLAG)
            if(WORD(1:1).eq.'e'.or.WORD(1:1).eq.'E')then
              ivedge(i)=1
            endif
          endif
   62   CONTINUE

C Read vertex list for each surface, strip comments, begin by finding
C the number of expected vertices (first item on list).
        if(NGT.eq.0)goto 1002
        DO 10 I=1,NGT
          if(I.ge.MGRT)goto 444
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'vertex list',IER)
          IF(IER.NE.0) RETURN
          IF(ND.GE.4) THEN
            K=0
            CALL EGETWI(OUTSTR,K,J,3,8,'F',
     &                  'number of associated vertices',IERV)
            NGVER(I)=J

C Now proceed to read vertices on one or more lines.
            DO 12 KV=1,NGVER(I)
              CALL EGETWI(OUTSTR,K,IVAL,0,MGRT,'F','vertex',IERV)
              IF(IERV.NE.0) THEN
                CALL EDISP(ITRU,' reading continuation line...')
                CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'vertex XYZ',IER)
                IF(IER.NE.0)RETURN
                K=0
                CALL EGETWI(OUTSTR,K,IVAL,0,MGRT,'F','vertex',IERV)
              ENDIF
              IF(IERV.NE.0) GOTO 1001
              JGVN(I,KV)=IVAL
   12       CONTINUE
          ENDIF
   10   CONTINUE
      ELSE
        CALL USRMSG(' Shape type illegal',' ','F')
      ENDIF

C Surface attributes:
C 1st item - surface number ISN, 
C 2nd item - 6 char name GSNAME()
C 3th item is the name of the multilayer construction ( 12 char).
C If end of file then assume no names, do not error.

C Begin with default assumptions for each surface then overwrite
C this if user supplied information exists.
 444  DO 411 I=1,NGT
        IF(I.LE.9)WRITE(GSNAME(I),'(a,i1)')'gt-',I
        IF(I.GT.9.and.I.LE.99)WRITE(GSNAME(I),'(a,i2)')'gt-',I
        IF(I.GT.99)WRITE(GSNAME(I),'(a,i3)')'gt-',I
        GMLCN(I)='UNKNOWN'
 411  continue

      DO 42 I=1,NGT
        CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'surface attributes',IER)
        IF(IER.EQ.2)THEN

C End of file sensed, however this is not an error at this point.
          IER=0
          GOTO 44
        ENDIF
        IF(IER.EQ.1)RETURN

C Read in the various attributes as in the following line. 
C        1         2
C2345678901234567890
C 1, Surf-1 UNKN    
        K=0
        CALL EGETWI(OUTSTR,K,ISN,1,MGRT,'W','sur indx UNUSED',IER)
        IF(IER.NE.0)GOTO 44
        CALL EGETW(OUTSTR,K,WORD,'W','G name',IFLAG)
        if(iflag.eq.0)GSNAME(I)=WORD(1:6)
        CALL EGETW(OUTSTR,K,WORD,'W','GMLC name',IFLAG)
        if(iflag.eq.0)GMLCN(I)=WORD(1:12)
   42 CONTINUE

C Now close geometry data file.
   44 CALL ERPFREE(IUNIT,ISTAT)

C Descriptive output.
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')

C Print summary of vertices/surface lists.
        IF(ITRC.GT.1)CALL GTINFO(ITRU)
      ENDIF

C Close geometry data file before exiting.
    6 CALL ERPFREE(IUNIT,ISTAT)
      RETURN

 1001 CALL USRMSG(' Conversion error in... ',OUTSTR,'W')
      IER=1
      goto 6

 1002 GOTO 44
      END


C ************* EGROUT 
C Generic routine to write a ground description file (GRN type) based 
C on information currently held in common blocks grndpln, GT, GT5.
C It is assumed that this information has been checked.
C GENFIL is the name of the file to be written to (any existing file
C by this name is overwritten). ITRU unit number for
C user output. IER=0 OK, IER=1 problem.

      SUBROUTINE  EGROUT(IFILG,GENFIL,ITRU,IER)
#include "building.h"

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT/GTNAME
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER GENFIL*72,GTNAME*15,GSNAME*6,GMLCN*12
      character outstr*124,outl*124,outld*124

      IER=0

C Open any existing file by this name (ask user for confirmation to
C over-write) or create a new file.
      CALL EFOPSEQ(IFILG,GENFIL,4,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

      WRITE(IFILG,30,IOSTAT=IOS,ERR=1003)
     &  GTNAME(1:lnblnk(GTNAME)),GENFIL(1:lnblnk(GENFIL))
  30  FORMAT('# ground topology of ',a,' defined in: ',a)

C File is always a GRN type as this is representation in common.
      WRITE(IFILG,'(4a)',IOSTAT=IOS,ERR=1003)
     &  'GRN',',',GTNAME,'   # type   topology name'

C Vertex coordinates.
        WRITE(IFILG,'(I4,I4,A)',IOSTAT=IOS,ERR=1003)
     &       NGTV,NGT,'  # vertices & surfaces '
        WRITE(IFILG,32,IOSTAT=IOS,ERR=1003)
  32    FORMAT('#  X co-ord, Y co-ord, Z co-ord ')
        DO 960 I = 1,NGTV
          if(ivedge(I).eq.1)then
            WRITE(outl,'(3f10.4)',IOSTAT=IOS,ERR=1003)XGT(I),
     &        YGT(I),ZGT(I)
            call SDELIM(outl,outld,'C',IW)
            WRITE(IFILG,'(2a,i3)',IOSTAT=IOS,ERR=1003)
     &        outld(1:lnblnk(outld)),'  e  # vert ',I
          else
            WRITE(outl,'(3f10.4)',IOSTAT=IOS,ERR=1003)XGT(I),
     &        YGT(I),ZGT(I)
            call SDELIM(outl,outld,'C',IW)
            WRITE(IFILG,'(2a,i3)',IOSTAT=IOS,ERR=1003)
     &        outld(1:lnblnk(outld)),'  -  # vert ',I
          endif
960     CONTINUE

        WRITE(IFILG,34,IOSTAT=IOS,ERR=1003)
  34    FORMAT('# no of vertices followed by list of associated vert')
        DO 970 I = 1,NGT
          WRITE(outstr,5650)NGVER(I),(JGVN(I,J),J=1,NGVER(I))
5650      FORMAT(1X,9(I3,','))
          WRITE(IFILG,'(2a,i3)')outstr(1:lnblnk(outstr)),' # surf ',I
970     CONTINUE

C Optional surface information.  Begin by inserting default information
C if for some reason there are blanks remaining.
        WRITE(IFILG,'(a)',IOSTAT=IOS,ERR=1003)
     &    '# surface attributes follow: '
        WRITE(IFILG,'(a)',IOSTAT=IOS,ERR=1003)
     &    '# id  surface   construction '
        DO 102 I=1,NGT
          IF(GSNAME(I)(1:2).EQ.'  ')THEN
            IF(I.LE.9)WRITE(GSNAME(I),'(a,i1)')'gt-',I
            IF(I.GT.9.and.I.LE.99)WRITE(GSNAME(I),'(a,i2)')'gt-',I
            IF(I.GT.99)WRITE(GSNAME(I),'(a,i3)')'gt-',I
          ENDIF
          IF(GMLCN(I)(1:2).EQ.'  ')GMLCN(I)='UNKNOWN'
          WRITE(outl,'(I3,1x,A6,1x,A15)',IOSTAT=IOS,ERR=1003) I,
     &      GSNAME(I),GMLCN(I)
          call SDELIM(outl,outld,'C',IW)
          WRITE(IFILG,'(a)',IOSTAT=IOS,ERR=1003) outld(1:lnblnk(outld))
  102   CONTINUE

   99 CALL ERPFREE(IFILG,IOS)
      RETURN

C Error messages.
 1003 if(IOS.eq.2)then
        CALL USRMSG('No permission to write in ',GENFIL,'W')
      else
        CALL USRMSG('File write error in ',GENFIL,'W')
      endif
      IER=1
      GOTO 99

      END


C ******************** EGTROT 
C Rotate ground by ANG degrees and return information via common grndpln.
C X1,Y1 is the point to rotate about.
      SUBROUTINE EGTROT(ANG,X1,Y1)
#include "building.h"
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)

      PI = 4.0 * ATAN(1.0)
      A=-ANG*PI/180.
      CA=COS(A)
      SA=SIN(A)
      DO 10 I=1,NGTV
        XXX=XGT(I)-X1
        YYY=YGT(I)-Y1
        XR=XXX*CA+YYY*SA
        YR=YYY*CA-XXX*SA
        XGT(I)=XR+X1
        YGT(I)=YR+Y1
   10 CONTINUE
      RETURN
      END


C ********************** GTINFO 
C GTINFO prints two summaries, the first includes vertex coordinates
C and a list of surfaces associated with each vertex.  The second
C prints a list of surface numbers and the list of vertices which 
C define them.  Information is gathered from common block G1.
      SUBROUTINE GTINFO(ITRU)
#include "building.h"

      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      DIMENSION ISASSO(50)
      CHARACTER OUTSTR*124,GSNAME*6,GMLCN*12

      CALL EDISP(ITRU,' ')
      CALL EDISP(ITRU,
     & ' The ground is defined by the following vertices:')
      CALL EDISP(ITRU,
     &' Vertex|X-coord.| Y-coord.| Z-coord.|Associated surfaces     |')
      DO 8790 I=1,NGTV
        IHIT=0
        DO 8792 IS=1,NGT
          DO 8794 IV=1,3
            IF(I.EQ.JGVN(IS,IV))THEN
              if(ihit+1.le.50)then
                IHIT=IHIT+1
                ISASSO(IHIT)=IS
              endif
            ENDIF
 8794     CONTINUE
 8792   CONTINUE
        if(ihit.le.10)then
         WRITE(OUTSTR,9993)I,XGT(I),YGT(I),ZGT(I),(ISASSO(IH),IH=1,IHIT)
 9993    FORMAT(I5,3F10.3,'  ',10(I3,','))
        else
         WRITE(OUTSTR,9994)I,XGT(I),YGT(I),ZGT(I),(ISASSO(IH),IH=1,10)
 9994    FORMAT(I5,3F10.3,'  ',10(I3,','),'...')
        endif
        CALL EDISP(ITRU,OUTSTR)
 8790 CONTINUE

C Surfaces with associated vertices.
      CALL EDISP(ITRU,' ')
      CALL EDISP(ITRU,
     &' Each surface (polygon) is composed of vertices as follows:')
      CALL EDISP(ITRU,
     &' Surface|const- |No. | vertex list (anticlockwise from top)')
      CALL EDISP(ITRU,
     &'        |ruction|Vert|')
      DO 9991 I=1,NGT
        NV=NGVER(I)
        WRITE(OUTSTR,9990) GSNAME(I),GMLCN(i)(1:8),NV,(JGVN(I,J),J=1,NV)
 9990   FORMAT(1x,a,1x,a,I5,8(',',I3))
        CALL EDISP(ITRU,OUTSTR)
 9991 CONTINUE
      RETURN
      END
