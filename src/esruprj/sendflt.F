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

C Set default uncertainties:
C   SDDFLT001:    Thermophysical properties. 

C ***************** SDDFLT001 *********************
C SDDFLT001 set defaults based on material type.
      subroutine SDDFLT001
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFCON,IFCON,LFMUL,IFMUL (for materials and constructions database)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/exporttg/xfile,tg,delim
      COMMON/exporttgi/ixopen,ixloc,ixunit

      common/UADFLT/MATTYPE(30),DFLTMAT(5,6)

      DIMENSION ICLN(30),CLSDES(40),CLSSEL(30),ICLSSEL(30)

      character CLSSEL*36
      character CLSDES*48,IDESC*72,H*72,KEY*1,outs*124
      character xfile*72,tg*1,delim*1
      character TYPEDES(5)*20,t32*32,d32*32

C Define material types.
      TYPEDES(1)='unknown'
      TYPEDES(2)='impermeable'
      TYPEDES(3)='non-hygroscopic'
      TYPEDES(4)='inorganic, porous'
      TYPEDES(5)='organic, hygroscopic'

C Set default values of material type.
      do 10 I=1,30
        MATTYPE(I)=1
 10   continue

C Set default uncertainties.
      do 12 I=1,5
        do 13 J=1,5
          DFLTMAT(I,J)=5.0
 13     continue
 12   continue

C Fix values based on current construction database.
      MATTYPE(1)=4
      MATTYPE(2)=4
      MATTYPE(3)=2
      MATTYPE(4)=5
      MATTYPE(5)=4
      MATTYPE(6)=4
      MATTYPE(7)=4
      MATTYPE(8)=4
      MATTYPE(9)=2
      MATTYPE(10)=4
      MATTYPE(11)=3
      MATTYPE(12)=3
      MATTYPE(13)=2
      MATTYPE(14)=4
      MATTYPE(15)=3

C Present list of material categories with toggles.
C Add uncertainty percentages to foot of list.

C Check that the material db is not empty.
    4 IER=0
      IREC=601
      READ(IFCON,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,(ICLN(I),I=1,30)
      IF(NCLASS.LE.0)THEN
        CALL USRMSG(' Materials db is empty.',' ','W')
        IER=1
        RETURN
      ENDIF

C Setup for menu.
C Gather the names of the various classes and present this list.
  40  ICO=-1
      CLSDES(1)=  '  Material category         type '
      M=1
      DO 44 I=1,NCLASS
        IC=ICLN(I)
        IREC=IC*20-19
        READ(IFCON,REC=IREC,IOSTAT=ISTAT,ERR=1000)NE,IDESC
        write(CLSSEL(I),'(A)') IDESC(1:36)
        IF(NE.GT.0)THEN
          M=M+1
          CALL EMKEY(M-1,KEY,IER)
          WRITE(CLSDES(M),'(A1,1X,3A)')
     &          KEY,IDESC(1:24),'>>',TYPEDES(MATTYPE(I))
        ENDIF
   44 CONTINUE

      CLSDES(M+1)=  '  --------------------------  '
      CLSDES(M+2)= ' Uncertainty bounds (% of mean value):'
      CLSDES(M+3)= '             Con-|Den-|Spec|IR  |Solr|Diff'
      CLSDES(M+4)= '             duct|sity|heat|emis|abs |res '
      write (CLSDES(M+5),'(a,6f5.1)') '1 unknown   ',
     &                                      (DFLTMAT(1,IMT),IMT=1,6)
      write (CLSDES(M+6),'(a,6f5.1)') '2 imperm    ',
     &                                      (DFLTMAT(2,IMT),IMT=1,6)
      write (CLSDES(M+7),'(a,6f5.1)') '3 non-hygro ',
     &                                      (DFLTMAT(3,IMT),IMT=1,6)
      write (CLSDES(M+8),'(a,6f5.1)') '4 inorg,por ',
     &                                      (DFLTMAT(4,IMT),IMT=1,6)
      write (CLSDES(M+9),'(a,6f5.1)') '5 org,hygro ',
     &                                      (DFLTMAT(5,IMT),IMT=1,6)
      CLSDES(M+10)= '  --------------------------  '
      CLSDES(M+11)= '? help                        '
      CLSDES(M+12)= '- exit                        '
      NCO=M+12

C Help text for this menu.
      H(1)='The classifications shown in the display include up to'
      H(2)='18 elements. To get a listing of the items included'
      H(3)='select one of the classifications.'

      CALL EMENU('Material classes',CLSDES,NCO,ICO)

      IF(ICO.EQ.NCO)THEN
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        CALL PHELPD('materials menu',3,'-',0,0,IER)
      ELSEIF(ICO.le.(NCO-3).and.ICO.ge.(NCO-7))THEN
        ITYP=ICO-(M+4)
        write (t32,'(6f5.1)')(DFLTMAT(ITYP,IMT),IMT=1,6)
        write (outs,'(2a)') 'For material type ',TYPEDES(ITYP)
        d32=' 1.0 1.0 1.0 1.0 1.0 0.0'
        call EASKS(t32,outs,'default uncertainties:',32,d32,
     &             'deflt uncertainty',IER,0)
        K=0
        call EGETWR(t32,K,DFLTMAT(ITYP,1),0.,100.,'F','conduct',IER)
        call EGETWR(t32,K,DFLTMAT(ITYP,2),0.,100.,'F','density',IER)
        call EGETWR(t32,K,DFLTMAT(ITYP,3),0.,100.,'F','spec heat',IER)
        call EGETWR(t32,K,DFLTMAT(ITYP,4),0.,100.,'F','emiss',IER)
        call EGETWR(t32,K,DFLTMAT(ITYP,5),0.,100.,'F','absorb',IER)
        call EGETWR(t32,K,DFLTMAT(ITYP,6),0.,100.,'F','dif resist',IER)
      ELSEIF(ICO.GT.1.AND.ICO.LT.(NCO-4))THEN
        MATTYPE(ICO-1)=MATTYPE(ICO-1)+1
        if (MATTYPE(ICO-1).gt.5) MATTYPE(ICO-1)=1
      endif

      goto 40
 1000 CALL USRMSG(' Cannot read material classes.',' ','W')
      RETURN

      end
