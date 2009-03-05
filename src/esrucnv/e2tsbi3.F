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

C ESRU tsbi3 translator - .
C Current version February 2005.

C E2tsbi3: Inside and outside materials and composition generation.
C e2vrml:  Generate VRLM 2.0 world of esp-r model.

C ******* Translate esp data to tsbi3 file geom and materials.
      SUBROUTINE e2tsbi3(itrc,incobs,IER)
#include "building.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/io/ioin,ioout,ioblk,iosblk,iotobs

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat

      character GDESCR*36,DESCRC*25
      CHARACTER NAM*72,TITL*72,outs*124
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      dimension iprndx(50),optndx(50)
      character optndx*12,OPT*12,ZSN*28,q*1,zname*12,zdesc*64,T12*12
      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      DIMENSION ZSN(MS)
      character CONTYPE*12
      logical found
      logical newgeo  ! to use for testing if new/old geometry file.

C Setup standard assumptions.
      IFIL=11
      newgeo=.false.  ! assume older format geometry.

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C Set character variable q to be a single quote.
      q=CHAR(39)

C Open the material and construction databases.
      call opendb(ier)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat)

C First task is to loop through each of the mlc and get a
C list of constructions used in this problem. Clear the 
      do 18 ii=1,50
        iprndx(ii)=0
        optndx(ii)=' '
  18  continue
      iij=0
      DO 20 INO=1,NMLC
        DO 23 IL=1,LAYERS(INO)

C For each layer get primitive db ref and then if first reference
C add it to the iprndx array.
          if(iij.gt.0)then
            found = .false.
            do 24 ij = 1,iij
              if(IPR(INO,IL).eq.iprndx(ij))found = .true.
  24        continue
            if(.NOT.found)then
              iij=iij+1
              iprndx(iij)=IPR(INO,IL)
            endif
          else
            iij=iij+1
            iprndx(iij)=IPR(INO,IL)
          endif
  23    continue
  20  continue

C Now, for each unique primitive write its index and a bit of
C text to form its name.
      WRITE(ioout,'(a)') '#wall_lib'
      WRITE(ioout,'(a)') ' '
      do 25 iy = 1,iij
        if(closemat)then
          legmat=iprndx(iy)   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            DBCON=matdbcon(matarrayindex)
            DBDEN=matdbden(matarrayindex)
            DBSHT=matdbsht(matarrayindex)
            E=matdbine(matarrayindex)
            A=matdbina(matarrayindex)
            DRV=matdbdrv(matarrayindex)
            write(TITL,'(a)') 'placeholder'
            write(NAM,'(a)') matname(matarrayindex)(1:32)
          endif
        else
          CALL ERPCDB(IFMAT,iprndx(iy),ITRU,IER,DBCON,DBDEN,
     &      DBSHT,E,A,DRV,TITL,NAM)
        endif
        ln = MIN0(lnblnk(NAM),10)
        if(iprndx(iy).gt.0.and.iprndx(iy).lt.10)then

C Don't write materials made of air, ie iprndx(iy)=0
          WRITE(ioout,'(a,i1,3a,3f11.4)') q,iprndx(iy),':',
     &      NAM(1:ln),q,DBDEN,DBSHT,DBCON
        elseif(iprndx(iy).ge.10.and.iprndx(iy).lt.100)then
          WRITE(ioout,'(a,i2,3a,3f11.4)') q,iprndx(iy),':',
     &      NAM(1:ln),q,DBDEN,DBSHT,DBCON
        else
          WRITE(ioout,'(a,i3,3a,3f11.4)') q,iprndx(iy),':',
     &      NAM(1:ln),q,DBDEN,DBSHT,DBCON
        endif
  25  continue

C Now write out each of the (unique) optical properties used in the problem.
      iij=0
      DO 30 INO=1,NMLC

C For each mlc get optics and then if first reference
C add it to the optndx array.
        WRITE(OPT,'(A)') DESC(INO)(21:32)
        if(iij.gt.0)then
          found = .false.
          do 34 ij = 1,iij
            if(OPT.eq.optndx(ij))found = .true.
  34      continue
          if(.NOT.found)then
            iij=iij+1
            optndx(iij)=OPT
          endif
        else
          iij=iij+1
          optndx(iij)=OPT
        endif
  30  continue

C Write out glazing library. Remember how may items
C in this library.
      nglib=iij
      WRITE(ioout,'(a)') ' '
      WRITE(ioout,'(a)') '#glass_lib'
      WRITE(ioout,'(a)') ' '
      do 35 iy = 1,nglib
        ln = lnblnk(optndx(iy))
        if(optndx(iy)(1:6).eq.'OPAQUE')then
          WRITE(ioout,'(4a)') q,optndx(iy)(1:ln),q,
     &      '  2.400  0.00  0.00'
        else
          CALL EROPTDB(ITRC,ITRU,optndx(iy),GDESCR,IER)
          WRITE(ioout,'(3a,3f8.3)') q,optndx(iy)(1:ln),q,
     &       UVAL,DG(1),VTRN
        endif
  35  continue

C Now for each mlc, write out its fields.
      WRITE(ioout,'(a)') ' '
      WRITE(ioout,'(a)') '#walltype'
      WRITE(ioout,'(a)') ' '

      do 36 ino = 1,NMLC
        WRITE(CONTYPE,'(A)') DESC(ino)(15:18)
        IF(CONTYPE.EQ.' ')CONTYPE='OPAQ'
        IF(CONTYPE(1:4).NE.'OPAQ')THEN
           WRITE(CONTYPE,'(A)') DESC(ino)(21:32)
        ELSE
           CONTYPE='OPAQ'
        ENDIF

        IF(CONTYPE.eq.'OPAQ')THEN
          WRITE(T12,'(A)')DESC(ino)(1:12)
          WRITE(ioout,'(3a)') q,T12(1:lnblnk(T12)),q

C Write layers in reverse order.
          IL=LAYERS(ino)+1
  37      continue
          IL=IL-1
          if(closemat)then
            legmat=IPR(ino,IL)   ! which legacy index
            if(legmat.ge.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          else
            CALL ERPCDB(IFMAT,IPR(ino,IL),ITRU,IER,DBCON,DBDEN,
     &        DBSHT,E,A,DRV,TITL,NAM)
          endif

C Not dealing with air gap resistance directly...
          ln = MIN0(lnblnk(NAM),10)
          rair = 0.0

C Test if next layer is air, if so add resistance to this layer
          if(IL-1.gt.0)then
            if(IPR(ino,IL-1).eq.0.and.IL.ge.2) rair = 0.17
          endif

          if(IPR(ino,IL).gt.0.and.IPR(ino,IL).lt.10)then
            WRITE(ioout,'(F6.3,a,a,i1,3a,a,F6.3)') DTHK(ino,IL),'  ',
     &            q,IPR(ino,IL),':',NAM(1:ln),q,'  0.0 ',rair
          elseif(IPR(ino,IL).ge.10.and.IPR(ino,IL).lt.100)then
            WRITE(ioout,'(F6.3,a,a,i2,4a,F6.3)') DTHK(ino,IL),'  ',
     &            q,IPR(ino,IL),':',NAM(1:ln),q,'  0.0 ',rair
          elseif(IPR(ino,IL).ge.100)then
            WRITE(ioout,'(F6.3,a,a,i3,4a,F6.3)') DTHK(ino,IL),'  ',
     &            q,IPR(ino,IL),':',NAM(1:ln),q,'  0.0 ',rair
          endif
          IF(IL.GT.1)GOTO 37
          WRITE(ioout,'(a)') '-1 '
          WRITE(ioout,'(a)') ' '
        ENDIF

  36  continue

C Now for each zone, write out the surfaces.
C Setup standard assumptions.
      IFIL=11

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8
      INPIC=NCOMP
      WRITE(ioout,'(a)') ' '
      WRITE(ioout,'(a)') '#surface'
      WRITE(ioout,'(a)') ' '
      do 4 mz=1,inpic

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call eclose(gversion(mz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        endif

C Find current zone information, check location if user might
C have edited vertices or added a surface.
        CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)

C Loop through all vertices and find the top and bottom of the zone.
        ZMX=-1.E+7
        ZMN=1.E+7
        do 40 i = 1,NTV
          ZMN=AMIN1(ZMN,Z(I))
          ZMX=AMAX1(ZMX,Z(I))
  40    continue

c Generate combined zone & generate surface names as identifiers. Derive
C surfa width sw from area/height.
        DO 60 IS=1,NSUR

C Find adjacency information, ie is connection type, izc is index of
C the other zone, isc is the other surface, ico is the connection index.
C ie=0=external, ie=1=similar, ie=2=fixed temp, ie=3=another zone,
C ie=4=ground, ie=5=adabetic.
          CALL SURADJ(mz,IS,IE,TMP,IZC,ISC,ICO,DESCRC)

C Zone connections field - add 9 to esp zone index.
          if(ie.eq.0)then
             lmz = mz+9
             lizc = 0
          elseif(ie.eq.1)then
             lmz = mz+9
             lizc = 2
          elseif(ie.eq.2)then
             lmz = mz+9
             lizc = 3
          elseif(ie.eq.3)then
             lmz = mz+9
             lizc = IZC+9
          elseif(ie.eq.4)then
             lmz = mz+9
             lizc = 1
          elseif(ie.eq.5)then
             lmz = mz+9
             lizc = 4
          endif

C         write(zsn(is),'(3a)') zname(mz)(1:lnblnk(zname(mz))),':',
C    &       SNAME(mz,is)

C Eliminate double definition of surfaces
          if(lmz.ge.10.and.lmz.lt.lizc)goto 60

C Variations required to keep spaces out of names
          if(lmz.lt.10.and.lizc.lt.10)then
            write(zsn(is),'(a,i1,a,a9,a,i1)') 'Z',lmz,':',
     &            SNAME(mz,is)(1:lnblnk(SNAME(mz,is))),':Z',lizc
          elseif(lmz.ge.10.and.lizc.lt.10)then
            write(zsn(is),'(a,i2,a,a8,a,i1)') 'Z',lmz,':',
     &            SNAME(mz,is)(1:lnblnk(SNAME(mz,is))),':Z',lizc
          elseif(lmz.lt.10.and.lizc.ge.10)then
            write(zsn(is),'(a,i1,a,a8,a,i2)') 'Z',lmz,':',
     &            SNAME(mz,is)(1:lnblnk(SNAME(mz,is))),':Z',lizc
          elseif(lmz.ge.10.and.lizc.ge.10)then
            write(zsn(is),'(a,i2,a,a7,a,i2)') 'Z',lmz,':',
     &            SNAME(mz,is)(1:lnblnk(SNAME(mz,is))),':Z',lizc
          endif

          if(PELV(I).GE.-1..AND.PELV(I).LE.1.)then

C If surface ~vertical.
            surh = zmx-zmn
            swid = sna(is)/surh
          elseif(PELV(I).GE.89..AND.PELV(I).LE.91.)then

C If surface ~ceiling, make it square.
            swid = SQRT(sna(is))
            surh = SQRT(sna(is))
          elseif(PELV(I).GE.-91..AND.PELV(I).LE.-89.)then

C If surface ~floor, make it square.
            swid = SQRT(sna(is))
            surh = SQRT(sna(is))
          else

C Surface sloped, assume square.
            swid = SQRT(sna(is))
            surh = SQRT(sna(is))
          endif

          ICF=-1
          lstr=lnblnk(SMLCN(IS))
          DO 200 JIC=1,NMLC
            if(lstr.lt.12)then
              IF(DESC(JIC)(1:lstr).EQ.SMLCN(IS)(1:lstr))ICF=JIC
            else
              IF(DESC(JIC)(1:12).EQ.SMLCN(IS)(1:12))ICF=JIC
            endif
  200     CONTINUE

C Conversion to tsbi3 elevation of surfaces
          felv=ABS(PELV(is)-90.)

          WRITE(ioout,'(3a,4F8.3)') q,zsn(is)(1:lnblnk(zsn(is))),q,
     &      PAZI(is),felv,swid,surh
          WRITE(ioout,'(a)') '-9999.00 -9999.00 -9999.00 '

C Check what is on the other side of a surface
          if(ie.eq.0)then
            WRITE(ioout,'(a)') '0.130 0.040'
          elseif(ie.eq.1.or.ie.eq.2)then
            WRITE(ioout,'(a)') '0.130 0.130'
          elseif(ie.eq.3)then
            WRITE(ioout,'(a)') '0.130 0.130'
          elseif(ie.eq.4)then
            WRITE(ioout,'(a)') '0.130 1.000'
          elseif(ie.eq.5)then
            WRITE(ioout,'(a)') '0.130 0.130'
          endif

C Air movement field.
          WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
          WRITE(ioout,'(4a)') q,'unknown',q,'  -1'

C Zones on both faces of surface
          WRITE(ioout,'(2I4)') lmz,lizc


C 4 parameter field.

C Find composite which matches. 

          IF(SOTF(IS)(1:4).ne.'OPAQ')THEN

C Opaque constructions in this surface - NONE
            WRITE(ioout,'(a)') '  -1' 

C Loop through each of the #glass_lib entries to the
C one matching the current MLC optics.
            WRITE(OPT,'(A)') DESC(ICF)(21:32)
            found = .false.
            do 234 iii = 1,nglib
              if(OPT.eq.optndx(iii))then
                found = .true.
                iiii=iii
              endif
 234        continue

C << the #glass_lib match is optndx(iiii) which should be >>
C << substituted in the write statement below >>
C Window NoOf, name and type
C           WRITE(ioout,'(12a)')'1 ',q,zsn(is)(1:lnblnk(zsn(is))),q,
C    &            ' ',q,SMLCN(is)(1:lnblnk(SMLCN(is))),q,
C    &            ' ',q,'unknown',q
            WRITE(ioout,'(12a)')'1 ',q,zsn(is)(1:lnblnk(zsn(is))),q,
     &            ' ',q,optndx(iiii),q,' ',q,'unknown',q

C Calculate the window U-value
            RT = 0
            DO 201, IL=1,LAYERS(ino)
              if(closemat)then
                legmat=IPR(ino,IL)   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
                  DBCON=matdbcon(matarrayindex)
                  DBDEN=matdbden(matarrayindex)
                  DBSHT=matdbsht(matarrayindex)
                  E=matdbine(matarrayindex)
                  A=matdbina(matarrayindex)
                  DRV=matdbdrv(matarrayindex)
                  write(TITL,'(a)') 'placeholder'
                  write(NAM,'(a)') matname(matarrayindex)(1:32)
                endif
              else
                CALL ERPCDB(IFMAT,IPR(INO,IL),ITRU,IER,DBCON,
     &            DBDEN,DBSHT,E,A,DRV,TITL,NAM)
              endif

C Calculate std U-value and display as well
              if(IPR(INO,IL).eq.0)then
                RT=RT+DRAIR(INO,IL,1)
              else
                RT=RT+DTHK(INO,IL)/DBCON
              endif
  201       CONTINUE
            RT=RT+0.055+0.123

C No attention given to the fact that window may not be vertical
            UVALUE=1.0/RT

C Global window geometry & U-value
C Increasing window gross-area with 5% to eliminiate
C tsbi3 user-interface errors
            fgarea=swid*surh*1.05
            WRITE(ioout,'(3F7.3,a,F6.3)')
     &            fgarea,swid,surh,' 0.0 0.0 ',UVALUE

C Local window geometry, ie overhangs & sidefinns - NONE
C ToDo: Add test for internal windows and supply standard 
C       overhangs and sidefinns in this cases !!!!!!
            flgeo=0.0
            if(lizc.gt.0)flgeo=1.0
            WRITE(ioout,'(6F7.3)')
     &            flgeo,flgeo,flgeo,flgeo,flgeo,flgeo

C SF-1, SF-2 & SF-3 - NO daylight calculations
            flgeo=0.0
            WRITE(ioout,'(3F7.3)')
     &            flgeo,flgeo,flgeo

C Shutters - NONE
            WRITE(ioout,'(4a)') q,'unknown',q,' -1'

C Curtains/blinds - NONE
            WRITE(ioout,'(4a)') q,'unknown',q,' -1'

C External obstracles - NONE
            WRITE(ioout,'(4a)') q,'unknown',q,' -1'

          ELSE
            if(closemat)then
              legmat=IPR(ICF,1)   ! which legacy index
              if(legmat.ge.0)then
                matarrayindex=mathash(legmat)  ! find matching array index
                DBCON=matdbcon(matarrayindex)
                DBDEN=matdbden(matarrayindex)
                DBSHT=matdbsht(matarrayindex)
                E=matdbine(matarrayindex)
                AE=matdbina(matarrayindex)
                DRV=matdbdrv(matarrayindex)
                write(TITL,'(a)') 'placeholder'
                write(NAM,'(a)') matname(matarrayindex)(1:32)
              endif
            else
              CALL ERPCDB(IFMAT,IPR(ICF,1),iuout,IER,DBCON,DBDEN,
     &           DBSHT,E,AE,DRV,TITL,NAM)
            endif
            WRITE(ioout,'(f8.3,8a,F8.2)') sna(is),'  ',
     &        q,zsn(is)(1:lnblnk(zsn(is))),q,'   ',
     &        q,SMLCN(is)(1:lnblnk(SMLCN(is))),q,AE 
            WRITE(ioout,'(a)') '-1' 
          ENDIF

C For this pass assume there are no windows or doors in the surface.
          WRITE(ioout,'(a)') '-1' 
          WRITE(ioout,'(a)') '-1' 
          WRITE(ioout,'(a)') ' ' 
  60    continue
   4  continue

C Now look at the zone section.
      WRITE(ioout,'(a)') ' ' 
      WRITE(ioout,'(a)') '#zone ' 
      WRITE(ioout,'(a)') ' ' 
      do 42 mz = 1,ncomp

C RE-Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(mz)
        CALL edisp(iuout,outs)
        call eclose(gversion(mz),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(mz),mz,1,ITRC,IUOUT,IER)
        endif

C Find current zone information, check location if user might
C have edited vertices or added a surface.
        CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)
        FLA=0.

C Loop through all vertices and find the top and bottom of the zone.
        ZMX=-1.E+7
        ZMN=1.E+7
        do 43 i = 1,NTV
          ZMN=AMIN1(ZMN,Z(I))
          ZMX=AMAX1(ZMX,Z(I))
  43    continue
        do 44 i = 1,nsur
          if(SVFC(I).eq.'FLOR')FLA=FLA+SNA(I)
  44    continue

C Write zone name.
        WRITE(ioout,'(3a)')q,zname(mz)(1:lnblnk(zname(mz))),q
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'

C Write floor areas, zone height.
        WRITE(ioout,'(3f12.4)') FLA,FLA,ZMX-ZMN
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'

C 4 zone parameters - dummy values.
        WRITE(ioout,'(a)') ' 1 1.000 0 0'
        WRITE(ioout,'(4a)') q,'unknown',q,'  -1'

C Zone solar information - dummy values.
        WRITE(ioout,'(a)') ' 0.100 0.100 4 2 1 0.500'
        WRITE(ioout,'(a)') ' '
  42  continue

C Nothing on the #fictemp part of tsbi3.
C Nothing on the #fictzone part of tsbi3.

C Parameters - dummy values.
        WRITE(ioout,'(a)') ' '
        WRITE(ioout,'(a)') '#parms '
        WRITE(ioout,'(a)') ' '
        WRITE(ioout,'(a)') '1995 -9999 -9999 0.00 0.500 '
        WRITE(ioout,'(3a)') q,'unknown',q
        WRITE(ioout,'(3a)') q,'CPH.try',q

      return
      end

C Extracted from opendb code in eddb.F
C Open the common databases - materials, multilayer constructions,
C and optical db.
      subroutine opendb(ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      LOGICAL    XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,close
      DIMENSION ICLN(30)
      character SOPT*12,outs*124,GDESCR*36
      IER=0

C Open currently named primitive construction db.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(3a)') ' No primitive constructions db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found.'
        call edisp(iuout,outs)
        MATDBOK=.FALSE.
      else

C Also check to see if the file has correct info in 
        IREC=601
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &    (ICLN(I),I=1,30)
        if(NCLASS.LE.0)then
          goto 1000   ! no classifications so try alternatives
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
 1000   CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
        call rascimat(iuout,IFMAT,LFMAT,IER)  ! try current ascii format
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(iuout,IFMAT,LFMAT,ier)  ! try older ascii format
          if(ier.ne.0)then
            call usrmsg('No readable materials database was not found',
     &                  'or file was corrupt','W')
            MATDBOK=.FALSE.
          else
            call eclose(matver,1.1,0.001,close)
            if(close)then
              call edisp(iuout,' Filled material data arrays...')
              MATDBOK=.TRUE.
            else
              call usrmsg(
     &          'No readable material database was found. ',
     &          'check other warnings for advise.','W')
              MATDBOK=.FALSE.
            endif
          endif
        endif
      endif
        
C Read multilayer db information into common.
 139  CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,ITRU,IER)
        IF(IER.NE.0)then
          write(outs,'(3a)') ' Problem with multilayer constr db ',
     &     LFMAT(1:lnblnk(LFMAT)),'... '
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        else
          call edisp(iuout,' Opened mlc db...')
          MLDBOK=.TRUE.
        endif
      else
        write(outs,'(3a)') ' No multilayer constructions db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found.'
        call edisp(iuout,outs)
      endif

C Open optical database and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(' Optical properties db not found or there was a ',
     &  ' problem reading it. Please check database manager ASAP.','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

      return
      end

C ******* Translate esp data to vrlm world.
C e2vrml takes zones/obstructions and any ground topology and
C creates a grey-scale VRML 2.0 world.
      SUBROUTINE e2vrml(itrc,IER)
#include "building.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      character CTYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/CTYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),BANG(MB)
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/io/ioin,ioout,ioblk,iosblk,iotobs
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/GTFIL/GTGEOM
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat

      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS,GTGEOM
      character GDESCR*36,MLCN*12,OTF*4,OPT*12
      character MLCSYM*12
      CHARACTER cfgroot*24,LCFGF*72
      CHARACTER NAM*72,TITL*72,BLKNAME*8,BLKMAT*12
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER GMLCN*12,GSNAME*6
      character ZSN*28,q*1,zname*12,zdesc*64
      DIMENSION SNA(MS),PAZI(MS),PELV(MS),ZSN(MS)
      character outs*124,outsd*124,hc*1
      logical XST,numb
      logical newgeo  ! to use for testing if new/old geometry file.

C Setup standard assumptions.
      IFIL=11
      newgeo=.false.  ! assume older format geometry.

C Ftr is cut-off point for deciding visual trans is high enough
C for surface to be considered fictitious.
      ftr=0.94

C Assume configuration file is from IFIL+5, any leakage description
C is fom IFIL+6, revised config file on IFIL+3, geometry on
C IFILE+2 and ASCII viewing and geometry file reading on IFILE+1. 
      ITA1 = IFIL+8

C Set character variable q to be a double quote and hc to the # character.
      q=CHAR(34)
      hc=CHAR(35)

C Open the matrial and constructions db.
      call opendb(ier)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat)

C Define a VRML material for each construction in the database.
      WRITE(ioout,'(a)')
     &  '# VRML materials derived from ESP-r constructions...'
      do 166 im= 1,NMLC

C Pick up general description of the composite.
        ifound=im
        call parsemlcdesc(DESC(IM),MLCN,OTF,OPT,MLCSYM)

C For the inside and outside layer get surface properties (AE & AI).
        if(closemat)then
          legmat=IPR(IM,1)   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            E=matdbine(matarrayindex)
            AE=matdbina(matarrayindex)
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IM,1),iuout,IER,DBCON,DBDEN,
     &      DBSHT,E,AE,DRV,TITL,NAM)
        endif
        if(closemat)then
          legmat=IPR(IM,LAYERS(IM))   ! which legacy index
          if(legmat.ge.0)then
            matarrayindex=mathash(legmat)  ! find matching array index
            E=matdbine(matarrayindex)
            AI=matdbina(matarrayindex)
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IM,LAYERS(IM)),iuout,IER,DBCON,
     &      DBDEN,DBSHT,E,AI,DRV,TITL,NAM)
        endif

C If a tmc then find tranmission functions.  If more transparent than
C the ficticious limit then mark to be skipped. Convert visible
C transmission to transmissivity (as per Ward 1/12/94 Radiance 2.4 paper).
C If TMC has an opaque layer then set gvt to a small number.
        if(OTF(1:4).NE.'OPAQ')then
          CALL EROPTDB(ITRC,iuout,OPT,GDESCR,IER)
          if(VTRN.gt.0.02)then
            gvt1=(sqrt(0.8402528435+(0.007252224*VTRN*VTRN))
     &            -0.9166530661)
            gvt=gvt1/0.0036261119/VTRN
          else
            gvt1=0.02
            gvt=0.02
          endif
          if(VTRN.gt.ftr)then
C << surface should be skipped logic for later...>>
            continue
          else
            continue
          endif
        else
          gvt=0.00
        endif

        WRITE(ioout,'(a)')'Anchor {'
        WRITE(ioout,'(5a)')'parameter [ ',q,'target = description',q,
     &    ' ]'
        WRITE(ioout,'(a)') '  children [ Shape {'
        if(OTF(1:4).NE.'OPAQ')then
          WRITE(ioout,'(3a)')
     &      '   appearance DEF Glass_',MLCN(1:lnblnk(MLCN)),
     &      ' Appearance {'
          WRITE(ioout,'(2a)')
     &      '     material Material { diffuseColor 1 1 1 ',
     &      ' ambientIntensity 1.0 specularColor 0 0 100  '
          WRITE(ioout,'(a,F5.2,a)') 
     &      '        shininess 0.725 transparency ',gvt,' } '
          WRITE(ioout,'(a)')'      } '
        else
          WRITE(ioout,'(3a)')
     &      '    appearance DEF Solid_',MLCN(1:lnblnk(MLCN)),
     &      ' Appearance {'
          WRITE(ioout,'(a,3F5.2,a,3F5.2)')
     &      '      material Material { diffuseColor ',
     &      1.0-AE,1.0-AE,1.0-AE,' specularColor ',1.0-AE,1.0-AE,1.0-AE
          WRITE(ioout,'(a,F5.2,a)')
     &      '        shininess 0.5  transparency ',gvt,' } '
          WRITE(ioout,'(a)')'      } '
        endif
        WRITE(ioout,'(a)') '    } '
        WRITE(ioout,'(a)') '  ] '
        WRITE(ioout,'(a)') '} '
        WRITE(ioout,'(a)') '  '
 166  continue
 
C For each zone in the model create an anchor.
      INPIC=NCOMP
      do 4 mz=1,inpic
        newfoc=mz

C Read in the zone geometry.
        WRITE(outs,'(a,a)')' Scanning : ',LGEOM(newfoc)
        CALL edisp(iuout,outs)
        call eclose(gversion(newfoc),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
        else
          call egomin(ITA1,LGEOM(newfoc),newfoc,1,ITRC,IUOUT,IER)
        endif
        CALL ZINFO(ITRU,SNA,ZOA,PAZI,PELV,VOL)
        WRITE(ioout,'(a)')' '
        WRITE(ioout,'(3a)')'# zone ',zname(newfoc),' starts here....'
        do 5 j=1,nsur

C Work out the colours for each of the surfaces in this zone by looping
C through each of the MLC and finding a match.
          ifound = 0
          do 6 im= 1,NMLC
            if(SMLCN(j)(1:12).eq.DESC(IM)(1:12))then

C Pick up general description of the composite.
              ifound=im
              call parsemlcdesc(DESC(IM),MLCN,OTF,OPT,MLCSYM)

C For the inside and outside layer get surface properties.
              if(closemat)then
                legmat=IPR(IM,1)   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
                  E=matdbine(matarrayindex)
                  AE=matdbina(matarrayindex)
                endif
              else
                CALL ERPCDB(IFMAT,IPR(IM,1),iuout,IER,DBCON,DBDEN,
     &            DBSHT,E,AE,DRV,TITL,NAM)
              endif
              if(closemat)then
                legmat=IPR(IM,LAYERS(IM))   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
                  E=matdbine(matarrayindex)
                  AI=matdbina(matarrayindex)
                endif
              else
                CALL ERPCDB(IFMAT,IPR(IM,LAYERS(IM)),iuout,IER,DBCON,
     &            DBDEN,DBSHT,E,AI,DRV,TITL,NAM)
              endif
              goto 7
            endif
  6       continue
  7       continue

C Remeber characters in surface name
          lsn=lnblnk(sname(newfoc,j))
          write(zsn(j),'(3a)') zname(newfoc)(1:lnblnk(zname(newfoc))),
     &      ':',sname(newfoc,j)(1:lsn)
          lzsn=lnblnk(zsn(j))

C Write entry for this surface in the HTML file.
          write(ioblk,'(a)')  ' '
          write(ioblk,'(7a)') '<a name=',q,zsn(j)(1:lzsn),q,'>About ',
     &      zsn(j)(1:lzsn),'</a>'
          write(ioblk,'(a)')  '<PRE>'

          WRITE(ioblk,193)
  193 FORMAT(' Sur| Area  |Azim|Elev| surface    |geometry|',
     &       ' construction |environment')
           WRITE(ioblk,194)
  194 FORMAT('    | m^2   |deg |deg | name       |type|loc|',
     &       ' name         |other side ')
 894      FORMAT(I3,F9.3,F5.0,F5.0,1X,A12,1X,A4,1X,A4,1X,A12,2X,A13)
          WRITE(ioblk,894)J,SNA(j),PAZI(j),PELV(j),
     &        SNAME(newfoc,j),SOTF(j),SVFC(j),SMLCN(j),SOTHER(j,1)
          write(ioblk,'(a)')  '</PRE>'
          write(ioblk,'(a)')  ' '

C If the first surface write initial anchor for this zone
C (which holds the coordinates of the zone along with the first surface.
          WRITE(ioout,'(a)')'Anchor {'
          WRITE(ioout,'(5a)')'parameter [ ',q,'target = description',q,
     &      ' ]'
          WRITE(ioout,'(8a)')'  url [ ',q,cfgroot(1:lnblnk(cfgroot)),
     &      '.html',hc,ZSN(j)(1:lzsn),q,' ]'
          WRITE(ioout,'(a)') '  children ['
          WRITE(ioout,'(a)') '    Shape {'
          if(j.eq.1)then
            if(OTF(1:4).NE.'OPAQ')then
              WRITE(ioout,'(2a)')
     &          '      appearance USE Glass_',MLCN(1:lnblnk(MLCN))
            else
              WRITE(ioout,'(2a)')
     &          '      appearance USE Solid_',MLCN(1:lnblnk(MLCN))
            endif

C Ensure first character is not a number.
            call ifnumber(sname(newfoc,j)(1:1),numb)
            if(numb)then
              WRITE(ioout,'(3a)')'      geometry DEF a',
     &          sname(newfoc,j)(1:lsn),' IndexedFaceSet {'
            else
              WRITE(ioout,'(3a)')'      geometry DEF ',
     &          sname(newfoc,j)(1:lsn),' IndexedFaceSet {'
            endif
            WRITE(ioout,'(a)') '        ccw TRUE'
            WRITE(ioout,'(a)') '        solid FALSE'
            WRITE(ioout,'(3a)')'        coord DEF COORD_',
     &        zname(newfoc)(1:lnblnk(zname(newfoc))),
     &        ' Coordinate { point ['

C Note on coordinates:
C In esp-r the +Z is up, in VRML +Y is up.  In esp-r and VRML +X is same.
C In esp-r +Y goes into the screen, in VRML -Z goes into screen.
            DO 40 JJ=1,NTV
              if(jj.lt.NTV)then
                WRITE(ioout,'(3F10.5,a)')X(JJ),Z(JJ),Y(JJ)*(-1.0),','
              else
                WRITE(ioout,'(3F10.5,a)')X(JJ),Z(JJ),Y(JJ)*(-1.0),'  ]'
              endif
   40       continue
            WRITE(ioout,'(a)') '        } '
            WRITE(ioout,'(a)') '        coordIndex [ '

C Write a comma separated list of vertices (in both directions) with a trailing -1.
            write(outs,'(24I4)') (JVN(J,K)-1,K=1,NVER(j))
            call sdelim(outs,outsd,'C',IW)
            WRITE(ioout,'(3a)')'    ',outsd(1:lnblnk(outsd)),',-1,'
            write(outs,'(24I4)') (JVN(J,K)-1,K=NVER(j),1,-1)
            call sdelim(outs,outsd,'C',IW)
            WRITE(ioout,'(3a)')'    ',outsd(1:lnblnk(outsd)),',-1'
            WRITE(ioout,'(a)') '        ] '
            WRITE(ioout,'(a,3F5.2,a,3F5.2,a)')
     &        '        color Color {  color [ ',
     &        1.0-AE,1.0-AE,1.0-AE,',',1.0-AI,1.0-AI,1.0-AI,' ]  } '
            WRITE(ioout,'(a)') '        colorIndex [ 0, 1  ]'
            WRITE(ioout,'(a)') '        colorPerVertex FALSE '
            WRITE(ioout,'(a)') '      } '
            WRITE(ioout,'(a)') '    } '
            WRITE(ioout,'(a)') '  ] '
            WRITE(ioout,'(a)') '} '
            WRITE(ioout,'(a)') '  '
          else

C Write differently if opaque or transparent.
            if(OTF(1:4).NE.'OPAQ')then
              WRITE(ioout,'(2a)')
     &          '     appearance USE Glass_',MLCN(1:lnblnk(MLCN))
            else
              WRITE(ioout,'(2a)')
     &          '     appearance USE Solid_',MLCN(1:lnblnk(MLCN))
            endif
            call ifnumber(sname(newfoc,j)(1:1),numb)
            if(numb)then
              WRITE(ioout,'(3a)')'      geometry DEF a',
     &          sname(newfoc,j)(1:lsn),' IndexedFaceSet {'
            else
              WRITE(ioout,'(3a)')'      geometry DEF ',
     &          sname(newfoc,j)(1:lsn),' IndexedFaceSet {'
            endif
            WRITE(ioout,'(a)')  '        ccw TRUE'
            if(OTF(1:4).NE.'OPAQ')then
              WRITE(ioout,'(a)')'        solid FALSE'
            else
              WRITE(ioout,'(a)')'        solid TRUE'
            endif
            WRITE(ioout,'(2a)') '        coord USE COORD_',
     &        zname(newfoc)(1:lnblnk(zname(newfoc)))
            WRITE(ioout,'(a)')  '        coordIndex [ '
            write(outs,'(24I4)') (JVN(J,K)-1,K=1,NVER(j))
            call sdelim(outs,outsd,'C',IW)
            WRITE(ioout,'(3a)') '    ',outsd(1:lnblnk(outsd)),',-1,'
            write(outs,'(24I4)') (JVN(J,K)-1,K=NVER(j),1,-1)
            call sdelim(outs,outsd,'C',IW)
            WRITE(ioout,'(3a)') '    ',outsd(1:lnblnk(outsd)),',-1'
            WRITE(ioout,'(a)') '        ] '
            WRITE(ioout,'(a,3F5.2,a,3F5.2,a)')
     &        '        color Color {  color [ ',
     &        1.0-AE,1.0-AE,1.0-AE,',',1.0-AI,1.0-AI,1.0-AI,' ]  } '
            WRITE(ioout,'(a)') '        colorIndex [ 0, 1  ]'
            WRITE(ioout,'(a)') '        colorPerVertex FALSE '
            WRITE(ioout,'(a)') '      } '
            WRITE(ioout,'(a)') '    } '
            WRITE(ioout,'(a)') '  ] '
            WRITE(ioout,'(a)') '} '
            WRITE(ioout,'(a)') '  '
          endif
  5     continue

C If there are obstructions associated with this zone read them in
C and convert into GB1 commons and write them out using similar
C logic.
        if(IOBS(newfoc).EQ.1)then
          CALL EGOMST(ITA1,ZOBS(newfoc),0,0,iuout,IER)
          WRITE(outs,'(a,a)')' Scanning : ',ZOBS(newfoc)
          CALL edisp(iuout,outs)
          WRITE(ioout,'(a)')' '
          WRITE(ioout,'(3a)')'# zone ',zname(newfoc),
     &      'obstructions start here....'
        elseif(IOBS(newfoc).EQ.2)then
          WRITE(ioout,'(a)')' '
          WRITE(ioout,'(3a)')'# zone ',zname(newfoc),
     &      'obstructions start here....'
        endif
        if(NB.eq.0) goto 302
        DO 301 IB=1,NB
          CALL CNVBLK(XO(IB),YO(IB),ZO(IB),DX(IB),DY(IB),DZ(IB),
     &      BANG(IB))

C Work out the colour for each block by looping
C through each of the MLC and finding a match.
          ifound = 0
          do 66 im= 1,NMLC
            if(BLKMAT(IB)(1:12).eq.DESC(IM)(1:12))then

C Pick up general description of the composite.
              ifound=im
              WRITE(MLCN,'(A)')DESC(IM)(1:12)

C For the inside and outside layer get surface properties.
              if(closemat)then
                legmat=IPR(IM,1)   ! which legacy index
                if(legmat.ge.0)then
                  matarrayindex=mathash(legmat)  ! find matching array index
                  E=matdbine(matarrayindex)
                  AE=matdbina(matarrayindex)
                endif
              else
                CALL ERPCDB(IFMAT,IPR(IM,1),iuout,IER,DBCON,DBDEN,
     &            DBSHT,E,AE,DRV,TITL,NAM)
              endif
              gvt=0.00
              goto 77
            endif
 66       continue
 77       continue

C Remeber characters in block name
          lsn=lnblnk(BLKNAME(IB))

C If the first face write initial anchor for this obstruction
C (which holds its coordinates of along with the first surface.
          do 78 kbs=1,6
            WRITE(ioout,'(a)')'Anchor {'
            WRITE(ioout,'(5a)')'parameter [ ',q,
     &        'target = description',q,' ]'
            WRITE(ioout,'(8a)')'  url [ ',q,cfgroot(1:lnblnk(cfgroot)),
     &       '.html',hc,BLKNAME(IB)(1:lsn),q,' ]'
            WRITE(ioout,'(a)') '  children ['
            WRITE(ioout,'(a)') '    Shape {'
            if(kbs.eq.1)then   ! include the block material
              WRITE(ioout,'(2a)')
     &          '     appearance USE Solid_',
     &          BLKMAT(IB)(1:lnblnk(BLKMAT(IB)))
              call ifnumber(BLKNAME(IB)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF a',
     &            BLKNAME(IB)(1:lsn),kbs,' IndexedFaceSet {'
              else
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF ',
     &            BLKNAME(IB)(1:lsn),kbs,' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)') '        solid FALSE'
              WRITE(ioout,'(3a)')'        coord DEF COORD_',
     &        BLKNAME(IB)(1:lnblnk(BLKNAME(IB))),' Coordinate { point ['

C Note on coordinates:
C In esp-r the +Z is up, in VRML +Y is up.  In esp-r and VRML +X is same.
C In esp-r +Y goes into the screen, in VRML -Z goes into screen.
C Each 
              DO 41 JJ=1,8
                if(jj.lt.8)then
                  WRITE(ioout,'(3F10.5,a)')XB(JJ),ZB(JJ),
     &              YB(JJ)*(-1.0),','
                else
                  WRITE(ioout,'(3F10.5,a)')XB(JJ),ZB(JJ),
     &              YB(JJ)*(-1.0),'  ]'
                endif
   41         continue
              WRITE(ioout,'(a)') '        } '

C Write a comma separated list of vertices with a trailing -1.
              write(outs,'(4I4)') (JVNB(kbs,K)-1,K=1,4)
              call sdelim(outs,outsd,'C',IW)
              WRITE(ioout,'(3a)') '        coordIndex [ ',
     &          outsd(1:lnblnk(outsd)),',-1 ]'
              WRITE(ioout,'(a)') '      } '
              WRITE(ioout,'(a)') '    } '
              WRITE(ioout,'(a)') '  ] '
              WRITE(ioout,'(a)') '} '
              WRITE(ioout,'(a)') '  '
            else
              WRITE(ioout,'(2a)')
     &          '     appearance USE Solid_',
     &          BLKMAT(IB)(1:lnblnk(BLKMAT(IB)))
              call ifnumber(BLKNAME(IB)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF a',
     &            BLKNAME(IB)(1:lsn),kbs,' IndexedFaceSet {'
              else
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF ',
     &            BLKNAME(IB)(1:lsn),kbs,' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)')'        solid TRUE'
              WRITE(ioout,'(2a)')'        coord USE COORD_',
     &          BLKNAME(IB)(1:lnblnk(BLKNAME(IB)))
              write(outs,'(4I4)') (JVNB(kbs,K)-1,K=1,4)
              call sdelim(outs,outsd,'S',IW)
              WRITE(ioout,'(3a)') '        coordIndex [ ',
     &          outsd(1:lnblnk(outsd)),',-1 ]'
              WRITE(ioout,'(a)') '      } '
              WRITE(ioout,'(a)') '    } '
              WRITE(ioout,'(a)') '  ] '
              WRITE(ioout,'(a)') '} '
              WRITE(ioout,'(a)') '  '
            endif
  78      continue
 301    continue
 302    continue
  4   continue

C Open the matrial and constructions db (again).
C      call opendb(ier)

C If there is a ground topology create surfaces for it.
      INQUIRE (FILE=gtgeom,EXIST=XST)
      if(XST)then
        WRITE(outs,'(a,a)')' Scanning : ',GTGEOM
        CALL edisp(iuout,outs)
        WRITE(ioout,'(a)')' '
        WRITE(ioout,'(a)')'# ground surfaces start here...'
        call EGRNIN(ITA1,GTGEOM,ITRC,ITRU,IER)
        if(ngt.ge.1)then
          WRITE(ioout,'(a)')' '
          WRITE(ioout,'(a)')'# ground topology starts here....'
          do 303 igg = 1,ngt

C Find the matching mlc index.
            ifound = 0
            AE=0.5
            DO 701 imm=1,NMLC
              IF(DESC(imm)(1:12).EQ.GMLCN(igg)(1:12))then
                ifound=im
                if(closemat)then
                  legmat=IPR(IM,1)   ! which legacy index
                  if(legmat.ge.0)then
                    matarrayindex=mathash(legmat)  ! find matching array index
                    E=matdbine(matarrayindex)
                    AE=matdbina(matarrayindex)
                  endif
                else
                  CALL ERPCDB(IFMAT,IPR(IMM,1),iuout,IER,DBCON,DBDEN,
     &              DBSHT,E,AE,DRV,TITL,NAM)
                endif
                gvt=0.00
C Debug.
C                write(6,*) 'matching ',imm,' ',DESC(imm),AE,IPR(IMM,1)
                goto 702
              endif
 701        CONTINUE
 702        continue

C Remeber characters in surface name
            lsn=lnblnk(gsname(igg))

C If the first surface write initial anchor for this zone
C (which holds the coordinates of the zone along with the first surface.
            WRITE(ioout,'(a)')'Anchor {'
            WRITE(ioout,'(5a)')'parameter [ ',q,'target = description',
     &        q,' ]'
            WRITE(ioout,'(8a)')'  url [ ',q,cfgroot(1:lnblnk(cfgroot)),
     &        '.html',hc,gsname(igg)(1:lsn),q,' ]'
            WRITE(ioout,'(a)') '  children ['
            WRITE(ioout,'(a)') '    Shape {'
            if(igg.eq.1)then
              WRITE(ioout,'(a)')'     appearance Appearance {'
              WRITE(ioout,'(a)')'        material Material {'
              WRITE(ioout,'(a,3F5.2)')'          diffuseColor ',
     &          1.0-AE,1.0-AE,1.0-AE
              WRITE(ioout,'(a,3F5.2)')'          specularColor ',
     &          1.0-AE,1.0-AE,1.0-AE
              WRITE(ioout,'(a)')'          shininess 0.4 '
              WRITE(ioout,'(a,F5.2)') '          transparency ',gvt
              WRITE(ioout,'(a)')'        } '
              WRITE(ioout,'(a)')'      } '
              call ifnumber(gsname(igg)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(3a)')'      geometry DEF a',
     &            gsname(igg)(1:lsn),' IndexedFaceSet {'
              else
                WRITE(ioout,'(3a)')'      geometry DEF ',
     &            gsname(igg)(1:lsn),' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)') '        solid FALSE'
              WRITE(ioout,'(2a)')'        coord DEF ',
     &          'ground_COORD Coordinate { point ['

C Note on coordinates:
C In esp-r the +Z is up, in VRML +Y is up.  In esp-r and VRML +X is same.
C In esp-r +Y goes into the screen, in VRML -Z goes into screen.
              DO 46 JJ=1,NGTV
                if(jj.lt.NGTV)then
                  WRITE(ioout,'(3F10.5,a)')XGT(JJ),ZGT(JJ),
     &              YGT(JJ)*(-1.0),','
                else
                  WRITE(ioout,'(3F10.5,a)')XGT(JJ),ZGT(JJ),
     &              YGT(JJ)*(-1.0),'  ]'
                endif
   46         continue
              WRITE(ioout,'(a)') '        } '
              WRITE(ioout,'(a)') '        coordIndex [ '

C Write a comma separated list of vertices with a trailing -1.
              write(outs,'(8I4)') (JGVN(igg,K)-1,K=1,NGVER(igg))
              call sdelim(outs,outsd,'C',IW)
              WRITE(ioout,'(3a)')'    ',outsd(1:lnblnk(outsd)),',-1'
              WRITE(ioout,'(a)') '        ] '
              WRITE(ioout,'(a)') '      } '
              WRITE(ioout,'(a)') '    } '
              WRITE(ioout,'(a)') '  ] '
              WRITE(ioout,'(a)') '} '
              WRITE(ioout,'(a)') '  '
            else
              WRITE(ioout,'(a)')'     appearance Appearance {'
              WRITE(ioout,'(a)')'        material Material {'
              WRITE(ioout,'(a,3F5.2)')'          diffuseColor ',
     &          1.0-AE,1.0-AE,1.0-AE
              WRITE(ioout,'(a,3F5.2)')'          specularColor ',
     &          1.0-AE,1.0-AE,1.0-AE
              WRITE(ioout,'(a)')'          shininess 0.4 '
              WRITE(ioout,'(a,F5.2)') '          transparency ',gvt
              WRITE(ioout,'(a)')'        } '
              WRITE(ioout,'(a)')'      } '
              call ifnumber(gsname(igg)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(3a)')'      geometry DEF a',
     &            gsname(igg)(1:lsn),' IndexedFaceSet {'
              else
                WRITE(ioout,'(3a)')'      geometry DEF ',
     &            gsname(igg)(1:lsn),' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)') '        solid FALSE'
              WRITE(ioout,'(3a)')'        coord USE ground_COORD'
              WRITE(ioout,'(a)') '        coordIndex [ '

C Write a comma separated list of vertices with a trailing -1.
              write(outs,'(8I4)') (JGVN(igg,K)-1,K=1,NGVER(igg))
              call sdelim(outs,outsd,'C',IW)
              WRITE(ioout,'(3a)')'    ',outsd(1:lnblnk(outsd)),',-1'
              WRITE(ioout,'(a)') '        ] '
              WRITE(ioout,'(a)') '      } '
              WRITE(ioout,'(a)') '    } '
              WRITE(ioout,'(a)') '  ] '
              WRITE(ioout,'(a)') '} '
              WRITE(ioout,'(a)') '  '
            endif
  303     continue
        endif
      endif
      return
      end

C ***** ifnumber
C ifnumber checks a character to see if it is a digit. typically
C passed the first character of a name. If a digit set numb = TRUE
      subroutine ifnumber(tchar,numb)
      character tchar*1
      logical numb

      numb=.false.
      if(tchar.eq.'1')then
        numb=.true.
      elseif(tchar.eq.'2')then
        numb=.true.
      elseif(tchar.eq.'3')then
        numb=.true.
      elseif(tchar.eq.'4')then
        numb=.true.
      elseif(tchar.eq.'5')then
        numb=.true.
      elseif(tchar.eq.'6')then
        numb=.true.
      elseif(tchar.eq.'7')then
        numb=.true.
      elseif(tchar.eq.'8')then
        numb=.true.
      elseif(tchar.eq.'9')then
        numb=.true.
      elseif(tchar.eq.'0')then
        numb=.true.
      endif
      return
      end

