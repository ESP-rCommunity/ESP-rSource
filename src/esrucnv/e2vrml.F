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

C ESRU vrml translator - .
C Current version October 2011.

C e2vrml: Generate VRLM 2.0 world of esp-r model.

C << consider a variant that writes to SVG or other more
C << modern file format.

C ******* Translate esp data to vrlm world.
C e2vrml takes zones/obstructions and any ground topology and
C creates a grey-scale VRML 2.0 world.
      SUBROUTINE e2vrml(itrc,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

      COMMON/GB1/XB(12),YB(12),ZB(12),JVNB(6,4)

      integer ioin,ioout,ioblk
      common/io/ioin,ioout,ioblk
      COMMON/GTFIL/GTGEOM
      common/grndpl/NGT,NGTV,XGT(MGTV),YGT(MGTV),ZGT(MGTV),JGVN(MGRT,8),
     &  NGVER(MGRT),IVEDGE(MGRT)
      COMMON/GT5/GSNAME(MGRT),GMLCN(MGRT)

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      CHARACTER*72 GTGEOM
      character GDESCR*36,MLCN*12,OTF*4,OPT*12
      character MLCSYM*12
      CHARACTER GMLCN*12,GSNAME*6
      character ZSN*28,q*1
      DIMENSION SNA(MS),PAZI(MS),PELV(MS),ZSN(MS)
      character outs*124,outsd*124,hc*1
      logical XST,numb
      integer llbm    ! length of block material
      
C Setup standard assumptions.
      IFIL=11

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
      call module_opendb(ier)

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so VRML',
     &    'export is not possible.','W')
        ier=1
        return
      endif

C Define a VRML material for each construction in the database.
      WRITE(ioout,'(a)')
     &  '# VRML materials derived from ESP-r constructions...'
      do 166 im= 1,NMLC

C Pick up general description of the composite.
        if(mlcver.eq.0)then
          call parsemlcdesc(DESC(IM),MLCN,OTF,OPT,MLCSYM,isymindex)
        else

        endif

C For the inside and outside layer get surface properties (AE & AI).
        matarrayindex=IPRMAT(IM,1)   ! which array index
        if(matarrayindex.ge.0)then
          AE=matdbina(matarrayindex)
        endif

        matarrayindex=IPRMAT(IM,LAYERS(IM))   ! which array index
        if(matarrayindex.ge.0)then
          AI=matdbina(matarrayindex)
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
        call georead(ITA1,LGEOM(newfoc),newfoc,1,IUOUT,IER)
        CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
        vol(newfoc)=zvol
        zonetotsurfacearea(newfoc)=zoa
        WRITE(ioout,'(a)')' '
        WRITE(ioout,'(3a)')'# zone ',zname(newfoc),' starts here....'
        do 5 j=1,nsur

C Work out the colours for each of the surfaces in this zone by looping
C through each of the MLC and finding a match.

C << place to use mlcver >>

          do 6 im= 1,NMLC
            if(SMLCN(j)(1:12).eq.DESC(IM)(1:12))then

C Pick up general description of the composite.
              call parsemlcdesc(DESC(IM),MLCN,OTF,OPT,MLCSYM,isymindex)

C For the inside and outside layer get surface properties.
              matarrayindex=IPRMAT(IM,1)   ! which array index
              if(matarrayindex.ge.0)then
                AE=matdbina(matarrayindex)
              endif

              matarrayindex=IPRMAT(IM,LAYERS(IM))   ! which array index
              if(matarrayindex.ge.0)then
                AI=matdbina(matarrayindex)
              endif
              goto 7
            endif
  6       continue
  7       continue

C Remeber characters in surface name
          lsn=lnblnk(sname(newfoc,j))
          write(zsn(j),'(3a)') zname(newfoc)(1:lnzname(newfoc)),
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
     &        zname(newfoc)(1:lnzname(newfoc)),' Coordinate { point ['

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
     &        zname(newfoc)(1:lnzname(newfoc))
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

C If there are obstructions associated with this zone convert
C into GB1 commons and write them out using similar logic.
        if(IOBS(newfoc).EQ.1)then
          WRITE(ioout,'(a)')' '
          WRITE(ioout,'(3a)')'# zone ',zname(newfoc),
     &      'obstructions start here....'
        elseif(IOBS(newfoc).EQ.2)then
          WRITE(ioout,'(a)')' '
          WRITE(ioout,'(3a)')'# zone ',zname(newfoc),
     &      'obstructions start here....'
        endif
        if(nbobs(newfoc).eq.0) goto 302
        DO 301 IB=1,nbobs(newfoc)
          if(BLOCKTYP(newfoc,IB)(1:4).eq.'obs ')then
            CALL CNVBLK(XOB(newfoc,IB),YOB(newfoc,IB),ZOB(newfoc,IB),
     &        DXOB(newfoc,IB),DYOB(newfoc,IB),DZOB(newfoc,IB),
     &        BANGOB(newfoc,IB,1))
          elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obs3')then
            CALL CNVBLK3A(XOB(newfoc,IB),YOB(newfoc,IB),ZOB(newfoc,IB),
     &        DXOB(newfoc,IB),DYOB(newfoc,IB),DZOB(newfoc,IB),
     &        BANGOB(newfoc,IB,1),BANGOB(newfoc,IB,2),
     &        BANGOB(newfoc,IB,3))
          elseif(BLOCKTYP(newfoc,IB)(1:4).eq.'obsp')then
            call CNVBLKP(newfoc,IB) ! convert obsp type.
          endif

C Work out the colour for each block by looping
C through each of the MLC and finding a match (for first 12 char).
          do 66 im= 1,NMLC
            if(BLOCKMAT(newfoc,IB)(1:12).eq.DESC(IM)(1:12))then

C Pick up general description of the composite.
              WRITE(MLCN,'(A)')DESC(IM)(1:12)

C For the inside and outside layer get surface properties.
              matarrayindex=IPRMAT(IM,1)   ! which array index
              if(matarrayindex.ge.0)then
                AE=matdbina(matarrayindex)
              endif
              gvt=0.00
              goto 77
            endif
 66       continue
 77       continue

C Remeber characters in block name
          lsn=LNBLOCKNAME(newfoc,IB)

C If the first face write initial anchor for this obstruction
C (which holds its coordinates of along with the first surface.
          do 78 kbs=1,6
            WRITE(ioout,'(a)')'Anchor {'
            WRITE(ioout,'(5a)')'parameter [ ',q,
     &        'target = description',q,' ]'
            WRITE(ioout,'(8a)')'  url [ ',q,cfgroot(1:lnblnk(cfgroot)),
     &       '.html',hc,BLOCKNAME(newfoc,IB)(1:lsn),q,' ]'
            WRITE(ioout,'(a)') '  children ['
            WRITE(ioout,'(a)') '    Shape {'
            if(kbs.eq.1)then   ! include the block material
              llbm=LNBLOCKMAT(newfoc,IB)
              WRITE(ioout,'(2a)')
     &         '     appearance USE Solid_',BLOCKMAT(newfoc,IB)(1:llbm)
              call ifnumber(BLOCKNAME(newfoc,IB)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF a',
     &            BLOCKNAME(newfoc,IB)(1:lsn),kbs,' IndexedFaceSet {'
              else
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF ',
     &            BLOCKNAME(newfoc,IB)(1:lsn),kbs,' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)') '        solid FALSE'
              WRITE(ioout,'(3a)')'        coord DEF COORD_',
     &          BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB)),
     &          ' Coordinate { point ['

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
              llbm=LNBLOCKMAT(newfoc,IB)
              WRITE(ioout,'(2a)')
     &         '     appearance USE Solid_',BLOCKMAT(newfoc,IB)(1:llbm)
              call ifnumber(BLOCKNAME(newfoc,IB)(1:1),numb)
              if(numb)then
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF a',
     &            BLOCKNAME(newfoc,IB)(1:lsn),kbs,' IndexedFaceSet {'
              else
                WRITE(ioout,'(2a,i1,a)')'      geometry DEF ',
     &            BLOCKNAME(newfoc,IB)(1:lsn),kbs,' IndexedFaceSet {'
              endif
              WRITE(ioout,'(a)') '        ccw TRUE'
              WRITE(ioout,'(a)')'        solid TRUE'
              WRITE(ioout,'(2a)')'        coord USE COORD_',
     &          BLOCKNAME(newfoc,IB)(1:LNBLOCKNAME(newfoc,IB))
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
            AE=0.5
            DO 701 imm=1,NMLC
              IF(DESC(imm)(1:12).EQ.GMLCN(igg)(1:12))then
                matarrayindex=IPRMAT(IM,1)   ! which array index
                if(matarrayindex.ge.0)then
                  AE=matdbina(matarrayindex)
                endif
                gvt=0.00
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

