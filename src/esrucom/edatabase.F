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

C The file edatabase.f is a collection of support facilities for 
C reading and reporting ESP-r databases and contains the following:

C  ERMLDB:  Read an ASCII composite db and return data via block MLC.
C  matchmlcdesc: parse string (DES) and see if it matches a MLC
C  ETMLDB:  Provides reporting on current description of a composite.
C  ETMLDBU returns U value of construction based on contents of common MLC.
C  EPKMLC:  Select a composite construction with display of details.
C  EMKAMLD: Creates an ascii composite constr db based on common MLC.
C  EROPTDB: Returns optical properties for TMC.
C  EDWINO:  Allow user to select a glazing type by name.
C  module_opendb: open materials constructions, multi-layer constructions and
C           optical properties databases (for utility applications).

C << todo - depreciate references to IPR array and use Material names only.>>

C ************* ERMLDB
C Given file name and unit number, read  an ASCII construction
C common file and return data via common block MLC.
C IFMUL is the database unit, ITRU unit number for user output,
C IER=0 OK, IER=1 problem. 
C DRAIR is the air gap layer resistance for 3 orientations.

      SUBROUTINE ERMLDB(ITRC,ITRU,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "CFC_common.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,layermatch
      LOGICAL        CFCDBOK

      logical closemat1,closemat2,close4, closecfc1
      logical unixok

      CHARACTER OUTSTR*124,NAM*72,H*72,outs*124
      CHARACTER RSTR*124
      CHARACTER WORD*20,MLCN*12
      character DES*48   ! the contents of line with MLC attributes
      character lworking*144,fs*1

      integer ier
      integer ileghope  ! for legacy record
      integer matarrayindex  ! for materials array index
      integer lex,lnt32,lnsym,lnmlcn     ! lenght of string
      integer cfcarrayindex ! for CFC layers db array index

C Note that the text which follows the thickness of the layer
C is extracted from the materials database and should not
C be edited by the user as it is scanned to check the specific
C named material matches.

C Keep track of how many times materials db read fails.
      matmiss=0
      closemat1=.false.  ! assume binary materils until proven otherwise
      closemat2=.false.

C Flag for CFC layers db version
      closecfc1=.false.

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C If materials database not opened it should have been opened
C prior to this call and matdbok would be false if it
C had been previously opened and failed.
      IF(.NOT.MATDBOK)THEN
        IER=0

C Scan the materials (either binary or ascii) file data into materials
C commons and if this was sucessful and matver was set to 1.2 then
C we can carry on using the materials common blocks for subsequent access.
        call scananymat(ier)
        if(ier.ne.0)then
          call usrmsg('No readable materials file was found so',
     &                'constructions attributes could be at risk.','W')
        endif
      ENDIF

C Do the same for CFClayers db
      IF(.NOT.CFCDBOK)THEN
        IER=0

C Scan the CFClayers db file data into CFC layers
C commons and if this was sucessful and cfcver was set to 1.1 then
C we can carry on using the CFC layers common blocks for subsequent access.
        call scancfcdb(ier)
        if(ier.ne.0)then
          call usrmsg('No readable CFC layers file was found so',
     &                'CFC attributes could be at risk.','W')
        endif
      ENDIF

C Find version of materials database, if neither closemat1 or closemat2
C is true then the material arrays could be problematic.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL USRMSG('Materials database data probably corrupt.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C Find version of CFClayers database, if not closecfc1
C is true then the CFC layer arrays could be problematic.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        CALL USRMSG('CFC layers database data probably corrupt.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C Free unit and check if constructions file exists.  Setup lworking for the
C expanded name to be used later.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=LFMUL
        write(currentfile,'(a)') LFMUL(1:lnblnk(LFMUL))
        CALL EFOPSEQ(IFMUL,LFMUL,1,IER)
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMUL(1:lnblnk(LFMUL))
        write(currentfile,'(a)') lworking(1:lnblnk(lworking))
        CALL EFOPSEQ(IFMUL,lworking,1,IER)
      endif
      IER=0

      IF(IER.NE.0)RETURN

C Read the number of items in the constructions file, followed by
C the number of layers & construction name.
      CALL STRIPC(IFMUL,OUTSTR,1,ND,1,'mlc db items',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NMLC,0,MMLC,'F',
     &              'no of composite constructions',IER)
      IF(IER.NE.0)THEN
        CALL ERPFREE(IFMUL,ISTAT)
        IER=1
        RETURN
      ENDIF

C Fill the MLC commons.
      mlcver = 0     ! constructions version (short names)
      write(mlcdbdoc,'(a)')
     &  'These constructions have not yet been documented.'
      mlccats=1      ! put everything in one category
      write(mlccatname(1),'(a)') 'general constructions'
      write(mlccatdoc(1),'(2a)')
     &  'Category general constructions includes all of the ',
     &  'MLC constructions in the file.'
      mlcdbitems=NMLC      ! remember how many items
      mlccatitems(1)=NMLC  ! put all in one category

C Get and set date stamp for the MLC arrays (since legacy file does not
C include this information.
      call dstamp(mlcdbdate)

C Loop to get all of the declaired constructions. Note if there
C are fewer items then an EOF error state might give a clue. EOF
C is 2 and set ier=3.
      DO 10 I = 1,NMLC
        CALL STRIPC(IFMUL,OUTSTR,0,ND,1,'mlc descr',IIER)
        IF(IIER.NE.0)then
          write(outs,*) 'EOF premature in constructions ',nmlc,iier
          call edisp(iuout,outs)
          ier=3    ! set error state for premature EOF.
          RETURN
        ENDIF

C Set MLC thickness to zero.
        THKMLC(I) =0.0

        K=0
        CALL EGETWI(OUTSTR,K,LAYERS(I),1,MMLC,'F','mlc layers',IER)

C Anything after number of layers is the description (which is overloaded
C to include information on optical properties and either the name
C of a MLC with inverted layers or the keywords SYMMETRIC or NONSYMMERTIC).
        CALL EGETRM(OUTSTR,K,RSTR,'W','layer description',IER)
        DES=RSTR(1:LNBLNK(RSTR))
        IF(IER.NE.0)GOTO 99

C Parse the DES string into string tokens. Also fill the MLC common blocks.
        mlcoptical(I)=' '
        mlcsymetric(I)=' '
        matsymindex(I)=0   ! not yet known 
        mlccatindex(I)= 1  ! only one class for now  
        WRITE(MLCN,'(A)')DES(1:12)
        WRITE(mlcname(I),'(A)')DES(1:12)
        WRITE(mlcdoc(I),'(2A)')DES(1:12),' is a ...'
        WRITE(mlctype(I),'(A)') DES(15:18)
        IF(mlctype(I).EQ.'  ') mlctype(I)='OPAQ'
        IF(mlctype(I)(1:4).NE.'OPAQ'.AND.mlctype(I)(1:3).NE.'CFC')THEN
          WRITE(mlcoptical(I),'(A)') DES(21:32)
          IF(mlcoptical(I)(1:2).EQ.'  ') mlcoptical(I)='UNKNOWN'
        ELSEIF(mlctype(I)(1:3).EQ.'CFC')THEN
          mlcoptical(I)='N/A'
        ELSE
          mlcoptical(I)='OPAQUE'
        ENDIF

C Read tag for name of inverted version or symmetry if the length of the
C DES string could hold the tag, otherwise set mlcsymetric to a blank.
        if(lnblnk(DES).gt.35)then
          WRITE(mlcsymetric(I),'(A)') DES(35:46)
        else
          mlcsymetric(I)='  '
          matsymindex(I)=0   ! signal not applicable     
        endif

C Debug.
C        write(6,*) 'mlc index name doc ',i,mlcname(i),' : ',
C     &    mlcdoc(i)(1:lnblnk(mlcdoc(i)))
C        write(6,*) 'type optical sym ',mlctype(I),' : ',mlcoptical(I),
C     &    ' : ',mlcsymetric(I)

        IF(LAYERS(I).LE.0)GOTO 10

        DO 15, IL=1,LAYERS(I)
          CALL STRIPC(IFMUL,OUTSTR,99,ND,1,'layer line',IER)
          IF(IER.NE.0)RETURN
          K=0

C IF construction is of type CFC2, read layer properties from
C the CFC layers db. Otherwise read layer properties from the 
C materials db. 
          check_CFC2_or_MLC:
     &    IF(mlctype(I)(1:4).EQ.'CFC2') THEN

            CALL EGETWI(OUTSTR,K,ITMCFCDB(I,IL),-1,MGIT_CFC,'F',
     &        'CFClayer db index',IER)
            CALL EGETWR(OUTSTR,K,DTHK(I,IL),0.0001,0.5,'W',
     &        'layer thickness',IER)

C Increment thickness of MLC.
            THKMLC(I)=THKMLC(I)+DTHK(I,IL)

C Current MLDB also includes the name of the layer from the
C CFClayers db.

            CALL EGETRM(OUTSTR,K,RSTR,'W','CFC layer name',IER)
            lex=MIN0(LNBLNK(RSTR),72)
            write(NAM,'(a)')RSTR(1:lex)
            lex=MIN0(LNBLNK(RSTR),124)
            write(LAYDESC(I,IL),'(a)')RSTR(1:lex)

            call parsecfcdesc(ITMCFCDB(I,IL),NAM,cfcarrayindex)
            if(cfcarrayindex.ne.0)then
C If a mis-match echo to the user corrective actions.
              if(cfcarrayindex.ne.ITMCFCDB(I,IL))then
                 call edisp(iuout,' --- WARNING --- ')
                 write(outs,'(a,i3,a,i3,2a)') 
     &            'Duplicate CFC layer names ',
     &            cfcitmindex(cfcarrayindex),' index does not match ',
     &            cfcitmindex(ITMCFCDB(I,IL)),' name: ',
     &            NAM(1:lnblnk(NAM))
                 call edisp(iuout,outs)
                 write(outs,*) 'as referenced in ',
     &            MLCN(1:lnblnk(MLCN))
                 call edisp(iuout,outs)
                 write(outs,'(2a)') 
     &            'Marking the confused layer with a 0. Check the ',
     &            'CFC layers database for two identically named'
                 call edisp(iuout,outs)
                 write(outs,'(2a)') 
     &            'layers, edit to make names unique, save and ',
     &            'then rescan databases and re-make zone files.'
                 call edisp(iuout,outs)
                 write(outs,'(2a)') 
     &           'Or use the interface to locate the named construc-',
     &           'tion and re-select the correct CFC layer.'
                 call edisp(iuout,outs)
                 call edisp(iuout,' --- make a note of this NOW --- ')
                 ITMCFCDB(I,IL)=0  ! could not find it
              else
C Found a matching cfcarray index, assign value to the ITMCFCDB.
                ITMCFCDB(I,IL)=cfcarrayindex
              endif    
            else

C Did not find it, mark as -1
C so that other facilities know about the confusion.
              ITMCFCDB(I,IL)=-1  ! could not find it
              write(outs,*)'a CFC layer referenced in ',
     &          MLCN(1:lnblnk(MLCN)),
     &          ' was not found (setting array index to -1).'
              call edisp(iuout,outs)
            endif

C Standard MLC
          ELSE  ! check_CFC2_or_MLC

C This construction is not of type CFC2 so set ITMCFCDB = -1
          ITMCFCDB(I,IL) = -1
C If the model is associated with a version 1.1 or 1.2 materials file
C some newer entities in the GAPS category may have an index of
C -99 which requires further logic in order to locate the relevant
C material properties.
          CALL EGETWI(OUTSTR,K,IPR(I,IL),-99,600,'F',
     &        'materials db index',IER)
          CALL EGETWR(OUTSTR,K,DTHK(I,IL),0.0001,0.5,'W',
     &        'layer thickness',IER)

C Increment thickness of MLC.
          THKMLC(I)=THKMLC(I)+DTHK(I,IL)

C Current MLDB also include the name of the layer from the materials
C db. If the layer is air then this name contains the R values for
C the air gap. In some cases the initial portion of NAM will be the
C same as matname and this needs to be decoded if IPR is -99.
C To simplify later tasks remember RSTR as LAYDESC. Be sure
C to write less than 72 char into NAM
          IF(ND.GT.2)THEN
            CALL EGETRM(OUTSTR,K,RSTR,'W','layer name:gap R',IER)
            lex=MIN0(LNBLNK(RSTR),72)
            write(NAM,'(a)')RSTR(1:lex)
            lex=MIN0(LNBLNK(RSTR),124)
            write(LAYDESC(I,IL),'(a)')RSTR(1:lex)

C Generally we want to assign values to the IPRMAT array so for each legacy 
C IPR. NOTE: in case there are multiple items with the same characters
C in the name also check to see if the legacy index associated
C with the found item matches the IPR scanned above. If we are
C still confused mark IPRMAT as -1.
            ileghope=IPR(I,IL)   ! what we hope we will find a match for

C Pass the legacy record number (ileghope) along with the name (NAM) of the
C material and get back the index of the material array that fits. If
C nothing found matarrayindex is zero.
            call parsematdesc(ileghope,NAM,matarrayindex)
            if(matarrayindex.ne.0)then

C If a mis-match echo to the user corrective actions.
              if(matlegindex(matarrayindex).ne.IPR(I,IL))then
                call edisp(iuout,' --- WARNING --- ')
                write(outs,'(a,i3,a,i3,2a)') 
     &            'Duplicate material names ',
     &            matlegindex(matarrayindex),' index does not match ',
     &            IPR(I,IL),' name: ',NAM(1:lnblnk(NAM))
                call edisp(iuout,outs)
                write(outs,*) 'as referenced in ',MLCN(1:lnblnk(MLCN))
                call edisp(iuout,outs)
                write(outs,'(2a)') 
     &            'Marking the confused layer with a -99. Check the ',
     &            'materials database for two identically named'
                call edisp(iuout,outs)
                write(outs,'(2a)') 
     &            'materials, edit to make names unique, save and ',
     &            'then rescan databases and re-make zone files.'
                call edisp(iuout,outs)
                write(outs,'(2a)') 
     &            'Or use the interface to locate the named construc-',
     &            'tion and re-select the correct material.'
                call edisp(iuout,outs)
                call edisp(iuout,' --- make a note of this NOW --- ')
                IPRMAT(I,IL)=-1  ! could not find it
              else

C Found a matching matarray index, assign value to the IPRMAT. If
C it actually is equivalent to the legacy zero gap material reflect this
C in the value of IPRMAT (in case parsematdesc did not do this).
                if(IPR(I,IL).eq.0)then
                  IPRMAT(I,IL)=0  ! to signal std air gap
                else
                  IPRMAT(I,IL)=matarrayindex
                endif
              endif    
            else

C Did not find it, check if IPR(I,IL) is zero, if so set
C IPRMAT to zero to signal an air gap.  If not mark as -1
C so that other facilities know about the confusion.
              if(IPR(I,IL).eq.0)then
                IPRMAT(I,IL)=0  ! to signal std air gap
              else
                IPRMAT(I,IL)=-1  ! could not find it
                write(outs,*)'a material referenced in ',
     &            MLCN(1:lnblnk(MLCN)),
     &            ' was not found (setting array index to -1).'
                call edisp(iuout,outs)
              endif
            endif

C For a standard gap pick up 'AIR' or 'gap' followed by assumed R values.
C Currently space in the gap name are not allowed.
            IF(IPR(I,IL).EQ.0)THEN
              K=0
              CALL EGETW(NAM,K,WORD,'W','gap label',IER)
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','vert R',IER)
              DRAIR(I,IL,1)=VAL
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','floor:ceil R',IER)
              DRAIR(I,IL,2)=VAL
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','other R',IER)
              DRAIR(I,IL,3)=VAL
            ELSEIF(IPR(I,IL).LT.0)THEN

C For a layer with no legacy index use the matarray index and assign values.
              matarrayindex=IPRMAT(I,IL)
              if(matarrayindex.ne.0)then
                DBCON=matdbcon(matarrayindex)
                DBDEN=matdbden(matarrayindex)
                DBSHT=matdbsht(matarrayindex)

C If g air gap and version 1.2 then assign DRAIR
                if(matopaq(matarrayindex)(1:1).eq.'g')then
                  if(closemat2)then
                    DRAIR(I,IL,1)=matgapares(matarrayindex,1)
                    DRAIR(I,IL,2)=matgapares(matarrayindex,2)
                    DRAIR(I,IL,3)=matgapares(matarrayindex,3)
                  else
                    continue
                  endif
                endif
              endif
            ELSE
              DRAIR(I,IL,1)=0.0
              DRAIR(I,IL,2)=0.0
              DRAIR(I,IL,3)=0.0

C And check the matarray index in case this is a gap and assign values.
              matarrayindex=IPRMAT(I,IL)
              if(matarrayindex.gt.0)then

C If g air gap and version 1.2 then assign DRAIR
                if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &             matopaq(matarrayindex)(1:1).eq.'h')then
                  if(closemat2)then
                    DRAIR(I,IL,1)=matgapares(matarrayindex,1)
                    DRAIR(I,IL,2)=matgapares(matarrayindex,2)
                    DRAIR(I,IL,3)=matgapares(matarrayindex,3)
                  endif
                endif
              endif
            ENDIF
C Debug.
C            write(6,*) i,il,DRAIR(I,IL,1),DRAIR(I,IL,2),DRAIR(I,IL,3)
            IF(IER.NE.0)GOTO 99
          ELSE

C There are two or less items on the line so the materials db name
C has not been included. If air ask for the air gap
C values, otherwise get element name from materials db.
            IF(IPR(I,IL).EQ.0)THEN

      H(1)='A set of assumptions on the R value of an air gap in'
      H(2)='case the composite construction is used in different'
      H(3)='orientations are requested.  This information supports'
      H(4)='the automatic creation of zone construction files.'
              lnt32=lnblnk(mlcname(i))
              WRITE(OUTSTR,117)IL,mlcname(I)(1:lnt32),
     &          DTHK(I,IL)*1000.0
  117         FORMAT('Default R value for air gap ',I2,' in ',a,
     &            ' (which is',F7.1,' mm thick)')

              if(DRAIR(I,IL,1).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(I,IL,1)
              endif
              CALL EASKR(VAL,OUTSTR,
     &             ' if the construction orientation is vertical: ',
     &             0.0,'W',99.9,'W',0.17,'def air gap R',IER,4)
              DRAIR(I,IL,1)=VAL

              if(DRAIR(I,IL,2).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(I,IL,2)
              endif
              CALL EASKR(VAL,OUTSTR,
     &             ' if the construction is a floor or ceiling  : ',
     &             0.0,'W',99.9,'W',0.17,'def air gap R',IER,4)
              DRAIR(I,IL,2)=VAL

              if(DRAIR(I,IL,3).lt.0.001)then
                 VAL=0.17
              else
                 VAL=DRAIR(I,IL,3)
              endif
              CALL EASKR(VAL,OUTSTR,
     &             ' if the construction is sloped or UNKNOWN   : ',
     &             0.0,'W',99.9,'W',0.17,'def air gap R',IER,4)
              DRAIR(I,IL,3)=VAL
            ELSE
              ier=0

C An air layer includes the R values in the display.
              matarrayindex=IPRMAT(I,IL)
              if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht
                if(matarrayindex.eq.0)then
                  DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
                  NAM='AIR'
                else
                  DBCON=matdbcon(matarrayindex)
                  DBDEN=matdbden(matarrayindex)
                  DBSHT=matdbsht(matarrayindex)
                  write(NAM,'(a)') matname(matarrayindex)(1:32)

C If all values are still 0.0 then not an actual material.
                  call eclose3(DBCON,DBDEN,DBSHT,0.0,0.0,0.0,0.001,
     &              CLOSE4)
                  if(CLOSE4)then
                    write(outs,'(A,I3,2A)') 
     &                ' Material array reference ',matarrayindex,
     &                ' has no data, or all zero...',NAM
                    call usrmsg(outs,'Please check your selection!','W')
                    ier=1  ! set so that matmiss can be incremented
                  endif
                endif
              endif
              if(ier.eq.1) matmiss=matmiss+1

              DRAIR(I,IL,1)=0.0
              DRAIR(I,IL,2)=0.0
              DRAIR(I,IL,3)=0.0
            ENDIF
          ENDIF
          IF(IER.NE.0.and.matmiss.gt.5)GOTO 99

          ENDIF check_CFC2_or_MLC

  15    CONTINUE

C Now that layers have been scanned we can check to see if they
C are symmetric or not. If the symmetric tag has not yet been 
C set check and set mlcsymetric.
        if(mlcsymetric(I)(1:2).EQ.'  ')then
          call ismlcsymmetric(i,layermatch)
          if(layermatch)then
            mlcsymetric(I)='SYMMETRIC   '
          else
            mlcsymetric(I)='NONSYMMETRIC'
          endif
        elseif(mlcsymetric(I)(1:9).EQ.'SYMMETRIC')then
          continue
        elseif(mlcsymetric(I)(1:12).EQ.'NONSYMMETRIC')then
          continue
        endif

C Earlier scanning of the construction data base might
C not have required checking the contents of the materials
C database. ETMLDB does ( it creates those messages about 
C a material having all zero values). Keep track of how
C many of these instances. If there are
        CALL ETMLDB(ITRC,ITRU,I,imerr)
        if(imerr.eq.1) matmiss=matmiss+1
        IF(imerr.NE.0.and.matmiss.gt.5)GOTO 99
  10  CONTINUE
      MLDBOK=.TRUE.

C Look for index for mlcsymetric in all the items in the file.
      do i=1,nmlc
        lnsym=lnblnk(mlcsymetric(I))         
        do 5 ii=1,nmlc
          lnmlcn=lnblnk(mlcname(ii))
          if(mlcsymetric(I)(1:lnsym).eq.mlcname(ii)(1:lnmlcn))then
            matsymindex(I)=ii   ! remember MLC index     
          endif
  5     continue
      end do

C Debug.
C      write(6,*) matsymindex

C Close file before exiting.
  88  CALL ERPFREE(IFMUL,ISTAT)
      RETURN

C If there were more than 5 misses in the database then
C warn the user that the materials database was probably
C the wrong format.
  99  if(matmiss.gt.5)then
        CALL USRMSG(
     &'Quite a few materials were found to be undefined or all zero',
     &'values. The materials db is probably for a different computer',
     &'W')
        IER=2
      elseif(matmiss.gt.1.and.matmiss.le.5)then
        CALL USRMSG(
     &'A few materials were found to be undefined or all zero values.',
     &'The materials db might not match the construction db.','W')
        IER=1
      else
        CALL USRMSG(
     &'A problem was encountered scanning the constructions db',
     &'and you might want to check further.','W')
        IER=1
      endif
      GOTO 88

      END

C ******* matchmlcdesc
C matchmlcdesc parse string (DES) and see if it matches a MLC
C item - if so return its MLC index. DES might be from SMLCN or some 
C other source.
      subroutine matchmlcdesc(DES,imlcindex)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameters DES length is flexable.
      character*(*) DES
      integer imlcindex

C Local variable.
      character MLCN*32
      integer ii,lnmlcn,lndesc

C Look for index for MLCN in the current constructions file.
      imlcindex=0  ! assume no matching MLC          
      lnmlcn=lnblnk(DES)
      WRITE(MLCN,'(A)')DES
      if(MLCN(1:4).eq.'UNKN') return
      do ii=1,nmlc
        lndesc=lnblnk(mlcname(ii))
        if(MLCN(1:lnmlcn).eq.mlcname(ii)(1:lndesc)) then
          imlcindex=ii   ! remember MLC index     
        endif
      end do
      return
      end

C ************* ismlcsymmetric
C isitsymmetric checks construction (index ifoc) to see if its layers
C are symmetrical and return the logical layermatch set to .true. if
C the thickness and air gap resistences are close and the material db
C index match.
C << note if transparent also should check optical properties >> 
      subroutine ismlcsymmetric(ifoc,layermatch) 

#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      logical layermatch,closea,closeb,closec,closed,closee

C If one layer then it is symmetric.
      if(LAYERS(ifoc).eq.1)then
        layermatch=.true.
        return
      endif

C Find one less than half-way through the construction and begin by
C assuming that the layers match.
      LOOP=INT(FLOAT(LAYERS(ifoc))/2.)
      layermatch=.true.
      DO 692 ILL=1,LOOP

C Copy layer data into temporary space. 
        DT=DTHK(ifoc,ILL)
        IPRT=IPR(ifoc,ILL)
        if(IPRT.eq.0)then
          DRT1=DRAIR(ifoc,ILL,1)
          DRT2=DRAIR(ifoc,ILL,2)
          DRT3=DRAIR(ifoc,ILL,3)
        else
          DRT1=0.0
          DRT2=0.0
          DRT3=0.0
        endif
        closea=.false.
        closeb=.false.
        closec=.false.
        closed=.false.
        closee=.false.

C Opposite layer is IOP. Test against data in IOP layer.
        IOP=LAYERS(ifoc)-ILL+1
        CALL ECLOSE(DT,DTHK(ifoc,IOP),0.001,closea)
        if(IPRT.eq.IPR(ifoc,IOP))closeb=.true.

C If the opposite layer is an air gap test against actual
C values, otherwise test against zero.
        if(IPR(ifoc,IOP).eq.0)then
          CALL ECLOSE(DRT1,DRAIR(ifoc,IOP,1),0.002,closec)
          CALL ECLOSE(DRT2,DRAIR(ifoc,IOP,2),0.002,closed)
          CALL ECLOSE(DRT3,DRAIR(ifoc,IOP,3),0.002,closee)
        else
          CALL ECLOSE(DRT1,0.0,0.002,closec)
          CALL ECLOSE(DRT2,0.0,0.002,closed)
          CALL ECLOSE(DRT3,0.0,0.002,closee)
        endif

C If opposite matches continue otherwise set layermatch to false and exit.
        if(closea.and.closeb.and.closec.and.closed.and.closee)then
          continue
        else
          layermatch=.false.
          return
        endif
  692 CONTINUE
      return
      end

C ******* parsematdesc
C Parse the NAM string from the MLC common block returning the index
C (matarrayindex) of the version 1.1 or 1.2 materials array.
C Newer MLC databases will include lines in the form:
C   104    0.0130  gypsum plaster : Gypsum based plaster
C and older MLC databases wil include lines in the form:
C   104    0.0130  Gypsum plaster
C So for newer MLC we are searching for ' : ' but in older
C files there will not be such a string.

C The logic initially looks for ' : ' and trys to match the matname 
C variable and if that fails it trys without ' : ' and if that
C fails it trys against matdoc. Lastly the value of
C ileghope is checked against matlegindex. If no match is found a
C warning is given and matarrayindex is returned as zero.

      subroutine parsematdesc(ileghope,NAM,matarrayindex)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk        ! function definition

C Parameters
      integer ileghope      ! the legacy index to match against
      character NAM*72      ! passed string from MLC
      integer matarrayindex ! returned index in materials array

      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN

      integer lnam,ltnam    ! length of material name
      integer ipos,i2pos,iepos ! position start, 2 char along, end
      character testname*32 ! local name for comparison
      character PNAM*72     ! local string variable for scanning
      logical foundcolon    ! true if ' : ' found
      character outs*124

C Initial assumptions.
      foundcolon=.false.
      matarrayindex=0

C If the ileghope is zero then we are dealing with the standard (implied)
C air gap and so no name is going to match and we can return with the
C initial matarrayindex=0.
      if(ileghope.eq.0)then
        return
      endif

C Copy NAM to PNAM and then search for ' : ' in PNAM and save to local
C strings the matname.
      PNAM=NAM
      lnam=lnblnk(PNAM)
      ipos=1   ! initial positions
      i2pos=3
      iepos=lnam-3
      do 42 ip=1,iepos
        if(PNAM(ipos:i2pos).eq.' : ')then

C Save the string prior to ' : ' in testname and then see if any of
C the matname matches.
          foundcolon=.true.
          write(testname,'(a)') PNAM(1:ipos-1)
          ltnam=lnblnk(testname)

C << todo - see if we can manage without using ileghope >>

C Find a matching matname, if the legacy index associated with this item
C matches ileghope then our search is complete.  If it does not match
C ileghope then there could be duplicate names to check further. If
C ileghope is -1 then return what was found as an index match not possible.
          do 43 imt=1,matdbitems
            if(testname(1:ltnam).eq.matname(imt)(1:ltnam))then
              if(matlegindex(imt).eq.ileghope)then
                matarrayindex=imt
                return
              else
                if(ileghope.eq.-1)then
                  matarrayindex=imt
                  return
                else
                  continue   ! keep looking for a text and index match
                endif
              endif
            endif
  43      continue
        else
          ipos=ipos+1
          i2pos=i2pos+1
        endif
  42  continue

C If we did not find a ' : ' then take the PNAM literally and check against both
C the matname and if that fails against matdoc
      if(.NOT.foundcolon)then

C Find a matching matname.
        do 44 imt=1,matdbitems
C check first the lenght of strings, if the stringsa re not equal the
C material is different
          if(lnblnk(matname(imt)).eq.lnam) then        
             if(PNAM(1:lnam).eq.matname(imt)(1:lnam))then

C Debug.
C            write(6,*) imt,' name x: match ',PNAM(1:lnam),' ',
C     &        matname(imt)(1:lnblnk(matname(imt)))
               matarrayindex=imt
               return
             endif
          endif
  44    continue

C If we got to this point also check for a matching matdoc string.
        do 45 imt=1,matdbitems
          if(PNAM(1:lnam).eq.matdoc(imt)(1:lnam))then

C Debug.
C            write(6,*) imt,' doc x: match ',PNAM(1:lnam),' ',
C     &        matdoc(imt)(1:lnam)
            matarrayindex=imt
            return
          endif
  45    continue
      endif

C There might have been a ' : ' but a text mismatch for matname so check matdoc.
      do 46 imt=1,matdbitems
        if(PNAM(1:lnam).eq.matdoc(imt)(1:lnam))then

C Debug.
C          write(6,*) imt,' doc match ',PNAM(1:lnam),' ',
C     &      matdoc(imt)(1:lnam)
          matarrayindex=imt
          return
        endif
  46  continue

C Could not find it via a string match so see if there is a legacy index that
C matches the required index.  This sometimes happens if the current version
C materials database has a material name edited and the MLC database does not
C have a matching string (but the legacy index is correct non-the-less).
      do 47 imt=1,matdbitems
        if(matlegindex(imt).eq.ileghope)then

C Debug.
C          write(6,*) imt,ileghope,matlegindex(imt),' index match ',
C     &      PNAM(1:lnam),' ',matname(imt)(1:lnblnk(matname(imt)))
          matarrayindex=imt
          return
        endif
  47  continue

C If we got here there was no match. The value of matarrayindex remains at zero.
      lnam=lnblnk(PNAM)
      write(outs,*) 
     &  'When scanning materials db looking for legacy index',ileghope
      call edisp(iuout,outs)
      write(outs,*) 'no match found for material named ',PNAM(1:lnam)
      call edisp(iuout,outs)
  
      return
      end

C ******* parsecfcdesc
C Parse the NAM string from the MLC common block returning the index
C (cfcarrayindex) of the version 1.1 CFC layers db index.
C Newer MLC databases will include lines in the form:
C   104    0.0130  gypsum plaster : Gypsum based plaster
C and older MLC databases wil include lines in the form:
C   104    0.0130  Gypsum plaster
C So for newer MLC we are searching for ' : ' but in older
C files there will not be such a string.

C The logic initially looks for ' : ' and trys to match the cfcname 
C variable and if that fails it trys without ' : ' and if that
C fails it trys against cfcdoc. If no match is found a
C warning is given and matarrayindex is returned as zero.

      subroutine parsecfcdesc(cfclikelyidx, NAM,cfcarrayindex)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk        ! function definition

C Parameters
      integer cfclikelyidx
      character NAM*72      ! passed string from MLC
      integer cfcarrayindex ! returned index in materials array

      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN

      integer lnam,ltnam    ! length of material name
      integer ipos,i2pos,iepos ! position start, 2 char along, end
      character testname*32 ! local name for comparison
      character PNAM*72     ! local string variable for scanning
      logical foundcolon    ! true if ' : ' found
      character outs*124

C Initial assumptions.
      foundcolon=.false.
      cfcarrayindex=0

C Copy NAM to PNAM and then search for ' : ' in PNAM and save to local
C strings the matname.
      PNAM=NAM
      lnam=lnblnk(PNAM)
      ipos=1   ! initial positions
      i2pos=3
      iepos=lnam-3
      do 42 ip=1,iepos
        if(PNAM(ipos:i2pos).eq.' : ')then

C Save the string prior to ' : ' in testname and then see if any of
C the cfcname matches.
          foundcolon=.true.
          write(testname,'(a)') PNAM(1:ipos-1)
          ltnam=lnblnk(testname)
C Find a matching cfcname, if the CFClayers db index associated with this item
C matches ileghope then our search is complete.  If it does not match
C ileghope then there could be duplicate names to check further. If
C ileghope is -1 then return what was found as an index match not possible.
          do 43 imt=1,cfcdbitems
            if(testname(1:ltnam).eq.cfcname(imt)(1:ltnam))then
              if(imt.eq.cfclikelyidx)then
                cfcarrayindex=imt
                return
              else
                continue   ! keep looking for a text and index match
              endif
            endif
  43      continue
        else
          ipos=ipos+1
          i2pos=i2pos+1
        endif
  42  continue

C If we did not find a ' : ' then take the PNAM literally and check against both
C the matname and if that fails against matdoc
      if(.NOT.foundcolon)then
C Find a matching matname.
        do 44 imt=1,cfcdbitems
C check first the lenght of strings, if the stringsa re not equal the
C CFC layer is different
          if(lnblnk(cfcname(imt)).eq.lnam) then        
             if(PNAM(1:lnam).eq.cfcname(imt)(1:lnam))then
               cfcarrayindex=imt
               return
             endif
          endif
  44    continue

C If we got to this point also check for a matching cfcdoc string.
        do 45 imt=1,cfcdbitems
          if(PNAM(1:lnam).eq.cfcdoc(imt)(1:lnam))then
            cfcarrayindex=imt
            return
          endif
  45    continue
      endif

C There might have been a ' : ' but a text mismatch for cfcname so check cfcdoc.
      do 46 imt=1,cfcdbitems
        if(PNAM(1:lnam).eq.cfcdoc(imt)(1:lnam))then
          cfcarrayindex=imt
          return
        endif
  46  continue

C If we got here there was no match. The value of cfcarrayindex remains at zero.
      lnam=lnblnk(PNAM)
      write(outs,*) 
     &  'When scanning CFClayers db '
      call edisp(iuout,outs)
      write(outs,*) 'no match found for CFC layer named ',PNAM(1:lnam)
      call edisp(iuout,outs)
  
      return
      end

C *************** ETMLDB
C ETMLDB provides reporting on current description of a construction
C based on contents of material.h. Should work for mlcver 0 and 1.
      SUBROUTINE ETMLDB(ITRC,ITRU,IFOC,ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition
      
      COMMON/GVALCAL/CLAMDA(MMLC)

      integer matarrayindex ! the indes within matdatarray
      integer cfcarrayindex ! CFClayers db index
      integer lnam    ! length of material name
      integer lfordoc ! length available for material documentation
      logical closemat1,closemat2, closecfc1
      logical bCloseToZero
      integer lnlist

      DIMENSION PNAM(ME)
      CHARACTER OUTSTR*148,PNAM*124
      CHARACTER SOPT*12,GDESCR*36
      integer lnmlcn,lnmlco,lnother
      CHARACTER layertp*9

      real RSI  ! used in reporting of R value in SI units
      
C Pick up general description of the composite.
      lnmlcn=lnblnk(mlcname(ifoc))
      lnmlco=lnblnk(mlcoptical(ifoc))
      isymindex=matsymindex(ifoc)
      ier=0
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        IF(mlctype(IFOC)(1:4).NE.'OPAQ'.AND.
     &     mlctype(IFOC)(1:3).NE.'CFC')THEN
          WRITE(OUTSTR,'(5a,f6.3)')
     &      ' Details of transparent construction: ',
     &      mlcname(ifoc)(1:lnmlcn),' with ',
     &      mlcoptical(ifoc)(1:lnmlco),
     &      ' optics and overall thickness ',THKMLC(ifoc)
          CALL EDISP(ITRU,OUTSTR)
        ELSEIF(mlctype(IFOC)(1:3).EQ.'CFC')THEN
          WRITE(OUTSTR,'(3a)')' ',
     &      'Details of complex fenestration construction: ',
     &      mlcname(ifoc)(1:lnmlcn)
          CALL EDISP(ITRU,OUTSTR)
        ELSE
          if(isymindex.eq.0)then
            WRITE(OUTSTR,'(3a,f6.3)')
     &      ' Details of opaque construction: ',
     &      mlcname(ifoc)(1:lnmlcn),
     &      ' and overall thickness ',THKMLC(ifoc)
          else
            lnother=lnblnk(mlcname(isymindex))
            WRITE(OUTSTR,'(5a,f6.3)')
     &      ' Details of opaque construction: ',
     &      mlcname(ifoc)(1:lnmlcn),
     &      ' linked to ',mlcname(isymindex)(1:lnother),
     &      ' & with overall thickness ',THKMLC(ifoc)
          endif
          CALL EDISP(ITRU,OUTSTR)
        ENDIF
      ENDIF

C Check version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL EDISP(ITRU,'Materials data may be incomplete.')
      endif

C Check version of CFClayers database.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        CALL EDISP(ITRU,'CFClayers data may be incomplete.')
      endif

C If construction is of CFC2 type, access CFClayers db commons
C for reporting
      check_CFC2_or_MLC:
     &If(mlctype(IFOC)(1:4).EQ.'CFC2')THEN

C Reporting based on report level.
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' ')
        write(OUTSTR,'(2a)')
     &' Layer|CFC |Thick |Conduc-|Density|Specif|Layer tp|',
     &' Description'
        CALL EDISP(ITRU,OUTSTR)
        write(OUTSTR,'(1a)')
     &'      |db  |(mm)  |tivity |       |heat  |        |'
        CALL EDISP(ITRU,OUTSTR)
      ELSEIF(ITRC.EQ.1)THEN
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Layer|CFC |Thick | Description ')
        CALL EDISP(ITRU,'      |db  |(mm)  | of material ')
      ENDIF

C Loop through all of the layers and compose RT for the construction.
      RT=0.
      DO 16, IL=1,LAYERS(IFOC)
      
C If data available in CFClayers array gather it. 
        cfcarrayindex=ITMCFCDB(IFOC,IL)   ! which array index
        if(cfcarrayindex.le.0)then
C Referenced item was not found. Write zeros.
          DBCON=0.0
          DBDEN=0.0
          DBSHT=0.0
          E=0.0
          A=0.0
          write(PNAM(IL),'(a)') 'unknown : unknown '
          write(layertp,'(a)') 'unknown' 
        else
          DBCON=cfcdbcon(cfcarrayindex)
          DBDEN=cfcdbden(cfcarrayindex)
          DBSHT=cfcdbsht(cfcarrayindex)
          E=0.0
          A=0.0
          if(CFCshdtp(cfcarrayindex).eq.iGasGap)then
            write(layertp,'(a)') 'Gap'
          elseif(CFCshdtp(cfcarrayindex).eq.iGlazing)then          
            write(layertp,'(a)') 'Glazing'
          elseif(CFCshdtp(cfcarrayindex).eq.iVenBlind)then
            write(layertp,'(a)') 'Ven.Blnd'
          elseif(CFCshdtp(cfcarrayindex).eq.iPleatedDrape)then
            write(layertp,'(a)') 'Pltd.Drp'
          elseif(CFCshdtp(cfcarrayindex).eq.iRollerBlind)then
            write(layertp,'(a)') 'Rol.Blnd'
          elseif(CFCshdtp(cfcarrayindex).eq.iInsectScreen)then
            write(layertp,'(a)') 'Ins.Scrn'
          endif
C Make up PNAM as combination of cfcnam and cfcdoc.
          lnam=lnblnk(cfcname(cfcarrayindex))
          lfordoc = 124 - (lnam +4)    ! space left for doc
          write(PNAM(IL),'(3a)') cfcname(cfcarrayindex)(1:lnam),
     &      ' : ',cfcdoc(cfcarrayindex)(1:lfordoc)
        endif

C Reporting.
        IF(ITRC.GT.1)THEN
          lnlist = MIN0(LNBLNK(PNAM(IL)),82)    ! space left for doc
          if(LAYERS(IFOC).eq.1)then 
            WRITE(OUTSTR,26)IL,cfcitmindex(ITMCFCDB(IFOC,IL)),
     &              DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,layertp,PNAM(IL)(1:lnlist)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.1)then 
            WRITE(OUTSTR,27)' Ext ',cfcitmindex(ITMCFCDB(IFOC,IL)),
     &              DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,layertp,PNAM(IL)(1:lnlist)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.LAYERS(IFOC))then 
            WRITE(OUTSTR,27)' Int ',cfcitmindex(ITMCFCDB(IFOC,IL)),
     &              DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,layertp,PNAM(IL)(1:lnlist)
          else 
            WRITE(OUTSTR,26)IL,cfcitmindex(ITMCFCDB(IFOC,IL)),
     &              DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,layertp,PNAM(IL)(1:lnlist)
          endif
  26      FORMAT(I5,I5,F7.1,F10.3,F7.0,F7.0,1X,A,A)
  27      FORMAT(A5,I5,F7.1,F10.3,F7.0,F7.0,1X,A,A)
          CALL EDISP(ITRU,OUTSTR)
        ELSEIF(ITRC.EQ.1)THEN

C Compact report has more room for material description.
          WRITE(OUTSTR,30)IL,cfcitmindex(ITMCFCDB(IFOC,IL)),
     &      DTHK(IFOC,IL)*1000.0,
     &      PNAM(IL)(1:lnblnk(PNAM(IL)))
  30      FORMAT(I5,I5,F8.1,1X,a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

  16  CONTINUE

      ELSE ! standard MLC (not CFC)

C Reporting based on report level.
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' ')
        write(OUTSTR,'(2a)')
     &' Layer|Thick |Conduc-|Density|Specif|IR  |Solar|Diffu|',
     &' R    |Description'
        CALL EDISP(ITRU,OUTSTR)
        write(OUTSTR,'(2a)')
     &'      |(mm)  |tivity |       |heat  |emis|abs  |resis|',
     &'m^2K/W'
        CALL EDISP(ITRU,OUTSTR)
      ELSEIF(ITRC.EQ.1)THEN
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Layer|Thick | Description ')
        CALL EDISP(ITRU,'      |(mm)  | of material ')
      ENDIF

C Loop through all of the layers and compose RT for the construction.
      RT=0.
      DO 15, IL=1,LAYERS(IFOC)
      
C If data available in material array gather it. And if an air
C layer (matarrayindex of zero or matopaq 'g' or 'h' then
C reset dbcon dbden dbsht andinclude the R values in the display.
        matarrayindex=IPRMAT(IFOC,IL)   ! which array index
        if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &     matopaq(matarrayindex)(1:1).eq.'h'.or.
     &     matarrayindex.eq.0)then
          DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
          E=0.99; A=0.99; DRV=1.0

          WRITE(PNAM(IL),'(A,3F5.2)')'air ',(DRAIR(IFOC,IL,IY),IY=1,3)
          RT=RT+DRAIR(IFOC,IL,1)
          RSI=DRAIR(IFOC,IL,1)
        elseif(matarrayindex.lt.0)then

C Referenced item was not in the materials hash list. Write zeros.
          DBCON=0.0
          DBDEN=0.0
          DBSHT=0.0
          E=0.0
          A=0.0
          DRV=0.0
          write(PNAM(IL),'(a)') 'unknown : unknown '

        else
          DBCON=matdbcon(matarrayindex)
          DBDEN=matdbden(matarrayindex)
          DBSHT=matdbsht(matarrayindex)
          E=matdbine(matarrayindex)
          A=matdbina(matarrayindex)
          DRV=matdbdrv(matarrayindex)

C Make up PNAM as combination of matnam and matdoc.
          lnam=lnblnk(matname(matarrayindex))
          lfordoc = 124 - (lnam +4)    ! space left for doc
          write(PNAM(IL),'(3a)') matname(matarrayindex)(1:lnam),
     &      ' : ',matdoc(matarrayindex)(1:lfordoc)

C Also report on thermal resistance (m^2K/W) of the layer. If dbcon
C is zero then avoid divide by zero.
          call eclose ( dbcon, 0.0, 1E-06, bCloseToZero)
          if(.not. bCloseToZero)then
            RSI=DTHK(IFOC,IL)/DBCON
          else
            RSI=1.0
          endif
          if(.not. bCloseToZero)then
            RT=RT+DTHK(IFOC,IL)/DBCON
          else
            RT=1.0
          endif
        endif

C Reporting.
        IF(ITRC.GT.1)THEN
          lnlist = MIN0(LNBLNK(PNAM(IL)),82)    ! space left for doc
          if(LAYERS(IFOC).eq.1)then 
            WRITE(OUTSTR,24)IL,DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:lnlist)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.1)then 
            WRITE(OUTSTR,25)' Ext ',DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:lnlist)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.LAYERS(IFOC))then 
            WRITE(OUTSTR,25)' Int ',DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:lnlist)
          else 
            WRITE(OUTSTR,24)IL,DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:lnlist)
          endif
  24      FORMAT(I5,F7.1,F10.3,F7.0,F7.0,F5.2,F5.2,F7.0,F6.2,1X,A)
  25      FORMAT(A5,F7.1,F10.3,F7.0,F7.0,F5.2,F5.2,F7.0,F6.2,1X,A)
          CALL EDISP(ITRU,OUTSTR)
        ELSEIF(ITRC.EQ.1)THEN

C Compact report has more room for material description.
          WRITE(OUTSTR,29)IL,DTHK(IFOC,IL)*1000.0,
     &      PNAM(IL)(1:lnblnk(PNAM(IL)))
  29      FORMAT(I5,F8.1,1X,a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

  15  CONTINUE

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
      CLAMDA(IFOC)=1./RT
      RTH=RT+0.04+0.13
      UVALUEH=1.0/RTH
      RTU=RT+0.04+0.10
      UVALUEU=1.0/RTU
      RTD=RT+0.04+0.17
      UVALUED=1.0/RTD
      RTI=RT+0.13+0.13
      UVALUEI=1.0/RTI

      If(mlctype(IFOC)(1:4).NE.'CFC2')THEN
        write(OUTSTR,'(A,3F7.3,a,F7.3)')
     &    ' ISO 6946 U values (horiz/upward/downward heat flow)=',
     &    UVALUEH,UVALUEU,UVALUED,' (partition)',UVALUEI

        IF(ITRC.NE.0)call edisp(itru,OUTSTR)

C Include admittance calculations (use TM33 Table 4.5 values for admittance calcs)
C If in verbose mode, otherwise ignore.
C << NOTE this code needs further debugging.
        if(ITRC.gt.1)then
          Rsi=0.12
          Rso=0.06
          call ADMIT(ITRU,IFOC,Rsi,Rso)
        endif
      endif

C Also list optical properties for this construction (if it is
C transparent and the optical property is something other than
C QPAQ.
      if(mlctype(IFOC)(1:4).NE.'OPAQ'.AND.
     &   mlctype(IFOC)(1:3).NE.'CFC')then
        if(mlcoptical(ifoc)(1:4).ne.'OPAQ'.and.
     &     mlcoptical(ifoc)(1:4).ne.'UNKN')then
          write(SOPT,'(a)') mlcoptical(ifoc)(1:12)
          IF(ITRC.NE.0)CALL EROPTDB(1,itru,SOPT,GDESCR,IER)
        endif
      endif

      ENDIF check_CFC2_or_MLC

      RETURN
      END

C *************** ETMLDBU
C ETMLDBU return ISO 6946 U values (UVH/UVU/UVD/UVI) of current construction based
C on contents of material.h. Logic is a variant of ETMLDB. Should work for mlcver 0 and 1.
      SUBROUTINE ETMLDBU(ITRC,ITRU,IFOC,UVH,UVU,UVD,UVI,UVG)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters passed
      integer itrc  ! trace level
      integer itru  ! unit to write to
      integer ifoc  ! which MLC construction (array index)
      real UVH      ! U value for horizontal flow
      real UVU      ! U value for inside upward flow
      real UVD      ! U value for inside downward flow
      real UVI      ! U value for inside partition
      real UVG      ! U value for inside downward flow to ground

C Material properties.
      integer matarrayindex ! the indes within matdatarray
      logical bCloseToZero

      CHARACTER OUTSTR*124

      real RTH  ! sum of surface coef for horiz flow at wall
      real RTU  ! sum of surface coef for upward flow at ceiling
      real RTD  ! sum of surface coef for downward flow at floor
      real RTI  ! sum of surface coef for horiz flow at partition
      real RTG  ! sum of surface coef at foundation
      integer lnmlcn,lnmlco

C Pick up lengths of strings in general description of the construction.
      lnmlcn=lnblnk(mlcname(ifoc))
      lnmlco=lnblnk(mlcoptical(ifoc))

C Reset U value reporting local variables.
      RT=0.

      DO 15, IL=1,LAYERS(IFOC)

C If data available in material array gather it. And if an air
C layer reset dbcon dbsht andinclude the R values in the display.
        matarrayindex=IPRMAT(IFOC,IL)   ! which array index

C And if matarrayindex is zero then resetn dbcon dbsht.
        if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &     matopaq(matarrayindex)(1:1).eq.'h'.or.
     &     matarrayindex.eq.0)then
          DBCON=0.0
          RT=RT+DRAIR(IFOC,IL,1)
          RSI=DRAIR(IFOC,IL,1)
        elseif(matarrayindex.lt.0)then
          DBCON=0.0  ! unknown material
        else
          DBCON=matdbcon(matarrayindex)

C Also report on thermal resistance (m^2K/W) of the layer. If dbcon
C is zero then avoid divide by zero.
          call eclose ( dbcon, 0.0, 1E-06, bCloseToZero)
          if(.not. bCloseToZero)then
            RSI=DTHK(IFOC,IL)/DBCON
          else
            RSI=1.0
          endif
          if(.not. bCloseToZero)then
            RT=RT+DTHK(IFOC,IL)/DBCON
          else
            RT=1.0
          endif
        endif

C Calculate and (optionally) report.
  15  CONTINUE

C ISO 6946 surface resistance assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow as well as
C an inside wall to inside wall hc 0.13 both sides. For the case
C of a foundation only the inside surface resistance is used.
      RTH=RT+0.04+0.13
      UVH=1.0/RTH
      RTU=RT+0.04+0.10
      UVU=1.0/RTU
      RTD=RT+0.04+0.17
      UVD=1.0/RTD
      RTI=RT+0.13+0.13
      UVI=1.0/RTI
      RTG=RT+0.17
      UVG=1.0/RTG
      write(OUTSTR,'(A,4F7.3)')
     &  ' ISO 6946 U values (horizontal/upward/downward/partition)= ',
     &  UVH,UVU,UVD,UVI
      IF(ITRC.NE.0)then
        CALL EDISP(ITRU,' ')
        if(mlctype(IFOC)(1:4).NE.'OPAQ'.AND.
     &     mlctype(IFOC)(1:3).NE.'CFC')then
          WRITE(OUTSTR,'(5a,F6.3,a,F6.3,a,f6.3,a,f6.3,a,f6.3)')
     &    ' ISO 6946 U values of ',mlcname(ifoc)(1:lnmlcn),
     &    ' with ',mlcoptical(ifoc)(1:lnmlco),
     &    ' optics are (horiz flow)',UVH,' (upwards flow)',UVU,
     &    ' (downwards flow)',UVD,' (partition) ',UVI,' (fndtn) ',UVG
        ELSE
          WRITE(OUTSTR,'(3a,F6.3,a,f6.2,a,f6.2,a,f6.3,a,f6.3)')
     &    ' ISO 6946 U values of ',mlcname(ifoc)(1:lnmlcn),
     &    ' are (horiz flow)',UVH,' (upwards flow)',UVU,
     &    ' (downwards flow)',UVD,' partition ',UVI,' (fndtn) ',UVG
        ENDIF
        call edisp(itru,OUTSTR)
      endif

      RETURN
      END

C ************* EPKMLC 
C EPKMLC Select a composite construction with display of details.
C Information is available via common block MLC.
C IER=0 OK, IER=1 problem.
      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      DIMENSION IVAL(MMLC+1),COMPS(MMLC+1)
      CHARACTER*(*) PROMPT1,PROMPT2
      CHARACTER H*72,COMPS*33

      integer lnmlcn

      IER=0
      ISEL=0

C If the common block MLC has not yet been filled the read in the
C conposite construction database.
      IF(.NOT.MLDBOK)THEN
        CALL ERMLDB(0,iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',' Unable to display selections ','W')
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this construction based on info
C in the materials database.
      H(1)='This information will be written into a zone construc-'
      H(2)='tion file according to the types of multilayer'
      H(3)='construction used by each surface.'
      H(4)='If you want to unreference a construction and reset'
      H(5)='it to UNKNOWN select the initial item in the list.'
      H(6)=' '
      H(7)=' '
      H(8)='For Controls of bidirectional data only: '
      H(9)='Select the construction for which the controls and the '
      H(10)='switch between different datasets will be applied on.'
      H(11)='WARNING: Avoid assigning more than one control loop '
      H(12)='to the same construction, especially when these controls '
      H(13)='are attempting to assign different datasets. The first '          
      H(14)='loop will be used if this mistake is made by the users. '

C Present list based on the name and type of the MLC.
      WRITE(COMPS(1),'(A)')'UNKNOWN (at this time) '
      DO 66 IWW=1,NMLC
        lnmlcn=lnblnk(mlcname(iww))
        WRITE(COMPS(IWW+1),'(3A)')mlcname(IWW)(1:lnmlcn),' ',
     &    mlctype(IWW)
   66 CONTINUE
      IX=1
      CALL EPICKS(IX,IVAL,PROMPT1,PROMPT2,
     &  33,NMLC+1,COMPS,'Construction composites',IER,14)

      IF(IX.EQ.0)RETURN
      ISEL=IVAL(1)
      if(ISEL.eq.1)then
        call edisp(iuout,'UNKNOWN MLC selected')
        ISEL=0
        return
      else
        ISEL=ISEL-1
        CALL ETMLDB(2,iuout,ISEL,imerr)
        call edisp(iuout,
     & ' ISO 6946 U value assumes: Rso=0.04 & Rsi=0.13 (m**2deg.C/W)')
        RETURN
      endif
      END


C *************** EMKAMLD 
C EMKAMLD: Creates a legacy ascii constructions database based on the
C current contents of common MLC.  In the case of an air layer sub-
C stitute the U value for each layer unless we are dealing with
C one of the [g] or [h] material types in which case the name
C is written out.

C This subroutine uses the material array data in matdatarray. Currently it
C writes the layer line with material name and documentation separate by 
C a : but truncates the total length to 72 characters. A future variant might 
C relax this limitation so as not to truncate the material documentation.
      SUBROUTINE EMKAMLD(ITRU,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer itru    ! feedback or file unit
      integer ier     ! zero is ok, one mlc error, two mat array issue
     
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      integer lnam    ! length of material name
      integer lfordoc ! length available for material documentation
      integer izero   ! transitional trick for air gap
      logical closemat1,closemat2
      logical closecfc1
      
      integer cfcarrayindex

      DIMENSION PNAM(ME)
      CHARACTER NAM*72,PNAM*72
      character lworking*144,fs*1
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume that data on composites is in common MLC.
      IF(.NOT.MLDBOK)THEN
        CALL LUSRMSG(' no information to put in ',LFMUL,'W')
        IER=1
        RETURN
      ENDIF
      IF(NMLC.GT.MMLC)THEN
        CALL LUSRMSG(' too many items to put in ',LFMUL,'W')
        IER=1
        RETURN
      ENDIF

C Check version of material database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL USRMSG('Materials database data not yet filled.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C Check version of material database.
      call eclose(cfcver,1.1,0.01,closecfc1)
      if(closecfc1)then
        continue
      else
        CALL USRMSG('CFC layers database data not yet filled.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C The ASCII file will be overwritten if it exists.
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        CALL EFOPSEQ(IFMUL,LFMUL,4,IER)
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    LFMUL(1:lnblnk(LFMUL))
        CALL EFOPSEQ(IFMUL,lworking,4,IER)
      endif
      
C Write out a file header.
      WRITE(IFMUL,30)LFMUL(1:lnblnk(LFMUL)),LFMAT(1:lnblnk(LFMAT)),
     &               LCFCDB(1:lnblnk(LCFCDB))
  30  FORMAT('# composite construction db defined in ',A,/,
     &       '# based on materials db ',A,/,
     &       '# and based on CFClayers db ',A)

C For each composite construction write the number of layers (LAYERS),
C its description and then the materials db reference IPR,
C thickness DTHK and material name NAM.  
C In the case of db reference 0 (air) overwrite NAM with the 3 values
C of DRAIR.
C For CFC2 type constructions, write the number of layers (LAYERS),
C description DESC, CFClayers db reference ITMCFCDB, thickness DTHK
C and CFC layer name NAM
      WRITE(IFMUL,'(I5,A)')NMLC,'     # no of composites '

      DO 110 I = 1,NMLC
        IF(LAYERS(I).EQ.0)GOTO 110
        DO 116, IL=1,LAYERS(I)

          check_CFC2_or_MLC:
     &    IF(mlctype(I)(1:4).EQ.'CFC2') THEN
            cfcarrayindex=ITMCFCDB(I,IL)

            IF(cfcarrayindex.EQ.0)THEN
C A confused CFC layer found.
              write(NAM,'(a)')
     &        'unable-to-determine : unable-to-determine CFC layer name'
              write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
            ELSEIF(cfcarrayindex.GT.0)THEN
C Make up NAM as combination of cfcnam and cfcdoc.
              lnam=lnblnk(cfcname(cfcarrayindex))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(NAM,'(3a)') cfcname(cfcarrayindex)(1:lnam),
     &        ' : ',cfcdoc(cfcarrayindex)(1:lfordoc)
              write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
            ENDIF

          ELSE ! check_CFC2_or_MLC
            matarrayindex=IPRMAT(I,IL)
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              WRITE(PNAM(IL),'(A,1X,3F6.3)')'gap',
     &             (DRAIR(I,IL,IY),IY=1,3)
            ELSEIF(matarrayindex.LT.0)THEN

C A confused material found.
              write(NAM,'(a)')
     &        'unable-to-determine : unable-to-determine material name'
              write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
            ELSE

C Make up NAM as combination of matnam and matdoc.
              lnam=lnblnk(matname(matarrayindex))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(NAM,'(3a)') matname(matarrayindex)(1:lnam),
     &        ' : ',matdoc(matarrayindex)(1:lfordoc)
              write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
            ENDIF
          ENDIF check_CFC2_or_MLC
  116   CONTINUE

C Write via material commons with comment before and after names.
        write(IFMUL,'(a)')
     &    '# layers  description  type  optics name   symmetry tag'
        write(IFMUL,'(i5,4x,8a)')LAYERS(I),mlcname(i)(1:12),'  ',
     &        mlctype(i)(1:4),'  ',mlcoptical(i)(1:12),'  ',
     &        mlcsymetric(i)(1:12),'  '
        write(IFMUL,'(a)')
     &    '# material ref thickness (m) description & air gap R'

C As a transition, for gap layers the index is going to have to
C be a zero to trick the reading code into recognizing it as
C a gap layer. 
        DO 115, IL=1,LAYERS(I)
          check_CFC2_or_MLC_2:
     &    IF(mlctype(I)(1:4).EQ.'CFC2') THEN
            cfcarrayindex=ITMCFCDB(I,IL)
            WRITE(IFMUL,'(I5,F10.4,2X,A)')ITMCFCDB(I,IL),
     &        DTHK(I,IL),PNAM(IL)(1:lnblnk(PNAM(IL)))
          ELSE ! check_CFC2_or_MLC_2
            matarrayindex=IPRMAT(I,IL)
            if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &         matopaq(matarrayindex)(1:1).eq.'h'.or.
     &         matarrayindex.eq.0)then
              izero=0
              WRITE(IFMUL,'(I5,F10.4,2X,A)')izero,DTHK(I,IL),
     &          PNAM(IL)(1:lnblnk(PNAM(IL)))
            else
              WRITE(IFMUL,'(I5,F10.4,2X,A)')IPR(I,IL),DTHK(I,IL),
     &          PNAM(IL)(1:lnblnk(PNAM(IL)))
            endif
          ENDIF check_CFC2_or_MLC_2
  115   CONTINUE
  110 CONTINUE

      CALL ERPFREE(IFMUL,ISTAT)
      RETURN

      END

C ******************* EROPTDB 
C EROPTDB reads the glazing optical database and searches for
C data on the named type SOPT returning info via COMMON/GOPT.
C If SOPT = 'ALL' then, else if SOPT = 'TMP' then copy the
C database to a temporary file on unit itopt (assumed to have
C already been opened). If user has requested a listing of the
C database then use itopt (which may be text feedback or a file unit).
      SUBROUTINE EROPTDB(ITRC,ITOPT,SOPT,GDESCR,IER)
#include "building.h"
#include "espriou.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure database)
C LOPTDB,IOPTDB (for optical database)
      
      integer lnblnk  ! function definition

C Parameters
      integer ITRC  ! if >= 1 the report as file is scanned
      integer ITOPT ! unit to send reports to
      character SOPT*12   ! optical type to match (also ALL & TMP)
      character GDESCR*36 ! description of the matching type
      integer IER   ! zero is ok, one if file could not be opened

      COMMON/OUTIN/IUOUT,IUIN

C DG Direct trans at 5 angles.
C HG Solar heat gain at 5 angles (for documentation purposes only).
C UVAL optical property U value (for documentation purposes only).
C VTRN Visible transmittance
C AB layer absorb at 5 angels.
C RF layer refractive index
C SRF solar refl (for documentation purposes only)
C SAB soalr abs  (for documentation purposes only)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB

C NOPT number of optical database items.
C GS (52 char) list of optical database items to select from.
      COMMON/GPICK/GS(MGOPT),nopt

      LOGICAL FOUND
      CHARACTER GS*52,WORD*20,outs*124,OUTSTR*124,GTYPE*12
      character lworking*144,fs*1
      integer lndbp   ! for length of standard database path
      logical unixok  ! to check for database path file separators

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Open the optical database, if scanning all items reset the
C counters for the number of single and double items before scan.
      IER=0
      if(SOPT(1:3).EQ.'ALL')then
        nopt=0

C Reporting.
        if(ITRC.GE.1)then
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,
     &      ' Optical properties for transparent constructions.')
        endif
      endif
      FOUND=.FALSE.
      GTYPE=' '
      GDESCR=' '

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use efopseq. If in
C the standard location then create lworking which has path
C prepended.
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        CALL EFOPSEQ(IOPTDB,LOPTDB,1,IER)
      elseif(ipathoptdb.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    loptdb(1:lnblnk(loptdb))

C Debug.
C        write(6,*) 'working optics ',lworking

        CALL EFOPSEQ(IOPTDB,lworking,1,IER)
      endif
      IF(IER.NE.0)THEN
        IER=1
        CALL EDISP(IUOUT,'Optical properties could not be scanned.')
        RETURN
      ENDIF
      if(ipathoptdb.eq.0.or.ipathoptdb.eq.1)then
        write(currentfile,'(a)') LOPTDB(1:lnblnk(LOPTDB))
      elseif(ipathoptdb.eq.2)then
        write(currentfile,'(a)') lworking(1:lnblnk(lworking))
      endif

C Read lines from file, discarding comments. If SOPT = 'ALL' then put
C in scan mode.  
   7  CALL STRIPC(IOPTDB,OUTSTR,0,ND,0,'optical db',IER)
      IF(IER.EQ.2)GOTO 72
      IF(IER.EQ.1)RETURN
      GTYPE=OUTSTR(1:12)
      GDESCR=OUTSTR(16:51)

C Signal if it matches a specific optical set requested.
      IF(GTYPE.EQ.SOPT)FOUND=.TRUE.

C If in scan mode, generate 2 sets of strings describing current con-
C tents of the optical database.
      IF(SOPT(1:3).EQ.'ALL')THEN
        if(nopt+1.LE.MGOPT)then
          nopt=nopt+1
          WRITE(GS(nopt),'(A12,3X,A36)')GTYPE,GDESCR
        else
          call usrmsg('Optics database can hold no more data sets.',
     &      'Please revise the database.','W')
          return
        endif
      ENDIF

C Check the next line for general properties as well as flag for TMC data.
      CALL STRIPC(IOPTDB,OUTSTR,0,ND,1,'general',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NDL,1,3,'W','no def layers',IER)
      CALL EGETWI(OUTSTR,K,NTL,1,ME,'F','no TMC layers',IER)
      CALL EGETWR(OUTSTR,K,VTRN,0.,0.999,'W','visib trn',IER)
      CALL EGETWR(OUTSTR,K,SRF,0.,0.999,'W','solar refl',IER)
      CALL EGETWR(OUTSTR,K,SAB,0.,0.999,'W','solar abs',IER)
      CALL EGETWR(OUTSTR,K,UVAL,-10.,9.99,'W','U value',IER)
      IF(IER.NE.0)GOTO 99

C Reporting.
      IF(ITRC.GE.1)THEN
        IF(FOUND.OR.SOPT(1:3).EQ.'ALL')THEN
          CALL EDISP(ITOPT,' ')
          WRITE(OUTSTR,22)GDESCR(1:LNBLNK(GDESCR)),GTYPE
   22     FORMAT(' ',A,': with id of: ',A12)
          CALL EDISP(ITOPT,OUTSTR)
          WRITE(OUTSTR,23)NTL,VTRN
   23     FORMAT(' with ',I1,
     &      ' layers [including air gaps] and visible trn: ',F4.2)
          CALL EDISP(ITOPT,OUTSTR)
        ENDIF
      ENDIF

C Read direct transmission and overall heat gain factor (the latter for
C documentation purposes).
      CALL STRIPC(IOPTDB,OUTSTR,0,ND,1,'general optic data',IER)
      IF(IER.NE.0)GOTO 73
      K=0
      CALL EGETWR(OUTSTR,K,DG(1),0.,0.999,'W','dir t @ 0',IER)
      CALL EGETWR(OUTSTR,K,DG(2),0.,0.999,'W','dir t @ 40',IER)
      CALL EGETWR(OUTSTR,K,DG(3),0.,0.999,'W','dir t @ 55',IER)
      CALL EGETWR(OUTSTR,K,DG(4),0.,0.999,'W','dir t @ 70',IER)
      CALL EGETWR(OUTSTR,K,DG(5),0.,0.999,'W','dir t @ 80',IER)
      CALL EGETWR(OUTSTR,K,HG(1),0.,0.999,'W','ht gn @ 0',IER)
      CALL EGETWR(OUTSTR,K,HG(2),0.,0.999,'W','ht gn @ 40',IER)
      CALL EGETWR(OUTSTR,K,HG(3),0.,0.999,'W','ht gn @ 55',IER)
      CALL EGETWR(OUTSTR,K,HG(4),0.,0.999,'W','ht gn @ 70',IER)
      CALL EGETWR(OUTSTR,K,HG(5),0.,0.999,'W','ht gn @ 80',IER)
      IF(IER.NE.0)GOTO 99

      IF(ITRC.GE.1)THEN
        IF(FOUND.OR.SOPT(1:3).EQ.'ALL')THEN
          CALL EDISP(ITOPT,
     &  ' Direct transmission @ 0, 40, 55, 70, 80 deg')
          WRITE(OUTSTR,'(2x,5F6.3)')(DG(I),I=1,5)
          CALL EDISP(ITOPT,OUTSTR)
          CALL EDISP(ITOPT,' Layer| absorption @ 0, 40, 55, 70, 80 deg')
        ENDIF
      ENDIF

C For each layer extract tmc info.
      DO 44 IL=1,NTL
        CALL STRIPC(IOPTDB,OUTSTR,99,ND,1,'general',IER)
        IF(IER.EQ.2)GOTO 72
        IF(IER.NE.0)GOTO 73
        K=0
        if(ND.eq.8)then

C If an old format database skip the first two fields.
          CALL EGETW(outstr,K,WORD,'W','old db fld',IER)
          CALL EGETW(outstr,K,WORD,'W','old db fld',IER)
        endif
        CALL EGETWR(OUTSTR,K,VAL,1.,2.99,'W','refrac',IER)
        RF(IL)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','abs @ 0',IER)
        AB(IL,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','abs @40',IER)
        AB(IL,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','abs @55',IER)
        AB(IL,3)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','abs @70',IER)
        AB(IL,4)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','abs @80',IER)
        AB(IL,5)=VAL
        IF(IER.NE.0)GOTO 99

C Reporting.
        IF(ITRC.GE.1)THEN
          IF(FOUND.OR.SOPT(1:3).EQ.'ALL')THEN
            WRITE(OUTSTR,26)IL,(AB(IL,J),J=1,5)
   26       FORMAT(I5,1X,5F6.3)
            CALL EDISP(ITOPT,OUTSTR)
          ENDIF
        ENDIF
  44  CONTINUE

C One item read, if 'ALL' then loop back, otherwise if a match exit.
      if(SOPT(1:3).EQ.'ALL')then
        GOTO 7
      elseif(SOPT(1:3).EQ.'TMP')then
      
C Write out item GTYPE to copy of db. 
        write(ITOPT,'(a)')
     &    '# 12 char id |  description       | thick | blind'
        write(ITOPT,'(a12,a,a)')GTYPE,'  :',GDESCR(1:lnblnk(GDESCR))
        write(ITOPT,'(a)')
     &    '# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val'
        write(ITOPT,'(a,i3,4f7.3)')'  1 ',NTL,VTRN,SRF,SAB,UVAL
        write(ITOPT,'(a)')
     &    '# direct trn @ 5 angles, total heat gain @ 5 angles'
        write(ITOPT,'(1x,10F6.3)')DG(1),DG(2),DG(3),DG(4),DG(5),HG(1),
     &    HG(2),HG(3),HG(4),HG(5)

C For each layer extract tmc info.
        write(ITOPT,'(a)')
     &    '# refr index, absorption @ 5 angles for each tmc layer'
        DO 45 IL=1,NTL
          write(ITOPT,'(1x,6F6.3)')RF(IL),AB(IL,1),AB(IL,2),AB(IL,3),
     &      AB(IL,4),AB(IL,5)
  45    CONTINUE
        GOTO 7
      ELSE
        IF(FOUND)THEN
          CALL ERPFREE(IOPTDB,ISTAT)
          IER=0
          RETURN
        ELSE
          GOTO 7
        ENDIF
      ENDIF

C End of file reached.
  72  IF(.NOT.FOUND)then
        if(SOPT.NE.'ALL'.and.SOPT.NE.'TMP')THEN
         write(outs,'(3a)') 'Can`t find ',SOPT,' glazing.'
         CALL USRMSG(outs,' Please try another one.','-')
         CALL ERPFREE(IOPTDB,ISTAT)
         IER=2
         RETURN
        else
         ier=0
        endif
      ELSE
        IER=0
      ENDIF

  73  CALL ERPFREE(IOPTDB,ISTAT)
      RETURN

C Error conditions.
  99  CALL USRMSG(' Problem with optical db near...',OUTSTR,'W')
      IER=1
      GOTO 73

      END

C ************* EDWINO 
C EDWINO Allow user to select a glazing type by name for inclusion
C in the geometry file attributes. SOPT is a 12 Char identity string
C for the optical properties choice. Displays choices held in common
C GPICK which was derived from a call to ___.
      SUBROUTINE EDWINO(SOPT,IER)
#include "building.h"
      common/pophelp/h(60)
      COMMON/GPICK/GS(MGOPT),nopt
      CHARACTER GS*52,H*72,SOPT*12
      DIMENSION IGVAL(60)

      IER=0
      noptz=nopt

C Insolation, present a list of current default insolation choices.
      H(1)='The following selections are based on data supplied '
      H(2)='by the Environmental Advisory Service of Pilkington '
      H(3)='Flat Glass Limited.  Other suppliers can be included'
      H(4)='by request.'
      H(5)='Selections with an internal blind assume it is at 45'
      H(6)='degrees, reflectance of 50%, absorptance 39% and'
      H(7)='transmittance 11%.'

      IX=1
      CALL EPICKS(IX,IGVAL,' ',' Glazing types: ',
     &            52,noptz,GS,'raw opticals',IER,7)
      IF(IX.EQ.0)THEN
        SOPT='UNKNOWN'
      ELSEIF(IX.EQ.1)THEN

C Strip out the initial part of the string.
        IF(IGVAL(1).NE.0)THEN
          WRITE(SOPT,'(A)')GS(IGVAL(1))(1:12)
        ELSE
          SOPT='UNKNOWN'
        ENDIF
      ENDIF

      RETURN
      END


C **** version of opendb for ESP-r modules that can assume consistent model ******

C module_opendb: open materials constructions, multi-layer constructions and
C optical properties databases. In the case of materials, first
C assume it is a binary file, check its contents and if a problem
C then scan new ascii format and if that does not work try the
C older ascii materials file to fill the materials data arrays.
C If sucessful the material common blocks will be filled and
C closemat1 or closemat2 will be set.

      subroutine module_opendb(ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      logical closemat1,closemat2
      character SOPT*12,outs248*248,GDESCR*36
      character lworking*144,fs*1   ! for use with expanded paths
      integer lndbp   ! for length of standard database path
      logical unixok

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Scan the materials (either binary or ascii) file data into materials
C commons and if this was sucessful and matver was set to 1.2 then
C we can carry on using the materials common blocks for subsequent access.
      call scananymat(ier)
      call eclose(matver,1.1,0.001,closemat1)
      call eclose(matver,1.2,0.001,closemat2)
        
C Read multilayer db information into common after checking
C that the file exists.
      CALL ERPFREE(IFMUL,ISTAT)
      if(ipathmul.eq.0.or.ipathmul.eq.1)then
        lworking=lfmul  ! use as is
      elseif(ipathmul.eq.2)then
        lndbp=lnblnk(standarddbpath)
        write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &    lfmul(1:lnblnk(lfmul))  ! prepend db folder path
      endif
      call FINDFIL(lworking,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.eq.1)then
          write(outs248,'(3a)') 
     &    ' Problems with materials used by Constructions db',
     &     LFMAT(1:lnblnk(LFMAT)),'!'
          call edisp248(iuout,outs248,100)
          call edisp(iuout,' ')
          MLDBOK=.FALSE.
        ELSEIF(IER.eq.2)then

          call edisp(iuout,
     &      'There were many undefined materials in the constructions')
          call edisp(iuout,'please check your model.')
        else

C Scan was ok so set mldbok to true.
          MLDBOK=.TRUE.
        endif
      else

C Could not find Constructions db at this time, report to user.
        write(outs248,'(3a)') 'Constructions db ',
     &     LFMUL(1:lnblnk(LFMUL)),' not found!'
        call edisp248(iuout,outs248,100)
        call edisp(iuout,' ')
        MLDBOK=.FALSE.
      endif

C Open Ooptical Properties db and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg('Optical Properties db not found or',
     &              'there was a problem reading it!','W')
        OPTKOK=.FALSE.
      else
        OPTKOK=.TRUE.
      endif

      return
      end
      
C ******************* ADMIT *******************
C Calculate U and Y values and surface factor.
C If a negative conductivity is supplied the abs value represents the 
C resistance of an air gap.
C << NOTE: this routine gets close for some constructions, but
C << some variables are off - further debugging required.
      subroutine ADMIT(ITRU,IFOC,Rsi,Rso)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"

      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2
      
      dimension thick(ME),rho(ME),Cp(ME),cond(ME)
      character outs*124
      
      COMPLEX P,L(2,2),M(2,2),N(2,2)

C Establish if material data arrays have been filled. If not return.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so admit-',
     &    'tance of construction attributes cannot be calculated.','W')
        return
      endif

C ESP-r defines layers from outside to inside. The code in
C subroutine admit works from inside to outside.
C Debug layer info.
C      write (ITRU,*) 'Layer  thick  k  rho  Cp'

      layer=LAYERS(IFOC)+1
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      do 5 IL=1,LAYERS(IFOC)
        matarrayindex=IPRMAT(IFOC,IL)   ! which array index
        if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht.
          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &       matopaq(matarrayindex)(1:1).eq.'h'.or.
     &       matarrayindex.eq.0)then
            DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
          else
            DBCON=matdbcon(matarrayindex)
            DBDEN=matdbden(matarrayindex)
            DBSHT=matdbsht(matarrayindex)
          endif
        endif
        layer=layer-1
        thick(layer)=DTHK(IFOC,IL)

C If an air gap set cond(IL) to negative of air gap res.
        if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &     matopaq(matarrayindex)(1:1).eq.'h'.or.
     &     matarrayindex.eq.0)then
          cond(layer) = (-1.0) * DRAIR(IFOC,IL,1)
          rho(layer)=1.0
          Cp(layer)=1.0
        else
          cond(layer)=DBCON
          rho(layer)=DBDEN
          Cp(layer)=DBSHT
        endif

C Debug
C        write (ITRU,'(2i3,2f6.3,2f6.0)') layer,
C     &    thick(layer),cond(layer),rho(layer),Cp(layer)
 5    continue
      
C Start calculation of U-value.
      Uvalue=Rsi
      
C Define inside surface resistance matrix.
      L(1,1)=CMPLX(1.,0.)
      L(1,2)=CMPLX(Rsi,0.)
      L(2,1)=CMPLX(0.,0.)
      L(2,2)=CMPLX(1.,0.)
      
C Loop through layers (inside to outside).
      do 10 I=1,LAYERS(IFOC)
        if (cond(I).gt.0.0) then
          Uvalue=Uvalue+(thick(I)/cond(I))

C Define terms for arrays.
          xP=((4.*ATAN(1.))*thick(I)*thick(I)*rho(I)*Cp(I))/
     &     (86400.0*cond(I))
          xP=SQRT(xP)
        
C Calculate cosh(P).
          xcreal=((exp(xP)+exp(-xP))*cos(xP))/2.
          xcimage=((exp(xP)-exp(-xP))*sin(xP))/2.

C and sinh(P).
          xsreal=((exp(xP)-exp(-xP))*cos(xP))/2.
          xsimage=((exp(xP)+exp(-xP))*sin(xP))/2.
        
C Enter matrix coefficients
          M(1,1)=CMPLX(xcreal,xcimage)
          M(1,2)=(thick(I))*CMPLX(xsreal,xsimage)/
     &         (cond(I)*CMPLX(xP,xP))
          M(2,1)=(cond(I)*CMPLX(xP,xP))*CMPLX(xsreal,xsimage)/
     &         (thick(I))
          M(2,2)=CMPLX(xcreal,xcimage)
        else
        
C Air gap
          Uvalue=Uvalue+abs(cond(I))
          M(1,1)=CMPLX(1.,0.)
          M(1,2)=CMPLX(abs(cond(I)),0.)
          M(2,1)=CMPLX(0.,0.)
          M(2,2)=CMPLX(1.,0.)
        endif

C Calculate L*M, result in N.
        call CMATMUL(L,M,N)
        
C Copy N to L.
        L(1,1)=N(1,1)
        L(1,2)=N(1,2)
        L(2,1)=N(2,1)
        L(2,2)=N(2,2)
 10   continue
      Uvalue=Uvalue+Rso
      Uvalue=1./Uvalue
 
      call edisp(itru,'  ')
      write (outs,'(3(a,f6.2))') 'Admittance calculations using Rsi ',
     &  rsi,' Rso ',rso,' & Uvalue=',Uvalue
      call edisp(itru,outs)
      
C Outside face resistance.
      M(1,1)=CMPLX(1.,0.)
      M(1,2)=CMPLX(Rso,0.)
      M(2,1)=CMPLX(0.,0.)
      M(2,2)=CMPLX(1.,0.)
      call CMATMUL(L,M,N)
            
C Calculate admittance.
      P=N(2,2)/N(1,2)
      Y=abs(P)
      omega=(12.0/(4.0*ATAN(1.)))*atan(aimag(P)/real(P))
      
C Calculate decrement factor.
      P=1./(Uvalue*N(1,2))
      f=abs(P)
      phi=(12.0/(4.0*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0.0) then
        phi=12.0-phi
      else
        phi=-phi
      endif
      
C Calculate surface factor.
      P=1.-(Rsi*N(2,2)/N(1,2))
      f=abs(P)
      phi=(12.0/(4.0*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0.0) then
        phi=12.0-phi
      else
        phi=-phi
      endif

C Report values for exterior.
      write (outs,'(6(a,f6.2))') ' External surface admittance Y=',Y,
     &  '  w=',omega,' decrement factor f=',f,'  phi=',phi,
     &  ' surface factor f=',f,'  phi=',phi
      call edisp(itru,outs)

C Internal surfaces.
      P=(N(2,2)-1.)/N(1,2)
      Y=abs(P)
      omega=(12.0/(4.0*ATAN(1.)))*atan(aimag(P)/real(P))

C Calculate surface factor.
      P=1.-(Rsi*P)
      f=abs(P)
      phi=(12.0/(4.0*ATAN(1.)))*atan(aimag(P)/real(P))
      if (phi.gt.0.0) then
        phi=12.0-phi
      else
        phi=-phi
      endif

C Report values for partitions.
      write (outs,'(4(a,f6.2))') ' Partition admittance Y=',Y,
     &  '  w=',omega,' surface factor f=',f,'  phi=',phi
      call edisp(itru,outs)
      return
      end

C ******************** CMATMUL 
C CMATMUL multiplies the complex (2x2) matrices A by B returning C.
      SUBROUTINE CMATMUL(A,B,C)

      COMPLEX A(2,2),B(2,2),C(2,2)

C Calculate A*B.
      DO 10 I=1,2
        C(I,1)=A(I,1)*B(1,1)+A(I,2)*B(2,1)
        C(I,2)=A(I,1)*B(1,2)+A(I,2)*B(2,2)
   10 CONTINUE

      return
      end
      


