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
C  ETMLDB:  Provides reporting on current description of a composite.
C  ETMLDBU returns U value of construction based on contents of common MLC.
C  EPKMLC:  Select a composite construction with display of details.
C  ERPCDB:  Reads one element from a binary materials db.
C  EMKAMLD: Creates an ascii composite constr db based on common MLC.
C  EROPTDB: Returns optical properties for TMC.
C  EDWINO:  Allow user to select a glazing type by name.
C  module_opendb: open materials constructions, multi-layer constructions and
C           optical properties databases (for utility applications).

C ************* ERMLDB
C Given file name and unit number, read  an ASCII composite 
C construction database and return data via common block MLC.
C IFMUL is the database unit, ITRU unit number for user output,
C IER=0 OK, IER=1 problem. 
C DRAIR is the air gap layer resistance for 3 orientations.

      SUBROUTINE ERMLDB(ITRC,ITRU,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,layermatch

      logical closemat1,closemat2,close4
      logical unixok

      CHARACTER OUTSTR*124,NAM*72,H*72,outs*124,outs248*248
      CHARACTER RSTR*124
      CHARACTER WORD*20,MLCN*12,OTF*4,OPT*12,MLCSYM*12
      character lworking*144,fs*1

      integer ier

C Note that the text which follows the thickness of the layer
C is extracted from the materials database and should not
C be edited by the user as it is scanned to check the specific
C named material matches.

C Keep track of how many times materials db read fails.
      matmiss=0
      closemat1=.false.  ! assume binary materils until proven otherwise
      closemat2=.false.

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

C Check the value of whichdbpath variable to see what to do with
C this file. If local or absolute path then use efopseq. If in
C the standard location then create lworking which has path
C prepended.
        if(ipathmat.eq.0.or.ipathmat.eq.1)then
          ier=0
        call EFOPRAN(IFMAT,LFMAT,40,1,IER)
          if(ier.ne.0)then
            write(outs248,'(3a)') ' No materials db ',
     &        LFMAT(1:lnblnk(LFMAT)),' found!'
            call edisp248(iuout,outs248,100)
            MATDBOK=.FALSE.
            ier=2    ! set error state so calling code will look for ascii version.
            return
          endif
        elseif(ipathmat.eq.2)then
          lndbp=lnblnk(standarddbpath)
          write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      LFMAT(1:lnblnk(LFMAT))

C Debug.
C          write(6,*) 'working materials ',lworking
          ier=0
          call EFOPRAN(IFMAT,lworking,40,1,IER)
          if(ier.ne.0)then
          write(outs,'(3a)') ' No materials db ',
     &        lworking(1:lnblnk(lworking)),' found!'
          call edisp(iuout,outs)
          MATDBOK=.FALSE.
          ier=2    ! set error state so calling code will look for ascii version.
          return
          endif
        endif
        IF(ier.eq.0)THEN

C Also check to see if the file has correct info in 
          IREC=601
          READ(IFMAT,REC=IREC,IOSTAT=IOS,ERR=1000)NCLASS
          if(NCLASS.LE.0)then
            goto 1000   ! no classifications so try alternatives
          else
            MATDBOK=.TRUE.
            matver = 0.0
            goto 139  !  a binary file found so carry on.
          endif
 1000     if(IOS.eq.2)then
            call edisp(iuout, 
     &        'ERMLDB: permission issue reading material classes.')
            CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
          else
            CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
          endif
          call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
          if(ier.eq.-2)then
            ier=0
            call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
            if(ier.ne.0)then
              call usrmsg('No readable materials database was found so',
     &                    'constructions database not reported.','W')
              MATDBOK=.FALSE.
              IER=1
              return
            else
              call eclose(matver,1.1,0.001,closemat1)
              call eclose(matver,1.2,0.001,closemat2)
              if(closemat1.or.closemat2)then
                call edisp(iuout,' Filled material data arrays...')
                MATDBOK=.TRUE.
              else
                call usrmsg(
     &            'No readable material database was found. ',
     &            'so constructions database not reported.','W')
                MATDBOK=.FALSE.
                IER=1
                return
              endif
            endif
          endif
        endif
      ENDIF

C Find version of materials database, if neither closemat1 or closemat2
C is true then the material arrays could be problematic.
 139  call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        CALL USRMSG('Materials database data probably corrupt.',
     &    'Please fix this first.','W')
        ier=2
        return
      endif

C Free unit and check if composite file exists.  Setup lworking for the
C MLC database expanded name to be used later.
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

C Read the number of items in the multi-layer database, followed by
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

      DO 10 I = 1,NMLC
        CALL STRIPC(IFMUL,OUTSTR,0,ND,1,'mlc descr',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,LAYERS(I),1,MMLC,'F','layrs',IER)

C Anything after no of layers is the description (which is overloaded
C to include information on optical properties and either the name
C of a MLC with inverted layers or the keywords SYMMETRIC or NONSYMMERTIC).
        CALL EGETRM(OUTSTR,K,RSTR,'W','layer description',IER)
        DESC(I)=RSTR(1:LNBLNK(RSTR))
        IF(IER.NE.0)GOTO 99

C Parse the DESC string into string tokens.
        call parsemlcdesc(DESC(I),MLCN,OTF,OPT,MLCSYM)

        IF(LAYERS(I).LE.0)GOTO 10

        DO 15, IL=1,LAYERS(I)
          CALL STRIPC(IFMUL,OUTSTR,99,ND,1,'layer',IER)
          IF(IER.NE.0)RETURN
          K=0

C If the model is associated with a version 1.1 materials database
C some newer entities in the GAPS category may have an index of
C -99 which requires further logic in order to locate the relevant
C material properties.
          CALL EGETWI(OUTSTR,K,IPR(I,IL),-99,600,'F',
     &        'materials db index',IER)
          CALL EGETWR(OUTSTR,K,DTHK(I,IL),0.0001,0.5,'W',
     &        'layer thickness',IER)

C Current MLDB also include the name of the layer from the materials
C db. If the layer is air then this name contains the R values for
C air gap. In some cases the initial portion of NAM will be the
C same as matname and this needs to be decoded if IPR is -99.
C To simplify later tasks remember RSTR as LAYDESC. Be sure
C to write less than 72 char into NAM
          IF(ND.GT.2)THEN
            CALL EGETRM(OUTSTR,K,RSTR,'W','layer name:gap R',IER)
            lex=MIN0(LNBLNK(RSTR),72)
	    write(NAM,'(a)')RSTR(1:lex)
            lex=MIN0(LNBLNK(RSTR),124)
	    write(LAYDESC(I,IL),'(a)')RSTR(1:lex)

C See if there is a matching matarray index, if so assign values
C to the IPRMAT array so for each IPR there is a IPRMAT. Note:
C NOTE: in case there are multiple items with the same characters
C in the name also check to see if the legacy index associated
C with the found item matches the IPR scanned above.
              ileghope=IPR(I,IL)   ! what we hope we will find a match for
              call parsematdesc(ileghope,NAM,matarrayindex)
              if(matarrayindex.ne.0)then
                if(matlegindex(matarrayindex).ne.IPR(I,IL))then
                call edisp(iuout,' --- WARNING --- ')
                  write(outs,'(a,i3,a,i3,2a)') 
     &              'Duplicate material names ',
     &              matlegindex(matarrayindex),' index does not match ',
     &              IPR(I,IL),' name: ',NAM(1:lnblnk(NAM))
                  call edisp(iuout,outs)
                  write(outs,*) 'as referenced in ',MLCN(1:lnblnk(MLCN))
                  call edisp(iuout,outs)
                  write(outs,'(2a)') 
     &            'Marking the confused layer with a -99. Check the ',
     &              'materials database for two identically named'
                  call edisp(iuout,outs)
                  write(outs,'(2a)') 
     &            'materials, edit to make names unique, save and ',
     &              'then rescan databases and re-make zone files.'
                  call edisp(iuout,outs)
                write(outs,'(2a)') 
     &            'Or use the interface to locate the named construc-',
     &            'tion and re-select the correct material.'
                call edisp(iuout,outs)
                call edisp(iuout,' --- make a note of this NOW --- ')
                  IPRMAT(I,IL)=-99  ! could not find it
                else
                  IPRMAT(I,IL)=matarrayindex
                endif    
              else

C Did not find it, check if IPR(I,IL) is zero, if so set
C IPRMAT to zero to signal an air gap.  If not mark as -99
C so that other facilities know about the confusion.
                if(IPR(I,IL).eq.0)then
                  IPRMAT(I,IL)=0  ! to signal std air gap
                else
                  IPRMAT(I,IL)=-99  ! could not find it
                  write(outs,*)'as referenced in ',MLCN(1:lnblnk(MLCN))
                  call edisp(iuout,outs)
                endif
              endif

C For a standard air gap pick up 'AIR' followed by assumed R values.
            IF(IPR(I,IL).EQ.0)THEN
              K=0
              CALL EGETW(NAM,K,WORD,'W','AIR label',IER)
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','vert R',IER)
              DRAIR(I,IL,1)=VAL
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','floor:ceil R',IER)
              DRAIR(I,IL,2)=VAL
              CALL EGETWR(NAM,K,VAL,0.,99.0,'W','other R',IER)
              DRAIR(I,IL,3)=VAL
            ELSEIF(IPR(I,IL).EQ.-99)THEN

C For a layer with no legacy index use the matarray index and assign values.
                matarrayindex=IPRMAT(I,IL)
                if(matarrayindex.ne.0)then
                  DBCON=matdbcon(matarrayindex)
                  DBDEN=matdbden(matarrayindex)
                  DBSHT=matdbsht(matarrayindex)

C If g or h air gap and version 1.2 then assign DRAIR
	          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &               matopaq(matarrayindex)(1:1).eq.'h')then
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

C If g or h air gap and version 1.2 then assign DRAIR
	          if(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &               matopaq(matarrayindex)(1:1).eq.'h')then
                    if(closemat2)then
                      DRAIR(I,IL,1)=matgapares(matarrayindex,1)
                      DRAIR(I,IL,2)=matgapares(matarrayindex,2)
                      DRAIR(I,IL,3)=matgapares(matarrayindex,3)
                    endif
                  endif
                endif
            ENDIF
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
              WRITE(OUTSTR,117)IL,DESC(I)(1:LNBLNK(DESC(I))),
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
  15    CONTINUE

C If the symmetric tag has not yet been set check and see if the MLC
C is or is not symmetrical and set MLCSYM.
        if(MLCSYM(1:2).EQ.'  ')then
          call ismlcsymmetric(i,layermatch)
          if(layermatch)then
            MLCSYM='SYMMETRIC   '
          else
            MLCSYM='NONSYMMETRIC'
          endif
        elseif(MLCSYM(1:9).EQ.'SYMMETRIC')then
          continue
        elseif(MLCSYM(1:12).EQ.'NONSYMMETRIC')then
          continue
        endif
        WRITE(DESC(I),89)MLCN,OTF,OPT,MLCSYM
   89   FORMAT(A12,2X,A4,2X,A12,2x,A12)

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

C ******* parsemlcdesc
C Parse the DESC string from the MLC common block returning text for
C MLCN (its name) ,OTF (whether OPAQ/TRAN/CFC) ,OPT (optical name) ,MLCSYM
C tag for symmetry. Where values are missing or unknown setup defaults.
      subroutine parsemlcdesc(DES,MLCN,OTF,OPT,MLCSYM)
      
      integer lnblnk  ! function definition

      character DES*48,MLCN*12,OTF*4,OPT*12,MLCSYM*12

C Clear parameters to be returned and then copy from DES.
      OPT=' '
      MLCSYM=' '
      WRITE(MLCN,'(A)')DES(1:12)
      WRITE(OTF,'(A)') DES(15:18)
      IF(OTF(1:2).EQ.'  ')OTF='OPAQ'
      IF(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')THEN
        WRITE(OPT,'(A)') DES(21:32)
        IF(OPT.EQ.' ')OPT='UNKNOWN'
      ELSEIF(OTF(1:4).EQ.'CFC ')THEN
        OPT='USE GSLedit'
      ELSE
        OPT='OPAQUE'
      ENDIF

C Read tag for name of inverted version or symmetry if the length of the
C DESC string could hold the tag, otherwise set MLCSYM to a blank.
      if(lnblnk(DES).gt.35)then
        WRITE(MLCSYM,'(A)') DES(35:46)
      else
        MLCSYM='  '
      endif
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

C Find a matching matname, if the legacy index associated with this item
C matches ileghope then our search is complete.  If it does not match
C ileghope then there could be duplicate names to check further. If
C ileghope is -99 then return what was found as an index match not possible.
          do 43 imt=1,matdbitems
            if(testname(1:ltnam).eq.matname(imt)(1:ltnam))then
              if(matlegindex(imt).eq.ileghope)then
                matarrayindex=imt
                return
              else
                if(ileghope.eq.-99)then
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
          if(PNAM(1:lnam).eq.matname(imt)(1:lnam))then

C Debug.
C            write(6,*) imt,' name x: match ',PNAM(1:lnam),' ',
C     &        matname(imt)(1:lnblnk(matname(imt)))
            matarrayindex=imt
            return
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

C *************** ETMLDB
C ETMLDB provides reporting on current description of a composite
C construction based on contents of common MLC.
      SUBROUTINE ETMLDB(ITRC,ITRU,IFOC,ier)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
#include "geometry.h"
#include "CFC_common.h"

      integer lnblnk  ! function definition

c-- ag     
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      CHARACTER SNAME*12,SOTF*32,SMLCN*32,SVFC*4,SOTHER*24,SUSE*8
      CHARACTER SPARENT*12
	  
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
c-- ag end

      COMMON/GVALCAL/CLAMDA(MMLC)

      integer matarrayindex ! the indes within matdatarray
      integer lnam    ! length of material name
      integer lfordoc ! length available for material documentation
      logical closemat1,closemat2
      logical bCloseToZero,XST

      DIMENSION PNAM(ME)
      CHARACTER OUTSTR*124,NAM*72,PNAM*72
      CHARACTER MLCN*12,OTF*4,OPT*12,SOPT*12,GDESCR*36
      character MLCSYM*12

      real RSI  ! used in reporting of R value in SI units
      
C Pick up general description of the composite.
      ier=0
      call parsemlcdesc(DESC(IFOC),MLCN,OTF,OPT,MLCSYM)
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        IF(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')THEN
          WRITE(OUTSTR,'(5a)')' Details of transparent construction: ',
     &      MLCN,' with ',OPT,' optics.'
          CALL EDISP(ITRU,OUTSTR)
        ELSEIF(OTF(1:4).EQ.'CFC ')THEN
          WRITE(OUTSTR,'(3a)')' ',
     &      'Details of complex fenestration construction: ',MLCN
          CALL EDISP(ITRU,OUTSTR)
        ELSE
          WRITE(OUTSTR,'(2a)')' Details of opaque construction: ',MLCN
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

C Reporting based on report level.
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' ')
        write(OUTSTR,'(2a)')
     &' Layer|Matr|Thick |Conduc-|Density|Specif|IR  |Solr|Diffu|',
     &' R    |Descr'
        CALL EDISP(ITRU,OUTSTR)
        write(OUTSTR,'(2a)')
     &'      |db  |(mm)  |tivity |       |heat  |emis|abs |resis|',
     &'m^2K/W'
        CALL EDISP(ITRU,OUTSTR)
      ELSEIF(ITRC.EQ.1)THEN
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Layer|Matr|Thick | Description ')
        CALL EDISP(ITRU,'      |db  |(mm)  | of material ')
      ENDIF

C Loop through all of the layers and compose RT for the construction.
      RT=0.
      DO 15, IL=1,LAYERS(IFOC)

C If data available in material array gather it. And if an air
C layer include the R values in the display.
          matarrayindex=IPRMAT(IFOC,IL)   ! which array index
          if(matarrayindex.ge.0)then
 
C And if matarrayindex is zero then reset dbcon dbden dbsht.
            if(matarrayindex.eq.0)then
              DBCON=0.0; DBDEN=0.0; DBSHT=0.0 
              E=0.99; A=0.99; DRV=1.0
            NAM='AIR'
            else
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)

C Make up NAM as combination of matnam and matdoc.
              lnam=lnblnk(matname(matarrayindex))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(NAM,'(3a)') matname(matarrayindex)(1:lnam),' : ',
     &          matdoc(matarrayindex)(1:lfordoc)
            endif

C Also check if it is a gap. If version 1.2 materials then we will
C have access to pre-defined air gap resistances.
            if(matarrayindex.eq.0)then
              continue
            elseif(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &             matopaq(matarrayindex)(1:1).eq.'h')then
              if(closemat2)then
                DRAIR(IFOC,IL,1)=matgapares(matarrayindex,1)
                DRAIR(IFOC,IL,2)=matgapares(matarrayindex,2)
                DRAIR(IFOC,IL,3)=matgapares(matarrayindex,3)
                RT=RT+DRAIR(IFOC,IL,1)  ! while we know
                RSI=DRAIR(IFOC,IL,1)
              endif
            endif
          else

C Referenced item was not in the materials hash list. Write zeros.
            DBCON=0.0
            DBDEN=0.0
            DBSHT=0.0
            E=0.0
            A=0.0
            DRV=0.0
            write(NAM,'(a)') 'unknown : unknown '
          endif

C << this logic could be updated to use something other than IPR >>
        IF(IPR(IFOC,IL).EQ.0)THEN
          WRITE(PNAM(IL),'(A,3F5.2)')'air ',(DRAIR(IFOC,IL,IY),IY=1,3)
          RT=RT+DRAIR(IFOC,IL,1)
          RSI=DRAIR(IFOC,IL,1)
        ELSEIF(IPR(IFOC,IL).EQ.-99)THEN

C RT and RSI Values should have been established above.
          WRITE(PNAM(IL),'(A)') NAM(1:72)
        ELSE

C Also report on thermal resistance (m^2K/W) of the layer. If dbcon
C is zero then avoid divide by zero.
          call eclose ( dbcon, 0.0, 1E-06, bCloseToZero)
          if(.not. bCloseToZero)then
            RSI=DTHK(IFOC,IL)/DBCON
          else
            RSI=1.0
          endif
          PNAM(IL)=NAM
          if(.not. bCloseToZero)then
            RT=RT+DTHK(IFOC,IL)/DBCON
          else
            RT=1.0
          endif
        ENDIF

C Reporting.
        IF(ITRC.GT.1)THEN
          if(LAYERS(IFOC).eq.1)then 
            WRITE(OUTSTR,24)IL,IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:44)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.1)then 
            WRITE(OUTSTR,25)' Ext ',IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:44)
          elseif(LAYERS(IFOC).ge.2.and.IL.eq.LAYERS(IFOC))then 
            WRITE(OUTSTR,25)' Int ',IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:44)
          else 
            WRITE(OUTSTR,24)IL,IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &              DBCON,DBDEN,DBSHT,E,A,DRV,RSI,PNAM(IL)(1:44)
          endif
  24      FORMAT(I5,I5,F7.1,F10.3,F7.0,F7.0,F5.2,F5.2,F7.0,F6.2,1X,A)
  25      FORMAT(A5,I5,F7.1,F10.3,F7.0,F7.0,F5.2,F5.2,F7.0,F6.2,1X,A)
          CALL EDISP(ITRU,OUTSTR)
        ELSEIF(ITRC.EQ.1)THEN

C Compact report has more room for material description.
          WRITE(OUTSTR,29)IL,IPR(IFOC,IL),DTHK(IFOC,IL)*1000.0,
     &      PNAM(IL)(1:lnblnk(PNAM(IL)))
  29      FORMAT(I5,I5,F8.1,1X,a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF
  15  CONTINUE

C ISO 6946 1/hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
C Turn this off for CFC, as it is wrong for CFC
      if(OTF(1:4).NE.'CFC ')then
        CLAMDA(IFOC)=1./RT
        RTH=RT+0.04+0.13
        UVALUEH=1.0/RTH
        RTU=RT+0.04+0.10
        UVALUEU=1.0/RTU
        RTD=RT+0.04+0.17
        UVALUED=1.0/RTD
        RTI=RT+0.13+0.13
        UVALUEI=1.0/RTI
        write(OUTSTR,'(A,3F7.3,a,F7.3)')
     &    ' ISO 6946 U values (horiz/upward/downward heat flow)=',
     &    UVALUEH,UVALUEU,UVALUED,' (partition)',UVALUEI
        IF(ITRC.NE.0)call edisp(itru,OUTSTR)
      endif

C Include admittance calculations (use TM33 Table 4.5 values for admittance calcs.
C << NOTE this code needs further debugging.
C      Rsi=0.12
C      Rso=0.06
C      call ADMIT(ITRC,ITRU,IFOC,Rsi,Rso)

C Also list optical properties for this construction (if it is
C transparent and the optical property is something other than
C QPAQ.
      if(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')then
        if(OPT(1:4).ne.'OPAQ'.and.OPT(1:4).ne.'UNKN')then
          SOPT=OPT
          IF(ITRC.NE.0)CALL EROPTDB(1,itru,SOPT,GDESCR,IER)
        endif
      elseif (OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).EQ.'CFC ') then
C << include output of optical CFC data here (?) >>
C We will need to loop through all zones, probably, as a MLC of type CFC
C can be used for different actual glazing properties on a zone for zone
C base (?)
      do 16, iZone=1,ncomp
        if (icfc(iZone).eq.1) then
C Read construction file
          call read_in_cfc_file(0,itru,IFIL+1,lcfcin(iZone),iZone,ier)
          do 17, iSurf=1,nzsur(iZone)
            if (icfcfl(iZone,iSurf).gt.0) then
              icfctp=icfcfl(iZone,iSurf)  ! cfc type ... should be correlated with current mlc
              LayNo=ncfc_el(iZone,icfctp) ! number of layers of current type
C             find out if current construction of mlc is that which current CFC is 
C             based on ...

c              if(NMLC.gt.0)then
c                ICF=-1
c                lstr=lnblnk(SMLCN(NS))
c                DO 20 IC=1,NMLC
c                  if(lstr.lt.12)then
c                    IF(DESC(IC)(1:lstr).EQ.SMLCN(NS)(1:lstr))ICF=IC
c                  else
c                    IF(DESC(IC)(1:12).EQ.SMLCN(NS)(1:12))ICF=IC
c                  endif

C           do 18, iAng=1,5
C             Calculate layer properties for 0°, 40°, 55°, 70° and 80° incidence angle
c             angi1=30
c             call cfc_eff_opt_prop(iZone,iSurface,icfctp,
c     &                               incang(iAng),
c     &                               angi1,
c     &                               proang_s(iZone,icfctp,iSurface))
C or total properties? Not quite clear where inc angle comes in, here ...
c             call solar_multilayer(icomp,isurf,icfctp,GBM,GDF,IBM,IDF,
c     &                 isolvis,TRANSBB_SYS,TRANSD_SYS,REFL_SYS,AbsSol)
C  18
C           Write for current zone / surface
            IF (ITRC.NE.0) then 
              write(OUTSTR,'(5a,F7.3)')
     &        'Zone <',zname(iZone)(1:lnzname(iZone)),
     &        '> Surface <',sname(iZone,iSurf),
     &        '> CFC Approx. ISO 6946 U-value (horiz)=',
     &        CFC_ISO_Uvalue(iZone,icfctp) ! this would be *very* nice ...
              call edisp(itru,OUTSTR)
C             Layer data
              write(outstr,'(a)') 
     &               'Optical layer properties '
		      CALL EDISP(ITRU,OUTSTR)
              write(outstr,'(2a)') 
     &               '  type  t_sol rf_sol rb_sol  t_vis rf_vis',
     &               ' rb_vis  eps_f  eps_b'
		      CALL EDISP(ITRU,OUTSTR)
C             Write out non-gas layer data (sequence order should be as in LBNL Window)
              do 19, iLay=1,LayNo
                if (icfcltp(iZone,icfctp,iLay).ne.0) then
                  write(outstr,'(2x,I4,1x,8(f6.3,1x))')
     &               icfcltp(iZone,icfctp,iLay),
     &               solT(iZone,icfctp,iLay),
     &               solRF(iZone,icfctp,iLay),
     &               solRB(iZone,icfctp,iLay),
     &               visT(iZone,icfctp,iLay),
     &               visRF(iZone,icfctp,iLay),
     &               visRB(iZone,icfctp,iLay),
     &               rlwEF(iZone,icfctp,iLay),
     &               rlwEB(iZone,icfctp,iLay)
                  CALL EDISP(ITRU,OUTSTR)
                endif ! not gas layer
  19          continue
              write(outstr,'(a)') ' '
              call edisp(itru,outstr)
            endif ! itrc.ne.0
          endif ! icfcfl.gt.0
  17    continue ! surface in zone loop

c        ENDIF ! lthrm(izone) exists
      endif ! icfc.gt.0

  16  continue ! zone loop

      endif ! end CFC output for QS report (?)

      RETURN
      END

C *************** ETMLDBU
C ETMLDBU return ISO 6946 U values (UVH/UVU/UVD/UVI) of current construction based
C on contents of common MLC. Logic is a variant of ETMLDB.
      SUBROUTINE ETMLDBU(ITRC,ITRU,IFOC,UVH,UVU,UVD,UVI)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Material properties.
      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2,bCloseToZero

      CHARACTER OUTSTR*124,NAM*72
      CHARACTER TITL*72,MLCN*12,OTF*4,OPT*12
      character MLCSYM*12

C Pick up general description of the composite.
      call parsemlcdesc(DESC(IFOC),MLCN,OTF,OPT,MLCSYM)

C Check version of material database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)

C Reset U value reporting local variables.
      RT=0.
      RTH=0.
      RTU=0.
      RTD=0.
      RTI=0.
      UVH=0.
      UVU=0.
      UVD=0.
      UVI=0.
      DO 15, IL=1,LAYERS(IFOC)

C If an air layer include the R values.
        if(closemat1.or.closemat2)then
          matarrayindex=IPRMAT(IFOC,IL)   ! which array index
          if(matarrayindex.ge.0)then

C And if matarrayindex is zero then resetn dbcon dbden dbsht.
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

C Make up NAM as combination of matnam and matdoc.
              lnam=lnblnk(matname(matarrayindex))
              lfordoc = 72 - (lnam +4)    ! space left for doc
              write(NAM,'(3a)') matname(matarrayindex)(1:lnam),' : ',
     &          matdoc(matarrayindex)(1:lfordoc)
            endif

C Also check if it is a gap.
            if(matarrayindex.eq.0)then
              continue
	    elseif(matopaq(matarrayindex)(1:1).eq.'g'.or.
     &             matopaq(matarrayindex)(1:1).eq.'h')then
              if(closemat2)then
                DRAIR(IFOC,IL,1)=matgapares(matarrayindex,1)
                DRAIR(IFOC,IL,2)=matgapares(matarrayindex,2)
                DRAIR(IFOC,IL,3)=matgapares(matarrayindex,3)
                RT=RT+DRAIR(IFOC,IL,1)  ! while we know
                RSI=DRAIR(IFOC,IL,1)
              endif
            endif
          endif
        else
          CALL ERPCDB(IFMAT,IPR(IFOC,IL),ITRU,IER,DBCON,DBDEN,
     &      DBSHT,E,A,DRV,TITL,NAM)
        endif

        IF(IER.EQ.1)RETURN
        IF(IPR(IFOC,IL).EQ.0)THEN
          RT=RT+DRAIR(IFOC,IL,1)
          RSI=DRAIR(IFOC,IL,1)
        ELSEIF(IPR(IFOC,IL).EQ.-99)THEN

C RT and RSI in above code.
          continue
        ELSE

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
        ENDIF

C Calculate and (optionally) report.
  15  CONTINUE

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow as well as
C an inside wall to inside wall hc 0.13 both sides.
      RTH=RT+0.04+0.13
      UVH=1.0/RTH
      RTU=RT+0.04+0.10
      UVU=1.0/RTU
      RTD=RT+0.04+0.17
      UVD=1.0/RTD
      RTI=RT+0.13+0.13
      UVI=1.0/RTI
      write(OUTSTR,'(A,4F7.3)')
     &  ' ISO 6946 U values (horiz/upward/downward/partition)= ',
     &  UVH,UVU,UVD,UVI
      IF(ITRC.NE.0)then
        CALL EDISP(ITRU,' ')
        IF(OTF(1:4).NE.'OPAQ'.AND.OTF(1:4).NE.'CFC ')THEN
          WRITE(OUTSTR,'(5a,F6.3,a,F6.3,a,f6.3,a,f6.3)')
     &    ' ISO 6946 U values of ',MLCN(1:lnblnk(MLCN)),' with ',OPT,
     &    ' optics are (horiz flow)',UVH,' (upwards flow)',UVU,
     &    ' (downwards flow)',UVD,' (partition) ',UVI
        ELSE
          WRITE(OUTSTR,'(3a,F6.3,a,f6.2,a,f6.2,a,f6.3)')
     &    ' ISO 6946 U values of ',MLCN(1:lnblnk(MLCN)),
     &    ' are (horiz flow)',UVH,' (upwards flow)',UVU,
     &    ' (downwards flow)',UVD,' partition ',UVI
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
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION IVAL(MMLC),COMPS(MMLC)
      CHARACTER*(*) PROMPT1,PROMPT2
      CHARACTER H*72,COMPS*33

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
      H(4)='Note that the trace level influences the amount of'
      H(5)='detail in this display and you may wish to reset the'
      H(6)='trace level to see additional information.'
      H(7)=' '
      H(8)='For Controls of bidirectional data only: '
      H(9)='Select the construction for which the controls and the '
      H(10)='switch between different datasets will be applied on.'
      H(11)='WARNING: Avoid assigning more than one control loop '
      H(12)='to the same construction, especially when these controls '
      H(13)='are attempting to assign different datasets. The first '          
      H(14)='loop will be used if this mistake is made by the users. '
      DO 66 IWW=1,NMLC
        WRITE(COMPS(IWW),'(A33)')DESC(IWW)(1:33)
   66 CONTINUE
      IX=1
      CALL EPICKS(IX,IVAL,PROMPT1,PROMPT2,
     &  33,NMLC,COMPS,'composites',IER,14)

      IF(IX.EQ.0)RETURN
      ISEL=IVAL(1)
      CALL ETMLDB(2,iuout,ISEL,imerr)
      call edisp(iuout,
     & ' ISO 6946 U value assumes: Rso=0.04 & Rsi=0.13 (m**2deg.C/W)')
      RETURN

      END

C *************** ERPCDB
C ERPCDB reads one element from a binary materials database
C returning them via parameter list.
C IFMAT is the unit number of the database,  IEL the requested 
C element, ITRU unit number for user output, IER=0 OK, IER=1 problem.
C ITRC is the trace level.  It is assumed that the database has been
C previously opened and other parts of the code will close it when
C appropriate.  E is the element emissivity, A is the solar absorb, DRV
C is the diffusion resistance, TITL is the classification title, PNAM is
C the material name.

C Note: this subroutine is for use with the binary materials file
C rather than the matdatarray common blocks.

      SUBROUTINE ERPCDB(IFMAT,IEL,ITRU,IER,DBCON,DBDEN,DBSHT,E,A,
     &                  DRV,TITL,PNAM)

      CHARACTER PNAM*72,TITL*72,outs*124
      logical close4

      IER=0
      close4=.false.

C The record in the material db is IEL + 1 UNLESS the
C reference is to material db 0 (air).
      PNAM=' '
      TITL=' '
      DBCON=0.0
      DBDEN=0.0
      DBSHT=0.0
      IF(IEL.NE.0)THEN
        IR=((IEL-1)/20)*20+1
        READ(IFMAT,REC=IR,IOSTAT=IOS,ERR=105)TITL
        IR = IEL + 1
        READ(IFMAT,REC=IR,IOSTAT=IOS,ERR=105)DBCON,DBDEN,DBSHT,E,A,
     &       DRV,PNAM

C If all values are still 0.0 then not an actual element.
        call eclose3(DBCON,DBDEN,DBSHT,0.0,0.0,0.0,0.001,CLOSE4)
        if(CLOSE4)then
          write(outs,'(A,I3,A,2i4)') ' Material db reference ',IEL,
     &                           ' has no data, or all zero...',IR,IFMAT
          call usrmsg(outs,' Please check your selection! ','W')
          ier=1
          return
        endif
      ELSE

C Air layer encountered so put in zeroes.
        E=0.99
        A=0.99
        DRV=1.0
        PNAM='AIR'
        TITL='GAPS'
      ENDIF

  99  RETURN
 105  if(IOS.eq.2)then
        write(outs,'(A,I3,A,i4,a,i3)') ' Materials db reference ',IEL,
     &    ' read error @ record ',IR,' on file unit ',IFMAT
      else
        write(outs,'(A,I3,A,i4,a,i3)') ' Materials db reference ',IEL,
     &    ' permission error @ record ',IR,' on file unit ',IFMAT
      endif
      call edisp(itru,outs)
      IER=1
      CALL ERPFREE(IFMAT,ISTAT)
      goto 99

      END

C *************** EMKAMLD 
C EMKAMLD: Creates a legacy ascii constructions database based on the
C current contents of common MLC.  In the case of an air layer sub-
C stitute the U value for each layer unless we are dealing with
C one of the [g] or [h] material types in which case the name
C is written out.

C This subroutine uses the material array data in matdatarray.
      SUBROUTINE EMKAMLD(ITRU,IER)
#include "building.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer itru    ! feedback or file unit
      integer ier     ! zero is ok, one mlc error, two mat array issue
     
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      integer lnam    ! length of material name
      integer lfordoc ! length available for material documentation
      logical closemat1,closemat2

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
      WRITE(IFMUL,30)LFMUL(1:lnblnk(LFMUL)),LFMAT(1:lnblnk(LFMAT))
  30  FORMAT('# composite construction db defined in ',A,/,
     &       '# based on materials db ',A)

C For each composite construction write the number of layers (LAYERS),
C its description (DESC) and then the materials db reference IPR,
C thickness DTHK and material name NAM.  
C In the case of db reference 0 (air) overwrite NAM with the 3 values
C of DRAIR.
      WRITE(IFMUL,'(I5,A)')NMLC,'     # no of composites '

      DO 110 I = 1,NMLC
        IF(LAYERS(I).EQ.0)GOTO 110
        DO 116, IL=1,LAYERS(I)
          IF(IPR(I,IL).EQ.0)THEN
            WRITE(PNAM(IL),'(A,1X,3F6.3)')'air',(DRAIR(I,IL,IY),IY=1,3)
          ELSEIF(IPR(I,IL).EQ.-99)THEN

C Re-establish data.
              matarrayindex=IPRMAT(I,IL)
              if(matarrayindex.gt.0)then

C Make up NAM as combination of matnam and matdoc.
                lnam=lnblnk(matname(matarrayindex))
                lfordoc = 72 - (lnam +4)    ! space left for doc
                write(NAM,'(3a)') matname(matarrayindex)(1:lnam),' : ',
     &            matdoc(matarrayindex)(1:lfordoc)
                write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
              endif
          ELSE
              matarrayindex=IPRMAT(I,IL)
              if(matarrayindex.ge.0)then

C And if matarrayindex is zero then resetn dbcon dbden dbsht to mimic erpcdb
                if(matarrayindex.eq.0)then
                NAM='AIR'
                elseif(matarrayindex.gt.0)then

C Make up NAM as combination of matnam and matdoc.
                  lnam=lnblnk(matname(matarrayindex))
                  lfordoc = 72 - (lnam +4)    ! space left for doc
                  write(NAM,'(3a)') matname(matarrayindex)(1:lnam),
     &              ' : ',matdoc(matarrayindex)(1:lfordoc)
                endif
              endif
            write(PNAM(IL),'(a)') NAM(1:lnblnk(NAM))
          ENDIF
  116   CONTINUE

        WRITE(IFMUL,32)LAYERS(I),DESC(I)(1:48)
   32   FORMAT('# layers  description   optics name   symmetry tag',/,
     &    I5,4X,A48,/,'# mat ref thickness (m) mat descr & air gap R')

        DO 115, IL=1,LAYERS(I)
          WRITE(IFMUL,'(I5,F10.4,2X,A)')IPR(I,IL),DTHK(I,IL),
     &              PNAM(IL)(1:lnblnk(PNAM(IL)))
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
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      logical XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
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

C Scan the binary file data into materials commons and if this was
C sucessful and matver was set to 1.1 in matformbin then we can
C carry on using the materials common blocks for subsequent access.
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


