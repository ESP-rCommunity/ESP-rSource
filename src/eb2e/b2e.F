C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C B2E     - the main controlling routine.
C ****************************************** B2E ***************************************
C b2e.F is the main roytine which links a BEMS system to
C ESP-r. The program reads atransfer file from the BEMS
C system and then commisions a simulation, returning the
C pertinent data to the BEMS tool. 
C *********************************** B2E ******************************

      PROGRAM B2E
      USE START_UP
#include "building.h"
#include "model.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appw/iappw,iappx,iappy
      COMMON/FILEP/IFIL
      common/trc/itrc
      COMMON/INPER/INIT
      COMMON/OUTPCH/ICOUT
      COMMON/PERC/ID1,IM1,IT1,ID2,IM2,IT2,IDS,IDF,INEW
      COMMON/DAYSF/KDS,KDF

C Path to problem and command line file (if any).
      common/rpath/path
      
C Significant figure reporting limit (NSIGFIG).
      common/SFIG/NSIGFIG

C Name of current application
      common/APPNAME/cAppName
      character cAppName*12

C Ask to overwrite flag.
      COMMON/OVRWT/AUTOVR
      logical AUTOVR      

      CHARACTER INTER*144,INF*144
      character path*72,outs248*248
      character fs*1

      character cVnum*38      ! returned from ESPrVersionNum
      character pagestitle*62 ! for banner title via epages call
      LOGICAL unixok

#ifdef OSI
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iiw1,iiw2,iiw3,iiw4,iimenu
#else
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iiw1,iiw2,iiw3,iiw4,iimenu
#endif

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.

C Initial assumptions.
      call ezero
      call curmodule('b2e ')
      cAppName = 'b2e'
      ITRC=1
      IUOUT=6
      IUIN=5
      LIMTTY=24
      LIMIT =24
      IFIL=10
      ID1=1
      IM1=1
      IT1=1
      ID2=31
      IM2=12
      IT2=24
      IDS=1
      IDF=365
      KDS=1
      KDF=24
      IUTDF=IFIL+7
      NSIGFIG=3
      AUTOVR=.false. ! by default, ask to overwrite files

C Get command line parameters.
      call parpsf(MODL,iappw,iappx,iappy,inf)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      write(path,'(a1,a1)')'.',fs

c Initialise output device, assume minimal trace, set reduced
C size of window display.
      MMOD=MODL
      if(iappw.eq.0.and.iappx.eq.0.and.iappy.eq.0)then
        iappw=570
        iappx=80
        iappy=100
      else
        if(iappx.le.0)iappx=80
        if(iappy.le.0)iappy=100
        if(iappw.le.200)then
          iappwi=int(570*iappw*0.01)
          iappw=iappwi
        elseif(iappw.gt.200)then
          continue
        endif
      endif

C Set pixels high to iappw and pixels wide to factor in monitor size.
      iapphi=iappw
      iappwi=int(real(iappw)*(1024.0/780.0))

      if(iappw.gt.0.and.iappw.lt.100)then
        menuchw = MAX0(int(24*iappw*0.01),12)
        LIMTTY= MAX0(int(8*iappw*0.01),4)
        LIMIT = MAX0(int(8*iappw*0.01),4)
      else
        menuchw = 24
        LIMTTY=8
        LIMIT =8
      endif
      IF(MMOD.EQ.8)THEN

C Set initial font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use proportional for menus
C and dialog.
        IMFS=5
        IFS=4
        ITFS=4
        call userfonts(IFS,ITFS,IMFS)
        call defaultfonts(IFS,ITFS,IMFS)  ! and remember these as defaults
      ELSE
        LIMTTY=16
        LIMIT =16
      ENDIF

C Find the current ESP-r version number and add it to application title.
      call ESPrVersionNum(cVnum)
      write(pagestitle,'(2a)') 'ESP-r BEMS link ',
     &  cVnum(1:lnblnk(cVnum))

      lntitle=lnblnk(pagestitle)
      CALL EPAGES(MMOD,IUIN,IUOUT,iappwi,iapphi,iappx,iappy,menuchw,
     &  pagestitle,lntitle)

      IF(MMOD.EQ.-6)then
        ICOUT=0
      else
        ICOUT=IUOUT
      endif

C Open the text display box equal to LIMTTY if MMOD = 8.
      IF(MMOD.EQ.8)THEN

C Setup and pass in parameters to win3d.
        iiw1=10; iiw2=30; iiw3=4; iiw4=3; iimenu=menuchw
        iigl=igl; iigr=igr; iigt=igt; iigb=igb; iigw=igw; iigwh=igwh
        CALL win3d(iimenu,iiw1,iiw2,iiw3,iiw4,
     &    iigl,iigr,iigt,iigb,iigw,iigwh)
        igl=int(iigl); igr=int(iigr); igt=int(iigt); igb=int(iigb)
        igw=int(iigw); igwh=int(iigwh)
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          call opencpw
          call opensetup
        endif
        mdispl=0
        nifgrey=0
        ncset=0
        ngset=0
        nzonec=0
        call foundcolour(mdispl,nifgrey,ncset,ngset,nzonec)
        call startbuffer()
      ENDIF
      call edisp(IUOUT,' ')
      CALL ESPrVersion("summary",cAppName,IUOUT)

C Find the user's home folder then get user's custom settings.
C Make temporary use of file unit IAF=IFIL+1.
      IAF=IFIL+1
      call usrhome(upath)
      if(unixok)then
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'.esprc'
      else
        write(esprc,'(3a)') upath(1:lnblnk(upath)),fs,'esprc'
      endif
      call scesprc(esprc,IAF,0,IIER)

C Scan the defaults file for default configuration.
      call escdef(IAF,'s',IER)

      INIT =0

C Take command line file name to LCMDF and use as initial climate file.
      if(inf(1:2).eq.'  '.or.inf(1:4).eq.'UNKN')then
        call edisp(iuout,'Error: no input file specified')
        close(ieout)
        CALL ERPFREE(ieout,ISTAT)
        call epwait
        call epagend
        STOP
      else
        INTER=inf
        write(OUTS248,'(A,A)')' the input file is: ',INTER
C        CALL EDISP248(IUOUT,OUTS248,80)
      endif

C Call the B2E engine, the main controlling routine with the filename argument .
      CALL B2ECON(INTER)

      END

C **************** B2ECON **********************************************
C This is the main controlling routine for the transfer of data between a BEMS
C system and ESP-r. TOnly one routine is listed:
C B2ECON

      SUBROUTINE B2ECON(INTER)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      character INTER*144
 
      INTEGER IERROR
      LOGICAL CFGSCN

      CFGSCN=.FALSE.
          
C Call the routines which read and process the data sent in the interaction file.
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,'b2e reading the BEMS interaction file ...')
      CALL RINTERA(INTER,IERROR,CFGSCN)
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,'b2e reading the BEMS transfer file file ...')
      CALL READBTF(IERROR)

C Generate look ahead climate data.
      CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,'b2e setting up climate prediction ... ')
      CALL CLOOKUP('OFFS')
      CALL EDISP(IUOUT,' ')

C Write out a tdf update file containing the data from the
C Bems update and the climate lookahead.
      CALL EDISP(IUOUT,' ') 
      CALL EDISP(IUOUT,'b2e writing out the tdf update file ...')
      CALL TDFUDWR('U',IERROR)

C Update the tdf database
      CALL EDISP(IUOUT,' ') 
      CALL EDISP(IUOUT,'b2e updating the tdf update file with BEMS')
      CALL EDISP(IUOUT,'data ... ')
      CALL TDFUPDT('BEMS')

C Run the simulation template and analyse results
      CALL EDISP(IUOUT,'b2e running simulation(s) ...')
      CALL RUNSIM

      RETURN
      END

C Dummy routines from common3dv.F
      SUBROUTINE ADJVIEW(IER)
      ier=0
      return
      end

      SUBROUTINE redraw(IER)
      IER=0
      return
      end

      subroutine chgazi(icazi,ifrlk)
      return
      end

      subroutine chgelev(icelev,ifrlk)
      return
      end

      subroutine chgpan(ix,iy)
      return
      end

      subroutine chgzoom(imode)
      return
      end

      subroutine optview
      return
      end

      subroutine chgsun(isunhour)
      return
      end

      subroutine chgeye(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,JITVNO,
     &   JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,JITHLZ,
     &   JITPPSW)

C Passed paramters.
      real EVX,EVY,EVZ,VX,VY,VZ,EAN,DIS
#ifdef OSI
      integer JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR,JITGRD
      integer JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#else
      integer*8 JITZNM,JITSNM,JITVNO,JITOBS,JITVIS,JITVOBJ,JITSNR
      integer*8 JITGRD,JITORG,JITBND,JITDSP,JITHLS,JITHLZ,JITPPSW
#endif     
      return
      end

      subroutine chgzonpik(jizgfoc,jnzg)
#ifdef OSI
      integer jizgfoc,jnzg  ! for use with viewtext
#else
      integer*8 jizgfoc,jnzg  ! for use with viewtext
#endif
      return
      end

      subroutine chgzonpikarray(jnznog,jnznogv)
#ifdef OSI
      integer jnznog,jnznogv
#else
      integer*8 jnznog,jnznogv
#endif
      return
      end

      subroutine imgdisp(iforce,focus,ier)
      character focus*4
      return
      end

      subroutine SVDSOPT
      return
      end

      subroutine RCDSOPT
      return
      end

      SUBROUTINE DRAWOBS(IFOC,ier)
      return
      end

      SUBROUTINE DRWSEN(ier)
      return
      end

      SUBROUTINE EGRNDR(IER)
      return
      end

      SUBROUTINE DSGRID(RH,GD,LD,IER)
      return
      end

      SUBROUTINE EMKVIEW(IUO,CFGOK,IER)
      logical cfgok
      ier=0
      return
      end

      SUBROUTINE EPKMLC(ISEL,PROMPT1,PROMPT2,IER)
      CHARACTER*(*) PROMPT1,PROMPT2
      ier=0
      return
      end
      
      SUBROUTINE EDMLDB2(chgdb,ACTION,isel,IER)
      logical chgdb
      character*1 ACTION
      integer isel,ier
      ier=0
      return
      end

      SUBROUTINE INLNST(ITYP)
      return
      end

      SUBROUTINE PLELEV(direc)
      CHARACTER direc*1
      return
      end

C << copy from edatabase.F >>
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
      WRITE(MLCN,'(A)')DES
      lnmlcn=lnblnk(MLCN)
      if(MLCN(1:4).eq.'UNKN') return
      do ii=1,nmlc
        if(MLCN(1:lnmlcn).eq.mlcname(ii)(1:lnmlcname(ii))) then
          imlcindex=ii   ! remember MLC index     
        endif
      end do
      return
      end

C Dummy routines.
      subroutine Find_season_parameters()
      return
      end

      SUBROUTINE BASESIMP_INPUTS(ICOMP,IER)
      integer icomp,ier
      return
      end

C The following from econstr.F
      SUBROUTINE ECONST(LCONS,IUNIT,ICOMP,ITRC,ITRU,IER)
      character LCONS*72
      integer IUNIT,ICOMP,ITRC,ITRU,IER
  
      return
      end

      SUBROUTINE ERTWIN(ITRC,ITRU,IUA,LUA,ICOMP,IER)
      character LUA*72
      integer ITRC,ITRU,IUA,ICOMP,IER
  
      return
      end

