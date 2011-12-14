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

C ************ MFINIT 
C Sets default values for iteration parameters, Pstack calculation
C model, matrix solver type and trace output flag. Assigns unit
C numbers for the flow network description (IFPRB), weather (IFCLM)
C and results (IFRES) files.

      SUBROUTINE MFINIT(IFPRB,IFCLM,IFRES)

#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C ICLIM,LCLIM (for climate file)
C default file names for databases
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

      CHARACTER DFILE*72,LTMP*72,lltmp*144
      character LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER OUTSTR*124,outs248*248
      logical XST,OK,DOK

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='MFINIT'  ! set for subroutine

C Initiates filling of flow component data arrays.
      CALL MFCDAT

C Set default values for iteration parameters, stack pressure difference
C calculation model, matrix solver type and trace output flag.
      MAXITF=1500
      FERREL=1.0E-4
      FERMFL=1.0E-10
      PMAX=50.
      STEFFR=-0.25
      IPSMOD=1
      MSLVTP=2
      MFTRAC=0

C Assign flow network description file. If given file does not
C exist, confirm with user that this is to hold a new description.
   20 IF(IFPRB.EQ.10) THEN
         CALL ERPFREE(IFPRB,ISTAT)
      ELSE
         IFPRB=10
      END IF
      helptopic='mass_flow_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
  289 CALL EASKS(LAPROB,'Flow network description file?',' ',
     &     72,DAPROB,'mass flow network file',IER,nbhelp)
      IF(LAPROB.EQ.' ')GOTO 289
      CALL ERPFREE(IFPRB,ISTAT)
      call FINDFIL(LAPROB,XST)
      IF(.NOT.XST)THEN
        CALL EASKAB('File does not exist!',' ',
     &              'Create?','Re-specify?',IW,nbhelp)
        IF(IW.EQ.2)GOTO 289
      ENDIF
      CALL EFOPSEQ(IFPRB,LAPROB,3,IER)
      IF(IER.NE.0) GOTO 20
      write(currentfile,'(a)') LAPROB(1:lnblnk(LAPROB))

C If the first record does not contain valid data then
C file is regarded as being empty.
      CALL STRIPC(IFPRB,OUTSTR,0,ND,1,'mass file line 1',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NNOD,0,MNOD,'F','number of nodes',IER)
      CALL EGETWI(OUTSTR,K,NCMP,0,MCMP,'F','number of component',IER)
      CALL EGETWI(OUTSTR,K,NCNN,0,MCNN,'F','number of connection',IER)
      IF(IER.EQ.1)GOTO 20

C Assign wind pressure distribution file and read NPRE. If for some
C reason IAPRES is still zero then set to 11.
   30 if(IAPRES.eq.0)IAPRES=11
      helptopic='pres_coef_file_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)

 288  write(outs248,'(2a)') 'Current pressure coefficients db is ',
     &  LAPRES(1:lnblnk(LAPRES))
      call edisp248(iuout,outs248,100)
      lltmp=LAPRES
      llt=lnblnk(lltmp)
      iglib = igraphiclib()
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(lltmp,'Pressure coefficients database?',' ',
     &      96,DAPRES,'pressure coefficients db',IER,nbhelp)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(lltmp,'Pressure coefficients database?',' ',
     &      124,DAPRES,'pressure coefficients db',IER,nbhelp)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(lltmp,'Pressure coefficients database?',' ',
     &      144,DAPRES,'pressure coefficients db',IER,nbhelp)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(lltmp,'Pressure coefficients database?',' ',
     &    144,DAPRES,'pressure coefficients db',IER,nbhelp)
      else
        CALL EASKF(lltmp,'Pressure coefficients database?',' ',
     &    96,DAPRES,'pressure coefficients db',IER,nbhelp)
      endif

      IF(LLTMP.EQ.' ')GOTO 288
      LAPRES=lltmp
      CALL EFOPSEQ(IAPRES,LAPRES,1,IER)
      IF(IER.NE.0) GOTO 30
      write(currentfile,'(a)') LAPRES(1:lnblnk(LAPRES))

C Read in the pressure coefficients database and check contents.
      CALL STRIPC(IAPRES,OUTSTR,0,ND,1,'Pressure coefficient db',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NPRE,1,MPRD,'W','Number of entries',IER)
      IF(NPRE.LT.1.OR.NPRE.GT.MPRD)THEN
        CALL USRMSG('Database is not of the correct type!',
     &              'Respecify.','W')
        GOTO 288
      ENDIF

C Assign weather file and read year number.
   40 IFCLM=12
      helptopic='climate_file_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      lltmp=LCLIM
      llt=lnblnk(lltmp)
      iglib = igraphiclib()
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(lltmp,'Weather file?',' ',
     &      96,DCLIM,'weather file',IER,nbhelp)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(lltmp,'Weather file?',' ',
     &      124,DCLIM,'weather file',IER,nbhelp)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(lltmp,'Weather file?',' ',
     &      144,DCLIM,'weather file',IER,nbhelp)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(lltmp,'Climate file?',' ',
     &    144,DCLIM,'weather file',IER,nbhelp)
      else
        CALL EASKF(lltmp,'Climate file?',' ',
     &    96,DCLIM,'weather file',IER,nbhelp)
      endif
      CALL ERPFREE(IFCLM,ISTAT)
      ier=0
      call EFOPRAN(IFCLM,lltmp,144,1,IER)
      IF(ier.eq.0)THEN
        READ(IFCLM,REC=366,ERR=999) IRY
        LCLIM=lltmp
      ELSE
        CALL LUSRMSG('Could not find ',lltmp,'W')
        goto 40
      ENDIF

C Assign results file; make sure this is not the problem file.
   50 IFRES=13
      helptopic='flow_res_file_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      DFILE='new.mfr'
      LTMP=LAFRES
      CALL EASKS(LTMP,'Network flow simulation results file?',
     &  ' ',72,DFILE,'flow results file',IER,nbhelp)
      IF(LTMP(1:lnblnk(LTMP)).EQ.LAPROB(1:lnblnk(LAPROB)))THEN
        CALL USRMSG('Same as the flow network description file!',
     &  'Please enter another file name.','W')
        GOTO 50
      ENDIF
      LAFRES=LTMP
      CALL EFOPSEQ(IFRES,LAFRES,3,IER)
      IF(IER.NE.0) GOTO 50
      write(currentfile,'(a)') LAFRES(1:lnblnk(LAFRES))
      RETURN

C File error trap.
  999 dok=.false.
      helptopic='mfs_file_error_trap'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK('Error reading weather data!','Continue?',
     &           OK,dok,nbhelp)
      IF(OK)THEN
        goto 40
      ELSE 

C Quit after removing the flow network description file if empty.
        IF(NNOD.EQ.0.AND.NCMP.EQ.0.AND.NCNN.EQ.0) THEN
          CALL EFDELET(IFPRB,ISTAT)
        ENDIF
        STOP 'MFINIT: terminating mfs.'
      ENDIF

      END

