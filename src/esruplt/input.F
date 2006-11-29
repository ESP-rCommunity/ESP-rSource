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

C This file contains the following routines:

C      MZINPT

C ******************** MZINPT
C MZINPT controls input of a data set which defines a plant
C configuration for simulation.
C Common block variables are:

C INDCFG      - configuration file index
C                2 = plant only

C IFLWN       - plant fluid flow simulation index (1 = on)
C ICFFS       - fluid flow network node associated with each plant
C               component. NB fluid flow network and plant energy
C               network need not be matched

      SUBROUTINE MZINPT

#include "plant.h"
#include "esprdbfile.h"

      COMMON/ER1/IER1
      COMMON/FILEP/IFIL
      common/pophelp/h(60)
      COMMON/C2CFG/LCFGF
      COMMON/C6/INDCFG
      COMMON/CHANNL/NOCHAN
      common/trc/itrc

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/FFN/IFLWN,ICFFS(MPCON)

      COMMON/PREC7/ITCNST

C Path to problem and command line file (if any).
      common/rpath/path
      common/rcmd/LCMDFL

C Defaults.
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      character*72 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF

      CHARACTER H*72,LCFGF*72,path*72,LCMDFL*144,L144*144
      CHARACTER outs*124
      LOGICAL OK,dok

C Reset error indicator
      IER1=0

C Initialise NOCHAN (next occupied) utilities file channel
      NOCHAN=IFIL+6

C Initialise plant component database unit.
      iaprob=ifil-1

C Initialise system configuration file (read only mode)
      IUNIT=IFIL+1

C Ask for plant file name.
      L144=LCMDFL

C Call EASKF depending on the current file name length.
   44 llt=lnblnk(L144)
      H(1)='This is the plant network definition.'

C Use ifdefs because the X11 version will be returning only the
C name of the file, while the GTK version will be returning the
C name with the full path.
#ifdef X11
      if(llt.lt.96)then
        CALL EASKF(L144,' Plant configuration file?',' ',96,DFCFG,
     &    'plt config file name',IER,1)
      elseif(llt.ge.96.and.llt.lt.124)then
        CALL EASKF(L144,' Plant configuration file?',' ',124,DFCFG,
     &    'plt config file name',IER,1)
      elseif(llt.ge.124.and.llt.le.144)then
        CALL EASKF(L144,' Plant configuration file?',' ',144,DFCFG,
     &    'plt config file name',IER,1)
      endif
#else
      CALL EASKF(L144,' Plant configuration file?',' ',144,DFCFG,
     &  'plt config file name',IER,1)
#endif

      IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 44
      call st2file(L144,LCMDFL)

C Find the path and local file name.
      call fdroot(LCMDFL,path,LCFGF)
      IFPNF=IFIL+1
      CALL EFOPSEQ(ifpnf,lcfgf,1,IER)
      IF(IER.LT.0)THEN
         IER=1
         RETURN
      ENDIF

C Since this is a plant only version then
C set INDCFG to plant only configuration.
      indcfg=2

c Call PLTCFG to read plant configuration data.
c iunit= unit for configuration file.
c iunit1= iaprob
c itrc  = If 0 then suppress reporting messages.
      call pltcfg(iunit,iunit1,IUOUT,itrc)

c Plant description is now complete
c Unless error occured, initiate computation/setting-up of:
      if(ier1.eq.0) then

c Fill valid component type arrays
        call mfcdat

c fluid flow simulation parameters if fluid flow network defined
        if(iflwn.eq.1) call mflwsu

c Plant matrix template setup & checks
        call mzpmxt(iunit1)

c and plant manufacturer's and/or static data & plant template checks
        call mzsdat

c For now set start-up period to 1. Later this will change to
c number of hours instead of days.
        itcnst=1
        H(1)='The start-up period is one or more days preceding'
        H(2)='the user defined simulation period. The results for'
        H(3)='this start-up period are not transferred to the'
        H(4)='results library and so cannot be analysed. '
        H(5)='  '
        H(6)='Note that a start-up period prior to January 1 '
        H(7)='will utilise climatic data from the end of the'
        H(8)='same year. '
        WRITE(outs,'(A,I3,A)')
     &    'Indicative simulation start-up period is ',
     &    ITCNST,' days.  Respecify ?'
        dok=.false.
        CALL ASKOK(' ',outs,OK,dok,8)
        IF(OK)THEN
          CALL EASKI(ITCNST,' ',' Number of days ? ',
     &          1,'F',364,'F',1,'startup period',IER,8)
        ENDIF
      ENDIF

c Free system configuration file (and if opened before also
c plant components database and/or fluid flow problem file).
      call erpfree(iunit,istat)
      if(iunit1.ne.0) call erpfree(iunit1,istat)

c Return to calling module
      return
      end
