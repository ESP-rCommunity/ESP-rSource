C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

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

C======Hot2000=============================
C	Subroutine:
C		SetDllParms
C
C	Parameters:
C		inf - character*144 -  input cfg file name
C
C	Comments:
C
C	This is NOT a "C" callable function.
C	Set parameters for esp-r engine as if running
C	in silent mode and text mode
C
C	
C     iuout = 215	- log file unit number
C     itu=iuout		- trace file unit number
C     inf = cCfgFile	- input file name
C     sps  = '????'	- simulation parameter set
C     aut  = 'silent'	- interactive or silent (esp-r silent mode switch)
C     LCMDFL            - cfg file name and path
C     inf=LCMDFL	- cfg file name and path            
C 
C     termtype = -1	- console mode
C     iappw = 0		- ??
C     iappx = 0		- ??
C     iappy = 0		- ??
C     actf  = 'UNKNOWN'	- ??
C     argument = ' '	- ??
C     mode 	= 'text' - esp-r running mode, graphics or tes
C     sps 	= 'win'  - operating system
C     actf 	= 'unknown' - ??
C     outs	=' '	- ??
C
C----------------------------------------------------     

      subroutine SetDllParms(termtype,iappw,iappx,iappy,inf,sps,aut,
     &	actf)
#include "building.h"
#include "model.h"

      common/outin/iuout,iuin
      common/shout/icout
      common/BPSDLL/ iLogFile, cBpsVersion, cPrjPath, cPrjName, 
     &           cCfgPath, cCfgFile, cLogFile, lBpsDebug

      character*72 cBpsVersion, cPrjPath, cPrjName, cCfgPath,
     &           cCfgFile, cLogFile
      logical lBpsDebug
     
      common/rcmd/LCMDFL

      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
            
      character LCMDFL*144,inf*144,sps*8,aut*12,fs*1,actf*72
 
      integer termtype,iappw,iappx,iappy
      logical unixok
      common/SPFLN/spsname
      character spsname*8
      
C NT version.
      integer m
      character*48 prog

C Unix version.
C      integer iargc,m
      character argument*72,mode*8,appn*24
      character outs*124,outs248*248

C==============================
C bps Dll variable assignments
C so that esp-r runs in silent 
C mode and all text is
C written to the log file (bps.log) 
C==============================
      
      iLogFile = 215
      inf    = cCfgFile
      LCMDFL = inf
            

      iappw  = 0
      iappx  = 0
      iappy  = 0 	
      sps    = spsname
      aut    = 'silent'      
      actf   = 'UNKNOWN'
      argument = ' '      
      termtype = -1

      upath = cCfgPath	
      mode = 'text'
      
      OPEN(unit=iuout,FILE=cLogFile, position ='APPEND')
      OPEN(unit=5,FILE=cLogFile, position ='APPEND')
      OPEN(unit=6,FILE=cLogFile, position ='APPEND')
      OPEN(unit=icout,FILE=cLogFile, position ='APPEND')
      OPEN(UNIT=iLogFile,FILE=cLogFile, position='APPEND' )
       	
      write(outs248,'(a,a)')' the input file is: ',LCMDFL
      call edisp(iuout,outs248)
 
 
      write(outs248,'(5a)') 'Starting bpsDll',
     &    ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &    inf(1:lnblnk(inf))

      call edisp(iuout,outs248)
      outs	=' '
      
      write(outs,'(5a)') 'and parameters ',sps(1:lnblnk(sps)),
     &    ' in ',aut,' mode.'
      call edisp(iuout,outs)

      return
      end
      
      
C======Hot2000=============================
C	Subroutine:
C		WriteToLog
C
C	Parameters:
C		strLogEntry - characer*30 -  input log file entry
C
C	Comments:
C
C	This is NOT a "C" callable function.
C
C
C----------------------------------------------------     

      subroutine WriteToLog( strLogEntry, bOkay )
      character*30 strLogEntry
      logical bOkay
            
      common/BPSDLL/ iLogFile, cBpsVersion, cPrjPath, cPrjName, 
     &           cCfgPath, cCfgFile, cLogFile, lBpsDebug

      character*72 cBpsVersion, cPrjPath, cPrjName, cCfgPath,
     &           cCfgFile, cLogFile
      logical lBpsDebug
 

      
      close( iLogFile )
      open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
      if( bOkay ) then
      	write( iLogFile, * ) strLogEntry, ' okay'
      else
      	write( iLogFile, * ) strLogEntry, ' not okay'
      endif
      close( iLogFile )
      open(UNIT=iLogFile, FILE=cLogFile,position='APPEND' )
      
      return
      end
      
