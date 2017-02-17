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

C ********************************************************************************
C************************************************************************************
C************************************************************************************
C************************************************************************************
C   MODULE PARSE_COMMAND_LINE written to extract the communality in all the modules for 
C   setting up environemt and parsing command lines.

C************************************************************************************
      MODULE PARSE_COMMAND_LINE
      IMPLICIT NONE
      
C Constants used to differentiate between modules      
      integer, parameter :: CONST_PARPSF = 1
      integer, parameter :: CONST_PARPSFF = 2
      integer, parameter :: CONST_PARSIM = 3
      integer, parameter :: CONST_PARRES = 4
      integer, parameter :: CONST_PARCLM = 5
      integer, parameter :: CONST_PARSISH = 6
      integer, parameter :: CONST_PARSPRJ = 7
      integer, parameter :: CONST_PARSFZ = 8
      integer, parameter :: CONST_PARTF = 9
      integer, parameter :: CONST_PARTFA = 10
      integer, parameter :: CONST_PARSE2R = 11
      integer, parameter :: CONST_PARSFACT = 12

      SAVE !ensures values do not change between sucessive invocations
C Declare parameters 
      character(144)  :: PassedCommandLine
      integer :: CommandLineArguments
      logical :: UsePassedCommandLine
      
C Add Proceedures
      CONTAINS

C************************************************************************************
C SetPassedCommandLine lets the command line be overwritten by setting a string
C with the passed in command line arguments. (Sets UsePassedCommandLine to true)
C************************************************************************************
      SUBROUTINE SetPassedCommandLine(argc, argv)
      IMPLICIT NONE
C Declare  calling parameter types
      character(144), intent(in)  :: argc !commandline string
      integer, intent(in) :: argv !number of commandline parameters

      PassedCommandLine = argc
      CommandLineArguments = argv
      UsePassedCommandLine = .true.
      
      END SUBROUTINE SetPassedCommandLine


C************************************************************************************
C GetCommandLineArguments () Parses the command line
C If the command line is passed in (for co-simulation with TRNSYS) 
C UsePassedCommandLine is true and the subroutine EGTW (lib/esru_lib.F)
C is used to parse the "command line" string.          
C *******************************************************************************
      SUBROUTINE GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      IMPLICIT NONE
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/APPNAME/cAppName
C Timeout information
      COMMON/TIMEOUT/sTimeout,iTimeoutFlag
C Flag for H3K customizations 
      common/H3KCustom/bEnableH3kExtensions
      logical bEnableH3kExtensions
C Declare  calling parameter types
      integer, intent(out)  ::  termtype,iappw,iappx,iappy,iverb
      integer, intent(in)  ::  module
      real, intent(out)     ::  data1,data2,data3
      character(144), intent(out)  :: inf, inf2
      character(16), intent(out)   :: zone,act
      character(72), intent(out)   :: actf,ascif
      character(12), intent(out)   :: aut,aim
      character(4), intent(out)    :: browse
      character(8), intent(out)    :: sps

C Variable declaration
      character(8)   :: ModuleName
      character(12)  :: cAppName
      character(72)  :: argument,sTimeout
      character(48)  :: prog
      character(8)   :: mode
      character(24)  :: appn
      character(348) :: outs !If this is only 248 clm crashes on install if pathnames are long
      integer :: m, i, IOS, IUOUT,IUIN, iXavail, pos
      integer :: iTimeoutFlag
      integer :: err
      logical :: unixok

C Function definitions      
      integer :: lnblnk
      integer :: iargc

C Variable initialisation
      argument = ' '
      data1=0.0
      data2=0.0
      data3=0.0
      iverb =0
      pos = 0
      i = 0
      iappw = 0
      iappx = 0
      iappy = 0
      aut  = 'interactive'

      IOS=0  ! initial value
      act = 'NONE'
      aim = 'NONE'
      browse = 'no'
      zone = 'ALL'
      sps  = 'UNKNOWN'
      inf  = 'UNKNOWN'
      inf2  = 'UNKNOWN'
      actf = 'UNKNOWN'
      ascif  = 'UNKNOWN'
      if (module.eq.CONST_PARRES) act = 'UNKNOWN'
      if (module.eq.CONST_PARSIM) aut  = 'default'


      if (UsePassedCommandLine) then 
        m = CommandLineArguments
      else 
        m = iargc()
      end if
C Determine if an X graphics library (GTK or X11) was linked
C into binary, and set default terminal type as necessary
      if ( iXavail() == 1 ) then
        termtype = 8
        mode = 'graphic'
      else
        termtype = -1
        mode = 'text'
      end if
      if (UsePassedCommandLine) then 
        call EGETW(PassedCommandLine,pos,prog,'-','program',err)
      else 
        call getarg(i,prog)
      end if
      call findapp(prog,appn)

      if(m.eq.0)then
        return
      elseif(m.ge.1)then
        Parseloop: DO
          i=i+1 
          if (UsePassedCommandLine) then 
            call EGETW(PassedCommandLine,pos,argument,'-',
     &                  'argument',err)
          else
            call getarg(i,argument)
          end if
          if(i.gt.m)EXIT Parseloop
          if(argument(1:5).eq.'-help')then
            call parpsfh(appn)
            stop
          elseif(argument(1:8) .eq. '-version' ) then
C Display version information and then quit
            CALL ESPrVersion("summary",cAppName,IUOUT)
            stop
          elseif( argument(1:10) .eq. '-buildinfo' ) then
C Display detailed build information and then quit
            CALL ESPrVersion("details",cAppName,IUOUT)
            stop
C Enable h3k-specific customizations that may disrupt general esp-r functionality
C (disabled by default!)            
          elseif(argument(1:4).eq.'-h3k') then 
            bEnableH3kExtensions = .true. 
          elseif(argument(1:3).eq.'-vv')then
            iverb = 2   ! tell application to run with debug verbose
          elseif(argument(1:2).eq.'-v')then
            iverb = 1   ! tell application to run with debug moderate
                        ! Note: this option MUST follow -version for both to be supported
          elseif(argument(1:5).eq.'-mode')then

            i=i+1
            if (UsePassedCommandLine) then 
              call EGETW(PassedCommandLine,pos,argument,
     &                   '-','argument',err)
            else 
              call getarg(i,argument)
            end if
            if(argument(1:4).eq.'text')then
              termtype = -1
              mode = 'text'
            elseif(argument(1:4).eq.'page')then
              termtype = -2
              mode = 'page'
            elseif(argument(1:5).eq.'graph')then
              termtype = 8
              mode = 'graphic'
            elseif(argument(1:6).eq.'script')then
              termtype = -6
              mode = 'script'
            endif
          elseif(argument(1:2).eq.'-s')then
            if ((module.ne.CONST_PARTF).or.
     &          (module.ne.CONST_PARTFA)) then
              i=i+1
              if (UsePassedCommandLine) then 
               call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
              else 
                call getarg(i,argument)
              end if
              read(argument,*,IOSTAT=IOS,ERR=2)iappw
              i=i+1
              if (UsePassedCommandLine) then 
               call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
              else 
                call getarg(i,argument)
              end if
              read(argument,*,IOSTAT=IOS,ERR=2)iappx
              i=i+1
              if (UsePassedCommandLine) then 
               call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
              else 
                call getarg(i,argument)
              end if
              read(argument,*,IOSTAT=IOS,ERR=2)iappy
            end if
          elseif(argument(1:5).eq.'-file')then
            if (module.eq.CONST_PARPSFF) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,inf,'-',
     &                   'argument',err)
              else 
                call getarg(i,inf)
              end if
              i=i+1
              if (UsePassedCommandLine) then 
               call EGETW(PassedCommandLine,pos,inf2,'-',
     &                   'argument',err)
              else 
                call getarg(i,inf2)
              end if
            else
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,inf,'-',
     &                   'argument',err)
              else 
                call getarg(i,inf)
              end if
            end if
          ! It is important to check for -actf before checking for -act.
          elseif(argument(1:5).eq.'-actf')then
             if ((module.eq.CONST_PARSFACT).or.
     &           (module.eq.CONST_PARRES).or.
     &           (module.eq.CONST_PARSIM).or.
     &           (module.eq.CONST_PARSE2R)) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,actf,'-',
     &                   'argument',err)
              else 
                call getarg(i,actf)
              end if
            end if
          elseif(argument(1:4).eq.'-act')then
            if ((module.eq.CONST_PARTFA).or.
     &          (module.eq.CONST_PARSFACT)) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
C If act is asci2bin or bin2asci read ascii file name
              if(act(1:8).eq.'asci2bin'.or.act(1:8).eq.'bin2asci')then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,actf,'-',
     &                   'argument',err)
                else 
                  call getarg(i,actf)
                end if
              end if
            else if (module.eq.CONST_PARSE2R) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
            else if (module.eq.CONST_PARRES) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,aut,'-',
     &                   'argument',err)
              else 
                call getarg(i,aut)
              end if
            else if (module.eq.CONST_PARCLM) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,aut,'-',
     &                   'argument',err)
              else 
                call getarg(i,aut)
              end if
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,ascif,'-',
     &                   'argument',err)
              else 
                call getarg(i,ascif)
              end if
            else if (module.eq.CONST_PARSISH) then
              i=i+1
              if (UsePassedCommandLine) then 
               call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
C If act is asci2bin or bin2asci read ascii file name
              if(act(1:8).eq.'asci2bin'.or.act(1:8).eq.'bin2asci')then
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,ascif,'-',
     &                   'argument',err)
                else 
                  call getarg(i,ascif)
                end if
              end if
            else if(module.eq.CONST_PARSPRJ)then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,act,'-',
     &                   'argument',err)
              else 
                call getarg(i,act)
              end if
              if(act(1:6).eq.'rotate')then

C If the -act argument is rotate then also deal with data1,
C data2 and data3.
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data1
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data2
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data3
              elseif(act(1:9).eq.'transform')then

C If the -act argument is transform then also deal with data1,
C data2 and data3.
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data1
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data2
                i=i+1
                if (UsePassedCommandLine) then 
                  call EGETW(PassedCommandLine,pos,argument,'-',
     &                   'argument',err)
                else 
                  call getarg(i,argument)
                end if
                read(argument,*,IOSTAT=IOS,ERR=2)data3
              end if
            end if
          elseif(argument(1:5).eq.'-zone')then
            if ((module.eq.CONST_PARSFZ).or.
     &          (module.eq.CONST_PARSISH).or.
     &          (module.eq.CONST_PARSE2R).or.
     &          (module.eq.CONST_PARSPRJ))then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,zone,'-',
     &                   'argument',err)
              else 
                call getarg(i,zone)
              end if
            end if
          elseif(argument(1:8).eq.'-purpose')then
            if (module.eq.CONST_PARSE2R) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,aim,'-',
     &                   'argument',err)
              else 
                call getarg(i,aim)
              end if
            end if            
          elseif(argument(1:2).eq.'-b')then
            if ((module.eq.CONST_PARSE2R).or.
     &          (module.eq.CONST_PARSIM)) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,browse,'-',
     &                   'argument',err)
              else 
                call getarg(i,browse)
              end if
            end if
          elseif(argument(1:2).eq.'-p')then
            if (module.eq.CONST_PARSIM) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,sps,'-',
     &                   'argument',err)
              else 
                call getarg(i,sps)
              end if
              i=i+1
              if (UsePassedCommandLine) then 
                 call EGETW(PassedCommandLine,pos,aut,'-',
     &                   'argument',err)
              else 
                call getarg(i,aut)
              end if
            end if
          elseif(argument(1:8).eq.'-timeout')then
            if (module.eq.CONST_PARSIM) then
              i=i+1
              if (UsePassedCommandLine) then 
                call EGETW(PassedCommandLine,pos,sTimeout,'-',
     &                   'argument',err)
              else 
                call getarg(i,sTimeout)
              end if

              iTimeoutFlag = 1
            end if
          else
            if (module.eq.CONST_PARTFA) then
C Assume argument is an input file without the -file token.
              write(inf,'(a)') argument(1:lnblnk(argument))
            end if
          end if
        end do parseloop
      end if


C Only give feedback if non-DOS.
        call isunix(unixok)
        if(unixok)then
          if  (module.eq.CONST_PARTFA) then
            write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' with action ',act(1:lnblnk(act))
          else if (module.eq.CONST_PARSFZ) then
            write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone))
          else if (module.eq.CONST_PARSFACT) then
            write(outs,'(10a)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' with action ',act(1:lnblnk(act)),
     &      ' applied to ',actf(1:lnblnk(actf))
          else if (module.eq.CONST_PARRES) then
            outs=' '
            write(outs,'(11a)') 'Starting ',appn(1:lnblnk(appn)),
     &        ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &        inf(1:lnblnk(inf)),' and action ',act(1:lnblnk(act)),
     &       ' in ',aut,' mode.'
          else if (module.eq.CONST_PARSISH) then
            write(outs,'(10a)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone))
     &      ,' with action ',act(1:lnblnk(act))
        else if (module.eq.CONST_PARCLM) then
          outs=' '
          write(outs,'(6a)') 'and action ',act(1:lnblnk(act)),
     &      ' in ',aut,' mode using/making file ',
     &      ascif(1:lnblnk(ascif))
        else if (module.eq.CONST_PARSE2R) then
          write(outs,'(12a)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' for focus zone ',zone(1:lnblnk(zone)),
     &      ' with scene purpose ',aim(1:lnblnk(aim)),
     &      ' with action ',act(1:lnblnk(act))
        else if (module.eq.CONST_PARSIM) then
          outs=' '
          if ( iVerb == 2 ) then
            write(outs,'(6a)') 'Starting ',appn(1:lnblnk(appn)),
     &        ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &        inf(1:lnblnk(inf))
            if(unixok)call edisp248(iuout,outs,90)
            outs=' '
            write(outs,'(7a)') 'and parameters ',sps(1:lnblnk(sps)),
     &        ' in ',aut,' mode and ',browse,' browsing.'
            if(unixok)call edisp248(iuout,outs,90)
          endif
        else if (module.eq.CONST_PARSPRJ) then
          write(outs,'(11a,3f6.1)') 'Starting ',appn(1:lnblnk(appn)),
     &      ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &      inf(1:lnblnk(inf)),' focused on zone ',zone(1:lnblnk(zone))
     &      ,' with action ',act(1:lnblnk(act)),' and data ',data1,
     &      data2,data3
          else
            write(outs,'(8a)') 'Starting ',appn(1:lnblnk(appn)),
     &        ' in mode ',mode(1:lnblnk(mode)),' with file ',
     &        inf(1:lnblnk(inf))
C         call edisp248(iuout,outs,90)
        end if

C Uncomment this line to see feedback.
C       call edisp248(iuout,outs,90)
        return
      endif

C Errors.
   2  call isunix(unixok)
      if(unixok)then
        SELECT CASE (module)
        CASE (CONST_PARPSF)
          ModuleName = 'parpsf'
        CASE (CONST_PARPSFF)
          ModuleName = 'parpsff'
        CASE (CONST_PARSIM)
          ModuleName = 'parsim'
        CASE (CONST_PARRES)
          ModuleName = 'parres'
        CASE (CONST_PARCLM)
          ModuleName = 'parclm'
        CASE (CONST_PARSISH)
          ModuleName = 'parsish'
        CASE (CONST_PARSPRJ)
          ModuleName = 'parsprj'
        CASE (CONST_PARSFZ)
          ModuleName = 'parsfz'
        CASE (CONST_PARTF)
          ModuleName = 'partf'
        CASE (CONST_PARTFA)
          ModuleName = 'partfa'
        CASE (CONST_PARSE2R)
          ModuleName = 'parse2r'
        CASE (CONST_PARSFACT)
          ModuleName = 'parsfact'
        END SELECT
        if(IOS.eq.2)then
          write(6,'(2a)')ModuleName,
     &        ': permission error getting parameters.'
        else
          write(6,'(2a)')ModuleName, ': error extracting parameters.'
        endif
      endif
      return
      END SUBROUTINE GetCommandLineArguments


C************************************************************************************
      END MODULE PARSE_COMMAND_LINE
C************************************************************************************
C************************************************************************************
C************************************************************************************


C Subroutines used by all modules for setting up environemt and
C parsing command lines.
C  SCESPRC: scans the ESP-r dot file (environment)
C  ESCDEF : scans the ESP-r defaults definition file

C parsepar variants: Parse command lines.
C  parpsf: allowa terminal, size and config file arguments
C         to be passed to the program from the invocation line.
C  parpsff: supports terminal, size and two file arguments
C         to be passed to the program from the invocation line.
C  parpsfh: Help message for parpsf.
C  parsim: allows terminal, size, config file, simulation parameter
C         file arguments to be passed
C  parres: allows terminal, size, results file, action and silent arguments
C         to be passed to res
C  parclm: allows terminal, size, input file, action and silent arguments
C         and output (converted) file to be passed to clm
C  parsish: allows the module ish to be passed a terminal, size, config file,
C         zone focus and actions arguments from the invocation line.
C parsfza: allows a terminal, size, config file, zone focus and actions arguments
C         to be passed to the program
C parsprj: allows terminal, size, config file, zone focus and actions arguments
C         (including 3 real data) to be passed to the prj
C parsfz: allows terminal, size, config file, zone focus arguments
C         to be passed to the program
C partf: allows terminal and config file arguments
C         to be passed to the program
C partfa: allows terminal, file and action arguments
C         to be passed to the program
C parse2r: allows terminal, size, config file, scene purpose and actions arguments
C         to be passed to e2r
C parsfact: allows terminal, size, file and actions arguments
C         to be passed to the program
C parcnv parse command line paramters for ecnv.
C FINDAPP: given a file name (string) see if it contains one of the
C          esp-r modules
c***********************************************************************************
c***********************************************************************************
c***********************************************************************************
c***********************************************************************************
      MODULE START_UP
      
      USE PARSE_COMMAND_LINE
C Add Proceedures
      CONTAINS
C***********************************************************************************************************************
C ********* parpsf (equivalent to startpsf.c)
C parpsf allowa terminal, size and config file arguments
C to be passed to the program from the invocation line.  
C***********************************************************************************************************************
      subroutine parpsf(termtype,iappw,iappx,iappy,inf)
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone,act
      character(72) :: actf,ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARPSF
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      
      end subroutine parpsf


C***********************************************************************************************************************
C ********* parpsff
C parpsff supports terminal, size and two file arguments
C to be passed to the program from the invocation line.  
C***********************************************************************************************************************
      subroutine parpsff(termtype,iappw,iappx,iappy,infa,infb)
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: infa, infb
      
      integer :: module, iverb
      real ::  data1,data2,data3
      character(16) :: zone,act
      character(72) :: actf,ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARPSFF
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,infa,infb,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      
      end subroutine parpsff


C***********************************************************************************************************************
C parres allows terminal, size, results file, action and silent arguments
C  to be passed to res from the invocation line.  
C***********************************************************************************************************************
      subroutine parres(termtype,iappw,iappx,iappy,inf,act,aut,actf)
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(72), intent(inout) :: actf
      character(12), intent(inout) :: aut
      character(16), intent(inout) :: act

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone
      character(72) :: ascif
      character(12) :: aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARRES
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine parres


C***********************************************************************************************************************
C ********* parclm
C parclm allows terminal, size, input file, action and silent arguments
C and output (converted) file to be passed to clm from the invocation line.  
C***********************************************************************************************************************
      subroutine parclm(termtype,iappw,iappx,iappy,inf,act,aut,ascif)
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(16), intent(inout) :: act
      character(12), intent(inout) :: aut
      character(72), intent(inout) :: ascif

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone
      character(72) :: actf
      character(12) :: aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARCLM
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      
      end subroutine parclm


C***********************************************************************************************************************
C ********* parsish
C parsish allows the module ish to be passed a terminal, size, config file,
C zone focus and actions arguments from the invocation line.  
C***********************************************************************************************************************
      subroutine parsish(termtype,iappw,iappx,iappy,inf,zone,act,
     &  ascif)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(16), intent(inout) :: act, zone
      character(72), intent(inout) :: ascif

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(72) :: actf
      character(12) :: aim, aut
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARSISH
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      
      end subroutine parsish


C***********************************************************************************************************************
C ********* parsprj 
C parsprj allows terminal, size, config file, zone focus and actions arguments
C (including 3 real data) to be passed to the prj from the invocation line.  
C***********************************************************************************************************************
      subroutine parsprj(termtype,iappw,iappx,iappy,inf,zone,act,
     &        data1,data2,data3,iverb)

      IMPLICIT NONE
C Declare  calling parameter types
      real, intent(inout)     ::  data1,data2,data3
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy,iverb
      character(144), intent(inout)  :: inf
      character(16), intent(inout) :: act, zone

C Local variables for GetCommandLineArguments.
      integer :: module
      character(144) :: inf2
      character(72) :: actf,ascif
      character(12) :: aim, aut
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARSPRJ
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine parsprj


C ********* parsfz (equivalent to startsfz.c)
C parsfz allows terminal, size, config file, zone focus arguments
C to be passed to the program from the invocation line.  

      subroutine parsfz(termtype,iappw,iappx,iappy,inf,zone)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(16), intent(inout)  :: zone

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: act
      character(72) :: actf,ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARSFZ
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine parsfz


C***********************************************************************************************************************
C ********* partf (equivalent to startup.c)
C partf allows terminal and config file arguments
C to be passed to the program from the invocation line.  
C***********************************************************************************************************************
      subroutine partf(termtype,inf)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype
      character(144), intent(inout)  :: inf

C Local variables for GetCommandLineArguments.
      integer :: module, iverb,iappw,iappx,iappy
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone,act
      character(72) :: actf,ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARTF
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine partf


C***********************************************************************************************************************
C ********* partfa (equivalent to partf with an action parameter)
C partfa allows terminal, file and action arguments
C  to be passed to the program from the invocation line.  
C  Logic allows for the -file token to be optional.
C***********************************************************************************************************************
      subroutine partfa(termtype,inf,act)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype
      character(144), intent(inout)  :: inf
      character(16), intent(inout)  :: act

C Local variables for GetCommandLineArguments.
      integer :: module, iverb,iappw,iappx,iappy
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone
      character(72) :: actf,ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARTFA
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine partfa


C***********************************************************************************************************************
C ********* parse2r 
C parse2r allows terminal, size, config file, scene purpose and actions arguments
C  to be passed to e2r from the invocation line.  
C***********************************************************************************************************************
      subroutine parse2r(termtype,iappw,iappx,iappy,inf,zone,aim,act,
     &  actf,browse)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(16), intent(inout) :: zone,act
      character(12), intent(inout) :: aim
      character(72), intent(inout) :: actf
      character(4), intent(inout) :: browse

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(72) :: ascif
      character(12) :: aut
      character(8) :: sps

      module = CONST_PARSE2R
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine parse2r


C***********************************************************************************************************************
C ********* parsim
C parsim: allows terminal, size, config file, simulation parameter file arguments
C  to be passed to the program from the invocation line.  
C***********************************************************************************************************************

      subroutine parsim(iverb,termtype,iappw,iappx,iappy,inf,browse,
     &  sps,aut,actf)
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  iverb, termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(72), intent(inout) :: actf
      character(12), intent(inout) :: aut
      character(4), intent(inout) :: browse
      character(8), intent(inout) :: sps

C Local variables for GetCommandLineArguments.
      integer :: module
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone,act
      character(72) :: ascif
      character(12) :: aim

      module = CONST_PARSIM
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      
      end subroutine parsim


C***********************************************************************************************************************
C ********* parsfact
C parsfact allows terminal, size, file and actions arguments
C  to be passed to the program from the invocation line.  
C***********************************************************************************************************************

      subroutine parsfact(termtype,iappw,iappx,iappy,inf,act,actf)
      
      IMPLICIT NONE
C Declare  calling parameter types
      integer, intent(inout)  ::  termtype,iappw,iappx,iappy
      character(144), intent(inout)  :: inf
      character(16), intent(inout) :: act
      character(72), intent(inout) :: actf

C Local variables for GetCommandLineArguments.
      integer :: module, iverb
      real ::  data1,data2,data3
      character(144) :: inf2
      character(16) :: zone
      character(72) :: ascif
      character(12) :: aut,aim
      character(4) :: browse
      character(8) :: sps

      module = CONST_PARSFACT
      call GetCommandLineArguments(module,termtype,iappw,iappx,
     &             iappy,inf,inf2,zone,act,actf,aut,ascif,aim,browse,
     &             iverb,sps,data1,data2,data3)
      end subroutine parsfact


C***********************************************************************************************************************

      END MODULE START_UP
C***********************************************************************************************************************
C***********************************************************************************************************************
C***********************************************************************************************************************
C***********************************************************************************************************************
C***********************************************************************************************************************
C***********************************************************************************************************************
C parcnv parse command line paramters for ecnv.
      subroutine parcnv(iverb,conv,iobs,ichop,itmprm,iform,oform,
     &                   inf,ouf,ucfg)
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      integer iargc,m
      integer termtype
      integer i,iverb,itmprm,iobs,ichop
      real conv
      character argument*72,prog*48,inf*144,outs*248,appn*24
      character iform*24,oform*24,ouf*144,ucfg*72,mode*8

      inf  = 'UNKNOWN'
      iform = 'UNKNOWN'
      oform = 'UNKNOWN'
      ouf  = 'UNKNOWN'
      ucfg  = 'UNKNOWN'
      conv = 0.001
      iverb = 0
      itmprm = 1
      iobs  = 0
      ichop = 0

C Get number of arguments and command name.
      termtype = -1
      mode = 'text'
      m = iargc()
      i = 0
      call getarg(i,prog)
      call findapp(prog,appn)

C If one parameter.
      if(m.eq.0)then
        stop
      elseif(m.ge.1)then
  41    i= i+1
        if(i.gt.m)goto 42
        call getarg(i,argument)
        if(argument(1:5).eq.'-help')then
      call edisp(iuout,'The product model filter. ')
      call edisp(iuout,' Use: [-v] [{-mm|-metre}] ')
      call edisp(iuout,'  -if {esp|dxf|ww} -in <file> ')
      call edisp(iuout,
     &  '  -of {esp|dxf|viewer|xfig|vrml} -out <file>')
      call edisp(iuout,' ')
      call edisp(iuout,'Where options are:')
      call edisp(iuout,' -if [esp|dxf|ww] = input format')
      call edisp(iuout,'      (dxf = AutoCAD V11-12)')
      call edisp(iuout,'      (ww = ww graphic commands)')
      call edisp(iuout,' -in [file]  = input file')
      call edisp(iuout,
     &  ' -of [esp|dxf|viewer|xfig|vrml] = dest format')
      call edisp(iuout,' -out [file] = output file')
      call edisp(iuout,' -v          = verbose mode')
      call edisp(iuout,' -mm -metre  = dxf coords in mm (default)')
      call edisp(iuout,' -keep       = retain scratch files')
      call edisp(iuout,' -obs        = include esp-r obstructions')
      call edisp(iuout,' ')
      call edisp(iuout,'The following combinations are supported:')
      call edisp(iuout,'  input format  output format')
      call edisp(iuout,'  esp-r         dxf, viewer, vrml')
      call edisp(iuout,'  dxf           esp-r, viewer')
      call edisp(iuout,' ')
      call edisp(iuout,'If converting to esp-r then an additional') 
      call edisp(iuout,'parameter is allowed:')
      call edisp(iuout,' -u [upgrade] when updating a model from ')
      call edisp(iuout,'    registration stage.')
      call edisp(iuout,' ')
      call edisp(iuout,' -help :this help message.')
      call edisp(iuout,' ')
          stop
        elseif(argument(1:2).eq.'-v')then
          iverb = 2   ! tell application to run with debug on
        elseif(argument(1:3).eq.'-mm')then
          conv = 0.001
        elseif(argument(1:6).eq.'-metre')then
          conv = 1.00
        elseif(argument(1:4).eq.'-obs')then
          iobs = 1
        elseif(argument(1:5).eq.'-chop')then
          ichop = 1
        elseif(argument(1:5).eq.'-keep')then
          itmprm = 0
        elseif(argument(1:3).eq.'-in')then
          i=i+1
          call getarg(i,inf)
        elseif(argument(1:4).eq.'-out')then
          i=i+1
          call getarg(i,ouf)
        elseif(argument(1:2).eq.'-u')then
          i=i+1
          call getarg(i,ucfg)
          call edisp(iuout,'Will attempt to update configuration')
        elseif(argument(1:3).eq.'-if')then
          i=i+1
          call getarg(i,iform)
        elseif(argument(1:3).eq.'-of')then
          i=i+1
          call getarg(i,oform)
        endif
        goto 41

  42    continue
        write(outs,'(4a)') 'Starting ecnv in ',
     &    oform(1:lnblnk(oform)),' mode with in file ',
     &    inf(1:lnblnk(inf))
        call edisp248(iuout,outs,100)
        write(outs,'(3a)') 'and output file ',ouf(1:lnblnk(ouf)),'.'
        call edisp248(iuout,outs,100)
        return
      endif
      end

C ******* FINDAPP
C FINDAPP given a file name (string) see if it contains one of the esp-r module names.
      SUBROUTINE findapp(string,appn)
      
      integer lnblnk  ! function definition

      CHARACTER*(*) string,appn
      logical unixok

C Clear variables and get string lengths.
      ilfilen = LEN(appn)
      ilstr = LEN(string)
      call isunix(unixok)

C Scan for last position of a `/' (UNIX) or '\' (NT).
      islash = 0
      DO 99 I=1,LEN(string)

C Comment out UNIX or NT version as required.
        if(unixok)then
          if(string(I:I).eq.'/')islash=I
        else
         if(string(I:I).eq.'\\')islash=I
C         if(string(I:I).eq.'\')islash=I
        endif
        if(ichar(string(I:I)).lt.32)goto 100
 99   CONTINUE

C If islash = 0 then no root.
100   if(islash.eq.0)then
        if(ilstr.ge.ilfilen)then
          write(appn,'(a)')string(1:ilfilen)
        elseif(ilstr.lt.ilfilen)then
          write(appn,'(a)')string(1:lnblnk(string))
        endif
      else

C If islash > 0 then root and file name
        is=islash+1
        write(appn,'(a)') string(is:lnblnk(string))
      endif

      RETURN
      END
C ******************** scesprc 
C scesprc scans the ESP-r dot file which should be located in the
C users home folder or in the esp-r distribution.  If not found
C then some defaults are set.
C Note: take this opportunity to instanciate common block stddbpath
C while we have information on where esp-r was installed.

      SUBROUTINE scesprc(LFDEF,IFDEF,ITRC,IER)
C Use espriou.h for climatelist file name and defdbfl.
#include "building.h"
#include "model.h"
#include "espriou.h"
#include "esprdbfile.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

C External text editor.
      common/texted/tedlbl,teditor

C External document previewer.
      common/textpv/tprevlbl,tpreview

C External figure generator.
      common/figtool/figlbl,figexe

C External graphing tool.
      common/graphtool/grflbl,grfexe

C External folder browser.
      common/folderbr/foldlbl,foldexe

C A set of possible image formats which can be accepted within
C a model configuration file. These definitions are held in the
C so-called esprc file. To add additional image types and/or 3rd
C party display applications edit the ESP-r Install script which
C creates the esprc file.
C   imgtyp is the number of different image formats supported
C   fmttag (4 char) is a tag for each image formt (e.g. GIF XBM)
C   fmtexe (20 char) is the application name used to display
C          images of type fmttag.
      common/showimg/imgtyp,fmttag(5),fmtexe(5)

C A set of possible cad packages: labels and executables.
      common/cad3rd/cadlbl,cadexe,cadfmt

C Report generator: label and executable.
      common/rep3rd/replbl,repexe

C Save journal. journoo = ON or OFF
      common/journopt/journio,iuj,journcmd,jfile

C Graphic capture, window dump & text print: label, command.
      common/gprint/gprlbl,gprcmd
      common/gxwd/gxwdlbl,gxwdcmd
      common/tprint/tprlbl,tprcmd

C Where validation tests are kept.
      character vldlbl*20,vldfl*96
      common/vldlst/vldlbl,vldfl

      logical localdot,unixok,dll

      character*(*) LFDEF
      CHARACTER OUTSTR*124,WORD*20
      character outs*124,outs248*248
      character fmttag*4,fmtexe*20,cadlbl*20,cadexe*20,cadfmt*4
      character replbl*20,repexe*20
      character gprlbl*20,gprcmd*48
      character gxwdlbl*20,gxwdcmd*60
      character tprlbl*20,tprcmd*48
      character tedlbl*20,teditor*20
      character tprevlbl*20,tpreview*48
      character figlbl*20,figexe*48
      character grflbl*20,grfexe*48
      character foldlbl*20,foldexe*48
      character journcmd*20,jfile*72

C Altdot is an alternative location for the esprc file in
C the installation folder.
      character altdot*144
#include "espinstalldir.h"

C Check if running in dll mode, if so clear most of the
C initial values because they will not be used.
      call isadll(dll)
      if(dll)then
        teditor=' '
        tedlbl='no word processor'
        tpreview=' '
        tprevlbl='no document preview'
        figlbl='diagram editor'
        figexe=' '
        grflbl='no graphing tool'
        grfexe=' '
        foldlbl='no folder browser'
        foldexe=' '
        cadlbl='no CAD package'
        cadexe=' '
        cadfmt='DXF'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        journcmd=' '
        jfile=' '
        imgtyp=1
        fmttag(1)='GIF'
        fmtexe(1)='display '
        gxwdlbl='no graphic dump'
      else

C Assume initial values for interactive use. Default to putting
C temporary files in the users home folder.
        teditor='vi'
        tedlbl='word processor'
        tpreview='GROFF -p '
        tprevlbl='document previewer'
        figlbl='diagram editor'
        figexe='xfig'
        grflbl='graphing tool'
        grfexe='xvgr'
        foldlbl='folder browser'
        foldexe='xfs '
        cadlbl='CAD package'
        cadexe=' '
        cadfmt='DXF'
        replbl='No report generator'
        repexe='UNKNOWN'
        gprcmd='UNKNOWN'
        journcmd=' '
        jfile=' '

C Assume two basic image types and that the application window dump
C will use xwud.
        imgtyp=1
        fmttag(1)='GIF'
        fmtexe(1)='display '
        gxwdlbl='Appl window dump'
      endif

C Set defaults and exemplars depending on whether Unix or NT.
      call isunix(unixok)
      ln=lnblnk(instpath)
      if(unixok)then
        write(gxwdcmd,'(3a)')'xwud -out ',upath(1:lnblnk(upath)),
     &  '/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a,a)') upath(1:lnblnk(upath)),'/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
        vldlbl='Validation standards'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
        write(standarddbpath,'(2a)') instpath(1:ln),'/databases'
        write(standardclmpath,'(2a)') instpath(1:ln),'/climate'
      else
        write(gxwdcmd,'(a)')'xwud -out c:/temp/gxwd.xwd'
        tprlbl='Text dump'
        write(tprcmd,'(a)') 'c:/temp/tx_dump'
        exemlbl='Standard exemplars'
        write(exemfl,'(2a)') instpath(1:ln),'/training/exemplars'
        dfdblbl='Standard defaults'
        write(defdbfl,'(2a)') instpath(1:ln),'/default'
        vldlbl='Validation tests'
        write(vldfl,'(2a)') instpath(1:ln),'/validation/stds_list'
        cdblbl='Standard climates'
        write(cdblfil,'(2a)') instpath(1:ln),'/climate/climatelist'
        write(altdot,'(2a)') instpath(1:ln),'/esprc'
        write(standarddbpath,'(2a)') instpath(1:ln),'/databases'
        write(standardclmpath,'(2a)') instpath(1:ln),'/climate'
      endif

C Debug.
C      write(6,*) 'standard databases are at ',
C     &  standarddbpath(1:lnblnk(standarddbpath))

C If running in dll mode skip reading .esprc file.
      if(dll)then
        return
      endif

C Open the esprc file, return if not found.
      IER=0
      CALL ERPFREE(IFDEF,ISTAT)

C Debug.
C      write(6,*) 'lfdef is ',lfdef

      call FPOPEN(IFDEF,ISTAT,1,0,LFDEF)
      IF(ISTAT.LT.0)THEN
        CALL ERPFREE(IFDEF,ISTAT)
        call FPOPEN(IFDEF,ISTAT,1,0,altdot)
        IF(ISTAT.LT.0)THEN
          call edisp(iuout,
     &    '  No preferences (esprc or .esprc) file found so setting')
          call edisp(iuout,
     &    '  default exemplars, CAD, report & print links.')
          IER=1
          CALL ERPFREE(IFDEF,ISTAT)
          RETURN
        else
          localdot=.false.
          esprc=altdot  ! assign the install folder file as the esprc file.
          if(itrc.ge.0) call edisp(iuout,
     &      '  Preferences (esprc) file - standard version.')
          write(currentfile,'(a)') esprc(1:lnblnk(esprc))
        endif
      else
        localdot=.true.
        if(itrc.ge.0) call edisp(iuout,
     &    '  Preferences (.esprc) file - users version.')
        write(currentfile,'(a)') lfdef(1:lnblnk(lfdef))
      ENDIF

C Read '*ESPRC' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'ESPRC tag',IER)
      if(OUTSTR(1:6).ne.'*ESPRC')then
        if(localdot)then
          write(outs248,'(2a)')LFDEF(:lnblnk(LFDEF)),
     &      ' not an .esprc file!'
        else
          write(outs248,'(2a)')altdot(:lnblnk(altdot)),
     &      ' not a .esprc file!'
        endif
        if(itrc.ge.1)then
          call edisp(iuout,OUTSTR)
          call edisp248(iuout,outs248,100)
        endif
        IER=1
        CALL ERPFREE(IFDEF,ISTAT)
        RETURN
      endif

C Take image types from the file.
      imgtyp=0


C If '*ESPRC' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'dot sequences',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','dot sequences',IFLAG)
      if(WORD(1:5).eq.'*gprn')then

C Capture rectangular area of screen via capexgf is in wwlib.c
        CALL EGETP(OUTSTR,K,gprlbl,'W','rect area lbl',IER)
        CALL EGETP(OUTSTR,K,gprcmd,'W','rect area exe',IER)
        write(outs,'(a,a,2x,a)')' Rectangular area grab:',gprlbl,gprcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capexgf(gprcmd)
      elseif(WORD(1:5).eq.'*gxwd')then

C Screen capture commands - dump whole screem via capexall in wwlib.c
        CALL EGETP(OUTSTR,K,gxwdlbl,'W','screen dump lbl',IER)
        CALL EGETP(OUTSTR,K,gxwdcmd,'W','screen dump exe',IER)
        write(outs,'(a,a,2x,a)')' Screen dump: ',gxwdlbl,gxwdcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capexall(gxwdcmd)
      elseif(WORD(1:5).eq.'*tprn')then

C Caputre text buffer to a file, capextf is in wwlib.c
        CALL EGETP(OUTSTR,K,tprlbl,'W','text print lbl',IER)
        CALL EGETP(OUTSTR,K,tprcmd,'W','text print file',IER)
        write(outs,'(a,a,2x,a)')' Local text print: ',tprlbl,tprcmd
        if(itrc.gt.1)call edisp(iuout,outs)
        call capextf(tprcmd)
      elseif(WORD(1:4).eq.'*cad')then
        CALL EGETP(OUTSTR,K,cadlbl,'W','cad lbl',IER)
        CALL EGETP(OUTSTR,K,cadexe,'W','cad exe',IER)
        CALL EGETW(OUTSTR,K,cadfmt,'W','cad fnt',IER)
        write(outs,'(a,a,2x,a,2x,a)')' Local CAD: ',cadlbl,cadexe,cadfmt
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:8).eq.'*journal')then

C Turn journaling on/off.
        CALL EGETW(OUTSTR,K,journcmd,'W','on/off',IFLAG)
        if(journcmd(1:2).eq.'ON'.or.journcmd(1:2).eq.'on')then
          journio=1
        else
          journio=0
        endif
      elseif(WORD(1:14).eq.'*image_display')then

C Add to the initial assumptions about display software.
        imgtyp=imgtyp+1
        CALL EGETP(OUTSTR,K,fmttag(imgtyp),'W','image fmt',IER)
        CALL EGETP(OUTSTR,K,fmtexe(imgtyp),'W','img exe',IER)
        write(outs,'(4a)')' Images : ',fmttag(imgtyp),' via ',
     &    fmtexe(imgtyp)
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*editor')then
        CALL EGETP(OUTSTR,K,tedlbl,'W','manual editor lbl',IER)
        CALL EGETP(OUTSTR,K,teditor,'W','editor exe',IER)
        write(outs,'(4a)')' Editor: ',tedlbl,' via ',teditor
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:11).eq.'*docpreview')then
        CALL EGETP(OUTSTR,K,tprevlbl,'W','document previewer lbl',IER)
        CALL EGETP(OUTSTR,K,tpreview,'W','doc previewer exe',IER)
        write(outs,'(4a)')' Preview: ',tprevlbl,' via ',tpreview
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:7).eq.'*figure')then
        CALL EGETP(OUTSTR,K,figlbl,'W','figure editor lbl',IER)
        CALL EGETP(OUTSTR,K,figexe,'W','figure editor exe',IER)
        write(outs,'(4a)')' Preview: ',figlbl,' via ',figexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*graphing')then
        CALL EGETP(OUTSTR,K,grflbl,'W','graphing tool lbl',IER)
        CALL EGETP(OUTSTR,K,grfexe,'W','graphing tool exe',IER)
        write(outs,'(4a)')' Graphing: ',grflbl,' via ',grfexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:9).eq.'*folderbr')then
        CALL EGETP(OUTSTR,K,foldlbl,'W','folder br tool lbl',IER)
        CALL EGETRM(OUTSTR,K,foldexe,'W','folder tool exe',IER)
        write(outs,'(4a)')' Folder browsing: ',foldlbl,' via ',foldexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:10).eq.'*exemplars')then
        CALL EGETP(OUTSTR,K,exemlbl,'W','exemplars list lbl',IER)
        CALL EGETRM(OUTSTR,K,exemfl,'W','exemplars file',IER)
        write(outs248,'(2a)')'  Exemplars (model list) - ',exemfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:12).eq.'*db_defaults')then
        CALL EGETP(OUTSTR,K,dfdblbl,'W','def db list lbl',IER)
        CALL EGETRM(OUTSTR,K,defdbfl,'W','default db file',IER)
        write(outs248,'(2a)')'  Default file - ',defdbfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:16).eq.'*validation_stds')then
        CALL EGETP(OUTSTR,K,vldlbl,'W','validation list lbl',IER)
        CALL EGETRM(OUTSTR,K,vldfl,'W','validation stds file',IER)
        write(outs248,'(4a)')' Validation: ',vldlbl,' in ',vldfl
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:12).eq.'*db_climates')then
        CALL EGETP(OUTSTR,K,cdblbl,'W','clm db list lbl',IER)
        CALL EGETRM(OUTSTR,K,cdblfil,'W','clm db list file',IER)
        write(outs248,'(2a)')'  Weather (list) - ',cdblfil
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      elseif(WORD(1:11).eq.'*report_gen')then
        CALL EGETP(OUTSTR,K,replbl,'W','report gen lbl',IER)
        CALL EGETRM(OUTSTR,K,repexe,'W','report gen exe',IER)
        write(outs,'(4a)')' Reports: ',replbl,' via ',repexe
        if(itrc.gt.1)call edisp(iuout,outs)
      elseif(WORD(1:5).eq.'*year')then
      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown dot command in... ',outstr,'W')
        goto 44
      endif

C Loop back for next command.
      goto 44

C Close file before exiting.
   45 CONTINUE
      if(localdot)then
        write(outs248,'(3a)') 'Scanning ',
     &         LFDEF(1:lnblnk(LFDEF)),' definitions.'
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      else
        write(outs248,'(3a)') 'Standard settings in: ',
     &         altdot(1:lnblnk(altdot)),' successfully scanned...'
        if(itrc.gt.1)call edisp248(iuout,outs248,100)
      endif
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END

C ******************** ESCDEF
C ESCDEF scans the ESP-r defaults definition file and returns
C the default file and database names via common block.
C It uses espinstalldir.h which was created by the Install
C script with the current esp-r install directory held
C in a string instpath.

C ESCDEF also scans default constructions to be offered
C for code complience purposes or for use defined uses.
      SUBROUTINE ESCDEF(IFDEF,act,IER)
#include "esprdbfile.h"
C Use espriou.h for climatelist file name and defdbfl.
#include "espriou.h"

      integer lnblnk  ! function definition
      character act*1 ! s for silent - for normal

      COMMON/OUTIN/IUOUT,IUIN

C Defaults.
      character*96 DFCFG,DFCTL,DEFRLB,DAPROB,DAFRES,DPNF
      COMMON/DEFLT2/DFCFG,DFCTL,DEFRLB,DAFRES,DAPROB,DPNF
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL
      common/deflt4/dinstpath

C Construction defaults:
C cdtype (char*8) identifier for each set of constructions
C cdtypelabel (char*32) menu select for each set of construcitons
C cdextwall (char*32) external wall
C cdextglz (char*32) external glazing/window
C cdextfrm (char*32) external frame
C cdextdisp (char*32) external display glazing
C cdextdoor (char*32) external door
C cdextvdoor (char*32) external vehicle door
C cdextflrf (char*32) external flat roof
C cdextslrf (char*32) external slope roof
C cdextskyl (char*32) external skylight
C cdextfnd (char*32) external foundation
C cdintfloor (char*32) internal floor
C cdintceil (char*32) internal ceiling
C cdintptn (char*32) internal partition
C cdintdoor (char*32) internal door
C cdintglz (char*32) internal glazing/window
      character cdtype*8,cdtypelabel*32,cdextwall*32,cdextglz*32
      character cdextfrm*32,cdextdisp*32,cdextdoor*32,cdextvdoor*32
      character cdextflrf*32,cdextslrf*32,cdextskyl*32,cdextfnd*32
      character cdintfloor*32,cdintceil*32,cdintptn*32,cdintdoor*32
      character cdintglz*32
      integer icdtypes
      common/deflt5t/icdtypes
      common/deflt5/cdtype(5),cdtypelabel(5),cdextwall(5),cdextglz(5),
     &  cdextfrm(5),cdextdisp(5),cdextdoor(5),cdextvdoor(5),
     &  cdextflrf(5),cdextslrf(5),cdextskyl(5),cdextfnd(5),
     &  cdintfloor(5),cdintceil(5),cdintptn(5),cdintdoor(5),cdintglz(5)

      CHARACTER OUTSTR*124,WORD*20,outs248*248

C << consider increasing size of these strings >>
      character*72 DFCFD
      character*72 DECMPDBFL,DICONDBFL
      character dinstpath*60,di*60
      logical unixok,dll
#include "espinstalldir.h"

C Assume default db names (in case of failure to find defaults
C file or failure reading it).

C << make a pass sorting out these default file names >>
      call isunix(unixok)
      if(unixok)then
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/cfg/bld_basic.cfg'
        write(DFCTL,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/ctl/bld_basic.ctl'
        write(DAPROB,'(2a)') di(1:min(ln,38)),
     &    'training/basic/nets/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:min(ln,31)),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:min(ln,45)),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:min(ln,51)),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:min(ln,51)),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:min(ln,51)),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:min(ln,51)),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:min(ln,51)),
     &    '/databases/material.db3.a'
        write(DFMUL,'(2a)') di(1:min(ln,51)),'/databases/multicon.db3'
        write(DOPTDB,'(2a)') di(1:min(ln,51)),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/profiles.db2.a'
        write(DPCDB,'(2a)') di(1:min(ln,51)),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:min(ln,51)),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:min(ln,51)),'/databases/mould.db1'
        write(DSBEM,'(2a)') di(1:min(ln,51)),'/databases/SBEM.db1'
        write(DCFCDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/cfclayers.db1.a'
        write(DPREDEF,'(2a)') di(1:min(ln,51)),
     &    '/databases/predefined.db1'
      else
C        dinstpath = 'c:/esru/esp-r'
        write(dinstpath,'(a)') instpath(1:lnblnk(instpath))
        write(di,'(a)') instpath(1:lnblnk(instpath))
        ln=lnblnk(di)
        write(DFCFG,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/cfg/bld_basic.cfg'
        write(DFCTL,'(2a)') di(1:min(ln,34)),
     &    '/training/basic/ctl/bld_basic.ctl'
        write(DAPROB,'(2a)') di(1:min(ln,38)),
     &    'training/basic/nets/bld_basic_af1.afn'
        write(DPNF,'(2a)') di(1:min(ln,31)),
     &    '/training/plant/vent_simple/cfg/vent.cfg'
        write(DFCFD,'(2a)') di(1:min(ln,45)),
     &    '/training/cfd/template.dfd'
        write(DEFRLB,'(2a)') di(1:min(ln,51)),'/databases/test.res'
        write(DAFRES,'(2a)') di(1:min(ln,51)),'/databases/test.mfr'
        write(DCLIM,'(2a)') di(1:min(ln,51)),'/climate/clm67'
        write(DAPRES,'(2a)') di(1:min(ln,51)),'/databases/pressc.db1'
        write(DFCON,'(2a)') di(1:min(ln,51)),
     &    '/databases/material.db3.a'
        write(DFMUL,'(2a)') di(1:min(ln,51)),'/databases/multicon.db3'
        write(DOPTDB,'(2a)') di(1:min(ln,51)),'/databases/optics.db2'
        write(DPRFDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/profiles.db2.a'
        write(DPCDB,'(2a)') di(1:min(ln,51)),'/databases/plantc.db1'
        write(DECMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/elcomp.db1'
        write(DMCMPDBFL,'(2a)') di(1:min(ln,51)),'/databases/mscomp.db1'
        write(DICONDBFL,'(2a)') di(1:min(ln,51)),'/databases/icons.db1'
        write(dmdbnam,'(2a)') di(1:min(ln,51)),'/databases/mould.db1'
        write(DSBEM,'(2a)') di(1:min(ln,51)),'/databases/SBEM.db1'
        write(DCFCDB,'(2a)') di(1:min(ln,51)),
     &    '/databases/cfclayers.db1.a'
        write(DPREDEF,'(2a)') di(1:min(ln,51)),
     &    '/databases/predefined.db1'
      endif

C << ?? Should there be default values for constructions? >>
      icdtypes = 0

C Check if running in dll mode, if so skip reading defaults file.
      call isadll(dll)
      if(dll)then
        return
      endif

C Open the defaults file, return if not found.
      IER=0
      CALL EFOPSEQ(IFDEF,defdbfl,1,IER)
      IF(IER.LT.0)THEN

C Application asked for a silent.
        if(act.eq.'s')then
          IER=1
          RETURN
        else
          write(outs248,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &     ' was not found (assuming standard databases).'
          call edisp248(IUOUT,outs248,100)
          IER=1
          RETURN
        endif
      ENDIF

C Read '*ESP-r Defaults' tag.
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'default tag',IER)
      if(OUTSTR(1:15).ne.'*ESP-r Defaults')then
        write(outs248,'(3a)') 'The file ',defdbfl(1:lnblnk(defdbfl)),
     &   ' is not a defaults file.'
        call edisp248(IUOUT,outs248,100)
        IER=1
        RETURN
      endif

C If '*ESP-r Defaults' found then read db until '*end' found.
  44  continue
      CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'def names',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','def names',IFLAG)
      if(WORD(1:5).eq.'*ipth')then
        CALL EGETRM(OUTSTR,K,dinstpath,'W','instal folder',IER)
      elseif(WORD(1:4).eq.'*cfg')then
        CALL EGETRM(OUTSTR,K,DFCFG,'W','def config',IER)
      elseif(WORD(1:4).eq.'*ctl')then
        CALL EGETRM(OUTSTR,K,DFCTL,'W','def control',IER)
      elseif(WORD(1:4).eq.'*mfn')then
        CALL EGETRM(OUTSTR,K,DAPROB,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*pnf')then
        CALL EGETRM(OUTSTR,K,DPNF,'W','def mass flow net',IER)
      elseif(WORD(1:4).eq.'*dfd')then
        CALL EGETRM(OUTSTR,K,DFCFD,'W','def domain flow definit.',
     &       IER)
      elseif(WORD(1:4).eq.'*res')then
        CALL EGETRM(OUTSTR,K,DEFRLB,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*mfr')then
        CALL EGETRM(OUTSTR,K,DAFRES,'W','def results lib',IER)
      elseif(WORD(1:4).eq.'*prm')then
        CALL EGETRM(OUTSTR,K,DFCON,'W','def material db',IER)
      elseif(WORD(1:4).eq.'*mlc')then
        CALL EGETRM(OUTSTR,K,DFMUL,'W','def multilayer db',IER)
      elseif(WORD(1:4).eq.'*opt')then
        CALL EGETRM(OUTSTR,K,DOPTDB,'W','def optical db',IER)
      elseif(WORD(1:4).eq.'*prs')then
        CALL EGETRM(OUTSTR,K,DAPRES,'W','def pressure db',IER)
      elseif(WORD(1:4).eq.'*evn')then
        CALL EGETRM(OUTSTR,K,DPRFDB,'W','def profiles db',IER)
      elseif(WORD(1:4).eq.'*clm')then
        CALL EGETRM(OUTSTR,K,DCLIM,'W','def climate db',IER)
      elseif(WORD(1:4).eq.'*pdb')then
        CALL EGETRM(OUTSTR,K,DPCDB,'W','def plant comp db',IER)
      elseif(WORD(1:5).eq.'*ecdb')then
        CALL EGETRM(OUTSTR,K,DECMPDBFL,'W','def elec comp db',IER)
      elseif(WORD(1:5).eq.'*mcdb')then
        CALL EGETRM(OUTSTR,K,DMCMPDBFL,'W','def misc comp db',IER)
      elseif(WORD(1:5).eq.'*icdb')then
        CALL EGETRM(OUTSTR,K,DICONDBFL,'W','default icons',IER)
      elseif(WORD(1:5).eq.'*mldb')then
        CALL EGETRM(OUTSTR,K,dmdbnam,'W','default mycotoxin db',IER)
      elseif(WORD(1:5).eq.'*sbem')then
        CALL EGETRM(OUTSTR,K,DSBEM,'W','default SBEM db',IER)
      elseif(WORD(1:6).eq.'*cfcdb')then
        CALL EGETRM(OUTSTR,K,DCFCDB,'W','def CFC db',IER)
      elseif(WORD(1:14).eq.'*constructions')then
  46    continue
        CALL STRIPC(IFDEF,OUTSTR,0,ND,1,'construction tags',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','constr tags',IFLAG)
        if(WORD(1:4).eq.'*set')then
          icdtypes=icdtypes+1
          CALL EGETW(OUTSTR,K,cdtype(icdtypes),'W',
     &      'construction set type',IER)
          CALL EGETRM(OUTSTR,K,cdtypelabel(icdtypes),'W',
     &      'construction set type menu',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_wall')then
          CALL EGETW(OUTSTR,K,cdextwall(icdtypes),'W',
     &      'construction set ext wall',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_glz')then
          CALL EGETW(OUTSTR,K,cdextglz(icdtypes),'W',
     &      'construction set ext glazing',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_frm')then
          CALL EGETW(OUTSTR,K,cdextfrm(icdtypes),'W',
     &      'construction set ext frame',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_disp')then
          CALL EGETW(OUTSTR,K,cdextdisp(icdtypes),'W',
     &      'construction set ext display glazing',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_door')then
          CALL EGETW(OUTSTR,K,cdextdoor(icdtypes),'W',
     &      'construction set ext door',IER)
          goto 46
        elseif(WORD(1:10).eq.'*ext_vdoor')then
          CALL EGETW(OUTSTR,K,cdextvdoor(icdtypes),'W',
     &      'construction set ext vehicle door',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_flrf')then
          CALL EGETW(OUTSTR,K,cdextflrf(icdtypes),'W',
     &      'construction set ext flat roof',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_slrf')then
          CALL EGETW(OUTSTR,K,cdextslrf(icdtypes),'W',
     &      'construction set ext flat roof',IER)
          goto 46
        elseif(WORD(1:9).eq.'*ext_skyl')then
          CALL EGETW(OUTSTR,K,cdextskyl(icdtypes),'W',
     &      'construction set ext skylight',IER)
          goto 46
        elseif(WORD(1:8).eq.'*ext_fnd')then
          CALL EGETW(OUTSTR,K,cdextfnd(icdtypes),'W',
     &      'construction set ext foundation',IER)
          goto 46
        elseif(WORD(1:10).eq.'*int_floor')then
          CALL EGETW(OUTSTR,K,cdintfloor(icdtypes),'W',
     &      'construction set internal floor',IER)
          goto 46
        elseif(WORD(1:9).eq.'*int_ceil')then
          CALL EGETW(OUTSTR,K,cdintceil(icdtypes),'W',
     &      'construction set internal floor',IER)
          goto 46
        elseif(WORD(1:8).eq.'*int_ptn')then
          CALL EGETW(OUTSTR,K,cdintptn(icdtypes),'W',
     &      'construction set internal partition',IER)
          goto 46
        elseif(WORD(1:9).eq.'*int_door')then
          CALL EGETW(OUTSTR,K,cdintdoor(icdtypes),'W',
     &      'construction set internal door',IER)
          goto 46
        elseif(WORD(1:8).eq.'*int_glz')then
          CALL EGETW(OUTSTR,K,cdintglz(icdtypes),'W',
     &      'construction set internal glazing',IER)
          goto 46
        elseif(WORD(1:8).eq.'*end_set')then
          goto 46
        elseif(WORD(1:18).eq.'*end_constructions')then
          goto 44
        else
          goto 46   ! nothing matches this try another line.
        endif
      elseif(WORD(1:4).eq.'*end')then
        goto 45
      else
        call usrmsg(' Unknown default in... ',outstr,'W')
        goto 44
      endif

      if(ier.ne.0)then
        write(outs248,'(3a)') 'Problem detected in ',
     &    defdbfl(:lnblnk(defdbfl)),' while reading...'
        CALL EDISP248(IUOUT,outs248,100)
        CALL EDISP(IUOUT,outstr)
        goto 45
      endif

C Loop back for next default file.
      goto 44

C Close defaults file before exiting.
   45 CONTINUE
      CALL ERPFREE(IFDEF,ISTAT)
      RETURN
      END SUBROUTINE


C ****** parpsfh
C parpsfh: Help message for several of the command line parsers.
      subroutine parpsfh(appn)
      COMMON/OUTIN/IUOUT,IUIN
      character appn*24

      if(appn(1:3).eq.'bld'.or.appn(1:3).eq.'bps')then
        call edisp(iuout,'This is the ESP-r Simulator. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-v|-vv] verbose or very verbose')
        call edisp(iuout,'   [-p <param_set_name> <how>]')
        call edisp(iuout,'    (where how {interactive|silent} defines')
        call edisp(iuout,'     whether the user interacts')
        call edisp(iuout,'   [-actf file providing bems commands')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,'   [-b {no|yes}]')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -mode options are:')
        call edisp(iuout,'    text    = text mode')
        call edisp(iuout,'    graphic = graphic mode')
        call edisp(iuout,'    script = script mode')
        call edisp(iuout,'  where -b (browse options) are used to')
        call edisp(iuout,'    indicate that the user has requested')
        call edisp(iuout,'    a simulation for a model they are')
        call edisp(iuout,'    browsing (in which case the results')
        call edisp(iuout,'    will be placed in the users home')
        call edisp(iuout,'    folder. Default is no.')
        call edisp(iuout,' ')
      elseif(appn(1:3).eq.'clm')then
        call edisp(iuout,'The climate analysis module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <climate db>]')
        call edisp(iuout,'   [-act <action> <how> <file>]')
        call edisp(iuout,'  (where action {asci2bin|bin2asci|epw2bin})')
        call edisp(iuout,'  (where how {interactive|silent} defines ')
        call edisp(iuout,'   whether the user interacts')
        call edisp(iuout,'  (where file is asci input or output file)')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,' ')
        call edisp(iuout,'Example converting binary file to asci: ')
        call edisp(iuout,
     &    ' clm -file current.clm -act bin2asci silent current.asci')
        call edisp(iuout,' ')
        call edisp(iuout,'Example converting asci file to binary: ')
        call edisp(iuout,
     &    ' clm -file new.clm -act asci2bin silent current.asci')
        call edisp(iuout,' ')
        call edisp(iuout,
     &    'Example converting (an edited) EPW file to binary:')
        call edisp(iuout,
     &    ' clm -file new.clm -act epw2bin silent current.epw')
        call edisp(iuout,' ')
      elseif(appn(1:3).eq.'dfs')then
        call edisp(iuout,'The stand-alone domain flow module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
      elseif(appn(1:3).eq.'e2r')then
        call edisp(iuout,'The visualisation module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <focus zone|index>]')
        call edisp(iuout,'       [-purpose <scene purpose>]')
        call edisp(iuout,'  where the scene purpose is ...')
        call edisp(iuout,'       [-act {Create|Calculate}]')
        call edisp(iuout,'  where Create generates radiance files and')
        call edisp(iuout,'        Calculate uses them.')
        call edisp(iuout,'       [-actf <file generated by bps>]')
      elseif(appn(1:3).eq.'grd')then
        call edisp(iuout,'The 2-D and 3D gridding module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
      elseif(appn(1:3).eq.'ish')then
        call edisp(iuout,'The shading & insolation database module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
        call edisp(iuout,'       [-s < width, offset left & top>]')
        call edisp(iuout,'       [-act {recalculate|update_silent}]')
        call edisp(iuout,'       [-act [recalculate|update_silent]}')
        call edisp(iuout,'       [     { <action> <file>]}')
        call edisp(iuout,'   (where action {asci2bin|bin2asci}]')
        call edisp(iuout,'   (where file is asci input or output file)')
        call edisp(iuout,'       [-act [useupdate_silent]}')
        call edisp(iuout,' ')
        call edisp(iuout,'  where -act recalculate')
        call edisp(iuout,'    interactively recalculates shading.')
        call edisp(iuout,'  where -act update_silent')
        call edisp(iuout,'    no usr interaction during recalculation.')
        call edisp(iuout,'  where -act useupdate_silent')
        call edisp(iuout,'    no usr interaction during recalculation')
        call edisp(iuout,'    and any existing ascii shading file is')
        call edisp(iuout,'    converted to a binary file otherwise a')
        call edisp(iuout,'    full calculation is performed')
        call edisp(iuout,' ')
      elseif(appn(1:3).eq.'mfs')then
        call edisp(iuout,'The stand-alone network flow solver. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
      elseif(appn(1:3).eq.'mrt'.or.appn(1:3).eq.'vwf')then
        call edisp(iuout,'The viewfactor calculation module.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
        call edisp(iuout,'       [-zone <name|index>]')
      elseif(appn(1:3).eq.'net')then
        call edisp(iuout,'The graphic network layout tool. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
      elseif(appn(1:3).eq.'pdb')then
        call edisp(iuout,'The plant components database manager. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <plant template file>]')
        call edisp(iuout,'   [-act <action> <file>]')
        call edisp(iuout,'  (where action {asci2bin|bin2asci})')
        call edisp(iuout,'  (where file is asci input or output file)')
        call edisp(iuout,'   [-s < width, offset left & top>]')
      elseif(appn(1:3).eq.'pdf')then
        call edisp(iuout,'The plant network definitions. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
      elseif(appn(1:3).eq.'pfs')then
        call edisp(iuout,'This is the power flow simulator. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <configuration file>]')
      elseif(appn(1:3).eq.'prj')then
        call edisp(iuout,'The project manager.')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-v|-vv] verbose or very verbose')
        call edisp(iuout,
     &     '       [-file <configuration>|<silent command>]')
        call edisp(iuout,'       [-zone <name|index>]')
        call edisp(iuout,'       [-act {silentread}] ')
        call edisp(iuout,' (where -act silentread assumes -file <name>')
        call edisp(iuout,'  is high-level model creation directives')
        call edisp(iuout,'  file. Otherwise the -file <name> is')
        call edisp(iuout,'  assumed to be a model configuration file)')
        call edisp(iuout,'       [-act transform < x y z values>] ')
        call edisp(iuout,' (where -act transform is followed by three')
        call edisp(iuout,'  values for the transform in X Y Z axis)')
        call edisp(iuout,'       [-act rotate < angle x y values>] ')
        call edisp(iuout,' (where -act rotate is followed by the angle')
        call edisp(iuout,'  degrees +=anticlockwise and the X and Y')
        call edisp(iuout,'  coordinates to rotate around.)')
        call edisp(iuout,'       [-act {QA} ] ')
        call edisp(iuout,'  creates a model contents report')
        call edisp(iuout,'       [-act {update_zone_con} ]')
        call edisp(iuout,'  updates the zone construction files.')
      elseif(appn(1:3).eq.'res')then
        call edisp(iuout,'This is the ESP-r analysis module. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <results file>]')
        call edisp(iuout,'   [-act <action, {interactive|silent}>]')
        call edisp(iuout,'  (where action {ipv_win1|ipv_spr|ipv_sum|)')
        call edisp(iuout,'                 ipv_aut|ipv_win2|ipv_trn|)')
        call edisp(iuout,'                 ipv_annual|BEMS|recover})')
        call edisp(iuout,'  (where default is interactive mode and')
        call edisp(iuout,'   silent is automatic execution)')
        call edisp(iuout,'   [-actf file providing bems commands or')
        call edisp(iuout,'   recovery commands]')
        call edisp(iuout,'   [-s < width, offset left & top>]')
        call edisp(iuout,' ')
      elseif(appn(1:3).eq.'vew')then
        call edisp(iuout,'The hidden line viewer. ')
        call edisp(iuout,'  Use: [-mode {text|graphic}]')
        call edisp(iuout,'       [-file <viewer_file>]')
      endif
      call edisp(iuout,'       [-s < width, offset left & top>]')
      call edisp(iuout,' ')
      call edisp(iuout,'  where -mode options are:')
      call edisp(iuout,'    text    = text mode ')
      call edisp(iuout,'    graphic = graphic mode')
      call edisp(iuout,'    script = script mode')
      call edisp(iuout,' ')
      call edisp(iuout,' -help: display this help message. ')
      call edisp(iuout,' -version: display version number and quit.')
      call edisp(iuout,' -buildinfo: display build details and quit.')
      return
      end

            
C-----------------------------------------------------------------------
C     Function bH3KExtentionsActive: Returns value of 
C     h3k_extentions flag 
C-----------------------------------------------------------------------
      logical function bH3KExtentionsActive()
      implicit none 
      
      common/H3KCustom/bEnableH3kExtensions
      logical bEnableH3kExtensions     
      
      bH3KExtentionsActive = bEnableH3kExtensions
      
      return 
      
      end function bH3KExtentionsActive
      
