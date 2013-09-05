C This file is part of the ESP-r system.
C Copyright Sustainable Building Energy Systems Laboratory, Carleton University.
C Please contact Ian Beausoleil-Morrison for details concerning licensing.

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


C ---------------------------------------------------------------------
C Created By: Andrea Pietila, Courtney Edwards, & John Kopf
C Initial Creation Date: August 19, 2010
C
C This file contains the following routines:
C HVACGIN: Priciple interface for creating a hvac file defining an idealized 
C          hvac system. Builds menus and governs user interactions. Operates 
C          as follows:
C          First, check whether a .hvac file already exists or not. If yes, 
C          then ask user whether they would like to use that file (note that 
C          they can change parts, if not all of it later), then skip the hvac 
C          definition stage, and continue to the summary page where they can 
C          view a summary of the .hvac system characteristics. If no .hvac
C          file is present, continue to the next step of .hvac system
C          selection and definition.
C HVAC_type:        Displays the idealized hvac types for user selection.
C FURNOPT:          User interface and variable definition for the furnace choice in the idealized HVAC system
C BASEBOARD_INPUTS: User interface and variable definition for the basboard choice in the idealized HVAC system
C HPHOT:            User interface and variable definition for the ASHP choice in the idealized HVAC system
C HPCOLD:           User interface and variable definition for the ASHP with cooling choice in the idealized HVAC system
C FURNACE_WRITE:    Writes variables associated with furnace system directly to .hvac file.
C BASEBOARD_WRITE:  Writes variables associated with baseboard system directly to .hvac file.
C HPHOT_WRITE:      Writes variables associated with heating heat pump system directly to .hvac file.
C HPCOLD_WRITE:     Writes variables associated with cooling heat pump system directly to .hvac file.
C HVAC_SUMMARY: Displays main hvac file summary menu
C SYS_SUMMARY:  Displays system characteristics summary menu of any system chosen from the hvac summary menu
C HVAC_ZERO:    Clears all common global variables in hvac_common.h, furnace_common.h and ashp_common.h
C
C 
C     In subroutines for each type, code exists to ensure fractions add to 1.  
C     Must change max value in the EASKR statements to ifraction instead of 1.0,
C     and the default value should be ifraction as well.  
C ---------------------------------------------------------------------
C -------------------------------HVACGIN-------------------------------
      SUBROUTINE HVACGIN

#include "building.h"
#include "model.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"
      
      COMMON/hvacinfo/ihvacflag,hvacfile
      COMMON/ER/IER
      COMMON/FILEP/IFIL
      COMMON/user/browse
      
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL        CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK      ! file flags
      
      COMMON/C21/IFCFG,cfgroot,LCFGF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/rpath/path
      
      CHARACTER path*72                !path
      CHARACTER LCFGF*72
      CHARACTER cfgroot*24
      CHARACTER hvacfile*72            ! hvac file name
      CHARACTER def_hvacfile*72        ! default hvac filename
      CHARACTER sfile*72               ! file name returned from browsefilelist (with file path)
      CHARACTER snpfile*72             ! file name returned from browsefilelist (no file path)
      CHARACTER fs*1                   ! folder separator
      CHARACTER outs*200               ! output array for display within GUI
      CHARACTER doit*248               ! temporary hvac folder path if folder does not exist
      CHARACTER TempString*248         ! temporary hvac folder path to check if folder exists
      CHARACTER CheckString*4          ! temporary string to determine if using standard set of folders
      INTEGER lnblnk                   ! function definition
      INTEGER ichoice                  ! selection choice
      INTEGER fbackup                  ! furnace backup
      INTEGER modechoice               ! ASHP and GSHP heating or cooling mode flag
      INTEGER i                        ! flag for current hvac number
      INTEGER k                        ! Variable used as a counter
      INTEGER heatok                   ! Flag for use of a heating system with a cooling HP 
      INTEGER hvac_typ                 ! hvac system type
      INTEGER ihvacflag                ! flags whether an hvac file exists
      INTEGER ival                     ! Passed value from subroutine
      INTEGER hvacunit                 ! file number for hvac file
      INTEGER priority                 ! system priority, either main(1) or backup (2) in a specific zone
      INTEGER edit_unit_number         ! returned from HVAC_MAIN_SUMMARY as the system number user chooses to edit, 0 if none
      INTEGER IER                      ! error reporting
      LOGICAL OK                       ! logical for OK box
      LOGICAL DOK                      ! default logical for OK box
      LOGICAL XST                      ! existing file check
      LOGICAL browse                   ! browse mode check
      LOGICAL unixok                   ! unix check
      LOGICAL new_hvac                 ! true if hvac file needs to be defined (no existing file found)
      LOGICAL hvacfile_good            ! true if hvac file name is properly entered
      LOGICAL exitflag                 ! returned from HVAC_MAIN_SUMMARY as true if user wishes to exit back to main menu
      LOGICAL save_flag                ! returned from HVAC_MAIN_SUMMARY as true if user wishes to save the system
      LOGICAL add_flag                 ! returned from HVAC_MAIN_SUMMARY as true if user wishes to add a system
      LOGICAL lexist                   ! true if hvac folder exists

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HVACGIN'  ! set for subroutine

      new_hvac=.false.
      hvacfile_good=.false.
      hvacunit=IFIL+20                 ! hvac file unit number

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      
C Check to see if hvac folder exists, if not create on and set hvacpth 
C of the folder as it is used in creating the hvac file name
      CheckString=path((lnblnk(path)-3):lnblnk(path))
C If user is using the standard set of folders, create hvac folder in 
C model directory (same location as cfg, ctl, dbs, etc., folders)
      if(CheckString(1:4).eq.'cfg/')then
        WRITE(TempString,'(3a)')path(1:(lnblnk(path)-4)),'hvac',fs
        INQUIRE(FILE=TempString,EXIST=lexist)
        if (.not.lexist) then
          write(doit,'(4a)') 'mkdir ',path(1:(lnblnk(path)-4)),'hvac'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(hvacpth,'(3a)')'..',fs,'hvac'
        endif
C Else if user is using a single folder, create hvac folder within that
C folder
      else
        WRITE(TempString,'(3a)')path(1:(lnblnk(path))),'hvac',fs
        INQUIRE(FILE=TempString,EXIST=lexist)
        if (.not.lexist) then
          write(doit,'(4a)') 'mkdir ',path(1:(lnblnk(path))),'hvac'
          call usrmsg('Creating folder:',doit,'-')
          call runit(doit,'-')
          call pausems(400)
          write(hvacpth,'(3a)')'.',fs,'hvac'
        endif
      endif
      
C Zero all current hvac data in common variables
      CALL HVAC_ZERO
C Attempt to read in the hvac file. If no current name then suggest
C one. If an existing name also provide option to deference it.

      do while (.not.hvacfile_good)
        if(hvacfile(1:2).eq.'  '.or.hvacfile(1:4).eq.'UNKN')then
          if(browse)then
            call usrmsg('No hvac associated with this model,',
     &                  'returning with no change.','W')
            return
          endif
          LN=max(1,LNBLNK(cfgroot))
          if(hvacpth(1:2).eq.'  '.or.hvacpth(1:2).eq.'./')then
            WRITE(hvacfile,'(4a)') hvacpth(1:lnblnk(hvacpth)),fs,
     &           cfgroot(1:LN),'.hvac'
          else
            WRITE(hvacfile,'(4a)') hvacpth(1:lnblnk(hvacpth)),fs,
     &           cfgroot(1:LN),'.hvac'
          endif

C Present current/default name and a browse option. If browse selected
C then set ichoice variable and jump to browse functions.
          def_hvacfile=hvacfile
          helptopic='zone_hvac_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS2CMD(def_hvacfile,'HVAC file? (suggested name)',' ',
     &      'cancel','browse',ichoice,72,' ','HVAC file',IER,nbhelp)
          if(ichoice.eq.1)then         ! If user wishes to cancel then return to networks menu
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          endif
        
C Else if hvac file(s) currently exist(s), allow option to dereference 
C or browse through existing files
        else
          def_hvacfile=hvacfile
          if(browse)then
            ihvacflag=1
            CALL ERPFREE(ihvacflag,ISTAT)
            call FINDFIL(hvacfile,XST)
            if(XST)then
      !       do something here if file exists and is found in browse mode (exit and skip to summary?)
            else
              call usrmsg('HVAC file not found. You are browsing',
     &                  'so returning without change to the model.','W')
              return
            endif
          else
          helptopic='zone_hvac_file'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKS2CMD(def_hvacfile,'HVAC file? (see help)',
     &      ' ','dereference','browse',ichoice,72,' ','HVAC file',
     &      IER,nbhelp)
     
C If user wishes to dereference the current hvac file, reset the hvac
C file path and hvac flag, and call EMKCFG to save the configuration
C file.
            if(ichoice.eq.1)then
              if(cfgok.and.(.NOT.browse))then
                CALL HVAC_ZERO
                hvacfile='  '        ! Clear hvac file path
                def_hvacfile='  '    ! Clear temporary hvac file path
                ihvacflag=0          ! Set hvac flag to 0 for non existant
                hvacfile_good=.false.
                CALL EMKCFG('-',IER)
                CALL USRMSG(' ',' ','-') ! Clear Screen
                return
              endif
            endif
          endif
        endif
        CALL USRMSG(' ',' ','-') ! Clear Screen
C If user wishes to browse to see what hvac files are available, call
C the browsefilelist subroutine to display all .hvac files in the hvac
C folder.
        if(ichoice.eq.2)then
          sfile=' '
          snpfile=' '
          call edisp(iuout,' ')
          call browsefilelist('?','hva','fil',sfile,snpfile,nfile,iier)
          if(nfile.gt.0)then
            sfile=' '
            snpfile=' '
            call browsefilelist('b','hva','fil',sfile,snpfile,nfile,
     &                           iier)
            if(snpfile(1:2).ne.'  ')then
              write(def_hvacfile,'(3a)')hvacpth(1:lnblnk(hvacpth)),fs,
     &               snpfile(1:lnblnk(snpfile))
            else
C If the user supplied a blank file then this is the same as
C de-referencing the file.

              call edisp(iuout,
     &                'A blank hvac file name returned. Clearing data.')
              CALL HVAC_ZERO
              hvacfile='  '        ! Clear hvac file path
              def_hvacfile='  '    ! Clear temporary hvac file path
              ihvacflag=0          ! Set hvac flag to 0 for non existant
              hvacfile_good=.false.
              CALL EMKCFG('-',IER)
            endif
          else

C If no files are present in the hvac folder, display a warning
            call edisp(iuout,'No hvac file present in folder!')
            CALL HVAC_ZERO
            hvacfile='  '        ! Clear hvac file path
            def_hvacfile='  '    ! Clear temporary hvac file path
            ihvacflag=0          ! Set hvac flag to 0 for non existant
            hvacfile_good=.false.
            CALL EMKCFG('-',IER)
          endif
        endif
        
        if(def_hvacfile(1:2).NE.'  ')then
          hvacfile=def_hvacfile
          hvacfile_good=.true.
          
          call ERPFREE(1,ISTAT)
          call FINDFIL(hvacfile,XST)
          if(XST)then
            new_hvac=.false.
          else
            call EASKAB(' File not found.','Options:',
     &                  'specify another','make new file',IW,nbhelp)
            if(IW.eq.1)then
              hvacfile='  '        ! Clear hvac file path
              def_hvacfile='  '    ! Clear temporary hvac file path
              ihvacflag=0          ! Set hvac flag to 0 for non existant
              hvacfile_good=.false.
            elseif(IW.eq.2)then
              new_hvac=.true.
              ihvacflag=0
              CALL EFOPSEQ(hvacunit,hvacfile,3,IER)
              CLOSE(hvacunit)
            endif
          endif
        endif
      enddo

C Open hvac file here

      exitflag=.false.
      save_flag=.false.
      add_flag=.false.
      
      if(XST)then
        call hvacinput(hvacfile)
        CLOSE(hvacunit)
      endif
      
      do while (.not.exitflag)
        if(.not.new_hvac)then
          
          call HVAC_MAIN_SUMMARY(edit_unit_number,add_flag,save_flag,
     &                           exitflag)
C If user chooses to save file, call the appropriate write routine
C for each system within the hvac file
          if(save_flag)then
            i=1
            CALL EFOPSEQ(hvacunit,hvacfile,3,IER)
            REWIND hvacunit
            WRITE(hvacunit,*) num_hvac_sys, site_altitude,
     &           '#Number of hvac systems,site_altitude'
            do while(i .le. num_hvac_sys)
              if (ihvac_type(i) .eq. 1) then
                write(hvacunit,'(A)') "#         "
                write(hvacunit,'(A,I1,A)')'#===Information for System ',
     &                           i,' Furnace===='
                CALL FURNACE_WRITE(i,hvacunit)
              elseif (ihvac_type(i) .eq. 3) then
                write(hvacunit,'(A)') "#         "
                write(hvacunit,'(A,I1,A)')'#===Information for System ',
     &                           i,' Baseboard===='
                CALL BASEBOARD_WRITE(i,hvacunit)
              elseif (iunit_function(i) .eq. 1) then
                write(hvacunit,'(A)') "#         "
                write(hvacunit,'(A,I1,A)')'#===Information for System ',
     &                           i,' Heat Pump Heating Mode==='
                CALL HPHOT_WRITE(i,hvacunit)
              elseif (iunit_function(i) .eq. 2) then
                write(hvacunit,'(A)') "#         "
                write(hvacunit,'(A,I1,A)')'#===Information for System ',
     &                           i,' Heat Pump Cooling Mode==='
                CALL HPCOLD_WRITE(i,hvacunit)
              else
              end if
              i=i+1
            end do
            i=0
            save_flag=.false.
            CLOSE(hvacunit)
            ihvacflag=1
            CALL EMKCFG('-',IER)
            call USRMSG('File Saved!',' ','-')

C     If user chooses to edit a specific system, set the variable 'i' to 
C     the system number to be edited and continue to the definition stage
          elseif(edit_unit_number.ne.0)then
            i = edit_unit_number
            if (ihvac_type(i).eq.1)then
              hvac_typ = 1
            elseif (ihvac_type(i).eq.3)then
              hvac_typ = 2
            elseif (ihvac_type(i).eq.7)then
              hvac_typ = 3
            elseif (ihvac_type(i).ge.8)then
              hvac_typ = 4
            endif
          elseif(add_flag) then !user chose to add a new item
            i = num_hvac_sys + 1
          endif
        endif

C     Continue with hvac file definition.
        if(new_hvac .or. add_flag .or. (edit_unit_number.gt.0))then
          if(new_hvac)then
            i=1
            CALL EASKR(site_altitude,  
     &        ' ','Select site altitude',
     &        0.,'F',8000.0, 'F',0., 'Altitude', ier,nbhelp)  
          endif
          if(edit_unit_number .eq. 0) then
            num_hvac_sys = num_hvac_sys +1
            call HVAC_type(hvac_typ)
          endif
          heatok=0
C If user makes no selection, or selects exit, go back to Plants & Networks Menu
          if(hvac_typ.eq.0)then
            return
C Display menu for furnace options
          elseif(hvac_typ.eq.1) then
            if(edit_unit_number .eq. 0 .and. num_hvac_sys .le. 4) then     
              call EASKABC('Furnace',' ','With A/C','Without A/C',
     &                   'Cancel',ichoice,nbhelp)
            else
              ichoice = 2
            endif
            if(ichoice.eq.3)then 
              return
            elseif(ichoice.eq.1)then
              priority = 1
              CALL FURNOPT(priority,i,exitflag)
              priority = 2
              fbackup = 1
C hvac_typ assigns the air conditioner to be ASHP 
C not sure if we need to change this to give the choice of ASHP or GSHP
              hvac_typ = 3
              call edisp(iuout,
     &          'Now entering options for associated a/c')
              i = i+1
              num_hvac_sys=num_hvac_sys+1
              CALL HPCOLD(hvac_typ,priority,fbackup,i,heatok,exitflag)
            elseif(ichoice.eq.2)then
              priority = 1
              CALL FURNOPT(priority,i,exitflag)
            endif
          
C Call for Baseboard
          elseif(hvac_typ.eq.2)then
            priority = 1
            CALL BASEBOARD_INPUTS(priority,i,exitflag)
            CALL USRMSG(' ',' ','-') ! Clear Screen
            i = num_hvac_sys

C Display menu for Air-Souce Heat Pumps Option.
          elseif(hvac_typ.eq.3)then
            do while(ipck.ne.2)
              helptopic='zone_hvac_ashp'
              call gethelptext(helpinsub,helptopic,nbhelp)
              call EASKABCD('Please select unit use',' ','Heating',
     &                    'Cooling','Heating & Cooling',
     &                    'Cancel',modechoice,nbhelp)      
              if(modechoice.eq.4)then 
                return
              end if
              if(modechoice.eq.3 .and. num_hvac_sys .eq. 5) then
                CALL PHELPD('Too many HVAC Sys', nbhelp,'-',0,0,ier)
                return
              end if  
              if(edit_unit_number .eq. 0)then
                call EASKABC('Select backup',' ','Furnace backup',
     &                   'no backup','Cancel',ichoice,nbhelp)      
                if(ichoice.eq.3)then 
                  return
                elseif(ichoice.eq.1) then
                  fbackup=1
                else
                  fbackup=0
                end if
              else
                fbackup =0
              end if 
              if((modechoice.eq.3 .and. ichoice.eq.1 .and. 
     &               num_hvac_sys .eq. 4) .or. (ichoice.eq.1 .and. 
     &               num_hvac_sys .eq. 5)) then
                CALL PHELPD('Too many HVAC Sys',nbhelp,'-',0,0,ier)
                return
              end if       
              if(ichoice.eq.1)then
                priority = 2
               CALL USRMSG('','Furnace backup described first',
     &           'W')
                CALL FURNOPT(priority,i,exitflag)
                i=i+1
                num_hvac_sys=num_hvac_sys+1
                priority =0
              end if
              if(modechoice.eq.1)then
                priority=1
                CALL HPHOT(hvac_typ,priority,fbackup,i,exitflag)
              elseif(modechoice.eq.2)then
                priority=1
                CALL HPCOLD(hvac_typ,priority,fbackup,i,heatok,exitflag)
              elseif(modechoice.eq.3)then
                priority=1
                if(edit_unit_number .ne. 0) then
                  helptopic='zone_hvac_ashp_no_heat'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL PHELPD('Possible Overwrite',nbhelp,'-',0,0,ier)
                  CALL EASKAB('', ' ','Continue (dangerous)', 'Go back',
     &              ipck,nbhelp)
                  if(ipck.eq.1) then !continue)
                    continue
                  elseif(ipck.eq.2) then !go back to choices
                    CALL USRMSG(' ',' ','-') ! Clear Screen
                    exit
                  end if
                endif
                call edisp(iuout,
     &            'Enter options for associated heating cycle')
                CALL HPHOT(hvac_typ,priority,fbackup,i,exitflag)
                call edisp(iuout,
     &            'Enter options for associated cooling cycle')
                i = i+1
                num_hvac_sys=num_hvac_sys+1
                heatok=1
                CALL HPCOLD(hvac_typ,priority,fbackup,i,heatok,exitflag)

              end if
              CALL USRMSG(' ',' ','-') ! Clear Screen
              exit
            end do
            
C Display menu for Ground Source Heat Pumps Options.
          elseif(hvac_typ.eq.4)then
            do while(ipck.ne.2)
              helptopic='zone_hvac_gshp'
              call gethelptext(helpinsub,helptopic,nbhelp)
              call EASKABCD('Please select unit use',' ','Heating',
     &                    'Cooling','Heating & Cooling','Cancel',
     &                    modechoice,nbhelp)
              if(modechoice.eq.4)then 
                return
              end if
              if(modechoice.eq.3 .and. num_hvac_sys .eq. 5) then
                CALL PHELPD('Too many HVAC Sys',nbhelp,'-',0,0,ier)
                return
              end if
              if(edit_unit_number .eq. 0)then 
                call EASKABC('Select backup',' ','Furnace backup',
     &                   'no backup','Cancel',ichoice,nbhelp)      
                if(ichoice.eq.3)then 
                  return
                elseif(ichoice.eq.1) then
                  fbackup=1
                else
                  fbackup=0
                end if
              else
                fbackup =0
              end if   
              if((modechoice.eq.3 .and. ichoice.eq.1 .and. 
     &               num_hvac_sys .eq. 4) .or. (ichoice.eq.1 .and. 
     &               num_hvac_sys .eq. 5)) then
                CALL PHELPD('Too many HVAC Sys',nbhelp,'-',0,0,ier)
                return
              end if  
              if(ichoice.eq.1)then
                priority = 2
                CALL USRMSG(' ',
     &                  'Now entering options for furnace backup','W')
                CALL FURNOPT(priority,i,exitflag)
                i=i+1
                num_hvac_sys=num_hvac_sys+1
                priority =0
              end if     
     
              if(modechoice.eq.1)then
                priority =1
                CALL USRMSG(' ',
     &                  'Now entering options for heating','W')
                CALL HPHOT(hvac_typ,priority,fbackup,i,exitflag)
              elseif(modechoice.eq.2)then
                priority =1
                CALL USRMSG(' ',
     &                  'Now entering options for cooling','W')
                CALL HPCOLD(hvac_typ,priority,fbackup,i,heatok,exitflag)
              elseif(modechoice.eq.3)then
                priority =1
                if(edit_unit_number .ne. 0) then
                  helptopic='zone_hvac_ashp_no_heat'
                  call gethelptext(helpinsub,helptopic,nbhelp)
                  CALL PHELPD('Possible Overwrite',nbhelp,'-',0,0,ier)
                  CALL EASKAB(' ', ' ','Continue (DANGEROUS! Eek!!!)', 
     &              'Go back',ipck,nbhelp)
                  if(ipck.eq.1) then !continue)
                    continue
                  elseif(ipck.eq.2) then !go back to choices
                    CALL USRMSG(' ',' ','-') ! Clear Screen
                    exit
                  end if
                endif
                call edisp(iuout,
     &            'Enter options for associated heating cycle')
                CALL HPHOT(hvac_typ,priority,fbackup,i,exitflag)
                call edisp(iuout,
     &            'Enter options for associated cooling cycle')
                i = i+1
                num_hvac_sys=num_hvac_sys+1
                heatok=1
                CALL HPCOLD(hvac_typ,priority,fbackup,i,heatok,exitflag)
              end if
              CALL USRMSG(' ',' ','-') ! Clear Screen
              exit
            end do
          end if
          new_hvac=.false.
          edit_unit_number = 0  ! reset in case they go back to summary and come back here
        endif

      enddo
          
      return
      end

C ---------------------------------------------------------------------
C ------------------------HVAC System Selection------------------------
C HVAC_type

C This routine displays the Idealized HVAC types for user selection.
C Returns hvac_typ zero if no selection was made otherwise a value
C between 1 and 4. To add more system choices the parameter hvac_systems
C should be increased to match the number of systems, and strings for
C the new system types added to the hvac_sys_typ array.
C ---------------------------------------------------------------------
      
      SUBROUTINE HVAC_type(hvac_typ)
      
#include "hvac_parameters.h"
C hvac_parameters.h for variable max_hvac_types
      
      INTEGER hvac_typ          ! choice to be returned from this subroutine
      INTEGER hvac_systems      ! number of hvac systems to choose from 
      INTEGER hvac_selections   ! what the user selected (returned from epicks)
      INTEGER hvac_choice       ! array of integers of possible choices
C Note: Maximum number of hvac system types (furnace, heat pump, etc ...) is
C defined in hvac_parameters.h as max_hvac_types (=8)
      PARAMETER (hvac_systems = 4)
      DIMENSION hvac_choice(max_hvac_types)
      CHARACTER*26 hvac_sys_typ(hvac_systems) ! array of strings for system types

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HVAC_type'  ! set for subroutine

C Fill in array of Idealized HVAC system types
      hvac_sys_typ(1)=' Furnace                '
      hvac_sys_typ(2)=' Baseboard Heating      '
      hvac_sys_typ(3)=' Air-Source Heat Pump   '
      hvac_sys_typ(4)=' Ground-Source Heat Pump'
C     hvac_sys_typ(n)=' New Idealized HVAC type'   ! enter new future system(s) here

C Help message            
      helptopic='zone_hvac_primary'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Present list to user.     
      hvac_selections = 1   ! user can only make one selection
      CALL EPICKS(hvac_selections,hvac_choice,
     &   'Select Idealized HVAC System Type','(See Help for Help) ',
     &   26,hvac_systems,hvac_sys_typ,'HVAC System Type:',IER,nbhelp)
      
      if(hvac_selections.eq.0)then
        hvac_typ = 0                ! user made no selection so return zero
      else
        hvac_typ = hvac_choice(1)   ! return first value of hvac_choice array
      endif
      
      return
      end
      
C --------------------------End Of: HVAC_type--------------------------
C ---------------------------------------------------------------------


C ---------------------------------------------------------------------
C FURNOPT
C
C This routine allows the user to input options of the furnace choice 
C in the idealized HVAC system
C ---------------------------------------------------------------------

C FURNOPT - define furnace system
C IER=0 indicates no error. if -3, error button clicked (in ESP-r Code documentation)
      SUBROUTINE FURNOPT(priority,i,exit)


#include "building.h"
#include "geometry.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

      common/C1/NCOMP, NCON
      
      INTEGER ipck                ! Integer returned from epicks menu
      INTEGER nfurntyp            ! Number of furnace options
      INTEGER nfueltyp            ! Number of fuel options
      INTEGER i                   ! identifier for current system number
      INTEGER k                   ! Counter used for zone association
      INTEGER NCOMP, NCON         ! Number of zones in system
      INTEGER sys_zone_number(ncomp)   !Zone number of of a zone associated with the system
      INTEGER zonenum(ncomp)      !Array of all zone numbers associated with the system 
      INTEGER priority            !Passed integer. 1 if primary, 2 if backup
      INTEGER ier                 ! Passed if nothing selected
      CHARACTER*50 furntyp(14)    ! Character array of all furnace options
      CHARACTER*50 fueltyp(14)    ! Character array of all fuel options
      CHARACTER*50 zoneopt(ncomp+1)  !Character array of all zone names
      REAL fraction               ! Fraction of power assigned to zones so far
      REAL ifraction              ! 1-fraction, fraction which remains to be assigned
      LOGICAL exit                ! Returns as true if user ends system definition early
      LOGICAL fuel_logic          !used for re-entering fuel type if not valid match with furnace type
      LOGICAL fuel_logic2         ! used for re-entering fuel type if not valid match with furnace type
      INTEGER chosen_sys(14)
      INTEGER chosen_fuel(5)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='FURNOPT'  ! set for subroutine

      ipck=1
      nfurntyp=14
      nfueltyp=5
      zonepck=ncomp
      ihvac_type(i) = 1
      exit=.false.
      
      if(priority .eq. 0) then
        helptopic='zone_system_priority'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(ipriority(i),'Specify the system priority',
     &             '1 for primary (default),2 for backup',1,'F',
     &             2,'F',1,'value must be 1 or 2',ier,nbhelp)
      else
        ipriority(i) = priority
      endif
      
C Furnace Type
      furntyp(1)='Advanced air tight wood stove'
      furntyp(2)='Catalytic converter'
      furntyp(3)='Flame retention head'
      furntyp(4)='Flue vent damper'
      furntyp(5)='Spark-ignition'
      furntyp(6)='Spark ignition and vent damper'
      furntyp(7)='Continuous pilot'
      furntyp(8)='Condensing furnace'
      furntyp(9)='Direct vent non-condensing'
      furntyp(10)='Induced draft'
      furntyp(11)='Mid efficiency'
      furntyp(12)='Electric forced-air'
      furntyp(13)='Wood furnace'
      furntyp(14)='Oil furnace or boiler'
      
      helptopic='zone_furnace_type'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      CALL EPICKS(ipck,chosen_sys,' ','Select a furnace type:',30,
     &  nfurntyp, furntyp,'Furnace types for idealized HVAC',
     &  ier,nbhelp)
      if(ipck.eq.0) then !nothing selected
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      isys_type(i) = chosen_sys(1)
      
      if(isys_type(i).eq.12)then
        ifuel_type(i) = 1
      elseif(isys_type(i).eq.3.or.isys_type(i).eq.4.or.
     & isys_type(i).eq.9.or.isys_type(i).eq.11.or.
     & isys_type(i).eq.14)then
        ifuel_type(i)=3
      elseif(isys_type(i).eq.1.or.isys_type(i).eq.2.or.
     & isys_type(i).eq.13)then
        ifuel_type(i) = 5
      else         
             
        helptopic='zone_furnace_fuel_type'
        call gethelptext(helpinsub,helptopic,nbhelp)
         
C Fuel Type            
        fuel_logic = .true.
          do while(fuel_logic)
          fuel_logic2 = .true.
          ipck =1
          fueltyp(1) = 'Electric'
          fueltyp(2) = 'Natural Gas'
          fueltyp(3) = 'Oil'
          fueltyp(4) = 'Propane'
          fueltyp(5) = 'Wood'
      
          CALL EPICKS(ipck, chosen_fuel, ' ','Select fuel type:',30,
     &      nfueltyp,fueltyp,'Fuel types - see help for options',
     &      ier,nbhelp)
          if(ipck.eq.0)then !nothing selected
            exit=.true.
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          endif
          ifuel_type(i) = chosen_fuel(1)
          helptopic='zone_valid_fuel_type'
          call gethelptext(helpinsub,helptopic,nbhelp)
          if((isys_type(i) .eq. 5) .or. (isys_type(i) .eq. 6) .or. 
     &     (isys_type(i) .eq. 7) .or. (isys_type(i) .eq. 10)) then
            if (ifuel_type(i) .ne. 2 .and. ifuel_type(i) .ne. 4) then
              CALL PHELPD('Incorrect combination',nbhelp,'-',0,0,ier)
            fuel_logic2 = .false.
            end if
          endif  
          if((isys_type(i) .eq. 8) .and. (ifuel_type(i) .ne. 2).and. 
     &       (ifuel_type(i) .ne. 3) .and. (ifuel_type(i) .ne. 4)) then
            CALL PHELPD('Incorrect combination',nbhelp,'-',0,0,ier)
            fuel_logic2 = .false.
          endif
          if(fuel_logic2) then
            fuel_logic = .false. 
          end if
        end do
      endif
      
      helptopic='zone_furnace_fuel_type'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(num_sys_zones(i),' ',
     &    'Total number of zones served by furnace system:',
     &    1,'F',NCOMP,'F',1,
     &    'Enter a number of zones within the range of your model'
     &    ,ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
            
C Choosing the zones associated with the system
C Select the number of zones the system is linked to

      helptopic='zone_furnace_zones'
      call gethelptext(helpinsub,helptopic,nbhelp)
   
      K=1    
      do while (K.LE.NCOMP)
        WRITE(zoneopt(K),'(4A)') zname(K)
        zonenum(K)=K
        K=K+1
      enddo
      
      ival = num_sys_zones(i)
      CALL EPICKS(ival, sys_zone_number, ' ',
     &            'Select zones being supplied:',30, ncomp, zoneopt,
     &            'Zones - see help for options',ier,nbhelp)   
      if(ival.eq.0)then !nothing selected
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      
      k = 0
      do while (k.lt.num_sys_zones(i))
        k = k+1
        isys_zone_num(i,k) = sys_zone_number(k)

      enddo

C The value isys_zone_num(i,k)' is the zone number of the zone picked in the epicks
C menu above, therefore this is the value that needs to be written to the 
C HVAC file in 'zones being supplied' section.
      fraction = 0
      ifraction = 1
      k=1
      
      helptopic='zone_furnace_frac'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      do while (k.LE.num_sys_zones(i))
        ifraction = 1-fraction
          nnm=sys_zone_number(k)
        CALL EASKR(sys_zone_cap_frac(i,k),  
     &        'Select fraction supplied to',zname(nnm),
     &        0.,'F',ifraction,'F',ifraction,'Fraction',ier,nbhelp)
        fraction = fraction + sys_zone_cap_frac(i,k)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        endif
        
C Check to make sure sum of fractions is 1. If not, redirect back to top
C of loop - can be added later when fractions matter        
        if ((k .eq. num_sys_zones(i)) .and. (fraction .ne. 1)) then
          CALL PHELPD('Unequated fractions',nbhelp,'-',0,0,ier)
          K = 1
          fraction = 0
        else
          K=K+1
        end if
        
      enddo
      
      helptopic='zone_furnace_capacity'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKR(ss_capacity(i),'Steady state capacity of the system',
     &           '(W):', 0.0, 'F', 1000000.0, 'F', 100.0, 
     &           'Please enter a capacity greater than 0',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      
      CALL EASKR(ss_efficiency(i), ' ', 
     &           'Steady state efficiency of the system', 
     &           0.0, 'F', 1.0,'F', 1.0,'efficiency',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      
      CALL EASKABCD(' ','circulation fan mode', 'No Fan Associated', 
     &              'Auto Mode', 'Continuous Mode','Cancel',
     &              ifan_operation(i),nbhelp)
      if(ifan_operation(i).eq.4) then !cancel selected
        ifan_operation(i) = 0
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      
      if(ifan_operation(i).ne.1)then
        helptopic='zone_sys_fan_cap'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(fan_power(i), ' ', 'Fan power (W)', -1.,'F',1.E4,'F',
     &    100.0, 'Invalid input, please see help file',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        endif
     
        CALL PHELPD('Continuous Mode Operation of Fan',nbhelp,'-',
     &    0,0,ier)
      else
        fan_power(i) = 0.
      end if
      
      if (isys_type(i) .eq. 10 .OR. isys_type(i) .eq. 8) then
        helptopic='zone_draft_fan_cap'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(draft_fan_power(i), ' ', 'Draft fan power (W)', -1.,
     &             'F',200.,'F', 75.0,
     &             'Invalid input, please see help file',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        endif
      else
        draft_fan_power(i) = 0.
      endif
      
      if (isys_type(i) .eq. 7) then
        helptopic='zone_furnace_pilot'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(pilot_power(i),' ','Pilot power (W)',0.1,'F',1000.,
     &             'F', 500., 'Pilot Power',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        endif
      endif
      
      helptopic='zone_ducts_included'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(iduct_sys_flag(i),'Specify if there is a duct system',
     &           '1 yes, 0 no (default)',0,'F',1,'F',0,
     &           'value must be 0 or 1',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      CALL USRMSG(' ',' ','-') ! Clear Screen

      return
      end

C --------------------------End Of: FURNOPT----------------------------
C ---------------------------------------------------------------------

C --------------------------BASEBOARD INPUT----------------------------
C BASEBOARD
C
C Baseboard reads in the priority of the system (priority) and the system 
C number(i) from HVACGIN, and prompts the user for all parameters associated
C with the baseboard system.  At present, the baseboard can only be selected
C as a standalone system, and cannot have an associated backup.  All entered
C variables are saved in the common hvac variables.  The user must actually save
C the hvac file for changes to be recorded.
C ---------------------------------------------------------------------

      SUBROUTINE BASEBOARD_INPUTS(priority,i,exit)

#include "building.h"
#include "geometry.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

      common/C1/NCOMP, NCON

C Common it up here.

      INTEGER ier                    ! Returned if error/nothing selected
      INTEGER priority               ! Passed as primary/backup
      INTEGER i                      ! Identifier for system number
      INTEGER NCOMP, NCON            ! Number of zones in system
      INTEGER sys_zone_number(NCOMP) ! Zone numbers of zones associated with the system
      INTEGER K                      ! Counter used for zone association
      INTEGER nnm                    ! Counter used for zone association
      INTEGER zonenum(ncomp)         ! Array of zone numbers associated with the system
      REAL fraction                  ! Fraction of power assigned to zones so far
      REAL ifraction                 ! 1-fraction, fraction which remains to be assigned
      CHARACTER*50 zoneopt(ncomp+1)  ! Array of zone names associated with system
      LOGICAL exit                   ! returns as true if user does not complete system definition

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='BASEBOARD_INPUTS'  ! set for subroutine

      ier = 0
      exit=.false.

C Add hvac system type to array - baseboard = 3
      ihvac_type(i) = 3

C Baseboard can only be primary system at this time.  Prompt for priority commented out below for future use.
      ipriority(i) = 1

C Select the priority of the system
C      if(priority.eq.0) then
C        h(1) = 'Select whether the baseboard system is a primary'
C        h(2) = 'system, or whether it serves as a backup system'
C        h(3) = 'in the zones.'

C        CALL EASKI(ipriority(i),'Specify the system priority',
C     &             '1 for primary (default), 2 for backup', 1, 
C     &             'F', 2, 'F', 1,'value must be 1 or 2',ier,3)
c        if(ier.eq.-3)then
C          exit=.true.
C          return
C        end if
C      else
C        ipriority(i) = priority
C      end if
     
C Select the number of zones the system is linked to

      helptopic='zone_baseboard_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
   
      CALL EASKI(num_sys_zones(i),' ',
     &           'Total number of zones served by baseboard system:',
     &           1,'F',NCOMP,'F',1,
     &          'Enter a number of zones within the range of your model'
     &           ,ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
    
      K=1    
      do while (K.LE.NCOMP)
        WRITE(zoneopt(K),'(4A)') zname(K)
        zonenum(K)=K
        K=K+1
      enddo
      
      ival = num_sys_zones(i) 
      CALL EPICKS(ival, sys_zone_number, ' ',
     &            'Select zones being supplied:',30, ncomp, zoneopt,
     &            'Zones - see help for options',ier,nbhelp)
      if(ival.ne.num_sys_zones(i)) then !nothing selected or not enough
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
        
      
      k = 0
      do while (k.lt.num_sys_zones(i))
        k = k+1
        isys_zone_num(i,k) = sys_zone_number(k)

      end do

C The value 'num_sys_zones(i)' is the zone number of the zone picked in the epicks
C menu above, therefore this is the value that needs to be written to the 
C HVAC file in 'zones being supplied' section.
      fraction = 0
      ifraction = 1
      k=1
      
      do while (k.LE.num_sys_zones(i))
        ifraction = 1-fraction
        nnm=sys_zone_number(k)   
        CALL EASKR(sys_zone_cap_frac(i,k),  
     &        'Select fraction supplied to',zname(nnm),
     &        0.,'F',ifraction, 'F',ifraction, 'Fraction', ier,nbhelp)
        fraction = fraction + sys_zone_cap_frac(i,k)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
        
C Check to make sure sum of fractions is 1. If not, redirect back to top 
C of loop - can be added later when fractions matter        
        if ((k .eq. num_sys_zones(i)) .and. (fraction .ne. 1)) then
          CALL PHELPD('Unequated fractions',nbehlp,'-',0,0,ier)
          k = 1
          fraction = 0
        else
          K=K+1
        end if
        
      end do
      
      CALL EASKR(ss_capacity(i),'Steady state capacity of the system',
     &       '(W):', 0.0, 'F', 1000000.0, 'F', 100.0, 
     &       'Please enter a capacity greater than 0',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
      CALL EASKR(ss_efficiency(i), 
     &       'Steady state efficiency of the system', 
     &       '(1 for electric baseboards)', 0., 'F', 1.,'F', 1.0,
     &       'Please enter an efficiency between 1 and 0',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
      
      CALL EASKABCD(' ','circulation fan mode', 'No Fan Associated', 
     &              'Auto Mode', 'Continuous Mode','Cancel',
     &              ifan_operation(i),nbehlp)
      if(ifan_operation(i).eq.4) then !cancel selected
        ifan_operation(i) = 0
        exit=.true.
        return
      endif

      if(ifan_operation(i).ne.1)then
      
        CALL EASKR(fan_power(i), ' ', 'Fan power (W)',-1.,'F',1.E4,'F',
     &        100.0,'Invalid input, please see help file',ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
        CALL PHELPD('Continuous Mode Operation of Fan',nbhelp,'-',
     &     0,0,ier)
      else
        fan_power(i) = 0.
      endif
      CALL USRMSG(' ',' ','-') ! Clear Screen
      return
      end
     
C --------------------------END OF BASEBOARD---------------------------
C ---------------------------------------------------------------------
    
    
C -----------Air/Ground Source Heat Pump for Heating Input-------------
C HPHOT
C
C HPHOT reads in the priority of the system (priority), the type of heat pump, 
C whether ASHP, GSHP, or GCEP (hpunit), whther it has a furnace backup
C (fbackup), and the system number(i) from HVACGIN, and prompts the user for 
C all parameters associated with the heat pump system.  
C At present, the heat pump can be used as a heater with furnace backup, or for heating
C & cooling with furnace backup. All entered variables  are saved in the common hvac
C variables.  The user must actually save the hvac file for changes to be recorded.
C ---------------------------------------------------------------------

      SUBROUTINE HPHOT(hpunit,priority,fbackup,i,exit)
      

#include "building.h"
#include "geometry.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "ashp_common.h"

      common/C1/NCOMP, NCON

C Common it up here.

      INTEGER hpunit                  ! Passed system type from HVACGIN
      INTEGER ier                     ! Returned if there is an error/no options selected
      INTEGER I                       ! Identifier for current system
      INTEGER priority                ! Passed priority of system, backup or primary
      INTEGER fbackup                 ! Passed whether system has a furnace backup
      INTEGER NCOMP, NCON             ! Number of zones in system
      INTEGER sys_zone_number(ncomp)  ! Array of zone numbers associated with the system
      REAL fraction                   ! Fraction of power assigned to zones so far
      REAL ifraction                  ! 1-fraction, fraction which remains to be assigned
      INTEGER zonenum(ncomp)          ! Array of zone number associated with hvac
      CHARACTER*50 zoneopt(ncomp+1)   ! Chacter array of zone names associated with hvac
      LOGICAL exit                    ! Returns as true if user does not complete system definition

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HPHOT'  ! set for subroutine

C Need to put something here about the GCEP

      ier = 0
      
C assign as heating
      iunit_function(i) = 1

C assign whether ASHP, GSHP, GCEP      
      if(hpunit.eq.3) then 
        ihvac_type(i) = 7
      else if (hpunit.eq.4) then
        ihvac_type(i) = 8
        isys_type(i) = 4
      end if

C Select the priority of the system
      if(priority .eq. 0) then

        helptopic='zone_hvac_hp_primary'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EASKI(ipriority(i),'Specify the system priority',
     &             '1 for primary (default), 2 for backup', 
     &             1, 'F', 2, 'F', 1,
     &             'value must be 1 or 2',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
      else
        ipriority(i) = priority
      end if
      
      if(hpunit.eq.3) then
        helptopic='zone_hvac_hp_poor'
        call gethelptext(helpinsub,helptopic,nbhelp)
       
        CALL EASKABC(' ', 'Enter the heat pump unit type', 
     &     'ASHP Typical','ASHP Good','ASHP Poor',isys_type(i),
     &     nbhelp)
      end if
      
C Select the number of zones the system is linked to

      helptopic='zone_hvac_hp_zones'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      CALL EASKI(num_sys_zones(i),' ',
     &           'Total number of zones served by heat pump system:',
     &           1,'F',NCOMP,'F',1,
     &         'Enter a number of zones within the range of your model',
     &           ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
    
      K=1    
      do while (K.LE.NCOMP)
        WRITE(zoneopt(K),'(4A)') zname(K)
        zonenum(K)=K
        K=K+1
      enddo
      
      ival = num_sys_zones(i)       
      CALL EPICKS(ival,sys_zone_number,'','Select zones being supplied',
     &         30, ncomp, zoneopt,'Zones - see help for options',
     &         ier,nbhelp)
      if(ival.ne.num_sys_zones(i))then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
      
      k = 0
      do while (k.lt.num_sys_zones(i))
        k = k+1
        isys_zone_num(i,k) = sys_zone_number(k)

      end do

C The value 'isys_zone_num(i,k)' is the zone number of the zone picked in the epicks
C menu above, therefore this is the value that needs to be written to the 
C HVAC file in 'zones being supplied' section.
      fraction = 0
      ifraction = 1
      k=1
      
      helptopic='zone_hvac_hp_zfrac'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      do while (k.LE.num_sys_zones(i))
        ifraction = 1-fraction
        nnm=sys_zone_number(k)   
        CALL EASKR(sys_zone_cap_frac(i,k),  
     &    'Select fraction supplied to',zname(nnm),
     &    0.,'F',ifraction, 'F',ifraction, 'Fraction', ier,nbhelp)     
        fraction = fraction + sys_zone_cap_frac(i,k)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
        
C Check to make sure sum of fractions is 1. If not, redirect back to top
C of loop - can be added later when fractions matter        
        if ((k .eq. num_sys_zones(i)) .and. (fraction .ne. 1)) then
          CALL PHELPD('Unequated fractions',nbhelp,'-',0,0,ier)
          k = 1
          fraction = 0
        else
          K=K+1
        end if
      end do


C Set heat pump capacity       
      helptopic='zone_hvac_hp_data'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      CALL EASKR(ss_capacity(i),'Steady state capacity of the system',
     &           '(W):', 0., 'F', 1.E6, 'F', 100.,
     &           'Enter a capacity greater than 0',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C Set heat pump COP       
      CALL EASKR(ss_cop(i),' ','Heat pump COP', 0., 'F', 1000., 'F', 
     &           0., 'COP', ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if

C Set heat pump flow rate
      CALL EASKR(flow_rate(i),' ','Flow Rate',-1., 'F', 10.0, 'F', 0., 
     &           'Flow rate.', ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
      
C Set flow rate at rating conditions.
      CALL EASKR(flow_rate_r(i),' ',
     &           'Flow Rate at rating conditions',-1.,'F', 10.0,'F',0.,
     &           'Rated Flow rate.', ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
      
      
C set circulation fan mode
      CALL EASKABCD(' ','circulation fan mode', 'No Fan Associated', 
     &              'Auto Mode', 'Continuous Mode','Cancel',
     &              ifan_operation(i), nbhelp)
      if(ifan_operation(i).eq.4) then !cancel selected
        ifan_operation(i) = 0
        exit=.true.
        return
      endif
       
      if(ifan_operation(i).ne.1)then

C set circulation fan position
        CALL EASKI(ifan_position(i),
     &         'Specify the circulation fan position',
     &         '1 for blow through (default), 2 for draw through',
     &         1, 'F', 2, 'F', 1, 'value must be 1 or 2', ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if

C set circulation fan power
        CALL EASKR(fan_power(i), ' ', 'Circulation Fan power (W)', 
     &             -1., 'F', 1.E4,'F', 100.,'Circulation Fan',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
 
        CALL PHELPD('Continuous Mode Operation of Fan',nbhelp,'-',
     &    0,0,ier)
        
        
      
C set outdoor  fan power
        CALL EASKR(outdoor_fan_power(i), ' ',
     &             'Outdoor (evaporator) Fan power (W)', 
     &             -1., 'F', 1.E4,'F', 100., 'Outdoor fan power',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
C set auto mode  fan power     
        CALL EASKR(fan_power_auto(i),' ',
     &             'Fan power associated with auto mode',
     &             -1., 'F', 1.E4,'F',100., 'Auto Fan Power',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
 
C set circulation fan position during rating test
        CALL EASKI(ifan_position_r(i), 
     &             'circulation fan position during rating test', 
     &             '1 for blow through (default), 2 for draw through',
     &             1, 'F', 2, 'F', 1, 'value must be 1 or 2', ier,
     &             nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
C set circulation mode  fan power during rating test
        CALL EASKR(fan_power_r(i),' ',
     &             'circulation fan power during rating test',
     &             -1., 'F', 1.E4,'F',100.,
     &             'Invalid input, please see help file',ier,
     &             nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
      else !no fan, but required to but a rated fan position as per Kamel's code in HVACInput.F in the cetc folder
        fan_power(i) = 0.
        outdoor_fan_power(i) = 0.
        fan_power_auto(i) = 0.
        fan_power_r(i) = 0.
        ifan_position_r(i) = 1
        ifan_position(i) = 1
      end if

C Temperature control
      helptopic='zone_hvac_hp_temp_ctl'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(itemp_control(i), 'Temperature Control', 
     &           '1 - balanced point, 2 - Restricted, 3 - Unrestricted',
     &           1,'F',3,'F',1,'value must be 1,2, or 3', ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C cutoff temperature
      CALL EASKR(cutoff_temp(i),' ','Cutoff T in C',-1., 'F', 50.,'F',
     &           50., 'Cutoff Temperature',ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if

C Backup system data has to be listed before HP data in hvac file      
      if (fbackup .eq. 1) then
        ibackup_heat(i)=1
        ibackup_sys_num(i)=i-1
      else
        ibackup_heat(i)=0
        ibackup_sys_num(i)=0
      end if
      CALL USRMSG(' ',' ','-') ! Clear Screen
      
      return
      end
      

C -------------------------------END OF--------------------------------
C -----------Air/Ground Source Heat Pump for Heating Input-------------
C ---------------------------------------------------------------------
     
C --------------------------HP for Cooling-----------------------------
C HPCOLD
C
C HPCOLD reads in the priority of the system (priority), it's function, 
C whether cooling, or both heating & cooling (hpunit), whther it has a furnace backup
C (fbackup), and the system number(i), and whether there is an associated heat pump for
C heating (heatok) from HVACGIN, and prompts the user for all parameters associated with 
C the heat pump system.  
C At present, the heat pump can be used for cooling with furnace backup, for heating
C & cooling with furnace backup, or as an associated a/c to a furnace. 
C All entered variables  are saved in the common hvac variables.  The user must actually 
C save the hvac file for changes to be recorded.
C ---------------------------------------------------------------------
      SUBROUTINE HPCOLD(hpunit,priority,fbackup,i,heatok,exit)
      

#include "building.h"
#include "geometry.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "ashp_common.h"

C    fbackup = 1 if there is a backup, 0 if not

      common/C1/NCOMP, NCON

C Common it up here.

      INTEGER hpunit                 ! Passed system type from HVACGIN
      INTEGER ier                    ! Returned if there is an error/no options selected
      INTEGER I                      ! Identifier for current system
      INTEGER priority               ! Passed priority of system, backup or primary
      INTEGER fbackup                ! Passed whether system has a furnace backup
      INTEGER NCOMP, NCON            ! Number of zones in system
      INTEGER sys_zone_number(ncomp) ! Array of zone numbers associated with the system
      REAL fraction                  ! Fraction of power assigned to zones so far
      REAL ifraction                 ! 1-fraction, fraction which remains to be assigned
      INTEGER zonenum(ncomp)         ! Array of zone number associated with hvac
      CHARACTER*50 zoneopt(ncomp+1)  ! Chacter array of zone names associated with hvac
      LOGICAL exit                   ! returns as true if user does not complete system definition
      INTEGER heatok                 ! Passed identifier of whether there is an associated heating system
      INTEGER day                    ! identifier for current day type
      INTEGER period                 ! Identifier for current period type
      CHARACTER*6 dayout             ! Variable used for writing out current day
      CHARACTER*9 periodout          ! Variable used for writing out current period
      INTEGER period_counter         ! Variable used for counting up until total number of periods

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HPCOLD'  ! set for subroutine
      
      ier =0
      exit=.false.
      
C assign as cooling
      iunit_function(i) = 2
C assign whether ASHP, GSHP, GCEP      
      if(hpunit.eq.3) then 
        ihvac_type(i) = 7
      else if (hpunit.eq.4) then
        ihvac_type(i) = 8
        isys_type(i) = 4
      end if
C Select the priority of the system
      if(priority .eq. 0) then

        helptopic='zone_hvac_hp_primary'
        call gethelptext(helpinsub,helptopic,nbhelp)

        CALL EASKI(ipriority(i),'Specify the system priority',
     &             '1 for primary (default), 2 for backup',1,
     &             'F',2,'F',1,'value must be 1 or 2',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
      else
        ipriority(i) = priority
      end if
      
      if(hpunit.eq.3) then
        helptopic='zone_hvac_hp_poor'
        call gethelptext(helpinsub,helptopic,nbhelp)
       
        CALL EASKABC(' ', 'Enter the heat pump unit type', 
     &    'ASHP Typical','ASHP Good','ASHP Poor',isys_type(i),nbhelp)
      end if
     
C Select the number of zones the system is linked to
      helptopic='zone_hvac_hp_zones'
      call gethelptext(helpinsub,helptopic,nbhelp)
   
      CALL EASKI(num_sys_zones(i),' ',
     &           'Total number of zones served by heat pump system:',
     &           1,'F',NCOMP,'F',1,
     &          'Enter a number of zones within the range of your model'
     &           ,ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
    
      K=1    
      do while (K.LE.NCOMP)
        WRITE(zoneopt(K),'(4A)') zname(K)
        zonenum(K)=K
        K=K+1
      enddo
      
      ival = num_sys_zones(i) 
      CALL EPICKS(ival, sys_zone_number, ' ',
     &            'Select zones being supplied:',30,ncomp,zoneopt,
     &            'Zones - see help for options',ier,nbhelp)
      if(ival.eq.0) then !nothing selected
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      k = 0
      do while (k.lt.num_sys_zones(i))
        k = k+1
        isys_zone_num(i,k) = sys_zone_number(k)

      end do

C The value 'isys_zone_num(i,k)' is the zone number of the zone picked in the epicks
C menu above, therefore this is the value that needs to be written to the 
C HVAC file in 'zones being supplied' section.
      fraction = 0
      ifraction = 1
      k=1
      
      do while (k.LE.num_sys_zones(i))
        ifraction = 1-fraction
        nnm=sys_zone_number(k)   
        CALL EASKR(sys_zone_cap_frac(i,k),  
     &        'Select fraction supplied to',zname(nnm),
     &        0.,'F',ifraction, 'F',ifraction, 'Fraction', ier,nbhelp)     
        fraction = fraction + sys_zone_cap_frac(i,k)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
        
C Check to make sure sum of fractions is 1. If not, redirect back to top
C of loop - can be added later when fractions matter        
        if ((k .eq. num_sys_zones(i)) .and. (fraction .ne. 1)) then
          CALL PHELPD('Unequated fractions',nbhelp,'-',0,0,ier)
          k = 1
          fraction = 0
        else
          K=K+1
        end if
      end do


C Set heat pump capacity       
      helptopic='zone_hvac_hp_zfrac'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      CALL EASKR(ss_capacity(i),'Steady state capacity of the system',
     &           '(W):', 0.0, 'F', 1000000.0, 'F', 100.0, 
     &           'Please enter a capacity greater than 0',ier,nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C Set heat pump COP       
      CALL EASKR(ss_cop(i),' ','Heat pump COP', 0., 'F', 1000., 'F', 
     &           0., 'COP', ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if

C Set heat pump flow rate
      CALL EASKR(flow_rate(i),' ','Flow Rate',-1., 'F', 10.0, 'F', 0., 
     &           'Flow rate.', ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C Set flow rate at rating conditions.
      CALL EASKR(flow_rate_r(i),' ',
     &           'Flow Rate at rating conditions',-1.,'F', 10.0, 'F',0.,
     &           'Rated Flow rate.', ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if

C set circulation fan mode
      CALL EASKABCD(' ','circulation fan mode', 'No Fan Associated', 
     &              'Auto Mode', 'Continuous Mode','Cancel',
     &              ifan_operation(i),nbhelp)
      if(ifan_operation(i).eq.4) then ! cancel selected
        ifan_operation(i) = 0
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      endif
      if(ifan_operation(i).ne.1)then

C set circulation fan position
        CALL EASKI(ifan_position(i),
     &             'Specify the circulation fan position', 
     &             '1 for blow through (default), 2 for draw through',
     &             1, 'F',2,'F',1,'value must be 1 or 2',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if

C set circulation fan power
        CALL EASKR(fan_power(i), ' ', 'Circulation Fan power (W)', 
     &             -1., 'F', 1.E4,'F', 100.,'Circulation Fan',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
 
        CALL PHELPD('Continuous Mode Operation of Fan',nbhelp,'-',
     &    0,0,ier)
      
C set outdoor  fan power
        CALL EASKR(outdoor_fan_power(i), ' ', 'Outdoor Fan power (W)', 
     &             -1., 'F', 1.E4,'F', 100., 'Outdoor fan power',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
C set auto mode  fan power     
        CALL EASKR(fan_power_auto(i),' ',
     &             'Fan power associated with auto mode',
     &             -1.,'F',1.E4,'F',100.,'Auto Fan Power',
     &             ier, nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
 
C set circulation fan position during rating test
        CALL EASKI(ifan_position_r(i),
     &             'circulation fan position during rating test',
     &             '1 for blow through (default), 2 for draw through',
     &             1,'F',2,'F',1,'value must be 1 or 2',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
C set circulation mode  fan power during rating test
        CALL EASKR(fan_power_r(i),' ',
     &             'circulation fan power during rating test',
     &             -1.,'F',1.E4,'F',100.,
     &             'Invalid input, please see help file',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
      else !no fan, but required to put a position value for the rated fan as per Kamel's code in HVACInput.F in the CETC folder
        fan_power(i) = 0.
        outdoor_fan_power(i) = 0.
        fan_power_auto(i) = 0.
        fan_power_r(i) = 0.
        ifan_position_r(i) = 1
        ifan_position(i) = 1
      end if
      
C sensible heat ratio
      helptopic='zone_hvac_hp_sensible'
      call gethelptext(helpinsub,helptopic,nbhelp)
      
      CALL EASKR(shrr(i),' ','Sensible Heat Ratio',
     &           0., 'F', 1.0,'F', 100., 'Heat Ratio',ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C Cooling Type
      CALL EASKI(icooling_type(i),'Cooling Type',
     &           '1 - conventional, 2 - economizer',1,'F',2,'F',1,
     &           'Cooling Type', ier, nbhelp)
      if(ier.eq.-3)then
        exit=.true.
        CALL USRMSG(' ',' ','-') ! Clear Screen
        return
      end if
     
C If Conventional
      if(icooling_type(i).eq.1) then
        period_counter = 0

C Number of Outdoor Air Day Types
        CALL EASKI(num_out_air_day_types(i),' ', 
     &            'Number of Outdoor Air Day Types',1
     &            ,'F',365,'F',1,'Air Types',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
       
        day=0
        do while (day.ne.num_out_air_day_types(i))

C Number of different periods in each day type.  This entry and the next
C for as many times as there are day types
          day = day +1
          write(dayout,'(A4,I2)') "day", day
          period = 0
       
          CALL EASKI(num_outside_air_day_periods(i,day),
     &               'Number of different periods in',
     &               dayout,1,'F',10,'F',1,'Periods',ier,nbhelp)
          if(ier.eq.-3)then
            exit=.true.
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          else
            itotal_num_day_periods = itotal_num_day_periods + 
     &      num_outside_air_day_periods(i,day)
          end if
       
          CALL EASKI(iend_hour_out_air_daytype(i,day),
     &               'Hour of the year end for end day type ',dayout, 
     &               1,'F',8760,'F',1,'Hour of year ',ier,nbhelp)
          if(ier.eq.-3)then
            exit=.true.
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          end if

C Number of end hour for each period in each day type.  This entry and the next for as many times as there are period types

          do while (period.ne.num_outside_air_day_periods(i,day))
            period_counter = period_counter + 1
            period = period+1
            write(periodout,'(I2)') period
C End hour for period m for day type n
            CALL EASKI(iend_hour_out_air_dayperiod(i,period_counter), 
     &                 'End hour for period',periodout,
     &                 1,'F',24,'F',1,'Periods',ier,nbhelp)
            if(ier.eq.-3)then
              exit=.true.
              CALL USRMSG(' ',' ','-') ! Clear Screen
              return
            end if
         
C Outdoor air flow rate for m for day type n         
            CALL EASKR(day_period_out_air_flow(i,period_counter),
     &                 'Outdoor air flow rate for period', periodout,
     &                 0.,'F',10.,'F',1.,'Periods',ier,nbhelp)
            if(ier.eq.-3)then
              exit=.true.
              CALL USRMSG(' ',' ','-') ! Clear Screen
              return
            end if
          end do
        end do
      else
C Economizer Option
C Economizer control type       
        helptopic='zone_hvac_hp_economizer'
        call gethelptext(helpinsub,helptopic,nbhelp)
       
        CALL EASKI(ieconomizer_control(i), 'Economizer Control Type ',
     &             'See help for details',1,'F',4,'F',1,
     &             'Economizer Type',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
       
        CALL EASKR(economizer_min_out_air_flow(i),
     &       'Economizer minimum outdoor air flow',
     &       'm3/s',0.,'F',10.0,'F',100.0,'Min. Air flow.',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
        CALL EASKR(economizer_ind_temp_setpoint(i),
     &       'Economizer indoor space set point',
     &       'C',0.,'F',100.0,'F',22.0,'Indoor Set Point',ier,nbhelp)
        if(ier.eq.-3)then
          exit=.true.
          CALL USRMSG(' ',' ','-') ! Clear Screen
          return
        end if
     
        if (ieconomizer_control(i).eq.1 .or. 
     &    ieconomizer_control(i).eq.2) then
       
          CALL EASKR(economizer_out_temp_limitcon(i),
     &           'Economizer outdoor temperature limit',
     &           'C',0.,'F',100.0,'F',25.0,'Temp limit',ier,nbhelp)
          if(ier.eq.-3)then
            exit=.true.
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          end if
     
        else
          CALL EASKR(economizer_out_enth_limitcon(i),
     &        'Economizer outdoor enthalpy limit control',
     &        'J/kg',0.,'F',100.0,'F',50.0,'enthalpy limit',ier,nbhelp)
          if(ier.eq.-3)then
            exit=.true.
            CALL USRMSG(' ',' ','-') ! Clear Screen
            return
          end if
   
        end if
      end if
      if (heatok.eq.1)then
        isys_num_heating(i)=i-1
      else
        isys_num_heating(i)=0
      end if
      
      helptopic='zone_hvac_hp_z_ctl'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(' ', 
     &           'control function type associated with cooling system', 
     &           'Ideal Controller', 'On/Off with Deadband',
     &           icontrol_function(i),nbhelp)

      CALL USRMSG(' ',' ','-') ! Clear Screen
      return
      end

C ---------------------------------------------------------------------
C FURNACE_WRITE
C 
C Subroutine for writing furnace system information, stored in global
C variables, to the idealized HVAC (.hvac) file
C ---------------------------------------------------------------------

      SUBROUTINE FURNACE_WRITE(i,hvacunit)

#include "building.h"      
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

      INTEGER i, hvacunit,k
      k = 0
C '(3i,10x,a10)'
      write(hvacunit,*)ihvac_type(i), ipriority(i), 
     &  num_sys_zones(i),"# HVAC system type; priority; # zones served"
      write(hvacunit,*)isys_type(i), ifuel_type(i),
     & (isys_zone_num(i,k),sys_zone_cap_frac(i,k), k=1,
     &  num_sys_zones(i)), ss_capacity(i), ss_efficiency(i),
     &  ifan_operation(i), fan_power(i), draft_fan_power(i),
     &  pilot_power(i), iduct_sys_flag(i), 
     &  "# type; fuel; 1st zone; fraction to 1st; 2nd zone; ", 
     &  "fraction to 2nd; SS capacity; eff; fan ops; fan power; ",
     &  "pilot pwr; duct"
     
      return
      end
      
C ---------------------------------------------------------------------
C BASEBOARD_WRITE
C 
C Subroutine for writing baseboard system information, stored in global
C variables, to the idealized HVAC (.hvac) file
C ---------------------------------------------------------------------

      SUBROUTINE BASEBOARD_WRITE(i,hvacunit)

#include "building.h"      
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"

      INTEGER i, hvacunit,k
      k = 0

      write(hvacunit,*)ihvac_type(i), ipriority(i), 
     &  num_sys_zones(i),
     &  "# HVAC system type; priority; # zones served"
      write(hvacunit,*)(isys_zone_num(i,k),sys_zone_cap_frac(i,k),k=1,
     &  num_sys_zones(i)), ss_capacity(i), ss_efficiency(i),
     &  ifan_operation(i),   
     &  fan_power(i), "#1st zone; fract.to 1st; 2nd zone; fract.to 2nd;"
     &  ," cap; eff; fan ops; fan power;"
     
      return
      end

C ---------------------------------------------------------------------
C HPHOT_WRITE
C
C Subroutine for writing heat pump (heating mode) system information, 
C stored in global variables, to the idealized HVAC (.hvac) file
C ---------------------------------------------------------------------

      SUBROUTINE HPHOT_WRITE(i,hvacunit)

#include "building.h"      
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"

      INTEGER i, hvacunit,k
      k=0

       write(hvacunit,*)ihvac_type(i), ipriority(i), 
     &  num_sys_zones(i),
     &  "# HVAC system type; priority; # zones served"
      write(hvacunit,*)iunit_function(i), isys_type(i), 
     & (isys_zone_num(i,k),sys_zone_cap_frac(i,k), k=1,
     &  num_sys_zones(i)), "# function; type; 1st zone; fraction; ",
     &  "2nd zone; fraction;"
      write(hvacunit,*)ss_capacity(i), ss_cop(i), "# capacity; COP;"
      write(hvacunit,*)flow_rate(i), flow_rate_r(i), ifan_operation(i), 
     &  ifan_position(i),fan_power(i), 
     &  outdoor_fan_power(i),fan_power_auto(i), ifan_position_r(i), 
     &  fan_power_r(i), "#flow rate; rated flow rate;", 
     & " circulation fan mode; position; power; outfoor fan mode;",
     &  " power; auto fan power; rated fan position; power"
      write(hvacunit,*) itemp_control(i), cutoff_temp(i),
     &  ibackup_heat(i),  ibackup_sys_num(i), "# temperature control", 
     &  "cutoff temperature,backup type, backup system number"
     
      return
      end     

C ---------------------------------------------------------------------
C HPCOLD_WRITE
C
C Subroutine for writing heat pump (cooling mode) system information, 
C stored in global variables, to the idealized HVAC (.hvac) file
C ---------------------------------------------------------------------

      SUBROUTINE HPCOLD_WRITE(i,hvacunit)

#include "building.h"      
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"

      INTEGER i, hvacunit, ival, k, j

       write(hvacunit,*)ihvac_type(i), ipriority(i), 
     &  num_sys_zones(i),
     &  "# HVAC system type; priority; # zones served"
      write(hvacunit,*)iunit_function(i), isys_type(i), 
     & (isys_zone_num(i,k),sys_zone_cap_frac(i,k), k=1,
     &  num_sys_zones(i)), "# function; type; 1st zone; fraction; ",
     &  "2nd zone; fraction;"
      write(hvacunit,*)ss_capacity(i), ss_cop(i), "# capacity; COP;"
      write(hvacunit,*)flow_rate(i), flow_rate_r(i), 
     &  ifan_operation(i), ifan_position(i),fan_power(i),
     &  outdoor_fan_power(i),
     &  fan_power_auto(i), ifan_position_r(i), fan_power_r(i),
     &  "#flow rate; rated flow rate; circulation fan mode; position;", 
     &  " power; outfoor fan mode; power; auto fan power;",
     &  " rated fan position; power"
      write(hvacunit,*)shrr(i), icooling_type(i), "#Sensible heat ratio"
     & ," cooling type"    
      if(icooling_type(i).eq.1) then
        write(hvacunit,*)num_out_air_day_types(i),"#number of day types"
        write(hvacunit,*)(num_outside_air_day_periods(i,k),
     &    iend_hour_out_air_daytype(i,k), k=1, 
     &    num_out_air_day_types(i)), 
     &    "#number of periods in day; end hour of day"
        k =1
        j =1
        ival = 1
        do while(k.le.num_out_air_day_types(i))
          write(hvacunit,*) (iend_hour_out_air_dayperiod(i,j),
     &      day_period_out_air_flow(i,j),
     &      j=ival,
     &      (ival+num_outside_air_day_periods(i,k)-1)), 
     &      '#end hour of period; flow rate' 
          ival = ival + num_outside_air_day_periods(i,k)
          k = k+1
        end do
      else
        write(hvacunit,*) ieconomizer_control(i), 
     &       "#economizer control type"
        if (ieconomizer_control(i) .eq. 1 .or. 
     &   ieconomizer_control(i) .eq. 2)  then
          write(hvacunit,*)economizer_min_out_air_flow(i),
     &      economizer_ind_temp_setpoint(i), 
     &      economizer_out_temp_limitcon(i), 
     &      "#min. flow; indoor set point; outdoor temp limit"
        else
          write(hvacunit,*)economizer_min_out_air_flow(i),
     &      economizer_ind_temp_setpoint(i), 
     &      economizer_out_enth_limitcon(i), "#min. flow;",
     &      " indoor set point; outdoor enthalpy limit"
        end if
      end if
      write(hvacunit,*)isys_num_heating(i), icontrol_function(i),
     &  "#number of associated heating system, associated ctl function"
      return
      end

C ---------------------------------------------------------------------
C ------------------------HVAC SUMMARY MENU----------------------------
C HVAC_SUMMARY
C 
C Subroutine for writing main hvac summary menu showing all systems
C described in the .hvac file. If one of the systems shown is chosen,
C an alternate summary menu appears showing specific characteristics
C for the HVAC system chosen. Also gives user the option to add a new
C hvac system into the current hvac file (maximum of 5).
C ---------------------------------------------------------------------

      SUBROUTINE HVAC_MAIN_SUMMARY(edit,add,save_flag,exit_flag)

#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"

      COMMON/hvacinfo/ihvacflag,hvacfile
      
      CHARACTER*72 hvacfile                       ! name of hvacfile
      CHARACTER*10 hvacnames(9)                   ! names of hvac systems in order corresponding to global variables
      CHARACTER*32 hvac_summary(10)               ! array of items in the hvac summary menu
      INTEGER i                                   ! counter for the do loop that sets up the menu based on the number of systems present
      INTEGER ipck                                ! selection made from the menu
      INTEGER edit                                ! integer returned to hvacgin of the system number that should be edited, 0 if none
      INTEGER nhvac_summary                       ! number of items in the hvac summary menu
      INTEGER system_number                       ! system number chosen from main menu
      INTEGER ihvacflag                           ! flags whether an hvac file exists
      
      LOGICAL add,save_flag                       ! logical returned to main HVAGIN as true if user chooses to save or add
      LOGICAL editflag                            ! logical returned from HVAC_SYS_SUMMARY as true if user chooses to edit a system
      LOGICAL exit_flag                           ! logical returned to main HVAGIN as true if user chooses to exit
      LOGICAL ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HVAC_MAIN_SUMMARY'  ! set for subroutine

      nhvac_summary=10
      edit=0
      ipck=0
      editflag=.false.
      exit_flag=.false.
      save_flag=.false.
      add=.false.
      
C Set up the hvacnames array
      hvacnames(1) = 'Furnace  '
      hvacnames(2) = 'Boiler   '
      hvacnames(3) = 'Baseboard'
      hvacnames(7) = 'ASHP     '
      hvacnames(8) = 'GSHP     '
      hvacnames(9) = 'GCEP     '
      
C Set up the main HVAC summary menu depending on the number of systems present in the hvac file
      do while (ipck.ne.nhvac_summary)
        ipck=-4
        
C       DEBUG
        do 10 i=1,5
          if(i.le.num_hvac_sys)then
            if (iunit_function(i).eq.1) then
              WRITE(hvac_summary(i),'(A,I1,A,A,A)')'System ',i,': ',
     &              hvacnames(ihvac_type(i)),' (Heating)'   
            elseif(iunit_function(i).eq.2) then  
              WRITE(hvac_summary(i),'(A,I1,A,A,A)')'System ',i,': ',
     &              hvacnames(ihvac_type(i)),' (Cooling)'    
            else
              WRITE(hvac_summary(i),'(A,I1,A,A)')'System ',i,': ',
     &              hvacnames(ihvac_type(i))
            endif
          else
            hvac_summary(i)='--'
          endif
   10   continue

        hvac_summary(6) ='______________________'
        hvac_summary(7) ='a Add a new system    '
        hvac_summary(8) ='s Save hvac file      '
        hvac_summary(9) ='? help                '
        hvac_summary(10)='- exit                '
        
C Help text for the main hvac summary menu
        helptopic='zone_hvac_sys_types'
        call gethelptext(helpinsub,helptopic,nbhelp)

C Create the menu display
        CALL EMENU('HVAC Summary',hvac_summary,nhvac_summary,ipck)
      
C If one of the systems is chosen, call the system summary routine
        if(ipck.le.0)then
          ! redraw menu if clicked off screen
        elseif (ipck.le.num_hvac_sys)then
          system_number=ipck
          call HVAC_SYS_SUMMARY(hvacnames(ihvac_type(system_number)),
     &                 system_number,ihvac_type(system_number),editflag)
        elseif(ipck.eq.10)then
          exit_flag=.true.
          CALL ASKOK('You will lose new info. if file hasnt been saved', 
     &          'Save file?', ok, dok, -1)
          if(ok) then
            save_flag = .true.
            exit
          else
            save_flag = .false.
          end if
          exit
        elseif(ipck.eq.9)then
          helptopic='zone_hvac_sys_types'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call PHELPD('Main HVAC Summary Menu:',nbhelp,'-',0,0,IER)
        elseif(ipck.eq.8)then
          save_flag=.true.
          exit
        elseif(ipck.eq.7)then
          if(num_hvac_sys .eq. 5) then
            CALL PHELPD('Too many HVAC Sys',nbhelp, '-', 0, 0, ier)
          else
            add=.true.
            exit
          end if
        endif
        if(editflag)then
          edit=ipck
          exit
        endif
      enddo
      CALL USRMSG(' ',' ','-') ! Clear Screen
      return
      end

C -----------------------------END OF----------------------------------
C ------------------------HVAC SUMMARY MENU----------------------------
C ---------------------------------------------------------------------

C ---------------------------------------------------------------------
C ---------------------HVAC SYSTEM SUMMARY MENU------------------------
C SYS_SUMMARY
C 
C Subroutine for writing lower level summary menu for specific system
C characteristics for the available hvac systems. Also gives user the
C option to edit the system chosen.
C ---------------------------------------------------------------------

      SUBROUTINE HVAC_SYS_SUMMARY(sys_name,sys_number,sys_type,editflag)
      
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"

      INTEGER nsys_summary                   ! number of items in system summary
      PARAMETER (nsys_summary=12)            ! set number here
      INTEGER sys_number                     ! system number passed from main menu
      INTEGER sys_type                       ! system type (integer) passed from main menu
      INTEGER ipick                          ! selection made from the system menu
      CHARACTER*9 sys_name                   ! system name passed from main menu
      CHARACTER*28 furntyp(14)               ! array of furnace types
      CHARACTER*11 fueltyp(5)                ! array of fuel types
      CHARACTER*38 sys_summary(nsys_summary) ! array of items in system summary
      CHARACTER*7 unitfunction(2)            ! array of heat pump modes
      LOGICAL editflag                       ! return to main summary as true if user wishes to edit current system

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='HVAC_SYS_SUMMARY'  ! set for subroutine
      
      editflag=.false.
      ipick=0
      
C Set up the furnace types array
      furntyp(1)= 'Advanced airtight wood stove'
      furntyp(2)= 'Catalytic converter'
      furntyp(3)= 'Flame retention head'
      furntyp(4)= 'Flue vent damper'
      furntyp(5)= 'Spark-ignition'
      furntyp(6)= 'Spark ignition (vent damper)'
      furntyp(7)= 'Continuous pilot'
      furntyp(8)= 'Condensing furnace'
      furntyp(9)= 'Direct vent non-condensing'
      furntyp(10)='Induced draft'
      furntyp(11)='Mid efficiency'
      furntyp(12)='Electric forced-air'
      furntyp(13)='Wood furnace'
      furntyp(14)='Oil furnace or boiler'
      
C Set up the heat pump mode array
      unitfunction(1) = 'Heating'
      unitfunction(2) = 'Cooling'
      
C Set up the fuel types array
      fueltyp(1)='Electric'
      fueltyp(2)='Natural Gas'
      fueltyp(3)='Oil'
      fueltyp(4)='Propane'
      fueltyp(5)='Wood'
      
      do while (ipick.ne.nsys_summary)
        ipick=-4
C Set up the system summary menu depending on the system type
        WRITE(sys_summary(1),'(A,I1,A,A)') 'System #',sys_number,': ',
     &                                      sys_name
        if(sys_type.eq.1)then
          WRITE(sys_summary(2),'(A,A)') 'Furnace: ',
     &                                   furntyp(isys_type(sys_number))
        else
          sys_summary(2)=' '
        endif
C If system is a furnace, display the fuel type, otherwise disply nothing
        if(sys_type.eq.1)then
          WRITE(sys_summary(3),'(A,A)') 'Fuel Type: ',
     &                                   fueltyp(ifuel_type(sys_number))
        elseif(sys_type.ge.7)then
          WRITE(sys_summary(3),'(A,A)') 'System Function: ',
     &                          unitfunction(iunit_function(sys_number))
        else
          sys_summary(3)=' '
        endif
        WRITE(sys_summary(4),'(A,I1)') 'Priority = ',
     &                                  ipriority(sys_number)
        WRITE(sys_summary(5),'(A,I1)') '# Zones Served = ',
     &                                  num_sys_zones(sys_number)
        WRITE(sys_summary(6),'(A,E10.2)') 'Capacity (W) = ',
     &                                  ss_capacity(sys_number)
C If the system is a furnace of baseboard, display the steady state
C efficiency, otherwise display the steady state COP
        if(sys_type.le.3)then
          WRITE(sys_summary(7),'(A,F5.3)') 'Steady state Efficiency = ',
     &                                    ss_efficiency(sys_number)
        else
          WRITE(sys_summary(7),'(A,F5.3)') 'Steady state COP = ',
     &                                    ss_cop(sys_number)
        endif
        sys_summary(8) ='_____________________________'
        sys_summary(9) ='e Edit system                '
        sys_summary(10)='d Delete system              '
        sys_summary(11)='? help                       '
        sys_summary(12)='- Back to main HVAC summary  '
        
C Help text for the main hvac summary menu
        helptopic='zone_hvac_sys_summary'
        call gethelptext(helpinsub,helptopic,nbhelp)
      
C Create the system summary menu display
        CALL EMENU('System Summary',sys_summary,nsys_summary,ipick)
      
        if(ipick.le.0)then
          ! redraw menu if clicked off screen
        elseif(ipick.eq.nsys_summary)then
          exit
        elseif(ipick.eq.nsys_summary-1)then
          helptopic='zone_hvac_sys_summary'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call PHELPD('System Summary Menu:',nbhelp,'-',0,0,IER)
        elseif(ipick.eq.nsys_summary-2)then
          ! do something here that will delete the system
          call PHELPD('Currently not an option but will be added',
     &                nbhelp,'-',0,0,IER)
        elseif(ipick.eq.nsys_summary-3)then
          editflag=.true.
          exit
        endif
        
      enddo
      CALL USRMSG(' ',' ','-') ! Clear Screen
      return
      end

C ------------------------------END OF---------------------------------
C ---------------------HVAC SYSTEM SUMMARY MENU------------------------
C ---------------------------------------------------------------------

C ---------------------------------------------------------------------
C ---------------------------HVAC ZEROING------------------------------
C HVAC_ZERO
C
C Subroutine for clearing all common global variables in hvac_common.h, 
C furnace_common.h and ashp_common.h, used by the files C hvacinput.F 
C and hvacgui.F
C ---------------------------------------------------------------------

      SUBROUTINE HVAC_ZERO()
      
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"
#include "furnace_common.h"
#include "ashp_common.h"

      num_hvac_sys=0
      site_altitude=0
      itotal_num_day_periods=0
      output_flag=.false.

C Clear all common variables of dimension (max_sys)
      do 10 i=1,max_sys
C Clear all common variables in furnace_common.h
        ss_efficiency(i)=0.0
        pilot_power(i)=0.0
        af(i)=0.0
        bf(i)=0.0
        cf(i)=0.0
        df(i)=0.0
C Clear all common variables in ashp_common.h
        iunit_function(i)=0
        ss_cop(i)=0.0
        shrr(i)=0.0
        time_constant(i)=0.0
        itemp_control(i)=0
        cutoff_temp(i)=0.0
        ibackup_heat(i)=0
        ibackup_sys_num(i)=0
        icooling_type(i)=0
        bfr(i)=0.0
        isys_num_heating(i)=0
        ahp(i)=0.0
        bhp(i)=0.0
        chp(i)=0.0
        dhp(i)=0.0
        ieconomizer_control(i)=0
        economizer_min_out_air_flow(i)=0.0
        economizer_ind_temp_setpoint(i)=0.0
        economizer_out_temp_limitcon(i)=0.0
        economizer_out_enth_limitcon(i)=0.0
        outdoor_fan_power(i)=0.0
        total_elec_consumption(i)=0.0
        elec_compressor(i)=0.0
        elec_outdoor_fan(i)=0.0
        elec_indoor_fan(i)=0.0
        average_cop(i)=0.0
        coil_sensible_load(i)=0.0
        coil_latent_load(i)=0.0
        coil_total_load(i)=0.0
C Clear all common variables in hvac_common.h
        ipriority(i)=0
        num_sys_zones(i)=0
        ss_capacity(i)=0.0
        flow_rate(i)=0.0
        ifan_operation(i)=0
        ifan_position(i)=0
        fan_power(i)=0.0
        fan_power_auto(i)=0.0
        ifan_position_r(i)=0
        fan_power_r(i)=0.0
        draft_fan_power(i)=0.0
        iduct_sys_flag(i)=0
        icontrol_function(i)=0
        izonecontrol(i)=0
        iFuel_type(i)=0
        sys_on_time(i)=0.0
        ihrs_under_heat(i)=0
        ihrs_under_cool(i)=0
        fASHP_AC_Coil_Load_Sensible(i)=0.0
        fASHP_AC_Coil_Load_Latent(i)=0.0
        fASHP_AC_Coil_Load_Total(i)=0.0
        fH3KActualCapacity(i)=0.0
        iLinkedSystem(i)=0
        bLinked(i)=.false.
        bSuppressThisSystem(i)=.false.
        bTwoModes(i)=.false.
        num_out_air_day_types(i)=0
C Clear all common variable matrices using MCOM in hvac_common.h
        do 20 j=1,mcom
          isys_zone_num(i,j)=0
          sys_zone_cap_frac(i,j)=0.0
          var_flow_rate(i,j)=0.0
          var_fan_power(i,j)=0.0
          sys_zone_cap_frac_initial(i,j)=0.0
          sys_zone_cap_frac_ret_air(i,j)=0.0
          zone_call_heat(i,j)=.false.
          zone_call_cool(i,j)=.false.
   20   continue
C Clear all common variable matrices for day/period information in hvac_common.h
        do 30 j=1,10
          num_outside_air_day_periods(i,j)=0
   30   continue
        do 40 j=1,100
          iend_hour_out_air_dayperiod(i,j)=0
          day_period_out_air_flow(i,j)=0.0
   40   continue
        do 50 j=1,8760
          iend_hour_out_air_daytype(i,j)=0
   50   continue
   
   10 continue
C Clear all common variable arrays of size (mcom) in hvac_common.h
      do 60 i=1,mcom
        outdoor_air_inf_cond(i)=0.0
        coil_moist_in(i)=0.0
        coil_moist_out1(i)=0.0
        coil_moist_out2(i)=0.0
        cont_fun_cool_cap(i)=0.0
        cont_fun_heat_cap(i)=0.0
   60 continue 
C Clear all common variable arrays of size (max_hvac_types) in hvac_common.h
      do 70 i=1,max_hvac_types
        ihvac_type(i)=0
   70 continue 
C Clear all common variable arrays of size (max_hvac_vars) in hvac_common.h
      do 80 i=1,max_hvac_vars
        isys_type(i)=0
   80 continue

      return
      end
      
C ------------------------------END OF---------------------------------
C ---------------------------HVAC ZEROING------------------------------
C ---------------------------------------------------------------------
