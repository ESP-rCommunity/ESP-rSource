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
C
C
C
C ConfigureH3KReports.F
C
C Author: Alex Ferguson
C Date: April 1, 2005 (no joke!)
C Copyright: Natural Resouces Canada
C
C This file contains procedures allowing a user
C to configure H3Kreports through bps's gui.
C
C
C-------------------------Config_H3k_reports_menu:---------------------
C This procedure displays  menu allowing a user to configure h3k reports
C
C
C----------------------------------------------------------------------   
      subroutine Config_H3k_reports_menu()
      implicit none

C External functions.
      integer lnblnk

C----------------------------------------------------------------------
C     Global variables
C----------------------------------------------------------------------
      common/pophelp/h(60)
      character*72 h              ! Character buffer for help text
C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      integer iParam_MAX          ! Maximum number of parameters
      parameter (iParam_MAX = 50 )
      integer iChar_len
      parameter (iChar_len = 29)
      integer iSerial_len
      parameter (iSerial_len = iParam_MAX * iChar_len )
      integer iParam_Count        ! number of paramerters returned
                                  !    from h3kreports

      character*20 cSerializer


      
      logical bValues(iParam_MAX) ! values of parameters
                                  !    returned by h3kreports


      character*50 cValue         ! Option value                            
      character*29 cMenu(30)      ! Menu to be displayed
      integer iMenu_Count         ! Number of choices on menu

      integer iChoice             ! User's choice

      logical bDone               ! Flag indicating configuration is
                                  !    complete

      character*128 cPaths(4)     ! Paths to be used when searching
                                  !    for xslt files
      integer iPaths              ! # of paths to search
      character*128 cFiles(12)    ! xslt files available
      character*128 cFileNames(12)    ! prompts for xslt files available

      character*72 cAltPath       ! custom xslt path
      data cAltPath /' '/
      save cAltPath

      character*72 cDestFile      ! Destination for xslt transform
      
      character*128 cFilePaths(12)! full paths to xslt files.
      logical bFileFound(12)      ! flags indicating that files
                                  !   are found
      integer iNumberSupported    ! # of xslt files supported                                  
      integer iNumberAvailable    ! # of xslt files available

      integer iPath, iFile        ! Counters
      integer iSS_list_start      ! start of stylesheet list
      integer iSS_list_end        ! end of stylesheet list 

      logical bStyleSheet_Selected ! flag indicating SS has been selected
      logical bTransform_available ! flag indicating "transform" option
                                   !     is available

      integer iErr                ! error flag
      integer iUnit               ! unit number
C----------------------------------------------------------------------
C     ESP-r variables
C----------------------------------------------------------------------
      common/uhome/upath
      character upath*72             ! path to ~/
C----------------------------------------------------------------------
C     References 
C----------------------------------------------------------------------                                  
      logical bH3K_Rep_Enabled       ! Function indicating the status of
                                     !    h3kreports output
      character*50 Rep_Report_Config ! Function returing the current value
                                     !    of configuration parameters
      logical rep_Report_List        ! Function returing the current status
                                     !    of a stylesheet
      logical rep_toggle_config      ! Funcion toggling an on-off configuration
                                     !    parameter.


C-----------------------------------------------------------------------
C     Set xslt paths. Note: cAltPath is defaulted to ' '
C-----------------------------------------------------------------------
      cPaths(1) = cAltPath
      cPaths(2) = ' '
      cPaths(3) = upath(1:lnblnk(upath)) // '/esp-r/xsl/'
      cPaths(4) = '/usr/esru/esp-r/xsl/'

      iPaths = 4
C-----------------------------------------------------------------------
C     Set available xslt supported files 
C-----------------------------------------------------------------------
      cFiles(1) = 'generic_summary.xsl'
      cFiles(2) = 'generic_summary_html.xsl'

      cFileNames(1) ='1 summary (CSV)  '
      cFileNames(2) ='2 summary (HTML) '

      iNumberSupported = 2
                         
      bDone = .false.


      
         
      
C-----------------------------------------------------------------------
C     Loop until user leaves menu
C-----------------------------------------------------------------------      
      do while ( .not. bDone )

C........Search for xsl files
         iNumberAvailable = 0
         do iFile = 1, iNumberSupported
   
            bFileFound (iFile) = .false.
            
            do iPath  = 1, iPaths
   
               if ( .not. bFileFound ( iFile ) ) then
                  
                  cFilePaths(iFile) =
     &             cPaths(iPath)(1:lnblnk(cPaths(iPath)))
     &              // cFiles(iFile)(1:lnblnk(cFiles(iFile)))

                  INQUIRE (
     &                   FILE  = cFilePaths(iFile)
     &                        (1:lnblnk(cFilePaths(iFile))),
     &                   EXIST = bFileFound(iFile) )
     
               endif
   
            enddo

            if (bFileFound(iFile)) then
               iNumberAvailable = iNumberAvailable + 1
            endif

         enddo
C----------------------------------------------------------------------
C        Create help prompt. This must be done each time the
C        help menu is displayed, because the `h` array will be
C        overwritten below when the user is prompted for files.
C----------------------------------------------------------------------

        h(1) = 'H3K Reports provides an alternate facility for '
        h(2) = 'recovering results from bps. Results can be saved'
        h(3) = 'in ascii, CSV or XML formats, and can be translated'
        h(4) = 'into HTML or text files using an XSL stylesheet.'
        h(5) = 'H3K Reports is compatable with ESP-r`s existing '
        h(6) = 'results recovery facilities, and can be used in '
        h(7) = 'conjunction with res.'
        h(8) = ' '
        h(9) = 'Files:'
        h(10)= '  input.xml: Configuration file stored in the cfg'
        h(11)= '    directory.                                   '
        h(11)= '  out.xml: XML formatted file containing summary '
        h(12)= '    results from the simulation. '
        h(13)= '  out.csv: CSV formatted file containing time step'
        h(14)= '    data from the simulation.'
        h(15)= '  '
        h(16)= '  It is also possible to translate the out.xml file'
        h(17)= '  into any ASCII format using an XSL stylesheet.'
        h(18)= ' '
        h(19)= 'Options:'
        h(20)= '  a Enable/disable H3k Reports '
        h(21)= '  b Dictionary output: Optionally output a listing '
        h(22)= '    of all data that can be accessed with H3K Reports.'
        h(23)= '    Data will be output to an ascii file called '
        h(24)= '    out.dictinoary.'
        h(25)= '  c XML Structure: Use flat or nested `tree` format'
        h(26)= '    for XML output files.'
        h(27)= '  d TS averaging: Optionally overide bps`s time-step'
        h(28)= '    averaging setting. '
        h(29)= '  e Report startup period: Optionally include data '
        h(30)= '    from the start-up period data in results '
        h(31)= '  f Output all data: Optionally report all data '
        h(32)= '    available to H3K Reports. If this option is not '
        h(33)= '    selected, only the variables specified in the '
        h(34)= '    input.xml configuration file will be reported. '
        h(35)= '    If no parameters are included in input.xml, H3K '
        h(36)= '    Reports will not produce any output unless `Output'
        h(37)= '    all data` is selected.'
        h(38)= ' '
        h(39)= 'XSL formatted output:'
        h(40)= '  1 summary (CSV): XSL file producing brief, summary'
        h(41)= '    statistics in an ascii file.'
        h(42)= '  2 summary (HTML): XSL file producing summary '
        h(43)= '    statistics in an HTML file.'
        h(44)= '  '
        h(45)= 'XSL options:'
        h(46)= '  g Path to XSL files...: Path in which bps should '
        h(47)= '    look for .xsl files.'
        h(48)= '  h Link stylesheet: Optionally include a link within '
        h(49)= '    out.xml to the selected stylesheet, permitting    '
        h(50)= '    out.xml to be viewed in a browser.'
        h(51)= '  i Apply stylesheet: Optionally apply the stylesheet '
        h(52)= '    to produce formatted output directly.'
        h(53)= '  j Transform target file: File formatted output should'
        h(54)= '    be saved in.'
        h(55)= ' '


C----------------------------------------------------------------------
C        Build menu
C----------------------------------------------------------------------

         if ( bH3K_Rep_Enabled() ) then
               cMenu (1)= 'a H3K Reports >> enabled      '
         else
               cMenu (1)= 'a H3K Reports >> disabled     '
         endif

         cValue = rep_Report_Config("output_dictionary")


         if ( bH3K_Rep_Enabled() ) then
            cMenu(2) = '  ____________________________'
            cMenu(3) = '  Select options:             '

            cValue = rep_Report_Config ("output_dictionary")
            if ( cValue(1:4) == 'true' ) then
               cMenu(4) = 'b Dictionary output: enabled '
            else
               cMenu(4) = 'b Dictionary output: disabled'
            endif

            cValue = rep_Report_Config ("hierarchy")
            if ( cValue(1:4) == 'flat' ) then
               cMenu(5) = 'c XML structure: flat'
            else
               cMenu(5) = 'c XML structure: tree'
            endif

            cValue = rep_Report_Config("time_step_averaging")
            if ( cValue(1:4) == 'true' ) then
               cMenu (6) = 'd TS averaging: on'
            elseif ( cValue(1:5) == 'false' ) then
               cMenu (6) = 'd TS averaging: off'
            else
               cMenu (6) = 'd TS averaging: default'
            endif

            cValue = rep_Report_config("report_startup_period_data")
            if ( cValue (1:4) == 'true' ) then
               cMenu(7) = 'e Report startup period     *'
            else
               cmenu(7) = 'e Report startup period '
            endif

            cMenu(8) = '  ____________________________'
            cMenu(9) = '  Select data:                '


            cValue = rep_Report_Config ("dump_all_data")
            if ( cValue(1:4) == "true" ) then
               cMenu(10) = 'f All data                  *'
            else
               cMenu(10) = 'f All data '
            endif           
            
            
            
            cMenu(11) = '  ____________________________'
            cMenu(12) = '  XSLT formatted output:      '

            
            iMenu_Count = 12

            
            bStyleSheet_Selected = .false.
            iSS_list_start = iMenu_Count
            
            if ( iNumberAvailable .gt. 0 ) then
               do iFile = 1, iNumberSupported
                  if ( bFileFound (iFile) ) then

                     iMenu_Count = iMenu_Count + 1 

                     if ( rep_Report_List("style_sheet",
     &                            cFilePaths(iFile)
     &                            (1:lnblnk(cFilePaths(iFile))))) then
                        cMenu ( iMenu_Count ) =
     &                     cFileNames(iFile)
     &                        (1:25) // '   *'
                        bStyleSheet_Selected = .true.
                     else

                        cMenu ( iMenu_Count ) =
     &                     cFileNames(iFile)
     &                        (1:lnblnk(cFileNames(iFile)))                     

                     endif 

                  endif
               enddo
            endif

            iSS_list_end = iMenu_Count

            cMenu(iMenu_Count+1) = ' '
            cMenu(iMenu_Count+2) = 'e Path to xsl files... '
            iMenu_Count = iMenu_Count + 2
            
            if ( bStyleSheet_Selected ) then
C..............xslt transform options 
               cValue = rep_Report_Config ( "link_style_sheet" )
               if ( cValue(1:4) == "true" ) then
                  cMenu(iMenu_Count+1) = 'f Link style sheet          *'
               else
                  cMenu(iMenu_Count+1) = 'f Link style sheet          '
               endif
               
               cValue = rep_Report_Config ( "apply_style_sheet" )
               if ( cValue(1:4) == "true" ) then
                  cMenu(iMenu_Count+2) = 'g Apply style sheet         *'
                  cMenu(iMenu_Count+3) = 'h Transform target file...   '
C.................Set flag indicating that transform is available                  
                  bTransform_available = .true.
               else
                  cMenu(iMenu_Count+2) = 'g Apply style sheet          '
                  cMenu(iMenu_count+3) = ' '
C.................Set flag indicating transform is unavailable                  
                  bTransform_available = .false.
               endif


            else

C..............Set flags indicating that transform option is unavailable
               bTransform_available = .false.
                                                
C..............Empty menu.            
               cMenu(iMenu_Count+1) = ' '
               cMenu(iMenu_Count+2) = ' '
               cMenu(iMenu_Count+3) = ' '

            endif

            iMenu_Count = iMenu_Count + 3
            
         else
         
            iMenu_Count = 1            
            
         endif

C----------------------------------------------------------------------
C        Footer
C----------------------------------------------------------------------
         iMenu_Count = iMenu_Count + 3
         cMenu(iMenu_count-2) = '  ____________________________'
         cMenu(iMenu_count-1) = '? Help                        '
         cMenu(iMenu_count-0) = '- Exit H3K Reports menu       '
      
C----------------------------------------------------------------------
C        Display menu
C----------------------------------------------------------------------

         iChoice=-3
C........Empty message bar?
         call usrmsg(' ',' ','-')

         call emenu('H3K Reports configuration',
     &         cMenu,iMenu_Count,iChoice)

C----------------------------------------------------------------------
C        Set parameters, as necessary
C----------------------------------------------------------------------
         if ( iChoice == iMenu_Count ) then
C...........Configuration is complete. Quit loop.
            bDone = .true.
         
         elseif ( iChoice == iMenu_Count - 1 ) then
C...........Display help menu
            call phelpd('H3K Reports',55,'H3K Reports ',0,0,ierr)
                    
         elseif ( iChoice == 1 ) then
C...........Toggle h3kreports on/off
            if ( bH3K_Rep_Enabled() ) then
              
               call h3k_enable_reports ( .false. )
            
            else
               
               call h3k_enable_reports ( .true. )
            
            endif 

         elseif ( bH3K_Rep_Enabled() ) then

C...........Toggle dictionary
            if ( iChoice == 4 ) then
            
                if( .not. rep_toggle_config("output_dictionary") ) then
                   ! Possible error trapping here
                endif 

C..........Toggle flat/tree hierarchy                
            elseif ( iChoice == 5 ) then
               cValue = rep_Report_Config ("hierarchy")
               if ( cValue(1:4)== "tree" ) then
                  call rep_Update_Config ("hierarchy", "flat")
               else 
                  call rep_Update_Config ("hierarchy", "tree")
               endif
               
C...........Toggle timestep averaging
            elseif ( iChoice == 6 ) then
               
               if ( .not. rep_toggle_config (
     &                      "time_step_averaging") ) then
                  ! Possibly error trap
               endif 

C...........Toggle reporting start-up period
            elseif ( iChoice == 7 ) then
               if ( .not. rep_toggle_config(
     &                       "report_startup_period_data") ) then
                  ! possibly error trap
               endif
            
C...........Toggle dump all data               
            elseif ( iChoice == 10 ) then
 
               if( .not. rep_toggle_config("dump_all_data") ) then
                   ! Possible error trapping here
                endif

C...........Enable/disable stylesheet                
            elseif ( iChoice .gt. iSS_list_start 
     &               .and. iChoice .le. iSS_list_end ) then
               
               iFile = iChoice - iSS_list_start

C..............If stylesheet already exists, drop it               
               if ( rep_Report_List("style_sheet",
     &              cFilePaths(iFile)(1:lnblnk(cFilePaths(iFile))))
     &             )then
     
                  call rep_Update_Config ("-style_sheet",
     &                                    cFilePaths(iFile)
     &                                    (1:lnblnk(cFilePaths(iFile))))

C..............If stylesheet does not exist, add it     
               else
                  call rep_Update_Config ("+style_sheet",
     &                                    cFilePaths(iFile)
     &                                    (1:lnblnk(cFilePaths(iFile))))
               endif 
               
C...........Update customizable xslt path               
            elseif ( iChoice ==  iSS_list_end + 2 ) then
               h(1) = 'H3K Reports will search for xsl files on your'
               h(2) = 'system. The default paths are: '
               h(3) = '  ~/esp-r/xsl/                 '
               h(4) = '  /usr/esru/esp-r/xsl/         '
               h(5) = '  ./ (the current directory)   '
               h(6) = ' '
               h(7) = 'You can also specify a custom directory '
               h(8) = 'containing xsl files. If xsl files are found'
               h(9) = 'in this path, they will be listed in the '
               h(10)= 'menu. '
               h(11)= ' '
               h(12)= 'Remember to include a trailing slash (`/`) '
               h(13)= 'when specifying directories .'
               
               
               CALL EASKS(cAltPath,' Path to xsl files?',
     &                   ' ',72,'./','Path to xslt files',ierr,13)

               cPaths(1) = cAltPath(1:lnblnk(cAltPath))

C...........Link stylesheet
            elseif ( iChoice ==  iSS_list_end + 3 .and.
     &               bStyleSheet_Selected ) then

                if( .not. rep_toggle_config("link_style_sheet") ) then
                   ! Possible error trapping here
                endif

C...........Apply stylesheet using integrated xslt processor
            elseif ( iChoice ==  iSS_list_end + 4 .and.
     &               bStyleSheet_Selected ) then
                
                if( .not. rep_toggle_config("apply_style_sheet") ) then
                   ! Possible error trapping here
                endif

C...............Empty transform destination file, if defined
                cValue = rep_Report_Config("transform_destination_file")
                if ( cValue (1:3) /= '   ' ) then
                   call rep_Update_Config
     &                   ("transform_destination_file", ' ')
                endif 
                
C...........Specify transform destination file
            elseif ( iChoice == iSS_list_end + 5 .and.
     &                bTransform_available ) then

               h(1) = 'If `apply style sheet` is selected, H3K Reports'
               h(2) = 'will automatically transform the xml output'
               h(3) = 'using the selected style sheet and write the'
               h(4) = 'results to disk. You can specify the name of the'
               h(5) = 'output file here.'
               
               call easks(cDestFile,' Transform destination file?',
     &                             ' ',72,
     &                             'Results.out',
     &                             'Transform destination file',ierr,5)
     &

               call rep_Update_Config ("transform_destination_file",
     &                                 cDestFile(1:lnblnk(cDestFile)))
     

            endif
                                          
         endif 
     
      enddo

C.....Update configuration file
      call rep_update_config_file ()
      
      return
      end 

      
      
