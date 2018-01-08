C ******* ESPrVersion
C Spit out ESP-r versioning information.
C
C  Arguement cVerbosity allows some control on the amount of detail 
C  provided: 
C    - 'summary' attempts to print release information, 
C    - 'details' prints versioning/build information.
C
C  But versioning/build information will be reported in all cases 
C  if the Install script has deemed this to be a 'development build'.

      subroutine ESPrVersion(cPassedType,cPassedAppName,iuout)
      implicit none
      
C PASSED VARIABLES 
      character cPassedAppName*(*)
      character cPassedType*(*)
      character cAppName*8           ! Name of current application
      integer iuout                  ! Output channel.

C LOCAL VARIABLES
C declare variables describing version and build information
      character cType*16             ! Flag indicating if version/build
                                     !  info should be dumped
      logical bBuild_Info            ! Flag indicating if build info should 
                                     !   be dumped. 
C      character cURL*256             ! Repository URL commented out in build_info.h
      
      character cTemp_a*124, cTemp_b*124 ! Temporary strings
      integer iLen   ! for actual string lengths
      
      logical bH3KExtentionsActive  ! Function returning status of h3k extentions. 
      
      
C Arrays mapping binary names to longer descriptions
      
      integer iBinCount
      parameter (iBinCount=22)
      
      character cBinNames*8
      character cBinDesc*72 
      dimension cBinNames(iBinCount)
      dimension cBinDesc(iBinCount)
      
      integer iBinIndex, iCurIndex
      character*3 cXLibs

      data cBinNames / 'aco', 'b2e', 'bps', 'dbm', 'dfs', 'dfv', 'c2e', 
     &                 'cdb', 'clm', 'e2r', 'ecnv', 'eco', 'grd', 'ish',
     &                 'mfs', 'mld', 'mrt', 'net', 'pdb', 'prj', 'res', 
     &                 'vew' /
      
      
      data cBinDesc / 'Acoustics Analysis Module',
     &                'BEMS link',
     &                'Integrated Building Performance Simulator',
     &                'Database Manager',
     &                'Domain Flow Solver',
     &                'Domain Flow Viewer',
     &                ' ','Components DB Manager',
     &                'Climate Analysist Tool',
     &                'Radiance link',
     &                'Data Conversion Utility',
     &                ' ',
     &                'Construction Gridding Tool',
     &                'Shading Analysis Tool',
     &                'Mass Flow Solver',
     &                'Mould analysis tool',
     &                'Mean Radiant Temperature Module',
     &                'Network tool',
     &                'Plant Database Tool',
     &                'Project Manager',
     &                'Results Analysis Tool',
     &                'Hidden Line Viewer' /
     
      save cBinNames, cBinDesc
      
C References:
      integer lnblnk
      logical rep_xmlstatus
      
C Include dynamically-created installation information. 
#include "build_info.h"

C Store passed application name and type in arrays of known
C length.
      iLen = lnblnk ( cPassedAppName )
      write (cAppName, '(A)' ) cPassedAppName( 1:MIN(iLen,8) )

      iLen = lnblnk ( cPassedType )
      write (cType, '(A)') cPassedType( 1:MIN(iLen,16) )

C Deterimine of summary or details 
      if ( cType(1:7) == "details" ) then
        bBuild_Info = .true.
      elseif ( cType(1:7) == "summary" ) then
        bBuild_Info = .false. 
      elseif ( cType(1:7) == "silent" ) then
        continue
      else
C cType is set programmically,
C and needs only rudementry error trapping.
        STOP " Argument to ESPrVersion must be 'summary' or 'details'"
      endif 

C Determine index of current application in cBinNames/cBinDesc arrays      
      do iCurIndex = 1, iBinCount
      
        iLen = lnblnk ( cBinNames(iCurIndex) )
        if ( cAppName(1:lnblnk(cAppName)) .eq. 
     &       cBinNames(iCurIndex)(1:iLen)) then 
          
          iBinIndex = iCurIndex
          
        endif 
      enddo

     
C Write out header ('ESP-r XYZ tool (xyx)'). 
C  e.g.: 'ESP-r integrated builing performance simulator (bps)'
      
      
C.....ESP-r banner 
      if ( bRelease ) then 
C.......Release version        
        write(cTemp_a,'(5A)') "ESP-r ", 
     &       cBinDesc(iBinIndex)(1:lnblnk(cBinDesc(iBinIndex))), 
     &       " Release ", 
     &       cRelease_Num(1:lnblnk(cRelease_Num)),
     &       "."
        call edisp(iuout,cTemp_a)      
      
      else 
        
        write(cTemp_a,'(2A)') "ESP-r ", 
     &       cBinDesc(iBinIndex)(1:lnblnk(cBinDesc(iBinIndex)))
        call edisp(iuout,cTemp_a)      
        
        write(cTemp_a,'(6A)') "This is a development copy of ", 
     &       cAppName(1:lnblnk(cAppName)),
     &       ", based on ESP-r release ",
     &       cRelease_Num(1:lnblnk(cRelease_Num)),"."
        call edisp(iuout,cTemp_a)    
        
      endif 
      
C If cType is silent return at this point.
      if ( cType(1:6) == "silent" ) return

C Copyright information:
      call edisp(iuout," ")      
      call edisp(iuout,      
     &  "Copyright resides with the member organisations of the ESP-r")
      call edisp(iuout,"development consortium as listed at:")
      call edisp(iuout,
     &  "http://www.esru.strath.ac.uk/Programs/ESP-r_central.htm.")      
      call edisp(iuout,"and https://github.com/ESP-rCommunity.")      

C Check if source was versioned (that is, was svn available, and 
C is it a working copy)
      if ( .not. bVersioned ) then     
C.......Write out non-versioned disclaimer.        
        
        call edisp(iuout," ")

        call edisp(iuout,
     &  "This copy of ESP-r is not under versioning control. Using")
        
        call edisp(iuout,
     &  "un-versioned copies of ESP-r may prevent you from")
        
        call edisp(iuout,
     &  "reconstituting models and reproducing simulation results in")
        
        call edisp(iuout,
     &  "the future. Development using unversioned copies of ESP-r")
        
        call edisp(iuout,
     &  "is also prone to error, and strongly discouraged.")
        
        call edisp(iuout," ")
        
        call edisp(iuout,
     &  "You can obtain a versioned copy of ESP-r here:")
        
        call edisp(iuout," ")
        
        write (cTemp_a,'(A)') 
     &  "   http://www.esru.strath.ac.uk/Programs/ESP-r_central.htm"
        call edisp(iuout,cTemp_a)
              
      endif 
      
      if ( bBuild_Info .or. .not. bRelease ) then 
C. .....Information about build:
        call edisp(iuout," ")
                 
        call edisp(iuout,"Build information:")
        
C.........Source code version:
C............' - Source:    BRANCH @ rXYZ (modified state)'
          
        if ( bVersioned ) then 
            
          write (cTemp_a,'(5A)') 
     &      "  SVN source -       ",
     &      cBranch(1:lnblnk(cBranch)), 
     &      "@r",
     &      cRevision(1:lnblnk(cRevision))
  
          if ( bModified ) then
            write (cTemp_a,'(2A)') cTemp_a(1:lnblnk(cTemp_a)),
     &        " (locally modified)"
          else 
            write (cTemp_a,'(2A)') cTemp_a(1:lnblnk(cTemp_a)),
     &        " (unmodified)"
          endif
        
        else
           write (cTemp_a,'(A)') 
     &       "  SVN source -       UNKNOWN"
        endif 
        call edisp(iuout,cTemp_a)      
          
C.........Build Platform (e.g.: i686:LINUX:2.6.9-gentoo-r1)
        write (cTemp_a,'(6A)')   "  Platform -         ",
     &         cArchitecture(1:lnblnk(cArchitecture)),":",
     &         cOperSystem(1:lnblnk(cOperSystem)),":",
     &         cOSversion(1:lnblnk(cOSversion))
        call edisp(iuout,cTemp_a)      
         
C.........Compiliers ( eg: gcc/g++/g77)
        write (cTemp_a,'(6A)')   "  Compilers -        ",
     &         cCC(1:lnblnk(cCC)),"/",
     &         cCPL(1:lnblnk(cCPL)),"/",
     &         cFC(1:lnblnk(cFC))
        call edisp(iuout,cTemp_a)

        call getXlibs(cXlibs)    ! find out the graphic library used
        
        write (cTemp_a,'(2A)')   "  Graphics library - ",
     &         cXLibs(1:3)
        call edisp(iuout,cTemp_a)

        if ( rep_xmlstatus() ) then  ! find whether XML is supported

          write (cXML_support, '(A)') "Supported"

        else

          write (cXML_support, '(A)') "Unsupported"

        endif 
        
        write (cTemp_a,'(2A)')   "  XML output -       ",
     &         cXML_support(1:lnblnk(cXML_support))
        call edisp(iuout,cTemp_a)
          
        if ( bH3KExtentionsActive() ) then  ! find whether H3k extentions are active

          write (cTemp_a,'(2A)')   "  H3K extensions -  ",
     &         "Active"
          call edisp(iuout,cTemp_a)

        endif 
            
      endif

      return
      end

C ******* ESPrVersionNum
C Return out ESP-r versioning information.

      subroutine ESPrVersionNum(cVnum)
      implicit none
      
C PASSED VARIABLES 
      character cVnum*16             ! Phrase with release number

C LOCAL VARIABLES
C declare variables describing version and build information
      integer iLen   ! for actual string lengths
            
C References:
      integer lnblnk
      
C Include dynamically-created installation information. 
#include "build_info.h"

C Write out version number. 
      write(cVnum,'(3A)') "release ", 
     &      cRelease_Num(1:lnblnk(cRelease_Num)),
     &      "."

      return
      end
