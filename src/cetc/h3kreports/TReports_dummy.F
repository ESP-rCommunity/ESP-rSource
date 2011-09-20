!       TReports_dummy.F
!
!       Created by Alex Ferguson
!       Created on March 19, 2004
!
!       This file contains stubs required compile ESP-r without 
!       support for XML output. 
!
! Logical function indicating that H3KREPORTS is not available
      LOGICAL FUNCTION REP_XMLSTATUS()
      REP_XMLSTATUS = .FALSE.
      RETURN
      END


      SUBROUTINE REP_UPDATE(DUMMY1, DUMMY2, DUMMY3, DUMMY4)
      REAL DUMMY2
      INTEGER DUMMY1, DUMMY3, DUMMY4

        ! Do nothing

      RETURN
      END


      SUBROUTINE REP_SUMMARY()

        ! Do nothing

      RETURN
      END

      SUBROUTINE REP_XML_SUMMARY()

        ! Do nothing

      RETURN
      END

      SUBROUTINE REP_CLEANUP_FILES()

        ! Do nothing

      RETURN
      END

      SUBROUTINE REP_SET_PARAMETER(DUMMY1, DUMMY2)
      CHARACTER*(*) DUMMY2, DUMMY1

      Return
      end


      SUBROUTINE config_h3k_reports_menu()

      return
      end

      logical function bH3K_rep_enabled()
      bH3K_rep_enabled = .false.
      return
      end




      Subroutine ReportNextTimeStep(iStep,rTime,iDay,bStartup)
      integer, intent(in) :: iStep
      real, intent(in) :: rTime
      integer, intent(in) :: iDay
      logical, intent(in) :: bStartup
      End Subroutine

      Subroutine GenerateOutput()
      End Subroutine

      Subroutine SetReportParameter(cName,cValue)
      character(len=*), intent(in) :: cName, cValue
      End Subroutine

      Subroutine SetReportEnable(bStatus)
      logical, intent(in) :: bStatus
      End Subroutine

      logical Function isH3KEnabled()
      isH3KEnabled = .false. 
      End Function isH3KEnabled

      Subroutine UpdateH3kReport()
      End Subroutine

      
!.....
      subroutine rep_toggle_config(cVariable)
      character(len=*), intent(in)::cVariable
      return 
      end subroutine
!.....
      subroutine rep_update_config_file()
      return 
      end subroutine
!.....
      logical function rep_report_list(cType,cValue)
      character(len=*), intent(in)::cType, cValue
      rep_report_list = .false.
      return 
      end function rep_report_list
!.....
      character(50) function rep_report_config(cDescription)
      character(len=*), intent(in)::cDescription
      rep_report_config = ' '
      return
      end function rep_report_config
!.....
      subroutine h3k_enable_reports(bStatus)
      logical bStatus 

      return 
      end subroutine
!.....
      subroutine generate_output()
      return
      end 
!.....
      subroutine report_next_time_step(iStep, iTime, iDay, bStartup)
      integer, intent(in) :: iStep
      real, intent(in) :: iTime 
      integer, intent(in) :: iDay
      logical, intent(in) :: bStartup
      return 
      end 
!..... 
      subroutine rep_Update_Config(cVariable,cValue)
      character(len=*), intent(in)::cVariable,cValue
      return
      end
!.....      
      subroutine add_to_report_wild3(iIdentifier, rValue, cWild1, 
     & cWild2, cWild3)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1, cWild2, cWild3
      return 
      End 
      
      subroutine add_to_report_wild2(iIdentifier, rValue, cWild1, 
     & cWild2)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1, cWild2
      return 
      End       
       
      subroutine add_to_report_wild1(iIdentifier, rValue, cWild1)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      character(len=*), intent(in) :: cWild1
      return 
      End      
!.....
      subroutine add_to_report_details(iIdentifier, cUnit, cType, 
     &                                 cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription
      return 
      End Subroutine     
!.....      
      subroutine add_to_report_details_wild3(iIdentifier, cWild1, 
     &  cWild2, cWild3, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, 
     &      cWild1, cWild2, cWild3
      return 
      end 
      
      subroutine add_to_report_details_wild2(iIdentifier, cWild1, 
     &  cWild2, cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, 
     &      cWild1, cWild2
      return 
      end 
      
      subroutine add_to_report_details_wild1(iIdentifier, cWild1, 
     &  cUnit, cType, cDescription)
      integer,intent(in) :: iIdentifier
      character(len=*), intent(in) :: cUnit, cType, cDescription, 
     &      cWild1
      return 
      end    

      subroutine add_to_report(iIdentifier, rValue)
      integer,intent(in) :: iIdentifier
      real, intent(in) :: rValue
      return 
      end 
   
      subroutine set_report_variable(Ident, 
     &                               Enabled, 
     &                               int_1, 
     &                               int_2, 
     &                               int_3, 
     &                               int_4, 
     &                               VariableName, 
     &                               MetaType, 
     &                               VariableType, 
     &                               Description)
     
      integer Ident 
      character(256)	:: VariableName 
      character(16)  :: MetaType 
      character(16)  :: VariableType 
      character(512) :: Description 
      logical*1      :: Enabled 
      integer int_1, int_2, int_3, int_4

      return 
      end 
!.....      
      subroutine  set_report_simulation_info(isd1,ism1, isd2, ism2,
     &                                       isds,isdf,ntstep)
      integer,intent(in)::isd1,ism1,isd2,ism2,isds,isdf,ntstep
      return 
      end 
      