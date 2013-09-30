C Jon Hand, May 2007
C Console application to take a text file of the
C source of an esp-r module (usually created by a
C cat *.F >all_source command.
C It opens the all_source file and if the source line
C is of type #include "building.h" or #include "acoustic.h"
C or #include "esprdbfile.h" etc. it replaces these lines
C with an indented   INCLUDE 'building.h'
C To compile (use either f90 or f77 or g77):
C f90 -c reposition_include.F -g
C f90 -o reposition_include reposition_include.o

      program reposition_include

      character fileinput*72
      character fileoutput*72
      character outs*124
      character sq*1,dq*1

      dq=char(34)   ! assign double quote
      sq=char(39)   ! assign single quote
      write(6,'(a)') 'Assumes input file is all_source and that the'
      write(6,'(a)') 'output file is all_source.F'

      write(fileinput,'(a)') 'all_source'    ! assumed name of catted file.
      write(fileoutput,'(a)') 'all_source.F' ! assumed patched file.
      open(1,file=fileinput,status='old')
      open(2,file=fileoutput,status='unknown')

  20  continue
      read(1,'(a)',IOSTAT=IOS,ERR=1) outs
      if(ios.eq.-1) goto 1
      if(outs(1:8).eq.'#include')then
        if(outs(11:20).eq.'building.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'building.h',sq
        elseif(outs(11:19).eq.'bc_data.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'bc_data.h',sq
        elseif(outs(11:33).eq.'MultiYear_simulations.h')then
         write(2,'(4a)')  '      INCLUDE ',sq,
     &     'MultiYear_simulations.h',sq
        elseif(outs(11:15).eq.'cfd.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'cfd.h',sq
        elseif(outs(11:20).eq.'net_flow.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'net_flow.h',sq
        elseif(outs(11:19).eq.'espriou.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'espriou.h',sq
        elseif(outs(11:17).eq.'power.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'power.h',sq
        elseif(outs(11:22).eq.'esprdbfile.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'esprdbfile.h',sq
        elseif(outs(11:17).eq.'plant.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'plant.h',sq
        elseif(outs(11:17).eq.'power.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'power.h',sq
        elseif(outs(11:19).eq.'control.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'control.h',sq
        elseif(outs(11:16).eq.'tdf2.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'tdf2.h',sq
        elseif(outs(11:23).eq.'uncertainty.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'uncertainty.h',sq
        elseif(outs(11:17).eq.'epara.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'epara.h',sq
        elseif(outs(11:18).eq.'gnetwk.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gnetwk.h',sq
        elseif(outs(11:22).eq.'dhw_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'dhw_common.h',sq
        elseif(outs(11:25).eq.'espinstalldir.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'espinstalldir.h',sq
        elseif(outs(11:19).eq.'plantdb.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'plantdb.h',sq
        elseif(outs(11:16).eq.'SOFC.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'SOFC.h',sq
        elseif(outs(11:23).eq.'cogen_tanks.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'cogen_tanks.h',sq
        elseif(outs(11:23).eq.'ADS_storage.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'ADS_storage.h',sq
        elseif(outs(11:23).eq.'aims_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'aims_common.h',sq
        elseif(outs(11:22).eq.'dhw_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'dhw_common.h',sq
        elseif(outs(11:29).eq.'Annex42_fuel_cell.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'Annex42_fuel_cell.h',sq
        elseif(outs(11:34).eq.'Annex42_combustion_CHP.h')then
          write(2,'(4a)') '      INCLUDE ',sq,
     &      'Annex42_combustion_CHP.h',sq
        elseif(outs(11:36).eq.'ground_temp_mains_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,
     &     'ground_temp_mains_common.h',sq
        elseif(outs(11:22).eq.'cetc_cogen.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'cetc_cogen.h',sq
        elseif(outs(11:31).eq.'chemical_properties.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'chemical_properties.h',sq
        elseif(outs(11:38).eq.'Hydrogen_demand_controller.h')then
          write(2,'(4a)') '      INCLUDE ',sq,
     &      'Hydrogen_demand_controller.h',sq
        elseif(outs(11:29).eq.'Hydrogen_MH_store.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'Hydrogen_MH_store.h',sq
        elseif(outs(11:28).eq.'CETC_definitions.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'CETC_definitions.h',sq
        elseif(outs(11:24).eq.'Stirling_CHP.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'Stirling_CHP.h',sq
        elseif(outs(11:27).eq.'hvac_parameters.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'hvac_parameters.h',sq
        elseif(outs(11:26).eq.'furnace_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'furnace_common.h',sq
        elseif(outs(11:23).eq.'hvac_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'hvac_common.h',sq
        elseif(outs(11:22).eq.'validation.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'validation.h',sq
        elseif(outs(11:26).eq.'dhw_parameters.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'dhw_parameters.h',sq
        elseif(outs(11:31).eq.'h3kstore_parameters.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'h3kstore_parameters.h',sq
        elseif(outs(11:27).eq.'h3kstore_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'h3kstore_common.h',sq
        elseif(outs(11:34).eq.'Annex42_combustion_CHP.h')then
          write(2,'(4a)') '      INCLUDE ',sq,
     &      'Annex42_combustion_CHP.h',sq
        elseif(outs(11:23).eq.'lookup_data.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'lookup_data.h',sq
        elseif(outs(11:22).eq.'build_info.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'build_info.h',sq
        elseif(outs(11:20).eq.'acoustic.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'acoustic.h',sq
        elseif(outs(11:15).eq.'b2e.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'b2e.h',sq
        elseif(outs(11:15).eq.'TAC.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'TAC.h',sq
        elseif(outs(11:17).eq.'gagge.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gagge.h',sq
        elseif(outs(11:15).eq.'LCA.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'LCA.h',sq
        elseif(outs(11:19).eq.'BATTERY.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'BATTERY.h',sq
        elseif(outs(11:28).eq.'Ventilation_Data.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'Ventilation_Data.h',sq
        elseif(outs(11:27).eq.'gcep_parameters.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gcep_parameters.h',sq
        elseif(outs(11:23).eq.'gcep_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gcep_common.h',sq
        elseif(outs(11:23).eq.'ashp_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'ashp_common.h',sq
        elseif(outs(11:26).eq.'gshp_v1_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gshp_v1_common.h',sq
        elseif(outs(11:23).eq.'gshp_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gshp_common.h',sq
        elseif(outs(11:26).eq.'gshp_hs_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gshp_hs_common.h',sq
        elseif(outs(11:26).eq.'gshp_sl_common.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'gshp_sl_common.h',sq
        elseif(outs(11:19).eq.'monitor.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'monitor.h',sq
        elseif(outs(11:19).eq.'H2_wind.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'H2_wind.h',sq
        elseif(outs(11:26).eq.'Hydrogen_PEMFC.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'Hydrogen_PEMFC.h',sq
        elseif(outs(11:27).eq.'WaterProperties.h')then
          write(2,'(4a)') '      INCLUDE ',sq,'WaterProperties.h',sq
        elseif(outs(11:33).eq.'DG_controller_include.h')then
          write(2,'(4a)') '      INCLUDE ',sq,
     &      'DG_controller_include.h',sq
        else
          write(2,'(a)') outs(1:lnblnk(outs))
        endif
        goto 20    ! loop back for more
      else
        write(2,'(a)') outs(1:lnblnk(outs))
        goto 20    ! loop back for more
      endif

  1   close(1)
      close(2)
      write(6,'(a)') 'Reached the end of the file'
      stop
      end

