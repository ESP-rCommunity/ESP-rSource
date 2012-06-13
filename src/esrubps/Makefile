SRCdir = `pwd`/..
BLDdir = $(SRCdir)/esrubld
COMdir = $(SRCdir)/esrucom
DFSdir = $(SRCdir)/esrudfs
MFSdir = $(SRCdir)/esrumfs
PFSdir = $(SRCdir)/esrupfs
PLTdir = $(SRCdir)/esruplt
PRJdir = $(SRCdir)/esruprj
CETCdir = $(SRCdir)/cetc
SHCdir = $(SRCdir)/shocc
COSIMdir = $(SRCdir)/co-sim
LIBdir = $(SRCdir)/lib
PROG = bps$(EXE)
DLL = bps".dll"

.SUFFIXES: .o .F
.F.o:
	$(MFC) $(FFLAGS) -c -fcray-pointer $<

%.o : %.f90
	$(MCC) $(FFLAGS) -c $<

%.o : %.cpp
	$(MCPL) $(CFLAGS) -c $<

%.o : %.c
	$(MCC) $(CFLAGS) -c $<

MODULE_OBJECTS = h3kmodule.o startup.o aim2_module.o aim2_timestep.o TCC.o

BPS_OBJECTS = bps.o adaptconf1.o adaptconf2.o adjb.o adjp.o ascii_mat.o \
	  bcfunc.o bctlf.o blibsv.o bmatsu.o bmatsv.o basesimp.o basesimp_inputs.o bscoeff.o bscoeff_extended.o \
	  bndarybl.o bndaryl.o bndaryt.o bsim2cfd.o btherm.o c2fdum.o casual.o cfcalc.o \
	  cfconv.o cfdat.o cfdrw.o cfgrid.o cfmgerb.o cfutil.o chkconvg.o chtmst.o \
	  climi.o cntmnt.o conc.o convect1.o convect2.o ctread.o dayfac.o drylink.o ecasctl.o \
	  econstr.o econtrol.o edatabase.o egeometry.o egrid.o emfnetw.o emoist.o \
	  enetbound.o enetmath.o enetmisc.o enetpost.o enetrewr.o enetsetu.o \
	  enetsolv.o eroper.o esgrid.o esru_misc.o esystem.o filelist.o g3dfac.o \
	  g3dfnc.o g3dmtx.o gcfunc.o gsdmtx.o inital.o initalc.o initalp.o \
	  input.o item.o lisolv.o locmage.o matsv.o mfbndr.o mfcdat.o mfcntl.o \
	  mfmach.o mfmode.o mfsbld.o mfsbps.o mfsplt.o mfutil.o mitzero.o \
	  moistr.o monitor.o montrb.o mstmsc.o mzmrx5.o mzout6.o mzoutp.o nwkrewr.o \
	  pcloop.o pcomp1.o pcomp2.o pcomp3.o pcomp4.o pcomps.o pcomps1.o \
	  plibsv.o plot.o pltcfg.o pmatrx.o pplib.o ppsub.o precal.o psychro.o \
	  reslib.o rnor.o rwipv.o rwroam.o rwsbem.o senrwl.o sensit.o sentim.o simcon.o solar.o sort.o \
	  spmatl.o spmisc.o stofunc.o subsys.o tdfile.o tdrecbps.o trstyp.o \
	  tscon.o utilf.o MechVentIN.o CentralVentSys.o \
	  hvacinput.o hvacsim.o furnace.o init_hvac_var.o fan.o ashp.o \
	  ashp_cooling.o baseboard.o hvac_output.o h3koutput.o h3kstore.o \
	  FC_components.o FC_functions.o gshpinput.o gshpsim.o \
	  GCEPinput.o GCEP_load.o GCEP_SIM.o GCEP_MLAiterate.o GCEPtground.o GCEPInit.o\
	  water_tanks.o h3k_utils.o SetDllParms.o AIMS.o TAC.o \
	  WaterProperties.o DG_controller.o elec_net_utilities.o window_control.o \
	  PEM_FC_pt1.o PEM_FC_pt2.o powoc_calc.o h3k_report_data.o radiant_floor.o DHW_module.o \
	  h3k_plant_postprocess.o h2_matrix_library.o h2_components.o Annex42_combustion_CHP.o \
	  chemical_properties.o h2_comps.o compressed_cylinder.o Hydrogen_PEMFC.o \
	  Hydrogen_MH_store.o enet_state_variable_manipulate.o Hydrogen_demand_controller.o \
	  MultiYear_climate.o CETC_BATTERY.o wind_turbine_profile_model.o \
	  plt_zone_gain_coupling.o cetc_cogen.o BC_data.o plt_bc_components.o Hydrogen_appliances.o \
	  Annex42_fuel_cell.o shocc_dummy.o ADS_storage_tanks.o ADS_storage_unit.o ADS_controller.o \
	  solar_collectors.o ground_temp_mains.o Lookup_data.o readTrnsys.o trnsys_wrapper.o trnsys_lib.o \
	  tank_intank_hx.o mains_temp_draw_profiles.o stratified_tank.o SiteUtilities.o RE-H2-ctl.o \
	  ncm_overheating.o complex_fenestration.o editCFC.o NCHE.o stratified_tank_1HX.o stratified_tank_2HX.o \
	  AirFlowModelRoutines.o Res_elec_Ctl.o harmonizer_timing.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod aim2_timestep.mod start_up.mod parse_command_line.mod \
          h3kmodule.f90
PREBPS_OBJECTS = ESPrTrnsysData.o harmonizer_dummy.o

$(PROG): $(MODULE_OBJECTS) $(BPS_OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(MODULE_OBJECTS) $(BPS_OBJECTS) $(ULIBS) $(F_to_C_flags)
PREBPSDLL_OBJECTS = ESPrTrnsysData.o harmonizer.o

MODULES = aim2.mod aim2_calcdata.mod aim2_h2k_dummy.mod aim2_inputdata.mod aim2_timestep.mod \
	parse_command_line.mod start_up.mod win32interface.mod dll_functions.mod tcc.mod cosimdatatypes.mod

theEXE:  $(PREBPS_OBJECTS) $(MODULE_OBJECTS) $(BPS_OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(PREBPS_OBJECTS) $(MODULE_OBJECTS) $(BPS_OBJECTS) $(ULIBS) $(F_to_C_flags)

theDLL:  $(PREBPSDLL_OBJECTS) $(MODULE_OBJECTS) $(BPS_OBJECTS)
	$(MFC) $(LD_FLAGS) -o $(DLL) $(FFLAGS) $(PREBPSDLL_OBJECTS) $(MODULE_OBJECTS) $(BPS_OBJECTS) $(ULIBS) $(F_to_C_flags)


adaptconf1.F:
	rm -f adaptconf1.F
	ln -s $(DFSdir)/adaptconf1.F .
adaptconf2.F:
	rm -f adaptconf2.F
	ln -s $(DFSdir)/adaptconf2.F .
adjb.F:
	rm -f adjb.F
	ln -s $(BLDdir)/adjb.F .
adjp.F:
	rm -f adjp.F
	ln -s $(PLTdir)/adjp.F .
ascii_mat.F:
	rm -f ascii_mat.F
	ln -s $(COMdir)/ascii_mat.F .
basesimp.F:
	rm -f basesimp.F
	ln -s $(BLDdir)/basesimp.F .
basesimp_inputs.F:
	rm -f basesimp_inputs.F
	ln -s $(PRJdir)/basesimp_inputs.F .
bcfunc.F:
	rm -f bcfunc.F
	ln -s $(BLDdir)/bcfunc.F .
bctlf.F:
	rm -f bctlf.F
	ln -s $(BLDdir)/bctlf.F .
blibsv.F:
	rm -f blibsv.F
	ln -s $(BLDdir)/blibsv.F .
bmatsu.F:
	rm -f bmatsu.F
	ln -s $(BLDdir)/bmatsu.F .
bndaryl.F:
	rm -f bndaryl.F
	ln -s $(DFSdir)/bndaryl.F .
bndarybl.F:
	rm -f bndarybl.F
	ln -s $(DFSdir)/bndarybl.F .
bndaryt.F:
	rm -f bndaryt.F
	ln -s $(DFSdir)/bndaryt.F .
bscoeff.F:
	rm -f bscoeff.F
	ln -s $(BLDdir)/bscoeff.F .
bscoeff_extended.F:
	rm -f bscoeff_extended.F
	ln -s $(CETCdir)/bscoeff_extended.F .
bsim2cfd.F:
	rm -f bsim2cfd.F
	ln -s $(DFSdir)/bsim2cfd.F .
btherm.F:
	rm -f btherm.F
	ln -s $(BLDdir)/btherm.F .
c2fdum.F:
	rm -f c2fdum.F
	ln -s $(COMdir)/c2fdum.F .
casual.F:
	rm -f casual.F
	ln -s $(BLDdir)/casual.F .
cfcalc.F:
	rm -f cfcalc.F
	ln -s $(DFSdir)/cfcalc.F .
cfconv.F:
	rm -f cfconv.F
	ln -s $(DFSdir)/cfconv.F .
cfdat.F:
	rm -f cfdat.F
	ln -s $(DFSdir)/cfdat.F .
cfdrw.F:
	rm -f cfdrw.F
	ln -s $(COMdir)/cfdrw.F .
cfgrid.F:
	rm -f cfgrid.F
	ln -s $(COMdir)/cfgrid.F .
cfmgerb.F:
	rm -f cfmgerb.F
	ln -s $(DFSdir)/cfmgerb.F .
cfutil.F:
	rm -f cfutil.F
	ln -s $(DFSdir)/cfutil.F .
chkconvg.F:
	rm -f chkconvg.F
	ln -s $(DFSdir)/chkconvg.F .
chtmst.F:
	rm -f chtmst.F
	ln -s $(BLDdir)/chtmst.F .
climi.F:
	rm -f climi.F
	ln -s $(BLDdir)/climi.F .
cntmnt.F:
	rm -f cntmnt.F
	ln -s $(MFSdir)/cntmnt.F .
conc.F:
	rm -f conc.F
	ln -s $(DFSdir)/conc.F .
convect1.F:
	rm -f convect1.F
	ln -s $(BLDdir)/convect1.F .
convect2.F:
	rm -f convect2.F
	ln -s $(BLDdir)/convect2.F .
ctread.F:
	rm -f ctread.F
	ln -s $(COMdir)/ctread.F .
dayfac.F:
	rm -f dayfac.F
	ln -s $(BLDdir)/dayfac.F .
drylink.F:
	rm -f drylink.F
	ln -s $(BLDdir)/drylink.F .
ecasctl.F:
	rm -f ecasctl.F
	ln -s $(COMdir)/ecasctl.F .
econstr.F:
	rm -f econstr.F
	ln -s $(COMdir)/econstr.F .
econtrol.F:
	rm -f econtrol.F
	ln -s $(COMdir)/econtrol.F .
edatabase.F:
	rm -f edatabase.F
	ln -s $(COMdir)/edatabase.F .
egeometry.F:
	rm -f egeometry.F
	ln -s $(COMdir)/egeometry.F .
egrid.F:
	rm -f egrid.F
	ln -s $(COMdir)/egrid.F .
emfnetw.F:
	rm -f emfnetw.F
	ln -s $(COMdir)/emfnetw.F .
emoist.F:
	rm -f emoist.F
	ln -s $(COMdir)/emoist.F .
enetbound.F:
	rm -f enetbound.F
	ln -s $(PFSdir)/enetbound.F .
enetmath.F:
	rm -f enetmath.F
	ln -s $(PFSdir)/enetmath.F .
enetmisc.F:
	rm -f enetmisc.F
	ln -s $(COMdir)/enetmisc.F .
enetpost.F:
	rm -f enetpost.F
	ln -s $(PFSdir)/enetpost.F .
enetrewr.F:
	rm -f enetrewr.F
	ln -s $(COMdir)/enetrewr.F .
enetsetu.F:
	rm -f enetsetu.F
	ln -s $(PFSdir)/enetsetu.F .
enetsolv.F:
	rm -f enetsolv.F
	ln -s $(PFSdir)/enetsolv.F .
eroper.F:
	rm -f eroper.F
	ln -s $(COMdir)/eroper.F .
esgrid.F:
	rm -f esgrid.F
	ln -s $(COMdir)/esgrid.F .
esru_misc.F:
	rm -f esru_misc.F
	ln -s $(COMdir)/esru_misc.F .
esystem.F:
	rm -f esystem.F
	ln -s $(COMdir)/esystem.F .
filelist.F:
	rm -f filelist.F
	ln -s $(COMdir)/filelist.F .
g3dfac.F:
	rm -f g3dfac.F
	ln -s $(BLDdir)/g3dfac.F .
g3dfnc.F:
	rm -f g3dfnc.F
	ln -s $(BLDdir)/g3dfnc.F .
g3dmtx.F:
	rm -f g3dmtx.F
	ln -s $(BLDdir)/g3dmtx.F .
gcfunc.F:
	rm -f gcfunc.F
	ln -s $(COMdir)/gcfunc.F .
gsdmtx.F:
	rm -f gsdmtx.F
	ln -s $(BLDdir)/gsdmtx.F .
inital.F:
	rm -f inital.F
	ln -s $(BLDdir)/inital.F .
initalc.F:
	rm -f initalc.F
	ln -s $(COMdir)/initalc.F .
initalp.F:
	rm -f initalp.F
	ln -s $(PLTdir)/initalp.F .
item.F:
	rm -f item.F
	ln -s $(COMdir)/item.F .
lisolv.F:
	rm -f lisolv.F
	ln -s $(DFSdir)/lisolv.F .
locmage.F:
	rm -f locmage.F
	ln -s $(DFSdir)/locmage.F .
matsv.F:
	rm -f matsv.F
	ln -s $(BLDdir)/matsv.F .
mfbndr.F:
	rm -f mfbndr.F
	ln -s $(DFSdir)/mfbndr.F .
mfcdat.F:
	rm -f mfcdat.F
	ln -s $(MFSdir)/mfcdat.F .
mfcntl.F:
	rm -f mfcntl.F
	ln -s $(MFSdir)/mfcntl.F .
mfmach.F:
	rm -f mfmach.F
	ln -s $(MFSdir)/mfmach.F .
mfmode.F:
	rm -f mfmode.F
	ln -s $(MFSdir)/mfmode.F .
mfsbld.F:
	rm -f mfsbld.F
	ln -s $(MFSdir)/mfsbld.F .
mfsbps.F:
	rm -f mfsbps.F
	ln -s $(MFSdir)/mfsbps.F .
mfsplt.F:
	rm -f mfsplt.F
	ln -s $(MFSdir)/mfsplt.F .
mfutil.F:
	rm -f mfutil.F
	ln -s $(MFSdir)/mfutil.F .
mitzero.F:
	rm -f mitzero.F
	ln -s $(DFSdir)/mitzero.F .
moistr.F:
	rm -f moistr.F
	ln -s $(BLDdir)/moistr.F .
monitor.F:
	rm -f monitor.F
	ln -s $(PLTdir)/monitor.F .
montrb.F:
	rm -f montrb.F
	ln -s $(BLDdir)/montrb.F .
mstmsc.F:
	rm -f mstmsc.F
	ln -s $(BLDdir)/mstmsc.F .
mzmrx5.F:
	rm -f mzmrx5.F
	ln -s $(DFSdir)/mzmrx5.F .
mzout6.F:
	rm -f mzout6.F
	ln -s $(BLDdir)/mzout6.F .
mzoutp.F:
	rm -f mzoutp.F
	ln -s $(BLDdir)/mzoutp.F .
ncm_overheating.F:
	rm -f ncm_overheating.F
	ln -s $(BLDdir)/ncm_overheating.F .
nwkrewr.F:
	rm -f nwkrewr.F
	ln -s $(COMdir)/nwkrewr.F .
pcloop.F:
	rm -f pcloop.F
	ln -s $(PLTdir)/pcloop.F .
pcomp1.F:
	rm -f pcomp1.F
	ln -s $(PLTdir)/pcomp1.F .
pcomp2.F:
	rm -f pcomp2.F
	ln -s $(PLTdir)/pcomp2.F .
pcomp3.F:
	rm -f pcomp3.F
	ln -s $(PLTdir)/pcomp3.F .
pcomp4.F:
	rm -f pcomp4.F
	ln -s $(PLTdir)/pcomp4.F .
pcomps1.F:
	rm -f pcomps1.F
	ln -s $(PLTdir)/pcomps1.F .
pcomps.F:
	rm -f pcomps.F
	ln -s $(PLTdir)/pcomps.F .
plibsv.F:
	rm -f plibsv.F
	ln -s $(PLTdir)/plibsv.F .
plot.F:
	rm -f plot.F
	ln -s $(DFSdir)/plot.F .
pltcfg.F:
	rm -f pltcfg.F
	ln -s $(COMdir)/pltcfg.F .
pmatrx.F:
	rm -f pmatrx.F
	ln -s $(PLTdir)/pmatrx.F .
pplib.F:
	rm -f pplib.F
	ln -s $(PLTdir)/pplib.F .
ppsub.F:
	rm -f ppsub.F
	ln -s $(PLTdir)/ppsub.F .
precal.F:
	rm -f precal.F
	ln -s $(BLDdir)/precal.F .
psychro.F:
	rm -f psychro.F
	ln -s $(COMdir)/psychro.F .
rnor.F:
	rm -f rnor.F
	ln -s $(COMdir)/rnor.F .
rwipv.F:
	rm -f rwipv.F
	ln -s $(COMdir)/rwipv.F .
rwroam.F:
	rm -f rwroam.F
	ln -s $(COMdir)/rwroam.F .
rwsbem.F:
	rm -f rwsbem.F
	ln -s $(COMdir)/rwsbem.F .
senrwl.F:
	rm -f senrwl.F
	ln -s $(COMdir)/senrwl.F .
sensit.F:
	rm -f sensit.F
	ln -s $(BLDdir)/sensit.F .
sentim.F:
	rm -f sentim.F
	ln -s $(BLDdir)/sentim.F .
solar.F:
	rm -f solar.F
	ln -s $(BLDdir)/solar.F .
sort.F:
	rm -f sort.F
	ln -s $(COMdir)/sort.F .
spmatl.F:
	rm -f spmatl.F
	ln -s $(BLDdir)/spmatl.F .
spmisc.F:
	rm -f spmisc.F
	ln -s $(COMdir)/spmisc.F .
startup.F:
	rm -f startup.F
	ln -s $(COMdir)/startup.F .
stofunc.F:
	rm -f stofunc.F
	ln -s $(PLTdir)/stofunc.F .
subsys.F:
	rm -f subsys.F
	ln -s $(BLDdir)/subsys.F .
tdfile.F:
	rm -f tdfile.F
	ln -s $(COMdir)/tdfile.F .
trstyp.F:
	rm -f trstyp.F
	ln -s $(PLTdir)/trstyp.F .
tscon.F:
	rm -f tscon.F
	ln -s $(BLDdir)/tscon.F .
utilf.F:
	rm -f utilf.F
	ln -s $(BLDdir)/utilf.F .
aim2_module.F:
	rm -f aim2_module.F
	ln -s $(BLDdir)/aim2_module.F .
aim2_timestep.F:
	rm -f aim2_timestep.F
	ln -s $(BLDdir)/aim2_timestep.F .
MechVentIN.F:
	rm -f MechVentIN.F
	ln -s $(CETCdir)/MechVentIN.F .
CentralVentSys.F:
	rm -f CentralVentSys.F
	ln -s $(CETCdir)/CentralVentSys.F .
hvacinput.F:
	rm -f hvacinput.F
	ln -s $(CETCdir)/hvacinput.F .
hvacsim.F:
	rm -f hvacsim.F
	ln -s $(CETCdir)/hvacsim.F .
init_hvac_var.F:
	rm -f init_hvac_var.F
	ln -s $(CETCdir)/init_hvac_var.F .
fan.F:
	rm -f fan.F
	ln -s $(CETCdir)/fan.F .
furnace.F:
	rm -f furnace.F
	ln -s $(CETCdir)/furnace.F .
ashp.F:
	rm -f ashp.F
	ln -s $(CETCdir)/ashp.F .
ashp_cooling.F:
	rm -f ashp_cooling.F
	ln -s $(CETCdir)/ashp_cooling.F .
baseboard.F:
	rm -f baseboard.F
	ln -s $(CETCdir)/baseboard.F .
hvac_output.F:
	rm -f hvac_output.F
	ln -s $(CETCdir)/hvac_output.F .
h3kstore.F:
	rm -f h3kstore.F
	ln -s $(CETCdir)/h3kstore.F .
h3koutput.F:
	rm -f h3koutput.F
	ln -s $(CETCdir)/h3koutput.F .
h3kmodule.f90:
	rm -f h3kmodule.f90
	ln -s $(CETCdir)/h3kmodule.f90 .
FC_components.F:
	rm -f FC_components.F
	ln -s $(CETCdir)/FC_components.F .
FC_functions.F:
	rm -f FC_functions.F
	ln -s $(CETCdir)/FC_functions.F .
water_tanks.F:
	rm -f water_tanks.F
	ln -s $(PLTdir)/water_tanks.F .
h3k_utils.F:
	rm -f h3k_utils.F
	ln -s $(CETCdir)/h3k_utils.F .
gshpinput.F:
	rm -f gshpinput.F
	ln -s $(CETCdir)/gshpinput.F .
gshpsim.F:
	rm -f gshpsim.F
	ln -s $(CETCdir)/gshpsim.F .
GCEP_SIM.F:
	rm -f GCEP_SIM.F
	ln -s $(CETCdir)/GCEP_SIM.F .
GCEP_MLAiterate.F:
	rm -f GCEP_MLAiterate.F
	ln -s $(CETCdir)/GCEP_MLAiterate.F .
GCEP_load.F:
	rm -f GCEP_load.F
	ln -s $(CETCdir)/GCEP_load.F .
GCEPtground.F:
	rm -f GCEPtground.F
	ln -s $(CETCdir)/GCEPtground.F .
GCEPInit.F:
	rm -f GCEPInit.F
	ln -s $(CETCdir)/GCEPInit.F .
GCEPinput.F:
	rm -f GCEPinput.F
	ln -s $(CETCdir)/GCEPinput.F .
SetDllParms.F:
	rm -f SetDllParms.F
	ln -s $(CETCdir)/SetDllParms.F .
AIMS.F:
	rm -f AIMS.F
	ln -s $(PLTdir)/AIMS.F .
TAC.F:
	rm -f TAC.F
	ln -s $(CETCdir)/TAC.F .
WaterProperties.F:
	rm -f WaterProperties.F
	ln -s $(CETCdir)/WaterProperties.F .
DG_controller.F:
	rm -f DG_controller.F
	ln -s $(CETCdir)/DG_controller.F .
elec_net_utilities.F:
	rm -f elec_net_utilities.F
	ln -s $(CETCdir)/elec_net_utilities.F .
PEM_FC_pt1.F:
	rm -f PEM_FC_pt1.F
	ln -s $(CETCdir)/PEM_FC_pt1.F
PEM_FC_pt2.F:
	rm -f PEM_FC_pt2.F
	ln -s $(CETCdir)/PEM_FC_pt2.F
window_control.F:
	rm -f window_control.F
	ln -s $(CETCdir)/window_control.F
powoc_calc.F:
	rm -f powoc_calc.F
	ln -s $(PFSdir)/powoc_calc.F .
h3k_report_data.F:
	rm -f h3k_report_data.F
	ln -s $(CETCdir)/h3k_report_data.F .
DHW_module.F:
	rm -f DHW_module.F
	ln -s $(CETCdir)/DHW_module.F .
h3k_plant_postprocess.F:
	rm -f h3k_plant_postprocess.F
	ln -s $(CETCdir)/h3k_plant_postprocess.F
radiant_floor.F:
	rm -f radiant_floor.F
	ln -s $(CETCdir)/radiant_floor.F .
h2_matrix_library.F:
	rm -f h2_matrix_library.F
	ln -s $(CETCdir)/h2_matrix_library.F .
h2_components.F:
	rm -f h2_components.F
	ln -s $(CETCdir)/h2_components.F .
Annex42_combustion_CHP.F:
	rm -f Annex42_combustion_CHP.F
	ln -s $(PLTdir)/Annex42_combustion_CHP.F .
chemical_properties.F:
	rm -f chemical_properties.F
	ln -s $(CETCdir)/chemical_properties.F
h2_comps.F:
	rm -f h2_comps.F
	ln -s $(CETCdir)/h2_comps.F
compressed_cylinder.F:
	rm -f compressed_cylinder.F
	ln -s $(PLTdir)/compressed_cylinder.F
Hydrogen_PEMFC.F:
	rm -f Hydrogen_PEMFC.F
	ln -s $(PLTdir)/Hydrogen_PEMFC.F
Hydrogen_MH_store.F:
	rm -f Hydrogen_MH_store.F
	ln -s $(PLTdir)/Hydrogen_MH_store.F
enet_state_variable_manipulate.F:
	rm -f enet_state_variable_manipulate.F
	ln -s $(PFSdir)/enet_state_variable_manipulate.F
Hydrogen_demand_controller.F:
	rm -f Hydrogen_demand_controller.F
	ln -s $(PLTdir)/Hydrogen_demand_controller.F
MultiYear_climate.F:
	rm -f MultiYear_climate.F
	ln -s $(COMdir)/MultiYear_climate.F
CETC_BATTERY.F:
	rm -f CETC_BATTERY.F
	ln -s $(PFSdir)/CETC_BATTERY.F
plt_zone_gain_coupling.F:
	rm -f plt_zone_gain_coupling.F
	ln -s $(PLTdir)/plt_zone_gain_coupling.F
cetc_cogen.F:
	rm -f cetc_cogen.F
	ln -s $(PLTdir)/cetc_cogen.F
plt_bc_components.F:
	rm -f plt_bc_components.F
	ln -s $(PLTdir)/plt_bc_components.F
RE-H2-ctl.F:
	rm -f RE-H2-ctl.F
	ln -s $(CETCdir)/RE-H2-ctl.F
Res_elec_Ctl.F:
	rm -f Res_elec_Ctl.F
	ln -s $(CETCdir)/Res_elec_Ctl.F
Hydrogen_appliances.F:
	rm -f Hydrogen_appliances.F
	ln -s $(PLTdir)/Hydrogen_appliances.F
wind_turbine_profile_model.F:
	rm -f wind_turbine_profile_model.F
	ln -s $(PFSdir)/wind_turbine_profile_model.F
Annex42_fuel_cell.F:
	rm -f Annex42_fuel_cell.F
	ln -s $(PLTdir)/Annex42_fuel_cell.F .
shocc_dummy.c:
	rm -f shocc_dummy.c
	ln -s $(SHCdir)/shocc_dummy.c .

##Unused 'time-out' code.
##timeout.cpp:
##	rm -f timeout.cpp
##	ln -s $(LIBdir)/timeout.cpp .
## timeout.c:
##	rm -f timeout.c
##	ln -s $(LIBdir)/timeout.c .
ADS_storage_tanks.F:
	rm -f ADS_storage_tanks.F
	ln -s $(PLTdir)/ADS_storage_tanks.F .
ADS_storage_unit.F:
	rm -f ADS_storage_unit.F
	ln -s $(PLTdir)/ADS_storage_unit.F .
ADS_controller.F:
	rm -f ADS_controller.F
	ln -s $(PLTdir)/ADS_controller.F .
solar_collectors.F:
	rm -f solar_collectors.F
	ln -s $(PLTdir)/solar_collectors.F .
ground_temp_mains.F:
	rm -f ground_temp_mains.F
	ln -s $(BLDdir)/ground_temp_mains.F .
readTrnsys.F:
	rm -f readTrnsys.F
	ln -s $(COMdir)/readTrnsys.F .
trnsys_wrapper.F:
	rm -f trnsys_wrapper.F
	ln -s $(CETCdir)/trnsys_wrapper.F .
trnsys_lib.F:
	rm -f trnsys_lib.F
	ln -s $(CETCdir)/trnsys_lib.F .
tank_intank_hx.F:
	rm -f tank_intank_hx.F
	ln -s $(PLTdir)/tank_intank_hx.F .
mains_temp_draw_profiles.F:
	rm -f mains_temp_draw_profiles.F
	ln -s $(PLTdir)/mains_temp_draw_profiles.F
stratified_tank.F:
	rm -f stratified_tank.F
	ln -s $(PLTdir)/stratified_tank.F .
complex_fenestration.F:
	rm -f complex_fenestration.F
	ln -s $(BLDdir)/complex_fenestration.F .
editCFC.F:
	rm -f editCFC.F
	ln -s $(PRJdir)/editCFC.F .
NCHE.F:
	rm -f NCHE.F
	ln -s $(PLTdir)/NCHE.F .
stratified_tank_1HX.F:
	rm -f stratified_tank_1HX.F
	ln -s $(PLTdir)/stratified_tank_1HX.F .
stratified_tank_2HX.F:
	rm -f stratified_tank_2HX.F
	ln -s $(PLTdir)/stratified_tank_2HX.F .
AirFlowModelRoutines.F:
	rm -f AirFlowModelRoutines.F
	ln -s $(BLDdir)/AirFlowModelRoutines.F .
TCC.F:
	rm -f TCC.F
	ln -s $(COSIMdir)/TCC.F .
harmonizer.F:
	rm -f harmonizer.F
	ln -s $(COSIMdir)/harmonizer.F.
harmonizer_timing.F:
	rm -f harmonizer_timing.F
	ln -s $(COSIMdir)/harmonizer_timing.F .
harmonizer_dummy.F:
	rm -f harmonizer_dummy.F
	ln -s $(COSIMdir)/harmonizer_dummy.F .
ESPrTrnsysData.F:
	rm -f ESPrTrnsysData.F
	ln -s $(COSIMdir)/ESPrTrnsysData.F .


strip:
	@if [ -f $(PROG) ]; then strip $(PROG); fi
	@if [ -f $(DLL) ]; then strip $(DLL); fi

install:
	@if [ -f $(PROG) ]; then cp $(PROG) $(ESPBinDir); fi
	@if [ -f $(DLL) ]; then cp $(DLL) $(ESPBinDir); fi

clean_dll:
	rm -f bmatsv.o ESPrTrnsysData.o TCC.o harmonizer.o pcomps.o pmatrix.o \
	ESPrTrnsysData.F TCC.F harmonizer.F pcomps.F pmatrx.F
clean:
	rm -f core $(PROG) $(PROG).exe $(DLL) $(PROG).dll $(PREBPS_OBJECTS) $(PREBPSDLL_OBJECTS) $(MODULE_OBJECTS) $(BPS_OBJECTS) $(MODULES) \
	adaptconf1.F adaptconf2.F adjb.F adjp.F aim2_module.F aim2_timestep.F \
	ascii_mat.F ashp.F ashp_cooling.F baseboard.F basesimp.F basesimp_inputs.F bcfunc.F bctlf.F \
	blibsv.F bmatsu.F bndaryl.F bndarybl.F bndaryt.F bscoeff.F bscoeff_extended.F \
	bsim2cfd.F btherm.F c2fdum.F casual.F CentralVentSys.F \
	CETC_BATTERY.F cetc_cogen.F compressed_cylinder.F ConfigureH3kReports.F \
	ConfigureH3kReports.o cfcalc.F cfconv.F cfdat.F cfdrw.F cfgrid.F cfmgerb.F \
	cfutil.F chemical_properties.F \
	chkconvg.F chtmst.F climi.F cntmnt.F conc.F convect1.F convect2.F ctread.F dayfac.F \
	drylink.F ecasctl.F econstr.F econtrol.F edatabase.F egeometry.F \
	egrid.F emfnetw.F emoist.F enetbound.F enetmath.F enetmisc.F enetpost.F \
	enetrewr.F enetsetu.F enetsolv.F enet_state_variable_manipulate.F eroper.F esgrid.F ESPrTrnsysData.F \
	esru_misc.F esystem.F fan.F filelist.F furnace.F g3dfac.F g3dfnc.F g3dmtx.F gcfunc.F gsdmtx.F \
	gshpinput.F gshpsim.F h2_components.F h2_comps.F h2_matrix_library.F h3koutput.F ESPrTrnsysData.F \
	h3kstore.F hvacinput.F hvac_output.F hvacsim.F Hydrogen_appliances.F TCC.F harmonizer.F harmonizer_timing.F \
	Hydrogen_demand_controller.F Hydrogen_MH_store.F Hydrogen_PEMFC.F harmonizer_dummy.F \
	inital.F initalc.F initalp.F item.F lisolv.F locmage.F matsv.F MechVentIN.F mfbndr.F \
	mfcdat.F mfcntl.F mfmach.F mfmode.F mfsbld.F mfsbps.F mfsplt.F \
	mfutil.F mitzero.F moistr.F monitor.F montrb.F mstmsc.F MultiYear_climate.F mzmrx5.F \
	mzout6.F mzoutp.F ncm_overheating.F nwkrewr.F pcloop.F pcomp1.F pcomp2.F pcomp3.F \
	pcomp4.F pcomps1.F pcomps.F plibsv.F plot.F pltcfg.F plt_bc_components.F \
	plt_zone_gain_coupling.F pmatrx.F pplib.F ppsub.F precal.F \
	psychro.F rnor.F rwipv.F rwroam.F rwsbem.F shocc_dummy.c senrwl.F sensit.F sentim.F solar.F sort.F \
	Annex42_fuel_cell.F spmatl.F stofunc.F \
	spmisc.F startup.F Annex42_combustion_CHP.F subsys.F tdfile.F trstyp.F tscon.F \
	TWildCards.cpp utilf.F Vnt_data.mod wind_turbine_profile_model.F \
	init_hvac_var.F \
	GCEPinput.F GCEP_load.F GCEP_SIM.F GCEP_MLAiterate.F GCEPtground.F GCEPInit.F \
	FC_components.F FC_functions.F water_tanks.F h3k_utils.F SetDllParms.F \
	AIMS.F TAC.F WaterProperties.F DG_controller.F elec_net_utilities.F \
	window_control.F PEM_FC_pt1.F PEM_FC_pt2.F powoc_calc.F \
	h3k_report_data.F radiant_floor.F DHW_module.F h3k_plant_postprocess.F RE-H2-ctl.F \
	ADS_storage_tanks.F ADS_storage_unit.F ADS_controller.F solar_collectors.F \
	ground_temp_mains.F readTrnsys.F trnsys_wrapper.F trnsys_lib.F \
	tank_intank_hx.F mains_temp_draw_profiles.F stratified_tank.F \
	complex_fenestration.F editCFC.F NCHE.F stratified_tank_1HX.F stratified_tank_2HX.F \
	AirFlowModelRoutines.F Res_elec_Ctl.F h3kmodule.f90
