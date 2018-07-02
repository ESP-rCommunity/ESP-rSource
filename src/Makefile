# ESP-r install makefile called by INSTALL script.
# (see README file before installing).
SRCdir=`pwd`

PROGRAMS = aco bps c2e cdb clm dfs e2r ecnv eco enet grd harmonizer ish mfs \
	   mld mrt pdb prj res vew stats

allX11: libX11 $(PROGRAMS)
allGTK: libGTK $(PROGRAMS)
allnoX: libnoX $(PROGRAMS)

# LIBRARIES:

libX11:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libX11; \
	$(MAKE) the_libVer

libGTK:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libGTK; \
	$(MAKE) the_libVer

libnoX:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libnoX; \
	$(MAKE) the_libVer

enableXML:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libH3KReports
	
disableXML:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libNoH3KReports

enableSQLite:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libSQLite
	
disableSQLite:
	cd $(SRCdir)/lib; \
	$(MAKE) the_libNoSQLite

InstBins:
	$(eval doInstall=yes)

# PROGRAMS:

export FMIarg

aco:
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eaco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eaco; $(MAKE); \
	fi
b2e:
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eb2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eb2e; $(MAKE); \
	fi 
bps: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/ebps; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/ebps; $(MAKE) theEXE; \
	fi
bpsdll: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/ebps; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/ebps; $(MAKE) theDLL; \
	fi
dfs: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/edfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/edfs; $(MAKE); \
	fi
dfv: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/edfv; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/edfv; $(MAKE); \
	fi
c2e: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/ec2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/ec2e; $(MAKE); \
	fi
cdb: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/ecdb; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/ecdb; $(MAKE); \
	fi
clm: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eclm; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eclm; $(MAKE); \
	fi
e2r: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/ee2r; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/ee2r; $(MAKE); \
	fi
ecnv: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/cnv; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/cnv; $(MAKE); \
	fi
eco: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eeco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eeco; $(MAKE); \
	fi
grd: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/egrd; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/egrd; $(MAKE); \
	fi
harmonizer: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/harmonizer; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/harmonizer; $(MAKE) theExe; \
	fi
harmonizerdll: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/harmonizer; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/harmonizer; $(MAKE) theDLL; \
	fi
ish: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eish; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eish; $(MAKE); \
	fi
mfs: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/emfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/emfs; $(MAKE); \
	fi
mld: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/emld; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/emld; $(MAKE); \
	fi
mrt: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/emrt; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/emrt; $(MAKE); \
	fi
enet: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/net; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/net; $(MAKE); \
	fi
pdb: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/epdb; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/epdb; $(MAKE); \
	fi
prj: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eprj; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eprj; $(MAKE); \
	fi
res: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/eres; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/eres; $(MAKE); \
	fi
vew: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/evew; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/evew; $(MAKE); \
	fi
stats: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/estats; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/estats; $(MAKE); \
	fi

clean:
	cd $(SRCdir)/lib;     $(MAKE) clean
	cd $(SRCdir)/eaco; $(MAKE) clean
	cd $(SRCdir)/eb2e; $(MAKE) clean
	cd $(SRCdir)/ebps; $(MAKE) clean
	cd $(SRCdir)/edfs; $(MAKE) clean
	cd $(SRCdir)/edfv; $(MAKE) clean
	cd $(SRCdir)/ec2e; $(MAKE) clean
	cd $(SRCdir)/ecdb; $(MAKE) clean
	cd $(SRCdir)/eclm; $(MAKE) clean
	cd $(SRCdir)/ee2r; $(MAKE) clean
	cd $(SRCdir)/cnv; $(MAKE) clean
	cd $(SRCdir)/eeco; $(MAKE) clean
	cd $(SRCdir)/egrd; $(MAKE) clean
	cd $(SRCdir)/eish; $(MAKE) clean
	cd $(SRCdir)/emfs; $(MAKE) clean
	cd $(SRCdir)/emld; $(MAKE) clean
	cd $(SRCdir)/emrt; $(MAKE) clean
	cd $(SRCdir)/net; $(MAKE) clean
	cd $(SRCdir)/epdb; $(MAKE) clean
	cd $(SRCdir)/eprj; $(MAKE) clean
	cd $(SRCdir)/eres; $(MAKE) clean
	cd $(SRCdir)/evew; $(MAKE) clean
	cd $(SRCdir)/estats; $(MAKE) clean
	cd $(SRCdir)/harmonizer; $(MAKE) clean
	@if [ -f $(SRCdir)/include/espinstalldir.h ]; then rm $(SRCdir)/include/espinstalldir.h; fi
	@if [ -f $(SRCdir)/include/build_info.h    ]; then rm $(SRCdir)/include/build_info.h;    fi	

strip:
	cd $(SRCdir)/eaco; $(MAKE) strip
	cd $(SRCdir)/eb2e; $(MAKE) strip
	cd $(SRCdir)/ebps; $(MAKE) strip
	cd $(SRCdir)/edfs; $(MAKE) strip
	cd $(SRCdir)/edfv; $(MAKE) strip
	cd $(SRCdir)/ec2e; $(MAKE) strip
	cd $(SRCdir)/ecdb; $(MAKE) strip
	cd $(SRCdir)/eclm; $(MAKE) strip
	cd $(SRCdir)/ee2r; $(MAKE) strip
	cd $(SRCdir)/cnv; $(MAKE) strip
	cd $(SRCdir)/eeco; $(MAKE) strip
	cd $(SRCdir)/egrd; $(MAKE) strip
	cd $(SRCdir)/harmonizer; $(MAKE) strip
	cd $(SRCdir)/eish; $(MAKE) strip
	cd $(SRCdir)/emfs; $(MAKE) strip
	cd $(SRCdir)/emld; $(MAKE) strip
	cd $(SRCdir)/emrt; $(MAKE) strip
	cd $(SRCdir)/net; $(MAKE) strip
	cd $(SRCdir)/epdb; $(MAKE) strip
	cd $(SRCdir)/eprj; $(MAKE) strip
	cd $(SRCdir)/eres; $(MAKE) strip
	cd $(SRCdir)/evew; $(MAKE) strip
	cd $(SRCdir)/estats; $(MAKE) strip

bincopy:
	cp eaco/aco $(ESPBinDir)/aco
	cp eb2e/b2e $(ESPBinDir)/b2e
	cp ebps/bps $(ESPBinDir)/bps
	cp edfs/dfs $(ESPBinDir)/dfs
	cp edfv/dfv $(ESPBinDir)/dfv
	cp ec2e/c2e $(ESPBinDir)/c2e
	cp ecdb/cdb $(ESPBinDir)/cdb
	cp eclm/clm $(ESPBinDir)/clm
	cp ee2r/e2r $(ESPBinDir)/e2r
	cp cnv/ecnv $(ESPBinDir)/ecnv
	cp eeco/eco $(ESPBinDir)/eco
	cp egrd/grd $(ESPBinDir)/grd
	cp eish/ish $(ESPBinDir)/ish
	cp emfs/mfs $(ESPBinDir)/mfs
	cp emld/mld $(ESPBinDir)/mld
	cp emrt/mrt $(ESPBinDir)/espvwf
	cp net/enet $(ESPBinDir)/enet
	cp epdb/pdb $(ESPBinDir)/pdb
	cp eprj/prj $(ESPBinDir)/prj
	cp eres/res $(ESPBinDir)/res
	cp evew/viewer $(ESPBinDir)/vewier
	cp stats/stats $(ESPBinDir)/stats
