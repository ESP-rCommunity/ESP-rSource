# ESP-r install makefile called by INSTALL script.
# (see README file before installing).
SRCdir=`pwd`

PROGRAMS = aco bps c2e cdb clm dbm dfs e2r ecnv eco enet grd harmonizer ish mfs \
	   mld mrt pdb prj res run vew stats

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
	  cd $(SRCdir)/esruaco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruaco; $(MAKE); \
	fi
b2e:
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrub2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrub2e; $(MAKE); \
	fi 
bps: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrubps; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrubps; $(MAKE) theEXE; \
	fi
bpsdll: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrubps; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrubps; $(MAKE) theDLL; \
	fi
dbm: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrudbm; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrudbm; $(MAKE); \
	fi
dfs: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrudfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrudfs; $(MAKE); \
	fi
dfv: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrudfs; $(MAKE) theCFV ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrudfs; $(MAKE) theCFV; \
	fi
c2e: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esruc2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruc2e; $(MAKE); \
	fi
cdb: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrucdb; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrucdb; $(MAKE); \
	fi
clm: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esruclm; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruclm; $(MAKE); \
	fi
e2r: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrue2r; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrue2r; $(MAKE); \
	fi
ecnv: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrucnv; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrucnv; $(MAKE); \
	fi
eco: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrueco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrueco; $(MAKE); \
	fi
grd: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrugrd; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrugrd; $(MAKE); \
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
	  cd $(SRCdir)/esruish; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruish; $(MAKE); \
	fi
mfs: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrumfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrumfs; $(MAKE); \
	fi
mld: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrumld; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrumld; $(MAKE); \
	fi
mrt: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrumrt; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrumrt; $(MAKE); \
	fi
enet: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrunet; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrunet; $(MAKE); \
	fi
pdb: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrupdb; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrupdb; $(MAKE); \
	fi
prj: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esruprj; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruprj; $(MAKE); \
	fi
res: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrures; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrures; $(MAKE); \
	fi
vew: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esruvew; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esruvew; $(MAKE); \
	fi
stats: 
	if [ "$(doInstall)" = "yes" ]; then \
	  cd $(SRCdir)/esrustats; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install; \
	else \
	  cd $(SRCdir)/esrustats; $(MAKE); \
	fi

clean:
	cd $(SRCdir)/lib;     $(MAKE) clean
	cd $(SRCdir)/esruaco; $(MAKE) clean
	cd $(SRCdir)/esrub2e; $(MAKE) clean
	cd $(SRCdir)/esrubps; $(MAKE) clean
	cd $(SRCdir)/esrudbm; $(MAKE) clean
	cd $(SRCdir)/esrudfs; $(MAKE) clean
	cd $(SRCdir)/esruc2e; $(MAKE) clean
	cd $(SRCdir)/esrucdb; $(MAKE) clean
	cd $(SRCdir)/esruclm; $(MAKE) clean
	cd $(SRCdir)/esrue2r; $(MAKE) clean
	cd $(SRCdir)/esrucnv; $(MAKE) clean
	cd $(SRCdir)/esrueco; $(MAKE) clean
	cd $(SRCdir)/esrugrd; $(MAKE) clean
	cd $(SRCdir)/esruish; $(MAKE) clean
	cd $(SRCdir)/esrumfs; $(MAKE) clean
	cd $(SRCdir)/esrumld; $(MAKE) clean
	cd $(SRCdir)/esrumrt; $(MAKE) clean
	cd $(SRCdir)/esrunet; $(MAKE) clean
	cd $(SRCdir)/esrupdb; $(MAKE) clean
	cd $(SRCdir)/esruprj; $(MAKE) clean
	cd $(SRCdir)/esrures; $(MAKE) clean
	cd $(SRCdir)/esruvew; $(MAKE) clean
	cd $(SRCdir)/esrustats; $(MAKE) clean
	cd $(SRCdir)/harmonizer; $(MAKE) clean
	@if [ -f $(SRCdir)/include/espinstalldir.h ]; then rm $(SRCdir)/include/espinstalldir.h; fi
	@if [ -f $(SRCdir)/include/build_info.h    ]; then rm $(SRCdir)/include/build_info.h;    fi	

strip:
	cd $(SRCdir)/esruaco; $(MAKE) strip
	cd $(SRCdir)/esrub2e; $(MAKE) strip
	cd $(SRCdir)/esrubps; $(MAKE) strip
	cd $(SRCdir)/esrudbm; $(MAKE) strip
	cd $(SRCdir)/esrudfs; $(MAKE) strip
	cd $(SRCdir)/esruc2e; $(MAKE) strip
	cd $(SRCdir)/esrucdb; $(MAKE) strip
	cd $(SRCdir)/esruclm; $(MAKE) strip
	cd $(SRCdir)/esrue2r; $(MAKE) strip
	cd $(SRCdir)/esrucnv; $(MAKE) strip
	cd $(SRCdir)/esrueco; $(MAKE) strip
	cd $(SRCdir)/esrugrd; $(MAKE) strip
	cd $(SRCdir)/harmonizer; $(MAKE) strip
	cd $(SRCdir)/esruish; $(MAKE) strip
	cd $(SRCdir)/esrumfs; $(MAKE) strip
	cd $(SRCdir)/esrumld; $(MAKE) strip
	cd $(SRCdir)/esrumrt; $(MAKE) strip
	cd $(SRCdir)/esrunet; $(MAKE) strip
	cd $(SRCdir)/esrupdb; $(MAKE) strip
	cd $(SRCdir)/esruprj; $(MAKE) strip
	cd $(SRCdir)/esrures; $(MAKE) strip
	cd $(SRCdir)/esruvew; $(MAKE) strip
	cd $(SRCdir)/esrustats; $(MAKE) strip

bincopy:
	cp esruaco/aco $(ESPBinDir)/aco
	cp esrub2e/b2e $(ESPBinDir)/b2e
	cp esrubps/bps $(ESPBinDir)/bps
	cp esrudbm/dbm $(ESPBinDir)/dbm
	cp esrudfs/dfs $(ESPBinDir)/dfs
	cp esrudfs/dfv $(ESPBinDir)/dfv
	cp esruc2e/c2e $(ESPBinDir)/c2e
	cp esrucdb/cdb $(ESPBinDir)/cdb
	cp esruclm/clm $(ESPBinDir)/clm
	cp esrue2r/e2r $(ESPBinDir)/e2r
	cp esrucnv/ecnv $(ESPBinDir)/ecnv
	cp esrueco/eco $(ESPBinDir)/eco
	cp esrugrd/grd $(ESPBinDir)/grd
	cp esruish/ish $(ESPBinDir)/ish
	cp esrumfs/mfs $(ESPBinDir)/mfs
	cp esrumld/mld $(ESPBinDir)/mld
	cp esrumrt/mrt $(ESPBinDir)/espvwf
	cp esrunet/enet $(ESPBinDir)/enet
	cp esrupdb/pdb $(ESPBinDir)/pdb
	cp esruprj/prj $(ESPBinDir)/prj
	cp esrures/res $(ESPBinDir)/res
	cp esruvew/viewer $(ESPBinDir)/vewier
	cp esrustats/stats $(ESPBinDir)/stats

tars: clean
	rm -f esp-r.tar esp-r.tar.gz esp-r_training.tar esp-r_training.tar.gz
	tar cvf - esru* archive bitmaps lib cetc Install* Makefile* Readme* bin \
	 climate databases GPL.txt Version env include manual tutorial \
	 validation common_list shocc src2html > esp-r.tar 2> esp-r.list; gzip -9 esp-r.tar
	tar cvf - training/[a-z]* training/3* > esp-r_training.tar 2>> esp-r.list; gzip -9 esp-r_training.tar
