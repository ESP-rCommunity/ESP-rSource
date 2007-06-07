# ESP-r install makefile called by INSTALL script.
# (see README file before installing).
SRCdir=`pwd`

PROGRAMS = aco bps c2e cfg clm dbm dfs e2r ecnv eco grd ish mfs \
	   mld mrt pdb prj res tdf vew

allX11: libX11 $(PROGRAMS)
allGTK: libGTK $(PROGRAMS)
allnoX: libnoX $(PROGRAMS)

# LIBRARIES:

libX11:
	cd $(SRCdir)/lib; \
	$(MAKE) sourcesX11 ; \
	$(MAKE) the_libX11

libGTK:
	cd $(SRCdir)/lib; \
	$(MAKE) sourcesGTK ; \
	$(MAKE) the_libGTK

libnoX:
	cd $(SRCdir)/lib; \
	$(MAKE) sourcesnoX ; \
	$(MAKE) the_libnoX

# PROGRAMS:

aco: 
	cd $(SRCdir)/esruaco; $(MAKE)
b2e: 
	cd $(SRCdir)/esrub2e; $(MAKE)
# pass the target 'include_XML/exclude_XML' to build
# bps with/without H3Kreports.
bps: 
	cd $(SRCdir)/esrubps; $(MAKE) $(xml_status)
dbm: 
	cd $(SRCdir)/esrudbm; $(MAKE)
dfs: 
	cd $(SRCdir)/esrudfs; $(MAKE)
c2e: 
	cd $(SRCdir)/esruc2e; $(MAKE)
cfg: 
	cd $(SRCdir)/esrucfg; $(MAKE)
clm: 
	cd $(SRCdir)/esruclm; $(MAKE)
e2r: 
	cd $(SRCdir)/esrue2r; $(MAKE)
ecnv:
	cd $(SRCdir)/esrucnv; $(MAKE)
eco: 
	cd $(SRCdir)/esrueco; $(MAKE)
grd: 
	cd $(SRCdir)/esrugrd; $(MAKE)
ish: 
	cd $(SRCdir)/esruish; $(MAKE)
mfs: 
	cd $(SRCdir)/esrumfs; $(MAKE)
mld: 
	cd $(SRCdir)/esrumld; $(MAKE)
mrt: 
	cd $(SRCdir)/esrumrt; $(MAKE)
net: 
	cd $(SRCdir)/esrunet; $(MAKE)
pdb: 
	cd $(SRCdir)/esrupdb; $(MAKE)
prj: 
	cd $(SRCdir)/esruprj; $(MAKE)
res: 
	cd $(SRCdir)/esrures; $(MAKE)
tdf: 
	cd $(SRCdir)/esrutdf; $(MAKE)
vew: 
	cd $(SRCdir)/esruvew; $(MAKE)
vww: 
	cd $(SRCdir)/esruvww; $(MAKE)

clean:
	cd $(SRCdir)/lib;     $(MAKE) clean
	cd $(SRCdir)/esruaco; $(MAKE) clean
	cd $(SRCdir)/esrub2e; $(MAKE) clean
	cd $(SRCdir)/esrubps; $(MAKE) clean
	cd $(SRCdir)/esrudbm; $(MAKE) clean
	cd $(SRCdir)/esrudfs; $(MAKE) clean
	cd $(SRCdir)/esruc2e; $(MAKE) clean
	cd $(SRCdir)/esrucfg; $(MAKE) clean
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
	cd $(SRCdir)/esrutdf; $(MAKE) clean
	cd $(SRCdir)/esruvew; $(MAKE) clean
	cd $(SRCdir)/esruvww; $(MAKE) clean
	@if [ -f $(SRCdir)/include/espinstalldir.h ]; then rm $(SRCdir)/include/espinstalldir.h; fi
	@if [ -f $(SRCdir)/include/build_info.h    ]; then rm $(SRCdir)/include/build_info.h;    fi	

strip:
	cd $(SRCdir)/esruaco; $(MAKE) strip
	cd $(SRCdir)/esrub2e; $(MAKE) strip
	cd $(SRCdir)/esrubps; $(MAKE) strip
	cd $(SRCdir)/esrudbm; $(MAKE) strip
	cd $(SRCdir)/esrudfs; $(MAKE) strip
	cd $(SRCdir)/esruc2e; $(MAKE) strip
	cd $(SRCdir)/esrucfg; $(MAKE) strip
	cd $(SRCdir)/esruclm; $(MAKE) strip
	cd $(SRCdir)/esrue2r; $(MAKE) strip
	cd $(SRCdir)/esrucnv; $(MAKE) strip
	cd $(SRCdir)/esrueco; $(MAKE) strip
	cd $(SRCdir)/esrugrd; $(MAKE) strip
	cd $(SRCdir)/esruish; $(MAKE) strip
	cd $(SRCdir)/esrumfs; $(MAKE) strip
	cd $(SRCdir)/esrumld; $(MAKE) strip
	cd $(SRCdir)/esrumrt; $(MAKE) strip
	cd $(SRCdir)/esrunet; $(MAKE) strip
	cd $(SRCdir)/esrupdb; $(MAKE) strip
	cd $(SRCdir)/esruprj; $(MAKE) strip
	cd $(SRCdir)/esrures; $(MAKE) strip
	cd $(SRCdir)/esrutdf; $(MAKE) strip
	cd $(SRCdir)/esruvew; $(MAKE) strip
	cd $(SRCdir)/esruvww; $(MAKE) strip

InstBins:
	cd $(SRCdir)/esruaco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrub2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrubps; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrudbm; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrudfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruc2e; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrucfg; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruclm; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrue2r; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrucnv; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrueco; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrugrd; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruish; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrumfs; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrumld; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrumrt; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrunet; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrupdb; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruprj; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrures; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esrutdf; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruvew; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install ;\
	cd $(SRCdir)/esruvww; $(MAKE) ESPBinDir=${ESPBinDir} EXE=${EXE} install

tars: clean
	rm -f esp-r.tar esp-r.tar.gz esp-r_training.tar esp-r_training.tar.gz
	tar cvf - esru* archive bitmaps lib cetc Install* Makefile* Readme* bin \
	 climate databases GPL.txt Version env include manual tutorial \
	 validation common_list shocc src2html > esp-r.tar 2> esp-r.list; gzip -9 esp-r.tar
	tar cvf - training/[a-z]* training/3* > esp-r_training.tar 2>> esp-r.list; gzip -9 esp-r_training.tar
