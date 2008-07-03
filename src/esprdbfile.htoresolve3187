C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C esprdbfile.h holds common block for database file names and units.

C parameters for generic databases
C MGDB - (integer) number of generic databases
C MGCL - (integer) number of categories in a generic database
C MGIT - (integer) number of items in a generic database
      integer MGDB,MGCL,MGIT 
      parameter (MGDB=13)
      parameter (MGCL=36)
      parameter (MGIT=600)

C Array of generic database file names (72 char).
      common/gendbf/gendbfn(MGDB)
      character gendbfn*72

C Array of default generic database file names (72 char).
      common/dgendbf/dgendbfn(MGDB)
      character dgendbfn*72

C Array of file unit numbers for generic databases.
      common/gendbfi/igendbf(MGDB)
      integer igendbf

C Array of logicals indicating generic database scanned ok.
      common/gendb0/gendbok(MGDB)
      logical gendbok

C Standard path to corporate databases (based on Install data)
      common/stddbpath/standarddbpath
      character standarddbpath*72

C Array of integers indicating whether database is defined via
C an absolute path (zero), local path (one), standard path (2)
      common/whichdbpath/ipathapres,ipathoptdb,ipathpcdb,ipathprodb,
     &                   ipathmat,ipathmul,ipathclim
      integer ipathapres,ipathoptdb,ipathpcdb,ipathprodb,
     &                   ipathmat,ipathmul,ipathclim

C LAPRES - (144 char) wind pressure coefficients distribution database
C iapres  - file unit for wind pressure coef database
      common/APRES/LAPRES,IAPRES
      character LAPRES*144

C LOPTDB - (144 char) optical database file name
C IOPTDB - unit number of optical database
      COMMON/GOPTDB/LOPTDB,IOPTDB
      character LOPTDB*144

C LPCDB - (144 char) plant template database file name
C IPCDB - unit number of plant template database
      common/C7/LPCDB,IPCDB
      character lpcdb*144

C LPRFDB - (144 char) event profile database file name
C IPRODB - unit number of event profile database
      COMMON/PRODB/LPRFDB,IPRODB
      character LPRFDB*144

C LFMAT - (72 char) materials db file name
C LFMUL - (72 char) constructions db file name
C IFMAT - unit number of materials database
C IFMUL - unit number of constructions database
      common/CONDB/LFMAT,IFMAT,LFMUL,IFMUL
      character LFMAT*72,LFMUL*72

C LCLIM - (72 char) climate file name associated with the model
C ICLIM - unit number of climate file
      COMMON/C22/ICLIM,LCLIM
      character LCLIM*72

C Default file names for databases (72 char):
C DCLIM - default climate, DAPRES - default pressure coef
C DFCON - default materials, DFMUL - default constructions
C DOPTDB - default optics (144 char), DPRFDB - default events (144 char)
C DPCDB - default plant components
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character*72 DCLIM,DFCON,DFMUL
      character DOPTDB*144,DPRFDB*144,DAPRES*144,DPCDB*144

C LSBEM - (72 char) default SBEM building type and systems db file name
      COMMON/SBEM00/LSBEM
      character*72 LSBEM

C Define as integers.
      integer iapres,ioptdb,ipcdb,iprodb,ifmat,ifmul,iclim

C end of esp-r_dbfile.h
