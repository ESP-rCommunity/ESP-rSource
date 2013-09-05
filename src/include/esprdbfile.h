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
     &                   ipathmat,ipathmul,ipathclim,ipathsbem,
     &                   ipathmsc
      integer ipathapres,ipathoptdb,ipathpcdb,ipathprodb,
     &                   ipathmat,ipathmul,ipathclim,ipathsbem,
     &                   ipathmsc

C Array of integers representing file unit numbers of databases
      integer iapres  ! file unit of wind pressure coef database
      integer ioptdb  ! file unit number of optical database
      integer ipcdb   ! file unit number of plant template database
      integer iprodb  ! file unit number of event profile database
      integer ifmat   ! file unit number of materials database
      integer ifmul   ! file unit number of constructions database
      integer iclim   ! file unit number of climate file
      common/idbunit/iapres,ioptdb,ipcdb,iprodb,ifmat,ifmul,iclim
      
C LAPRES - (144 char) wind pressure coefficients distribution database
      common/APRES/LAPRES
      character LAPRES*144

C LOPTDB - (144 char) optical database file name
      COMMON/GOPTDB/LOPTDB
      character LOPTDB*144

C LPCDB - (144 char) plant template database file name
      common/C7/LPCDB
      character lpcdb*144

C parameters for event profile databases
C MPRFDBPER - (integer) number of periods in a profile
      integer MPRFDBPER 
      parameter (MPRFDBPER=35)

C LPRFDB - (144 char) event profile database file name
      COMMON/PRODB/LPRFDB
      character LPRFDB*144

C LFMAT - (144 char) materials db file name
C LFMUL - (144 char) constructions db file name
      common/CONDB/LFMAT,LFMUL
      character LFMAT*144,LFMUL*144

C LCLIM - (144 char) climate file name associated with the model
      COMMON/C22/LCLIM
      character LCLIM*144

C Misc components database MCMPDBFL (144 char).
      CHARACTER MCMPDBFL*144
      COMMON/MCMPDBNAM/MCMPDBFL

C Default file names for databases:
C DCLIM - default climate, DAPRES - default pressure coef (144 char)
C DFCON - default materials (144 char), DFMUL - default constructions (144 char)
C DOPTDB - default optics (144 char), DPRFDB - default events (144 char)
C DPCDB - default plant components (144 char), DSBEM - default UK SBEM (144 char)
C DMCMPDBFL - default mscel components (144 char),
C dmdbnam - default mycotoxin db
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB,DSBEM,
     &  DMCMPDBFL,dmdbnam
      character DFMUL*144
      character DOPTDB*144,DPRFDB*144,DAPRES*144,DPCDB*144
      character DSBEM*144,DCLIM*144,DFCON*144,DMCMPDBFL*144
      character dmdbnam*72

C LSBEM - (144 char) SBEM building type and systems db file name
      COMMON/SBEM00/LSBEM
      character LSBEM*144

C end of esp-r_dbfile.h
