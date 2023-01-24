C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C esprdbfile.h holds common block for common data file names and units.

C parameters for generic databases
C MGDB - (integer) number of generic databases
C MGCL - (integer) number of categories in a generic database
C MGIT - (integer) number of items in a generic database
      integer MGDB,MGCL,MGIT
      parameter (MGDB=13)
      parameter (MGCL=36)
      parameter (MGIT=600)

C Standard path to corporate common files (based on Install data)
      common/stddbpath/standarddbpath
      character standarddbpath*72

C Standard path to corporate weather files (based on Install data)
      common/stdclmpath/standardclmpath
      character standardclmpath*72

C Array of integers indicating whether common files are defined via
C an absolute path (zero), local path (one), standard path (2)
      common/whichdbpath/ipathapres,ipathoptdb,ipathpcdb,ipathprodb,
     &                   ipathmat,ipathmul,ipathclim,ipathsbem,
     &                   ipathmsc,ipathmould,ipathcfc,ipathpredef
      integer ipathapres,ipathoptdb,ipathpcdb,ipathprodb,
     &                   ipathmat,ipathmul,ipathclim,ipathsbem,
     &                   ipathmsc,ipathmould,ipathcfc,ipathpredef

C Array of integers representing file unit numbers of databases
      integer iapres  ! file unit of wind pressure coefficients
      integer ioptdb  ! file unit number of common optics
      integer ipcdb   ! file unit number of plant template database
      integer iprodb  ! file unit number of common event profiles
      integer ifmat   ! file unit number of common materials
      integer ifmul   ! file unit number of common constructions
      integer iclim   ! file unit number of climate file
      integer icfcdb  ! file unit number of cfc database file
      common/idbunit/iapres,ioptdb,ipcdb,iprodb,ifmat,ifmul,iclim,
     &               icfcdb
      
C LAPRES - (144 char) common wind pressure coefficients
      common/APRES/LAPRES
      character LAPRES*144

C LOPTDB - (144 char) common optics file name
      COMMON/GOPTDB/LOPTDB
      character LOPTDB*144

C LPCDB - (144 char) plant template database file name
      common/C7/LPCDB
      character lpcdb*144

C parameters for event profiles
C MPRFDBPER - (integer) number of periods in a profile
      integer MPRFDBPER 
      parameter (MPRFDBPER=35)

C LPRFDB - (144 char) common event profile file name
      COMMON/PRODB/LPRFDB
      character LPRFDB*144

C LFMAT - (144 char) common materials file name
C LFMUL - (144 char) connon constructions file name
      common/CONDB/LFMAT,LFMUL
      character LFMAT*144,LFMUL*144

C LFMOULD - (144 char) mould isopleths file name
      common/mouldb/lfmould 
      character lfmould*144

C LCLIM - (144 char) climate file name associated with the model
      COMMON/C22/LCLIM
      character LCLIM*144
      
C LCFCDB - (144 char) CFC db file name associated with the model
      COMMON/CFCDB/LCFCDB
      character LCFCDB*144

C Active components database MCMPDBFL (144 char).
      CHARACTER MCMPDBFL*144
      COMMON/MCMPDBNAM/MCMPDBFL

C Default file names for common data files (all 144 char):
C DCLIM - default climate, DAPRES - default pressure coefficients
C DFCON - default materials, DFMUL - default constructions
C DOPTDB - default optics, DPRFDB - default events
C DPCDB - default plant components, DSBEM - default UK SBEM
C DMCMPDBFL - default active components (144 char),
C dmdbnam - default mould ispleths
C DCFCDB - default CFClayers, DPREDEF - default predefined objects
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB,DSBEM,
     &  DMCMPDBFL,dmdbnam,DCFCDB,DPREDEF
      character DFMUL*144
      character DOPTDB*144,DPRFDB*144,DAPRES*144,DPCDB*144
      character DSBEM*144,DCLIM*144,DFCON*144,DMCMPDBFL*144
      character dmdbnam*144,DCFCDB*144,DPREDEF*144

C LSBEM - (144 char) SBEM building type and systems db file name
      COMMON/SBEM00/LSBEM
      character LSBEM*144

C LPREDEF - (144 char) predefined objects db file name.
      COMMON/CPREDEF/LPREDEF
      character LPREDEF*144

C end of esp-r_dbfile.h
