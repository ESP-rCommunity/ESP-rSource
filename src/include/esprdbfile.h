C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C esprdbfile.h holds common block for database file names and units.

C LAPRES - (72 char) wind pressure coefficients distribution database
C iapres  - file unit for wind pressure coef database
      common/APRES/LAPRES,IAPRES
      character LAPRES*72

C LOPTDB - (72 char) optical database file name
C IOPTDB - unit number of optical database
      COMMON/GOPTDB/LOPTDB,IOPTDB
      character LOPTDB*72

C LPCDB - (72 char) plant template database file name
C IPCDB - unit number of plant template database
      common/C7/LPCDB,IPCDB
      character lpcdb*72

C LPRFDB - (72 char) event profile database file name
C IPRODB - unit number of event profile database
      COMMON/PRODB/LPRFDB,IPRODB
      character LPRFDB*72

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
C DOPTDB - default optics, DPRFDB - default events
C DPCDB - default plant components
      COMMON/DEFLT1/DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB
      character*72 DCLIM,DAPRES,DFCON,DFMUL,DOPTDB,DPRFDB,DPCDB

C Define as integers.
      integer iapres,ioptdb,ipcdb,iprodb,ifmat,ifmul,iclim

C end of esp-r_dbfile.h
