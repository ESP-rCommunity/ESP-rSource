C
C lookup_data.h
C 
C This file contains data structures used by the lookup data
C (in table format) facility.


C Flag indicating that lookup table data file has been defined

        logical bLookup_data_defined

C Name of lookup table data file
 
        character*72 cLookup_data_file_name

C Flag indicating that there is an error with the lookup
C table data file

        logical bLookup_data_err

C Unit number for temporary file used during run-time

        integer iLookup_TmpFile

C Named constant for temporary binary file name

        character*72 cLookup_TmpFile_name
        data cLookup_TmpFile_name /'TAB.tmp'/

C Maximum number of columns in lookup table

        integer iLookup_max_cols
        parameter ( iLookup_max_cols = 20 )

C Maximum number of rows in lookup table
        integer iLookup_max_rows
        parameter ( iLookup_max_rows = 100 )

C Version number

        real fLookup_file_version

C Number of defined columns and rows in lookup table 

        integer iLookup_col_count           ! columns
        integer iLookup_row_count           ! rows

C Lookup table data names, units and column numbers

        character*248 cLookup_col_names( iLookup_max_cols )
        character*248 cLookup_col_units( iLookup_max_cols )


C Common data structure

        common/Lookup_data_statc/ cLookup_data_file_name,
     &                            cLookup_col_names,
     &                            cLookup_col_units
  
        common/Lookup_data_statb/ bLookup_data_defined,
     &                            bLookup_data_err

        common/Lookup_data_statn/ iLookup_TmpFile,
     &                            iLookup_col_count,
     &                            iLookup_row_count
