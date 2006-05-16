
	PROGRAM HELLO
	
	CHARACTER*128 H3K_rep_NAME

	Do i = 1, 10512

C.......H3Kreports.(INFILTRATION).start......................
C.......The following code is used to trasnport data to 
C.......the H3K reporting object
C.......
C.......Any changes to this code should be made within
C.......the H3Kreports demarcaration comments

C.......Water temperature (average of present and future)
C.......-> set name
        WRITE(H3K_rep_NAME,'(A)') 
     &     'building/infiltration/air_infiltration'
C.......-> update meta-data
        call REP_SET_META(
     &       H3K_rep_NAME,
     &       'units',
     &       '(m3/s)'
     &       )
C.......-> update data 
        call REP_REPORT(
     &       3.0,
     &       H3K_rep_NAME
     &       )
        WRITE(H3K_rep_NAME,'(A)') 
     &     'building/infiltration/air_changes_per_hour'
C.......-> update meta-data
        call REP_SET_META(
     &       H3K_rep_NAME,
     &       'units',
     &       '(ACH)'
     &       )
C.......-> update data 
        call REP_REPORT(
     &       4.0,
     &       H3K_rep_NAME
     &       )


C.......H3Kreports.(INFILTRATION).start......................
	
		HOUR = (i/12);
		do while(HOUR .gt. 11) 
			HOUR = HOUR - 12
		end do
		IF( i .gt. 1000) THEN 
			CALL REP_UPDATE(i, HOUR, (i/(12*24)))
		ENDIF
	End Do
	
	CALL REP_XML_SUMMARY
	
	END
