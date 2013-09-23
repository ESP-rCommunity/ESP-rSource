
	PROGRAM HELLO
	use h3kmodule

	Do i = 1, 10512

C.......H3Kreports.(INFILTRATION).start......................
C.......The following code is used to trasnport data to
C.......the H3K reporting object
C.......
C.......Any changes to this code should be made within
C.......the H3Kreports demarcaration comments

C.......Water temperature (average of present and future)
C.......-> set name
        Call AddToReport(rvBldInfAirInf%Identifier,3.0)

        Call AddToReport(rvBldInfAirChg%Identifier,4.0)


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
