C This file contains a range of stochastic functions for use in ESP-r
C plant and systems modelling. 
C The routines described are:
C StdDisVal - returns a value based on a standard distribution 
C           about a mean.

C ------------------------- StdDisVal ---------------------------------
C Thus routine returns a value based on a user defined probability density 
C function (pdf). The maximum and minimum values are set as +/- PI standard deviations.
C The parameters passed are:
C MeanVal - the mean value
C StdDev  - the standard deviation about the mean
C Incr   -  increment
C Rseed   - random seed (integer)
C ActVal  - the specific value calculated [Returned]
C DisPos  - force distribution to be > 0

      Subroutine StdDisVal(MeanVal,StdDev,Incr,ActVal,DisPos)

      implicit none

      COMMON/OUTIN/IUOUT,IUIN
      
      Real MeanVal,StdDev,Incr,ActVal,ValMin,ValMax,Thresh,Cprob,
     &PI,ProbT,Ninc, Rand_No, Prob1, Prob2,LNMeanVal,LNStdDev
      Integer iuout,iuin
      Logical Lcalc,DisPos

      Character outs*124


C Initialise variables - initially setting the duration to the mean value.
      PI=22.0/7.0
      Cprob=0.0
      ActVal=MeanVal
      Thresh=Rand_No()
      Lcalc=.true.

      ValMin=MeanVal-3*StdDev
      IF(DisPos)THEN
        IF(ValMin.LT.0.0)ValMin=1.0E-6
      ENDIF
      ValMax=MeanVal+3*StdDev
      Ninc=ValMin


C Error checking

      if(StdDev.lt.1.0E-6)then
        Lcalc=.false.
        write(outs,'(a)')
     &  'Stochastic.F; StdDisVal; Warning: std dist ~ 0!' 
        call edisp(iuout,outs)
        write(outs,'(a)')
     &  'return value has been set to the mean' 
        call edisp(iuout,outs)
      endif 
      if((ValMax-ValMin).lt.Incr)then
        Lcalc=.false.
        write(outs,'(a)')
     &  'Stochastic.F; StdDisVal; Warning: increment > max/min range' 
        call edisp(iuout,outs)
        write(outs,'(a)')
     &  'return value has been set to the mean' 
        call edisp(iuout,outs)
      endif

C Loop through and calculate the cumulative probability at user defined 
C increments
      Do while((Ninc.le.ValMax).and.Lcalc)
        Ninc=Ninc+Incr

C For a positive distribution then force a lognormal distribution if minimum<0.
        if(DisPos)then
          if(Ninc.LT.1.0E-6)Ninc=1.0E-6
          LNStdDev=(log((StdDev/MeanVal)**2.0+1))**0.5
          LNMeanVal=
     &log((MeanVal**2.0)/((StdDev**2.0+MeanVal**2.0)**0.5))
          Prob1=1/(Ninc*LNStdDev*((2.0*PI)**0.5))
          Prob2=((log(Ninc)-LNMeanVal)**2.0)/(2.0*(LNStdDev**2.0))
          ProbT=Incr*(Prob1*EXP(-Prob2))
        else
          Prob1=1/(StdDev*((2.0*PI)**0.5))
          Prob2=((Ninc-MeanVal)**2.0)/(2.0*(StdDev**2.0))
          ProbT=Incr*(Prob1*EXP(-Prob2))
        endif

        Cprob=Cprob+ProbT

C Return the actual duration
        if(Cprob.ge.Thresh)then
          ActVal=Ninc
          Lcalc=.false.
        endif
      EndDo  

C Check if Cprob has still not reached thresh i.e. Rand > 0.99 
      if((Cprob.lt.Thresh).and.Lcalc) then
          ActVal=ValMax        
      endif 

      Return
      End

      SUBROUTINE SRAND(ISEED)
C
C  This subroutine sets the integer seed to be used with the
C  companion RAND function to the value of ISEED.  A flag is 
C  set to indicate that the sequence of pseudo-random numbers 
C  for the specified seed should start from the beginning.
C
      COMMON /SEED/JSEED,IFRST
C
      JSEED = ISEED
      IFRST = 0
C
      RETURN
      END

      REAL FUNCTION RAND_NO()
C
C  This function returns a pseudo-random number for each invocation.
C  It is a FORTRAN 77 adaptation of the "Integer Version 2" minimal 
C  standard number generator whose Pascal code appears in the article:
C
C     Park, Steven K. and Miller, Keith W., "Random Number Generators: 
C     Good Ones are Hard to Find", Communications of the ACM, 
C     October, 1988.
C
      PARAMETER (MPLIER=16807,MODLUS=2147483647,MOBYMP=127773,
     +           MOMDMP=2836)
C
      COMMON  /SEED/JSEED,IFRST
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      SAVE    NEXTN
C
      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      ENDIF
C
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      ENDIF
      RAND_NO = REAL(NEXTN)/REAL(MODLUS)
C
      RETURN
      END
      BLOCKDATA RANDBD
      COMMON /SEED/JSEED,IFRST
C
      DATA JSEED,IFRST/123456789,0/
C
      END


