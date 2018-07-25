C Suprf2.f contains subroutines that are called from subroutines in the files 
C Supwi.f, Suple.f, Suprf.f:
C CORSUP
C LININT
C CORRAD
C CORPANS
C CORPAN
C SEARCH

        SUBROUTINE CORSUP (par1,par2,pol,Nmax,Lmax,N,CORP)
c************************************************************************
c Purpose: this routine calculates fitting polinomial equation f(par) by 
c          using the relevant coefficients
c
c Module : # 4.3.1.2.3.2, TG#3, mxg/March 1,1989
c Changes: February 7, 1990 (added Lmax)
c Limits : 
c
c Pass parameters:
c
c IO # Name    unit         description                
c
c I  1 par      (-)   Parameter
c I  2 pol      (-)   Coefficients for the polinomial equation 
c I  3 Nmax     (-)   Number of rows in the coefficients data block
c I  4 Lmax     (-)   Number of columns in the coefficients data block
c I  5 N        (-)   nth row corresponding to a specific parameter value
c O  6 CORP     (-)   Correction factor for CP 

        REAL par1,par2,pol(Lmax,Nmax)
        INTEGER N,J,I,KK,K

        REAL CORP
        kK=int(Lmax**.5)
        CORP=0
        K=0
        DO 10 I=1,KK
        DO 10 J=1,KK
           K=K+1
           CORP = CORP+pol(K,N)*par1**(J-1)*par2**(I-1)
10      CONTINUE
        RETURN
        END     

        SUBROUTINE LININT (P,P1,P2,CO1,CO2,CO)
c************************************************************************
c Purpose: this routine calculates the value of a CP correction coefficient
c          by linear interpolation between two reference values.

c Module : # 4.3.1.2.3.3, TG#3, mxg/March 1,1989
c
c Pass parameters:
c IO # Name    unit         description                
c
c I  1 P       (-)   Actual value of the parameter    
c I  2 P1      (-)   The lower between two references values of the 
c                    parameter close to the actual one
c I  3 P2      (-)   The higher one
c I  4 CO1     (-)   Corr. Coeff. related to P1
c I  5 CO1     (-)   Corr. Coeff. related to P2
c O  6 CO      (-)   Corr. Coeff. related to P

        REAL P,P1,P2,CO1,CO2
        REAL CO
        CO = (P*(CO2-CO1)-P1*CO2+P2*CO1)/(P2-P1)

        RETURN
        END

        SUBROUTINE CORRAD (par,rad,Nmax,N,CORP)
c************************************************************************
c Purpose: this routine calculates fitting root-square equation f(par) 
c          by using the relevant coefficients
c
c Module : # 4.3.1.2.3.2, TG#3, mxg/March 1,1989
c Changes: this subroutine written on November 2, 1989
c Limits : 
c
c Pass parameters:
c
c IO # Name    unit         description                
c
c I  1 par      (-)   Parameter
c I  2 rad      (-)   Coefficients for root-square equation 
c I  3 Nmax     (-)   Number of rows in the coefficients data block
c I  4 N        (-)   nth row corresponding to a specific parameter value
c O  5 CORP     (-)   Correction factor for CP 

        REAL par,rad(Nmax,3)
        INTEGER N

        REAL CORP
        corp0 = rad(N,1)*par + rad(N,2)/par + rad(N,3)
        if (corp0.LT.0.) then
           corp=100.
           return
        endif
        CORP = (rad(N,1)*par + rad(N,2)/par + rad(N,3))**.5
        RETURN
        END     


        SUBROUTINE CORPANS (par,pol,Nmax,Lmax,N,CORP)
c************************************************************************
c Purpose: this routine calculates fitting polinomial equation f(par) by 
c          using the relevant coefficients
c
c Module : # 4.3.1.2.3.2, TG#3, mxg/March 1,1989
c Changes: February 7, 1990 (added Lmax)
c
c Pass parameters:
c IO # Name    unit         description                
c
c I  1 par      (-)   Parameter
c I  2 pol      (-)   Coefficients for the polinomial equation 
c I  3 Nmax     (-)   Number of rows in the coefficients data block
c I  4 Lmax     (-)   Number of columns in the coefficients data block
c I  5 N        (-)   nth row corresponding to a specific parameter value
c O  6 CORP     (-)   Correction factor for CP 

        REAL par,pol(Lmax,Nmax)
        INTEGER N,J

        REAL CORP
 
        CORP=0
        DO 10 J=1,Lmax
           CORP = CORP+pol(J,N)*par**(J-1)
10      CONTINUE
        RETURN
        END     


        SUBROUTINE CORPAN (par,pol,Nmax,Lmax,N,CORP)
c************************************************************************
c Purpose: this routine calculates fitting polinomial equation f(par) by 
c          using the relevant coefficients
c
c Module : # 4.3.1.2.3.2, TG#3, mxg/March 1,1989
c Changes: February 7, 1990 (added Lmax)
c
c Pass parameters:
c IO # Name    unit         description                
c
c I  1 par      (-)   Parameter
c I  2 pol      (-)   Coefficients for the polinomial equation 
c I  3 Nmax     (-)   Number of rows in the coefficients data block
c I  4 Lmax     (-)   Number of columns in the coefficients data block
c I  5 N        (-)   nth row corresponding to a specific parameter value
c O  6 CORP     (-)   Correction factor for CP 
c
c example:
c CALL
c***********************************************************************

        REAL par,pol(Nmax,Lmax)
        INTEGER N,J

        REAL CORP
 
        CORP=0
        DO 10 J=1,Lmax
          CORP = CORP+pol(N,J)*par**(Lmax-J)
10      CONTINUE
        RETURN
        END     


        SUBROUTINE SEARCH (A,B,Mmax,M)
c************************************************************************
c Purpose: this routine calculates the position number for an actual value 
c          of a parameter with respect to a reference data block, in case
c          of two-parameters array
c
c Module : # 4.3.1.2.3.1, TG#3, mxg/March 1,1989
c
c Pass parameters:
c
c IO # Name    unit         description                
c
c I  1 A       (-)   Reference values of the parameter    
c I  2 B       (-)   Actual value of the parameter
c I  3 Mmax    (-)   Maximum number of ref. values for the parameter
c O  4 M       (-)   Number of the ref. parameter value closest downward
c                    to the actual value

        REAL A(Mmax),B
        REAL DF
        INTEGER M
        D1=100.0
        DO 10 I=1,Mmax
          DF=A(I)-B
          IF (DF.GT.0.AND.DF.LT.D1) THEN
            D1=DF
            M=I-1
          ENDIF
10      CONTINUE

        RETURN
        END     
