C Program to perform Multiple Linear Regression

c Declare variables 
c mclm = max climates simulated for each model
c mm   = no of independant variables
      parameter (mclm=21, mm=10,  nwk=2*mclm+mm+1)
      parameter (MX=5,MI=5,MA=5,MC=2,MS=3,MO=5,ML=5)
      parameter (Mmodel=MX*MI*MA*MC*MS*MO*ML)
      dimension Xin(mclm,mm), Yin(mclm), a(mm), wk(mm,nwk)
      dimension clmname(mclm),iclmlen(mclm),csvfilename(mclm)
      dimension clmdata(mclm,12,mm)
      dimension modheat(Mmodel,12,mclm),modcool(Mmodel,12,mclm)
      character fname*40,ASDF*1
      character*80 clmname,tmp,tmp1,tmp2,csvfilename
      real modheat,modcool

C Provide user with some information
      print *,'Program to run MLR for the SAPP project'
      print *,' '
      print *,'Before proceeding please ensure that this folder '
      print *,'contains the following '
      print *,'One clm.summary.data.a file'
      print *,'One or more RRDB.[climatename].csv files'
      print *,' '
      print *,'There should be no file with the name '
      print *,'FRDB.csv present in the results folder'
      print *,'If present delete it before continuing'
      print *,' '
      print *,'Press any key and then return to continue '
      read *,ASDF 
      print *,'wats up doc?'

C Init
      l=1    ! Number of dependent variables Y
      m=10   ! Number of independent variables X(i)
      n=mclm ! Number of lines to read per model per month 
             ! (climates simulated)

C Read climate summay file
      print *,'reading ../results/clm.summary.data.a'
      fname='../results/clm.summary.data.a'
      open(1,file=fname,status='old')

C Expect to find one string (climate name) followed by 
C 12 lines of mcoeff records each (independant variables)
      do 100 i=1,mclm
       clmname(i)=' '
       read(1,*,end=100)clmname(i)
       do 101 j=1,12
         read(1,*,end=101)(clmdata(i,j,k),k=1,mm-1)
 101   continue
 100  continue
      close(1)

C Read csv files
C For each climate 
      do 200 icl=1,mclm
        tmp=clmname(icl)
        call LENTRIM(tmp,40,iln)
        iclmlen(icl)=iln-8
        clmname(icl)=tmp(9:iln)
C        csvfilename(icl)='RRDB.'//tmp(9:iln)//'.csv'
        csvfilename(icl)=
     &  '../tmp/energy_res/RRDB.uk_SAPP_'//tmp(1:iln)//'.csv'
        open(2,file=csvfilename(icl),status='old')

C Now get model number (line number) from RRDB.csv files
        iline=0
        do 201 ix=1,mx
        do 201 ii=1,mi
        do 201 ia=1,ma
        do 201 ic=1,mc
        do 201 is=1,ms
        do 201 io=1,mo
        do 201 il=1,ml
          iline=iline+1
          read(2,*,end=201)tmp1,tmp2,
     &    (modheat(iline,imnth,icl),imnth=1,12),
     &    (modcool(iline,imnth,icl),imnth=1,12)
 201    continue
        close(2)
        print *,'read climate file no. ',icl,' ',csvfilename(icl)
 200  continue
      print *,'read all the files, performing MLR ...'

C Now write out data in a form suitable for MLR
      open(3,file='../results/FRDB.csv',status='new')
      iline=0
      do 300 ix=1,mx
      do 300 ii=1,mi
      do 300 ia=1,ma
      do 300 ic=1,mc
      do 300 is=1,ms
      do 300 io=1,mo
      do 300 il=1,ml
        iline=iline+1
        do 301 imnth=1,12
          do 302 icl=1,mclm

C Set MLR variables
            Xin(icl,10)=1.
            do 304 icoeff=1,9
              Xin(icl,icoeff)=clmdata(icl,imnth,icoeff)
 304      continue
            Yin(icl)=modheat(iline,imnth,icl)
 302      continue
 
c Find the regression coefficients and R-squared value
          call NORMAL(Xin,Yin,n,m,l,mclm,mm,a,wk)
          call calcod(Xin,Yin,mclm,mm,a,R2,n)

C Print results
          if(imnth.eq.1)then
            write(3,660)'X',ix,'I',ii,'A',ia,'C',ic,'S',is,'O',io,
     &      'L',il,',month0',imnth,(a(icf),icf=1,m),R2
          elseif(imnth.lt.10)then
            write(3,660)'X',ix,'I',ii,'A',ia,'C',ic,'S',is,'O',io,
     &      'L',il,',month0',imnth,(a(icf),icf=1,m),R2
c            write(3,661)'              ',
c     &      'month0',imnth,(a(icf),icf=1,m),R2
          else
            write(3,663)'X',ix,'I',ii,'A',ia,'C',ic,'S',is,'O',io,
     &      'L',il,',month',imnth,(a(icf),icf=1,m),R2
c            write(3,662)'              ',
c     &      'month',imnth,(a(icf),icf=1,m),R2
          endif
 301    continue
 300  continue
      close(3)

C Debug
c      print *,Xin
c      print *,'Yin '
c      print *,Yin
c Print the coefficients
c      print *, 'The coefficients are: '
c      do i=1, m
c      print 650, ' a(', i, ')= ', a(i)
c      enddo
c      print *,' R squared = ',R2
c
650   format(a, i1, a, 1p10e14.6)

C To write XIACSOL on every line
 660  format(a,i1,a,i1,a,i1,a,i1,a,i1,a,i1,a,i1,a,i1,',',11
     &(e11.5,','),e11.5)
 663  format(a,i1,a,i1,a,i1,a,i1,a,i1,a,i1,a,i1,a,i2,',',11
     &(e11.5,','),e11.5)

C To write XIACSOL once for a 12 month period
 661  format(2a,i1,',',11(e11.5,','),e11.5)
 662  format(2a,i2,',',11(e11.5,','),e11.5)
      end

c-----------------------------------------------------------------------
      subroutine NORMAL(X,Y,n,m,l,nn,mm,a,wk)
c-----------------------------------------------------------------------
c Calculate the regression coefficient based on the normal equation:
c    a=(XT*X)^(-1)*XT*Y
c where
c    X(n,m) ... independent variable matrix (input)
c    Y(n,l) ... dependent variable matrix (input)
c    XT(m,n) ... transpose of X
c    l ... number of dependent variables
c    m ... number of independent variables
c    n ... number of samples
c    nn ... the leading dimension of X & Y as declared in the calling program
c    mm ... the leading dimension of a & wk declared in the calling program
c  Work vector wk should have at least 2*n+m+1 columns
c-----------------------------------------------------------------------
c Programming Note: Use GAUSSJ of Numerical Recipes to invert a square matrix XT*X
c-----------------------------------------------------------------------
c Link this+GAUSSJ;
c-----------------------------------------------------------------------

c Declare variables ----------------------------------------------------
      dimension X(nn,1), Y(nn,1), a(mm,1), wk(mm,1)

      j1=n+1
      j2=n+m+1
      jb=n+m+n+1

c wk <-- Xt
      call transp(wk,X,n,m,nn,mm)
c wk(1,j1) <-- Xt*X
      call mulcab(wk(1,j1),wk,X,m,n,m,mm,nn,mm)
c wk(1,j1) <-- (Xt*X)^(-1)
      call GAUSSJ(wk(1,j1), m, mm, wk(1,jb), 1, 1)
c wk(1,j2) <-- (Xt*X)^(-1)*Xt
      call mulcab(wk(1,j2),wk(1,j1),wk,m,m,n,mm,mm,mm)
c a <-- (Xt*X)^(-1)*Xt*Y
      call mulcab(a,wk(1,j2),Y,m,n,l,mm,nn,mm)

      end


c-----------------------------------------------------------------------
      subroutine transp(At,A,m,n,ldA,ldAt)
c-----------------------------------------------------------------------
c Transpose of matrix A(m,n) => At(n,m)
c mm=exact row dimension of A as specified in the dimension statement
c    in the calling program.
c mclm=exact row dimension of At as specified in the dimension statement
c    in the calling program.
c-----------------------------------------------------------------------
      dimension A(ldA,1), At(ldAt,1)
      do i=1, m
      do j=1, n
        At(j,i) = A(i,j)
      enddo
      enddo
      end


c-----------------------------------------------------------------------
      subroutine mulcab(C,A,B,m,l,n,ldA,ldB,ldC)
c-----------------------------------------------------------------------
c Multiply the two matrices together -----------------------------------
c     C(m,n) = A(m,l)*B(l,n).
c lda,ldb,ldc=exact row dimensions (Leading Dimension) of A, B, and C,
c    respectively, as specified in the dimension statement in the calling
c    program.
c-----------------------------------------------------------------------
      dimension A(ldA,1), B(ldB,1), C(ldC,1)

      do i=1, m
      do j=1, n
        C(i,j)=0.
        do k=1, l
          C(i,j)=C(i,j)+A(i,k)*B(k,j)
        enddo
      enddo
      enddo

      end


c***********************************************************************
      SUBROUTINE gaussj(a,n,np,b,m,mp)
c***********************************************************************
      INTEGER m,mp,n,np,NMAX
      REAL a(np,np),b(np,mp)
      PARAMETER (NMAX=50)
c-----------------------------------------------------------------------
c Linear equation solution by Gauss-Jordan elimination, equation (2.1.1) above. a(1:n,1:n)
c is an input matrix stored in an array of physical dimensions np by np. b(1:n,1:m) is an input
c matrix containing the m right-hand side vectors, stored in an array of physical dimensions
c np by mp. On output, a(1:n,1:n) is replaced by its matrix inverse, and b(1:n,1:m) is
c replaced by the corresponding set of solution vectors.
c Parameter: NMAX is the largest anticipated value of n.
c-----------------------------------------------------------------------
      INTEGER i,icol,irow,j,k,l,ll,indxc(NMAX),indxr(NMAX),ipiv(NMAX)
c-----------------------------------------------------------------------
c The integer arrays ipiv, indxr, and indxc are used for
c bookkeeping on the pivoting.
c-----------------------------------------------------------------------
      REAL big,dum,pivinv 
      do 11 j=1,n
        ipiv(j)=0
 11   enddo
      do 22 i=1,n ! This is the main loop over the columns to be reduced
        big=0.
        do 13 j=1,n ! This is the outer loop of the search for a pivot element
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                stop 'singular matrix in gaussj'
              endif
 12         enddo
          endif
 13     enddo   
        ipiv(icol)=ipiv(icol)+1
c-----------------------------------------------------------------------
c We now have the pivot element, so we interchange rows, if needed, to put the pivot
c element on the diagonal. The columns are not physically interchanged, only relabeled
c indxc(i), the column of the ith pivot element, is the ith column that is reduced, while
c indxr(i) is the row in which that pivot element was originally located. If indxr(i) !=
c indxc(i) there is an implied column interchange. With this form of bookkeeping, the
c solution b's will end up in the correct order, and the inverse matrix will be scrambled
c by columns.
c-----------------------------------------------------------------------
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
 14       enddo   
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
 15       enddo   
        endif
c-----------------------------------------------------------------------
c We are now ready to divide the pivot row by the pivot
c element, located at irow and icol.
c-----------------------------------------------------------------------
        indxr(i)=irow 
        indxc(i)=icol
        if (a(icol,icol).eq.0.) stop 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
 16     enddo   
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
 17     enddo   
c-----------------------------------------------------------------------
c Next, we reduce the rows......except for the pivot one, of course.
c-----------------------------------------------------------------------
        do 21 ll=1,n       
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
 18         enddo   
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
 19         enddo   
          endif
 21     enddo
 22   enddo
c-----------------------------------------------------------------------
c This is the end of the main loop over columns of the reduction.
c It only remains to unscramble the solution in view
c of the column interchanges. We do this by interchanging
c pairs of columns in the reverse order that the permutation was built up.
c-----------------------------------------------------------------------
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
 23       enddo   
        endif
 24   enddo   
      return 
      END

C ******************** CALCOD ***********************
C Subroutine to determine the coefficient of determination R-squared
C given observed values of Y and values of Y predicted from MLR
C It uses the following variables
C SS_total = sum of squares of difference b/w Y and mean Y
C SS_regression = sum of squares of difference b/w Y predicted from MLR 
C                 and mean Y
C SS_error pr RSS = sum of squares of difference b/w Y and Y predicted from MLR 
C                 and mean Y
      subroutine calcod(Xout,Yout,jj,kk,ae,coef,no)

c Declare variables
      dimension Xout(jj,kk), Yout(jj),ae(kk)

C Initialise
      Ybar=0.0
      SS_total=0.0
      RSS=0.0
      YMLR=0.0

C Calculate Ybar (mean observed Y)
      do 10 n=1,no
        Ybar=Ybar+Yout(n)
  10  continue
      Ybar=Ybar/no
      
C Calculate deviation from mean 
      do 20 n=1,no
c       SS_total=SS_total+(Ybar-Yout(n))**2
        SS_total=SS_total+(Yout(n))**2
 20   continue
      
C Calculate error
      do 30 n=1,no

C First calculate predicted Y
        YMLR=0.0
        do 40 m=1,kk
          YMLR=YMLR+ae(m)*Xout(n,m)
 40     continue
        RSS=RSS+(YMLR-Yout(n))**2
 30   continue
      
C Calculate R squared value
      coef=(SS_total-RSS)/SS_total
      return
      end

C ******************** LENTRIM ***********************
C Subroutine to return the length of a string
      subroutine LENTRIM(str,mlen,ilen)
      character*80 str
      ilen=0
      do i=40,1,-1
        if(str(i:i).eq.char(32))ilen=ilen-1
      enddo
      ilen=40+ilen
      return
      end






