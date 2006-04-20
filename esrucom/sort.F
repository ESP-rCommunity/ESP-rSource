C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C SORT.F contains two routines at present:
C   SORTI - sort one lists of integers into ascending or descending 
C           order, with the option to carry another array.
C   SORTR - sort one lists of reals into ascending or descending 
C           order, with the option to carry another array.
C   INTFREQB - take integer array IX and return a frequency
C           bin in integer array IY.

C ******************* SORTI ***********************
C Sort an array and optionally make the same interchanges in
C an auxiliary array.  The array may be sorted in increasing
C or decreasing order.  A slightly modified QUICKSORT
C algorithm is used.
C    IX - integer array of values to be sorted
C    IY - integer array to be (optionally) carried along
C    N  - number of values in integer array IX to be sorted
C    KFLAG - control parameter
C         = 2  means sort IX in increasing order and carry IY along.
C         = 1  means sort IX in increasing order (ignoring IY)
C         =-1  means sort IX in decreasing order (ignoring IY)
C         =-2  means sort IX in decreasing order and carry IY along.

      SUBROUTINE SORTI(IX,IY,N,KFLAG)

      INTEGER T, TT, TTY, TY
      DIMENSION IX(N), IY(N), IL(21), IU(21)

      NN=N
      if (NN.le.1) RETURN
      KK=ABS(KFLAG)
      if (KK.NE.1.AND.KK.NE.2) RETURN

C Alter array IX to get decreasing order if needed.
      if (KFLAG.LE.-1) then
         do 10 I=1,NN
            IX(I)=-IX(I)
   10    continue
      endif
      if (KK.EQ.2) GO TO 100

C Sort IX only.
      M=1
      I=1
      J=NN
      R=0.375E0

   20 if (I.EQ.J) GOTO 60
      if (R.LE.0.5898437E0) then
         R=R+3.90625E-2
      else
         R=R-0.21875E0
      endif

   30 K=I

C Select a central element of the array and save it in location T.
      IJ=I + INT((J-I)*R)
      T=IX(IJ)

C If first element of array is greater than T, interchange with T.
      if (IX(I) .GT. T) then
         IX(IJ)=IX(I)
         IX(I)=T
         T=IX(IJ)
      endif
      L=J

C If last element of array is less than than T, interchange with T.
      if (IX(J) .LT. T) then
         IX(IJ)=IX(J)
         IX(J)=T
         T=IX(IJ)

C If first element of array is greater than T, interchange with T.
         if (IX(I) .GT. T) then
            IX(IJ)=IX(I)
            IX(I)=T
            T=IX(IJ)
         endif
      endif

C Find an element in the second half of the array which is smaller
C than T.
   40 L=L-1
      if (IX(L) .GT. T) GO TO 40

C Find an element in the first half of the array which is greater
C than T.
   50 K=K+1
      if (IX(K) .LT. T) GO TO 50

C Interchange these elements.
      if (K.le.L) then
         TT=IX(L)
         IX(L)=IX(K)
         IX(K)=TT
         GO TO 40
      endif

C Save upper and lower subscripts of the array yet to be sorted.
      if (L-I .GT. J-K) then
         IL(M)=I
         IU(M)=L
         I=K
         M=M+1
      ELSE
         IL(M)=K
         IU(M)=J
         J=L
         M=M+1
      endif
      GO TO 70

C Begin again on another portion of the unsorted array.
   60 M=M-1
      if (M .EQ. 0) GO TO 190
      I=IL(M)
      J=IU(M)

   70 if (J-I .GE. 1) GO TO 30
      if (I .EQ. 1) GO TO 20
      I=I-1

   80 I=I+1
      if (I .EQ. J) GO TO 60
      T=IX(I+1)
      if (IX(I).le.T) GO TO 80
      K=I

   90 IX(K+1)=IX(K)
      K=K-1
      if (T .LT. IX(K)) GO TO 90
      IX(K+1)=T
      GO TO 80

C Sort IX and carry IY along.
  100 M=1
      I=1
      J=NN
      R=0.375E0

  110 if (I .EQ. J) GO TO 150
      if (R.le.0.5898437E0) then
         R=R+3.90625E-2
      else
         R=R-0.21875E0
      endif
  120 K=I

C Select a central element of the array and save it in location T.
      IJ=I + INT((J-I)*R)
      T=IX(IJ)
      TY=IY(IJ)

C If first element of array is greater than T, interchange with T.
      if (IX(I) .GT. T) then
         IX(IJ)=IX(I)
         IX(I)=T
         T=IX(IJ)
         IY(IJ)=IY(I)
         IY(I)=TY
         TY=IY(IJ)
      endif
      L=J

C If last element of array is less than T, interchange with T.
      if (IX(J) .LT. T) then
         IX(IJ)=IX(J)
         IX(J)=T
         T=IX(IJ)
         IY(IJ)=IY(J)
         IY(J)=TY
         TY=IY(IJ)

C If first element of array is greater than T, interchange with T.
         if (IX(I) .GT. T) then
            IX(IJ)=IX(I)
            IX(I)=T
            T=IX(IJ)
            IY(IJ)=IY(I)
            IY(I)=TY
            TY=IY(IJ)
         endif
      endif

C Find an element in the second half of the array which is smaller
C than T.
  130 L=L-1
      if (IX(L) .GT. T) GO TO 130

C Find an element in the first half of the array which is greater
C than T.
  140 K=K+1
      if (IX(K) .LT. T) GO TO 140

C Interchange these elements.
      if (K.le.L) then
         TT=IX(L)
         IX(L)=IX(K)
         IX(K)=TT
         TTY=IY(L)
         IY(L)=IY(K)
         IY(K)=TTY
         GO TO 130
      endif

C Save upper and lower subscripts of the array yet to be sorted.
      if (L-I .GT. J-K) then
         IL(M)=I
         IU(M)=L
         I=K
         M=M+1
      ELSE
         IL(M)=K
         IU(M)=J
         J=L
         M=M+1
      endif
      GO TO 160

C Begin again on another portion of the unsorted array.
  150 M=M-1
      if (M .EQ. 0) GO TO 190
      I=IL(M)
      J=IU(M)

  160 if (J-I .GE. 1) GO TO 120
      if (I .EQ. 1) GO TO 110
      I=I-1

  170 I=I+1
      if (I .EQ. J) GO TO 150
      T=IX(I+1)
      TY=IY(I+1)
      if (IX(I).le.T) GO TO 170
      K=I

  180 IX(K+1)=IX(K)
      IY(K+1)=IY(K)
      K=K-1
      if (T .LT. IX(K)) GO TO 180
      IX(K+1)=T
      IY(K+1)=TY
      GO TO 170

C Clean up.
  190 if (KFLAG.le.-1) then
         DO 200 I=1,NN
            IX(I)=-IX(I)
  200    CONTINUE
      endif
      RETURN
      END

C ******************* SORTR ***********************
C Sort an array and optionally make the same interchanges in
C an auxiliary array.  The array may be sorted in increasing
C or decreasing order.  A slightly modified QUICKSORT
C algorithm is used.
C    X - array of values to be sorted
C    Y - array to be (optionally) carried along
C    N - number of values in array X to be sorted
C    KFLAG - control parameter
C         = 2  means sort X in increasing order and carry Y along.
C         = 1  means sort X in increasing order (ignoring Y)
C         =-1  means sort X in decreasing order (ignoring Y)
C         =-2  means sort X in decreasing order and carry Y along.

      SUBROUTINE SORTR(X,Y,N,KFLAG)

      DIMENSION X(N), Y(N), IL(21), IU(21)

      NN=N
      if (NN.lt.1) RETURN
      KK=ABS(KFLAG)
      if (KK.NE.1.AND.KK.NE.2) RETURN

C Alter array X to get decreasing order if needed.
      if (KFLAG.le.-1) then
         DO 10 I=1,NN
            X(I)=-X(I)
   10    CONTINUE
      endif
      if (KK .EQ. 2) GO TO 100

C Sort X only.
      M=1
      I=1
      J=NN
      R=0.375E0

   20 if (I .EQ. J) GO TO 60
      if (R.le.0.5898437E0) then
         R=R+3.90625E-2
      ELSE
         R=R-0.21875E0
      endif

   30 K=I

C Select a central element of the array and save it in location T.
      IJ=I + INT((J-I)*R)
      T=X(IJ)

C If first element of array is greater than T, interchange with T.
      if (X(I) .GT. T) then
         X(IJ)=X(I)
         X(I)=T
         T=X(IJ)
      endif
      L=J

C If last element of array is less than than T, interchange with T.
      if (X(J) .LT. T) then
         X(IJ)=X(J)
         X(J)=T
         T=X(IJ)

C If first element of array is greater than T, interchange with T.
         if (X(I) .GT. T) then
            X(IJ)=X(I)
            X(I)=T
            T=X(IJ)
         endif
      endif

C Find an element in the second half of the array which is smaller
C than T.
   40 L=L-1
      if (X(L) .GT. T) GO TO 40

C Find an element in the first half of the array which is greater
C than T.
   50 K=K+1
      if (X(K) .LT. T) GO TO 50

C Interchange these elements.
      if (K.le.L) then
         TT=X(L)
         X(L)=X(K)
         X(K)=TT
         GO TO 40
      endif

C Save upper and lower subscripts of the array yet to be sorted.
      if (L-I .GT. J-K) then
         IL(M)=I
         IU(M)=L
         I=K
         M=M+1
      ELSE
         IL(M)=K
         IU(M)=J
         J=L
         M=M+1
      endif
      GO TO 70

C Begin again on another portion of the unsorted array.
   60 M=M-1
      if (M .EQ. 0) GO TO 190
      I=IL(M)
      J=IU(M)

   70 if (J-I .GE. 1) GO TO 30
      if (I .EQ. 1) GO TO 20
      I=I-1

   80 I=I+1
      if (I .EQ. J) GO TO 60
      T=X(I+1)
      if (X(I).le.T) GO TO 80
      K=I

   90 X(K+1)=X(K)
      K=K-1
      if (T .LT. X(K)) GO TO 90
      X(K+1)=T
      GO TO 80

C Sort X and carry Y along.
  100 M=1
      I=1
      J=NN
      R=0.375E0

  110 if (I .EQ. J) GO TO 150
      if (R.le.0.5898437E0) then
         R=R+3.90625E-2
      ELSE
         R=R-0.21875E0
      endif

  120 K=I

C Select a central element of the array and save it in location T.
      IJ=I + INT((J-I)*R)
      T=X(IJ)
      TY=Y(IJ)

C If first element of array is greater than T, interchange with T.
      if (X(I) .GT. T) then
         X(IJ)=X(I)
         X(I)=T
         T=X(IJ)
         Y(IJ)=Y(I)
         Y(I)=TY
         TY=Y(IJ)
      endif
      L=J

C If last element of array is less than T, interchange with T.
      if (X(J) .LT. T) then
         X(IJ)=X(J)
         X(J)=T
         T=X(IJ)
         Y(IJ)=Y(J)
         Y(J)=TY
         TY=Y(IJ)

C If first element of array is greater than T, interchange with T.
         if (X(I) .GT. T) then
            X(IJ)=X(I)
            X(I)=T
            T=X(IJ)
            Y(IJ)=Y(I)
            Y(I)=TY
            TY=Y(IJ)
         endif
      endif

C Find an element in the second half of the array which is smaller
C than T.
  130 L=L-1
      if (X(L) .GT. T) GO TO 130

C Find an element in the first half of the array which is greater
C than T.
  140 K=K+1
      if (X(K) .LT. T) GO TO 140

C Interchange these elements.
      if (K.le.L) then
         TT=X(L)
         X(L)=X(K)
         X(K)=TT
         TTY=Y(L)
         Y(L)=Y(K)
         Y(K)=TTY
         GO TO 130
      endif

C Save upper and lower subscripts of the array yet to be sorted.
      if (L-I .GT. J-K) then
         IL(M)=I
         IU(M)=L
         I=K
         M=M+1
      ELSE
         IL(M)=K
         IU(M)=J
         J=L
         M=M+1
      endif
      GO TO 160

C Begin again on another portion of the unsorted array.
  150 M=M-1
      if (M .EQ. 0) GO TO 190
      I=IL(M)
      J=IU(M)

  160 if (J-I .GE. 1) GO TO 120
      if (I .EQ. 1) GO TO 110
      I=I-1

  170 I=I+1
      if (I .EQ. J) GO TO 150
      T=X(I+1)
      TY=Y(I+1)
      if (X(I).le.T) GO TO 170
      K=I

  180 X(K+1)=X(K)
      Y(K+1)=Y(K)
      K=K-1
      if (T .LT. X(K)) GO TO 180
      X(K+1)=T
      Y(K+1)=TY
      GO TO 170

C Clean up.
  190 if (KFLAG.le.-1) then
         DO 200 I=1,NN
            X(I)=-X(I)
  200    CONTINUE
      endif
      RETURN
      END

C ****************** INTFREQB **********************
C Intfreqb takes an integer array IX of size NX and returns
C a frequency bin in integer arrays INBIN & ICBIN of size NY
C with NB the number of bins found and IMIN and IMAX the bounds.
C ifreq is the maximum number of items in a bin
C ifreqbin is the bin where the maximum number of items
C ibinval is the scalar value at the maximum nb of items bin
      subroutine intfreqb(itrc,itru,IX,NX,INBIN,ICBIN,NY,NB,IAMIN,
     &  IAMAX,IFREQ,IFREQBIN,IBINVAL)
      DIMENSION ix(nx),INBIN(ny),ICBIN(ny)

      character outs*124,tab*1,tranged*10,trange*10

C First recover bounds of array
C << alter commented out line to exlude any zero items.
      tab=','
      IAMAX= -10000
      IAMIN=  10000
      do 40 I=1,NX
        IAMAX=MAX0(IX(I),IAMAX)
C to exlude zeros.        if(ix(i).ne.0)IAMIN=MIN0(IX(I),IAMIN)
        IAMIN=MIN0(IX(I),IAMIN)
 40   continue
      write(outs,'(a,i8,a,i8)')'The data range is',IAMIN,' to',IAMAX
      if(itrc.gt.0)call edisp(itru,outs)

C Reset number of bins and size of typcial bin.
      IDXBIN=1
      NBINS=IAMAX-IAMIN
      IBIN1=IAMIN
      IBINN=IAMAX
      if(NBINS.lt.NY)then
        continue
      else
        IDXBIN=2
        NBINS=(IAMAX-IAMIN)/2
        if(NBINS.lt.NY)then
          continue
        else
          IDXBIN=3
          NBINS=(IAMAX-IAMIN)/3
          if(NBINS.lt.NY)then
            continue
          else
            IDXBIN=4
            NBINS=(IAMAX-IAMIN)/4
            if(NBINS.lt.NY)then
              continue
            else
              IDXBIN=5
              NBINS=(IAMAX-IAMIN)/5
              if(NBINS.lt.NY)then
                continue
              else
                IDXBIN=6
                NBINS=(IAMAX-IAMIN)/6
                if(NBINS.lt.NY)then
                  continue
                else
                  IDXBIN=7
                  NBINS=(IAMAX-IAMIN)/7
                  if(NBINS.lt.NY)then
                    continue
                  else
                    IDXBIN=8
                    NBINS=(IAMAX-IAMIN)/8
                    if(NBINS.lt.NY)then
                      continue
                    else
                      IDXBIN=9
                      NBINS=(IAMAX-IAMIN)/9
                      if(NBINS.lt.NY)then
                        continue
                      else
                        IDXBIN=10
                        NBINS=(IAMAX-IAMIN)/10
                        if(NBINS.lt.NY)then
                          continue
                        else
                          IDXBIN=11
                          NBINS=(IAMAX-IAMIN)/11
                          if(NBINS.lt.NY)then
                            continue
                          else
                            IDXBIN=12
                            NBINS=(IAMAX-IAMIN)/12
                          endif
                        endif
                      endif
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif
      endif
      NBINS=NBINS+1

C Empty bins of hits.
      DO 510 I=1,NBINS
        INBIN(I) = 0
        ICBIN(I) = 0
  510 CONTINUE
      IHITS = 0
      IMISS = 0
      iover = 0
      iunder = 0
      do 520 I=1,NX
        IB = INT(((IX(I) - IBIN1) / IDXBIN) + 1)
C Debug...
C        write(6,*) 'ib ix ibin1 idxbin ',ib,ix(i)-ibin1,idxbin
        if(IB.LE.0)then
          iunder = iunder +1
          IMISS = IMISS + 1
        elseif(IB.GT.NBINS)then
          iover = iover +1
          IMISS = IMISS + 1
        else
          INBIN(IB) = INBIN(IB) + 1
          IHITS = IHITS + 1
        endif
  520 continue

C Save largest number of hits in a bin (IFREQ) and which bin
C it was in (IFREQBIN).
      IFREQ=0
      IFREQBIN=0
      IXBIN=IBIN1
      DO 700 I=1,NBINS
        if(INBIN(I).gt.IFREQ)then
          IFREQ=INBIN(I)
          IFREQBIN=I
          IBINVAL=IXBIN
        endif
        IXBIN=IBIN1+IDXBIN
  700 CONTINUE

C Calculate cumulative frequency.
      ITOTAL=0
      DO 710 I=1,NBINS
        IF(I.EQ.1)ICBIN(I)=INBIN(I)
        IF(I.GT.1)ICBIN(I)=ICBIN(I-1)+INBIN(I)
        ITOTAL=ITOTAL+INBIN(I)
  710 CONTINUE

      if(itrc.gt.0)then
        call edisp(itru,
     &' Bin  data    Distri-  freq.   cumulative  cumulative')
        call edisp(itru,
     &'      range   bution    (%)   distribution  freq (%)')
      endif

C Provide initial range and then loop through each.
      IXBINS=IBIN1
      IXBIN=IBIN1+IDXBIN
      DO 101 I=1,NBINS
        if(idxbin.eq.1)then
          write(trange,'(I3,a)') IXBINS,'-'
        else
          write(trange,'(I3,a,I3)') IXBINS,'-',IXBIN
        endif
        call SDELIM(trange,tranged,'N',IW)
        IXBINS=IXBIN
        IXBIN=IXBIN+IDXBIN
        CFREQ=(FLOAT(ICBIN(I))/FLOAT(ITOTAL))*100.0
        FREQ=(FLOAT(INBIN(I))/FLOAT(ITOTAL))*100.0
        if(I.eq.1)then
          WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &      tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iunder,' below'
        elseif(I.eq.NBINS)then
          WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2,a,i6,a)') I,
     &      tranged,INBIN(I),FREQ,ICBIN(I),CFREQ,'  ',iover,' above'
        else
          WRITE(outs,'(i3,2x,a,i5,2x,f6.2,i9,f12.2)')I,tranged,
     &      INBIN(I),FREQ,ICBIN(I),CFREQ
        endif
        if(itrc.gt.0)call edisp(itru,outs)
  101 CONTINUE

      return
      end
