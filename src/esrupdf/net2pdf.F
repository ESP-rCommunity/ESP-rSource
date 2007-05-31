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

C The net2pdf subroutine reads in net connections and components
C files and converts them to a plant network file.

      SUBROUTINE NET2PNF

#include "plant.h"

      common /pcddsc/ pcdesc(maxpc), npref(mpcom)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common/pcnam/pcname(mpcom)
      COMMON/CONET/IPCOMPA,IPCOMPB
      common/pophelp/h(60)
      DIMENSION ival(mpcom)
      LOGICAL OK,dok

      CHARACTER H*72,TYPE*15,MRK*7,txt1*40
      CHARACTER*25 NDATA1,NDATA2,CNAME1,CNAME2,DUMNAM1,DUMNAM2,DUMNAM3
      character pcdesc*80,pcname*15,icnf*74,conf*74, outs*124
      character cvrdsc*68,mscdsc*68,txt*68 

C Open the net file containing the components description.
C The file is called 'save.icn' by net. This can be changed to
C a user defined name later.
      ICNUNIT=14
      ICONUNIT=15

      icnf='save.icn'
      conf='save.con'

C Read the components data from file.
      h(1)='The icon data file << ... >> '
11    CALL EASKS(icnf,'icon data file name ? ',' ',74,'save.icn',
     &  'con file',IER,1)
      CALL EFOPSEQ(ICNUNIT,icnf,1,IER)
      IF(IER.LT.0) THEN
        dok=.false.
        h(1)='The file name given was not found. Check the '
        h(2)='file name and folder. '
        CALL ASKOK('File does not exist','try again ? ',ok,dok,2) 
        IF(OK) THEN  
          GOTO 11
        ELSE
          RETURN  
        ENDIF
      ENDIF
      READ(ICNUNIT,*) NICON

C Initialise the number of 'valid components'
      IPCOMP=0

C Read in the save.icn file created by net
      DO 20 I= 1,NICON
        READ(ICNUNIT,*) TYPE
C Check that the component is not a 'hidden' one.
        IF(TYPE.NE.'XXX') THEN
          IPCOMP=IPCOMP+1
          READ(ICNUNIT,*) IBMAP,ICONUM,IBMNO,IPOSX,IPOSY
          READ(ICNUNIT,*) IDATA,NDATA1,PCNAME(IPCOMP),NDATA2,IDBREF,MRK
        ELSE
          READ(ICNUNIT,*)  IDUM1,IDUM2,IDUM3,IDUM4,IDUM5
          READ(ICNUNIT,*)  IDUM6,DUMNAM1,DUMNAM2,DUMNAM3,IDUM9,MRK
          GOTO 20
        ENDIF

C Specify the plant component database entry for the component.
        NPREF(IPCOMP)=IDBREF

C Call subroutine to read data associated with selected
C component from plant data base.
        call rcdata(IPCOMP,1)
        if(nci(IPCOMP).gt.0) then

C If component has any control variable(s),
C prompt user to enter its value.
          do 101 jj=1, nci(IPCOMP)
            txt=cvrdsc(jj)
            write(txt1,'(2a)') ' For component: ',pcname(ipcomp)
            value=0.0
            h(1)=
     &         'A control variable may be subjected to control action.'
            h(2)='For example, in a heating coil model, the heat'
            h(3)='injected to air may be an elected control variable.'
            h(4)=
     &         'If no control loop is defined, a realistic value must'
            h(5)='be assigned to this variable otherwise its value is'
            h(6)='determined on the basis of some sensed condition and'
            h(7)='the active control law.'
            h(8)=' '
            CALL EASKR(value,txt1,txt,0.,'-',0.,'-',0.,
     &           ' item value?',IER,9)
            cdata(IPCOMP,jj)=value
  101     continue
        endif
        nmisc=npi(IPCOMP)

        if(nmisc.gt.0) then
          write(txt1,'(2a)') ' For component: ',pcname(ipcomp)

C If component has miscellaneous data,
C ask user if he wishes to modify any parameters.
          dok=.false.
          h(1)='Chance to edit component parameters. '
          CALL ASKOK( txt1,
     &      'Do you want to change component parameters?',OK,dok,1)
          if(.not.OK) goto 20

C Display miscellaneous data items and let user pick one.
  105     call askpar(itm,ival,I)
          if(itm.ne.0) then

C Read new value for selected item(s).
            do 106 iij = 1,itm
              txt=mscdsc(ival(iij))
              dvalue=tadata(IPCOMP,ival(iij))
              value=dvalue
              call easkr(value, txt1,txt,0.,'-',0.,'-',dvalue,
     &          ' item value?',IER,0)
              tadata(IPCOMP,ival(iij))=value
 106        continue
            write(outs,'(2a)') 'Updated data for ',pcname(ipcomp)
            call edisp(iuout,outs)
            do 910 jj=1,npi(ipcomp)
              write(outs,'(a50,a3,g12.5)')
     &          mscdsc(jj),' : ',tadata(ipcomp,jj)
              call edisp(iuout,outs)
  910       continue
            goto 105
          endif
        endif
  20  CONTINUE

      NPCOMP=IPCOMP


C Read the connections data from file.
      h(1)='An icon connections file << ... >> '
12    CALL EASKS(conf,'icon connections file ?',' ',74,' ','con file',
     &IER,1)
      CALL EFOPSEQ(ICONUNIT,conf,1,IER)
      IF(IER.LT.0) THEN
        dok=.true.
        h(1)='the file name given was not found. This is a chance'
        h(2)='to check and revise the file name. '
        CALL ASKOK('File does not exist','try again ? ',ok,dok,2) 
        IF(OK) THEN  
          GOTO 12
        ELSE
          RETURN  
       ENDIF
      ENDIF

      READ(ICONUNIT,*) NCN
      IF(NCN.GT.0) THEN
        DO 30 J=1,NCN
          READ(ICONUNIT,*) CNAME1,IC2,ICX1,ICY1,CNAME2,IC1,
     &    ICX2,ICY2,ICNUM

          DO 40 K=1,NPCOMP
            IF (PCNAME(K)(1:15).EQ.CNAME1(1:15)) THEN
               IPC2(J)=K 
               IPCOMPB=K     
            ELSEIF(PCNAME(K)(1:15).EQ.CNAME2(1:15)) THEN
               IPC1(J)=K 
               IPCOMPA=K 
            ENDIF
  40    CONTINUE

C  Fill in the missing data for the connection
        write(outs,'(a,i3)') 'Please supply some additional information'
        call edisp(iuout,outs)
        write(outs,'(a,i3)') 'for plant network connection No. ',j
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        call edisp(iuout,'Current data: ')    
        write(outs,'(a)')
     &'Component     |connects to | Component    '
        call edisp(iuout,outs)
        write(outs,'(a15,a12,a15)') 
     & PCNAME(IPC2(J)),'  ------>   ',PCNAME(IPC1(J))
        call edisp(iuout,outs)  
        CALL CONDEF('G',99)
        call edisp(iuout,' ')

 30     CONTINUE
      ENDIF

      RETURN
      END
