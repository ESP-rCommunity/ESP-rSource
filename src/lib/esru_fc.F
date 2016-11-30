C Fortran dummies for stuff in C code of lib folder mostly related
C to graphic calls. It is useful for applications that need to be
C compiled as pure Fortran.

C If the parameter of the C function change the matching subroutine
C in this source file should be adapted to match.

C  sizeint     :- passess window size & position
C  win3d       :- opens a viewing box taking into account menu
C                 width and dialogue box.
C  win3dclr    :- clear viewing box.
C  winfnt(n)   :- changes the font (4 different sizes 0,1,2,3)
C  opensetup   :- place environment button on screen.
C  viewtext    :- displays a line of text within the viewing
C                 box with size and location parameters
C  feedbox     :- open feedback background box
C  opengdisp   :- opens a scrolling text display area.
C  etplot      :- general line plotting.
C  axiscale    :- determines scaling parameters for horizontal
C                 and vertical axis.
C  linescale   :- stores scaling parameters for lines.
C  dinterval   :- determins the tic interval for an axis as well
C                 as the number of decimal places.
C  vrtaxis     :- draws a vertical axis with tic marks and labels.
C  horaxis     :- draws a horizontal axis with tic marks and labels.
C  captextf    :- text feedback capture
C  captexgf    :- graphic feedback capture
C  updcapt     :- notify for capture button
C  capexall    :- save whole display images

C ************

      subroutine sizeint(isize,iulx,iuly)
      return
      end

      subroutine sizehwxy(isizeh,isizew,iulx,iuly)
      return
      end

      subroutine jwinint(iterm,msg)
      character*(*) msg
      write(6,'(a)') msg
      return
      end

      subroutine setcscale()
      return
      end

      subroutine clrcscale()
      return
      end

      subroutine setgscale()
      return
      end

      subroutine clrgscale()
      return
      end

      subroutine setzscale()
      return
      end

      subroutine foundcolour(md,nic,ncs,ngs,nzc)
      return
      end

      subroutine winscl(act,n)
      character act*1
      return
      end

      subroutine userfonts(ifs,itfs,imfs)
      return
      end

      subroutine winenqcl(act,n,nxcolid)
      character act*1
      return
      end

      subroutine winfin
      return
      end

      subroutine winclr
      return
      end

C      subroutine windcl(n,ir,ig,ib)
C      return
C      end

C      subroutine f_to_c_l(msg,if_len,ilen)
C      character*(*) msg
C      if_len = LEN(msg)
C      ilen = lnblnk(msg)
C      return
C      end

C      subroutine winlod(name,itime,lix,liy)
C      character*(*) name
C      return
C      end

C      subroutine showlogo(itime,lix,liy)
C      return
C      end

C      subroutine wwcopen(name)
C      character*(*) name
C      return
C      end

C      subroutine wwcclose(name)
C      character*(*) name
C      return
C      end

C      subroutine wwcsetstart
C      write(6,*) '*start_set'
C      return
C      end

C      subroutine wwcsetend
C      write(6,*) '*end_set'
C      return
C      end

      subroutine wstxpt(ix,iy,buff)
      character*(*) buff
      write(6,*) buff
      return
      end

      subroutine textatxy(ix,iy,buff,ixcolid)
      character*(*) buff
      write(6,*) buff
      return
      end

      subroutine winfnt(n)
      return
      end

      subroutine feedbox(menu_char,id_lines,igw,igh)
      return
      end

      subroutine win3d(menu_c,icl,icr,ict,icb,ivl,ivr,ivt,ivb,igw,igh)
      return
      end

      subroutine win3dclr
      return
      end

      subroutine viewtext(msg,linep,iside,isize)
      character*(*) msg
      return
      end

      subroutine etlabel(msg,x,y,ipos,isize)
      character*(*) msg
      write(6,*) msg
      return
      end

      subroutine pause_effect
      dum1 = 10.25
      dum2 = 10.25
      dum3 = 10.25
      dum4 = 5.0
      do 42 i=1,100
        dum = dum1*dum2*dum3/dum4
  42  continue
      return
      end

      subroutine forceflush
      return
      end

      subroutine pausems(msec)
      return
      end

      subroutine pauses(is)
      return
      end

      subroutine esymbol(ix,iy,isym,isize)
      return
      end

      subroutine qbox(msg,msglen,iasklen,ib_bottom,ib_left,act)
      character*(*) msg
      character act*1
      write(6,*) '? ',act,msg
      return
      end

      subroutine dbox(msg,msglen,iasklen,ib_bottom,ib_left,act)
      character*(*) msg
      character act*1
      write(6,*) 'def ',act,msg
      return
      end

      subroutine okbox(msg,msglen,iasklen,ib_bottom,ib_left,act)
      character*(*) msg
      character act*1
      write(6,*) 'ok ',act,msg
      return
      end

      subroutine epopup(title,list,impx,impy,ino)
      character*(*) title,list(*)
      return
      end

      subroutine altbox(msg,msglen,iasklen,ib_bottom,ib_left,act)
      character*(*) msg
      character act*1
      write(6,*) 'alt ',act,msg
      return
      end

      subroutine abcdboxs(msg,msglen,iasklen,ib_bottom,ib_left,act)
      character*(*) msg
      character act*1
      write(6,*) 'abcd ',act,msg
      return
      end

      subroutine openaskbox(msg1,msg2,iasklen)
      character*(*) msg1,msg2
      write(6,*) msg1
      write(6,*) msg2
      return
      end

      subroutine openaskaltbox(msg1,msg2,alt,iasklen)
      character*(*) msg1,msg2,alt
      write(6,*) msg1
      write(6,*) msg2
      write(6,*) alt
      return
      end

      subroutine openaskcnclbox(msg1,msg2,cncl,iasklen)
      character*(*) msg1,msg2,cncl
      write(6,*) msg1
      write(6,*) msg2
      write(6,*) cncl
      return
      end

      subroutine strip_leading_space(line)
      character*(*) line
      character fstr*124
      fstr = ' '
      write(fstr,'(a)') line(1:LEN(line))
      line = ' '
      K=0
      DO 99 I=1,LEN(line)
        IF(fstr(I:I).NE.' '.OR.K.GE.1)THEN
          if(ichar(fstr(I:I)).lt.32)goto 100
          K=K+1
          line(K:K)=fstr(I:I)
        ENDIF
 99   CONTINUE
 100  return
      end

C Dummy subroutine with same parameters as function in esru_x.c
      subroutine updhelp(dh,ni,iw)
      dimension dh(60)
      character dh*72
      return
      end

      subroutine egphelp(impx,impy,ipflg,ishowmoreflg,iuresp)
      return
      end

      subroutine askdialog(sstr,id,iq)
      character*(*) sstr
      return
      end

      subroutine askaltdialog(sstr,alt,id,iq)
      character*(*) sstr,alt
      return
      end

      subroutine askcncldialog(sstr,cncl,id,iq)
      character*(*) sstr,cncl
      return
      end

      subroutine msgbox(msg1,msg2)
      character*(*) msg1,msg2
C      write(6,*) msg1
C      write(6,*) msg2
      return
      end

      subroutine continuebox(msg1,msg2,opta)
      character*(*) msg1,msg2,opta
C      write(6,*) msg1
C      write(6,*) msg2
C      write(6,*) opta
      return
      end

      subroutine abbox(msg1,msg2,opta,optb,iok)
      character*(*) msg1,msg2,opta,optb
      return
      end

      subroutine abcdefbox(msg1,msg2,opta,optb,optc,optd,opte,optf,
     &  optg,iok)
      character*(*) msg1,msg2,opta,optb,optc,optd,opte,optf,optg
      return
      end

      subroutine opengdisp(menu_char,idispl_l,idial_l,igdw,igdh)
      return
      end

      subroutine egdisp(msg,line)
      character*(*) msg
      write(6,*) msg
      return
      end

      subroutine espad(limit,limtty,line)
      return
      end

      subroutine egdispclr
      return
      end

      subroutine trackview(ichar,irx,iry)
      return
      end

      subroutine eline(ix,iy,ioperation)
      return
      end

      subroutine u2pixel(ux,uy,ix,iy)
      ix=0
      iy=0
      return
      end

      subroutine pixel2u(ux,uy,gx,gy)
      gx=0.0
      gy=0.0
      return
      end

      subroutine etplotwwc(ux,uy,iupdown,isym)
      return
      end

      subroutine etplot(ux,uy,iupdown,isym)
      return
      end

      subroutine earc(x,y,rad,ang1,ang2,operation)
      return
      end

      subroutine nwkgflg(ngf)
      return
      end

      subroutine nwksmod(sel,con,dat)
      return
      end

      subroutine edline(ixy,iy1,ix2,iy2,ipdis)
      return
      end

      subroutine edwline(ixy,iy1,ix2,iy2)
      return
      end

      subroutine eswline(ixy,iy1,ix2,iy2)
      return
      end

      subroutine edash(ixy,iy1,ix2,iy2,ipdis)
      return
      end

      subroutine echain(ixy,iy1,ix2,iy2,ipdis)
      return
      end

      subroutine erectan(x,y,dx,dy,dt)
      return
      end

      subroutine egrbox(x,y,dx,dy,igp)
      return
      end

      subroutine etriang(x,y,dx,dy,dt)
      return
      end

C      subroutine ecirarc(x,y,ths,thf,r,num,di)
C      return
C      end

      subroutine ecirc(ix,iy,irad,ioperation)
      return
      end

      subroutine axiscale(igw,igh,xmn,xmx,ymn,ymx,xsc,ysc,sca,xadd,yadd)
      return
      end

      subroutine linescale(iloff,xladd,xlscale,iboff,xbadd,xbscale)
      return
      end

      subroutine dinterval(v1,v2,dv,ndec,mode)
      return
      end

      subroutine labelstr(n,val,iwticc,sstr)
      character sstr*10
      return
      end

      subroutine vrtaxis(ymn,ymx,ioffl,ioffb,iofft,yadd,sca,mode,side,
     &  msg)
      character*(*) msg
      return
      end

      subroutine horaxis(xmn,xmx,ioffl,ioffr,ioffb,xadd,sca,mode,msg)
      real xmn,xmx,sca,xadd
      integer ioffl,ioffr,ioffb
      character*(*) msg
      return
      end

      subroutine horaxishdw(xmn,xmx,ioffl,ioffr,ioffb,xadd,sca,mode,
     &  ind,idiv,isjday,msg)
      real xmn,xmx,sca,xadd
      integer ioffl,ioffr,ioffb,ind,idiv,isjday
      character*(*) msg
      return
      end

      subroutine updmenu(items,types,nitmsptr,iw)
      character*(*) items(*)
      character*(*) types
      return
      end

      subroutine closemouse
      return
      end

      subroutine evwmenu(titleptr,impx,impy,iwth,irpx,irpy,
     &  ino,ipflg,iuresp)
      character*(*) titleptr
      return
      end

C      subroutine aux_menu
C      return
C      end

C      subroutine refreshenv
C      return
C      end

      subroutine opencpw
      return
      end

      subroutine opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,iicfgz,
     &  iicfgn,iicfgc,iicfgdfn)
      return
      end

      subroutine opensetup
      return
      end

      subroutine updwire(iavail)
      return
      end

      subroutine updcapt(iavail)
      return
      end

      subroutine updazi(iavail)
      return
      end

      subroutine capexgf(cmd)
      character*(*) cmd
      return
      end

      subroutine capextf(cmd)
      character*(*) cmd
      return
      end

      subroutine capexall(cmd)
      character*(*) cmd
      return
      end

      subroutine openmouse(mseb1,mseb2,mseb3)
      character*(*) mseb1,mseb2,mseb3
      return
      end

      subroutine findrtb(iright,itop,ibottom)
      iright=0
      itop=0
      ibottom=0
      return
      end

C      subroutine curproject(fcfgroot,fpath,fupath,fimgpth,fdocpth,
C     &  ibrowse,iincomp,iincon)
C      character*(*) fcfgroot,fpath,fupath,fimgpth,fdocpth
C      return
C      end

C     subroutine curviews_(EVX,EVY,EVZ,VX,VY,VZ,EAN,JITZNM,JITSNM,
C     & JITVNO,JITOBS,JITSNR,JITGRD,JITORG,DIS,JITBND,JITDSP,JITHLS,
C     & JITHLZ,JITPPSW)
C      return
C      end

      subroutine curmodule(fcmodule)
      character*(*) fcmodule
      return
      end

      subroutine textpixwidth(buff,ipixw)
      character*(*) buff
      integer ipixw
      return
      end

      subroutine openask2altbox(prom1,prom2,msg1, msg2,istr)
      character*(*) prom1,prom2,msg1, msg2
      integer istr
      return
      end

      subroutine ask2altdialog(STRVAL,CMD,CMD2,idef,iquery)
      character*(*) STRVAL,CMD,CMD2
      integer idef, iquery
      return
      end

C      subroutine nwkgflg(ngf)
C      return
C      end

      subroutine fprofma(sstr,nstr,title,list,nlist,listtypes,impx,
     &  impy,impcwth,iswidth,listact,ino,nhelp)
      character*(*) sstr(*),list(*)
      character*(*) title,listtypes
      dimension iswidth(nlist),listact(nlist)
      return
      end

C Dummy subroutine for a function available in GTK for displaying images
C in a pop-up window.
C      subroutine popupimage(head,topic,act,longtfile)
C      character*(*) head,topic,act,longtfile
C      return
C      end

      subroutine winlodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &            inuma, inumb,ixbul,iybul,ixblr,iyblr)
      character*(*) fname
      integer ixoffset,iyoffset,iwidth,ihight,
     &            inuma, inumb,ixbul,iybul,ixblr,iyblr
      return
      end

      subroutine checklodpart(fname,ixoffset,iyoffset,iwidth,ihight,
     &      inuma,inumb,ixbul,iybul,ixblr,iyblr)
      character*(*) fname
      integer ixoffset,iyoffset,iwidth,ihight,
     &        inuma, inumb,ixbul,iybul,ixblr,iyblr
      return
      end

      subroutine findviewtext(icx,line,isize,ix,iy)
      integer icx,line,isize,ix,iy
      return
      end

C      subroutine getfilelist(subpath,action,listf,nwlistf,nlistf)
C      character*(*) subpath, action, listf
C      integer nwlistf(50),nlistf
C      return
C      end

C Return string indicating xlibs
      subroutine getXlibs( cLibrary )

      character*3 cLibrary
      write(cLibrary, '(A)') "noX"

      return
      end


C A zero says there is neither X11 or GTK available.
      integer function iXavail()
      iXavail = 0
      return
      end

C A three says that this is the text only version.
      integer function igraphiclib()
      igraphiclib = 3
      return
      end



C      function lnblnk(string)
C      character*(*) string
C      character a*1,sl*1
C      left=len(string)
C      lnb=leng(string)
C      sl='\'
C      islash=INDEX(string,sl)
C      if(lnb.lt.left.and.lnb.gt.0.and.islash.eq.0)then
C        lnblnk=lnb
C        return
C      endif
C      lnblnk=left
C 42   continue
C      left=left-1
C      if(left.le.0)then
C        lnblnk = 1
C        return
C      else
C        a=string(left:left)
C        if(ichar(a).gt.32)then
C          if(islash.eq.0)then
C            lnblnk = left+1
C          else
C            lnblnk = left
C          endif
C          return
C        elseif(ichar(a).eq.32.or.ichar(a).eq.9)then
C          goto 42
C        endif
C      endif
C      end

