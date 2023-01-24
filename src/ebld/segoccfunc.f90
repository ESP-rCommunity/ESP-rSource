! These are the functions used in the multi-segmented model
! Respiration heat loss/ Shivering/ Sweating /Skin blood flow/ impedance in each artery based on Avolio arteries tree
! In addition a function to calculate the cardiac output every time step using the Fourier series to represent the pulsatile shape.
! Intel MKL Math Kernel been used!!

      real function Qress(M,T,HR)
      implicit none
      real, intent(in) :: M,T,HR
      real :: Tex,mres
      real :: cpair=1005.7
      mres=1.43e-6*M
      Tex=32.6+0.06*T+32.0*HR
      Qress=mres*(cpair*(Tex-T))
      return
      end function Qress

      real function Qresl(M,T,HR)
      implicit none
      real, intent(in) :: M,T,HR
      real :: HRex,mresl
      real :: hfg=2.43e6
      mresl=1.43e-6*M
      HRex=0.2*HR+0.0277+6.5e-5*T
      Qresl=mresl*(hfg*(Hrex-HR))
      return
      end function Qresl

      real function Mshiv(coreTemp,skinTemp,w)
      implicit none
      real, intent(in) :: coreTemp,skinTemp,w
      real :: Tshiver,Mshivmax
      if (coreTemp<=35.8) then
        Tshiver=35.5
      else
        Tshiver=-1.0222e4+570.97*coreTemp-7.9455*coreTemp**2
      end if

      if (coreTemp<37.1) then
        Mshivmax=-1.1861e9+6.552e7*coreTemp-9.0418e5*coreTemp**2
      else
        Mshivmax=0.0
      end if
      if (Mshivmax<=0) then
        Mshivmax=0.0
      end if
      if ((skinTemp>=40-Tshiver).and.(skinTemp<=Tshiver)) then
        Mshiv=Mshivmax*(1.0-((skinTemp-20.0)/(Tshiver-20.0))**2)*w/3600.0
        return
      else
        Mshiv=0.0
        return
      end if
      if (Mshiv<=0) then
        Mshiv=0.0
      end if
      return
      end function Mshiv

      real function msw(coreTemp,skinTemp,A,w)
      implicit none
      real, intent(in) :: coreTemp,skinTemp,w,A
      real :: Tsweat

      if (skinTemp<33.0) then
        Tsweat=42.084-0.15833*skinTemp
      else
        Tsweat=36.85
      end if

      if (coreTemp>Tsweat) then
        msw=(45.8+739.4*(coreTemp-Tsweat))*w/3600.0/1000.0/A
        return
      else
        msw=0.0
        return
      end if
      end function msw

      real function Psat(T)
      real, intent(in) :: T

      real :: c8=-5800.2206,c9=1.3914993,c10=-0.04860239,c11=0.41764768e-4,&
          c12=-0.14452093e-7,c13=6.549673
      Psat=(exp(c8/T+c9+c10*T+c11*T**2+c12*T**3+c13*log(T)))/1000.0
      return
      end function Psat

      real function Psk(Psatsk,he,Pa,msw)
      real, intent(in) :: Psatsk,he,Pa,msw
      Psk=(Psatsk/he+Pa*0.3+msw*0.3*2.43e6/he)/(1/he+0.3)
      if (Psk>=Psatsk) then
        Psk=Psatsk
      end if
      return
      end function Psk

      real function facto(a)
      implicit none
      integer :: a, i
      real :: k
      k=1
      do i=1,a,1
        k=k*i
      end do
      facto=k
      return
      end function facto

      complex(8) function besselj0(a)
      implicit none
      real :: facto
      integer :: i
      complex(8) :: k,a
      k=1
      do i=1,50,1
        k=k+((-1)**i)*((a/2)**(2*i))/facto(i)/facto(i)
      end do
      besselj0=k
      return
      end function besselj0

      complex(8) function besselj1(a)
      implicit none
      real :: facto
      integer :: i
      complex(8) :: k,a
      k=a/2
      do i=1,50,1
        k=k+((-1)**i)*((a/2)**(2*i+1))/facto(i)/facto(i+1)
      end do
      besselj1=k
      return
      end function besselj1

      subroutine bloodtree(beatpermin,ratios,a,arteryradius, coreTemp)

      implicit none
      common/stddbpath/standarddbpath
      character standarddbpath*72
      common/filep/ifil
      integer ifil
      type arteriestype
        real length
        real radius
        real thick
        real E
        integer isterminal
        integer previous
        complex(8) charact_imp
        complex(8) input_imp
        complex(8) term_imp
        complex(8) gamma
        complex(8) ratio
      end type
      type (arteriestype) arteries(138)
      real :: z(138,7)
      real :: z_t(7)
      integer :: nodes(69,8),k,nodeok(69),p,ok,s,t,i
      integer :: nodes_t(8),iunit
      real :: ro=1.05,sigma=0.5,gamma0,beatpermin,w,a,arteryradius(138),coreTemp

      real :: l,phi,E,h,R0,c0,alpha
      complex(8) :: JJ=(0,1),F10,Z0,g,Zi,term,x,gamma,ratios(138)
      complex(8) :: besselj0,besselj1

      character auxFilename*170
      character(len=4000) :: buffer
      integer :: ios = 0
      integer :: count = 0, line_index = 0 
      Character(len = 30) :: start_identifier, stop_identifier

      intent(in) :: beatpermin,arteryradius,coreTemp
      intent(out) :: a,ratios

      iunit = ifil + 1

      start_identifier = "arteries_param start"
      stop_identifier  = "arteries_param end"
      auxFilename = trim(standarddbpath) // trim('/multi_seg_occup_DB.txt')
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) z_t
            z(line_index, :) = z_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)
      do i=1,138
        arteries(int(z(i,1)))%length=z(i,2)
        arteries(int(z(i,1)))%radius=z(i,3)
        arteries(int(z(i,1)))%thick=z(i,4)
        arteries(int(z(i,1)))%E=z(i,5)*1e6
        arteries(int(z(i,1)))%isterminal=int(z(i,6))
        arteries(int(z(i,1)))%previous=int(z(i,7))
      end do


      start_identifier = "arterie nodes start"
      stop_identifier  = "arterie nodes end"
      auxFilename = trim(standarddbpath) // trim('/multi_seg_occup_DB.txt')
      ios = 0
      line_index = 0
      count = 0
      open(iunit, file=auxFilename)
      do while (ios == 0)
        read(iunit, '(A)', iostat=ios) buffer
        if (ios == 0) then
          if(buffer == stop_identifier) then
            count = 0
            EXIT
          endif
          if (count == 1) then
            line_index = line_index + 1
            read (buffer, *) nodes_t
            nodes(line_index, :) = nodes_t 
          endif
          if(buffer == start_identifier) then
            count =  1
          endif
        endif
      enddo
      close(iunit)

      w=2.0*3.14159*beatpermin/60.0

      do k=1,138
        arteries(k)%term_imp=(0.0,0.0)
      end do

      do k=1,69
        nodeok(k)=0
      end do

      do k=1,138
        if (arteries(k)%isterminal==1) then
          if (k > 128 .and. coreTemp>=37.2) then
            gamma0 = 0.975
          else if (k > 128 .and. coreTemp<37.2) then
            gamma0 = 0.975
          else
            gamma0 = 0.8
          end if
          l=arteries(k)%length
          phi=(3.14159/12.0)*(1-exp(-2.0*w))
          E=arteries(k)%E
          h=arteries(k)%thick
          R0=arteryradius(k)
          c0=sqrt(E*h/2.0/ro/R0)
          alpha=R0*sqrt(w*ro/R0)
          F10=2.0*besselj1(alpha*JJ**1.5)/alpha/JJ**1.5/besselj0(alpha*JJ**1.5)
          Z0=(cos(phi/2)+JJ*sin(phi/2))*ro*c0/sqrt(1-sigma**2)/(1-F10)**0.5
          g=JJ*(w/c0)*sqrt(1-sigma**2)*(1-F10)**(-0.5)*(cos(phi/2)-JJ*sin(phi/2))
          Zi=Z0*(1+gamma0)/(1-gamma0)
          arteries(k)%charact_imp=Z0
          arteries(k)%input_imp=Zi
          arteries(k)%term_imp=Zi
          arteries(k)%gamma=gamma0
          arteries(k)%ratio=(1-gamma0)/(exp(g*l)-gamma0*exp(-g*l))
        end if
      end do

      do while (nodeok(69)==0)
        do k=1,69
          term=(0.0,0.0)
          if (nodeok(k)==0) then
            AA:do p=1,nodes(k,2)
              if (arteries(nodes(k,p+3))%term_imp/=(0.0,0.0)) then
                ok=1
              else
                ok=0
              end if
              if (ok==0) then
                exit AA
              end if
            end do AA

            if (ok==1) then
              do s=1,nodes(k,2)
                term=term+1/arteries(nodes(k,s+3))%input_imp
              end do
              t=nodes(k,3)
              x=1/term
              l=arteries(t)%length
              phi=(3.14159/12.0)*(1-exp(-2.0*w))
              E=arteries(t)%E
              h=arteries(t)%thick
              R0=arteryradius(t)
              c0=sqrt(E*h/2.0/ro/R0)
              alpha=R0*sqrt(w*ro/R0)
              F10=2.0*besselj1(alpha*JJ**1.5)/alpha/JJ**1.5/besselj0(alpha*JJ**1.5)
              Z0=(cos(phi/2)+JJ*sin(phi/2))*ro*c0/sqrt(1-sigma**2)/(1-F10)**0.5
              g=JJ*(w/c0)*sqrt(1-sigma**2)*(1-F10)**(-0.5)*(cos(phi/2)-JJ*sin(phi/2))
              if (nodes(k,2)==1) then
                x=x+arteries(nodes(k,4))%term_imp
              end if
              gamma=(x-Z0)/(x+Z0)
              Zi=Z0*(1+gamma*exp(-2.0*g*l))/(1-gamma*exp(-2.0*g*l))
              arteries(t)%charact_imp=Z0
              arteries(t)%input_imp=Zi
              arteries(t)%term_imp=x
              arteries(t)%gamma=gamma
              arteries(t)%ratio=(1-gamma)/(exp(g*l)-gamma*exp(-g*l))
              nodeok(k)=1
            end if
          end if
        end do
      end do
      do k=1,138
        ratios(k)=(1.0,0.0)
      end do

      do k=2,138
        ratios(k)=ratios(k)*arteries(arteries(k)%previous)%ratio
      end do

      a=cdabs(arteries(1)%input_imp)
      return

      end subroutine bloodtree

! Use the Fourier function to represent the wave shape of heart beat so more accurate blood flow can be calculated in every time step.
      subroutine four(ratio,beatpermin,step,velocity,m,cardiac)
      implicit none
      integer i,j
      real, intent(in) :: beatpermin,step,cardiac
      complex(8), intent(in) :: ratio
      real, intent(out) :: velocity(2,int(60.0/beatpermin/step)+1),m
      complex (8):: CardOut(35),CardOutf(35),CardOutr(35)
      real a(35),b(35),t,w,r,phi,CardOutratio

      data CardOutr /0.0,15.0,30.0,45.0,60.0,52.5,45.0,42.0,39.0,33.0,27.0,22.5,18.0,12.0&
        ,6.0,-4.5,-15.0,-10.5,-6.0,-3.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,-0.6,-1.2,-0.6,0.0,0.0,0.0/

      do i=1,35
        CardOut(i)=CardOutr(i)*dcmplx(1.0537)
      end do
      
      CardOutratio=(310000.0-250000.0)/(427500.0-250000.0)

      do i=1,35
        CardOut(i)=CardOut(i)*(((1.4758-2.0688*exp(-5*CardOutratio))-1)*CardOutratio+1.0)
      end do

      if (CardOutratio .LE. 0.15) then
        data CardOutf /(393.5788877784,0),(190.0860242893,-328.5214874078),(-149.9294624471,-189.6618100318),&
          (-45.7237551412,-28.28183106),(-85.2609979122,-43.3477095962),(-34.1564518669,32.2960095378),&
          (-31.4910887243,-0.8399462338),(-12.7844460419,32.2906013871),(8.8119893943,1.1596391064),&
          (1.5575547113,0.5705446387),(6.9107157459,-12.8874267262),(-14.3445239407,-8.1044726402),&
          (-4.1082220555,-1.1706864477),(-15.1777699206,-2.1161928789),(-4.9731813649,6.5652629078),&
          (-6.5228666383,2.464159016),(-0.38707668010146357,5.975134489),(0.7041147036,0.3768376652),&
          (0.7041147036,-0.3768376652),(-0.38707668010144403,-5.975134489),(-6.5228666383,-2.464159016),&
          (-4.9731813649,-6.5652629078),(-15.1777699206,2.1161928789),(-4.1082220555,1.1706864477),&
          (-14.3445239407,8.1044726402),(6.9107157459,12.8874267262),(1.5575547113,-0.5705446387),&
          (8.8119893943,-1.1596391064),(-12.7844460419,-32.2906013871),(-31.4910887243,0.8399462338),&
          (-34.1564518669,-32.2960095378),(-85.2609979122,43.3477095962),(-45.7237551412,28.28183106),&
          (-149.9294624471,189.6618100318),(190.0860242893,328.5214874078)/
      elseif(CardOutratio .GT. 0.150 .and. CardOutratio .LE. 0.25) then
        data CardOutf /(376.3137893173,0),(181.7475335131,-314.1102575303),(-143.3525168544,-181.3419282337),&
          (-43.7179942658,-27.0411938889),(-81.5208594811,-41.44617855),(-32.6581131021,30.8792826709)&
          ,(-30.1096712642,-0.8031003692),(-12.2236316117,30.8741117591),(8.4254344513,1.1087692961)&
          ,(1.4892295641,0.5455165956),(6.6075638455,-12.3220948493)&
          ,(-13.7152736788,-7.7489542868),(-3.9280069563,-1.1193320245),(-14.5119677136,-2.0233619889)&
          ,(-4.7550231541,6.2772649637),(-6.2367284885,2.356063919),(-0.37009681354489032,5.7130236683)&
          ,(0.6732273515,0.3603069529),(0.6732273515,-0.3603069529),(-0.37009681354487256,-5.7130236683)&
          ,(-6.2367284885,-2.356063919),(-4.7550231541,-6.2772649637),(-14.5119677136,2.0233619889)&
          ,(-3.9280069563,1.1193320245),(-13.7152736788,7.7489542868),(6.6075638455,12.3220948493)&
          ,(1.4892295641,-0.5455165956),(8.4254344513,-1.1087692961),(-12.2236316117,-30.8741117591)&
          ,(-30.1096712642,0.8031003692),(-32.6581131021,-30.8792826709),(-81.5208594811,41.44617855)&
          ,(-43.7179942658,27.0411938889),(-143.3525168544,181.3419282337),(181.7475335131,314.1102575303)/

      elseif(CardOutratio .GT. 0.25 .and. CardOutratio .LE. 0.4) then
        data CardOutf /(440.9778982361,0),(212.9782315009,-368.0855847229)&
          ,(-167.9855837969,-212.5029287916),(-51.2303024064,-31.6878338914),(-95.5290459635,-48.5681078551),&
          (-38.2699398346,36.1854429942),(-35.2835849432,-0.9411016097),(-14.324086786,36.1793835356),&
          (9.8732241059,1.2992953426),(1.7451322322,0.6392557716),(7.7429785986,-14.4394695139)&
          ,(-16.0720460752,-9.0805005608),(-4.6029784213,-1.3116731238),(-17.0056405141,-2.3710476272)&
          ,(-5.5721054506,7.3559226076),(-7.3084205224,2.7609195958),(-0.43369262464976899,6.6947245659)&
          ,(0.788911783,0.4222205173),(0.788911783,-0.4222205173),(-0.43369262464975122,-6.6947245659)&
          ,(-7.3084205224,-2.7609195958),(-5.5721054506,-7.3559226076),(-17.0056405141,2.3710476272)&
          ,(-4.6029784213,1.3116731238),(-16.0720460752,9.0805005608),(7.7429785986,14.4394695139)&
          ,(1.7451322322,-0.6392557716),(9.8732241059,-1.2992953426),(-14.324086786,-36.1793835356)&
          ,(-35.2835849432,0.9411016097),(-38.2699398346,-36.1854429942),(-95.5290459635,48.5681078551)&
          ,(-51.2303024064,31.6878338914),(-167.9855837969,212.5029287916),(212.9782315009,368.0855847229)/

      elseif(CardOutratio .GT. 0.40 .and. CardOutratio .LE. 0.65) then
        data CardOutf /(492.3581930202,0),(237.7932718031,-410.9728721005)&
          ,(-187.558330752,-237.2625894628),(-57.1993726252,-35.37992425),(-106.6595596733,-54.226993955)&
          ,(-42.7289406098,40.4015697782),(-39.3946322376,-1.0507535408),(-15.9930497987,40.3948043052)&
          ,(11.0235973266,1.4506820175),(1.9484653446,0.7137383027),(8.6451474477,-16.121876281)&
          ,(-17.9446715932,-10.1385100381),(-5.1392918944,-1.464501989),(-18.9870432694,-2.647308924)&
          ,(-6.221336221,8.2129938429),(-8.1599570787,3.0826066084),(-0.48422407983004767,7.4747566788)&
          ,(0.8808314011,0.4714153063),(0.8808314011,-0.4714153063),(-0.48422407983002103,-7.4747566788)&
          ,(-8.1599570787,-3.0826066084),(-6.221336221,-8.2129938429),(-18.9870432694,2.647308924)&
          ,(-5.1392918944,1.464501989),(-17.9446715932,10.1385100381),(8.6451474477,16.121876281)&
          ,(1.9484653446,-0.7137383027),(11.0235973266,-1.4506820175),(-15.9930497987,-40.3948043052)&
          ,(-39.3946322376,1.0507535408),(-42.7289406098,-40.4015697782),(-106.6595596733,54.226993955)&
          ,(-57.1993726252,35.37992425),(-187.558330752,237.2625894628),(237.7932718031,410.9728721005)/

      elseif(CardOutratio .GT. 0.650 .and. CardOutratio .LE. 0.75) then
        data CardOutf /(465.5092596791,0),(224.8260950726,-388.561986277)&
          ,(-177.3305307655,-224.3243515313),(-54.0802163594,-33.4506109141),(-100.8432750081,-51.2699253683)&
          ,(-40.3988758431,38.1984195733),(-37.2463916479,-0.9934545821),(-15.1209279694,38.1920230303)&
          ,(10.4224662111,1.3715744383),(1.8422129923,0.6748172237),(8.1737163009,-15.2427293759)&
          ,(-16.9661252863,-9.5856438849),(-4.8590396158,-1.3846407887),(-17.9516550778,-2.5029477215)&
          ,(-5.8820786564,7.7651285949),(-7.7149839945,2.9145080564),(-0.45781871027217402,7.0671484645)&
          ,(0.8327985179,0.4457084158),(0.8327985179,-0.4457084158),(-0.45781871027215715,-7.0671484645)&
          ,(-7.7149839945,-2.9145080564),(-5.8820786564,-7.7651285949),(-17.9516550778,2.5029477215)&
          ,(-4.8590396158,1.3846407887),(-16.9661252863,9.5856438849),(8.1737163009,15.2427293759)&
          ,(1.8422129923,-0.6748172237),(10.4224662111,-1.3715744383),(-15.1209279694,-38.1920230303)&
          ,(-37.2463916479,0.9934545821),(-40.3988758431,-38.1984195733),(-100.8432750081,51.2699253683)&
          ,(-54.0802163594,33.4506109141),(-177.3305307655,224.3243515313),(224.8260950726,388.561986277)/

      elseif(CardOutratio .GT. 0.750 .and. CardOutratio .LE. 0.85) then
        data CardOutf /(644.7729185081,0),(311.4047131451,-538.1939042112)&
          ,(-245.6190107606,-310.7097524311),(-74.9060479692,-46.3321568294),(-139.6771629928,-71.0135378063)&
          ,(-55.9561395185,52.9083062443),(-51.5896604575,-1.3760254967),(-20.9438687946,52.899446447)&
          ,(14.4360693525,1.8997560953),(2.5516335559,0.9346836003),(11.3213449675,-21.1125748874)&
          ,(-23.4996359131,-13.2769938619),(-6.7302144674,-1.9178541863),(-24.8646848498,-3.4668116126)&
          ,(-8.1472171462,10.7554136091),(-10.6859587493,4.036860334),(-0.63412080389836412,9.7886472638)&
          ,(1.1535021479,0.617346938),(1.1535021479,-0.617346938),(-0.63412080389832681,-9.7886472638)&
          ,(-10.6859587493,-4.036860334),(-8.1472171462,-10.7554136091),(-24.8646848498,3.4668116126)&
          ,(-6.7302144674,1.9178541863),(-23.4996359131,13.2769938619),(11.3213449675,21.1125748874)&
          ,(2.5516335559,-0.9346836003),(14.4360693525,-1.8997560953),(-20.9438687946,-52.899446447)&
          ,(-51.5896604575,1.3760254967),(-55.9561395185,-52.9083062443),(-139.6771629928,71.0135378063)&
          ,(-74.9060479692,46.3321568294),(-245.6190107606,310.7097524311),(311.4047131451,538.1939042112)/

      elseif(CardOutratio .GT. 0.850 .and. CardOutratio .LE. 0.95) then
        data CardOutf /(902.9797606339,0),(436.1103657807,-753.7199358865)&
          ,(-343.9800109113,-435.1370999359),(-104.9030493119,-64.8864099011),(-195.6125134677,-99.4517380141)&
          ,(-78.364428806,74.0960551175),(-72.2493422319,-1.9270709703),(-29.3310855466,74.083647311)&
          ,(20.2171618476,2.660535601),(3.5734650005,1.3089885592),(15.8551097222,-29.5673519637)&
          ,(-32.9103394431,-18.593921046),(-9.4254074176,-2.6858812837),(-34.8220381617,-4.8551367936)&
          ,(-11.4098653608,15.0625445447),(-14.9652756756,5.65346818),(-0.88806188237866834,13.7086253306)&
          ,(1.6154355487,0.864570726),(1.6154355487,-0.864570726),(-0.88806188237862216,-13.7086253306)&
          ,(-14.9652756756,-5.65346818),(-11.4098653608,-15.0625445447),(-34.8220381617,4.8551367936)&
          ,(-9.4254074176,2.6858812837),(-32.9103394431,18.593921046),(15.8551097222,29.5673519637)&
          ,(3.5734650005,-1.3089885592),(20.2171618476,-2.660535601),(-29.3310855466,-74.083647311)&
          ,(-72.2493422319,1.9270709703),(-78.364428806,-74.0960551175),(-195.6125134677,99.4517380141)&
          ,(-104.9030493119,64.8864099011),(-343.9800109113,435.1370999359),(436.1103657807,753.7199358865)/

      elseif(CardOutratio .GT. 0.950) then
        data CardOutf /(680.5096165792,0),(328.6643961934,-568.0234341223)&
          ,(-259.2325050252,-327.9309171103),(-79.0577341568,-48.9001280516),(-147.4188042095,-74.9494806579)&
          ,(-59.0575223555,55.8407621702),(-54.4490301154,-1.4522920494),(-22.1046878893,55.8314113164)&
          ,(15.2361920577,2.0050505456),(2.6930584752,0.9864886694),(11.9488333053,-22.2827445589)&
          ,(-24.8021090308,-14.0128745221),(-7.1032382646,-2.0241517277),(-26.2428161416,-3.6589605015)&
          ,(-8.5987786664,11.3515350617),(-11.2782306492,4.2606043139),(-0.66926710588930760,10.3311854532)&
          ,(1.217435289,0.6515635443),(1.217435289,-0.6515635443),(-0.66926710588930405,-10.3311854532)&
          ,(-11.2782306492,-4.2606043139),(-8.5987786664,-11.3515350617),(-26.2428161416,3.6589605015)&
          ,(-7.1032382646,2.0241517277),(-24.8021090308,14.0128745221),(11.9488333053,22.2827445589)&
          ,(2.6930584752,-0.9864886694),(15.2361920577,-2.0050505456),(-22.1046878893,-55.8314113164)&
          ,(-54.4490301154,1.4522920494),(-59.0575223555,-55.8407621702),(-147.4188042095,74.9494806579)&
          ,(-79.0577341568,48.9001280516),(-259.2325050252,327.9309171103),(328.6643961934,568.0234341223)/
      endif

! Use constant coefficient, as a replacement for using external library Fourier functions.
      a(1)=dble(CardOutf(1))/35.0
      b(1)=0.0
      do i=2,35
        a(i)=2.0*dreal(CardOutf(i))/35.0
        b(i)=-2.0*dimag(CardOutf(i))/35.0
      end do

      t=0.0
      i=1
      do while (t<=60.0/beatpermin)
        velocity(1,i)=t
        velocity(2,i)=0.0
        i=i+1
        t=t+step
      end do

      velocity(1,int(60.0/beatpermin/step))=60.0/step
      velocity(2,int(60.0/beatpermin/step))=0.0
      w=2.0*3.14159*beatpermin/60.0

      r=cdabs(ratio)
      phi=datan2(dimag(ratio),dreal(ratio))

      do i=1,int(35/2)
        do j=1,int(60.0/beatpermin/step)
          t=velocity(1,j)
          velocity(2,j)=velocity(2,j)+r*(0.5*a(i)*cos((i-1)*w*t+phi)+0.5*a(i)*cos((i-1)*w*t-phi)&
            +0.5*b(i)*sin((i-1)*w*t+phi)+0.5*b(i)*sin((i-1)*w*t-phi))
        end do
      end do

      t=0
      do i=1,int(60.0/beatpermin/step)
        t=t+step*velocity(2,i)
      end do

      m=t*beatpermin/60.0

      return

      end subroutine four

      real function CardOut(coreTemp,skinTemp)
      real, intent(in) :: coreTemp,skinTemp
      real CardOutcon,CardOutdil

      if (skinTemp>=33.7) then
            CardOutcon=290000.0
      else if ((skinTemp>10.7).and.(skinTemp<33.7)) then
            CardOutcon=(skinTemp-10.7)*(290000.0-270000.0)/(33.7-10.7)+270000.0
      else
            CardOutcon=270000.0
      end if

      if (coreTemp>=41) then
          CardOutdil=427500
      else if (coreTemp>=37.2 .and. coreTemp<41) then
            CardOutdil=(coreTemp-37.2)*(508000.0-427500.0)/(41-37.2)+427500.0
      else if ((coreTemp>36.8).and.(coreTemp<37.2)) then
            CardOutdil=(coreTemp-36.8)*(427500.0-290000.0)/(37.2-36.8)+290000.0
      else
            CardOutdil=290000.0
      end if

      CardOut=CardOutdil*CardOutcon/290000.0
      return
      end function CardOut

      real function beatpermin(M,CardOut,sh)
      real, intent(in) :: M,CardOut,sh
      beatpermin=3.4*(0.0476*(M+sh)/1.83-7)+75
      return
      end function beatpermin

      subroutine calc_velocity(velocity,beatpermin,step,time,v)
      real, intent(in) :: beatpermin,step,velocity(2,int(60.0/beatpermin/step)+1),time
      real, intent(out) :: v
      real :: t
      if (time<=60.0/beatpermin) then
        t=time/step+1
      else
        t=floor((time*beatpermin/60.0-floor(time*beatpermin/60.0))/step)
      end if
      if (int(t)==0) then
        t=1.0
      end if
      if (int(t)>int(60.0/beatpermin/step)) then
        t=int(60.0/beatpermin/step)
      end if
      v=velocity(2,int(t))
      return
      end subroutine calc_velocity

      ! Calculates skin blood flow (cm3/h).
      real function mskin(perfusion,coreTemp,skinTemp,basal,min,max)
      real, intent(in) :: perfusion,coreTemp,skinTemp,max,min,basal
      real :: dil,con
      if (coreTemp<=36.8) then
        dil=basal
      else if ((coreTemp>36.8).and.(coreTemp<37.2)) then
        dil=(coreTemp-36.8)*(max-basal)/(37.2-36.8)+basal
      else
        dil=max
      end if

      if (skinTemp<=10.7) then
        con=min
      else if ((skinTemp>10.7).and.(skinTemp<33.7)) then
        con=(skinTemp-10.7)*(basal-min)/(33.7-10.7)+min
      else
        con=basal
      end if

      mskin=con*dil/basal
      if (mskin/3600 > 0.95*perfusion) then
        mskin = 0.95*perfusion*3600
      end if
      return
      end function mskin

      real function Regain(Phi,coef1,coef2,p1,p2)
      implicit none
      real, intent(in) :: Phi,coef1(5),coef2(5),p1,p2
      integer :: i
      Regain=0
      do i=1,5
        Regain=Regain+p1*coef1(i)*Phi**(5-i)+p2*coef2(i)*Phi**(5-i)
      end do
      return
      end function Regain

      real function Phi(Regain,coef_i1,coef_i2,p1,p2)
      implicit none
      real, intent(in) :: Regain,coef_i1(5),coef_i2(5),p1,p2
      integer :: i
      Phi=0
      do i=1,5
        Phi=Phi+p1*coef_i1(i)*Regain**(5-i)+p2*coef_i2(i)*Regain**(5-i)
      end do
      return
      end function Phi

      real function Sorption(RH,coef)
      implicit none
      real, intent(in) :: RH,coef(7)
      integer :: i
      Sorption=0
      do i=1,7
        Sorption=Sorption+coef(i)*RH**(7-i)
      end do
      Sorption=Sorption*1000.0
      return
      end function Sorption

      real function AVAcoef(t)
      real, intent(in) :: t
      AVAcoef = 50*exp(-t/600)/600
      return
      end function AVAcoef





