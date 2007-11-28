C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001-7.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C esru_ask.F includes notices used by ESP-r modules.

C   cpwpk()     Copyright notice.
C   askabout() Creates text for a popup notice for each module.

C ***** copyright notice.
      subroutine cpwpk(il)
      common/pophelp/h(60)
      dimension dh(60)
      character H*72,dh*72

C First copy any existing help into dh array.
      do 50 i=1,60
        dh(i)=h(i)
   50 continue

      H(1)='This module is part of the ESP-r system. Copyright'
      H(2)='Energy Systems Research Unit, University of Strathclyde'
      H(3)='2001-2007.'
      H(4)=' '
      H(5)='ESP-r is free software. You can redistribute it and/or'
      H(6)='modify it under the terms of the GNU General Public'
      H(7)='License as published by the Free Software Foundation'
      H(8)='(version 2 or later).'
      H(9)=' '
      H(10)='ESP-r is distributed in the hope that it will be useful'
      H(11)='but WITHOUT ANY WARRANTY; without even the implied'
      H(12)='warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR'
      H(13)='PURPOSE. See the GNU General Public License for more'
      H(14)='details (available from Free Software Foundation, Inc.,'
      H(15)='59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.'
      CALL PHELPD('clm copyright',15,'-',0,0,IER)

C Recover previous help via dh array.
      do 51 i=1,60
        h(i)= dh(i)
   51 continue

      return
      end

C **** askabout() Creates text for a popup notice for each module. ***
C The intent of askabout is to have one place for the opening help
C message of each applicaton (and thus support an application independent
C static help menu under GTK).
C If initonly is 1 then only assign the h() array and do not display
C If initonly is 0 save prior h(), update h(), display and re-establish prior.
      subroutine askabout(module,initonly)
      common/pophelp/h(60)
      dimension dh(60)
      character H*72,dh*72,module*4

C First copy any existing help into dh array.
      if(initonly.eq.0)then
C        write(6,*) 'the module is ',module,' request to display' 
        do 50 i=1,60
          dh(i)=h(i)
   50   continue
      else
C        write(6,*) 'the module is ',module,' request to initialise only'
      endif

      if(module(1:3).eq.'aco')then
        H(1)='Aco supports an appraisal of the acoustic reverberation'
        H(2)='time of a zone. Please follow this procedure to perform'
        H(3)='this appraisal.'
        H(4)=' '
        H(5)=' 1) Load an ESP-r model.'
        H(6)=' 2) From the main menu select item `b BCF` and enter the'
        H(7)='    corrsponding BCF file (extension: bcf). This file'
        H(8)='    holds the acoustic coefficients for each'
        H(9)='    construction defined in your project.'
        H(10)=' 3) Select the zone(s) for analysis using option `d`.'
        H(11)=' 4) Start the simulation via option `k`.'
        H(12)=' '
        H(13)='It is possible to modify the calculation method by'
        H(14)='selecting `j calculation method >>` and then'
        H(15)='re-initiating the calculation (step 4)). Several'
        H(16)='options are available as follows.'
        H(17)=' `e frequency band` - switch from 1 to 1/3 octave band.'
        H(18)=' `f display >>` - switch between numeric formats.'
        H(19)=' `g output` - definition of the output data to display:'
        H(20)='     a) reverberation time only;'
        H(21)='     b) as a) plus equivalent surface area;'
        H(22)='     c) as b) plus the absorption coefficient for each'
        H(23)='        surface.'
        H(24)=' `h air absorption >>` - included or not; although'
        H(25)='     normally included, excluding it allows the effect'
        H(26)='     of the air absorption to be studied.'
        H(27)=' `i air properties` - allows definition of a constant'
        H(28)='     air temperature and humidity or the imposition'
        H(29)='     os a file of time varying values passed from an'
        H(30)='     ESP-r thermal simulation.'
        if(initonly.eq.0)then
          CALL PHELPD('aco opening',30,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'b2e')then
        H(1)='B2e manages the process of updating measured data'
        H(2)='when running ESP-r in BEMS mode.'
        if(initonly.eq.0)then
          CALL PHELPD('b2e opening',2,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'bld')then
        H(1)='Bld is a constrained version of ESP-r`s integrated'
        H(2)='simulation engine, bps, which supports building thermal'
        H(3)='and network flow but not plant, electrical power,'
        H(4)='moisture and CFD domain modelling.'
        if(initonly.eq.0)then
          CALL PHELPD('bld opening',4,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'bps')then
        H(1)='The ESP-r integrated simulation engine supports'
        H(2)='the multi domain modelling (thermal, lighting,'
        H(3)='acoustic and air/ power/ mositure flow) of models'
        H(4)='comprising building, plant and control elements.'
        H(5)=' '
        H(6)=' Initial command selections are as follows:'
        H(7)=' '
        H(8)=' ? Help:  this message.'
        H(9)=' a Input: initiates the input facilities which read all'
        H(10)='     data files and performing all time independant'
        H(11)='     pre-simulation calculations.'
        H(12)=' b Climate: permits the re-specification of climate.'
        H(13)=' c Initiate: set simulation details and initiate.  '
        H(14)=' t Trace:  allows the user to retrieve selected results'
        H(15)='     not normally included in the results library.'
        H(16)=' r Reporting: sets reporting (silent|summary|verbose)'
        H(17)='    as the model is read in and calculations progress.'
        if(initonly.eq.0)then
          CALL PHELPD('bps opening',17,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'cfg')then
  2     H(1)='This productivity aid is designed to assist the user'
        H(2)='to complete the definition of a problems topology via'
        H(3)='checking the match of surfaces to each other. '
        H(4)=' '
        H(5)='If a probable match is found then it will be presented'
        H(6)='for confirmation otherwise the user can specify one.'
        H(7)='of the standard boundary condition types (exterior,'
        H(8)='ground, adiabetic, etc.). '
        H(9)=' '
        H(10)='cfg may be useful in three cases: '
        H(11)=' 1) early in the problem definition to enhance the'
        H(12)='    clarity of the model.'
        H(13)=' '
        H(14)=' 2) prior to simulations, and when inconsistencies'
        H(15)='    have been detected in the model.'
        H(16)=' '
        H(17)=' 3) for browsing (systematically) through a problem'
        H(18)='    description.'
        H(19)=' '
        H(20)='Program control moves essentially from top to bottom'
        H(21)='of the control menu.  The tolerance settings may be '
        H(22)='used to allow for minor differences between zone'
        H(23)='descriptions and vertex placement. '
        if(initonly.eq.0)then
          CALL PHELPD('cfg opening',23,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'clm')then
        H(1)='Clm provides browsing, editing and analysis facilities'
        H(2)='for climatic data used by ESRU applications.'
        H(3)=' '
        H(4)='Begin by specifying a binary climate database. If it '
        H(5)='contains data you may proceed to analyse it.  If it is a'
        H(6)='new database you will need to either type in data, use'
        H(7)='curve fitting techniques or import data from a suitable'
        H(8)='ascii file (esp-r or EPW format). '
        H(9)=' '
        H(10)='The `manage climatelist` helps to create high-level'
        H(11)='information on available climate files, including '
        H(12)='seasons and typical assessment periods. Such information'
        H(13)='is held in a file called `climatelist` and which is '
        H(14)='typically located in /usr/esru/esp-r/climate '
        H(15)=' '
        H(16)='Note: the range of days which are associated with'
        H(17)='typical periods or seasons are scanned from a file'
        H(18)='called `climatelist`. You can alter this and save'
        H(19)='updated information to a text file which can then'
        H(20)='be inserted into the climatelist file via a text'
        H(21)='editor. '
        H(22)=' '
        H(23)='The standard climatelist file is found in the folder'
        H(24)='/usr/esru/esp-r/climate, however you may nominate '
        H(25)='a different location by editing the *db_climates '
        H(26)='entry of the file /usr/esru/esp-r/esprc or the dot'
        H(27)='file .esprc which may be located in your home folder.'
        H(28)=' '
        H(29)='ESP-r holds climate data as a random access binary'
        H(30)='file for quick access by the simulator. '
        H(33)=' '
        H(31)='The ASCII version of this file is useful for moving'
        H(32)='climate data between computer systems. Its format'
        H(33)='is as follows: '
        H(34)=' col     description                          variable'
        H(35)='1. Diffuse solar on the horizontal (W/M**2)       IDIF'
        H(36)='2. External dry bulb temperature   (TENTHS DEG.C) ITMP'
        H(37)='3. Direct normal solar intensity   (W/M**2)       IDNR'
        H(38)='4. Prevailing wind speed           (TENTHS M/S)   IVEL'
        H(39)='5. Wind direction                  (DEGREES)      IDIR'
        H(40)='6. Relative humidity               (PERCENT)      IHUM'
        H(41)=' '
        H(42)='There is a header which holds site related information.'
        H(43)=' '
        H(44)='ESP-r is able to import climate data in EPW format (a '
        H(45)='format shared by EnergyPlus and another dozen tools. '
        if(initonly.eq.0)then
          CALL PHELPD('clm opening',45,'clm_dbitems ',0,0,IER)
        endif
      elseif(module(1:3).eq.'dbm')then
        H(1)='Welcome to the ESP-r database mananger. '
        H(2)=' '
        h(3)='You can access a description of the db manager or the'
        h(4)='documentation associated with the current database. '
        if(initonly.eq.0)then
          CALL PHELPD('dbm opening',4,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'dfs')then
        H(1)='The domain flow solver supports separate solution'
        H(2)='of CFD domains (without thermal zones). If you want'
        H(3)='to do an integrated simulation use bps.'
        if(initonly.eq.0)then
          CALL PHELPD('dfs opening',3,'-',0,0,IER)
        endif
      elseif(module(1:4).eq.'ecnv')then
        h(1)='The module ecnv converts between different application'
        h(2)='files e.g. esp-r geometry to DXF. '
        if(initonly.eq.0)then
          CALL PHELPD('ecnv opening',2,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'e2r')then
        H(1)='This application converts an ESP-r model definition'
        H(2)='into a form which can be interpreted by the lighting '
        H(3)='analysis package RADIANCE.  It does so by creating  '
        H(4)='descriptive files and executing RADIANCE modules as  '
        H(5)='required. Depending on the type of monitor (mono/grey/'
        H(6)='colour) certain RADIANCE modules will be excluded. '
        H(7)=' '
        H(8)='Although e2r acts as a `desktop`, driving RADIANCE for'
        H(9)='several common tasks, the user should have some'
        H(10)='knowledge of RADIANCE to go beyond the `default`.'
        H(11)='There are times when the descriptive files may need '
        H(12)='to be modified - to add textures, vary surface  '
        H(13)='colours etc. You can access an editor from within e2r'
        H(14)='to accomplish this. '
        H(15)=' '
        H(16)='Choose the generate/ edit option to create a Radiance'
        H(17)='model, then view the scene via the calculate option.'
        H(18)=' '
        H(19)='You must have the radiance executables folder '
        H(20)='in your path.'
        if(initonly.eq.0)then
          CALL PHELPD('e2r opening',20,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'eco')then
        H(1)='This application support an ecobalance analsyis'
        H(2)='of the current model. It is currently underdevelopment.'
        H(3)='Contact Stephane.Citherlet@epfl.ch for details. '
        if(initonly.eq.0)then
          CALL PHELPD('eco opening',3,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'grd')then
        H(1)='    This is the main controller for the 1D and '
        H(2)='multi-D building construction, and ground gridding.'
        H(3)=' '
        H(4)='In order to activate this menu the problem should'
        H(5)='be defined first. The available options are:'
        H(6)='a  show the 1D building constructions gridding menu.'
        H(7)='b  show the multi-D constructions gridding menu.'
        H(8)='c  show the ground gridding menu.'
        if(initonly.eq.0)then
          CALL PHELPD('grd opening',8,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'ish')then
        H(1) ='The shading and insolation module (ish) provides'
        H(2) ='facilities to calculate shading patterns on exterior'
        H(3) ='surfaces and the insolation distribution across the'
        H(4) ='inside surfaces of a zoneroom. '
        H(5) =' '
        H(6) ='It consults the model configuration file for zone'
        H(7) ='geometry and shading obstructions and generates a'
        H(8) ='file of temporal patterns to be used in simulations.'
        H(9) =' '
        H(10)='Both interactive analysis or silent recalculation modes'
        H(11)='may be invoked.'
        H(12)=' '
        H(13)='The site plan view is useful in order to check that'
        H(14)='obstruction blocks are correctly positioned, while'
        H(15)='views from the sun provide an experiential way to'
        H(16)='assess shading patterns.'
        H(17)=' '
        H(18)='Note: Shading deals with external obstructions'
        h(19)='and surfaces, while insolation deals with the'
        H(20)='illumination patterns on internal surfaces caused'
        H(21)='by sunlight penetrating windows. Remember that the'
        H(22)='the insolation results can be affected by window'
        H(23)='shading.'
        H(24)=' '
        H(25)='While an ASHRAE window shading algorithm is available,'
        H(26)='its results are not yet saved to the Zone Shading &'
        H(27)='Insolation file (although this may be achieved via the'
        H(28)='edit/import/export facility.'
        H(29)=' '
        H(30)='The edit/import/export facility works with existing'
        H(31)='Zone Shading & Insolation files only. You may edit'
        H(32)='hourly data for one or more surfaces, export monthly'
        H(33)='shading data to a text file (insolation export is not'
        H(34)='yet available). You may also import data from an'
        H(35)='appropriately formatted text file.'
        if(initonly.eq.0)then
          CALL PHELPD('ish opening',35,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mfs')then
        H(1) ='This module is a stand-alone solver for network flow'
        H(2) ='problems.  It requires a network flow description'
        H(3) ='as well as weather and pressure distribution'
        H(4) ='databases.    '
        if(initonly.eq.0)then
          CALL PHELPD('mfs opening',4,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mld')then
        h(1)='This is the main menu for mould growth results'
        h(2)='analysis. The user can examine the growing'
        h(3)='conditions for a particular mould from the results'
        h(4)='of a building simulation (with combined heat and'
        h(5)='moisture transfer).'  
        h(6)='Statistics - reports the time conditions are'
        h(7)='suitable for the growth of a particular mould.'
        if(initonly.eq.0)then
          CALL PHELPD('mld opening',7,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'mrt'.or.module(1:3).eq.'vwf')then
        H(1)='MRT attrib & calc: Reads a viewfactor file which'
        H(2)='  includes the definition of MRT sensors within a'
        H(3)='  zone.  The user may calculate viewfactors for the'
        H(4)='  sensors or use previously computed viewfactors to'
        H(5)='  be used for re-evaluation of position dependent'
        H(6)='  MRT with different sets of surface temperatures.'
        H(7)='  '
        H(8)='View factor calc: Calculate viewfactors for all'
        H(9)='  surfaces of the input geometry. '
        H(10)=' '
        H(11)='Merge: Write zone viewfactors to file. '
        H(12)=' '
        H(13)='Calc Parameters: Change number of bubbles on a'
        H(14)='  surface, or number of patches on bubble. '
        H(15)=' '
        H(16)='Display surface info: list out the attributes of'
        H(17)='  the surfaces [depending on trace verbosity] '
        if(initonly.eq.0)then
          CALL PHELPD('vwf opening',17,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'pdb')then
        h(1) =' The MENU commands have the following meaning :'
        h(2) =' '
        h(3) =' HELP    this facility.'
        h(4) =' '
        h(5) =' LIST    chronological, sequential, alphabetical or'
        h(6) ='         user-defined-order list of the database'
        h(7) =' '
        h(8) =' INSERT  allows plant components to be entered into'
        h(9) ='         the database'
        h(10)=' '
        h(11)=' DELETE  allows components to be deleted from'
        h(12)='         the database'
        h(13)=' '
        h(14)=' EDIT    enables component attributes to be'
        h(15)='         individually edited'
        h(16)=' '
        h(17)=' FINISH  terminates the current program run'
        if(initonly.eq.0)then
          CALL PHELPD('pdb opening',17,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'plt')then
        h(1) =' Menu "ESRUplt" commands are as follows :'
        h(2) =' 1:  initiates the input section of the program'
        h(3) ='     reading all data files and performing all'
        h(4) ='     time independant pre-simulation calculations.'
        h(5) ='     Menu picks "2" and "3" cannot be selected'
        h(6) ='     until the data has been successfully entered.'
        h(7) =' 2:  invokes the simulation section of the program'
        h(8) =' 3:  permits the re-specification of a climate file'
        h(9) =' t:  allows the user to retrieve selected results'
        h(10)='     from a subroutine trace facility. This facility'
        h(11)='     is for use primarily by the experienced user.'
        h(12)=' '
        h(13)=' w:  stops non-fatal warning messages being'
        h(14)='     printed. However, the total number of warning'
        h(15)='     messages will be output in the simulation'
        h(16)='     description prior to commencement of simulation.'
        h(17)=' ?:  invokes this and other descriptive messages'
        h(18)='     as an aid to inexperienced users. These '
        h(19)='     messages are a synopsis of, and should be used'
        h(20)='     in conjunction with, the ESP documentation set'
        h(21)='     "ESP-r A Building and Plant Energy Simulation'
        h(22)='     System".'
        if(initonly.eq.0)then
          CALL PHELPD('plt opening',22,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'prj')then
        H(1) ='This tool provides project management facilities for'
        H(2) ='the ESP-r system and related tools. Tutorials are'
        H(3) ='available - accessed via the `tutorial` button on'
        H(4) ='the right - that provide details on all aspects of'
        H(5) ='the definition, performance appraisal and evolution'
        H(6) ='of energy system models.'
        H(7) =' '
        H(8) ='In using the Project Manager, you might typically begin'
        H(9) ='by checking the system databases to see if they are'
        H(10)='appropriate to your requirements. If not, you will'
        H(11)='need to create one or more project-specific database.'
        H(12)='Thereafter you might proceed by commencing a new'
        H(13)='project or, if you are new to ESP-r, by accessing an'
        H(14)='existing exemplar model and study its composition as'
        H(15)='an aid to understanding the different model parts and.'
        H(16)='options.'
        H(17)=' '
        H(18)='The menu options are as follows.'
        H(19)=' '
        H(20)=' `database maintenance` - to browse and edit a range of'
        H(21)='     databases that underpin the model.'
        H(22)=' `validation testing` - to initiate standard tests to'
        H(23)='     ensure that the current version of ESP-r is valid.'
        H(24)=' `open existing` - access an existing model, perhaps'
        H(25)='     one of the on-line exemplar models.'
        H(26)=' `create new` - create a model from scratch.'
        H(27)=' `import CAD file` - import of geometry from a CAD file'
        H(28)=' `export current model` - export of model to format'
        H(29)=' `   required by several 3rd party modelling systems.'
        H(30)=' `archive current model` - archive model (if owned by'
        H(31)='     user.'
        H(32)=' `folders & files` - lists the location and content of'
        H(33)='     the file set associated with the current model.'
        H(34)=' `save model` - writes out all changes held in memory'
        H(35)='     to the current model files.'
        H(36)=' `save model as` - writes out all changes held in'
        H(37)='     memory new, user specified model files.'
        H(38)=' `feedback` - a toggle that controls the verbosity'
        H(39)='     level of user messages.'
        H(40)='  `preferences` - this function is not yet available.'
        H(41)=' '
        H(42)='After a model is created or an existing one loaded, the'
        H(43)='options in the `Current model` section have the'
        H(44)='following meaning.'
        H(45)=' '
        H(46)=' `cfg` - the name of the current system configuration'
        H(47)='     file.'
        H(48)=' `path` - the path to the folder where the cfg file'
        H(49)=' `   is located.'
        H(50)=' `root` - the root name of the model from which the'
        H(51)='     name of principal model files is derived.'
        H(52)=' `title` - the model description.'
        H(53)=' `variants` - a facility to create new models as'
        H(54)='     variants of the current model (perhaps in support'
        H(55)='     of design options appraisal).'
        H(56)=' `browse/edit/simulate` - access to options that allow'
        H(57)='     the model to be browsed, edited (but only if'
        H(58)='     owned) or simulated.'
        if(initonly.eq.0)then
          CALL PHELPD('prj opening',58,'-',0,0,IER)
          H(1)='While the Project Manager is normally run in graphics'
          H(2)='mode, it may also be started in text/script mode or'
          H(3)='passed a model at invocation time. Type `esp-r -help`'
          H(4)='at the command prompt for options.'
          H(5)=' '
          H(6)='The `window` button at lower right allows the the font'
          H(7)='size of different areas of the Project Mamager to be'
          H(8)='changed as long as environment variables EFONT_0,'
          H(9)='EFONT_1, EFONT_2 and EFONT_3 have been associated with'
          H(10)='available fixed width fonts.'
          CALL PHELPD('prj opening',10,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'pro')then
        H(1)= '     ESP-r Standard Profiles Database '
        H(2)= '   ================================ '
        H(3)= ' '
        H(4)= ' The data requirements are summarised below :- '
        H(5)= ' '
        H(6)= ' for each profile - number of events.'
        H(7)= ' '
        H(8)= ' for each event - start and finish hours'
        H(9)= '                  percentage of first magnitude '
        H(10)='                  percentage of second magnitude '
        H(11)=' '
        H(12)='Currently you may add new profiles at the end of the'
        H(13)='database or, for existing profiles alter event con- '
        H(14)='tents but not the number of events. '
        H(15)=' '
        H(16)='Profiles in this database are currently used to  '
        H(17)='assist in the defintion of zone operations.  The'
        H(18)='standard is found in /usr/esru/esp-r/databases '
        if(initonly.eq.0)then
          CALL PHELPD('pro opening',18,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'res')then
        H(1)='Results analysis is the simulation results '
        H(2)='recovery facility for ESP-r. '
        H(4)=' '
        H(5)='It always begins with the specification of a results'
        H(6)='library which may contain results for one or more '
        H(7)='simulations performed by the Simulator. '
        H(8)=' '
        H(9) ='The symbols >> in a menu denotes a toggle selection,'
        H(10)='the inclusion of a [?] in a prompt means that a'
        H(11)='default and explanation of the item is available.'
        H(12)=' '
        H(13)='Current menu options are: '
        H(14)=' 1 select result file - select this if the results'
        H(15)='   file needs to be changed. Not used very often'
        H(16)='   as res is usually started with a specified file'
        H(17)='   name.'
        H(18)=' 2 select result set - if more than one assessment'
        H(19)='   is stored in the file (perhaps for a different'
        H(20)='   period) you can re-select using this facility.'
        H(21)=' 3 define output period - allows you to `zoom in`'
        H(22)='   on a portion of an assessment.'
        H(23)=' 4 select zones - allows you to select one or more'
        H(24)='   zones to include in a graph or report.'
        H(25)=' a graphs - includes a range of graph types e.g.'
        H(26)='   variable(s) vs time, frequency binns.'
        H(27)=' c timestep reports - tabular listings of data at'
        H(28)='   each timestep over the selected period.'
        H(29)=' d enquire about - e.g. statistics, energy balances,'
        H(30)='   hours over/under, frequency binn tables'
        H(31)=' e plant results - anything to do with detailed'
        H(32)='   system components and their performance metrics.'
        H(33)=' f indoor environmental quality - comfort and air'
        H(34)='   quality issues'
        H(35)=' g electrical results - if an electical domain is'
        H(36)='   included in the model all reports/graphs are here.'
        H(37)=' h CFD - if there is a CFD domain, 2D and 3D graphs'
        H(38)='   and images at slices can be produced here.'
        if(initonly.eq.0)then
          CALL PHELPD('res opening',38,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'tdf')then
        H(1)='The ESRU Temporal Definition Facility TDF supports the'
        H(2)='creation and management of temporal data from test'
        H(3)='cells, experiments or 3rd party software.  '
        H(4)=' '
        H(5)='TDF can also hold scheduling specifications for short'
        H(6)='timestep casual gains, air movement and boundary '
        H(7)='conditions for use by the ESP-r suite. It is also useful'
        H(8)='for data that changes at each timestep or in ways that'
        H(9)='are difficult for normal schedules. '
        H(10)=' '
        H(11)='Preferences : file names & types, periods, import/export'
        H(12)='             files & date display.'
        H(13)='Save : Update the TDF file and/or convert between ascii.'
        H(14)='       and binaray versions. '
        H(15)=' '
        H(16)='Currently the following item types are offered: '
        H(17)=' ALLCLMT - all climate data (includes next 6 items)'
        H(18)=' BDTEXTT - ambient temperature'
        H(19)=' WINDVLT - wind velocity'
        H(20)=' WINDDRT - wind direction'
        H(21)=' RELHUMT - ambient relative humidity'
        H(22)=' DIFHSLT - diffuse horizonal radiation'
        H(23)=' GLOHSLT - global horizonal radiation'
        H(24)=' DIRSOLT - direct normal solar radiation'
        H(25)=' SETPTTT - control setpoint temperature'
        H(26)=' CASUALT - zone casual gains (combined gains)'
        H(27)=' CASUAL3T -zone casual gains (Occup/Lights/SM Pwr)'
        H(28)=' ACTIVITY -global database: gains,setpoints, etc.'
        H(29)=' '
        H(30)='To create a new file, select option 1 and follow the'
        H(31)='instructions given. You will be asked for the year '        
        H(32)='and the period and the timesteps per hour and then '        
        H(33)='for an initial temporal type. '        
        H(34)=' '        
        H(35)='Insert by selecting an insertion point, the list will'
        H(36)='expand and you can select a generic type. '
        H(37)=' '        
        H(38)='Delete by selecting a slot and the list will contract.'
        H(39)=' '        
        H(40)='Copy by selecting source and then destination.'
        H(41)='The destination data will be overwritten - so you need'
        H(42)='to have created a destination item to overwrite before'        
        H(43)='you ask for a copy. '
        H(44)=' '
        H(45)='The TDF file is the main temporal file.'
        H(46)='The period shown is related to all tabular data'
        H(47)='and includes any `startup` time required by the'
        H(48)='simulation.  Example: for a simulation at half hour '
        H(49)='intervals over 10 days and with 2 startup days the '
        H(50)='temporal file should include 288 timesteps. '
        h(51)=' '
        h(52)='Note on file headers:'
        h(53)='Older asci temporal files may start with `TDFdatabase2`'
        h(54)='before using them please change 1st line to use the'
        h(55)='identifier ASCIITDF2'
        h(56)='Newer ascii files will start with `ASCIITDF3`.'
        h(57)='Also note that binary/scratch temporal files are'
        h(58)='machine dependant and are not always safe for use on'
        h(59)='another computer.'
        if(initonly.eq.0)then
          CALL PHELPD('tdf opening',59,'-',0,0,IER)
        endif
      elseif(module(1:3).eq.'vew')then
        H(1)= ' ? HELP     to list command options available'
        H(2)= ' Byebye     to exit'
        H(3)= ' /Input     to TYPE IN numerical values of E F or M '
        H(4)= ' Eye point  to pick the Eye-point with cursor'
        H(5)= ' Foc &Midp  to pick the Focus and Mid-points together'
        H(6)= ' Mid point  to pick the Mid-point alone'
        H(7)= ' Hidden     to remove hidden lines'
        H(8)= ' Dashed     to draw them dashed'
        H(9)= ' Visible    to draw them solid'
        H(10)=' Lens mm    to specify Lens focal length (in mm)'
        H(11)=' *Enlarg %  to specify Enlargement factor'
        H(12)=' Angle vew  to specify viewcone angle (degrees)'
        H(13)='       (negative for biggest picture that will fit)'
        H(14)='  '
        H(15)= ' 4 Orthogr  to select orthogonal projection'
        H(16)= ' 3 Perspec  to select perspective projection'
        H(17)= ' 2 || horz  to select parallel proj. onto horiz. plane'
        H(18)= ' 1 || vert  to select parallel proj. onto vert. plane'
        H(19)= ' Type all   to list all current view parameters'
        H(20)= ' Input all  to type in all view parameters'
        H(21)= ' Screen     to draw the picture'
        H(22)= ' Out file   to file it in a picture-file'
        H(23)= ' New file   to select a new input file'
        H(24)=' +Def vews  to select default views'
        if(initonly.eq.0)then
          CALL PHELPD('vew opening',24,'-',0,0,IER)
        endif
      elseif(module(1:4).eq.'wire')then
        h(1)= 'This interface provides control of the wireframe image'
        h(2)= 'of the model geometry. There are several sections as'
        h(3)= 'follows: '
        h(4)= ' View orientation - allows you to defined the eye point'
        h(5)= '   (where you are looking from), the view point (what'
        h(6)= '   you are looking at). Both of these can be edited by'
        h(7)= '   typing in new values or by using the up/down arrows'
        h(8)= '   to the right of each number.'
        h(9)= '   The view bounds can be toggled from optimum to use'
        h(10)='   a user specified angle of view. Reducing the angle'
        h(11)='   of view will act to zoom into the view.'
        h(12)=' '
        h(13)=' Display information toggles - this toggles off various'
        h(14)='   attributes (e.g. zone names, site grid).'
        h(15)=' '
        h(16)=' Display grid - controls whether a dotted grid is drawn'
        h(17)='   and the density of the grid.'
        h(18)=' '
        h(19)=' Zones to display - one box for each zone in the model'
        h(20)='   (unticking removes that zone from the view)'
        h(21)=' '
        h(22)=' Display options - three sets of options:'
        h(23)='   View type: pick one of these'
        h(24)='   Include: allows you to draw all or a subset of the'
        h(25)='     model geometry e.g. only surfaces facing outside.'
        h(26)='   Hilight: allows you to identify specific attributes'
        h(27)='     of entities by drawing them in a bold line.'
        h(28)=' '
        h(29)='To apply one or more changes click on the Apply button.'
        h(30)=' '
        h(31)='To apply one or more changes and close the wireframe '
        h(32)='control interface click on the OK button. '
        h(33)=' '
        h(34)='Note: a few of the selections are not yet operational. '
        if(initonly.eq.0)then
          CALL PHELPD('wirefram ctl',34,'-',0,0,IER)
        endif
      else
        H(1)='Did not find a matching ESP-r application name.'
        H(2)='So no one has linked this facility yet. '
        if(initonly.eq.0)then
          CALL PHELPD('ask about mismatch',2,'-',0,0,IER)
        endif
      endif

C Recover previous help via dh array.
      if(initonly.eq.0)then
        do 51 i=1,60
          h(i)= dh(i)
   51   continue
      endif
      return
      end

