Adapting your Cygwin environment:

The ESP-r installer does not change the system 'PATH' environment variable. You will need to set an environment variable PATH so that the folder /usr/esru/esp-r/bin is known. 

Method one - In the Cygwin command window (note alternative ./link_to commands):

  cd
  mkdir bin
  cp /opt/esru/bash_profile .bash_profile
  cp /opt/esru/bin/link_to bin
  HOME=/home/your_user_name
  export HOME
  cd bin
  ./link_to /opt/esru/esp-r/binX11

or
  ./link_to /opt/esru/esp-r/binGTK

If the link_to script fails to run it may be because it uses the csh command
interpretor. There is an alternative link_to_bash in /opt/esru/bin.

NOTE the DOT added to the bash_profile in the above command set. Next logout of cygwin and then log back in and issue the following command

  echo $PATH

what is printed out should include /home/your_home_folder_name/bin

check if one of the ESP-r executables is found by giving the command:
  which prj

it should report /home/your_home_folder_name/bin/prj


Method two
(In the Cygwin command window) Go back to your home folder and see if there is a .cshrc or a .profile file.

  cd
  ls -al

Some users of ESP-r prefer to use the "C" shell. The entry in the .cshrc file in your home folder might look something like:

  set path = ( $path /usr/local/bin /usr/local/bin/ray /usr/local/lib/ray /home/fred/bin \
    /opt/esru/esp-r/binX11 )

or
  set path = ( $path /usr/local/bin /usr/local/bin/ray /usr/local/lib/ray /home/fred/bin \
    /opt/esru/esp-r/binGTK )

If you are running the bash shell then you would look for an entry
in the .profile file something like:

export PATH=/usr/local/bin:/opt/esru/esp-r/binX11:/home/fred/bin:$PATH

Once you have updated either the .cshrc file or the .profile file then you will need to log out and then back in again.  To see if the environment variables are correct give the following command:

  which prj

If there is an answer then the environment is ok.

IF YOU NEED TO EDIT a text file in cygwin you have the option of using an editor within the Cygwin environment or you can use something like NotePad++ in Windows. DO NOT USE WORD to edit a text file in Cygwin.

Dependencies:

If ESP-r executables do not run it may be because dll (library) files are missing or are not the correct version. Here is a list of what you need for the X11 version of the simulator:
CygX11-6.dll, Cygwin1.dll, Cygxml2-2.dll, Cygxslt-1.dll, Cyggcc-S-1.dll, Cyggfortran-3.dll, Cygstdc++6.dll

Here is a list of what is required for the GTK version of the simulator:
Cygwin1.dll, Cyggdk-X11-2.0.dll, Cyggdk_Pixbuf-2.0-0.dll, Cygglib-2.0-0.dll, Cyggobject-2.0-0.dll, Cyggtk-X11-2.0-0.dll, Cygpango-1.0-0.dll, Cygxml2-2.dll, Cygxslt-1.dll, Cyggcc-S-1.dll, Cyggfortran-3.dll, Cygstdc++6.dll

To find about dependencies the tool dependency walker 2.2 sort of works (it expects dll to be in the same folder as the executable and does not know about Cygwin paths). You can find it at http://www.dependencywalker.com


Starting ESP-r modules within Cygwin

To run graphic applications (such as ESP-r modules) inside Cygwin you need to use a graphic command window. The initial Cygwin window only takes text commands. A common command to start up a graphic command window is to issue the following command in the Cygwin command window:
  startxwin 

this will start up an "X" command window (typically it will have a white background).

There are two ways to start the ESP-r project manager.  If you want to open up an existing model use the Cyginw 'cd' commands to go to the model cfg folder and locate a file ending with .cfg  If that file name is my_model.cfg then you can start the project manager with the following command to open up that model:
  prj -file my_model.cfg

If you want to create a new model use the Cygwin "cd" commands to go to a folder where you want to create new models. Many users will create a "Models" folder under their Cygwin home folder and work there. To start up the project manager to create a new model use the following command sequence:

  cd /home/my_home_folder/Models
  prj

  
Keeping the standard distribution databases safe:

ESP-r is supplied with a number of databases in the folder C:\cygwin\opt\esp-r\databases and your second task should be to make an archival copy of this folder.  Accidents happen.

Example models:

ESP-r has dozens of example models which are available from within the interface.  These example models are rather like the databases that are supplied with ESP-r - they are a resource to draw from. If you want to modify them, for example to change geometric details there is a function in the ESP-r project manager to copy the model to your own working area where you can adapt them as required.

The best way to browse or work with a existing model is to find the 'cfg' folder of the model and locate files ending in .cfg and invoke ESP-r's Project Manager via the command prj -file the_file_name.cfg. 

If you have trouble starting ESP-r...

Check you PATH environment variable within Cyginw. This is usually defined in the .profile or .bash_profile file. You home folder should have a bin folder in which you place links to the ESP-r executables. Use the link_to_bash script to set these up.

Learning how to use ESP-r....

Of course you would never want to spend an hour making a model when you can do it in a day or two so you will definitely want to avoid downloading the ESP-r Cookbook from the publications pages of www.esru.strath.ac.uk and the associated exercises. 

What can I do with it?

ESP-r is distributed under a GPL license you can use it for any purpose (private or commercial) consistent with that license.  ESP-r still shows a research bias - it assumes that you have opinions about the physics of buildings and will use those opinions in creating models.  And yet, it does look different from most Windows applications, it does not have enough 'undo' facilities and there is more jargon in the interface than we would like and there is a world-wide development community who are evolving ESP-r (and you can join in the effort (see section below).

Where to find out more...

Go to <www.esru.strath.ac.uk> and look in the software and training and publications pages.  Join the community of users and developers by following the instructions for joining the ESP-r list which is also on the web page. 

About editing model files...

If you must hack the model files use Notepad++ (a free download) or a native Cygwin text editor (nano or nedit). Word will mess up ESP-r model files.

I want to evolve ESP-r...

Yes you can. The GPL license encourages this and there is a world-wide development team and a central source code control repository that can be accessed.  To find out more about this look on the ESRU web pages.
