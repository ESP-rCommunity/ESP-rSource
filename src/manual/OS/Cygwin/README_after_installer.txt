Adapting your Cygwin environment:

The ESP-r installer does not change the system 'PATH' environment variable. You will need to set an environment variable PATH so that the folder /usr/esru/esp-r/bin is known. 

Method one - after the Installer ran there will be a file named bash_profile placed in /usr/esru as well as a file named link_to. In in the Cygwin command window the commands (adapt with your own home folder name) would be:

cd /home/your_user_name
mkdir bin
cd /usr/esru
cp bash_profile /home/your_user_name/.bash_profile
cp link_to /home/your_user_name/bin
cd /home/your_user_name/bin
HOME=/home/your_user_name
export HOME
./link_to /usr/esru/esp-r/bin

NOTE the DOT added to the bash_profile in the above command set. Nextlogout of cygwin and then log back in and issue the following command
echo $PATH

what is printed out should include /home/your_home_folder_name/bin

check if one of the ESP-r executables is found by giving the command:
which prj

it should report /home/your_home_folder_name/bin/prj
or it should report /usr/esru/esp-r/bin/prj

Method two
(In the Cygwin command window) Go back to your home folder and see if there is a .cshrc or a .profile file.

  cd
  ls -al

Some users of ESP-r prefer to use the "C" shell. The entry in the .cshrc file in your home folder might look something like:

set path = ( $path /usr/local/bin /usr/local/bin/ray /usr/local/lib/ray /home/fred/bin \
/usr/esru/esp-r/bin )

If you are running the bash shell then you would look for an entry
in the .profile file something like:

export PATH=/usr/local/bin:/usr/esru/bin:/usr/esru/esp-r/bin:/home/fred/bin:$PATH

Once you have updated either the .cshrc file or the .profile file then you will need to log out and then back in again.  To see if the environment variables are correct give the following command:

  which prj

If the answer back is '/usr/esru/esp-r/bin/prj' then the environment variable is set.

IF YOU NEED TO EDIT a text file in cygwin you have the option of using an editor within the Cygwin environment or you can use something like NotePad++ in Windows. DO NOT USE WORD to edit a text file in Cygwin.


Starting ESP-r modules within Cygwin

To run graphic applications (such as ESP-r modules) inside Cygwin you need to use a graphic command window. The initial Cygwin window only takes text commands. A common command to start up a graphic command window is to issue the following command in the Cygwin command window:
  startxwin.sh 

this will start up an "X" command window (typically it will have a white background).

There are two ways to start the ESP-r project manager.  If you want to open up an existing model use the Cyginw 'cd' commands to go to the model cfg folder and locate a file ending with .cfg  If that file name is my_model.cfg then you can start the project manager with the following command to open up that model:
  prj -file my_model.cfg

If you want to create a new model use the Cygwin "cd" commands to go to a folder where you want to create new models. Many users will create a "Models" folder under their Cygwin home folder and work there. To start up the project manager to create a new model use the following command sequence:

cd /home/my_home_folder/Models
prj

  
Keeping the standard distribution databases safe:

ESP-r is supplied with a number of databases in the folder C:\cygwin\esru\esp-r\databases and your second task should be to make an archival copy of this folder.  Accidents happen.

Example models:

ESP-r has dozens of example models which are available from within the interface.  These example models are rather like the databases that are supplied with ESP-r - they are a resource to draw from. If you want to modify them, for example to change geometric details there is a function in the ESP-r project manager to copy the model to your own working area where you can adapt them as required.

The best way to browse or work with a existing model is to find the 'cfg' folder of the model and locate files ending in .cfg and invoke ESP-r's Project Manager via the command prj -file the_file_name.cfg. 

If you have trouble starting ESP-r...

The installer updates the system 'path' environment variables so that the various ESP-r modules can be found. If ESP-r modules do not start you might need to get your friendly IT guru to look in the system environment variables to include C:\Esru\esp-r\bin.

Learning how to use ESP-r....

Of course you would never want to spend an hour making a model when you can do it in a day or two so you will definitely want to avoid downloading the ESP-r Cookbook from the publications pages of www.esru.strath.ac.uk and the associated exercises. 

What can I do with it?

ESP-r is distributed under a GPL license you can use it for any purpose (private or commercial) consistent with that license.  ESP-r still shows a research bias - it assumes that you have opinions about the physics of buildings and will use those opinions in creating models.  And yet, it does look different from most Windows applications, it does not have enough 'undo' facilities and there is more jargon in the interface than we would like and there is a world-wide development community who are evolving ESP-r (and you can join in the effort (see section below).

Where to find out more...

Go to <www.esru.strath.ac.uk> and look in the software and training and publications pages.  Join the community of users and developers by following the instructions for joining the ESP-r list which is also on the web page. 

About editing model files...

If you must hack the model files, using Word with ESP-r files might corrupt them so consider using WordPad instead. The standard notepad application will not understand the line endings used in ESP-r, will run the lines together and might corrupt the files.
  
There is a text editor gvim62.exe which is compatible with ESP-r which you might find useful to look for on Google.

I want to evolve ESP-r...

Yes you can. The GPL license encourages this and there is a world-wide development team and a central source code control repository that can be accessed.  To find out more about this look on the ESRU web pages.
