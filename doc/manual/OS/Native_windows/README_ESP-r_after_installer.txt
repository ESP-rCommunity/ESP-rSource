Setting up associations:

The installer does not always create an association between the configuration file of an ESP-r model (files ending with .cfg) and the esp-r.cmd file located in C:\Esru\Models  If clicking on a model .cfg file does not startup the Project Manager, one of your first tasks should be to use the Windows Explorer tools->file associations to point to C:\Esru\Models\esp-r.cmd

The installer does not setup a Start menu item for ESP-r so don't go looking there (yet).

Keeping the standard distribution databases safe:

ESP-r is supplied with a number of databases in the folder C:\Esru\esp-r\databases and your second task should be to make an archival copy of this folder.  Accidents happen.

Example models:

ESP-r has dozens of example models which are available from within the interface.  These example models are rather like the databases that are supplied with ESP-r - they are a resource to draw from. If you want to modify them, for example to change geometric details there is a function in the ESP-r project manager to copy the model to your own working area where you can adapt them as required.

As an example of a project working area, we have placed a folder C:\Esru\Models.  We suggest you start new models here. ESP-r still has a few glitches when it is working with folders which include spaces so to avoid frustration work in a place without spaces in the folder name.

The best way to browse or work with a existing model is to find the 'cfg' folder of the model and locate files ending in .cfg which you should be able to click on to invoke ESP-r's Project Manager. 

If you have trouble starting ESP-r...

The installer updates the system 'path' environment variables so that the various ESP-r modules can be found. If ESP-r modules do not start you might need to get your friendly IT guru to look in the system environment variables to include C:\Esru\esp-r\bin as well as C:\GTK\bin

Learning how to use ESP-r....

Of course you would never want to spend an hour making a model when you can do it in a day or two so you will definitely want to avoid downloading the ESP-r Cookbook from the publications pages of www.esru.strath.ac.uk and the associated exercises. 

What can I do with it?

ESP-r is distributed under a GPL license you can use it for any purpose (private or commercial) consistent with that license.  ESP-r still shows a research bias - it assumes that you have opinions about the physics of buildings and will use those opinions in creating models.  And yet, it does look different from most Windows applications, it does not have enough 'undo' facilities and there is more jargon in the interface than we would like and there is a world-wide development community who are evolving ESP-r (and you can join in the effort (see section below).

Where to find out more...

Go to <www.esru.strath.ac.uk> and look in the software and training and publications pages.  Join the community of users and developers by following the instructions for joining the ESP-r list which is also on the web page. 

About editing model files...

If you must hack the model files, using Word with ESP-r files might corrupt them so consider using WordPad instead. The standard notepad application will not understand the line endings used in ESP-r, will run the lines together and might corrupt the files.
  
There is a text editor gvim62.exe which is compatible with ESP-r which you might find useful to look for on Google.

Features not yet enabled for the native Windows version...

ESP-r is work in progress on Windows. We have not yet finished the click-on-bitmap geometry input facility. The icon based systems components network tool has not been ported.  And there is some work to be done to link in with native windows versions of the Radiance suite of visual simulation tools.

I want to evolve ESP-r...

Yes you can. The GPL license encourages this and there is a world-wide development team and a central source code control repository that can be accessed.  To find out more about this look on the ESRU web pages.
