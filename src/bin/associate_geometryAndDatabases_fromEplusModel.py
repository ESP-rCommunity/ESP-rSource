#! /usr/bin/env python

# associate_geometryAndDatabases_fromEplusModel.py version 4.1a.
# Places associations with the database and geometry files created by the import scripts,
#   into a cfg file.
# Will work with models in "registration only" or "building only" contexts.
# This script does not check whether databases and geometry files are valid.
# Assumes that existing cfg file has no zones, and is associated with default databases.
# Assumes that the materials database is located at "../dbs/Eplus_imported.materialdb".
# Assumes that the constructions database is located at "../dbs/Eplus_imported.constrdb".
# Assumes that all geometry files in the folder "../zones/" represent a desired zone,
#   and are named "[zone name].geo".
# Saves edited cfg file in the same directory as the cfg file passed as an argument,
# named "Eplus_imported.cfg".

# Command line options:
#     None

# Command line arguments:
#     1 - String containing path to ESP-r model configuration file 
#         (if it does not have an extension ".cfg" program will query your choice).

import sys, os
from glob import glob

def ask_YorN(s_desc):
# Part of APUF (Andy's Python Useful Functions).
# Ask yes or no question and return a logical.
# Argument is a string containing the question, the function automatically adds syntax guidance onto the end of this.
    while True:
        s_YorN=raw_input(s_desc+' Enter "y" or "n", or "e" to exit: ')
        if s_YorN=='y' or s_YorN=='Y': 
            return True
        elif s_YorN=='n' or s_YorN=='N': 
            return False
        elif s_YorN=='e' or s_YorN=='E': 
            sys.exit('Exiting.')
        elif s_YorN=='42':
            print 'Nice reference you hoopy frood, but try again.'
        else: 
            print 'Unrecognised input, please try again.'

# Parse command line.
s_cfgIn=sys.argv[1]
if not os.path.isfile(s_cfgIn): sys.exit('Error: input file "'+s_cfgIn+'" does not exist. Exiting.')
if not s_cfgIn[-4:]=='.cfg': 
    if not ask_YorN('Input file "'+s_cfgIn+'" appears not to be a cfg file. Continue?'): sys.exit('Exiting.')

ls_pth=s_cfgIn.split('/')
if len(ls_pth)>1:
    s_pth='/'.join(ls_pth[:-1])+'/'
else:
    s_pth='./'
f_cfgIn=open(s_cfgIn,'r')
f_cfgOut=open(s_pth+'Eplus_imported.cfg','w')

# Scan through cfg file line by line.
b_regOnly=False
i_prjCount=-1
for s_line in f_cfgIn:
    if s_line.strip()=='': continue
    ls_line=s_line.split()
# Scan for model stage index line.
    if ls_line[0]=='*indx':
        if ls_line[1]=='0': 
            b_regOnly=True
            f_cfgOut.write('*indx    1 # Building only\n')
        elif ls_line[1]=='1':
            f_cfgOut.write(s_line)
        else:
            print 'Warning: model context index "'+str(ls_line[1])+'" not recognised. Exiting.'
            sys.exit()
# Scan for materials database line.
    elif ls_line[0]=='*stdmat':
        f_cfgOut.write('*mat  ../dbs/Eplus_imported.materialdb\n')
# Scan for contruction database line.
    elif ls_line[0]=='*stdmlc':
        f_cfgOut.write('*mlc  ../dbs/Eplus_imported.constrdb\n')
# If registration only, scan for "* PROJ LOG" line and insert line "* Building" 2 lines after it.
    elif b_regOnly:
        if  len(ls_line)>1 and ls_line[1]=='PROJ':
            i_prjCount=1
            f_cfgOut.write(s_line)
        elif i_prjCount>0:
            i_prjCount=i_prjCount-1
            f_cfgOut.write(s_line)
        elif i_prjCount==0:
            i_prjCount=-1
            f_cfgOut.write('* Building\n')       
            f_cfgOut.write(s_line)
        else:
            f_cfgOut.write(s_line)
# Scan for zone listings, if not registration only.
    elif not b_regOnly and s_line.strip()=='0  # no of zones':
        ls_geoFiles=glob('../zones/*.geo')
        f_cfgOut.write('{:>7d}  # no of zones\n'.format(len(ls_geoFiles)))        
# reset number of zones       
        i_zonNum=0
        for s_geoFile in ls_geoFiles:
            i_zonNum=i_zonNum+1
            s_text=('*zon {:>3d}\n'.format(i_zonNum)+
                '*geo '+s_geoFile+'\n'+
                '*zend\n')
            f_cfgOut.write(s_text)
    else:
        f_cfgOut.write(s_line)

# If zones not found, dump zone listings, connections and AFN lines at the end.
if b_regOnly:
    ls_geoFiles=glob(s_pth+'../zones/*.geo')
    ls_geoFiles.sort()
    f_cfgOut.write('{:>7d}  # no of zones\n'.format(len(ls_geoFiles))) 
# reset number of zones       
    i_zonNum=0
    for s_geoFile in ls_geoFiles:
        i_zonNum=i_zonNum+1
        s_text=('*zon {:>3d}\n'.format(i_zonNum)+
            '*geo '+s_geoFile+'\n'+
            '*zend\n')
        f_cfgOut.write(s_text)
    f_cfgOut.write('*cnn Eplus_imported.cnn  # connections\n')
    f_cfgOut.write('   0   # no fluid flow network\n')

f_cfgIn.close()
f_cfgOut.close()

