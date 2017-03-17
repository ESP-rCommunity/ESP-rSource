#! /usr/bin/env python

# import_materialsAndConstructions_fromEplusModel.py version 4.1a.
# Scans an EnergyPlus idf file, and imports materials and constructions into ESP-r ASCII databases.
# Designed for use with EnergyPlus v8.5 models, other versions may not be supported.
# Will import classes "Material", "Material:NoMass", "Material:InfraredTransparent", "Material:AirGap",
# "WindowMaterial:SimpleGlazingSystem", "WindowMaterial:Glazing", "WindowMaterial:Gas" and "Construction":
#   - Material - All data except diffusion resistance (default value of 20.0 MNs/g.m is assumed) imported.
#   - Material:NoMass - Conductivity calculated from thermal resistance assuming default thickness of 0.1 m.
#     Values of density (10.0 kg/m^3) and specific heat capacity (10.0 J/kg.K) are assumed, to represent
#     minimal mass.  Emissivity and solar absorptivity imported, diffusion resistance from "fict" material
#     (19200.0 MNs/g.m) assumed.
#   - Material:InfraredTransparent - No data given in idf file, so these materials are imported as effectively
#     fictional materials using properties of the existing "fict" ESP-r material, but are considered opaque.
#   - Material:AirGap - ESP-r handles air gaps rather differently; the thermal resistance of these material
#     classes is remembered and associated with any constructions that reference them.  Default thickness of
#     0.1 m is assumed.
#   - WindowMaterial:SimpleGlazingSystem - Thermal conductivity is calculated to reach the required U value
#     assuming a thickness of 0.01 m.  Density (2710.0 kg/m^3), specific heat capacity (837.0 J/kg.K),
#     emissivity (0.83) and absorptance (0.05) are assumed to represent typical glass panes.  Optical properties
#     are all set to 0.0 as placeholders, though the required SHGC is noted in the material description.
#   - WindowMaterial:Glazing - Density (2710.0 kg/m^3), specific heat capacity (837.0 J/kg.K) and absorptance
#     (0.05) are assumed to represent typical glass panes.  Longwave transmission and colour rendering are set
#     to 0.0, but other optical properties are imported.
#   - WindowMaterial:Gas - Implemented as ordinary air gaps, with default thermal resistance (0.17 m^2.K/W)
#     assumed.  If the gas type attribute is not "Air" then the user will be warned of the potentially incorrect
#     assumption.  Thickness imported.
#   - Construction - So long as associated materials have all been imported successfully, these should be
#     imported faithfully.  Constructions will be skipped if a material is referenced that has not been imported,
#     and the user will be warned of this.
# This program does not create an optical properties database.
# Assumes that construction classes are below material classes in the idf file.
# Inverted construction associations are not held in idf files by default, so all constructions are marked as
#   "NONSYMMETRIC" and it is trusted that the EnergyPlus model has them correctly assigned.
# The script will give warnings if it detects air gap materials on outside layers of constructions, but it is
#   left to the user to remedy these situations appropriately.
# Materials database is saved in the same directory as the idf file passed as an argument, named
# "Eplus_imported.materialdb".
# Constructions database is saved in the same directory as the idf file passed as an argument, named
# "Eplus_imported.constrdb".

# Possible future updates:
# Extend to import optical database?
# Develop for option to add to existing databases?
# Consider more robust handling of constructions in respect of number of layers and IRT materials?
#   (maybe examine how this is handled in the EnergyPlus code?)

# Command line options:
#     None

# Command line arguments:
#     1 - String containing path to EnergyPlus model file
#         (if it does not have an extension ".idf" program will query your choice).

import sys, os
import datetime
from subprocess import call

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
s_idfFile=sys.argv[1]
if not os.path.isfile(s_idfFile): sys.exit('Error: input file "'+s_idfFile+'" does not exist. Exiting.')
if not s_idfFile[-4:]=='.idf':
    if not ask_YorN('Input file "'+s_idfFile+'" appears not to be an idf file. Continue?'): sys.exit('Exiting.')

# Main program.

ls_pth=s_idfFile.split('/')
if len(ls_pth)>1:
    s_pth='/'.join(ls_pth[:-1])+'/'
else:
    s_pth='./'

# Write ESP-r materials database header.
f_matDbs=open(s_pth+'Eplus_imported.materialdb','w')
curDateTime=datetime.datetime.now()
s_dateTime=curDateTime.strftime('%a %b %d %X %Y')
s_text=('*Materials 1.1\n'+
    '*date,'+s_dateTime+'\n'+
    '*doc,ESP-r materials imported from EnergyPlus model "'+s_idfFile+'" using program "import_materialsAndConstructions_fromEplusModel.py".\n'+
    '2  # number of classifications\n'+
    '#  \n'+
    '# Materials have the following attributes:\n'+
    '#  conductivity (W/(m-K), density (kg/m**3) specific heat (J/(kg-K)\n'+
    '#  emissivity out (-) emissivity in (-)#   absorptivity out (-) absorptivity in (-)\n'+
    '#  diffusion resistance (MNs g^-1m^-1)\n'+
    '#  default thickness (mm)\n'+
    '#  flag [-] legacy [o] opaque [t] transparent\n'+
    '#       [g] gas or air gap\n'+
    '#  \n'+
    '# Transparent material additonal attributes:\n'+
    '#  longwave tran (-) solar direct tran (-) solar reflec out (-) solar refled in (-)\n'+
    '#  visable tran (-) visable reflec out (-) visable reflec in (-) colour rendering (-)\n'+
    '#  \n'+
    '# Gas material additional attributes:\n'+
    '#  air gap resistance for vert horiz other\n'+
    '#  \n'+
    '# class index |nb items|description (32 char)\n'+
    '*class, 1,uniqueString_numMats,Imported\n'+
    'Category ( 1) Imported contains standard materials imported from an EnergyPlus model.\n')
f_matDbs.write(s_text)

# Write ESP-r constructions database header.
f_conDbs=open(s_pth+'Eplus_imported.constrdb','w')
s_text=('*CONSTRUCTIONS,1.0 # multilayer constructions\n'+
    '*Text\n'+
    'Automatically extracted from and EnergyPlus IDF file.\n'+
    '*End_text\n'+
    '*date,'+s_dateTime+'\n'+
    '*Category,general constructions,general constructions,Category general constructions includes all of the MLC.\n'+
    '#\n')
f_conDbs.write(s_text)

# Scan through idf file.
f_idfFile=open(s_idfFile,'r')
found=False
found_noMass=False
found_IRT=False
found_airGap=False
found_con=False
found_SGS=False
found_glaz=False
found_wgas=False
b_isEndLine=False
linecount=0 # one referenced.
matcount=0 # zero referenced, but material 0 already exists (ESP-r placeholder air gap).
concount=0 # one referenced.
layercount=0 # one referenced.
# These lists are to easily retrieve the ...
ls_matNames=['[air gap]',] # ... numeric material reference.
ls_matThick=['[n/a]',]     # ... thickness.
ls_matDesc=['Air layer with default properties assumed for legacy treatment of air (index zero).',]  # ... material description.
ls_gapNames=[]           # ... gap material numeric reference.
li_gapTypes=[]           # ... whether gaps are in solid (1) or glazed (2) constructions.
ls_gapValues=[]         # ... gap thermal resistance or thickness, depending on type.

# Get name of idf file without path.
s_idfFileOnly=(s_idfFile.split('/')).pop()

# Parameterise truncation limits.
i_matTrunc=30 # material name
i_conTrunc=30 # construction name

# Debug ("i"): limit to 3000 lines.
#i=0
for s_line in f_idfFile:
    #i=i+1
    #if i>3000: break

# Material found. Scan values, place into list and output to ESP-r database when finished.
# Lines: 1 name, 2 roughness, 3 thickness (M), 4 conductivity (W/m K), 5 density (kg/m^3), 6 specific heat (J/kg K),
# 7 thermal absorptance, 8 solar absorptance, 9 visible absorptance.
    if found:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_matNames.append(s_val)
            ls_matDesc.append(s_val+' material imported from Eplus model '+s_idfFileOnly+'.')
# Truncate name if greater than i_matTrunc characters.
            lls_text[0][1]=s_val[:i_matTrunc]
            lls_text[0][4]=ls_matDesc[-1]
        elif linecount==3:
            ls_matThick.append(s_val)
            lls_text[1][8]=str(float(s_val)*1000.0) # convert to mm for materials db.
        elif linecount==4:
            lls_text[1][0]=s_val
        elif linecount==5:
            lls_text[1][1]=s_val
        elif linecount==6:
            lls_text[1][2]=s_val
        elif linecount==7:
            lls_text[1][3]=s_val
            lls_text[1][4]=s_val
        elif linecount==8:
            lls_text[1][5]=s_val
            lls_text[1][6]=s_val
# Now have all the information we need, reset counts and logicals in preparation for scanning
# for the next material, and write the material to the ESP-r database.
            linecount=0
            found=False
# Debug: write entries to standard out.
            #print ','.join(lls_text[0])
            #print ','.join(lls_text[1])
            f_matDbs.write(','.join(lls_text[0])+'\n')
            f_matDbs.write(','.join(lls_text[1])+'\n')

# "no mass" material found. Use thermal resistance to calculate conductivity,
# scan emissivity and absorptivity.
    elif found_noMass:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_matNames.append(s_val)
            ls_matDesc.append(s_val+' "no mass" material imported from Eplus model '+s_idfFileOnly+'.')
            ls_matThick.append('0.1')
            lls_text[0][1]=s_val[:i_matTrunc]
            lls_text[0][4]=ls_matDesc[-1]
        elif linecount==3:
# Assuming thickness of 0.1m, use thermal resistance to calculate conductivity.
            d_cond=0.1/float(s_val)
# Limit to a minimum of 0.01.
            if d_cond<0.01: d_cond=0.01
            lls_text[1][0]='{:.3f}'.format(d_cond)
        elif linecount==4:
            lls_text[1][3]=s_val
            lls_text[1][4]=s_val
        elif linecount==5:
            lls_text[1][5]=s_val
            lls_text[1][6]=s_val
# Finished reading, reset counts and logicals and write entry to database.
            linecount=0
            found_noMass=False
            f_matDbs.write(','.join(lls_text[0])+'\n')
            f_matDbs.write(','.join(lls_text[1])+'\n')

# Infrared transparent material found. This is a special case; only the name is held in the
# idf file, so add this to the standard material name list and write a "fictitious" material
# into the materials database with the given name, to represent it.
    elif found_IRT:
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        ls_matNames.append(s_val)
        ls_matDesc.append(s_val+' infrared transparent (effectively fictitious) material imported from Eplus model '+s_idfFileOnly+'.')
        ls_matThick.append('0.004')
        lls_text[0][1]=s_val[:i_matTrunc]
        lls_text[0][4]=ls_matDesc[-1]
        f_matDbs.write(','.join(lls_text[0])+'\n')
        f_matDbs.write(','.join(lls_text[1])+'\n')
        found_IRT=False

# Air gap. Scan name and thermal resistance, store for later retrieval.
    elif found_airGap:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_gapNames.append(s_val)
            li_gapTypes.append(1)
        elif linecount==2:
            ls_gapValues.append(s_val)
# Finished reading, reset counts and logicals.
            linecount=0
            found_airGap=False

# Found simple glazing system material. Scan name and required U value and SHGC, calculate thermal
# conductivity and write placeholder entry to materials database.
    elif found_SGS:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_matNames.append(s_val)
            ls_matThick.append('0.01')
# Truncate name if greater than i_matTrunc characters.
            lls_text[0][1]=s_val[:i_matTrunc]
# Don't write description until we have SHGC value.
        elif linecount==2:
# This is the U value - use to calculate effective thermal conductivity assuming a thickness of 0.1m.
            d_Uvalue=float(s_val)
            lls_text[1][0]='{:.4f}'.format(d_Uvalue*0.02)
        elif linecount==3:
# This is the SHGC value, note in the description.
            ls_matDesc.append(ls_matNames[-1]+' simple glazing system (SHGC='+s_val+') material imported from Eplus model '+s_idfFileOnly+'.')
            lls_text[0][4]=ls_matDesc[-1]
# Finished reading, reset counts and logicals and write entry to database.
            linecount=0
            found_SGS=False
            f_matDbs.write(','.join(lls_text[0])+'\n')
            f_matDbs.write(','.join(lls_text[1])+'\n')

# Found glazing material. Scan data, assemble output list and write entry to database.
# Assume that "front" in idf file means outside face.
    elif found_glaz:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_matNames.append(s_val)
            ls_matDesc.append(s_val+' glazing material imported from Eplus model '+s_idfFileOnly+'.')
# Truncate name if greater than i_matTrunc characters.
            lls_text[0][1]=s_val[:i_matTrunc]
            lls_text[0][4]=ls_matDesc[-1]
        elif linecount==4:
            ls_matThick.append(s_val)
            lls_text[1][8]=str(float(s_val)*1000.0)
        elif linecount==5:
            lls_text[1][11]=s_val
        elif linecount==6:
            lls_text[1][12]=s_val
        elif linecount==7:
            lls_text[1][13]=s_val
        elif linecount==8:
            lls_text[1][14]=s_val
        elif linecount==9:
            lls_text[1][15]=s_val
        elif linecount==10:
            lls_text[1][16]=s_val
        elif linecount==12:
            lls_text[1][3]=s_val
        elif linecount==13:
            lls_text[1][4]=s_val
        elif linecount==14:
            lls_text[1][0]=s_val
# Finished reading, reset counts and logicals and write entry to database.
            linecount=0
            found_glaz=False
            f_matDbs.write(','.join(lls_text[0])+'\n')
            f_matDbs.write(','.join(lls_text[1])+'\n')

# Found glazing gap material. Check gas type attribute, and place name and thickness in lists for
# later retrieval.
    elif found_wgas:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
            ls_gapNames.append(s_val)
            li_gapTypes.append(2)
        elif linecount==2:
# Check gas type; if it is not "Air" then throw up a warning to the user of potentially incorrect
# assumptions.
            if not s_val=='Air':
                print ('Warning: window gap material "'+ls_gapNames[-1]+'" has gas type "'+s_val+'" not "Air",'+
                    ' assumed thermal resistance values (0.170) may be incorrect.')
        elif linecount==3:
            ls_gapValues.append(s_val)
# Finished reading, reset counts and logicals.
            linecount=0
            found_wgas=False

# Found construction. Scan name and layer material names (outside to inside, same as ESP-r luckily),
# construct appropriate listings and write to the ESP-r constructions database.
    elif found_con:
        linecount=linecount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip() 
        if s_line_strpd=='':
# Blank line, move on.
            continue   
        elif s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if linecount==1:
# Write header lines for the construction.
            s_nam=s_val[:i_conTrunc]
	    ls_text=['*item,'+s_nam+','+s_nam+' # tag name menu entry',]
	    ls_text.append('*itemdoc,'+s_nam+' is a ...')
	    ls_text.append('*incat,'+'general constructions')
	    ls_text.append('*type,OPAQ,OPAQUE,NONSYMMETRIC')
        else:
            layercount=layercount+1

# Test to see if the material name is in either the material or gap list; if it isnt, skip the construction.
            #print s_val
            if s_val in ls_matNames:
                i_ind=ls_matNames.index(s_val)
                ls_text.append('*layer,{},{:>.4f},{} : {}'.format(i_ind,float(ls_matThick[i_ind]),s_val[:i_matTrunc],ls_matDesc[i_ind]))
            elif s_val in ls_gapNames:
# First, check if this is the first or last layer. Throw up a warning if it is; this material will screw up an ESP-r simulation.
                if layercount==1 or b_isEndLine:
                    print 'Warning: construction "'+s_nam+'" has an air gap for an outside layer.'
                    print 'This construction will break an ESP-r simulation and requires your attention!'

# In EnergyPlus, gap materials in solid constructions appear to have no associated thickness. A default thickness of 0.1m is assumed.
                i_ind=ls_gapNames.index(s_val)
                if li_gapTypes[i_ind]==1:
                    d_resist=float(ls_gapValues[i_ind])
                    ls_text.append('*layer,0,0.1000,gap  {:.3f} {:.3f} {:.3f}'.format(d_resist,d_resist,d_resist))
                elif li_gapTypes[i_ind]==2:
                    d_thick=float(ls_gapValues[i_ind])
                    ls_text.append('*layer,0,{:.4f},gap  0.170 0.170 0.170'.format(d_thick))
                else:
                    print 'Warning: something went wrong with layer '+str(layercount)+' of construction "'+s_nam+'" (air gap, Eplus material "'+s_val+'").'
                    print 'Assuming default properties.'
                    ls_text.append('*layer,0,0.1000,gap  0.170 0.170 0.170')
            else:
                print 'Warning: material name "'+s_val+'" in construction "'+s_nam+'" not recognised. Skipping this construction.'
                found_con=False
                b_isEndLine=False
                linecount=0
                layercount=0
                concount=concount-1

# If line ended with a semicolon, construction done. Write tag line, reset counts and logicals and write entry
# to database.
            if b_isEndLine:
                ls_text.append('*end_item')
                layercount=0
                linecount=0
                found_con=False
                b_isEndLine=False
                f_conDbs.write('\n'.join(ls_text)+'\n')

# Scan for materials or constructions.
    else:
        s_val=s_line.strip()
        if s_val=='Material,':
            found=True
            matcount=matcount+1
            lls_text=[['*item','[name]',' '+str(matcount),' 1','[description]'],
                ['[conductivity]','[density]','[specific heat]','[emissivity]','[emissivity]','[absorptivity]','[absorptivity]','20.000','[thickness]','-']]
        elif s_val=='Material:NoMass,':
            found_noMass=True
            matcount=matcount+1
# As material is flagged as "no mass", use density, specific heat capacity and diffusion resistance of the "fict" ESP-r material.
# Assume a thickness of 0.1m.
            lls_text=[['*item','[name]',' '+str(matcount),' 1','[description]'],
                ['[conductivity]','10.000','10.000','[emissivity]','[emissivity]','[absorptivity]','[absorptivity]','19200.000','100.0']]
        elif s_val=='Material:InfraredTransparent,':
            found_IRT=True
            matcount=matcount+1
# The material data has been adapted from the "fict" material in the default ESP-r materials database.
            lls_text=[['*item','[name]',' '+str(matcount),' 1','[description]'],
                ['20.0000','10.000','10.000','0.990','0.990','0.010','0.010','19200.000','6.0','-']]
        elif s_val=='Material:AirGap,':
            found_airGap=True
        elif s_val=='Construction,':
            found_con=True
            concount=concount+1
            ls_text=['# layers  description  type  optics name   symmetry tag',]
        elif s_val=='WindowMaterial:SimpleGlazingSystem,':
            found_SGS=True
            matcount=matcount+1
            lls_text=[['*item','[name]',' '+str(matcount),' 1','[description]'],
                ['[conductivity]','2710.000','837.000','0.830','0.830','0.050','0.050','20.000','10.0','t',
                '0.000','0.000','0.000','0.000','0.000','0.000','0.000','0.000']]
        elif s_val=='WindowMaterial:Glazing,':
            found_glaz=True
            matcount=matcount+1
            lls_text=[['*item','[name]',' '+str(matcount),' 1','[description]'],
                ['[conductivity]','2710.000','837.000','[emissivity]','[emissivity]','0.050','0.050','20.000','[thickness]','t',
                '0.000','[solar_trans]','[solar_reflect_out]','[solar_reflect_in]','[visible_trans]','[visible_reflect_out]','[visible_reflect_in]','0.000']]
        elif s_val=='WindowMaterial:Gas,':
            found_wgas=True

# Write the placeholder air material and end line to materials database.
s_text=('# class index |nb items|description (32 char)\n'+
    '*class, 2, 1,GAPS\n'+
    'Category (2) GAPS holds the implied air material (legacy index of zero).\n'+
    '*item,air gap, 0, 2,Air layer with default properties assumed for legacy treatment of air (index zero).\n'+
    '0.0000,0.000,0.000,0.990,0.990,0.990,0.990,1.000,25.0,-\n'+
    '*end')
f_matDbs.write(s_text)
f_matDbs.close()

f_conDbs.write('*db_end\n')
f_conDbs.close()

# Substitute in the number of materials and constructions using sed.
call(['sed','-e','s/uniqueString_numMats/'+str(matcount)+'/g','-i',s_pth+'Eplus_imported.materialdb'])
call(['sed','-e','s/uniqueString_numCons/'+str(concount)+'/g','-i',s_pth+'Eplus_imported.constrdb'])

f_idfFile.close()
