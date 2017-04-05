#! /usr/bin/env python

# import_geometryAndAttribution_fromEplusModel.py version 4.1a.
# Scans through idf file and imports zones, geometry and attribution into ESP-r geometry files
# and connections file.
# Designed for use with EnergyPlus v8.5 models, other versions may not be supported.
# This script does not check whether entities conform to ESP-r limitations, such as maximum
#   number of surfaces or vertices.
# Will import classes "Zone", "BuildingSurface:Detailed" and "FenestrationSurface:Detailed".
# Assumes relative coordinate system and all zones have the same origin.
# Assumes counterclockwise vertex direction (same as ESP-r).
# Assumes "FenestrationSurface:Detailed" classes are after "BuildingSurface:Detailed" classses
#   in idf file.
# Outputs geometry files in the same directory as the idf file passed as an argument, named
# "[zone name].geo".
# Outputs connections file in the same directory as the idf file passed as an argument, named
# "Eplus_imported.cnn".

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

ls_pth=s_idfFile.split('/')
if len(ls_pth)>1:
    s_pth='/'.join(ls_pth[:-1])+'/'
else:
    s_pth='./'
f_idfFile=open(s_idfFile,'r')

# Parameterise truncation limits.
i_surfTrunc=12 # surface name
i_conTrunc=30 # material name
i_zonTrunc=12 # zone name

# Scan for zones.
b_foundZone=False
ls_zoneNames=[]
i_zoneCount=0
i_lineCount=0
b_isEndLine=False
lls_surfNames=[]   # double list of names (by zone and surface)
lls_surfTypes=[]   # double list of types (by zone and surface)
lls_surfCons=[]    # double list of constructions (by zone and surface)
llls_surfBounds=[] # tripple list of boundary conditions
lllls_surfVerts=[] # quad list of vertices (by ?? )
lls_surfParents=[] # double list of parents (by zone and surface)
for s_line in f_idfFile:
    
# Found zone class, get zone name and store in list.
    if b_foundZone:
        i_lineCount=i_lineCount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip()      
        if s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        elif s_line_strpd=='':
# Blank line, move on.
            continue
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

        if i_lineCount==1:
            ls_zoneNames.append(s_val.replace(" ", "-"))
# Zone name is all we need (assume all zones have the same origin), reset to look for next zone.
            i_lineCount=0
            b_foundZone=False
            b_isEndLine=False
            continue
        
        if b_isEndLine:
            i_lineCount=0
            b_foundZone=False
            b_isEndLine=False

# Search for zone classes.
    else:
        if s_line.strip()=='Zone,':
            b_foundZone=True
            i_zoneCount=i_zoneCount+1
            lls_surfNames.append([])
            lls_surfTypes.append([])
            lls_surfCons.append([])
            llls_surfBounds.append([])
            lllls_surfVerts.append([])
            lls_surfParents.append([])

f_idfFile.seek(0)

# Scan through for surfaces.
b_foundSurf=False
b_foundFen=False
i_lineCount=0
i_extraLines=0
b_isEndLine=False
i_totSurfs=0
li_zoneSurfs=[]
for i in range(i_zoneCount): li_zoneSurfs.append(0)
for s_line in f_idfFile:

# Found building surface. Scan name, type, construction, zone name and vertices and store in
# lists as appropriate.
    if b_foundSurf:
        i_lineCount=i_lineCount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip()      
        if s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        elif s_line_strpd=='':
# Blank line, move on.
            continue
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

# Surfaces may be "deactivated" (0 vertices), so remember attributes until we get 
# to the number of vertices (line 10) to make sure they need to be added to the lists.
        if i_lineCount==1:
            s_surfName=s_val.replace(" ", "-")
        elif i_lineCount==2:
            s_surfType=s_val
        elif i_lineCount==3:
# Construction name may be truncated
            s_surfCon=s_val[:i_conTrunc].replace(" ", "-")
        elif i_lineCount==4:
            s_zoneName=s_val.replace(" ", "-")
            i_zoneRef=ls_zoneNames.index(s_val.replace(" ", "-")) #inline comment
            li_zoneSurfs[i_zoneRef]+=1
        elif i_lineCount==5:
            s_surfBound=s_val
        elif i_lineCount==6:
            s_surfOther=s_val.replace(" ", "-")
        elif i_lineCount==10:
# Number of vertices. If this is 0, the surface is "deactivated" and it shouldn't be referenced elsewhere, 
# so skip it. If number of vertices is >0 then store data to lists and scan vertex coordinates.
            if s_val=='0':
                b_foundSurf=False
                i_lineCount=0
                b_isEndLine=False
                continue
            lls_surfNames[i_zoneRef].append(s_surfName)
            if s_surfType.upper()=='WALL': lls_surfTypes[i_zoneRef].append('VERT')
            elif s_surfType.upper()=='FLOOR': lls_surfTypes[i_zoneRef].append('FLOR')
            elif s_surfType.upper()=='CEILING' or s_surfType.upper()=='ROOF': lls_surfTypes[i_zoneRef].append('CEIL')
            else: 
                print 'Warning: surface type "'+s_surfType+'" not recognised for surface "'+s_surfName+'" in zone "'+s_zoneName+'". Setting attribute to "UNKN".'
                lls_surfTypes[i_zoneRef].append('UNKN')
            lls_surfCons[i_zoneRef].append(s_surfCon)
            if s_surfBound.upper()=='OUTDOORS': llls_surfBounds[i_zoneRef].append(['EXTERIOR','0','0'])
# Assume ground temperature profile 1.
            elif s_surfBound.upper()=='GROUND': llls_surfBounds[i_zoneRef].append(['GROUND','01','00'])
            elif s_surfBound.upper()=='SURFACE': 
# Look for referenced other side surface, if not found set as "UNKNOWN,0,0".
                i1_otherZoneRef=0
                b_found=False
                for ls_surfNames in lls_surfNames:
                    i1_otherZoneRef=i1_otherZoneRef+1
                    if s_surfOther in ls_surfNames: 
                        llls_surfBounds[i_zoneRef].append(['ANOTHER',str(i1_otherZoneRef),str(ls_surfNames.index(s_surfOther)+1)])
                        b_found=True
                        break
                if not b_found:
                    llls_surfBounds[i_zoneRef].append(['FINDME',s_surfOther,'0'])
            else:
                print ('Warning: boundary condition "'+s_surfBound+'" not recognised for surface "'
                    +s_surfName+'" in zone "'+s_zoneName+'". Setting attribute to "UNKNOWN,0,0".')
                llls_surfBounds[i_zoneRef].append(['UNKNOWN','0','0'])
            lls_surfParents[i_zoneRef].append('-')
            lllls_surfVerts[i_zoneRef].append([])
        elif i_lineCount>10:
# Now scanning vertices. These lines should come in threes; X coord, Y coord, Z coord.
# However, the three coords might be on the same line, i.e. [X],[Y],[Z],.
# Count the commas to decide.
            if s_val.count(',')==2:
                ls_val=s_val.split(',')
                lllls_surfVerts[i_zoneRef][-1].append(ls_val)
                i_extraLines=i_extraLines+1
            elif (i_lineCount-10-i_extraLines)%3==1:
# X coordinate, first one so increment count and add list for this vertex.
                lllls_surfVerts[i_zoneRef][-1].append([])
                lllls_surfVerts[i_zoneRef][-1][-1].append(s_val)
            else:
                lllls_surfVerts[i_zoneRef][-1][-1].append(s_val)  
          
        #print b_isEndLine
        if b_isEndLine:
            b_foundSurf=False
            i_lineCount=0
            i_extraLines=0
            b_isEndLine=False


# Found fenestration surface. Scan name, type, construction, parent surface name, boundary condition 
# and vertex coordinates, and store in lists as appropriate.
# and 
    elif b_foundFen:
        i_lineCount=i_lineCount+1
        s_line_strpd=s_line.strip()
# Strip out comments, denoted in idf by "!".
        s_line_strpd=s_line_strpd.split('!',1)[0]
        s_line_strpd=s_line_strpd.strip()      
        if s_line_strpd[-1]==',':
            s_val=s_line_strpd[0:-1]
        elif s_line_strpd[-1]==';':
# This is the end line, and ends in a semicolon instead.
            s_val=s_line_strpd[0:-1]
            b_isEndLine=True
        elif s_line_strpd=='':
# Blank line, move on.
            continue
        else:
            print 'Warning: could not detect line end ("," or ";") in line "'+s_line_strpd+'". Skipping this line.'
            continue

# Surfaces may be "deactivated" (0 vertices), so remember attributes until we get 
# to the number of vertices (line 10) to make sure they need to be added to the lists.

        if i_lineCount==1:
            s_surfName=s_val
        elif i_lineCount==3:
# Construction name may be truncated.
            s_surfCon=s_val[:i_conTrunc].replace(" ", "-")
        elif i_lineCount==4:
            s_surfParent=s_val.replace(" ", "-")
        elif i_lineCount==5:
            s_surfOther=s_val.replace(" ", "-")
        elif i_lineCount==10:
# Number of vertices. If this is 0, the surface is "deactivated" and it shouldn't be referenced elsewhere, 
# so skip it. If number of vertices is >0 then store data to lists and scan vertex coordinates.
            if s_val=='0':
                b_foundFen=False
                i_lineCount=0
                b_isEndLine=False
                continue

# Use parent surface name to find zone.
            i_zoneRef=-1
            b_found=False
            for ls_surfNames in lls_surfNames:
                i_zoneRef=i_zoneRef+1
                if s_surfParent in ls_surfNames:
                    i_surfRef=ls_surfNames.index(s_surfParent)
                    lls_surfParents[i_zoneRef].append(s_surfParent)
                    b_found=True
                    s_zoneName=ls_zoneNames[i_zoneRef]
                    break
            if not b_found: 
                print 'Warning: could not find parent surface "'+s_surfParent+'" for child "'+s_surfName+'". This will be written as a normal surface.'
                lls_surfParents[i_zoneRef].append('-')
            lls_surfNames[i_zoneRef].append(s_surfName)
# Inherit type from parent surface.
            if b_found: lls_surfTypes[i_zoneRef].append(lls_surfTypes[i_zoneRef][i_surfRef])
            else: lls_surfTypes[i_zoneRef].append('UNKN')
            lls_surfCons[i_zoneRef].append(s_surfCon)
# Guess that the assumption in EnergyPlus is that if no other side surface is given, assumed
# to be exterior.
            if s_surfOther=='': llls_surfBounds[i_zoneRef].append(['EXTERIOR','0','0'])
            else: 
# Look for referenced other side surface, if not found then set as "FINDME,[other surface name],0".
# This will be processed later to find the appropriate connection.
                i1_otherZoneRef=0
                b_found=False
                for ls_surfNames in lls_surfNames:
                    i1_otherZoneRef=i1_otherZoneRef+1
                    if s_surfOther in ls_surfNames: 
                        llls_surfBounds[i_zoneRef].append(['ANOTHER',str(i1_otherZoneRef),str(ls_surfNames.index(s_surfOther)+1)])
                        b_found=True
                        break
                if not b_found:
                    llls_surfBounds[i_zoneRef].append(['FINDME',s_surfOther,'0'])
                    print llls_surfBounds[i_zoneRef]
            lllls_surfVerts[i_zoneRef].append([])
        elif i_lineCount>10:
# Now scanning vertices. These lines should come in threes; X coord, Y coord, Z coord.
# However, the three coords might be on the same line, i.e. [X],[Y],[Z],.
# Count the commas to decide.
            if s_val.count(',')==2:
                ls_val=s_val.split(',')
                lllls_surfVerts[i_zoneRef][-1].append(ls_val)
                i_extraLines=i_extraLines+1
            elif (i_lineCount-10-i_extraLines)%3==1:
# X coordinate, first one so increment count and add list for this vertex.
                lllls_surfVerts[i_zoneRef][-1].append([])
                lllls_surfVerts[i_zoneRef][-1][-1].append(s_val)
            else:
                lllls_surfVerts[i_zoneRef][-1][-1].append(s_val)  
          
        #print b_isEndLine
        if b_isEndLine:
            b_foundFen=False
            i_lineCount=0
            b_isEndLine=False

# Search for surface classes.
    else:
        if s_line.strip()=='BuildingSurface:Detailed,':
            b_foundSurf=True
            i_totSurfs+=1
        elif s_line.strip()=='FenestrationSurface:Detailed,':
            b_foundFen=True
            i_totSurfs+=1

# Find and resolve any boundary conditions that couldn't be resolved while reading.
for i_1 in range(len(llls_surfBounds)):
    for i_2 in range(len(llls_surfBounds[i_1])):
        if llls_surfBounds[i_1][i_2][0]=='FINDME':
            s_surfOther=llls_surfBounds[i_1][i_2][1]
            i1_otherZoneRef=0
            b_found=False
            for ls_surfNames in lls_surfNames:
                i1_otherZoneRef+=1
                if s_surfOther in ls_surfNames:
                    llls_surfBounds[i_1][i_2]=['ANOTHER',str(i1_otherZoneRef),str(ls_surfNames.index(s_surfOther)+1)]
                    break

# Debug: write lists to standard out.
i_zoneRef=-1
for s_zoneName in ls_zoneNames:
    i_zoneRef=i_zoneRef+1
    print 'Zone name: '+s_zoneName
    print lls_surfNames[i_zoneRef]
    print lls_surfTypes[i_zoneRef]
    print lls_surfCons[i_zoneRef]
    print llls_surfBounds[i_zoneRef]
    print lllls_surfVerts[i_zoneRef]
    print lls_surfParents[i_zoneRef]
    print 'zone surfaces ',li_zoneSurfs[i_zoneRef]
    print 'total surfaces ',i_totSurfs
    print ''

f_idfFile.close()

# Reorder lists into alphabetical order by zone name.
# Do this by getting a sorted list of names, then using the sorted and unsorted
# lists to derive a mapping for the other lists.
l_sorted=sorted(ls_zoneNames)
li_mappings=[ls_zoneNames.index(a) for a in l_sorted]
#print ls_zoneNames,l_sorted,li_mappings
ls_zoneNames=l_sorted
sortIt=lambda l,mappings: [l[i] for i in mappings]
lls_surfNames=sortIt(lls_surfNames,li_mappings)
lls_surfTypes=sortIt(lls_surfTypes,li_mappings)
lls_surfCons=sortIt(lls_surfCons,li_mappings)
llls_surfBounds=sortIt(llls_surfBounds,li_mappings)
lllls_surfVerts=sortIt(lllls_surfVerts,li_mappings)
lls_surfParents=sortIt(lls_surfParents,li_mappings)

# Translate data into format compatible with ESP-r and output geometry files.

# Loop over zones.
for s_zoneName,ls_surfNames,ls_surfTypes,ls_surfCons,lls_surfBounds,llls_surfVerts,ls_surfParents in zip(
    ls_zoneNames,lls_surfNames,lls_surfTypes,lls_surfCons,llls_surfBounds,lllls_surfVerts,lls_surfParents):
    
# First, assemble lists of vertices (non-repeating) and surface associations.
    lls_geoVerts=[]
    lls_geoSurfs=[]
    li_childInds=[]
    i_surfInd=-1
    for lls_surfVerts,s_surfParent in zip(llls_surfVerts,ls_surfParents):
        i_surfInd=i_surfInd+1
        li_childInds.append(0)
        lls_geoSurfs.append(['*edges','',])
        for ls_surfVerts in lls_surfVerts:
            #print ls_surfVerts
            ls_geoVerts=['*vertex','{:.5f}'.format(float(ls_surfVerts[0])),'{:.5f}'.format(float(ls_surfVerts[1])),'{:.5f}'.format(float(ls_surfVerts[2]))]
            if ls_geoVerts in lls_geoVerts:
                lls_geoSurfs[-1].append(str(lls_geoVerts.index(ls_geoVerts)+1))
            else:
                lls_geoVerts.append(ls_geoVerts)
                lls_geoSurfs[-1].append(str(len(lls_geoVerts)))
# If surface has a parent, need to add vertices to the parent to go around child.
# Impose the following convention for each child: add the first parent vertex onto 
# the end of the parent vertex list, then append the child vertex list in reverse 
# order, but with the last vertex (after reversal) added onto the beginning.
        if not s_surfParent=='-':
            i_parentRef=ls_surfNames.index(s_surfParent)
            li_childInds[i_parentRef]=li_childInds[i_parentRef]+1
            lls_geoSurfs[i_parentRef].append(lls_geoSurfs[i_parentRef][2])
            ls_childVertRefs=lls_geoSurfs[-1][2:]
            ls_childVertRefs.reverse()
            ls_childVertRefs.insert(0,ls_childVertRefs[-1])
            lls_geoSurfs[i_parentRef]=lls_geoSurfs[i_parentRef]+ls_childVertRefs
# Rename child surfaces after their parent with an additional "child index" appended.
            ls_surfNames[i_surfInd]=(ls_surfNames[i_parentRef]+'_'+str(li_childInds[i_parentRef]))

# Add number of vertices into surface association list.
    for i in range(0,len(lls_geoSurfs)):
        lls_geoSurfs[i][1]=str(len(lls_geoSurfs[i])-2)

# Assemble attribute list.
    lls_geoAtts=[]
    for s_surfName,s_surfType,s_surfCon,ls_surfBound,s_surfParent in zip(
        ls_surfNames,ls_surfTypes,ls_surfCons,lls_surfBounds,ls_surfParents):
        
        lls_geoAtts.append(['*surf',s_surfName[-i_surfTrunc:],s_surfType,s_surfParent[-i_surfTrunc:],'-','-',s_surfCon,'OPAQUE']+ls_surfBound)

# Debug: print lists to standard out.
    print 'Zone name: '+s_zoneName
    #print lls_geoVerts
    #print lls_geoSurfs
    #print lls_geoAtts
    #print ''

# Write ESP-r geometry file.

# Open and write header.
    curDateTime=datetime.datetime.now()
    s_dateTime=curDateTime.strftime('%a %b %d %X %Y')
    f_geoFile=open(s_pth+s_zoneName[:i_zonTrunc]+'.geo','w')
    s_text=('*Geometry 1.1,GEN,'+s_zoneName[:i_zonTrunc]+' # tag version, format, zone name\n'+
        '*date '+s_dateTime+'  # latest file modification \n'+
        s_zoneName[:i_zonTrunc]+' describes a zone imported from EnergyPlus zone "'+s_zoneName+'".\n'+
        '# tag, X co-ord, Y co-ord, Z co-ord\n')
    f_geoFile.write(s_text)

# Write vertex listings.
    for ls_geoVerts in lls_geoVerts:
        f_geoFile.write(','.join(ls_geoVerts)+'\n')

# Write vertex-surface association listings.
    f_geoFile.write('# \n# tag, number of vertices followed by list of associated vert\n')
    for ls_geoSurfs in lls_geoSurfs:
        f_geoFile.write(','.join(ls_geoSurfs)+'\n')

# Write surface attributes.
    f_geoFile.write('# \n'+
        '# surf attributes:\n'+
        '#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN\n'+
        '#  child of (surface name), useage (pair of tags) \n'+
        '#  construction name, optical name\n'+
        '#  boundary condition tag followed by two data items\n')
    for ls_geoAtts in lls_geoAtts:
        f_geoFile.write(','.join(ls_geoAtts)+'\n')

# Write end data.
    f_geoFile.write('# \n'+
        '*insol,3,0,0,0  # default insolation distribution\n'+
        '# \n'+
        '# shading directives\n'+
        '*shad_calc,none  # no temporal shading requested\n'+
        '# \n'+
        '*insol_calc,none  # no insolation requested\n'+
        '# \n'+
        '*base_list,0,10.00,0  # zone base')

    f_geoFile.close()

# Dump out a dummy cnn file.
# Write the header.
curDateTime=datetime.datetime.now()
s_dateTime=curDateTime.strftime('%a %b %d %X %Y')
f_cnnFile=open(s_pth+'Eplus_imported.cnn','w')
s_text=('*connections for Eplus_imported\n'+
        '*date '+s_dateTime+'  # latest file modification \n'+
        str(i_totSurfs)+' # number of connections\n')
f_cnnFile.write(s_text)
# Write what data we do have.
i_zoneRef=-1
for i_zoneSurfs in li_zoneSurfs:
    i_zoneRef=i_zoneRef+1
    i_surfRef=-1
    lls_surfBounds=llls_surfBounds[i_zoneRef]
    for ls_surfBounds in lls_surfBounds:
        i_surfRef=i_surfRef+1
	if ls_surfBounds[0]=='GROUND':
            s_text='  '+str(i_zoneRef+1)+'  '+str(i_surfRef+1)+'  4  '+ls_surfBounds[1]+'  '+ls_surfBounds[2]+' \n'
	elif ls_surfBounds[0]=='ANOTHER':
            s_text='  '+str(i_zoneRef+1)+'  '+str(i_surfRef+1)+'  3  '+ls_surfBounds[1]+'  '+ls_surfBounds[2]+' \n'
	elif ls_surfBounds[0]=='EXTERIOR':
            s_text='  '+str(i_zoneRef+1)+'  '+str(i_surfRef+1)+'  0  '+ls_surfBounds[1]+'  '+ls_surfBounds[2]+' \n'
	elif ls_surfBounds[0]=='UNKNOWN':
            s_text='  '+str(i_zoneRef+1)+'  '+str(i_surfRef+1)+'  0  '+ls_surfBounds[1]+'  '+ls_surfBounds[2]+' \n'
        f_cnnFile.write(s_text)
f_cnnFile.close()
