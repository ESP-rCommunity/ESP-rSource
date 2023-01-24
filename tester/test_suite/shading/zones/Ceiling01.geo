*Geometry 1.1,GEN,Ceiling01 # tag version, format, zone name
*date Fri Apr 30 11:21:35 2021  # latest file modification 
describes the roof or attic zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-1.00000,0.75000,3.02000  #   1
*vertex,-10.74600,0.75000,3.02000  #   2
*vertex,-10.74600,10.49600,3.02000  #   3
*vertex,-1.00000,10.49600,3.02000  #   4
*vertex,-1.00000,5.62300,4.64417  #   5
*vertex,-10.74600,5.62300,4.64417  #   6
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,3,4  #   1
*edges,4,1,5,6,2  #   2
*edges,3,2,6,3  #   3
*edges,4,3,6,5,4  #   4
*edges,3,4,5,1  #   5
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,to_main,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,001,006  #   1 ||< to_attic:Main
*surf,WestSlope,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,NorthGable,VERT,-,-,-,cladding,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,EastSlope,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   4 ||< external
*surf,SouthGable,VERT,-,-,-,cladding,OPAQUE,EXTERIOR,000,000  #   5 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,1,    94.98 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,  5  5 # geometric blocks
*obs,-3.7500,-0.0000,2.0470,0.7500,4.0000,0.0100,-270.00,1.00,obstr_1 NONE  # block   1
*obs,-10.7460,4.7500,2.3970,4.0000,1.0000,0.0100,-270.00,1.00,obstr_2 NONE  # block   2
*obs,-5.2460,10.4960,2.6300,0.5000,0.5000,0.0100,-270.00,1.00,obstr_3 NONE  # block   3
*obs,-0.0000,4.0620,2.5970,1.2000,1.0000,0.0100,-270.00,1.00,obstr_4 NONE  # block   4
*end_block
