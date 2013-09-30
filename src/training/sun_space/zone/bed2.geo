*Geometry 1.1,GEN,bed2 # tag version, format, zone name
*date Fri Jul 17 11:56:36 2009  # latest file modification 
bed2 describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.75100,12.59100,0.00000  #   1
*vertex,10.30100,15.27600,0.00000  #   2
*vertex,7.61700,16.82600,0.00000  #   3
*vertex,6.06700,14.14100,0.00000  #   4
*vertex,8.75100,12.59100,2.40000  #   5
*vertex,10.30100,15.27600,2.40000  #   6
*vertex,7.61700,16.82600,2.40000  #   7
*vertex,6.06700,14.14100,2.40000  #   8
*vertex,9.60800,15.67600,0.60000  #   9
*vertex,8.87200,16.10100,0.60000  #  10
*vertex,8.87200,16.10100,2.10000  #  11
*vertex,9.60800,15.67600,2.10000  #  12
*vertex,7.97200,13.04100,0.00000  #  13
*vertex,7.97200,13.04100,2.40000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,10,2,3,7,6,2,9,12,11,10,9  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,13,14,8  #  4
*edges,5,5,6,7,8,14  #  5
*edges,5,1,13,4,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,1,5,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,s_bed1,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,05,03  #   1 ||< s_bed2:bed1
*surf,ext_east,VERT,-,-,-,ext_east,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,s_adjhouse,VERT,-,-,-,ext_west,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,s_bath,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,04,02  #   4 ||< s_bed2:bath
*surf,ceil,CEIL,-,-,-,int_roof,OPAQUE,ANOTHER,10,06  #   5 ||< bed2:roof
*surf,floor,FLOR,-,-,-,house_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,win,VERT,-,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   7 ||< external
*surf,s_hall,VERT,-,-,-,door,OPAQUE,ANOTHER,03,08  #   8 ||< s_bed2:hall
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   2 # list of surfs
  2  7
# 
*insol_calc,all_applicable   1 # insolation sources
  7
# 
*base_list,1,6,     9.61 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,10 10 # geometric blocks
*obs,9.702,15.645,0.600,0.600,0.100,1.500,60.000,S,NONE  # block  1
*obs,8.888,16.115,0.600,0.600,0.100,1.500,60.000,N,NONE  # block  2
*obs,9.705,15.643,2.100,0.600,1.050,0.100,60.000,T,NONE  # block  3
*end_block
