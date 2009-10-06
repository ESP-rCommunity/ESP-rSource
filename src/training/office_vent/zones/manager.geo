*Geometry 1.1,GEN,manager # tag version, format, zone name
*date Fri Jul 17 11:05:01 2009  # latest file modification 
manager describes cellular office
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,3.00000,0.00000,0.00000  #   2
*vertex,3.00000,4.50000,0.00000  #   3
*vertex,1.00000,4.50000,0.00000  #   4
*vertex,0.00000,4.50000,0.00000  #   5
*vertex,0.00000,0.00000,3.00000  #   6
*vertex,3.00000,0.00000,3.00000  #   7
*vertex,3.00000,4.50000,3.00000  #   8
*vertex,1.00000,4.50000,3.00000  #   9
*vertex,0.00000,4.50000,3.00000  #  10
*vertex,0.00000,0.00000,0.90000  #  11
*vertex,3.00000,0.00000,0.90000  #  12
*vertex,1.00000,0.00000,0.90000  #  13
*vertex,2.00000,0.00000,0.90000  #  14
*vertex,1.00000,0.00000,0.50000  #  15
*vertex,2.00000,0.00000,0.50000  #  16
*vertex,0.10000,0.00000,1.00000  #  17
*vertex,2.90000,0.00000,1.00000  #  18
*vertex,2.90000,0.00000,2.90000  #  19
*vertex,0.10000,0.00000,2.90000  #  20
*vertex,2.90000,4.50000,0.10000  #  21
*vertex,1.10000,4.50000,0.10000  #  22
*vertex,1.10000,4.50000,2.90000  #  23
*vertex,2.90000,4.50000,2.90000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,5,2,3,8,7,12  #  1
*edges,10,3,4,9,8,3,21,24,23,22,21  #  2
*edges,4,4,5,10,9  #  3
*edges,5,5,1,11,6,10  #  4
*edges,5,6,7,8,9,10  #  5
*edges,5,1,5,4,3,2  #  6
*edges,8,1,2,12,14,16,15,13,11  #  7
*edges,4,15,16,14,13  #  8
*edges,12,11,13,14,12,7,6,11,17,20,19,18,17  #  9
*edges,4,17,18,19,20  # 10
*edges,4,21,22,23,24  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,pt_general,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,ANOTHER,02,02  #   1 ||< pt_mgrs:general
*surf,part_frame,VERT,-,-,-,insul_frame,OPAQUE,ANOTHER,02,13  #   2 ||< part_frame:general
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,02,01  #   3 ||< door:general
*surf,pt_other,VERT,-,-,-,gyp_gyp_ptn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,05,01  #   5 ||< Surf-6:ceil_void
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,spandral,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,vent,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,frame,VERT,-,-,-,insul_frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,glazing,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,part_glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,02,14  #  11 ||< part_glaz:general
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   4 # list of surfs
  7  8  9 10
# 
*insol_calc,all_applicable   1 # insolation sources
 10
# 
*base_list,1,6,    13.50 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,20 20 # geometric blocks
*obs,0.000,-0.100,3.100,3.000,0.020,0.300,0.000,ob1,NONE  # block  1
*obs,0.000,-0.300,3.100,3.000,0.020,0.250,0.000,ob2,NONE  # block  2
*obs,0.000,-0.500,3.100,3.000,0.020,0.220,0.000,ob3,NONE  # block  3
*obs,0.000,-0.700,3.100,3.000,0.020,0.200,0.000,ob4,NONE  # block  4
*obs,0.000,-0.900,3.100,3.000,0.020,0.200,0.000,ob5,NONE  # block  5
*obs,0.000,-1.100,3.100,3.000,0.020,0.200,0.000,ob6,NONE  # block  6
*end_block
