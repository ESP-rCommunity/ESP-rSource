*Geometry 1.1,GEN,kitliv # tag version, format, zone name
*date Fri Jul 17 11:56:36 2009  # latest file modification 
kitliv describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.48800,5.93200,0.00000  #   1
*vertex,10.58800,9.56900,0.00000  #   2
*vertex,5.21900,12.66900,0.00000  #   3
*vertex,3.11900,9.03200,0.00000  #   4
*vertex,8.48800,5.93200,2.40000  #   5
*vertex,10.58800,9.56900,2.40000  #   6
*vertex,5.21900,12.66900,2.40000  #   7
*vertex,3.11900,9.03200,2.40000  #   8
*vertex,3.76800,8.65700,0.60000  #   9
*vertex,4.50400,8.23200,0.60000  #  10
*vertex,4.50400,8.23200,2.10000  #  11
*vertex,3.76800,8.65700,2.10000  #  12
*vertex,7.05900,6.75700,0.60000  #  13
*vertex,7.79500,6.33200,0.60000  #  14
*vertex,7.79500,6.33200,2.10000  #  15
*vertex,7.05900,6.75700,2.10000  #  16
*vertex,9.28800,7.31700,0.60000  #  17
*vertex,10.08800,8.70400,0.60000  #  18
*vertex,10.08800,8.70400,2.10000  #  19
*vertex,9.28800,7.31700,2.10000  #  20
*vertex,7.12300,11.56900,0.00000  #  21
*vertex,7.12300,11.56900,2.40000  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,17,20,19,18,17  #  1
*edges,4,2,21,22,6  #  2
*edges,4,3,4,8,7  #  3
*edges,16,4,1,14,13,16,15,14,1,5,8,4,9,12,11,10,9  #  4
*edges,5,5,6,22,7,8  #  5
*edges,5,1,4,3,21,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,17,18,19,20  #  8
*edges,4,13,14,15,16  #  9
*edges,4,21,3,7,22  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,s_buf,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,07,06  #   1 ||< s_liv:buf_1
*surf,s_hall,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,03,04  #   2 ||< s_liv:hall
*surf,s_adjhouse,VERT,-,-,-,ext_west,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,s_west,VERT,-,-,-,int_west,OPAQUE,ANOTHER,02,02  #   4 ||< s_liv:west_space
*surf,ceil,CEIL,-,-,-,int_roof,OPAQUE,ANOTHER,10,02  #   5 ||< liv:roof
*surf,floor,FLOR,-,-,-,house_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,win1,VERT,-,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   7 ||< external
*surf,win3,VERT,-,-,-,doub_glaz,DC_8074_04nb,ANOTHER,07,08  #   8 ||< int_win:buf_1
*surf,win2,VERT,-,-,-,doub_glaz,DC_8074_04nb,EXTERIOR,0,0  #   9 ||< external
*surf,s_bath,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,04,04  #  10 ||< s_liv:bath
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   2 # list of surfs
  7  9
# 
*insol_calc,all_applicable   2 # insolation sources
  7  9
# 
*base_list,1,6,    26.04 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,20 20 # geometric blocks
*obs,3.528,8.240,0.600,0.400,0.100,1.450,60.000,nl,NONE  # block  1
*obs,4.264,7.815,0.600,0.400,0.100,1.450,60.000,nr,NONE  # block  2
*obs,3.641,8.637,2.200,0.400,1.000,0.100,240.000,nt,NONE  # block  3
*obs,6.819,6.340,0.600,0.400,0.100,1.450,60.000,sl,NONE  # block  4
*obs,7.555,5.915,0.600,0.400,0.100,1.450,60.000,sr,NONE  # block  5
*obs,7.019,6.687,2.200,0.400,1.000,0.100,240.000,st,NONE  # block  6
*end_block
