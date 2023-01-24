*Geometry 1.1,GEN,Main # tag version, format, zone name
*date Fri Apr 30 12:03:28 2021  # latest file modification 
simplified elevation of main zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-1.00000,0.75000,0.53000  #   1
*vertex,-1.00000,0.75000,3.02000  #   2
*vertex,-10.74600,0.75000,3.02000  #   3
*vertex,-10.74600,0.75000,0.53000  #   4
*vertex,-10.74600,10.49600,3.02000  #   5
*vertex,-10.74600,10.49600,0.53000  #   6
*vertex,-1.00000,10.49600,3.02000  #   7
*vertex,-1.00000,10.49600,0.53000  #   8
*vertex,-3.75000,0.75000,1.49656  #   9
*vertex,-7.75000,0.75000,1.49656  #  10
*vertex,-7.75000,0.75000,1.94656  #  11
*vertex,-3.75000,0.75000,1.94656  #  12
*vertex,-10.74600,2.98212,0.53000  #  13
*vertex,-10.74600,3.85212,0.53000  #  14
*vertex,-10.74600,3.85212,2.56000  #  15
*vertex,-10.74600,2.98212,2.56000  #  16
*vertex,-10.74600,5.25000,1.49656  #  17
*vertex,-10.74600,8.25000,1.49656  #  18
*vertex,-10.74600,8.25000,2.29656  #  19
*vertex,-10.74600,5.25000,2.29656  #  20
*vertex,-5.74600,10.49600,1.03000  #  21
*vertex,-5.24600,10.49600,1.03000  #  22
*vertex,-5.24600,10.49600,2.53000  #  23
*vertex,-5.74600,10.49600,2.53000  #  24
*vertex,-1.00000,8.26388,0.53000  #  25
*vertex,-1.00000,7.39388,0.53000  #  26
*vertex,-1.00000,7.39388,2.56000  #  27
*vertex,-1.00000,8.26388,2.56000  #  28
*vertex,-1.00000,5.16177,1.49656  #  29
*vertex,-1.00000,4.16177,1.49656  #  30
*vertex,-1.00000,4.16177,2.49656  #  31
*vertex,-1.00000,5.16177,2.49656  #  32
*vertex,-3.75000,0.75000,0.53000  #  33
*vertex,-5.74600,10.49600,0.53000  #  34
# 
# tag, number of vertices followed by list of associated vert
*edges,12,1,2,3,4,1,33,9,10,11,12,9,33  #   1
*edges,16,6,4,13,14,15,17,18,19,20,17,15,16,13,4,3,5  #   2
*edges,12,8,6,34,21,22,23,24,21,34,6,5,7  #   3
*edges,16,1,8,25,26,27,29,30,31,32,29,27,28,25,8,7,2  #   4
*edges,4,1,4,6,8  #   5
*edges,4,7,5,3,2  #   6
*edges,4,9,12,11,10  #   7
*edges,4,14,13,16,15  #   8
*edges,4,18,17,20,19  #   9
*edges,4,22,21,24,23  #  10
*edges,4,26,25,28,27  #  11
*edges,4,30,29,32,31  #  12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Mainfloor2,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,00,000  #   1 ||< external
*surf,Mainfloor1,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,00,000  #   2 ||< external
*surf,Mainfloor4,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,00,000  #   3 ||< external
*surf,Mainfloor3,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,00,000  #   4 ||< external
*surf,to_bsm,FLOR,-,-,-,floors,OPAQUE,ANOTHER,003,006  #   5 ||< to_main:Foundation-1
*surf,to_attic,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,002,001  #   6 ||< to_main:Ceiling01
*surf,Right0001,VERT,-,-,-,d_glz,TRAN,EXTERIOR,00,000  #   7 ||< external
*surf,Door-01,VERT,-,-,-,door,OPAQUE,EXTERIOR,00,000  #   8 ||< external
*surf,Front0001,VERT,-,-,-,d_glz,TRAN,EXTERIOR,00,000  #   9 ||< external
*surf,Left0001,VERT,-,-,-,dbl_glz,TRAN,EXTERIOR,00,000  #  10 ||< external
*surf,Door-02,VERT,-,-,-,door,OPAQUE,EXTERIOR,00,000  #  11 ||< external
*surf,Back0001,VERT,-,-,-,d_glz,TRAN,EXTERIOR,00,000  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable  10 # list of surfs
  1  2  3  4  7  8  9 10 11 12
# 
*insol_calc,all_applicable   4 # insolation sources
  7  9 10 12
# 
*base_list,1,5,    94.98 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,  5  5 # geometric blocks
*obs,-3.7500,-0.0000,2.0470,0.7500,4.0000,0.0100,-270.00,1.00,obstr_1 NONE  # block   1
*obs,-10.7460,4.7500,2.3970,4.0000,1.0000,0.0100,-270.00,1.00,obstr_2 NONE  # block   2
*obs,-5.2460,10.4960,2.6300,0.5000,0.5000,0.0100,-270.00,1.00,obstr_3 NONE  # block   3
*obs,-0.0000,4.0620,2.5970,1.2000,1.0000,0.0100,-270.00,1.00,obstr_4 NONE  # block   4
*end_block
