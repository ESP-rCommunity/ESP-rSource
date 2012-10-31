*Geometry 1.1,GEN,passage # tag version, format, zone name
*date Tue Jul 21 13:27:29 2009  # latest file modification 
passage describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.00000,4.50000,0.00000  #   1
*vertex,5.50000,4.50000,0.00000  #   2
*vertex,10.00000,4.50000,0.00000  #   3
*vertex,14.50000,4.50000,0.00000  #   4
*vertex,19.00000,4.50000,0.00000  #   5
*vertex,19.00000,6.00000,0.00000  #   6
*vertex,1.00000,6.00000,0.00000  #   7
*vertex,1.00000,4.50000,2.70000  #   8
*vertex,5.50000,4.50000,2.70000  #   9
*vertex,10.00000,4.50000,2.70000  #  10
*vertex,14.50000,4.50000,2.70000  #  11
*vertex,19.00000,4.50000,2.70000  #  12
*vertex,19.00000,6.00000,2.70000  #  13
*vertex,1.00000,6.00000,2.70000  #  14
*vertex,4.20000,4.50000,0.00000  #  15
*vertex,5.00000,4.50000,0.00000  #  16
*vertex,5.00000,4.50000,2.10000  #  17
*vertex,4.20000,4.50000,2.10000  #  18
*vertex,8.70000,4.50000,0.00000  #  19
*vertex,9.50000,4.50000,0.00000  #  20
*vertex,9.50000,4.50000,2.10000  #  21
*vertex,8.70000,4.50000,2.10000  #  22
*vertex,13.20000,4.50000,0.00000  #  23
*vertex,14.00000,4.50000,0.00000  #  24
*vertex,14.00000,4.50000,2.10000  #  25
*vertex,13.20000,4.50000,2.10000  #  26
*vertex,17.70000,4.50000,0.00000  #  27
*vertex,18.50000,4.50000,0.00000  #  28
*vertex,18.50000,4.50000,2.10000  #  29
*vertex,17.70000,4.50000,2.10000  #  30
*vertex,1.00000,5.80000,0.00000  #  31
*vertex,1.00000,5.00000,0.00000  #  32
*vertex,1.00000,5.00000,2.10000  #  33
*vertex,1.00000,5.80000,2.10000  #  34
*vertex,18.00000,6.00000,1.70000  #  35
*vertex,11.00000,6.00000,1.70000  #  36
*vertex,11.00000,6.00000,2.10000  #  37
*vertex,18.00000,6.00000,2.10000  #  38
# 
# tag, number of vertices followed by list of associated vert
*edges,8,1,15,18,17,16,2,9,8  #  1
*edges,8,2,19,22,21,20,3,10,9  #  2
*edges,8,3,23,26,25,24,4,11,10  #  3
*edges,8,4,27,30,29,28,5,12,11  #  4
*edges,4,5,6,13,12  #  5
*edges,10,6,7,14,13,6,35,38,37,36,35  #  6
*edges,8,7,31,34,33,32,1,8,14  #  7
*edges,7,8,9,10,11,12,13,14  #  8
*edges,17,1,32,31,7,6,5,28,27,4,24,23,3,20,19,2,16,15  #  9
*edges,4,15,16,17,18  # 10
*edges,4,19,20,21,22  # 11
*edges,4,23,24,25,26  # 12
*edges,4,27,28,29,30  # 13
*edges,4,31,32,33,34  # 14
*edges,4,35,36,37,38  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,prt_dg,VERT,-,-,-,block_part,OPAQUE,ANOTHER,01,03  #   1 ||< back_dg:direct_gain
*surf,prt_rad,VERT,-,-,-,block_part,OPAQUE,ANOTHER,02,03  #   2 ||< back_dg:rad_test
*surf,prt_exch,VERT,-,-,-,block_part,OPAQUE,ANOTHER,03,03  #   3 ||< back_dg:heatexch
*surf,prt_mix,VERT,-,-,-,block_part,OPAQUE,ANOTHER,04,03  #   4 ||< back_dg:mixed_test
*surf,right,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,north,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,left,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ceil,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,05  #   8 ||< ceil_pasg:roof
*surf,floor,FLOR,-,-,-,passg_floor,OPAQUE,GROUND,01,00  #   9 ||< ground profile  1
*surf,door_dg,VERT,-,-,-,door,OPAQUE,ANOTHER,01,08  #  10 ||< door:direct_gain
*surf,door_rad,VERT,-,-,-,door,OPAQUE,ANOTHER,02,08  #  11 ||< door:rad_test
*surf,door_exch,VERT,-,-,-,door,OPAQUE,ANOTHER,03,08  #  12 ||< door:heatexch
*surf,door_mix,VERT,-,-,-,door,OPAQUE,ANOTHER,04,08  #  13 ||< door:mixed_test
*surf,door,VERT,-,-,-,door,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,glaz,VERT,-,-,-,dbl_glz,DCF7671_06nb,EXTERIOR,0,0  #  15 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,    27.00 0  # zone base list
