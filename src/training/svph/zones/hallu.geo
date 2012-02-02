*Geometry 1.1,GEN,upper_hall # tag version, format, zone name
*date Tue Dec 13 17:39:46 2011  # latest file modification 
upper_hall describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,7.45500,10.71600,2.40000  #   1
*vertex,6.43900,10.44400,2.40000  #   2
*vertex,4.75700,9.99300,2.40000  #   3
*vertex,5.31600,7.90800,2.40000  #   4
*vertex,5.01900,7.82800,2.40000  #   5
*vertex,5.27600,6.87100,2.40000  #   6
*vertex,7.25400,7.40100,2.40000  #   7
*vertex,8.27000,7.67300,2.40000  #   8
*vertex,7.45500,10.71600,4.80000  #   9
*vertex,4.75700,9.99300,4.80000  #  10
*vertex,5.31600,7.90800,4.80000  #  11
*vertex,5.01900,7.82800,4.80000  #  12
*vertex,5.27600,6.87100,4.80000  #  13
*vertex,7.25400,7.40100,4.80000  #  14
*vertex,8.27000,7.67300,4.80000  #  15
*vertex,6.68300,10.51600,2.60000  #  16
*vertex,5.52400,10.20600,2.60000  #  17
*vertex,5.52500,10.20200,3.68000  #  18
*vertex,6.68400,10.51300,3.68000  #  19
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,3,10,9,1,16,19,18,17,16  #  1
*edges,4,3,4,11,10  #  2
*edges,4,4,5,12,11  #  3
*edges,4,5,6,13,12  #  4
*edges,4,6,7,14,13  #  5
*edges,4,7,8,15,14  #  6
*edges,4,8,1,9,15  #  7
*edges,7,9,10,11,12,13,14,15  #  8
*edges,4,8,7,2,1  #  9
*edges,6,7,6,5,4,3,2  # 10
*edges,4,16,17,18,19  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,prt_bth,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,05,07  #   2 ||< bth_uha:bath
*surf,prt_btha,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,05,06  #   3 ||< bth_uhb:bath
*surf,prt_b1,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,06,08  #   4 ||< p_hall:bedroom_1
*surf,prt_b4,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,09,01  #   5 ||< p_hall:bedroom_4
*surf,prt_b2,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,07,02  #   6 ||< p_hall:bedroom_2
*surf,prt_h_b3,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,08,02  #   7 ||< p_hall:bedroom_3
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,06  #   8 ||< ceil_hall:loft
*surf,flr_hall,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,03,10  #   9 ||< ceil_z10:hall
*surf,flr_kit,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,04,07  #  10 ||< kit_uh:kitchen
*surf,window,VERT,n_ext,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,9.10,1  # zone base
