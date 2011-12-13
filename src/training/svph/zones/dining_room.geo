*Geometry 1.1,GEN,dining_room # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
dining_room describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.27600,6.87100,0.00000  #   1
*vertex,2.18500,6.04300,0.00000  #   2
*vertex,2.62500,4.40100,0.00000  #   3
*vertex,3.00000,3.00000,0.00000  #   4
*vertex,6.09100,3.82800,0.00000  #   5
*vertex,5.27600,6.87100,2.40000  #   6
*vertex,2.18500,6.04300,2.40000  #   7
*vertex,2.62500,4.40100,2.40000  #   8
*vertex,3.00000,3.00000,2.40000  #   9
*vertex,5.60800,3.69900,2.40000  #  10
*vertex,6.09100,3.82800,2.40000  #  11
*vertex,5.83200,4.79400,2.40000  #  12
*vertex,5.34900,4.66500,2.40000  #  13
*vertex,3.57200,3.14400,0.06000  #  14
*vertex,5.31100,3.61000,0.06000  #  15
*vertex,5.30900,3.61700,2.16000  #  16
*vertex,3.57000,3.15200,2.16000  #  17
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,7,6  #  1
*edges,4,2,3,8,7  #  2
*edges,4,3,4,9,8  #  3
*edges,11,4,5,11,10,9,4,14,17,16,15,14  #  4
*edges,5,5,1,6,12,11  #  5
*edges,4,12,13,10,11  #  6
*edges,7,6,7,8,9,10,13,12  #  7
*edges,5,5,4,3,2,1  #  8
*edges,4,14,15,16,17  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,kit_din,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,04,04  #   1 ||< kit_din:kitchen
*surf,gar_kit,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,14,04  #   2 ||< gar_din:garage
*surf,w_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,s_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,din_liv,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,01,03  #   5 ||< din_liv:living_room
*surf,ceil_b4,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,09,09  #   6 ||< flr_din:bedroom_4
*surf,ceil_b1,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,06,11  #   7 ||< flr_din:bedroom_1
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #   8 ||< user def grnd profile  1
*surf,glazing,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    10.08 0  # zone base list
