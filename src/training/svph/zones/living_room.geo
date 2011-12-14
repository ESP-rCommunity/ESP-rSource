*Geometry 1.1,GEN,living_room # tag version, format, zone name
*date Tue Dec 13 17:31:19 2011  # latest file modification 
living_room describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.58800,8.29400,0.00000  #   1
*vertex,6.41100,7.17500,0.00000  #   2
*vertex,5.27500,6.87100,0.00000  #   3
*vertex,6.09100,3.82800,0.00000  #   4
*vertex,11.40300,5.25100,0.00000  #   5
*vertex,10.58800,8.29400,2.40000  #   6
*vertex,7.25404,7.40085,2.40000  #   7
*vertex,6.41100,7.17500,2.40000  #   8
*vertex,5.27500,6.87100,2.40000  #   9
*vertex,6.09100,3.82800,2.40000  #  10
*vertex,8.06900,4.35800,2.40000  #  11
*vertex,11.40300,5.25100,2.40000  #  12
*vertex,9.80500,4.82300,0.00000  #  13
*vertex,9.80500,4.82300,2.10000  #  14
*vertex,6.32700,3.89100,2.10000  #  15
*vertex,6.32700,3.89100,0.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,8,7,6  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,9,4,16,15,14,13,5,12,11,10  #  4
*edges,4,5,1,6,12  #  5
*edges,4,6,7,11,12  #  6
*edges,5,7,8,9,10,11  #  7
*edges,7,5,13,16,4,3,2,1  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hall_liv,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,03,07  #   1 ||< p_living:hall
*surf,kit_liv,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,04,05  #   2 ||< kit_liv:kitchen
*surf,din_liv,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,02,05  #   3 ||< din_liv:dining_room
*surf,liv_con,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,15,01  #   4 ||< liv_con:conservatory
*surf,e_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ceil_b2,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,07,08  #   6 ||< flr_liv:bedroom_2
*surf,ceil_b4,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,09,08  #   7 ||< flr_liv:bedroom_4
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #   8 ||< user def grnd profile  1
*surf,glazing,VERT,liv_con,-,-,double_glaz,DCF7672_06nb,ANOTHER,15,02  #   9 ||< glazing:conservatory
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    17.33 0  # zone base list
