*Geometry 1.1,GEN,conservatory # tag version, format, zone name
*date Tue Dec 13 17:57:35 2011  # latest file modification 
conservatory describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,11.40400,5.25100,0.00000  #   1
*vertex,9.80500,4.82300,0.00000  #   2
*vertex,9.80500,4.82300,2.10000  #   3
*vertex,6.32800,3.89100,2.10000  #   4
*vertex,6.32800,3.89100,0.00000  #   5
*vertex,6.09100,3.82800,0.00000  #   6
*vertex,6.09100,3.82800,0.40000  #   7
*vertex,6.09100,3.82800,2.70000  #   8
*vertex,10.63100,5.04400,2.70000  #   9
*vertex,11.40400,5.25100,2.70000  #  10
*vertex,11.92200,3.31900,0.00000  #  11
*vertex,7.38198,2.10307,0.00000  #  12
*vertex,6.60900,1.89600,0.00000  #  13
*vertex,6.60900,1.89600,0.40000  #  14
*vertex,6.60900,1.89600,2.00000  #  15
*vertex,7.38200,2.10300,2.00000  #  16
*vertex,11.14900,3.11200,2.00000  #  17
*vertex,11.92200,3.31900,2.00000  #  18
*vertex,11.92200,3.31900,0.40000  #  19
*vertex,11.40400,5.25100,0.40000  #  20
*vertex,11.40400,5.25100,2.40000  #  21
*vertex,8.07000,4.35800,2.40000  #  22
*vertex,8.06998,4.35807,2.70000  #  23
*vertex,6.09026,3.83078,2.40000  #  24
*vertex,11.12456,3.20317,2.03303  #  25
*vertex,7.38200,2.10300,0.20000  #  26
*vertex,11.92200,3.31900,0.20000  #  27
*vertex,6.77775,2.03890,2.03303  #  28
*vertex,6.28419,3.87974,2.70000  #  29
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,3,4,5,6,7,24,22,21,20  #  1
*edges,4,2,5,4,3  #  2
*edges,5,7,14,15,8,24  #  3
*edges,6,26,27,19,18,17,16  #  4
*edges,5,10,18,19,20,21  #  5
*edges,5,28,25,9,23,29  #  6
*edges,4,6,13,14,7  #  7
*edges,8,13,12,11,27,26,16,15,14  #  8
*edges,5,11,1,20,19,27  #  9
*edges,10,17,18,10,9,25,28,29,8,15,16  # 10
*edges,7,1,11,12,13,6,5,2  # 11
*edges,5,21,22,23,9,10  # 12
*edges,5,22,24,8,29,23  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,liv_con,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,01,04  #   1 ||< liv_con:living_room
*surf,glazing,VERT,liv_con,-,-,double_glaz,DCF7672_06nb,ANOTHER,01,09  #   2 ||< glazing:living_room
*surf,w_glaz,VERT,-,-,-,single_glaz,SCF8782_06nb,EXTERIOR,0,0  #   3 ||< external
*surf,s_glaz,VERT,-,-,-,single_glaz,SCF8782_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,e_glaz,VERT,-,-,-,single_glaz,SCF8782_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,r_glazing,SLOP,-,-,-,single_glaz,SCF8782_06nb,EXTERIOR,0,0  #   6 ||< external
*surf,w_frame,VERT,-,-,-,cons_frame,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,s_frame,VERT,-,-,-,cons_frame,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,e_frame,VERT,-,-,-,cons_frame,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,r_frame,SLOP,-,-,-,cons_frame,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,floor,FLOR,-,-,-,cons_floor,OPAQUE,GROUND,00,01  #  11 ||< user def grnd profile  1
*surf,b2_con,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,07,05  #  12 ||< p_conserv:bedroom_2
*surf,b4_con,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,09,10  #  13 ||< Surf-10:bedroom_4
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   4 # insolation sources
  3  4  5  6
# 
*base_list,0,11.00,1  # zone base
