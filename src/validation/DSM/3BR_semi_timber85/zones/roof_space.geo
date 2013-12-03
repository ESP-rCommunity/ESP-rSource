*Geometry 1.1,GEN,roof_space # tag version, format, zone name
*date Mon Mar 19 09:54:39 2012  # latest file modification 
roof_space describes cold roof above bedrooms
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,5.57568,5.60000  #   1
*vertex,0.10000,5.57568,5.60000  #   2
*vertex,0.10000,3.90000,5.60000  #   3
*vertex,3.60000,3.90000,5.60000  #   4
*vertex,3.60000,4.10000,5.60000  #   5
*vertex,3.60000,4.90000,5.60000  #   6
*vertex,4.50000,4.90000,5.60000  #   7
*vertex,6.50000,4.90000,5.60000  #   8
*vertex,6.50000,5.57568,5.60000  #   9
*vertex,3.60000,3.70000,5.60000  #  10
*vertex,3.60000,2.80000,5.60000  #  11
*vertex,3.60000,2.50000,5.60000  #  12
*vertex,4.50000,2.50000,5.60000  #  13
*vertex,5.50000,2.50000,5.60000  #  14
*vertex,5.50000,3.60000,5.60000  #  15
*vertex,5.50000,4.40000,5.60000  #  16
*vertex,5.50000,4.90000,5.60000  #  17
*vertex,5.50000,2.40000,5.60000  #  18
*vertex,6.50000,2.40000,5.60000  #  19
*vertex,3.60000,1.90000,5.60000  #  20
*vertex,6.50000,1.90000,5.60000  #  21
*vertex,0.10000,1.90000,5.60000  #  22
*vertex,0.10000,3.90000,7.60000  #  23
*vertex,3.60000,3.90000,7.60000  #  24
*vertex,6.50000,3.90000,7.60000  #  25
# 
# tag, number of vertices followed by list of associated vert
*edges,7,2,1,7,6,5,4,3  #  1
*edges,15,9,8,17,16,15,14,13,12,11,10,4,5,6,7,1  #  2
*edges,7,17,8,19,18,14,15,16  #  3
*edges,7,21,20,12,13,14,18,19  #  4
*edges,7,20,22,3,4,10,11,12  #  5
*edges,6,1,2,23,24,25,9  #  6
*edges,5,8,9,25,21,19  #  7
*edges,4,3,22,23,2  #  8
*edges,6,20,21,25,24,23,22  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,br2_ceiling,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,04,15  #   1 ||< br2_ceiling:up_br2
*surf,ceil_staur,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,05,20  #   2 ||< ceil_staur:upstair_pa
*surf,wc_ceiling,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,06,09  #   3 ||< wc_ceiling:upper_wc
*surf,br1_ceiling,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,07,12  #   4 ||< br1_ceiling:up_br_one
*surf,br3_ceiling,FLOR,-,-,-,Floorti_rf,OPAQUE,ANOTHER,08,13  #   5 ||< br3_ceiling:up_br_three
*surf,r1,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,wc_side_wall,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,side_wall,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,r2,SLOP,-,ROOF,PITCHED,Roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,23.52,0  # zone base
