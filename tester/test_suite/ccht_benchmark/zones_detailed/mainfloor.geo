*Geometry 1.1,GEN,mainfloor # tag version, format, zone name
*date Wed May  5 16:30:33 2021  # latest file modification 
mainfloor describes the main floor of the house as 1 zone, not i
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,2.60000  #   1
*vertex,12.00000,0.00000,2.60000  #   2
*vertex,12.00000,6.79000,2.60000  #   3
*vertex,4.97000,6.79000,2.60000  #   4
*vertex,4.97000,12.20000,2.60000  #   5
*vertex,0.00000,12.20000,2.60000  #   6
*vertex,0.00000,7.00000,2.60000  #   7
*vertex,0.00000,0.00000,5.62000  #   8
*vertex,12.00000,0.00000,5.62000  #   9
*vertex,12.00000,6.79000,5.62000  #  10
*vertex,4.97000,6.79000,5.62000  #  11
*vertex,4.97000,12.20000,5.62000  #  12
*vertex,0.00000,12.20000,5.62000  #  13
*vertex,0.00000,7.00000,5.62000  #  14
*vertex,0.00000,2.70000,3.35000  #  15
*vertex,0.00000,1.07000,3.35000  #  16
*vertex,0.00000,1.07000,5.19000  #  17
*vertex,0.00000,2.70000,5.19000  #  18
*vertex,0.00000,5.93000,2.60000  #  19
*vertex,0.00000,4.13000,2.60000  #  20
*vertex,0.00000,11.20000,3.20000  #  21
*vertex,0.00000,10.17000,3.20000  #  22
*vertex,0.00000,10.17000,5.03000  #  23
*vertex,0.00000,11.20000,5.03000  #  24
*vertex,0.00000,8.67000,3.20000  #  25
*vertex,0.00000,7.64000,3.20000  #  26
*vertex,0.00000,7.64000,5.03000  #  27
*vertex,0.00000,8.67000,5.03000  #  28
*vertex,12.00000,4.25000,2.60000  #  29
*vertex,12.00000,5.35000,2.60000  #  30
*vertex,12.00000,1.00000,3.20000  #  31
*vertex,12.00000,3.43000,3.20000  #  32
*vertex,12.00000,3.43000,5.04000  #  33
*vertex,12.00000,1.00000,5.04000  #  34
*vertex,3.97000,12.20000,3.20000  #  35
*vertex,3.34000,12.20000,3.20000  #  36
*vertex,3.34000,12.20000,5.04000  #  37
*vertex,3.97000,12.20000,5.04000  #  38
*vertex,1.63000,12.20000,3.20000  #  39
*vertex,1.00000,12.20000,3.20000  #  40
*vertex,1.00000,12.20000,5.04000  #  41
*vertex,1.63000,12.20000,5.04000  #  42
*vertex,4.83000,2.27000,2.60000  #  43
*vertex,4.83000,0.00000,2.60000  #  44
*vertex,4.83000,0.00000,5.62000  #  45
*vertex,4.83000,2.27000,5.62000  #  46
*vertex,7.25000,2.27000,2.60000  #  47
*vertex,7.25000,2.27000,5.62000  #  48
*vertex,7.25000,0.00000,5.62000  #  49
*vertex,7.25000,0.00000,2.60000  #  50
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,44,45,8  #   1
*edges,12,2,29,30,3,10,9,2,31,34,33,32,31  #   2
*edges,4,3,4,11,10  #   3
*edges,4,4,5,12,11  #   4
*edges,16,5,6,40,39,42,41,40,6,13,12,5,35,38,37,36,35  #   5
*edges,16,6,7,26,25,28,27,26,7,14,13,6,21,24,23,22,21  #   6
*edges,12,1,16,15,18,17,16,1,8,14,7,19,20  #   7
*edges,11,8,45,46,48,49,9,10,11,12,13,14  #   8
*edges,4,15,16,17,18  #   9
*edges,4,21,22,23,24  #  10
*edges,4,25,26,27,28  #  11
*edges,4,31,32,33,34  #  12
*edges,4,35,36,37,38  #  13
*edges,4,39,40,41,42  #  14
*edges,4,44,43,46,45  #  15
*edges,4,43,47,48,46  #  16
*edges,4,49,48,47,50  #  17
*edges,4,9,49,50,2  #  18
*edges,15,44,1,20,19,7,6,5,4,3,30,29,2,50,47,43  #  19
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,sidewall_1,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,front,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,garage_share,VERT,-,-,-,ccht_wall,OPAQUE,ANOTHER,002,001  #   3 ||< main_share:garage
*surf,garage_back,VERT,-,-,-,ccht_wall,OPAQUE,ANOTHER,002,004  #   4 ||< garage_back:garage
*surf,sidewall_2,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #   5 ||< external
*surf,back_2,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #   6 ||< external
*surf,back_1,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #   7 ||< external
*surf,main_ceiling,CEIL,-,-,-,floors_r,OPAQUE,ANOTHER,004,006  #   8 ||< 2nd_floor:secondfloor
*surf,w_dining,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #   9 ||< external
*surf,w_family1,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #  10 ||< external
*surf,w_family2,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #  11 ||< external
*surf,w_living,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #  12 ||< external
*surf,w_side1,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #  13 ||< external
*surf,w_side2,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,000,000  #  14 ||< external
*surf,stair_north,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,005,007  #  15 ||< main_north:stairwell
*surf,stair_east,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,005,006  #  16 ||< main_east:stairwell
*surf,stair_south,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,005,014  #  17 ||< main_south:stairwell
*surf,sidewallfron,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,000,000  #  18 ||< external
*surf,main_floor,FLOR,-,-,-,floors,OPAQUE,ANOTHER,001,004  #  19 ||< bsment_ceiln:basement
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,19,   102.87 0  # zone base list
