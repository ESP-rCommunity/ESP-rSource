*Geometry 1.1,GEN,stairwell # tag version, format, zone name
*date Wed May  5 17:18:13 2021  # latest file modification 
stairwell describes the stairwell for the basement will be merg
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.83000,0.00000,0.00000  #   1
*vertex,7.25000,0.00000,0.00000  #   2
*vertex,7.25000,2.27000,0.00000  #   3
*vertex,4.83000,2.27000,0.00000  #   4
*vertex,4.83000,0.00000,2.60000  #   5
*vertex,7.25000,0.00000,2.60000  #   6
*vertex,7.25000,2.27000,2.60000  #   7
*vertex,4.83000,2.27000,2.60000  #   8
*vertex,7.25000,0.00000,5.62000  #   9
*vertex,4.83000,0.00000,5.62000  #  10
*vertex,7.25000,2.27000,5.62000  #  11
*vertex,4.83000,2.27000,5.62000  #  12
*vertex,7.25000,0.00000,8.06500  #  13
*vertex,4.83000,0.00000,8.06500  #  14
*vertex,7.25000,2.27000,8.06500  #  15
*vertex,4.83000,2.27000,8.06500  #  16
*vertex,5.37500,0.00000,5.72000  #  17
*vertex,6.62500,0.00000,5.72000  #  18
*vertex,6.62500,0.00000,6.97000  #  19
*vertex,5.37500,0.00000,6.97000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #   1
*edges,4,2,3,7,6  #   2
*edges,4,3,4,8,7  #   3
*edges,4,4,1,5,8  #   4
*edges,4,1,4,3,2  #   5
*edges,4,7,8,12,11  #   6
*edges,4,8,5,10,12  #   7
*edges,4,5,6,9,10  #   8
*edges,10,10,9,13,14,10,17,18,19,20,17  #   9
*edges,4,15,11,12,16  #  10
*edges,4,14,16,12,10  #  11
*edges,4,15,13,9,11  #  12
*edges,4,14,13,15,16  #  13
*edges,4,11,9,6,7  #  14
*edges,4,18,17,20,19  #  15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,st_bsm_out,VERT,-,-,-,foundation_,OPAQUE,BASESIMP,01,003  #   1 ||< BASESIMP config type   1
*surf,bsm_south,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,01,008  #   2 ||< stair_south:basement
*surf,bsm_east,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,01,007  #   3 ||< stair_east:basement
*surf,bsm_north,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,01,006  #   4 ||< stair_north:basement
*surf,stair_floor,FLOR,-,-,-,slab_floor,OPAQUE,BASESIMP,01,003  #   5 ||< BASESIMP config type   1
*surf,main_east,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,03,016  #   6 ||< stair_east:mainfloor
*surf,main_north,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,03,015  #   7 ||< stair_north:mainfloor
*surf,st_1_out,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,00,000  #   8 ||< external
*surf,st_2_out,VERT,-,-,-,ccht_wall,OPAQUE,EXTERIOR,00,000  #   9 ||< external
*surf,2nd_east,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,04,014  #  10 ||< stair_east:secondfloor
*surf,2nd_north,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,04,013  #  11 ||< stair_north:secondfloor
*surf,2nd_south,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,04,015  #  12 ||< stair_south:secondfloor
*surf,stair_ceilin,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,011  #  13 ||< stair_ceilin:roof
*surf,main_south,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,03,017  #  14 ||< stair_south:mainfloor
*surf,w_triangle,VERT,-,-,-,ccht_window,DCF7671_06nb,EXTERIOR,00,000  #  15 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,     5.49 0  # zone base list
