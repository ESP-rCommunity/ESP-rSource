*Geometry 1.1,GEN,TheChannel # tag version, format, zone name
*date Wed Jan 15 08:34:35 2014  # latest file modification 
TheChannel describes a double facade cavity with air flow
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,-0.75000,0.00000  #   1
*vertex,4.50000,-0.75000,0.00000  #   2
*vertex,4.50000,-0.05000,0.00000  #   3
*vertex,0.00000,-0.05000,0.00000  #   4
*vertex,0.00000,-0.75000,4.25000  #   5
*vertex,4.50000,-0.75000,4.25000  #   6
*vertex,4.50000,-0.05000,4.25000  #   7
*vertex,0.00000,-0.05000,4.25000  #   8
*vertex,0.00000,-0.05000,3.50000  #   9
*vertex,4.50000,-0.05000,3.50000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,5,2,3,10,7,6  #  2
*edges,4,3,4,9,10  #  3
*edges,5,4,1,5,8,9  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,10,9,8,7  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,CurtGlass,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-2,VERT,-,-,-,door,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,iCW,VERT,-,-,-,dbl_glz,DCF7671_06nb,ANOTHER,01,01  #   3 ||< iCW:TheSpace
*surf,Wall-4,VERT,-,-,-,door,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,Top-5,CEIL,-,-,-,door,OPAQUE,ANOTHER,05,06  #   5 ||< Base-6:mixTop
*surf,Base-6,FLOR,-,-,-,door,OPAQUE,ANOTHER,04,05  #   6 ||< Top-5:mixBottom
*surf,panelCW,VERT,-,-,-,dummy_pnls,OPAQUE,ANOTHER,02,01  #   7 ||< panelCW:hungCeiling
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     3.15 0  # zone base list
