
Note that the model bld_simple_fzy.cfg uses a fuzzy
logic controller which does not work (there is an
error in the file which has not been clarified and
corrected).

The model bld_simple_shd.cfg includes examples of
different types of shading obstructions:
a) rectangular blocks for some portions of adjacent building
   and the trunk of the tree
b) general polygon obstructions representing the corner of
   adjacent building which is not square
c) general polygon obstruction represented a tapered tree
   shape
d) general polygon obstruction represented the gable end
e) rectangular block with 10 degree Y rotation to represent
   the overhang of the roof.
   
General polygon obstructions are converted from existing
obstructions. Individual coordinates can then be edited
to form the required shape (as long as the sides are
flat).

Block obstructions can be rotated around the Z axis as well 
as a 2nd axis.
