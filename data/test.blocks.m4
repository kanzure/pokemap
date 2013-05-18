divert(-1)

This file should be preprocessed with m4.

The various block types.

The solid terrains.
define(`rock',  0x00)
define(`dirt',  0x01)
define(`grass', 0x02)
define(`water', 0x03)
define(`sand',  0x04)
define(`trees', 0x05)

For terrain interactions, one terrain is "lower" and one is "higher".
This refers solely to the terrain numbers, and is only to disambiguate them.

The double-terrain transition offsets.
define(`rockdirt',   0x10)
define(`rockgrass',  0x20)
define(`rockwater',  0x30)
define(`rocksand',   0x40)
define(`rocktrees',  0x50)
define(`dirtgrass',  0x60)
define(`dirtwater',  0x70)
define(`dirtsand',   0x80)
define(`dirttrees',  0x90)
define(`grasswater', 0xa0)
define(`grasssand',  0xb0)
define(`grasstrees', 0xc0)
define(`watersand',  0xd0)
define(`watertrees', 0xe0)
define(`sandtrees',  0xf0)

Double-terrain interactions.
define(`north',     0x00) Two terrains, horizontal, lower on north.
define(`south',     0x01) Two terrains, horizontal, lower on south.
define(`east',      0x02) Two terrains, vertical, lower on east.
define(`west',      0x03) Two terrains, vertical, lower on west.

define(`innerne',   0x04) Two terrains, inner corner, lower on northeast.
define(`innerse',   0x05) Two terrains, inner corner, lower on southeast.
define(`innernw',   0x06) Two terrains, inner corner, lower on northwest.
define(`innersw',   0x07) Two terrains, inner corner, lower on southwest.

define(`outerne',   0x08) Two terrains, outer corner, higher on northeast.
define(`outerse',   0x09) Two terrains, outer corner, higher on southeast.
define(`outernw',   0x0a) Two terrains, outer corner, higher on northwest.
define(`outersw',   0x0b) Two terrains, outer corner, higher on southwest.

define(`slash',     0x0c) Two terrains, diagonal, lower on northeast and southwest.
define(`backslash', 0x0d) Two terrains, diagonal, lower on southeast and northwest.

For double-terrain interactions, use offset+interaction.

This gives us a block map that looks like this:

      0x_0  0x_1  0x_2  0x_3  0x_4  0x_5  0x_6  0x_7  0x_8  0x_9  0x_a  0x_b  0x_c  0x_d  0x_e  0x_f
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
0x0_ |SOLID|SOLID|SOLID|SOLID|SOLID|SOLID|                                                           |
     |ROCK |DIRT |GRASS|WATER|SAND |TREES|                                                           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x1_ |ROCK-DIRT INTERACTIONS                                                             |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x2_ |ROCK-GRASS INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x3_ |DIRT-GRASS INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x4_ |ROCK-WATER INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x5_ |DIRT-WATER INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x6_ |GRASS-WATER INTERACTIONS                                                           |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x7_ |ROCK-SAND INTERACTIONS                                                             |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x8_ |DIRT-SAND INTERACTIONS                                                             |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0x9_ |GRASS-SAND INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xa_ |WATER-SAND INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xb_ |ROCK-TREES INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xc_ |DIRT-TREES INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xd_ |GRASS-TREES INTERACTIONS                                                           |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xe_ |WATER-TREES INTERACTIONS                                                           |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+           +
0xf_ |SAND-TREES INTERACTIONS                                                            |           |
     |                                                                                   |           |
     +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
      north south east  west  inner                   outer                   slash back
                              ne    se    nw    sw    ne    se    nw    sw          slash

This layout is reasonably organized, and gives us a complete set of two-terrain interaction blocks.
We have 40 blocks left over for special stuff, before we have to start sacrificing interactions.

Solid blocks.

define( solid,
block `eval($1)' n:``$1'' s:``$1'' e:``$1'' w:``$1'')

Double-terrain interactions.
For these macros, I expect the lower terrain type to be the earlier parameter.

In this blockset, I use the following convention for edge names:
  If an edge is entirely one terrain, it is named for that terrain.
  If an edge is multiple terrains, they are concatenated in order:
    For vertical edges, from north to south.
    For horizontal edges, from west to east.

Horizontal/vertical interactions.

define( horizontals,
block `eval($1$2+north)' n:``$1'' s:``$2'' e:``$1$2'' w:``$1$2''
block `eval($1$2+south)' n:``$2'' s:``$1'' e:``$2$1'' w:``$2$1'')

define( verticals,
block `eval($1$2+east)' n:``$2$1'' s:``$2$1'' e:``$1'' w:``$2''
block `eval($1$2+west)' n:``$1$2'' s:``$1$2'' e:``$2'' w:``$1'')

Corners.

define( innercorners,
block `eval($1$2+innerne)' n:``$2$1'' s:``$2'' e:``$1$2'' w:``$2''
block `eval($1$2+innerse)' n:``$2'' s:``$2$1'' e:``$2$1'' w:``$2''
block `eval($1$2+innernw)' n:``$1$2'' s:``$2'' e:``$2'' w:``$1$2''
block `eval($1$2+innersw)' n:``$2'' s:``$1$2'' e:``$2'' w:``$2$1'')

define( outercorners,
block `eval($1$2+outerne)' n:``$1$2'' s:``$1'' e:``$2$1'' w:``$1''
block `eval($1$2+outerse)' n:``$1'' s:``$1$2'' e:``$1$2'' w:``$1''
block `eval($1$2+outernw)' n:``$2$1'' s:``$1'' e:``$1'' w:``$2$1''
block `eval($1$2+outersw)' n:``$1'' s:``$2$1'' e:``$1'' w:``$1$2'')

Diagonals.

define( diagonals,
block `eval($1$2+slash)' n:``$2$1'' s:``$1$2'' e:``$1$2'' w:``$2$1''
block `eval($1$2+backslash)' n:``$1$2'' s:``$2$1'' e:``$2$1'' w:``$1$2'')

All the double transitions.

define( double,
`horizontals(`$1',`$2')'
`verticals(`$1',`$2')'
`innercorners(`$1',`$2')'
`outercorners(`$1',`$2')'
`diagonals(`$1',`$2')')

Now for the actual block definitions.

divert(0)dnl
solid(`rock')
solid(`dirt')
solid(`grass')
solid(`water')
solid(`sand')
solid(`trees')
double(`rock',  `dirt')
double(`rock',  `grass')
double(`rock',  `water')
double(`rock',  `sand')
double(`rock',  `trees')
double(`dirt',  `grass')
double(`dirt',  `water')
double(`dirt',  `sand')
double(`dirt',  `trees')
double(`grass', `water')
double(`grass', `sand')
double(`grass', `trees')
double(`water', `sand')
double(`water', `trees')
double(`sand',  `trees')
