/* PSutils.c contains utilities related to the handling of pseudocolor tables.
 */
#include "VIDSdefs.h"
PSTable	defaultPS;			/* global copies of our table	*/

/************************************************************************/
/* InitPSTable performs a one-time intialization for the pseudocolor
 * handling routines.
 */
int InitPSTable()
{
  PresetPSTable(&defaultPS, 1);
  return SUCCESS;
}
/************************************************************************/
/* NewPSTable will create a pointer to a new pseudocolor table associated
 * with the given image plane.  If a table already exists, it is re-used, 
 * otherwise a new table is allocated and ramped.
 */
PSTable *NewPSTable(env, imp)
  VIDSEnvironment	*env;
  int			imp;
{
  PSTable *tbl;

  tbl = env->planes[imp].pstable;
  if (tbl != NULL)
    return tbl;
  tbl = malloc(sizeof(PSTable));
  if (tbl == NULL)
    ABORT(NULL,"Sorry, insuficient memory to modify pseudocolor table","VIDS-INSUFMEM");
  env->planes[imp].pstable = tbl;
  PresetPSTable(tbl, 1);
  return tbl;
}
/************************************************************************/
/* CurrentPSTable will return a pointer to the current pseudocolor table
 * associated with the given image plane.
 */
PSTable *CurrentPSTable(env, imp)
  VIDSEnvironment	*env;
  int			imp;
{
  if (env->planes[imp].pstable == NULL)
    return &defaultPS;
  return (env->planes[imp].pstable);
}

#define NPSTABS	7		/* Number of pseudocolor tables */

/************************************************************************/
/* PresetPSTable will set a pseudocolor table to a pre-set table.
 * Returns FAIL if the table number is invalid.
 */
int PresetPSTable(pstable, table)
  PSTable	*pstable;
  int		table;
{
  int n1, n2, i, j, inc, tabno;

  static struct {
    int n;
    ColorTriplet colors[32];
  } preset_table[NPSTABS] =
    {
	{ 8,    0,0,255,	0,128,255,	0,255,255,	0,255,0,
		84,200,0,	255,255,0,	255,128,0,	255,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       },
	{ 16,	170,0,255,	115,0,210,	125,70,240,	0,50,255,
		50,110,224,	30,150,200,	0,200,200,	0,185,120,
		0,225,100,	0,255,0,	200,255,150,	255,255,0,
		255,215,0,	255,160,0,	255,100,0,	255,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       },
	{ 32,	0,0,0,		0,0,160,	0,0,200,	0,0,255,
		0,70,230,	0,100,200,	0,130,170,	0,150,150,
		0,170,130,	0,190,100,	0,210,80,	0,220,60,
		0,200,0,	90,180,0,	130,170,0,	220,170,0,
		220,200,0,	255,220,0,	255,240,0,	255,255,0,
		255,140,80,	255,120,80,	255,80,80,	200,60,60,
		220,0,0,	255,0,0,	230,0,100,	245,0,120,
 		255,0,150,	255,50,180,	255,0,255,	255,255,255 },
	{ 16,	0,0,160,	0,0,255,	0,100,200,	0,130,170,
		0,170,130,	0,210,80,	0,200,0,	130,170,0,
		255,220,0,	255,255,0,	255,140,80,	255,80,80,
		255,0,0,	230,0,100,	255,0,150,	255,0,255,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       },
	{ 8,	0,0,255,	0,130,170,	0,170,130,	0,200,0,
		255,255,0,	255,80,80,	255,0,0,	255,0,150,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       },
	{ 6,	0,0,255,	0,170,130,	0,200,0,	255,255,0,
		255,0,0,	255,0,150,
/*filler*/					0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       },
	{ 4,	0,0,255,	0,190,100,	255,255,0,	255,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0,
/*filler*/	0,0,0,		0,0,0,		0,0,0,		0,0,0       }
    };

  if (table == 0)			/* table 0 == bw ramp */
  {
    RampLut(pstable->red);
    RampLut(pstable->green);
    RampLut(pstable->blue);
    return SUCCESS;
  }

  tabno = table - 1;

  if (tabno < 0 || tabno >= NPSTABS)
    return FAIL;				/* invalid table number */

  inc = 256 / preset_table[tabno].n;

  for (i=0; i<preset_table[tabno].n; i++)
  {
    n1 = inc * i;
    n2 = n1 + inc;
    if (n2 + inc > 256)		/* no more room for another increment */
      n2 = 256;
    for (j=n1; j<n2; j++)
    {
      pstable->red[j]   = preset_table[tabno].colors[i].red;
      pstable->green[j] = preset_table[tabno].colors[i].green;
      pstable->blue[j]  = preset_table[tabno].colors[i].blue;
    }
  }
  return SUCCESS;
}
