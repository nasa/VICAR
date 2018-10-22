/*===========================================================================*
 |  DSNID.C -- Returns the DSN Source Station ID # and station name          |
 |       given its GCF code. 						     |
 |  Reference: 820-13 (OPS-6-6 for Mark III and OPS-6-8 for Mark IV)         |
 |	       1839-10 A-13.						     |
 |
 |   Revision  History:
 |      Date       FR #            Description
 |  --------  -----  ------------------------------------------------------- 
 |   8-1-94        ---      GAC - Ported to Unix. 
 *===========================================================================*/
#include "mve.h"

int zdsnid(station_code,mrk3_station_id,mrk4_station_id,station_name)
int *station_code;	/* Input GCF source code */
int *mrk3_station_id;	/* DSN Mark III Source Station ID # (= 0 if unknown) */
int *mrk4_station_id;	/* DSN Mark  IV Source Station ID # (= 0 if unknown) */
char station_name[];	/* Source station name */
{
	int		i, icode, jcode;
static 	struct src
	{
	short	code;			/* GCF source code */
	char 	station_name[13];	/* Station name */
	short 	m3_station;  		/* Mark 3 station id */
	short 	m4_station;		/* Mark 4 station id */
	} 		dcode[] = {

  {0x13,	"MCCC SIM    ",	27, 81},
  { 0x14,	"Int MCCC    ",	99, 84},
  { 0x19,	"DSS-14      ",	14,  0},
  { 0x21,	"OCC CHLTN,UK",	53, 53},
  { 0x26,	"MIL 71      ",	71, 71},
  { 0x27,	"ESOCC       ",	54, 54},
  {0x29,	"DSS-11      ",	11, 0},
  {0x2D,	"PARKS, AUST ",	49, 49},
  {0x46,	"CAPE A0     ",	70, 73},
  {0x51,  "MGN BTG     ",  0,  0},
  {0x57,	"Goldstone-10",  0, 10},
  {0x67,	"WILHEIM     ",	67, 50},
  {0x76,	"DSS-62      ",	62, 62},
  {0x77,	"NOCC        ",	24, 24},
  {0x7A,	"DSS-63      ",	63,  0},
  {0x7C,	"GCF TEST    ",	20, 29},
  {0x7F,	"DSN GCF TEST",	20, 20},
  {0x81,	"Goldstone-12",	 0, 12},
  {0x82,	"Goldstone-14",	 0, 14},
  {0x83,	"Goldstone-15",	 0, 15},
  {0x84,	"Goldstone-16",	 0, 16},
  {0x85,  "JPL MCCC RT ",  0,  0},
  {0x86,	"DSS-43      ",	43,  0},
  {0x89,	"DSS-13      ",	13, 13},
  {0x8C,	"MCCC AMPTEE ",	 0,  0},
  {0x8E,	"MCCC/GSOC   ",	 0,  0},
  {0x90,  "Canberra-40 ",  0,  0},
  {0x91,  "JPL         ",  0,  0},
  {0x92,	"Canberra-42 ",	42, 42},
  {0x93,	"Canberra-43 ",  0, 43},
  {0x94,	"Canberra-45 ",  0, 45},
  {0x95,	"Canberra-46 ",  0, 46},
  {0xA4,	"DSN/MCCC    ",	98, 83},
  {0xA7,	"GSOC        ",	69, 52},
  {0xB7,	"DSS DRG-ATRS",	25, 25},
  {0xB8,	"DSS DRG-IDR ",	25, 25},
  {0xBC,	"NDPA        ",	22, 22},
  {0xBD,  "GLL Blocker ",  0,  0},
  {0xC2,	"DSS-44      ",	44, 44},
  {0xD2,  "VLA N.M.    ",  0, 19},
  {0xD7,  "Madrid-60   ",  0, 60},
  {0xDB,	"Goldstone-17",	 0, 16},
  {0xE5,	"Madrid-62   ",	 0, 62},
  {0xF3,	"Madrid-61   ",	 0, 61},
  {0xF4,	"Madrid-63   ",	 0, 63},
  {0xF6,  "Madrid-66   ",  0, 66},
  {0xFD,  "Madrid-65   ",  0, 65},
  {0,	"Unknown     ",	 0,  0}};

  icode = *station_code;
  for (i=0; i<256 ;i++)
  {
    jcode = (int) dcode[i].code;
    if (jcode == 0) break;
    if (icode == jcode)
    {
      if (mrk3_station_id) *mrk3_station_id = dcode[i].m3_station;
      if (mrk4_station_id) *mrk4_station_id = dcode[i].m4_station;
      if (station_name) zmve(  1, 12, dcode[i].station_name,    station_name , 1, 1 );
      return(1);
    }
  }

  if (mrk3_station_id) *mrk3_station_id = 0;
  if (mrk4_station_id) *mrk4_station_id = 0;
  if (station_name) zmve(  1, 12, dcode[i].station_name,    station_name,  1, 1 ); 
  return(0);
}
