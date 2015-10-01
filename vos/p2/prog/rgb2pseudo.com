$!****************************************************************************
$!
$! Build proc for MIPL module rgb2pseudo
$! VPACK Version 1.9, Monday, December 07, 2009, 17:00:48
$!
$! Execute by entering:		$ @rgb2pseudo
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module rgb2pseudo ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rgb2pseudo.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("rgb2pseudo.imake") .nes. ""
$   then
$      vimake rgb2pseudo
$      purge rgb2pseudo.bld
$   else
$      if F$SEARCH("rgb2pseudo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rgb2pseudo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rgb2pseudo.bld "STD"
$   else
$      @rgb2pseudo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rgb2pseudo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rgb2pseudo.com -mixed -
	-s rgb2pseudo.c -
	-i rgb2pseudo.imake -
	-p rgb2pseudo.pdf -
	-t tstrgb2pseudo.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rgb2pseudo.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Revision History:
   6-March 1995 ... CRI ... MSTP S/W Conversion (VICAR Porting) */

#include "vicmain_c"
#include "ibisfile.h"
#include "ibiserrs.h"
#include <stdlib.h>
#include <string.h>

void mybcopy(char *src, char *dst, int len);
void mybzero(char *buf, int len);

#define EQUAL(x,y) (strcmp((x),(y)) == 0)


void main44(void)
{
  int	inputs[3];	/* unit numbers for input files	*/
  int	output;		/* unit number for output file	*/
  int	lutUnit;	/* unit number for lut file	*/
  int	ibis;   	/* file handle for lut file     */
  int	i;		/* increment variable		*/
  int	nInputs;	/* number of input files	*/
  int	status;		/* status return value		*/
  int	nbits;		/* number of bits for color hist*/
  int   num_cols,col_length,num_rows;
  char *iformat,*iorg,*imode;
  int   srow,nrows,column;
  int	nl,ns,nb;
  char	org[4];
  char format[10];
  float floatBuf[256];
  unsigned char rLut[256], gLut[256], bLut[256];
  unsigned char *inAddr[3];
  unsigned char *outFile, *lutFile;
  
  zifmessage ("RGB2PSEUDO version 6-MAR-1995");

  zveaction("SA", "");

  zvpcnt("INP", &nInputs);

  if (nInputs == 2)
  {
    zvmessage("Sorry, only 3 files or 1 3-band file allowed","");
    zabend();
  }
  for (i = 1; i <= nInputs; i++)
  {
    zvunit(&inputs[i - 1], "INP", i, NULL);

    zvopen(inputs[i - 1], "ADDRESS", &inAddr[i-1], NULL);
  }

  zvget(inputs[0], "NL", &nl, "NS", &ns, "NB", &nb, "ORG", org,
        "FORMAT", format, NULL);

  if (!EQUAL(format, "BYTE"))
  {
    zvmessage("Sorry, only BYTE format inputs are allowed.","");
    zabend();
  }
  if (nInputs == 1)
  {
    if (nb < 3)
    {
      zvmessage("Sorry, three bands are required for input.","");
      zvmessage("Please use a multiband file or three separate inputs","");
      zabend();
    }
    if (EQUAL(org, "BSQ"))
    {
      inAddr[1] = inAddr[0] + (nl * ns);
      inAddr[2] = inAddr[1] + (nl * ns);
    }
    else
    {
      zvmessage("Sorry, only BSQ organization is currently supported","");
      zabend();
    }
  }

  zvunit(&output, "OUT", 1, NULL);
  zvopen(output, "OP", "WRITE", "U_NL", nl, "U_NS", ns, 
         "U_NB", 1, "ADDRESS", &outFile, NULL);

  zvp("NBITS", &nbits, &i);
  zvmessage("Starting conversion...","");
  rgb_to_image_and_luts(inAddr[0], inAddr[1], inAddr[2], outFile, 
                        rLut, gLut, bLut, nl, ns, nbits);

  for (i = 0; i < nInputs; i++) {
    zvclose(inputs[i], NULL);
  }
  zvmessage("Writing output...","");
  zvclose(output, NULL);

  /* Get unit number and Open IBIS file for output */
  zvunit(&lutUnit, "OUT", 2, NULL);
  num_cols   = 3;
  col_length = 256;
  num_rows   = 256;
  iformat    = NULL;
  iorg       = IORG_COLUMN;
  imode      = IMODE_WRITE;
  status = IBISFileOpen (lutUnit, &ibis, imode, num_cols, 
                num_rows, iformat , iorg);	/* open output	*/
  if (status != 1) {
     IBISSignalU(ibis,status,1);
  }

  for (i = 0; i < 256; i++) {
    floatBuf[i] = (float) rLut[i];
  }
/*          unit  */
/*          |        column  */
/*          |        |    column-length  */
/*          |        |    |    column_data  */
/*          |        |    |    |             */
/*  putcol(&lutUnit, &1, &256, floatBuf);      */
  column = 1;
  srow   = 1;
  nrows  = 256;
  status = IBISColumnWrite (ibis, (char *)floatBuf, column, srow, nrows);
  if (status != 1) {
     IBISSignalU(ibis,status,1);
  }

  for (i = 0; i < 256; i++) {
    floatBuf[i] = (float) gLut[i];
  }

/*  putcol(&lutUnit, &2, &256, floatBuf); */
  column = 2;
  status = IBISColumnWrite (ibis, (char *)floatBuf, column, srow, nrows);
  if (status != 1) {
     IBISSignalU(ibis,status,1);
  }

  for (i = 0; i < 256; i++) {
    floatBuf[i] = (float) bLut[i];
  }

  column = 3;
  status = IBISColumnWrite (ibis, (char *)floatBuf, column, srow, nrows);
  if (status != 1) {
     IBISSignalU(ibis,status,1);
  }

  status = IBISFileClose (ibis, ICLOSE_UKEEP);

  if (status != 1) {
     IBISSignalU(ibis,status,1);
  }

  zvmessage("All done!","");
  return;
}
/****************************************************************************/
/* FullColor.c -- Convert 24 bit RGB images into 8 bit pseudocolor
 *
 *		Original software by	Alan Mazer	fall 1988
 *		converted to VICAR by	Dan Stanfill	Jan 1989
 */

#if THINK_C
#include <storage.h>
#endif
#ifndef NULL
#define NULL ((void*)0)
#endif

struct pix_list_def {
    int pix_num;
    struct pix_list_def *next;
};

struct entry_def {
    unsigned char red,green,blue;
    unsigned int color;
    int count;
    struct pix_list_def pix_list_head;
    struct pix_list_def *last;
};

    /* Rgb_to_image_and_luts - convert 24 bit RGB image into 8 bit composite */

rgb_to_image_and_luts(red,green,blue,composite,red_lut,green_lut,blue_lut,nl,ns,bits_used)
unsigned char *red,*green,*blue,*composite,*red_lut,*green_lut,*blue_lut;
int nl,ns,bits_used;
{
    int length,next_free_entry,compare(),max_error;
    struct entry_def *table,temp_table_entry;
    struct pix_list_def *pix_space;

	/* build table, initially assuming we only need 4 bits */

    table = NULL;
	    /* allocate space for color table */

    (void)allocate_space(&table,&pix_space,bits_used,nl,ns,&length);

	    /* build table from planes */

    (void)output_message("building histogram");
    next_free_entry = build_table(red,green,blue,table,pix_space,length,nl,
	    ns,bits_used);

	    /* force background color to be second for now */

    mybcopy((char *)(table+1),(char *)&temp_table_entry,
	    sizeof(temp_table_entry));
    mybcopy((char *)(table+next_free_entry-1),(char *)(table+1),
	    sizeof(temp_table_entry));
    mybcopy((char *)&temp_table_entry,(char *)(table+next_free_entry-1),
	    sizeof(temp_table_entry));

	    /* sort to determine colors to use */

    (void)output_message("sorting");
    ShellSort((char *)(table+2),next_free_entry-2,sizeof(*table),compare);

	    /* merge infrequent colors to nearest class */

    (void)output_message("merging");
    max_error = merge(table,next_free_entry);

	/* move background to start and foreground to end */

    mybcopy((char *)table,(char *)&temp_table_entry,    		/* t <- #0    */
	sizeof(temp_table_entry));
    mybcopy((char *)(table+1),(char *)table,	        	/* #0 <- #1   */
	sizeof(temp_table_entry));
    mybcopy((char *)(table+255),(char *)(table+1),	        /* #1 <- #255 */
	sizeof(temp_table_entry));
    mybcopy((char *)&temp_table_entry,(char *)(table+255),
	sizeof(temp_table_entry));				/* #255 <- t  */

	/* get look up tables and composite image */

    (void)make_composite(table,red_lut,green_lut,blue_lut,composite,nl,ns,
	bits_used);

	/* free allocated space */

    free((char *)table);
    free((char *)pix_space);

    return(0);
}

    /* Allocate_space - get space for tables, freeing previous if any */

allocate_space(table_ptr,pix_space_ptr,bits_used,nl,ns,length_ptr)
struct entry_def **table_ptr;
struct pix_list_def **pix_space_ptr;
int bits_used,nl,ns,*length_ptr;
{
    int int_temp;

    if (*table_ptr != NULL) {
	free((char *)*table_ptr);
	free((char *)*pix_space_ptr);
    }
    int_temp = 1<<bits_used;
    *length_ptr = int_temp * int_temp * int_temp;
    *table_ptr = (struct entry_def *)malloc((unsigned int)*length_ptr *
	sizeof(struct entry_def));
    mybzero((char *)*table_ptr,*length_ptr * sizeof(struct entry_def));

    *pix_space_ptr = (struct pix_list_def *)malloc((unsigned int)nl*ns*
	sizeof(struct pix_list_def));
    return(0);
}

    /* Build_table - takes RGB planes and allocated space and creates table */
    /* of colors used.  Each color has associated with it a list of pixels  */
    /* using that color.  The function returns the number of colors found.  */

build_table(red,green,blue,table,pix_space,length,nl,ns,bits_used)
unsigned char *red,*green,*blue;
struct pix_list_def *pix_space;
struct entry_def *table;
int length,nl,ns,bits_used;
{
    unsigned char pix_red,pix_green,pix_blue;
    unsigned int shift_left_red[256],shift_left_green[256],
	shift_left_blue[256];
    int int_temp,pixel,source,next_free_entry;
    unsigned int color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    register struct pix_list_def *next_pix_list;
    register int nl_ns;
    register struct entry_def *tree_pos;

	/* make tables for right and left shifts */

    for (int_temp = 0;int_temp < 256;int_temp++) {
	shift_left_red[int_temp] =
	    (int_temp >> (8-bits_used)) << (2*bits_used);
	shift_left_green[int_temp] =
	    (int_temp >> (8-bits_used)) << bits_used;
	shift_left_blue[int_temp] = (int_temp >> (8-bits_used));
    }

	/* initialize table */

    next_free_entry = 0;
    for (pix_red=0;pix_red < (unsigned char)(1<<bits_used);pix_red++)
	for (pix_green=0;pix_green < (unsigned char)(1<<bits_used);pix_green++)
	    for (pix_blue=0;pix_blue < (unsigned char)(1<<bits_used);pix_blue++) {
		int_temp = pix_red << 2*bits_used |
		    pix_green << bits_used | pix_blue;
		(table+int_temp)->color = int_temp;
		(table+int_temp)->red = pix_red;
		(table+int_temp)->green = pix_green;
		(table+int_temp)->blue = pix_blue;
		(table+int_temp)->last = &((table+int_temp)->pix_list_head);
	    }

	/* build table */

        red_ptr = red-1;
        green_ptr = green-1;
        blue_ptr = blue-1;
        next_pix_list = pix_space-1;
        nl_ns = nl*ns;

    for (pixel=0;pixel<nl_ns;pixel++) {
	color = *(shift_left_red + *++red_ptr) |
	    *(shift_left_green + *++green_ptr) |
	    *(shift_left_blue + *++blue_ptr);


	tree_pos = table+color;
	tree_pos->count++;
	(++next_pix_list)->pix_num = pixel;

	next_pix_list->next = NULL;
	tree_pos->last->next = next_pix_list;
	tree_pos->last = next_pix_list;

    }

	/* compress table */

    source = 1;
    for (next_free_entry = 1;;next_free_entry++) {
	while ((table+source)->count == 0 && source < length) source++;
	if (source == length) break;
	mybcopy((char *)(table+source),(char *)(table+next_free_entry),
	    sizeof(*table));
	source++;
    }

    if ((table+next_free_entry-1)->pix_list_head.next !=
	    (table+length-1)->pix_list_head.next) {
	mybcopy((char *)(table+length-1),(char *)(table+next_free_entry),
	    sizeof(*table));
	next_free_entry++;
    }

    (void)output_message("found %d colors",next_free_entry);
    return(next_free_entry);
}

    /* Merge - takes color table generated by Build_Table and merges */
    /* infrequently used colors in with most popular.                */

merge(table,num_colors)
struct entry_def *table;
int num_colors;
{
    int unclassed_index,best_dist,best_index,dist;
    int j,square[513],int_temp,max_error;
    register int *square_off;
    register struct entry_def *table_ptr;
    register int red_diff,green_diff,blue_diff;
    register unsigned char pix_red,pix_green,pix_blue;

	/* make table for squares of differences and provide ptr to zero^2 */

    for (int_temp = -256;int_temp <= 256;int_temp++)
	square[int_temp+256] = int_temp*int_temp;
    square_off = &square[256];

	/* do merge */

    max_error = 0;
    for (unclassed_index=256;unclassed_index < num_colors;
	    unclassed_index++) {
	pix_red = (table+unclassed_index)->red;
	pix_green = (table+unclassed_index)->green;
	pix_blue = (table+unclassed_index)->blue;
	red_diff = pix_red - table->red;
	green_diff = pix_green - table->green;
	blue_diff = pix_blue - table->blue;
	best_dist = *(square_off+red_diff) + *(square_off+green_diff) +
	    *(square_off+blue_diff);
	best_index = 0;
	table_ptr = table+1;
	for (j=1;j < 256;j++) {
	    red_diff = pix_red - table_ptr->red;
	    green_diff = pix_green - table_ptr->green;
	    blue_diff = pix_blue - table_ptr->blue;
	    dist = *(square_off+red_diff) + *(square_off+green_diff) +
		*(square_off+blue_diff);
	    if (dist < best_dist) {
		best_dist = dist;
		best_index = j;
	    }
	    table_ptr++;
	}
	(table+best_index)->last->next =
	    (table+unclassed_index)->pix_list_head.next;
	(table+best_index)->last =
	    (table+unclassed_index)->last;
	(table+best_index)->count += (table+unclassed_index)->count;
	if (best_dist > max_error) max_error = best_dist;
    }
    return(max_error);
}

    /* Make_composite - takes table containing colors and associated */
    /* pixels, fills in look up tables, and creates 8 bit image.     */

make_composite(table,red_lut,green_lut,blue_lut,composite,nl,ns,bits_used)
struct entry_def *table;
unsigned char *red_lut,*green_lut,*blue_lut;
unsigned char *composite;
int nl,ns,bits_used;
{
    int dn;
    struct pix_list_def *temp_pix_ptr;

	/* determine lookup table */

    for (dn=0;dn < 255;dn++) {
	red_lut[dn] =
	    ((table+dn)->red << (8-bits_used)) | (0xff >> bits_used);
	green_lut[dn] =
	    ((table+dn)->green << (8-bits_used)) | (0xff >> bits_used);
	blue_lut[dn] =
	    ((table+dn)->blue << (8-bits_used)) | (0xff >> bits_used);
    }
    red_lut[255] = green_lut[255] = blue_lut[255] = 0; /* don't round up */

	/* create new red, green, and blue planes */

    mybzero((char *)composite,nl*ns);

    (void)output_message("creating composite image");
    for (dn=0;dn < 256;dn++) {
	temp_pix_ptr = (table+dn)->pix_list_head.next;
	while (temp_pix_ptr != NULL) {
	    *(composite+temp_pix_ptr->pix_num) = dn;
	    temp_pix_ptr = temp_pix_ptr->next;
	}
    }
    return(0);
}

compare(entry1,entry2)
struct entry_def *entry1,*entry2;
{
    int return_value;
    return_value = (entry1->count < entry2->count? 1:
	(entry1->count == entry2->count? 0:-1));
    return(return_value);
}

void mybzero(buf, len)
  char *buf;
  int len;
{
  int i;
  
  for (i = 0; i < len; i++, buf++)
    *buf = 0;
  return;
}

void mybcopy(src, dst, len)
  char *src,*dst;
  int	len;
{
#if THINK_C
  BlockMove(src, dst, (long)len);
#endif
#if VMS_OS
  memcpy (dst, src, len);
#endif
#if UNIX_OS
  memcpy (dst, src, len);
#endif
  return;
}

output_message(msg, p1, p2, p3, p4, p5, p6, p7, p8)
  char *msg;
  int	p1,p2,p3,p4,p5,p6,p7,p8;
{
  char msgBuf[256];
  sprintf(msgBuf, msg, p1,p2,p3,p4,p5,p6,p7,p8);
#if VMS_OS
  zvmessage(msgBuf,"");
#endif
#if UNIX_OS
  zvmessage(msgBuf,"");
#endif
#if THINK_C
  printf("%s\n", msgBuf);
#endif
  return;
}
/**********************************************************************/
/* ShellSort -- Perform a shellsort on an array of elements.  The user
 * provides the compare routine for ShellSort to use.  Calling sequence
 * and usage is identical to the standard C routine qsort().
 *
 * Dan Stanfill		Jan 1989
 */
int ShellSort(start, nelem, size, compar)
  char *start;		/* starting addr for sort	*/
  long	nelem;		/* number of elements to sort	*/
  int	size;		/* size of one element in bytes	*/
  int	(*compar)();	/* addr of compare routine	*/
{
  int	i,j,h;
  char  *v;
  
  v = (char *)malloc(size);
  if (v == 0) return 0;

  for (h = 1; h <= nelem; h = 3 * h + 1);
  while (h >= 3)
  {
    h /= 3;
    for (i = h; i < nelem; i++)
    {
      mybcopy(start + size * i, v, size);
      j = i;
      while ((*compar)(start + size * (j - h), v) > 0)
      {
	mybcopy(start + size * (j - h), start + size * j, size);
	j -= h;
	if (j < h) break;
      }
      mybcopy(v, start + size * j, size);
    }
  }
  return 1;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rgb2pseudo.imake
#define PROGRAM rgb2pseudo

#define R2LIB

#define MODULE_LIST rgb2pseudo.c

#define MAIN_LANG_C
#define USES_C
#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create rgb2pseudo.pdf
process help=*
parm inp (string,40) count=(1:3)
parm out (string,40) count=2
parm nbits int valid=(2:5) default=4
end-proc
.title
rgb2pseudo - convert RGB triplet to pseudo-color
.help
rgb2pseudo takes a 24-bit RGB triplet (either 3 separate files or 3 bands
in a single file) and converts it to 8 bit (256 color) pseudo-color.  The
resultant output consists of two files:  one BYTE format image and one
VIDS format (IBIS tabular file) pseudo-color table file.

The purpose of rgb2pseudo is to select the "best" 256 colors with which to
view a given image, so that it may be displayed in color on workstations
which only display 256 colors simultaneously.

To view the pseudo-color image:

Pixel Pusher (Macintosh):  Use "Open New Transfer" in the "File" menu
    to open each of the two output files.  For the Lookup table file,
    use the "Attach to:" pop-up menu to select the image window, and
    click on "Attach".

VIDS (VICAR systems):  Display the image file with JDISP.  Use the
    JLOAD-PSEUDO command to load the lookup table file as a pseudo-
    color table.  Use the JPSEUDO command to put the device in pseudo-
    color mode.

HISTORY:  6 March 1995   J. Turner (CRI)  Made portable for UNIX

.level1
.vari inp
24 bit color input file names:
3 files (red, green, blue) OR
1 3-band file
.vari out
Two output files: (image,lut)
.vari nbits
Number of most significant bits
of each color for histogram.
.level2
.vari inp
INP specifies the input file name(s).  Normally, INP would be three
BYTE images, representing (red, green, blue) respectively.  It could
also be a single 3-band file.
.vari out
OUT specifies the names of the two output files.  The first output file
is the name of an image file which will be a BYTE format VICAR image.
The DN of each pixel represents a color given by the pseudo-color table
in the second output file.

The second output file is the name of a lookup table file which contains
the 256 colors to be used to display the image.  It is an IBIS "Tabular"
file, containing the three (red, green, blue) lookup tables.  This file
is compatible with VIDS (using JLOAD-PSEUDO or JLOAD-STRETCH) and 
Pixel Pusher (on the Macintosh; use "Open New Transfer" to view it).
.vari nbits
NBITS is the magic parameter. It is used as follows.

The color of each pixel in the input is determined by taking the NBITS
most significant bits of the red, green, and blue pixel values, and
shifting them together to fit into one integer.  A histogram is collected
giving the frequency of each color in the input, and the 254 most common
colors (plus black and white) are selected.  Each of the remaining colors
is then mapped to its nearest match (euclidean distance) in the 256 chosen.

NBITS should be 3, 4, or 5.  Choosing 3 bits will force the 256 colors
to be further apart from one another, and often gives the best results
on a picture with a lot of disparate colors.  Five bits will tend to allow
the chosen colors to be more similar, and is thus the best choice when 
an image contains gradual shading.  For a quick look, 4 bits almost always
gives reasonable results (and is thus the default). 

I have found, however, that if you want the best, it pays to run all three
and compare the results.
.end
$ Return
$!#############################################################################
$Test_File:
$ create tstrgb2pseudo.pdf
procedure
refgbl $echo
body 
!@!@let _onfail="continue"
let _onfail="return"
let $echo="yes"
!
gen out=r.img nl=5 ns=5 ival=0
gen out=g.img nl=5 ns=5 ival=10
gen out=b.img nl=5 ns=5 ival=20
rgb2pseudo inp=(r.img,g.img,b.img) out=(pseu.img,ibis.img) nbits=4
list pseu.img
!qrep inp=ibis.img
ibis-list inp=ibis.img
!
end-proc
$ Return
$!#############################################################################
