/****************************************************************************
 * Program ISISLAB prints the ISIS label and history objects of an ISIS cube.
 *
 * 26feb92 --lwk-- initial version
 * 20sep95 --lwk-- ported to Alpha
 * 25jun96 --lwk-- don't make 2-D Histo object required, so it can read any
 *		ISIS label, not just NIMS cubes
 * 03nov96 --lwk-- allow tabs as separators (so can read EDR labels)
 * 29sep98 --lwk-- recognize DATA_TABLE for EDRs
 * 25jan99 --lwk-- made portable to Unix:  fixed some function arguments &
 *		initializations, removed ZIA, added size to zvopen
 *  1sep00 --lwk-- cleaned up code for non-NIMS-cube cases
 *  2nov05 --lwk-- removed references to specific objects (except for HISTORY)
 */

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include "xvmaininc.h"
#include "vicmain_c"

#define FUNCTION 
#define MAXLEN 133	/* max. length of a label line */
#define RECL 512	/* record length of PDS file */

char msg[MAXLEN];
char buf[RECL];
int iun, recno;

/****************************************************************************/
void main44(void)

{
  int bptr=0, i, j, k, hstart, nhist, nlab, nrec, stat;
  char opt[8];

	/* tell user the version */
  zvmessage("*** ISISLAB version 2017-08-08 ***",0);

  stat = zvunit( &iun, "INP", 1, NULL);

  /* Unix vicar needs size parameters ... make it big enough to
   * handle any label */
  stat = zvopen( iun, "OPEN_ACT", "SA", "IO_ACT", "SA", "OP", "READ",
   "COND","NOLABELS", "U_NL", 1000, "U_NS", RECL, NULL);

	/* Read first record of the PDS label */
  stat = zvread( iun, buf, "NSAMPS", RECL, NULL);

	/* find the number of label */
  stat = find_keyword( "LABEL_RECORDS", &bptr);
  if (!stat) zmabend(" *** cannot find LABEL_RECORDS item ***");
  stat = get_ival( &nlab, &bptr);
  if (!stat) zmabend(" *** cannot find LABEL_RECORDS value ***");

	/* check for option requested */
  k = 1;
  zvparm(  "OPTION", opt, &i, &j, k, 0);
  if (!strcmp( opt, "LABEL")) goto listl;

  stat = find_keyword( "^HISTORY", &bptr);
  if (stat) {
    stat = get_ival( &hstart, &bptr);
    if (!stat) zmabend(" *** cannot find HISTORY address ***");
  }
  else {
    if (!strcmp( opt, "HISTORY")) zmabend(" no History item present");
    else zvmessage(" no History item present",0);
    strcpy( opt, "LABEL");
	/* Reload first label record */
    stat = zvread( iun, buf, "LINE", 1, "NSAMPS", RECL, NULL);
    goto listl;
  }

	/* History length is defined by start of next item, so we
	 * need to find its address -- we assume that:
	 * 1.  all objects are signalled by a "^" in column 1;
	 * 2. this is unique;
	 * 3. HISTORY is always the first object (if present). */

  stat = find_char( "^", &bptr);
  if (stat) {
    stat = get_ival( &nhist, &bptr);
    if (!stat) zmabend(" *** error getting object address ***");
  }
  nhist -= hstart;

listl:
  if (!strcmp( opt, "HISTORY")) {
    for (i=0; i<nlab; i++)	/* skip the label */
      stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
    bigprnt("********* HISTORY OBJECT **********");
    nrec = nhist;
  }
  else {
    bigprnt("********** LABEL OBJECT **********");
    nrec = nlab;
  }

	/* go read the assigned labels: */
  recno = 0;		/* record # currently being read */
  read_recs( nrec);

  if (!strcmp( opt, "HISTORY")) {
    bigprnt("********** END OF HISTORY OBJECT **********");
    return;
  }
  else {
    bigprnt("********** END OF LABEL OBJECT **********");
    if (!strcmp( opt, "LABEL")) return;
  }

	/* here if OPT = BOTH */
  for (i=0; i < nlab-recno; i++)	/* skip the blank label records */
    stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
  bigprnt("********** HISTORY OBJECT **********");
  recno = 0;
  read_recs( nhist);
  bigprnt("********** END OF HISTORY OBJECT **********");
  return;
}


/***************************************************************************/
FUNCTION read_recs( nrec)
/*
 * read 'nrec' records and print them out
 */
int nrec;
{
  int bptr, i, stat;

  bptr = 1;		/* initialize buffer pointer */
  i = 0;
  stat = 1;		/* non-empty record status */
  while (recno < nrec) {

    if ( i >= MAXLEN-1 ) {
      msg[MAXLEN-1] = 0;
      zvmessage( msg,0);
      i = 0;
    }
    else if (i>1 && msg[i-2] == '\r' && msg[i-1] == '\n' ) {
      msg[i-2] = 0;
      zvmessage( msg,0);
      i = 0;
    }
    msg[i++] = buf[bptr];
    stat = incr(&bptr);
    if (!stat) break;
  }
}


/***************************************************************************/
FUNCTION bigprnt( strng)
/*
 * print a header
 */
char *strng;
{
  zvmessage(" ",0);
  zvmessage( strng,0);
  zvmessage(" ",0);
}


/***************************************************************************/
int FUNCTION find_keyword( keyword, bptr)
/*
 * find keyword and prepare for value reading
 */
char keyword[];
int *bptr;
{
  int count, found, keylength, startptr, stat;

  startptr  = *bptr;			/* Record starting pointer   */
  found     = 0;				/* Item not found yet        */
  keylength = strlen(keyword);			/* Determine itemname length */

  while ( !found ) {
    count = 0;
    stat = 1;
    while ( buf[*bptr] != keyword[count] )
      if ( incr( bptr ) == 0 ) return 0;
    if ( incr( bptr ) == 0 ) return 0;
    count++;

    while ( buf[*bptr] == keyword[count] && count < keylength ) {
      if ( incr( bptr ) == 0 ) return 0;
      else count++;
    }
    if ( ( buf[*bptr]==' ' || buf[*bptr] == '=' || buf[*bptr] == '\t' ) &&
     count == keylength )
      found = 1;
  }

  return 1;
}


/***************************************************************************/
int FUNCTION find_char( keyword, bptr)
/*
 * find a 1-character keyword
 */
char keyword[];
int *bptr;
{
  int found, keylength, startptr, stat;

  startptr  = *bptr;			/* Record starting pointer   */
  found     = 0;			/* Item not found yet        */
  keylength = strlen(keyword);
  if (keylength>1) zmabend(" illegal call to find_char");
  while ( !found ) {
    stat = 1;
    while ( buf[*bptr] != keyword[0] )
      if ( incr( bptr ) == 0 ) return 0;
    if ( incr( bptr ) == 0 ) return 0;
    found = 1;
  }
  return 1;
}


/*****************************************************************************/
int FUNCTION get_ival( intitem, bptr)
/*
 * get the next integer value after bptr
 */
int *intitem, *bptr;

{
  char integer[10];
  int count, stat;

  memset(integer,0,10);
  count = 0;

	/* find first digit */
  while (!isdigit( buf[ *bptr])) {
    stat = incr(bptr);
    if( stat == 0 ) return 0;
  }

	/* continue until last digit is found: */
  while (isdigit( buf[ *bptr]) != 0) {
    integer[count++] = buf[*bptr];
    stat = incr(bptr);
    if( stat == 0 ) return 0;
  }

  *intitem = atoi(integer);		/* convert string into INT   */
  return 1;
}


/*****************************************************************************/
int FUNCTION incr( a)
/*
 * increment pointer to buffer -- when full, read in a new record
 * and increment recno
 *
 * check if new record is all-blank or -zero, if so declare end
 */
int *a;
{
  int blnk, i, stat;

  (*a)++;
  if ( *a < RECL ) return 1;
  else {
    stat = zvread( iun, buf, "NSAMPS", RECL, NULL);
    *a = 0;
    recno++;
	/* check for empty record: */
    blnk = 1;
    for (i=0; i<RECL; i++)
      if (buf[i] != 0 && buf[i] != ' ') blnk = 0;
    if (blnk) return 0;
    return 1;
  }
}	

