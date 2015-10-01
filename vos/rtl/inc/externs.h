/************************************************************************/
/* Define global variables as externs for most RTL routines.  Globals	*/
/* are declared (and better commented) in global.c.			*/
/************************************************************************/
#ifndef _EXTERNS_H
#define _EXTERNS_H

#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"

#if VMS_OS
#pragma nostandard		/* turn off portability checks on PUBLICREF */
#endif

#if RTL_USE_TAE

/* TAE parameter blocks.  "parb" is the parblk with the initial program	*/
/* parameters (set up by the main include file), and "iparb" is the	*/
/* interactive parblk set up by xvintract().  The xvparm()-related	*/
/* routines use parb, and the xviparm() routines use iparb.		*/

PUBLICREF struct LARGE_PARBLK parb;
PUBLICREF struct LARGE_PARBLK iparb;

#else

PUBLICREF char *parb, *iparb;		/* dummies for XVPARM */

#endif

#if RTL_USE_TAPE

/* Tape handling variables, mostly copies of TAE global variables */

PUBLICREF char *i_tape[MAXTAPES];	/* tape names		*/
PUBLICREF int i_file[MAXTAPES];		/* current file pos	*/
PUBLICREF int i_rec[MAXTAPES];		/* current record pos	*/
PUBLICREF short int i_count;		/* number of tapes	*/
PUBLICREF char i_open[MAXTAPES]; 	/* open status		*/
PUBLICREF int i_nxt_file[MAXTAPES];	/* "next" file		*/

#endif

PUBLICREF char null_str;

PUBLICREF char default_file_name[];

PUBLICREF char def_err_act[MAX_SHORT_STRING_SIZE];
PUBLICREF char def_err_mess[MAX_STRING_SIZE];

PUBLICREF struct UNITS active_units[N_ACTIVE_UNITS_ALLOWED];

/* Indicates which routine was called for checking accessibility in	*/
/* unit_table (see the "access" column in the definition).		*/
PUBLICREF char current_access;

PUBLICREF unsigned short current_call;	/* current top-level call */
					/* for error_handler      */

PUBLICREF int first_call;    /* true before any routine has been called once. */

PUBLICREF int primary_input_open;
PUBLICREF int primary_input_unit;
PUBLICREF int primary_instance;
PUBLICREF int primary_unit_requested;

PUBLICREF V2_OFFSET primary_allocation_size;	/* for open_disk_output */
PUBLICREF int primary_eof_record;		/* for open_disk_output */

PUBLICREF struct trans no_trans;

PUBLICREF int label_record;

PUBLICREF int v2_error_code;

PUBLICREF int parm_file_unit;

PUBLICREF int applic_lang;

PUBLICREF struct HISTORY history[];

/* see comment in global.c */
PUBLICREF VALUE_TABLE *current_table[N_ACTIVE_UNITS_ALLOWED];

PUBLICREF struct UNIT_TABLE unit_table[];

PUBLICREF VALUE_TABLE default_table[];

/* see comment in global.c */
PUBLICREF VALUE_TABLE *label_table[N_ACTIVE_UNITS_ALLOWED];

PUBLICREF struct UNIT_TABLE label_options[];

PUBLICREF VALUE_TABLE label_default_table[];

#if VMS_OS
#pragma standard
#endif

#endif

