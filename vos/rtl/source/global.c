#include "xvmaininc.h"
#include <ctype.h>
#if RTL_USE_TAE
#include "taeconf.inp"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"

#if VMS_OS
#pragma nostandard		/* turn off portability checks on PUBLICDEF */
#endif

#if RTL_USE_TAE

PUBLICDEF struct LARGE_PARBLK parb;  /* TAE parblk for program start params */
PUBLICDEF struct LARGE_PARBLK iparb; /* parblk for XVINTRACT interact params */

#else

PUBLICDEF char *parb, *iparb;	/* dummies for XVPARM routines */

#endif

/* The following used to be in xvmessage - determines if it uses printf	*/
/* or if it tries to use TAE I/O.  Note that we clear it here, then set	*/
/* it in x/zvpblk(), then possibly clear it again if it's determined	*/
/* we're running from the shell.  This is so that programs using the	*/
/* RTL but not using the standard startup (like VIDS) get the correct	*/
/* default of clear (0).						*/

PUBLICDEF int in_vicar = 0;


#if RTL_USE_TAPE

/* Tape handling variables, mostly copies of TAE global variables */

PUBLICDEF char *i_tape[MAXTAPES];       /* tape names           */
PUBLICDEF int i_file[MAXTAPES];         /* current file pos     */
PUBLICDEF int i_rec[MAXTAPES];          /* current record pos   */
PUBLICDEF short int i_count;            /* number of tapes      */
PUBLICDEF char i_open[MAXTAPES];        /* open status          */
PUBLICDEF int i_nxt_file[MAXTAPES];     /* "next" file		*/

#endif

PUBLICDEF char null_str = '\0';

PUBLICDEF char default_file_name[8] = ".z";

PUBLICDEF char def_err_act[MAX_SHORT_STRING_SIZE];	/* Defaults for	*/
PUBLICDEF char def_err_mess[MAX_STRING_SIZE];		/* xveaction()	*/

PUBLICDEF struct UNITS active_units[N_ACTIVE_UNITS_ALLOWED];

/* Indicates which routine (Open, Read, etc.) was called, for checking	*/
/* accessibility of optionals in unit_table.				*/
PUBLICDEF unsigned char current_access=0;

PUBLICDEF unsigned short current_call=0;	/* current top-level call */
						/* for error_handler */

PUBLICDEF int first_call = TRUE;	/* true before any routine has	*/
					/* been called once.		*/

PUBLICDEF int primary_input_open = FALSE;
PUBLICDEF int primary_input_unit;
PUBLICDEF int primary_instance = 1;	/* 0 means don't use primary input */
					/* -1 means use primary_unit_requested*/
PUBLICDEF int primary_unit_requested;	/* unit # requested via x/zvselpiu() */

PUBLICDEF V2_OFFSET primary_allocation_size;	/* for open_disk_output */
PUBLICDEF int primary_eof_record;		/* for open_disk_output */

PUBLICDEF struct trans no_trans =
	{ NULL, NULL, NULL, 1, 1, 1, 0 }; /* No data fmt translations */

PUBLICDEF int label_record;	/* current record # for output labels */

PUBLICDEF int v2_error_code = 0; /* Error code of last error encountered */

PUBLICDEF int parm_file_unit;	/* Unit # for PARMS files */

PUBLICDEF int applic_lang;	/* C_LANG or FORTRAN_LANG */

PUBLICDEF struct HISTORY history[] = {
  {TASK_KEY,	""},
  {"USER",		""},
  {"DAT_TIM",	""}
};

/* current_table and label_table are now arrays of pointers.  Each	*/
/* pointer points to an array of VALUE_TABLE entries, which is		*/
/* dynamically allocated when needed.  This prevents the bulk of apps	*/
/* from consuming too much memory for unused tables, while allowing for	*/
/* huge numbers of available units (note: you can have many more units	*/
/* than open files, due to OS limits).  The tables are still accessible	*/
/* via array syntax, e.g. current_table[unit][value].			*/

PUBLICDEF UNIT_TABLE_ENTRY *current_table[N_ACTIVE_UNITS_ALLOWED];

PUBLICDEF struct UNIT_TABLE unit_table[] =
{
    /* Routine    init   type    size  nel  mode    access     name   */

{ v2_line_size,    YES,  INTEGER,  4,   1,  INPUT,      R|W,    "LINE"      },
{ v2_samp_size,    YES,  INTEGER,  4,   1,  INPUT,      R|W,    "SAMP"      },
{ v2_cond,         YES,  STRING,   40,  1,  INPUT,  O|A,        "COND"      },
{ v2_band_size,    YES,  INTEGER,  4,   1,  INPUT,      R|W,    "BAND"      },
{ v2_unavailable,  YES,  INTEGER,  4,   1,  INPUT,      R|W,    "NLINES"    },
{ v2_nsamp_size,   YES,  INTEGER,  4,   1,  INPUT,      R|W,    "NSAMPS"   },
{ v2_band_size,    YES,  INTEGER,  4,   1,  INPUT,      R|W,    "NBANDS"    },
{    NULL,         YES,  INTADDR,  4,   1,  OUTPUT, O,          "ADDRESS"   },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "SLICE1"    },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "SLICE2"    },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "SLICE3"    },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "SLICE4"    },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "NSLICE1"   },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "NSLICE2"   },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "NSLICE3"   },
{ v2_unavailable,   NO,  ADDR,     4,   1,  LOCAL,       0,     "NSLICE4"   },
{ v2_line_size,     NO,  INTEGER,  4,   1,  INPUT,  O|A|  W,    "U_NL"      },
{ v2_samp_size,     NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_NS"      },
{ v2_band_size,     NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_NB"      },
{ v2_image_size,    NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_N1"      },
{ v2_image_size,    NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_N2"      },
{ v2_image_size,    NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_N3"      },
{ v2_unavailable,   NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_N4"      },
{ v2_image_org,     NO,  STRING,   8,   1,  INPUT,  O|A,        "U_ORG"     },
{ v2_op,            NO,  STRING,   8,   1,  INPUT,  O|A|R,      "OP"        },
{ v2_error_action,  NO,  STRING,   8,   1,  INPUT,  O|A|R,      "OPEN_ACT"  },
{ v2_error_action,  NO,  STRING,   8,   1,  INPUT,  O|A,        "IO_ACT"    },
{ v2_closa,         NO,  STRING,   8,   1,  INPUT,  O|A|      C,"CLOS_ACT"  },
{ v2_format,        NO,  STRING,   8,   1,  SYSTEM, O|A|    G,  "FORMAT"    },
{ v2_format,        NO,  STRING,   8,   1,  INPUT,  O|A|R,      "U_FORMAT"  },
{ v2_error_mess,    NO,  MESSAGE, 120,  1,  INPUT,  O|A,        "OPEN_MES"  },
{ v2_error_mess,    NO,  MESSAGE, 120,  1,  INPUT,  O|A|R,      "IO_MESS"   },
{ v2_type,          NO,  STRING,   8 ,  1,  SYSTEM, O|A|    G,  "TYPE"      },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,          G,  "FLAGS"     },
{    NULL,          NO,  MESSAGE, 250,  1,  OUTPUT,         G,  "NAME"      },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "BUFSIZ"    },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "DIM"       },
{ v2_dim,           NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_DIM"     },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,      0,     "EOL"       },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "RECSIZE"   },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,          G,  "VARSIZE"   },
{    NULL,          NO,  STRING,   8,   1,  SYSTEM,         G,  "ORG"       },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "NL"        },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "NS"        },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "NB"        },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "N1"        },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "N2"        },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "N3"        },
{ v2_unavailable,   NO,  INTEGER,  4,   1,  SYSTEM,         G,  "N4"        },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,          G,  "IMG_REC"   },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "LBLSIZE"   },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,       0,     "BUFSTATE"  },
{    NULL,          NO,  INTEGER,  4,   1,  OUTPUT,         G,  "PIX_SIZE"  },
{    NULL,          NO,   ADDR,    4,   1,  LOCAL,       0,     "LABELS"    },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,       0,     "LBLALLOC"  },
{ v2_format,        NO,  STRING,   8,   1,  INPUT,  O|A,        "O_FORMAT"  },
{ v2_format,        NO,  STRING,   8,   1,  INPUT,  O|A,        "I_FORMAT"  },
{    NULL,          NO,  MESSAGE, 250,  1,  INPUT,            U,"U_NAME"    },
{    NULL,          NO,  INTEGER,  4,   1,  LOCAL,       0,     "EOL_SIZE"  },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "NBB"       },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM,         G,  "NLB"       },
{ v2_binary_size,   NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_NBB"     },
{ v2_binary_size,   NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_NLB"     },
{ v2_u_file,        NO,  INTEGER,  4,   1,  INPUT,  O|A,        "U_FILE"    },
{ v2_method,        NO,  STRING,   8,   1,  INPUT,  O|A|R,      "METHOD"    },
{ v2_error_action,  NO,  STRING,   8,   1,  INPUT,  O|A,        "LAB_ACT"   },
{ v2_error_mess,    NO,  MESSAGE, 120,  1,  INPUT,  O|A,        "LAB_MESS"  },
{ v2_host_chk,      NO,  STRING,   11,  1,  SYSTEM, O|A|    G,  "HOST"      },
{ v2_intfmt_chk,    NO,  STRING,   8,   1,  SYSTEM, O|A|    G,  "INTFMT"    },
{ v2_realfmt_chk,   NO,  STRING,   8,   1,  SYSTEM, O|A|    G,  "REALFMT"   },
{ v2_convert_chk,   NO,  STRING,   8,   1,  INPUT,  O|A,        "CONVERT"   },
{ v2_host_chk,      NO,  STRING,   11,  1,  SYSTEM, O|A|    G,  "BHOST"     },
{ v2_intfmt_chk,    NO,  STRING,   8,   1,  SYSTEM, O|A|    G,  "BINTFMT"   },
{ v2_realfmt_chk,   NO,  STRING,   8,   1,  SYSTEM, O|A|    G,  "BREALFMT"  },
{ v2_convert_chk,   NO,  STRING,   8,   1,  INPUT,  O|A,        "BIN_CVT"   },
{ v2_bltype_chk,    NO,  STRING,   11,  1,  SYSTEM, O|A|    G,  "BLTYPE"    },
{ v2_upd_hist_chk,  NO,  STRING,   8,   1,  INPUT,  O|A|    G,  "UPD_HIST"  },
{ v2_compress_chk,  NO,  STRING,   16,  1,  SYSTEM, O|      G,  "COMPRESS"  },
{    NULL,          NO,  ADDR,     4,   1,  LOCAL,  O|      G,  "LAZYINDEX" },
{    NULL,          NO,  ADDR,     4,   1,  LOCAL,  O|      G,  "ENCODED_BUF"},
{    NULL,          NO,  ADDR,     4,   1,  LOCAL,  O|      G,  "DECODED_BUF"},
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM, O|      G,  "EOCI1"     },
{    NULL,          NO,  INTEGER,  4,   1,  SYSTEM, O|      G,  "EOCI2"     }

};

/* Note: EOCI1 = end of compressed data from start of file - lower end	*/
/* of 64-bit value							*/
/* EOCI2 = end of compressed data from start of file - upper end of	*/
/* 64-bit value								*/

PUBLICDEF VALUE_TABLE_INIT default_table[] =
{
   /*   default                                                     */

	{0},{0},		/* LINE, SAMP  */
	{&null_str},		/* COND        */
	{0},{0},{0},{0},	/* BAND... NBANDS */
        {0},			/* ADDRESS     */
	{0},{0},{0},{0},{0},{0},{0},{0}, /* SLICE1 ... NSLICE4 */
	{0},			/* U_NL        */
	{0},			/* U_NS        */
	{0},			/* U_NB        */
	{0},			/* U_N1        */
	{0},			/* U_N2        */
	{0},			/* U_N3        */
	{0},			/* U_N4        */
	{&null_str},		/* U_ORG       */
	{"READ"},			/* OP          */
	{def_err_act},		/* OPEN_ACT    */
	{def_err_act},		/* IO_ACT      */
	{&null_str},		/* CLOS_ACT    */
	{"BYTE"},			/* FORMAT      */
	{&null_str},		/* U_FORMAT    */
	{def_err_mess},		/* OPEN_MES    */
	{def_err_mess},		/* IO_MESS     */
	{"IMAGE"},		/* TYPE        */
	{0},			/* FLAGS       */
	{&null_str},		/* NAME        */
	{(char *)-1},		/* BUFSIZE     */
	{(char *)3},		/* DIM         */
	{0},			/* U_DIM       */
	{FALSE},			/* EOL         */
	{0},			/* RECSIZE     */
	{0},			/* VARSIZE     */
	{"BSQ"},                  /* ORG         */
	{0},{0},{0},{0},{0},{0},{0},		/* NL ... N4   */
	{0},			/* IMG_REC     */
	{0},			/* LBLSIZE     */
	{0},			/* BUFSTATE    */
	{0},			/* PIX_SIZE    */
	{0},			/* LABELS      */
	{0},			/* LBLALLOC    */
	{&null_str},		/* O_FORMAT    */
	{&null_str},		/* I_FORMAT    */
	{&null_str},		/* U_NAME      */
	{0},			/* EOL_SIZE    */
	{0},{0},{0},{0},		/* NBB,NLB,U_NBB,U_NLB */
	{(char *)-1},		/* U_FILE      */
	{"SEQ"},			/* METHOD      */
	{def_err_act},		/* LAB_ACT     */
	{def_err_mess},		/* LAB_MESS    */
	{NATIVE_HOST_LABEL},	/* HOST        */
	{NATIVE_INTFMT},		/* INTFMT      */
	{NATIVE_REALFMT},		/* REALFMT     */
	{"ON"},			/* CONVERT     */
        {DEF_HOST_LABEL},		/* BHOST       */
	{DEF_INTFMT_LABEL},	/* BINTFMT     */
	{DEF_REALFMT_LABEL},	/* BREALFMT    */
        {"OFF"},			/* BIN_CVT     */
	{&null_str},		/* BLTYPE      */
	{"OFF"},			/* UPD_HIST    */
	{"NONE"},			/* COMPRESS    */
 	{0},                      /* LAZYINDEX   */
 	{0},                      /* ENCODED_BUF */
 	{0},                      /* DECODED_BUF */
        {0},                      /* EOCI1       */
        {0}                       /* EOCI2       */
};

/* Label processing globals:                                          */

/* see current_table comment for structure */

PUBLICDEF LABEL_TABLE_ENTRY *label_table[N_ACTIVE_UNITS_ALLOWED];

PUBLICDEF struct UNIT_TABLE label_options[] =
{
    /* Routine    init   type    size nel  mode    access           name   */

{ v2_hist_name,    YES,  STRING,   9,  1,  INPUT,  A|D|G|I,         "HIST"    },
{ v2_instance,     YES,  INTEGER,  4,  1,  INPUT,  A|D|G|I,         "INSTANCE"},
{ v2_label_format, YES,  STRING,   8,  1,  INPUT,  A|  G,           "FORMAT"  },
{    NULL,         YES,  INTADDR,  4,  1,  OUTPUT, A|  G,           "LEVEL"   },
{    NULL,         YES,  INTADDR,  4,  1,  OUTPUT,     G,           "LENGTH"  },
{ v2_element,      YES,  INTEGER,  4,  1,  INPUT,  A|D|G,           "ELEMENT" },
{ v2_element,      YES,  INTEGER,  4,  1,  INPUT,  A|D|G,           "NELEMENT"},
{    NULL,         YES,  INTADDR,  4,  1,  OUTPUT,   D|G|  HI|RI,   "NRET"    },
{ v2_str_item,     YES,  INTEGER,  4,  1,  INPUT,  A|  G|  HI|RI,   "ULEN"    },
{ v2_error_action, YES,  STRING,   8,  1,  INPUT,  A|D|G|I|HI|RI|NI,"ERR_ACT" },
{ v2_error_mess,   YES,  MESSAGE, 120, 1,  INPUT,  A|D|G|I|HI|RI|NI,"ERR_MESS"},
{ v2_ladd_mode,    YES,  STRING,   8,  1,  INPUT,  A,               "MODE"    },
{    NULL,         YES,  INTADDR,  4,  1,  OUTPUT,       I|      NI,"STRLEN"  },
{ v2_property_name,YES,  STRING,  32,  1,  INPUT,  A|D|G|I,         "PROPERTY"},
{    NULL,         YES,  INTADDR,  4,  1,  OUTPUT,            RI,   "INST_NUM"},
{    NULL,          NO,  INTADDR,  4,  1,  OUTPUT,       I|      NI,"MOD"     },
{    NULL,          NO,  INTEGER,  4,  1,  LOCAL,     0,            "POSITION"}

};

PUBLICDEF VALUE_TABLE_INIT label_default_table[] =
{
	{&null_str},		/* HIST        */
	{(char *)1},		/* INSTANCE    */
	{&null_str},		/* FORMAT      */
	{0},			/* LEVEL       */
	{0},			/* LENGTH      */
	{0},			/* ELEMENT     */
	{0},			/* NELEMENTS   */
	{0},			/* NRET        */
	{0},			/* ULEN        */
	{"D"},			/* ERR_ACT     */
	{def_err_mess},		/* ERR_MESS    */
	{"ADD"},			/* MODE        */
	{0},			/* STRLEN      */
	{&null_str},		/* PROPERTY    */
        {0},                      /* INST_NUM    */
	{0},			/* MOD         */
	{0},			/* POSITION    */
};

#if VMS_OS
#pragma standard
#endif

/************************************************************************/
/*									*/
/* Optional parameter validation routines.				*/
/* All strings passed in here are C strings.				*/
/*									*/
/************************************************************************/

int v2_error_action(VALUE_TYPE value)

/* the value is correct if it is an ascii string of length less than	*/
/* or equal to 3 containing only S, U, or A, or a blank.		*/

{
   char p[10];

   if (strlen(value.s) > 3)
      return BAD_ERR_ACT_VALUE;

   v2_make_upper_case(p,value.s);

   if (strspn( p, "USA ") != strlen(p))
      return BAD_ERR_ACT_VALUE;

   return SUCCESS;
}

/************************************************************************/

int v2_error_mess(VALUE_TYPE value)

/* the value is correct if it is an ascii string of length	*/
/* less than or equal to MAX_STRING_SIZE			*/

{
   int i;

   if (strlen(value.s) > MAX_STRING_SIZE) return ERR_MESS_TOO_LONG;

   for (i=0; i<strlen(value.s); i++)
      if (isascii(value.s[i]) != TRUE)
         return STRING_HAS_NON_ASCII_CHARS;

   return SUCCESS;
}

/************************************************************************/

int v2_image_size(VALUE_TYPE value)

/* value is correct if it is a positive integer		*/

{
   if (value.i <= 0)
      return IMPROPER_IMAGE_SIZE_PARAM;
   return SUCCESS;
}

/************************************************************************/

int v2_samp_size(VALUE_TYPE value)

/* value is correct if it is a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_SAMP_SIZE_PARAM;
   return SUCCESS;
}

int v2_nsamp_size(VALUE_TYPE value)

/* value is correct if it is a positive integer		*/

{
   if (value.i < 0) return IMPROPER_SAMP_SIZE_PARAM;

   /* mms 08-29-2009							*/
   /* Special warning message. I think this is an error, but I'm not	*/
   /* sure. For now, just warn and we'll see where this pops up		*/

   if (value.i == 0) {
/*      zvmessage("NSAMP seems to have been set to 0 in zvread/zvwrite. This causes a full line to be read or written. I think this an an error, but I'm not sure, so we are warning you about it", "MIKE-WARNING"); */
      return IMPROPER_SAMP_SIZE_PARAM;
   }


   return SUCCESS;
}

/************************************************************************/

int v2_line_size(VALUE_TYPE value)

/* value is correct if it is a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_LINE_SIZE_PARAM;
   return SUCCESS;
}

/************************************************************************/

int v2_band_size(VALUE_TYPE value)

/* value is correct if it is a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_BAND_SIZE_PARAM;
   return SUCCESS;
}

/************************************************************************/

int v2_binary_size(VALUE_TYPE value)

/* value is correct if it is a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_BINARY_SIZE_PARAM;
   return SUCCESS;
}

/************************************************************************/

int v2_instance(VALUE_TYPE value)

/* value is correct if it is a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_LABEL_INSTANCE;
   return SUCCESS;
}

/************************************************************************/

int v2_element(VALUE_TYPE value)

/* -1 is also valid (means 'all' for nelement, and 'after last' for element) */

{
   if (value.i < -1) return IMPROPER_ELEMENT_NUMBER;
   return SUCCESS;
}

/************************************************************************/

int v2_image_org(VALUE_TYPE value)

/* value is valid if it's one of "BSQ", "BIL", or "BIP"		*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return BAD_ORG;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"BSQ")) return SUCCESS;
   if (EQUAL(p,"BIL")) return SUCCESS;
   if (EQUAL(p,"BIP")) return SUCCESS;

   return BAD_ORG;
}

/************************************************************************/

int v2_method(VALUE_TYPE value)

/* value is correct if it is a string equal to "SEQ" or "RANDOM"	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_METHOD_STRING;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"SEQ") || EQUAL(p,"RANDOM"))
      return SUCCESS;
   else return IMPROPER_METHOD_STRING;
}

/************************************************************************/

int v2_op(VALUE_TYPE value)

/* value is correct if it is "READ", "WRITE", or "UPDATE"	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_OP_STRING;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"READ") ||
       EQUAL(p,"UPDATE") ||
       EQUAL(p,"WRITE")) return SUCCESS;
   else return IMPROPER_OP_STRING;
}

/************************************************************************/

int v2_format(VALUE_TYPE value)

/*   value is correct if it is one of the strings listed below	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_FORMAT_STRING;

   v2_make_upper_case(p,value.s);

   if ( EQUAL("BYTE",p) ||
        EQUAL("WORD",p) ||
        EQUAL("FULL",p) ||
        EQUAL("HALF",p) ||
        EQUAL("DOUB",p) ||
        EQUAL("LONG",p) ||
        EQUAL("REAL",p) ||
        EQUAL("COMP",p) ||
        EQUAL("COMPLEX",p)) return SUCCESS;

    else return IMPROPER_FORMAT_STRING;
}

/************************************************************************/

int v2_type(VALUE_TYPE value)

/*   value is correct if it is one of the strings listed below	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return BAD_FILE_TYPE;

   v2_make_upper_case(p,value.s);

   if ( EQUAL("IMAGE",p) ||
	EQUAL("PARMS",p) ||
	EQUAL("PARM",p) ||
        EQUAL("PARAM",p) ||
	EQUAL("GRAPH1",p) ||
	EQUAL("GRAPH2",p) ||
	EQUAL("GRAPH3",p) ||
	EQUAL("TABULAR",p))   return SUCCESS;

   else return BAD_FILE_TYPE;

}

/************************************************************************/

int v2_dim(VALUE_TYPE value)

/* Value is correct if it is between 0 and MAX_IMAGE_DIMENSION	*/

{
   if (value.i > 0 && value.i <= MAX_IMAGE_DIMENSION)
      return SUCCESS;
   else return IMPROPER_DIMENSION;
}

/************************************************************************/

int v2_hist_name(VALUE_TYPE value)

/* the value must be an ascii string.  Formerly had to be <= 8 bytes	*/
/* in length but this was changed to allow for longer hist names.	*/
/* Only the first 8 characters are significant, however.		*/

{
   int i;

   for (i=0; i<MIN(strlen(value.s),MAX_HIST_NAME_SIZE); i++)
      if (!isascii(value.s[i]))
         return HIST_NAME_HAS_NON_ASCII_CHAR;
   return SUCCESS;
}

/************************************************************************/

int v2_property_name(VALUE_TYPE value)

/* The value must be an ascii string. */

{
   int i;

   for (i=0; i<strlen(value.s); i++)
      if (!isascii(value.s[i]))
         return STRING_HAS_NON_ASCII_CHARS;
   return SUCCESS;
}

/************************************************************************/

int v2_label_format(VALUE_TYPE value)

/* Value must be one of "INT", "REAL", "DOUB", or "STRING"	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return ILLEGAL_FORMAT_REQUEST;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"INT")) return SUCCESS;
   if (EQUAL(p,"REAL")) return SUCCESS;
   if (EQUAL(p,"DOUB")) return SUCCESS;
   if (EQUAL(p,"STRING")) return SUCCESS;

   return ILLEGAL_FORMAT_REQUEST;
}

/************************************************************************/

int v2_str_item(VALUE_TYPE value)

/* Value must be less than the max label item size.	*/

{
   if (value.i > 0 && value.i <= MAX_LABEL_VALUE_SIZE)
      return SUCCESS;
   else return IMPROPER_LENGTH;
}

/************************************************************************/

int v2_cond(VALUE_TYPE value)

/* Value must be a string of the appropriate length	*/

{
   if (strlen(value.s) > MAX_STRING_SIZE)
      return COND_STRING_TOO_LONG;
   return SUCCESS;
}

/************************************************************************/

int v2_closa(VALUE_TYPE value)

/* Value must be a string of the appropriate length	*/

{
   if (strlen(value.s) > MAX_STRING_SIZE) return ACT_STRING_TOO_LONG;
   return SUCCESS;
}

/************************************************************************/

int v2_u_file(VALUE_TYPE value)

/* Value must be a non-negative integer		*/

{
   if (value.i < 0) return IMPROPER_FILE_NUMBER;
   return SUCCESS;
}

/************************************************************************/

int v2_ladd_mode(VALUE_TYPE value)

/* value is correct if it is equal to "ADD", "INSERT", or "REPLACE"	*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_MODE_STRING;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"ADD") ||
       EQUAL(p,"INSERT") ||
       EQUAL(p,"REPLACE")) return SUCCESS;
   else return IMPROPER_MODE_STRING;
}

/************************************************************************/

int v2_unavailable(VALUE_TYPE UNUSED(value))

/* This error message is meant to be more informative than UNDEFINED_OPTIONAL */
/* for a documented function that has not been implemented.		      */
{
   return NOT_IMPLEMENTED;
}

/************************************************************************/

int v2_convert_chk(VALUE_TYPE value)

/* value is correct if it is equal to "ON" or "OFF" */

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_CONVERT_SETTING;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"ON") ||
       EQUAL(p,"OFF")) return SUCCESS;
   else return IMPROPER_CONVERT_SETTING;
}

/************************************************************************/

int v2_intfmt_chk(VALUE_TYPE value)

/* value is correct if it is equal to "LOW", "HIGH", "NATIVE", or "LOCAL" */

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_INTFMT;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"LOW") ||
       EQUAL(p,"HIGH") ||
       EQUAL(p,"NATIVE") ||
       EQUAL(p,"LOCAL"))
      return SUCCESS;
   else return IMPROPER_INTFMT;
}

/************************************************************************/

int v2_realfmt_chk(VALUE_TYPE value)

/* value is correct if it is equal to "VAX", "IEEE", "RIEEE", "NATIVE",	*/
/* or "LOCAL"								*/

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_REALFMT;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"VAX") ||
       EQUAL(p,"IEEE") ||
       EQUAL(p,"RIEEE") ||
       EQUAL(p,"NATIVE") ||
       EQUAL(p,"LOCAL"))
      return SUCCESS;
   else return IMPROPER_REALFMT;
}

/************************************************************************/

int v2_host_chk(VALUE_TYPE value)

/* the value is correct if it is an ascii string of length	*/
/* less than or equal to MAX_SHORT_STRING_SIZE			*/

{
   int i;

   if (strlen(value.s) > MAX_SHORT_STRING_SIZE) return HOST_TOO_LONG;

   for (i=0; i<strlen(value.s); i++)
      if (isascii(value.s[i]) != TRUE)
         return STRING_HAS_NON_ASCII_CHARS;

   return SUCCESS;
}

/************************************************************************/

int v2_bltype_chk(VALUE_TYPE value)

/* the value is correct if it is an ascii string of length	*/
/* less than or equal to MAX_SHORT_STRING_SIZE			*/

{
   int i;

   if (strlen(value.s) > MAX_SHORT_STRING_SIZE) return BLTYPE_TOO_LONG;

   for (i=0; i<strlen(value.s); i++)
      if (isascii(value.s[i]) != TRUE)
         return STRING_HAS_NON_ASCII_CHARS;

   return SUCCESS;
}

/************************************************************************/

int v2_upd_hist_chk(VALUE_TYPE value)

/* value is correct if it is equal to "ON" or "OFF" */

{
   char p[MAX_STRING_SIZE+1];

   if (strlen(value.s) > MAX_STRING_SIZE)
      return IMPROPER_UPD_HIST_SETTING;

   v2_make_upper_case(p,value.s);

   if (EQUAL(p,"ON") ||
       EQUAL(p,"OFF")) return SUCCESS;
   else return IMPROPER_UPD_HIST_SETTING;
}

/************************************************************************/

int v2_compress_chk(VALUE_TYPE value)

/* value is correct if it is equal to "NONE" or "BASIC" or "BASIC2" */
/* or just "NONE" if compression is disabled.			    */

{
   char p[COMPRESSION_NAME_LEN+1];
   
   if (strlen(value.s) > COMPRESSION_NAME_LEN)
      return INVALID_COMPRESSION_TYPE;

   v2_make_upper_case(p,value.s);

#if RTL_USE_COMPRESSION
   if (EQUAL(p,"BASIC") ||
       EQUAL(p, "BASIC2") ||
       EQUAL(p, "NONE")) return SUCCESS;
   else return INVALID_COMPRESSION_TYPE;
#else
   if (EQUAL(p,"NONE")) return SUCCESS;
   else return INVALID_COMPRESSION_TYPE;
#endif
}


