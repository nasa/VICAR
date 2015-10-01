#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine determines which translation(s) to use.  Translations	*/
/* may be required due to different data types, different hosts, or	*/
/* both.  Two translation steps may be required, in which case a	*/
/* special routine is called instead of calling the translations	*/
/* directly.								*/

#define TYPE_BYTE	0
#define TYPE_HALF	1
#define TYPE_FULL	2
#define TYPE_REAL	3
#define TYPE_DOUB	4
#define TYPE_COMP	5
#define N_TYPES		6

#define IHOST_LOW	0
#define IHOST_HIGH	1
#define N_IHOST		2

#define RHOST_VAX	0
#define RHOST_IEEE	1
#define RHOST_RIEEE	2
#define N_RHOST		3

struct table {
   char *name;
   int value;
};

static struct table type_table[] = {
   { "BYTE", TYPE_BYTE },
   { "HALF", TYPE_HALF },
   { "FULL", TYPE_FULL },
   { "REAL", TYPE_REAL },
   { "DOUB", TYPE_DOUB },
   { "COMP", TYPE_COMP },
   { "WORD", TYPE_HALF },		/* obsolete */
   { "LONG", TYPE_FULL },		/* obsolete */
   { "COMPLEX", TYPE_COMP }		/* obsolete */
};

static struct table ihost_table[] = {
   { "LOW",  IHOST_LOW  },
   { "HIGH", IHOST_HIGH }
};

static struct table rhost_table[] = {
   { "VAX",   RHOST_VAX   },
   { "IEEE",  RHOST_IEEE  },
   { "RIEEE", RHOST_RIEEE }
};

static trans_fn type_rr[N_TYPES][N_TYPES] = {
{0,           v2_byte2half,v2_byte2full,v2_byte2real,v2_byte2doub,v2_byte2comp},
{v2_half2byte,0,           v2_half2full,v2_half2real,v2_half2doub,v2_half2comp},
{v2_full2byte,v2_full2half,0,           v2_full2real,v2_full2doub,v2_full2comp},
{v2_real2byte,v2_real2half,v2_real2full,0,           v2_real2doub,v2_real2comp},
{v2_doub2byte,v2_doub2half,v2_doub2full,v2_doub2real,0,           v2_doub2comp},
{v2_comp2byte,v2_comp2half,v2_comp2full,v2_comp2real,v2_comp2doub,0           }
};

   /*  low2low,    low2high  */
   /*  high2low,   high2high */

static trans_fn byte_rr[N_IHOST][N_IHOST] = {
   { 0,             0              },
   { 0,             0              }
};

static trans_fn half_rr[N_IHOST][N_IHOST] = {
   { 0,             v2_trans_swap2 },
   { v2_trans_swap2,0              }
};

static trans_fn full_rr[N_IHOST][N_IHOST] = {
   { 0,             v2_trans_swap4 },
   { v2_trans_swap4,0              }
};

   /*  vax2vax     vax2ieee     vax2rieee    */
   /*  ieee2vax    ieee2ieee    ieee2rieee   */
   /*  rieee2vax   rieee2ieee   rieee2rieee  */

static trans_fn real_rr[N_RHOST][N_RHOST] = {
   { 0,             v2_r_vax2ieee,  v2_r_vax2rieee  },
   { v2_r_ieee2vax, 0,              v2_trans_swap4  },
   { v2_r_rieee2vax,v2_trans_swap4, 0               }
};

static trans_fn doub_rr[N_RHOST][N_RHOST] = {
   { 0,             v2_d_vax2ieee,  v2_d_vax2rieee  },
   { v2_d_ieee2vax, 0,              v2_trans_swap8  },
   { v2_d_rieee2vax,v2_trans_swap8, 0               }
};

static trans_fn comp_rr[N_RHOST][N_RHOST] = {
   { 0,             v2_c_vax2ieee,  v2_c_vax2rieee  },
   { v2_c_ieee2vax, 0,              v2_c_ieee2rieee },
   { v2_c_rieee2vax,v2_c_rieee2ieee,0               }
};

static int v2_bytes_per_pixel2(int type, int ihost, int rhost);
static int search_table(char *string, struct table table[], int n);

/************************************************************************/

int v2_determine_translation(int unit)
{
   struct bufstate *bufstate;
   int status;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   /* Read */
   status=v2_translate_input(CURRENT_S_VALUE(FORMAT), CURRENT_S_VALUE(U_FORMAT),
			 CURRENT_S_VALUE(INTFMT), CURRENT_S_VALUE(REALFMT),
				&bufstate->read_trans);
   if (status != SUCCESS)
      return status;

   /* Write */
   status=v2_translate_output(CURRENT_S_VALUE(U_FORMAT),CURRENT_S_VALUE(FORMAT),
			  CURRENT_S_VALUE(INTFMT), CURRENT_S_VALUE(REALFMT),
			  &bufstate->write_trans);
   if (status != SUCCESS)
      return status;

   bufstate->read_trans.notransamt = CURRENT_I_VALUE(NBB);
   bufstate->write_trans.notransamt = CURRENT_I_VALUE(NBB);

   if (EQUAL(CURRENT_S_VALUE(CONVERT), "OFF")) { /* don't do any translations */
      bufstate->read_trans.transfn = NULL;
      bufstate->write_trans.transfn = NULL;
      return SUCCESS;
   }

   return SUCCESS;
}       


/************************************************************************/
/* Set up translation structure for input.  The source is (possibly) in	*/
/* a foreign host's representation, while the destination must be	*/
/* native.  Since data type conversions must be done native, we do the	*/
/* host conversion first, then the data type.				*/
/************************************************************************/

int v2_translate_input(
   char *stype,			/* In: Source data type (foreign host) */
   char *dtype,			/* In: Destination data type (native) */
   char *sihost,		/* In: Host representation for ints */
   char *srhost,		/* In: Host representation for reals */
   struct trans *trans		/* Out: translation structure */
)

{
   int type_src, type_dest, ihost_src, ihost_dest, rhost_src, rhost_dest;
   char temp[30];

   v2_make_upper_case_max(temp, stype, 29);
   type_src = search_table(temp, type_table,
			 sizeof(type_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, dtype, 29);
   type_dest = search_table(temp, type_table,
			 sizeof(type_table)/sizeof(struct table));

   if (type_src == -1 || type_dest == -1)
      return IMPROPER_FORMAT_STRING;

   v2_make_upper_case_max(temp, sihost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_INTFMT);
   ihost_src = search_table(temp, ihost_table,
			 sizeof(ihost_table)/sizeof(struct table));
   ihost_dest = search_table(NATIVE_INTFMT, ihost_table,
			 sizeof(ihost_table)/sizeof(struct table));

   if (ihost_src == -1 || ihost_dest == -1)
      return IMPROPER_INTFMT;

   v2_make_upper_case_max(temp, srhost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_REALFMT);
   rhost_src = search_table(temp, rhost_table,
			 sizeof(rhost_table)/sizeof(struct table));
   rhost_dest = search_table(NATIVE_REALFMT, rhost_table,
			 sizeof(rhost_table)/sizeof(struct table));

   if (rhost_src == -1 || rhost_dest == -1)
      return IMPROPER_REALFMT;

   /* Get pixel sizes */

   trans->spixsize = v2_bytes_per_pixel2(type_src, ihost_src, rhost_src);
   trans->dpixsize = v2_bytes_per_pixel2(type_dest, ihost_dest, rhost_dest);
   trans->notransamt = 0;

   /* Convert host first, then data type */

   switch (type_src) {		/* 1st translation: host conversion */
      case TYPE_BYTE: trans->transfn1=byte_rr[ihost_src][ihost_dest]; break;
      case TYPE_HALF: trans->transfn1=half_rr[ihost_src][ihost_dest]; break;
      case TYPE_FULL: trans->transfn1=full_rr[ihost_src][ihost_dest]; break;
      case TYPE_REAL: trans->transfn1=real_rr[rhost_src][rhost_dest]; break;
      case TYPE_DOUB: trans->transfn1=doub_rr[rhost_src][rhost_dest]; break;
      case TYPE_COMP: trans->transfn1=comp_rr[rhost_src][rhost_dest]; break;
      default: return INVALID_FORMAT_TRANSLATION;
   }

   trans->transfn2 = type_rr[type_src][type_dest];	/* 2nd: type */

   if (trans->transfn1 == NULL)		/* no 1st trans, so use 2nd only */
      trans->transfn = trans->transfn2;
   else if (trans->transfn2 == NULL)	/* no 2nd trans, so use 1st only */
      trans->transfn = trans->transfn1;
   else {				/* both there, so use special routine */
      trans->transfn = v2_dual_translation;
      trans->midpixsize = v2_bytes_per_pixel2(type_src, ihost_dest, rhost_dest);
   }

   /* The host translations can handle non-aligned data, but the type	*/
   /* translations cannot.  So, if we are translating type only, and	*/
   /* the source type is non-byte, then we need to check for alignment	*/
   /* when doing the translation.					*/
   /* For input, only the input buffer can be non-aligned.		*/

   if (trans->transfn1 == NULL && trans->transfn2 != NULL) {
      if (trans->spixsize != 1) {
         trans->transfn = v2_align_in_translation;
      }
   }

   if (trans->transfn1 == v2_bad_trans || trans->transfn2 == v2_bad_trans)
      return INVALID_FORMAT_TRANSLATION;

   return SUCCESS;
}

/************************************************************************/
/* Set up translation structure for output.  The destination is		*/
/* (possibly) in a foreign host's representation, while the source must	*/
/* be native.  Since data type conversions must be done native, we do	*/
/* the data type conversion first, then the host.			*/
/************************************************************************/

int v2_translate_output(
   char *stype,			/* In: Source data type (native) */
   char *dtype,			/* In: Destination data type (foreign host) */
   char *dihost,		/* In: Host representation for ints */
   char *drhost,		/* In: Host representation for reals */
   struct trans *trans		/* Out: translation structure */
)

{
   int type_src, type_dest, ihost_src, ihost_dest, rhost_src, rhost_dest;
   char temp[30];

   v2_make_upper_case_max(temp, stype, 29);
   type_src = search_table(temp, type_table,
			 sizeof(type_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, dtype, 29);
   type_dest = search_table(temp, type_table,
			 sizeof(type_table)/sizeof(struct table));

   if (type_src == -1 || type_dest == -1)
      return IMPROPER_FORMAT_STRING;

   ihost_src = search_table(NATIVE_INTFMT, ihost_table,
			 sizeof(ihost_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, dihost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_INTFMT);
   ihost_dest = search_table(temp, ihost_table,
			 sizeof(ihost_table)/sizeof(struct table));

   if (ihost_src == -1 || ihost_dest == -1)
      return IMPROPER_INTFMT;

   rhost_src = search_table(NATIVE_REALFMT, rhost_table,
			 sizeof(rhost_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, drhost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_REALFMT);
   rhost_dest = search_table(temp, rhost_table,
			 sizeof(rhost_table)/sizeof(struct table));

   if (rhost_src == -1 || rhost_dest == -1)
      return IMPROPER_REALFMT;

   /* Get pixel sizes */

   trans->spixsize = v2_bytes_per_pixel2(type_src, ihost_src, rhost_src);
   trans->dpixsize = v2_bytes_per_pixel2(type_dest, ihost_dest, rhost_dest);
   trans->notransamt = 0;

   /* Convert data type first, then host */

   trans->transfn1 = type_rr[type_src][type_dest];		/* 1st: type */

   switch (type_dest) {		/* 2nd translation: host conversion */
      case TYPE_BYTE: trans->transfn2=byte_rr[ihost_src][ihost_dest]; break;
      case TYPE_HALF: trans->transfn2=half_rr[ihost_src][ihost_dest]; break;
      case TYPE_FULL: trans->transfn2=full_rr[ihost_src][ihost_dest]; break;
      case TYPE_REAL: trans->transfn2=real_rr[rhost_src][rhost_dest]; break;
      case TYPE_DOUB: trans->transfn2=doub_rr[rhost_src][rhost_dest]; break;
      case TYPE_COMP: trans->transfn2=comp_rr[rhost_src][rhost_dest]; break;
      default: return INVALID_FORMAT_TRANSLATION;
   }

   if (trans->transfn1 == NULL)		/* no 1st trans, so use 2nd only */
      trans->transfn = trans->transfn2;
   else if (trans->transfn2 == NULL)	/* no 2nd trans, so use 1st only */
      trans->transfn = trans->transfn1;
   else {				/* both there, so use special routine */
      trans->transfn = v2_dual_translation;
      trans->midpixsize = v2_bytes_per_pixel2(type_dest, ihost_src, rhost_src);
   }

   /* The host translations can handle non-aligned data, but the type	*/
   /* translations cannot.  So, if we are translating type only, and	*/
   /* the source type is non-byte, then we need to check for alignment	*/
   /* when doing the translation.					*/
   /* For output, only the output buffer can be non-aligned.		*/

   if (trans->transfn1 != NULL && trans->transfn2 == NULL) {
      if (trans->dpixsize != 1) {
         trans->transfn = v2_align_out_translation;
      }
   }

   if (trans->transfn1 == v2_bad_trans || trans->transfn2 == v2_bad_trans)
      return INVALID_FORMAT_TRANSLATION;

   return SUCCESS;
}       

/************************************************************************/
/* Return the number of bytes in a pixel for the given data type/host	*/
/* combination.  Ihost comes from the INTFMT label, RHOST from the	*/
/* REALFMT label, and TYPE from FORMAT.					*/
/************************************************************************/

int v2_bytes_per_pixel(char *ctype, char *cihost, char *crhost, int *status)
{
   int type, ihost, rhost;
   char temp[30];

   v2_make_upper_case_max(temp, ctype, 29);
   type =search_table(temp,type_table, sizeof(type_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, cihost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_INTFMT);
   ihost=search_table(temp,ihost_table,
			sizeof(ihost_table)/sizeof(struct table));
   v2_make_upper_case_max(temp, crhost, 29);
   if (EQUAL(temp, "NATIVE") || EQUAL(temp, "LOCAL"))
      strcpy(temp, NATIVE_REALFMT);
   rhost=search_table(temp,rhost_table,
			sizeof(rhost_table)/sizeof(struct table));

   *status = SUCCESS;

   if (type == -1)  *status = IMPROPER_FORMAT_STRING;
   if (ihost == -1) *status = IMPROPER_INTFMT;
   if (rhost == -1) *status = IMPROPER_REALFMT;

   if (*status != SUCCESS)
      return 0;				/* Bad inputs */

   return v2_bytes_per_pixel2(type, ihost, rhost);
}

/************************************************************************/
/* bytes_per_pixel but with indexes instead of strings for args.  For	*/
/* use only internal to this module.					*/
/************************************************************************/

static int v2_bytes_per_pixel2(int type, int ihost, int rhost)
{
   static int byte_size[N_IHOST] = { 1, 1 };
   static int half_size[N_IHOST] = { 2, 2 };
   static int full_size[N_IHOST] = { 4, 4 };
   static int real_size[N_RHOST] = { 4, 4, 4 };
   static int doub_size[N_RHOST] = { 8, 8, 8 };
   static int comp_size[N_RHOST] = { 8, 8, 8 };

   if (ihost == -1)			/* shouldn't happen! */
      ihost = 0;
   if (rhost == -1)
      rhost = 0;

   switch (type) {
      case TYPE_BYTE: return byte_size[ihost];
      case TYPE_HALF: return half_size[ihost];
      case TYPE_FULL: return full_size[ihost];
      case TYPE_REAL: return real_size[rhost];
      case TYPE_DOUB: return doub_size[rhost];
      case TYPE_COMP: return comp_size[rhost];
      default: return byte_size[ihost];		/* shouldn't happen! */
   }
}

/************************************************************************/
/* Search the table for the given string, and return the value from the	*/
/* table.  This routine should only be visible within this module.	*/
/************************************************************************/

static int search_table(char *string, struct table table[], int n)
{
   int i;

   for (i=0; i<n; i++) {
      if (EQUAL(string, table[i].name))
         return table[i].value;
   }

   return -1;

}

