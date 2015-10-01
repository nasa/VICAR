#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"		/* TAE configuration definitions	*/
#include "parblk.inc"		/* par block structure definitions	*/
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAE

#define ERROR(x) {v2_error_handler(unit,x); return x;}

struct OLD_PARM_LINE {
   char p_name[8+1];		/* parm name				*/
   char p_type;			/* V_INTEGER, V_REAL, V_STRING, V_REAL8	*/
   short p_count;		/* TCL parm count			*/
   char p_size;			/* max size (if string)			*/
   int val_len;			/* Total length of value		*/
};

static struct trans short_conv,int_conv,real_conv,doub_conv; /*zvtrans*/
static int setup_conversions(int unit);
static void convert_old_parm_header(struct OLD_PARM_LINE *old_header, 
		    struct PARM_LINE *new_header, char *parm_name);
static void convert_parm_header(struct PARM_LINE *header);


/************************************************************************/
/* Initialize data set parameters on parameter block.			*/
/*									*/
/* This subroutine is used to take parameters stored in a parameter	*/
/* data set and store them in the par block so that they are accessible	*/
/* through xvparm.							*/
/* 									*/
/* It should never be called by an applications program.		*/
/*									*/
/* The parameter file may have come from a different host, in which	*/
/* case we need to translate both the data and the headers using the	*/
/* zvtrans routines.							*/
/*									*/
/* There are three types of parameter files:				*/
/*    PARAM		oldest style, fixed-length records		*/
/*    PARM		new variable-length records but old header	*/
/*    PARMS		new variable-length records and new header	*/
/* The records were changed from fixed to variable length to accomodate	*/
/* differing lengths of parameters.  A tiepoint with thousands of values*/
/* would force the single-valued parameters that went with it to be	*/
/* thousands of bytes too long, wasting space.				*/
/* The header was changed to accomodate an increase in the length of	*/
/* parameter names in TAE.  The old header format allowed only 8	*/
/* character names, while the new format is open-ended.			*/
/************************************************************************/

int zvpinit(
   void *parb		/* Pointer to start of the par block	*/
)

{
   int i, status;
   struct VARIABLE *current_parm;	/* Pointer to current parameter	*/
   int line;				/* line increment in file I/O	*/
   int nl;				/* Number of lines in image	*/
   int ns;				/* Number of bytes per line	*/
   int count;				/* TCL count of parms		*/
   int unit;				/* File unit number		*/
   int oldtype;				/* TRUE if old fixed len recs (PARAM) */
   int oldhead;				/* TRUE if old header (PARM or PARAM) */
   int eof;				/* TRUE if at end of file	*/
   char filename[MAX_FILE_NAME_SIZE+1];	/* Name of param file		*/
   char stype[10];			/* File type as string		*/
   char *data_ptr;			/* pointer to value data	*/
   char **str_ptr;			/* pointer to data_ptr for str.	*/
   char *buff = NULL;			/* ptr to line buf (oldtype)	*/
   char *buf = NULL;			/* ptr to data (inside buff for	old) */
   char tae_type;			/* p_type made to agree with v_type */
   char msgbuf[80];			/* Message buffer		*/
   struct OLD_PARM_LINE old_header;
   struct PARM_LINE header;
   char parm_name[STRINGSIZ+1];

/* Check for the existence of a parameter data set */
   zvp("PARMS",filename,&count);

   if (count == 0) return SUCCESS;		/* return if no pds	*/

   zvunit(&unit,"XX",1,"U_NAME",filename, NULL);	/* Open it	*/
   zvopen(unit,"OPEN_ACT","SA","IO_ACT","SA","LAB_ACT","SA", NULL);

   zlget(unit,"system","type",stype, NULL);
   if (EQUAL(stype, "PARMS")) {	/* new format, new header */
      oldtype = FALSE;
      oldhead = FALSE;
   }
   else if (EQUAL(stype, "PARM")) {	/* new format, old header */
      oldtype = FALSE;
      oldhead = TRUE;
   }
   else if (EQUAL(stype, "PARAM")) {	/* old, fixed-length records format */
      oldtype = TRUE;
      oldhead = TRUE;
   }
   else
      ERROR(NOT_PARAMETER_FILE);

   status = setup_conversions(unit);
   if (status != SUCCESS)
      ERROR(status);

/* Parm files have a header, followed by header.val_len bytes of data,	*/
/* followed immediately by another header.  The buffer allocated to	*/
/* read the data becomes the actual buffer used by the par block,	*/
/* except for real*4 data, which gets copied to a real*8 buffer.	*/
/* The old format (if oldtype == TRUE), had one header and data area	*/
/* per record.  The difference in processing is that a record-sized	*/
/* buffer is allocated to read the data, then it is always copied to a	*/
/* new buffer of the correct size.					*/
/* The new-style header has the header, followed by header.name_len	*/
/* bytes of name, and header.val_len bytes of data, as above.		*/

   if (oldtype) {
      zvget(unit,"NL",&nl,"NS",&ns, NULL);	/* Get number of parms	*/
						/* and record length	*/
      buff = (char *)malloc(ns);
      if (buff == NULL) ERROR(INSUFFICIENT_MEMORY);
      buf = buff + sizeof(header);
   }
   else {
      status = v2_parm_init(unit);
      if (status != SUCCESS)
	 return status;
   }

/* Loop through all the parameters in the file				*/

   line = 0;
   eof = FALSE;

   while (!eof) {
      if (oldtype) {			/* get line from file */
	 if (line++ >= nl) {
	    eof = TRUE;
	    continue;
	 }
	 zvread(unit, buff, NULL);
	 convert_old_parm_header((struct OLD_PARM_LINE *)buff, &header, 
				 parm_name);
	 data_ptr = buf;
      }
      else {
	 if (oldhead) {
	   v2_parm_read((char*) &old_header, 
		     sizeof(old_header));  /* read header */
	    if ((char)old_header.p_type == (char)PARM_V_EOF) {
	       eof = TRUE;
	       continue;
	    }
	    convert_old_parm_header(&old_header, &header, parm_name);
	 }
	 else {					/* new header */
	   v2_parm_read((char*) &header, 
		     sizeof(header));	/* read header */
	    if ((char)header.p_type == (char)PARM_V_EOF) {
	       eof = TRUE;
	       continue;
	    }
	    convert_parm_header(&header);
	    if (header.version != 0)		/* whoops, new version! */
	       ERROR(PARM_FILE_VERSION);
	    v2_parm_read(parm_name, header.name_len);
	 }
	 buf = (char *)malloc(header.val_len);
	 if (buf == 0) ERROR(INSUFFICIENT_MEMORY);
	 data_ptr = buf;
	 v2_parm_read(buf, header.val_len);
      }

/* Find the currently named parameter in the par block			*/

      current_parm = p_fvar((struct PARBLK*) parb, parm_name);

      if (current_parm == 0) {	/* Current parameter not in par block */
	 sprintf(msgbuf,"XVPINIT -- Parameter %s not found", parm_name);
	 zvmessage(msgbuf,"VIC2-GENERR");
	 zabend();
      }

/* Make sure parameter was defaulted (allow explicit interactive parm	*/
/* to override parm file entry)						*/

      if (current_parm->vu_class.vs_parm.V_default == FALSE) continue;

/* Make sure type agrees */

      tae_type = (header.p_type==PARM_V_REAL8) ? PARM_V_REAL : header.p_type;
      if (current_parm->v_type != tae_type) {
	 sprintf(msgbuf,"XVPINIT -- Data type of %s does not agree with PDF",
		 current_parm->v_name);
	 zvmessage(msgbuf,"VIC2-GENERR");
	 zabend();
      }

/* Fill in VARIABLE structure at current_parm */
      current_parm->vu_class.vs_parm.V_default = FALSE; /* clear def flag */
      current_parm->vu_value.vs_direct.V_count = header.p_count;/* TCL count */
      current_parm->vu_value.vs_direct.V_size = header.p_size;  /* max string size */

      if (header.p_type == PARM_V_REAL) {  /* Transfer real*4 to real*8 buf */
	 data_ptr = (char *)malloc((header.val_len)*2);	  /* Allocate a place */
	 if (data_ptr == 0) ERROR(INSUFFICIENT_MEMORY); /* to store data   */
         zvtrans((int *)&real_conv, buf, data_ptr, 
		 header.p_count);	/* real->doub */
	 if (!oldtype)
	    free(buf);
      }
      else {	     /* store directly as is (with host conversion if needed) */
	 if (oldtype) {		/* old style needs buffer copied */
	    data_ptr = (char *)malloc(header.val_len);		/* Allocate a */
	    if (data_ptr == 0) ERROR(INSUFFICIENT_MEMORY);	/* place to   */
								/* store data */
	    if (header.p_type == PARM_V_INTEGER)
	       zvtrans((int *)&int_conv, buf, data_ptr, header.p_count);
	    else if (header.p_type == PARM_V_REAL)
	       zvtrans((int *)&doub_conv, buf, data_ptr, 
		       header.p_count);
	    else				/* PARM_V_STRING */
	       memcpy(data_ptr,buf, header.val_len);
	 }
	 else {			/* new style may need types converted */
	    if (header.p_type == PARM_V_INTEGER && int_conv.transfn != 0) {
	       data_ptr = (char *)malloc(header.val_len);
	       if (data_ptr == 0) ERROR(INSUFFICIENT_MEMORY);
	       zvtrans((int *)&int_conv, buf, 
		       data_ptr, header.p_count);
	       free(buf);
	    }
	    else if (header.p_type == PARM_V_REAL && doub_conv.transfn != 0) {
	       data_ptr = (char *)malloc(header.val_len);
	       if (data_ptr == 0) ERROR(INSUFFICIENT_MEMORY);
	       zvtrans((int *)&doub_conv, buf, data_ptr, 
		       header.p_count);
	       free(buf);
	    }
		/* Strings don't need to be converted */
	 }
      }

/* Store pointer to data*/

      if (current_parm->v_type == PARM_V_STRING) {
	 /* For strings, store array of pointers to data */
	 i = sizeof(char *) * header.p_count;
	 str_ptr = (char **)malloc (i);  /* array of ptrs to actual data */
	 if (str_ptr == 0) ERROR(INSUFFICIENT_MEMORY);
	 current_parm->vu_value.vs_direct.V_cvp = (GENPTR)str_ptr;

	 for (i = 0; i < header.p_count; i++) { /* Fill in array of ptrs */
	    *str_ptr = data_ptr;
	    str_ptr++;
	    data_ptr += strlen(data_ptr) + 1;
	 }
      }
      else {
	 current_parm->vu_value.vs_direct.V_cvp = data_ptr;
      }
   }
   if (oldtype)
      free(buff); 
   else
      v2_parm_close();

   zvclose(unit,NULL);		/* close the file		*/
   return SUCCESS;
}

/************************************************************************/
/* Set up the data type conversions for header and data			*/
/************************************************************************/

static int setup_conversions(int unit)
{
   int status;

   status = zvtrans_inu((int *)&short_conv, "HALF", "HALF", unit);
   if (status == SUCCESS)
      status = zvtrans_inu((int *)&int_conv, "FULL", "FULL", unit);
   if (status == SUCCESS)
      status = zvtrans_inu((int *)&real_conv, "REAL", "DOUB", unit);
   if (status == SUCCESS)
      status = zvtrans_inu((int *)&doub_conv, "DOUB", "DOUB", unit);

   return status;
}

/************************************************************************/
/* Convert the non-chars in the parameter header			*/
/************************************************************************/

static void convert_parm_header(struct PARM_LINE *header)
{
   short stemp;
   int itemp;

   zvtrans((int *)&int_conv, &header->version, &itemp, 1);
   header->version = itemp;
   zvtrans((int *)&int_conv, &header->val_len, &itemp, 1);
   header->val_len = itemp;

   zvtrans((int *)&short_conv, &header->p_count, &stemp, 1);
   header->p_count = stemp;
   zvtrans((int *)&short_conv, &header->p_size, &stemp, 1);
   header->p_size = stemp;
}

/************************************************************************/
/* Convert the non-chars in the parameter header and copy from the old	*/
/* format structure to the new format structure.			*/
/************************************************************************/

static void convert_old_parm_header(struct OLD_PARM_LINE *old_header, 
			struct PARM_LINE *new_header, char *parm_name)
{
   new_header->version = 0;
   zvtrans((int *)&int_conv, &old_header->val_len, 
	   &new_header->val_len, 1);
   zvtrans((int *)&short_conv, &old_header->p_count, 
	   &new_header->p_count, 1);
   new_header->p_size = old_header->p_size;	/* byte -> short */
   new_header->p_type = old_header->p_type;
   new_header->name_len = strlen(old_header->p_name)+1;
   strcpy(parm_name, old_header->p_name);
}

#else

int zvpinit(void *UNUSED(parb))
{
   current_call = VPOPEN;	/* not really but this should never happen */
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

