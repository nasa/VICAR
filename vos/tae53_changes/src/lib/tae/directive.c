/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* TDM CHECKOUT FILE_TIME=25-APR-1985 20:53 DUA1:[TAEV1.RCJM]DIRECTIVE.C;6 */
/* TDM CHECKOUT FILE_TIME=29-NOV-1984 17:05 DUA1:[TAEV1.OLB]DIRECTIVE.C;1 */
/* TLA CHECKOUT FILE_TIME=17-MAY-1984 11:23 DUA0:[TAEV1.OLB]DIRECTIVE.C;82 */
/* TDM CHECKOUT FILE_TIME=14-FEB-1984 18:27 DUA0:[TAEV1.OLB]DIRCTV.C;79 */
/* TDM CHECKOUT FILE_TIME=31-OCT-1983 17:10 DUA0:[TAEV1.OLB]DIRCTV.C;78 */
/* TDM CHECKOUT FILE_TIME=11-OCT-1983 17:50 DUA0:[TAEV1.OLB]DIRCTV.C;76 */
/*
 * SOURCE FILE FOR THE DIRECTIVE PACKAGE.
 *
 * This package contains the subroutines to read and extract the text 
 * information associated with a directive in MDF/PDF/HLP files.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	11-oct-83	Unix compilations errors...palm
 *	20-oct-83	Fix "end-of-file" bug in reading text records...dm
 *	26-oct-83	Implement .if/.elseif and .if1 conditionals...dm
 *	31-oct-83	Fix *db declaration...palm
 *	18-jan-84	Fix bug in incopen (for UNIX crash)...dm
 *	03-feb-84	Implement .command processing, add  d_cdirctv...dm
 *	12-feb-83	Change function name incclose to d_incclose...dm
 *	06-mar-84	Call flush_cmd if command string overflows...dm
 *	22-mar-84	Change function name incopen to d_incopen...dm
 *	17-may-84	New f_crack calling sequence...palm
 *	15-nov-84	Change parsedir to parserec for more general use
 *			in parsing sections of the directive line...lia
 ***********************************************************************
 * CHANGES MADE IN THE RCJM TREE (RCJM SPECIFIC):
 *
 *	22-apr-85	Implement access of remote .include files...dm
 *	27-apr-85	Update get_remote_file calling seq...dm
 *************************************************************************
 *	05-nov-86	Eliminate #ifdef TAE_RCJM's...nhe
 *	07-nov-86	Put get_reminc in another source...nhe
 *	15-jul-87	Variable substitution.   TBD: avoid overhead if not 
 *			requested? ...palm
 *	10-aug-87	Remove direct call to get_reminc--a pointer to
 *			get_reminc is now passed in the d_remote call.
 *			This keeps this package cleaner--we don't want
 *			direct references from here back to TM....palm
 *	17-jun-88	remove ascii dependency...tp
 *			add include file 'chartype.inc' 
 *      14-feb-92	add .reference to "include" text from another directive
 *			can only reference one level...cew
 */

#include	"taeconf.inp"
#include	"tmhost.inp"
#include	"tminc.inc"
#include	"dirinc.inc"
#include	"chartype.inc"
#include "taeintproto.h"


    struct DTABLE
	{
	TEXT	name[12];
	CODE	type;
	};		

/*  record types			*/

#define		DCOMMENT   1		/* comment record	    	*/
#define		DTEXT 	   2		/* text record 	 		*/
#define		DIRECTIVE  3		/* directive record		*/  
#define		DPAGE	   4		/* .PAGE record			*/
#define		DINCLUDE   5		/* .INCLUDE record		*/
#define		DCONDITION 6		/* .IF/.ELSEIF/.IFEND records 	*/
#define		DREFERENCE 7		/* .REFERENCE record            */

#define		SWITCH	   FAIL		/* switch .include and main files  */


/*  subtypes of conditional records	*/

#define		DIF1       61
#define		DIFN1      62
#define		DIF	   63
#define		DELSE	   64
#define		DIFN	   65
#define		DIFEND	   66


FUNCTION   static  CODE  rectype
(
 TEXT	*recptr		/* IN: pointer to text record    */
 );
FUNCTION static  VOID  striplw
(  
 TEXT	*ptr			/* IN/OUT: text record */
   );
FUNCTION  static  CODE  getline_tae
(
 struct  DIRBLK	*db,		/* IN: context of file to be read    */
 FUNINT		incread,	/* IN: true if read from include file*/
 TEXT		record[]	/* OUT: text information 	     */
 );

/*	The reason for the following is explained under 'd_setsub' below.     */

typedef CODE (*FUNCTION_PTR_INT) (struct  DIRBLK *db, TEXT remspec[], TEXT filespec[]);
    static FUNCTION_PTR d_sub_function = (FUNCTION_PTR) NULL;
    static FUNCTION_PTR_INT d_get_reminc = (FUNCTION_PTR_INT) NULL;
    static GENPTR	d_context = NULL;	/* context for the function   */

/*  Macro to do substitution if an entry point has been defined */

#define SUBSTITUTE(record, size)  \
    if (d_sub_function != NULL)	\
    	(*d_sub_function) (record, size, d_context, FALSE);


FUNCTION  VOID  s_stptrl
(
 TEXT	*ptr			/* IN/OUT: text record */
);
FUNCTION static CODE full_field 
(
 struct	SFILE  	*sptr,			/* IN: sfile of main file    */
 TEXT cmdstr[]		/* IN/OUT: full dirctv field */
);
FUNCTION  static  CODE  inctext
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record */
 );
FUNCTION  static  CODE  maintext
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record 		*/
 );
FUNCTION  static  CODE  pos_next
(
 struct  DIRBLK   *db,		/* in: directive block		   */
 struct  SFILE    *sptr,		/* in/out: ptr to sfile to read	   */
 TEXT	record[]		/* in: record with conditinal stmt */
 );
FUNCTION  static  VOID  get_ctype
(    
 TEXT	record[],		/* in: record with conditional stmt */
 CODE	*subtype,			/* out: record subtype		*/
 TEXT	arg[]			/* out: argument of condition	*/
);


/*	d_setsub.  Set substitution function and context. 
 *
 *	This exists so that this package--a library package--does
 *	not have direct references to TM modules.  TM calls
 *	this entry to set the "substitute" function and
 *	the "context" to be used for substitution.
 *
 *	The return value is the old substitute function pointer.
 */

FUNCTION  FUNCTION_PTR d_setsub 
(
 FUNCTION_PTR new_function	/* in: pointer to substitute function */
)
    {
    FUNCTION_PTR 	old_function;

    old_function = d_sub_function;
    d_sub_function = new_function;
    return (old_function);
    }	
    	

/*	d_setctx.   Set context to be passed to substitute function.
 *
 *	The return value is the old context pointer.
 *	
 */

FUNCTION GENPTR d_setctx 
(
 GENPTR new_context	/* in: new context for substitute */
)
    {
    GENPTR 	old_context;    

    old_context = d_context;
    d_context = new_context;
    return (old_context);
    }    

/*
 * 	d_init.  Initialise directive package.
 *      This routine must be called before any other call to the directive 
 *	package.
 */

FUNCTION  VOID  d_init
(
 struct  DIRBLK	*db,		/* OUT: pointer to directive block */
 struct  SFILE	*sf,		/* IN:	pointer to file's SFILE    */
 TEXT libr[],		/* IN:  file library name	   */
 TEXT **sys_char,	/* IN:  ptr to system characteristics*/
 FUNINT char_cnt	/* IN:  count of characteristic strings	   */
)
    {
    (*db).sfileptr = sf;		/* save pointer to sfile 	*/
    s_copy(libr, (*db).libr);		/* save library name		*/
    (*db).incfile.posctx.possav = FALSE; /* no include file  active	*/
    (*db).incpos.possav = FALSE;	 /* no include file open	*/
    (*db).sys_char = sys_char;		 /* save sys_char address	*/
    (*db).char_cnt = char_cnt;		 /* save count			*/
    (*db).inblock = FALSE;		 /* not within if/else block	*/
    (*db).c_valid = FALSE;		 /* condition valid flase	*/
    (*db).remctx = NULL;		 /* default is local help	*/
    return;
    }


/*
 * 	d_dirctv.  Read next directive record and return directive type.
 *	Returned codes are : 	SUCCESS,
 *				D_ERROR (file access error)
 */

FUNCTION  CODE  d_dirctv
(
 struct  DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT dirctv[],	/* OUT: directive type (including '.') */ 
 TEXT field[]	/* OUT: associated text */
 )
    {
    TEXT		record[STRINGSIZ+1];	/* local buffer to read a record */
    CODE 		code;			/* status: SUCCESS/D_ERROR */

    do
	{
	code = getline_tae(db, FALSE, record);		/* read next record */ 
   	if (code == F_FILERR) return(D_ERROR);		/* file read error  */
	}
    while (rectype(record) != DIRECTIVE);	   /* until a directive      */
    parserec(record, dirctv, field);		   /* parse directive record */
    return(code);
    }

/*
 * 	d_cdirctv.  Read next directive record and return directive type, 
 *		    allowing for continuation of the directive field
 *		    upto CMDLINSIZ.
 *	Returned codes are : 	SUCCESS,
 *				D_ERROR (file access error or field too long)
 */

FUNCTION  CODE  d_cdirctv
(
 struct  DIRBLK	*db,		/* IN: pointer to dirctive block     */
 TEXT dirctv[],	/* OUT: directive type (including .) */
 TEXT field[]	/* OUT: associated text 	     */
)
    {
    CODE 		code;			/* status: SUCCESS/D_ERROR   */

    code = d_dirctv(db, dirctv, field);		     	/* get next directive*/
    if (code == SUCCESS)
	code = full_field((*db).sfileptr, field);	/* get full command  */
    return(code);
    }

/*
 *	full_field. Return the full directive field (upto CMDLINESIZ)
 *	allowing for continuation of the directive record.
 *	Note that the continued lines must obey the same conventions as 
 *	the regular TCL commands.
 *	Return codes:
 *			SUCCESS
 *			D_ERROR
 */

FUNCTION static CODE full_field 
(
 struct	SFILE  	*sptr,			/* IN: sfile of main file    */
 TEXT cmdstr[]		/* IN/OUT: full dirctv field */
)
    {
    TEXT		inbuf[STRINGSIZ+1];
    CODE		code;
    COUNT		nrec;			/* record counter, ignored */
    BOOL		inquote;		/* TRUE if continued quote */

    s_copy(cmdstr, inbuf);			/* first iteration	   */
    cmdstr[0] = EOS;
    inquote = FALSE;				/* not inside quote	   */

    while ((code = app_cmd(inbuf, cmdstr, inquote)) == CONTINUE)
						 /* append input to cmdstr */
  	{
	code = f_read(sptr, inbuf);		/* read next line	   */
        SUBSTITUTE(inbuf, STRINGSIZ);
  	if (code != SUCCESS) 
	    return(D_ERROR);			/* error even on EOF 	   */
	}
    if (code == FAIL)
	{
	cmdstr[0] = EOS;			/* ignore the command	   */
	flush_cmd(sptr, inbuf, &nrec);		/* flush the rest of it    */
	}
    return (code == SUCCESS ? SUCCESS : D_ERROR);
    }

/* 
 * 	d_search. Search for the next directive record of given name.
 *	Returned codes are :	SUCCESS,
 *				D_EOF (end of file),
 *				D_ERROR (file access error, or field too long)
 */

FUNCTION  CODE  d_search
(
 struct DIRBLK  *db,	 	/* IN: directive block		     */
 TEXT dirctv[],		/* IN: directive name  to search for */
 TEXT field[]		/* OUT: associated text 	     */
 )
    {
    CODE	code;				/* SUCCESS/D_EOF/D_ERROR     */
    TEXT	nxtdir[STRINGSIZ+1];		/* directive retrieved       */
    
    do
	{
	code = d_dirctv(db, nxtdir, field);		/* get next dirctv rec*/
        if (code == D_ERROR) return(code);		/* file error        */
	else if (s_equal(nxtdir,  ".END"))  return(D_EOF);    /* end of file */
	}
    while (!s_equal(nxtdir, dirctv));			/* loop till match   */
    if (s_equal (dirctv, ".command"))			/* if a .command     */
	code = full_field((*db).sfileptr, field);	/* get full text     */
    return(code);					/* error if too long */
    }

/*
 * 	d_text.  Read the next line of text for a directive.
 *		 Note: The next record may come from the main file or an 
 *		 include file.
 *	Return codes are :	SUCCESS,
 *			 	D_EOT (end of text),
 *			 	D_PAGE (.PAGE directive encountered),
 *				D_ERRINC (error in opening include file)
 *		    	  and   D_ERROR (file access errors)
 *
 */

FUNCTION  CODE  d_text
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record */
)
    {
    CODE		code;		 	/* status: SUCCESS/D_EOT/  */
						/* D_PAGE/D_ERRINC/D_ERROR */
    unsigned		incread;		/* read from include file ? */

    code = SUCCESS;				/* initialise		*/
    incread = (*db).incfile.posctx.possav;	/* TRUE if file active	*/
    do						/* repeat until right file */
	{
	if (incread)
	    code = inctext(db, record);		/* get text from inc file */
	else
	    code = maintext(db, record);	/* get text from main file */

	incread = !incread;			/* reverse file to read	   */
	}
    while (code == SWITCH);
    return (code);
    }

/*
 * 	getline.  Get the next non-comment line. 
 *		 NOTES: - when a record is read from the include file, 
 *		 	  f_read() sets .possav to true .
 *			- DCONDITION type of records are transparent 
 *			  to routines above getline.
 */

FUNCTION  static  CODE  getline_tae
(
 struct  DIRBLK	*db,		/* IN: context of file to be read    */
 FUNINT		incread,	/* IN: true if read from include file*/
 TEXT		record[]	/* OUT: text information 	     */
 )   
    {
    CODE		code;
    struct	SFILE	*sptr;		/* pointer to sfile for read	    */
    CODE		rtype;		/* record type			    */

    code = SUCCESS;				/* initialise 		    */
    if (incread)				/* if to read from include file */
	sptr = &(*db).incfile;			/* point to its sfile	    */
    else					/* get line from main file  */
	sptr = (*db).sfileptr;			/* point to main file 	    */
    do
	{
	code = f_read(sptr, record);		/* read next line 	    */
        SUBSTITUTE(record, STRINGSIZ);
	if ((rtype = rectype(record)) == DCOMMENT)	/* ignore comments  */
	    continue;
	else if (rtype == DCONDITION)		/* a .if type record	    */
	    {
	    code = pos_next(db, sptr, record);	/* position for next read   */
  	    if (code == F_EOF && incread)	/* premature  EOF of include file */
	        code = D_ERROR;			/* flag as error	    */
	    }
 	else
	    break;				/* any other type is okay   */
	}
    while (code == SUCCESS);
    if (code == F_EOF && !incread)		/* if EOF of main file 	    */
	{
	s_copy(".END", record);			/* simulated .END directive */
	code = SUCCESS;
	}
    return(code);
    }

/*
 * 	d_incopen. Open the given include file .
 *		After the open, the main file is positioned at the record 
 *		following the .include record.
 *		NOTE: If the new include file is the same as the existing
 *		include file (which is already open) we only rewind it.
 */

FUNCTION  CODE  d_incopen
(
 struct  DIRBLK  	*db,		/* IN: directive block	*/
 struct  POSCTX	*curinc	/* IN: posctx of .include record */
)
    {
    CODE		code;
    TEXT		dirctv[STRINGSIZ+1];
    TEXT		filespec[STRINGSIZ+1];
    TEXT		dummyrec[STRINGSIZ+1];
    TEXT		lastspec[STRINGSIZ+1];
    struct  FSBLOCK	fsblock;
    TEXT		errstr[STRINGSIZ+1];
    TEXT		remspec[STRINGSIZ+1];

    dummyrec[0] = EOS;
    lastspec[0] = filespec[0] = EOS;
    if ((*db).incpos.possav)
	{
	f_setpos((*db).sfileptr, &(*db).incpos);	/* position to last .inc */
	f_read((*db).sfileptr, dummyrec);		/* read .include record */
    	SUBSTITUTE(dummyrec, STRINGSIZ);
	parserec(dummyrec, dirctv, lastspec);		/* last inc filespec */
	dummyrec[0] = EOS;
	}
    f_setpos((*db).sfileptr, curinc);			/* reposition    */
    f_read((*db).sfileptr, dummyrec);			/* read .include record */
    SUBSTITUTE(dummyrec, STRINGSIZ);
    parserec(dummyrec, dirctv, filespec);		/* extract file spec */
    if (s_equal(filespec, lastspec))			/* if same file	     */
	code = f_rewind(&(*db).incfile);		/* rewind file	     */
    else
	{
	d_incclose(db);					/* close old incfile */
	if ((*db).remctx != NULL)			/* if a remote help  */
	    {
	    s_copy(filespec, remspec);
	    filespec[0] = EOS;
	    if (d_get_reminc == NULL)			/* if no callback    */
		return (FAIL);
	    code = (*d_get_reminc) (db, remspec, filespec); 
  	    if (code != SUCCESS) 
		return (code);
	    }
	f_crack(filespec, (*db).libr, "", HLP_TYPE, &fsblock, errstr);	
	code = f_opnblk(&(*db).incfile, INCLUN, &fsblock, F_READ); /* open it */
	if (code == SUCCESS)
	    f_movpos(curinc, &(*db).incpos);		/* save new .incude  */
	}
    return(code);
    }

/*
 *	d_incclose. Close include file.
 */

FUNCTION  VOID  d_incclose
(
    struct   DIRBLK	*db  			/* directive block	*/
 )
    {
    if ((*db).incpos.possav)			/* if there was a file  */
	{
	f_close(&(*db).incfile, F_KEEP);	/* close it		*/
	(*db).incpos.possav = FALSE;		/* mark as closed	*/
	(*db).incfile.posctx.possav = FALSE;	/* also, inactive	*/
	}
    return;
    }

/*
 * 	inctext. Get a text line from the include file		
 *	Return codes are :	SUCCESS,
 *			 	D_PAGE (.PAGE directive encountered),
 *				D_ERRINC (error in opening include file)
 *		    	  	D_ERROR (file access errors)
 *				SWITCH (no more records)
 */

FUNCTION  static  CODE  inctext
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record */
 )
    {
    CODE		code;		 	/* status: SUCCESS/D_EOT/  */
						/* D_PAGE/D_ERRINC/D_ERROR */

    code = getline_tae(db, TRUE, record);		/* read next non-comment line*/
    if (code == F_EOF)				/* end of file 		*/
	{
	(*db).incfile.posctx.possav = FALSE;	/* file no more active  */
	return(SWITCH);
 	}
    else if (code != SUCCESS)
	return(D_ERROR);			/* file i/o error	*/

    if (rectype(record) == DTEXT)		/* text record read	*/
	return(SUCCESS);
    else if (rectype(record) == DPAGE)		/* .PAGE directive	*/
	return(D_PAGE);
    else 
	return(D_ERROR);			/* anything else error 	*/
    } 

/*
 *	maintext. Get a text record from the main file
 *	Return codes are :	SUCCESS,
 *				SWITCH (switch to include file)
 *				D_EOT (end of text)
 *			 	D_PAGE (.PAGE directive encountered),
 *				D_ERRINC (error in opening include file)
 *		    	  	D_ERROR (file access errors)
 *
 *	TBD: How to determine input file type to allow/reject .include records
 */
    
FUNCTION  static  CODE  maintext
(
 struct	DIRBLK	*db,		/* IN: pointer to directive block */
 TEXT		record[]	/* OUT: text record 		*/
 )
    {
    CODE		code;		 	/* status: SUCCESS/D_EOT/  */
						/* D_PAGE/D_ERRINC/D_ERROR */
    struct	POSCTX	curinc;			/* posctx of .include rec  */
    static struct POSCTX curref;		/* posctx of .reference rec  */
    static BOOL	inref = FALSE;			/* TRUE if in reference    */
    TEXT	directive[STRINGSIZ+1];
    TEXT	fullreffield[STRINGSIZ+1];
    TEXT	reffield[STRINGSIZ+1];
    TEXT	refdirective[STRINGSIZ+1];
    TEXT	field[STRINGSIZ+1];
    TEXT	dummyrec[STRINGSIZ+1];


    code = getline_tae(db, FALSE, record);		/* read next non-comment line*/
    if (code != SUCCESS) 
	return(D_ERROR);			/* file read error 	*/
    if (rectype(record) == DINCLUDE)		/* .include record	*/
	{
	f_movpos(&(*(*db).sfileptr).posctx, &curinc);
	code = d_incopen(db, &curinc);		/* open the include file */
	if (code == SUCCESS)
	    return(SWITCH);			/* switch to include file */
	else
	    return(D_ERRINC);			/* could not open include file */
	}	
    else if (rectype(record) == DREFERENCE &&
	     !inref)				/* allow one level of ref */
        {
	inref = TRUE;

	/* skip over the .reference */
	dummyrec[0] = EOS;
	f_read((*db).sfileptr, dummyrec);	/* read .reference record */

	/* remember the current position */
	f_movpos(&(*(*db).sfileptr).posctx, &curref);

	/* determine what is being referenced */
	parserec (record, directive, fullreffield);
	parserec (fullreffield, refdirective, reffield);

	/* scan for directive other than DREFERENCE that matches reffield */
	f_rewind((*db).sfileptr);
	while ( FOREVER )
	    {
	    code = d_dirctv (db, directive, field);
	    if (s_equal (".END", directive))
		/* couldn't find reference.  Is D_ERROR the right code ? */
		return (D_ERROR);
	    if (code != DREFERENCE && 
		s_equal (refdirective, directive) &&
		s_equal (reffield, field))
		break;
	    }
	return (maintext(db, record));
	}
    else if (rectype(record) == DPAGE)		/* a .page record 	*/
	return (D_PAGE);
    else if (rectype(record) == DIRECTIVE)	/* a directive		*/
	{
	if (inref)
	    {
	    /* if currently in a referenced block of text and a directive is */
	    /* found, jump back to the original position.		     */
	    inref = FALSE;
	    f_setpos((*db).sfileptr, &curref);
	    return (maintext(db, record));
	    }
	else if (!s_lseq(".END", record))	/* not end of file	*/
	    f_setpos((*db).sfileptr, &(*(*db).sfileptr).posctx);  /* backup */
	return(D_EOT);				/* end of text		*/
        }
    else
	return(SUCCESS);
    }

/*
 * 	match. Check if the argument matches any entry in table.
 */

FUNCTION  static  BOOL match
(
 struct  DIRBLK  *db,			/* in: dirctv block	*/
 TEXT	    name[]			/* in: name to match    */
)
    {
    COUNT	    i;	

    for (i=0; i < (*db).char_cnt; i++)
	{
	if (s_equal(name, (*db).sys_char[i]))
	    return (TRUE);
	}
    return (FALSE);				/* no match		*/
    }   

/*
 * 	parserec.  Parse the given record into two fields.
 *		  
 *	Note: A directive record must start with a "." in the first 
 * 	character position.
 */

FUNCTION  VOID  parserec
(
 TEXT  	*recptr,       /* IN: ptr to a DIRECTIVE record */
 TEXT	*nameptr,	/* OUT: ptr to name */
 TEXT	field[]	/* OUT: associated field */ 
 )
    {
    TEXT	*ptr;		
    
    for (ptr = recptr; !iswhite(*ptr) ;)
	*nameptr++ = *ptr++;	 	/* copy first word */
    *nameptr = EOS;			/* mark end of name */
    s_copy(ptr, field);			/* copy the rest to field */
    striplw(field);			/* strip leading white spaces */
    s_stptrl(field);			/* strip trailing comments etc.*/
    return;
    }

/*
 *	pos_next. Position for next read satisfying the if/else conditions.
 */

FUNCTION  static  CODE  pos_next
(
 struct  DIRBLK   *db,		/* in: directive block		   */
 struct  SFILE    *sptr,		/* in/out: ptr to sfile to read	   */
 TEXT	record[]		/* in: record with conditinal stmt */
 )
    {
    CODE	subtype;		/* record subtype		   */
    TEXT	arg[STRINGSIZ+1];	/* conditional argument    	   */
    TEXT	dummyrec[STRINGSIZ+1];	/* dummy record 		   */
    BOOL	arg_match;		/* true if argument in the list	   */
    CODE	code;

    get_ctype(record, &subtype, arg);	/* get subtype and argument	   */
    if (subtype != DIFEND && NULLSTR(arg))
	return (D_ERROR);		/* must have an argument	   */
    arg_match = match(db, arg);

    if ((*db).inblock)			/* if within an if/else block	   */
	{
	if (subtype == DIF1 || subtype == DIFN1 
			|| subtype == DIF || subtype == DIFN)
	    return (D_ERROR);		/* not allowed 			   */
 	}
    else
	{
	if (subtype == DELSE || subtype == DIFEND)
	    return (D_ERROR);		/* not allowed outside a block	   */
        }
/*		*******	first check one-liners	*******			   */

    if (subtype == DIF1)		/* a .if1 record		   */
	{
	code = arg_match ? SUCCESS : 	   /* if matches, okay		   */
		f_read(sptr, dummyrec);    /* else, discard next record    */
	return (code);
	}
    else if (subtype == DIFN1)		/* a .ifn1 record		   */
	{
	code = (!arg_match) ? SUCCESS :    /* if does not match, okay	   */
		f_read(sptr, dummyrec);    /* else, discard next record    */
	return (code);
	}

/*		     	******* block construct  **********	   	   */

    if (subtype == DIFEND)			/* .ifend record	   */
	{
	(*db).inblock = (*db).c_valid = FALSE;	/* turn off flags  	   */
	return(SUCCESS);    
	}
    (*db).inblock = TRUE;			/* within block construct  */
    if ((subtype == DIF && arg_match) || 
		(subtype == DIFN && !arg_match))			
	{					/* .if/.ifn  record	   */
	(*db).c_valid = TRUE;			/* condition valid 	   */
	return (SUCCESS);			/* position okay	   */
	}
    else if (subtype == DELSE)
	{
	if (!(*db).c_valid && arg_match)	/* check only if not yet valid*/ 
	    {
	    (*db).c_valid = TRUE;		/* if matches, okay	   */
	    return (SUCCESS);
	    }
	}
    do					/* read next conditional record	   */
	{
    	code = f_read(sptr, dummyrec);
    	SUBSTITUTE(dummyrec, STRINGSIZ);
        }
    while (code == SUCCESS && rectype(dummyrec) != DCONDITION);
    if (code == SUCCESS)
	code = pos_next(db, sptr, dummyrec);   	/*  position accordingly   */
    return (code);    
    }

/*
 * 	rectype. Determine the type of a given record (directive/text/comment).
 *	Note: A directive record must start with a "." in the first character
 * 	position to avoid conflict with continuation lines within a procedure .
 */

FUNCTION   static  CODE  rectype
(
 TEXT	*recptr		/* IN: pointer to text record    */
 )
    {
    TEXT	*ptr;
    COUNT	i;


    static  struct  DTABLE rt_tbl[] =		/* table of record types */ 
	{
	{".PAGE",	DPAGE	   },
	{".INCLUDE",	DINCLUDE   },
	{".REFERENCE",	DREFERENCE },
	{".IF1",	DCONDITION },
	{".IFN1",	DCONDITION },
	{".IF",	   	DCONDITION },
	{".ELSEIF",	DCONDITION },
	{".IFN",	DCONDITION },
	{".IFEND",	DCONDITION },
	{"",		0 }
	};

    if (*recptr == '.')				/* if starts with a '.'    */
	{
	for (i=0; !NULLSTR(rt_tbl[i].name); i++)
	    if (s_lseq(rt_tbl[i].name, recptr))
	        return (rt_tbl[i].type);	
	return (DIRECTIVE);			/* default case		   */
        }
    for (ptr = recptr; iswhite(*ptr) && *ptr!= EOS ;ptr++)
	;					/* get first nonblank char */
    if (*ptr == '!') return(DCOMMENT);		/* a comment record */
    return(DTEXT);				/* else, assume text */
    }

/*
 *	get_ctype. Determine the subtype of a conditional record.
 */

FUNCTION  static  VOID  get_ctype
(    
 TEXT	record[],		/* in: record with conditional stmt */
 CODE	*subtype,			/* out: record subtype		*/
 TEXT	arg[]			/* out: argument of condition	*/
)
    {
    TEXT	name[STRINGSIZ+1];
    COUNT	i;

    static struct DTABLE ctable[] = 
	{
	{".if1",	DIF1 } ,
	{".ifn1",     	DIFN1 },
	{".if",		DIF },
	{".elseif",	DELSE },
	{".ifn",	DIFN },
	{".ifend",	DIFEND }, 
 	{"",		0 } 
	};

    arg[0] = EOS;				/* initialize	     */
    parserec(record, name, arg);		/* parse the record  */
    for (i=0; !NULLSTR(ctable[i].name); i++)
	{	
	if (s_equal(name,ctable[i].name))	/* match found	     */
	    break;
	}
    *subtype = ctable[i].type;			/* get type	     */
    return;
    }


/*
 * Strip leading white space.
 */

FUNCTION static  VOID  striplw
(  
 TEXT	*ptr			/* IN/OUT: text record */
   )
    {
    TEXT	*s;			/* record address */

    s = ptr;
    while (iswhite(*ptr) && *ptr!= EOS )		/* if blank or tab */
	ptr++;
    if (ptr > s) s_shift(s, (ptr-s));		/* left shift record */
    return;
    }

/*
 * Strip trailing comments and white spaces from a text record
 */

FUNCTION  VOID  s_stptrl
(
 TEXT	*ptr			/* IN/OUT: text record */
 )
    {
    TEXT	*i;			/* index to a character */
    BOOL	quote;			/* true if inside a quote */


    quote = FALSE;
    for (i = ptr; *i != EOS; i++)		/* scan each character */
	{
	if (*i == '"') quote = !quote;		/* if quote, reverse state */
	if (*i == '!' && !quote) break; 	/* start of comment */
        }
    i-- ;					/* new logical end of string */
    
    if (i >= ptr)				/* not a null string */
	while (iswhite(*i) && *i!= EOS ) i--; 	/* ignore trailing blank,tab */
    *(++i) = EOS;				/* new end of string */
    return;	
    }		



/* 	d_remote. Set the remote indicator.
 *
 *	NOTE: This routine must be called after the d_init call to
 *	indicate that the include files referenced in this text file
 *	are located at the remote node.
 */

FUNCTION  FUNCTION_PTR_INT d_remote
(
 struct  DIRBLK	*db,		/* in: pointer to directive block */
 struct  CONTXT	*ctxptr,	/* in: pointer to current context */
 FUNCTION_PTR_INT	get_reminc	/* in: pointer to file handler	  */
 )
    {
    FUNCTION_PTR_INT old_get_reminc;

    (*db).remctx = ctxptr;
    old_get_reminc = d_get_reminc;
    d_get_reminc = get_reminc;	
    return (old_get_reminc);
    }
