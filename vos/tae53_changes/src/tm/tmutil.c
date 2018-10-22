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



/*	TAE monitor utility functions.
 *	The functions in this source file are in alphabetical order.
 *
 * 	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	11-oct-83	Fixed unix compilation; highlight returns number
 *			of non-printable charactes...palm
 *	25-oct-83	Moved hightlight function to terminal.cnp...palm
 *	19-jan-84	New high_lfield...palm
 *			Also, new hdisptxt, hdisprtxt...palm
 *	28-feb-84	Inserted routine to build header for global help...lim
 *	07-may-84	Move the IMPORT declaration...lim
 *			'toprefs'-->'numrefs'...nhe
 *	02-aug-84	"Terminal Monitor" --> "TAE Monitor" (PR396)...peb
 *	14-aug-84	Remove input parameter 'msg' from addproc and change
 *			format of message...lia
 *	01-oct-84	Add 'class' arg to addint...nhe
 *	16-oct-84	Add handling of internal procs...nhe
 *	09-nov-84	Comments to addref; clean usearch logic; 
 *			new int_search function (PR 892)...palm
 *	27-nov-84	TCL 67: search() to find parm qualifs...peb
 *	04-dec-84	Conditionalize functions needed by rcjm agent...dm
 *	15-dec-84	TCL 97: change addproc to pick-up line # from SEARCH
 *			block if processing a goto...lia
 *	18-dec-84	Fix line # for onfail error...lia
 *
 ******  CHANGES  MADE IN THE RCJM TREE  **************
 *
 *	31-mar-85	Move functions irange() and rrange() to library for
 *			use by parfile encoding/decoding to ascii...dm
 *
 *****  MERGE WITH THE FOLLOWING TAE_V1.3 CHANGES...dm (24-may-85)
 *
 *	22-mar-85	Output sub-command name (if it exists) on the header
 *                      line of a help-parm screen (left_pbld).
 *
 **************************************************************************  
 *
 *	10-sep-86	Add comments to addproc() for PR 1129 fix...peb
 *	03-apr-87	Fix usearch to take advantage of lookex's new 
 *			capability to find qualifiers to any level and
 *			for any class of variable...palm
 *	27-may-87	Temporary workaround in gettitle() for windows ...dm 
 *	02-may-88	New IsParFile function...palm
 *	01-feb-89	VAX C complained about a missing ';'...ljn
 *	06-feb-89	New parhdrRead function...palm
 *	10-aug-94	PR2830: added maxc, count and valvec to addint...krw
 */


#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"terminc.inc"
#include	"dirinc.inc"		/* includes for d_package		*/
#include	"parblk.inc"		/* PAR file definition */
#include "taeintproto.h"

FUNCTION struct VARIABLE *srchrf 
(
    struct VARIABLE	*refs[],	/* in:  vector of pointers to glbl variabs*/
    FUNINT		numref,		/* in:  number of glbls (zero = none)	*/
    TEXT		name[]		/* in:  name of variable to search for	*/

 );






/* addint - add an integer variable to a symbol table.
 * Returns FAIL if dynamic memory overflow encountered.
 */

FUNCTION CODE addint 
(
    struct SYMTAB	*st,		/* in/out: symbol table			*/
    TEXT		name[],		/* in:  name of variable to add		*/
    FUNINT		maxc,		/* in:  maximum count			*/
    FUNINT		count,		/* in:  count				*/
    FUNINT		valvec[],	/* in:  values of integer		*/
    FUNINT		class		/* in: class (V_LOCAL, etc.)		*/

 )
    {
    struct VARIABLE	*v;
    TAEINT		*civ;		/* integer value in st			*/
    FAST int		i;

    if ((v = allvar(st)) == NULL)	/* allocate new variable		*/
	return(FAIL);
    s_copy(name, (*v).v_name);
    (*v).v_class = class;
    (*v).v_type    = V_INTEGER;
    (*v).v_minc = 1;
    (*v).v_maxc = maxc;
    (*v).v_count = count;
    (*v).v_dcount  = 0;
    (*v).v_default = FALSE;
    (*v).v_size    = 0;
    (*v).v_valid = (*v).v_cvp = (*v).v_dvp = NULL;
    if (((*v).v_cvp = allval(v)) == NULL)
	return(FAIL);
    civ  = (TAEINT *) (*v).v_cvp;
    for (i=0; i<count; ++i)
	civ[i] = (TAEINT) valvec[i];
    return(SUCCESS);
    }

/*	addref - add a reference in the current proc context
 *	to an existing global variable.
 *
 *	CAUTION: do not call this -- we are trying to
 *	phase it out because (a) it does a lookex that
 *	the caller should have already done and (b)
 *	it does not check against the size (MAXREF) of 
 *	the ref array.	
 */

FUNCTION VOID addref 
(
    struct CONTXT	*pctx,		/* in/out: proc ocntext			*/
    TEXT		name[]		/* in:  name of global variable		*/

 )
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table			*/
    struct VARIABLE	*v;

    (*pctx).numrefs++;			/* bump number of references		*/
    v = lookex(&glbtab, name);		/* point to global variable		*/
    (*pctx).refs[((*pctx).numrefs)-1] = v;	/* set reference pointer		*/
    (*v).v_refcnt++;			/* bump the ref count for this global	*/
    return;
    }

/*	addstr2 - add a string variable to a symbol table.
 *	Returns FAIL if dynamic memory overflow encountered.
 */

FUNCTION CODE addstr2 
(
    struct SYMTAB	*st,		/* in/out: symbol table		*/
    TEXT		name[],		/* in:  name of variable to add	*/
    FUNINT		maxc,		/* in: max value count		*/
    FUNINT		count,		/* in: current value count	*/
    TEXT		*val[],		/* in:  string value vector	*/
    FUNINT		class		/* in: class (for v_class)	*/

 )
    {
    struct VARIABLE	*v;


    if ((v = allvar(st)) == NULL)	/* allocate new variable	*/
	return(FAIL);
    s_copy(name, (*v).v_name);
    (*v).v_type  = V_STRING;
    (*v).v_class = class;
    (*v).v_count = 0;			/* otherwise set_value deallocates */
    (*v).v_minc = 1;
    (*v).v_maxc = maxc;
    (*v).v_size = STRINGSIZ;
    if (((*v).v_cvp = allval(v)) == NULL)
	return(FAIL);
    if (set_value(v, (GENPTR)val, count) != SUCCESS)
        return (FAIL);
    return(SUCCESS);
    }

/*
 *	addproc - add proc information (name and line number) to message
 *	if we are in a PDF.  For primary level, a one-line message.
 *
 *	Note on pdf_line:  
 *
 *	The pdf_line field is the line number of the last
 *	line following the first line of the current command.
 *	To report an error in the
 *	current command, we use pdf_line + 1.  This is done because
 *	line number boundaries for one command string are lost when
 *	getpst resolves continuation.  When reporting an error, it
 *	is better for the user to see the line number of the first line 
 *	of the offending command (rather than the line number of the
 *	last line of the offending command).
 *
 *	Note that internal procs are handled by referring back to the
 *	internal proc block.
 */
 
FUNCTION  VOID  addproc
(
    struct  CONTXT  *pctx,		/* IN: proc context */
    TEXT	    record[]		/* IN/OUT: formatted line */

 )
    {
    COUNT	num;
    TEXT	linenum[12];
    struct CONTXT *ctx;
    struct SEARCH *srch;

    if ((*pctx).prclevel > 0)			/* fancy only for PDFs	*/
        {
    	if (!(s_equal((*pctx).pdf.libr, "/LOCAL/"))) /* flag for internal proc */
    	    {
	    s_append(" proc '", record);
	    s_append((*pctx).pdf.name, record);	/* copy proc name	*/
	    s_append("', line ", record );
	    num = -1;
	    if ((*pctx).srchblk != NULL)
		{
		srch = (struct SEARCH *) (*pctx).srchblk;
		if ((*srch).type == SRCH_GOTO)
		    {
		    num = (*srch).cmd_line + 1;
		    if ((*srch).onfailcmd)	/* if onfail cmd under process */
			num = num - 1;		/* already at line causing onfail */
		    }
		}
	    if (num < 0)
		{
		num = (*pctx).pdf_line + 1;
		if ((*pctx).onfailcmd)		/* if onfail cmd under process */
		    num = num - 1;		/* already at line causing onfaile */
		}
	    s_i2s(num, linenum);		/* convert line number to string */
	    s_append(linenum, record);		/* copy line number	*/
    	    }
    	else 
    	    {
	    s_append(" line ", record );
	    num = -1;
	    if ((*pctx).srchblk != NULL)
		{
		srch = (struct SEARCH *) (*pctx).srchblk;
		if ((*srch).type == SRCH_GOTO)
		    {
		    num = (*srch).cmd_line + 1;
		    if ((*srch).onfailcmd)
			num = num - 1;
		    }
		}
	    if (num < 0)
		{
		num = (*pctx).pdf_line + 1;
		if ((*pctx).onfailcmd)
		    num = num - 1;
		}
	    s_i2s(num, linenum);
	    s_append(linenum, record);		/* copy line number	*/
    	    s_append(" in internal proc '", record);
 	    s_append((*pctx).pdf.name, record);
	    s_append("' in proc '", record);
    	    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	    s_append((*ctx).pdf.name, record);	/* copy containing proc name */
    	    s_append("'", record);
    	    }    	    
    	}
    return;
    }     

/*
 *	dsphdr - display header on formatted screen.
 */

FUNCTION VOID dsphdr 
(
    TEXT	dspname[],		/* in:  display name		*/
    TEXT	left_str[],		/* in:  left string for header	*/
    FUNINT	pagenum,		/* in:  page number 		*/
					/* (.le. 0  means no page nrs)	*/
    FUNINT	lastflag		/* in:  TRUE if last page 	*/

 )
    {
    IMPORT COUNT	termcols;	/* number columns on terminal screen	*/
    IMPORT CODE		termtype;	/* terminal type			*/

    COUNT	n = 0;			/* number of non-printables	*/
    TEXT	header[STRINGSIZ+1];	/* header record		*/
    COUNT	nchar;			/* character count		*/
    TEXT	number[12];		/* page number			*/
    COUNT	page_col;		/* column where "PAGE" is	*/

#define PAGE_OFF   5		/* "PAGE NN." starts at termcols-*	*/
    
    page_col = termcols - PAGE_OFF;
    if (termtype == T_CRT)
	{
	s_copy(dspname, header);	     
        if ( !NULLSTR(dspname) )
		{
		s_append(":", header);
		n = t_highlight(header);	/* hightlight header	*/
   		s_append(" ", header);
		}
        nchar = s_append(left_str, header);	/* copy left side string */
	if (nchar - n > page_col-1)			/* if too long		*/
	    s_copy ("...", &header[page_col-5]);
   	t_output(HDRLIN, 1, header);		/* write LHS of header 	*/
	}

    if (pagenum <= 0)				/* suppress page nr	*/
	return;		
    s_copy("Pg ", header);
    s_i2s(pagenum, number);			/* page number to string */
    nchar = s_append(number, header);		/* copy page number */
    header[nchar++] = (lastflag)  ?  '.' : '+';	/* add continuation mark */
    header[nchar] = EOS;
    if (termtype == T_CRT) 
        t_output(HDRLIN, page_col, header);
    else
	t_write(header, T_DOUBLE);
    return;
    }
	
/* gettitle - read in .TITLE text & return it to the caller.
 * Assumes help file is already positioned past .TITLE.
 */

FUNCTION CODE gettitle 
(
    struct DIRBLK	*db,		/* in/out: directive block for help file*/
    struct TXTSTOR	*title,		/* out: title text in dynamic storage	*/
    TEXT		errmsg[],	/* out: error message			*/
    TEXT		key[]		/* out: error key			*/

 )
    {
    IMPORT COUNT	termcols;	/* number columns on terminal screen	*/

    TEXT		buf[STRINGSIZ+1];
    CODE		code;

    initxt(title);			/* init dynamic storage text manip pkg	*/
    while((code = d_text(db, buf)) != D_EOT)	/* read .TITLE text to next directive*/
	{
	if (code != SUCCESS) goto gt_rderr;

/***  TBD: don't use termcols for window version; it is huge.  ****/
	buf[80] = EOS;		/* enforce max str length = screen width*/
	if (addtxt(title, buf) != SUCCESS) goto gt_overr;	/* add string to block*/
	}
    return(SUCCESS);

gt_rderr:
    s_copy("Error reading the title text.", errmsg);
    s_copy("TAE-TTLERR", key);
    fretxt(title);
    return(FAIL);

gt_overr:
    s_copy("TAE Monitor internal memory overflow.", errmsg);
    s_copy("TAE-MEMOVR", key);
    fretxt(title);
    return(FAIL);
    }

/*	high_lfield.    Build a left justified field with
 *	terminal highlighting sequence.
 */

FUNCTION VOID high_lfield 
(
    TEXT	input[],	/* in: input string		*/
    FUNINT	width,		/* in: width of field		*/
    TEXT	output[]	/* out: string with high sequence */

 )
    {
    COUNT	i;

    i = s_copy (input, output);
    s_blank (&output[i], width-i);	/* blank fill field	*/
    t_highlight (output);		/* add escape sequences	*/
    return;
    }    


/*	IsParFile.
 *
 *	Return TRUE if the file looks like a binary PAR file. 
 *	This assumes that the file is positioned at the
 *	beginning.
 */

FUNCTION BOOL IsParFile 
(
	struct SFILE *sfile
 )
{
CODE	code;
struct PARHDR	parhdr;

#ifdef VAX_VMS

COUNT		bytes;

/*************
	In VMS, binary and text files have the same basic
	format, so we can just read the record whether it
	is text or binary.
**************/

code = f_bread (sfile, &parhdr, sizeof(parhdr), &bytes);
f_rewind (sfile);
if (code != SUCCESS)
    return (FALSE);

/*****	
	if it's an old PAR file, we call it a PAR file and
	let someone else complain about the format:
******/

if (s_equal (parhdr.sentinel, OLD_P_SENTINEL) || 
    s_equal (parhdr.sentinel, P_SENTINEL)          )
    return (TRUE);
if (bytes == sizeof (parhdr)  &&  s_equal (parhdr.sentinel, P_BIGSENTINEL))
    return (TRUE);
return (FALSE); 

#endif


#ifdef UNIX

/*********
	Strategy for UNIX files.   Make sure the binary record
	byte count is correct, then check the PARHDR record

	We can't call f_bread directly, because the file may
	be a text file with garbage record counts.
**********/

COUNT	bytes;
code = fread (&bytes, sizeof (bytes), 1, (*sfile).fileptr);
if (code != 1)
    {
    f_rewind (sfile);
    return (FALSE);
    }
code = fread (&parhdr, sizeof (parhdr), 1, (*sfile).fileptr);
f_rewind (sfile);

/*****	
	if it's an old PAR file, we call it a PAR file and
	let someone else complain about the format:
*****/

if (s_equal (parhdr.sentinel, OLD_P_SENTINEL) ||
    s_equal (parhdr.sentinel, P_SENTINEL) )
    return (TRUE);
if (bytes == sizeof (parhdr)  &&   s_equal (parhdr.sentinel, P_BIGSENTINEL))
     return (TRUE);
return (FALSE); 

#endif

}

/*
 * left_fbld . Build the left header with file/library name for display.
 */

FUNCTION  VOID  left_fbld
(
    TEXT	libname[],		/* IN: library name		*/
    TEXT	fname[],		/* IN: proc/mdf  name		*/
    TEXT	subc[],			/* IN: subcmd name (ignor if null)*/
    TEXT	type[],			/* IN: mdf/proc/command	*/
    TEXT	lefthead[]		/* OUT: string to be displayed	*/

 )
    {
    TEXT	filename[FSPECSIZ+1];

    lefthead[0] = EOS;
    if ( !NULLSTR(fname) )
        {
    	s_copy(type, lefthead);			/* indicate type*/
    	s_append(" \"", lefthead);    	
    	f_name(fname, filename);		/* ignore library, if embedded */
    	s_append(filename, lefthead);		/* add file name */
    	if (!NULLSTR(subc))
	{
	s_append("-", lefthead);
	s_append(subc, lefthead);
	}
    }
    if (!s_equal(type, "command"))		/* if not command */
	{
        if ( !NULLSTR(libname) )
        	{
        	if ( !NULLSTR(fname) )
			s_append("\" ", lefthead);	
		s_append("library \"", lefthead);	
		s_append(libname, lefthead);		/* add library name */
		}
	}
    if ( !NULLSTR(fname) || !NULLSTR(libname) )
    	s_append("\"", lefthead);		
    return;
    }

/* left_gbld - build the left side of header for global help
 *
 */

FUNCTION VOID left_gbld
(
    TEXT		pdfname[],	/* in: the name of the pdf file		*/
    TEXT		varname[],	/* in: the name of the global variable		*/
    TEXT		type[],		/* in: global				*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 )
    {
    lefthead[0]=EOS;
    if (!NULLSTR(varname))		/* no global variable ? */
	{
	s_append(type, lefthead);
	s_append(" \"", lefthead);
	s_append(varname, lefthead);
	s_append("\"", lefthead);
	s_append(", defined in \"", lefthead);
	s_append(pdfname, lefthead);
	s_append("\"", lefthead);
	}
    return;
    }

/* left_pbld - build the left side of header for parameter help
 *
 */

FUNCTION VOID left_pbld
(
    TEXT		procname[],	/* in: the name of the proc		*/
    TEXT		subcname[],	/* in: the name of the subcommand		*/
    TEXT		parmname[],	/* in: the name of the parm		*/
    TEXT		type[],		/* in: proc/command			*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 )
    {
    TEXT		filename[FSPECSIZ+1];

    lefthead[0]=EOS;
    if (!NULLSTR(parmname))			/* no parm?				*/
	{
	s_append(" parameter \"", lefthead);
	s_append(parmname, lefthead);
	s_append("\"", lefthead);
	}
    if (!NULLSTR(procname))
	{
	if (s_equal(type, "proc"))
	    f_name(procname, filename);		/* ignore libname ,if any */
	else
	    s_copy(procname, filename);

	s_append(", ", lefthead);
	s_append(type, lefthead);
	s_append(" \"", lefthead);
	s_append(filename, lefthead);		/* add proc file name */
        if (!NULLSTR(subcname))
           {
           s_append("-", lefthead);
           s_append(subcname, lefthead);
           }
	s_append("\"", lefthead);
	}
    return;
    }

/* left_sbld - build the left side of header for subcommand help.
 *
 */

FUNCTION VOID left_sbld
(
    TEXT		procname[],	/* in: the name of the proc		*/
    TEXT		subcname[],	/* in: the name of the subcommand	*/
    TEXT		type[],		/* in: proc/command			*/
    TEXT		lefthead[]	/* out: left side of the header		*/

/* Actually we only build the part of the header that goes after the display name 	*/

 )
    {
    TEXT		filename[FSPECSIZ+1];

    lefthead[0]=EOS;
    if (!NULLSTR(subcname))			/* no subcommand?		*/
	{
	s_append(" subcommand \"", lefthead);
	s_append(subcname, lefthead);
	s_append("\"", lefthead);
	}
    if (!NULLSTR(procname))
	{
	if (s_equal(type, "proc"))
	    f_name(procname, filename);		/* ignore libname ,if any */
	else
	    s_copy(procname, filename);
	s_append(", ", lefthead);
	s_append(type, lefthead);
	s_append(" \"", lefthead);
	s_append(filename, lefthead);		/* add proc file name */
	s_append("\"", lefthead);
	}
    return;
    }


/*	search - exact search of local, parameters, and references.	
 *	There are two major uses of this:
 *
 *    -- To find out if a variable already exists.
 *    -- To find the value of a variable
 *
 *	The search here resolves indirection for NAME parameters.
 *	(For an unresolved search,  see usearch.)
 */    
    
FUNCTION struct VARIABLE *search 
(
    TEXT		name[],		/* in:  variable name		*/
    struct CONTXT	*pctx		/* in:  proc context		*/

 )
    {
    FAST struct VARIABLE *p;		


    p = usearch(name, pctx);		/* unresolved search		*/
    if (p == NULL)
        return (NULL);
    p = RESOLVE(p);			/* resolve indirection		*/
    return(p);
    }

/*
 *	usearch.   Same as search, but the VARIABLE pointer is not
 *	RESOLVEd.  This is called by someone who wants to work
 *	with the original VARIABLE structure for NAME parameters.
 *
 *	Note that qualifiers may exist for any class of variable
 *	and that lookex now resolves multiple levels of qualifiers.
 */
FUNCTION struct VARIABLE *usearch 
(
    TEXT		name[],		/* in:  variable name		*/
    FAST struct CONTXT	*pctx		/* in:  proc context		*/

 )
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table		*/
    FAST struct VARIABLE *p;		/* current ptr			*/


    p = lookex (&((*pctx).locst), name);	/* try local table */
    if (p != NULL)
	return (p);
    p = lookex(&((*pctx).parmst), name); 	/* try parm table  */
    if (p != NULL)
	return (p);
    if ((*pctx).prclevel > 0)			/* try globals     */
	p = srchrf((*pctx).refs, (*pctx).numrefs, name);
    else
	p = lookex(&glbtab, name);
    if (p != NULL)
	return (p);

    /*   Variable not found so far.  If local proc, try telescoping:	*/

    if (s_equal((*pctx).pdf.libr, "/LOCAL/"))  
	return (usearch (name, (*pctx).backlink));
    else
	return (NULL);
    }

/*	int_search - exact search of local, parameters, 
 *	and references internal to the current proc.
 *
 *	This differs from the normal 'search' and 'usearch' functions
 *	in that outer procs are not searched.
 *	This function is used by declare.c to determine
 *	variable existence, allowing internal procs to re-define
 *	(and override) variables defined in outer procs.	
 *	
 *	NOTE: function value is an 'unresolved' VARIABLE pointer.
 *
 */    
    
FUNCTION struct VARIABLE *int_search 
(
    FAST TEXT		name[],		/* in:  variable name		*/
    FAST struct CONTXT	*pctx		/* in:  proc context		*/

 )
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table		*/
    FAST struct VARIABLE *p;

    p = lookex(&((*pctx).locst), name);		/* in local table?	*/
    if (p != NULL)
	return(p);
    p = lookex(&((*pctx).parmst), name);	/* in parm table?	*/
    if (p != NULL)
	return(p);
    if ((*pctx).prclevel > 0)
	p = srchrf((*pctx).refs, (*pctx).numrefs, name);  /* gbl refs?	*/
    else
	p = lookex(&glbtab, name);
    return(p);
    }

/* srchrf - search for a variable among a list of referenced globals.
 * Returns NULL if not found.
 */

FUNCTION struct VARIABLE *srchrf 
(
    struct VARIABLE	*refs[],	/* in:  vector of pointers to glbl variabs*/
    FUNINT		numref,		/* in:  number of glbls (zero = none)	*/
    TEXT		name[]		/* in:  name of variable to search for	*/

 )
    {
    COUNT		i;
    struct VARIABLE	*v;
    COUNT		period_index;
    TEXT		tmp_name[STRINGSIZ+1];

    s_copy (name, tmp_name);
    period_index = s_index (tmp_name, '.');	/* qualified?		  */
    if (period_index > 0)
	tmp_name [period_index] = EOS;		/* clip at first name	  */
    for (i = 0; i < numref; i++)
	{
	v = refs[i];
	if (s_equal(tmp_name, (*v).v_name))
	    {
	    if (period_index > 0)
		v = lookex (&(*v).v_qualst, &tmp_name[period_index+1]);
	    return(v);
	    }
	}
    return(NULL);
    }


/* The following functions in this package manipulate and display 
 * blocks of dynamically allocated text.
 * The text is stored in a TXTSTOR structure, which must be initialized
 * by calling initxt before calling other functions.
 */



/* addtxt - add text to a TXTSTOR block (dynamic storage).
 * Returns SUCCESS unless dynamic memory overflow.
 */

FUNCTION CODE addtxt 
(
    struct TXTSTOR	*block,		/* in/out: bloct to add a line of text to*/
    TEXT		str[]		/* in:  string to add			*/

 )
    {
    COUNT		num;
    TEXT		**np;

    num = (*block).numline;
    if ((np = (TEXT **)tae_alloc(num+1, sizeof(TEXT *))) == NULL)
	return(FAIL);
    if (num != 0)
	bytmov((GENPTR)(*block).tp, (GENPTR)np, num * sizeof(TEXT *));	/* move old ptrs to new vector*/
    if ((np[num] = s_save(str)) == NULL)	/* put str in dynamic memory	*/
	{
	tae_free((GENPTR)np);
	return(FAIL);
	}
    if (num != 0)
	tae_free((GENPTR)(*block).tp);	/* free old ptr vector			*/
    (*block).tp = np;			/* point to new pointer vector		*/
    (*block).numline++;
    return(SUCCESS);
    }

/* brktxt - break text up for display field width.
 * Returns FAIL if not enough dynamic storage available.
 */

FUNCTION CODE brktxt 
(
    struct TXTSTOR	*block,		/* in/out: text storage block		*/
    FUNINT		width		/* in:  field width			*/

 )
    {
    struct TXTSTOR	tmpblk;
    COUNT		i;
    COUNT		ninlin;		/* number of lines in block on input	*/
    TEXT		**inval;
    TEXT		instr[STRINGSIZ+1];
    char		savchr;
    COUNT		strlen;
    COUNT		bklen;
    COUNT		start;

    if ((*block).numline == 0)
	return(SUCCESS);
    tmpblk.numline = 0;
    ninlin = (*block).numline;
    inval  = (*block).tp;		/* point to input vector of str ptrs	*/
    for (i = 0; i < ninlin; i++)	/* for each string in input block	*/
	{
	s_copy(inval[i], instr);
	strlen = s_length(instr);
	start  = 0;
	do
	    {
	    bklen = min(s_length(&instr[start]), width);
	    savchr = instr[start+bklen];
	    instr[start+bklen] = EOS;	/* temporarily cut string off to width	*/
	    if (addtxt(&tmpblk, &instr[start]) != SUCCESS)
		{
		fretxt(&tmpblk);
		return(FAIL);
		}
	    instr[start+bklen] = savchr;
	    start += bklen;
	    } while (start < strlen);
	}
    for (i = 0; i < ninlin; i++)	/* for each string in input block	*/
	s_free(inval[i]);		/* free the string from input block	*/
    tae_free((GENPTR)inval);		/* free the input pointer vector	*/
    (*block).numline = tmpblk.numline;
    (*block).tp      = tmpblk.tp;
    return(SUCCESS);
    }

/* choptxt - chop a TXTSTOR structure off at a max number of strings.
 */

FUNCTION VOID choptxt 
(
    struct TXTSTOR	*block,		/* in/out: block to chop off		*/
    FUNINT		maxstr		/* in:  max # strings allowed in block	*/

 )
    {
    TEXT		**str;
    COUNT		i;

    if (maxstr <= 0  ||  (*block).numline <= maxstr)	/* only chop if too large*/
	return;
    str = (*block).tp;
    for (i = maxstr; i < (*block).numline; i++)
	s_free(str[i]);
    (*block).numline = maxstr;
    return;
    }

/* disprtxt - display, right-justified,  dynamically stored multi-line text
 * starting at a specified line.
 * Assumes terminal type is T_CRT.
 * Assumes storage block was initialized with initxt and manipulated
 * with functions in this package.
 */

FUNCTION VOID disprtxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		rtcol,		/* in:  column number to right-justify against*/
    struct TXTSTOR	*block		/* in:  storage block containing dynam stored text*/

 )
    {
    TEXT		**str;
    COUNT		i;
    COUNT		col;

    str = (*block).tp;
    for (i = 0; i < (*block).numline; i++, line++)
	{
	col = rtcol - s_length(str[i]) + 1;
	wrttxt(line, col, str[i], FALSE);
	}
    return;
    }

/* disptxt - display dynamically stored multi-line text starting at a specified
 * line and column.  Assumes terminal type is T_CRT.
 * Assumes storage block was initialized with initxt and manipulated
 * with functions in this package.
 */

FUNCTION VOID disptxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		col,		/* in:  column number to start each line*/
    struct TXTSTOR	*block		/* in:  storage block containing dynam stored text*/

 )
    {
    TEXT		**str;
    COUNT		i;

    str = (*block).tp;
    for (i = 0; i < (*block).numline; i++, line++)
	wrttxt(line, col, str[i], FALSE);
    return;
    }

/* fretxt - free stored text from dynamic memory.
 * The text in this block must have been stored using functions in this
 * package.
 */

FUNCTION VOID fretxt 
(
    struct TXTSTOR	*block

 )
    {
    TEXT		**str;
    COUNT		i;

    if ((*block).numline == 0)
	return;
    str = (*block).tp;
    for (i = 0; i < (*block).numline; i++)
	s_free(str[i]);		/* free each string			*/
    tae_free((GENPTR)str);		/* free the pointer vector		*/
    (*block).numline = 0;
    return;
    }

/*	hdisprtxt.  Display right justified text. 
 *	Highlighting may be turned on/off.
 *	Like disprtxt.
 */

FUNCTION VOID hdisprtxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		rtcol,		/* in:  column number to right-justify against*/
    struct TXTSTOR	*block,		/* in:  storage block containing dynam stored text*/
    FUNINT		width,		/* in: width of field			*/
    BOOL		highlight	/* in: TRUE to highlight FALSE to	*/
	    				/*     turn highlight off		*/

 )
    {
    TEXT		**str;
    COUNT		i, j;
    COUNT		col;
    TEXT		string[STRINGSIZ+1];

    str = (*block).tp;
    for (i = 0; i < (*block).numline; i++, line++)
	{
	s_blank (string, width);
	j = s_length (str[i]);
	s_copy (str[i], &string[width-j]);
        if (highlight)
	    t_highlight (string);
	col = rtcol - width + 1;
	wrttxt(line, col, string, FALSE);
	}
    return;
    }

/* 	hdisptxt.   Display left justified text.
 *	Like disptxt but text is highlighted or highlighting is
 *	turned off.
 */

FUNCTION VOID hdisptxt 
(
    FUNINT		line,		/* in:  display line # to start		*/
    FUNINT		col,		/* in:  column number to start each line*/
    struct TXTSTOR	*block,		/* in:  storage block containing dynam stored text*/
    FUNINT		width,		/* in: width of field			*/
    BOOL		highlight	/* in: TRUE to highlight FALSE		*/
					/*     to turn highlight off		*/

 )
    {
    TEXT		**str;
    COUNT		i, j;
    TEXT		string[STRINGSIZ+1];

    str = (*block).tp;
    for (i = 0; i < (*block).numline; i++, line++)
	{
	if (highlight)
	    high_lfield (str[i], width, string);
	else
	    {
	    j = s_copy (str[i], string);
	    s_blank (&string[j], width-j);
	    }
	wrttxt(line, col, string, FALSE);
	}
    return;
    }

/* initxt - initialize a TXTSTOR block for subsequent calls to this package.
 */

FUNCTION VOID initxt 
(
    struct TXTSTOR	*block		/* out:  block to initialize		*/

 )
    {
    (*block).numline = 0;
    return;
    }

/*	parhdrRead.    Read and check the header record of a par file.
 *
 *	returns FAIL if a problem, with the file already closed. 
 */

FUNCTION CODE parhdrRead 
(
	struct SFILE	*sfile,		/* in: opened SFILE	*/
	struct PARHDR	*parhdr,	/* out: buffer to receive hdr */
	TEXT		errmsg[STRINGSIZ+1],
	TEXT		errkey[STRINGSIZ+1]

 )
{
CODE	code;
COUNT	recsize;

errmsg[0] = EOS;
errkey[0] = EOS;
 zero_block ((GENPTR) parhdr, sizeof (*parhdr));
 code = f_bread (sfile, (GENPTR) parhdr, sizeof(*parhdr), &recsize);
if (code != SUCCESS)
    {
    sprintf (errmsg, "Error reading file. %s.", (*sfile).errmsg);
    s_copy ("TAE-RDERR", errkey);
    f_close (sfile, F_KEEP);
    return (FAIL);
    }
if (s_equal ((*parhdr).sentinel, OLD_P_SENTINEL) ) 
    {
    s_copy ("File has obsolete format.  Type \"?\" for conversion info.", 
		errmsg);
    s_copy ("TAE-OLDPARFILE", errkey);
    f_close (sfile, F_KEEP);
    return (FAIL);
    }
if (!s_equal ((*parhdr).sentinel, P_BIGSENTINEL))
    {
    s_copy ("File is not correctly formatted.", errmsg); 
    s_copy ("TAE-FMTRESTR", errkey);
    f_close (sfile, F_KEEP);
    return (FAIL);
    }
return (SUCCESS);
}
