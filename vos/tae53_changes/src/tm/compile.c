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



/*
 * This file contains:
 *
 *	- The do function for the COMPILE intrinsic command,
 *	  along with supporting functions.  The COMPILE command
 *	  compiles a PDF into a faster executing binary file.
 * 	  This compilation is an optimization step that is not
 *	  required for valid execution of a PDF.
 *
 *	- The function "prc_compiled" which is called by pdftab,
 *	  after a proc is determined to be compiled,
 *	  in order to build the context block symbol tables.
 *
 *	- The function "prep_c_help" which does preparation
 *	  for displaying help on a compiled PDF.
 *
 * CHANGE LOG:
 *
 *	20-oct-84	Add prep_c_help function...peb
 *	23-oct-84	Add logic for subcommand compilation...peb
 *	24-oct-84	Add logic for input of compiled w/ subcs...peb
 *	01-nov-84	compilation to set "curproc" C global...peb
 *	12-nov-84	Handle NAME parms & gbl deref for PARM DEFAULT...peb
 *	25-jan-85	PR on lost open file quota for certain compiles...peb
 *	01-feb-85	Fix bug on too many parms...peb
 *	31-may-85	Fix crash on deref of gbl in PARM DEFAULT...peb
 *	16-july-85	Conditional compilation of f_bread before f_brewrite
 *			for VAX_VMS...dm
 *	25-jul-85	Fix UNIX lint compilation errors...dm
 *	03-oct-85	PR 946: Changed 'in_comp_pbk' to do run-time validation 
 *			on variables which are of type "file"...dab
 *	29-oct-85	PR 933: Allow compilation of parameter qualifiers by
 *			changing 'compile_do' and 'compilation' to properly open
 *			and position PDF file for qualifier data..dab
 *	08-dec-86	PR 1021: Reopened to fix PARM DEFAULT dereferencing
 *			a global in a compiled process...lia
 *	29-mar-87	Use LARGE_PARBLK to read PARBLKs from compiled PDFs
 *			so CPDs created outboard do not have to chunk into
 *			small records like COMPILE does...palm
 *	22-jul-87	Add get_compcmd() in effort to force TM into
 *			libraries...ljn
 *	30-jul-87	Make GLOBAL proc execution pick up current global
 *			values like it works for non-compiled PDFs.  That is,
 *			here we must do the work of declare:gbl_defaults...palm
 *	07-aug-87	PR1223: zero structs so spare fields get zeros...palm
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	05-dec-87	Executable PAR files...palm
 *	12-feb-88	Call pblk_out with LARGE_PARBLK so very large,
 *			qualified variables may be saved...palm
 * 01-feb-89	Portable VAX C doesn't like x = y = NULL...ljn
 * 08-feb-89	Use new parhdrRead...palm
 * 28-jun-90	Removed get_compcmd()...ljn
 * 19-apr-91	Because of VMS def of NULL, some rec fields changed to '0'...ljn
 * 22-oct-91	PR1220: Allow passthru to work from compiled PDF...ljn/bdean
 * 22-oct-92	Prototype of tae_alloc uncessary and Ultrix 4.3 does not like
 *		it...rt
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"fileinc.inp"		/* file & file position context structs	*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"tmhost.inp"		/* TM-only host dependent definitions	*/
#include 	"pgminc.inc"		/* for M_ codes				*/
#include	"parblk.inc"		/* PARBLK structs and defs		*/
#include	"compiled.inc"		/* for compiled PDFs			*/
#include	"dirinc.inc"		/* directive block struct and defs	*/
#include "taeintproto.h"


#define	CP_WRITE	0
#define	CP_REWRITE	1
#define PAR_FILE	0		/* std PAR files have .msgtyp=0 */

/* Memory-resident PDF data for the COMPILE command.
 * The parameters for the command are defined
 * by a vector of RESIDVAR structures.
 * The RESIDVAR structure is explained in the TMINC include file.
 */


    static struct RESIDVAR ptcomp[] =
	{
	/* name    type      k  m maxc    size     dc val      dvp*/

	  {"INPROC",  V_STRING, 0, 1, 1,    FSPECSIZ,   -1, NULL,    NULL},
	  {"OUTPROC", V_STRING, 0, 0, 1,    FSPECSIZ,   0,  NULL,    NULL}
	};




FUNCTION CODE compile_do 
(
    struct  CONTXT	*cpctx,		/* current proc context 	  */
    struct  CONTXT	*npctx		/* 'compile' command line context */

 );

    GLOBAL struct ITRCMD compcmd[] = 		/* compile commands  */
	{
{5, "COMPILE",  "",         Y_GENERAL,  I_NPM(ptcomp), ptcomp,  compile_do },

{0, "", 0, 0, 0, 0, 0}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};

FUNCTION static CODE get_help 
(
    TEXT		help_spec[FSPECSIZ+1],	/* in: help file spec	*/
    struct SFILE	*fileout,		/* in/out: compilation output file*/
    struct CP_HEADER	*header,		/* in/out: compil. output header record*/
    TEXT		name_in[],	/* in:  PDF file name (for help search)	*/
    TEXT		libr_in[]	/* in:  PDF file library (for help search)*/

 );
FUNCTION static VOID init_header 
(
    struct CONTXT	*ctx,		/* in:  compilation proc context	*/
    struct CP_HEADER	*header,	/* out: skel compiled PDF header	*/
    FUNINT		body_line	/* in:  line # of BODY in uncompiled PDF*/

 );
FUNCTION static VOID make_intro 
(
    struct CONTXT	*ctx,		/* in:  compilation proc context	*/
    TEXT		string[]	/* out: intro record string		*/

 );
FUNCTION static CODE move_body 
(
    struct SFILE	*filein,		/* in/out: compil. source file	*/
    struct SFILE	*fileout,		/* in/out: compil. output file	*/
    FUNINT		local_help,		/* in:  TRUE if help text is in source*/
    struct CP_HEADER	*header		/* in/out: compil. output header record*/

 );
FUNCTION static CODE out_comp_pbk 
(
    struct SFILE	*file,		/* in/out: compiled PDF file		*/
    FUNINT		msgtyp,		/* in:  PARBLK message type		*/
    struct SYMTAB	*st,		/* in:  symbol table to build from	*/
    struct POSCTX	*first_pos	/* out: file posit of 1st record in grp	*/

 );
FUNCTION static CODE out_comp_sub 
(
    struct SFILE	*file,		/* in/out: compilation output file	*/
    struct SUBCMD	*s1,		/* in:  first SUBCMD in chain		*/
    FUNINT		mode		/* in:  CP_WRITE or CP_REWRITE		*/

 );
FUNCTION static VOID pos_directive 
(
    struct SFILE	*file,		/* in:  file context (has position)	*/
    TEXT		string[],	/* in:  last record that was read	*/
    struct CP_HEADER	*header	/* in/out: comp'd proc header record	*/

 );
FUNCTION static CODE var_add 
(
    struct CONTXT	*ctx,		/* in/out: context to add to		*/
    struct VARIABLE	*var		/* in:  variable to add			*/

 );



/*
 *	bld_n_var - build a name parameter given a proc context built so far
 *	and a variable read from a compiled proc.
 */

FUNCTION static CODE bld_n_var 
(
    struct VARIABLE	*vin,		/* in:  name parm read from compiled	*/
    struct VARIABLE	*vout,		/* out: built var w/ ptrs to ref'd var	*/
    struct CONTXT	*ctx		/* in:  proc context built so far	*/

 )
    {
    s_copy ((*vin).v_name, (*vout).v_name);
    (*vout).v_type  = V_NAME;
    (*vout).v_class = V_PARM;
    if ((*vin).v_nref == NULL)		/* if no variable named yet		*/
	(*vout).v_ref = (*vout).v_dref = NULL;
    else				/* otherwise point to ref'd var		*/
	(*vout).v_ref = (*vout).v_dref = search((*vin).v_nref, ctx);
    return(SUCCESS);
    }

/*
 *	chk_directive - returns TRUE if this is a help directive.
 */

FUNCTION static BOOL chk_directive 
(
    TEXT		string[]	/* in:  string to check for directive	*/

 )
    {
    if ((s_lseq(".TITLE", string))  ||
	(s_lseq(".HELP", string))   ||
	(s_lseq(".LEVEL1", string)) ||
	(s_lseq(".LEVEL2", string)))
	return(TRUE);
    return(FALSE);
    }

/*
 *	compilation - do the guts of PDF compilation.
 *
 *	Assumes the input and output files have been opened.
 */

FUNCTION static VOID compilation 
(				 

    struct SFILE	*filein,	/* in/out: file to compile	*/
    struct SFILE	*fileout,	/* in/out: compiled output file	*/
    TEXT		name_in[],	/* in:  PDF file name (for help search
						and internal proc search) */
    TEXT		libr_in[],	/* in:  PDF file library (for help search
						and internal proc search) */
    TEXT		type_in[],	/* in:	PDF file type (for internal proc
						search) */
    TEXT		attr_in[]	/* in:	PDF file attribute (for internal
						proc search) */

    )
    {
    IMPORT struct CONTXT *curproc;	/* current proc context		*/

    struct CONTXT	*saved_curproc;
    CODE		code;
    TEXT		string[STRINGSIZ+1];
    COUNT		body_line;
    struct CONTXT	ctx_compile;		/* context during compilation*/
    struct COMP_HOUSEKEEP housekeep;		/* local copy of housekeeping info*/
    struct CP_HEADER	header;			/* local copy of file header	*/
    struct SUBCMD	*s;
    struct POSCTX	pos;
    BOOL		local_help;		/* TRUE if help text is in PDF	*/

/* initialize the compilation context & local tables	*/

    saved_curproc = curproc;
    curproc = &ctx_compile;
    ctx_compile.prclevel = 2;			/* arbitrary -- so inictx...	*/
    code = inictx(&ctx_compile);		/* won't add locals		*/
    if (code != SUCCESS)
	return;
    s_copy(name_in, ctx_compile.pdf.name);
    s_copy(libr_in, ctx_compile.pdf.libr);
    s_copy(type_in, ctx_compile.pdf.type);
    s_copy(attr_in, ctx_compile.pdf.attr);
    ctx_compile.intrinsic = FALSE;
    ctx_compile.special   = COMPILING;
    ctx_compile.comp      = &housekeep;
    housekeep.header      = &header;
    housekeep.before.link = NULL;
    housekeep.after.link = NULL;
    housekeep.cursub = NULL;

/* compile the pre-body part of the input file	*/

    code = pdftab(&ctx_compile, filein);	/* build symbol tabs		*/
    if (code != SUCCESS)
	goto pdf_err;
    local_help = s_equal(ctx_compile.help_spec, "*");
    body_line = ctx_compile.pdf_line;		/* BODY line # in uncompiled PDF*/

    make_intro(&ctx_compile, string);		/* build intro record string	*/
    code = f_write(fileout, string);		/* intro record to compiled file*/
    if (code != SUCCESS)
	goto write_err;

    init_header(&ctx_compile, &header, body_line);	/* initialize header rec*/
    code = f_bwrite(fileout, 			/* skeleton header rec to compiled*/
		(GENPTR)&header, sizeof(struct CP_HEADER));
    if (code != SUCCESS)
	goto write_err;
    MOVE_STRUCT((*fileout).posctx, housekeep.pos_header);	/* save header pos*/

    code = out_comp_sub(fileout, ctx_compile.subptr,
		CP_WRITE);				/* write skeleton CP_SUBCMDs	*/
    if (code != SUCCESS)
	goto write_err;

    code = out_comp_pbk(fileout, C_BEFORE, 		/* build & write...	*/
		&housekeep.before, &pos);		/* "before" PARBLK(s)	*/
    if (code != SUCCESS)
	goto write_err;
    MOVE_STRUCT(pos, header.before);			/* save "before" pos	*/

    for (s = ctx_compile.subptr; s != NULL; s = (*s).link)	/* for each subc*/
	{
	code = out_comp_pbk(fileout, C_SUBPAR,		/* build & write...	*/
	    &(*s).symtab, &pos);			/* this subc's PARBLK(s)*/
	if (code != SUCCESS)
	    goto write_err;
	MOVE_STRUCT(pos, (*s).position);		/* save PARBLK pos	*/
	}

    code = out_comp_pbk(fileout, C_AFTER, 		/* build & write...	*/
	&housekeep.after, &pos);			/* "after" PARBLK(s)	*/
    if (code != SUCCESS)
	goto write_err;
    MOVE_STRUCT(pos, header.after);			/* save "after" pos	*/

/* now the body and help info	*/

    code = f_write(fileout, "BODY");		/* BODY record to compiled file*/
    if (code != SUCCESS)
	goto write_err;
    MOVE_STRUCT((*fileout).posctx, header.body);/* save BODY pos		*/
    code = move_body(filein, fileout, 		/* move rest of proc...		*/
			local_help, &header);	/* (& help text if in PDF)	*/
    if (code != SUCCESS)
	goto move_body_err;
    if (!local_help)				/* if help is in external file	*/
	{
	code = get_help(ctx_compile.help_spec,
		fileout, &header, name_in, libr_in);  	/* get help text	*/
	if (code != SUCCESS)
	    goto get_help_err;
	}

/* Now re-write the header with the pointers filled in.
 * Note that for UNIX systems we do not read the record before 
 * writing as read and write to the same stream does not work properly.
 * So we position ourselves to the right record and rewrite the record
 * (i.e. under UNIX f_brewrite is the same as f_bwrite).
 */

    f_setpos(fileout, &housekeep.pos_header);
#ifdef  VAX_VMS
    code = f_bread(fileout, (GENPTR)&tmp_header, sizeof(struct CP_HEADER),
			&recsize);		/* dummy read to lock position	*/
    if (code != SUCCESS)
	goto reread_err;
    if (recsize != sizeof(struct CP_HEADER))
	{
	code = F_BTOOSMALL;			/* another wrong rec possibility*/
	goto reread_err;
	}
#endif
    code = f_brewrite(fileout, 	(GENPTR)&header, 
		sizeof(struct CP_HEADER));	/* CP_HEADER record to compiled file*/ 
    if (code != SUCCESS)
	goto write_err;

/* Finally, re-write the CP_SUBCMD records with the pointers filled in.	*/

    code = out_comp_sub(fileout, ctx_compile.subptr,
		CP_REWRITE);			/* write filled in CP_SUBCMDs	*/
    if (code != SUCCESS)
	goto write_err;

    clsctx(&ctx_compile);
    curproc = saved_curproc;
    return;


    if (code == F_EOF)
	tmmsg(PROCFAIL, "Early EOF in re-reading compilation output file.",
	      "TAE-RDEOF", 0, 0, 0, 0, 0);
    else if (code == F_BTOOSMALL)
	tmmsg(PROCFAIL, "Wrong record in re-read of compilation output file.",
	      "TAE-WRGREC", 0, 0, 0, 0, 0);
    else
	tmmsg(PROCFAIL, "Unable to re-read compilation output file. %s.",
	      "TAE-RDERR", (uintptr_t) (*fileout).errmsg, 0, 0, 0, 0);
    goto close_err;

write_err:
    tmmsg(PROCFAIL, "Unable to write compilation output file. %s.",
	  "TAE-WRTERR", (uintptr_t) (*fileout).errmsg, 0, 0, 0, 0);
    goto close_err;

pdf_err:
move_body_err:
get_help_err:
close_err:
    clsctx(&ctx_compile);
    curproc = saved_curproc;
    return;
    }

/*
 *	compile_do - Compile the specified proc.
 *
 *	The default input file type is "PDF".
 *	The default output file type is "CPD".
 */

FUNCTION CODE compile_do 
(
    struct  CONTXT	*cpctx,		/* current proc context 	  */
    struct  CONTXT	*npctx		/* 'compile' command line context */

 )
    {
    struct VARIABLE	*vin, *vout;
    struct SFILE	fileout;
    IMPORT struct SFILE prcfil;
    CODE		code;
    TEXT		fspecout[FSPECSIZ+1];
    TEXT		name_in[FNAMESIZ+1];
    TEXT		libr_in[FLIBRSIZ+1];
    TEXT		type_in[FTYPESIZ+1];
    TEXT		attr_in[FATTRSIZ+1];

    if ((*npctx).prclevel > 1)		/* if not called from secondary level */
	clssav (cpctx);			/* close calling proc and save pos. */

    vin  = lookex(&(*npctx).parmst, "INPROC");
    vout = lookex(&(*npctx).parmst, "OUTPROC");
    code = f_opnspc(&prcfil, PDFLUN, SVAL(*vin, 0), "", "",
		    PDF_TYPE, F_READ);		/* open PDF for read	*/
    if (code != SUCCESS)
	goto openin_err;			/* report error		*/
    f_name(prcfil.full_spec, name_in);		/* extract input PDF file name	*/
    f_libr(prcfil.full_spec, libr_in);		/* extract input PDF file library*/
    f_type(prcfil.full_spec, type_in);		/* extract input PDF file type */
    f_attr(prcfil.full_spec, attr_in);		/* extract input PDF file attribute*/
    if ((*vout).v_count == 0)			/* if output file not specd*/
	s_copy(name_in, fspecout);		/* use input file name	*/
    else
	s_copy(SVAL(*vout, 0), fspecout);	/* use specified output file	*/
    code = f_opnspc(&fileout, CPDLUN, fspecout, "", "",
		    CPD_TYPE, F_WRITE);		/* open compile output	*/
    if (code != SUCCESS)
	goto openout_err;			/* report error		*/

    compilation(&prcfil, &fileout, name_in, libr_in,
		type_in, attr_in);	/* do the compilation	*/

    f_close(&prcfil, F_KEEP);
    f_close(&fileout, F_KEEP);
    if ((*npctx).prclevel > 1)		/* if not called from secondary level */
       opnsav (cpctx);			/* reopen and reposition proc */
    return(DO_CHECK);

openin_err:
    tmmsg(PROCFAIL, "Unable to open file to be compiled. %s.",
	  "TAE-OPNRD", (uintptr_t) prcfil.errmsg, 0, 0, 0, 0);
    if ((*npctx).prclevel > 1)		/* if not called from secondary level */
       opnsav (cpctx);			/* reopen and reposition proc */
    return (DO_CHECK);

openout_err:
    tmmsg(PROCFAIL, "Unable to open compilation output file. %s.",
	  "TAE-OPNWRT", (uintptr_t) fileout.errmsg, 0, 0, 0, 0);
    f_close(&prcfil, F_KEEP);
    if ((*npctx).prclevel > 1)		/* if not called from secondary level */
       opnsav (cpctx);			/* reopen and reposition proc */
    return (DO_CHECK);
    }

/*
 *	get_help - open external help file & move text to compilation
 *	output file.
 *	Since help text is not required, function only fails on a read error,
 *	or a write error, not on a help file open failure.
 */

FUNCTION static CODE get_help 
(
    TEXT		help_spec[FSPECSIZ+1],	/* in: help file spec	*/
    struct SFILE	*fileout,		/* in/out: compilation output file*/
    struct CP_HEADER	*header,		/* in/out: compil. output header record*/
    TEXT		name_in[],	/* in:  PDF file name (for help search)	*/
    TEXT		libr_in[]	/* in:  PDF file library (for help search)*/

 )
    {
    CODE		code;
    struct SFILE	help_file;
    TEXT		string[STRINGSIZ+1];

    if (NULLSTR(help_spec))
	code = f_opnspc(&help_file, HELPLUN, "", libr_in, name_in,
			HLP_TYPE, F_READ);	/* open help w/ PDF name, library*/
    else
	code = f_opnspc(&help_file, HELPLUN, help_spec, "", "",
			HLP_TYPE, F_READ);	/* open specified help file	*/
    if (code != SUCCESS)
	return(SUCCESS);
    while ((code = f_read(&help_file, string)) != F_EOF) /* until EOF		*/
	{
	if (code != SUCCESS)
	    goto read_err;
	code = f_write(fileout, string);	/* pass the input record thru*/
	if (code != SUCCESS)
	    goto write_err;
	pos_directive(fileout, string, header);	/* save position in header...	*/
						/* if this is help directive	*/
	}
    f_close (&help_file, F_KEEP);		/* close the help file		*/
    return(SUCCESS);

read_err:
    tmmsg(PROCFAIL, "Unable to read external help file. %s.",
	  "TAE-RDERR", (uintptr_t) help_file.errmsg, 0, 0, 0, 0);
    goto error_ret;

write_err:
    tmmsg(PROCFAIL,"Unable to write help text to compilation output file. %s.",
	  "TAE-WRTERR", (uintptr_t) (*fileout).errmsg, 0, 0, 0, 0);
    goto error_ret;

error_ret:
    f_close (&help_file, F_KEEP);		/* close the help file		*/
    return(FAIL);
    }

/*
 *	init_header - build skeleton compiled PDF header record.
 */

FUNCTION static VOID init_header 
(
    struct CONTXT	*ctx,		/* in:  compilation proc context	*/
    struct CP_HEADER	*header,	/* out: skel compiled PDF header	*/
    FUNINT		body_line	/* in:  line # of BODY in uncompiled PDF*/

 )
    {
    IMPORT TEXT		vrsion[];
    TEXT		time_buf[STRINGSIZ+1];

    zero_block ((GENPTR)header, sizeof (*header));
    (*header).type       = C_HEADER;
    s_bcopy(vrsion, (*header).version, CP_TIMESIZ);	/* TM executable version*/
    get_time(time_buf);
    s_bcopy(time_buf, (*header).timedate, CP_TIMESIZ);	/* save time of compil	*/
    (*header).body_line = body_line;
    s_copy((*ctx).exe_spec, (*header).execute);		/* EXECUTE= file spec	*/
    (*header).interrupt = (*ctx).interrupt;
    (*header).body.possav       =			/* no file posits known	*/
	(*header).title.possav  =
	(*header).help.possav   =
	(*header).level1.possav =
	(*header).level2.possav =
	(*header).before.possav =
	(*header).after.possav  = FALSE;
    return;
    }

/*
 *	in_comp_pbk - input variables from PARBLK records in a compiled PDF.
 *
 *	Assumes the compiled PDF is open and positioned to the first PARBLK
 *	record to be read.
 *	The variables encountered are added to the context block symbol tables.
 *	(In the case of global variables, reference pointers are added to the
 *	context, pointing to the actual globals in TM's global symbol table;
 *	this is analogous to REFGBL processing in an uncompiled PDF.
 *
 *	This function assumes that all variables in the PARBLK are to be
 *	added to the context (rather than updating the context).
 */

FUNCTION static CODE in_comp_pbk 
(
    struct SFILE	*file,		/* in/out: compiled PDF			*/
    struct CONTXT	*ctx,		/* in/out: context to receive vars	*/
    FUNINT		req_type	/* in: C_BEFORE or C_AFTER		*/

 )
    {

    struct LARGE_PARBLK	p;		/* PARBLK buffer for file reads		*/
    struct VARIABLE	*v; 
    CODE		code;
    COUNT		recsize;

    while (FOREVER)
	{
	code = f_bread(file, (GENPTR)&p, sizeof(p), &recsize);
	if (code == F_EOF) {
	    if (req_type == PAR_FILE)	/* many PAR files don't have p.last */
		break;
	    else
	        goto eof_err;
	}
	if (code != SUCCESS)
	    goto read_err;
	if (p.msgtyp != req_type)
	    goto type_err;
	makeabs(&p.symtab, p.pool);	/* make pointers absolute		*/
	if (chk_parblk((struct PARBLK*) &p) != SUCCESS)	/* check internal integrity		*/
	    goto format_err;
	for (v = p.symtab.link; v != NULL; v = (*v).v_link)
	    {
	    if ((*v).v_file)		/* must verify file vars at run-time */
		{
		code = chk_vector (v, (*v).v_type, (*v).v_cvp,
				   (*v).v_count, FALSE);
		if (code != SUCCESS)
		    goto access_err;
		}
	    code = var_add(ctx, v);	/* add variable to context		*/
	    if (code != SUCCESS)
		goto close_err;
	    }
	if (p.last)			/* if this is the last PARBLK in group	*/
	    break;
	}
    return(SUCCESS);

eof_err:
    tmmsg(PROCFAIL, "Early EOF in compiled PDF.", "TAE-EOFERR", 0, 0, 0, 0, 0);
    return(FAIL);

read_err:
    tmmsg(PROCFAIL, "Unable to read compiled PDF.  %s.",
	  "TAE-RDERR", (uintptr_t) (*file).errmsg, 0, 0, 0, 0);
    return(FAIL);

type_err:
format_err:
    tmmsg(PROCFAIL, "Compiled PDF is malformed.", "TAE-CPDFMALF", 0, 0, 0, 0, 
	  0);
    return(FAIL);

access_err:
    tmmsg(PROCFAIL, "Invalid DEFAULT or INITIAL specification for '%s'.",
	  "TAE-DFTERR", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    return(FAIL);

close_err:
    return(FAIL);

    }

/*
 *	in_comp_sub - find chosen subcommand in compiled PDF.
 *	Builds a SUBCMD struct for the chosen subcommand.
 *	Returns empty string in sub.name if FAIL or if no subcommand was
 *	specified and none exists in the compiled PDF.
 *
 *	NOTE:  Assumes file is open and positioned such that the next
 *		sequential read will be of the first C_SUBCMD record.
 */

FUNCTION static CODE in_comp_sub 
(
    struct SFILE	*file,		/* in/out: compiled PDF			*/
    struct CONTXT	*ctx,		/* in/out: proc context			*/
    struct SUBCMD	*sub		/* out:  SUBCMD for chosen subc		*/

 )
    {
    struct SUBCMD	*s;		/* current entry			*/
    struct SUBCMD	*found_s = 0;
    struct CP_SUBCMD	sub_rec;
    COUNT		recsize;
    CODE		code;
    BOOL		found;
    struct VARIABLE	*v;

    (*sub).name[0] = EOS;		/* until we find chosen subcommand	*/

/* First build the subcommand chain	*/

    while (FOREVER)			/* build SUBCMD chain			*/
	{
	code = f_bread(file, (GENPTR)&sub_rec,
	    sizeof(struct CP_SUBCMD), &recsize);  /* read next CP_SUBCMD	*/
	if (code != SUCCESS)
	    goto read_err;
	if (sub_rec.type != C_SUBCMD)
	    goto type_err;
	if (NULLSTR(sub_rec.subcmd))	/* if end of CP_SUBCMDs sentinal	*/
	    break;
	s = allsub(&(*ctx).subptr);	/* new SUBCMD struct into table		*/
	s_copy(sub_rec.subcmd, (*s).name);
	(*s).deflt = sub_rec.defalt;
	MOVE_STRUCT(sub_rec.pos_parblk, (*s).position);
	}

/* Now find the chosen subcommand	*/

    if ((*ctx).subptr == NULL  &&  NULLSTR((*ctx).subcmd))
	return(SUCCESS);
    found = FALSE;
    for (s = (*ctx).subptr; s != NULL; s = (*s).link)	/* look thru chain	*/
	{
	(*ctx).subs = TRUE;			/* the proc has subcommands	*/
	if (NULLSTR((*ctx).subcmd))		/* if no subc specified...	*/
	    {
	    if ((*s).deflt)			/* use the default		*/
		{
		found = TRUE;
		found_s = s;
		break;
		}
	    }
	else if (s_lseq((*ctx).subcmd, (*s).name)) /* if cmd line has substring match*/
	    {
	    if (found)				/* if 2nd match			*/
		goto ambig_err;
	    found = TRUE;
	    found_s = s;
	    }
	}
    if (!found)					/* if chosen subc not found...*/
	return(SUCCESS);			/* let pdftab figure it out	*/

    MOVE_STRUCT(*found_s, *sub);		/* move SUBCMD struct to caller	*/
    (*ctx).subfnd = TRUE;
    s_copy((*found_s).name, (*ctx).subcmd);	/* move full name to context	*/
    v = lookex(&(*ctx).locst, "_SUBCMD");
    set_string(v, (*found_s).name);
    return(SUCCESS);

read_err:
    tmmsg(PROCFAIL, "Unable to read compiled PDF.  %s.",
	  "TAE-RDERR", (uintptr_t) (*file).errmsg, 0, 0, 0, 0);
    return(FAIL);

type_err:
    tmmsg(PROCFAIL, "Compiled PDF is malformed.", "TAE-CPDFMALF", 0, 0, 0, 
	  0, 0);
    return(FAIL);

ambig_err:
    tmmsg(SUCCESS,
	"Subcommand '%s' specified twice or '%s' ambiguous.",
	  "TAE-SUBTWC", (uintptr_t) (*s).name, (uintptr_t) (*ctx).subcmd,
	  0, 0, 0);
    (*ctx).special = SUB_SEARCH;	/* flag to pdftab -- emulates subcmd_do	*/
    return(SUCCESS);
    }

/*
 *	make_intro - Construct compiled PDF intro record.
 */

FUNCTION static VOID make_intro 
(
    struct CONTXT	*ctx,		/* in:  compilation proc context	*/
    TEXT		string[]	/* out: intro record string		*/

 )
    {
    if ((*ctx).proctype == Y_PROCESS)
	s_copy("PROCESS", string);
    else if ((*ctx).proctype == Y_PROCEDURE)
	s_copy("PROCEDURE", string);
    else if ((*ctx).proctype == Y_PARMSET)
	s_copy("PARMSET", string);
    else if ((*ctx).proctype == Y_GLOBAL)
	s_copy("GLOBAL", string);
    s_append("-COMPILED", string);
    return;
    }

/*
 *	move_body - move body of proc (& help text if in PDF) to compilation
 *			output file.
 *	Assumes the files are positioned correctly such that the
 *	text move can proceed sequentially.
 */

FUNCTION static CODE move_body 
(
    struct SFILE	*filein,		/* in/out: compil. source file	*/
    struct SFILE	*fileout,		/* in/out: compil. output file	*/
    FUNINT		local_help,		/* in:  TRUE if help text is in source*/
    struct CP_HEADER	*header		/* in/out: compil. output header record*/

 )
    {
    CODE		code;
    TEXT		string[STRINGSIZ+1];

    while ((code = f_read(filein, string)) != F_EOF)	/* until EOF		*/
	{
	if (code != SUCCESS)
	    goto read_err;
	if (!local_help)			/* if help text not expected	*/
	    if (chk_directive(string))		/* & if this is a help directive*/
		goto unexpect_dir_err;
	code = f_write(fileout, string);	/* pass the input record thru*/
	if (code != SUCCESS)
	    goto write_err;
	if (local_help)				/* if help text expected in source PDF*/
	    pos_directive(fileout, string, header); /* save position in header...	*/
						/* if this is help directive	*/
	}
    return(SUCCESS);

read_err:
    tmmsg(PROCFAIL, "Unable to read file to be compiled. %s.",
	  "TAE-RDERR", (uintptr_t) (*filein).errmsg, 0, 0, 0, 0);
    return(FAIL);

write_err:
    tmmsg(PROCFAIL, "Unable to write compilation output file. %s.",
	  "TAE-WRTERR", (uintptr_t) (*fileout).errmsg, 0, 0, 0, 0);
    return(FAIL);

unexpect_dir_err:
    tmmsg(PROCFAIL, "Unexpected help text encountered in compilation source.",
	  "TAE-UNEXPECTH", 0, 0, 0, 0, 0);
    return(FAIL);
    }

/*
 *	out_comp_pbk - build PARBLK record group from a symbol table
 *	and output to the compiled PDF file.
 */

FUNCTION static CODE out_comp_pbk 
(
    struct SFILE	*file,		/* in/out: compiled PDF file		*/
    FUNINT		msgtyp,		/* in:  PARBLK message type		*/
    struct SYMTAB	*st,		/* in:  symbol table to build from	*/
    struct POSCTX	*first_pos	/* out: file posit of 1st record in grp	*/

 )
    {
    struct LARGE_PARBLK	p;
    CODE		code;
    struct VARIABLE	*v;
    COUNT		num_var;	/* sent so far				*/
    struct VARIABLE	*first_p_var;	/* first var in "p"			*/

    (*first_pos).possav = FALSE;
    zero_block ((GENPTR) &p, sizeof (p));	/* get all spare fields zero */
    p.msgtyp = msgtyp;
    r_init(p.pool, sizeof p.pool);	/* init PARBLK storage area		*/
    p.symtab.link = NULL;		/* in case no variables			*/
    p.last        = FALSE;
    num_var       = 0;

/* Pack all the variables in each 'symtab' into as many parblks as necessary.  */

    for (v = (*st).link; v != NULL; v = (*v).v_link)
	{
	num_var++;
	code = pblk_out(file, (struct PARBLK*) &p,
			sizeof(p.pool), v);    /* add variable     */
	if (code != SUCCESS)
	    return(code);
	first_p_var = p.symtab.link;
	if (num_var > 1  &&
	    (*first_p_var).v_link == NULL  &&	/* if we just wrote to disk...*/
	    !(*first_pos).possav)		/* for the first time	*/
	    MOVE_STRUCT((*file).posctx, *first_pos);	/* save pos of 1st rec	*/
	}
    if (p.symtab.link != NULL)			/* write last block 	*/
    	{
	makerel(&p.symtab, p.pool);		/* make ptrs relative	*/
	p.last = TRUE;
	p.blksiz = r_top(p.pool) - (GENPTR) &p;
	code = f_bwrite(file, (GENPTR) &p, p.blksiz);
	if (code != SUCCESS)
    	    return (code);
	if (!(*first_pos).possav)			/* if was 1st rec...	*/
	    MOVE_STRUCT((*file).posctx, *first_pos);	/* save its position	*/
    	}
    return(SUCCESS);
    }

/*
 *	out_comp_sub - Build a CP_SUBCMD record for each SUBCMD struct
 *	in the SUBCMD chain.  Write each CP_SUBCMD record to the compilation
 *	output file.
 *	If the mode is CP_REWRITE the CP_SUBCMD records are written over
 *	existing CP_SUBCMD records (i.e., the compilation second pass).
 *	The file is assumed to be positioned such that the next sequential
 *	read/write operation is to the first CP_SUBCMD record position.
 */

FUNCTION static CODE out_comp_sub 
(
    struct SFILE	*file,		/* in/out: compilation output file	*/
    struct SUBCMD	*s1,		/* in:  first SUBCMD in chain		*/
    FUNINT		mode		/* in:  CP_WRITE or CP_REWRITE		*/

 )
    {
    struct CP_SUBCMD	out_rec;
    struct SUBCMD	*s;
    CODE		code;

    for (s = s1; s != NULL; s = (*s).link)	/* for each SUBCMD in chain	*/
	{
	  zero_block ((GENPTR) &out_rec, sizeof (out_rec));
	out_rec.type = C_SUBCMD;		/* record type			*/
	s_copy((*s).name, out_rec.subcmd);	/* subcommand name		*/
	out_rec.defalt = (*s).deflt;		/* default subcommand?		*/
	MOVE_STRUCT((*s).position, out_rec.pos_parblk);	/* PARBLK file position	*/
	if (mode == CP_WRITE)			/* if this is the first write	*/
	    {
	    code = f_bwrite(file, 		/* CP_SUBCMD record to compiled file*/
			(GENPTR)&out_rec, sizeof(struct CP_SUBCMD));
	    if (code != SUCCESS)
		goto write_err;
	    }
	else					/* CP_REWRITE			*/
	    {
#ifdef VAX_VMS
	    code = f_bread(file, (GENPTR)&tmp_rec,
		sizeof(struct CP_SUBCMD), &recsize);	/* dummy read to lock position	*/
	    if (code != SUCCESS)
		goto reread_err;
	    if (recsize != sizeof(struct CP_SUBCMD))
		{
		code = F_BTOOSMALL;		/* another wrong rec possibility*/
		goto reread_err;
		}
#endif
	    code = f_brewrite(file, (GENPTR)&out_rec, 
			sizeof(struct CP_SUBCMD)); /* CP_HEADER record to compiled file*/ 
	    if (code != SUCCESS) goto write_err; 
	    } 
	} 
    if (mode == CP_WRITE)		/* if this is the first write		*/
	{
	  zero_block ((GENPTR) &out_rec, sizeof (out_rec));
	out_rec.type = C_SUBCMD;
	out_rec.subcmd[0] = EOS;	/* build & write termination...		*/
	code = f_bwrite(file, 		/* CP_SUBCMD record to compiled file	*/
		    (GENPTR)&out_rec, sizeof(struct CP_SUBCMD));
	if (code != SUCCESS)
	    goto write_err;
	}
    return(SUCCESS);

    if (code == F_EOF)
	tmmsg(PROCFAIL, "Early EOF in re-reading compilation output file.",
	      "TAE-RDEOF", 0, 0, 0, 0, 0);
    else if (code == F_BTOOSMALL)
	tmmsg(PROCFAIL, "Wrong record in re-read of compilation output file.",
	      "TAE-WRGREC", 0, 0, 0, 0, 0);
    else
	tmmsg(PROCFAIL, "Unable to re-read compilation output file. %s.",
	      "TAE-RDERR", (uintptr_t) (*file).errmsg, 0, 0, 0, 0);
    return(FAIL);

write_err:
    tmmsg(PROCFAIL, "Unable to write compilation output file. %s.",
	  "TAE-WRTERR", (uintptr_t) (*file).errmsg, 0, 0, 0, 0);
    return(FAIL);
    }

/*
 *	pos_directive - save file position in compiled PDF header
 *	record if the last record read was a help directive.
 */

FUNCTION static VOID pos_directive 
(
    struct SFILE	*file,		/* in:  file context (has position)	*/
    TEXT		string[],	/* in:  last record that was read	*/
    struct CP_HEADER	*header	/* in/out: comp'd proc header record	*/

 )
    {
    if (s_lseq(".TITLE", string))
	MOVE_STRUCT((*file).posctx, (*header).title);
    else if (s_lseq(".HELP", string))
	MOVE_STRUCT((*file).posctx, (*header).help);
    else if (s_lseq(".LEVEL1", string))
	MOVE_STRUCT((*file).posctx, (*header).level1);
    else if (s_lseq(".LEVEL2", string))
	MOVE_STRUCT((*file).posctx, (*header).level2);
    return;
    }

/*
 *	prc_compiled.  Build symbol tables from a compiled PDF.
 *
 *	Assumes that the compiled PDF is open and positioned such that
 *	the next sequential read will be of the C_HEADER record.
 */

FUNCTION CODE prc_compiled 
(
    struct SFILE	*file,		/* in/out: compiled PDF control block	*/
    struct CONTXT	*ctx		/* in/out: context for compiled PDF	*/

 )
    {
    struct CP_HEADER	header;		/* buffer for compiled files header rec	*/
    CODE		code;
    COUNT		recsize;
    struct SUBCMD	sub;		/* the chosen subcommand		*/

/* First read & process the header record */

    code = f_bread(file, (GENPTR)&header, sizeof(struct CP_HEADER), &recsize);
    if (code != SUCCESS)
	goto read_err;
    if (header.type != C_HEADER)
	goto hdr_type_err;
    s_bcopy(header.execute, (*ctx).exe_spec, FSPECSIZ);
    (*ctx).interrupt = header.interrupt;
    (*ctx).body_line = header.body_line;	/* use line #...*/
    (*ctx).pdf_line  = header.body_line - 1;	/* from uncompiled PDF	*/

/* Next read CP_SUBCMD records, pick the chosen subcmd, & build SUBCMD struct*/

    code = in_comp_sub(file, ctx, &sub);	/* find correct subcmd		*/
    if (code != SUCCESS)
	goto in_sub_err;

/* Now the PARBLKs	*/

    if (header.before.possav)			/* if "before" PARBLK exists	*/
	{
	f_setpos(file, &header.before);		/* pos to 1st "before" PARBLK	*/
	code = in_comp_pbk(file, ctx, C_BEFORE);/* read the before PARBLKs	*/
	if (code != SUCCESS)
	    goto in_pbk_err;
	}

    if (!NULLSTR(sub.name)  &&  sub.position.possav)	/* if there's a subc...*/
	{					/* & associated PARBLK		*/
	f_setpos(file, &sub.position);		/* pos to 1st subc PARBLK	*/
	code = in_comp_pbk(file, ctx, C_SUBPAR);/* read the subc PARBLKs	*/
	if (code != SUCCESS)
	    goto in_pbk_err;
	}

    if (header.after.possav)			/* if "after" PARBLK exists	*/
	{
	f_setpos(file, &header.after);		/* pos to 1st "after" PARBLK	*/
	code = in_comp_pbk(file, ctx, C_AFTER);	/* read the after PARBLKs	*/
	if (code != SUCCESS)
	    goto in_pbk_err;
	}

/* Now position for normal PDF procedure processing.	*/
/* I.e., the next sequential read will be the BODY statement	*/

    f_setpos(file, &header.body);	/* position for proced body (or help)*/
    MOVE_STRUCT(header.body, (*ctx).prcpos);
    if ((*ctx).proctype == Y_PROCESS)
        (*ctx).inbody = TRUE;         /* so passthru will work */
    return(SUCCESS);


read_err:
    tmmsg(PROCFAIL, "Unable to read compiled PDF. %s.",
	  "TAE-RDERR", (uintptr_t) (*file).errmsg, 0, 0, 0, 0);
    return(FAIL);

hdr_type_err:
    tmmsg(PROCFAIL, "Compiled PDF has bad header record.", "TAE-CPDFBHDR",
	  0, 0, 0, 0, 0);
    return(FAIL);

in_sub_err:
in_pbk_err:
    return(FAIL);
    }

/*	prc_par_file.   Build symbol tables from a simple PAR file.
 *
 *	This is used when a PAR file is the "proc".
 */

FUNCTION CODE prc_par_file 
(
    	struct SFILE	*file,		/* opened PAR file	*/
        struct CONTXT	*ctx		/* contxt to build	*/

 )
    {
    struct PARHDR parhdr;		/* PAR file header block*/
    CODE	code;
    TEXT	errmsg[STRINGSIZ+1], errkey[STRINGSIZ+1];
    
    (*ctx).proctype = Y_PARMSET;	/* make it look just like PARMSET */
    code = parhdrRead (file, &parhdr, errmsg, errkey);
    if (code != SUCCESS)
	{
	  tmmsg (PROCFAIL, errmsg, errkey, 0, 0, 0, 0, 0);
	return (FAIL);
	}
    code = in_comp_pbk (file, ctx, PAR_FILE);
    return (code);
    }

/*
 *	prep_c_help - prepare to display help on a compiled PDF.
 *
 *	Assumes file is positioned so next sequential read is of the
 *	header record.
 */

FUNCTION CODE prep_c_help 
(
    struct DIRBLK	*dirblk,	/* in/out: directive block		*/
    struct TXTSTOR	*title,		/* out: title text			*/
    TEXT		msg[],		/* error message (if any)		*/
    TEXT		key[]		/* error key (if any)			*/

 )
    {
    struct SFILE	*file;
    struct CP_HEADER	header;		/* compiled PDF header record		*/
    COUNT		recsize;
    TEXT		string[STRINGSIZ+1];
    CODE		code;

    file = (*dirblk).sfileptr;
    code = f_bread(file, (GENPTR)&header, sizeof(struct CP_HEADER), &recsize);
    if (code != SUCCESS)
	goto read_err;
    if (header.type != C_HEADER)
	goto hdr_type_err;
    if (header.title.possav)		/* if title text exists			*/
	{
	f_setpos(file, &header.title);	/* position to .TITLE			*/
	code = f_read(file, string);	/* read the .TITLE record		*/
	if (code != SUCCESS)
	    goto read_err;
	code = gettitle(dirblk, title, msg, key);
	if (code != SUCCESS)
	    return(FAIL);
	}
    else				/* no title				*/
	(*title).numline = 0;
    f_setpos(file, &header.body);	/* position for rest of help processing	*/
    return(SUCCESS);

read_err:
    s_copy("Error reading the compiled PDF.", msg);
    s_copy("TAE-RDERR", key);
    return(FAIL);

hdr_type_err:
    s_copy("Compiled PDF has bad header record.", msg);
    s_copy("TAE-CPDFBHDR", key);
    return(FAIL);
    }

/*
 *	var_add - Add a variable (from a compiled PARBLK) to a proc context.
 *
 *	If the variable is of type V_GLOBAL, only the reference pointer
 *	is added.
 *
 *	Note that for GLOBAL procs, the PARMs remain parameters until the
 *	proc is executed.
 */

FUNCTION static CODE var_add 
(
    struct CONTXT	*ctx,		/* in/out: context to add to		*/
    struct VARIABLE	*var		/* in:  variable to add			*/

 )
    {
    IMPORT struct SYMTAB glbtab;

    struct VARIABLE	*targvar;
    struct VARIABLE	*gv;
    struct VARIABLE	*save_link;
    struct VARIABLE	*defd_var;
    struct VARIABLE	*refd_var;
    CODE		type, class;
    CODE		code;
    TEXT		*save_deref;

    type  = (*var).v_type;
    class = (*var).v_class;
    if (class == V_GLOBAL)		/* analogous to REFGBL sttmt for 1 var	*/
	{
	defd_var = search((*var).v_name, ctx);
	if (defd_var != NULL)
	    {
	    if ((*defd_var).v_class == V_GLOBAL  &&  (*defd_var).v_implicit)
		return(SUCCESS);	/* if implicit, skip with no EM		*/
	    else
		goto dblref_err;
	    }
	if (lookex(&glbtab, (*var).v_name) == NULL)
	    goto undef_err;
	addref(ctx, (*var).v_name);
	}
    else
	{
	if (class == V_PARM)
	    targvar = allvar(&(*ctx).parmst);	/* alloc new var in ctx parm st	*/
	else
	    targvar = allvar(&(*ctx).locst);	/* alloc new var in ctx loc st	*/
	save_deref = NULL;
	if (type == V_NAME)			/* if NAME parm, point to ref'd var*/
	    {
	    code = bld_n_var(var, targvar, ctx); /* build name parameter	*/
	    if (code != SUCCESS)
		goto badref_err;
	    }
	else
	    {
	    if (class == V_PARM  &&  (*var).v_deref)
		{				/* if deref'd default value	*/
		save_deref = (TEXT *)(*var).v_cvp; /* save deref pointer	*/
		(*var).v_cvp = NULL;		/* so specvcopy isn't fooled	*/
		}
	    save_link = (*targvar).v_link;
	    specvcopy(var, targvar);		/* copy var to ctxt symb tab var*/
	    if (save_deref != NULL)		/* if PARM DEFAULT deref'd	*/
		{
		(*targvar).v_deref = 0;
		refd_var = search(save_deref, ctx); /* search for referenced var	*/
		if (refd_var == NULL)
		    goto pd_undef_err;
		(*targvar).v_cvp = allval (targvar);	/* alloc space for val vector*/
		code = set_value(targvar, (*refd_var).v_cvp, (*refd_var).v_count);
		}
	    (*targvar).v_link = save_link;	/* specvcopy nulled the link	*/
	    }
	if ((*ctx).proctype == Y_GLOBAL && class == V_PARM)
	    {
	    gv = lookex (&glbtab, (*var).v_name);	/* global exists?     */
	    if (gv != NULL)
		{
		code=deep_value_copy (gv, targvar, (*gv).v_name, (*gv).v_name);
		if (code != SUCCESS)
		    goto global_mismatch;		        
		}
	    }
	}
    return(SUCCESS);

dblref_err:
    tmmsg(PROCFAIL, "Variable '%s' previously defined or referenced.",
	  "TAE-REFEX", (uintptr_t) (*var).v_name, 0, 0, 0, 0);
    return(FAIL);

badref_err:
    tmmsg(PROCFAIL, "Reference to undefined variable '%s'.",
	  "TAE-UNDEFVAR", (uintptr_t) (*var).v_name, 0, 0, 0, 0);
    return(FAIL);

undef_err:
    tmmsg(PROCFAIL, "Reference to undefined global variable '%s'.",
	  "TAE-UNDEFGBL", (uintptr_t) (*var).v_name, 0, 0, 0, 0);
    return(FAIL);

pd_undef_err:
    tmmsg(PROCFAIL,
	"PARM DEFAULT references undefined global variable '%s'.",
	  "TAE-UNDEFGBL", (uintptr_t) (*var).v_name, 0, 0, 0, 0);
    return(FAIL);

global_mismatch:
    tmmsg (PROCFAIL, "'%s' is incorrect re-definition of existing global.", 
	   "TAE-GBLREDEF", (uintptr_t) (*var).v_name, 0, 0, 0, 0);
    return (FAIL);
    }
