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
 *	hard_help  - process and write to output file an opened help file
 *
 *	Assumes we're positioned anywhere before .TITLE and that .TITLE 
 *	precedes .HELP.  If help is on a compiled PDF, assumes we're
 *	positioned to read the CP_HEADER record next.
 *
 *	NOTE: (*helpblk).compiled tells us whether this is a compiled PDF.
 *
 *	Returns: SUCCESS  or  FAIL.	
 *
 *	CHANGE LOG:
 *	08-may-89	into Century tree...palm 
 *	08-may-89	modern include file names...palm 
 */

#include	"stdh.inp"		/* standard C definitions		*/
#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"dirinc.inc"		/* directive package			*/
#include	"syninc.inc"		/* syntax package			*/
#include	"helpinc.inc"		/* help output control block		*/
#include	"terminc.inc"		/* terminal package			*/
#include "taeintproto.h"

#define CURRENT_POS &((dirblk.sfileptr)->posctx)
#define CUR_POS     &(((*dirblk).sfileptr)->posctx)


FUNCTION  static  VOID  store_help_error
(    
    struct  HELPBLK	*helpblk,		/* in/out: help output block */
    TEXT		errmsg[],		/* in: error message	     */
    TEXT		errkey[]		/* in: error message key     */
    
     );

CODE put_title
(
    struct SFILE	*f,
    struct TXTSTOR	*title,
    TEXT		msg[],
    TEXT		key[]

 );

CODE put_text_block
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    TEXT		msg[],
    TEXT		key[]
 );

CODE put_header
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    TEXT		type[],		/* ".SUBCMD" or ".VARIABLE"	*/
    TEXT		name[],		/* name of subcmd or variable	*/
    TEXT		msg[],		/* out: error message		*/
    TEXT		key[]		/* out: error key		*/
 );
CODE put_level2_text
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    struct POSCTX	*level2_pos,
    TEXT		type[],
    TEXT		name[],
    TEXT		msg[],
    TEXT		key[]

 );


FUNCTION CODE hard_help
(
    struct   SFILE	*fctx,		/* In: file context block for opened file	*/
    TEXT  		curlib[],	/* In: library name of opened file		*/
    TEXT		procname[],	/* In: proc name for LHS of help display	*/
    TEXT		outname[],	/* In: name of output file			*/
    struct  HELPBLK	*helpblk	/* out: help output control block		*/

 )
{

    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics	*/

    struct DIRBLK	dirblk;		/* directive control block		*/
    struct TXTSTOR	title;		/* context for dynamically stored title text */
    struct SFILE	f;
    struct POSCTX	saved_pos;
    struct POSCTX	level2_pos;
    BOOL		has_level2;
    TEXT		str[STRINGSIZ+1];
    CODE		code;
    TEXT		field[STRINGSIZ+1];	/* text field after a directive	*/
    TEXT		name[STRINGSIZ+1];
    TEXT		key[KEYSIZ+1];
    TEXT		msg[STRINGSIZ+1];

					/* If no output name was given,	*/
    if (NULLSTR(outname))		/* use pdf or command name	*/
    {
	f_name(procname, outname);
	s_append(".MEM", outname);	/* procname.MEM for output	*/
    }

/* note -- SAVELUN is safe because |save| qual is not allowed for this command */
    code = f_ophf(&f, SAVELUN, outname, F_WRITE);	/* open output file	*/
    if (code != SUCCESS)
    {
	sprintf(msg, "Error opening output, host code = %d", f.host_code);
	store_help_error(helpblk, msg, "TAE-OPNWRT");
	return(FAIL);
    }

    d_init(&dirblk, fctx, curlib, (TEXT **)(*char_gbl).v_cvp, 
	(*char_gbl).v_count);    		/* init directive block		*/
    if ((*helpblk).compiled)			/* if PDF is compiled		*/
    {
	if ((code = prep_c_help(&dirblk, &title, msg, key)) != SUCCESS)	/* prepare*/
	    goto put_error;
	str[0] = EOS;
    }
    else					/* PDF is not compiled		*/
    {
	code = d_dirctv(&dirblk, str, field);
	if (s_equal(str, ".TITLE"))
	{				/* read the title into dynamic store			*/
	    if ((code = gettitle(&dirblk, &title, msg, key))!= SUCCESS) 
		goto put_error;
	    code = d_dirctv(&dirblk, str, field);
	}
	else				/* no title */
	{
	    title.numline = 0;
	}
    }

    code = put_title(&f, &title, msg, key);  /* start filling output file */
    if (code != SUCCESS) goto put_error;

    if (title.tp != NULL) fretxt(&title); /* free the dyamically-stored title	*/

/* str should contain .HELP directive now.  If not, find it	*/
    if (! s_equal( str, ".HELP"))
    {
	code = d_search(&dirblk, ".HELP", field);
	if (code != SUCCESS)
	{
	    store_help_error(helpblk, "No help available on '%s'.", "TAE-NOHELP");
	    return (FAIL);
	}
    }
    
    code = put_text_block(&f, &dirblk, msg, key);	/* output general help	*/
    if (code != SUCCESS) goto put_error;

    code = d_search(&dirblk, ".LEVEL1", field);
    if (code != SUCCESS)		/* if there is no level1 help then */
	goto return_success;		/* assume we are done and exit	   */

    f_movpos(CURRENT_POS, &saved_pos);		/* save position in input */
    code = d_search(&dirblk, ".LEVEL2", field); /* find detailed help */
    has_level2 = (code == SUCCESS);
    if (has_level2)				/* if detailed help exists,  */
	f_movpos(CURRENT_POS, &level2_pos);	/* save the position in file */

    f_setpos(dirblk.sfileptr, &saved_pos);	/* reset position to curr pos */
    code = d_dirctv(&dirblk, str, name);	/* gives str=".TITLE"	     */

    while (1)
    {
	code = d_dirctv(&dirblk, str, name);
	if (code != SUCCESS)			/* if no more directives, we */
	    break;				/* are done		     */

	if (s_equal(str, ".END") || s_equal(str, ".LEVEL2")) /* done */
	    break;

	code = put_header(&f, &dirblk, str, name, msg, key);
	if (code != SUCCESS) goto put_error;
	if (has_level2)
	    code = put_level2_text(&f, &dirblk, &level2_pos,
				   str, name, msg, key);
	if (code != SUCCESS) goto put_error;
    }

return_success:
    f_close(&f, F_KEEP);
    s_lower(outname);
    sprintf(msg, "Wrote file \"%s\".", outname);
    put_stdout(msg);
    return (SUCCESS);

put_error:
    store_help_error(helpblk, msg, key);
    return (FAIL);
}

CODE put_level2_text
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    struct POSCTX	*level2_pos,
    TEXT		type[],
    TEXT		name[],
    TEXT		msg[],
    TEXT		key[]

 )
{

    struct POSCTX		saved_pos;
    CODE			code;
    TEXT			next_dirctv[STRINGSIZ+1];
    TEXT			field[STRINGSIZ+1];

/* start looking at level2 help */
    f_movpos(CUR_POS, &saved_pos);
    f_setpos((*dirblk).sfileptr, level2_pos);
    do 
    {
	code = d_dirctv(dirblk, next_dirctv, field);

    } while ((code == SUCCESS)
	   && ! (s_lseq(next_dirctv, type) && s_equal(field, name))
	   && ! s_lseq(next_dirctv, ".END"));
    
    if ((code == SUCCESS) && ! s_lseq(next_dirctv, ".END")) 
    {							/* found level2 help */
	code = put_text_block(f, dirblk, msg, key);
	if (code != SUCCESS)
	    return (code);
    }

    f_setpos((*dirblk).sfileptr, &saved_pos);
    return (SUCCESS);
}

CODE put_text_block
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    TEXT		msg[],
    TEXT		key[]
 )
{
    CODE		code;
    TEXT		str[STRINGSIZ+1];

    while (1)
    {
	code = d_text(dirblk, str);
	while (code == D_PAGE) code = d_text(dirblk, str);

	if (code != SUCCESS) break;

	code = f_write(f, str);
	if (code != SUCCESS) goto write_err;
    }

    if (code == D_EOT)
	return (SUCCESS);

    s_copy("Error reading help file",msg);
    s_copy("TAE-HLPRD", key);
    return (FAIL);

write_err:
    sprintf(msg, "Error writing to output file, host code = %d",
	    (*f).host_code);
    s_copy("TAE-HLPWRT", key);
    return (FAIL);
}

CODE put_title
(
    struct SFILE	*f,
    struct TXTSTOR	*title,
    TEXT		msg[],
    TEXT		key[]

 )
{
    CODE	code;
    COUNT	i;
    TEXT	*txt;
    TEXT	str[STRINGSIZ+1];

    for (i = 0; i < (*title).numline; i++)
    {
	txt = (*title).tp[i];
	sprintf(str, "\t\t%s", txt);
	code = f_write(f, str);
	if (code != SUCCESS) goto wrt_err;
    }

    code = f_write(f, "\n\n\n");
    if (code != SUCCESS) goto wrt_err;

    return (SUCCESS);

wrt_err:
    sprintf(msg, "Error writing to output file, host code = %d",
	    (*f).host_code);
    s_copy("TAE-HLPWRT", key);
    return (FAIL);
}

CODE put_header
(
    struct SFILE	*f,
    struct DIRBLK	*dirblk,
    TEXT		type[],		/* ".SUBCMD" or ".VARIABLE"	*/
    TEXT		name[],		/* name of subcmd or variable	*/
    TEXT		msg[],		/* out: error message		*/
    TEXT		key[]		/* out: error key		*/
 )
{
    TEXT		header[STRINGSIZ+1];
    CODE		code;

    if (s_lseq(type, ".SUBCMD"))
    {
	s_copy("\f\t\tSubcommand ", header);
    }
    else if (s_lseq(type, ".VARIABLE"))
    {
	s_copy("\n\n\n\tParameter ", header);
    }
    else
    {
	s_copy("Bad directive:  Expected .SUBCMD or .VARIABLE", msg);
	s_copy("TAE-BADHLPDIR", key);
	return (FAIL);
    }

    s_append(name, header);
    code = f_write(f, header);
    code = f_write(f, " ");
    if (code != SUCCESS)
	goto error_out;

    code = put_text_block(f, dirblk, msg, key);
    if (code != SUCCESS) return (code);

    code = f_write(f, " ");
    if (code == SUCCESS)
	return (SUCCESS);

error_out:
    sprintf(msg, "Error writing output file, host code = %d", (*f).host_code);
    s_copy("TAE-HLPWRT", key);
    return (FAIL);
}

/*
 *	store_help_error. Generate  error message for help related error.
 */
	
FUNCTION  static  VOID  store_help_error
(    
    struct  HELPBLK	*helpblk,		/* in/out: help output block */
    TEXT		errmsg[],		/* in: error message	     */
    TEXT		errkey[]		/* in: error message key     */
    
     )
    {
    s_copy(errkey, (*helpblk).errkey);		/* put key in block  	*/
    sprintf((*helpblk).errmsg, errmsg, (*helpblk).helpobj);  /* format message */
    return;					/* and put in block  	*/
    }
