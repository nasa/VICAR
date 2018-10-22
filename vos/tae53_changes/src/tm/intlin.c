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
 * This file contains functions to receive and parse interactive command
 * lines
 *
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	24-aug-83	Add escape handling to scripts...peb
 *	05-oct-83	Changed function names, added line_screen()...dm
 *	25-oct-83	Changed highlight to t_highligt...palm
 *	30-oct-83	Fix ")" algorithm in intprm()...peb
 *	24-jan-84	Several new calling sequences and new logic
 *			for Micro-editor...palm
 *	06-mar-84	Call flush_cmd if bldcmd returns FAIL...dm
 *	11-apr-84	New calling sequences to editor for better
 *			unix portability...palm
 *	18-apr-84	New calling sequence to (again) editor...palm
 *	30-apr-84	Mode flag for getscr and cmd_editor calls for
 *			UNIX...dm
 *	27-jun-84	Clear control/c to eliminate its possible affect
 *			on the new command...lia
 *	02-nov-84	Add .pause and .delay to script...palm
 *	05-nov-84	Change handling of <ESC> in script files...jtm
 *	04-apr-85	Change prompt strategy for compatibility with
 *			VMS V4 editor, i.e., the cmd_editor and tut_editor
 *			subroutines must do the prompting....palm
 *	01-aug-86	Modifications for the TAE FACELIFT...krw
 *	10-feb-86	Merged TAE FACELIFT with v1.4 TAE...krw
 *
 ********************** Post TAE 2.1 changes *******************************
 *
 *	19-may-87	Updated for TAE-Plus philosophy. Checked global 
 *			variable DisplayId to determine VT100 or Window 
 *			mode screen operations...dm
 *      12-aug-87       Merge in 2.2 changes...tpl
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl
 *	17-oct-88	Get prompt on PAUSE, DELAY...nhe
 *	01-feb-89	VAX C needs IMPORT BOOL of XFACELIFT...ljn
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	27-jun-90	Remove Facelift code...ljn
 */


#include	"taeconf.inp"
#include	"tmhost.inp"
#include	"terminc.inc"
#include	"syninc.inc"
#include	"tminc.inc"
#include	"eventinc.inp"
#include "taeintproto.h"

FUNCTION  CODE  line_screen 
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD    */
    COUNT	line_num,		/* IN: input line number	     */
    TEXT 	prompt[],		/* IN: prompt string		     */
    TEXT  	line[],			/* OUT: line from interactive source */
    CODE	*term			/* OUT: line terminator 	     */

 );

/* cmd_noscreen - get interactive command string in no-screen mode.
 * Gets full TCL command line, including continuation lines.
 * On return, all continuation lines have been appended to the first
 * line, with the "+" replaced by a blank separator.
 * Comments and trailing blanks are stripped.
 * Uses the caller supplied prompt.
 */

FUNCTION VOID cmd_noscreen
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD*/
    FAST TEXT	pmtbuf[],		/* in:  prompt buffer		*/
    FAST TEXT	cmdstr[]		/* out: command string		*/

 )
    {
    IMPORT struct SFILE  *scr_file;	/* pointer to script file ctx 	*/
    IMPORT struct ECB    ecbi;		/* operator-attn ecb		*/

    CODE	term;			/* line terminator		*/
    CODE	code;
    TEXT	inbuf[STRINGSIZ+1];	/* single line input buffer	*/
    TEXT	locpmt[STRINGSIZ+1];
    COUNT	pl;			/* prompt length		*/
    COUNT	nrec;

    cmdstr[0] = EOS;
    code = SUCCESS;			/* start assuming no continuation */
    do
	{
	pl = s_copy(pmtbuf, locpmt);
	if (code == CONTINUE)		/* if this is a continuation line */
	    s_append("+", locpmt);	/* append a "+"			      */
	code = line_noscreen (archive, locpmt, inbuf, &term);
					/* get next line (from oper or script)*/
	t_attn(&ecbi);			/* clear operator-attn ecb	*/
	if (code == F_FILERR)
	    {
	      m_put("Error reading script file.", "TAE-RDERR",0,0,0,0,0);
	    cmdstr[0] = EOS;
	    return;
	    }
	if (term == T_ESCAPE)
	    {
	    code = SUCCESS;		/* reinitialize			      */
	    cmdstr[0] = EOS;
	    continue;
	    }
	code = bldcmd(inbuf, cmdstr);	/* append cmd to full command string  */
	if (code == FAIL && scr_file != NULL)	/* if from script	*/
	    flush_cmd(scr_file, inbuf, &nrec);	 /* ignore rest of the cmd    */
	}
    while (code == CONTINUE);			/* loop if continuation line  */
    return;
    }

/*
 * cmd_screen - get an interactive command string in screen mode.
 * (This routine is functionally equivalent to cmd_noscreen(), but
 * called when we are in formatted screen mode i.e. within help,
 *  tutor, menu )
 */

FUNCTION  CODE  	cmd_screen 
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD */
    TEXT	cmdstr[],		/* out: command string		  */
    CODE	*terminat		/* command line terminator        */

 )
    {
    IMPORT struct  SFILE	*scr_file;	/* SFILE for the script file  */
    IMPORT struct  ECB		ecbi;		/* operator-attn ecb	      */
    IMPORT COUNT		termlines;	/* number of lines on terminal*/

    TEXT		inbuf[STRINGSIZ+1];	/* single line input buffer */
    TEXT		outbuf[STRINGSIZ+1];
    COUNT		pmtline;		/* display line for "?"	*/
    COUNT		col;
    COUNT		nrec;
    CODE		code;
    CODE		cmdcode;

    pmtline = termlines - PMTLIN;
    cmdstr[0] = EOS;
    cmdcode = SUCCESS;
    do
        {
	col = (cmdcode == CONTINUE) ?
	    s_copy("?+ ", outbuf) : s_copy("? ", outbuf);
	code = line_screen (archive, pmtline, outbuf, inbuf, terminat);
	t_attn(&ecbi);				/* clear operator-attn ecb */
	if (code != SUCCESS  ||  *terminat == T_ESCAPE)
	    break;
	cmdcode = bldcmd(inbuf, cmdstr);	/* app to full cmd str (& strip  comments)*/
	if (code == FAIL && scr_file != NULL)	/* if from script	*/
	    flush_cmd(scr_file, inbuf, &nrec);	 /* ignore rest of the cmd    */
	}
    while (cmdcode == CONTINUE);

    t_lclear(termlines-ERRLIN, 1);

    if (code == F_FILERR)
      wrterr("Error reading script file.", "TAE-RDERR",0,0,0,0,0);
    if (*terminat == T_ESCAPE)
	cmdstr[0] = EOS;		/* ignore the input command */
    return(code);
    }

/* getscr - Get the next line from script file.
 * This function reads a line from the script file. If an error is encountered
 * in reading the file,  interactive source is set to OPERATOR.
 * If the line ends in two consecutive escape characters, they are treated
 * as the indicator to ignore the command. The escape characters are replaced
 * with ascii "<ESC>" sequences to allow the user to see escape chars were
 * present.
 *
 * Return codes:
 *
 *	SUCCESS
 *	F_FILERR if a read or rewind error occurred
 *	F_EOF    if end of the last script file was encountered
 *	F_KEEP   keep going; i.e., the line was .pause or .delay
 */

FUNCTION CODE getscr
(
    FAST TEXT	line[],				/* OUT: record from script file*/
    CODE	*term,				/* OUT: terminator character */
    FUNINT	screen				/* IN: true if screen mode   */

 )
    {
    IMPORT struct SFILE		*scr_file;	/* pointer to script file context */
    IMPORT COUNT 		scpcnt;		/* number of cycles for script file */
    CODE		code;
    TEXT		tmpbuf[5+STRINGSIZ+1];
    COUNT		line_len;		/* Length of script line */

    *term = T_CR;
    while (FOREVER)
	{
	line[0] = EOS;
	if ((code = f_read(scr_file, line)) == F_FILERR)    /* get cmd	*/
	    {
	    scpcnt = 0;
	    scr_file = NULL;
	    return(code);
	    }
	else if (code == F_EOF)			/* if end of script file */
	    {
	    scpcnt--;
	    if (scpcnt == 0)
	        {
		f_close(scr_file, F_KEEP);	/* close script file	   */
		scr_file = NULL;		/* set source to operator */
	    	return(F_EOF);
		}
	    else
		{
		if ((code = f_rewind(scr_file)) != SUCCESS)	/* rewind script*/
		    {
		    scpcnt = 0;
		    scr_file = NULL;
		    return(F_FILERR);
		    }
		continue;
		}
	    }
	break;
	}

    if (scr_file != NULL)			/* a line read from script */
	{
	line_len = s_length (line);		/* get length of the line  */
	if ((line[line_len-1] == CHAR_ESCAPE) &&
	    (line[line_len-2] == CHAR_ESCAPE))	/* line end in two escape chars? */
	    {
	    *term = T_ESCAPE;
	    line[line_len-2] = EOS;		/* discard escape & trailing chars*/
	    }
	s_bcopy(line, tmpbuf, STRINGSIZ);
	if (*term == T_ESCAPE)
	    s_append("<ESC>", tmpbuf);
	if (s_lseq (".pause", line))		/* .pause command?	 */
	    {
	    t_write (tmpbuf, T_NULL);		/* leave cursor at right */
	    t_read (tmpbuf, &code);		/* read anything	 */
	    return (F_KEEP);
    	    }
	else if (s_lseq (".delay", line))	/* .delay command?	 */
	    {
	    t_write (tmpbuf, T_NULL);		/* leave cursor at right */
	    wait_hold (5000);			/* delay 5 seconds	 */
	    return (F_KEEP); 		
	    }
	else	
	    t_write(tmpbuf, T_NULL);		/* echo normal line 	 */
#ifdef UNIX
	if (!screen)				/* in scroll mode	 */
	    t_write("\n", T_NULL);		/* add line feed	 */
#endif
	}
    return(SUCCESS);
    }

/*
 * intprm - get command parameter from a command string.
 * This function enforces the max # parameters allowed for any tutormode cmd.
 * This function assumes at most 1 command line parameter, unless maxparm == -1.
 * If maxparm == -1, the number of parms is not enforced, and if there are
 * more than 1 actually present, the 1st is returned in cmdprm.
 * NOTE:
 *	This function returns formatted error message and error key to the
 *	caller.
 */

FUNCTION  CODE  intprm 
(
    FUNINT		maxparm,	/* in: max number of parameters (0/1) */
    TEXT		cmdnm[],	/* in: command name */
    TEXT		cmdstr[],	/* in:  the command string	*/
    TEXT		cmdprm[],	/* out: the command parameter	*/
    TEXT		msg[],		/* out: error message		*/
    TEXT		key[]		/* out: error key		*/

 )
    {
    struct SYNBLK	sb;		/* syntax block			*/
    TEXT		token[TOKESIZ+1];
    CODE		code;

    msg[0] = key[0] = EOS;
    initok(&sb, cmdstr);		/* init syntax block for syntax pkg   */
    getvrb(&sb, token);			/* skip the verb		      */
    token[0] = EOS;			/* initialise */
    code = getfld(&sb, token);		/* get first parameter 	*/
    if (code == S_SYNERR) goto in_syner;
    if (token[0] != EOS)			/* parameter found */
	{
	if (maxparm == 0) goto in_nperr;	/* if no parameters allowed   */
	s_copy(token, cmdprm);
	token[0] = EOS;
	code = getfld(&sb, token);
	if (token[0] != EOS  &&  maxparm != -1)	/* if too many parameters     */
	    goto in_tperr;
	if (code == S_RPAREN)
	    {
	    if (s_index(cmdprm, '(') < 0)	/* if this ')' isn't closing */
		goto in_tperr;
	    else
		{
		code = getfld(&sb, token);	/* look again for trailing tokens*/
		if ((token[0] != EOS  &&  maxparm != -1)  ||
		    code == S_RPAREN)
		    goto in_tperr;
		}
	    }
	}
    else
	cmdprm[0] = EOS;
    return(SUCCESS);

in_syner:
    sprintf (msg, "Incorrect format in a command field.");
    s_copy ("TAE-FMTERR", key);
    return(FAIL);

in_nperr:
    sprintf(msg, "The '%s' command has no parameters.", cmdnm);
    s_copy("TAE-NOPARM", key);
    return(FAIL);

in_tperr:
    sprintf(msg, "The '%s' command accepts only %d parameter%s.",
		cmdnm, maxparm, (maxparm>1? "s":""));
    s_copy("TAE-NUMPARM", key);
    return(FAIL);
    }

/*
 * line_noscreen . Get the next interactive line in no-screen mode..
 * The next line is read from the operator or the script file,
 * depending upon the interactive source.
 *
 * Return codes:  SUCCESS or F_FILERR.
 */

FUNCTION  CODE  line_noscreen
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD    */
    TEXT	prompt[],		/* IN: prompt string		     */
    TEXT  	line[],			/* OUT: line from interactive source */
    CODE	*term			/* OUT: line terminator 	     */

 )
    {
    IMPORT struct SFILE		*scr_file;	/* pointer to script file ctx*/
    TEXT	loc_prompt[STRINGSIZ+1];
    CODE	code;	

    s_copy (prompt, loc_prompt);
    if (scr_file != NULL)
	{
	t_highlight (loc_prompt);
	while (FOREVER) 
	    {
	    t_write (loc_prompt, T_PROMPT);	/* write prompt		     */
	    code = getscr(line, term, FALSE);	/* get from script file      */
	    if (code == F_EOF) break;		/* go to operator	     */
	    if (code == F_KEEP) 
		t_write ("", T_STDCC);		/* KEEP=> pause or delay     */
	    else
		return (code);
	    } 
	s_copy ("", loc_prompt);		/* avoid double prompt on EOF */
	}
    if (scr_file == NULL)
	{
        code = cmd_editor (archive, line, loc_prompt, -1, FALSE);
	*term = (code == SUCCESS) ? T_CR : T_ESCAPE;
	code = SUCCESS;				/* terminal: success	*/
	}
    return(code);
    }

/*
 * line_screen . Get the next interactive line in screen mode..
 * The next line is read from the operator or the script file,
 * depending upon the interactive source.
 *
 * Return codes:  SUCCESS or F_FILERR.
 */

FUNCTION  CODE  line_screen 
(
    CODE 	archive,		/* in: A_MENU, A_TUTOR, A_LASTCMD    */
    COUNT	line_num,		/* IN: input line number	     */
    TEXT 	prompt[],		/* IN: prompt string		     */
    TEXT  	line[],			/* OUT: line from interactive source */
    CODE	*term			/* OUT: line terminator 	     */

 )
    {
    IMPORT struct SFILE		*scr_file;	/* pointer to script file ctx*/
    IMPORT COUNT termlines;			/* number of lines on termnl */
    CODE	code;	
    TEXT	loc_prompt[STRINGSIZ+1];

    if (scr_file != NULL)
	{
        s_copy (prompt, loc_prompt);
        t_highlight (loc_prompt);
	while (FOREVER)
	    {
	    t_lclear(line_num, 1);		/* clear prompt line         */
            t_output( line_num, 1, loc_prompt);	/* write prompt              */
	    code = getscr(line, term, TRUE);	/* get from script file      */
	    if (code == F_EOF) break;		/* if EOF try again from opr */
	    if (code != F_KEEP)			/* KEEP => delay or pause    */
	        return(code);
	    }
	}
    if (scr_file == NULL)
	{

 	t_lclear( line_num, 1);

	if (archive == A_TUTOR)
	    code = tut_editor (line, prompt, line_num);
	else if (archive == A_MENU)
	    code = menu_editor (line, prompt, line_num);
	else
	    code = cmd_editor (archive, line, prompt, line_num, TRUE);
	*term = (code == SUCCESS) ? T_CR : T_ESCAPE;
	code = SUCCESS;				/* terminal: always success */
	}
    return(code);
    }
