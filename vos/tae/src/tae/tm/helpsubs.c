/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 *	TAE HELP FACILITY
 *
 *  Change log:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	13-jul-83	Fixed 17-line help problem (pr #411)...dm
 *	15-aug-83	Implemented HELPBLK for error reporting etc,
 *			Implemented "?" for message help...dm
 *	18-aug-83	Fixed put_stdout() calling sequence...dm
 *	08-sep-83	Fixed dimension with SIZ to SIZ+1...palm
 *	04-oct-83	More user lines on help screen...dm
 *	11-oct-83	Fixed unix lint/compilation errors...palm
 *	25-oct-83	Make message file name lowercase before open
 *			so, under unix, user doesn't have to worry
 *			about the case of the message key...palm
 *	29-oct-83	New d_init calling sequence (for .if)...dm
 *	31-oct-83	Close opened msg help file in msg_help()...dm
 *	01-nov-83	Some error msg rewording, other minor changes...dm
 *	17-nov-83	Fix help related problems (pr 577, 585, 586)...dm
 *	19-nov-83	Move lines-per-page computation to helper ...dm
 *	23-nov-83	fix pr's #590, #594...dm
 *	20-jan-84	Fix HELP-PARM to look around .SUBCMD when
 *			searching for help text...palm
 *	25-jan-84	New cmd_screen call...palm
 *	10-feb-84	Implement entr_help for mdf entry help...dm
 *	12-feb-84	Change termtype to CODE from COUNT, refer old
 *			incclose() as  d_incclose()...dm
 *	14-feb-84	Fix bad s_s2i call...palm
 *	16-feb-84	Implement parameter summary page...dm
 *	27-feb-84	Fix crash on no level2 help text...dm
 *      28-feb-84	Reformatted parameter summary page and
 *			fixed display header for HELP-GLOBAL...tlm
 *	22-mar-84	Refer old incopen() as d_incopen()...dm
 *	04-may-84	VALUE_x TO xVAL ... ces
 *	07-may-84	Move IMPORT declarations...lim
 *	17-may-84	New hierarchy and f_crack calling sequence...palm
 *	6-jun-84	Unconditionally set .helpobj in opnhlp...palm
 *	6-jun-84	Fix pr #643 (Conditional Help directives fail)...dm
 *	7-jun-84	Write title to stdout for hardcopy help (pr #626)...dm
 *	6-jul-84	Restrict display of size for string with valid in
 *			parameter summary page...lia
 *			Fix crash in parameter summary for hardcopy...lia
 *	13-jul-84	Display menu and proc libraries for hardcopy help...lia
 *	23-aug-84	Send help to stdout from procedures...nhe
 *	12-oct-84	New pos_scroll() as implemented for UNIX...dm
 *	19-oct-84	TCL 117: Handle compiled PDFs...peb
 *	23-oct-84	New calling seq for getpst...nhe
 *	25-oct-84	Add logic to handle subcommand help...lia
 *	29-oct-84	Fix bad error message for ambiguous subcommand...lia
 *	07-nov-84	Add logic to handle .variable with name and scope
 *			fields...lia
 *	08-dec-84	PR 901: correctly setup helpblk.helpobj...lia
 *			Threw a bunch of structs and defs in HELPINC...ljn
 *************************************************************************
 * CHANGES MADE IN THE RCJM TREE
 *
 *	22-apr-85	Invoke d_remote() for help on remote procs...dm
 *
 *************************************************************************
 * MERGE FOLLOWING CHANGES FROM TAE-V1.3...dm (23-may-85)
 *
 *	22-mar-85	Pass subcommand name to routine that formats the
 *                      title line of the help-parm screen
 **********************************************************************
 *			Threw a bunch of structs and defs in HELPINC...ljn
 *	04-sep-86	Change calling seq to getpst() for PR 1129...peb
 *	10-feb-87	Added TAE_FACELIFT Changes...krw
 *      20-May-87       Added check for DisplayId(xwindow)...tpl
 *      12-aug-87       Integrated with 2.2 changes...tpl
 *	02-dec-87	Declare DisplayId as GENPTR...dm
 *	29-jan-88	Merged overlooked VMS change...ljn
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl
 *	24-feb-88	Changed dummy var to NULL in cmd_parse() calls...ljn
 *	17-mar-88	PR1499: Add call to isTerm() in opnhlp()...ljn
 *	24-mar-88	Deleted TAE_RCJM condtionals...ljn
 *	05-may-88	isTerm() -> f_isterm()...ljn
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	23-may-90	Remove RCJM stuff...ljn
 *	27-jun-90	Remove Facelift code...ljn
 *      19-mar-91       Do a s_lower on intrinsic cmd name...tpl/ln
 * 02-oct-91	PR1201: Help msgs were broken (also added wptfac support)...ljn
 * 11-sep-92    PR875 use ... if value is too long...tpl
 * 19-jan-93    termcols must be IMPORTed...rt
 */



#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"eventinc.inp"	/* event control package		*/
#include	"syninc.inc"	/* syntax package			*/
#include	"terminc.inc"	/* terminal package			*/
#include	"dirinc.inc"	/* directive package			*/
#include	"helpinc.inc"	/* help related struct			*/
#include 	"tminc.inc"	/* TM 					*/

    GLOBAL int	v142hsub = 0;	/* Version number			*/

/*
 * Help command definitions and struct
 */

    struct  HELPCMD
	{
	COUNT	abbchar;		/* min # of characters reqd (0=all) */
	TEXT	cmdnm[TOKESIZ+1];	/* command name */
	COUNT	numparm;		/* number of parameters */
	};

    static struct HELPCMD	helpcmd[] =
	{
	{0,  "",	0 },
	{1,  "PAGE",	1 },
	{1,  "EXIT",	0 },
	};					

#define NHCMD	sizeof(helpcmd) / sizeof(struct HELPCMD)

#ifndef testmain


/*
 *	disp_page - display the specified page
 *
 */

    FUNCTION static CODE disp_page(page, pageblk, dirblk, lefthead, titlept,
				lines_ppg, parmhelp, lastpage)

    FUNINT		page;		/* in: the page number 			*/
    struct PAGEBLK	pageblk[];	/* in: start of each page		*/
    struct DIRBLK	*dirblk;	/* in: directive control block		*/
    TEXT		lefthead[];	/* in: string to follow "HELP DISPLAY: "*/
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept;	/* in: pointer to title string block	*/
					/*     null means no title		*/
    COUNT		lines_ppg[];	/* in: text lines per page of display 	*/
    FUNINT		parmhelp;	/* in: TRUE if help on parameter	*/
    FUNINT		*lastpage;	/* out: TRUE if page displayed is last	*/

    {
    IMPORT COUNT	termlines;		/* number of lines on terminal screen 	*/
    IMPORT struct ECB	ecbi;			/* operator-attn ecb		*/
    IMPORT BOOL		full_scr_help;		/* TRUE if full-screen		*/

    COUNT		numlines;		/* # of lines written so far	*/
    CODE		code;
    TEXT		inline_[STRINGSIZ+1];
    COUNT		max_lines;
    COUNT		title_len;		/* actual lines in title + 1	*/
    COUNT		titlesize;		/* title size in displayed page	*/
    COUNT		newpage;
    COUNT		page_pos();
    BOOL		titlerr;		/* TRUE if error in title block	*/
    BOOL		e_occur();
    VOID		t_attn();
    BOOL		s_equal();		/* TRUE if strings equivalent	*/
    BOOL		s_lseq();

    if (full_scr_help) t_clear();	/* clear the screen		*/
    title_len = (titlept == NULL) ? 0 : (*titlept).numline+1;
					/* +1 for following blank line	*/
    if (titlept == NULL || page > 1)	/* if no title, or not page 1 	*/
	{
	titlesize = 0;
	titlerr = FALSE;
	}
    else				/* title present		*/
	{
        if (title_len > termlines - (H_HDR_SIZE+H_TRAIL_SIZE))
    	    {
    	    titlesize = 1;			   /* arbitrarily set a size */
    	    titlerr = TRUE;
    	    }
    	else
    	    {
	    titlesize = title_len;
	    disptxt(H_HDR_SIZE+1, 1, titlept);	   /* display the title      */
    	    put_line(H_HDR_SIZE+titlesize, 1, " ", TRUE);
						   /* blank line before text */
    	    titlerr = FALSE;
    	    }
	}
    max_lines = (page == 1) ? lines_ppg[0] : lines_ppg[1];
    newpage = page_pos(page, pageblk, dirblk, lines_ppg);	/* position to the specified page */
    numlines = 0;
    do {code = d_text(dirblk, inline_);} while (code==D_PAGE);	/* skip leading .pages */
    *lastpage = FALSE;			/* assume not last page		*/
    if (code != D_EOT && (pageblk[newpage-1].mainpos.possav==FALSE))
	{				/* if this page not displayed before */
  	f_movpos(&(*(*dirblk).sfileptr).posctx, &pageblk[newpage-1].mainpos);
        f_movpos(&(*dirblk).incfile.posctx, &pageblk[newpage-1].incpos);
	pageblk[newpage-1].inblock = (*dirblk).inblock;
	}
    while (numlines < max_lines)
    	{
    	numlines++;
        if (numlines > 1)			/* first line already read   */
	    code = d_text(dirblk, inline_);	/* get next line    	     */
	if (code == D_EOT)
    	    {
    	    if (!parmhelp) *lastpage = TRUE;    /* if parm, summary page last*/
    	    break;
    	    }
    	if (code == D_PAGE)
    	    break;
    	put_line(numlines+H_HDR_SIZE+titlesize, 1, inline_, TRUE);	/* write the line    */
    	if (e_occur(&ecbi))			/* abort display on ^C	*/
    	    {
    	    t_attn(&ecbi);			/* reset for next ^C	*/
    	    break;
    	    }
    	}
    if (lookahead(dirblk) == D_EOT)
	if (!parmhelp) *lastpage = TRUE;	/* .page at end is ignored*/
    dsphdr("Help", lefthead, newpage, *lastpage);
    dsptrail(titlerr);				/* and the trailer	*/
    return(SUCCESS);
    }

/*
 *	disp_help. Display help text.
 *
 *	Return codes:
 *			SUCCESS or FAIL	
 *	NOTE: if helping on parameter, the last page is the summary page.
 */

    FUNCTION  static CODE  disp_help(dirblk, type, lefthead, titlept,
			   helpblk, v, infile)

    struct DIRBLK	*dirblk;	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		type[];		/* in: menu/proc/command/parm/msg/gen	*/
    TEXT		lefthead[];	/* in: string to follow "HELP DISPLAY: " */
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept;	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    struct HELPBLK	*helpblk;	/* out: help control block		*/
    struct VARIABLE	*v;		/* in: variable pointing to parameter 	*/
					/* -- NULL if not help on parm		*/
    FUNINT		infile;		/* in: TRUE if (parm) level2 text exists	*/
					/* ignored if help on others		*/

   {
    IMPORT  CODE	termtype;		/* T_CRT or not			*/
    IMPORT  COUNT	termlines;		/* number of lines on terminal screen 	*/
    IMPORT  BOOL	full_scr_help;		/* TRUE if full screen		*/		
    IMPORT  CODE        HelpWinDisplay();

    struct PAGEBLK	pageblk[MAX_HELP_PAGES];  /* ptr to start of each page */
    CODE		code;
    COUNT 		page;
    COUNT		text_pages;
    BOOL		lastpage;
    BOOL		parmhelp;		/* TRUE if helping on parameter    	*/
    COUNT		lines_ppg[2];		/* # of lines to be displayed	*/
    COUNT		title_len;		/* length of title +1 blank line */

/*
 * NOTE:
 * members mainpos and incpos of pageblk contain the record position contexts
 * of the  main file and the include file (if any) respectively, at the
 * beginning of each page.
 */
    
    title_len = (titlept == NULL) ? 0 : (*titlept).numline+1;
						/* compute lines per page    */
    lines_ppg[0] = termlines - 		
	(H_HDR_SIZE+title_len+H_TRAIL_SIZE);	/* lines on 1st page 	     */
    lines_ppg[1] = lines_ppg[0] + title_len;	/* lines on other pages      */
    page = 1;					/* page counter		     */
    parmhelp = (v != NULL);			/* V block given if help-parm*/
    text_pages = parmhelp ? 0 : MAX_HELP_PAGES; /* default help pages	     */

    if (!full_scr_help)				/* scrolling help? */
    	{
	noscr_help(dirblk, lefthead, titlept, parmhelp, v, infile);  /* no screen help */
	return (SUCCESS);
	}
    if (parmhelp && !infile)
	text_pages = 0;
    else
	{
	code = ini_page(dirblk, pageblk);	/* initialize page ptr block */
        if (code == FAIL && !parmhelp)		/* return only if not a parm */
	    {
	    gen_hlperr(helpblk, "No help available.", "TAE-NOTEXT");
	    return(FAIL);
	    }
	else if (code == SUCCESS && parmhelp)
	    {
	    text_pages = page_pos(MAX_HELP_PAGES, pageblk,
		dirblk, lines_ppg);		/* get number of text pages */
	    }
	}
    do
    	{
	if (parmhelp && page > text_pages)	/* last page on parmhelp    */
	    {
	    lastpage = TRUE;
	    code = disp_summary(v, text_pages+1, lefthead, lines_ppg);
	    }
	else
	    code = disp_page(page, pageblk, dirblk, lefthead, titlept,
		lines_ppg, parmhelp, &lastpage);
	(*helpblk).nextcmd[0] = EOS;
    	code = get_hinput(lastpage, &page,
	       (*helpblk).nextcmd, NEXTSIZ);	/* get new page number		*/
    	}    					/* end main loop			*/
    while ( code != EXIT_CMD);			/* loop until user says exit	*/
    d_incclose(dirblk);				/* close .include file, if open	*/
    pos_scroll();		/* position for scroll mode	*/
    return(SUCCESS);
    }


/*
 *	disp_summary. Display parameter summary.
 */

    FUNCTION  static  CODE  disp_summary(v, page, lefthead, lines_ppg)

    struct  VARIABLE  	*v;		/* VARIABLE struct for the parameter */
    FUNINT		page;		/* page number of display	     */
    TEXT		lefthead[];	/* left hand side of header	     */
    COUNT		lines_ppg[];	/* # of lines per page		     */

    {

    if (v == NULL)			/* safety check			*/
	return(FAIL);			/* error			*/
    parm_sum(v);			/* display  summary text	*/
    dsphdr("Help", lefthead, page, TRUE);	/* display header	*/
    dsptrail(FALSE);				/* and the trailer	*/
    return(SUCCESS);
    }


/*
 *	dsptrail - display the help page trailer
 *
 */

    FUNCTION static VOID dsptrail(titlerr)

    BOOL		titlerr;	/* in: TRUE if there was an error in title block	*/

    {
    IMPORT COUNT	termlines;		/* number of lines on terminal screen 	*/
    IMPORT COUNT	termcols;

    TEXT		border[STRINGSIZ+1];
    COUNT		i;

    for (i=0; i<termcols; i++)
          border[i] = BORDER_CHAR;
    border[i] = EOS;
    put_line(termlines - ERRLIN-1, 1, border, TRUE);
    if (titlerr) help_err("Bad title block in help file.", "TAE-BADTITLE");
    return;
    }

/*
 *	entr_help. Display help on a given mdf entry.
 *
 */
	
    FUNCTION  CODE  entr_help(fctx, libname, filename, helpblk)

    struct   SFILE	*fctx;		/* IN: SFILE of file with entries   */
    TEXT		libname[];	/* IN: library name for file	    */
    TEXT     		filename[];	/* IN: name of file with entries    */
    struct   HELPBLK	*helpblk;	/* IN/OUT: help control block	    */

    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics  */

    struct   DIRBLK	dirblk;		/* directive control block	   */
    TEXT     dirctv[STRINGSIZ+1];	/* directive name found	 	   */
    TEXT     record[STRINGSIZ+1];
    TEXT     header[STRINGSIZ+1];	/* display header		   */
    CODE   		code;


    dirctv[0] = EOS;
    d_init(&dirblk, fctx, libname, (TEXT **)(*char_gbl).v_cvp,
	(*char_gbl).v_count);    		/* init directive block    */
    d_dirctv(&dirblk, dirctv, record);		/* check following dirctv  */
    if (!s_equal(dirctv, ".EHELP"))		/* not an entry help	   */
	{
	gen_hlperr(helpblk, "No help available on '%s'.", "TAE-NOHELP");
	return (FAIL);
	}
    s_copy("MENU: ", header);			/* build entry help header */
    s_append(filename, header);
    s_append(" ", header);
    s_append((*helpblk).helpobj , header);
    code = helper(&dirblk, "menu", header, NULL, helpblk);
						/* display help data */
    return (code);
    }


/*
 *	gen_hlperr. Generate  error message for help related error.
 */
	
    FUNCTION  VOID  gen_hlperr(helpblk, errmsg, errkey)

    struct  HELPBLK	*helpblk;		/* in/out: help output block */
    TEXT		errmsg[];		/* in: error message	     */
    TEXT		errkey[];		/* in: error message key     */

    {
    s_copy(errkey, (*helpblk).errkey);		/* put key in block  	*/
    sprintf((*helpblk).errmsg, errmsg, (*helpblk).helpobj);  /* format message */
    return;					/* and put in block  	*/
    }

/*
 *	genhelp - Position to and display general (i.e, .HELP) data
 *
 *	returns are SUCCESS  or  FAIL.
 *      NOTE: The name of the object to help on is provided by the caller
 *	in helpblk.helpobj.
 */

    FUNCTION  CODE genhelp(fctx, libname, type, lefthead, helpblk)

    struct SFILE	*fctx;		/* In: file ctx block for opened file */
    TEXT		libname[FLIBRSIZ+1];	/* library where file exists  */
    TEXT		type[8];	/* In: menu/gen  		      */
    TEXT		lefthead[];	/* In: left side string of help disp  */
    struct  HELPBLK	*helpblk;	/* In/out: help output control block  */

    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics     */

    struct DIRBLK	dirblk;			/* directive control block    */
    struct TXTSTOR	title;			/* dynamically stored title   */
    TEXT		str[STRINGSIZ+1];
    CODE		code;
    TEXT		dummy[STRINGSIZ+1];
    TEXT		key[KEYSIZ+1];

    d_init(&dirblk, fctx, libname, (TEXT **)(*char_gbl).v_cvp,
	(*char_gbl).v_count);    		/* init directive block       */
    code = d_dirctv(&dirblk, str, dummy);
    if (s_equal(str, ".TITLE"))
	{					/* read title to dyn storage  */
	if ((code = gettitle(&dirblk, &title, dummy, key))!= SUCCESS)
    	    {
	    gen_hlperr(helpblk, dummy, key);	/* save gettitle error msg    */
    	    return(FAIL);
    	    }
	code = d_dirctv(&dirblk, str, dummy);
	}
    else				/* no title */
	title.numline = 0;
    if (!(s_equal(str, ".HELP")))
    	code = d_search(&dirblk, ".HELP", dummy);
    if (code == SUCCESS)
	code = helper(&dirblk, type, lefthead, &title, helpblk);
							/* display help	*/
    else
	{
	code = FAIL;
	gen_hlperr(helpblk, "No help available on '%s'.", "TAE-NOHELP");
	}
    if (title.tp != NULL) fretxt(&title);	/* free the dyamically-stored title	*/
    return(code);
    }

/*
 *	get_hinput - get help input line
 *
 *	Return SUCCESS,  or if 'exit' return EXIT_CMD
 */

    FUNCTION static CODE get_hinput(lastpage, page, nextcmd, nextsiz)

    FUNINT		lastpage;	/* in: true if we the current page is last */
    COUNT		*page;		/* in/out: the new page number		*/
    TEXT		nextcmd[];	/* out: next command (or null)		*/
    FUNINT		nextsiz;	/* in: size of nextcmd			*/

    {
    TEXT		param[STRINGSIZ+1];    	/* associated parameter */
    CODE		code;

    while (FOREVER)
  	{
    	code = gthlpcmd(param, nextcmd, nextsiz);	/* get command and parameter */
    	if (code == NULL_CMD)				/* a carriage return */
	    {
	    *page = (lastpage) ? 1 : *page+1; 		/* wrap around if last */
	    return (SUCCESS);
	    }
    	else if (code == PAGE_CMD)
	    {
	    code = get_page(param, page);
	    if (code == SUCCESS)
		{
		if (*page > MAX_HELP_PAGES)		/* if page too large */
		    *page = MAX_HELP_PAGES;		/* give last page    */
		return (SUCCESS);
		}
	    }
    	else 						/* EXIT or FAIL	     */
	    return (EXIT_CMD);
	}
    }

/*
 *	get_page - get the page number from the command line
 *
 */

    FUNCTION  static  CODE get_page(pagestr, page)

    TEXT		pagestr[];		/* in: page string 	*/
    COUNT		*page;			/* in/out: page number	*/

    {
    CODE		code;
    TAEINT		pagenum;
    static TEXT		pagen_msg[] = "Page must be a positive number.";


    if (NULLSTR(pagestr))
	{
	(*page)++;			/* if no value for page, go to next*/
	code = SUCCESS;
	}
    else
	{
	code = s_s2i(pagestr, &pagenum);	/* make an integer	*/
	if (code != SUCCESS || pagenum <= 0)
	    {
	    help_err(pagen_msg, "TAE-PAGENUM");
    	    code = FAIL;
	    }
	else
	    *page = pagenum;			/* get page number	*/
	}
    return(code);
    }

/*
 *	gthlpcmd - get help related command from the operator.
 */

    FUNCTION  static  CODE  gthlpcmd(param, nextcmd, nextsiz)

    TEXT	param[];			/* out: command parm	*/
    TEXT	nextcmd[];			/* out: next command	*/
    FUNINT	nextsiz;			/* in: size of ^	*/

    {
    IMPORT CODE		termtype;		/* T_CRT or not			*/
    IMPORT COUNT	termlines;		/* number of lines on terminal screen 	*/

    TEXT	command[STRINGSIZ+1];		/* command field of...	*/
    TEXT	cmdstr[CMDLINSIZ+1];		/* command string 	*/
    CODE	terminat;			/* command line terminator */
    struct	SYNBLK	  sb;			/* syntax block 	*/
    struct	HELPCMD	  *cmdptr;		/* ptr to a cmd table entry */
    CODE	code;
    COUNT	i;
    TEXT	errmsg[STRINGSIZ+1];
    TEXT	errkey[KEYSIZ+1];
    static TEXT		helpopts[] = 		/* prompt			*/
			"Enter EXIT or PAGE n (or press RETURN for next page)";


    while (FOREVER)
	{
        put_line(termlines - PMTLIN - 1, 1, helpopts, FALSE); 	/* display options */
 	code = cmd_screen (A_NONE, cmdstr, &terminat);	
	if (code != SUCCESS)
	    return(FAIL);			/* could not get a command  */
	if (terminat == T_ESCAPE)		/* no command 		    */
	    continue;
	initok(&sb, cmdstr);			/* init for syntax scanning */
  	getvrb(&sb, command);			/* retrieve command name    */
	
	for (i = 0; i < NHCMD; i++)			/* search table     */
 	    {
	    cmdptr = &(helpcmd[i]);
	    if ((*cmdptr).abbchar == 0)
	        {
	    	if (s_equal(command, (*cmdptr).cmdnm))
	  	    break;				/* exact match found */
	    	}
	     else if (s_length(command) >= (*cmdptr).abbchar)
	    	{
	    	if (s_lseq(command, (*cmdptr).cmdnm))
		    break;				/* strings match */
	    	}
	    }
	if (i < NHCMD)					/* if match found    */
	    {						/* get param, if any */
	    param[0] = EOS;
	    code = intprm((*cmdptr).numparm, (*cmdptr).cmdnm, cmdstr, param,
			errmsg, errkey);
	    if (code == SUCCESS)
		{
		if (termtype == T_CRT)
		    t_lclear(termlines - ERRLIN, 1); 	/* blank error msg line */
		return (i+1000);			/* return cmd type	*/
		}
	    else
		help_err(errmsg, errkey);		/* report error		*/
	    }
	else						/* not a help command	*/
	    {						/* previous mode command*/
	    s_bcopy(cmdstr, nextcmd, nextsiz);		/* bounded copy 	*/
	    return (EXIT_CMD);				/* loooks like an EXIT	*/
	    }
	}
    }

/*
 *	helper - process help file text and handle help displays
 *
 *	Returns:  SUCCESS or FAIL.
 *
 */

    FUNCTION CODE helper(dirblk, type, lefthead, titlept, helpblk)

    struct DIRBLK	*dirblk;	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		type[];		/* in: menu/proc/command/parm/msg/gen	*/
    TEXT		lefthead[];	/* in: string to follow "HELP DISPLAY: " */
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept;	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    struct HELPBLK	*helpblk;	/* out: help control block		*/

    {
    CODE		code;


    code = disp_help(dirblk, type, lefthead, titlept, helpblk, NULL, TRUE);
					/* display help text		*/
    return (code);
    }

/*
 *	help_err - Write a help error message
 *
 */

    FUNCTION  VOID  help_err(errmsg, key,a1, a2, a3, a4, a5)

    TEXT		errmsg[];		/* in: the message to write		*/
    TEXT		key[];			/* in: message key		*/
    FUNINT		a1, a2, a3, a4, a5;

    {
    wrterr(errmsg, key, a1, a2, a3, a4, a5);
    return;
    }

/*
 *	ini_page - Initialize the page block
 *
 *	Return codes: SUCCESS or FAIL.
 */

    FUNCTION CODE ini_page(dirblk, pageblk)

    struct DIRBLK	*dirblk;		/* in: directive block		*/
    struct PAGEBLK	pageblk[];		/* in/out: page block to be inititialized */

    {	
    struct SFILE	*sfile;
    COUNT		i;
    CODE		code;
    TEXT		inline_[STRINGSIZ+1];

/*
 *	read the first line and save its position
 */
     do
        {code = d_text(dirblk, inline_);}
    while (code==D_PAGE);
    if (code != SUCCESS)  				/* no text found */
    	return(FAIL);				
    sfile = (*dirblk).sfileptr;
    f_movpos(&(*sfile).posctx, &pageblk[0].mainpos);	     /* save mainfile position */
    f_movpos(&(*dirblk).incfile.posctx, &pageblk[0].incpos); /* save incfile position */
    pageblk[0].inblock = (*dirblk).inblock;		     /* save inblock condition */
    for (i=1; i<MAX_HELP_PAGES; i++)
	{
	pageblk[i].mainpos.possav = FALSE;
    	pageblk[i].incpos.possav = FALSE;
	pageblk[i].inblock = FALSE;
    	}
    position(dirblk, pageblk, 1);			/* position at page 1 */
    return(SUCCESS);

    }

/*
 *	lookahead - Read a line to see if it's EOT
 *
 *	Return D_EOT or SUCCESS
 */

    FUNCTION CODE lookahead(dirblk)

    struct DIRBLK	*dirblk;		/* in: directive control block	*/

/* We restore via the sfile using the sfile already there.  We do this
 * because the sfile has the record address of the record just read	*/

    {
    TEXT		dummy[STRINGSIZ+1];
    CODE		code;

    do
	code = d_text(dirblk, dummy);
    while (code == D_PAGE);
    if ((*dirblk).incfile.posctx.possav)	/* if read from inc file */
	f_setpos(&(*dirblk).incfile, &(*dirblk).incfile.posctx);
    else					/* text read frommain file */
	f_setpos((*dirblk).sfileptr, &(*(*dirblk).sfileptr).posctx);
    if (code != D_EOT) code = SUCCESS;
    return(code);
    }

/*
 *	msg_help. Help on a message key.
 *	The file name containing the key is obtained from the first part
 *	of the message key. The file is then located by a hierarchy search
 *	through the libraries. However, if the file name is "TAE" or "WPT", it
 *	is searched in the TAE help library MSGHELPLIB, rather than by a 
 *	hierarchy search.
 *
 */

    FUNCTION  CODE msg_help(fctx, msgkey, helpblk)

    struct  SFILE	*fctx;		/* In: file context block for opened file			*/
    TEXT		msgkey[];	/* In: message key 		*/
    struct  HELPBLK	*helpblk;	/* Out: help output control blk	*/

    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics */

    COUNT		nchar;
    TEXT		key[KEYSIZ+1];		/* key to be located 	    */
    TEXT		filenm[FSPECSIZ+1];
    struct  FSBLOCK	fsblock;		/* fsblock for help files   */
    struct  DIRBLK	dirblk;			/* directive block 	    */
    struct  INXREC	inxrec;			/* index file record struct */
    struct SYNBLK	sb;			/* syntax block		    */
    TEXT		keyrec[STRINGSIZ+1];
    TEXT		string1[STRINGSIZ+1];
    TEXT		string2[STRINGSIZ+1];
    TEXT		header[40];
    COUNT		recsize;		/* size of record read    */
    CODE		code;
    TEXT		errstr[STRINGSIZ+1];


    nchar = s_index(msgkey, '-');		/* locate '-' in msgkey */
    if (nchar <= 0) goto mh_synerr;		/* key syntax error	*/
    bytmov((GENPTR)msgkey, (GENPTR)filenm, nchar); /* get facility part */
    s_bcopy("FAC", &filenm[nchar], FNAMESIZ);
    s_lower (filenm);				/* make standard case	*/
    s_copy(&msgkey[nchar+1], key);		/* get key part		*/
    if (NULLSTR(key))				/* key syntax error	*/
    	goto mh_synerr;
    if (s_equal(filenm, "TAEFAC") || s_equal(filenm, "WPTFAC"))
	{					/* if help on tae messages   */
	f_crack(filenm, MSGHELPLIB, "", INX_TYPE, &fsblock, errstr);
	code = f_opnblk(fctx, HELPLUN, &fsblock, F_READ);
	}
    else
	code = hierarchy(fctx, filenm, INX_TYPE, HELPLUN, &fsblock, errstr);
    if (code != SUCCESS) goto mh_facerr;	/* error in open	     */
    do
	{
	code = f_bread(fctx, &inxrec, sizeof(inxrec), &recsize);  /* read index file record */
	if (s_equal(inxrec.key, key))		/* key matches ? 	     */
	    break;
	}
    while (code == SUCCESS);
    if (code != SUCCESS) goto mh_keyerr;	/* key not found, error      */

    f_close(fctx, F_KEEP);			/* close index file	     */
    s_copy(MSG_TYPE, fsblock.type);		/*  modify for text file type*/
    code = f_opnblk(fctx, HELPLUN, &fsblock, F_READ);	/* open in same libr */
    if (code != SUCCESS) goto mh_facerr;

    f_setpos(fctx, &inxrec.posctx);		/* position to specified rec */
    code = f_read(fctx, keyrec);		/* read the record 	     */
    if (code == SUCCESS)
	{					/* check if proper one 	     */
	initok(&sb, keyrec);	
	getfld(&sb, string1);			/* get first word	     */
	getfld(&sb, string2);			/* get second word	     */
	}
    if (code != SUCCESS || (!s_equal(string1, ".key"))
	|| (!s_equal(string2, key))) goto  mh_badrec;	/* invalid record    */

    s_copy(" message key = '", header);		        /* build display hdr */
    s_append(msgkey, header);
    s_append("'", header);
    d_init(&dirblk, fctx, fsblock.libr, (TEXT **)(*char_gbl).v_cvp,
	(*char_gbl).v_count);     			/* init directive blk*/
    code = helper(&dirblk, "msg", header, NULL, helpblk);  /* display help data */
    f_close(fctx, F_KEEP);				/* close the file    */
    return(code);				

mh_synerr:
    gen_hlperr(helpblk,
    		"Incorrect message key format.  Correct format is 'xxx-yyy'.",
	 	"TAE-BADKEY");
    return(FAIL);

mh_facerr:
    s_bcopy(filenm, (*helpblk).helpobj, nchar);		/* facility name    */
    gen_hlperr(helpblk, "No help available for messages beginning with '%s'.",
		"TAE-NOFAC");
    return(FAIL);

mh_keyerr:
    s_copy(key, (*helpblk).helpobj);
    gen_hlperr(helpblk, "No help available for message with key '%s'.",
    		"TAE-NOKEY");
    f_close(fctx, F_KEEP);		/* close the message file	*/
    return(FAIL);

mh_badrec:
    gen_hlperr(helpblk, "Malformed help message file.", "TAE-MALMSGF");
    f_close(fctx, F_KEEP);		/* close the message file	*/
    return(FAIL);			
    }

/*
 *	noscr_help. Help if not full screen.  We just put to standard output or the
 *		    terminal, depending on the last argument.  We need this
 *		    last argument because tutor wants to go to the terminal
 *		    no matter what the current setting of standard out is.
 *
 */

    FUNCTION  static VOID noscr_help(dirblk, hdr, titlept, parmhelp, v, infile)

    struct DIRBLK	*dirblk;	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		hdr[];		/* in: header				*/
    struct TXTSTOR	*titlept;	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    FUNINT  		parmhelp;	/* in: TRUE if helping on parameter    	*/
    struct  VARIABLE	*v;		/* in: variable pointing to parameter 	*/
    FUNINT		infile;		/* in: true if level2 help exists	*/

   {
    IMPORT  struct ECB	ecbi;			/* operator-attn ecb	*/

    TEXT		helptext[STRINGSIZ+1];
    CODE		code;
    BOOL		e_occur();
    VOID		t_attn();
    COUNT 		i;
    TEXT 		**str;
    TEXT		headstr[STRINGSIZ+1];

    put_line(0,0,"",FALSE);			/* put header		*/
    s_copy(" HELP INFORMATION - ", headstr);
    s_append(hdr, headstr);
    put_line(0,0,headstr, FALSE);
    put_line(0,0,"", FALSE);
    if (titlept != NULL && (*titlept).numline != 0)
	{
	str = (*titlept).tp;
	for (i=0; i < (*titlept).numline; i++)
    	    put_line(0,0,str[i], FALSE);	/* title		*/
	}	
    else
        put_line(0,0,"", FALSE);
    if (infile)					/* help text in file	*/
	{
	code = d_text(dirblk, helptext);
    	while (code != D_EOT)			/* display the text	*/
    	    {
    	    if (e_occur(&ecbi))			/* abort display on ^C	*/
    	        {
    	        t_attn(&ecbi);			/* reset for next ^C	*/
    	        return;
    	        }
    	    if (code == D_PAGE)
	        {
    	        put_line(0,0,"", FALSE);
    		put_line(0,0,"", FALSE);
	        }
	    else
        	put_line(0,0,helptext, FALSE);
    	    code = d_text(dirblk, helptext);
    	    }
        }
    if (parmhelp)				/* helping on parameter	   */
	parm_sum(v);				/* also give summary page  */
    return;
    }

/*	opnhlp - open help file.
 *	
 *	Return codes are SUCCESS or FAIL.
 *
 */

    FUNCTION CODE opnhlp (fctx, procstr, proc, sub, library,
			  itrinsic, helpblk)

    struct SFILE	*fctx;			/* out: opened help file context */
    TEXT		procstr[];		/* in/out: proc(-sub) name	*/
    TEXT		proc[];			/* out: proc name to help on	*/
    TEXT		sub[];			/* out: sub name to help on	*/
    TEXT		library[];		/* out: library name		*/
    FUNINT		*itrinsic;		/* out: TRUE if intrinsic cmd	*/
    struct  HELPBLK	*helpblk;		/* out: help output control blk	*/

    {
    struct VARIABLE	*v;			/* symbol table variable	*/
    struct ITRCMD 	*itrincmd;		/* ITRCMD command table entry	*/
    struct ITRCMD	*itrinsub;		/* ITRCMD subcommand table entry */
    struct ITRCMD	*itr;
    TEXT		sttmt[CMDLINSIZ+1];	/* PDF intro statement		*/
    struct SYMTAB	pdfstb;			/* PDF intro sttmt symbol table	*/
    CODE		code;	
    struct FSBLOCK	fsblock;		/* file spec block		*/
    TEXT		name[FNAMESIZ+1];
    COUNT		recs;
    COUNT		num_prec;	/* # comment, null lines preceding sttmt*/
    struct SYNBLK	sb;			/* a syntax block		*/
    TEXT		subcmd[SUBCMDSIZ+1];
    TEXT		errstr[STRINGSIZ+1];
    struct POSCTX	pos;
    struct ITRCMD	*intrin();
    struct ITRCMD	*itrsub();
    struct VARIABLE	*lookex();

    (*helpblk).compiled = FALSE;
    pdfstb.link = NULL;			/* in case incorrect deltab		*/
    if (cmd_parse(&sb, procstr, NULL, proc, sub) != SUCCESS)
    	goto dh_prerr;
    itrincmd = intrin(proc);		/* point to intrinsic command entry	*/
    if (itrincmd != NULL)		/* if proc is intrinsic 		*/
	{				/* build FSBLOCK for the help file	*/
        TEXT intrcmd[FNAMESIZ+1];
	*itrinsic = TRUE;				/* mark as intrinsic	*/
	s_copy ( (*itrincmd).cmd, intrcmd );
	s_lower(intrcmd);
	s_copy(HELPLIB, fsblock.libr);
	s_copy(intrcmd, fsblock.name);
	s_copy(HLP_TYPE, fsblock.type);
	fsblock.attr[0] = EOS;
	s_copy(fsblock.libr, library);			/* copy to caller	*/
	s_copy((*itrincmd).cmd, proc);			/* return full name	*/
        s_copy (fsblock.name, (*helpblk).helpobj);	/* set for caller	*/
	if (!NULLSTR(sub) || sub[1] == '-')
	    {
	    itrinsub = itrsub(sub, itrincmd);
	    if (itrinsub != NULL)
		s_copy ((*itrinsub).subcmd, sub);
	    else
		goto dh_suberr;
	    }
	}
    else						/* proc not intrinsic 	*/
	{						/* 1st open PDF		*/
	*itrinsic = FALSE;				/* mark not intrinsic	*/
	code = hierarchy(fctx, proc, PDF_TYPE, PDFLUN, &fsblock, errstr);
	if (code != SUCCESS) goto dh_poper;
	if (f_isterm (fctx)) goto dh_poper;  	/* PR 1499		*/
	s_copy(fsblock.libr, library);			/* copy to caller	*/
	s_copy(fsblock.name, name);
        s_copy (fsblock.name, (*helpblk).helpobj);	/* set for caller	*/
	if (getpst(fctx, sttmt, &recs, &num_prec, &pos) != SUCCESS) /* get PDF intro 	*/
	    goto dh_pirer;
	if (prsstt(sttmt, &itr, &pdfstb) != SUCCESS)	/* parse intro		*/
	    goto dh_piper;
	if (s_equal((*itr).subcmd, "COMPILED"))		/* is help on compiled?	*/
	    {						/* if so...		*/
	    deltab(&pdfstb);
	    (*helpblk).compiled = TRUE;			/* flag it and...	*/
	    return(SUCCESS);				/* any help is file resident*/
	    }
	v  = lookex(&pdfstb, "HELP");			/* point to HELP var 	*/
    	if (v == NULL)					/* not an intro cmd	*/
	    goto dh_intrer;
	if (s_equal(SVAL(*v,0), "*"))
	    {
	    deltab(&pdfstb);
	    return(SUCCESS);				/* PDF contains help 	*/
	    }
	f_crack(SVAL(*v,0), library, name,
	    HLP_TYPE, &fsblock, errstr);		/* re-build fsblock	*/
        deltab(&pdfstb);
	f_close(fctx, F_KEEP);				/* close PDF		*/
	}
    if (f_opnblk(fctx, HELPLUN, &fsblock, F_READ) != SUCCESS)
	    goto dh_operr;
    return(SUCCESS);

dh_piper:
dh_pirer:
    f_close(fctx, F_KEEP);
dh_poper:
dh_operr:
    deltab (&pdfstb);
    s_copy(proc, (*helpblk).helpobj);		
    gen_hlperr(helpblk, "No help available on '%s'.", "TAE-NOHELP");
    return(FAIL);

dh_intrer:
    f_close(fctx, F_KEEP);
    deltab (&pdfstb);
    s_copy(proc, (*helpblk).helpobj);		
    gen_hlperr(helpblk, "Introductory command missing in the '%s' pdf.",
		"TAE-MISINTRO");
    return(FAIL);

dh_suberr:
    s_copy(sub, (*helpblk).helpobj);
    gen_hlperr(helpblk, "Undefined or ambiguous subcommand '%s'.",
    		"TAE-UNDEFSUB");
    return(FAIL);

dh_prerr:
    s_copy(procstr, (*helpblk).helpobj);		
    gen_hlperr(helpblk, "Incorrect command name format.", "TAE-ERPROC");
    return(FAIL);
    }

/*
 *	page_pos - Position to specified page
 *
 *	Return the page number of the page reached
 *	(either the input page or the last)
 */

    FUNCTION COUNT page_pos(page, pageblk, dirblk, lines_ppg)

    FUNINT		page;		/* in: the page to position to	    */
    struct PAGEBLK	pageblk[];	/* in: page control block	    */
    struct DIRBLK	*dirblk;	/* in/out: directive control block  */
    COUNT		lines_ppg[];	/* in: number of lines per page	    */

    {
    COUNT		i, currpage;
    COUNT		max_lines;	/* lines in current page	    */
    COUNT		numlines;
    CODE		code;
    TEXT		inline_[STRINGSIZ+1];


    code = position(dirblk, pageblk, page);	/* position to required page */

    if (code == FAIL)				/* never saw this page before */
    	{				
    	for (i=0; i < MAX_HELP_PAGES; i++)	
    	    if (pageblk[i].mainpos.possav == FALSE) break;	/* found the first empty 	*/
	position(dirblk, pageblk, i);		/* reset to last displayed page */
    	if (i == MAX_HELP_PAGES)
    	    {
    	    help_err("Help information exceeds TAE capacity.   Continuing.",
    			"TAE-EXHELP");
    	    return (MAX_HELP_PAGES);
    	    }
    	currpage = i;
    	while (currpage < page)
    	    {
    	    do
		{code = d_text(dirblk, inline_);}
	    while (code == D_PAGE); 	/* skip consec .PAGE	*/
	    max_lines = (currpage == 1) ? lines_ppg[0] : lines_ppg[1];
    	    numlines = 1;
    	    if (code != D_EOT && (pageblk[currpage-1].mainpos.possav==FALSE))
		{			  /* if we don't yet have this page */
 		f_movpos(&(*(*dirblk).sfileptr).posctx,
					&pageblk[currpage-1].mainpos);
		f_movpos(&(*dirblk).incfile.posctx,
					&pageblk[currpage-1].incpos);
		pageblk[currpage-1].inblock = (*dirblk).inblock;
		}
     	    while (numlines < max_lines)
    		{
    		if (code == D_EOT) 		/* end of help text		*/
    		    {
    		    if (numlines==1) currpage--;  /* means we never read any text on this page */
       		    position(dirblk, pageblk, currpage);
					/* reset position context for new page	*/
    		    return(currpage);
    		    }
    		if (code == D_PAGE) break;
    		numlines++;
    		code = d_text(dirblk, inline_);
    		}
    	    currpage++;
    	    }
    	}
    if (lookahead(dirblk)==D_EOT)
        {
        page--;		/* don't start on an EOT */
	position(dirblk, pageblk, currpage-1);
    	}
    return(page);
    }

/*	parm_help. Detailed help on a parameter.
 *
 *	Note: It is assumed that the last page of help on a parameter
 *	is the parameter summary page, which does not exist in the help
 *	text. It is generated out of the information in the VARIABLE
 *	struct for that parameter and displayed as a help page,
 *	transparent to the user.
 */

    FUNCTION CODE parm_help(v, infile, dirblk, lefthead, titlept, helpblk)

    struct VARIABLE     *v;		/* in: VARIABLE struct for the parameter	*/
    FUNINT		infile;		/* in: TRUE if parm help text is in file	*/
    struct DIRBLK	*dirblk;	/* in: directive control block pointing	*/
					/*     to start of help text		*/
    TEXT		lefthead[];	/* in: string to follow "HELP DISPLAY: " */
    					/*     in the help display header	*/
    struct TXTSTOR	*titlept;	/* in: pointer to title string. 		*/
    					/*     null if no title or null title	*/
    struct HELPBLK	*helpblk;	/* out: help control block		*/

    {
    CODE		code;

    code = disp_help(dirblk, "parm", lefthead, titlept, helpblk,
			v, infile);	    	/* display help information  */
    return (code);
    }

/*     parm_sum. Parameter Specification Summary Display
 *
 *     Information displayed include:
 *
 *	    - type ( INTEGER, REAL, STRING, NAME, KEYWORD, FILE )
 *	    - access ( IN, OUT, INOUT, NOCHECK; valid for type = FILE only )
 *	    - size ( 1 to 132 characters; valid for type = STRING only )
 *	    - count ( min : max )
 *	    - nullable ( yes, no )
 *	    - qualifiers (yes, no)
 *	    - valid list
 */


    FUNCTION CODE parm_sum (v)

    struct VARIABLE 	*v;	/* in: Ptr to VARIABLE struct of parameter   */

    {
    IMPORT  BOOL	full_scr_help;

    TEXT		type[8];	/* parameter type               */
    TEXT	 	line[80];	/* formatted output text line   */
    TEXT		blanks[23];	/* array of blanks for filler   */
    TEXT		num[14];
    TINY		n;
    COUNT		row;		/* row number for output        */
    COUNT		col1;		/* column number for attribute  */
    COUNT		col2;		/* column number for value      */

    if (full_scr_help)
	t_clear ();			/* Clear CRT screen 		*/
    else
	put_line(0,0," ",FALSE);	/* else, write a blank line	*/

    row = H_HDR_SIZE+1;			/* position after page header   */
    if ((*v).v_valid != NULL && full_scr_help)
	{
	col1 = 7;			/* columns for leftside of a    */
	col2 = 23;			/*  double display with valid list */
	}
    else
	{
	col1 = 20;			/* columns for a single display */
	col2 = 44;			/*  with no valid list or non-CRT */
	}
    parms_hdr(col1, col2, &row);	/* Display summary headers  	*/
    s_blank (blanks, 22);
    s_blank (line, 79);
    s_copy ("type", &line[col1]);
    if ((*v).v_type == V_INTEGER)	/* Determine parameter type */
        s_copy ("INTEGER", type);
    else if ((*v).v_type == V_REAL)
        s_copy ("REAL", type);
    else if ((*v).v_type == V_STRING)
	{
	if ((*v).v_keyword)		/* KEYWORD a subcategory of STRING */
	    s_copy ("KEYWORD", type);
	else if ((*v).v_file)		/* FILE a subcategory of STRING */
	    s_copy ("FILE", type);
	else s_copy ("STRING", type);
	}
    else if ((*v).v_type == V_NAME)
	s_copy ("NAME", type);
    s_append (blanks, line);
    s_copy (type, &line[col2]);
    put_line (row++, 1, line, FALSE);
    					/* Continue process for type != NAME */
    if (! s_equal (type, "NAME"))
	{
	if ((*v).v_file)		/* Determ access mode for type=FILE */
	    {
	    s_blank (line, 79);		
	    s_copy ("access mode", &line[col1]);
	    s_append (blanks, line);
	    if ((*v).v_filemode == V_IN)
		s_copy ("IN", &line[col2]);
	    else if ((*v).v_filemode == V_OUT)
		s_copy ("OUT", &line[col2]);
	    else if ((*v).v_filemode == V_INOUT)
		s_copy ("INOUT", &line[col2]);
	    else if ((*v).v_filemode == V_NOCHECK)
		s_copy ("NOCHECK", &line[col2]);
	    put_line (row++, 1, "", FALSE);
	    put_line (row++, 1, line, FALSE);	
	    }
    					/* Get max size for type=STRING */
    	if (s_equal (type, "STRING") && (*v).v_valid == NULL)
	    {
	    s_blank (line, 79);
	    s_copy ("max size", &line[col1]);
	    s_append (blanks, line);
	    s_i2s ((*v).v_size, &line[col2]);
	    put_line (row++, 1, "", FALSE);
	    put_line (row++, 1, line, FALSE);	
	    }
					/* Get min and max count */
    	s_blank (line, 79);
	s_copy ("count", &line[col1]);
	s_append (blanks, line);
	s_i2s ((*v).v_minc, &line[col2]);
	if ((*v).v_minc != (*v).v_maxc) /* a range of number for count ? */
	    {
	    s_append (" : ", line);	/* append on max count   */
	    s_i2s ((*v).v_maxc, num);
	    s_append (num, line);
	    }
    	put_line (row++, 1, "", FALSE);
    	put_line (row++, 1, line, FALSE);	

	s_blank (line, 79);		/* Get status for nullable */
	s_copy ("nullable", &line[col1]);
	s_append (blanks, line);
	s_copy ((*v).v_nullable ? "yes" : "no", &line[col2]);
	put_line (row++, 1, "", FALSE);
	put_line (row++, 1, line, FALSE);

	s_blank (line, 79);			/* qualifiers	     */
	s_copy ("qualifiers", &line[col1]);
	s_append (blanks, line);
	s_copy ((*v).v_qualst.link ? "yes" : "no", &line[col2]);
    	put_line (row++, 1, "", FALSE);
    	put_line (row++, 1, line, FALSE);	
	put_line (row++, 1, "", FALSE);

	n = row;			/* save row number            */
	parms_getval(v, type, &row);	/* Display the valid list     */
	s_blank (line, 79);
	line[0] = DIVIDE_LINE;
	line[1] = EOS;
	for (; row < n; row++)		/* extend division line to longest */
	    put_line (row, 34, line, FALSE); /*  side on a double display    */
	}
    return;
    }

/*
 *	parms_hdr - display parameter summary headers.
 */

    FUNCTION static VOID parms_hdr(col1, col2, row_num)

    FUNINT		col1;		/* in: column number for attribute */
    FUNINT		col2;		/* in: column number for value     */
    COUNT		*row_num;	/* in/out: row number to write on  */

    {
    TEXT		line[80];	/* formatted output text line      */
    TEXT		blanks[23];	/* array of blanks for filler      */
    COUNT		row;
    COUNT		margin;		/* column number to start output   */
    BOOL		sectim;		/* flag to indicate 2nd header     */

    row = *row_num;
    sectim = TRUE;
    if (row == H_HDR_SIZE+1)		/* print title with 1st header only */
	{
	s_blank (line, 79);
	s_copy ("PARAMETER DEFINITION", &line[26]);
    	put_line (row++, 1, line, FALSE);	
    	put_line (row++, 1, "", FALSE);
	sectim = FALSE;
	margin = 1;
	}
    else
	margin = 34;
    s_blank (blanks, 22);
    s_blank (line, 79);
    if (sectim)				/* add division line for second list */
	s_copy ("|   ", line);
    s_copy ("attribute", &line[col1]);
    s_append (blanks, line);
    s_copy ("value", &line[col2]);
    put_line (row++, margin, line, FALSE);
     s_blank (line, 79);
    if (sectim)				/* add division line for second list */
	s_copy ("|   ", line);
    s_copy ("---------", &line[col1]);
    s_append (blanks, line);
    s_copy ("-----", &line[col2]);
    put_line (row++, margin, line, FALSE);
    *row_num = row;
    return;
    }

/*
 *	parms_getval - Get the valid list for parameter summary.
 */

    FUNCTION static VOID parms_getval(v, type, row_num)

    struct VARIABLE 	*v;		/* in: ptr to VARIABLE struct     */
    TEXT		type[];		/* in: variable data type	  */
    COUNT		*row_num;	/* in/out: row number to write on */

    {
    IMPORT  BOOL	full_scr_help;
    IMPORT COUNT	termcols;

    TINY		i;
    BOOL		lastitem;	/* TRUE if reached last item in list */
    BOOL		exitflag;	/* TRUE if display page full         */
    struct I_VALID 	*iv;		/* ptr to integer range check struct */
    struct R_VALID 	*rv;		/* ptr to real range checking struct */
    struct S_VALID 	*sv;		/* ptr to string range check struct  */
    TEXT		line[VALIDSIZ+40];	/* formatted output text line        */
    TEXT		blanks[23];	/* array of blanks for filler        */
    TEXT		num[14];
    COUNT		row;
    COUNT		col1;		/* column number for attribute       */
    COUNT		col2;		/* column number for value           */
    COUNT		margin;		/* column number to start output     */

    row = *row_num;
    if ((*v).v_valid != NULL)		/* Any entry for valid list ? */
	{
	lastitem = FALSE;
	if (full_scr_help)		/* full screen			*/
	    {
	    margin = 34;
	    col1 = 4;			/* set column numbers for CRT */
	    col2 = 19;
	    row = H_HDR_SIZE + 3;	/* reset row to line with header */
	    parms_hdr(col1, col2, &row);  /* output 2nd header for valid list */
	    }
	else
	    {
	    margin = 1;	
	    col1 = 20;			/* set column numbers for non-CRT */
	    col2 = 44;
	    }
	s_blank (blanks, 22);
	s_blank (line, 79);
	s_copy ("valid list", &line[col1]);
	s_append (blanks, line);
	    				/* Get valid values for type=INTEGER */
	if (s_equal (type, "INTEGER"))
	    {
	    iv = (struct I_VALID *) (*v).v_valid;
	    for (i = 0; i < (*iv).count; i++)
		{
		s_i2s ((*iv).range[i].low, &line[col2]);
		if ((*iv).range[i].low != (*iv).range[i].high)
		    {
		    s_append (" -> ", line);
		    s_i2s ((*iv).range[i].high, num);
		    s_append (num, line);
		    }
		if (i+1 == (*iv).count)
		    lastitem = TRUE;
		parms_dspval (lastitem, margin, &row, line, &exitflag);
		if (exitflag) break;
		}
	    }
	    				/* Get valid values for type=REAL */
	else if (s_equal (type, "REAL"))
	    {
	    rv = (struct R_VALID *) (*v).v_valid;
	    for (i = 0; i < (*rv).count; i++)
		{
		s_r2s ((*rv).range[i].low, &line[col2]);
		if ((*rv).range[i].low != (*rv).range[i].high)
		    {
		    s_append (" -> ", line);
		    s_r2s ((*rv).range[i].high, num);
		    s_append (num, line);
		    }
		if (i+1 == (*rv).count)
		    lastitem = TRUE;
		parms_dspval (lastitem, margin, &row, line, &exitflag);
		if (exitflag) break;
		}
	    }
	else
	    {
	    TAEINT lenallow;

	    lenallow = termcols - margin - col2 - 1;

	    sv = (struct S_VALID *) (*v).v_valid;
	    for (i = 0; i < (*sv).count; i++)
		{
		s_copy ("\"", &line[col2]);

		if ( s_length ( (*sv).slist[i].string ) > lenallow )
		    {
		    s_bcopy((*sv).slist[i].string,&line[col2+1], lenallow);
		    line[col2+lenallow-4]='\0';
		    s_append ("...\"", line);
		    }
 		else
		    {
		    s_append ((*sv).slist[i].string, line);
		    s_append ("\"", line);
		    }
		if (i+1 == (*sv).count)
		    lastitem = TRUE;
		parms_dspval (lastitem, margin, &row, line, &exitflag);
		if (exitflag) break;
		}
	    }
	}
    *row_num = row;			/* new row number		*/
    return;
    }

/*
 *	parms_dspval - display a line of the valid list for parameter
 *		   summary, help detect page full condition and
 *		   display special indicator should more items
 *		   be in the valid list than those displayed.
 */

    FUNCTION static VOID parms_dspval(lastitem, margin, row_num, text,
    				      exitflag)

    BOOL		lastitem;	/* in: TRUE if last item in list  */
    FUNINT		margin;         /* in: column number to output to */
    COUNT		*row_num;	/* in/out: row number to write on */
    TEXT		text[];		/* in/out: output text line       */
    BOOL		*exitflag;	/* out: TRUE if display page full */

    {
    IMPORT COUNT	termlines;	/* num of lines on terminal screen */

    COUNT		row;

    row = *row_num;
    *exitflag = FALSE;
    if (row == termlines-H_TRAIL_SIZE-1) /* last avail line on screen ?    */
	{
	*exitflag = TRUE;
	if (!lastitem)			/* more items in valid list ?      */
	    s_append (", ...", text);	/* attach etc. indicator to line   */
	}
    if (margin != 1)			/* double display format ?         */
	text[0] = DIVIDE_LINE;		/* add division line               */
    put_line (row++, margin, text, FALSE);
    s_blank (text, 79);
    *row_num = row;			/* new row number		*/
    return;
    }


/*
 *	position. Position to text as specified in the page block.
 * 	NOTE:
 *		If include file posctx ( .incpos of pageblk) is NULL, we
 *		position in the mainfile. Else we open the include file from
 *		the mainfile posctx (.mainpos  of pageblk) and then position
 *		in the include file. We also reset the .inblock flag in
 *		dirblk so that the directive package would know how to handle
 *		the conditional directives encountered during read.
 */

    FUNCTION  static CODE  position(dirblk, pageblk, page)

    struct DIRBLK	*dirblk;	/* in/out: directive control block	*/
    struct PAGEBLK	pageblk[];	/* in: page control block		*/
    FUNINT		page;		/* in: the page to position to		*/


    {
    CODE		code;
    COUNT		i;


    if (page > MAX_HELP_PAGES)
	return (FAIL);			/* can not position there	*/
    i = page-1;				/* index to pageblk array	*/
    if (!pageblk[i].mainpos.possav)	/* NULL posctx in mainfile	*/
	return(FAIL);			/* page not read before		*/
    if (pageblk[i].incpos.possav)	/* page start in  include file 	*/
	{
	code = d_incopen(dirblk, &pageblk[i].mainpos);	/* open include file */
	if (code == D_ERRINC) return(code);
	f_setpos(&(*dirblk).incfile, &pageblk[i].incpos);  /* pos at page start */
	(*dirblk).incfile.posctx.possav = TRUE;		/* enable inc file   */
	}
    else
	{
	f_setpos((*dirblk).sfileptr, &pageblk[i].mainpos);	/* position in main file */
	(*dirblk).incfile.posctx.possav = FALSE;	/* disable inc file  */
	}
    (*dirblk).inblock = pageblk[i].inblock;		/* tell if inside block */
    return(SUCCESS);
    }

/*
 * pos_parm - position to the detailed help data on a parameter
 *
 * Assumes we're positioned before "LEVEL2" for the specified parameter
 *
 * returns: SUCCESS or  FAIL.
 *
 */

    FUNCTION static CODE pos_parm(dirblk, parmname, procname, subname,
				  fullparm, helpblk)

    struct DIRBLK	*dirblk;	/* In: directive control block		*/
    TEXT		parmname[];	/* In: parameter name, maybe abbrev	*/
    TEXT		procname[];	/* In: proc or command name		*/
    TEXT		subname[];	/* In: subcommand name			*/
    TEXT		fullparm[];	/* Out: full parameter name		*/
    struct  HELPBLK	*helpblk;	/* out: output block from help		*/

    {
    TEXT		dummy[STRINGSIZ+1];
    TEXT		str[STRINGSIZ+1];
    TEXT		dirctv[STRINGSIZ+1];
    TEXT		varname[STRINGSIZ+1];
    TEXT		scope[STRINGSIZ+1];
    TEXT		proc[FSPECSIZ+1];
    TEXT		subcmd[SUBCMDSIZ+1];
    struct SYNBLK	sb;			/* a syntax block		*/
    CODE		code;
    COUNT		sub_match;
    COUNT		priority;		/* measure of closeness in match
    						 * 1 = best match; 10 = no match */
    COUNT		base_pri;
    COUNT		matchpri;		/* priority of best match	*/
    struct POSCTX	matchctx;		/* context if a substring match	*/


    s_copy(parmname, fullparm);			/* assume exact match		*/
    matchpri = 10;
    sub_match = 0;
    code = d_search(dirblk, ".LEVEL2", dummy);		/* Find detailed help */
    if (code != SUCCESS)
	code = FAIL;
    else
	while (FOREVER)					/* find the parameter*/
	    {
	    if (d_dirctv(dirblk, dirctv, str)!=SUCCESS ||
	       !s_lseq(dirctv, ".VARIABLE"))
		{
		if (s_lseq (dirctv, ".SUBCMD"))		/* skip over SUBCMDs */
		    continue;			
    		if (sub_match==0)			/* if no substring match...*/
		    code = FAIL;
		break;
		}
	    priority = 10;
	    proc[0] = EOS;
	    subcmd[0] = EOS;
	    parserec(str, varname, scope);		/* separate fields in .VAR */
	    if (!NULLSTR(scope))
		cmd_parse(&sb, scope, NULL, proc, subcmd);
	    if (s_equal(varname, parmname)) 		/* exact match?		*/
		base_pri = 1;
    	    else if (s_lseq(parmname, varname))		/* substring match?	*/
		base_pri = 4;
	    else continue;

	    if (NULLSTR(scope))
		priority = base_pri + 2;
	    else if (NULLSTR(proc) || s_equal(procname, proc))
		{
		if (NULLSTR(subcmd))
		    priority = base_pri + 1;
		else if (!NULLSTR(subname) && s_lseq(subname, subcmd))
		    priority = base_pri;
		}
	    if (priority == 1)				/* exact match found	*/
		{
		s_copy(parmname, fullparm);
		return(SUCCESS);
		}
	    if (priority < 10)				/* a match found ?	*/
		{
    		sub_match++;
		if (priority < matchpri)
		    {					/* save the context	*/
		    matchpri = priority;
		    d_text(dirblk, dummy);		/* get position of next	*/
		    MOVE_STRUCT((*(*dirblk).sfileptr).posctx, matchctx);
		    s_bcopy(varname, fullparm, NAMESIZ);  /* full parm name	*/
		    }
    		}
	    }       		
    if (code == FAIL)
	{
	gen_hlperr(helpblk, "No help available for parameter '%s'.",
		  "TAE-NOPARHELP");
	return(FAIL);
        }

    if (matchpri >= 4 && sub_match > 1)
	{
	fullparm[0] = EOS;			/* no full name for parm */
	gen_hlperr(helpblk, "Ambiguous parameter abbreviation '%s'.",
		   "TAE-AMBIGPAR");
    	return(FAIL);
	}
    f_setpos((*dirblk).sfileptr, &matchctx);	/* assume a submatch...	*/
    return(SUCCESS);
    }

/*
 * pos_subcmd - position to the detailed help data on a subcommand
 *
 * Assumes we're positioned before "LEVEL2" for the specified subcommand
 *
 * returns: SUCCESS or  FAIL.
 *
 */

    FUNCTION static CODE pos_subcmd(dirblk, procname, type, subname,
    				    fullsub, helpblk)

    struct DIRBLK	*dirblk;	/* In: directive control block		*/
    TEXT		procname[];	/* In: proc or command name		*/
    TEXT		type[];		/* In: proc/command			*/
    TEXT		subname[];	/* In: subcommand name, maybe abbrev		*/
    TEXT		fullsub[];	/* Out: full subcommand name		*/
    struct  HELPBLK	*helpblk;	/* out: output block from help		*/

    {
    TEXT		dummy[STRINGSIZ+1];
    TEXT		str[STRINGSIZ+1];
    TEXT		subcmd[STRINGSIZ+1];
    CODE		code;
    COUNT		sub_match;
    struct POSCTX	matchctx;		/* context if a substring match	*/


    s_copy(subname, fullsub);			/* assume exact match		*/
    sub_match = 0;
    code = d_search(dirblk, ".LEVEL2", dummy);		/* Find detailed help */
    if (code != SUCCESS)
	code = FAIL;
    else
	while (FOREVER)					/* find the subcommand */
	    {
	    if (d_dirctv(dirblk, str, subcmd)!=SUCCESS ||
	       !s_lseq(str, ".SUBCMD"))
		{
		if (s_lseq (str, ".VARIABLE"))		/* skip over VARIABLEs */
		    continue;			
    		if (sub_match==0)			/* if no substring match...*/
		    code = FAIL;
		break;
		}
	    if (s_equal(subcmd, subname)) 		/* exact match?		*/
    		return(SUCCESS);
    	    if (s_lseq(subname, subcmd))		/* substring match?	*/
    		{					/* save the context	*/
    		sub_match++;
    		d_text(dirblk, dummy);			/* get position of next	*/
    		MOVE_STRUCT((*(*dirblk).sfileptr).posctx, matchctx);
    		s_bcopy(subcmd, fullsub, SUBCMDSIZ);	/* full subcommand name	*/
    		}
	    }       		
    if (code == FAIL)
	{
	s_copy("No help available for subcommand '%s' of ", dummy);
	s_append(type, dummy);
	s_append(" '", dummy);
	s_append(procname, dummy);
	s_append("'.", dummy);
	gen_hlperr(helpblk, dummy, "TAE-NOSUBHELP");
	return(FAIL);
        }

    if (sub_match > 1)
	{
	fullsub[0] = EOS;			/* no full name for subcmd */
	gen_hlperr(helpblk, "Ambiguous subcommand abbreviation '%s'.",
		   "TAE-AMBIGSUB");
    	return(FAIL);
	}
    f_setpos((*dirblk).sfileptr, &matchctx);	/* assume a submatch...	*/
    return(SUCCESS);
    }

/*
 *	proc_help  - process and display an opened help file
 *
 *	Assumes we're positioned anywhere before .TITLE and that .TITLE
 *	precedes .HELP.  If help is on a compiled PDF, assumes we're
 *	positioned to read the CP_HEADER record next.
 *
 *	NOTE: (*helpblk).compiled tells us whether this is a compiled PDF.
 *
 *	Returns: SUCCESS  or  FAIL.	
 *
 */

    FUNCTION CODE proc_help(fctx, curlib, libname, procname, subname,
    			    parmname, type, helpblk)

    struct   SFILE	*fctx;		/* In: file context block for opened file			*/
    TEXT  		curlib[];	/* In: library name of opened file	*/
    TEXT		libname[];	/* In: library name for LHS of header	 */
    TEXT		procname[];	/* In: proc name for LHS of help display */
    TEXT		subname[];	/* In: name of subcommand, or NULL if none	*/
    TEXT		parmname[];	/* In/out: name of parm, or NULL if none	*/
    TEXT		type[];		/* In: proc/menu/command/global	*/
    struct  HELPBLK	*helpblk;	/* out: help output control block    */

    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics    */
    struct DIRBLK	dirblk;		/* directive control block	     */
    struct TXTSTOR	title;		/* context for dynamically stored title text */
    TEXT		str[STRINGSIZ+1];
    CODE		code;
    TEXT		dummy[STRINGSIZ+1];
    TEXT		lefthead[STRINGSIZ+1];
    TEXT		fullname[SUBCMDSIZ+1];	/* name, not abbreviated	*/
    TEXT		key[KEYSIZ+1];
    TEXT		msg[STRINGSIZ+1];

    lefthead[0] = EOS;
    d_init(&dirblk, fctx, curlib, (TEXT **)(*char_gbl).v_cvp,
	(*char_gbl).v_count);    		/* init directive block		*/
    if ((*helpblk).compiled)			/* if PDF is compiled		*/
	{
	if ((code = prep_c_help(&dirblk, &title, msg, key)) != SUCCESS)	/* prepare*/
    	    {
	    gen_hlperr(helpblk, msg, key);	/* save gettitle error msg	*/
    	    return(FAIL);
	    }
	str[0] = EOS;
	}
    else					/* PDF is not compiled		*/
	{
	code = d_dirctv(&dirblk, str, msg);
	if (s_equal(str, ".TITLE"))
	    {				/* read the title into dynamic store			*/
	    if ((code = gettitle(&dirblk, &title, msg, key))!= SUCCESS)
		{
		gen_hlperr(helpblk, msg, key);	/* save gettitle error msg	*/
		return(FAIL);
		}
	    code = d_dirctv(&dirblk, str, dummy);
	    }
	else				/* no title */
	    title.numline = 0;
	}

    code = SUCCESS;					/* success for help-parm	*/
    							/*  and help-global */
    if (!NULLSTR(libname))				/* if library given	*/
	{						/* help on proc/cmd	*/
	left_fbld(libname, procname, "", type, lefthead);	/* build proc name string*/
	if ( !(s_equal(str, ".HELP")))
    	    code = d_search(&dirblk, ".HELP", msg);
    	}
    if (code == SUCCESS)
	{
	if (!NULLSTR(parmname))				/* help on parm or global	*/
	    {
	    code = pos_parm(&dirblk, parmname, procname, subname,
		    fullname, helpblk);			/* position to the parameter		*/
	    if (!NULLSTR(fullname))			/* if full name known	*/
		{
		s_copy(fullname, parmname);		/* copy it to  caller	*/
		s_copy(fullname, (*helpblk).helpobj);	/* also for error	*/
		}
	    if (s_equal(type, "global"))
		left_gbld(procname, fullname, type, lefthead); /* build global help header */
	    else
		left_pbld(procname, subname, fullname, "proc",
			lefthead); 			/* build param help header */
	    fretxt(&title);				/* force null title	*/
	    }
	else if (!NULLSTR(subname))			/* help on subcommand	*/
	    {
	    code = pos_subcmd(&dirblk, procname, type, subname,
			      fullname, helpblk); 	/* position to the subcommand */
	    if (!NULLSTR(fullname))
		{
		s_copy(fullname, subname);		/* copy it to caller	*/
		s_copy(fullname, (*helpblk).helpobj);	/* also for error	*/
		}
	    left_sbld(procname, fullname, type, lefthead);
	    }
	if (code == SUCCESS)
	    code = helper(&dirblk, type, lefthead, &title, helpblk);
	}
    else
	{
	code = FAIL;
	s_copy(procname, (*helpblk).helpobj);
	gen_hlperr(helpblk, "No help available on '%s'.", "TAE-NOHELP");
	}
    if (title.tp != NULL) fretxt(&title);		/* free the dyamically-stored title	*/
    return(code);
    }

/*  put_line - Put a help line.  Generalized routine to output a line of
 *	       help text.  Used only by those routines that cannot assume
 *	       anything about their environment.  Our choices for output
 *	       are standard output and terminal (even if standard output is
 *	       not the terminal).  If output is terminal we must decide
 *	       between scrolling and directed-screen output.
 */

    FUNCTION  static VOID  put_line(line, col, text, blank)

    FUNINT	line;			/* IN: line number to write to */
    FUNINT	col;			/* IN: column number to write to */
    TEXT	text[];			/* IN: message text */
    FUNINT	blank;			/* IN: True if rest to be blanked */

    {
    IMPORT  CODE   termtype;		/* terminal type */
    IMPORT  BOOL   full_scr_help;	/* TRUE if we want full-screen	     */
    IMPORT  BOOL   help_to_term;	/* TRUE if we must force to terminal */

    if (full_scr_help)	
	{
	if (blank) t_lclear(line, 1);		/* write a blank line */
        t_output(line, col, text);		/* output with no cleanup */
	}
    else if (help_to_term)
	t_write(text, T_STDCC);			/* write with carriage return */
    else
    	put_stdout(text);
    return;
    }



#else
/*
 *	Test program for helper
 *
 ******	WARNING: THIS TEST PROGRAM IS NOW OBSOLETE AND SHOULD BE DELETED.*****
 *
 */
    GLOBAL COUNT	termtype, termlines, termcols;

    FUNCTION main ()

    {
    IMPORT TEXT 	vrsion[];
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics	*/

    TEXT		string[STRINGSIZ+1];
    TINY		i;
    COUNT		count;
    struct TXTSTOR	title;			/* for title text pointer	*/
    struct DIRBLK	dirblk;			/* directive control block	*/
    TEXT		directive[STRINGSIZ+1];
    TEXT		dummy[STRINGSIZ+1];
    TEXT		filename[STRINGSIZ+1];
    struct SFILE	sfile;
    CODE 		code;

    printf(" Helper test, version %s\n", vrsion);
    t_init(&termlines, &termcols, &termtype);
    while (FOREVER)
    	{
    	t_write(" Enter a file name> ", T_PROMPT);
    	t_read(filename, &dummy);
    	code = f_open(&sfile, 1, "", filename, HLP_TYPE, F_READ);
    	if (code != SUCCESS)
    	    {
    	    t_write(" TEST: unable to open file", T_DOUBLE);
    	    continue;
    	    }
    	d_init(&dirblk, &sfile, "", (TEXT **)(*char_gbl).v_cvp,
	    (*char_gbl).v_count);    		/* init directive block	*/
    	code = d_dirctv(&dirblk, directive, dummy);
    	if (s_equal(directive, ".TITLE"))
    	    {
    	    if (code = gettitle(&dirblk, &title, dummy, dummy) != SUCCESS)
    		{
    		t_write(" TEST: problem with gettitle access", T_DOUBLE);
    		continue;
    		}
    	    code = d_dirctv(&dirblk, directive, dummy);
    	    }
    	else
    	    title.numline = 0;
    	if (s_equal(directive, ".HELP"))
    	    code = helper(&dirblk, "PROC",  &title, dummy, NEXTSIZ+1);
    	}
    }
#endif
