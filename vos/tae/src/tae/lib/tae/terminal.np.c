/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* Change log:
 *	24-jan-84	Suppress echoing line-feed on CRT t_input...dm
 *	11-apr-84	Fix t_highlight bug...dm
 *	11-apr-84	Support for editors (t_gettok, etc)...palm
 *			The problem here is to recognize the GOLD and
 *			arrow keys from termcap; presently, we require
 *			that arrow/GOLD keys are sequences beginning
 *			with ESCAPE--terminals like regent25 will
 *			not work with the TAE editors.
 *	18-apr-84	new t_count...palm
 *	18-apr-84	Make t_read character oriented like t_input
 *			so escape sequences can be detected...palm
 *	22-may-84	Fix escape-sequence-lost problem...dm
 *	12-dec-86	Fix lint warnings...peb
 *	17-jan-87	In t_read, don't echo '\n' if the termination was
 *			T_ESCAPE; this avoids a strange scroll when hitting
 *			up-arrow...palm
 *	11-Nov-87	Added XrInput loop for facelift in read_terminal...gcc
 *	20-nov-87	Added check_event argument to read_terminal()
 *			and new routones t_read_event(), t_input_event()...dm
 *	25-jan-88	Add detection of PF2 and PF3...palm
 *	06-feb-88	PR1329: UNIX highlighting...ljn
 *	21-mar-88	PR999: t_pos takes negative line numbers...ljn
 *	20-apr-88	Changed XEVENT_CHECK to XWINDOWS...ljn
 *	13-may-88	New t_pinit (process init) that does same as t_init
 *			except does not re-initialize the screen, the thinking
 *			being that if TM already has the screen in the proper
 *			state, a process may not want to reset, which for
 *			xterm means "clear"...palm
 *	23-jun-88	Include chartype.inc...tpl
 *	29-jun-88	Change X include file names for X11R2...tpl
 *	10-nov-88	Modify for SYSV...ljn
 *	01-dec-88	Use tgoto() in t_pos() to format hcursor...ljn	
 ***********************************************************************
 *
 *	04-jun-89	Remove Xr dependency, 
 *			Convert to new Wpt architecture...dm
 *
 ***********************************************************************
 *	07-feb-90	PR 292: t_init should return lines and columns equal 0
 *			if type equals T_NOTTERM...ljn
 *	27-jun-90	Removed Facelift code...ljn
 */

/*>>UNIX<<	*/
#include <stdio.h>
#include <signal.h>
#include "taeconf.inp"

#ifdef macII
#define SYSV
#endif

#ifdef SYSV
#include <sys/termio.h>
#define TIOCGETP TCGETA
#define TIOCSETP TCSETA
#else
#include <sgtty.h>
#endif

#include "terminc.inc"
#include "eventinc.inp"
#ifdef TERMINAL_TOL
#include "fileinc.inp"
#endif
#include "chartype.inc"


#ifndef testmain
    GLOBAL int vterm13 = 0;


/*
 *	BSD/UNIX-dependent terminal I/O package for C callers.
 *	
 *	This package is used for I/O only when the caller knows that
 *	the session is interactive.  The t_init call returns such
 *	information.
 *
 *	As a formality, the functions are programmed to return a CODE,
 *	SUCCESS or FAIL;  it is not expected that callers will check
 *	the CODE because (a) the extra logic required and (b) there
 *	is no one to report the error to.
 *
 *	For CRT-dependent functions, the BSD termcap/termlib 
 *	capabilities are used.  Any terminal with an entry in termcap
 *	for cm, ce, and cl may be used here.  In addition, the TAE
 *	editor will work if (a) termcap entries exist for ku, kd, kl,
 *	kr, and k1 (GOLD) and (b) the character sequences for these
 *	keys begin with escape.
 *	
 *	Under BSD_UNIX, if standard output is the terminal, the I/O
 *	is line buffered, which implies that no data is written to the
 * 	terminal until a CR character is encountered, or the output buffer
 *	is flushed. This problem is avoided by calling setbuf to declare
 *	the standard output as completely unbuffered before any other
 *	I/O is performed.
 */

/*	static data:		*/

#ifdef TERMINAL_TOL
    static BOOL		term_log = FALSE; /* TRUE if we're logging output */
    struct SFILE	tsf;		/* SFILE for output log		*/
#define TOL_LUN		1		/* dummy: not used in UNIX	*/
#endif    


    static long		ltype    = 0;	/* (local) terminal type	*/
    static long		lcols	 = 0;	/* (local) columns		*/
    static long		llines	 = 0;	/* (local) lines		*/
    static TEXT		*line_clear;	/* line clear string		*/
    static TEXT		*screen_clear;	/* screen clear strng		*/
    static TEXT		*cursor;	/* cursor movement string	*/
    static TEXT		*hcursor;      	/* horizontal cursor string	*/
    static TEXT		*creturn;	/* carriage return	  	*/
    static TEXT		*stand_out1;	/* highlighting			*/
    static TEXT		*stand_out2;	/* highlighting			*/
    static TEXT		control[250];	/* control strings		*/
#ifdef SYSV
    static struct termio init_char;  	/* initial characteristics	*/
    static struct termio single_char;   /* single-character mode        */
#else
    static struct sgttyb init_char;  	/* initial characteristics	*/
    static struct sgttyb single_char;   /* single-character mode        */
#endif
    static LONG		up_count = 0;	/* screen update counter	*/

    GLOBAL	TEXT	PC;		/* needed by termlib		*/
    GLOBAL	TEXT	*BC;		/* backspace string		*/
    static	TEXT    bs_string[] = "\010";
    GLOBAL	TEXT	*UP;		/* move cursor up one line	*/
    GLOBAL	short   ospeed;		/* terminal speed		*/

    IMPORT TEXT	*tgetstr(), *tgoto();
    static VOID t_outc();
    static VOID read_terminal();
    
    static struct ECB dummy;
    static struct ECB *next_ecb = &dummy;
    static BOOL	esc_code = NULL ;
    static struct ESCSEQ 		/* escape sequences:		*/
        {
        TEXT	*strptr;		/* pointer to esc string	*/
        TEXT	*name;			/* termcap name of string	*/
        CODE	code;			/* t_gettok return code		*/
    	}  escseq[] =
            {
	    NULL, "k1", T_GOLD,
	    NULL, "k2", T_F2,
	    NULL, "k3", T_F3,
	    NULL, "k4", T_F4,
	    NULL, "kl", T_LEFT,
	    NULL, "kd", T_DOWN,
	    NULL, "kr", T_RIGHT,
	    NULL, "ku", T_UP
	    };
    static int n_escseq = sizeof(escseq)/sizeof(struct ESCSEQ);
    static COUNT max_esc;		/* max length of an esc sequence */


/*	t_pinit.    Process Init.
 *
 *	This is t_init for processes where you don't want the
 *	terminal to be reset, the thinking being that TM has
 *	already done the initialization.    Reset on some
 *	terminals (e.g., xterm) involves clearing the screen,
 *	not always a good thing for a process to do.
 *
 */

    static BOOL processInit = FALSE ;

    FUNCTION CODE t_pinit (lines, columns, type)

    COUNT	*lines;			/* out: number of lines on crt	*/
    COUNT	*columns;		/* out: number of columns 	*/
    CODE	*type;			/* out: type of terminal:	*/
    					/*	T_CRT			*/
    					/* 	T_NOTCRT		*/
    					/* 	T_NOTTERM		*/

    {
    CODE	retcode;

    processInit = TRUE;
    retcode = t_init (lines, columns, type);
    processInit = FALSE;
    return (retcode);
    }

/*
 *	t_init.		Initialize terminal package.
 *
 *	Return codes: SUCCESS or FAIL.
 *
 *	Note on 'type' argument: if T_CRT, then the terminal package CRT
 *	entries can be used (t_pos, t_input, t_output, t_clear). If T_NOTTERM
 *	then the current process does not have a terminal associated with 
 *	it (i.e., not interactive).  T_NOTCRT means that there is a terminal
 *	but it cannot be used for direct cursor positioning.
 */

    FUNCTION CODE t_init (lines, columns, type)

    COUNT	*lines;			/* out: number of lines on crt	*/
    COUNT	*columns;		/* out: number of columns 	*/
    CODE	*type;			/* out: type of terminal:	*/
    					/*	T_CRT			*/
    					/* 	T_NOTCRT		*/
    					/* 	T_NOTTERM		*/

    {
    TEXT	buffer[1024];
    TEXT	*area;
    TEXT	*ptr;
    TEXT	*getenv();
    TEXT	*term_name;
    int		i, l;
    CODE	code;

    if (ltype != 0)			/* only init one time		*/
	{
	*type = ltype;
	*lines = llines;
	*columns = lcols;
	return (SUCCESS);
	}

#ifdef TERMINAL_TOL
    	term_log = FALSE;		/* be sure we don't try to log	*/
#endif

    if (!isatty(1))			/* is stdout a terminal?	*/
        {
	*lines = llines = 0;
	*columns = lcols = 0;
	*type = ltype = T_NOTTERM;
        setvbuf(stdout, NULL, _IOLBF, 0);	/* set line buffering */
	return (SUCCESS);
	}
    setbuf(stdout, NULL);		/* unbuffer terminal output	*/
    ioctl(1, TIOCGETP, &init_char);	/* get initial characteristics	*/
    MOVE_STRUCT(init_char, single_char);/* copy characteristics         */  
#ifdef SYSV
    single_char.c_lflag = init_char.c_lflag & ~(ICANON | ECHO);
    single_char.c_cc[VEOF] = 1;
    single_char.c_cc[VEOL] = 1;
    ospeed = init_char.c_cflag & CBAUD;	/* set speed for termlib	*/
#else
    single_char.sg_flags = 
	(init_char.sg_flags | CBREAK) & ~ECHO;	/* for single char mode */
    ospeed = init_char.sg_ospeed;	/* set speed for termlib	*/
#endif
    *lines = llines = 24;		/* defaults in case unknown	*/
    *columns = lcols = 80;
    term_name = getenv ("TERM");
    if (term_name == NULL)
        goto unknown;
    code = tgetent(buffer, term_name);
    if (code != 1)
        goto unknown;			/* TERM not found in termcap	*/
    llines = tgetnum("li");
    if (llines == -1)
        llines = 24;
    lcols = tgetnum("co");
    if (lcols == -1)
        lcols = 80;
    *lines = llines;
    *columns = lcols;
    area = control;
    ptr = tgetstr("pc", &area);
    PC = (ptr == NULL) ? 0 : *ptr;	/* set for termlib use		*/
    UP = tgetstr("up", &area);		/* set for termlib use		*/
    if (tgetflag("bs"))
	BC = bs_string;			/* use CONTROL-H for backspace	*/
    else
	BC = tgetstr("bc", &area);	/* special backspace		*/
    cursor = tgetstr("cm", &area);
    if (cursor == NULL)
        goto unknown;
    creturn = tgetstr("cr", &area);
    hcursor = tgetstr("ch", &area);
    line_clear = tgetstr("ce", &area);
    screen_clear = tgetstr("cl", &area);
    if (screen_clear == NULL)
        goto unknown;

    stand_out1 = tgetstr("so", &area);	/* highlight on			*/
    stand_out2 = tgetstr("se", &area);	/* highlight off		*/
				/* see t_highlight for explanation	*/
    stand_out1 = isdigit(*stand_out1) ? stand_out1 + 1 : stand_out1;
    stand_out2 = isdigit(*stand_out2) ? stand_out2 + 1 : stand_out2;

    max_esc = 0;
    for (i=0; i < n_escseq; i++)	/* get escape sequences		*/
        {
	escseq[i].strptr = tgetstr (escseq[i].name, &area);
	if (escseq[i].strptr != NULL)
	    {
	    l = s_length (escseq[i].strptr);
	    if (l > max_esc) max_esc = l;
	    }
	}
    *type = ltype = T_CRT;		/* has everything TAE needs	*/
    ptr = tgetstr ("is", &area);	
    if (ptr != NULL && !processInit) 
	tputs (ptr, 1, t_outc);		/* init terminal	*/
    ptr = tgetstr ("ks", &area);
    if (ptr != NULL && !processInit) 
	tputs (ptr, 1, t_outc);		/* init keypad		*/
    return (SUCCESS);    

unknown:
    *type = ltype = T_NOTCRT;		/* if unknown, then unsupported */
    return (SUCCESS);
    }

/*	Event maniuplation for operator attention:
 *
 *
 *	t_signal.   Signal catcher for operator attention (usually 
 *	the delete key).   (This should never be called by a t_ user.)
 */

    FUNCTION VOID t_signal ()
    {
    (*next_ecb).flag = TRUE;		/* set event			*/
    next_ecb = &dummy;			/* reset for next signal	*/
    signal(SIGINT, t_signal);		/* re-enable so next delete will*/
    					/* not kill the process	        */
    return;
    }
/*
 *	t_attn.   Enable for next SIGINT signal (usually delete key).
 *	Must call here for each time you want the signal enabled.
 */

    FUNCTION CODE t_attn(ecb)
    struct ECB *ecb;		/* in/out: ecb to enable	*/

    {
    (*ecb).flag = FALSE;	/* clear event			*/
    next_ecb = ecb;		/* tell t_signal which ECB to set*/
    signal(SIGINT, t_signal);	/* enable signal catcher	*/
    return (SUCCESS);
    }
/* 
 *	e_occur.    Returns true if event has occurred.   You cannot
 *	wait on operator attention, you can only poll it.  The
 *	process control function ("process") has special UNIX logic
 *	to simulate waiting on operator attention.
 */
    FUNCTION BOOL e_occur(ecb)
    struct ECB *ecb;
    {
    return ((*ecb).flag);
    }

/*	t_highlight.   Surround string with terminal sequence for
 *	highlighting.   Returns number of non-printable characters.
 */

    FUNCTION COUNT t_highlight(string)

    TEXT	string[];		/* in/out: string to highlight	*/

    {
    TEXT local[STRINGSIZ+1];

/*
 *	t_highlight has a conceptual problem in unix:  the escape
 *	sequences in the termcap file have numeric prefixes indicating
 *	the number of milliseconds delay required for highlighting.
 *	Such delays are normally interpreted by tputs, for example
 *	see t_clear.   With the TAE highlight concept--i.e., surrounding
 *	a string with escape sequence and letting the user 
 *	subsequently write the string with t_write/t_output--we
 *	never have the opportunity to call tputs to interpret the
 *	delays.
 *
 *	Our approach is to strip the highlighting escape
 *	sequence of this delay element during the initialization
 *	phase. See t_init() above.
 */
    if (stand_out1 == NULL || stand_out2 == NULL)
	return(0);
    s_copy (string, local);		/* local copy 		*/
    s_copy(stand_out1, string);	
    s_append(local, string);
    s_append(stand_out2, string);

    return (s_length(stand_out1) + s_length(stand_out2));
    }

/*
 *	t_clear.  Clear screen of CRT.
 *
 */

    FUNCTION CODE t_clear()

    {
    if (ltype == T_CRT) 
	{
	up_count++;
        tputs (screen_clear, 1, t_outc);
	}
#ifdef TERMINAL_TOL
    if (term_log) f_write(&tsf, "%!CLR");	/* log term output	*/
#endif
    return(SUCCESS);
    }

/*	t_outc.  Send one character, for tputs use.
 */
    FUNCTION static VOID t_outc(chr)
    
    FUNINT	chr;
    {
    putc(chr, stdout);
    return;
    }

/* 
 *	t_lclear.  Clear line starting with given column number.
 *	A negative line number means use the current line.
 */

    FUNCTION CODE t_lclear(line, col)

    FUNINT	line;		/* in: line to clear (-1 = current line)*/
    FUNINT	col;		/* in: and start clearing at this col	*/

    {
    TEXT	string[STRINGSIZ+1];

    if (ltype == T_CRT)
	{
        up_count++;
	t_pos (line, col);			/* position		*/
	if (line_clear == NULL)	
	    {					/* no hardware line clr */
	    s_blank (string, lcols-col+1);	/* make blank string	*/
	    t_write (string, T_NULL);
	    t_pos (line, col);			/* re-position		*/
	    }
	else
            tputs (line_clear, 1, t_outc);	/* efficient form	*/
	}
    return(SUCCESS);
    }

/*
 *	t_read.  Read line from terminal.  CRT not required.
 *	(If it is a CRT, we read from current position.)
 */	
	
    FUNCTION CODE t_read (string, term)

    FAST TEXT	string[STRINGSIZ+1];	/* out: string read		*/
    CODE	*term;			/* out: terminator used:	*/
    					/*      T_CR (carriage return)	*/
    					/*      T_ESCAPE (escape)	*/
    {
    read_terminal (string, term, FALSE);  /* read line with editing	*/
    if (*term == T_CR)
	putc ('\n', stdout);		/* echo terminator		*/
    return (SUCCESS);
    }




/*
 *	t_input.   Read line from CRT.
 */

    FUNCTION CODE t_input (line, column, string, term)

    FUNINT	line;			/* in: line number (start at 1)   */
    FUNINT	column;			/* in: column number (start at 1) */
    TEXT	string[STRINGSIZ+1];	/* out: string typed		*/
    CODE	*term;			/* out: terminator: 		*/
    					/* T_CR or T_ESCAPE		*/
    {

    t_pos (line, column);
    read_terminal (string, term, FALSE);
    return (SUCCESS);
    }

/*	read_terminal.    This function goes character-by-character
 *	because (a) we want escape to terminate the read and
 *	(b) when reading from last line of a CRT, we do not want
 *	the newline to be echoed--it would scroll the display up
 *	one character. 
 *
 *	Going character-by-character means we must do our own
 *	echoing and 'editing' (kill, erase, ...).
 *
 *	NOTE: the deletion of the TAB character does not work
 *	properly.
 *
 *	If check_event argument is set to TRUE, the terminal is added as a 
 *	channel to Wpt input devices. The input is received as a Wpt event.
 */
    
    FUNCTION static VOID read_terminal (string, term, check_event)

    TEXT	string[];		/* out: string read		  */
    CODE	*term;			/* out: terminator		  */
    FUNINT	check_event;		/* in: True if to check Wpt events */
 
    {
    FAST int		c;
    FAST TEXT	*last_char;	
    TEXT	*start_char;

    static TEXT rubout[] = "\b \b";	/* "bs, space, bs" 		*/

    last_char = string + STRINGSIZ - 1;
    start_char = string;	
    ioctl(1, TIOCSETP, &single_char);		/* set: single mode	*/

    while (1)
	{
	c = getc(stdin);
	*string = EOS;
#ifdef SYSV
	if (c == init_char.c_cc[VERASE])	/* if erase character	   */
#else	
	if (c == init_char.sg_erase) 		/* if erase character	   */
#endif
	    {
	    if (string <= start_char)		/* nothing to delete 	   */
		continue;
    	    fputs (rubout, stdout);		/* erase character	   */
	    string--;				/* decrement length	   */
	    }
#ifdef SYSV
	else if (c == init_char.c_cc[VKILL])
#else
	else if (c == init_char.sg_kill )
#endif
	    {
            if (ltype == T_CRT)			
		for(; string > start_char; string--)
		    fputs (rubout, stdout);	/* erase character on CRT */
    	    else
    		putc ('\n', stdout);		/* next line for hardcopy */
    	    string = start_char;		/* start again		  */
	    }
	else if (c == EOF || c == '\033')	/* escape 		   */
	    {
	    *term = T_ESCAPE;
	    break;
	    }
	else if (c == '\n' || string > last_char)   /* new-line or no room */
	    {
	    *term = T_CR;
	    break;
	    }
	else					/* normal text		   */
	    {
	    putc(c, stdout);			/* echo back		   */
	    *string++ = c;
	    }
	}
    if (c == '\033')			/* if escape, wait till complete */
	esc_code = get_toke(TRUE);	/* save the code		 */

    ioctl(1, TIOCSETP, &init_char);		/* reset to normal mode    */
    return (SUCCESS);
    }    

/*
 *	t_bell.  Ring bell on terminal.
 */

    FUNCTION CODE t_bell ()

    {
    fputs ("\007", stdout);
#ifdef TERMINAL_TOL
    if (term_log) f_write(&tsf, "%!BELL");	/* log term output	*/
#endif
    return;
    }

/*
 *	t_output.  Write to specific position of CRT.
 *	If any carriage control needed, it must be in the string.
 */

    FUNCTION CODE t_output (line, column, string)

    FUNINT	line;			/* in: line number (start at 1)   */
    FUNINT	column;			/* in: column number (start at 1) */
    TEXT	string[];		/* in: string to write		  */

    {
    CODE code;

    t_pos(line, column);		/* position			*/
    code = t_write(string, T_NULL);	/* write with no cc		*/
    return (code);
    }

/*
 *	t_write.  Write line to terminal.  CRT not required.
 */

    FUNCTION CODE t_write (string, cc)

    TEXT	string[];		/* in: string to write		*/
    FUNINT	cc;			/* in: carriage control:	*/
    					/*     T_STDCC (standard)	*/
    					/*     T_NULL  (none)		*/
    					/*     T_PROMPT 		*/
    					/*     T_DOUBLE		   	*/
    {
    
    up_count++;
    fputs (string, stdout);
    if (cc == T_STDCC)
        putc('\n', stdout);
    else if (cc == T_DOUBLE)
        {
        putc('\n', stdout);
        putc('\n', stdout);
        }
#ifdef TERMINAL_TOL
    if (term_log) f_write(&tsf, string);	/* log term output	*/
#endif
    return (SUCCESS);        
    }

/*
 *	t_pos.	Position cursor on CRT.
 *	A negative line number means to position cursor on current line.
 *
 *	NOTE: It is not understood why the tgoto() call for hcursor needs
 *	to have column and row arguments reversed from those of cursor.
 */

    FUNCTION CODE t_pos (line, column)
    
    FUNINT	line;			/* in: line number (start at 1)   */
					/*  -1 means use current line	  */
    FUNINT	column;			/* in: column number (start at 1) */

    {
    FAST TEXT  *cursor_string;
#ifdef TERMINAL_TOL
    TEXT	outstr[100];		/* formatted string for term. log */
#endif 
    
    if (ltype == T_CRT) 
	{
	if (line < 0)
	    {
	    if (hcursor == NULL || creturn == NULL)
		{
		line = llines;			/* go to screen bottom	*/
	    	cursor_string = tgoto(cursor, column-1, line-1);
	    	tputs(cursor_string, 1, t_outc);
		}
	    else				/* use most recent line	*/
	    	{
	    	tputs(creturn, 1, t_outc);
		if (column > 1)
		    {
	    	    cursor_string = tgoto(hcursor, 0, column-1);
	    	    tputs(cursor_string, 1, t_outc);
		    }
		}
	    }
	else
	    {
	    cursor_string = tgoto(cursor, column-1, line-1);
	    tputs(cursor_string, 1, t_outc);
	    }
	}
#ifdef TERMINAL_TOL
    if (term_log)
	{
	if (line < 0) line = 0;		/* pick some reasonable number	*/
	sprintf(outstr, "%%POS%02d%02d",line,column); /* format for %POSLLCC */
	f_write(&tsf, outstr);
	}
#endif
    return (SUCCESS);
    }    

/*	t_getpos.    Get terminal position.
 *
 *	In UNIX, this does nothing because TERMCAP/termlib does not seem
 *	to support getting cursor position.  The UNIX/TAE editor still
 *	works reasonably because of the way that line number is passed to
 *	the editor function cmd_editor.
 */
    FUNCTION VOID t_getpos (line, column)

    COUNT	*line;		/* out: line number	*/
    COUNT	*column;	/* out: column number	*/

    {
    return;
    }


/*	t_gettok.   Get next keyboard token and no echo.
 *		
 *		Return CODE:
 *
 *			T_UNKNOWN -- some strange escape sequence
 *			T_UP	-- up arrow
 *			T_DOWN    -- down arrow
 *			T_RIGHT   -- right arrow
 *			T_LEFT    -- left arrow
 *			T_GOLD    -- gold (PF1) key
 *			T_2ESC -- double escape (by user)
 *			other   -- an ascii character
 */

    FUNCTION CODE t_gettok ()

    {
    FAST int	code;

    ioctl (1, TIOCSETP, &single_char);
    code = get_toke (FALSE);
    ioctl (1, TIOCSETP, &init_char);
    return (code);
    }

/*
 *	t_1gettok.   This is same as gettok, but we assume
 *	that the escape of the escape sequence has already
 *	been read.  This is a kludge to allow the efficiency
 *	of doing normal terminal reads using t_read/t_in with
 *	escape as terminator.
 *
 */

    FUNCTION CODE t_1gettok ()

    {
    FAST int code;

    ioctl (1, TIOCSETP, &single_char);		/* set single mode	*/
    code = get_toke (TRUE);
    ioctl (1, TIOCSETP, &init_char);		/* reset		*/
    return (code);
    }
/*
 *
 *	get_toke.   Local function to do the work for t_gettok/t_1gettok.
 */
    FUNCTION static CODE get_toke (esc_gone)

    BOOL	esc_gone;		/* in: TRUE if escape alread read */

    {
    TEXT	string[STRINGSIZ+1];
    CODE	code;
    COUNT	p, i;

    if (esc_code != NULL)
	{
	code = esc_code;
	esc_code = NULL;		/* reset			*/
	return(code);			/* return saved code		*/
	}
    if (!esc_gone)			/* if escape not already read	*/
	{
	code = getc(stdin);
	if (code != '\033')
	    return (code);		/* not an escape sequence	*/
	}
    string[0] = '\033';				/* start with 1 escape	*/
    for (p=1; p <= max_esc; p++)
        {
	string[p] = getc(stdin);
	string[p+1] = EOS;			/* terminate string	*/
	if (s_equal(string, "\033\033"))	/* double escape	*/
	    return (T_2ESC);
	for (i=0; i < n_escseq; i++)		/* look for a match	*/
	    if (escseq[i].strptr != NULL && 
		s_equal (escseq[i].strptr, string))
	        return (escseq[i].code);
        }
    return (T_UNKNOWN);				/* unknown escape seq	*/
    }    

#ifdef TERMINAL_TOL
/*
 *	t_entol  Enable terminal output logging.
 *
 *	returns FAIL if logging already enabled, a CODE and an error message
 *	on an open error
 */
    FUNCTION CODE t_entol (fspec, errmsg)

    TEXT	*fspec;		/* in: file spec for file to log to 	*/
    TEXT	**errmsg;	/* out: pointer to errmsg		*/

    {
    CODE	code;

    *errmsg = NULL;
    if (term_log) return(FAIL);	/* already logging		*/
    code = f_opnspc(&tsf, TOL_LUN, fspec, "", "", "TOL", F_WRITE);
    if (code == SUCCESS) 
    	term_log = TRUE;
    else
    	*errmsg = tsf.errmsg;		/* return host error message	*/
    return(code);
    }

/*
 *	t_distol  Disable terminal output logging.
 */
    FUNCTION CODE t_distol ()


    {
    CODE	code;

    if (!term_log) return(SUCCESS);	/* weren't logging		*/
    term_log = FALSE;
    code = f_close(&tsf, F_KEEP);
    return(code);
    }
#endif

/*
 *	t_count. Return screen update counter.
 *	
 *	NOTE:	The screen is updated every time t_clear, t_lclear,
 *		t_write or t_output is called.
 */

    FUNCTION  LONG  t_count()

    {
    return(up_count);
    }	


#else

    main ()	/* test case */

    {
    struct ECB ecb;
    COUNT lines, columns;
    CODE type, term, code;
    TEXT string[STRINGSIZ+1];
    TEXT	*str;

    t_init(&lines, &columns, &type);
    t_clear();
    printf("lines, columns, type = %d, %d, %d\n",
	   lines, columns, type);
    t_output(10, 30, "line 10, column 30");
    t_write("\nnon-CRT write to next line\n", T_STDCC);
    t_input(20, 10, string, &term);
    printf("Terminator = %s\n",  (term == T_CR) ? "(return)" : "(escape)");
    t_highlight (string);
    t_output(22, 10, string);	/* echo */
    t_lclear (22, 15);		/* clear original except 5 character */
    t_write ("hit left, right, up, down, GOLD, or any key");
    code = t_gettok ();
    if (code == T_LEFT) str = "LEFT";
    else if (code == T_RIGHT) str = "RIGHT";
    else if (code == T_UP) str = "UP";
    else if (code == T_DOWN) str = "DOWN";
    else if (code == T_GOLD) str = "GOLD";
    else if (code == T_UNKNOWN) str = "UNKNOWN";
    else 
        {
        string[0] = code;
        string[1] = EOS;
        str = string;
        }    
    t_write (str, T_STDCC);	
    for (;;)
	{
	t_bell();		/* bell every time controlc hit */
        t_write(" ", T_STDCC);	/* flush			*/
        t_attn(&ecb);
	while (!e_occur(&ecb))  /* loop till event		*/
	    ;
	}    
    exit ();
    }
#endif
