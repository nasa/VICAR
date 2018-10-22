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
 *	Micro-editor.   This allows the user to edit a line before
 *	sending it to TAE with Carriage Return.
 *
 *	TBD: handling of tabs correctly.
 *	TBD: a line longer than 80 from t_read not handled correctly.
 *
 *	CHANGE LOG:
 *	11-apr-84	new calling sequence for UNIX portability...palm
 *
 *			The problem attacked here is that termcap/termlib
 *			does not have facilities for reading cursor
 *			position.  So in UNIX, we assume that the
 *			line number can be passed to the editor;  this
 *			works ok in all cases except in command mode
 *			where the caller does not know the line number.
 *
 *			In UNIX command mode, cmd_editor cannot obtain
 *			the current line number, so we repeat the
 *			prompt and the response on the last line for
 *			editing.
 *
 *			Note that in UNIX, t_getpos does nothing--the
 *			line number passed by caller is un-changed.
 * 	30-apr-84	Mode flag in editor calling sequence for UNIX
 *			portability...dm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *	16-jun-84	New PF2 to cycle valid strings...palm
 *	09-jul-84	Update to be in sync with UNIX changes...dm
 *	14-sep-84	Add check for index beyond max count of parm...lia
 *	01-dec-84	TCL 67: Use TUTCTX instead of EDTCTX...peb
 *	04-apr-85	VMS V4 editor compatibility...palm
 *	30-apr-85	Fix cursor position after CR in command mode
 *			on a 'retrieved' command...palm
 *	03-may-85	Fix pf2 crash referencing outside of v_count...palm
 *	08-may-85	Avoid screen scroll with t_edit up/down arrows...palm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	19-sep-85	PR 1012: Fixed bld_response so it doesn't crash TM when
 *			tutoring on NAME parm w/o value via up/down arrow...dab
 *	12-nov-86	Added TAE Facelift...krw
 *	27-mar-87	Safety check: if current value not valid, don't crash
 *			in valid_cycle....palm
 *	28-mar-87	PF3 for cycle valids backwards...palm
 *	18-may-87	Ported fix in cmd_editor (shows up under UNIX only): 
 *			to invoke t_input() for screen mode...ljn
 *	07-aug-87	Add new menu_editor...palm
 *      21-sep-87       Move high_off out of nonfacelift restriction...tpl
 *	19-nov-87	Call t_read_event() to receive x events while 
 *			waiting for input...dm
 *      03-feb-88       Changed DisplayId to XFACELIFT
 *	21-mar-88	Uknown terminal line number passed to t_output()
 *			as negative number...ljn
 *	05-apr-88	PR1203: When looking for delete key, allow ^H...ljn
 *	27-jul-88	Removed X Window stuff...ljn
 *	13-jun-89	Removed TAE_FACELIFT...ljn
 *      23-mar-90       ifdef xfacelift in call to t_edit...tpl/dm
 *	27-jun-90	Remove Facelift code...ljn
 *      11-sep-92 PR875 Save real value and displayed value separately...tpl 
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"terminc.inc"	/* terminal package			*/
#include 	"tminc.inc"	/* TM structures			*/
#include 	"tmhost.inp"	/* Host specific definitions		*/
#include	"menuinc.inc"	/* menu structures			*/
#include "taeintproto.h"


FUNCTION static struct VARIABLE *previous 
(
    struct SYMTAB *symtab,	/* in: symbol table		*/
    struct VARIABLE *vcur	/* in: current VARIABLE		*/

 );
FUNCTION static COUNT valid_cycle 
(
    FUNINT	maxl,		/* in: max line size		*/
    TEXT	outline[],	/* out: reponse line		*/
    TEXT	line[],		/* out: reponse line		*/
    BOOL	forward	/* in: TRUE if cycle forward.	*/
				/*     FALSE for backwards	*/

 );
FUNCTION static BOOL visible 
(
    struct VARIABLE	*v,		/* in: variable of interest	*/
    FUNINT		index,		/* in: variable's index		*/
    FUNINT		page		/* in: current page number	*/

 );
FUNCTION static VOID menu_fetch_up 
(        
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	line[]		/* out: response line	*/

	 );
FUNCTION static VOID menu_fetch_down 
(
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	line[]		/* out: response line	*/

 );
FUNCTION static VOID menu_high_on (void);
FUNCTION static VOID menu_high_off (void);

FUNCTION static CODE fetch_up 
(        
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	outline[],	/* out: response line	*/
    TEXT	line[]		/* out: response line	*/

	 );
FUNCTION static CODE fetch_down 
(
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	outline[],		/* out: response line	*/
    TEXT	line[]		/* out: response line	*/

 );
FUNCTION static COUNT bld_response 
(
    struct VARIABLE *v,		/* in: TCL parameter to display		*/
    FUNINT	index,		/* in: value index			*/
    FUNINT	maxl,		/* in: max size of response line	*/
    FUNINT	vindex,		/* in: if positive, the S_VALID index	*/
				/* to use for obtaining a value		*/
    TEXT	line[],		/* out: restricted response line        */
    TEXT	rline[]	/* out: unrestricted response line 	*/

 );
FUNCTION static VOID high_on(void);
FUNCTION static VOID move_down(BOOL);
FUNCTION static VOID high_off(void);



/*	cmd_editor.   Command mode micro-editor.
 *
 *	Return code:
 *	 	SUCCESS -- line obtained successfully
 *		FAIL -- user killed command with double escape
 *	
 *	The special control sequences honored are:
 *                                              
 *		UP arrow: get most recent command
 *		DOWN arrow: down one from UP
 *		RIGHT arrow: move right one character
 *		LEFT arrow: move left one character
 *		GOLD/any-other: do extreme of the key
 *		CONTROL/U: clear the line
 *		CONTROL/X: clear the line
 *		CR: enter the line
 */

FUNCTION CODE cmd_editor 
(
    CODE	archive,	/* in: A_NONE or A_LASTCMD		*/
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: prompt string			*/
    COUNT	line_num,	/* in: line number on screen		*/
				/*     (-1 if line number not known)	*/
    FUNINT	screen		/* TRUE if in screen mode		*/

 )
    {
    IMPORT struct VARIABLE *last_gbl;
    IMPORT COUNT 	termcols;
    IMPORT CODE 	termtype;

    CODE		terminator;
    COUNT		index, lm, c, maxl, i, j, l;
    COUNT		count, column;
    TEXT		tmpline[STRINGSIZ+1];
    FAST BOOL		gold_active, gold;
    FAST CODE		keytoke;	

#ifdef VAX_VMS
    IMPORT BOOL tt_editing;
    if (tt_editing)		/* is "SET TERM/LINE_EDITING" active	*/
	goto edit_with_vms;
#endif
    lm = s_copy (prompt, tmpline) + 1;	/* copy prompt, set left margin */
    t_highlight (tmpline);		/* highlight prompt		*/


    if (screen)
	{
	t_output (line_num, 1, tmpline);	/* prompt for screen	*/
	t_input (line_num, lm, line, &terminator);
	}
    else
	{
        t_write (tmpline, T_PROMPT);		/* scrolling prompt	*/
        t_read (line, &terminator);		/* read line 		*/
	}
    if (terminator == T_CR)
	return (SUCCESS);	/* line read efficiently with no editing*/
    if (termtype != T_CRT)
        return (T_2ESC);	/* not a CRT, so kill the command	*/
    gold_active = FALSE;
    count = (*last_gbl).v_count;
    maxl = termcols - lm ;	/* maximum chars that can be read	*/
    line [maxl] = EOS;		/* clip line if too long		*/
    index = -1;			/* current $LASTCMD index		*/
    l = s_length(line);		/* current number of chars in line	*/
    keytoke = t_1gettok();	/* get first token (escape already came)*/
    t_getpos (&line_num, &column);	/* get line nr (unless UNIX)	*/
    if (line_num < 0)			/* if line nr still not known	*/
	{				/* stay on the most recent line	*/
	i = s_copy (prompt, tmpline);
	i += t_highlight (tmpline);	/* add highlighting		*/
	s_copy (line, &tmpline[i]); 	/* and the line typed so far	*/
	t_output (line_num, 1, tmpline);
	}
    i = l;				/* current index in line buffer	*/
    for (; keytoke != TERM_RETCHAR; keytoke=t_gettok())
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
        if (keytoke == '\177'
		|| keytoke == '\010')	/* DELETE key or Ctrl-H		*/
	    {
	    if (i == 0)			/* already at left?	*/
		{
		/* t_bell(); */
		continue;
		}
	    if (gold)
		c = i;				/* number of chars to delet */
	    else
		c = 1;		
	    for (j=i-c; j+c <= l; j++)		/* delete c chars to left */
		line[j] = line[j+c];		
	    s_copy (line, tmpline);		/* temp copy of line	*/
	    for (j=l-c; j < l; j++)		/* add spaces for...	*/
		tmpline[j] = ' ';		/* each char deleted	*/
	    tmpline[l] = EOS;
	    l = l - c;				/* new line length	*/
	    i = i - c;				/* new current positon	*/
	    t_output (line_num, lm+i, &tmpline[i]);	/* re-paint line*/
	    t_pos (line_num, lm + i);		/* position to current	*/
    	    }
	else if (keytoke == '\025' || keytoke == '\030')
	    {					/* CONTROL/U or		*/
	    i = 0;				/* CONTROL/X		*/
	    l = 0;
	    line[0] = EOS;
	    t_lclear (line_num, lm);	
	    }
	else if (keytoke < T_SPECIAL)		/* normal key?		*/
	    {
	    if (keytoke < ' ' && keytoke != '\t')
		continue;			/* ignore control chars */
    	    if (keytoke == '\t')
    		keytoke = ' ';			/* tab: crude approach  */
	    if (l >= maxl)			
	        {
	        t_bell(); 			/* too many chars 	*/
	    	continue;
	    	}
	    for (j=l; j > i; j--)		/* move chars over 	*/
	        line[j] = line[j-1];
	    line[i] = keytoke;			/* insert new char	*/
	    line[++l] = EOS;			/* new length and EOS	*/
	    t_write (&line[i], T_NULL);		/* re-paint right side	*/
	    i++;				/* move to next char	*/
	    t_pos (line_num, lm + i);		/* position to next	*/
	    }	
	else if (keytoke == T_LEFT)
	    {
	    if (i == 0)
	        ; /* t_bell(); */		/* too far left		*/
	    else if (gold)			/* GOLD/LEFT		*/
	        {
		t_pos (line_num, lm);		/* position to first	*/
		i = 0;
		}
	    else				/* normal LEFT		*/
		{
		i--;				/* move left one char	*/
		t_pos(line_num, lm + i);
		}
	    }
        else if (keytoke == T_RIGHT)
	    {
	    if (i >= l)
	        ; /* t_bell(); */		/* too far right	*/
	    else if (gold)			/* GOLD/RIGHT		*/
	        {
		i = l;
		t_pos (line_num, lm + i);
		}
	    else				/* RIGHT		*/
		{
		i++;
		t_pos (line_num, lm + i);
		}
	    }
	else if (keytoke == T_UP)
	    {
	    if (archive != A_LASTCMD || count <= 0)  /* $LASTCMD ok?	*/
	        {
	        /* t_bell(); */			/* if not, error	*/
		continue;
		}
	    if (gold)				/* GOLD/UP		*/
		index = count-1;		/* goto oldest cmd	*/
	    else
	        {
	        if (index+1 < count)		/* if more, then...	*/
	            index++;			/* next oldest cmd	*/
	        }
	    s_bcopy (SVAL(*last_gbl, index), line, maxl);
	    t_output (line_num, lm, line);
	    l = s_length(line);
	    i = l;
	    t_lclear (line_num, lm + l);	/* clear rest of line	*/
	    }
	else if (keytoke == T_DOWN)
	    {
	    if (archive != A_LASTCMD || count <= 0)  /* $LASTCMD ok?	*/
	        {
	        /* t_bell(); */
	        continue;
	        }
	    if (gold)	
		index = 0;			/* get most recent cmd	*/
	    else if (index < 0)			/* starting up?		*/
		continue;			/* already down 	*/
	    else
		{
		if (index > 0)			/* if room to move...	*/
		    index--;			/* move down one	*/
		}
	    s_bcopy(SVAL(*last_gbl, index), line, maxl);
	    t_output (line_num, lm, line);
	    l = s_length(line);
	    i = l;
	    t_lclear (line_num, lm + l);	/* clear rest of line	*/
	    }	
	else if (keytoke == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (keytoke == T_2ESC)		/* double escape?	*/
	    {
	    line[0]=0;				/* formality		*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
        else
	    ; /* t_bell(); */			/* unknown token	*/
        }
    tmpline[0] = TERM_RETCHAR;
    tmpline[1] = '\0';
    t_write (tmpline, T_NULL);			/* echo CR		*/
    return (SUCCESS);

/*	This code make use of the VMS line editor and is executed
 *	when "SET TERM/LINE_EDIT" is active.
 */
#ifdef VAX_VMS
edit_with_vms:					/* cmd_editor:		*/
    line[0] = EOS;				/* start edit with null	*/
    lm = s_length (prompt) + 1;			/* left margin		*/
    code = t_edit(prompt, line, screen);
    if (code  == T_CR)
	return (SUCCESS);
    t_getpos (&line_num, &column);		/* get line nr 		*/
    maxl = termcols - lm ;			/* max chars to read	*/
    gold_active = FALSE;
    count = (*last_gbl).v_count;		/* nr of $LASTCMDs	*/
    index = -1;					/* $LASTCMD index	*/
    do
        {
        t_lclear (line_num, 1);		/* clear line if CRT		*/
	line[0] = EOS;			/* start new line		*/
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
	if (code == T_2ESC)
	    return (FAIL);
	else if (code == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (code == T_UP)
	    {
	    if (archive != A_LASTCMD || count <= 0)  /* $LASTCMD ok?	*/
		continue;
	    if (gold)				/* GOLD/UP		*/
		index = count-1;		/* goto oldest cmd	*/
	    else
	        {
	        if (index+1 < count)		/* if more, then...	*/
	            index++;			/* next oldest cmd	*/
	        }
	    s_bcopy (SVAL(*last_gbl, index), line, maxl);
	    }
	else if (code == T_DOWN)
	    {
	    if (archive != A_LASTCMD || count <= 0)  /* $LASTCMD ok?	*/
	        continue;
	    if (gold)	
		index = 0;			/* get most recent cmd	*/
	    else if (index < 0)			/* starting up?		*/
		continue;			/* already down 	*/
	    else
		{
		if (index > 0)			/* if room to move...	*/
		    index--;			/* move down one	*/
		}
	    s_bcopy(SVAL(*last_gbl, index), line, maxl);
	    }	
	}
        while ((code = vms_edit1 (prompt, line, screen)) != T_CR);
    return(SUCCESS);

#endif
    }
#ifdef VAX_VMS
FUNCTION CODE vms_edit1 
(
    TEXT 	prompt[],	/* in: prompt string			*/
    TEXT	line[],		/* out: user-typed line			*/
    FUNINT	screen		/* TRUE if in screen mode		*/

 )
    {
    CODE code;

    code = t_edit1 (prompt, line, screen);
    return(code);
    }
#endif

/************************************************************************/
/* SEE TUTOR.CNP for the TAE_FACELIFT version of the following routines */
/************************************************************************/
/*	tut_editor.   Tutor mode micro-editor.
 *
 *	Return code:
 *	 	SUCCESS -- line obtained successfully
 *		FAIL -- user killed command with double escape
 *	
 *	The special control sequences honored are:
 *
 *		UP arrow: get most recent variable
 *		DOWN arrow: down one variable
 *		RIGHT arrow: move right one character
 *		LEFT arrow: move left one character
 *		GOLD/any-other: do extreme of the key
 *		PF2: cycle current VALID display
 *		CONTROL/U: clear the line
 *		CONTROL/X: clear the line
 *		CR: enter the line
 */

FUNCTION CODE tut_editor 
(
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: characters in prompt string	*/
    COUNT	line_num	/* in: line number			*/

 )
    {
    IMPORT COUNT 	 termcols;
    IMPORT struct TUTCTX tutctx;

    CODE	code, terminator;
    COUNT	lm, c, maxl, i, j, l;
    TEXT	tmpline[STRINGSIZ+1];
    TEXT	outline[STRINGSIZ+1];
    FAST BOOL	gold_active, gold;
    FAST CODE	keytoke;	

#ifdef VAX_VMS
    IMPORT BOOL tt_editing;
    if (tt_editing)		/* is "SET TERM/LINE_EDITING" active	*/
	goto edit_with_vms;
#endif
    lm = s_copy (prompt, tmpline) + 1;	/* copy prompt, set left margin */
    t_highlight (tmpline);		/* highlight prompt		*/
    t_output (line_num, 1, tmpline);			/* prompt	*/
    maxl = termcols - lm ;		/* max chars that can be read	*/
    if (tutctx.edtcmd)			/* "call back" after ...	*/
        {				/* generating a SHOW command?	*/
	tutctx.edtcmd = FALSE;	
	i = l = bld_response (tutctx.vcur, tutctx.index, maxl, -1,outline,line);
        high_on ();			/* highlight current value	*/
	t_output (line_num, lm, outline);
	t_lclear (line_num, lm + l);
	keytoke = t_gettok();		/* get first keyboard token	*/
	}
    else				/* first call for this line	*/
	{
	t_input (line_num, lm, line, &terminator);
	if (terminator == T_CR)
	    return (SUCCESS);		/* line read without editing	*/
	line [maxl] = EOS;		/* clip line if too long	*/
	keytoke = t_1gettok ();		/* get remainder of esc sequence*/
	i = l = s_length (line);
	}
    gold_active = FALSE;
    for (; keytoke != TERM_RETCHAR; keytoke=t_gettok())
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
        if (keytoke == '\177'
		|| keytoke == '\010')	/* DELETE key or Ctrl-H		*/
	    {
	    if (i == 0)				/* already at left	*/
		{
		/* t_bell(); */
		continue;
		}
	    if (gold)				/* GOLD/DELETE:		*/
    		{
		j = s_index (line, '=');	/* find first '='	*/
		if (j >= 0) 			/* if found		*/
		    c = (i > j+1) ? (i-j-1) : i;  /* delete to '='	*/
		else
		    c = i;			/* delete all: no '='	*/
		}
	    else
		c = 1;		
	    for (j=i-c; j+c <= l; j++)		/* delete c chars to left */
		line[j] = line[j+c];		
	    s_copy (line, tmpline);		/* temp copy of line	*/
	    for (j=l-c; j < l; j++)		/* add spaces for...	*/
		tmpline[j] = ' ';		/* each char deleted	*/
	    tmpline[l] = EOS;
	    l = l - c;				/* new line length	*/
	    i = i - c;				/* new current positon	*/
	    if (i == 0)				/* if now at left side	*/
		high_off();			/* turn highlight off	*/
	    t_output (line_num, lm+i, &tmpline[i]);	/* re-paint 	*/
	    t_pos (line_num, lm + i);		/* position to current	*/
    	    }
	else if (keytoke == '\025'  || keytoke == '\030')
	    {					/* CONTROL/U or		*/
	    i = 0;				/* CONTROL/X		*/
	    l = 0;
	    line[0] = EOS;
	    high_off();				/* turn off highlight	*/
	    t_lclear (line_num, lm);	
	    }
	else if (keytoke < T_SPECIAL)		/* normal key?		*/
	    {
	    if (keytoke < ' ' && keytoke != '\t')
		continue;			/* ignore control chars */
    	    if (keytoke == '\t')
    		keytoke = ' ';			/* tab: crude approach  */
	    if (l >= maxl)			
	        {
	        t_bell();			/* too many chars 	*/
	    	continue;
	    	}
	    for (j=l; j > i; j--)		/* move chars over 	*/
	        line[j] = line[j-1];
	    line[i] = keytoke;			/* insert new char	*/
	    line[++l] = EOS;			/* new length and EOS	*/
	    t_write (&line[i], T_NULL);		/* re-paint right side	*/
	    i++;				/* move to next char	*/
	    t_pos (line_num, lm + i);		/* position to next	*/
	    }	
	else if (keytoke == T_LEFT)
	    {
	    if (i == 0)
		continue;			/* already at far left	*/
	    else if (gold)			/* GOLD/LEFT		*/
	        {
		j = s_index (line, '=');	/* equals present?	*/
		if (j >= 0)
		    i = (i > j+1) ? j+1 : 0;	/* move to = if to right*/
		else
		    i = 0;			/* move to begin	*/
		}
	    else				/* normal LEFT		*/
		i--;				/* move left one char	*/
	    t_pos (line_num, lm+i);		/* new position		*/
	    }
        else if (keytoke == T_RIGHT)
	    {
	    if (i >= l)
	        ; /* t_bell(); */ 		/* too far right	*/
	    else if (gold)			/* GOLD/RIGHT		*/
	        {
		i = l;
		t_pos (line_num, lm + i);
		}
	    else				/* RIGHT		*/
		{
		i++;
		t_pos (line_num, lm + i);
		}
	    }
	else if (keytoke == T_UP)
	    {
	    if (tutctx.subtut)			/* ignore if subcmd	*/
	        continue;
            code = fetch_up (gold, maxl, outline, line);	/* get previous		*/
	    if (code != SUCCESS)
		return (SUCCESS);		/* must SHOW first	*/
	    t_output (line_num, lm, outline);
	    l = s_length (line);
	    i = l;				/* positio to end 	*/
	    t_lclear (line_num, lm + i);	/* clear rest of line	*/
	    }
	else if (keytoke == T_DOWN)
	    {
	    if (tutctx.subtut)			/* ignore if subcmd	*/
	        continue;
	    code = fetch_down (gold, maxl, outline, line);	/* get next	*/
	    if (code != SUCCESS)
		return (SUCCESS);		/* must SHOW first	*/
	    t_output (line_num, lm, outline);
	    l = s_length (line);
	    i = l;
	    t_lclear (line_num, lm + l);	/* clear rest of line	*/
	    }	
	else if (keytoke == T_F2  ||  keytoke == T_F3)	/* valid cycle	*/
	    {
	    if (tutctx.highact && (*tutctx.vcur).v_type == V_STRING &&
	       (*tutctx.vcur).v_valid != NULL)
	       {
	       i = l = valid_cycle (maxl, outline, line, keytoke==T_F2);
	       t_output (line_num, lm, outline);
	       t_lclear (line_num, lm+ l);
	       }
	    }
	else if (keytoke == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (keytoke == T_2ESC)		/* double escape?	 */
	    {
	    line[0]=0;				/* formality		*/
    	    high_off();				/* turn highlight off	*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
        else
	    ; /* t_bell(); */			/* unknown token	*/
        }
/*  tmpline[0] = TERM_RETCHAR;
 *  tmpline[1] = 0;
 *  t_write (tmpline, T_NULL);			**	echo CR		**
 */
    high_off();					/* turn highlight off	*/
    return (SUCCESS);

/*	This code make use of the VMS line editor and is executed
 *	when "SET TERM/LINE_EDIT" is active.
 */
#ifdef VAX_VMS
edit_with_vms:				/* tutor mode editor:		*/
    lm = s_length(prompt) + 1;		/* start column of response	*/
    maxl = termcols - lm ;		/* max chars that can be read	*/
    if (tutctx.edtcmd)			/* "call back" after ...	*/
        {				/* generating a SHOW command?	*/
	tutctx.edtcmd = FALSE;	
	i = l = bld_response (tutctx.vcur, tutctx.index,maxl,-1,outline,line);
        high_on ();			/* highlight current value	*/
	}
    else				/* first call for this line	*/
	line[0] = EOS;			/* start with no line		*/
    t_lclear (line_num, 1);		/* position/clear prompt line	*/
    gold_active = FALSE;
    while ((code = t_edit (prompt, outline, TRUE)) != T_CR)
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
	if (code == T_UP)
	    {
	    if (!tutctx.subtut)			/* if real tutor:	*/
		{
		code = fetch_up (gold, maxl, outline, line);/* get previous	*/
		if (code != SUCCESS)
		    return (SUCCESS);		/* must SHOW first*/
                }
	    }
	else if (code == T_DOWN)
	    {
	    if (!tutctx.subtut)			/* if real tutor	*/
    		{
		code = fetch_down (gold, maxl, outline, line);/* get next*/
		if (code != SUCCESS)
		    return (SUCCESS);		/* must SHOW first	*/
    		}
	    }	
	else if (code == T_F2  || code == T_F3)	/* valid cycle		*/
	    {
	    if (tutctx.highact && (*tutctx.vcur).v_type == V_STRING &&
	       (*tutctx.vcur).v_valid != NULL)
	       valid_cycle (maxl, outline, line, code==T_F2);
	    }
	else if (code == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (code == T_2ESC)		/* double escape?	 */
	    {
	    line[0]=0;				/* formality		*/
    	    high_off();				/* turn highlight off	*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
	t_lclear (line_num, 1);			/* clear prompt line	*/
	}
    high_off();
    return (SUCCESS);
#endif /* VAX_VMS */

    }

/*	fetch_up.   Get response for UP arrow in tutor.
 *
 *	Return:
 *		SUCCESS -- if up arrow handling completed
 *		FAIL -- Tutor command was generated and editing is
 *			temporarily finished until tut_editor re-called.
 */
FUNCTION static CODE fetch_up 
(        
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	outline[],	/* out: response line	*/
    TEXT	line[]		/* out: response line	*/

	 )
    {
    IMPORT struct TUTCTX tutctx;	/* tutor context for editor 	*/

    struct VARIABLE *v;
    struct CONTXT *pctx;

    high_off ();			/* turn current highlighting off*/
    pctx = tutctx.ctx;			/* proc context ptr		*/
    if ((*pctx).parmst.link == NULL)	/* if no parms, give user a...	*/
	{
	s_copy ("RUN", line);		/* RUN command			*/
	return (SUCCESS);
	}
    if (tutctx.start)
        tutctx.start = FALSE;			/* position already set	     */
    else
        {
	if (tutctx.index > 0  && gold)		/* if moving to top of .vcur */
	    tutctx.index = 0;
	else if (tutctx.index == 0) 		/* if moving to previous var */
	    {
	    tutctx.vcur = v = previous (&(*pctx).parmst, tutctx.vcur);
	    if ((*v).v_type == V_NAME  ||  gold)
		tutctx.index = 0;		/* gold or NAME: to top	*/
	    else
		tutctx.index = (*v).v_maxc - 1;	/* else to last element */
	    }
	else
	    tutctx.index--;			/* simple up in current parm */
        }
    if (!visible (tutctx.vcur, tutctx.index, tutctx.curpag))
	{
	tutctx.edtcmd = TRUE;			/* must display value	*/
	s_copy ("SHOW", line);
	return (FAIL);
	}
    bld_response (tutctx.vcur, tutctx.index, maxl, -1, outline,  line);
    high_on ();					/* turn highlight on	*/
    return (SUCCESS);
    }

/*	fetch_down.   Get response for DOWN arrow in tutor.
 *
 *	Return:
 *		SUCCESS -- response generated okay
 *		FAIL -- tutor cmd generated and must wait for call back
 */
FUNCTION static CODE fetch_down 
(
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	outline[],		/* out: response line	*/
    TEXT	line[]		/* out: response line	*/

 )
    {
    IMPORT struct TUTCTX tutctx;	/* tutor context		*/

    struct CONTXT *pctx;

    high_off ();			/* turn highlight off		*/
    pctx = tutctx.ctx;			/* proc context ptr		*/
    if ((*pctx).parmst.link == NULL)	/* if no parms, give him a...	*/
	{
	s_copy ("RUN", line);		/* RUN command			*/
	return (SUCCESS);
	}
    if (tutctx.start)
        tutctx.start = FALSE;		/* position already set		*/
    else
        {
	move_down (gold);		/* move down to next parm	*/
	if (!visible (tutctx.vcur, tutctx.index, tutctx.curpag))
	    {
	    tutctx.edtcmd = TRUE;	/* must make it visible		*/
	    s_copy ("SHOW", line);
	    return (FAIL);
	    }
        }
    bld_response (tutctx.vcur, tutctx.index, maxl, -1, outline, line);
    high_on ();				/* highlight current		*/
    return (SUCCESS);
    }

/*	bld_response.    Build a response line for tutor.
 *	The string length is returned as function value.
 *
 *	"variable = value" or
 *	"variable(index) = value"
 *
 */

FUNCTION static COUNT bld_response 
(
    struct VARIABLE *v,		/* in: TCL parameter to display		*/
    FUNINT	index,		/* in: value index			*/
    FUNINT	maxl,		/* in: max size of response line	*/
    FUNINT	vindex,		/* in: if positive, the S_VALID index	*/
				/* to use for obtaining a value		*/
    TEXT	line[],		/* out: restricted response line        */
    TEXT	rline[]	/* out: unrestricted response line 	*/

 )
    {
    struct VARIABLE *vref;
    struct S_VALID  *valid;
    COUNT	i;
    CODE	type;

    i = s_copy ((*v).v_name, rline);
    if ((*v).v_type != V_NAME  &&  (*v).v_maxc > 1)   /* if multi-valued */
        {
	rline[i++] = '(';
	s_i2s (index+1, &rline[i]);
	i = s_length(rline);
	rline[i++] = ')';
	}
    rline[i++] = '=' ;
    type = (*v).v_type;
    if (vindex >= 0)				       /* use S_VALID:	*/
        {
	valid = (struct S_VALID *)(*v).v_valid;
	s_copy ((*valid).slist[vindex].string, &rline[i]);
        addqu (&rline[i]);
	}	
    else if ( (type != V_NAME && index >= (*v).v_count) || 
	 (type == V_NAME && (*v).v_ref == NULL) ) /* NAME parm must have ref value */
        rline [i] = EOS;				/* no value	*/
    else if (type == V_INTEGER)
        s_i2s(IVAL(*v, index), &rline[i]);
    else if (type == V_REAL)
        s_r2s(RVAL(*v, index), &rline[i]);
    else if (type == V_STRING)
        {
        s_copy (SVAL(*v, index), &rline[i]);
        addqu (&rline[i]);				/* add quotes	*/
        }
    else if (type == V_NAME)
	{
	vref = (*v).v_ref;
	s_copy ((*vref).v_name, &rline[i]);	/* use name of reference */
	}
    if (s_length(rline) > maxl)
        {
        s_bcopy (rline, line, maxl);	/* indicate clipped	*/
        s_copy ("...\"", &line[maxl-4]);	/* indicate clipped	*/
        line[maxl] = EOS;			/* only for strings:	*/
	i = maxl;
        }
    else
	i = s_copy (rline, line );

    return (i);
    }

/*	previous.   Get previous variable name.
 *
 *	The previous may be NULL, if there are no parameters.
 */

FUNCTION static struct VARIABLE *previous 
(
    struct SYMTAB *symtab,	/* in: symbol table		*/
    struct VARIABLE *vcur	/* in: current VARIABLE		*/

 )
    {
    struct VARIABLE *v, *p;	/* working VARIABLE pointer	*/

    if (vcur == NULL)
	return (NULL);			/* safety check		*/
    if (vcur == (*symtab).link)		/* first variable?	*/
        {
	v = vcur;			/* search for last	*/
	while (1)
	    {
	    if ((*v).v_link == NULL)
		return (v);		/* last is previous to first */
	    v = (*v).v_link;
	    }
	}

    p = NULL;	
    for (v=(*symtab).link; v != NULL; v = (*v).v_link)
        {
	if (v == vcur)
	    return (p);
	p = v;			/* new previous			*/
	}	
    return (NULL);		/* safety catch for bad vcur	*/
    }

/*	high_on.	Turn on highlighting for current value.
 */
    FUNCTION static VOID high_on (void)

    {
    IMPORT struct TUTCTX tutctx;

    if (tutctx.vcur != NULL)		/* no PARMs protection     */
        {
	high_value (tutctx.vcur, tutctx.index, TRUE);
    	tutctx.highact = TRUE;
	tutctx.vindex = -1;		/* no valid cycle established yet */
    	}
    return ;
    }

/*	move_down.   Move current tutor position down.
 *	This updates the tutctx.vcur and tutctx.index variables.
 */
FUNCTION static VOID move_down 
(
    BOOL	gold		/* in: TRUE if moving way down	*/

 )
    {
    IMPORT struct TUTCTX tutctx;		/* editor context	*/

    struct CONTXT	*pctx;			/* current proc context	*/
    struct VARIABLE 	 *v;

    pctx = tutctx.ctx;			
    v = tutctx.vcur;				/* current position     */
    if (gold || 				/* move to new parm?	*/
        (*v).v_type == V_NAME  ||
        tutctx.index >= (*v).v_maxc - 1)
        {
	tutctx.vcur = v = (*v).v_link; 		/* get next parm	*/
    	if (v == NULL)
	    tutctx.vcur = v = (*pctx).parmst.link; /* wrap to beginning	*/
	tutctx.index = 0;
        }
    else					/* move to...		*/
        tutctx.index++;				/* next component	*/
    return;
    }

/*	visible.    TRUE if the variable and component is visible
 *	on the tutor screen.
 */

FUNCTION static BOOL visible 
(
    struct VARIABLE	*v,		/* in: variable of interest	*/
    FUNINT		index,		/* in: variable's index		*/
    FUNINT		page		/* in: current page number	*/

 )
    {
    struct TUTEXT 	*tp;

    tp = (*v).v_tp;		
    if (tp == NULL || page != (*tp).pagnum)		/* on another page?  */
        return (FALSE);			
    if (index+1 > (*tp).panbot ||  index+1 < (*tp).pantop) /* outside panel? */
        return (FALSE);
    return (TRUE);
    }


/*	valid_cycle.     Cycle thru valid values for a string.
 *
 *	Since PF2 means forward and PF3 means backwards, this
 *	is usually called with forward = "key==PF2".
 */

FUNCTION static COUNT valid_cycle 
(
    FUNINT	maxl,		/* in: max line size		*/
    TEXT	outline[],	/* out: reponse line		*/
    TEXT	line[],		/* out: reponse line		*/
    BOOL	forward	/* in: TRUE if cycle forward.	*/
				/*     FALSE for backwards	*/

 )
    {
    IMPORT struct TUTCTX tutctx;
    COUNT	vindex;
    struct S_VALID *valid;
    struct VARIABLE *v;

    v = tutctx.vcur;			/* local convenience copy */
    valid = (struct S_VALID *) (*v).v_valid;
    if (tutctx.vindex < 0)
	{				/* establish VALID position: */
	if (tutctx.index < (*v).v_count)
	    {				/* find current value in VALID list */
	    tutctx.vindex = -1;		/* in case not found: start at begin*/
	    for (vindex = 0; vindex < (*valid).count; vindex++)
		{
		if (s_equal ((*valid).slist[vindex].string,
	    	             SVAL(*v, tutctx.index)))
		    {
	    	    tutctx.vindex = vindex; 	/* set current valid index */
		    break;
		    }
		}	
	    }
	else
	    tutctx.vindex = -1;  	/* no value: start at beginning */
        }
    if (forward)
    	tutctx.vindex =
	    (tutctx.vindex >= (*valid).count-1) ? 0 : tutctx.vindex+1;
    else
	tutctx.vindex = 
	    (tutctx.vindex <= 0) ? (*valid).count-1 : tutctx.vindex-1;	
    return (bld_response (v, tutctx.index, maxl, tutctx.vindex,outline, line));
    }

/************************************************************************/
/*	menu_editor.   Menu mode micro-editor.
 *
 *	Return code:
 *	 	SUCCESS -- line obtained successfully
 *		FAIL -- user killed command with double escape
 *	
 *	The special control sequences honored are:
 *
 *		UP arrow: get most recent entry
 *		DOWN arrow: down one entry 
 *		RIGHT arrow: move right one character
 *		LEFT arrow: move left one character
 *		GOLD/any-other: do extreme of the key
 *		CONTROL/U: clear the line
 *		CONTROL/X: clear the line
 *		CR: execute the currently displayed command
 */

FUNCTION CODE menu_editor 
(
    TEXT	line[],		/* out: user-typed line			*/
    TEXT 	prompt[],	/* in: characters in prompt string	*/
    COUNT	line_num	/* in: line number			*/

 )
    {
    IMPORT COUNT 	 termcols;

    CODE	terminator;
    COUNT	lm, c, maxl, i, j, l;
    TEXT	tmpline[STRINGSIZ+1];
    FAST BOOL	gold_active, gold;
    FAST CODE	keytoke;	

#ifdef VAX_VMS
    IMPORT BOOL tt_editing;
    if (tt_editing)		/* is "SET TERM/LINE_EDITING" active	*/
	goto edit_with_vms;
#endif
    lm = s_copy (prompt, tmpline) + 1;	/* copy prompt, set left margin */
    t_highlight (tmpline);		/* highlight prompt		*/
    t_output (line_num, 1, tmpline);			/* prompt	*/
    maxl = termcols - lm ;		/* max chars that can be read	*/
    t_input (line_num, lm, line, &terminator);
    if (terminator == T_CR)
	return (SUCCESS);		/* line read without editing	*/
    line [maxl] = EOS;			/* clip line if too long	*/
    keytoke = t_1gettok ();		/* get remainder of esc sequence*/
    i = l = s_length (line);
    gold_active = FALSE;
    for (; keytoke != TERM_RETCHAR; keytoke=t_gettok())
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
        if (keytoke == '\177'	
		|| keytoke == '\010')	/* DELETE key or Ctrl-H		*/
	    {
	    if (i == 0)				/* already at left	*/
		{
		/* t_bell(); */
		continue;
		}
	    if (gold)				/* GOLD/DELETE:		*/
		c = i;				/* delete whole line	*/
	    else
		c = 1;				/* delete one char	*/
	    for (j=i-c; j+c <= l; j++)		/* delete c chars to left */
		line[j] = line[j+c];		
	    s_copy (line, tmpline);		/* temp copy of line	*/
	    for (j=l-c; j < l; j++)		/* add spaces for...	*/
		tmpline[j] = ' ';		/* each char deleted	*/
	    tmpline[l] = EOS;
	    l = l - c;				/* new line length	*/
	    i = i - c;				/* new current positon	*/
	    if (i == 0)				/* if now at left side	*/
		menu_high_off();		/* turn highlight off	*/
	    t_output (line_num, lm+i, &tmpline[i]);	/* re-paint 	*/
	    t_pos (line_num, lm + i);		/* position to current	*/
    	    }
	else if (keytoke == '\025'  || keytoke == '\030')
	    {					/* CONTROL/U or		*/
	    i = 0;				/* CONTROL/X		*/
	    l = 0;
	    line[0] = EOS;
	    menu_high_off();			/* turn off highlight	*/
	    t_lclear (line_num, lm);	
	    }
	else if (keytoke < T_SPECIAL)		/* normal key?		*/
	    {
	    if (keytoke < ' ' && keytoke != '\t')
		continue;			/* ignore control chars */
    	    if (keytoke == '\t')
    		keytoke = ' ';			/* tab: crude approach  */
	    if (l >= maxl)			
	        {
	        t_bell();			/* too many chars 	*/
	    	continue;
	    	}
	    for (j=l; j > i; j--)		/* move chars over 	*/
	        line[j] = line[j-1];
	    line[i] = keytoke;			/* insert new char	*/
	    line[++l] = EOS;			/* new length and EOS	*/
	    t_write (&line[i], T_NULL);		/* re-paint right side	*/
	    i++;				/* move to next char	*/
	    t_pos (line_num, lm + i);		/* position to next	*/
	    }	
	else if (keytoke == T_LEFT)
	    {
	    if (i == 0)
		continue;			/* already at far left	*/
	    else if (gold)			/* GOLD/LEFT		*/
		i = 0;				/* move to begin	*/
	    else				/* normal LEFT		*/
		i--;				/* move left one char	*/
	    t_pos (line_num, lm+i);		/* new position		*/
	    }
        else if (keytoke == T_RIGHT)
	    {
	    if (i >= l)
	        ; /* t_bell(); */ 		/* too far right	*/
	    else if (gold)			/* GOLD/RIGHT		*/
	        {
		i = l;
		t_pos (line_num, lm + i);
		}
	    else				/* RIGHT		*/
		{
		i++;
		t_pos (line_num, lm + i);
		}
	    }
	else if (keytoke == T_UP)
	    {
            menu_fetch_up (gold, maxl, line);		/* get previous	*/
	    t_output (line_num, lm, line);
	    l = s_length (line);
	    i = l;				/* positio to end 	*/
	    t_lclear (line_num, lm + i);	/* clear rest of line	*/
	    }
	else if (keytoke == T_DOWN)
	    {
	    menu_fetch_down (gold, maxl, line);		/* get next ent */
	    t_output (line_num, lm, line);
	    l = s_length (line);
	    i = l;
	    t_lclear (line_num, lm + l);	/* clear rest of line	*/
	    }	
	else if (keytoke == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (keytoke == T_2ESC)		/* double escape?	 */
	    {
	    line[0]=0;				/* formality		*/
    	    menu_high_off();			/* turn highlight off	*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
        else
	    ; /* t_bell(); */			/* unknown token	*/
        }
    menu_high_off();				/* turn highlight off	*/
    return (SUCCESS);

/*	This code make use of the VMS line editor and is executed
 *	when "SET TERM/LINE_EDIT" is active.
 */
#ifdef VAX_VMS
edit_with_vms:				/* tutor mode editor:		*/
    lm = s_length(prompt) + 1;		/* start column of response	*/
    maxl = termcols - lm ;		/* max chars that can be read	*/
    line[0] = EOS;			/* start with no line		*/
    t_lclear (line_num, 1);		/* position/clear prompt line	*/
    gold_active = FALSE;
    while ((code = t_edit (prompt, line, TRUE)) != T_CR)
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
	if (code == T_UP)
	    menu_fetch_up (gold, maxl, line);		/* get previous	*/
	else if (code == T_DOWN)
	    menu_fetch_down (gold, maxl, line);		/* get next	*/
	else if (code == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (code == T_2ESC)		/* double escape?	 */
	    {
	    line[0]=0;				/* formality		*/
    	    high_off();				/* turn highlight off	*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
	t_lclear (line_num, 1);			/* clear prompt line	*/
	}
    menu_high_off();
    return (SUCCESS);
#endif /* VAX_VMS */
    }

/*	menu_fetch_up.   Get response for UP arrow in menu. 
 *
 */
FUNCTION static VOID menu_fetch_up 
(        
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	line[]		/* out: response line	*/

	 )
    {
    IMPORT struct CURMEN  *Curmenu;		/* menu context 	     */

    menu_high_off ();				/* turn off highlighting     */
    line[0] = EOS;
    if (Curmenu == NULL || (*Curmenu).nent <= 0)/* safety check		     */
        return ;		
    if (gold)					/* move to first entry?	     */
	(*Curmenu).index = 0;
    else if ((*Curmenu).index <= 0)		/* wrap to last ?	     */
	(*Curmenu).index = (*Curmenu).nent - 1;
    else 
        (*Curmenu).index--;			/* move back one	     */
    sprintf (line, "%d", (*Curmenu).index+1);	/* build response	     */
    menu_high_on ();				/* turn highlighting on      */
    return ;
    }


/*	menu_fetch_down.   Get response for DOWN arrow in menu.
 *
 */
FUNCTION static VOID menu_fetch_down 
(
    BOOL	gold,		/* in: TRUE if GOLD/UP	*/
    FUNINT	maxl,		/* in: max line length	*/
    TEXT	line[]		/* out: response line	*/

 )
    {
    IMPORT struct CURMEN *Curmenu;		/* menu context		*/


    menu_high_off ();				/* turn highlight off	*/
    line[0] = EOS;
    if (Curmenu == NULL || (*Curmenu).nent <= 0)/* safety check		*/
        return ;		
    if (gold)	
        (*Curmenu).index = (*Curmenu).nent-1;	/* move to bottom	*/
    else if ((*Curmenu).index < 0 || (*Curmenu).index >= (*Curmenu).nent - 1)
	(*Curmenu).index = 0;			/* wrap to top	        */
    else
        (*Curmenu).index++;			/* move down one	*/
    sprintf (line, "%d", (*Curmenu).index+1);
    menu_high_on ();				/* highlight current	*/
    return ;
    }

/*	menu_high_on and menu_high_off.
 *
 *	Turn menu entry highlight on/off
 */

    FUNCTION static VOID menu_high_on (void)

    {
    IMPORT struct CURMEN *Curmenu; 
    TEXT	line[STRINGSIZ+1];

    
    if (Curmenu == NULL ||
	(*Curmenu).nent <= 0 || 
	(*Curmenu).index < 0 || 
	(*Curmenu).index >= (*Curmenu).nent)
        return;						/* safety check */
    s_copy ((*Curmenu).entryline[(*Curmenu).index], line);
    t_highlight (line);
    t_output ((*Curmenu).linenr[(*Curmenu).index], 1, line);
    return;
    }



    FUNCTION static VOID menu_high_off (void)

    {
    IMPORT struct CURMEN *Curmenu; 

    if (Curmenu == NULL ||
	(*Curmenu).nent <= 0 || 
	(*Curmenu).index < 0 || 
	(*Curmenu).index >= (*Curmenu).nent)
        return;						/* safety check */
    t_output ((*Curmenu).linenr[(*Curmenu).index], 1, 
				(*Curmenu).entryline[(*Curmenu).index]);
    return;
    }

/*	high_off.   Turn off any highlighting if active.
 */
    FUNCTION static VOID high_off (void)

    {
    IMPORT struct TUTCTX tutctx;

    if (tutctx.highact)			/* highlight active?		*/
	{				/* turn off highlight		*/
	high_value (tutctx.vcur, tutctx.index, FALSE);
	tutctx.highact = FALSE;		/* no highlight active		*/
	}
    return;
    }
