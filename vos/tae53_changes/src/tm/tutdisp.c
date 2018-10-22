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
 * This file contains functions to manipulate tutor displays,
 * both in screen and noscreen mode.
 *
 * This file is extracted from tutor.c during the tutor.c breakup of 6-dec-84.
 * See full explanation in tutor.c.
 *
 * The functions in this source file are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	10-dec-84	TCL 67: parm screen qual notice no descr check...peb
 *	19-jul-85	Fix bug in left_pbld calling sequence...dm
 *	19-sep-85	PR 1012: Change high_value to display blanks when
 *			NAME parm does'nt have current value...dab
 *	31-jul-86	Added code for the TAE_FACELIFT...krw
 *	11-feb-87	Merged TAE FACELIFT with V1.4 TAE...krw
 *
 ********************* Post TAE 2.1 changes *******************************
 *
 *	19-may-87	Updated for TAE-Plus philosophy. Checked global 
 *			variable DisplayId to determine VT100 or Window 
 *			mode screen operations...dm
 *	12-aug-87	Fix "show x.q" and "page x.q" and "x.q = value":
 *			they crash tutor in getpgnum...tpl
 *	02-dec-87	Declare DisplayId as GENPTR...dm
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl
 *	24-may-88	Fix incomplete screen update with up or douwm arrow
 *			upon tutor entry of paging beyond last page...hsk
 *	08-dec-88	Honor COMPRESS_TUTOR...palm
 *	29-jan-89	Truncate parm name display to 8 chars (even tho
 *			NAMESIZ will soon be larger than 8). This so
 *			that the tutor screen design is the same...palm
 *	06-feb-89	Fix bug in above when highlighting...palm 
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	27-jun-90	Removed Facelift code...ljn
 * 	20-may-91	fix longstanding crash with "tutor save-",
 *			where you enter subcommand mode first; this
 *			bug was apparently masked by architectures other
 *			than Sun4 (but what about VAX?)...palm
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 */


#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"dirinc.inc"		/* includes for d_ package		*/
#include	"terminc.inc"
#include "taeintproto.h"



    IMPORT  struct TUTCTX tutctx;	/* tutor & micro-editor context	*/

    static TEXT		msg_mmov[] = "Terminal Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/
    static TEXT		msg_ambi[] = "Ambiguous parameter abbreviation, '%s'.";
    static TEXT		key_ambi[] = "TAE-AMBIGPAR";
    static TEXT		msg_unre[] = "Unrecognized parameter name, '%s'.";
    static TEXT		key_unre[] = "TAE-TUNRECPAR";
    static TEXT		msg_ambs[] = "Ambiguous subcommand abbreviation, '%s'.";
    static TEXT		key_ambs[] = "TAE-AMBIGSUB";
    static TEXT		msg_unrs[] = "Unrecognized subcommand, '%s'.";
    static TEXT		key_unrs[] = "TAE-UNRECSUB";

    static TEXT		nodescr_notice[] = "Description not available";

FUNCTION static COUNT getbot 
(
    struct VARIABLE	*v		/* in:  var containing the panel	*/

 );

FUNCTION static VOID disppag 
(
    struct CONTXT	*pctx,		/* in:  proc context			*/
    FUNINT		pagnum,		/* in:  page # to display		*/
    struct TXTSTOR	*title		/* in:  title text			*/

 );
FUNCTION static VOID listpqnam 
(
    struct SYMTAB	*symtab	/* in:  symbol table containing parms	*/

 );


/* adjtop - adjust the value panel top & bottom component numbers.
 * Uses the supplied component number as the new bottom component
 * number and adjusts the panel top to fit the display.
 * Assumes the value panel position array is up to date.
 * This function should not be called except for string parameters, and
 * then only if a component is being updated that will overflow the
 * currently displayed value panel.
 */

FUNCTION static VOID adjtop 
(
    struct VARIABLE	*v,		/* in/out: var containing panel to be adjusted*/
    FUNINT		compnum	/* in:  component # of new panel bottom	*/

 )
    {
    struct TUTEXT	*t;

    t = (*v).v_tp;
    if (t == NULL)			/* to protect against unallocated tutor extension*/
	return;
    for ((*t).pantop = 1, (*t).panbot = getbot(v);
	    (*t).panbot < (*v).v_maxc;
	    (*t).pantop++, (*t).panbot = getbot(v))
	if ((*t).panbot >= compnum)	/* if we've found the right top		*/
	    break;
    return;
    }

/* adjval - adjust the value panel top & bottom component numbers.
 * Assumes the value panel position array is up to date.
 * Puts the requested component at the top of the panel with one
 * exception.  If the requested component is close enough to the
 * highest possible component number (.v_maxc) that this highest
 * component can fit on the panel and still allow components above the
 * requested component, the lower numbered components are displayed
 * above the requested component to the extent possible.
 */

FUNCTION VOID adjval 
(
    struct VARIABLE	*v,		/* in/out: var containing panel to be adjusted*/
    FUNINT		compnum	/* in:  component # of new panel top	*/

 )
    {
    struct TUTEXT	*t;

    t = (*v).v_tp;
    if (t == NULL)			/* to protect against unallocated tutor extension*/
	return;
    if ((*v).v_type == V_NAME)
	(*t).pantop = (*t).panbot = 1;
    else
	{
	for ((*t).pantop = 1, (*t).panbot = getbot(v);
		(*t).panbot < (*v).v_maxc;
		(*t).pantop++, (*t).panbot = getbot(v))
	    if ((*t).pantop == compnum)
		break;
	}
    return;
    }

/* dispbld - display a specific tutor page.  Do help reads & data storage as
 * necessary.
 *
 * NOTE:  For screen tutor, the page number to parameter correspondence
 *	  is assigned here, and is permanent for this tutor session.
 *	  Each parameter is allowed enough space on its page to allow for
 *	  all level one help text, and for the parameter value to grow
 *	  to its maximum possible length.  For string type parameters,
 *	  this maximum possible length must allow for the maximum possible
 *	  string length for each component of the parameter value.
 *	  If, after the maximum possible display length for the parameter
 *	  length for the parameter is calculated, there is room remaining,
 *	  tutor attempts to fit the next parameter on the same page, and
 *	  so on.  No parameter is allowed more than one full tutor page.
 *	  The value panel concept, and value panel scrolling, are used
 *	  to make the entire set of value components available to the user.
 *
 *	  One result of the above algorithm is that after the tutor data
 *	  structures have been built for the particular proc being tutored
 *	  on, tutor code can assume that any parameter whose value display
 *	  might overflow the available space is alone on the tutor page.
 *
 *  NOTE:  Under TAE-Plus, we check if TM is initialized to talk
 *  	  to the X Window server (implying that it is using a graphics
 *  	  display device as its screen). In that case we call the routine
 *  	  DispTutWin() to display tutor page in a window. Otherwise, we
 *  	  display the page in regular VT100 simulated mode as described
 *	  above.
 *
 */

FUNCTION CODE dispbld 
(
    struct CONTXT	*pctx,		/* in/out: proc context			*/
    struct TXTSTOR	*title,		/* in:  .TITLE text			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct VARIABLE	*v;		/* variable structure			*/
    struct SUBCMD	*s;
    struct TUTEXT	*t;		/* tutor extension			*/
    COUNT		lvarpg;		/* page # of last variable or subc	*/
    CODE		code;


/* If TAE is initialized to use X Windows, assume the entire Tutor
 * display is a big page, containing all the parameters. We position to
 * the appropriate place through scroll.
 */

    code   = SUCCESS;
    lvarpg = 0;
    if (!tutctx.srch1cplt)		/* if search not yet complete for lev 1 data*/
	{
	if (!tutctx.subtut)		/* if tutoring on parms			*/
	    {
	    for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)
		{
		t = (*v).v_tp;				/* point to tutor extension	*/
		if (t == NULL  ||  (*t).pagnum == 0)	/* if parm not yet integrated in tutor disp*/
		    {
		    code = bldvhlp(pctx, v, lvarpg, title, hf);	/* read in lev 1 help for parm & remain parms on pg*/
		    t = (*v).v_tp;
		    }
		if ((*t).pagnum == tutctx.curpag)	/* if this parm is on right page	*/
		    break;
		lvarpg = (*t).pagnum;
		}
	    if (v == NULL)			/* end of symbol table.  Last page defined*/
		{
		tutctx.lastpag = (lvarpg == 0)  ?  1 : lvarpg; /* always at least 1 pg	*/
		tutctx.srch1cplt = TRUE;
		tutctx.curpag = tutctx.lastpag;
		}
	    else if ((*v).v_link == NULL)
		{
		tutctx.lastpag   = (*t).pagnum;
		tutctx.srch1cplt = TRUE;
		}
	    }
	else					/* tutoring on subcommands	*/
	    {
	    for (s = (*pctx).subptr; s != NULL; s = (*s).link)
		{
		t = &(*s).ext;				/* point to tutor extension	*/
		if (t == NULL  ||  (*t).pagnum == 0)	/* if parm not yet integrated in tutor disp*/
		    {
		    code = bldshlp(pctx, s, lvarpg, title, hf);	/* read in lev 1 help for parm & remain parms on pg*/
		    t = &(*s).ext;
		    }
		if ((*t).pagnum == tutctx.curpag)	/* if this parm is on right page	*/
		    break;
		lvarpg = (*t).pagnum;
		}
	    if (s == NULL)			/* end of symbol table.  Last page defined*/
		{
		tutctx.lastpag = (lvarpg == 0)  ?  1 : lvarpg; /* always at least 1 pg	*/
		tutctx.srch1cplt = TRUE;
		tutctx.curpag    = tutctx.lastpag;
		}
	    else if ((*s).link == NULL)
		{
		tutctx.lastpag = (*t).pagnum;
		tutctx.srch1cplt = TRUE;
		}
	    }   
	}
    else
	if (tutctx.curpag > tutctx.lastpag)
	    tutctx.curpag = tutctx.lastpag;
    if (tutctx.dispreq)
	{
	disppag(pctx, tutctx.curpag, title);   /* display the page	*/
	tutctx.dispreq = FALSE;
	}
    return(code);
    }

/* dspl1hlp - display level 1 help for NOSCREEN tutor given a
 * valid tutor extension.
 */

FUNCTION static VOID dspl1hlp 
(
    struct TUTEXT	*t		/* in:  tutor extension			*/

 )
    {
    struct TXTSTOR	*block;
    TEXT		buf[STRINGSIZ+1];
    TEXT		**str;
    COUNT		i;

    if ((*t).l1hexist)
	{
	block = &(*t).l1help;
	str   = (*block).tp;
	for (i = 0; i < (*block).numline; i++)
	    {
	    s_blank(buf, 4);
	    s_append(str[i], buf);
	    t_write(buf, T_STDCC);
	    }
	}
    else
	{
	s_copy ("    ", buf);
	s_append (nodescr_notice, buf);
	t_write (buf, T_STDCC);
	}
    return;
    }

/*	get_parm1.   Get the first parameter of a given page.
 */
FUNCTION struct VARIABLE *get_parm1 
(
    struct CONTXT *pctx,	/* in: proc context		*/
    FAST FUNINT	  page,		/* in: page number		*/
    struct SFILE  *hf		/* in: help file		*/

 )
    {
    FAST struct VARIABLE *v, *savev;
    COUNT vpage, savepage;

    vpage = -1;			/* in case getpgnum returns FAIL */
    savev = (*pctx).parmst.link;
    for (v=(*pctx).parmst.link; v != NULL; v=(*v).v_link)
	{					
	savepage = vpage;
	getpgnum (pctx, (*v).v_name, &vpage, hf);	
	if (vpage == page)
	    return (v);
	if (savepage != vpage)
	    savev = v;
	}
     
    /* End of symbol table. Reaches this point when user provided page # */
    /* (page n) is greater than the lastpage of parms for the first time */
    if (v == NULL)
	{
	tutctx.lastpag = (vpage == 0) ? 1 : vpage; /* always at least 1 pg */
	tutctx.srch1cplt = TRUE;  /* since pagnum is assigned to last parm */ 
	tutctx.curpag = tutctx.lastpag;
	return (savev);
	}
    return 0;
    }  

/* getbot - return the bottom component number of a tutor value panel.
 * Assumes the panel top specifier and the panel posit array are up to date.
 * For string type, uses only the space required for the current value of
 * each string (i.e., does not allow for growth to the max possible length
 * of the last component).
 */

FUNCTION static COUNT getbot 
(
    struct VARIABLE	*v		/* in:  var containing the panel	*/

 )
    {
    IMPORT COUNT	termlines;	/* number of lines on CRT		*/

    COUNT		i;
    TINY		*nlin;
    COUNT		begline, endline;
    COUNT		maxline;
    COUNT		sofar;
    struct TUTEXT	*t;

    if ((*v).v_type == V_NAME)
	return(1);
    t      = (*v).v_tp;
    nlin   = (*t).compnlin;
    begline = tutctx.pageStart + (*t).startlin;
    if (tutctx.spaceLines)			/* maintain bug: field compat */
        endline = termlines  - (ERRLIN + 2);  	/* keyin+prompt+error */
    else					/* compress mode: do right */ 
	endline = termlines - (ERRLIN + 1);	/* keyin+prompt+error */
    maxline = endline - begline + 1;
    sofar   = 0;
    for (i = (*t).pantop, nlin += i-1; i <= (*v).v_maxc; i++, nlin++)	/* for each component	*/
	{
	if (sofar + *nlin > maxline)
	    return(i-1);
	sofar += *nlin;			/* add number lines for this component	*/
	}
    return(i-1);
    }

/* getoffs - get the offset in display lines of this component
 *	     from the top of the display panel.
 * Assumes the panel position array and the panel top specifier are
 * up to date.
 */

FUNCTION static COUNT getoffs 
(
    struct VARIABLE	*v,		/* in:  variable containing the panel	*/
    FUNINT		compnum	/* in:  component number to find offset of*/

 )
    {
    TINY		*nlin;
    struct TUTEXT	*t;
    COUNT		offset;
    COUNT		i;

    t = (*v).v_tp;
    nlin = (*t).compnlin;
    offset = 0;
    for (i = (*t).pantop, nlin += i-1; i < compnum; i++, nlin++)
	offset += *nlin;
    return(offset);
    }

/* getpgnum - get level 1 display page number of a proc parameter.
 * Performs help reads & page numbering of unencountered parms as necessary.
 * Returns FAIL if parmname is not in the parameter symbol table,
 * or we ran out of dynamic memory.
 */

FUNCTION CODE getpgnum 
(
    struct CONTXT	*pctx,		/* in/out: proc context to find parm in	*/
    TEXT		parmname[],	/* in:  proc parameter name		*/
    COUNT		*pagnum,	/* out: display page number		*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct VARIABLE	*v;
    struct TUTEXT	*t = 0;
    COUNT		lvarpg;
    CODE		code;

    if ((code = lookab(&(*pctx).parmst, parmname, &v)) != SUCCESS) goto parm_err;
    if (s_index (parmname, '.')  >= 0)		/* qualified ?	      */
 	goto parm_err;
    parmname = (*v).v_name;			/* use full spelling  */
    lvarpg = 0;
    for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)
	{
	t = (*v).v_tp;
	if (t == NULL  ||  (*t).pagnum == 0)	/* if parm not yet integrated in tutor display*/
	    {
	    if (bldvhlp(pctx, v, lvarpg, &tutctx.title, hf) != SUCCESS)
					/*integrat parm & remain parms on pg*/
		return(FAIL);
	    t = (*v).v_tp;
	    }
	if (s_equal (parmname, (*v).v_name))
	    break;
	lvarpg = (*t).pagnum;
	}
    *pagnum = (*t).pagnum;
    return(SUCCESS);

parm_err:
    if (code == AMBIG)
      tutmsg(msg_ambi, key_ambi, (uintptr_t) parmname, 0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
    else
      tutmsg(msg_unre, key_unre, (uintptr_t) parmname, 0, 0, 0, 0);	/* unrecognized parameter		*/
    return(FAIL);
    }

/* getspnum - get level 1 display page number of a proc subcommand.
 * Performs help reads & page numbering of unencountered subcs as necessary.
 * Returns FAIL if subcommand name is not in the subcommand table,
 * or we ran out of dynamic memory.
 */

FUNCTION CODE getspnum 
(
    struct CONTXT	*pctx,		/* in/out: proc context to find subc in	*/
    TEXT		subname[],	/* in:  subcmd name			*/
    COUNT		*pagnum,	/* out: display page number		*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct SUBCMD	*s;
    struct TUTEXT	*t;
    COUNT		lsubpg;
    CODE		code;

    if ((code = subab((*pctx).subptr, subname, &s)) != SUCCESS) goto subc_err;
    lsubpg = 0;
    for (s = (*pctx).subptr; ; s = (*s).link)
	{
	t = &(*s).ext;
	if (t == NULL  ||  (*t).pagnum == 0)	/* if subc not yet integrated in tutor display*/
	    {
	    if (bldshlp(pctx, s, lsubpg, &tutctx.title, hf) != SUCCESS)
					/*integrat subc & remain subcs on pg*/
		return(FAIL);
	    t = &(*s).ext;
	    }
	if (s_lseq(subname, (*s).name))
	    break;
	lsubpg = (*t).pagnum;
	}
    *pagnum = (*t).pagnum;
    return(SUCCESS);

subc_err:
    if (code == AMBIG)
      tutmsg(msg_ambs, key_ambs, (uintptr_t) subname, 0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
    else
      tutmsg(msg_unrs, key_unrs, (uintptr_t) subname, 0, 0, 0, 0);	/* unrecognized subcommand		*/
    return(FAIL);
    }

/*	high_value.    Highlight (or un-highlight)
 *	a parameter name and one component's value.
 *	Parameter must currently be displayed.
 */

FUNCTION VOID high_value 
(
    struct VARIABLE	*v,		/* in: parameter to highlight	*/
    FUNINT		index,		/* in: index to highlight	*/
    BOOL		highlight	/* in: TRUE to highlight	*/
					/*     FALSE to turn off	*/

 )
    {
    struct TXTSTOR	txt;			/* text storage block	*/
    COUNT		comp_line, start_line;
    COUNT		i;
    struct TUTEXT 	*t;
    CODE		code;
    TEXT		string [STRINGSIZ+1];
    BOOL		nonblank;	/* blanks or value to be displayed ? */
    TEXT		nameString[STRINGSIZ+1];

    t = (*v).v_tp;		/* tutor extension ptr		*/
    initxt (&txt);		/* init text storage block	*/
    start_line = tutctx.pageStart + (*t).startlin;
    if (highlight)
	{
	s_bcopy ((*v).v_name, nameString, 8);	/* tutor displays 8 max */
        high_lfield (nameString, 8, string);
	}
    else
	{
	i = s_bcopy ((*v).v_name, string, 8);	/* tutor displays 8 max */
	s_blank (&string[i], 8-i);
	}	
    t_output (start_line, TUTNAMECOL, string);
    comp_line = getoffs (v, index+1)  +  start_line;
    code = addcomp (&txt, v, index+1);
    if (code == SUCCESS)		/* (if no memory, do nothing)	*/
        {
	if ((*v).v_type == V_NAME)	/* Any current reference value? */
	    nonblank = (*v).v_ref != NULL ? TRUE : FALSE;
	else				/* Any current explicit value?  */
	    nonblank = (index < (*v).v_count)  ||
		       ((*v).v_count == 0 && index == 0) ? TRUE : FALSE;
	if (nonblank)
    	    {						/* non-blank field: */
	    if ((*v).v_type == V_STRING)
		hdisptxt (comp_line, TUTVALCOL, &txt, TUTVALSIZ, highlight);
	    else
		hdisprtxt (comp_line, TUTVALCOL+TUTNUMRTCOL, 
			   &txt, TUTNUMRTCOL-1, highlight);
	    }
	else
	    {							
	    s_blank (string, TUTVALSIZ);		/* no value...	*/
	    if (highlight)
	        t_highlight (string);
	    t_output (comp_line, TUTVALCOL, string);	/* so blank it	*/
	    }
	}
    fretxt (&txt);
    return;
    }

/* h1dsp_parm - display level 1 help for a parm.
 */

FUNCTION VOID h1dsp_parm 
(
    struct CONTXT	*ctx,		/* in:  proc context			*/
    struct VARIABLE	*v,		/* variable to display help on		*/
    struct SFILE	*hf		/* in:  help file			*/

 )
    {
    COUNT		pagnum;		/* dummy to receive page #		*/
    CODE		code;
    struct TUTEXT	*t;

    code = getpgnum(ctx, (*v).v_name, &pagnum, hf);	/* make sure help reads done for this parm*/
    if (code != SUCCESS) return;
    t = (*v).v_tp;
    dspl1hlp(t);					/* display lev 1 help	*/
    return;
    }

/*	h1dsp_subc - display level 1 help for a subcommand.
 */

FUNCTION CODE h1dsp_subc 
(
    struct CONTXT	*ctx,		/* in:  proc context			*/
    struct SUBCMD	*s,		/* subcommand to display help on		*/
    struct SFILE	*hf		/* in:  help file			*/

 )
    {
    COUNT		pagnum;		/* dummy to receive page #		*/
    CODE		code;
    struct TUTEXT	*t;

    code = getspnum(ctx, (*s).name, &pagnum, hf);	/* make sure help reads done for this subc*/
    if (code != SUCCESS) return(FAIL);
    t = &(*s).ext;
    dspl1hlp(t);					/* display lev 1 help	*/
    return(SUCCESS);
    }

/* inipan - initialize tutor value panel position array.
 * Allocates the panel position array if necessary.
 * If the tutor extension for this variable hasn't yet been allocated
 * (i.e., if this variable has never been displayed and isn't now being
 * set up for display) then this function just returns.
 */

FUNCTION CODE inipan 
(
    struct VARIABLE	*v		/* in/out: variable containing the panel*/

 )
    {
    struct TUTEXT	*t;
    TINY		*nlin;
    COUNT		i;
    struct TXTSTOR	txt;
    COUNT		maxcount;
    CODE		type;

    if ((t = (*v).v_tp) == NULL)
	return(SUCCESS);
    type = (*v).v_type;
    maxcount = (type == V_NAME)  ?  1 : (*v).v_maxc;
    if ((nlin = (*t).compnlin) == NULL)	/* if posit array not yet allocated	*/
	{
	nlin = (*t).compnlin = (TINY *)tae_alloc(maxcount, sizeof(TINY));	/* alloc the posti array*/
	if (nlin == NULL)
	    return(FAIL);
	}
    if (type == V_NAME)
	*nlin = 1;
    else
	for (i = 1; i <= maxcount; i++, nlin++)
	    {
	    if (i <= (*v).v_count)
		{
		initxt(&txt);			/* init the text struct		*/
		if (addcomp(&txt, v, i) != SUCCESS)
		    return(FAIL);
		*nlin = (TINY)txt.numline;
		fretxt(&txt);
		}
	    else
		*nlin = 1;
	    }
    return(SUCCESS);
    }

/* list_parm - list a parm and it's value.
 */

FUNCTION VOID list_parm 
(
    struct VARIABLE	*v		/* in:  variable to list		*/

 )
    {
    IMPORT COUNT	termcols;

    TEXT		buf[STRINGSIZ+1];
    COUNT		i;
    CODE		code;
    COUNT		length;
    CODE		type;
    struct VARIABLE	*vref;

    IMPORT TEXT		qual_notice[];

    buf[0] = EOS;				/* initialize	*/
    type   = (*v).v_type;
    m_fpname((*v).v_name, buf, STRINGSIZ);	/* get name 	*/
    if (type == V_NAME)
	{
	vref = (*v).v_ref;
	if (vref == NULL)
	    s_append(" (no reference)", buf);
	else
	    {
	    if ((*v).v_default)			/* ref local to PDF	*/
	        s_append("+", buf);
	    s_append((*vref).v_name, buf);
	    }
	}
    else
	{
	for (i = 0; i < (*v).v_count; i++)		/* for each component */
	    {
	    code = m_fpval(v, i, buf, termcols, STRINGSIZ);	/* put in buffer */
	    if (code == FAIL)				/* no more room	*/
		{
		t_write(buf, T_STDCC);			/* write the line */
		buf[0] = EOS;			
		--i;					/* ignore last increment	*/
		}
	    }
	if ((*v).v_count > 0)
	    {
	    length = s_length(buf);
	    buf[length-2] = EOS;			/* delete last ','	*/
	    }
	else if ((*v).v_count < 0)
	    s_append(" (no value specified)", buf);
	else if ((*v).v_count == 0)
	    s_append(" --  (null value)", buf);
	}
    t_write(buf, T_STDCC);			/* final flush of output buffer	*/
    if (type != V_NAME)
	if ((*v).v_pv12  &&  (*v).v_qualst.link != NULL) /* if parm has qualifs	*/
	    t_write(qual_notice, T_STDCC);		/* tell the user	*/
    return;
    }

/*	list_preface.   Display caller-supplied preface.
 */

FUNCTION VOID list_preface 
(
    struct VARIABLE	*p	/* in: strings to display	*/

 )
    {
    COUNT	i;

    for (i=0; i < (*p).v_count; i++)
        {
	s_strip (SVAL(*p, i));	/* remove trailing blanks */
	t_write (SVAL(*p, i), T_STDCC);
	}
    return;
    }

/*	list_subc - list a subcommand and whether or not it's the default.
 */

FUNCTION VOID list_subc 
(
    struct SUBCMD	*s		/* in:  subcommand to list		*/

 )
    {
    TEXT		buf[STRINGSIZ+1];

    buf[0] = EOS;				/* initialize	*/
    s_copy((*s).name, buf);
    if ((*s).deflt)				/* if this is the default subc	*/
	s_append("  ** DEFAULT **", buf);
    else
	s_append("  (not default)", buf);
    t_write(buf, T_STDCC);			/* final flush of output buffer	*/
    return;
    }

/*	listpnam - list parameter names for NOSCREEN tutor.
 *	Also lists those parms which have defined qualifiers.
 */

FUNCTION VOID listpnam 
(
    struct SYMTAB	*symtab	/* in:  symbol table containing parms	*/

 )
    {
    struct VARIABLE	*v;
    TEXT		buf[STRINGSIZ+1];
    COUNT		cushion_len;
    COUNT		num_parm;
    COUNT		len;

    v = (*symtab).link;
    if (v == NULL)
	s_copy("Parameters requested: none.", buf);
    else
	{
	cushion_len = STRINGSIZ - NAMESIZ - 5;
	s_copy("Parameters requested:  ", buf);
	num_parm = 0;
	for ( ; v != NULL; v = (*v).v_link)
	    {
	    num_parm++;
	    if ((*v).v_link == NULL)		/* if last parm			*/
		{
		if (num_parm == 2)
		    {
		    len = s_length(buf);
		    buf[len-2] = EOS;		/* strip trailing ", "		*/
		    s_append(" ", buf);
		    }
		if (num_parm > 1)
		    s_append("and ", buf);
		s_append((*v).v_name, buf);
		s_append(".", buf);
		}
	    else
		{
		s_append((*v).v_name, buf);
		s_append(", ", buf);
		if (s_length(buf) > cushion_len)
		    {
		    s_append("...", buf);
		    break;
		    }
		}
	    }
	}
    t_write(buf, T_STDCC);
    listpqnam (symtab);			/* list those parms with def'd quals	*/
    return;
    }

/*	listpqnam - lists those parms which have defined qualifiers,
 *	for NOSCREEN tutor.
 */

FUNCTION static VOID listpqnam 
(
    struct SYMTAB	*symtab	/* in:  symbol table containing parms	*/

 )
    {
    struct VARIABLE	*v;
    TEXT		qual_buf[STRINGSIZ+1];
    TEXT		tmp_buf[STRINGSIZ+1];
    COUNT		cushion_len;
    COUNT		num_q_parm;		/* no. parms with def'd quals	*/
    COUNT		len;
    COUNT		i;

    v = (*symtab).link;
    if (v == NULL)
	return;
    qual_buf[0] = EOS;
    cushion_len = STRINGSIZ - NAMESIZ - 5;
    num_q_parm = 0;	
    for ( ; v != NULL; v = (*v).v_link)
	{
	if (!(*v).v_pv12  ||  (*v).v_qualst.link == NULL)
	    continue;
	num_q_parm++;
	if (num_q_parm == 1)
	    s_copy ("Parameters with defined qualifiers:  ", qual_buf);
	s_append((*v).v_name, qual_buf);
	s_append(", ", qual_buf);
	if (s_length(qual_buf) > cushion_len)
	    {
	    s_append("...", qual_buf);
	    break;
	    }
	}
    if (NULLSTR(qual_buf))
	return;
    len = s_length (qual_buf);
    if (!s_equal (&qual_buf[len-3], "..."))
	{
	if (num_q_parm == 1)
	    {
	    qual_buf[len-2] = '.';
	    qual_buf[len-1] = EOS;
	    }
	else				/* there was more than 1 parm with quals*/
	    {
	    for (i = len-3; i > 0; i--)	/* search backwards for last "," betw words*/
		{
		if (qual_buf[i] == ',')
		    {
		    s_copy (&qual_buf[i+2], tmp_buf);	/* last parm name to scratch buf*/
		    len = s_length (tmp_buf);
		    tmp_buf[len-2] = '.';		/* close with "."	*/
		    tmp_buf[len-1] = EOS;
		    if (num_q_parm == 2)
			s_copy (" and ", &qual_buf[i]);
		    else				/* more than 2 parms with quals*/
			s_copy (", and ", &qual_buf[i]);
		    s_append (tmp_buf, qual_buf);	/* put back last parm name*/
		    break;
		    }
		}
	    }
	}
    t_write (qual_buf, T_STDCC);
    return;
    }

/*	listqnam - list names of qualifiers desired for a particular
 *	parameter in qualifier space of NOSCREEN tutor.
 */

FUNCTION VOID listqnam 
(
    struct SYMTAB	*symtab,	/* in:  symbol table containing quals	*/
    TEXT		parm_name[]	/* in:  name of parm being qualified	*/

 )
    {
    struct VARIABLE	*v;		/* points to each qual (quals look like parms)*/
    TEXT		buf[STRINGSIZ+1];
    COUNT		cushion_len;
    COUNT		num_parm;
    COUNT		len;


    v = (*symtab).link;
    cushion_len = STRINGSIZ - NAMESIZ - 5;
    s_copy ("'", buf);
    s_append (parm_name, buf);
    s_append ("' parameter qualifiers requested:  ", buf);
    num_parm = 0;
    for ( ; v != NULL; v = (*v).v_link)
	{
	num_parm++;
	if ((*v).v_link == NULL)		/* if last parm			*/
	    {
	    if (num_parm == 2)
		{
		len = s_length(buf);
		buf[len-2] = EOS;		/* strip trailing ", "		*/
		s_append(" ", buf);
		}
	    if (num_parm > 1)
		s_append("and ", buf);
	    s_append((*v).v_name, buf);
	    s_append(".", buf);
	    }
	else
	    {
	    s_append((*v).v_name, buf);
	    s_append(", ", buf);
	    if (s_length(buf) > cushion_len)
		{
		s_append("...", buf);
		break;
		}
	    }
	}
    t_write(buf, T_STDCC);
    return;
    }

/* listsnam - list subcommand names for NOSCREEN tutor.
 */

FUNCTION VOID listsnam 
(
    struct SUBCMD	*subptr	/* in:  subcommand chain		*/

 )
    {
    struct SUBCMD	*s;
    TEXT		buf[STRINGSIZ+1];
    COUNT		cushion_len;
    COUNT		num_subc;
    COUNT		len;

    s = (*subptr).link;
    if (s == NULL)
	s_copy("No subcommands.", buf);
    else
	{
	cushion_len = STRINGSIZ - SUBCMDSIZ - 5;
	s_copy("Subcommands:  ", buf);
	num_subc = 0;
	for ( ; s != NULL; s = (*s).link)
	    {
	    num_subc++;
	    if ((*s).link == NULL)		/* if last subc			*/
		{
		if (num_subc == 2)
		    {
		    len = s_length(buf);
		    buf[len-2] = EOS;		/* strip trailing ", "		*/
		    s_append(" ", buf);
		    }
		if (num_subc > 1)
		    s_append("and ", buf);
		s_append((*s).name, buf);
		s_append(".", buf);
		}
	    else
		{
		s_append((*s).name, buf);
		s_append(", ", buf);
		if (s_length(buf) > cushion_len)
		    {
		    s_append("...", buf);
		    break;
		    }
		}
	    }
	}
    t_write(buf, T_STDCC);
    return;
    }

/* nlinval - return the max number of lines required for the components of
 * one parm value in a tutor display.
 */

FUNCTION COUNT nlinval 
(
    struct VARIABLE	*v		/* in:  variable to fin # lines for	*/

 )
    {
    COUNT		num;
    COUNT		mult;

    if ((*v).v_type == V_NAME)
	return(1);
    num = (*v).v_maxc;			/* should be 1 line per possible component	*/
    if ((*v).v_type == V_STRING)	/* unless oversize strings		*/
	{
	mult = ((*v).v_size - 1 + 2) / TUTVALSIZ + 1;	/* "+2" is for surrounding quotes*/
	num *= mult;
	}
    return(num);
    }


/* dispcnum - display component numbers for a value panel.
 * Assumes the panel position array is up to date.
 * Assumes the panels top component specifier is up to date.
 * Assumes the variable is not of type V_NAME.
 */

FUNCTION static VOID dispcnum 
(
    struct VARIABLE	*v,		/* in:  variable containing value	*/
    FUNINT		top,		/* in:  top component to be numbered	*/
    FUNINT		bottom		/* in:  bottom component to be numbered	*/

 )
    {
    struct TUTEXT	*t;		/* tutor ext with panel posit array	*/
    COUNT		i;
    TINY		*nlin;
    COUNT		line;
    TEXT		buf[STRINGSIZ+1];
    TEXT		tmpbuf[4];

    t    = (*v).v_tp;			/* point to the tutor extension		*/
    line = tutctx.pageStart + (*t).startlin;
    nlin = (*t).compnlin;
    for (i = (*t).pantop, nlin += i-1; i <= bottom; i++, nlin++)
	{
	if (i >= top)			/* if we're supposed to number this component*/
	    {
	    s_copy("(", buf);
	    s_i2s(i, tmpbuf);
	    s_append(tmpbuf, buf);
	    s_append(")", buf);
	    if (i == (*t).panbot) {	/* if this is last component in panel	*/
		if (i == (*v).v_maxc)	/* and if it's the last possible component*/
		    s_append(".", buf);
		else
		    s_append("+", buf);
	    }
	    t_output(line, TUTCOMPNUMCOL, buf);
	    }
	line += *nlin;			/* posit for next component		*/
	}
    return;
    }

/* dispcomp - display a value component.
 * Assumes this value is already on display.
 * This function updates .panbot, .compnlin.
 * If the component's new value uses the same number of display lines, just
 * redisplay the component.  If the number of lines changes, all the lower
 * components in the panel must be scrolled, including eliminating or adding
 * component(s) at the bottom of the panel.  If the component is for a string
 * parameter near the bottom of the displayed value panel, and the new value
 * causes the component to overflow the panel, the following algorithm is used:
 *
 *	- the updated component becomes the new bottom component of the
 *	  value panel.
 *	- the panel top component is reset to a higher numbered component
 *	  as necessary to allow room for the new bottom component value.
 *	- the entire value panel is redisplayed.
 *
 * If the component update involved fill, the filled components
 * whose component numbers are already on the screen get their new
 * values displayed.
 */

FUNCTION VOID dispcomp 
(
    struct VARIABLE	*v,		/* in:  the variable to display		*/
    FUNINT		compnum,	/* in:  subscr (starting with 1) of the component*/
    FUNINT		fillcomp	/* in:  lowest #d comp that was filled (0 = no fill)*/

 )
    {
    IMPORT COUNT	termlines;	/* number of lines on CRT		*/

    struct TXTSTOR	comptxt;
    TINY		*nlin;		/* number of display lines		*/
    COUNT		sline, soffset;	/* starting line, offset for this component*/
    COUNT		valstlin, numclrln;
    CODE		type;
    BOOL		disp_done;
    struct TUTEXT	*t;
    COUNT		i;

    type = (*v).v_type;
    t = (*v).v_tp;
    valstlin = tutctx.pageStart + (*t).startlin;
    soffset  = getoffs(v, compnum);	/* get starting displ offset for this component*/
    sline    = valstlin + soffset;
    numclrln = min((*t).linrange, termlines-ERRLIN-tutctx.pageStart)
		- soffset;
    disp_done = FALSE;
    initxt(&comptxt);			/* init component text structure	*/
    nlin = (*t).compnlin;		/* point to number of lines array	*/
    nlin += compnum-1;			/* point to element for this component	*/
    if (addcomp(&comptxt, v, compnum) != SUCCESS) goto over_err;	/* add one component text*/
    if (comptxt.numline == *nlin) 	/* if number display lines hasn't changed*/
	{
	clrmlin(sline, TUTVALCOL, *nlin);	/* clear the old component value*/
	if (type != V_NAME  &&  (*v).v_maxc > 1)
	    dispcnum(v, compnum, compnum);
	}
    else
	{
	*nlin = (TINY)comptxt.numline;
	(*t).panbot = getbot(v);		/* get new bottom component of panel*/
	if ((*t).panbot < compnum)		/* if new string overflowed panel*/
	    {
	    adjtop(v, compnum);			/* adj panel top to conform...	*/
	    					/* to bigger bottom component	*/
	    dispval(v);				/* display whole value panel	*/
	    disp_done = TRUE;
	    }
	else
	    {
	    for (i = compnum+1; i <= (*t).panbot; i++)	/* for each lower component in panel*/
		if (addcomp(&comptxt, v, i) != SUCCESS) goto over_err;
	    clrmlin(sline, TUTVALCOL, numclrln);
	    if (type != V_NAME  &&  (*v).v_maxc > 1)
		dispcnum(v, compnum, (*t).panbot);	/* display component numbers	*/
	    }
	}
    if (!disp_done)					/* if val not yet disp'd*/
	{
	if (type == V_STRING)
	    disptxt(sline, TUTVALCOL, &comptxt);
	else
	    disprtxt(sline, TUTVALCOL + TUTNUMRTCOL, &comptxt);
	}
    fretxt(&comptxt);
    if (fillcomp > 0)				/* if there were filled components*/
	for (i = max(fillcomp, (*t).pantop); i < compnum; i++)	/* for each filled comp on screen*/
	    {
	    soffset = getoffs(v, i);
	    sline   = valstlin + soffset;
	    initxt(&comptxt);
	    if (addcomp(&comptxt, v, i) != SUCCESS) goto over_err;
	    if (type == V_STRING)
		disptxt(sline, TUTVALCOL, &comptxt);
	    else
		disprtxt(sline, TUTVALCOL + TUTNUMRTCOL, &comptxt);
	    fretxt(&comptxt);
	    }
    return;

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);			/* dynam mem overflow		*/
    return;
    }

/* disppag - display a page of tutor.
 * Assumes that all necessary level 1 help reads have been done,
 * and that the parms or subcommands for this page have been
 * properly page numbered.
 *
 */

FUNCTION static VOID disppag 
(
    struct CONTXT	*pctx,		/* in:  proc context			*/
    FUNINT		pagnum,		/* in:  page # to display		*/
    struct TXTSTOR	*title		/* in:  title text			*/

 )
    {
    IMPORT COUNT	termlines;	/* number lines on terminal screen	*/
    IMPORT COUNT	termcols;

    struct VARIABLE	*v = NULL;
    struct SUBCMD	*s = 0;
    struct TUTEXT	*t = 0;
    COUNT		line;
    BOOL		lastflag;	/* TRUE if this is the last page	*/
    COUNT		stlin;		/* starting screen line for vars or subcs*/
    TEXT		lefthead[STRINGSIZ+1];
    TEXT		string[STRINGSIZ+1];
    COUNT		col;
    TEXT		name[STRINGSIZ+1];
    TEXT		hdrtype[STRINGSIZ+1];
    static TEXT run_msg[] = "Type RUN to execute.";
    COUNT		l;
    COUNT		q_notice_line;

    IMPORT TEXT		qual_notice[];
    IMPORT	struct  VARIABLE *tutoropt_gbl;  /* tutor options global      */
    BOOL        no_tag, no_name, no_library;

    no_tag = search_vector ((TEXT**) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_TAG");
    no_name = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_NAME");
    no_library = search_vector ((TEXT **)(*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_LIBRARY");
    if (pagnum > tutctx.lastpag  &&  tutctx.srch1cplt)
	pagnum = tutctx.lastpag;
    lastflag = (pagnum == tutctx.lastpag) ? TRUE : FALSE;
    t_clear();				/* clear the screen			*/
    disptxt(tutctx.titleLine, 1, title);	/* display title text			*/
    line = tutctx.titleLine + (*title).numline + tutctx.spaceLines;
    if ((*pctx).intrinsic)
	s_copy ("command", hdrtype);
    else
	s_copy ("proc", hdrtype);
    if (tutctx.subtut)
	{
	left_fbld  ( 				/* build left header         */
		   no_library ? "" : (*pctx).pdf.libr,
		   no_name ? "" : (*pctx).pdf.name,
		   "",				/* subcommand not used */
		   hdrtype, 			/* header type */
		   lefthead);
	dsphdr (no_tag ? "" : "Tutor/Subcommands",	/* display header      */
		lefthead, pagnum, lastflag);   
	for (s = (*pctx).subptr; s != NULL; s = (*s).link)	/* find 1st subc on pg*/
	    {
	    t = &(*s).ext;
	    if ((*t).pagnum == pagnum)
		break;
	    }
	s_copy((*s).name, name);
	}
    else
	{
	if (tutctx.qualtut)
	    {
	    left_pbld ((*pctx).pdf.name, (*pctx).subcmd, tutctx.parmname,
		hdrtype, lefthead);			/* build header name */
	    dsphdr("Tutor/Qualifiers", lefthead, pagnum, lastflag); /* display page header*/
	    }
	else
	    {
	    left_fbld  ( 				/* build left header         */
		   no_library ? "" : (*pctx).pdf.libr,
		   no_name ? "" : (*pctx).pdf.name,
  		   (*pctx).subcmd, hdrtype, lefthead);	/* build header name */
	    dsphdr (no_tag ? "" : "Tutor",	 /* display header      */
	            lefthead, pagnum, lastflag); /* display page header*/
	    }
	for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)	/* find 1st var on pg*/
	    {
	    t = (*v).v_tp;
	    if ((*t).pagnum == pagnum)
		break;
	    }
	if (v == NULL)			/* if proc has no parameters		*/
	    {
	    line = ((termlines - PMTLIN) - 1 - line) / 2  +  line;
	    s_copy ("Proc '", string);
	    s_append ((*pctx).pdf.name, string);
	    if ((*pctx).subcmd[0] != EOS)
		{
		s_append ("-", string);
		s_append ((*pctx).subcmd, string);
		}
	    l = s_append ("' has no parameters.", string);
	    t_output(line, (termcols-l)/2, string);
	    t_output(line+2, (termcols-sizeof(run_msg))/2, run_msg);	
	    return;
	    }
	s_bcopy((*v).v_name, name, 8);	/* tutor only displays 8 chars !*/
	}
    dispflab(line, tutctx.subtut, tutctx.qualtut); /* display field labels	*/
    line = tutctx.pageStart;
    stlin = line;
    col = (tutctx.subtut)  ?  TUTSUBDESCCOL : TUTVARDESCCOL;
    while ((*t).pagnum == pagnum)	/* for each parm on page		*/
	{
	t_output(line, TUTNAMECOL, name);
	if ((*t).l1hexist)
	    disptxt(line, col, &(*t).l1help);	/* display help lev 1 text*/
	else if (v && !(*v).v_qualst.link)
	    t_output(line, col, nodescr_notice);    /* no l1 help & no quals */
	if (tutctx.subtut)
	    {
	    if ((*s).deflt)
		t_output(line, TUTSUBDFLTCOL, "**DEFAULT**");
	    line = stlin + (*t).linrange + tutctx.spaceLines;
	    if ((s = (*s).link) == NULL)
		break;
	    if ((t = &(*s).ext) == NULL)
		break;
	    s_copy((*s).name, name);
	    }
	else if (v)				/* safety check */
	    {
	    if ((*v).v_pv12  &&  (*v).v_qualst.link != NULL) /* if parm has quals...*/
		{
		/*  if no level1 help, this msg replaces the "no descr avail" */
		q_notice_line = line + (*t).l1help.numline;
		t_output(q_notice_line, col, qual_notice);
		}
	    dispval(v);				/* display parm value		*/
	    line = stlin + (*t).linrange + tutctx.spaceLines;
	    if ((v = (*v).v_link) == NULL)	/* point to next parm			*/
		break;
	    if ((t = (*v).v_tp) == NULL)
		break;
	    s_bcopy((*v).v_name, name, 8);	/* tutor only displays 8 */
	    }
	}
    return;
    }

/* dispval - display value components for this parameter on tutor display.
 * Assumes the value panel position array is up to date.
 * Assumes the value panel top & bottom specifiers are up to date.
 */

FUNCTION VOID dispval 
(
    struct VARIABLE	*v		/* in:  variable to display value of	*/

 )
    {
    IMPORT COUNT	termlines;	/* number lines on terminal screen	*/

    CODE		type;
    struct TXTSTOR	valtxt;
    COUNT		i;
    struct TUTEXT	*t;
    COUNT		line;
    COUNT		maxvalln;	/* max display lines allowed for value	*/
    COUNT		numclrln;

    type     = (*v).v_type;
    t        = (*v).v_tp;
    line     = tutctx.pageStart + (*t).startlin;
    maxvalln = max(termlines - ERRLIN - line, 0);
    numclrln = min(nlinval(v), maxvalln);	/* get number of lines to clear	*/
    initxt(&valtxt);
    for (i = (*t).pantop; i <= (*t).panbot; i++)
	if (addcomp(&valtxt, v, i) != SUCCESS) goto over_err;	/* add each compoment's text*/
    clrmlin(line, TUTVALCOL, numclrln);
    if (type != V_NAME  &&  (*v).v_maxc > 1)
	dispcnum(v, (*t).pantop, (*t).panbot);	/* display component numbers*/
    if (type == V_STRING)
	disptxt(line, TUTVALCOL, &valtxt);
    else
	disprtxt(line, TUTVALCOL+TUTNUMRTCOL, &valtxt);
    fretxt(&valtxt);		/* free strings from dyn memory		*/
    return;

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);		/* dyn mem overflow			*/
    return;
    }

/* tutmsg - display an error message.  Depending on tutor source specific
 * flag, writes thru wrterr or tmmsg.
 * If called from processing of a cmd returned from help, hold the message.
 */

FUNCTION VOID tutmsg 
(
    TEXT		msg[],
    TEXT		key[],
    uintptr_t		a1, 
    uintptr_t		a2, 
    uintptr_t		a3, 
    uintptr_t		a4, 
    uintptr_t		a5 
 )
    {
    if (!tutctx.prev_mode || tutctx.msg_held)
	{
	if (tutctx.crt_messages)
	    wrterr(msg, key, a1, a2, a3, a4, a5);
	else
	    tmmsg(PROCFAIL, msg, key, a1, a2, a3, a4, a5);
	tutctx.msg_held = FALSE;
	}
    else				/* must hold message for screen display	*/
	{
	tutctx.msg_held = TRUE;
	sprintf(tutctx.held_msg, msg, a1, a2, a3, a4, a5);
	s_copy(key, tutctx.held_key);
	}
    return;
    }
