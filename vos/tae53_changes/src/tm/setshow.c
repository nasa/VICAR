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
 *	SET and SHOW commands.
 *
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	21-aug-83	Nullables...palm
 *	07-sep-83	Updated for formatting function name changes...dm
 *	12-sep-83	Implemented display-global etc...dm
 *	10-oct-83	fix lint errors...palm
 *	23-nov-83	Delete blank lines between parameters in display...dm
 *	07-FEB-84	Put Y_DISPLAY into TMINC...nhe
 *	24-feb-84	SHOW-ASYNC, SHOW-WAITING...nhe
 *	2-mar-84	Add DISPLAY-PARFILE
 *	03-mar-84	Change defaults for show-wait...tnhe
 *	04-may-84	VALUE_x to xVAL and new_gbls to replace lookex... ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Move the IMPORT declarations...lim
 *			'toprefs'-->'numrefs'...nhe
 *	17-may-84	Add code to show_do function for SHOW and SHOW-LIB
 *			to honor control/c...lim
 *	04-jun-84	Implement default intrinsic subcommand...lim
 *	14-jun-84	New SHOW-BATCH...palm
 *	28-jun-84	Process parameter VARIABLE = "all"...lia
 *	03-aug-84	CLASS parameter for DISPLAY-PARFILE...palm
 *	12-oct-84	Make show_async and show_waiting conditional as in
 *			UNIX 1.2...dm
 *	28-oct-84	Add FULL for SHOW-ASYNC and SHOW-WAIT...nhe
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *	16-dec-84	TCL 67: DISPLAY to show "parm.qual="...peb
 *
 **************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	04-mar-85	Implement show-path command for rcjm...dm
 *
 ************************************************************************
 *
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	08-may-87	PR1181: changed "SHOW" Y_PDF to lower...ljn
 *	13-jul-87	Display qualifiers for every type of variable.
 *			The old "qualifier list" for -ALL has been
 *			removed...palm 
 *	22-jul-87	Add get_setshowcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *      27-aug-87       do casting on all to keep UNIX happy...tpl
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	26-jan-89	MAXVAL -> INT_MAXVAL for RESIDVAR...palm
 *	23-may-90	Removed RCJM stuff by referring to old TAE_RCJM...ljn
 *	28-jun-90	Removed get_setshowcmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"fileinc.inp"	/* file package				*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"terminc.inc"	/* terminal package			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/
#include	"eventinc.inp"	/* event control package		*/
#include "taeintproto.h"



FUNCTION  static BOOL  attention(void);
    static TEXT *all[] = {"ALL"};		/* default for jobname  */
						/*   and variable list  */
    static TEXT *brief[] = {"BRIEF"};		/* default for FORM in  */
						/*   SHOW-ASYNC & -WAIT */

    static struct RESIDVAR setlib_parms[] =
      {{"LIBRARY", V_STRING, 0, 0, INT_MAXVAL,  FSPECSIZ,  0,  NULL,   NULL}};

    static struct RESIDVAR display_parms[] =
      {{"VARIABLE", V_STRING, 0, 1, INT_MAXVAL,  F_Q_NAMESIZ,  1,  NULL, (GENPTR) all}};

#ifdef TAE_ASYNC

    BEGIN_VALIDS(sho_vald, 2)
{{"FULL"}, {"BRIEF"}}
    END_VALIDS

    static struct RESIDVAR shoasy_parms[] =
    {
      {"JOB",     V_STRING, 0, 1, INT_MAXVAL,  JOBNAMESIZ,  1,  NULL, (GENPTR) all},
      {"FORM",    V_STRING, 0, 1, 1,       sizeof("BRIEF"), 1, (GENPTR)&sho_vald,
       (GENPTR) brief},
    };
#endif

    BEGIN_VALIDS(disp_val, 4)
{{"ALL"}, {"PARMS"}, {"LOCALS"}, {"GLOBALS"}}
    END_VALIDS

    static  struct RESIDVAR  pt_disp[] = /* display selected variables */
	{
/* name		type	k minc  maxc	 size	dc	val   dvp */
	
	  {"FILE",     V_STRING,   0, 1,   1,    FSPECSIZ, -1,	NULL, NULL},
	  {"VARIABLE", V_STRING,   0, 1,   INT_MAXVAL,
	   F_Q_NAMESIZ,  1,   NULL, (GENPTR) all},
	  {"CLASS",    V_STRING,   0, 1,   1,     NAMESIZ,  1,    (GENPTR)&disp_val,
	   (GENPTR) all}
	};

FUNCTION CODE display_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE dispall_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE dispgbl_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE disploc_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE dispparm_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE set_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE setadd_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE setdel_do(struct CONTXT*,struct CONTXT*);
    FUNCTION CODE show_do(struct CONTXT*,struct CONTXT*);
#ifdef TAE_ASYNC
    FUNCTION CODE showasy_do(struct CONTXT*,struct CONTXT*);
#endif
    FUNCTION CODE restore_do(struct CONTXT*,struct CONTXT*);		/* yes, this does DISPLAY-PARFILE */


/*
 *	NOTE: DISPLAY has special flags so that $SFI and $SKEY are not cleared
 *       before DISPLAY activation.  You can therefore "DISPLAY $SFI".
 */

    GLOBAL struct ITRCMD setshowcmd[] =
    {
{4, "DISPLAY", "VARIABLE", Y_DISPLAY|Y_DEFSUB,
					1,   display_parms,  display_do },
{4, "DISPLAY", "ALL",      Y_DISPLAY,   0,   NULL,	     dispall_do	},
{4, "DISPLAY", "GLOBALS",  Y_DISPLAY,   0,   NULL,	     dispgbl_do	},
{4, "DISPLAY", "LOCALS",   Y_DISPLAY,   0,   NULL,	     disploc_do	},
{4, "DISPLAY", "PARMS",    Y_DISPLAY,   0,   NULL,	     dispparm_do},
{4, "DISPLAY", "PARFILE",  Y_DISPLAY,   I_NPM(pt_disp),
					     pt_disp,        restore_do},
{4, "SETLIB",  "SET",	   Y_GENERAL|Y_DEFSUB,	
					1,   setlib_parms,   set_do     },
{4, "SETLIB",  "ADD",      Y_GENERAL,	1,   setlib_parms,   setadd_do  },
{4, "SETLIB",  "DELETE",   Y_GENERAL,	1,   setlib_parms,   setdel_do  },
{2, "SHOW",    "LIBRARY",  Y_GENERAL|Y_DEFSUB,	
					0,   NULL,  	     show_do    },
#ifdef TAE_ASYNC
{2, "SHOW",    "ASYNC",    Y_GENERAL,	2,   shoasy_parms,   showasy_do },
{2, "SHOW",    "WAITING",  Y_GENERAL,	2,   shoasy_parms,   showasy_do },
#endif
{2, "show",    "BATCH",        Y_PDF,	0,   NULL,    	     NULL	},
{0, ""}		/* TERMINATOR ENTRY */
     };

    static    BOOL	attn_flag;	/* TRUE if ^c already set	*/

/*
 *	Do function for DISPLAY-VARIABLE.
 *
 */

    FUNCTION CODE display_do (

    struct CONTXT	*pctx,		/* current proc context		*/
    struct CONTXT	*cmdctx		/* DISPLAY command line context	*/
    )

    {
    struct VARIABLE	*v;		/* the variable to display		*/
    struct VARIABLE	*varname;	/* var containing name of var to display*/
    COUNT		vnum;		/* number of variables to display	*/
    CODE		code;
    BOOL		bad_name;	/* found an error			*/


    varname = lookex(&(*cmdctx).parmst, "VARIABLE");

    if ( (*varname).v_count == 1 && s_equal(SVAL(*varname, 0), "ALL"))
	{
	code = dispall_do(pctx, cmdctx);	/* same as display-all     */
	return (code);
	}
    else
	{
    	bad_name = FALSE;
	put_stdout("");
	for (vnum = 0; vnum < (*varname).v_count; vnum++)	/* for each var requested*/
	    {
	    v = search(SVAL(*varname, vnum), pctx);		/* find the variable	*/
	    if (v == NULL)
	        {
	    	tmmsg(PROCFAIL, "Reference to undefined variable '%s'.",
			"TAE-UNDEFVAR",
		      (uintptr_t)SVAL(*varname, vnum),0,0,0,0);
	    	bad_name = TRUE;
	    	continue;
	    	}
	    disp_var(v, SVAL(*varname,vnum));	
    	    disp_qual (v, SVAL(*varname, vnum));
	    }
	put_stdout("");
	if (bad_name)
	    return (DO_CHECK);
	return(DO_SUCCESS);
	}
    }

/*
 *	dispall_do.	Display all symbol tables.
 */

    FUNCTION  CODE  dispall_do(

    struct CONTXT	*pctx,		/* current proc context		*/
    struct CONTXT	*cmdctx		/* DISPLAY command line context	*/
    )

    {

    put_stdout ("");
    put_stdout ("**** displaying globals ****");
    dispgbl_do(pctx, cmdctx);
    if (attn_flag) return (SUCCESS);	/* control-c from operator	*/

    put_stdout (" **** displaying locals ****");
    disploc_do(pctx, cmdctx);
    if (attn_flag) return (SUCCESS);

    put_stdout (" **** displaying parameters ****");
    dispparm_do(pctx, cmdctx);
    if (attn_flag) return (SUCCESS);
    return SUCCESS;
    }

/*
 *	dispgbl_do.	display global parameters.
 *	NOTE: It displays global parameters at current proc activation
 *	      level only. At primary (interactive) level, all global
 *	      parameters are displayed.
 */

    FUNCTION  CODE  dispgbl_do(

    struct CONTXT	*pctx,		/* current proc context		*/
    struct CONTXT	*cmdctx		/* DISPLAY command line context	*/
    )

    {
    IMPORT  struct	SYMTAB	glbtab;  /* global symbol table		*/

    struct  VARIABLE	*v;
    COUNT		i;

    if ((*pctx).prclevel == 0)				/* if at primary level	*/
	{
	for (v=glbtab.link; v != NULL; v=(*v).v_link)	/* get each global	*/
	    {
	    if (attention()) break;			/* abort display on ^c	*/
	    disp_var(v, (*v).v_name);			/* display it		*/
    	    disp_qual (v, (*v).v_name);
	    }
	}
    else
	{
	if ((*pctx).numrefs == 0)
	    {
	    put_stdout("...none...");
	    }
	else
	    {
	    for (v=glbtab.link; v != NULL; v=(*v).v_link)	/* get alphabetically	*/
	    	{
	        if (attention()) break;			/* abort display on ^c */
		for (i=0; i < (*pctx).numrefs; i++)	/* scan thru current table	*/
		    {
		    if (v != (*pctx).refs[i]) continue;	/* no  match 		*/
		    disp_var(v, (*v).v_name);		/* else, display it	*/
    	            disp_qual (v, (*v).v_name);		
		    break;				/* go for next one	*/
		    }
		}
	    }
	}
    put_stdout("");
    return (SUCCESS);
    }

/*
 *	disploc_do.	display local parameters.
 */


FUNCTION  CODE  disploc_do
(
    struct CONTXT	*pctx,		/* current proc context		*/
    struct CONTXT	*cmdctx	/* DISPLAY command line context	*/

 )
    {
    struct  VARIABLE	*v;

    if ((*pctx).locst.link == NULL)
	put_stdout("...none...");
    else
	{
	for (v=(*pctx).locst.link; v != NULL; v=(*v).v_link)  /* get each local */
	    {
	    if (attention()) break;		  /* abort display on ^c	*/
	    disp_var(v, (*v).v_name);		  /* display it     */
    	    disp_qual (v, (*v).v_name);	
	    }
	}
    put_stdout("");
    return (SUCCESS);
    }

/*
 *	dispparm_do.	display all parameters.
 */


    FUNCTION  CODE  dispparm_do(

    struct CONTXT	*pctx,		/* current proc context		*/
    struct CONTXT	*cmdctx		/* DISPLAY command line context	*/
    )

    {
    struct  VARIABLE	*v;

    if ((*pctx).parmst.link == NULL)
        put_stdout("...none...");
    else
	{
	for (v=(*pctx).parmst.link; v != NULL; v=(*v).v_link)  /* get each parm */
	    {
	    if (attention()) break;			/* abort display on ^c	*/
	    disp_var(v, (*v).v_name);			/* display parameter	*/
    	    disp_qual (v, (*v).v_name);
	    }
	}
    put_stdout("");
    return (SUCCESS);
    }

/*	disp_qual.  display qualifiers (if any).
 *
 *	(The variable name has to be passed into here because
 *	(*v).v_name only gives the finest level name and 
 *	display needs the fully-qualified name.
 *
 */

FUNCTION VOID disp_qual 
(	
	struct VARIABLE *v,	/* in: variable with (maybe) qualifiers */
	TEXT		name[]	/* in: qualified name of variable 	*/

	)
    {
    TEXT		name_buf[STRINGSIZ+1];
    struct VARIABLE 	*vq;

    for (vq=(*v).v_qualst.link; vq != NULL; vq=(*vq).v_link)  /* get each qual */
	{
	if (attention()) break;			/* abort display on ^c	*/
	s_copy (name, name_buf);
	s_append (".", name_buf);
	s_append ((*vq).v_name, name_buf);	/* makes "parm.qual"	*/
	disp_var (vq, name_buf);		/* display parameter	*/
        disp_qual (vq, name_buf);		/* get next level	*/
	}
    return;
    }

/*
 *	disp_var.  Display the variable.
 *
 *	NOTE: if variable is a parameter qualifier, caller will pass in
 *	the full variable name (i.e., "parm.qual").  
 *
 *	We don't automatically display qualifiers because some
 *	traditional callers may not want such.
 */

FUNCTION  VOID  disp_var
(
    struct  VARIABLE 	*v,		/* in: variable being displayed		*/
    TEXT		var_name[]	/* in: name to use in var display	*/

 )
    {
    IMPORT struct SFILE	*pstdo_fil;	/* standard output file		*/
    IMPORT COUNT	termcols;	/* column width of terminal	*/

    COUNT		endcol;		/* number of columns for display	*/
    COUNT		length;
    COUNT		i;
    TEXT		outbuf[STRINGSIZ+1];
    CODE		code;

    outbuf[0] = EOS;				/* initialize		*/
    endcol   = (pstdo_fil == NULL)  ?  termcols : STRINGSIZ;
    m_fpname(var_name, outbuf, endcol);		/* get name 		*/
    v = RESOLVE(v);
    if (v == NULL)				/* NULL v_ref		*/
	{
	s_append (" (no reference)", outbuf);
	put_stdout (outbuf);
	return;
	}
    for (i=0; i < (*v).v_count; i++)		/* for each value	*/
	{
	code = m_fpval(v, i, outbuf, endcol,
		MAX( endcol, STRINGSIZ));		/* put in buffer */
	if (code == FAIL)				/* no more room	 */
	    {
	    put_stdout(outbuf);				/* write the line */
	    outbuf[0] = EOS;			
	    --i;				/* ignore last increment */
	    }
	}
    if ((*v).v_count > 0)
	{
	length= s_length(outbuf);
	outbuf[length-2] = EOS;			/* delete last ','	*/
	}
    else if ((*v).v_count == 0)
        s_append (" -- (null value)", outbuf);
    else
	s_append(" (no value)", outbuf);
    put_stdout(outbuf);				/* final flush of output buffer	*/
    return;
    }

/*
 *      attention. Check for operator attention.
 */

    FUNCTION  static BOOL  attention(void)

    {
    IMPORT  struct  ECB	ecbi;			/* operator attn ecb	*/


    attn_flag = e_occur(&ecbi);			/* TRUE if ^c occured      */
    return (attn_flag);
    }

/*
 *	Do function for SETLIB-SET.
 */
    FUNCTION CODE set_do(

    struct CONTXT	*c,		/* current context		*/
    struct CONTXT	*n		/* SETLIB context		*/
    )

    {
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/

    struct VARIABLE	*aplib;		/* $APLIB 			*/
    struct VARIABLE	*setlib;	/* SETLIB command		*/
    TEXT		**av;		/* value vector for $APLIB	*/
    TEXT		**sv;		/* value vector for SETLIB	*/
    TEXT		*newval[MAXVAL];
    COUNT		i,j,k;
    CODE		code;

    setlib = lookex(&((*n).parmst), "LIBRARY");
    aplib =  apl_gbl;
    sv = (TEXT **) (*setlib).v_cvp;
    av = (TEXT **) (*aplib).v_cvp;

    /* strategy: build new vector locally in newval then set_value	*/

    j = 0;					/* index in newval	*/
    for (i=0; i < (*setlib).v_count; i++)	/* for each SETLIB lib	*/
	if (s_equal(sv[i], "*"))
	    {
	    for (k=0; k < (*aplib).v_count; k++) /* copy $APLIB here	*/
		{
		if (j >= (*aplib).v_maxc)
 		    goto too_many;
		newval[j++] = av[k];
		}
	    }
	else
	    {
	    if (j >= (*aplib).v_maxc) goto too_many;
	    newval[j++] = sv[i];
	    }
    code = set_value (aplib, (GENPTR)newval, j);
    if (code != SUCCESS)
        goto no_memory;
    return(DO_CHECK);

too_many:
    tmmsg(PROCFAIL, "More than %d libraries specified.",
          "TAE-LIBS", (uintptr_t)(*aplib).v_maxc,0,0,0,0);
    return(DO_CHECK);

no_memory:
    overr();					/* report no memory		*/
    return(DO_CHECK);
    }

/*
 *	Do function for SETLIB-ADD.
 */
    FUNCTION CODE setadd_do(

    struct CONTXT	*c,		/* current context		*/
    struct CONTXT	*n		/* SETLIB context		*/
    )

    {
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/

    struct VARIABLE	*aplib;		/* $APLIB 			*/
    struct VARIABLE	*setlib;	/* SETLIB command		*/
    TEXT		**av;		/* value vector for $APLIB	*/
    TEXT		**sv;		/* value vector for SETLIB	*/
    TEXT		*newval[MAXVAL];
    COUNT		i,j;
    CODE		code;

    setlib = lookex(&((*n).parmst), "LIBRARY");
    aplib =  apl_gbl;
    sv = (TEXT **) (*setlib).v_cvp;
    av = (TEXT **) (*aplib).v_cvp;

    /* strategy: build new vector locally in newval then set_value	*/

    if ((*setlib).v_count + (*aplib).v_count > (*aplib).v_maxc)
	goto too_many;
    for (i=0, j=0; i < (*aplib).v_count; i++)	/* start with $APLIB	*/
	newval[j++] = av[i];
    for (i=0; i < (*setlib).v_count; i++)	/* tack on LIBRARY=	*/
	newval[j++] = sv[i];
    code = set_value (aplib, (GENPTR)newval, j);
    if (code != SUCCESS)
        goto no_memory;
    return(DO_CHECK);

too_many:
    tmmsg(PROCFAIL, "More than %d libraries specified.",
          "TAE-LIBS", (uintptr_t)(*aplib).v_maxc,0,0,0,0);
    return(DO_CHECK);

no_memory:
    overr();					/* report no memory		*/
    return(DO_CHECK);
    }

/*
 *	Do function for SETLIB-DELETE.
 */

    FUNCTION CODE setdel_do(

    struct CONTXT	*c,		/* current proc context		*/
    struct CONTXT	*n		/* this command context		*/
    )

    {
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/

    struct VARIABLE	*setlib;
    struct VARIABLE	*aplib;	
    TEXT		*newval[MAXVAL];
    TEXT		*newval1[MAXVAL];
    BOOL		found;
    BOOL		code;
    COUNT		i,j;
    TEXT		**av;
    TEXT		**sv;
    
    setlib = lookex(&((*n).parmst), "LIBRARY");
    aplib =  apl_gbl;
    sv = (TEXT **) (*setlib).v_cvp;
    av = (TEXT **) (*aplib).v_cvp;

/*  Strategy: build new value vector in newval and then copy it into $APLIB	*/

    for (i=0; i < (*aplib).v_count; i++)	/* copy of $APLIB	*/
        newval[i] = av[i];		
    for (i=0; i < (*setlib).v_count; i++)	/* for each LIBRARY	*/
        {
        if (s_equal(sv[i], "*"))		/* clear all libraries	*/
      	    {
            set_value(aplib, NULL, 0);		/* set $aplib null	*/
            return (DO_CHECK);
            }
	found = FALSE;
	for (j=0; j < (*aplib).v_count; j++)	/* lookup this LIBRARY	*/
	    if (s_equal(av[j], sv[i]))
		{
		found = TRUE;
		newval[j] = NULL;		/* "revmove" from list	*/
		}
	if (!found)
	  tmmsg(PROCFAIL, "Library '%s' not found", "TAE-MISSLIB", (uintptr_t)sv[i],0,0,0,0);
	}
    for (i=0, j=0; i < (*aplib).v_count; i++)
	if (newval[i] != NULL)
	    newval1[j++] = newval[i];	/* make new vector; note that 	*/
					/* set_value frees the strings	*/

    /* Note on set_value call: we are playing fair here because set_value
     * makes dynamic copies of the new value strings before deleting the
     * existing $aplib strings.
     */
    code = set_value(aplib, (GENPTR)newval1, j);
    if (code != SUCCESS)
        overr();
    return (DO_CHECK);
    }

/*
 *	Do function for SHOW-LIBRARY.
 */

    FUNCTION CODE show_do(

    struct CONTXT	*c,			/* current context		*/
    struct CONTXT 	*n			/* context of SHOW		*/
    )

    {
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/
    IMPORT  struct  VARIABLE	*usrl_gbl;	/* pointer to $USERLIB 	*/
    IMPORT  struct  VARIABLE	*sysl_gbl;	/* pointer to $SYSLIB 	*/

    COUNT		i;
    struct VARIABLE	*v;	
    TEXT		**vv;
    BOOL		any_found;

    put_stdout(" ");				/* blank line		*/
    v = usrl_gbl;
    vv = (TEXT **) (*v).v_cvp;
    put_stdout("User Library ($USERLIB):");
    if ( (*v).v_count == 0 || NULLSTR(vv[0]))
        put_stdout("...none...");
    else
        put_stdout(vv[0]);

    put_stdout(" ");				/* blank line		*/
    put_stdout("Application Libraries ($APLIB): ");
    any_found = FALSE;
    v = apl_gbl;
    vv = (TEXT **) (*v).v_cvp;
    for (i=0; i < (*v).v_count; i++)
	{
	if (NULLSTR(vv[i])) continue;		/* ignore null names	*/
	any_found = TRUE;
	if (attention()) return (DO_CHECK);	/* abort show on ^c     */
	put_stdout(vv[i]);		
	}
    if (!any_found) put_stdout("...none...");

    put_stdout(" ");				/* blank line		*/
    v = sysl_gbl;
    vv = (TEXT **) (*v).v_cvp;
    put_stdout("System Library ($SYSLIB):");
    if ((*v).v_count == 0 || NULLSTR(vv[0]))
        put_stdout("...none...");
    else
        put_stdout(vv[0]);
    put_stdout(" ");				/* blank line		*/
    return (DO_CHECK);
    }


#ifdef TAE_ASYNC
/*  showasy_do - for SHOW-ASYNC and (??) SHOW-WAITING
 *
 */

    FUNCTION CODE showasy_do (

    struct CONTXT	*procctx,		/* current context	*/
    struct CONTXT 	*cmdctx			/* context of SHOW	*/
    )

    {
    CODE		code;


    code = showasy(procctx, cmdctx);		/* let async routines do it */
    return(code);
    }
#endif
