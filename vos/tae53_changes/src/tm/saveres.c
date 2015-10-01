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
 *	This file contains the function to perform save and restore
 *	of TCL variables.
 *
 *	CHANGE LOG:
 *
 *	06-nov-83	Add RESTORE subcommand of -all
 *			for consistency with SAVE...palm
 *	23-nov-83	Close save/restore files before error exit...dm
 *	16-feb-84	New subcommands and new file format...nhe
 *	28-feb-84	Updates to same...palm
 *	02-mar-84	Implement display-parfile...palm
 *	02-mar-84	Exclude on restore not save....palm
 *	16-mar-84	Fix double defs of globals from procedures and
 *			dis-allow creation of parms...palm
 *	21-mar-84	Put the CODE into save_sel function...nhe
 *	04-may-84	VALUE_x to xVAL ...ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *			'toprefs'-->'numrefs'...nhe
 *	16-may-84	Add code to restore_do function for DISPLAY-PARFILE
 *			to honor control/c...lim
 *	8-jun-84	TCL-99: don't create global if level 0 local
 *			of same name exists...palm
 *	14-jun-84	Default intrinsic subcommands...lia
 *	25-jun-84	Remove TBD that is now PR639...palm
 *	29-jun-84	Process parameter VARIABLE = "all"...lia
 *	02-aug-84	Save variables in same order as symbol table
 *			so display-parfile looks better;
 *			Implement DISPLAY-PARFILE with class select...palm
 *	16-aug-84	pr396: I/O error key consolidation...peb
 *	10-sep-84	pr 639: New flags in vmove() calling seq...peb
 *	15-sep-84	pr 639: symb_up calls specvcopy instead of vcopy...peb
 *	29-oct-84	Additional exclude list for restore at remote node..dm
 *	13-nov-84	TCL 117: pblk_out to check parblk for compiling...peb
 *	20-nov-84	TCL 67: use v_pv12...peb
 *	04-nov-84	Conditinalize code needed by rcjm remote agent...dm
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *	16-dec-84	TCL 67: new disp_var call seq, DISPLAY_PARFILE quals...peb
 *
 ****************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	16-jan-85	Retract remote exclude logic from exclude()...dm
 *****************************************************************************	
 *
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	16-jul-86	Add $JOB to exclude list ....palm
 *	17-sep-86	Add $PARENT to exclude list...palm
 *	14-jul-87	Display qualifiers for DISPLAY-PARFILE...palm
 *	22-jul-87	Add get_saverescmd() as part of effort to force TM 
 *			into libraries...ljn
 *	07-aug-87	PR1223: zero PARHDR and PARBLK before writing so
 *			that spare fields have zeros and can be used
 *			for something meaningful in future...palm
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	06-dec-87	Set p.last in last record of PAR file;  use
 *			LARGE_PARBLK for restore; set new PARHDR.filename field;
 *			allow SAVE-VARIABLE variable name of "X.*" to
 *			save the qualifiers of X; change SAVE subcommand names
 *			to PARMS, GLOBALS, LOCALS rather than singular...palm
 *	12-feb-88	Change to pblk_out to fix recursion problem; add
 *			new argument  to pblk_out for pool size...palm
 *	15-feb-88	Fix bug in above recursion call in pblk_out...palm
 *	22-feb-88	Fix pblk_out to maintain same msgtyp for all
 *			PARBLKs written in order to make compiled files
 *			be correctly formed; fixed pblk_out to zero_block
 *			using the correct size...palm 
 *	24-mar-88	Apollo requires braces on static struct initializers..ln
 *	26-jan-89	MAXVAL -> INT_MAXVAL for RESIDVAR...palm
 *	08-feb-89	Use new parhdrRead function...palm
 *	28-jun-90	Removed get_saverescmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	11-may-92	Used P_BIGSENTINEL instead of P_SENTINEL...tpl
 *	13-may-93	LARGE_PARBLK is larger then max VMS record size.
 *			Use Vm_parbwrite and Vm_parbread instead of f_bwrite
 *			and f_bread to work-around this limitation...cew
 */

#include	"taeconf.inp"		/* TAE configuration definitions */
#include	"tminc.inc"		/* TM host-independent defs	 */
#include	"symtab.inc"		/* Symbol table			 */
#include	"parblk.inc"		/* Parameter block defs		 */
#include	"resinc.inc"		/* inc for restricted allocation */
#include	"fileinc.inp"		/* file primitive		 */
#include	"tmhost.inp"
#include	"eventinc.inp"		/* event control package	 */
#include	"compiled.inc"		/* for proc compilation		*/
#include "taeintproto.h"

FUNCTION CODE set_options 
(
    struct CONTXT	*cmdctx,	/* in: cmd context	             */
    TEXT		*var[],		/* out: array of   select names      */
    FUNINT		*varcnt,	/* out: number of select names	     */
    TEXT		*target[],	/* out: array of target names        */
    BOOL		*select,	/* out: true if selected variables   */
    BOOL		*global,	/* out: true if 'global' subcommand  */
    BOOL		*local,		/* out: true if 'local' subcommand   */
    BOOL		*parm		/* out: true if 'parm' subcommand    */

 );
FUNCTION  CODE  symb_up 
(
    struct  CONTXT 	*ctx,		/* in: symbol table		*/
    struct  VARIABLE	*var,		/* in: variable 		*/
    TEXT		*targ		/* in: targets variable, or NULL */
	    				/* if !NULL, target must exist   */
 );
FUNCTION CODE save_sel 
(
    struct CONTXT	*procctx,	/* in: context for variables	*/
    struct VARIABLE	*list,		/* in: list, values of a varible*/
    struct SFILE	*f		/* in: opened file		*/

 );



/*
 * Memory-resident PDF data for commands performed in this file.
 * The parameters for each command are defined by a vector of
 * RESIDVAR structures.
 * The RESIDVAR structure is explained in the TMINC include file.
 */

    static TEXT *all[] = {"ALL"};		/* default for variable list */

    static  struct RESIDVAR  ptrest[] =		/* restore */
	{
	/* name		type	k  minc  maxc	 size	dc	val   dvp */
	
	  {"FILE",     V_STRING,   0,  1,   1,  FSPECSIZ, -1,	NULL, NULL}
	};


    static  struct RESIDVAR  ptrsel[] =		/* save selected variables */
	{
	/* name		type	k  minc  maxc	 size	dc	val   dvp */
	
	  {"FILE",     V_STRING,   0, 1,   1,      FSPECSIZ, -1,    NULL, NULL},
	  {"VARIABLE", V_STRING,   0, 1,   INT_MAXVAL, 
						F_Q_NAMESIZ, 1,  NULL, 
	   (GENPTR)all},
	  {"TARGET",   V_STRING,   0, 0,   INT_MAXVAL, 
	   F_Q_NAMESIZ, 0,  NULL, NULL}
	};

    static  struct RESIDVAR  ptsave[] =		/* save 	  */
	{
	/* name		type	k minc  maxc	 size	dc	val   dvp */
	
	  {"FILE",     V_STRING,   0, 1,   1,  FSPECSIZ, -1,	NULL, NULL}
	};

    static  struct RESIDVAR  ptssel[] =	 /* save/display selected variables */
	{
	/* name		type	k minc  maxc	 size	dc	val   dvp */
	
	  {"FILE",     V_STRING,   0, 1,   1,  FSPECSIZ, -1,	NULL, NULL},
	  {"VARIABLE", V_STRING,   0, 1,   INT_MAXVAL, 
	   F_Q_NAMESIZ, 1, NULL, 
	   (GENPTR) all}
	};




/*
 * ITRCMD  structures for SAVE and RESTORE commands.
 */


CODE 	restore_do(struct CONTXT*, struct CONTXT*),	save_do(struct CONTXT*, struct CONTXT*);

    GLOBAL struct  ITRCMD  saverescmd[] = 	/* save/restore commands */
	{
{4, "RESTORE", "VARIABLE", Y_GENERAL|Y_DEFSUB,
					I_NPM(ptrsel), ptrsel,  restore_do },
{4, "RESTORE", "ALL",	   Y_GENERAL,	I_NPM(ptrest), ptrest,  restore_do },
{4, "RESTORE", "GLOBALS",  Y_GENERAL,	I_NPM(ptrest), ptrest,  restore_do },
{4, "RESTORE", "LOCALS",   Y_GENERAL,	I_NPM(ptrest), ptrest,  restore_do },
{4, "RESTORE", "PARMS",    Y_GENERAL,	I_NPM(ptrest), ptrest,  restore_do },
{4, "SAVE",    "VARIABLE", Y_GENERAL|Y_DEFSUB,
					I_NPM(ptssel), ptssel,  save_do    },
{4, "SAVE",    "ALL",	   Y_GENERAL,	I_NPM(ptsave), ptsave,  save_do    },
{4, "SAVE",    "GLOBALS",  Y_GENERAL,	I_NPM(ptsave), ptsave,  save_do    },
{4, "SAVE",    "LOCALS",   Y_GENERAL,	I_NPM(ptsave), ptsave,  save_do    },
{4, "SAVE",    "PARMS",    Y_GENERAL,   I_NPM(ptsave), ptsave,  save_do    },
{0, ""},  /* TERMINATOR ENTRY, REQUIRED AT THE END */
	};


/*
 * 	restore_do. Restore local/global variables from saved file.
 *	This also handles the DISPLAY-PARFILE command.
 */

FUNCTION  CODE  restore_do
(
    struct  CONTXT	*cpctx,		/* current proc context 	  */
    struct  CONTXT	*npctx		/* 'restore' command line context */

 )
    {
    IMPORT  struct ECB	ecbi;		/* operator attn ecb		  */

    struct  VARIABLE	*v, *vp;
    BOOL    gbl_rest;			/* TRUE if globals to be restored */
    BOOL    loc_rest;			/* TRUE if locals to be restored */
    BOOL    parm_rest;			/* TRUE if parms to be restored */
    BOOL    gbl_read, loc_read, parm_read; /* TRUE if record type read */
    struct  PARHDR 	ph;		/* parameter file header      */
    struct  LARGE_PARBLK p;		/* parameter block           */
    struct  SFILE	f;		/* file I/O block           */
    COUNT		recsize;
    CODE		code;
    TEXT		*targets[MAXVAL];
    TEXT		*vars[MAXVAL];	
    BOOL		select;		
    BOOL		display, all;
    BOOL		parm_display=FALSE, local_display=FALSE,
			global_display = FALSE, all_display = FALSE, do_display;
    COUNT		i, varcnt;
    TEXT errmsg[STRINGSIZ+1], errkey[STRINGSIZ+1];

    code = set_options(npctx, vars, &varcnt, targets, &select, &gbl_rest,
    			 &loc_rest, &parm_rest);
    if (code != SUCCESS)
    	return (DO_CHECK);
    display = s_equal ((*npctx).pdf.name, "DISPLAY");
    if (display)
	{
	all_display = parm_display = local_display = global_display = FALSE;
	v = lookex (&(*npctx).parmst, "CLASS");
        if (s_equal(SVAL(*v,0), "ALL"))
	    all_display = TRUE;
	else if (s_equal(SVAL(*v,0), "PARMS"))
	    parm_display = TRUE;
	else if (s_equal(SVAL(*v,0), "GLOBALS"))
	    global_display = TRUE;
	else
	    local_display = TRUE;
	}
    v = lookex(&(*npctx).parmst, "FILE");	/* look for file name	*/
    code = f_opnspc(&f, SAVELUN, SVAL(*v, 0), "", "",
		    PAR_TYPE, F_READ);		/* open for read	*/
    if (code != SUCCESS)  goto open_err;	/* report error		*/
    code = parhdrRead (&f, &ph, errmsg, errkey);  /* read/check PARHDR record */
    if (code != SUCCESS)
	{
	  tmmsg (PROCFAIL, errmsg, errkey,0,0,0,0,0);
	return (DO_CHECK);	
	}


/*  read the parameter records and restore locals/globals/parms		*/

    gbl_read = loc_read = parm_read = FALSE; 	/* initialize		*/
    while (FOREVER)
	{					/* read each record 	*/
#if defined(vms) || defined(__VMS)
	code = Vm_parbread(&f, (GENPTR) &p, sizeof(p), &recsize);
#else
	code = f_bread(&f, (GENPTR) &p, sizeof(p), &recsize);
#endif
   	if (code != SUCCESS) break;
	makeabs(&p.symtab, p.pool);		/* make pointers absolute */
	if (chk_parblk((struct PARBLK*)&p) != SUCCESS)
	    goto format_err;			/* check internal integrity */
	if (select)
      	    {
	    for (i=0; i < varcnt; i++)		/* for each selection	  */
	        {
		if (vars[i] == NULL) continue;	/* already got this one	  */
		vp = lookex (&p.symtab, vars[i]);
		if (vp == NULL) continue;
		if (display)
		    {
		    if (e_occur(&ecbi)) goto ctrlc_exit;
		    disp_var (vp, vars[i]);
		    disp_qual (vp, vars[i]);
		    }
	   	else
		    symb_up (cpctx, vp, targets[i]);
		vars[i] = NULL;			/* mark this already found*/
		}
	    }
	else
	    {
	    for (vp=p.symtab.link; vp != NULL; vp = (*vp).v_link)
	        {
		if (display)
		    {
		    if (e_occur(&ecbi)) goto ctrlc_exit;
		    do_display = all_display ||
		                (parm_display && (*vp).v_class == V_PARM) ||
		    		(local_display && (*vp).v_class == V_LOCAL) ||
		    		(global_display && (*vp).v_class == V_GLOBAL) ;
		    if (do_display)
			{
			disp_var (vp, (*vp).v_name);
			disp_qual (vp, (*vp).v_name);
			}
		    }
		else if ((*vp).v_class == V_GLOBAL && gbl_rest)
		    {
		    gbl_read = TRUE;
		    symb_up (cpctx, vp, NULL);
		    }
		else if ((*vp).v_class == V_PARM && parm_rest)
		    {
		    parm_read = TRUE;
		    symb_up (cpctx, vp, NULL);
		    }
		else if ((*vp).v_class == V_LOCAL && loc_rest)
		    {
		    loc_read = TRUE;
		    symb_up (cpctx, vp, NULL);
		    }
		}
	    }
	}
    if (code != F_EOF)	goto read_err;	
    f_close(&f, F_KEEP);
    all = gbl_rest && parm_rest && loc_rest;
    if (!all && !select)
        {
	if (gbl_rest && !gbl_read)
	    tmmsg(PROCFAIL, "No global variables found.",
		  "TAE-NOGBLRES",0,0,0,0,0);
	if (loc_rest && !loc_read)
	    tmmsg(PROCFAIL, "No local variables found.",
		  "TAE-NOLOCRES",0,0,0,0,0);
	if (parm_rest && !parm_read)
	    tmmsg(PROCFAIL, "No parameters found.",
		  "TAE-NOPARMRES",0,0,0,0,0);
        }
    for (i=0; i < varcnt; i++)
	if (vars[i] != NULL)
	    tmmsg (PROCFAIL, "Variable '%s' not found.", "TAE-NOVAR", 
		   (uintptr_t)vars[i],0,0,0,0);
    return (DO_CHECK);

format_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Restore file not properly formatted.",
	  "TAE-FMTRESTR",0,0,0,0,0);
    return (DO_CHECK);

open_err:
    tmmsg(PROCFAIL, "Unable to open restore file. %s.",
	  "TAE-OPNRD", (uintptr_t)f.errmsg,0,0,0,0);
    return (DO_CHECK);

read_err:
    f_close(&f, F_KEEP);
    tmmsg(PROCFAIL, "Unable to read restore file. %s.",
	  "TAE-RDERR", (uintptr_t)f.errmsg,0,0,0,0);
    return (DO_CHECK);
ctrlc_exit:
    f_close(&f, F_KEEP);
    return (DO_CHECK);
    }

/*
 *	save_do.  Save global and/or local variables.
 */

    FUNCTION  CODE  save_do(cpctx, npctx)

    struct  CONTXT	*cpctx;		/* invoking proc context	*/
    struct  CONTXT	*npctx;		/* 'save' command line context	*/

    {
    IMPORT  struct SYMTAB glbtab;	/* global symbol table		*/

    FAST struct VARIABLE *v, *v2;
    TEXT    *subc;			/* ptr to subcmd string		*/
    struct  SFILE	f;		/* file I/O block		*/
    FAST    CODE	code;
    struct  SYMTAB	*st1, *st2, *st3; /* points to symbol tables	*/
    static struct  SYMTAB nullst = {NULL}; /* null symbol table		*/
    BOOL	all_classes;

    v = lookex(&(*npctx).parmst, "FILE");	/* look for file name	*/
    v2 = lookex(&(*npctx).parmst, "VARIABLE");	/* look for var'ble list*/
    subc = (*npctx).subcmd;			/* point to subcmd name	*/
    code = f_opnspc(&f, SAVELUN, SVAL(*v, 0), "", "",
		    PAR_TYPE, F_WRITE);		/* create the file	*/
    if (code != SUCCESS)  goto open_err;	/* report error		*/
    if (s_equal(subc, "VARIABLE") &&
	((*v2).v_count != 1 || !s_equal(SVAL(*v2, 0), "ALL")))
    	{
    	code = save_sel(cpctx, v2, &f);		/* save selected variables */
    	if (code != SUCCESS)
    	    {
    	    f_close(&f, F_DELETE);
    	    return(DO_CHECK);
    	    }
    	}
    else
        {
	st1 = st2 = st3 = &nullst;
	all_classes = s_equal (subc, "VARIABLE") || s_equal (subc, "ALL");
	if (all_classes || s_equal(subc, "GLOBALS"))
	    st1 = &glbtab;				
	if (all_classes || s_equal(subc, "LOCALS"))	
	    st2 = &(*cpctx).locst;
	if (all_classes || s_equal(subc, "PARMS"))
	    st3 = &(*cpctx).parmst;
	code = save_pfile(&f, 0, NULL, 0, st1, st2, st3);
	if (code != SUCCESS) goto write_err;
        }
    f_close(&f, F_KEEP);
    return (DO_CHECK);


open_err:
    tmmsg(PROCFAIL, "Unable to open save file. %s.",
	  "TAE-OPNWRT", (uintptr_t)f.errmsg,0,0,0,0);
    return (DO_CHECK);

write_err:
    f_close(&f, F_DELETE);
    tmmsg(PROCFAIL, "Unable to write to save file. %s.",
	  "TAE-SAVEWRT", (uintptr_t)f.errmsg,0,0,0,0);
    return (DO_CHECK);
    }


 /*
 * 	exclude. Check if a TCL variable is to excluded from restore.
 *	This function is called for save/restore/delete of locals and globals.
 *
 *	Function return codes:
 *
 *		TRUE -  should be excluded.
 *		FALSE - Not to be excluded.
 *
 */
FUNCTION  BOOL exclude
(
    struct  VARIABLE 	*v		/* in: pointer to global variable */

 )
    {

    FAST  COUNT		i;
    COUNT		num_ent;
    TEXT		**list;

    static TEXT	*gblist [] =  { "$SESSION",
    				"$RUNTYPE",
    			 	"$SFI",
    				"$SKEY",
				"$JOB",
#ifdef VICAR_EXCLUDE
                                "$SWITCH",
#else
				"$LASTCMD",
#endif
				"$LOG",
				"$PARENT",
				"$USERLIB"
#ifndef VICAR_EXCLUDE                           /* (we don't need it) */
				,
				"$TAPES",	/* followings for JPL only  */
				"$TFILE",
				"$TREC"
#endif
			        };

    static  TEXT *loclist [] = {"_STDOUT",
				"_ONFAIL",
				"_PROC",
				"_SUBCMD"
				};

#define  NUM_GBL  sizeof(gblist) / sizeof(TEXT *)	/* # of gbl entries */ 
#define  NUM_LOC  sizeof(loclist) / sizeof(TEXT *)	/* # of local entries */

    if ((*v).v_class == V_PARM)
	return (FALSE);
    num_ent = ((*v).v_class == V_GLOBAL) ? NUM_GBL : NUM_LOC;
    list = ((*v).v_class == V_GLOBAL) ? gblist : loclist;

    for (i=0; i < num_ent; i++)
	if (s_equal((*v).v_name, list[i])) 	
	    return (TRUE);			/* found in the exclude list */
    return (FALSE);				/* not found in the list     */
    }



/*
 *  save_sel - Save variables specified in list
 *
 *  returns SUCCESS/FAIL; error messages typed
 *
 */
FUNCTION CODE save_sel 
(
    struct CONTXT	*procctx,	/* in: context for variables	*/
    struct VARIABLE	*list,		/* in: list, values of a varible*/
    struct SFILE	*f		/* in: opened file		*/

 )
    {
    TEXT		**names;	/* pointer to name vector	*/
    TEXT		*name;		/* pointer to current name	*/
    CODE		code;
    COUNT		i;
    struct VARIABLE	*vector[MAXVAL];
    struct VARIABLE	*v;
    COUNT		l;
    BOOL		qual_save = FALSE;	/* assume not x.* save */

    names = (TEXT **)(*list).v_cvp;		/* point to name list  */
    for (i=0; i < (*list).v_count; i++)	
    	{
	name = names[i];			/* current name pointer */
        l = s_length (name);			/* length of name for...*/
        if (l > 2 && s_equal (".*", name + l - 2))  /* of form nnnn.* ?	*/
            {
	    if ((*list).v_count != 1)
	    	{
	        tmmsg (PROCFAIL, "Only one qualified name allowed.", 
		       "TAE-MULTIQUAL",0,0,0,0,0);
	        return (DO_CHECK);
	        }	    	    	  	          	    	    
	    *(name + l - 2) = EOS;		/* clip off the .*	*/
	    qual_save = TRUE;	
	    }
        v = usearch (name, procctx);		/* get variable	   */
    	if (v == NULL)
    	    {
    	    tmmsg(PROCFAIL, "Variable '%s' is undefined.  No file created.",
    		  "TAE-UNDEFVAR", (uintptr_t)name,0,0,0,0);
    	    return(FAIL);
    	    }
	if (qual_save)
	    break;
        vector[i] = v;
    	}
    if (qual_save)
      code = save_pfile(f, 0, NULL, 0, &(*v).v_qualst, 0, 0);
    else
      code = save_pfile(f, 0, vector, (*list).v_count, 0, 0, 0);	
    if (code != SUCCESS)
    	{
	tmmsg(PROCFAIL, "Unable to write to save file, %s",
	      "TAE-SAVEWRT", (uintptr_t)(*f).errmsg,0,0,0,0);
	return (FAIL);
    	}
    return(SUCCESS);
    }

/*
 *  set_options - Determine the user-selected options for restore
 *
 *  return SUCCESS or FAIL; error msg typed
 *
 *  Also sets the 'target' vector if we have targets
 *
 */
FUNCTION CODE set_options 
(
    struct CONTXT	*cmdctx,	/* in: cmd context	             */
    TEXT		*var[],		/* out: array of   select names      */
    FUNINT		*varcnt,	/* out: number of select names	     */
    TEXT		*target[],	/* out: array of target names        */
    BOOL		*select,	/* out: true if selected variables   */
    BOOL		*global,	/* out: true if 'global' subcommand  */
    BOOL		*local,		/* out: true if 'local' subcommand   */
    BOOL		*parm		/* out: true if 'parm' subcommand    */

 )
    {
    COUNT		i;
    struct VARIABLE	*p2, *p3;
    TEXT	**targ;
    TEXT	*subc;

    *varcnt = 0;
    *select = FALSE;
    p2 = lookex(&(*cmdctx).parmst, "VARIABLE");
    p3 = lookex(&(*cmdctx).parmst, "TARGET");	/* targets also selected	     */

    if (p2 == NULL || ((*p2).v_count == 1 && s_equal(SVAL(*p2, 0), "ALL")))
	{  					/* no variables specified ?  */
	subc = (*cmdctx).subcmd;
	*global = s_equal (subc, "GLOBALS");
	*local = s_equal (subc, "LOCALS");
	*parm = s_equal (subc, "PARMS");
        if (s_equal (subc, "ALL") || s_equal (subc, "VARIABLE"))
	    *global = *local = *parm = TRUE;
	if (p3 != NULL && (*p3).v_count != 0)
	    {
	    tmmsg(PROCFAIL,
		  "TARGET parameter not allowed with VARIABLE = 'ALL'.", "TAE-BADCOMBO",0,0,0,0,0);
	    return(FAIL);
	    }
    	}
    else
	{
	*select = *global = *local = *parm = TRUE;
    	for (i=0; i < (*p2).v_count; i++)
    	    var[i] = SVAL(*p2, i);
        *varcnt = (*p2).v_count;
	if (p3 != NULL && (*p3).v_count != 0)	/* target present?	*/
    	    {
    	    if ((*p2).v_count != (*p3).v_count)
    		{
    		tmmsg(PROCFAIL,
		      "VARIABLE count must equal TARGET count.", "TAE-BADTARG",0,0,0,0,0);
    		return(FAIL);
    		}
    	    targ = (TEXT **)(*p3).v_cvp;	/* point to array of names */
    	    }
    	else
    	    targ = var;				/* if no target use vars   */
    	for (i=0; i < (*p2).v_count; i++)
    	    target[i] = targ[i];
    	}
    return(SUCCESS);
    }


/*
 *	set_quals - set parameter qualifiers for an existing variable.
 *	The parameter qualifiers may or may not already exist.
 */

FUNCTION static CODE set_quals 
(
    struct VARIABLE	*targ,		/* in/out: variable to set quals of	*/
    struct SYMTAB	*q_st		/* in:  qualifier symbol table to set from*/

 )
    {
    struct VARIABLE	*sqv;		/* source qualifier var			*/
    struct VARIABLE	*tqv;		/* target qualifier var			*/
    CODE		code;

    for (sqv = (*q_st).link; sqv != NULL; sqv = (*sqv).v_link)
	{
	tqv = lookex(&(*targ).v_qualst, (*sqv).v_name);
	if (tqv == NULL)
	    {				/* must create qualifier		*/
	    tqv = allvar(&(*targ).v_qualst);
	    if (tqv == NULL)
		goto over_err;
	    code = specvcopy(sqv, tqv);
	    if (code != SUCCESS)
		goto copy_err;
	    }
	else				/* update val & nested qualifiers	*/
	    {
	    code = chk_vector(tqv, (*sqv).v_type,
			(*sqv).v_cvp, (*sqv).v_count, FALSE);
	    if (code != SUCCESS)
		goto format_err;
	    code = set_value(tqv, (*sqv).v_cvp, (*sqv).v_count);
	    if (code != SUCCESS)
		goto over_err;
	    code = set_quals(tqv, &(*sqv).v_qualst);	/* recursive - for nested*/
	    if (code != SUCCESS)
		goto qual_err;
	    }
	}
    return(SUCCESS);

over_err:
    overr();
    return(FAIL);

copy_err:
    return(FAIL);

format_err:
    tmmsg(PROCFAIL, "Mismatch restoring qualifier '%s' for parameter '%s'.",
	  "TAE-MISMATCH", (uintptr_t)(*sqv).v_name, (uintptr_t)(*targ).v_name,0,0,0);
    return (FAIL);

qual_err:
    return (FAIL);
    }

/*
 * 	symb_up - Update a symbol table with provided variable, adding the
 *	variable if it doesn't exist.  (But we don't create parms.)
 *
 *  returns SUCCESS or FAIL; outputs err msg if FAIL
 */

FUNCTION  CODE  symb_up 
(
    struct  CONTXT 	*ctx,		/* in: symbol table		*/
    struct  VARIABLE	*var,		/* in: variable 		*/
    TEXT		*targ		/* in: targets variable, or NULL */
	    				/* if !NULL, target must exist   */
 )
    {
    IMPORT  struct SYMTAB glbtab;	/* global symbol table		*/
    IMPORT  struct CONTXT primctx;	/* level 0 context		*/

    struct  VARIABLE	*v2;
    struct  VARIABLE    *save_link;		
    CODE		code, type, class;

    type = (*var).v_type;
    class = (*var).v_class;
    if (targ == NULL)		
	v2 = usearch ((*var).v_name, ctx);
    else
    	{
    	v2 = usearch (targ, ctx);
    	if (v2 == NULL)
    	    goto undef_err;			/* targets must exist	    */
    	}
    if (v2 == NULL && (*var).v_class == V_GLOBAL)
	v2 = lookex (&glbtab, (*var).v_name);	/* not REF'd but a global   */
    if (v2 == NULL)
	{					/* must create variable	    */
	if (class == V_GLOBAL)
	    {
	    if (lookex (&primctx.locst, (*var).v_name) != NULL)
	        goto local_conflict;
	    v2 = alpha_var((*var).v_name, &glbtab);
	    }
	else if (class == V_LOCAL)
	    v2 = allvar (&(*ctx).locst);
	else					/* don't create parms	    */
	    goto parm_err;
	if (v2 == NULL) goto over_err;		/* no memory available      */
	save_link = (*v2).v_link;		/* save link to next global */
	if (specvcopy(var, v2) == FAIL) 	/* copy the variable	    */
	    return (FAIL);		
	(*v2).v_link = save_link;		/* vcopy had nulled it	    */
	}
    else
	{
	if (exclude(v2))			/* protect _PROC, etc.	*/
	    return (SUCCESS);
	code = chk_vector(v2, type, (*var).v_cvp, (*var).v_count, FALSE);
	if (code != SUCCESS) goto format_err;
	if (set_value(v2, (*var).v_cvp, (*var).v_count) != SUCCESS)
	    goto over_err;
	if ((*var).v_pv12 && (*v2).v_pv12)	/* only if PARBLK from new TAE version	*/
	    if (set_quals(v2, &(*var).v_qualst) != SUCCESS)
		goto qual_err;
	}
    return (SUCCESS);	

format_err:
    tmmsg(PROCFAIL, "Mismatch restoring '%s'.", "TAE-MISMATCH", (uintptr_t)(*var).v_name,0,0,0,0);
    return (FAIL);

over_err:
    overr();
    return (FAIL);			

parm_err:
    tmmsg (PROCFAIL, "Parameter '%s' does not exist.",
	   "TAE-RESTPARM", (uintptr_t)(*var).v_name,0,0,0,0);
    return (FAIL);

undef_err:
    tmmsg(PROCFAIL, "Variable '%s' does not exist.",
	  "TAE-UNDEFVAR", (uintptr_t)targ,0,0,0,0);
    return(FAIL);

local_conflict:
    tmmsg (PROCFAIL,
	   "Global variable name '%s' conflicts with level 0 local variable.",
	   "TAE-LOCCONFLICT", (uintptr_t)(*var).v_name,0,0,0,0);
    return (FAIL);

qual_err:
    return(FAIL);		/* EM printed by set_quals			*/
    }

#ifdef XXXXXX
/*	This function is not currently used		*/
/*
 * 	pack_gbl. Build a PARBLK for currently referenced globals.
 *	
 *	NOTE: certain globals such as $SFI, $SKEY which are not included
 *	in the block as they need not be saved/restored.
 */

    FUNCTION  static CODE pack_gbl(ctx, par)

    struct CONTXT	*ctx;		/* input: current context block	*/
    struct PARBLK 	*par;		/* output: parameter block	*/

    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table		*/

    struct VARIABLE	*v;		/* working pointer		*/
    struct VARIABLE	*pcur;		/* current VARIABLE struct      */
    GENPTR		r_top();	/* top of storage area		*/
    COUNT		i;
    struct VARIABLE * vmove();		/* moves one variable		*/


    r_init((*par).pool, sizeof((*par).pool));	/* init storage area	*/
    (*par).symtab.link = NULL;		/* in case no variables		*/
    pcur = (struct VARIABLE *) &(*par).symtab;
				 /* assumes .link, .v_link at same position  */

    if ((*ctx).prclevel == 0)	
	{
	for (v=glbtab.link; v != NULL; v=(*v).v_link)	
	    {
	    pcur = (*pcur).v_link = vmove(v, (*par).pool, 0);  /* move and link in	*/
	    if (pcur == NULL) return (FAIL);
	    }
	}
    else
	{
	for (i=0; i < (*ctx).numrefs; i++)
	    {
	    v = (*ctx).refs[i];			
	    pcur = (*pcur).v_link = vmove(v, (*par).pool, 0);  /* move and link in	*/
	    if (pcur == NULL) return (FAIL);
	    }
	}

    (*par).last = TRUE;				/* only one block for now	*/
    makerel(&(*par).symtab, (*par).pool);  /* make all pool pointers relative*/
    (*par).blksiz = r_top((*par).pool) - (GENPTR)par ;  /* logical block size */
    return(SUCCESS);			
    }
#endif

/*
 *  save_pfile - Save the specified variables into a standard save files
 *
 *  We assume an opened file
 *
 *  returns SUCCESS/FAIL; error msg in sf
 *
 */

FUNCTION CODE save_pfile 
  (
    struct SFILE	*sf,		/* in: pointer to opened file context*/
    FUNINT		msgtype,	/* in: message type for p.msgtyp     */
    struct VARIABLE	*vector[],	/* in: array of variables to save    */
    FUNINT		vcnt,		/* in: number of vars in vector      */
    struct SYMTAB 	*symtab1,	/* in: ptr to array of symtab
					   ptrs   */
    struct SYMTAB 	*symtab2,	/* in: ptr to array of symtab
					   ptrs   */
    struct SYMTAB 	*symtab3	/* in: ptr to array of symtab
					   ptrs   */
   )
    {
    struct  PARHDR 	ph;		/* parameter file header	     */
    struct  LARGE_PARBLK p;		/* parameter block		     */
    CODE		code;
    COUNT		i;
    struct  VARIABLE	*v, *vx;
    struct FSBLOCK	fsblock;	/* file spec block		     */
    TEXT		ignore[STRINGSIZ+1];

/* 	Write the header					*/

    zero_block ((GENPTR)&ph, sizeof (ph));
    s_copy(P_BIGSENTINEL, ph.sentinel);
    ph.recsize = p.blksiz;			/* not really known now */
    s_copy("TIME", ph.datetime);		/* dummy date/time for now */
    fsblock.name[0] = EOS;			/* in case F_crack error   */
    f_crack((*sf).full_spec, "", "", "", &fsblock, ignore);
    s_bcopy (fsblock.name, ph.filename, sizeof (ph.filename) - 1);	        
    code = f_bwrite(sf, (GENPTR) &ph, sizeof(struct PARHDR));	
    if (code != SUCCESS)
    	return (code);
    zero_block ((GENPTR)&p, sizeof (p));
    p.msgtyp = msgtype;
    r_init(p.pool, sizeof(p.pool));		/* init storage area	*/

/* Pack all the variables in 'vector' into as many parblks as necessary.  */

    for (i=0; i < vcnt ; i++)
    	{
    	vx = vector[i];
    	code = pblk_out(sf, (struct PARBLK*)&p, sizeof (p.pool), vector[i]);
    	if (code != SUCCESS)
    	    return (code);
    	}

/* Pack all the variables in each 'symtab' into as many parblks as necessary.  */
    if(symtab1) {
      for (v=(*symtab1).link; v!=NULL; v = (*v).v_link)
	{
	  if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
	  code = pblk_out(sf, (struct PARBLK*)&p, sizeof(p.pool), v);
	  if (code != SUCCESS)
	    return (code);
	}
    }
    if(symtab2) {
      for (v=(*symtab2).link; v!=NULL; v = (*v).v_link)
	{
	  if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
	  code = pblk_out(sf, (struct PARBLK*)&p, sizeof(p.pool), v);
	  if (code != SUCCESS)
	    return (code);
	}
    }
    if(symtab3) {
      for (v=(*symtab3).link; v!=NULL; v = (*v).v_link)
	{
	  if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
	  code = pblk_out(sf, (struct PARBLK*)&p, sizeof(p.pool), v);
	  if (code != SUCCESS)
	    return (code);
	}
    }
    p.last = TRUE;				/* flag last record     */
    makerel(&p.symtab, p.pool);			/* make ptrs relative	*/
    p.blksiz = r_top(p.pool) - (GENPTR) &p;
#if defined(vms) || defined(__VMS)
    code = Vm_parbwrite(sf, (GENPTR) &p, p.blksiz);
#else
    code = f_bwrite(sf, (GENPTR) &p, p.blksiz);
#endif
    if (code != SUCCESS)
	return (code);
    return(SUCCESS);
    }

/*
 *  pblk_out - Accumulate variables in a parblk, and write it when full
 *
 *  returns SUCCESS or FAIL; error code in sfile
 *
 */
FUNCTION CODE pblk_out 
(
    struct SFILE	*sf,		/* in: file context		*/
    struct PARBLK	*par,		/* in/out: parblk		*/
    FUNINT		poolsize,	/* bytes in parblk.pool		*/
    struct VARIABLE	*var		/* in: the variable to add	*/

 )
    {
    CODE		msgtyp;		/* caller's message type        */
    struct VARIABLE	*vcur;		/* current variable pointer	*/
    CODE		code;
    struct VARIABLE	*v;		/* scratch VARIABLE pointer	 */

#define PARBLK_HEADER_SIZE   ((GENPTR)(*par).pool - (GENPTR)par)

    msgtyp = (*par).msgtyp;
    code = SUCCESS;
    if ((*par).msgtyp == C_BEFORE  ||	/* if PARBLK for proc compilation...	*/
	(*par).msgtyp == C_AFTER   ||	/* need to pass NAME parms thru to PARBLK*/
	(*par).msgtyp == C_SUBPAR)	
	vcur = vmove(var, (*par).pool, VM_PDF | VM_VALID | VM_NMREF);
    else
	vcur = vmove(var, (*par).pool, VM_PDF | VM_VALID);  /* move to pool */
    if ((*par).symtab.link == NULL  &&  vcur == NULL)
        return (FAIL);			/* no room for even one VAR	*/
    else if (vcur == NULL) 		/* no more room, write it	*/
    	{
    	makerel(&(*par).symtab, (*par).pool);
	(*par).blksiz = r_top((*par).pool) - (GENPTR)par ;
#if defined(vms) || defined(__VMS)
    	code = Vm_parbwrite(sf, (GENPTR) par, (*par).blksiz);	
#else
    	code = f_bwrite(sf, (GENPTR) par, (*par).blksiz);	
#endif
    	if (code != SUCCESS)
    	    return (code);
        zero_block ((GENPTR)par, PARBLK_HEADER_SIZE + poolsize); 
	r_init((*par).pool, poolsize);		/* init storage area	*/
	(*par).msgtyp = msgtyp;			/* restore 		*/
    	code = pblk_out(sf, par, poolsize, var);
    	}
    else
    	{
	for (v = (struct VARIABLE *) & (*par).symtab;
	     (*v).v_link != NULL;  v = (*v).v_link)
	     ;					/* find end of chain	*/
	(*v).v_link = vcur;			/* and link to old last */
        (*vcur).v_link = NULL;			/* this is last in chain*/
    	}
    return(code);
    }


#ifdef OBSOLETE
/*
 *  var_pull - Pull the named variables from the specified symbol table, and
 *	      build a new symbol table
 *
 *  returns FAIL on allocation failure
 *
 * We pull the variables in 'names'; if one is missing, we delete it from the
 * targets list, and add it to the missing list.
 *
 *
 */
    FUNCTION CODE var_pull (bigst, names, count, newst, target, targ2,
    				missing, m)

    struct SYMTAB	*bigst;		/* in: symbol table from which to get variables	*/
    TEXT		*names[];	/* in: list of variable names		*/
    FUNINT		count;		/* in: number of names			*/
    struct SYMTAB	*newst;		/* in/out: symbol table to build	*/
    TEXT		*target[];	/* in: list of targets			*/
    TEXT		*targ2[];	/* out: edited list of targets		*/
    TEXT		*missing[];	/* out: variables not found		*/
    COUNT		*m;		/* out: number not found		*/

    {
    COUNT		i, j;
    struct VARIABLE	*vold, *vnew, *lookex(), *allvar();
    CODE		code;
    GENPTR		s_save();


    for (i=0, j=0, *m=0; i<count; i++)
    	{
        vold = lookex (bigst, names[i]);	/* get variable              */
    	if (vold == NULL)
    	    {					/* not there, remember & go to next  */
    	    missing[*m] = s_save(names[i]);
    	    if (missing[(*m)++] == NULL)
    		{
    		overr();
    		return(FAIL);
    		}
    	    continue;
    	    }
    	targ2[j++] = target[i];			/* add to final targets list */
    	if ((vnew = allvar(newst)) ==NULL)	/* allocate and link new variable	*/
    	    {
    	    overr();				/* allocation failure        */
    	    return(FAIL);
    	    }
    	code  = vcopy(vold, vnew);		/* copy the variable structure	*/
	(*vnew).v_default = TRUE;		/* current is now default	*/
    	}
    return(SUCCESS);
    }
#endif
