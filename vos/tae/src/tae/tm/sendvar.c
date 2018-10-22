/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*	SENDVAR/RECVAR/ENABLE-RECVAR.  Interprocess communication.
 *
 * CHANGE LOG:
 *
 *	12-sep-86	Move in additional changes made for SUN UNIX :
 *			- fix declaration of variable jv in sendvar_do()
 *			- move maintenance of enable indicator to c_ 
 *			  functions...lia
 *	17-sep-86	Remove JOB parm from ENABLE-RECVAR and use
 *			$JOB for path name....palm
 *	22-sep-86	Fix bad call to chk_vector...palm
 *	29-sep-86	Rename error msg [TAE-ENABLED] to [TAE-PATHENABLE],
 *			[TAE-PATHERR] to [TAE-PATH], and 
 *			[TAE-COMM] to [TAE-COMMERR]...lia
 *	22-jul-87	Add get_sendcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	24-mar-88	Apollo requires braces on static array initializers...ln
 *	26-jan-89	MAXVAL -> INT_MAXVAL in RESIDVR...palm
 *	28-jun-90	Removed get_sendcmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *
 */

#include	"taeconf.inp"		/* TAE configuration definitions */
#include	"tmhost.inp"		/* host-dependent defs		 */
#include	"comminc.inp"		/* c_ definitions		 */
#include	"symtab.inc"		/* symbol table			 */
#include	"tminc.inc"		/* TM-only host-independent      */
#include	"parblk.inc"

    GLOBAL int	v10sendv = 0;		/* source version                */


GLOBAL struct PATH rec_path = {0};	/* input port for RECVAR	  */
struct VARIABLE	*search();		/* find VARIABLE from context     */
struct VARIABLE	*lookex();		/* find a parm in sym tab	  */
struct VARIABLE *vmove();
GENPTR r_top();

    static struct RESIDVAR send_par[] =  
	{
/* name    type      k  m    maxc       size        dc  val      dvp      */

"VARIABLE",V_STRING, 0, 0, INT_MAXVAL, NAMESIZ+1,     0, NULL,    NULL,
"JOB",     V_STRING, 0, 1,      1,  JOBNAMESIZ+1,    -1, NULL,    NULL
	};


static TEXT *jobvv[] = {"ALL"};

    static struct RESIDVAR recvar_par[] =  
	{
/* name    type      k  m    maxc       size        dc  val      dvp      */

"VARIABLE",V_STRING, 0, 0, INT_MAXVAL, NAMESIZ+1,    0, NULL,    NULL,
"JOB",     V_STRING, 0, 1,      1,  JOBNAMESIZ+1,    1, NULL,    (GENPTR)jobvv
	};


/*	quotas are such a problem, we allow user sizing of the mailbox: */
 
    static struct I_VALID size_valid = {1, 700, sizeof (struct PARBLK)};
    static TAEINT def_size[] = {sizeof(struct PARBLK)/2} ;

    static struct RESIDVAR enable_par[] =
	{
/* name    type      k  m    maxc       size        dc  val      dvp      */

"SIZE",   V_INTEGER, 0, 1,      1,             0,    1, (GENPTR)&size_valid,
								(GENPTR)def_size
	};


CODE sendvar_do(), recvar_do(), enable_recvar(), disable_recvar();


    GLOBAL struct ITRCMD sendcmd[] = 	/* send/receive commands  */
	{
{0, "SENDVAR",	"",      Y_GENERAL,  I_NPM(send_par), send_par,     sendvar_do},
{0, "RECVAR","RECEIVE",  Y_GENERAL|Y_DEFSUB,  
				     I_NPM(recvar_par), recvar_par,  recvar_do},
{0, "RECVAR","ENABLE",   Y_GENERAL,  I_NPM(enable_par),
						    enable_par,  enable_recvar},
{0, "RECVAR","DISABLE",   Y_GENERAL,  0, 	          NULL, disable_recvar},
{0,""} 			/* terminator entry */
	};


/*	enable-recvar.     Create input mailbox so
 * 	the RECVAR command can be used to receive PARBLKs.
 *
 *	Under VMS, the mailbox is created as temporary, so
 *	you must play with logical name tables to send something
 *	to this TM from another task.  See $CREMBX in "System Services".
 */
 
    FUNCTION CODE enable_recvar (proc_ctx, cmd_ctx)

    struct CONTXT *proc_ctx;	/* in/out: proc context	*/
    struct CONTXT *cmd_ctx;	/* in: SENDVAR context  */

    {
    IMPORT CODE run_type;		/* current TM context		*/
    IMPORT struct VARIABLE *job_gbl;	/* $JOB TCL global		*/
    struct VARIABLE  *s;		/* ptr to SIZE parameter 	*/
    TEXT *path_name;
    CODE code;
    
    s  = lookex (&(*cmd_ctx).parmst, "SIZE");
    path_name = SVAL(*job_gbl,0);		/* use current job name	    */
    if (rec_path.name[0] != 0)			/* path already exists	*/
        {
        tmmsg (SUCCESS, "RECVAR already enabled.", "TAE-PATHENABLE");
        return (DO_CHECK);
        }
    code = c_crepath (&rec_path, IVAL(*s,0), path_name, TMPMBX);
    if (code != SUCCESS)
	tmmsg (PROCFAIL, "Error creating RECVAR path. %s.", 
			"TAE-PATH", rec_path.errmsg);
    return (DO_CHECK);
    }





/*	disable_recvar.   Disable RECVAR command by
 *	dDeleting TM's mailbox ("path").
 *
 */

    FUNCTION CODE disable_recvar ()

    {
    c_delpath (&rec_path);
    return  (SUCCESS);
    }

/*	sendvar_do.   Send a PARBLK to the task identified by
 *	the JOB parameter.  (The JOB name is used as a path name.)
 *	The VARIABLE parameter lists the TCL variables to be
 *	in the PARBLK.
 *
 */

    FUNCTION CODE sendvar_do (proc_ctx, cmd_ctx)

    struct CONTXT *proc_ctx;	/* in/out: proc context	*/
    struct CONTXT *cmd_ctx;	/* in: SENDVAR context  */

    {
    IMPORT struct VARIABLE *skey_gbl;
    struct VARIABLE  jobv;
    static TEXT  *jv[1];	/* string value vector	*/
    struct VARIABLE  *vv;	/* ptr to VARIABLE parameter 	*/
    struct VARIABLE  *j;	/* ptr to JOB parameter 	*/
    struct VARIABLE  *pcur;	/* current link in SYMTAB	*/
    struct VARIABLE  *v; 	/* working variable ptr		*/
    struct PARBLK    parblk;	/* message to send out		*/
    TEXT errmsg[STRINGSIZ+1];
    CODE code;
    COUNT i;

    jv[0] = rec_path.name;
    vv = lookex(&(*cmd_ctx).parmst, "VARIABLE");   
    j  = lookex (&(*cmd_ctx).parmst, "JOB");
    r_init (parblk.pool, P_BYTES);	
    zero_block ((GENPTR)&parblk, (GENPTR)parblk.pool - (GENPTR)&parblk);
    parblk.symtab.link = NULL;
    parblk.last = TRUE;
    pcur = (struct VARIABLE *) & parblk.symtab;
    for (i=0; i < (*vv).v_count; i++)		/* for each VARIABLE:  */
        {
	v = search (SVAL(*vv, i),  proc_ctx);	/* get ref'ed variable */
	if (v == NULL)
	    {
	    tmmsg (PROCFAIL, "Variable '%s' is undefined.",
	    		"TAE-UNDEFVAR", SVAL(*vv, i));
	    return (DO_CHECK);
	    }
	pcur = (*pcur).v_link = vmove(v, parblk.pool, 0);  /* put v in parblk */
	if (pcur == NULL)
    	    goto overflow;
	}        
    MOVE_STRUCT (*skey_gbl, jobv);		/* use $SKEY as skeleton...  */
    s_copy ("_JOB", jobv.v_name);		/* for scratch _JOB variable */
    jobv.v_cvp = (GENPTR) jv;				/* give it our job name	     */
    jobv.v_class = V_LOCAL;			/* and make it a local	     */
    (*pcur).v_link = vmove(&jobv, parblk.pool, 0);  /* put _JOB in parblk    */
    makerel (&parblk.symtab, parblk.pool);	        /* make ptrs relative */
    parblk.blksiz = r_top(parblk.pool) - (GENPTR)&parblk;	
    code = c_pmwcd (SVAL(*j,0), (GENPTR)&parblk, parblk.blksiz, errmsg); 
    if (code != SUCCESS)
        {
        tmmsg (PROCFAIL, "Error transmitting block.  %s.", 
		"TAE-COMMERR", errmsg);
        return (DO_CHECK);
        }
    return (SUCCESS);



overflow:
    tmmsg (PROCFAIL, "VBLOCK overflow. Too much data.", "TAE-VBLOCK");
    return (DO_CHECK);
    }



/*	recvar_do.   Receive TCL variables from another task.
 *
 *	The VARIABLE list indicates the candidate variables to
 *	be received and the JOB parameter restricts receipt to
 *	the indicated job.  A job name of "ALL" accepts from
 *	any job.
 */

    FUNCTION recvar_do (proc_ctx, cmd_ctx)
    
    struct CONTXT *proc_ctx;	/* in/out: proc context	*/
    struct CONTXT *cmd_ctx;	/* in: SENDVAR context  */

    {
    struct VARIABLE  *vv;	/* ptr to VARIABLE parameter 	*/
    struct VARIABLE  *j;	/* ptr to JOB parameter 	*/
    struct VARIABLE  *jp;	/* ptr to _JOB VARIABLE in parblk*/
    struct VARIABLE  *vfrom, *vto;
    struct PARBLK parblk;	/* receive buffer		*/
    COUNT  size, i;
    CODE code;

    if (rec_path.name[0] == 0)
	{
	tmmsg (PROCFAIL, "ENABLE-RECVAR has not been executed.", 
	    	"TAE-NOENABLE");
	return (DO_CHECK);
	}
    vv = lookex(&(*cmd_ctx).parmst, "VARIABLE");   
    j  = lookex (&(*cmd_ctx).parmst, "JOB");
    for (i=0; i < (*vv).v_count; i++)
        {
	if (search (SVAL(*vv,i), proc_ctx) == NULL)
	    {
	    tmmsg (PROCFAIL, "Variable '%s' is undefined.",  "TAE-UNDEFVAR",
			SVAL(*vv,i));
	    return (DO_CHECK);
	    }
	}
    while (FOREVER)					/* till good parblk */
	{
	size = rec_path.size;
	code = c_getmsg (&rec_path, (GENPTR)&parblk, &size);
	if (code != SUCCESS)
	    {
	    tmmsg (PROCFAIL, "Cannot receive message. %s.", "TAE-COMMERR", 
	       rec_path.errmsg);
	    return (DO_CHECK);
	    }    
	makeabs (&parblk.symtab, parblk.pool);		/* make links abs     */
        code = chk_parblk (&parblk);			/* check structure    */
	if (code != SUCCESS)
    	    goto bad_parblk;
	if (s_equal ("ALL", SVAL(*j,0)))                /* accept any msg */
	    break;
	jp = lookex (&parblk.symtab, "_JOB");
        if (jp == NULL)					/* must have _JOB  */
    	    goto bad_parblk;
	if (s_equal(SVAL(*jp,0), SVAL(*j,0)))
	    break;
	}
    for (i=0; i < (*vv).v_count; i++)
	{
	vto = search (SVAL(*vv,i), proc_ctx);
	if (vto == NULL)
	   continue;				/* should never happen  */	
	vfrom = lookex (&parblk.symtab, (*vto).v_name);	
	if (vfrom == NULL)
	    continue;				/* intentional: no complaint */
	code= chk_vector (vto, (*vfrom).v_type, (*vfrom).v_cvp, 
			(*vfrom).v_count, FALSE);
	if (code != SUCCESS)
	    tmmsg (PROCFAIL, "Received variable '%s' incompatible.", 
	        "TAE-INCOMPAT", (*vto).v_name);
	else
	    code = set_value (vto, (*vfrom).v_cvp, (*vfrom).v_count);
	}
    return (DO_CHECK);


bad_parblk:
	tmmsg (PROCFAIL, "Received VBLOCK incorrectly formatted.", 
		    "TAE-PARBLK");
	return (DO_CHECK);
    }
