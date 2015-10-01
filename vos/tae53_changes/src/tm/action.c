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



/*	action processing for ACTION=action for PARMSETs.
 *
 *	This feature is not fully implemented.  The underpinnings
 *	are here in the tree so future work will be easier.
 *
 *	TBD:  ACTION not honored for compiled PDF--the 
 *	_ACTION local is lost.
 *
 *	Change Log:
 *
 *	24-mar-88	Added braces to array initializer...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"fileinc.inp"	/* file package				*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/
#include "taeintproto.h"

/*	table relating ACTION name to entry points	*/

struct ACTION
    {
    TEXT	name[NAMESIZ+1];
    CODE	(*setup)(struct CONTXT *);	
				/* action setup routine	*/
    CODE	(*run)(struct CONTXT *);	
	/* action run routine	*/
    };

CODE	none_setup(struct CONTXT *), none_run(struct CONTXT *), 
  parfile_setup(struct CONTXT *), 
  parfile_run(struct CONTXT *),
  global_setup(struct CONTXT *), global_run(struct CONTXT *), 
  window_setup(struct CONTXT *), 
  window_run(struct CONTXT *);

static struct ACTION action[] = 
{
  {"NONE",  	none_setup, 	none_run},
  {"PARFILE", 	parfile_setup,	parfile_run},
  {"GLOBALS", 	global_setup,   global_run},
  {"WINDOW", 	window_setup,	window_run},
  {"", 0, 0}					/* terminator entry */
};


/*	action_setup.   Called after CONTXT has been set with
	default parameters.   This may update the defaults
	according to the ACTION specified on the PARMSET.
*/

FUNCTION CODE action_setup 
(
    struct CONTXT *ctx		/* in/out: current proc context	*/

 )
    {
    struct VARIABLE *v;
    struct ACTION *a;
    CODE	code;

    if ((*ctx).proctype != Y_PARMSET)		/* only for PARMSETs	*/
        return (SUCCESS);
    v = lookex (&(*ctx).locst, "_ACTION");	/* get name of action	*/
    if (v == NULL)
        return (SUCCESS);
    code = SUCCESS;
    for (a=action; !NULLSTR((*a).name); a++)
        if (s_equal((*a).name, SVAL(*v,0)))
    	    {
    	    code = (*(*a).setup) (ctx);		/* call setup action	*/
    	    break;
    	    }
    return (code);
    }


/*	action_run.   Called when user runs a proc.
 *
 *	This honors the ACTION= for PARMSETs.
 */

FUNCTION CODE action_run 
(
    struct CONTXT *ctx		/* in/out: current proc context	*/

 )
    {
    struct VARIABLE *v;
    struct ACTION *a;
    CODE	code;

    if ((*ctx).proctype != Y_PARMSET)		/* only for PARMSETs	*/
        return (SUCCESS);
    v = lookex (&(*ctx).locst, "_ACTION");	/* get name of action	*/
    if (v == NULL)
        return (SUCCESS);
    code = SUCCESS;
    for (a=action; !NULLSTR((*a).name); a++)
        if (s_equal((*a).name, SVAL(*v,0)))
    	    {
    	    code = (*(*a).run) (ctx);		/* call setup action	*/
    	    break;
    	    }
    return (code);
    }


/* 	Routines not yet implemented:
*/

FUNCTION CODE none_setup(struct CONTXT *UNUSED(x))  {return (SUCCESS);}

FUNCTION CODE none_run (struct CONTXT *UNUSED(x))  {return (SUCCESS);}

FUNCTION CODE parfile_setup(struct CONTXT *UNUSED(x))  {return (SUCCESS);}

FUNCTION CODE parfile_run(struct CONTXT *UNUSED(x))  {return (SUCCESS);}

FUNCTION CODE global_setup(struct CONTXT *UNUSED(x))  {return (SUCCESS);}

FUNCTION CODE global_run(struct CONTXT *UNUSED(x))  {return (SUCCESS);}


/*	window_setup.  Demonstrate how to change defaults.
 *
 *	Presumably, we would have a table like ACTION to
 *	call window subcommand processors.  For the prototype
 *	we just do case on subcmd.
 */

FUNCTION CODE window_setup 
(
    struct CONTXT *ctx		/* in/out: proc context before updtab/tutor */

 )
    {
    TEXT	window_name[STRINGSIZ+1];    
    static TAEINT int_vv[] = {100};		/* int value vector	*/
    struct VARIABLE *v;

    v = lookex (&(*ctx).locst, "_PROC");	
    s_copy (SVAL(*v,0), window_name);		/* window name = proc name */
    if (s_equal((*ctx).subcmd, "CREATE"))
        {
        /* here, find out if window exists and return DO_CHECK it does 	*/
        /* otherwise SUCCESS					  	*/
        return (SUCCESS);
        }
    else if (s_equal ((*ctx).subcmd, "MODIFY"))	
        {
        v = lookex (&(*ctx).parmst, "TITLE");
        if (v == NULL)
	    goto missing_parm;
	set_string (v, "this was set by window_setup");
	v = lookex (&(*ctx).parmst, "MAXLINES");
	if (v == NULL)
	   goto missing_parm;
	set_value (v, (GENPTR) int_vv, 1);
	return (SUCCESS);
    	}    	
    else
        return (SUCCESS);


missing_parm:			
	tmmsg (PROCFAIL, "Required parameter not defined in WINDOW PARMSET",
	       "TAE-MISWINPARM", 0, 0, 0, 0, 0);	   
	return (DO_CHECK);
    }

/*	window_run.
 *
 */

FUNCTION CODE window_run 
(
    struct CONTXT	*ctx		/* in/out: executing pdf context */

 )
    {

    tmmsg (SUCCESS, "Window subcommand '%s' is executing.", 
           "TAE-DUMMY", (uintptr_t) (*ctx).subcmd, 0, 0, 0, 0);
    return (DO_CHECK);
    }
