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



/* This file contains the table of groups of intrinsic TAE commands,
 * as well as all the functions to locate the entries for these commands.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	10-oct-83	Fix unix lint errors...palm
 *	11-oct-83	Add SAVE/RESTORE commands...palm
 *	07-feb-84	Y_BATCH-->Y_ABI; remove Y_LOOPBOUND...nhe
 *	23-feb-84	Change "LISTCMD" to "HI001" ...palm
 *	01-mar-84	Add async commands...nhe
 *	23-mar-84	HI003...palm
 *	04-may-84	Replace setgst with set_string ... ces
 *	14-jun-84	Implement default intrinsic subcommand...lim
 *	14-jun-84	New itr_lookup function (to support new
 *			Y_PDF logic in prccmd)...palm
 *	10-oct-84	Add ref to for compile_do...peb
 *	12-oct-84	Conditional compilation of async commands (as for
 *			unix 1.2)...dm
 *	14-oct-84	Update end-proc entry for internal procs...nhe
 *	09-nov-84	PR 895: fix Apollo UNIX compilation error...palm
 *	30-jul-85	Optimize intrin...palm
 *	16-jul-86	Re-arrange intrinsic vector to get critical commands
 *			(declarations) at the top where they are found 
 *			quickly...palm
 *	16-jul-86	New entry for SENDVAR/RECVAR commands...palm
 *	22-jul-87	PROBLEM: How to put all TM modules in VMS libraries and
 *			at the same time ensure that each and every external
 *			variable gets linked in? We had to FORCE the linker
 *			to recognize modules with the intrinsic tables, for
 *			example. We rejected the possible solution to this
 *			problem of using globalrefs and -defs as producing
 *			too many portability warnings.
 *
 *	09-aug-87	New thinking on the cmd table linking (above):
 *			Calling the get_* functions is bad from a paging
 *			viewpoint, because we have to page in almost all 
 *			of TM just to initialize our local tables during
 *			initialization.  The new scheme uses the get_*
 *			functions as a way of making the library refs work
 *			but we don't call them, just reference them...palm
 *	09-aug-87	Re-order intrinsic table for TM optimization...palm
 *      12-aug-87       Added windowcmd and get_windowcmd...tpl
 *	03-feb-89	Honor Y_ANYSUB in command table...palm
 *	27-jun-90	Remove Facelift code...ljn
 *	28-jun-90	No more maintmlib.a, thus no more get_* stuff...ljn
 */

#include	"taeconf.inp"		/* TAE configuration definitions*/
#include	"fileinc.inp"		/* file package			*/
#include	"symtab.inc"		/* symbol table			*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include "taeintproto.h"


/*	ITRCMD for all ENDs placed here because all subcommands for one
 *	command must be contiguous in the ITRCMD and END commands are
 *	processed in several different source modules.
 */

#define	Y_ENDIF		Y_PROC | Y_BODY | Y_SEARCH | Y_PROCSYN | Y_FALSE | Y_CMD | Y_ABI
#define	Y_ENDPROC	Y_PROC | Y_BODY | Y_PREBODY | Y_SEARCH | Y_PROCSYN | Y_FALSE | Y_SUBSRCH | Y_CMD | Y_ABI | Y_PROCSEARCH
#define	Y_SUBCOMMAND    Y_DECLARE | Y_ENDSUB 

    static struct ITRCMD endcmd[] = 
    {
{0, "END",	"FOR",		Y_LOOPBOUND,	0,	NULL,	endfor_do },
{0, "END",	"GLOBAL",  	Y_BATCH|Y_SUBSRCH|Y_DECLARE,
						0,	NULL,	endgbl_do },
{0, "END",      "IF",   	Y_ENDIF,	0,	NULL,	endif_do  },
{0, "END",	"LOOP",		Y_LOOPBOUND,	0,	NULL,	endloop_do},
{0, "END",	"PROC",	        Y_ENDPROC,	0,	NULL,	endproc_do},
{0, "END",	"SUBCMD", 	Y_SUBCOMMAND,	0,	NULL,   endsub_do },
{0, "HI001",  	"",		Y_GENERAL,	0,	NULL,   listcmd_do},
{0, "HI003",	"",  		Y_GENERAL,	0,      NULL,	version_do },
{0, ""}  					/* terminator entry	*/
     };


#ifdef TAE_ASYNC
    IMPORT struct ITRCMD        asynccmd[];
#endif
    IMPORT struct ITRCMD        condcmd[];
    IMPORT struct ITRCMD        dyncmd[];
    IMPORT struct ITRCMD        letcmd[];
    IMPORT struct ITRCMD        misccmd[];
    IMPORT struct ITRCMD        hostcmd[];
    IMPORT struct ITRCMD        tutorcmd[];
    IMPORT struct ITRCMD        menucmd[];
    IMPORT struct ITRCMD        setshowcmd[];
    IMPORT struct ITRCMD        loopcmd[];
    IMPORT struct ITRCMD        helpcmd[];
    IMPORT struct ITRCMD        logcmd[];
    IMPORT struct ITRCMD        declarecmd[];
    IMPORT struct ITRCMD        saverescmd[];
    IMPORT struct ITRCMD	compcmd[];
    IMPORT struct ITRCMD        sendcmd[];
    IMPORT struct ITRCMD        commands_ins[];
    IMPORT struct ITRCMD        windowcmd[];


/*	Command table pointers.   Each entry in the intrinsic table
 *	consists of a pointer to an ITRCMD table.
 *
 ***********************************************************************
 **	NOTICE: the order of this table is critical to TAE 
 **	performance.  Popular/fast commands (PROCEDURE, PARM, etc.)
 **	should be near the top of the table.
 ***********************************************************************
 */

    static struct ITRCMD *intrinsic[] =
    	{
	declarecmd,	/* DON'T CHANGE ORDER !!! SEE ABOVE */
    	misccmd,
	endcmd, 
    	condcmd,
    	letcmd, 
	loopcmd, 
    	dyncmd, 
	hostcmd, 
	tutorcmd, 
	menucmd, 
	setshowcmd, 
	helpcmd, 
	logcmd, 
	saverescmd, 
	compcmd, 
	sendcmd, 
	commands_ins, 
	windowcmd, 
#ifdef TAE_ASYNC
    	asynccmd,
#endif 
    	};

#define NR_TABLES sizeof(intrinsic)/sizeof(struct TTRCMD *) 


/*	intrin_init. 
 *
 *	This no longer does anything--we keep the call to here
 *	in TM.C in case we need it someday.
 *
 */
    FUNCTION CODE intrin_init (void)
    
    {
    return (SUCCESS);
    }


/*
 *  intrin.	Find command in intrinsic tables.  The returned pointer
 *  is the pointer to the first occurence of the command. (A command may
 *  occur several times successively, each occurence with a different
 *  subcommand).  We assume here that the intrinsic tables have no
 *  ambiguities w.r.t. abbreviations of commands.
 */

FUNCTION struct ITRCMD * intrin 
(
    FAST TEXT		*cmd		/* in:  command 			*/

 )
    {
    IMPORT TEXT s_table[];		/* lower to upper conversion	*/

    FAST struct ITRCMD 	*p;
    FAST COUNT		i;
    COUNT		len;


    len = s_length(cmd);			/* length of command	  */
    for (i=0;  i < NR_TABLES; i++)
	{
	p = intrinsic[i];		/* ptr to next ITRCMD table */
	for ( ; !NULLSTR((*p).cmd);  p++)	
	    {
	      if (s_table [(int) cmd[0]] != s_table [(int) (*p).cmd[0]]) 
		continue; 			/* slight optimization	*/
	    if ((*p).abbchr == 0)		/* if all chars required*/
    		{
		if (s_equal(cmd, (*p).cmd)) return (p);
    		}
	    else if (len >= (*p).abbchr)	/* abbreviations 		*/
    		{
		if (s_lseq(cmd, (*p).cmd)) return (p);
    		}
	    }
	}
    return(NULL);
    }

/*
 *	itrsub.  Locates the ITRCMD structure given a subcommand and the
 *	pointer to the first ITRCMD structure for a command.
 *
 *	Returns pointer to the correct ITRCMD structure or NULL if the
 *	subcommand is non-existent or ambiguous.
 *
 *	If the subcommand is null, then it must match an ITRCMD entry
 *	which has a null subcommand or an entry marked Y_DEFSUB.
 * 
 */

FUNCTION struct ITRCMD * itrsub
(
    TEXT		subcmd[],		/* in: subcommand	*/
    struct ITRCMD    	*p			/* in: first ITRCMD	*/

 )
    {
    COUNT		matches;		/* number of matches	*/
    TEXT		*first_cmd;
    struct ITRCMD	*p_match = 0;

    matches = 0;
    first_cmd = (*p).cmd;
    do
	{
	if (NULLSTR(subcmd))			/* special check for null...	*/
	    {
 	    if (NULLSTR((*p).subcmd) || (*p).flags & Y_DEFSUB)
		{
		p_match = p;
		matches++;
		break;
		}
	    }
	else if (s_lseq(subcmd, (*p).subcmd))
	    {
	    p_match = p;
	    matches++;
	    }
	p++;
	}
    while (s_equal(first_cmd, (*p).cmd));	/* stay with initial command	*/

    if (matches == 1) return(p_match);
    else 	      return(NULL);
    } 

/*	itr_lookup.    Lookup intrinsic command-subcommand.
 *
 *	Returns:
 *		SUCCESS	-- command-subcommand found
 *		ITR_PDF -- command/subcommand is a Y_PDF
 *		ITR_BADCMD -- unknown command
 *		ITR_BADSUB -- unknown or ambiguous subcommand
 */

FUNCTION CODE itr_lookup
(
    TEXT		cmd[],		/* in: command 		*/
    TEXT		subcmd[],	/* in: subcommand	*/
    struct ITRCMD	**itrcmd	/* out: entry pointer	*/

 )
    {
    struct ITRCMD *in;

    in = *itrcmd = intrin (cmd);		/* find command		*/
    if (in == NULL)
        return (ITR_BADCMD);	
    if ((*in).flags & Y_ANYSUB)			/* any subcommand ok?	*/
	return (SUCCESS);			/* then look no more    */
    in = *itrcmd = itrsub (subcmd, in);		/* find subcommand	*/
    if (in == NULL)
	return (ITR_BADSUB);
    if ((*in).flags & Y_PDF)
	return (ITR_PDF);
    return (SUCCESS);
    }

/*	listcmd_do.   List all intrinsic commands.
 */
    
FUNCTION CODE listcmd_do 
(
    struct CONTXT	*pc,		/* in: proc context	*/
    struct CONTXT	*c		/* in: command context	*/

 )
    {
    struct ITRCMD 	*p;
    COUNT		i;
    TEXT		line[STRINGSIZ+1];


    for (i = 0; i < NR_TABLES; i++)		/* for each table	*/
	{
	for (p = intrinsic[i]; !NULLSTR((*p).cmd);  p++)
	    {
    	    s_copy ((*p).cmd, line);
    	    s_append ("-", line);	
    	    s_append ((*p).subcmd, line);
     	    put_stdout (line);
    	    }
	}
    return(0);
    }

/*  version_do - Put TM version number
 *
 */
FUNCTION CODE version_do 
(
    struct CONTXT	*procctx,		/* in: proc context	*/
    struct CONTXT	*cmdctx		/* in: cmd context	*/

 )
    {
    IMPORT TEXT		vrsion[];
    IMPORT  struct  VARIABLE 	*skey_gbl;	/* pointer to $skey	*/

    put_stdout(vrsion);
    set_string (skey_gbl, vrsion);		/* set skey		*/
    return (DO_SUCCESS);
    }
