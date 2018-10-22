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



/* Functions associated with command qualifiers.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	18-jul-83	Exit with NOLATCH on error...peb
 *	04-aug-83	No qualifier strings for intrinsics...dm
 *	21-sep-83	Fix RESIDVARs for UNIX...palm
 *	10-oct-83	Fix UNIX compilation errors...palm
 *	21-feb-84	Add num_qual for no. of legal qualifiers...peb
 *	06-mar-84	STDOUT qualifier be nullable with null default...peb
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Move the IMPORT declaration...lim
 *	12-jun-84	Change default runtype to --...nhe
 *	11-sep-84	Add JOB and NODE qualifiers...nhe
 *	10-dec-84	Add ASYNCEND...nhe
 *	29-apr-85	Change position of NODE, and unconditionalize...nhe
 *	19-jun-85	Fix warning error with new C compiler V2.0...lia
 *	24-mar-88	Delete TAE_RCJM conditional...ljn
 *	26-jan-89	new valid format...palm
 *	01-aug-91	Braces for static string initializers...ljn
 *
 */

#include	"stdh.inp"		/* standard C definitions		*/
#include	"taeconf.inp"		/* TAE configuration definitions	*/

#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"syninc.inc"		/* syntax-block etc.			*/
#include "taeintproto.h"




/*		Command Qualifier Definitions					*/

    static TEXT	*nulls[] = {""};	/* Null string default		*/
    static TEXT *sil_str[] = {"SILENT"};	/* vector for 'silent'		*/

    BEGIN_VALIDS(ae_valid, 2)
{{"NOTIFY"}, {"SILENT"}}
    END_VALIDS

    static struct RESIDVAR resid_qual[] = 
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"RESTORE", V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,    (GENPTR)nulls},
	  {"RUNTYPE", V_STRING, 0, 0, 2,      FSPECSIZ, 0, NULL,    NULL},
	  {"SAVE",	   V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,    (GENPTR)nulls},
	  {"STDOUT",  V_STRING, 0, 0, 1,      FSPECSIZ, 0, NULL,    NULL},
	  {"JOB",	   V_STRING, 0, 0, 1,	   JOBNAMESIZ,0,NULL,    NULL},
	  {"ASYNCEND",V_STRING, 0, 1, 1, sizeof("NOTIFY"),1,(GENPTR)&ae_valid, 
	   (GENPTR)sil_str},
	  {"NODE",    V_STRING, 0, 0, 2,	   NODENAMESIZ,0,NULL,   NULL}
	};
#define NUM_QUAL sizeof resid_qual/sizeof(struct RESIDVAR)

    GLOBAL COUNT	num_qual = NUM_QUAL;	/* for other sources to reference*/

/* setqlf - set the qualifiers.
 *
 * Get the qualifiers from a command line and add them to the qualifier
 * symbol table.  Assumes the symbol table is empty when called.
 *
 * Return codes: SUCCESS or S_NONE (no qualifiers) or FAIL
 */

FUNCTION CODE setqlf 
(
    FUNINT		intrin,		/* true if intrinsic		*/
    struct SYNBLK	*sb,		/* in/out: syntax block (command stream)*/
    struct SYMTAB	*st		/* out:  qualifier symbol table		*/

 )
    {
    IMPORT CODE		parmlatch;	/* parm latch flag			*/

    TEXT		qualstr[CMDLINSIZ+1];	/* qualifier string (all quals)	*/
    CODE		qcode, code;
    struct SYNBLK	locsb;		/* syntax block for qualifier stream	*/

    qcode = getqlf(sb, qualstr);			/* get string of qualifiers	*/
    if (qcode != SUCCESS && qcode !=S_NONE)
	{
	  tmmsg(PROCFAIL, (*sb).errmsg, "TAE-QUALERR",0,0,0,0,0);
	return(FAIL);
	}
    if (intrin) 
	return(qcode);
    code = memtab(resid_qual, NUM_QUAL, st);	/* init qual symbol table 		*/
    if (code != SUCCESS)
    	return(FAIL);
    if (qcode == S_NONE)
    	return(S_NONE);				/* no qualifiers on the line	*/
    initok(&locsb, qualstr);			/* init qual stream for syntax package	*/
    code = updtab(st, &locsb);			/* update qual sym tab based on new qualifiers	*/
    if (code != SUCCESS)
	parmlatch = NOLATCH;			/* don't want to latch from bad qualif*/
    return(code);
    }
