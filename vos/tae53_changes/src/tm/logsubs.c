/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TLM CHECKOUT FILE_TIME= 7-MAY-1984 16:04 DUA0:[TAEV1.TM]LOGSUBS.C;28 */
/* TDM CHECKOUT FILE_TIME=12-MAR-1984 15:31 DUA0:[TAEV1.TM]LOGSUBS.C;24 */
/* TDM CHECKOUT FILE_TIME=20-NOV-1983 18:20 DUA0:[TAEV1.TM]LOGSUBS.C;21 */
/* TDM CHECKOUT FILE_TIME=31-OCT-1983 16:13 DUA0:[TAEV1.TM]LOGSUBS.C;20 */
/* TDM CHECKOUT FILE_TIME=10-OCT-1983 19:23 DUA0:[TAEV1.TM]LOGSUBS.C;19 */
/*TDM         CHECKOUT FILE_TIME=22-AUG-1983 14:54 DUA0:[TAEV1.TM]LOGSUBS.C;17 */
/*TDM         CHECKOUT FILE_TIME=11-JUL-1983 16:57 DUA0:[TAEV1.TM]LOGSUBS.C;15 */

/*
 *	Session logging.
 *
 * 
 *	
 ********************************************************************************
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	22-aug-83	Updated for NULLABLES...dm
 *	02-sep-83	Logged reference name for NAME parameters...dm
 *	07-sep-83	Updated for forp... function name changes...dm
 *	10-oct-83	Fixed unix lint errors...palm
 *	31-oct-83	PR 566: change m_put call to put_stdout...dm
 *	01-nov-83	Fix recursion bug,add operator atten to logmsg...dm
 *	23-nov-83	Fix 'no echo to terminal' bug  (pr #593)...dm
 *	12-mar-84	Use s_bcopy to set lastkey...palm
 *	19-mar-84	Remove double check of msgtype for logging...dm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *			'toprefs'-->'numrefs'...nhe
 *	15-aug-84	Modify log message process, added a parameter to logmsg
 *			and created a new function add_trace (pr #628)...lia
 *	08-dec-84	Delete use of $MESSAGE(2) and eliminated display
 *			of trace information...lia
 *	06-may-85	Fix dimension with SIZ to SIZ+1...lia
 *	01-jul-85	Fix UNIX compilation error...dm
 *	11-may-89	Don't set lastkey when null key...palm 
 *	22-oct-89	if null key, put_stdout rather than put_msgout...palm 
 *
 ******************************************************************************
 */

#include	"taeconf.inp"		/* TAE configuration */
#include	"parblk.inc"		/* parameter block definitions */
#include	"terminc.inc"		/* terminal package */
#include	"tminc.inc"		/* TM  related definitions */
#include	"sessinc.inc"		/* session log struct */
#include "taeintproto.h"
#ifdef SESSION_LOG
   

#define	  	PARLOGSIZ   STRINGSIZ+6		/* allow for ""), +  */
#define		MSGLOGSIZ   STRINGSIZ+KEYSIZ+3	/* allow for key string */


FUNCTION  static VOID  slprocnm
(
    struct  CONTXT  *ctx			/* in: context block for proc */

);
FUNCTION  static VOID  slforprm
(    
    struct  CONTXT	*ctx			/* IN: proc context block */

     );
FUNCTION  static  VOID  slforgbl
(    
    struct  CONTXT	*ctx			/* IN: proc context block  */

     );
FUNCTION  static VOID  outname
(
    TEXT		prefix[],		/* IN: output record prefix */
    TEXT		name[],			/* IN: name of variable	*/
    TEXT		line[],			/* IN/OUT: record to add name */
    COUNT		length			/* IN: max record length */


 );
FUNCTION  static   VOID  add_trace
(
    struct CONTXT	*ctx,		/* IN: context of executing process */
    TEXT		record[]	/* OUT: formatted message 	*/

 );
FUNCTION  VOID  sltime(void);
FUNCTION  VOID  slparm
(
    TEXT		prefix[],		/* record prefix 	   */
    struct  SYMTAB      *symtab		/* symbol table address    */

 );
FUNCTION  VOID  slwrite
(
    TEXT		prefix[],		/* IN: prefix to line 	*/
    TEXT		line[]			/* IN: line to log    	*/


 );
FUNCTION  static VOID  outvalue
(
    TEXT		prefix[],		/* IN: output record prefix */
    struct  VARIABLE	*vv,			/* IN: pointer to variable */
    COUNT		index,			/* IN: index to value vector */
    TEXT		line[],			/* IN/OUT: record to add name */
    COUNT		stdlen,			/* IN: standard line size */
    COUNT		longlen,		/* IN: max record length */
    FUNINT		lastflag		/* IN: true if last value */


 );


/*
 * slproc - session log proc activation information
 *
 */


FUNCTION  VOID  slproc
(
    struct  CONTXT   *context		/* in: context block for the proc */

 )
    {
    IMPORT  struct  SESSLOG	sesslog;	/* session log struct */

    if (!sesslog.enable)
	return;				/* by-pass overhead if disabled */
    sltime();				/* log current time */
    slprocnm(context);			/* log proc name */
    slforprm(context);			/* log proc parameters */
    slforgbl(context);			/* log proc referrenced globals */
    return;
    }

/*
 * sltime - log current time to session log file 
 */

    FUNCTION  VOID  sltime(void)

    {
    TEXT	time[STRINGSIZ+1];		/* current time(dd-mmm-yyyy hh:mm:ss)*/

    time[0] = EOS;				/* safety catch		*/
    get_time(time);				/* get current time 	*/
    slwrite("TS: ", time);			/* log time to file only*/
    return;
    }

/*
 *
 *	slprocnm - log proc name to session log file
 */

FUNCTION  static VOID  slprocnm
(
    struct  CONTXT  *ctx			/* in: context block for proc */

 )
    {
    TEXT	record[STRINGSIZ+1];		/* output buffer */

    s_copy("\"", record);
    f_spec(&(*ctx).pdf, &record[1]);		/* build host file spec	*/
    s_append("\"", record);			/* add end quote	*/
    if (!NULLSTR((*ctx).subcmd))			
	{					/* if subcmd present	*/
	s_append("-", record);
	s_append((*ctx).subcmd, record);	/* append subcommand name */
	}
    if ((*ctx).parmst.link != NULL)		/* variables to follow ? */
	s_append("+", record);			/* append trailing +	*/
    slwrite("PA: ", record);			/* log to file only	*/
    return;
    }

/*
 * slforprm .  format the input parameter variables  (in a linked list) 
 */

FUNCTION  static VOID  slforprm
(    
    struct  CONTXT	*ctx			/* IN: proc context block */

     )
    {
    slparm("PA: ", &(*ctx).parmst);		/* format for proc activation */
    return;
    }


/*
 * slparm . parameter formatting function
 */

FUNCTION  VOID  slparm
(
    TEXT		prefix[],		/* record prefix 	   */
    struct  SYMTAB      *symtab		/* symbol table address    */

 )
    {
    IMPORT	COUNT		termcols;	/* number of columns on terminal */

    TEXT		line[PARLOGSIZ+1];	/* OUT: formatted parameter rec */
    struct  VARIABLE	*vv;			/* pointer to a variable   */
    COUNT		index;			/* index pointer 	   */
    COUNT		max_index;		/* max value of index ptr  */
    BOOL		lastflag;		/* true if last variable   */
    COUNT		stdsiz;			/* standard length of line */

    stdsiz = termcols-PREFIXSIZ;
    line[0] = EOS;
    for (vv = (*symtab).link; vv != NULL; vv = (*vv).v_link)
	{
	outname(prefix, (*vv).v_name, line, stdsiz);		/* output name */
        if ((*vv).v_class == V_PARM && (*vv).v_type == V_NAME)  /* if NAME var */
	    max_index = 0;
	else
	    max_index = ((*vv).v_count >= 1) ? ((*vv).v_count-1) : 0;
	for (index = 0; index <= max_index; index++)
	    {
	    lastflag = ((*vv).v_link == NULL  &&   index == max_index) ;
	    outvalue(prefix, vv, index, line, stdsiz, PARLOGSIZ, lastflag); 
 	    }
	}
    return;
    }

/*
 * slforgbl .  format the input global variables  (in a refed list) 
 */

FUNCTION  static  VOID  slforgbl
(    
    struct  CONTXT	*ctx			/* IN: proc context block  */

     )
    {
    IMPORT	COUNT		termcols;	/* number of columns on terminal */

    TEXT		line[PARLOGSIZ+1];	/* OUT: formatted parameter rec */
    struct  VARIABLE	*vv;			/* pointer to a variable   */
    COUNT		index;			/* index pointer to value  */
    COUNT		max_index;		/* max value of index ptr  */
    COUNT		i;			/* index to refs array     */
    BOOL		lastflag;		/* true if last value      */
    COUNT		stdsiz;			/* standard length of line */

    stdsiz = termcols-PREFIXSIZ;
    line[0] = EOS;
    for (i = 0; i < (*ctx).numrefs; i++)		/* for each global */
	{
    	vv = (*ctx).refs[i];				/* point to VARIABLE struct */
	outname("PR: ", (*vv).v_name, line, stdsiz);	/* output name	   */
	max_index = ((*vv).v_count >= 1) ? ((*vv).v_count-1) : 0;
	for (index = 0; index <= max_index; index++)
	    {
	    lastflag =  (i == ((*ctx).numrefs)-1  &&   index == max_index);
	    outvalue("PR: ", vv, index, line, stdsiz, PARLOGSIZ, lastflag); 
 	    }
	}
     return;
    }

/*
 * 	outname. Add variable name to output buffer.
 *	If buffer is full, write to session log file and start with
 *	a fresh buffer.
 */

FUNCTION  static VOID  outname
(
    TEXT		prefix[],		/* IN: output record prefix */
    TEXT		name[],			/* IN: name of variable	*/
    TEXT		line[],			/* IN/OUT: record to add name */
    COUNT		length			/* IN: max record length */


 )
    {
    CODE 		code;
    
    code = m_fpname(name, line, length);	/* get formatted name */
    if (code == FAIL) 				/* no more room in buffer */
	{
	s_append("+", line);			/* put continuation char */
	slwrite(prefix, line);			/* write to session log file */
	line[0] = EOS;				/* start with a fresh buffer */
        m_fpname(name, line, length);		/* add name to new line */
	}
    return;
    }

/*
 * 	outvalue. Add variable value(s) to output buffer.
 *	If buffer is full, write to session log file and start with
 *	a fresh  buffer.
 *	NOTE: If a NAME variable, add reference name rather than value.
 */

FUNCTION  static VOID  outvalue
(
    TEXT		prefix[],		/* IN: output record prefix */
    struct  VARIABLE	*vv,			/* IN: pointer to variable */
    COUNT		index,			/* IN: index to value vector */
    TEXT		line[],			/* IN/OUT: record to add name */
    COUNT		stdlen,			/* IN: standard line size */
    COUNT		longlen,		/* IN: max record length */
    FUNINT		lastflag		/* IN: true if last value */


 )
    {
    CODE 		code;
    COUNT		nchar;

    if ((*vv).v_type == V_NAME)			/* if name parameter	     */
	code = m_frname(vv, line, stdlen, longlen);	    /* get ref name  */
    else
  	code = m_fpval(vv, index, line, stdlen, longlen);   /* get value     */
 
    if (code == FAIL) 				/* no more room in buffer    */
	{
	s_append("+", line);			/* put continuation char     */
	slwrite(prefix, line);			/* write to session log file */
	line[0] = EOS;				/* start with a fresh buffer */
    	outvalue(prefix, vv, index, line, stdlen, longlen, FALSE);
	}
    if (lastflag) 				/* last value of last variable */
	{
	nchar = s_length(line);
	line[nchar-2] = EOS;			/* del hanging separator ", " */
	slwrite(prefix, line);			/* write the last line */
	}
     return;
    }

/*
 *	slterm. Log proc termination.
 */
  
    FUNCTION  VOID  slterm(void)

    {
    IMPORT  COUNT		termcols;	/* number of columns on terminal */
    IMPORT  struct  SESSLOG	sesslog;	/* session log struct */
    IMPORT  struct  VARIABLE  *sfi_gbl;		/* pointer to $sfi	*/
    IMPORT  struct  VARIABLE  *skey_gbl;	/* pointer to $skey	*/

    TEXT		line[PARLOGSIZ+1];	/* OUT: formatted parameter rec */
    struct  VARIABLE	*vv;			/* pointer to a variable */
    COUNT		stdsiz;			/* standard length of line */


    if (!sesslog.enable)
	return;					/* by-pass overhead if disabled */
    sltime();					/* time stamp	*/
    stdsiz = termcols-PREFIXSIZ;
    line[0] = EOS;
    vv = sfi_gbl;						/* log $sfi	*/
    outname("PT: ", (*vv).v_name, line, stdsiz);		/* output name	*/
    outvalue("PT: ", vv, 0, line, stdsiz, PARLOGSIZ, FALSE);	/* output value */

    vv = skey_gbl;						/* log $skey	*/
    outname("PT: ", (*vv).v_name, line, stdsiz);		/* output name	*/
    outvalue("PT: ", vv, 0, line, stdsiz, PARLOGSIZ, TRUE);	/* output value */
 	    
    return;
    }

/*
 * 	slwrite. Write data to session log file.
 *	
 */

FUNCTION  VOID  slwrite
(
    TEXT		prefix[],		/* IN: prefix to line 	*/
    TEXT		line[]			/* IN: line to log    	*/


 )
    {
    IMPORT  struct  SESSLOG	sesslog;	/* session log struct */

    TEXT		record[PREFIXSIZ+PARLOGSIZ+1];	/* local buffer  */
    COUNT		nchar;				/* record length */

    if (sesslog.enable)
	{
	s_copy(prefix, record);			/* copy prefix */
	s_append(line, record);			/* add text to be logged */
	nchar = s_length(record);
	if (nchar > STRINGSIZ)			/* if record too long   */
	    s_copy("...", &record[STRINGSIZ-3]); /* truncate record	*/
	f_write(&sesslog.sfile, record);	/* write to log file	*/
    	}
    return;
    }
#endif

/*
 *	logmsg .  Log message from subprocess.
 */

FUNCTION   VOID  logmsg
(
    struct CONTXT	*ctx,		/* in: context of executing process */
    struct PARBLK	*parblk	/* in: parblk from subprocess	*/

 )
    {
    IMPORT TEXT 	lastkey[];	/* most recent message key	*/

    struct VARIABLE	*v1, *v2;
    TEXT		msgtext[MSGLOGSIZ+1];	/* local buffer for msg + key */
    TEXT		tracetxt[STRINGSIZ+1];	/* local buf for trace info */
    COUNT		nchar;			/* len of output string	*/
    BOOL		nullKey;


    v1 = lookex(&(*parblk).symtab, "MESSAGE");		/* find message test */
    v2 = lookex(&(*parblk).symtab, "KEY");		/* find message key  */
    if (v1 == NULL || v2 == NULL)			/* garbage from proc?*/	
	return;						
    nullKey = NULLSTR (SVAL(*v2,0)) || s_equal (SVAL(*v2,0), " ");
    if (nullKey)
	{
	s_bcopy (SVAL(*v1, 0), msgtext, sizeof (msgtext) - 1);
	put_stdout (msgtext);
	}
    else
	{
	s_bcopy(SVAL(*v2, 0), lastkey, KEYSIZ);		/* save for help-msg */
	msgtext[0] = EOS;
	m_pfmt(SVAL(*v1, 0), SVAL(*v2, 0), msgtext,0,0,0,0,0);  	/* format output  */
	put_outmsg (msgtext);			/* write to output devices   */
	}

#ifdef SESSION_LOG
    tracetxt[0] = EOS;
    add_trace (ctx, tracetxt);
    if (tracetxt[0] == EOS)
	slwrite ("MS: ", msgtext);
    else
	{
	nchar = s_length(msgtext);
	if (nchar > STRINGSIZ-7)		/* if record too long	*/
	    s_copy("...; +", &msgtext[STRINGSIZ-10]);       /* truncate */
	else
	    s_append ("; +", msgtext);
	slwrite ("MS: ", msgtext);		/* log to file	*/
	slwrite ("MS: ", tracetxt);
	}
#endif

    return;	
    }

/*
 *	add_trace .  Add proc information (name and line number) to
 *	message for locating where message is sent.  If location is
 *	in the body portion of the PDF only the proc name will be added.
 */

FUNCTION  static   VOID  add_trace
(
    struct CONTXT	*ctx,		/* IN: context of executing process */
    TEXT		record[]	/* OUT: formatted message 	*/

 )
    {
    TEXT		linenum[12];

    s_copy (" proc '", record);		/* start new line	*/
    s_append ((*ctx).pdf.name, record);	/* copy proc name	*/
    if ((*ctx).proctype == Y_PROCESS && (*ctx).inbody)
   	s_append ("'", record);
    else
	{
	s_append ("', line ", record);
	s_i2s ((*ctx).pdf_line + 1, linenum); /* convert line to int	*/
	s_append (linenum, record);		/* copy line number	*/
	}
    return;
    }


#ifdef SESSION_LOG
/*
 *	logdyn .  Log parameters for dynamic command processing .
 */

FUNCTION   VOID  logdyn
(
    struct PARBLK	*parblk	/* in: parblk from subprocess	*/
 )
    {
    if ((*parblk).msgtyp == M_CONTINUE)			/* dynamic cmd param */
	{
	makeabs(&(*parblk).symtab, (*parblk).pool);	/* make pointers absolute */
	slparm("DP: ", &(*parblk).symtab);
	makerel(&(*parblk).symtab, (*parblk).pool);	/* make relative again */
	}
    return;	
    }
#endif


#ifdef SESSION_LOG2
/*
 *	sl2write: Write 2nd session log file if enabled
 */

FUNCTION CODE sl2write
(
    TEXT record[],	/* input: record to be written */
    BOOL breakup	/* input: TRUE if rec needs breaking up w/cmdindex[] */

 )
    {
    IMPORT struct SESSLOG sesslog;	/* session log structure */
    IMPORT struct SFILE sess2file;
    IMPORT COUNT cmdindex[CMDLINSIZ];	/* breakup indexes into cmdstr */
    COUNT reccount;
    COUNT lowindex;
    TEXT  buf[CMDLINSIZ+1];
    COUNT length;

    if (!sesslog.enable) return 0;
    if (breakup)
	{
	reccount = 0;
	lowindex = 0;
	while (FOREVER)
	    {
	    length = s_copy(record,buf);
	    if ((length > cmdindex[reccount]) &&
		(cmdindex[reccount] > 0))    /* if this isn't the last string */
		{
                buf[cmdindex[reccount]] = EOS;  /* cut off the command line */
		s_append("+", buf);	/* cut off the line */
		f_write(&sess2file, &buf[lowindex]);
		lowindex = cmdindex[reccount++];  /* more to next partial str */
		}

/* Since cmd line substitutions are done after cmdindex is set up, the	*/
/* last line may be too big for editors, printers, etc.  Write it out	*/
/* in smaller chunks.							*/

	    else
		{
		while (length > lowindex+PRINTERLINESIZ-1)
		    {
		    buf[lowindex+PRINTERLINESIZ-1] = '+';
		    buf[lowindex+PRINTERLINESIZ] = EOS;
		    f_write(&sess2file, &buf[lowindex]);
		    lowindex += PRINTERLINESIZ-1;
		    length = s_copy(record, buf);
		    }
		f_write(&sess2file, &buf[lowindex]);
		break;
		}
	    }
	}
    else f_write(&sess2file, record);
    return 0;
    }
#endif
