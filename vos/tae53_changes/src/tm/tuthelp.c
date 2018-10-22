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
 * This file contains functions to perform help file processing while
 * in tutor mode of TAE.
 *
 * This file is extracted from tutor.c during the tutor.c breakup of 6-dec-84.
 * See full explanation in tutor.c.
 *
 * The functions in this source file are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	09-dec-84	TCL 67: display qual indication in lev 1 help...peb
 *
 ****** CHANGES SPECIFIC TO RCJM ****************
 *
 *	03-mar-85	Add support for remote help files
 *	13-mar-85	Give help spec name for remote help file...dm
 *	14-mar-85	Return SUCCESS even if no remote help...dm
 *	15-mar-85	If no remote help spec known, give pdf name...dm
 *	18-apr-85	Changed function name for remote help...dm
 *	22-apr-85	Invoke d_remote to initialize for remote help...dm
 *	25-apr-85	Additional argument to get_remote_file call...dm
 *	27-apr-85	Update get_remote_file calling seq...dm
 *
 ******** MERGE WITH THE FOLLOWING TAE_V1.3 CHANGES...dm (24-may-85)
 *
 *	06-may-85	Fix dimension with SIZ] to SIZ+1]...lia
 *
 *****************************************************************
 *
 *	25-jul-85	Fix UNIX lint errors...dm
 *	18-aug-86	TAE_FACELIFT changes...krw
 *
 ********************** Post TAE 2.1 changes *******************************
 *
 *	19-may-87	Updated for TAE-Plus philosophy. Checked global 
 *			variable DisplayId to determine VT100 or Window 
 *			mode screen operations...dm
 *        
 *      03-feb-88       Changed DisplayId to XFACELIFT..tpl 
 *	24-feb-88	PR 1504: Change label arg in cmd_parse call to NULL...ln
 *      26-feb-88       Handle parm-page properly in XFACLIFT mode...tpl
 *	24-mar-88	Deleted TAE_RCJM conditionals...ljn
 *	08-dec-88	Honor COMPRESS_TUTOR...palm
 * 	12-jun-89	Removed TAE_FACELIFT...ljn
 * 	23-may-90	Remove RCJM stuff by referring to old TAE_RCJM...ljn
 *	27-jun-90	Remove Facelift code...ljn
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"dirinc.inc"		/* includes for d_ package		*/
#include	"helpinc.inc"		/* include for help interface		*/
#include	"syninc.inc"
#include	"terminc.inc"
#include	"compiled.inc"		/* compiled PDF structs and defs	*/
#include "taeintproto.h"



    IMPORT  struct TUTCTX tutctx;	/* tutor & micro-editor context	*/


    static TEXT		msg_mmov[]="Terminal Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/
    static TEXT		msg_hrde[] = "Error reading help file. %s";
    static TEXT		key_hrde[] = "TAE-RDERR";	/* help file read error in tutor*/

FUNCTION static CODE inlevel1 
(
    struct CONTXT	*pctx,		/* in/out: proc context w/ tab to update.	*/
    struct SFILE	*hf,		/* in/out: help file control block	*/
    TEXT		subcmd[]	/* in: subcommand qualifier to search	*/

 );


/* bldshlp - read in help level 1 data for a subcommand and for the remaining
 * subcommands on this tutor page.
 */

FUNCTION CODE bldshlp 
(
    struct CONTXT	*pctx,		/* in/out: all the parameters		*/
    struct SUBCMD	*s,		/* in/out: the 1st subcommand of this page*/
    FUNINT		lsubpg,		/* in:  page # of last page built	*/
    struct TXTSTOR	*title,		/* in:  title text (just to know # lines)*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    IMPORT COUNT	  termlines;	/* number lines on terminal screen	*/

    COUNT		pagnum;		/* page # of page being built		*/
    struct TUTEXT	*stp;		/* tutor extension of current subc being built into this page*/	
    COUNT		startlin;	/* starting display line # for this subc*/
    COUNT		usedlin;
    COUNT		maxlin;		/* max lines for level 1 text on any page*/
    COUNT		nsubpg;		/* # subcommands in this page		*/
    struct SUBCMD	*lastsub;	/* prev subcommand in subc table	*/
    CODE		code;
    TEXT		subcmd[SUBCMDSIZ+1];

    maxlin = termlines  - 		/* minus total overhead lines:  */
	(ERRLIN 			/* keyin line + prompt line     */ 
	+ 1				/* error msg line		*/
	+ tutctx.pageStart - 1	 	/* number of header lines       */ 
	+ tutctx.spaceLines);		/* propagated bug for compat    */
    usedlin = 0;			/* # lines used on this page	*/
    nsubpg  = 0;
    lastsub = NULL;
    pagnum  = lsubpg + 1;
    do
	{
	stp = &(*s).ext;
	s_copy((*pctx).subcmd, subcmd);
	while (!(*stp).l1hexist)	/* while lev 1 help for this subc not yet found*/
	    {
	    if ((code = inlevel1(pctx, hf, subcmd)) == FAIL)
		goto inl1_err;		/* input .SUBCMD text for 1 subc*/
	    if (code == TUT_H1DONE)
		{
		if (NULLSTR(subcmd)) break;
		subcmd[0] = EOS;
		tutctx.h1done = FALSE;
		f_movpos(&tutctx.lev1start, &tutctx.lasth1pos);
					/* now start over on .LEVEL1	*/
		}
	    }
	if (usedlin != 0)
	    usedlin += tutctx.spaceLines;	/* for inter-subc gap on screen		*/
	nsubpg  += 1;
	startlin = usedlin;
	usedlin += max((*stp).l1help.numline, 1);	/* max of # lines lev 1 help, value(s)*/
	if (usedlin > maxlin  &&  nsubpg > 1)	/* if this subc won't fit on page*/
	    break;
	(*stp).startlin = startlin;
	(*stp).linrange = usedlin;
	(*stp).pagnum = pagnum;
	if (usedlin >= maxlin)
	    break;
	lastsub = s;
	} while ((s = (*s).link) != NULL);
    if (s == NULL  &&  lastsub != NULL)
	{
	stp = &(*lastsub).ext;
	tutctx.lastpag = (*stp).pagnum;
	}
    else if (s != NULL  &&  (*s).link == NULL)
	tutctx.lastpag = (*stp).pagnum;
    return(SUCCESS);

inl1_err:
    return(FAIL);
    }

/* bldvhlp - read in help level 1 data for a parm and for the remaining parms
 * in this tutor page.
 */

FUNCTION CODE bldvhlp 
(
    struct CONTXT	*pctx,		/* in/out: all the parameters		*/
    struct VARIABLE	*v,		/* in/out: the 1st variable of this page*/
    FUNINT		lvarpg,		/* in:  page # of last page built	*/
    struct TXTSTOR	*title,		/* in:  title text (just to know # lines)*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    IMPORT COUNT	termlines;	/* number of lines on terminal screen	*/
    COUNT		pagnum;		/* page # of page being built		*/
    struct TUTEXT	*vtp;		/* tutor extension of current var being built into this page*/	
    COUNT		startlin;	/* starting display line # for this var	*/
    COUNT		usedlin;
    COUNT		maxlin;		/* max lines for level 1 text on any page*/
    COUNT		nvarpg;		/* # variables in this page		*/
    struct VARIABLE	*lastvar;	/* prev var in symbol table		*/
    struct VARIABLE	*nextv;		/* look ahead in list		*/
    CODE		code;
    TEXT		subcmd[SUBCMDSIZ+1];
    COUNT		l1_lines;

    /****
    In the following calculation, we propagate a bug--the last useable 
    line is not used--when in normal tutor mode.  We must do such for
    compatibility with screens in the field.  For the new compressed tutor,
    when tutctx.spaceLines is zero, we can use the last line.
    ****/

    maxlin = termlines  - 		/* minus total overhead lines:  */
	(ERRLIN 			/* keyin line + prompt line     */ 
	+ 1				/* error msg line		*/
	+ tutctx.pageStart - 1	 	/* number of header lines       */ 
	+ tutctx.spaceLines);		/* propagated bug for compat    */
    usedlin = 0;			/* # lines used on this page		*/
    nvarpg  = 0;
    lastvar = NULL;
    pagnum  = lvarpg + 1;
    do
	{
	if ((*v).v_tp == NULL)		/* if this var has no tutor extension	*/
	    if (((*v).v_tp = (struct TUTEXT *) tae_alloc(1, sizeof(struct TUTEXT)))
		== NULL) goto over_err;
	vtp = (*v).v_tp;
	s_copy((*pctx).subcmd, subcmd);
	while (!(*vtp).l1hexist)	/* while lev 1 help for this parm not yet found*/
	    {
	    if ((code = inlevel1(pctx, hf, subcmd)) == FAIL)
		goto inl1_err;		/* input .VAR text for 1 parm*/
	    if (code == TUT_H1DONE)
		{
		if (NULLSTR(subcmd)) break;
		subcmd[0] = EOS;	/* reset for search w/o subcmd		*/
		tutctx.h1done = FALSE;
		f_movpos(&tutctx.lev1start, &tutctx.lasth1pos);
					/* now start over on .LEVEL1	*/
		}
	    }
	if (usedlin != 0)
	    usedlin += tutctx.spaceLines; /* for inter-parm gap on screen */
	nvarpg  += 1;
	startlin = usedlin;
	l1_lines = (*vtp).l1help.numline;
	if ((*v).v_pv12  &&  (*v).v_qualst.link != NULL) /* if parm has quals...*/
	    l1_lines++;				/* for qualifier notice		*/
	usedlin += max(l1_lines, nlinval(v));	/* max of # lines lev 1 help, value(s)*/
	if (usedlin > maxlin  &&  nvarpg > 1)	/* if this isn't 1st parm on page & won't fit*/
	    break;
	(*vtp).startlin = startlin;
	(*vtp).linrange = usedlin;
	(*vtp).pagnum = pagnum;
	if (inipan(v) != SUCCESS) goto over_err;	/* init panel posit array elements	*/
	adjval(v, 1);					/* init panel top to 1st component*/
	if (usedlin >= maxlin)		/* if no more parms will fit on page	*/
	    break;

 	nextv = (*v).v_link;
	if (nextv != NULL && (*nextv).v_page)		/* PARM-PAGE...	*/
	    break;					/* (force new)	*/
	lastvar = v;
	} while ((v = (*v).v_link) != NULL);
    if (v == NULL  &&  lastvar != NULL)
	{
	vtp = (*lastvar).v_tp;
	tutctx.lastpag = (*vtp).pagnum;
	}
    else if (v != NULL  &&  (*v).v_link == NULL)
	tutctx.lastpag = (*vtp).pagnum;
    return(SUCCESS);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);		/* dyn mem overflow			*/
    return(FAIL);

inl1_err:
    return(FAIL);
    }

/* inlevel1 - input level 1 help text for 1 parm or subcommand.
 */

FUNCTION static CODE inlevel1 
(
    struct CONTXT	*pctx,		/* in/out: proc context w/ tab to update.	*/
    struct SFILE	*hf,		/* in/out: help file control block	*/
    TEXT		subcmd[]	/* in: subcommand qualifier to search	*/

 )
    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics 	*/

    TEXT		dirctv[STRINGSIZ+1];
    TEXT		field[STRINGSIZ+1];
    TEXT		scope[STRINGSIZ+1];
    TEXT		prmname[SUBCMDSIZ+1];
    TEXT		proc[FSPECSIZ+1];
    TEXT		subname[SUBCMDSIZ+1];
    struct SYNBLK	sb;
    struct VARIABLE	*v;
    struct SUBCMD	*s;
    struct TUTEXT	*t;
    TEXT		str[STRINGSIZ+1];
    CODE		code, code1;
    struct DIRBLK	db;			/* directive block for d_ pkg	*/
    TEXT		libe[FLIBRSIZ+1];
    struct CONTXT	*ctx;

    if (!tutctx.h1exist  ||  tutctx.h1done)
	return(TUT_H1DONE);
    f_setpos(hf, &tutctx.lasth1pos);	/* restore last lev 1 help position	*/
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
    s_copy ((*ctx).pdf.libr, libe);	/* make sure we have a libe good for internal procs */
    d_init(&db, hf, libe,
	(TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);	/* init directive block	*/
    while (FOREVER)
	{
	if ((code = d_dirctv(&db, dirctv, field)) != SUCCESS) goto read_err;	/* input next directive line*/
	f_movpos(&(*hf).posctx, &tutctx.lasth1pos);
	if (!s_lseq(".VAR", dirctv)  &&  !s_equal(".SUBCMD", dirctv))
	    {
	    tutctx.h1done = TRUE;
	    return(TUT_H1DONE);
	    }
	if (tutctx.subtut  &&  s_equal(".SUBCMD", dirctv))
	    break;
	if (!tutctx.subtut  &&  s_lseq(".VAR", dirctv))	
	    break;
	}
    initok(&sb, field);			/* init sb for syntax pkg		*/
    code = gettok(&sb, prmname);
    if (code != S_ALPHA && (!tutctx.subtut || code != '-')) goto name_err;
    if (tutctx.subtut)
	{
	if (code == '-')
	    prmname[0] = EOS;
	else
	    prmname[SUBCMDSIZ] = EOS;	
	if ((s = subex((*pctx).subptr, prmname)) == NULL) goto nsubc_err;
	t = &(*s).ext;
	}
    else
	{
	prmname[NAMESIZ] = EOS;
	proc[0] = EOS;
	subname[0] = EOS;
	s_copy(sb.curchr, scope);
	if (!NULLSTR(scope))
	    cmd_parse(&sb, scope, NULL, proc, subname);
	if (!NULLSTR(proc) && !s_equal((*pctx).pdf.name, proc)) goto nparm_err;
	if (!s_equal(subcmd, subname)) goto nparm_err;
	if ((v = lookex(&(*pctx).parmst, prmname)) == NULL) goto nparm_err;
	if ((*v).v_tp == NULL)
	    if (((*v).v_tp = (struct TUTEXT *) tae_alloc(1, sizeof(struct TUTEXT))) == NULL)
		goto over_err;
	t = (*v).v_tp;
	}
    if ((*t).l1hexist) goto name_err;
    (*t).l1hexist = TRUE;
    code1 = SUCCESS;				/* in case we get out at once */
    while ((code = txtread(&db, str, &tutctx.lasth1pos)) == SUCCESS)
	{
	if ((code1 = addtxt(&(*t).l1help, str)) != SUCCESS) break;
	}
    d_incclose(&db);				/* close .include file, if any	*/
    if (code != D_EOT) goto read_err;
    if (code1 != SUCCESS) goto over_err;
    if (tutctx.screen)
	if ((brktxt(&(*t).l1help, TUTHL1SIZ)) != SUCCESS) goto over_err;	/* break text up for descr field width*/
    return(SUCCESS);

read_err:
    tutmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);	/* tutor mode help file read error	*/
    tutctx.h1done = TRUE;
    return(TUT_H1DONE);

nsubc_err:
nparm_err:
name_err:
    f_read(hf, str);
    f_movpos(&(*hf).posctx, &tutctx.lasth1pos);
    return(SUCCESS);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);			/* dyn mem overflow		*/
    return(FAIL);
    }

/* inlevel2 - input level 2 help text for a parm or subcommand.
 *
 * 	The tutor extension flag t.l2hexist is set for the variable
 *	for which level 2 help is next encountered. This flag is checked
 *	by the caller to determine if l2 help exists for a particular
 *	variable (or subcommand).
 * NOTE:  Unrecognized directive after .LEVEL2 will cause remaining
 *	  .VAR, .SUBCMD directives to be lost.
 *
 */

FUNCTION CODE inlevel2 
(
    struct CONTXT	*pctx,		/* in/out: proc context w/ tab to update.*/
    struct SFILE	*hf,		/* in/out: help file control block	*/
    TEXT		type[],		/* in: subcmd/parm			*/
    TEXT		subcmd[]	/* in: subcommand qualifier to search	*/

 )
    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics 	*/

    TEXT		dirctv[STRINGSIZ+1];
    TEXT		field[STRINGSIZ+1];
    TEXT		scope[STRINGSIZ+1];
    TEXT		prmname[NAMESIZ+1];
    TEXT		proc[FSPECSIZ+1];
    TEXT		subname[SUBCMDSIZ+1];
    struct SYNBLK	sb;
    struct VARIABLE	*v;
    struct SUBCMD	*s;
    struct TUTEXT	*t;
    TEXT		str[STRINGSIZ+1];
    CODE		code;
    struct DIRBLK	db;			/* directive block for d_ pkg	*/
    TEXT		libe[FLIBRSIZ+1];
    struct CONTXT	*ctx;

    if (tutctx.nohelpf || tutctx.srch2cplt)	/* if search complete, no help		*/
	goto nhelp_err;
    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/"));
		ctx = (*ctx).backlink);
    s_copy ((*ctx).pdf.libr, libe);
    d_init(&db, hf, libe,
	(TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);    /* init directive block	*/

    if (!tutctx.h2exist)		/* if lev 2 help existence not yet known*/
	{
	if (tutctx.h1exist)		/* if level1 exists			*/
	    f_setpos(hf, &tutctx.lasth1pos); /* start looking from lev 1 help posit	*/
	else
	    f_setpos(hf, &(*hf).posctx);	/* look from current position	*/
	while ((code = d_dirctv(&db, dirctv, field)) == SUCCESS)
	    {
	    if (s_equal(dirctv, ".LEVEL2"))		/* .level2 help found	*/
		break;
	    else if (s_equal(dirctv, ".HELP"))		/* .HELP  encountered	*/
		{
		f_movpos(&(*hf).posctx, &tutctx.hlppos);/* save .help posctx	*/
		tutctx.hlpexist = TRUE;			/* mark as exists	*/
		}
	    else if (s_equal(dirctv, ".END"))		/* End of file reached	*/
	        {
	        tutctx.srch2cplt = TRUE;
	        goto nhelp_err;
	        }
	    }
	if (code != SUCCESS)
	    goto read_err;
	if (f_read(hf, str) != SUCCESS) goto read_err;
	tutctx.h2exist = TRUE;
	f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
	f_movpos(&(*hf).posctx, &tutctx.lev2start);
	}
    f_setpos(hf, &tutctx.lasth2pos);	/* restore last lev 2 help position	*/
    while (FOREVER)
	{
	if (d_dirctv(&db, dirctv, field) != SUCCESS) goto read_err;	/* input next directive line*/
	f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
	if (!s_lseq(".VAR", dirctv)  &&  !s_equal(".SUBCMD", dirctv))
	    {
	    tutctx.srch2cplt = TRUE;
	    goto nhelp_err;
	    }
	if (s_equal(type, "subcmd") &&  s_equal(".SUBCMD", dirctv))
	    break;
	if (s_equal(type, "parm") &&  s_lseq(".VAR", dirctv))
	    break;
	}
    initok(&sb, field);			/* init sb for syntax pkg		*/
    code = gettok(&sb, prmname);
    if (code != S_ALPHA && (!s_equal(type, "subcmd") || code != '-'))
	goto noparm_err;
    if (s_equal(type, "subcmd"))
	{
	if (code == '-')
	    prmname[0] = EOS;
	else
	    prmname[SUBCMDSIZ] = EOS;
	s = subex((*pctx).subptr, prmname);
	if (s == NULL) goto nparm_ret;
	t = &(*s).ext;
	}
    else
	{
	prmname[NAMESIZ] = EOS;
	proc[0] = EOS;
	subname[0] = EOS;
	s_copy(sb.curchr, scope);
	if (!NULLSTR(scope))
	    cmd_parse(&sb, scope, NULL, proc, subname);
	if (!NULLSTR(proc) && !s_equal((*pctx).pdf.name, proc)) goto nparm_ret;
	if (!s_equal(subcmd, subname)) goto nparm_ret;
	v = lookex(&(*pctx).parmst, prmname);
	if (v == NULL) goto nparm_ret;
	if ((*v).v_tp == NULL)
	    if (((*v).v_tp = (struct TUTEXT *) tae_alloc(1, sizeof(struct TUTEXT)))
		== NULL) goto over_err;
	t = (*v).v_tp;
	}
    if ((*t).l2hexist) goto nparm_ret;
    (*t).l2hexist = TRUE;
    f_movpos(&(*hf).posctx, &(*t).l2hpos);	/* save help posit for this var	*/
    f_read(hf, str);				/* pass this position		*/
    f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
    return(SUCCESS);

nparm_ret:
    f_read(hf, str);
    f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
    return(SUCCESS);

nhelp_err:
    return(TUT_NOL2HELP);				/* no leve2 help 	*/

read_err:
    tutmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);	/* tutor mode help file read error*/
    return(FAIL);

noparm_err:
    tutmsg("Level 2 .VARIABLE statement missing a parameter name.", "TAE-TNPNAM", 0, 0, 0, 0, 0);
    return(FAIL);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);		/* dyn mem overflow			*/
    return(FAIL);
    }

/*
 *	tut_h_prep - prepare for help info in opened help file.
 *	The file may be the PDF, in which case it may be compiled,
 *	or it may be a separate file.
 *	Upon return from this function, file is positioned to first line
 *	of .LEVEL1 text.
 */

FUNCTION static CODE tut_h_prep 
(
    struct CONTXT	*pctx,		/* in: proc context			*/
    struct SFILE	*hf,		/* in/out: help file			*/
    struct TXTSTOR	*title		/* out: blodk of .TITLE text		*/

 )
    {
    IMPORT struct VARIABLE *char_gbl;	/* system characteristics		*/

    struct CONTXT	*ctx;
    TEXT		libe[FLIBRSIZ+1];
    struct DIRBLK	db;		/* directive ctrl blk for d_  package	*/
    struct CP_HEADER	header;		/* compiled PDF header record		*/
    COUNT		recsize;
    CODE		code;
    TEXT		field[STRINGSIZ+1];
    TEXT		dirtyp[STRINGSIZ+1];
    TEXT		ttlmsg[STRINGSIZ+1];
    TEXT		ttlkey[STRINGSIZ+1];

    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/"));
	    ctx = (*ctx).backlink);
    s_copy ((*ctx).pdf.libr, libe);
    d_init(&db, hf, libe,
	(TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);	/* init directive block	*/
    if ((*pctx).compiled)			/* if compiled PDF		*/
	{
	f_rewind(hf);
	f_read(hf, field);			/* dummy read -- space to header*/
	code = f_bread(hf, (GENPTR)&header, sizeof(struct CP_HEADER), &recsize);
	if (code != SUCCESS)
	    goto read_err;
	if (header.type != C_HEADER)
	    goto hdr_type_err;
	if (header.title.possav)		/* if .TITLE text exists			*/
	    {
	    f_setpos(hf, &header.title);	/* position to .TITLE			*/
	    code = f_read(hf, field);		/* read the .TITLE record		*/
	    if (code != SUCCESS)
		goto read_err;
	    code = gettitle(&db, title, ttlmsg, ttlkey);
	    if (code != SUCCESS)
		goto title_err;
	    }
	if (header.help.possav)			/* if .HELP text exists		*/
	    {
	    MOVE_STRUCT(header.help, tutctx.hlppos); /* save .HELP posit	*/
	    tutctx.hlpexist = TRUE;
	    }
	if (header.level2.possav)		/* if .LEVEL2 help text exists	*/
	    {
	    f_setpos(hf, &header.level2);	/* posit to .LEVEL2		*/
	    f_read(hf, field);
	    f_read(hf, field);			/* dummy reads to pass .LEVEL2	*/
	    f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
	    f_movpos(&(*hf).posctx, &tutctx.lev2start);
	    tutctx.h2exist = TRUE;
	    }
	if (header.level1.possav)		/* if .LEVEL1 help text exists	*/
	    {
	    f_setpos(hf, &header.level1);	/* posit to .LEVEL1		*/
	    f_read(hf, field);
	    f_read(hf, field);			/* dummy reads to pass .LEVEL1	*/
	    f_movpos(&(*hf).posctx, &tutctx.lasth1pos); /* save as last level 1 help posit*/
	    f_movpos(&(*hf).posctx, &tutctx.lev1start); /* & permanently as start of .LEVEL section*/
	    tutctx.h1exist = TRUE;
	    }
	tutctx.nohelpf= !tutctx.hlpexist && !tutctx.h1exist && !tutctx.h2exist;
	}
    else					/* not in compiled PDF		*/
	{
	while ((code = d_dirctv(&db, dirtyp, field)) == SUCCESS)
	    {
	    if (s_equal(dirtyp, ".TITLE"))		    /* if .TITLE text present 	*/
		{
		if (gettitle(&db, title, ttlmsg, ttlkey) != SUCCESS)
		    goto title_err;
		}
	    else if (s_equal(dirtyp, ".HELP"))
		{
		f_movpos(&(*hf).posctx, &tutctx.hlppos);    /* save .HELP posit*/
		tutctx.hlpexist = TRUE;
		}
	    else if (s_equal(dirtyp, ".LEVEL1"))
		{
		f_read(hf, field);		/* dummy read to get past .LEVEL1	*/
		f_movpos(&(*hf).posctx, &tutctx.lasth1pos);	/* save as last level 1 help posit*/
		f_movpos(&(*hf).posctx, &tutctx.lev1start);	/* & permanently as start of .LEVEL section*/
		tutctx.h1exist = TRUE;
		break;
		}
	    else if (s_equal(dirtyp, ".LEVEL2"))
		{
		f_read(hf, field);		/* dummy read to get past .LEVEL2	*/
		f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
		f_movpos(&(*hf).posctx, &tutctx.lev2start);
		tutctx.h2exist = TRUE;
		}
	    else if (s_equal(dirtyp, ".END"))
		{
		tutctx.nohelpf=	!tutctx.hlpexist  &&
				!tutctx.h1exist  &&  !tutctx.h2exist;
		break;
		}
	    else
		goto struct_err;		/* otherwise badly structured help file	*/
	    }
	if (code != SUCCESS)
	    goto dir_err;
	}
    return(SUCCESS);

title_err:
    tutmsg(ttlmsg, ttlkey, 0, 0, 0, 0, 0);		/* help file title read error     */
    goto close_err;

struct_err:
    tutmsg("Unrecognized directive '%s' in help file.", "TAE-BADHLPDIR", 
	   (uintptr_t) dirtyp, 0, 0, 0, 0);
    fretxt(title);
    goto close_err;

dir_err:
    tutmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);	/* help file read error			*/
    fretxt(title);
    goto close_err;

read_err:
    tutmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);	/* help file read error			*/
    goto close_err;

hdr_type_err:
    tutmsg("Compiled PDF has bad header record.", "TAE-CPDFBHDR", 0, 0, 0, 0, 0);
    goto close_err;

close_err:
    return(FAIL);
    }

/* tutohlp - open .HELP file for tutor and return .TITLE text.
 * Upon return, help file is positioned to 1st line of .LEVEL1 text.
 */

FUNCTION CODE tutohlp 
(
    struct SFILE	*pdf,		/* in:  PDF				*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct TXTSTOR	*title,		/* out: block of .TITLE text (in dyn mem)*/
    struct SFILE	**ohf		/* out: help file control block	*/

 )
    {
    TEXT		field[STRINGSIZ+1];
    CODE		code;
    struct SFILE	*hf;		/* local version of ohf		*/
    CODE		terminat;
    TEXT		libe[FLIBRSIZ+1];
    struct CONTXT	*ctx;
    BOOL		sephelp;

    if ((*pctx).compiled  ||
	s_equal((*pctx).help_spec, "*"))	/* if help info in PDF		*/
	{
	sephelp = FALSE;
	hf = pdf;
	*ohf = hf;
	}
    else
	{
	sephelp = TRUE;
	if ((hf = (struct SFILE *) tae_alloc(1, sizeof(struct SFILE))) == NULL)
	    goto over_err;
	*ohf = hf;
	if ((*pctx).intrinsic)
	    code = f_open(hf, HELPLUN, HELPLIB, (*pctx).pdf.name,
		HLP_TYPE, F_READ);
	else
    	    {
    	    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	    s_copy ((*ctx).pdf.libr, libe);
	    code = f_opnspc(hf, HELPLUN, (*pctx).help_spec,
		   libe, (*pctx).pdf.name, HLP_TYPE, F_READ);
    	    }
        if (code != SUCCESS) goto open_err;
	}
    code = tut_h_prep(pctx, hf, title); /* find & save posits at least thru .LEVEL1*/
    if ((*pctx).compiled)
	tutctx.hlp_searched = TRUE;	/* all searching done for compiled PDFs	*/
    if (code != SUCCESS)
	goto prep_err;
    return(SUCCESS);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);		/* dyn mem overflow		*/
    tutctx.nohelpf = TRUE;
    goto prompt;

open_err:
    tae_free((GENPTR)hf);
    *ohf = NULL;			/* to keep cls_tutor from freeing */
    tutctx.nohelpf = TRUE;
    return(SUCCESS);

prep_err:
    tutctx.nohelpf = TRUE;
    if (sephelp)
	{
	f_close(hf, F_KEEP);
	tae_free((GENPTR)hf);
	sephelp = FALSE;
	*ohf = NULL;			/* to keep cls_tutor from freeing */
	}
    goto prompt;

prompt:
    t_write("Do you wish to continue (Y/N)?", T_PROMPT);
    t_read(field, &terminat);
    if (s_lseq("Y", field))
	return(SUCCESS);
    else
	return(FAIL);
    }
