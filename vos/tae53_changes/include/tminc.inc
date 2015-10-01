/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TM include file.  Contains all host-independent TM-only definitions.
 *
 *	CHANGE LOG:
 *
 *	16-DEC-82	Removed all includes; See includes.c...palm
 *	16-DEC-82	Add link to previous context block;  this
 *			may help debugging and someday we may need
 *			to look back for VARIABLE searches...palm
 *	11-JAN-83	Add CMD_RETPRIM
 *	27-JAN-83	New CONTXT fields...palm
 *	31-JAN-83	Add NOLATCH, etc.,struct TXTSTOR, screen constants...PEB
 *	01-FEB-83	add UNDEFINE (value undefined)...PEB
 *	07-FEB-83	add TUTMHELPLUN...PEB
 *	10-feb-83	New symbols for goto, break, next...palm
 *	10-feb-82	Subst DYNFLATCH, DYNSLATCH for DYNLATCH,
 *			add FULLSET, SUBSET
 *	16-feb-83	Add SEARCH structure...palm
 *	04-mar-83	New ITRCMD struct; DO codes...palm
 *	09-mar-83	New CONTXT block for new file processing...palm
 *	21-mar-83	New SUBCMD structure...palm
 *	22-mar-83	TMINC_BASE definition...palm
 *	22-mar-83	Y_SUBSRCH bit...palm
 *	23-MAR-83	Modify tutor screen constants for subc display...peb
 *	24-mar-83	New subsiz field in CONTXT...palm
 *	27-mar-83	New size of DEFVRB.repvrb...palm
 *	29-mar-83	Remove CMDSIZ; you now use FNAMESIZ...palm
 *	01-apr-83	New syntax for ptr in TXTSTOR...palm
 *	01-apr-83	New body_line in CONTXT...palm
 *	02-apr-83	Add SOME_REJECTED...peb
 *	15-apr-83	New ctx_ins in CONTXT...palm
 *	19-apr-83	Add bit definition to CONTXT block (intrinsic)..palm
 *	21-apr-83	Add DEL_GLOBAL, HELP_INTRO, NOT_SPECIAL, SUB_SEARCH
 *			and CONTXT.special with above defns...peb
 *	22-apr-83	Add struct INXREC, delete structs DEFTAB, DEFVRB...dm
 *	22-apr-83	Add TUTOR_ATTEMPT...peb
 *	27-apr-83	Add GETPLATCH...nhe
 *	29-apr-83	Add new Y_ bits, change Y_GENERAL, make 
 *			'flags' LONG...nhe
 *	15-MAY-83	Add Y_BATCH to Y_DECLARE...jtm
 *	05-jun-83	Add FAIL_NOHELP...nhe
 *	09-jun-83	Add other FAIL_XXX codes...dm
 *	25-jul-83	Add TUTCOMPNUMCOL, value panel members in TUTEXT...peb
 *	27-jul-83	Change TUTVALCHDCOL to 55...peb
 *	09-aug-83	Change TUTEXT .pagnum, .linrange to UTINY...peb
 *	23-aug-83	Add DP_NOTDONE...peb
 *	25-aug-83	Add TRUNCATE...palm
 *	26-sep-83	Add DP_COMPLETED...palm
 *	27-sep-83	Remove DYNFLATCH, DYNSLATCH, GETPLATCH...peb
 *	05-oct-83	Changed ERRLIN and PMTLIN definitions...dm
 *	30-oct-83	Add TUTOR_ACTIVATION and CMD_ACTIVATION...peb
 *	25-jan-84	New EDTCTX structure for micro-editor...palm
 *			And A_NONE, A_LASTCMD, A_TUTOR...palm
 *	03-feb-84	Add DO_CONTINUE, DO_ABORT...nhe
 *	06-feb-84	Add Y_RETURN...nhe
 *	07-feb-84	Eliminate Y_INTERRUPT, add Y_LOOPBOUND, 
 *			Y_ABI, Y_DISPLAY...nhe
 *	09-feb-84	Rename NEW_DSP, PROMPT_NEW, KEEP_DSP to 
 *			AUTO_PAINT, PROMPT_PAINT, NO_PAINT respectively...dm
 *	23-feb-84	Cleanup EDTCTX...palm
 *	29-feb-84	Add MON_MSG structure
 *	01-mar-84	Add ACB...nhe
 *	06-mar-84	Add parent_type to MON_MSG...nhe
 *	07-mar-84	Add proc to ACB...nhe
 *	11-mar-84	Add wait_num to ACB...nhe
 *	13-mar-84	Change proc field in ACB...palm
 *	03-may-84	mon_msg.stdout-->mon_msg.std_out (for UNIX)...nhe
 *	06-may-84	Remove .defalt from RESIDVAR...peb
 *	07-may-84	Change toprefs to be a COUNT; rename to numrefs...nhe
 *	30-may-84	Add Y_DEFSUB...palm
 *	13-jun-84	Add .interrupt bit to CONTXT...palm
 *	14-jun-84	Add Y_PDF bit for itrcmd...palm
 *	14-jun-84	New ITR_BADCMD, ITR_BADSUB, ITR_PDF...palm
 *	15-jun-84	New EDTCTX.vindex...palm
 *	11-jul-84	Fix comments in TUTEXT struct...peb
 *	13-jul-84	Add CODE_ERR...nhe
 *	01-aug-84	New file spec size in MON_MSG;
 *			New CONTXT and SUBCMD fields for compiled PDFs...palm
 *	06-aug-84	New MON_MSG for new async activation scheme...palm
 *	05-sep-84	Add VM_DVP, VM_VALID, VM_PDF, VM_TUTEXT...peb
 *	10-sep-84	VM_ flags should be bit assignments...peb
 *	13-sep-84	Add node and remote flag to ACB...nhe
 *	15-sep-84	Remove VM_TUTEXT...peb
 *	27-sep-84	Add MO_STATUS to MON_MSG...nhe
 *	03-oct-84	Add MO_CONT...nhe
 *	05-oct-84	Add acbptr to CONTXT and HI_FSBLOCK to MON_MSG...nhe
 *	11-oct-84	Update MON_MSG and ACB with conditionals for UNIX
 *			as implemented on UNIX 1.2...dm
 *	12-oct-84	TCL 117: Added struct COMP_HOUSEKEEP...peb
 *	14-oct-84	Add Y_PROCSEARCH, SRCH_PROC, & (*CONTXT).int_procs...nhe
 *	16-oct-84	Add MO_GETPAR for async GETPAR requests...jtm
 *	23-oct-84	Add .before & .after to COMP_HOUSEKEEP...peb
 *	25-oct-84	Add DO_HOLD...nhe
 *	28-oct-84	Make key size in MON_MSG bigger...nhe
 *	05-nov-84	Add GET_PARM_QUAL...peb
 *	07-nov-84	Add NOPROC & SAVEPROC for flag help_proc...lia
 *	13-nov-84	Add VM_NMREF...peb
 *	20-nov-84	Break up ACB to ACB and ATCB for RCJM agents...dm
 *	01-dec-84	Add TUTCTX structure...peb
 *	07-dec-84	TCL 67: add TUT_ defines from tutor breakup...peb
 *	07-dec-84	TCL 67: remove EDTCTX structure...peb
 *	10-dec-84	Add .notify to ACB
 *	10-dec-84	Add MO_EXIT to mon_msg type definition...dm
 *	10-dec-84	Add TUT_ACCEPT...peb
 *	11-dec-84	TCL 97: add flag for onfail command executing in
 *			CONTXT block and line number in SEARCH block...lia
 *	14-dec-84	TCL 67: Add tutctx.parmname ...peb
 *	14-dec-84	TCL 97: add onfail command flag in SEARCH block...lia
 *	28-dec-84	PR 905: move in ICBLOCK structure...lia
 *
 **** COPIED VERSION FROM TAE$INC TREE
 *	28-jan-85	Add int_chan field to struct ATCB...dm
 *	30-jan-85	Remove all async related structs to asyncinc...dm
 *	21-apr-85	Declare CONTXT.acbptr as GENPTR for portability...dm
 *
 *	15-aug-85	Make negative constants of form (-1) rather than
 *			-1 to help compiler catch "{X Y}"...palm
 *	09-sep-85	PR 961: Add CONTXT.asydyntut to identify if tutoring
 *			for async or sync proc...dab
 *	03-nov-86	From old RCJM TAECONF: add JOB_EXIT, JOB_ERR,
 *			PROC_INVOC, TUTOR_INVOC, HELP_REQ...nhe
 *
 **** RETROFIT OF CHANGE TO UNIX v1.3
 *	13-nov-86	Originally 25-jul-85: Include SYNINC to fix
 *			UNIX lint error.
 *	13-jul-87	add new parmqual bit to CONTXT...palm
 ************************************
 *
 *	07-aug-87	Add new A_MENU for menu editor...palm
 *	09-aug-87	New spare definition for CONTXT...palm
 *	02-feb-88	PR1540: CONTXT.LABEL and SEARCH.LABEL get LABELSIZ+1
 *			dimensions instead of NAMESIZE+1...ln
 *	02-may-88	Add new parfile bit to support exeucution 
 *			of PAR files...palm
 *	28-nov-88	New selftutor field in CONTXT...palm
 *	09-dec-88	New tutctx fields, spaceLines and pageStart...palm
 *	01-feb-89	New widths in RESIDVAR for minc, maxc, etc. ...palm
 *	03-feb-89	Widen .pantop and .panbot in TUTEXT for new
 *			MAXVAL...palm
 *	02-aug-89	PS/2 port...ljn
 *	05-oct-92	PR1669 TUTCTX prev_cmd is set to TOKESIZ...tpl
 *
 * Last arbitrary constant value used (coord with FILE, TERMINAL):	73
 */

#ifndef I_TMINC			/* prevent double include		*/
#define I_TMINC	0

#include	"symtab.inc"	/* TM symbol table defs			*/
#include	"fileinc.inp"	/* file package defs			*/
#include	"compiled.inc"	/* for compiled PDFs			*/
#include	"syninc.inc"	/* Syntax block defs			*/

/* TAE standard constants:	*/

#define TMINC_BASE	1	/* base; many of these must be <256	*/

#define AUTO_PAINT	30	/* paint screen automatically	   	*/
#define	CMD_ACTIVATION	58	/* vs. TUTOR_ACTIVATION			*/
#define	CMDMODE		17	/* vs. MENUMODE, TUTORMODE, INTMODE	*/
#define COMPILING	69	/* CONTXT.special flag			*/
#define	DEL_GLOBAL	44	/* CONTXT.special flag			*/
#define DO_SUCCESS	SUCCESS	/* normal intrinsic return (no _onfail)	*/
#define DO_RETURN	38	/* terminate proc; no _onfail		*/
#define DO_ABORT	51	/* returned when ABORT seen		*/
#define	DO_CHECK	39	/* hit _onfail if $SFI negative		*/
#define DO_CONTINUE	52	/* returned on CONTINUE && INTMODE	*/
#define DO_EXIT		49	/* 'exit' seen from while dyncmd mode	*/
#define DO_RUN		50	/* 'run' or 'tutor' run, dyncmd mode	*/
#define DO_HOLD		60	/* 'hold' from tutor			*/
#define DO_STOP		40	/* kill all levels			*/
#define	DP_NOTDONE	53	/* dp_ins didn't process dyn parm request*/
#define DP_COMPLETED	56	/* dp_ins completed the dynamic session	*/
#define DYNMODE		51	/* cmd_mode is dyncmd			*/
#define FAIL_NOHELP	(-1)	/* No help information available	*/
#define FAIL_NOTEXT	(-2)	/* No help-text in help file		*/
#define	FAIL_RDERR	(-3)	/* Error in reading help data		*/
#define	FAIL_ERPROC	(-4)	/* Error in proc name to help on	*/
#define	FAIL_NOPARHELP	(-5)	/* No help for the given parameter	*/
#define	FAIL_AMBIGPAR	(-6)	/* Ambiguous parameter name for help	*/
#define	FULLSET		37	/* vs. SUBSET (for dynamic tutor)	*/
#define GET_PARM_QUAL	70	/* CONTXT.special flag			*/
#define	HELP_INTRO	45	/* vs. DEL_GLOBAL,NOT_SPECIAL,SUB_SEARCH,TUTOR_ATTEMPT*/
#define	INTMODE		18	/* vs. CMDMODE, MENUMODE, TUTORMODE	*/
#define	ITR_PDF		60	/* itr_lookup: this is a Y_PDF entry	*/
#define	ITR_BADCMD	61	/* itr_lookup: command not found	*/
#define	ITR_BADSUB	62	/* itr_lookup: subcommand not found	*/
#define JOB_EXIT	72	/* user wants remote job to exit	*/
#define JOB_ERR		73	/* remote job terminated with error	*/
#define	MENUMODE	19	/* vs. CMDMODE, INTMODE, TUTORMODE	*/
#define NO_PAINT	29	/* do not paint screen (keep the display)*/
#define	NOLATCH		33	/* vs. RUNLATCH				*/
#define NOPROC		41	/* vs. SAVEPROC				*/
#define NORMCMD		52	/* cmd_code is normal (not dyn or int)	*/
#define	NOT_SPECIAL	46	/* vs. DEL_GLOBAL,HELP_INTRO,SUB_SEARCH,TUTOR_ATTEMPT*/
#define	OPERATOR	14	/* OPERATOR or SCRIPT			*/
#define PROCFAIL	(-1)	/* standard failure status code for proc*/
#define PROMPT_PAINT	31	/* prompt before painting screen	*/
#define	QUALVALSIZ	15	/* max qualifier value size		*/
#define	RUNLATCH	34	/* vs. NOLATCH				*/
#define SAVEPROC	42	/* vs. NOPROC				*/
#define	SOME_REJECTED	43	/* some variables rejected		*/
#define SRCH_BREAK	1	/* searching for BREAK place		*/
#define SRCH_NEXT	2	/* searching for NEXT place		*/
#define SRCH_GOTO	3	/* searching for GOTO label		*/
#define SRCH_PROC	4	/* searching for matching END-PROC	*/
#define	SCRIPT		15	/* OPERATOR or SCRIPT			*/
#define	STACK_EMPTY	1	/* if stack empty			*/
#define	STACK_FAULT	(-1)	/* error code from stack manip functions*/
#define	STACK_OVRHD	3	/* number of TINYs required for stack housekeeping*/
#define	STTMT_FAIL	27	/* statement fail code			*/
#define	SUBSET		38	/* vs. FULLSET				*/
#define	SUB_SEARCH	47	/* vs. DEL_GLOBAL,HELP_INTRO,NOT_SPECIAL,TUTOR_ATTEMPT*/
#define TRUNCATE	54	/* forpval: value truncated		*/
#define	TUTMHELPLUN	1	/* LUN for help on tutor mode		*/
#define	TUTOR_ACTIVATION 59	/* vs. CMD_ACTIVATION			*/
#define	TUTOR_ATTEMPT	48	/* vs. DEL_GLOBAL,HELP_INTRO,NOT_SPECIAL,SUB_SEARCH*/
#define	TYPE_ELSE	4	/* ELSE type value, found on type stack.*/
#define	TYPE_ELSEIFCHK	2	/* Found on type stack if still	looking for true condition.*/
#define	TYPE_ELSEIFND	3	/* Found on type stack if already found
				 * a true condition within an if/elseif construct.*/
#define	TYPE_IF		1	/* IF type value, found	on type stack.	*/
#define	TYPE_NONE	0	/* Value pushed on the top of type stack
				 * at initialization. Indicates not
				 * currently within any construct.	*/
#define TYPE_FOR	5	/* FOR type value, on type stack	*/
#define	TYPE_LOOP	6	/* LOOP type value, on type stack	*/
#define TYPE_PROC	7	/* PROC type				*/
#define	TUTORMODE	20	/* vs. CMDMODE, INTMODE, MENUMODE	*/
#define	UNDEFINE	35	/* value undefined			*/

/*
 * bit flags for vmove() indicating which special parts of variable
 * structure should be saved in PARBLK.
 */

#define	VM_DVP		0x01	/* for vmove() -- copy DVP vector	*/
#define	VM_PDF		0x02	/* for vmove() -- copy PDF structure	*/
#define	VM_VALID	0x08	/* for vmove() -- copy valid structure	*/
#define VM_NMREF	0x10	/* for vmove() -- leave type=NAME ref intact*/
			
/* constants for formatted displays	*/

#define	ERRLIN		2	/* error msg line (subtract from max lines)	*/
#define	HDRLIN		1	/* display header line			*/
#define	PMTLIN		0	/* prompt line (e.g., "?") (subtract from max lines)*/
#define	TITLELIN	3	/* title line				*/
#define	TUTSUBDESCCHDCOL 22	/* tutor subc descr column header column*/
#define	TUTSUBDESCCOL	17	/* tutor subc descr column		*/
#define	TUTVARDESCCHDCOL 16	/* tutor parm descr column header column*/
#define	TUTVARDESCCOL	11	/* tutor parm descr column		*/
#define	TUTHL1SIZ	32	/* width of tutor help level 1 descr field	*/
#define	TUTNUMRTCOL	19	/* rightmost col for int, real vals (count forward from TUTVALCOL)*/
#define	TUTNAMECHDCOL	3	/* tutor parm or subc name column header column	*/
#define	TUTNAMECOL	1	/* tutor parm or subc name column 		*/
#define	TUTVALCHDCOL	55	/* tutor parm value column header column*/
#define	TUTVALCOL	47	/* tutor parm value column 		*/
#define	TUTVALSIZ	26	/* width of tutor value field		*/
#define	TUTCOMPNUMCOL	74	/* tutor component number column	*/
#define	TUTSUBDFLTCOL	53	/* tutor column for "**DEFAULT**" on subc displ*/


/* General error message structure for errors with CODES and text	*/

    struct CODE_ERR
    	{
    	TEXT	*text;		/* text of the error message		*/
    	TEXT	*key;		/* message key				*/
    	CODE	code;		/* message code				*/
    	};


/* bits for INTRINSIC.flags:						*/

#define	Y_PRIM		0x0001	/* permitted on primary level		*/
#define Y_PROC  	0x0002	/* permitted in PDF			*/
#define Y_INTRO 	0x0004	/* permitted as introductory PDF cmd	*/
#define Y_PREBODY	0x0008	/* permitted before BODY 		*/
#define Y_BODY		0x0010	/* permitted in BODY			*/
#define Y_QUALIF	0x0020	/* qualfiers allowed			*/
#define Y_SEARCH	0x0040	/* call even in search mode		*/
#define Y_FALSE		0x0080	/* call even in false bracket		*/
#define Y_ENDSUB	0x0100	/* call even in inactive subcommand	*/
#define Y_PROCSYN	0x0200	/* proc invocation syntax		*/
#define	Y_CLEAR		0x0400	/* clear $SFI and $SKEY before call	*/
#define Y_ONFAIL	0x0800	/* valid as an _ONFAIL command		*/
#define Y_SUBSRCH	0x1000	/* call this even when subcmd searching	*/
#define Y_CMD		0x2000	/* command allowed in normal cmd mode	*/
#define Y_DYNCMD	0x4000	/* command allowed in dynamic cmd mode	*/
#define Y_INTER	       0x10000 /* command allowed when runtype=interactive*/
#define Y_BATCH	       0x20000 /* command allowed when runtype=batch	*/
#define Y_ASYNC	       0x40000 /* command allowed when runtype=async	*/
#define Y_DEFSUB       0x80000 /* flags the entry with the default subcmd */
#define Y_PDF	      0x100000 /* do this command/subcommand via PDF   	*/
#define Y_PROCSEARCH  0x200000 /* call in PROC search mode (for int procs) */
#define Y_ANYSUB      0x400000  /* allow any subcommand (for NOP)	*/
#ifdef SYNTAX_CHECK
#define Y_SYNTAX      0x800000  /* don't exec in syntax check mode (VICAR ext)*/
#endif

#define Y_ABI	    Y_ASYNC | Y_BATCH | Y_INTER	/* all runtypes; for convenience */
#define Y_GENERAL   Y_PRIM|Y_PROC|Y_BODY|Y_PROCSYN|Y_CLEAR|Y_ABI|Y_CMD  /*general case */
#define Y_GENINTER  Y_PRIM|Y_PROC|Y_BODY|Y_PROCSYN|Y_CLEAR|Y_INTER|Y_CMD  /*general , not batch or async */
#define Y_GENQUAL   Y_GENERAL|Y_QUALIF				/* general and qualfiers */
#define Y_DECLARE   Y_PROC|Y_PREBODY|Y_PROCSYN|Y_CMD|Y_ABI
#define Y_DISPLAY   Y_BODY|Y_PROCSYN|Y_CMD|Y_ABI
#define	Y_LOOPBOUND Y_PROC | Y_BODY | Y_SEARCH | Y_PROCSYN | Y_CMD | Y_ABI
#define Y_RETURN    Y_PROC | Y_BODY | Y_ONFAIL | Y_PROCSYN | Y_CMD | Y_ABI


#define	I_NPM(A)	(sizeof(A) / sizeof(struct RESIDVAR))	/* # parms in intrins cmd*/


/* Structure for resident PDFs.  This bears some similarity to the VARIABLE
 * structure, but makes certain assumptions:
 *
 *	- no link is required since the number of parms for an intrinsic
 *	  command can be determined at compile time.
 *	- the type is never V_NAME
 *	- the class is always V_PARM (rather than V_GLOBAL or LOCAL)
 *	- no hidden parameters
 *	- no file parameters (i.e., filemode is always V_NOTFILE)
 *	- no tutor extension
 *	- the current count and current values are not specified
 */

    struct RESIDVAR
	{
	char		name[NAMESIZ+1];	/* variable name          */
	TINY		type;		/* V_INTEGER, V_REAL, or V_STRING */
	BIT_FIELD_ALIGN
	unsigned	kiv    : 1;	/* TRUE if this is a keyword      */
	BIT_FIELD_ALIGN
	COMPACT_COUNT	minc;		/* minimum vector count	          */
	COMPACT_COUNT	maxc;		/* maximum vector count	          */
	COUNT		size;		/* max string size	          */
	COMPACT_COUNT	dcount;		/* default count	          */ 
	GENPTR		valid;		/* pointer to VALID structure     */	
	GENPTR		dvp;		/* pointer to default value vector*/
	};


/* Constants for proc types	*/

#define	Y_UNKNOWN	0
#define	Y_PROCESS	1
#define	Y_PROCEDURE	2
#define	Y_GLOBAL	3
#define Y_PARMSET	4
struct SUBCMD;
struct COMP_HOUSEKEEP;

    struct CONTXT			/* proc context structure		*/
	{
	struct CONTXT	*backlink;		/* pointer to previous CONTXT	*/
	COUNT		numrefs;		/* nmbr defd gbl - zero means none defined*/
	struct VARIABLE	*refs[MAXREF];		/* ptrs to refs (gbls) - NULL indicates deleted*/
	struct SYMTAB	locst;			/* local symbol table		*/
	struct SYMTAB	parmst;			/* parameter symbol table	*/
	struct SYMTAB	qualst;			/* command qualifier symb table	*/
	struct POSCTX	prcpos;			/* current posit in procedure	*/
	CODE		proctype;		/* Y_PROCESS, Y_PROCEDURE, or Y_GLOBAL*/
	TINY		prclevel;		/* proc level (primary is 0)	*/
	BIT_FIELD_ALIGN
	unsigned	subblk : 1;		/* TRUE if in a SUBCMD block	*/
	unsigned	subact : 1;		/* FALSE if inactive due to not	*/
						/* being in right subcommand block*/
	unsigned	subfnd : 1;		/* TRUE if correct SUBCMD blk has been found*/
	unsigned	inbody : 1;		/* TRUE if in procedure body	*/
        unsigned	subs : 1;		/* TRUE if subcmds exist	*/
	unsigned	intrinsic : 1;		/* TRUE if intrinsic command 	*/
	unsigned 	interrupt : 1;		/* TRUE if OPTIONS=INTERRUPT	*/
	unsigned	compiled : 1;		/* TRUE if compiled PDF		*/
	unsigned	onfailcmd : 1;		/* TRUE if executing onfail cmd	*/
	unsigned	asydyntut : 1;		/* TRUE if tutoring for async job */
	unsigned	parmqual :1;		/* TRUE for parm qualifier 	*/
	unsigned	parfile : 1;		/* TRUE if executing par file */

	unsigned 	selftutor : 1;		/* TRUE if options=selftutor  */
	unsigned	compress  : 1;		/* TRUE if compressed tutor   */
	BIT_FIELD_ALIGN
	TINY		locifs[MAXIF+STACK_OVRHD];	/* local if stack	*/
	TINY		glbifs[MAXIF+STACK_OVRHD];	/* global if stack	*/
	TINY		typstk[MAXNEST+STACK_OVRHD];	/* type of each nesting (FOR, IF, etc.)*/
	struct SYNBLK	*sb;			/* syntax block (if not proc invoc syntax)*/
	TEXT		label[LABELSIZ+1];	/* label of current line in this proc		*/
	struct FSBLOCK	pdf;			/* pdf file components		*/
	TEXT		subcmd[SUBCMDSIZ+1];	/* sub-command			*/
	TEXT		exe_spec[FSPECSIZ+1];	/* "EXECUTE =" image file spec	*/
	TEXT		help_spec[FSPECSIZ+1];	/* "HELP =" help file spec	*/
	COUNT		pdf_line;		/* line number of: last line	*/
						/* of the previous command	*/
	GENPTR		icbptr;			/* iteration block chain header	*/
	GENPTR		srchblk;		/* search block ptr (or NULL)	*/
	struct SUBCMD	*subptr;		/* subcmd list head pointer	*/
	TINY		subsiz;			/* chars in user-typed subcmd   */
        COUNT		body_line;		/* line # of last line of BODY	*/
        GENPTR		ctx_ins;		/* installation use		*/
	CODE		special;		/* special processing modes:	*/
						/* NOT_SPECIAL, DEL_GLOBAL,	*/
						/* HELP_INTRO, SUB_SEARCH,	*/
						/* or COMPILING			*/
	struct COMP_HOUSEKEEP *comp;		/* struct of compiled data	*/
	GENPTR		acbptr;		/* pointer to current ACB	*/
	GENPTR		int_procs;		/* chain of internal procs	*/
	TEXT		spare[40];		/* reserved		*/
	};

/* note that all subcommands for one command must be contiguous in the table */
struct RESIDVAR;

    struct ITRCMD			/* defines an intrinsic TAE command	*/
	{
	TINY		abbchr;		/* min # chars reqd (0 = all)		*/
	TEXT		*cmd;		/* command name				*/
	TEXT		*subcmd;	/* subcommand name		*/
	LONG		flags;		/* flag bits			*/
	TINY		numprm;		/* number of parameters defd for this cmd*/
	struct RESIDVAR	*partab;	/* static param table (null if no parms)*/
	CODE		(*cproc)(struct CONTXT *, struct CONTXT*);	/* command processing routine		*/
	};


/* structure for dynamically stored text strings.	*/

    struct TXTSTOR			/* dynamic text storage			*/
	{
	COUNT		numline;	/* # lines of text			*/
	TEXT		**tp;		/* ptr to vector of str ptrs (same as	*/
					/* string values in VARIABLE structure)	*/
	};

    struct TUTEXT			/* tutor information for this parm	*/
	{
	UTINY		pagnum;		/* page number of parm (0: not yet known)*/
	struct TXTSTOR	l1help;		/* level 1 help text			*/
	struct POSCTX	l2hpos;		/* level 2 help position		*/
	BIT_FIELD_ALIGN
	unsigned	l1hexist : 1;	/* TRUE if lev 1 help exists for this parm*/
	unsigned	l2hexist : 1;	/* TRUE if lev 2 help exists for this parm*/
	BIT_FIELD_ALIGN
	TINY		startlin;	/* starting display line #		*/
	UTINY		linrange;	/* range of line # (0: not yet known)	*/
	COMPACT_COUNT	pantop;		/* index of component at top of window	*/
	COMPACT_COUNT	panbot;		/* index of component at bottom of window	*/
	TINY		*compnlin;	/* ptr to array of number of display...	*/
					/* lines reqd for each component	*/
	};


#define	A_NONE	  0x1000	/* type of 'archive' for editor calls	*/
#define A_TUTOR   0x1001
#define A_LASTCMD 0x1002
#define A_MENU	  0x1004	

#define NOFORCE	   	0x7555
#define FORCE_NOSCREEN	0x7556
#define FORCE_SCREEN	0x7557

/* tutor specific codes	*/

#define	TUT_EXIT	5		/* code to exit from tutor		*/
#define	TUT_H1DONE	6
#define	TUT_RUNSUB	7
#define	TUT_TUTPARM	8		/* leave subcommand mode to tutor on parms*/
#define	TUT_PREV	9		/* cmd returned a prev mode command	*/
#define TUT_NOL2HELP	10		/* no level2 help text for a parm	*/
#define TUT_HOLD	11		/* return on HOLD command		*/
#define	TUT_ACCEPT	12		/* ACCEPT in param qualifier tutor space*/
struct SUBCMD;

    struct TUTCTX		/* tutor context for micro-editor & tutor use*/
        {
	BIT_FIELD_ALIGN
        unsigned	edtcmd : 1;	/* edt: TRUE if tutor cmd generated by editor */
	unsigned 	screen : 1; 	/* edt: TRUE if screen mode		*/
	unsigned	subtut : 1; 	/* edt: TRUE if subcommand display	*/
	unsigned	start  : 1; 	/* edt: TRUE if latched at vcur/index	*/
	unsigned	highact: 1; 	/* edt: TRUE if highlighting active	*/
	BIT_FIELD_ALIGN
	struct VARIABLE *vcur;		/* edt: current parameter		*/
	COUNT		index;		/* edt: current index in vcur	*/
	COUNT		vindex;		/* edt: current S_VALID index	*/
	struct SUBCMD 	*cursub;	/* edt: future: current subcommand 	*/
	COUNT		curpag;		/* edt: current page number displayed*/
	struct CONTXT 	*ctx;		/* edt: context of tutored proc	*/
	struct POSCTX  	saved_pos;	/* saved position of the PDF		*/
	COUNT	  	saved_line;	/* saved # of last line of prev sttmt	*/
	BOOL		tut_open;	/* TRUE if cls_tutor not yet called*/
	struct SFILE	*helpf;		/* help file				*/
	struct TXTSTOR 	title;		/* .TITLE text (dyn storage cntrl block)*/
	TEXT		prev_cmd[TOKESIZ+1];	/* prev mode cmd buffer (for help)*/
	BIT_FIELD_ALIGN
	unsigned	prev_mode   : 1;	/* true if prev mode command	*/
	unsigned	prev_hopen  : 1;	/* TRUE if help file already opened*/
	unsigned	dyntut      : 1;	/* TRUE if dynamic tutor		*/
	unsigned	hlpexist    : 1;	/* TRUE if position of .HELP directive known*/
	unsigned	hlp_searched :1;	/* TRUE if searched for .HELP	*/
	unsigned	h1exist     : 1;	/* TRUE if .LEVEL1 encountered	*/
	unsigned	h2exist     : 1;	/* TRUE if .LEVEL2 encountered	*/
	unsigned	h1done      : 1;	/* TRUE if all lev 1 help read	*/
	unsigned	nohelpf     : 1;	/* TRUE if there is no help file*/
	unsigned	dispreq     : 1;	/* TRUE if tutor lev 1 page display reqd*/
	unsigned	srch1cplt   : 1; /* TRUE if tutor lev 1 has been resolved for all parms*/
	unsigned	srch2cplt   : 1; /* TRUE if proc has been completely searched for help level 2 data*/
	unsigned	crt_messages :1;	/* TRUE if screen mode messages	*/
	unsigned	msg_held    : 1;	/* TRUE if msg held till next disp page	*/
	unsigned	qualtut     : 1;	/* TRUE if parm qual tutor display*/
	BIT_FIELD_ALIGN
	struct POSCTX	lev1start;	/* 1st record after .LEVEL1 statement	*/
	struct POSCTX	lev2start;	/* 1st record after .LEVEL2 statement	*/
	struct POSCTX	lasth1pos;	/* most recent posit in help 1 text	*/
	struct POSCTX	lasth2pos;	/* most recent posit in help 2 text	*/
	struct POSCTX	hlppos;		/* position of .HELP directive in help file*/
	COUNT		lastpag;	/* highest numbered tutor page		*/
	TEXT		held_msg[STRINGSIZ+1];
	TEXT		held_key[KEYSIZ+1];
	struct CONTXT	*saved_proc;	/* saved proc context		*/
	struct VARIABLE	*preface;	/* preface for noscreen mode	*/
	TEXT		parmname[NAMESIZ+1];	/* name of parm being qualified -- qual disp only*/
	COUNT		spaceLines;	/* number of space lines 'tween parms*/
	COUNT		pageStart;	/* line number of first val on page  */ 
	COUNT		titleLine;	/* line number of first title line   */
	};	


    struct SUBCMD
        {
        struct SUBCMD	*link;			/* link to next subcmd	*/
	TEXT	        name[SUBCMDSIZ+1];	/* subcmd name		*/
	BIT_FIELD_ALIGN
	unsigned	deflt : 1;		/* TRUE = default	*/
	BIT_FIELD_ALIGN
	struct TUTEXT	ext;			/* help ptrs, etc.	*/ 

	/* the following fields are used for compiled PDFs. */
	struct POSCTX	position;		/* start of PARBLKs	*/
	struct SYMTAB 	symtab;			/* VARAIBLEs for this   */
						/* subcommand		*/
	};

/* help-message index record structure		*/

    struct  INXREC
	{
	TEXT	key[KEYSIZ+1];			/* message key		*/
	struct	POSCTX	posctx;			/* .key record position ctx */
	};



/*	SEARCH structure.
 *	
 *	This is the structure which describes the search context.
 *	The search context is established when a GOTO, BREAK, or NEXT
 *	TCL command is encountered. The structure defines the type
 * 	search and, if GOTO, the label to go to.  Also used for internal
 *	proc search.
 */
    struct	SEARCH
	{
	CODE		type;			/* SRCH_BREAK, SRCH_GOTO, or 
 *						   SRCH_NEXT */
	TINY		srchstk[MAXNEST + STACK_OVRHD];	/* Search stack */
	TEXT		label[LABELSIZ+1];	/* If GOTO, the label */
	COUNT		cmd_line;		/* Line number of command */
	BIT_FIELD_ALIGN
	unsigned	onfailcmd : 1;		/* TRUE if onfail command */
	BIT_FIELD_ALIGN
	};

/*  ICBLOCKs are linked into the proc CONTXT.  It is the iteration
 *  control block established when a FOR or LOOP TCL command is encountered.
 */

    struct	ICBLOCK			/* Defines an iteration control block */
	{
	struct	ICBLOCK	*i_link;	/* Link to next block in chain */
	TINY		   type;	/* Type of block, TYPE_FOR or TYPE_LOOP */
	struct	POSCTX	 posctx;	/* File positioning context */
	COUNT	       pdf_line;	/* PDF line number of FOR or LOOP statement */
	struct	VARIABLE     *v;	/* Pointer to FOR variable */
	struct	SYMTAB	forlist;	/* Symbol table for FOR list values */
	};

/*
 *  IPBLOCKs are linked into the proc CONTXT.  They contain the
 *  record addresses of internal procs
 */

    struct IPBLOCK 		/* define for internal procs	*/
    	{
    	struct IPBLOCK 	*flink;
    	TEXT 		procname[FNAMESIZ+1];	/* int'l proc name */
    	struct 	POSCTX	position;		/* starting record on int'l proc */
    	};

/*  Compiled PDF housekeeping data.
 */

    struct COMP_HOUSEKEEP
	{
	struct CP_HEADER	*header;	/* ptr to im memory CP_HEADER	*/
	struct POSCTX		pos_header;	/* pos of CP_HEADER in compiled*/
	struct SYMTAB		before;		/* for before subcommands	*/
	struct SYMTAB		after;		/* for after subcommands	*/
	struct SUBCMD		*cursub;	/* current subcommand		*/
	};

/*
 *	Type of user commands executed at a remote node
 */
#define PROC_INVOC	1
#define	TUTOR_INVOC	2
#define	HELP_REQ	3





#define AC_TUTOR	0
#define AC_CMD		1
#endif
