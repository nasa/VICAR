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
 *	Variable declaration intrinsic commands.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	19-aug-83	Nullables; new rule for DEFAULT names; 
 *			PR 429 fix...palm
 *	29-aug-83	Keyword parameters...palm
 *	02-sep-83	PR 429: check PARM versus existing gbl in
 *			gbl_defaults; PR 488...palm
 *	06-sep-83	Make DEFGBL have INITIAL rather than DEFAULT...palm
 *	08-sep-83	Set v_file bit when type=file...palm
 *			Valid table for ACCESS parameter...palm
 *			V_NOTFILE changed to V_NOCHECK...palm
 *	14-sep-83	Remove allocation of v_dvp; only tutor needs this
 *			so we let it create v_dvp...palm
 *	21-sep-83	Order global symbol table alphabetically...dm
 *	21-sep-83	Fix RESIDVAR init for UNIX compatibility...palm
 *	21-sep-83	Fix v_count error in set_defaults...palm
 *	25-sep-83	Fix s_s2i for v_size as output...palm
 *	19-oct-83	PR 539 fix (COUNT mishandled)...palm
 *	20-jan-84	New PARM-PAGE feature...palm
 *	07-feb-84	Replace Y_BATCH with Y_ABI...nhe
 *	13-mar-84	Allow REFGBL in body...palm
 *	25-mar-84	Fix message for double refgbl (PR 555)...palm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Clean-up the usage of IMPORT...lim
 *	06-jun-84	New logic in set_default to catch situation where
 *			a DEFAULT or INITIAL specification is de-referenced.
 *			(TCL-102)...palm
 *	8-jun-84	TCL-99: check level 0 locals before permitting
 *			a global to be defined...palm
 *	25-jun-84	PR 782: fix error msg in refgbl_do...palm
 *	17-oct-84	PR 847: refgbl_do to accept refs of implicits...peb
 *	23-oct-84	TCL 117: Compilation (also alphabetize fcns)...peb
 *	30-oct-84	TCL 117: Enforce PDF restrictions for compilation...peb
 *	04-nov-84	TCL 67: Build parm qualifier symbol table...peb
 *	09-nov-84	PR 892: fix scoping rule; check new REFGBLs 
 *			against MAXREF...palm
 *	12-nov-84	TCL 117: Handle NAME parms & PARM DEFAULT derefs...peb
 *****************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	02-jan-85	Add -REDEFINE...nhe
 *	01-mar-85	Make -REDEFINE portable...nhe
 *	23-may-85 	Delete -REDEFINE subcommand, merge with TAE-V1.3...dm  	
 ****************************************************************************
 * MERGE BACK WITH TAE-V1.3...dm (23-may-85)
 *
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	03-oct-85	PR 946: 'set_defaults' passes "compiling" flag to
 *			'chk_vector' so no file access check if compiling...dab
 *	13-jul-87	Locals with qualifiers...palm
 *	19-jul-87	Fix gbl_defaults so that all parm qualifiers pick
 *			up default values from the global qualifiers.  In
 *			addition to gbl_defaults change, the QUALs are 
 *			now processed BEFORE defaults...palm
 *	22-jul-87	Add get_declearcmd () as part of effort to force TM 
 *			into libraries...ljn
 *	02-aug-87	Add TYPE=VOID and ACCESS=EVENT for TaePlus...palm
 *			(VOID types not fully implemented for now)
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	11-dec-87	New VIEW statement...palm
 *      20-jan-88       New REFVIEW command for declaring a view file...tpl
 *	21-feb-88	Fix DEFGBL crash by picking the right name of
 *			the initial value (INITIAL rather than DEFAULT)...palm
 *	26-jan-89	MAXVAL -> INT_MAXVAL in RESIDVAR; new valid list...palm
 *			Note that some RESIDVAR parms must be MAXVAL, for
 *			example, DEFAULT needs MAXVAL in order to set the
 *			value of a large parm...palm
 *	06-jan-90	Removed VIEW statement. Conditionally compiled out
 *			since we are so close to v4.1 delivery..krw	
 *			To compile with the code, define ADD_VIEW
 *	28-jun-90	Removed get_declarecmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"fileinc.inp"		/* file & file position context structs	*/
#include	"syninc.inc"		/* syntax package structure and defs	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
    
    GLOBAL int	v60declare = 0;		/* source version			*/

    struct VARIABLE		*search();	
    struct VARIABLE		*int_search();	
    struct VARIABLE		*lookex();
    static struct VARIABLE *addvar();

    static CODE gbl_defaults();
    static CODE set_defaults();


    static TEXT *one[]= {"1"};
    static TEXT *null_string[] = {""};
    static TEXT *nocheck[] = {"NOCHECK"};	
    static TEXT *defstring[] = {"STRING"};/* default type is string	*/

    BEGIN_VALIDS(access, 5)
	   "NOCHECK", "IN", "INOUT", "OUT", "EVENT"
    END_VALIDS

/* TOKESIZ cannot be used for "size" because VARIABLE's size is a UTINY	*/
/* and TOKESIZ is greater than 255...  rgd				*/

    static struct RESIDVAR ptlocl[] =	/* LOCAL and DEFGBL PDF		*/
	{
/* name    type      k  m maxc      size     dc val      dvp	*/

"NAME",    V_STRING, 0, 1, INT_MAXVAL, 
				   NAMESIZ, -1, NULL,   NULL,
"TYPE",    V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)defstring,
"COUNT",   V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)one,
"INITIAL", V_STRING, 0, 0, MAXVAL, 
				   MAXSTRSIZ,0, NULL,   NULL,
"VALID",   V_STRING, 0, 1, MAXVAL, 
				   VALIDSIZ,  1, NULL,   (GENPTR)null_string,
"ACCESS",  V_STRING, 1, 1, 1,      MAXSTRSIZ,1, (GENPTR)&access,
    							  (GENPTR)nocheck,
"QUALS",   V_STRING, 0, 0, 1,      MAXSTRSIZ,0, NULL,   NULL
	};


    static struct RESIDVAR ptparm[] =	/* PARM PDF			*/
	{
/* name    type      k  m maxc      size     dc val      dvp	*/

"NAME",    V_STRING, 0, 1, INT_MAXVAL, 
				   NAMESIZ, -1, NULL,   NULL,
"TYPE",    V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)defstring,
"COUNT",   V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)one,
"DEFAULT", V_STRING, 0, 0, MAXVAL, 
				   MAXSTRSIZ,0, NULL,   NULL,
"VALID",   V_STRING, 0, 1, MAXVAL, 
				   VALIDSIZ,  1, NULL,   (GENPTR)null_string,
"ACCESS",  V_STRING, 1, 1, 1,      MAXSTRSIZ,1, (GENPTR)&access,
    							  (GENPTR)nocheck,
"QUALS",   V_STRING, 0, 0, 1,      MAXSTRSIZ,0, NULL,   NULL
	};

    static struct RESIDVAR ptview[] =	/* VIEW PDF			*/
	{
/* name    type      k  m maxc      size     dc val      dvp	*/

"NAME",    V_STRING, 0, 1, INT_MAXVAL, 
				   NAMESIZ, -1, NULL,   NULL,
"TYPE",    V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)defstring,
"COUNT",   V_STRING, 0, 1, 2,      NAMESIZ,  1, NULL,   (GENPTR)one,
"TITLE",   V_STRING, 0, 0, MAXVAL, 
				   MAXSTRSIZ,0, NULL,   NULL,
"VALID",   V_STRING, 0, 1, MAXVAL, 
			           VALIDSIZ,  1, NULL,   (GENPTR)null_string,
"ACCESS",  V_STRING, 1, 1, 1,      MAXSTRSIZ,1, (GENPTR)&access,
    							  (GENPTR)nocheck,
"PRESENT", V_STRING, 0, 0, 1,      MAXSTRSIZ,0, NULL,   NULL
	};


    static struct RESIDVAR ptrefg[] =	/* REFGBL PDF			*/
	{
	/* name    type      k  m maxc      size     dc val      dvp	*/

	"NAME",    V_STRING, 0, 1, INT_MAXVAL, NAMESIZ,  -1, NULL,    NULL
	};

    static struct RESIDVAR ptrefview[] =	/* REFVIEW PDF		*/
	{
	/* name    type      k  m maxc      size          dc val      dvp	*/

	"NAME",    V_STRING, 0, 1,    1, MAXSTRSIZ,    -1, NULL,   NULL
	};


#ifdef ADD_VIEW
    CODE	view_do();
#endif
    CODE	parm_do(),  local_do(), refgbl_do(), defgbl_do();
    CODE        refview_do();

/* note: we allow LOCAL pre-body and body				*/
#define Y_LOCAL		Y_ABI | Y_PROCSYN | Y_PREBODY | Y_BODY |Y_CMD


    GLOBAL struct ITRCMD declarecmd[] =
        {
{0, "LOCAL",	"",  Y_LOCAL,		I_NPM(ptlocl), ptlocl, 	local_do   },
#ifdef ADD_REDEFINE
{0, "LOCAL","REDEFINE",Y_LOCAL,		I_NPM(ptlocl), ptlocl, 	local_do   },
#endif
{4, "REFVIEW",	"",  Y_DECLARE,		I_NPM(ptrefview), ptrefview,
                                                             refview_do    },
/*{4, "REFVIEW",	"SUBCMD",  Y_DECLARE,		I_NPM(ptrefview), ptrefview,
                                                             refview_do    },
{4, "REFVIEW",	"QUALIF",  Y_DECLARE,		I_NPM(ptrefview), ptrefview,
                                                             refview_do    },
*/
#ifdef ADD_VIEW
{0, "VIEW",	"",  Y_DECLARE,		I_NPM(ptview), ptview,  view_do    },
#endif
{0, "PARM",	"",  Y_DECLARE,		I_NPM(ptparm), ptparm,  parm_do    },
{0, "PARM", "PAGE",  Y_DECLARE,		I_NPM(ptparm), ptparm,  parm_do    },
{0, "REFGBL",	"",  Y_LOCAL,		I_NPM(ptrefg), ptrefg,  refgbl_do  },
{0, "DEFGBL",	"",  Y_LOCAL,		I_NPM(ptlocl), ptlocl,  defgbl_do  },
#ifdef ADD_REDEFINE
{0, "DEFGBL","REDEFINE",Y_LOCAL,	I_NPM(ptlocl), ptlocl,  defgbl_do  },
#endif
{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};

#ifdef ADD_VIEW
#define V_VIEW 0xF
#if  V_VIEW == V_PARM  || V_VIEW == V_LOCAL || V_VIEW == V_GLOBAL
	compilation error: we picked a bad constant for V_VIEW
#endif
#endif


/*	addvar - add a parameter to a symbol table.
 *	Adds a variable to a symbol table given a temporary symbol table
 *	built from a DEFGBL, PARM, or LOCAL command.
 *
 *	Return: VARIABLE ptr if success; NULL otherwise.
 */
    FUNCTION static struct VARIABLE *addvar (procctx, cmdctx, class)

    struct CONTXT	*procctx;	/* in/out: context of enclosing proc*/
    struct CONTXT	*cmdctx;	/* in: command context		    */
    FUNINT		class;		/* in: variable class		    */

    {
    IMPORT GENPTR		allval();
    IMPORT struct VARIABLE	*allvar();
    IMPORT struct VARIABLE	*alpha_var();
    IMPORT TEXT			*s_save();
    IMPORT struct SYMTAB	glbtab;
    IMPORT struct CONTXT	primctx;	/* level 0 context	*/

    struct VARIABLE	*tv;		/* target (new) variable	*/
    struct VARIABLE	*pv;		/* variable from statement	*/
    struct VARIABLE	*nv;		/* NAME variable in cmdctx	*/
    TEXT   vname[NAMESIZ+1];		/* name of variable being added */
    TAEINT		size, rmin, rmax; 
    COUNT		i;
    COUNT		j;		/* index in list of names	*/
    struct SYMTAB	*destst;	/* table to hold new variable	*/
    struct VARIABLE	*cv, *fv, *vv, *qv;	/* pointers in cmdctx.parmst*/
    struct VARIABLE	*gl;		/* referenced global		*/
    CODE		code;
    struct SUBCMD	*s;
    BOOL		compiling;
    BOOL		void_type;	/* TRUE if type=VOID		*/
    BOOL		viewStatement = FALSE;
    TEXT		*parmName;


#ifdef ADD_VIEW
if (class == V_VIEW) 			/* a VIEW is pretty much a PARM */
    {
    class = V_PARM;
    viewStatement = TRUE;		/* and this used to catch differences */
    }
#endif

/* addvar: NAME processing	*/

compiling = ((*procctx).special == COMPILING);
if (compiling)					/* if we're just compiling	*/
    {
    if ((*procctx).subblk)			/* if in a SUBCMD block	*/
	{
	s = (*(*procctx).comp).cursub;		/* current SUBCMD table entry*/
	destst = &(*s).symtab;			/* sym tab for this SUBCMD blk*/
	}
    else
	{
	if ((*procctx).subs)			/* if any SUBCMDs found yet	*/
	    destst = &(*(*procctx).comp).after; /* after subs symbol table	*/
	else
	    destst = &(*(*procctx).comp).before; /* before subs symbol table	*/
	}
    }
else						/* not compiling a PDF	*/
    {
    if (class == V_LOCAL)
	destst = &(*procctx).locst;		/* use local table	*/
    else if (class == V_PARM)
	destst = &(*procctx).parmst;		/* use parameter table	*/
    else
	destst = &glbtab;			/* use global table	*/
    }
nv = lookex(&(*cmdctx).parmst, "NAME");

for (j=0; j < (*nv).v_count; j++)		/* for each name	*/
    {						/* i.e., "PARM (a,b,c)"	*/
    if (name_check(SVAL(*nv,j)) != SUCCESS)	/* invalid name		*/
    	goto dp_badname;
    s_copy(SVAL(*nv, j), vname);		/* get the name		*/
    if (reserved (vname))			/* Reserved word 	*/
	goto dp_reserved;
    if (class == V_GLOBAL)			
        {					
        if (lookex (&glbtab, vname) != NULL)
    	    continue;				/* already exists: ignore */
	if (lookex (&primctx.locst, vname) != NULL)
	    goto dp_dupname;			/* globals must be unique */
        }					/* agaist level 0 locals  */
    else
    	if (int_search(vname, procctx) != NULL)
            goto dp_dupname;			/* name already exists	*/
    if (class == V_GLOBAL)			/* for globals - 	*/
	tv = alpha_var(vname, destst);		/* alloc and link alphabetically */	
    else
    	tv = allvar(destst);			/* alloc new vrbl & link into tab*/
    (*tv).v_class = class;
    s_copy(vname, (*tv).v_name);

    /*   addvar: TYPE processing		*/

    pv  = lookex(&(*cmdctx).parmst, "TYPE");	/* TYPE parm from sttmt  */
    void_type =	s_lseq(SVAL(*pv,0), "VOID");
    if (s_lseq(SVAL(*pv, 0), "INTEGER")  ||  void_type)
	{
	if ((*pv).v_count > 1)			/* if string size specified	*/
	    goto dp_cnerr;
	(*tv).v_type = V_INTEGER;		/* for now, let VOID=INTEGER 	*/
	}
    else if (s_lseq(SVAL(*pv, 0), "REAL"))
	{
	if ((*pv).v_count > 1)			/* if string size specified	*/
	    goto dp_cnerr;
	(*tv).v_type = V_REAL;
	}
    else if (s_lseq(SVAL(*pv, 0), "STRING"))
	{
	(*tv).v_type = V_STRING;
	if ((*pv).v_count == 2)			/* if string size specified	*/
	    {
	    if (s_s2i(SVAL(*pv, 1), &size) != SUCCESS)	/* get & convrt size*/
		goto dp_szerr;
	    if (size <= 0 || size > MAXSTRSIZ)	/* if str size specified too large*/
		goto dp_mszer;
	    (*tv).v_size = size;
	    }
	else
	    (*tv).v_size = MAXSTRSIZ;		/* string size default	*/ 
	}
    else if (s_lseq(SVAL(*pv, 0), "KEYWORD"))
        {
	(*tv).v_type = V_STRING;		/* actually a string	*/
	if ((*tv).v_class == V_PARM)
	    (*tv).v_keyword = TRUE;		/* make a keyword parm	*/
	(*tv).v_size = VALIDSIZ;
	}
    else if (s_lseq(SVAL(*pv, 0), "FILE"))
	{
	if ((*pv).v_count > 1)			/* if string size specified	*/
	    goto dp_cnerr;
	(*tv).v_file = TRUE;
	(*tv).v_type = V_STRING;		/* a FILE is a special string	*/
	(*tv).v_size = MAXSTRSIZ;		/* OK if MAXSTRSIZ>filespec size	*/ 
	}
    else if (s_lseq(SVAL(*pv,0), "NAME"))	/* checks for TYPE=NAME:*/
        {
        if (class != V_PARM)
     	    goto dp_namerr;
        (*tv).v_type = V_NAME;
        (*tv).v_ref = (*tv).v_dref = NULL;
	(*tv).v_nref = NULL;
        if ((*procctx).proctype == Y_GLOBAL)  
	    goto dp_namerr;
	}
    else					/* otherwise, illegal parm type	*/
	goto dp_tperr;				

    if (viewStatement && (*tv).v_type != V_STRING)
	goto dp_tperr;

    /* addvar: COUNT, VALID, and ACCESS	*/

    cv  = lookex(&(*cmdctx).parmst,  "COUNT");	
    vv  = lookex(&(*cmdctx).parmst,  "VALID");	
    fv  = lookex(&(*cmdctx).parmst,  "ACCESS");
    if ((*tv).v_type == V_NAME)
	{
	if (!(*cv).v_default || !(*vv).v_default || !(*fv).v_default)
	    goto dp_combo;			/* invalid combination	*/
        }
    else
        {
        (*tv).v_maxc = 0;			/* flag: no maxc yet	*/
        (*tv).v_nullable = FALSE;		/* assume not nullable	*/
        for (i=0; i < (*cv).v_count; i++)	/* for each 'range'	*/
	    {
	    if (irange(SVAL(*cv, i), &rmin, &rmax) != SUCCESS)
	        goto dp_nierr;
            if (rmin == 0)
		{
	        rmin = 1;			/* keep v_maxc > 0	*/
	        (*tv).v_nullable = TRUE;
    		}
            if (rmax != 0)			/* a 'real' range	*/
		{
    		if ((*tv).v_maxc != 0)		/* only one 'real' range*/
    		    goto dp_nierr;		/* allowed		*/
		if (rmax > MAXVAL)
		    goto dp_nverr1;
		if (rmin < 1)
		    goto dp_nverr2;
		(*tv).v_minc =  rmin;
		(*tv).v_maxc =  rmax;
		}
            }
        if ((*tv).v_maxc == 0) goto dp_nverr2;	/* must have some substance*/
	(*tv).v_count   = (*tv).v_dcount = -1;	/* assume no value for now */
	(*tv).v_cvp     = (*tv).v_dvp = NULL;
	if ((*tv).v_class == V_PARM  &&  (*tv).v_keyword  &&  (*vv).v_default)
	    goto valid_required;
        if (!(*vv).v_default)
	    if (bld_valid(destst, tv, (*vv).v_cvp, (*vv).v_count) != SUCCESS)
		goto dp_badvalid;
	if (s_equal(SVAL(*fv,0), "EVENT"))	/* "EVENT" okay for all types */
	    (*tv).v_event = TRUE;
	else if (!(*fv).v_default  &&  !(*tv).v_file)
	    goto dp_fmerr1;		    /* some ACCESS only good for file */
	else if (s_equal(SVAL(*fv,0), "NOCHECK"))
	    (*tv).v_filemode = V_NOCHECK;
	else if (s_equal(SVAL(*fv,0), "IN"))
	    (*tv).v_filemode = V_IN;
	else if (s_equal(SVAL(*fv,0), "INOUT"))
	    (*tv).v_filemode = V_INOUT;
	else if (s_equal(SVAL(*fv,0), "OUT"))
	    (*tv).v_filemode = V_OUT;
	else 
	    goto dp_fmerr;
        }

/* addvar:  QUALS (parameter qualifiers)	*/

/*	Class GLOBAL should be allowed but is not because of the
 *	way that TM:save_p_quals determines the target variable:
 *	it looks at the last in symbol table and uses that one.
 *	For globals, the last in table isn't the right one.  If
 *	you want qualified globals, go the formal route (use a GLOBAL PDF)
 *	and everything will work.
 */

    qv = lookex(&(*cmdctx).parmst, (viewStatement) ? "PRESENT" : "QUALS");
    if ((*qv).v_count > 0)		/* if a qualifier set was declared*/
	{
        if (class == V_GLOBAL)
            {
            tmmsg (PROCFAIL, "Qualfiers not allowed for DEFGBL.",
    			"TAE-NOQUAL");
            goto dp_badpqual;
            }
	code = set_p_quals(procctx, SVAL(*qv,0), class);  
	if (code != SUCCESS)
	    goto dp_badpqual;
	}

/* addvar: TITLE or INITIAL or DEFAULT	*/

    if ((*tv).v_type != V_NAME)
	(*tv).v_cvp = allval(tv);		/* allocate value vector    */
    if (viewStatement)
	parmName = "TITLE";
    else if (class == V_GLOBAL || class == V_LOCAL)
	parmName = "INITIAL";
    else 
	parmName = "DEFAULT";
    pv  = lookex(&(*cmdctx).parmst,  parmName);		/* get initial value */	
    code = SUCCESS;				
    if ((*procctx).proctype == Y_GLOBAL  &&
	(*tv).v_class == V_PARM  &&
	(gl = lookex(&glbtab, (*tv).v_name)) != NULL)	/* get defaults from...		*/
	code = gbl_defaults(tv, gl);			/* corresponding global		*/
    else if (!(*pv).v_default  &&  (*tv).v_type != V_NAME)
        code = set_defaults(tv, pv, compiling);
    else if (!(*pv).v_default  &&  (*tv).v_type == V_NAME)
        {
	if (compiling)
	    {
	    if ((*pv).v_type == V_NAME)
		(*tv).v_nref = s_save((*(*pv).v_ref).v_name);
	    else
		(*tv).v_nref = s_save(SVAL(*pv, 0));
	    }
	else					/* not compiling		*/
	    {
	    if ((*pv).v_type == V_NAME)			/* DEFAULT=@x	*/
		(*tv).v_dref = (*tv).v_ref = (*pv).v_ref;
	    else					/* DEFAULT=x	*/
		(*tv).v_dref = (*tv).v_ref = search(SVAL(*pv,0), procctx);
	    if ((*tv).v_ref == NULL)
		goto dp_badref;
	    (*tv).v_default = TRUE;
	    }
	}
    if (code != SUCCESS)
        goto dp_baddef;
    if (void_type)		/* void ignores DEFAULT, COUNT, etc	*/
	{
	(*tv).v_nullable = TRUE;		/* allow null value	*/
	(*tv).v_minc = 0;
	(*tv).v_maxc = 1;			
        (*tv).v_count = 0;			/* set null value	*/
	}
    }
return (tv);


dp_badname:
    tmmsg(PROCFAIL, "Invalid variable name '%s'.", "TAE-INVNAME",
    	SVAL(*nv,j));
    return(NULL);

dp_reserved:
    tmmsg(PROCFAIL, "'%s' is a reserved TCL word.", "TAE-RESERVED",
    	SVAL(*nv,j));
    return(NULL);

dp_namerr:
    tmmsg(PROCFAIL, "TYPE=NAME not allowed for '%s'.", "TAE-TYPERRN",
          (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return (NULL);

dp_dupname:
    tmmsg(PROCFAIL, "'%s' name conflicts with existing variable.",
    		 "TAE-VARCNFLCT", SVAL(*nv,j));
    return (NULL);

dp_cnerr:
dp_tperr:
    tmmsg(PROCFAIL, "Invalid TYPE specification for '%s'.",
    		 "TAE-TYPERR", (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return(NULL);

dp_szerr:
dp_mszer:
    tmmsg(PROCFAIL, "Invalid string size specification for '%s'.", 
    		"TAE-STRSIZ", (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return(NULL);

dp_combo:
    tmmsg(PROCFAIL, "VALID, ACCESS, and COUNT not allowed with TYPE = NAME.",
         "TAE-NAMERR");
    delvar(destst, tv);			/* delete the new variable	*/
    return (NULL);

dp_nverr1:
dp_nverr2:
dp_nierr:
    tmmsg(PROCFAIL, "Invalid COUNT specification for '%s'.", 
    	"TAE-CNTERR", (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return(NULL);

dp_fmerr:
    tmmsg(PROCFAIL, "Invalid ACCESS specification for '%s'.",
    	"TAE-ACCERR",  (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return(NULL);

dp_fmerr1:
    tmmsg(PROCFAIL, "ACCESS specification not allowed for non-FILE '%s'.",
	  "TAE-ACCNOTALL", (*tv).v_name);
    delvar(destst, tv);			/* delete the new variable	*/
    return (NULL);

valid_required:
    tmmsg(PROCFAIL, "VALID specification required for KEYWORD parameter '%s'.", 
	  "TAE-VALIDREQ", (*tv).v_name);
    delvar(destst, tv);
    return (NULL);

dp_badvalid:
dp_baddef:
dp_badpqual:
    delvar(destst, tv);
    return(NULL);

dp_badref:
    tmmsg(PROCFAIL, "Undefined DEFAULT variable reference '%s'.",
          "TAE-BADREF", SVAL(*pv,0));
    return (NULL);			/* parm only so no delete	*/
}

/*	refview_do - define a view by adding the intrinsic local _VIEW
 *      to the symbol table with the specified name as value.
 *	Build from a REFVIEW command.
 *
 *	Return: SUCCESS or FAIL
 */

    FUNCTION CODE refview_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in:  context of containing proc 	*/
    struct CONTXT	*cmdctx;		/* in:  context of PARM sttmt		*/

    {
    TEXT		viewfile[FSPECSIZ+1];  /* Full view file name*/
    IMPORT GENPTR		allval();
    IMPORT struct VARIABLE	*allvar();
    IMPORT struct VARIABLE	*alpha_var();
    IMPORT TEXT			*s_save();

    struct VARIABLE	*tv;		/* target (new) variable	*/
    struct VARIABLE	*pv;		/* variable from statement	*/
    struct VARIABLE	*nv;		/* NAME variable in cmdctx	*/
    TAEINT		size, rmin, rmax; 
    COUNT		i;
    COUNT		j;		/* index in list of names	*/
    struct SYMTAB	*destst;	/* table to hold new variable	*/
    struct VARIABLE	*cv, *fv, *vv, *qv;	/* pointers in cmdctx.parmst*/
    struct VARIABLE	*gl;		/* referenced global		*/
    CODE		code;
    struct SUBCMD	*s;
    BOOL		compiling;
    BOOL		void_type;	/* TRUE if type=VOID		*/
    TEXT                viewname[10];
    TEXT		*parmName;
    TEXT                **vcvp;



compiling = ((*procctx).special == COMPILING);
if (compiling)					/* if we're just compiling	*/
    {
    if ((*procctx).subblk)			/* if in a SUBCMD block	*/
	{
	s = (*(*procctx).comp).cursub;		/* current SUBCMD table entry*/
	destst = &(*s).symtab;			/* sym tab for this SUBCMD blk*/
	}
    else
	{
	if ((*procctx).subs)			/* if any SUBCMDs found yet	*/
	    destst = &(*(*procctx).comp).after; /* after subs symbol table	*/
	else
	    destst = &(*(*procctx).comp).before; /* before subs symbol table	*/
	}
    }
else						/* not compiling a PDF	*/
    {
	destst = &(*procctx).locst;		/* use local table	*/
    }

s_copy ( "_VIEWPDF", viewname );
s_copy ( "tutorview", viewfile );

if ( (*cmdctx).subcmd != NULL )
    {
    if (s_equal((*cmdctx).subcmd, "SUBCMD"))	/* if subcmd		*/
        {
        s_copy ( "_SUBCPDF", viewname );
        s_copy ( "subcview", viewfile );
        }

    else if (s_equal((*cmdctx).subcmd, "QUALIF"))/* if qualifier view	*/
        {
        s_copy ( "_QUALPDF", viewname );
        s_copy ( "qualview", viewfile );
        }
    }

nv = lookex(&(*cmdctx).parmst, "NAME");

if ( nv != NULL )
        s_copy ( SVAL(*nv, 0), viewfile );	/* get the name		*/



tv = int_search(viewname, procctx);
if (tv == NULL)
        {
    	tv = allvar(destst);			/* alloc new vrbl & link into tab*/
        (*tv).v_class = V_LOCAL;
        s_copy(viewname, (*tv).v_name );
	(*tv).v_file = TRUE;
	(*tv).v_type = V_STRING;
        (*tv).v_size = MAXSTRSIZ;		/* string size default	*/ 
        (*tv).v_maxc = 1;			/* flag: no maxc yet	*/
        (*tv).v_minc = 1;			/* flag: no maxc yet	*/
        (*tv).v_nullable = TRUE;        	/* assume not nullable	*/
	(*tv).v_count   = (*tv).v_dcount = -1;	/* assume no value for now */
	(*tv).v_cvp     = (*tv).v_dvp = NULL;
	(*tv).v_cvp = allval(tv);		/* allocate value vector    */
	}

code = set_string  (tv, viewfile );
if ( code == SUCCESS )
   return (DO_SUCCESS);
else
   return (DO_RETURN);
}

/*	defgbl_do - perform DEFGBL command.
 *	
 *	Creates a user defined global.
 *
 *!	If the user wants a "REDEFINE" we first save the old global in case
 *!	the new definition is no good.
 */

    FUNCTION CODE defgbl_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in:  context of containing proc 	*/
    struct CONTXT	*cmdctx;		/* in:  context of PARM sttmt		*/

    {
    struct VARIABLE	*v, *addvar();
#ifdef ADD_REDEFINE
    IMPORT struct SYMTAB glbtab;

    struct SYMTAB	tempst;			/* a temporary symbol table  */
    CODE		code;
    BOOL		redef = FALSE;

    tempst.link = NULL;
#endif

    if ((*procctx).special == COMPILING)	/* if we're compiling a PDF...	*/
	{
	tmmsg(PROCFAIL, "DEFGBL statement not allowed in compiled proc.",
	    "TAE-COMPDEFG");
	return (DO_RETURN);			/* DEFGBL not allowed		*/
	}
#ifdef ADD_REDEFINE
    if (s_equal ((*cmdctx).subcmd, "REDEFINE"))		/* redefine?	    */
    	{
    	code = redef_var (procctx, cmdctx, V_GLOBAL);
    	return ((code == SUCCESS)? DO_SUCCESS : DO_RETURN);
    	} 
#endif
    v = addvar(procctx, cmdctx, V_GLOBAL);
    return ((v != NULL)? DO_SUCCESS : DO_RETURN);  /* SUCCESS if not NULL    */
    }

/*	gbl_defaults.   Grab parm defaults from an existing global.
 *	This is called when (a) we are in a GLOBAL PDF, (b) we are
 *	defining a PARM, and (c) the PARM name is same as an existing
 *	global.  Note that any DEFAULT on the PARM statement is ignored.
 */

    FUNCTION static CODE gbl_defaults (tv, gl)

    struct VARIABLE	*tv;	/* in/out: variable being defined	*/
    struct VARIABLE	*gl;	/* in: existing global			*/
    	
    {
    struct VARIABLE *tvq;	/* target variable qualifier		*/
    struct VARIABLE *gvq;	/* global variable qualifier		*/
    CODE		code;

    if ((*tv).v_type != (*gl).v_type ||
        (*tv).v_size >  (*gl).v_size ||
        (*tv).v_minc <  (*gl).v_minc ||
        (*tv).v_maxc >  (*gl).v_maxc ||
	(*tv).v_nullable != (*gl).v_nullable)
        goto bad_ref;
    (*tv).v_default = TRUE;
    code = chk_vector (tv, (*gl).v_type, (*gl).v_cvp, (*gl).v_count, FALSE);
    if (code != SUCCESS)
        goto bad_ref;
    code = set_value (tv, (*gl).v_cvp, (*gl).v_count);
    if (code != SUCCESS)
	return (code);

/*	qualifiers: make sure every global qualifier has a destination	*/

    for (gvq = (*gl).v_qualst.link; gvq != NULL; gvq = (*gvq).v_link)
	{
	tvq = lookex (&(*tv).v_qualst, 	(*gvq).v_name);
	if (tvq == NULL)
	    goto bad_qref;
	code = gbl_defaults (tvq, gvq);
	if (code != SUCCESS)
	    return (code);
	}
    return (SUCCESS);


bad_ref:
    tmmsg(PROCFAIL, "Inconsistent re-definition of existing global '%s'.",
          "TAE-REDEF", (*gl).v_name);
    return (FAIL);

bad_qref:
    tmmsg(PROCFAIL, "Re-definition of existing global omits qualifier '%s'.",
          "TAE-REDEF", (*gvq).v_name);

    return (FAIL);
    }

/*
 *	local_do - perform LOCAL command.
 */

    FUNCTION CODE local_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in/out:  containing proc context	*/
    struct CONTXT	*cmdctx;		/* in:  ctxt from LOCAL command		*/

    {

    CODE		code;
    struct VARIABLE	*v;

#ifdef ADD_REDEFINE
    if (s_equal ((*cmdctx).subcmd, "REDEFINE"))		/* redefine?	    */
    	{
    	code = redef_var (procctx, cmdctx, V_LOCAL);
    	return ((code==SUCCESS)? DO_SUCCESS : DO_RETURN);
    	}
#endif
    v = addvar(procctx, cmdctx, V_LOCAL);
    return ((v!=NULL)? DO_SUCCESS : DO_RETURN);
    }

/*	parm_do - perform PARM command.
 *	Adds a variable to the dynamic symbol table of the proc being defined
 *	by this PDF.
 *
 *	Note that for GLOBAL procs, the parameters remain parameters until
 *	the proc is "executed".
 */

    FUNCTION CODE parm_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in:  context of containing proc 	*/
    struct CONTXT	*cmdctx;		/* in:  context of PARM sttmt		*/

    {
    struct VARIABLE	*v;

    v = addvar(procctx, cmdctx, V_PARM);
    if (v != NULL)
	{
	(*v).v_page = s_equal ((*cmdctx).subcmd, "PAGE");
        return (DO_SUCCESS);
	}
    else
        return (DO_RETURN);
    }

/*	view_do.
 *
 *	A VIEW statement is just like a PARM but some
 *	of the parameter names are changed to make
 *	VIEW statements look good.
 */

#ifdef ADD_VIEW
    FUNCTION CODE view_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in:  context of containing proc 	*/
    struct CONTXT	*cmdctx;		/* in:  context of PARM sttmt		*/

    {
    struct VARIABLE	*v;

    v = addvar(procctx, cmdctx, V_VIEW);
    if (v != NULL)
        return (DO_SUCCESS);
    else
        return (DO_RETURN);
    }
#endif

#ifdef ADD_REDEFINE
/*	redef_var.	Execute REDEFINE subcommand for declarations.
 *
 *	"Redefine" means to replace if the variable exists and just
 *	define as usual if it does not.  Note that we don't allow 
 *	replacement of "protected" globals; we then just use the new value.
 *
 *	Returns SUCCESS/FAIL
 */
    FUNCTION CODE	redef_var (procctx, cmdctx, class)

    struct CONTXT	*procctx;	/* in:  context of containing proc   */
    struct CONTXT	*cmdctx;	/* in:  context of delcaration sttmt */
    FUNINT		class;		/* in:  V_GLOBAL or V_LOCAL only     */

    {

    IMPORT struct SYMTAB	glbtab;	/* globals symbol table		     */

    struct SYMTAB	*stpt;
    struct SYMTAB	tempst;		/* a temporary symbol table  */
    struct VARIABLE	*v, *oldv, *iv, *genv, *find_slot(), 
				*allvar(), *lookex();
    struct VARIABLE	*savev = NULL;
    CODE		code;
    BOOL		redef = FALSE;

    tempst.link = NULL;
    genv = lookex (&(*cmdctx).parmst, "NAME");
    if ((*genv).v_count > 1 )
	{
	tmmsg (PROCFAIL, "Only one variable at a time can be redefined.",
			 "TAE-MAXREDEF");
	return (FAIL);
	}
    stpt = ((class == V_LOCAL)? &(*procctx).locst : &glbtab);
    oldv = lookex (stpt, SVAL(*genv,0));	
    if (oldv != NULL)			/* see if existing  	    */
	{
	if (class == V_GLOBAL && (*oldv).v_protect)   /* exists & protected, just update current   */
	    {
	    iv = lookex(&(*cmdctx).parmst,  "INITIAL");	/* declared initial value	     */
	    code = set_defaults(oldv, iv, FALSE);
	    return ((code==SUCCESS)? SUCCESS : FAIL);
	    }
	savev = allvar(&tempst);		/* we'll save oldv	    */
	specvcopy (oldv, savev);		/* save the old one	    */
    	if (class == V_GLOBAL)
	    del_gbl (&glbtab, oldv);		/* and delete it 	    */
    	else
    	    del_loc (procctx, oldv);		/* delete local		    */
	}
    v = addvar(procctx, cmdctx, class);
    if (v != NULL)				/* success		    */
    	{
    	if (savev != NULL)			/* successful redefinition  */
    	    {
    	    tempst.link = savev;
    	    delvar (&tempst, savev);		/* get rid of saved var	    */
    	    }
        return (SUCCESS);
    	}
    else if (savev != NULL && class == V_GLOBAL) /* failed while working on a redefinition */
    	{				       /* if 'redefine' put back old */
    	genv = find_slot ((*savev).v_name, &glbtab);    /* in correct alphabetic slot */
    	if (genv == NULL)
    	    {
    	    (*savev).v_link = glbtab.link;
    	    glbtab.link = savev;
    	    }
    	else
    	    {
    	    (*savev).v_link = (*genv).v_link;
    	    (*genv).v_link = savev;
    	    }
    	}
    else if (savev != NULL && class == V_LOCAL)  /* failed redefining a local */
    	{
    	for (genv = (struct VARIABLE *)&(*procctx).locst; (*genv).v_link != NULL; genv = (*genv).v_link)
    			;			/* find end of chain	     */
    	(*genv).v_link = savev;
    	}
    return (FAIL);
    }
#endif

/*	refgbl_do - perform REFGBL command.
 *	Create a reference to a global for this proc context.
 *	Assumes the global is not already ref'd by this proc.
 *	Ignore (after checking for presence of global) if from primary level.
 *
 *	If we're compiling a PDF, we build a skeleton variable in the 
 *	appropriate compilation symbol table, rather than creating an actual
 *	reference.
 */

    FUNCTION CODE refgbl_do (procctx, cmdctx)

    struct CONTXT	*procctx;		/* in/out: context of containing proc	*/
    struct CONTXT	*cmdctx;		/* in:  context from REFGBL cmd line	*/

    {
    IMPORT struct SYMTAB glbtab;
    IMPORT struct VARIABLE *allvar();

    struct SYMTAB	*destst;		/* destination sym tab (for comp'd PDFs*/
    struct SUBCMD	*s;
    struct VARIABLE	*tv;			/* target var (for comp'd PDFs)	*/
    struct VARIABLE	*nv;			/* NAME variable		*/
    COUNT		i;
    struct VARIABLE	*defd_var, *rv;

    nv   = lookex(&((*cmdctx).parmst), "NAME");	
    for (i=0; i < (*nv).v_count; i++)			/* for each global */
        {
	if ((*procctx).special == COMPILING)
	    {
	    if ((*procctx).subblk)			/* if in a SUBCMD block	*/
		{
		s = (*(*procctx).comp).cursub;		/* current SUBCMD table entry*/
		destst = &(*s).symtab;			/* sym tab for this SUBCMD blk*/
		}
	    else
		{
		if ((*procctx).subs)			/* if any SUBCMDs found yet*/
		    destst = &(*(*procctx).comp).after; /* after subs symbol table*/
		else
		    destst = &(*(*procctx).comp).before; /* before subs symbol table*/
		}
	    tv = allvar(destst);
	    s_copy(SVAL(*nv,i), (*tv).v_name);
	    (*tv).v_class = V_GLOBAL;
	    (*tv).v_type  = V_INTEGER;			/* placeholder		*/
	    (*tv).v_count = 0;
	    (*tv).v_cvp = NULL;
	    (*tv).v_dvp = NULL;
	    }
	else
	    {
	    defd_var = search(SVAL(*nv,i), procctx);
	    if (defd_var != NULL)
		{
		if ((*defd_var).v_class == V_GLOBAL  &&  (*defd_var).v_implicit)
		    continue;		/* if implicit, skip with no EM		*/
		else
		    {
		    tmmsg(PROCFAIL, "Variable '%s' previously defined or referenced.",
			"TAE-REFEX", SVAL(*nv,i));
		    return(DO_RETURN);
		    }
		}
	    rv = lookex (&glbtab, SVAL(*nv,i));
	    if (rv == NULL)
		{
		tmmsg(PROCFAIL, "Reference to undefined global variable '%s'.",
		    "TAE-UNDEFGBL", SVAL(*nv, i));
		return(DO_RETURN);
		}
	    if ((*procctx).numrefs >= MAXREF)
		{
		tmmsg (PROCFAIL, "Too many global references.", "TAE-REFS");
		return (DO_RETURN);
		}
	    
	    (*procctx).refs[(*procctx).numrefs] = rv;
	    (*procctx).numrefs++;
    	    (*rv).v_refcnt++;	  /* bump the ref count for this global	*/
	    }
        }
    return(DO_SUCCESS);
    }

/*	reserved - Check the name of a PARM GLOBAL or LOCAL to make 
 *	sure it's not reserved.
 *
 *	Return codes:
 *	
 *	TRUE - Name is one of the reserved words.
 *	FALSE - Name is not reserved.
 */

    FUNCTION CODE reserved (name)

    TEXT		name[];		/* in: candidate name		*/

    {
    static TEXT	*res_words [] = {"NOT",
    			     	"AND",
    				"OR",
    				NULL};
    CODE	i;


    for (i = 0; res_words[i] != NULL; i++)
	if (s_equal (name,res_words[i]))
	    return (TRUE);			/* A match was found */
    return (FAIL);				/* Not reserved */
    }

/*
 *	set_defaults.   Set non-NAME defaults (or INITIAL)
 *	into current value.
 */
    FUNCTION static CODE set_defaults(tv, dv, compiling)

    struct VARIABLE	*tv;		/* in/out: variable being defined	*/
    struct VARIABLE	*dv;		/* in: DEFAULT= variable		*/
    FUNINT		compiling;	/* in: TRUE if we're compiling a PDF	*/

    {
    IMPORT TEXT		*s_save();
    COUNT  i;
    struct VARIABLE 	*ref;
    CODE 		code;

    (*tv).v_default = TRUE;
    if ((*dv).v_type == V_NAME)		/* was the value de-ref'd?	*/
					/* (See notes in repval)	*/
        {
	ref = (*dv).v_ref;		/* use the reference		*/
	if (compiling  &&  (*ref).v_class == V_GLOBAL)
	    {
	    (*tv).v_deref = TRUE;
	    (*tv).v_cvp = s_save((*ref).v_name);
	    }
	else
	    {
	    code = chk_vector (tv, (*ref).v_type, (*ref).v_cvp,
			       (*ref).v_count, compiling);
	    if (code != SUCCESS)
		goto dp_dferr;
	    code = set_value (tv, (*ref).v_cvp, (*ref).v_count);
	    }
	}
    else				/* DEFAULT is directly valued	*/
        {
	if ((*dv).v_count == 0)	/* DEFAULT is null value		*/
	    {
	    if (!(*tv).v_nullable)
		goto dp_mcerr;
	    }
	else			/* DEFAULT is substantial		*/
	    if ((*dv).v_count < (*tv).v_minc  ||  (*dv).v_count > (*tv).v_maxc)
		goto dp_mcerr;
	(*tv).v_count = (*dv).v_count;
	if ((*tv).v_type == V_INTEGER)
	    {
	    for (i = 0; i < (*dv).v_count; i++)
		if (s_s2i(SVAL(*dv, i), &IVAL(*tv, i)) != SUCCESS)
		    goto dp_dferr;
	    }
	else if ((*tv).v_type == V_REAL)
	    {
	    for (i=0; i < (*dv).v_count; i++)
		if (s_s2r(SVAL(*dv, i), &RVAL(*tv, i)) != SUCCESS)
		    goto dp_dferr;
	    }
	else if ((*tv).v_type == V_STRING)
	    {
	    for (i = 0; i < (*dv).v_count; i++)
		SVAL(*tv, i)  = s_save(SVAL(*dv, i)) ;
	    }
	if (chk_vector(tv, (*tv).v_type, (*tv).v_cvp, (*tv).v_count,
		       compiling) != SUCCESS) 
	    goto dp_dferr;
	if (trans_valid(tv) != SUCCESS)	/* translate to valid form	*/
	    goto dp_dferr;
        }
    return(SUCCESS);

dp_mcerr:
dp_dferr:
    tmmsg(PROCFAIL, "Invalid DEFAULT or INITIAL specification for '%s'.",
	"TAE-DFTERR", (*tv).v_name);
    return(FAIL);
    }

/*
 *	set_p_quals - build parameter qualifier symbol table.
 *
 *	Uses prccmd to open proc containing parameter qualifiers and build
 *	the qualifier symbol table.  The GET_PARM_QUAL value for the
 *	special flag causes prccmd to:
 *
 *	- stop processing the proc at the BODY statement if the proc is a
 *	  procedure.
 *	- at the close of processing of qual_proc, move qual_proc's parameter
 *	  symbol table to the qualst member of the last variable in
 *	  "procctx"'s parmst chain.
 *
 */

    FUNCTION static CODE set_p_quals (procctx, qual_proc, class)

    struct CONTXT	*procctx;	/* in: proc context -- rcvs parm quals	*/
    TEXT		qual_proc[];	/* in: specifier for the qualifier proc	*/
    CODE		class;		/* in: V_PARM, V_LOCAL, etc. */

    {
    CODE		saved_special;
    BOOL		saved_parmqual;
    CODE		code;

    if (class == V_GLOBAL)
        return (FAIL); 
    saved_special = (*procctx).special;
    (*procctx).special = GET_PARM_QUAL;	/* flag to prccmd -- we're getting...	*/
					/* qualifiers; don't execute	*/
    saved_parmqual = (*procctx).parmqual;
    (*procctx).parmqual = (class == V_PARM);
    code = prccmd(qual_proc, procctx);	/* otherwise, process like any command	*/
    (*procctx).special = saved_special;
    (*procctx).parmqual = saved_parmqual;
    if (code == DO_SUCCESS)
	code = SUCCESS;
    else
	code = FAIL;
    return(code);
    }

