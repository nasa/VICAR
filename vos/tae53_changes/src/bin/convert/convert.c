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



/*	TAE Plus PAR and RES file conversion 	*/

/****************

	This converts from the traditional TAE PAR file
	format (MAXVAL<=100, VALIDSIZ=24, etc.) into
	the "modern" format (MAXVAL > 128, VALIDSIZ=80).
	Resource files are also handled.

	Beware: if the input files do not have a directory
	specification, then the conversion is "in-place"
	and there's no way to get back to the old format
	(unless you make a PDF with $TWB/tools/generate
	and then create a PAR file using an old TM).

CHANGE LOG:
28-jun-89 	allow uppercase in file names...palm 	
29-sep-89	declare vm_new()'s and vm_free()'s...ljn
18-jul-93	Remove declarations of Vm_New()'s and Vm_Free()'s...kbs
****************/

#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include	"vminc.inc"

#include "taeintproto.h"

Id		convertVm(Id);
FUNCTION struct VARIABLE *allvar(
    struct SYMTAB *head		/* in/out: pointer to symbol table header */
    );

FUNCTION VOID x_error(
    FUNINT	mode,			/* in: P_ABORT or P_CONT	*/
    TEXT	message[],		/* in: message text		*/
    TEXT	key[],			/* in: message key 		*/
    uintptr_t 	A1,			/* in: integer or string ptrs 	*/
    uintptr_t	A2,
    uintptr_t	A3
    );

FUNCTION VOID convertPar (
	TEXT	*old,		/* old par file spce	*/
	TEXT	*new		/* new par file spec	*/
);
FUNCTION VOID convertRes (
	TEXT	*old,		/* old res file spce	*/
	TEXT	*new		/* new res file spec	*/
);
FUNCTION CODE extractFileName (
	TEXT	*spec,
	TEXT 	*name
);


FUNCTION int main (
	
	int	argc,
	TEXT	*argv[]
)

{
VOID	(*converter) (TEXT *,TEXT *);
TEXT	executable[STRINGSIZ+1];
TEXT	outputFile[STRINGSIZ+1];
int	verbose = TRUE;			/* -v not needed for now */
TEXT	*type;
COUNT	lines, cols;
CODE	ttype;

t_pinit (&lines, &cols, &ttype);
f_force_lower (FALSE);				/* take file names literal */
extractFileName (argv[0], executable);
converter = s_equal (executable, "parconvert") ? convertPar : convertRes;
type = s_equal (executable, "parconvert") ? ".par" : ".res" ;
if (argv[1] == NULL || s_equal(executable, "convert") ||
    s_lseq ("-h", argv[1]) || s_equal ("?", argv[1]))
    {
    printf ("usage: parconvert file, ...\n");
    printf ("usage: resconvert file, ...\n");
    printf ("	converts old PAR and RES files to new format\n");
    exit (1);
    }
else if (s_equal ("-v", argv[1]))		/* not really used anymore */
   {
   verbose = TRUE;
   argv ++;					/* files start at argv[2] */
   }
for (argv++; *argv != NULL; argv++)
    {
    extractFileName (*argv, outputFile);
    if (verbose)
#ifdef VMS
	printf ("  Converting %s to []%s%s.",  *argv, outputFile, type); 
#else
	printf ("  Converting %s to ./%s%s.",  *argv, outputFile, type); 
#endif
    (*converter) (*argv, outputFile);
    printf ("\n");
    }
exit (0);					/* normal termination */
}


FUNCTION GENPTR old_Vm_New(FUNINT);
FUNCTION VOID old_Vm_Free(struct VM_STRUCT *);
FUNCTION CODE old_Vm_ReadFromDisk(GENPTR, TEXT[]);

FUNCTION VOID convertPar (
	TEXT	*old,		/* old par file spce	*/
	TEXT	*new		/* new par file spec	*/
)
{
Id	oldVmId;
Id	newVmId;

oldVmId = (Id) old_Vm_New (P_ABORT);
old_Vm_ReadFromDisk (oldVmId, old);
newVmId = convertVm (oldVmId);
Vm_WriteToDisk (newVmId,  new);
old_Vm_Free ((struct VM_STRUCT *)oldVmId);
Vm_Free ((struct VM_STRUCT *)newVmId);
}



static COUNT ParCol; 		/* column number of component Par names */


FUNCTION CODE convertAddVm (
	Id	oldVmId,
	Id	collection,
	TEXT	name[]
);
FUNCTION Id old_Co_New(CODE);
FUNCTION VOID old_Co_Free(struct COLLECTION *, VOID (*)(Id, CODE));
FUNCTION CODE old_Co_ReadFile(Id, TEXT *, CODE);
typedef CODE (*coFunction)(Id, COUNT *, TEXT *, CODE);
FUNCTION CODE old_Co_ForEach(struct COLLECTION *, coFunction, GENPTR);

FUNCTION VOID convertRes (
	TEXT	*old,		/* old res file spce	*/
	TEXT	*new		/* new res file spec	*/
)
{
Id	oldCollection, newCollection;

oldCollection = (Id) old_Co_New (0);
old_Co_ReadFile (oldCollection, old, P_ABORT);
newCollection = (Id) Co_New (0);

/*	For each Vm in the old collection, convert and add to new collection */

ParCol = 10;			/* current column number (approx) */
printf ("\n    ");		/* new line and indent a little	 */
old_Co_ForEach ((struct COLLECTION *)oldCollection, (coFunction)convertAddVm, newCollection); 
Co_WriteFile ((struct COLLECTION *)newCollection, new);

/*	clean up   */

old_Co_Free ((struct COLLECTION *)oldCollection, (void (*)(char *,CODE))old_Vm_Free);
Co_Free ((struct COLLECTION *)newCollection, (void (*)(char *,CODE))Vm_Free);
}




FUNCTION Id convertVm (Id oldVmId);

/*	ForEach callback: convert a Vm object and add to a collection 	*/

FUNCTION CODE convertAddVm (

	Id	oldVmId,
	Id	collection,
	TEXT	name[]
)
{
Id	newVm;

if (ParCol > 60)		/* walk across the screen	*/
    {
    printf ("\n    ");		/* new line and indent a little	 */
    ParCol = 10;
    }
printf ("%s ", name);		/* display par file name */
ParCol += s_length(name) + 1;
newVm = convertVm (oldVmId);
Co_Add ((struct COLLECTION *)collection, newVm, name, 0);
return (0);
}

struct OLD_VARIABLE;
FUNCTION CODE convertVariable (
	struct OLD_VARIABLE  *oldV,
	struct SYMTAB 	     *newSymtab
);
FUNCTION CODE old_Vm_ForEach(Id, CODE (*)(struct VARIABLE *, GENPTR), GENPTR);


/*	convert a Vm object		*/

FUNCTION Id convertVm (Id oldVmId)
{
Id	newVmId;
struct VM_STRUCT  *vm;

newVmId = (Id) Vm_New (0);
vm = (struct VM_STRUCT *) newVmId;
old_Vm_ForEach (oldVmId, (CODE (*)(struct VARIABLE *,char*))convertVariable,  (GENPTR)&(*vm).npblk.symtab);
return (newVmId);
}


/*	the following allow the old definitions to live 
	in the same compilation with the new definitins
*/

#undef I_SYMTAB
#define VARIABLE OLD_VARIABLE
#define SYMTAB   OLD_SYMTAB
#define VV_INTEGER VVI
#define VV_FLOAT	VVF
#define VV_STRING  VVS
#define DEFPDF		OLD_DEFPDF
#define I_VALID		OLD_I_VALID
#define I_RANGE		OLD_I_RANGE
#define R_VALID		OLD_R_VALID
#define R_RANGE		OLD_R_RANGE
#define S_VALID  	OLD_S_VALID
#define S_RANGE  	OLD_S_RANGE
#define VALID_STRING	OLD_VALID_STRING
#define ES_VALID	OLD_ES_VALID


#undef   VALIDSIZ			/* avoid compilation error from...*/
#undef   NAMESIZ
#define  NAMESIZ 		8	/* old value */

#include "oldsymtab.inc"


#undef	VARIABLE
#undef  SYMTAB
#undef  S_VALID


/*	convert a VARIABLE from old to new format    */


FUNCTION CODE convertVariable (
	struct OLD_VARIABLE  *oldV,
	struct SYMTAB 	     *newSymtab
)
{
struct VARIABLE *newV;
struct OLD_VARIABLE *q;

newV = allvar (newSymtab);			/* allocate/link zero'd block */
s_copy ((*oldV).v_name, (*newV).v_name);
(*newV).v_type = (*oldV).v_type;
(*newV).v_class = (*oldV).v_class;

/*	here, we assume directly valued (i.e., not V_NAME)	*/

(*newV).v_keyword = (*oldV).v_keyword;
if ((*newV).v_class == V_PARM)
    {
    (*newV).v_iparm = (*oldV).v_iparm;
    (*newV).v_default = (*oldV).v_default;
    (*newV).v_page = (*oldV).v_page;
    }
(*newV).v_pv12 = TRUE;
(*newV).v_count = (*oldV).v_count;
(*newV).v_minc = (*oldV).v_minc;
(*newV).v_maxc = (*oldV).v_maxc	;
(*newV).v_size = (*oldV).v_size;
(*newV).v_filemode = (*oldV).v_filemode;
(*newV).v_nullable = (*oldV).v_nullable;
(*newV).v_file = (*oldV).v_file;
(*newV).v_event = (*oldV).v_event;
(*newV).v_cvp = (*oldV).v_cvp;		/* double ref the vv -- see below */
(*newV).v_dcount = (*oldV).v_dcount;

(*oldV).v_count = 0;			/* and avoid bad free !     */
(*oldV).v_cvp   = NULL;			/* ditto		    */

/*	handle qualifiers	*/

for (q=(*oldV).v_qualst.link;  q != NULL; q = (*q).v_link)
    convertVariable (q, &(*newV).v_qualst);

/*	handle new valid format 	*/

if ((*oldV).v_type == V_STRING  &&  (*oldV).v_valid)
    {
    struct OLD_S_VALID *oldValid;
    struct S_VALID     *newValid;
    COUNT		i;

    oldValid = (struct OLD_S_VALID *) (*oldV).v_valid;
    newValid = (struct S_VALID *) 
	tae_alloc (1, Vm_ValidSize((*oldV).v_type,  (*oldValid).count));
    (*newV).v_valid =  (GENPTR) newValid;
    for (i=0; i < (*oldValid).count; i++)
	(*newValid).slist[i].string = s_save ((*oldValid).slist[i].string);
    (*newValid).count = (*oldValid).count;
    }
return (0);
}


/*
 *    allvar.   Build VARIABLE structure.
 *
 *    Allocates structure and places it at end of symbol table chain. 
 *
 */

    FUNCTION struct VARIABLE *allvar(
    struct SYMTAB *head		/* in/out: pointer to symbol table header */
    )

    {
    FAST struct VARIABLE *p;	/* pointer to allocated structure*/
    FAST struct VARIABLE *pc;	/* current pointer		 */


    p = (struct VARIABLE *) tae_alloc(1, sizeof(struct VARIABLE));
    if (p==NULL)
        return(NULL);
    zero_block((GENPTR) p, sizeof (struct VARIABLE));
    (*p).v_minc = 1;			/* default mincount	*/
    for (pc=(struct VARIABLE *) head; (*pc).v_link != NULL; pc=(*pc).v_link)
	;				/* find end of chain ...*/
    (*pc).v_link = p;			/* link in new struc	*/
    return(p);
    }




FUNCTION CODE extractFileName (
	TEXT	*spec,
	TEXT 	*name
)
{
struct FSBLOCK	fsblock;
TEXT	errmsg[STRINGSIZ+1];
CODE	code;

name[0] = EOS;
code = f_crack (spec, "", "", "", &fsblock, errmsg);	
if (code != SUCCESS)
    return (code);
s_copy (fsblock.name, name);
return (SUCCESS);
}


/* 
 *	x_error.   report error and abort.
 *
 */

    FUNCTION VOID x_error(

    FUNINT	mode,			/* in: P_ABORT or P_CONT	*/
    TEXT	message[],		/* in: message text		*/
    TEXT	key[],			/* in: message key 		*/
    uintptr_t 	A1,			/* in: integer or string ptrs 	*/
    uintptr_t	A2,
    uintptr_t	A3
    )
    {
   
    TEXT	msgstring[2*STRINGSIZ+1];   /* formatted message string	*/

            sprintf(msgstring, message, A1, A2, A3);	
            if (s_length(msgstring) > STRINGSIZ)     /* allow upto STRINGSIZ  */
	        s_copy("...", &msgstring[STRINGSIZ-3]);	    /* truncate */
  	    printf("\n");
	    printf(msgstring);
  	    printf("\nConversion terminating prematurely.\n");
	    exit(1);
    }
