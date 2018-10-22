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



/*	TAE Plus PAR file Upgrade (parupgrade utility) */

/****************

	This converts from pre-5.2 TAE PAR file
	format ( blksiz is COMPACT_UCOUNT) to
	the "big" format ( blksiz is LONG ).
	Resource files are handled in a shell script call resupgrade.

	Beware: if the input files do not have a directory
	specification, then the conversion is "in-place"
	and there's no way to get back to the old format
	(unless you make a PDF with $TAEBIN/par2pdf
	and then create a PAR file using an old TM).

CHANGE LOG:
    01-may-92 Created...tpl
    14-may-92 Upgraded to update all ".fg", ".bg" and ".font" v_size 
              to 132...krw
    25-may-93 $TAEBIN/$TAEPLAT changed to just $TAEBIN...kbs
    22-jul-93 Vm_Free() declaration does not match vmcoproto.h declaration...rt
****************/

#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include	"vminc.inc"
#include	"oldparblk.inc"
#include	"oldvminc.inc"

#include "taeintproto.h"


FUNCTION VOID ConvertParFile (TEXT *, TEXT *);
FUNCTION VOID ReadAndConvertVm (
    struct  OLD_VM_STRUCT   *oldvm,     /* in: existing vm          */
    struct  VM_STRUCT       *newvm,         /* in: existing vm          */
    struct SFILE            *f             /* in: opened file context  */
);
FUNCTION Id ConvertParBlock ( Id    oldvmid, TEXT *filespec);
FUNCTION VOID err_handler (

    FUNINT      mode,                   /* SUCCESS or FAIL */
    TEXT        message[],              /* in: message text             */
    uintptr_t   A1,	             /* in: integer or string ptrs   */
    uintptr_t   A2,
    uintptr_t   A3
);

FUNCTION CODE extractFileName (TEXT *spec, TEXT *name);
FUNCTION GENPTR Old_Vm_New(FUNINT);
FUNCTION VOID Old_Vm_Free(GENPTR);
FUNCTION VOID oldmakeabs(struct SYMTAB *, ALIGN *);
FUNCTION TEXT *s_substring(TEXT *, TEXT *);


FUNCTION int main (
int	argc,
TEXT	*argv[]
)

{
TEXT	outputFile[STRINGSIZ+1];
COUNT	lines, cols;
CODE	ttype;
BOOL    verbose=FALSE;

if ( argc < 2 ) 
    {
    printf ("A file name must be specified\n");
    exit (1);
    }
t_pinit (&lines, &cols, &ttype);
f_force_lower (FALSE);				/* take file names literal */

/* loop thru each one */

if (s_equal ("-v", argv[1]))                    /* not really used anymore */
   {
   if ( argc < 3 ) 
	{
    	printf ("A file name must be specified\n");
    	exit (1);
    	}

   verbose = TRUE;
   argv++;                                      /* files start at argv[2] */
   }

for (argv++; *argv != NULL; argv++)
    {
    extractFileName (*argv, outputFile);
    if (verbose)
#ifdef VMS
        printf ("  Converting %s to []%s%s.\n",  *argv, outputFile, ".par");
#else
        printf ("  Converting %s to ./%s%s.\n",  *argv, outputFile, ".par");
#endif
    ConvertParFile (*argv, outputFile);
    }
exit (0);					/* normal termination */
return(0);
}


FUNCTION VOID ConvertParFile (

TEXT	*old,		/* old par file spce	*/
TEXT	*new		/* new par file spec	*/
)
{
Id	oldVmId;
Id	newVmId;

oldVmId = (Id) Old_Vm_New (P_ABORT);		/* alloc an old vm id */
/* 
 * read the file and convert it
 */
newVmId = ConvertParBlock(oldVmId, old);
Vm_WriteToDisk (newVmId,  new);
Old_Vm_Free (oldVmId);
Vm_Free ((struct VM_STRUCT *)newVmId);
}


FUNCTION CODE extractFileName (

TEXT	*spec,
TEXT 	*name
)
{
struct FSBLOCK	fsblock;
TEXT	errmsg[STRINGSIZ+1];
CODE	code;

/*
 * get the full file name
 */
name[0] = EOS;
code = f_crack (spec, "", "", "", &fsblock, errmsg);	
if (code != SUCCESS)
    return (code);
s_copy (fsblock.name, name);
return (SUCCESS);
}

/*
 * convert an old par block to a new par block
 */
FUNCTION Id ConvertParBlock (
Id    oldvmid,
TEXT *filespec
)
{

    struct  SFILE         f;              /* SFILE structure              */
    struct  OLDPARHDR     oldph;          /* old p-file header record         */
    struct  OLD_VM_STRUCT *oldvm;
    struct  OLD_NP_PARBLK *oldnp;
    CODE                  code;
    LONG		  recsize;
    struct  VM_STRUCT     *newvm;
    Id	    		  newVmId;

    oldvm = (struct OLD_VM_STRUCT *)oldvmid;
    oldnp = &( (*oldvm).npblk );

    newVmId = (Id) Vm_New (0);
    newvm = (struct VM_STRUCT *) newVmId;

    /* open the file */
    code = f_opnspc(&f, 1, filespec, "", "", PAR_TYPE, F_READ);
    (*oldnp).hostcode = (code == SUCCESS) ? code : f.host_code;
    if (code != SUCCESS)
        goto bad_open;
    /* read in old header */
    code = f_bread( &f, (GENPTR)&oldph, sizeof(struct OLDPARHDR), (COUNT *)&recsize);

    (*oldnp).hostcode = (code == SUCCESS) ? code : f.host_code;/* save error */
    if (s_equal (oldph.sentinel, OLD_P_SENTINEL))
        goto old_format;
    if (s_equal(oldph.sentinel, P_BIGSENTINEL))
       goto good_format;
    if (!s_equal(oldph.sentinel, P_SENTINEL))
       goto bad_format;
    if ( code != SUCCESS )
	goto bad_read;

/*
 *  read and convert each par block
 */
    ReadAndConvertVm (oldvm, newvm, &f);          

    f_close (&f, F_KEEP);
    return ( newVmId );

bad_open:
    err_handler(FAIL, "Unable to open parameter file '%s'.  %s.",
            (uintptr_t)filespec, (uintptr_t)f.errmsg, 0);

bad_read:
    f_close(&f, F_KEEP);
    err_handler (FAIL, "Error reading parameter file '%s'.  %s.",
            (uintptr_t)filespec, (uintptr_t)f.errmsg,0);

old_format:
    f_close(&f, F_KEEP);
    err_handler (FAIL,
#ifdef UNIX
           "File '%s' has obsolete format.  Must use $TAEBIN/parconvert first.",
#else
           "File '%s' has obsolete format.  Must use tae$lib:parconvert first.",
#endif
           (uintptr_t)filespec,0,0);

bad_format:
    f_close(&f, F_KEEP);
    err_handler (FAIL, "Parameter file '%s' is not correctly formatted.", (uintptr_t)filespec,0,0 );

good_format:
    f_close(&f, F_KEEP);
    err_handler (SUCCESS, 
   "Parameter file '%s' is correctly formatted, no conversion is needed.",
          (uintptr_t)filespec,0,0);
    return(0);
    }

FUNCTION VOID ReadAndConvertVm (
    struct  OLD_VM_STRUCT   *oldvm,     /* in: existing vm          */
    struct  VM_STRUCT       *newvm,         /* in: existing vm          */
    struct SFILE            *f             /* in: opened file context  */
)

    {
    struct      NP_PARBLK     newp;
    struct      OLD_LARGE_PARBLK oldp;
    COUNT                        recsize;
    CODE                         code;
    struct VARIABLE             *vp, *v, *last_var;
    BOOL	aView = FALSE;

/*
 *  loop till the end
 */
    while (FOREVER)
        {                                       /* read each record     */
        code = f_bread(f, (GENPTR) &oldp, sizeof(oldp), &recsize);
        if (code == F_EOF)
            return;
        else if (code != SUCCESS)
            goto bad_read;
        oldmakeabs(&oldp.symtab, oldp.pool);    /* make pointers absolute */

/*
 *	copy some of the header info 
 */
 	newp = newvm->npblk;
	newp.msgtyp = oldp.msgtyp;
	newp.actmod = oldp.actmod;
	newp.vers   = oldp.vers;
	newp.extra1 = oldp.extra1;
	newp.extra2 = oldp.extra2;
	newp.hostcode = oldp.hostcode;
	newp.mode   = oldp.mode;
	newp.ctxptr = oldp.ctxptr;

/*
 * 	are we dealing with a TAE Plus View?
 */
	aView = (s_substring("_v", f->full_spec) != 0);
	
        /* find last variable in the new vm object */

        for (last_var=(struct VARIABLE *)&(*newvm).npblk.symtab;
                          (*last_var).v_link != NULL;
                          last_var = (*last_var).v_link )
                ;
        for (vp=oldp.symtab.link; vp != NULL; vp = (*vp).v_link)
            {
            v = Vm_AllocVar (&(*newvm).npblk.symtab);
            if (Vm_SpCopyVar(vp, v) == FAIL)    /* copy the variable        */
		    {
		    printf ("Fail to copy var\n");
                    return /*(FAIL)*/;
		    }

	/* if aView, look for "fg", "bg" and "font" quals */
	/* this is a view and v has quals */

	    if (aView && ((*v).v_qualst.link != 0))
		{
	 	struct VARIABLE *qual;
		for (qual = (*v).v_qualst.link; qual; qual = (*qual).v_link)
		    {
		    if ((*qual).v_type == V_STRING &&
			(*qual).v_size != STRINGSIZ &&
			(s_equal((*qual).v_name, "fg") ||
			s_equal((*qual).v_name, "bg") ||
			s_equal((*qual).v_name, "font")))
			{
			(*qual).v_size = STRINGSIZ;
			}
		    }	
		}

            last_var = (*last_var).v_link = v;  /* link into the symtab */
            }
        if (oldp.last)                             /* last record for this vm */
            return;
       }

bad_read:
    err_handler (FAIL, "Error reading parameter file '%s'.  %s.",
           (uintptr_t)(*f).full_spec, (uintptr_t)(*f).errmsg,0);
    return;

    }


/*
 *      err_handler -   report error and exit.
 *
 */

    FUNCTION VOID err_handler (

    FUNINT      mode,                   /* SUCCESS or FAIL */
    TEXT        message[],              /* in: message text             */
    uintptr_t   A1,	             /* in: integer or string ptrs   */
    uintptr_t   A2,
    uintptr_t   A3
    )
    {

    TEXT        msgstring[2*STRINGSIZ+1];   /* formatted message string */

    sprintf(msgstring, message, A1, A2, A3);
    if (s_length(msgstring) > STRINGSIZ)     /* allow upto STRINGSIZ  */
        s_copy("...", &msgstring[STRINGSIZ-3]);     /* truncate */
    printf("\n");
    printf(msgstring);
    if ( mode == FAIL )
       printf("\nConversion terminated prematurely.\n");
    exit(1);
    }

