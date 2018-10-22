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

//static char collection_cVersion[] = "@(#)collection.c	33.1 8/26/94";

/*	Collection package.
 *
 *	A collection may hold any kind of object.   A collection
 *	is a set of members, each member being a pointer
 *	to some object.  You create a collection with Co_New;
 *	add members with Co_Add; and access members with
 *	Co_Find or Co_ForEach.   Co_Free deletes the collection.
 * 
 *	12-dec-87	Add Co_ReadFile...palm
 *	24-mar-88	Add qualified names ...palm
 *	24-mar-88	Fix Co_ForEach when last entry is deleted..palm
 *	27-apr-88	Allow input file to be UNIX archive file...palm
 *	02-may-88	Write UNIX archive for output...palm
 *	03-sep-88	Fix call to gettimeofday...palm
 *	02-oct-88	Change protection for archive members to 644...palm
 *	10-nov-88	SYSV #include modification...ljn
 *	25-nov-88	For archives, use the member name in the collection
 *			rather than the PARHDR name as done before...palm
 *	01-dec-88	Fix HP varying format for archive file...palm
 *	02-dec-88	Co_Free_Nocall...nci
 *	01-feb-89	Missing #endif...ljn
 *			More...palm
 *	08-feb-89	New check for old resource format...palm
 *	17-feb-89	Fix obsolete error message for $TLIB...palm
 *	18-sep-89	VMS doesn't create .res files in archive format...ljn
 *	23-oct-89	Incorrect OLDRESFILE error message...ljn
 *      06-nov-89       return NULL in Co_Remove...tpl
 *	07-feb-89	Apollo archive files need special treatment...ljn
 *	25-apr-91	New VMS TAE logicals...ljn
 *	06-may-92       Used new P_BIGSENTINEL...tpl
 *	22-jul-92	PR1519: Label Co_Empty, Co_Count, Co_IdVector,
 *			and Co_NameVector as UNSUPPORTED...kbs
 *	05-oct-92	IBM AIX support...krw/rt
 *	06-oct-92	SGI needs BSD time header file...bth/rt
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *	25-may-93	$TAEBIN/all reference changed to just $TAEBIN...kbs
 *	09-jul-93	Use of vmcoproto.h required some changes...rt
 *	15-aug-93	PR2181. For demo version, limit # of panels.  
 *			Also add definition of IsWorkBench global...ljn, cew
 *	01-oct-93	PR2356 added a space between "an" "application" in
 *			the Demo panel limit message...krw
 *	07-dec-93	Solaris port (backported dag from aug-93)...cob
 *	10-may-94	SCO port...swd
 *	11-jul-94	Intergraph port:  Need <sys/time.h>, not <time.h>...dag
 */

#include	"taeconf.inp"
#include	"parblk.inc"
#include	"fileinc.inp"
#include <stdio.h>
#include "taeintproto.h"
#ifdef AIX
#include	<sys/types.h>			/* for pid and gid */
#define         AR_SENTINEL     "<aiaff>"       /* without newline */
#else
#define		AR_SENTINEL     "!<arch>"	/* without newline */
#endif

#ifdef UNIX
#if defined(sco)
#define PORTAR
#endif
#include 	<ar.h>
#ifdef macII
#include 	<sys/time.h>
#else
#if defined(SYSV) && !defined(sgi) && !(defined(sun) && OSMajorVersion >= 5) && !(defined(sco)) && !(defined(__clipper__)) && !defined(__linux) && !defined(linux)
#include	<time.h>
#else /* SYSV */
#include	<sys/time.h>
#endif /* SYSV */
#endif /* macII */
#endif /* UNIX */

#ifdef VMS
#define RESCONVERT $TAEPDF:resconvert
#else
#define RESCONVERT $TAEPDF/resconvert
#endif

GLOBAL BOOL IsWorkBench = FALSE;  	/* Indicates that WB is current app */

#define C_VM	      0xaaaa		/* type of Vm object	  */
#define C_COLLECTION  0xcccc		/* type of collect object */

#ifndef	testmain

/*	Each object in the collection gets a "container"
 *	with the following structure:
 */

struct LINK;
struct LINK 
    {
    struct LINK *next;			/* next in collection*/
    TEXT	*name;			/* name of object    */
    CODE	type;			/* type of object    */
    Id		object;			/* pointer to object */
    };


#ifdef AIX
/* Static data required by the AIX archive structure. */
/* Arrays are allocated in Co_WriteFile */
/* and updated in writeVmToFile */
static    TEXT 		*memOffsets;
static    TEXT 		**memNames;
static    COUNT		members;
static    COUNT		curMemberIndex;
static    COUNT		memTableSize;
static	  long		nxtmem;		/* next member offset */
static	  long		prvmem;		/* prevous member offset */
#endif

FUNCTION static void* writeVmToFile (Id vm, GENPTR fin, TEXT* name, CODE type);


FUNCTION Id Co_New (flags)

    CODE flags;			/* C_DUPLICATES, C_ALPHA	*/

    {
    struct COLLECTION *c;

    c = (struct COLLECTION *) tae_alloc (1, sizeof(struct COLLECTION));
    (*c).flags = flags;
    (*c).last = NULL;	
    (*c).count = 0;
    return ((Id) c);
    }

/*	Co_Add.   add member to collection.
 * 
 *	TBD: Currently C_DUPLICATES and C_ALPHA are not honored.
 *	
 *	Currently: we add only to end of the list.
 */

FUNCTION void Co_Add 
(
 struct COLLECTION 	*c, 	/* Id of a collection 	*/
 Id		object,		/* pointer to an object	*/
 TEXT	name[],		/* name of object	*/
 CODE	type		/* type of object	*/
 )
    {
    struct LINK 	*last;
    struct LINK		*newLink;

    last = (*c).last;
    newLink = (struct LINK *) tae_alloc (1, sizeof (struct LINK));
    if (last == NULL)			/* if collection empty: */
        (*newLink).next = newLink;	/* link to self		*/
    else
	{
	(*newLink).next = (*last).next;	
        (*last).next = newLink;
	}
    (*c).last = newLink;		
    (*newLink).name = s_save (name);
    (*newLink).object = object; 
    (*newLink).type = type;
    (*c).count ++;
    }    


FUNCTION VOID Co_Free 
(
    struct COLLECTION *c,			/* id of collection     */
    VOID	      (*deleteFunction)(Id Obj,CODE Type)	/* for deleting each	*/
)
    {
    struct LINK *last;
    struct LINK	*current;
    struct LINK *next;

    last = (*c).last;    
    if (last != NULL)
        {
        current = (*last).next;   		/* start at begin	*/
        while (1) 
	    {
     	    next = (*current).next;		/* capture next		*/
	    if (deleteFunction != NULL)
		(*deleteFunction) ((*current).object, (*current).type);
	    s_free ((*current).name);
	    tae_free (current);	    		/* free LINK struct	*/
     	    if (current == last)
    		break;
            current = next;
	    }    
        }
   tae_free (c);				/* free context block	*/
    }    

/*
 *  Co_Free_Nocall - Like Co_Free but no freefunction parameter.
 */

ADA_FUNCTION VOID Co_Free_Nocall 
(
    struct COLLECTION *c			/* IN: ptr to collection */
 )
    {
    Co_Free (c, NULL);
    }


/*	Co_Empty.   TRUE if collection is empty.
 *
 */

UNSUPPORTED BOOL Co_Empty 
(

    struct COLLECTION *c
)	
    {
    return ((*c).last == NULL);
    }


/*	Co_ForEach.   Call a function for each member object.
 *
 *	The calling stops prematurely if aFunction returns
 *	something other than NULL.
 *
 *	The return code from Co_ForEach is the last return code
 *	from the aFunction.
 *
 *	The logic here is coded so that the aFunction can
 *	Co_Remove the object from the collection and the
 *	callbacks continue ok.
 */
	
FUNCTION void * Co_ForEach 
(
	
 struct COLLECTION	*c,			/* collection id	*/
 void *    		(*aFunction) (Id Obj, GENPTR context, TEXT* name, CODE type),	/* function to call	*/
 GENPTR		context		/* arg to pass		*/
)
    {
    struct LINK		*last;
    struct LINK 	*current;
    struct LINK 	*next;
    void *		code;

    last = (*c).last;
    code = 0;
    if (last != NULL)
        {
	current = (*last).next;   
        while (1)  
	    {
	    next = (*current).next; 		
	    code = (*aFunction) ((*current).object, 
    				 context, 
    				 (*current).name,
    				 (*current).type);
	    if (code != 0 || (*c).last == NULL || current == (*c).last)
		break;
    	    current = next;
	    }
        }
    return (code);
    }



static void* countEntries 
(

 Id	object,
 GENPTR	count,
 TEXT	*name,
 CODE	type
 )
{
(*((COUNT *)count))++;
return (0);
} 


UNSUPPORTED COUNT Co_Count (struct COLLECTION *c)		/* return number of entries */
{
COUNT	count;
count = 0;
Co_ForEach (c, countEntries, (GENPTR) &count);
return (count); 
}


/*	Callback to build pointer vector:	*/

FUNCTION static void* buildIdVector 
(
 Id	aMember,
 GENPTR vectorin,
 TEXT	*name,
 CODE type
 )
{
  Id **vector = (Id**) vectorin;
  *(*vector) = aMember;
  (*vector)++;
  return (0);
}

typedef TEXT *Name;

/*	Callback to build name vector	*/

FUNCTION static void* buildNameVector 
(
 Id	aMember,
 GENPTR vectorin,
 TEXT	*name,
 CODE   type
 )
{
  Name **vector = (Name**) vectorin;
  *(*vector) = name;
  (*vector) ++;
  return (0);
}

/*	Co_IdVector.   Build a vector of Ids
 *	one component for each object in the list.
 *
 *	Caller must de-allocate the vector.
 */

UNSUPPORTED Id * Co_IdVector 
(
	
    struct COLLECTION	*c			/* collection id	*/
 )
    {
    Id			*vector, *vector0;
    COUNT		count;

    count = Co_Count (c);
    vector0 = (Id *) tae_alloc (1, (count+1) * sizeof (Id *));
    vector = vector0;
    Co_ForEach (c, buildIdVector, (GENPTR) &vector);
    *vector = NULL;				/* null terminate	*/
    return (vector0);
    }


UNSUPPORTED Name *Co_NameVector 
(
	
    struct COLLECTION	*c			/* collection id	*/
)
    {
    Name		*vector, *vector0;
    COUNT		count;

    count = Co_Count (c);
    vector0 = (Name *) tae_alloc (1, (count+1) * sizeof (Name *));
    vector = vector0;
    Co_ForEach (c, buildNameVector, (GENPTR) &vector);
    *vector = NULL;				/* null terminate	*/
    return (vector0);
    }


/*	Co_Remove.   
 *
 *	Returns NULL if member not found; non-NULL return is
 *	the Id of the removed object.
 *
 *	NOTE: we do not delete the object, merely its membership
 *	in the collection.  The caller may not want to delete
 *	the object--an object may be a member of many collections.
 */

    FUNCTION Id Co_Remove 
(

 struct COLLECTION	*c,		/* a collection 	*/
 TEXT			name[]		/* member to delete	*/
)
    {
    struct LINK		*current, *previous;
    Id			object;

    if ((*c).last == NULL)
	return (NULL);
    previous = (*c).last;			/* start at tail	*/
    current = (*previous).next;			/* start at head	*/
    while (1)
        {
	if (s_equal (name, (*current).name))
	    {	
	    if (current == (*c).last)
		(*c).last = ((*current).next == current) ? NULL : previous;
	    (*previous).next = (*current).next;	/* unlink from list     */
	    object = (*current).object;		/* Id of removed obj 	*/
	    tae_free ((*current).name);		
	    tae_free (current);
	    (*c).count --;
	    return (object);	    		/* removed successfully */
	    }
	if (current == (*c).last)	
	    return (NULL); 			/* not found		*/
	previous = current;
	current = (*current).next;
	}
    }


/*	Co_Find.   Find object by name.
 *
 *	This handles composite names, i.e. when a collection is
 *	a collection of collections.
 *
 *	For example, if given "A.B", Co_Find
 *	would find the the collection object "A" and then find the 
 *	object "B" within "A".   If somewhere along the line,
 *	the object type is C_VM, then we call Vm_Find to
 *	find the rest of the name.
 *
 */

struct OBJECT_INFO	/* for comm to searchFunction thru Co_ForEach  */
    {
    CODE	type;			/* output of searchFunction */
    TEXT	*name;			/* input to searchFunction  */
    };

FUNCTION static void* searchFunction (Id id, GENPTR objectInfo, 
				      TEXT *objectName, CODE type);

FUNCTION Id Co_Find 
(

 struct COLLECTION	*c,		/* in: collection id	*/
    TEXT		name[]		/* in: name to find	*/
 )
    {
    TEXT		loc_name[STRINGSIZ+1];    
    void *		object;
    struct OBJECT_INFO	objectInfo;		/* context block	*/
    COUNT		period;
    
    s_bcopy (name, loc_name, STRINGSIZ);	/* local copy 		*/
    period = s_index (loc_name, '.');		/* composite name?	*/
    if (period >= 0)
	loc_name[period] = EOS;			/* clip at first name   */
    objectInfo.name = loc_name;
    object = Co_ForEach (c, searchFunction, (GENPTR) &objectInfo); 	
    if (period < 0 || object == NULL)  		/* if simple name...	*/
      return ((Id) object);			/* finished		*/
    if (objectInfo.type == C_VM)
      return ((Id)Vm_Find ((Id) object, &loc_name[period+1]));
    else if (objectInfo.type == C_COLLECTION)
      return (Co_Find ((struct COLLECTION *) object, &loc_name[period+1]));
    else
         return (NULL);
    }


FUNCTION static void* searchFunction 
(
 Id			id,		/* id of this object	*/
 GENPTR	objectInfo_in,	/* context	        */
 TEXT		*objectName,	/* name  of this object */
 CODE		type		/* type of object       */
 )
{
  struct OBJECT_INFO* objectInfo = (struct OBJECT_INFO*) objectInfo_in;
  if (s_equal (objectName, (*objectInfo).name))
    {
      (*objectInfo).type = type;
      return (id);
    }
  else
    return (NULL);
}

/*	Co_ReadFile.  Add Vm objects to a collection.
 *	The Vm objects are read from a concatenated
 *	PAR file or a UNIX archive file.
 *
 */

    FUNCTION CODE Co_ReadFile 
(

 struct COLLECTION*     cm,		/* in: existing collection  */
 TEXT			*filespec,	/* in: name of file to read */
 CODE			mode		/* in: mode for the vm
					   objs */
)
    {
    struct SFILE	f;
    struct PARHDR	ph;
    CODE		code;
    COUNT		recsize;
    struct VM_STRUCT * vm;
    TEXT		record [STRINGSIZ+2];
    BOOL		archive;
    TEXT		fullSpec[STRINGSIZ+1];
    struct	FSBLOCK fsblock;		/* to dig out file name     */
    TEXT		errstr[STRINGSIZ+1];
    COUNT		islash;
#ifdef DEMO_PANEL_LIMIT
    static short vm_count = 0;
    extern BOOL IsWorkBench;
#endif

#ifdef UNIX

#ifdef AIX
	/* NOTE: IBM AIX archive format is completely different from */
	/* all other archive formats */
    struct FULL_HEADER {	/* need header and space for file name */
        struct ar_hdr	header;	
	TEXT *restOfFileName[255]; /* AIX archive name may be 255 chars long */
        } hdrRecord;
    FL_HDR fixed_rec;
    long recptr, ptreven, memTablePtr;
    long namlen,nxtptr;
#else
    union 
        {
	TEXT record [STRINGSIZ+1];		/* padding for long records */
	struct ar_hdr header;			/* the real archive header  */
	} hdrRecord;

#endif
#endif /* unix */

#ifdef apollo
    COUNT		dummy_size;
    COUNT		count;
    TEXT		arSize[sizeof(hdrRecord.ar_size)];
#endif


    code = f_opnspc (&f, 1, filespec, "", "", "res", F_READ);
    if (code != SUCCESS)
	{
        x_error(mode, "Unable to open resource file '%s'.  %s.", "TAE-PFOPN",
		(uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
        return (FAIL);	
	}

    /*	determine whether file is concatenated or archive format */
#ifdef AIX
/* read the fixed length header record of the ibm ar file */
    code = fread( &fixed_rec, FL_HSZ, 1, f.fileptr);
    if (code == SUCCESS  &&  s_lseq (AR_SENTINEL, (TEXT *)&fixed_rec)) 
	{
        sscanf(fixed_rec.fl_fstmoff, "%d", &recptr); /* Next record to read */
        sscanf(fixed_rec.fl_memoff, "%d", &memTablePtr); /* member table ptr */
#else
    code = f_read (&f, record);
    if (code == SUCCESS  &&  s_equal (record, AR_SENTINEL)) 
	{
#endif
	archive = TRUE;
	}
    else
	{
	archive = FALSE;	
	f_rewind (&f);				/* re-start at beginning  */
	}

    /*	read each Vm object from the file */

    while (FOREVER)
	{
	if (archive)					/* skip header 	    */
	    {
#ifdef UNIX
#ifdef AIX
	    if (memTablePtr <= recptr) break;	/* don't read member table */
/* move to position of first member */
	    fseek(f.fileptr, recptr,0);
/* read first member header to get size and namelen */
	    recsize = fread(&hdrRecord, sizeof(hdrRecord), 1, f.fileptr);
	    code = SUCCESS;
#else
	    code = f_read (&f, (char *) &hdrRecord);		/* read hdr or pad  */
	    if (code == SUCCESS  &&  hdrRecord.record[0] == EOS)
	      code = f_read (&f, (char *) &hdrRecord);		/* skip EOF padding */
#endif
	    if (code == F_EOF)
	   	{
		code = SUCCESS;
		break;
		}
	    else if (code != SUCCESS)
		{
	        f_close(&f, F_KEEP);
	        x_error (mode, "File '%s' not correctly formatted.",
			 "TAE-RESFORMAT", (uintptr_t) filespec, 0, 0);
	        return (FAIL);
		}
#ifdef apollo
	    s_bcopy (hdrRecord.header.ar_name, fullSpec, 
				sizeof(hdrRecord.header.ar_name));
	    s_strip (fullSpec);		/* remove trailing blanks */
	    islash = s_index (fullSpec, '/');		/*  HP (and others?)  */
	    if (islash >= 0)				/* term with slash    */
		fullSpec[islash] = EOS;			/* clip at slash      */
	    if (s_equal (fullSpec, ARLGNAME) ||
			s_equal (fullSpec, ARMODNAME))
		{
		s_bcopy (hdrRecord.ar_size, arSize,
				sizeof(hdrRecord.ar_size));
		arSize[sizeof(hdrRecord.ar_size)-1] = EOS;
		s_strip (arSize);
		s_s2i (arSize, &dummy_size);
		count = fseek (f.fileptr, dummy_size, 1);
	        if (count == -1)
		    {
	            f_close(&f, F_KEEP);
	            x_error (mode, "File '%s' not correctly formatted.",
			 "TAE-RESFORMAT", filespec);
	            return (FAIL);
		    }
		continue;
		}
#endif
#ifdef AIX
	    sscanf(hdrRecord.header.ar_namlen,"%d",&namlen);
	    sscanf(hdrRecord.header.ar_nxtmem,"%d",&nxtptr);

            s_bcopy (hdrRecord.header._ar_name.ar_name, fullSpec, namlen);

/* calculate next even byte boundary for start of member data */

	    ptreven = AR_HSZ + namlen + recptr;
	    ptreven = (ptreven +1 )/2 *2;
	    fseek(f.fileptr, ptreven, 0);

	    recptr = nxtptr;		/* set to look at next archive record */
#else
	    s_bcopy (hdrRecord.header.ar_name, fullSpec, 
				sizeof(hdrRecord.header.ar_name));
	    s_strip (fullSpec);		/* remove trailing blanks */
	    islash = s_index (fullSpec, '/');		/*  HP (and others?)  */
	    if (islash >= 0)				/* term with slash    */
		fullSpec[islash] = EOS;			/* clip at slash      */
#endif
	    f_crack (fullSpec, "", "", "", &fsblock, errstr);     /* dig name */

#else  /* unix */
	    x_error (mode, "File '%s' not correctly formatted.",
			"TAE-RESFORMAT", filespec);
	    return (FAIL);
#endif /* unix */
	    }
		/* Now read the Parblk header then the Parblk itself */
	code = f_bread (&f, (GENPTR)&ph, sizeof (ph), &recsize);
	if (code == F_EOF)
	    {
	    code = SUCCESS;			/* normal end-of-file	*/
	    break;
	    }
	else if (code == SUCCESS && s_equal (ph.sentinel, OLD_P_SENTINEL))
	    {
	    f_close (&f, F_KEEP);
	    x_error (mode, 
		"File '%s' has obsolete format.  Use RESCONVERT.",
		     "TAE-OLDRESFILE", (uintptr_t) filespec, 0, 0);
	    return (FAIL);
	    }
	else if (code == SUCCESS && s_equal (ph.sentinel, P_SENTINEL))
	    {
	    f_close (&f, F_KEEP);
	    x_error (mode, 
#ifdef UNIX
           "File '%s' has obsolete format. Use $TAEBIN/resupgrade.",
#else
           "File '%s' has obsolete format. Use tae$lib:resupgrade.",
#endif
		     "TAE-OLDRESFILE", (uintptr_t) filespec, 0, 0);
	    return (FAIL);
	    }
	else if (code != SUCCESS || !s_equal (ph.sentinel, P_BIGSENTINEL))
            {
	    f_close(&f, F_KEEP);
	    x_error(mode, "Error reading resource file '%s'.  %s.", "TAE-PFRD",
		    (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
	    return (FAIL);
            }
	vm = Vm_New (mode);			/* create new vm object   */
	code = Vm_ReadVm (vm, &f); 		/* read parblk records    */
	if (code != SUCCESS)
	    break;				/* x_error already called */
#ifdef DEMO_PANEL_LIMIT
	/* each panel has a target and a view */
	/* allow 1 extra par block for _directory */
        if (!IsWorkBench && (++vm_count > DEMO_PANEL_LIMIT*2+1))
	    {
    	    f_close (&f, F_KEEP);
            printf("\nThis is a demonstration version of TAE Plus.\n");
	    printf("It restricts the number of panels allowed in an "); 
	    printf("application to %d.\n", DEMO_PANEL_LIMIT);
	    printf("Because you have exceeded this number, ");
 	    printf("your operation has failed.\n");
	    exit (1);
	    }
#endif
	if (archive)
	  Co_Add (cm, (GENPTR) vm, fsblock.name, C_VM);	/* add to collection      */
	else	
	  Co_Add (cm, (GENPTR) vm, ph.filename, C_VM);	/* add to collection      */
	}    
    f_close (&f, F_KEEP);
    return (code);
    }



#ifdef UNIX

/*	UNIX kludge to get current position (as opposed to
	the position of the record just read
*/

FUNCTION VOID f_curpos (
			struct SFILE	*sfile,
	struct POSCTX   *posctx
			)
{

(*posctx).pos = ftell ((*sfile).fileptr);
(*posctx).possav = TRUE;
}

#ifdef AIX
/*	Another kludge. We need a straight binary write without the
 *	inter-record count as provided by f_bwrite. 
 *	This routine is essentially f_wrtbio without the sector parameter.
 */
static FUNCTION  f_wbinary(f, buffer, size)

struct SFILE        *f;             /* in/out: SFILE for the file   */
GENPTR              buffer;         /* in: buffer with data to write */
int                 size;           /* in: size of buffer in bytes  */

{
int                 num_bytes;

num_bytes = fwrite(buffer, sizeof(char), size, (*f).fileptr);
if (num_bytes != size)              /* error in writing      */
    {
    return(F_FILERR);
    }
return (SUCCESS);
}
#endif /* AIX */

#endif /* unix */

/*	Co_WriteFile.   Write a collection of Vm objects to a file.
 *
 *	In UNIX, file format is "UNIX archive".
 *	In VMS, the file format is "concatenated PAR files."
 *
 *
 */
    FUNCTION CODE Co_WriteFile 
(

 struct COLLECTION* c,		/* in: collection of Vm objects */
 TEXT		*filespec	/* in: file specification
					   */
 )
    {
    CODE		code;
    void*               code2;
    struct SFILE	f;
#ifdef AIX
    FL_HDR		fl_hdr;		/* AIX fixed header */
    AR_HDR		ar_hdr;		/* archive member header */
    COUNT		i;		/* generic counter */
    TEXT tempString[12+1];
    struct POSCTX	memTablePosition;
#endif

    code = f_opnspc (&f, 1, filespec, "", "", "res", F_WRITE);
    if (code != SUCCESS)
        {
	x_error(P_CONT, "Error opening resource file '%s'.  %s.", "TAE-PFOPN",
		(uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
        return (FAIL);
        }
#ifndef VMS
#ifdef AIX
	/* AIX requires a complex archive structure. */
	/* First the fixed header */

				    /* Clear the fixed header for now */
    for (i=0; i<FL_HSZ; i++) ((TEXT *)&fl_hdr)[i] = ' ';
				    /* Archive file magic string */
    sprintf (fl_hdr.fl_magic, "%s\n", AR_SENTINEL);
				    /* Offset to member table */
    fl_hdr.fl_memoff[0] = '0';
    				    /* Offset to global symbol table(None)*/
    fl_hdr.fl_gstoff[0] = '0';

    fl_hdr.fl_gstoff[0] = '0';
				    /* Offset to first archive member */
				    /* Since new, then right after header */
    sprintf (fl_hdr.fl_fstmoff,"%-12ld", FL_HSZ);
    				    /* Offset to last archive member */
    fl_hdr.fl_lstmoff[0] = '0';
    				    /* Offset to first mem on free list (None)*/
    fl_hdr.fl_freeoff[0] = '0';
    f_wbinary (&f, &fl_hdr, FL_HSZ); /* Write the Fixed header, updated later */

	/* Now prepare for the Member Table */
	/* this must be filled out as we go along. */
    members = Co_Count (c);		/* How many members are there? */
    memOffsets = (TEXT *)tae_alloc (members, 12);
    memNames = (TEXT **) tae_alloc (members, sizeof (TEXT *));
    curMemberIndex = 0;
    prvmem = 0;
    nxtmem = FL_HSZ;		      /* first record at start after header */
    memTableSize = 12 * (members + 1);/* later add length of each name string */
#else
    f_write (&f, AR_SENTINEL);			/* archive sentinel string */ 
#endif
#endif
	/* Write each xxx.par file */
    code2 = Co_ForEach (c, writeVmToFile, (GENPTR) &f);
#ifdef AIX
	/* Now write Member Table header and record */
	/* Then update the fixed header */
	/* Finally, free the member stuff and we are done */
	/* Note: currently positioned ready to write the member Table hdr*/
    for (i=0; i<AR_HSZ; i++) ((TEXT *)&ar_hdr)[i] = ' ';

    sprintf (tempString, "%-12ld", memTableSize);
    bytmov (tempString, ar_hdr.ar_size, sizeof (ar_hdr.ar_size));
    ar_hdr.ar_nxtmem[0] = '0';
    sprintf (tempString, "%-12ld", prvmem); 
    bytmov (tempString, ar_hdr.ar_prvmem, sizeof (ar_hdr.ar_prvmem));
    
    ar_hdr.ar_date[0] = '0';
    ar_hdr.ar_uid[0] = '0';
    ar_hdr.ar_gid[0] = '0';
    ar_hdr.ar_mode[0] = '0';
    ar_hdr.ar_namlen[0] = '0';
    ar_hdr._ar_name.ar_fmag[0] = '`';
    ar_hdr._ar_name.ar_fmag[1] = '\n';

    f_curpos (&f, &memTablePosition);	/* position of member Table header */
    f_wbinary (&f, &ar_hdr, AR_HSZ);	/* write archive hdr recrd */

	/* write the number of members */
    sprintf (tempString, "%-12ld", members);
    f_wbinary (&f, tempString, 12);

	/* write the member Offset array */
    f_wbinary (&f, memOffsets, 12 * members);

	/* Now write each name (null terminated ) */
    for (i = 0; i < members; ++i)
	f_wbinary(&f, memNames[i], s_length(memNames[i])+1); /* need EOS too */
   
	/* update fixed header */
    f_rewind (&f);			/* now postion back to beginning to */
    bytmov (ar_hdr.ar_prvmem, fl_hdr.fl_lstmoff, sizeof (fl_hdr.fl_lstmoff));
    sprintf (tempString, "%-12ld", memTablePosition.pos);
    bytmov (tempString,   fl_hdr.fl_memoff, sizeof(fl_hdr.fl_memoff));
    f_wbinary (&f, &fl_hdr, FL_HSZ);	/* update the fixed header */

    tae_free (memOffsets);
    for (i = 0; i < members; i++)
	tae_free(memNames[i]);	
    tae_free (memNames);
#endif
    f_close (&f, F_KEEP);
    if (code2 != 0)
	return (FAIL);			/* premature Co_ForEach termination */
    return (SUCCESS);
    }



/*	Co_ForEach callback: write one vm to the already open file	*/

FUNCTION static void* writeVmToFile (Id vm, GENPTR fin, TEXT* name, CODE type)
{
  struct SFILE* f = (struct SFILE*) fin;
    CODE		code;		
    struct PARHDR	ph;

    TEXT		filespec[STRINGSIZ+1];
    TEXT		sizeString[STRINGSIZ+1];
    TEXT		dateString[STRINGSIZ+1];
    LONG		fileSize;

    struct POSCTX	headerPosition;
    struct POSCTX	eofPosition;		/* position of next file */
    struct POSCTX	startPosition;		/* position of file start */
    COUNT		length;

#ifdef UNIX
#ifdef AIX
    TEXT		tempString[12+1];
    struct POSCTX	memberPos;
    struct FULL_HEADER {	/* need header and space for file name */
        struct ar_hdr	ar_hdr;
	TEXT *restOfFileName[255];
        } header;
#else
    struct ar_hdr	header;
#endif
    struct timeval	time;

    /*	build and write the archive header record */

    s_copy (name, filespec);
    s_append (".par", filespec);		/* add .par suffix	  */
#ifdef AIX
    s_blank ((TEXT *) &header, sizeof (struct FULL_HEADER) - 1);
    length = s_length (filespec);
    sprintf (tempString, "%-12ld", length);
    bytmov (tempString, header.ar_hdr.ar_namlen, 
	sizeof (header.ar_hdr.ar_namlen));

    s_copy (filespec, header.ar_hdr._ar_name.ar_name);

    sprintf (tempString, "%-12ld", prvmem);
    bytmov (tempString, header.ar_hdr.ar_prvmem, 
	sizeof (header.ar_hdr.ar_prvmem));
    sprintf (tempString, "%-12ld", nxtmem);	/* save member Offsets */
    bytmov (tempString, &memOffsets[curMemberIndex*12], 12);/* save mem Offset*/
    f_movpos (&(f->posctx), &memberPos);
    memberPos.pos = nxtmem;
    f_setpos (f, &memberPos);

    memNames[curMemberIndex] = tae_alloc (1, length+1);
    s_copy (filespec, memNames[curMemberIndex++]);
    memTableSize += length + 1;
    prvmem = nxtmem;

    sprintf (tempString, "%-12d", getuid());	/* user id */
    bytmov (tempString, header.ar_hdr.ar_uid, s_length(tempString));
    sprintf (tempString, "%-12d", getgid());	/* group id */
    bytmov (tempString, header.ar_hdr.ar_gid, s_length(tempString));
    bytmov ("644", header.ar_hdr.ar_mode, 3);
#else
    s_blank ((TEXT *) &header, sizeof (struct ar_hdr) - 1);
    length = min(s_length (filespec), sizeof (header.ar_name));
    bytmov (filespec, header.ar_name, length); 
    bytmov ("000   ", header.ar_uid,  sizeof (header.ar_uid));
    bytmov ("000   ", header.ar_gid,  sizeof (header.ar_gid));
    bytmov ("100644  ", header.ar_mode, sizeof (header.ar_mode));
    header.ar_fmag[0] = '`';
    header.ar_fmag[1] = EOS;			/* becomes \n when written */
#endif
    gettimeofday (&time, NULL);
    sprintf (dateString, "%lu", time.tv_sec);
#ifdef AIX
    bytmov (dateString, header.ar_hdr.ar_date, s_length(dateString));
#else
    bytmov (dateString, header.ar_date, s_length(dateString));
#endif
#endif /* unix */
#ifdef AIX
    f_curpos (f, &headerPosition);	/* save position of the rec   */
    f_wbinary(f, &header, s_length(&header));
    f_write(f,"`");
    {
    long ptreven = ftell ((*f).fileptr);	/* force to even boundary */
    ptreven = (ptreven +1 )/2 * 2;
    fseek((*f).fileptr, ptreven, 0);
    }
#else
    f_write (f, (TEXT *) &header); /* write archive hdr recrd */
    f_movpos (&(*f).posctx, &headerPosition);	/* save position of the rec   */
#endif

    zero_block ((GENPTR) &ph, sizeof (ph));
    s_copy (P_BIGSENTINEL, ph.sentinel);
    ph.recsize = sizeof (struct LARGE_PARBLK);
    s_copy ("TIME", ph.datetime);
    s_bcopy (name, ph.filename, sizeof (ph.filename)-1);

    code = f_bwrite (f, (GENPTR)&ph, sizeof (ph));
    if (code != SUCCESS)
	return (0);				/* stop the Co_ForEach   */
    f_movpos (&(*f).posctx, &startPosition);	/* position of PARHDR record */
    code = Vm_WriteVm ((struct VM_STRUCT *)vm, f);
    if (code == SUCCESS)
	{

#ifdef UNIX
        f_curpos (f, &eofPosition);
	f_setpos (f, &headerPosition);		/* go back and update header  */
	fileSize = eofPosition.pos - startPosition.pos;
	sprintf (sizeString, "%ld", fileSize);	/* convert to ascii	    */
#ifdef AIX
	bytmov (sizeString, header.ar_hdr.ar_size, s_length (sizeString));
	nxtmem = (fileSize & 1) ? eofPosition.pos+1 : eofPosition.pos;
	sprintf (tempString, "%-ld", nxtmem);
	bytmov (tempString, header.ar_hdr.ar_nxtmem, 
	    sizeof(header.ar_hdr.ar_nxtmem));
	f_wbinary (f, &header, s_length(&header));
#else
	bytmov (sizeString, header.ar_size, s_length (sizeString));
	f_write (f, (TEXT*) &header);
#endif
        f_setpos (f, &eofPosition);		/* get ready for next file    */
	if (fileSize & 1)
	    f_write (f, "");			/* get to dble byte bndry */
#endif /* unix */
        return (0);			/* keep Co_ForEach going  */
	}
    else
        return (0);			/* stop the Co_ForEach	  */
    }


#else /* testmain */

main ()

{
Id	c, sc;
int	i;
TEXT	name[STRINGSIZ+1];
CODE	testFunction(), deleteFunction ();
int	*ip;
CODE	display ();
CODE	Vm_Free ();
CODE 	writeSingleFile ();
struct VARIABLE	*v;

c =  Co_New (0);
for (i=0; i < 10; i++)
    {
    ip = (int *) tae_alloc (1, sizeof (int));	/* each obj is an int */
    *ip = i;					/* value of obj  */
    sprintf (name, "%d", i);			/* make a name	 */
    Co_Add (c, ip, name, i);			/* let type == i */
    }
Co_ForEach (c, testFunction, "proper context");
ip = Co_Find (c, "5");
if (*ip != 5) printf ("\n***********error in Co_Find call\n\n");
Co_Free (c, deleteFunction);

/*	start again and test remove */

c = Co_New (0);
for (i=0; i < 10; i++)
    {
    ip = (int *) tae_alloc (1, sizeof (int));	/* each obj is an int */
    *ip = i;					/* value of obj  */
    sprintf (name, "%d", i);			/* make a name	 */
    Co_Add (c, ip, name, i);			/* let type == i */
    }
ip = (int *) Co_Remove (c, "0");
tae_free (ip);
ip = (int *) Co_Remove (c, "5");
tae_free (ip);
ip = (int *) Co_Remove (c, "9");
tae_free (ip);
Co_ForEach (c, testFunction, "0, 5, and 9 missing?");
Co_Free (c, deleteFunction);
printf ("\n\n********** remember to check dm_bytes zero here *********\n");

/*	vm read and write	*/

    c = Co_New (0);
    Co_ReadFile (c, "test", P_ABORT) ;		/* read concatenated file */
    Co_ForEach (c, display, NULL);		/* show variable names	  */
    Co_ForEach (c, writeSingleFile, NULL);	/* write individual files */
    Co_WriteFile (c, "newtest") ;		/* wirte one concat file  */
    Co_Free (c, Vm_Free);
    printf ("\n\n********** remember to check dm_bytes zero here *********\n");

/*	test collections of collections	*/

c =  Co_New (0);
for (i=0; i < 10; i++)
    {
    ip = (int *) tae_alloc (1, sizeof (int));	/* each obj is an int */
    *ip = i;					/* value of obj  */
    sprintf (name, "%d", i);			/* make a name	 */
    Co_Add (c, ip, name, i);			/* let type == i */
    }
sc = Co_New (0);				/* super collection       */
Co_Add (sc, c, "subc1", C_COLLECTION);		/* with 4 sub-collections */
Co_Add (sc, c, "subc2", C_COLLECTION);
Co_Add (sc, c, "subc3", C_COLLECTION);
Co_Add (sc, c, "subc4", C_COLLECTION);

/*	do some composite lookups	*/

ip = (int *) Co_Find (sc, "subc1.5");
printf ("should be a 5: %d\n", *ip);
ip = (int *) Co_Find (sc, "subc2.1");
printf ("should be a 1: %d\n", *ip);
Co_Free (c, deleteFunction);
Co_Free (sc, NULL);
printf ("\n\n********** remember to check dm_bytes zero here *********\n");

/*	check composite Co_Find with Vm objects		*/

c = Co_New (0);
Co_ReadFile (c, "test", P_ABORT) ;		/* read concatenated file */
v = (struct VARIABLE *) Co_Find (c, "pp_target.p2");
printf ("variable P2 in block pp_target: %s\n", SVAL(*v,0));
v = (struct VARIABLE *) Co_Find (c, "pp_view._panel.origin");
printf ("variable _panel.origin in block pp_view: %d  %d \n", 
					      IVAL(*v,0), IVAL(*v,1));
Co_Free (c, Vm_Free);
printf ("\n\n********** remember to check dm_bytes zero here *********\n");

}


FUNCTION CODE testFunction (i, string, name, type)

    int 	*i; 		/* object	*/
    TEXT	*string;	/* context	*/
    TEXT	*name;
    FUNINT	type;

    {
    printf ("object = %d, context='%s', name='%s', type=%d\n", 
    		*i, string, name, type);
    return (0);
    }

FUNCTION VOID deleteFunction (i)
    
    int	*i;
    {
    printf ("delete called with %d\n", *i);
    tae_free (i);
    }

FUNCTION CODE	display (vm, context, name)	/* display a vm */

    Id		vm;
    CODE	context;		/* not used 	*/
    TEXT	*name;			/* name of vm	*/

    {
    CODE	displayv();

    printf ("vm name %s\n", name);
    Vm_ForEach (vm, displayv, NULL);
    return (0);
    }


FUNCTION CODE displayv (v)
    struct VARIABLE *v;
    {
    printf ("\t %s\n", (*v).v_name);
    return (0);
    }

FUNCTION CODE writeSingleFile (vm, context, name)
	Id	vm;
        Id	context;	/* not used */
        TEXT	*name;
    {
    CODE	code;

    code = Vm_WriteToDisk (vm, name);
    if (code != SUCCESS)
        exit (-1);
    return (0);			/* Co_ForEach callback */
    }


/* 
 *	x_error.   Report error and abort if in P_ABORT mode; continue
 *	otherwise.
 *
 */

FUNCTION VOID x_error
(

 FUNINT	mode,			/* in: P_ABORT or P_CONT	*/
 TEXT	message[],		/* in: message text		*/
 TEXT	key[],			/* in: message key 		*/
 uintptr_t 	A1, A2, A3		/* in: integer or string ptrs
					 */
 )
    {
   
    printf (message, A1, A2, A3);
    exit (0);
    }

#endif /* testmain */
