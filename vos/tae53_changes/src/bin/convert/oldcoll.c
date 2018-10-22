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
 *	28-sep-89	declare startPosition for non-UNIX system...ljn
 *	05-apr-91	Strange ~ in front of NULL...ljn
 *	05-oct-92	IBM RS/6000 port...rt
 *	06-oct-92	SGI... need to use the BSD time header file...bth/rt
 *	22-oct-92	Prototyping tae_alloc in each function is unecessary
 *			and Ultrix 4.3 does not like it...rt
 *      09-dec-93       Solaris Port: Solaris (SunOS 5.x) needs to use 
 *                      <sys/time.h> instead of <time.h> (backport from aug-93)
 *                      ...ws/cb
 *	19-apr-94	SCO port: SCO needs sys/time.h instead of time.h,
 *			needs PORTAR defined for ar.h ...swd
 *	15-jun-94	Intergraph Port: Use <sys/time.h> instead of <time.h>
 *			(backport from 06-apr-93)...bth/rt
 */

#include	"taeconf.inp"
#include	"parblk.inc"
#include	"fileinc.inp"
#define		AR_SENTINEL     "!<arch>"	/* without newline */

#ifdef UNIX
#if defined(sco)
#define PORTAR
#endif
#include 	<ar.h>
#if defined(macII) || defined(sgi) || defined(sco) || defined(__clipper__)
#include	<sys/time.h>
#else
#ifdef SYSV
#if defined(mipseb) || (defined(sun) && OSMajorVersion >= 5) || defined(__linux__)
#include	<sys/time.h>
#else
#include	<time.h>
#endif
#else
#include	<sys/time.h>
#endif
#endif
#endif

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


struct VM_STRUCT;

/*	The collection context block has the following format.
 *	By pointing to the last element in the LINKed list,
 *	we can access the last element (for additions) or
 *	the first element (for streaming).
 */


#if 0	/* now in vmcoproto.h */
struct COLLECTION
    {
    struct LINK *last;			/* ptr to last entry */
    CODE	flags;			/* OR of ...	     */

#define C_DUPLICATES  0x0001		/* duplicates ok 	   */
#define C_ALPHA	      0x0002		/* maintain in alpha order */

    COUNT	count;
    };
#endif


FUNCTION Id old_Co_New (

    CODE flags			/* C_DUPLICATES, C_ALPHA	*/
)

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

FUNCTION VOID old_Co_Add (

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


FUNCTION VOID old_Co_Free (

    struct COLLECTION *c,			/* id of collection     */
    VOID	      (*deleteFunction)(Id, CODE)	/* for deleting each	*/
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

FUNCTION VOID old_Co_Free_Nocall (
    GENPTR	c			/* IN: ptr to collection */
)
    {
    old_Co_Free ((struct COLLECTION *)c, NULL);
    }


/*	Co_Empty.   TRUE if collection is empty.
 *
 */

FUNCTION BOOL old_Co_Empty (

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
	
/* WARNING:  Not all functions in here have the same arg lists!!!	*/
/* So who knows if this will actually work.  rgd 2/2010			*/
typedef CODE (*coFunction)(Id, COUNT *, TEXT *, CODE);
FUNCTION CODE old_Co_ForEach (
	
    struct COLLECTION	*c,			/* collection id	*/
    coFunction aFunction,	/* function to call	*/
    GENPTR		context		/* arg to pass		*/
)

    {
    struct LINK		*last;
    struct LINK 	*current;
    struct LINK 	*next;
    CODE		code;

    last = (*c).last;
    code = 0;
    if (last != NULL)
        {
	current = (*last).next;   
        while (1)  
	    {
	    next = (*current).next; 		
	    code = (*aFunction) ((*current).object, 
    				 (COUNT *)context, 
    				 (*current).name,
    				 (*current).type);
	    if (code != 0 || (*c).last == NULL || current == (*c).last)
		break;
    	    current = next;
	    }
        }
    return (code);
    }



static COUNT countEntries (

	Id	object,
	COUNT	*count,
	TEXT	*name,
	CODE	type
)
{
(*count)++;
return (0);
} 


FUNCTION COUNT old_Co_Count (		/* return number of entries */
    
   struct COLLECTION *c
)
{
COUNT	count;
count = 0;
old_Co_ForEach (c, (coFunction)countEntries, (GENPTR)&count);
return (count); 
}


/*	Callback to build pointer vector:	*/

FUNCTION static CODE buildIdVector (
	Id	aMember,
	Id	**vector,
	TEXT	*name
)
    {
    *(*vector) = aMember;
    (*vector)++;
    return (0);
    }

typedef TEXT *Name;

/*	Callback to build name vector	*/

FUNCTION static CODE buildNameVector (
	Id	aMember,
	Name	**vector,
	TEXT	*name
)
    {
    *(*vector) = name;
    (*vector) ++;
    return (0);
    }


/*	Co_IdVector.   Build a vector of Ids
 *	one component for each object in the list.
 *
 *	Caller must de-allocate the vector.
 */

FUNCTION Id * old_Co_IdVector (
	
    struct COLLECTION	*c			/* collection id	*/
)

    {
    Id			*vector, *vector0;
    COUNT		count;

    count = old_Co_Count (c);
    vector0 = (Id *) tae_alloc (1, (count+1) * sizeof (Id *));
    vector = vector0;
    old_Co_ForEach (c, (coFunction)buildIdVector, (GENPTR)&vector);
    *vector = NULL;				/* null terminate	*/
    return (vector0);
    }


FUNCTION Name *old_Co_NameVector (
	
    struct COLLECTION	*c			/* collection id	*/
)

    {
    Name		*vector, *vector0;
    COUNT		count;

    count = old_Co_Count (c);
    vector0 = (Name *) tae_alloc (1, (count+1) * sizeof (Name *));
    vector = vector0;
    old_Co_ForEach (c, (coFunction)buildNameVector, (GENPTR)&vector);
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

    FUNCTION Id old_Co_Remove (

    	struct COLLECTION	*c,		/* a collection 	*/
        TEXT			name[]		/* member to delete	*/
    )

    {
    struct LINK		*current, *previous;
    Id			object;

    if ((*c).last == NULL)
	return (FAIL);
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





FUNCTION static Id searchFunction (

    Id			id,		/* id of this object	*/
    struct OBJECT_INFO	*objectInfo,	/* context	        */
    TEXT		*objectName,	/* name  of this object */
    CODE		type		/* type of object       */
)

    {
    if (s_equal (objectName, (*objectInfo).name))
        {
        (*objectInfo).type = type;
        return (id);
        }
    else
        return (NULL);
    }


FUNCTION Id old_Co_Find (

    struct COLLECTION	*c,		/* in: collection id	*/
    TEXT		name[]		/* in: name to find	*/
)

    {
    TEXT		loc_name[STRINGSIZ+1];    
    Id			object;
    struct OBJECT_INFO	objectInfo;		/* context block	*/
    
    s_bcopy (name, loc_name, STRINGSIZ);	/* local copy 		*/
    objectInfo.name = loc_name;
    object = (Id) (uintptr_t)old_Co_ForEach (c, (coFunction)searchFunction, (GENPTR)&objectInfo); 	
    return (object);			/* finished		*/
    }


/*	Co_ReadFile.  Add Vm objects to a collection.
 *	The Vm objects are read from a concatenated
 *	PAR file or a UNIX archive file.
 *
 */

    FUNCTION CODE old_Co_ReadFile (

    	Id 			c,		/* in: existing collection  */
        TEXT			*filespec,	/* in: name of file to read */
	CODE			mode		/* in: mode for the vm objs */
    )
    {
    struct SFILE	f;
    union {
          struct PARHDR	ph;			/* standard header          */	
	  TEXT		padding[300];		/* in case reading new file */
	  } headBuffer;
    CODE		code;
    COUNT		recsize;
    Id			vm;
    TEXT		record [STRINGSIZ+2];
    BOOL		archive;
    TEXT		fullSpec[STRINGSIZ+1];
    struct	FSBLOCK fsblock;		/* to dig out file name     */
    TEXT		errstr[STRINGSIZ+1];
    COUNT		islash;

#ifdef UNIX
    union 
        {
	TEXT record [STRINGSIZ+1];		/* padding for long records */
	struct ar_hdr header;			/* the real archive header  */
	} hdrRecord;
#endif


    code = f_opnspc (&f, 1, filespec, "", "", "res", F_READ);
    if (code != SUCCESS)
	{
        x_error(mode, "Unable to open resource file '%s'.  %s.", "TAE-PFOPN",
	    (uintptr_t)filespec, (uintptr_t)f.errmsg, 0);
        return (FAIL);	
	}

    /*	determine whether file is concatenated or archive format */
    code = f_read (&f, record);
    if (code == SUCCESS  &&  s_equal (record, AR_SENTINEL)) 
	archive = TRUE;
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
	    code = f_read (&f, (TEXT *)&hdrRecord);	/* read hdr or pad  */
	    if (code == SUCCESS  &&  hdrRecord.record[0] == EOS)
		code = f_read (&f, (TEXT *)&hdrRecord);	/* skip EOF padding */
	    if (code == F_EOF)
	   	{
		code = SUCCESS;
		break;
		}
	    else if (code != SUCCESS)
		{
	        f_close(&f, F_KEEP);
	        x_error (mode, "File '%s' not correctly formatted.",
			 "TAE-RESFORMAT", (uintptr_t)filespec, 0, 0);
	        return (FAIL);
		}
#ifdef AIX
	    s_bcopy (hdrRecord.header._ar_name.ar_name, fullSpec,
                               sizeof(hdrRecord.header._ar_name.ar_name));
#else
	    s_bcopy (hdrRecord.header.ar_name, fullSpec, 
				sizeof(hdrRecord.header.ar_name));
#endif
	    s_strip (fullSpec);		/* remove trailing blanks */
	    islash = s_index (fullSpec, '/');		/*  HP (and others?)  */
	    if (islash >= 0)				/* term with slash    */
		fullSpec[islash] = EOS;			/* clip at slash      */
	    f_crack (fullSpec, "", "", "", &fsblock, errstr);     /* dig name */
#else
	    x_error (mode, "File '%s' not correctly formatted.",
			"TAE-RESFORMAT", filespec, 0, 0);
	    return (FAIL);
#endif
	    }
	code = f_bread (&f, (GENPTR)&headBuffer, sizeof (headBuffer), &recsize);
	if (code == F_EOF)
	    {
	    code = SUCCESS;			/* normal end-of-file	*/
	    break;
	    }
	else if (code == SUCCESS && 
		s_equal (headBuffer.ph.sentinel, "<<new par-file>>"))
	    {
	    f_close(&f, F_KEEP);
	    x_error(mode, "Resource file '%s' has already been converted.", 
		"TAE-VNEWFORMAT", (uintptr_t)filespec, 0, 0);
	    return (FAIL);
	    }
	else if(code != SUCCESS || !s_equal(headBuffer.ph.sentinel, P_SENTINEL))
            {
	    f_close(&f, F_KEEP);
	    x_error(mode, "Error reading resource file '%s'.  %s.", "TAE-PFRD",
		(uintptr_t)filespec, (uintptr_t)f.errmsg, 0);
	    return (FAIL);
            }
	vm = Vm_New (mode);			/* create new vm object   */
	code = Vm_ReadVm ((struct VM_STRUCT *)vm, &f); 		/* read parblk records    */
	if (code != SUCCESS)
	    break;				/* x_error already called */
	if (archive)
	    old_Co_Add ((struct COLLECTION *)c, vm, fsblock.name, C_VM);
	else	
	    old_Co_Add ((struct COLLECTION *)c, vm, headBuffer.ph.filename, C_VM);
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

#endif 

/*	Co_WriteFile.   Write a collection of Vm objects to a file.
 *
 *	In UNIX, file format is "UNIX archive".
 *	In VMS, the file format is "concatenated PAR files."
 *
 *
 */
FUNCTION static CODE writeVmToFile (Id, struct SFILE *, TEXT *name);

    FUNCTION CODE old_Co_WriteFile (

    	Id		c,		/* in: collection of Vm objects */
        TEXT		*filespec	/* in: file specification 	*/
    )
    {
    CODE		code;
    struct SFILE	f;

    code = f_opnspc (&f, 1, filespec, "", "", "res", F_WRITE);
    if (code != SUCCESS)
        {
	x_error(P_CONT, "Error opening resource file '%s'.  %s.", "TAE-PFOPN",
		(uintptr_t)filespec, (uintptr_t)f.errmsg, 0);
        return (FAIL);
        }
    f_write (&f, AR_SENTINEL);			/* archive sentinel string */ 
    code = old_Co_ForEach ((struct COLLECTION *)c, (coFunction)writeVmToFile, (GENPTR)&f);
    f_close (&f, F_KEEP);
    if (code != 0)
	return (FAIL);			/* premature Co_ForEach termination */
    return (SUCCESS);
    }



/*	Co_ForEach callback: write one vm to the already open file	*/

FUNCTION static CODE writeVmToFile (

	Id		vm,		/* in: vm to write	*/
	struct SFILE	*f,		/* in: SFILE to use	*/
	TEXT		*name)		/* in: name of vm	*/

    {
    CODE		code;		
    struct PARHDR	ph;


    TEXT		filespec[STRINGSIZ+1];
    TEXT		sizeString[STRINGSIZ+1];
    TEXT		dateString[STRINGSIZ+1];
    LONG		fileSize;

    struct POSCTX	startPosition;		/* position of file start */
#ifdef UNIX
    struct ar_hdr	header;
    struct POSCTX	headerPosition;
    struct POSCTX	eofPosition;		/* position of next file */
    COUNT		length;
    struct timeval	time;

    /*	build and write the archive header record */

    s_blank ((TEXT *) &header, sizeof (struct ar_hdr) - 1);
    s_copy (name, filespec);
    s_append (".par", filespec);		/* add .par suffix	  */
#ifdef AIX
    length = min(s_length (filespec), sizeof (header._ar_name.ar_name));
    bytmov (filespec, header._ar_name.ar_name, length);
#else
    length = min(s_length (filespec), sizeof (header.ar_name));
    bytmov (filespec, header.ar_name, length); 
#endif
    gettimeofday (&time, NULL);
    sprintf (dateString, "%lu", time.tv_sec);
    bytmov (dateString,   header.ar_date, s_length(dateString));
    bytmov ("000   ", header.ar_uid,  sizeof (header.ar_uid));
    bytmov ("000   ", header.ar_gid,  sizeof (header.ar_gid));
    bytmov ("100644  ", header.ar_mode, sizeof (header.ar_mode));
#ifdef AIX
    header._ar_name.ar_fmag[0] = '\`';
    header._ar_name.ar_fmag[1] = EOS;           /* becomes \n when written */
#else
    header.ar_fmag[0] = '`';
    header.ar_fmag[1] = EOS;			/* becomes \n when written */
#endif
    f_write (f, (TEXT *)&header);		/* write archive hdr recrd */
    f_movpos (&(*f).posctx, &headerPosition);	/* save position of the rec   */
#endif

    zero_block ((GENPTR)&ph, sizeof (ph));
    s_copy (P_SENTINEL, ph.sentinel);
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
	bytmov (sizeString, header.ar_size, s_length (sizeString));
	f_write (f, (TEXT *)&header);
        f_setpos (f, &eofPosition);		/* get ready for next file    */
	if (fileSize & 1)
	    f_write (f, "");			/* get to dble byte bndry */
#endif
        return (0);			/* keep Co_ForEach going  */
	}
    else
        return (0);			/* stop the Co_ForEach	  */
    }


#else

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
Co_ForEach (c, (coFunction)testFunction, "proper context");
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
Co_ForEach (c, (coFunction)testFunction, "0, 5, and 9 missing?");
Co_Free (c, deleteFunction);
printf ("\n\n********** remember to check dm_bytes zero here *********\n");

/*	vm read and write	*/

    c = Co_New (0);
    Co_ReadFile (c, "test", P_ABORT) ;		/* read concatenated file */
    Co_ForEach (c, (coFunction)display, NULL);		/* show variable names	  */
    Co_ForEach (c, (coFunction)writeSingleFile, NULL);	/* write individual files */
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


FUNCTION CODE testFunction (

    int 	*i, 		/* object	*/
    TEXT	*string,	/* context	*/
    TEXT	*name,
    FUNINT	type
)

    {
    printf ("object = %d, context='%s', name='%s', type=%d\n", 
    		*i, string, name, type);
    return (NULL);
    }

FUNCTION VOID deleteFunction (int *i)
    {
    printf ("delete called with %d\n", *i);
    tae_free (i);
    }

FUNCTION CODE	display (	/* display a vm */

    Id		vm,
    CODE	context,		/* not used 	*/
    TEXT	*name			/* name of vm	*/
)

    {
    CODE	displayv();

    printf ("vm name %s\n", name);
    Vm_ForEach (vm, displayv, NULL);
    return (NULL);
    }


FUNCTION CODE displayv (struct VARIABLE *v)
    {
    printf ("\t %s\n", (*v).v_name);
    return (NULL);
    }

FUNCTION CODE writeSingleFile (
	Id	vm,
        Id	context,	/* not used */
        TEXT	*name
)
    {
    CODE	code;

    code = Vm_WriteToDisk (vm, name);
    if (code != SUCCESS)
        exit (-1);
    return (NULL);			/* Co_ForEach callback */
    }


/* 
 *	x_error.   Report error and abort if in P_ABORT mode; continue
 *	otherwise.
 *
 */

    FUNCTION VOID x_error(

    FUNINT	mode,			/* in: P_ABORT or P_CONT	*/
    TEXT	message[],		/* in: message text		*/
    TEXT	key[],			/* in: message key 		*/
    uintptr_t 	A1,			/* in: integer or string ptrs 	*/
    uintptr_t   A2,
    uintptr_t   A3
    )
    {
   
    printf (message, A1, A2, A3);
    exit (0);
    }

#endif
