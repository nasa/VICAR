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
 *	TAE version 4 XCO_ inner bridges for manipulation of 
 *	collections of Vm objects (CO_ Package)
 *
 *	CHANGE LOG:
 *
 *	6/22/89 Initial Cut...rsg/AS
 *	12-mar-90	Changed all function names to agree with XCOBR
 *			names...krw
 *	22-jul-92	Label Co_FindIt UNSUPPORTED...kbs
 *	09-jul-93	Use of vmcoproto.h required some changes...rt
 *	07-dec-93	Solaris port (backported dag from aug-93)...cob
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include 	"parblk.inc"	/* get P_BADNAME, etc.                  */ 
#include 	"symtab.inc" 
#include        "fileinc.inp"
#include        "pgminc.inc"
#include "taeintproto.h"


    COUNT	bytes;
    TEXT  	cname[STRINGSIZ+1]; /* generic C string for F77 conversion */
    CODE	code;

    UNSUPPORTED Id Co_FindIt
    (
     Id			colid,		/* in: collection id	*/
     TEXT		name[],		/* in: name to find	*/
    CODE		*type		/* out: type of collection */
     );
FUNCTION static void* searchIt
(
    Id			id,		/* id of this object	*/
    GENPTR		objectInfoin,	/* context	        */
    TEXT		*objectName,	/* name  of this object */
    CODE		type		/* type of object       */

);
/* FUNCTION static void* getnextfcn  */
/* ( */
/*     Id			id,		/\* id of this object	*\/ */
/*     GENPTR		objectInfoin,	/\* context	        *\/ */
/*     TEXT		*objectName,	/\* name  of this object *\/ */
/*     CODE		type		/\* type of object       *\/ */

/*  ); */


/*
*	xco_add:  add member to collection
*/

FUNCTION VOID BRIDGE2_NAME(xco_add)
(	
 TAEINT *c,	/* in: id of the collection to add the object to */
 Id     *objid,	/* in: id of the object to be added to collection */
 FORSTR *memnam,	/* in: name ofthe member object */
 TAEINT *type,	/* in: type of the object */
 TAEINT *status	/* out: SUCCESS or error code */
)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(memnam, cname, 0); */
/* 		s_strip(cname); */
/* 		Co_Add(*c, *objid, cname, *type); */
/* 		*status = SUCCESS; */
/* 		return; */
	}


/*
*	xco_remove:  remove a member from a collection
*/

FUNCTION VOID BRIDGE2_NAME(xco_remove)
(
 TAEINT *colxid,		/* in: collection id */
 FORSTR *memnam,		/* in: name of the member to be removed */
 TAEINT *status			/* out: SUCCESS or error code */
 )
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(memnam, cname, 0); */
/* 		s_strip(cname); */
/* 		code = (CODE) Co_Remove((struct COLLECTION*) *colxid, cname); */
/* 		*status = (code == 0)? FAIL: SUCCESS; */
/* 		return; */
	}

		

/*
*	xco_find:  find specific member
*/

FUNCTION VOID BRIDGE2_NAME(xco_find)
(
 TAEINT *colxid,	/* in: collection id */
 FORSTR *memnam,	/* in: name of member object */
 TAEINT *objid,	/* out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status	/* out: SUCCESS or error code */
)
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(memnam, cname, 0); */
/* 		s_strip(cname); */
/*   		code = *objid = (TAEINT) Co_FindIt(*colxid, cname, type);  */
/* 		if (code == 0) */
/* 			*status = FAIL; */
/* 		else */
/* 			*status = SUCCESS; */
/* 		return; */
	}

/*	Co_FindIt.   Find object by name.  Same as Co_Find, except returns
 *			collection type as well. Inheritance would have been
 *			nice here!
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

struct OBJECT_INFO	/* for comm to callback function thru Co_ForEach  */
    {
    CODE	type;			/* output of searchIt*/
    TEXT	*name;			/* input to searchIt*/
    BOOL	first;		 	/* input to searchIt*/
    };

#define C_COLLECTION  0xcccc     /* type of collect object */
#define C_VM          0xaaaa     /* type of Vm object      */
    UNSUPPORTED Id Co_FindIt
    (
     Id			colid,		/* in: collection id	*/
     TEXT		name[],		/* in: name to find	*/
    CODE		*type		/* out: type of collection */
     )
    {
    TEXT		loc_name[STRINGSIZ+1];    
    Id			object;
    struct OBJECT_INFO	objectInfo;		/* context block	*/
    COUNT		period;
    
    s_bcopy (name, loc_name, STRINGSIZ);	/* local copy 		*/
    period = s_index (loc_name, '.');		/* composite name?	*/
    if (period >= 0)
	loc_name[period] = EOS;			/* clip at first name   */
    objectInfo.name = loc_name;
    object = (Id) Co_ForEach ((struct COLLECTION*) colid, searchIt, 
			      (GENPTR) &objectInfo); 	
    *type = objectInfo.type;
    if (period < 0 || object == NULL)  		/* if simple name...	*/
	return (object);			/* finished		*/
    if (objectInfo.type == C_VM)
         return ((Id)Vm_Find (object, &loc_name[period+1]));
    else if (objectInfo.type == C_COLLECTION) {
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
//         return (Co_FindIt (object, &loc_name[period+1], *type));
    } else
         return (NULL);
    }



FUNCTION static void* searchIt
(
    Id			id,		/* id of this object	*/
    GENPTR		objectInfoin,	/* context	        */
    TEXT		*objectName,	/* name  of this object */
    CODE		type		/* type of object       */

 )
    {
      struct OBJECT_INFO * objectInfo = (struct OBJECT_INFO *) objectInfoin;
        if (s_equal (objectName, (*objectInfo).name))
        {
            (*objectInfo).type = type;
            return (id);
        }
        else
           return (NULL);
    }


/*
*	xco_find_next:  find next member 
*	(for looping through and operating on each member of a collection)
*/

FUNCTION VOID BRIDGE2_NAME(xco_find_next)
(
 TAEINT *colxid,	/* in: collection id */
 FORSTR *memnam,	/* in/out: name of member object */
 TAEINT *objid,	/* in/out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status	/* out: SUCCESS or error code */
)

	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 	  static TAEINT lastcoid = 0;     /\* colxid of previous call *\/ */
/*     	  struct OBJECT_INFO	objectInfo;	/\* context block	*\/ */
/* 	  TEXT	objName[STRINGSIZ+1]; */

/* 	  objectInfo.name = objName; */
/* 	  if ((*objid == 0) | (*colxid != lastcoid)) /\* if colxid changed *\/ */
/* 	     { */
/* 	     objectInfo.first = TRUE;	/\* start search from beginning *\/ */
/* 	     lastcoid = *colxid; */
/* 	     objectInfo.name[0] = EOS; */
/* 	     } */
/* 	  else  */
/* 	     { */
/* 	     objectInfo.first = FALSE; */
/* 	     s_for2c(memnam, objectInfo.name, 0); */
/* 	     s_strip(objectInfo.name); */
/* 	     } */
/* 	  *objid = code = (CODE) Co_ForEach ((struct COLLECTION*)*colxid,  */
/* 					     getnextfcn, (GENPTR) &objectInfo);  */
/* 	  *type = objectInfo.type; */
/* 	  s_c2for (objectInfo.name, memnam, 0); */
/* 	  if (code != 0) */
/* 		  *status = SUCCESS; */
/* 	  else */
/* 		  *status = FP_EOP; /\* no more objects in collection *\/ */
/* 	  return; */
	}

/* FUNCTION static void* getnextfcn  */
/* ( */
/*     Id			id,		/\* id of this object	*\/ */
/*     GENPTR		objectInfoin,	/\* context	        *\/ */
/*     TEXT		*objectName,	/\* name  of this object *\/ */
/*     CODE		type		/\* type of object       *\/ */

/*  ) */
/*     { */

/*       struct OBJECT_INFO * objectInfo = (struct OBJECT_INFO *) objectInfoin; */
/*         if ((*objectInfo).first == TRUE) */
/* 	{ */
/* 	/\* */
/* 	 * prevent duplicate adjacent member names going forever */
/* 	 *\/ */
/*             if (s_equal (objectName, (*objectInfo).name)) return (NULL); */
/* 	    (*objectInfo).first = FALSE; */
/*             (*objectInfo).type = type; */
/*             (*objectInfo).name = objectName; */
/* 	    return (id); */
/*         } */
/*         if (s_equal (objectName, (*objectInfo).name)) */
/*         { /\* get next one and return it *\/ */
/* 	    (*objectInfo).first = TRUE; */
/* 	    return (NULL); */
/* 	} */
/*         else */
/*           return (NULL); */
/*     } */



/*
*	xco_free:  free a collection 
*	TBDTBD.  Should a check be made to prevent freeing a non-empty
*		collection?
*/

FUNCTION VOID BRIDGE2_NAME(xco_free)
(
 TAEINT *colxid,		/* in: collection id */
 TAEINT *status		/* out: SUCCESS or error code */
 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		Co_Free_Nocall((struct COLLECTION*)*colxid); */
/* 		*status = SUCCESS; */
/* 		return; */
	}


/*
*	xco_new:  create an empty collection
*/

FUNCTION VOID BRIDGE2_NAME(xco_new)
(	
 TAEINT *colxid,		/* out: collection id */
 TAEINT *flags,		/* in: currently ignored */
 TAEINT *status		/* out: SUCCESS or error code */
)

	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/*                 /\* We should not assume a pointer is sizeof(TAEINT) *\/ */
/* 		*colxid = (TAEINT)Co_New (*flags); */
/* 		*status = SUCCESS; */
/* 		return; */
	}


/*
*	xco_read_file:  read collection of vm objects from a tae plus resource file.
*/

FUNCTION VOID BRIDGE2_NAME(xco_read_file)
(	
 TAEINT *colxid,		/* in: collection id */
 FORSTR *fspec,		/* in: file specification of resource file */
 TAEINT *mode,		/* in: to be used for the vm objects created */
 TAEINT *status		/* out: SUCCESS or error code */
)
	{

	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(fspec, cname, 0); */
/* 		s_strip(cname); */
/*   		*status = Co_ReadFile((struct COLLECTION*)*colxid, cname,  */
/* 				      *mode); */
/* 		return; */
	}



/*
*	xco_write_file:  write collection of vm objects to a tae plus resource file
*/

FUNCTION VOID BRIDGE2_NAME(xco_write_file)
(	
 TAEINT *colxid,		/* in: collection id */
 FORSTR *fspec,		/* in: file specification of resouce file */
 TAEINT *status		/* out: SUCCESS or error code */
)

	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		s_for2c(fspec, cname, 0); */
/* 		s_strip(cname); */
/* 		code = Co_WriteFile((struct COLLECTION*)*colxid, cname); */
/* 		return; */
	}

