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



/* FORTRAN to "C" outer bridge */
/*
 *	TAE version 4 XCO_ bridges for manipulation of collections of 
 *	Vm Objects
 *	FORTRAN-callable.
 *
 *	CHANGE LOG:
 *	6/22/89 Initial Cut...rsg/AS
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"forstr.inp"	/* fortran-77 string struct		*/
#include "taeintproto.h"
FUNCTION VOID BRIDGE2_NAME(xco_add)
(	
 TAEINT *c,	/* in: id of the collection to add the object to */
 Id     *objid,	/* in: id of the object to be added to collection */
 FORSTR *memnam,	/* in: name ofthe member object */
 TAEINT *type,	/* in: type of the object */
 TAEINT *status	/* out: SUCCESS or error code */
);
FUNCTION VOID BRIDGE2_NAME(xco_remove)
(
 TAEINT *colxid,		/* in: collection id */
 FORSTR *memnam,		/* in: name of the member to be removed */
 TAEINT *status			/* out: SUCCESS or error code */
 );
FUNCTION VOID BRIDGE2_NAME(xco_find)
(
 TAEINT *colxid,	/* in: collection id */
 FORSTR *memnam,	/* in: name of member object */
 TAEINT *objid,	/* out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status	/* out: SUCCESS or error code */
 );
FUNCTION VOID BRIDGE2_NAME(xco_find_next)
(
 TAEINT *colxid,	/* in: collection id */
 FORSTR *memnam,	/* in/out: name of member object */
 TAEINT *objid,	/* in/out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status	/* out: SUCCESS or error code */
 );
FUNCTION VOID BRIDGE2_NAME(xco_free)
(
 TAEINT *colxid,		/* in: collection id */
 TAEINT *status		/* out: SUCCESS or error code */
 );
FUNCTION VOID BRIDGE2_NAME(xco_new)
(	
 TAEINT *colxid,		/* out: collection id */
 TAEINT *flags,		/* in: currently ignored */
 TAEINT *status		/* out: SUCCESS or error code */
	);
FUNCTION VOID BRIDGE2_NAME(xco_read_file)
(	
 TAEINT *colxid,		/* in: collection id */
 FORSTR *fspec,		/* in: file specification of resource file */
 TAEINT *mode,		/* in: to be used for the vm objects created */
 TAEINT *status		/* out: SUCCESS or error code */
	);
FUNCTION VOID BRIDGE2_NAME(xco_write_file)
(	
 TAEINT *colxid,		/* in: collection id */
 FORSTR *fspec,		/* in: file specification of resouce file */
 TAEINT *status		/* out: SUCCESS or error code */
	);


/*
*	xco_add:  add member to collection
*/

FUNCTION VOID BRIDGE1_NAME(xco_add) 
(
 
 TAEINT *colxid,	/* in: id of the collection to add the object to */
 TAEINT *objid,	/* in: id of the object to be added to collection */
 TEXT   *memnam,	/* in: text name ofthe member object */
 TAEINT *type,	/* in: type of the object */
 TAEINT *status,	/* out: SUCCESS or error code */
 STRLEN memnaml
 )
	{
	  // This is actually *broken* on 64-bit systems, a 32 bit
	  // integer can't be cast to a 64 bit pointer. Leave this
	  // in place to get a clean compile, but trigger an error
	  // if this is actually called.

	  printf("Calling a broken function. This can't be fixed on a 64 bit system");
	  abort();
/* 		FORSTR memnamd; */

/* 		memnamd.length = GETLEN(memnaml); */
/* 		memnamd.pointer = memnam; */

/* 		BRIDGE2_NAME(xco_add) (colxid, objid, &memnamd, type, status); */

/* 		return; */
	}


/*
*	xco_remove:  remove a member from the collection 
*/

FUNCTION VOID BRIDGE1_NAME(xco_remove)
(	
 TAEINT *colxid,		/* in: collection id */
 TEXT   *memnam,		/* in: name of the member to be removed */
 TAEINT *status,		/* out: SUCCESS or error code */
 STRLEN memnaml
)

	{
		FORSTR memnamd;

		memnamd.length = GETLEN(memnaml);
		memnamd.pointer = memnam;
		BRIDGE2_NAME(xco_remove) (colxid, &memnamd, status);
	}



/*
*	xco_find:  find specific member
*/

	FUNCTION VOID BRIDGE1_NAME(xco_find) 
(
	
 TAEINT *colxid,	/* in: collection id */
 TEXT   *memnam,	/* in: text name of member object */
 TAEINT *objid,	/* out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status,	/* out: SUCCESS or error code */
 STRLEN memnaml
)
	{

		FORSTR memnamd;

		memnamd.length = GETLEN(memnaml);
		memnamd.pointer = memnam;

		BRIDGE2_NAME(xco_find) (colxid, &memnamd, objid, type, status);

		return;
	}


/*
*	xco_find_next:  find next member 
*	(for looping through and operating on each member of a collection)
*/

FUNCTION VOID BRIDGE1_NAME(xco_find_next) 
(	
 TAEINT *colxid,	/* in: collection id */
 TEXT   *memnam,	/* out: text name of next member object */
 TAEINT *objid,	/* out: id of the object found */
 TAEINT *type,	/* out: type of object specified in xcadd call */
 TAEINT *status,	/* out: SUCCESS or error code */
 STRLEN memnaml
)

	{
		FORSTR memnamd;

		memnamd.length = GETLEN(memnaml);
		memnamd.pointer = memnam;

		BRIDGE2_NAME(xco_find_next) ( colxid, &memnamd, objid, type, status);

		return;
	}



/*
*	xco_free:  free a collection 
*/

FUNCTION VOID BRIDGE1_NAME(xco_free) 
(	
	TAEINT *colxid,		/* in: collection id */
	TAEINT *status		/* out: SUCCESS or error code */

	)
	{
		BRIDGE2_NAME(xco_free) (colxid,  status);

		return;
	}


/*
*	xco_new:  create an empty collection
*/

/* 	FUNCTION VOID BRIDGE1_NAME(xco_new) (colxid, flags, status) */
	
/* 	TAEINT *colxid;		/\* out: collection id *\/ */
/* 	TAEINT *flags;		/\* in: currently ignored *\/ */
/* 	TAEINT *status;		/\* out: SUCCESS or error code *\/ */

/* 	{ */

/* 		BRIDGE2_NAME(xcnew) (colxid, flags, status); */

/* 		return; */
/* 	} */


/*
*	xco_read_file:  read collection of vm objects from a tae plus resource file.
*/

FUNCTION VOID BRIDGE1_NAME(xco_read_file) 
(	
	TAEINT *colxid,		/* in: collection id */
	TEXT   *fspec,		/* in: file specification of resource file */
	TAEINT *mode,		/* in: to be used for the vm objects created */
	TAEINT *status,		/* out: SUCCESS or error code */
	STRLEN fspecl

	)
	{
		FORSTR fspecd;

		fspecd.length = GETLEN(fspecl);
		fspecd.pointer = fspec;

		BRIDGE2_NAME(xco_read_file) (colxid, &fspecd, mode, status);

		return;
	}



/*
*	xco_write_file:  write collection of vm objects to a tae plus resource file
*/

/* 	FUNCTION VOID BRIDGE1_NAME(xco_write_file) (colxid, fspec, status, fspecl) */
	
/* 	TAEINT *colxid;		/\* in: collection id *\/ */
/* 	TEXT   *fspec;		/\* in: file specification of resouce file *\/ */
/* 	TAEINT *status;		/\* out: SUCCESS or error code *\/ */
/* 	STRLEN fspecl; */

/* 	{ */
/* 		FORSTR fspecd; */

/* 		fspecd.length = GETLEN(fspecl); */
/* 		fspecd.pointer = fspec; */

/* 		BRIDGE2_NAME(xcwfil) (colxid, &fspecd, status); */

/* 		return; */
/* 	} */


