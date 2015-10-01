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
 *	Restricted allocation package.
 *
 *	This package allows the caller to "package" linked data structures
 *	into a block of memory and send the block to another process.  The
 *	receiving process can re-construct the pointers.
 *
 *	To use the R package, the caller defines a storage area of type
 *      ALIGN, dimensioned according to how much storage is needed.
 *	The macro STORAGE in RESINC.INC simplifies the allocation of
 *	the storage block.  
 *
 *	The area is initialized with the r_init function; it builds a 
 *	control block at the beginning of the area to keep track of
 * 	allocation.  The caller can allocate storage from the area
 *	using the r_alloc function -- it is similar to calloc but 
 *	allocates within the storage block.
 *
 *	The r_setrel and r_setabs functions allow for conversions of
 *	pointers in the storage block to/from a "block relative" format.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	18-feb-84	Use COMPACT_COUNT to keep parblk format same
 *			from v1.1 to v1.2....palm
 *	15-sep-86	Explain why r_dealloc is nothing...palm
 *	30-mar-87	Change R_BLOCK to use COMPACT_UCOUNT rather than
 *			COMPACT_COUNT so we get up to 64Kb in a block
 *			while maintaining compatibility with existing
 *			PAR files...palm
 * 10-apr-92	Prevent r_init and r_newsize from allocating a pool so big 
 *		that the pool and the parblk header do not fit into a record of
 *		size MAX_COMPACT_UCOUNT. When we change the parblk
 *		size field to a LONG, this will no longer be necessary...krw
 * 05-may-92    Changed pool and parblk fields: MAX_COMPACT_UCOUNT to 
 *		LARGE_P_BYTES and COMPACT_UCOUNT to LONG and removed previous
 *		temporarily work around...tpl
 *		
 *
 */


#include <taeconf.inp>
#include <resinc.inc>
#include <parblk.inc>
#include "taeintproto.h"

/*
 *	Control block for keeping up with available memory within the
 *	caller's block.  R_BLOCK is always allocated at the beginning
 *	of the caller's block.  
 * 
 *      Before TAE V5.2, the fields in the R_BLOCK and parblk header's
 *      are COMPACT_UCOUNT. This change invalidated all pre-5.2 par blocks.
 */

    struct R_BLOCK 
	{
	LONG current;		/* first available ALIGN index	*/
	LONG size;		/* remaining ALIGN elements	*/
	LONG top;		/* highest+1 index allocated	*/
	};

#define NULL_FLAG  LARGE_P_BYTES        /* means NULL as relative offset */


/*
 * 	r_init.  Initializes a block of storage so that other 
 * 	r_ calls may be executed.  A block of storage is an array
 *	of ALIGN.
 */

    FUNCTION VOID r_init(

	ALIGN	*storage,	/* output: block of memory 	*/
	FUNINT	bytes		/* input: bytes in storage	*/
    )

	{
	struct  R_BLOCK	*r;
	COUNT	i;
	LONG	size;		/* size of pool in ALIGNs	*/

	r = (struct R_BLOCK *) storage;		/* point to control block  */
	i = ALIGNS(sizeof(struct R_BLOCK));	/* ALIGNs needed for r	   */
	size = bytes/sizeof(ALIGN) - i;	/* remaining storage elements*/

	/* 
	   We clip the pool size to the max we can track with 
	   LARGE_P_BYTES.  We go one less than LARGE_P_BYTES
	   because LARGE_P_BYTES is itself a flag for NULL_VALUE.
	*/

	(*r).size = min (LARGE_P_BYTES-1, size);	
	(*r).current = i;
	(*r).top = i;				/* highest+1 allocated 	*/

	}

/*
 *	r_alloc.  Allocate restricted storage.
 */
    FUNCTION GENPTR r_alloc(
    
	ALIGN	*s,			/* input/output: storage block	*/
	FUNINT	bytes			/* input: bytes to allocate	*/
    )

	{
	COUNT	nunits;			/* ALIGNS to allocate		*/
	GENPTR	p;			/* returned value		*/
	struct R_BLOCK	*r;		/* control block		*/

	r = (struct R_BLOCK *)s;	/* control block is at begin	*/
	nunits = ALIGNS(bytes);		/* ALIGN elements to allocate	*/
	if (nunits <= (*r).size)	/* enough room?			*/
	    {
	    p = (GENPTR) (s + (*r).current); /* currently availble slot */
	    (*r).size -= nunits;	/* new remaining elements	*/
	    (*r).current += nunits;	/* update available slot ptr	*/
	    (*r).top = (*r).current;	/* TBD: allow for de-allocation */
	    return(p);
	    }
	return(NULL);			/* no space available		*/
	}

/* 
 *	r_dealloc.   De-allocate within restricted storage.
 *
 *
 *      TBD: Restricted package deallocation.  
 *	(For now, not implemented.)
 *
 */

    FUNCTION VOID r_dealloc(

    ALIGN	*s,			/* storage area			*/
    GENPTR	p			/* block to de-allocate		*/
    )

    {
    return;				/* nothing for now		*/
    }

/*
 *	r_top.  Returns GENPTR to the highest+1 byte allocated so far.
 *	This is nice for finding the "logical length" of storage area.
 *
 */

    FUNCTION GENPTR r_top(
   	
	ALIGN	*s			/* storage area			*/
    )
	{
	struct R_BLOCK *r;		/* control block		*/

	r = (struct R_BLOCK *) s;	/* control block at begin	*/
	return ((GENPTR)  (s + (*r).current)); 
	}	

/*
 *	r_setrel.  Makes a pointer "relative" to its storage block so that
 *	eventually, the pointer can be re-constructed by r_setabs.
 *
 *	The strategy here is to overlay a value of type COUNT on top of the
 *	pointer.  This should be portable if COUNTs have a compatible 
 *	alignment with pointers and the number of bits is less than or equal.
 *
 *	This is called by the SET_REL macro for convenience.
 */

    FUNCTION VOID r_setrel(

	ALIGN	*s,		/* input: storage area name		*/
	GENPTR	pp,		/* input: address of value to fix	*/
	GENPTR   p		/* input: pointer value to make rel	*/
    )

    {
    LONG *i;			/* pointer to LONG variable	*/

    i = (LONG *) pp;		/* address of pointer to fix		*/
    if (p == NULL)
	*i = NULL_FLAG;		/* NULL pointers become a special flag	*/
    else
        *i = ((ALIGN *) p) - s;	/* offset in ALIGNs			*/
    }    

/* 
 *	r_setabs.  Takes a pointer previously made relative by r_setrel and
 *	re-constructs an absolute pointer.  r_setabs is called by the
 *	SET_ABS macro for convenience.
 *
 *	r_setabs is usually called after the storage block has been transfered
 *	to a different address space.
 */

    FUNCTION GENPTR r_setabs(

	ALIGN	*s,			/* input: storage block		*/
	GENPTR	pp			/* input: address of pointer	*/
    )
    {
    LONG *i;				/* address of relative pointer	*/

    i = (LONG *) pp;			/* get pointer in usuable form	*/
    if (*i == NULL_FLAG)
	return(NULL);			/* negative is flag for NULL	*/
    else
	return( (GENPTR) (s + *i));	/* positive is index in s	*/
    }

/*
 *	r_newsiz.  When a restricted block is read into another address space
 *	the block that it's read into may have a different size.
 *	r_newsiz must be called to notify r_ that the pool of restricted
 *	memory has a different length. 
 */

    FUNCTION VOID r_newsiz(

    ALIGN	*s,		/* in: address of pool			*/
    COUNT	bytes		/* in: number of bytes in pool		*/
    )

    {
    struct R_BLOCK	*r;	
    COUNT		i;
    LONG		size;
    LONG		bytes2alloc;

   /*
      Calculate minimum of bytes and the largest size so that an entire
      parblk will fit into a record of size LONG 
   */
    bytes2alloc = min(bytes, (LARGE_P_BYTES - 
	sizeof(struct PARBLK) - P_BYTES - 8)); /* -8 for align safety */

    r = (struct R_BLOCK *) s;
    i = bytes2alloc/sizeof(ALIGN);	/* number of ALIGNs available	*/
    size = i - (*r).current ;		/* new size remaining		*/

    /* 
       We clip the pool size to the max we can track with 
       LARGE_P_BYTES.  We go one less than LARGE_P_BYTES
       because LARGE_P_BYTES is itself a flag for NULL_VALUE.
    */

    (*r).size = min (LARGE_P_BYTES-1, size);	/* clip at LARGE_P_BYTES   */
    }
