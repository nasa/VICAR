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



/*****************************************************************************
 *
 *    Function prototypes for supported VM and CO functions.  This file is
 * only used by C (whatever flavor) and not C++.  The C++ declarations and
 * use of some of these functions is different enough that trying to use
 * this file for C and C++ at this time.  symbol.h has many of these
 * declarations (in a slightly different form).  When you need to add
 * a declaration for a C function for use with C++, modify symbol.h
 * 
 * Change Log:
 * 07-jul-93 Initial...rt
 *
 *****************************************************************************/

        /* Make multiple includes of this file safe. */
#ifndef _TAE_VmCoProto_h
#define _TAE_VmCoProto_h

#ifndef __cplusplus    /* These declarations only work with C for now */
#include <vminc.inc>

/*	The collection context block has the following format.
 *	By pointing to the last element in the LINKed list,
 *	we can access the last element (for additions) or
 *	the first element (for streaming).
 */


struct COLLECTION
    {
    struct LINK *last;			/* ptr to last entry */
    CODE	flags;			/* OR of ...	     */

#define C_DUPLICATES  0x0001		/* duplicates ok 	   */
#define C_ALPHA	      0x0002		/* maintain in alpha order */

    COUNT	count;
    };

/* Supported VM functions */
    VOID Vm_Call _TAE_PROTO ((Id vmid));
    VOID Vm_Copy _TAE_PROTO ((struct VM_STRUCT	*target, 
        struct VM_STRUCT *source));
    CODE Vm_DynTutor _TAE_PROTO ((Id vmid, TEXT pdfname[], FUNINT pdfset));
    struct VARIABLE * Vm_Find _TAE_PROTO ((Id vmid, TEXT name[]));
    struct VARIABLE * Vm_FindVar _TAE_PROTO ((Id vmid, const TEXT *name));
    CODE Vm_ForEach _TAE_PROTO ((Id vmid, 
        CODE (*aFunction)(struct VARIABLE  *, GENPTR), GENPTR contextArg));
    CODE Vm_FormatVar _TAE_PROTO ((Id vmid, TEXT line[], COUNT length));
    VOID Vm_Free _TAE_PROTO ((struct VM_STRUCT* vmid));
    CODE Vm_GetAttribute _TAE_PROTO ((Id vmid, TEXT name[], CODE *type,
                                      COUNT *n, BOOL *dflt, CODE *access));
    VOID Vm_GetHostError _TAE_PROTO ((Id vmid, CODE *vmcode));
    CODE Vm_GetValidIntg _TAE_PROTO ((Id vmid, TEXT name[], FUNINT *count,
                                      TAEINT **ilow, TAEINT **ihigh));
    CODE Vm_GetValidReal _TAE_PROTO ((Id vmid, TEXT name[], FUNINT *count,
                                      TAEFLOAT **rlow, TAEFLOAT **rhigh));
    CODE Vm_GetValidString _TAE_PROTO ((Id vmid, TEXT name[], FUNINT *count,
                                        TEXT ***vector));
    VOID Vm_InitFormat _TAE_PROTO ((Id vmid));
    struct VM_STRUCT *Vm_New _TAE_PROTO ((FUNINT mode));
    FILE * Vm_OpenStdout _TAE_PROTO ((Id vmid));
    CODE Vm_ReadFromDisk _TAE_PROTO ((Id vmid, TEXT spec[]));
    CODE Vm_ReadFromTM _TAE_PROTO ((Id vmid));
    CODE Vm_SetIntg _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count,
                                 TAEINT intg[], FUNINT mode));
    CODE Vm_SetMax _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count));
    CODE Vm_SetMin _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count));
    CODE Vm_SetNextMenu _TAE_PROTO ((Id vmid, TEXT name[]));
    CODE Vm_SetParmPage _TAE_PROTO ((Id vmid, TEXT name[], BOOL flag));
    CODE Vm_SetReal _TAE_PROTO ((Id, TEXT name[], FUNINT count,
                                 TAEFLOAT *real, FUNINT mode));
    CODE Vm_SetString _TAE_PROTO ((Id, TEXT name[], FUNINT count,
                                   TEXT *vector[], FUNINT mode));
    CODE Vm_SetStringLength _TAE_PROTO ((Id vmid, TEXT name[], FUNINT strlen));
    CODE Vm_SetTCLVar _TAE_PROTO ((Id vmid));
    CODE Vm_SetValidIntg _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count,
                                      TAEINT *ilo, TAEINT *ihi));
    CODE Vm_SetValidReal _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count,
                                      TAEFLOAT rlow[], TAEFLOAT rhigh[]));
    CODE Vm_SetValidString _TAE_PROTO ((Id vmid, TEXT name[], FUNINT count,
                                        TEXT *vector[]));
    CODE Vm_WriteToDisk _TAE_PROTO ((Id vmid, TEXT spec[]));


/* Supported CO functions */
    VOID Co_Add _TAE_PROTO ((struct COLLECTION 	*aCollection, Id anObject, 
			     TEXT memName[], CODE type));
    Id Co_Find _TAE_PROTO (( struct COLLECTION	*aCollection, TEXT memName[]));
void * Co_ForEach _TAE_PROTO ((struct COLLECTION* aCollection, void* (*aFunction)(Id Obj, GENPTR context, TEXT* name, CODE type),
                                 GENPTR contextArg));
VOID Co_Free _TAE_PROTO ((struct COLLECTION * aCollection, VOID (*freeFunction)(Id,CODE) ));
    Id Co_New _TAE_PROTO ((CODE flags));
    CODE Co_ReadFile _TAE_PROTO (( struct COLLECTION* aCollection, TEXT fileSpec[], CODE mode));
    Id Co_Remove _TAE_PROTO ((struct COLLECTION	*aCollection,  TEXT memName[]));
    CODE Co_WriteFile _TAE_PROTO (( struct COLLECTION *aCollection, TEXT filespec[]));

#endif /* __cplusplus */

/* DO NOT ADD ANYTHING AFTER THIS endif */
#endif 	/* _TAE_VmCoProto_h */
