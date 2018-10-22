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



/*	"oldmodules.c"   -- this compiles previous 
	versions of selected TAE modules in order
	to read old par files.
*/
/*
 * Change Log
 * 22-oct-92	Prototyping tae_alloc is unecessary and Ultrix 4.3 does not
 *		like it...rt
 */

/* Include the internal prototypes before redefining, so it doesn't */
/* don't get re-included and define the functions incorrectly */

#include "taeintproto.h"

/*	the following maps Vm names to old_ so the "old" Vm
	package can live in the same executable along
	with the newly compiled vm package
*/

#define Vm_CopyVar old_Vm_CopyVar
#define Vm_CopyValue old_Vm_CopyValue
#define Vm_CopyValid old_Vm_CopyValid
#define Vm_SpCopyVar old_Vm_SpCopyVar
#define Vm_MoveSymtab old_Vm_MoveSymtab
#define Vm_ReadFromDisk old_Vm_ReadFromDisk
#define Vm_ReadVm old_Vm_ReadVm
#define Vm_WriteToDisk old_Vm_WriteToDisk
#define Vm_WriteVm old_Vm_WriteVm
#define Vm_ParblkOut old_Vm_ParblkOut
#define Vm_st2blk old_Vm_st2blk
#define Vm_VarMove old_Vm_VarMove
#define Vm_ValueMove old_Vm_ValueMove
#define Vm_New old_Vm_New
#define Vm_SetIntg old_Vm_SetIntg
#define Vm_SetReal old_Vm_SetReal
#define Vm_SetString old_Vm_SetString
#define Vm_SetOneString old_Vm_SetOneString
#define Vm_SetShortString old_Vm_SetShortString
#define Vm_Alloc old_Vm_Alloc
#define Vm_Save old_Vm_Save
#define Vm_AllocValue old_Vm_AllocValue
#define Vm_AllocVar old_Vm_AllocVar
#define Vm_FreeTable old_Vm_FreeTable
#define Vm_FreeValue old_Vm_FreeValue
#define Vm_FreeVar old_Vm_FreeVar
#define Vm_FindStVar old_Vm_FindStVar
#define Vm_ValueSize old_Vm_ValueSize
#define Vm_AddVar old_Vm_AddVar
#define Vm_Free old_Vm_Free
#define Vm_SetMin old_Vm_SetMin
#define Vm_SetMax old_Vm_SetMax
#define Vm_SetStringLength old_Vm_SetStringLength
#define Vm_SetParmPage old_Vm_SetParmPage
#define Vm_SetValidIntg old_Vm_SetValidIntg
#define Vm_SetValidReal old_Vm_SetValidReal
#define Vm_SetValidString old_Vm_SetValidString
#define Vm_SetOneValidString old_Vm_SetOneValidString
#define Vm_AllocValid old_Vm_AllocValid
#define Vm_ValidSize old_Vm_ValidSize

#define Vm_CopyVarToVm old_Vm_CopyVarToVm 
#define Vm_ForEach old_Vm_ForEach 
#define Vm_Parblk old_Vm_Parblk 
#define Vm_Copy old_Vm_Copy 
#define f_curpos old_f_curpos		/* avoid double def */


#include	"taeconf.inp"		

#undef		MAXVAL
#define		MAXVAL		100	/* previous value */
#undef		NAMESIZ
#define		NAMESIZ		8

/*	the following get the old definitions of VARIABLE and
	PARBLK.   I_SYMTAB, I_PARBLK definitions will prevent
	the actual includes in the source module from
	doing anything
*/

#include	"oldsymtab.inc"
#include	"oldparblk.inc"

/*	include the old modules 	*/

#include	"oldtaeutil.c"
#include	"oldtranssym.c"
#include	"oldvmcopy.c"
#include	"oldvmmisc.c"
#include	"oldvmfile.c"
#include	"oldvmparmgen.c"
#include	"oldvmvalid.c"
#include	"oldcoll.c"
