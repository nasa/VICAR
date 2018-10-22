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



/********************************************************************
* 
* access_c_structs - this file of functions is used to give
*	a proper function call entry to do what most C programmers
*	would just do with a macro--extract fields from a TAE struct.
*
* Change Log:
*	9/15/88	Initial (created for Ada bridge)...nci
*       1/03/91 Added Vm_Extract_VALIDCOUNT...cew
*       7/22/92 PR1519: Label all functions as ADA_FUNCTION...kbs
*
********************************************************************/

#include 	"taeconf.inp"
#include 	"parblk.inc"
#include	"symtab.inc"
#include "taeintproto.h"

ADA_FUNCTION void Vm_Extract_CLASS (struct VARIABLE *vptr, int *class)
{
  *class = (*vptr).v_class;
}

ADA_FUNCTION void Vm_Extract_NAME (struct VARIABLE *vptr, char *name)
{
  s_copy ((*vptr).v_name, name);
}

ADA_FUNCTION void Vm_Extract_TYPE (struct VARIABLE *vptr, int *vtype)
{
  *vtype = (*vptr).v_type;
}

ADA_FUNCTION void Vm_Extract_COUNT (struct VARIABLE *vptr, int *count)
{
  *count = (*vptr).v_count;
}

ADA_FUNCTION void Vm_Extract_IVAL (struct VARIABLE *vptr, int index, int *ival)
{
  *ival = IVAL(*vptr, index);
}

ADA_FUNCTION void Vm_Extract_RVAL (struct VARIABLE *vptr, int index, TAEFLOAT *rval)
{
  *rval = RVAL(*vptr, index);
}

ADA_FUNCTION void Vm_Extract_SVAL (struct VARIABLE *vptr, int index, char *sval)
{
  s_copy (SVAL(*vptr, index), sval); 
}

ADA_FUNCTION void Vm_Extract_VALIDCOUNT (struct VARIABLE *vptr, int *count)
{
  struct R_VALID	*rvalid;
  struct I_VALID	*ivalid;
  struct S_VALID	*svalid;
  
  if ((*vptr).v_type == V_REAL)
    {
      rvalid = (struct R_VALID *)(*vptr).v_valid;
      if (rvalid != (struct R_VALID *) NULL)
	(*count) = (*rvalid).count;
    }
  else if ((*vptr).v_type == V_INTEGER)
    {
      ivalid = (struct I_VALID *)(*vptr).v_valid;
      if (ivalid != (struct I_VALID *) NULL)
	(*count) = (*ivalid).count;
    }
  else if ((*vptr).v_type == V_STRING)
    {
      svalid = (struct S_VALID *)(*vptr).v_valid;
      if (svalid != (struct S_VALID *) NULL)
	(*count) = (*svalid).count;
    }
  else
    *count = 0;
}
