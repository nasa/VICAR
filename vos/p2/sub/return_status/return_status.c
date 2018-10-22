/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <string.h>

#include "return_status.h"

typedef	struct	{	char	*FullName;
			char	*Nmemonic;
			int	Value;
		} Tbl_typ;

static Tbl_typ	Codes[] = {
	{"NORMAL",		"N",		RTN_C_NORMAL },
	{"WARNING",		"W",		RTN_C_WARNING },
	{"INFO",		"I",		RTN_C_INFO },
	{"ERROR",		"E",		RTN_C_ERROR },
	{"DEBUG",		"D",		RTN_C_DEBUG },
	{"CRITICAL",		"C",		RTN_C_CRITICAL },
	{"RECOVER",		"R",		RTN_C_RECOVER },
	{"FATAL",		"F",		RTN_C_FATAL },
	{ "N/A",		"?",		-1} };

static Tbl_typ	SubSys[] = {
	{"Generic",		"GNRC",		RTN_S_GENERIC },
	{"General",		"GNRL",		RTN_S_GENERAL },
	{"VICAR RTL",		"VIC",		RTN_S_VICAR },
	{"Catalog_API",		"CAT",		RTN_S_CAT_API },
	{"Label_API",		"LBL",		RTN_S_LBL_API },
	{"Parameter",		"PARM",		RTN_S_PARAM },
	{"SFDU",		"SFDU",		RTN_S_SFDU },
	{"CHDO",		"CHDO",		RTN_S_CHDO },
	{"Telemetry",		"TLM",		RTN_S_TLM_PKT },
	{ "N/A",		"?",		-1} };

static Tbl_typ	Errors[] = {
	{"Undefined",			"Undef",	RTN_V_UNDEF },
	{"Invalid feature",		"Invld",	RTN_V_INVALID },
	{"Unsupported feature",		"Unsup",	RTN_V_UNSUPPORTED },
	{"Time-out expired",		"TmOut",	RTN_V_TIMEOUT },
	{"No more data available",	"EOD",		RTN_V_EOD },
	{"Allocate resource",		"Alloc",	RTN_V_ALLOCATE },
	{"Open a resource",		"Open",		RTN_V_OPEN },
	{"Create a resource",		"Creat",	RTN_V_CREATE },
	{"Attach a resource",		"Attch",	RTN_V_ATTACH },
	{"Resource exists",		"Exist",	RTN_V_EXISTS },
	{"Initialization",		"Init",		RTN_V_INITIALIZE },
	{"VICAR Run Time Library",	"V_RTL",	RTN_V_VICAR_RTL },
	{"Missing Value",		"M_Val",	RTN_V_MISSING_VALUE },
	{"Pathname Syntax",		"Path",		RTN_V_BAD_PATH },
	{"End of File",			"EOF",		RTN_V_EOF },
	{"Insufficient Data",		"Insuf",	RTN_V_INSUFF_DATA },
	{"Insufficient Memory",		"NoMem",	RTN_V_INSUFF_MEMORY },
	{"Too Much Data",		"Mucho",	RTN_V_MUCHO_DATA },
	{"Missing Data",		"NoDat",	RTN_V_MISSING_DATA },
	{"Array index bounds",		"Idx",		RTN_V_BOUND_ERROR },
	{"I/O error",			"IoErr",	RTN_V_IO_ERROR },
	{"Function not implemented",	"NoImp",	RTN_V_NOT_IMPLEMENTED },
	{"Data SFDU record",		"S_Dat",	RTN_V_SFDU_DATA },
	{"Informational SFDU record",	"S_Inf",	RTN_V_SFDU_INFO },
	{"Delimiter SFDU record",	"S_Del",	RTN_V_SFDU_DELIM },
	{"Error SFDU record",		"S_Err",	RTN_V_SFDU_ERROR },
	{"Unknown SFDU length",		"U_len",	RTN_V_SFDU_UNKWN_LEN},
	{"Resource not available",	"Avail",	RTN_V_NOT_AVAILABLE },
	{"Invalid argument value",	"InArg",	RTN_V_INVLD_ARG },
	{"Item not found",		"NoFnd",	RTN_V_NOT_FOUND },
	{"Inconsistent Usage",		"Incst",	RTN_V_INCONSISTENT },
	{"Unexpected Result",		"UnExp",	RTN_V_UNEXPECTED },
	{ "N/A",			"?",		-1} };

static char	MsgBuffer[256];

/******************************************************************************
 *
 *				err_msg
 *
 *	Formats a text string describing the status assocated with the
 *  specified 'ErrorNumber'.
 *
 *****************************************************************************/
const char	*err_msg(
	int	ErrorNumber,
	int	CodeMask,
	int	SubSysMask,
	int	NumberMask)
{ int	Idx;
  char	Temp[32];

  memset(MsgBuffer,0,sizeof(MsgBuffer));


  if (CodeMask)
  { for (Idx=0; (Codes[Idx].Value != -1 &&
                 RTN_CODE(ErrorNumber) != Codes[Idx].Value); Idx++);

    if (CodeMask & RTN_M_STRING)
       strcpy(Temp,Codes[Idx].FullName);
    else if (CodeMask & RTN_M_NMEMONIC)
         strcpy(Temp,Codes[Idx].Nmemonic);
    else if (CodeMask & RTN_M_VALUE)
            sprintf(Temp,"%d",RTN_CODE(ErrorNumber));

    strcat(MsgBuffer,Temp);

    if (SubSysMask || NumberMask) strcat(MsgBuffer,": ");
  }


  if (SubSysMask)
  { for (Idx=0; (SubSys[Idx].Value != -1 &&
                 RTN_SUBSYS(ErrorNumber) != SubSys[Idx].Value); Idx++);

    if (SubSysMask & RTN_M_NMEMONIC)
       sprintf(Temp,"[%s]",SubSys[Idx].Nmemonic);
    else if (SubSysMask & RTN_M_STRING)
            sprintf(Temp,"[%s]",SubSys[Idx].FullName);
    else if (SubSysMask & RTN_M_VALUE)
            sprintf(Temp,"[%d]",RTN_SUBSYS(ErrorNumber));

    strcat(MsgBuffer,Temp);

    if (NumberMask) strcat(MsgBuffer," ");
  }


  if (NumberMask)
  { for (Idx=0; (Errors[Idx].Value != -1 &&
                 RTN_NUMBER(ErrorNumber) != Errors[Idx].Value); Idx++);

    if (NumberMask & RTN_M_STRING) strcat(MsgBuffer,Errors[Idx].FullName);
    else if (NumberMask & RTN_M_NMEMONIC)
            strcat(MsgBuffer,Errors[Idx].Nmemonic);
    if (NumberMask & (RTN_M_STRING | RTN_M_NMEMONIC)) strcat(MsgBuffer," ");

    if (NumberMask & RTN_M_VALUE)
    { sprintf(Temp,"(%d)",RTN_NUMBER(ErrorNumber));
      strcat(MsgBuffer,Temp);
    }
  }


  return MsgBuffer;
}
