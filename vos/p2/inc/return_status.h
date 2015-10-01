#ifndef RETURN_STATUS_INCLUDED
#define  RETURN_STATUS_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1999, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdlib.h>		/* for abs() call */

/***  Function prototypes  ***/
const char      *err_msg( int, int, int, int );


/***  General error codes  ***
 ***  0x00 - Normal		Everything performed nominally
 ***  0x01 - Warning		Did NOT do it; but it's not a problem
 ***  0x02 - Informational	Did it; but be aware of this
 ***  0x03 - Error		Did NOT do it; does not effect anything else
 ***  0x04 - Debugging		Did it; programmer should know about this
 ***  0x05 - Critical		Did NOT do it; you should probably stop
 ***  0x06 - Recovered		Did it w/ problems, but fixed 'em
 ***  0x07 - Fatal error	Did NOT do it; screwed-up, you better stop
 ***  0x08 - Notify		Report it (page some-one ?) - modifier only
 **/

/***  Field Masks  ***/
#define  RTN_M_CODE	0x0000000F
#define  RTN_M_NUMBER	0x000FFFF0	/* Over 4096 is Subsystem specific */
#define  RTN_M_SUBSYS	0xFFF00000

#define  RTN_M_NORMAL	0x00
#define  RTN_M_WARNING	0x01
#define  RTN_M_INFO	0x02
#define  RTN_M_ERROR	0x03
#define  RTN_M_DEBUG	0x04
#define  RTN_M_CRITICAL 0x05
#define  RTN_M_RECOVER	0x06
#define  RTN_M_FATAL	0x07
#define  RTN_M_NOTIFY	0x08

#define  RTN_M_VALUE		0x01
#define  RTN_M_NMEMONIC		0x02
#define  RTN_M_STRING		0x04

/***  Field bit offsets  ***/
#define  RTN_O_CODE		 0
#define  RTN_O_NUMBER		 4
#define  RTN_O_SUBSYS		20

/***  Extraction Macros  ***/
#define  RTN_FAILURE(x)	( (x) & 0x01 && ((x) & RTN_M_CODE) > RTN_M_WARNING )
#define  RTN_SUCCESS(x)	( !((x) & 0x01) )
#define  RTN_CODE(x)	( (x) & RTN_M_CODE )
#define  RTN_NUMBER(x)	( ((unsigned int)(x) & RTN_M_NUMBER) >> RTN_O_NUMBER )
#define  RTN_SUBSYS(x)	( ((unsigned int)(x) & RTN_M_SUBSYS) >> RTN_O_SUBSYS )

/***  Message Macros  ***/
#define  RTN_DFLT_MSG(x)	err_msg((x), \
				RTN_M_STRING, \
				RTN_M_NMEMONIC, \
				(RTN_M_VALUE|RTN_M_STRING))
#define  RTN_CODE_MSG(x)	err_msg((x),RTN_M_STRING,0,0)
#define  RTN_SUBSYS_MSG(x)	err_msg((x),0,RTN_M_STRING,0)
#define  RTN_NUMBER_MSG(x)	err_msg((x),0,0,RTN_M_STRING)

/***  Creation Macros  ***/
#define  RTN_SET(x,y)		( (unsigned int)(x) << (y) )

#define  RTN_STAT_VALUE(x,y,z)	( RTN_SET((x),RTN_O_NUMBER) | \
                                  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_SET((z),RTN_O_CODE) )
#define  RTN_STAT_NORMAL(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_NORMAL )
#define  RTN_STAT_WARNING(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_WARNING )
#define  RTN_STAT_INFO(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_INFO )
#define  RTN_STAT_ERROR(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_ERROR )
#define  RTN_STAT_DEBUG(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_DEBUG )
#define  RTN_STAT_CRITICAL(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_CRITICAL )
#define  RTN_STAT_RECOVER(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_RECOVER )
#define  RTN_STAT_FATAL(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_FATAL )
#define  RTN_STAT_NOTIFY(x,y)	( RTN_SET((x),RTN_O_NUMBER) | \
				  RTN_SET((y),RTN_O_SUBSYS) | \
				  RTN_M_NOTIFY )


/***  Return code values  ***/
#define  RTN_C_NORMAL		0
#define  RTN_C_WARNING		1
#define  RTN_C_INFO		2
#define  RTN_C_ERROR		3
#define  RTN_C_DEBUG		4
#define  RTN_C_CRITICAL		5
#define  RTN_C_RECOVER		6
#define  RTN_C_FATAL		7
#define  RTN_C_NOTIFY		8

/***  Return subsystem values  ***/
#define  RTN_S_GENERIC		0	/* Generic */
#define  RTN_S_GENERAL		1	/* General */
#define  RTN_S_VICAR		2	/* VICAR RTL errors */
#define  RTN_S_CAT_API		3	/* Catalog API */
#define  RTN_S_LBL_API		4	/* Label API */
#define  RTN_S_PARAM		5	/* Parameter processing */
#define  RTN_S_SFDU		6	/* SFDU processing */
#define  RTN_S_CHDO		7	/* CHDO processing */
#define  RTN_S_TLM_PKT		8	/* Telemetry Packet */

/***  Return number values  ***/
#define  RTN_V_UNDEF		  0	/* Undefined error */
#define  RTN_V_INVALID		  1	/* Invalid feature */
#define  RTN_V_UNSUPPORTED	  2	/* Unsupported feature */
#define  RTN_V_TIMEOUT		  3	/* Not completed within time limit */
#define  RTN_V_EOD		  4	/* No more data available */
#define  RTN_V_ALLOCATE		  5	/* Allocate "resource" */
#define  RTN_V_OPEN		  6	/* Open a "resource" */
#define  RTN_V_CREATE		  7	/* Create a "resource" */
#define  RTN_V_ATTACH		  8	/* Attach a "resource" */
#define  RTN_V_EXISTS		  9	/* "Resource" exists */
#define  RTN_V_INITIALIZE	 10	/* Initialize process */
#define  RTN_V_VICAR_RTL	 11	/* VICAR Run-time library error */
#define  RTN_V_MISSING_VALUE	 12	/* Missing a value */
#define  RTN_V_NO_PERIOD	 13	/* Resource name invalid ... */
					/* ... seperate words by '.' */
#define  RTN_V_BAD_PATH		 14	/* Syntax error in path name */
#define  RTN_V_EOF		 15	/* End of file encountered */
#define  RTN_V_INSUFF_DATA	 16	/* Insufficient data available */
#define  RTN_V_INSUFF_MEMORY	 17	/* Requested memory not available */
#define  RTN_V_MUCHO_DATA	 18	/* Too much data available */
#define  RTN_V_MISSING_DATA	 19	/* Not all requested data available */
#define  RTN_V_BOUND_ERROR	 20	/* Exceeded expected bounds/range */
#define  RTN_V_IO_ERROR		 21	/* IO error occured */
#define  RTN_V_NOT_IMPLEMENTED	 22	/* Function not implemented */
#define  RTN_V_NOT_AVAILABLE	 23	/* "Resource" not available */
#define  RTN_V_INVLD_ARG	 24	/* Invalid argument */
#define  RTN_V_NOT_FOUND	 25	/* not found */
#define  RTN_V_INCONSISTENT	 26	/* Resource is inconsistently defined */
#define  RTN_V_UNEXPECTED	 27	/* Unexpected result */

#define  RTN_V_SFDU_DATA	110	/* SFDU is a DATA transport */
#define  RTN_V_SFDU_INFO	111
#define  RTN_V_SFDU_DELIM	112	/* SFDU is marker delimited */
#define  RTN_V_SFDU_UNKWN_LEN	113

#define  RTN_V_SFDU_ERROR	120

/***  Generic Return status values  ***/
#define  RTN_NORMAL		RTN_STAT_NORMAL  (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_WARNING		RTN_STAT_WARNING (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_INFO		RTN_STAT_INFO    (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_ERROR		RTN_STAT_ERROR   (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_DEBUG		RTN_STAT_DEBUG   (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_CRITICAL		RTN_STAT_CRITICAL(RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_RECOVER		RTN_STAT_RECOVER (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_FATAL		RTN_STAT_FATAL   (RTN_V_UNDEF, RTN_S_GENERIC)

/***  General Codes  ***/
#define  RTN_TIME_OUT		RTN_STAT_WARNING (RTN_V_TIMEOUT, RTN_S_GENERAL)
#define  RTN_EOD		RTN_STAT_WARNING (RTN_V_EOD, RTN_S_GENERAL)
#define  RTN_UNSUPPORTED	RTN_STAT_ERROR   (RTN_V_UNSUPPORTED, RTN_S_GENERAL)
#define  RTN_INVALID		RTN_STAT_ERROR   (RTN_V_INVALID, RTN_S_GENERAL)
#define  RTN_ALLOCATE_ERROR	RTN_STAT_ERROR   (RTN_V_ALLOCATE, RTN_S_GENERAL)
#define  RTN_OPEN_ERROR		RTN_STAT_ERROR   (RTN_V_OPEN, RTN_S_GENERAL)
#define  RTN_CREATE_ERROR	RTN_STAT_ERROR   (RTN_V_CREATE, RTN_S_GENERAL)
#define  RTN_ATTACH_ERROR	RTN_STAT_ERROR   (RTN_V_ATTACH, RTN_S_GENERAL)
#define  RTN_VICAR_RTL		RTN_STAT_ERROR   (RTN_V_VICAR_RTL, RTN_S_GENERAL)
#define  RTN_CONVERT_VIC(x)	RTN_STAT_ERROR   (abs(x),RTN_S_VICAR)
#define  RTN_INSUFF_DATA	RTN_STAT_WARNING (RTN_V_INSUFF_DATA, RTN_S_GENERAL)
#define  RTN_INSUFF_MEMORY	RTN_STAT_ERROR   (RTN_V_INSUFF_MEMORY, RTN_S_GENERAL)
#define  RTN_MISSING_DATA	RTN_STAT_WARNING (RTN_V_MISSING_DATA, RTN_S_GENERAL)
#define  RTN_IO_ERROR		RTN_STAT_ERROR   (RTN_V_IO_ERROR, RTN_S_GENERAL)
#define  RTN_INIT_ERROR		RTN_STAT_ERROR   (RTN_V_INITIALIZE, RTN_S_GENERAL)
#define  RTN_MISSING_VALUE	RTN_STAT_ERROR   (RTN_V_MISSING_VALUE, RTN_S_GENERAL)
#define  RTN_EXISTS_ERROR	RTN_STAT_ERROR   (RTN_V_EXISTS, RTN_S_GENERAL)
#define  RTN_TOO_MUCH_DATA	RTN_STAT_ERROR   (RTN_V_MUCHO_DATA, RTN_S_GENERAL)
#define  RTN_NOT_IMPLEMENTED	RTN_STAT_ERROR   (RTN_V_NOT_IMPLEMENTED, RTN_S_GENERAL)
#define  RTN_NOT_AVAILABLE	RTN_STAT_ERROR   (RTN_V_NOT_AVAILABLE, RTN_S_GENERAL)
#define  RTN_INVLD_ARG_IGNORED	RTN_STAT_WARNING (RTN_V_INVLD_ARG, RTN_S_GENERAL)
#define  RTN_INVLD_ARG		RTN_STAT_ERROR   (RTN_V_INVLD_ARG, RTN_S_GENERAL)

/***  Parameter Processing Codes  ***/
#define  RTN_PARAM_MISSING	RTN_STAT_WARNING (RTN_V_MISSING_VALUE, RTN_S_PARAM)
#define  RTN_PARAM_NO_PERIOD	RTN_STAT_ERROR   (RTN_V_NO_PERIOD, RTN_S_PARAM)
#define  RTN_PARAM_BAD_PATH	RTN_STAT_ERROR   (RTN_V_BAD_PATH, RTN_S_PARAM)
#define  RTN_PARAM_EOF		RTN_STAT_NORMAL  (RTN_V_EOF, RTN_S_PARAM)
#define  RTN_PARAM_UNDEF	RTN_STAT_ERROR   (RTN_V_UNDEF, RTN_S_PARAM)

/***  Catalog Codes  ***/
#define  RTN_CAT_EXCEEDED_TABLE	RTN_STAT_ERROR   (RTN_V_BOUND_ERROR, RTN_S_CAT_API)
#define  RTN_CAT_STORED_PROC	RTN_STAT_ERROR   (RTN_V_INVALID, RTN_S_CAT_API)

/***  Label Codes  ***/
#define  RTN_LBL_MISSING_REQ	RTN_STAT_ERROR   (RTN_V_MISSING_DATA, RTN_S_LBL_API)
#define  RTN_LBL_MISSING_OPT	RTN_STAT_WARNING (RTN_V_MISSING_DATA, RTN_S_LBL_API)
#define  RTN_LBL_UNSUPPORTED	RTN_STAT_ERROR   (RTN_V_UNSUPPORTED, RTN_S_LBL_API)

/***  SFDU Codes  ***/
#define  RTN_SFDU_INSUFF	RTN_STAT_WARNING (RTN_V_INSUFF_DATA, RTN_S_SFDU)
#define  RTN_SFDU_DATA		RTN_STAT_NORMAL  (RTN_V_SFDU_DATA, RTN_S_SFDU)
#define  RTN_SFDU_INFO		RTN_STAT_NORMAL  (RTN_V_SFDU_INFO, RTN_S_SFDU)
#define  RTN_SFDU_EOF		RTN_STAT_NORMAL  (RTN_V_EOF, RTN_S_SFDU)
#define  RTN_SFDU_UNKWN_LEN	RTN_STAT_NORMAL  (RTN_V_SFDU_UNKWN_LEN, RTN_S_SFDU)
#define  RTN_SFDU_DELIMITER	RTN_STAT_NORMAL  (RTN_V_SFDU_DELIM, RTN_S_SFDU)
#define  RTN_SFDU_INVALID	RTN_STAT_ERROR   (RTN_V_INVALID, RTN_S_SFDU)
#define  RTN_SFDU_UNSUPPORTED	RTN_STAT_ERROR   (RTN_V_UNSUPPORTED, RTN_S_SFDU)

#define  RTN_SFDU_ERROR		RTN_STAT_ERROR   (RTN_V_SFDU_ERROR, RTN_S_SFDU)

/***  CHDO Codes  ***/
#define  RTN_CHDO_TOO_MANY	RTN_STAT_ERROR   (RTN_V_MUCHO_DATA, RTN_S_CHDO)
#define  RTN_CHDO_UNSUPPORTED	RTN_STAT_ERROR   (RTN_V_UNSUPPORTED, RTN_S_CHDO)
#define  RTN_CHDO_NOT_FOUND	RTN_STAT_ERROR   (RTN_V_NOT_FOUND, RTN_S_CHDO)
#define  RTN_CHDO_INVALID	RTN_STAT_ERROR   (RTN_V_INVALID, RTN_S_CHDO)
#define  RTN_CHDO_INCONSISTENT	RTN_STAT_ERROR   (RTN_V_INCONSISTENT, RTN_S_CHDO)

/***  Telemetry Packet Codes  ***/
#define  RTN_PKT_INVALID	RTN_STAT_ERROR   (RTN_V_INVALID, RTN_S_TLM_PKT)
#define  RTN_PKT_UNDEF		RTN_STAT_ERROR   (RTN_V_UNDEF, RTN_S_TLM_PKT)
#define  RTN_PKT_INCONSISTENT	RTN_STAT_ERROR   (RTN_V_INCONSISTENT, RTN_S_TLM_PKT)
#define  RTN_PKT_UNEXPECTED	RTN_STAT_ERROR   (RTN_V_UNEXPECTED, RTN_S_TLM_PKT)
#define  RTN_PKT_UNSUPPORTED	RTN_STAT_ERROR   (RTN_V_UNSUPPORTED, RTN_S_TLM_PKT)
#define  RTN_PKT_EXISTS		RTN_STAT_WARNING (RTN_V_EXISTS, RTN_S_TLM_PKT)
#define  RTN_PKT_RANGE		RTN_STAT_ERROR   (RTN_V_BOUND_ERROR, RTN_S_TLM_PKT)


/*******************/
/***  OLD CODES  ***/
/***  Don't Use  ***/
/*******************/
#ifndef RTS_ERRORS_INCLUDED
#define  RTS_ERRORS_INCLUDED	1

#define  RTS_RTN_BAD(x)		RTN_FAILURE(x)
#define  RTS_RTN_GOOD(x)	RTN_SUCCESS(x)
#define  ERR_NUMBER(x)		RTN_NUMBER(x)
#define  ERR_CODE(x)		RTN_CODE(x)

#define  ERR_NORMAL		RTN_M_NORMAL
#define  ERR_WARNING		RTN_M_WARNING
#define  ERR_INFO		RTN_M_INFO
#define  ERR_ERROR		RTN_M_ERROR
#define  ERR_DEBUG		RTN_M_DEBUG
#define  ERR_RECOVER		RTN_M_RECOVER
#define  ERR_FATAL		RTN_M_FATAL
#define  ERR_NOTIFY		RTN_M_NOTIFY

/***  Generic Codes  ***/
#define  RTS_NORMAL		RTN_STAT_NORMAL  (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTS_WARNING		RTN_STAT_WARNING (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTS_INFO		RTN_STAT_INFO    (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTS_ERROR		RTN_STAT_ERROR   (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTS_DEBUG		RTN_STAT_DEBUG   (RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTN_CRITICAL		RTN_STAT_CRITICAL(RTN_V_UNDEF, RTN_S_GENERIC)
#define  RTS_FATAL		RTN_STAT_FATAL   (RTN_V_UNDEF, RTN_S_GENERIC)

/***  General Codes  ***/
#define  TIME_OUT		RTN_STAT_WARNING ( 1, RTN_S_GENERIC)
#define  EOD			RTN_STAT_WARNING ( 2, RTN_S_GENERIC)
#define  ALLOCATE_ERROR		RTN_STAT_ERROR   ( 3, RTN_S_GENERIC)
#define  OPEN_ERROR		RTN_STAT_ERROR   ( 4, RTN_S_GENERIC)
#define  VICAR_IO_ERR		RTN_STAT_ERROR   ( 5, RTN_S_GENERIC)
/***  Parameter Processing Codes  ***/
#define  MISSING_VALUE		RTN_STAT_WARNING ( 6, RTN_S_GENERIC)
#define  PARAM_NO_PERIOD	RTN_STAT_ERROR   ( 7, RTN_S_GENERIC)
#define  PARAM_BAD_PATH		RTN_STAT_ERROR   ( 8, RTN_S_GENERIC)
#define  PARAM_EOF		RTN_STAT_NORMAL  ( 9, RTN_S_GENERIC)
#define  PARAM_UNDEF		RTN_STAT_ERROR   (10, RTN_S_GENERIC)
/***  SFDU Codes  ***/
#define  INSUFF_DATA		RTN_STAT_WARNING (16, RTN_S_GENERIC)
#define  DATA_SFDU		RTN_STAT_NORMAL  (17, RTN_S_GENERIC)
#define  INFO_SFDU		RTN_STAT_NORMAL  (18, RTN_S_GENERIC)
#define  DELIMITER_SFDU		RTN_STAT_NORMAL  (19, RTN_S_GENERIC)
#define  INVALID_SFDU		RTN_STAT_ERROR   (20, RTN_S_GENERIC)
#define  ERROR_SFDU		RTN_STAT_ERROR   (21, RTN_S_GENERIC)
#define  UNSUPPORTED_SFDU	RTN_STAT_ERROR   (22, RTN_S_GENERIC)

/***  CHDO Codes  ***/
#define  TOO_MANY_CHDOS		RTN_STAT_ERROR   (23, RTN_S_GENERIC)
#define  UNSUPPORTED_CHDO	RTN_STAT_ERROR   (24, RTN_S_GENERIC)

/***  Telemetry Packet Codes  ***/
#define  INVALID_PKT		RTN_STAT_ERROR   (32, RTN_S_GENERIC)
#define  UNDEF_PKT_HDR		RTN_STAT_ERROR   (33, RTN_S_GENERIC)

/***  Catalog Codes  ***/
#define  EXCEEDED_TABLE		RTN_STAT_ERROR   (48, RTN_S_GENERIC)

#endif

#ifdef __cplusplus
}
#endif

#endif
