#ifndef MIPS_LBL_GEN_API_INCLUDED
#define MIPS_LBL_GEN_API_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/******************************************************************************
 *      This file defines the structures used by the multimission label
 *  processing software.  It contains definitions for label components
 *  as well as the processing control structure and template structure
 *  for defining a set of label items (property or history)
 *
 * 	This file only contains the necessary components to build and
 *  use the label processing software, as such it is an API, application
 *  program interface.  Examples of code that actually use the API can
 *  be found in the module LBL_ROUTINES.COM
 *
 *	The LBL_ROUTINE package supports 'versioning' of keywords for
 *  reading, and can write multiple elements per keyword.  This package
 *  is geared toward writing a block or set of related keywords at one
 *  time, such as a property or history label.
 *
 *	This file also defines a number of constants and structures that
 *  can (maybe should) be used by the end-user application program.  The
 *  structures define 'Value' and 'Valid' components for a variety of
 *  different variable types (Integer, Float, Double, String, etc.).  There
 *  are also cases where multi-element arrays are defined.  This gives the
 *  option of having one valid flag associated with the set of values.  An
 *  alternative would be for the specific element to be declared as an array
 *  of Value/Valid pairs.  The distinction may seem subtle, but implementing
 *  the wrong method for a specific usage will yeild undesirable results.
 *
 *	The difference is dependent on the intent of the label item.  If the
 *  the label item must contian an exact number values (e.g., a direction
 *  vector), then a multi-element array with one valid flag should be used.
 *  If a label item MAY have at most a number of elements, (e.g., a
 *  position coordinate, where the position might be determined in 2 or 3
 *  axes), then an array of Value/Valid pairs should be declared.  Using
 *  this mechanism only places relevant values in the label, eliminating
 *  ambiquity as to what any extra values might mean.
 *
 *	Using the valid flag to define the PDS 'non-value' values for a
 *  multi-element value, will set all values for the entire set.
 *  Additionally, the PDS values will always be a string value.  When obtaining
 *  label items that contain PDS-values, the valid flag is set to idicate the
 *  PDS-value, and the keyword value is left unassigned, except for STRING
 *  variable types.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *
 ******************************************************************************/
#define  LBL_KEYWORD_VERSION	2	/* Defines API's keyword generation */
#define  LBL_VERSION_ONE	"V1"
#define  LBL_VERSION_TWO	"V2,MPL,MER"

#define  LBL_KEYWORD_DELIMITERS	" ,;."
#define  LBL_KEYWORDLIST_LTH	1024	/* Max string length of keyword list */
#define  LBL_KEYWORD_LTH	64	/* Max string length of a keyword */
#define  LBL_TRUE		1
#define  LBL_FALSE		0
#define  LBL_NULL		0
#define  LBL_REQUIRED		LBL_TRUE
#define  LBL_OPTIONAL		LBL_FALSE
#define  LBL_CONTINUE		LBL_TRUE
#define  LBL_NO_CONT		LBL_FALSE
#define  LBL_NO_RETURN		-1
#define  LBL_INVALID		0
#define  LBL_VALID		1
#define  LBL_DELETE             2
#define  LBL_PDS_NULL		10
#define  LBL_PDS_UNK		12
#define  LBL_PDS_NA		14
#define  LBL_OBTAIN		LBL_TRUE

#define  LBL_WRITE		0
#define  LBL_READ	        1	
#define  LBL_AUGMENT            2

#define  LBL_CHK_PRESENT(x)	(x)
#define  LBL_CHK_VALID(x)	((x) & 0x01)
#define  LBL_CHK_NULL(x)	((x) == LBL_PDS_NULL)
#define  LBL_CHK_UNK(x)		((x) == LBL_PDS_UNK)
#define  LBL_CHK_NA(x)		((x) == LBL_PDS_NA)

#define LBL_PDS_STRING_NA	"N/A"		/* string N/A */
#define LBL_PDS_STRING_UNK	"UNK"		/* string UNK */
#define LBL_PDS_STRING_NULL	"NULL"		/* string NULL */

/***  Constant Label Operations  ***/
#define  LBL_MAX_STRING         256	/* Length of a string value */
#define  LBL_MAX_ITEMS		  5	/* Multi-values for product labels */

/***  Constant Label Values  ***/
/* Generic string lengths */
#define  LBL_FLAG_LTH		  8	/* Max value length for a '_FLAG' keyword	*/
#define  LBL_ID_LTH		 48	/* Max value length for an '_ID' keyword	*/
#define  LBL_TYPE_LTH		 32	/* Max value length for a '_TYPE' keyword	*/
#define  LBL_TIME_LTH		 32	/* Max value length for a '_TIME' keyword	*/
#define  LBL_NAME_LTH		 64	/* Max value length for some '_NAME' keywords	*/
#define  LBL_LONG_NAME_LTH	128	/* Max value length for some '_NAME' keywords	*/
#define  LBL_DESC_LTH		256	/* Max value length for a '_DESC' keyword	*/

/* Generic array sizes */
#define  LBL_VECTOR_ARRAY	  3
#define  LBL_POSITION_ARRAY	  3
#define  LBL_QUATERNION_ARRAY	  4
#define  LBL_TRANSFORM_ARRAY	 12
#define  LBL_RGB_ITEMS		  3
#define  LBL_COORD_SYS_INDEX	  10

#define  LBL_OFFSET(x,y)	((int)(uintptr_t)(&(((x *)0)->y)))

typedef struct
	{ /***	Internal Processing Controls - Not Label Items	***/
	int		FileUnit;
	int		Instance;
	int		RtnStatus;
	int		ErrorCount;
	char		ErrMsgBuf[256];
	unsigned int	ProceedOnError	:  1;
	unsigned int	Obtain		:  3;
	unsigned int	Reserved	: 28;
	} LblApiCntrl_typ;

typedef struct
	{ /***  General label element processing table.  This structure  ***/
	  /***  is used to define the set of items in the label          ***/
	  /***  The last element of table must be NULL                   ***/
	char		*KeywordList;	/* Keyword alias list, token sperated */
	char		*Format;	/* STRING INT REAL or DOUB          */
	int		Required;	/* Must be specified                */
	int		Continuation;	/* If this is a multi-element label */
					/* NOTE: Used when the number of    */
					/* values are variable.             */
					/* NOTE: All continued elements must*/
					/* have 'MaxElements' equal to 1    */
	int		MaxElements;	/* Maximum elements allowed         */
	int		Element;	/* Element number of an item        */
	char		*KeywordUsed;	/* for PROJ API debugging only      */
	int		ValueOffset;	/* Buffer offset to keyword value   */
	int		ValidOffset;	/* Buffer offset to keyword valid flag */
	int		RtnElementOffset; /* Buffer offset to elements returned count */
	int		MemoryAllocated;/* Max memory (mainly used for strings) */
	} LblApiElement_typ;

typedef struct
	{ /***  General label object used to transfer label elements.  ***/
	LblApiElement_typ	*Table;	/* Array of label elements */
	char			*Type;
	char			*NameKeyword;
	const char		*NameValue;
	char			*Buffer;
	int			BufferSize;
	} LblApiProcess_typ;


/***  Function Prototypes  ***/
	int	LblProcessor( LblApiCntrl_typ *, LblApiProcess_typ * );
	int	LblSetVersion( const char * );
	void	PrintLabelElements( LblApiProcess_typ * );
	void	TestLoadLabelElements( LblApiProcess_typ * );
const	char	*LblErrorMessage( void );

typedef struct
	{
	int		Value;
	int		Valid;
	} LblApiIntItem_typ;

typedef struct
	{
	float		Value;
	int		Valid;
	} LblApiRealItem_typ;

typedef struct
	{
	float		Value[LBL_VECTOR_ARRAY];
	int		Valid;
	} LblApiRealVectorItem_typ;

typedef struct
	{
	float		Value[LBL_QUATERNION_ARRAY];
	int		Valid;
	} LblApiQuaternionItem_typ;

typedef struct
	{
	double		Value;
	int		Valid;
	} LblApiDoubleItem_typ;

typedef struct
	{
	char		Value[LBL_MAX_STRING];
	int		Valid;
	} LblApiStringItem_typ;

typedef struct
	{
	char		Value[LBL_ID_LTH];
	int		Valid;
	} LblApiIdItem_typ;

typedef struct
	{
	char		Value[LBL_TYPE_LTH];
	int		Valid;
	} LblApiTypeItem_typ;

typedef struct
	{
	char		Value[LBL_TIME_LTH];
	int		Valid;
	} LblApiTimeItem_typ;

typedef struct
	{
	char		Value[LBL_NAME_LTH];
	int		Valid;
	} LblApiNameItem_typ;

typedef struct
	{
	char		Value[LBL_LONG_NAME_LTH];
	int		Valid;
	} LblApiLongNameItem_typ;

typedef struct
	{
	char		Value[LBL_DESC_LTH];
	int		Valid;
	} LblApiDescItem_typ;

typedef struct
	{
	char		Value[LBL_FLAG_LTH];
	int		Valid;
	} LblApiFlagItem_typ;

#ifdef __cplusplus
}
#endif

#endif
