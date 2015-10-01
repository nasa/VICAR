/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/

#ifndef TAE
#ifndef VRDI
#define VRDI            /* default */
#endif
#endif

/*
 *
 *	XDERRORS.H 
 *
 */

#ifdef VRDI

#define	XD_ERROR_FLAG		0x10000

#define MAKE_ERROR(x)		((-x-1) & (~XD_ERROR_FLAG))
#define MAKE_POINTER(x)		(-(x ^ XD_ERROR_FLAG) - 1)

#define IS_XD_CODE(x)		((x<0) && ((XD_ERROR_FLAG & x) == 0))
#define IS_KNOWN_CODE(x)	((MAKE_POINTER(x) >= 0) &&		\
				 (MAKE_POINTER(x) < N_ERRORS) &&	\
				 (KEY(x) != 0))

#define	IS_WARNING(x)	(error_table[MAKE_POINTER(x)].severity == XD_WARN)
#define	IS_ERROR(x)	(error_table[MAKE_POINTER(x)].severity == XD_ERROR)
#define	IS_FATAL(x)	(error_table[MAKE_POINTER(x)].severity == XD_FATAL)

#define	SET_BITS(C,A)	 C=0;\
			  if (A > 1) C |= SYS_MSG_BIT;\
			  if (A > 2) C |= ABORT_BIT; 

#define	KEY(x)		error_table[MAKE_POINTER(x)].key
#define	MSG(x)		error_table[MAKE_POINTER(x)].message

#define SYS_MSG(x)	((SYS_MSG_BIT & x) != 0)
#define ABORT_FLAG(x)	((ABORT_BIT   & x) != 0)

#define XD_UNKNOWN	0
#define XD_NO_ERROR	1
#define	XD_WARN		2
#define XD_ERROR	3
#define	XD_FATAL	4

#ifndef SUCCESS
#define	SUCCESS		1
#endif

#define	AFG_NOT_ACTIVE		MAKE_ERROR(0)		/* -65537 */
#define	AFG_NOT_AVAILABLE	MAKE_ERROR(1)		/* -65538 */
#define	AW_NOT_ON_IMP		MAKE_ERROR(2)		/* -65539 */
#define	AW_NOT_SQUARE		MAKE_ERROR(3)		/* -65540 */
#define	AW_SIZES_DIFFERENT	MAKE_ERROR(4)		/* -65541 */
#define	BAD_ACCESS_WINDOW	MAKE_ERROR(5)		/* -65542 */
#define	BAD_ASCII_CODE		MAKE_ERROR(6)		/* -65543 */
#define	BAD_COORDINATE_COUNT	MAKE_ERROR(7)		/* -65544 */
#define	BAD_DISPLAY_WINDOW	MAKE_ERROR(8)		/* -65545 */
#define	BAD_PIXEL_COUNT		MAKE_ERROR(9)		/* -65546 */
#define	BAD_VECTOR_COUNT	MAKE_ERROR(10)		/* -65547 */
#define	CANNOT_BYPASS_LUT	MAKE_ERROR(11)		/* -65548 */
#define	CANNOT_CONNECT_LUT	MAKE_ERROR(12)		/* -65549 */
#define	CANNOT_DEALLOCATE	MAKE_ERROR(13)		/* -65550 */
#define	CANNOT_ZOOM		MAKE_ERROR(14)		/* -65551 */
#define	DEVICE_ALREADY_ACTIVE	MAKE_ERROR(15)		/* -65552 */
#define	DEVICE_ALREADY_ALLOC	MAKE_ERROR(16)		/* -65553 */
#define	DEVICE_ALREADY_OPEN	MAKE_ERROR(17)		/* -65554 */
#define	DEVICE_CANNOT_DO_IT	MAKE_ERROR(18)		/* -65555 */
#define	DEVICE_NOT_ACTIVE	MAKE_ERROR(19)		/* -65556 */
#define	DEVICE_NOT_ALLOC	MAKE_ERROR(20)		/* -65557 */
#define	DEVICE_NOT_AVAIL	MAKE_ERROR(21)		/* -65558 */
#define	DEVICE_NOT_OPEN		MAKE_ERROR(22)		/* -65559 */
#define	FONT_FILE_EOF		MAKE_ERROR(23)		/* -65560 */
#define	FONT_FILE_OPEN_ERROR	MAKE_ERROR(24)		/* -65561 */
#define	FONT_FILE_READ_ERROR	MAKE_ERROR(25)		/* -65562 */
#define	FONT_TABLE_OVERFLOW	MAKE_ERROR(26)		/* -65563 */
#define	FUNC_NOT_IMPLEMENTED	MAKE_ERROR(27)		/* -65564 */
#define	IMP_TOO_SMALL		MAKE_ERROR(28)		/* -65565 */
#define	INVALID_ARGUMENT	MAKE_ERROR(29)		/* -65566 */
#define	INVALID_AW_ROTATION	MAKE_ERROR(30)		/* -65567 */
#define	INVALID_CHAR_COUNT	MAKE_ERROR(31)		/* -65568 */
#define	INVALID_COORDINATES	MAKE_ERROR(32)		/* -65569 */
#define	INVALID_DEVICE_REQ	MAKE_ERROR(33)		/* -65570 */
#define	INVALID_DEVICE_TYPE	MAKE_ERROR(34)		/* -65571 */
#define	INVALID_FONT_HEIGHT	MAKE_ERROR(35)		/* -65572 */
#define	INVALID_FONT_LOCATION	MAKE_ERROR(36)		/* -65573 */
#define	INVALID_IMP_SIZE	MAKE_ERROR(37)		/* -65574 */
#define	INVALID_INFO_REQUEST	MAKE_ERROR(38)		/* -65575 */
#define	INVALID_OUTPUT_SIZE	MAKE_ERROR(39)		/* -65576 */
#define	INVALID_RADIUS		MAKE_ERROR(40)		/* -65577 */
#define	INVALID_FONT_SCALE	MAKE_ERROR(41)		/* -65578 */
#define	INVALID_TEXT_PREC	MAKE_ERROR(42)		/* -65579 */
#define	MEMORY_ERROR		MAKE_ERROR(43)		/* -65580 */
#define	MUST_SET_ALL_DWS	MAKE_ERROR(44)		/* -65581 */
#define	MUST_ZOOM_ALL		MAKE_ERROR(45)		/* -65582 */
#define	NOT_IN_ACCESS_WINDOW	MAKE_ERROR(46)		/* -65583 */
#define	NO_AUTO_TRACK		MAKE_ERROR(47)		/* -65584 */
#define	NO_SUCH_CURSOR		MAKE_ERROR(48)		/* -65585 */
#define	NO_SUCH_CURSOR_FORM	MAKE_ERROR(49)		/* -65586 */
#define	NO_SUCH_CURSOR_RATE	MAKE_ERROR(50)		/* -65587 */
#define	NO_SUCH_FONT		MAKE_ERROR(51)		/* -65588 */
#define	NO_SUCH_IMP		MAKE_ERROR(52)		/* -65589 */
#define	NO_SUCH_INPUT_DEVICE	MAKE_ERROR(53)		/* -65590 */
#define	NO_SUCH_KNOB		MAKE_ERROR(54)		/* -65591 */
#define	NO_SUCH_LUT		MAKE_ERROR(55)		/* -65592 */
#define	NO_SUCH_LUT_SECTION	MAKE_ERROR(56)		/* -65593 */
#define	NO_SUCH_SWITCH		MAKE_ERROR(57)		/* -65594 */
#define	OVERLAY_NOT_AVAILABLE	MAKE_ERROR(58)		/* -65595 */
#define	UNDEFINED_OPERATION	MAKE_ERROR(59)		/* -65596 */
#define	UNIT_OUT_OF_RANGE	MAKE_ERROR(60)		/* -65597 */
#define	DIB_OPEN_ERROR		MAKE_ERROR(61)		/* -65598 */
#define	DIB_FILE_ERROR		MAKE_ERROR(62)		/* -65599 */
#define	NO_OWNED_GENERIC_DEV	MAKE_ERROR(63)		/* -65600 */
#define	NO_AVAIL_GENERIC_DEV	MAKE_ERROR(64)		/* -65601 */
#define	NO_DEFAULT_DEVICE	MAKE_ERROR(65)		/* -65602 */
#define	NO_SUCH_DEVICE		MAKE_ERROR(66)		/* -65603 */
#define	CANNOT_ALLOC_DEVICE	MAKE_ERROR(67)		/* -65604 */
#define	CANNOT_ALLOC_HIRES	MAKE_ERROR(68)		/* -65605 */
#define	CANNOT_ALLOC_LORES	MAKE_ERROR(69)		/* -65606 */
#define	CREATE_LNM_ERROR	MAKE_ERROR(70)		/* -65607 */
#define	DELETE_LNM_ERROR	MAKE_ERROR(71)		/* -65608 */
#define	GET_PID_ERROR		MAKE_ERROR(72)		/* -65609 */
#define	GET_OWNER_NAME_ERROR	MAKE_ERROR(73)		/* -65610 */
#define	GET_OWNER_TERM_ERROR	MAKE_ERROR(74)		/* -65611 */
#define	GET_VAX_SYMBOL_ERROR	MAKE_ERROR(75)		/* -65612 */
#define	GET_OWNER_PID_ERROR	MAKE_ERROR(76)		/* -65613 */
#define	CANNOT_DEALL_DEVICE	MAKE_ERROR(77)		/* -65614 */
#define	INVALID_WARNING_ACTION	MAKE_ERROR(78)		/* -65615 */
#define	INVALID_ERROR_ACTION	MAKE_ERROR(79)		/* -65616 */
#define	INVALID_FATAL_ACTION	MAKE_ERROR(80)		/* -65617 */
#define	TRANS_LNM_ERROR		MAKE_ERROR(81)		/* -65618 */
#define	CANNOT_RESET_MPC	MAKE_ERROR(82)		/* -65619 */
#define	CANNOT_POS_CURSOR	MAKE_ERROR(83)		/* -65620 */
#define	DEVICE_FILE_OPEN_ERROR	MAKE_ERROR(84)		/* -65621 */
#define	SERVER_NOT_AVAILABLE	MAKE_ERROR(85)		/* -65622 */
#define	SHMEM_INVALID_SIZE	MAKE_ERROR(86)		/* -65623 */
#define	SHMEM_ACCESS_VIOLATION	MAKE_ERROR(87)		/* -65624 */
#define	SHMEM_NO_SUCH_ID	MAKE_ERROR(88)		/* -65625 */
#define	SHMEM_NO_MORE_IDS	MAKE_ERROR(89)		/* -65626 */
#define	SHMEM_OUT_OF_MEMORY	MAKE_ERROR(90)		/* -65627 */
#define	SHMEM_CANNOT_ALLOC	MAKE_ERROR(91)		/* -65628 */
#define SHMEM_CANNOT_REMOVE	MAKE_ERROR(92)		/* -65629 */
#define	SHMEM_CANNOT_DETACH	MAKE_ERROR(93)		/* -65630 */
#define INVALID_CURSOR_SIZE	MAKE_ERROR(94)		/* -65631 */
#define INVALID_COLOR		MAKE_ERROR(95)		/* -65632 */
#define SHMEM_ALREADY_ACTIVE	MAKE_ERROR(96)		/* -65633 */

#define N_ERRORS	97
#define	LAST_ERROR	MAKE_ERROR(N_ERRORS-1)

#ifdef PUBLIC
#ifndef	XD_INITIALIZE
PUBLIC	struct  {
   int	severity;
   char	*key;
   char	*message;
   }	error_table[N_ERRORS];
#else /* XD_INITIALIZE */
PUBLIC	struct  {
   int	severity;
   char	*key;
   char	*message;
   }	error_table[N_ERRORS] = {
    {
	XD_ERROR,
	"AFGNOTACT",
	"Alphanumeric font generator is not active"
    },
    {
	XD_ERROR,
	"NOAFG",
	"Alphanumeric font generator is not available"
    },
    {
	XD_WARN,
	"AWOFFIMP",
	"Part of the access window is not on the image plane"
    },
    {
	XD_WARN,
	"AWNOTSQ",
	"Access window is not square"
    },
    {
	XD_WARN,
	"AWDIFF",
	"Access windows are different sizes"
    },
    {
	XD_ERROR,
	"BADAW",
	"Access window improperly defined"
    },
    {
	XD_FATAL,
	"BADASCII",
	"Invalid ASCII character code encountered in font file"
    },
    {
	XD_ERROR,
	"BADCOORCT",
	"The number of coordinates is less than 2"
    },
    {
	XD_ERROR,
	"BADDW",
	"Display window location is not allowed"
    },
    {
	XD_ERROR,
	"BADPIXCNT",
	"Pixel count was less than 1"
    },
    {
	XD_FATAL,
	"BADVECCNT",
	"Invalid vector count encountered in font file"
    },
    {
	XD_WARN,
	"NOBYPASS",
	"Cannot bypass the look-up tables"
    },
    {
	XD_WARN,
	"CANTCNCT",
	"Cannot make IMP/LUT connection"
    },
    {
	XD_ERROR,
	"NOTOWNER",
	"Unable to deallocate device; this process does not own it"
    },
    {
	XD_WARN,
	"CANTZOOM",
	"Cannot zoom by the specified factor"
    },
    {
	XD_WARN,
	"DEVACTIVE",
	"Device is already active"
    },
    {
	XD_WARN,
	"DEVALLOC",
	"Device is already allocated"
    },
    {
	XD_WARN,
	"DEVOPEN",
	"Device is already open"
    },
    {
	XD_WARN,
	"DEVCANTDO",
	"Device does not support the requested function"
    },
    {
	XD_ERROR,
	"DEVNOTACT",
	"Device is not active"
    },
    {
	XD_FATAL,
	"DEVNOTALL",
	"Device is not allocated"
    },
    {
	XD_FATAL,
	"DEVNOTAVL",
	"Device is not available"
    },
    {
	XD_ERROR,
	"DEVNOTOPN",
	"Device is not open"
    },
    {
	XD_FATAL,
	"FNTEOF",
	"Unexpected end-of-file in font definition file"
    },
    {
	XD_FATAL,
	"FNTOPNERR",
	"Error opening font definition file"
    },
    {
	XD_FATAL,
	"FNTRDERR",
	"Error reading font definition file"
    },
    {
	XD_FATAL,
	"FNTTBLOVR",
	"Internal font table overflow"
    },
    {
	XD_WARN,
	"FNCNOTIMP",
	"Function not implemented"
    },
    {
	XD_ERROR,
	"IMPSMALL",
	"Image plane must be as large as video output"
    },
    {
	XD_ERROR,
	"INVARG",
	"Invalid argument received"
    },
    {
	XD_ERROR,
	"INVAWROT",
	"Invalid access window rotation angle"
    },
    {
	XD_ERROR,
	"INVCHCNT",
	"Invalid character count"
    },
    {
	XD_ERROR,
	"INVCOORD",
	"Invalid coordinates"
    },
    {
	XD_ERROR,
	"INVDEVREQ",
	"Invalid request for display device"
    },
    {
	XD_ERROR,
	"INVDEVTYP",
	"Invalid device type"
    },
    {
	XD_ERROR,
	"INVFNTHGT",
	"Invalid font height"
    },
    {
	XD_ERROR,
	"INVFNTLOC",
	"Invalid font location"
    },
    {
	XD_ERROR,
	"INVIMPSIZ",
	"Invalid image memory plane size"
    },
    {
	XD_ERROR,
	"INVINFO",
	"Invalid request for information"
    },
    {
	XD_ERROR,
	"INVOUTSIZ",
	"Invalid video output size"
    },
    {
	XD_ERROR,
	"INVRADIUS",
	"Invalid circle radius"
    },
    {
	XD_ERROR,
	"INVFNTSC",
	"Invalid font horizontal scale factor"
    },
    {
	XD_ERROR,
	"INVTXTPRC",
	"Invalid text precision"
    },
    {
	XD_FATAL,
	"MEMERR",
	"Memory allocation error",
    },
    {
	XD_WARN,
	"MUSTSETDW",
	"Display window of all images have been set"
    },
    {
	XD_WARN,
	"MUSTZOOM",
	"All images have been zoomed"
    },
    {
	XD_ERROR,
	"NOTINAW",
	"Requested coordinates are not within the access window"
    },
    {
	XD_WARN,
	"NOAUTO",
	"Auto-tracking is not available"
    },
    {
	XD_ERROR,
	"NOCURS",
	"No such cursor"
    },
    {
	XD_WARN,
	"NOCURSFRM",
	"No such cursor form"
    },
    {
	XD_WARN,
	"NOCURSRAT",
	"No such cursor rate"
    },
    {
	XD_ERROR,
	"NOFONT",
	"Unable to locate font description file"
    },
    {
	XD_ERROR,
	"NOIMP",
	"No such image memory plane"
    },
    {
	XD_ERROR,
	"NOINPDEV",
	"No such input device"
    },
    {
	XD_ERROR,
	"NOKNOB",
	"No such input 'knob'"
    },
    {
	XD_ERROR,
	"NOLUT",
	"No such look-up table"
    },
    {
	XD_ERROR,
	"NOLUTSECT",
	"No such look-up table section"
    },
    {
	XD_ERROR,
	"NOSWITCH",
	"No such input 'switch'"
    },
    {
	XD_ERROR,
	"NOOVRLY",
	"Graphics overlay is not available"
    },
    {
	XD_ERROR,
	"UNDEFOP",
	"Undefined arithmetic/logical operation"
    },
    {
	XD_ERROR,
	"UNOUTRNG",
	"Unit number is out of range"
    },
    {
	XD_FATAL,
	"DIBOPEN",
	"Device Information File open error"
    },
    {
	XD_FATAL,
	"DIBERROR",
	"Error found in Device Information File, contact system manager"
    },
    {
	XD_FATAL,
	"NOOWNGDEV",
	"Unable to find owned generic device"
    },
    {
	XD_FATAL,
	"NOAVLGDEV",
	"Unable to find available generic device"
    },
    {
	XD_FATAL,
	"NODEFDEV",
	"No default device for this terminal"
    },
    {
	XD_FATAL,
	"NOSUCHDEV",
	"Unable to find device"
    },
    {
	XD_FATAL,
	"CANTALLDEV",
	"Unable to allocate device"
    },
    {
	XD_FATAL,
	"CANTALLHI",
	"Unable to allocate high resolution Deanza"
    },
    {
	XD_FATAL,
	"CANTALLOW",
	"Unable to allocate low resolution Deanza"
    },
    {
	XD_FATAL,
	"CRELNMERR",
	"Unable to create logical name"
    },
    {
	XD_FATAL,
	"DELNMERR",
	"Unable to delete logical name"
    },
    {
	XD_FATAL,
	"GETPIDERR",
	"Unable to get process ID"
    },
    {
	XD_FATAL,
	"GETNAMERR",
	"Unable to get device owner username"
    },
    {
	XD_FATAL,
	"GETERMERR",
	"Unable to get device owner terminal"
    },
    {
	XD_FATAL,
	"GETVAXERR",
	"Unable to get value of VAX system symbol"
    },
    {
	XD_FATAL,
	"GETOPIDERR",
	"Unable to get device owner process ID"
    },
    {
	XD_FATAL,
	"CANTDALLDEV",
	"Unable to deallocate device"
    },
    {
	XD_WARN,
	"INVWRNACT",
	"Invalid warning action"
    },
    {
	XD_WARN,
	"INVERRACT",
	"Invalid error action"
    },
    {
	XD_WARN,
	"INVFTLACT",
	"Invalid fatal action"
    },
    {
	XD_FATAL,
	"TRANLNMERR",
	"Unable to translate logical name"
    },
    {
	XD_ERROR,
	"PRESETERR",
	"Unable to reset peripheral controller"
    },
    {
	XD_WARN,
	"CANTPOSCUR",
	"Unable to position cursor"
    },
    {
	XD_FATAL,
	"DEVFLERR",
	"Unable to open device status file"
    },
    {
	XD_FATAL,
	"SRVNOTAVL",
	"Server is not available for requested device"
    },
    {
	XD_ERROR,
	"SHMINVLDSIZ",
	"Shared memory size is not within system limits"
    },
    {
	XD_ERROR,
	"SHMACCVIOL",
	"Shared memory id exists but operation permission is denied"
    },
    {
	XD_ERROR,
	"SHMNOID",
	"Shared memory identifier does not exist"
    },
    {
	XD_ERROR,
	"SHMNOMOREIDS",
	"Shared memory identifiers exceed system limit"
    },
    {
	XD_ERROR,
	"SHMNOMEM",
	"Shared memory exceeds available physical memory"
    },
    {
       XD_ERROR,
       "CANTALLSHM",
       "Cannot allocate shared memory"
    },
    {
       XD_ERROR,
       "CANTREMSHM",
       "Cannot remove shared memory"
    },
    {
       XD_ERROR,
       "CANTDETSHM",
       "Cannot detach shared memory"
    },
    {
	XD_ERROR,
	"INVCURSIZ",
	"Invalid cursor size"
    },
    {
	XD_ERROR,
	"INVCLR",
	"Invalid color"
    },
    {
	XD_WARN,
	"SHMACT",
	"Shared memory is already active"
    }
    };

#endif /* XD_INITIALIZE */

#endif /* PUBLIC */

#endif /* VRDI */

#ifdef TAE

#define	SUCCESS		1

#define N_ERRORS	31

#define CANNOT_ALLOC_DEVICE		-2
#define CANNOT_DEALL_DEVICE		-3
#define CREATE_LNM_ERROR		-4
#define DELETE_LNM_ERROR		-5
#define DEVICE_ALREADY_ALLOC		-6
#define DEVICE_NOT_AVAIL		-7
#define DIB_FILE_ERROR			-8
#define DIB_OPEN_ERROR			-9
#define GET_OWNER_PID_ERROR		-10
#define GET_OWNER_TERM_ERROR		-11
#define GET_PID_ERROR			-12
#define GET_VAX_SYMBOL_ERROR		-13
#define MEMORY_ERROR			-14
#define NO_AVAIL_GENERIC_DEV		-15
#define NO_DEFAULT_DEVICE		-16
#define NO_OWNED_GENERIC_DEV		-17
#define NO_SUCH_DEVICE			-18
#define DEVICE_FILE_OPEN_ERROR		-19
#define SERVER_NOT_AVAILABLE		-20
#define SHMEM_INVALID_SIZE		-21
#define SHMEM_ACCESS_VIOLATION		-22
#define SHMEM_NO_SUCH_ID		-23
#define SHMEM_NO_MORE_IDS		-24
#define SHMEM_OUT_OF_MEMORY		-25
#define SHMEM_CANNOT_ALLOC		-26
#define SHMEM_CANNOT_REMOVE		-27
#define SHMEM_CANNOT_DETACH		-28
#define	DEVICE_CANNOT_DO_IT		-29
#define	DEVICE_NOT_ALLOC		-30
#define	TRANS_LNM_ERROR			-31
#define SHMEM_ALREADY_ACTIVE		-32

#ifndef	XD_INITIALIZE
PUBLIC	struct  {
   char	*key;
   char	*message;
   }	error_table[N_ERRORS];
#else /* XD_INITIALIZE */
PUBLIC	struct  {
   char	*key;
   char	*message;
   }	error_table[N_ERRORS] = {
    {
	"CANTALLDEV",
	"Unable to allocate device"
    },
    {
	"CANTDALLDEV",
	"Unable to deallocate device"
    },
    {
	"CRELNMERR",
	"Unable to create logical name"
    },
    {
	"DELNMERR",
	"Unable to delete logical name"
    },
    {
	"DEVALLOC",
	"Device is already allocated"
    },
    {
	"DEVNOTAVL",
	"Device is not available"
    },
    {
	"DIBERROR",
	"Error found in Device Information File, contact system manager"
    },
    {
	"DIBOPEN",
	"Device Information File open error"
    },
    {
	"GETOPIDERR",
	"Unable to get device owner process ID"
    },
    {
	"GETERMERR",
	"Unable to get name of current terminal"
    },
    {
	"GETPIDERR",
	"Unable to get process ID"
    },
    {
	"GETVAXERR",
	"Unable to get value of VAX system symbol"
    },
    {
	"MEMOVR",
	"Memory allocation error",
    },
    {
	"NOAVLGDEV",
	"Unable to find available generic device"
    },
    {
	"NODEFDEV",
	"No default device for this terminal"
    },
    {
	"NOOWNGDEV",
	"Unable to find owned generic device"
    },
    {
	"NOSUCHDEV",
	"Unable to find device"
    },
    {
	"DEVFLERR",
	"Unable to open device status file"
    },
    {
	"SRVNOTAVL",
	"Server is not available for requested device"
    },
    {
	"SHMINVLDSIZ",
	"Shared memory size is not within system limits"
    },
    {
	"SHMACCVIOL",
	"Shared memory id exists but operation permission is denied"
    },
    {
	"SHMNOID",
	"Shared memory identifier does not exist"
    },
    {
	"SHMNOMOREIDS",
	"Shared memory identifiers exceed system limit"
    },
    {
	"SHMNOMEM",
	"Shared memory exceeds available physical memory"
    },
    {
       "CANTALLSHM",
       "Cannot allocate shared memory"
    },
    {
       "CANTREMSHM",
       "Cannot remove shared memory"
    },
    {
       "CANTDETSHM",
       "Cannot detach shared memory"
    },
    {
	"DEVCANTDO",
	"Device does not support the requested function"
    },
    {
	"DEVNOTALL",
	"Device is not allocated"
    },
    {
	"TRANLNMERR",
	"Unable to translate logical name"
    },
    {
	"SHMACT",
	"Shared memory is already active"
    }
    };
#endif /* XD_INITIALIZED */

#endif /* TAE */
