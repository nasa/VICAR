/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/

#ifndef VRDI_XDEXTERNS_H
#define VRDI_XDEXTERNS_H

#ifndef TAE
#include "xvmaininc.h"
#else
#define VMS_OS 1		/* TAE only used for VMS */
#define UNIX_OS 0
#endif

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef TAE
#ifndef VRDI
#define VRDI            /* default */
#endif
#endif

/*
 *
 *	XDEXTERNS.H 
 *
 */

#ifndef INTEGER
typedef	int		*INTEGER;
typedef	float		*REAL;
typedef	int		*LOGICAL;
typedef	unsigned char	*BYTE;
typedef	char		*STRING;
#endif

#define	FUNCTION

#if VMS_OS
#define VMS_JOB_TABLE		1
#define VMS_PROCESS_TABLE	2
#define X_SCRATCH_DIR		"V2$SCRATCH:"
#define X_SOCKET_HDR		"SOCK_"
#if ALPHA_ARCH
#ifdef	XD_INITIALIZE
#define	PRIVATE
#define	PUBLIC
#else /* not XD_INITIALIZE */
#define	PRIVATE	extern
#define	PUBLIC	extern
#endif /* XD_INITIALIZE */
#else
#ifdef	XD_INITIALIZE
#define	PRIVATE	globaldef noshare
#define	PUBLIC	globaldef readonly
#else /* not XD_INITIALIZE */
#define	PRIVATE	globalref
#define	PUBLIC	globalref
#endif /* XD_INITIALIZE */
#endif /* ALPHA_ARCH */
#endif /* VMS_OS */

#if UNIX_OS
#define X_SCRATCH_DIR		"/tmp/"
#define X_SOCKET_HDR		"/tmp/"
#ifdef XD_INITIALIZE
#define PRIVATE
#define PUBLIC
#else
#define PRIVATE extern
#define PUBLIC extern
#endif
#endif /* UNIX_OS */

#ifdef	XD_INITIALIZE
PRIVATE int flag_group=0;
PRIVATE int mask=1;
PRIVATE int invmask=~1;
#else /* not XD_INITIALIZE */
PRIVATE int flag_group;
PRIVATE int mask;
PRIVATE int invmask;
#endif /* XD_INITIALIZE */

#define	TRUE	1
#define	FALSE	0

#ifndef MIN
#define MIN(x,y)	((x) < (y) ? (x) : (y))
#endif  /* MIN */
#ifndef MAX
#define MAX(x,y)	((x) > (y) ? (x) : (y))
#endif  /* MAX */

#define SQUARED(x)	(x * x)

#ifdef VRDI

#define	SYS_MSG_BIT	1
#define ABORT_BIT	2
#define NUM_ERRORS	3

#define	ALL_BITS	0xFF
#define	PI		3.1415926
#define	MINUS_ONE	-1

#define	ROUND( x )	((int) (x < 0 ? (x-0.5) : (x+0.5)))

#endif /* VRDI */

#define	DEVNAME_SIZE	4
#define DEVCODE_SIZE	2
#define	DEVMAKE_SIZE	16
#define	TERMINAL_SIZE	5
#define	LOGNAME_SIZE	3

#define SYSALLOC_YES	'Y'
#define SYSALLOC_NO	'N'
#define SYSALLOC_X	'X'

#define	MAXIMUM_UNITS	50

#define DEV_DEP_SIZE    35
#define	DCB_SIZE	82 + DEV_DEP_SIZE
#define SHMEM_DCB	0
#define SHMEM_TEK	10
#define SHMEM_RAMTEK	11
#define SHMEM_DUMMY	12
#define SHMEM_JUP	13		/* Duplicated in jupmouse.h */
#define SHMEM_X		14
#define SHMEM_MAX	15	/* max # of shmem types (for array alloc) */

#define MAX_GROUPS	32

/* The DCB structure is accessed as an integer array by xddinfo(), with	*/
/* hard-coded numbers referring to specific items in the structure.	*/
/* For this reason, DO NOT rearrange anything above the DeviceDependent	*/
/* element.  You may use the "reserved" slots for something else, but	*/
/* make sure the slots stay there.					*/

/* Note:  Device_Name used to be char *.  But that caused problems with	*/
/* 64-bit builds because xddinfo assumes the size of things.  Since the	*/
/* field is not used anyway, this fixes the problem.  rgd 2010/09/17	*/

struct	DCB_STRUCTURE {   
   int		Device_Name;	/* Unused - replaced by Device_Name_Array */
   int		Device_Type;
   int		reserved_0;
   int		Number_LUTs;
   int		Number_IMPs;
   int		Number_Lines;
   int		Number_Samples;
   int		Available_Configs;
   int		Device_Unit_Number;
   int		Valid_Display_Mode;
   int		Output_Mode;
   int		IMP_Size;
   int		Video_Size;
   int		Aspect_Ratio;
   int		Video_Lines;
   int		Video_Samples;
   int		reserved_4;
   int		reserved_5;
   int		reserved_6;
   int		Batch_Mode;
   int		Each_IMP_Has_DW;
   int		reserved_8;
   int		reserved_9;
   int		reserved_10;
   int		Number_LUT_Sections;
   int		May_Bypass_LUTs;
   int		Maximum_LUT_Value;
   int		Each_IMP_Zooms;
   int		May_Connect_IMP_LUT;
   int		May_Zoom_IMPs;
   int		Overlay_Available;
   int		Overlay_On;
   int		May_Connect_IMP_Overlay;
   int		May_Bypass_Overlay_LUT;
   int		Overlay_IMP;
   int		Overlay_LUT_Section;
   int		Overlay_LUT_Characteristic;
   int		Number_Overlay_LUT_Sections;
   int		Max_Zoom_Factor;
   int		Number_Bits_In_Overlay;
   int		Overlay_Bypass;
   int		AFG_Available;
   int		AFG_Active;
   int		Number_AFG_Lines;
   int		Number_AFG_Characters;
   int		Number_AFG_Character_Types;
   int		Default_Overlay_LUT;
   int		reserved_17;
   int		Number_Cursors;
   int		Number_Cursor_Types;
   int		Number_Cursor_Blink_Rates;
   int		Auto_Track_Available;
   int		Auto_Track_Device;
   int		Auto_Track_Cursor;
   int		May_Color_Cursor;
   int		Device_Has_Processor;
   int		Max_Cursor_XSize;
   int		Max_Cursor_YSize;
   int		reserved_24;
   int		May_Resize_Cursor;
   int		Number_IO_Devices;
   int		IO_Device_1_Type;
   int		IO_Device_1_Coordinates;
   int		IO_Device_1_Pen;
   int		IO_Device_1_Number_Switches;
   int		reserved_26;
   int		reserved_27;
   int		reserved_28;
   int		reserved_29;
   int		reserved_30;
   int		IO_Device_2_Type;
   int		IO_Device_2_Coordinates;
   int		IO_Device_2_Pen;
   int		IO_Device_2_Number_Switches;
   int		DeviceDependent[DEV_DEP_SIZE];
   int		Contents_Initialized;
   char		Device_Name_Array[DEVNAME_SIZE+1];
   int		Device_Config_Flag;
   int		Overlay_Lut_Flag;
   };

#ifdef VRDI

PRIVATE struct DCB_STRUCTURE *DCB[MAXIMUM_UNITS];

PRIVATE int shm_active[MAXIMUM_UNITS];
#define SHARED_MEMORY_ACTIVE	shm_active[*Unit]
#define ZSHARED_MEMORY_ACTIVE	shm_active[unit]

PRIVATE int Device_Active[MAXIMUM_UNITS];
#define DEV_ACTIVE		Device_Active[*Unit]
#define ZDEV_ACTIVE             Device_Active[unit]

PRIVATE int XD_Allocated[MAXIMUM_UNITS];
#define DEVICE_ALLOCATED 	XD_Allocated[*Unit]
#define ZDEVICE_ALLOCATED 	XD_Allocated[unit]

#endif /* VRDI */

PRIVATE	struct	DIB_STRUCTURE	{
   char		Available;
   char		DeviceName[DEVNAME_SIZE+1];
   char		DeviceCode[DEVCODE_SIZE+1];
#ifdef VRDI
   char		Make[DEVMAKE_SIZE+1];
#endif
   int		DeviceType;
   char		SubType;
   int		nImps;
   int		nLuts;
   int		nLines;
   int		nSamps;
   int		nCursors;
#ifdef VRDI
   char		Processor;
   char		Resolution;
   int		DefaultConfig[4];
   char		AlphaNumerics;
   char		Overlay;
   char		AutoTracking;
#endif /* VRDI */
   char		SystemAllocatable;
   char		Terminal[TERMINAL_SIZE+1];
   int		SystemNumber;
   char		LogicalName[LOGNAME_SIZE+1];
   }	*DIB[MAXIMUM_UNITS];

#ifdef TAE

#ifdef XD_INITIALIZE
PRIVATE int	TotalDevices = FALSE;
#else /* XD_INITIALIZE */
PRIVATE int	TotalDevices;
#endif /* XD_INITIALIZE */

#endif /* TAE */

#ifdef VRDI

#ifdef	XD_INITIALIZE
PRIVATE	int	xd_initialized = FALSE;
PRIVATE int	xd_warn_action  = 0;
PRIVATE	int	xd_error_action = 0;
PRIVATE	int	xd_fatal_action = 0;
PRIVATE int	NoUnit = MINUS_ONE;
PRIVATE int	TotalDevices = FALSE;
#else /* XD_INITIALIZE */
PRIVATE	int	xd_initialized;
PRIVATE int	xd_warn_action;
PRIVATE	int	xd_error_action;
PRIVATE	int	xd_fatal_action;
PRIVATE int	NoUnit;
PRIVATE int	TotalDevices;
#endif /* XD_INITIALIZE */

PRIVATE int	xd_current_call;
PRIVATE	int	(*access_window[MAXIMUM_UNITS])[4];
PRIVATE	int	(*display_window[MAXIMUM_UNITS])[2];
PRIVATE	int	(*zoom_factor[MAXIMUM_UNITS]);
PRIVATE	int	(*imp_to_lut[MAXIMUM_UNITS]);
PRIVATE	int	(*lut_section[MAXIMUM_UNITS]);
PRIVATE int	(*cursor_position[MAXIMUM_UNITS])[2];
PRIVATE int	(*cursor_form[MAXIMUM_UNITS]);
PRIVATE int	(*cursor_blink[MAXIMUM_UNITS]);
PRIVATE int	(*cursor_size[MAXIMUM_UNITS])[2];
PRIVATE int	(*cursor_color[MAXIMUM_UNITS])[3];
PRIVATE int	(*cursor_active[MAXIMUM_UNITS]);
PRIVATE int	(*lut_bypass[MAXIMUM_UNITS]);
PRIVATE int	(*lut_flag[MAXIMUM_UNITS]);
PRIVATE int	(*image_plane_flag[MAXIMUM_UNITS]);

#define	MAX_FONT_CHAR	127

PRIVATE	struct	{
   char		color;
   char		mask;
   int		precision;
   int		number;
   int		height;
   float	scale;
   double	sine;
   double	cosine;
   int		vCount[MAX_FONT_CHAR+1];
   char		*ptrMD[MAX_FONT_CHAR+1];
   float	*ptrX[MAX_FONT_CHAR+1];
   float	*ptrY[MAX_FONT_CHAR+1];
   float	cWidths[MAX_FONT_CHAR+1];
   }	xd_font_info;

#define	FONT_COLOR	xd_font_info.color
#define	FONT_MASK	xd_font_info.mask
#define	FONT_PRECISION	xd_font_info.precision
#define	FONT_NUMBER	xd_font_info.number
#define	FONT_SINE	xd_font_info.sine
#define	FONT_COSINE	xd_font_info.cosine
#define	FONT_HEIGHT	xd_font_info.height
#define	FONT_SCALE	xd_font_info.scale

#define DCB_INITIALIZED		DCB[*Unit]->Contents_Initialized
#define ZDCB_INITIALIZED	DCB[unit]->Contents_Initialized

#define DEV_NAME		DCB[*Unit]->Device_Name_Array
#define DEV_TYPE		DCB[*Unit]->Device_Type

#define ZDEV_NAME		DCB[unit]->Device_Name_Array
#define ZDEV_TYPE		DCB[unit]->Device_Type

#define N_LUTS			DCB[*Unit]->Number_LUTs
#define N_IMPS			DCB[*Unit]->Number_IMPs
#define N_LINES			DCB[*Unit]->Number_Lines
#define N_SAMPS			DCB[*Unit]->Number_Samples

#define ZN_LUTS			DCB[unit]->Number_LUTs
#define ZN_IMPS			DCB[unit]->Number_IMPs
#define ZN_LINES		DCB[unit]->Number_Lines
#define ZN_SAMPS		DCB[unit]->Number_Samples

#define AVAIL_CONFIGS		DCB[*Unit]->Available_Configs
#define ZAVAIL_CONFIGS		DCB[unit]->Available_Configs
#define	 IMP_512_BIT			0x00000001
#define	 IMP_1024_BIT			0x00000002
#define  IMP_640_480_BIT		0x00000004
#define  IMP_640_512_BIT		0x00000008
#define  IMP_1024_512_BIT		0x00000010
#define  IMP_1280_1024_BIT		0x00000020
#define	 IMP_2048_BIT			0x00000040
#define	 IMP_4096_BIT			0x00000080

#define	 VIDEO_512_BIT			0x00000100
#define	 VIDEO_1024_BIT			0x00000200
#define	 VIDEO_640_480_BIT		0x00000400
#define  VIDEO_640_512_BIT		0x00000800
#define  VIDEO_1024_512_BIT		0x00001000
#define  VIDEO_1280_1024_BIT		0x00002000

#define	 ASPECT_1_1_BIT			0x00010000
#define	 ASPECT_4_3_BIT			0x00020000

#define	 FULL_COLOR_BIT			0x00100000
#define	 PSEUDO_COLOR_BIT		0x00200000
#define	 BLACK_AND_WHITE_BIT		0x00400000

#define DEV_UNIT_NO		DCB[*Unit]->Device_Unit_Number
#define ZDEV_UNIT_NO		DCB[unit]->Device_Unit_Number

#define VALID_MODE		DCB[*Unit]->Valid_Display_Mode
#define ZVALID_MODE		DCB[unit]->Valid_Display_Mode
#define OUTPUT_MODE		DCB[*Unit]->Output_Mode
#define ZOUTPUT_MODE		DCB[unit]->Output_Mode
#define	 FULL_COLOR			1
#define	 PSEUDO_COLOR			2
#define	 BLACK_AND_WHITE		3

#define IMP_SIZE		DCB[*Unit]->IMP_Size
#define ZIMP_SIZE		DCB[unit]->IMP_Size
#define	 IMP_512			1
#define	 IMP_1024			2
#define  IMP_640_480			3
#define  IMP_640_512			4
#define  IMP_1024_512			5
#define  IMP_1280_1024			6
#define	 IMP_2048			7
#define	 IMP_4096			8

#define VIDEO_SIZE		DCB[*Unit]->Video_Size
#define ZVIDEO_SIZE		DCB[unit]->Video_Size
#define	 VIDEO_512			1
#define	 VIDEO_1024			2
#define	 VIDEO_640_480			3
#define  VIDEO_640_512			4
#define  VIDEO_1024_512			5
#define  VIDEO_1280_1024		6

#define ASPECT_RATIO		DCB[*Unit]->Aspect_Ratio
#define ZASPECT_RATIO		DCB[unit]->Aspect_Ratio
#define	 ASPECT_1_1			1
#define	 ASPECT_4_3			2

#define	VIDEO_LINES		DCB[*Unit]->Video_Lines
#define	VIDEO_SAMPLES		DCB[*Unit]->Video_Samples

#define	ZVIDEO_LINES		DCB[unit]->Video_Lines
#define	ZVIDEO_SAMPLES		DCB[unit]->Video_Samples

#define BATCH_MODE		DCB[*Unit]->Batch_Mode
#define ZBATCH_MODE		DCB[unit]->Batch_Mode

#define EACH_IMP_HAS_DW		DCB[*Unit]->Each_IMP_Has_DW
#define ZEACH_IMP_HAS_DW	DCB[unit]->Each_IMP_Has_DW

#define N_LUT_SECTIONS		DCB[*Unit]->Number_LUT_Sections
#define MAY_BYPASS_LUT		DCB[*Unit]->May_Bypass_LUTs
#define MAX_LUT_VALUE		DCB[*Unit]->Maximum_LUT_Value
#define EACH_IMP_ZOOMS		DCB[*Unit]->Each_IMP_Zooms
#define	MAY_CONNECT_IMP_LUT	DCB[*Unit]->May_Connect_IMP_LUT
#define	MAY_ZOOM_IMPS		DCB[*Unit]->May_Zoom_IMPs
#define	MAX_ZOOM_FACTOR		DCB[*Unit]->Max_Zoom_Factor

#define ZN_LUT_SECTIONS		DCB[unit]->Number_LUT_Sections
#define ZMAY_BYPASS_LUT		DCB[unit]->May_Bypass_LUTs
#define ZMAX_LUT_VALUE		DCB[unit]->Maximum_LUT_Value
#define ZEACH_IMP_ZOOMS		DCB[unit]->Each_IMP_Zooms
#define	ZMAY_CONNECT_IMP_LUT	DCB[unit]->May_Connect_IMP_LUT
#define	ZMAY_ZOOM_IMPS		DCB[unit]->May_Zoom_IMPs
#define	ZMAX_ZOOM_FACTOR	DCB[unit]->Max_Zoom_Factor

#define OVERLAY_AVAILABLE	DCB[*Unit]->Overlay_Available
#define OVERLAY_ON		DCB[*Unit]->Overlay_On
#define MAY_CONNECT_IMP_OVERLAY	DCB[*Unit]->May_Connect_IMP_Overlay
#define MAY_BYPASS_OVERLAY_LUT	DCB[*Unit]->May_Bypass_Overlay_LUT
#define OVERLAY_BYPASS		DCB[*Unit]->Overlay_Bypass
#define OVERLAY_IMP		DCB[*Unit]->Overlay_IMP
#define OVERLAY_LUT_SECTION	DCB[*Unit]->Overlay_LUT_Section
#define OVERLAY_LUT_CHAR	DCB[*Unit]->Overlay_LUT_Characteristic
#define BITS_IN_OVERLAY		DCB[*Unit]->Number_Bits_In_Overlay
#define	 SEPERATE_LUT		1
#define	 SAME_AS_IMP_LUT	2
#define N_OVERLAY_SECTIONS	DCB[*Unit]->Number_Overlay_LUT_Sections
#define DEFAULT_OVERLAY_LUT	DCB[*Unit]->Default_Overlay_LUT

#define ZOVERLAY_AVAILABLE		DCB[unit]->Overlay_Available
#define ZOVERLAY_ON			DCB[unit]->Overlay_On
#define ZMAY_CONNECT_IMP_OVERLAY	DCB[unit]->May_Connect_IMP_Overlay
#define ZMAY_BYPASS_OVERLAY_LUT		DCB[unit]->May_Bypass_Overlay_LUT
#define ZOVERLAY_BYPASS			DCB[unit]->Overlay_Bypass
#define ZOVERLAY_IMP			DCB[unit]->Overlay_IMP
#define ZOVERLAY_LUT_SECTION		DCB[unit]->Overlay_LUT_Section
#define ZOVERLAY_LUT_CHAR		DCB[unit]->Overlay_LUT_Characteristic
#define ZBITS_IN_OVERLAY		DCB[unit]->Number_Bits_In_Overlay
#define ZN_OVERLAY_SECTIONS		DCB[unit]->Number_Overlay_LUT_Sections
#define ZDEFAULT_OVERLAY_LUT		DCB[unit]->Default_Overlay_LUT

#define AFG_AVAILABLE		DCB[*Unit]->AFG_Available
#define AFG_ACTIVE		DCB[*Unit]->AFG_Active
#define N_AFG_LINES		DCB[*Unit]->Number_AFG_Lines
#define N_AFG_CHARACTERS	DCB[*Unit]->Number_AFG_Characters
#define N_AFG_CHARACTER_TYPES	DCB[*Unit]->Number_AFG_Character_Types

#define ZAFG_AVAILABLE		DCB[unit]->AFG_Available
#define ZAFG_ACTIVE		DCB[unit]->AFG_Active
#define ZN_AFG_LINES		DCB[unit]->Number_AFG_Lines
#define ZN_AFG_CHARACTERS	DCB[unit]->Number_AFG_Characters
#define ZN_AFG_CHARACTER_TYPES	DCB[unit]->Number_AFG_Character_Types

#define N_CURSORS		DCB[*Unit]->Number_Cursors
#define N_CURSOR_TYPES		DCB[*Unit]->Number_Cursor_Types
#define N_CURSOR_BLINK_RATES	DCB[*Unit]->Number_Cursor_Blink_Rates
#define MAX_CURSOR_XSIZE	DCB[*Unit]->Max_Cursor_XSize
#define MAX_CURSOR_YSIZE	DCB[*Unit]->Max_Cursor_YSize
#define MAY_RESIZE_CURSOR	DCB[*Unit]->May_Resize_Cursor
#define MAY_COLOR_CURSOR	DCB[*Unit]->May_Color_Cursor
#define	AUTO_TRACK_AVAILABLE	DCB[*Unit]->Auto_Track_Available
#define	AUTO_TRACK_DEVICE	DCB[*Unit]->Auto_Track_Device
#define	AUTO_TRACK_CURSOR	DCB[*Unit]->Auto_Track_Cursor

#define ZN_CURSORS		DCB[unit]->Number_Cursors
#define ZN_CURSOR_TYPES		DCB[unit]->Number_Cursor_Types
#define ZN_CURSOR_BLINK_RATES	DCB[unit]->Number_Cursor_Blink_Rates
#define ZMAX_CURSOR_XSIZE	DCB[unit]->Max_Cursor_XSize
#define ZMAX_CURSOR_YSIZE	DCB[unit]->Max_Cursor_YSize
#define ZMAY_RESIZE_CURSOR	DCB[unit]->May_Resize_Cursor
#define ZMAY_COLOR_CURSOR	DCB[unit]->May_Color_Cursor
#define	ZAUTO_TRACK_AVAILABLE	DCB[unit]->Auto_Track_Available
#define	ZAUTO_TRACK_DEVICE	DCB[unit]->Auto_Track_Device
#define	ZAUTO_TRACK_CURSOR	DCB[unit]->Auto_Track_Cursor

#define DEV_HAS_PROCESSOR	DCB[*Unit]->Device_Has_Processor
#define ZDEV_HAS_PROCESSOR	DCB[unit]->Device_Has_Processor

#define N_IO_DEVICES		DCB[*Unit]->Number_IO_Devices
#define ZN_IO_DEVICES		DCB[unit]->Number_IO_Devices

#define		NO_DEVICE	0
#define		DEVICE_1D	1
#define		DEVICE_2D	2
#define		DEVICE_3D	3
#define		DEVICE_SWITCH	4

#define		NO_COORDINATES	0
#define		ABSOLUTE	1
#define		RELATIVE	2

#define IO_DEV_1_TYPE		DCB[*Unit]->IO_Device_1_Type
#define IO_DEV_1_COORDINATES	DCB[*Unit]->IO_Device_1_Coordinates
#define IO_DEV_1_PEN		DCB[*Unit]->IO_Device_1_Pen
#define IO_DEV_1_N_SWITCHES	DCB[*Unit]->IO_Device_1_Number_Switches

#define ZIO_DEV_1_TYPE		DCB[unit]->IO_Device_1_Type
#define ZIO_DEV_1_COORDINATES	DCB[unit]->IO_Device_1_Coordinates
#define ZIO_DEV_1_PEN		DCB[unit]->IO_Device_1_Pen
#define ZIO_DEV_1_N_SWITCHES	DCB[unit]->IO_Device_1_Number_Switches

#define IO_DEV_2_TYPE		DCB[*Unit]->IO_Device_2_Type
#define IO_DEV_2_COORDINATES	DCB[*Unit]->IO_Device_2_Coordinates
#define IO_DEV_2_PEN		DCB[*Unit]->IO_Device_2_Pen
#define IO_DEV_2_N_SWITCHES	DCB[*Unit]->IO_Device_2_Number_Switches

#define ZIO_DEV_2_TYPE		DCB[unit]->IO_Device_2_Type
#define ZIO_DEV_2_COORDINATES	DCB[unit]->IO_Device_2_Coordinates
#define ZIO_DEV_2_PEN		DCB[unit]->IO_Device_2_Pen
#define ZIO_DEV_2_N_SWITCHES	DCB[unit]->IO_Device_2_Number_Switches

#define	LEFT	0
#define	TOP	1
#define	RIGHT	2
#define	BOTTOM	3

#define	AW( I )			access_window[*Unit][(I)-1]
#define	AW_LEFT( I )		access_window[*Unit][(I)-1][LEFT]
#define	AW_TOP( I )		access_window[*Unit][(I)-1][TOP]
#define	AW_RIGHT( I )		access_window[*Unit][(I)-1][RIGHT]
#define	AW_BOTTOM( I )		access_window[*Unit][(I)-1][BOTTOM]

#define	ZAW( I )		access_window[unit][(I)-1]
#define	ZAW_LEFT( I )		access_window[unit][(I)-1][LEFT]
#define	ZAW_TOP( I )		access_window[unit][(I)-1][TOP]
#define	ZAW_RIGHT( I )		access_window[unit][(I)-1][RIGHT]
#define	ZAW_BOTTOM( I )		access_window[unit][(I)-1][BOTTOM]

#define	DW( I )			display_window[*Unit][(I)-1]
#define	DW_LEFT( I )		display_window[*Unit][(I)-1][LEFT]
#define	DW_TOP( I )		display_window[*Unit][(I)-1][TOP]

#define	ZDW( I )		display_window[unit][(I)-1]
#define	ZDW_LEFT( I )		display_window[unit][(I)-1][LEFT]
#define	ZDW_TOP( I )		display_window[unit][(I)-1][TOP]

#define	ZOOM( I )		zoom_factor[*Unit][(I)-1]
#define	ZZOOM( I )		zoom_factor[unit][(I)-1]

#define	RED_LUT			1
#define	GREEN_LUT		2
#define	BLUE_LUT		3
#define	WHICH_IMP( L )		imp_to_lut[*Unit][(L)-1]
#define	WHICH_SECTION( L )	lut_section[*Unit][(L)-1]
#define BYPASS( L )		lut_bypass[*Unit][(L)-1]

#define	ZWHICH_IMP( L )		imp_to_lut[unit][(L)-1]
#define	ZWHICH_SECTION( L )	lut_section[unit][(L)-1]
#define ZBYPASS( L )		lut_bypass[unit][(L)-1]

#define CURSOR_ACTIVE( C )	cursor_active[*Unit][(C)-1]
#define	CURSOR_X( C )		cursor_position[*Unit][(C)-1][0]
#define	CURSOR_Y( C )		cursor_position[*Unit][(C)-1][1]
#define CURSOR_FORM( C )        cursor_form[*Unit][(C)-1]
#define CURSOR_BLINK( C )       cursor_blink[*Unit][(C)-1]
#define CURSOR_XSIZE( C )	cursor_size[*Unit][(C)-1][0]
#define CURSOR_YSIZE( C )	cursor_size[*Unit][(C)-1][1]
#define CURSOR_RED( C )		cursor_color[*Unit][(C)-1][0]
#define CURSOR_GREEN( C )	cursor_color[*Unit][(C)-1][1]
#define CURSOR_BLUE( C )	cursor_color[*Unit][(C)-1][2]

#define ZCURSOR_ACTIVE( C )	cursor_active[unit][(C)-1]
#define ZCURSOR_X( C )          cursor_position[unit][(C)-1][0]
#define ZCURSOR_Y( C )          cursor_position[unit][(C)-1][1]
#define ZCURSOR_FORM( C )       cursor_form[unit][(C)-1]
#define ZCURSOR_BLINK( C )      cursor_blink[unit][(C)-1]
#define ZCURSOR_XSIZE( C )	cursor_size[unit][(C)-1][0]
#define ZCURSOR_YSIZE( C )	cursor_size[unit][(C)-1][1]
#define ZCURSOR_RED( C )	cursor_color[unit][(C)-1][0]
#define ZCURSOR_GREEN( C )	cursor_color[unit][(C)-1][1]
#define ZCURSOR_BLUE( C )	cursor_color[unit][(C)-1][2]

#define	CHECK_UNIT_NUMBER		((*Unit >= 0) && (*Unit < MAXIMUM_UNITS))
#define	CHECK_DEVICE_OPEN		(xd_initialized && DCB[*Unit] != 0)
#define	CHECK_DEVICE_ACTIVE		DEV_ACTIVE
#define	CHECK_IMP( I )			((I >  0) && (I <= N_IMPS))
#define	CHECK_LUT( L )			((L >  0) && (L <= N_LUTS))
#define	CHECK_LUT_SECTION( S )		((S >  0) && (S <= N_LUT_SECTIONS))
#define	CHECK_OVERLAY_SECTION( S )	((S >= 0) && (S <= N_OVERLAY_SECTIONS))
#define	CHECK_CURSOR( C )		((C >  0) && (C <= N_CURSORS))
#define	CHECK_CURSOR_TYPE( F )		((F >=-1) && (F <= N_CURSOR_TYPES))
#define	CHECK_CURSOR_BLINK_RATE( B )	((B >= 0) && (B <= N_CURSOR_BLINK_RATES))
#define CHECK_CURSOR_XSIZE( X )		((X >= 1) && (X <= MAX_CURSOR_XSIZE))
#define CHECK_CURSOR_YSIZE( Y )		((Y >= 1) && (Y <= MAX_CURSOR_YSIZE))
#define CHECK_COLOR( C )		((C >= 0) && (C <= 255))
#define	CHECK_IO_DEVICE( D )		((D >  0) && (D <= N_IO_DEVICES))
#define	CHECK_ERROR_ACTION( A )		((A >  0) && (A <= NUM_ERRORS))
#define CHECK_GROUP_NUMBER		((*Group >= 0) && (*Group <= MAX_GROUPS))

#define	ZCHECK_UNIT_NUMBER		((unit >= 0) && (unit < MAXIMUM_UNITS))
#define	ZCHECK_DEVICE_OPEN		(xd_initialized && DCB[unit] != 0)
#define	ZCHECK_DEVICE_ACTIVE		ZDEV_ACTIVE
#define	ZCHECK_IMP( I )			((I >  0) && (I <= ZN_IMPS))
#define	ZCHECK_LUT( L )			((L >  0) && (L <= ZN_LUTS))
#define	ZCHECK_LUT_SECTION( S )		((S >  0) && (S <= ZN_LUT_SECTIONS))
#define	ZCHECK_OVERLAY_SECTION( S )	((S >  0) && (S <= ZN_OVERLAY_SECTIONS))
#define	ZCHECK_CURSOR( C )		((C >  0) && (C <= ZN_CURSORS))
#define	ZCHECK_CURSOR_TYPE( F )		((F >=-1) && (F <= ZN_CURSOR_TYPES))
#define	ZCHECK_CURSOR_BLINK_RATE( B )	((B >= 0) && (B <= ZN_CURSOR_BLINK_RATES))
#define ZCHECK_CURSOR_XSIZE( X )	((X >= 1) && (X <= ZMAX_CURSOR_XSIZE))
#define ZCHECK_CURSOR_YSIZE( Y )	((Y >= 1) && (Y <= ZMAX_CURSOR_YSIZE))
#define ZCHECK_COLOR( C )		((C >= 0) && (C <= 255))
#define	ZCHECK_IO_DEVICE( D )		((D >  0) && (D <= ZN_IO_DEVICES))
#define	ZCHECK_ERROR_ACTION( A )	((A >  0) && (A <= NUM_ERRORS))
#define ZCHECK_GROUP_NUMBER		((group >= 0) && (group <= MAX_GROUPS))

#define SET_FLAG( F )			F = F | invmask
#define CLEAR_FLAG( F )             	F = F & invmask
#define CHECK_FLAG( F )			(F & mask)

#define ZSET_FLAG( F )			F = F | invmask
#define ZCLEAR_FLAG( F )             	F = F & invmask
#define ZCHECK_FLAG( F )		(F & mask)

#define CONFIG_FLAG			DCB[*Unit]->Device_Config_Flag
#define GLUT_FLAG			DCB[*Unit]->Overlay_Lut_Flag
#define LUT_FLAG( L )			lut_flag[*Unit][L]
#define IMP_FLAG( I )			image_plane_flag[*Unit][I]

#define ZCONFIG_FLAG			DCB[unit]->Device_Config_Flag
#define ZGLUT_FLAG			DCB[unit]->Overlay_Lut_Flag
#define ZLUT_FLAG( L )			lut_flag[unit][L]
#define ZIMP_FLAG( I )			image_plane_flag[unit][I]

#define MAKE_LOGICAL( Val )	((Val) && 0x00000001)
#define	BIT_TEST( Val, BitNo )	(((Val) & (1 << (BitNo))) != 0)

#endif /* VRDI */

#endif
