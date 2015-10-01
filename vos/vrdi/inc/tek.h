#include "xvmaininc.h"
#include "tekinc:STI_CBIND.H"

#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#ifdef TEK_INITIALIZE
#define PRIVATE globaldef noshare
#else
#define PRIVATE globalref
#endif
#endif

#if UNIX_OS
#define PRIVATE
#endif

#define ODD_NUM(x)		((x) % 2)
#define POSITIVE_SIGN(x)	((x) & 0x10)

#ifndef MIN
#define MIN(x,y)	((x) < (y) ? (x) : (y))
#endif  /* MIN */
#ifndef MAX
#define MAX(x,y)	((x) > (y) ? (x) : (y))
#endif  /* MAX */

#define TEK_TRACKBALL		80
#define TEK_CURSOR		0

/*  These macros convert from Tek screen coordinates to Tek pixel  */
/*  coordinates and back.                                          */
#define TEK_SCREEN2PIXEL(x)	(int)(((5.0 * (float)(x)) / 16.0) + 0.5)
#define TEK_PIXEL2SCREEN(x)	(int)(((16.0 * (float)(x)) / 5.0) + 0.5)

/*  These macros convert from Tek pixel coordinates to VRDI  */
/*  coordinates and back.                                    */
#define X_TEK2VRDI(x)		(x+1)
#define X_VRDI2TEK(x)		(x-1)
#define Y_TEK2VRDI(y)		(N_LINES-(y))
#define Y_VRDI2TEK(y)		(N_LINES-(y))

#define STEREO_OFFSET		512

#define TEK_MAX_PLANES		24
#define TEK_MAX_COLORS		256
#define TEK_MAX_SCREEN_SPACE	3274
#define TEK_MAX_PIXEL_SPACE	1023

#define TEK_ON			1
#define TEK_OFF			0

/*  Constants used to set terminal mode (llcode)  */
#define TEK			0
#define TEK_ANSI		1
#define TEK_BINARY		6

/*  Constants used to set display mode (lldsmd)  */
#define TEK_SINGLE_BUFFER	1
#define TEK_INDEXED_COLOR	1
#define TEK_TRUE_COLOR		2
#define TEK_MONO_VIEW		1
#define TEK_STEREO_VIEW		2

/*  Constants used to set color mode (llclmd)  */
#define TEK_MACHINE_RGB		4
#define TEK_OPAQUE		1
#define TEK_ADDITIVE		3
#define TEK_NORMAL_COLOR	1
#define TEK_MONOCHROME		2

/*  Constants used to set line types (lllntp)  */
#define TEK_PIXEL_WIDE		0
#define TEK_HOT_DOG		1
#define TEK_RECTANGULAR		2

/*  Constants used to set line styles (lllnst)  */
#define TEK_SOLID		0

/*  Constants used to mask the 8-bit R, G, and B values when  */
/*  calculating true color pixel encoding.  (llrasw)          */
#define TEK_MASK0		0xFC
#define TEK_MASK1		0x03
#define TEK_MASK2		0xF0
#define TEK_MASK3		0x0F
#define TEK_MASK4		0xC0
#define TEK_MASK5		0x3F

/*  Constants used to define image planes  */
#define TEK_RESET		0
#define TEK_RED			1
#define TEK_GREEN		2
#define	TEK_BLUE		3
#define TEK_OVERLAY		4
#define TEK_RIGHT_IMAGE		0
#define TEK_LEFT_IMAGE		1

/*  Constants used to mask the 8-bit R, G, and B values from  */
/*  the 24-bit Tektronix index  (lpfbw)                       */

#ifdef TEK_INITIALIZE
PRIVATE long PXSHIFT[] = { 0x00, 0x00, 0x08, 0x10, 0x00 };
PRIVATE long PXMASK[] = { 0xFFFFFF, 0x0000FF, 0x00FF00, 0xFF0000, 0x0F };
#else
PRIVATE long PXSHIFT[];
PRIVATE long PXMASK[];
#endif

#define TEK_RED_MASK		0xFF0000l
#define TEK_GREEN_MASK		0x00FF00l
#define TEK_BLUE_MASK		0x0000FFl

/*  Constants used in drawing curves (lldcve)  */
#define ARC			1
#define CHORD			2
#define PIE			3

/*  Constants used in inserting graphics primitives into segments (lliisg)  */
#define END_OF_SEGMENT		-1
#define BEGIN_OF_SEGMENT	1
#define BEFORE_UNIT		0
#define END_OF_UNIT		1
#define AFTER_UNIT		2

/*  Constants used in view commands (llrnvw)  */
#define TEK_CURRENT_VIEW	0
#define TEK_ALL_VIEWS		-1

/*  Constants used in setting segment mode (llwmsg)  */
#define FUTURE_SEGMENTS		-2
#define ALL_SEGMENTS		-1
#define TEK_SET_MODE		1
#define TEK_XOR_MODE		2
#define TEK_AND_MODE		3
#define TEK_OR_MODE		4

/*  Constants used in setting pixel modes (llbpxl)  */
#define TEK_NO_CHANGE		0
#define TEK_XOR_PIXELS		7
#define TEK_SET_PIXELS		11
#define TEK_AND_PIXELS		12
#define TEK_OR_PIXELS		15
#define TEK_ADD_PIXELS		17
#define TEK_SUBTRACT_PIXELS	18
#define TEK_BITS_PER_PIXEL	12
#define TEK_ALL_SURFACES	-1

/*  Constants used to set screen coordinate mode (llcord)  */
#define MODE12BIT		0
#define MODE32BIT		1
#define MODE24BIT		2

#define REPORT_SIZE		0

/*  Constants used to set dialog area color index  (lldain)  */
#define TRUE_WHITE		16777215l
#define INDEX_WHITE		255

/*  Constants used to set ALU operating mode  (lpalu)  */
#define ALU_XOR			6
#define ALU_AND			8
#define ALU_SET			12
#define ALU_OR			14
#define ALU_ADD			16
#define ALU_SUBTRACT		17

/*  Constant used to tell raster write the number of pixels per word  */
#define PIXELS_PER_WORD		4

/*  Constants used to control graphics pipe  (lppipe)  */
#define PIPE_MASK		0x8000
#define PIPE_MODE		0x0000

/*  Constants used in saving and restoring pixels  (llrpfm)  */
#define TEK_BUFFER		1
#define MOVE_RELEASE		2

/*  Constants used to select frame buffer to read from/write to  (lpsfb)  */
#define FRAME_BUFFER_1		1
#define FRAME_BUFFER_2		2
#define TEK_OVERLAY_PLANES	4

/*  Constants used in translating string received from terminal after an  */
/*  llsave command.                                                       */
#define TEK_HEAD_OFFSET		32
#define TEK_EOLN		18

/*  Software image plane look-up tables  */
PRIVATE unsigned char (*TEK_LUTS);
PRIVATE unsigned char (*TEK_LUTS_3D_LEFT);
PRIVATE unsigned char (*TEK_LUTS_3D_RIGHT);

#define TEK_LUT(lut, color) \
	(*(TEK_LUTS + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))
#define TEK_LUT_3DL(lut, color) \
	(*(TEK_LUTS_3D_LEFT + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))
#define TEK_LUT_3DR(lut, color) \
	(*(TEK_LUTS_3D_RIGHT + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))

/*  Software overlay plane look-up tables  */
PRIVATE unsigned char (*TEK_OVLY_LUTS);
PRIVATE unsigned char (*TEK_OVLY_LUTS_LEFT);
PRIVATE unsigned char (*TEK_OVLY_LUTS_RIGHT);

#define TEK_OVLY_LUT(lut, color) \
	(*(TEK_OVLY_LUTS + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))
#define TEK_OVLY_LUT_L(lut, color) \
	(*(TEK_OVLY_LUTS_LEFT + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))
#define TEK_OVLY_LUT_R(lut, color) \
	(*(TEK_OVLY_LUTS_RIGHT + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))

/*  Software image memory planes  */
PRIVATE unsigned char (*TEK_IMPS);
PRIVATE unsigned char (*TEK_IMPS_3D_LEFT);
PRIVATE unsigned char (*TEK_IMPS_3D_RIGHT);

#define TEK_IMP(imp, x, y) \
(*(TEK_IMPS + (((imp)-1)*N_SAMPS*N_LINES) + (((y)-1)*N_SAMPS) + ((x)-1)))
#define TEK_IMP_3DL(imp, x, y) \
(*(TEK_IMPS_3D_LEFT + (((imp)-1)*N_SAMPS*N_LINES) + (((y)-1)*N_SAMPS) + ((x)-1)))
#define TEK_IMP_3DR(imp, x, y) \
(*(TEK_IMPS_3D_RIGHT + (((imp)-1)*N_SAMPS*N_LINES) + (((y)-1)*N_SAMPS) + ((x)-1)))

/*  Since refreshing the screen takes so long, we would like to avoid  */
/*  doing it whenever possible.  Therefore, we keep track of the LUT   */
/*  connections, the zoom factors, the display window locations, and   */
/*  the rotation angles--if they do not change, we do not need to re-  */
/*  fresh the screen.                                                  */

#define TEK_WHICH_IMP(lut)	DCB[*Unit]->DeviceDependent[(lut)-1]
#define TEK_ZOOM(imp)		DCB[*Unit]->DeviceDependent[(imp)+2]
#define TEK_DW_LEFT(imp)	DCB[*Unit]->DeviceDependent[(imp)+6]
#define TEK_DW_TOP(imp)		DCB[*Unit]->DeviceDependent[(imp)+10]
#define TEK_ANGLE(imp)		DCB[*Unit]->DeviceDependent[(imp)+14]
#define TEK_WRITTEN(imp)	DCB[*Unit]->DeviceDependent[(imp)+18]
#define TEK_OVERLAY_ON		DCB[*Unit]->DeviceDependent[23]
#define TEK_OVERLAY_IMP		DCB[*Unit]->DeviceDependent[24]

/*  Constants used when calling TEK_AREA  */
#define TEK_WRITE_AREA		0
#define TEK_REFRESH_AREA	1
#define TEK_LUT_AREA		2
#define TEK_FILL_AREA		3
#define TEK_REFRESH_SCREEN	4

/*  Constants used to determine rotation angle of an image plane, used in  */
/*  TEK_Rotate().                                                          */
#define TEK_NO_ANGLE		0
#define TEK_180			1
#define TEK_MINUS90		2
#define TEK_90			3

/*  Constants used to set the minimum error level at which error messages  */
/*  will be displayed  (llerth)                                            */
#define ERR_LEVEL_0		0
#define ERR_LEVEL_1		1
#define ERR_LEVEL_2		2
#define ERR_LEVEL_3		3
#define ERR_LEVEL_4		4
#define ERR_LEVEL_5		5
