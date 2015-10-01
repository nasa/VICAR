
#include "ivsinc:ivasdef.h"
#include "ivsinc:gphdef.h"
#include "ivsinc:gmdef.h"
#include "ivasdepinc:ivasstubdef.h"
#include "xaninc:standard.h"
#include "xaninc:errordef.h"

#define	IVAS_X_GPH(x)	((x)-1)
#define	IVAS_Y_GPH(y)	(N_LINES-(y))
#define IVAS_GPH_X(x)	((x)+1)
#define	IVAS_GPH_Y(y)	(N_LINES-(y))

#define	IVAS_X_IMG(x)	((x)-1)
#define	IVAS_Y_IMG(y)	((y)-1)
#define	IVAS_IMG_X(x)	((x)+1)
#define	IVAS_IMG_Y(y)	((y)+1)

#define MAKE_EVEN(Val) ((Val) & 0xFFFFFFFE)
#define ODD_TEST(Val) (((Val) & 1) == 1)

/*  The following macros are used in the IVASmaWriteRandom routine, which  */
/*  is part of the IVAS_VECTOR.C file.  The IVAS system include files on   */
/*  the AVIRIS do contain the following macros; those on MIPL do not.  To  */
/*  guarantee system portability, we have added the macros to the VRDI     */
/*  IVAS include file.                                                     */

#define InitBuffer(fixSize, actualSize) \
   SDIword _data[HeaderSize+fixSize], *_ptr = _data; \
   int _idx, _inWords = actualSize

#define SendBuffer \
   Write (_data, _ptr-_data, True, False)

#define ReceiveBuffer \
   Read (_ptr = _data, count, True, False)

#define HeaderSize	16

#define PackHeader(classID, moduleID, routineID, wait) \
   PackRaw (0xAA80 | ByteOrder << 1 | (wait isnot 0)); \
   PackRaw (_inWords); \
   PackRaw (moduleID << 8 | routineID); \
   PackRaw (classID)

/*  Since the IVAS does not permit direct Overlay Look-Up Table reads,  */
/*  we use the first sixteen elements in the device-dependent array in  */
/*  the DCB to store the current values in the LUT.  These values are   */
/*  accessed in IVAS_OVERLAY.C                                          */

/*  The IVAS cursor routines deal only with the center of the 128x128   */
/*  pixel space.  In a resizable cursor, however, we work with the      */
/*  upper left corner of the pixel area, while the IVAS routines deal   */
/*  with the center of the pixel area.  To aid in translating back and  */
/*  forth, we keep the current cursor form and size in the device-dep-  */
/*  endent array in the DCB.  These values are accessed in IVAS_CURSOR.C*/

#define IVAS_CURSOR_FORM		DCB[*Unit]->DeviceDependent[16]
#define IVAS_CURSOR_XSIZE		DCB[*Unit]->DeviceDependent[17]
#define IVAS_CURSOR_YSIZE		DCB[*Unit]->DeviceDependent[18]

/*  The IVAS cursor coloring routine can only accept four bits for each */
/*  of the primary colors--red, green, and blue.  The XDCCOLOR routine  */
/*  accepts eight bits for each, so we mask off the four most signifi-  */
/*  cant bits and use those when setting the cursor color.              */

#define IVAS_CURSOR_MASK		0xF0
