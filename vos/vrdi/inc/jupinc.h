#include <jgio.h>
#include <jconfig.h>

#if VMS_OS
#ifdef 	JUP_INITIALIZE
#define 	J_PRIVATE	globaldef noshare
#else
#define 	J_PRIVATE	globalref
#endif
#endif

#if UNIX_OS
#define J_PRIVATE
#endif

#define	JUP_X_IMG(x)	((x)-1)
#define	JUP_Y_IMG(y)	(N_LINES-(y))
#define	JUP_IMG_X(x)	((x)+1)
#define	JUP_IMG_Y(y)	(N_LINES-(y))

#define MAKE_EVEN(Val) ((Val) & 0xFFFFFFFE)
#define ODD_TEST(Val) (((Val) & 1) == 1)

/* Logical names for video definition files.  Set up by vicset1.com */

#define JUP_480_VIDEO_FILE	"JUP_480_VIDEO"
#define JUP_1024_VIDEO_FILE	"JUP_1024_VIDEO"

#define VIDEO_FILE	((VIDEO_SIZE==VIDEO_640_480) ? \
		 JUP_480_VIDEO_FILE : JUP_1024_VIDEO_FILE)

/* Save the current device state because j_config() doesn't know what	*/
/* the other processes are doing.  Used in jup_interface.  Same for	*/
/* j_cursor_sel().							*/

#define JUP_VIDEO (DCB[*Unit]->DeviceDependent[0])	/* int */
#define JUP_MOUSE (DCB[*Unit]->DeviceDependent[1])	/* int */
#define JUP_GPUNO (DCB[*Unit]->DeviceDependent[2])	/* int */

#define CONFIG_MODE (DCB[*Unit]->DeviceDependent[3])	/* int */
#define CONFIG_OVBITS (DCB[*Unit]->DeviceDependent[4])	/* int */
#define CONFIG_CURSOR (DCB[*Unit]->DeviceDependent[5])	/* int */

#define JUP_VIDEO_LINES (DCB[*Unit]->DeviceDependent[6])	/* int */

/* Define several variables that must be local to the process.	*/
/* However, they're different for every device so they must be	*/
/* indexed by Unit.						*/

J_PRIVATE int mbx_chan[MAXIMUM_UNITS];		/* must be per process */
#define MBX_CHAN (mbx_chan[*Unit])

J_PRIVATE struct shmem_jup *shmem_array[MAXIMUM_UNITS];	/* must be per process*/
#define SHMEM (shmem_array[*Unit])

#ifdef 	JUP_INITIALIZE
J_PRIVATE JFILE *J12file_array[MAXIMUM_UNITS] = 0;	/* zeros them all */
#else
J_PRIVATE JFILE *J12file_array[MAXIMUM_UNITS];
#endif
#define J12file (J12file_array[*Unit])

J_PRIVATE int local_mode[MAXIMUM_UNITS];	/* for config change checking */
J_PRIVATE int local_ovbits[MAXIMUM_UNITS];
J_PRIVATE int local_cursor[MAXIMUM_UNITS];	/* cursor image/overly select */

