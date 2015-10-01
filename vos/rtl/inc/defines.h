#ifndef DEFINES_H
#define DEFINES_H

/* The following should use VMS_OS but some old apps don't include xvmaininc */
#ifdef vms		/* For compatibility with old applications only! */
#include "v2$inc:applic.h"		/* Remove as soon as possible! */
#include "v2$inc:errdefs.h"
#else
#include "applic.h"
#include "errdefs.h"
#endif
#include "xvmaininc.h"

/* The following must be maintained in parallel with the TAE include	*/
/* file vicartae.inp.							*/

#define MAXTAPES	15	/* Size of tape table			*/
#define TAPEDEVSIZE	64	/* Max chars in tape device spaec	*/
#define TAPENAMESIZE	15	/* Max chars in tape symbolic name	*/

#define I_TAPE		0	/* codes returned from i_analyze()	*/
#define I_DISK		1
#define I_SYNERR	2

/* End of vicartae.inp parallel						*/

/* The following must be maintained in parallel with the TAE include	*/
/* file taeconf.inp.							*/

#define NAMESIZE	15		/* same as NAMESIZ */

/* End of taeconf.inp parallel						*/

/* The following must be maintained in parallel with the TAE include	*/
/* file symtab.inc.							*/

#define PARM_V_INTEGER	1		/* same as V_INTEGER */
#define PARM_V_REAL	2		/* same as V_REAL */
#define PARM_V_STRING	3		/* same as V_STRING */

#define PARM_V_EOF	-1		/* for PARMS files; no TAE equiv */
#define PARM_V_REAL8	0		/* ditto */

/* End of symtab.inc parallel						*/


/* Max counts */

#define N_HISTORY_ITEMS		3	/* # of entries in 'history' global */
#define N_UNIT_TABLE_ENTRIES	83	/* # of entries in current_table */
#define N_LABEL_TABLE_ENTRIES	17	/* # of entries in label_table */
#define N_ACTIVE_UNITS_ALLOWED	2000	/* max # of active units */
#define N_COMPRESSION_TYPES	1	/* # of compressions available */
#define MAX_TASKS		200	/* max # of tasks in a label */
#define MAX_PROPS		200	/* max # of properties in a label */
/* Note that the RTL doesn't care about MAX_TASKS or MAX_PROPS; they are */
/* just there to assist applications in statically allocating buffers.   */
#define MAX_IMAGE_DIMENSION	4
#define MAX_LABEL_LEVELS	10


/* Max lengths */

#define PARAM_NAME_LENGTH	9	/* max parameter name length */
#define MAX_STRING_SIZE		132
#define MAX_SHORT_STRING_SIZE	11	/* Three longwords without NULL   */
#define MAX_LABEL_ITEM_SIZE	6120
#define MAX_SIMPLE_LABEL_ITEM_SIZE 250
#define MAX_LABEL_VALUE_SIZE	250
#define MAX_LABEL_KEY_SIZE	32
#define COMPRESSION_NAME_LEN	16	/* max len of compression name */
/* FN size was 120.  NOTE:  VMS code has NOT been modified to handle >120! */
#define MAX_FILE_NAME_SIZE	250
#define MAX_HIST_NAME_SIZE	8
#define MAX_PIXEL_SIZE		8
#define LABEL_SIZE_ITEM		24	/* see SYSTEM_KEY_TEMPLATE */


/* Buffer sizes */

#define MAX_DISK_RECSIZE	32767	/* maximum size of RMS disk record */
#define MAX_ANSI_RECSIZE	32767	/* maximum size of ANSI tape record */
#define MAX_TAPE_BLOCKSIZE	65534	/* max size of foreign tape block */
#define AVG_TAPE_BLOCKSIZE	8192	/* size to use when record's too big */

#define LABEL_BUFFER_EXTRA	MAX_LABEL_ITEM_SIZE + 1
					/* extra space for label expansion */

#define EXTRA_FILE_SIZE		1024	/* Extra room in file allocation size */
#define APPROX_BUFFER_SIZE	20000
#define BUFFER_INCREMENT	5000
#define RANDOM_BUFFER_SIZE	1024


/* Miscellaneous flag values */

#define READ_OK	  0			/* for det_tape_blksize */
#define DONT_READ 1

#define C_LANG		0		/* for applic_lang */
#define FORTRAN_LANG	1

#define NO_UNIT -1		/* for error_handler, errors with no unit */


/* Status codes */

#define RMS_SUCCESS 0x10001


/* Strings */

#define PROCESS_ID_LOGICAL	"V2$PIDCODE"	/* if exits, holds process id */
#define PRIMARY_NAME		"PxYz0aBc"	/* a name unlikely to be used */
#define TASK_KEY		"TASK"
#define PROPERTY_KEY		"PROPERTY"

#define SYSTEM_KEY		"LBLSIZE"
#define SYSTEM_KEY_TEMPLATE "LBLSIZE=                " /* see LABEL_SIZE_ITEM */

#define DEF_HOST_LABEL		"VAX-VMS"
#define DEF_INTFMT_LABEL	"LOW"
#define DEF_REALFMT_LABEL	"VAX"


/* Macros */

#define CURRENT_VALUE(x)	current_table[unit][x].value
#define CURRENT_S_VALUE(x)	current_table[unit][x].pvalue
#define CURRENT_I_VALUE(x)	current_table[unit][x].ivalue
#define CURRENT_IP_VALUE(x)	current_table[unit][x].ipvalue
#define CURRENT_PP_VALUE(x)	current_table[unit][x].ppvalue
#define PRIMARY_VALUE(x)	current_table[primary_input_unit][x].value
#define PRIMARY_S_VALUE(x)	current_table[primary_input_unit][x].pvalue
#define PRIMARY_I_VALUE(x)	current_table[primary_input_unit][x].ivalue
#define PRIMARY_IP_VALUE(x)	current_table[primary_input_unit][x].ipvalue
#define LABEL_VALUE(x)		label_table[unit][x].value
#define LABEL_S_VALUE(x)	label_table[unit][x].pvalue
#define LABEL_I_VALUE(x)	label_table[unit][x].ivalue
#define LABEL_IP_VALUE(x)	label_table[unit][x].ipvalue
#define LAZY_INDEX(x)           ((long*)(current_table[unit][78].ipvalue))[x]
#define COMPRESSED              !EQUAL(current_table[unit][77].pvalue, "NONE")

#define CEIL(x,y)		(((x)+(y)-1) / (y))

#define BINARY_ACCESS	((int)CURRENT_I_VALUE(FLAGS) & BINARY)
#define SEQ_DEVICE	((int)CURRENT_I_VALUE(FLAGS) & SEQUENTIAL_DEVICE)


/* Supported device types */

#define DEV_DISK	0
#define DEV_TAPE	1
#define DEV_ANSI	2
#define DEV_MEMORY	3
#define DEV_ARRAY	4
#define DEV_DECNET	5


/* Indices into history structure */

#define TASK 0
#define USER 1
#define DAT_TIM 2


/* Indices into the unit table */

#define LINE		0
#define SAMP		1
#define COND		2
#define BAND		3
#define NLINES		4
#define NSAMPS		5
#define NBANDS		6
#define ADDRESS         7
#define SLICE1		8
#define SLICE2		9
#define SLICE3		10
#define SLICE4		11
#define NSLICE1		12
#define NSLICE2		13
#define NSLICE3		14
#define NSLICE4		15
#define U_NL		16
#define U_NS		17
#define U_NB		18
#define U_N1		19
#define U_N2		20
#define U_N3		21
#define U_N4		22
#define U_ORG		23
#define OP		24			/* to the OP item */
#define OPEN_ACT	25
#define IO_ACT		26
#define CLOS_ACT	27
#define FORMAT		28
#define U_FORMAT	29
#define OPEN_MES	30
#define IO_MESS		31
#define TYPE		32
#define FLAGS		33		/* an index to the FLAG indicator*/
#define NAME		34		/* an index to the NAME item     */
#define BUFSIZE		35
#define DIM		36
#define U_DIM		37
#define EOL		38
#define RECSIZE		39
#define VARSIZE		40		/* size of var length record on read */
#define ORG		41
#define NL		42
#define NS		43
#define NB		44
#define N1		45
#define N2		46
#define N3		47
#define N4		48
#define IMG_REC		49
#define LBLSIZE		50
#define BUFSTATE	51
#define PIX_SIZE	52
#define LABELS		53
#define LBLALLOC	54		/* alloc'ed size of LABELS */
#define O_FORMAT	55
#define I_FORMAT	56
#define U_NAME		57
#define EOL_SIZE	58
#define NBB		59
#define NLB		60
#define U_NBB		61
#define U_NLB		62
#define U_FILE		63
#define METHOD		64
#define LAB_ACT		65
#define LAB_MESS	66
#define HOST		67
#define INTFMT		68
#define REALFMT		69
#define CONVERT		70
#define BHOST		71
#define BINTFMT		72
#define BREALFMT	73
#define BIN_CVT		74
#define BLTYPE		75
#define UPD_HIST        76
/* The following are for compression */
#define COMPRESS	77
#define LAZYINDEX       78
#define ENCODED_BUF     79
#define DECODED_BUF     80
#define EOCI1           81
#define EOCI2           82

/* Indices into the label table */

#define HIST		0
#define INSTANCE	1
#define LFORMAT		2
#define LEVEL		3
#define LENGTH		4
#define ELEMENT		5
#define NELEMENTS	6
#define NRET		7
#define ULEN		8
#define ERR_ACT		9
#define ERR_MESS	10
#define MODE		11
#define STRLEN		12
#define PROPERTY	13
#define INST_NUM	14
#define MOD		15
#define POSITION	16


/* Defines used in unit_table and label_table globals */

#define STRING	0			/* string type parameter */
#define REAL	1			/* real type parameter */
#define INTEGER	2			/* integer type parameter */
#define ADDR	3			/* addr type parameter */
#define INTADDR	4			/* pointer to integer type parameter */
#define MESSAGE	5			/* message (long string) parameter */

#define INPUT	0			/* input to xv routine */
#define OUTPUT	1			/* output from xv routine */                    /* table entry. INPUT and OUTPUT  */
#define SYSTEM	2			/* system label item */
#define LOCAL	3			/* internal only */


/* Unit table access bits */

#define O	0x0001			/* parameter accessible from open */
#define R	0x0002			/* read */
#define W	0x0004			/* write */
#define A	0x0008			/* add */
#define G	0x0010			/* get */
#define C	0x0020			/* close */
#define U	0x0040			/* unit */

#define I	0x0002			/* (label) info */
#define D	0x0004			/* (label) delete */
#define HI	0x0020			/* (label) hinfo */
#define NI	0x0040			/* (label) ninfo */
#define RI	0x0080			/* (label) pinfo (note RI not PI) */

/* Flag bits for the FLAGS entry in current_table */

#define OPEN		0x0001		/* file is open */
#define NO_LABELS	0x0002		/* labels are not present after open */
#define LABELS_MODIFIED	0x0010		/* labels have been modified */
#define UNIT_IS_TAPE	0x0020		/* unit is a tape */
#define DATA_WRITTEN	0x0040		/* first logical data record written */
#define NOBLOCK		0x0080		/* tapes will be unblocked */
#define BINARY		0x0200		/* binary labels are visible */
#define VARREC		0x0400		/* variable length records on tape */
#define SEQUENTIAL_DEVICE 0x4000	/* Device is sequential-only access */


/* current_call values: bits 0-7 = routine, bit 8 (0x0100) = no unit	*/
/* associated with routine, bit 9 (0x0200) = err_flag, bit 10 (0x400) =	*/
/* io_flag, bit 11 (0x0800) = open_flag					*/

#define VADD		0x0800
#define VCLO		0x0801
#define VGET		0x0802
#define VOPE		0x0803
#define VREA		0x0404
#define VWRI		0x0405
#define LADD		0x0206
#define LDEL		0x0207
#define LGET		0x0208
#define LHIN		0x0209
#define LINF		0x020A
#define LNIN		0x020B
#define VUNI		0x010C
#define VCMD		0x010D
#define VPARM		0x010E
#define VIPARM		0x010F
#define VP		0x0110
#define VPCNT		0x0111
#define VIPCNT		0x0112
#define VPSTAT		0x0113
#define VIPSTAT		0x0114
#define VPONE		0x0115
#define VIPONE		0x0116
#define LGETLABEL	0x0417
#define VPARMD		0x0118
#define VIPARMD		0x0119
#define VTRANS_SET	0x011A
#define VTRANS_IN	0x011B
#define VTRANS_OUT	0x011C
#define VTRANS_INU	0x081D
#define VPIXSIZEU	0x081E
#define VPIXSIZE	0x011F
#define VTPINFO		0x0120
#define VTPMODE		0x0821
#define VTPSET		0x0122
#define VFILPOS		0x0823
#define VCMDOUT		0x0124
#define VIP		0x0125
#define VPOPEN		0x0826
#define VPOUT		0x0427
#define VHOST		0x0128
#define VPIXSIZEB	0x0829
#define VTRANS_INB	0x082A
#define LPINFO		0x022B

/* Bufstate flags */

#define BUF_VALID 0x01	/* TRUE if buffer is valid (i.e. it holds real data */
#define BUF_DIRTY 0x02	/* TRUE if buffer contains unwritten data */
#define BUF_ALIGN 0x04	/* TRUE if we always read/write on bufsize boundaries */
#define BUF_SEQ   0x08	/* TRUE if we use the sequential method on the buffer */
#define BUF_CACHE 0x10	/* TRUE if we are going to use read caching */
#define BUF_VARREC 0x20	/* TRUE if variable length record */
#define BUF_NOPREREAD 0x40 /* TRUE if device can't pre-read before a write */

#endif /* DEFINES_H */
