/*****************************************************************************/
/* GLL_SSI_BIN_DIFF.H                                                        */
/*    written by F. Moss, 11-Aug-1994                                        */
/*****************************************************************************/
/* In some cases, full-frame and sum-mode EDRs must be processed             */
/* differently, so there is a flag for each one. However, the structures     */
/* are declared the same for both (i.e. the number of pixels is 800 for each */
/* one, even though there's only 400 pixles in a SM EDR.)  Be sure to read   */
/* or write the correct number of data items if you are working with a       */
/* sum-mode EDR.                                                             */

#define FF_HALF_DATA     11   /* random numbers--no significance */
#define SM_HALF_DATA     22
#define BYTE_DATA        33    


/* These are globals used for datatype conversion. */
static int byte_trans[12], byte_size;
static int half_trans[12], half_size;
static int full_trans[12], full_size;
/*****************************************************************************/
/* Convenience macros & routine prototypes to reduce typing */

/* For translating any format that's read in... (RGD) */
#define TBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (from)+=byte_size;
#define THALF(from, to) zvtrans(half_trans,(from),(to),1); (from)+=half_size;
#define TFULL(from, to) zvtrans(full_trans,(from),(to),1); (from)+=full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);

/* For writing out in native format.....*/
#define TOBYTE(from, to) zvtrans(byte_trans,(from),(to),1); (to)+=byte_size;
#define TOHALF(from, to) zvtrans(half_trans,(from),(to),1); (to)+=half_size;
#define TOFULL(from, to) zvtrans(full_trans,(from),(to),1); (to)+=full_size;
#define TOSTRN(from, to, n) zmove((from),(to),(n)); (to)+=(n);
