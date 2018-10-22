/*****************************************************************************/
/* GLL_PH2_SSI_BIN.H                                                         */
/*    written by F. Moss, 11-Aug-1994                                        */
/*****************************************************************************/
#define DEFAULT_HOST     "VAX-VMS"     /* Defines the host type for the data */
                                       /* that is being written out.         */
#define GLL_PH2_SSI_EDR  "GLL_PH2_SSI_EDR" /* Binary label type for GLL SSI EDRs */
                                           /* and UDRs.                          */

/* statuses */

#define BAD_MALLOC       -1

/* In some cases, full-frame and sum-mode EDRs must be processed             */
/* differently, so there is a flag for each one. However, the structures     */
/* are declared the same for both (i.e. the number of pixels is 800 for each */
/* one, even though there's only 400 pixles in a SM EDR.)  Be sure to read   */
/* or write the correct number of data items if you are working with a       */
/* sum-mode EDR.                                                             */

#define FF_HALF_DATA     11   /* random numbers--no significance */
#define SM_HALF_DATA     22
#define BYTE_DATA        33    

#define NLINES           800     /* number of lines in a UDR or FF EDR image */
#define NSAMPS           NLINES  /* number of samples in a UDR or FF EDR image */

#define NLINES_SE        400        /* number of lines in a summation EDR img*/
#define NSAMPS_SE        400        /* number of samples in a summation EDR img*/

/*****************************************************************************/
/* Number of samples in the image line  header               (binary prefix) */
/*****************************************************************************/
#define BYTE_PREFIX_SZ        200                                

/*****************************************************************************/
/* These two #defines are useful in setting up calls for                     */
/* get_ssi_bdv_hdr_rec() and write_ssi_bdv_hdr_rec(). They indicate the      */
/* number of lines in the binary telemetry header, for UDRs and EDRs         */
/* respectively.  To get the first line in the bad data value header, check  */
/* the data format of the file (half or byte) to determine whether it's an   */
/* EDR or a UDR. Line #1 of the bdv header is UDR_THDR_RECS+1, if it's a UDR,*/
/* or EDR_THDR_RECS+1, if it's an EDR.                                       */
/*                                                                           */
/* Cut-off points:                                                           */
/* UDR:    1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*                                                                           */
/* FF EDR: 1800 bytes in record 1. (Whole header fits in one record.)        */
/*                                                                           */
/* SM EDR: 1000 bytes in record 1.   hdr->record_id through hdr->hist[55]    */
/*          800 bytes in record 2.   hdr->hist[56] through hdr->hist[255]    */
/*****************************************************************************/
#define UDR_THDR_RECS    2     /* # records in a UDR binary telemetry header */
#define FF_EDR_THDR_RECS 1     /* # recs in a full-frame EDR binary telemetry header */
#define SM_EDR_THDR_RECS 2     /* # recs in a sum-mode EDR binary telemetry header */

/*****************************************************************************/
/* General defines. */
#define ROUTINE          /* */
#define SUCCESS          1    
#ifndef TRUE
#define TRUE             1    
#endif
#ifndef FALSE
#define FALSE            0
#endif

/*****************************************************************************/
/* These are globals used for datatype conversion. */
/*
static int ph2_byte_trans[12], ph2_byte_size;
static int ph2_half_trans[12], ph2_half_size;
static int ph2_full_trans[12], ph2_full_size;
*/
/*****************************************************************************/
/* Convenience macros & routine prototypes to reduce typing */

/* For translating any format that's read in... (RGD) */
/*
#define TBYTE(from, to) zvtrans(ph2_byte_trans,(from),(to),1); (from)+=ph2_byte_size;
#define THALF(from, to) zvtrans(ph2_half_trans,(from),(to),1); (from)+=ph2_half_size;
#define TFULL(from, to) zvtrans(ph2_full_trans,(from),(to),1); (from)+=ph2_full_size;
#define TSTRN(from, to, n) zmove((from),(to),(n)); (from)+=(n);
*/
/* For writing out in native format.....*/
/*
#define TOBYTE(from, to) zvtrans(ph2_byte_trans,(from),(to),1); (to)+=ph2_byte_size;
#define TOHALF(from, to) zvtrans(ph2_half_trans,(from),(to),1); (to)+=ph2_half_size;
#define TOFULL(from, to) zvtrans(ph2_full_trans,(from),(to),1); (to)+=ph2_full_size;
#define TOSTRN(from, to, n) zmove((from),(to),(n)); (to)+=(n);
*/
/*****************************************************************************/
/* static function prototypes for translating any format that's read in      */

static void trin_scet_typ();         
static void trin_ert_typ();
static void trin_sclk_typ();
static void trin_flg_comp_typ();

/* static function prototypes for writing out in native format               */
static void trout_scet_typ();     
static void trout_ert_typ();
static void trout_sclk_typ();
static void trout_flg_comp_typ();

/* more static function prototypes                                           */
static void init_trans_out();
static void init_trans_inb();
static void init_pixsizeb();

void get_ssi_data_format();
char ph2_aline[80];                /* For status messages, etc. One text line.   */

/* end module */
