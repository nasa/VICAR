/*****************************************************************************/
/* GLL_SSI_BIN.H                                                             */
/* Bad data value header interface code                                      */
/*    written by M. O'Shaughnessy, 2-Nov-1992                                */
/* 30may96  -lwk- fixed NSAMPS_SE					     */
/*****************************************************************************/
#define DEFAULT_HOST     "VAX-VMS"     /* Defines the host type for the data */
                                       /* that is being written out.         */
#define GLL_SSI_EDR      "GLL_SSI_EDR" /* Binary label type for GLL SSI EDRs */
                                       /* and UDRs.                          */
/* statuses */

#define BAD_MALLOC       -1
#define NO_BDVH          -2 /* No bad data value header was found */
#define END_BDVH         -3 /* End of binary header reached successfully */
                            /* (Image data was found.) */
#define OVERRUN_BDVH     -4 /* binary header exceeded NLB */
#define EMPTY_BDVH_REC   -5 /* blank bdvh line */

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
#define NSAMPS           NLINES  /* number of samples in a UDR or FF EDR image*/

#define NLINES_SE        400     /* number of lines in a summation EDR img*/
#define NSAMPS_SE        400	 /* number of samples in a summation EDR img */

/* Structure & defines for the GLL EDR/UDR binary bad data value header */
/* These #defines identify the type of bad data in a particular bdv rec */

#define BDV_EMPTY_REC       0
#define BDV_IMAGE_DATA      2     /* If you get this one, you've run out of 
                                   bdv rec's and are in image data. 
                                   Reference: UDR and EDR SIS */
#define BDV_DATA_DROPOUT    3
#define BDV_SATURATED_PX    4
#define BDV_LFW_PX          5
#define BDV_SINGLE_PX_SPIKE 6
#define BDV_RS_OVERFLOW     7

/* These #defines identify whether a bad data group is a single pixel,  */
/* a line segment, or a column (sample) segment. They are used with the */
/* "code" field of the bdvh structure, shown below.                     */

#define BDV_SPX             1
#define BDV_LINE_SEG        2
#define BDV_SAMP_SEG        3
    
/* Maximum number of element coordinates in a bdv record.  Only the      */
/* largest set (for FFE) is used to dimension arrays, but the others are */
/* provided for completeness.                                            */

/* The "E" in FFE and SME indicates EDR format */
#define MAX_FFE_SPX_OBJ     448 /* full frame single pixel errors        */
#define MAX_FFE_LINE_OBJ    299 /* full frame line segemnt               */
#define MAX_FFE_COL_OBJ     299 /* full from column segment              */

#define MAX_SME_SPX_OBJ     248 /* summation mode single pixel errors    */
#define MAX_SME_LINE_OBJ    165 /* summation mode line segment           */
#define MAX_SME_COL_OBJ     165 /* summation mode column segment         */

/* The "U" in FFU and SMU indicates UDR format */
#define MAX_FFU_SPX_OBJ     248 /* full frame single pixel errors        */
#define MAX_FFU_LINE_OBJ    165 /* full frame line segemnt               */
#define MAX_FFU_COL_OBJ     165 /* full from column segment              */

#define MAX_SMU_SPX_OBJ     248  /* summation mode single pixel errors   */
#define MAX_SMU_LINE_OBJ    164  /* summation mode line segment          */
#define MAX_SMU_COL_OBJ     164  /* summation mode column segment        */

/* the structure below can be used for any of the four formats: ff or    */
/* summation mode edrs or udrs. The data arrays have a maximum size equal */
/* to the dimensions of the largest required arrays (for ffe).           */

typedef struct {
  short record_id;               /* identifies the type of bad data */
  short code;                    /* 1=single pix, 2=line seg, 3=samp seg */
  short nobjs;                   /* number of objects in the list */
  union error_coords {
    struct single_pixel_element {  /* coordinates of a single pixel error */
      short line;
      short sample;
    } pixel_data [MAX_FFE_SPX_OBJ];
    struct line_element {         /* coords of a line segment */
      short line;
      short ss;
      short ns;
    } line_data [MAX_FFE_LINE_OBJ];
    struct column_element {      /* coords of a sample segment */
      short sample;
      short sl;
      short nl;
    } column_data [MAX_FFE_COL_OBJ];
  } coords;
} ssi_bdvh_typ; 

/*****************************************************************************/
/* Number of samples in the image line bad-data value header (binary prefix) */
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
#define KEY_SZ           8    
#define SUCCESS          1    
#define FAILURE          0
#define TRUE             1    
#define FALSE            0

/*****************************************************************************/
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

/*****************************************************************************/
/* static function prototypes for translating any format that's read in      */

static void trin_scet_typ();         
static void trin_ert_typ();
static void trin_sclk_typ();
static void trin_ssi_lrs_p_typ();
static void trin_ssi_flg_typ();
static void trin_j2000_ascii_typ();

/* static function prototypes for writing out in native format               */
static void trout_scet_typ();     
static void trout_ert_typ();
static void trout_sclk_typ();
static void trout_ssi_lrs_p_typ();
static void trout_ssi_flg_typ();
static void trout_j2000_ascii_typ();

/* more static function prototypes                                           */
static void init_trans_out();
static void init_trans_inb();
static void init_pixsizeb();

void get_ssi_data_format();
char aline[80];                /* For status messages, etc. One text line.   */

/* end module */
