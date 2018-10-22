#ifndef  MIPS_GLL_ICT_QQC_INCLUDED
#define  MIPS_GLL_ICT_QQC_INCLUDED 1


/* 
   This include file defines the SFDU header structures for the ICT 
   Compression QQC.
   
   History:

   Date                Reference       Description
   __________________  ______________  __________________________________
   19- 8- 1994                          Damon D. Knight - Original Delivery
                                                                            */

#include   "rts_typedefs.h"
#include   "gll_rts_main.h"

/* Compression Ratio Structure */

typedef struct
        {
         FIELD           s:1;             /* Sign (0:7)                     */
         FIELD           exponent:8;      /* Exponent (0:6-0,1:7)         */
         FIELD           mantissa:23;     /* Mantissa (1:6-0,2,3)           */
        }
        ieee_flt_pt_typ;


/* Record Creation Time */

typedef struct
        {
         UWORD           days;            /* Days since Jan. 1, 1958 (0-1)  */
         UWORD           milliseconds;    /* Millisecs of current day (2-3) */
        }
        sfdu_rct_typ;


/* Compression Slice Structure */

typedef struct
        {
         ieee_flt_pt_typ comp_ratio;       /* Compression Ratio (0-3)       */
         UBYTE           status;           /* Status of Decompression (4)   */
         UBYTE           qfactor;          /* Spare (5)                 */
        }
        comp_str_typ;

/* NIMS Compression Slice Structure */

typedef struct
        {
         UWORD           bytes_decomp;     /* Bytes Decompressed (0-1)      */
         UWORD           bytes_comp;       /* Bytes Compressed (2-3)        */
         UBYTE           status;           /* Status of Decompression (4)   */
         UBYTE           qfactor;          /* Spare (5)                     */
        }
        nims_comp_str_typ;


/* Primary Label Structure */

typedef struct
        {
         BYTE            author_id[4];     /* Control Authority ID (0-3)    */
         UBYTE           version_id;       /* Version ID (4)                */
         BYTE            class_id;         /* Identifies label class (5)    */
         UWORD           spares;           /* Each set to ASCII (6-7)       */
         BYTE            ddp_id[4];        /* Data Descr. Pack. ID (8-11)   */
         UINT            blk_length[2];    /* Length of the SFDU (12-19)    */
        }
        qqc_pri_lab_typ;


/* Subheader Aggregation CHDO Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Subhdr aggregation type (0-1)  */
         UWORD           chdo_length;      /* Length of all subheaders (2-3) */
        }
        qqc_aggr_typ;


/* Primary Header Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Tlm Pri Hdr CHDO type (0-1)    */
         UWORD           chdo_length;      /* Lgth of CHDO value field (2-3) */
         UBYTE           major;            /* SFDU major type (4)            */
         UBYTE           minor;            /* SFDU minor type (5)            */
         UBYTE           mission_id;       /* Mission identifier code (6)    */
         UBYTE           format;           /* SFDU format type (7)           */
        }
        qqc_pri_hdr_typ;


/* QQC Secondary Header Structure */

typedef struct
        {
         UWORD           chdo_type;        /* Secondary Hdr CHDO type (0-1)   */
         UWORD           chdo_length;      /* Lgth of CHDO value field (2-3)  */
         sfdu_rct_typ    rct;              /* Record Creation Time (4-9)      */
         sclk_typ        first_sclk;       /* SCLK of first slice (10-15)     */
         sclk_typ        last_sclk;        /* SCLK of last slice (16-21)      */
         UBYTE           num_of_slices;    /* Number of slices (22)           */
         UBYTE           spare1;           /* Spare 1 (23)     	              */
         UINT            spare2;           /* Spare 2 (24-27)                 */
         comp_str_typ    slice[255];   /* Compress. data slices (28-1557) */
        }
        qqc_sec_hdr_typ;

/* NIMS QQC Secondary Header Structure */

typedef struct
        {
         UWORD              chdo_type;       /* Secondary Hdr CHDO type (0-1)   */
         UWORD              chdo_length;     /* Lgth of CHDO value field (2-3)  */
         sfdu_rct_typ       rct;             /* Record Creation Time (4-9)      */
         sclk_typ           first_sclk;      /* SCLK of first slice (10-15)     */
         sclk_typ           last_sclk;       /* SCLK of last slice (16-21)      */
         UBYTE              num_of_slices;   /* Number of slices (22)           */
         UBYTE              spare1;          /* Spare 1 (23)     	              */
         UINT               spare2;          /* Spare 2 (24-27)                 */
         nims_comp_str_typ  nims_slice[255]; /* Compress. data slices (28-1557) */
        }
        nims_qqc_sec_hdr_typ;


/* QQC Header Structure */

typedef struct
        {
         qqc_pri_lab_typ pri_lab;
         qqc_aggr_typ aggr;
         qqc_pri_hdr_typ pri_hdr;
         qqc_sec_hdr_typ sec_hdr;
        }
        qqc_hdr_typ;

/* NIMS QQC Header Structure */

typedef struct
        {
         qqc_pri_lab_typ pri_lab;
         qqc_aggr_typ aggr;
         qqc_pri_hdr_typ pri_hdr;
         nims_qqc_sec_hdr_typ sec_hdr;
        }
        nims_qqc_hdr_typ;

/* Slice Information Structure */

typedef struct
        {
         UBYTE             found;
         UBYTE             comp_stat;
         float             compression;
        }
        slice_info_typ;

/* NIMS Slice Information Structure */

typedef struct
        {
         UBYTE             found;
         UBYTE             comp_stat;
         UWORD             bytes_decomp;
         UWORD             bytes_comp;
        }
        nims_slice_info_typ;

/* QQC Information Structure */

typedef struct
        {
         UBYTE             app_id;
         UWORD             rct_days;
         UWORD             rct_milliseconds;
         int               rim;
         UBYTE             mod91;
         UBYTE             mod10;
         UBYTE             mod8;
         int               last_rim;
         UBYTE             last_mod91;
         UBYTE             last_mod10;
         UBYTE             last_mod8;
         UBYTE             num_of_slices;
         UBYTE             telm_frmt;
         UBYTE             qfactor;
         slice_info_typ    slice[255];
        }
        qqc_info_typ;

/* NIMS QQC Information Structure */

typedef struct
        {
         UBYTE                  app_id;
         UWORD                  rct_days;
         UWORD                  rct_milliseconds;
         int                    rim;
         UBYTE                  mod91;
         UBYTE                  mod10;
         UBYTE                  mod8;
         int                    last_rim;
         UBYTE                  last_mod91;
         UBYTE                  last_mod10;
         UBYTE                  last_mod8;
         int                    sec_rim;
         UBYTE                  sec_mod91;
         UBYTE                  sec_mod10;
         UBYTE                  sec_mod8;
         int                    sec_last_rim;
         UBYTE                  sec_last_mod91;
         UBYTE                  sec_last_mod10;
         UBYTE                  sec_last_mod8;
         UBYTE                  num_of_slices1;
         UBYTE                  num_of_slices2;
         UBYTE                  telm_frmt;
         UBYTE                  qfactor;
         nims_slice_info_typ    nims_slice[364];
        }
        nims_qqc_info_typ;




#endif     
