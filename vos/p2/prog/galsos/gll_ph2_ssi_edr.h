#ifndef  MIPS_GLL_PH2_SSI_EDR_INCLUDED
#define  MIPS_GLL_PH2_SSI_EDR_INCLUDED 1

#include "rts_typedefs.h"
#include "gll_rts_main.h"
#include "gll_decompression.h"
#include "gll_ict_qqc.h"   

/**************************************************************************** 
 * This include file defines the UDR and EDR structure for Galileo Phase II *
 * telemetry records.                                                       *
 *                                                                          *   
 * History:                                                                 *
 *                                                                          *
 * Date                Reference       Description                          *
 * __________________  ______________  __________________________________   *
 * 18- 4-1994          UDR SISes       Damon D. Knight - Original Delivery  *
 * Mar 4 1995          FR 85639        GMY-Change EDR format from UBYTE to  *
 *                                     short int.                           *
 *                                                                          *
 ****************************************************************************/


#define  SSI_UDR_LEN		1000
#define  SSI_PRFX_LEN		 200
#define  SSI_UDR_PIXELS		 800
#define  SSI_HDR_LEN		1800
#define  SSI_MAX_LINES		 800
#define  SSI_EDR_PIXELS		1600

/* SSI3 Housekeeping */

typedef struct
        {
         FIELD           exposure_no:5;  /* Exposure Number (bits 0-4)        */
         FIELD           cmnd_gain:2;    /* Commanded Gain State (bits 5-6)   */
         FIELD           flood:1;        /* Light Flood (bit 7)               */
        }
        word23_typ;

typedef struct
        {
         FIELD           cmnd_filter_pos:3;   /* Commanded Filter Pos. (bits 0-2)  */
         FIELD           cmnd_filter_step:1;  /* Commanded Filter Step. (bits 3)   */
         FIELD           cmnd_blemish:1;      /* Commanded Blemish Pro. (bits 4)   */
         FIELD           cmnd_expo_mode:1;    /* Commanded Exposure Mode (bit 5)   */
         FIELD           cmnd_expo_cycle:1;   /* Commanded Exposure Cycle (bits 6) */
         FIELD           skip:1;         /* Skip (bit 7)                      */
        }
        word24_typ;

typedef struct
        {
         FIELD           state_used:2;   /* State Used (bits 0-1)             */
         FIELD           comp_stat:2;    /* Compressor Status (bits 2-3)      */
         FIELD           long_expo:1;    /* Long Exposure Cycle (bit 4)       */
         FIELD           img_mode:3;     /* Imaging Mode (bits 5-7)           */
        }
        word25_typ;

typedef struct
        {
         FIELD           odd_parity:1;   /* Odd Parity (bits 0)               */
         FIELD           filter:3;       /* Actual Filter (bits 1-3)          */
         FIELD           blemish:1;      /* Blemish Protection Status (bits 4)*/
         FIELD           watchdog:1;     /* Watchdog Trip Flag (bits 5)       */
         FIELD           par_clock:1;    /* Parallel Clock State (bits 6)     */
         FIELD           mem_write:1;    /* Imaging Mode (bits 7)             */
        }
        word26_typ;


/* SSI 3 Housekeeping Packet */

typedef struct
        {
         char            ra[8];            /* (0-7)                       */
         char            dec[8];           /* (8-15)                       */
         char            twist[8];         /* (16-23)                       */
         char            clock_angle[8];   /* (24-31)                       */
         UBYTE           ccd_temp_fine;    /* (32)                         */
         UBYTE           ccd_temp_coarse;  /* (33)                         */
         UBYTE           pic_no;           /* (34)                        */
         word23_typ      word23;           /* (35)                        */
         word24_typ      word24;           /* (36)                        */
         word25_typ      word25;           /* (37)                        */
         word26_typ      word26;           /* (38)                        */
        }
        ssi3_hk_typ;

/* Compression Flags */

typedef struct
        {
         FIELD           barc_comp:1;    /* BARC Compression (1, bit 0)       */
         FIELD           barc_mode:1;    /* Barc Comp. Mode (1, bit 1)        */
         FIELD           expo:1;         /* Exposure Mode (1, bit 2)          */
         FIELD           flood:1;        /* Light Flood Status (1, bit 3)     */
         FIELD           blem:1;         /* Blemish Protection (1, bit 4)     */
         FIELD           clock:1;        /* Parallel Clock State (1, 5)       */
         FIELD           ict_comp:1;     /* ICT Comp. (1, bit 6)              */
         FIELD           huff_comp:1;    /* Huffman Comp. (1, bit 7)          */
         UBYTE           reserved1;      /* Reserved bits (2)                 */
        }
        flag_comp_typ;

/* Input Source Structure */

typedef struct
        {
         FIELD           SFDU:1; /* bit 0 */
         FIELD           WBDL:1; /* bit 1 */
         FIELD           SDR:1; /* bit 2 */
         FIELD           IDR:1; /* bit 3 */
         FIELD           EDR:1; /* bit 4 */
         FIELD           MIPS:1; /* bit 5 */
         FIELD           APB:1; /* bit 6 */
         FIELD           reserved:1;  /* bit 7 */
        }
        input_src;

/* Number of Truncation bits */

typedef struct
        {
         FIELD           block0:2;       /* (0, 0-1)                         */
         FIELD           block1:2;       /* (0, 2-3)                         */
         FIELD           block2:2;       /* (0, 4-5)                         */
         FIELD           block3:2;       /* (0, 6-7)                         */
         FIELD           block4:2;       /* (1, 0-1)                         */
         FIELD           block5:2;       /* (1, 2-3)                         */
         FIELD           block6:2;       /* (1, 4-5)                         */
         FIELD           block7:2;       /* (1, 6-7)                         */
         FIELD           block8:2;       /* (2, 0-1)                         */
         FIELD           block9:2;       /* (2, 2-3)                         */
         FIELD           block10:2;      /* (2, 4-5)                         */
         FIELD           block11:2;      /* (2, 6-7)                         */
         FIELD           block12:2;      /* (3, 0-1)                         */
         FIELD           filler1:6;      /* (3, 2-7)                         */
        }
        trun_bits_typ;


/* Line Construction */

typedef struct
        {
         FIELD           part_pkts:4;    /* No. tlm pkts for line (125,0-3)  */
         FIELD           full_pkts:4;    /* No. full pkts for line (125,4-7) */
        }
        line_con_typ;

/* MIPS Physical Recording Words */

typedef struct
        {
         BYTE            os[8];          /* Operating system version (0-7)   */
         BYTE            reserved1[26];  /* Used during Phase I              */
         BYTE            cpu[8];         /* CPU name                         */
         BYTE            date[11];       /* Generation date                  */
         BYTE            filler[6];      /* No Description                   */
        }
        mips_prd_typ;

/* UDR, REDR and EDR Image Line Record */

typedef struct
        {
         UBYTE           record_id;      /* Record Identifier (0)            */
         UBYTE           reserved1A;     /* Used during Phase I (1-3)        */
         UWORD           reserved1B;
         UWORD           log_seq;        /* Logical sequence (4-5)           */
         ert_typ         ert;            /* Earth Received Time (6-14)       */
         sclk_typ        sclk;           /* SCLK of 1st minor frame (15-21)  */
         mips_prd_typ    mips_prd;       /* Recording & valid device (22-80) */
         UWORD           record_format;  /* Record Format (81-82)            */
         UBYTE           input_type;     /* Input type (83)                  */
         input_src       source;         /* Input source (84)                */
         UBYTE           reserved3A;     /* Used during Phase I (85-102)     */
         UBYTE           reserved3B;  
         UBYTE           reserved3C[12];  
         UWORD           reserved3D;  
         UWORD           reserved3E;  
         trun_bits_typ   trun_bits;      /* Truncated bits/pixel (103-105)   */
         UWORD           trun_pix;       /* No. pixels truncated (107-108)   */
         UWORD           version;        /* Catalog ver. ident img (109-110) */
         UWORD           reserved4;      /* Used during Phase I (111-112)    */
         UBYTE           dsn_id;         /* Defined GLL-820-13/OPS-6-8 (113) */
         UWORD           line_no;        /* Image line number (114-115)      */
         UBYTE           reserved5;      /* Used during Phase I              */
         UWORD           ss1;            /* Starting sample (117-118)        */
         UWORD           es1;            /* Ending sample (119-120)          */
         UWORD           ss2;            /* Starting sample (121-122)        */
         UWORD           es2;            /* Ending sample (123-124)          */
         line_con_typ    line_con;       /* Line construction (125)          */
         UBYTE           apid;           /* Application pkt ID (126)         */
         UINT            pkt_seq;        /* Tlm pkt is 1st pixel (127-130)   */
         UWORD           pkt_pix_srt;    /* Starting samp of pkt (131-132)   */
         UWORD           strt_pix;       /* Truth table position (133-134)   */
         UWORD           stop_pix;       /* Truth table position (135-136)   */
         rct_typ         rct;            /* TIS Record Creation (137-145)    */
         BYTE            decomp_stat;    /* Decompression status/errors (146)*/
         char            comp_ratio[6];  /* Compression Ratio (147-152)      */
         BYTE            reserved6[47];  /* Reserved (153-199)               */
        }
        ssi_prefix_typ;

/* Telemetry Header */

typedef struct                                    /* Format Identification   */
        {
         UBYTE           record_id;      /* Record Identifier (0)             */
         UBYTE           reserved1;      /* Used during Phase I (1)           */
         char            project[10];    /* Project name = "GALILEO" (2-11)   */
         char            instrument[6];  /* Instrument name = "SSI" (12-17)   */
         UWORD           reserved2;      /* Used during Phase 1 (18-19)       */
         UWORD           log_seq;        /* Logical Sequence (20-21)          */
         ert_typ         first_ert;      /* ERT of first packet (22-30)       */
         ert_typ         last_ert;       /* ERT of last packet (31-39)        */
         sclk_typ        first_sclk_rec; /* SCLK of first record (40-46)      */
         sclk_typ        last_sclk_rec;  /* SCLK of last record (47-53)       */
         scet_typ        scet;           /* SCET middle open shutter (54-62)  */
         mips_prd_typ    mips_prd;       /* MIPS Phy. Record Data (63-121)    */
         UWORD           rec_format;     /* Record Format (122-123)           */
         UINT            reserved3A;     /* Used during Phase I (124-127)     */
         UBYTE           boom;           /* Boom obscuration flag (128)       */
         UWORD           miss_lines;     /* Lines with no pixels (129-130)    */
         UWORD           part_lines;     /* Lines with valid pixels (131-132) */
         UWORD           reserved4;      /* Used during Phase I (133-134)     */
         UWORD           seq_break;      /* Tot no. of missing pkts (135-136) */
         UWORD           reserved5[3];   /* Used during Phase I (137-142)     */
         UWORD           sfdus;          /* Total no. minor frames (143-144)  */
         char            pic_no[7];      /* Picture no. (145-151)             */
         UBYTE           reserved6[12];  /* Used during Phase I (152-163)     */
         flag_comp_typ   flags;          /* Compression flags (164-165)       */
         char            mean_dn[6];     /* Mean DN of all levels (166-171)   */
         char            trun_bits[6];   /* Mean no. of trun bits (172-177)   */
         char            trun_pixel[6];  /* Mean no. of trun pixels (178-183) */
         BYTE            mean_if[12];    /* Mean I/F Level (184-195)          */
         BYTE            entrop_avg[7];  /* Entropy level picture (196-202)   */
         BYTE            entropies[15][7]; /* Entropy level 15 lines (203-307)*/
         BYTE            pointing[3][8]; /* Scan platform coords (308-331)    */
         BYTE            scale_fact[2][8]; /* Radio. Calib. scale fact (332-347) */
         BYTE            slope_file[32]; /* Radiometric file for proc. (348-379) */     
         BYTE            offset_file[32]; /* Radiometric file for proc.(380-411) */     
         char            activity[20];   /* 20 ASCII Chars. (412-431)         */
         UBYTE           filler;         /* (432)                             */
         UBYTE           filter;         /* (433)                             */
         UBYTE           exposure;       /* Exposure number (434)             */
         UBYTE           image_mode;     /* Imaging_mode (435)                */
         UBYTE           gain;           /* Camera gain state (436)           */
         UINT            range;          /* Target's range sun (437-440)      */
         UBYTE           reserved7;      /* Used during Phase I (441)         */
         UWORD           version;        /* MIPS Cat. ver. no. (442-443)      */
         sclk_typ        start_sclk_img; /* SCLK start of image (444-450)     */
         sclk_typ        end_sclk_img;   /* SCLK end of image (451-457)       */
         ssi3_hk_typ     ssi3_pkt;       /* Data SSI3 tlm pkt (458-496)       */
         UBYTE           reserved8[279];     
         UINT            histogram[256]; /* 256 32-bit histogram (776-1799)   */
        }
        ssi_hdr_typ;


typedef	struct
	{
	ssi_prefix_typ			prefix;
	unsigned char			pixels[SSI_UDR_PIXELS];
	}
	ssi_line_typ;

typedef	struct
	{
	ssi_prefix_typ			prefix;
	short			        pixels[SSI_EDR_PIXELS];
	}
	ssi_edr_line_typ;

#endif     
