/******************************************************************************
 *  Include file ABLE86.H - Data structure returned from routine ABLE86.      *
 *  Initial release: Aug 14, 1996 GMY                                         *
 ******************************************************************************/

typedef struct
	{ int	lbl_typ;	/* Input: Size of structure	*/
				/*	in 32-bit elements	*/
				/* Ouput: Label type		*/
				/*	0 - Invalid		*/
				/*	1 - Ground		*/
				/*	2 - Flight		*/
	  int	frm_nmbr;	/* Frame number=100*RIM+MOD91	*/
	  float	exposure;	/* Exposure time (msec)		*/
	  int	fltr;		/* Filter position (0-7)	*/
	  int	frm_rate;	/* Frame scan-rate		*/
				/*	1 -  2 1/3		*/
				/*	2 -  8 2/3		*/
				/*	3 - 30 1/3		*/
				/*	4 - 60 2/3		*/
       				/*	5 - 15 1/6 (Phase II only)*/
	  int   mofibe;		/* Camera flags:		*/
				/*	Flood			*/
				/*	Inverted		*/
				/*	Blem-protect		*/
				/*	Extended expose		*/
	                        /*      M&O (PhaseII)           */
	  int   boom_f;		/* Boom flag:			*/
				/*	0 - Not obscured	*/
				/*	1 - Possibly obscured	*/
				/*	2 - Obscured		*/
	  int	gain;		/* Gain-state:			*/
				/*	1 - 400K		*/
				/*	2 - 100K		*/
				/*	3 -  40K		*/
				/*	4 -  10K		*/
	  int   minor_frm;	/* 66 2/3 msec & 8 1/3 msec CLK	*/
				/*      10 * MOD10 + MOD8	*/
	  int	scet_year;	/* SCET year-of-century		*/
	  int	scet_day;	/* SCET day-of-year		*/
	  int	scet_hour;	/* SCET hour-of-day  		*/
	  int	scet_minute;	/* SCET minute-of-hour		*/
	  int	scet_second;	/* SCET second-of-minute	*/
	  int	scet_millisec;	/* SCET millseond-of-second	*/
	  int	partition;	/* RIM reset counter		*/
	  char	trgt_nm[12];	/* Target name			*/
	  float iof;		/* Conversion factor: DN to reflectance	*/
	  float cnv;		/* Conversion factor: DN to radiance	*/
	  float	sol_range;	/* Sun to target body distance (km)	*/
	  char	dc_fn[20];	/* Dark Current file name		*/
	  char	rad_fn[20];	/* Radiometric SLOPE file name	*/
	  char	blem_fn[20];	/* Blemish file name			*/
	  char	so_fn[20];	/* Shutter-Offset file name		*/
	  char	edr_id[8];	/* EDR tape ID				*/
	  int	edr_nmbr;	/* EDR file number (0-999)		*/
	  int	ubwc;		/* Uneven-Bit-Weighting correction flag	*/
				/*	0 - Off				*/
				/*	1 - On				*/
	  char  pic_nmbr[8];	/* Picture nmbr (uses 7 bytes)		*/
          int   seqno;		/* Phase 2 sequence number		*/
          float entropy;	/* Image entropy			*/
	  char  dc_path[32];	/* Dark current disk and directory	*/
          char  cal_path[32];	/* Radiometric file disk and directory	*/
          char  blem_path[32];	/* Blemish file disk and directory	*/
          char  offset_path[32];/* Shutter-offset file disk and directory */
          int   rmode;		/* SSI readout mode (0=contiguous, 1=sample) */
	} able_86_typ;


