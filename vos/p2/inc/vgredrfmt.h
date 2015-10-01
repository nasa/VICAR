/*===========================================================================*
 | VGREDRFMT.H  -- Project Voyager EDR tape format			     |
 |									     |
 |	GCFDATA   defines the GCF data block (total length = 20 bytes)	     |
 |	VGRSUBCOM defines the Voyager ISS Subcom data (tot length = 40 bytes)|
 |	EDRHDR    defines the EDR header record (total length = 1280 bytes)  |
 |	EDRLIN    defines the EDR line record (total length = 1040 bytes)    |
 |									     |
 | History:								     |
 |  9-Feb-1989	39568	PXZ- Added new flag "compression" 		     |
 *===========================================================================*/

#include "gcfdata.h"			/* GCF block data definitions	     */
#include "vgrsubcom.h"			/* Voyager ISS Subcom definitions    */

union input_id {
	unsigned short source;
	struct {
		unsigned type	   :8;  /* 0=VGR1, 1=VGR2, 4=VGR1 SIM, ...   */
		unsigned unused    :1;  /* Unused			     */
		unsigned rt        :1;  /* 1=data from R/T (via DACS)        */
		unsigned sdr       :1;	/* 1=data from SDR tape		     */
		unsigned idr       :1;  /* 1=data from IDR		     */
		unsigned edr       :1;  /* 1=data from EDR		     */
		unsigned sfdu      :1;  /* 1=data from SFDU tape 	     */
		unsigned unused2   :1;  /* 				     */
		unsigned fill_data :1;	/* 1=fill data			     */
		} src;
	};

struct edrhdr
	{
	char recid;			/* Header record id = 0 	     */
	unsigned char fileno;		/* EDR tape file number = 0,1,2,...  */
	unsigned short phys_seq_no;	/* EDR tape physical sequence number */
	unsigned short log_seq_no;	/* EDR tape logical sequence number  */
	unsigned ert_day	:9;	/* ERT day of year, first valid line */
	unsigned ert_year	:7;	/* ERT year of century,first valid l */
	unsigned short ert_min;		/* ERT minute of day,first valid line*/
	unsigned short ert_msec;	/* ERT msec of minute,first valid lin*/
	unsigned lert_day	:9;	/* ERT day of year, last valid line  */
	unsigned lert_year	:7;	/* ERT year of century,last valid li */
	unsigned short lert_min;	/* ERT minute of day,last valid line */
	unsigned short lert_msec;	/* ERT msec of minute,last valid line*/
	unsigned short fds_mod16;	/* FDS mod16 count, first valid line */
	unsigned short fds_mod60;	/* FDS mod60 count, first valid line */
	unsigned short fds_line;	/* FDS line count, first valid line  */
	unsigned short lfds_mod16;	/* FDS mod16 count, last valid line  */
	unsigned short lfds_mod60;	/* FDS mod60 count, last valid line  */
	unsigned short lfds_line;	/* FDS line count, last valid line   */
	unsigned scet_day	:9;	/* SCET day of year		     */
	unsigned scet_year	:7;	/* SCET year of century		     */
	unsigned short scet_min;	/* SCET minute of day 		     */
	unsigned short scet_msec;	/* SCET msec of minute  	     */
	char system_version[6];		/* "VMSx.x" */
	char input_device[3];
	char input_fileid[6];
	char output_device[3];  	/* "MT3" */
	char output_volume[6];		/* "EDR002" */
	char cpu;			/* "1", "2", or "3" */
	char creation_day[3];		/* julian day of record creation     */
	char creation_year[2];		/* year of record creation 	     */
	short rver_number;              /* catalog raw version number        */
	struct gcfdata gcf[2];		/* GCF data, first & last valid line */
	unsigned irt_day	:9;	/* IRT day of year		     */
	unsigned irt_year	:7;	/* IRT year of century               */
	unsigned short irt_min;		/* IRT minute of day  		     */
	unsigned short irt_msec;	/* IRT msec of minute  		     */
	unsigned char  tlm_mode;	/* telemetry mode		     */
	unsigned char unused;

	struct {		/* Source Data Summary */

		unsigned short unused1;
		struct
			{
			unsigned sc_id		  :1;	/* 0=VGR-2, 1=VGR-1  */
			unsigned imcode		  :5;	/* Image format code */
			unsigned major_data_type  :2;	/* 2=Imaging Data    */
			unsigned unused		  :8;	/* Unused */
			} fid;

		unsigned short system_noise_temp_min;
		unsigned short system_noise_temp_max;
			 short symbol_snr_min;
			 short symbol_snr_max;
		unsigned short agc_min;
		unsigned short agc_max;
		unsigned short pn_errs;		/* # bit-errs in 32-bit PN code */
		unsigned short fds_count_errs;  /* # bit-errs in 24-bit FDSC */
		unsigned short sync_pars[3];
		unsigned short nlines;		/* Total # of valid lines    */
		unsigned short nfull;		/* number of full lines      */
		unsigned short npartial;	/* number of partial lines   */
		unsigned short nbadrec;		/* # of unreadable records   */
		unsigned short nlog_seq_breaks;	/* # of logical seq breaks   */
		unsigned short sort_par[4];	/* Note: [2]=station 2       */
		unsigned short nmf_from_idr;	/* # of minor frms from IDR  */
		unsigned short nmf_from_wbdl;	/* # of minor frms from DACS */
		unsigned short nmf_from_sdr;	/* # of minor frms from SDR  */
		unsigned short nmf_missing;	/* # of missing minor frames */
		unsigned short unused2;
		char picno[10];	        	/* Picture No,e.g."1234U-234"*/
		char target_body[10];		/* Target body, "GANYMEDE"   */
		union input_id input;		/* Input source & type       */
		} sds;

	struct vgrsubcom subcom;
	unsigned short iss_engineering[5];
	unsigned int	im2wp_flag : 1;		/* if set, data from PWS part>*/
						/*>of the carzy IM2W format   */
	unsigned int	compression: 1;		/* 0=OFF, 1=ON		      */
	unsigned int	unused4    : 6;
	unsigned char	unused5[5];
	int hist[256];				/* raw image histogram */

	struct {	/* record trailer */
		unsigned short unused4;
		unsigned short fds_count[3];	/* copy of words 9-11 */
		} trailer;
	};



struct edrlin
	{
	int not_written_to_tape;	/* Padding to make record 1024 bytes */
	char recid;			/* Header record id = 1 */
	unsigned char fileno;		/* EDR tape file number = 0,1,2,...  */
	unsigned short phys_seq_no;	/* EDR tape physical sequence number */
	unsigned short log_seq_no;	/* EDR tape logical sequence number  */
	unsigned ert_day	:9;	/* ERT day of year (1-366)	     */
	unsigned ert_year	:7;	/* ERT year of century (0-99)	     */
	unsigned short ert_min;		/* ERT minute of day		     */
	unsigned short ert_msec;	/* ERT msec of minute		     */
	unsigned short unused1[3];
	unsigned short fds_mod16;	/* FDS mod16 count,		     */
	unsigned short fds_mod60;	/* FDS mod60 count, 		     */
	unsigned short fds_line;	/* FDS line count,		     */
	unsigned short unused2[6];	/* = 0				     */
	char system_version[6];		/* "VMSx.x" */
	char input_device[3];
	char input_fileid[6];
	char output_device[3];  	/* "MT3" */
	char output_volume[6];		/* "EDR002" */
	char cpu;			/* "1", "2", or "3" */
	char creation_day[3];		/* julian day of record creation     */
	char creation_year[2];		/* year of record creation 	     */
	short source;			/* for merged files, source of line, */
					/*    1=primary, 2=secondary	     */
	struct gcfdata gcf;		/* GCF data block		     */
	unsigned short unused3[14];

	struct {		/* Source Data Summary */
		unsigned short line_no;

		struct
			{
			unsigned sc_id		  :1;	/* 0=VGR-2, 1=VGR-1  */
			unsigned imcode		  :5;	/* Image format code */
			unsigned major_data_type  :2;	/* 2=Imaging Data    */
			unsigned unused		  :8;	/* Unused */
			} fid;

		unsigned short system_noise_temp;
		unsigned short unused4;
		unsigned short symbol_snr;
		unsigned short unused5;
		unsigned short agc;
		unsigned short unused6;
		unsigned short pn_errs;
		unsigned short fds_count_errs;
		unsigned short unused7;
		unsigned short allowed_pn_errs;
		unsigned short allowed_fdsc_errs;
		unsigned short nmf_total;	/* total # of minor frames   */
		unsigned short nmf_full;	/* # of full minor frames    */
		unsigned short nmf_partial;	/* # of partial minor frames */
		unsigned short nmf_missing;	/* # of missing minor frames */
		unsigned short sync_status[10];
		unsigned short bits_retained[10];
		union input_id input;		/* input source & type       */
  		}sds;	

	unsigned short subcom[10];	/*from each minor frame for this line*/
	unsigned short unused8[2];
	unsigned short first_pixel;
	unsigned short last_pixel;
	unsigned char pixel_value[800];

	struct {		/* line trailer */
		unsigned short reserved[7];		/*for IPL use*/
		unsigned short fds_count[3];	/* copy of words 9-11 */
		} trailer;
  	};
