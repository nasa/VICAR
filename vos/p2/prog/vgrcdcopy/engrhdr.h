/*  I kept the order of all the packed bit-fields so that VAX formats   */
/*  are retained.  I had to reverse these fields for the UNIX platforms	*/
/*                                                                      */
/*  --> This is the VAX VERSION OF THE Eng. Header Data Structure <--   */
/*  Myche McAuley  6/95							*/


#include "gcfdata.h"			/* GCF block data definitions	     */
#include "vgrsubcom.h"			/* Voyager ISS Subcom definitions    */

union input_id {
  unsigned short source;
    struct {
      unsigned type      :8;	/* 0=VGR1, 1=VGR2, 4=VGR1 SIM, ...   */
      unsigned unused    :1;	/* Unused			     */
      unsigned rt        :1;	/* 1=data from R/T (via DACS)        */
      unsigned sdr       :1;	/* 1=data from SDR tape		     */
      unsigned idr       :1;	/* 1=data from IDR		     */
      unsigned edr       :1;	/* 1=data from EDR		     */
      unsigned sfdu      :1;	/* 1=data from SFDU tape 	     */
      unsigned unused2   :1;	/* 				     */
      unsigned fill_data :1;	/* 1=fill data			     */
    } src;
};

union eventbuf  {
  unsigned short word;
  struct  {
    unsigned day        :9;
    unsigned year       :7;
  }  day_year;
};

struct edrhdr
  {
  char recid;			/* Header record id = 0 	     */
  unsigned char fileno;		/* EDR tape file number = 0,1,2,...  */
  unsigned short phys_seq_no;	/* EDR tape physical sequence number */
  unsigned short log_seq_no;	/* EDR tape logical sequence number  */
  union eventbuf ert;		/* ERT day & year, first valid line  */
  unsigned short ert_min;	/* ERT minute of day,first valid line*/
  unsigned short ert_msec;	/* ERT msec of minute,first valid lin*/
  union eventbuf lert;		/* ERT day & year, last valid line   */
  unsigned short lert_min;	/* ERT minute of day,last valid line */
  unsigned short lert_msec;	/* ERT msec of minute,last valid line*/
  unsigned short fds_mod16;	/* FDS mod16 count, first valid line */
  unsigned short fds_mod60;	/* FDS mod60 count, first valid line */
  unsigned short fds_line;	/* FDS line count, first valid line  */
  unsigned short lfds_mod16;	/* FDS mod16 count, last valid line  */
  unsigned short lfds_mod60;	/* FDS mod60 count, last valid line  */
  unsigned short lfds_line;	/* FDS line count, last valid line   */
  union eventbuf scet;		/* SCET day & year		     */
  unsigned short scet_min;	/* SCET minute of day 		     */
  unsigned short scet_msec;	/* SCET msec of minute  	     */
  char system_version[6];	/* "VMSx.x" */
  char input_device[3];
  char input_fileid[6];
  char output_device[3];  	/* "MT3" */
  char output_volume[6];	/* "EDR002" */
  char cpu;			/* "1", "2", or "3" */
  char creation_day[3];		/* julian day of record creation     */
  char creation_year[2];	/* year of record creation 	     */
  short rver_number;            /* catalog raw version number        */
  struct gcfdata gcf[2];	/* GCF data, first & last valid line */
  union eventbuf irt;		/* IRT day & year                    */
  unsigned short irt_min;	/* IRT minute of day  		     */
  unsigned short irt_msec;	/* IRT msec of minute  		     */
  unsigned char  tlm_mode;	/* telemetry mode		     */
  unsigned char unused;

  struct {		/* Source Data Summary */

    unsigned short unused1;
    struct
    {
      unsigned sc_id		  :1;	/* 0=VGR-2, 1=VGR-1  */
      unsigned imcode		  :5;	/* Image format code */
      unsigned major_data_type    :2;	/* 2=Imaging Data    */
      unsigned unused		  :8;	/* Unused */
    } fid;

    unsigned short system_noise_temp_min;
    unsigned short system_noise_temp_max;
             short symbol_snr_min;
             short symbol_snr_max;
    unsigned short agc_min;
    unsigned short agc_max;
    unsigned short pn_errs;		/* # bit-errs in 32-bit PN code */
    unsigned short fds_count_errs;	/* # bit-errs in 24-bit FDSC */
    unsigned short sync_pars[3];
    unsigned short nlines;		/* Total # of valid lines    */
    unsigned short nfull;		/* number of full lines      */
    unsigned short npartial;		/* number of partial lines   */
    unsigned short nbadrec;		/* # of unreadable records   */
    unsigned short nlog_seq_breaks;	/* # of logical seq breaks   */
    unsigned short sort_par[4];		/* Note: [2]=station 2       */
    unsigned short nmf_from_idr;	/* # of minor frms from IDR  */
    unsigned short nmf_from_wbdl;	/* # of minor frms from DACS */
    unsigned short nmf_from_sdr;	/* # of minor frms from SDR  */
    unsigned short nmf_missing;		/* # of missing minor frames */
    unsigned short unused2;
    char picno[10];	        	/* Picture No,e.g."1234U-234"*/
    char target_body[10];		/* Target body, "GANYMEDE"   */
    union input_id input;		/* Input source & type       */
  } sds;

    struct vgrsubcom subcom;
  unsigned short iss_engineering[5];
  struct  {
  unsigned int	im2wp_flag : 1;		/* if set, data from PWS part>*/
					/*>of the crazy IM2W format   */
  unsigned int	compression: 1;		/* 0=OFF, 1=ON		      */
  unsigned int	unused4    : 6;
  }  nept_byte;
  unsigned char	unused5[5];

};
