/*===========================================================================*
 | UGCFDATA.H  -- Ground Control Facility Data Block Definition		     |
 |                For UN*X machines.                                         |
 |                ---> i.e. bit-fields are in reversed order                 |
 |									     |
 |	Total length = <machine dependent due to packing...20 bytes on VAX   |
 |									     |
 |	Reference: E. Kelly,"DSN MKIV Changes Affecting the VGR PRJ",	     |
 |		   IOM Voyager-GDSE-84-029, 19 July 1984.		     |
 *===========================================================================*/

union gdd_udt  {
  short word;
  struct  {
    unsigned gdd : 3;    /* DSN gross data description */
    unsigned udt : 6;    /* DSN user dependent type */
    unsigned ddt : 7;    /* DSN data dependent type */
  } bits;
};

union dsn_status  {
  short word;
  struct  {
    unsigned cu_data      : 2;    /* DSN coded/uncoded data */
    unsigned lock_status  : 6;    /* DSN lock status */
    unsigned config       : 8;    /* DSN configuration */
  } bits;
};

struct gcfdata  {
  unsigned short sync_code_msb;		/* 16 msb of frame-sync code	     */
  unsigned char source_station;		/* DSN source station		     */
  unsigned char sync_code_lsb;		/*  8 lsb of frame_sync code	     */
  unsigned char block_format;		/* Format code for this GCF block    */
  unsigned char destination_code; 	/* DSN destination codes 	     */
  union gdd_udt gddudt;			/* DSN GDD, UDT, DDT data            */
  struct  {
    unsigned ddt		:1;
    unsigned sc_number	:7;		/* 31=VGR1,32=VGR2		*/
					/* 41=VGR1 SIM,42=VGR2 SIM 	*/
    unsigned time_msb		:8;
  }  s1;
  unsigned short time_lsb;
  struct  {
    unsigned dsn_mark_no	:2;	/* 0=Mark III, 1=Mark IV             */
    unsigned day_of_year	:10;
    unsigned block		:4;
  }  s2;
  unsigned char msec_clock;
  unsigned char serial_number;
  union dsn_status dsn;		/* DSN configuration and lock status */
  unsigned short esc;
};
