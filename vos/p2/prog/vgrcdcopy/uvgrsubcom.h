/*===========================================================================*
 |  VGRSUBCOM.H  -- Voyager ISS Subcom Data Structure definition	     |
 | 									     |
 |  Reference: 618-236, Rev A, Section 3				     |
 *===========================================================================*/

struct par_ind {
	unsigned spare		  :4;	/* Spare bits			     */
	unsigned na_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned na_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned na_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned na_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned na_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned na_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	unsigned wa_cycle_ind	  :1;	/* 0=actual/pseudo prepare, 1=read   */
	unsigned wa_beam_ind      :1;	/* 0=beam on, 1=beam off	     */
	unsigned wa_shutter_reset :1;	/* 0=reset pulse sent, 1=not sent    */
	unsigned wa_shutter_open  :1;	/* 0=open pulse sent, 1=not sent     */
	unsigned wa_shutter_close :1;	/* 0=close pulse sent, 1=not sent    */
	unsigned wa_light_flood   :1;	/* 0=flood pulse sent, 1=not sent    */
	};

union parword_ind {
	unsigned short word;
	struct par_ind bits;
       };

union parword20 {
	unsigned short word;
	struct {	
	  unsigned unused	   :5; 
	  unsigned wa_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned na_lsb_trunc    :1;  /* 0=truncated, 1=no truncation      */
	  unsigned memory_readout  :1;	/* 1=secondry memory readout,0=normal*/
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  } bits;
	struct {	
	  unsigned pixel_avg_ind   :3;  /* 0=.LT.min_count, 7=.GE.min_count  */
	  unsigned pixel_average   :5;  /* average of pixels above thresh    */
	  unsigned g1_voltage	   :3;	/* G1 Voltage (0-7)		     */
	  unsigned gain_state	   :1;	/* 0=low gain, 1=high gain	     */
	  unsigned fds_code	   :4;	/* FDS destination code: 5=WA, 6=NA  */
	  } bits2;
	};

struct vgrsubcom {
	struct  {
	unsigned camera_id	:1 ;	/* Camera ID: 0=WA, 1=NA 	     */
	unsigned shuttered_pic  :15;	/* 0=unshuttered, all ones=shuttered */
	}  subword1;

	struct  {			/* Subcom word #2 */
	unsigned line_number    :10;	/* Image line number		     */
	unsigned segment_number :6 ;	/* Line segment number=0,1...,sr-1   */
	}  subword2;

	struct  {			/* Subcom word #3 */
	unsigned spare		:4 ;	/* Spare bits = 0		     */
	unsigned exposure_table :1 ;	/* exposure table, 0:old, 1:new	     */
	unsigned na_elect_cal	:1 ;	/* NA electronics cal status: 1=on   */
	unsigned wa_elect_cal	:1 ;	/* WA electronics cal status: 1=on   */
	unsigned exposure	:5 ;	/* Exposure time code 		     */
	unsigned filter		:3 ;	/* Filter position 		     */
	unsigned odd_parity_bit :1 ;	/* Filter odd parity bit	     */
	}  subword3;

	unsigned short picture_count;	/* Subcom word #4: Shuttered pic cnt */

	struct {			/* Subcom word #5: Par word A */
	  unsigned mode    	  :3;	/* Camera mode			     */
	  unsigned use	          :5;	/* Use count			     */
	  unsigned next_mode	  :3;	/* Next camera mode		     */
	  unsigned next_use       :5;  	/* Next use count		     */
	  } parword_a;

	unsigned short parword_a_ind;	/* Subcom word #6: Par word A ind    */
	unsigned short parword_a_ptr;	/* Subcom word #7: Par word A pointer*/

	struct {			/* Subcom word #8: Par word B        */
	  unsigned na_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  unsigned na_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned na_filt         :3;	/* Filter position or step count     */
	  unsigned use         	   :7;	/* Use count			     */
	  } parword_b;

	unsigned short parword_b_ptr;	/* Subcom word #9: Par word B pointer*/

	struct {			/* Subcom word #10: Par word C       */
	  unsigned wa_exposure	   :5;	/* 0=Auto Exposure, >0 Exposure #    */
	  unsigned wa_filt_step_mode :1;/* 0=Position mode, 1=Step mode      */
	  unsigned wa_filt         :3;	/* Filter position or step count     */
	  unsigned use    	   :7;	/* Use count			     */
	  } parword_c;

	unsigned short parword_c_ptr;	/* Subcom word #11: Par word C pointe*/

	struct {			/* Subcom word #12: Par word D	     */
	  unsigned na_optics_cal   :5;	/* =0 off, >0 Exposurce #	     */
	  unsigned na_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned wa_shutter_select :2;/* 0=normal, 1=start long exp,       */
					/* 2=end long exp, 3=open	     */
	  unsigned use    	   :7;	/* Use count			     */
	  } parword_d;

	unsigned short parword_d_ind;	/* Subcom word #13: Par word D ind   */
	unsigned short parword_d_ptr;	/* Subcom word #14: Par word D pointe*/

					/* Subcom words #15-19 */
	unsigned char na_sample1;	/* First NA sample from previous frm */
	unsigned char wa_sample1;	/* First WA sample from previous frm */
	unsigned char na_sample2;	/* Secnd NA sample from previous frm */
	unsigned char wa_sample2;	/* Secnd WA sample from previous frm */
	unsigned char na_sample3;	/* Third NA sample from previous frm */
	unsigned char wa_sample3;	/* Third WA sample from previous frm */
	unsigned char na_sample4;	/* Forth NA sample from previous frm */
	unsigned char wa_sample4;	/* Forth WA sample from previous frm */
	unsigned char na_sample5;	/* Fifth NA sample from previous frm */
	unsigned char wa_sample5;	/* Fifth WA sample from previous frm */

	union parword20 word20;		/* Subcom word #20 */
	};
