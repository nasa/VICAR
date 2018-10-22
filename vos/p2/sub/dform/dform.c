/*

       dform

       the purpose of dform is to manipulate data into a
       byte form suitable for display 


       calling sequence:
       call dform ( in_buf, out_buf, ns, hisbuf, isw, itype, cmode, ib1, ib2, 
                    r1, r2, coll )

       where    in_buf    	input array

		out_buf 	output byte array

                ns     		no. of elements in array in_buf

                hist_buf 	histogram buffer

                isw    		process switch
                             0 = convert data and acquire histogram
                             1 = initialize for later calls

                itype  		data type	(in C language)
                             0 = byte		(0 = signed char)
                             1 = integer * 2	(1 = short int)
                             2 = integer * 4	(2 = int)
                             3 = real * 4	(3 = float)

                cmode  		conversion mode
                             0 = no compression                      
                             1 = bit range selected
                             2 = scaled

                ib1    		low order bit selected
                ib2    		high order bit selected

                r1     		low scale value
                r2     		high scale value  

		coll   		histogram collection flag
		      	     0 = no histogram required
			     1 = histogram required

---------------------------------------------------------------------
 
       Revision history . . .

       bam/ram/hbd/jhr 2/86    fixed.....?????

       bam   9/86    
       	Replaced missing card from byte histogram collect loop
        that screwed up the histograms for byte data.....

       jfm   June 22, 1993
	Made portable by Justin McNeill. Additional
	argument added for output array. COLL argument
	is no longer optional. Test programs expanded
	and corrected. Bit extraction for INTEGER*4 
	data corrected.

	jfm   Dec. 13, 1993	
 	Corrected byte-to-byte transfer and simplified
	handling of data conversion (removed unnecesary	
	allocation of holding arrays). Expanded test
	script. (FR 76815) (JFM059)

*/

#define	NO_COMPRESSION	0
#define BIT_EXTRACTION  1
#define SCALING		2

#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zdform(in_buf, out_buf, ns, hist_buf, isw, itype, cmode,
	ib1, ib2, r1, r2, coll )
void 	*in_buf;
char 	*out_buf;
int 	ns, hist_buf[], isw, itype, cmode, ib1, ib2, coll;
float	r1, r2;
{
/* 
	Save information from initialization.
*/
static int	data_type, conversion_mode, 
		low_order_bit, high_order_bit;
static float	low_scale_value, high_scale_value, scalar;

int 		x;			/* Loop control variables	   */
float 		value_difference;	/* Scalar calculation variable	   */

char		csamp;
int 		ihb1,ihb2,number_bits;
int 		intval,intsamp;
short int 	isamp,itemp;
float 		rtemp;

char		*byte_buf;
int		*int_buf;
short int	*short_buf;
float		*float_buf;

/* 
	Check ISW for appropriate route
	
	This is the initialization mode. Save all the pertinent
	information for later use.
*/
if ( isw == 1 )
	{
	for ( x=0;x<256;x++ )
		hist_buf[x] = 0;

	data_type 	= itype;
	conversion_mode = cmode;	
	low_order_bit	= ib1;
	high_order_bit	= ib2;

	if ( conversion_mode == SCALING )	/* Compute data scalar 	*/	
		{
		low_scale_value		= r1;
		high_scale_value 	= r2;
		value_difference	= high_scale_value - low_scale_value;
		if ( value_difference == 0.0 ) 
			value_difference = 255.0;
		scalar 			= 255.0 / value_difference;
		}
	}
else
	/*	

	Switch on data type of input.

	*/
	switch ( data_type ) 	{

	case 0:	/* BYTE to BYTE data transfer */

		byte_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= *(byte_buf+x);
				intval 		= *(byte_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
			    {
			    csamp = *(byte_buf+x);
			    out_buf[x] = (csamp>>ihb1)&~(~0<<number_bits);
			    if ( coll == 1 )
			      hist_buf[(int) out_buf[x]] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(byte_buf+x) < low_scale_value )
					*(byte_buf+x) = (char)low_scale_value;
				if ( *(byte_buf+x) > high_scale_value )
					*(byte_buf+x) = (char)high_scale_value;
				itemp = ((float)(*(byte_buf+x))-low_scale_value)
						* scalar;
				out_buf[x] = (char)itemp;

				if ( coll == 1 )
					hist_buf[itemp] += 1;
				}
			break;	}

		break;

	case 1: /* HALFWORD array conversion */

		short_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(short_buf+x);
				intval 		= (char)*(short_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
			    {
			    isamp = *(short_buf+x);
			    itemp = (isamp>>ihb1)&~(~0<<number_bits);
			    out_buf[x] = (char)itemp;
			    if ( coll == 1 )
				hist_buf[itemp] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(short_buf+x) < low_scale_value )
					*(short_buf+x) = (short)low_scale_value;
				if ( *(short_buf+x) > high_scale_value )
					*(short_buf+x) = (short)high_scale_value;
				itemp = ((float)*(short_buf+x)-low_scale_value)
						* scalar;
				out_buf[x] = (char)itemp;
				if ( coll == 1 )
					hist_buf[itemp] += 1;
				}
			break;	}

		break;

	case 2: /* INT array conversion  */

		int_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(int_buf+x);
				intval 		= (char)*(int_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			ihb1 = low_order_bit;
			ihb2 = high_order_bit;
			number_bits = ihb2 - ihb1 + 1;
			for ( x=0;x<ns;x++ )
		    	    {
			    intsamp = *(int_buf+x);
			    intval = (intsamp>>ihb1)&~(~0<<number_bits);
			    out_buf[x] =  (char)intval;
			    if ( coll == 1 )
				hist_buf[intval] += 1;
			    }
			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(int_buf+x) < low_scale_value )
					*(int_buf+x) = low_scale_value;
				if ( *(int_buf+x) > high_scale_value )
					*(int_buf+x) = high_scale_value;
				intval = (*(int_buf+x)-low_scale_value)*scalar;
				out_buf[x] = (char)intval;
				if ( coll == 1 )
					hist_buf[intval] += 1;
				}
			break;	}

		break;

	case 3: /* FLOAT array conversion */

		float_buf = in_buf;

		switch( conversion_mode ) 	{
		
		case NO_COMPRESSION:

			for ( x=0;x<ns;x++ )
				{
			  	out_buf[x] 	= (char)*(float_buf+x);
				intval 		= (char)*(float_buf+x);
				if ( intval<0 )
					intval += 256;
				if ( coll == 1 ) /* Acquire histogram	*/
					hist_buf[intval] += 1;
				}
			break;

		case BIT_EXTRACTION:

			break;

		case SCALING:

			for ( x=0;x<ns;x++ )
				{
				if ( *(float_buf+x) < low_scale_value )
					*(float_buf+x) = low_scale_value;
				if ( *(float_buf+x) > high_scale_value )
					*(float_buf+x) = high_scale_value;
				rtemp = (*(float_buf+x)-low_scale_value)*scalar;
				out_buf[x] = (char)rtemp;
				intval = (int)rtemp;
				if ( coll == 1 )
					hist_buf[intval] += 1;
				}
			break;	}

		break;
	}
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(dform, DFORM) (in_buf, out_buf, ns, hist_buf, isw, itype, cmode,
	ib1, ib2, r1, r2, coll )
void 	*in_buf;
char 	*out_buf;
int 	*ns, *hist_buf, *isw, *itype, *cmode, *ib1, *ib2, *coll;
float	*r1, *r2;
{
   zdform( in_buf, out_buf, *ns, hist_buf, *isw, *itype, *cmode,
	*ib1, *ib2, *r1, *r2, *coll );
}

