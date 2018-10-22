#ifndef CARTOTAEUTILS_H
#define CARTOTAEUTILS_H

/*======================================================

mq_out_int

output integer value to parameter block

arguments :

      1. pname: input, char *pname;
         Parameter name.

      2. val: output, int val;
         Value output to parameter block.

mq_out_int returns integer variables to the parameter block.
*/

void mq_out_int (char *pname, int val);

/*======================================================

mq_out_real

output real value to parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, double val;
	 Value output to parameter block.

mq_out_real returns integer variables to the parameter block.  The name
and value are output to the print log.
*/

void mq_out_real (char *pname, double val);

/*======================================================

mq_out_string

returns character string to the parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, char *val;
	 String returned to parameter block.

      3. maxlen: input, int maxlen;
	 max length to put to output parameter block

mq_out_string returns character strings to the
parameter block.
*/

void mq_out_string (char *pname, char *val, int maxlen);

#endif
