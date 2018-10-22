/* This is completely broken... don't use!!  Use the RTL zvtrans */
/* facilities instead.   rgd 3/2010 */
long intibm (in)
char in[4];
{
  unsigned char temp;

  temp = in[0];
  in[0] = in[3];
  in[3] = temp;
  temp = in[1];
  in[1] = in[2];
  in[2] = temp;

  return(*((long *)in));
}
  

long intibm_ (in)
char in[4];
{
  return(intibm(in));
}

