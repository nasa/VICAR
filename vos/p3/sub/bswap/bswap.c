static char sccsBswap[] = "@(#)  bswap.c 2.2 9/9/88 ";
/* bswap(in,n)    swap byte pairs

     in = buffer holding the byte pairs
     n  = number of byte pairs to swap

  ORIGINAL CODE: 

  .TITLE	BSWAP
  .PSECT	BSWAP
  .ENTRY	BSWAP,^M<R2,R3>
  ;	IN	R2
  ;	N	R3
	MOVL     4(AP),R2
        MOVL    @8(AP),R3
        MOVL	#2,R1
  ;
  LOOP:	MOVB	1(R2),R0
	MOVB	(R2),1(R2)
	MOVB	R0,(R2)
	ADDL2   R1,R2
	SOBGTR	R3,LOOP
	RET
  .END

  NEW CODE: 
*/

bswap_(in,n)
     char in[];
     int *n;
{
  bswap(in,*n);
}

bswap(in,n)
     char in[];
     int  n;
{
  int register i, indx;
  char register temp;

  for (i=0;i<n;indx=i*2,i++) {
    temp = in[indx];
    in[indx] = in[indx+1];
    in[indx+1] = temp;
  }
}
    
