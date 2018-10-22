/************************************************************************/
/*									*/
/*			tzbits.c					*/
/*		C Implementation by: Steve Hwan				*/
/*									*/
/*		Creation Date: 16 October 1993				*/
/*		Last Modified: 22 October 1993				*/
/*									*/
/*	This is a driver program to test the routines zbits.c and	*/
/* zhbits.c.								*/
/* 	This program can be called without any parameters and it	*/
/* should look just like the output of the old bits program.  Option-	*/
/* ally, it can be called with the following parameters in the		*/
/* following order:							*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vicmain_c"
#include "ftnbridge.h"

#define SUCCESS		1
#define LAST_BIT_SHORT	(8* sizeof(short int) -1)
#define LAST_BIT_LONG	(8* sizeof(long int) -1)

void ztbits()
{
  FTN_NAME(tbits)();
}

/* tzbits_help just prints out the parameters to use the test program */
void tzbits_help(hbit)
int hbit;
{
  char mymessage[80];

  zvmessage("Optional parameters are:","");
  zvmessage("HWORD   half word input(hex)    default: 0x05AF","");
  zvmessage("FWORD   full word input(hex)    default: 0x0055AAFF","");
  zvmessage("LOBIT   low(starting) bit(dec)  default: 0","");
  sprintf(mymessage,"HIBIT  high(ending) bit(dec)   default: %d\n",hbit);
  zvmessage("Or type tzbits help to get this message\n","");
  zvmessage(mymessage,"");

  return;
}



main44()
{
  int i;			/* dummy index */
  short int half_in;		/* half word input */
  long  int word_in;		/* full word input */
  int lbit,hbit;		/* starting and ending of bits to extract */
  long int output_word;		/* return value */
  char card[132];		/* dummy string to pass to zvmessage */
  char temp[132];			/* temporary string to deal with zv routines */
  int count;			/* used in zvp */

  /*** GET PARAMETERS LOBIT,HIBIT ***/
  zvp("LOBIT", &lbit, &count);
  zvp("HIBIT", &hbit, &count);
  if (hbit==-1) hbit=LAST_BIT_LONG;

  /*** GET PARAMETER HWORD ***/
  zvp("HWORD", temp, &count);
  if (count<=0)
    half_in=0x05AF;
  else {
    if (!strcmp(temp,"help")) {
      tzbits_help(hbit);
      return;
    }
    count=sscanf(temp,"%hx",&half_in);
    if (count<=0)
      half_in=0x05AF;
  } /* else */

  /*** GET PARAMETER FWORD ***/
  zvp("FWORD", temp, &count);
  if (count<=0)
    word_in=0x0055AAFF;
  else {
    count=sscanf(temp,"%x",&word_in);
    if (count<=0)
      word_in=0x0055AAFF;
  }

  /* arbitrary part - lets user select which bits to look at in addition */
  /* to normal test. */
  zvmessage("Running user selected lbit and hbit.(Defaults to whole word)","");
  zbits(word_in, lbit,hbit, &output_word);
  sprintf(temp,"\t %x\n",output_word);
  zvmessage(temp,"");

  /* Test the half word routine, looking bit by bit */
  zvmessage("*****Half word tests*****","");
  card[0]=0;
  for(i=LAST_BIT_SHORT; i>=0; i--) {
    zhbits(half_in, i,i,&output_word);
    sprintf(temp,"%x",output_word);
    strcat(card,temp);
  }
  zvmessage(card,"");

  /* Test the full word routine, looking bit by bit */
  zvmessage("*****Full word tests*****","");
  card[0]=0;
  for(i=LAST_BIT_LONG; i>=0; i--) {
    zbits(word_in, i,i,&output_word);
    sprintf(temp,"%x",output_word);
    strcat(card,temp);
  }
  zvmessage(card,"");

  zvmessage("Calling FORTRAN version....","");
  ztbits();

}
