#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzccase)()

{
 static char c[]= "a#*defgHIJKL";

	zvmessage("Original string:", "");
	zvmessage(c, "");
	zccase(c,0,-1);
	zvmessage("After reversing case:", "");
	zvmessage(c, "");
	zccase(c,1,12);
	zvmessage("In upper case:", "");
	zvmessage(c, "");
	zccase(c,-1,-1);
	zvmessage("In lower case:", "");
	zvmessage(c, "");
	zuprcase(c);
	zvmessage("In upper case again:", "");
	zvmessage(c, "");

}
