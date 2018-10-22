#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"
#include "vgi_keys.h"

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

main44()
{
   int  inpchar, done = FALSE;

   zvmessage("\nTesting GETINPUT C interface.\n", "");
   zvmessage("Testing \"wait mode\" input handling.  Please type a random assortment of", "");
   zvmessage("characters; then type \"q\" to quit and go on to the next test.\n", "");

   while (!done) {
      inpchar = zwaitforinput(TRUE);
      zprintchar(inpchar);
      if (inpchar == (int) 'q' || inpchar == (int) 'Q')
         done = TRUE;
   }
   inpchar = zwaitforinput(FALSE);

   zvmessage("\nTesting \"polling mode\" input handling.  Please type a random assortment of", "");
   zvmessage("characters; then type \"q\" to quit and go on to the next test.\n", "");

   done = FALSE;
   while (!done) {
      inpchar = zpollinput(TRUE);
      if (inpchar != (int) '\0')
         zprintchar(inpchar);
      if (inpchar == 'q' || inpchar == 'Q')
         done = TRUE;
   }
   inpchar = zpollinput(FALSE);
   zvmessage("\nDone testing GETINPUT C interface.\n", "");

   FTN_NAME(tgetinputf)();
   exit(0);
}


FTN_NAME(printchar)(inpchar)
int *inpchar;
{
   return (zprintchar(*inpchar));
}

zprintchar(inpchar)
int inpchar;
{
   char msg[81];

   switch (inpchar) {
      case VGI_INP_ERROR:
         zvmessage("An error occurred--unable to read keyboard input.", "");
         break;
      case VGI_CTRL_A:
         zvmessage("You typed <CTRL>-A", "");
         break;
      case VGI_CTRL_B:
         zvmessage("You typed <CTRL>-B", "");
         break;
      case VGI_CTRL_C:
         zvmessage("You typed <CTRL>-C", "");
         break;
      case VGI_CTRL_D:
         zvmessage("You typed <CTRL>-D", "");
         break;
      case VGI_CTRL_E:
         zvmessage("You typed <CTRL>-E", "");
         break;
      case VGI_CTRL_F:
         zvmessage("You typed <CTRL>-F", "");
         break;
      case VGI_CTRL_G:
         zvmessage("You typed <CTRL>-G", "");
         break;
      case VGI_CTRL_H:
         zvmessage("You typed <CTRL>-H or <BackSpace>", "");
         break;
      case VGI_CTRL_I:
         zvmessage("You typed <CTRL>-I or <TAB>", "");
         break;
      case VGI_CTRL_J:
         zvmessage("You typed <Return> or <LF>", "");
         break;
      case VGI_CTRL_K:
         zvmessage("You typed <CTRL>-K", "");
         break;
      case VGI_CTRL_L:
         zvmessage("You typed <CTRL>-L or <FormFeed>", "");
         break;
      case VGI_CTRL_M:
         zvmessage("You typed <Return> or <LF>", "");
         break;
      case VGI_CTRL_N:
         zvmessage("You typed <CTRL>-N", "");
         break;
      case VGI_CTRL_O:
         zvmessage("You typed <CTRL>-O", "");
         break;
      case VGI_CTRL_P:
         zvmessage("You typed <CTRL>-P", "");
         break;
      case VGI_CTRL_Q:
         zvmessage("You typed <CTRL>-Q", "");
         break;
      case VGI_CTRL_R:
         zvmessage("You typed <CTRL>-R", "");
         break;
      case VGI_CTRL_S:
         zvmessage("You typed <CTRL>-S", "");
         break;
      case VGI_CTRL_T:
         zvmessage("You typed <CTRL>-T", "");
         break;
      case VGI_CTRL_U:
         zvmessage("You typed <CTRL>-U", "");
         break;
      case VGI_CTRL_V:
         zvmessage("You typed <CTRL>-V", "");
         break;
      case VGI_CTRL_W:
         zvmessage("You typed <CTRL>-W", "");
         break;
      case VGI_CTRL_X:
         zvmessage("You typed <CTRL>-X", "");
         break;
      case VGI_CTRL_Y:
         zvmessage("You typed <CTRL>-Y", "");
         break;
      case VGI_CTRL_Z:
         zvmessage("You typed <CTRL>-Z", "");
         break;
      case VGI_DEL:
         zvmessage("You typed <DEL> or <BackSpace>", "");
         break;
      case VGI_ESC:
         zvmessage("You typed <ESC>", "");
         break;
      case VGI_PF1:
         zvmessage("You typed <PF1>", "");
         break;
      case VGI_PF2:
         zvmessage("You typed <PF2>", "");
         break;
      case VGI_PF3:
         zvmessage("You typed <PF3>", "");
         break;
      case VGI_PF4:
         zvmessage("You typed <PF4>", "");
         break;
      case VGI_F1:
         zvmessage("You typed <F1>", "");
         break;
      case VGI_F2:
         zvmessage("You typed <F2>", "");
         break;
      case VGI_F3:
         zvmessage("You typed <F3>", "");
         break;
      case VGI_F4:
         zvmessage("You typed <F4>", "");
         break;
      case VGI_F5:
         zvmessage("You typed <F5>", "");
         break;
      case VGI_F6:
         zvmessage("You typed <F6>", "");
         break;
      case VGI_F7:
         zvmessage("You typed <F7>", "");
         break;
      case VGI_F8:
         zvmessage("You typed <F8>", "");
         break;
      case VGI_F9:
         zvmessage("You typed <F9>", "");
         break;
      case VGI_F10:
         zvmessage("You typed <F10>", "");
         break;
      case VGI_F11:
         zvmessage("You typed <F11>", "");
         break;
      case VGI_F12:
         zvmessage("You typed <F12>", "");
         break;
      case VGI_F13:
         zvmessage("You typed <F13>", "");
         break;
      case VGI_F14:
         zvmessage("You typed <F14>", "");
         break;
      case VGI_F15:
         zvmessage("You typed <F15>", "");
         break;
      case VGI_F16:
         zvmessage("You typed <F16>", "");
         break;
      case VGI_F17:
         zvmessage("You typed <F17>", "");
         break;
      case VGI_F18:
         zvmessage("You typed <F18>", "");
         break;
      case VGI_F19:
         zvmessage("You typed <F19>", "");
         break;
      case VGI_F20:
         zvmessage("You typed <F20>", "");
         break;
      case VGI_UP:
         zvmessage("You typed <Up Arrow>", "");
         break;
      case VGI_DOWN:
         zvmessage("You typed <Down Arrow>", "");
         break;
      case VGI_RIGHT:
         zvmessage("You typed <Right Arrow>", "");
         break;
      case VGI_LEFT:
         zvmessage("You typed <Left Arrow>", "");
         break;
      case VGI_NEXT:
         zvmessage("You typed <NEXT>", "");
         break;
      case VGI_PREV:
         zvmessage("You typed <PREV>", "");
         break;
      case VGI_FIND:
         zvmessage("You typed <FIND>", "");
         break;
      case VGI_SELECT:
         zvmessage("You typed <SELECT>", "");
         break;
      case VGI_INSERT:
         zvmessage("You typed <INSERT>", "");
         break;
      case VGI_REMOVE:
         zvmessage("You typed <REMOVE>", "");
         break;
      case VGI_NOT_KNOWN:
         zvmessage("You typed a key which the subroutine does not recognize.", "");
         break;
      default:
         sprintf(msg, "You typed '%c' (ASCII %d)", (char) inpchar, inpchar);
         zvmessage(msg, "");
         break;
   }
}
