#include <stdio.h>
#include "vicmain_c"

void main44()
{
  char strng[201], *x, *getenv_vic(), *strcat();

  zvmessage("this should be a non-existent name:\n","");
  x = getenv_vic("XYZZY");
  if (x == NULL) {
    sprintf( strng, "%s\n", "XYZZY =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "XYZZY = ");
    strcat( x, getenv_vic("XYZZY"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");

  zvmessage("this should always return a value:\n","");
  x = getenv_vic("SPICEKER");
  if (x == NULL) {
    sprintf( strng, "%s\n", "SPICEKER =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "SPICEKER = ");
    strcat( x, getenv_vic("SPICEKER"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");

  zvmessage("'this name has a system value in VMS but none in Unix:\n","");
  x = getenv_vic("CLUE$HISTORY");
  if (x == NULL) {
    sprintf( strng, "%s\n", "CLUE$HISTORY =");
  }
  else {
    x = (char *)malloc(201);
    strcat( x, "CLUE$HISTORY = ");
    strcat( x, getenv_vic("CLUE$HISTORY"));
    sprintf( strng, "%s\n", x);
  }
  zvmessage( strng,"");
}

