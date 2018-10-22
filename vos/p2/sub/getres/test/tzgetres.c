#include "xvmaininc.h"
#include "ftnbridge.h"

void   FTN_NAME(tzgetres) ()
{
  int    camera ;
  float  rloc[404] ;

  zvmessage(" ", " ") ;
  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge version  ****** ", " ");
  zvmessage(" ", " ") ;

  camera = 1;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 2;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 3;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 4;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 5;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 6;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 7;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

  camera = 8;
  zgetres(&rloc[0], camera) ;
  zprnt(4, 1, &camera, "  Camera # = .") ;
  zvmessage("     *****   Reseau Locations   ***** "," ");
  zprnt(7, 404, &rloc[0], ".") ;
  zvmessage(" ", " ") ;

}
