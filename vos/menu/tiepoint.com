$!
$! IBIS TIEPOINT/COREGISTRATION 
$!
$ @TAE$MENU:CLRSCRN
$ write sys$output ""
$ write sys$output "            IBIS Tiepoint and Coregistration"
$ write sys$output "*******************************************************"
$ write sys$output ""
$ write sys$output "    Steps       Manual     Semi-Automatic   Automatic"
$ write sys$output "-------------------------------------------------------"
$ write sys$output "1) Find         hand          PICREG         PICMATCH"
$ write sys$output "   Tiepoints"
$ write sys$output ""
$ write sys$output "2) Convert to   IGENER        special        PICMATCH"
$ write sys$output "   Interface    or EDIBIS     processing "
$ write sys$output ""
$ write sys$output "3) Convert to   TIEPARM       TIEPARM        TIEPARM"
$ write sys$output "   PARMS file                                   "
$ write sys$output ""
$ write sys$output "4) Grid         TIECONM       TIECONM        TIECONM"
$ write sys$output "   Tiepoints                                      "
$ write sys$output " "
$ write sys$output "5) Plot         PLOTTING      PLOTTING       PLOTTING"
$ write sys$output "   Tiepoints    & TIEPLOT     & TIEPLOT      & TIEPLOT"
$ write sys$output ""
$ write sys$output "6) Rubber       MGEOM/GEOMA   MGEOM/GEOMA    MGEOM/GEOMA"
$ write sys$output "   Sheet        or POLYGEOM   or POLYGEOM    or POLYGEOM"
$  exit
