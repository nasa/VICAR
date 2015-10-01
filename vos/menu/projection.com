$!
$! MAP PROJECTION
$!
$ @TAE$MENU:CLRSCRN
$ write sys$output "                   IBIS Map Projections"
$ write sys$output "********************************************************"
$ write sys$output "    Steps               Manual           Semi-Auto
$ write sys$output "-------------------------------------------------------"
$ write sys$output "1) Find Lat/Lon/         hand         PICREG for OL/OS"
$ write sys$output "   OL/OS Tiepoints                  & special processing"
$ write sys$output ""
$ write sys$output "2) Convert to           POLYGEN           PCOPOUT"
$ write sys$output "   GR-1 file                                 "
$ write sys$output ""
$ write sys$output "3) Map Project Lat/     POLYMAP           POLYMAP"
$ write sys$output "   Lon to UTM Coords                     "
$ write sys$output ""
$ write sys$output "4) Convert UTM          POLYREG           POLYREG"
$ write sys$output "   to NL/NS                                  "
$ write sys$output " "
$ write sys$output "5) Convert to           TIEPARM           TIEPARM"
$ write sys$output "   Parms file"
$ write sys$output ""
$ write sys$output "6) Grid Tiepoints       TIECONM           TIECONM"
$ write sys$output ""
$ write sys$output "7) Rubber Sheet       MGEOM/GEOMA       MGEOM/GEOMA"
$ write sys$output "                      or POLYGEOM       or POLYGEOM"
$  exit
