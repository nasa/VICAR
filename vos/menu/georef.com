$!
$! Creation of the IBIS Geo-Reference
$!
$ @TAE$MENU:CLRSCRN
$ write sys$output ""
$ write sys$output "            Creation of the IBIS Geo-Reference"
$ write sys$output "   ******************************************************"
$ write sys$output ""
$ write sys$output "                 Domestic      Foreign       Special  "
$ write sys$output "    Steps         Input         Input        Purpose"
$ write sys$output "-------------------------------------------------------"
$ write sys$output "1) Digitize     Intergraph     log proc      EDIMAGE"
$ write sys$output "                               or VQUIC"
$ write sys$output ""
$ write sys$output "2) Convert       DGN2GRAF      PCOPOUT       EDIMAGE"
$ write sys$output "   to GR-1                                    "
$ write sys$output ""
$ write sys$output "3) Convert       POLYSCRB      POLYSCRB      EDIMAGE"
$ write sys$output "   to Image                                     "
$ write sys$output ""
$ write sys$output "4) Region         PAINT         PAINT        EDIMAGE"
$ write sys$output "   Filling                                      "
$ write sys$output " "
$ write sys$output ""
$ write sys$output ""
$  exit
