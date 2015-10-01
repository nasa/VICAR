$!
$! MULTIVARIATE  ANALYSIS
$!
$ @TAE$MENU:CLRSCRN
$ write sys$output ""
$ write sys$output "             IBIS Multivariate Analysis"
$ write sys$output "*******************************************************"
$ write sys$output ""
$ write sys$output "    Steps                Image              Tabular"
$ write sys$output "-----------------------------------------------------"
$ write sys$output "1) Preprocess             MSS             application"
$ write sys$output "   Input Data                              specific"
$ write sys$output ""
$ write sys$output "2) Convert to           MSSIBIS             ------   "
$ write sys$output "   Interface file    "
$ write sys$output ""
$ write sys$output "3) Analysis            IBISSTAT or        IBISSTAT or"
$ write sys$output "                       MF or IBISLSQ      MF or IBISLSQ"
$ write sys$output ""
$ write sys$output ""
$ write sys$output ""
$ write sys$output ""
$ write sys$output ""
$ write sys$output ""
$  exit
