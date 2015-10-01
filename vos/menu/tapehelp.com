$!
$!  This is a warning file to explain about the program TAPES.
$!
$ @TAE$MENU:CLRSCRN
$ write sys$output "            ************************************"
$ write sys$output "       **********************************************"
$ write sys$output "   ******************************************************"
$ write sys$output ""  
$ write sys$output "                TAPES is NOT a VICAR program."
$ write sys$output ""
$ write sys$output "     This program runs under VAX/VMS and not VICAR."
$ write sys$output "     For further information on TAPES, enter DCL mode"
$ write sys$output "     (type COMMAND and then DCL) and at the DCL"
$ write sys$output "     prompt ($), type:"
$ write sys$output ""
$ write sys$output "                    $ HELP TAPES"
$ write sys$output ""  
$ write sys$output "     When you are ready to run TAPES, use TAPES"  
$ write sys$output "     (also in DCL)."
$ write sys$output ""
$ write sys$output "                    $ TAPES"
$ write sys$output ""  
$ write sys$output "   ******************************************************"
$ write sys$output "       **********************************************"
$ write sys$output "            ************************************"
$ write sys$output ""
$  exit
