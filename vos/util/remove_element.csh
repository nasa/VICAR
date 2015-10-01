#!/bin/tcsh -f
# Simple script to remove any instances of $2 in the colon-separated list
# $1.  Result is printed to stdout.
set output
set pathvar = `echo "$1" | tr ':' ' '`
foreach i ($pathvar)
   if ($i !~ $2) set output=($output $i)
end
echo $output | tr ' ' ':'
exit

