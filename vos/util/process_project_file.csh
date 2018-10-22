#!/bin/csh -f

# Simple script to massage the machine-dependend conditionals in vicset1,
# run the C preprocessor, and massage them back.
# Because "#" is used so often in comments in the file, we instead use
# "@" instead.  So, first replace any occurrences of "#" with "//" (so it
# looks like a comment to cpp), then replace any @'s at the beginning of
# a line with #, then run cpp, then convert the //'s back to #.  Of course,
# because there are legitimate //'s in URL's, we first preserve *those* as
# %% and then restore them later.  Finally, we must also preserve "/*" (as
# "^^") and restore it, so cpp doesn't see bogus comments.
#
# At the end, we remove any lines starting with *.  This allows for
# meta-comments in both the main and included files, which don't appear in
# the final output.  Finally, multiple blank lines are squeezed down to one
# to look better.
#
# So, you have cpp-style conditionals that start with @.
#

if ($#argv == 0) then
  echo "Usage:"
  echo "$0 orig_file SYM1 SYM2... >modified_file"
  echo "where SYM1, SYM2 etc. are symbols to define in the preprocessor."
  exit
endif

set infile = $1

set defines = ""

foreach i ($argv[2-])
  set defines = "$defines -D$i"
end

# Replace existing // with %% and /* with ^^

sed -e 's#//#%%#g' -e 's#/\*#^^#g' <$infile >/tmp/tmp_process1.$$

# Replace existing #'s with //

sed -e 's%#%//%g' </tmp/tmp_process1.$$ >/tmp/tmp_process2.$$

# Replace @'s at the beginning of a line with #

sed -e 's/^@/#/' </tmp/tmp_process2.$$ >/tmp/tmp_process3.$$

# Run the C preprocessor.  Support // comments, and pass them through.

cpp -nostdinc -C -P -undef -Usun -Ulinux -I. $defines /tmp/tmp_process3.$$ /tmp/tmp_process4.$$

# Remove any hash from cpp

sed -e '/^#.*/d' </tmp/tmp_process4.$$ >/tmp/tmp_process5.$$

# Replace the //'s back with #

sed -e 's%//%#%g' </tmp/tmp_process5.$$ >/tmp/tmp_process6.$$

# Replace the %%'s back with // and ^^ back with /*.

sed -e 's#%%#//#g' -e 's#\^^#/*#g' </tmp/tmp_process6.$$ >/tmp/tmp_process7.$$

# Remove meta-comment lines (start with *).

sed -e '/^\*.*$/d' </tmp/tmp_process7.$$ >/tmp/tmp_process8.$$

# Squeeze multiple blank lines into one.  This is copied from the sed man
# page (Solaris), in the Examples.  See there for comments.  Results go to
# stdout.

sed -n -e '/./{' -e p -e d -e } -e '/^$/p' -e ':Empty' -e '/^$/{' -e N -e 's/.//' -e 'b Empty' -e } -e p </tmp/tmp_process8.$$

rm /tmp/tmp_process1.$$
rm /tmp/tmp_process2.$$
rm /tmp/tmp_process3.$$
rm /tmp/tmp_process4.$$
rm /tmp/tmp_process5.$$
rm /tmp/tmp_process6.$$
rm /tmp/tmp_process7.$$
rm /tmp/tmp_process8.$$

exit

