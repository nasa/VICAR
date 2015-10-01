#!/bin/csh
# This script fetches TAE from the canonical storage area (/usr/local/tae).
# VERY IMPORTANT: It assumes the current directory is the top level of the
# VICAR tree!

chmod -R u+w tae
mkdir tae52
(cd tae52; tar xvf ../../external/tae/tae5.2.solaris.tar)
chmod -R u+w tae52
cp tae52_changes/bin/all/platform tae52/bin/all/platform
chmod +x tae52/bin/all/platform

mkdir tae53
(cd tae53; tar xvf ../../external/tae/tae5.3.axp-unix.tar)
chmod -R u+w tae53
cp tae53_changes/bin/all/platform tae53/bin/all/platform
chmod +x tae53/bin/all/platform

# Mac OS X (or at least its HFS+ file system) is only sort-of case sensitive.
# The softlink of e.g. PGMINC.FIN to pgminc.fin is problematic; they appear to
# be the same file.  A tar transfer can pick up the symlink instead of the
# file; the symlink then appears to point to itself.  So, we simply make a
# copy instead of a link.  This way, no matter which one is actually picked
# up, we get a valid file.  This should not hurt other platforms as long as
# the two are kept in sync (which should not be a problem since this copy is
# made during the build).

#rm tae53/include/PGMINC.FIN
#cp tae53/include/pgminc.fin tae53/include/PGMINC.FIN
#rm tae53/include/XIINC.FIN
#cp tae53/include/xiinc.fin tae53/include/XIINC.FIN

