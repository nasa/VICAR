# This script fetches TAE52 from the canonical storage area (/usr/local/tae).
# VERY IMPORTANT: It assumes the current directory is the top level of the
# VICAR tree!
# 6/22/00
#
chmod -R u+w tae
mkdir tae52
(cd tae52; tar xvf /usr/local/tae/tae5.2.solaris.tar)
chmod -R u+w tae52
cp tae52_changes/bin/all/platform tae52/bin/all/platform
chmod +x tae52/bin/all/platform

