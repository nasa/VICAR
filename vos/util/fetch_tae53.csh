# This script fetches TAE53 from the canonical storage area (/usr/local/tae).
# VERY IMPORTANT: It assumes the current directory is the top level of the
# VICAR tree!
# 6/22/00
#
chmod -R u+w tae
mkdir tae53
(cd tae53; tar xvf ../../external/tae/tae5.3.axp-unix.tar)
chmod -R u+w tae53
cp tae53_changes/bin/all/platform tae53/bin/all/platform
chmod +x tae53/bin/all/platform

