#!/bin/csh
source $V2TOP/vicset2.csh
foreach inc_file (*.com)
	vunpack $inc_file -q system
end
