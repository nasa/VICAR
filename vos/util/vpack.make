#
# Build the VPACK and VUNPACK programs
#

all: $(VICCPU)/vpack $(VICCPU)/vunpack

$(VICCPU)/vpack: vpack.c pack.h
	cd $(VICCPU); cc -o vpack ../vpack.c

$(VICCPU)/vunpack: vunpack.c pack.h
	cd $(VICCPU); cc -o vunpack ../vunpack.c

