all:
	-mkdir $$VICCPU
	-chmod +x makelinks
	-if [ -d $$VICCPU ]; then ( cd $$VICCPU; rm -f * ); fi;
	./makelinks
	(cd $$VICCPU; make)

