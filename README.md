# VICAR
VICAR, which stands for Video Image Communication And Retrieval, is a general purpose image processing software system that has been developed since 1966 to digitally process multi-dimensional imaging data.

We are pleased to announce that the VICAR Open Source Core version 3.0 is now available.
Please visit the VICAR Open Source release homepage:

[http://www-mipl.jpl.nasa.gov/vicar_open.html](http://www-mipl.jpl.nasa.gov/vicar_open.html).

This release includes:

- Pre-built binaries for 32- and 64-bit Linux(Red Hat 7.3) and Mac OS X 64-bit. 
- A Docker Centos7 image that runs the 64-bit Linux binaries.
- A Docker Centos7 image preloaded with VOS 64-bit Linux(Red Hat 7.3).
- 24 new application programs: 
classifier,
clusterer,
denoisetv,
destripe,
file2tcl,
gamma,
gtigeolo,
gtlabfix,
horizon,
ibisclst2,
ibisclst3,
mooresc,
randpixel,
rgb2ish,
sampler,
sc2rpc,
scinterp,
shadow,
shp2rast,
tclmath,
wedge,
wnr2005,
xyzpic2,
zipcol2
- 4 new application programs were added in Vicar Open Source Core version 2.0 but were not listed
  previously:
getzval,
gtappend,
gtmss,
vtiff3o
- 13 anomalies, 27 bug fixes/improvements.
- The .com files for GUI,P1,P2,P3 have been unpacked and the .com files have been eliminated.
  There is a subdirectory per application, with all of that application's files.	
- A new plugin that enables GDAL reading and writing of VICAR image files. 
- New Use Case Examples:
	* Landsat 7 Mosaic
	* SRTM Mosaic
	* Landsat 8 Multi-Spectral Analysis
	* Landsat 8 Pan Sharpening
	* Using ISIS with VICAR to Process Galileo Europa Imagery
	* Neptune’s Satellite Proteus

For a full list of programs being released [click here](vos/docsource/vicar/VICAR_OS_contents_v3.0.pdf).

## VICAR Discussion Forum

We have set up a [VICAR Open Source Google group](https://groups.google.com/forum/#!forum/vicar-open-source/), where you can find notifications of new releases, bug reports, and general discussion. You can join the group [here](https://groups.google.com/forum/#!forum/vicar-open-source/join). 

## Obtaining VICAR

The VICAR source code can be obtained via github.com using the links below. In addition to the VICAR source code an externals package containing 3rd party software is required. 

Note that for externals you'll find a tarball that contains all the platforms, as
well as separate ones for each platform. Externals are 3rd party packages that are required to run VICAR. See section 2 of the [Building VICAR document](vos/docsource/vicar/VICAR_build_3.0.pdf) for more information. You need only the one that
applies to your machine type.

Pre-built VICAR binaries are available at https://github.com/nasa/VICAR/releases.

#### Tarballs

* Main VICAR source code:  [Click to download](https://github.com/nasa/VICAR/tarball/master)
* Linux 32-bit externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v3.0/vicar_open_ext_x86-linux_3.0.tar.gz)
* Linux 64-bit externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v3.0/vicar_open_ext_x86-64-linx_3.0.tar.gz)
* Mac OS X externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v3.0/vicar_open_ext_x86-mac64-osx_3.0.tar.gz)
* Solaris externals:  [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v3.0/vicar_open_ext_sun-solr_3.0.tar.gz)
* All externals (don't get unless you know you need this): [Click to download](http://www-mipl.jpl.nasa.gov/vicar_os/v3.0/vicar_open_ext_3.0.tar.gz)

## Using VICAR

* The VICAR installation guide can be found [here](vos/docsource/vicar/VICAR_build_3.0.pdf).
* The VICAR quick-start guide can be found [here](vos/docsource/vicar/VICAR_guide_3.0.pdf).


Questions:  vicar_help@jpl.nasa.gov
