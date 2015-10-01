C      Generation time:  2-APR-1988 16:33:26.29
C      
C              Symbol definitions for the TAE XI (Image File I-O)
C              subroutine package
C      
C              CAUTION:  this include file is generated from XIINC.TXT using
C              the GENERATE.COM DCL procedure.  Only XIINC.TXT should be
C              edited!!
C      
C              UNIX users BEWARE!! The XIINC.FIN file generated from
C              XIINC.TXT needs two further changes before it is ready for
C              UNIX. (1) Edit the file using the VMS editor making no changes,
C              but creating a new version of the file. This does some magic
C              with the file format making it more palatable to UNIX.
C              (2) Check for any TABS and remove them as required by
C              Apollo Fortran.
C      
C        CHANGE LOG:
C      
C              26-jul-84        New definition language...palm
C              27-sep-84        Constants & struct IFCB for C-callable image pkg...peb
C              29-mar-85        Allow xsect to be C-callable...lia
C              02-may-85        Fix for DECNET-compatible XI...palm
C              03-may-85        Fix use of FAB, RAB, and NAM...palm
C              08-may-85        Increase the value for I_FSSIZ...lia
C      
C      Lines per channel
       INTEGER xlines
       PARAMETER (xlines=1)
C      Channels in file
       INTEGER xchans
       PARAMETER (xchans=2)
C      Bytes per line
       INTEGER xlinsz
       PARAMETER (xlinsz=3)
C      Number of label records
       INTEGER xlabs
       PARAMETER (xlabs=4)
C      Bytes in one label record
       INTEGER xlabsz
       PARAMETER (xlabsz=5)
C      File organization (xci or xcs)
       INTEGER xorg
       PARAMETER (xorg=6)
C      Starting block number of line 1, channel 1
       INTEGER xstart
       PARAMETER (xstart=7)
C      Number of blocks in the file
       INTEGER xblocs
       PARAMETER (xblocs=8)
C      
C             The following indexes are VAX-dependent.
C      
C      VMS channel number for SYS$QIO
       INTEGER xqioch
       PARAMETER (xqioch=9)
C      System return code for SYS$QIO
       INTEGER xqioer
       PARAMETER (xqioer=10)
C      IOSB for SYS$QIO -- 1st elements
       INTEGER xiosb
       PARAMETER (xiosb=11)
C      IOSB -- 2nd element, dummy
       INTEGER xiosb1
       PARAMETER (xiosb1=12)
C      Sentinel word: constant IMAG
       INTEGER xsenti
       PARAMETER (xsenti=13)
C      Event flag allocated for I-O
       INTEGER xefn
       PARAMETER (xefn=14)
C      Host returned error code
       INTEGER xhcode
       PARAMETER (xhcode=15)
C      characters in file specification
       INTEGER xfilsz
       PARAMETER (xfilsz=16)
C      file specification
       INTEGER xfilnm
       PARAMETER (xfilnm=17)
C      
C              Other constants
C      
C      Channel interleaved organization
       INTEGER xci
       PARAMETER (xci=1)
C      Channel sequential organization
       INTEGER xcs
       PARAMETER (xcs=2)
C      file deletion mode (at close time)
       INTEGER xdel
       PARAMETER (xdel=3)
C      space (integer elements) to hold file name
       INTEGER xfilsp
       PARAMETER (xfilsp=25)
C      IFCB dimension: note room for spare
       INTEGER xifdim
       PARAMETER (xifdim=50)
C      Invalid line specfication (STATUS)
       INTEGER xivlin
       PARAMETER (xivlin=4)
C      Incorrect label number (STATUS)
       INTEGER xnolab
       PARAMETER (xnolab=5)
C      file save mode
       INTEGER xsave
       PARAMETER (xsave=6)
C      host file spec size in bytes
       INTEGER xfssiz
       PARAMETER (xfssiz=132)
C      Bytes per sector
       INTEGER xsect
       PARAMETER (xsect=512)
