$!****************************************************************************
$!
$! Build proc for MIPL module qksort
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:55
$!
$! Execute by entering:		$ @qksort
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module qksort ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("qksort.imake") .nes. ""
$   then
$      vimake qksort
$      purge qksort.bld
$   else
$      if F$SEARCH("qksort.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qksort
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qksort.bld "STD"
$   else
$      @qksort.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qksort.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qksort.com -
	-s qksort.c -
	-i qksort.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create qksort.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* This routine is called by a fortran subroutine 
   to do quick sorting */
#define M          7 
#define NSTACK  1000 
qksort_(array1,array2,n)
     int array1[],array2[],*n;
{ 
     qksort(array1,array2,*n);
}
qksort(arr1,arr2,n)
     int n;
     int arr1[],arr2[];
{
  int l=0,jstack=0,j,ir,iq,i;
  int istack[NSTACK+1];
  int a,b;
  ir=n-1;
  for(;;){
    if(ir-l < M){
	for(j=l;j<=ir;j++){
	   a=arr1[j];
	   b=arr2[j];
	   for(i=j-1;arr1[i]>a && i>=0;i--){
	     arr1[i+1]=arr1[i];
	     arr2[i+1]=arr2[i];
           } /* for */
	   arr1[i+1]=a;
	   arr2[i+1]=b;

        } /* for */ 
	
	if(jstack==0)return;
	ir=istack[jstack--];
	l=istack[jstack--];
   }/* if */ 
   else {
     i=l;
     j=ir;
     iq=(l+ir)/2;
     a=arr1[iq];
     b=arr2[iq];
     arr1[iq]=arr1[l];
     arr2[iq]=arr2[l];
     for(;;){
       while (j>=0 && a<arr1[j])j--;
       if(j<=i){
         arr1[i]=a;
         arr2[i]=b;
         break;
     }/* for */
     arr1[i]=arr1[j];
     arr2[i++]=arr2[j];
     while(a>arr1[i] && i<n)i++;
     if(j<=i){
       arr1[(i=j)]=a;
       arr2[(i=j)]=b;
       break;
     }/* if */
     arr1[j]=arr1[i];
     arr2[j--]=arr2[i];
   } /* for  */
   if(ir-1 >= i-l){
     istack[++jstack]=i;
     istack[++jstack]=ir;
     ir=i-1;
   }/* if */ 
   else {
     istack[++jstack]=l;
     istack[++jstack]=i-1;
     l=i+1;
   }/* else */
  } /* else */
 }/* for */
}/* qsort */



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qksort.imake

#define SUBROUTINE qksort

#define MODULE_LIST qksort.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
