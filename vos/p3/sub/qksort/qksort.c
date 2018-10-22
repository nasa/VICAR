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



