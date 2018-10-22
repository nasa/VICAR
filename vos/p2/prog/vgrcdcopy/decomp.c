#include <stdio.h>
#include <stdlib.h>

/****************************************************************************
*_TITLE node_def - definition of NODE type                                  *
*_ARGS NONE                                                                 *

*_DESCR node_def defines the basic element of a node used to build the      *
*       Huffman tree for data decompression.  The code declares a user      *
*       defined type that consists of an integer field and a left and right *
*       pointer field.  The *right and *left pointers point to another      *
*       NODE structure, a recursive reference of itself.  The dn field will *
*       contain a -1 if the node is not a leaf (or end node) otherwise it   *
*       will contain the pixel difference value.  This value is then        *
*       subtracted from the preceding pixel and 256 is added to normalize   *
*       the actual first difference value.  The left and right pointers are *
*       undefined (NULL) if the node is a leaf, otherwise they will point   *
*       to the next adjacent node in the tree.                              *

*_LIMS  This definition has been tested on VAX 750 (VMS 4.7), DEC MicroVAX  *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original Version            *
*_END                                                                       *
*****************************************************************************/

  typedef struct leaf 
              {
                struct leaf *right;
                short int dn;
                struct leaf *left;
               } NODE;


/*************************************************************************
 Declare the tree pointer. This pointer will hold the root of the tree
 once the tree is created by the accompanying routine huff_tree.
**************************************************************************/
  NODE *tree;


 void decompress(ibuf,obuf,nin,nout)
/****************************************************************************
*_TITLE decompress - decompresses image lines stored in compressed format   *
*_ARGS  TYPE       NAME      I/O        DESCRIPTION                         */
        char       *ibuf;  /* I         Compressed data buffer              */
        char       *obuf;  /* O         Decompressed image line             */
        int   *nin;   /* I         Number of bytes on input buffer     */
        int   *nout;  /* I         Number of bytes in output buffer    */

/*
*_DESCR This routine decompresses Huffman encoded compressed data.  Huffman *
*       coding uses a variable number of bits to encode different values of *
*       the original data.  The compression results because the most        *
*       frequently occurring data value is represented in the smallest      *
*       number of bits.  This routine is called by a main program to        *
*       decompress an image line contained in ibuf.  The first byte of      *
*       ibuf contains the actual first pixel value of the line.  All        *
*       other bytes in the input line contain the compressed data.  The     *
*       output buffer, obuf, will contain the decompressed image line       *
*       after the call to the "dcmprs" routine.                             *

*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       Please note that the calling convention used for this routine       *
*       follow VAX/VMS FORTRAN pass-by-reference standards to allow linking *
*       to FORTRAN calling routines.  C programmers must be aware of this   *
*       when coding calling routines.                                       *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/


  {
 /* The external root pointer to tree */
    extern NODE *tree;

 /* Declare functions called from this routine */
    void dcmprs();


/*************************************************************************
  This routine is fairly simple as it's only function is to call the 
  routine dcmprs. 
**************************************************************************/

    dcmprs(ibuf,obuf,nin,nout,tree);
  
    return;
  }



void decmpinit(hist)
/***************************************************************************
*_TITLE decmpinit - initializes the Huffman tree                           *
*_ARGS  TYPE       NAME      I/O        DESCRIPTION                        */
        int   *hist;  /* I         First-difference histogram.  This  *
*                                       array MUST be dimensioned to at    *
*                                       least 511 elements.  There are a   *
*                                       total of 511 first-differerence    *
*                                       values.  The least first-          *
*                                       difference value is 0-255 or -255, *
*                                       while the largest first-difference *
*                                       is 255-0 or 255.  The first-       *
*                                       difference values are normalized   *
*                                       for table use to the range 1 to    *
*                                       511 by adding 256 to each          *
*                                       difference.                        */

/*
*_DESCR This routine is relatively simple; it is responsible for creating  *
*       the Huffman tree from the first difference histogram of the image. *
*       In a first difference histogram, the first byte in each image line *
*       is the actual pixel value.  All other pixels are obtained by       *
*       subtracting the first difference value at the current pixel from   *
*       the actual value of the preceding pixel and adding 256 to provide  *
*       a positive number.  The I-th element of the array "hist" should be *
*       the frequency of occurances for the value I.  Note the declaration *
*       of the pointer tree.  This external variable is defined by this    *
*       routine.  It returns the pointer to the root of the Huffman tree   *
*       created by "huff_tree". The huff_tree routine will automatically   *
*       swap the low and high order bytes of the 32-bit elements in the    *
*       of the first difference histogram for the computer systems which   *
*       store integers in "most significant byte first" order. For computer*
*       systems which store 32-bit words in "least significant byte first  *
*       order, no swapping of the bytes occurs.


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX    *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an      *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other   *
*       systems, check for portability conflicts.                          *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original C Version         *
*_END                                                                      *
****************************************************************************/

{
  extern NODE *tree;          /* Huffman tree root pointer */

  /* Specify the calling function to initialize the tree */
  NODE *huff_tree(int *);

/****************************************************************************
  Simply call the huff_tree routine and return.
*****************************************************************************/

  tree = huff_tree(hist);

  return;
 }





NODE *huff_tree(hist)
/****************************************************************************
*_TITLE huff_tree - constructs the Huffman tree; returns pointer to root    *
*_ARGS  TYPE          NAME        I/O   DESCRIPTION                         */
        int     *hist;     /* I    First difference histogram          */
/*
*_DESC  huff_tree constructs a binary Huffman tree and returns the pointer  *
*       to the root of the tree.  The implementation here may not be the    *
*       most efficient, but conditions of the algorithm used to compress    *
*       the data governed the design of this algorithm.  Other              *
*       implementations are available in FORTRAN and VAX MACRO Assembler.   *
*       This routine allocates memory as needed to construct the tree.      *
*       The tree is implemented as a user defined structure described       *
*       above.  The algorithm uses an array of node pointers allocated      *
*       for all possible values.  This array is then initialized by         *
*       assigning all leafs to the array.  Each leaf has a cooresponding    *
*       frequency assigned to it and the frequencies are sorted in ascending*
*       order.  All zero frequencies are ignored and tree construction      *
*       begins.  The tree is built by combining the two least occuring      *
*       frequencies into one node.  This new node is treated as one by      *
*       adding together the two frequencies forming a cummulative frequency *
*       of the combining nodes.  The second smallest node now contains the  *
*       newly combined node and the smallest node is deleted from the list. *
*       The frequency list is then resorted to determine the next two node  *
*       combinations until one node is left.  This node will be the root of *
*       the tree.  This pointer is then returned to the calling routine.    *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       This routine uses the memory allocation routine "malloc".  Check    *
*       for call specifications and casting portability of these features   *
*       for the compiler in used.                                           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original C Version          *
*_END                                                                       *
*****************************************************************************/

  {
  /*  Local variables used */
    int freq_list[512];      /* Histogram frequency list */
    NODE **node_list;             /* DN pointer array list */
    
    register int *fp;        /* Frequency list pointer */
    register NODE **np;           /* Node list pointer */

    register int num_freq;   /* Number non-zero frequencies in histogram */
    int sum;                 /* Sum of all frequencies */

    register short int num_nodes; /* Counter for DN initialization */
    register short int cnt;       /* Miscellaneous counter */

    short int znull = -1;         /* Null node value */
    
    register NODE *temp;          /* Temporary node pointer */

  /* Functions called */
    void sort_freq();
    NODE *new_node();

/***************************************************************************
  Allocate the array of nodes from memory and initialize these with numbers
  corresponding with the frequency list.  There are only 511 possible 
  permutations of first difference histograms.  There are 512 allocated 
  here to adhere to the FORTRAN version.
****************************************************************************/

   fp = freq_list;
   node_list = (NODE **) malloc(sizeof(temp)*512);
   if (node_list == NULL)
    {
      printf("\nOut of memory in huff_tree!\n");
      exit(1);
    }
   np = node_list;

   for (num_nodes=1, cnt=512 ; cnt-- ; num_nodes++)
     {
/**************************************************************************
    The following code has been added to standardize the VAX byte order
    for the "int" type.  This code is intended to make the routine
    as machine independant as possible.
***************************************************************************/
        unsigned char *cp = (unsigned char *) hist++;
        unsigned int j;
        short int i;
        for (i=4 ; --i >= 0 ; j = (j << 8) | *(cp+i));

/* Now make the assignment */
        *fp++ = j;
        temp = new_node(num_nodes);
        *np++ = temp;
     }

     (*--fp) = 0;         /* Ensure the last element is zeroed out.  */

/***************************************************************************
  Now, sort the frequency list and eliminate all frequencies of zero.
****************************************************************************/

  num_freq = 512;
  sort_freq(freq_list,node_list,num_freq);

  fp = freq_list;
  np = node_list;

  for (num_freq=512 ; (*fp) == 0 && (num_freq) ; fp++, np++, num_freq--);


/***************************************************************************
  Now create the tree.  Note that if there is only one difference value,
  it is returned as the root.  On each interation, a new node is created
  and the least frequently occurring difference is assigned to the right
  pointer and the next least frequency to the left pointer.  The node 
  assigned to the left pointer now becomes the combination of the two
  nodes and it's frequency is the sum of the two combining nodes.
****************************************************************************/

  for (temp=(*np) ; (num_freq--) > 1 ; )
    {
        temp = new_node(znull);
        temp->right = (*np++);
        temp->left = (*np);
        *np = temp;
        *(fp+1) = *(fp+1) + *fp;
        *fp++ = 0;
        sort_freq(fp,np,num_freq);
    }
  
  return temp;
 }




NODE *new_node(value)
/****************************************************************************
*_TITLE new_node - allocates a NODE structure and returns a pointer to it   *
*_ARGS  TYPE        NAME        I/O     DESCRIPTION                         */
        short int   value;    /* I      Value to assign to DN field         */

/*
*_DESC  new_node allocates virtual memory for a new NODE structure.  It     *
*       initializes the right and left pointers to NULL and the dn field    *
*       is initialized to the passed parameter 'value'.                     *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       This routine uses the malloc routine that requests virtual memory   *
*       from the system.  Most C libraries have some version of this        *
*       function.  Check the reference manuals for compatibility.           *

*_HIST  17-FEB-88  Kris Becker  USGS, Flagstaff Original Version            *
*_END                                                                       *
*****************************************************************************/

  {
    NODE *temp;         /* Pointer to the memory block */


/***************************************************************************
  Allocate the memory and intialize the fields.
****************************************************************************/

  temp = (NODE *) malloc(sizeof(NODE));

  if (temp != NULL) 
    {
      temp->right = NULL;
      temp->dn = value;
      temp->left = NULL;
    }
  else
    {
       printf("\nOut of memory in new_node!\n");
       exit(1);
    }

   return temp;
  }



 void sort_freq(freq_list,node_list,num_freq)
/****************************************************************************
*_TITLE sort_freq - sorts frequency and node lists in increasing freq. order*
*_ARGS  TYPE       NAME            I/O  DESCRIPTION                         */
        int   *freq_list;   /* I   Pointer to frequency list           */
        NODE       **node_list;  /* I   Pointer to array of node pointers   */
        int   num_freq;     /* I   Number of values in freq list       */

/*
*_DESCR This routine uses an insertion sort to reorder a frequency list     *
*       in order of increasing frequency.  The corresponding elements       *
*       of the node list are reordered to maintain correspondence.  The     *
*       node list is actually a pointer to an array of pointers to tree     *
*       nodes.

*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/
  {
    /* Local Variables */
    register int *i;       /* primary pointer into freq_list */
    register int *j;       /* secondary pointer into freq_list */
    
    register NODE **k;          /* primary pointer to node_list */
    register NODE **l;          /* secondary pointer into node_list */

    int temp1;             /* temporary storage for freq_list */
    NODE *temp2;                /* temporary storage for node_list */

    register int cnt;      /* count of list elements */


/************************************************************************
  Save the current element - starting with the second - in temporary
  storage.  Compare with all elements in first part of list moving 
  each up one element until the element is larger.  Insert current 
  element at this point in list.
*************************************************************************/

   if (num_freq <= 0) return;      /* If no elements or invalid, return */

   for (i=freq_list, k=node_list, cnt=num_freq ; --cnt ; *j=temp1, *l=temp2)
     {
        temp1 = *(++i);
        temp2 = *(++k);

        for (j = i, l = k ;  *(j-1) > temp1 ; )
          {
            *j = *(j-1);
            *l = *(l-1);
            j--;
            l--;
            if ( j <= freq_list) break;
          }
 
     }
  return;
  }




 void dcmprs(ibuf,obuf,nin,nout,root)
/****************************************************************************
*_TITLE dcmprs - decompresses Huffman coded compressed image lines          *
*_ARGS  TYPE       NAME       I/O       DESCRIPTION                         */
        char       *ibuf;   /* I        Compressed data buffer              */
        char       *obuf;   /* O        Decompressed image line             */
        int   *nin;    /* I        Number of bytes on input buffer     */
        int   *nout;   /* I        Number of bytes in output buffer    */
        NODE       *root;   /* I        Huffman coded tree                  */

/*
*_DESCR This routine follows a path from the root of the Huffman tree to    *
*       one of it's leaves.  The choice at each branch is decided by the    *
*       successive bits of the compressed input stream.  Left for 1, right  *
*       for 0.  Only leaf nodes have a value other than -1.  The routine    *
*       traces a path through the tree until it finds a node with a value   *
*       not equal to -1 (a leaf node).  The value at the leaf node is       *
*       subtracted from the preceeding pixel value plus 256 to restore      *
*       the uncompressed pixel.  This algorithm is then repeated until the  *
*       entire line has been processed.                                     *


*_LIMS  This routine has been tested on VAX 750 (VMS 4.6), DEC MicroVAX     *
*       (ULTRIX 2.2), SUN Workstation (UNIX 4.2, release 3.4), and an       *
*       IBM PC (MICROSOFT 4.0, 3.0 compilers).  When converting to other    *
*       systems, check for portability conflicts.                           *

*       Please note that the calling convention used for these routines     *
*       follow VAX/VMS FORTRAN pass-by-reference standards to allow         *
*       linking to FORTRAN calling routines.  C programmers must be aware   *
*       of this when coding calling routines.                               *

*_HIST  17-FEB-88 Kris Becker USGS, Flagstaff Original C Version            *
*_END                                                                       *
*****************************************************************************/
  {
    /* Local Variables */
    register NODE *ptr = root;        /* pointer to position in tree */
    register unsigned char test;      /* test byte for bit set */
    register unsigned char idn;       /* input compressed byte */

    register char odn;                /* last dn value decompressed */

    char *ilim = ibuf + *nin;         /* end of compressed bytes */
    char *olim = obuf + *nout;        /* end of output buffer */



/**************************************************************************
  Check for valid input values for nin, nout and make initial assignments.
***************************************************************************/

    if (ilim > ibuf && olim > obuf)
       odn = *obuf++ = *ibuf++;
    else
       {
           printf("\nInvalid byte count in dcmprs!\n");
           exit(1);
       }

/**************************************************************************
  Decompress the input buffer.  Assign the first byte to the working 
  variable, idn.  An arithmatic and (&) is performed using the variable
  'test' that is bit shifted to the right.  If the result is 0, then
  go to right else go to left.
***************************************************************************/

    for (idn=(*ibuf) ; ibuf < ilim  ; idn =(*++ibuf))
     {
        for (test=0x80 ; test ; test >>= 1)
           {
            ptr = (test & idn) ? ptr->left : ptr->right;

            if (ptr->dn != -1) 
              {
                if (obuf >= olim) return;
                odn -= ptr->dn + 256;
                *obuf++ = odn;
                ptr = root;
              }
          }
     }
   return;
  }

void free_tree(nfreed)
/****************************************************************************
*_TITLE free_tree - free memory of all allocated nodes                      *
*_ARGS  TYPE       NAME       I/O        DESCRIPTION                        */
        int   *nfreed;  /* O        Return of total count of nodes     *
*                                        freed.                             */

/*
*_DESCR This routine is supplied to the programmer to free up all the       *
*       allocated memory required to build the huffman tree.  The count     *
*       of the nodes freed is returned in the parameter 'nfreed'.  The      *
*       purpose of the routine is so if the user wishes to decompress more  *
*       than one file per run, the program will not keep allocating new     *
*       memory without first deallocating all previous nodes associated     *
*       with the previous file decompression.                               *

*_HIST  16-AUG-89 Kris Becker   USGS, Flagstaff Original Version            *
*_END                                                                       *
****************************************************************************/

{
	int total_free = 0;

	extern NODE *tree;      /* Huffman tree root pointer */

/* Specify the function to free the tree */
	int free_node();
/****************************************************************************
  Simply call the free_node routine and return the result.
*****************************************************************************/

	*nfreed = free_node(tree,total_free);

	return;
}

int free_node(pnode,total_free)
/***************************************************************************
*_TITLE free_node - deallocates an allocated NODE pointer
*_ARGS  TYPE     NAME          I/O   DESCRIPTION                           */
        NODE     *pnode;       /* I  Pointer to node to free               */
        int total_free;   /* I  Total number of freed nodes           */

/*
*_DESCR  free_node will check both right and left pointers of a node       *
*        and then free the current node using the free() C utility.        *
*        Note that all nodes attached to the node via right or left        *
*        pointers area also freed, so be sure that this is the desired     *
*        result when calling this routine.                                 *

*        This routine is supplied to allow successive calls to the         *
*        decmpinit routine.  It will free up the memory allocated          *
*        by previous calls to the decmpinit routine.  The call to free     *
*        a previous huffman tree is:  total = free_node(tree,(long) 0);    *
*        This call must be done by the programmer application routine      *
*        and is not done by any of these routines.                         *
*_HIST   16-AUG-89  Kris Becker U.S.G.S  Flagstaff Original Version        */
{
	if (pnode == (NODE *) NULL) return(total_free);
	
	if (pnode->right != (NODE *) NULL)
		total_free = free_node(pnode->right,total_free);
	if (pnode->left != (NODE *) NULL)
		total_free = free_node(pnode->left,total_free);

	free((char *) pnode);
	return(total_free + 1);
}

