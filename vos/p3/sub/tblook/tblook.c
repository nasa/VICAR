static char sccsTblook[] = "@(#)  tblook.c 1.1 1/29/87 ";

tblook_ (table, input1, input2, output, ns)
unsigned char table[256][256], *input1, *input2, *output;
int *ns;
{
  register i;
  unsigned char t_table[256][256];

  for (i=0;i<*ns;i++) {
    output[i] = table[input1[i]][input2[i]];
  }
}




