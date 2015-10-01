#include "tek.h"

long rgb2tek(red, green, blue)
unsigned char	red, green, blue;
{
   long	tekindex=0, temp;

   tekindex += blue;
   temp = green;
   temp = temp << 8;
   tekindex += temp;
   temp = red;
   temp = temp << 16;
   tekindex += temp;
   return(tekindex);
}

tek2rgb(tekindex, red, green, blue)
long	tekindex;
unsigned char	*red, *green, *blue;
{
   long	temp;

   temp = tekindex;
   temp = temp & TEK_BLUE_MASK;
   *blue = temp;

   temp = tekindex;
   temp = temp & TEK_GREEN_MASK;
   temp = temp >> 8;
   *green = temp;

   temp = tekindex;
   temp = temp & TEK_RED_MASK;
   temp = temp >> 16;
   *red = temp;
}

long imp2tek(imp, color)
int		imp;
unsigned char	color;
{
   long	tekindex=0;

   tekindex = (long) color;
   if (imp == TEK_RED)
      tekindex = tekindex << 16;
   else if (imp == TEK_GREEN)
      tekindex = tekindex << 8;
   else if (imp == TEK_BLUE)
      tekindex = color;
   else
      tekindex = 0;

   return (tekindex);
}

unsigned char tek2imp(imp, tekindex)
int	imp;
long	tekindex;
{
   unsigned char	color;

   if (imp == TEK_RED)
   {
      tekindex = tekindex & TEK_RED_MASK;
      tekindex = tekindex >> 16;
   }
   else if (imp == TEK_GREEN)
   {
      tekindex = tekindex & TEK_GREEN_MASK;
      tekindex = tekindex >> 8;
   }
   else if (imp == TEK_BLUE)
   {
      tekindex = tekindex & TEK_BLUE_MASK;
   }
   else
   {
      tekindex = 0;
   }

   color = tekindex;
   return (color);
}

encode_color(red, green, blue, byte_array)
unsigned char	red, green, blue;
unsigned char	*byte_array;
{
   byte_array[0] = ((red & TEK_MASK0) >> 2) + 32;
   byte_array[1] = (((red & TEK_MASK1) << 4) | ((green & TEK_MASK2) >> 4)) + 32;
   byte_array[2] = (((green & TEK_MASK3) << 2) | ((blue&TEK_MASK4) >> 6)) + 32;
   byte_array[3] = (blue & TEK_MASK5) + 32;
}

int encode_8bit(input, input_length, output)
unsigned char	*input;
int	input_length;
unsigned char	*output;
{
   int	in_ptr, out_ptr, mode=1;

   for (in_ptr=0, out_ptr=0; in_ptr < input_length; in_ptr++)
   {
      if (mode == 1)
      {
         output[out_ptr++] = ((input[in_ptr] & TEK_MASK0) >> 2) + 32;
         mode = 2;
      }
      else if (mode == 2)
      {
         output[out_ptr++] = (((input[in_ptr-1] & TEK_MASK1) << 4) | 
                             ((input[in_ptr] & TEK_MASK2) >> 4)) + 32;
         mode = 3;
      }
      else if (mode == 3)
      {
         output[out_ptr++] = (((input[in_ptr-1] & TEK_MASK3) << 2) |
                             ((input[in_ptr] & TEK_MASK4) >> 6)) + 32;
         output[out_ptr++] = (input[in_ptr] & TEK_MASK5) + 32;
         mode = 1;
      }
   }

   if (mode == 2)
      output[out_ptr++] = ((input[in_ptr-1] & TEK_MASK1) << 4) + 32;

   if (mode == 3)
      output[out_ptr++] = ((input[in_ptr-1] & TEK_MASK3) << 2) + 32;

   output[out_ptr] = '\0';
   return(out_ptr);
}
 
loadcmap(mode)
int	mode;
{
   long jterm, x;
   long int imap[4];
   long int icmry[2], idisry[3];

   llinit(&jterm);

   idisry[0] = TEK_SINGLE_BUFFER;
   idisry[1] = TEK_TRUE_COLOR;
   idisry[2] = mode;

   lldsmd(3, idisry);

   llclmd(TEK_MACHINE_RGB, TEK_ADDITIVE, TEK_NORMAL_COLOR);

   for (x = 0; x < TEK_MAX_COLORS; x++)
   {
      imap[0] = x;
      imap[1] = x;
      imap[2] = x;
      imap[3] = x;
      llclmp(1, 4, imap);
   }

   icmry[0] = 1;
   lldcmd(1, icmry);
   lldain(TRUE_WHITE, 0, 0);
   llstop();
}
 
reverse(char_array, length, shift)
unsigned char	char_array[];
int		length, shift;
{
   int			i;
   unsigned char	temp;

   for (i = 0; i < length; i+=4)
   {
      if (shift)
      {
         temp = char_array[i];
         char_array[i] = char_array[i+3] >> 4;
         char_array[i+3] = temp >> 4;
         temp = char_array[i+1];
         char_array[i+1] = char_array[i+2] >> 4;
         char_array[i+2] = temp >> 4;
      }
      else
      {
         temp = char_array[i];
         char_array[i] = char_array[i+3];
         char_array[i+3] = temp;
         temp = char_array[i+1];
         char_array[i+1] = char_array[i+2];
         char_array[i+2] = temp;
      }
   }
}
 
int2tekint(number, string)
int	number;
char	string[];
{
  unsigned char		tempstr[80], remainder;
  int			i=0, quotient, j;

  quotient = abs(number) / 16;
  remainder = abs(number) % 16;
  if (number < 0)
    tempstr[i++] = remainder + 32;
  else
    tempstr[i++] = remainder + 48;

  while (quotient > 0)
  {
    tempstr[i++] = (quotient % 64) + 64;
    quotient = quotient / 64;
  }

  for (j = 0; j < i; j++)
    string[j] = tempstr[(i-1)-j];
  string[j] = '\0';
}

int tekint2int(string)
unsigned char	string[];
{
  int	index, remainder, number=0;

  for (index = 0; index < strlen(string)-1; index++)
  {
    remainder = string[index] - 64;
    number = (64 * number) + remainder;
  }

  if (index != 0)
    number = (16 * number);

  if (POSITIVE_SIGN(string[index]))
  {
    number += string[index] - 48;
  }
  else
  {
    number += string[index] - 32;
    number *= -1;
  }

  return(number);
}
 
parse_string(imp, num_pixels, tkstring, pixels)
int		imp, num_pixels;
unsigned char	tkstring[], pixels[];
{
  int	strindex=0, pxindex=0, offset;
  unsigned char	tmpclr[3];

  offset = TEK_HEAD_OFFSET;
  while ((tkstring[offset] < '0') || (tkstring[offset] > '?'))
    offset++;
  offset++;
  offset += 3;
  while ((tkstring[offset] < '0') || (tkstring[offset] > '?'))
    offset++;
  offset++;

  for (strindex=offset, pxindex=0; pxindex<num_pixels; pxindex++)
  {
    strindex++;					/* Skip over leading 0 */
    tmpclr[0] = tkstring[strindex++];
    if ((tmpclr[0] >= '0') && (tmpclr[0] <= '?'))
    {
      tmpclr[1] = '\0';
    }
    else
    {
      tmpclr[1] = tkstring[strindex++];
      tmpclr[2] = '\0';
    }
    if (imp == TEK_RED)
      pixels[pxindex] = (unsigned char) tekint2int(tmpclr);

    tmpclr[0] = tkstring[strindex++];
    if ((tmpclr[0] >= '0') && (tmpclr[0] <= '?'))
    {
      tmpclr[1] = '\0';
    }
    else
    {
      tmpclr[1] = tkstring[strindex++];
      tmpclr[2] = '\0';
    }
    if (imp == TEK_GREEN)
      pixels[pxindex] = (unsigned char) tekint2int(tmpclr);

    tmpclr[0] = tkstring[strindex++];
    if ((tmpclr[0] >= '0') && (tmpclr[0] <= '?'))
    {
      tmpclr[1] = '\0';
    }
    else
    {
      tmpclr[1] = tkstring[strindex++];
      tmpclr[2] = '\0';
    }
    if (imp == TEK_BLUE)
      pixels[pxindex] = (unsigned char) tekint2int(tmpclr);
  }
}
