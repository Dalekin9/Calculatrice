#include <stdio.h>

int foo (int x, int y)
{
  return x+y;
}

int main ()
{
  int i = 1;

  /* order of evaluation of arguments */
  printf("%d\n",foo (i++,i--));

  /* order of evaluation of summands  */
  printf("%d\n",foo(i++,1)+foo(i--,1));
}
