#include <stdio.h>
#include <string.h>

char a[100000][33], b[33];
int c[100000];

int f(int a, int b)
{
  return a + b;
}

int main()
{
  int n, i, s = 0;
  scanf("%d", &n);
  while (n--)
  {
    scanf("%s", b);
    for (i = 0; i < s; i++)
      if (strcmp(b, a[i]) == 0)
        break;
    if (i == s)
    {
      printf("OK\n");
      strcpy(a[s++], b);
    }
    else
    {
      c[i]++;
      printf("%s%d\n", b, c[i]);
    }
  }
  return 0;
}
