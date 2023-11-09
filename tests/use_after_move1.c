#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    int *p1 = malloc(10);
    free(p1);
    p1[0] = 5; // use after move
    return 0;
}