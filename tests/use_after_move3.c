#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    int *p1 = malloc(10);
    int *p2 = p1;
    free(p1); // use after move
    free(p2);
    return 0;
}