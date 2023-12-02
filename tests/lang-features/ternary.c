#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    void *p1 = malloc(5);
    1 == 2 ? free(p1) : free(0);
    return 0;
}