#include <stddef.h>

void *owned malloc(unsigned long size);

int main()
{
    void *p1 = malloc(5);
    void *p2 = p1;
    p2 = malloc(10);
    return 0;
}
