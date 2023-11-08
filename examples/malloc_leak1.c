#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    void *p1 = malloc(5);
    void *p2 = p1;
    // free(p2);
    p2 = malloc(10);
    // free(p2);
    return 0;
}