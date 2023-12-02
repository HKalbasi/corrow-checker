#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    void *p1 = (malloc(5), 0);
    void *p2 = (0, malloc(5));
    free(p1);
    free(p2);
    return 0;
}