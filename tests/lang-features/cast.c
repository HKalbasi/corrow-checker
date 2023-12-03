#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int main()
{
    // p1 is leaked, p2 is not
    void *p1 = malloc(5);
    size_t p1_addr = (size_t)p1;
    void *p2 = malloc(5);
    size_t p2_addr = (size_t)p2;
    void *p2_again = (void*)p2_addr;
    free(p2_again);
}