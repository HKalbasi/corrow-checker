#include <stddef.h>

void *owned falloc(unsigned long size);

int main()
{
    void *p1 = falloc(5);
    void *p2 = p1;
    p2 = falloc(10);
    return 0;
}
