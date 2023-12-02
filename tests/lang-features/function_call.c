#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

int sum;

void free_and_sum(int *owned ptr) {
    //sum += *ptr;
    free(ptr);
}

int main()
{
    void *p1 = malloc(4);
    void *p2 = malloc(4);
    //*p2 = 77;
    free_and_sum(p2);
    return 0;
}