#include <stddef.h>

void* restrict malloc(unsigned long size);

int main() {
    void* p1 = malloc(5);
    void* p2 = p1;
    // p2 = 0;
    return 0;
}