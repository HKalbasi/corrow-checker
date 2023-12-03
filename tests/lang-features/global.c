#include <stddef.h>

void *owned malloc(unsigned long size);
void free(void *owned ptr);

void* global_config;

enum Foo {
    Foo1 = 1,
    Foo2 = 2,
};

int main()
{
    void* p1 = malloc(Foo1);
    void* p2 = malloc(Foo2);
    global_config = p1;
    global_config = p2;
}
