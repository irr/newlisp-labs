#include <stdio.h>

static char name[] = "ivan";
static int  value = 100;

void staticTest() {
    printf("staticTest: %s\n", name);
}

int dynamicTest(int v) {
    printf("dynamicTest: %d\n", value);
    value = v;
    printf("dynamicTest: %d\n", value);
    return value;
}

int main() {
    staticTest();
    dynamicTest(1972);
}
