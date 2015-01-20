// g++ -std=c++0x -pthread test.cc -o test && ./test

#include "testlib.h"

using namespace std;

static Test obj("ivan", 100);

extern "C" {
	void staticTest();
	int dynamicTest(int value);
}

void staticTest() {
    obj.getName();
    obj.getValue();
}

int dynamicTest(int value) {
    Test test("alessandra", 1000);
    test.getName();
    test.getValue();
    test.setValue(value);
    return test.getValue();
}

int main() {
    staticTest();
    dynamicTest(1972);
}
