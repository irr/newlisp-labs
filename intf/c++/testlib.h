#include <iostream>

using namespace std;

class Test {
	private:
	    string name;
	    int value;

	public:
		Test(const string& name, const int& value) : name(name), value(value) {
			cout << this << ": constructor" << endl;
		}

		~Test() {
			cout << this << ": destructor" << endl;
		}

		void setName(const string& name) {
			cout << this << ": setName = " << name << endl;
			this->name = name;
		}

		string getName() const {
			cout << this << ": getName = " << name << endl;
			return name;
		}

		void setValue(const int& value) {
			cout << this << ": setValue = " << value << endl;
			this->value = value;
		}

		int getValue() const {
			cout << this << ": getValue = " << value << endl;
			return value;
		}
};
