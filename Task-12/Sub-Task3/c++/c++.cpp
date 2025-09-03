#include <iostream>
#include <fstream>
using namespace std;

void pyramid(int n) {
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < n - i; j++) cout << " ";
        if (i == 1) {
            cout << "*" << endl;
        } else {
            cout << "*";
            for (int j = 0; j < 2*i - 3; j++) cout << " ";
            cout << "*" << endl;
        }
    }
    for (int j = 0; j < 2*n - 1; j++) cout << "*";
    cout << endl;
}

int main() {
    int n;
    ifstream file("input.txt");
    file >> n;
    file.close();
    pyramid(n);
    return 0;
}
