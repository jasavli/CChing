// Main Program.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <string>
#include <fstream>
using namespace std;

// pridobi tekst (koda) iz navedene datoteke
void pridobiTekst(string &s, string potDoDatoteke)
{
    ifstream f(potDoDatoteke);

    string tempS;

    if (!f.is_open()) {
        cout << "Error opening the file!";
    }
    else {
        while (getline(f, tempS)) {
            s += tempS + "\n";
        }

        f.close();
    }
}

int main()
{
    string koda;
    pridobiTekst(koda, "Test1.txt");
    cout << koda;
}








// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
