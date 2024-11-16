// Main Program.cpp : This file contains the 'main' function. Program execution begins and ends there.


#include <iostream>
#include <string>
#include <fstream>
#include <list>
#include <sstream>

using namespace std;

// pridobi tekst (koda) iz navedene datoteke
void pridobiTekst(string& s, const string& potDoDatoteke)
{
    ifstream f(potDoDatoteke);
    string tempS;

    if (!f.is_open()) {
        cout << "Error opening the file!" << endl;
    }
    else {
        while (getline(f, tempS)) {
            s += tempS + "\n";
        }
        f.close();
    }
}
// Za vsako vrstico doda presledek, zaradi napake pri ločitvi v blokih
void DodajPresledekZaVrsticami(string& koda)
{
    stringstream ss(koda);
    string token;
    string result;

    while (getline(ss, token, '\n'))
    {
        result += token + " ";
    }

    koda = result;
}
// loči vso kodo glede na narekovaje -> notri ali vzunaj, in shrani aglede na to
string LociPoNarekovajih(string koda, list<string>& stringi) {
    bool vNarekovajih = false;
    list<string> vrstica;
    stringstream ss(koda);
    string token;
    string s;

    while (getline(ss, token, '"')) {
        if (vNarekovajih) {
            stringi.push_back(token);
            vrstica.push_back("\"\"");
        }
        else {
            vrstica.push_back(token);
        }
        vNarekovajih = !vNarekovajih;
    }

    
    for (const auto& part : vrstica) {
        s += part;
    }

    return s;
}
// pridobi element v listu, ki je na polju polje
string DobiItemIzLista(list<string> list, int polje) 
{
    auto it = list.begin();
    advance(it, polje);
    return *it;
}


// Stringe, ki smo jih dali ven, ponovno vpiše v narekovaje
list<list<string>> ZdruzitevStringovVNarekovaje(list <list< string >> koda, list<string>& stringi)
{
    int i = 0;
    for (auto& row : koda) 
    {
        for (auto& elem : row) 
        {
            if (elem == "\"\"") 
            {
                elem = "\"" + DobiItemIzLista(stringi, i) + "\"";
                i++;
            }
        }
    }
    return koda;
}
//loči vse vrstice kode po znaku ;
list<string> lociPoVejicah(const string& s)
{
    list<string> vrsticeKode;
    stringstream ss(s);
    string token;


    while (getline(ss, token, ';')) 
    {
        vrsticeKode.push_back(token); 
    }

    return vrsticeKode;
}

// vsako vrstico kode razčleni na posamezne dele
// primer: '"cwrite = "Hello World!"' razstavi na 'cwrite','=','"Hello','World!"' 
list<list<string>> lociVrsticePoBlockih(const list<string>& vrstice) 
{
    list<list<string>> list2D; 

    for (const string& vrstica : vrstice) 
    {
        list<string> list1D;  
        stringstream ss(vrstica);
        string token;

  
        while (ss >> token) 
        {
            list1D.push_back(token);
        }

        list2D.push_back(list1D);
    }
    return list2D;
    
    /*for (const auto& row : list2D)
    {
        for (const auto& elem : row) 
        {
            cout << elem << endl;
        }
        cout << "---" << endl;
    }*/
}
int main()
{
    list<string> stringi;
    string koda;
    pridobiTekst(koda, "Test1.txt");
    DodajPresledekZaVrsticami(koda);
    koda = LociPoNarekovajih(koda, stringi);
    list<string> tokens = lociPoVejicah(koda);
    list<list<string>> razclenjenaKoda = lociVrsticePoBlockih(tokens);
    razclenjenaKoda = ZdruzitevStringovVNarekovaje(razclenjenaKoda, stringi);



    for (const auto& row : razclenjenaKoda)
    {
        for (const auto& elem : row)
        {
            cout << elem << endl;
        }
        cout << "---" << endl;
    }


    return 0;
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
