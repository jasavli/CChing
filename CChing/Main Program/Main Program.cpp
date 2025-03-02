﻿// Main Program.cpp : This file contains the 'main' function. Program execution begins and ends there.

//    PJ#

#include <iostream>
#include <string>
#include <fstream>
#include <list>
#include <sstream>
#include "Spremenljivka.h"

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
// loči vso kodo glede na narekovaje -> notri ali zunaj, in shrani glede na to
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
/*string DobiImeIzLista(list<Spremenljivka> list, int polje)
{
    auto it = list.begin();
    advance(it, polje);
    return it->ime;
}*/
// preveri, če je item v listu
bool JeItemVListu(list<string> list, string item) {
    for (const auto& element : list) {
        if (item == element) {
            return true;
        }
    }
    return false;
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
    
}
// preveri, če lahko dan string spremenimo v int ali ne
bool aliJeStringInt(string blok) 
{
    auto it = blok.begin();
    for (int i = 0; i < blok.size(); i++) {
        if (!(isdigit(*it))) return false;
        advance(it, i);
    }
    return true;
}
// preveri v katerem listu se nahaja blok
int kjeSeNahajaBlok(string blok, list<string> keywords, list<string> operatorji)
{
    for (int i = 0; i < keywords.size(); i++) 
    {
        if (blok == DobiItemIzLista(keywords, i)) return 1;
    }
    for (int i = 0; i < operatorji.size(); i++)
    {
        if (blok == DobiItemIzLista(operatorji, i)) return 2;
    }
    if (blok[0] == '\"' || aliJeStringInt(blok) || blok == "true" || blok == "false") return 3;
    return 4;

}
// preveri, če je dani blok v listu operatorjev
bool jeZnakOperator(string it, list<string> operatorji) {
    for (int i = 0; i < operatorji.size(); i++) {
        if (it == DobiItemIzLista(operatorji, i)) return true;
    }
    return false;
}
// pred in za vsakem operatorju (razen če je dvojni) doda presledek --> dani so v različne bloke
string dodajPresledekPriOperatorjih(string koda, list<string> operatorji) {
    string rezultat;
    list<string> dvodelniOperatorji = { "+","-","*", "/" ,"%","!","=","<", ">" };
    char prejsniZnak = '\0';

    for (auto it = koda.begin(); it != koda.end(); ++it) {
        string znak(1, *it);

        if (jeZnakOperator(znak, operatorji)) {
            rezultat += " ";
            rezultat += *it;
            // preveri, če je naslednji znak +, - ali =
            auto naslednjiIt = next(it, 1);
            if (naslednjiIt != koda.end()) {
                if (naslednjiIt != koda.end() && ((*naslednjiIt == '=' && JeItemVListu(dvodelniOperatorji, string(1, *it))) || (*it == '+' && *naslednjiIt == '+') || (*it == '-' && *naslednjiIt == '-')))
                {
                    rezultat += *naslednjiIt;
                    ++it;
                    rezultat += " ";
                }
                else rezultat += " ";
            }
            
        }
        else {
            rezultat += *it;
        }

        prejsniZnak = *it;
    }

    return rezultat;
}



int main()
{
    string file = "Test1.txt";



    list<string> keywords = { "cwrite", "cread", "string", "int", "bool", "while", "for", "if", "elseif", "else", "switch", "case" }; //list 1
    list<string> operatorji = { "+", "++","-","--","*","/","%","=","!","<",">","+=","-=","*=","/=","%=","==","!=","<=",">=", "{", "}", "(", ")",",","\"", ":", ".", "[","]"};//list 2

    list<Spremenljivka> vrednost = {}; // list 3
    list<Spremenljivka> spremenljivke = {}; //list 4

    int vrstaBloka;
    list<string> stringi;
    string koda;
    pridobiTekst(koda, file);
    DodajPresledekZaVrsticami(koda);
    koda = dodajPresledekPriOperatorjih(koda, operatorji);
    koda = LociPoNarekovajih(koda, stringi);
    list<string> tokens = lociPoVejicah(koda);
    list<list<string>> razclenjenaKoda = lociVrsticePoBlockih(tokens);
    razclenjenaKoda = ZdruzitevStringovVNarekovaje(razclenjenaKoda, stringi);


    // začne z izvrševanjem kode
    for (const auto& glavniStevecVrstic : razclenjenaKoda)
    {
        for (const auto& elementVBloku : glavniStevecVrstic)
        {
            vrstaBloka = kjeSeNahajaBlok(elementVBloku,keywords,operatorji);

            switch (vrstaBloka)
            {
                //keywords
            case 1:
                if (elementVBloku == "int" || elementVBloku == "bool" || elementVBloku == "string") {
                    auto it = next(glavniStevecVrstic.begin());
                    if (it != glavniStevecVrstic.end()) {
                        string varName = *it;
                        it = next(it);
                        if (it != glavniStevecVrstic.end() && *it == "=") {
                            it = next(it);
                            if (it != glavniStevecVrstic.end()) {
                                string value = *it;
                                try {
                                    declareVariable(elementVBloku, varName, value);
                                    cout << "Declared " << elementVBloku << " " << varName << " = " << value << endl;
                                }
                                catch (const exception& e) {
                                    cout << "Error: " << e.what() << endl;
                                }
                            }
                        }
                    }
                }
                break;
                // operatorji in znaki
            case 2:
                break;
                // vrednosti
            case 3:
                break;
                // spremenljivke
            case 4:
                break;
            }
        }
        cout << "---" << endl;
    }





    /*for (const auto& row : razclenjenaKoda)
    {
        for (const auto& elem : row)
        {
            cout << elem << endl;
        }
        cout << "---" << endl;
    }*/


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
