#pragma once
#include <iostream>
#include <string>


using namespace std;
class Spremenljivka
{
	
	public:
		string ime;
		Spremenljivka() {

		}
		static string pridobiIme(Spremenljivka s1) {
			return s1.ime;
		}
};
class String : Spremenljivka 
{
	protected:
		string vrednost;
	public:
		
	
};
class Int : Spremenljivka
{
protected:
	int vrednost;
public:


};
class Bool : Spremenljivka
{
protected:
	bool vrednost;
public:


};
