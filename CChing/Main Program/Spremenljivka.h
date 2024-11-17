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
};
class String : Spremenljivka 
{
	protected:
		string vrednost;
	public:
		void ustvariString(string ime, string vrednost) 
		{

		}
	
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
