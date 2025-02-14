#pragma once
#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>
#include <stdexcept>

using namespace std;


class Spremenljivka {
protected:
    string ime;

public:
    Spremenljivka(string name) : ime(name) {}
    virtual ~Spremenljivka() = default;
    string getIme() { return ime; }
    virtual string getValueAsString() = 0;
};


class String : public Spremenljivka {
private:
    string vrednost;
public:
    String(string name, string value) : Spremenljivka(name), vrednost(value) {}
    string getValueAsString() override { return vrednost; }
    void setVrednost(string value) { vrednost = value; }
};


class Int : public Spremenljivka {
private:
    int vrednost;
public:
    Int(string name, int value) : Spremenljivka(name), vrednost(value) {}
    string getValueAsString() override { return to_string(vrednost); }
    void setVrednost(int value) { vrednost = value; }
    int getVrednost() { return vrednost; }
};

class Bool : public Spremenljivka {
private:
    bool vrednost;
public:
    Bool(string name, bool value) : Spremenljivka(name), vrednost(value) {}
    string getValueAsString() override { return vrednost ? "true" : "false"; }
    void setVrednost(bool value) { vrednost = value; }
};


unordered_map<string, Spremenljivka*> variableStore;


void declareVariable(string type, string name, string value) {
    if (variableStore.find(name) != variableStore.end()) {
        throw runtime_error("Variable " + name + " already declared.");
    }
    if (type == "string") {
        variableStore[name] = new String(name, value);
    }
    else if (type == "int") {
        variableStore[name] = new Int(name, stoi(value));
    }
    else if (type == "bool") {
        variableStore[name] = new Bool(name, value == "true");
    }
    else {
        throw runtime_error("Unknown type " + type);
    }
}


string getVariableValue(string name) {
    if (variableStore.find(name) == variableStore.end()) {
        throw runtime_error("Variable " + name + " not found.");
    }
    return variableStore[name]->getValueAsString();
}

void assignVariable(string name, string value) {
    if (variableStore.find(name) == variableStore.end()) {
        throw runtime_error("Variable " + name + " not found.");
    }
    if (dynamic_cast<String*>(variableStore[name])) {
        dynamic_cast<String*>(variableStore[name])->setVrednost(value);
    }
    else if (dynamic_cast<Int*>(variableStore[name])) {
        dynamic_cast<Int*>(variableStore[name])->setVrednost(stoi(value));
    }
    else if (dynamic_cast<Bool*>(variableStore[name])) {
        dynamic_cast<Bool*>(variableStore[name])->setVrednost(value == "true");
    }
    else {
        throw runtime_error("Invalid assignment.");
    }
}
