#include "day07.hpp"

#include <fstream>
#include <iostream>

using namespace std;

Bag::Bag(string color) { this->color = color; }

bool Bag::contains(string color) {
    for (shared_ptr<Bag> bag : this->containList) {
        if (bag->color == color) {
            return true;
        }
    }
    return false;
}

shared_ptr<Bag> Graph::findBag(string color) {
    for (shared_ptr<Bag> storedBag : this->bags) {
        if (storedBag->color == color) {
            return storedBag;
        }
    }
    return nullptr;
}

void Graph::addContains(string color1, string color2) {
    shared_ptr<Bag> outer = this->findBag(color1);
    shared_ptr<Bag> inner = this->findBag(color2);

    if (!outer) {
        outer = shared_ptr<Bag>(new Bag(color1));
        this->bags.push_back(outer);
    }

    if (!inner) {
        inner = shared_ptr<Bag>(new Bag(color2));
        this->bags.push_back(inner);
    }

    outer->containList.insert(inner);
}

bool Graph::contains(string color1, string color2) {
    shared_ptr<Bag> outer = this->findBag(color1);

    if (!outer) {
        return false;
    }

    if (outer->contains(color2)) {
        return true;
    }

    for (shared_ptr<Bag> inner : outer->containList) {
        if (this->contains(inner->color, color2)) {
            return true;
        }
    }

    return false;
}

int Graph::getNumContains(string color) {
    int numContaining = 0;

    for (shared_ptr<Bag> bag : this->bags) {
        if (this->contains(bag->color, color)) {
            numContaining++;
        }
    }
    return numContaining;
}

int main(int argc, const char* argv[]) {
    Graph graph = Graph();

    string line;
    ifstream input("processed.txt");

    string* primary = nullptr;
    if (input.is_open()) {
        while (getline(input, line)) {
            if (!primary) {
                primary = new string(line);
            } else if (line == "") {
                delete primary;
                primary = nullptr;
            } else {
                graph.addContains(*primary, line);
            }
        }
        input.close();
    }

    cout << "Part 1: " << graph.getNumContains("shiny gold") << endl;
}
