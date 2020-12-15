#include "day07.hpp"

#include <fstream>
#include <iostream>

// --- Day 7: Handy Haversacks ---

// You land at the regional airport in time for your next flight. In fact, it
// looks like you'll even have time to grab some food: all flights are currently
// delayed due to issues in luggage processing.

// Due to recent aviation regulations, many rules (your puzzle input) are being
// enforced about bags and their contents; bags must be color-coded and must
// contain specific quantities of other color-coded bags. Apparently, nobody
// responsible for these regulations considered how long they would take to
// enforce!

// For example, consider the following rules:

// light red bags contain 1 bright white bag, 2 muted yellow bags.
// dark orange bags contain 3 bright white bags, 4 muted yellow bags.
// bright white bags contain 1 shiny gold bag.
// muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
// shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
// dark olive bags contain 3 faded blue bags, 4 dotted black bags.
// vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
// faded blue bags contain no other bags.
// dotted black bags contain no other bags.
// These rules specify the required contents for 9 bag types. In this example,
// every faded blue bag is empty, every vibrant plum bag contains 11 bags (5
// faded blue and 6 dotted black), and so on.

// You have a shiny gold bag. If you wanted to carry it in at least one other
// bag, how many different bag colors would be valid for the outermost bag? (In
// other words: how many colors can, eventually, contain at least one shiny gold
// bag?)

// In the above rules, the following options would be available to you:

// A bright white bag, which can hold your shiny gold bag directly.
// A muted yellow bag, which can hold your shiny gold bag directly, plus some
// other bags. A dark orange bag, which can hold bright white and muted yellow
// bags, either of which could then hold your shiny gold bag. A light red bag,
// which can hold bright white and muted yellow bags, either of which could then
// hold your shiny gold bag. So, in this example, the number of bag colors that
// can eventually contain at least one shiny gold bag is 4.

// How many bag colors can eventually contain at least one shiny gold bag? (The
// list of rules is quite long; make sure you get all of it.)

using namespace std;

Bag::Bag(string color) { this->color = color; }

bool Bag::contains(string color) {
    for (auto inner : this->containList) {
        if (inner->first.color == color) {
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

void Graph::addContains(string color1, string color2, int num) {
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

    auto item = shared_ptr<pair<Bag, int>>(new pair<Bag, int>(*inner, num));
    outer->containList.insert(item);
}

bool Graph::contains(string color1, string color2) {
    shared_ptr<Bag> outer = this->findBag(color1);

    if (!outer) {
        return false;
    }

    if (outer->contains(color2)) {
        return true;
    }

    for (auto inner : outer->containList) {
        if (this->contains(inner->first.color, color2)) {
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

// --- Part Two ---

// It's getting pretty expensive to fly these days - not because of ticket
// prices, but because of the ridiculous number of bags you need to buy!

// Consider again your shiny gold bag and the rules from the above example:

// faded blue bags contain 0 other bags.
// dotted black bags contain 0 other bags.
// vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black
// bags. dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted
// black bags. So, a single shiny gold bag must contain 1 dark olive bag (and
// the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each
// of those): 1 + 1*7 + 2 + 2*11 = 32 bags!

// Of course, the actual rules have a small chance of going several levels
// deeper than this example; be sure to count all of the bags, even if the
// nesting becomes topologically impractical!

// Here's another example:

// shiny gold bags contain 2 dark red bags.
// dark red bags contain 2 dark orange bags.
// dark orange bags contain 2 dark yellow bags.
// dark yellow bags contain 2 dark green bags.
// dark green bags contain 2 dark blue bags.
// dark blue bags contain 2 dark violet bags.
// dark violet bags contain no other bags.
// In this example, a single shiny gold bag must contain 126 other bags.

// How many individual bags are required inside your single shiny gold bag?

int Graph::getNumEnclosingBags(string color) {
    shared_ptr<Bag> bag = this->findBag(color);
    int numContains = 1;

    for (auto inner : bag->containList) {
        Bag innerBag = inner->first;
        int num = inner->second;

        numContains += num * this->getNumEnclosingBags(innerBag.color);
    }

    return numContains;
}

int Graph::getNumBagsInside(string color) {
    int enclosingBags = this->getNumEnclosingBags(color);
    return enclosingBags - 1;  // Subtract the outer bag itself
}

int main(int argc, const char* argv[]) {
    Graph graph = Graph();

    string line;
    ifstream input("processed.txt");

    string* primary = nullptr;
    int num = 0;
    if (input.is_open()) {
        while (getline(input, line)) {
            if (!primary) {
                // First color in a sequence: outer bag
                primary = new string(line);
            } else if (line == "") {
                // Group separator: clear out buffer
                delete primary;
                primary = nullptr;
            } else {
                // Subsequent line in a sequence: inner bag or number
                try {
                    // Number input line
                    num = stoi(line);
                } catch (invalid_argument) {
                    // Color input line
                    graph.addContains(*primary, line, num);
                }
            }
        }
        input.close();
    }

    cout << "Part 1: " << graph.getNumContains("shiny gold") << endl;
    cout << "Part 2: " << graph.getNumBagsInside("shiny gold") << endl;
}
