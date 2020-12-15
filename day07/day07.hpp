#ifndef day_h
#define day_h

#include <set>
#include <string>
#include <vector>

class Bag {
   public:
    std::string color;
    std::set<std::shared_ptr<std::pair<Bag, int>>> containList =
        std::set<std::shared_ptr<std::pair<Bag, int>>>();

    Bag(std::string color);
    bool contains(std::string color);
};

class Graph {
    std::vector<std::shared_ptr<Bag>> bags =
        std::vector<std::shared_ptr<Bag>>();

    std::shared_ptr<Bag> findBag(std::string color);
    int getNumEnclosingBags(std::string color);
    bool contains(std::string color1, std::string color2);

   public:
    void addContains(std::string color1, std::string color2, int num);
    int getNumContains(std::string color);
    int getNumBagsInside(std::string color);
};

#endif /* day_h */
