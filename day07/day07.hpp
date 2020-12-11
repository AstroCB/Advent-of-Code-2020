#ifndef day_h
#define day_h

#include <set>
#include <string>
#include <vector>

class Bag {
   public:
    std::string color;
    std::set<std::shared_ptr<Bag> > containList =
        std::set<std::shared_ptr<Bag> >();

    Bag(std::string color);
    bool contains(std::string color);
};

class Graph {
    std::shared_ptr<Bag> findBag(std::string color);

   public:
    std::vector<std::shared_ptr<Bag> > bags =
        std::vector<std::shared_ptr<Bag> >();
    void addContains(std::string color1, std::string color2);
    bool contains(std::string color1, std::string color2);
    int getNumContains(std::string color);
};

#endif /* day_h */
