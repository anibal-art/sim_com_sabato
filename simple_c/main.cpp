#include <iostream>
#include "ziggurat.hpp"

using namespace std;

int main()
{
     // Inicializar semilla y tablas
        r4_set_seed(123456789u);
        r4_nor_setup();
        r4_exp_setup();

        std::cout << "10000 uniformes U(0,1):\n";
        for (int i = 0; i < 10000; ++i)
            std::cout << UNI() << "\n";

        std::cout << "\n5 normales N(0,1):\n";
        for (int i = 0; i < 5; ++i)
            std::cout << r4_nor() << "\n";

        std::cout << "\n5 exponenciales Exp(1):\n";
        for (int i = 0; i < 5; ++i)
            std::cout << r4_exp() << "\n";

        return 0;
}
