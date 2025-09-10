#ifndef ZIGGURAT_H
#define ZIGGURAT_H

#pragma once
#include <cstdint>
#include <cmath>
#include <cstdlib>

// ===== Estado global y RNG SHR3 =====
static uint32_t jsr = 123456789u;  // semilla por defecto

inline void r4_set_seed(uint32_t s) { jsr = (s ? s : 123456789u); }

inline uint32_t SHR3() {
    jsr ^= (jsr << 13);
    jsr ^= (jsr >> 17);
    jsr ^= (jsr << 5);
    return jsr;
}

inline double UNI() {
    return 0.5 + static_cast<int32_t>(SHR3()) * 2.3283064365386963e-10;
}

// ===== Ziggurat para Normal =====
static uint32_t kn[128];
static double   wn[128], fn[128];
static double   r4nor_r = 3.442619855899;   // borde del escalón 0
static constexpr double m1 = 2147483648.0;  // 2^31

inline void r4_nor_setup() {
    const double v = 9.91256303526217e-3;
    double q = v / std::exp(-0.5 * r4nor_r * r4nor_r);

    kn[0]   = static_cast<uint32_t>( (r4nor_r / q) * m1 );
    kn[1]   = 0;

    wn[0]   = q / m1;
    wn[127] = r4nor_r / m1;

    fn[0]   = 1.0;
    fn[127] = std::exp(-0.5 * r4nor_r * r4nor_r);

    double x = r4nor_r;
    double y = 0.0;

    for (int i = 126; i >= 1; --i) {
        y = std::sqrt( -2.0 * std::log( v / x + std::exp(-0.5 * x * x) ) );
        kn[i+1] = static_cast<uint32_t>( (y / x) * m1 );
        wn[i]   = y / m1;
        fn[i]   = std::exp(-0.5 * y * y);
        x = y;
    }
}

inline double r4_nor() {
    for (;;) {
        int32_t hz = static_cast<int32_t>(SHR3());
        uint32_t iz = static_cast<uint32_t>(hz) & 127u;

        if (std::abs(hz) < static_cast<int32_t>(kn[iz]))
            return hz * wn[iz];
        else {
            if (iz == 0u) {
                double x, y;
                do {
                    x = -std::log(UNI()) * (1.0 / r4nor_r);
                    y = -std::log(UNI());
                } while (y + y < x * x);
                return (hz > 0) ? r4nor_r + x : -r4nor_r - x;
            } else {
                double x = hz * wn[iz];
                if (fn[iz] + UNI() * (fn[iz-1] - fn[iz]) < std::exp(-0.5 * x * x))
                    return x;
            }
        }
    }
}

// ===== Ziggurat para Exponencial =====
static uint32_t ke[256];
static double   we[256], fe[256];

inline void r4_exp_setup() {
    const double m2 = 4294967296.0; // 2^32
    const double de = 7.69711747013104972; // límite derecho
    const double ve = 3.9496598225815571993e-3; // área de cada escalón
    double q = ve / std::exp(-de);

    ke[0]   = static_cast<uint32_t>( (de / q) * m2 );
    ke[1]   = 0;

    we[0]   = q / m2;
    we[255] = de / m2;

    fe[0]   = 1.0;
    fe[255] = std::exp(-de);

    double x = de;
    double y = 0.0;

    for (int i = 254; i >= 1; --i) {
        y = -std::log( ve / x + std::exp(-x) );
        ke[i+1] = static_cast<uint32_t>( (y / x) * m2 );
        we[i]   = y / m2;
        fe[i]   = std::exp(-y);
        x = y;
    }
}

inline double r4_exp() {
    for (;;) {
        uint32_t jz = SHR3();
        uint32_t iz = jz & 255u;

        if (jz < ke[iz])
            return jz * we[iz];
        else {
            if (iz == 0u) {
                double x;
                do { x = -std::log(UNI()) * 7.69711747013104972; }
                while ( -std::log(UNI()) < x );
                return 7.69711747013104972 + x;
            } else {
                double x = jz * we[iz];
                if (fe[iz] + UNI() * (fe[iz-1] - fe[iz]) < std::exp(-x))
                    return x;
            }
        }
    }
}

#endif // ZIGGURAT_H
