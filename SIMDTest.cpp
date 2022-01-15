#include "Type.hpp"
#include "SIMD/SIMDVector.hpp"
#include "SIMD/VectorSpan.hpp"

#include <array>
#include <iostream>
#include <ranges>

int main(){

    nnd::DynamicArray<float, 32> test2(21);
    nnd::DynamicArray<float, 32> test3(21);

    std::ranges::fill(test2, float{ 2.0 });
    std::ranges::fill(test3, float{ 3.0 });

    ann::vector_span<float> span2{test2};
    ann::vector_span<float> span3{test3};

    ann::DataVector<float> accumulator{};

    for (size_t i = 0; i<span2.size(); i+=1){
        accumulator = span2[i]*span3[i] + accumulator;
    }

    float total = ann::simd_ops::reduce(accumulator);

    auto excess2 = span2.excess();
    auto excess3 = span3.excess();

    for (size_t i = 0; i<excess2.size(); i+=1){
        total = excess2[i]*excess3[i] + total;
    }
    
    std::cout << total << std::endl;

    return 0;
}