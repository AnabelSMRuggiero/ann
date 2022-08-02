/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

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