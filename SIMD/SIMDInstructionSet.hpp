/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_SIMDINSTRUCTIONSET_HPP
#define NND_SIMDINSTRUCTIONSET_HPP

#include <cstddef>
#include <concepts>

#include <immintrin.h>

namespace ann{

enum struct InstructionSet{
    //X64
    avx,
    avx2,
    fma,
    avx512,
    //ARM... maybe at some point.
    neon,
    //Catch all
    unknown
};

consteval InstructionSet DetectInstructionSet(){
    #ifdef __AVX512__
        return InstructionSet::avx512;
    #elif defined __FMA__
        return InstructionSet::fma;
    #elif defined __AVX2__
        return InstructionSet::avx2;
    #elif defined __AVX__
        return InstructionSet::avx;
    #else
        return InstructionSet::unknown;
    #endif
}

template<typename Option, typename... Options>
concept OneOf = (std::same_as<Option, Options> || ...);



template<typename DataType, InstructionSet instructions>
struct VectorIntrinsic;

template<>
struct VectorIntrinsic<float, InstructionSet::fma>{
    using type = __m256;

};


constexpr InstructionSet defaultInstructionSet = DetectInstructionSet();

template<typename DataType, InstructionSet instructions = defaultInstructionSet>
using VectorIntrinsic_t = typename VectorIntrinsic<DataType, instructions>::type;

template<typename DataType>
using DefaultVector_t = typename VectorIntrinsic<DataType, defaultInstructionSet>::type;

template<InstructionSet set>
constexpr size_t vectorWidth = 0;

template<>
constexpr size_t vectorWidth<InstructionSet::avx512> = 64;

template<>
constexpr size_t vectorWidth<InstructionSet::fma> = 32;

template<>
constexpr size_t vectorWidth<InstructionSet::avx2> = 32;

template<>
constexpr size_t vectorWidth<InstructionSet::avx> = 32;

}

#endif