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
#include <utility>
#include <functional>

#include <immintrin.h>

namespace ann{

template <typename Func, typename Tuple>
constexpr auto FoldTuple(Func&& func, Tuple&& tuple){

    auto folder = [&]<typename... Args>(Args&&... args){
        return std::tuple{func(std::forward<Args>(args))...};
    };
    
    return std::apply(folder, std::forward<Tuple>(tuple));

};

template<typename Reducer, typename Arg1, typename Arg2, typename... Args>
constexpr auto Reduce(Reducer&& reducer, Arg1&& arg1, Arg2&& arg2, Args&&... args){
    if constexpr(sizeof...(args) == 0){
        return std::invoke(std::forward<Reducer>(reducer), std::forward<Arg1>(arg1), std::forward<Arg2>(arg2));
    } else {
        return Reduce(std::forward<Reducer>(reducer), 
                      std::invoke(std::forward<Reducer>(reducer), std::forward<Arg1>(arg1), std::forward<Arg2>(arg2)),
                      std::forward<Args>(args)...);
    }
}

constexpr auto testReduce = Reduce(std::plus<>{}, 1, 3.1415, 'a');

//This needs work- constrain so only Tuple is tuple-like?
template <typename Init, typename Reducer, typename Tuple>
constexpr auto ReduceTuple(Init&& init, Reducer&& reducer, Tuple&& tuple){

    if constexpr (std::tuple_size_v<Tuple> == 0) return init;

    auto reduceFunc = [&]<typename...Args>(Args&&... args){
        return Reduce(std::forward<Reducer>(reducer), std::forward<Args>(args)...);
    };
    
    return std::apply(reduceFunc, std::tuple_cat(std::forward_as_tuple<Init>(init), tuple));

};

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

template<>
struct VectorIntrinsic<float, InstructionSet::avx2>{
    using type = __m256;

};

template<>
struct VectorIntrinsic<float, InstructionSet::avx>{
    using type = __m256;

};

constexpr InstructionSet defaultInstructionSet = DetectInstructionSet();

template<typename DataType, InstructionSet instructions = defaultInstructionSet>
using VectorIntrinsic_t = typename VectorIntrinsic<std::remove_const_t<DataType>, instructions>::type;

template<typename DataType>
using DefaultVector_t = typename VectorIntrinsic<std::remove_const_t<DataType>, defaultInstructionSet>::type;

template<InstructionSet set>
constexpr size_t vectorWidth = 0;

template<>
inline constexpr size_t vectorWidth<InstructionSet::avx512> = 64;

template<>
inline constexpr size_t vectorWidth<InstructionSet::fma> = 32;

template<>
inline constexpr size_t vectorWidth<InstructionSet::avx2> = 32;

template<>
inline constexpr size_t vectorWidth<InstructionSet::avx> = 32;

}

#endif