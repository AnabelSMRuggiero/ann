/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_SIMDOPERATIONS_HPP
#define NND_SIMDOPERATIONS_HPP

#include <cstddef>
#include <type_traits>
#include <concepts>
#include <tuple>

#include "SIMDInstructionSet.hpp"


namespace ann{


template<size_t arity>
struct Op{
    static constexpr size_t Arity = arity;
};

struct Negate : Op<1> {};

struct Add : Op<2> {};

struct Subtract : Op<2> {};

struct Multiply : Op<2> {};

struct Divide : Op<2> {};



struct FMA : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<typename DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2, DataVector& operand3){
        
    }

};

struct FMS : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<typename DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2, DataVector& operand3){
        
    }

};

template<typename Oper, size_t arity>
concept Operation = std::is_base_of_v<Op<Oper::Arity>, Oper> && Oper::Arity == arity;

constexpr bool test = Operation<Add, 2>;

constexpr bool dontblowup = Operation<Negate, 1>;


}

#endif