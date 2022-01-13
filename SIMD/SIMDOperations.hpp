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

//Needs special handling
struct Negate : Op<1> {};

struct Add : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2){
        return {_mm256_add_ps(operand1.vec, operand2.vec)};
    }
};

struct Subtract : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2){
        return {_mm256_sub_ps(operand1.vec, operand2.vec)};
    }
};

struct Multiply : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2){
        return {_mm256_mul_ps(operand1.vec, operand2.vec)};
    }
};

struct Divide : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2){
        return {_mm256_div_ps(operand1.vec, operand2.vec)};
    }
};

template<typename DataVector, typename UnderlyingType, typename Intrinsic>
concept VectorRequirements = std::same_as<UnderlyingType, typename DataVector::UnderlyingType> && std::same_as<Intrinsic, typename DataVector::VectorType>;

struct FMA : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2, DataVector& operand3){
        return DataVector{_mm256_fmadd_ps(operand1.vec, operand2.vec, operand3.vec)};
    }

};

struct FMS : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<__m256, float> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2, DataVector& operand3){
        return DataVector{_mm256_fmsub_ps(operand1.vec, operand2.vec, operand3.vec)};
    }

};

template<typename Oper, size_t arity>
concept Operation = std::is_base_of_v<Op<Oper::Arity>, Oper> && Oper::Arity == arity;


}

#endif