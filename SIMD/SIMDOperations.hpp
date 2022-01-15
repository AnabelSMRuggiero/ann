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

#include <immintrin.h>

#include "SIMDInstructionSet.hpp"


namespace ann{

template<typename DataType, InstructionSet instructions = defaultInstructionSet>
struct DataVector;

template<typename DataVector, typename UnderlyingType, typename Intrinsic>
concept VectorRequirements = std::same_as<UnderlyingType, typename DataVector::UnderlyingType> && std::same_as<Intrinsic, typename DataVector::VectorType>;


template<size_t arity>
struct Op{
    static constexpr size_t Arity = arity;
};

struct Zero : Op<0> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> RequirementsTag>
    auto operator()(RequirementsTag) const {
        return DataVector<typename RequirementsTag::UnderlyingType, RequirementsTag::instructionSet>{_mm256_setzero_ps()};
    }
};

//Needs special handling
struct Negate : Op<1> {};

struct Load : Op<1> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> VectorRef>
    DataVector<float, VectorRef::instructionSet> operator()(VectorRef& operand1) const {
        return {_mm256_loadu_ps(operand1.dataPtr)};
    }

    template<VectorRequirements<float, __m256> VectorRef>
        requires (VectorRef::alignment >= 32)
    DataVector<float, VectorRef::instructionSet> operator()(VectorRef& operand1) const {
        return {_mm256_load_ps(operand1.dataPtr)};
    }
};

struct VectorReduce : Op<1> {

    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    float operator()(DataVector& operand1) const {
        __m256 temp = _mm256_hadd_ps(operand1.vec, _mm256_setzero_ps());
        temp = _mm256_hadd_ps(temp, _mm256_setzero_ps());
        auto constexprBranchElision = []<typename Vec>(Vec vec)->auto{
            if constexpr (std::is_union_v<Vec>){
                return vec.m256_f32[0] + vec.m256_f32[4];
            } else{
                return vec[0] + vec[4];
            }
        };
        return constexprBranchElision(temp);
    }

};

struct Add : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2) const {
        return DataVector{_mm256_add_ps(operand1.vec, operand2.vec)};
    }
};

struct Subtract : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2) const {
        return DataVector{_mm256_sub_ps(operand1.vec, operand2.vec)};
    }
};

struct Multiply : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2) const {
        return DataVector{_mm256_mul_ps(operand1.vec, operand2.vec)};
    }
};

struct Divide : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2) const {
        return DataVector{_mm256_div_ps(operand1.vec, operand2.vec)};
    }
};

struct Store : Op<2> {
    static constexpr std::tuple validInstructionSets {
        InstructionSet::avx,
        InstructionSet::avx2,
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> VectorRef, VectorRequirements<float, __m256> DataVector>
    void operator()(VectorRef operand1, const DataVector& operand2) const {
        return _mm256_storeu_ps(operand1.dataPtr, operand2.vec);
    }

    template<VectorRequirements<float, __m256> VectorRef,  VectorRequirements<float, __m256> DataVector>
        requires (VectorRef::alignment >= 32)
    void operator()(VectorRef operand1, const DataVector& operand2) const {
        return _mm256_store_ps(operand1.dataPtr, operand2.vec);
    }
};

struct FMA : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(const DataVector& operand1, const DataVector& operand2, const DataVector& operand3) const {
        return DataVector{_mm256_fmadd_ps(operand1.vec, operand2.vec, operand3.vec)};
    }

};

struct FMS : Op<3>{
    static constexpr std::tuple validInstructionSets {
        InstructionSet::fma,
        InstructionSet::avx512
    };

    template<VectorRequirements<float, __m256> DataVector>
    DataVector operator()(DataVector& operand1, DataVector& operand2, DataVector& operand3) const {
        return DataVector{_mm256_fmsub_ps(operand1.vec, operand2.vec, operand3.vec)};
    }

};

template<typename Oper, size_t arity>
concept Operation = std::is_base_of_v<Op<Oper::Arity>, Oper> && Oper::Arity == arity;


namespace simd_ops{
    inline static constexpr VectorReduce reduce{};
}

}

#endif