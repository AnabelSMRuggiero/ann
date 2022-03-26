/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_SIMDVECTOR_HPP
#define NND_SIMDVECTOR_HPP

#include <concepts>
#include <tuple>
#include <functional>
#include <utility>

#include "SIMDInstructionSet.hpp"
#include "SIMDOperations.hpp"
#include "../Type.hpp"

namespace ann{

template<typename Derived>
struct DataVectorBase{
    using DerivedClass = Derived;
};

template<typename DataType, InstructionSet instructions>
struct RequirementsTag{
    using UnderlyingType = DataType;
    using VectorType = VectorIntrinsic_t<DataType, instructions>;
    static constexpr InstructionSet instructionSet = instructions;
};

template<typename DataType, InstructionSet instructions>
struct DataVector : DataVectorBase<DataVector<DataType, instructions>>{
    using UnderlyingType = DataType;
    using VectorType = VectorIntrinsic_t<DataType, instructions>;
    using Requirements = RequirementsTag<DataType, instructions>;
    
    static constexpr Requirements requireTag{};
    static constexpr InstructionSet instructionSet = instructions;

    DataVector(): DataVector(Zero{}(requireTag)) {};

    constexpr DataVector(const DataVector&) = default;

    constexpr DataVector(const VectorType& vec): vec{vec} {};

    template<typename OtherVector>
        requires (!std::same_as<OtherVector, DataVector>)
    constexpr DataVector(const DataVectorBase<OtherVector>& other): vec{Evaluate(static_cast<const OtherVector&>(other)).vec} {}

    template<typename OtherVector>
    constexpr DataVector& operator=(const DataVectorBase<OtherVector>& expression){
        vec = Evaluate(static_cast<const OtherVector&>(expression)).vec;
        return *this;
    }


    VectorType vec;
};

template<typename DataType, InstructionSet instructions = defaultInstructionSet, size_t align = alignof(DataType)>
struct VectorReference : DataVectorBase<VectorReference<DataType, instructions, align>>{
    using UnderlyingType = DataType;
    using VectorType = VectorIntrinsic_t<DataType, instructions>;

    static constexpr size_t alignment = align;
    static constexpr InstructionSet instructionSet = instructions;

    VectorReference() = default;

    constexpr VectorReference(DataType* dataPtr): dataPtr{dataPtr} {}

    constexpr VectorReference(nnd::AlignedPtr<DataType, alignment> alignedPtr): dataPtr{static_cast<DataType*>(alignedPtr)} {}

    VectorReference(const VectorReference&) = default;

    
    template<typename OtherVector>
        requires (!std::is_const_v<DataType>)
    VectorReference& operator=(const DataVectorBase<OtherVector>& expression){
        simd_ops::store(dataPtr, Evaluate(static_cast<const OtherVector&>(expression)).vec);
    }

    DataType* dataPtr;
};

template<typename Op, typename... Operands>
    requires Operation<Op, sizeof...(Operands)>
struct VectorOperation : DataVectorBase<VectorOperation<Op, Operands...>>{
    
    std::tuple<const Operands&...> operands;
    
    constexpr VectorOperation(Op, const Operands&... operands): operands{operands...} {}

    constexpr VectorOperation(Op, std::tuple<const Operands&...> operands): operands{std::move(operands)} {}

};





template<typename DataType, InstructionSet instructions>
[[gnu::flatten]] constexpr const auto& Evaluate(const DataVector<DataType, instructions>& vec){
    return vec;
}

template<typename DataType, InstructionSet instructions, size_t align>
[[gnu::flatten]] constexpr auto Evaluate(const VectorReference<DataType, instructions, align>& vec){
    return simd_ops::load(vec);
}

template<typename Op, typename... Operands>
    requires Operation<Op, sizeof...(Operands)>
[[gnu::flatten]] constexpr auto Evaluate(const VectorOperation<Op, Operands...>& expression){

    auto recurser = [] (const auto& operand){
        return Evaluate(operand);
    };

    return std::apply(Op{}, FoldTuple(recurser, expression.operands));
}


//VectorOperation test{Multiply{}, DataVector<float, InstructionSet::avx>{}, DataVector<float, InstructionSet::avx>{}};

template<typename Operation, typename LHSDerived, typename RHSDerived>
constexpr auto MakeOperation(Operation, const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return VectorOperation{Operation{}, static_cast<const LHSDerived&>(lhsOperand), static_cast<const RHSDerived&>(rhsOperand)};

};

template<typename Derived>
constexpr auto operator-(const DataVectorBase<Derived>& operand){

    return VectorOperation{Negate{}, static_cast<const Derived&>(operand)};

}


template<typename LHSDerived, typename RHSDerived>
constexpr auto operator+(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Add{}, lhsOperand, rhsOperand);

}

template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
constexpr auto operator+(const DataVectorBase<VectorOperation<Multiply, FirstDerived, SecondDerived>>& lhsOperand, const DataVectorBase<ThirdDerived>& rhsOperand){
    using LHSDerived = VectorOperation<Multiply, FirstDerived, SecondDerived>;

    return VectorOperation{FMA{},
                           tuple_cat(static_cast<const LHSDerived&>(lhsOperand).operands,
                                     std::tuple<const ThirdDerived&>{static_cast<const ThirdDerived&>(rhsOperand)})
                          };
}

// Clang doesn't try reversing operands to see if the fma contraction version is more specialized
template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
constexpr auto operator+(const DataVectorBase<FirstDerived>& lhsOperand, const DataVectorBase<VectorOperation<Multiply, SecondDerived, ThirdDerived>>& rhsOperand){
    using RHSDerived = VectorOperation<Multiply, SecondDerived, ThirdDerived>;

    return VectorOperation{FMA{},
                           tuple_cat(static_cast<const RHSDerived&>(rhsOperand).operands,
                                     std::tuple<const ThirdDerived&>{static_cast<const FirstDerived&>(lhsOperand)})
                          };
}

template<typename LHSDerived, typename RHSDerived>
constexpr auto operator-(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Subtract{}, lhsOperand, rhsOperand);

}


template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
constexpr auto operator-(const DataVectorBase<VectorOperation<Multiply, FirstDerived, SecondDerived>>& lhsOperand, const DataVectorBase<ThirdDerived>& rhsOperand){
    using LHSDerived = VectorOperation<Multiply, FirstDerived, SecondDerived>;

    return VectorOperation{FMS{},
                           tuple_cat(static_cast<const LHSDerived&>(lhsOperand).operands,
                                     std::tuple{static_cast<const ThirdDerived&>(rhsOperand)})
                          };
}

// Clang doesn't try reversing operands to see if the fms contraction version is more specialized
template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
constexpr auto operator-(const DataVectorBase<FirstDerived>& lhsOperand, const DataVectorBase<VectorOperation<Multiply, SecondDerived, ThirdDerived>>& rhsOperand){
    using RHSDerived = VectorOperation<Multiply, SecondDerived, ThirdDerived>;

    return VectorOperation{FNMA{},
                           tuple_cat(static_cast<const RHSDerived&>(rhsOperand).operands,
                                     std::tuple<const ThirdDerived&>{static_cast<const FirstDerived&>(lhsOperand)})
                          };
}

template<typename LHSDerived, typename RHSDerived>
constexpr auto operator*(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Multiply{}, lhsOperand, rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
constexpr auto operator/(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Divide{}, lhsOperand, rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
[[gnu::flatten]] constexpr LHSDerived& operator+=(DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return static_cast<LHSDerived&>(lhsOperand) = static_cast<const LHSDerived&>(lhsOperand) + static_cast<const RHSDerived&>(rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
[[gnu::flatten]] constexpr LHSDerived& operator-=(DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return static_cast<LHSDerived&>(lhsOperand) = static_cast<const LHSDerived&>(lhsOperand) - static_cast<const RHSDerived&>(rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
[[gnu::flatten]] constexpr LHSDerived& operator*=(DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return static_cast<LHSDerived&>(lhsOperand) = static_cast<const LHSDerived&>(lhsOperand) * static_cast<const RHSDerived&>(rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
[[gnu::flatten]] constexpr LHSDerived& operator/=(DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return static_cast<LHSDerived&>(lhsOperand) = static_cast<const LHSDerived&>(lhsOperand) / static_cast<const RHSDerived&>(rhsOperand);

}




}

#endif