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

#include "SIMDInstructionSet.hpp"
#include "SIMDOperations.hpp"
#include <concepts>

namespace ann{

template<typename Derived>
struct DataVectorBase{
    using DerivedClass = Derived;
};

template<typename DataType, InstructionSet instructions>
struct DataVector : DataVectorBase<DataVector>{
    using UnderlyingType = DataType;

};

template<typename Op, typename... Operands>
    requires Operation<Op, sizeof...(Operands)>
struct VectorOperation : DataVectorBase<VectorOperation>{
    
    std::tuple<Operands&&...> operands;
    
    VectorOperation(Op, Operands&&... operands): operands{std::forward_as_tuple<Operands...>(operands...)} {}

    VectorOperation(Op, std::tuple<Operands&&...> operands): operands{std::move(operands)} {}
};


VectorOperation test{Multiply{}, DataVector<float, InstructionSet::avx>{}, DataVector<float, InstructionSet::avx>{}};


template<typename LHSDerived, typename RHSDerived>
auto operator+(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){
    //return BinaryVectorOperation<LHSDerived, RHSDerived, BinaryOperation::add>{static_cast<LHSDerived&>(lhsOperand), static_cast<RHSDerived&>(rhsOperand)};
}

template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
auto operator+(const DataVectorBase<VectorOperation<Multiply, FirstDerived, SecondDerived>>& lhsOperand, const DataVectorBase<ThirdDerived>& rhsOperand){
    //static_cast<BinaryVectorOperation<FirstDerived, SecondDerived, BinaryOperation::multiply>&>(lhsOperand).lhsOperandRef;
    //static_cast<BinaryVectorOperation<FirstDerived, SecondDerived, BinaryOperation::multiply>&>(lhsOperand).rhsOperandRef;
    //return TernaryVectorOperation<FirstDerived, SecondDerived, ThirdDerived, TernaryOperation::fma>{static_cast<&>(lhsOperand), static_cast<ThirdDerived&>(rhsOperand)};
    return VectorOperation{FMA{}, tuple_cat(static_cast<VectorOperation<Multiply, FirstDerived, SecondDerived>&>(lhsOperand).operands, {static_cast<ThirdDerived&>(rhsOperand)})};
}

template<typename LHSDerived, typename RHSDerived>
auto operator*(DataVectorBase<LHSDerived>& lhsOperand, DataVectorBase<RHSDerived>& rhsOperand){
    //return BinaryVectorOperation<LHSDerived, RHSDerived, BinaryOperation::multiply>{static_cast<LHSDerived&>(lhsOperand), static_cast<RHSDerived&>(rhsOperand)};
}




}

#endif