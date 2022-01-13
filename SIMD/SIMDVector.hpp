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

namespace ann{

template<typename Derived>
struct DataVectorBase{
    using DerivedClass = Derived;
};

template<typename DataType, InstructionSet instructions = defaultInstructionSet>
struct DataVector : DataVectorBase<DataVector>{
    using UnderlyingType = DataType;
    using VectorType = VectorIntrinsic_t<DataType, instructions>;

    static constexpr instructionSet = instructions;

    VectorType vec;
};

template<typename Op, typename... Operands>
    requires Operation<Op, sizeof...(Operands)>
struct VectorOperation : DataVectorBase<VectorOperation>{
    
    std::tuple<Operands&&...> operands;
    
    VectorOperation(Op, Operands&&... operands): operands{std::forward_as_tuple<Operands...>(operands...)} {}

    VectorOperation(Op, std::tuple<Operands&&...> operands): operands{std::move(operands)} {}

};



template <typename Func, typename Tuple>
constexpr auto FoldTuple(Func&& func, Tuple&& tuple){

    auto folder = [&](auto&&... args){
        return std::tuple{func(args)...};
    };
    
    return std::apply(folder, tuple);

};

template<typename Reducer, typename Arg1, typename Arg2, typename... Args>
constexpr auto Reduce(Reducer&& reducer, Arg1&& arg1, Arg2&& arg2, Args&&... args){
    if constexpr(sizeof...(args) == 0){
        return std::invoke(std::forward<Reducer>(reducer), std::forward<Arg1>(arg1), std::forward<Arg2>(arg2)));
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
    
    return std::apply(reduceFunc, std::tuple_cat(std::forward_as_tuple<Init>(init), tuple);

};

template<typename DataType, InstructionSet instructions>
const auto& Evaluate(const DataVector<DataType, instructions>& vec){
    return vec;
}

template<typename Op, typename... Operands>
    requires Operation<Op, sizeof...(Operands)>
auto Evaluate(const VectorOperation<Op, Operands...>& expression){

    auto recurser = [] (const auto& operand){
        return Evaluate(operand);
    };

    return std::apply(Op{}, FoldTuple(recurser, expression.operands));
}


//VectorOperation test{Multiply{}, DataVector<float, InstructionSet::avx>{}, DataVector<float, InstructionSet::avx>{}};

template<typename Operation, typename LHSDerived, typename RHSDerived>
auto MakeOperation(Operation, const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return VectorOperation{Operation{}, static_cast<const LHSDerived&>(lhsOperand), static_cast<const RHSDerived&>(rhsOperand)};

};

template<typename Derived>
auto operator+(const DataVectorBase<Derived>& operand){

    return VectorOperation{Negate{}, static_cast<const Derived&>(operand)};

}


template<typename LHSDerived, typename RHSDerived>
auto operator+(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Add{}, lhsOperand, rhsOperand);

}

template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
auto operator+(const DataVectorBase<VectorOperation<Multiply, FirstDerived, SecondDerived>>& lhsOperand, const DataVectorBase<ThirdDerived>& rhsOperand){
    using LHSDerived = VectorOperation<Multiply, FirstDerived, SecondDerived>;

    return VectorOperation{FMA{},
                           tuple_cat(static_cast<const LHSDerived&>(lhsOperand).operands,
                                     std::tuple{static_cast<const ThirdDerived&>(rhsOperand)})
                          };
}

template<typename LHSDerived, typename RHSDerived>
auto operator-(const DataVectorBase<LHSDerived>& lhsOperand, const DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Subtract{}, lhsOperand, rhsOperand);

}


template<typename FirstDerived, typename SecondDerived, typename ThirdDerived>
auto operator-(const DataVectorBase<VectorOperation<Multiply, FirstDerived, SecondDerived>>& lhsOperand, const DataVectorBase<ThirdDerived>& rhsOperand){
    using LHSDerived = VectorOperation<Multiply, FirstDerived, SecondDerived>;

    return VectorOperation{FMS{},
                           tuple_cat(static_cast<const LHSDerived&>(lhsOperand).operands,
                                     std::tuple{static_cast<const ThirdDerived&>(rhsOperand)})
                          };
}

template<typename LHSDerived, typename RHSDerived>
auto operator*(DataVectorBase<LHSDerived>& lhsOperand, DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Multiply{}, lhsOperand, rhsOperand);

}

template<typename LHSDerived, typename RHSDerived>
auto operator/(DataVectorBase<LHSDerived>& lhsOperand, DataVectorBase<RHSDerived>& rhsOperand){

    return MakeOperation(Divide{}, lhsOperand, rhsOperand);

}




}

#endif