/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_SPACEMETRICS_HPP
#define NND_SPACEMETRICS_HPP

#include <cmath>
#include <cstddef>
#include <execution>
#include <functional>
#include <numeric>
#include <type_traits>
#include <utility>

#include "../Type.hpp"
#include "../SIMD.hpp"

namespace ann{


constexpr std::size_t register_count = 16;



template<typename Type, typename Reduce>
constexpr auto pattern_reduce(std::span<Type, 1> elements, Reduce&& reducer){
    return elements[0];
}

template<typename Type, typename Reduce>
constexpr auto pattern_reduce(std::span<Type, 2> elements, Reduce&& reducer){
    return std::invoke(std::forward<Reduce>(reducer), elements[0], elements[1]);
}

template<typename Type, std::size_t NumElements, typename Reduce>
constexpr auto pattern_reduce(std::span<Type, NumElements> elements, Reduce&& reducer){
    return std::invoke(std::forward<Reduce>(reducer), 
                       pattern_reduce(elements.template subspan<0, NumElements/2>(), std::forward<Reduce>(reducer)), 
                       pattern_reduce(elements.template subspan<NumElements/2, NumElements - NumElements/2>(), std::forward<Reduce>(reducer)));
}

template<typename Type, std::size_t NumElements, typename Reduce>
constexpr auto pattern_reduce(std::array<Type, NumElements>&& elements, Reduce&& reducer){
    return pattern_reduce(std::span{elements}, std::forward<Reduce>(reducer)); 
                       
}

template<typename DataType, std::size_t NumElements, typename Transform>
constexpr std::array<ann::DataVector<DataType>, NumElements> bulk_transform(auto&& iterator_lhs, auto&& iterator_rhs, Transform&& transformer){
    auto fold = [&]<std::size_t... indexes>(std::index_sequence<indexes...>){
        return std::array<ann::DataVector<DataType>, NumElements>{
            std::invoke(
                std::forward<Transform>(transformer),
                iterator_lhs[indexes],
                iterator_rhs[indexes]
            )...
        };
    };

    return fold(std::make_index_sequence<NumElements>{});
}

template<typename DataType, std::size_t NumElements, typename Transform>
constexpr auto bulk_transform(auto&& iterator, Transform&& transformer) noexcept{
    auto fold = [&]<std::size_t... indexes>(std::index_sequence<indexes...>){
        //using element_type = std::remove_reference_t<std::invoke_result_t<Transform, decltype(iterator[std::size_t{0}])>>;
        return std::array<ann::DataVector<DataType>, NumElements>{
            std::invoke(
                std::forward<Transform>(transformer),
                iterator[indexes]
            )...
        };
    };

    return fold(std::make_index_sequence<NumElements>{});
}

template<typename DataType, std::size_t NumElements>
constexpr std::array<ann::DataVector<DataType>, NumElements> bulk_load(auto&& iterator)noexcept{
    return bulk_transform<DataType, NumElements>(std::forward<decltype(iterator)>(iterator), [](auto&& element){ return ann::DataVector<float>{element};});
}

template<std::size_t NumElements, typename Transform, typename Reduce>
constexpr auto transform_reduce_step(auto&& lhs_vector, auto&& rhs_vector, Transform&& transformer, Reduce&& reducer){
    return pattern_reduce(bulk_transform<NumElements>(lhs_vector, rhs_vector, std::forward<Transform>(transformer)), std::forward<Reduce>(reducer));
}

template<typename DataType, typename Transform, typename Reduce>
auto vector_transform_reduce(ann::vector_span<const DataType> lhs_vector, ann::vector_span<const DataType> rhs_vector, Transform transformer, Reduce reducer){

    std::size_t index = 0;
    std::size_t size = lhs_vector.size();
    auto begin_lhs = lhs_vector.begin();
    auto begin_rhs = rhs_vector.begin();

    ann::DataVector<DataType> accumulator{};

    while (index + 7 < size){
        std::array<ann::DataVector<DataType>, 8> transformed = bulk_transform<DataType, 8>(begin_lhs+index, begin_rhs+index, transformer);
        accumulator = reducer(accumulator, pattern_reduce(std::span{transformed}, reducer));
        index += 8;
    }

    if (index + 3 < size){
        std::array<ann::DataVector<DataType>, 4> transformed = bulk_transform<DataType, 4>(begin_lhs+index, begin_rhs+index, transformer);
        accumulator = reducer(accumulator, pattern_reduce(std::span{transformed}, reducer));
        index += 4;
    }

    if (index + 1 < size){
        std::array<ann::DataVector<DataType>, 2> transformed = bulk_transform<DataType, 2>(begin_lhs+index, begin_rhs+index, transformer);
        accumulator = reducer(accumulator, pattern_reduce(std::span{transformed}, reducer));
        index += 2;
    }

    if (index < size){
        accumulator = reducer(accumulator, transformer(lhs_vector[index], rhs_vector[index]));
    }

    std::span excess_lhs = lhs_vector.excess();
    std::span excess_rhs = rhs_vector.excess();
    auto result = std::transform_reduce(
                                    std::begin(excess_lhs),
                                    std::end(excess_lhs),
                                    std::begin(excess_rhs),
                                    ann::simd_ops::reduce(accumulator),
                                    std::plus<>{},
                                    std::multiplies<>{});

    return result;
}

template<size_t p, typename DataEntry, typename RetValue = double>
RetValue PNorm(const DataEntry& point){
    using Extent = typename DataEntry::value_type;
    RetValue acc = std::transform_reduce(point.begin(),
                          point.end(),
                          RetValue(0),
                          std::plus<RetValue>(),
                          [](const Extent& extent)->RetValue{ return std::pow(extent, p);});

    return std::pow(acc, 1.0/p);
}

template<typename DataEntry>
void Normalize(DataEntry& entry){
    using Extent = typename DataEntry::value_type;
    Extent norm = PNorm<2, DataEntry, Extent>(entry);

    for(auto& component: entry) component /= norm;
}


template<typename VectorA, typename VectorB, typename RetType=float>
RetType Dot(const VectorA& pointA, const VectorB& pointB){
    using ExtentA = typename VectorA::value_type;
    using ExtentB = typename VectorB::value_type;
    auto transformFunc = [](ExtentA operandA, ExtentB operandB){
        return static_cast<RetType>(operandA) * static_cast<RetType>(operandB);
    };

    RetType accum = std::transform_reduce(std::execution::unseq,
                                    pointA.begin(),
                                    pointA.end(),
                                    pointB.begin(),
                                    RetType(0),
                                    std::plus<RetType>(),
                                    transformFunc);

    return accum;
};


}

#endif 