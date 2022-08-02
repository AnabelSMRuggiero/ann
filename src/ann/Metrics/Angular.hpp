/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_ANGULAR_HPP
#define NND_ANGULAR_HPP

#include <cmath>
#include <cstddef>
#include <functional>
#include <numeric>
#include <execution>
#include <stdexcept>
#include <type_traits>
#include <utility>


#include "SpaceMetrics.hpp"

namespace ann{

//Assumes data is already normalized
template<typename LHSDistance, typename RHSDistance, typename RetType=std::common_type_t<LHSDistance, RHSDistance>>
RetType inner_product(ann::vector_span<const LHSDistance> point_lhs, ann::vector_span<const RHSDistance> point_rhs){

    return RetType(1.0) - std::clamp(vector_transform_reduce(point_lhs, point_rhs, std::multiplies<>{}, std::plus<>{}), RetType(-1.0), RetType(1.0));

};


template<size_t numPointsTo>
void batch_inner_product(ann::vector_span<const float> point_from,
                        std::span<const ann::vector_span<const float>, numPointsTo> points_to,
                        std::span<float, numPointsTo> resultLocation) noexcept {

    
    std::array<ann::DataVector<float>, numPointsTo> accumulators{};

    static_assert(register_count > numPointsTo+1);
    constexpr std::size_t surplus_registers = register_count-numPointsTo;
    constexpr std::size_t vector_length = surplus_registers/2;

    /* vvv core loop vvv */
    std::size_t index = 0;
    for (; index + vector_length-1 <point_from.size(); index+=vector_length){
        std::array<ann::DataVector<float>, vector_length> from_segments = bulk_load<float, vector_length>(point_from.begin() + index);
        for(size_t j = 0; j<numPointsTo; j+=1){
            std::array<ann::DataVector<float>, vector_length> elementwise_multiply = bulk_transform<float, vector_length>(from_segments, points_to[j].begin() + index, std::multiplies<>{});
            accumulators[j] += pattern_reduce(std::span{elementwise_multiply}, std::plus<>{});
        }
    }
    
    for(; index < point_from.size(); ++index){
        for(size_t j = 0; j<numPointsTo; j+=1){
            accumulators[j] += point_from[index] * points_to[j][index];
        }
    }

    /* ^^^ core loop ^^^ */



    for(size_t j = 0; j<numPointsTo; j+=1) resultLocation[j] = ann::simd_ops::reduce(accumulators[j]);

    //Take care of the excess. I might be able to remove this at some point
    ann::vector_span<const float>::excess_type excessFrom = point_from.excess();
    if (excessFrom.size() > 0){
        std::array<ann::vector_span<const float>::excess_type, numPointsTo> excessesTo;
        for(size_t j = 0; j<numPointsTo; j+=1) excessesTo[j] = points_to[j].excess();

        for (size_t i = 0; i<excessFrom.size(); i += 1){
            for (size_t j = 0; j<numPointsTo; j+=1){
                resultLocation[j] +=  excessesTo[j][i] * excessFrom[i];
            }
        }
    }

    for (std::size_t j = 0; j<numPointsTo; ++j){
        //resultLocation[j] = std::acos(std::clamp(resultLocation[j], -1.0f, 1.0f))/std::numbers::pi_v<float>;
        resultLocation[j] = 1.0f - resultLocation[j];
    }

}

/*

struct AngularMetricPair{
    using DistType = float;
    float operator()(const AlignedSpan<const float> lhsVector, const AlignedSpan<const float> rhsVector) const{
        return AngularMetric<AlignedSpan<const float>, AlignedSpan<const float>, float>(lhsVector, rhsVector);
    };
    
    std::vector<float> operator()(AlignedSpan<const float> lhsVector, const std::vector<AlignedSpan<const float>>& rhsVectors) const{
        return AngularBatcher(lhsVector, rhsVectors);
    };
};
*/
}


#endif