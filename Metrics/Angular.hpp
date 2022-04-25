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

#include <numeric>
#include <execution>
#include <type_traits>

#include "../Type.hpp"
#include "../SIMD.hpp"

namespace ann{

//Assumes data is already normalized
template<typename VectorA, typename VectorB, typename RetType=double>
RetType inner_product(const VectorA& pointA, const VectorB& pointB){
    using ExtentA = typename VectorA::value_type;
    using ExtentB = typename VectorB::value_type;

    auto transformFunc = [](ExtentA operandA, ExtentB operandB){
        return static_cast<RetType>(operandA) * static_cast<RetType>(operandB); 
    };
    RetType accum = std::transform_reduce(std::execution::unseq,
                                    std::begin(pointA),
                                    std::end(pointA),
                                    std::begin(pointB),
                                    RetType(0),
                                    std::plus<RetType>(),
                                    transformFunc);
    return static_cast<RetType>(1.0) - accum;
};

template<size_t numPointsTo>
void batch_inner_product(const ann::vector_span<const float> pointFrom,
                        std::span<const ann::vector_span<const float>, numPointsTo> pointsTo,
                        std::span<float, numPointsTo> resultLocation) noexcept {

    
    std::array<ann::DataVector<float>, numPointsTo> accumulators;



    /* vvv core loop vvv */

    for (size_t i = 0; i<pointFrom.size(); i+=1){
        for(size_t j = 0; j<numPointsTo; j+=1){
            accumulators[j] += pointsTo[j][i] - pointFrom[i];
        }
    }

    /* ^^^ core loop ^^^ */



    for(size_t j = 0; j<numPointsTo; j+=1) resultLocation[j] = ann::simd_ops::reduce(accumulators[j]);

    //Take care of the excess. I might be able to remove this at some point
    ann::vector_span<const float>::excess_type excessFrom = pointFrom.excess();
    if (excessFrom.size() > 0){
        std::array<ann::vector_span<const float>::excess_type, numPointsTo> excessesTo;
        for(size_t j = 0; j<numPointsTo; j+=1) excessesTo[j] = pointsTo[j].excess();

        for (size_t i = 0; i<excessFrom.size(); i += 1){
            for (size_t j = 0; j<numPointsTo; j+=1){
                resultLocation[j] +=  excessesTo[j][i] - excessFrom[i];
            }
        }
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