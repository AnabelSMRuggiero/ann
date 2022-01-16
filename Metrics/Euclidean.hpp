/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_EUCLIDEAN_HPP
#define NND_EUCLIDEAN_HPP

#include <immintrin.h>
#include <type_traits>
#include <bit>
#include <array>
#include <span>
#include <cassert>

#include "SpaceMetrics.hpp"

#include "../Type.hpp"
#include "../MemoryResources.hpp"
#include "../SIMD.hpp"

namespace nnd{

template<typename VectorA, typename VectorB, typename RetType=double>
RetType EuclideanNorm(const VectorA& pointA, const VectorB& pointB){

    auto transformFunc = [](const auto& operandA, const auto& operandB){
        RetType diff = static_cast<RetType>(operandA) - static_cast<RetType>(operandB);
        return diff*diff;
    };

    RetType accum = std::transform_reduce(std::execution::unseq,
                                          std::begin(pointA), std::end(pointA),
                                          std::begin(pointB),
                                          RetType(0),
                                          std::plus<RetType>(),
                                          transformFunc);
    return std::sqrt(accum);
};


template<size_t numPointsTo>
void BatchEuclideanNorm(const ann::vector_span<const float> pointFrom,
                        std::span<const ann::vector_span<const float>, numPointsTo> pointsTo,
                        std::span<float, numPointsTo> resultLocation) noexcept {

    
    std::array<ann::DataVector<float>, numPointsTo> accumulators;



    /* vvv core loop vvv */

    for (size_t i = 0; i<pointFrom.size(); i+=1){
        for(size_t j = 0; j<numPointsTo; j+=1){
            ann::DataVector<float> diff = pointsTo[j][i] - pointFrom[i];
            accumulators[j] += diff*diff;
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
                float diff = excessesTo[j][i] - excessFrom[i];
                resultLocation[j] += diff*diff;
            }
        }
    }

    
    
    //Last I checked, this emits an avx sqrt on clang
    for (auto& res: resultLocation) res = std::sqrt(res);


}
/*
template<size_t numPointsTo>
void BatchEuclideanNorm(const AlignedSpan<const float> pointB,
                        std::span<const AlignedSpan<const float>, numPointsTo> pointsTo,
                        std::span<float, numPointsTo> resultLocation) noexcept {

    
    std::array<__m256, numPointsTo> accumulators;
    for (__m256& accumulator: accumulators) accumulator = _mm256_setzero_ps();

    std::array<__m256, numPointsTo> toComponents;

    __m256 fromComponent1, fromComponent2;

    size_t index = 0;

    

    [[likely]] if(pointB.size()>=8){
        //Pre load first set of elements
        fromComponent1 = _mm256_load_ps(&(pointB[0]));

        //for (size_t i = 0; i < numPointsTo; i+=1) toComponents[i] = NTLoadFloat(&(pointsTo[i][0]));
        for (size_t i = 0; i < numPointsTo; i += 1) toComponents[i] = _mm256_load_ps(&(pointsTo[i][0]));
            
        //Core computation loop
        for(;index+15<pointB.size(); index+=8){
            fromComponent2 = _mm256_load_ps(&(pointB[index+8]));
            
            for(size_t j = 0; j<numPointsTo; j+=1) toComponents[j] = _mm256_sub_ps(toComponents[j], fromComponent1);
            for(size_t j = 0; j<numPointsTo; j+=1) accumulators[j] = _mm256_fmadd_ps(toComponents[j], toComponents[j], accumulators[j]);
                
            //Load for next iteration
            for(size_t j = 0; j<numPointsTo; j+=1) toComponents[j] = _mm256_load_ps(&(pointsTo[j][index+8]));
            //for(size_t j = 0; j<numPointsTo; j+=1) toComponents[j] = NTLoadFloat(&(pointsTo[j][index+8]));
            fromComponent1 = fromComponent2;
        }
        
        
        //Already have fromComponent1 loaded for the last iter
        for(size_t j = 0; j<numPointsTo; j+=1) toComponents[j] = _mm256_sub_ps(toComponents[j], fromComponent1);
        for(size_t j = 0; j<numPointsTo; j+=1) accumulators[j] = _mm256_fmadd_ps(toComponents[j], toComponents[j], accumulators[j]);

        index +=8;
        fromComponent2 = _mm256_setzero_ps();
        //reduce the results
        for(size_t j = 0; j<2; j+=1){
            for (auto& accumulator: accumulators){
                accumulator = _mm256_hadd_ps(accumulator, fromComponent2);
            }
        }

        for (size_t j = 0; j<numPointsTo; j+=1){
            
            //This constexpr branch works with MSVC and Clang, haven't tried GCC, but I suspect it should.
            if constexpr (std::is_union_v<__m256>){
                resultLocation[j] = accumulators[j].m256_f32[0] + accumulators[j].m256_f32[4];
            } else{
                resultLocation[j] = accumulators[j][0] + accumulators[j][4];
            }
            //__GNUC__
        }

    }
    //Take care of the excess. I should be able to remove this when I get alignment right
    for ( ; index<pointB.size(); index += 1){
        for (size_t j = 0; j<numPointsTo; j+=1){
            float diff = pointsTo[j][index] - pointB[index];
            resultLocation[j] += diff*diff;
        }
    }
    //Last I checked, this emits an avx sqrt on clang
    for (auto& res: resultLocation) res = std::sqrt(res);


}
*/





}

#endif