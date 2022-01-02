/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_SIMD_HPP
#define NND_SIMD_HPP

#include <cstddef>

namespace nnd {




// x*y + z

/*

    x*y -> BinaryVectorOperation<vector, vector, multiply> tmp

    tmp + z -> BinaryVectorOperation<BinaryVectorOperation<vector, vector, multiply>, vector, add> res
               TernaryVectorOperation<
*/

}

#endif