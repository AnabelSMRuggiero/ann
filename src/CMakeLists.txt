# NNDescent.cpp: Copyright (c) Anabel Ruggiero
# At the time of writting, this code is unreleased and not published under a license.
# As a result, I currently retain all legal rights I am legally entitled to.
# 
# I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
# Please refer to the project repo for any updates regarding liscensing.
# https://github.com/AnabelSMRuggiero/NNDescent.cpp




set(
    ANN_SOURCE_DIR
    ${CMAKE_CURRENT_SOURCE_DIR}/ann
)

set(
    ALIGNED_MEMORY_HEADERS
    ${ANN_SOURCE_DIR}/AlignedMemory/AlignedAllocator.hpp
    ${ANN_SOURCE_DIR}/AlignedMemory/AlignedSpan.hpp
    ${ANN_SOURCE_DIR}/AlignedMemory/ContainerHelpers.hpp
    ${ANN_SOURCE_DIR}/AlignedMemory/DualVector.hpp
    ${ANN_SOURCE_DIR}/AlignedMemory/DynamicArray.hpp
)

set(
    DATA_HEADERS
    ${ANN_SOURCE_DIR}/Data/DataBlock.hpp
    ${ANN_SOURCE_DIR}/Data/DataIterator.hpp
    ${ANN_SOURCE_DIR}/Data/DataSet.hpp
    ${ANN_SOURCE_DIR}/Data/UnevenBlock.hpp
)

set(
    METRICS_HEADERS
    ${ANN_SOURCE_DIR}/Metrics/Angular.hpp
    ${ANN_SOURCE_DIR}/Metrics/Euclidean.hpp
    ${ANN_SOURCE_DIR}/Metrics/SpaceMetrics.hpp
)

set(
    SIMD_HEADERS
    ${ANN_SOURCE_DIR}/SIMD/SIMDInstructionSet.hpp
    ${ANN_SOURCE_DIR}/SIMD/SIMDOperations.hpp
    ${ANN_SOURCE_DIR}/SIMD/VectorSpan.hpp
)

set(
    ANN_HEADERS
    ${ANN_SOURCE_DIR}/Data.hpp
    ${ANN_SOURCE_DIR}/DataDeserialization.hpp
    ${ANN_SOURCE_DIR}/DataSerialization.hpp
    ${ANN_SOURCE_DIR}/DelayConstruct.hpp
    ${ANN_SOURCE_DIR}/MemoryResources.hpp
    ${ANN_SOURCE_DIR}/NestedRanges.hpp
    ${ANN_SOURCE_DIR}/PointerManipulation.hpp
    ${ANN_SOURCE_DIR}/SIMD.hpp
    ${ANN_SOURCE_DIR}/TemplateManipulation.hpp
    ${ANN_SOURCE_DIR}/Type.hpp
)

add_library(anns_numerical_nonsense INTERFACE)

target_sources(
    anns_numerical_nonsense
    INTERFACE
        ${ANN_HEADERS}
        ${SIMD_HEADERS}
        ${METRICS_HEADERS}
        ${DATA_HEADERS}
        ${ALIGNED_MEMORY_HEADERS}
)

target_include_directories(
    anns_numerical_nonsense
    INTERFACE
        ${CMAKE_CURRENT_SOURCE_DIR}
)

target_compile_features(anns_numerical_nonsense INTERFACE cxx_std_20)