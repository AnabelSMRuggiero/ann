# NNDescent.cpp: Copyright (c) Anabel Ruggiero
# At the time of writting, this code is unreleased and not published under a license.
# As a result, I currently retain all legal rights I am legally entitled to.
# 
# I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
# Please refer to the project repo for any updates regarding liscensing.
# https://github.com/AnabelSMRuggiero/NNDescent.cpp

add_executable(ann_test)

target_sources(
    ann_test
    PRIVATE DualVectorTest.cpp
)

target_link_libraries(
    ann_test
    PRIVATE Catch2::Catch2WithMain
    INTERFACE anns_numerical_nonsense
)