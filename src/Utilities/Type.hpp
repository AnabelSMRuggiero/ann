/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_TYPE_HPP
#define NND_TYPE_HPP

#include <vector>
#include <concepts>
#include <memory>
#include <ranges>
#include <algorithm>

namespace nnd{

template<typename ValueType, size_t alignment=32>
struct AlignedArray{
    using value_type = ValueType;
    private:
    std::unique_ptr<ValueType[]> data;
    size_t capacity;

    public:

    AlignedArray() = default;

    AlignedArray(size_t size): data(new (std::align_val_t(alignment)) ValueType[size]), capacity(size) {};

    AlignedArray(const AlignedArray& other): data(new (std::align_val_t(alignment)) ValueType[other.capacity]), capacity(other.capacity) {
        std::copy(other.begin(), other.end(), this->begin());
    };

    AlignedArray& operator=(AlignedArray&& other) = default;

    AlignedArray& operator=(const AlignedArray& other) = default;

    size_t size() const { return capacity; }

    ValueType* begin() { return std::assume_aligned<alignment>(data.get()); }

    ValueType* end() { return data.get() + capacity; }

    ValueType& operator[](size_t index) { return data[index]; }

    const ValueType* begin() const { return data.get(); }

    const ValueType* end() const { return data.get() + capacity; }

    const ValueType& operator[](size_t index) const{ return data[index]; }

};

template<typename ElementType, size_t alignment=32>
struct AlignedSpan{

    using value_type = std::remove_cv_t<ElementType>;
    private:
    ElementType* data;
    size_t extent;

    public:

    template<typename ConvertableToElement>
    AlignedSpan(const AlignedArray<ConvertableToElement, alignment>& dataToView): data(dataToView.begin()), extent(dataToView.size()){};

    template<typename ConvertableToElement>
    AlignedSpan(const AlignedSpan<ConvertableToElement>& spanToCopy): data(spanToCopy.data), extent(spanToCopy.extent){};

    ElementType* begin() const { return std::assume_aligned<alignment>(data); }

    ElementType* end() const { return data + extent; }

    ElementType& operator[](size_t index) const { return data[index]; };

    size_t size() const { return extent; };

};


template<typename DataTypeA, typename DataTypeB, typename RetType=double>
using SpaceMetric = RetType (*)(const DataTypeA&, const DataTypeB&);


template<typename DataTypeA, typename DataTypeB, typename RetType=std::vector<double>>
using BatchMetric = RetType (*)(const std::vector<DataTypeA>&, const DataTypeB&);

/*
I need to rethink this if this is the direction I'll go to refactor templates
template<typename DistType, typename COMExtent, typename DataType,
         std::integral BlockNumberType = size_t,
         std::integral DataIndexType = size_t>
struct nndBaseTypes{
    
    using DataEntry = AlignedArray<DataType>;

};

template<typename baseTypeLiteral>
using DistType = typename baseTypeLiteral::DistType;

template<typename baseTypeLiteral>
using COMExtent = typename baseTypeLiteral::COMExtent;

template<typename baseTypeLiteral>
using DataType = typename baseTypeLiteral::DataType;

template<typename baseTypeLiteral>
using BlockNumberType = typename baseTypeLiteral::BlockNumberType;

template<typename baseTypeLiteral>
using DataIndexType = typename baseTypeLiteral::DataIndexType;

template<typename baseTypeLiteral>
using DataEntry = typename baseTypeLiteral::DataEntry;
*/


}


#endif