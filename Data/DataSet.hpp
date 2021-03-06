/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_DATASET_HPP
#define NND_DATASET_HPP

#include <cstddef>
#include <filesystem>
#include <fstream>
#include <exception>
#include <iostream>
#include <new>

#include "../Type.hpp"
#include "../SIMD/VectorSpan.hpp"
#include "../AlignedMemory/DynamicArray.hpp"

#include "../AlignedMemory/AlignedAllocator.hpp"
#include "DataIterator.hpp"
#include "../Metrics/SpaceMetrics.hpp"

namespace nnd{

inline void OpenData(std::filesystem::path dataPath, const size_t vectorLength, const size_t endEntry, const size_t startEntry = 0){
    using DataType = float;

    std::ifstream dataStream(dataPath, std::ios_base::binary);
    if (!dataStream.is_open()) throw std::filesystem::filesystem_error("File could not be opened.", dataPath, std::make_error_code(std::io_errc::stream));

    const size_t numElements = vectorLength * (endEntry-startEntry);

    ann::dynamic_array<float> dataArr( vectorLength * (endEntry-startEntry));

    dataStream.read(reinterpret_cast<char*>(dataArr.begin()), numElements*sizeof(DataType));

    
}

template<typename DataEntry, std::align_val_t alignment>
size_t EntryPadding(std::size_t entryLength){
    size_t entryBytes = sizeof(DataEntry)*entryLength;
    size_t excessBytes = entryBytes % static_cast<std::size_t>(alignment);
    if (excessBytes == 0) return 0;
    size_t paddingBytes = static_cast<std::size_t>(alignment) - excessBytes;
    size_t paddingEntries = paddingBytes/sizeof(DataEntry);
    assert(paddingEntries*sizeof(DataEntry) == paddingBytes);
    return paddingBytes/sizeof(DataEntry);
    //((sizeof(DataType)*entryLength)%alignment > 0) ? alignment - entryLength%alignment : 0
}

using namespace ann::udl;
template<typename DataType, std::align_val_t align=ann::default_align>
struct DataSet{
    using value_type = DataType;
    using DataView = typename DefaultDataView<ann::aligned_array<DataType, align>>::ViewType;
    using ConstDataView = typename DefaultDataView<ann::aligned_array<DataType, align>>::ViewType;
    using const_vector_view = ann::vector_span<const value_type, ann::defaultInstructionSet, static_cast<std::size_t>(align)>;

    using iterator = ann::data_iterator<DataType, align>;
    using const_iterator = ann::data_iterator<const DataType, align>;

    //using iterator = typename std::vector<DataEntry>::iterator;
    //using const_iterator = typename std::vector<DataEntry>::const_iterator;
    //std::valarray<unsigned char> rawData;
    static constexpr std::align_val_t alignment{align};
    

    private:
    //DynamicArray<DataType> samples;
    std::size_t entry_length;
    std::size_t length_with_padding;
    std::size_t index_start;
    ann::aligned_array<DataType, alignment> samples;

    public:
    DataSet(std::filesystem::path data_path, std::size_t entry_length, std::size_t end_entry, std::size_t start_entry = 0, std::size_t file_header = 0):
        entry_length{entry_length},
        length_with_padding{entry_length + EntryPadding<DataType, alignment>(entry_length)},
        index_start(start_entry),
        samples(){
            
            std::size_t number_of_entries = end_entry - start_entry;

            assert(length_with_padding==entry_length); 
            std::ifstream dataStream(data_path, std::ios_base::binary);

            if (!dataStream.is_open()) throw std::filesystem::filesystem_error("File could not be opened.", data_path, std::make_error_code(std::io_errc::stream));

            const size_t numElements = length_with_padding * number_of_entries;

            ann::aligned_array<value_type, alignment> dataArr( numElements );

            dataStream.seekg(file_header + entry_length*start_entry);
            dataStream.read(reinterpret_cast<char*>(dataArr.begin()), numElements*sizeof(DataType));

            this->samples = std::move(dataArr);
            //if constexpr(std::same_as<uint32_t, value_type>){
            //    for (const auto& entry: this->samples) std::cout << entry << std::endl;
            //}
            
    }


    std::size_t IndexStart() const{
        return index_start;
    }
    /*
    DataView operator[](size_t i){
        return {samples.begin() + i*sampleLength, sampleLength};
    }

    ConstDataView operator[](size_t i) const{
        return {samples.begin() + i*sampleLength, sampleLength};
    }
    */
    DataView operator[](std::size_t i){
        value_type* dataPtr = samples.data();
        dataPtr += i * entry_length;
        if constexpr(alignment > ann::align_val_of<value_type>){
            return DataView(MakeAlignedPtr(dataPtr, *this), entry_length);
        } else {
            return DataView(dataPtr, entry_length);
        }
        /*
        AlignedPtr<value_type, alignment> ptr = samples.GetAlignedPtr(sampleLength);
        ptr += i;
        return DataView(ptr, sampleLength);
        */
        //return blockData[i];
    }
    
    ConstDataView operator[](std::size_t i) const{
        const value_type* dataPtr = samples.data();
        dataPtr += i * entry_length;
        return ConstDataView(MakeAlignedPtr(dataPtr, *this), entry_length);
        /*
        AlignedPtr<const value_type, alignment> ptr = samples.GetAlignedPtr(sampleLength);
        ptr += i;
        return ConstDataView(ptr, sampleLength);
        */
    }

    iterator begin(){
        return iterator{length_with_padding, entry_length, samples.data()};
    }

    const_iterator begin() const{
        return const_iterator{length_with_padding, entry_length, samples.data()};
    }

    const_iterator cbegin() const{
        return const_iterator{length_with_padding, entry_length, samples.data()};
    }

    iterator end(){
        return iterator{length_with_padding, entry_length, samples.data() + length_with_padding*(size())};
    }

    const_iterator end() const{
        return const_iterator{length_with_padding, entry_length, samples.data() + length_with_padding*(size())};
    }

    const_iterator cend() const{
        return const_iterator{length_with_padding, entry_length, samples.data() + length_with_padding*(size())};
    }

    std::size_t size() const{
        return samples.size()/(length_with_padding);
    }

    std::size_t SampleLength() const{
        return entry_length;
    }
    

};

/*
struct DataIterator{
    const size_t sampleLength;
    DataType* pointedSample;

    DataIterator& operator++(){
        pointedSample + sampleLength;
        return *this;
    }

    DataIterator operator++(int){
        DataIterator copy = *this;
        pointedSample + sampleLength;
        return copy;
    }

    DataIterator& operator--(){
        pointedSample - sampleLength;
        return *this;
    }

    DataIterator operator--(int){
        DataIterator copy = *this;
        pointedSample - sampleLength;
        return copy;
    }

    DataIterator operator+(std::ptrdiff_t inc){
        DataIterator copy{sampleLength, pointedSample+inc*sampleLength};
        return copy;
    }

    DataIterator operator-(std::ptrdiff_t inc){
        DataIterator copy{sampleLength, pointedSample-inc*sampleLength};
        return copy;
    }

    std::ptrdiff_t operator-(DataIterator other){
        return (pointedSample - other.pointedSample)/sampleLength;
    }

    bool operator==(DataIterator other){
        return pointedSample == other.pointedSample;
    }

    DataView operator*(){
        return DataView{pointedSample, sampleLength};
    }
};
*/

template<typename DataEntry>
void NormalizeDataSet(DataSet<DataEntry>& dataSet){
    for (auto&& entry: dataSet){
        ann::Normalize(entry);
    }
};


template<typename DataEntry, typename TargetDataType, std::endian endianness>
void SerializeDataSet(const DataSet<DataEntry>& dataSet, const std::string filePath){

    std::ofstream outStream(filePath, std::ios_base::binary);

    for(const auto& sample : dataSet.samples){
        for (const auto value : sample){
            TargetDataType valueToSerialize = static_cast<TargetDataType>(value);
            SerializeData<TargetDataType, endianness>(outStream, value);
        }
    };

}

}

#endif