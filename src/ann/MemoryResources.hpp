/*
NNDescent.cpp: Copyright (c) Anabel Ruggiero
At the time of writting, this code is unreleased and not published under a license.
As a result, I currently retain all legal rights I am legally entitled to.

I am currently considering a permissive license for releasing this code, such as the Apache 2.0 w/LLVM exception.
Please refer to the project repo for any updates regarding liscensing.
https://github.com/AnabelSMRuggiero/NNDescent.cpp
*/

#ifndef NND_MULTITRANSFORM_HPP
#define NND_MULTITRANSFORM_HPP

#include <exception>
#include <memory_resource>
#include <cstddef>
#include <utility>
#include <new>
#include <array>
#include <optional>
#include <iostream>
#include <algorithm>
#include <atomic>
#include <mutex>
#include <bit>
#include <memory>
#include <cstddef>
#include <type_traits>
#include <cstdlib>
#include <iostream>
#include <thread>

#include "AlignedMemory/AlignedAllocator.hpp"
#include "AlignedMemory/DynamicArray.hpp"
#include "PointerManipulation.hpp"

namespace ann{

template<typename TryLockable>
[[nodiscard]] std::unique_lock<TryLockable> spin_acquire(TryLockable& mutex){
    std::unique_lock retLock{mutex, std::try_to_lock};
    while(!retLock){
        std::this_thread::yield();
        retLock.try_lock();
    }
    return std::move(retLock);
}


struct FreeListNode{
    std::byte* chunkStart;
    size_t chunkSize;
    size_t chunkAlign;
    FreeListNode* next;
};

//Not fully implemented
/*
struct FreeListResource : std::pmr::memory_resource{

    void* do_allocate( std::size_t bytes, std::size_t alignment ) override{
        auto [newSize, newAlign] = AdjustAllocation(bytes, alignment);
        if (head == nullptr){
            return GetChunk(newSize, newAlign);
        } else {

        }
        //itr through list and return valid chunk size
            //if good relocate list node to new alloc end
            //and return ptr to chunk start

        //else return new chunk
    }

    void do_deallocate( void* p, std::size_t bytes, std::size_t alignment ) override{
        //find list node location from args
        //push to front of list
        //if list over threshold, return an old chunk upstream
    }

    bool do_is_equal( const std::pmr::memory_resource& other ) const noexcept override{
        return this == &other;
    };


    private:



    std::pmr::memory_resource* upstream = std::pmr::get_default_resource();
    FreeListNode* head = nullptr;
    size_t listSize = 0;


    void* FindNodeLocation(std::byte* chunkStart, size_t size, size_t alignment){
        //alignment = std::max(alignment , alignof(FreeListNode));
        size_t excess = size % alignof(FreeListNode);
        if(excess > 0){
            size += alignof(FreeListNode) - excess;
        }
        void* ptrToNode = static_cast<void*>(chunkStart + size);
        return ptrToNode;
    }

    void ReemplaceNode(FreeListNode* chunkNode, size_t requestedSize, size_t requestedAlignment){
        FreeListNode temp = *chunkNode;
        temp.next = nullptr;
        chunkNode->~FreeListNode();
        void* newLoc = FindNodeLocation(temp.chunkStart, requestedSize, requestedAlignment);
        new (newLoc) FreeListNode(temp);
    }

    FreeListNode* PopFront(){
        FreeListNode* oldHead = head;
        head = head->next;
        listSize -= 1;
        return oldHead;
    }

    void PushFront(FreeListNode* newNode){

        newNode->next = head;
        head = newNode;
        listSize += 1;

    }

    void ReturnChunk(FreeListNode* listNode){
        upstream->deallocate(listNode->chunkStart, listNode->chunkSize, listNode->chunkAlign);
    }

    std::pair<size_t, size_t> AdjustAllocation(size_t size, size_t alignment){
        alignment = std::max(alignment, alignof(FreeListNode));
        size_t excess = size % alignof(FreeListNode);
        if(excess > 0){
            size += alignof(FreeListNode) - excess;
        }
        size += sizeof(FreeListNode);
        return {size, alignment};
    }

    void* GetChunk(size_t size, size_t alignment){

        //auto [newSize, newAlign] = AdjustAllocation(size, alignment);

        void* chunk = upstream->allocate(size, alignment);

        std::byte* byteArray = new (chunk) std::byte[size];
        std::byte* nodeLocation = byteArray + (size - sizeof(FreeListNode));
        new (nodeLocation) FreeListNode{byteArray, size, alignment, nullptr};

        return chunk;
    }

};
*/


struct cache_node{
    std::byte* chunkStart = nullptr;
    size_t chunkSize = 0;
    size_t chunkAlign = 0;

    cache_node() = default;
    cache_node(const cache_node&) = default;
    cache_node(cache_node&&) = default;
    cache_node& operator=(const cache_node&) = default;
    cache_node& operator=(cache_node&&) = default;

    cache_node(std::byte* chunkStart, size_t chunkSize, size_t chunkAlign):
        chunkStart{chunkStart},
        chunkSize{chunkSize},
        chunkAlign{chunkAlign} {}

    cache_node(std::nullptr_t): cache_node(){}

    cache_node& operator=(std::nullptr_t){
        chunkStart = nullptr;
        return *this;
    }

    operator bool() const {
        return chunkStart == nullptr;
    }

    bool operator==(const cache_node&) const = default;

    bool operator==(std::nullptr_t) const {
        return chunkStart == nullptr;
    }
};

template<typename CachePolicy>
struct CacheBase;

template<typename CachePolicy>
concept HasOwningType = requires { 
    typename CachePolicy::OwningCache;
    {typename CachePolicy::OwningCache{}} -> std::same_as<std::true_type>;
};

template<typename CachePolicy>
concept HasOwningMember = requires { 
    {std::integral_constant<bool, CachePolicy::isOwning>{}} -> std::same_as<std::true_type>;
};

template<typename CachePolicy>
concept OwningCache = (HasOwningType<CachePolicy> || HasOwningMember<CachePolicy>) && requires (CachePolicy derived){
    typename CacheBase<CachePolicy>::NodeType;
    derived.Register(typename CacheBase<CachePolicy>::NodeType{});
};

template<typename CachePolicy>
struct CacheBase : std::pmr::memory_resource{
    public:
    using NodeType = cache_node;
    using Derived = CachePolicy;
    using ResourceType = std::pmr::memory_resource;

    static constexpr bool isOwning = OwningCache<Derived>;

    

    protected:
    void* do_allocate( std::size_t bytes, std::size_t alignment ) override{
        auto [newSize, newAlign] = AdjustAllocation(bytes, alignment);

        NodeType fromCache = DerivedCast().SearchCache(newSize, newAlign);

        if (fromCache != nullptr){
            std::byte* memPtr = fromCache.chunkStart;
            ReemplaceNode(fromCache, bytes, alignment);
            return static_cast<void*>(memPtr);
        } else {
            return GetChunk(newSize, newAlign);
        }
        //else return new chunk
    }

    void do_deallocate( void* p, std::size_t bytes, std::size_t alignment ) override{
        std::byte* byteArray = std::launder(static_cast<std::byte*>(p));
        NodeType* nodeLocation = std::launder(static_cast<NodeType*>(FindNodeLocation(byteArray, bytes, alignment)));

        NodeType nodeToReturn = *nodeLocation;
        nodeLocation->~NodeType();

        if (NodeType evictedNode = DerivedCast().PlaceNode(*nodeLocation); evictedNode != nullptr){
            ReturnChunk(evictedNode);
        }
    }


    bool do_is_equal( const std::pmr::memory_resource& other ) const noexcept override {
        return this == &other;
    };


    
    void ReturnChunk(const NodeType& memToReturn){
        Upstream().deallocate(memToReturn.chunkStart, memToReturn.chunkSize, memToReturn.chunkAlign);
    }

    private:
    Derived& DerivedCast() {
        return static_cast<Derived&>(*this);
    }

    ResourceType& Upstream(){
        return static_cast<Derived&>(*this).GetResource();
    }
    
    void* FindNodeLocation(std::byte* chunkStart, size_t size, size_t alignment){
        alignment = std::max(alignment, alignof(NodeType));
        size_t excess = size % alignof(NodeType);
        if(excess > 0){
            size += alignof(NodeType) - excess;
        }
        void* ptrToNode = static_cast<void*>(chunkStart + size);
        return ptrToNode;
    }

    void ReemplaceNode(const NodeType& nodeToPlace, size_t requestedSize, size_t requestedAlignment){
        void* newLoc = FindNodeLocation(nodeToPlace.chunkStart, requestedSize, requestedAlignment);
        new (newLoc) NodeType(nodeToPlace);
    }




    std::pair<size_t, size_t> AdjustAllocation(size_t size, size_t alignment){
        alignment = std::max(alignment, alignof(NodeType));
        size_t excess = size % alignof(NodeType);
        if(excess > 0){
            size += alignof(NodeType) - excess;
        }
        size += sizeof(NodeType);
        return {size, alignment};
    }

    void* GetChunk(size_t size, size_t alignment){

        //auto [newSize, newAlign] = AdjustAllocation(size, alignment);

        void* chunk = Upstream().allocate(size, alignment);

        std::byte* byteArray = new (chunk) std::byte[size];
        if constexpr (isOwning){
            DerivedCast().Register(NodeType{byteArray, size, alignment});
        }

        std::byte* nodeLocation = byteArray + (size - sizeof(NodeType));
        new (nodeLocation) NodeType{byteArray, size, alignment};
        

        return chunk;
    }
};

template<size_t cacheSlots = 3>
struct ShallowCache : CacheBase<ShallowCache<cacheSlots>> {
    using ResourceType = std::pmr::memory_resource;
    using OwningCache = std::false_type;
    static constexpr bool isOwning = OwningCache{};
    static constexpr size_t cacheSize = cacheSlots;

    using NodeType = typename CacheBase<ShallowCache>::NodeType;

    using CacheIterator = typename std::array<NodeType, cacheSize>::iterator;

    ShallowCache() = default;

    ShallowCache(std::pmr::memory_resource* upstream): upstream(*upstream) {}

    // Do I want a movable/copiable cache to be possible?
    // If not, move these deletes to the base.
    ShallowCache(const ShallowCache&) = delete;

    ShallowCache(ShallowCache&&) = delete;

    ShallowCache& operator=(const ShallowCache&) = delete;

    ShallowCache& operator=(ShallowCache&&) = delete;

    ~ShallowCache(){
        for (auto itr = cachedMemory.begin(); itr != partitionPoint; itr++){
            this->ReturnChunk(*itr);
        }
    }

    NodeType SearchCache(const size_t chunkSize, const size_t alignment){
        for (auto itr = cachedMemory.begin(); itr != partitionPoint; itr++){
            NodeType& node = *itr;
            if ((node.chunkSize >= chunkSize) && (node.chunkAlign >= alignment)){
                NodeType retNode = node;
                node = nullptr;
                --partitionPoint;
                std::swap(node, *partitionPoint);
                return retNode;
            }
        }

        return nullptr;
    }

    NodeType PlaceNode(NodeType nodeToPlace){
        if (partitionPoint != cachedMemory.end()){
            *partitionPoint = nodeToPlace;
            partitionPoint++;
            return nullptr;
        } else{
            NodeType retNode = cachedMemory.back();
            cachedMemory.back() = cachedMemory.front();
            cachedMemory.front() = nodeToPlace;
            return retNode;
        }
    }

    ResourceType& GetResource(){
        return upstream;
    }

    private:
    std::array<NodeType, cacheSize> cachedMemory;
    CacheIterator partitionPoint = cachedMemory.begin();

    ResourceType& upstream{*std::pmr::get_default_resource()};
};



struct UnboundedCache : CacheBase<UnboundedCache> {
    using ResourceType = std::pmr::memory_resource;

    static constexpr bool isOwning = true;

    using NodeType = typename CacheBase<UnboundedCache>::NodeType;

    UnboundedCache() = default;

    UnboundedCache(std::pmr::memory_resource* upstream): upstream(*upstream) {}

    // Do I want a movable/copiable cache to be possible?
    // If not, move these deletes to the base.
    UnboundedCache(const UnboundedCache&) = delete;

    UnboundedCache(UnboundedCache&&) = delete;

    UnboundedCache& operator=(const UnboundedCache&) = delete;

    UnboundedCache& operator=(UnboundedCache&&) = delete;

    ~UnboundedCache(){
        for(auto&& node : memoryHistory){
            ReturnChunk(node);
        }
    }

    NodeType SearchCache(const size_t chunkSize, const size_t alignment){
        auto locationItr = std::ranges::lower_bound(cachedMemory, chunkSize, std::less<>{}, &NodeType::chunkSize);
        while (locationItr != cachedMemory.end()){
            if (locationItr->chunkAlign >= alignment){
                NodeType retNode = *locationItr;
                cachedMemory.erase(locationItr);
                return retNode;
            }
            ++locationItr;
        }
        return nullptr;
    }

    std::nullptr_t PlaceNode(NodeType nodeToPlace){
        auto locationItr = std::ranges::lower_bound(cachedMemory, nodeToPlace.chunkSize, std::less<>{}, &NodeType::chunkSize);
        cachedMemory.insert(locationItr, std::move(nodeToPlace));
        return nullptr;
    }

    void Register(NodeType newNode){
        //auto locationItr = std::ranges::lower_bound(memoryHistory, newNode.chunkSize, std::less<>{}, &NodeType::chunkSize);
        //cachedMemory.insert(locationItr, std::move(newNode));
        memoryHistory.push_back(std::move(newNode));
    }

    ResourceType& GetResource(){
        return upstream;
    }

    private:
    ResourceType& upstream{*std::pmr::get_default_resource()};

    std::pmr::vector<NodeType> cachedMemory{&upstream};
    std::pmr::vector<NodeType> memoryHistory{&upstream};

};



//For testing, prints allocs and deallocs to cout
struct ChatterResource : std::pmr::memory_resource{

    ChatterResource() = default;

    ChatterResource(std::pmr::memory_resource* upstream): upstream(upstream){}

    ChatterResource(const ChatterResource&) = delete;

    ChatterResource(ChatterResource&&) = delete;

    ChatterResource& operator=(const ChatterResource&) = delete;

    ChatterResource& operator=(ChatterResource&&) = delete;

    private:

    void* do_allocate( std::size_t bytes, std::size_t alignment ) override{

        std::cout << "Allocation - size: " << bytes << ", alignment: " << alignment << std::endl;

        return upstream->allocate(bytes, alignment);
        //else return new chunk
    }

    void do_deallocate( void* p, std::size_t bytes, std::size_t alignment ) override{

        std::cout << "Deallocation - size: " << bytes << ", alignment: " << alignment << std::endl;

        upstream->deallocate(p, bytes, alignment);
    }

    bool do_is_equal( const std::pmr::memory_resource& other ) const noexcept override{
        return this == &other;
    };

    std::pmr::memory_resource* upstream = std::pmr::get_default_resource();
};

struct single_list_node{
    single_list_node* next;
};
struct MemoryChunk{
    std::byte* start;
    size_t size;
};


//Implementation detail of AtomicMultiPool
// Guh, atomics hard
/*
    SingleListNode* currentHead = stackHead.load();
            while(currentHead != nullptr){
                if(stackHead.compare_exchange_strong(currentHead, currentHead->next)){
                    currentHead->~SingleListNode();
                    return PtrCast<std::byte*>(currentHead);
                }
            }
*/
struct AtomicPool{
    const size_t chunkSize;
    const size_t restockAmount;

    AtomicPool(const size_t chunkSize, const size_t restockAmount, std::pmr::memory_resource* upstream):
        chunkSize{chunkSize},
        restockAmount{restockAmount},
        memoryChunks{upstream} {}

    std::byte* Allocate(const auto& grabFromUpstream){
        
        while(true){
            single_list_node* currentHead = stackHead.load();
            while(currentHead != nullptr){
                if(stackHead.compare_exchange_strong(currentHead, currentHead->next)){
                    currentHead->~single_list_node();
                    return PtrCast<std::byte*>(currentHead);
                }
            }
            if(currentHead == nullptr){
                Restock(grabFromUpstream, currentHead);
            }
        }
    }

    void Restock(auto& grabFromUpstream, single_list_node* currentHead = nullptr){
        std::unique_lock restockGuard(restockMutex, std::try_to_lock_t{});
        if (restockGuard){
            do{
                std::byte* memoryChunk = grabFromUpstream(restockAmount*chunkSize, memoryChunks);
                //memoryChunks.push_back(memoryChunk);
                for (size_t i = 0; i<restockAmount; i+=1){
                    single_list_node* newNode = new (memoryChunk) single_list_node{stackHead.load()};
                    while(!stackHead.compare_exchange_strong(newNode->next, newNode)){};
                    memoryChunk += chunkSize;
                    stackHead.notify_one();
                }
            } while((waitingThreads.load() == 0) && (stackHead.load() != nullptr));
        } else{
            waitingThreads.fetch_add(1);
            stackHead.wait(currentHead);
            waitingThreads.fetch_sub(1);
        }
    }

    void Deallocate(std::byte* memoryChunk){
        single_list_node* returningChunk = new (memoryChunk) single_list_node{stackHead.load()};
        while(!stackHead.compare_exchange_strong(returningChunk->next, returningChunk)){};
    }

    void Release(auto& returnToUpstream){
        returnToUpstream(memoryChunks);
    }

    private:
    std::atomic<single_list_node*> stackHead{nullptr};
    std::mutex restockMutex;
    std::atomic<size_t> waitingThreads{0};
    std::pmr::vector<MemoryChunk> memoryChunks;
};

struct LockPool{
    const std::size_t chunkSize;
    const std::size_t restockAmount;

    LockPool(const size_t chunkSize, const size_t restockAmount, std::pmr::memory_resource* upstream):
        chunkSize{chunkSize},
        restockAmount{restockAmount},
        memoryChunks{upstream} {}

    std::byte* Allocate(auto& grabFromUpstream){

        std::unique_lock poolLock = spin_acquire(poolGuard);

        if(stackHead == nullptr){ 
            Restock(grabFromUpstream);
        }

        single_list_node* retChunk = stackHead;
        std::ranges::swap(stackHead, retChunk->next);

        retChunk->~single_list_node();
        return PtrCast<std::byte*>(retChunk);
    }

    void Restock(auto& grabFromUpstream, single_list_node* currentHead = nullptr){

        std::byte* memoryChunk = grabFromUpstream(restockAmount*chunkSize, memoryChunks);
        
        for (size_t i = 0; i<restockAmount; i+=1){
            single_list_node* newNode = new (memoryChunk) single_list_node{stackHead};
            std::ranges::swap(stackHead, newNode);
            memoryChunk += chunkSize;
        }    
 
    }

    void Deallocate(std::byte* memoryChunk){

        std::unique_lock poolLock = spin_acquire(poolGuard);

        single_list_node* returningChunk = new (memoryChunk) single_list_node{stackHead};
        std::ranges::swap(stackHead, returningChunk);

    }

    

    void Release(auto& returnToUpstream){
        returnToUpstream(memoryChunks);
    }

    private:
    
    single_list_node* stackHead{nullptr};
    std::mutex poolGuard;
    std::pmr::vector<MemoryChunk> memoryChunks;
};

struct memory_array{
    static constexpr std::size_t pointers_per_slot = std::hardware_destructive_interference_size/sizeof(std::byte*);

    using memory_slots = pmr::aligned_array<std::atomic<std::byte*>, 64_a>;
    using slot = std::span<std::atomic<std::byte*>>;

    memory_slots slots;
    std::atomic<std::size_t> switch_counter = 0;
    memory_array(std::size_t num_slots, std::pmr::memory_resource* resource): 
        slots{num_slots*pointers_per_slot, resource} {}

    slot operator[](std::size_t index){
        std::size_t switch_state = switch_counter.load();
        return slot{slots.begin() + (index*pointers_per_slot), pointers_per_slot};
        
        
    }

    auto raw_begin(){
        return slots.begin();
    }

    auto raw_end(){
        return slots.end();
    }
};

struct array_pool{
    static constexpr std::size_t number_of_slots = 1 << 4;
    static constexpr std::size_t counter_mask = number_of_slots - 1;


    std::atomic<std::size_t> allocation_counter{0};
    std::atomic<std::size_t> deallocation_counter{0};
    std::atomic<std::size_t> switch_counter{0};
    std::mutex pool_lock;
    single_list_node* reserve_memory{nullptr};

    std::array<memory_array, 2> preallocated_memory;
    std::array<memory_array, 2> deallocation_space;
    const std::size_t chunk_size;
    const std::size_t restock_amount;
    std::pmr::vector<MemoryChunk> memory_chunks;

    
    array_pool(std::size_t chunk_size,
               std::size_t restock_amount,
               std::pmr::memory_resource* resource):
        preallocated_memory{
            memory_array{number_of_slots, resource},
            memory_array{number_of_slots, resource}
        },
        deallocation_space{
            memory_array{number_of_slots, resource},
            memory_array{number_of_slots, resource}
        },
        chunk_size{chunk_size},
        restock_amount{restock_amount},
        memory_chunks(resource) {}
    /*
    ~array_pool(){
        std::cout<< "Pool chunk size: " << chunk_size << "\t Number of allocations:" << allocation_counter << std::endl;
    }
    */

    template<std::invocable<std::size_t, std::pmr::vector<MemoryChunk>&> FetchFunc>
    void initialize_stock(FetchFunc&& fetch_from_upstream){
        for(auto& mem_array : preallocated_memory){
            for(std::ranges::subrange prealloc_view ={mem_array.raw_begin(), mem_array.raw_end()};
            auto& atomic_ptr : prealloc_view){
                if(reserve_memory == nullptr){
                    fill_reserve(fetch_from_upstream);
                }
                if(atomic_ptr == nullptr){    
                    atomic_ptr.store(pop_reserve());
                }
            }
        }
    }
    
    template<std::invocable<std::size_t, std::pmr::vector<MemoryChunk>&> FetchFunc>
    std::byte* Allocate(FetchFunc&& fetch_from_upstream){
        
        
        while(true){
            const std::size_t slot = allocation_counter.fetch_add(std::size_t{1}, std::memory_order_relaxed) & counter_mask;
            std::size_t counter_state = switch_counter.load();
            auto memory_slot = preallocated_memory[counter_state & 1][slot];
            for(auto& atomic_ptr : memory_slot){
                std::byte* ret_pointer = atomic_ptr.exchange(nullptr);
                if (ret_pointer != nullptr){
                    return ret_pointer;
                }
            }

            if(std::unique_lock grabbed_lock{pool_lock, std::try_to_lock}){
                std::size_t old_state = switch_counter.fetch_add(std::size_t{1});
                switch_counter.notify_all();
                restock(fetch_from_upstream, old_state);
            } else{
                switch_counter.wait(counter_state);
            }
            
        }
    }

    template<std::invocable<std::size_t, std::pmr::vector<MemoryChunk>&> FetchFunc>
    void Deallocate(std::byte* memory_chunk, FetchFunc&& fetch_from_upstream){
        const std::size_t slot = deallocation_counter.fetch_add(std::size_t{1}, std::memory_order_relaxed) & counter_mask;

        while(true){
            std::size_t counter_state = switch_counter.load();
            auto memory_slot = deallocation_space[counter_state & 1][slot];
            for(auto& atomic_ptr : memory_slot){
                std::byte* expected = nullptr;
                if (atomic_ptr.compare_exchange_strong(expected, memory_chunk)){
                    return;
                }
            }

            
            if(std::unique_lock grabbed_lock{pool_lock, std::try_to_lock}){
                std::size_t old_state = switch_counter.fetch_add(std::size_t{1});
                switch_counter.notify_all();
                restock(fetch_from_upstream, old_state);
            } else{
                switch_counter.wait(counter_state);
            }
        }
    }

    void Release(auto& return_to_upstream){
        clear_arrays();
        reserve_memory = nullptr;
        return_to_upstream(memory_chunks);
    }
    

    

    void restock(auto& fetch_from_upstream, std::size_t old_state){
        auto prealloc_begin = preallocated_memory[old_state & 1].raw_begin();
        auto prealloc_end = preallocated_memory[old_state & 1].raw_end();

        auto dealloc_begin = deallocation_space[old_state & 1].raw_begin();
        auto dealloc_end = deallocation_space[old_state & 1].raw_end();
        
        auto advance_itrs = [&]{
            prealloc_begin = std::find(prealloc_begin, prealloc_end, nullptr);
            dealloc_begin = std::find_if(dealloc_begin, dealloc_end, [](auto& ptr) {return ptr != nullptr;});
            return prealloc_begin != prealloc_end
                && dealloc_begin  != dealloc_end;
        };


        while (advance_itrs()){
            auto memory = dealloc_begin->exchange(nullptr);
            prealloc_begin->store(memory);
        }

        if (prealloc_begin != prealloc_end){
            // we have to fill more spaces in preallocated_memory
            while (prealloc_begin !=  prealloc_end) {
                if(reserve_memory == nullptr){
                    fill_reserve(fetch_from_upstream);
                }
                prealloc_begin->store(pop_reserve());
                prealloc_begin = std::find(prealloc_begin, prealloc_end, nullptr);
            }

        } else if(dealloc_begin != dealloc_end){
            // we have to clear more spaces in deallocation_space
            while (dealloc_begin != dealloc_end){
                push_reserve(dealloc_begin->exchange(nullptr));
                dealloc_begin = std::find_if(dealloc_begin, dealloc_end, [](auto& ptr) {return ptr != nullptr;});
            }
        }
    }

    void fill_reserve(auto& fetch_from_upstream){
        std::byte* memory_chunk = fetch_from_upstream(restock_amount*chunk_size, memory_chunks);
        for (size_t i = 0; i<restock_amount; i+=1){
            single_list_node* new_node = new (memory_chunk) single_list_node{reserve_memory};
            std::ranges::swap(reserve_memory, new_node);
            memory_chunk += chunk_size;
        } 
    }

    std::byte* pop_reserve(){
        single_list_node* ret_memory = reserve_memory;
        std::ranges::swap(reserve_memory, ret_memory->next);

        std::destroy_at<single_list_node>(ret_memory);
        return PtrCast<std::byte*>(ret_memory);
    }

    void push_reserve(std::byte* memory_chunk){
        single_list_node* returning_chunk = new (memory_chunk) single_list_node{reserve_memory};
        std::ranges::swap(reserve_memory, returning_chunk);
    }

    void clear_arrays(){
        constexpr auto advance  = [](auto& begin, auto& end){
            return std::find_if(begin, end, [](auto& ptr){return ptr != nullptr;});
        };

        auto sweep_array = [&](auto& array){
            auto begin = array.raw_begin();
            auto end = array.raw_begin();
            begin = advance(begin, end);
            while(begin != end){
                push_reserve(begin->exchange(nullptr));
                begin =  advance(begin, end);
            }
        };
        

        sweep_array(preallocated_memory[0]);
        sweep_array(preallocated_memory[1]);
       
        sweep_array(deallocation_space[0]);
        sweep_array(deallocation_space[1]);
    }
    
};

struct UpstreamGuard : public std::pmr::memory_resource{

    private:

    void* do_allocate( std::size_t bytes, std::size_t alignment ) override {

        return upstream->allocate(bytes, alignment);
        
    }

    void do_deallocate( void* p, std::size_t bytes, std::size_t alignment ) override {

        upstream->deallocate(p, bytes, alignment);

    }

    bool do_is_equal( const std::pmr::memory_resource& other ) const noexcept override {

        return this == &other;

    };


    std::mutex& mutexRef;
    std::pmr::memory_resource* upstream;
};

struct oversized_handler{

    oversized_handler(std::pmr::memory_resource* upstream, std::mutex& upstreamLockRef) : 
        cache{upstream},
        upstreamLock{upstreamLockRef} {}

    void* Allocate( std::size_t bytes, std::size_t alignment ) {
        std::scoped_lock lock{upstreamLock};
        return cache.allocate(bytes, alignment); 
    }

    void Deallocate( void* p, std::size_t bytes, std::size_t alignment ) {
        std::scoped_lock lock{upstreamLock};
        cache.deallocate(p, bytes, alignment);
    }

    private:
    UnboundedCache cache;
    std::mutex& upstreamLock;

};


template<std::size_t numberOfPools>
constexpr std::array<array_pool, numberOfPools> make_pools(std::size_t startSize,
                                                         std::size_t defaultRestock,
                                                         std::pmr::memory_resource* upstream){

    auto makePool = [&](const size_t idx){
        return array_pool{startSize << idx, defaultRestock, upstream};
    };

    auto builder = [&]<size_t... Idx>(std::index_sequence<Idx...>){
        return std::array{makePool(Idx)...};
    };
    return builder(std::make_index_sequence<numberOfPools>{});
}


struct threaded_multipool : std::pmr::memory_resource{
    static constexpr size_t startSize = 16;
    static constexpr size_t numberOfPools = 12;
    static constexpr size_t biggestPoolSize = startSize<<(numberOfPools-1); //8k atm

    static constexpr size_t defaultAlignment = 16;
    static_assert(defaultAlignment>=sizeof(std::byte*)); //Implementation assumption
    static constexpr size_t defaultRestock = 16*16; //can do something smarter later

    auto bind_fetch_from_upstream(){
        return [&, this] (const size_t chunkSize, std::pmr::vector<MemoryChunk>& poolChunks) mutable ->std::byte*{
            std::unique_lock lockMultipool{this->upstreamLock};
            std::byte* memory = static_cast<std::byte*>(this->upstreamAlloc.allocate_bytes(chunkSize, defaultAlignment));
            //This may cause the vector to reallocate, that reallocation MUST be guarded.
            poolChunks.push_back({memory, chunkSize});
            return memory;
        };
    }

    threaded_multipool(std::pmr::memory_resource* upstream = std::pmr::get_default_resource()): upstreamAlloc{upstream}, memoryPools{make_pools<numberOfPools>(startSize, defaultRestock, upstream)} {
        
        for(auto& pool : memoryPools){
            pool.initialize_stock(bind_fetch_from_upstream());
        }
    }

    threaded_multipool(const threaded_multipool&) = delete;

    threaded_multipool(threaded_multipool&&) = delete;

    threaded_multipool& operator=(const threaded_multipool&) = delete;

    threaded_multipool& operator=(threaded_multipool&&) = delete;

    ~threaded_multipool(){
        Release();
    }

    private:


    void* do_allocate(std::size_t bytes, std::size_t alignment) override{

        if ((bytes+alignment * (alignment>defaultAlignment))>biggestPoolSize){

            return OversizedAlloc(bytes, alignment);

        }

        [[likely]] if (alignment<=defaultAlignment){
            
            return DefaultAlloc(bytes, alignment);

        } else { //guh, who are you, me?

            return OveralignedAlloc(bytes, alignment);
        }
    }

    void do_deallocate(void* p, std::size_t bytes, std::size_t alignment) override{
        if ((bytes + alignment * (alignment>defaultAlignment))>biggestPoolSize){
            return OversizedDealloc(p, bytes, alignment);
        }

        [[likely]] if (alignment<=defaultAlignment){
            
            return DefaultDealloc(p, bytes, alignment);

        } else { //guh, who are you, me?

            return OveralignedDealloc(p, bytes, alignment);
        }
    }

    bool do_is_equal(const std::pmr::memory_resource& other) const noexcept override{
        return this == &other;
    };

    

    void* OversizedAlloc(std::size_t bytes, std::size_t alignment){
        return oversizedAlloc.Allocate(bytes, alignment);
    }

    void* DefaultAlloc(std::size_t bytes, std::size_t alignment){

        size_t adjustedSize = std::bit_ceil(bytes);

        size_t poolIndex = [&]()->size_t{
            if (adjustedSize <= startSize){
                return 0;
            }
            constexpr size_t indexOffset = std::countr_zero(startSize);
            return std::countr_zero(adjustedSize) - indexOffset;
        }();

        std::byte* chunk = memoryPools[poolIndex].Allocate(bind_fetch_from_upstream());

        return static_cast<void*>(chunk);
    }

    void* OveralignedAlloc(std::size_t bytes, std::size_t alignment){

        size_t adjustedSize = std::bit_ceil(bytes+alignment);

        size_t poolIndex = [&]()->size_t{
            if (adjustedSize <= startSize){
                return 0;
            }
            constexpr size_t indexOffset = std::countr_zero(startSize);
            return std::countr_zero(adjustedSize) - indexOffset;
        }();

        std::byte* chunkStart = memoryPools[poolIndex].Allocate(bind_fetch_from_upstream());

        void* alignPtr = static_cast<void*>(chunkStart);
        std::align(alignment, bytes, alignPtr, adjustedSize);

        std::byte* retPointer = static_cast<std::byte*>(alignPtr);

        if ((adjustedSize-bytes) > sizeof(std::byte*)*2){
            new (retPointer + bytes) std::byte{1}; //pointer to head of chunk is on this side
            new (retPointer + bytes + sizeof(std::byte*)) std::byte*{chunkStart};
        } else {
            new (retPointer + bytes) std::byte{0}; //pointer is near beginning of alloc;
            new (retPointer - sizeof(std::byte*)) std::byte*{chunkStart};
        }

        return static_cast<void*>(retPointer);
    }

    void OversizedDealloc(void* p, std::size_t bytes, std::size_t alignment ){
        oversizedAlloc.Deallocate(p, bytes, alignment);
    }

    void DefaultDealloc(void* p, std::size_t bytes, std::size_t alignment){
        size_t adjustedSize = std::bit_ceil(bytes);

        size_t poolIndex = [&]()->size_t{
            if (adjustedSize <= startSize){
                return 0;
            }
            constexpr size_t indexOffset = std::countr_zero(startSize);
            return std::countr_zero(adjustedSize) - indexOffset;
        }();

        memoryPools[poolIndex].Deallocate(static_cast<std::byte*>(p), bind_fetch_from_upstream());
    }

    void OveralignedDealloc(void* p, std::size_t bytes, std::size_t alignment ){
        size_t adjustedSize = std::bit_ceil(bytes+alignment);

        size_t poolIndex = [&]()->size_t{
            if (adjustedSize <= startSize){
                return 0;
            }
            constexpr size_t indexOffset = std::countr_zero(startSize);
            return std::countr_zero(adjustedSize) - indexOffset;
        }();

        std::byte* allocStart = static_cast<std::byte*>(p);
        std::byte* pointerLoc = std::launder(allocStart + bytes); //do I even really need this?


        if (*pointerLoc == std::byte{1}){ //pointer to head of chunk is on this side
            std::byte** chunkStartLocation = std::launder(PtrCast<std::byte**>(pointerLoc) + 1);
            std::byte* chunkStart = *chunkStartLocation;
            memoryPools[poolIndex].Deallocate(chunkStart, bind_fetch_from_upstream());
            return;
        } else if(*pointerLoc == std::byte{0}){ //pointer is near beginning of alloc;
            std::byte** chunkStartLocation = std::launder(PtrCast<std::byte**>(allocStart) - 1);
            std::byte* chunkStart = *chunkStartLocation;
            memoryPools[poolIndex].Deallocate(chunkStart, bind_fetch_from_upstream());
            return;
        } else [[unlikely]] {
            // only here due to memory corruption
            std::cerr << "ann::threaded_multipool has detected memory corruption" << std::endl;
            std::abort();
        }

    }

    public:
    void Release(){
        auto returnToUpstream = [&, this] (std::pmr::vector<MemoryChunk>& poolChunks){
            for(auto& chunk : poolChunks){
                this->upstreamAlloc.deallocate_bytes(chunk.start, chunk.size, defaultAlignment);
            }
        };
        for(auto& pool : memoryPools){
            pool.Release(returnToUpstream);
        }
    }

    private:

    std::pmr::polymorphic_allocator<> upstreamAlloc;
    std::mutex upstreamLock;
    oversized_handler oversizedAlloc{upstreamAlloc.resource(), upstreamLock};
    std::array<array_pool, numberOfPools> memoryPools;
    //Add in some sort of flexible memory cache to handle oversized allocs
};








}

#endif