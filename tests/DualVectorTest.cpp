#include <array>
#include <algorithm>
#include <concepts>
#include <cstdint>
#include <functional>
#include <memory_resource>
#include <ranges>
#include <utility>

#include "catch2/catch_test_macros.hpp"

#include "ann/AlignedMemory/DualVector.hpp"

static_assert(std::ranges::input_range<ann::dual_vector<float, float>>);

using test_iter = ann::dual_vector_iterator<float *, float*>;

static_assert(std::common_reference_with<
      std::iter_reference_t<test_iter>&&, std::iter_value_t<test_iter>&
    > );
using common_ref = std::common_reference_t<ann::dual_vector_reference<float&, float&>&&, std::pair<float,float>&>;

static_assert(std::convertible_to<
      std::iter_reference_t<test_iter>&&, common_ref
    > );


static_assert(std::common_reference_with<
      std::iter_reference_t<test_iter>&&, std::iter_rvalue_reference_t<test_iter>&&
    > );

static_assert(std::common_reference_with<
      std::iter_rvalue_reference_t<test_iter>&&, const std::iter_value_t<test_iter>&
    >);

TEST_CASE( "Push back and insert", "[dual_vector]"){
    ann::dual_vector<float, float> two_d_vecs{};
    REQUIRE(two_d_vecs.size() == 0);
    REQUIRE(two_d_vecs.capacity() == 0);

    two_d_vecs.push_back(std::pair{1.4f, 2.5f});

    REQUIRE(two_d_vecs.size() == 1);
    REQUIRE(two_d_vecs.capacity() >= 1);
    //std::pair<float, float> back_pair = two_d_vecs.back();
    REQUIRE(two_d_vecs.back() == std::pair{1.4f, 2.5f});

    two_d_vecs.push_back(std::pair{1.4f, 2.5f});
    two_d_vecs.push_back(std::pair{1.4f, 2.5f});
    two_d_vecs.push_back(std::pair{1.4f, 2.5f});
    two_d_vecs.push_back(std::pair{1.4f, 2.5f});

    REQUIRE(two_d_vecs.size() == 5);
    REQUIRE(two_d_vecs.capacity() >= 5);

    two_d_vecs.insert(two_d_vecs.end(), 3.4f, 3.1f);
    REQUIRE(two_d_vecs.size() == 6);
    
    
    REQUIRE(two_d_vecs.back() == std::pair{3.4f, 3.1f});

    two_d_vecs.insert(two_d_vecs.begin()+3, 5.1f, 6.7f);
    REQUIRE(two_d_vecs.size() == 7);

    
    REQUIRE(two_d_vecs[3] == std::pair{5.1f, 6.7f});

    std::array<std::pair<float, float>, 7> expected{
        std::pair{1.4f, 2.5f},
        std::pair{1.4f, 2.5f},
        std::pair{1.4f, 2.5f},
        std::pair{5.1f, 6.7f},
        std::pair{1.4f, 2.5f},
        std::pair{1.4f, 2.5f},
        std::pair{3.4f, 3.1f}
    };

    REQUIRE(std::ranges::equal(two_d_vecs, expected));
}

TEST_CASE( "Memory layout", "[dual_vector]"){
    ann::dual_vector<std::uint16_t, std::uint64_t> uneven_sizes{};
    uneven_sizes.push_back(std::pair<std::uint16_t, std::uint64_t>{1, 64363});

    REQUIRE(std::less<>{}((void *) &uneven_sizes.front().second, (void *) &uneven_sizes.front().first));
    REQUIRE(reinterpret_cast<std::uintptr_t>(&uneven_sizes.front().second) % alignof(std::uint64_t) == 0);
    REQUIRE(reinterpret_cast<std::uintptr_t>(&uneven_sizes.front().first) % alignof(std::uint16_t) == 0);
}


TEST_CASE( "Copy, move, and list init", "[dual_vector]"){
    ann::dual_vector<std::uint16_t, std::uint64_t> first_vec = {{134, 253446}, {135, 252446}, {34, 25346}};
    ann::dual_vector<std::uint16_t, std::uint64_t> second_vec = first_vec;
    
    REQUIRE(first_vec == second_vec);

    ann::dual_vector<std::uint16_t, std::uint64_t> third_vec = {{14, 35263512}, {72, 5673}, {14, 35263512}, {72, 5673}};
    second_vec = third_vec;

    REQUIRE(second_vec == third_vec);

    ann::dual_vector<std::uint16_t, std::uint64_t> fourth_vec = std::move(first_vec);
    second_vec = ann::dual_vector<std::uint16_t, std::uint64_t>{{134, 253446}, {135, 252446}, {34, 25346}};

    REQUIRE(second_vec == fourth_vec);
}

TEST_CASE( "Nested data structures with std::allocator", "[dual_vector]"){
    using nested_vecs = ann::dual_vector<std::vector<std::int32_t>, std::vector<std::int64_t>>;
    nested_vecs first_vec = {{{435, 2351, 53}, {-235, 43562, 32435, 5234}}, {{43, 24351, 73, 365}, {235, -4562, 32535}}};
    nested_vecs second_vec = first_vec;

    REQUIRE(first_vec == second_vec);

    nested_vecs third_vec = {
        {
            {435, 2351, 53, 3365}, 
            {-235, -43722, 632435, 524}
        }, 
        {
            {43, 24351, 73, 365},
            {235, -4562, 32535}
        },
        {
            {9403, 34895, -589342, 39054, -23523},
            {543525}
        }
    };

    second_vec = third_vec;

    REQUIRE(second_vec == third_vec);

    nested_vecs fourth_vec = std::move(first_vec);
    second_vec = nested_vecs{{{435, 2351, 53}, {-235, 43562, 32435, 5234}}, {{43, 24351, 73, 365}, {235, -4562, 32535}}};

    REQUIRE(second_vec == fourth_vec);

}

constexpr auto check_allocator = [](auto&& outer_allocator){
    return [=](const auto& inner_container){
        return inner_container.get_allocator() == outer_allocator;
    };
};

enum class vector_half{
    first,
    second
};

template<vector_half Half>
bool check_allocator_scoping(auto&& dual_vec){
    if constexpr (Half == vector_half::first){
        return std::ranges::all_of(
            dual_vec.view_first(),
            check_allocator(dual_vec.get_allocator())
        );
    } else {
        return std::ranges::all_of(
            dual_vec.view_second(),
            check_allocator(dual_vec.get_allocator())
        );
    }
}

TEST_CASE( "Nested data structures with std::pmr::polymorphic_allocator", "[dual_vector]"){
    using nested_vecs = ann::pmr::dual_vector<std::pmr::vector<std::int32_t>, std::pmr::vector<std::int64_t>>;
    std::pmr::monotonic_buffer_resource first_resource{};
    nested_vecs first_vec = nested_vecs({{{435, 2351, 53}, {-235, 43562, 32435, 5234}}, {{43, 24351, 73, 365}, {235, -4562, 32535}}}, &first_resource);

    REQUIRE(
        check_allocator_scoping<vector_half::first>(first_vec)
    );

    REQUIRE(
        check_allocator_scoping<vector_half::second>(first_vec)
    );

    nested_vecs second_vec(first_vec, &first_resource);

    REQUIRE(
        check_allocator_scoping<vector_half::first>(second_vec)
    );

    REQUIRE(
        check_allocator_scoping<vector_half::second>(second_vec)
    );

    REQUIRE(first_vec == second_vec);

    nested_vecs third_vec = {
        {
            {435, 2351, 53, 3365}, 
            {-235, -43722, 632435, 524}
        }, 
        {
            {43, 24351, 73, 365},
            {235, -4562, 32535}
        },
        {
            {9403, 34895, -589342, 39054, -23523},
            {543525}
        }
    };

    second_vec = third_vec;

    REQUIRE(second_vec == third_vec);
    REQUIRE(second_vec.get_allocator() != third_vec.get_allocator());
    REQUIRE(
        check_allocator_scoping<vector_half::first>(second_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::second>(second_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::first>(third_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::second>(third_vec)
    );

    nested_vecs fourth_vec{std::move(first_vec), {}};
    second_vec = nested_vecs{{{435, 2351, 53}, {-235, 43562, 32435, 5234}}, {{43, 24351, 73, 365}, {235, -4562, 32535}}};

    REQUIRE(second_vec == fourth_vec);
    REQUIRE(second_vec.get_allocator() != fourth_vec.get_allocator());
    REQUIRE(
        check_allocator_scoping<vector_half::first>(second_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::second>(second_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::first>(fourth_vec)
    );
    REQUIRE(
        check_allocator_scoping<vector_half::second>(fourth_vec)
    );
}