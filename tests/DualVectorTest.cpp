#include <array>
#include <algorithm>
#include <concepts>
#include <cstdint>
#include <functional>
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

    two_d_vecs.insert(two_d_vecs.end(), 3.4, 3.1);
    REQUIRE(two_d_vecs.size() == 6);
    
    
    REQUIRE(two_d_vecs.back() == std::pair{3.4f, 3.1f});

    two_d_vecs.insert(two_d_vecs.begin()+3, 5.1, 6.7);
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
    
    REQUIRE(std::ranges::equal(first_vec, second_vec));

}