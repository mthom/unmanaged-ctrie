#ifndef TEST_CTRIE_HPP_INCLUDED
#define TEST_CTRIE_HPP_INCLUDED

#include "ctrie.hpp"
#include "gtest/gtest.h"
#include "ref_string.hpp"

using namespace kl_ctrie;

template <typename T>
struct local_hash
{
  using result_type = size_t;
    
  inline size_t operator()(const T& s) const
  {
    size_t hash = 0;
    for(auto it = s.cbegin(), end = s.cend(); it != end; ++it)
      hash ^= (*it) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
    
    return hash;
  }
};

class ctrie_tests : public ::testing::Test
{
protected:
  ctrie<ref_string<>, int, local_hash<ref_string<>>, std::allocator, kl_ctrie::identity> ct;
  
  virtual void SetUp()
  {
    for(char c = 'a'; c <= 'z'; ++c) {
      for(int i = 1; i < 65; ++i) {
	ct.insert(ref_string<>(i, c), i);
      }
    }
  }
};
#endif
