#ifndef SIMPLE_VECTOR_HPP_INCLUDED
#define SIMPLE_VECTOR_HPP_INCLUDED

#include <cassert>
#include <cstring>

template <typename T, class Alloc>
class simple_vector
{  
private:
  size_t sz, cap;
  T* data;

  class iterator {
  private:
    T* it;

  public:
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::forward_iterator_tag;
    using reference = T&;
    using pointer = T*;
    using value_type = T;
    
    iterator(T* it) : it(it) {}

    iterator(const iterator& iter) : it(iter.it) {}
    
    iterator& operator++(int) {
      auto result = iterator { it };
      it = &it[1];
      return result;
    }

    iterator& operator++() {
      it = &it[1];
      return *this;
    }

    inline bool operator==(const iterator& iter) const {
      return it == iter.it;
    }

    inline bool operator!=(const iterator& iter) const {
      return it != iter.it;
    }

    inline iterator operator+(uint64_t i) const {
      return iterator { &it[i] };
    }
    
    inline iterator& operator=(iterator&& iter) {
      it = iter.it;
      iter.it = nullptr;
      return *this;
    }
    
    inline iterator& operator=(const iterator& iter) {
      it = iter.it;
      return *this;
    }
    
    inline T& operator*() {
      return *it;
    }

    inline const T& operator*() const {
      return *it;
    }
  };
  
public:
  using value_type = T;

  simple_vector() : sz(0), cap(0), data(nullptr) {}
  
  simple_vector(const size_t n)
    : sz(n)
    , cap(2 * n)
    , data(Alloc().allocate(cap * sizeof(T)))
  {}
  
  simple_vector(size_t n, const T& init)
    : sz(n)
    , cap(2 * n)
    , data(Alloc().allocate(cap * sizeof(T)))
  {
    for(size_t i = 0; i < n; ++i)
      new(data[i]) T(init);
  }
  
  simple_vector(simple_vector&& v)
    : sz(v.sz)
    , cap(v.cap)
    , data(v.data)
  {
    v.sz = v.cap = 0;
    v.data = nullptr;
  }

  simple_vector(std::initializer_list<T> lst)
    : sz(0)
    , cap(2 * lst.size())
    , data(Alloc().allocate(cap * sizeof(T)))
  {        
    for(auto& t : lst)
      push_back(t);
  }
  
  simple_vector(const simple_vector& v)
    : sz(v.sz)
    , cap(v.cap)
    , data(Alloc().allocate(cap * sizeof(T)))
  {
    std::memcpy(reinterpret_cast<void*>(data),
		reinterpret_cast<const void*>(v.data),
		sz * sizeof(T));
  }

  simple_vector& operator=(simple_vector&& v)
  {
    sz = v.sz;
    cap = v.cap;
    data = v.data;

    v.sz = v.cap = 0;
    v.data = nullptr;

    return *this;
  }
  
  simple_vector& operator=(const simple_vector& v)
  {
    sz = v.sz;
    cap = v.cap;
    data = v.data;

    return *this;
  }
  
  bool operator==(const simple_vector& v) const
  {
    if(sz != v.sz)
      return false;

    for(size_t i = 0; i < sz; ++i)
      if(data[i] != v[i])
	return false;

    return true;
  }

  inline bool operator!=(const simple_vector& v) const
  {
    return !(this->operator==(v));
  }
  
  inline iterator begin() {
    return iterator { data };
  }

  inline iterator end() {
    return iterator { &data[sz] };
  }
  
  void reserve(size_t new_cap)
  {    
    assert(new_cap >= sz);

    cap = new_cap;
    T* new_data = Alloc().allocate(new_cap * sizeof(T));

    std::memcpy(reinterpret_cast<void*>(new_data),
		reinterpret_cast<const void*>(data),
		sz * sizeof(T));

    data = new_data;
  }

  inline size_t size() const {
    return sz;
  }

  inline size_t capacity() const {
    return cap;
  }

  inline bool empty() const {
    return sz == 0;
  }
  
  inline T pop_back()
  {
    T result(data[sz-1]);
    --sz;
    return result;
  }
  
  void push_back(const T& t)
  {
    if(sz == cap)
      reserve(2 * cap);

    data[sz++] = t;
  }

  inline T& back() {
    return data[sz-1];
  }

  inline T& front() {
    return data[0];
  }

  inline T& operator[](size_t i) {
    return data[i];
  }

  inline const T& operator[](size_t i) const {
    return data[i];
  }
};

#endif
