#ifndef SIMPLE_VECTOR_HPP_INCLUDED
#define SIMPLE_VECTOR_HPP_INCLUDED

#include <cassert>
#include <cstring>

template <typename T, class Alloc>
class simple_vector
{  
private:
  size_t sz, cap;
  T* data_;

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
      it = reinterpret_cast<T*>(reinterpret_cast<size_t>(it) + sizeof(T));
      return result;
    }

    iterator& operator++() {
      it = reinterpret_cast<T*>(reinterpret_cast<size_t>(it) + sizeof(T));
      return *this;
    }

    inline bool operator==(const iterator& iter) const {
      return it == iter.it;
    }

    inline bool operator!=(const iterator& iter) const {
      return it != iter.it;
    }

    inline iterator operator+(uint64_t i) const {
      return iterator { reinterpret_cast<T*>(reinterpret_cast<size_t>(it) + i * sizeof(T)) };
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

  simple_vector() : sz(0), cap(0), data_(nullptr) {}
  
  simple_vector(const size_t n)
    : sz(n)
    , cap(2 * n)
    , data_(Alloc().allocate(cap))
  {}
  
  simple_vector(size_t n, const T& init)
    : sz(n)
    , cap(2 * n)
    , data_(Alloc().allocate(cap))
  {
    for(size_t i = 0; i < n; ++i)
      new(data_[i]) T(init);
  }
  
  simple_vector(simple_vector&& v)
    : sz(v.sz)
    , cap(v.cap)
    , data_(v.data_)
  {
    v.sz = v.cap = 0;
    v.data_ = nullptr;
  }

  simple_vector(std::initializer_list<T> lst)
    : sz(0)
    , cap(2 * lst.size())
    , data_(Alloc().allocate(cap))
  {        
    for(auto& t : lst)
      push_back(t);
  }
  
  simple_vector(const simple_vector& v)
    : sz(v.sz)
    , cap(v.cap)
    , data_(Alloc().allocate(cap))
  {
    std::memcpy(reinterpret_cast<void*>(data_),
		reinterpret_cast<const void*>(v.data_),
		sz * sizeof(T));
  }

  simple_vector& operator=(simple_vector&& v)
  {
    sz = v.sz;
    cap = v.cap;
    data_ = v.data_;

    v.sz = v.cap = 0;
    v.data_ = nullptr;

    return *this;
  }
  
  simple_vector& operator=(const simple_vector& v)
  {
    sz = v.sz;
    cap = v.cap;
    data_ = v.data_;

    return *this;
  }
  
  bool operator==(const simple_vector& v) const
  {
    if(sz != v.sz)
      return false;

    for(size_t i = 0; i < sz; ++i)
      if(data_[i] != v[i])
	return false;

    return true;
  }

  inline bool operator!=(const simple_vector& v) const
  {
    return !(this->operator==(v));
  }
  
  inline iterator begin() {
    return iterator { data_ };
  }

  inline iterator end() {
    return iterator { reinterpret_cast<T*>(reinterpret_cast<size_t>(data_) + sz * sizeof(T)) };
  }
  
  void reserve(size_t new_cap)
  {    
    assert(new_cap >= sz);

    cap = new_cap;
    T* new_data_ = Alloc().allocate(new_cap);

    std::memcpy(reinterpret_cast<void*>(new_data_),
		reinterpret_cast<const void*>(data_),
		sz * sizeof(T));

    data_ = new_data_;
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

  inline T* data()
  {
    return data_;
  }
  
  inline T pop_back()
  {
    T result(data_[sz-1]);
    --sz;
    return result;
  }  
  
  void push_back(const T& t)
  {
    if(sz == cap)
      reserve(2 * cap);

    data_[sz++] = t;
  }

  inline T& back() {
    return data_[sz-1];
  }

  inline T& front() {
    return data_[0];
  }

  inline T& operator[](size_t i) {
    return data_[i];
  }

  inline const T& operator[](size_t i) const {
    return data_[i];
  }
};

#endif
