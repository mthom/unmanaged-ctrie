#ifndef IDENTITY_HPP_INCLUDED
#define IDENTITY_HPP_INCLUDED

#include "simple_vector.hpp"

namespace kl_ctrie
{    
  template <class T>
  class identity {
    T data;

  public:  
    template <typename... Ts>
    identity(Ts&&... items) : data(std::forward<Ts>(items)...)
    {}
    
    inline operator const T&() const {
      return data;
    }
  };
  
  template <class T>
  class identity<T*>
  {
    T* data;

  public:
    template <typename... Ts>
    identity(Ts&&... items) : data(std::forward<Ts>(items)...)
    {}
    
    identity(T* data_) : data(data_) {}
  
    inline operator T*() {
      return data;
    }

    identity<T*>& operator=(T*) = delete;
    
    inline identity<T*>& write(void*, T* data_)
    {
      data = data_;
      return *this;
    }

    inline T* get() {
      return data;
    }
    
    inline T* const operator->() const {
      return data;
    }    

    inline bool operator==(const identity<T*>& it) const
    {
      return data == it.data;
    }
    
    inline bool operator!=(const identity<T*>& it) const
    {
      return data != it.data;
    }

    inline bool null() const
    {
      return data == nullptr;
    }
  };

  template <class T>
  class identity<std::atomic<T*>>
  {
  private:
    std::atomic<T*> data;

  public:
    identity(T* data_) : data(data_) {}

    inline bool operator==(T* const t) const
    {
      return data == t;
    }
    
    inline T* load(std::memory_order mem) const
    {
      return data.load(mem);
    }

    inline void store(void*, T* val, std::memory_order mem)
    {
      data.store(val, mem);
    }

    inline bool compare_exchange_strong(void*,
					T*& expected,
					T* desired,
					std::memory_order success,
					std::memory_order failure)
    {
      return data.compare_exchange_strong(expected, desired, success, failure);
    }
  };

  template <typename T>
  class branch_vector_allocator {};

  template <typename T>
  class branch_vector_allocator<identity<T>>
  {
  public:
    using value_type = identity<T>;

    branch_vector_allocator() = default;

    identity<T>* allocate(std::size_t n)
    {
      std::allocator<identity<T>> a;
      return a.allocate(n);
    }

    void deallocate(identity<T>*, size_t) {}
  };
}
#endif
