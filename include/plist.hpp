#ifndef PLIST_HPP_INCLUDED
#define PLIST_HPP_INCLUDED

#include <atomic>
#include <functional>
#include <iterator>

using namespace std;

// lock-free concurrent persistent lists. plist is expected to
// interact with some mode of garbage collection via the allocator,
// which is why it never deletes anything.
template <typename T>
struct plist_node {
  const T data;
  plist_node* next;

  template <typename U, typename... Us>
  plist_node(U&& arg, Us&&... args, plist_node* const next = nullptr)
    : data(std::forward<U>(arg), std::forward<Us>(args)...)
    , next(next)
  {}
};

template <typename T,
	  template <class> class Alloc>
class plist
{
private:
  std::atomic<plist_node<T>*> head;

  explicit plist(plist_node<T>* head)
    : head(head)
  {}  
public:
  class iterator : public std::iterator<std::forward_iterator_tag, const plist_node<T>>
  {
  private:
    plist_node<T> const* ptr;

  public:
    iterator(const iterator& it)
      : ptr(it.ptr)
    {}
    
    iterator(plist_node<T> const* ptr) : ptr(ptr)
    {}

    inline iterator operator++(int) {
      return iterator { ptr->next };
    }

    inline iterator& operator++() {
      ptr = ptr->next;
      return *this;
    }

    inline const T& operator*() {
      return ptr->data;
    }

    inline bool operator==(const iterator& it) {
      return ptr == it.ptr;
    }

    inline bool operator!=(const iterator& it) {
      return ptr != it.ptr;
    }

    inline iterator& operator=(const iterator& it) {
      ptr = it.ptr;
    }
  };

  inline plist& operator=(const plist& pl) {    
    plist_node<T>* const new_head = pl.head.load(std::memory_order_relaxed);
    head.store(new_head, std::memory_order_relaxed);

    return *this;
  }
  
  iterator begin() const {
    return iterator { head.load(std::memory_order_relaxed) };
  }

  iterator end() const {
    return iterator { nullptr };
  }
  
  plist() : head(nullptr)
  {}  
  
  plist(const plist& pl)
    : head(pl.head.load(std::memory_order_relaxed))
  {}

  plist(std::initializer_list<T> init)
  {
    if(init.size() != 0) {
      plist_node<T>* cur_head;
      plist_node<T>** next = &cur_head;

      Alloc<plist_node<T>> a;
      
      for(auto&& item : init) {
	void* ptr = a.allocate(1);
	*next = new(ptr) plist_node<T>(item);
	next = &((*next)->next);
      }

      head.store(cur_head, std::memory_order_relaxed);
    }
  }
  
  inline const T& car() const
  {
    return head.load(std::memory_order_relaxed)->data;
  }
  
  inline plist cdr() const
  {
    return plist(head.load(std::memory_order_relaxed)->next);
  }
  
  inline bool empty() const
  {
    return head.load(std::memory_order_relaxed) == nullptr;
  }

  template <typename... Us>
  plist push_front(Us&&... args) const
  {
    Alloc<plist_node<T>> a;
    plist_node<T>* const curr_head = head.load(std::memory_order_relaxed);
    void* ptr = a.allocate(1);

    return plist(new(ptr) plist_node<T>(std::forward<Us>(args)..., curr_head));
  }

  size_t size() const
  {
    plist_node<T>* curr_node = head.load(std::memory_order_relaxed);
    size_t sz = 0;
    
    while(curr_node) {
      ++sz;
      curr_node = curr_node->next;
    }

    return sz;
  }

  T const* find_first_by(std::function<bool(const T&)> pred) const
  {
    plist_node<T>* curr_node = head.load(std::memory_order_relaxed);

    while(curr_node) {
      if(pred(curr_node->data))
	return &static_cast<T const&>(curr_node->data);

      curr_node = curr_node->next;
    }

    return nullptr;
  }  
  
  plist remove_first_by(std::function<bool(const T&)> pred) const
  {
    plist_node<T>* new_head = head.load(std::memory_order_relaxed);

    if(!new_head)
      return plist();

    if(pred(new_head->data))
      return cdr();

    Alloc<plist_node<T>> a;
    
    void* p = a.allocate(1);
    plist_node<T>* new_node = new(p) plist_node<T>(new_head->data);
    plist_node<T>** new_node_tail = &new_node->next;
    
    for(plist_node<T>* curr_node = new_head->next;
	curr_node != nullptr;
	curr_node = curr_node->next)
    {
      if(pred(curr_node->data)) {
	*new_node_tail = curr_node->next;
	return plist(new_node);
      } else {
	p = a.allocate(sizeof(plist_node<T>));
	*new_node_tail = new(p) plist_node<T>(curr_node->data);
	new_node_tail = &(*new_node_tail)->next;
      }
    }

    return plist(new_node);
  }

  void remove_all()
  {
    plist_node<T>* n = head.exchange(nullptr, std::memory_order_relaxed);
    plist_node<T>* nnext;

    while(n) {
      nnext = n->next;
      delete n;
      n = nnext;
    }
  }
};
#endif
