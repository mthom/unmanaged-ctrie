#ifndef CTRIE_HPP_INCLUDED
#define CTRIE_HPP_INCLUDED

#include <atomic>
#include <cassert>
#include <iostream>
#include <limits>
#include <memory>
#include <vector>

#include "identity.hpp"
#include "plist.hpp"
#include "simple_vector.hpp"

namespace kl_ctrie
{  
  template <template <class> class Alloc, class T, class... Ts>
  inline T* cons(Ts&&... args)
  {
    Alloc<T> a;
    T* ptr = a.allocate(1);
    return new(ptr) T(std::forward<Ts>(args)...);
  }

  inline uint64_t population_count64(uint64_t w)
  {
    w -= (w >> 1) & 0x5555555555555555ULL;
    w = (w & 0x3333333333333333ULL) + ((w >> 2) & 0x3333333333333333ULL);
    w = (w + (w >> 4)) & 0x0f0f0f0f0f0f0f0fULL;
    return (w * 0x0101010101010101ULL) >> 56;
  }

  class gen {
  private:
    struct inner_gen {};

    std::shared_ptr<inner_gen> inner_g;
  public:
    gen() : inner_g(std::make_shared<inner_gen>()) {}
    gen(gen& g) : inner_g(g.inner_g) {}
    gen(const gen& g) : inner_g(g.inner_g) {}

    gen& operator=(gen&) = delete;
    gen& operator=(const gen&) = delete;
    gen& operator=(gen&&) = delete;

    inline bool operator==(const gen& g) const {
      return inner_g.get() == g.inner_g.get();
    }
  };

  template <typename V>
  class ctrie_query_result
  {
  private:
    bool redo;
    const V* val;

    ctrie_query_result(bool redo, const V* val)
      : redo(redo), val(val)
    {}
  public:
    static const ctrie_query_result<V> restart;
    static const ctrie_query_result<V> not_found;

    ctrie_query_result(const V* val = nullptr)
      : redo(false), val(val)
    {}

    inline bool operator==(const ctrie_query_result& q) const
    {
      return redo == q.redo && val == q.val;
    }

    inline bool operator!=(const ctrie_query_result& q) const
    {
      return redo != q.redo || val != q.val;
    }

    inline ctrie_query_result& operator=(const V* val_)
    {
      val = val_;
      return *this;
    }

    inline ctrie_query_result& operator=(const ctrie_query_result& q)
    {
      val = q.val;
      redo = q.redo;
      return *this;
    }

    inline operator const V*()
    {
      return val;
    }
  };

  template <typename V>
  const ctrie_query_result<V> ctrie_query_result<V>::restart = ctrie_query_result<V>(true, nullptr);
  template <typename V>
  const ctrie_query_result<V> ctrie_query_result<V>::not_found = ctrie_query_result<V>(false, nullptr);

  template <typename, typename, class, template <class> class, template <class> class>
  class ctrie;

  template <typename, typename, class, template <class> class, template <class> class>
  struct inode;

  template <typename, typename, class, template <class> class, template <class> class>
  struct cnode;

  template <typename, typename, class, template <class> class, template <class> class>
  struct tnode;

  template <typename, typename, class, template <class> class, template <class> class>
  struct lnode;

  template <typename, typename, class, template <class> class, template <class> class>
  struct prev_node {
    virtual ~prev_node() {}
    virtual void* derived_ptr() = 0;
  };

  template <typename, typename, class, template <class> class, template <class> class>
  struct branch;

  // change to use custom vector types.
  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  using branch_vector = simple_vector<Barrier<branch<K, V, Hash, Alloc, Barrier>*>,
				      branch_vector_allocator<Barrier<branch<K, V, Hash, Alloc, Barrier>*>>>;				      
  
  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct main_node : prev_node<K, V, Hash, Alloc, Barrier> {
    Barrier<std::atomic<prev_node<K, V, Hash, Alloc, Barrier>*>> prev;

    main_node() : prev(nullptr) {}

    virtual ~main_node() {}

    virtual branch<K, V, Hash, Alloc, Barrier>* in_resurrect(inode<K, V, Hash, Alloc, Barrier>*) = 0;

    virtual ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>&,
					 K&,
					 typename Hash::result_type,
					 int,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 gen) = 0;
    virtual bool insert(ctrie<K, V, Hash, Alloc, Barrier>&,
			K&,
			V&,
			typename Hash::result_type,
			int,
			inode<K, V, Hash, Alloc, Barrier>*,
			inode<K, V, Hash, Alloc, Barrier>*,
			gen) = 0;
    virtual ctrie_query_result<V> remove(ctrie<K, V, Hash, Alloc, Barrier>&,
					 K&,
					 V*,
					 typename Hash::result_type,
					 int,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 gen) = 0;
    virtual void* derived_ptr() = 0;
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct failure : prev_node<K, V, Hash, Alloc, Barrier> {
    Barrier<main_node<K, V, Hash, Alloc, Barrier>*> prev;

    failure(Barrier<main_node<K, V, Hash, Alloc, Barrier>*> prev_)
      : prev(prev_)
    {}

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct branch {
    virtual ~branch() {}
    virtual main_node<K, V, Hash, Alloc, Barrier>* in_to_contracted(bool&) = 0;
    virtual void in_renewed(gen,
			    ctrie<K, V, Hash, Alloc, Barrier>&,
			    Barrier<branch<K, V, Hash, Alloc, Barrier>*>&,
			    cnode<K, V, Hash, Alloc, Barrier>*) = 0;
    virtual ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>&,
					 K&,
					 typename Hash::result_type,
					 int,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 inode<K, V, Hash, Alloc, Barrier>*,
					 cnode<K, V, Hash, Alloc, Barrier>*,
					 gen) = 0;
    virtual void compressed(ctrie<K, V, Hash, Alloc, Barrier>&,
			    Barrier<branch<K, V, Hash, Alloc, Barrier>*>&,
			    cnode<K, V, Hash, Alloc, Barrier>* cn) = 0;
    virtual void* derived_ptr() = 0;
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct snode : branch<K, V, Hash, Alloc, Barrier> {
    const K k;
    const V v;
    typename Hash::result_type const hc;

    snode(const K& k_, const V& v_, typename Hash::result_type const hc_)
      : k(k_), v(v_), hc(hc_)
    {}

    ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>&,
				 K& k_,
				 typename Hash::result_type hc_,
				 int,
				 inode<K, V, Hash, Alloc, Barrier>*,
				 inode<K, V, Hash, Alloc, Barrier>*,
				 cnode<K, V, Hash, Alloc, Barrier>*,
				 gen)
    {
      if(hc == hc_ && k == k_) {
	return &v;
      } else
	return ctrie_query_result<V>::not_found;
    }

    void in_renewed(gen,
		    ctrie<K, V, Hash, Alloc, Barrier>&,
		    Barrier<branch<K, V, Hash, Alloc, Barrier>*>& narr,
		    cnode<K, V, Hash, Alloc, Barrier>* cn)
    {
      narr.write(reinterpret_cast<void*>(cn), this);
    }

    void compressed(ctrie<K, V, Hash, Alloc, Barrier>&,
		    Barrier<branch<K, V, Hash, Alloc, Barrier>*>& b,
		    cnode<K, V, Hash, Alloc, Barrier>* cn)
    {
      b.write(reinterpret_cast<void*>(cn), this);
    }

    tnode<K, V, Hash, Alloc, Barrier>* copy_tombed()
    {
      return cons<Alloc, tnode<K, V, Hash, Alloc, Barrier>>(this);
    }

    main_node<K, V, Hash, Alloc, Barrier>* in_to_contracted(bool& in_snode)
    {
      in_snode = true;
      return copy_tombed();
    }

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct tnode : main_node<K, V, Hash, Alloc, Barrier> {
    const snode<K, V, Hash, Alloc, Barrier>* sn;

    tnode(const snode<K, V, Hash, Alloc, Barrier>* sn_)
      : sn(sn_)
    {}

    inline snode<K, V, Hash, Alloc, Barrier>* copy_untombed()
    {
      return cons<Alloc, snode<K, V, Hash, Alloc, Barrier>>(sn->k, sn->v, sn->hc);
    }

    ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>& ct,
				 K& k,
				 typename Hash::result_type hc,
				 int lev,
				 inode<K, V, Hash, Alloc, Barrier>* i,
				 inode<K, V, Hash, Alloc, Barrier>* parent,
				 gen)
    {
      if(!ct.is_read_only()) {
	i->clean(parent, ct, lev - 6);
	return ctrie_query_result<V>::restart;
      } else {
	if(sn->hc == hc && sn->k == k)
	  return &sn->v;
	else
	  return ctrie_query_result<V>::not_found;
      }
    }

    bool insert(ctrie<K, V, Hash, Alloc, Barrier>& ct,
		K&,
		V&,
		typename Hash::result_type,
		int lev,
		inode<K, V, Hash, Alloc, Barrier>* i,
		inode<K, V, Hash, Alloc, Barrier>* parent,
		gen)
    {
      i->clean(parent, ct, lev - 6);
      return false;
    }

    ctrie_query_result<V> remove(ctrie<K, V, Hash, Alloc, Barrier>& ct,
					  K&,
					  V*,
					  typename Hash::result_type,
					  int lev,
					  inode<K, V, Hash, Alloc, Barrier>* i,
					  inode<K, V, Hash, Alloc, Barrier>* parent,
					  gen)
    {
      i->clean(parent, ct, lev - 6);
      return ctrie_query_result<V>::restart;
    }

    branch<K, V, Hash, Alloc, Barrier>* in_resurrect(inode<K, V, Hash, Alloc, Barrier>*)
    {
      return copy_untombed();
    }

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  branch<K, V, Hash, Alloc, Barrier>* resurrect(inode<K, V, Hash, Alloc, Barrier>* inode,
						main_node<K, V, Hash, Alloc, Barrier>* inodemain)
  {
    return inodemain->in_resurrect(inode);
  }

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct cnode : main_node<K, V, Hash, Alloc, Barrier> {
    uint64_t bmp;
    branch_vector<K, V, Hash, Alloc, Barrier> arr;
    const gen g;

    cnode(uint64_t bmp_, const gen& g_)
      : bmp(bmp_), g(g_)
    {}
    
    cnode(uint64_t bmp_, branch_vector<K, V, Hash, Alloc, Barrier>&& arr_, const gen& g_)
      : bmp(bmp_), arr(std::move(arr_)), g(g_)
    {}

    cnode<K, V, Hash, Alloc, Barrier>* updated_at(uint64_t pos,
						  branch<K, V, Hash, Alloc, Barrier>* nn,
						  gen g_)
    {
      branch_vector<K, V, Hash, Alloc, Barrier> narr(arr);
      
      auto cn = cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp, g_);

      narr[pos].write(reinterpret_cast<void*>(cn), nn);
      cn->arr = std::move(narr);
      return cn;
    }

    cnode<K, V, Hash, Alloc, Barrier>* removed_at(uint64_t pos, uint64_t flag, gen g_)
    {
      branch_vector<K, V, Hash, Alloc, Barrier> narr;

      narr.reserve(arr.size() - 1);
      std::copy(arr.begin(),
		arr.begin() + pos,
		std::back_inserter<decltype(arr)>(narr));
      std::copy(arr.begin() + pos + 1,
		arr.end(),
		std::back_inserter<decltype(arr)>(narr));

      return cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp ^ flag, std::move(narr), g_);
    }

    cnode<K, V, Hash, Alloc, Barrier>* inserted_at(uint64_t pos,
						   uint64_t flag,
						   branch<K, V, Hash, Alloc, Barrier>* nn,
						   gen g_)
    {
      branch_vector<K, V, Hash, Alloc, Barrier> narr;
      narr.reserve(arr.size() + 1);

      std::copy(arr.begin(),
		arr.begin() + pos,
		std::back_inserter<decltype(arr)>(narr));
      narr.push_back(nn);
      std::copy(arr.begin() + pos,
		arr.end(),
		std::back_inserter<decltype(arr)>(narr));

      return cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp | flag, std::move(narr), g_);
    }

    cnode<K, V, Hash, Alloc, Barrier>* renewed(const gen& ngen, ctrie<K, V, Hash, Alloc, Barrier>& ct)
    {
      branch_vector<K, V, Hash, Alloc, Barrier> narr(arr.size());
      auto cn = cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp, ngen);

      for(size_t i = 0; i < arr.size(); ++i)
	arr[i]->in_renewed(ngen, ct, narr[i], cn);

      cn->arr = std::move(narr);

      return cn;
    }

    branch<K, V, Hash, Alloc, Barrier>* in_resurrect(inode<K, V, Hash, Alloc, Barrier>* in)
    {
      return in;
    }

    main_node<K, V, Hash, Alloc, Barrier>* to_contracted(int lev)
    {
      if(arr.size() == 1 && lev > 0) {
	bool in_snode;
	auto result = arr[0]->in_to_contracted(in_snode);

	if(in_snode)
	  return result;
      }

      return this;
    }

    cnode<K, V, Hash, Alloc, Barrier>* to_compressed(ctrie<K, V, Hash, Alloc, Barrier>& ct,
						     int lev,
						     gen g_)
    {
      branch_vector<K, V, Hash, Alloc, Barrier> tmp_arr(arr.size());
      auto cn = cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp, g_);
      
      for(size_t i = 0; i < arr.size(); ++i)
	arr[i]->compressed(ct, tmp_arr[i], cn);

      cn->arr = std::move(tmp_arr);
      cn->to_contracted(lev);

      return cn;
    }

    static main_node<K, V, Hash, Alloc, Barrier>* dual(snode<K, V, Hash, Alloc, Barrier>* x,
						       uint64_t xhc,
						       snode<K, V, Hash, Alloc, Barrier>* y,
						       uint64_t yhc,
						       int lev,
						       gen g_)
    {
      if(lev < 66) {
	uint64_t xidx = (xhc >> lev) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
	uint64_t yidx = (yhc >> lev) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
	uint64_t bmp = (1ULL << xidx) | (1ULL << yidx);

	if(xidx == yidx) {
	  auto subinode_main = dual(x, xhc, y, yhc, lev + 6, g_);
	  auto subinode = cons<Alloc, inode<K, V, Hash, Alloc, Barrier>>(subinode_main, g_);

	  return cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp,
								branch_vector<K, V, Hash, Alloc, Barrier>({subinode}),
								g_);
	} else {
	  if(xidx < yidx)
	    return cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp,
								  branch_vector<K, V, Hash, Alloc, Barrier>({x, y}),
								  g_);
	  else
	    return cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(bmp,
								  branch_vector<K, V, Hash, Alloc, Barrier>({y, x}),
								  g_);
	}
      } else {
	std::initializer_list<snode<K, V, Hash, Alloc, Barrier>> init({*x, *y});
	return cons<Alloc, lnode<K, V, Hash, Alloc, Barrier>>(init);
      }
    }

    bool insert(ctrie<K, V, Hash, Alloc, Barrier>& ct,
		K& k,
		V& v,
		typename Hash::result_type hc,
		int lev,
		inode<K, V, Hash, Alloc, Barrier>* i,
		inode<K, V, Hash, Alloc, Barrier>* parent,
		gen start_gen)
    {
      uint64_t idx = (hc >> lev) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
      uint64_t flag = 1ULL << idx;
      uint64_t mask = flag - 1;
      uint64_t pos = population_count64(bmp & mask);

      if((bmp & flag) != 0) {
	auto target = arr[pos].get();

	if(auto in = dynamic_cast<inode<K, V, Hash, Alloc, Barrier>*>(target))
	{
	  if(start_gen == in->g) {
	    return ct.insert(in, k, v, hc, lev + 6, i, start_gen);
	  } else {
	    if(i->gcas(this, renewed(start_gen, ct), ct))
	      return insert(ct, k, v, hc, lev, i, parent, start_gen);
	    else
	      return false;
	  }
	} else {
	  auto sn = reinterpret_cast<snode<K, V, Hash, Alloc, Barrier>*>(target);

	  if(sn->hc == hc && sn->k == k) {
	    auto nn = updated_at(pos,
				 cons<Alloc, snode<K, V, Hash, Alloc, Barrier>>(k, v, hc),
				 i->g);	    
	    
	    auto result = i->gcas(this, nn, ct);	    
	    
	    return result;
	  } else {
	    auto rn = g == i->g ? this : renewed(i->g, ct);

	    auto nn =
	      rn->updated_at(pos,
			     cons<Alloc, inode<K, V, Hash, Alloc, Barrier>>(cnode::dual(sn,
											sn->hc,
											cons<Alloc, snode<K, V, Hash, Alloc, Barrier>>(k, v, hc),
											hc,
											lev + 6,
											i->g),
									    i->g),
			     g);

	    return i->gcas(this, nn, ct);
	  }
	}
      } else {
	auto rn = g == i->g ? this : renewed(i->g, ct);
	auto ncnode = rn->inserted_at(pos,
				      flag,
				      cons<Alloc, snode<K, V, Hash, Alloc, Barrier>>(k, v, hc),
				      i->g);

	return i->gcas(this, ncnode, ct);
      }
    }

    ctrie_query_result<V> remove(ctrie<K, V, Hash, Alloc, Barrier>& ct,
				 K& k,
				 V* v,
				 typename Hash::result_type hc,
				 int lev,
				 inode<K, V, Hash, Alloc, Barrier>* i,
				 inode<K, V, Hash, Alloc, Barrier>* parent,
				 gen start_gen)
    {
      uint64_t idx  = (hc >> lev) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
      uint64_t flag = 1ULL << idx;

      ctrie_query_result<V> res;

      if((bmp & flag) == 0)
	return ctrie_query_result<V>::not_found;
      else {
	uint64_t pos = population_count64(bmp & (flag - 1));
	auto target = arr[pos].get();

	if(auto in = dynamic_cast<inode<K, V, Hash, Alloc, Barrier>*>(target)) {
	  if(start_gen == in->g)
	    res = ct.remove(in, k, v, hc, lev + 6, i, start_gen);
	  else {
	    if(i->gcas(this, renewed(start_gen, ct), ct))
	      res = remove(ct, k, v, hc, lev, i, parent, start_gen);
	    else
	      res = ctrie_query_result<V>::restart;
	  }
	} else {
	  auto sn = reinterpret_cast<snode<K, V, Hash, Alloc, Barrier>*>(target);

	  if(sn->hc == hc && sn->k == k && (!v || sn->v == *v)) {
	    auto ncn = removed_at(pos, flag, i->g)->to_contracted(lev);

	    if(i->gcas(this, ncn, ct)) {
	      res = &sn->v;
	    } else {
	      res = ctrie_query_result<V>::restart;
	    }
	  } else
	    res = ctrie_query_result<V>::not_found;
	}
      }

      if(res == ctrie_query_result<V>::restart || res == ctrie_query_result<V>::not_found)
	return res;
      else {
	if(parent != nullptr) {
	  auto n = i->gcas_read(ct);
	  if(auto tn = dynamic_cast<tnode<K, V, Hash, Alloc, Barrier>*>(n)) {
	    auto nonlive = n;
	  clean_parent:
	    auto pm = parent->gcas_read(ct);

	    if(auto cn = dynamic_cast<cnode<K, V, Hash, Alloc, Barrier>*>(pm)) {
	      uint64_t idx = (hc >> (lev - 6)) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
	      uint64_t flag = 1ULL << idx;

	      if((bmp & flag) != 0) {
		uint64_t pos = population_count64(bmp & (flag - 1));
		auto target = static_cast<branch<K, V, Hash, Alloc, Barrier>*>(arr[pos]);

		if(target == i) {
		  if(auto tn = dynamic_cast<tnode<K, V, Hash, Alloc, Barrier>*>(nonlive)) {
		    auto ncn = updated_at(pos, tn->copy_untombed(), i->g)->to_contracted(lev - 6);

		    if(!parent->gcas(this, ncn, ct))
		      if(ct.rdcss_read_root()->g == start_gen)
			goto clean_parent;
		  }
		}
	      }
	    }
	  }
	}

	return res;
      }
    }

    ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>& ct,
				 K& k,
				 typename Hash::result_type hc,
				 int lev,
				 inode<K, V, Hash, Alloc, Barrier>* i,
				 inode<K, V, Hash, Alloc, Barrier>* parent,
				 gen start_gen)
    {
      uint64_t idx  = (hc >> lev) & ctrie<K, V, Hash, Alloc, Barrier>::hash_mask;
      uint64_t flag = 1ULL << idx;

      if((bmp & flag) == 0) {
	return ctrie_query_result<V>::not_found;
      }

      uint64_t pos = bmp == std::numeric_limits<decltype(bmp)>::max()
	? idx : population_count64(bmp & (flag - 1));

      return arr[pos]->lookup(ct, k, hc, lev, i, parent, this, start_gen);
    }

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier>
  struct lnode : main_node<K, V, Hash, Alloc, Barrier> {
    const plist<snode<K, V, Hash, Alloc, Barrier>, Alloc> contents;

    lnode(std::initializer_list<snode<K, V, Hash, Alloc, Barrier>>& init)
      : contents(init)
    {}

    lnode(plist<snode<K, V, Hash, Alloc, Barrier>, Alloc> contents_)
      : contents(contents_)
    {}

    branch<K, V, Hash, Alloc, Barrier>* in_resurrect(inode<K, V, Hash, Alloc, Barrier>* in)
    {
      return in;
    }

    main_node<K, V, Hash, Alloc, Barrier>* removed(K& k) const
    {
      auto nl = contents.remove_first_by([&k](const snode<K, V, Hash, Alloc, Barrier>& sn) {
	  return sn.k == k;
	});

      if(nl.size() > 1)
	return cons<Alloc, lnode<K, V, Hash, Alloc, Barrier>>(nl);
      else
	return cons<Alloc, tnode<K, V, Hash, Alloc, Barrier>>(&nl.car());
    }

    main_node<K, V, Hash, Alloc, Barrier>* inserted(K& k, V& v, typename Hash::result_type hc) const
    {
      auto nl = contents.push_front(snode<K, V, Hash, Alloc, Barrier>(k, v, hc));
      return cons<Alloc, lnode<K, V, Hash, Alloc, Barrier>>(nl);
    }

    inline const V* get(K& k) const
    {
      auto ptr =
	contents.find_first_by([&k](const snode<K, V, Hash, Alloc, Barrier>& sn) {
	    return sn.k == k;
	  });

      if(ptr)
	return &ptr->v;
      else
	return nullptr;
    }

    inline bool insert(ctrie<K, V, Hash, Alloc, Barrier>& ct,
		       K& k,
		       V& v,
		       typename Hash::result_type hc,
		       int,
		       inode<K, V, Hash, Alloc, Barrier>* i,
		       inode<K, V, Hash, Alloc, Barrier>*,
		       gen)
    {
      auto nn = inserted(k, v, hc);
      return i->gcas(this, nn, ct);
    }

    ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>&,
				 K& k,
				 typename Hash::result_type,
				 int,
				 inode<K, V, Hash, Alloc, Barrier>*,
				 inode<K, V, Hash, Alloc, Barrier>*,
				 gen)
    {
      return get(k);
    }

    ctrie_query_result<V> remove(ctrie<K, V, Hash, Alloc, Barrier>& ct,
				 K& k,
				 V* v,
				 typename Hash::result_type,
				 int,
				 inode<K, V, Hash, Alloc, Barrier>* i,
				 inode<K, V, Hash, Alloc, Barrier>*,
				 gen)
    {
      if(v == nullptr) {
	auto optv = get(k);
	auto nn = removed(k);

	if(i->gcas(this, nn, ct))
	  return optv;
	else
	  return ctrie_query_result<V>::restart;
      } else {
	if(auto v0 = get(k)) {
	  if(v0 == v) {
	    auto nn = removed(k);
	    if(i->gcas(this, nn, ct))
	      return v0;
	    else
	      return ctrie_query_result<V>::restart;
	  } else
	    return ctrie_query_result<V>::not_found;
	} else
	  return ctrie_query_result<V>::not_found;
      }
    }

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  class inode_or_rdcss
  {
  public:
    virtual ~inode_or_rdcss() {}
    virtual void* derived_ptr() = 0;
  private:
    virtual inode<K, V, Hash, Alloc, Barrier>*
    rdcss_commit(Barrier<std::atomic<inode_or_rdcss<K, V, Hash, Alloc, Barrier>*>>&,
		 ctrie<K, V, Hash, Alloc, Barrier>&,
		 bool) = 0;

    friend class ctrie<K, V, Hash, Alloc, Barrier>;
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier = identity>
  struct inode : branch<K, V, Hash, Alloc, Barrier>, inode_or_rdcss<K, V, Hash, Alloc, Barrier>
  {
    Barrier<std::atomic<main_node<K, V, Hash, Alloc, Barrier>*>> main;
    const gen g;

    inode()
      : main(cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(0, branch_vector<K, V, Hash, Alloc, Barrier>(), gen()))
    {}

    inode(const gen& g_)
      : main(cons<Alloc, cnode<K, V, Hash, Alloc, Barrier>>(0, branch_vector<K, V, Hash, Alloc, Barrier>(), g_))
      , g(g_)
    {}

    inode(main_node<K, V, Hash, Alloc, Barrier>* const& main_, const gen& g_)
      : main(main_), g(g_)
    {}

    bool in_insert(ctrie<K, V, Hash, Alloc, Barrier>& ct,
		   K& k,
		   V& v,
		   typename Hash::result_type hc,
		   int lev,
		   inode<K, V, Hash, Alloc, Barrier>* i,
		   inode<K, V, Hash, Alloc, Barrier>* parent,
		   cnode<K, V, Hash, Alloc, Barrier>* caller,
		   gen start_gen)
    {
      if(start_gen == g) {
	return ct.insert(this, k, v, hc, lev + 6, i, start_gen);
      } else {
	if(i->gcas(caller, caller->renewed(start_gen, ct), ct))
	  return caller->insert(ct, k, v, hc, lev, i, parent, start_gen);
	else
	  return false;
      }
    }

    void in_renewed(gen ngen,
		    ctrie<K, V, Hash, Alloc, Barrier>& ct,
		    Barrier<branch<K, V, Hash, Alloc, Barrier>*>& narr,
		    cnode<K, V, Hash, Alloc, Barrier>* cn)
    {
      narr.write(reinterpret_cast<void*>(cn), copy_to_gen(ngen, ct));
    }

    main_node<K, V, Hash, Alloc, Barrier>* in_to_contracted(bool& in_snode)
    {
      in_snode = false;
      return nullptr;
    }

    inline inode<K, V, Hash, Alloc, Barrier>*
    copy_to_gen(gen ngen, ctrie<K, V, Hash, Alloc, Barrier>& ct)
    {
      auto nmain = gcas_read(ct);
      return cons<Alloc, inode<K, V, Hash, Alloc, Barrier>>(nmain, ngen);
    }

    inline main_node<K, V, Hash, Alloc, Barrier>*
    gcas_commit(main_node<K, V, Hash, Alloc, Barrier>* m, ctrie<K, V, Hash, Alloc, Barrier>& ct)
    {
    begin:
      prev_node<K, V, Hash, Alloc, Barrier>* p = m->prev.load(std::memory_order_relaxed);
      auto r = ct.rdcss_read_root();

      if(p == nullptr) {
	return m;
      }

      if(auto ptr = dynamic_cast<failure<K, V, Hash, Alloc, Barrier>*>(p)) {
	if(main.compare_exchange_strong(reinterpret_cast<void*>(this),
					m,
					ptr->prev,
					std::memory_order_relaxed,
					std::memory_order_relaxed))
	{
	  return ptr->prev;
	}

	m = main.load(std::memory_order_relaxed);
	goto begin;
      }	else {
	if((r->g == g) && !ct.read_only) {
	  if(m->prev.compare_exchange_strong(reinterpret_cast<void*>(m->derived_ptr()),
					     p,
					     nullptr,
					     std::memory_order_relaxed,
					     std::memory_order_relaxed))
	  {
	    return m;
	  } else
	    goto begin;
	} else {
	  auto pm = reinterpret_cast<main_node<K, V, Hash, Alloc, Barrier>*>(p);
	  auto pp = cons<Alloc, failure<K, V, Hash, Alloc, Barrier>>(pm);
	  m->prev.compare_exchange_strong(reinterpret_cast<void*>(m->derived_ptr()),
					  p,
					  pp,
					  std::memory_order_relaxed,
					  std::memory_order_relaxed);

	  m = main.load(std::memory_order_relaxed);
	  goto begin;
	}
      }
    }

    inline main_node<K, V, Hash, Alloc, Barrier>*
    gcas_read(ctrie<K, V, Hash, Alloc, Barrier>& ct)
    {
      auto m = main.load(std::memory_order_relaxed);

      if(!m->prev.load(std::memory_order_relaxed))
	return m;

      return gcas_commit(m, ct);
    }

    inline bool gcas(main_node<K, V, Hash, Alloc, Barrier>* old,
		     main_node<K, V, Hash, Alloc, Barrier>* n,		     
		     ctrie<K, V, Hash, Alloc, Barrier>& ct)
    {
      n->prev.store(reinterpret_cast<void*>(n->derived_ptr()),
		    old,
		    std::memory_order_relaxed);

      if(main.compare_exchange_strong(reinterpret_cast<void*>(this),
				      old,
				      n,
				      std::memory_order_relaxed,
				      std::memory_order_relaxed))
      {
	gcas_commit(n, ct);
	return n->prev == nullptr;
      } else {
	return false;
      }
    }

    void clean(inode<K, V, Hash, Alloc, Barrier>* nd, ctrie<K, V, Hash, Alloc, Barrier>& ct, int lev)
    {
      auto m = nd->gcas_read(ct);
      if(auto cn = dynamic_cast<cnode<K, V, Hash, Alloc, Barrier>*>(m))
	nd->gcas(cn, cn->to_compressed(ct, lev, g), ct);      
    }

    void compressed(ctrie<K, V, Hash, Alloc, Barrier>& ct,
		    Barrier<branch<K, V, Hash, Alloc, Barrier>*>& b,
		    cnode<K, V, Hash, Alloc, Barrier>* cn)
    {
      auto inodemain = gcas_read(ct);
      assert(inodemain != nullptr);
      b.write(reinterpret_cast<void*>(cn), resurrect(this, inodemain));
    }

    inode<K, V, Hash, Alloc, Barrier>* rdcss_commit(Barrier<std::atomic<inode_or_rdcss<K, V, Hash, Alloc, Barrier>*>>&,
						    ctrie<K, V, Hash, Alloc, Barrier>&,
						    bool)
    {
      return this;
    }

    ctrie_query_result<V> lookup(ctrie<K, V, Hash, Alloc, Barrier>& ct,
				 K& k,
				 typename Hash::result_type hc,
				 int lev,
				 inode<K, V, Hash, Alloc, Barrier>* i,
				 inode<K, V, Hash, Alloc, Barrier>* parent,
				 cnode<K, V, Hash, Alloc, Barrier>* cn,
				 gen start_gen)
    {
      if(ct.read_only || start_gen == g) {
	return ct.lookup(this, k, hc, lev + 6, i, start_gen);
      } else {
	if(i->gcas(cn, cn->renewed(start_gen, ct), ct))
	  return ct.lookup(i, k, hc, lev, parent, start_gen);
	else
	  return ctrie_query_result<V>::restart;
      }
    }

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier>
  struct rdcss_descriptor : public inode_or_rdcss<K, V, Hash, Alloc, Barrier>
  {
    Barrier<inode<K, V, Hash, Alloc, Barrier>*> ov;
    Barrier<main_node<K, V, Hash, Alloc, Barrier>*> expected_main;
    Barrier<inode<K, V, Hash, Alloc, Barrier>*> nv;
    bool committed;

    inode<K, V, Hash, Alloc, Barrier>* rdcss_commit(Barrier<std::atomic<inode_or_rdcss<K, V, Hash, Alloc, Barrier>*>>& root,
						    ctrie<K, V, Hash, Alloc, Barrier>& ct,
						    bool abort)
    {
      if(abort) {
	inode_or_rdcss<K, V, Hash, Alloc, Barrier>* desc = this;

	if(root.compare_exchange_strong(nullptr,
					desc,
					ov,
					std::memory_order_relaxed,
					std::memory_order_relaxed))
	  return ov;
	else
	  return nullptr;
      } else {
	auto oldmain = ov->gcas_read(ct);
	if(oldmain == expected_main) {
	  inode_or_rdcss<K, V, Hash, Alloc, Barrier>* desc = this;

	  if(root.compare_exchange_strong(nullptr,
					  desc,
					  nv,
					  std::memory_order_relaxed,
					  std::memory_order_relaxed))
	  {
	    committed = true;
	    return nv;
	  } else {
	    return nullptr;
	  }
	} else {
	  inode_or_rdcss<K, V, Hash, Alloc, Barrier>* desc = this;

	  if(root.compare_exchange_strong(nullptr,
					  desc,
					  nv,
					  std::memory_order_relaxed,
					  std::memory_order_relaxed))
	    return ov;
	  else
	    return nullptr;
	}
      }
    }

    rdcss_descriptor(inode<K, V, Hash, Alloc, Barrier>* ov,
		     main_node<K, V, Hash, Alloc, Barrier>* expected_main,
		     inode<K, V, Hash, Alloc, Barrier>* nv)
      : ov(ov), expected_main(expected_main), nv(nv), committed(false)
    {}

    void* derived_ptr()
    {
      return reinterpret_cast<void*>(this);
    }
  };

  template <typename K,
	    typename V,
	    class Hash,
	    template <class> class Alloc,
	    template <class> class Barrier>
  class ctrie {
  private:
    Barrier<std::atomic<inode_or_rdcss<K, V, Hash, Alloc, Barrier>*>> root;
    bool read_only;

    static const size_t hash_mask = 0x3f;

    using hash_result_type = typename Hash::result_type;

    friend struct cnode<K, V, Hash, Alloc, Barrier>;
    friend struct inode<K, V, Hash, Alloc, Barrier>;

    inline inode<K, V, Hash, Alloc, Barrier>* rdcss_commit(bool abort)
    {
      auto v = root.load(std::memory_order_relaxed);
      inode<K, V, Hash, Alloc, Barrier>* ptr = nullptr;

      while(!(ptr = v->rdcss_commit(root, *this, abort)));

      return ptr;
    }

    bool rdcss_root(inode<K, V, Hash, Alloc, Barrier>* ov,
		    main_node<K, V, Hash, Alloc, Barrier>* expected_main,
		    inode<K, V, Hash, Alloc, Barrier>* nv)
    {
      auto desc = cons<Alloc, rdcss_descriptor<K, V, Hash, Alloc, Barrier>>(ov, expected_main, nv);
      inode_or_rdcss<K, V, Hash, Alloc, Barrier>* ovv = ov;

      if(root.compare_exchange_strong(nullptr,
				      ovv,
				      desc,
				      std::memory_order_relaxed,
				      std::memory_order_relaxed))
      {
	return rdcss_commit(false);
      } else {
	return false;
      }
    }

    inline inode<K, V, Hash, Alloc, Barrier>* rdcss_read_root(bool abort = false)
    {
      auto r = root.load(std::memory_order_relaxed);

      if(auto in = dynamic_cast<inode<K, V, Hash, Alloc, Barrier>*>(r)) {
	return in;
      } else {
	return rdcss_commit(abort);
      }
    }

    inline bool insert(inode<K, V, Hash, Alloc, Barrier>* i,
		       K& k,
		       V& v,
		       typename Hash::result_type hc,
		       int lev,
		       inode<K, V, Hash, Alloc, Barrier>* parent,
		       gen start_gen)
    {
      auto imain = i->gcas_read(*this);
      return imain->insert(*this, k, v, hc, lev, i, parent, start_gen);
    }

    inline ctrie_query_result<V> lookup(inode<K, V, Hash, Alloc, Barrier>* i,
					K& k,
					typename Hash::result_type hc,
					int lev,
					inode<K, V, Hash, Alloc, Barrier>* parent,
					gen start_gen)
    {
      auto imain = i->gcas_read(*this);
      return imain->lookup(*this, k, hc, lev, i, parent, start_gen);
    }

    inline ctrie_query_result<V> remove(inode<K, V, Hash, Alloc, Barrier>* i,
					K& k,
					V* v,
					typename Hash::result_type hc,
					int lev,
					inode<K, V, Hash, Alloc, Barrier>* parent,
					gen start_gen)
    {
      auto imain = i->gcas_read(*this);
      return imain->remove(*this, k, v, hc, lev, i, parent, start_gen);
    }

    ctrie(inode_or_rdcss<K, V, Hash, Alloc, Barrier>* root_, bool read_only_ = false)
      : root(root_)
      , read_only(read_only_)
    {}
  public:
    ctrie()
      : root(cons<Alloc, inode<K, V, Hash, Alloc, Barrier>>())
      , read_only(false)
    {}

    ctrie(const ctrie& ct)
      : root(ct.root.load(std::memory_order_relaxed))
      , read_only(ct.read_only)
    {}

    ctrie snapshot()
    {
    begin:
      auto r = rdcss_read_root();
      auto expmain = r->gcas_read(*this);

      if(rdcss_root(r, expmain, r->copy_to_gen(gen(), *this)))
	return ctrie(r->copy_to_gen(gen(), *this));
      else
	goto begin;
    }

    inline bool is_read_only() const
    {
      return read_only;
    }

    inline void insert(K k, V v)
    {
      typename Hash::result_type hc = Hash()(k);      

    begin:
      auto r = rdcss_read_root();
      if(!insert(r, k, v, hc, 0, nullptr, r->g)) {
	goto begin;
      }
    }

    inline const V* remove(K k)
    {
      typename Hash::result_type hc = Hash()(k);

    begin:
      auto r = rdcss_read_root();
      auto res = remove(r, k, nullptr, hc, 0, nullptr, r->g);

      if(res != ctrie_query_result<V>::restart)
	return res;
      else
	goto begin;
    }

    inline const V* lookup(K k)
    {
      typename Hash::result_type hc = Hash()(k);

    begin:
      auto r = rdcss_read_root();
      auto res = lookup(r, k, hc, 0, nullptr, r->g);

      if(res == ctrie_query_result<V>::restart)
	goto begin;
      else
	return res;
    }

    static_assert(sizeof(typename Hash::result_type) == 8, "hash result type must have 64 bits.");
  };
}
#endif
