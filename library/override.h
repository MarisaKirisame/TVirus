#pragma once

#include <mimalloc.h>

uint64_t allocated = 0;
uint64_t allocated_highest = 0;

template <typename T>
T __hook_add(T p) {
    allocated += mi_usable_size(p);
    allocated_highest = allocated > allocated_highest ? allocated : allocated_highest;
    return p;
}

void __hook_sub(void* p) {
    allocated -= mi_usable_size(p);
}

/*
// Standard C allocation
#define malloc(n)               __hook_add(mi_malloc(n))
#define calloc(n,c)             __hook_add(mi_calloc(n,c))
#define realloc(p,n)            (__hook_sub(p), __hook_add(mi_realloc(p,n)))
#define free(p)                 (__hook_sub(p), mi_free(p))

#define strdup(s)               __hook_add(mi_strdup(s))
#define strndup(s,n)            __hook_add(mi_strndup(s,n))
#define realpath(f,n)           __hook_add(mi_realpath(f,n))

// Microsoft extensions
#define _expand(p,n)            mi_expand(p,n)
#define _msize(p)               mi_usable_size(p)
#define _recalloc(p,n,c)        (__hook_sub(p), __hook_add(mi_recalloc(p,n,c)))

#define _strdup(s)              __hook_add(mi_strdup(s))
#define _strndup(s,n)           __hook_add(mi_strndup(s,n))
#define _wcsdup(s)              __hook_add((wchar_t*)mi_wcsdup((const unsigned short*)(s)))
#define _mbsdup(s)              __hook_add(mi_mbsdup(s))
#define _dupenv_s(b,n,v)        mi_dupenv_s(b,n,v)
#define _wdupenv_s(b,n,v)       mi_wdupenv_s((unsigned short*)(b),n,(const unsigned short*)(v))

// Various Posix and Unix variants
#define reallocf(p,n)           (__hook_sub(p), __hook_add(mi_reallocf(p,n)))
#define malloc_size(p)          mi_usable_size(p)
#define malloc_usable_size(p)   mi_usable_size(p)
#define cfree(p)                (__hook_sub(p), mi_free(p))

#define valloc(n)               __hook_add(mi_valloc(n))
#define pvalloc(n)              __hook_add(mi_pvalloc(n))
#define reallocarray(p,s,n)     (__hook_sub(p), __hook_add(mi_reallocarray(p,s,n)))
#define reallocarr(p,s,n)       mi_reallocarr(p,s,n)
#define memalign(a,n)           __hook_add(mi_memalign(a,n))
#define aligned_alloc(a,n)      __hook_add(mi_aligned_alloc(a,n))
#define posix_memalign(p,a,n)   mi_posix_memalign(p,a,n)
#define _posix_memalign(p,a,n)  mi_posix_memalign(p,a,n)

// Microsoft aligned variants
#define _aligned_malloc(n,a)                  __hook_add(mi_malloc_aligned(n,a))
#define _aligned_realloc(p,n,a)               (__hook_sub(p), __hook_add(mi_realloc_aligned(p,n,a)))
#define _aligned_recalloc(p,s,n,a)            (__hook_sub(p), __hook_add(mi_aligned_recalloc(p,s,n,a)))
#define _aligned_msize(p,a,o)                 mi_usable_size(p)
#define _aligned_free(p)                      (__hook_sub(p), mi_free(p))
#define _aligned_offset_malloc(n,a,o)         __hook_add(mi_malloc_aligned_at(n,a,o))
#define _aligned_offset_realloc(p,n,a,o)      (__hook_sub(p), __hook_add(mi_realloc_aligned_at(p,n,a,o)))
#define _aligned_offset_recalloc(p,s,n,a,o)   (__hook_sub(p), __hook_add(mi_recalloc_aligned_at(p,s,n,a,o)))
*/

#if defined(__cplusplus)
  #include <new>

  #if defined(_MSC_VER) && defined(_Ret_notnull_) && defined(_Post_writable_byte_size_)
  // stay consistent with VCRT definitions
  #define mi_decl_new(n)          mi_decl_nodiscard mi_decl_restrict _Ret_notnull_ _Post_writable_byte_size_(n)
  #define mi_decl_new_nothrow(n)  mi_decl_nodiscard mi_decl_restrict _Ret_maybenull_ _Success_(return != NULL) _Post_writable_byte_size_(n)
  #else
  #define mi_decl_new(n)          mi_decl_nodiscard mi_decl_restrict
  #define mi_decl_new_nothrow(n)  mi_decl_nodiscard mi_decl_restrict
  #endif

  void operator delete(void* p) noexcept              { __hook_sub(p); mi_free(p); };
  void operator delete[](void* p) noexcept            { __hook_sub(p); mi_free(p); };

  void operator delete  (void* p, const std::nothrow_t&) noexcept { __hook_sub(p); mi_free(p); }
  void operator delete[](void* p, const std::nothrow_t&) noexcept { __hook_sub(p); mi_free(p); }

  mi_decl_new(n) void* operator new(std::size_t n) noexcept(false) { return __hook_add(mi_new(n)); }
  mi_decl_new(n) void* operator new[](std::size_t n) noexcept(false) { return __hook_add(mi_new(n)); }

  mi_decl_new_nothrow(n) void* operator new  (std::size_t n, const std::nothrow_t& tag) noexcept { (void)(tag); return __hook_add(mi_new_nothrow(n)); }
  mi_decl_new_nothrow(n) void* operator new[](std::size_t n, const std::nothrow_t& tag) noexcept { (void)(tag); return __hook_add(mi_new_nothrow(n)); }

  #if (__cplusplus >= 201402L || _MSC_VER >= 1916)
  void operator delete  (void* p, std::size_t n) noexcept { __hook_sub(p); mi_free_size(p,n); };
  void operator delete[](void* p, std::size_t n) noexcept { __hook_sub(p); mi_free_size(p,n); };
  #endif

  #if (__cplusplus > 201402L || defined(__cpp_aligned_new))
  void operator delete  (void* p, std::align_val_t al) noexcept { __hook_sub(p); mi_free_aligned(p, static_cast<size_t>(al)); }
  void operator delete[](void* p, std::align_val_t al) noexcept { __hook_sub(p); __hook_sub(p);mi_free_aligned(p, static_cast<size_t>(al)); }
  void operator delete  (void* p, std::size_t n, std::align_val_t al) noexcept { __hook_sub(p); mi_free_size_aligned(p, n, static_cast<size_t>(al)); };
  void operator delete[](void* p, std::size_t n, std::align_val_t al) noexcept { __hook_sub(p); mi_free_size_aligned(p, n, static_cast<size_t>(al)); };
  void operator delete  (void* p, std::align_val_t al, const std::nothrow_t&) noexcept { __hook_sub(p); mi_free_aligned(p, static_cast<size_t>(al)); }
  void operator delete[](void* p, std::align_val_t al, const std::nothrow_t&) noexcept { __hook_sub(p); mi_free_aligned(p, static_cast<size_t>(al)); }

  void* operator new  (std::size_t n, std::align_val_t al) noexcept(false) { return mi_new_aligned(n, static_cast<size_t>(al)); }
  void* operator new[](std::size_t n, std::align_val_t al) noexcept(false) { return mi_new_aligned(n, static_cast<size_t>(al)); }
  void* operator new  (std::size_t n, std::align_val_t al, const std::nothrow_t&) noexcept { return __hook_add(mi_new_aligned_nothrow(n, static_cast<size_t>(al))); }
  void* operator new[](std::size_t n, std::align_val_t al, const std::nothrow_t&) noexcept { return __hook_add(mi_new_aligned_nothrow(n, static_cast<size_t>(al))); }
  #endif
#endif

#include <iostream>
#include <atomic>
#include <fstream>
#include <chrono>
#include <filesystem>
#include <vector>
#include <nlohmann/json.hpp>

std::chrono::time_point record_start_time = std::chrono::system_clock::now();
std::chrono::time_point record_last_time = record_start_time;

struct record_t {
    uint64_t allocated;
    int64_t timestamp; 
};

NLOHMANN_DEFINE_TYPE_NON_INTRUSIVE(record_t, allocated, timestamp)
std::fstream fs(log_path, std::ios::out);

bool record() {
  if (std::chrono::system_clock::now() - record_last_time < std::chrono::milliseconds(100)) {
    return false;
  } else {
    record_last_time = std::chrono::system_clock::now();
    record_t rec({allocated, std::chrono::duration_cast<std::chrono::nanoseconds>(record_last_time.time_since_epoch()).count()});
    nlohmann::json data(rec);
    fs << data << std::endl;
    return true;
  }
}