#pragma once

#include <vector>
#include <unordered_map>
#include <array>
#include <memory>
#include <utility>

namespace alisp
{

namespace management
{


template<typename T>
struct Resource {
    T res;
    uint32_t id;
};


template<typename T, size_t tag>
class Registry {

  public:

    constexpr static std::uint32_t REG_BITS = 0xF << 27;
    constexpr static std::uint32_t TAG_BITS = tag << 27;
    constexpr static std::uint32_t INLINED_BIT = 0x80000000;
    static constexpr size_t INLINED = 10;
    
  private:
    
    std::array<Resource<T>, INLINED> inline_res;
    std::vector<Resource<T>> dyn_res;
    std::vector<uint32_t> free_list;
    
    uint32_t inlined_cnt = 0;

    uint32_t next_id() {
        if (!free_list.empty()) {
            auto i = free_list.back();
            free_list.pop_back();
            return i;
        }
        if (inlined_cnt < INLINED) {
            return (inlined_cnt++ | INLINED_BIT | TAG_BITS);
        }
        return (dyn_res.size() & ~INLINED_BIT) | TAG_BITS;
    }

    Resource<T>* get_memory(uint32_t t_index) {
        if ((t_index & INLINED_BIT) != 0) {
            return &inline_res[t_index & ~INLINED_BIT & ~REG_BITS];
        }
        const auto dyn_index = (t_index & ~INLINED_BIT & ~REG_BITS);
        return dyn_res.data() + dyn_index;
    }

  public:

    Resource<T>* put_resource(T t_res){
        auto id = next_id();
        if ((id & INLINED_BIT) != 0) {
            Resource<T>* mem = get_memory(id);
            new (mem) Resource<T>{t_res, id};
            return mem;
        }

        const auto dyn_id = (id & ~INLINED_BIT & ~REG_BITS);
        dyn_res.insert(dyn_res.begin()+dyn_id, {t_res, id});
        return &dyn_res[dyn_id];
        
    };

    template<typename ... Arg>
    Resource<T>* emplace_resource(Arg ... t_args){
        
        auto id = next_id();
        if ((id & INLINED_BIT) != 0) {
            Resource<T>* mem = get_memory(id);
            new (mem) Resource<T>{T(t_args...), id};
            return mem;
        }
        
        const auto dyn_id = (id & ~INLINED_BIT & ~REG_BITS);
        dyn_res.insert(dyn_res.begin()+dyn_id, {T(t_args...), id});
        return &dyn_res[dyn_id];    
        
    }

    void destroy_resource(uint32_t t_id){
        get_memory(t_id)->~Resource<T>();
        free_list.push_back(t_id);
    };
    
    Resource<T>* get_resource(uint32_t t_id) {
        return get_memory(t_id);
    }

    bool belong(uint32_t t_id ) {
        return ((t_id & REG_BITS) >> 27) == tag;
    }

    T& operator[](uint32_t t_ind) {
        return get_memory(t_ind)->res;
    }
    
};


}

}
