#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{


struct signal_exception : public std::runtime_error
{

    signal_exception(ALObject* sym, ALObject* list) :
        runtime_error(format(sym, list)), m_sym(sym), m_list(list)
    {
        
    }

  private:
    ALObject* m_sym;
    ALObject* m_list;

    static std::string format(ALObject* sym, ALObject* list){
        std::ostringstream ss;
        ss << "Signal error <" << dump(sym) << "> :";
        ss << dump(list);
        ss << '\n';
        return ss.str();
    }
};


}
