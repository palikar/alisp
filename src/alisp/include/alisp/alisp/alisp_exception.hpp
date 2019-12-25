#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{


struct signal_exception : public std::runtime_error
{

    signal_exception(ALObjectPtr sym, ALObjectPtr list) :
        runtime_error(format(sym, list)), m_sym(sym), m_list(list)
    {
        
    }

  private:
    ALObjectPtr m_sym;
    ALObjectPtr m_list;

    static std::string format(ALObjectPtr sym, ALObjectPtr list){
        std::ostringstream ss;
        ss << "Signal error <" << dump(sym) << "> :";
        ss << dump(list);
        ss << '\n';
        return ss.str();
    }
};

// class environment_error : public std::runtime_error
// {
//   public:
//     environment_error(const std::string& t_why) : runtime_error(t_why){}

// };

}
