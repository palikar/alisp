/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <cstdint>
#include <utility>
#include <memory>
#include <unordered_map>


#include "alisp/utility/meta.hpp"
#include "alisp/utility/vector_view.hpp"

#include "alisp/config.hpp"

namespace alisp
{
using namespace std::string_literals;

enum class ALObjectType
{

    INT_VALUE = 0,  // 1
    REAL_VALUE,     // 12.3
    STRING_VALUE,   // "<value>"
    SYMBOL,         // <symbol>
    LIST            // (<obj_1> [<obj_n> ...])
};

constexpr const char *alobject_type_to_string(ALObjectType type)
{
    constexpr const char *const names[] = { "integer", "real", "string", "symbol", "list" };
    return names[static_cast<int>(type)];
}


class ALObject;
#ifdef USE_MANUAL_MEMORY
using ALObjectPtr                  = ALObject *;
inline constexpr bool USING_SHARED = false;
#else
using ALObjectPtr                  = std::shared_ptr<ALObject>;
using ALObjectCPtr                 = std::shared_ptr<const ALObject>;
inline constexpr bool USING_SHARED = true;
#endif


class alobject_error : public std::runtime_error
{
  private:
    ALObjectCPtr m_obj{ nullptr };

  public:
    explicit alobject_error(const std::string &t_why) : runtime_error(t_why) {}
    alobject_error(const std::string &t_why, ALObjectCPtr t_obj) : runtime_error(t_why), m_obj(t_obj) {}

    ALObjectCPtr obj() { return m_obj; }
};

namespace env
{
class Environment;
}
namespace eval
{
class Evaluator;
}


struct Prim
{
    using func_type = ALObjectPtr (*)(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval);
    ALObjectPtr (*function)(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval);
};

class ALObject : public std::conditional_t<USING_SHARED, std::enable_shared_from_this<ALObject>, utility::empty_base>
{
  public:
    using list_type   = std::vector<ALObjectPtr>;
    using view_type   = utility::vector_view<ALObjectPtr>;
    using int_type    = int64_t;
    using real_type   = double;
    using string_type = std::string;

    using data_type = std::variant<int_type, real_type, string_type, list_type, view_type>;

  private:
    template<typename Type, typename Visitor, typename Or> decltype(auto) visit_or(Visitor &&visitor, Or &&other) const
    {
        if (const auto val = std::get_if<Type>(&m_data))
        {
            return visitor(*val);
        }
        else
        {
            return other();
        }
    }


    template<typename Type> void check() const
    {
        if (!std::get_if<Type>(&m_data))
        {
            if constexpr (std::is_same_v<Type, list_type>)
            {
                if (check_temp_flag()) return;
                throw alobject_error("Not a list object.", shared_from_this());
            }
            if constexpr (std::is_same_v<Type, string_type>)
                throw alobject_error("Not a string object.", shared_from_this());
            if constexpr (std::is_same_v<Type, real_type>)
                throw alobject_error("Not a real object.", shared_from_this());
            if constexpr (std::is_same_v<Type, int_type>) throw alobject_error("Not a int object.", shared_from_this());
        }
    }


    template<typename Type> Type as() const
    {
        check<Type>();
        return std::get<Type>(m_data);
    }

  public:
    ALObject() : m_data(0.0), m_type(ALObjectType::REAL_VALUE) {}
    explicit ALObject(real_type value) : m_data(value), m_type(ALObjectType::REAL_VALUE) {}
    explicit ALObject(int_type value) : m_data(value), m_type(ALObjectType::INT_VALUE) {}
    ALObject(string_type value, bool symbol = false)
      : m_data(value), m_type(symbol ? ALObjectType::SYMBOL : ALObjectType::STRING_VALUE)
    {
    }

    explicit ALObject(list_type value) : m_data(std::move(value)), m_type(ALObjectType::LIST) {}

    explicit ALObject(view_type value) : m_data(std::move(value)), m_type(ALObjectType::LIST) { set_temp_flag(); }
    ALObject(list_type::iterator value_1, list_type::iterator value_2)
      : m_data(view_type(value_1, value_2)), m_type(ALObjectType::LIST)
    {
        set_temp_flag();
    }

    ALObjectType type() const { return m_type; }
    bool is_int() const { return m_type == ALObjectType::INT_VALUE; }
    bool is_string() const { return m_type == ALObjectType::STRING_VALUE; }
    bool is_real() const { return m_type == ALObjectType::REAL_VALUE; }
    bool is_list() const { return m_type == ALObjectType::LIST; }
    bool is_sym() const { return m_type == ALObjectType::SYMBOL; }

    ALObjectPtr &i(const size_t index)
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data)[index];
        }
        return children()[index];
    }

    const ALObjectPtr &i(const size_t index) const
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data)[index];
        }
        return children()[index];
    }

    ALObjectPtr &operator[](const size_t index) { return i(index); }

    const ALObjectPtr &operator[](const size_t index) const { return i(index); }

    auto length() const noexcept
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).size();
        }

        return visit_or<list_type>([](const auto &vec) { return std::size(vec); },
                                   []() { return list_type::size_type(0); });
    }

    auto size() const { return length(); }

    list_type &children()
    {
        check<list_type>();
        if (check_temp_flag())
            throw alobject_error("Accesing the children elements of a temporary object.", shared_from_this());
        return std::get<list_type>(m_data);
    }

    const list_type &children() const
    {
        check<list_type>();
        if (check_temp_flag())
            throw alobject_error("Accesing the children elements of a temporary object.", shared_from_this());
        return std::get<list_type>(m_data);
    }

    string_type &to_string()
    {
        check<string_type>();
        return std::get<std::string>(m_data);
    }

    const string_type &to_string() const
    {
        check<string_type>();
        return std::get<std::string>(m_data);
    }

    real_type to_real() const
    {
        if (std::get_if<real_type>(&m_data))
        {
            return std::get<real_type>(m_data);
        }
        if (std::get_if<int_type>(&m_data))
        {
            return static_cast<real_type>(std::get<int_type>(m_data));
        }
        throw alobject_error("Not a number object.", shared_from_this());
        return 0.0;
    }

    int_type to_int() const
    {
        check<int_type>();
        return std::get<int_type>(m_data);
    }

    void set(int_type val)
    {
        check<int_type>();
        m_data = val;
    }

    void set(real_type val)
    {
        check<real_type>();
        m_data = val;
    }

    void set(string_type val)
    {
        check<string_type>();
        m_data = std::move(val);
    }

    void add(ALObjectPtr new_child) { children().push_back(new_child); }

    data_type &data() { return m_data; }

    auto get_prime() { return m_prime; }

    void make_prime(Prim::func_type func)
    {
        set_function_flag();
        set_prime_flag();
        m_prime = func;
    }

    auto get_function() { return std::pair(i(0), i(1)); }

    //     7    6    5    4    3    2   1     0
    //   0000 0000 0000 0000 0000 0000 0000 0000
    //   0000 0000 0000 0000 0000 0000 0000 0001 - BIND_TYPE
    //   0000 0000 0000 0000 0000 0000 0000 1110 - TYPE (not used)
    //   0000 0000 0000 0000 0000 1111 1111 0000 - LOC_MASK
    //   0000 0000 0000 0000 0001 0000 0000 0000 - IN_TLB
    //   0000 0000 0000 0000 0010 0000 0000 0000 - IS_PRIME
    //   0000 0000 0000 0000 0100 0000 0000 0000 - IS_FUNCTION
    //   0000 0000 0000 0000 1000 0000 0000 0000 - IS_MACRO
    //   0000 0000 0000 0001 0000 0000 0000 0000 - CONST
    //   0000 0000 0000 0010 0000 0000 0000 0000 - CHAR
    //   0000 0000 0000 0100 0000 0000 0000 0000 - TEMP_OBJECT

    struct AlObjectFlags
    {
        constexpr static std::uint32_t BIND_TYPE = 0x00000001;
        constexpr static std::uint32_t TYPE      = 0x0000000E;
        constexpr static std::uint32_t LOC       = 0x00000FF0;
        constexpr static std::uint32_t PRIME     = 0x00002000;
        constexpr static std::uint32_t FUN       = 0x00004000;
        constexpr static std::uint32_t MACRO     = 0x00008000;
        constexpr static std::uint32_t CONST     = 0x00010000;
        constexpr static std::uint32_t CHAR      = 0x00020000;
        constexpr static std::uint32_t TEMP      = 0x00040000;
    };

    inline void set_function_flag() { m_flags |= AlObjectFlags::FUN; }
    inline void set_prime_flag() { m_flags |= AlObjectFlags::PRIME; }
    inline void set_macro_flag() { m_flags |= AlObjectFlags::MACRO; }
    inline void set_const_flag() { m_flags |= AlObjectFlags::CONST; }
    inline void set_char_flag() { m_flags |= AlObjectFlags::CHAR; }
    inline void set_temp_flag() { m_flags |= AlObjectFlags::TEMP; }

    inline void reset_function_flag() { m_flags &= ~AlObjectFlags::FUN; }
    inline void reset_prime_flag() { m_flags &= ~AlObjectFlags::PRIME; }
    inline void reset_macro_flag() { m_flags &= ~AlObjectFlags::MACRO; }
    inline void reset_const_flag() { m_flags &= ~AlObjectFlags::CONST; }
    inline void reset_char_flag() { m_flags &= ~AlObjectFlags::CHAR; }
    inline void reset_temp_flag() { m_flags &= ~AlObjectFlags::TEMP; }

    inline bool check_function_flag() const { return (m_flags & AlObjectFlags::FUN) > 0; }
    inline bool check_prime_flag() const { return (m_flags & AlObjectFlags::PRIME) > 0; }
    inline bool check_macro_flag() const { return (m_flags & AlObjectFlags::MACRO) > 0; }
    inline bool check_const_flag() const { return (m_flags & AlObjectFlags::CONST) > 0; }
    inline bool check_char_flag() const { return (m_flags & AlObjectFlags::CHAR) > 0; }
    inline bool check_temp_flag() const { return (m_flags & AlObjectFlags::TEMP) > 0; }

    void set_location(std::uint_fast16_t loc) { m_flags &= (~AlObjectFlags::LOC) | (loc << 4); }
    auto get_location() { return ((m_flags & AlObjectFlags::LOC) >> 4); }

    auto begin()
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).begin();
        }
        return std::begin(children());
    }

    auto end()
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).end();
        }
        return std::end(children());
    }

    auto begin() const
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).begin();
        }
        return std::begin(children());
    }

    auto end() const
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).end();
        }
        return std::end(children());
    }

    auto cbegin() const
    {

        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).begin();
        }

        return std::cbegin(children());
    }

    auto cend() const
    {
        if (check_temp_flag())
        {
            return std::get<view_type>(m_data).end();
        }
        return std::cend(children());
    }

    ALObjectPtr get_prop(const std::string &t_name) const
    {
        auto search = m_props.find(t_name);
        if (search != m_props.end())
        {
            return search->second;
        }
        return nullptr;
    }

    void set_prop(const std::string &t_name, ALObjectPtr t_value)
    {
        if (m_props.count(t_name) > 0)
        {
            m_props.at(t_name) = std::move(t_value);
        }
        else
        {
            m_props.insert({ t_name, std::move(t_value) });
        }
    }

    bool prop_exists(const std::string &t_name) const { return m_props.count(t_name) != 0; }

    auto &props() { return m_props; }

    const auto &props() const { return m_props; }

    std::string pretty_print() const
    {
        std::ostringstream oss;
        oss << "(ALObject<" << alobject_type_to_string(type()) << "> ";
        switch (type())
        {
            case ALObjectType::INT_VALUE:
                oss << to_int() << " <#o" << std::oct << to_int() << " "
                    << "#x" << std::hex << to_int() << std::dec;
                if (check_char_flag())
                {
                    oss << " ?" << char(to_int());
                }
                oss << ">";
                break;
            case ALObjectType::REAL_VALUE: oss << to_real(); break;
            case ALObjectType::SYMBOL: oss << to_string(); break;
            case ALObjectType::STRING_VALUE: oss << '\"' << to_string() << '\"'; break;
            case ALObjectType::LIST:
                if (check_prime_flag())
                    oss << "*prime*";
                else if (check_macro_flag())
                    oss << "*macro*";
                else if (check_function_flag())
                    oss << "*func*";
                break;
        }

        if (check_const_flag())
        {
            oss << "<c>";
        }
        oss << ")";

        return oss.str();
    }

  private:
    data_type m_data;
    Prim::func_type m_prime = nullptr;
    const ALObjectType m_type;
    std::uint_fast32_t m_flags = 0;
    std::unordered_map<std::string, ALObjectPtr> m_props;
};

inline std::ostream &operator<<(std::ostream &os, const ALObject &t_obj)
{
    os << t_obj.pretty_print();
    return os;
}

inline std::ostream &operator<<(std::ostream &os, const ALObjectPtr t_obj)
{
    os << t_obj->pretty_print();
    return os;
}

inline std::string dump(const ALObjectCPtr &obj)
{
    std::ostringstream str;

    switch (obj->type())
    {
        case ALObjectType::INT_VALUE: str << obj->to_int(); break;

        case ALObjectType::REAL_VALUE: str << obj->to_real(); break;

        case ALObjectType::STRING_VALUE: str << "\"" << obj->to_string() << "\""; break;

        case ALObjectType::SYMBOL: str << obj->to_string(); break;

        case ALObjectType::LIST:
            if (obj->length() == 0)
            {
                str << "() ";
                break;
            }

            str << "(";
            for (const auto ob : *obj)
            {
                str << dump(ob) << " ";
            }
            str.seekp(-1, std::ios_base::end);
            str << ")";
            break;
    }

    return str.str();
}


namespace parser
{

class ParserBase
{

  public:
    ParserBase()              = default;
    ParserBase(ParserBase &&) = default;
    ParserBase &operator=(ParserBase &&) = delete;
    ParserBase &operator=(const ParserBase &&)                                               = delete;
    virtual ~ParserBase()                                                                    = default;
    virtual std::vector<ALObjectPtr> parse(std::string &input, const std::string &file_name) = 0;
};


}  // namespace parser

struct FileLocation
{
    size_t col  = 0;
    size_t line = 0;
    std::string &file;
};


}  // namespace alisp
