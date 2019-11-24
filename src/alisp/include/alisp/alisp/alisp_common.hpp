#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>

namespace alisp
{

enum class ALObjectType
{

    INT_VALUE, // 1
    REAL_VALUE , // 12.3
    STRING_VALUE, // "<value>"
    SYMBOL,     // <symbol>
    LIST       // (<obj_1> [<obj_n> ...])

};

enum class ALCellType
{

    FUNCTION,
    PRIMITIVE,
    MACRO,
    VALUE

};

class ALObject
{
  public:

    using list_type = std::vector<ALObject*>;
    using int_type = int64_t;
    using real_type = double;
    using string_type = std::string;

    using data = std::variant<int_type, real_type, string_type, list_type>;

  public:

    ALObject() : m_data(0.0), m_type(ALObjectType::REAL_VALUE){}
    ALObject(double value) : m_data(value), m_type(ALObjectType::REAL_VALUE){}
    ALObject(int64_t value) : m_data(value), m_type(ALObjectType::INT_VALUE){}
    ALObject(std::string value, bool symbol=false) : m_data(value), m_type(symbol ? ALObjectType::SYMBOL : ALObjectType::STRING_VALUE){}
    ALObject(std::vector<ALObject*> value) : m_data(std::move(value)), m_type(ALObjectType::LIST){}

    ALObject* i(const size_t index){
        return children()[index];
    }
    auto length() noexcept {
        if (const auto val =  std::get_if<std::vector<ALObject*>>(&m_data)) {
            return val->size();
        } else {
            return size_t(0);
        }
    }
    std::vector<ALObject*>& children() {
        return std::get<std::vector<ALObject*>>(m_data);
    }

    ALObjectType type() const {return m_type;}
    
    std::string to_string() const noexcept {
        return std::get<std::string>(m_data);
    }
    real_type to_real() const noexcept {
        return std::get<double>(m_data);
    }
    int_type to_int() const noexcept {
        return std::get<int_type>(m_data);
    }

    void set_value(int64_t val){
        m_data = val;
    }
    void set_value(double val){
        m_data = val;
    }
    void set_value(std::string val){
        m_data = std::move(val);
    }
    void add_child(ALObject* new_child){
        children().push_back(new_child);
    }
    

    static std::string dump(ALObject* obj)
    {
        std::ostringstream str;

        switch(obj->type())
        {
          case ALObjectType::INT_VALUE:
              str << obj->to_int() << " ";
              break;
              
          case ALObjectType::REAL_VALUE:
              str << obj->to_real() << " ";
              break;
              
          case ALObjectType::STRING_VALUE:
              str << "\"" << obj->to_string() << "\"" << " ";
              break;
              
          case ALObjectType::SYMBOL:
              str << obj->to_string() << " ";
              break;
              
          case ALObjectType::LIST:
              str << "(";
              for (auto ob : obj->children())
              {
                  str << dump(ob);
              }
              str << ") ";
              break;
        }
        
        return str.str();
    }

  private:

    data m_data;
    const ALObjectType m_type;

};


struct Callable
{

    ALObject *params;
    ALObject *body;
};


struct Value
{
    ALObject *val;
};


namespace env{class Environment;}

struct Prim
{
    using func_type = ALObject* (*)(ALObject* obj, env::Environment* env);
    ALObject *(*function)(ALObject* obj, env::Environment* env);
};


class ALCell
{
  public:


    using data = std::variant<std::nullptr_t, Callable, Value, Prim>;

    ALCell(std::string t_name) : m_name(std::move(t_name)), m_data(nullptr) {}

    auto make_prim(Prim::func_type func){
        m_data = Prim{func};
        m_type = ALCellType::PRIMITIVE;
        return *this;
    }
    auto make_function(ALObject *params, ALObject *body){
        m_data = Callable{params, body};
        m_type = ALCellType::FUNCTION;
        return *this;
    }
    auto make_value(ALObject *value){
        m_data = Value{value};
        m_type = ALCellType::VALUE;
        return *this;
    }

    bool is_null() { return std::holds_alternative<std::nullptr_t>(m_data); }
    bool is_value() { return std::holds_alternative<Value>(m_data); }
    bool is_callable() { return std::holds_alternative<Callable>(m_data); }
    bool is_pirm() { return std::holds_alternative<Prim>(m_data); }

    [[nodiscard]] auto type() -> ALCellType {return m_type;}
    [[nodiscard]] auto value() -> ALObject*  {return std::get<Value>(m_data).val;}
    [[nodiscard]] auto callable() -> std::pair<ALObject*, ALObject*> {
        auto call = std::get<Callable>(m_data);
        return std::make_pair(call.params, call.body);
    }
    [[nodiscard]] auto prim() -> Prim::func_type {return std::get<Prim>(m_data).function;}


  private:
    std::string m_name;
    data m_data;
    ALCellType m_type;
};


namespace detail
{

struct ALOBjectHelper
{

    template<typename T,
             typename = std::enable_if_t<std::is_integral_v<T>>>
    static ALObject* get(T a){

        return new ALObject(static_cast<int64_t>(a));
    }

    static ALObject* get(double a){
        return new ALObject(a);
    }

    static ALObject* get(std::string a){
        return new ALObject(a);
    }

    static ALObject* get(std::vector<ALObject*> vec_objs)
    {
        return new ALObject(vec_objs);
    }


    static ALObject* get(ALObject* obj){
        return obj;
    }

    template<typename ...T>
    static ALObject* get(T... objs){
        std::vector<ALObject*> vec_objs;
        vec_objs.reserve(sizeof...(objs));
        (vec_objs.push_back(ALOBjectHelper::get(objs)), ...);
        return new ALObject(vec_objs);
    }

};

}


template<typename ... T>
inline auto make_object(T && ... args)
{
    return detail::ALOBjectHelper::get(std::forward<T>(args) ...);
}

inline auto make_symbol(std::string name)
{
    auto obj = new ALObject(name, true);
    return obj;
}

template<typename T>
inline auto make_int(T value)
{
    static_assert(std::is_integral_v<T>, "Value must be of integer type");
    return make_object(static_cast<int64_t>(value));
}

template<typename T>
inline auto make_double(T value)
{
    static_assert(std::is_arithmetic_v<T>, "Value must be of real type");
    return make_object(static_cast<double>(value));
}

inline auto make_string(std::string value)
{
    return make_object(value);
}



inline auto splice(ALObject* t_obj, std::vector<ALObject>::difference_type start_index,
                   std::vector<ALObject>::difference_type end_index = -1){

    const auto size = static_cast<std::vector<ALObject>::difference_type>(std::size(t_obj->children()));
    const auto end_move = end_index == -1 ? size : end_index;

    const auto new_child = std::vector<ALObject*>(std::next(std::begin(t_obj->children()),  start_index),
                                                  std::next(std::begin(t_obj->children()), end_move));
    return make_object(new_child);    
}

namespace parser
{

class ParserBase
{

  public:

    ParserBase() = default;
    ParserBase(ParserBase&&) = default;
    ParserBase &operator=(ParserBase&&) = delete;
    ParserBase &operator=(const ParserBase&&) = delete;
    virtual ~ParserBase() = default;
    virtual std::vector<ALObject*> parse(const std::string* input, std::string file_name) = 0;
    
};


}



struct FileLocation {
    size_t col = 0;
    size_t line = 0;
    std::string& file;
};


}  // namespace alisp
