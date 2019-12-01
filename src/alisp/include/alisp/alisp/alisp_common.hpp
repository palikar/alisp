#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <bitset>

namespace alisp
{

enum class ALObjectType
{

    INT_VALUE = 0, // 1
    REAL_VALUE , // 12.3
    STRING_VALUE, // "<value>"
    SYMBOL,     // <symbol>
    LIST       // (<obj_1> [<obj_n> ...])
};

constexpr const char* alobject_type_to_string(ALObjectType type){
    constexpr const char* const names[] = {"integer", "real", "string", "symbol", "list"};
    return names[static_cast<int>(type)];
}

enum class ALCellType
{

    FUNCTION,
    PRIMITIVE,
    MACRO,
    VALUE
};


enum class ValueType
{
    PLAIN,
    SEXP,
    CALLABLE
};

class alobject_error : public std::runtime_error
{
  public:
    alobject_error(const std::string& t_why) : runtime_error(t_why) {}

};

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif    


class ALObject
{
  public:

    using list_type = std::vector<ALObject*>;
    using int_type = int64_t;
    using real_type = double;
    using string_type = std::string;

    using data_type = std::variant<int_type, real_type, string_type, list_type>;

  private:

    template<typename Type, typename Visitor, typename Or>
    decltype(auto) visit_or(Visitor &&visitor, Or &&other) const
    {
        if (const auto val =  std::get_if<Type>(&m_data)) {
            return visitor(*val);
        } else {
            return other();
        }
    }


    template<typename Type>
    void check() const
    {
        if (!std::get_if<Type>(&m_data)) {
            if constexpr (std::is_same_v<Type, list_type>) throw alobject_error("Not a list object.");
            if constexpr (std::is_same_v<Type, string_type>) throw alobject_error("Not a string object.");
            if constexpr (std::is_same_v<Type, real_type>) throw alobject_error("Not a real object.");
            if constexpr (std::is_same_v<Type, int_type>) throw alobject_error("Not a int object.");
        }
    }

    
    template<typename Type>
    Type as() const
    {
        check<Type>();
        return std::get<Type>(m_data);
    }

  public:

    ALObject() : m_data(0.0), m_type(ALObjectType::REAL_VALUE){}
    ALObject(double value) : m_data(value), m_type(ALObjectType::REAL_VALUE){}
    ALObject(int64_t value) : m_data(value), m_type(ALObjectType::INT_VALUE){}
    ALObject(std::string value, bool symbol=false) : m_data(value), m_type(symbol ? ALObjectType::SYMBOL : ALObjectType::STRING_VALUE){}
    ALObject(std::vector<ALObject*> value) : m_data(std::move(value)), m_type(ALObjectType::LIST){}

    ALObjectType type() const {return m_type;}
    bool is_int() const { return m_type == ALObjectType::INT_VALUE; }
    bool is_string() const { return m_type == ALObjectType::STRING_VALUE; }
    bool is_real() const { return m_type == ALObjectType::REAL_VALUE; }
    bool is_list() const { return m_type == ALObjectType::LIST; }
    bool is_sym() const { return m_type == ALObjectType::SYMBOL; }
    
    ALObject* i(const size_t index){
        return children()[index];
    }
    auto length() noexcept {
        return visit_or<list_type>( [](const auto& vec){ return std::size(vec); }, [](){ return size_t(0); });
    }
    list_type& children() {
        check<list_type>();
        return std::get<std::vector<ALObject*>>(m_data);
    }

    string_type to_string() const {
        check<string_type>();
        return std::get<std::string>(m_data);
    }
    real_type to_real() const {
        if (std::get_if<real_type>(&m_data)) {
            return std::get<real_type>(m_data);
        }
        if (std::get_if<int_type>(&m_data)) {
            return static_cast<real_type>(std::get<int_type>(m_data));
        }
        throw alobject_error("Not a number object.");
        return 0.0;
    }
    int_type to_int() const {
        check<int_type>();
        return std::get<int_type>(m_data);
    }

    void set(int_type val){
        check<int_type>();
        m_data = val;
    }
    void set(real_type val){
        check<real_type>();
        m_data = val;
    }
    void set(string_type val){
        check<string_type>();
        m_data = std::move(val);
    }
    void add(ALObject* new_child){
        children().push_back(new_child);
    }

    data_type& data() { return m_data;}

    void set_callable_value_flag()
    {
        m_flags |= ( 1 << 1  );
        m_flags &= ~( 1 );
    }
    void set_value_value_flag()
    {
        m_flags |= ( 1 );
        m_flags &= ~( 1 << 1 );
    }
    void set_sexp_value_flag()
    {
        m_flags |= ( 1 | 1 << 1  );
    }

    bool is_callable() { return (m_flags & s_val_mask)  == 0b10; }
    
    bool is_sexp() { return (m_flags & s_val_mask)  == 0b11; }
    
    bool is_value() { return (m_flags & s_val_mask)  == 0b01; }



    std::string pretty_print() const{
        std::ostringstream oss;
        oss << "(ALObject<" << alobject_type_to_string(type()) << "> )";
        return oss.str();
    }

    

  private:
    static constexpr unsigned char s_val_mask = 0b0000'0011;
    
    data_type m_data;
    const ALObjectType m_type;
    std::uint_fast32_t m_flags = 0;
    
};


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif


struct Callable
{

    ALObject *params;
    ALObject *body;
};


struct Value
{
    ALObject *val;
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
    using func_type = ALObject* (*)(ALObject* obj, env::Environment* env, eval::Evaluator* eval);
    ALObject *(*function)(ALObject* obj, env::Environment* env, eval::Evaluator* eval);
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

struct FileLocation
{
    size_t col = 0;
    size_t line = 0;
    std::string& file;
};




}  // namespace alisp
