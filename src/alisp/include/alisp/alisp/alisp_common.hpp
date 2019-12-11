#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <bitset>
#include <cstdint>


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


struct Prim
{
    using func_type = ALObject* (*)(ALObject* obj, env::Environment* env, eval::Evaluator* eval);
    ALObject *(*function)(ALObject* obj, env::Environment* env, eval::Evaluator* eval);
};


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
        return std::get<list_type>(m_data);
    }
    const list_type& children() const {
        check<list_type>();
        return std::get<list_type>(m_data);
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

    std::string pretty_print() const{
        std::ostringstream oss;
        oss << "(ALObject<" << alobject_type_to_string(type()) << "> )";
        return oss.str();
    }



    auto get_prime() { reinterpret_cast<Prime::func_type>(children()[0]); }
    auto make_prime(Prim::func_type func) {
        set_function_flag();
        set_prime_flag();
        children()[0] = reinterpret_cast<ALObject*>(func);
        return this;
    }

    
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

    struct AlObjectFlags
    {
      public:

        constexpr static std::uint32_t BIND_TYPE = 0x00000001;
        constexpr static std::uint32_t TYPE = 0x0000000E;
        constexpr static std::uint32_t LOC = 0x00000FF0;
        constexpr static std::uint32_t PRIME = 0x00002000;
        constexpr static std::uint32_t FUN = 0x00004000;
        constexpr static std::uint32_t MACRO = 0x00008000;
    };


    void set_function_flag() { m_flags |= AlObjectFlags::FUN; }
    void set_prime_flag() { m_flags |= AlObjectFlags::PRIME; }
    void set_macro_flag() { m_flags |= AlObjectFlags::MACRO; }

    void reset_function_flag() { m_flags &= ~AlObjectFlags::FUN; }
    void reset_prime_flag() { m_flags &= ~AlObjectFlags::PRIME; }
    void reset_macro_flag() { m_flags &= ~AlObjectFlags::MACRO; }

    bool check_function_flag() { (m_flags & AlObjectFlags::FUN) > 0; }
    bool check_prime_flag() { (m_flags & AlObjectFlags::PRIME) > 0; }
    bool check_macro_flag() { (m_flags & AlObjectFlags::MACRO) > 0; }
    

    void set_location(std::uint_fast16_t loc) { m_flags &= (~AlObjectFlags::LOC) | (loc << 4); }
    auto get_location() { return ((m_flags & AlObjectFlags::LOC) >> 4);}


    auto begin() { return std::begin(children()); }
    auto end() { return std::end(children()); }
    auto cbegin() const { return std::cbegin(children()); }
    auto cend() const { return std::cend(children()); }

    friend operator bool() const;

  private:
    data_type m_data;
    const ALObjectType m_type;
    std::uint_fast32_t m_flags = 0;
    
};



namespace env
{
class Environment;
}


namespace eval
{
class Evaluator;
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

struct FileLocation
{
    size_t col = 0;
    size_t line = 0;
    std::string& file;
};



struct signal_exception : public runtime_error
{

    signal_exception(ALOBject* sym, ALOBject* list) :
        m_sym(sym), m_list(list), runtime_error(format(sym, list))
    {
        
    }

  private:
    ALOBject* m_sym;
    ALOBject* m_list;

    static std::string format(){
        std::ostringstream ss;
        ss << "Signal error <" << dump(m_sym) << "> :";
        ss << dump(list);
        ss << '\n';
        return ss.str();
    }

    

};



    
}  // namespace alisp
