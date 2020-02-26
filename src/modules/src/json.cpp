#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/utility/string_utils.hpp"

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <cstdint>
#include <cctype>
#include <type_traits>


namespace json
{

using namespace alisp;

namespace detail
{

template<typename T>
[[nodiscard]] constexpr auto parse_num(const std::string_view t_str) noexcept -> typename std::enable_if<std::is_integral<T>::value, T>::type
{
    T t = 0;
    for (const auto c : t_str) {
        if (c < '0' || c > '9') {
            return t;
        }
        t *= 10;
        t += c - '0';
    }
    return t;
}


template<typename T>
[[nodiscard]] auto parse_num(const std::string_view t_str) -> typename std::enable_if<!std::is_integral<T>::value, T>::type
{
    T t = 0;
    T base{};
    T decimal_place = 0;
    int exponent = 0;

    for (const auto c : t_str) {
        switch (c)
        {
          case '.':
              decimal_place = 10;
              break;
          case 'e':
          case 'E':
              exponent = 1;
              decimal_place = 0;
              base = t;
              t = 0;
              break;
          case '-':
              exponent = -1;
              break;
          case '+':
              break;
          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
              if (decimal_place < 10) {
                  t *= 10;
                  t += static_cast<T>(c - '0');
              }
              else {
                  t += static_cast<T>(c - '0') / decimal_place;
                  decimal_place *= 10;
             }
             break;
          default:
             break;
          }
       }
       return exponent ? base * std::pow(T(10), t * static_cast<T>(exponent)) : t;
    }

}

struct JSONParser
{

    static bool isspace(const char c) noexcept
    {
#ifdef CHAISCRIPT_MSVC
        // MSVC warns on these line in some circumstances
#pragma warning(push)
#pragma warning(disable : 6330)
#endif
        return ::isspace(c) != 0;
#ifdef CHAISCRIPT_MSVC
#pragma warning(pop)
#endif

    }

    static void consume_ws( const std::string &str, size_t &offset )
    {
        while( isspace( str.at(offset) ) && offset <= str.size() ) { ++offset; }
    }

    static ALObjectPtr parse_object( const std::string &str, size_t &offset )
    {
        // JSON Object( JSON::Class::Object );
        alisp::ALObject::list_type object;

        ++offset;
        consume_ws( str, offset );
        if( str.at(offset) == '}' ) {
            ++offset;
            auto new_obj = alisp::make_list(object);
            new_obj->set_prop("--json-object--", Qt);
            return new_obj;
        }


        for (;offset<str.size();) {
            auto key = alisp::make_symbol(":"s += parse_next( str, offset )->to_string());

            consume_ws( str, offset );
            if( str.at(offset) != ':' ) {
                throw std::runtime_error(std::string("JSON ERROR: Object: Expected colon, found '") + str.at(offset) + "'\n");
            }
            consume_ws( str, ++offset );
            alisp::ALObjectPtr Value = parse_next( str, offset );


            object.push_back(key);
            object.push_back(Value);
            // Object[Key.to_string()] = Value;

            consume_ws( str, offset );
            if( str.at(offset) == ',' ) {
                ++offset; continue;
            }
            else if( str.at(offset) == '}' ) {
                ++offset; break;
            }
            else {
                throw std::runtime_error(std::string("JSON ERROR: Object: Expected comma, found '") + str.at(offset) + "'\n");
            }
        }

        auto new_obj = alisp::make_list(object);
        new_obj->set_prop("--json-object--", Qt);
        return new_obj;
    }

    static ALObjectPtr parse_array( const std::string &str, size_t &offset )
    {
        // JSON Array( JSON::Class::Array );
        alisp::ALObject::list_type array;
        // size_t index = 0;

        ++offset;
        consume_ws( str, offset );
        if( str.at(offset) == ']' ) {
            ++offset;
            auto new_arr = alisp::make_list(array);
            new_arr->set_prop("--json-array--", Qt);
            return new_arr;
        }

        for (;offset < str.size();) {
            array.push_back(parse_next( str, offset ));
            // Array[index++] = parse_next( str, offset );
            consume_ws( str, offset );

            if( str.at(offset) == ',' ) {
                ++offset; continue;
            }
            else if( str.at(offset) == ']' ) {
                ++offset; break;
            }
            else {
                throw std::runtime_error(std::string("JSON ERROR: Array: Expected ',' or ']', found '") + str.at(offset) + "'\n");
            }
        }

        auto new_arr = alisp::make_list(array);
        new_arr->set_prop("--json-array--", Qt);
        return new_arr;
    }

    static ALObjectPtr parse_string( const std::string &str, size_t &offset )
    {
        std::string val;
        for( char c = str.at(++offset); c != '\"' ; c = str.at(++offset) ) {
            if( c == '\\' ) {
                switch( str.at(++offset) ) {
                  case '\"': val += '\"'; break;
                  case '\\': val += '\\'; break;
                  case '/' : val += '/' ; break;
                  case 'b' : val += '\b'; break;
                  case 'f' : val += '\f'; break;
                  case 'n' : val += '\n'; break;
                  case 'r' : val += '\r'; break;
                  case 't' : val += '\t'; break;
                  case 'u' : {
                      val += "\\u" ;
                      for( size_t i = 1; i <= 4; ++i ) {
                          c = str.at(offset+i);
                          if( (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') ) {
                              val += c;
                          } else {
                              throw std::runtime_error(std::string("JSON ERROR: String: Expected hex character in unicode escape, found '") + c + "'");
                          }
                      }
                      offset += 4;
                  } break;
                  default  : val += '\\'; break;
                }
            } else {
                val += c;
            }
        }
        ++offset;
        auto new_str = alisp::make_string(val);
        new_str->set_prop("--json-string--", Qt);
        return new_str;
    }

    static ALObjectPtr parse_number( const std::string &str, size_t &offset )
    {
        std::string val, exp_str;
        char c = '\0';
        bool isDouble = false;
        bool isNegative = false;
        std::int64_t exp = 0;
        bool isExpNegative = false;
        if( offset < str.size() && str.at(offset) == '-' ) {
            isNegative = true;
            ++offset;
        }
        for (; offset < str.size() ;) {
            c = str.at(offset++);
            if( c >= '0' && c <= '9' ) {
                val += c;
            } else if( c == '.' && !isDouble ) {
                val += c;
                isDouble = true;
            } else {
                break;
            }
        }
        if( offset < str.size() && (c == 'E' || c == 'e' )) {
            c = str.at(offset++);
            if( c == '-' ) {
                isExpNegative = true;
            } else if( c == '+' ) {
                // do nothing
            } else {
                --offset;
            }

            for (; offset < str.size() ;) {
                c = str.at(offset++);
                if( c >= '0' && c <= '9' ) {
                    exp_str += c;
                } else if( !isspace( c ) && c != ',' && c != ']' && c != '}' ) {
                    throw std::runtime_error(std::string("JSON ERROR: Number: Expected a number for exponent, found '") + c + "'");
                }
                else {
                    break;
                }
            }
            exp = detail::parse_num<std::int64_t>( exp_str ) * (isExpNegative?-1:1);
        }
        else if( offset < str.size() && (!isspace( c ) && c != ',' && c != ']' && c != '}' )) {
            throw std::runtime_error(std::string("JSON ERROR: Number: unexpected character '") + c + "'");
        }
        --offset;

        if( isDouble ) {
            return make_real((isNegative?-1:1) * detail::parse_num<double>( val ) * std::pow( 10, exp ));
        } else {
            if( !exp_str.empty() ) {
                return make_real((isNegative?-1:1) * static_cast<double>(detail::parse_num<std::int64_t>( val )) * std::pow( 10, exp ));
            } else {
                return make_int((isNegative?-1:1) * detail::parse_num<std::int64_t>( val ));
            }
        }
    }

    static ALObjectPtr parse_bool( const std::string &str, size_t &offset )
    {
        if( str.substr( offset, 4 ) == "true" ) {
            offset += 4;
            return Qt;
        } else if( str.substr( offset, 5 ) == "false" ) {
            offset += 5;
            return Qnil;
        } else {
            throw std::runtime_error(std::string("JSON ERROR: Bool: Expected 'true' or 'false', found '") + str.substr( offset, 5 ) + "'");
        }
    }

    static ALObjectPtr parse_null( const std::string &str, size_t &offset )
    {
        if( str.substr( offset, 4 ) != "null" ) {
            throw std::runtime_error(std::string("JSON ERROR: Null: Expected 'null', found '") + str.substr( offset, 4 ) + "'");
        }
        offset += 4;
        return Qnil;
    }

    static ALObjectPtr parse_next( const std::string &str, size_t &offset )
    {
        char value;
        consume_ws( str, offset );
        value = str.at(offset);
        switch( value ) {
          case '[' : return parse_array( str, offset );
          case '{' : return parse_object( str, offset );
          case '\"': return parse_string( str, offset );
          case 't' :
          case 'f' : return parse_bool( str, offset );
          case 'n' : return parse_null( str, offset );
          default  : if( ( value <= '9' && value >= '0' ) || value == '-' ) {
                return parse_number( str, offset );
            }
        }
        throw std::runtime_error(std::string("JSON ERROR: Parse: Unexpected starting character '") + value + "'");
    }

};

static std::string json_escape( const std::string &str )
{
    std::string output;
    for(char i : str) {
        switch( i ) {
          case '\"': output += "\\\""; break;
          case '\\': output += "\\\\"; break;
          case '\b': output += "\\b";  break;
          case '\f': output += "\\f";  break;
          case '\n': output += "\\n";  break;
          case '\r': output += "\\r";  break;
          case '\t': output += "\\t";  break;
          default  : output += i; break;
        }
    }
    return output;
}

inline ALObjectPtr load( const std::string &str )
{
    size_t offset = 0;
    return JSONParser::parse_next( str, offset );
}

static std::string dump(ALObjectPtr t_json, long depth = 1, std::string tab = "  ")
{
    
    if (t_json->prop_exists("--json-object--")){
        std::string pad = "";
        for( long i = 0; i < depth; ++i, pad += tab ) { }

        std::string s = "{\n";
        bool skip = true;

        if (std::size(*t_json) == 0) {
            return "{}";
        }

        for(size_t i = 0; i < std::size(*t_json) - 1; i+=2) {
            if( !skip ) { s += ",\n"; }
            
            s += ( pad + "\"" + json_escape(utility::erase_substr(t_json->i(i)->to_string(), ":")) + "\" : " + dump(t_json->i(i+1) , depth + 1, tab ) );
            skip = false;
            }
        
        s += ( "\n" + pad.erase( 0, 2 ) + "}" ) ;
        return s;
        
    } else if (t_json->prop_exists("--json-array--")){
        std::string s = "[";
        bool skip = true;

        for(auto& p : *t_json) {
            if( !skip ) { s += ", "; }
            s += dump(p, depth + 1, tab );
            skip = false;
        }
        
        s += "]";
        return s;
        
    } else if (t_json->prop_exists("--json-string--")){
        return "\"" + json_escape(t_json->to_string()) + "\"";
    } else if (pint(t_json)) {
        return std::to_string(t_json->to_int());
    } else if (preal(t_json)) {
        return std::to_string(t_json->to_real());
    } else if(t_json == Qnil) {
        return "false";
    } else if(t_json == Qt) {
        return "true";
    } 
    return "";
}

ALObjectPtr Fparse_json(ALObjectPtr, env::Environment *, eval::Evaluator *)
{


    return load(R"( {
"key-1" : "value-1",
"key-2" : [1, 2, 3],
"key-3":true,
"key-4":false,
"key-5": [],
"key-6": {},
"key-6": null,
"key-7": {
   "key-7-1" : "sadasd",
   "key-7-2" : ["sadasd", "asd"]
}
})");
}


ALObjectPtr Fdump_json(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    return make_string(json::dump(eval->eval(obj->i(0))));
}

}

ALISP_EXPORT alisp::env::ModulePtr init_json(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mjson = alisp::module_init("json");
    auto json_ptr = Mjson.get();

    alisp::module_defun(json_ptr, "json-parse", &json::Fparse_json);
    alisp::module_defun(json_ptr, "json-dump", &json::Fdump_json);

    alisp::module_defun(json_ptr, "load-file", &json::Fparse_json);
    alisp::module_defun(json_ptr, "dump-file", &json::Fparse_json);

    return Mjson;
}
