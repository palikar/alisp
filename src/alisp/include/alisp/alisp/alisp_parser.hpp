#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>
#include <unordered_map>
#include <array>
#include <tuple>
#include <stdexcept>
#include <cctype>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/utility/lite_string.hpp"
#include "alisp/utility/macros.hpp"
#include "alisp/utility/hash.hpp"


namespace alisp
{


namespace detail
{

enum Alphabet
{
    id_alphabet = 0,
    whitespace_alphabet,
};

constexpr size_t LENGTHOF_ALPHABET = 256;
constexpr size_t ALPHABETS_NUM = 2;

struct Position
{
    Position() = default;

    Position(const char * t_pos, const char * t_end) noexcept;

    static std::string_view str(const Position &begin, const Position &end) noexcept;

    Position &operator++() noexcept;

    Position &operator--() noexcept;

    const char& operator*() const noexcept;

    bool operator==(const Position &rhs) const noexcept;

    bool operator!=(const Position &rhs) const noexcept;


    Position &operator+=(size_t distance) noexcept;

    Position operator+(size_t distance) const noexcept;


    bool has_more() const noexcept;

    size_t remaining() const noexcept;


    size_t line = 0;
    size_t col = 0;

  private:
    const char *pos = nullptr;
    const char *end = nullptr;
    size_t last_col = 0;

};


struct DepthTracker
{

    DepthTracker(size_t& depth);
    ~DepthTracker();

  private:
    size_t& m_depth;
};


constexpr size_t MAX_DEPTH = 256;

}


namespace
{

class parse_exception : public std::runtime_error
{
  public:

    parse_exception(const std::string& t_why, const FileLocation& t_where, const std::string& t_input) :
        runtime_error(format(t_why, t_where, t_input))
    {}


  private:
    static std::string format(const std::string& t_why, const FileLocation& t_where, const std::string& t_input)
    {
        const static int LINE_CONTEXT = 1;
        std::ostringstream ss;

        ss << "\t" << "In file \'" << t_where.file << '\'' << ": " << "line: " << t_where.line << ", col: " << t_where.col << '\n';
        ss << "\t" << t_why << "\n";

        auto lines = utility::split(t_input, '\n');

        auto start_index = static_cast<int>(t_where.line) - LINE_CONTEXT < 0 ? 0 : static_cast<int>(t_where.line) - LINE_CONTEXT;
        auto end_index = (static_cast<int>(t_where.line) + LINE_CONTEXT) > static_cast<int>(std::size(lines)) ?
            static_cast<int>(std::size(lines)) : static_cast<int>(std::size(lines)) + LINE_CONTEXT;

        for (auto i = static_cast<size_t>(start_index); i < static_cast<size_t>(end_index); ++i) {
            ss << "\t" <<  i << " |" << "\t" << lines[i] << "\n";
        }

        return ss.str();
    }

};


}


#define PARSE_ERROR(MSG) do{throw parse_exception(MSG, FileLocation{position.col, position.line, *m_file}, *m_input);}while(0)

namespace parser
{

template <class Environment>
class ALParser : public ParserBase
{
  private:

    detail::Position position;
    size_t depth;
    std::shared_ptr<std::string> m_file;
    const std::string* m_input;

    // const ErrorHandler& err;
    Environment& env;


    constexpr static utility::LiteString cr_lf{"\r\n"};
    constexpr static utility::LiteString nl{"\n"};

    template<typename Array2D, typename First, typename Second>
    constexpr static void set_alphabet(Array2D &array, const First first, const Second second) noexcept
    {
        auto *first_ptr = &std::get<0>(array) + static_cast<std::size_t>(first);
        auto *second_ptr = &std::get<0>(*first_ptr) + static_cast<std::size_t>(second);
        *second_ptr = true;
    }

    constexpr bool char_in_alphabet(char c, detail::Alphabet a) const noexcept
    {
        return this->alphabet[a][static_cast<uint8_t>(c)];
    }

    constexpr static std::array<std::array<bool, detail::LENGTHOF_ALPHABET>, detail::ALPHABETS_NUM> build_alphabet() noexcept
    {
        std::array<std::array<bool, detail::LENGTHOF_ALPHABET>, detail::ALPHABETS_NUM> alph{};

        set_alphabet(alph, detail::whitespace_alphabet, ' ');
        set_alphabet(alph, detail::whitespace_alphabet, '\t');
        set_alphabet(alph, detail::whitespace_alphabet, '\n');

        for ( size_t c = 'a' ; c <= 'z' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
        for ( size_t c = 'A' ; c <= 'Z' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
        for ( size_t c = '0' ; c <= '9' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
        set_alphabet(alph, detail::id_alphabet, '-');
        set_alphabet(alph, detail::id_alphabet, '_');
        set_alphabet(alph, detail::id_alphabet, '+');
        set_alphabet(alph, detail::id_alphabet, '-');
        set_alphabet(alph, detail::id_alphabet, '*');
        set_alphabet(alph, detail::id_alphabet, '/');
        set_alphabet(alph, detail::id_alphabet, '=');
        set_alphabet(alph, detail::id_alphabet, '$');
        set_alphabet(alph, detail::id_alphabet, '%');
        set_alphabet(alph, detail::id_alphabet, '?');
        set_alphabet(alph, detail::id_alphabet, '|');
        set_alphabet(alph, detail::id_alphabet, '@');
        set_alphabet(alph, detail::id_alphabet, ':');
        set_alphabet(alph, detail::id_alphabet, '?');
        set_alphabet(alph, detail::id_alphabet, '<');
        set_alphabet(alph, detail::id_alphabet, '>');


        return alph;
    }

    constexpr static auto alphabet = build_alphabet();

  public:


    ALParser(Environment& env_) : env(env_)
    {
    }


    ALParser(const ALParser&) = delete;
    ALParser(ALParser&&) = default;
    ALParser &operator=(const ALParser&) = delete;
    ALParser &operator=(ALParser&&) = delete;

  private:

    void skip_whitespace(){

        while (this->position.has_more() && char_in_alphabet(*position, detail::whitespace_alphabet))
        {
            ++position;
        }
    }

    void skip_eol(){
        while(position.has_more() && check_char(*nl.c_str()))
        {
            ++position;
        }
    }

    bool check_char(const char c)
    {
        return *this->position == c;
    }

    bool check_num()
    {
        auto temp = position;

        if (check_char('-') || check_char('+')) {
            ++position;
        }
        bool val = false;

        bool dot_found = false;
        bool e_found = false;
        bool e_minus_found = false;
        while (position.has_more() && !char_in_alphabet(*position, detail::whitespace_alphabet))
        {

            if (check_char('.') && !dot_found) {
                dot_found = true;
                ++position;
                continue;
            }

            if (check_char('e') && !e_found) {
                e_found = true;
                ++position;
                continue;
            }
            if (check_char('-') && !e_minus_found && e_found) {
                e_minus_found = true;
                ++position;
                continue;
            }

            if ((std::isdigit(*position) == 0) && !val) {
                position = temp;
                return false;
            } else {
                position = temp;
                return true;
            }

            if (check_char('.') or check_char('e') or check_char('.') or check_char('-')){
                position = temp;
                return false;
            }
            
            val = true;
            ++position;
        }
        position = temp;
        return val;
    }

    void skip_line()
    {
        auto current_line = position.line;
        while (position.has_more() && !check_char(*nl.c_str())) {
            ++position;
            if (current_line != position.line) return;
        }

    }

    bool check_id()
    {
        bool val = false;
        auto temp = position;
        while(position.has_more() && char_in_alphabet(*position, detail::id_alphabet)
              && !char_in_alphabet(*position, detail::whitespace_alphabet))
        {
            ++position;
            val = true;
        }

        position = temp;
        return val;
    }

    ALObject* parse_id()
    {
        auto temp = this->position;
        while(this->position.has_more() && char_in_alphabet(*this->position, detail::id_alphabet))
        {
            ++this->position;
        }

        const auto word = detail::Position::str(temp, this->position);

        auto word_hash = hash::hash(word);

        switch(word_hash){
          case hash::hash("--FILE--"): return make_string(*m_file);
          case hash::hash("--LINE--"): return make_int(position.line);
        }

        return env::intern(std::string(word));
    }

    ALObject* parse_string()
    {

        if(*this->position != '\"') {PARSE_ERROR("Invalid string literal.");}

        ++this->position;

        auto temp = this->position;

        while(this->position.has_more() && *this->position != '\"' )
        {
            ++this->position;
            if ( *this->position == '\\') { this->position += 2; }
        }

        if (*this->position != '\"')
        {
            PARSE_ERROR("Invalid string literal.");
        }
        const auto text = detail::Position::str(temp, this->position);
        ++this->position;

        return make_string(std::string{text});

    }

    ALObject* parse_number()
    {

        int sign = 1;
        bool real = false;


        auto t = this->position;
        if (*this->position == '-')
        {
            sign = -1;
            ++this->position;
        }

        if (*this->position == '+') { ++this->position; }

        if (!std::isdigit(*this->position) && *this->position != '.')
        {
            this->position = t;
            PARSE_ERROR("Invalid number literal.");
        }

        auto temp = this->position;
        if (*this->position == '.')
        {
            ++this->position;
            real = true;
        }

        while(this->position.has_more() && std::isdigit(*this->position))
        {
            ++this->position;
            if (*this->position == '.')
            {
                if (real)
                {
                    PARSE_ERROR("Invalid number literal.");
                }
                real = true;
                ++this->position;
            }
        }

        const auto res = detail::Position::str(temp, this->position);

        if (real) {
            double num = static_cast<double>(sign) * std::stod( std::string(res) );
            return make_double(num);
        } else {
            int num = sign * std::stoi( std::string(res) );
            return make_int(num);
        }
    }

    ALObject* parse_quote()
    {
        if (!check_char('\'')) { PARSE_ERROR("Expected \'"); }
        ++position;
        skip_whitespace();
        auto obj = parse_next();
        if (!obj) { PARSE_ERROR("Expected expression after \'"); }
        return make_object(make_symbol("quote"), obj);
    }

    ALObject* parse_backquote()
    {
        ++position;
        skip_whitespace();
        if (!check_char('`')) { PARSE_ERROR("Expected \'`\'"); }
        auto obj = parse_next();
        if (!obj) { PARSE_ERROR("Expected expression after \'`\'"); }
        return make_object(make_symbol("`"), obj);
    }

    ALObject* parse_hashtag()
    {
        if (!check_char('#')) { PARSE_ERROR("Expected \'#\'"); }
        ++position;
        if (!check_char('\'')) { PARSE_ERROR("Expected \'"); }        
        ++position;
        skip_whitespace();
        auto obj = parse_next();
        if (!obj ) { PARSE_ERROR("Expected expression after \'"); }
        return make_object(make_symbol("function"), obj);
    }

    ALObject* parse_list()
    {
        // TODO : check char here
        if (!check_char('(')) { PARSE_ERROR("Malformed list. Missing openning parentheses."); }

        ++position;
        skip_whitespace();

        if(check_char(')')){
            ++position;
            return Qnil;
        }

        std::vector<ALObject*> objs;
        while(true){

            auto next_obj = parse_next();
            if(next_obj){
                objs.emplace_back(next_obj);
            } else {
                PARSE_ERROR("Malformed list. Cannot parse element.");
                return nullptr;
            }

            skip_whitespace();

            if(check_char(')')){
                ++position;
                break;
            }

            if (!position.has_more()) { PARSE_ERROR("Malformed list. Missing closing parentheses."); }

        }

        return make_object(objs);

    }

    ALObject* parse_comma()
    {
        if (!check_char(',')) { PARSE_ERROR("Expected comma"); }
        
        ++position;
        if (check_char('@')) {
            ++position;
            skip_whitespace();
            auto obj = parse_next();
            if ( !obj ) { PARSE_ERROR("Expected expression after \'@,\'"); }
            return make_object(make_symbol("@,"), obj);
        } else {
            skip_whitespace();
            auto obj = parse_next();
            if ( !obj ) { PARSE_ERROR("Expected expression after \',\'"); }
            return make_object(make_symbol(","), obj);
        }
    }

    ALObject* parse_next()
    {
        detail::DepthTracker dt{depth};
        if (depth > detail::MAX_DEPTH && detail::MAX_DEPTH != 0) {
            return nullptr;
        }

        skip_whitespace();
        if(!position.has_more()) return nullptr;

        while ( *position == ';' ) {
            skip_line();
            skip_whitespace();
        }
        
        if ( *position == '(' ) return parse_list();
        if ( *position == '\"') return parse_string();
        if ( *position == '\'') return parse_quote();
        if ( *position == ',' ) return parse_comma();
        if ( *position == '\'') return parse_quote();
        if ( *position == '`' ) return parse_backquote();
        if ( *position == '#' ) return parse_hashtag();
        if (  check_num()     ) return parse_number();
        if (  check_id( )     ) return parse_id();
        
        if( position.has_more()) { PARSE_ERROR("Unparsed input. Cannot parse.");}
        
        return nullptr;
    }


  public:

    std::vector<ALObject*> parse(const std::string* input, std::string file_name) override
    {
        std::vector<ALObject*> objects;

        const auto begin = input->empty() ? nullptr : &input->front();
        const auto end = begin == nullptr ? nullptr : begin + input->size();
        this->position = detail::Position(begin, end);

        m_file = std::make_shared<std::string>(std::move(file_name));
        m_input = input;

        depth = 0;

        while(position.has_more())
        {
            auto next_object = parse_next();
            if (next_object)
            {
                objects.emplace_back(next_object);
            }
        }

        return objects;

    }


};

}


#undef PARSE_ERROR

}
