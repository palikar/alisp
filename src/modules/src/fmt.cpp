#include <fmt/format.h>
#include <vector>
#include <type_traits>
#include <utility>

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"

namespace fmt
{
using namespace alisp;

namespace detail
{

enum class BlockType {
    LBRACE,
    RBRACE,
    FORMAT
};

struct FmtBlock
{
    size_t index;
    size_t size;
    BlockType t;
    int arg_index{-1};
    char fill;
    char align;
};

struct FmtParser
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

    static void consume_ws(const std::string &str, size_t &offset)
    {
        while (isspace(str.at(offset)) && offset <= str.size()) { ++offset; }
    }

    static FmtBlock parse_block(const std::string &str, size_t &offset)
    {
        size_t size = 0;
        int index = -1;
        char fill = '\0';
        char align= '\0';

        std::cout << str.at(offset) << "\n";
        if (std::isdigit(str.at(offset))) {
            
            size_t start = offset;
            while (std::isdigit(str.at(offset))) {
                ++offset;
                ++size;
            }
            size_t end = offset;
            index = std::stoi(str.substr(start, end));
        }

        if (str.at(offset) == ':') {
            ++offset;
            ++size;
            
            auto c_1 = str.at(offset);
            auto c_2 = str.at(offset + 1);

            if (c_2 == '<' or c_2 == '>' or c_2 == '^') {
                fill = c_1;
                align = c_2;
                ++offset;
                ++offset;
            }

            if (c_1 == '<' or c_1 == '>' or c_1 == '^') {
                align = c_1;
                ++offset;
            }

            auto c_3 = str.at(offset);
            if (c_1 == '+' or c_1 == '-') {
                ++offset;
            }

            if(str.at(offset) == '#') {
                ++offset;
            }

            if(str.at(offset) == '0') {
                ++offset;
            }

            if (std::isdigit(str.at(offset))) {
            }

            if(str.at(offset) == '.') {
                ++offset;
            }

            
            
            
            //parse the spec
        }
        
        return {offset, size+2, BlockType::FORMAT, index};
    }

    static std::vector<FmtBlock> parse_next(const std::string &str, size_t &offset)
    {
        std::vector<FmtBlock> blocks;
            
        consume_ws(str, offset);

        while (offset < std::size(str)) {

            if (offset + 1 < std::size(str)) {
                
                if (str.at(offset) == '{' and str.at(offset + 1) == '{') {
                    blocks.push_back({offset, 2, BlockType::LBRACE});
                    ++offset;
                    ++offset;
                    continue;
                }
            
                if (str.at(offset) == '}' and str.at(offset + 1) == '}') {
                    blocks.push_back({offset, 2, BlockType::RBRACE});
                    ++offset;
                    ++offset;
                    continue;
                }
            }
            

            if (str.at(offset) == '{') {
                ++offset;
                blocks.push_back(parse_block(str, offset));
                if (str.at(offset) != '}') {
                    //error!
                }
                ++offset;
                continue;
            }
            ++offset;
        }
        
        return blocks;
    }
};


inline auto parse_fmt_string(const std::string &str)
{
    size_t offset = 0;
    return FmtParser::parse_next(str, offset);
}

}

ALObjectPtr Ffmt(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);

    for (auto b :  detail::parse_fmt_string(str->to_string())){
        std::cout << "at "<< b.index << " for " << b.size;
        std::cout << " of type " << static_cast<int>(b.t) << " with arg_index " << b.arg_index <<"\n";
    };
        
    return Qt;
}

}

ALISP_EXPORT alisp::env::ModulePtr init_fmt(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mfmt = alisp::module_init("fmt");
    auto fmt_ptr = Mfmt.get();


    alisp::module_defun(fmt_ptr, "fmt", &fmt::Ffmt);

    return Mfmt;
}
