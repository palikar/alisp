#include <fmt/format.h>
#include <vector>
#include <type_traits>
#include <utility>
#include <initializer_list>
#include <iomanip>
#include <bitset>

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"

namespace fmt
{
using namespace alisp;

namespace detail
{

enum class BlockType
{
    LBRACE,
    RBRACE,
    FORMAT
};

struct FmtBlock
{
    size_t index;
    size_t size;

    BlockType t;

    int arg_index{ -1 };
    char fill{ '\0' };
    char align{ '\0' };
    char sign{ '\0' };
    bool alternate{ false };
    bool null_form{ false };
    int width{ -1 };
    int precision{ -1 };
    char type{ '\0' };
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

    static auto inspect(const std::string &str, size_t offset, std::initializer_list<char> chars)
    {
        if (offset >= str.size() or str.at(offset) == '}') { return false; }
        for (auto c : chars)
        {
            if (c == str.at(offset)) { return true; }
        }
        return false;
    }

    static FmtBlock parse_block(const std::string &str, size_t &offset, size_t start_offset)
    {
        size_t size = 0;
        int index   = -1;
        char fill   = '\0';
        char align  = '\0';
        char sign   = '\0';
        bool alternate{ false };
        bool null_form{ false };
        int width     = -1;
        int precision = -1;
        char type     = '\0';

        if (std::isdigit(str.at(offset)) or inspect(str, offset, { '{' }))
        {

            size_t start = offset;
            while (std::isdigit(str.at(offset)))
            {
                ++offset;
                ++size;
            }
            size_t end = offset;
            index      = std::stoi(str.substr(start, end));
        }

        if (str.at(offset) == ':')
        {
            ++offset;
            ++size;

            if (inspect(str, offset + 1, { '<', '>', '^' }))
            {
                fill  = str.at(offset);
                align = str.at(offset + 1);
                offset += 2;
                size += 2;
            }

            if (inspect(str, offset, { '<', '>', '^' }))
            {
                align = str.at(offset);
                ++offset;
                ++size;
            }

            if (inspect(str, offset, { '-', '+' }))
            {
                sign = str.at(offset);
                ++offset;
                ++size;
            }

            if (inspect(str, offset, { '#' }))
            {
                alternate = true;
                ++offset;
                ++size;
            }

            if (inspect(str, offset, { '0' }))
            {
                null_form = true;
                ++offset;
                ++size;
            }

            if (std::isdigit(str.at(offset)) or inspect(str, offset, { '{' }))
            {

                size_t start = offset;
                while (std::isdigit(str.at(offset)))
                {
                    ++offset;
                    ++size;
                }
                size_t end = offset;
                width      = std::stoi(str.substr(start, end));
            }

            if (inspect(str, offset, { '.' }))
            {
                ++offset;

                if (std::isdigit(str.at(offset)))
                {
                    size_t start = offset;
                    while (std::isdigit(str.at(offset)))
                    {
                        ++offset;
                        ++size;
                    }
                    size_t end = offset;
                    precision  = std::stoi(str.substr(start, end));
                }
                else if (inspect(str, offset, { '{' }))
                {
                }
            }

            if (inspect(str,
                        offset,
                        { 'a', 'A', 'c', 'e', 'E', 'f', 'F', 'g', 'G', 'p', 's', 'b', 'B', 'd', 'n', 'o', 'x', 'X' }))
            {
                ++size;
                type = str.at(offset);
                ++offset;
            }
        }

        if (str.at(offset) != '}')
        {
            // error
        }

        return { start_offset, size + 2,  BlockType::FORMAT, index, fill,      align,
                 sign,         alternate, null_form,         width, precision, type };
    }

    static std::vector<FmtBlock> parse_next(const std::string &str, size_t &offset)
    {
        std::vector<FmtBlock> blocks;

        consume_ws(str, offset);

        while (offset < std::size(str))
        {

            if (offset + 1 < std::size(str))
            {

                if (str.at(offset) == '{' and str.at(offset + 1) == '{')
                {
                    blocks.push_back({ offset, 2, BlockType::LBRACE });
                    offset += 2;
                    continue;
                }

                if (str.at(offset) == '}' and str.at(offset + 1) == '}')
                {
                    blocks.push_back({ offset, 2, BlockType::RBRACE });
                    offset += 2;
                    continue;
                }
            }

            if (str.at(offset) == '{')
            {
                ++offset;
                blocks.push_back(parse_block(str, offset, offset - 1));
                if (str.at(offset) != '}')
                {
                    // error!
                }
                ++offset;
                continue;
            }

            ++offset;
        }

        for (auto b : blocks)
        {
            std::cout << "at " << b.index << " for " << b.size;
            std::cout << " of type " << static_cast<int>(b.t) << " with arg_index " << b.arg_index;
            std::cout << " with align " << b.align << " with fill " << b.fill << " with sign " << b.sign
                      << " with alternate  " << b.alternate << " with null " << b.null_form << " with width  "
                      << b.width << " with precision " << b.precision << " of type " << b.type << "\n";
        };


        return blocks;
    }
};

static auto parse_fmt_string(const std::string &str)
{
    size_t offset = 0;
    return FmtParser::parse_next(str, offset);
}

static std::string handle_format_block(FmtBlock &t_block, int &t_i, ALObjectPtr t_args)
{

    const auto index = static_cast<size_t>(t_block.arg_index == -1 ? t_i++ : t_block.arg_index);
    auto args_size   = std::size(*t_args);

    if (index + 1 >= args_size)
    {
        // error
    }

    auto obj = t_args->i(static_cast<size_t>(index + 1));

    auto base_str = [&]() {
        switch (t_block.type)
        {

          case 's':
          {
              if (!pstring(obj)) {}
              return obj->to_string();
          }
              
          case 'c':
          {
              if (!obj->check_char_flag() and !obj->is_int()) {}
              return std::string(1, char(obj->to_int()));
          }
              
          case 'b':
          case 'B':
          {
              if (!pint(obj)) {}
              std::cout << std::bitset<64>(obj->to_int()).to_string() << "\n";
              return std::bitset<64>(obj->to_int()).to_string();
          }

          case 'x':
          case 'X':
          {
              if (!pint(obj)) {}
              std::stringstream ss_x;
              ss_x << std::isupper(t_block.type) ? "0X" : "0x";
              ss_x << std::hex;
              ss_x << obj->to_int();
              return ss_x.str();          
          }

          case 'd':
          {
              if (!pint(obj)) {}
              return std::to_string(obj->to_int());
          }
              
          case 'n':
          {
              if (!pint(obj)) {}
              return std::to_string(obj->to_int());
          }
          
          case 'o':
          {
              if (!pint(obj)) {}
              std::stringstream ss_o;
              ss_o << std::isupper(t_block.type) ? "0X" : "0x";
              ss_o << std::oct;
              ss_o << obj->to_int();
              return ss_o.str();
          }

          case 'a':
          case 'A':
          {
              if (!preal(obj)) {}
              return std::string{ "" };
          }
              
          case 'e':
          case 'E':
          {
              if (!preal(obj)) {}
              return std::string{ "" };
          }
          
          case 'f':
          case 'F':
          {
              if (!preal(obj)) {}
              return std::string{ "" };
          }
          
          case 'g':
          case 'G':
          {
              if (!preal(obj)) {}
              return std::string{ "" };
          }
          
        }

        return to_string(obj);
    }();

    char fill = t_block.fill == '\0' ? ' ' : t_block.fill;

    if (t_block.width != -1)
    {
        if (t_block.align == '<')
        {
            auto s = std::string(t_block.width - base_str.size(), fill);
            return base_str += s;
        }
        else if (t_block.align == '>')
        {
            auto s = std::string(t_block.width - base_str.size(), fill);
            return s += base_str;
        }
        else if (t_block.align == '^')
        {
            auto s = std::string(t_block.width / 2 - base_str.size(), fill);
            return (std::string(s) += base_str) += s;
        }
    }
    else
    {
        return base_str;
    }


    return "";
}

static std::string expand_string(std::string t_str, std::vector<FmtBlock> &&t_blocks, ALObjectPtr t_args)
{
    int i      = 0;
    size_t acc = 0;

    for (auto &bl : t_blocks)
    {
        if (bl.t == BlockType::LBRACE)
        {
            t_str.replace(bl.index - acc, bl.size, "{");
            acc += bl.size - 1;
        }
        else if (bl.t == BlockType::RBRACE)
        {
            t_str.replace(bl.index - acc, bl.size, "}");
            acc += bl.size - 1;
        }
        else if (bl.t == BlockType::FORMAT)
        {
            auto str = handle_format_block(bl, i, t_args);
            t_str.replace(bl.index - acc, bl.size, str);
            acc += bl.size - str.size();
        }
    }

    return t_str;
}

static auto format_string(const std::string &t_format, ALObjectPtr t_args)
{
    return expand_string(t_format, parse_fmt_string(t_format), t_args);
}

}  // namespace detail

ALObjectPtr Ffmt(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    auto res = detail::format_string(str->to_string(), args);
    return make_string(res);
}

}  // namespace fmt

ALISP_EXPORT alisp::env::ModulePtr init_fmt(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mfmt    = alisp::module_init("fmt");
    auto fmt_ptr = Mfmt.get();


    alisp::module_defun(fmt_ptr, "fmt", &fmt::Ffmt);

    return Mfmt;
}
