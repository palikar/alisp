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

#include <fmt/format.h>
#include <vector>
#include <type_traits>
#include <utility>
#include <initializer_list>
#include <iomanip>
#include <bitset>
#include <algorithm>
#include <locale>

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"

namespace fmt
{
using namespace alisp;

auto fmt_signal = alisp::make_symbol("fmt-signal");

namespace detail
{
template<typename T> auto int_to_bin(T num)
{
    std::string str{ "" };
    while (num > 0)
    {
        str += num % 2 == 1 ? "1" : "0";
        num /= 2;
    }
    std::reverse(str.begin(), str.end());
    return str;
}

template<typename T> auto int_to_hex(T num)
{
    constexpr static char digits[] = "0123456789ABCDEF";
    std::string str{ "" };
    while (num > 0)
    {
        str += digits[num % 16];
        num /= 16;
    }
    std::reverse(str.begin(), str.end());
    return str;
}

template<typename T> auto int_to_oct(T num)
{
    constexpr static char digits[] = "01234567";
    std::string str{ "" };
    while (num > 0)
    {
        str += digits[num % 8];
        num /= 8;
    }
    std::reverse(str.begin(), str.end());
    return str;
}

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
        while (isspace(str.at(offset)) && offset <= str.size())
        {
            ++offset;
        }
    }

    static auto inspect(const std::string &str, size_t offset, std::initializer_list<char> chars)
    {
        if (offset >= str.size() or str.at(offset) == '}')
        {
            return false;
        }
        for (auto c : chars)
        {
            if (c == str.at(offset))
            {
                return true;
            }
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
            signal(fmt_signal, "FMT ERROR: Invalid syntax in format string"s, "Unexpected symbol:"s += str.at(offset));
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
                    signal(fmt_signal, "FMT ERROR: Invalid syntax in format string"s, "Missing closing bracket \'}\'"s);
                }
                ++offset;
                continue;
            }


            ++offset;
        }

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
        signal(fmt_signal, "FMT ERROR: Not enough arguments provided"s, index + 1, dump(t_args));
    }

    auto obj = t_args->i(index + 1);

    auto sign = [&](auto t_obj) {
        if (t_block.sign == '+')
        {
            return t_obj->to_real() > 0 ? '+' : '-';
        }
        else if (t_block.sign == '-')
        {
            return t_obj->to_real() < 0 ? '-' : '\0';
        }
        else if (t_block.sign == ' ')
        {
            return t_obj->to_real() < 0 ? '-' : ' ';
        }
        else
        {
            return '\0';
        }
    };

    auto base_str = [&]() {
        switch (t_block.type)
        {

            case 's': {
                if (!pstring(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected string"s, dump(obj));
                }
                return obj->to_string();
            }

            case 'c': {
                if (!obj->check_char_flag() and !obj->is_int())
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected char"s, dump(obj));
                }
                return std::string(1, char(obj->to_int()));
            }

            case 'b':
            case 'B': {
                if (!pint(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected int"s, dump(obj));
                }
                return std::string{ sign(obj) } += (std::isupper(t_block.type) ? "0B" : "0b"s) +=
                       int_to_bin(obj->to_int());
            }

            case 'x':
            case 'X': {
                if (!pint(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected int"s, dump(obj));
                }
                return std::string{ sign(obj) } += (std::isupper(t_block.type) ? "0X" : "0x"s) +=
                       int_to_hex(obj->to_int());
            }

            case 'o': {
                if (!pint(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected int"s, dump(obj));
                }
                return (std::string{ sign(obj) } += "0") += int_to_oct(obj->to_int());
            }

            case 'd': {
                if (!pint(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected int"s, dump(obj));
                }
                return std::string{ sign(obj) } += std::to_string(obj->to_int());
            }

            case 'n': {
                if (!pint(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected int"s, dump(obj));
                }
                std::ostringstream ss_n;
                ss_n.imbue(std::locale());
                ss_n << std::string{ sign(obj) };
                if (t_block.precision == -1)
                {
                    ss_n << std::setprecision(t_block.precision);
                }
                ss_n << obj->to_real();
                return ss_n.str();
            }

            case 'a':
            case 'A': {
                if (!preal(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected real"s, dump(obj));
                }
                std::ostringstream ss_a;
                ss_a << std::string{ sign(obj) };
                if (t_block.precision == -1)
                {
                    ss_a << std::setprecision(t_block.precision);
                }
                ss_a << obj->to_real();
                return ss_a.str();
            }

            case 'e':
            case 'E': {
                if (!preal(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected real"s, dump(obj));
                }
                std::ostringstream ss_e;
                ss_e << std::string{ sign(obj) };
                if (t_block.precision == -1)
                {
                    ss_e << std::scientific;
                }
                ss_e << obj->to_real();
                return ss_e.str();
            }

            case 'f':
            case 'F': {
                if (!preal(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected real"s, dump(obj));
                }
                std::ostringstream ss_f;
                ss_f << std::string{ sign(obj) };
                if (t_block.precision == -1)
                {
                    ss_f << std::fixed << std::setprecision(t_block.precision);
                }
                ss_f << obj->to_real();
                return ss_f.str();
            }

            case 'g':
            case 'G': {
                if (!preal(obj))
                {
                    signal(fmt_signal, "FMT ERROR: Invalid object"s, "Expected real"s, dump(obj));
                }
                std::ostringstream ss_g;
                ss_g << std::string{ sign(obj) };
                if (t_block.precision == -1)
                {
                    ss_g << std::setprecision(t_block.precision);
                }
                ss_g << obj->to_real();
                return ss_g.str();
            }
        }

        return to_string(obj);
    }();

    char fill = t_block.fill == '\0' ? ' ' : t_block.fill;

    if (t_block.width != -1)
    {
        if (t_block.align == '<')
        {
            auto s = std::string(static_cast<size_t>(t_block.width) - base_str.size(), fill);
            return base_str += s;
        }
        else if (t_block.align == '>')
        {
            auto s = std::string(static_cast<size_t>(t_block.width) - base_str.size(), fill);
            return s += base_str;
        }
        else if (t_block.align == '^')
        {
            auto s = std::string(static_cast<size_t>(t_block.width) / 2 - base_str.size(), fill);
            return (s += base_str) += s;
        }
    }

    return base_str;
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

static std::string format_string(const std::string &t_format, ALObjectPtr t_args)
{
    return expand_string(t_format, parse_fmt_string(t_format), t_args);
}

}  // namespace detail

ALObjectPtr Ffmt(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    return make_string(detail::format_string(str->to_string(), args));
}

ALObjectPtr Fprintf(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    auto res = detail::format_string(str->to_string(), args);
    std::cout << res;
    al::cout << res;
    return make_string(res);
}

ALObjectPtr Fprintfln(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    auto res = detail::format_string(str->to_string(), args);
    std::cout << res << '\n';
    al::cout << res << '\n';
    return make_string(res);
}


ALObjectPtr Feprintf(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    auto res = detail::format_string(str->to_string(), args);
    al::cerr << res;
    return make_string(res);
}

ALObjectPtr Feprintfln(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);
    auto args = eval_transform(eval, obj);

    auto str = args->i(0);
    assert_string(str);

    auto res = detail::format_string(str->to_string(), args);
    al::cerr << res << '\n';
    return make_string(res);
}

}  // namespace fmt

ALISP_EXPORT alisp::env::ModulePtr init_fmt(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mfmt    = alisp::module_init("fmt");
    auto fmt_ptr = Mfmt.get();

    alisp::module_doc(fmt_ptr, R"(The `fmt` module helps you format strings.

The formating is based on the python's [Format Specification Mini-Language](https://docs.python.org/3.4/library/string.html#formatspec)
as well as the C++ library [FMT](https://fmt.dev/latest/syntax.html). For most the things, internally alisp uses the mentioned library, but
the `fmt` module implements its own formating from scratch.

The syntax for the formating strings is as close to [FMT's syntax](https://fmt.dev/latest/syntax.html) as possible.
In most cases you can simply use `{}` for a replacement field. For further detail you can check out the link with the FMT's syntax.

To note is that the `printf` and `fmt` functions in the `fmt` module use the same syntax.

)");

    alisp::module_defun(fmt_ptr,
                        "fmt",
                        &fmt::Ffmt,
                        R"((fmt FORMAT_STRING [ARG]...)

Return a string resulting from the formating FORMAT_STRING with the
given arguments.

```elisp
(fmt "this is formated wiht {} and {}" 42 "some words")
```
)");

    alisp::module_defun(fmt_ptr,
                        "printf",
                        &fmt::Fprintf,
                        R"((printf FORMAT_STRING [ARG]...)

Print the string FORMAT_STRING formated with the given arguments on
the standard output.

)");

    alisp::module_defun(fmt_ptr,
                        "printfln",
                        &fmt::Fprintfln,
                        R"((printf FORMAT_STRING [ARG]...)

Print the string FORMAT_STRING formated with the given arguments on
the standard output followed by a new line.
)");

    alisp::module_defun(fmt_ptr,
                        "eprintf",
                        &fmt::Feprintf,
                        R"((eprintf FORMAT_STRING [ARG]...)

Print the string FORMAT_STRING formated with the given arguments on
the standard error stream.
)");

    alisp::module_defun(fmt_ptr,
                        "eprintfln",
                        &fmt::Feprintfln,
                        R"((eprintfln FORMAT_STRING [ARG]...)

Print the string FORMAT_STRING formated with the given arguments on
the standard error stream followed by a new line.
)");


    return Mfmt;
}
