#include "alisp/alisp/error_messaging.hpp"

#include <utility>
#include <iostream>

#include "absl/strings/substitute.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_split.h"

#include <rang.hpp>



namespace alisp
{

static const size_t LINES_CONTEXT = 2;
// static const size_t CHARS_CONTEXT = 15;


ErrorMessanger::ErrorMessanger()
{

};


void ErrorMessanger::set_input(std::string input)
{
    this->current_input = std::move(input);
    this->lines = absl::StrSplit(this->current_input, '\n');
}


void ErrorMessanger::set_file(std::string file)
{
    if (file.empty())
    {
        this->file_input = false;
    }
    else
    {
        this->current_file = std::move(file);
        this->file_input = true;
    }
}

    
void ErrorMessanger::lexer_error(size_t char_num,
                                 size_t line_num,
                                 const std::string& msg) const
{

    std::cout << rang::fg::red << "Lexer error:" << rang::fg::reset;
    
    std::string error_msg{};
    if(this->file_input)
    {
        error_msg += absl::Substitute(" in file $0", this->current_file);
    }    
    error_msg += absl::Substitute(" on line $0 char $1\n=======> $2\n", char_num, line_num, msg);
    
    
    const size_t start_line = line_num < LINES_CONTEXT ? 0 : line_num - LINES_CONTEXT;
    const size_t end_line = (start_line + 2*LINES_CONTEXT) >= this->lines.size() ?
        (this->lines.size() - 1) : (start_line + 2*LINES_CONTEXT);

    std::cout << error_msg;
    
    for (size_t i = start_line; i <= end_line; ++i)
    {
        if (i == line_num)
        {
            std::cout << '>' << rang::fg::magenta << this->lines[i] << rang::fg::reset << '\n';
        }
        else
        {
            std::cout << this->lines[i] << '\n';
        }
    }

}


void ErrorMessanger::parser_error(const ALToken& token,
                                  const std::string& msg) const
{

}

void ErrorMessanger::runtime_error(const std::string& msg) const
{


}


}

