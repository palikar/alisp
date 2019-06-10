#pragma once

#include <string>
#include <vector>

#include "alisp/alisp/common_lexer.hpp"

namespace alisp
{



class ErrorMessanger
{
  private:

    std::string current_file;
    std::string current_input;
    std::vector<std::string> lines;
    
    bool file_input = false;

  public:


    ErrorMessanger();


    void set_input(std::string input);
    void set_file(std::string file);

    
    void lexer_error(size_t char_num,
                     size_t line_num,
                     const std::string& msg) const;

    
    void parser_error(const ALToken& token,
                      const std::string& msg) const;
    
    void runtime_error(const std::string& msg) const;
    
    
};







}
