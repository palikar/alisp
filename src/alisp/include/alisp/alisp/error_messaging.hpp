
#include <string>
#include <vector>

#include "alisp/alisp/common_lexer.hpp"

namespace alisp
{



class Name
{
  private:

    std::string current_file;
    std::string current_input;


  public:
    Name();

    
    void lexer_error(int char_num,
                     int line_num,
                 const std::string& msg,
                 const std::vector<std::string> input);

    void parser_error(const Token& token,
                      const std::string& msg);

    void runtime_error(const std::string& msg);
    
    
};







}
