#include "alisp/alisp/alisp_parser.hpp"


namespace alisp
{


ALParser::ALParser(const std::vector<alisp::ALToken>& tokens_) :
    tokens(tokens_),
    current_token(0)
{}
void ALParser::nextToken()
{
    ++(this->current_token);
}

ALToken ALParser::peek()
{
    return this->tokens[this->current_token+1];
}
    
std::optional<ALToken> ALParser::currentToken()
{
    if (this->current_token >= tokens.size())
    {
        return std::nullopt;
    }
    return this->tokens[this->current_token];
}

std::vector<ALObject*> ALParser::parseWhole()
{
    std::vector<ALObject*> objs;
    while (this->current_token < this->tokens.size())
    {
        objs.push_back(parse());
    }
    return objs;
}

ALObject* ALParser::parse()
{
        
    auto tok = currentToken();
    if (!tok)
        return nullptr;

    ALToken& token = tok.value();
        
    switch (token.getType()) {
            
      case TokenType::LEFT_BRACKET: {
          ALObject *list = new ALObject(ObjectType::LIST);
              
          list->content = std::vector<ALObject*>();
          auto& obj_list = std::get<std::vector<ALObject*>>(list->content);

          nextToken();
          while(true)
          {
              tok = currentToken();
              if(!tok)
              {
                  std::cout << "Malformed sexp!" << "\n";
                  exit(1);
              }

              token = tok.value();
                  
              if(token.getType() == TokenType::RIGHT_BRACKET)
              {
                  nextToken();
                  break;
              }
                  
              ALObject* next_obj = parse();
              if(next_obj != nullptr)
              {
                  obj_list.push_back(next_obj);
              }                  
                           
          }
          return list;
      }
              
      case TokenType::STRING : {
          ALObject *str = new ALObject(ObjectType::STRING);
          str->content = token.getContentAs<std::string>();
          nextToken();
          return str;
      }
              
      case TokenType::NUMBER : {
          ALObject *num = new ALObject(ObjectType::INT);
          num->content = token.getContentAs<int>();
          nextToken();
          return num;
      }

          case TokenType::REAL_NUMBER : {
              ALObject *num = new ALObject(ObjectType::REAL);
              num->content = token.getContentAs<float>();
              nextToken();
              return num;
          }
              
          case TokenType::ID : {
              ALObject *symbol = new ALObject(ObjectType::SYMBOL);
              symbol->content = token.getContentAs<std::string>();
              nextToken();
              return symbol;
          }

          case TokenType::RIGHT_BRACKET : {
              std::cout << "Unexpected right bracket" << "\n";
              exit(1);
          }

          case TokenType::QUOTE : {
						
              // ALObject *cell = new ALObject(ObjectType::CELL);

              // Cell cell_obj{};

              // cell_obj.con = new ALObject(ObjectType::SYMBOL);
              // cell_obj.con->content = std::string{"quote"};

              // nextToken();
              // cell_obj.cdr = parse();

              // cell->content = cell_obj;

              // return cell;              
          }

              

              
              
          default:
              return nullptr;
        }
        
        return nullptr;
    }



}
