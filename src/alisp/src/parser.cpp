#include "alisp/alisp/parser.hpp"


namespace alisp
{


ALParser::ALParser(const std::vector<alisp::Token>& tokens_) :
    tokens(tokens_),
    current_token(0)
{}


void ALParser::nextToken()
{
    ++(this->current_token);
}

Token ALParser::peek()
{
    return this->tokens[this->current_token+1];
}
    
std::optional<Token> ALParser::currentToken()
{
    if (this->current_token >= tokens.size())
    {
        return std::nullopt;
    }
    return this->tokens[this->current_token];
}

std::vector<Object*> ALParser::parseWhole()
{
    std::vector<Object*> objs;
    while (this->current_token < this->tokens.size())
    {
        objs.push_back(parse());
    }
    return objs;
}

Object* ALParser::parse()
{
        
    auto tok = currentToken();
    if (!tok)
        return nullptr;

    Token& token = tok.value();
        
    switch (token.getType()) {
            
      case TokenType::LEFT_BRACKET: {
          Object *list = new Object(ObjectType::LIST);
              
          list->content = std::vector<Object*>();
          auto& obj_list = std::get<std::vector<Object*>>(list->content);

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
                  
              Object* next_obj = parse();
              if(next_obj != nullptr)
              {
                  obj_list.push_back(next_obj);
              }                  
                           
          }
          return list;
      }
              
      case TokenType::STRING : {
          Object *str = new Object(ObjectType::STRING);
          str->content = token.getContentAs<std::string>();
          nextToken();
          return str;
      }
              
      case TokenType::NUMBER : {
          Object *num = new Object(ObjectType::INT);
          num->content = token.getContentAs<int>();
          nextToken();
          return num;
      }

          case TokenType::REAL_NUMBER : {
              Object *num = new Object(ObjectType::REAL);
              num->content = token.getContentAs<float>();
              nextToken();
              return num;
          }
              
          case TokenType::ID : {
              Object *symbol = new Object(ObjectType::SYMBOL);
              symbol->content = token.getContentAs<std::string>();
              nextToken();
              return symbol;
          }

          case TokenType::RIGHT_BRACKET : {
              std::cout << "Unexpected right bracket" << "\n";
              exit(1);
          }

          case TokenType::QUOTE : {
              Object *cell = new Object(ObjectType::CELL);

              Cell cell_obj{};

              cell_obj.con = new Object(ObjectType::SYMBOL);
              cell_obj.con->content = std::string{"quote"};

              nextToken();
              cell_obj.cdr = parse();

              cell->content = cell_obj;

              return cell;              
          }

              

              
              
          default:
              return nullptr;
        }
        
        return nullptr;
    }



}
