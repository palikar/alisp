#include <vector>
#include <string>


#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"

namespace alisp
{
	template class ALLexer<ErrorMessanger>;



	using SimpleParser = ALLexer<ErrorMessanger>;
}
