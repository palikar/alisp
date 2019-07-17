#include <vector>
#include <string>


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_lexer.hpp"

namespace alisp
{
	template class ALLexer<ErrorMessanger>;



	using SimpleParser = ALLexer<ErrorMessanger>;
}
