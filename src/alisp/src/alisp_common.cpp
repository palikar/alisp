
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"

namespace alisp
{

operator ALObject::bool(){
    return is_truthy(this);
}


}
