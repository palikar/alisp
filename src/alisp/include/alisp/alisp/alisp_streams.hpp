#pragma once

#include "alisp/management/registry.hpp"
#include "alisp/streams/streams.hpp"


namespace alisp
{


namespace al
{

streams::ALStream& cout = *streams::CoutStream::get_instance();
streams::ALStream& cin = *streams::CinStream::get_instance();



}


}

