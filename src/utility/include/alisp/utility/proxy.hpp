#include <utility>
#include <tuple>
#include <vector>
#include <iostream>
#include <iterator>

#include "alisp/utility/helpers.hpp"

namespace alisp::utility
{
namespace detail
{

template<typename T, typename...Tracker>
struct proxy
{

  public:

    proxy(T& obj, Tracker && ... track) :
        m_obj(&obj), m_track(std::forward<decltype(track)>(track)...)  {
        (track.before(obj), ...);
        
    }

    ~proxy(){
        ::alisp::utility::for_each(m_track, [&](auto& tracker){tracker.after(*m_obj);});
    }

    T* operator ->() { return m_obj;}

  private:
    T* m_obj;
    std::tuple<Tracker ...> m_track;};

}

template<typename T, typename ...Tracker>
class Tracked : public Tracker ...
{
  public :
    explicit Tracked(T& obj) : m_obj(obj) {}

    detail::proxy<T, Tracker...> operator ->() {
        return detail::proxy<T, Tracker...>(m_obj, static_cast<Tracker>(*this)...);
    }
    
  private :
    T& m_obj;

};

}
