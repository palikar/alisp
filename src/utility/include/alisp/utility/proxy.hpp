#include <utility>
#include <tuple>
#include <vector>
#include <iostream>
#include <iterator>

namespace alisp::utility
{
namespace detail
{


template<std::size_t I = 0, typename FuncT, typename... Tp>
inline std::enable_if_t<I == sizeof...(Tp), void>
for_each(std::tuple<Tp...> &, FuncT)
{ }

template<std::size_t I = 0, typename FuncT, typename... Tp>
inline std::enable_if_t<I < sizeof...(Tp), void>
for_each(std::tuple<Tp...>& t, FuncT f)
{
    f(std::get<I>(t));
    for_each<I + 1, FuncT, Tp...>(t, f);
};
    

template<typename T, typename...Tracker>
struct proxy{

  public:

    explicit proxy(T& obj, Tracker && ... track) :
        m_obj(&obj), m_track(std::forward<decltype(track)>(track)...)  {
        (track.before(obj), ...);
        
    }

    ~proxy(){
        detail::for_each(m_track, [&](auto& tracker){tracker.after(*m_obj);});
    }

    T* operator ->() { return m_obj;}

  private:
    T* m_obj;
    std::tuple<Tracker ...> m_track;
};

}

template<typename T, typename ...Tracker>
class Tracked : public Tracker ... {
  public :
    explicit Tracked(T& obj) : m_obj(obj) {}

    detail::proxy<T, Tracker...> operator ->() {
        return detail::proxy<T, Tracker...>(m_obj, static_cast<Tracker>(*this)...);
    }
    
  private :
    T& m_obj;

};


}
