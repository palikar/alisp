/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */





#pragma once

#include <type_traits>
#include <iterator>
#include <vector>
#include <utility>


namespace alisp::utility
{

template <class, class Enable = void> struct is_iterator : std::false_type {};
template <typename T> 
struct is_iterator<T, typename std::enable_if< std::is_base_of<std::input_iterator_tag, typename std::iterator_traits<T>::iterator_category>::value ||
                                               std::is_same<std::output_iterator_tag, typename std::iterator_traits<T>::iterator_category>::value 
                                               >::type> : std::true_type {};


template<typename T>
constexpr bool is_iterator_v = is_iterator<T>::value;

template<typename T>
class vector_view
{

  public:
          
    using value_type = typename std::vector<T>::value_type;
    using size_type = typename std::vector<T>::size_type;
    using difference_type = typename std::vector<T>::difference_type;
    using pointer = typename std::vector<T>::pointer;
    using const_pointer = typename std::vector<T>::const_pointer;
    using reference = typename std::vector<T>::reference;
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;
    using reverse_iterator = typename std::vector<T>::reverse_iterator;
    using const_reverse_iterator = typename std::vector<T>::const_reverse_iterator;

  private:
    iterator m_first;
    iterator m_last;
    

  public:

    vector_view() {}

    explicit vector_view(std::vector<T>& t_vec) :
        m_first(std::begin(t_vec)),  m_last(std::end(t_vec))
    {}

    vector_view(typename std::vector<T>::iterator t_begin, typename std::vector<T>::iterator t_end) :
        m_first(t_begin),  m_last(t_end)
    {}

    reference operator[](size_t index)
    {
        return *std::next(m_first, static_cast<difference_type>(index));
        
    }

    const reference operator[](size_t index) const
    {
        return *std::next(m_first, static_cast<difference_type>(index));
        
    }

    size_type size() const
    {
        return static_cast<size_type>(std::distance(m_first, m_last));
    }

    iterator begin() { return m_first; }
    iterator end() { return m_last; }

    const_iterator cbegin() { return m_first; }
    const_iterator cend() { return m_last; }

    const_iterator begin() const { return m_first; }
    const_iterator end() const { return m_last; }

    value_type front() { return *m_first; }
    value_type back() { return *std::next(m_last, -1); }

    bool empty() const { return size() == 0; }    

};

template<typename T>
vector_view(std::vector<T>& t_vec) -> vector_view<T>;

template<typename It, typename = std::enable_if<is_iterator_v<It>>>
vector_view(It t_begin, It t_end) -> vector_view<typename std::iterator_traits<It>::value_type>;

}
