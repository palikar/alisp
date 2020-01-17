#pragma once

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>


namespace alisp::utility {

inline std::vector<std::string> split(const std::string& s, char delimiter)
{
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter))
    {
        tokens.push_back(token);
    }
    return tokens;
}

inline std::vector<std::string> split(const std::string& s, const std::string& delimiter)
{
    std::vector<std::string> tokens;
    
    size_t last = 0; size_t next = 0;

    while ((next = s.find(delimiter)) != std::string::npos) {
        tokens.push_back(s.substr(last, next-last));
        last = next + 1;
    }
    tokens.push_back(s.substr(last));
    return tokens;
}

inline void erase(std::string& str, const std::string& from)
{
    replace(str, from, "");
}

// from https://stackoverflow.com/a/874160
inline bool ends_with(const std::string& str, const std::string& suffix)
{
    return str.size() >= suffix.size() && 0 == str.compare(str.size()-suffix.size(), suffix.size(), suffix);
}

inline bool starts_with(const std::string& str, const std::string& prefix)
{
    return str.size() >= prefix.size() && 0 == str.compare(0, prefix.size(), prefix);
}

inline std::string replace(std::string subject, const std::string& search, const std::string& replace)
{
    size_t pos = 0;
    if ((pos = subject.find(search, pos)) != std::string::npos) {
        subject.replace(pos, search.length(), replace);
    }
    return subject;
}

inline void replace_all(std::string& str, const std::string& from, const std::string& to)
{
    if(from.empty())
        return;
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}

std::string& str_toupper(std::string& s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::toupper(c); });
    return s;
}

std::string& str_tolower(std::string& s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return std::tolower(c); });
    return s;
}

std::string trim(const std::string& str)
{
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first)
    {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}


}
