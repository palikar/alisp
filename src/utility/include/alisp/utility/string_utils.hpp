
#include <vector>
#include <string>
#include <sstream>



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

inline void replace(std::string& str, const std::string& from, const std::string& to)
{
    if(from.empty())
        return;
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos)
    {
		str.replace(start_pos, from.length(), to);
		start_pos += to.length();
    }
}


inline void erase(std::string& str, const std::string& from)
{
    replace(str, from, "");
}


}
