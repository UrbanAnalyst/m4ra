#include "utils.h"

std::string cut_string_end (const std::string s, const std::string the_end)
{
    const size_t pos = the_end.size ();

    std::string s_end = s.substr (s.size () - pos, s.size () - 1);
    std::string s_short;

    if (std::strcmp (s_end.c_str (), the_end.c_str ()) == 0L) {
        s_short = s.substr (0, s.size () - pos);
    } else
    {
        s_short = s;
    }

    return (s_short);
}
