#include "utils.h"

void utils::cut_terminal_rev (std::string &s)
{
    const std::string revend = "_rev";

    const size_t n = s.length ();
    const std::string send = s.substr (n - 4, n - 1);
    if (std::strcmp (send.c_str (), revend.c_str ()) == 0)
        s = s.substr (0, n - 4);
}
