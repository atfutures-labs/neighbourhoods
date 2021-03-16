#include "preprocess.h"
#include <unordered_set>

using namespace cpp11;
namespace writable = cpp11::writable;

const nodeset_t preprocess::get_terminal_nodes (
        const std::vector <std::string> &n1,
        const std::vector <std::string> &n2)
{

    nodeset_t terminal, not_terminal;

    // loop over both vectors separately to avoid copying
    preprocess::get_one_terminal_nodes (
            n1, terminal, not_terminal);
    preprocess::get_one_terminal_nodes (
            n2, terminal, not_terminal);

    return terminal;
}

void preprocess::get_one_terminal_nodes (
        const std::vector <std::string> &nodes,
        nodeset_t &terminal,
        nodeset_t &not_terminal)
{

    for (auto n: nodes)
    {
        if (not_terminal.find (n) == not_terminal.end ())
        {
            if (terminal.find (n) == terminal.end ())
            {
                terminal.emplace (n);
            } else
            {
                terminal.erase (n);
                not_terminal.emplace (n);
            }
        }
    }
}

template <typename T1, typename T2>
void preprocess::copy_column (
        const list &df,
        const std::string &col,
        std::vector <T2> &result)
{
    T1 s = df [col];
    result.resize (s.size ());
    std::copy (s.begin (), s.end (), result.begin ());
}

template <typename T>
void preprocess::copy_vec (
        const std::vector <T> &vfrom,
        std::vector <T> &vto)
{
    vto.resize (vfrom.size ());
    std::copy (vfrom.begin (), vfrom.end (), vto.begin ());
}


[[cpp11::register]]
writable::integers cpp_preprocess(list df)
{
    std::vector <std::string> n1, n2;
    preprocess::copy_column <strings, std::string> (df, ".vx0", n1);
    preprocess::copy_column <strings, std::string> (df, ".vx1", n2);

    std::vector <int> index (n1.size ());
    std::iota (std::begin (index), std::end (index), 0);

    strings e = df ["edge_"];
    std::string e0 = e [0];

    nodeset_t terminal = preprocess::get_terminal_nodes (n1, n2);

    nodeset_t terminal_all;
    for (auto t: terminal)
        terminal_all.emplace (t);

    while (terminal.size () > 0)
    {
        const int newsize = index.size () - terminal.size ();
        std::vector <int> index2 (newsize);
        std::vector <std::string> n1temp (newsize), n2temp (newsize);

        int count = 0;
        for (auto i = 0; i < index.size (); i++)
        {
            // https://github.com/r-lib/cpp11/issues/129#issuecomment-730415040
            std::string n1_i = r_string (n1 [i]);
            std::string n2_i = r_string (n2 [i]);

            if (terminal.find (n1_i) == terminal.end () &&
                    terminal.find (n2_i) == terminal.end ())
            {
                index2 [count] = index [i];
                n1temp [count] = n1 [i];
                n2temp [count] = n2 [i];
                count++;
            }
        }
        
        terminal.clear ();
        terminal = preprocess::get_terminal_nodes (n1temp, n2temp);
        for (auto t: terminal)
            terminal_all.emplace (t);

        preprocess::copy_vec <int> (index2, index);
        preprocess::copy_vec <std::string> (n1temp, n1);
        preprocess::copy_vec <std::string> (n2temp, n2);
    }

    strings v0 = df [".vx0"];
    strings v1 = df [".vx1"];

    writable::integers out (v0.size () - terminal_all.size ());

    int count = 0;
    for (int i = 0; i < v0.size (); i++)
    {
        if (terminal_all.find (v0 [i]) == terminal_all.end () &&
                terminal_all.find (v1 [i]) == terminal_all.end ())
        {
            out [count++] = i;
        }
    }

    return out;
}
