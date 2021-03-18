#include "preprocess.h"
#include <ctime>
#include <stdexcept>
#include <unordered_set>

using namespace cpp11;
namespace writable = cpp11::writable;

const nodeset_t preprocess::get_terminal_nodes (
        const std::vector <node_t> &n1,
        const std::vector <node_t> &n2)
{

    nodeset_t terminal;
    nodeset_t not_terminal;
    terminal.reserve (n1.size ());
    not_terminal.reserve (n1.size ());

    // loop over both vectors separately to avoid copying
    preprocess::get_one_terminal_nodes (
            n1, terminal, not_terminal);
    preprocess::get_one_terminal_nodes (
            n2, terminal, not_terminal);

    return terminal;
}

void preprocess::get_one_terminal_nodes (
        const std::vector <node_t> &nodes,
        nodeset_t &terminal,
        nodeset_t &not_terminal)
{

    for (node_t n: nodes)
    {
        if (n.length () < 1)
            continue;
        if (not_terminal.count (n) == 0)
        {
            if (terminal.count (n) == 0)
            {
                terminal.insert (n);
            } else
            {
                terminal.erase (n);
                not_terminal.insert (n);
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
    result.resize (static_cast <size_t> (s.size ()));
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

    std::vector <node_t> n1, n2;
    preprocess::copy_column <strings, node_t> (df, ".vx0", n1);
    preprocess::copy_column <strings, node_t> (df, ".vx1", n2);

    std::vector <int> index (n1.size ());
    std::iota (std::begin (index), std::end (index), 0);

    nodeset_t terminal = preprocess::get_terminal_nodes (n1, n2);

    nodeset_t terminal_all;
    terminal_all.reserve (n1.size ());
    for (auto t: terminal)
        terminal_all.emplace (t);

    int nloops = 0;
    while (terminal.size () > 0)
    {
        std::vector <int> index2;
        std::vector <node_t> n1temp, n2temp;
        index2.reserve (index.size ());
        n1temp.reserve (index.size ());
        n2temp.reserve (index.size ());

        size_t count = 0;
        for (size_t j = 0; j < index.size (); j++)
        {
            // https://github.com/r-lib/cpp11/issues/129#issuecomment-730415040
            node_t n1_i = static_cast <node_t> (r_string (n1 [j]));
            node_t n2_i = static_cast <node_t> (r_string (n2 [j]));

            if (terminal.count (n1_i) == 0 &&
                    terminal.count (n2_i) == 0)
            {
                index2.push_back (index [j]);
                n1temp.push_back (n1_i);
                n2temp.push_back (n2_i);
                count++;
            }
        }
        index2.resize (count);
        n1temp.resize (count);
        n2temp.resize (count);

        terminal = preprocess::get_terminal_nodes (n1temp, n2temp);
        for (auto t: terminal)
            terminal_all.insert (t);

        preprocess::copy_vec <int> (index2, index);
        preprocess::copy_vec <node_t> (n1temp, n1);
        preprocess::copy_vec <node_t> (n2temp, n2);

        nloops++;
    }

    strings v0 = df [".vx0"];
    strings v1 = df [".vx1"];

    writable::integers out (v0.size () - static_cast <int> (terminal_all.size ()));

    int count = 0;
    for (int i = 0; i < v0.size (); i++)
    {
        if (terminal_all.find (v0 [i]) == terminal_all.end () &&
                terminal_all.find (v1 [i]) == terminal_all.end ())
        {
            out [count++] = i + 1; // return 1-based R indexing
        }
    }

    return out;
}
