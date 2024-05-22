#ifndef BEARMONADTERMINAL_H
#define BEARMONADTERMINAL_H

#include "BearLibTerminal.h"
#include <string.h>

void terminal_color_from_name(const char* name)
{
	terminal_color(color_from_name(name));
}

void terminal_bkcolor_from_name(const char* name)
{
	terminal_bkcolor(color_from_name(name));
}

void terminal_print_ptr(int x, int y, const char* s, dimensions_t* dim)
{
  dimensions_t d = terminal_print(x, y, s);
	memcpy(dim, &d, sizeof(*dim));
}

void terminal_print_ext_ptr(int x, int y, int w, int h, int align, const char* s, dimensions_t* dim)
{
	dimensions_t d = terminal_print_ext(x, y, w, h, align, s);
	memcpy(dim, &d, sizeof(*dim));
}

void terminal_measure_ptr(const char* s, dimensions_t* dim)
{
	dimensions_t d = terminal_measure(s);
	memcpy(dim, &d, sizeof(*dim));
}

void terminal_measure_ext_ptr(int w, int h, const char* s, dimensions_t* dim)
{
	dimensions_t d = terminal_measure_ext(w, h, s);
	memcpy(dim, &d, sizeof(*dim));
}

#endif