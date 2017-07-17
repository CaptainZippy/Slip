#include "Pch.h"

std::string string_format(const char* fmt, ...) {
	std::string str;
	if (str.capacity() < 15)
	{
		str.reserve(15);
	}
	str.resize(str.capacity());
	va_list ap;

	while (1) {
		va_start(ap, fmt);
		int n = vsnprintf(&str[0], str.size(), fmt, ap) + 1;
		va_end(ap);

		if (n > str.capacity()) {
			str.resize(n);
		}
		else if (n > 0) {
			str.resize(n);
			return str;
		}
		else {
			str.resize(str.size() * 2);
		}
	}
}
