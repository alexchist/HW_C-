#include <cstring>
#include <iostream>
#include <algorithm>

class String {
private:
    size_t cap;
    size_t sz;
    char* str;

    void resize_buffer(size_t newCap) {
        cap = newCap;
        char* buf = new char[cap + 1];
        if (sz > 0) {
            std::copy(str, str + sz + 1, buf);
        }
        delete[] str;
        str = buf;
    }

public:
    String(size_t n) : cap(n), sz(n), str(new char[n + 1]) {
        str[n] = 0;
    }

    String(const char* other) : String(strlen(other)) {
        std::copy(other, other + sz, str);
    }

    String(size_t n, char c) : String(n) {
        std::fill(str, str + n, c);
    }

    String() : String((size_t) 0) {};

    String(const String& other) : String(other.sz) {
        std::copy(other.str, other.str + sz, str);
    }

    ~String() {
        delete[] str;
    }

    String& operator=(const String& other) {
        if (other.sz > cap) {
            resize_buffer(other.sz);
        }
        std::copy(other.str, other.str + other.sz, str);
        sz = other.sz;
        str[sz] = 0;
        return *this;
    }

    char& operator[](size_t ind) {
        return str[ind];
    }

    const char& operator[](size_t ind) const {
        return str[ind];
    }

    size_t length() const {
        return sz;
    }

    size_t size() const {
        return sz;
    }

    size_t capacity() const {
        return cap;
    }

    void push_back(const char c) {
        if (sz == cap) {
            resize_buffer(cap * 2 + 1);
        }
        str[sz] = c;
        sz++;
        str[sz] = 0;
    }

    void pop_back() {
        sz--;
        str[sz] = 0;
    }

    char& front() {
        return str[0];
    }

    const char& front() const {
        return str[0];
    }

    char& back() {
        return str[sz - 1];
    }

    const char& back() const {
        return str[sz - 1];
    }

    String& operator+=(const String& other) {
        if (sz + other.sz > cap) {
            resize_buffer((sz + other.sz) * 2);
        }
        std::copy(other.str, other.str + other.sz, str + sz);
        sz += other.sz;
        str[sz] = 0;
        return *this;
    }

    String& operator+=(const char& other) {
        push_back(other);
        return *this;
    }

    size_t find(const String& substr) const {
        for (size_t start = 0; start + substr.sz <= sz; ++start) {
            if (strncmp(substr.data(), str + start, substr.sz) == 0) {
                return start;
            }
        }
        return sz;
    }

    size_t rfind(const String& substr) const {
        if (substr.sz > sz) return sz;
        for (int start = sz - substr.sz; start >= 0; --start) {
            if (strncmp(substr.data(), str + start, substr.sz) == 0) {
                return start;
            }
        }
        return sz;
    }

    String substr(int start, int count) const {
        String res(count, 'a');
        std::copy(str + start, str + start + count, res.str);
        return res;
    }

    bool empty() const {
        return sz == 0;
    }

    void clear() {
        sz = 0;
        str[0] = 0;
    }

    void shrink_to_fit() {
        resize_buffer(sz);
    }

    char* data() {
        return str;
    }

    const char* data() const {
        return str;
    }
};

bool operator==(const String& first, const String& second) {
    if (first.size() != second.size()) {
        return false;
    }
    return strcmp(first.data(), second.data()) == 0;
}

bool operator<(const String& first, const String& second) {
    return strcmp(first.data(), second.data()) < 0;
}

bool operator!=(const String& first, const String& second) {
    return !(first == second);
}

bool operator>(const String& first, const String& second) {
    return second < first;
}

bool operator<=(const String& first, const String& second) {
    return !(first > second);
}

bool operator>=(const String& first, const String& second) {
    return !(first < second);
}

String operator+(const String& first, const String& second) {
    String res(first);
    res += second;
    return res;
}

String operator+(const String& first, const char& second) {
    String res(first);
    res += second;
    return res;
}

String operator+(const char& first, const String& second) {
    String res(1, first);
    res += second;
    return res;
}

std::ostream& operator<<(std::ostream& out, const String& s) {
    for (size_t i = 0; i < s.size(); ++i) {
        out << s[i];
    }
    return out;
}

std::istream& operator>>(std::istream& in, String& s) {
    s.clear();
    char c;
    in.get(c);
    while (isspace(c)) {
        in.get(c);
    }
    while (!isspace(c) && !in.eof()) {
        s.push_back(c);
        in.get(c);
    }
    return in;
}
