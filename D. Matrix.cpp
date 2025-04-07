#include <utility>
#include <vector>
#include <iostream>
#include <array>
#include <algorithm>
#include <complex>
#include <cstring>
//#define int long long

class BigInteger;

class Rational;

BigInteger operator*(const BigInteger& a, int b);

BigInteger operator*(int a, const BigInteger& b);

BigInteger operator+(const BigInteger& a, const BigInteger& b);

BigInteger operator-(const BigInteger& a, const BigInteger& b);

class BigInteger {
    bool is_negative = false;
    bool need_to_del_zeros = true;
    friend Rational;

    friend std::istream& operator>>(std::istream&, BigInteger&);

private:

    BigInteger multi_to_one(BigInteger num, int digit) {
        long long carry = 0;
        num.is_negative = false;
        for (size_t i = 0; i < num.size() or carry > 0; ++i) {
            if (i == num.size()) {
                num.digits.push_back(0);
            }
            carry += (long long) num.digits[i] * digit;
            num.digits[i] = carry % BASE;
            carry /= BASE;
        }
        return num;
    }

    void karatsuba(long long* left, long long* right, long long* res, size_t n) {
        if (n <= 64) {
            for (size_t i = 0; i < n; i++) {
                for (size_t j = 0; j < n; j++) {
                    res[i + j] += left[i] * right[j];
                }
            }
        } else {
            size_t k = n / 2;
            long long l[k], r[k], t[n];
            for (size_t i = 0; i < k; i++) {
                l[i] = left[i] + left[k + i];
                r[i] = right[i] + right[k + i];
            }
            memset(t, 0, n * sizeof(long long));
            karatsuba(l, r, t, k);
            karatsuba(left, right, res, k);
            karatsuba(left + k, right + k, res + n, k);
            long long* t1 = t, * t2 = t + k;
            long long* s1 = res, * s2 = res + k, * s3 = res + 2 * k, * s4 = res + 3 * k;
            for (size_t i = 0; i < k; i++) {
                long long c1 = s2[i] + t1[i] - s1[i] - s3[i];
                long long c2 = s3[i] + t2[i] - s2[i] - s4[i];
                res[k + i] = c1;
                res[n + i] = c2;
            }
        }
    }

    BigInteger multiply(const BigInteger& first, const BigInteger& second) {
        if (first.size() == 0 or second.size() == 0) {
            return {0};
        }
        if (first.size() == 1) {
            return multi_to_one(second, first.digits[0]);
        }
        if (second.size() == 1) {
            return multi_to_one(first, second.digits[0]);
        }
        size_t n = 1;
        while (n < std::max(first.size(), second.size())) {
            n *= 2;
        }
        long long left[n];
        for (size_t i = 0; i < first.size(); ++i) {
            left[i] = first.digits[i];
        }
        memset(left + first.size(), 0, (n - first.size()) * sizeof(long long));
        long long right[n];
        for (size_t i = 0; i < second.size(); ++i) {
            right[i] = second.digits[i];
        }
        memset(right + second.size(), 0, (n - second.size()) * sizeof(long long));
        long long res[2 * n];
        memset(res, 0, 2 * n * sizeof(long long));
        karatsuba(left, right, res, n);
        BigInteger ans;
        for (size_t i = 0; i < 2 * n; ++i) {
            if (i + 1 < 2 * n) {
                res[i + 1] += res[i] / BASE;
                res[i] %= BASE;
            }
            ans.digits.push_back(res[i]);
        }
        while (ans.size() > 0 and ans.digits.back() == 0) {
            ans.digits.pop_back();
        }
        return ans;
    }

    std::string toString(int x, size_t it) const {
        std::string ans;
        for (int i = 0; i < logBASE; ++i) {
            if (x == 0 and it == 0) break;
            ans += x % 10 + '0';
            x /= 10;
        }
        std::reverse(ans.begin(), ans.end());
        return ans;
    }

    int get_ind(size_t ind) const {
        return digits[ind];
    }

public:
    static const int BASE = 1000000;
    static const int logBASE = 6;
    std::vector<int> digits = {};

    BigInteger() : is_negative(false) {};

    BigInteger(int x) {
        if (x < 0) {
            is_negative = true;
            x *= -1;
        }
        while (x) {
            digits.push_back(x % BASE);
            x /= BASE;
        }
    };

    BigInteger(const BigInteger&) = default;

    BigInteger& operator=(const BigInteger&) = default;


    size_t size() const {
        return digits.size();
    }

    std::weak_ordering operator<=>(const BigInteger& other) const {
        if (other.is_negative != is_negative) {
            return other.is_negative <=> is_negative;
        }
        if (!is_negative) {
            if (digits.size() != other.size()) {
                return digits.size() <=> other.size();
            }
            int sz = digits.size();
            for (size_t i = 0; i < digits.size(); ++i) {
                if (digits[sz - i - 1] != other.get_ind(sz - i - 1)) {
                    return digits[sz - i - 1] <=> other.get_ind(sz - i - 1);
                }
            }
            return 0 <=> 0;
        }
        if (digits.size() != other.size()) {
            return other.size() <=> digits.size();
        }
        int sz = digits.size();
        for (size_t i = 0; i < digits.size(); ++i) {
            if (digits[sz - i - 1] != other.get_ind(sz - i - 1)) {
                return other.get_ind(sz - i - 1) <=> digits[sz - i - 1];
            }
        }
        return 0 <=> 0;
    }

    bool operator==(const BigInteger&) const = default;

    BigInteger& operator-=(const BigInteger&);

    BigInteger& operator+=(const BigInteger& other) {
        if (!is_negative && other.is_negative) {
            BigInteger cp_oth = other;
            cp_oth.is_negative = false;
            *this -= cp_oth;
            return *this;
        }
        if (is_negative && !other.is_negative) {
            BigInteger cp_oth = other;
            BigInteger cp_this = *this;
            cp_this.is_negative = false;
            cp_oth -= cp_this;
            *this = cp_oth;
            return *this;
        }
        size_t sz = other.size();
        int carry = 0;
        for (size_t i = 0; i < sz; ++i) {
            carry += other.get_ind(i);
            if (digits.size() <= i) {
                digits.push_back(0);
            }
            digits[i] += carry;
            carry = digits[i] / BASE;
            digits[i] %= BASE;
        }
        size_t i = sz;
        while (carry > 0) {
            if (digits.size() <= i) {
                digits.push_back(0);
            }
            digits[i] += carry;
            carry = digits[i] / BASE;
            digits[i] %= BASE;
            i++;
        }
        return *this;
    }

    BigInteger& operator*=(const BigInteger& other) {
        bool fl_is_negative = is_negative ^ other.is_negative;
        *this = multiply(*this, other);
        is_negative = fl_is_negative;
        if (digits.empty()) {
            is_negative = false;
        }
        return *this;
    }

    BigInteger& operator/=(const BigInteger& other_) {
        BigInteger ans(0);
        BigInteger other = other_;
        ans.is_negative = is_negative ^ other.is_negative;
        is_negative = false;
        other.is_negative = false;
        int sz_oth = other.size();
        int sz_this = digits.size();
        for (int i = 0; i < sz_this - sz_oth + 1; ++i) {
            int l = 0, r = BASE;
            while (r - l > 1) {
                int j = (l + r) / 2;
                BigInteger here = other;
                here *= BigInteger(j);
                reverse(here.digits.begin(), here.digits.end());
                for (int k = 0; k < sz_this - sz_oth - i; ++k) {
                    here.digits.push_back(0);
                }
                reverse(here.digits.begin(), here.digits.end());
                while (!here.digits.empty() and here.digits.back() == 0) {
                    here.digits.pop_back();
                }
                if (here <= *this) {
                    l = j;
                } else {
                    r = j;
                }
            }
            BigInteger here = other;
            here *= BigInteger(l);
            reverse(here.digits.begin(), here.digits.end());
            for (int k = 0; k < sz_this - sz_oth - i; ++k) {
                here.digits.push_back(0);
            }
            reverse(here.digits.begin(), here.digits.end());
            while (!here.digits.empty() and here.digits.back() == 0) {
                here.digits.pop_back();
            }
            if (here <= *this) {
                *this -= here;
                ans.digits.push_back(l);
            }
        }
        reverse(ans.digits.begin(), ans.digits.end());
        if (need_to_del_zeros) {
            while (!ans.digits.empty() && ans.digits.back() == 0) {
                ans.digits.pop_back();
            }
        }
        if (ans.digits.empty()) {
            ans.is_negative = false;
        }
        *this = ans;
        return *this;
    }

    BigInteger& operator%=(const BigInteger& other_) {
        BigInteger cp = *this;
        cp /= other_;
        cp *= other_;
        *this -= cp;
        return *this;
    }

    BigInteger operator-() {
        if (digits.empty()) {
            BigInteger bg = *this;
            bg.is_negative = false;
            return bg;
        }
        BigInteger cp(*this);
        cp.is_negative ^= 1;
        return cp;
    }

    std::string toString() const {
        std::string ans;
        if (is_negative) {
            ans += '-';
        }
        if (digits.empty()) {
            ans += '0';
        }
        for (size_t i = 0; i < digits.size(); ++i) {
            ans += toString(digits[digits.size() - 1 - i], i);
        }
        return ans;
    }

    BigInteger& operator++() {
        *this += 1;
        return *this;
    }

    BigInteger operator++(signed) {
        const BigInteger temp = *this;
        *this += 1;
        return temp;
    }

    BigInteger& operator--() {
        *this -= 1;
        return *this;
    }

    const BigInteger operator--(signed) {
        const BigInteger temp = *this;
        *this -= 1;
        return temp;
    }


    explicit operator bool() const {
        return !digits.empty();
    }
};

BigInteger operator "" _bi(unsigned long long x) {
    return BigInteger(x);
}

BigInteger& BigInteger::operator-=(const BigInteger& other) {
    if (!is_negative && !other.is_negative) {
        if (*this < other) {
            BigInteger cp_oth = other;
            cp_oth -= *this;
            *this = cp_oth;
            is_negative = true;
            return *this;
        }
        for (size_t i = 0; i < other.size(); ++i) {
            if (digits[i] < other.get_ind(i)) {
                digits[i + 1] -= 1;
                digits[i] += BASE;
            }
            digits[i] -= other.get_ind(i);
        }
        size_t i = other.size();
        while (i < digits.size() and digits[i] < 0) {
            digits[i + 1] -= 1;
            digits[i] += BASE;
            ++i;
        }
        while (!digits.empty() and digits.back() == 0) {
            digits.pop_back();
        }
        return *this;
    } else if (!is_negative && other.is_negative) {
        BigInteger cp_oth = other;
        cp_oth.is_negative = false;
        *this += cp_oth;
        return *this;
    } else if (is_negative && !other.is_negative) {
        BigInteger cp_this = *this;
        cp_this.is_negative = false;
        cp_this += other;
        cp_this.is_negative = true;
        *this = cp_this;
        return *this;
    } else {
        BigInteger cp_oth = other;
        BigInteger cp_this = *this;
        cp_this.is_negative = false;
        cp_oth.is_negative = false;
        cp_oth -= cp_this;
        *this = cp_oth;
        return *this;
    }
}

BigInteger operator+(const BigInteger& a, const BigInteger& b) {
    BigInteger cp = a;
    cp += b;
    return cp;
}


BigInteger operator-(const BigInteger& a, const BigInteger& b) {
    BigInteger cp(a);
    cp -= b;
    return cp;
}

BigInteger operator*(const BigInteger& a, const BigInteger& b) {
    BigInteger cp(a);
    cp *= b;
    return cp;
}

BigInteger operator*(const BigInteger& a, int b) {
    BigInteger cp(a);
    cp *= b;
    return cp;
}

BigInteger operator*(int a, const BigInteger& b) {
    BigInteger cp(a);
    cp *= b;
    return cp;
}

BigInteger operator/(const BigInteger& a, const BigInteger& b) {
    BigInteger cp(a);
    cp /= b;
    return cp;
}

BigInteger operator%(const BigInteger& a, const BigInteger& b) {
    BigInteger cp(a);
    cp %= b;
    return cp;
}


std::istream& operator>>(std::istream& in, BigInteger& bg) {
    bg.digits.clear();
    bg.is_negative = false;
    char c;
    std::vector<int> digits;
    in.get(c);
    while (isspace(c)) {
        in.get(c);
    }
    while (!isspace(c) && !in.eof()) {
        if (c == '-') {
            bg.is_negative ^= 1;
        } else if ('0' <= c and c <= '9') {
            digits.push_back(c - '0');
        }
        in.get(c);
    }
    std::reverse(digits.begin(), digits.end());
    for (size_t i = 0; i < digits.size(); i += bg.logBASE) {
        int num = 0;
        for (int j = bg.logBASE - 1; j >= 0; --j) {
            if (i + j >= (size_t) digits.size()) continue;
            num *= 10;
            num += digits[i + j];
        }
        bg.digits.push_back(num);
    }
    while (bg.size() > 0 and bg.digits.back() == 0) {
        bg.digits.pop_back();
    }
    if (bg.digits.empty()) {
        bg.is_negative = false;
    }
    return in;
}

std::ostream& operator<<(std::ostream& out, const BigInteger& bg) {
    out << bg.toString();
    return out;
}

class Rational {
private:

    BigInteger gcd(BigInteger a, BigInteger b) {
        a.is_negative = false;
        while (b) {
            a %= b;
            std::swap(a, b);
        }
        return a;
    }

    void relax() {
        BigInteger num2 = gcd(num, den);
        num /= num2;
        den /= num2;
    }

public:
    BigInteger num = 0, den = 1;

    Rational() {};

    Rational(int x) : num(x) {};

    Rational(const BigInteger& x) : num(x) {}

    Rational& operator+=(const Rational& other) {
        num = num * other.den + other.num * den;
        den *= other.den;
        relax();
        return *this;
    }

    Rational& operator-=(const Rational& other) {
        num = num * other.den - other.num * den;
        den *= other.den;
        relax();
        return *this;
    }

    Rational& operator*=(const Rational& other) {
        num *= other.num;
        den *= other.den;
        relax();
        return *this;
    }

    Rational& operator/=(const Rational& other) {
        num *= other.den;
        den *= other.num;
        if (den.is_negative) {
            num.is_negative ^= 1;
            den.is_negative = false;
        }
        relax();
        return *this;
    }

    Rational operator-() {
        Rational cp(*this);
        cp.num.is_negative ^= 1;
        return cp;
    }

    bool operator==(const Rational& other) const {
        return num * other.den == den * other.num;
    }

    auto operator<=>(const Rational& other) const {
        return num * other.den - den * other.num <=> 0;
    }

    bool operator==(const int& other) const {
        return num == den * other;
    }

    auto operator<=>(const int other) const {
        return num - den * other <=> 0;
    }

    std::string toString() {
        std::string ans;
        ans += num.toString();
        if (den != 1) {
            ans += '/';
            ans += den.toString();
        }
        return ans;
    }


    std::string asDecimal(size_t precision = 0) const {
        if (num == 0) {
            std::string ans = "0";
            if (precision > 0) ans += '.';
            for (size_t i = 0; i < precision; ++i) {
                ans += '0';
            }
            return ans;
        }
        Rational cp = *this;
        reverse(cp.num.digits.begin(), cp.num.digits.end());
        for (size_t i = 0; i < precision; ++i) {
            cp.num.digits.push_back(0);
        }
        reverse(cp.num.digits.begin(), cp.num.digits.end());
        BigInteger cp2 = cp.num / den;
        std::string ans = cp2.toString();

        for (size_t i = 0; i < precision * (num.logBASE - 1); ++i) {
            ans.pop_back();
        }
        bool have_is_negative = false;
        if (ans[0] == '-') {
            have_is_negative = true;
            ans.erase(ans.begin());
        }
        if (num < den) {
            while (ans.size() < precision + 1) {
                ans.insert(ans.begin(), '0');
            }
        }
        if (have_is_negative) {
            ans.insert(ans.begin(), '-');
        }
        if (precision > 0) {
            ans.insert(ans.end() - precision, '.');
        }
        return ans;
    }

    explicit operator double() const {
        std::string here = asDecimal(20);
        bool is_negative = false;
        if (here[0] == '-') {
            is_negative = true;
            here.erase(here.begin());
        }
        double res = 0;
        bool fl = true;
        double flt = 0.1;
        for (char i : here) {
            if (i == '.') {
                fl = false;
                continue;
            }
            if (fl) {
                res *= 10;
                res += i - '0';
            } else {
                res += (i - '0') * flt;
                flt /= 10;
            }
        }
        if (is_negative) {
            res *= -1;
        }
        return res;
    }
};

Rational operator+(const Rational& a, const Rational& b) {
    Rational cp = a;
    cp += b;
    return cp;
}

Rational operator-(const Rational& a, const Rational& b) {
    Rational cp = a;
    cp -= b;
    return cp;
}

Rational operator*(const Rational& a, const Rational& b) {
    Rational cp = a;
    cp *= b;
    return cp;
}

Rational operator/(const Rational& a, const Rational& b) {
    Rational cp = a;
    cp /= b;
    return cp;
}

std::istream& operator>>(std::istream& in, Rational& rt) {
    BigInteger bg;
    in >> bg;
    rt = bg;
    return in;
}

std::ostream& operator<<(std::ostream& in, Rational& rt) {
    in << rt.toString();
    return in;
}

template<int N, int D>
struct isPrimeHelper {
    static constexpr bool value = N % D == 0 ? false : isPrimeHelper<N, D - 1>::value;
};

template<int N>
struct isPrimeHelper<N, 1> {
    static constexpr bool value = true;
};

template<int N, int L, int R>
struct Root {
    static const int mid = (L + R) / 2;
    static const bool down = ((static_cast<long long>(mid) * mid) >= N);
    static const int value =
            Root<N, (down ? L : mid + 1), (down ? mid : R)>::value;
};

template<int N, int Mid>
struct Root<N, Mid, Mid> {
    static const int value = Mid;
};

template<int N>
struct isPrime {
    static constexpr bool value = isPrimeHelper<N, Root<N, 1, N>::value>::value;
};

template<size_t M>
class Residue {
    int value = 0;

public:
    Residue(int n) {
        value = n % static_cast<int>(M);
        value += M;
        value %= M;
    }

    Residue() {}

    explicit operator int() const {
        return value;
    }

    explicit operator double() const {
        return value;
    }

    void operator+=(Residue<M> oth) {
        value += oth.value;
        value %= M;
    }

    void operator-=(Residue<M> oth) {
        value -= oth.value;
        value += M;
        value %= M;
    }

    void operator*=(Residue<M> oth) {
        value = static_cast<long long>(value) * oth.value % M;
    }

    // (a ** p) % M
    int binPow(int a, int p) {
        if (p == 0) return 1;
        if (p % 2) {
            return static_cast<long long>(binPow(a, p - 1)) * a % M;
        }
        long long c = binPow(a, p / 2);
        return c * c % M;
    }

    void operator/=(Residue<M> oth) {
        static_assert(isPrime<M>::value);
        int obr = binPow(oth.value, M - 2);
        value = static_cast<long long>(value) * obr % M;
    }

    bool operator==(Residue<M> oth) const {
        return value == oth.value;
    }

    bool operator!=(Residue<M> oth) const {
        return value != oth.value;
    }
};

template<size_t N>
Residue<N> operator+(Residue<N> a, Residue<N> b) {
    a += b;
    return a;
}

template<size_t N>
Residue<N> operator-(Residue<N> a, Residue<N> b) {
    a -= b;
    return a;
}

template<size_t N>
Residue<N> operator*(Residue<N> a, Residue<N> b) {
    a *= b;
    return a;
}

template<size_t N>
Residue<N> operator/(Residue<N> a, Residue<N> b) {
    a /= b;
    return a;
}

template<size_t N, size_t M, typename Field>
class Matrix;

template<size_t N, size_t M, typename Field=Rational>
class MatrixHelper {
    Field matrix[N][M];
    int rank = 0;
    int cnt_swaps = 0;
public:
    void doGauss(bool need_to_normalize = false) {
        std::vector<std::pair<size_t, size_t>> swaps;
        for (size_t col = 0, row = 0; col < M && row < N; ++col) {
            size_t sel = row;
            for (size_t i = row; i < N; ++i) {
                if (matrix[i][col] != 0) {
                    sel = i;
                    break;
                }
            }
            if (matrix[sel][col] == 0)
                continue;
            for (size_t i = col; i < M; ++i) {
                std::swap(matrix[sel][i], matrix[row][i]);
            }
            if (sel != row) cnt_swaps++;

            for (size_t i = 0; i < N; ++i) {
                if (i != row) {
                    Field c = matrix[i][col] / matrix[row][col];
                    for (size_t j = col; j < M; ++j) {
                        matrix[i][j] -= matrix[row][j] * c;
                    }
                } else if (need_to_normalize) {
                    Field c = matrix[row][col];
                    for (size_t j = col; j < M; ++j) {
                        matrix[i][j] /= c;
                    }
                }
            }
            ++row;
            rank = row;
        }
    }

    MatrixHelper() {}

    MatrixHelper(const Matrix<N, M, Field>& oth) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] = oth[i][j];
            }
        }
    }

    Field* operator[](int i) {
        return matrix[i];
    }

    const Field* operator[](int i) const {
        return matrix[i];
    }

    int get_rank() {
        return rank;
    }

    Field get_det() {
        Field ans = 1;
        for (size_t i = 0; i < N; ++i) {
            ans *= matrix[i][i];
        }
        if (cnt_swaps % 2) {
            ans *= -1;
        }
        return ans;
    }
};

template<size_t N, size_t M, typename Field=Rational>
class Matrix {
private:
    Field matrix[N][M];
public:
    Matrix() {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] = 0;
            }
        }
    }

    Matrix(std::initializer_list<std::vector<Field>> oth) {
        int i = 0;
        for (auto& c : oth) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] = c[j];
            }
            ++i;
        }
    }

    Field* operator[](int i) {
        return matrix[i];
    }

    const Field* operator[](int i) const {
        return matrix[i];
    }

    bool operator==(const Matrix<N, M, Field>& oth) const {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                if (oth[i][j] != matrix[i][j]) return false;
            }
        }
        return true;
    }

    void operator+=(const Matrix<N, M, Field>& oth) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] += oth.matrix[i][j];
            }
        }
    }

    void operator-=(const Matrix<N, M, Field>& oth) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] -= oth.matrix[i][j];
            }
        }
    }

    void operator*=(const Field& oth) {
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] *= oth;
            }
        }
    }

    template<size_t K>
    Matrix<N, K, Field> operator*(const Matrix<M, K, Field>& b) const {
        Matrix<N, K, Field> ans;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < K; ++j) {
                for (size_t k = 0; k < M; ++k) {
                    ans[i][j] += matrix[i][k] * b[k][j];
                }
            }
        }
        return ans;
    }

    int rank() const {
        MatrixHelper<N, M, Field> h = *this;
        h.doGauss();
        return h.get_rank();
    }

    Field det() const {
        static_assert(N == M);
        MatrixHelper<N, M, Field> h = *this;
        h.doGauss();
        return h.get_det();
    }

    Matrix<M, N, Field> transposed() const {
        Matrix<M, N, Field> ans;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                ans[j][i] = matrix[i][j];
            }
        }
        return ans;
    }

    Matrix<N, M, Field> inverted() const {
        static_assert(N == M);
        constexpr size_t M2 = M * 2;
        MatrixHelper<N, M2, Field> h;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                h[i][j] = matrix[i][j];
            }
        }
        for (size_t i = 0; i < N; ++i) {
            h[i][i + N] = 1;
        }
        h.doGauss(true);
        Matrix<N, M, Field> ans;
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < N; ++j) {
                ans[i][j] = h[i][j + N];
            }
        }
        return ans;
    }

    void invert() {
        Matrix<N, M, Field> mt = inverted();
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                matrix[i][j] = mt[i][j];
            }
        }
    }

    Field trace() const {
        static_assert(N == M);
        Field ans = 0;
        for (size_t i = 0; i < N; ++i) {
            ans += matrix[i][i];
        }
        return ans;
    }

    Matrix<N, M, Field>& operator*=(Matrix<N, M, Field> oth) {
        Matrix<N, M, Field> new_m = *this * oth;
        *this = new_m;
        return *this;
    }

    std::array<Field, M> getRow(unsigned ind) const {
        std::array<Field, M> ans;
        for (size_t i = 0; i < M; ++i) {
            ans[i] = matrix[ind][i];
        }
        return ans;
    }

    std::array<Field, N> getColumn(unsigned ind) const {
        std::array<Field, N> ans;
        for (size_t i = 0; i < N; ++i) {
            ans[i] = matrix[i][ind];
        }
        return ans;
    }
};


template<size_t N, typename Field=Rational>
using SquareMatrix = Matrix<N, N, Field>;

template<size_t N, size_t M, typename Field=Rational>
Matrix<N, M, Field> operator+(const Matrix<N, M, Field>& a, const Matrix<N, M, Field>& b) {
    Matrix<N, M, Field> c = a;
    c += b;
    return c;
}

template<size_t N, size_t M, typename Field=Rational>
Matrix<N, M, Field> operator-(const Matrix<N, M, Field>& a, const Matrix<N, M, Field>& b) {
    Matrix<N, M, Field> c = a;
    c -= b;
    return c;
}

template<size_t N, size_t M, typename Field=Rational>
Matrix<N, M, Field> operator*(const Matrix<N, M, Field>& a, const Field& b) {
    Matrix<N, M, Field> c = a;
    c *= b;
    return c;
}

template<size_t N, size_t M, typename Field=Rational>
Matrix<N, M, Field> operator*(const Field& b, const Matrix<N, M, Field>& a) {
    Matrix<N, M, Field> c = a;
    c *= b;
    return c;
}
