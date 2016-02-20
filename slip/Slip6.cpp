
#include "Pch.h"

void error(const char* msg) {
	__debugbreak();
}
template<typename T>
void assert2(T t, const char* msg) {
	if(!t) error(msg);
}
#define assert(A) assert2(A, #A)
#define cast(T,a) dynamic_cast<T*>(a)

template<typename T>
struct array_view {
	array_view(const T* s, const T* e) : m_begin(s), m_end(e) {}
	array_view(const std::vector<T>& a) : m_begin(a.data()), m_end(m_begin+a.size()) {}
	size_t size() const { return m_end - m_begin; }
	const T& operator[](int i) const { assert(i >= 0); assert(i < size()); return m_begin[i]; }
	const T* begin() const { return m_begin; }
	const T* end() const { return m_end; }
	array_view<T> ltrim(int n) const { assert(n <= size()); return array_view<T>(m_begin + n, m_end); }
	const T* m_begin;
	const T* m_end;
};

struct Env;
struct List;
struct Type;


struct Atom {
	virtual ~Atom() {}
	virtual Atom* eval(Env* env) = 0;
	virtual Atom* normalize() = 0;
	virtual void print() const = 0;
	Atom() : m_type(nullptr) {}
	Atom* m_type;
};

struct Value : Atom {
	Atom* eval(Env* env) override {
		return this;
	}
	Atom* normalize() override {
		return this;
	}
};

struct Type : public Value {
	void print() const override {
		printf("type");
	}
	static Type s_bool;
	static Type s_string;
	static Type s_num;
	static Type s_list;
};

Type Type::s_num;
Type Type::s_string;

struct Bool : Value {
	Bool(bool v) : m_val(v) {}
	void print() const override {
		printf(m_val ? "true" : "false");
	}
	bool m_val;
};

struct Num : Value {
	Num(int n) : m_num(n) { m_type = &Type::s_num; }
	void print() const override {
		printf("%i", m_num);
	}
    int m_num;
};

struct String : Value {
	String(const char* s = nullptr) : m_str(s) { m_type = &Type::s_string; }
	void print() const override {
		printf("%s", m_str);
	}
	const char* m_str;
};

struct Env : Value {
	Env(Env *p) : m_parent(p) { }
	Atom* get(const char* sym) {
		for(Env* e = this; e; e = e->m_parent) {
			auto it = e->m_tab.find(sym);
			if(it != e->m_tab.end()) {
				return it->second;
			}
		}
		error("not found");
		return nullptr;
	}
	void print() const override {
		printf("<env>");
	}
	void put(const char* sym, Atom* val) {
		m_tab.emplace(sym, val);
	}
	Env* m_parent;
	typedef std::unordered_map<std::string, Atom*> Table;
	Table m_tab;
};

struct Symbol : Atom {
	Symbol(const char* s) : m_sym(s) {}
	virtual Atom* eval(Env* env) override {
		return env->get(m_sym);
	}
	void print() const override {
		printf("'%s", m_sym);
	}
	Atom* normalize() {
		return this;
	}
	const char* m_sym;
};

struct Callable : Value {
	typedef array_view<Atom*> ArgList;
	virtual Atom* call(Env* env, ArgList args) = 0;
	void print() const override {
		printf("<callable>");
	}
};

struct List : Atom {
	typedef std::vector<Atom*> vector;
	Atom* eval(Env* env) override {
		assert(lst.size());
		Atom* atom = at(0)->eval(env);
		if(auto call = cast(Callable, atom)) {
			return call->call(env, Callable::ArgList(lst).ltrim(1));
		}
		error("not callable");
		return nullptr;
	}
	Atom* normalize() override {
		std::vector<List*> lets;
		List* fin = new List();
		for(auto arg : lst) {
			auto a = normalize2(arg, lets);
			fin->append(a);
		}
		List* ret = new List();
		ret->append(new Symbol("let"));
		List* loc = new List();
		for(auto l : lets) {
			loc->append(l);
		}
		ret->append(loc);
		ret->append(fin);
		return ret;
	}
	static Atom* normalize2(Atom* arg, std::vector<List*>& lets) {
		if(auto l = cast(List, arg)) {
			List* simp = new List();
			for(auto a : l->lst)
			{
				Atom* b = normalize2(a, lets);
				simp->append(b);
			}
			Symbol* sym;
			{
				char buf[256];
				sprintf(buf, "t%i", lets.size());
				sym = new Symbol(strdup(buf));
			}
			List* let = new List();
			let->append(sym);
			let->append(simp);
			lets.push_back(let);
			return sym;
		}
		else {
			return arg->normalize();
		}
	}

	vector::const_iterator begin() const {
		return lst.begin();
	}
	vector::const_iterator end() const {
		return lst.end();
	}
	size_t size() const {
		return lst.size();
	}
	void append(Atom* a) {
		lst.push_back(a);
	}
	Atom* at(int i) const {
		return lst[i];
	}
	void print() const override {
		printf("(");
		const char* sep = "";
		for(auto a : lst) {
			printf(sep);
			a->print();
			sep = " ";
		}
		printf(")");
	}
protected:
	std::vector<Atom*> lst;
};

struct Lambda : Callable {
	Lambda(Env* lex_env, List* arg_names, Atom* body)
		: m_lex_env(lex_env), m_arg_names(new List()), m_body(body) {
		for(auto a : *arg_names) {
			if(auto bs = cast(Symbol, a)) {
				m_arg_names->append(bs);
			}
			else {
				auto ts = a->eval(lex_env);
				m_arg_names->append(ts);
			}
		}
	}
	Atom* call(Env* env, ArgList arg_vals) override {
		Env* e = new Env(m_lex_env);
		assert(m_arg_names->size() == arg_vals.size());
		for(int i = 0; i < m_arg_names->size(); ++i) {
			Atom* a = arg_vals[i]->eval(env);
			auto s = cast(Symbol, m_arg_names->at(i));
			assert( s->m_type == nullptr || a->m_type->eval(env) == s->m_type->eval(env));
			e->put( s->m_sym, a);
		}
		return m_body->eval(e);
	}
	Atom* normalize() override {
		Atom* body = m_body->normalize();
		return new Lambda(m_lex_env, m_arg_names, body);
	}
	void print() const override {
		printf("(lambda (");
		for(auto a : *m_arg_names) {
			a->print();
			printf(" ");
		}
		printf(") ");
		m_body->print();

	}
	Env* m_lex_env;
	List* m_arg_names;
	Atom* m_body;
};

struct Vau : Callable {
	Env* m_lex_env;
	List* m_arg_names;
	Atom* m_env_sym;
	Atom* m_body;
	Vau(Env* lex_env, List* arg_names, Atom* symbol, Atom* body)
	: m_lex_env(lex_env), m_arg_names(new List()), m_env_sym(symbol), m_body(body) { 
		for(auto a : *arg_names) {
			if(auto bs = cast(Symbol, a)) {
				m_arg_names->append(bs);
			}
			else {
				auto ts = a->eval(lex_env);
				m_arg_names->append(ts);
			}
		}
	}

	Atom* call(Env* env, ArgList args) override {
		assert(args.size() == 2);
		Env* e = new Env(env);
		List* arg_vals = cast(List, args[0]);
		assert(arg_vals->size() == m_arg_names->size());
		for(int i = 0; i < arg_vals->size(); ++i) {
			e->put(cast(Symbol, m_arg_names->at(i))->m_sym, arg_vals->at(i));
		}
		return args[1]->eval(e);
	}
	Atom* normalize() override {
		assert(0);
		return nullptr;
	}
};

struct BuiltinLambda : Callable {
	typedef Atom* (*Func)(ArgList args);
	BuiltinLambda(Func f) : m_func(f) { }
	Atom* call(Env* env, ArgList arg_vals) override {
		std::vector<Atom*> args;
		for(auto arg : arg_vals ) {
			Atom* a = arg->eval(env);
			args.push_back(a);
		}
		return m_func(args);
	}
	Atom* normalize() override {
		return this;
	}
	Func m_func;
};

struct BuiltinVau : Callable {
	typedef Atom* (*Func)(Env*, ArgList);
	BuiltinVau(Func f) : m_func(f) { }
	Atom* call(Env* env, ArgList arg_vals) override {
		return (*m_func)(env, arg_vals);
	}
	Atom* normalize() override {
		return this;
	}
	Func m_func;
};

Atom* v_eval(Env* env, Callable::ArgList args) {
	assert(args.size()==1);
	return args[0]->eval(env);
}

Atom* v_type(Env* env, Callable::ArgList args) {
	assert(args.size() == 2);
	auto t = cast(Type, args[1]->eval(env));
	auto s = cast(Symbol, args[0]);
	assert(s);
	assert(t);
	s->m_type = t;
	return s;
}

Atom* v_begin(Env* env, Callable::ArgList args) {
	Atom* r = nullptr;
	for(auto a : args) {
		r = a->eval(env);
	}
	return r;
}

Atom* v_define(Env* env, Callable::ArgList args) {
	assert(args.size() == 2);
	if(auto s = cast(Symbol, args[0])) {
		Atom* v = args[1]->eval(env);
		env->put(s->m_sym, v);
		return v;
	}
	error("define needs a symbol");
	return nullptr;
}

Atom* v_lambda(Env* env, Callable::ArgList args) {
	assert(args.size() == 2);
	if(List* names = cast(List, args[0])) {
		return new Lambda(env, names, args[1]);
	}
	return nullptr;
}

Atom* v_vau(Env* env, Callable::ArgList args) {
	assert(args.size() == 3);
	assert(cast(List, args[0]));
	assert(cast(Symbol, args[1]));
	if(List* names = cast(List, args[0])) {
		return new Vau(env, names, args[1], args[2]);
	}
	return nullptr;
}

Atom* v_let(Env* env, Callable::ArgList args) {
	assert(args.size() == 2);
	List* lets = cast(List, args[0]);
	Env* e = new Env(env);
	for(auto item : *lets) {
		List* l = cast(List, item);
		assert(l->size() == 2);
		Symbol* s = cast(Symbol, l->at(0));
		Atom* a = l->at(1)->eval(e);
		e->put(s->m_sym, a);
	}
	return args[1]->eval(e);
}

Atom* v_normalize(Env* env, Callable::ArgList args) {
	assert(args.size() >= 1);
	for(auto arg : args) {

	}
	return nullptr;
}

Atom* l_sub(Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = cast(Num, args[0])->m_num;
	for(auto arg : args.ltrim(1)) {
		Num* a = cast(Num, arg);
		acc -= a->m_num;
	}
	return new Num(acc);
}

Atom* l_mul(Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = 1;
	for(auto arg : args) {
		Num* a = cast(Num, arg);
		acc *= a->m_num;
	}
	return new Num(acc);
}
Atom* l_div(Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = cast(Num, args[0])->m_num;
	for(auto arg : args.ltrim(1)) {
		Num* a = cast(Num, arg);
		acc /= a->m_num;
	}
	return new Num(acc);
}

Atom* l_print(Callable::ArgList args) {
	const char* sep = "";
	for(auto arg : args) {
		printf("%s", sep);
		arg->print();
		sep = " ";
	}
	printf("\n");
	return nullptr;
}

Atom* l_normalize(Callable::ArgList args) {
	assert(args.size() == 1);
	return args[0]->normalize();
}

const char* strndup(const char* s, const char* e) {
	size_t n = e - s;
	char* p = new char[n+1];
	memcpy(p, s, n);
	p[n] = 0;
	return p;
}

struct Input {
	Input(const char* s, const char* n="<input>") : start(s), idx(0), name(n) {}
	void eatwhite() {
		while(isspace(start[idx])) {
			++idx;
		}
	}
	char peek() const {
		return start[idx];
	}
	char next() {
		return start[idx++];
	}
	const char* peekbuf() const {
		return start + idx;
	}
	const char* start;
	const char* name;
	int idx;
};

Atom* parse_string(Input& in) {
	Atom* ret = nullptr;
	while(ret == nullptr) {
		switch(in.peek()) {
			case '\0':
				return nullptr;
			case ' ':
			case '\r':
			case '\n':
			case '\t':
				in.eatwhite();
				break;
			case ';':
				while(int c = in.next()) {
					if(c == '\n') { break; }
				}
				break;
			case '(':{
				in.next();
				List* lst = new List();
				while(Atom* a = parse_string(in)) {
					lst->append(a);
				}
				assert(in.next() == ')');
				ret = lst;
				break;
			}
			case ')':
				return nullptr;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				int value = in.next() - '0';
				while(int c = in.peek()) {
					//if(isdigit(c) || isalpha(c) || c == '_'){
					if(isdigit(c)) {
						value = value * 10 + c - '0';
						in.next();
					}
					else break;
				}
				ret = new Num(value);
				break;
			}
			case '"': {
				in.next();
				const char* s = in.peekbuf();
				while(ret == nullptr) {
					switch(int c = in.next()) {
						case 0:
							error("eof in string"); return nullptr;
						case '"':
							ret = new String(strndup(s, in.peekbuf() - 1));
							break;
						default:
							break;
					}
				}
				break;
			}
			default: {
				if(isalpha(in.peek()) || in.peek() == '_' || in.peek()=='@') {
					const char* s = in.peekbuf();
					in.next();
					while(int c = in.peek()) {
						if(isdigit(c) || isalpha(c) || c == '@' || c == '_' || c == '?' || c == '!'){
							in.next();
						}
						else break;
					}
					ret = new Symbol(strndup(s, in.peekbuf()));
					break;
				}
				else throw 0;
			}
		}
	}
	in.eatwhite();
	if(in.peek() == ':') {
		in.next();
		Atom* rhs = parse_string(in);
		rhs->m_type = ret;
		ret = rhs;
	}
	return ret;
}

Atom* parse_file(const char* fname) {
	FILE* fin = fopen(fname, "r");
	std::vector<char> txt;
	while(1) {
		char buf[4096];
		size_t n = fread(buf, 1, sizeof(buf), fin);
		if(n == -1) error("Read error");
		else if(n == 0) break;
		else txt.insert(txt.end(), buf, buf + n);
	}
	txt.push_back(0);
	Input input(&txt[0], fname);
	List* l = new List();
	l->append(new Symbol("begin"));
	while( Atom* a = parse_string(input) ) {
		l->append( a );
	}
	return l;
}

int main() {
	Atom* prog = parse_file("d:/sk/slip/slip_6.slip");
	Env* env = new Env(nullptr);
	env->put("eval", new BuiltinVau(&v_eval));
	env->put("begin", new BuiltinVau(&v_begin));
	env->put("define", new BuiltinVau(&v_define));
	env->put("lambda", new BuiltinVau(&v_lambda));
	env->put("vau", new BuiltinVau(&v_vau));
	env->put("let", new BuiltinVau(&v_let));
	env->put("print", new BuiltinLambda(&l_print));
	env->put("mul", new BuiltinLambda(&l_mul));
	env->put("div", new BuiltinLambda(&l_div));
	env->put("sub", new BuiltinLambda(&l_sub));
	env->put("normalize", new BuiltinLambda(&l_normalize));
	env->put("Num", &Type::s_num);
	v_eval(env, Callable::ArgList(&prog,&prog+1));
    return 0;
}