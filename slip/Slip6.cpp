
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

#define Error( fmt, ... ) Detail::_Error("%s(%i,%i): error " fmt, __FILE__, __LINE__, 0, __VA_ARGS__)
#define Error_At( loc, fmt, ... ) Detail::_Error("%s(%i,%i): error " fmt, loc.filename(), loc.line(), loc.col(), __VA_ARGS__)

namespace Detail {
	void _Error( const char* fmt, ... ) {
		va_list va;
		va_start( va, fmt );
		char buf[2048];
		vsnprintf( buf, sizeof( buf ), fmt, va );
		va_end( va );
		printf( "%s", buf );
		OutputDebugStringA( buf );
		throw 0;
	}
}



template<typename T>
struct view_ptr {
	view_ptr() : m_ptr( nullptr ) {}
	view_ptr(T* p) : m_ptr( p ) {}
	operator T*( ) const { return m_ptr; }
	T* operator->( ) const { return m_ptr; }
	T* m_ptr;
};

template<typename T>
struct array_view {
	array_view(const T* s, const T* e) : m_begin(s), m_end(e) {}
	array_view(const std::vector<T>& a) : m_begin(a.data()), m_end(m_begin+a.size()) {}
	static array_view<T> from_single( const T& t ) { return array_view( &t, &t+1 ); }
	size_t size() const { return m_end - m_begin; }
	const T& operator[](unsigned i) const { assert(i < size()); return m_begin[i]; }
	const T* begin() const { return m_begin; }
	const T* end() const { return m_end; }
	array_view<T> ltrim(unsigned n) const { assert(n <= size()); return array_view<T>(m_begin + n, m_end); }
	const T* m_begin;
	const T* m_end;
};

struct Atom;
struct Symbol;
struct Env;
struct List;
struct Type;

struct SourceManager {
	struct FileInfo {
		std::string m_name;
		std::string m_contents;
	};

	struct Location {
		view_ptr<const FileInfo> m_file;
		long m_off;
		const char* filename() const {
			return m_file ? m_file->m_name.data() : "<unnamed>";
		}
		int line() const {
			if( m_file ) {
				auto& txt = m_file->m_contents;
				return std::count( txt.begin(), txt.begin() + m_off, '\n' ) + 1;
			}
			return 0;
		}
		int col() const {
			if( m_file ) {
				auto& txt = m_file->m_contents;
				auto nl = txt.rfind( '\n', m_off );
				return m_off - (( nl == std::string::npos ) ? 0 : nl);
			}
			return 0;
		}
	};

	struct Input {
		Input( const char* s, const char* e, FileInfo* n ) : start( s ), cur(s), end( e ), info( n ) {}
		explicit operator bool() const {
			return cur != end;
		}
		void eatwhite() {
			while( cur != end && isspace( *cur ) ) {
				++cur;
			}
		}
		int peek() const {
			return *cur;
		}
		int next() {
			if( cur == end ) return -1;
			return *cur++;
		}
		const char* peekbuf() const {
			return cur;
		}
		Location location() const {
			return Location{ info, cur - start };
		}
		const char* cur;
		const char* start;
		const char* end;
		const FileInfo* info;
	};

	Input load( const char* fname ) {
		while( 1 ) {
			auto it = m_files.find( fname );
			if( it != m_files.end() ) {
				auto& txt = it->second->m_contents;
				return Input( &txt[0], &txt[0]+txt.size(), it->second );
			}
			else if( FILE* fin = fopen( fname, "r" ) ) {
				std::string txt;
				while( 1 ) {
					char buf[4096];
					size_t n = fread( buf, 1, sizeof( buf ), fin );
					if( n == -1 ) error( "Read error" );
					else if( n == 0 ) break;
					else txt.append( buf, buf + n );
				}
				m_files[fname] = new FileInfo{ fname, txt };
			}
			else {
				return Input( nullptr, nullptr, nullptr );
			}
		}
	}
	
	std::map< std::string, FileInfo* > m_files;
};

namespace Detail {
	void _Error_At( SourceManager::Location loc, char* fmt, ... ) {
		va_list va;
		va_start( va, fmt );
		vprintf( fmt, va );
		va_end( va );
	}
}

struct Atom {
	virtual ~Atom() {}
	virtual Atom* eval(Env* env) = 0;
	virtual Atom* normalize() = 0;
	virtual void print() const = 0;
	Atom() : m_type(nullptr) {}
	Atom* m_type;
	SourceManager::Location m_loc;
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
	Bool( bool v ) : m_val( v ) {}
	void print() const override {
		printf( m_val ? "true" : "false" );
	}
	bool m_val;

	static Bool s_false;
	static Bool s_true;
};

Bool Bool::s_false{ false };
Bool Bool::s_true{ true };


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

struct Symbol : Atom {
	Symbol( const char* s ) : m_sym( s ) {}
	Atom* eval( Env* env ) override;
	void print() const override {
		printf( "'%s", m_sym );
	}
	Atom* normalize() {
		return this;
	}
	const char* m_sym;
};

struct Env : Value {
	Env(Env *p) : m_parent(p) { }
	Atom* get(const Symbol* sym) {
		const char* s = sym->m_sym;
		for(Env* e = this; e; e = e->m_parent) {
			auto it = e->m_tab.find(s);
			if(it != e->m_tab.end()) {
				return it->second;
			}
		}
		Error_At(sym->m_loc, "Symbol '%s' not found", s);
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

Atom* Symbol::eval( Env* env ) {
	return env->get( this );
}


struct Callable : Value {
	typedef array_view<Atom*> ArgList;
	virtual Atom* call(Env* env, Atom* arg0, ArgList args) = 0;
	void print() const override {
		printf("<callable>");
	}
};

struct List : Atom {
	typedef std::vector<Atom*> vector;
	Atom* eval(Env* env) override {
		if( lst.empty() ) {
			Error_At( m_loc, "Empty list is illegal" );
		}
		Atom* atom = lst[0]->eval(env);
		if(auto call = cast(Callable, atom)) {
			return call->call(env, lst[0], Callable::ArgList(lst).ltrim(1));
		}
		Error_At(lst[0]->m_loc, "Expected a callable as the first argument");
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
	Atom* call(Env* env, Atom* arg0, ArgList arg_vals) override {
		Env* e = new Env(m_lex_env);
		if( m_arg_names->size() != arg_vals.size() ) {
			Error_At( arg0->m_loc, "Wrong number of arguments. Expected %i, got %i", m_arg_names->size(), arg_vals.size() );
		}
		for(unsigned i = 0; i < m_arg_names->size(); ++i) {
			Atom* a = arg_vals[i]->eval(env);
			auto s = cast(Symbol, m_arg_names->at(i));
			if( s->m_type != nullptr ) {
				auto nt = s->m_type->eval( env );
				auto at = a->m_type->eval( env );
				if( at != nt ) {
					Error_At( arg_vals[i]->m_loc, "Mismatched type for argument '%i'", i );
				}
			}
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

	Atom* call(Env* env, Atom* arg0, ArgList args) override {
		assert(args.size() == 2);
		Env* e = new Env(env);
		List* arg_vals = cast(List, args[0]);
		assert(arg_vals->size() == m_arg_names->size());
		for(unsigned i = 0; i < arg_vals->size(); ++i) {
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
	typedef Atom* (*Func)(Env* env, ArgList args);
	BuiltinLambda(Func f) : m_func(f) { }
	Atom* call(Env* env, Atom* arg0, ArgList arg_vals) override {
		std::vector<Atom*> args;
		for(auto arg : arg_vals ) {
			Atom* a = arg->eval(env);
			args.push_back(a);
		}
		return m_func(env, args);
	}
	Atom* normalize() override {
		return this;
	}
	Func m_func;
};

struct BuiltinVau : Callable {
	typedef Atom* (*Func)(Env*, ArgList);
	BuiltinVau(Func f) : m_func(f) { }
	Atom* call(Env* env, Atom* arg0, ArgList arg_vals) override {
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
		auto r = new Lambda(env, names, args[1]);
		r->m_loc = args[0]->m_loc;
		return r;
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

Atom* v_cond( Env* env, Callable::ArgList args ) {
	for( auto arg : args ) {
		if( List* l = cast( List, arg ) ) {
			if( l->size() != 2 ) {
				Error_At( l->m_loc, "Expected list of size 2" );
			}
			if( l->at( 0 )->eval(env) != &Bool::s_false ) {
				return l->at( 1 )->eval( env );
			}
		}
		else {
			Error_At( arg->m_loc, "Expected list of size 2" );
		}
	}
	return nullptr;
}

Atom* l_eq( Env* env, Callable::ArgList args ) {
	if( args.size() != 2 ) {
		Error( "Expected list of size 2" );
	}
	if( Num* n0 = cast( Num, args[0] ) ) { // hack
		if( Num* n1 = cast( Num, args[1] ) ) { // hack
			return n0->m_num == n1->m_num ? &Bool::s_true : &Bool::s_false;
		}
	}
	Error( "fixme" );
	return &Bool::s_false;
}

Atom* l_map( Env* env, Callable::ArgList args ) {
	if( args.size() != 2 ) {
		Error( "Expected list of size 2" );
	}
	if( Callable* c = cast( Callable, args[0] ) ) {
		if( List* l = cast( List, args[1] ) ) {
			List* r = new List();
			for( auto i : *l ) {
				r->append( c->call( env, c, Callable::ArgList::from_single(i) ) );
			}
			return r;
		}
	}
	Error( "fixme" );
	return nullptr;
}

Atom* l_add( Env* env, Callable::ArgList args ) {
	assert( args.size() >= 1 );
	int acc = 0;
	for( auto arg : args ) {
		Num* a = cast( Num, arg );
		acc += a->m_num;
	}
	return new Num( acc );
}

Atom* l_sub( Env* env, Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = cast(Num, args[0])->m_num;
	for(auto arg : args.ltrim(1)) {
		Num* a = cast(Num, arg);
		acc -= a->m_num;
	}
	return new Num(acc);
}

Atom* l_mul( Env* env, Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = 1;
	for(auto arg : args) {
		Num* a = cast(Num, arg);
		acc *= a->m_num;
	}
	return new Num(acc);
}

Atom* l_div( Env* env, Callable::ArgList args) {
	assert(args.size() >= 1);
	int acc = cast(Num, args[0])->m_num;
	for(auto arg : args.ltrim(1)) {
		Num* a = cast(Num, arg);
		acc /= a->m_num;
	}
	return new Num(acc);
}

Atom* l_print(Env* env, Callable::ArgList args) {
	const char* sep = "";
	for(auto arg : args) {
		printf("%s", sep);
		arg->print();
		sep = " ";
	}
	printf("\n");
	return nullptr;
}

Atom* l_normalize( Env* env, Callable::ArgList args) {
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


Atom* parse_string(SourceManager::Input& in) {
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
				List* lst = new List();
				lst->m_loc = in.location();
				in.next();
				while(Atom* a = parse_string(in)) {
					lst->append(a);
				}
				if( in.next() != ')' ) {
					Error_At( lst->m_loc, "Missing ')' for list begun here" );
					throw 0;
				}
				ret = lst;
				break;
			}
			case ')':
				return nullptr;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				auto loc = in.location();
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
				ret->m_loc = loc;
				break;
			}
			case '"': {
				auto loc = in.location();
				in.next();
				const char* s = in.peekbuf();
				while(ret == nullptr) {
					switch(int c = in.next()) {
						case -1:
						case 0:
							Error_At(loc, "End of input while parsing quoted string");
							return nullptr;
						case '"':
							ret = new String(strndup(s, in.peekbuf() - 1));
							ret->m_loc = loc;
							break;
						default:
							break;
					}
				}
				break;
			}
			default: {
				if(isalpha(in.peek()) || in.peek() == '_' || in.peek()=='@') {
					auto loc = in.location();
					const char* s = in.peekbuf();
					in.next();
					while(int c = in.peek()) {
						if(isdigit(c) || isalpha(c) || c == '@' || c == '_' || c == '?' || c == '!'){
							in.next();
						}
						else break;
					}
					ret = new Symbol(strndup(s, in.peekbuf()));
					ret->m_loc = loc;
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

Atom* parse_file(SourceManager& sm, const char* fname) {
	if( SourceManager::Input input = sm.load( fname ) ) {
		List* l = new List();
		l->append( new Symbol( "begin" ) );
		while( Atom* a = parse_string( input ) ) {
			l->append( a );
		}
		return l;
	}
	else {
		Error( "Unable to open '%s'", fname );
		return nullptr;
	}
}

int evaluate(const char* fname, Atom* argv) {
	SourceManager sm;
	if( Atom* prog = parse_file( sm, fname ) ) {
		Env* env = new Env( nullptr );
		env->put( "eval", new BuiltinVau( &v_eval ) );
		env->put( "begin", new BuiltinVau( &v_begin ) );
		env->put( "define", new BuiltinVau( &v_define ) );
		env->put( "lambda", new BuiltinVau( &v_lambda ) );
		env->put( "vau", new BuiltinVau( &v_vau ) );
		env->put( "let", new BuiltinVau( &v_let ) );
		env->put( "cond", new BuiltinVau( &v_cond ) );
		env->put( "print", new BuiltinLambda( &l_print ) );
		env->put( "add", new BuiltinLambda( &l_add ) );
		env->put( "mul", new BuiltinLambda( &l_mul ) );
		env->put( "div", new BuiltinLambda( &l_div ) );
		env->put( "sub", new BuiltinLambda( &l_sub ) );
		env->put( "normalize", new BuiltinLambda( &l_normalize ) );
		env->put( "eq?", new BuiltinLambda( &l_eq ) );
		env->put( "map", new BuiltinLambda(&l_map) );
		env->put( "Num", &Type::s_num );
		env->put( "true", &Bool::s_true );
		env->put( "false", &Bool::s_false );
		env->put( "argv", argv );
		v_eval( env, Callable::ArgList( &prog, &prog + 1 ) );
		return 0;
	}
	return 1;
}

int main(int argc, const char* argv[]) {
	try {
		List* args = new List();
		static SourceManager::FileInfo cmdline{ "cmdline" };
		for( int i = 1; i < argc; ++i ) {
			SourceManager::Input in( argv[i], argv[i] + strlen( argv[i] ), &cmdline );
			args->append( parse_string( in ) );
		}
		//return evaluate( "../slip_6.slip" );
		int ret = evaluate( "bench/ack.slip", args );
		return ret;
	}
	catch(float) {
		return 1;
	}
}
