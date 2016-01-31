
struct Atom {
    struct Tag { enum Enum { Null, Bool, Num, String, Symbol, Pair, Operative }; };
    Atom( Tag::Enum t ) : tag( t ) {}
    Tag::Enum tag;
};

struct Bool : Atom {
    Bool() : Atom(Tag::Bool), val(false) {}
    bool val;
};

struct Num : Atom {
    Num() : Atom( Tag::Num ), num( 0 ) {}
    int num;
};

struct String : Atom {
    String() : Atom( Tag::String ), str( nullptr ) {}
    const char* str;
};

struct Symbol : Atom {
    Symbol() : Atom( Tag::Symbol ), sym( nullptr ) {}
    const char* sym;
};

struct Pair : Atom {
    Pair() : Atom( Tag::Pair ), car( nullptr ), cdr( nullptr ) {}
    Atom* car;
    Atom* cdr;
};

struct Operative : Atom {
};

Atom* parse_string( const char* txt, const char* inputname = "<input>" ) {
    Atom* ret = new List( [Ident( "begin" )] )
}

Atom* parse_file( const char* fname ) {

}


int main() {
    return 0;
}