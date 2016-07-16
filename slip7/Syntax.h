#pragma once
// Atom
// .List
// .Number
// .String
// .Type

static const char* indent( int n ) {
    static char buf[128];
    for( int i = 0; i < n; ++i )
        buf[i] = ' ';
    buf[n] = 0;
    return buf;
}

struct Atom {
    typedef SourceManager::Location Location;
    Atom(const Location& loc) : m_loc(loc) {}
    virtual ~Atom() {}
    virtual void print(int i=0) const = 0;

    Location m_loc;
//    Atom* m_type;
};


struct Type : public Atom {

};

struct List : public Atom {
    List( const Location& loc ) : Atom( loc ) {
    }
    void append( Atom* a ) {
        items.push_back( a );
    }
    void print(int i) const override {
        printf( "%s(\n", indent(i) );
        for( auto a : items ) {
            a->print(i+1);
        }
        printf( "%s)\n", indent(i) );
    }
    std::vector<Atom*> items;
};

struct Value : Atom {
    Value( const Location& loc ) : Atom( loc ) {}
    void print(int i) const override {
        auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end-m_loc.m_start );
        printf( "%s%s\n", indent(i), s.c_str() );
    }
};

struct String : Value {
    String( const Location& loc ) : Value( loc ) {}
    void print(int i) const override {
        auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        printf( "%s\"%s\"\n", indent(i), s.c_str() );
    }
};

struct Symbol : Value {
    Symbol( const Location& loc ) : Value( loc ) {}

};

struct Number : Value {
    Number( const Location& loc ) : Value( loc ) {}
    void print(int i) const override {
        auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        printf( "%s#%s\n", indent(i), s.c_str() );
    }
};





