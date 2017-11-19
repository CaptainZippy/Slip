#pragma once
#include "SourceManager.h"
#include "Reflect.h"

namespace Lex {
    struct Atom;
    struct List;
    struct Number;
    struct String;
    struct Symbol;

    static Atom* parse_string( Input& in );
    Atom* parse_one( Input& in );
    List* parse_file( SourceManager& sm, const char* fname );


    struct Atom : public Reflect::AbstractReflected {
    public:
        REFLECT_DECL();
        Atom( const SourceLocation& loc ) : m_loc( loc ) {}
        virtual ~Atom() {}

        SourceLocation m_loc{};
        Atom* m_decltype = nullptr;
        std::vector<Atom*> m_attrs;

        Atom() {}
    protected:
        static const char* indent( int i );
    };

    struct Value : Atom {
        REFLECT_DECL();
        Value( const SourceLocation& loc ) : Atom( loc ), m_text( text() ) {}
        std::string text() const {
            return m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        }
        Value() {}
    protected:
        std::string m_text;
    };
    


    struct String : Value {
        String( const SourceLocation& loc ) : Value( loc ) {}
    };

    struct Symbol : Value {
        Symbol( const SourceLocation& loc ) : Value( loc ) {}
    };

    struct Number : Value {
        Number( const SourceLocation& loc ) : Value( loc ) {}
    };


    struct List : public Atom {
        List( const SourceLocation& loc ) : Atom( loc ) {
        }
        int size() const {
            return (int) items.size();
        }
        void append( Atom* a ) {
            items.push_back( a );
        }
        std::vector< Atom* > items;
    };
}
