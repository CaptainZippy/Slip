#pragma once
#include "Source.h"
#include "Reflect.h"

namespace Lex {
    struct Atom;
    struct List;
    struct Number;
    struct String;
    struct Symbol;

    Atom* parse_string( Input& in );
    Atom* parse_one( Input& in );
    Result parse_file( SourceManager& sm, const char* fname, List** out );


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
        Value() {}
        Value( const SourceLocation& loc ) : Atom( loc ) {}
        string_view text() const {
            auto s = m_loc.m_file->m_contents.c_str();
            return { s + m_loc.m_start, s + m_loc.m_end };
        }
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
