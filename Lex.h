#pragma once
#include "Source.h"
#include "Reflect.h"

namespace Slip::Lex {
        /// Load a file and parse it
    unique_ptr<List> parse_input( Input& input );


    struct Atom : public Reflect::AbstractReflected {
    public:
        REFLECT_DECL();
        Atom( const SourceLocation& loc ) : m_loc( loc ) {}

        SourceLocation m_loc{};
        Atom* m_decltype = nullptr;
        vector<Atom*> m_attrs;

        Atom() = default;
    protected:
        static const char* indent( int i );
    };

    struct Value : Atom {
        REFLECT_DECL();
        Value() = default;
        Value( const SourceLocation& loc ) : Atom( loc ) {}
        string_view text() const {
            auto s = m_loc.m_file->m_contents.c_str();
            return { s + m_loc.m_start, m_loc.m_end - m_loc.m_start };
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
        size_t size() const {
            return m_items.size();
        }
	    Atom* at(int i) const {
            return m_items[i];
        }
        void append( Atom* a ) {
            m_items.push_back( a );
        }
        array_view< Atom* > items() {
            return m_items;
        }
        vector< Atom* > m_items;
    };
}
