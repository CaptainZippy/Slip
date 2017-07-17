#pragma once
#include "SourceManager.h"
#include "Reflect.h"

namespace Syntax {
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
        void print( int i ) const { _print( i ); }

        SourceLocation m_loc{};
        Atom* m_type = nullptr;
        std::vector<Atom*> m_attrs;

		Atom() {}
    protected:
		virtual void _print(int i) const {}// = 0;
        static const char* indent( int i );
    };

    struct Value : Atom {
		REFLECT_DECL();
        Value( const SourceLocation& loc ) : Atom( loc ), m_text( text() ) {}
        void _print( int i ) const override {
            auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
            printf( "%s%s\n", indent( i ), s.c_str() );
        }
        std::string text() const {
            return m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
        }
		Value() {}
    protected:
        std::string m_text;
    };
	


    struct String : Value {
        String( const SourceLocation& loc ) : Value( loc ) {}

        void _print( int i ) const override {
            printf( "%s\"%s\"\n", indent( i ), text().c_str() );
        }
    };

    struct Symbol : Value {
        Symbol( const SourceLocation& loc ) : Value( loc ) {}
    };

    struct Number : Value {
        Number( const SourceLocation& loc ) : Value( loc ) {}
    protected:
        void _print( int i ) const override {
            auto s = m_loc.m_file->m_contents.substr( m_loc.m_start, m_loc.m_end - m_loc.m_start );
            printf( "%s#%s\n", indent( i ), s.c_str() );
        }
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
        void _print( int i ) const override {
            printf( "%s(\n", indent( i ) );
            for( auto& a : items ) {
                a->print( i + 1 );
            }
            printf( "%s)\n", indent( i ) );
        }

        std::vector< Atom* > items;
    };
}
