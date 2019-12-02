#pragma once
#include "Slip.h"

namespace Slip::Io {
    struct TextOutput {
        FILE* m_file{ nullptr };
        bool m_close{ false };
        std::string m_indent;
        enum class State {
            Normal,
            Start,
            Mid,
        };
        State m_state{ State::Normal };

        TextOutput()
            : m_file(stdout)
            , m_close(false) {
        }
        TextOutput(const char* fname)
            : m_file(fopen(fname, "w"))
            , m_close(true) {
        }
        ~TextOutput() {
            if (m_close) {
                fclose(m_file);
            }
        }
        void begin(string_view s) {
            write(s);
            m_indent.push_back(' ');
            //m_sep = false;
        }

        /// Write, indenting on newlines
        void write(string_view s);
        /// Raw Write
        void _write(string_view s);

        //void write(const void* s) {
        //    fprintf(m_file, "%p", s);
        //    m_sep = true;
        //}
        void sep() {
            //if (m_sep) {
            //    fprintf(m_file, " ");
            //    m_sep = false;
            //}
        }
        void end(string_view s = {}) {
            m_indent.erase(m_indent.size() - 1);
            write(s);
        }
        void nl() {
            fprintf(m_file, "\n");
            m_state = State::Start;
        }
    };

    struct SourceManager {
        virtual ~SourceManager() = default;
        virtual TextInput load(const char* fname) = 0;
    };

    struct SourceNameAndContents {
        string m_name;
        string m_contents;
    };

    struct SourceLocation {

        const SourceNameAndContents* m_file{ nullptr };
        size_t m_start{ 0 };
        size_t m_end{ 0 };

        SourceLocation() = default;

        SourceLocation(const SourceNameAndContents* f, long s, long e = -1) : m_file(f), m_start(s), m_end(e>=0?e:s) {
        }

        SourceLocation(SourceLocation s, SourceLocation e) : m_file(s.m_file), m_start(s.m_start), m_end(e.m_start) {
            assert(s.m_file == e.m_file);
        }
        const char* filename() const {
            return m_file ? m_file->m_name.data() : "<unnamed>";
        }
        int line() const;
        int col() const;

        string_view text() const {
            auto s = m_file->m_contents.c_str();
            assert(m_end >= 0);
            return { s + m_start, m_end - m_start };
        }
    };

    struct TextInput {
        TextInput() = default;
        TextInput(const char* s, const char* e, SourceNameAndContents* n)
            : cur(s), start(s), end(e), info(n) {}

        explicit operator bool() const {
            return cur != end;
        }
        void eatwhite() {
            while (cur != end && isspace(*cur)) {
                ++cur;
            }
        }
        bool available() const {
            return cur != end;
        }
        int peek() const {
            assert(cur < end);
            return *cur;
        }
        int next() {
            if (cur == end) return -1;
            return *cur++;
        }
        long tell() const {
            return safe_cast(cur - start);
        }
        long tellEnd() const {
            return safe_cast(end - start);
        }
        SourceLocation location() const {
            return SourceLocation(info, tell());
        }
        SourceLocation location(long s, long e = -1) const {
            return SourceLocation(info, s, e >= 0 ? e : s + 1);
        }
        const char* cur{ nullptr };
        const char* start{ nullptr };
        const char* end{ nullptr };
        const SourceNameAndContents* info{ nullptr };
    };
}
