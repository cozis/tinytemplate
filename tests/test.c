
struct {
    const char *src;
    const char *exp;
    const char *params;
} cases[] = {

};

typedef enum {
    SECTION_INPUT,
    SECTION_OUTPUT,
    SECTION_PARAMS,
} Section;

static void runTest(const char *src, size_t len)
{
    size_t params_offset;
    size_t params_length;
    size_t input_offset;
    size_t input_length;
    size_t output_offset;
    size_t output_length;

    size_t i = 0;
    while (1) {
        while (i < len && src[i] != '@')
            i++;

        if (i < len && isalpha(src[i])) {

            size_t kword_off = i;
            do
                i++;
            while (i < len && isalpha(src[i]));
            size_t kword_len = i - kword_off;
            
            bool is_kword = true;
            Section section;
            if (kword_len == 6 && !strncmp("params", src + kword_off, 6)) {
                section = SECTION_PARAMS;
            } else if (kword_len == 5 && !strncmp("input", src + kword_off, 5)) {
                section = SECTION_INPUT;
            } else if (kword_len == 6 && !strncmp("output", src + kword_off, 6)) {
                section = SECTION_OUTPUT;
            } else
                is_kword = false;

            if (is_kword) {
                // Get to the end of the line
                while (i < len && (src[i] == ' ' || src[i] == '\t' || src[i] == '\r'))
                    i++;
                if (i < len && src[i] != '\n') {
                    
                }
                while (i < len && src[i] != '\n')
                    i++;
                if (i < len) {
                    assert(src[i] == '\n');
                    i++;
                }
                switch (section) {
                    case SECTION_INPUT:   input_offset = i; break;
                    case SECTION_OUTPUT: output_offset = i; break;
                    case SECTION_PARAMS: params_offset = i; break;
                }
            }
        }
        
    }
}

int main(void)
{

    return 0;
}
