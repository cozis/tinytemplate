
all: tt

tt: cli.c tinytemplate.c xjson.c
	gcc $^ -o $@ -Wall -Wextra -g

clean:
	rm tt tt.exe
