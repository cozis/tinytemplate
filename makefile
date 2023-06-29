
all: tt

tt: src/cli.c src/tinytemplate.c 3p/xjson.c
	gcc $^ -o $@ -Wall -Wextra -g -I3p

clean:
	rm tt tt.exe
