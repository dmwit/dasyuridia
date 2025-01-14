#include <lua.h>
#include <poll.h>

static struct pollfd req = {0, POLLIN, 0};

static int poll_stdin(lua_State *L) {
	lua_pushboolean(L, poll(&req, 1, 0));
	return 1;
}

extern int luaopen_poll_stdin(lua_State *L) {
	lua_pushcfunction(L, poll_stdin);
	lua_setglobal(L, "poll_stdin");
	lua_getglobal(L, "poll_stdin");
	return 1;
}
