const char *ifTerminal(const char*);

#define BLACK ifTerminal("\x1b[30m")
#define RED ifTerminal("\x1b[31m")
#define GREEN ifTerminal("\x1b[32m")
#define YELLOW ifTerminal("\x1b[33m")
#define BLUE ifTerminal("\x1b[34m")
#define MAGENTA ifTerminal("\x1b[35m")
#define CYAN ifTerminal("\x1b[36m")
#define WHITE ifTerminal("\x1b[37m")
#define GRAY ifTerminal("\x1b[90m")

#define RESET ifTerminal("\x1b[0m")
#define BOLD ifTerminal("\x1b[1m")
#define BRIGHT ifTerminal("\x1b[1m")
#define DIM = ifTerminal("\x1b[2m")

