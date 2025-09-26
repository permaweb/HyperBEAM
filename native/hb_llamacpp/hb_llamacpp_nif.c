#include <erl_nif.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdlib.h>

static ErlNifResourceType* SERVER_RES_TYPE = NULL;

typedef struct {
    pid_t pid;
    char host[128];
    int port;
    char model[512];
} server_state_t;

static server_state_t g_state = { .pid = 0 };

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_already_running;
static ERL_NIF_TERM atom_binary_not_found;
static ERL_NIF_TERM atom_spawn_failed;
static ERL_NIF_TERM atom_health_timeout;
static ERL_NIF_TERM atom_not_running;
static ERL_NIF_TERM atom_port_in_use;
static ERL_NIF_TERM atom_model_not_found;

static int is_listening(const char* host, int port) {
    // simple connect check
    int sockfd;
    struct sockaddr_in servaddr;
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(port);
    servaddr.sin_addr.s_addr = inet_addr(host);

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) return 0;
    int ret = connect(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr));
    close(sockfd);
    return ret == 0;
}

static int check_health_endpoint(const char* host, int port) {
    int sockfd;
    struct sockaddr_in servaddr;
    char request[512];
    char response[1024];
    int ret = 0;
    
    // Create socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) return 0;
    
    // Set socket timeout
    struct timeval timeout;
    timeout.tv_sec = 5;
    timeout.tv_usec = 0;
    setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
    setsockopt(sockfd, SOL_SOCKET, SO_SNDTIMEO, &timeout, sizeof(timeout));
    
    // Setup server address
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(port);
    servaddr.sin_addr.s_addr = inet_addr(host);
    
    // Connect
    if (connect(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr)) == 0) {
        // Send HTTP GET request to /health
        snprintf(request, sizeof(request), 
                "GET /health HTTP/1.1\r\n"
                "Host: %s:%d\r\n"
                "Connection: close\r\n"
                "\r\n", host, port);
        
        if (send(sockfd, request, strlen(request), 0) > 0) {
            // Read response
            ssize_t bytes_read = recv(sockfd, response, sizeof(response) - 1, 0);
            if (bytes_read > 0) {
                response[bytes_read] = '\0';
                // Check for HTTP 200 OK in response
                if (strstr(response, "200 OK") != NULL) {
                    ret = 1;
                }
            }
        }
    }
    
    close(sockfd);
    return ret;
}

static ERL_NIF_TERM start_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    int port;
    ErlNifBinary model_bin, host_bin;
    if (!enif_inspect_binary(env, argv[0], &model_bin)) return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &host_bin)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &port)) return enif_make_badarg(env);

    if (g_state.pid != 0) {
        return enif_make_tuple2(env, atom_error, atom_already_running);
    }

    // validate paths
    char model_path[512];
    memset(model_path, 0, sizeof(model_path));
    size_t msz = model_bin.size < sizeof(model_path)-1 ? model_bin.size : sizeof(model_path)-1;
    memcpy(model_path, model_bin.data, msz);
    model_path[msz] = '\0';
    if (access(model_path, R_OK) != 0) {
        return enif_make_tuple2(env, atom_error, atom_model_not_found);
    }

    const char* bin_path = "_build/llama.cpp/build/bin/llama-server";
    if (access(bin_path, X_OK) != 0) {
        return enif_make_tuple2(env, atom_error, atom_binary_not_found);
    }

    // check port
    char host_buf[128];
    memset(host_buf, 0, sizeof(host_buf));
    size_t hsz = host_bin.size < sizeof(host_buf)-1 ? host_bin.size : sizeof(host_buf)-1;
    memcpy(host_buf, host_bin.data, hsz);
    host_buf[hsz] = '\0';
    if (is_listening(host_buf, port)) {
        return enif_make_tuple2(env, atom_error, atom_port_in_use);
    }

    pid_t pid = fork();
    if (pid < 0) {
        return enif_make_tuple2(env, atom_error, atom_spawn_failed);
    }
    if (pid == 0) {
        // child: exec server
        char port_str[16];
        snprintf(port_str, sizeof(port_str), "%d", port);
        execl(bin_path, bin_path, "--samplers", "temperature=0", "-c", "16384", "-s", "1234", "-m", model_path, "--host", host_buf, "--port", port_str, (char*)NULL);
        _exit(127);
    }

    // parent: wait for health
    strncpy(g_state.host, host_buf, sizeof(g_state.host)-1);
    strncpy(g_state.model, model_path, sizeof(g_state.model)-1);
    g_state.port = port;
    g_state.pid = pid;

    // First wait for port to be listening, then check health endpoint
    // Wait up to 60s for large models (port listening + health check)
    const int port_tries = 600; // 60 seconds
    int port_ready = 0;
    
    for (int i = 0; i < port_tries; i++) {
        if (is_listening(g_state.host, g_state.port)) {
            port_ready = 1;
            break;
        }
        usleep(100000); // 100ms
    }
    
    if (!port_ready) {
        // timeout: kill child
        kill(pid, SIGKILL);
        waitpid(pid, NULL, 0);
        g_state.pid = 0;
        return enif_make_tuple2(env, atom_error, atom_health_timeout);
    }
    
    // Now check health endpoint (additional 30s timeout)
    const int health_tries = 1200; // 30 seconds
    for (int i = 0; i < health_tries; i++) {
        if (check_health_endpoint(g_state.host, g_state.port)) {
            return atom_ok;
        }
        usleep(100000); // 100ms
    }

    // timeout: kill child
    kill(pid, SIGKILL);
    waitpid(pid, NULL, 0);
    g_state.pid = 0;
    return enif_make_tuple2(env, atom_error, atom_health_timeout);
}

static ERL_NIF_TERM stop_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    (void)argc; (void)argv;
    if (g_state.pid != 0) {
        kill(g_state.pid, SIGKILL);
        waitpid(g_state.pid, NULL, 0);
        g_state.pid = 0;
        g_state.host[0] = '\0';
        g_state.model[0] = '\0';
        g_state.port = 0;
    }
    return atom_ok;
}

static int on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info) {
    (void)priv; (void)load_info;
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_already_running = enif_make_atom(env, "already_running");
    atom_binary_not_found = enif_make_atom(env, "binary_not_found");
    atom_spawn_failed = enif_make_atom(env, "spawn_failed");
    atom_health_timeout = enif_make_atom(env, "health_timeout");
    atom_not_running = enif_make_atom(env, "not_running");
    atom_port_in_use = enif_make_atom(env, "port_in_use");
    atom_model_not_found = enif_make_atom(env, "model_not_found");
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"start_server_nif", 3, start_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"stop_server_nif", 0, stop_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

ERL_NIF_INIT(dev_llamacpp_nif, nif_funcs, on_load, NULL, NULL, NULL)
