#include <zephyr/kernel.h>

#include <zephyr/device.h>
#include <zephyr/drivers/uart.h>

#include <zephyr/sys/byteorder.h>
#include <zephyr/sys/printk.h>

#include <zephyr/net/coap.h>
#include <zephyr/net/coap_link_format.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef CONFIG_SOC_POSIX
#include <sys/types.h>
#include <zephyr/drivers/console/native_posix_console.h>
pid_t getpid(void);
#endif /* CONFIG_SOC_POSIX */

#include <ufw/compiler.h>
#include <ufw/endpoints.h>
#include <ufw/hexdump.h>
#include <ufw/octet-buffer.h>
#include <ufw/rfc1055.h>

#include <sx-parser.h>

#include "ifc/text/spi.h"

cr_number port00_spi_state = 0u;

struct cr_port port00_spi = {
    .name = "port00-spi",
    .type = CR_PORT_TYPE_SPI,
    .api  = &cr_port_impl_spi_text,
    .data = &port00_spi_state,
    .cfg.spi = {
        .frame_length = 16u,
        .bit_order = CR_BIT_MSB_FIRST,
        .cs = {
            .number = 1u,
            .polarity = CR_LOGIC_INVERTED
        },
        .clk = {
            .rate = 0u,
            .edge = CR_EDGE_RISING,
            .phase_delay = false
        }
    },
    .lines = 0u,
    .line = 0u,
    .initialised = false
};

extern const struct device *uart0;
const struct device *uart1;

#define MAX_COAP_MSG_LEN 256
#define NUM_PENDINGS 10
static struct coap_pending pendings[NUM_PENDINGS];

static int
send_coap_reply(struct coap_packet *cpkt,
                const struct sockaddr *addr,
                socklen_t addr_len)
{
    int r;

    //r = sendto(sock, cpkt->data, cpkt->offset, 0, addr, addr_len);
    if (r < 0) {
        printk("Failed to send %d", errno);
        r = -errno;
    }

    return r;
}

static int
well_known_core_get(struct coap_resource *resource,
                    struct coap_packet *request,
                    struct sockaddr *addr, socklen_t addr_len)
{
    struct coap_packet response;
    uint8_t *data;
    int r;

    data = (uint8_t *)k_malloc(MAX_COAP_MSG_LEN);
    if (!data) {
        return -ENOMEM;
    }

    r = coap_well_known_core_get(resource, request, &response,
                                 data, MAX_COAP_MSG_LEN);
    if (r < 0) {
        goto end;
    }

    r = send_coap_reply(&response, addr, addr_len);

end:
    k_free(data);

    return r;
}
static int
test_get(struct coap_resource *resource,
         struct coap_packet *request,
         struct sockaddr *addr, socklen_t addr_len)
{
    struct coap_packet response;
    uint8_t payload[40];
    uint8_t token[COAP_TOKEN_MAX_LEN];
    uint8_t *data;
    uint16_t id;
    uint8_t code;
    uint8_t type;
    uint8_t tkl;
    int r;

    code = coap_header_get_code(request);
    type = coap_header_get_type(request);
    id = coap_header_get_id(request);
    tkl = coap_header_get_token(request, token);

    printk("*******");
    printk("type: %u code %u id %u", type, code, id);
    printk("*******");

    if (type == COAP_TYPE_ACK) {
        return 0;
    }

    data = (uint8_t *)k_malloc(MAX_COAP_MSG_LEN);
    if (!data) {
        return -ENOMEM;
    }

    r = coap_packet_init(&response, data, MAX_COAP_MSG_LEN,
                         COAP_VERSION_1, COAP_TYPE_ACK, tkl, token, 0, id);
    if (r < 0) {
        goto end;
    }

    r = send_coap_reply(&response, addr, addr_len);
    if (r < 0) {
        goto end;
    }

    if (type == COAP_TYPE_CON) {
        type = COAP_TYPE_CON;
    } else {
        type = COAP_TYPE_NON_CON;
    }

    /* Do not free and allocate "data" again, re-use the buffer */
    r = coap_packet_init(&response, data, MAX_COAP_MSG_LEN,
                         COAP_VERSION_1, type, tkl, token,
                         COAP_RESPONSE_CODE_CONTENT, id);
    if (r < 0) {
        goto end;
    }

    r = coap_append_option_int(&response, COAP_OPTION_CONTENT_FORMAT,
                               COAP_CONTENT_FORMAT_TEXT_PLAIN);
    if (r < 0) {
        goto end;
    }

    r = coap_packet_append_payload_marker(&response);
    if (r < 0) {
        goto end;
    }

    /* The response that coap-client expects */
    r = snprintk((char *) payload, sizeof(payload),
                 "Type: %u\nCode: %u\nMID: %u\n", type, code, id);
    if (r < 0) {
        goto end;
    }

    r = coap_packet_append_payload(&response, (uint8_t *)payload,
                                   strlen(payload));
    if (r < 0) {
        goto end;
    }

    r = send_coap_reply(&response, addr, addr_len);

end:
    k_free(data);

    return r;
}

static int
test_put(struct coap_resource *resource,
         struct coap_packet *request,
         struct sockaddr *addr, socklen_t addr_len)
{
    struct coap_packet response;
    uint8_t token[COAP_TOKEN_MAX_LEN];
    const uint8_t *payload;
    uint8_t *data;
    uint16_t payload_len;
    uint8_t code;
    uint8_t type;
    uint8_t tkl;
    uint16_t id;
    int r;

    code = coap_header_get_code(request);
    type = coap_header_get_type(request);
    id = coap_header_get_id(request);
    tkl = coap_header_get_token(request, token);

    printk("*******");
    printk("type: %u code %u id %u", type, code, id);
    printk("*******");

    payload = coap_packet_get_payload(request, &payload_len);
    if (payload) {
        //net_hexdump("PUT Payload", payload, payload_len);
    }

    if (type == COAP_TYPE_CON) {
        type = COAP_TYPE_ACK;
    } else {
        type = COAP_TYPE_NON_CON;
    }

    data = (uint8_t *)k_malloc(MAX_COAP_MSG_LEN);
    if (!data) {
        return -ENOMEM;
    }

    r = coap_packet_init(&response, data, MAX_COAP_MSG_LEN,
                         COAP_VERSION_1, type, tkl, token,
                         COAP_RESPONSE_CODE_CHANGED, id);
    if (r < 0) {
        goto end;
    }

    r = send_coap_reply(&response, addr, addr_len);

end:
    k_free(data);

    return r;
}

static int
test_post(struct coap_resource *resource,
          struct coap_packet *request,
          struct sockaddr *addr, socklen_t addr_len)
{
    static const char * const location_path[] = { "location1",
        "location2",
        "location3",
        NULL };
    const char * const *p;
    struct coap_packet response;
    uint8_t token[COAP_TOKEN_MAX_LEN];
    const uint8_t *payload;
    uint8_t *data;
    uint16_t payload_len;
    uint8_t code;
    uint8_t type;
    uint8_t tkl;
    uint16_t id;
    int r;

    code = coap_header_get_code(request);
    type = coap_header_get_type(request);
    id = coap_header_get_id(request);
    tkl = coap_header_get_token(request, token);

    printk("*******");
    printk("type: %u code %u id %u", type, code, id);
    printk("*******");

    payload = coap_packet_get_payload(request, &payload_len);
    if (payload) {
        //net_hexdump("POST Payload", payload, payload_len);
    }

    if (type == COAP_TYPE_CON) {
        type = COAP_TYPE_ACK;
    } else {
        type = COAP_TYPE_NON_CON;
    }

    data = (uint8_t *)k_malloc(MAX_COAP_MSG_LEN);
    if (!data) {
        return -ENOMEM;
    }

    r = coap_packet_init(&response, data, MAX_COAP_MSG_LEN,
                         COAP_VERSION_1, type, tkl, token,
                         COAP_RESPONSE_CODE_CREATED, id);
    if (r < 0) {
        goto end;
    }

    for (p = location_path; *p; p++) {
        r = coap_packet_append_option(&response,
                                      COAP_OPTION_LOCATION_PATH,
                                      *p, strlen(*p));
        if (r < 0) {
            goto end;
        }
    }

    r = send_coap_reply(&response, addr, addr_len);

end:
    k_free(data);

    return r;
}

static const char * const test_path[] = { "test", NULL };

static struct coap_resource resources[] = {
    { .get = well_known_core_get,
      .path = COAP_WELL_KNOWN_CORE_PATH },
    { .path = test_path,
      .get = test_get,
      .put = test_put,
      .post = test_post },
    { }
};

static void
process_coap_request(uint8_t *data, uint16_t data_len,
                     struct sockaddr *client_addr,
                     socklen_t client_addr_len)
{
    struct coap_packet request;
    struct coap_pending *pending;
    struct coap_option options[16] = { 0 };
    uint8_t opt_num = 16U;
    uint8_t type;
    int r;

    r = coap_packet_parse(&request, data, data_len, options, opt_num);
    if (r < 0) {
        printk("Invalid data received (%d)\n", r);
        return;
    }

    type = coap_header_get_type(&request);

    pending = coap_pending_received(&request, pendings, NUM_PENDINGS);
    if (!pending) {
        goto not_found;
    }

    /* Clear CoAP pending request */
    if (type == COAP_TYPE_ACK || type == COAP_TYPE_RESET) {
        k_free(pending->data);
        coap_pending_clear(pending);

        if (type == COAP_TYPE_RESET) {
            //remove_observer(client_addr);
        }
    }

    return;

not_found:
    r = coap_handle_request(&request, resources, options, opt_num,
                            client_addr, client_addr_len);
    if (r < 0) {
        printk("No handler for such request (%d)\n", r);
    }
}

static int process_client_request(void)
{
    int received;
    struct sockaddr client_addr;
    socklen_t client_addr_len;
    uint8_t request[MAX_COAP_MSG_LEN];

    do {
        client_addr_len = sizeof(client_addr);
        //received = recvfrom(sock, request, sizeof(request), 0,
        //                    &client_addr, &client_addr_len);
        if (received < 0) {
            printk("Connection error %d", errno);
            return -errno;
        }

        process_coap_request(request, received, &client_addr,
                             client_addr_len);
    } while (true);

    return 0;
}

void
cr_uart_send(const struct device *tty, const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(tty, str[i]);
    }
}

void
uart_sink(const char *str)
{
    cr_uart_send(uart0, str);
}

#define RB_DEFAULT_INIT_SIZE 64u

struct resizeable_buffer {
    size_t index;
    size_t size;
    char *buffer;
};

struct resizeable_buffer ni_buffer;

void
rb_enlarge(struct resizeable_buffer *rb)
{
    if (rb->size == 0) {
        rb->size = RB_DEFAULT_INIT_SIZE;
    } else {
        rb->size += rb->size / 2;
    }

    if (rb->buffer == NULL) {
        rb->buffer = malloc(rb->size);
    } else {
        rb->buffer = realloc(rb->buffer, rb->size);
    }

    if (rb->buffer == NULL) {
        printk("Instrumentation is out of memory!\n");
        exit(0);
    }
}

void
rb_init(struct resizeable_buffer *rb)
{
    rb->index = 0u;
    rb->size = 0u;
    rb->buffer = NULL;
    rb_enlarge(rb);
}

void
ni_dispatch(struct sx_node *node)
{
    if (sx_is_list(node)) {
        if (sx_is_the_symbol(sx_car(node), "load-spi")) {
            struct sx_node *car = sx_pop(&node);
            sx_destroy(&car);
            if (sx_is_list(node))
                cr_spi_text_load(node);
            else
                sx_destroy(&node);
        }
    } else {
        sx_destroy(&node);
    }
}

void
ni_toplevel(struct resizeable_buffer *rb, const char ch)
{
    if (rb->index > rb->size) {
        rb_enlarge(rb);
    }

    if (ch == '\n') {
        rb->buffer[rb->index] = '\0';
        rb->index = 0u;
        struct sx_parse_result p = sx_parse_string(rb->buffer);
        if (p.status == SXS_SUCCESS) {
            printk("(instrumentation %s)\n", rb->buffer);
            ni_dispatch(p.node);
        } else {
            printk("(sx-error %d %zu)\n", p.status, p.position);
        }
        /* Acknowledge processing of instrumentation request. This ensures a
         * client can block until this happens to avoid race conditions. */
        cr_uart_send(uart1, "ok\n");
    } else {
        rb->buffer[rb->index] = ch;
        rb->index++;
    }
}

static int
cr_uart_read(void *driver, void *ch)
{
    int rc = uart_poll_in(driver, ch);
    return (rc == -1 ? -EAGAIN : rc);
}

void
main(void)
{
    printk("ChipRemoteFirmware running on %s\n", CONFIG_BOARD);

#ifdef CONFIG_SOC_POSIX
    printk("(activated!)\n");
    printk("(firmware-pid %u)\n", getpid());
    posix_flush_stdout();
#endif /* CONFIG_SOC_POSIX */

    uart0 = DEVICE_DT_GET(DT_NODELABEL(uart0));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }

    uart1 = DEVICE_DT_GET(DT_NODELABEL(uart1));
    if (uart1 == NULL) {
        printk("Could not access uart1. Giving up.\n");
        return;
    }

    struct resizeable_buffer nirb;
    rb_init(&nirb);

    /* Make an octet buffer */
    OctetBuffer cr_buffer;
    static uint8_t cr_buffer_memory[1024];
    octet_buffer_space(&cr_buffer, cr_buffer_memory, 1024);

    /* Make sink and connect it to octet buffer */
    Sink cr_sink;
    sink_to_buffer(&cr_sink, &cr_buffer);

    /* Make a source that is connected to uart0 */
    Source slip_source = OCTET_SOURCE_INIT(cr_uart_read, (void*)uart0);

    /* Make SLIP context */
    RFC1055Context slip;
    rfc1055_context_init(&slip, RFC1055_WITH_SOF);

    size_t frame_count = 0u;
    char ch1 = 0;
    for (;;) {
        const int rc0 = rfc1055_decode(&slip, &slip_source, &cr_sink);

        //printk("DEBUG: %d, %zu\n", rc0, cr_buffer.used);
        if (rc0 < 0) {
            if (rc0 != -EAGAIN) {
                printk("error: %s\n", strerror(-rc0));
            }
            if (rc0 == -ENOMEM) {
                printk("Discarding message.\n");
                octet_buffer_clear(&cr_buffer);
            }
        } else if (rc0 == 1) {
            /* Feed the thing to coap */
            printk("SLIP frame #%zu:\n", frame_count);
            hexdump_stdout(cr_buffer.data, cr_buffer.used, 0u);
            struct coap_packet request;
            struct coap_option options[16] = { 0 };
            uint8_t opt_num = 16u;
            const int rc = coap_packet_parse(&request,
                                             cr_buffer.data, cr_buffer.used,
                                             options, opt_num);
            printk("CoAP parse: %d (%s)\n", rc, strerror(rc < 0 ? -rc : rc));
            octet_buffer_clear(&cr_buffer);
            frame_count++;
        }

        const int rc1 = uart_poll_in(uart1, &ch1);

        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1);
        }

        if (rc1 != 0 && rc1 != 0) {
            k_usleep(1000);
        }
    }
}
