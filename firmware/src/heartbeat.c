/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file heartbeat.c
 * @brief Heartbeat thread for chip-remote boards.
 *
 * If the "chipremote,heartbeat" node is set in the "chosen" subtree of the
 * devicetree of a target board, this file is compiled using that node as the
 * GPIO device to heartbeat blink that LED.
 *
 * If this file is not compiled, the thread does not exist.
 */

#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>

#include <ufw/compiler.h>

#define HEARTBEAT_NODE DT_CHOSEN(chipremote_heartbeat)
static const struct gpio_dt_spec hb = GPIO_DT_SPEC_GET(HEARTBEAT_NODE, gpios);

static void
cr_heartbeat(UNUSED void *p1, UNUSED void *p2, UNUSED void *p3)
{
    if (device_is_ready(hb.port) == false) {
        printk("Could not access heartbeat LED.\n");
        return;
    }

    int ret = gpio_pin_configure_dt(&hb, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        printk("Could not configure LED pin.\n");
        return;
    }

    enum { ON, OFF } state = OFF;
    const unsigned int offtimes[] = { 200, 700 };
    const unsigned int ontime = 100;
    unsigned int n = 0u;

    for (;;) {
        switch (state) {
        case OFF:
            k_msleep(offtimes[n & 1u]);
            gpio_pin_set_dt(&hb, 0);
            state = ON;
            n++;
            break;
        case ON:
            k_msleep(ontime);
            gpio_pin_set_dt(&hb, 1);
            state = OFF;
            break;
        }
    }
}

#define CR_HEARTBEAT_STACKSIZE 128
#define CR_HEARTBEAT_PRIORITY   12

K_THREAD_DEFINE(cr_headerbeat_thread,
                CR_HEARTBEAT_STACKSIZE,
                cr_heartbeat, NULL, NULL, NULL,
                CR_HEARTBEAT_PRIORITY, 0, 0);
