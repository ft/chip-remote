/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>

#include <ufw/compiler.h>

#define LED0_NODE DT_ALIAS(led0)
static const struct gpio_dt_spec led0 = GPIO_DT_SPEC_GET(LED0_NODE, gpios);

static void
led0_heartbeat(UNUSED void *p1, UNUSED void *p2, UNUSED void *p3)
{
    if (device_is_ready(led0.port) == false) {
        printk("Could not access heartbeat LED.\n");
        return;
    }

    int ret = gpio_pin_configure_dt(&led0, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        printk("Could not configure LED pin.\n");
        return;
    }

    enum { ON, OFF } state = OFF;
    const unsigned int offtimes[] = { 100, 500 };
    const unsigned int ontime = 100;
    unsigned int n = 0u;

    for (;;) {
        switch (state) {
        case OFF:
            k_msleep(offtimes[n & 1u]);
            gpio_pin_set_dt(&led0, 0);
            state = ON;
            n++;
            break;
        case ON:
            k_msleep(ontime);
            gpio_pin_set_dt(&led0, 1);
            state = OFF;
            break;
        }
    }
}

#define LED0_HEARTBEAT_STACKSIZE 128
#define LED0_HEARTBEAT_PRIORITY   12

K_THREAD_DEFINE(led0_headerbeat_thread,
                LED0_HEARTBEAT_STACKSIZE,
                led0_heartbeat, NULL, NULL, NULL,
                LED0_HEARTBEAT_PRIORITY, 0, 0);
