#ifndef INC_PROTO_UTILS_H
#define INC_PROTO_UTILS_H

void tx_init(void);
void tx_add_n(char *, size_t);
void tx_add(char *);
void tx_add_space(void);
void tx_add_integer(uint32_t);
void tx_trigger(void);

#endif /* INC_PROTO_UTILS_H */