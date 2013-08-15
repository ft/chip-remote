#ifndef INC_REQUESTS_H
#define INC_REQUESTS_H

int cr_handle_features(int, struct cr_words *);
int cr_handle_lines(int, struct cr_words *);
int cr_handle_modes(int, struct cr_words *);
int cr_handle_port(int, struct cr_words *);
int cr_handle_ports(int, struct cr_words *);

int cr_handle_bye(int, struct cr_words *);
int cr_handle_focus(int, struct cr_words *);
int cr_handle_hi(int, struct cr_words *);
int cr_handle_version(int, struct cr_words *);

#endif /* INC_REQUESTS_H */
