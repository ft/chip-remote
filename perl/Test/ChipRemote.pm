#!/usr/bin/perl

# Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
# Terms for redistribution and use can be found in LICENCE.

# This is a helper library to talk to a simulator instance of the firmware
# code. This is a tiny bit more convenient then cooking up here-docs on the
# command line.

package Test::ChipRemote;

use strict;
use warnings;
use diagnostics;
use English qw{ -no_match_vars };

use IO::Handle;
use IO::Select;
use IPC::Open2;
use Text::Diff;
use Term::ANSIColor;

use Exporter;
use base qw{ Exporter };
use vars qw{ @EXPORT };
@EXPORT = qw{ cr_request
              cr_request_io
              cr_run_script
              cr_line_trace_reply_in
              cr_line_trace_reply_out
              cr_line_trace_wait
              cr_test_set
              cr_test_title };

# The idea is to spawn a simulator instance and then play some requests to that
# process. In the process, collect the output from the simulation instance and
# diff it against a defined expected output.

my $comment;
my $max_multi_steps = 32;
my $simulator = q{./remote-fw/remote-fw.elf};
my $read_timeout = 1;
my $write_timeout = 1;
my $title = q{SET A TITLE!};
my $trace = 1;

my %setters = (
    max_multi_steps => sub { $max_multi_steps = $_[0] },
    read_timeout => sub { $read_timeout = $_[0] },
    simulator => sub { $simulator = $_[0] },
    trace => sub { $trace = $_[0] },
    write_timeout => sub { $write_timeout = $_[0] }
);

sub cr_test_set {
    my ($data) = @_;

    if (ref $data ne q{HASH}) {
        print "# cr_test_set: argument needs to be a hashref.\n";
        exit 42;
    }

    foreach my $key (sort keys %{ $data }) {
        if (defined $setters{$key}) {
            $setters{$key}->($data->{$key});
        } else {
            print "# cr_test_set: Unknown parameter `$key'.\n";
            exit 42;
        }
    }
}

sub expected_from_script {
    my ($script) = @_;
    my (@expected);

    push @expected, (q{>>> HI}, q{<<< Hi there, stranger.});
    foreach my $step (@{ $script }) {
        push @expected, q{>>> } . $step->{request};
        if ($step->{additional} > 0) {
            # This request defines additional lines, which are to be inserted
            # before any replies and prefixed by "-!- ".
            for my $m (@{ $step->{more} }) {
                push @expected, q{-!- } . $m;
            }
        }
        if ($step->{n_replies} == 1) {
            push @expected, q{<<< } . $step->{replies}->[0];
        } else {
            foreach my $reply (@{ $step->{replies} }) {
                push @expected, q{<<< } . $reply;
                push @expected, q{>>> MORE};
            }
            push @expected, q{<<< DONE};
        }
    }
    push @expected, (q{>>> BYE}, q{<<< Have a nice day.});
    return \@expected;
}

sub fail {
    my ($reason, $log) = @_;

    print qq{# $reason\n};
    print qq{# Current log:\n};
    foreach my $entry (@{ $log }) {
        print qq{# |$entry|\n};
    }
    print qq{# --- End-of-log ---\n};
    exit 1;
}

sub collect_replies {
    my ($handle, $log, $wantednumoflines) = @_;
    my (@fh, $nlcnt, $tmp);

    $nlcnt = 0;
    while ($nlcnt < $wantednumoflines) {
        @fh = $handle->{select}->can_read($read_timeout);
        unless ($#fh >= 0) {
            defined $tmp and chomp $tmp;
            push @{ $log }, qq{[read-buffer] "} .
                (defined $tmp ? $tmp : q{}) . q{"};
            fail("Read timeout (value: $read_timeout)", $log);
        }
        my $buf;
        sysread($handle->{output}, $buf, 1)
            or fail("Failed to read from simulator! Did it crash?\n", $log);
        $nlcnt++ if ($buf eq "\n");
        $tmp .= $buf if ($buf ne "\n" || $nlcnt < $wantednumoflines);
    }
    return split /\n/, $tmp;
}

sub transact {
    my ($request, $wantednumoflines, $handle, $log) = @_;
    my (@fh);

    @fh = $handle->{select}->can_write($write_timeout);
    unless ($#fh >= 0) {
        fail("Write timeout (value: $write_timeout)", $log);
    }
    syswrite $handle->{input}, $request . "\n";
    push @{ $log }, collect_replies($handle, $log, $wantednumoflines);
}

sub slurp_handle_to_log {
    my ($handle, $log) = @_;
    my (@fh, $reply, $buf);

    @fh = $handle->{select}->can_read(0.1);
    return if ($#fh < 0);
    $reply = q{};
    BYTE: while (sysread($handle->{output}, $buf, 1)) {
        $reply .= $buf;
        @fh = $handle->{select}->can_read(0.1);
        last BYTE if ($#fh < 0);
    }
    push @{ $log }, split(/\n/, $reply);
    return;
}

sub walk_script_with_program {
    my ($sim, $script) = @_;
    my ($pid, $handle, @log, $rc, $reply);

    $handle = {
        select => IO::Select->new(),
        input => IO::Handle->new(),
        output => IO::Handle->new()
    };
    $pid = open2($handle->{output},
                 $handle->{input},
                 $simulator, ());
    $handle->{select}->add($handle->{input});
    $handle->{select}->add($handle->{output});
    transact(q{HI}, 2, $handle, \@log);
    foreach my $step (@{ $script }) {
        transact($step->{request}, 2 + $step->{additional}, $handle, \@log);
        if ($step->{n_replies} > 1) {
            my $multi_steps = 0;
            do {
                transact(q{MORE}, 2, $handle, \@log);
                $multi_steps++;
                if ($multi_steps > $max_multi_steps) {
                    fail("Too many multiline steps ($max_multi_steps)", \@log);
                }
            } until ($log[$#log] eq '<<< DONE');
        }
    }
    transact(q{BYE}, 2, $handle, \@log);
    # Now eat up any remaining lines the simulator might output. If any, that
    # will definitely make the test fail later on.
    slurp_handle_to_log($handle, \@log);
    waitpid $pid, 0;
    $rc = $? >> 8;
    if ($rc != 0) {
        fail("Child process returned non-zero ($rc), indicating failure.",
             \@log);
    }
    if ($trace) {
        print qq{# --- IO trace: ---\n};
        foreach my $entry (@log) {
            my $copy = $entry;
            if ($copy =~ s,^<<<,,) {
                print q{# };
                print color q{green};
                print q{<<<};
            } elsif ($copy =~ s,^>>>,,) {
                print q{# };
                print color q{red};
                print q{>>>};
            } else {
                $copy =~ s,^...,,;
                print q{# };
                print color q{magenta};
                print q{-!-};
            }
            print qq{$copy};
            print color q{reset};
            print qq{\n};
        }
        print qq{# ---    ----   ---\n};
    }
    return \@log;
}

sub cr_print_comment {
    my ($c) = @_;

    if (defined $c) {
        print qq{ # $c\n};
    } else {
        print qq{\n};
    }
}

sub cr_run_script {
    my ($script) = @_;
    my ($expect, $output, @diff);

    $expect = expected_from_script($script);
    $output = walk_script_with_program($simulator, $script);
    @diff = split /^/, diff($output, $expect, { STYLE => "Table" });
    if ($#diff >= 0) {
        print qq{# --- Differences detected (actual vs expected): ---\n};
        foreach my $entry (@diff) {
            chomp $entry;
            print qq{# };
            my $isdiff = 0;
            $isdiff = 1 if ($entry =~ m/^\*/ || $entry =~ m/\*$/);
            print color 'red' if ($isdiff);
            print $entry;
            print color 'reset' if ($isdiff);
            print qq{\n};
        }
        print qq{not ok 1 - $title};
        cr_print_comment($comment);
    } else {
        print qq{ok 1 - $title};
        cr_print_comment($comment);
    }
}

sub cr_line_trace_reply {
    my ($line, $value, $direction) = @_;
    my $reply = q{};

    $reply .= $line->{port};
    $reply .= $direction;
    $reply .= sprintf q{[%04x]}, 1 << $line->{index};
    $reply .= q|{| . $line->{role} . q|}|;
    $reply .= q|(| . $value . q|)|;
}

sub cr_line_trace_reply_in {
    my ($line, $value) = @_;
    cr_line_trace_reply($line, $value, q{>});
}

sub cr_line_trace_reply_out {
    my ($line, $value) = @_;
    cr_line_trace_reply($line, $value, q{<});
}

sub cr_line_trace_wait {
    my ($time) = @_;

    return qq{time: $time};
}

sub cr_request {
    my ($request, @replies) = @_;

    return { request => $request,
             replies => \@replies,
             n_replies => ($#replies + 1),
             additional => 0 };
}

sub cr_request_io {
    my ($request, $reply, $line_trace) = @_;

    return { request => qq{$request},
             replies => [ $reply ],
             n_replies => 1,
             more => $line_trace,
             additional => $#{ $line_trace } + 1 };
}

sub cr_test_title {
    my ($xtitle, $xcomment) = @_;

    if (! -x $simulator || ! -e q{remote-fw/config.sim.h}) {
        print "1..0 # Skipped: Simulator not available.\n";
        exit 0;
    }
    print "1..1\n";
    $title = $xtitle;
    $comment = $xcomment;
}
