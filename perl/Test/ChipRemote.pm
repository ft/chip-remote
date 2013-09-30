#!/usr/bin/perl

# Copyright (c) 2013 chip-remote workers, All rights reserved.
# Terms for redistribution and use can be found in doc/LICENCE.

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
              cr_run_script
              cr_test_set
              cr_test_title };

# The idea is to spawn a simulator instance and then play some requests to that
# process. In the process, collect the output from the simulation instance and
# diff it against a defined expected output.

my $simulator = q{./remote-fw/remote-fw.elf};
my $read_timeout = 1;
my $write_timeout = 1;
my $title = q{SET A TITLE!};
my $trace = 1;

my %setters = (
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
        print qq{#  |$entry|\n};
    }
    print qq{# --- End-of-log ---\n};
    exit 1;
}

sub collect_replies {
    my ($handle, $log) = @_;
    my (@fh, $nlcnt, $tmp);

    $nlcnt = 0;
    while ($nlcnt < 2) {
        @fh = $handle->{select}->can_read($read_timeout);
        unless ($#fh >= 0) {
            chomp $tmp;
            push @{ $log }, qq{[read-buffer] "$tmp"};
            fail("Read timeout (value: $read_timeout)", $log);
        }
        my $buf;
        sysread $handle->{output}, $buf, 1;
        $nlcnt++ if ($buf eq "\n");
        $tmp .= $buf if ($buf ne "\n" || $nlcnt < 2);
    }
    return split /\n/, $tmp;
}

sub transact {
    my ($request, $handle, $log) = @_;
    my (@fh);

    @fh = $handle->{select}->can_write($write_timeout);
    unless ($#fh >= 0) {
        fail("Write timeout (value: $write_timeout)", $log);
    }
    syswrite $handle->{input}, $request . "\n";
    push @{ $log }, collect_replies($handle, $log);
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
    transact(q{HI}, $handle, \@log);
    foreach my $step (@{ $script }) {
        transact($step->{request}, $handle, \@log);
        if ($step->{n_replies} > 1) {
            do {
                transact(q{MORE}, $handle, \@log);
            } until ($log[$#log] eq '<<< DONE');
        }
    }
    transact(q{BYE}, $handle, \@log);
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
            } else {
                $copy =~ s,^...,,;
                print q{# };
                print color q{red};
                print q{>>>};
            }
            print qq{$copy};
            print color q{reset};
            print qq{\n};
        }
        print qq{# ---    ----   ---\n};
    }
    return \@log;
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
            print qq{#   };
            if ($entry =~ m/^\*/) {
                print color 'red';
            }
            print $entry;
            if ($entry =~ m/^\*/) {
                print color 'reset';
            }
            print qq{\n};
        }
        print qq{nok 1 - $title\n};
    } else {
        print qq{ok 1 - $title\n};
    }
}

sub cr_request {
    my ($request, @replies) = @_;

    return { request => $request,
             replies => \@replies,
             n_replies => ($#replies + 1) };
}

sub cr_test_title {
    my ($t) = @_;

    if (! -x $simulator || ! -e q{remote-fw/config.sim.h}) {
        print "1..0 # Skipped: Simulator not available.\n";
        exit 0;
    }
    print "1..1\n";
    $title = $t;
}
