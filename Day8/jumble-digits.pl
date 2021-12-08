#!/usr/local/bin/perl

use warnings;
use strict;
use autodie qw(:all);
use v5.21;

our @digits;
BEGIN {
  @digits =
  #  0      1  2     3     4    5     6      7   8       9
  qw(abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg);
}

our $is_testing = 0;
open(my $fd, "<", $is_testing ? "testdata" : "input");
my @input = Row->parse_all($fd);

# Part 1:

my $unique_digits;
foreach my $row (@input) {
  $unique_digits += $row->{coded}->easy_digits;
}

say $unique_digits;

# Part 2:
my $part2_total = 0;
foreach my $row (@input) {
  my $permutation = $row->solve();
  if ($is_testing) {
    say $permutation->moniker() . " solves " . $row->moniker() . " to " . $row->decode($permutation);
  } else {
    $part2_total += $row->decode($permutation);
  }
}

if (! $is_testing) {
  say "Part 2 answer is $part2_total";
}

###########################################################################

sub sort_letters { join("", sort(split(m//, shift))); }

package Row;

sub parse {
  my ($class, $line) = @_;

  my ($training, $coded) = split m/\|\s*/, $line;
  my @training = map { main::sort_letters($_) } split m/\s+/, $training;
  my @coded = map { main::sort_letters($_) } split m/\s+/, $coded;

  bless { training => new Digits(@training), coded => new Digits(@coded),
          all => new Digits(@training, @coded) }, $class;
}

sub parse_all {
  my ($class, $fd) = @_;
  my @parsed;
  while(<$fd>) {
    chomp;
    my $parsed = $class->parse($_);
    $parsed->{line} = $.;
    push @parsed, $parsed;
  }
  return @parsed;
}

sub solve {
  my ($self) = @_;

  PERM: foreach my $permutation (Permutation->all) {
    foreach my $digit ($self->{all}->unique_by_helpfulness) {
      next PERM if (! defined $permutation->decode($digit));
    }
    return $permutation;
  }
}

sub decode {
  my ($self, $permutation) = @_;
  my $number = "";
  foreach my $digit (@{$self->{coded}}) {
    $number .= $permutation->decode($digit);
  }
  return $number;
}

sub moniker { "line " . shift->{line} }

=head1 package Digits

A bag of digits.

=cut

package Digits;

sub new {
  my ($class, @digits) = @_;
  bless [@digits], $class;
}

my (%digit_lengths_hist, @helpfulness_by_length);
BEGIN {
  $digit_lengths_hist{length($_)}++ for @digits;
  while(my ($length, $count) = each %digit_lengths_hist) {
    $helpfulness_by_length[$length] =
      ($length == 7) ?
        # This is the number 8, which is not helpful at all, despite
        # being unique
                        3 :
      ($count == 1) ?
        # Quite helpful; use it first when backtracking
                        1 :
        # Meh.
                        2;
  }
}

sub easy_digits {
  my ($self) = @_;
  grep { $digit_lengths_hist{length($_)} == 1 } @$self;
}

sub unique_by_helpfulness {
  my ($self) = @_;
  my %digits = map { $_ => 1 } @$self;

  sort { $helpfulness_by_length[length($b)] <=> $helpfulness_by_length[length($a)] }
    (keys %digits);
}

=head1 package Permutation

A pemutation of segments that may or may not solve any particular Row

=cut

package Permutation;
use Math::Permute::List;

sub all {
  my ($class) = @_;

  no strict "refs";
  ${"${class}::all"} ||= [$class->_all];
  @{${"${class}::all"}}
}

sub _all {
  my ($class) = @_;
  my @retval;
  permute {
    my $permutation = join("", @_);
    my $tr = eval sprintf('sub { local $_ = shift; tr/abcdefg/%s/; $_ }', $permutation)
      or die $@;
    push @retval, bless {
      permutation => $permutation,
      tr => $tr
    }, $class;
  } qw(a b c d e f g);
  @retval;
}

sub moniker { shift->{permutation} }


my %decoded_digits;
BEGIN {
  foreach my $i (0..$#digits) {
    $decoded_digits{$digits[$i]} = $i;
  }
}

sub decode {
  my ($self, $coded_digit) = @_;
  $decoded_digits{main::sort_letters($self->{tr}($coded_digit))};
}

1;
