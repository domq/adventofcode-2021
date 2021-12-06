#!/usr/bin/perl

use warnings;
use strict;
use v5.21;
use autodie qw(:all);

our $WHOWINS = "squid";

sub load {
  my ($file) = @_;

  open(*BINGO, "<", "$file");

  our @numbers;
  our @boards;

  my $board = "";
  while(<BINGO>) {
    if (m/,/) {
      chomp;
      @numbers = split m/,/;
    } elsif (m/^$/) {
      if ($board) { push @boards, $board; }
      $board = "";
    } else {
      s/\b/ /g;
      $board .= $_;
    }
  }

  push @boards, $board;

  { numbers => \@numbers, boards => \@boards };
}

sub play {
  my ($game) = @_;
  return { state => "stalemate" } unless defined(
    my $drawn = shift @{$game->{numbers}});

  my %winning_boards;
  local $_;
  foreach (@{$game->{boards}}) {
    s| (${drawn})\b|$1<|;
    if (defined(my $score = won($_))) {
      if ($WHOWINS eq "submarine") {
        return { state => "submarine-won", score => $score * $drawn };
      } else {
        $winning_boards{$_} = $score;
      }
    }
  }

  if (%winning_boards) {
    $game->{boards} = [ grep { ! $winning_boards{$_} } @{$game->{boards}} ];
    if (! @{$game->{boards}}) {
      # Last board won; we know how to let the squid win
      my (@scores) = values %winning_boards;
      return { state => "squid-won", score => $scores[0] * $drawn };
    }
  }

  return { state => "playing" };
}

sub matrix_walk {
  my ($walker, @numbers) = @_;
  my $matrix = [];
  for($a = 0; $a < sqrt(@numbers); $a++) {
    push @$matrix, my $row = [];
    for($b = 0; $b < sqrt(@numbers); $b++) {
      push @$row, $walker->();
    }
  }
  return $matrix;
}

sub matrix_dump {
  my ($matrix) = @_;
  foreach my $row (@$matrix) {
    print join(" ", map { " " x (5 - length($_)) . $_; } @$row); print "\n";
  }
}

sub won {
  local $_ = shift;
  my (@numbers) = m/\s*(\d+<?)\s*/gsm;
  my $order = int(sqrt(@numbers));
  die unless $order ** 2 == @numbers;

  foreach my $matrix (matrix_walk(sub {$numbers[$a * $order + $b]}, @numbers),
                      matrix_walk(sub {$numbers[$b * $order + $a]}, @numbers)) {
    foreach my $row (@$matrix) {
      my @checked = map { m/(\d+)</? ($1) : () } @$row;
      if (@checked == $order) {
        return score(@numbers);
      }
    }
  }
  return undef;
}

sub score {
  my (@remaining) = grep { ! m|<| } @_;
  local $_;
  my $sum = 0;
  $sum += $_ for @remaining;
  return $sum;
}

###################################################
# use Data::Dumper; warn Dumper(load "testdata");

my $game = load "input";

my $state;
do {
  $state = play($game);
} until ($state->{state} ne "playing");

use Data::Dumper; say Dumper $state;
