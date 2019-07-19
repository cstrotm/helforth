#! /usr/bin/perl

use strict;

while (<>) {
  s/&/&amp;/g;
  s/</&lt;/g;
  s/>/&gt;/g;
  print $_;
}
