#! /usr/bin/perl

use strict;

while (<>) {
  s/</|AMP|lt;/g;
  s|//|/<b></b>/|g;
  print $_;
}
