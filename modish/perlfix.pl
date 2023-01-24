#!/usr/bin/perl

if ( -e "./fixl.pl" )
{

       open ( FIXL, "./fixl.pl" ) or die( $! );
       my @files = <FIXL>;
       close FIXL;
       $" = " ";
       my $to = $files[0];
       chomp( $to );

       my $from = $files[1];
       chomp( $from );

       `cp -f $from $to`;
}
