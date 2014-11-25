#!/usr/bin/perl
use Getopt::Long;

#
# Turn off auto abbreviations, so we can control them if we want to.
#
Getopt::Long::Configure("no_auto_abbrev");

#
# Options can be specified here. "exec|e" allows command-line args of
# the form -e, -exec, --exec, and --e.
#
# Current options are:
# --help         Print a usage message
#
GetOptions("help|?"=>\$help_flag);

#
# Check to see if the user wants a usage message -- if so, print one and exit
#
if ($help_flag) {
  usage();
  exit;
}

#
# Grab back the name of the program file -- this is needed for all cases
# except --help, which is handled above (and which doesn't check)
#
if ($ARGV[0]) {
  $c_prog = $ARGV[0];
} else {
  print STDERR "Error: A C program file in Maude-ready format must be specified on the command line.\nUse --help or -? for usage.\n";
  exit;
}

#
# Verify the specified program exists.
#
if (! (-e $c_prog.".cil")) {
  print STDERR "Source file $c_prog.cil not found\n";
  exit;
}

#
# Open the file; we will process it line by line, looking for verification
# tasks and associated information.
#
open CSOURCE, "< $c_prog.cil" or die "Cannot open file $c_prog.cil\n";

#
# Open the destination file name. This will be the actual verification
# tasks in Maude-ready form. Open it up front, so we can die early
# if for some reason we cannot do it.
#
open CDEST, "> $c_prog.maude" or die "Cannot open output file $c_prog.maude\n";

#
# Set up the list we will use to track structure definitions
#
@struct_defs = ();

#
# Track what type of info we are processing.
#
$struct_flag = 0;
$task_flag = 0;

#
# Buffers for building Maude code
#
$outbuf = "";
$struct_list = "";

#
# Now, process the source file. In this first pass, we just gather
# definitions we need when generating the verification tasks, but
# we don't actually do anything with the tasks at this point.
#
print STDOUT "Reading structure definitions\n";

while ($line = <CSOURCE>) {
  chomp($line);
  #print STDOUT "Read line: $line\n";
  if ($struct_flag == 0 && $task_flag == 0) {
    if ($line =~ m/^([^\@]*)\@\@structBEGIN(.*)$/i) {
      #print STDOUT "Found structure begin: $2\n";
      $struct_flag = 1;
      push @struct_defs, $2
    } elsif ($line =~ m/^([^@]*)\@\@taskBEGIN\@\@([^@]*)\@\@(.*)$/i) {
      #print STDOUT "Found task begin: $2\n";
      $task_flag = 1;
    }
  } elsif ($struct_flag == 1) {
    if ($line =~ m/^([^\@]*)\@\@structEND(.*)$/i) {
      #print STDOUT "Found struct end: $1\n";
      $outbuf = pop @struct_defs;
      $outbuf = $outbuf . " " . $1;
      push @struct_defs, $outbuf;
      $struct_flag = 0;
    } else {
      $outbuf = pop @struct_defs;
      $outbuf = $outbuf . " " . $line;
      push @struct_defs, $outbuf;
    }
  } elsif ($task_flag == 1) {
    if ($line =~ m/^([^\@]*)\@\@taskEND(.*)$/i) {
      #print STDOUT "Found task end: $1\n";
      #$outbuf = pop @verify_tasks;
      #$outbuf = $outbuf . " " . $1;
      $task_flag = 0;
    }
  }
}

print STDOUT "Finished reading structure definitions\n";

#
# Done with first pass
#
close CSOURCE;

#
# Transform structure definitions into a Maude-formatted
# list, for use in the verification tasks.
#
&toMaudeList;

#
# Re-open and start at the beginning
#
open CSOURCE, "< $c_prog.cil" or die "Cannot open file $c_prog.cil\n";

print STDOUT "Generating verification tasks\n";

while ($line = <CSOURCE>) {
  chomp($line);
  if ($struct_flag == 0 && $task_flag == 0) {
    if ($line =~ m/^([^\@]*)\@\@structBEGIN(.*)$/i) {
      $struct_flag = 1;
    } elsif ($line =~ m/^([^@]*)\@\@taskBEGIN\@\@([^@]*)\@\@(.*)$/i) {
      $task_flag = 1;
      print STDOUT "Generating task for " . $2 . "\n";
      print CDEST  "red check ( \"" . $2 . "\", " . $3 . " ";
    }
  } elsif ($struct_flag == 1) {
    if ($line =~ m/^([^\@]*)\@\@structEND(.*)$/i) {
      $struct_flag = 0;
    } 
  } elsif ($task_flag == 1) {
    if ($line =~ m/^([^\@]*)\@\@taskEND(.*)$/i) {
      #print STDOUT "Found task end: $1\n";
      print CDEST $1 . " , " . $struct_list . " ) .\n\n";
      $task_flag = 0;
      print STDOUT "Task generated.\n";
    } else {
      print CDEST $line . " ";
    }
  }
}

close CSOURCE;

#print STDOUT "Preparing to print structure definitions:\n";

#foreach $struct (@struct_defs) {
#  print STDOUT "$struct\n";
#}

#print STDOUT "Preparing to print task definitions:\n";

#foreach $task (@verify_tasks) {
  #print STDOUT "$task\n";
#  print CDEST  "red check ( \"" . $task->[0] . "\", " . $task->[1] . ", " . $struct_list . " ) .\n\n";
#}

close CDEST;

# Perl trim function to remove whitespace from the start and end of the string
sub trim($) {
  my $string = shift;
  $string =~ s/^\s+//;
  $string =~ s/\s+$//;
  return $string;
}
# Left trim function to remove leading whitespace
sub ltrim($) {
  my $string = shift;
  $string =~ s/^\s+//;
  return $string;
}
# Right trim function to remove trailing whitespace
sub rtrim($) {
  my $string = shift;
  $string =~ s/\s+$//;
  return $string;
}

#
# Take contents of a Perl list and turn them into a Maude list
# with the given terminator.
#
sub toMaudeList {
  $item_count = 0;
  $struct_list = "";

  foreach $struct (@struct_defs) {
    $item_count++;
    $struct_list = $struct_list . "__(" . $struct . ",";
  }
  $struct_list = $struct_list . "empty";
  for ($i = 0; $i < $item_count; $i++) {
    $struct_list = $struct_list . ")";
  }
}

#
# Usage message 
#
sub usage()
  {
    print STDERR << "EOF";

verify.pl prepares verification units from the output of the CIL parser.

usage: $0 [flags] prog

flags include:
--help                display usage info (which you are now reading) and exit

short forms include:
-?       =        --help

EOF
}
