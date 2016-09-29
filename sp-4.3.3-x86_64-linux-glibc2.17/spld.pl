#!/usr/bin/perl -w
# -*- Mode:perl; buffer-read-only:t -*-
# [PM] 3.9.1b4 **Note**: InstallSICStus.in needs to know about all AC_SUBST variables in this file

my $opt_config_default="/home/amelo/Documents/SICStus/bin/spconfig-4.3.3"; # [PM] 3.8.5 sprm 1678

#!@PERL@ -w
# -*- Mode:perl -*-
# ** NOTE ** @VAR@ are substituted by Utils/Makefile (by configure only for binary installs)
#################################################################################
# Description:    Perl source-code for spld. "spld --help" for usage info.
#################################################################################
# [PM] 3.9 Lines starting with "## PLFRM " have been (automatically) commented out for this platform.
# [PM] 3.9 lines containing <hash> @ONLYWIN32@ will be removed on non-windows
# [PM] 3.9 lines containing <hash> @ONLYNONWIN32@ will be removed on windows
# [PM] 4.2 lines containing <hash> ... @ONLYNONMACOSX@ will be removed on Mac OS X (note that this need not come immediately after the hash mark)

# [PM] 4.0.2+ strict and warnings. If you turn this off consider the
# immortal words of Dirty Harry: "... you've got to ask yourself one
# question: Do I feel lucky? Well, do ya, punk?"
use strict;
use warnings;
# [PM] 4.0.2 for some reason diagnostics goes berserk when using PerlApp (ONLYNONWIN32 really means not-using-perlapp)
# [PM] 4.2 diagnostics does not work on Mac OS X 10.6.6. unless XCode (and its docs?) is installed (SPRM 12049)
use diagnostics;                # @ONLYNONWIN32@ @ONLYNONMACOSX@

use Getopt::Long;               # [PM] 4.0.2+

# [PM] A copy of the opt_exechome set-up is in splfr.pl.in. Keep in synch!
############################
## BEGIN opt_exechome set-up

# [PM] 3.9.1
use FindBin;

use File::Spec;

# [PM] On Win32 this should work at least as well as the old code (since $0 is absolute in PerlApp)
#      On UNIX this should work whenever FindBin works, i.e., unless
#      user invokes this script in some funny way.

my $opt_exechome = $FindBin::Bin;
## [PM] 3.9.2b2 SPRM 3631
##            We need short path so that $SICSTUS need not be quoted.
##            This in turn is because of the totally brain damaged way
##            that cmd.exe (and thus the perl builtin system())
##            treats double quotes
## PLFRM $opt_exechome = Win32::GetShortPathName($opt_exechome);                               

## END opt_exechome set-up
##########################

# Also in splfr.pl.in. Keep in synch!
# [PM] 3.9 The following line (the magic marker) marks the place where
# the spld.config variables should be inserted. Only variables
# actually used are inserted and they all get an initial value "". The
# reason for this is to avoid warning about spld.config variables that
# only occur once in this script. This also makes any remaining
# varnings more likely to be significant.
# @ [PM] 4.0.2+ no longer used MAGIC_SPLD_CONFIG_MARKER@

#
# Much code in here is copied to splfr.pl.in, Keep in sync!
#

#### START of option variables
## Keep option variables common with splfr.pl.in first. Keep in sync!

my $opt_help=0;
my $opt_verbose=0;
my $opt_version=0;
my $opt_output="a.out";		# Any better idea?
my @cflags = ();
my $opt_config="";
my $opt_sicstus="";
my $opt_moveable=-1;            # [PM] 4.2 -1 means it was not supplied on the command line
my $opt_no_moveable=-1;         # [PM] 4.2 -1 means it was not supplied on the command line
my $opt_keep=0;
my $opt_static=0;
my $opt_nocompile=0;               # [PM] do not compile/link just generate code
my $opt_embed_manifest=0;          # [PM] 4.0 embed VC8 manifest
my $opt_no_embed_manifest=0;       # [PM] 4.0 embed VC8 manifest
my $opt_multi_sp_aware=0;          # [PM] 3.9 compile with -DMULTI_SP_AWARE
my $opt_namebase="";               # [PM] 3.9

# my $opt_locale=-1;                 # [PM] 4.3 set process locale from environment (SPRM 12206). -1 means not supplied on command line.

my $opt_no_locale=-1;              # [PM] 4.3 Do not set process locale from environment (SPRM 12206). -1 means not supplied on command line. --no-locale
my $opt_locale_name="";            # [PM] 4.3 process locale ("" means from environment) (SPRM 12206). --locale=NAME

my $opt_with_jdk="";
my $opt_with_tcltk="";
my $opt_with_tcl="";
my $opt_with_tk="";
my $opt_with_bdb="";

my $opt_window;

## [PM] 3.9b5 *not* in spld_prefix.pl. The platform of a binary
## installation is decided at build-time (at SICS) not at install-time
## (at customer).
my $opt_platform="x86_64-linux-glibc2.17";
my $config_file_basename="spconfig-4.3.3";
my $release_year="2016"; # [PM] 3.11.2
my $USE_RUNTIME_LICENSE="yes"; # "yes" or "no"
my $USE_RUNTIME_LICENSE_SPRT="no"; # "yes" or "no"
my $USE_RUNTIME_LICENSE_SPRE="yes"; # "yes" or "no"
my $use_runtime_license=0;
my $SICSTUS_VERSION="40303";

# my $opt_LD=0;                   # should be removed when $use_getopt is permanented

## things not in common with splfr.pl.in

my $opt_main="default";
my $opt_system_type=0;
my $opt_resources="";
my @opt_resources = ();

my $opt_more_memory=0;             # [PM] 3.10 apply platform specific tricks to get more usable memory for SICStus.
my $opt_no_more_memory=1;          # [PM] 4.0.2+ more-memory is a 3.12 thing, always disable in SP4
my $opt_linux_linker_script=0;     # [PM] 3.10 modify linker script to free up lower 256MB (What --more-memory does for linux)

my $opt_wrapper=-1;                 # [PM] -1 means that it was not supplied on the command line
my $opt_no_wrapper=0;
my $opt_embed_sav_file=0;          # [PM] 3.11.2 embed supplied sav file(s) as data resources.
my $opt_no_embed_sav_file=0;
my $opt_resources_from_sav=0;      # [PM] 3.10 extract resource name from (embedded) .sav-file meta data.
my $opt_no_resources_from_sav=0;   # [PM] 3.10 
my $opt_respath;


my $opt_nortable=0;
my $opt_interactive=0;
my $opt_lang="";
my $opt_memhook="";
my $memhook_name="";            # [PM] 4.0.5 set from --opt_memhook.
my $opt_userhook=0;
my $opt_dontlink=0;
my $opt_extended_rt=0;
my $opt_development=0;
# [PM] 4.2 Allow (extended) runtime system to boot as devsys if SP_USE_DEVSYS is set in environment
# True by default but user may want to turn it off for security etc.
my $opt_allow_devsys=1;
my $opt_shared=0;                  # [PM] 3.9b5
my $opt_jnilib=0;                  # [PM] 3.9.1
my $opt_no_sprt=0;                 # [PM] 3.9 do not link with sprt (static/dynamic/implib). For multi SP
my $opt_embed_rt_sav=0;
my $opt_no_embed_rt_sav=0;
my $opt_cc_data_res=0;             # [PM] 3.9b2 Use cc to compile data resource (reset from $configs{"RESGEN_TYPE"} below)
                                # The default should be 0, if 0 then $configs{"RESGEN_TYPE"} from spld.config is used
my $opt_embed_license=1;        # [PM] 4.0.5 embed runtime license (default true but ignored if !$use_runtime_license)
my $opt_license_file="";        # [PM] 4.0.5 Read license from here instead of default
my $opt_no_embed_license=0;

# [PM] not needed. hack the config file instead
# $opt_no_win32_special=0;


#### END of option variables


my @linkfiles = ();		# files to send to the linker
my @ldflags = ();			# Flags to send to the linker
my @ldrpath = ();                  # [PM] 3.9.0 rpath elements but NOT -rpath/-R<PATH>, see @ldrpath
my @prologfiles = ();		# Prolog code

my @csrcfiles = ();             # C/C++-src to compile
my @savfiles = ();
my @autoobj = ();               # Object-code
my @main_code = ();		# object code for main program

my @tmpfiles = ();              # Temporary files to delete when done


#### START Option processing. Much copied to splfr.pl.in  Keep in sync!

my %config;                     # [PM] 4.0.2+ contents of config file

if (@ARGV <= 0) {
  print STDERR "! no arguments.";
  usage();
  exit 1;
}

# my $use_getopt = 1;
#
# # [PM] 4.0.2+ make it possible to fall back to old behavior
# if ($ARGV[0] eq "--old-getargs") {
#   shift @ARGV;
#   $use_getopt = 0;
# }

### [PM] 4.0.2+ new option processing
my %configs;

# if ($use_getopt)
{

Getopt::Long::Configure("ignore_case",
                        "auto_abbrev",
                        "no_getopt_compat", # do not allow '+' to start options
                        "permute", # allow options to be mixed with other command line arguments
                        "pass_through", # leave unrecognized options-like arguments on @ARGV
                        "no_ignore_case",
                        # auto_version, auto_help does not seem to be supported in the Perl we use on Win32
                        # "no_auto_version", # do not handle --version specially
                        # "no_auto_help", # do not handle --help specially
                        "prefix_pattern=--|-"
                        # , "debug"
                       );
my @nonoptions = ();
my %opt_conf;

# FIXME: beware of old option processing that used previously set option values (e.g. objext).

GetOptions("<>" => sub { push @nonoptions, @_ }, # Why does not $_ work?
## Keep options common with splfr.pl.in first. Keep in synch.
           "help|?" => \$opt_help,
           "verbose|v+" => \$opt_verbose, # multiple --verbose accumulates
           "vv" => sub { $opt_verbose = 2 }, # no need to use $opt_vv
           "version" => \$opt_version,

# FIXME: Use '!' argument specification for no{-} -processing?
           "multi-sp-aware" => \$opt_multi_sp_aware,
           "output|o=s" => \$opt_output,
           "namebase=s" => \$opt_namebase,
# FIXME: Use '!' argument specification for no{-} -processing?
           "nocompile" => \$opt_nocompile,
           "static|S" => \$opt_static,
           "sicstus=s" => \$opt_sicstus,
# FIXME: Use '!' argument specification for no{-} -processing?
           "moveable" => \$opt_moveable,
           "no-moveable" => \$opt_no_moveable,

           "cflag=s" => \@cflags,
           "config=s" => \$opt_config,
           "keep" => \$opt_keep,
           "exechome=s" => \$opt_exechome,
           "LD" => sub { die("!FINISH") }, # !FINISH is handled specially (stop option processing, like '--'.)
           "conf=s" => \%opt_conf,  # [PM] 4.0.2+ override config entries

           "with-jdk=s" => \$opt_with_jdk,
           "with-tcl=s" => \$opt_with_tcl,          # not documented. Remove it?
           "with-tk=s" => \$opt_with_tk,          # not documented. Remove it?
           "with-tcltk=s" => \$opt_with_tcltk,
           "with-bdb=s" => \$opt_with_bdb,

## Options specific to spld.pl.in

           # [PM] 4.0.2+ gone: "debugprint" => sub { $opt_verbose = 3 },

           "main=s" => \$opt_main,
           "development|D" => \$opt_development,
           "extended-rt|E" => \$opt_extended_rt,
           "allow-devsys!" => \$opt_allow_devsys, # [PM] 4.2 

           "shared" => \$opt_shared,
           "jnilib" => \$opt_jnilib,
           "resources=s" => \@opt_resources,
           "respath=s" => \$opt_respath,
           "window" => \$opt_window,
           "cc-data-res" => \$opt_cc_data_res,

# FIXME: Use '!' argument specification for no{-} -processing?
           "nortable" => \$opt_nortable,

# FIXME: Use '!' argument specification for no{-} -processing?
           "embed-rt-sav" => \$opt_embed_rt_sav,
           "no-embed-rt-sav" => \$opt_no_embed_rt_sav,

# FIXME: Use '!' argument specification for no{-} -processing?
           "embed-sav-file" => \$opt_embed_sav_file,    # [PM] 3.11.2
           "no-embed-sav-file" => \$opt_no_embed_sav_file, # [PM] 3.11.2

# FIXME: Use '!' argument specification for no{-} -processing?
           "embed-manifest" => \$opt_embed_manifest,    # [PM] 4.0
           "no-embed-manifest" => \$opt_no_embed_manifest, # [PM] 4.0

# FIXME: Use '!' argument specification for no{-} -processing?
           "embed-licence" => \$opt_embed_license,
           "embed-license" => \$opt_embed_license, # s/c spelling
           "no-embed-licence" => \$opt_no_embed_license,
           "no-embed-license" => \$opt_no_embed_license, # s/c spelling
           "licence-file=s" => \$opt_license_file,
           "license-file=s" => \$opt_license_file, # s/c spelling

           "interactive|i" => \$opt_interactive,

# FIXME: no longer allowed?
           "lang=s" => \$opt_lang, # --lang= iso

# FIXME: make this a no-opt ignored for backard compatibility?
# FIXME: Use '!' argument specification for no{-} -processing?
           "more-memory" => \$opt_more_memory,
           "no-more-memory" => \$opt_no_more_memory,

           "no-locale" => \$opt_no_locale,
           "locale=s" => \$opt_locale_name,

           "memhook=s" => \$opt_memhook,         # --memhook= default|malloc (was sbrk|default|malloc|sparse)
           "userhook" => \$opt_userhook,

# FIXME: Use '!' argument specification for no{-} -processing?
           "no-sprt" => \$opt_no_sprt,

# FIXME: Use '!' argument specification for no{-} -processing?
           "wrapper" => \$opt_wrapper, # [PM] 3.9.1 --wrapper generate wrapper even for platforms that does not need it
           "no-wrapper" => \$opt_no_wrapper, # [PM] 3.9.1 disable wrapper for platforms where --wrapper is the default

# FIXME: Use '!' argument specification for no{-} -processing?
           "resources-from-sav" => \$opt_resources_from_sav,
           "no-resources-from-sav" => \$opt_no_resources_from_sav

          ) ||
  ( usage(), exit 1 );

&verbose("\@ARGV is (" . join("|", @ARGV) . ")\n");

## Some early options

# [PM] 3.11.3 SPRM 8062 FindBin.pm 1.44 returns path with terminating slash
$opt_exechome =~ s,(.*)/$,$1,;  # remove last slash, if any, unless root dir

# Uses $opt_config and $opt_exechome
&read_config();                 # read config file into %configs

# [PM] 4.0.2+ Process any config file overrides
foreach (keys (%opt_conf)) {
  if (defined $configs{$_}) {
    verbose("Overriding config option $_=" . $opt_conf{$_} . "\n");
    $configs{$_} = $opt_conf{$_};
  } else {
    die("! Unknown configure option: $_.");
  }
}

# Option canonicalization
@opt_resources = split(/,/,join(',',@opt_resources));
if ($configs{"SPLIT_OPT_CFLAG"}) {
  @cflags = grep { $_ ne '' } split(/,/,join(',',@cflags)); # also remove empty elements!
}

if ($opt_no_embed_license) {
  $opt_embed_license = 0;
} elsif ($opt_development || $opt_main eq "prolog") {
  $opt_embed_license = 0;
}

verbose("\@nonoptions is (" . join("|", @nonoptions) . ")\n");
foreach (@nonoptions) {
  &param_sub($_);
}

# Note splfr.pl.in uses @ldinputs (which is somewhat better, there may non-files there too FIXME: really need to distinguish user-supplied files from -- args and linger options)
push @linkfiles , @ARGV;        # add any remaining arguments last on linkfiles
}                               # $use_getopt

#### END Option processing.

## [PM] 3.9.1 copies of verbose and vverbose is in splfr.pl.in keep in synch!
sub verbose {
    if ($opt_verbose >= 1) {
	print STDERR "@_";
    }
}

sub vverbose {
    if ($opt_verbose >= 2) {
	print STDERR "@_";
    }
}

sub debugprint {
    if ($opt_verbose >= 3) {
	print STDERR "@_";
    }
}

sub iswin32 {
    if ($opt_platform =~ m/win32/) {
	return 1;
    } else {
	return 0;
    }
}

sub basename {
    my $suffix = $_[1];
    if ($suffix) {
	$_[0] =~ m,([^/]*)\Q$suffix\E$,s;
	return $1;
    } else {
	$_[0] =~ m,([^/]*)$,s;
	return $1;
    }
}


sub dirname {
  $_[0] =~ m,^(.*)/([^/]*)$,s;
  return $1;
}

# From Perl Cookbook
sub trim {
    my @out = @_;
    for (@out) {
	s/^\s+//;
	s/\s+$//;
	s/\s+/ /g;
    }
    return wantarray ? @out : $out[0];
}

# [PM] 4.0.2 ensure we always clean up.
END {
  # Delete temporary files unless --keep has been given
  if (@tmpfiles) {
    if ($opt_keep) {
      verbose("% Keeping temporary files: @tmpfiles\n");
    } else {
      verbose("% Removing temporary files: @tmpfiles\n");
      unlink @tmpfiles;
    }
  }
}

# !! Also used in splfr.pl.in. Keep in synch!
# Parse the configuration file spld.config (which is generated
# by configure).
# The configuration file is matched using the following regexp:
# Assignment = ^(\w+)\s*=(.*)$
# Where $1 is the variable name and $2 is the value to assign
# to it.
sub parse_config
{
  open CONF, "$opt_config" or
    die("! Could not open configuration file $opt_config: $!\n");
  verbose("% Reading configuration file \"$opt_config\"\n");
  while (<CONF>) {
    if (/^(\w+)\s*=(.*)$/) {
      $configs{$1} = $2;
#      $pname = $1;
#      $$pname = $2;
    }
  }
  close CONF;
}

# !! Also used in splfr.pl.in. Keep in synch!
sub read_config
{
  # [PM] 3.8.5 sprm1678 Do not relocate according to exechome if
  #      --config is explicitly specified 
  # Also used in splfr.pl.in. Keep in synch!
  if (!$opt_config) {
    if ($opt_exechome) {
      my $fname;
      # [PM] 3.9.1b4 name changes are passed from configure instead (For movability).
      # # Make sure to catch any name changes of the configuration
      # # file, it is currently version-suffixed.
      # $fname = &basename($opt_config);
      $fname = $config_file_basename;
      $opt_config="$opt_exechome" . "/$fname";
    } else {
      $opt_config=$opt_config_default;
    }
  }
  parse_config();
}

sub optdebug {
#   print STDERR @_;
}

# A similar function exists in splfr.pl.in, keep in sync!
sub param_sub
{
  $_ = $_[0];

  verbose("% Handling non-option arg $_");

  if (0) {
  }
#   elsif ($opt_LD) {                # [PM] 4.0.2+ Never true with new option processing
#     verbose("... as linker input\n");
#     push @linkfiles, $_;
#   }
  elsif (/\.(o|obj|s\.o|so|dll|sl|dylib|lib)$/) {
    verbose("... as linker input\n");
    push @linkfiles, $_;
  } elsif (/\.(pro|pl|ql|po)$/) {
    verbose(" ... as Prolog code\n");
    push @prologfiles, $_;
  } elsif (/\.sav$/) {
    verbose(" ... as Prolog saved state\n");
    push @savfiles, $_;
  } elsif (/\.(c|cc|C|cxx|cpp)$/) {
    verbose(" ... as C/C++ source file\n");
    push @csrcfiles, $_;
    # # [PM] 3.9.2b2 do not keep the object file
    # push @tmpfiles, "$`." . $configs{"OBJEXT"}; # $` is "the string preceeding the last successful match"
  } else {
    verbose(" ... as linker input\n");
    push @ldflags, $_;
  }
}


## Common option aliases and canonicalization

if ($opt_no_resources_from_sav) {
  $opt_resources_from_sav=0
}

# A copy is in splfr.pl.in. Keep in synch!
my $opt_exechome_parent;
# set $opt_exechome_parent
# [PM] 3.9.1 by now $opt_exechome has been set-up (from default or from --exechome=<DIR>)
{
  # there has got to be a simpler way of removing the last directory component!?
  my @dirs;
  my ($volume,$exechomedirs,$exechomefile);
  # [PM] 4.0.2+ done above
  # # [PM] 3.11.3 SPRM 8062 FindBin.pm 1.44 returns path with terminating slash
  # # Note: this will incorrectly blame FindBin if --exechome was passed
  # # a slash-terminated path. No big deal, removing the slash is OK anyway.
  # if ($opt_exechome =~ m,.+/$,) { # exechome ends in slash but is not root dir
  #   vverbose("Broken Perl module FindBin, FindBin::Bin ends in slash (\"$opt_exechome\")\n");
  #   vverbose("  Applying workaround to broken Perl module FindBin\n");
  #   $opt_exechome =~ s,/$,,;       # remove last slash
  #   vverbose("  New result == \"$opt_exechome\"\n");
  # }

  if (File::Spec->can('splitdir')) {
    vverbose("Can File::Spec->splitdir(\"$opt_exechome\")\n");
    # [PM] 3.9.1 contrary to the perldoc.com docs splitdir is *not*
    # available in Perl 5.005_03 (the version of Perl we have on
    # rs6000-aix-4.3 (AIX 4.3.3)
    if (&iswin32()) {           # [PM] 3.9.1 win32 implies File::Spec->can('splitpath')
      vverbose("Can File::Spec->splitpath(\"$opt_exechome\")");
      ($volume,$exechomedirs,$exechomefile) = File::Spec->splitpath($opt_exechome,1);
      vverbose("== (\"" . ($volume ? $volume : "") . "\",\"${exechomedirs}\",\"" . $exechomefile . "\")\n");
    } else {
      # vverbose("Cannot File::Spec->splitpath(\"$opt_exechome\")\n");
      undef($volume);
      $exechomefile = "";
      $exechomedirs = $opt_exechome;
    }
    @dirs = File::Spec->splitdir($exechomedirs);
  } else {
    # [PM] 4.1.3 Can this happen anymore (used to be an issue on some legacy platforms)
    my $tmp = $opt_exechome;
    # undef($volume);
    # $exechomefile = "";

    vverbose("Cannot File::Spec->splitdir(\"$opt_exechome\") fallback to split(...)\n");
    # [PM] 3.9.1 do not remove initial / (this matches the behaviour of splitdir)
    # $tmp =~ s@^/@@;    # remove initial /
    @dirs = split(m@/@, $tmp);
  }
  # [PM] 3.9.1 here @dirs looks like ("" "usr" "local" "bin" ) NOTE the initial empty component

  pop @dirs;
  # [PM] 3.9.1 here @dirs looks like ("" "usr" "local" )

  vverbose("\@dirs==@dirs\n");

  my $opt_exechome_parent_dir;
  if (File::Spec->can('catdir')) {
    vverbose("Can File::Spec->catdir(@dirs)");
    $opt_exechome_parent_dir = File::Spec->catdir(@dirs);
    vverbose("==\"$opt_exechome_parent_dir\"\n");
  } else {                      # File::Spec missing on really old perl (5.004_04) used on IRIX 6.5
    vverbose("Cannot File::Spec->catdir(@dirs), fallback to join()");
    $opt_exechome_parent_dir = join("/",@dirs);
    vverbose("==\"$opt_exechome_parent_dir\"\n");
  }
  if (&iswin32()) {             # Win32 implies File::Spec->can('catpath')
    vverbose("Can File::Spec->catpath($volume, $opt_exechome_parent_dir, \"\")");
    $opt_exechome_parent = File::Spec->catpath($volume, $opt_exechome_parent_dir, "");
    vverbose("==\"$opt_exechome_parent\"\n");
  } else {
    $opt_exechome_parent = $opt_exechome_parent_dir;
  }
  vverbose("\$opt_exechome_parent==\"$opt_exechome_parent\"\n");
}

if ($opt_nocompile) {
  $opt_keep = 1;
}

# [PM] 3.9.1 FIXME --shared does the wrong thing on MacOS X (actually
# on all platforms --shared does what --jnilib should do but this only
# differes on MacOS X where --shared should create a .dylib and not a
# FLI-style .bundle)
if ($opt_jnilib) {
  $opt_shared=1;
}

$opt_system_type = ($opt_development ? 1 : $opt_extended_rt ? 2 : 0);

if ($USE_RUNTIME_LICENSE =~ "yes") {
  $use_runtime_license=1 if ($opt_system_type == 0 && $USE_RUNTIME_LICENSE_SPRT =~ "yes");
  $use_runtime_license=1 if ($opt_system_type == 2 && $USE_RUNTIME_LICENSE_SPRE =~ "yes");
}


my $opt_lang_no=0;                 # 1 for sicstus, 2 for iso xref spaux.c.in

if ($opt_lang ne "") {
  verbose("opt_lang is \"$opt_lang\"");
  $_ = $opt_lang;
  if (/iso/) {
# [PM] 4.0.2+ not possible to change language, iso is the default
#    $opt_lang_no=2;
  }
# [PM] 4.0.2+
#  elsif (/sicstus/) {
#    $opt_lang_no=1;    
#  }
 else {
    die("! --lang must be \"iso\".");
  }
}

# [PM] 4.0.5 memhook is back
if ($opt_memhook ne "") {
  verbose("opt_memhook is \"$opt_memhook\"");
  $_ = $opt_memhook;
  if (/default/) {
    # nothing
  } elsif (/malloc/) {
    $memhook_name="SP_SET_MEMALLOC_HOOKS_HINT_USE_MALLOC";
  } elsif (/sbrk/ || /sparse/) {
    print STDERR "*Warning*: ignoring --memhook=$_ (obsolescent)\n";      
  } else {
    die("! Unknown --memhook $opt_memhook, must be one of default, malloc");
  }
}
my $opt_moveable_default = ($configs{"SPLD_OPT_MOVEABLE_DEFAULT"} =~ m/yes/);
if (&iswin32()) {
  $opt_moveable_default=1;      # windows apps are always moveable
}

# [PM] 4.3 Whether process locale should be set (note the name mismatch)
my $opt_no_locale_default = !($configs{"SPLD_OPT_USE_LOCALE_DEFAULT"} =~ m/yes/) || !&iswin32();
my $opt_locale_name_default = $configs{"SPLD_OPT_LOCALE_NAME_DEFAULT"};

$opt_dontlink=$configs{"NOLINK_OPT"};

# Similar in splfr.pl.in. Keep in synch!
sub version {
    # [PM] 4.3 Keep the first line of --version synced between all tools
    print <<EOF;
spld (SICStus Prolog $configs{"SICSTUS_VERSION_STRING"})

SICStus Prolog Release $configs{"SICSTUS_VERSION_STRING"}
Copyright (c) 1998-$release_year SICS Swedish ICT

Report bugs at:

    http://www.sics.se/sicstus/bugreport/bugreport.html
EOF

    exit 0 if ($opt_version);
}

# A similar function exists in splfr.pl.in, keep in sync!
sub usage
{
  my $opt_moveable_default_marker;
  my $opt_no_moveable_default_marker;
  if ($opt_moveable_default) {
    $opt_moveable_default_marker = " (default)";
    $opt_no_moveable_default_marker = "";
  } else {
    $opt_moveable_default_marker = "";
    $opt_no_moveable_default_marker = " (default)";
  }

  my $opt_locale_default_marker;
  my $opt_no_locale_default_marker;
  if ($opt_no_locale_default) {
    $opt_no_locale_default_marker = " (default)";
    $opt_locale_default_marker = "";
  } else {
    $opt_no_locale_default_marker = "";
    $opt_locale_default_marker = " (default)";
  }

  print <<EOF;

Usage: spld [options] files...

    -?|--help                   Prints a summary of all options.
    --version                   Prints the version of spld.
    -v,--verbose                Be verbose. Multiple occurrences increase verbosity.
    --namebase=<Base>           Name base for generated source.
    --nocompile                 Generate files but do not compile anything.
                                Implies --keep.
    -o,--output=<File>          Specify name of output executable.
    -D,--development            Create a development system. Implies --main=prolog.
    -E,--extended-rt            Create an extended runtime system.
                                Implies --embed-license
    -S,--static                 Prefer static linking over dynamic.
                                Implies --embed-rt-sav unless --no-embed-rt-sav or -D.
                                Implies --embed-sav-file  unless --no-embed-sav-file.
    --multi-sp-aware            Compile the application with support for using more than one
                                SICStus runtime in the same process. See the manual.
    --shared                    Create a shared object instead of an ordinary executable.
                                Implies --main=none.
                                Not compatible with --static.
                                Somewhat experimental.
    --main=<Type>               Specify what kind of main() the executable should
	                        have. Possible values are "prolog", "restore",
	                        "load", "user" and "none". 
    --resources=<Reslist>       Comma-separated list of resources to pre-link the
                                executable with (e.g., --resources=jasper,tcltk)
                                Data resource can be specified as <FILE>=<NAME>, e.g.,
                                --resources=tcltk,/home/joe/test.sav=/test.sav
                                would embed the tcltk foreign resource and the contents
                                of /home/joe/test.sav and make it accessible with
                                SP_restore(\"URL:x-sicstus-resource:/test.sav\")
                                Multiple occurences accumulates.
    --resources-from-sav        Automatically add any foreign resources needed by the
                                saved state. Can only be used if the .sav-file was
                                embedded as a data resources (with --resources).
    --no-resources-from-sav     Disable --resources-from-sav.
    --respath=<Path>            Path to search for resources. Same syntax as the
                                PATH environment variable.
    --cflag=<Option>            Option to send to compiler.
                                Multiple occurences accumulates.
                                If <Option> contain commas then each comma-separated part is treated
                                as a separate compiler option. This may change in the future,
                                instead you should use multiple occurences of --cflag.
    --                          Send rest of command-line to the link step without further processing.
                                (linking is typically done by the C compiler)
    --config=<File>             Specify an alternate configuration file.
    --conf OPT=VAL              Override configuration file value of option OPT.
                                Can occur multiple times.
    --sicstus=<Exec>            Specify an alternate SICStus executable.
    --window                    Create a windowed executable. (Win32 only)
    --moveable                  Do not hardcode paths into the executable$opt_moveable_default_marker
    --no-moveable               Hardcode paths into the executable$opt_no_moveable_default_marker
    --keep                      Do not erase temporary files and glue code.
    -i,--interactive            Call SP_force_interactive()
                                Only with --main=load or --main=restore.
    --locale=NAME               Set process locale to NAME, empty NAME means set locale from environment$opt_locale_default_marker
    --no-locale                 Do not set process locale (like before 4.3)$opt_no_locale_default_marker
    --userhook                  Object code contains SU_initialize() to be called
                                before SP_initialize(). Ignored for --main={user|none}
    --memhook=<Hook>            Specify memory allocator. One of "default" and "malloc".
    --embed-rt-sav              Embed runtime prolog code (sprt.sav)
    --no-embed-rt-sav           Do not embed sprt.sav even if --static
    --embed-sav-file            Embed argument saved state file as a data resource.
    --no-embed-sav-file         Do not embed saved state as a data resource even if --static
    --embed-license             Embed license information into executable (for use with --extended-rt)
    --no-embed-license          Do not embed license (default)
    --license-file=<File>       Use license from <File> instead of the default when embedding license.

    --allow-devsys              Allow debugging of runtime system by setting environment variables.
                                This is the default except with --development, where it is ignored.
    --no-allow-devsys           Do not allow debugging of runtime system by setting
                                environment variables.

    --with-<Package>=<Dir>      See the manual.
    --no-sprt                   Undocumented
    --cc-data-res               Undocumented
    --wrapper                   Undocumented
    --no-wrapper                Undocumented
    --jnilib                    Undocumented
    --exechome=<Dir>            Undocumented
    --embed-manifest            Undocumented
    --no-embed-manifest         Undocumented


Legacy options:

    -LD                         Same as --
    --vv                        Same as -v -v
    --lang=<Lang>               Chose Prolog dialect. <Lang> must be \"iso\" (the default).
                                Only with --main=load or --main=restore.
    --more-memory               Ignored, for backward compatibility.
    --no-more-memory            Ignored, for backward compatibility.

For a more detailed explanation of these options, consult the online
manual:

    http://www.sics.se/sicstus/docs/

Report bugs at:

    http://www.sics.se/sicstus/bugreport/bugreport.html

EOF

  # [PM] 4.0.2+ Caller should exit 0 or exit 1 as needed
  #    exit 0;
}

if ($opt_help) {
  usage();
  exit 0;
}

if ($opt_version || $opt_verbose) {
  version();
}


# arg 0 = name of main function
# arg 1 = name of emitted file
# arg 2 = bootpath to encode in file
sub emit_main {
    my ($mainname, $file, $bootpath, $files) = @_;
    if (!open(EMITFILE, ">$file")) {
      die("! could not open $file: $!")
    }
    verbose("% Generating main function ($file)...\n");

    print EMITFILE <<EOF;
/* -*- Mode:C; buffer-read-only:t -*-
   This file is automatically generated, do not edit. */
#include <stdio.h>
#include <sicstus/sicstus.h>
EOF
    print EMITFILE "#define SPLD_MAIN_NAME $mainname\n";
#   print EMITFILE "#define SPLD_DSP $opt_system_type\n";
    if (0                       # [PM] bootpath is gone
        && $bootpath ne "") {
      print EMITFILE "#define SPLD_BOOTPATH \"$bootpath\"\n";
    }
    if ($opt_interactive) {
      print EMITFILE "#define SPLD_INTERACTIVE 1\n";
    }
    if ($opt_userhook) {
      print EMITFILE "#define SPLD_USERHOOK 1\n";
    }
    if ($opt_lang_no) {
      print EMITFILE "#define SPLD_LANGUAGE $opt_lang_no\n";
    }

    # [PM] 4.0.5 memhook is back
    if ($memhook_name ne "") {
      print EMITFILE "#define SPLD_MEMHOOK $memhook_name\n";
    }

    {
      my $sav_files_seen=0;
      my $others_seen=0;
      my $sav_file = "";

      for (split /\s+/, $files) {
        if (/\.sav$/) {
          $sav_files_seen++;
          if ($sav_files_seen==1) {
            $sav_file = $_;
          } else {
            print STDERR "*Warning*: ignoring \"$_\", will load \"$sav_file\"\n";
          }
        } else {
          $others_seen++;
        }
      }
      if ($sav_files_seen) {
        print EMITFILE "#define SPLD_SAV_FILE \"$sav_file\"\n";
      }

      if ($others_seen) {
        print EMITFILE "#define SPLD_LOAD_FILES {";
        for (split /\s+/, $files) {
          if (/\.sav$/) {
            ;                   # skip
          } else {
	    # [PM] 4.1 use &"foo"[0] to avoid C++ warnings about implicit conversion from char[] to char*
            print EMITFILE "&\"$_\"[0], ";
          }
        }
        print EMITFILE "0}\n";
      }
    }

    print EMITFILE <<EOF;
/* this will define a main function with name "$mainname" */
#define SPLD_GENERATE_MAIN 1
#include <sicstus/spaux.c>
EOF
    close EMITFILE;
}

sub run_cc {
  my @commandline = @_;
  my @command = ();

 FILTER_ARGS:
  for (@commandline) {
    verbose("$_\n");
    if ($_ eq "" ||		# ignore empty strings
	# Ignore "-nologo" if -v is passed to spld.
	($opt_verbose && $configs{"NOVERBOSE_FLAG"} ne "" && m/$configs{"NOVERBOSE_FLAG"}/)) {
      next FILTER_ARGS;
    }

    push @command, $_;
  }

  if ($opt_nocompile) {
    verbose("Skipping command: @command\n");
    return;
  }

  verbose("@command\n");
  my $rc = system("@command");
  if ($rc != 0) {
    die("! compiler command failed: $?");
  }
}

my $ld_builds_rpath_from_L = ($configs{"LD_BUILDS_RPATH_FROM_L"} =~ m/yes/);


## $opt_cc_data_res can be used to override the default ($configs{"RESGEN_TYPE"})
## data resource generation method and instead use the C generation
## method.
my $data_resource_cc = ( $opt_cc_data_res or ($configs{"RESGEN_TYPE"} eq "cc") );

# A copy of this code in splfr.pl.in, keep in synch!
# [PM] 3.9.2 if running as a cygwin perl script we need to convert
# back to POSIX path from Win32 paths in some cases. 
# [PM] 4.0.5 always uses forward slash on Win32 (to improve atom-syntax compatibility)
sub nativepath {
  my $orig_path = $_[0];
  vverbose("nativepath(\"$orig_path\") ->");

  if (($orig_path ne "") && &iswin32()) {
    if ($^O eq "cygwin") {
      $orig_path=`cygpath -w "$orig_path"`;
      # [PM] 3.9.2 for some reason there will be a terminating newline remove it
      chomp $orig_path;
    }
    $orig_path =~ s@\\@/@g;
  }
  vverbose(" \"$orig_path\"\n");
  return $orig_path;
}

sub posixpath {
  my $orig_path = $_[0];
  vverbose("posixpath(\"$orig_path\") ->");

  if (($orig_path ne "") && &iswin32() && ($^O eq "cygwin")) {
    $orig_path =~ s@\\@/@g;
    $orig_path=`cygpath -u "$orig_path"`;
    # [PM] 3.9.2 this was needed with cygpath -w, probably is with -u too.
    chomp $orig_path;
  }

  vverbose(" \"$orig_path\"\n");
  return $orig_path;
}


# A copy of this code in splfr.pl.in, keep in synch!
my ($prefix, $native_prefix);
my ($SP_BINAUXDIR, $SP_BINAUXDIR_NATIVE);
my $SP_LIBDIR;
my $SP_ROOTLIBDIR;
# [PM] 3.9.1 set-up paths
{
  if (&iswin32()) {
    # [PM] 3.9b4 these are where spld/splfr can find files at compile/link time

    # [PM] 3.9.1
    $prefix = $opt_exechome_parent;
    # [PM] pre 3.9.1
    # $prefix="$opt_exechome";
    # $prefix =~ s@(.*)[/\\][^/\\]*?$@$1@; # Strip last component of path

    # Duplicated in splfr.pl.in KEEP IN SYNCH
    # [PM] 3.9.2 when running as a script under cygwin we need a native path for CL.EXE
    $native_prefix = &nativepath($prefix);

    $SP_BINAUXDIR="$prefix/bin";
    $SP_BINAUXDIR_NATIVE="$native_prefix/bin";

    $SP_LIBDIR="$prefix";
    $SP_ROOTLIBDIR="$prefix/bin";
  } else {
    # [PM] 3.9.1 note that these lib/ are not related to the value of $configs{"SP_RTSYS_DIRNAME"}/
    # [PM] 3.9b4 these are where spld can find files at compile/link time
    if ($opt_exechome) {        # [PM] 3.9.1 now on UNIX as well
      vverbose("\$opt_exechome_parent = \"$opt_exechome_parent\", \$DEF_PREFIX = \"" . $configs{"DEF_PREFIX"} . "\"\n");
      $prefix = $opt_exechome_parent;
      vverbose("Setting prefix using $opt_exechome/.. to \"$prefix\"\n");
    } else {                    # fall back, should not happen
      verbose("ERROR: could not set \$opt_exechome, fallback to \$DEF_PREFIX \"" . $configs{"DEF_PREFIX"} . "\"\n");
      $prefix=$configs{"DEF_PREFIX"};
    }
    $native_prefix=$prefix;
    $SP_BINAUXDIR="$prefix/lib/" . $configs{"SP_DIRNAME"} . "/bin";
    $SP_BINAUXDIR_NATIVE=$SP_BINAUXDIR;
    $SP_LIBDIR="$prefix/lib/" . $configs{"SP_DIRNAME"};
    $SP_ROOTLIBDIR="$prefix/lib"; # $SP_RT_DIR
  }
}                               # end of shared code

# [PM] DIRSEP is used to create a path suitable as a quoted prolog
#      atom. This makes backslash a bad idea. Luckily, Perl on Win
#      appears to live happliy with forward slash as dir
#      separator. Furthermore, some (all?) of the paths being
#      manipulated already use forward slash even when used in a
#      windows path (e.g., C:/Program Files/...)

# BUT: Was there not a problem with Win 95/98 and forward slash?
# ([PM] 3.9b2 yes Win95..Win2k do not accept forward slash in command.exe paths)
my $DIRSEP="/";


if ($opt_moveable == -1 && $opt_no_moveable > 0) {
  # --no-moveable
  $opt_moveable=0;
} elsif ($opt_no_moveable == -1 && $opt_moveable > 0) {
  # --moveable
  $opt_moveable=1;
} elsif ($opt_moveable > 0 && $opt_moveable > 0) {
  die("! --moveable and --no-moveable ar mutually exclusive.");
} else {
  $opt_moveable=$opt_moveable_default;
}

if (&iswin32()) {
  $opt_moveable=1;		# windows apps are always moveable
}

# [PM] 3.9b4 these are where the generated executable can find files at run time
# For now they are the same as the compile time values but they
# should start using run-time look-up of SP_APP_DIR and SP_RT_DIR
my $SP_RT_BINAUXDIR="$SP_BINAUXDIR"; # $SP_RT_DIR/$configs{"SP_DIRNAME"}/bin
# $SP_RT_LIBDIR="$SP_LIBDIR"; # $SP_RT_DIR/$configs{"SP_DIRNAME"}
my $SP_RT_ROOTLIBDIR="$SP_ROOTLIBDIR"; # $SP_RT_DIR


my $SP_BINDIR="$prefix/bin";
my $SP_INCDIR="$native_prefix/include";

# [PM] 3.9.2b2 on UNIX we have version suffixed exes, use these to
# permit multiple versions of sicstus to be installed in the same bin
# directory.
my $SICSTUS_VERSION_EXE_SUFFIX;
if (&iswin32()) {
  $SICSTUS_VERSION_EXE_SUFFIX = "";
} else {
  $SICSTUS_VERSION_EXE_SUFFIX = "-" . $configs{"SICSTUS_VERSION_STRING"};
}

my $SICSTUS;
if ($opt_sicstus ne "") {
  $SICSTUS=$opt_sicstus;
} else {
  $SICSTUS="$SP_BINDIR/sicstus$SICSTUS_VERSION_EXE_SUFFIX";
}

if (&iswin32() && ($^O ne "cygwin")) {
    # Replace '/' with '\' on Windows, since system() does not
    # accept '/' in commands when piping stuff to them.
    $SICSTUS =~ s@/@\\@g;
}

my $SICSTUS_FLAGS = "-f";
# [PM] 3.12.2 inhibit both logo and "loading ..." messages unless verbose
if (!$opt_verbose) {
  $SICSTUS_FLAGS .= " --nologo --noinfo";
}

$opt_namebase = trim($opt_namebase); # needed?
my $def_namebase;
if ($opt_static) {
  $def_namebase = "spldgen_s_"; # prevents dynamic and static versions to clash when --keep
} else {                        # dynamic
  $def_namebase = "spldgen_";
}

if ($opt_keep) {
  if ($opt_namebase eq "") {
    $opt_namebase = $def_namebase;
  }
}

my $tmpfilebase;
if ($opt_namebase ne "") {
  $tmpfilebase = $opt_namebase;
} else {
  # [PM] 3.9 no longer use SPTMP. The way SPTMP was used never guaranteed uniqueness anyway.
  $tmpfilebase = sprintf("%s%d_%d_", "$def_namebase", $$, time()); # $$ is Process ID
}

my $QC=$configs{"QUOTECHAR"};

$opt_with_jdk = trim($opt_with_jdk);
$opt_with_bdb = trim($opt_with_bdb);
$opt_with_tcltk = trim($opt_with_tcltk);
$opt_with_tcl = trim($opt_with_tcl);
$opt_with_tk = trim($opt_with_tk);

if ($opt_with_bdb ne "") {
  $configs{"BDBLIB"} =~ s/\Q$configs{"BDB_PATH"}\E/$opt_with_bdb/g;
}
if ($opt_with_tcltk ne "") {
  $configs{"TCLLIB"} =~ s/\Q$configs{"TCLTK_PATH"}\E/$opt_with_tcltk/g;
} else {
  if ($opt_with_tcl ne "") {
    $configs{"TCLLIB"} =~ s/\Q$configs{"TCL_PATH"}\E/$opt_with_tcl/g;
  }
  if ($opt_with_tk ne "") {
    $configs{"TCLLIB"} =~ s/\Q$configs{"TK_PATH"}\E/$opt_with_tk/g;
  }
}


if ($opt_window) {
    if (&iswin32()) {
	push @ldflags, "$SP_BINAUXDIR_NATIVE/spcon.lib";
	push @ldflags, "$SP_BINAUXDIR_NATIVE/sicstus.res";
	push @cflags, "-DSP_WIN=1";
	push @cflags, "-DSP_CONSOLE"; # [PM] 3.9 moved here
    } else {
	die("! the --window option is only available on Win32 platforms.");
	# $opt_window = 0;
    }
}

# [PM] 3.9.1 FIXME SPLD_SHLD should be set by configure, eventually
my $SPLD_SHLD=$configs{"SPLFR_SHLD"};


# [PM] April 2000 Further investigation indicates that spld should not
# look in "$SP_LIBDIR/library" at all. See if this causes problems.
my $def_respath = "$SP_LIBDIR/library/$opt_platform";


if ($opt_respath) {
    $opt_respath = &posixpath($opt_respath);
    # [PM] 3.9.2 it does not matter if PATHSEP is ";" also when running as a cygwin script
    $opt_respath .= $configs{"PATHSEP"}.$def_respath;
} else {
    $opt_respath = $def_respath;
}

# [PM] 3.9.2 $opt_respath is a POSIX path here if running as a cygwin script

# verbose("respath = $opt_respath\n");

my $resfailed = "";
# [PM] 3.9.2 Win32 paths if running as a cygwin script
my @resargs = ();

# [PM] 3.9.1 FIXME we should not zap opt_resources since this will
# miss data resouces (and rpath-only resources, although this makes
# little sense on Darwin). We probably should do something reasonable
# in process_resources() instead.
#
# if ($opt_resources and $opt_platform =~ m/darwin/ and not $opt_static) {
#   print STDERR "*Warning*: dynamic pre-linked resources are not available on the MacOS X platform.\n";
#   print STDERR "*Warning*: --resources argument ignored.\n";
#   $opt_resources="";
# }

&process_resources();           # [PM] 3.9 must be done before examining @savfiles

my $mainname = "user_main";

if ($opt_main eq "default") {
  my $do_warn=1;
  if ($opt_shared) {
    $opt_main = "none";
  } elsif (@savfiles > 0) {
    $opt_main = "restore";
  } elsif (@prologfiles > 0) {
    $opt_main = "load";
  } elsif ($opt_system_type==1) {
    $opt_main = "prolog";
    # [PM] 3.8.6 do not warn for spld -D without --main=prolog
    #            (since no other --prolog value is compatible with -D)
    $do_warn=0;
  } else {
    $opt_main = "user";
  }
  verbose("* No specified main function. Defaulting to \"$opt_main\".\n") if ($do_warn);
}


my $defpath = 0;

# Parse --main option.
my $gen_main=1;                    # [PM] 3.9 should we generate main()

if ($opt_shared) {
  if ($opt_main ne "none") {
    die("! --shared requires --main=none.")
  }
  if ($opt_static) {            # [PM] 3.9b5 should eventually allow --static --shared!
    die("! --shared incompatible with --static.")
  }
}

$_ = $opt_main;

SWITCH: {
  # [PM] 3.8.6 Added warning about ignored files (should give error?)

  if (/prolog/) {
    if (@prologfiles > 0 or @savfiles > 0) { # [PM] 3.8.6
      print STDERR "* some input files ignored (incompatible with --main=prolog)\n";
      print STDERR "           The ignored files were: @prologfiles @savfiles\n";
    }

    $opt_system_type=1;         # --main=prolog => --development
    last SWITCH;
  }
  # [PM] 3.9b5 FIXME restore and load should be merged
  #      the emitted main file already does the right thing by first
  #      restoring and then loading all other files.
  if (/restore/) {
    my $tmpfile = "$tmpfilebase" . "restore_main";
    my $state = $savfiles[-1];
    if (not $state) {
      print STDERR "* No saved-state specified. Using \"main.sav\".\n";
      $state = "main.sav";
      push @savfiles, "main.sav";
    }
    if ($opt_system_type!=1) {
      if (1) {                  # [PM] 3.9b5 load both .sav and others
        if ($opt_moveable) {
          &emit_main($mainname,"$tmpfile.c","","@savfiles @prologfiles");
        } else {
          &emit_main($mainname,"$tmpfile.c","$SP_RT_BINAUXDIR","@savfiles @prologfiles");
        }

      } else {
        if ($opt_moveable) {
          &emit_main($mainname,"$tmpfile.c","","$state");
        } else {
          &emit_main($mainname,"$tmpfile.c","$SP_RT_BINAUXDIR","$state");
        }
      }
      push @csrcfiles, "$tmpfile.c";
      push @tmpfiles, "$tmpfile.c";
      # push @tmpfiles, "$tmpfile." . $configs{"OBJEXT"};

      # [PM] 3.9b5 all non-.sav files will be SP_load-ed after SP_restore.
      #            any extraneous .sav-file will be warned about in
      #            emit_main so do not do that here.
      # if (@prologfiles >= 1 or @savfiles > 1) {
      #   print "* some input files ignored (incompatible with --main=restore)\n";
      #   print "           The ignored files were: @prologfiles @savfiles[0..@savfiles-2]\n";
      # }
    } else {
      die("! --main=$_ is incompatible with -D or --development.")
    }

    last SWITCH;
  }
  if (/load/) {
    my $tmpfile = "$tmpfilebase" . "load_main";
    if ($opt_system_type!=1) {
      if (@savfiles > 0) {      # [PM] 3.8.6
        print STDERR "* some input files ignored (incompatible with --main=load)\n";
        print STDERR "           The ignored files were: @savfiles\n";
      }

      if ($opt_moveable) {
        &emit_main($mainname,"$tmpfile.c","","@prologfiles");
      } else {
        &emit_main($mainname,"$tmpfile.c","$SP_RT_BINAUXDIR","@prologfiles");
      }
      push @csrcfiles, "$tmpfile.c";
      push @tmpfiles, "$tmpfile.c";
      # push @tmpfiles, "$tmpfile." . $configs{"OBJEXT"};
    } else {
      die("! --main=$_ is incompatible with -D or --development.");
    }
    last SWITCH;
  }
  if (/user/) {

    if ($opt_system_type==1) {
      die("! --main=$_ is incompatible with -D or --development.");
    }

    if (@prologfiles > 0 or @savfiles > 0) { # [PM] 3.8.6
      print STDERR "* some input files ignored (incompatible with --main=user)\n";
      print STDERR "           The ignored files were: @prologfiles @savfiles\n";
    }
    $opt_system_type = ($opt_extended_rt ? 2 : 0);
    last SWITCH;
  }

  if (/none/) {                 # [PM] 3.9 experimental
    # Inhibit generation of most/all supporting code
    if ($opt_userhook) {
      print STDERR "*Warning*: --userhook ignored, incompatible with --main=none option.\n";
    }
    # $opt_userhook=1;          # this inhibits generating default userhook
    $defpath=1;                 #
    $gen_main=0;                # do not generate main()

    if ($opt_system_type==1) {
      die("! --main=$_ is incompatible with -D or --development.");
    }
    {
      # my @sav_files_res = grep /\QURL:x-sicstus-resource:\E/, @savfiles;
      my @sav_files_files = grep {not /\QURL:x-sicstus-resource:\E/} @savfiles; # PRM 3242

      if (@prologfiles > 0 or @sav_files_files > 0) {
        print STDERR "* some input files ignored (incompatible with --main=$_)\n";
        print STDERR "           The ignored files were: @prologfiles @sav_files_files\n";
      }
    }

    $opt_system_type = ($opt_extended_rt ? 2 : 0);
    last SWITCH;
  }
}

## [PM] 4.0 emit main on all platforms.
## if (not &iswin32()) {
  if ($gen_main
      && !$opt_window           # [PM] 4.0 window executable use WinMain (which also tells linker to assume /SUBSYSTEM:WINDOWS)
     ) {
    my $tmpfile = "$tmpfilebase" . "main_wrapper";
    push @csrcfiles, "$tmpfile.c";
    push @tmpfiles, "$tmpfile.c";
    # push @tmpfiles, "$tmpfile." . $configs{"OBJEXT"};

    verbose("% Generating wrapper for main function ($tmpfile.c)...\n");

    if (!open(EMITFILE, ">$tmpfile.c")) {
      die("! could not open $tmpfile.c: $!");
    }
    print EMITFILE <<EOF;
#include <sicstus/sicstus.h> /* sp_main_internal() */
int SPCDECL main(int argc, char *argv[]) {
  return sp_main_internal(argc, argv);
}
EOF
    close(EMITFILE);
  }
## }

# Start assembling the parameters to something which
# can be sent to the compiler.

my $exe_kind = "";              # Used as ${exe_kind} below
if ($gen_main) {

# Whether the executable is windowed or not.
if ($opt_window) {
    $exe_kind="windowed ";
    push @main_code, split(' ',$configs{"WINMAINOBJ"});
} else {
    $exe_kind="character-based ";
    push @main_code, split(' ',$configs{"CHARMAINOBJ"});
}

# Determine which save-dump to use at start-up
if ($opt_system_type==1) {
    $exe_kind="development ";
} elsif ($opt_system_type==2) {
    $exe_kind="extended runtime ";
} else {
    $exe_kind="runtime ";
}

# Kind of main program
if ($opt_main eq "prolog") {
  push @main_code, split(' ',$configs{"DSMAINOBJ"});
}
}                               # $gen_main

verbose("% Building a ${exe_kind}executable.\n");

if (! $opt_multi_sp_aware) {    # also if --main=none (PRM 3213)
  push @main_code, $configs{"SPAUXOBJ"};
}


# Prepend $SP_BINAUXDIR_NATIVE to all files in $main_code (not SP_RT_BINAUXDIR)
for my $file (@main_code) {
  # note that $file is an alias to each array element
  $file = "$SP_BINAUXDIR_NATIVE/$file";
}


# Add all files in @main_code to @linkfiles
push @linkfiles, @main_code;




# [PM] 3.9 FIXME: Broken: at this point @linkfiles may contain files not supplied by user
#                  ditto for csrcfiles. Perhaps need to consider data_resource_cc too.
#                  (no big deal, the linker will complain).
#
# At this points, all input files should be collected.
if (($opt_main eq "user" or $opt_main eq "none") and @linkfiles == 0 and @csrcfiles == 0) {
  die("! No C/C++ source or object files present (needed when main is \"user\" or \"none\")");
}

# [PM] 3.9b5 SU_initialize is only called by RT if opt_userhook
# development always calls SU_initialize so need a dummy version
#
# Generate default definition of SU_initialize()
if ((not $opt_userhook) && $opt_system_type==1) {
  my $tmpfile = "$tmpfilebase" . "defuserhook";

  push @csrcfiles, "$tmpfile.c";
  push @tmpfiles, "$tmpfile.c";
  # push @tmpfiles, "$tmpfile." . $configs{"OBJEXT"};
  if (!open(EMITFILE, ">$tmpfile.c")) {
    die("! could not open $tmpfile.c: $!");
  }
  verbose("% Generating default definition of SU_initialize() ($tmpfile.c)...\n");
  print EMITFILE <<EOF;
/* This file is automatically generated. Do not edit! */
#include <sicstus/sicstus.h> /* SU_initialize() declaration */
int SPCDECL SU_initialize(int argc, char *argv[]) {
    (void)argc; (void)argv;
    return 0;
}
EOF
  close EMITFILE;
}


my $numfiles = @csrcfiles + @linkfiles + @prologfiles + @savfiles;
if (!$numfiles) {
  die("! No input files.");
}

# [PM] 3.9.1 The libs should be passed last so for linkers (e.g., IRIX 6.5)
#            that need to see the use of a symbol before the definition.
#            SPLD_EXE_LDFLAGS and SPLD_EXE_LIBS are now passed explicitly to run_cc()
# 
# # [PM] 3.9.1 what used to be called LDFLAGS is now split and renamed
# $SPLD_LDFLAGS = "$configs{"SPLD_EXE_LDFLAGS"} $configs{"SPLD_EXE_LIBS"}";
# 
# if (&iswin32()) {
#   $SPLD_LDFLAGS =~ s@\\@/@g;
# }
# 
# push @ldflags, split(' ',$SPLD_LDFLAGS);

if (&iswin32()) {
  $configs{"SPLD_EXE_LDFLAGS"} =~ s@\\@/@g;
  $configs{"SPLD_EXE_LIBS"} =~ s@\\@/@g;
}
my @spld_exe_ldflags = split(' ', $configs{"SPLD_EXE_LDFLAGS"});
my @spld_exe_libs = split(' ', $configs{"SPLD_EXE_LIBS"});

if ($opt_multi_sp_aware && $opt_static) {
    print STDERR "*Warning*: --multi-sp-aware ignored, incompatible with --static option.\n";
    $opt_multi_sp_aware=0;
}

push(@cflags, "-DSICSTUS_TARGET_VERSION=$SICSTUS_VERSION"); # [PM] 4.0.5 for consistency check
push(@cflags, "-DMULTI_SP_AWARE") if ($opt_multi_sp_aware);

if (&iswin32()) {               # WINDOWS
  if (not $opt_no_sprt) {
    # [PM]
    if ($opt_static) {
      unshift @ldflags, "$SP_BINAUXDIR_NATIVE/" . $configs{"RTKERNEL_BASENAME_WITH_STAT_SUFFIX"}; # not SP_RT_BINAUXDIR
    } else {
      unshift @ldflags, "$SP_BINAUXDIR_NATIVE/" . $configs{"IMPLIB"}; # not SP_RT_BINAUXDIR
    }
  }
} else {                        # UNIX
  if (not $opt_no_sprt) {
    # [PM] 3.9.1 note that these lib/ are not related to the value of $configs{"SP_RTSYS_DIRNAME"}/
    if ($opt_static) {
        # [PM] 3.9.1 Use full path to static version to avoid depending on
        # linker search rules and ability to specify CC_FORCE_DYN
        unshift @ldflags, "$prefix" . "/lib/" . $configs{"RTKERNEL_BASENAME_WITH_STAT_SUFFIX"};
    } else {
      my $sprt = $configs{"RTKERNEL_BASENAME_SANS_SUFFIX"};
      $sprt =~ s/^lib//;         # remove initial "lib" HACK ALERT

      my @sp_lflags = ("-L$prefix"."/lib",
                    "-l$sprt"
                   );
      @ldflags = (@sp_lflags, @ldflags);
    }
  }
}



# [PM] 3.9 Not used? @RFILES = ();
# [PM] 3.9.1 not used? @RLIBS = ();

sub push_data_resource {
  my ($resfile,$resname)  = @_;

  # Get rid of backslash so prolog reader does not see it in an quited atom context.
  $resfile =~ s@\\@/@g;
  $resname =~ s@\\@/@g;

  new_resgen("$resfile", "$resname");

  # The pattern <FILE_PATH>=<NAME_PATH> signals that this is a data resource
  push @resargs, ("'" . &nativepath($resfile) . "=" . $resname . "'" . " ");
}                               # push_data_resource

my $already_warned_about_pre_linked=0;

sub push_resource {
  my $resfile = $_[0];

  my $linkfile = $resfile;
  my $basename = &basename($resfile); # a.k.a. non-directory part
  # print("basename($resfile) = $basename\n");
  verbose("% Found ");

  # [PM] 3.9.1 FIXME this should probably be something like FLI_IMPLIB_SFX instead of SHSFX
  #            It must match whatever is used in process_resources @sfxlist
  if ($basename =~ m/\Q$configs{"FLI_SHSFX"}\E$/s) {
    my $dirname = &dirname($resfile);
    verbose("shared resource '$resfile'\n");

    # [PM] 3.9.1 Pre-linked dynamic resources does not work at all on
    # some platforms, it does not work well on some platforms and it
    # does not seem to bring any advantages.
    if (! $already_warned_about_pre_linked) {
      print STDERR "*Warning*: pre-linking of dynamic resources is neither recommended nor needed.\n";
      $already_warned_about_pre_linked=1; # only warn once
    }

    if ($dirname) {
      # [PM] 3.9.1 A hack to put $dirname on RPATH so that the foreign
      # resource will be found by the run-time loader on OSes where
      # the run-time loader ignores the full pathname of the shared
      # object (such as Tru64)
      vverbose("pushing -L$dirname\n");
      push @ldflags, "-L$dirname";
    }
    push @resargs, ("'" . basename($resfile,"." . $configs{"FLI_SHSFX"}) . "'" . " ");
  } elsif ($basename =~ m/\Q$configs{"STSFX"}\E$/s) {
    verbose("static resource '$resfile'\n");
    push @resargs, ("'" . basename($resfile,"." . $configs{"STSFX"}) . "'" . " ");
  } else {
    verbose("unknown type resource '$resfile'\n");
    $resfile =~ m,([^\.]*)$,s; # pick up suffix
    push @resargs, ("'" . basename($resfile,".$1") . "'" . " ");
  }
  push @linkfiles, &nativepath($linkfile);
}

if ($opt_no_more_memory) { $opt_more_memory = 0; } # ignore --more-memory if --no-more-memory was specified
# [PM] 3.11.2 true if --more-memory was passed on command line but --no-more-memory was not
my $opt_more_memory_explicit = $opt_more_memory;


### Things that turn on --more-memory ($opt_more_memory) goes here
if ( ($opt_platform =~ m/^x86-linux-/)) { # [PM] 3.11.2 --more-memory is now the default
  $opt_more_memory=1;
}

### Things that turn off --more-memory ($opt_no_more_memory) goes here
if ($opt_shared) {
  if ($opt_more_memory_explicit) {
    print STDERR "*Warning*: --more-memory not compatible with --shared, ignored\n";
  }
  $opt_no_more_memory=1;
}

### finally obey $opt_no_more_memory and then $opt_more_memory
if ($opt_no_more_memory) { $opt_more_memory=0; } # this is not redundant!

## Only use $opt_more_memory below this line

if ($opt_more_memory) {
  if ( ($opt_platform =~ m/^x86-linux-/)) { # [PM] 3.10.1 test was negated in 3.10.0... (SPRM 4388)
   if (!$opt_more_memory_explicit) {
     verbose("Using implicit --more-memory option\n")
   }

    $opt_linux_linker_script=1;
  }
  else {
    verbose("* --more-memory ignored for this platform\n");
  }
  # ... other platforms here
}

if ($opt_linux_linker_script) {
  if (! ($opt_platform =~ m/^x86-linux-/)) {
    print STDERR "*Warning*: --linux-linker-script not compatible with this platform, ignored\n";
    $opt_linux_linker_script=0;
  }
}

if ($opt_linux_linker_script) {
  if ($opt_shared) {
    print STDERR "*Warning*: --linux-linker-script not compatible with --shared, ignored\n";
    $opt_linux_linker_script=0;
  }
}

if ($opt_linux_linker_script) {
  verbose("Creating a modified linker GNU ld script\n");
  my $linkerscriptfile= "$tmpfilebase" . "linker_script.x";
  my $setLC_ALL;
  # [PM] 3.11.1 SPRM 7579
  if ($configs{"SPLD_OPT_MORE_MEMORY_NEEDS_LC_ALL_C"} eq "yes") {
    # [PM] 3.11.3 SPRM 8086 use 'env' cmd for portability
    $setLC_ALL="env LC_ALL=C ";
  } else {
    $setLC_ALL="";
  }
  my $dummy_exec = "$tmpfilebase" . "dummy_exec";

  # [PM] 3.11.3 Argh, using gcc does not work well since the
  # complaints about undefined main() will get mixed up with the
  # linker script on stderr. Instead, as a hack, we use the ld -m
  # EMULATION flag used with ld -r by splfr
  #
  # 
  # # [PM] 3.11.3 We need to go to ld via gcc so that any 32/64-bit
  # #             flags in $CFLAGS can affect which linker script is
  # #             output by ld --verbose --noinhibit-exec is so that ld
  # #             does not exit with error despite unresolved main().  I
  # #             have not found a way to pass --verbose to ld via gcc
  # #             without also having gcc trying to link an executable.
  # #             Also note that ld --verbose writes to stdout but gcc
  # #             -Wl,--verbose writes to stderr.
  # #
  # # $pipe_cmd="$setLC_ALL$configs{"LD"} --verbose |";
  # $pipe_cmd="$setLC_ALL$configs{"CC"} $CFLAGS -o $dummy_exec -Wl,--verbose -Wl,--noinhibit-exec 2>&1 |";

  my $pipe_cmd= $setLC_ALL . $configs{"LD"} . " " . $configs{"SPLFR_LD_r_FLAGS"} . " --verbose |";
  vverbose("Opening pipe \"$pipe_cmd\"\n");
  open(LDPIPE, $pipe_cmd) or die("! Could not get linker script from " . $configs{"LD"} . ": $!");
  open(LINKERSCRIPT, "> $linkerscriptfile") or die("! Could not open linker script file $linkerscriptfile: $!");
  push @tmpfiles, $linkerscriptfile;
  push @tmpfiles, $dummy_exec;

  # [PM] A small state machine
  # "initial" -- seen "using internal linker script" --> "find_start"
  # "find_start" -- seen the ========== immediately following "using ..." -> "body"
  # "body" -- lines not looking like ===== -> print the line -> "body"
  # "body" -- seen the ========== terminating the linker script -> exit

  my $wellformed = 0;
  my $found_magic = 0;          # found the TEXT segment info
  my $state = "initial";
  while (<LDPIPE>) {
    vverbose("$_");
    if ($state eq "body") {
      if (/=================================/) {
        $wellformed=$found_magic;
        last;
      }
      # Change TEXT segment start to make sure everything below 0x10000000 is free for SP to use

      # [PM] 3.11.3 SPRM 8133, SPRM 8080 On Fedora (and probably RH

      # Enterprise Linux) the start address occurs more than
      # once. Replace it everywhere it occurs but still look for the
      # SIZEOF_HEADERS line to increase the likelihood that we know
      # what we are doing.
      # This was on
      #   Fedora Core release 2 (Tettnang) (x86-linux-glibc2.3)
      #   GNU ld version 2.15.90.0.3 20040415
      # SPRM 8080 used Red Hat Enterprise Linux 3.0 (U2) (glibc 2.3)
      # and was probably hit by the same problem.
      #
      # NOTE: this fix is correct but I have not yet confirmed that it helps with SPRM 8080, SPRM 8133

#       if (s,\Q0x08048000 + SIZEOF_HEADERS;\E,0x10048000 + SIZEOF_HEADERS; /* reserve more low-memory for SICStus */,) {
#         vverbose("Found and changed magic linker script line\n");
#         vverbose("New line: $_");
#         $found_magic=1;
#       }
      if (m,\Q0x08048000 + SIZEOF_HEADERS;\E,) {
        vverbose("Found magic linker script line\n");
        $found_magic=1;
      }
      if (s,\Q0x08048000\E,0x10048000 /* reserve more low-memory for SICStus */,g) {
        vverbose("Found and changed magic linker script constant\n");
        vverbose("New line: $_");
      }
      print LINKERSCRIPT "$_";
      next;
    }
    if ($state eq "initial") {
      if (/using internal linker script/) {
        vverbose("State transition: initial->find_start\n");
        $state = "find_start";  # next line should be =======
      }
      next;
    }
    if ($state eq "find_start") {
      if (/=================================/) {
        vverbose("State transition: find_start->body\n");
        $state="body";
      } else {
        last;                   # malformed
      }
      next;
    }
    die "! Internal error\n";
  }

  close(LINKERSCRIPT);
  close(LDPIPE);
  if (! $wellformed) {
    die ("!Failed to modify linker script");
  }
  vverbose("Adding modified linker script \"-Wl,--script=$linkerscriptfile\" to ldflags\n");
  push @ldflags, "-Wl,--script=$linkerscriptfile";
}


# Add extra flags for resources which use third-party software.
sub check_extra_lflags {
  my ($resource,$only_rpath) = @_;

  # [PM] 3.8.6 $only_rpath -- add -R/-rpath for resource but not any libraries.
  #      Currently (3.8.6) only for jasper (see configure.in for a discussion on JDK_RPATH) (3.9.1 now for tcltk/bdb as well, tru64)

  my %module_lib = ( "tcltk" => $configs{"TCLLIB"},
                     "jasper" => $configs{"JAVALIB"},
                     "bdb" => $configs{"BDBLIB"}
                     # [PM] 4.0.2 as of SP4 any necessary "sockets" lib is always built-in
                     # "sockets" => $configs{"SOCKETLIB"}
                     );
  my %module_rpath = ( "jasper" => $configs{"JDK_RPATH"}
                     );
                     
#   my @xmodules = ("tcltk", "jasper", "bdb", "sockets");
#   my @xmodules_lib_var_names = (TCLLIB, JAVALIB, BDBLIB, SOCKETLIB);
#   my @xmodules_rpath_var_names = (
#                                DUMMY_RPATH, # TCL_RPATH,
#                                JDK_RPATH,
#                                DUMMY_RPATH, # BDB_RPATH,
#                                DUMMY_RPATH # SOCKET_RPATH
#                               );
  for my $module (keys (%module_lib)) { # @xmodules
    # my $mod_lib_var_name = shift @xmodules_lib_var_names;
    # my $mod_rpath_var_name = shift @xmodules_rpath_var_names;
    if ($resource =~ m/$module\.$configs{"FLI_SHSFX"}/ ||
        $resource =~ m/$module\.$configs{"STSFX"}/) {
      my $mod_lib = trim($module_lib{$module}); # trim($$mod_lib_var_name);

      # [PM] 3.9b5 this means if --moveable is specified then
      # --resources=-jasper will still embed RPATH but
      # --resources=jasper will not. This seems sane.
      # [PM] 3.9.1 FIXME: interaction with CC_NOLIBPATH
      # [PM] 3.9.1 FIXME: interaction with LD_BUILDS_RPATH_FROM_L (this esp. with --moveable and rpath only --resources)
      if ($only_rpath) {
        my $mod_rpath;
        vverbose("Processing RPATH-only $resource\n");
        vverbose(" \$mod_lib=$mod_lib\n");
        if (defined ($module_rpath{$module})) { # defined($$mod_rpath_var_name)
          $mod_rpath = trim($module_rpath{$module}); # trim($$mod_rpath_var_name);
        } else {
          vverbose(" No module specific path, extracting -L<DIR>\n");
          # No module specific rpath. Try to extract -L<DIR> and use
          # -R<DIR>
          # FIXME: there could be more than one -L argument!
          # [PM] 3.9.1 FIXME: mod_rpath should be built in a loop through mod_lib
          if ($mod_lib =~ m/-L([^\"]\S*?)\s/) {
            $mod_rpath = "$1";
          } else {
            $mod_rpath = "";
          }
        }
        if ($mod_rpath) {
          if ($ld_builds_rpath_from_L) { # [PM] 3.9.1 AIX
            my @rpath_dirs = split(":", $mod_rpath);

            # [PM] 3.9.1 By default the AIX linker adds each -L<dir>
            # to the runpath. This alone would allow us to use either
            # -L<dir> or adding <dir> to the run-path explicitly.
            #
            # However, the AIX 4.3.3 linker does not add -L<dir> dirs
            # if an explicit run-path is used. So were we to add
            # $mod_rpath to the run-path we would have to ensure that
            # all -L<dir>s were also explicitly added (as if
            # $ld_builds_rpath_from_L was false).
            #
            # However, the adventure does not stop there, if an
            # explicit run-path is specified to the AIX 4.3.3 linker
            # the standard directories (/usr/lib:/lib) will not be
            # searched at run-time, leading to disaster. To cope with
            # that would require some configure parameter
            # add_last_to_runpath_if_runpath_is_passed_explicitly,
            # which is a little too long a name, if nothing else...
            #
            # Instead never use explicit run-path if $ld_builds_rpath_from_L is true (i.e., on AIX)


            for my $rpath_dir (@rpath_dirs) {
              vverbose("Adding -L$rpath_dir to ldflags (will be added to run-path by linker)\n");
              push @ldflags, "-L$rpath_dir";
            }
          } else {
            vverbose("Adding $mod_rpath to ldrpath");
            push @ldrpath,$mod_rpath;
          }
        }

      } else {                  # not $only_rpath
        push @ldflags, split(' ',$mod_lib);
      }
    }
  }
}

my $data_resources_postprocess;

sub win_encode_resname {
  my $resname = $_[0];
  my $encoded_resname = $resname;

  $encoded_resname =~ y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/; # upcase a-z
  # [PM] 3.12.6 Give up on those characters that need #&<DECIMALCODE>; (since it has never worked and no-one complained)
  #            (RC.EXE will complain about too strange file names even if we quote the resource name).
  if ($encoded_resname =~ m,[^ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./_],) {
    my $tmp = $encoded_resname;
    $tmp =~ y,ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./_,,d; # delete allowed chars
    die("! Invalid resource name \"$resname\", only a-z, A-Z, 0-9, '/', '_' and '.' allowed (disallowed: \"$tmp\")");
  }
  vverbose("Encoded resname \"$resname\" as \"$encoded_resname\"\n");

  return $encoded_resname;      # for now. See fli_win.c for what should really be done
}

sub new_resgen_spres {            # also used for CC-based resource compiling
  my ($resfile, $resname, $use_cc) = @_;
  my $suffix = ".spresdata";
  my $restype = "SICSTUSRESOURCE1.0";
  my $encoded_name = win_encode_resname($resname);
  my $linkfile = $tmpfilebase . basename($resfile) . $suffix;

  verbose("% Generating data resource resource data file ($linkfile) from ($resfile)...\n");

  push @tmpfiles, $linkfile;

# [PM] 3.9b5 use --goal to minimize dependency on shell and echo (primarily a problem for Win32)
  my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC"
                 . "("
                 . "asserta(user:(portray_message(error, _Message) :- abolish(user:portray_message/2), print_message(error, _Message), halt(1))), "
                 . "use_module(library(resgen)), spld_resgen_prepare_data_resource_prefix_raw('" . &nativepath($resfile) . "','$resname','" . &nativepath($linkfile) . "'),halt);halt(1)." . "$QC");

  verbose("> @command\n");


  my $rc = system("@command");
  if ($rc != 0) {
    die("! Could not build data resource resource data file.");
  }
  if (!open(LINKFILE, ">> $linkfile")) {
    die("! Could not add data to data resource resource data file: $!");
  }
  if (!open(RESFILE, "< $resfile")) {
    die("! Could not open data resource file for input: $!");
  }
  binmode(LINKFILE);
  binmode(RESFILE);
  while (<RESFILE>) {
    print LINKFILE "$_";
  }
  close(RESFILE);
  close(LINKFILE);

  if ($use_cc) {                # generate .c input (any platform without direct support for .o-generation)
    ;                           # nothing
  } else {                      # generate .rsrc input (Win32)
    if ($data_resources_postprocess eq "") {
      $data_resources_postprocess = "// SICStus data resources\n// (encoded)NAME TYPE DATAFILE\n";
    }
    $data_resources_postprocess .= "$encoded_name $restype $linkfile\n";
  }
  return $linkfile;
}

sub postprocess_data_resources_win {
  my $rsrc_file = "$tmpfilebase" . "data_resource.rsrc";
  my $res_file = "$tmpfilebase" . "data_resource.res";

  verbose("% Generating .rsrc file for data resources ($rsrc_file) ...\n");

  if (!open(RSRCFILE, "> $rsrc_file")) {
    die("! Could not open data resource .rsrc file: $!");
  }
  push @tmpfiles, $rsrc_file;

  print RSRCFILE "$data_resources_postprocess";
  close(RSRCFILE);

  verbose("% Generating .res file for data resources ($res_file) ...\n");
  # [PM] 3.9.1 now in spld.config $configs{"RCEXE"}="rc";
  my $RCVERBOSE;
  if ($opt_verbose) {
    $RCVERBOSE="-v"
  } else {
    $RCVERBOSE=""
  }

  push @tmpfiles, $res_file;
  my @command = ($configs{"RCEXE"} . " -x $RCVERBOSE -r -fo$res_file $rsrc_file");

  if ($opt_nocompile) {
    verbose("Skipping command: @command\n");
  } else {
    verbose("> @command\n");
    my $rc = system("@command");
    if ($rc != 0) {
      die("! Could not generate .res file for data resources.");
    }
  }
  # this is done even if $opt_nocompile
  push @linkfiles, $res_file;
}

sub postprocess_data_resources {
  if (&iswin32) {
    postprocess_data_resources_win();
  } else {
    die("! Data resources not supported on this platform");
  }
}

sub new_resgen {
  my ($resfile, $resname) = @_;

  if ($data_resource_cc) {
    my $raw_data_file = new_resgen_spres("$resfile", "$resname", 1);
    my $tmpcfile = "$tmpfilebase" . "data_resource" . basename($resfile) .".c";
    my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC" . "(assert(user:(portray_message(error,_M) :- abolish(user:portray_message/2), print_message(error, _M),halt(1))), use_module(library(resgen)), spld_file_to_cc('" . &nativepath($raw_data_file) . "', '$resname', '" . &nativepath($tmpcfile) . "'),halt);halt(1)." . "$QC");

    verbose("> @command\n");
    my $rc = system("@command");
    if ($rc != 0) {
      die("! Could not build data resource file");
    }
    push @csrcfiles, "$tmpcfile";
    push @tmpfiles, "$tmpcfile";
  } else {
    my $linkfile="";

    if (&iswin32() && ($configs{"RESGEN_TYPE"} eq "win32res")) {
      my $linkfile = new_resgen_spres("$resfile", "$resname", 0);
    } elsif ($configs{"RESGEN_TYPE"} eq "prefix") {
      $linkfile = basename($resfile) . "." . $configs{"OBJEXT"};

      verbose("% Generating data resource file ($linkfile) from ($resfile)...\n");

      my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC" . "(assert(user:(portray_message(error,_M) :- abolish(user:portray_message/2), print_message(error, _M),halt(1))), use_module(library(resgen)), spld_resgen_prepare_data_resource_prefix('" . &nativepath($resfile) . "','$resname','" . &nativepath($linkfile) . "'),halt);halt(1)." . "$QC");

      verbose("> @command\n");

      push @tmpfiles, $linkfile;

      my $rc = system("@command");
      if ($rc != 0) {
        die("! Could not build data resource file");
      }

      {
        if (!open(LINKFILE, ">> $linkfile")) {
          die("! Could not add data to data resource file: $!");
        }
        if (!open(RESFILE, "< $resfile")) {
          die("! Could not open data file for input: $!");
        }

        binmode(LINKFILE);
        binmode(RESFILE);
        while (<RESFILE>) {
          print LINKFILE "$_";
        }
        close(RESFILE);
        close(LINKFILE);

        push @linkfiles, $linkfile;
      }

    } else {                    # neither prefix, win32res or cc, barf
      die("! Unkown or incompatible data resource generation method \"" . $configs{"RESGEN_TYPE"} . "\"");
    }
  }
}

# sub resgen {
#   my $datafile = $_[0];
#   my $suffix = "." . $configs{"OBJEXT"};
# 
#   if (&iswin32()) {
#     $suffix = ".res";
#   }
#   my $linkfile = basename($datafile) . $suffix;
# 
#   verbose("% Generating data resource file ($linkfile) from ($datafile)...\n");
# 
#   my $resname = basename($datafile); # includes suffix
# 
#   my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC" . "(assert(user:(portray_message(error,_M) :- abolish(user:portray_message/2), print_message(error, _M),halt(1))), use_module(library(resgen)), spld_resgen_prepare_data_resource_prefix('" . &nativepath($datafile) . "','$resname','" . &nativepath($linkfile) . "'),halt);halt(1)." . "$QC");
# #  my @command = ("echo $QC" . "assert(user:(portray_message(error,_M) :- abolish(user:portray_message/2), print_message(error, _M),halt(1))), use_module(library(resgen)), spld_resgen_prepare_data_resource_prefix('$datafile','$resname','$linkfile')." . "$QC | $SICSTUS -f");
#   verbose("> @command\n");
#   my $rc = system("@command");
#   if ($rc != 0) {
#     die("! Could not build data resource file");
#   }
#   if (!open(LINKFILE, ">> $linkfile")) {
#     die("! Could not add data to data resource file: $!");
#   }
#   if (!open(DATAFILE, "< $datafile")) {
#     die("! Could not open data file for input: $!");
#   }
#   binmode(LINKFILE);
#   binmode(DATAFILE);
#   while(<DATAFILE>) {
#     print LINKFILE "$_";
#   }
#   close(DATAFILE);
#   close(LINKFILE);
#   return $linkfile;
# }


## must be called before defaulting --main (since it may add to @savfiles)
sub process_resources {
  my $data_res_seen=0;
  my $spXX_sav_seen=0;
  my $sav_name="";
  my $opt_resources_from_sav_used=0;
  my $default_resources_from_sav;

  debugprint("Entering process_resources\n");

  # Set search order for resource suffixes.
  my @sfxlist;
# [PM] 4.1.3 FLI_SHARED_PRELINKS has been "no" on all platforms for a long time
#  if ($configs{"FLI_SHARED_PRELINKS"} eq "no") { # Win32 AND MacOSX where dynamic foreign resources cannot be prelinked

    # [PM] 3.9.1 On Win32 (but not MacOS X/Darwin) it would be
    # possible to pre-link dynamic foreign resources by using an
    # import library. However, I think pre-linked dynamic foreign
    # resource should be abandoned completetely since it gives no
    # benefit (except perhaps in some obscure cases involving RPATH).

    # [PM] 3.9 Windows use .lib for both archives and import libraries.
    # Consider the QP method of adding 's' to the base name of static resources
    if ($opt_static) {
      @sfxlist = ($configs{"STSFX"});
    } else {
      @sfxlist = ($configs{"STSFX"});
    }
#  } else {
#    # [PM] 3.9.1 FIXME this should probably be something like FLI_IMPLIB_SFX instead of SHSFX
#    # Unix treats .so as import libraries so should look for both kinds
#    if ($opt_static) {
#      @sfxlist = ($configs{"STSFX"}, $configs{"FLI_SHSFX"});
#    } else {
#      @sfxlist = ($configs{"FLI_SHSFX"}, $configs{"STSFX"});
#    }
#  }


  ## [PM] 3.9 content of the .rc-file passed to RC.EXE for linking data
  ## resources into .res-file
  $data_resources_postprocess="";

#  my @opt_resources = split(/,/, $opt_resources);


  # [PM] 3.11.2 new --embed-sav-file. Default true if no explicit resources
  if ((@opt_resources == 0)  # no explicit resources
      && @savfiles == 1         # exactly one .sav file
      && $opt_static
      && !$opt_no_embed_sav_file # not explicitly turned off
     ) {                        # --static
    verbose("Defaulting to --embed-sav-file\n");
    $opt_embed_sav_file=1;
  }
      
  if ($opt_no_embed_sav_file) { $opt_embed_sav_file = 0; }

  if ($opt_embed_sav_file)
    {
      if (@savfiles == 1) {     # exactly one sav file
        # FIXME: 
        #   Should be able to specify --embed-sav-file even if other
        #   resources present. Also if more than one sav file

        # replace the sav file argument with a data resource argument
        my $explicit_sav_file = shift @savfiles;
        # @savfiles and should be empty here
        my $resname  = "/" . basename($explicit_sav_file);
        my $resspec = $explicit_sav_file . "=" . $resname;
        verbose("Converting argument $explicit_sav_file to data resource spec \"$resspec\"\n");
        push @opt_resources, $resspec;
      }
      elsif (@savfiles == 0) {   # no sav files
        print STDERR "*Warning*: --embed-sav-file specified but no .sav file argument\n";
        $opt_no_embed_sav_file=1;
        $opt_embed_sav_file=0;
      } else {                  # @savfiles>1
        print STDERR "*Warning*: --embed-sav-file ignored, cannot handle more than one sav-file\n";
        $opt_no_embed_sav_file=1;
        $opt_embed_sav_file=0;
      }
    }

  # [PM] 3.10 For static executables, if there are no foreign resources
  # and exactly one data resource (a sav file) specified then default
  # to --resources-from-sav.
  # [PM] 3.10 This will only be used if an embedded sav-file appears,
  # in which case it is the lone --resources argument.
  # [PM] 3.11.2 FIXME: Should be able to extract resources from multiple sav files.
  $default_resources_from_sav = ((@opt_resources == 1)
                                 && $opt_static
                                 && !$opt_no_resources_from_sav
                                 && !$opt_resources_from_sav
                                 );


 OUTER: for my $res_arg (@opt_resources) {
    my $only_rpath=0;
    my $data_res=0;
    my $data_file;
    my $data_file_is_sav_name=0;
    my $data_name;
    my @res;                    # [PM] 3.10 only contains $res_arg unless $opt_resources_from_sav
    my $r;

    verbose("% Processing \"$res_arg\"");
    if ($res_arg =~ s/^-(.*)/$1/) {
      verbose(", RPATH-ONLY flag detected, continues with \$res_arg==\"$res_arg\"");
      $only_rpath=1;            # ignored if data resource
    }
    if ($res_arg =~ m/^(.*)=(.*)$/) {
      $data_res=1;
      $data_file="$1";
      $data_name="$2";
      $data_res_seen=1;

      verbose(", data resource detected (file \"$data_file\" resource name \"$data_name\")");

      if ( $opt_system_type==1 ? ($data_name eq "/spds.sav") :
           $opt_system_type==2 ? ($data_name eq "/spre.sav") :
           ($data_name eq "/sprt.sav") ) {
        $spXX_sav_seen = 1;
        verbose(", default spXX.sav will not be embedded");
      }
      if ($data_name =~ m/\.sav$/) { # a sav file
        if (not $sav_name) {    # only if not already set
          if (not (($data_name =~ m%/sprt.sav%) or
                   ($data_name =~ m%/spre.sav%) or
                   ($data_name =~ m%/spds.sav%))) { # do not restore these
            $sav_name=$data_name; # $sav_name is the first embedded .sav file.
            $data_file_is_sav_name=1; # [PM] 3.10 $data file is the sav file to extract resource names from
          }
        }
      }
    }
    verbose("\n");

    @res = ();
    if ($data_res) {
      my $found = 0;
      if (-e $data_file) {
        $found=1;
      } else {
        verbose("Data resource \"$data_file\" does not exist, looking in \"$opt_respath\", ");

        for my $resbase (split(/\Q$configs{"PATHSEP"}\E/, $opt_respath)) {
          my $resfile = $resbase . $DIRSEP . $data_file;
          if (-e $resfile) {
            $found=1;
            $data_file = $resfile;
            verbose("found \"$data_file\"\n");
            last;
          }
        }
        verbose("not found\n") if (not $found);
      }
      if ($found) {
        push_data_resource($data_file, $data_name);
        
        if ($data_file_is_sav_name
            && $default_resources_from_sav)
          {
            verbose("Defaulting to --resources-from-sav\n");
            $opt_resources_from_sav=1;
          }

        if ($opt_resources_from_sav
            && $data_file_is_sav_name) {
          my $in_meta_info=0;
          verbose("Extracting needed foreign resource names from '$data_file'\n");
          $opt_resources_from_sav_used=1;
          if (!open(DATAFILE, "< $data_file")) {
            die("! Could not open sav-file \"$data_file\" for input: $!");
          }
          binmode(DATAFILE);
          while (<DATAFILE>) {
            last if m/^archmask=/;
            # [PM] 4.1.3 since 4.1(?) archmask line looks like "version=DECNUM archmask=HEXNUM"
            last if m/^version=/;
            if ($in_meta_info) {
              last if m/^# +META_INFO +END/;
              if (m/^# +FR: +"([^"]*)"/) {
                verbose("Found resource name \"$1\"\n");
                push @res, $1
              }
            } else {
              if (m/# +META_INFO +([0-9]+)/) {
                if ($1 == 1) {
                  $in_meta_info=1
                } else {
                  die("META_INFO version is \"$1\", expected \"1\" (in \"$data_file\")\n");
                }
              }
            }

          }
          close(DATAFILE);
          vverbose("\@res==@res\n");
        }
      } else {
        die("! Could not find data resource \"$data_file\"");
      }
      # [PM] 3.10 here @res is typically empty so not further
      # processing will be done. The exception is with
      # opt_resources_from_sav
    } else {                    # not $data_res
      @res = ($res_arg);
    }

    # [PM] 3.10 typically @res is the single foreign resource but if
    # resource names are extracted from an embedded .sav-file then
    # @res is the names of all such resources.
    for my $resname (@res) {
      if ((not $only_rpath)
           # [PM] 4.2.1 SPRM 12134 only check existence if it has
           # extension or slash. Do not look for plain resource names
           # (e.g. extracted from sav file)
           && ( $resname =~ m,[.\\/], )
           && -e $resname) {
        push_resource($resname);
      } else {
        my $res_found=0;
      RESPATH_LOOP: for my $resbase (split(/\Q$configs{"PATHSEP"}\E/, $opt_respath)) {
          for my $sfx (@sfxlist) {
            my $resfile = $resbase . $DIRSEP . $resname . ".$sfx";
            push_resource($resfile) if ((not $only_rpath) && -e $resfile);
            if ($only_rpath || -e $resfile) {
              check_extra_lflags("$resname.$sfx", $only_rpath);
              $res_found=1;
              last RESPATH_LOOP;
            }
          }
        }
        $resfailed .= $resname . " " if (! $res_found);
      }
    }
  }

#  if ($data_res_seen) {
#    $opt_embed_rt_sav=1;        # embed sprt.sav by default if any data resource present
#  }

  # by default embed sprt.sav in static run-time systems
  if ($opt_static && (not $opt_no_sprt) && $opt_system_type!=1 && (not $opt_no_embed_rt_sav)) {
    $opt_embed_rt_sav=1
  }

  if ($opt_embed_rt_sav && not $spXX_sav_seen) {
      if ($opt_extended_rt) {
	  push_data_resource("$SP_BINAUXDIR_NATIVE/spre.sav", "/spre.sav"); # not SP_RT_BINAUXDIR
      } else {
	  push_data_resource("$SP_BINAUXDIR_NATIVE/sprt.sav", "/sprt.sav"); # not SP_RT_BINAUXDIR
      }
  }

  # [PM] 4.0.5 USE_RUNTIME_LICENSE
  if ($use_runtime_license) {
    if ($opt_embed_license) {
      my $licfile = "$tmpfilebase" . "runtime_license.pl";
      verbose("% Extracting license information" . ( ($opt_license_file ne "") ? " from $opt_license_file" : "") . " into $licfile...\n");
      push @tmpfiles, $licfile;
      
      my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC"
                     . "("
                     . "asserta(user:(portray_message(error, _Message) :- abolish(user:portray_message/2), print_message(error, _Message), halt(1))), "
                     . "use_module(library(resgen)), spld_resgen_prepare_license('" . &nativepath($licfile) . "',$opt_system_type,'" . &nativepath($opt_license_file) . "'),halt);halt(1)." . "$QC");

      verbose("> @command\n");
      my $rc = system("@command");
      if ($rc != 0) {
        die("! Could not generate licence file.");
      }
      
      push_data_resource($licfile, "/license.pl");
    }
  }

  if ($sav_name) {
    if (not @savfiles) {
      verbose("Adding implicit argument URL:x-sicstus-resource:$sav_name\n");
      push @savfiles, "URL:x-sicstus-resource:$sav_name";
    }
  }

  if ($data_resources_postprocess) {
    postprocess_data_resources();
  }
  if ($opt_resources_from_sav && ! $opt_resources_from_sav_used)
    {
      print STDERR "*Warning*: --resources-from-sav specified but no embedded .sav file\n";
    }
  debugprint("Exit process_resources\n");
}                               # process_resources

if ($resfailed ne "") {
  die "! failed to locate the following resources: $resfailed\n";
}

#
# # Split options at commas into list @cflags
# for $x (split(/,/, $opt_cflag)) {
#   # Ignore empty arguments
#   if ($x ne "") {
#     push @cflags, $x;
#   }
# }

if (not &iswin32()) {
  if ($opt_moveable) {
    # [PM] 3.9.1 update to use ./$SP_RTSYS_DIRNAME instead of hardwiring ./lib/
    # [PM] 3.9b4 Assume a run-time layout like:
    # myapp.exe
    # $SP_RTSYS_DIRNAME/
    # +--- libsprt38.so
    # +--- sicstus-3.8.7/
    #      +--- bin/
    #      |    +--- sprt.sav
    #      +--- library/
    #           +--- <files from $SP_PATH/library>
    #
    #
    # thus -R "\$ORIGIN/$SP_RTSYS_DIRNAME" will ensure sprt.so will be found on
    # systems where $ORIGIN is supported (SysV ABI, supported at least
    # by Solaris and Linux)
    if (($configs{"HAVE_SYSV_ORIGIN"} eq "yes")
        # && $LD_ROPT
       )
      {
        # push @ldflags, "$LD_ROPT" . "'\$ORIGIN/$configs{"SP_RTSYS_DIRNAME"}'";
        push @ldrpath, "\$ORIGIN/" . $configs{"SP_RTSYS_DIRNAME"};
      }
  } else {                      # not $opt_moveable

    # [PM] 3.9.1 Some platforms adds -L directories to RPATH
    # automatically. For these platforms it is pointless to build a
    # separate rpath.
    #
    # However, on AIX if the rpath is set explicitly -L<DIR>s are
    # ignored. This is a problem if rpath-only resources (eg..,
    # --resources=-jasper) are used. FIXME: probably should build the
    # rpath and then use it only if some rpath component was
    # explicitly specified (e.g., by --resources=-jasper)

    # Platforms where -L<DIR>s are added include AIX 4.3.3 (only AIX SICStus 3.9.1)
    if (! $ld_builds_rpath_from_L) {
      # Add DT_RPATH for each -L directory. ([PM] 3.9b4 According to
      # the GNU docs this is a bad idea esp. with slow NFS mounts)
      vverbose("Extracting -L<DIR> from @{ldflags}\n");
      for (@ldflags) {
        if (m/^-L(.*)$/) {
          vverbose("adding rpath element $1 from ldflags\n");
          push @ldrpath, "$1";
        }
      }
    }                           # not $ld_builds_rpath_from_L
  }                             # not $opt_moveable
}                               # not &iswin32()

if (!$opt_nortable) {
  my $tmpfile = "$tmpfilebase" . "prolog_rtable";
  my $rtable = "$tmpfile" . ".c";
  push @csrcfiles, $rtable;
  push @tmpfiles, $rtable;
  # my $rtable_obj = "$tmpfile." . $configs{"OBJEXT"};
  # push @tmpfiles, $rtable_obj;

  verbose("% Generating resource table ($rtable)...\n");
  # [PM] 3.8.6 check resargs instead of opt_resources so that
  #            rpath-only resources (-jasper) do not count
  if (@resargs) {
    my $clink_opt;
    if ($data_resource_cc) {
      $clink_opt = "--link-data"; # let the C linker resolve the reference to the data resource
    } else {
      if (&iswin32() && ($configs{"RESGEN_TYPE"} eq "win32res")) {
        $clink_opt = "--no-link-data";
      } else {
        if ($configs{"RESGEN_TYPE"} eq "prefix") {
          $clink_opt = "--link-data"; # let the C linker resolve the reference to the data resource
        } else {
          # Should not get here
          die("! Internal error: Unkown or incompatible data resource generation method \"" . $configs{"RESGEN_TYPE"} . "\"");
        }
      }
    }
    debugprint("About to call spld_prepare_resource_table($rtable)\n");
    my $verbosity = ($opt_verbose ? "--verbose" : "" );
    my @command = ("$SICSTUS $SICSTUS_FLAGS --goal $QC" . "use_module(library(fligen)),spld_prepare_resource_table." . "$QC -a $verbosity $clink_opt --outfile='" . &nativepath($rtable) . "' @resargs");

    verbose("> @command\n");
    my $rc = system("@command");
    if ($rc != 0) {
      die("! Could not build resource table");
    }
    debugprint("Called spld_prepare_resource_table($rtable)\n");

  } else {                      # no @resargs
    # Write an empty resource file. This is actually equivalent to
    # calling prepare_resource_table with [] as first arg, but without
    # having to actually call SICStus. Useful in order to bootstrap
    # the development executable.
    if (!open(RTABLE, "> $rtable")) {
      die("! Could not open $rtable: $!");
    }
    print RTABLE <<EOF;
/* #define INCLUDED_DATADEFS_H */
#include <sicstus/sicstus.h>

SP_MainFun *sp_pre_linkage[] = {0};
char *sp_pre_map[] = {0};
EOF
    close(RTABLE);
  }
} else {
  verbose("* No resource table generated\n");
}

my @DEF_CFLAGS = split(' ', $configs{"CFLAGS"} . " " . $configs{"EXTRA_CFLAGS"}); # [PM] 4.0.2+ added EXTRA_CFLAGS
# [PM] 3.9.2 Made SPLD_DSP undefined the same as 0 (i.e., ordinary RT)
#            This way old (non-SICS) make-files will continue to work
my $spld_dsp_value = $opt_system_type;
if ($spld_dsp_value != 1) {     # not devsys
    # [PM] 4.2 propagate extra magic bits on SPLD_DSP
    if (!$opt_allow_devsys) {
        my $SPLD_DSP_BIT_DISALLOW_DEVSYS = 0x0080; # xref sicstus.h
        $spld_dsp_value |= $SPLD_DSP_BIT_DISALLOW_DEVSYS;
    }
}
push @cflags, "-DSPLD_DSP=$spld_dsp_value" if $spld_dsp_value != 0;
push(@cflags, "-DSPLD_SET_PROCESS_LOCALE") if ($opt_no_locale!=1 && !&iswin32());
push(@cflags, "-DSPLD_PROCESS_LOCALE_NAME=$opt_locale_name") if ($opt_no_locale!=1 && !&iswin32());


if ($opt_verbose >= 2) {
  push @cflags, $configs{"VERBOSE_FLAG"};
}
my @incr_cflags = ();
if ($opt_shared) {
  @incr_cflags = split(' ', $configs{"INCR_CFLAGS"});
}

verbose("% Compiling C sources...\n");
for my $src (@csrcfiles) {
  my $obj = $src;
  $obj =~ s/(.*)\.(\w+)$/$1.$configs{"OBJEXT"}/;
  push @tmpfiles, $obj;         # [PM] 4.0.2+ 

  my @OUTPUT_SPEC = split(' ', $configs{"NOLINK_OUTPUT_OPT"} . $obj);

  my @commandline = ($configs{"CC"},
                  # [PM] 3.9.beta2 SPRM 2421, quote include dir
                  "-I\"$SP_INCDIR\"", # [PM] Mar 2000 get the right version of sicstus.h by looking here *first*
		  @DEF_CFLAGS,
                  @incr_cflags,
		  @cflags,
		  ## [PM] Mar 2000, moved up: "-I$SP_INCDIR",
		  $configs{"NOLINK_OPT"},
		  $src,
		  @OUTPUT_SPEC);
  run_cc(@commandline);

  push @autoobj, $obj;
}

my $win32_cc_special = "";
my $win32_shld_special = "";

if (&iswin32()) {
  # [PM] if the linker complains then fix the problem, not the complaint
  # Was: $win32_special = "-link -nodefaultlib:libc.lib";
  # Make sure that the linker gets debug flag if the C-compiler does.
  if (1  # [PM] 3.9.1 since it now configurable we do not need to second guess the CFLAGS.
      || ($configs{"CFLAGS"} =~ m/-Yd/)) {
    # [PM] 3.9.1 now configurable
    $win32_cc_special = $configs{"WIN32_SPLD_CC_SPECIAL"};
    $win32_shld_special = $configs{"WIN32_SPLD_SHLD_SPECIAL"};
    # # [PM] Was: $win32_special .= " -debug -debugtype:BOTH";
    # $win32_cc_special = "-link -debug -debugtype:BOTH";
    # $win32_shld_special = "-debug -debugtype:BOTH";
  }

}




# verbose("@DEF_CFLAGS\n");
# verbose("@OUTPUT_SPEC\n");

$opt_output =~ s@\\@/@g;

verbose("Linking \"$opt_output\"\n");

my $rpath="";

# if ($LD_ROPT && @ldrpath) {
#   my $delim = "";
#   for $p (@ldrpath) {
#     if ($p) {
#       $rpath .= $delim . $p;
#       $delim = ":";
#     }
#   }
# }
#

if (!$opt_shared) {
  push @ldrpath, split(':', $configs{"SPLD_EXE_EXTRA_RPATH"});
}

# Setup $rpath from @ldrpath
if (@ldrpath) {
  my $delim = "";
  my %foo;                      # hash table used to avoid adding duplicate elements to rpath
  for my $p (@ldrpath) {
    if ($p) {
      if ($foo{$p}++ == 0) {    # if $p occurs for the first time in %foo then its entry before incrementation will be zero
        $rpath .= $delim . $p;
        $delim = ":";
      }
    }
  }
}

if ($opt_embed_manifest || $opt_no_embed_manifest) {
  if ($opt_no_embed_manifest)
    {
      $opt_embed_manifest = 0;
    }
} else {                        # no option specified
  if ($configs{"SPLD_EMBED_MANIFEST"} eq "yes") {
    $opt_embed_manifest = 1;    # [PM] 4.0 default on Win32, we assume Visual Studio 2005
  }
}

if ($opt_shared) {

  if ($rpath && $configs{"SPLD_SHLD_ROPT"}) {
    vverbose("Adding RPATH \"" . $configs{"SPLD_SHLD_ROPT"} . "'$rpath'\" to ldflags\n");
    push @ldflags, $configs{"SPLD_SHLD_ROPT"} . "'$rpath'";
  }

  my @OUTPUT_SPEC = split(' ', $configs{"SHLD_OUTPUT_OPT"} . $opt_output);

  # [PM] This may not be right at all on platforms where various kinds
  # of shared objecs exists (such as MacOS X).
  # Also, things like ignore_unresolved is questionable for spld (and
  # also, perhaps, for splfr in 3.9).

  # We really should have SPLD_SHLDFLAGS.
  my @shld_flags = split(' ', $configs{"SPLFR_SHLDFLAGS"});
  my $extra_libs;
  # [PM] 3.9.1 AIX need -lm with spld --jnilib
  if ($opt_jnilib) {
    $extra_libs = $configs{"SPLD_JNI_LIBS"};
  } else {                      # ordinary shared
    $extra_libs = "";           # FIXME: consider SPLD_SHARED_LIBS
  }
  run_cc($SPLD_SHLD,
         @shld_flags,
         @autoobj,
         @linkfiles,
         @ldflags,
         $configs{"SPLD_SHLD_JNILIB_FLAGS"}, # [PM] 3.9.1 -bexpall on AIX with SHLD=(xlc or ld)
         $extra_libs,
         # @RLIBS,
         @OUTPUT_SPEC,
         $win32_shld_special);
  if ($opt_embed_manifest) {
    my $manifest="$opt_output.manifest";
    run_cc($configs{"MT"},
           "/manifest", $manifest,
           "/outputresource:$opt_output;#2"   # 2 is ISOLATIONAWARE_MANIFEST_RESOURCE_ID
          );
    # Delete the embedded manifest file
    if (!$opt_keep) {           # also implied by $opt_nocompile
      push @tmpfiles, $manifest;
    }
  }

} else {                        # not shared (i.e., ordinary executable)
  if ($rpath && $configs{"CC_ROPT"}) {
    vverbose("Adding RPATH \"" . $configs{"CC_ROPT"}  . "'$rpath'\" to ldflags\n");
    push @ldflags, $configs{"CC_ROPT"} . "'$rpath'";
  }

  my @OUTPUT_SPEC = split(' ', $configs{"OUTPUT_OPT"} . $opt_output);
  run_cc($configs{"CC"},
         "-I$SP_INCDIR",        # [PM] Mar 2000 look here *first* for correct sicstus.h
         @DEF_CFLAGS,
         @cflags,
         @spld_exe_ldflags,     # [PM] 3.9.1 used to be on @ldflags
         # [PM] Mar 2000 moved up: "-I$SP_INCDIR",
         @autoobj,
         @linkfiles,
         @ldflags,

         ($opt_moveable ? $configs{"CC_NOLIBPATH"} : ""), # [PM] 3.9.1 What used to be called AIXNOLIBPATH

         # [PM] 3.9 not used? $MATHLIB,
         # @RLIBS,

         @spld_exe_libs,        # [PM] 3.9.1 used to be on @ldflags

         @OUTPUT_SPEC,
         $win32_cc_special);

  if ($opt_embed_manifest) {
    my $manifest="$opt_output.manifest";
    run_cc($configs{"MT"},
           "/manifest", $manifest,
           "/outputresource:$opt_output;#1"   # 1 is CREATEPROCESS_MANIFEST_RESOURCE_ID
          );
    # Delete the embedded manifest file
    if (!$opt_keep) {           # also implied by $opt_nocompile
      push @tmpfiles, $manifest;
    }
  }
}

if (! $opt_nocompile) {
  print STDERR "Created \"$opt_output\"\n";
}

# Generate wrapper scripts on platform which cannot hardcode shared library
# paths into executables (such as Rhapsody). 

if ($opt_no_wrapper) {
  $opt_wrapper=0;
}

if ($opt_wrapper == -1) {         # -1 means not specified
  if ($configs{"SPLD_OPT_WRAPPER_DEFAULT"} eq "yes") { # --opt-wrapper is the default
    if ($opt_shared) {
      $opt_wrapper = 0;
    } elsif ($opt_moveable) {
      $opt_wrapper = 0;
    } else {
      $opt_wrapper=1;
    }
  } else {                      # --opt-wrapper is not the default
    $opt_wrapper=0;
  }

  # [PM] 3.9.1 For some platforms where the executable cannot be
  # made moveable we can set-up the appropriate environment
  # variables in the wrapper.
  if ($opt_moveable) {
    if ($configs{"SPLD_OPT_MOVEABLE_NEEDS_WRAPPER"} eq "yes"
        && (not $opt_static)    # static are assumed self-contained
       ) {
      print STDERR "*Warning*: --moveable implies --wrapper\n";
      $opt_wrapper = 1;
    }
  }
}

if ($opt_wrapper) {
    verbose("% Creating wrapper script...\n");
    if (! $opt_nocompile) {
      my $rc = system($configs{"CP"} . " $opt_output $opt_output.exe");
      if ($rc != 0) {
        die("! Could not generate wrapper script.");
      }
    }
    if (!open(WRAP, "> $opt_output")) {
        die("! Could not open $opt_output.");
    }

    print WRAP <<EOF;
#!/bin/sh
# -*- buffer-read-only:t -*-
# This wrapper is automatically generated. Do not edit!

argv_0="\$0"

EOF

    # [PM] 3.9.1 put back this special handling of darwin later, if
    # needed (and document why HOME/lib is there ([PM] 3.9.1 see man
    # dyld, HOME/lib is on DYLD_FALLBACK_LIBRARY_PATH (which we should
    # consider using instead of DYLD_LIBRARY_PATH)
    #
    # if ($opt_platform =~ m/rhapsody/ || $opt_platform =~ m/darwin/) { # [PM] 3.8.7 added darwin
    #     $sp_rtl_path="$SP_RT_ROOTLIBDIR:\$HOME/lib:/usr/local/lib:/lib:/usr/lib";
    # } else

    my $sp_rtl_path="$SP_RT_ROOTLIBDIR"; # location of libsprt39.so

    print WRAP "setpath() {\n";	# setpath

    # [PM] 3.8.7 set TCL/TK_LIBRARY unless already set.
    if ($configs{"TCL_LIBRARY"}) {
        print WRAP <<EOF;
   if [ "x\$TCL_LIBRARY" = "x" ] ; then
       TCL_LIBRARY='$configs{"TCL_LIBRARY"}'
       export TCL_LIBRARY
   fi
EOF
    }                           # $configs{"TCL_LIBRARY"}

    if ($configs{"TK_LIBRARY"}) {
        print WRAP <<EOF;
   if [ "x\$TK_LIBRARY" = "x" ] ; then
       TK_LIBRARY='$configs{"TK_LIBRARY"}'
       export TK_LIBRARY
   fi
EOF
    }                           # $configs{"TK_LIBRARY"}

    print WRAP <<EOF;
   SP_APP_DIR="\`dirname \$argv_0\`"
   export SP_APP_DIR
   if [ -d "\$SP_APP_DIR" ] ; then
      # Get an absolute path
      SP_APP_DIR=\`(cd "\$SP_APP_DIR" && pwd)\`
   fi
EOF
    print WRAP "   sp_rtlpath=\"$sp_rtl_path\"\n";
    if ($opt_moveable) {
      print WRAP <<EOF;
   sp_exe_dir=\"\$SP_APP_DIR\"
   if test "/" = "\$sp_exe_dir"; then
      sp_exe_dir="/."
   fi
   if test -d "\$sp_exe_dir/" . $configs{"SP_RTSYS_DIRNAME"}; then
      sp_rtlpath="\$sp_exe_dir/" . $configs{"SP_RTSYS_DIRNAME"}
   fi
EOF
    }                           # $opt_moveable

    # [PM] 3.9.1 FIXME clean this up
    print WRAP <<EOF;

   SP_RT_DIR="\$sp_rtlpath"
   if [ -d "\$SP_RT_DIR" ] ; then
      # Get an absolute path
      SP_RT_DIR=\`(cd "\$SP_RT_DIR" && pwd)\`
   fi
   export SP_RT_DIR

EOF

   # [PM] 3.9.1 now only set the appropriate variable
   my $rtlpath=$configs{"LD_LIBRARY_PATH_name"};
   # [PM] 3.8.7 add to existing path if if was non-empty
   print WRAP <<EOF;
   if [ "x\$$rtlpath" != "x" ] ; then
       $rtlpath="\$sp_rtlpath:\$$rtlpath"
   else
       $rtlpath="\$sp_rtlpath"
   fi
   export $rtlpath
EOF

    print WRAP "}\n";            # setpath

    print WRAP "\nbase=\$argv_0.exe\n";

    if (1                     # [PM] 3.9.1 always to this (especially if --moveable and SPLD_OPT_MOVEABLE_NEEDS_WRAPPER)
        || (not $opt_moveable)
       ) {
	print WRAP "setpath\n";
    }

   # 3.9b5 Pass on "\$@" directly instead. Was:
   #    # [PM] BROKEN
   #    # Probably better break for -a and the else below instead of setting 'args' and then
   #    # use 'exec \$base "$@"' instead of 'exec \$base \$args'
   #    # The difference is that this requires all wrapper args to go first, a reasonable requirement.
   #  elif [ -a = \$1 ] ; then       # Do not interpret after -a
   #    args="\$args \$@"
   #    break;
   #  else
   #    args="\$args \$1"

    print WRAP <<EOF;
while [ "" != "\$1" ] ; do
  if [ -base = \$1 ] ; then
    base=\$2;
    shift;
  elif [ -gdb = \$1 ] ; then
    base="gdb \$base"
  elif [ -setpath = \$1 ] ; then
    setpath;
  # elif [ -a = \$1 ] ; then       # Do not interpret after -a (subsumed by else case below)
  #   break;
  else                          # unknown option, pass it and all that comes after to \$base
    break;
  fi
  shift
done
# [PM] 3.9b5 pass on "\$@" directly
# exec \$base \$args
exec \$base "\$@"
EOF

   { verbose("% Wrapper script is $opt_output and binary is $opt_output.exe\n"); }
}


# [PM] 4.0.2+ now called by END block
# cleanup_temps();


