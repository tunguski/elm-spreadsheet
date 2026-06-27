#!/usr/bin/env perl
#
# gen-test-report.pl <report.json> <out.html>
#
# Turns the elm-test JSON report into a friendly, self-contained HTML page.
#
# The runner emits {"tests":[{"name":"<a> > <b> > <test>","status":"pass"|"fail"}],
# "passed":N,"failed":N,...}. Test names are "›"-separated paths; we drop the top-level
# suite name and group by the next segment. The "›" separator arrives as raw byte 0x9B on
# a CP1252 JVM (Windows) or as UTF-8 (e2 80 ba) on a UTF-8 JVM (CI); both are normalised.
# Test names in this suite contain no double quotes, so a simple regex scan is robust.

use strict;
use warnings;

my ($in, $out) = @ARGV;
die "usage: gen-test-report.pl <report.json> <out.html>\n" unless $in && $out;

open my $fh, '<:raw', $in or die "cannot read $in: $!";
local $/;
my $json = <$fh>;
close $fh;

# Normalise the describe separator (UTF-8 › or CP1252 0x9B) to a unit separator.
$json =~ s/\xe2\x80\xba/\x1f/g;
$json =~ s/\x9b/\x1f/g;

# Collect tests in order, grouped by their second path segment. The runner emits each
# test twice, so we de-duplicate on (group, leaf) and compute the counts ourselves.
my @order;          # group names in first-seen order
my %groups;         # group => [ [leaf, status], ... ]
my %seen;           # "group\x1fleaf" => 1
my $passed = 0;
my $failed = 0;

while ($json =~ /\{"name":"(.*?)","status":"(pass|fail)"\}/g) {
    my ($name, $status) = ($1, $2);
    my @parts = grep { length } map { my $p = $_; $p =~ s/^\s+|\s+$//g; $p } split /\x1f/, $name;
    shift @parts if @parts >= 3;            # drop top-level suite name
    my $group = @parts ? shift @parts : 'General';
    my $leaf  = @parts ? join(' › ', @parts) : $group;
    next if $seen{"$group\x1f$leaf"}++;     # skip the duplicate emission
    if ($status eq 'pass') { $passed++ } else { $failed++ }
    push @order, $group unless exists $groups{$group};
    push @{ $groups{$group} }, [ $leaf, $status ];
}

my $total = $passed + $failed;
my $ok = ($failed == 0);
my $status_word = $ok ? 'All tests passing' : 'Some tests failed';
my $status_cls  = $ok ? 'ok' : 'bad';

sub esc {
    my $s = shift;
    $s =~ s/&/&amp;/g;
    $s =~ s/</&lt;/g;
    $s =~ s/>/&gt;/g;
    return $s;
}

my $sections = '';
for my $group (@order) {
    my @items = @{ $groups{$group} };
    my $gpass = grep { $_->[1] eq 'pass' } @items;
    my $gtot  = scalar @items;
    my $lis = '';
    for my $it (@items) {
        my ($leaf, $status) = @$it;
        my $cls  = $status eq 'pass' ? 'pass' : 'fail';
        my $mark = $status eq 'pass' ? '&#10003;' : '&#10007;';
        $lis .= qq{<li class="t $cls"><span class="mark">$mark</span><span class="leaf">}
              . esc($leaf) . qq{</span></li>};
    }
    my $g = esc($group);
    $sections .= qq{<section class="group"><h2>$g<span class="gcount">$gpass/$gtot</span></h2>}
               . qq{<ul class="tests">$lis</ul></section>};
}

my $html = <<"HTML";
<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>elm-spreadsheet \x{b7} test report</title>
<style>
:root { --ok:#188038; --bad:#d93025; --accent:#1a73e8; --border:#e0e3e9; --muted:#5f6368; }
* { box-sizing:border-box; }
body { margin:0; background:#f4f6fa; color:#202124;
  font-family:-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif; }
.wrap { max-width:900px; margin:0 auto; padding:0 20px 80px; }
.head { text-align:center; padding:52px 0 18px; }
.head h1 { margin:0 0 14px; font-size:2rem; letter-spacing:-.02em; color:var(--accent); }
.summary { display:inline-flex; align-items:center; gap:16px; padding:14px 26px; border-radius:14px;
  background:#fff; border:1px solid var(--border); box-shadow:0 8px 24px rgba(16,24,40,.05); }
.badge { font-weight:800; font-size:1.05rem; padding:6px 14px; border-radius:999px; color:#fff; }
.badge.ok { background:var(--ok); } .badge.bad { background:var(--bad); }
.counts { color:var(--muted); font-size:14px; }
.counts b { color:#202124; }
.back { display:inline-block; margin:18px 0 6px; color:var(--accent); text-decoration:none; font-size:14px; }
.back:hover { text-decoration:underline; }
.group { background:#fff; border:1px solid var(--border); border-radius:14px;
  padding:8px 20px 14px; margin:18px 0; box-shadow:0 1px 3px rgba(16,24,40,.04); }
.group h2 { font-size:15px; margin:14px 2px 8px; display:flex; align-items:center; gap:10px; }
.gcount { font-size:12px; font-weight:600; color:var(--muted); background:#eef1f6;
  border-radius:999px; padding:2px 10px; }
ul.tests { list-style:none; margin:0; padding:0; }
li.t { display:flex; align-items:baseline; gap:10px; padding:5px 6px; border-radius:7px; font-size:13.5px; }
li.t:hover { background:#f7f9fc; }
.mark { font-weight:700; width:16px; flex:none; text-align:center; }
li.pass .mark { color:var(--ok); }
li.fail { background:#fdecea; }
li.fail .mark { color:var(--bad); }
.leaf { color:#2b2f36; }
.foot { text-align:center; color:var(--muted); font-size:12.5px; margin-top:28px; }
</style>
</head>
<body>
<div class="wrap">
  <div class="head">
    <h1>elm-spreadsheet test report</h1>
    <div class="summary">
      <span class="badge $status_cls">$status_word</span>
      <span class="counts"><b>$passed</b> passed \x{b7} <b>$failed</b> failed \x{b7} <b>$total</b> total</span>
    </div>
    <div><a class="back" href="index.html">&larr; back to the examples</a></div>
  </div>
  $sections
  <div class="foot">Generated from the elm-test JSON report \x{b7} the engine is pure, so every check runs headlessly.</div>
</div>
</body>
</html>
HTML

open my $ofh, '>:encoding(UTF-8)', $out or die "cannot write $out: $!";
print $ofh $html;
close $ofh;

print "wrote $out: $passed passed, $failed failed, $total total\n";
