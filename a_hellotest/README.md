Erlang Common Test: Minimal Test
================================
*a_hellotest*

Common Tests come in 'Suites', which are groups of tests.

This sample is the simplest possible Common Test Suite, with only one test, which has only one test case, which does basically nothing.

The test just returns "ok" to itself. It has no subject file that it runs etc.

-----------------------------------------------------------------------
Note: this doc is in markdown, with HTML mixed in to show what you should see. Either you really do the samples steps yourself as proposed. Then you won't need the HTML. Otherwise, this doc is probably best viewed online at https://github.com/Eonblast/erlang-arcana/tree/master/a_hellotest
-----------------------------------------------------------------------


A Minimal Test
--------------

The test suite is in file `test/hello_SUITE.erl`, the source is basically:
      
      all() -> [my_test_case].
    
      my_test_case(_Config) -> ok.

That's really the core of it.

The all() function is mandatory and is expected to return a list of function names, as atoms, that comprise the suite. The individual functions are called **test cases**.

It can mix group definitions into the return list, but we'll ignore that for now.

The function my_test_case() must take a configuration parameter. We ignore that for now.

Any test function must basically return ok or crash. 

It can also return an explanation why it prefers to skip the current run {skip, Reason} or some more advanced stuff. But returning ok or crashing is really the essence of it. **Specifically, if something goes wrong, you must never only print and error and return. You must crash!**

This is what makes Common Test a typical Erlang thing: in effect, every match using '=' can be a test. And it will crash if the match fails. Without the Common Test framework, this is still good practice when trying things in Erlang. It is the Erlang way of writing assertions.

This is merely formalized (big time) with Common Tests, and put into suits, much as with Unit Tests.

If you look at the file `test/hello_SUITE.erl` you'll find some overhead, and explanations:
	
	-module(hello_SUITE).
	-compile(export_all).
	-include_lib("common_test/include/ct.hrl").
	
	%%--------------------------------------------------------------------
	%% Function: all() -> GroupsAndTestCases | {skip,Reason}
	%% GroupsAndTestCases = [{group,GroupName} | TestCase]
	%% GroupName = atom()
	%% TestCase = atom()
	%% Reason = term()
	%%--------------------------------------------------------------------
	
	all() -> 
		[my_test_case].
	
	%%--------------------------------------------------------------------
	%% Function: TestCase(Config0) ->
	%%               ok | exit() | {skip,Reason} | {comment,Comment} |
	%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
	%% Config0 = Config1 = [tuple()]
	%% Reason = term()
	%% Comment = term()
	%%--------------------------------------------------------------------
	
	my_test_case(_Config) -> 
		ok.

The comments are taken from the Erlang docs.

Pathes
------

Use this first sample to make sure `ct_run` and `common_test/include/ct.hrl` are in your pathes. I.e. you should be able to run `./ct_run` from your command line and include `-include_lib("common_test/include/ct.hrl").` with no problem. Following the steps below, you'll find out.

This should work out of the box after Erlang is installed. Sometimes it does not and you may find help here:

http://www.erlang.org/doc/apps/common_test/run_test_chapter.html


Running the Test
----------------

You run the test like this:

      ct_run -dir test

This is what you should see:

    machine:~/erlang-arcana/a_hellotest me$   ct_run -dir test
    Erlang R14B01 (erts-5.8.2) [source] [rq:1] [async-threads:0] [hipe] [kernel-poll:false]
    
    
    
    
    Common Test v1.5.2 starting (cwd is /Users/me/erlang-arcana/a_hellotest)
    
    Eshell V5.8.2  (abort with ^G)
    (ct@machine)1> 
    Common Test: Running make in test directories...
    
    CWD set to: "/Users/me/erlang-arcana/a_hellotest/ct_run.ct@machine.2011-12-12_17.31.58"
    
    TEST INFO: 1 test(s), 1 case(s) in 1 suite(s)
    
    Testing erlang-arcana.a_hellotest: Starting test, 1 test cases
    Testing erlang-arcana.a_hellotest: TEST COMPLETE, 1 ok, 0 failed of 1 test cases 
    
    Updating /Users/me/erlang-arcana/a_hellotest/index.html... done
    Updating /Users/me/erlang-arcana/a_hellotest/all_runs.html... done
    machine:~/erlang-arcana/a_hellotest me$ 

Test Results
------------

But the command line output is not considered the 'result' of the test. That is much more verbose and structured: check out the actual test output by opening, with a browser:

      ./index.html

**The output of Common Tests are multiple HTML files that document the test run, in detail. The most recent test run can be inspected by opening the file `./index.html.`**

The index.html, and others, are created each time you run the test.

Just take a look yourself, it's self explanatory. This is what you should see:

<div class=screen style="border:2px solid black; margin: 10px;">
<center>
<H1>Test Results</H1>
</center>
<BR>
<center>

<font color=blue>All test runs in "a_hellotest"</font>
<br><br>
<table border="3" cellpadding="5" BGCOLOR="#E4F0FE">
<tr>
<th>Test Name</th>
<th>Label</th>
<th>Test Run Started</th>
<th><font color="#E4F0FE">_</font>Ok<font color="#E4F0FE">_</font></th>
<th>Failed</th>
<th>Skipped<br>(User/Auto)</th>

<th>Missing<br>Suites</th>
<th>Node</th>
<th>CT Log</th>
<th>Old Runs</th>
</tr>
<TR valign=top>
<TD><FONT SIZE=-1><A HREF="ct_run.ct@machine.2011-12-12_17.58.51/erlang-arcana.a_hellotest.logs/run.2011-12-12_17.58.52/suite.log.html">erlang-arcana.a_hellotest</A></FONT></TD>
<TD ALIGN=center><FONT SIZE=-1><B>-</FONT></B></TD>
<TD><FONT SIZE=-1>Mon Dec 12 2011 17:58:51</FONT></TD>

<TD ALIGN=right>1</TD>
<TD ALIGN=right>0</TD>
<TD ALIGN=right>0 (0/0)</TD>
<TD ALIGN=right>0</TD>
<TD ALIGN=right><FONT SIZE=-1>ct@machine</FONT></TD>
<TD><FONT SIZE=-1><A HREF="ct_run.ct@machine.2011-12-12_17.58.51/ctlog.html">CT Log</A></FONT></TD>
<TD><FONT SIZE=-1>none</FONT></TD>
</TR>
<TR valign=top>
<TD><B>Total</B></TD><TD>&nbsp;</TD>

<TD>&nbsp;</TD>
<TD ALIGN=right><B>1<B></TD>
<TD ALIGN=right><B>0<B></TD>
<TD ALIGN=right>0 (0/0)</TD>
<TD ALIGN=right><B>0<B></TD>
<TD>&nbsp;</TD>
<TD>&nbsp;</TD>
</TR>
</TABLE>
</center>
<P><center>
<BR><BR>
<HR>

<P><FONT SIZE=-1>
Copyright &copy; 2011 <A HREF="http://erlang.ericsson.se">Open Telecom Platform</A><BR>
Updated: <!date>Mon Dec 12 2011 17:58:53<!/date><BR>
</FONT>
</center>
</div>



For all tests (in our example, only one), this index.html lists:

* **Test Name** (e.g. ct-samples.a_hellotest)
* **Label** (tests can be given names, we don't use this yet)
* **Test Run Started** (a time)
* **Ok** (a count of passed tests)
* **Failed** (a count of failed tests)
* **Skipped** (User/Auto) (counts of tests that have been skipped, and why)
* **Missing Suites** (a count of test suites that could not be located)
* **Node** (node name of the test run)
* **CT Log** (log of this test, see below)
* **Old Runs** (link to a list of previous test runs' output)

Digging Deeper
--------------

'Test Name' and 'CT Log' are links that you can use to dig deeper:

* **Test Log** (comes up when clicking on 'Test Name') shows results for each test case
* **Source code** (link in 'Test Log' screen) shows the actual code used per test case
* **CT Log** shows test run configuration and process output, per test run

This gives you comprehensive information to detect the causes of failures across test runs, i.e. comparing test results of before and after a code change.

Click through the links yourself. You'll find stuff like this:

<div class=screen style="border:2px solid black; margin: 10px;">
<H2>Results from test "erlang-arcana.a_hellotest"</H2>
Test started at 2011-12-12 17:31:59<p>Host:<br>
Run by hd on Paulchen<br>Used Erlang 5.8.2 in <tt>/opt/local/lib/erlang</tt>.
<p><a href="suite.log">Full textual log</a>
<br><a href="cover.html">Coverage log</a>
<p>Suite contains 1 test cases.
<p>
<table bgcolor="white" border="3" cellpadding="5"><tr><th>Num</th><th>Module</th><th>Case</th><th>Log</th><th>Time</th><th>Result</th><th>Comment</th></tr>
<tr valign=top><td><font color="black">1</font></td><td><font color="black">hello_SUITE</font></td><td><a href="hello_suite.my_test_case.html">my_test_case</a></td><td><a href="hello_suite.my_test_case.html#top"><</a> <a href="hello_suite.my_test_case.html#end">></a></td><td><font color="black">0.000s</font></td><td><font color="green">Ok</font></td></tr>
<tr><td></td><td><b>TOTAL</b></td><td></td><td></td><td>0.537s</td><td><b>Ok</b></td><td>1 Ok, 0 Failed of 1</td></tr>
</table>
 </div>

## Auto Archving

**A new test run does not overwrite a previous test's output.**

Each run creates a subdirectory with HTML files of the form `ct_run.ct@<host>.<time>` e.g. `ct_run.ct@machine.2011-12-12_15.08.31`

A list of all test runs, linking to all results is found in `./all_runs.html` a link to that page is also found at the very top of the `./index.html`


## Test Script

There is a convenience shell script in this folder that also runs the tests:

      ./test.sh

This script is present in all folders, each time running the main thing that the folder is about. In this case:

      ct_run -dir test

## Cleaning

If you run the tests a couple of times, you'll get an archive of test results, index.html, all_runs.html and a couple of subdirectories.

To start over, clean the directory using 

      ./clean.sh
      

## Next

The next tutorial step is in folder `../b_hellotest2`.
Online at https://github.com/Eonblast/erlang-arcana/tree/master/b_hellotest2

While this test was a minimal suite, the next one is a full skeleton with stubs for all elements that a full fledged test suite would see.


-----------------------------------------------------------------------

If you want to add to this very text, please use github preview:
http://github.github.com/github-flavored-markdown/preview.html

-----------------------------------------------------------------------
Erlang Arcana Tutorial
&copy; 2011 Eonblast Corporation http://www.eonblast.com. MIT Open Source License.
