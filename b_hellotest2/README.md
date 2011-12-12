Erlang Common Test: Complete Test Suite Skeleton
================================================
*b_hellotest2*

This test does exactly the same as the previous one, *a_hellotest*: nothing.

As before, this test just returns "ok" to itself. It has no subject file that it runs etc.

This time though, the source in `test/hello_SUITE.erl` has stub functions for all the things
that a test suite can do:

* **all()** - the only mandatory function: test start
* **suite()** - optional adjustment for settings for the suite
* **init_per_suite(Config)** - optional initialization run before the suite starts
* **end_per_suite(_Config)** - optional wind down 
* **init_per_group(_GroupName, Config)** - optional initialization run before a group of test cases is started
* **end_per_group(_GroupName, _Config)** - optional wind down
* **init_per_testcase(_TestCase, Config)** - optional initialization run before a test case is started
* **end_per_testcase(_TestCase, _Config)** - optional wind down
* **groups()** - definition of test case groups

But the functional part is still only:
      
      all() -> [my_test_case].
      my_test_case(_Config) -> ok.

## The Skeleton

**All control functions of a test suite file.**

This is the list of all functions that you can have in a test suite file that the Common Test system will recognize and use to set the configuration according to your needs; blanket, or in most precise granulity.

The following description is not complete but aims to give an idea about what exists and what options you have. There are links right into the Erlang docs provided that will provide you with the details.
    
The page to start going deeper in the Erlang online docs is:
http://www.erlang.org/doc/apps/common_test/write_test_chapter.html
    
### Optional Suite 'Info' Function

**Configuration of the suite.**

The suite() function, if present, is expected to return settings for the test suite.

    %%--------------------------------------------------------------------
    %% Function: suite() -> Info
    %% Info = [tuple()]
    %%--------------------------------------------------------------------
    
    suite() ->
        [{timetrap,{seconds,30}}].
        
It can return these settings:

* timetrap, see http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#timetraps
* require flags, see http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#id208674 
* stylesheet, see http://www.erlang.org/doc/apps/common_test/run_test_chapter.html#html_stylesheet
* userdata, see http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#info_function
* silent_connections, see http://www.erlang.org/doc/apps/common_test/run_test_chapter.html#silent_connections

These suite settings can be overwritten by the per test case info-functions, see immediately below:

### Optional Test 'Info' Function

**Configuration of one test case.**

For each test case function there can be an additional function with the same name but with no arguments. This is the test case info function. The test case info function is expected to return a list of tagged tuples, like the suite() function and overwrites settings of the suite() function.
    
See: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#id208674


    %% Optional test case setup: same name as test case but arity 0. 
    %%--------------------------------------------------------------------
    %% Function: TestCase() -> Info
    %% Info = [tuple()]
    %%--------------------------------------------------------------------

    my_test_case() -> 
        [].


### Group Definition

**Assigning test cases to groups and set group-wide configuration.**

This function is optional and if present, returns a list of group names and per-group settings.

    %%--------------------------------------------------------------------
    %% Function: groups() -> [Group]
    %% Group = {GroupName,Properties,GroupsAndTestCases}
    %% GroupName = atom()
    %% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
    %% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
    %% TestCase = atom()
    %% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
    %% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
    %%              repeat_until_any_ok | repeat_until_any_fail
    %% N = integer() | forever
    %%--------------------------------------------------------------------
    
    groups() ->
        [].

This function is used to group individual test cases into one group.
GroupName should be unique within the suite module.

Groups can be nested, executed in parallel, sequentially, in random order,
repeatedly, or skipped. And groups can share one initialization function, 
see below.

Example for the definition of a group:

    groups() -> [{group1, [parallel], [test1a,test1b]},
                       {group2, [shuffle,sequence], [test2a,test2b,test2c]}].

To specify in which order groups should be executed (also with respect to test cases that are not part of any group), tuples on the form {group,GroupName} should be added to the all/0 list. Example:

    all() -> [testcase1, {group,group1}, testcase2, {group,group2}].

See: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#id212746

### Optional per suite initialization

**Preparing and winding down, per suite.**

The function `init_per_suite()` is executed once before the suite starts.

Straight from the Erlang docs:

It typically contains initializations that are common for all test cases in the suite, and that are only to be performed once. It is recommended to be used for setting up and verifying state and environment on the SUT (System Under Test) and/or the CT host node, so that the test cases in the suite will execute correctly. Examples of initial configuration operations: Opening a connection to the SUT, initializing a database, running an installation script, etc.

end_per_suite is called as the final stage of the test suite execution (after the last test case has finished). The function is meant to be used for cleaning up after init_per_suite.

The argument to init_per_suite is Config, the same key-value list of runtime configuration data that each test case takes as input argument. init_per_suite can modify this parameter with information that the test cases need. 

If init_per_suite fails, all test cases in the test suite will be skipped automatically (so called auto skipped), including end_per_suite. 

If `init_per_suite()` exists, there must also be a `end_per_suite()` function.

Also see http://www.erlang.org/doc/apps/common_test/write_test_chapter.html#id209706


    %%--------------------------------------------------------------------
    %% Function: init_per_suite(Config0) ->
    %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
    %% Config0 = Config1 = [tuple()]
    %% Reason = term()
    %%--------------------------------------------------------------------
        
    init_per_suite(Config) ->
        Config.


    %%--------------------------------------------------------------------
    %% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
    %% Config0 = Config1 = [tuple()]
    %%--------------------------------------------------------------------
        
    end_per_suite(_Config) ->
        ok.

### Optional Group initialization

**Preparing and winding down, per group.**

You can initialize, and wind down, per group, essentially the same as with `init_per_suite`:
    
    %%--------------------------------------------------------------------
    %% Function: init_per_group(GroupName, Config0) ->
    %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
    %% GroupName = atom()
    %% Config0 = Config1 = [tuple()]
    %% Reason = term()
    %%--------------------------------------------------------------------
    
    init_per_group(_GroupName, Config) ->
        Config.


    %%--------------------------------------------------------------------
    %% Function: end_per_group(GroupName, Config0) ->
    %%               void() | {save_config,Config1}
    %% GroupName = atom()
    %% Config0 = Config1 = [tuple()]
    %%--------------------------------------------------------------------
    
    end_per_group(_GroupName, _Config) ->
        ok.
    
### Optional Test Case initialization

**Preparing and winding down, per case.**

You can initialize and wind down for every test case, the same as
with `init_per_suite`. (A 'case' is one function, e.g. my_test_case())

Note that init_per_testcase() will be executed also if a test case belongs
to a group. So there can be three init functions lined up to be executed
before the actual test: suite, group, case.
    
    %% Optional case-wise pre test initialization (a case is a function call)
    %%--------------------------------------------------------------------
    %% Function: init_per_testcase(TestCase, Config0) ->
    %%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
    %% TestCase = atom()
    %% Config0 = Config1 = [tuple()]
    %% Reason = term()
    %%--------------------------------------------------------------------
    
    init_per_testcase(_TestCase, Config) ->
        Config.


    %% Optional case-wise post test wind down
    %%--------------------------------------------------------------------
    %% Function: end_per_testcase(TestCase, Config0) ->
    %%               void() | {save_config,Config1} | {fail,Reason}
    %% TestCase = atom()
    %% Config0 = Config1 = [tuple()]
    %% Reason = term()
    %%--------------------------------------------------------------------
    
    end_per_testcase(_TestCase, _Config) ->
        ok.
    

### Mandatory suite start
    
    %% Mandatory list of test cases and test groups, and skip orders. 
    %%--------------------------------------------------------------------
    %% Function: all() -> GroupsAndTestCases | {skip,Reason}
    %% GroupsAndTestCases = [{group,GroupName} | TestCase]
    %% GroupName = atom()
    %% TestCase = atom()
    %% Reason = term()
    %%--------------------------------------------------------------------
    
    all() -> 
        [my_test_case].
    
### Sample Test Case 

    %% A test case. The ok is irrelevant. What matters is, if it returns.
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


## Running

Runs the same as *a_hellotest*:

      ./test.sh

Or, as before, use:

      ct_run -dir test

## Results

See a discussion of the test results at *a_hellotest*,
online at https://github.com/Eonblast/erlang-arcana/tree/master/a_hellotest
      
## Cleaning

If you run the tests a couple of times, you'll get an archive of test results, index.html, all_runs.html and a couple of subdirectories.

To start over, clean the directory using 

      ./clean.sh
      

## Next

The next tutorial step is in folder `../c_hellotest3`.
Online at https://github.com/Eonblast/erlang-arcana/tree/master/c_hellotest3

-----------------------------------------------------------------------

If you want to add to this very text, please use github preview:
http://github.github.com/github-flavored-markdown/preview.html

-----------------------------------------------------------------------
Erlang Arcana Tutorial
&copy; 2011 Eonblast Corporation http://www.eonblast.com. MIT Open Source License.
      