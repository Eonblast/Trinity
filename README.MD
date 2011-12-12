Erlang Arcana Tutorial
======================

**Tutorial and samples combining Common Test, Gen Server, Supervisors.**

&copy; 2011 Eonblast Corporation http://www.eonblast.com. MIT Open Source License.

-----------------------------------------------------------------------


This is a beginner's introduction to Erlang/OTP concepts. It is structured 
in folders, with a README.MD in each folder that explains basics and has
instructions of what to look at and what to try with the sample code.

The sample code is supposed to serve as starting point and templates for
your own needs. It is usually commented in a way to help you expand on it.

-----------------------------------------------------------------------

The purpose of this repository is to start out or refresh your knowledge
on the trivial but essential Erlang stuff beyond the language itself.
It is not for learning the Erlang language, but how to use it right.

As such it is more about the OTP part in Erlang/OTP. But it really must be
seen as part of Erlang proper: the Erlang way of doing things like tests,
servers, and in general system development and architecture.

The Erlang language is rather small and fast to pick up despite the fact
that it is 'so different' from the syntax of the Algol heirs: C, Java et al.
And can I recommend Learn You Some Erlang again for the best online resource?

It gets tricky once you get past that first learning phase, into the 'standard
library' area, in the case of Erlang, OTP, the 'Open Telecom Protocol', that
really is not at all restricted to telecom. But its name gives a pointer to 
one problem: it's full of outdated backward compatibilities and low consistency
in naming, parameters etc. That's great for the hard core Erlang community 
but not so great when learning Erlang for real. You need to break into the
'Arcana', the ancient secret arts there. And you need to learn a lot of trivial
stuff that just happens to be as it is. In my experience, it's no fun at all.

It's worth it as the magic really is powerful. It's also *completely inevitable*
to really get going with Erlang, to even *really* understand its power. And
what you can do with it, and how you should go about it.

This repository tries to give a head start into these things. Basically, to
get your head around them and get a better idea what it is all about. And,
quite honestly, to enable you to *then* read the Erlang docs, which are made
with a lot of love but also in circular dependency on knowledge you might not
yet have when starting out.

So as much as possible I am trying to bring some fun back to these harder parts.
I'll be happy to hear your feedback on it. Please send your pull requests.

-----------------------------------------------------------------------

This is what awaits:

Folders
-------
	a_hellotest				minimal Common Test suite, no test subject
	b_hellotest2			complete Common Test skeleton, no test subject
	c_hellotest3			minimal Common Test suite, with no functionality
	d_servertest			Common Test of a minimal client and server 
	e_genserver				Gen Server skeleton, no Common Test
	f_genservertest			same as e but wrapped by a Common Test
	g_supervisor			Supervisor skeleton, no Common Test
	h_supervisortest		combines f and g for a gen server and a supervisor test suite
	i_supervisortest2		same as h but with a dynamic child. look for **


These folders contain samples, building on each other, in alphabetical order.
They demonstrate the Erlang OTP concepts of:

* Common Tests
* Gen Servers
* Supervisors

**Common Tests** are the usual way to create program tests in Erlang. 
Common Tests are better suited to test entire systems than Java-style Unit Tests.
Generally, a Common Test basically checks if a process crashes, or not. The means
to crash it, or not, are simple pattern matches by '='.

**Gen Servers** are the OTP way of programming servers, using the OTP behaviour
'supervisor' and herding the functional parts into a call back module that
is essentially programmed in *sequential* tasks that are guaranteed to be
run sequential within one gen server.

**Supervisors** are the central OTP building blocks of program robustness and
the Erlang way of programming defensively: they *complement 'let it crash'* with
a very structured, hierarchical way to react to those crashes. Supervisors
do not do anything, so they do not ever crash, except if they can't restart
their worker childs at all.

hd/11

License
-------

Copyright (c) 2011 Eonblast Corporation http://www.eonblast.com.

Permission is  hereby  granted,  free of charge,  to any person
obtaining  a copy of this software and associated documentation
files  (the  "Software"),  to  deal  in  the  Software  without 
restriction,  including  without limitation  the rights to use,
copy, modify,  merge,  publish, distribute,  sublicense, and/or 
sell  copies of the  Software,  and to permit  persons  to whom
the  Software  is furnished to do so,  subject to the following 
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
