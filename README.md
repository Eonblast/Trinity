EonBeam - Dev Tutorials 3: OTP
==============================

**| Solar Tribes: Solar Pirates (Tier 2)                   |**                                
**| Eonbeam Comet Data Server                              |**                           
**| (c) 2010-11 Eonblast Corporation. All Rights Reserved. |**                   
**| Not for publication.                                   |**

-----------------------------------------------------------------------

**3**: Tutorial samples combining Common Test, Gen Server, Supervisors

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


These folders contain samples, building on top of each other, 
in alphabetical order. They demonstrate the Erlang OTP

* Common Tests
* Gen Servers
* Supervisors

Common Tests are better suited to test entire systems than Unit Tests.
Generally, Common Test basically checks if a process crashes, or not. The means
to crash it, or not, are simple pattern matches by '='.

Gen Servers are the OTP way of programming servers, using the OTP behaviour
'supervisor' and herding the functional parts into a call back module that
is essentially programmed in sequential tasks that are guaranteed to be
run sequential within one gen server.

Supervisors are the central OTP building blocks of program stability and
the Erlang way of programming defensive: they complement 'let it crash' by
a very structured, hierarchical way to react to those crashes. Supervisors
do not do anything, so they do not ever crash, except if they can't restart
their worker childs.

hd/11