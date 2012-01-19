/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool.bench;



import java.util.ArrayList;



/** A not easy to explain benchmark.
 */
class MultiVolatileJavaExperiment {
    
    public static void main(String[] args) {
	(new MultiVolatileJavaExperiment()).mainMethod(args);
    }
    
    int size = Integer.parseInt(System.getProperty("size"));
    int par = Integer.parseInt(System.getProperty("par"));
    
    public void mainMethod(String[] args) {
	int times = 0;
	if (args.length == 0) times = 1;
	else times = Integer.parseInt(args[0]);
	ArrayList<Long> measurements = new ArrayList<Long>();
	
	for (int i = 0; i < times; i++) {
	    long start = System.currentTimeMillis();
	    run();
	    long end = System.currentTimeMillis();
	    
	    long time = (end - start);
	    System.out.println(i + ") Running time: " + time + " ms");
	    measurements.add(time);
	}
	
	System.out.println(">>>");
	System.out.println(">>> All running times: " + measurements);
	System.out.println(">>>");
    }
    
    public void run() {
	int sz = size / par;
	ArrayList<Thread> threads = new ArrayList<Thread>();
	
	for (int i = 0; i < par; i++) {
	    threads.add(new Reader(sz));
	    threads.get(i).start();
	}
	for (int i = 0; i < par; i++) {
	    try {
		threads.get(i).join();
	    } catch (Exception e) {
	    }
	}
    }
    
    final class Foo {
	int x = 0;
    }
    
    final class Reader extends Thread {
	volatile Foo vfoo = new Foo();
	Foo bar = null;
	int sz;
	
	public Reader(int _sz) {
	    sz = _sz;
	}
	
	public void run() {
	    int i = 0;
	    while (i < sz) {
		vfoo.x = 1;
		// with the following line commented
		// the scalability is almost linear
		bar = vfoo; // <- makes benchmark 2x slower for 2 processors - why?
		i++;
	    }
	}
    }
    
}
