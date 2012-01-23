/*    ____          __                   __                            *\
**   / __/_______ _/ /___ ____ ___ ___  / /                            **
**  _\ \ / __/ _ `/ // _ `/ _ | _ | _ \/ /   Memory Pool API           **
** /___/ \__/\_,_/_/ \_,_/ .__|___|___/_/    github.com/axel22/mempool **
\*                      /_/                  (c) 2011-2012             */

package org.scalapool.bench;



import java.util.ArrayList;



class MultiStackJavaExperiment {
    
    final class Foo {
	int x = 0;
    }
    
    final class Worker extends Thread {
	Foo[] array = new Foo[1024];
	int sz;
	
	public Worker(int _sz) {
	    sz = _sz;
	}
	
	public void run() {
	    //Foo[] arr = new Foo[1024];
	    Foo[] arr = array;
	    loop(arr);
	}
	
	public void loop(Foo[] arr) {
	    int i = 0;
	    int pos = 512;
	    Foo v = new Foo();
	    while (i < sz) {
	    	if (i % 2 == 0) {
	    	    arr[pos] = v;
	    	    pos += 1;
	    	} else {
	    	    pos -= 1;
	    	    v = arr[pos];
	    	}
	    	i++;
	    }
	}
	
	public void loop2(Foo[] arr) {
            int i = 0;
            int pos = 512;
            Foo v = new Foo();
            while (i < sz) {
                if (i % 2 == 0) {
                    arr[pos >>> 1] = v;
                    pos += 64;
                } else {
                    v = arr[(pos - 64) >>> 1];
                }
                i++;
                if (pos > arr.length)
                    pos = 512;
            }
	}
    }
    
    public static void main(String[] args) {
	(new MultiStackJavaExperiment()).mainMethod(args);
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
	    threads.add(new Worker(sz));
	    threads.get(i).start();
	}
	for (int i = 0; i < par; i++) {
	    try {
		threads.get(i).join();
	    } catch (Exception e) {
	    }
	}
    }
    
}
