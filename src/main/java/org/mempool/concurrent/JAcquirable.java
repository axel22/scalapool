/*    __  ___                        __                             *\
**   /  |/  /___ __ _  ___ ___ ___  / /                             **
**  / /|_/ // -_)  ' \/ _ | _ | _ \/ /    Memory Pool API           **
** /_/  /_/ \__/_/_/_/ .__|___|___/_/     github.com/axel22/mempool **
\*                  /_/                   (c) 2011-2012             */

package org.mempool.concurrent;



import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;



public class JAcquirable {
    public static AtomicIntegerFieldUpdater<JAcquirable> updater = AtomicIntegerFieldUpdater.newUpdater(JAcquirable.class, "refcount");
    
    protected volatile int refcount = 0;
}




