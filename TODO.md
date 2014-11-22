optimization
===================


Let's consider an example 
```
r = (A+B)*(AB)*
```
Thus the set of all possible PDs is 
```
{
 (A+B)*(AB)*,   -- (1)
 B(AB)*,        -- (2)
 (AB)*,         -- (3)
}
```
And the delta are
```
(1) --A--> (1)
(1) --A--> (2)
(1) --B--> (1)
(2) --B--> (3)
(3) --A--> (2)
```

Let's consider the matching problem of matching "ABAB" with r


```
     A       B       A       B
(1) --> (1) --> (1) --> (1) --> (1)
    \-> (2)         \-> (2)
            \-> (3) /       \-> (3)

```
For each step, we need to look up the next possible states, given the source state and the transition label.
After the lookup, we need to remove the duplicating/conflicting states. e.g. 
At the second A, we have a pair of conflicting transition
namely 
```
(1),A, (2)
``` 
a   nd 
```
(3),A, (2)
```
The second one is discarded thanks to the PCRE matching policy.


before we start looking into parallizing the above, let's consider using DFA instead of NFA.
Converting an NFA into a DFA can be done using a bitmap to encode the possible matching states based on absence/presence of each NFA state (0 : absence, 1 presence), the above matching derivation can be visualized as 
```
     A       B       A       B
(1) --> (1) --> (1) --> (1) --> (1)
(2) \-> (2)     (2) \-> (2)     (2)
(3)     (3) \-> (3) /   (3) \-> (3)

```
or

```
       A     B     A     B
(1) 1 --> 1 --> 1 --> 1 --> 1
(2) 0 \-> 1     0 \-> 1     0
(3) 0     0 \-> 1 /   0 \-> 1

```
In theory, there will be 2^n DFA states (max) given n is the number of NFA states.
Let's consider the practicality of this approach. Suppose we only consider alphabet, e.g. |\sigma| = 256 = 2^8
We will precompute all possible NFA transitions, (s, l, t), where s is the source NFA state and, t is the destination 
NFA state, l is the label. We use an IntMap to capture the transition. Suppose we have a 64bit machine, we use the label l as part of the key. Hence we have 64-8 = 56 bit to encode the possible NFA states. Hence this approach has a limitation of only allowing 56 NFA states.  On the other hand, the lookup and the duplication are computed during "compile" time. 

(PS: the current version of LeftToRightD is a bit similar to this, but the benchmark result is still not good. There are still some unevaluated compilation time thunk escape to runtime.)



Let's consider a paralell implementation. 



Recall that our running example

```
     A       B       A       B
(1) --> (1) --> (1) --> (1) --> (1)
    \-> (2)         \-> (2)
            \-> (3) /       \-> (3)

```
Each transition requires a hash table lookup using the source states and the input label as the compound keys. As highlighted earlier, the output set containing all possible target states may contain duplicate (i.e. conflicting) states. A deduplication step is required. 


There are a few possible ways of breaking this task into sub tasks.

1. horizontally splitting "ABAB" into "AB" and "AB" (like chunking and parmap)
However, the computation is more like a fold rather than a map. Not sure whether it is possible.

2. apply parMap for each lookup and dedup?, e.g. in consuming the first label B. Instead of 
```
map (\state -> lookup state 'B' hashTable) [1,2]
```
we do 
```
parMap (\state -> lookup state 'B' hashTable) [1,2]
```
However this leads to too many sparks created and 90% of them are garbage collected. See LeftToRightP3 for details

SPARKS: 34788298 (152093 converted, 0 overflowed, 0 dud, 34469817 GC'd, 166388 fizzled)

3. vertically split the task if there are more than one source NFA states. If we do this naively, we will have exponential growth of the sparks, as we do not do any deduplication on the output target states. In LeftToRightP2, we try to only split once and not split subsequently. However there is no performance gain as 90% of the spark are GC'ed.

 SPARKS: 634836 (13585 converted, 0 overflowed, 0 dud, 605166 GC'd, 16085 fizzled)

4. vertically split the task if there are more than one source NFA states. The split subtask/spark are run in paralell in a fixed steps. After all steps, the result will be merged and deduped. If there are still input labels remains, split again. As in LeftToRightP, there is no performance gain as 90% of the spark are GC'ed.

  SPARKS: 1858941 (12086 converted, 0 overflowed, 0 dud, 1830321 GC'd, 16534 fizzled)

5. vertically split the task into 3 actors, each actors in charge of one NFA state, this will distribute the lookup and the duplicate steps. However this has a potential blow up of the number of actors. We haven't tried to implement this approach yet.



LATEST,

In LeftToRightP4, we fixed the problem of huge GC time. The problem
was contributed by the use of list of Binder update functions. Hence
we adopt a more aggressive strategy, the Binders are updated along the
transitions take place. Cf patMatchesIntStatePdPat0 vs
patMatchesIntStatePdPat0'. In addition, we move the par to the
lookupPdPat0 where the "pairing" operation take place. The "pairing"
operation pairs up the output NFA state with the resulting Binder environment.

Now the bottle neck is gone, however there is still no speed up on e.g.

matching ABABABAB... with "^(A|B)*(AB)*$"

RTS reports

SIT-NYPs-MBP-2:par_example luzm$ time ./Eg1P4 +RTS -s -N2 -g1 -lf > /dev/null
   5,923,085,856 bytes allocated in the heap
         420,648 bytes copied during GC
          69,920 bytes maximum residency (3 sample(s))
          42,440 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10424 colls,     0 par    0.34s    0.43s     0.0000s    0.0054s
  Gen  1         3 colls,     0 par    0.00s    0.01s     0.0023s    0.0065s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 5000004 (719374 converted, 0 overflowed, 0 dud, 3827985 GC'd, 452645 fizzled)

  INIT    time    0.00s  (  0.05s elapsed)
  MUT     time    2.21s  (  2.10s elapsed)
  GC      time    0.34s  (  0.44s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    2.55s  (  2.58s elapsed)

  Alloc rate    2,683,795,581 bytes per MUT second

  Productivity  86.5% of total user, 85.6% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

real	0m2.752s
user	0m2.554s
sys	0m0.323s


Threadscope shows

uneven spark task allocation, i.e. the main thread is still taking a
long time

In LeftToRightP6, we run the NFA transition in parallel for N steps if
there are more than one source states. After the N steps of parallel
executes, the states are merged. This shows that we have more
converted sparks and no GC sparks. There are still some frizzle
sparks.

It is almost the same as the version w/o parallel execution, and
faster than LeftToRightP4


IT-NYPs-MBP-2:par_example luzm$ time ./Eg1P6 +RTS -s -N2   | less

real	0m6.665s
user	0m2.040s
sys	0m0.174s
SIT-NYPs-MBP-2:par_example luzm$ time ./Eg1P6 +RTS -s -N2  > /dev/null
   7,204,042,624 bytes allocated in the heap
      23,411,696 bytes copied during GC
          70,488 bytes maximum residency (4 sample(s))
          43,088 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10512 colls, 10512 par    0.22s    0.13s     0.0000s    0.0003s
  Gen  1         4 colls,     3 par    0.00s    0.00s     0.0002s    0.0003s

  Parallel GC work balance: 5.75% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 80 (40 converted, 0 overflowed, 0 dud, 0 GC'd, 40 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    1.76s  (  1.40s elapsed)
  GC      time    0.22s  (  0.13s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    1.99s  (  1.53s elapsed)

  Alloc rate    4,088,344,158 bytes per MUT second

  Productivity  88.7% of total user, 115.2% of total elapsed

gc_alloc_block_sync: 3174
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

real	0m1.539s
user	0m1.989s
sys	0m0.154s
