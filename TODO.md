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

#1 horizontally splitting "ABAB" into "AB" and "AB" (like chunking and parmap)
However, the computation is more like a fold rather than a map. Not sure whether it is possible.

#2 virtically split the task if there are more than one source NFA states. If we do this naively, we will have exponential growth of the sparks, as we do not do any deduplication on the output target states. In LeftToRightP2, we try to only split once and not split subsequently. However this is no performance gain as 90% of the spark are GC'ed.

#3 virtically split the task into 3 actors, each actors in charge of one NFA state, this will distribute the lookup and the duplicate steps. However this has a potential blow up of the number of actors. We haven't tried to implement this approach yet.

#4 apply parMap for each lookup and dedup, e.g. in consuming the first label B. Instead of 
```
map (\state -> lookup state 'B' hashTable) [1,2]
```
we do 
```
parMap (\state -> lookup state 'B' hashTable) [1,2]
```
However this leads to too many sparks created and 90% of them are garbage collected. See LeftToRightP3

