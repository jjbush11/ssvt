# Recurring Questions

## Problem 4 

### What can cause the after function to return multiple states? Motivate your answer. 
The after function, as described in Tretmans' work on ioco theory, takes a set of states and a trace (a sequence of inputs and outputs) and returns the set of states that the system can be in after processing the trace. Multiple states can be returned by the after function due non-determinism. If the system described by the LTS (Labeled Transition System) is non-deterministic, then a given input or sequence of inputs may lead to more than one possible next state. Non-determinism means that from a certain state, there can be multiple transitions labeled with the same action leading to different states.


### What does it mean for a model and implementation to be input-output conformant (ioco)? Motivate your answer. 
Input-output conformance (ioco) is a formal testing theory that defines when an implementation of a system is considered to conform to its specification (model). Under ioco, an implementation is said to be conformant if, after any sequence of inputs (traces) that the model allows, the implementation's outputs are among those that the model could produce.

### What does a quiescent state indicate? 
A quiescent state in a system, particularly in the context of input-output conformance (ioco) testing and labeled transition systems (LTS), indicates a state where the system is stable and cannot produce any outputs on its own without receiving further inputs. In other words, when a system is in a quiescent state, it is essentially waiting for an external action (like an input) to proceed.

In ioco testing, quiescence is important because it is treated as an observable behavior. It is typically denoted by δ (delta) and is used to represent the absence of outputs. This allows testers to verify not only the active responses of a system (like producing outputs) but also its passive behaviors (like waiting or not responding immediately) against the specified model.

### What is the difference between the traces and straces functions?

In the context of the article discussing input-output conformance (ioco) and labeled transition systems (LTS), the difference between traces and straces functions is:

Traces: The traces function generates all possible sequences of labels (inputs and outputs) that can be observed from interacting with a system, starting from the initial state. It does not include quiescence (the absence of output or waiting state) in its output.

Straces (Suspension Traces): The straces function, on the other hand, extends the concept of traces by including quiescence (denoted as δ). It generates sequences of labels along with the possible occurrences of quiescence. This means that straces captures not only the active behavior of the system (like traces) but also its passive states where it is waiting for inputs without producing outputs.

The inclusion of quiescence (δ) in straces (suspension traces) does indeed mean that straces can potentially be infinitely larger than traces.

Traces only include observable sequences of inputs and outputs. The length of each trace is finite and bounded by the number of transitions defined in the system.

Straces, by incorporating quiescence (δ), can lead to an infinite number of suspension traces. This is because quiescence can theoretically occur at any point in a trace and repeatedly so. For instance, a system could remain quiescent indefinitely after any sequence of inputs and outputs, leading to an infinite expansion of possible suspension traces.

Therefore, while traces provide a finite set of possible behaviors, straces extend this to potentially infinite behaviors due to the repeated and unbounded inclusion of quiescent states. This aspect is particularly significant in theoretical and practical considerations of ioco testing and modeling system behaviors.

## If two models have equivalent traces, to what extent does that entail that the models are the same? Motivate your answer.
Potential answer 1: 
Equivalent traces between two models indicate that the two models have the same observable behaviors. This means that the two models produce the same outputs for the same inputs. However, equivalent traces do not necessarily mean that the two models are the same. This is because equivalent traces do not capture the internal states of the models. Two models can have the same observable behaviors but different internal states. This means that the two models can be different in terms of their internal structure and the way they process inputs to produce outputs. Therefore, equivalent traces do not entail that the models are the same.

Potential answer 2:
If two models have equivalent traces, it means they exhibit the same sequences of observable interactions (inputs and outputs) when starting from their initial states. However, having equivalent traces does not necessarily entail that the models are the same in all aspects. Here's why:

Internal Structure and State Space: While both models respond similarly to the same sequences of inputs, their internal state structures might be different. They could have different numbers of states, different state representations, or different internal transitions (like τ-transitions in LTS).

Non-observable Behaviors: Equivalent traces only reflect observable behaviors. The models might differ in non-observable aspects, such as internal computations, efficiency, memory usage, or other performance characteristics.

Quiescence and Non-functional Properties: Traces do not include quiescent states or non-functional properties (like reliability, scalability, etc.). Two models might react identically to the same inputs but differ in how they handle idle states or in their non-functional behaviors.

In summary, while equivalent traces indicate that models behave identically in terms of observable inputs and outputs, they do not capture the entirety of the models' characteristics. Differences in internal structures, non-observable behaviors, and non-functional properties are not represented in traces, meaning models can still be distinct in these aspects despite having equivalent traces.

## Problem 5
### In general mutation testing, what kind of mutants could be considered 'relevant'? 
Mutations which are considered relevant are those which are likely to cause a fault in the program. This means that the mutation is likely to cause a change in the program's behavior. For instance, a mutation that changes the program's output is likely to be relevant. On the other hand, a mutation which does not change the program's behavior because it is equivalent to the original program is not relevant.

### Consider commit-aware mutation testing. What properties should 'relevant' mutations have in context? 

The definition of commit-relevant mutants; a set of mutants that affect the changed program behaviours and represent the commit-relevant test requirements.
So the relevant mutants are those that affect the changed program behaviours and not any other behaviours.