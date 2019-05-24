---
layout: post
title: "Using cryptography for tamper-proof election results after close of polling"
permalink: /cryptography-election-results
---

Over the years, I've been somewhat dismayed by various reports of tampering of
[EVMs](https://www.eci.gov.in/evm/) after polls have closed. Especially in this
year's Lok Sabha polls the issue received a lot of coverage.

In this post I present an approach, using digital fingerprints, that will render
tampering of EVMs and election results **after close of polling** useless. While
there might be holes in this approach, I still believe there's merit in
discussing this.

1. TOC
{:toc}

## Summary

The solution involves generating and disclosing to the public a digital
fingerprint of the result from each EVM as soon as polls close. Each fingerprint
is a seemingly random string of characters, however they have a couple of highly
desirable properties:

1. A fingerprint is unique to each permutation of the result i.e. two different
   results will practically never have the same fingerprint
2. It is impossible to figure out the election result just by looking at the
   fingerprint

The fingerprints are generated using a [cryptographic hash
function][], specifically [SHA-3][]. In subsequent
sections, we will see how they work. But first, it's very important to
understand what this solution does not solve.

[cryptographic hash function]: https://simple.wikipedia.org/wiki/Cryptographic_hash_function
[SHA-3]: https://en.wikipedia.org/wiki/SHA-3

## What this DOES NOT solve

The solution proposed here **cannot** prevent tampering of EVMs before, or
during, the election. Nor cannot it solve the problem of [booth
capture](https://en.wikipedia.org/wiki/Booth_capturing).

It only focuses on securing one aspect of the polling process, and that is
manipulation of election results after polling closes. In fact, it only works if
EVMs have not been tampered with, and booth capture has not occurred.

After disclosure of the digital fingerprint, which should be done as soon as
polling closes, tampering of EVMs becomes irrelevant as an altered result will
not be able to match the disclosed fingerprint.

## How it works

Next up are some details about how this works. Note that the sections below are
slightly technical. You can skip this and jump to [concluding
thoughts](#concluding-thoughts) for the remaining non-technical conclusions.

### Crytographic hash functions

_(You can skip this section if you already know how they work)_

A [cryptographic hash function][] is a mathematical construct that takes an
input text of any length and mixes its bytes to produce a fixed size
string. This string, also known as a _digest_ or a _hash_, is a digital
fingerprint of the input message.

Examples of such hash functions include MD5, SHA-1, SHA-3, etc.

Some example (SHA-1) hashes are shown below:

```
|----------------------+------------------------------------------|
| Text                 | Hash (SHA-1)                             |
|----------------------+------------------------------------------|
| abracadabra          | 0b8c31dd3a4c1e74b0764d5b510fd5eaac00426c |
|----------------------+------------------------------------------|
| lorem ipsum          | bfb7759a67daeb65410490b4d98bb9da7d1ea2ce |
|----------------------+------------------------------------------|
| the quick brown fox  | ced71fa7235231bed383facfdc41c4ddcc22ecf1 |
|----------------------+------------------------------------------|
| the quick brown fix  | e3a75de65fea42239e26476f6efe110f69932b8f |
|----------------------+------------------------------------------|
| the quick brown fox  | 3e4991b48bcb1bd9d3c4c14a1f24c415deaba466 |
| jumped over the lazy |                                          |
| dog                  |                                          |
|----------------------+------------------------------------------|
```

The important things to know about cryptographic hashes are:

1. It is extremely easy to calculate the hash of any text
2. It is extremely difficult to find a text that has a given hash
3. If you have a text and its hash, it is extremely difficult to find another
   text that has the same hash.

Also, as the third and fourth examples show, even a slight change in text input
usually leads to large changes in the output hash.

So, while it's very easy to calculate the hash of the string "The quick brown
fox jumps over the lazy dog", it is impossible to do the reverse -- if all you
had was the hash `3e4991b48bcb1bd9d3c4c14a1f24c415deaba466`, you won't be able
to find the string that produced this hash.

Moreover, it is impossible to find another string that has the same hash.

Hash functions are also
[deterministic](https://en.wikipedia.org/wiki/Deterministic_algorithm) i.e. they
will always produce the same output for the same input, no matter when or how
many times they are called.

It is important to understand that that hash functions **DO NOT** encrypt the
input string. There is no secret key involved, so there's no chance of losing a
key that will break the whole scheme. Hash functions only take one input -- the
text for which the digest needs to be produced.

(Note that while the examples here use [SHA-1][], it is quite old and not as
secure anymore. It is recommended to use [SHA-3][] instead. The only reason we
use SHA-1 here is for the purpose of readability - hash strings generated by
SHA-3 are a bit longer)

[SHA-1]: https://en.wikipedia.org/wiki/SHA-1

### Using hash functions to secure election results

What we are trying to achieve is this: once polling closes, we want a guarantee
that the result in an EVM at that moment will not be different from the
result that is revealed on counting day.

The result recorded in an EVM is simply a sequence of numbers, where each number
indicates the votes received by a candidate (the order of these numbers is the
same as the order of candidates on the ballot unit, which is fixed a few weeks
prior to voting).

Assume that at a polling station there are five candidates, and the result
stored inside the EVM at close of polling is this:
`400,300,500,200,100`. (i.e. the first candidate received 400 votes, the second
candidate received 300 votes, and so on). The SHA-1 hash of this string is
`91699a41d11cbe2e18319949151fd03ef529a833`.

The EVM will only reveal the generated hash string and nothing else. This can
safely be disclosed to the public at large.

On the day of counting, the EVM reveals the actual result. Anyone can look at
the result and compute its hash. If the computed hash matches the hash revealed
earlier, one can be fairly confident that the EVM has not been tampered with or
replaced after polling closed.

How do we know that this works? Remember that even if you know the original
string and the hash, you cannot find another string that has the same hash. So
even if someone were to break into an EVM, view the result and change it, they
can't find another sequence of numbers that would have the same hash. Replacing
an EVM won't help either since the hash is already public.

Another important aspect to consider is that one shouldn't be able to figure out
the result from the hash. Remember that it is impossible to figure out the
original string just from the hash, so this should in theory work. **However**,
since we already know the number of candidates and the voters, it may not be
that difficult to calculate the result be brute force, especially if the number
of candidates or voters is low. We'll discuss this in more detail next.

### Calculating the result by brute force

Consider a polling station with 50 voters and only 2 candidates. There are only
51 ways in which the vote share can be divided between the two candidates:

```
0,50
1,49
2,48
...
50,0
```

So if someone wants to know the poll result beforehand, they can simply compute
the hash for all 51 permutations of the result (i.e. create a [rainbow
table](https://en.wikipedia.org/wiki/Rainbow_table)):

```
|--------+------------------------------------------|
| Result | Hash                                     |
|--------+------------------------------------------|
| 0,50   | c87b42a20015ca36b3ee027a8e125c7a71e3d4f8 |
| 1,49   | 151eaff1df5bbc8f0259d679047560b45740544e |
| 2,48   | 1f5916b0dbfa228a07b7d6293aca31e0e1dd53d6 |
| ...    |                                          |
| 50,0   | 406840d6e2e9517378d13240b158c2cf843e8d67 |
|--------+------------------------------------------|
```

Now compare the hash provided by the EVM with the hashes in this table. The
result is the one whose hash matches with the one provided by the EVM. This is
known as the brute-force approach to cracking a hash.

As the number of candidates and voters increase, the probability of being able
to carry out a brute force attack becomes lower:

* At 100 voters and 5 candidates, commodity hardware can crack the result in
  seconds.

* At 600 voters and 10 candidates, the fastest bitcoin mining hardware around
  (which specializes in computing hashes at a high speed) will take a few days
  to crack the result.

* At 1000 voters and 15 candidates, one can be fairly confident that not even a
  nation-state cannot brute force their way to the result.

Generally, for a polling station with _n_ voters and _k_ candidates, the number
of permutations of the result is <a
href="https://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics)"><sup>n+k-1</sup>C<sub>k-1</sub></a>.

Clearly, when the number of candidates and/or voters is low, it's fairly easy to
calculate the result beforehand (even though nothing's wrong with the hash
function itself -- this is known as a [side-channel
attack](https://en.wikipedia.org/wiki/Side-channel_attack)).

So cryptographic hash functions alone are not sufficient to protect the secrecy
of election results. How do we fix this?

### Randomization

The answer lies in randomization. Generate a long enough random number, append
it to the result text, then compute the hash of this combined text. On counting
day, when the results are revealed, the random number that was used should be
revealed too, so that hash computation can still be verified independently.

Going back to our hypothetical result string: `400,300,500,200,100`. Let's say
the EVM generates this random number: `249825579`. We simply append this number
to the result: `400,300,500,200,100,249825579` and compute the hash of the
combined text. The resultant hash is revealed immediately. And on counting day,
the randomly generated number `249825579` is also revealed alongwith the each
candidate's vote count.

What's a long enough random number? A 128-bit random number (i.e. a number
picked at random from `2^128` possibilites) should be good enough. If a true
128-bit random number is appended to every result text, no matter how low the
number of voters/candidates are, the number of permutations is no less than
`2^128`. This is big enough that even if you had the fastest supercomputer ever
built at your disposal, earth itself will be incinerated by the sun before it
can compute the result.

The problem with random numbers, though, is that generating truly random numbers
is hard. And it is impossible to generate them from software without an external
source of randomness. Do the EVMs ship with a
[component](https://en.wikipedia.org/wiki/Hardware_random_number_generator) that
generates high quality random numbers? I think not.

## Concluding thoughts

### Feasibility

Can this scheme work? Probably yes.

Is it feasible to do this today? Probably no.

As discussed under randomization, EVMs most likely don't ship with a hardware
based random number generator. So adopting this approach will likely require a
hardware upgrade to the EVMs, besides firmware upgrades. That perhaps makes this
scheme quite infeasible in the short term.

### Disclosure of voting patterns

One of the problems that has come up with EVMs in recent years is that a
candidate will usually get to know how many votes they received from each
polling station in their constituency. Some of them have threatened voters with
post-poll reprisals if a particular area did not vote for them. This led to the
introduction of a [Totalizer](https://en.wikipedia.org/wiki/Totaliser) that
allows votes cast in about 14 polling stations to be counted together.

Our approach, which requires the hash and the random number to be generated in
the EVM, is not compatible with this.

For it to work, it's the totalizer instead of the EVMs that needs to change.

1. All the EVMs whose results are mixed in a single totalizer will need to be
   brought together as soon as polls close,
2. Random number and hash generation will happen in the totalizer after the
   results from these EVMs are added up.

### Impact of VVPATs

In recent years, the election commission introduced
[VVPAT](https://en.wikipedia.org/wiki/Voter-verified_paper_audit_trail) based
EVMs -- besides registering the vote electronically, VVPAT machines also print
the vote on a paper, and store the paper votes in a sealed ballot box.

Unfortunately, only a small subset of paper votes are counted and tallied with
the EVM result. If all the paper based votes were to be counted, that combined
with a verifiable digital fingerprint of the result will, in my opinion, go a
long way towards assuring the public about the sanctity of the polling process.
