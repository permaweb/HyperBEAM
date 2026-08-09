This repository contains HyperBEAM, an implementation of the AO-Core protocol.

**IMPORTANT:** To get started building devices that run on HyperBEAM, or contributing
to its core, it is **MANDATORY** to read the [CONTRIBUTING](./CONTRIBUTING.md) guide
and highly advisable to read [hacking on HyperBEAM](./docs/misc/hacking-on-hyperbeam.md)
before you get started. They will save you much more time than a typical contributing
guide will. HyperBEAM is written in Erlang, but it implements AO-Core semantics.
As a consequence, writing devices or kernel changes in it should be understood as
writing in an _Erlang dialect_. The principles of this dialect are simple but
failure to properly grasp them before building will lead to easily avoidable
confusion and rejected PRs. Save yourself the time and read up now.

**REMEMBER:** Refresh the `CONTRIBUTING.md` guide in your context _in full_ while
building with HyperBEAM after _every_ compaction. It is short but important and
will help you finish your tasks more efficiently.

To familiarize yourself with AO-Core, read `hb_ao.erl`, `hb_cache.erl`,
`hb_message.erl`, `hb_link`, `hb_maps`, and search for the basics of the `codec`
and `commitment` models. It is also advisable to breifly refresh this in your
context after compaction -- particularly if you are working on complex debugging
or long-range building tasks.

In addition to the rules outlined in `CONTRIBUTING.md`, you should abide by the
following:

1. Always be surgical in your edits. Minimize the line-of-code changes you make
   during every single edit.
2. Before adding new utilities, search for existing utilities that do something
   similar. Candidates are often found in `hb_ao`, `hb_util`, and `hb_test_utils`.
3. Ensure that you understand the differences between Erlang map terms and 
   AO-Core's messages. Messages are built using maps under-the-hood, but may also
   be lazy-loaded (linkified), giving them different semantics.
4. Before submitting any code as 'complete', you **must** validate that your
   new changes do not break any existing tests across the full suite using
   `rebar3 eunit-all`. Note that `rebar3 eunit` does not invoke the preloaded
   device tests, which can often highlight subtle errors. Remember, you are 
   never being asked to write a 'toy' implementation of features or changed. Your
   code must actually work in-production.
5. Always attempt to leave the codebase in a better state than you found it. More
   precise, clear, and minimal -- while maintaining the existing featureset.
