This repository contains HyperBEAM, an implementation of the AO-Core protocol.

To familiarize yourself with AO-Core, read the `README.md` file.

To understand how to write code for HyperBEAM, read `CONTRIBUTING.md` for 
repository-level guidelines, and `docs/misc/hacking-on-hyperbeam.md` learn about
its debugging tools and infrastructure.

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
   new changes do not break any existing tests across the full suite. You are 
   never being asked to write a 'toy' implementation of features or changed. Your
   code must actually work in-production.
5. Always attempt to leave the codebase in a better state than you found it. More
   precise, clear, and minimal -- while maintaining the existing featureset.