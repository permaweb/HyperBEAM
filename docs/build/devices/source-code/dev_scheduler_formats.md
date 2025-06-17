# [Module dev_scheduler_formats.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_formats.erl)




This module is used by dev_scheduler in order to produce outputs that
are compatible with various forms of AO clients.

<a name="description"></a>

## Description ##

It features two main formats:

- `application/json`
- `application/http`

The `application/json` format is a legacy format that is not recommended for
new integrations of the AO protocol.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos2_normalize_data-1">aos2_normalize_data/1*</a></td><td>The <code>hb_gateway_client</code> module expects all JSON structures to at least
have a <code>data</code> field.</td></tr><tr><td valign="top"><a href="#aos2_normalize_types-1">aos2_normalize_types/1</a></td><td>Normalize an AOS2 formatted message to ensure that all field NAMES and
types are correct.</td></tr><tr><td valign="top"><a href="#aos2_to_assignment-2">aos2_to_assignment/2</a></td><td>Create and normalize an assignment from an AOS2-style JSON structure.</td></tr><tr><td valign="top"><a href="#aos2_to_assignments-3">aos2_to_assignments/3</a></td><td>Convert an AOS2-style JSON structure to a normalized HyperBEAM
assignments response.</td></tr><tr><td valign="top"><a href="#assignment_to_aos2-2">assignment_to_aos2/2*</a></td><td>Convert an assignment to an AOS2-compatible JSON structure.</td></tr><tr><td valign="top"><a href="#assignments_to_aos2-4">assignments_to_aos2/4</a></td><td></td></tr><tr><td valign="top"><a href="#assignments_to_bundle-4">assignments_to_bundle/4</a></td><td>Generate a <code>GET /schedule</code> response for a process as HTTP-sig bundles.</td></tr><tr><td valign="top"><a href="#assignments_to_bundle-5">assignments_to_bundle/5*</a></td><td></td></tr><tr><td valign="top"><a href="#cursor-2">cursor/2*</a></td><td>Generate a cursor for an assignment.</td></tr><tr><td valign="top"><a href="#format_opts-1">format_opts/1*</a></td><td>For all scheduler format operations, we do not calculate hashpaths,
perform cache lookups, or await inprogress results.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aos2_normalize_data-1"></a>

### aos2_normalize_data/1 * ###

`aos2_normalize_data(JSONStruct) -> any()`

The `hb_gateway_client` module expects all JSON structures to at least
have a `data` field. This function ensures that.

<a name="aos2_normalize_types-1"></a>

### aos2_normalize_types/1 ###

`aos2_normalize_types(Msg) -> any()`

Normalize an AOS2 formatted message to ensure that all field NAMES and
types are correct. This involves converting field names to integers and
specific field names to their canonical form.
NOTE: This will result in a message that is not verifiable! It is, however,
necessary for gaining compatibility with the AOS2-style scheduling API.

<a name="aos2_to_assignment-2"></a>

### aos2_to_assignment/2 ###

`aos2_to_assignment(A, RawOpts) -> any()`

Create and normalize an assignment from an AOS2-style JSON structure.
NOTE: This method is destructive to the verifiability of the assignment.

<a name="aos2_to_assignments-3"></a>

### aos2_to_assignments/3 ###

`aos2_to_assignments(ProcID, Body, RawOpts) -> any()`

Convert an AOS2-style JSON structure to a normalized HyperBEAM
assignments response.

<a name="assignment_to_aos2-2"></a>

### assignment_to_aos2/2 * ###

`assignment_to_aos2(Assignment, RawOpts) -> any()`

Convert an assignment to an AOS2-compatible JSON structure.

<a name="assignments_to_aos2-4"></a>

### assignments_to_aos2/4 ###

`assignments_to_aos2(ProcID, Assignments, More, RawOpts) -> any()`

<a name="assignments_to_bundle-4"></a>

### assignments_to_bundle/4 ###

`assignments_to_bundle(ProcID, Assignments, More, Opts) -> any()`

Generate a `GET /schedule` response for a process as HTTP-sig bundles.

<a name="assignments_to_bundle-5"></a>

### assignments_to_bundle/5 * ###

`assignments_to_bundle(ProcID, Assignments, More, TimeInfo, RawOpts) -> any()`

<a name="cursor-2"></a>

### cursor/2 * ###

`cursor(Assignment, RawOpts) -> any()`

Generate a cursor for an assignment. This should be the slot number, at
least in the case of mainnet `ao.N.1` assignments. In the case of legacynet
(`ao.TN.1`) assignments, we may want to use the assignment ID.

<a name="format_opts-1"></a>

### format_opts/1 * ###

`format_opts(Opts) -> any()`

For all scheduler format operations, we do not calculate hashpaths,
perform cache lookups, or await inprogress results.

