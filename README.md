cs2500-server
=============

This package is currently unstable, and mostly unusable. Some of the
scripts and configuration files may be out of date or otherwise broken;
I have done my best here to point to those that work as of this writing.

This package contains scripts and configuration files to run and
administer the student-facing and grader-facing handin servers, as well
as to transfer files between those two servers.

See the companion package, cs2500-client, for information on the client
software that submits to this server.

## Overview
The cs2500 servers consist of a student server and a grading server.
The student server is used by students to turn in assignments and check
grades. The grading server is used by staff to return graded
assignments, exams, etc.

Note that this is a bit of a hack: the two servers are really just two
instances of the same "handin" server, originally intended for
submission only, but the code has since been repurposed for grading as
well.

## Initial Server Setup

TODO

(should include notes on opening up the firewall, running the server
itself, installing the handin project (Leif's "multi" branch), setting
config files, generating SSL keys/certs, xvfb-run)

### Creating Users

The users.rktd file records information on each user. It takes the form
of an S-expression which is a list of user S-expressions. For example:

((asmith ((plaintext "password1") "Alice Smith" "" "" ""))
 (bjones ((plaintext "password2") "Bob Jones"   "" "" "")))

In the above, "asmith" is a username, "password1" is their password, and
"Alice Smith" is their full name. The other fields record other user
metadata (not documented here, but empty strings are valid default
values).

The script bin/gen-users.rkt can help generate this file. See the top of
that file for usage details.

## Assignment Workflow
At a high level, the workflow for each assignment is this:
1. Create and activate the assignment on the student server.
2. Wait for the assignment submission deadline to pass, then deactivate
   the assignment on the student server.
3. Distribute the submissions from the student server to appropriate
   graders in the grader server (requires creation and activation of the
   assignment in the grader server)
4. Wait for the grading deadline to pass, then deactivate the assignment
   on the grader server.
5. Transfer the graded assignments back to the student server.

Sections below give details for each of these steps. (TODO)

TODO: a section on assigning pairs, or the pair file in general

### Creating Assignments

1. Run the bin/make-assignment script with appropriate arguments (see
   the top of that script for usage details). This creates a new
   directory under student-server for each part of the assignment.
2. Make sure that the pairs.rktd file in student-server is up-to-date for the
   current assignment.

The above steps do not "activate" the assignment; that is, they do not
make it available for students to submit to. To activate an assignment's
part, add the name of the part's directory as a string in the
`active-dirs` list in student-server/config.rktd.

Deactivating a part (i.e. putting it into the inactive-dirs list) makes
it available for viewing on the webisite, but does not allow students to
submit to it.

# OLD NOTES

##Updating
Each semester, you should update the server certificate. The script
`bin/gen-key` will generate and install the certificates, but you need
to update the handin clients manually.


##Student server
The student server includes checker scripts for checking that
assignments are in the right language. These scripts need to be copied
into the appropriate homework folder and renamed to `checker.rkt`.

The `init` folder contains various scripts to automatically
start/restart the server. These scripts may need to be modified to point
to the right server configuration directories and to run as a valid user.


##Grades server
The grades server includes checker scripts for checking that grades are
returns in the right format. These formats are crucial to automatically
updating grades. The checker scripts need to be copied into the
appropriate homework, quiz, or exam folder and renamed to
`checker.rkt`.

The grades server does not allow users to sign up for obvious reasons.
Use the `gen-graders.rkt` script in `bin` to 

The `init` folder contains various scripts to automatically
start/restart the server. These scripts may need to be modified to point
to the right server configuration directories and to run as a valid user.

##Automation
###Gradebook
The server uses Eli's grading scripts to parse annotations and manage
the gradebook automatically. They are copied and distributed with
permission. These scripts require server users have certain fields,
which are included in `config.rktd.default`.

The location of the student server needs to be configured in
`bin/utils/sh-init` to use Eli's scripts.

###Due dates
The server can automatically manage due dates and send assignments to
graders. These require some configuration. Each student must assigned a
grader in the student server's `users.rktd` file. The `cs2500-lib`
project includes examples for batch updating the server's users. Due
dates can be managed by via cron, and using the `cs2500-lib` interface
to the student server's configuration.

Further more grader information needs to exist in `graders.rktd`. An
example exists as `graders.rktd.default`.

###Posting and updating grades
Grades can be automatically posted to the student server from the grades
server. The script `update.sh` in `bin` can be modified to do this. It
assumes the grades are submitted in a particular format which is
enforced by the grades server.
