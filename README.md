cs2500-server
=============

This package is currently unstable, and mostly unusable. It require the
cs2500-scripts package to be reorganized and renamed cs2500-lib.

This package contains the cs2500 assignment and grades server
startup scripts, configurations files, checker scripts, assignment and
grade automation scripts.

Requires cs2500-lib

##Intro.
The cs2500 servers consist of a student server and a grading server.
The student server is used by students to turn in assignments and check
grades. The grading server is used by staff to return graded
assignments, exams, etc.


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
