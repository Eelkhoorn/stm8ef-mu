/*
 * This file is part of muforth: https://muforth.dev/
 *
 * Copyright (c) 2002-2024 David Frech. (Read the LICENSE for details.)
 */

#include "muforth.h"

#include <stdlib.h>     /* exit(3) */
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

static struct string cmd_line;

static void convert_command_line(int argc, char *argv[])
{
    char *copied;   /* pointer to last byte copied */

    /* skip arg[0] */
    argc--;
    argv++;

    /* Concatenate the args into a string in the dictionary. */
    mu_here();
    copied = cmd_line.data = (char *)POP_ADDR;

    while (argc--)
    {
        copied = string_copy(copied, *argv++);
        *copied++ = ' ';
    }

    /* Record length. */
    cmd_line.length = copied - cmd_line.data;

    /*
     * No need to null-terminate! This string is evaluated by the Forth
     * parser, not C code. Any pieces - like filenames - that get passed to
     * C are copied out of this string into the dictionary and
     * null-terminated first - just like input from _any other_ source.
     */
    PUSH(cmd_line.length);
    mu_allot();
}

void mu_push_command_line()
{
    PUSH_ADDR(cmd_line.data);
    PUSH(cmd_line.length);
}

void mu_bye()
{
    exit(0);
}
/*
Get the currunt working directory, so we can compile it in the target.
*/ 

#ifndef MAX_BUF
#define MAX_BUF 200
#endif
void mu_push_pwd()
{
    char path[MAX_BUF];
    
    errno = 0;
    if (getcwd(path, MAX_BUF) == NULL) {
        if (errno == ERANGE)
            printf("[ERROR] pathname exceeds 200 characters\n");
        else
            perror("getcwd");
        exit(EXIT_FAILURE);
    }

    getcwd(path, MAX_BUF);
    PUSH_ADDR(path);
    PUSH(strlen(path));
}

int main(int argc, char *argv[])
{
    muforth_init();
    convert_command_line(argc, argv);
    muforth_start();
    return 0;
}