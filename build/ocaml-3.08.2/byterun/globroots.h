/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: globroots.h,v 1.3 2003/12/15 18:10:47 doligez Exp $ */

/* Registration of global memory roots */

#ifndef CAML_GLOBROOTS_H
#define CAML_GLOBROOTS_H

#include "mlvalues.h"

/* Skip list structure */

struct global_root {
  value * root;                    /* the address of the root */
  struct global_root * forward[1]; /* variable-length array */
};

#define MAX_LEVEL 15

struct global_root_list {
  value * root;                 /* dummy value for layout compatibility */
  struct global_root * forward[MAX_LEVEL]; /* forward chaining */
  int level;                    /* max used level */
};

extern struct global_root_list caml_global_roots;

#endif /* CAML_GLOBROOTS_H */
