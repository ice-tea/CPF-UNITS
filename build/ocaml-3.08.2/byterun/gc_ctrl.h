/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: gc_ctrl.h,v 1.15 2004/01/02 19:23:22 doligez Exp $ */

#ifndef CAML_GC_CTRL_H
#define CAML_GC_CTRL_H

#include "misc.h"

extern double
     caml_stat_minor_words,
     caml_stat_promoted_words,
     caml_stat_major_words;

extern long
     caml_stat_minor_collections,
     caml_stat_major_collections,
     caml_stat_heap_size,
     caml_stat_top_heap_size,
     caml_stat_compactions,
     caml_stat_heap_chunks;

void caml_init_gc (unsigned long, unsigned long, unsigned long,
                   unsigned long, unsigned long);


#ifdef DEBUG
void caml_heap_check (void);
#endif

#endif /* CAML_GC_CTRL_H */
