/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1997 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: setsid.c,v 1.5.6.1 2004/08/23 11:31:44 doligez Exp $ */

#include <fail.h>
#include <mlvalues.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

CAMLprim value unix_setsid(value unit)
{
#ifdef HAS_SETSID
  return Val_int(setsid());
#else
  invalid_argument("setsid not implemented");
  return Val_unit;
#endif
}
