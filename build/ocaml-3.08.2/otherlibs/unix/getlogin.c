/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: getlogin.c,v 1.8 2001/12/07 13:40:29 xleroy Exp $ */

#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"
#include <errno.h>

extern char * getlogin(void);

CAMLprim value unix_getlogin(void)
{
  char * name;
  name = getlogin();
  if (name == NULL) unix_error(ENOENT, "getlogin", Nothing);
  return copy_string(name);
}
