/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: ia32sse2.c,v 1.1 2003/10/24 09:18:01 xleroy Exp $ */

/* Test whether IA32 assembler supports SSE2 instructions */

int main()
{
  asm("pmuludq %mm1, %mm0");
  return 0;
}
