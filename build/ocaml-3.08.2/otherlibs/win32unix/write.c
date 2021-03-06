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

/* $Id: write.c,v 1.7.6.2 2004/07/08 08:40:55 xleroy Exp $ */

#include <errno.h>
#include <string.h>
#include <mlvalues.h>
#include <memory.h>
#include <signals.h>
#include "unixsupport.h"

CAMLprim value unix_write(value fd, value buf, value vofs, value vlen)
{
  long ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    while (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      memmove (iobuf, &Byte(buf, ofs), numbytes);
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        enter_blocking_section();
        ret = send(s, iobuf, numbytes, 0);
        leave_blocking_section();
        if (ret == SOCKET_ERROR) {
          win32_maperr(WSAGetLastError());
          uerror("write", Nothing);
        }
        numwritten = ret;
      } else {
        BOOL ret;
        HANDLE h = Handle_val(fd);
        enter_blocking_section();
        ret = WriteFile(h, iobuf, numbytes, &numwritten, NULL);
        leave_blocking_section();
        if (! ret) {
          win32_maperr(GetLastError());
          uerror("write", Nothing);
        }
      }
      written += numwritten;
      ofs += numwritten;
      len -= numwritten;
    }
  End_roots();
  return Val_long(written);
}

CAMLprim value unix_single_write(value fd, value buf, value vofs, value vlen)
{
  long ofs, len, written;
  DWORD numbytes, numwritten;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      memmove (iobuf, &Byte(buf, ofs), numbytes);
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        enter_blocking_section();
        ret = send(s, iobuf, numbytes, 0);
        leave_blocking_section();
        if (ret == SOCKET_ERROR) {
          win32_maperr(WSAGetLastError());
          uerror("single_write", Nothing);
        }
        numwritten = ret;
      } else {
        BOOL ret;
        HANDLE h = Handle_val(fd);
        enter_blocking_section();
        ret = WriteFile(h, iobuf, numbytes, &numwritten, NULL);
        leave_blocking_section();
        if (! ret) {
          win32_maperr(GetLastError());
          uerror("single_write", Nothing);
        }
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}
