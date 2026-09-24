/* Windows-capable replacement for notty's src-unix/native/winsize.c.
 * Upstream notty has no Windows backend (sys/ioctl.h, SIGWINCH), so a
 * stock master clone cannot build notty.unix on mingw. CI copies this
 * file over the upstream one before pinning; the #else branch is the
 * untouched upstream code, so Unix builds are unaffected.
 *
 * This is a compile-only shim: CI never runs the TUI on Windows, and
 * bin/main.ml falls back to the plain-text dashboard on Win32. Delete
 * this file (and the notty pin) when the frontend moves to lambda-term.
 */
#ifdef _WIN32
#include <windows.h>
#include <caml/mlvalues.h>

CAMLprim value caml_notty_winsize (value vfd) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  (void)vfd;
  if (GetConsoleScreenBufferInfo (GetStdHandle (STD_OUTPUT_HANDLE), &info)) {
    int cols = info.srWindow.Right - info.srWindow.Left + 1;
    int rows = info.srWindow.Bottom - info.srWindow.Top + 1;
    return Val_int ((cols << 16) + ((rows & 0x7fff) << 1));
  }
  return Val_int (0);
}

#define __unit() value unit __attribute__((unused))

CAMLprim value caml_notty_winch_number (__unit()) {
  return Val_int (0);
}
#else
#include <sys/ioctl.h>
#include <signal.h>
#include <caml/mlvalues.h>

CAMLprim value caml_notty_winsize (value vfd) {
  int fd = Int_val (vfd);
  struct winsize w;
  if (ioctl (fd, TIOCGWINSZ, &w) >= 0)
    return Val_int ((w.ws_col << 16) + ((w.ws_row & 0x7fff) << 1));
  return Val_int (0);
}

#define __unit() value unit __attribute__((unused))

CAMLprim value caml_notty_winch_number (__unit()) {
  return Val_int (SIGWINCH);
}
#endif
