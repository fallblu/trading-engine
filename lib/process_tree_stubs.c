#include <caml/mlvalues.h>

#if defined(__linux__)
#include <sys/prctl.h>
#endif

CAMLprim value trading_engine_enable_child_subreaper(value unit)
{
  (void)unit;
#if defined(__linux__)
  return Val_int(prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0));
#else
  return Val_int(0);
#endif
}
