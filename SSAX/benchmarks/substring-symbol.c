/*
 *------------------------------------------------------------------------
 *			substring->symbol
 *
 * Converting a substring to a Scheme symbol
 *
 * This C code implements a function substring->symbol:
 *	substring->symbol STR BEG END
 * which converts a substring of STR to a symbol. The function can be
 * defined as
 *      (define (substring->symbol STR BEG END)
 *	    (string->symbol (substring STR BEG END)))
 * However, the current C version is faster and produces no garbage
 * in case the resulting symbol has already been seen. This function
 * can be regarded as a "shared substring" implementation of the
 * above code.
 *
 * To use this efficient version in your Scheme code, you have to add
 * (extern (substring->symbol::symbol (::string ::int ::int)    
 *		"substring_to_symbol"))
 * (pragma
 * 	   (substring->symbol no-cfa-top nesting))
 * to your module declaration.
 *
 * This code is specific to the Bigloo Scheme system by Manuel Serrano.
 *
 * The code is almost entirely based on a function 'string_to_symbol'
 * in bigloo2.4b/runtime/Clib/csymbol.c of Bigloo distribution.
 *
 * $Id$
 *------------------------------------------------------------------------
 */

#include <string.h>
#include <bigloo.h>
#include <assert.h>

extern obj_t string_to_symbol( char * );

obj_t
substring_to_symbol(char *name, const int beg, const int end)
{
  const int total_len = strlen(name);

#if 0
  printf("\nIn substring_to_symbol\n");
#endif

  assert( beg >= 0 && beg < total_len );
  if( end == total_len )
    return string_to_symbol(name+beg);

  assert( end >= 0 && end < total_len );
  {
    const char c = name[end];	/* Save the character at name[end] */
    obj_t symb;
    name[end] = '\0';		/* Forcibly terminate the string */
    symb = string_to_symbol(name+beg);
    name[end] = c;		/* Restore the char name[end] */
    return symb;
  }
}
