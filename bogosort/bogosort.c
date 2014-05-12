/* bogosort */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#ifndef TRUE
   #define TRUE 1
   #define FALSE 0
#endif

void rearrange( char **, int );
int is_sorted( char **, int );

int main( int argc, char **argv ) {
   char **array;
   int arraylen;
   int i;

   array = &argv[1];
   arraylen = argc - 1;

   /* show unsorted array */
   for ( i = 0; i < arraylen; i++ ) {
      fprintf(stdout,"%s ", array[i]);
   }
   fprintf(stdout,"\n");

   /* bogosort */
   while ( ! is_sorted( array, arraylen ) ) {
      rearrange( array, arraylen );   
   }
   
   /* show sorted array */
   for ( i = 0; i < arraylen; i++ ) {
      fprintf(stdout,"%s ", array[i]);
   }
   fprintf(stdout,"\n");
}

int is_sorted( char **array, int len ) {
   int i;
   if ( len <= 1 ) return TRUE;
   for ( i = 1; i < len; i++ ) {
      /* reverse this test to change the sorting order */
      if ( strcmp(array[i],array[i-1]) < 0 ) return FALSE;
   }
   return TRUE;
}

void rearrange( char **array, int len ) {
   int i;   /* loop index */
   int j;   /* swap index */
   char *t; /* temporary storage */

   for ( i = 0; i < len; i++ ) {
      j = random() % len;
      t = array[j];
      array[j] = array[i];
      array[i] = t;
   }   
}

/**/
