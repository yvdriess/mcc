
#ifndef INCLUDED_STRESSTEST_H
#define INCLUDED_STRESSTEST_H

#include <complex>
#include <stdlib.h>
#include <unistd.h>

#include <cnc/cnc.h>
#include <cnc/debug.h>

struct context;


struct step_1 {
  int execute( const int& t, context& c ) const;
};

struct step_2 {
  int execute( const int& t, context& c ) const;
};

struct source {
  int execute( const int& t, context& c ) const;
};

struct sink {
  int execute( const int& t, context& c ) const;
};


struct context: public CnC::context< context > {

  CnC::item_collection< int, int > items_1;
  CnC::item_collection< int, int > items_2;
  CnC::item_collection< int, int > items_3;
  CnC::item_collection< int, bool > signals;
  CnC::tag_collection< int > tags_1;
  CnC::tag_collection< int > tags_2;
  CnC::tag_collection< int > tags_3;
  context(): 
    CnC::context< context >(),
    items_1( this ),
    items_2( this ),
    items_3( this ),
    tags_1( this , false ),
    tags_2( this , false ),
    tags_3( this , false )
      {
        prescribe( tags_1 , step_1() );
        prescribe( tags_2 , step_2() );
        prescribe( tags_3 , sink() );
      }

};

#endif
