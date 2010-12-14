
#ifndef INCLUDED_MCCOMPILED_H
#define INCLUDED_MCCOMPILED_H

#include <cnc/cnc.h>
#include <cnc/debug.h>

typedef double amplitude;

struct context;


struct GEN {
  int execute( const int& t, context& c ) const;
};

struct KRON {
  int execute( const int& t, context& c ) const;
};

struct CZ {
  int execute( const int& t, context& c ) const;
};


struct context: public CnC::context< context > {

  CnC::item_collection< int, amplitude > TANGLE_1;
  CnC::item_collection< int, amplitude > TANGLE_1;
  CnC::item_collection< int, amplitude > TANGLE_1;
  CnC::item_collection< int, amplitude > TANGLE_1;
  CnC::tag_collection< int > TAG_1;
  CnC::tag_collection< int > TAG_1;
  CnC::tag_collection< int > TAG_1;
  const int CONST_1;
  const int CONST_1;
  context(CONST_1,CONST_2): 
    CnC::context< context >(),
    CONST_1(CONST_1),
    CONST_1(CONST_1),
    TANGLE_1( this ),
    TANGLE_1( this ),
    TANGLE_1( this ),
    TANGLE_1( this ),
    TAG_1( this , false ),
    TAG_1( this , false ),
    (TAG_3)( this , false )
      {
        prescribe( TAG_1, GEN() );
        prescribe( TAG_1, GEN() );
        prescribe( TAG_1, GEN() );
    }

};

#endif
