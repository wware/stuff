
// Note that until the bug in egcs-2.90.27 980315 (egcs-1.0.2 release)
// gets fixed this file needs to be compiled with -fguiding-decls.  
// Trying to fix the warning caused by compileing without -fguiding-decls
// causes an internal compiler error.

#ifndef _INC_SETS_DOT_H
#define _INC_SETS_DOT_H

#include<stdio.h>
#include<stdlib.h>
#include<iostream.h>
#include<strstream.h>
#include<tcl.h>
#include<ctype.h>

// **********************************************************************
//
//		See the html documentation for how to use these classes
//
// This file implements sets through tcl hash tables.
// See the html documentation accesed through ~/indev/lithas/docs/html/gui.html
// for information on how to use these data structures.
//
// **********************************************************************




// The SetDatabase is intended to be accessed only by the SetDatabaseHandle
// class.  This allows an instance of SetDatabase to be pointed
// to by an aribtrary number of SetDatabaseHandle instances.  When
// the last SetDatabaseHandle instance stops pointing to a database,
// the database deletes itself.  
//
// The advantages of this approach are:
//
// (1) If more than one thing wants to point to a given database,
//     we don't have to make copies of the database or jump through
//     hoops to make sure memory is managed properly.
//
// (2) Passing the database around is much easier because no class
//     that holds the database has to worry about who owns the
//     database.

typedef Tcl_HashEntry * SetEntryToken;
const SetEntryToken STE_NULL_TOKEN = NULL;

template <class KeyClass, class ValueClass> class SetDatabaseHandle;
template <class KeyClass, class ValueClass> class SetDatabaseHandleIterator;

template <class KeyClass, class ValueClass>
class SetDatabase {
  friend class SetDatabaseHandleIterator<KeyClass,ValueClass>;
  friend class SetDatabaseHandle<KeyClass,ValueClass>;
  friend ostream & operator<<<KeyClass, ValueClass>(ostream & s, 
						    const SetDatabase & d);
  friend istream & operator>><KeyClass, ValueClass>(istream & s, 
						    SetDatabase & d);
public:
  inline int GetKeyType() const {return databaseKeyType;}

  inline int Size() const {return itemsInTable;}

  // WARNING: DANGER: casting going on here
  ValueClass Search( const KeyClass key, int * found) const
  {
    Tcl_HashEntry * entry =
      Tcl_FindHashEntry((Tcl_HashTable*)(&hashTable), (char *) key);
    if (entry == NULL) {
      if (found != NULL) *found = 0;
      return (ValueClass) NULL;
    }
    if (found != NULL) *found = 1;
    return (ValueClass) Tcl_GetHashValue(entry);
  }

  void ForEachItemInSetDo(void (*function)(KeyClass key,ValueClass value))
  {
    Tcl_HashSearch searchPtr;
    Tcl_HashEntry * currentEntry = Tcl_FirstHashEntry(&hashTable, &searchPtr);
    while (currentEntry != NULL) {
      KeyClass key = (KeyClass) Tcl_GetHashKey(&hashTable,currentEntry);
      ValueClass value = (ValueClass ) Tcl_GetHashValue(currentEntry);
      function(key,value);
      currentEntry = Tcl_NextHashEntry(&searchPtr);
    }
  }


  void ForEachItemInSetDo(void (*function)(KeyClass key,ValueClass value,
					   void * extraArg),
			  void * extraArgForFunction)
  {
    Tcl_HashSearch searchPtr;
    Tcl_HashEntry * currentEntry = Tcl_FirstHashEntry(&hashTable, &searchPtr);
    while (currentEntry != NULL) {
      KeyClass key = (KeyClass) Tcl_GetHashKey(&hashTable,currentEntry);
      ValueClass value = (ValueClass ) Tcl_GetHashValue(currentEntry);
      function(key,value,extraArgForFunction);
      currentEntry = Tcl_NextHashEntry(&searchPtr);
    }
  }

  int Add( const KeyClass & key,  const ValueClass  object, 
	   ValueClass * oldObject , SetEntryToken * token)
  {
    int notAlreadyInTable;
    InvalidateIteratorsAttachedToThisDatabase();
    *token = Tcl_CreateHashEntry(&hashTable, (char *) key, &notAlreadyInTable);
    if (notAlreadyInTable) 
      itemsInTable++;
    else  *oldObject = (ValueClass) Tcl_GetHashValue(*token);
    Tcl_SetHashValue(*token,object);
    return notAlreadyInTable;
  }

  int Add( const KeyClass & key,  const ValueClass  object, SetEntryToken * token)
    {
      int notAlreadyInTable;
      InvalidateIteratorsAttachedToThisDatabase();
      *token = Tcl_CreateHashEntry(&hashTable, (char *) key, &notAlreadyInTable);
      if (notAlreadyInTable) 
	itemsInTable++;
      Tcl_SetHashValue(*token,object);
      return notAlreadyInTable;
    }

  int Add( const KeyClass & key,  const ValueClass  object)
  {
    int notAlreadyInTable;
    InvalidateIteratorsAttachedToThisDatabase();
    Tcl_HashEntry * newEntry =
      Tcl_CreateHashEntry(&hashTable, (char *) key, &notAlreadyInTable);
    if (notAlreadyInTable) 
      itemsInTable++;
    Tcl_SetHashValue(newEntry,object);
    return notAlreadyInTable;
  }

  void DeleteEntryForToken(SetEntryToken token)
    {
      InvalidateIteratorsAttachedToThisDatabase();
      Tcl_DeleteHashEntry(token);
      itemsInTable--;
    }
  
  ValueClass Delete( const KeyClass & key, int * found)
  {
    ValueClass  returnValue = (ValueClass) NULL;
    Tcl_HashEntry * entry = Tcl_FindHashEntry(&hashTable, (char *) key);
    if (entry != NULL) {
      InvalidateIteratorsAttachedToThisDatabase();
      itemsInTable--;
      returnValue = (ValueClass ) Tcl_GetHashValue(entry);
      Tcl_DeleteHashEntry(entry);
      if (found != NULL) *found = 1;
    } else
      if (found != NULL) *found = 0;
    return returnValue;
  }

protected:

  SetDatabase(int typeOfKeys)
    :numberOfHandlesToThisObject(0), _numberRefs(0),
     databaseKeyType(typeOfKeys) ,
     itemsInTable(0), _IDNumber(0)
  {
    Tcl_InitHashTable(&hashTable, typeOfKeys);
  }

  ~SetDatabase()
  {
    Tcl_DeleteHashTable(&hashTable);
  }

  bool MatchesIDNumber(const int IDNumber) {return IDNumber == _IDNumber;}

  void InvalidateIteratorsAttachedToThisDatabase()
    {
#ifdef CHECK_ASSUMPTIONS
      _IDNumber++;
#endif
    }

  int numberOfHandlesToThisObject;
  int _numberRefs;
  Tcl_HashTable hashTable;
  int databaseKeyType;
  int itemsInTable;
private:
  SetDatabase( const SetDatabase &); // no one should use this!

  int _IDNumber;
};


template <class KeyClass, class ValueClass>
class SetDatabaseHandle {
  friend class SetDatabaseHandleIterator<KeyClass,ValueClass>;
  friend SetDatabaseHandle operator&<KeyClass, ValueClass>
  (SetDatabaseHandle&, SetDatabaseHandle &);
  friend SetDatabaseHandle operator|<KeyClass, ValueClass>
  (SetDatabaseHandle &, SetDatabaseHandle &);
  friend SetDatabaseHandle operator-<KeyClass, ValueClass>
  (SetDatabaseHandle &, SetDatabaseHandle &);
  friend ostream& operator<<<KeyClass, ValueClass>
  (ostream&,const SetDatabaseHandle<KeyClass,ValueClass>&);
  friend istream& operator>><KeyClass, ValueClass>
  (istream&,SetDatabaseHandle<KeyClass,ValueClass>&);
public:
  SetDatabaseHandle(int typeOfKeys)
    :database(new SetDatabase<KeyClass,ValueClass>(typeOfKeys))
  {
    GrabDb(database);
  }

  SetDatabaseHandle(SetDatabase<KeyClass,ValueClass> * d)
    :database(d)
  {
    GrabDb(d);
  }

  SetDatabaseHandle(SetDatabaseHandle * c)
    :database(c->database)
  {
    GrabDb(c->database);
  }

  SetDatabaseHandle( const SetDatabaseHandle & c)
    :database(c.database)
  {
    GrabDb(c.database);
  }

  ~SetDatabaseHandle()
  {
    LetGoDb(database);
  }

  SetDatabaseHandle & operator=(const SetDatabaseHandle  &);

  bool operator==(const SetDatabaseHandle  &);

  bool operator!=(const SetDatabaseHandle  &);


  inline void ForEachItemInSetDo(void (*function)(KeyClass key,
						  ValueClass value))
  {
    database->ForEachItemInSetDo(function);
  }

  inline void ForEachItemInSetDo(void (*function)(KeyClass , ValueClass ,
						  void * extraArg),
				 void * extraArgForFunction)
  {
    database->ForEachItemInSetDo(function,extraArgForFunction);
  }

  inline int Size() const {return database->Size();}

  inline ValueClass Search( const KeyClass object, int * found = NULL) const
  {
    return database->Search(object,found);
  }

  inline ValueClass Delete(KeyClass k,int * found = NULL) 
  {return database->Delete(k,found);}

  inline void DeleteEntryForToken(SetEntryToken token)
    {database->DeleteEntryForToken(token);}

  inline int Add( const KeyClass & key, const ValueClass  object)
  {
    return database->Add(key,object);
  }

  inline int Add
  ( const KeyClass & key,  const ValueClass object, SetEntryToken * token)
  {
    return database->Add(key,object,token);
  }

  inline int Add
  ( const KeyClass & key, const ValueClass  object,
    ValueClass * oldObject, SetEntryToken * token)
  {
    return database->Add(key,object,oldObject,token);
  }

protected:

  // The LetGoDb function decrements the number of things pointing
  // to s by 1.  If the number of things pointing to s goes below
  // 1, then s is delete'd.  This handles the memory management
  // for the SetDatabase class pointed to by the SetDatabaseHandles.
  void LetGoDb(SetDatabase<KeyClass,ValueClass> * s){
    if (--(s->numberOfHandlesToThisObject) < 1) delete s;}
  
  // The Grab function increments the number of things pointing
  // to s by 1.  See the comment for the LetGoDb function for 
  // more information.
  void GrabDb(SetDatabase<KeyClass,ValueClass> * s)
    {s->numberOfHandlesToThisObject++;}

  bool MatchesIDNumber(const int IDNumber) const
    {return database->MatchesIDNumber(IDNumber);}
  inline Tcl_HashTable * GetDatabaseHashTable(){return &(database->hashTable);}
  SetDatabase<KeyClass,ValueClass> * database;

  // these two routines are for maintaining a count of references
  // to values inside the database, because the database should not
  // be deleted if the number of refs != 0
  void LetGoRef()
    {database->_numberRefs--;}
  void GrabRef()
    {database->_numberRefs++;}

  bool HasRef() const
    {return (database->_numberRefs != 0);}

  // indicate if this is the only handle
  // may be needed to delete objects if this handle is about to die
  bool OnlyHandle() const
    {return (database->numberOfHandlesToThisObject == 1);}
};

/* **********************************************************************
 *
 * FUNCTION:	AndDatabases
 *
 * INPUTS:	smaller is the smaller Tcl_HashTable to and with larger
 *		larger is the larger Tcl_HashTable to and with smaller
 *		result is where the (smaller & larger) is stored.
 *
 * OUTPUT:	none
 *
 * PURPOSE:	This function goes through every item in smaller.
 *		Each item that is also present in larger is added
 *		to result.  This function is meant to be used bye operator&.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:08:57 PST 1998
 *
 ********************************************************************** */

template <class KeyClass, class ValueClass>
void AndDatabases(Tcl_HashTable * smaller, 
		  Tcl_HashTable * larger, 
		  SetDatabase<KeyClass,ValueClass> * result)
{
  Tcl_HashSearch searchPtr;
  Tcl_HashEntry * currentEntry = Tcl_FirstHashEntry(smaller, &searchPtr);
  while (currentEntry != NULL) {
    KeyClass key = (KeyClass) Tcl_GetHashKey(smaller,currentEntry);
    if (NULL != Tcl_FindHashEntry(larger, (char *) key))
      result->Add(key,(ValueClass) Tcl_GetHashValue(currentEntry));
    currentEntry = Tcl_NextHashEntry(&searchPtr);
  }
}

/* **********************************************************************
 *
 * FUNCTION:	SubtractDatabases
 *
 * INPUTS:	negative is the database to be subtracted from positive
 *	        result is set to contain everything in positive that is
 *		not in negative.
 *
 * OUTPUT:	none
 *
 * PURPOSE:	This "Subtracts" everything in the negative hash table
 *		from the positive hash table and stores the result in
 *		result.  This function is meant to be used by operator-.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:11:06 PST 1998
 *
 ********************************************************************** */


template <class KeyClass, class ValueClass>
void SubtractDatabases(Tcl_HashTable * positive,
		       Tcl_HashTable * negative, 
		       SetDatabase<KeyClass, ValueClass> * result)
{
  Tcl_HashSearch searchPtr;
  Tcl_HashEntry * currentEntry = Tcl_FirstHashEntry(positive, &searchPtr);
  while (currentEntry != NULL) {
    KeyClass key = (KeyClass) Tcl_GetHashKey(positive,currentEntry);
    if (NULL ==  Tcl_FindHashEntry(negative, (char *) key))
      result->Add(key,(ValueClass ) Tcl_GetHashValue(currentEntry));
    currentEntry = Tcl_NextHashEntry(&searchPtr);
  }
}

/* **********************************************************************
 *
 * FUNCTION:	MergeDatabases
 *
 * INPUTS:	Everything in the input hash table is put into the
 *		result database.
 *
 * OUTPUT:	none
 *
 * PURPOSE:	This puts everything in input into result to implement
 *		operator|.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:39:24 PST 1998
 *
 ********************************************************************** */


template <class KeyClass, class ValueClass>
void MergeDatabases(Tcl_HashTable * input,
		    SetDatabase<KeyClass,ValueClass> * result)
{
  Tcl_HashSearch searchPtr;
  Tcl_HashEntry * currentEntry = Tcl_FirstHashEntry(input, &searchPtr);
  while (currentEntry != NULL) {
    result->Add((KeyClass) Tcl_GetHashKey(input,currentEntry),
		(ValueClass ) Tcl_GetHashValue(currentEntry));
    currentEntry = Tcl_NextHashEntry(&searchPtr);
  }
}

/* **********************************************************************
 *
 * FUNCTION:	operator|
 *
 * INPUTS:	a and b are SetDatabaseHandles to combine
 *
 * OUTPUT:	returns a SetDatabaseHandle containing everything in a and b.
 *
 * PURPOSE:	This is used to implement a union of the two input sets.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:41:02 PST 1998
 *
 ********************************************************************** */

template <class KeyClass, class ValueClass>
SetDatabaseHandle<KeyClass,ValueClass> 
operator|(SetDatabaseHandle<KeyClass,ValueClass> & a, 
	  SetDatabaseHandle<KeyClass,ValueClass> & b)
{
  ASSUME(a.database->GetKeyType() == b.database->GetKeyType());

  SetDatabaseHandle<KeyClass,ValueClass> result(a.database->GetKeyType());
  MergeDatabases(a.GetDatabaseHashTable(),result.database);
  MergeDatabases(b.GetDatabaseHashTable(),result.database);

  return result;
}

/* **********************************************************************
 *
 * FUNCTION:	operator&
 *
 * INPUTS:	a and b are SetDatabaseHandles to intersect
 *
 * OUTPUT:	returns a new SetDatabaseHandle containing the intersection
 *		of a nd b.
 *
 * PURPOSE:	This is used to implement the intersection of two sets.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:42:27 PST 1998
 *
 ********************************************************************** */

template <class KeyClass, class ValueClass>SetDatabaseHandle<KeyClass,ValueClass>
operator&(SetDatabaseHandle<KeyClass,ValueClass> & a, 
	  SetDatabaseHandle<KeyClass,ValueClass> & b)
{
  ASSUME(a.database->GetKeyType() == b.database->GetKeyType());

  SetDatabaseHandle<KeyClass,ValueClass> result(a.database->GetKeyType());
  if (a.database->Size() < b.database->Size()) 
    AndDatabases(a.GetDatabaseHashTable(),
		 b.GetDatabaseHashTable(),
		 result.database);
  else
    AndDatabases(b.GetDatabaseHashTable(),
		 a.GetDatabaseHashTable(),
		 result.database);

  return result;
}

/* **********************************************************************
 *
 * FUNCTION:	operator-
 *
 * INPUTS:	b is subtracted from a
 *
 * OUTPUT:	Returns a new SetDatabaseHandle containing everything that
 *		is in a but not in b.
 *
 * PURPOSE:	This implements subtraction of sets.
 *
 * MODIFIED:	Emin Martinian, emin@iname.com, Wed Jan 14 13:43:31 PST 1998
 *
 ********************************************************************** */

template <class KeyClass, class ValueClass>SetDatabaseHandle<KeyClass,ValueClass> 
operator-(SetDatabaseHandle<KeyClass,ValueClass>  &a, 
	  SetDatabaseHandle<KeyClass,ValueClass>  &b)
{
  ASSUME(a.database->GetKeyType() == b.database->GetKeyType());

  SetDatabaseHandle<KeyClass,ValueClass> result(a.database->GetKeyType());
  SubtractDatabases(a.GetDatabaseHashTable(),
		    b.GetDatabaseHashTable(),
		    result.database);
  return result;
}

  
template <class KeyClass, class ValueClass>
SetDatabaseHandle<KeyClass,ValueClass> & 
SetDatabaseHandle<KeyClass,ValueClass>::operator=
(const SetDatabaseHandle<KeyClass,ValueClass> & other) 
{
  if (other.database != database) {
    LetGoDb(database);
    database = other.database;
    GrabDb(database);
  }
  return *this;
}

template <class KeyClass, class ValueClass>
bool
SetDatabaseHandle<KeyClass,ValueClass>::operator==
(const SetDatabaseHandle<KeyClass,ValueClass> & other) 
{
  return (other.database == database);
}

template <class KeyClass, class ValueClass>
bool
SetDatabaseHandle<KeyClass,ValueClass>::operator!=
(const SetDatabaseHandle<KeyClass,ValueClass> & other) 
{
  return (other.database != database);
}

template <class KeyClass, class ValueClass>
ostream & operator<<(ostream & s, const SetDatabase<KeyClass,ValueClass> & d)
{ 
  Tcl_HashSearch searchPtr;
  Tcl_HashEntry * currentEntry = 
    Tcl_FirstHashEntry((Tcl_HashTable *) // this cast discards the const
		       &(d.hashTable), &searchPtr);
  while (currentEntry != NULL) {
    KeyClass key = Tcl_GetHashKey(&(d.hashTable),currentEntry);
    s << "{{" << (KeyClass) key << "} ";
    s << "{" << (ValueClass) Tcl_GetHashValue(currentEntry) << "}} " ;
    currentEntry = Tcl_NextHashEntry(&searchPtr);
  }
  return s;
}
  
template <class KeyClass, class ValueClass>
ostream & operator<<(ostream & s, const SetDatabaseHandle<KeyClass,ValueClass> & h)
{
  s << *(h.database);
  return s;
}


template <class KeyClass, class ValueClass>
istream & operator>>(istream & s, SetDatabase<KeyClass,ValueClass> & d)
{ 
  char c;
  char ** mainArgv = NULL;
  char ** subArgv = NULL;
  int mainArgc, subArgc, notInteger, index;
  char * inputString = NULL;
  while ( (s >> c) && (isspace(c)) ); // eat up white space
  if ( (!s) || (c != '{') ) goto stopBecauseOfError;
  s.putback(c);
  s.gets(&inputString);
  if (!s) goto stopBecauseOfError;
  if (TCL_OK != (Tcl_SplitList(NULL,inputString,&mainArgc,&mainArgv)))
    goto stopBecauseOfError;
  for (int i = 0; i < mainArgc; ++i) {
      if ( (TCL_OK != (Tcl_SplitList(NULL,mainArgv[i],&subArgc,&subArgv)))
	   || (subArgc != 2) ) goto stopBecauseOfError;
      index = strtintOnlyCheck(subArgv[1],&notInteger);
      if (notInteger || (! d.Add(subArgv[0], index))) goto stopBecauseOfError;
      Tcl_Free((char*) subArgv);
      subArgv = NULL;
  }
  delete [] inputString;
  Tcl_Free( (char*) mainArgv);
  return s;

 stopBecauseOfError:
  if (mainArgv != NULL) Tcl_Free( (char*) mainArgv);
  if (subArgv != NULL) Tcl_Free( (char*) subArgv);
  s.clear(ios::badbit); // set state of stream to bad;
  if (inputString != NULL) delete [] inputString;
  return s;
}
  
template <class KeyClass, class ValueClass>
istream & operator>>(istream & s, SetDatabaseHandle<KeyClass,ValueClass> & h)
{
  s >> *(h.database);
  return s;
}

template <class KeyType, class ValueType>
class SetDatabaseHandleIterator {
public: 
  SetDatabaseHandleIterator(SetDatabaseHandle<KeyType,ValueType> const & handle)
  :_handle(handle), _IDNumber(handle.database->_IDNumber), 
   _currentHashEntry(Tcl_FirstHashEntry(&_handle.database->hashTable,&_searchPtr))
    { }

  inline bool ItemsRemaining() 
    {
      return _currentHashEntry != NULL;
    }

  inline KeyType GetCurrentKey() 
    {
      ASSUME( ItemsRemaining() );
      ASSUME( _handle.MatchesIDNumber(_IDNumber) );
      return (KeyType) Tcl_GetHashKey(&_handle.database->hashTable,_currentHashEntry);
    }

  inline ValueType GetCurrentValue() 
    {
      ASSUME( ItemsRemaining() );
      ASSUME( _handle.MatchesIDNumber(_IDNumber) );
      return (ValueType) Tcl_GetHashValue(_currentHashEntry);
    }
  
  KeyType operator++(int) // postfix operator
    {
      ASSUME(ItemsRemaining());
      ASSUME( _handle.MatchesIDNumber(_IDNumber) );
      KeyType returnValue = 
	(KeyType) Tcl_GetHashKey(&_handle.database->hashTable,_currentHashEntry);
      _currentHashEntry = Tcl_NextHashEntry(&_searchPtr);
      return returnValue;
    }

protected:
  const SetDatabaseHandle<KeyType,ValueType> &	_handle;
  int						_IDNumber;  
  Tcl_HashSearch				_searchPtr;
  Tcl_HashEntry *				_currentHashEntry;
};

#endif
