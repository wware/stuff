#ifndef E_REDBLACK_TREE
#define E_REDBLACK_TREE


#include "misc.h"
#include "TemplateStack.h"
#include <math.h>
#include <limits.h>
#include <iostream>

//  CONVENTIONS:  
//                Function names: Each word in a function name begins with 
//                a capital letter.  An example funcntion name is  
//                CreateRedTree(a,b,c). Furthermore, each function name 
//                should begin with a capital letter to easily distinguish 
//                them from variables. 
//                                                                     
//                Variable names: Each word in a variable name begins with 
//                a capital letter EXCEPT the first letter of the variable 
//                name.  For example, int newLongInt.  Global variables have 
//                names beginning with "g".  An example of a global 
//                variable name is gNewtonsConstant. 


#ifndef MAX_INT
#define MAX_INT INT_MAX // some architechturs define INT_MAX not MAX_INT
#endif

// The RedBlackEntry class is an Abstract Base Class.  This means that no
// instance of the RedBlackEntry class can exist.  Only classes which
// inherit from the RedBlackEntry class can exist.  Furthermore any class
// which inherits from the RedBlackEntry class must define the member
// function GetKey().  The Print() member function does not have to
// be defined because a default definition exists.
//
// The GetKey() function should return an integer key for that entry.
// The key for an entry should never change otherwise bad things might occur.

class RedBlackEntry {
public:
  RedBlackEntry();
  virtual ~RedBlackEntry();
  virtual int GetKey() const = 0;
  virtual void Print() const;
};

class RedBlackTreeNode {
  friend class RedBlackTree;
public:
  void Print(RedBlackTreeNode*,
	     RedBlackTreeNode*) const;
  RedBlackTreeNode();
  RedBlackTreeNode(RedBlackEntry *);
  RedBlackEntry * GetEntry() const;
  ~RedBlackTreeNode();
protected:
  RedBlackEntry * storedEntry;
  int key;
  int red; /* if red=0 then the node is black */
  RedBlackTreeNode * left;
  RedBlackTreeNode * right;
  RedBlackTreeNode * parent;
};

class RedBlackTree {
public:
  RedBlackTree();
  ~RedBlackTree();
  void Print() const;
  RedBlackEntry * DeleteNode(RedBlackTreeNode *);
  RedBlackTreeNode * Insert(RedBlackEntry *);
  RedBlackTreeNode * GetPredecessorOf(RedBlackTreeNode *) const;
  RedBlackTreeNode * GetSuccessorOf(RedBlackTreeNode *) const;
  RedBlackTreeNode * Search(int key);
  TemplateStack<RedBlackTreeNode *> * Enumerate(int low, int high) ;
  void CheckAssumptions() const;
protected:
  /*  A sentinel is used for root and for nil.  These sentinels are */
  /*  created when RedBlackTreeCreate is caled.  root->left should always */
  /*  point to the node which is the root of the tree.  nil points to a */
  /*  node which should always be black but has aribtrary children and */
  /*  parent and no key or info.  The point of using these sentinels is so */
  /*  that the root and nil nodes do not require special cases in the code */
  RedBlackTreeNode * root;
  RedBlackTreeNode * nil;
  void LeftRotate(RedBlackTreeNode *);
  void RightRotate(RedBlackTreeNode *);
  void TreeInsertHelp(RedBlackTreeNode *);
  void TreePrintHelper(RedBlackTreeNode *) const;
  void FixUpMaxHigh(RedBlackTreeNode *);
  void DeleteFixUp(RedBlackTreeNode *);
};

#endif
