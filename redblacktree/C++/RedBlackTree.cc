#include "RedBlackTree.h"
#include <stdio.h>
#include <math.h>

// If the symbol CHECK_RB_TREE_ASSUMPTIONS is defined then the
// code does a lot of extra checking to make sure certain assumptions
// are satisfied.  This only needs to be done if you suspect bugs are
// present or if you make significant changes and want to make sure
// your changes didn't mess anything up.
// #define CHECK_RB_TREE_ASSUMPTIONS 1


const int MIN_INT=-MAX_INT;

RedBlackTreeNode::RedBlackTreeNode()
{
}

RedBlackTreeNode::RedBlackTreeNode(RedBlackEntry * newEntry)
  : storedEntry (newEntry) , key(newEntry->GetKey())
{
}

RedBlackTreeNode::~RedBlackTreeNode()
{
}

RedBlackEntry * RedBlackTreeNode::GetEntry() const
{
    return storedEntry;
}

RedBlackEntry::RedBlackEntry()
{
}

RedBlackEntry::~RedBlackEntry()
{
}

void RedBlackEntry::Print() const {
  cout << "No Print Method defined. Using Default: " << GetKey() << endl;
}

RedBlackTree::RedBlackTree()
{
  nil = new RedBlackTreeNode;
  nil->left = nil->right = nil->parent = nil;
  nil->red = 0;
  nil->key = MIN_INT;
  nil->storedEntry = NULL;
  
  root = new RedBlackTreeNode;
  root->parent = root->left = root->right = nil;
  root->key = MAX_INT;
  root->red=0;
  root->storedEntry = NULL;  
}

/***********************************************************************/
/*  FUNCTION:  LeftRotate */
/**/
/*  INPUTS:  the node to rotate on */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input: this, x */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly.  */
/***********************************************************************/

void RedBlackTree::LeftRotate(RedBlackTreeNode* x)
{
  RedBlackTreeNode* y;
 
  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when DeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  y=x->right;
  x->right=y->left;

  if (y->left != nil) y->left->parent=x; /* used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */
  
  y->parent=x->parent;   

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  if( x == x->parent->left) {
    x->parent->left=y;
  } else {
    x->parent->right=y;
  }
  y->left=x;
  x->parent=y;

#ifdef CHECK_RB_TREE_ASSUMPTIONS
  CheckAssumptions();
#elif defined(DEBUG_ASSERT)
  Assert(!nil->red,"nil not red in RedBlackTree::LeftRotate");
#endif
}

/***********************************************************************/
/*  FUNCTION:  RighttRotate */
/**/
/*  INPUTS:  node to rotate on */
/**/
/*  OUTPUT:  None */
/**/
/*  Modifies Input?: this, y */
/**/
/*  EFFECTS:  Rotates as described in _Introduction_To_Algorithms by */
/*            Cormen, Leiserson, Rivest (Chapter 14).  Basically this */
/*            makes the parent of x be to the left of x, x the parent of */
/*            its parent before the rotation and fixes other pointers */
/*            accordingly.  */
/***********************************************************************/

void RedBlackTree::RightRotate(RedBlackTreeNode* y)
{
  RedBlackTreeNode* x;

  /*  I originally wrote this function to use the sentinel for */
  /*  nil to avoid checking for nil.  However this introduces a */
  /*  very subtle bug because sometimes this function modifies */
  /*  the parent pointer of nil.  This can be a problem if a */
  /*  function which calls LeftRotate also uses the nil sentinel */
  /*  and expects the nil sentinel's parent pointer to be unchanged */
  /*  after calling this function.  For example, when DeleteFixUP */
  /*  calls LeftRotate it expects the parent pointer of nil to be */
  /*  unchanged. */

  x=y->left;
  y->left=x->right;

  if (nil != x->right)  x->right->parent=y; /*used to use sentinel here */
  /* and do an unconditional assignment instead of testing for nil */

  /* instead of checking if x->parent is the root as in the book, we */
  /* count on the root sentinel to implicitly take care of this case */
  x->parent=y->parent;
  if( y == y->parent->left) {
    y->parent->left=x;
  } else {
    y->parent->right=x;
  }
  x->right=y;
  y->parent=x;

#ifdef CHECK_RB_TREE_ASSUMPTIONS
  CheckAssumptions();
#elif defined(DEBUG_ASSERT)
  Assert(!nil->red,"nil not red in RedBlackTree::RightRotate");
#endif
}

/***********************************************************************/
/*  FUNCTION:  TreeInsertHelp  */
/**/
/*  INPUTS:  z is the node to insert */
/**/
/*  OUTPUT:  none */
/**/
/*  Modifies Input:  this, z */
/**/
/*  EFFECTS:  Inserts z into the tree as if it were a regular binary tree */
/*            using the algorithm described in _Introduction_To_Algorithms_ */
/*            by Cormen et al.  This funciton is only intended to be called */
/*            by the Insert function and not by the user */
/***********************************************************************/

void RedBlackTree::TreeInsertHelp(RedBlackTreeNode* z)
{
  /*  This function should only be called by RedBlackTree::Insert */
  RedBlackTreeNode* x;
  RedBlackTreeNode* y;
    
  z->left=z->right=nil;
  y=root;
  x=root->left;
  while( x != nil) {
    y=x;
    if ( x->key > z->key) { 
      x=x->left;
    } else { /* x->key <= z->key */
      x=x->right;
    }
  }
  z->parent=y;
  if ( (y == root) ||
       (y->key > z->key) ) { 
    y->left=z;
  } else {
    y->right=z;
  }

#if defined(DEBUG_ASSERT)
  Assert(!nil->red,"nil not red in RedBlackTree::TreeInsertHelp");
#endif
}

/*  Before calling InsertNode  the node x should have its key set */

/***********************************************************************/
/*  FUNCTION:  InsertNode */
/**/
/*  INPUTS:  newEntry is the entry to insert*/
/**/
/*  OUTPUT:  This function returns a pointer to the newly inserted node */
/*           which is guarunteed to be valid until this node is deleted. */
/*           What this means is if another data structure stores this */
/*           pointer then the tree does not need to be searched when this */
/*           is to be deleted. */
/**/
/*  Modifies Input: tree */
/**/
/*  EFFECTS:  Creates a node node which contains the appropriate key and */
/*            info pointers and inserts it into the tree. */
/***********************************************************************/

RedBlackTreeNode * RedBlackTree::Insert(RedBlackEntry * newEntry)
{
  RedBlackTreeNode * y;
  RedBlackTreeNode * x;
  RedBlackTreeNode * newNode;

  x = new RedBlackTreeNode(newEntry);
  TreeInsertHelp(x);
  newNode = x;
  x->red=1;
  while(x->parent->red) { /* use sentinel instead of checking for root */
    if (x->parent == x->parent->parent->left) {
      y=x->parent->parent->right;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->right) {
	  x=x->parent;
	  LeftRotate(x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	RightRotate(x->parent->parent);
      } 
    } else { /* case for x->parent == x->parent->parent->right */
             /* this part is just like the section above with */
             /* left and right interchanged */
      y=x->parent->parent->left;
      if (y->red) {
	x->parent->red=0;
	y->red=0;
	x->parent->parent->red=1;
	x=x->parent->parent;
      } else {
	if (x == x->parent->left) {
	  x=x->parent;
	  RightRotate(x);
	}
	x->parent->red=0;
	x->parent->parent->red=1;
	LeftRotate(x->parent->parent);
      } 
    }
  }
  root->left->red=0;
  return(newNode);

#ifdef CHECK_RB_TREE_ASSUMPTIONS
  CheckAssumptions();
#elif defined(DEBUG_ASSERT)
  Assert(!nil->red,"nil not red in RedBlackTree::Insert");
  Assert(!root->red,"root not red in RedBlackTree::Insert");
#endif
}

/***********************************************************************/
/*  FUNCTION:  GetSuccessorOf  */
/**/
/*    INPUTS:  x is the node we want the succesor of */
/**/
/*    OUTPUT:  This function returns the successor of x or NULL if no */
/*             successor exists. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:  uses the algorithm in _Introduction_To_Algorithms_ */
/***********************************************************************/
  
RedBlackTreeNode * RedBlackTree::GetSuccessorOf(RedBlackTreeNode * x) const
{ 
  RedBlackTreeNode* y;

  if (nil != (y = x->right)) { /* assignment to y is intentional */
    while(y->left != nil) { /* returns the minium of the right subtree of x */
      y=y->left;
    }
    return(y);
  } else {
    y=x->parent;
    while(x == y->right) { /* sentinel used instead of checking for nil */
      x=y;
      y=y->parent;
    }
    if (y == root) return(nil);
    return(y);
  }
}

/***********************************************************************/
/*  FUNCTION:  GetPredecessorOf  */
/**/
/*    INPUTS:  x is the node to get predecessor of */
/**/
/*    OUTPUT:  This function returns the predecessor of x or NULL if no */
/*             predecessor exists. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:  uses the algorithm in _Introduction_To_Algorithms_ */
/***********************************************************************/

RedBlackTreeNode * RedBlackTree::GetPredecessorOf(RedBlackTreeNode * x) const
{
  RedBlackTreeNode* y;

  if (nil != (y = x->left)) { /* assignment to y is intentional */
    while(y->right != nil) { /* returns the maximum of the left subtree of x */
      y=y->right;
    }
    return(y);
  } else {
    y=x->parent;
    while(x == y->left) { 
      if (y == root) return(nil); 
      x=y;
      y=y->parent;
    }
    return(y);
  }
}

/***********************************************************************/
/*  FUNCTION:  Print */
/**/
/*    INPUTS:  none */
/**/
/*    OUTPUT:  none  */
/**/
/*    EFFECTS:  This function recursively prints the nodes of the tree */
/*              inorder. */
/**/
/*    Modifies Input: none */
/**/
/*    Note:    This function should only be called from ITTreePrint */
/***********************************************************************/

void RedBlackTreeNode::Print(RedBlackTreeNode * nil,
			     RedBlackTreeNode * root) const
{
  storedEntry->Print();
  printf(", key=%i ",key);
  printf("  l->key=");
  if( left == nil) printf("NULL"); else printf("%i",left->key);
  printf("  r->key=");
  if( right == nil) printf("NULL"); else printf("%i",right->key);
  printf("  p->key=");
  if( parent == root) printf("NULL"); else printf("%i",parent->key);
  printf("  red=%i\n",red);
}

void RedBlackTree::TreePrintHelper( RedBlackTreeNode* x) const
{  
  if (x != nil) {
    TreePrintHelper(x->left);
    x->Print(nil,root);
    TreePrintHelper(x->right);
  }
}

RedBlackTree::~RedBlackTree()
{
  RedBlackTreeNode * x = root->left;
  TemplateStack<RedBlackTreeNode *> stuffToFree;

  if (x != nil) {
    if (x->left != nil) {
      stuffToFree.Push(x->left);
    }
    if (x->right != nil) {
      stuffToFree.Push(x->right);
    }
    // delete x->storedEntry;
    delete x;
    while( stuffToFree.NotEmpty() ) {
      x = stuffToFree.Pop();
      if (x->left != nil) {
	stuffToFree.Push(x->left);
      }
      if (x->right != nil) {
	stuffToFree.Push(x->right);
      }
      // delete x->storedEntry;
      delete x;
    }
  }
  delete nil;
  delete root;
}


/***********************************************************************/
/*  FUNCTION:  Print */
/**/
/*    INPUTS:  none */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  This function recursively prints the nodes of the tree */
/*             inorder. */
/**/
/*    Modifies Input: none */
/**/
/***********************************************************************/

void RedBlackTree::Print() const
{
  TreePrintHelper(root->left);
}

/***********************************************************************/
/*  FUNCTION:  DeleteFixUp */
/**/
/*    INPUTS:  x is the child of the spliced */
/*             out node in DeleteNode. */
/**/
/*    OUTPUT:  none */
/**/
/*    EFFECT:  Performs rotations and changes colors to restore red-black */
/*             properties after a node is deleted */
/**/
/*    Modifies Input: this, x */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

void RedBlackTree::DeleteFixUp(RedBlackTreeNode* x)
{
  RedBlackTreeNode * w;
  RedBlackTreeNode * rootLeft = root->left;

  while( (!x->red) && (rootLeft != x)) {
    if (x == x->parent->left) {
      w=x->parent->right;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	LeftRotate(x->parent);
	w=x->parent->right;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->right->red) {
	  w->left->red=0;
	  w->red=1;
	  RightRotate(w);
	  w=x->parent->right;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->right->red=0;
	LeftRotate(x->parent);
	x=rootLeft; /* this is to exit while loop */
      }
    } else { /* the code below is has left and right switched from above */
      w=x->parent->left;
      if (w->red) {
	w->red=0;
	x->parent->red=1;
	RightRotate(x->parent);
	w=x->parent->left;
      }
      if ( (!w->right->red) && (!w->left->red) ) { 
	w->red=1;
	x=x->parent;
      } else {
	if (!w->left->red) {
	  w->right->red=0;
	  w->red=1;
	  LeftRotate(w);
	  w=x->parent->left;
	}
	w->red=x->parent->red;
	x->parent->red=0;
	w->left->red=0;
	RightRotate(x->parent);
	x=rootLeft; /* this is to exit while loop */
      }
    }
  }
  x->red=0;

#ifdef CHECK_RB_TREE_ASSUMPTIONS
  CheckAssumptions();
#elif defined(DEBUG_ASSERT)
  Assert(!nil->red,"nil not black in RedBlackTree::DeleteFixUp");
#endif
}


/***********************************************************************/
/*  FUNCTION:  DeleteNode */
/**/
/*    INPUTS:  tree is the tree to delete node z from */
/**/
/*    OUTPUT:  returns the RedBlackEntry stored at deleted node */
/**/
/*    EFFECT:  Deletes z from tree and but don't call destructor */
/**/
/*    Modifies Input:  z */
/**/
/*    The algorithm from this function is from _Introduction_To_Algorithms_ */
/***********************************************************************/

RedBlackEntry * RedBlackTree::DeleteNode(RedBlackTreeNode * z)
{
  RedBlackTreeNode* y;
  RedBlackTreeNode* x;
  RedBlackEntry * returnValue = z->storedEntry;

  y= ((z->left == nil) || (z->right == nil)) ? z : GetSuccessorOf(z);
  x= (y->left == nil) ? y->right : y->left;
  if (root == (x->parent = y->parent)) { /* assignment of y->p to x->p is intentional */
    root->left=x;
  } else {
    if (y == y->parent->left) {
      y->parent->left=x;
    } else {
      y->parent->right=x;
    }
  }
  if (y != z) { /* y should not be nil in this case */

#ifdef DEBUG_ASSERT
    Assert( (y!=nil),"y is nil in DeleteNode \n");
#endif
    /* y is the node to splice out and x is its child */
  
    y->left=z->left;
    y->right=z->right;
    y->parent=z->parent;
    z->left->parent=z->right->parent=y;
    if (z == z->parent->left) {
      z->parent->left=y; 
    } else {
      z->parent->right=y;
    }
    if (!(y->red)) {
      y->red = z->red;
      DeleteFixUp(x);
    } else
      y->red = z->red; 
    delete z;
#ifdef CHECK_RB_TREE_ASSUMPTIONS
    CheckAssumptions();
#elif defined(DEBUG_ASSERT)
    Assert(!nil->red,"nil not black in RedBlackTree::Delete");
#endif
  } else {
    if (!(y->red)) DeleteFixUp(x);
    delete y;
#ifdef CHECK_RB_TREE_ASSUMPTIONS
    CheckAssumptions();
#elif defined(DEBUG_ASSERT)
    Assert(!nil->red,"nil not black in RedBlackTree::Delete");
#endif
  }
  return returnValue;
}


/***********************************************************************/
/*  FUNCTION:  Enumerate */
/**/
/*    INPUTS:  tree is the tree to look for keys between [low,high] */
/**/
/*    OUTPUT:  stack containing pointers to the nodes between [low,high] */
/**/
/*    Modifies Input: none */
/**/
/*    EFFECT:  Returns a stack containing pointers to nodes containing */
/*             keys which in [low,high]/ */
/**/
/***********************************************************************/

TemplateStack<RedBlackTreeNode *> * RedBlackTree::Enumerate(int low, 
							    int high)
{
  TemplateStack<RedBlackTreeNode *> * enumResultStack = 
    new TemplateStack<RedBlackTreeNode *>(4);

  RedBlackTreeNode* x=root->left;
  RedBlackTreeNode* lastBest=NULL;

  while(nil != x) {
    if ( x->key > high ) {
      x=x->left;
    } else {
      lastBest=x;
      x=x->right;
    }
  }
  while ( (lastBest) && (low <= lastBest->key) ) {
    enumResultStack->Push(lastBest);
    lastBest=GetPredecessorOf(lastBest);
  }
  return(enumResultStack);
}

void RedBlackTree::CheckAssumptions() const
{
 VERIFY(nil->key == MIN_INT);
 VERIFY(root->key == MAX_INT);
 VERIFY(nil->storedEntry == NULL);
 VERIFY(root->storedEntry == NULL);
 VERIFY(nil->red == 0);
 VERIFY(root->red == 0);
}
 


