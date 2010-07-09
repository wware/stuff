#include <tcl.h>
#include <stdio.h>
#include "interval_tree.h"
#include "RedBlackTree.h"
#include "sets.H"

static SetDatabaseHandle<char*,IntervalTree *> 
global_IntervalTreesInTcl(TCL_STRING_KEYS);

static SetDatabaseHandle<char*,RedBlackTree *> 
global_RedBlackTreesInTcl(TCL_STRING_KEYS);

class SimpleInterval : public Interval {
public:
  SimpleInterval(const int low,const int high)
    :_low(low), _high(high)
    { }
  
  int GetLowPoint() const { return _low;}
  int GetHighPoint() const { return _high;}
  IntervalTreeNode * GetNode() { return _node;}
  void SetNode(IntervalTreeNode * node) {_node = node;}
protected:
  int _low;
  int _high;
  IntervalTreeNode * _node;
};

class SimpleKey : public RedBlackEntry {
public:
  SimpleKey(const int key)
    :_key(key) {
  }
  int GetKey() const { return _key;}
protected:
  int _key;
};

int CreateIntervalTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			    char ** argv)
{
  if (argc != 2) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: intTreeCreate nameOfTree\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL != global_IntervalTreesInTcl.Search(argv[1])) {
    Tcl_SetResult(interp,
		  "An interval tree with that name already exists!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    global_IntervalTreesInTcl.Add(argv[1],new IntervalTree);
    return TCL_OK;
  }
}

int DeleteIntervalTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			    char ** argv)
{
  IntervalTree * treeToDelete;
  if (argc != 2) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: intTreeDelete nameOfTree\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (treeToDelete = 
		      global_IntervalTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "An interval tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    delete treeToDelete;
    global_IntervalTreesInTcl.Delete(argv[1]);
    return TCL_OK;
  }
}

int QueryIntervalTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int low;
  int high;
  IntervalTree * tree;
  if (argc != 4) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: intTreeQuery name low high \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_IntervalTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "An interval tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if ( (TCL_OK != Tcl_GetInt(interp,argv[2],&low)) ||
	 (TCL_OK != Tcl_GetInt(interp,argv[3],&high)) )
      return TCL_ERROR;
    else {
      TemplateStack<void *> * queryResults = tree->Enumerate(low,high);
      Tcl_Obj * listResult = Tcl_NewListObj(0,NULL);
      for (int i=0, last = queryResults->Size(); i < last; i++) {
	char buffer[100];
	Interval * currentInterval = (Interval *) (*queryResults)[i];
	Tcl_Obj * intervalList = Tcl_NewListObj(0,NULL);
	sprintf(buffer,"%i",currentInterval->GetLowPoint());
	Tcl_ListObjAppendElement(interp,intervalList,
				 Tcl_NewStringObj(buffer,-1));
	sprintf(buffer,"%i",currentInterval->GetHighPoint());
	Tcl_ListObjAppendElement(interp,intervalList,
				 Tcl_NewStringObj(buffer,-1));
	Tcl_ListObjAppendElement(interp,listResult,intervalList);
      }
      Tcl_SetObjResult(interp,listResult);
      return TCL_OK;
    }
  }
}

int AddIntervalToTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int low;
  int high;
  IntervalTree * tree;
  if (argc != 4) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: intTreeAdd name low high \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_IntervalTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "An interval tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if ( (TCL_OK != Tcl_GetInt(interp,argv[2],&low)) ||
	 (TCL_OK != Tcl_GetInt(interp,argv[3],&high)) )
      return TCL_ERROR;
    else {
      SimpleInterval * newSimpleInterval = new SimpleInterval(low,high);
      newSimpleInterval->SetNode(tree->Insert(newSimpleInterval));
      return TCL_OK;
    }
  }
}

int IntervalTreeRemoveNode(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int low;
  int high;
  IntervalTree * tree;
  if (argc != 4) {
    Tcl_SetResult(interp,
		  "Wrong # args.\nUsage: intTreeRemoveNode name low high \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_IntervalTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "An interval tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if ( (TCL_OK != Tcl_GetInt(interp,argv[2],&low)) ||
	 (TCL_OK != Tcl_GetInt(interp,argv[3],&high)) )
      return TCL_ERROR;
    else {
      TemplateStack<void *> * queryResults = tree->Enumerate(low,high);
      for (int i=0, last = queryResults->Size(); i < last; i++) {
	SimpleInterval * currentInterval = 
	  (SimpleInterval *) (*queryResults)[i];
	if ( (currentInterval->GetLowPoint() == low) &&
	     (currentInterval->GetHighPoint() == high) ) {
	  tree->DeleteNode(currentInterval->GetNode());
	  delete currentInterval;
	  return TCL_OK;
	}
      }
      Tcl_SetResult(interp,"Interval not found in tree!\n",TCL_STATIC);
      return TCL_ERROR;
    }
  }
}



// red black tree stuff follows:

int CreateRedBlackTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			    char ** argv)
{
  if (argc != 2) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: rbTreeCreate nameOfTree\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL != global_RedBlackTreesInTcl.Search(argv[1])) {
    Tcl_SetResult(interp,
		  "A red black tree with that name already exists!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    global_RedBlackTreesInTcl.Add(argv[1],new RedBlackTree);
    return TCL_OK;
  }
}

int DeleteRedBlackTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			    char ** argv)
{
  RedBlackTree * treeToDelete;
  if (argc != 2) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: rbTreeDelete nameOfTree\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (treeToDelete = 
		      global_RedBlackTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "A red black tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    delete treeToDelete;
    global_RedBlackTreesInTcl.Delete(argv[1]);
    return TCL_OK;
  }
}


int AddToRedBlackTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int key;
  RedBlackTree * tree;
  if (argc != 3) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: rbTreeAdd name key \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_RedBlackTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "A red black tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if (TCL_OK != Tcl_GetInt(interp,argv[2],&key))
      return TCL_ERROR;
    else {
      SimpleKey * newSimpleKey = new SimpleKey(key);
      tree->Insert(newSimpleKey);
      return TCL_OK;
    }
  }
}

int QueryRedBlackTreeInTcl(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int low;
  int high;
  RedBlackTree * tree;
  if (argc != 4) {
    Tcl_SetResult(interp,
		  "Wrong # arguments.\nUsage: rbTreeQuery name low high \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_RedBlackTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "A red black tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if ( (TCL_OK != Tcl_GetInt(interp,argv[2],&low)) ||
	 (TCL_OK != Tcl_GetInt(interp,argv[3],&high)) )
      return TCL_ERROR;
    else {
      TemplateStack<RedBlackTreeNode *> * queryResults = 
	tree->Enumerate(low,high);
      Tcl_Obj * listResult = Tcl_NewListObj(0,NULL);
      for (int i=0, last = queryResults->Size(); i < last; i++) {
	char buffer[100];
	RedBlackEntry * currentEntry = (*queryResults)[i]->GetEntry();
	sprintf(buffer,"%i",currentEntry->GetKey());
	Tcl_ListObjAppendElement(interp,listResult,
				 Tcl_NewStringObj(buffer,-1));
      }
      Tcl_SetObjResult(interp,listResult);
      return TCL_OK;
    }
  }
}

int RedBlackTreeRemoveNode(ClientData, Tcl_Interp * interp, int argc,
			   char ** argv)
{
  int low;
  int high;
  RedBlackTree * tree;
  if (argc != 4) {
    Tcl_SetResult(interp,
		  "Wrong # args.\nUsage: rbTreeRemoveNode name low high \n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else if (NULL == (tree = global_RedBlackTreesInTcl.Search(argv[1]))) {
    Tcl_SetResult(interp,
		  "A red black tree with that name doesn't exist!\n",
		  TCL_STATIC);
    return TCL_ERROR;
  } else {
    if ( (TCL_OK != Tcl_GetInt(interp,argv[2],&low)) ||
	 (TCL_OK != Tcl_GetInt(interp,argv[3],&high)) )
      return TCL_ERROR;
    else {
      TemplateStack<RedBlackTreeNode *> * queryResults = 
	tree->Enumerate(low,high);
      for (int i=0, last = queryResults->Size(); i < last; i++) {
	RedBlackEntry * curEntry = (*queryResults)[i]->GetEntry();
	if ((low <= curEntry->GetKey()) && (curEntry->GetKey() <= high)) {
	  tree->DeleteNode( (*queryResults)[i]);
	  delete curEntry;
	  return TCL_OK;
	}
      }
      Tcl_SetResult(interp,"no matching keys found in tree!\n",TCL_STATIC);
      return TCL_ERROR;
    }
  }
}

#ifdef TCL_XT_TEST
#include <X11/Intrinsic.h>
#endif

#ifdef TCL_TEST
EXTERN int		TclObjTest_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int		Tcltest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif /* TCL_TEST */
#ifdef TCL_XT_TEST
EXTERN int		Tclxttest_Init _ANSI_ARGS_((Tcl_Interp *interp));
#endif

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tcl_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int main(int argc, char ** argv)
{
#ifdef TCL_XT_TEST
    XtToolkitInitialize();
#endif
    Tcl_Main(argc, argv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int Tcl_AppInit(Tcl_Interp * interp)
{
    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

#ifdef TCL_TEST
#ifdef TCL_XT_TEST
     if (Tclxttest_Init(interp) == TCL_ERROR) {
	 return TCL_ERROR;
     }
#endif
    if (Tcltest_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "Tcltest", Tcltest_Init,
            (Tcl_PackageInitProc *) NULL);
    if (TclObjTest_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }
#endif /* TCL_TEST */

    /*
     * Call the init procedures for included packages.  Each call should
     * look like this:
     *
     * if (Mod_Init(interp) == TCL_ERROR) {
     *     return TCL_ERROR;
     * }
     *
     * where "Mod" is the name of the module.
     */

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */
    Tcl_CreateCommand(interp, "intTreeCreate",
		      CreateIntervalTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "intTreeDelete",
		      DeleteIntervalTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "intTreeAdd",
		      AddIntervalToTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "intTreeQuery",
		      QueryIntervalTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "intTreeRemoveNode",
		      IntervalTreeRemoveNode, NULL, NULL);


    Tcl_CreateCommand(interp, "rbTreeCreate",
		      CreateRedBlackTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "rbTreeDelete",
		      DeleteRedBlackTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "rbTreeAdd",
		      AddToRedBlackTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "rbTreeQuery",
		      QueryRedBlackTreeInTcl, NULL, NULL);

    Tcl_CreateCommand(interp, "rbTreeRemoveNode",
		      RedBlackTreeRemoveNode, NULL, NULL);
    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY);
    return TCL_OK;
}
