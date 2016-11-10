object RedBlackTree {

    object Color 
    {
        val RED: Boolean = true;
        val BLACK: Boolean = false; 

    }

    object NullNode extends Node { 

        key = null.asInstanceOf[Int];
        color = Color.BLACK;
        left = NullNode;
        right = NullNode;
        parent = NullNode;
        isNone = false;

        override def valueBool() = false;

    }

    object NoneNode extends Node { 

        key = null.asInstanceOf[Int];
        color = Color.BLACK;
        left = NullNode;
        right = NullNode;
        parent = NullNode;
        isNone = true;
        override def valueBool() = false;

    }

    class Node(pKey: Int = null.asInstanceOf[Int], pColor: Boolean = Color.RED)
    {
        var key: Int = pKey;
        var color: Boolean = pColor;
        var left: Node = NullNode;
        var right: Node = NullNode;
        var parent: Node = NullNode;
        var isNone: Boolean = false;

        def valueBool() = true;

        def str( pLevel: Int = 0, pIndent: String = "   "): String = 
        {

            val level = pLevel;
            val indent = pIndent;

            var string = indent * level + key + (if (color) "[R]" else "[B]");
            if (left.valueBool()){
              string = string + "\n" + left.str(level + 1, indent);
            }
            if (right.valueBool()){

              string = string + "\n" + right.str(level + 1, indent);
            }
            return string;
        }


    }

    class RBTree()
    {
        var root: Node = NullNode;
        var size: Int = 0; 
        /*toString*/
        def str():String = 
        {
            return "root.size = " + size + "\n"  + root.str();

        } 

        def addNode(pKey: Int)
        {   var newNode:Node = new Node(pKey);
            insertNode(newNode);
        }



        def insertNode(pNewNode: Node)
        {
            var newNode = pNewNode;
            //Inserts new node in correct location.
            insertAux(newNode);

            newNode.color = Color.RED;

            while(newNode != root && newNode.parent.color == Color.RED){


                // Node was inserted to the right of the root.
                if (newNode.parent == newNode.parent.parent.left){

                    var temp: Node = newNode.parent.parent.right;
                    

                    // Case 1 Fix: Change colors.
                    if (temp.valueBool() && temp.color == Color.RED){

                      newNode.parent.color = Color.BLACK;
                      temp.color = Color.BLACK;

                      newNode.parent.parent.color = Color.RED;
                      newNode = newNode.parent.parent;
                    }

                    // Case 2 Fix: Rotate, then change colors.

                    else{
                        if (newNode == newNode.parent.right){
                            newNode = newNode.parent;
                            rotateLeft(newNode);
                        }

                        newNode.parent.color = Color.BLACK;
                        newNode.parent.parent.color = Color.RED;
                        rotateRight(newNode.parent.parent);
                    }
                }

                //Node was inserted to the left of the root.
                else{
                    var temp: Node = newNode.parent.parent.left;
                    // Case 1 Fix: Change colors.
                    if (temp.valueBool() && temp.color == Color.RED){


                      newNode.parent.color = Color.BLACK;
                      temp.color = Color.BLACK;
                      newNode.parent.parent.color = Color.RED;
                      newNode = newNode.parent.parent;

                    }
                    //Case 2 Fix: Rotate, then fix colors.
                    else{

                        if (newNode == newNode.parent.left){
                            newNode = newNode.parent;
                            rotateRight(newNode);
                        } 

                        newNode.parent.color = Color.BLACK;
                        newNode.parent.parent.color = Color.RED;
                        rotateLeft(newNode.parent.parent);
                    }


                }
                
            }
            root.color = Color.BLACK;

        }

        //Functions used to find correct placement for new node in tree.
        //Searches tree for correct location based on key.
        def insertAux(pNewNode: Node)
        {

            var newNode: Node = pNewNode;
            var temp: Node = NullNode;
            var rootNode: Node = root;

            //Case: Tree is not empty, searches tree.
            while (rootNode.valueBool()){

                temp = rootNode;

                if (newNode.key < rootNode.key){
                    rootNode = rootNode.left;
                }

                else{
                    rootNode = rootNode.right;
                }
            }

            //Tree is empty, new node is root.
            newNode.parent = temp;
            if (!temp.valueBool()){
                root = newNode;
            }
            else{

                if (newNode.key < temp.key){
                    temp.left = newNode
                }
                else{

                    temp.right = newNode
                }
            }
            //Increase tree size.
            size += 1;
            

        }
        // Main delete function.
        def deleteNode(pNode: Node): Node = {
            // Checks if node is leaf.
            var node: Node = pNode;
            var temp: Node = NullNode;
            var temp2: Node = NullNode;

            if (!node.left.valueBool() || !node.right.valueBool()){
                temp = node;

            }
            // If it's not a leaf, finds successor.
            else{
                temp = getSuccessor(node);

            }

            // Moves brach to parent node.
            if (temp.left.valueBool()){
                temp2 = temp.right;
            }
            else{
                temp2 = temp.left;
            }

            temp2.parent = temp.parent;

            // Node is root.
            if (!temp.parent.valueBool()){
                root = temp2;
            }
            else{
              // Moves branch to other side of tree.
                if (temp == temp.parent.left){
                    temp.parent.left = temp2;
                }
                else{
                    temp.parent.right = temp2;
                }
            }

            if (temp != node){

                node.key = temp.key;

            }

            // Calls delete fixup function.
            if (temp.color == Color.BLACK){
                 
                
                deleteAux(temp2);
            }

            // Decreases size.
            size -= 1;

            return temp;
        }

        // Get minimum function.Used in successor function.
        def getMin(pNode: Node = NoneNode): Node = {

            var node: Node = pNode;

            if (node.isNone){
                node = root;
            }
            while (node.left.valueBool()){

                node = node.left;
            }

            return node
        }
  
      // Successor function. Used to delete nodes and during the
      // inorder walk.
        def getSuccessor(pNode: Node): Node = {
            // If there's a left child, get the left-most
            // node in right subtree.
            var node: Node = pNode;
            if (node.right.valueBool()){

              return getMin(node.right);
            }
            // If not, search using parent and upper subtrees.
            var temp: Node = node.parent;
            while (temp.valueBool() && node == temp.right){
                node = temp;
                temp = temp.parent;
            }

            return temp;
        }

        // Inorder walk.
        def inorder(pStart: Node = NoneNode){

            var start: Node = pStart;
            if (start.isNone){
                start = root;
            }

            start = getMin();
            var all = "";
            while(start.valueBool()){
            all +="(" + start.key + ")" + ":"  
            start = getSuccessor(start);
            }
            println(all)
        }

        // Regular binary search by key. Returns node object.
        def searchNode(pKey: Int, pNode: Node = NoneNode): Node = {
            // Tree is empty.
            var node: Node = pNode;
            var key: Int = pKey;

            if (node.isNone){
                node = root
            }
            while (node.valueBool() && node.key != key)
                // Look left.
                if (key < node.key){
                    node = node.left
                }
                // Look right.
                else{
                    node = node.right
                }
                return node
        }
        // Tree is empty.
        def isEmpty():Boolean = {
                return root.valueBool();
        }
        def throwsExceptionRight() {
            throw new IllegalStateException("No right child!");
        }
        def throwsExceptionLeft() {
            throw new IllegalStateException("No left child!");
        }
        // Rotate left function. Used in insert and delete fixups.
        def rotateLeft(pNode: Node){
        // Case: Rotation not possible.
            var node: Node = pNode;

            if (!node.right.valueBool()){
                throwsExceptionRight();
            }

            var temp: Node =  node.right;

            node.right = temp.left;
            if (temp.left.valueBool()){
                temp.left.parent = node;
            }

            temp.parent = node.parent;
            // Checks if node is root after rotation.
            if (!node.parent.valueBool()){
                root = temp;
            }
            else{
                if (node == node.parent.left){
                    node.parent.left = temp;
                }
                else{
                    node.parent.right = temp;
                }
            }
            temp.left = node;
            node.parent = temp;
        }

        // Rotate right function. Used in insert and delete fixups.
        def rotateRight(pNode: Node){
            // Case: Rotation not possible.
            var node: Node = pNode;
            if(!node.left.valueBool()){
                throwsExceptionLeft();
            }

            var temp: Node = node.left;
            node.left = temp.right
            if (temp.right.valueBool()){
                temp.right.parent = node;
            }

            temp.parent = node.parent;
            // Checks if node is root after rotation.
            if(!node.parent.valueBool()){
                root = temp;
            }
            else{
                if (node == node.parent.left){
                    node.parent.left = temp;
                }
                else{
                    node.parent.right = temp;
                }
            }
            temp.right = node;
            node.parent = temp;
        }

        def deleteAux(pNode: Node){
            var node: Node = pNode;
            while (node != root && node.color == Color.BLACK){
                if (node == node.parent.left){
                    var rightBrother: Node = node.parent.right;

                    if (rightBrother.color == Color.RED){
                        rightBrother.color = Color.BLACK;
                        node.parent.color = Color.RED;
                        rotateLeft(node.parent);
                        rightBrother = node.parent.right;
                    }
                    if (rightBrother.left.color == Color.BLACK && rightBrother.right.color == Color.BLACK){
                        rightBrother.color = Color.RED;
                        node = node.parent;
                    }
                    else{
                        if (rightBrother.right.color == Color.BLACK){
                            rightBrother.left.color = Color.BLACK;
                            rightBrother.color = Color.RED;
                            rotateRight(rightBrother);
                            rightBrother = node.parent.right;
                    }
                    rightBrother.color = node.parent.color;
                    node.parent.color = Color.BLACK;
                    rightBrother.right.color = Color.BLACK;
                    rotateLeft(node.parent);
                    node = root;
                  }
                }
                else{

                    var leftBrother: Node = node.parent.left;
                    if (leftBrother.color == Color.RED){
                        leftBrother.color = Color.BLACK;
                        node.parent.color = Color.RED;
                        rotateRight(node.parent);
                        leftBrother = node.parent.left
                    }
                    if (leftBrother.right.color == Color.BLACK && leftBrother.left.color == Color.BLACK){
                        leftBrother.color = Color.RED;
                        node = node.parent;
                    }
                    else{
                        if (leftBrother.left.color == Color.BLACK){
                            leftBrother.right.color = Color.BLACK;
                            leftBrother.color = Color.RED;
                            rotateLeft(leftBrother);
                            leftBrother = node.parent.left;
                        }
                        leftBrother.color = node.parent.color;
                        node.parent.color = Color.BLACK;
                        leftBrother.left.color = Color.BLACK;
                        rotateRight(node.parent)
                        node = root;
                    }
                }
            }
            node.color = Color.BLACK// Auxiliary delete function. Handles violation cases and fixups.
        }

    }

        object NullSNode extends SNode { 

        key = null.asInstanceOf[String];
        color = Color.BLACK;
        left = NullSNode;
        right = NullSNode;
        parent = NullSNode;
        isNone = false;

        override def valueBool() = false;

    }

    object NoneSNode extends SNode { 

        key = null.asInstanceOf[String];
        color = Color.BLACK;
        left = NullSNode;
        right = NullSNode;
        parent = NullSNode;
        isNone = true;
        override def valueBool() = false;

    }

    class SNode(pKey: String = null.asInstanceOf[String], pColor: Boolean = Color.RED)
    {
        var key: String = pKey;
        var color: Boolean = pColor;
        var left: SNode = NullSNode;
        var right: SNode = NullSNode;
        var parent: SNode = NullSNode;
        var isNone: Boolean = false;

        def valueBool() = true;

        def str( pLevel: Int = 0, pIndent: String = "   "): String = 
        {

            val level = pLevel;
            val indent = pIndent;

            var string = indent * level + key + (if (color) "[R]" else "[B]");
            if (left.valueBool()){
              string = string + "\n" + left.str(level + 1, indent);
            }
            if (right.valueBool()){

              string = string + "\n" + right.str(level + 1, indent);
            }
            return string;
        }


    }

    class RBSTree()
    {
        var root: SNode = NullSNode;
        var size: Int = 0; 
        /*toString*/
        def str():String = 
        {
            return "root.size = " + size + "\n"  + root.str();

        } 

        def addSNode(pKey: String)
        {   var newSNode:SNode = new SNode(pKey);
            insertSNode(newSNode);
        }



        def insertSNode(pNewSNode: SNode)
        {
            var newSNode = pNewSNode;
            //Inserts new SNode in correct location.
            insertAux(newSNode);

            newSNode.color = Color.RED;

            while(newSNode != root && newSNode.parent.color == Color.RED){


                // SNode was inserted to the right of the root.
                if (newSNode.parent == newSNode.parent.parent.left){

                    var temp: SNode = newSNode.parent.parent.right;
                    

                    // Case 1 Fix: Change colors.
                    if (temp.valueBool() && temp.color == Color.RED){

                      newSNode.parent.color = Color.BLACK;
                      temp.color = Color.BLACK;

                      newSNode.parent.parent.color = Color.RED;
                      newSNode = newSNode.parent.parent;
                    }

                    // Case 2 Fix: Rotate, then change colors.

                    else{
                        if (newSNode == newSNode.parent.right){
                            newSNode = newSNode.parent;
                            rotateLeft(newSNode);
                        }

                        newSNode.parent.color = Color.BLACK;
                        newSNode.parent.parent.color = Color.RED;
                        rotateRight(newSNode.parent.parent);
                    }
                }

                //SNode was inserted to the left of the root.
                else{
                    var temp: SNode = newSNode.parent.parent.left;
                    // Case 1 Fix: Change colors.
                    if (temp.valueBool() && temp.color == Color.RED){


                      newSNode.parent.color = Color.BLACK;
                      temp.color = Color.BLACK;
                      newSNode.parent.parent.color = Color.RED;
                      newSNode = newSNode.parent.parent;

                    }
                    //Case 2 Fix: Rotate, then fix colors.
                    else{

                        if (newSNode == newSNode.parent.left){
                            newSNode = newSNode.parent;
                            rotateRight(newSNode);
                        } 

                        newSNode.parent.color = Color.BLACK;
                        newSNode.parent.parent.color = Color.RED;
                        rotateLeft(newSNode.parent.parent);
                    }


                }
                
            }
            root.color = Color.BLACK;

        }

        //Functions used to find correct placement for new SNode in tree.
        //Searches tree for correct location based on key.
        def insertAux(pNewSNode: SNode)
        {

            var newSNode: SNode = pNewSNode;
            var temp: SNode = NullSNode;
            var rootSNode: SNode = root;

            //Case: Tree is not empty, searches tree.
            while (rootSNode.valueBool()){

                temp = rootSNode;

                if (newSNode.key < rootSNode.key){
                    rootSNode = rootSNode.left;
                }

                else{
                    rootSNode = rootSNode.right;
                }
            }

            //Tree is empty, new SNode is root.
            newSNode.parent = temp;
            if (!temp.valueBool()){
                root = newSNode;
            }
            else{

                if (newSNode.key < temp.key){
                    temp.left = newSNode
                }
                else{

                    temp.right = newSNode
                }
            }
            //Increase tree size.
            size += 1;
            

        }
        // Main delete function.
        def deleteSNode(pSNode: SNode): SNode = {
            // Checks if SNode is leaf.
            var SNode: SNode = pSNode;
            var temp: SNode = NullSNode;
            var temp2: SNode = NullSNode;

            if (!SNode.left.valueBool() || !SNode.right.valueBool()){
                temp = SNode;

            }
            // If it's not a leaf, finds successor.
            else{
                temp = getSuccessor(SNode);

            }

            // Moves brach to parent SNode.
            if (temp.left.valueBool()){
                temp2 = temp.right;
            }
            else{
                temp2 = temp.left;
            }

            temp2.parent = temp.parent;

            // SNode is root.
            if (!temp.parent.valueBool()){
                root = temp2;
            }
            else{
              // Moves branch to other side of tree.
                if (temp == temp.parent.left){
                    temp.parent.left = temp2;
                }
                else{
                    temp.parent.right = temp2;
                }
            }

            if (temp != SNode){

                SNode.key = temp.key;

            }

            // Calls delete fixup function.
            if (temp.color == Color.BLACK){
                 
                
                deleteAux(temp2);
            }

            // Decreases size.
            size -= 1;

            return temp;
        }

        // Get minimum function.Used in successor function.
        def getMin(pSNode: SNode = NoneSNode): SNode = {

            var SNode: SNode = pSNode;

            if (SNode.isNone){
                SNode = root;
            }
            while (SNode.left.valueBool()){

                SNode = SNode.left;
            }

            return SNode
        }
  
      // Successor function. Used to delete SNodes and during the
      // inorder walk.
        def getSuccessor(pSNode: SNode): SNode = {
            // If there's a left child, get the left-most
            // SNode in right subtree.
            var SNode: SNode = pSNode;
            if (SNode.right.valueBool()){

              return getMin(SNode.right);
            }
            // If not, search using parent and upper subtrees.
            var temp: SNode = SNode.parent;
            while (temp.valueBool() && SNode == temp.right){
                SNode = temp;
                temp = temp.parent;
            }

            return temp;
        }

        // Inorder walk.
        def inorder(pStart: SNode = NoneSNode){

            var start: SNode = pStart;
            if (start.isNone){
                start = root;
            }

            start = getMin();
            var all = "";
            while(start.valueBool()){
            all +="(" + start.key + ")" + ":"  
            start = getSuccessor(start);
            }
            println(all)
        }

        // Regular binary search by key. Returns SNode object.
        def searchSNode(pKey: String, pSNode: SNode = NoneSNode): SNode = {
            // Tree is empty.
            var SNode: SNode = pSNode;
            var key: String = pKey;

            if (SNode.isNone){
                SNode = root
            }
            while (SNode.valueBool() && SNode.key != key)
                // Look left.
                if (key < SNode.key){
                    SNode = SNode.left
                }
                // Look right.
                else{
                    SNode = SNode.right
                }
                return SNode
        }
        // Tree is empty.
        def isEmpty():Boolean = {
                return root.valueBool();
        }
        def throwsExceptionRight() {
            throw new IllegalStateException("No right child!");
        }
        def throwsExceptionLeft() {
            throw new IllegalStateException("No left child!");
        }
        // Rotate left function. Used in insert and delete fixups.
        def rotateLeft(pSNode: SNode){
        // Case: Rotation not possible.
            var SNode: SNode = pSNode;

            if (!SNode.right.valueBool()){
                throwsExceptionRight();
            }

            var temp: SNode =  SNode.right;

            SNode.right = temp.left;
            if (temp.left.valueBool()){
                temp.left.parent = SNode;
            }

            temp.parent = SNode.parent;
            // Checks if SNode is root after rotation.
            if (!SNode.parent.valueBool()){
                root = temp;
            }
            else{
                if (SNode == SNode.parent.left){
                    SNode.parent.left = temp;
                }
                else{
                    SNode.parent.right = temp;
                }
            }
            temp.left = SNode;
            SNode.parent = temp;
        }

        // Rotate right function. Used in insert and delete fixups.
        def rotateRight(pSNode: SNode){
            // Case: Rotation not possible.
            var SNode: SNode = pSNode;
            if(!SNode.left.valueBool()){
                throwsExceptionLeft();
            }

            var temp: SNode = SNode.left;
            SNode.left = temp.right
            if (temp.right.valueBool()){
                temp.right.parent = SNode;
            }

            temp.parent = SNode.parent;
            // Checks if SNode is root after rotation.
            if(!SNode.parent.valueBool()){
                root = temp;
            }
            else{
                if (SNode == SNode.parent.left){
                    SNode.parent.left = temp;
                }
                else{
                    SNode.parent.right = temp;
                }
            }
            temp.right = SNode;
            SNode.parent = temp;
        }

        def deleteAux(pSNode: SNode){
            var SNode: SNode = pSNode;
            while (SNode != root && SNode.color == Color.BLACK){
                if (SNode == SNode.parent.left){
                    var rightBrother: SNode = SNode.parent.right;

                    if (rightBrother.color == Color.RED){
                        rightBrother.color = Color.BLACK;
                        SNode.parent.color = Color.RED;
                        rotateLeft(SNode.parent);
                        rightBrother = SNode.parent.right;
                    }
                    if (rightBrother.left.color == Color.BLACK && rightBrother.right.color == Color.BLACK){
                        rightBrother.color = Color.RED;
                        SNode = SNode.parent;
                    }
                    else{
                        if (rightBrother.right.color == Color.BLACK){
                            rightBrother.left.color = Color.BLACK;
                            rightBrother.color = Color.RED;
                            rotateRight(rightBrother);
                            rightBrother = SNode.parent.right;
                    }
                    rightBrother.color = SNode.parent.color;
                    SNode.parent.color = Color.BLACK;
                    rightBrother.right.color = Color.BLACK;
                    rotateLeft(SNode.parent);
                    SNode = root;
                  }
                }
                else{

                    var leftBrother: SNode = SNode.parent.left;
                    if (leftBrother.color == Color.RED){
                        leftBrother.color = Color.BLACK;
                        SNode.parent.color = Color.RED;
                        rotateRight(SNode.parent);
                        leftBrother = SNode.parent.left
                    }
                    if (leftBrother.right.color == Color.BLACK && leftBrother.left.color == Color.BLACK){
                        leftBrother.color = Color.RED;
                        SNode = SNode.parent;
                    }
                    else{
                        if (leftBrother.left.color == Color.BLACK){
                            leftBrother.right.color = Color.BLACK;
                            leftBrother.color = Color.RED;
                            rotateLeft(leftBrother);
                            leftBrother = SNode.parent.left;
                        }
                        leftBrother.color = SNode.parent.color;
                        SNode.parent.color = Color.BLACK;
                        leftBrother.left.color = Color.BLACK;
                        rotateRight(SNode.parent)
                        SNode = root;
                    }
                }
            }
            SNode.color = Color.BLACK// Auxiliary delete function. Handles violation cases and fixups.
        }

    }






































    def main(args: Array[String]) {
        
        var tree = new RBSTree();

      
  tree.addSNode("ee1")
  tree.addSNode("10")
  tree.addSNode("3")
  tree.addSNode("7jajaja")
  
  tree.addSNode("20")
  tree.addSNode("15")
  tree.addSNode("4")
  tree.addSNode("4")
  var Snode: SNode = tree.searchSNode("15");
  tree.deleteSNode(Snode);
  
        

            println(tree.str() +"\n"+ Snode.key) 
        tree.inorder()
        


       ;
    }

}