###########################################
# Red Black Trees Implementation - Python #
# Authors: Marisol Gonzalez Coto,         #
#          Izcar Muñoz Torrez,            #
#          Jonathan Rodríguez Martínez    #
# Class: Programming Languages            #
# Group: 40                               #
# Teacher: Jose Castro                    #
###########################################

# Imports
import sys

""" Class Declaration Start """

# Regular Node Class
class Node:
  # Static values to distinguish red and black nodes.
  RED = True
  BLACK = False

  # Regular node constructor.
  def __init__(self, key, color = RED):
    self.color = color
    self.key = key
    self.left = self.right = self.parent = NullNode.instance()

  def __str__(self, level = 0, indent = "   "):
    string = level * indent + str(self.key)
    if self.right:
      string = string + "\n" + self.right.__str__(level + 1, indent)
    if self.left:
      string = string + "\n" + self.left.__str__(level + 1, indent)
    return string

  def __nonzero__(self):
    return True

  def __bool__(self):
    return True

# Null node class. Used to insert new nodes.
class NullNode(Node):
  __instance__ = None

  # Implemented as singleton so only one root node exists.
  @classmethod
  def instance(self):
    if self.__instance__ is None:
      self.__instance__ = NullNode()
    return self.__instance__

  # Null node constructor.
  def __init__(self):
    self.color = Node.BLACK
    self.key = None
    self.left = self.right = self.parent = None

  def __nonzero__(self):
    return False

  def __bool__(self):
    return False

class RBTree:
  nodeType = None
  # Tree constructor.
  def __init__(self, nodeType):
    self.root = NullNode.instance()
    self.size = 0
    self.nodeType = nodeType

  def getType(self):
    return self.nodeType
    
  def __str__(self):
    return ("(Tree size = %d)\n" % self.size)  + str(self.root)

  # Main insert method.
  def addNode(self, key):
    self.insertNode(Node(key))

  # Auxiliary insert function.
  def insertNode(self, newNode):
    # Inserts new node in correct location.
    self.insertAux(newNode)

    # All new nodes are set to red in the beginning.
    newNode.color = Node.RED
    # New node validations.
    while newNode != self.root and newNode.parent.color == Node.RED:
      # Node was inserted to the right of the root.
      if newNode.parent == newNode.parent.parent.left:
        temp = newNode.parent.parent.right
        # Case 1 Fix: Change colors.
        if temp and temp.color == Node.RED:
          newNode.parent.color = Node.BLACK
          temp.color = Node.BLACK
          newNode.parent.parent.color = Node.RED
          newNode = newNode.parent.parent
        # Case 2 Fix: Rotate, then change colors.
        else:
          if newNode == newNode.parent.right:
            newNode = newNode.parent
            self.rotateLeft(newNode)
          newNode.parent.color = Node.BLACK
          newNode.parent.parent.color = Node.RED
          self.rotateRight(newNode.parent.parent)
      # Node was inserted to the left of the root.
      else:
        temp = newNode.parent.parent.left
        # Case 1 Fix: Change colors.
        if temp and temp.color == Node.RED:
          newNode.parent.color = Node.BLACK
          temp.color = Node.BLACK
          newNode.parent.parent.color = Node.RED
          newNode = newNode.parent.parent
        # Case 2 Fix: Rotate, then fix colors.
        else:
          if newNode == newNode.parent.left:
            newNode = newNode.parent
            self.rotateRight(newNode)
          newNode.parent.color = Node.BLACK
          newNode.parent.parent.color = Node.RED
          self.rotateLeft(newNode.parent.parent)
    self.root.color = Node.BLACK

  # Functions used to find correct placement for new node in tree.
  # Searches tree for correct location based on key.
  def insertAux(self, newNode):
    temp = NullNode.instance()
    rootNode = self.root
    # Case: Tree is not empty, searches tree.
    while rootNode:
      temp = rootNode
      if newNode.key < rootNode.key:
        rootNode = rootNode.left
      else:
        rootNode = rootNode.right

    # Tree is empty, new node is root.
    newNode.parent = temp
    if not temp:
      self.root = newNode
    else:
      if newNode.key < temp.key:
        temp.left = newNode
      else:
        temp.right = newNode
    # Increase tree size.
    self.size += 1
    
  # Main delete function.
  def deleteNode(self, key):
    node = self.searchNode(key)
    # Checks if node is leaf.
    if not node.left or not node.right:
      temp = node
    # If it's not a leaf, finds successor.
    else:
      temp = self.getSuccessor(node)
    # Moves brach to parent node.
    if not temp.left:
      temp2 = temp.right
    else:
      temp2 = temp.left
    temp2.parent = temp.parent

    # Node is root.
    if not temp.parent:
      self.root = temp2
    else:
      # Moves branch to other side of tree.
      if temp == temp.parent.left:
        temp.parent.left = temp2
      else:
        temp.parent.right = temp2

    if temp != node:
      node.key = temp.key

    # Calls delete fixup function.
    if temp.color == Node.BLACK:
      self.deleteAux(temp2)

    # Decreases size.
    self.size -= 1
    return temp

  # Auxiliary delete function. Handles violation cases and fixups.
  def deleteAux(self, node):
    while node != self.root and node.color == Node.BLACK:
      # Deleted node on the left.
      if node == node.parent.left:
        rightBrother = node.parent.right
        # Case 1: Rotate, then fix color red if necessary.
        if rightBrother.color == Node.RED:
          rightBrother.color = Node.BLACK
          node.parent.color = Node.RED
          self.rotateLeft(node.parent)
          rightBrother = node.parent.right
        # Color fix.
        if rightBrother.left.color == Node.BLACK and rightBrother.right.color == Node.BLACK:
          rightBrother.color = Node.RED
          node = node.parent
        else:
          # Fix color black after right rotation.
          if rightBrother.right.color == Node.BLACK:
            rightBrother.left.color = Node.BLACK
            rightBrother.color = Node.RED
            self.rotateRight(rightBrother)
            rightBrother = node.parent.right
          rightBrother.color = node.parent.color
          node.parent.color = Node.BLACK
          rightBrother.right.color = Node.BLACK
          self.rotateLeft(node.parent)
          node = self.root
      # Deleted node on the right.
      else:
        leftBrother = node.parent.left
        # Color fix: node is red.
        if w.color == Node.RED:
          w.color = Node.BLACK
          node.parent.color = Node.RED
          self.rotateRight(node.parent)
          leftBrother = node.parent.left
        if leftBrother.right.color == Node.BLACK and leftBrother.left.color == Node.BLACK:
          leftBrother.color = Node.RED
          node = node.parent
        else:
          # Rotate, then fix color.
          if leftBrother.left.color == Node.BLACK:
            leftBrother.right.color = Node.BLACK
            leftBrother.color = Node.RED
            self.rotateLeft(leftBrother)
            leftBrother = node.parent.left
          leftBrother.color = node.parent.color
          node.parent.color = Node.BLACK
          leftBrother.left.color = Node.BLACK
          self.rotateRight(node.parent)
          node = root
    node.color = Node.BLACK

  def clear(self):
    while self.root:
        self.deleteNode(self.getMin().key)

  # Rotate left function. Used in insert and delete fixups.
  def rotateLeft(self, node):
    # Case: Rotation not possible.
    if not node.right:
      raise "No right child!"
    temp = node.right
    node.right = temp.left
    if temp.left:
      temp.left.parent = node
    temp.parent = node.parent
    # Checks if node is root after rotation.
    if not node.parent:
      self.root = temp
    else:
      if node == node.parent.left:
        node.parent.left = temp
      else:
        node.parent.right = temp
    temp.left = node
    node.parent = temp

  # Rotate right function. Used in insert and delete fixups.
  def rotateRight(self, node):
    # Case: Rotation not possible.
    if not node.left:
      raise "No left child!"
    temp = node.left
    node.left = temp.right
    if temp.right:
      temp.right.parent = node
    temp.parent = node.parent
    # Checks if node is root after rotation.
    if not node.parent:
      self.root = temp
    else:
      if node == node.parent.left:
        node.parent.left = temp
      else:
        node.parent.right = temp
    temp.right = node
    node.parent = temp

  # Get minimum function.Used in successor function.
  def getMin(self, node = None):
    if node is None:
      node = self.root
    while node.left:
      node = node.left
    return node
  
  # Successor function. Used to delete nodes and during the
  # inorder walk.
  def getSuccessor(self, node):
    # If there's a left child, get the left-most
    # node in right subtree.
    if node.right:
      return self.getMin(node.right)
    # If not, search using parent and upper subtrees.
    temp = node.parent
    while temp and node == temp.right:
      node = temp
      temp = temp.parent
    return temp

  # Inorder walk.
  def inorder(self, start = None):
    print("\nInorder Walk\n\n")
    if start is None:
      start = self.root
    start = self.getMin()
    while start:
      if self.getSuccessor(start):
          print(str(start.key) + ", ")
      else:
          print(start.key, "\n")
      start = self.getSuccessor(start)

  # Regular binary search by key. Returns node object.
  def searchNode(self, key, node = None):
    # Tree is empty.
    if node is None:
      node = self.root
    while node and node.key != key:
      # Look left.
      if key < node.key:
        node = node.left
      # Look right.
      else:
        node = node.right
    return node

  # Tree is empty.
  def isEmpty(self):
    return bool(self.root)

""" Class Declaration End """

""" Controller """

class Controller:
    tree = None

    def __init__(self):
        pass

    def createTree(self, nodeType):
        self.tree = RBTree(nodeType)

    def checkValue(self, value):
        nodeType = self.tree.getType()
        if nodeType == 1:
            try:
                value = int(value)
                return True
            except:
                return False
        else:
            if type(value) == str:
                return True
        return False

    def insert(self, value):
        nodeType = self.tree.getType()
        if nodeType == 1:
            self.tree.addNode(int(value))
        else:
            self.tree.addNode(value)

    def delete(self, value):
        try:
            nodeType = self.tree.getType()
            if nodeType == 1:
                self.tree.deleteNode(int(value))
            else:
                self.tree.deleteNode(value)
            print("\nNode deleted.\n")
        except:
            print("\nError: Key does not exist.\n")

    def search(self, value):
        try:
            nodeType = self.tree.getType()
            if nodeType == 1:
                res = self.tree.searchNode(int(value))
            else:
                res = self.tree.searchNode(value)
            if res:
                print("\nFound key!\n")
            else:
                print("\nKey not found.\n")
        except:
            print("\nError: Key not found.\n")

    def clear(self):
        self.tree.clear()

    def inorder(self):
        self.tree.inorder()

    def prettyPrint(self):
        print(self.tree.__str__())
        print("\n")

    def treeMenu(self):
        print("PLEASE SELECT AN OPTION \n")
        print("1 - Integer RBTree")
        print("2 - String RBTree")
        print("3 - Exit")

    def mainMenu(self):
        print("PLEASE SELECT AN OPTION \n")
        print("1 - Insert Node")
        print("2 - Delete Node")
        print("3 - Search Element")
        print("4 - Inorder Walk")
        print("5 - Pretty Print Tree")
        print("6 - Clear Tree")
        print("7 - Exit")

    def getValidChoiceTreeMenu(self):
        self.treeMenu()
        try:
            choice = int(input("\nOption: "))
            if choice > 0 and choice < 4:
                return choice
            else:
                print("\n\nPlease enter a number from 1 to 3.\n\n")
                return self.getValidChoiceTreeMenu()
        except:
            print("\n\nPlease enter a valid integer.\n\n")
            return self.getValidChoiceTreeMenu()

    def getValidChoiceMainMenu(self):
        self.mainMenu()
        try:
            choice = int(input("\nOption: "))
            if choice > 0 and choice < 8:
                return choice
            else:
                print("\n\nPlease enter a number from 1 to 8.\n\n")
                return self.getValidChoiceMainMenu()
        except:
            print("\n\nPlease enter a valid integer.\n\n")
            return self.getValidChoiceMainMenu()
                     
""" Controller End """
    
""" Main Program """
def main():
    controller = Controller()
    nodeType = controller.getValidChoiceTreeMenu()
    if nodeType == 1:
        controller.createTree(1)
        print("\nInteger RBTree Created.\n")
    elif nodeType == 2:
        controller.createTree(2)
        print("\nString RBTree Created.\n")
    else:
        sys.exit()

    end = False
    while not end:
        choice = controller.getValidChoiceMainMenu()
        if choice == 1:
            value = input("\nKey: ")
            if controller.checkValue(value):
                controller.insert(value)
                print("\nNode inserted.\n")
                controller.prettyPrint()
            else:
                print("\nInvalid key.\n")
        elif choice == 2:
            value = input("\nKey: ")
            controller.delete(value)
            controller.prettyPrint()
        elif choice == 3:
            value = input("\nKey: ")
            controller.search(value)
        elif choice == 4:
            controller.inorder()
        elif choice == 5:
            controller.prettyPrint()
        elif choice == 6:
            controller.clear()
            print("\nTree cleared.\n")
        else:
            end = True
    
""" Main Program End """

main()
  
