
# Type Node
type Node
    value
    red
    colorA
    parent
    left
    right
    Node(val) = new(val, false, false, nothing, nothing,nothing)
end

type RBTree
    leaf::Node
    root::Node
end

##Metodos del arbol
function getMax(tree::RBTree, node=nothing)
    if node == nothing
        node = tree.root
    end
    if node == tree.leaf
        return tree.leaf
    end
    while node.right != tree.leaf
        node = node.right
    end
    return node
end

function getMin(tree::RBTree, node=nothing)
    if node == nothing
        node = tree.root
    end
    if node == tree.leaf
        return tree.leaf
    end
    while node.left != tree.leaf
        node = node.left
    end
    return node
end


function search(tree::RBTree, x)
    searchAux(tree, x,tree.root)
end

function searchAux(tree, x, node)
    if node == tree.leaf
        return tree.leaf
    elseif x == node.value
        return node
    elseif x < node.value
        return searchAux(tree, x, node.left)
    elseif x > node.value
        return searchAux(tree, x, node.right)
    end
end

function clear(tree::RBTree)
    tree.root = tree.leaf
end

function insert(tree::RBTree, x)
    insertNode(tree, Node(x))
end

function insertNode(tree::RBTree, newNode)
    x = tree.root
    y = tree.leaf
    while x != tree.leaf
        y = x
        if newNode.value < x.value
            x = x.left
        else
            x = x.right
        end
    end

    newNode.parent = y
    if y == tree.leaf
        tree.root = newNode
    elseif newNode.value < y.value
        y.left = newNode
    else
        y.right = newNode
    end
    newNode.left = tree.leaf
    newNode.right = tree.leaf
    newNode.red = true
    checkInsert(tree, newNode)
end

function checkInsert(tree::RBTree, node)
    while node.parent.red
        if node.parent == node.parent.parent.left
            uncle = node.parent.parent.right
            if uncle.red
                node.parent.red = false
                uncle.red = false
                node.parent.parent.red = true
                node = node.parent.parent
            else
                if node == node.parent.right
                    node = node.parent
                    rotateLeft(tree, node)
                end
                node.parent.red = false
                node.parent.parent.red = true
                rotateRight(tree, node.parent.parent)
            end
        else
            uncle = node.parent.parent.left
            if uncle.red
                node.parent.red = false
                uncle.red = false
                node.parent.parent.red = true
                node = node.parent.parent
            else
                if node == node.parent.left
                    node = node.parent
                    rotateRight(tree, node)
                end
                node.parent.red = false
                node.parent.parent.red = true
                rotateLeft(tree, node.parent.parent)
            end
        end
    end
    tree.root.red = false
end

function replace(tree::RBTree, deleteNode::Node, child::Node)
    if deleteNode.parent == tree.leaf
        tree.root = child
    elseif deleteNode == deleteNode.parent.left
        deleteNode.parent.left = child
    else
        deleteNode.parent.right = child
    end
    child.parent = deleteNode.parent    
end

function delete(tree::RBTree, x)
    node = search(tree, x)
    if node == tree.leaf
        return "Value does not exist"
    end
    return "Deleted: "*string(deleteNode(tree, node))    
end

function deleteNode(tree::RBTree, deleteNode::Node)
    value = deleteNode.value
    nodeAux = deleteNode
    nodeAux.colorA = nodeAux.red
    if deleteNode.left == tree.leaf
        x = deleteNode.right
        replace(tree, deleteNode, deleteNode.right)
    elseif deleteNode.right == tree.leaf
        x = deleteNode.left
        replace(tree, deleteNode, deleteNode.left)
    else
        nodeAux = getMax(tree, deleteNode.left)
        nodeAux.colorA = nodeAux.red
        x = nodeAux.left
        if nodeAux.parent == deleteNode
            x.parent = nodeAux
        else
            replace(tree, nodeAux, nodeAux.left)
            nodeAux.left = deleteNode.left
            nodeAux.left.parent = nodeAux
        end
        replace(tree, deleteNode, nodeAux)
        nodeAux.right = deleteNode.right
        nodeAux.right.parent = nodeAux
        nodeAux.red = deleteNode.red
    end
    if !nodeAux.colorA
        checkDelete(tree,x)
    end
    return value
end

function checkDelete(tree::RBTree, node::Node)
    while node != tree.root && !node.red
        if node == node.parent.left
            uncle = node.parent.right
            if uncle.red
                uncle.red = false
                node.parent.red = true
                rotateLeft(tree, node.parent)
                uncle = node.parent.right
            end
            if !uncle.left.red && !uncle.right.red
                uncle.red = true
                node = node.parent
            elseif !uncle.right.red
                uncle.left.red = false
                uncle.red = true
                rotateRight(tree, uncle)
                uncle = node.parent.right
            end
            uncle.red = node.parent.red
            node.parent.red = false
            uncle.right.red = false
            rotateLeft(tree, node.parent)
            node = tree.root
        else
            uncle = node.parent.left
            if uncle.red
                uncle.red = false
                node.parent.red = true
                rotateRight(tree, node.parent)
                uncle = node.parent.left
            end
            if !uncle.right.red && !uncle.left.red
                uncle.red = true
                node = node.parent
            elseif !uncle.left.red
                uncle.right.red = false
                uncle.red = true
                rotateLeft(tree, uncle)
                uncle = node.parent.left
            end
            uncle.red = node.parent.red
            node.parent.red = false
            uncle.left.red = false
            rotateRight(tree, node.parent)
            node = tree.root
        end
    end
    node.red = false
end


function rotateLeft(tree::RBTree, node::Node)
    nodeAux = node.right
    node.right = nodeAux.left
    if nodeAux.left != tree.leaf
        nodeAux.left.parent = node
    end
    nodeAux.parent = node.parent
    if node.parent == tree.leaf
        tree.root = nodeAux
    elseif node == node.parent.left
        node.parent.left = nodeAux
    else
        node.parent.right = nodeAux
    end
    nodeAux.left = node
    node.parent = nodeAux    
end

function rotateRight(tree::RBTree, node::Node)
    nodeAux = node.left
    node.left = nodeAux.right
    if nodeAux.right != tree.leaf
        nodeAux.right.parent = node
    end
    nodeAux.parent = node.parent
    if node.parent == tree.leaf
        tree.root = nodeAux
    elseif node == node.parent.right
        node.parent.right = nodeAux
    else
        node.parent.left = nodeAux
    end
    nodeAux.right = node
    node.parent = nodeAux    
end

function inorder(tree::RBTree)
    inorderAux(tree,tree.root, [])
end

function inorderAux(tree::RBTree, node::Node, list)
    if node == tree.leaf
        return list
    else
        inorderAux(tree,node.left, list)
        push!(list, node.value)
        inorderAux(tree,node.right, list)
        return list
    end
end

function prettyPrint(tree::RBTree, root=nothing, prefix="")
    if root == nothing
        root = tree.root
    end
    if root == tree.leaf
        println(prefix*"<leaf>")
        return
    end
    color = ""
    if root.red
        color = ", red)"
    else
        color = ", black)"
    end
    println(prefix*"\\-"*"("*string(root.value)*color)
    prettyPrint(tree, root.left, prefix*"|\t")
    prettyPrint(tree, root.right, prefix*"|\t")
end

function printMenu()
    println("What do you want to do?")
    println("\t1- Add a number to the numbers tree")
    println("\t2- Add a string to the strings tree")
    println("\t3- Delete a number from the numbers tree")
    println("\t4- Delete a string from the strings tree")
    println("\t5- Search a number in the numbers tree")
    println("\t6- Search a string in the strings tree")
    println("\t7- Show inorder list of the numbers tree")
    println("\t8- Show inorder list of the strings tree")
    println("\t9- Pretty print the numbers tree")
    println("\t10- Pretty print the strings tree")
    println("\t11- Clear the numbers tree")
    println("\t12- Clear the strings tree")
    println("\t13- Exit")
end


function controler(option::Int, rbNumbers::RBTree, rbStrings::RBTree)
    if option == 1
        print("Enter the number you wish to insert: ")
        number = parse(Float64, readline(STDIN))
        insert(rbNumbers, number)
        return "Number inserted"

    elseif option == 2
        print("Enter the string you wish to insert: ")
        string = chop(readline(STDIN))
        insert(rbStrings, string)
        return "String  inserted"

    elseif option == 3
        print("Enter the number you wish to delete: ")
        number = parse(Float64, readline(STDIN))
        return delete(rbNumbers, number)

    elseif option == 4
        print("Enter the string you wish to delete: ")
        string = readline(STDIN)
        return delete(rbStrings, string)

    elseif option == 5
        print("Enter the number you wish to find: ")
        number = parse(Float64, readline(STDIN))
        result = search(rbNumbers, number).value
        if result == nothing
            return "Not found"
        else
            return "Found"
        end

    elseif option == 6
        print("Enter the string you wish to find: ")
        string = chop(readline(STDIN))
        result = search(rbStrings, string).value
        if result == nothing
            return string*" not found"
        else
            return "Found "*result
        end

    elseif option == 7
        return inorder(rbNumbers)

    elseif option == 8
        return inorder(rbStrings)

    elseif option == 9
        prettyPrint(rbNumbers)
        return "Tree printed"

    elseif option == 10
        prettyPrint(rbStrings)
        return "Tree printed"

    elseif option == 11
        clear(rbNumbers)
        return "Tree cleared"

    elseif option == 12
        clear(rbStrings)
        return "Tree cleared"
    end
end


function main()
    node1 = Node(nothing)
    node2 = Node(nothing)
    rbNumbersTree = RBTree(node1, node1)
    rbStringsTree = RBTree(node2, node2)
    println("Welcome to the Julia Red-Black Tree Manager")
    while true
        printMenu()
        print("Select an option: ")
        option = parse(Int, readline(STDIN))
        if option < 1 || option > 13
            println("Please, select a valid option :)")
        end
        if option == 13
            println("End")
            break
        else
            println("\n"*string(controler(option, rbNumbersTree, rbStringsTree))*"\n")
        end
    end
end
main()

