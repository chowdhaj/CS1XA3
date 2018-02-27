echo ""

for (( i = 1 ; i <= 89 ; i++ )); do
    sleep 0.03
    echo -n "-"
done

echo ""

foo="NOTE: Before Proceeding, Please Maximize The Terminal Window. This Script Uses ASCII Art."
for (( i=0; i<${#foo}; i++ )); do
    sleep 0.07
    echo -n "${foo:$i:1}"
done

echo ""

foo="                                 Press Enter To Continue"
for (( i=0; i<${#foo}; i++ )); do
    if [ $i -lt 33 ]
    then
        sleep 0.0001
    else
        sleep 0.07
    fi
    echo -n "${foo:$i:1}"

done

echo ""

for (( i = 1 ; i <= 89 ; i++ )); do
  sleep 0.03
  echo -n "-"
done

read input

echo "
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                       #
#   P P P P P   R R R R R   O O O O O   J J J J J   E E E E E   C C C C C   T T T T T   #
#   P       P   R       R   O       O       J       E           C               T       #
#   P       P   R       R   O       O       J       E           C               T       #
#   P       P   R       R   O       O       J       E           C               T       #
#   P P P P P   R R R R R   O       O       J       E E E E     C               T       #
#   P           R R         O       O   J   J       E           C               T       #
#   P           R   R       O       O   J   J       E           C               T       #
#   P           R     R     O       O   J   J       E           C               T       #
#   P           R       R   O O O O O   J J J       E E E E E   C C C C C       T       #
#                                                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                       #
#       A       N       N       A       L           Y       Y   Z Z Z Z Z   E E E E E   #
#     A   A     N N     N     A   A     L            Y     Y           Z    E           #
#    A     A    N  N    N    A     A    L             Y   Y           Z     E           #
#   A       A   N  N    N   A       A   L              Y Y           Z      E           #
#   A A A A A   N   N   N   A A A A A   L               Y           Z       E E E E     #
#   A       A   N    N  N   A       A   L               Y          Z        E           #
#   A       A   N    N  N   A       A   L               Y         Z         E           #
#   A       A   N     N N   A       A   L               Y        Z          E           #
#   A       A   N       N   A       A   L L L L L       Y       Z Z Z Z Z   E E E E E   #
#                                                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                       #
#   [1] Compare Local Repo With Remote Repo   [5] Create A TimeStamp For Submission     #
#   [2] Put All Uncommited Changes In A File  [6] Where Am I? Who Am I? Who Are You?    #
#   [3] Extract #TODO Text From Projects      [7] Turn Off The Laptop Shutdown/Restart  #
#   [4] Check Haskell Files For Syntax Errors [8] Exit This Script [TRY ME!!!]          #
#                                                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
"
#sleep 5s

echo -n "Enter A Number From 1 - 8: "
read selection

echo ""

if [ $selection == 1 ]
then
    echo "Comparing Local Repo With Remote Repo"
    echo 2s
    
    git fetch
    git status
    
    echo "Do you wanna perform a Git Pull? (y/n)"
    read gitPull

    if [ "$gitPull" == "y" ]
    then
        git pull
    else
        echo "Okay, I won't pull anything"
    fi

elif [ $selection == 2 ]
then
    echo "Putting All Uncommitted Changes In A File"
    sleep 2s

    git diff > changes.log
    
    echo "Do you wanna see the changes you've made? (y/n)"
    read gitDiff

    if [ "$gitDiff" == "y"  ]
    then
        echo ""
        cat changes.log
    else
        echo "Alright, I won't open it"
    fi

elif [ $selection == 3 ]
then
    echo "Extracting #TODO Text From All Projects"
    sleep 2s

    grep -r --exclude=README.md --exclude=ProjectAnalyze.sh --exclude=todo.log "#TODO" . > "todo.log"

    echo "Do you want to see all the stuff you need To Do? (y/n)"
    read grepTodo

    if [ "$grepTodo" == "y" ]
    then
        echo ""
        cat todo.log
    else
        echo "No worries, I won't open it"
    fi

elif [ $selection == 4 ]
then
    echo "Checking Haskell Files For Syntax Errors"
    sleep 2s

    find . -name "*.hs" -exec ghc -fno-code {} \; &> error.log

    echo "Do you wanna see all the mistakes you may or may not have made? (y/n)"
    read mistakes

    if [ "$mistakes" == "y" ]
    then
        echo ""
        cat error.log
    else
        echo "Cool, you can view it later"
    fi

elif [ $selection == 5 ]
then
    echo "Creating A TimeStamp For Submission Purposes"
    sleep 2s

    date > timestamp.txt

    echo "Timestamp successfully created, would you like to open it? (y/n)?"
    read tim

    if [ "$tim" == "y" ]
    then
        echo "Here You Go: "
        echo ""
        cat timestamp.txt
    else
        echo ""
        echo "Don't forget to submit it with your assignment!"
    fi

elif [ $selection == 6 ] 
then
    echo "Answering Deep Philosophical Questions About Life"
    sleep 2s

echo "

|￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣
|  NAME  : $(whoami)                    
|  DATE  : $(date)  
|  PLACE : $(pwd)            
|＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿
(\__/) ||
(o  o) ||
/    |=//
(____)
"

elif [ $selection == 7 ]
then
    echo "What would you like to do?
[  1 = Shutdown  ]
[  2 = Restart   ]"
    read option
    
    if [ $option == 1  ]
    then
        sudo shutdown
    elif [ $option == 2 ]
    then
        sudo restart
    else
        echo "Your machine will NOT shutdown/restart"
    fi 

elif [ $selection == 8 ]
then
    clear
    echo "
            H I   C U R T I S

I F   Y O U   D O N ' T   G I V E   M E   A N  
A+   O N   T H I S   A S S I G N M E N T...

T H I S   B U N N Y   W I L L   D I E ! ! !

T H I N K   O F   T H E   B U N N Y   :(

                          .^.
                         /  |
                        /  /
                       / ,/
          <.-------.--- /
          <._ __.-/ o. o\  
                 (    Y  )
                 )     /
                /     (
               /       \
            /.-\/       \
          /  _  \        \ 
         /     . \ .)  /  )
        /       )( /  /(, /
       ,|      /     )
      ( |     /     /
          \__ (__   (__     
             \�._,)._,)

"
    sleep 6s
    echo "Before you answer this question, think of the bunny"
    echo "P.S. The Bunny says, \"Please save me, Curtis\" "
    echo "So, are you gonna gimme an A? (y/n)"
    read curtis

    if [ "$curtis" == "y" ]
    then
        echo "Good Answer! You Saved The Bunny!"
    else
        echo "Tsk. Tsk. WWF Is Disappointed"
        sudo shutdown -r now
    fi

else
    echo "-------------------------"
    echo "I N V A L I D   E N T R Y"
    echo "-------------------------"

fi

echo ""
echo "Thank You For Using This Script. Good Bye :)"
sleep 3s
clear
