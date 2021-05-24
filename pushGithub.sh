git remote rm origin
git remote add origin 'git@github.com:hongyehuicheng/lilishop.git'
git pull remote master
git push origin master --force
if [ "$?" = "0" ]
then
echo -e "\033[42;34m push to github success! \033[0m"
else
echo -e "\033[41;30m push to github fail! \033[0m"
exit 1
fi

git remote rm origin

git remote add origin 'git@gitee.com:beijing_hongye_huicheng/lilishop.git'

git pull remote master

git branch --set-upstream-to=origin/master master