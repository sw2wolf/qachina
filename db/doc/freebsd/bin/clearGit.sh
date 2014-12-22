commit=$(git log --since="1 week ago" --reverse --pretty=%h | head -n1)
if [ -n "$commit" ]; then
   git checkout --orphan temp_remove_old_history "$commit"
   git commit -m "Truncated history" --allow-empty
   git rebase --onto temp_remove_old_history "$commit" master
   git branch -D temp_remove_old_history
   git prune --progress
   git gc --aggressive
fi
