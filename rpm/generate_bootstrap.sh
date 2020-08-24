#!/bin/sh -e

die()
{
    echo "$@" >&2
    echo 'exit'  >&2
    exit 1
}

isolate_git_submodule()
# usage: isolate_git_submodule <module>
# description: Isolate git submodule workaround bugs in rubys make_snapshot
# RUBY#17134
# https://bugs.ruby-lang.org/issues/17132
{
    cp --archive "$1" "$1.git"
    rm "$1.git/.git"
    mkdir "$1.git/.git"
    cp --archive .git/modules/$1/* $1.git/.git/.
    sed -e '/worktree/d' -i $1.git/.git/config
}

if [ ! -e ./rpm ] ; then
    die "This script needs to be executed inside the package-root"
fi

rm -rf bootstrap

version="$(grep --line-buffered ^Version rpm/ruby.spec|head -n1| sed "s/Version: //")"
(
    cd ruby || exit 1
    if ! git diff-index --quiet HEAD -- ; then
        die 'local changes found, please commit first'
    fi
)
shortref=$(cd ruby; git rev-parse --short HEAD)

isolate_git_submodule ruby
# Generate a snapshot of ruby to generate the bootstrap code that we need to
# build ruby from scratch.
./ruby/tool/make-snapshot -packages=tar -srcdir="$PWD/ruby.git" "$PWD/snapshot"
tar -xf snapshot/ruby-$version-$shortref.tar
rm -rf snapshot

(
    cd ruby
    git reset --hard HEAD      # Restore tracked files
    git clean -d -x -f         # Delete untracked files
)

mkdir -p bootstrap

(
    cd ruby-$version-$shortref
    find . -name \*\.gem -delete
    
    # Look for files that only exist inside the bootstrapped snapshot 
    LANG=C diff -burq ../ruby $PWD| grep ruby-$version-$shortref  | cut -f3,4 -d ' '| sed -e 's/:\ /\//' -e "s|$PWD/||" | while
        read -r file ; do
        cp --recursive --parents "$file" ../bootstrap/.
    done   
)

rm -rf ruby-$version-$shortref
rm -rf ruby.git
