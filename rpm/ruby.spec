%bcond_with X11

Name:           ruby
Version:    1.9.3.551
Release:    mer1
BuildRequires:  bison
BuildRequires:  gdbm-devel
BuildRequires:  libffi-devel
BuildRequires:  libyaml-devel
BuildRequires:  ncurses-devel
BuildRequires:  openssl-devel
BuildRequires:  readline-devel
BuildRequires:  zlib-devel
BuildRequires:  ca-certificates
BuildRequires:  fdupes
%if %{with X11}
BuildRequires:  libX11-devel
%endif
#!BuildIgnore: sb2-tools-qt5-armv6l-dependency-inject
#!BuildIgnore: sb2-tools-qt5-armv7l-dependency-inject
#!BuildIgnore: sb2-tools-qt5-armv7hl-dependency-inject
#!BuildIgnore: sb2-tools-qt5-armv7tnhl-dependency-inject

Provides:       rubygem-rake = 0.9.2.2
Provides:       ruby(abi) = 1.9.3

Url:            http://www.ruby-lang.org/
Source:         %{name}-%{version}.tar.bz2
Source1:        gem_build_cleanup
Source2:        gemrc
Source4:        rubygems.attr
Source5:        rubygemsdeps.sh
Source6:        ruby.macros
Source7:        gem_install_wrapper.sh

Summary:        An Interpreted Object-Oriented Scripting Language
License:        BSD-2-Clause or Ruby
Group:          Development/Languages

%description
Ruby is an interpreted scripting language for quick and easy
object-oriented programming.  It has many features for processing text
files and performing system management tasks (as in Perl).  It is
simple, straight-forward, and extensible.

* Ruby features:

- Simple Syntax

- *Normal* Object-Oriented features (class, method calls, for
   example)

- *Advanced* Object-Oriented features(Mix-in, Singleton-method, for
   example)

- Operator Overloading

- Exception Handling

- Iterators and Closures

- Garbage Collection

- Dynamic Loading of Object Files (on some architectures)

- Highly Portable (works on many UNIX machines; DOS, Windows, Mac,
BeOS, and more)


%package devel
Summary:        Development files to link against Ruby
Group:          Development/Languages
Requires:       %{name} = %{version}
Provides:       rubygems19 = 1.3.7
Provides:       rubygems19_with_buildroot_patch
Provides:       rubygems_with_buildroot_patch

%description devel
Development files to link against Ruby.

%if 0%{?with_docs}
%package doc-ri
Summary:        Ruby Interactive Documentation
Group:          Development/Languages
Requires:       %{name} = %{version}
BuildArch:      noarch

%description doc-ri
This package contains the RI docs for ruby

%package doc-html
Summary:        This package contains the HTML docs for ruby
Group:          Development/Languages
Requires:       %{name} = %{version}
BuildArch:      noarch

%description doc-html
This package contains the HTML docs for ruby
%endif

%package examples
Summary:        Example scripts for ruby
Group:          Development/Languages/Ruby
Requires:       %{name} = %{version}
BuildArch:      noarch

%description examples
Example scripts for ruby

%package test-suite
Requires:       %{name} = %{version}
Summary:        An Interpreted Object-Oriented Scripting Language
Group:          Development/Languages
BuildArch:      noarch

%description test-suite
Ruby is an interpreted scripting language for quick and easy
object-oriented programming.  It has many features for processing text
files and performing system management tasks (as in Perl).  It is
simple, straight-forward, and extensible.

* Ruby features:

- Simple Syntax

- *Normal* Object-Oriented features (class, method calls, for
   example)

- *Advanced* Object-Oriented features(Mix-in, Singleton-method, for
   example)

- Operator Overloading

- Exception Handling

- Iterators and Closures

- Garbage Collection

- Dynamic Loading of Object Files (on some architectures)

- Highly Portable (works on many UNIX machines; DOS, Windows, Mac,
BeOS, and more)

%prep
%setup -q -n %{name}-%{version}/%{name}

%build

cd ../bootstrap
autoconf
%configure \
  --with-mantype=man \
  --enable-shared \
  --disable-rpath

%{__make} miniruby V=0 %{?jobs:-j%jobs}

echo -e "#!/bin/bash\n$PWD/miniruby -I$PWD/lib \"\$@\"" > ../miniruby
%{__chmod} +x ../miniruby

cd ../%{name}
autoconf
%configure \
  --with-mantype=man \
  --enable-shared \
  --with-baseruby=$PWD/../miniruby \
  %if 0%{?with_docs}
  --enable-install-doc \
  %else
  --disable-install-doc \
  %endif
  %if ! %{with X11}
  --without-X11 \
  %endif
  --disable-rpath

%{__make} all V=0 %{?jobs:-j%jobs}

%install
make install DESTDIR=%{buildroot}

%{__install} -D -m 0755 %{S:1} %{buildroot}/%{_bindir}/gem_build_cleanup
%{__install} -D -m 0644 %{S:2} %{buildroot}/etc/gemrc
%{__install} -D -m 0644 %{S:4} %{buildroot}/%{_rpmconfigdir}/fileattrs/rubygems.attr
%{__install} -D -m 0755 %{S:5} %{buildroot}/%{_rpmconfigdir}/rubygemsdeps.sh
%{__install} -D -m 0644 %{S:6} %{buildroot}/etc/rpm/macros.ruby
%{__install} -D -m 0755 %{S:7} %{buildroot}/%{_libdir}/rpm/gem_install_wrapper.sh

%{__chmod} +x %{buildroot}/%{_libdir}/ruby/1.9.1/abbrev.rb
%{__chmod} +x %{buildroot}/%{_libdir}/ruby/1.9.1/set.rb

%if 0%{?with_docs}
%fdupes -s %buildroot/%{_datadir}/ri
%endif

%if 0%{?run_tests}
%check
export LD_LIBRARY_PATH="$PWD"
# we know some tests will fail when they do not find a /usr/bin/ruby
make check V=1 ||:
%endif

%post   -p /sbin/ldconfig
%postun -p /sbin/ldconfig

%files
%defattr(-,root,root,-)
%config %{_sysconfdir}/rpm/macros.ruby
%config %{_sysconfdir}/gemrc
%{_rpmconfigdir}/fileattrs/rubygems.attr
%{_rpmconfigdir}/rubygemsdeps.sh
%{_bindir}/gem_build_cleanup
%{_bindir}/erb
%{_bindir}/gem
%{_bindir}/irb
%{_bindir}/rake
%{_bindir}/rdoc
%{_bindir}/ri
%{_bindir}/ruby
%{_bindir}/testrb
%{_libdir}/libruby.so.1.9*
%{_libdir}/ruby/
%{_libdir}/rpm/gem_install_wrapper.sh
%{_mandir}/man1/ri.1*
%{_mandir}/man1/irb.1*
%{_mandir}/man1/erb.1*
%{_mandir}/man1/rake.1*
%{_mandir}/man1/ruby.1*
%doc ChangeLog  COPYING  COPYING.ja  GPL  KNOWNBUGS.rb  LEGAL  NEWS  README  README.EXT  README.EXT.ja  README.ja  ToDo doc/* sample/

%files devel
%defattr(-,root,root,-)
%{_includedir}/ruby-1.9.1
%{_libdir}/libruby.so
%exclude %{_libdir}/libruby-static.a
%{_libdir}/pkgconfig/ruby-1.9.pc

%if 0%{?with_docs}
%files doc-ri
%defattr(-,root,root,-)
%{_datadir}/ri/
%endif
