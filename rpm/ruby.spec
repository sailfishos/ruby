Name:           ruby
Version:        @VERSION@
Release:        0
BuildRequires:  bison
BuildRequires:  gdbm-devel
BuildRequires:  libffi-devel
BuildRequires:  libyaml-devel
BuildRequires:  ncurses-devel
BuildRequires:  openssl-devel
BuildRequires:  readline-devel
BuildRequires:  zlib-devel
BuildRequires:  libX11-devel
BuildRequires:  ca-certificates

Provides:       rubygem-rake = 0.9.2.2
Provides:       ruby(abi) = 1.9.3

Url:            http://www.ruby-lang.org/
Source:         %{name}-%{version}.tar.bz2
Source6:        ruby.macros
Source7:        gem_install_wrapper.sh

Summary:        An Interpreted Object-Oriented Scripting Language
License:        BSD-2-Clause or Ruby
Group:          Development/Languages/Ruby

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
Group:          Development/Languages/Ruby
Requires:       %{name} = %{version}
Provides:       rubygems19 = 1.3.7
Provides:       rubygems19_with_buildroot_patch
Requires:       ruby-common

%description devel
Development files to link against Ruby.

%package doc-ri
Summary:        Ruby Interactive Documentation
Group:          Development/Languages/Ruby
Requires:       %{name} = %{version}
BuildArch:      noarch

%description doc-ri
This package contains the RI docs for ruby

%package doc-html
Summary:        This package contains the HTML docs for ruby
Group:          Development/Languages/Ruby
Requires:       %{name} = %{version}
BuildArch:      noarch

%description doc-html
This package contains the HTML docs for ruby

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
Group:          Development/Languages/Ruby
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
autoconf
%configure \
  --with-mantype=man \
  --enable-shared \
  --disable-rpath

%{__make} -k miniruby V=1 || true
%{__cp} miniruby $PWD/../miniruby

%configure \
  --with-mantype=man \
  --enable-shared \
  --with-baseruby=$PWD/../miniruby \
  --disable-rpath

%{__make} -k miniruby V=1

%install
make install DESTDIR=%{buildroot}
%{__install} -D -m 0644 %{S:6} %{buildroot}/etc/rpm/macros.ruby
%{__install} -D -m 0755 %{S:7} %{buildroot}/usr/lib/rpm/gem_install_wrapper.sh

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
%config(noreplace) /etc/rpm/macros.ruby19
%{_bindir}/erb%{rb_binary_suffix}
%{_bindir}/gem%{rb_binary_suffix}
%{_bindir}/irb%{rb_binary_suffix}
%{_bindir}/rake%{rb_binary_suffix}
%{_bindir}/rdoc%{rb_binary_suffix}
%{_bindir}/ri%{rb_binary_suffix}
%{_bindir}/ruby%{rb_binary_suffix}
%{_bindir}/testrb%{rb_binary_suffix}
%{_libdir}/libruby%{rb_binary_suffix}.so.1.9*
%{_libdir}/ruby/
/usr/lib/rpm/gem_install_wrapper.sh
%{_mandir}/man1/ri%{rb_binary_suffix}.1*
%{_mandir}/man1/irb%{rb_binary_suffix}.1*
%{_mandir}/man1/erb%{rb_binary_suffix}.1*
%{_mandir}/man1/rake%{rb_binary_suffix}.1*
%{_mandir}/man1/ruby%{rb_binary_suffix}.1*
%doc ChangeLog  COPYING  COPYING.ja  GPL  KNOWNBUGS.rb  LEGAL  NEWS  README  README.EXT  README.EXT.ja  README.ja  ToDo doc/* sample/

%files devel
%defattr(-,root,root,-)
%{_includedir}/ruby-%{rb_ver}
%{_libdir}/libruby%{rb_binary_suffix}.so
%exclude %{_libdir}/libruby%{rb_binary_suffix}-static.a
%{_libdir}/pkgconfig/ruby-1.9.pc

%files doc-ri
%defattr(-,root,root,-)
%dir %{_datadir}/ri/
%{_datadir}/ri/%{rb_ver}/

