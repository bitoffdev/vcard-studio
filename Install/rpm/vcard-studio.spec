Name:           vcard-studio
Version:        1.4.0
Release:        1%{?dist}
Summary:        A contact management application with support for vCard file format.

Group:          Applications/Productivity
License:        Public Domain
URL:            https://app.zdechov.net/vcard-studio
Source0:        vcard-studio.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

#BuildRequires:  lazarus

%description
A contact management application with support for vCard file format (.vcf).
  
Features:
* Add, Edit, Clone or Remove contacts and contact properties
* Shows contact fields in multiple tabs General, Home, Work, Social, Chat and Others
* Contact photo displayed and can be changed (supported image types JPEG, PNG, GIF and BMP) 
* Quick filter by table columns in contacts list
* Combine multiple contact files together
* Split contacts to separate files
* Find any value in contacts
* Find duplicate contacts to merge
* Copy, Cut and Paste contacts from/to clipboard
* Multilingual interface (English, Czech)

%global debug_package %{nil}

%prep
%setup -q -c $(RPM_NAME)-$(RPM_VERSION)


%build
lazbuild --build-mode=Release vCardStudio.lpi

%install
rm -rf $RPM_BUILD_ROOT
install -d -m 755 $RPM_BUILD_ROOT/usr/bin
install -s -m 755 vCardStudio $RPM_BUILD_ROOT/usr/bin
install -d -m 755 $RPM_BUILD_ROOT/usr/share/vCardStudio/Languages
install -m 644 Languages/vCardStudio.pot $RPM_BUILD_ROOT/usr/share/vCardStudio/Languages
install -m 644 Languages/vCardStudio.cs.po $RPM_BUILD_ROOT/usr/share/vCardStudio/Languages
install -m 644 Languages/UFormAbout.pot $RPM_BUILD_ROOT/usr/share/vCardStudio/Languages
install -m 644 Languages/UFormAbout.cs.po $RPM_BUILD_ROOT/usr/share/vCardStudio/Languages
install -d -m 755 $RPM_BUILD_ROOT/usr/share/vCardStudio/Images
install -m 755 Images/Profile.png $RPM_BUILD_ROOT/usr/share/vCardStudio/Images
install -d -m 755 $RPM_BUILD_ROOT/usr/share/applications
install -m 644 Install/deb/vCardStudio.desktop $RPM_BUILD_ROOT/usr/share/applications
install -d -m 755 $RPM_BUILD_ROOT/usr/share/pixmaps
install -m 644 Install/deb/vCardStudio.png $RPM_BUILD_ROOT/usr/share/pixmaps

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/bin/vCardStudio
/usr/share/applications/vCardStudio.desktop
/usr/share/vCardStudio/*
/usr/share/vCardStudio/Images/*
/usr/share/pixmaps/vCardStudio.png

%changelog
