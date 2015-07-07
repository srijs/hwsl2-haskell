CFLAGS=-Wall -mavx2 -msse2 -msse4.1 -mpclmul -O3

build:
	clang $(CFLAGS) HWSL2.c "-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers" -dynamiclib -o libhwsl2.dylib
