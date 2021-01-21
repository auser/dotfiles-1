export INTEL_HAXM_HOME=/usr/local/Caskroom/intel-haxm
export JAVA_HOME=$(/usr/libexec/java_home -v 1.8)

export ANDROID_HOME=~/Library/Android/sdk
export ANDROID_SDK_ROOT=~/Library/Android/sdk
export ANDROID_AVD_HOME=~/.android/avd

export ANT_HOME=/usr/local/opt/ant
export MAVEN_HOME=/usr/local/opt/maven
export GRADLE_HOME=/usr/local/opt/gradle
export ANDROID_NDK_HOME=$ANDROID_SDK_ROOT/ndk-bundle

export PATH=$ANT_HOME/bin:$PATH
export PATH=$MAVEN_HOME/bin:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH=$ANDROID_HOME/tools:$PATH
export PATH=$ANDROID_HOME/platform-tools:$PATH
export PATH=$ANDROID_HOME/build-tools/19.1.0:$PATH
export PATH=$ANDROID_HOME/emulator:$PATH
