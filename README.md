# json
JSON parser and formatter

To implement this repository using `jitpack`. write on your `build.gradle`:

```gradle
repositories {
    //...
    maven {
        url "https://jitpack.io"
    }
}

dependencies {
    //...
    implementation 'com.github.cufyorg:util:0.1.0'
    implementation 'com.github.cufyorg:base:0.1.0'
}
```

-----

To parse a json-text:

```java
outputObject = JSNO.parse(inputString);
```

To format an object to a json-text:

```java
outputString = JSON.format(inputObject);
```

To use more parsing specifications:

```java
JSON.global.parse(inputReader, outputObject, inputClazz, outputClazz);
```
or if you want to classify the input:
```java
JSON.global.cparse(inputReader, outputObject, outputClazz);
```

To use more formatting specifications:

```java
JSON.global.format(inputObject, outputObject, inputClazz, outputClazz);
```
