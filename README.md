# json
### So advanced JSON formatter, parser and classifyer
- Uses readers and writers (buffered)
- Completly inhiretable and easy to do (syntax and algorethem)
- Supports comments
- Supports recursion
- Can parse to an existing container (List or Map) and it deep override them
- Can specifiy the type of the input or output using Clazzes (see [Clazz.java][clazz])

To implement this repository using `jitpack`. Write the following on your `build.gradle`:

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
    implementation 'com.github.cufyorg:json:0.1.0'
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

[clazz]: https://github.com/cufyorg/base/blob/master/src/main/java/cufy/lang/Clazz.java
