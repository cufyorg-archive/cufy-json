# JSON [![](https://jitpack.io/v/cufyorg/json.svg)](https://jitpack.io/#cufyorg/json)
### So advanced JSON formatter, parser and classifier
- Uses readers and writers (buffered)
- Completely inheritable and easy to do (syntax and algorithm)
- Supports comments
- Supports recursion
- Can parse to an existing container (List or Map) and it deep override them
- Can specify the type of the input or output using Clazzes (see [Clazz.java][clazz])

###Dependencies
- Util [(cufyorg:util)](https://github.com/cufyorg/util)
- Base [(cufyorg:base)](https://github.com/cufyorg/base)

---

To parse a json-text:

```java 
Object outputObject = JSNO.parse(inputString);
```

To format an object to a json-text:

```java 
String outputString = JSON.format(inputObject);
```

To use more parsing specifications:

```java 
JSON.global.parse(inputReader, outputObject, inputClazz, outputClazz);
```
Or if you want auto-classify the input:

```java 
JSON.global.cparse(inputReader, outputObject, outputClazz);
```

To use more formatting specifications:

```java 
JSON.global.format(inputObject, outputObject, inputClazz, outputClazz);
```

[clazz]: https://github.com/cufyorg/base/blob/master/src/main/java/cufy/lang/Clazz.java
