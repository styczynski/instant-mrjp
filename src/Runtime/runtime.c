#include <stdio.h>
#include <stdlib.h>
#include <unistdio.h>
#include <unistr.h>

#include "runtime.h"

#define __LATTE_RUNTIME_DEBUG_ENABLED false
#define __LATTE_RUNTIME_DEBUG_PRINT_ADDRESSES false
#define __LATTE_RUNTIME_GC_ENABLED false
#define DEBUG(args...) if(__LATTE_RUNTIME_DEBUG_ENABLED) { fprintf(stderr, "~#LATCINSTR#~ "); fprintf(stderr, args); fprintf(stderr, "\n"); fflush(stderr); }
#define XDEBUG(args...) if(true) { fprintf(stderr, "~#LATCINSTR#~ "); fprintf(stderr, args); fprintf(stderr, "\n"); fflush(stderr); }
#define FORMAT_PTR(PTR) ((__LATTE_RUNTIME_DEBUG_PRINT_ADDRESSES)?(PTR):((void*)(0)))

extern void bzero(void *s, size_t n);
extern void *memcpy(void *dest, const void *src, size_t n);

extern struct Type _class_Array;
extern struct Type _class_Object;
extern struct Type _class_String;

typedef obj (*toStringPtr)(obj);

char *errMsg;

uint8_t emptyString[] = "";

obj __new(struct Type *t) {
    DEBUG("Calling __new v3");
    DEBUG("Perform reference malloc type=%p", FORMAT_PTR(t));
    DEBUG("__new examine type: <type parent=%p> [%d]", FORMAT_PTR(t->parent), t->dataSize);
    obj r = malloc(sizeof(struct Reference)+t->dataSize);
    DEBUG("Set __new type/counter");
    r->type = t;
    r->counter = 0;
    r->data = 0;
    DEBUG("Do init for non array non string?");
    if (t->dataSize > 0) {
        bzero((r+36), t->dataSize);
    }
    // if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
    //     DEBUG("Perform init");
    //     r->data = malloc(t->dataSize);
    //     bzero(r->data, t->dataSize);
    // } else {
    //     DEBUG("Just set data to NULL");
    //     r->data = NULL;
    // }
    r->methods = r->type->methods;
    DEBUG("Completed __new %p <type %p, par %p> inner data=%p size=%d", FORMAT_PTR(r), FORMAT_PTR(r->type), FORMAT_PTR(r->type->parent), FORMAT_PTR(r->data), t->dataSize);
    return r;
}

void __free(obj r) {
    DEBUG("__free %p", FORMAT_PTR(r));
    if (r->type == &_class_Array) {
        //struct Array *arr = r->data;
        void **els = r->data;
        if (r->elementSize == sizeof(void *)) {
            for (int i = 0; i < r->length; i++)
                __decRef(els[i]);
        }
        if (els != NULL)
            free(els);
    } else if (r->type == &_class_String) {
        void *els = (r->data);
        if (els != NULL && els != emptyString)
            free(els);
    }
    if (r->data != NULL)
        free(r->data);
    free(r);
}

void __incRef(obj r) {
    if (!__LATTE_RUNTIME_GC_ENABLED) return;
    DEBUG("__incRef %p", FORMAT_PTR(r));
    if (r != NULL) {
        r->counter++;
    }
    DEBUG("__incRef end %p", FORMAT_PTR(r));
}
void __decRef(obj r) {
    DEBUG("__decRef %p", FORMAT_PTR(r));
    if (!__LATTE_RUNTIME_GC_ENABLED) return;
    if (r != NULL) {
        r->counter--;
        if (r->counter <= 0) {
            if (r->type != &_class_Array) {
                for (int i = 0; i < r->type->referenceOffsetsSize; i++)
                    __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
            }
            __free(r);
        }
    }
    DEBUG("__decRef end %p", FORMAT_PTR(r));
}

obj __newRefArray(int32_t length) { return __newArray(sizeof(obj), length); }
obj __newIntArray(int32_t length) {
    return __newArray(sizeof(int32_t), length);
}
obj __newByteArray(int32_t length) {
    return __newArray(sizeof(int8_t), length);
}
obj __newArray(int32_t size, int32_t length) {
    obj r = __new(&_class_Array);
    void* arr = malloc(size * length);
    r->data = arr;
    r->elementSize = size;
    r->length = length;
    if (length > 0) {
        //arr->elements = malloc(size * length);
        bzero(arr, size * length);
    }
    return r;
}

void *__getelementptr(obj array, int32_t index) {
    // if (array == NULL) {
    //     errMsg = "ERROR: Array is null.";
    //     error();
    // }
    // struct Array *arr = ((struct Array *)array->data);
    // if (index >= arr->length || index < 0) {
    //     errMsg = "ERROR: Array index out of range.";
    //     fprintf(stderr, "%d, %d\n", index, arr->length);
    //     error();
    // }
    // return arr->elements + index * arr->elementSize;
    return NULL;
}

obj __cast(obj o, struct Type *t) {
    DEBUG("__cast");
    DEBUG("__cast %p %p [%d]", FORMAT_PTR(o), FORMAT_PTR(t), t->dataSize);
    if (o == NULL) {
        DEBUG("__cast object is null");
        return NULL;
    }
    DEBUG("__cast get underlying type");
    struct Type *to = o->type;
    while (to != NULL) {
        DEBUG("__cast iterate parent upward %p [%d]", FORMAT_PTR(to), to->dataSize);
        if (t == to) {
            DEBUG("__cast found correct parent: %p", FORMAT_PTR(to));
            return o;
        }
        struct Type *prev = to;
        to = to->parent;
        if (prev == to) {
            DEBUG("__cast loop, break %p", FORMAT_PTR(to));
            break;
        }
    }
    DEBUG("finished the cast (FAILED)");
    return NULL;
}

void __errorNull() {
    errMsg = "ERROR: Null pointer reference.";
    error();
}

obj __createString(char *c) {
    DEBUG("Call __createString() v2");
    //DEBUG("Try to create string from %p", FORMAT_PTR(c));
    if (c == NULL) {
        DEBUG("C is NULL exit");
        return __createString(emptyString);
    }
    DEBUG("Perform new on _class_String");
    obj r = __new(&_class_String);
    DEBUG("String allocated");
    DEBUG("Measure strlen");
    r->length = u8_strlen(c);
    DEBUG("Check unicode, len=%d", r->length);
    uint8_t* invalid_unit = u8_check(c, r->length);
    if (u8_check(c, r->length) != NULL) {
        DEBUG("Non-unicode string encoding at byte '%s' #%d", c, (int)(((uint64_t)invalid_unit)-((uint64_t)c)));
        errMsg = "ERROR: Non-unicode string encoding.";
        error();
    }
    if (r->length > 0) {
        int len = r->length;
        uint8_t *str = malloc(len + 1);
        r->data = str;
        memcpy(str, c, len);
        str[len] = 0;
    } else {
        r->data = emptyString;
    }
    DEBUG("Str init completed %p (with data=%p, size=%d, str='%s')", FORMAT_PTR(r), FORMAT_PTR(r->data), r->length, r->data);
    return r;
}

// BuiltIn classes' methods
obj _Object_toString(obj o) {
    obj ret = __createString("Object");
    __incRef(ret);
    return ret;
}
int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }

obj _Array_toString(obj arr) {
    char start[] = "[";
    char delim[] = ", ";
    char end[] = "]";

    obj *strings = malloc(sizeof(obj) * arr->length);
    int32_t *lenghts = malloc(sizeof(int32_t) * arr->length);
    int32_t totalLenght = 0;

    for (int i = 0; i < arr->length; i++) {
        if (arr->elementSize == sizeof(int32_t)) {
            int32_t *elements = arr->data;
            strings[i] = intToString(elements[i]);
        } else if (arr->elementSize == sizeof(int8_t)) {
            int8_t *elements = arr->data;
            strings[i] = byteToString(elements[i]);
        } else {
            obj *elements = arr->data;
            obj element = elements[i];
            if (element == NULL) {
                strings[i] = __createString("null");
                __incRef(strings[i]);
            } else {
                obj (*toString)(obj) = ((void **)element->type->methods)[0];
                strings[i] = toString(element);
            }
        }
        lenghts[i] = u8_strlen((strings[i]->data));
        totalLenght += lenghts[i];
    }

    int32_t bufferSize = u8_strlen(start) + totalLenght +
                         (arr->length - 1) * u8_strlen(delim) +
                         u8_strlen(end) + 1;
    uint8_t *buffer = malloc(bufferSize);
    int32_t index = 0;
    u8_strcpy(buffer + index, start);
    index++;
    for (int i = 0; i < arr->length; i++) {
        u8_strcpy(buffer + index, (strings[i]->data));
        index += lenghts[i];
        if (i != arr->length - 1) {
            u8_strcpy(buffer + index, delim);
            index += 2;
        }
        __decRef(strings[i]);
    }
    u8_strcpy(buffer + index, end);
    buffer[bufferSize - 1] = 0;
    obj ret = __createString(buffer);
    __incRef(ret);
    free(lenghts);
    free(strings);
    free(buffer);
    return ret;
}

obj _String_toString(obj str) {
    __incRef(str);
    return str;
}
int32_t _String_getHashCode(obj str) {
    int32_t hash = 0x811c9dc5;
    uint8_t *rawstring = str->data;
    int32_t strlen = u8_strlen(rawstring);
    for (int i = 0; i < strlen; i++) {
        hash ^= rawstring[i];
        hash *= 0x01000193;
    }
    return hash;
}
int8_t _String_equals(obj o1, obj o2) {
    if (o2 == NULL)
        return false;
    if (o2->type != &_class_String)
        return false;
    if (_String_length(o1) != _String_length(o2))
        return false;
    uint8_t *rs1 = (o1->data);
    uint8_t *rs2 = (o2->data);
    return u8_strcmp(rs1, rs2) == 0;
}
obj _String_substring(obj str, int32_t startIndex, int32_t length) {
    if (length < 0) {
        errMsg = "ERROR: Substring with negative length.";
        error();
    }
    if (length == 0)
        return __createString("");
    if (startIndex >= _String_length(str)) {
        errMsg = "ERROR: Substring starting index is too big.";
        error();
    }
    uint8_t *rs = (str->data);
    uint8_t *offset_str = rs;
    ucs4_t character;
    while (startIndex-- > 0)
        offset_str += u8_next(&character, offset_str) - offset_str;
    uint8_t *end = offset_str;
    int32_t counter = 0;
    while (counter < length) {
        if (u8_next(&character, end) == NULL) {
            errMsg = "ERROR: Substring reached end of string.";
            error();
        }
        end += u8_next(&character, end) - end;
        counter++;
    }
    int32_t bufferSize = end - offset_str + 1;
    uint8_t *buffer = malloc(bufferSize);
    u8_strncpy(buffer, offset_str, bufferSize);
    buffer[bufferSize - 1] = 0;
    obj ret = __createString(buffer);
    __incRef(ret);
    free(buffer);
    return ret;
}
int32_t _String_length(obj str) {
    if (str->length < 0) {
        str->length = u8_mbsnlen(str->data, u8_strlen(str->data));
    }
    return str->length;
}
int32_t _String_indexOf(obj str, obj substr, int32_t startFrom) {
    if (substr == NULL) {
        errMsg = "ERROR: IndexOf null substring argument.";
        error();
    }
    if (startFrom >= _String_length(str)) {
        errMsg = "ERROR: IndexOf starting index is too big.";
        error();
    }
    if (_String_length(str) < _String_length(substr))
        return -1;
    uint8_t *rs = (str->data);
    uint8_t *rsub = (substr->data);
    uint8_t *start = rs;
    ucs4_t c;
    while (startFrom-- > 0) {
        if (u8_next(&c, start) == NULL)
            return -1;
        start += u8_next(&c, start) - start;
    }
    uint8_t *index = u8_strstr(start, rsub);
    uint32_t counter = 0;
    while ((rs += u8_next(&c, rs) - rs) != index)
        counter++;
    return counter;
}
obj _String_getBytes(obj str) {
    uint8_t *rs = (str->data);
    int32_t len = rs == NULL ? 0 : u8_strlen(rs);
    obj arr = __newByteArray(len + 1);
    memcpy((arr->data), rs, len);
    __incRef(arr);
    return arr;
}
int8_t _String_endsWith(obj str, obj substr) {
    if (substr == NULL) {
        errMsg = "ERROR: EndsWith null substring argument.";
        error();
    }
    uint8_t *rs = (str->data);
    uint8_t *rsub = (substr->data);
    return u8_endswith(rs, rsub);
}
int8_t _String_startsWith(obj str, obj substr) {
    if (substr == NULL) {
        errMsg = "ERROR: StartsWith null substring argument.";
        error();
    }
    uint8_t *rs = (str->data);
    uint8_t *rsub = (substr->data);
    return u8_startswith(rs, rsub);
}
obj _String_concat(obj str, obj secondstr) {
    DEBUG("String concat on %p and %p", str, secondstr);
    if (secondstr == NULL) {
        __incRef(str);
        return str;
    }
    uint8_t *rs1 = (str->data);
    uint8_t *rs2 = (secondstr->data);
    DEBUG("Take strlen");
    int32_t len1 = u8_strlen(rs1);
    int32_t len2 = u8_strlen(rs2);
    uint8_t *buffer = malloc(len1 + len2 + 1);
    DEBUG("perform strcpy");
    u8_strcpy(buffer, rs1);
    u8_strcpy(buffer + len1, rs2);
    buffer[len1 + len2] = 0;
    DEBUG("Create final string result='%s' (len1=%d, len2=%d, str1='%s', str2='%s')", buffer, len1, len2, rs1, rs2);
    obj ret = __createString(buffer);
    __incRef(ret);
    free(buffer);
    DEBUG("String concat completed");
    return ret;
}

char charAtErr[] = "ERROR: String too short.";
int32_t _String_charAt(obj str, int32_t index) {
    uint8_t *rs = (str->data);
    ucs4_t c;
    while (index-- > 0) {
        if (u8_next(&c, rs) == NULL) {
            errMsg = charAtErr;
            error();
        }
        rs += u8_next(&c, rs) - rs;
    }
    if (u8_strmbtouc(&c, rs) <= 0) {
        errMsg = charAtErr;
        error();
    }
    return c;
}

void ddd(obj str) {
    DEBUG("%p", str);
}

// functions
int8_t printString(obj str) {
    DEBUG("Calling printString(%p)", FORMAT_PTR(str));
    DEBUG("Str data is %p", FORMAT_PTR(str->data));
    if (str == NULL)
        str = __createString("null");
    __incRef(str);
    uint8_t *rs = (str->data);
    DEBUG("Str inner data %p", FORMAT_PTR(rs));
    printf("%s\n", rs);
    __decRef(str);
    DEBUG("printString(%p) completed", FORMAT_PTR(str))
    return 0;
}
int8_t printInt(int32_t i) {
    DEBUG("Calling printInt()")
    printf("%d\n", i);
    DEBUG("printInt() completed")
    return 0;
}
int8_t printBoolean(int8_t b) {
    DEBUG("Calling printBoolean()")
    if (b)
        printf("true\n");
    else
        printf("false\n");
    DEBUG("printBoolean() completed")
    return 0;
}
obj intToString(int32_t i) {
    DEBUG("Calling intToString() conversion")
    char buffer[11];
    sprintf(buffer, "%d", i);
    obj ret = __createString(buffer);
    __incRef(ret);
    DEBUG("intToString() conversion completed")
    return ret;
}

obj byteToString(uint8_t i) {
    DEBUG("Calling byteToString() conversion")
    char buffer[11];
    sprintf(buffer, "%u", i);
    obj ret = __createString(buffer);
    __incRef(ret);
    DEBUG("byteToString() conversion completed")
    return ret;
}

obj boolToString(int8_t b) {
    DEBUG("boolToString() conversion completed")
    obj ret;
    if (b)
        ret = __createString("true");
    else
        ret = __createString("false");
    __incRef(ret);
    DEBUG("boolToString() conversion completed")
    return ret;
}

int8_t print(obj o) {
    DEBUG("Calling generic print(obj)")
    if (o == NULL)
        o = __createString("null");
    __incRef(o);
    obj (*toStr)(obj) = ((void **)o->type->methods)[0];
    obj str = toStr(o);
    DEBUG("Subcall to internal printString() method")
    printString(str);
    __decRef(str);
    __decRef(o);
    DEBUG("Generic print(obj) completed")
    return 0;
}

int8_t printBinArray(obj arr) {
    DEBUG("Calling printBinArray(arr)")
    if (arr == NULL){
        print(arr);
        return 0;
    }
    __incRef(arr);
    fwrite(arr->data, sizeof(int8_t), arr->length, stdout);
    __decRef(arr);
    DEBUG("printBinArray(arr) call completed")
    return 0;
}

int8_t error() {
    DEBUG("Calling error()")
    if (errMsg != NULL)
        fprintf(stderr, "%s\n", errMsg);
    else
        fprintf(stderr, "%s\n", "ERROR: User error.");
    DEBUG("Exiting via error() (exit=1)")
    exit(1);
    return 1;
}

int32_t readInt() {
    int32_t i;
    char *line = NULL;
    size_t size = 0;
    ssize_t unused = getline(&line, &size, stdin);
    int unused2 = sscanf(line, "%d ", &i);
    free(line);
    return i;
}
obj readString() {
    char *line = NULL;
    size_t size = 0;
    ssize_t unused = getline(&line, &size, stdin);
    size = u8_strlen(line);
    line[size - 1] = 0; // remove newline
    obj l = __createString(line);
    __incRef(l);
    free(line);
    return l;
}
