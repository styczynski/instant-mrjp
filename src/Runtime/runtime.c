#include <stdio.h>
#include <stdlib.h>
#include <unistdio.h>
#include <unistr.h>

#include "runtime.h"

#define __LATTE_RUNTIME_DEBUG_ENABLED false
#define __LATTE_RUNTIME_DEBUG_PRINT_ADDRESSES true
#define __LATTE_RUNTIME_GC_ENABLED true
#define DEBUG(args...) if(__LATTE_RUNTIME_DEBUG_ENABLED) { fprintf(stderr, "~#LATCINSTR#~ "); fprintf(stderr, args); fprintf(stderr, "\n"); fflush(stderr); }
#define XDEBUG(args...) if(true) { fprintf(stderr, "~#LATCINSTR#~ "); fprintf(stderr, args); fprintf(stderr, "\n"); fflush(stderr); }
#define FORMAT_PTR(PTR) ((__LATTE_RUNTIME_DEBUG_PRINT_ADDRESSES)?(PTR):((void*)(0)))
#define IS_NULL(PTR) (((void*)(PTR)) == ((void*)(&_LAT_NULL)))
#define VAL_NULL ((obj)(&_LAT_NULL))

extern void bzero(void *s, size_t n);
extern void *memcpy(void *dest, const void *src, size_t n);

extern struct Type _class_Array;
extern struct Type _class_Object;
extern struct Type _class_String;

typedef obj (*toStringPtr)(obj);

char *errMsg;

uint8_t emptyString[] = "";


void __incRef(obj r) {
    if (!__LATTE_RUNTIME_GC_ENABLED) return;
    DEBUG("__incRef %p", FORMAT_PTR(r));
    if (!IS_NULL(r)) {
        r->counter++;
    }
    DEBUG("__incRef end %p", FORMAT_PTR(r));
}

static uint32_t gc_level = 0;
static uint64_t gc_visited[1000000];
static uint32_t gc_visited_length = 0;

obj __newRefArray(int32_t length) {
    return __newArray(sizeof(obj), length, true);
}
obj __newIntArray(int32_t length) {
    return __newArray(sizeof(int32_t), length, false);
}
obj __newByteArray(int32_t length) {
    return __newArray(sizeof(int8_t), length, false);
}
obj __newArray(int32_t size, int32_t length, bool use_null_initializer) {
    obj r = __new(&_class_Array);
    void* arr = malloc(size * length);
    r->data = arr;
    r->elementSize = size;
    r->length = length;
    if (length > 0) {
        if (use_null_initializer) {
            obj* arr_obj = (obj*) arr;
            for (int i=0; i<length; ++i) {
                arr_obj[i] = VAL_NULL;
            }
        } else {
            bzero(arr, size * length);
        }
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
    if (IS_NULL(o)) {
        DEBUG("__cast object is null");
        return VAL_NULL;
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
    return VAL_NULL;
}

void __errorNull() {
    errMsg = "ERROR: Null pointer reference.";
    error();
}

obj __createString(char *c) {
    DEBUG("Call __createString() v2");
    //DEBUG("Try to create string from %p", FORMAT_PTR(c));
    if (IS_NULL(c)) {
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

static uint32_t type_info_string_repr_level = 0;
static uint64_t type_info_string_repr_visited[1000000];
static uint32_t type_info_string_repr_visited_length = 0;

obj _typeInfoStringRepr(obj o) {
    if (IS_NULL(o)) {
        return __createString("null");
    }
    if (o->type == &_class_Array) {
        return _Array_toString(o);
    }
    if (o->type == &_class_String) {
        return _String_concat(__createString("\""), _String_concat(o, __createString("\"")));
    }
    for (int i=0;i<type_info_string_repr_visited_length;++i) {
        if (((uint64_t)o) == type_info_string_repr_visited[i]) {
            // Recursive call
            return __createString("<recursive>");
        }
    }
    type_info_string_repr_visited[type_info_string_repr_visited_length++] = (uint64_t)o;

    uint8_t *name_buffer = malloc(200);
    name_buffer[0] = '\0';
    u8_strcat(name_buffer, o->type->typeName);
    u8_strcat(name_buffer, "{");
    obj result = __createString(name_buffer);
    free(name_buffer);

    const int fieldsInfoLength = o->type->fieldsInfoLength;
    for(int i=0;i<fieldsInfoLength;++i) {
        char* fieldInfo = (char*) (((uint64_t)o->type->fieldsInfo) + ((uint64_t)o->type->fieldsInfoOffsets[i]));
        char type = fieldInfo[0];
        char* name = fieldInfo+1;
        obj* fieldValue = (obj*)(((uint64_t)(&o->others)) + ((uint64_t)(o->type->fieldsDataOffsets[i])));

        //DEBUG("FIELD %c %s (offset=%d, val=%d)", type, name, ((uint64_t)(o->type->fieldsDataOffsets[i])), *((uint32_t*) fieldValue));

        uint8_t *buffer = malloc(200);
        buffer[0] = '\0';
        u8_strcat(buffer, name);
        u8_strcat(buffer, ": ");
        result = _String_concat(result, __createString(buffer));
        free(buffer);

        switch(type) {
            case 'A': {
                if (IS_NULL(*fieldValue)) {
                    result = _String_concat(result, __createString("null"));
                } else {
                    result = _String_concat(result, _Array_toString(*fieldValue));
                }
                break;
            }
            case 'S': {
                if (IS_NULL(*fieldValue)) {
                    result = _String_concat(result, __createString("null"));
                } else {
                    result = _String_concat(result, __createString("\""));
                    result = _String_concat(result, *fieldValue);
                    result = _String_concat(result, __createString("\""));
                }
                break;
            }
            case 'C': {
                if (IS_NULL(*fieldValue)) {
                    result = _String_concat(result, __createString("null"));
                } else {
                    obj (*toString)(obj) = ((void **)(*fieldValue)->type->methods)[0];
                    result = _String_concat(result, toString(*fieldValue));
                }
                break;
            }
            case 'I': {
                uint32_t* fVal = (uint32_t*)fieldValue;
                char *val_buf = (char*)malloc(50 * sizeof(char));
                sprintf(val_buf, "%d", *fVal);
                result = _String_concat(result, __createString(val_buf));
                free(val_buf);
                break;
            }
            case 'B': {
                //DEBUG("GOT B");
                uint8_t* fVal = (uint8_t*)fieldValue;
                char *val_buf = (char*)malloc(13 * sizeof(char));
                if (*fVal) {
                    sprintf(val_buf, "true");
                } else {
                    sprintf(val_buf, "false");
                }
                result = _String_concat(result, __createString(val_buf));
                free(val_buf);
                break;
            }
        }

        if (i!=fieldsInfoLength-1) {
            result = _String_concat(result, __createString(", "));
        }
    }
    result = _String_concat(result, __createString("}"));
    return result;
}

obj typeInfoStringRepr(obj o) {
    if (type_info_string_repr_level == 0) {
        type_info_string_repr_visited_length = 0;
    }
    type_info_string_repr_level++;
    //__incRef(o);
    obj ret = _typeInfoStringRepr(o);
    __incRef(ret);
    //__decRef(o);
    type_info_string_repr_level--;
    if (type_info_string_repr_level == 0) {
        type_info_string_repr_visited_length = 0;
    }
    return ret;
}

// BuiltIn classes' methods
obj _Object_toString(obj o) {
    //obj ret = __createString("Object");
    //__incRef(ret);
    //return ret;
    return typeInfoStringRepr(o);
}
int32_t _Object_getHashCode(obj o) { return (int32_t)(int64_t)o; }
int8_t _Object_equals(obj o1, obj o2) { return o1 == o2; }



obj _Array_toString(obj arr) {
    if (arr->length == 0) {
        obj ret = __createString("[]");
        __incRef(ret);
        return ret;
    }

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
            if (IS_NULL(element)) {
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
    if (IS_NULL(o2))
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
    if (IS_NULL(substr)) {
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
    if (IS_NULL(substr)) {
        errMsg = "ERROR: EndsWith null substring argument.";
        error();
    }
    uint8_t *rs = (str->data);
    uint8_t *rsub = (substr->data);
    return u8_endswith(rs, rsub);
}
int8_t _String_startsWith(obj str, obj substr) {
    if (IS_NULL(substr)) {
        errMsg = "ERROR: StartsWith null substring argument.";
        error();
    }
    uint8_t *rs = (str->data);
    uint8_t *rsub = (substr->data);
    return u8_startswith(rs, rsub);
}
obj _String_concat(obj str, obj secondstr) {
    DEBUG("String concat on %p and %p", str, secondstr);
    if (IS_NULL(secondstr)) {
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
    if (IS_NULL(str))
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
    DEBUG("Calling generic print(obj)");
    DEBUG("Calling generic obj.type=%p", FORMAT_PTR(o->type));
    if (IS_NULL(o)) {
        DEBUG("DETECTED obj IS NULL");
        o = __createString("null");
    }
    //__incRef(o);
    obj (*toStr)(obj) = ((void **)o->type->methods)[0];
    obj str = toStr(o);
    DEBUG("Subcall to internal printString() method")
    printString(str);
    //__decRef(str);
    //__decRef(o);
    DEBUG("Generic print(obj) completed")
    return 0;
}

int8_t printBinArray(obj arr) {
    DEBUG("Calling printBinArray(arr)")
    if (IS_NULL(arr)){
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

static obj* gc_ref = NULL;
static uint32_t gc_ref_cap = 0;
static uint32_t gc_ref_length = 0;

obj __new(struct Type *t) {
    DEBUG("Calling __new v3");
    DEBUG("Perform reference malloc type=%p", FORMAT_PTR(t));
    DEBUG("__new examine type: <type parent=%p> [%d]", FORMAT_PTR(t->parent), t->dataSize);
    obj r = malloc(sizeof(struct Reference)+t->dataSize+10);

    if (gc_ref_length+1 >= gc_ref_cap) {
        // Resize GC table if needed
        const uint32_t new_gc_ref_cap = gc_ref_cap > 100 ? gc_ref_cap*2 : 100;
        void** new_gc_ref = malloc(sizeof(obj) * new_gc_ref_cap);
        for(int i=0;i<gc_ref_length;++i) {
            new_gc_ref[i] = gc_ref[i];
        }
        if (gc_ref != NULL) {
            free(gc_ref);
        }
        gc_ref = new_gc_ref;
        gc_ref_cap = new_gc_ref_cap;
    }
    // Save pointer
    DEBUG("gc_ref save %p %d %d", gc_ref, gc_ref_cap, gc_ref_length);
    gc_ref[gc_ref_length++] = (void*)r;

    DEBUG("Set __new type/counter");
    r->type = t;
    r->counter = 0;
    r->data = NULL;
    r->methods = r->type->methods;
    r->length=0;
    DEBUG("Do init for non array non string?");
    if (t->dataSize > 0) {
        //bzero((r+36), t->dataSize);
        memcpy(&(r->others), t->initializer, t->dataSize);
    }
    // if (t->dataSize > 0 && t != &_class_Array && t != &_class_String) {
    //     DEBUG("Perform init");
    //     r->data = malloc(t->dataSize);
    //     bzero(r->data, t->dataSize);
    // } else {
    //     DEBUG("Just set data to NULL");
    //     r->data = NULL;
    // }
    DEBUG("Completed __new %p <type %p, par %p> inner data=%p size=%d", FORMAT_PTR(r), FORMAT_PTR(r->type), FORMAT_PTR(r->type->parent), FORMAT_PTR(r->data), t->dataSize);
    uint64_t test = *((uint64_t*)(r+36));DEBUG("Just check. R+36=%p (is null=%d)", FORMAT_PTR(test), IS_NULL(((void*)test)));
    return r;
}

static gc_trigger_n = 0;

void run_gc() {
    gc_trigger_n++;
    if (gc_trigger_n < 10) {
        return;
    }
    gc_trigger_n = 0;
    DEBUG("RUN GC");
    for(int i=0;i<gc_ref_length;++i) {
        if(gc_ref[i] != NULL && !IS_NULL(gc_ref[i]) && gc_ref[i] != emptyString) {
            obj p = (obj)gc_ref[i];
            if (p->counter <= 0) {
                __free(p, 0);
            }
            gc_ref[i] = NULL;
        }
    }
}

void __free(obj r, uint32_t gc_pos) {
    if (gc_pos == 0) {
        return;
    }
    if (IS_NULL(r) || r == NULL) {
        DEBUG("Free completed. Value is nullish");
        return;
    }
    //DEBUG("__free %p [%s]", FORMAT_PTR(r), rrepr->data);
    if (r->type == &_class_Array) {
        DEBUG("Free got Array");
        //struct Array *arr = r->data;
        void **els = r->data;
        if (r->elementSize == sizeof(void *)) {
            for (int i = 0; i < r->length; i++)
                __decRef(els[i]);
        }
        if (els != NULL) {
            free(els);
        }
    } else if (r->type == &_class_String) {
        DEBUG("Free got String");
        void *els = (r->data);
        if (els != NULL && els != emptyString) {
            DEBUG("Free underlying String data");
            free(els);
        }
    } else {
        DEBUG("Free got other");
        //free(r->data);
    }
    DEBUG("__free end %p", FORMAT_PTR(r));
    free(r);
    gc_ref[gc_pos-1] = NULL;
}


void __decRef(obj r) {
    if (gc_level == 0) {
        gc_visited_length = 0;
    }
    ++gc_level;
    obj rrepr = typeInfoStringRepr(r);
    DEBUG("__decRef %p [%s]", FORMAT_PTR(r), rrepr->data);
    if (!__LATTE_RUNTIME_GC_ENABLED) return;
    if (!IS_NULL(r) && r != NULL) {
        for (int i=0;i<gc_visited_length;++i) {
            if (((uint64_t)r) == gc_visited[i]) {
                // Recursive call
                return;
            }
        }
        gc_visited[gc_visited_length++] = (uint64_t)r;
        r->counter--;
        if (r->counter <= 0) {
            if (true) {//r->type != &_class_Array) {
                const int fieldsInfoLength = r->type->fieldsInfoLength;
                for(int i=0;i<fieldsInfoLength;++i) {
                    char* fieldInfo = (char*) (((uint64_t)r->type->fieldsInfo) + ((uint64_t)r->type->fieldsInfoOffsets[i]));
                    char type = fieldInfo[0];
                    char* name = fieldInfo+1;
                    obj* fieldValue = (obj*)(((uint64_t)(&r->others)) + ((uint64_t)(r->type->fieldsDataOffsets[i])));
                    switch(type) {
                        case 'A': {
                            __decRef(*fieldValue);
                            break;
                        }
                        case 'S': {
                            __decRef(*fieldValue);
                            break;
                        }
                        case 'C': {
                            __decRef(*fieldValue);
                            break;
                        }
                        case 'I': {
                            // Do nothing
                            break;
                        }
                        case 'B': {
                            // Do nothing
                            break;
                        }
                    }
                }
            }
            if (r->type == &_class_Array) {
                if (r->elementSize == sizeof(void *)) {
                    void **els = r->data;
                    for (int i = 0; i < r->length; i++) {
                        __decRef(els[i]);
                    }
                }
            }
            // if (r->type != &_class_Array) {
            //     for (int i = 0; i < r->type->referenceOffsetsSize; i++)
            //         __decRef(*(obj *)(r->data + r->type->referenceOffsets[i]));
            // }
            //__free(r);
            //DEBUG("FREE!!!!");
            //free(r);

        }
    }
    DEBUG("__decRef end %p %d", FORMAT_PTR(r), gc_level-1);
    --gc_level;
    if (gc_level == 0) {
        gc_visited_length = 0;
    }
}