package com.gdn.x.product.rest.web.helper.impl.test;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.x.product.rest.web.model.FieldType;

public class ModelCheckerUtil {

  private static final String DELIMITER = ".";
  private static final String OBJ = "obj";
  private static final Date DEFAULT_DATE = new Date();
  private final GdnMapper mapper;

  public ModelCheckerUtil(GdnMapper mapper) {
    this.mapper = mapper;
  }

  public List<String> checkField(Class<?> component, Class<?> smaller, String... ignore)
      throws Exception {
    Object initiate = initiate(component, null);
    System.out.println(initiate);
    Object mappedObject = mapper.deepCopy(initiate, smaller);
    List<String> arrayList = new ArrayList<>();
    checkIfProperlySet(mappedObject.getClass(), mappedObject, arrayList,
        Stream.of(ignore).collect(Collectors.toList()));
    return arrayList;
  }

  private boolean checkIfProperlySet(Class<?> type, Object object, List<String> strangeFields,
      List<String> ignore) throws Exception {
    String typeName = type.getName().toUpperCase().replace(DELIMITER, "");
    FieldType fieldType = FieldType.value(typeName);
    try {
      switch (fieldType) {
        case JAVAUTILLIST:
          if (object == null) {
            return false;
          } else {
            Object list = ((List<Object>) object).get(0);
            return checkIfProperlySet(list.getClass(), list, strangeFields, ignore);
          }
        case JAVAUTILMAP:
          if (object == null) {
            return false;
          } else {
            Map<Object, Object> map = (Map<Object, Object>) object;
            Object key = map.keySet().iterator().next();
            Object value = map.values().iterator().next();
            boolean keyIsSet = checkIfProperlySet(key.getClass(), key, strangeFields, ignore);
            boolean valueIsSet = checkIfProperlySet(value.getClass(), value, strangeFields, ignore);
            return keyIsSet && valueIsSet;
          }
        case JAVAUTILSET:
          if (object == null) {
            return false;
          } else {
            Object set = ((Set<Object>) object).iterator().next();
            return checkIfProperlySet(set.getClass(), set, strangeFields, ignore);
          }
        case OBJECT:
          if (object == null) {
            return false;
          } else {
            if (Enum.class.isAssignableFrom(object.getClass())) {
              return object == object.getClass().getEnumConstants()[0];
            } else {
              for (Field field : object.getClass().getDeclaredFields()) {
                int modifiers = field.getModifiers();
                if (Modifier.isFinal(modifiers) || Modifier.isStatic(modifiers)) {
                  continue;
                }
                field.setAccessible(true);
                Class<?> typeField = field.getType();
                if (!checkIfProperlySet(typeField, field.get(object), strangeFields, ignore)) {
                  String qualifiedName =
                      field.getDeclaringClass().getName() + DELIMITER + field.getName();
                  System.out.println(qualifiedName);
                  if (!ignore.contains(qualifiedName)) {
                    strangeFields.add(qualifiedName);
                  }
                }
              }
              return true;
            }
          }
        default:
          return getDefaultPrimitive(fieldType).equals(object);
      }
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  private Object getDefaultPrimitive(FieldType fieldType) {
    switch (fieldType) {
      case INT:
      case JAVALANGINTEGER:
        return Integer.valueOf(1);
      case DOUBLE:
      case JAVALANGDOUBLE:
        return Double.valueOf(1D);
      case BYTE:
      case JAVALANGBYTE:
        return Byte.valueOf(OBJ);
      case SHORT:
      case JAVALANGSHORT:
        return Short.valueOf(OBJ);
      case LONG:
      case JAVALANGLONG:
        return Long.valueOf(1L);
      case FLOAT:
      case JAVALANGFLOAT:
        return Float.valueOf(1F);
      case BOOLEAN:
      case JAVALANGBOOLEAN:
        return Boolean.valueOf(true);
      case CHAR:
      case JAVALANGCHARACTER:
        return Character.valueOf(OBJ.charAt(0));
      case JAVALANGSTRING:
        return new String(OBJ);
      case JAVAUTILDATE:
        return DEFAULT_DATE;
      case JAVAMATHBIGDECIMAL:
        return new BigDecimal(1);
      default:
        throw new UnsupportedOperationException();
    }
  }

  private Object initiate(Class<?> classObject, Type type) throws Exception {
    String typeName = classObject.getName().toUpperCase().replace(DELIMITER, "");
    FieldType fieldType = FieldType.value(typeName);
    try {
      switch (fieldType) {
        case JAVAUTILLIST:
          ArrayList<Object> list = new ArrayList<>();
          Class<?> stringListClass =
              (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[0];
          list.add(initiate(stringListClass, null));
          return list;
        case JAVAUTILMAP:
          Map<Object, Object> map = new HashMap<>();
          Class<?> keyClass = (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[0];
          Class<?> valueClass = (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[1];
          map.put(initiate(keyClass, null), initiate(valueClass, null));
          return map;
        case JAVAUTILSET:
          Set<Object> set = new java.util.HashSet<>();
          Class<?> setClass = (Class<?>) ((ParameterizedType) type).getActualTypeArguments()[0];
          set.add(initiate(setClass, null));
          return set;
        case OBJECT:
          if (Enum.class.isAssignableFrom(classObject)) {
            Object sample = classObject.getEnumConstants()[0];
            return sample;
          } else {
            Object object = classObject.newInstance();
            for (Field field : classObject.getDeclaredFields()) {
              int modifiers = field.getModifiers();
              if (Modifier.isFinal(modifiers) || Modifier.isStatic(modifiers)) {
                continue;
              }
              field.setAccessible(true);
              Class<?> declaringClass = field.getType();
              field.set(object, initiate(declaringClass, field.getGenericType()));
            }
            return object;
          }
        default:
          return getDefaultPrimitive(fieldType);
      }
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    }
  }

}
