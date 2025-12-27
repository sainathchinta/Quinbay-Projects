package com.gdn.mta.product.service.util;

import java.lang.reflect.Field;
import java.util.EnumMap;
import java.util.Map;

import org.springframework.stereotype.Component;

import com.gdn.mta.product.commons.annotation.LogAuditUpdateProduct;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;

@Component
public class LogAuditUpdateProductAnnotationUtilBean implements LogAuditUpdateProductAnnotationUtil{

  @Override
  public Map<UpdateProductActivity, String> getLogAuditUpdateProductPropertyMap(
      Class<?> domainClass) {
    Map<UpdateProductActivity,String> result = new EnumMap<>(UpdateProductActivity.class);
    Field[] fields = domainClass.getDeclaredFields();
    for (Field field : fields) {
      LogAuditUpdateProduct annotation = field.getAnnotation(LogAuditUpdateProduct.class);
      if (annotation != null) {
        result.put(annotation.value(), field.getName());
      }
    }
    return result;
  }
}
