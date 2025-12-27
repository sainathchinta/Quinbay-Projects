package com.gdn.mta.product.service.util;

import java.util.Map;

import com.gdn.mta.product.commons.constant.UpdateProductActivity;

public interface LogAuditUpdateProductAnnotationUtil {
  Map<UpdateProductActivity, String> getLogAuditUpdateProductPropertyMap(
      Class<?> domainClass);
}
