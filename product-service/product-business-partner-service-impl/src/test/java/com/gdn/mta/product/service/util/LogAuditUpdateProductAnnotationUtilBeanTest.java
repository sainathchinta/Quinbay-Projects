package com.gdn.mta.product.service.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;

public class LogAuditUpdateProductAnnotationUtilBeanTest {

  private LogAuditUpdateProductAnnotationUtilBean annotationUtil = new LogAuditUpdateProductAnnotationUtilBean();
  
  @Test
  public void testGetLogAuditUpdateProductPropertyMap() {
    Assertions.assertNotNull(annotationUtil.getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class));
  }
}
