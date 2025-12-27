package com.gdn.mta.product.commons.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.gdn.mta.product.commons.constant.UpdateProductActivity;

@Retention(value=RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface LogAuditUpdateProduct {
  UpdateProductActivity value();
}
