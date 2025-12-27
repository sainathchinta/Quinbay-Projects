package com.gdn.partners.pcu.internal.validaton.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


import com.gdn.partners.pcu.internal.validaton.validator.SuspensionProductBulkActionWebRequestValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = SuspensionProductBulkActionWebRequestValidator.class)
public @interface SuspensionProductBulkActionsWebRequestValid {

  String message();

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};

}
