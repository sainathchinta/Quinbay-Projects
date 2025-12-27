package com.gdn.partners.pcu.internal.validaton.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import com.gdn.partners.pcu.internal.validaton.validator.DraftProductAttributeValidator;

@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = DraftProductAttributeValidator.class)
public @interface DraftProductAttributeValid {

  String message();

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};
}
