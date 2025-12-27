package com.gdn.partners.pcu.external.validation.validator.Annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;

import com.gdn.partners.pcu.external.validation.validator.Validations.CreateBrandWipValidation;

@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = CreateBrandWipValidation.class)
public @interface CreateBrandWipValid {

  String message();

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};
}
