package com.gdn.partners.pcu.internal.validaton.validator;

import java.util.Objects;


import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang3.StringUtils;

import com.gdn.partners.pcu.internal.validaton.annotation.DraftProductAttributeValid;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;

public class DraftProductAttributeValidator
    implements ConstraintValidator<DraftProductAttributeValid, ProductWebRequest> {

  @Override
  public void initialize(DraftProductAttributeValid draftProductAttributeValid) {
  }

  @Override
  public boolean isValid(ProductWebRequest productWebRequest,
      ConstraintValidatorContext constraintValidatorContext) {
    return productWebRequest.getProductAttributes().stream()
        .filter(this::validateProductAttributeForBasicView)
        .filter(p -> AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE.name().equals(p.getAttribute().getAttributeType().name()))
        .allMatch(this::validateDescriptiveAttribute);
  }

  private boolean validateProductAttributeForBasicView(ProductAttributeWebRequest productAttributeWebRequest) {
    return Objects.nonNull(productAttributeWebRequest) &&
        Objects.nonNull(productAttributeWebRequest.getAttribute()) &&
        productAttributeWebRequest.getAttribute().isBasicView() &&
        !productAttributeWebRequest.getAttribute().isSkuValue();
  }

  private boolean validateDescriptiveAttribute(ProductAttributeWebRequest productAttributeWebRequest) {
    return productAttributeWebRequest.getProductAttributeValues().stream()
        .map(ProductAttributeValueWebRequest::getDescriptiveAttributeValue)
        .noneMatch(StringUtils::isEmpty);
  }

}
