package com.gdn.mta.product.util.validator;

import java.nio.charset.StandardCharsets;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.google.common.base.CharMatcher;

public class DescriptiveFieldCharacterValidator {

  public static void validateProductCreationRequest(ProductCreationRequest productCreationRequest, List<String> exclusionList, boolean validate) {
    String description = new String(productCreationRequest.getDescription(), StandardCharsets.UTF_8);
    if (validate) {
      if (containsNonAsciiCharacter(productCreationRequest.getName(), exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else if (containsNonAsciiCharacter(description, exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else if (StringUtils.isNotBlank(productCreationRequest.getUniqueSellingPoint()) && containsNonAsciiCharacter(
          productCreationRequest.getUniqueSellingPoint(), exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else {
        for (ProductAttributeRequest productAttributeRequest : productCreationRequest.getProductAttributes()) {
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())) {
            for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest.getProductAttributeValues()) {
              if (StringUtils.isNotBlank(productAttributeValueRequest.getDescriptiveAttributeValue())
                  && containsNonAsciiCharacter(productAttributeValueRequest.getDescriptiveAttributeValue(), exclusionList)) {
                throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
              }
            }
          }
        }
      }
    }
  }

  public static void validateProductLevel3Request(ProductL3UpdateRequest productL3UpdateRequest, List<String> exclusionList, boolean validate) {
    if (validate) {
      if (containsNonAsciiCharacter(productL3UpdateRequest.getProductName(), exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else if (containsNonAsciiCharacter(productL3UpdateRequest.getDescription(), exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else if (StringUtils.isNotBlank(productL3UpdateRequest.getUniqueSellingPoint()) && containsNonAsciiCharacter(
          productL3UpdateRequest.getUniqueSellingPoint(), exclusionList)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
      } else {
        for (ProductLevel3AttributeRequest productLevel3AttributeRequest : productL3UpdateRequest.getAttributes()) {
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(productLevel3AttributeRequest.getAttributeType())
              && CollectionUtils.isNotEmpty(productLevel3AttributeRequest.getValues())) {
            for (String value : productLevel3AttributeRequest.getValues()) {
              if (StringUtils.isNotBlank(value) && containsNonAsciiCharacter(value, exclusionList)) {
                throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_CHARACTERS_IN_PRODUCT_INFO);
              }
            }
          }
        }
      }
    }
  }

  private static boolean containsNonAsciiCharacter(String data, List<String> exclusionList) {
    for (String exclusionSymbol : exclusionList) {
      data = data.replaceAll(exclusionSymbol, StringUtils.EMPTY);
    }
    return !CharMatcher.ascii().matchesAllOf(data);
  }

}
