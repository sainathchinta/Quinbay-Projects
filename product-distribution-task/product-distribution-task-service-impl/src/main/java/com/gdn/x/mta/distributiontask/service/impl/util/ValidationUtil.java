package com.gdn.x.mta.distributiontask.service.impl.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.enums.ApiErrorCode;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.Nullable;

@Slf4j
public class ValidationUtil {

  public static VendorQuickApprovalResponse vendorQuickApprovalProductValidation(Product product,
      CategoryDetailResponse categoryDetailResponse, boolean isBulkAction,
    boolean validateAndHealQuickApprovalEnabled)  {
    VendorQuickApprovalResponse vendorQuickApprovalResponse = new VendorQuickApprovalResponse();
    List<String> errorCodes = new ArrayList<>();
    if (!Constants.APPROVED.equalsIgnoreCase(product.getBrandApprovalStatus())) {
      log.warn("Product code : {}, error {}", product.getProductCode(), ApiErrorCode.BRAND_IS_INREVIEW.getDesc());
      errorCodes.add(ApiErrorCode.BRAND_IS_INREVIEW.getCode());
    }
    if (!isBulkAction &&  product.isRestrictedKeywordsPresent()) {
      log.warn("Product code : {}, error {}", product.getProductCode(),
          ApiErrorCode.CONTAIN_RESTRICTED_KEYWORD.getDesc());
      errorCodes.add(ApiErrorCode.CONTAIN_RESTRICTED_KEYWORD.getCode());
    }
    if (isMandatoryAttributesNotFilled(categoryDetailResponse.getCategoryAttributes(),
        product.getProductAttributes(), product.getProductCode())) {
      log.warn("Product code : {}, error {}", product.getProductCode(),
          ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getDesc());
      errorCodes.add(ApiErrorCode.MANDATORY_FIELD_NOT_FOUND.getCode());
    }
    if (!isBulkAction && StringUtils.isNotEmpty(product.getImageViolations()) && !Constants.PENDING.equalsIgnoreCase(
        product.getImageViolations())) {
      log.warn("Product code : {}, error {}", product.getProductCode(), ApiErrorCode.FAULTY_ON_IMAGES.getDesc());
      errorCodes.add(ApiErrorCode.FAULTY_ON_IMAGES.getCode());
    }
    if (!isBulkAction && StringUtils.isNotEmpty(product.getTextViolations()) && !Constants.PENDING
        .equalsIgnoreCase(product.getTextViolations())) {
      log.warn("Product code : {}, error {}", product.getProductCode(),
          ApiErrorCode.FAULTY_ON_PRODUCT_TEXT_CONTENT.getDesc());
      errorCodes.add(ApiErrorCode.FAULTY_ON_PRODUCT_TEXT_CONTENT.getCode());
    }
    if (!isBulkAction && isFamilyColourNotSelected(product.getProductItems())) {
      log.warn("Product code : {}, error {}", product.getProductCode(), ApiErrorCode.FAMILY_COLOUR_NOT_FOUND.getDesc());
      errorCodes.add(ApiErrorCode.FAMILY_COLOUR_NOT_FOUND.getCode());
    }
    if(validateAndHealQuickApprovalEnabled) {
      if (CollectionUtils.isEmpty(product.getProductAttributes())) {
        log.warn("Product Attributes empty for product : {} and Error : {} ",
          product.getProductCode(), ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getDesc());
        errorCodes.add(ApiErrorCode.PRODUCT_ATTRIBUTES_CANNOT_BE_EMPTY.getCode());
      }
      if (CollectionUtils.isEmpty(product.getProductItems())) {
        log.warn("Product Attributes empty for product : {} and Error : {} ",
          product.getProductCode(), ApiErrorCode.PRODUCT_ITEMS_CANNOT_BE_EMPTY.getDesc());
        errorCodes.add(ApiErrorCode.PRODUCT_ITEMS_CANNOT_BE_EMPTY.getCode());
      }
      if (CollectionUtils.isEmpty(product.getProductImages())) {
        log.warn("Product Attributes empty for product : {} and Error : {} ",
          product.getProductCode(), ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getDesc());
        errorCodes.add(ApiErrorCode.PRODUCT_IMAGES_MUST_NOT_BE_EMPTY.getCode());
      }
      List<ProductItemImage> productItemImagesList =
        Optional.ofNullable(product.getProductItems()).orElse(Collections.emptyList()).stream()
          .flatMap(productItem -> Optional.ofNullable(productItem.getProductItemImages())
            .orElse(Collections.emptyList()).stream()).collect(Collectors.toList());
      if (CollectionUtils.isEmpty(productItemImagesList)) {
        log.warn("Product Attributes empty for product : {} and Error : {} ",
          product.getProductCode(), ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getDesc());
        errorCodes.add(ApiErrorCode.PRODUCT_ITEM_IMAGES_CANNOT_BE_EMPTY.getCode());
      }
    }
    vendorQuickApprovalResponse.setErrorCodes(errorCodes);
    return vendorQuickApprovalResponse;
  }

  private static boolean isFamilyColourNotSelected(List<ProductItem> productItems) {
    try {
      productItems.stream().map(productItem -> productItem.getProductItemAttributes())
          .flatMap(productItemAttributes -> productItemAttributes.stream())
          .filter(productItemAttribute -> Constants.FAMILY_COLOUR.equals(productItemAttribute.getName()))
          .forEach(ValidationUtil::checkFamilyColourValue);
    } catch (ApplicationRuntimeException e) {
      log.error("Family colour must not be empty");
      return true;
    }
    return false;
  }


  private static void checkFamilyColourValue(ProductItemAttribute productItemAttribute) {
    if (StringUtils.isEmpty(productItemAttribute.getValue())) {
      throw new ApplicationRuntimeException();
    }
  }

  private static boolean isMandatoryAttributesNotFilled(List<CategoryAttributeResponse> categoryAttributeResponses,
      List<ProductAttribute> productAttributes, String productCode) {
    List<String> productAttributeResponseList = categoryAttributeResponses.stream()
        .filter(productAttributeResponse -> productAttributeResponse.getAttribute().isMandatory())
        .map(productAttributeResponse -> productAttributeResponse.getAttribute().getAttributeCode())
        .collect(Collectors.toList());
    Map<String, List<ProductAttribute>> productAttributeMap =
        productAttributes.stream().collect(Collectors.groupingBy(ProductAttribute::getAttributeCode));
    for (String attributeCode : productAttributeResponseList) {
      if (!productAttributeMap.containsKey(attributeCode)) {
        log.error("Mandatory Attribute not filled attribute code : {} Product Code : {}", attributeCode, productCode);
        return true;
      }
    }
    return false;
  }

  public static void checkParameter(boolean expression, @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorMessage), String.valueOf(errorMessage));
    }
  }

  public static void checkParameter(boolean expression, @Nullable Object errorCode,
      @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorCode), String.valueOf(errorMessage));
    }
  }

  public static boolean validateIprActionField(IprActionRequest request) {
    switch (ProductStateIPR.valueOf(request.getAction())) {
      case WHITELISTED:
        ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getReviewerNotes()),
            ErrorCategory.NOTES_MUST_NOT_BE_EMPTY.getCode(),
            ErrorCategory.NOTES_MUST_NOT_BE_EMPTY.getMessage());
        break;
      case EVIDENCE_REQUESTED:
      case SUSPENDED:
        ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getSellerNotes()),
            ErrorCategory.NOTES_MUST_NOT_BE_EMPTY.getCode(),
            ErrorCategory.NOTES_MUST_NOT_BE_EMPTY.getMessage());
        ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getViolationType()),
            ErrorCategory.VIOLATION_TYPE_MUST_NOT_BE_EMPTY.getCode(),
            ErrorCategory.VIOLATION_TYPE_MUST_NOT_BE_EMPTY.getMessage());
        ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getReasons()),
            ErrorCategory.REASONS_MUST_NOT_BE_EMPTY.getCode(),
            ErrorCategory.REASONS_MUST_NOT_BE_EMPTY.getMessage());
        break;
      case RELEASED:
        break;
      default:
        return false;
    }
    return true;
  }

  public static void validateIprActionRequest(IprActionRequest iprActionRequest) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(iprActionRequest.getProductSku()),
        ErrorCategory.PRODUCT_SKU_MUST_NOT_BE_EMPTY.getCode(),
        ErrorCategory.PRODUCT_SKU_MUST_NOT_BE_EMPTY.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(iprActionRequest.getAction()),
        ErrorCategory.IPR_ACTION_MUST_NOT_BE_EMPTY.getCode(),
        ErrorCategory.IPR_ACTION_MUST_NOT_BE_EMPTY.getMessage());
    ValidationUtil.checkParameter(
        StringUtils.isNotBlank(ProductStateIPR.getValueOrEmpty(iprActionRequest.getAction())),
        ErrorCategory.INVALID_ACTION_PROVIDED.getCode(),
        ErrorCategory.INVALID_ACTION_PROVIDED.getMessage());
    ValidationUtil.checkParameter(validateIprActionField(iprActionRequest),
        ErrorCategory.INVALID_ACTION_PROVIDED.getCode(),
        ErrorCategory.INVALID_ACTION_PROVIDED.getMessage());
  }
}
