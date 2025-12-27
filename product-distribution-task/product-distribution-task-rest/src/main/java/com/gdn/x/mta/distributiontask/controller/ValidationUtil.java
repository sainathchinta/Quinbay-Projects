package com.gdn.x.mta.distributiontask.controller;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.service.impl.PDTExceptions.ExceptionUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import static com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil.*;

import java.util.Objects;

/**
 * Created by Vishal on 16/12/16.
 */
final class ValidationUtil {

  private static final int ATTRIBUTE_VALUE_LENGTH = 255;

  private ValidationUtil() {
    throw new IllegalAccessError("Utility class");
  }

  static ValidationUtil getInstance() {
    return new ValidationUtil();
  }

  static void validateRejectProductParams(String vendorCode,
      RejectProductVendorRequest rejectProductVendorRequest) throws Exception {
    ExceptionUtil.checkConditions(!StringUtils.isEmpty(vendorCode), VENDOR_CODE_EMPTY_ERROR);
    ExceptionUtil.checkConditions(rejectProductVendorRequest != null, REJECT_PRODUCT_DETAIL_ERROR);
    ExceptionUtil.checkConditions(!StringUtils.isEmpty(rejectProductVendorRequest.getProductCode()),
        PRODUCT_CODE_EMPTY);
    ExceptionUtil
        .checkConditions(!StringUtils.isEmpty(rejectProductVendorRequest.getNotes()), NOTES_EMPTY);
    ExceptionUtil.checkConditions(Objects.nonNull(rejectProductVendorRequest.getRejectReasonRequest()),
        REJECT_REASON_NULL_ERROR);
    ExceptionUtil.checkConditions(
        CollectionUtils.isNotEmpty(rejectProductVendorRequest.getRejectReasonRequest().getContent())
            || CollectionUtils.isNotEmpty(rejectProductVendorRequest.getRejectReasonRequest().getProduct())
            || CollectionUtils.isNotEmpty(rejectProductVendorRequest.getRejectReasonRequest().getImage()),
        REJECT_REASON_OF_PRODUCT_NULL_OR_EMPTY);
  }


  static void validateUpdateProductImageDetails(DistributionProductDetailRequest request)
      throws Exception {
    validateProductContentImageUpdate(request);
    ExceptionUtil.checkConditions(!CollectionUtils.isEmpty(request.getProductImages()),
        PRODUCT_IMAGES_EMPTY);
  }

  private static void validateProductContentImageUpdate(DistributionProductDetailRequest request)
      throws Exception {
    ExceptionUtil
        .checkConditions(!StringUtils.isEmpty(request.getProductCode()), PRODUCT_CODE_EMPTY);
    ExceptionUtil
        .checkConditions(!StringUtils.isEmpty(request.getProductName()), PRODUCT_NAME_EMPTY);
    ExceptionUtil.checkConditions(request.getCreatedDate() != null, CREATED_DATE_EMPTY);
  }

  static void validateExistingProductDetails(String vendorCode, Product existingProduct)
      throws Exception {
    ExceptionUtil.checkConditions(existingProduct != null, PRODUCT_NOT_FOUND_ERROR);
    ExceptionUtil.checkConditions(existingProduct.getCurrentVendor() != null,
        PRODUCT_UNASSIGNED_STATUS_ERROR);
    ExceptionUtil
        .checkConditions(vendorCode.equals(existingProduct.getCurrentVendor().getVendorCode()),
            PRODUCT_UNASSIGNED_TO_VENDOR_ERROR);
  }

  static void validateUpdateProductDetails(DistributionProductDetailRequest request)
      throws Exception {
    ValidationUtil.validateProductContentImageUpdate(request);
    ExceptionUtil.checkConditions(request.getDescription() != null, DESCRIPTION_MUST_NOT_BE_BLANK);
    ExceptionUtil
        .checkConditions(request.getLongDescription() != null, LONG_DESCRIPTION_MUST_NOT_BE_BLANK);
    ExceptionUtil.checkConditions(request.getLength() != null, LENGTH_MUST_NOT_BE_BLANK);
    ExceptionUtil.checkConditions(request.getWidth() != null, WIDTH_MUST_NOT_BE_BLANK);
    ExceptionUtil.checkConditions(request.getHeight() != null, HEIGHT_MUST_NOT_BE_BLANK);
    ExceptionUtil.checkConditions(request.getWeight() != null, WEIGHT_MUST_NOT_BE_BLANK);
    ExceptionUtil
        .checkConditions(request.getShippingWeight() != null, SHIPPING_WEIGHT_MUST_NOT_BE_BLANK);
    ExceptionUtil.checkConditions(!CollectionUtils.isEmpty(request.getProductAttributes()),
        PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK);
    for (DistributionProductAttributeRequest attributes : request.getProductAttributes()) {
      String attributeValue = attributes.getValue();
      if (!StringUtils.isEmpty(attributeValue)) {
        ExceptionUtil.checkConditions(attributeValue.length() <= ATTRIBUTE_VALUE_LENGTH,
            ATTRIBUTE_VALUE_LENGTH_EXCEEDED_LENGTH);
      }

    }
  }
}
