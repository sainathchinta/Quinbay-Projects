package com.gdn.x.productcategorybase.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.Gender;
import com.gdn.x.productcategorybase.dto.SizeChartHeaderType;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataColumn;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataRow;
import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.exception.ValidationException;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.Nullable;
import static com.gdn.common.base.GdnPreconditions.checkArgument;
import org.apache.commons.lang.time.DateUtils;
import org.springframework.beans.factory.annotation.Value;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.Date;


@Slf4j
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ValidationUtil {

  public static void checkParameter(boolean expression, @Nullable Object errorCode,
    @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorCode), String.valueOf(errorMessage));
    }
  }

  public static void checkParameter(boolean expression, @Nullable Object errorMessage) {
    if (!expression) {
      throw new ValidationException(String.valueOf(errorMessage), String.valueOf(errorMessage));
    }
  }

  public static void validateSizeChartRequest(SizeChartRequest sizeChartRequest,
    Map<String, String> dimensionNameToCodeMap, Set<String> sizeChartValidUnits) {
    checkParameter(StringUtils.isNotBlank(sizeChartRequest.getName()),
        ErrorMessage.SIZE_CHART_NAME_CANNOT_BE_BLANK_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_NAME_CANNOT_BE_BLANK.getMessage());
    checkParameter(StringUtils.isNotBlank(sizeChartRequest.getBusinessPartnerCode()),
        ErrorMessage.BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK_ERROR_CODE.getMessage(),
        ErrorMessage.BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK.getMessage());
    checkParameter(StringUtils.isNotBlank(sizeChartRequest.getSizeAttributeName()),
        ErrorMessage.ATTRIBUTE_NAME_MUST_NOT_BE_BLANK_ERROR_CODE.getMessage(),
        ErrorMessage.ATTRIBUTE_NAME_MUST_NOT_BE_BLANK.getMessage());
    checkParameter(CollectionUtils.isNotEmpty(sizeChartRequest.getSelectedValueTypes()),
        ErrorMessage.SELECTED_VALUE_TYPES_LIST_CANNOT_BE_EMPTY_ERROR_CODE.getMessage(),
        ErrorMessage.SELECTED_VALUE_TYPES_LIST_CANNOT_BE_EMPTY.getMessage());
    checkParameter(CollectionUtils.isNotEmpty(sizeChartRequest.getSelectedDimensionCodes()),
        ErrorMessage.SELECTED_DIMENSION_LIST_CANNOT_BE_EMPTY_ERROR_CODE.getMessage(),
        ErrorMessage.SELECTED_DIMENSION_LIST_CANNOT_BE_EMPTY.getMessage());
    checkParameter(StringUtils.isNotBlank(sizeChartRequest.getGender()),
        ErrorMessage.SIZE_CHART_GENDER_CANNOT_BE_BLANK_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_GENDER_CANNOT_BE_BLANK.getMessage());
    if(StringUtils.equals(sizeChartRequest.getBusinessPartnerCode(), Constants.INTERNAL)) {
      checkParameter(StringUtils.isNotBlank(sizeChartRequest.getBrand()),
          ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK_ERROR_CODE.getMessage(),
          ErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK.getMessage());
      checkParameter(Gender.validGender(sizeChartRequest.getGender()),
          ErrorMessage.GENDER_IS_INVALID_ERROR_CODE.getMessage(),
          ErrorMessage.GENDER_IS_INVALID.getMessage());
    }
    checkParameter(StringUtils.isNotBlank(sizeChartRequest.getUnit()),
        ErrorMessage.SIZE_CHART_UNIT_CANNOT_BE_BLANK_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_UNIT_CANNOT_BE_BLANK.getMessage());
    checkParameter(sizeChartValidUnits.contains(sizeChartRequest.getUnit()),
        ErrorMessage.SIZE_CHART_INVALID_UNIT_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_INVALID_UNIT.getMessage());
    checkParameter(CollectionUtils.isNotEmpty(sizeChartRequest.getSizeChartRows()),
        ErrorMessage.SIZE_CHART_ROWS_CANNOT_BE_EMPTY_ERROR_CODE.getMessage(),
        ErrorMessage.SIZE_CHART_ROWS_CANNOT_BE_EMPTY.getMessage());
    int columnCount =
      sizeChartRequest.getSelectedValueTypes().size() + sizeChartRequest.getSelectedDimensionCodes()
        .size();
    validateSizeChartRows(sizeChartRequest.getSizeChartRows(), columnCount, dimensionNameToCodeMap,
      sizeChartRequest.getSelectedDimensionCodes(), sizeChartRequest.getSelectedValueTypes());
  }

  private static void validateSizeChartRows(List<SizeChartDataRow> sizeChartRows, int columnCount,
    Map<String, String> dimensionNameToCodeMap, List<String> selectedDimensionCodes,
    List<String> selectedValueTypes) {
    for (SizeChartDataRow sizeChartDataRow : sizeChartRows) {
      checkParameter(columnCount == sizeChartDataRow.getColumns().size(),
          ErrorMessage.SIZE_CHART_HEADER_MISMATCH_ERROR_CODE.getMessage(),
          ErrorMessage.SIZE_CHART_HEADER_MISMATCH.getMessage());
      for (SizeChartDataColumn sizeChartDataColumn : sizeChartDataRow.getColumns()) {
        checkParameter(
            SizeChartHeaderType.validSizeChartHeaderType(sizeChartDataColumn.getKeyType()),
            ErrorMessage.INVALID_HEADER_TYPE_IN_SIZE_CHART_ERROR_CODE.getMessage(),
            ErrorMessage.INVALID_HEADER_TYPE_IN_SIZE_CHART.getMessage());
        if (SizeChartHeaderType.DIMENSION.name().equals(sizeChartDataColumn.getKeyType())) {
          checkParameter(selectedDimensionCodes.contains(
                  dimensionNameToCodeMap.get(sizeChartDataColumn.getKeyName())),
              ErrorMessage.SELECTED_DIMENSION_AND_ROW_DIMENSION_DOES_NOT_MATCH_ERROR_CODE.getMessage(),
              ErrorMessage.SELECTED_DIMENSION_AND_ROW_DIMENSION_DOES_NOT_MATCH.getMessage());
          checkParameter(StringUtils.isNotBlank(sizeChartDataColumn.getValue()) || (
              StringUtils.isNotBlank(sizeChartDataColumn.getMin()) && StringUtils.isNotBlank(
                sizeChartDataColumn.getMax())), ErrorMessage.DIMENSION_VALUE_CANNOT_BE_EMPTY_ERROR_CODE.getMessage(),
            ErrorMessage.DIMENSION_VALUE_CANNOT_BE_EMPTY.getMessage());
          // Validation to ensure that the provided value is either a single value or a range, but not both
          // XOR operator returns true only if one of the conditions is met.
          checkParameter(StringUtils.isNotBlank(sizeChartDataColumn.getValue()) ^
                  (StringUtils.isNotBlank(sizeChartDataColumn.getMin()) || StringUtils.isNotBlank(
                      sizeChartDataColumn.getMax())),
              ErrorMessage.DIMENSION_VALUE_CAN_EITHER_BE_SINGLE_OR_RANGE_ERROR_CODE.getMessage(),
              ErrorMessage.DIMENSION_VALUE_CAN_EITHER_BE_SINGLE_OR_RANGE.getMessage());
          validateDimensionValue(sizeChartDataColumn);
        } else {
          checkParameter(selectedValueTypes.contains(sizeChartDataColumn.getKeyName()),
              ErrorMessage.SELECTED_VALUE_TYPE_AND_ROW_VALUE_TYPE_DOES_NOT_MATCH_ERROR_CODE.getMessage(),
              ErrorMessage.SELECTED_VALUE_TYPE_AND_ROW_VALUE_TYPE_DOES_NOT_MATCH.getMessage());
          checkParameter(StringUtils.isNotBlank(sizeChartDataColumn.getValue()),
              ErrorMessage.VALUE_TYPE_IS_NOT_SELECTED_ERROR_CODE.getMessage(),
              ErrorMessage.VALUE_TYPE_IS_NOT_SELECTED.getMessage());
        }
      }
    }
  }

  public static void validateDimensionValue(SizeChartDataColumn sizeChartDataColumn) {
    if (StringUtils.isBlank(sizeChartDataColumn.getValue())) {
      checkParameter(validateSizeChartRange(sizeChartDataColumn),
          ErrorMessage.MAX_VALUE_SHOULD_BE_GREATER_THAN_MIN_VALUE_ERROR_CODE.getMessage(),
          ErrorMessage.MAX_VALUE_SHOULD_BE_GREATER_THAN_MIN_VALUE.getMessage());
    } else {
      validateAndConvertNumericString(sizeChartDataColumn.getValue());
    }
  }

  public static boolean validateSizeChartRange(SizeChartDataColumn sizeChartDataColumn) {
    double min = validateAndConvertNumericString(sizeChartDataColumn.getMin());
    double max = validateAndConvertNumericString(sizeChartDataColumn.getMax());
    return Double.compare(max, min) > Constants.ZERO;
  }

  public static Double validateAndConvertNumericString(String number){
    try {
      return Double.parseDouble(number);
    }
    catch (NumberFormatException ex){
      throw new ValidationException(
          ErrorMessage.DIMENSION_VALUE_CAN_ONLY_HAVE_NUMERIC_VALUES_ERROR_CODE.getMessage(),
          ErrorMessage.DIMENSION_VALUE_CAN_ONLY_HAVE_NUMERIC_VALUES.getMessage());
    }
  }

  public static void validateSelectedValueTypes(Attribute attribute,
    List<String> selectedValueTypes) throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    List<String> attributeValueTypes = new ArrayList<>();
    if (StringUtils.isNotBlank(attribute.getValueTypes())) {
      attributeValueTypes =
        objectMapper.readValue(attribute.getValueTypes(), new TypeReference<List<String>>() {
        });
    }
    checkParameter(new HashSet<>(attributeValueTypes).containsAll(selectedValueTypes),
        ErrorMessage.VALUE_TYPES_SELECTED_IS_NOT_MAPPED_TO_ATTRIBUTE_ERROR_CODE.getMessage(),
        ErrorMessage.VALUE_TYPES_SELECTED_IS_NOT_MAPPED_TO_ATTRIBUTE.getMessage());
  }

  public static Map<String, String> validateSelectedDimensions(
    List<DimensionMapping> dimensionMappingList, List<String> selectedDimensionCodes) {
    Map<String, String> dimensionNameToCodeMap = dimensionMappingList.stream().collect(
      Collectors.toMap(dimensionMapping -> dimensionMapping.getDimension().getName(),
        dimensionMapping -> dimensionMapping.getDimension().getDimensionCode()));
    ValidationUtil.checkParameter(
        dimensionNameToCodeMap.values().containsAll(selectedDimensionCodes),
        ErrorMessage.DIMENSION_MAPPING_FOR_SIZE_CHART_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE_ERROR_CODE.getMessage(),
        ErrorMessage.DIMENSION_MAPPING_DOES_NOT_EXIST_FOR_ATTRIBUTE_CODE);
    return dimensionNameToCodeMap;
  }

  public static void validateDimensionMappingUpdateRequest(
    List<DimensionMappingRequest> dimensionMappingRequests, Set<String> uniqueDimensionIds) {
    dimensionMappingRequests.forEach(dimensionMappingRequest -> checkParameter(
      uniqueDimensionIds.add(dimensionMappingRequest.getDimensionId()),
      ErrorMessage.INVALID_DIMENSION_MAPPING_UPDATE_REQUEST.getMessage()));
  }

  public static boolean isExternal(String businessPartnerCode) {
    return !StringUtils.equals(businessPartnerCode, Constants.INTERNAL);
  }

  public static void validateBrandAuthorisationRequest(String storeId,
    BrandAuthUpdateRequest brandAuthUpdateRequest) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(brandAuthUpdateRequest.getBrandCode()),
      ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotBlank(brandAuthUpdateRequest.getSellerCode()),
      ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK.getMessage());
    ValidationUtil.checkParameter(
      CollectionUtils.isNotEmpty(brandAuthUpdateRequest.getDocumentLinks()),
      ErrorMessage.DOCUMENT_LIST_CANNOT_BE_EMPTY.getMessage());
    ValidationUtil.checkParameter(Objects.nonNull(brandAuthUpdateRequest.getAuthStartDate()),
      ErrorMessage.AUTH_START_OR_END_DATE_MUST_NOT_BE_NULL.getMessage());
    ValidationUtil.checkParameter(Objects.nonNull(brandAuthUpdateRequest.getAuthExpireDate()),
      ErrorMessage.AUTH_START_OR_END_DATE_MUST_NOT_BE_NULL.getMessage());
    ValidationUtil.checkParameter(brandAuthUpdateRequest.getAuthExpireDate().toInstant()
        .isAfter(brandAuthUpdateRequest.getAuthStartDate().toInstant()),
      ErrorMessage.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE.getMessage());
  }

  public static void validateCreateBrandAuthWipRequest(String storeId,
      BrandAuthCreateWipRequest brandAuthCreateWipRequest, int numberOfYears,
      BrandResponse savedBrand, boolean isInternalCreation) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandAuthCreateWipRequest.getBrandCode()),
        String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    checkArgument(isInternalCreation || StringUtils.isNotBlank(
            brandAuthCreateWipRequest.getIprRegistrationNumber()),
        String.valueOf(ErrorMessage.IPR_REGISTRATION_NUMBER_MUST_NOT_BE_BLANK));
    if (Objects.isNull(brandAuthCreateWipRequest.getAuthStartDate())) {
      brandAuthCreateWipRequest.setAuthStartDate(new Date());
    }
    if (Objects.isNull(brandAuthCreateWipRequest.getAuthExpireDate())) {
      brandAuthCreateWipRequest.setAuthExpireDate(DateUtils.addYears(new Date(), numberOfYears));
    }
    if (brandAuthCreateWipRequest.getAuthExpireDate()
        .before(brandAuthCreateWipRequest.getAuthStartDate())) {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          ErrorMessage.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE.getMessage());
    }
    if (CollectionUtils.isNotEmpty(brandAuthCreateWipRequest.getDocumentLinks())) {
      checkArgument(
          brandAuthCreateWipRequest.getDocumentLinks().stream().noneMatch(StringUtils::isEmpty),
          ErrorMessage.FILE_NAME_SHOULD_NOT_BE_NULL_OR_EMPTY.getMessage());
    }
    if (Objects.isNull(savedBrand)) {
      log.error("Brand not found {} ", brandAuthCreateWipRequest.getBrandCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.BRAND_IS_NOT_VALID_OR_NOT_PROTECTED.getMessage());
    }
    if (!isInternalCreation && !savedBrand.isProtectedBrand()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.BRAND_IS_NOT_PROTECTED.getMessage());
    }
  }
}
