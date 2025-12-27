package com.gdn.partners.pcu.master.service.impl.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.pcu.master.client.model.DimensionMappingDTO;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.partners.pcu.master.model.request.MinWholesaleDiscountServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleConfigServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleMappingServiceRequest;
import com.gdn.partners.pcu.master.web.model.request.AutoApprovalRuleDetailsDto;
import com.gdn.x.productcategorybase.dto.request.MinWholesaleDiscountRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleConfigRequest;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;
import com.gdn.partners.pcu.master.client.model.ModifyDimensionMappingRequest;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.master.web.model.request.ModifyDimensionMappingWebRequest;
import com.google.common.base.Preconditions;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.model.attribute.AttributeValueUpdateModel;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryAttributeUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryKeywordsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeValueUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;

public class RequestHelper {

  private static final String IFRAME = "iframe";
  private static final String LEADING_LT = "^<+";
  private static final String TRAILING_GT = ">+$";

  public static MasterAttributeUpdateRequest toMasterAttributeUpdateRequest(
      AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest) {
    MasterAttributeUpdateRequest masterAttributeUpdateRequest = new MasterAttributeUpdateRequest();
    masterAttributeUpdateRequest.setSortType(
        AttributeSortTypeRequest.valueOf(attributeValuesUpdateServiceRequest.getSortType()));
    masterAttributeUpdateRequest.setUpdatedBy(attributeValuesUpdateServiceRequest.getUpdatedBy());
    masterAttributeUpdateRequest.setUpdatedDate(attributeValuesUpdateServiceRequest.getUpdatedDate());
    attributeValuesUpdateServiceRequest.getAttributeValues()
        .stream()
        .map(RequestHelper::toAttributeValueUpdateRequest)
        .collect(Collectors.toCollection(masterAttributeUpdateRequest::getAttributeValues));
    attributeValuesUpdateServiceRequest.getAddedAttributeValues()
        .stream()
        .map(RequestHelper::toAttributeValueUpdateRequest)
        .collect(Collectors.toCollection(masterAttributeUpdateRequest::getAddedAttributeValues));
    attributeValuesUpdateServiceRequest.getDeletedAttributeValues()
        .stream()
        .map(RequestHelper::toAttributeValueUpdateRequest)
        .collect(Collectors.toCollection(masterAttributeUpdateRequest::getDeletedAttributeValues));
    return masterAttributeUpdateRequest;
  }

  public static MasterAttributeAddRequest toMasterAttributeAddRequest(
      AttributeValueAddServiceRequest attributeValueAddServiceRequest) {
    MasterAttributeAddRequest masterAttributeAddRequest =
        MasterAttributeAddRequest.builder().value(attributeValueAddServiceRequest.getValue())
            .sequence(attributeValueAddServiceRequest.getSequence()).build();
    masterAttributeAddRequest.setCreatedBy(attributeValueAddServiceRequest.getCreatedBy());
    masterAttributeAddRequest.setCreatedDate(attributeValueAddServiceRequest.getCreatedDate());
    return masterAttributeAddRequest;
  }

  private static AttributeValueUpdateRequest toAttributeValueUpdateRequest(
      AttributeValueUpdateModel attributeValueUpdateModel){
    return AttributeValueUpdateRequest.builder()
        .id(attributeValueUpdateModel.getId())
        .allowedAttributeCode(attributeValueUpdateModel.getAllowedAttributeCode())
        .value(attributeValueUpdateModel.getValue())
        .valueEn(attributeValueUpdateModel.getValueEn())
        .valueType(attributeValueUpdateModel.getValueType())
        .sequence(attributeValueUpdateModel.getSequence())
        .build();
  }

  public static MasterAttributeRequest convertToMasterAttributeRequest(MasterAttributeDTO
      request) {
    MasterAttributeRequest attribute = new MasterAttributeRequest();
    BeanUtils.copyProperties(request, attribute, "allowedAttributeValues", "predefinedAllowedAttributeValues",
        "attributeType", "sortType");
    setAttributeTypeAndSortType(attribute, request);
    return attribute;
  }

  private static void setAttributeTypeAndSortType(MasterAttributeRequest attribute,
      MasterAttributeDTO request) {
    if (request.getAttributeType() != null) {
      attribute.setAttributeType(AttributeType.valueOf(request.getAttributeType().toString()));
    }
    if (request.getSortType() != null) {
      attribute.setSortType(AttributeSortTypeRequest.valueOf(request.getSortType().toString()));
    }
  }

  public static CategoryInfoUpdateRequest toCategoryInfoUpdateRequest(
      CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest) {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = new CategoryInfoUpdateRequest();
    BeanUtils.copyProperties(categoryInfoUpdateServiceRequest, categoryInfoUpdateRequest);
    return categoryInfoUpdateRequest;
  }

  public static CategoryInfoUpdateRequest toCategoryInfoUpdateRequestForStatus(
      CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest) {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = new CategoryInfoUpdateRequest();
    BeanUtils.copyProperties(categoryStatusChangeServiceRequest, categoryInfoUpdateRequest);
    return categoryInfoUpdateRequest;
  }

  public static CategoryMappingsUpdateRequest toCategoryMappingsUpdateRequest(
      CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest) {
    CategoryMappingsUpdateRequest categoryMappingsUpdateRequest = new CategoryMappingsUpdateRequest();
    BeanUtils.copyProperties(categoryMappingsUpdateServiceRequest, categoryMappingsUpdateRequest,
        "addedAttributes", "deletedAttributes", "addedKeywords", "deletedKeywords", "wholesaleMapping");
    categoryMappingsUpdateRequest.setAddedAttributes(
        categoryMappingsUpdateServiceRequest.getAddedAttributes().stream()
            .map(RequestHelper::toCategoryAttributeUpdateRequest).collect(Collectors.toList()));
    categoryMappingsUpdateRequest.setDeletedAttributes(
        categoryMappingsUpdateServiceRequest.getDeletedAttributes().stream()
            .map(RequestHelper::toCategoryAttributeUpdateRequest).collect(Collectors.toList()));
    categoryMappingsUpdateRequest.setAddedKeywords(categoryMappingsUpdateServiceRequest.getAddedKeywords().stream()
        .map(RequestHelper::toCategoryKeywordsUpdateRequest).collect(Collectors.toList()));
    categoryMappingsUpdateRequest.setDeletedKeywords(categoryMappingsUpdateServiceRequest.getDeletedKeywords().stream()
        .map(RequestHelper::toCategoryKeywordsUpdateRequest).collect(Collectors.toList()));
    WholesaleMappingRequest wholesaleMapping = new WholesaleMappingRequest();
    if(Objects.nonNull(categoryMappingsUpdateServiceRequest.getWholesaleMapping())){
      toWholeMappingRequest(categoryMappingsUpdateServiceRequest.getWholesaleMapping(),
          wholesaleMapping);
    }
    categoryMappingsUpdateRequest.setWholesaleMapping(wholesaleMapping);
    return categoryMappingsUpdateRequest;
  }

  public static void toWholeMappingRequest(
      WholesaleMappingServiceRequest wholesaleMappingServiceRequest,
      WholesaleMappingRequest wholesaleMappingRequest) {
    wholesaleMappingRequest.setConfigurationType(
        wholesaleMappingServiceRequest.getConfigurationType());
    for (WholesaleConfigServiceRequest wholesaleConfigServiceRequest :
        wholesaleMappingServiceRequest.getWholesaleConfig()) {
      wholesaleMappingRequest.getWholesaleConfig()
          .add(toWholesaleConfigRequest(wholesaleConfigServiceRequest));
    }
  }

  public static WholesaleConfigRequest toWholesaleConfigRequest(
      WholesaleConfigServiceRequest wholesaleConfigServiceRequest) {
    if(Objects.isNull(wholesaleConfigServiceRequest)){
      return null;
    }
    WholesaleConfigRequest wholesaleConfigRequest = new WholesaleConfigRequest();
    if (Objects.nonNull(wholesaleConfigServiceRequest.getQuantity())) {
      wholesaleConfigRequest.setQuantity(wholesaleConfigServiceRequest.getQuantity());
    }
    for (MinWholesaleDiscountServiceRequest minWholesaleDiscountServiceRequest :
        wholesaleConfigServiceRequest.getMinWholesaleDiscount()) {
      wholesaleConfigRequest.getMinWholesaleDiscount()
          .add(toMinWholesaleDiscountRequest(minWholesaleDiscountServiceRequest));
    }
    return wholesaleConfigRequest;
  }

  private static MinWholesaleDiscountRequest toMinWholesaleDiscountRequest(
      MinWholesaleDiscountServiceRequest minWholesaleDiscountServiceRequest) {
    if(Objects.isNull(minWholesaleDiscountServiceRequest)){
      return null;
    }
    return MinWholesaleDiscountRequest.builder()
        .percentage(minWholesaleDiscountServiceRequest.getPercentage())
        .price(minWholesaleDiscountServiceRequest.getPrice()).build();
  }

  private static CategoryAttributeUpdateRequest toCategoryAttributeUpdateRequest(
      CategoryAttributeUpdateServiceRequest categoryAttributeUpdateServiceRequest){
    return CategoryAttributeUpdateRequest.builder()
        .attributeId(categoryAttributeUpdateServiceRequest.getAttributeId())
        .mainDefiningAttribute(categoryAttributeUpdateServiceRequest.isMainDefiningAttribute())
        .sequence(categoryAttributeUpdateServiceRequest.getSequence())
        .usp(categoryAttributeUpdateServiceRequest.isUsp()).build();
  }

  private static CategoryKeywordsUpdateRequest toCategoryKeywordsUpdateRequest(
      CategoryKeywordsUpdateServiceRequest categoryKeywordsUpdateServiceRequest) {
    return CategoryKeywordsUpdateRequest.builder().keyword(categoryKeywordsUpdateServiceRequest.getKeyword())
        .keywordId(categoryKeywordsUpdateServiceRequest.getKeywordId())
        .action(categoryKeywordsUpdateServiceRequest.getAction()).type(categoryKeywordsUpdateServiceRequest.getType())
        .destinationCategory(categoryKeywordsUpdateServiceRequest.getDestinationCategory())
        .message(categoryKeywordsUpdateServiceRequest.getMessage())
        .validateByDs(categoryKeywordsUpdateServiceRequest.getValidateByDs()).build();
  }

  public static CategoryDetailRequest toCategoryDetailRequest(
      CategoryCreateServiceRequest categoryCreateServiceRequest) {
    CategoryDetailRequest categoryDetailRequest = CategoryDetailRequest.builder().catalogId(categoryCreateServiceRequest.getCatalogId())
        .categoryInfoDetail(toCategoryInfoUpdateRequest(
            categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest()))
        .categoryMappingsDetail(toCategoryMappingsUpdateRequest(
            categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest()))
        .build();
    BeanUtils.copyProperties(categoryCreateServiceRequest, categoryDetailRequest,
        "categoryMappingsDetail", "categoryInfoDetail");
    return categoryDetailRequest;
  }

  public static CategoryRestrictedKeywordsRequest toCategoryRestrictedKeywordsRequest(
      CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest){
    CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest = CategoryRestrictedKeywordsRequest.builder()
        .categoryCode(categoryRestrictedKeywordsWebRequest.getCategoryCode())
        .keyword(categoryRestrictedKeywordsWebRequest.getKeyword()).build();
    return categoryRestrictedKeywordsRequest;
  }

  public static void checkAutoQcConfigUpdateAccessibility(String[] accessibilities) throws Exception {
    if (!Arrays.asList(Credential.getAccessibilities()).contains(Constants.INTERNAL_AUTO_QC_CONFIG)) {
      throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
    }
  }

  public static AutoQcConfigRequest convertToAutoQcConfigRequest(
      AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest) throws Exception {
    AutoQcConfigRequest autoQcConfigRequest = new AutoQcConfigRequest();
    autoQcConfigRequest.setRuleEnabled(autoQcConfigUpdateWebRequest.getRuleEnabled());
    autoQcConfigRequest.setImageConfig(new ArrayList<>());
    for (AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDtoWeb :
        autoQcConfigUpdateWebRequest.getImageConfig()) {
      autoQcConfigRequest.getImageConfig()
          .add(toAutoApprovalRuleDetailsDto(autoApprovalRuleDetailsDtoWeb));
    }
    return autoQcConfigRequest;
  }

  public static com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto toAutoApprovalRuleDetailsDto(
      AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDtoWeb) {
    if(Objects.isNull(autoApprovalRuleDetailsDtoWeb)){
      return null;
    }
    return com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto.builder()
        .keyName(autoApprovalRuleDetailsDtoWeb.getKeyName())
        .operator(autoApprovalRuleDetailsDtoWeb.getOperator())
        .value(autoApprovalRuleDetailsDtoWeb.getValue())
        .valueType(autoApprovalRuleDetailsDtoWeb.getValueType()).build();
  }

  public static String validateDataForHtml(String request, boolean switchForHTMLValidation) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (!switchForHTMLValidation) {
      return request;
    }
    request = normalizeInput(request);
    if (request.contains(Constants.LT) || request.contains(Constants.GT) || request.contains(Constants.AMP)) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), customWhitelist());
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request = StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, customWhitelist()));
    }
    return request;
  }

  private static String normalizeInput(String input) {
    input = input.replaceAll(LEADING_LT, StringUtils.EMPTY).replaceAll(TRAILING_GT, StringUtils.EMPTY);
    return input;
  }

  private static Whitelist customWhitelist() {
    return Whitelist.relaxed().addTags(IFRAME)
        .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen");
  }

  public static ModifyDimensionMappingRequest convertToModifyDimensionMappingRequest(
      ModifyDimensionMappingWebRequest modifyDimensionMappingWebRequest) {
    ModifyDimensionMappingRequest dimensionMappingRequest = new ModifyDimensionMappingRequest();
    dimensionMappingRequest.setAddedDimensionMapping(
        convertToDimensionMappingDTO(modifyDimensionMappingWebRequest.getAddedDimensionMapping()));
    dimensionMappingRequest.setUpdateDimensionMapping(
        convertToDimensionMappingDTO(modifyDimensionMappingWebRequest.getUpdateDimensionMapping()));
    dimensionMappingRequest.setDeletedDimensionMapping(convertToDimensionMappingDTO(
        modifyDimensionMappingWebRequest.getDeletedDimensionMapping()));
    return dimensionMappingRequest;
  }

  public static List<DimensionMappingDTO> convertToDimensionMappingDTO(List<com.gdn.partners.pcu.master.web.model.request.DimensionMappingDTO> dimensionMappingDTOsWeb) {
    List<DimensionMappingDTO> dimensionMappingDTOS = new ArrayList<>();
    for(com.gdn.partners.pcu.master.web.model.request.DimensionMappingDTO dimensionMappingDTOWeb: dimensionMappingDTOsWeb ){
      DimensionMappingDTO dimensionMappingDTO = new DimensionMappingDTO();
      BeanUtils.copyProperties(dimensionMappingDTOWeb, dimensionMappingDTO);
      dimensionMappingDTOS.add(dimensionMappingDTO);
    }
    return dimensionMappingDTOS;
  }

  public static void checkArgument(boolean expression, String errorMessage) {
    try {
      Preconditions.checkArgument(expression, errorMessage);
    } catch (IllegalArgumentException var3) {
      throw new ValidationException(errorMessage);
    }
  }

  public static void checkArgumentsOfDimension(String nameEnglish, byte[] description,
      byte[] descriptionEnglish, String example) {
    RequestHelper.checkArgument(StringUtils.isNotBlank(nameEnglish),
        ErrorMessages.DIMENSION_NAME_ENGLISH_SHOULD_NOT_BE_BLANK);
    RequestHelper.checkArgument(Objects.nonNull(description),
        ErrorMessages.DIMENSION_DESCRIPTION_SHOULD_NOT_BE_BLANK);
    RequestHelper.checkArgument(Objects.nonNull(descriptionEnglish),
        ErrorMessages.DIMENSION_DESCRIPTION_ENGLISH_SHOULD_NOT_BE_BLANK);
    RequestHelper.checkArgument(StringUtils.isNotBlank(example),
        ErrorMessages.DIMENSION_EXAMPLE_SHOULD_NOT_BE_BLANK);
  }

  public static void validateAccessibilityForProductTab(boolean validateProductAccessibility,
      List<String> accessibility, boolean isExternal, String productAccessibilityList) {
    if (validateProductAccessibility && isExternal && StringUtils.isNotBlank(productAccessibilityList)) {
      String[] productAccessibility = productAccessibilityList.split(Constants.COMMA_DELIMITER_NO_SPACE);
      Arrays.stream(productAccessibility).filter(access -> !accessibility.contains(access)).forEach(access -> {
        throw new UnauthorizedException(ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
      });
    }
  }
}
