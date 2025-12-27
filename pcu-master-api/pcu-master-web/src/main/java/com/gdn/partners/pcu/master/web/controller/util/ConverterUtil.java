package com.gdn.partners.pcu.master.web.controller.util;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.master.model.request.MinWholesaleDiscountServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleConfigServiceRequest;
import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.web.model.request.DimensionMappingDTO;
import com.gdn.partners.pcu.master.web.model.request.MinWholesaleDiscountWebRequest;
import com.gdn.partners.pcu.master.web.model.request.WholesaleConfigWebRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.MinWholesaleDiscountRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleConfigRequest;

import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.attribute.AttributeResponse;
import com.gdn.partners.pcu.master.model.attribute.AttributeValue;
import com.gdn.partners.pcu.master.model.attribute.AttributeValueUpdateModel;
import com.gdn.partners.pcu.master.model.request.AttributeValueAddServiceRequest;
import com.gdn.partners.pcu.master.model.request.AttributeValuesUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryAttributeUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryHierarchyServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryKeywordsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleMappingServiceRequest;
import com.gdn.partners.pcu.master.web.model.AttributeValueUpdateWebModel;
import com.gdn.partners.pcu.master.web.model.request.AllowedAttributeValueDTO;
import com.gdn.partners.pcu.master.web.model.request.AttributeValueAddWebRequest;
import com.gdn.partners.pcu.master.web.model.request.AttributeValuesUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryAttributeUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryCreateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryInfoUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryKeywordUpdateWebRequestList;
import com.gdn.partners.pcu.master.web.model.request.CategoryKeywordsUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryMappingsUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryStatusChangeWebRequest;
import com.gdn.partners.pcu.master.web.model.request.MasterAttributeDTO;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.PredefinedAllowedAttributeValueDTO;
import com.gdn.partners.pcu.master.web.model.request.WholesaleMappingWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AttributeMasterResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeSortType;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.client.model.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;

public class ConverterUtil {

  public static AttributeMasterResponse convertMasterAttributeResponseToAttributeResponse(
      MasterAttributeResponse masterAttributeResponse) {
    AttributeMasterResponse attributeMasterResponse = new AttributeMasterResponse();
    BeanUtils.copyProperties(masterAttributeResponse, attributeMasterResponse, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", "attributeType", "sortType");
    setSortType(attributeMasterResponse, masterAttributeResponse);
    return attributeMasterResponse;
  }

  private static AttributeMasterResponse setSortType(AttributeMasterResponse attributeMasterResponse,
      MasterAttributeResponse request) {
    if (Objects.nonNull(request.getSortType())) {
      attributeMasterResponse.setSortType(AttributeSortType.valueOf(request.getSortType().name()));
    }
    if(Objects.nonNull(request.getAttributeType())) {
      attributeMasterResponse.setAttributeType(request.getAttributeType());
    }
    return attributeMasterResponse;
  }

  public static List<AttributeValueWebResponse> toAttributeValueWebResponses(List<AttributeValue> attributeValues){
    List<AttributeValueWebResponse> attributeValueWebResponse = new ArrayList<>();
    for (AttributeValue attributeValue : attributeValues) {
      attributeValueWebResponse.add(toAttributeValueWebResponse(attributeValue));
    }
    return attributeValueWebResponse;
  }

  public static AttributeValueWebResponse toAttributeValueWebResponse(AttributeValue attributeValue) {
    AttributeValueWebResponse attributeValueWebResponse = AttributeValueWebResponse.builder().build();
    BeanUtils.copyProperties(attributeValue, attributeValueWebResponse);
    return attributeValueWebResponse;
  }

  public static List<AttributeResponse> toMasterAttributeResponses(
      List<MasterAttributeResponse> masterAttributeResponses) {
    List<AttributeResponse> attributeResponses = new ArrayList<>();
    for (MasterAttributeResponse masterAttributeResponse : masterAttributeResponses) {
      attributeResponses.add(toAttributeResponse(masterAttributeResponse));
    }
    return attributeResponses;
  }

  public static AttributeResponse toAttributeResponse(
      MasterAttributeResponse masterAttributeResponse) {
    AttributeResponse attributeResponse = AttributeResponse.builder().build();
    BeanUtils.copyProperties(masterAttributeResponse, attributeResponse);
    attributeResponse.setEnglishName(masterAttributeResponse.getNameEnglish());
    attributeResponse.setEnglishDescription(masterAttributeResponse.getDescriptionEnglish());
    return attributeResponse;
  }

  public static AttributeValueWebResponse toAttributeValueWebResponse(AttributeValueResponse attributeValueResponse) {
    AttributeValueWebResponse attributeValueWebResponse =
        AttributeValueWebResponse.builder().id(attributeValueResponse.getId()).value(attributeValueResponse.getValue())
            .allowedAttributeCode(attributeValueResponse.getAllowedAttributeCode())
            .predefinedAllowedAttributeCode(attributeValueResponse.getPredefinedAllowedAttributeCode()).build();
    return attributeValueWebResponse;
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

  private static void setAllowedAttribute(MasterAttributeRequest attribute, MasterAttributeDTO
      request, String storeId) {
    for (AllowedAttributeValueDTO allowedAttributeValueRequest : request
        .getAllowedAttributeValues()) {
      AllowedAttributeValueRequest allowedAttributeValue = new AllowedAttributeValueRequest();
      BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
      if (org.springframework.util.StringUtils.isEmpty(allowedAttributeValue.getStoreId())) {
        allowedAttributeValue.setStoreId(storeId);
      }
      attribute.getAllowedAttributeValues().add(allowedAttributeValue);
    }
  }

  private static void setPredefinedAllowedAttribtue(MasterAttributeRequest attribute,
      MasterAttributeDTO request, String storeId) {
    for (PredefinedAllowedAttributeValueDTO predefinedAllowedAttributeValueRequest : request
        .getPredefinedAllowedAttributeValues()) {
      PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValue =
          new PredefinedAllowedAttributeValueRequest();
      BeanUtils
          .copyProperties(predefinedAllowedAttributeValueRequest, predefinedAllowedAttributeValue);
      if (org.springframework.util.StringUtils.isEmpty(predefinedAllowedAttributeValue.getStoreId())) {
        predefinedAllowedAttributeValue.setStoreId(storeId);
      }
      attribute.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    }
  }

  private static void setDimensionMapping(MasterAttributeRequest attribute, MasterAttributeDTO request) {
    for(DimensionMappingDTO dimensionMappingDTO : request.getDimensionMapping()) {
      DimensionMappingRequest dimensionMappingRequest = new DimensionMappingRequest();
      BeanUtils.copyProperties(dimensionMappingDTO, dimensionMappingRequest);
      attribute.getDimensionMapping().add(dimensionMappingRequest);
    }
  }

  public static MasterAttributeRequest convertMasterAttributeRequestToAttribute(MasterAttributeDTO
      request, String storeId) {
    MasterAttributeRequest attribute = new MasterAttributeRequest();
    BeanUtils.copyProperties(request, attribute, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", "attributeType", "sortType", "dimensionMapping");
    setAttributeTypeAndSortType(attribute, request);
    setAllowedAttribute(attribute, request, storeId);
    setPredefinedAllowedAttribtue(attribute, request, storeId);
    setDimensionMapping(attribute, request);
    if (org.springframework.util.StringUtils.isEmpty(attribute.getStoreId())) {
      attribute.setStoreId(storeId);
    }
    return attribute;
  }

  public static AttributeValuesUpdateServiceRequest toAttributeValuesUpdateServiceRequest(
      AttributeValuesUpdateWebRequest attributeValuesUpdateWebRequest) {
    AttributeValuesUpdateServiceRequest attributeValuesUpdateServiceRequest =
        AttributeValuesUpdateServiceRequest.builder()
            .sortType(attributeValuesUpdateWebRequest.getSortType())
            .updatedBy(attributeValuesUpdateWebRequest.getUpdatedBy())
            .updatedDate(attributeValuesUpdateWebRequest.getUpdatedDate())
            .attributeValues(new ArrayList<>())
            .addedAttributeValues(new ArrayList<>())
            .deletedAttributeValues(new ArrayList<>())
            .build();
    attributeValuesUpdateWebRequest.getAttributeValues().stream()
        .map(ConverterUtil::toAttributeValueUpdateModel)
        .collect(Collectors.toCollection(attributeValuesUpdateServiceRequest::getAttributeValues));
    attributeValuesUpdateWebRequest.getAddedAttributeValues().stream()
        .map(ConverterUtil::toAttributeValueUpdateModel)
        .collect(Collectors.toCollection(attributeValuesUpdateServiceRequest::getAddedAttributeValues));
    attributeValuesUpdateWebRequest.getDeletedAttributeValues().stream()
        .map(ConverterUtil::toAttributeValueUpdateModel)
        .collect(Collectors.toCollection(attributeValuesUpdateServiceRequest::getDeletedAttributeValues));
    return attributeValuesUpdateServiceRequest;
  }

  public static AttributeValueAddServiceRequest toAttributeValueAddServiceRequest(
      AttributeValueAddWebRequest attributeValueAddWebRequest, ClientParameterHelper clientParameterHelper) {
    AttributeValueAddServiceRequest attributeValueAddServiceRequest =
        AttributeValueAddServiceRequest.builder().value(attributeValueAddWebRequest.getValue())
            .sequence(attributeValueAddWebRequest.getSequence()).createdDate(new Date())
            .createdBy(clientParameterHelper.getUsername()).build();
    return attributeValueAddServiceRequest;
  }

  private static AttributeValueUpdateModel toAttributeValueUpdateModel(
      AttributeValueUpdateWebModel attributeValueUpdateWebModel){
    return AttributeValueUpdateModel.builder().id(attributeValueUpdateWebModel.getId())
        .allowedAttributeCode(attributeValueUpdateWebModel.getAllowedAttributeCode())
        .sequence(attributeValueUpdateWebModel.getSequence())
        .value(attributeValueUpdateWebModel.getValue())
        .valueEn(attributeValueUpdateWebModel.getValueEn())
        .valueType(attributeValueUpdateWebModel.getValueType())
        .build();
  }


  public static GetSubCategoriesServiceRequest getCategorySummaryRequest(String catalogType, String catalogId,
      String parentId, boolean hideNonInventory, String filterType, String documentFilterType,
      boolean ignoreB2bExclusive, boolean filterHalalCategory) {
    return GetSubCategoriesServiceRequest.builder().catalogType(catalogType).catalogId(catalogId).parentId(parentId)
        .hideNonInventory(hideNonInventory).filterType(filterType).documentFilterType(documentFilterType)
        .ignoreB2bExclusive(ignoreB2bExclusive).filterHalalCategory(filterHalalCategory).build();
  }

  /**
   * Remove duplicate list entry<br/>
   * Eg:<br/>
   * <ol>
   * <li>[A,B]</li>
   * <li>[A,B,C,D]</li>
   * <li>[A,D]</li>
   * </ol>
   * Remove entry no. 1 because it's been covered by entry no.2
   *
   * @param listOfIds
   */
  public static List<List<CategorySearchWebResponse>> removeDuplicateListEntry(List<List<CategorySearchWebResponse>> listOfIds) {
    List<List<CategorySearchWebResponse>> result = new ArrayList<>();
    List<Integer> duplicateIndexes = new ArrayList<>();
    for (int x = 0; x < listOfIds.size(); x++) {
      List<CategorySearchWebResponse> ids = listOfIds.get(x);
      for (int y = 0; y < listOfIds.size(); y++) {
        if (x != y) {
          List<CategorySearchWebResponse> currIds = listOfIds.get(y);
          if (currIds.size() > ids.size()) {
            List<CategorySearchWebResponse> subListOfCurrIds = currIds.subList(0, ids.size());
            if (ids.equals(subListOfCurrIds)) {
              duplicateIndexes.add(x);
              break;
            }
          }
        }
      }
      if (duplicateIndexes.indexOf(x) < 0) {
        result.add(listOfIds.get(x));
      }
    }
    return result;
  }

  public static CategoryHierarchyServiceRequest toCategoryHierarchyServiceRequest(String categoryName, String catalogId,
      String filterType, String documentFilterType) {
    return CategoryHierarchyServiceRequest.builder().categoryName(categoryName).catalogId(catalogId)
        .filterType(filterType).documentFilterType(documentFilterType).build();
  }

  public static CategoryInfoUpdateServiceRequest toCategoryInfoUpdateServiceRequest(String categoryCode,
      CategoryInfoUpdateWebRequest categoryInfoUpdateWebRequest, ClientParameterHelper clientParameterHelper,
      boolean switchForHTMLValidation) {
    CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest = new CategoryInfoUpdateServiceRequest();
    BeanUtils.copyProperties(categoryInfoUpdateWebRequest, categoryInfoUpdateServiceRequest);
    categoryInfoUpdateServiceRequest.setName(
        RequestHelper.validateDataForHtml(categoryInfoUpdateWebRequest.getName(),
            switchForHTMLValidation));
    categoryInfoUpdateServiceRequest.setNameEnglish(
        RequestHelper.validateDataForHtml(categoryInfoUpdateWebRequest.getNameEnglish(),
            switchForHTMLValidation));
    categoryInfoUpdateServiceRequest.setDefaultDescription(
        Objects.nonNull(categoryInfoUpdateWebRequest.getDefaultDescription()) ?
            RequestHelper.validateDataForHtml(
                new String(categoryInfoUpdateWebRequest.getDefaultDescription()),
                switchForHTMLValidation).getBytes(StandardCharsets.UTF_8) :
            null);
    categoryInfoUpdateServiceRequest.setDescriptionEnglish(
        Objects.nonNull(categoryInfoUpdateWebRequest.getDescriptionEnglish()) ?
            RequestHelper.validateDataForHtml(
                new String(categoryInfoUpdateWebRequest.getDescriptionEnglish()),
                switchForHTMLValidation).getBytes(StandardCharsets.UTF_8) :
            null);
    categoryInfoUpdateServiceRequest.setUpdatedBy(clientParameterHelper.getUsername());
    categoryInfoUpdateServiceRequest.setUpdatedDate(new Date());
    categoryInfoUpdateServiceRequest.setStoreId(clientParameterHelper.getStoreId());
    return categoryInfoUpdateServiceRequest;
  }

  public static CategoryStatusChangeServiceRequest toCategoryStatusChangeServiceRequest(
      CategoryStatusChangeWebRequest categoryStatusChangeWebRequest, String categoryCode,
      ClientParameterHelper clientParameterHelper) {
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = new CategoryStatusChangeServiceRequest();
    BeanUtils.copyProperties(categoryStatusChangeWebRequest, categoryStatusChangeServiceRequest);
    categoryStatusChangeServiceRequest.setCategoryCode(categoryCode);
    categoryStatusChangeServiceRequest.setUpdatedBy(clientParameterHelper.getUsername());
    categoryStatusChangeServiceRequest.setUpdatedDate(new Date());
    categoryStatusChangeServiceRequest.setStoreId(clientParameterHelper.getStoreId());
    return categoryStatusChangeServiceRequest;
  }

  public static CategoryMappingsUpdateServiceRequest toCategoryMappingsUpdateServiceRequest(String categoryId,
      CategoryMappingsUpdateWebRequest categoryMappingsUpdateWebRequest, ClientParameterHelper clientParameterHelper) {
    CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest =
        CategoryMappingsUpdateServiceRequest.builder().id(categoryId).updatedDate(new Date())
            .updatedBy(clientParameterHelper.getUsername())
            .storeId(clientParameterHelper.getStoreId()).build();
    categoryMappingsUpdateServiceRequest.setAddedAttributes(
        categoryMappingsUpdateWebRequest.getAddedAttributes().stream()
            .map(ConverterUtil::toCategoryAttributeUpdateServiceRequest).collect(Collectors.toList()));
    categoryMappingsUpdateServiceRequest.setDeletedAttributes(
        categoryMappingsUpdateWebRequest.getDeletedAttributes().stream()
            .map(ConverterUtil::toCategoryAttributeUpdateServiceRequest).collect(Collectors.toList()));
    categoryMappingsUpdateServiceRequest.setAddedKeywords(
        categoryMappingsUpdateWebRequest.getAddedKeywords().stream()
            .map(ConverterUtil::toCategoryKeywordsUpdateServiceRequest).collect(Collectors.toList()));
    categoryMappingsUpdateServiceRequest.setDeletedKeywords(
        categoryMappingsUpdateWebRequest.getDeletedKeywords().stream()
            .map(ConverterUtil::toCategoryKeywordsUpdateServiceRequest).collect(Collectors.toList()));
    categoryMappingsUpdateServiceRequest.setAddedMasterCategoryIds(
        categoryMappingsUpdateWebRequest.getAddedMasterCategoryIds());
    categoryMappingsUpdateServiceRequest.setDeletedMasterCategoryIds(
        categoryMappingsUpdateWebRequest.getDeletedMasterCategoryIds());
    WholesaleMappingServiceRequest wholesaleMappingServiceRequest = new WholesaleMappingServiceRequest();
    if(Objects.nonNull(categoryMappingsUpdateWebRequest.getWholesaleMapping())){
      toWholesaleMappingServiceRequest(wholesaleMappingServiceRequest,
          categoryMappingsUpdateWebRequest.getWholesaleMapping());
    }
    categoryMappingsUpdateServiceRequest.setWholesaleMapping(wholesaleMappingServiceRequest);
    return categoryMappingsUpdateServiceRequest;
  }

  private static void toWholesaleMappingServiceRequest(
      WholesaleMappingServiceRequest wholesaleMappingServiceRequest,
      WholesaleMappingWebRequest wholesaleMappingWebRequest) {
    wholesaleMappingServiceRequest.setConfigurationType(
        wholesaleMappingWebRequest.getConfigurationType());
    wholesaleMappingServiceRequest.setWholesaleConfig(
        wholesaleMappingWebRequest.getWholesaleConfig().stream()
            .map(ConverterUtil::toWholesaleConfigServiceRequest).collect(Collectors.toList()));
  }

  public static WholesaleConfigServiceRequest toWholesaleConfigServiceRequest(
      WholesaleConfigWebRequest wholesaleConfigWebRequest) {
    if(Objects.isNull(wholesaleConfigWebRequest)){
      return null;
    }
    return WholesaleConfigServiceRequest.builder().quantity(wholesaleConfigWebRequest.getQuantity())
        .minWholesaleDiscount(wholesaleConfigWebRequest.getMinWholesaleDiscount().stream()
            .map(ConverterUtil::toMinWholesaleDiscountServiceRequest).collect(Collectors.toList()))
        .build();
  }

  public static MinWholesaleDiscountServiceRequest toMinWholesaleDiscountServiceRequest(
      MinWholesaleDiscountWebRequest minWholesaleDiscountWebRequest) {
    if(Objects.isNull(minWholesaleDiscountWebRequest)){
      return null;
    }
    return MinWholesaleDiscountServiceRequest.builder()
        .percentage(minWholesaleDiscountWebRequest.getPercentage())
        .price(minWholesaleDiscountWebRequest.getPrice()).build();
  }

  private static CategoryAttributeUpdateServiceRequest toCategoryAttributeUpdateServiceRequest(
      CategoryAttributeUpdateWebRequest categoryAttributeUpdateWebRequest){
    return CategoryAttributeUpdateServiceRequest.builder()
        .attributeId(categoryAttributeUpdateWebRequest.getAttributeId())
        .mainDefiningAttribute(categoryAttributeUpdateWebRequest.isMainDefiningAttribute())
        .sequence(categoryAttributeUpdateWebRequest.getSequence())
        .usp(categoryAttributeUpdateWebRequest.isUsp()).build();
  }

  public static CategoryCreateServiceRequest toCategoryCreateServiceRequest(
      CategoryCreateWebRequest categoryCreateWebRequest, ClientParameterHelper clientParameterHelper, boolean switchForHTMLValidation) {

    return CategoryCreateServiceRequest.builder()
        .storeId(clientParameterHelper.getStoreId())
        .catalogId(categoryCreateWebRequest.getCatalogId()).categoryInfoUpdateServiceRequest(
            toCategoryInfoUpdateServiceRequest(null,
                categoryCreateWebRequest.getCategoryInfoDetail(), clientParameterHelper, switchForHTMLValidation))
        .categoryMappingsUpdateServiceRequest(toCategoryMappingsUpdateServiceRequest(
            null, categoryCreateWebRequest.getCategoryMappingsDetail(), clientParameterHelper))
        .createdDate(new Date())
        .createdBy(clientParameterHelper.getUsername()).build();
  }

  private static CategoryKeywordsUpdateServiceRequest toCategoryKeywordsUpdateServiceRequest(
      CategoryKeywordsUpdateWebRequest categoryKeywordsUpdateWebRequest){
    return CategoryKeywordsUpdateServiceRequest.builder().keyword(categoryKeywordsUpdateWebRequest.getKeyword())
        .keywordId(categoryKeywordsUpdateWebRequest.getKeywordId()).action(categoryKeywordsUpdateWebRequest.getAction())
        .destinationCategory(categoryKeywordsUpdateWebRequest.getDestinationCategory())
        .type(categoryKeywordsUpdateWebRequest.getType()).message(categoryKeywordsUpdateWebRequest.getMessage())
        .validateByDs(categoryKeywordsUpdateWebRequest.getValidateByDs())
        .build();
  }

  public static CategoryKeywordUpdateRequestList toCategoryKeywordRequestList(
      CategoryKeywordUpdateWebRequestList categoryKeywordUpdateWebRequestList,
      ClientParameterHelper clientParameterHelper) {
    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList = new CategoryKeywordUpdateRequestList();
    categoryKeywordUpdateRequestList.setStoreId(clientParameterHelper.getStoreId());
    categoryKeywordUpdateRequestList.setCreatedBy(clientParameterHelper.getUsername());
    categoryKeywordUpdateRequestList.setCreatedDate(new Date());
    categoryKeywordUpdateRequestList.setUpdatedBy(clientParameterHelper.getUsername());
    categoryKeywordUpdateRequestList.setUpdatedDate(new Date());
    categoryKeywordUpdateRequestList.setAddedKeywords(
        Optional.ofNullable(categoryKeywordUpdateWebRequestList.getAddedKeywords()).orElseGet(Collections::emptyList)
            .stream().map(ConverterUtil::toCategoryKeywordsUpdateRequest).collect(Collectors.toList()));
    categoryKeywordUpdateRequestList.setDeletedKeywords(
        Optional.ofNullable(categoryKeywordUpdateWebRequestList.getDeletedKeywords()).orElseGet(Collections::emptyList)
            .stream().map(ConverterUtil::toCategoryKeywordsUpdateRequest).collect(Collectors.toList()));
    return categoryKeywordUpdateRequestList;
  }

  private static CategoryKeywordsUpdateRequest toCategoryKeywordsUpdateRequest(
      CategoryKeywordsUpdateWebRequest categoryKeywordsUpdateWebRequest) {
    return CategoryKeywordsUpdateRequest.builder().keywordId(categoryKeywordsUpdateWebRequest.getKeywordId())
        .keyword(categoryKeywordsUpdateWebRequest.getKeyword()).action(categoryKeywordsUpdateWebRequest.getAction())
        .destinationCategory(categoryKeywordsUpdateWebRequest.getDestinationCategory())
        .type(categoryKeywordsUpdateWebRequest.getType()).message(categoryKeywordsUpdateWebRequest.getMessage())
        .validateByDs(categoryKeywordsUpdateWebRequest.getValidateByDs()).build();
  }

  public static WholesaleMappingRequest toCategoryWholesaleConfig(WholesaleMappingWebRequest wholesaleMappingWebRequest) {
    WholesaleMappingRequest wholesaleMappingRequest = new WholesaleMappingRequest();
    toWholeMappingRequest(wholesaleMappingWebRequest, wholesaleMappingRequest);
    return wholesaleMappingRequest;
  }

  private static void toWholeMappingRequest(WholesaleMappingWebRequest wholesaleMappingWebRequest,
      WholesaleMappingRequest wholesaleMappingRequest) {
    wholesaleMappingRequest.setConfigurationType(wholesaleMappingWebRequest.getConfigurationType());
    wholesaleMappingRequest.setWholesaleConfig(
        wholesaleMappingWebRequest.getWholesaleConfig().stream()
            .map(ConverterUtil::toWholesaleConfigRequest).collect(Collectors.toList()));
  }

  private static WholesaleConfigRequest toWholesaleConfigRequest(
      WholesaleConfigWebRequest wholesaleConfigWebRequest) {
    if (Objects.isNull(wholesaleConfigWebRequest)) {
      return null;
    }
    return WholesaleConfigRequest.builder().quantity(wholesaleConfigWebRequest.getQuantity())
        .minWholesaleDiscount(wholesaleConfigWebRequest.getMinWholesaleDiscount().stream()
            .map(ConverterUtil::toMinWholesaleDiscountRequest).collect(Collectors.toList()))
        .build();
  }

  private static MinWholesaleDiscountRequest toMinWholesaleDiscountRequest(
      MinWholesaleDiscountWebRequest minWholesaleDiscountWebRequest) {
    if (Objects.isNull(minWholesaleDiscountWebRequest)) {
      return null;
    }
    return MinWholesaleDiscountRequest.builder().price(minWholesaleDiscountWebRequest.getPrice())
        .percentage(minWholesaleDiscountWebRequest.getPercentage()).build();
  }

  public static OscInfoUpdateDTO toOscInfoUpdateDTO(OriginalSalesCategoryUpdateWebRequest request) {
    OscInfoUpdateDTO oscInfoUpdateDTO = new OscInfoUpdateDTO();
    BeanUtils.copyProperties(request, oscInfoUpdateDTO);
    return oscInfoUpdateDTO;
  }
}
