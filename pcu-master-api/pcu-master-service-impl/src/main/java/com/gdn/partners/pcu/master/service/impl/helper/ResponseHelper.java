package com.gdn.partners.pcu.master.service.impl.helper;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryAttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import com.gdn.partners.pcu.master.client.model.AllowedAttributeValueResponse;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.master.web.model.response.MinWholesaleDiscountWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleConfigWebResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import org.apache.commons.collections.CollectionUtils;

import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.master.client.model.BaseMarginGroup;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategorySuggestionResponse;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.MarginFactor;
import com.gdn.partners.pcu.master.client.model.Margin;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.web.model.request.ProfitMarginWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AttributeDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValueWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AttributeValuesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRuleDetailWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.BaseMarginHierarchyWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CatalogTreeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryAttributeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryInfoWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryMappingResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryReferenceWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySuggestionWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryTreeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CreateCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginDetailsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MasterCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OriginalSalesCategoryResponse;
import com.gdn.partners.pcu.master.web.model.response.OscDetailsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscSummaryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.ProfitMarginWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.ShippingWebResponse;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;


/**
 * Created by govind on 01/11/2018 AD.
 */
public class ResponseHelper {

  public static final String ORDER_ITEM_ID = "-";
  public static final String PERCENTAGE = "PERCENTAGE";

  public static boolean validateResponse(GdnRestListResponse clientResponse) {

    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getContent())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess()) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static boolean validateResponse(ListBaseResponse clientResponse) {

    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getContent())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }


  public static List<CategoryWebResponse> toCategoryDetailResponseList(List<CategoryDTO> categoryDTOS) {
    return categoryDTOS.stream().map(ResponseHelper::toCategoryDetailResponse).collect(Collectors.toList());
  }

  private static CategoryWebResponse toCategoryDetailResponse(CategoryDTO categoryDTO) {
    CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
    BeanUtils.copyProperties(categoryDTO, categoryWebResponse);
    return categoryWebResponse;
  }

  public static List<CatalogDetailResponse> toCatalogDetailResponseList(List<CatalogResponse> catalogResponses) {
    List<CatalogDetailResponse> catalogDetailResponses = new ArrayList<>();
    for (CatalogResponse catalogResponse : catalogResponses) {
      catalogDetailResponses.add(toCatalogDetailResponse(catalogResponse));
    }
    return catalogDetailResponses;
  }

  private static CatalogDetailResponse toCatalogDetailResponse(CatalogResponse catalogResponse) {
    CatalogDetailResponse catalogDetailResponse = new CatalogDetailResponse();
    BeanUtils.copyProperties(catalogResponse, catalogDetailResponse);
    return catalogDetailResponse;
  }

  public static boolean validateCatalogType(String catalogType) {
    if (Arrays.stream(CatalogType.values()).map(Enum::toString).collect(Collectors.toList()).contains(catalogType)) {
      return true;
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, ErrorMessages.ERR_INVALID_CATALOG_TYPE);
    }
  }

  /**
   * Convert into category info web response
   *
   * @param categoryDetailAndShippingResponse
   * @return
   */
  public static CategoryInfoWebResponse toCategoryInfoWebResponse(
      CategoryDetailAndShippingResponse categoryDetailAndShippingResponse) {
    CategoryInfoWebResponse response = new CategoryInfoWebResponse();
    BeanUtils.copyProperties(categoryDetailAndShippingResponse, response, "categoryAttributes", "productCategories",
        "masterCategoryReferences", "salesCategoryReferences");
    CatalogDetailResponse catalog = new CatalogDetailResponse();
    if (Objects.nonNull(categoryDetailAndShippingResponse.getCatalog())) {
      BeanUtils.copyProperties(categoryDetailAndShippingResponse.getCatalog(), catalog);
    }
    response.setCatalog(catalog);

    if (Objects.nonNull(categoryDetailAndShippingResponse.getParentCategoryId())) {
      response.setParentCategoryId(categoryDetailAndShippingResponse.getParentCategoryId());
    }

    OriginalSalesCategoryResponse originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
    if (Objects.nonNull(categoryDetailAndShippingResponse.getOriginalSalesCategoryResponse())) {
      BeanUtils.copyProperties(categoryDetailAndShippingResponse.getOriginalSalesCategoryResponse(), originalSalesCategoryResponse);
    }
    response.setOriginalSalesCategory(originalSalesCategoryResponse);

    setCategoryAttributeDetails(categoryDetailAndShippingResponse, response);
    setCategoryReferencesWebResponse(categoryDetailAndShippingResponse, response);
    setShippingWebResponse(categoryDetailAndShippingResponse, response);
    return response;
  }

  /**
   * Set shipping responses in category info web response
   *
   * @param categoryDetailAndShippingResponse
   * @param response
   */
  private static void setShippingWebResponse(CategoryDetailAndShippingResponse categoryDetailAndShippingResponse,
      CategoryInfoWebResponse response) {
    if (CollectionUtils.isNotEmpty(categoryDetailAndShippingResponse.getShippingResponses())) {
      List<ShippingWebResponse> shippingWebResponses = new ArrayList<>();
      for (ShippingResponse shippingResponse : categoryDetailAndShippingResponse.getShippingResponses()) {
        ShippingWebResponse shippingWebResponse = new ShippingWebResponse();
        BeanUtils.copyProperties(shippingResponse, shippingWebResponse);
        shippingWebResponses.add(shippingWebResponse);
      }
      response.setShippingResponses(shippingWebResponses);
    }
  }

  /**
   * Set master / sales category references in category info web response
   *
   * @param categoryDetailAndShippingResponse
   * @param response
   */
  private static void setCategoryReferencesWebResponse(
      CategoryDetailAndShippingResponse categoryDetailAndShippingResponse, CategoryInfoWebResponse response) {
    if (CollectionUtils.isNotEmpty(categoryDetailAndShippingResponse.getMasterCategoryReferences())) {
      List<CategoryReferenceWebResponse> categoryReferenceWebResponses = new ArrayList<>();
      for (CategoryReferenceResponse categoryReferenceResponse : categoryDetailAndShippingResponse
          .getMasterCategoryReferences()) {
        if (!categoryReferenceResponse.isMarkForDelete() && !categoryReferenceResponse.getMasterCategoryReference()
            .isMarkForDelete()) {
          CategoryReferenceWebResponse categoryReferenceWebResponse = new CategoryReferenceWebResponse();
          BeanUtils.copyProperties(categoryReferenceResponse, categoryReferenceWebResponse);
          categoryReferenceWebResponse.setMasterCategoryResponse(getCategoryWebResponse(categoryReferenceResponse.getMasterCategoryReference()));
          categoryReferenceWebResponses.add(categoryReferenceWebResponse);
        }
      }
      response.setMasterCategoryReference(categoryReferenceWebResponses);
    }

    if (CollectionUtils.isNotEmpty(categoryDetailAndShippingResponse.getSalesCategoryReferences())) {
      List<CategoryReferenceWebResponse> categoryReferenceWebResponses = new ArrayList<>();
      for (CategoryReferenceResponse categoryReferenceResponse : categoryDetailAndShippingResponse
          .getSalesCategoryReferences()) {
        if (!categoryReferenceResponse.isMarkForDelete() && !categoryReferenceResponse.getSalesCategoryReference()
            .isMarkForDelete()) {
          CategoryReferenceWebResponse categoryReferenceWebResponse = new CategoryReferenceWebResponse();
          BeanUtils.copyProperties(categoryReferenceResponse, categoryReferenceWebResponse);
          categoryReferenceWebResponse.setSalesCategoryResponse(getCategoryWebResponse(categoryReferenceResponse.getSalesCategoryReference()));
          categoryReferenceWebResponses.add(categoryReferenceWebResponse);
        }
      }
      response.setSalesCategoryReference(categoryReferenceWebResponses);
    }
  }

  private static CategoryWebResponse getCategoryWebResponse(CategoryResponse categoryResponse) {
    CategoryWebResponse categoryWebResponse = new CategoryWebResponse();
    if (Objects.nonNull(categoryResponse)) {
      BeanUtils.copyProperties(categoryResponse, categoryWebResponse);
    }
    return categoryWebResponse;
  }

  /**
   * Set attribute responses in category info web response
   *
   * @param categoryDetailAndShippingResponse
   * @param response
   */
  private static void setCategoryAttributeDetails(CategoryDetailAndShippingResponse categoryDetailAndShippingResponse,
      CategoryInfoWebResponse response) {
    if (CollectionUtils.isNotEmpty(categoryDetailAndShippingResponse.getCategoryAttributes())) {
      List<CategoryAttributeWebResponse> categoryAttributeWebResponses = new ArrayList<>();
      for (CategoryAttributeResponse categoryAttributeResponse : categoryDetailAndShippingResponse
          .getCategoryAttributes()) {
        if (!categoryAttributeResponse.isMarkForDelete()) {
          CategoryAttributeWebResponse categoryAttributeWebResponse = new CategoryAttributeWebResponse();
          AttributeResponse attributeResponse = categoryAttributeResponse.getAttribute();
          BeanUtils.copyProperties(categoryAttributeResponse, categoryAttributeWebResponse, "allowedAttributeValues",
              "predefinedAllowedAttributeValues");
          if (Objects.nonNull(attributeResponse)) {
            BeanUtils.copyProperties(attributeResponse, categoryAttributeWebResponse);
          }
          categoryAttributeWebResponses.add(categoryAttributeWebResponse);
        }
      }
      response.setCategoryAttributes(categoryAttributeWebResponses);
    }
  }

  /**
   * Convert to category mapping response
   *
   * @param categoryDTO
   * @return
   */
  public static CategoryMappingResponse toCategoryMappingResponse(CategoryDTO categoryDTO) {
    CategoryMappingResponse categoryMappingResponse = new CategoryMappingResponse();
    BeanUtils.copyProperties(categoryDTO, categoryMappingResponse);
    return categoryMappingResponse;
  }

  /**
   * Validation for category margin while activating a category
   *
   * @param clientResponse
   * @return
   */
  public static boolean marginResponseValidation(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getValue())) {
      return false;
    }
    return true;
  }

  /**
   * Validation for category shipping while activating category
   *
   * @param clientResponse
   */
  public static boolean categoryShippingValidation(GdnRestListResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || CollectionUtils.isEmpty(clientResponse.getContent())) {
      return false;
    }
    return true;
  }

  /**
   * Convert to create category web response
   *
   * @param createCategoryResponse
   * @return
   */
  public static CreateCategoryWebResponse toCreateCategoryWebResponse(CreateCategoryResponse createCategoryResponse) {
    return new CreateCategoryWebResponse(createCategoryResponse.getCategoryCode());
  }

  /**
   * To attribute value web response
   *
   * @param attributeResponses
   * @return
   */
  public static List<AttributeValuesWebResponse> toAttributeValuesWebResponseList(
      List<AttributeResponse> attributeResponses) {
    return attributeResponses.stream().map(ResponseHelper::toAttributeValuesWebResponse).collect(Collectors.toList());
  }

  private static AttributeValuesWebResponse toAttributeValuesWebResponse(AttributeResponse attributeResponse) {
    AttributeValuesWebResponse attributeValuesWebResponse = new AttributeValuesWebResponse();
    attributeValuesWebResponse.setId(attributeResponse.getId());
    attributeValuesWebResponse.setAttributeCode(attributeResponse.getAttributeCode());
    List<String> attributeValues = Optional.ofNullable(attributeResponse.getAllowedAttributeValues())
        .orElse(Collections.emptyList()).stream().map(AllowedAttributeValueResponse::getValue)
        .collect(Collectors.toList());
    attributeValuesWebResponse.setValues(attributeValues);
    return attributeValuesWebResponse;
  }

  public static MarginCategoryWebResponse toMarginCategoryWebResponse(
      MarginCategoryResponse marginCategoryResponse) {
    MarginCategoryWebResponse marginCategoryWebResponse = new MarginCategoryWebResponse();
    BeanUtils.copyProperties(marginCategoryResponse, marginCategoryWebResponse);
    marginCategoryWebResponse.setCategoryCode(marginCategoryResponse.getCategoryId());
    return marginCategoryWebResponse;
  }

  public static List<AttributeValueWebResponse> toAttributeValueWebResponseList(
      List<PredefinedAllowedAttributeValueResponse> attributeResponses) {
    return attributeResponses.stream().map(ResponseHelper::toAttributeValueWebResponse).collect(Collectors.toList());
  }

  public static AttributeValueWebResponse toAttributeValueWebResponse(
      PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse) {
    AttributeValueWebResponse attributeValueWebResponse = AttributeValueWebResponse.builder()
        .predefinedAllowedAttributeCode(predefinedAllowedAttributeValueResponse.getPredefinedAllowedAttributeCode())
        .id(predefinedAllowedAttributeValueResponse.getId()).value(predefinedAllowedAttributeValueResponse.getValue())
        .valueEn(predefinedAllowedAttributeValueResponse.getValueEn())
        .sequence(predefinedAllowedAttributeValueResponse.getSequence()).build();
    return attributeValueWebResponse;
  }

  public static AutoApprovalRulesListWebResponse toAutoApprovalRulesListWebResponse(
      AutoApprovalRulesListResponse autoApprovalRulesListResponse) {
    AutoApprovalRulesListWebResponse autoApprovalRulesListWebResponse = new AutoApprovalRulesListWebResponse();
    autoApprovalRulesListWebResponse.setAutoApprovalRulesWebResponseList(
        autoApprovalRulesListResponse.getAutoApprovalRulesDtoList().stream()
            .map(autoApprovalRulesDto -> toAutoApprovalRulesWebResponse(autoApprovalRulesDto))
            .collect(Collectors.toList()));
    return autoApprovalRulesListWebResponse;
  }

  public static AutoApprovalRulesWebResponse toAutoApprovalRulesWebResponse(AutoApprovalRulesDto autoApprovalRulesDto) {
    AutoApprovalRulesWebResponse autoApprovalRulesWebResponse = new AutoApprovalRulesWebResponse();
    BeanUtils.copyProperties(autoApprovalRulesDto, autoApprovalRulesWebResponse, "ruleConfig", "imageQcConfig",
        "autoApprovalType");
    autoApprovalRulesWebResponse.setAutoApprovalType(autoApprovalRulesDto.getAutoApprovalType().name());
    autoApprovalRulesWebResponse.setRuleConfig(autoApprovalRulesDto.getRuleConfig().stream()
        .map(autoApprovalRuleDetailsDto -> toAutoApprovalRuleDetailWebResponse(autoApprovalRuleDetailsDto))
        .collect(Collectors.toList()));
    autoApprovalRulesWebResponse.setImageConfig(autoApprovalRulesDto.getImageQcConfig().stream()
        .map(autoApprovalRuleDetailsDto -> toAutoApprovalRuleDetailWebResponse(autoApprovalRuleDetailsDto))
        .collect(Collectors.toList()));
    autoApprovalRulesWebResponse.setNeedRevisionImageConfig(autoApprovalRulesDto.getNeedRevisionImageQcConfig().stream()
        .map(autoApprovalRuleDetailsDto -> toAutoApprovalRuleDetailWebResponse(autoApprovalRuleDetailsDto))
        .collect(Collectors.toList()));
    return autoApprovalRulesWebResponse;
  }

  public static AutoApprovalRuleDetailWebResponse toAutoApprovalRuleDetailWebResponse(
      AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto) {
    AutoApprovalRuleDetailWebResponse autoApprovalRuleDetailWebResponse = new AutoApprovalRuleDetailWebResponse();
    BeanUtils.copyProperties(autoApprovalRuleDetailsDto, autoApprovalRuleDetailWebResponse);
    return autoApprovalRuleDetailWebResponse;
  }

  private static AttributeValueWebResponse toAttributeValueWebResponseFromAllowedAttributeValue(
      AllowedAttributeValueResponse allowedAttributeValueResponse) {
    AttributeValueWebResponse attributeValueWebResponse = AttributeValueWebResponse.builder()
        .allowedAttributeCode(allowedAttributeValueResponse.getAllowedAttributeCode())
        .id(allowedAttributeValueResponse.getId()).value(allowedAttributeValueResponse.getValue())
        .sequence(allowedAttributeValueResponse.getSequence()).build();
    return attributeValueWebResponse;
  }

  public static MarginCategoryWebResponse getMarginBusinessPartnerWebResponse(MarginOrderResponse marginOrderResponse) {
    MarginCategoryWebResponse marginCategoryWebResponse = new MarginCategoryWebResponse();
    BeanUtils.copyProperties(marginOrderResponse, marginCategoryWebResponse);
    return marginCategoryWebResponse;
  }

  public static FilterMarginsByOrderItemsRequest getFilterMarginsByOrderItemsRequest(String businessPartnerCode,
      String categoryCode, boolean officialStore, boolean setDefaultOrderTypeForMargin,
      String defaultOrderTypeForMargin) {
    FilterMarginsByOrderItemsRequest filterMarginsByOrderItemsRequest = new FilterMarginsByOrderItemsRequest();
    com.gdn.partners.pcu.master.client.model.OrderItem orderItem =
        new com.gdn.partners.pcu.master.client.model.OrderItem();
    orderItem.setCategoryCode(categoryCode);
    orderItem.setOrderItemId(ORDER_ITEM_ID);
    orderItem.setStoreCode(businessPartnerCode);
    orderItem.setTransactionDate(new Date());
    orderItem.setOfficialStore(officialStore);
    if (setDefaultOrderTypeForMargin) {
      orderItem.setOrderType(defaultOrderTypeForMargin);
    }
    filterMarginsByOrderItemsRequest.setMarginOrderItem(Collections.singletonList(orderItem));
    return filterMarginsByOrderItemsRequest;
  }


  public static MarginCategoryWebResponse getMarginBusinessPartnerWebResponse
      (Optional<OrderItemMarginsResponse> orderItemMarginsResponse, String categoryCode) {
    MarginCategoryWebResponse marginCategoryWebResponse = new MarginCategoryWebResponse();
    if (Objects.nonNull(orderItemMarginsResponse)) {
      Optional<Margin> firstMargin = orderItemMarginsResponse.flatMap(
          response -> Optional.ofNullable(response.getMargins()).orElse(new ArrayList<>()).stream()
              .filter(Objects::nonNull).findFirst());
      firstMargin.ifPresent(margin -> {
        marginCategoryWebResponse.setMarginType(margin.getMarginType());
        marginCategoryWebResponse.setMaximumValue(margin.getMaximumMargin());
        marginCategoryWebResponse.setMinimumValue(margin.getMinimumMargin());
        marginCategoryWebResponse.setTransactionFee(margin.getTransactionFee());
        marginCategoryWebResponse.setValueType(PERCENTAGE);
        marginCategoryWebResponse.setValue(margin.getMarginPercentage());
        marginCategoryWebResponse.setCategoryCode(categoryCode);
      });
    }
    return marginCategoryWebResponse;
  }


  public static AttributeDetailWebResponse toAttributeDetailWebResponse(AttributeResponse attributeResponse) {
    AttributeDetailWebResponse attributeDetailWebResponse = new AttributeDetailWebResponse();
    BeanUtils.copyProperties(attributeResponse, attributeDetailWebResponse);
    attributeDetailWebResponse.setAttributeValues(attributeResponse.getAllowedAttributeValues().stream().map(
        allowedAttributeValueResponse -> toAttributeValueWebResponseFromAllowedAttributeValue(
            allowedAttributeValueResponse)).collect(Collectors.toList()));
    attributeDetailWebResponse.getAttributeValues().addAll(
        attributeResponse.getPredefinedAllowedAttributeValues().stream().map(
            predefinedAllowedAttributeValueResponse -> toAttributeValueWebResponse(
                predefinedAllowedAttributeValueResponse)).collect(Collectors.toList()));
    return attributeDetailWebResponse;
  }

  public static RestrictedKeywordsWebResponse getRestrictedKeywordsWebResponse(
      RestrictedKeywordsResponse restrictedKeywordsResponse) {
    RestrictedKeywordsWebResponse restrictedKeywordsWebResponse = new RestrictedKeywordsWebResponse();
    BeanUtils.copyProperties(restrictedKeywordsResponse, restrictedKeywordsWebResponse);
    return restrictedKeywordsWebResponse;
  }

  public static RestrictedKeywordHistoryWebResponse getRestrictedKeywordHistoryWebResponse(
      RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse) {
    RestrictedKeywordHistoryWebResponse response = new RestrictedKeywordHistoryWebResponse();
    BeanUtils.copyProperties(restrictedKeywordHistoryResponse, response, "id", "storeId", "version");
    return response;
  }

  public static UiValidationRestrictedKeywordsWebResponse toUiValidationRestrictedKeywordsWebResponse(
      UiValidationRestrictedKeywordsResponse uiValidationRestrictedKeywordsResponse) {
    UiValidationRestrictedKeywordsWebResponse response = new UiValidationRestrictedKeywordsWebResponse();
    BeanUtils.copyProperties(uiValidationRestrictedKeywordsResponse, response);
    return response;
  }

  public static RestrictedKeywordsListingWebResponse toRestrictedKeywordsListingWebResponse(
      RestrictedKeywordsListingResponse restrictedKeywordsListingResponse) {
    RestrictedKeywordsListingWebResponse response = new RestrictedKeywordsListingWebResponse();
    BeanUtils.copyProperties(restrictedKeywordsListingResponse, response);
    return response;
  }

  public static WholesaleMappingWebResponse getWholesaleMappingWebResponse(
      WholesaleMappingResponse wholesaleMappingResponse) {
    WholesaleMappingWebResponse wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    toWholesaleMappingWebResponse(wholesaleMappingResponse, wholesaleMappingWebResponse);
    return wholesaleMappingWebResponse;
  }

  public static void toWholesaleMappingWebResponse(
      WholesaleMappingResponse wholesaleMappingResponse,
      WholesaleMappingWebResponse wholesaleMappingWebResponse) {
    if (Objects.isNull(wholesaleMappingResponse)) {
      return;
    }
    wholesaleMappingWebResponse.setConfigurationType(
        wholesaleMappingResponse.getConfigurationType());
    wholesaleMappingWebResponse.setWholesalePriceConfigEnabled(
        wholesaleMappingResponse.isWholesalePriceConfigEnabled());
    List<WholesaleConfigWebResponse> wholesaleConfigWebResponses =
        wholesaleMappingResponse.getWholesaleConfig().stream()
            .map(ResponseHelper::toWholesaleConfigWebResponse)
            .collect(Collectors.toList());
    wholesaleMappingWebResponse.setWholesaleConfig(wholesaleConfigWebResponses);
  }

  private static WholesaleConfigWebResponse toWholesaleConfigWebResponse(
      WholesaleConfigResponse wholesaleConfigResponse) {
    if (Objects.isNull(wholesaleConfigResponse)) {
      return null;
    }
    return WholesaleConfigWebResponse.builder().quantity(wholesaleConfigResponse.getQuantity())
        .minWholesaleDiscount(wholesaleConfigResponse.getMinWholesaleDiscount().stream()
            .map(ResponseHelper::toMinWholesaleDiscountWebResponse).collect(Collectors.toList()))
        .build();
  }

  private static MinWholesaleDiscountWebResponse toMinWholesaleDiscountWebResponse(
      MinWholesaleDiscountResponse minWholesaleDiscountResponse) {
    if (Objects.isNull(minWholesaleDiscountResponse)) {
      return null;
    }
    return MinWholesaleDiscountWebResponse.builder().price(minWholesaleDiscountResponse.getPrice())
        .percentage(minWholesaleDiscountResponse.getPercentage()).build();
  }

  public static List<CatalogTreeWebResponse> getCatalogTreeWebResponse(
      List<List<CategoryResponse>> listOfCategoryResponses) {
    checkArgument(!Objects.isNull(listOfCategoryResponses), ErrorMessages.CATEGORY_LIST_MUST_NOT_BE_NULL);
    List<CatalogTreeWebResponse> catalogTreeList = new ArrayList<>();
    for (List<CategoryResponse> categoriesList : listOfCategoryResponses) {
      List<CategoryTreeWebResponse> categoryTreeList = new ArrayList<>();
      for (CategoryResponse category : categoriesList) {
        CategoryTreeWebResponse categoryTree = new CategoryTreeWebResponse();
        categoryTree.setCategoryName(category.getName());
        categoryTree.setCategoryCode(category.getCategoryCode());
        categoryTree.setCategoryId(category.getId());
        categoryTree.setCategoryNameEnglish(category.getNameEnglish());
        categoryTree.setLevel(categoriesList.size() - categoriesList.indexOf(category));
        categoryTreeList.add(categoryTree);
      }
      if (CollectionUtils.isNotEmpty(categoriesList)) {
        String catalogCode = categoriesList.get(0).getCatalog().getCatalogCode();
        catalogTreeList.add(new CatalogTreeWebResponse(catalogCode, categoryTreeList));
      }
    }
    return catalogTreeList;
  }

  public static List<SuggestedCategoriesWebResponse> toPredictedCategoriesWebResponse(
      ProductCategorySuggestionResponse productCategoryPredictionResponse) {
    if (CollectionUtils.isEmpty(productCategoryPredictionResponse.getPredicted_categories())) {
      return new ArrayList<>();
    } else {
      return productCategoryPredictionResponse.getPredicted_categories().stream().map(
          predictedCategoriesResponse -> new SuggestedCategoriesWebResponse(
              toCategorySuggestionWebResponse(predictedCategoriesResponse.getCategories()))).collect(Collectors.toList());
    }
  }

  private static List<CategorySuggestionWebResponse> toCategorySuggestionWebResponse(
      List<CategorySuggestionResponse> categories) {
    List<CategorySuggestionWebResponse> categorySuggestionWebResponses = new ArrayList<>();
    for (CategorySuggestionResponse categorySuggestionResponse : categories) {
      CategorySuggestionWebResponse categorySuggestionWebResponse =
          CategorySuggestionWebResponse.builder().categoryId(categorySuggestionResponse.getCategory_id())
              .categoryCode(categorySuggestionResponse.getCategory_code())
              .categoryLevel(categorySuggestionResponse.getCategory_level())
              .categoryName(categorySuggestionResponse.getCategory_name())
              .categoryNameEnglish(categorySuggestionResponse.getCategory_name_english()).build();
      categorySuggestionWebResponses.add(categorySuggestionWebResponse);
    }
    return categorySuggestionWebResponses;
  }

  public static boolean validateResponse(GdnRestSimpleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() || Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(clientResponse.getErrorMessage());
    }
    return true;
  }

  public static List<OscSummaryWebResponse> toOscSummaryWebResponse(List<OscSummaryResponse> oscSummaryList) {
    List<OscSummaryWebResponse> oscSummaryWebResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(oscSummaryList)) {
      for (OscSummaryResponse oscSummaryResponse : oscSummaryList) {
        OscSummaryWebResponse response = new OscSummaryWebResponse();
        BeanUtils.copyProperties(oscSummaryResponse, response);
        response.setId(oscSummaryResponse.getId());
        oscSummaryWebResponses.add(response);
      }
    }
    return oscSummaryWebResponses;
  }

  public static OscDetailsWebResponse toOscDetailsWebResponse(com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse originalSalesCategoryResponse) {
    OscDetailsWebResponse oscDetailsWebResponse = new OscDetailsWebResponse();
    BeanUtils.copyProperties(originalSalesCategoryResponse, oscDetailsWebResponse, Constants.MASTER_CATEGORIES);
    List<MasterCategoryWebResponse> masterCategoryWebResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(originalSalesCategoryResponse.getMasterCategories())) {
      for (CategoryResponse categoryResponse : originalSalesCategoryResponse.getMasterCategories()) {
        MasterCategoryWebResponse masterCategoryWebResponse = new MasterCategoryWebResponse();
        BeanUtils.copyProperties(categoryResponse, masterCategoryWebResponse);
        masterCategoryWebResponses.add(masterCategoryWebResponse);
      }
    }
    oscDetailsWebResponse.setMasterCategories(masterCategoryWebResponses);
    return oscDetailsWebResponse;
  }

  public static List<BaseMarginHierarchyWebResponse> toBaseMarginHierarchyWebResponse(List<CategoryResponse>
      categoryResponses, List<BaseMarginResponse> baseMarginResponse) {
    List<BaseMarginHierarchyWebResponse> baseMarginHierarchyWebResponses = new ArrayList<>();
    int level = categoryResponses.size();
    Map<String, BaseMarginResponse> categoryCodeBaseMarginResponseMap = baseMarginResponse.stream()
        .collect(Collectors.toMap(BaseMarginResponse::getCategoryCode, Function.identity(),
            (categoryCode1, categoryCode2) -> categoryCode1));
    for (CategoryResponse categoryResponse : categoryResponses) {
      baseMarginHierarchyWebResponses.add(0, toBaseMarginResponse(categoryResponse, level,
          categoryCodeBaseMarginResponseMap));
      level -= 1;
    }
    return baseMarginHierarchyWebResponses;
  }

  private static BaseMarginHierarchyWebResponse toBaseMarginResponse(CategoryResponse categoryResponse,
      int level, Map<String, BaseMarginResponse> categoryCodeBaseMarginResponseMap) {
    BaseMarginHierarchyWebResponse baseMarginHierarchyWebResponse = new BaseMarginHierarchyWebResponse();
    baseMarginHierarchyWebResponse.setCategoryCode(categoryResponse.getCategoryCode());
    baseMarginHierarchyWebResponse.setCategoryName(categoryResponse.getName());
    baseMarginHierarchyWebResponse.setCategoryNameEnglish(categoryResponse.getNameEnglish());
    baseMarginHierarchyWebResponse.setLevel(level);
    if (categoryCodeBaseMarginResponseMap.containsKey(categoryResponse.getCategoryCode())) {
      baseMarginHierarchyWebResponse.setMarginDetails(marginDetailsWebResponses(
          categoryCodeBaseMarginResponseMap.get(categoryResponse.getCategoryCode())));
    }
    return baseMarginHierarchyWebResponse;
  }

  private static List<MarginDetailsWebResponse> marginDetailsWebResponses(BaseMarginResponse baseMarginResponse) {
    List<MarginDetailsWebResponse> marginDetailsWebResponses = new ArrayList<>();
    for (BaseMarginGroup baseMarginGroup : Optional.ofNullable(baseMarginResponse.getBaseMargins())
        .orElse(new ArrayList<>())) {
      MarginDetailsWebResponse marginDetailsWebResponse = new MarginDetailsWebResponse();
      marginDetailsWebResponse.setMarginPercentage(baseMarginGroup.getPercentage());
      marginDetailsWebResponse.setChannel(marginFactorTypes(baseMarginGroup.getFactors(),
          Constants.CHANNEL));
      marginDetailsWebResponse.setSellerType(marginFactorTypes(baseMarginGroup.getFactors(),
          Constants.SELLER_TYPE));
      marginDetailsWebResponses.add(marginDetailsWebResponse);
    }
    return marginDetailsWebResponses;
  }

  private static String marginFactorTypes(List<MarginFactor> factors, String type) {
    return Optional.ofNullable(factors).orElse(new ArrayList<>())
        .stream().filter(factor -> type.equals(factor.getFactorType()))
        .findFirst().orElse(new MarginFactor()).getFactorValue();
  }

  public static ProfitMarginWebResponse toProfitMarginWebResponse(ProfitMarginWebRequest profitMarginWebRequest,
      MarginCategoryWebResponse marginCategoryWebResponse) {
    ProfitMarginWebResponse profitMarginWebResponse = new ProfitMarginWebResponse();
    BeanUtils.copyProperties(marginCategoryWebResponse, profitMarginWebResponse);
    profitMarginWebResponse.setCommissionPercentage(marginCategoryWebResponse.getValue());
    if (profitMarginWebRequest.getSellingPrice() != 0 && profitMarginWebRequest.getBasicProductPrice() !=0) {
      profitMarginWebResponse.setCommission(roundToPrecision(calculateCommission(
          profitMarginWebRequest.getSellingPrice(), marginCategoryWebResponse)));
      updateProfitMarginAndProfitMarginPercentage(profitMarginWebRequest, profitMarginWebResponse);
    }
    return profitMarginWebResponse;
  }

  private static Double calculateCommission(Double sellingPrice, MarginCategoryWebResponse marginCategoryWebResponse) {
    if (Objects.isNull(marginCategoryWebResponse.getValue())) {
      marginCategoryWebResponse.setValue(Constants.ZERO);
    }
    Double commission = sellingPrice * marginCategoryWebResponse.getValue() / Constants.HUNDRED;

    if (Objects.nonNull(marginCategoryWebResponse.getMinimumValue())
        && commission < marginCategoryWebResponse.getMinimumValue()) {
      commission = marginCategoryWebResponse.getMinimumValue();
    } else if (Objects.nonNull(marginCategoryWebResponse.getMaximumValue())
        && commission > marginCategoryWebResponse.getMaximumValue()) {
      commission = marginCategoryWebResponse.getMaximumValue();
    }

    return commission;
  }

  private static void updateProfitMarginAndProfitMarginPercentage(ProfitMarginWebRequest profitMarginWebRequest,
      ProfitMarginWebResponse profitMarginWebResponse) {
    if (Objects.isNull(profitMarginWebResponse.getTransactionFee())) {
      profitMarginWebResponse.setTransactionFee(Constants.ZERO);
    }
    double profitMargin = profitMarginWebRequest.getSellingPrice() -
        (profitMarginWebRequest.getBasicProductPrice() + profitMarginWebResponse.getTransactionFee() +
            profitMarginWebResponse.getCommission());
    Double profitMarginPercentage =
        profitMargin * Constants.HUNDRED / profitMarginWebRequest.getBasicProductPrice();
    profitMarginWebResponse.setProfitMargin(roundToPrecision(profitMargin));
    profitMarginWebResponse.setProfitMarginPercentage(roundToPrecision(profitMarginPercentage));
  }

  public static double roundToPrecision(double number) {
    double scale = Math.pow(10, Constants.PRECISION);
    return Math.round(number * scale) / scale;
  }

  public static void validateResponseByErrorCode(GdnRestListResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode()
      .equals(clientResponse.getErrorCode())) {
      throw new ValidationException(clientResponse.getErrorMessage());
    }else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
  }

  public static void validateResponseByErrorCodeExcludeValue(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() && Arrays.stream(ErrorCodes.values())
        .map(ErrorCodes::getErrorCode).collect(Collectors.toSet())
        .contains(clientResponse.getErrorCode())) {
      throw new ValidationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
  }

  public static void validateResponseByErrorCode(GdnRestSingleResponse clientResponse) {
    if (Objects.isNull(clientResponse) || Objects.isNull(clientResponse.getValue())) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() && ErrorCategory.VALIDATION.getCode()
            .equals(clientResponse.getErrorCode())) {
      throw new ValidationException(clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
  }

  public static void validateResponseByErrorCode(GdnBaseRestResponse clientResponse) {
    if (Objects.isNull(clientResponse)) {
      throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
    }
    if (!clientResponse.isSuccess() && Arrays.stream(ErrorCodes.values())
        .map(ErrorCodes::getErrorCode).collect(Collectors.toSet())
        .contains(clientResponse.getErrorCode())) {
      throw new ValidationException(clientResponse.getErrorCode(),
          clientResponse.getErrorMessage());
    } else if (!clientResponse.isSuccess()) {
      throw new ClientException((ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));
    }
  }

  public static boolean isSizeChartNameValid(String sizeChartName) {
    return sizeChartName != null && sizeChartName.length() <= Constants.SIZE_CHART_NAME_LENGTH;
  }
}
