package com.gdn.partners.pcu.master.service.impl.helper;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import brave.Response;
import com.gdn.partners.pcu.master.client.model.AllowedAttributeValueResponse;
import com.gdn.partners.pcu.master.client.model.AttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryAttributeResponse;
import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;

import com.gda.mta.product.dto.AutoApprovalRulesDto;
import com.gda.mta.product.dto.response.AutoApprovalRuleDetailsDto;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.master.client.model.BaseMarginGroup;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategorySuggestionResponse;
import com.gdn.partners.pcu.master.client.model.Margin;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import com.gdn.partners.pcu.master.client.model.MarginFactor;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;
import com.gdn.partners.pcu.master.client.model.SuggestedCategoriesResponse;
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
import com.gdn.partners.pcu.master.web.model.response.CategoryInfoWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryMappingResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CreateCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.ProfitMarginWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ResponseHelperTest {

  private static final String ERROR_MESSAGE = "ERROR MESSAGE";
  private static final String ERROR_CODE = "ERROR_CODE";
  private static final String VALIDATION_CODE = "VALIDATION";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String NAME = "name";
  private static final String NAME_ENGLISH = "nameEnglish";
  private static final String ATT_NAME = "attribute name";
  private static final String ATTRIBUTE_CODE = "attribute_code";
  private static final String CATEGORY_CODE = "category_code";
  private static final String BUSINESS_PARTNER_CODE = "business-partner-code";
  private static final String CATALOG_CODE = "catalog_code";
  private static final Double VALUE = 2.0;
  private static final String MARGIN_NOTE = "margin";
  private static final BaseResponse RESPONSE = new BaseResponse();
  private static final Double TRANSACTION_FEE = 1.0;
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "PREDEFINED_ALLOWED_ATTRIBUTE_CODE";
  private static final String ID = "ID";
  private static final Integer SEQUENCE = 1;
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE_VALUE = "VALUE";
  private static final String KEYWORD = "KEYWORD";
  private static final String KEYWORD_ID = "KEYWORD_ID";
  private static final String ACTIVITY = "activity";
  private static final String OLD_VALUE = "oldValue";
  private static final String CREATED_BY = "dev";
  private static final String NEW_VALUE = "newValue";
  private static final String CONFIGURATION_TYPE = "PRICE_PERCENTAGE";
  private static final Integer QUANTITY = 10;
  private static final Double PRICE = 10000.0;
  private static final Double PERCENTAGE = 10D;
  private static final boolean WHOLESALE_CONFIG_ENABLED = true;
  private static final String CATEGORY_ID = "categoryId";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String PRODUCT_NAME = "productName";
  private static final String OSC_CODE = "OSC-00001";
  private static final String OSC_SHORT_TEXT = "oscShortText";
  private static final String OSC_LONG_TEXT = "oscLongText";
  private static final String RULE_NAME = "OFFICIAL_SELLERS";
  private static final String AUTO_APPROVAL_TYPE = "CONTENT_AND_IMAGE";
  private static final String KEY_NAME = "blur";
  private static final String VALUE_AUTO = "5";
  private static final String VALUE_TYPE = "int";
  private static final String OPERATOR = "<=";
  private static final String CHANNEL_ALL = "CHANNEL_ALL";
  private static final String SELLER_ALL = "SELLER_ALL";
  private static final Double SELLING_PRICE = 200.0;
  private static final Double BASE_PRICE = 100.0;
  private static final String BASE_MARGIN = "BASE_MARGIN";
  private static final Double MIN_COMM = 10.0;
  private static final Double MAX_COMM = 30.0;
  private static final double NUM_1 = 12.3482;
  private static final double NUM_2 = 3456.9832;
  private static final double NUM_3 = 546.1;
  private static final double NUM_4 = 126;
  private static final double NUM_5 = -12.3432;
  private static final double NUM_6 = -3456.9882;
  private static final double NUM_7 = -546.1;
  private static final double NUM_8 = -126;
  private static final double NUM_PRECISION_1 = 12.35;
  private static final double NUM_PRECISION_2 = 3456.98;
  private static final double NUM_PRECISION_3 = 546.10;
  private static final double NUM_PRECISION_4 = 126.00;
  private static final double NUM_PRECISION_5 = -12.34;
  private static final double NUM_PRECISION_6 = -3456.99;
  private static final double NUM_PRECISION_7 = -546.10;
  private static final double NUM_PRECISION_8 = -126.00;

  private CategoryDetailAndShippingResponse categoryDetailAndShippingResponse;
  private List<ShippingResponse> shippingResponses;
  private List<CategoryReferenceResponse> masterCategoryReferences;
  private List<CategoryReferenceResponse> salesCategoryReferences;
  private List<CategoryAttributeResponse> categoryAttributes;
  private CategoryAttributeResponse categoryAttributeResponse;
  private ShippingResponse shippingResponse;
  private CategoryReferenceResponse masterCategoryReference;
  private CategoryReferenceResponse salesCategoryReference;
  private AttributeResponse attributeResponse;
  private CategoryResponse categoryResponse;
  private CatalogResponse catalogResponse;
  private OriginalSalesCategoryResponse originalSalesCategoryResponse;
  private List<AttributeResponse> attributeResponses;
  private MarginCategoryResponse marginCategoryResponse;
  private MarginOrderResponse marginOrderResponse;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse;
  private AllowedAttributeValueResponse allowedAttributeValueResponse;
  private RestrictedKeywordsResponse restrictedKeywordsResponse;
  private List<RestrictedKeywordsResponse> restrictedKeywordsResponses;
  private WholesaleMappingResponse wholesaleMappingResponse;
  private WholesaleConfigResponse wholesaleConfigResponse;
  private MinWholesaleDiscountResponse minWholesaleDiscountResponse;
  private ProductCategorySuggestionResponse productCategoryPredictionResponse;
  private AutoApprovalRulesListResponse autoApprovalRulesListResponse;
  private AutoApprovalRulesDto autoApprovalRulesDto;
  private AutoApprovalRuleDetailsDto autoApprovalRuleDetailsDto;
  private AutoApprovalRuleDetailWebResponse autoApprovalRuleDetailWebResponse;
  private BaseMarginResponse baseMarginResponse;
  private BaseMarginResponse baseMarginResponse1;
  private MarginFactor marginFactor1;
  private MarginFactor marginFactor2;
  private BaseMarginGroup baseMarginGroup;
  private BaseMarginGroup baseMarginGroup1;
  private ProfitMarginWebRequest profitMarginWebRequest;
  private MarginCategoryWebResponse marginCategoryWebResponse;

  @BeforeEach
  void setUp() throws Exception {
    autoApprovalRuleDetailsDto = new AutoApprovalRuleDetailsDto();
    autoApprovalRuleDetailsDto.setKeyName(KEY_NAME);
    autoApprovalRuleDetailsDto.setValue(VALUE_AUTO);
    autoApprovalRuleDetailsDto.setOperator(OPERATOR);
    autoApprovalRuleDetailsDto.setValueType(VALUE_TYPE);
    autoApprovalRulesDto = new AutoApprovalRulesDto();
    autoApprovalRulesDto.setRuleName(RULE_NAME);
    autoApprovalRulesDto.setAutoApprovalType(AutoApprovalType.CONTENT_AND_IMAGE);
    autoApprovalRulesDto.setSequenceNumber(SEQUENCE);
    autoApprovalRulesDto.setMarkForDelete(true);
    autoApprovalRulesDto.setRuleConfig(Arrays.asList(autoApprovalRuleDetailsDto));
    autoApprovalRulesDto.setImageQcConfig(Arrays.asList(autoApprovalRuleDetailsDto));
    autoApprovalRulesDto.setNeedRevisionConfigEnabled(true);
    autoApprovalRulesDto.setNeedRevisionImageQcConfig(Arrays.asList(autoApprovalRuleDetailsDto));
    autoApprovalRulesListResponse = new AutoApprovalRulesListResponse();
    autoApprovalRulesListResponse.setAutoApprovalRulesDtoList(Arrays.asList(autoApprovalRulesDto));
    autoApprovalRuleDetailWebResponse = new AutoApprovalRuleDetailWebResponse();
    autoApprovalRuleDetailWebResponse.setKeyName(KEY_NAME);
    autoApprovalRuleDetailWebResponse.setValue(VALUE_AUTO);
    autoApprovalRuleDetailWebResponse.setOperator(OPERATOR);
    autoApprovalRuleDetailWebResponse.setValueType(VALUE_TYPE);
    categoryDetailAndShippingResponse = new CategoryDetailAndShippingResponse();
    catalogResponse = new CatalogResponse();
    catalogResponse.setName(NAME);
    shippingResponse = new ShippingResponse();
    shippingResponse.setDeliveredByMerchant(Boolean.TRUE);
    shippingResponse.setDirectFlight(Boolean.TRUE);
    shippingResponse.setSpecialHandling(Boolean.TRUE);
    shippingResponse.setSizeChartRequired(Boolean.TRUE);
    shippingResponses = new ArrayList<>();
    shippingResponses.add(shippingResponse);
    categoryDetailAndShippingResponse.setShippingResponses(shippingResponses);
    categoryDetailAndShippingResponse.setWholesalePriceConfigEnabled(true);
    attributeResponse = new AttributeResponse();
    attributeResponse.setName(NAME);
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributes = new ArrayList<>();
    categoryAttributes.add(categoryAttributeResponse);
    masterCategoryReference = new CategoryReferenceResponse();
    salesCategoryReference = new CategoryReferenceResponse();
    categoryResponse = new CategoryResponse();
    categoryResponse.setName(NAME);
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    masterCategoryReference.setMasterCategoryReference(categoryResponse);
    salesCategoryReference.setSalesCategoryReference(categoryResponse);
    masterCategoryReferences = new ArrayList<>();
    salesCategoryReferences = new ArrayList();
    masterCategoryReferences.add(masterCategoryReference);
    salesCategoryReferences.add(salesCategoryReference);
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    attributeResponses = Arrays.asList(attributeResponse);

    marginCategoryResponse = new MarginCategoryResponse();
    marginCategoryResponse.setCategoryId(CATEGORY_CODE);
    marginCategoryResponse.setValue(VALUE);
    marginCategoryResponse.setNote("margin");

    marginOrderResponse = new MarginOrderResponse();
    marginOrderResponse.setValue(VALUE);
    marginOrderResponse.setTransactionFee(TRANSACTION_FEE);

    predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue(PREDEFINED_ALLOWED_ATTRIBUTE_CODE_VALUE);
    predefinedAllowedAttributeValueResponse.setId(ID);
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueResponse.setSequence(SEQUENCE);

    allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode(ID);
    allowedAttributeValueResponse.setSequence(SEQUENCE);
    allowedAttributeValueResponse.setValue(String.valueOf(VALUE));

    this.attributeResponse.setAttributeCode(ATTRIBUTE_CODE);
    this.attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));
    this.attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));

    restrictedKeywordsResponse = new RestrictedKeywordsResponse();
    restrictedKeywordsResponse.setKeyword(KEYWORD);
    restrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsResponse.setSelected(false);
    restrictedKeywordsResponses = new ArrayList<>();
    restrictedKeywordsResponses.add(restrictedKeywordsResponse);

    minWholesaleDiscountResponse = new MinWholesaleDiscountResponse();
    minWholesaleDiscountResponse.setPercentage(PERCENTAGE);
    minWholesaleDiscountResponse.setPrice(PRICE);
    wholesaleConfigResponse =  new WholesaleConfigResponse();
    wholesaleConfigResponse.setQuantity(QUANTITY);
    wholesaleConfigResponse.setMinWholesaleDiscount(Arrays.asList(minWholesaleDiscountResponse));
    wholesaleMappingResponse = new WholesaleMappingResponse();
    wholesaleMappingResponse.setWholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLED);
    wholesaleMappingResponse.setConfigurationType(CONFIGURATION_TYPE);
    wholesaleMappingResponse.setWholesaleConfig(Arrays.asList(wholesaleConfigResponse));

    CategorySuggestionResponse categorySuggestionResponse =
        new CategorySuggestionResponse("1", CATEGORY_ID, CATEGORY_CODE, CATEGORY_NAME, CATEGORY_NAME);
    SuggestedCategoriesResponse suggestedCategoriesResponse =
        new SuggestedCategoriesResponse(Arrays.asList(categorySuggestionResponse), 0.8);
    productCategoryPredictionResponse =
        new ProductCategorySuggestionResponse(Arrays.asList(suggestedCategoriesResponse), PRODUCT_NAME);

    originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
    originalSalesCategoryResponse.setId(ID);
    originalSalesCategoryResponse.setOscCode(OSC_CODE);
    originalSalesCategoryResponse.setOscShortText(OSC_SHORT_TEXT);
    originalSalesCategoryResponse.setOscLongText(OSC_LONG_TEXT);
    originalSalesCategoryResponse.setActivated(true);
    originalSalesCategoryResponse.setStoreId(Constants.STORE_ID);

    marginFactor1 = new MarginFactor();
    marginFactor1.setFactorType(Constants.SELLER_TYPE);
    marginFactor1.setFactorValue(SELLER_ALL);

    marginFactor2 = new MarginFactor();
    marginFactor2.setFactorType(Constants.CHANNEL);
    marginFactor2.setFactorValue(CHANNEL_ALL);

    baseMarginGroup = new BaseMarginGroup();
    baseMarginGroup.setPercentage(PERCENTAGE);
    baseMarginGroup.setFactors(Arrays.asList(marginFactor1, marginFactor2));

    baseMarginGroup1 = new BaseMarginGroup();
    baseMarginGroup1.setPercentage(PERCENTAGE);
    baseMarginGroup1.setFactors(null);

    baseMarginResponse = new BaseMarginResponse();
    baseMarginResponse.setCategoryCode(CATEGORY_CODE);
    baseMarginResponse.setBaseMargins(Arrays.asList(baseMarginGroup, baseMarginGroup1));

    baseMarginResponse1 = new BaseMarginResponse();
    baseMarginResponse1.setCategoryCode(CATEGORY_CODE);

    profitMarginWebRequest = new ProfitMarginWebRequest();
    marginCategoryWebResponse = new MarginCategoryWebResponse();

    profitMarginWebRequest.setCategoryCode(CATEGORY_CODE);
    profitMarginWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    profitMarginWebRequest.setSellingPrice(SELLING_PRICE);
    profitMarginWebRequest.setBasicProductPrice(BASE_PRICE);

    marginCategoryWebResponse.setCategoryCode(CATEGORY_CODE);
    marginCategoryWebResponse.setValue(PERCENTAGE);
    marginCategoryWebResponse.setMarginType(BASE_MARGIN);
    marginCategoryWebResponse.setMaximumValue(MAX_COMM);
    marginCategoryWebResponse.setMinimumValue(MIN_COMM);
    marginCategoryWebResponse.setTransactionFee(TRANSACTION_FEE);

  }

  @Test
  void validateResponse_GdnRestSingleResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestSingleResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestSingleResponse_SuccessFalseTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestSingleResponse_ValueNullTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnRestSingleResponse_ValueNullTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
            new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
            new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnRestSingleResponse_SuccessFalseTest_ValidationCode() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
            new GdnRestSingleResponse<>(ERROR_MESSAGE, VALIDATION_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }


  @Test
  void validateResponseByErrorCode_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode((GdnRestSingleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnRestListResponse_SuccessTrueTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
            new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, Collections.singletonList(RESPONSE),
                    new PageMetaData(0, 10, 100), REQUEST_ID);
    ResponseHelper.validateResponseByErrorCode(gdnRestListResponse);
  }

  @Test
  void validateResponseByErrorCode_GdnRestListResponse_SuccessFalseTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
            new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, false, Collections.singletonList(RESPONSE),
                    new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnRestListResponse_SuccessFalseTest_ValidationCode (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
            new GdnRestListResponse<>(ERROR_MESSAGE, VALIDATION_CODE, false, Collections.singletonList(RESPONSE),
                    new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }


  @Test
  void validateResponseByErrorCode_GdnRestListResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode((GdnRestListResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }



  @Test
  void validateResponseByErrorCode_GdnRestSingleResponse_SuccessTrueTest() {
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
      new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, RESPONSE, REQUEST_ID);
    ResponseHelper.validateResponseByErrorCode(gdnRestSingleResponse);
  }

  @Test
  void validateResponseByErrorCodeExcludeValue_GdnRestSingleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCodeExcludeValue(null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCodeExcludeValue_GdnRestSingleResponse_SuccessFalse_ErrorCodeTest() {
    GdnRestSingleResponse<BaseResponse> response = new GdnRestSingleResponse<>(ERROR_MESSAGE,
        ErrorCodes.DIMENSION_ALREADY_EXISTS.getErrorCode(), false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCodeExcludeValue(response);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCodeExcludeValue_GdnRestSingleResponse_SuccessFalseTest() {
    GdnRestSingleResponse<BaseResponse> response = new GdnRestSingleResponse<>(ERROR_MESSAGE,
        ERROR_CODE, false, RESPONSE, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCodeExcludeValue(response);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCodeExcludeValue_GdnRestSingleResponse_SuccessTrueTest() {
    GdnRestSingleResponse<BaseResponse> response = new GdnRestSingleResponse<>(null,
        null, true, RESPONSE, REQUEST_ID);
    ResponseHelper.validateResponseByErrorCodeExcludeValue(response);
  }

  @Test
  void validateResponseByErrorCode_GdnBaseResponse_successFalseDimensionAlreadyExists() {
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ErrorCodes.DIMENSION_ALREADY_EXISTS.getErrorMessage(),
            ErrorCodes.DIMENSION_ALREADY_EXISTS.getErrorCode(), false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ValidationException.class, exception.getClass());
      assertEquals(ErrorCodes.DIMENSION_ALREADY_EXISTS.getErrorMessage(), exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnBaseResponse_nullTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode((GdnBaseRestResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnBaseResponse_successFalse() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(false);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponseByErrorCode(gdnBaseRestResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponseByErrorCode_GdnBaseResponse_successTrue() {
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse(true);
    ResponseHelper.validateResponseByErrorCode(gdnBaseRestResponse);
  }

  @Test
  void validateResponse_GdnRestSingleResponse_SuccessTrueTest (){
    GdnRestSingleResponse<BaseResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, RESPONSE, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestSingleResponse));
  }

  @Test
  void validateResponse_GdnRestListResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestListResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestListResponse_SuccessFalseTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, false, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestListResponse_ValueNullTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, null,
            new PageMetaData(0, 10, 100), REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestListResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestListResponse_SuccessTrueTest (){
    GdnRestListResponse<BaseResponse> gdnRestListResponse =
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, true, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestListResponse));
  }

  @Test
  void validateResponse_GdnBaseRestResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnBaseRestResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnBaseRestResponse_SuccessFalseTest (){
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnBaseRestResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnBaseRestResponse_SuccessTrueTest (){
    GdnBaseRestResponse gdnBaseRestResponse =
        new GdnBaseRestResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID);
    assertTrue(ResponseHelper.validateResponse(gdnBaseRestResponse));
  }

  @Test
  void getRestrictedKeywordHistoryWebResponseTest() {
    RestrictedKeywordHistoryResponse restrictedKeywordHistoryResponse = new RestrictedKeywordHistoryResponse();
    restrictedKeywordHistoryResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordHistoryResponse.setActivity(ACTIVITY);
    restrictedKeywordHistoryResponse.setNewValue(NEW_VALUE);
    restrictedKeywordHistoryResponse.setOldValue(OLD_VALUE);
    restrictedKeywordHistoryResponse.setCreatedBy(CREATED_BY);
    RestrictedKeywordHistoryWebResponse response =
        ResponseHelper.getRestrictedKeywordHistoryWebResponse(restrictedKeywordHistoryResponse);
    Assertions.assertEquals(KEYWORD_ID, response.getKeywordId());
    Assertions.assertEquals(ACTIVITY, response.getActivity());
    Assertions.assertEquals(NEW_VALUE, response.getNewValue());
    Assertions.assertEquals(OLD_VALUE, response.getOldValue());
    Assertions.assertEquals(CREATED_BY, response.getCreatedBy());
  }

  @Test
  void toUiValidationRestrictedKeywordsWebResponseTest() {
    UiValidationRestrictedKeywordsResponse uiValidationRestrictedKeywordsResponse =
        new UiValidationRestrictedKeywordsResponse();
    uiValidationRestrictedKeywordsResponse.setKeyword(KEYWORD);
    uiValidationRestrictedKeywordsResponse.setKeywordId(KEYWORD_ID);
    UiValidationRestrictedKeywordsWebResponse response =
        ResponseHelper.toUiValidationRestrictedKeywordsWebResponse(uiValidationRestrictedKeywordsResponse);
    Assertions.assertEquals(KEYWORD, response.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, response.getKeywordId());
  }

  @Test
  void toRestrictedKeywordsListingWebResponseTest() {
    RestrictedKeywordsListingResponse restrictedKeywordsListingResponse = new RestrictedKeywordsListingResponse();
    restrictedKeywordsListingResponse.setKeyword(KEYWORD);
    restrictedKeywordsListingResponse.setKeywordId(KEYWORD_ID);
    restrictedKeywordsListingResponse.setValidateOnUi(true);
    restrictedKeywordsListingResponse.setValidateByDs(true);
    RestrictedKeywordsListingWebResponse response =
        ResponseHelper.toRestrictedKeywordsListingWebResponse(restrictedKeywordsListingResponse);
    Assertions.assertEquals(KEYWORD, response.getKeyword());
    Assertions.assertEquals(KEYWORD_ID, response.getKeywordId());
    Assertions.assertTrue(response.getValidateOnUi());
  }

  @Test
  void validateResponse_ListBaseResponse_NullResponseTest (){
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((ListBaseResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_ListBaseResponse_SuccessFalseTest (){
    ListBaseResponse listBaseResponse =
        new ListBaseResponse(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID,  new ArrayList<>(), null);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(listBaseResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_ListBaseResponse_ValueNullTest () {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID,  null, null);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(listBaseResponse);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_ListBaseResponse_SuccessTrueTest (){
    ListBaseResponse listBaseResponse =
        new ListBaseResponse(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID,  new ArrayList<>(), null);
    assertTrue(ResponseHelper.validateResponse(listBaseResponse));
  }

  @Test
  void validateCatalogType_successTrueTest() {
    assertTrue(ResponseHelper.validateCatalogType(CatalogType.MASTER_CATALOG.name()));
  }

  @Test
  void validateCatalogTypeSalesCatalogSuccessTrueTest() {
    assertTrue(ResponseHelper.validateCatalogType(CatalogType.SALES_CATALOG.name()));
  }

  @Test
  void validateCatalogTypeB2bSalesCatalogSuccessTrueTest() {
    assertTrue(ResponseHelper.validateCatalogType(CatalogType.B2B_SALES_CATALOG.name()));
  }

  @Test
  void validateCatalogType_SuccessFalseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateCatalogType(REQUEST_ID);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      assertEquals(ErrorCategory.INVALID_FORMAT.getMessage() + ErrorMessages.ERR_INVALID_CATALOG_TYPE,
          exception.getMessage());
    }
  }

  @Test
  void toCategoryDetailResponseListTest() {
    List<CategoryDTO> categoryDTOS = new ArrayList<>();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setName(NAME);
    categoryDTOS.add(categoryDTO);
    List<CategoryWebResponse> categoryWebResponses = ResponseHelper.toCategoryDetailResponseList(categoryDTOS);
    Assertions.assertNotNull(categoryWebResponses);
    Assertions.assertTrue(categoryWebResponses.get(0).getName().equals(NAME));
  }

  @Test
  void toCatalogDetailResponseListTest() {
    List<CatalogResponse> catalogResponses = new ArrayList<>();
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setName(NAME);
    catalogResponses.add(catalogResponse);
    List<CatalogDetailResponse> categoryResponses = ResponseHelper.toCatalogDetailResponseList(catalogResponses);
    Assertions.assertNotNull(categoryResponses);
    Assertions.assertTrue(categoryResponses.get(0).getName().equals(NAME));
  }

  @Test
  void toCategoryInfoWebResponseTestForMasterCategory() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    categoryDetailAndShippingResponse.setSalesCategoryReferences(salesCategoryReferences);
    categoryDetailAndShippingResponse.setCategoryAttributes(categoryAttributes);
    categoryDetailAndShippingResponse.setOriginalSalesCategoryResponse(originalSalesCategoryResponse);
    categoryDetailAndShippingResponse.getSalesCategoryReferences().get(0).setSalesCategoryReference(new CategoryResponse());
    categoryDetailAndShippingResponse.getSalesCategoryReferences().get(0).getSalesCategoryReference().setHalalCategory(true);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getSalesCategoryReference());
    Assertions.assertNull(response.getMasterCategoryReference());
    Assertions.assertTrue(response.getShippingResponses().get(0).isSizeChartRequired());
    Assertions.assertTrue(response.getOriginalSalesCategory().isActivated());
    Assertions.assertEquals(OSC_CODE, response.getOriginalSalesCategory().getOscCode());
    Assertions.assertEquals(OSC_LONG_TEXT, response.getOriginalSalesCategory().getOscLongText());
    Assertions.assertEquals(OSC_SHORT_TEXT, response.getOriginalSalesCategory().getOscShortText());
    Assertions.assertEquals(ID, response.getOriginalSalesCategory().getId());
    Assertions.assertTrue(response.getSalesCategoryReference().get(0).getSalesCategoryResponse().isHalalCategory());
  }

  @Test
  void toCategoryInfoWebResponseForMarkForDeleteSalesCategoryTest() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    salesCategoryReferences.get(0).setMarkForDelete(Boolean.TRUE);
    categoryDetailAndShippingResponse.setSalesCategoryReferences(salesCategoryReferences);
    categoryDetailAndShippingResponse.setCategoryAttributes(categoryAttributes);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getSalesCategoryReference());
    Assertions.assertNull(response.getMasterCategoryReference());
    Assertions.assertEquals(response.getSalesCategoryReference().size(), 0);
  }

  @Test
  void toCategoryInfoWebResponseForMarkForDeleteSalesCategoryTest2() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    salesCategoryReferences.get(0).getSalesCategoryReference().setMarkForDelete(Boolean.TRUE);
    categoryDetailAndShippingResponse.setSalesCategoryReferences(salesCategoryReferences);
    categoryDetailAndShippingResponse.setCategoryAttributes(categoryAttributes);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getSalesCategoryReference());
    Assertions.assertNull(response.getMasterCategoryReference());
    Assertions.assertEquals(response.getSalesCategoryReference().size(), 0);
    Assertions.assertTrue(response.isWholesalePriceConfigEnabled());
  }

  @Test
  void toCategoryInfoWebResponseTestForMasterCategoryWithAttributes() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    categoryDetailAndShippingResponse.setSalesCategoryReferences(salesCategoryReferences);
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setMarkForDelete(Boolean.TRUE);
    attributeResponse.setName(ATT_NAME);
    categoryAttributeResponse.setAttribute(attributeResponse);
    categoryAttributes.add(categoryAttributeResponse);
    categoryDetailAndShippingResponse.setCategoryAttributes(categoryAttributes);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNotNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getSalesCategoryReference());
    Assertions.assertNull(response.getMasterCategoryReference());
    assertEquals(response.getCategoryAttributes().size(), 1);
  }

  @Test
  void toCategoryInfoWebResponseTestForSalesCategory() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    categoryDetailAndShippingResponse.setMasterCategoryReferences(masterCategoryReferences);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getMasterCategoryReference());
    Assertions.assertNull(response.getSalesCategoryReference());
  }

  @Test
  void toCategoryInfoWebResponseTestForMarkForDeleteTrue() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    masterCategoryReferences.get(0).getMasterCategoryReference().setMarkForDelete(Boolean.TRUE);
    categoryDetailAndShippingResponse.setMasterCategoryReferences(masterCategoryReferences);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getMasterCategoryReference());
    Assertions.assertNull(response.getSalesCategoryReference());
    Assertions.assertEquals(response.getMasterCategoryReference().size(), 0);
  }

  @Test
  void toCategoryInfoWebResponseTestForMarkForDeleteTrue2() {
    categoryDetailAndShippingResponse.setCatalog(catalogResponse);
    masterCategoryReferences.get(0).setMarkForDelete(Boolean.TRUE);
    categoryDetailAndShippingResponse.setMasterCategoryReferences(masterCategoryReferences);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getMasterCategoryReference());
    Assertions.assertNull(response.getSalesCategoryReference());
    Assertions.assertEquals(response.getMasterCategoryReference().size(), 0);
  }

  @Test
  void toCategoryInfoWebResponseTestWithNullCatalog() {
    categoryDetailAndShippingResponse.setMasterCategoryReferences(masterCategoryReferences);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getMasterCategoryReference());
    Assertions.assertNull(response.getSalesCategoryReference());
  }

  @Test
  void toCategoryInfoWebResponseTestWithNonNullParentCategoryId() {
    categoryDetailAndShippingResponse.setParentCategoryId(CATEGORY_CODE);
    categoryDetailAndShippingResponse.setMasterCategoryReferences(masterCategoryReferences);
    CategoryInfoWebResponse response = ResponseHelper.toCategoryInfoWebResponse(categoryDetailAndShippingResponse);
    Assertions.assertNotNull(response);
    Assertions.assertNull(response.getCategoryAttributes());
    Assertions.assertNotNull(response.getMasterCategoryReference());
    Assertions.assertNull(response.getSalesCategoryReference());
  }

  @Test
  void toAutoApprovalRulesListWebResponseTest() {
    AutoApprovalRulesListWebResponse response =
        ResponseHelper.toAutoApprovalRulesListWebResponse(autoApprovalRulesListResponse);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(1, response.getAutoApprovalRulesWebResponseList().size());
  }

  @Test
  void toAutoApprovalRulesWebResponseTest() {
    AutoApprovalRulesWebResponse response = ResponseHelper.toAutoApprovalRulesWebResponse(autoApprovalRulesDto);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(RULE_NAME, response.getRuleName());
    Assertions.assertEquals(AUTO_APPROVAL_TYPE, response.getAutoApprovalType());
    Assertions.assertEquals(SEQUENCE, response.getSequenceNumber());
    Assertions.assertEquals(true, response.isMarkForDelete());
    Assertions.assertTrue(response.isNeedRevisionConfigEnabled());
    Assertions.assertEquals(Arrays.asList(autoApprovalRuleDetailWebResponse), response.getImageConfig());
    Assertions.assertEquals(Arrays.asList(autoApprovalRuleDetailWebResponse), response.getRuleConfig());
    Assertions.assertEquals(Arrays.asList(autoApprovalRuleDetailWebResponse), response.getNeedRevisionImageConfig());
  }

  @Test
  void toAutoApprovalRuleDetailWebResponse() {
    AutoApprovalRuleDetailWebResponse response =
        ResponseHelper.toAutoApprovalRuleDetailWebResponse(autoApprovalRuleDetailsDto);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(KEY_NAME, response.getKeyName());
    Assertions.assertEquals(VALUE_AUTO, response.getValue());
    Assertions.assertEquals(VALUE_TYPE, response.getValueType());
    Assertions.assertEquals(OPERATOR, response.getOperator());
  }

  @Test
  void toCategoryMappingResponseTest() {
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTO.setName(NAME);
    CategoryMappingResponse categoryMappingResponse = ResponseHelper.toCategoryMappingResponse(categoryDTO);
    Assertions.assertEquals(categoryMappingResponse.getCategoryCode(), CATEGORY_CODE);
    Assertions.assertEquals(categoryMappingResponse.getName(), NAME);
  }

  @Test
  void marginResponseValidation_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.marginResponseValidation((GdnRestSingleResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void categoryShippingValidation_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.categoryShippingValidation((GdnRestListResponse) null);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void marginResponseValidation_CustomExceptionTest() {
    boolean response = ResponseHelper
        .marginResponseValidation(new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, RESPONSE, REQUEST_ID));
    Assertions.assertEquals(Boolean.FALSE, response);

  }

  @Test
  void categoryShippingValidation_CustomExceptionTest() {
    boolean response = ResponseHelper.categoryShippingValidation(
        new GdnRestListResponse<>(ERROR_MESSAGE, ERROR_CODE, false, Collections.singletonList(RESPONSE),
            new PageMetaData(0, 10, 100), REQUEST_ID));
    Assertions.assertEquals(Boolean.FALSE, response);
  }

  @Test
  void toCreateCategoryWebResponseTest() {
    CreateCategoryResponse createCategoryResponse = new CreateCategoryResponse();
    createCategoryResponse.setCategoryCode(CATEGORY_CODE);
    CreateCategoryWebResponse categoryMappingResponse = ResponseHelper.toCreateCategoryWebResponse(createCategoryResponse);
    Assertions.assertEquals(categoryMappingResponse.getCategoryCode(), CATEGORY_CODE);
  }

  @Test
  void toAttributeValuesWebResponseListTest() {
    List<AttributeValuesWebResponse> responseList = ResponseHelper.toAttributeValuesWebResponseList(attributeResponses);
    Assertions.assertNotNull(responseList);
    assertEquals(ATTRIBUTE_CODE, responseList.get(0).getAttributeCode());
  }

  @Test
  void toMarginCategoryWebResponseTest(){
    MarginCategoryWebResponse response = ResponseHelper.toMarginCategoryWebResponse(marginCategoryResponse);
    Assertions.assertEquals(CATEGORY_CODE, response.getCategoryCode());
    Assertions.assertEquals(MARGIN_NOTE, response.getNote());
    Assertions.assertEquals(VALUE, response.getValue());
  }

  @Test
  void toMarginBusinessPartnerWebResponseTest(){
    MarginCategoryWebResponse response = ResponseHelper.getMarginBusinessPartnerWebResponse(this.marginOrderResponse);
    Assertions.assertEquals(VALUE,response.getValue());
    Assertions.assertEquals(TRANSACTION_FEE,response.getTransactionFee());
  }

  @Test
  void toAttributeValueWebResponseTest() {
    AttributeValueWebResponse attributeValueWebResponse =
        ResponseHelper.toAttributeValueWebResponse(predefinedAllowedAttributeValueResponse);
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE_VALUE, attributeValueWebResponse.getValue());
    Assertions.assertEquals(ID, attributeValueWebResponse.getId());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        attributeValueWebResponse.getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(SEQUENCE, attributeValueWebResponse.getSequence());
  }

  @Test
  void toAttributeDetailWebResponseTest() {
    AttributeDetailWebResponse attributeDetailWebResponse =
        ResponseHelper.toAttributeDetailWebResponse(attributeResponse);
    assertEquals(ATTRIBUTE_CODE, attributeDetailWebResponse.getAttributeCode());
    assertEquals(allowedAttributeValueResponse.getAllowedAttributeCode(),
        attributeDetailWebResponse.getAttributeValues().get(0).getAllowedAttributeCode());
    assertEquals(predefinedAllowedAttributeValueResponse.getValue(),
        attributeDetailWebResponse.getAttributeValues().get(1).getValue());
  }

  @Test
  void getRestrictedKeywordsWebResponseTest() {
    restrictedKeywordsResponse.setType(CONFIGURATION_TYPE);
    restrictedKeywordsResponse.setDestinationCategoryName(NAME);
    restrictedKeywordsResponse.setDestinationCategoryEnglishName(NAME_ENGLISH);
    RestrictedKeywordsWebResponse restrictedKeywordsWebResponse =
        ResponseHelper.getRestrictedKeywordsWebResponse(restrictedKeywordsResponse);
    Assertions.assertNotNull(restrictedKeywordsWebResponse);
    assertEquals(KEYWORD, restrictedKeywordsWebResponse.getKeyword());
    assertEquals(KEYWORD_ID, restrictedKeywordsWebResponse.getKeywordId());
    assertEquals(CONFIGURATION_TYPE, restrictedKeywordsWebResponse.getType());
    assertEquals(NAME, restrictedKeywordsWebResponse.getDestinationCategoryName());
    assertEquals(NAME_ENGLISH, restrictedKeywordsWebResponse.getDestinationCategoryEnglishName());
  }

  @Test
  void getWholesaleMappingWebResponseTest() {
    WholesaleMappingWebResponse wholesaleMappingWebResponse =
        ResponseHelper.getWholesaleMappingWebResponse(wholesaleMappingResponse);
    Assertions.assertNotNull(wholesaleMappingWebResponse);
    assertEquals(CONFIGURATION_TYPE, wholesaleMappingWebResponse.getConfigurationType());
    assertEquals(WHOLESALE_CONFIG_ENABLED, wholesaleMappingWebResponse.isWholesalePriceConfigEnabled());
  }

  @Test
  void getCatalogTreeWebResponseTest() {
    catalogResponse.setCatalogCode(CATALOG_CODE);
    categoryResponse.setCatalog(catalogResponse);
    categoryResponse.setId(CATEGORY_ID);
    categoryResponse.setNameEnglish(CATEGORY_NAME);
    List<CatalogTreeWebResponse> responseList = ResponseHelper
        .getCatalogTreeWebResponse(Collections.singletonList(Collections.singletonList(categoryResponse)));
    Assertions.assertNotNull(responseList);
    assertEquals(CATALOG_CODE, responseList.get(0).getCatalogCode());
    assertEquals(1, responseList.get(0).getCategories().get(0).getLevel());
    assertEquals(CATEGORY_ID, responseList.get(0).getCategories().get(0).getCategoryId());
    assertEquals(CATEGORY_NAME, responseList.get(0).getCategories().get(0).getCategoryNameEnglish());
  }

  @Test
  void getCatalogTreeWebResponse_EmptyTest() {
    List<CatalogTreeWebResponse> responseList =
        ResponseHelper.getCatalogTreeWebResponse(Collections.singletonList(new ArrayList<>()));
    Assertions.assertNotNull(responseList);
  }

  @Test
  void getCatalogTreeWebResponse_NullTest() {
    try {
      List<CatalogTreeWebResponse> responseList = ResponseHelper.getCatalogTreeWebResponse(null);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex.getErrorMessage());
    }
  }

  @Test
  void toPredictedCategoriesWebResponseTest() {
    List<SuggestedCategoriesWebResponse> suggestedCategoriesWebRespons =
        ResponseHelper.toPredictedCategoriesWebResponse(productCategoryPredictionResponse);
    assertEquals("1", suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryLevel());
    assertEquals(CATEGORY_CODE, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_ID, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryId());
    assertEquals(CATEGORY_NAME, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryName());
    assertEquals(CATEGORY_NAME, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryNameEnglish());
  }

  @Test
  void toPredictedCategoriesWebResponseEmptyTest() {
    List<SuggestedCategoriesWebResponse> suggestedCategoriesWebRespons =
        ResponseHelper.toPredictedCategoriesWebResponse(new ProductCategorySuggestionResponse());
    assertTrue(suggestedCategoriesWebRespons.isEmpty());
  }

  @Test
  void validateResponse_GdnRestSimpleResponse_NullResponseTest() {
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse((GdnRestSimpleResponse) null);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestSimpleResponse_SuccessFalseTest() {
    GdnRestSimpleResponse<String> gdnRestSingleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, REQUEST_ID, ID);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSingleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestSimpleResponse_ValueNullTest() {
    GdnRestSimpleResponse<String> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID, null);
    Exception exception = new Exception();
    try {
      ResponseHelper.validateResponse(gdnRestSimpleResponse);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ClientException.class, exception.getClass());
      assertEquals(ERROR_MESSAGE, exception.getMessage());
    }
  }

  @Test
  void validateResponse_GdnRestSimpleResponse_SuccessTrueTest() {
    GdnRestSimpleResponse<String> gdnRestSimpleResponse =
        new GdnRestSimpleResponse<>(ERROR_MESSAGE, ERROR_CODE, true, REQUEST_ID, ID);
    assertTrue(ResponseHelper.validateResponse(gdnRestSimpleResponse));
  }

  @Test
  void toBaseMarginHierarchyWebResponseEmptyResponseTest() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setCategoryCode(CATEGORY_ID);
    List<BaseMarginHierarchyWebResponse> response =
        ResponseHelper.toBaseMarginHierarchyWebResponse(Arrays.asList(categoryResponse,
            categoryResponse1), Arrays.asList(baseMarginResponse, baseMarginResponse1));
    assertEquals(2, response.size());
    assertEquals(1, response.get(0).getLevel());
    assertEquals(2, response.get(1).getLevel());
    assertEquals(categoryResponse.getCategoryCode(), response.get(1).getCategoryCode());
    assertEquals(categoryResponse.getName(), response.get(1).getCategoryName());
    assertEquals(categoryResponse.getNameEnglish(), response.get(1).getCategoryNameEnglish());
    assertEquals(PERCENTAGE, response.get(1).getMarginDetails().get(0).getMarginPercentage(), 0);
    assertEquals(new ArrayList<>(), response.get(0).getMarginDetails());
  }



  @Test
  void getMarginBusinessPartnerWebResponseNullTest() {
    ResponseHelper.getMarginBusinessPartnerWebResponse(null, CATEGORY_CODE);
  }

  @Test
  void getMarginBusinessPartnerWebResponseMarginAsTest() {
    Optional<OrderItemMarginsResponse> orderItemMarginsResponse = Optional.of(new OrderItemMarginsResponse());
    List<Margin> marginList = new ArrayList<>();
    marginList.add(null);
    orderItemMarginsResponse.get().setMargins(marginList);
    ResponseHelper.getMarginBusinessPartnerWebResponse(orderItemMarginsResponse, CATEGORY_CODE);
  }

  @Test
  void toProfitMarginWebResponseTest() {
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(79.0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(79.0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertEquals(PERCENTAGE, profitMarginWebResponse.getCommissionPercentage(), 0);
    Assertions.assertEquals(20.0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void toProfitMarginWebResponseNullCheckTest() {
    marginCategoryWebResponse.setTransactionFee(null);
    marginCategoryWebResponse.setMinimumValue(null);
    marginCategoryWebResponse.setMaximumValue(null);
    marginCategoryWebResponse.setValue(null);
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(100.0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(100.0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertNull(profitMarginWebResponse.getCommissionPercentage());
    Assertions.assertEquals(0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void toProfitMarginWebResponseMinCheckTest() {
    marginCategoryWebResponse.setMinimumValue(99.0);
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(0.0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(0.0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertEquals(PERCENTAGE, profitMarginWebResponse.getCommissionPercentage(), 0.0);
    Assertions.assertEquals(99.0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void toProfitMarginWebResponseMaxCheckTest() {
    marginCategoryWebResponse.setMaximumValue(10.0);
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(89.0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(89.0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertEquals(PERCENTAGE, profitMarginWebResponse.getCommissionPercentage(), 0.0);
    Assertions.assertEquals(10.0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void toProfitMarginWebResponseMaxCheckSellingPriceZeroTest() {
    marginCategoryWebResponse.setMaximumValue(10.0);
    profitMarginWebRequest.setSellingPrice(0.0);
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertEquals(PERCENTAGE, profitMarginWebResponse.getCommissionPercentage(), 0.0);
    Assertions.assertEquals(0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void toProfitMarginWebResponseMaxCheckBasicProductPriceZeroTest() {
    marginCategoryWebResponse.setMaximumValue(10.0);
    profitMarginWebRequest.setBasicProductPrice(0.0);
    ProfitMarginWebResponse profitMarginWebResponse =
        ResponseHelper.toProfitMarginWebResponse(profitMarginWebRequest, marginCategoryWebResponse);
    Assertions.assertEquals(0, profitMarginWebResponse.getProfitMargin(), 0);
    Assertions.assertEquals(0, profitMarginWebResponse.getProfitMarginPercentage(), 0);
    Assertions.assertEquals(PERCENTAGE, profitMarginWebResponse.getCommissionPercentage(), 0.0);
    Assertions.assertEquals(0, profitMarginWebResponse.getCommission(), 0);
  }

  @Test
  void roundToPrecisionTest() {
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_1), NUM_PRECISION_1, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_2), NUM_PRECISION_2, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_3), NUM_PRECISION_3, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_4), NUM_PRECISION_4, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_5), NUM_PRECISION_5, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_6), NUM_PRECISION_6, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_7), NUM_PRECISION_7, 0);
    Assertions.assertEquals(ResponseHelper.roundToPrecision(NUM_8), NUM_PRECISION_8, 0);
  }

  @Test
  void toWholesaleMappingWebResponse() {
    WholesaleMappingWebResponse wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    ResponseHelper.toWholesaleMappingWebResponse(wholesaleMappingResponse,
        wholesaleMappingWebResponse);
    Assertions.assertTrue(wholesaleMappingWebResponse.isWholesalePriceConfigEnabled());
    Assertions.assertEquals(CONFIGURATION_TYPE, wholesaleMappingWebResponse.getConfigurationType());
    Assertions.assertEquals(1, wholesaleMappingWebResponse.getWholesaleConfig().size());
  }

  @Test
  void toWholesaleMappingWebResponse_nullWholesaleMappingResponse() {
    WholesaleMappingWebResponse wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    ResponseHelper.toWholesaleMappingWebResponse(null,
        wholesaleMappingWebResponse);
    Assertions.assertNull(wholesaleMappingWebResponse.getConfigurationType());
    Assertions.assertEquals(0, wholesaleMappingWebResponse.getWholesaleConfig().size());
  }

  @Test
  void toWholesaleMappingWebResponse_nullWholesaleConfigResponse() {
    WholesaleMappingWebResponse wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    wholesaleMappingResponse.setWholesaleConfig(Collections.singletonList(null));
    ResponseHelper.toWholesaleMappingWebResponse(wholesaleMappingResponse,
        wholesaleMappingWebResponse);
    Assertions.assertNull(wholesaleMappingWebResponse.getWholesaleConfig().get(0));
  }

  @Test
  void toWholesaleMappingWebResponse_nullMinWholesaleDiscountResponse() {
    WholesaleMappingWebResponse wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    wholesaleMappingResponse.getWholesaleConfig().get(0)
        .setMinWholesaleDiscount(Collections.singletonList(null));
    ResponseHelper.toWholesaleMappingWebResponse(wholesaleMappingResponse,
        wholesaleMappingWebResponse);
    Assertions.assertNull(
        wholesaleMappingWebResponse.getWholesaleConfig().get(0).getMinWholesaleDiscount().get(0));
  }

}
