package com.gdn.partners.pcu.master.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.master.client.model.CategoryDetailAndShippingResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.margin.webmodel.GdnRestSimpleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.feign.MarginFeign;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.feign.ProductCategorySuggestionFeign;
import com.gdn.partners.pcu.master.client.feign.XBPFeign;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategoryCodesListRequest;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.CategorySuggestionResponse;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.Margin;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;
import com.gdn.partners.pcu.master.client.model.SuggestedCategoriesResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.model.request.CategoryAttributeUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryKeywordsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.model.request.MinWholesaleDiscountServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleConfigServiceRequest;
import com.gdn.partners.pcu.master.model.request.WholesaleMappingServiceRequest;
import com.gdn.partners.pcu.master.service.impl.exception.ActivationValidationException;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ProfitMarginWebRequest;
import com.gdn.partners.pcu.master.web.model.response.CatalogTreeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryInfoWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryMappingResponse;
import com.gdn.partners.pcu.master.web.model.response.DocumentWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscDetailsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscSummaryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordAction;
import com.gdn.partners.pcu.master.web.model.response.SuggestedCategoriesWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRestrictedKeywordsRequest;
import com.gdn.x.productcategorybase.dto.request.MinWholesaleDiscountRequest;
import com.gdn.x.productcategorybase.dto.request.OriginalSalesCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleConfigRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.CreateCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.OscSummaryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

public class CategoryServiceImplTest {
  private static final String DEFAULT_STORE_ID = "12345";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_CATEGORY_CODE = "CGC-001";
  private static final String DEFAULT_CATEGORY_ID = "CGI-001";
  private static final String DEFAULT_CATEGORY_ID_2 = "CGI-002";
  private static final String DEFAULT_CATALOG_TYPE = "MASTER_CATALOG";
  private static final String DEFAULT_CREATED_BY = "Creator";
  private static final Integer DEFAULT_LOGISTIC_ADJUSTMENT = 1;
  private static final String DEFAULT_NAME = "Name";
  private static final Integer DEFAULT_SEQUENCE = 1;
  private static final Long DEFAULT_VERSION = 1L;
  private static final String DEFAULT_DESCRIPTION = "Description";
  private static final String DEFAULT_STATE = "State";
  private static final String DEFAULT_UPDATED_BY = "Updator";
  private static final String DEFAULT_ATTRIBUTE_CODE = "ATC-001";
  private static final String DEFAULT_ATTRIBUTE_TYPE = "DESCRIPTIVE_ATTRIBUTE";
  private static final String DEFAULT_ATTRIBUTE_ID = "AID-001";
  private static final String STORE_ID = "storeId";
  private static final String UPDATED_BY = "userName";
  private static final Date UPDATED_DATE = new Date();
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String ID = "id";
  private static final String PARENT_CATEGORY_ID = "parentCategoryId";
  private static final String PARENT_CATEGORY_ID_1 = "parentCategoryId1";
  private static final String NAME = "name";
  private static final String NAME_1 = "apple";
  private static final String NAME_2 = "apple1";
  private static final String NAME_ENGLISH = "nameEnglish";
  private static final String NAME_ENGLISH_1 = "nameEnglish1";
  private static final String NAME_ENGLISH_2 = "nameEnglish2";
  private static final byte[] DESCRIPTION = "defaultDescription".getBytes();
  private static final byte[] DESCRIPTION_ENGLISH = "descriptionEnglish".getBytes();
  private static final Integer INTERNAL_ACTIVATION_INTERVAL = 100;
  private static final Integer LOGISTIC_ADJUSTMENT = 10;
  private static final Integer SEQUENCE = 5;
  private static final String CATEGORY_ID = "categoryId";
  private static final String OSC_CODE = "oscCode";
  private static final String OSC_TEXT = "anyText";
  private static final Boolean ACTIVATED = true;
  private static final String CATEGORY_ID_1 = "categoryId1";
  private static final String SALES_CATALOG = "sales";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String MASTER_CATEGORY_ID_1 = "masterCategoryId1";
  private static final String MASTER_CATEGORY_ID_2 = "masterCategoryId2";
  private static final String CATALOG_ID = "catalogId";
  private static final String REQUEST_ID = "request_id";
  private static final String PARENT_ID = "parent_id";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = Integer.MAX_VALUE;
  private static final Integer TOTAL_RECORDS = Integer.MAX_VALUE;
  private static final String FILTER_TYPE = "ACTIVE";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String DESCRIPTION_EMPTY_ERROR_MESSAGE =
      "Anda tidak berhasil mengaktifkan kategori categoryName karena kategori deskripsi tidak boleh kosong";
  private static final String MARGIN_NOT_SET_ERROR_MESSAGE =
      "Anda tidak berhasil mengaktifkan kategori categoryName karena kategori belum memiliki margin";
  private static final String CATEGORY_SHIPPING_CODE_NOT_SET =
      "Anda tidak berhasil mengaktifkan kategori categoryName karena kategori belum memiliki shipping code";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String VALUE_TYPE = "VALUE_TYPE";
  private static final int DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final String KEYWORD = "keyword";
  private static final String DESTINATION_CATEGORY = "destination_category";
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD1 = "keyword1";
  private static final String DESTINATION_CATEGORY1 = "destination_category1";
  private static final String KEYWORD_ID1 = "keywordId1";
  private static final String CONFIGURATION_TYPE = "PRICE_PERCENTAGE";
  private static final Integer QUANTITY = 10;
  private static final Double PRICE = 10000.0;
  private static final Double PERCENTAGE = 10D;
  private static final boolean WHOLESALE_CONFIG_ENABLED = true;
  private static final String CATEGORY_CODE_2 = "category_code_2";
  private static final String CHILD_CATEGORY_NAME = "child_category_name";
  private static final String DOCUMENT_FILTER_TYPE = "ALL";
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private static final String DOCUMENT_TYPE = "documentType";
  private static final String DOCUMENT_DESCRIPTION = "Document list for category";
  private static final String PRODUCT_NAME = "product_name";
  private static final String OSC_SHORT_TEXT = "oscShortText";
  private static final String OSC_LONG_TEXT = "oscLongText";
  public static final String STRING = "STRING";
  public static final String RESTRICTED_KEYWORD_TYPE_LIST =
      "[{\"enName\":\"Used Product\",\"inName\":\"Produk Bekas\",\"value\":0},{\"enName\":\"Competitor's name\",\"inName\":\"Nama pesaing\",\"value\":0},{\"enName\":\"Doctor's prescription\",\"inName\":\"Resep dokter\",\"value\":0},{\"enName\":\"Illegal drugs\",\"inName\":\"Obat-obatan terlarang\",\"value\":0},{\"enName\":\"Fake product\",\"inName\":\"Produk palsu\",\"value\":0},{\"enName\":\"Cigarette\",\"inName\":\"Rokok\",\"value\":0},{\"enName\":\"Google Voucher\",\"inName\":\"Google Voucher\",\"value\":0},{\"enName\":\"Gambling\",\"inName\":\"Perjudian\",\"value\":0},{\"enName\":\"Others\",\"inName\":\"Lain\",\"value\":0}]";
  public static final String RESTRICTED_KEYWORD_ACTION_LISTS =
      "[{\"enName\":\"Straight rejection\",\"inName\":\"Straight rejection\",\"value\":3},{\"enName\":\"Need revision\",\"inName\":\"Need revision\",\"value\":2},{\"enName\":\"Change category to\",\"inName\":\"Change category to\",\"value\":1},{\"enName\":\"No action\",\"inName\":\"No action\",\"value\":0}]";
  public static final String STRAIGHT_REJECTION = "Straight rejection";
  public static final String NEED_REVISION = "Need revision";
  public static final String CHANGE_CATEGORY_TO = "Change category to";
  public static final String NO_ACTION = "No action";
  public static final String CLIENT_ID = "ClientId";
  public static final String CHANNEL_ID = "channelId";
  public static final String USERNAME = "username";

  private GdnRestListResponse<CategoryDTO> categoryDTOGdnRestListResponse;
  private List<CategoryDTO> categoryDTOS;
  private CategoryDTO categoryDTO;
  private PageMetaData pageMetaData;
  private CategoryResponse categoryResponse;
  private CatalogResponse catalogResponse;
  private List<CatalogResponse> catalogResponses;
  private GdnRestListResponse<CatalogResponse> catalogResponseGdnRestListResponse;
  private GdnRestListResponse<CategoryHierarchyResponse> categoryHierarchyResponses;
  private List<CategoryResponse> categoryResponseList;
  private List<CategoryHierarchyResponse> categoryHierarchyResponseList;
  private List<OscSummaryResponse> oscSummaryResponseList = new ArrayList<>();
  private OscSummaryResponse oscSummaryResponse;
  private CategoryHierarchyResponse categoryHierarchyResponse1;
  private CategoryDetailAndShippingResponse categoryDetailAndShippingResponse;
  private GdnRestSingleResponse<CategoryDetailAndShippingResponse> response;
  private CategoryMappingsUpdateServiceRequest categoryMappingsUpdateServiceRequest;
  private CategoryCreateServiceRequest categoryCreateServiceRequest;
  private CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest;
  private CategoryCodeRequest categoryCodeRequest;
  private MarginCategoryResponse marginCategoryResponse;
  private MarginOrderResponse marginOrderResponse;
  private GdnRestListResponse<ProductSummaryResponse> productSummaryResponseGdnRestListResponse;
  private List<ProductSummaryResponse> productSummaryResponses;
  private ProductSummaryResponse productSummaryResponse;
  private CategoryKeywordsUpdateRequest categoryKeywordsUpdateRequest;
  private CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList;
  private CategoryKeywordsUpdateServiceRequest categoryKeywordsUpdateServiceRequest;
  private CategoryKeywordsUpdateServiceRequest categoryKeywordsUpdateServiceRequest1;
  private Pageable pageable = PageRequest.of(0, 10);
  private RestrictedKeywordsResponse restrictedKeywordsResponse;
  private CategoryRestrictedKeywordsRequest categoryRestrictedKeywordsRequest;
  private CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest;
  private WholesaleMappingServiceRequest wholesaleMappingServiceRequest;
  private MinWholesaleDiscountServiceRequest minWholesaleDiscountServiceRequest;
  private WholesaleConfigServiceRequest wholesaleConfigServiceRequest;
  private WholesaleMappingResponse wholesaleMappingResponse;
  private WholesaleConfigResponse wholesaleConfigResponse;
  private MinWholesaleDiscountResponse minWholesaleDiscountResponse;
  private WholesaleMappingRequest wholesaleMappingRequest;
  private OscInfoUpdateDTO oscInfoUpdateDTO;
  private WholesaleConfigRequest wholesaleConfigRequest;
  private MinWholesaleDiscountRequest minWholesaleDiscountRequest;
  private CategoryTreeResponse categoryTreeResponse;
  private List<CategoryTreeResponse> children;
  private GdnRestListResponse<CategoryTreeResponse> categoryTreeResponseGdnRestListResponse;
  private SystemParameterResponse systemParameterResponse;
  private GdnRestSingleResponse<SystemParameterResponse> systemParameterResponseGdnRestSingleResponse;
  private ProductCategorySuggestionResponse productCategoryPredictionResponse;
  private OriginalSalesCategoryWebRequest originalSalesCategoryWebRequest;
  private OriginalSalesCategoryRequest originalSalesCategoryRequest;
  private OriginalSalesCategoryResponse originalSalesCategoryResponse;
  private RestrictedKeywordAction restrictedKeywordAction1;
  private RestrictedKeywordAction restrictedKeywordAction2;
  private RestrictedKeywordAction restrictedKeywordAction3;
  private RestrictedKeywordAction restrictedKeywordAction4;
  private List<RestrictedKeywordAction> restrictedKeywordActionList;
  private RestrictedKeywordAction restrictedKeywordAction5;
  private RestrictedKeywordAction restrictedKeywordAction6;
  private RestrictedKeywordAction restrictedKeywordAction7;
  private RestrictedKeywordAction restrictedKeywordAction8;
  private RestrictedKeywordAction restrictedKeywordAction9;
  private RestrictedKeywordAction restrictedKeywordAction10;
  private RestrictedKeywordAction restrictedKeywordAction11;
  private RestrictedKeywordAction restrictedKeywordAction12;
  private RestrictedKeywordAction restrictedKeywordAction13;
  List<RestrictedKeywordAction> restrictedKeywordTypeList;
  private BaseMarginResponse baseMarginResponse;
  private ProfileResponse profileResponse;
  private ProfitMarginWebRequest profitMarginWebRequest;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @InjectMocks
  private CategoryServiceImpl categoryService;

  @Captor
  private ArgumentCaptor<CategoryInfoUpdateRequest> categoryInfoUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryMappingsUpdateRequest> categoryMappingsUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryDetailRequest> categoryDetailRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryCodeRequest> categoryCodeRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<FilterMarginsByOrderItemsRequest> marginOrderItemArgumentCaptor;

  @Mock
  private MarginFeign marginFeign;

  @Mock
  private XBPFeign xbpFeign;

  @Mock
  private ProductCategorySuggestionFeign productCategorySuggestionFeign;

  @Mock
  private ObjectMapper objectMapper = new ObjectMapper();

  private ObjectMapper mapper = new ObjectMapper();

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(categoryService, "restrictedKeywordTypeList", RESTRICTED_KEYWORD_TYPE_LIST);
    ReflectionTestUtils.setField(categoryService, "restrictedKeywordActionLists", RESTRICTED_KEYWORD_ACTION_LISTS);
    restrictedKeywordAction1 = new RestrictedKeywordAction();
    restrictedKeywordAction2 = new RestrictedKeywordAction();
    restrictedKeywordAction3 = new RestrictedKeywordAction();
    restrictedKeywordAction4 = new RestrictedKeywordAction();
    restrictedKeywordAction5 = new RestrictedKeywordAction();
    restrictedKeywordAction6 = new RestrictedKeywordAction();
    restrictedKeywordAction7 = new RestrictedKeywordAction();
    restrictedKeywordAction8 = new RestrictedKeywordAction();
    restrictedKeywordAction9 = new RestrictedKeywordAction();
    restrictedKeywordAction10 = new RestrictedKeywordAction();
    restrictedKeywordAction11 = new RestrictedKeywordAction();
    restrictedKeywordAction12 = new RestrictedKeywordAction();
    restrictedKeywordAction13 = new RestrictedKeywordAction();
    restrictedKeywordTypeList = new ArrayList<>();
    restrictedKeywordActionList = new ArrayList<>();

    restrictedKeywordAction1.setEnName(STRAIGHT_REJECTION);
    restrictedKeywordAction1.setInName(STRAIGHT_REJECTION);
    restrictedKeywordAction1.setValue(3);
    restrictedKeywordAction2.setEnName(NEED_REVISION);
    restrictedKeywordAction2.setInName(NEED_REVISION);
    restrictedKeywordAction2.setValue(2);
    restrictedKeywordAction3.setEnName(CHANGE_CATEGORY_TO);
    restrictedKeywordAction3.setInName(CHANGE_CATEGORY_TO);
    restrictedKeywordAction3.setValue(1);
    restrictedKeywordAction4.setEnName(NO_ACTION);
    restrictedKeywordAction4.setInName(NO_ACTION);
    restrictedKeywordAction4.setValue(0);
    restrictedKeywordActionList.add(restrictedKeywordAction1);
    restrictedKeywordActionList.add(restrictedKeywordAction2);
    restrictedKeywordActionList.add(restrictedKeywordAction3);
    restrictedKeywordActionList.add(restrictedKeywordAction4);
    restrictedKeywordAction5.setEnName("Used Product");
    restrictedKeywordAction5.setInName("Produk Bekas");
    restrictedKeywordAction5.setValue(0);
    restrictedKeywordAction6.setEnName("Competitor's name");
    restrictedKeywordAction6.setInName("Nama pesaing");
    restrictedKeywordAction6.setValue(0);
    restrictedKeywordAction7.setEnName("Doctor's prescription");
    restrictedKeywordAction7.setInName("Resep dokter");
    restrictedKeywordAction7.setValue(0);
    restrictedKeywordAction8.setEnName("Illegal drugs");
    restrictedKeywordAction8.setInName("Obat-obatan terlarang");
    restrictedKeywordAction8.setValue(0);
    restrictedKeywordAction9.setEnName("Fake product");
    restrictedKeywordAction9.setInName("Produk palsu");
    restrictedKeywordAction9.setValue(0);
    restrictedKeywordAction10.setEnName("Cigarette");
    restrictedKeywordAction10.setInName("Rokok");
    restrictedKeywordAction10.setValue(0);
    restrictedKeywordAction11.setEnName("Google Voucher");
    restrictedKeywordAction11.setInName("Google Voucher");
    restrictedKeywordAction11.setValue(0);
    restrictedKeywordAction12.setEnName("Gambling");
    restrictedKeywordAction12.setInName("Perjudian");
    restrictedKeywordAction12.setValue(0);
    restrictedKeywordAction13.setEnName("Others");
    restrictedKeywordAction13.setInName("Lain");
    restrictedKeywordAction13.setValue(0);
    restrictedKeywordTypeList.add(restrictedKeywordAction5);
    restrictedKeywordTypeList.add(restrictedKeywordAction6);
    restrictedKeywordTypeList.add(restrictedKeywordAction7);
    restrictedKeywordTypeList.add(restrictedKeywordAction8);
    restrictedKeywordTypeList.add(restrictedKeywordAction9);
    restrictedKeywordTypeList.add(restrictedKeywordAction10);
    restrictedKeywordTypeList.add(restrictedKeywordAction11);
    restrictedKeywordTypeList.add(restrictedKeywordAction12);
    restrictedKeywordTypeList.add(restrictedKeywordAction13);
    categoryDetailAndShippingResponse = new CategoryDetailAndShippingResponse();
    marginCategoryResponse = new MarginCategoryResponse();
    marginCategoryResponse.setCategoryId(CATEGORY_CODE);
    categoryDetailAndShippingResponse.setName(NAME);
    categoryDetailAndShippingResponse.setId(CATEGORY_ID);
    categoryDetailAndShippingResponse.setUmkm(true);
    categoryDetailAndShippingResponse.setWholesalePriceConfigEnabled(true);
    response = new GdnRestSingleResponse<>(null, ErrorMessage.ERROR_IN_FETCHING_CATEGORY_INFO.getMessage());
    CategoryAttributeUpdateServiceRequest categoryAttributeUpdateServiceRequest1 =
        CategoryAttributeUpdateServiceRequest.builder().attributeId(ATTRIBUTE_ID_1)
            .mainDefiningAttribute(true).usp(false).sequence(1).build();
    CategoryAttributeUpdateServiceRequest categoryAttributeUpdateServiceRequest2 =
        CategoryAttributeUpdateServiceRequest.builder().attributeId(ATTRIBUTE_ID_2)
            .mainDefiningAttribute(false).usp(false).sequence(2).build();
    categoryKeywordsUpdateServiceRequest =
        CategoryKeywordsUpdateServiceRequest.builder().keyword(KEYWORD).destinationCategory(DESTINATION_CATEGORY)
            .keywordId(KEYWORD_ID).build();
    categoryKeywordsUpdateServiceRequest1 =
        CategoryKeywordsUpdateServiceRequest.builder().keyword(KEYWORD1).destinationCategory(DESTINATION_CATEGORY1)
            .keywordId(KEYWORD_ID1).build();
    wholesaleMappingServiceRequest = WholesaleMappingServiceRequest.builder().configurationType(CONFIGURATION_TYPE)
        .wholesaleConfig(Collections.singletonList(wholesaleConfigServiceRequest)).build();
    categoryMappingsUpdateServiceRequest = CategoryMappingsUpdateServiceRequest.builder().id(ID)
        .addedAttributes(Collections.singletonList(categoryAttributeUpdateServiceRequest1))
        .deletedAttributes(Collections.singletonList(categoryAttributeUpdateServiceRequest2))
        .addedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_1))
        .deletedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_2))
        .addedKeywords(Collections.singletonList(categoryKeywordsUpdateServiceRequest))
        .deletedKeywords(Collections.singletonList(categoryKeywordsUpdateServiceRequest1))
        .wholesaleMapping(wholesaleMappingServiceRequest)
        .updatedBy(UPDATED_BY).storeId(STORE_ID).updatedDate(UPDATED_DATE).build();

    categoryInfoUpdateServiceRequest =
        CategoryInfoUpdateServiceRequest.builder().id(ID).categoryCode(CATEGORY_CODE).name(NAME)
            .nameEnglish(NAME_ENGLISH).defaultDescription(DESCRIPTION).descriptionEnglish(DESCRIPTION_ENGLISH)
            .display(true).deliveredByMerchant(false).internalActivationInterval(INTERNAL_ACTIVATION_INTERVAL)
            .logisticAdjustment(LOGISTIC_ADJUSTMENT).directFlight(true).specialHandling(true).sequence(SEQUENCE)
            .warranty(true).needIdentity(true).parentCategoryId(PARENT_CATEGORY_ID).updatedBy(UPDATED_BY)
            .storeId(STORE_ID).ageLimit(true).updatedDate(UPDATED_DATE).umkm(true).wholesalePriceConfigEnabled(false)
            .sizeChartRequired(true).build();
    categoryCreateServiceRequest = CategoryCreateServiceRequest.builder()
        .categoryInfoUpdateServiceRequest(categoryInfoUpdateServiceRequest)
        .categoryMappingsUpdateServiceRequest(categoryMappingsUpdateServiceRequest)
        .catalogId(CATALOG_ID).storeId(STORE_ID).createdBy(UPDATED_BY).createdDate(UPDATED_DATE)
        .build();
    categoryDTOS = new ArrayList<>();
    categoryDTO = new CategoryDTO();
    categoryDTO.setParentCategoryId(PARENT_ID);
    categoryDTO.setSequence(PAGE);
    categoryDTO.setName(NAME);
    catalogResponse = new CatalogResponse();
    catalogResponse.setId(CATALOG_ID);
    catalogResponse.setCatalogCode(CATALOG_ID);
    categoryDTO.setCatalog(catalogResponse);
    categoryDTO.setCategoryCode(CATEGORY_CODE);
    categoryDTO.setNameEnglish(NAME_ENGLISH);
    categoryDTOS.add(categoryDTO);
    pageMetaData = new PageMetaData(PAGE.longValue(), SIZE.longValue(), TOTAL_RECORDS.longValue());
    catalogResponses = new ArrayList<>();
    catalogResponses.add(catalogResponse);
    catalogResponseGdnRestListResponse = new GdnRestListResponse<>(catalogResponses, pageMetaData, REQUEST_ID);
    categoryDTOGdnRestListResponse = new GdnRestListResponse<>(categoryDTOS, pageMetaData, REQUEST_ID);
    categoryResponse = new CategoryResponse();
    categoryResponse.setId(CATEGORY_ID);
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setParentCategoryId(PARENT_CATEGORY_ID);
    categoryResponse.setName(CATEGORY_NAME);
    categoryResponse.setOscUpdatedBy(UPDATED_BY);
    categoryResponse.setOscUpdatedDate(UPDATED_DATE);
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setId(CATEGORY_ID_1);
    categoryResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse1.setParentCategoryId(PARENT_CATEGORY_ID_1);
    categoryResponse1.setNameEnglish(NAME_ENGLISH_1);
    categoryResponse1.setName(CATEGORY_NAME);
    categoryResponse.setCatalog(catalogResponse);
    categoryResponse1.setCatalog(catalogResponse);
    categoryResponse.setNameEnglish(NAME_ENGLISH_2);
    categoryResponseList = new ArrayList<>();
    categoryResponseList.add(categoryResponse1);
    categoryResponseList.add(categoryResponse);
    categoryCodeRequest = new CategoryCodeRequest(new ArrayList<>());
    categoryCodeRequest.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    categoryHierarchyResponse1 = new CategoryHierarchyResponse();
    categoryHierarchyResponse1.setCategoryCode(CATEGORY_CODE_1);
    categoryHierarchyResponse1.setCategoryId(CATEGORY_ID_1);
    categoryHierarchyResponse1.setCategoryHierarchy(categoryResponseList);
    categoryHierarchyResponseList = Collections.singletonList(categoryHierarchyResponse1);
    categoryHierarchyResponses = new GdnRestListResponse<>(categoryHierarchyResponseList, pageMetaData, REQUEST_ID);

    marginOrderResponse = new MarginOrderResponse();
    marginOrderResponse.setValue(1.0d);
    marginOrderResponse.setValueType(VALUE_TYPE);

    productSummaryResponses = new ArrayList<>();
    productSummaryResponse = new ProductSummaryResponse();
    productSummaryResponses.add(productSummaryResponse);
    productSummaryResponseGdnRestListResponse = new GdnRestListResponse<>(productSummaryResponses, pageMetaData, REQUEST_ID);
    ReflectionTestUtils.setField(categoryService, "defaultSalesCatalogCode", "SALES_CATALOG");
    ReflectionTestUtils.setField(categoryService, "categorySequencePageSize", SEQUENCE);

    categoryKeywordsUpdateRequest =
        CategoryKeywordsUpdateRequest.builder().keyword(KEYWORD).destinationCategory(DESTINATION_CATEGORY)
            .keywordId(KEYWORD_ID).build();

    categoryKeywordUpdateRequestList = CategoryKeywordUpdateRequestList.builder()
        .addedKeywords(Collections.singletonList(categoryKeywordsUpdateRequest)).build();

    restrictedKeywordsResponse = RestrictedKeywordsResponse.builder().keywordId(KEYWORD_ID).keyword(KEYWORD).build();
    categoryRestrictedKeywordsRequest =
        CategoryRestrictedKeywordsRequest.builder().categoryCode(CATEGORY_CODE).keyword(KEYWORD).build();
    categoryRestrictedKeywordsWebRequest =
        CategoryRestrictedKeywordsWebRequest.builder().categoryCode(CATEGORY_CODE).keyword(KEYWORD).build();

    minWholesaleDiscountServiceRequest =
        MinWholesaleDiscountServiceRequest.builder().price(PRICE).percentage(PERCENTAGE).build();
    wholesaleConfigServiceRequest =
        WholesaleConfigServiceRequest.builder().quantity(QUANTITY)
        .minWholesaleDiscount(Collections.singletonList(minWholesaleDiscountServiceRequest)).build();
    wholesaleMappingServiceRequest =
        WholesaleMappingServiceRequest.builder().configurationType(CONFIGURATION_TYPE)
        .wholesaleConfig(Collections.singletonList(wholesaleConfigServiceRequest)).build();
    minWholesaleDiscountResponse = MinWholesaleDiscountResponse
        .builder().percentage(PERCENTAGE).price(PRICE).build();
    wholesaleConfigResponse = WholesaleConfigResponse
        .builder().quantity(QUANTITY).minWholesaleDiscount(Collections.singletonList(minWholesaleDiscountResponse)).build();
    wholesaleMappingResponse = WholesaleMappingResponse
        .builder().wholesaleConfig(Collections.singletonList(wholesaleConfigResponse))
        .wholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLED).configurationType(CONFIGURATION_TYPE).build();

    minWholesaleDiscountRequest = MinWholesaleDiscountRequest.builder().percentage(PERCENTAGE).price(PRICE).build();
    wholesaleConfigRequest = WholesaleConfigRequest.builder().quantity(QUANTITY)
        .minWholesaleDiscount(Collections.singletonList(minWholesaleDiscountRequest)).build();
    wholesaleMappingRequest = WholesaleMappingRequest.builder().configurationType(CONFIGURATION_TYPE)
        .wholesaleConfig(Collections.singletonList(wholesaleConfigRequest)).build();
    categoryTreeResponse = new CategoryTreeResponse();
    categoryTreeResponse.setCategoryCode(CATEGORY_CODE);
    categoryTreeResponse.setCategoryName(CATEGORY_NAME);
    categoryTreeResponse.setParentCategory(StringUtils.EMPTY);
    CategoryTreeResponse child = new CategoryTreeResponse();
    child.setParentCategory(PARENT_CATEGORY_ID);
    child.setCategoryName(CHILD_CATEGORY_NAME);
    child.setCategoryCode(CATEGORY_CODE_2);
    child.setParentCategory(PARENT_CATEGORY_ID);
    children = new ArrayList<>();
    children.add(child);
    categoryTreeResponse.setChildren(children);
    categoryTreeResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(categoryTreeResponse), pageMetaData, REQUEST_ID);

    systemParameterResponse = new SystemParameterResponse();
    systemParameterResponse.setVariable(DOCUMENT_TYPE);
    systemParameterResponse.setValue(DOCUMENTS);
    systemParameterResponse.setDescription(DOCUMENT_DESCRIPTION);
    systemParameterResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, systemParameterResponse, REQUEST_ID);

    CategorySuggestionResponse categorySuggestionResponse =
        new CategorySuggestionResponse("1", CATEGORY_ID, CATEGORY_CODE, CATEGORY_NAME, CATEGORY_NAME);
    SuggestedCategoriesResponse suggestedCategoriesResponse =
        new SuggestedCategoriesResponse(Arrays.asList(categorySuggestionResponse), 0.8);
    productCategoryPredictionResponse =
        new ProductCategorySuggestionResponse(Arrays.asList(suggestedCategoriesResponse), PRODUCT_NAME);

    ReflectionTestUtils.setField(categoryService, "categorySuggestionMaxLimit", 5);
    ReflectionTestUtils.setField(categoryService, "categorySuggestionThreshold", 0.0);

    originalSalesCategoryWebRequest = new OriginalSalesCategoryWebRequest();
    originalSalesCategoryWebRequest.setOscCode(OSC_CODE);
    originalSalesCategoryWebRequest.setOscShortText(OSC_SHORT_TEXT);
    originalSalesCategoryWebRequest.setOscLongText(OSC_LONG_TEXT);

    originalSalesCategoryRequest = new OriginalSalesCategoryRequest();
    originalSalesCategoryRequest.setOscCode(OSC_CODE);
    originalSalesCategoryRequest.setOscShortText(OSC_SHORT_TEXT);
    originalSalesCategoryRequest.setOscLongText(OSC_LONG_TEXT);
    originalSalesCategoryRequest.setStoreId(STORE_ID);

    oscSummaryResponse =
        OscSummaryResponse.builder().activated(ACTIVATED).oscCode(OSC_CODE).oscLongText(OSC_TEXT).oscShortText(OSC_TEXT)
            .build();
    oscSummaryResponse.setId(ID);
    oscSummaryResponseList.add(oscSummaryResponse);
    oscInfoUpdateDTO = OscInfoUpdateDTO.builder().newOscCode(OSC_CODE).activated(true).build();

    originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
    originalSalesCategoryResponse.setOscCode(OSC_CODE);
    originalSalesCategoryResponse.setOscShortText(OSC_SHORT_TEXT);
    originalSalesCategoryResponse.setOscLongText(OSC_LONG_TEXT);
    originalSalesCategoryResponse.setMasterCategories(Arrays.asList(categoryResponse, categoryResponse1));

    baseMarginResponse = new BaseMarginResponse();
    baseMarginResponse.setCategoryCode(CATEGORY_CODE);

    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    profitMarginWebRequest = new ProfitMarginWebRequest();
    profitMarginWebRequest.setCategoryCode(CATEGORY_CODE);
    profitMarginWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(marginFeign);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  private CategoryInfoUpdateRequest generateCategoryInfoUpdateRequest() {
    CategoryInfoUpdateRequest request = new CategoryInfoUpdateRequest();
    request.setName(CATEGORY_NAME);
    request.setCategoryCode(DEFAULT_CATEGORY_CODE);
    request.setId(DEFAULT_CATEGORY_ID);
    request.setActivated(true);
    return request;
  }

  private CategoryStatusChangeServiceRequest generateCategoryStatusChangeServiceRequest() {
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = new CategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setName(CATEGORY_NAME);
    categoryStatusChangeServiceRequest.setCategoryCode(DEFAULT_CATEGORY_CODE);
    categoryStatusChangeServiceRequest.setId(DEFAULT_CATEGORY_ID);
    categoryStatusChangeServiceRequest.setCatalogType(SALES_CATALOG);
    categoryStatusChangeServiceRequest.setActivated(true);
    return categoryStatusChangeServiceRequest;
  }

  private CategoryDetailResponse generateCategoryDetailResponse() {
    List<CategoryReferenceResponse> categoryReferenceResponses = new ArrayList<CategoryReferenceResponse>();
    CategoryReferenceResponse categoryReferenceResponse = new CategoryReferenceResponse();
    categoryReferenceResponse.setId(DEFAULT_CATEGORY_ID);
    categoryReferenceResponse.setMasterCategoryReference(new CategoryResponse());
    categoryReferenceResponse.getMasterCategoryReference().setId(DEFAULT_CATEGORY_ID);
    categoryReferenceResponses.add(categoryReferenceResponse);

    List<CategoryAttributeResponse> categoryAttributeResponses = new ArrayList<CategoryAttributeResponse>();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    categoryAttributeResponse.setAttribute(new AttributeResponse());
    categoryAttributeResponse.getAttribute().setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    categoryAttributeResponse.getAttribute().setAttributeType(DEFAULT_ATTRIBUTE_TYPE);
    categoryAttributeResponse.getAttribute().setId(DEFAULT_ATTRIBUTE_ID);
    categoryAttributeResponse.getAttribute().setMarkForDelete(false);
    categoryAttributeResponse.getAttribute().setName(DEFAULT_NAME);
    categoryAttributeResponse.setId(DEFAULT_CATEGORY_ID);
    categoryAttributeResponse.setMarkForDelete(false);
    categoryAttributeResponses.add(categoryAttributeResponse);

    CategoryDetailResponse response = new CategoryDetailResponse();
    response.setMasterCategoryReferences(categoryReferenceResponses);
    response.setCatalog(new CatalogResponse());
    response.setCategoryAttributes(categoryAttributeResponses);
    response.setCategoryCode(DEFAULT_CATEGORY_CODE);
    response.setParentCategoryId(DEFAULT_CATEGORY_ID_2);
    response.setCreatedBy(DEFAULT_CREATED_BY);
    response.setCategoryCode(DEFAULT_CATEGORY_CODE);
    response.setCreatedDate(new Date());
    response.setDefaultDescription(new byte[1]);
    response.setDescription(new byte[1]);
    response.setDisplay(true);
    response.setId(DEFAULT_CATEGORY_ID);
    response.setLogisticAdjustment(DEFAULT_LOGISTIC_ADJUSTMENT);
    response.setMarkForDelete(false);
    response.setName(DEFAULT_NAME);
    response.setNeedIdentity(false);
    response.setParentCategoryId(DEFAULT_CATEGORY_ID_2);
    response.setSalesCategoryReferences(new ArrayList<CategoryReferenceResponse>());
    response.getSalesCategoryReferences().add(new CategoryReferenceResponse());
    response.setSequence(DEFAULT_SEQUENCE);
    response.setShortDescription(DEFAULT_DESCRIPTION);
    response.setState(DEFAULT_STATE);
    response.setStoreId(DEFAULT_STORE_ID);
    response.setUpdatedBy(DEFAULT_UPDATED_BY);
    response.setUpdatedDate(new Date());
    response.setVersion(DEFAULT_VERSION);
    response.setViewable(false);
    response.setWarranty(false);
    response.setCatalog(new CatalogResponse("","", "SALES_CATALOG"));
    return response;
  }

  private GdnRestListResponse<CategoryShippingResponse> getCategoryShippingResponse() {
    GdnRestListResponse<CategoryShippingResponse> response = new GdnRestListResponse<>();
    List<CategoryShippingResponse> categoryShippingResponses = new ArrayList<>();
    CategoryShippingResponse categoryShippingResponse = new CategoryShippingResponse();
    categoryShippingResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);
    categoryShippingResponses.add(categoryShippingResponse);
    response.setContent(categoryShippingResponses);
    response.setSuccess(Boolean.TRUE);
    return response;
  }

  private List<CategoryResponse> generateCategoryResponses() {
    List<CategoryResponse> categoriesList = new ArrayList<CategoryResponse>();
    CategoryResponse category1 = new CategoryResponse();
    category1.setSequence(1);
    CategoryResponse category2 = new CategoryResponse();
    category2.setSequence(2);
    categoriesList.add(category1);
    categoriesList.add(category2);
    return categoriesList;
  }

  private GdnRestListResponse<CategoryResponse> getResponse() {
    GdnRestListResponse<CategoryResponse> gdnRestListResponse = new GdnRestListResponse<>();
    gdnRestListResponse.setSuccess(true);
    gdnRestListResponse.setContent(generateCategoryResponses());
    return gdnRestListResponse;
  }


  @Test
  void activate_MasterCatalogTest() throws Exception {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(new MarginCategoryResponse(), DEFAULT_REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE)).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    Mockito.verify(marginFeign).filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
  }

  @Test
  void activate_MasterCatalogNewMarginChangesTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    ReflectionTestUtils.setField(categoryService, "setDefaultOrderTypeForMargin", true);
    ReflectionTestUtils.setField(categoryService, "defaultOrderTypeForMargin", Constants.B2C_RETAIL);
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(any()))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(),
            marginOrderItemArgumentCaptor.capture());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
    Assertions.assertEquals(marginOrderItemArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType(),
        Constants.B2C_RETAIL);
  }

  @Test
  void activate_MasterCatalogNewMarginChangesFlagFalseTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    ReflectionTestUtils.setField(categoryService, "setDefaultOrderTypeForMargin", false);
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(any()))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(),
            marginOrderItemArgumentCaptor.capture());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
    Assertions.assertNull(marginOrderItemArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType());
  }

  @Test
  void activate_MasterCatalogNewMarginChangesExceptionTest() throws Exception {
    try {
      ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
      CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
      CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
      CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = generateCategoryStatusChangeServiceRequest();
      categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
      GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
      Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
          .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
      Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
      Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE)).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
      ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
      List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
      OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
      List<Margin> marginList = new ArrayList<>();
      Margin margin = new Margin();
      margin.setMarginType("BASE-MARGIN");
      margin.setReplacementType("-");
      marginList.add(margin);
      orderItemMarginsResponse.setMargins(marginList);
      orderItemMarginsResponses.add(orderItemMarginsResponse);
      listBaseResponse.setContent(orderItemMarginsResponses);
      listBaseResponse.setSuccess(true);
      Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
      Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
      Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
      Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
      Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
      Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), any())).thenThrow(RuntimeException.class);
      Mockito.when(xbpFeign.filterByBusinessPartnerCode(any()))
          .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
      categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (ActivationValidationException ex){
      Assertions.assertNotNull(ex.getMessage());
    } finally {
      Mockito.verify(marginFeign)
          .filterMargin(any(), any(), any(), any(), any(), any());
      Mockito.verify(clientParameterHelper).getClientId();
      Mockito.verify(clientParameterHelper).getChannelId();
      Mockito.verify(clientParameterHelper).getStoreId();
      Mockito.verify(clientParameterHelper).getRequestId();
      Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
      Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    }
  }

  @Test
  void activate_MasterCatalogB2bExclusiveProductTest() throws Exception {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    categoryInfoUpdateRequest.setB2bExclusive(true);
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    categoryStatusChangeServiceRequest.setB2bExclusive(true);
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
        .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
    Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE)).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
    Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
  }

  @Test
  void activateTest() throws Exception {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE)).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
  }

  @Test
  void activateEmptyDescriptionExceptionTest() {
    Exception exception = null;
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    categoryDetailResponse.setDefaultDescription(StringUtils.EMPTY.getBytes());
    try {
      Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
          .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
      Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(new GdnRestSingleResponse<>(new MarginCategoryResponse(), DEFAULT_REQUEST_ID));
      Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
      categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ActivationValidationException.class);
      Assertions.assertEquals(DESCRIPTION_EMPTY_ERROR_MESSAGE, exception.getMessage());
      Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
      Mockito.verify(marginFeign)
          .filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    }
  }

  @Test
  void activateMarginNotSetExceptionTest() {
    Exception exception = null;
    GdnRestListResponse<CategoryShippingResponse> response = getCategoryShippingResponse();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    try {
      Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
          .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
      Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(new GdnRestSingleResponse<>(null, DEFAULT_REQUEST_ID));
      Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE)).thenReturn(response);
      categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ActivationValidationException.class);
      Assertions.assertEquals(MARGIN_NOT_SET_ERROR_MESSAGE, exception.getMessage());
      Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
      Mockito.verify(marginFeign)
          .filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    }
  }

  @Test
  void activateCategoryShippingCodeNotSetExceptionTest() {
    Exception exception = null;
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    CategoryDetailResponse categoryDetailResponse = generateCategoryDetailResponse();
    try {
      Mockito.when(pcbFeign.getCategoryDetail(DEFAULT_CATEGORY_ID))
          .thenReturn(new GdnRestSingleResponse<>(categoryDetailResponse, DEFAULT_REQUEST_ID));
      Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString()))
          .thenReturn(new GdnRestSingleResponse<>(new MarginCategoryResponse(), DEFAULT_REQUEST_ID));
      Mockito.when(pcbFeign.getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE))
          .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), pageMetaData, REQUEST_ID));
      categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ActivationValidationException.class);
      Assertions.assertEquals(CATEGORY_SHIPPING_CODE_NOT_SET, exception.getMessage());
      Mockito.verify(pcbFeign).getCategoryDetail(DEFAULT_CATEGORY_ID);
      Mockito.verify(marginFeign)
          .filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString());
      Mockito.verify(pcbFeign).getCategoryShippingByCategoryCode(DEFAULT_CATEGORY_CODE);
    }
  }

  @Test
  void updateCategoryInfoTest() {
    categoryInfoUpdateServiceRequest.setDocumentType(DOCUMENTS);
    when(pcbFeign.updateCategoryInfo(any(CategoryInfoUpdateRequest.class), anyBoolean()))
        .thenReturn(new GdnBaseRestResponse(true));
    categoryService.updateCategoryInfo(categoryInfoUpdateServiceRequest);
    verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequestArgumentCaptor.capture(), anyBoolean());
    assertEquals(STORE_ID, categoryInfoUpdateRequestArgumentCaptor.getValue().getStoreId());
    assertEquals(NAME, categoryInfoUpdateRequestArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryInfoUpdateRequestArgumentCaptor.getValue().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION),
        Arrays.toString(categoryInfoUpdateRequestArgumentCaptor.getValue().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH),
        Arrays.toString(categoryInfoUpdateRequestArgumentCaptor.getValue().getDescriptionEnglish()));
    assertEquals(CATEGORY_CODE, categoryInfoUpdateRequestArgumentCaptor.getValue().getCategoryCode());
    assertEquals(SEQUENCE, categoryInfoUpdateRequestArgumentCaptor.getValue().getSequence());
    assertEquals(UPDATED_BY, categoryInfoUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(PARENT_CATEGORY_ID, categoryInfoUpdateRequestArgumentCaptor.getValue().getParentCategoryId());
    assertEquals(LOGISTIC_ADJUSTMENT, categoryInfoUpdateRequestArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(INTERNAL_ACTIVATION_INTERVAL,
        categoryInfoUpdateRequestArgumentCaptor.getValue().getInternalActivationInterval());
    assertFalse(categoryInfoUpdateRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().getAgeLimit());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryInfoUpdateRequestArgumentCaptor.getValue().getDangerousGoodsLevel());
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isUmkm());
    assertTrue(DOCUMENTS.equals(categoryInfoUpdateRequestArgumentCaptor.getValue().getDocumentType()));
    assertTrue(categoryInfoUpdateRequestArgumentCaptor.getValue().isSizeChartRequired());
  }

  @Test
  void updateCategoryInfoTestInvalidResponse() {
    CategoryInfoUpdateServiceRequest categoryInfoUpdateServiceRequest =
        CategoryInfoUpdateServiceRequest.builder().build();
    when(pcbFeign.updateCategoryInfo(any(CategoryInfoUpdateRequest.class), anyBoolean()))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      categoryService.updateCategoryInfo(categoryInfoUpdateServiceRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(pcbFeign).updateCategoryInfo(any(CategoryInfoUpdateRequest.class), anyBoolean());
    }
  }

  @Test
  void getCategoryInfoWithShippingDetailTest() {
    categoryDetailAndShippingResponse.setDocumentType(DOCUMENTS);
    response.setValue(categoryDetailAndShippingResponse);
    Mockito.when(this.pcbFeign.getCategoryInfoByCategoryId(true, CATEGORY_ID)).thenReturn(response);
    SingleBaseResponse<CategoryInfoWebResponse> categoryInfoWebResponse =
        categoryService.getCategoryInfoWithShippingDetail(CATEGORY_ID, REQUEST_ID, true);
    Mockito.verify(this.pcbFeign).getCategoryInfoByCategoryId(true, CATEGORY_ID);
    Assertions.assertNotNull(categoryInfoWebResponse);
    Assertions.assertEquals(NAME, categoryInfoWebResponse.getValue().getName());
    Assertions.assertEquals(CATEGORY_ID, categoryInfoWebResponse.getValue().getId());
    assertTrue(categoryInfoWebResponse.getValue().isUmkm());
    assertTrue(DOCUMENTS.equals(categoryInfoWebResponse.getValue().getDocumentType()));
  }

  @Test
  void getCategoryInfoWithShippingDetailWithClientExceptionTest() {
    Exception exception = null;
    SingleBaseResponse<CategoryInfoWebResponse> categoryInfoWebResponse = null;
    Mockito.when(this.pcbFeign.getCategoryInfoByCategoryId(true, CATEGORY_ID))
        .thenThrow(ClientException.class);
    try {
      categoryInfoWebResponse =
          categoryService.getCategoryInfoWithShippingDetail(CATEGORY_ID, REQUEST_ID, true);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ClientException.class);
      Mockito.verify(this.pcbFeign).getCategoryInfoByCategoryId(true, CATEGORY_ID);
      Assertions.assertNull(categoryInfoWebResponse);
    }
  }

  @Test
  void getCategoryInfoWithShippingDetailWithExceptionTest() {
    Exception exception = null;
    SingleBaseResponse<CategoryInfoWebResponse> categoryInfoWebResponse = null;
    Mockito.when(this.pcbFeign.getCategoryInfoByCategoryId(true, CATEGORY_ID))
        .thenThrow(RuntimeException.class);
    try {
      categoryInfoWebResponse =
          categoryService.getCategoryInfoWithShippingDetail(CATEGORY_ID, REQUEST_ID, true);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), RuntimeException.class);
      Mockito.verify(this.pcbFeign).getCategoryInfoByCategoryId(true, CATEGORY_ID);
      Assertions.assertNull(categoryInfoWebResponse);
    }
  }

  @Test
  void deactivateTest() throws Exception {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    categoryInfoUpdateRequest.setActivated(false);
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    categoryService.deactivate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
  }

  @Test
  void activate_ClientExceptionTest() throws Exception {
    Exception exception = null;
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = generateCategoryStatusChangeServiceRequest();
    categoryStatusChangeServiceRequest.setCatalogType(DEFAULT_CATALOG_TYPE);
    Mockito.when(marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString()))
        .thenThrow(ClientException.class);
    try {
      categoryService.activate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getClass(), ClientException.class);
      Mockito.verify(marginFeign)
          .filterMarginCategoryByCategoryCodeAndOrderDate(Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  void deactivateCategoryClientExceptionTest() throws Exception {
    CategoryInfoUpdateRequest categoryInfoUpdateRequest = generateCategoryInfoUpdateRequest();
    categoryInfoUpdateRequest.setActivated(false);
    Mockito.when(pcbFeign.updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE))
        .thenThrow(ClientException.class);
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest =
        generateCategoryStatusChangeServiceRequest();
    try {
      categoryService.deactivate(DEFAULT_REQUEST_ID, categoryStatusChangeServiceRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign).updateCategoryInfo(categoryInfoUpdateRequest, Boolean.TRUE);
    }
  }

  @Test
  void updateCategoryMappingsTest() {
    when(pcbFeign.updateCategoryMappings(any(CategoryMappingsUpdateRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    categoryService.updateCategoryMappings(categoryMappingsUpdateServiceRequest);
    verify(pcbFeign).updateCategoryMappings(categoryMappingsUpdateRequestArgumentCaptor.capture());
    assertEquals(STORE_ID, categoryMappingsUpdateRequestArgumentCaptor.getValue().getStoreId());
    assertEquals(ID, categoryMappingsUpdateRequestArgumentCaptor.getValue().getId());
    assertEquals(ATTRIBUTE_ID_1, categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).getAttributeId());
    assertEquals(1, categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).getSequence(), 0);
    assertTrue(categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).isMainDefiningAttribute());
    assertFalse(categoryMappingsUpdateRequestArgumentCaptor.getValue().getAddedAttributes().get(0).isUsp());
    assertEquals(ATTRIBUTE_ID_2, categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getDeletedAttributes().get(0).getAttributeId());
    assertEquals(MASTER_CATEGORY_ID_1, categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getAddedMasterCategoryIds().get(0));
    assertEquals(MASTER_CATEGORY_ID_2, categoryMappingsUpdateRequestArgumentCaptor.getValue()
        .getDeletedMasterCategoryIds().get(0));
    assertEquals(UPDATED_BY, categoryMappingsUpdateRequestArgumentCaptor.getValue().getUpdatedBy());
  }

  @Test
  void updateCategoryMappingsTestInvalidResponse() {
    when(pcbFeign.updateCategoryMappings(any(CategoryMappingsUpdateRequest.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    categoryMappingsUpdateServiceRequest.setWholesaleMapping(null);
    Exception exception = null;
    try{
      categoryService.updateCategoryMappings(categoryMappingsUpdateServiceRequest);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      verify(pcbFeign).updateCategoryMappings(categoryMappingsUpdateRequestArgumentCaptor.capture());
    }
  }

  @Test
  void createCategoryTest() {
    categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().setDocumentType(DOCUMENTS);
    when(pcbFeign.createCategory(any(CategoryDetailRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new CreateCategoryResponse(CATEGORY_CODE), REQUEST_ID));
    categoryService.createCategory(categoryCreateServiceRequest);
    verify(pcbFeign).createCategory(categoryDetailRequestArgumentCaptor.capture());
    CategoryDetailRequest categoryDetailRequest = categoryDetailRequestArgumentCaptor.getValue();
    assertEquals(NAME, categoryDetailRequest.getCategoryInfoDetail().getName());
    assertEquals(NAME_ENGLISH, categoryDetailRequest.getCategoryInfoDetail().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION),
        Arrays.toString(categoryDetailRequest.getCategoryInfoDetail().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH),
        Arrays.toString(categoryDetailRequest.getCategoryInfoDetail().getDescriptionEnglish()));
    assertEquals(ATTRIBUTE_ID_1, categoryDetailRequest.getCategoryMappingsDetail()
        .getAddedAttributes().get(0).getAttributeId());
    assertEquals(1, categoryDetailRequest.getCategoryMappingsDetail()
        .getAddedAttributes().get(0).getSequence(), 0);
    assertTrue(categoryDetailRequest.getCategoryMappingsDetail()
        .getAddedAttributes().get(0).isMainDefiningAttribute());
    assertFalse(
        categoryDetailRequest.getCategoryMappingsDetail().getAddedAttributes().get(0).isUsp());
    assertEquals(MASTER_CATEGORY_ID_1, categoryDetailRequest.getCategoryMappingsDetail()
        .getAddedMasterCategoryIds().get(0));
    assertEquals(UPDATED_BY, categoryDetailRequest.getCreatedBy());
    assertTrue(categoryDetailRequest.getCategoryInfoDetail().isUmkm());
    assertTrue(
        DOCUMENTS.equals(categoryDetailRequestArgumentCaptor.getValue().getCategoryInfoDetail().getDocumentType()));
    assertTrue(categoryDetailRequest.getCategoryInfoDetail().isSizeChartRequired());
  }

  @Test
  void createCategoryTestInvalidResponse() {
    when(pcbFeign.createCategory(any(CategoryDetailRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    Exception exception = null;
    try{
      categoryService.createCategory(categoryCreateServiceRequest);
    }catch (Exception e){
      exception = e;
    }finally {
      assertEquals(ClientException.class, exception.getClass());
      verify(pcbFeign).createCategory(categoryDetailRequestArgumentCaptor.capture());
    }
  }

  @Test
  void getSalesCategoryMappingByCategoryCodesTest() {
    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, null, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryMappingResponse> response =
        categoryService.getSalesCategoryMappingByCategoryCodes(Collections.singletonList(CATEGORY_CODE), CATALOG_ID);
    Mockito.verify(pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, null, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false);
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(PARENT_ID, response.get(0).getParentCategoryId());
    Assertions.assertEquals(NAME, response.get(0).getName());
    Assertions.assertEquals(NAME_ENGLISH, response.get(0).getNameEnglish());
  }

  @Test
  void getSalesCategoryMappingByCategoryCodesWithSortingTest() {
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setName(NAME_1);
    category.setId(CATEGORY_ID);
    category.setNameEnglish(NAME_ENGLISH_1);
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setId(CATALOG_ID);
    category.setCatalog(catalogResponse);
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setName(NAME_2);
    categoryDTO.setNameEnglish(NAME_ENGLISH_2);
    categoryDTO.setId(ID);
    categoryDTO.setCatalog(catalogResponse);
    categoryDTOGdnRestListResponse.getContent().add(category);
    categoryDTOGdnRestListResponse.getContent().add(categoryDTO);
    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, null, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false)).thenReturn(categoryDTOGdnRestListResponse);
    List<CategoryMappingResponse> response =
        categoryService.getSalesCategoryMappingByCategoryCodes(Collections.singletonList(CATEGORY_CODE), CATALOG_ID);
    Mockito.verify(pcbFeign)
        .getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, null, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false);
    Assertions.assertEquals(CATEGORY_CODE_1, response.get(0).getCategoryCode());
    Assertions.assertEquals(NAME_1, response.get(0).getName());
    Assertions.assertEquals(NAME_2, response.get(1).getName());
    Assertions.assertEquals(NAME, response.get(2).getName());
    Assertions.assertEquals(NAME_ENGLISH_1, response.get(0).getNameEnglish());
    Assertions.assertEquals(NAME_ENGLISH_2, response.get(1).getNameEnglish());
    Assertions.assertEquals(NAME_ENGLISH, response.get(2).getNameEnglish());
  }

  @Test
  void getSalesCategoryMappingByCategoryCodesWithChildrenTest() {
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(CATEGORY_CODE_1);
    category.setName(NAME);
    categoryDTO.setName(NAME);
    category.setId(CATEGORY_ID);
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setId(CATALOG_ID);
    category.setCatalog(catalogResponse);
    List<CategoryDTO> categoryDTOList = Arrays.asList(category, categoryDTO);
    GdnRestListResponse<CategoryDTO> listResponse =
        new GdnRestListResponse<>(categoryDTOList, pageMetaData, REQUEST_ID);
    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, null, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false)).thenReturn(categoryDTOGdnRestListResponse);

    Mockito.when(
        pcbFeign.getChildFromParentByCatalogIdWithChildCount(CATALOG_ID, CATEGORY_ID, PAGE, TOTAL_RECORDS, FILTER_TYPE,
            DOCUMENT_FILTER_TYPE, false, false)).thenReturn(listResponse);
    Mockito.when(pcbFeign.getHierarchyByCategoryCodes(any(CategoryCodeRequest.class)))
        .thenReturn(categoryHierarchyResponses);
    List<CategoryMappingResponse> response =
        categoryService.getSalesCategoryMappingByCategoryCodes(Arrays.asList(CATEGORY_CODE, CATEGORY_CODE_1),
            CATALOG_ID);
    Mockito.verify(pcbFeign, times(2))
        .getChildFromParentByCatalogIdWithChildCount(eq(CATALOG_ID), stringArgumentCaptor.capture(), eq(PAGE),
            eq(TOTAL_RECORDS), eq(FILTER_TYPE), eq(DOCUMENT_FILTER_TYPE), eq(false), eq(false));
    Mockito.verify(pcbFeign).getHierarchyByCategoryCodes(categoryCodeRequestArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE, response.get(0).getCategoryCode());
    Assertions.assertEquals(NAME_ENGLISH, response.get(0).getNameEnglish());
  }

  @Test
  void getMarginByCategoryCodeTest(){
    GdnRestSingleResponse<MarginCategoryResponse> clientResponse =
        new GdnRestSingleResponse<MarginCategoryResponse>(null, null, true, marginCategoryResponse,
            null);
    Mockito.when(marginFeign
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(CATEGORY_CODE), Mockito.anyString())).thenReturn(clientResponse);
    MarginCategoryWebResponse response = categoryService.getMarginByCategoryCode(CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(CATEGORY_CODE), Mockito.anyString());
    Assertions.assertEquals(CATEGORY_CODE, response.getCategoryCode());
  }

  @Test
  void getMarginByCategoryCodeNewMarginTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(any()))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    MarginCategoryWebResponse response = categoryService.getMarginByCategoryCode(CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getMarginByCategoryCode_whenClientExceptionTest(){
    MarginCategoryWebResponse response = null;
    GdnRestSingleResponse<MarginCategoryResponse> clientResponse =
        new GdnRestSingleResponse<MarginCategoryResponse>(null, null, false, marginCategoryResponse,
            null);
    Mockito.when(marginFeign
        .filterMarginCategoryByCategoryCodeAndOrderDate(eq(CATEGORY_CODE),
            Mockito.anyString())).thenReturn(clientResponse);
    try {
      response = categoryService.getMarginByCategoryCode(CATEGORY_CODE);
    } catch (ClientException exception) {}
    finally {
      Mockito.verify(marginFeign)
          .filterMarginCategoryByCategoryCodeAndOrderDate(eq(CATEGORY_CODE),
              Mockito.anyString());
      Assertions.assertNull(response);
    }
  }

  @Test
  void findCategorySummaryByParentIdTest() {
    GdnRestListResponse<CategoryResponse> response = getResponse();
    when(pcbFeign.findCategorySummaryByParentId(SEQUENCE, PARENT_ID))
        .thenReturn(response);
    List<CategoryResponse> categoryResponses = this.categoryService.findCategorySummaryByParentId(PARENT_ID);
    Mockito.verify(pcbFeign)
        .findCategorySummaryByParentId(SEQUENCE, PARENT_ID);
    assertEquals(2, categoryResponses.size());
    assertEquals(response.getContent(), categoryResponses);
  }

  @Test
  void findCategorySummaryByParentId_withClientExceptionTest() {
    GdnRestListResponse<CategoryResponse> response = null;
    when(this.pcbFeign.findCategorySummaryByParentId(SEQUENCE, PARENT_ID))
        .thenThrow(ClientException.class);
    try {
      response = pcbFeign.findCategorySummaryByParentId(SEQUENCE, PARENT_ID);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(this.pcbFeign)
          .findCategorySummaryByParentId(SEQUENCE, PARENT_ID);
      Assertions.assertNull(response);
    }
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeTest() {
    MarginCategoryWebResponse response = null;
    GdnRestSingleResponse<MarginOrderResponse> clientResponse =
        new GdnRestSingleResponse<>(null, null, true, marginOrderResponse, null);
    Mockito.when(marginFeign.filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(clientResponse);
    response = categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
        BUSINESS_PARTNER_CODE,
        CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(eq(BUSINESS_PARTNER_CODE),
            eq(CATEGORY_CODE), Mockito.anyString(), Mockito.anyString());
    Assertions.assertEquals(response.getValue(), 1.0d, 0.0d);
    Assertions.assertEquals(response.getValueType(), VALUE_TYPE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCode_whenClientExceptionTest() {
    MarginCategoryWebResponse response = null;
    GdnRestSingleResponse<MarginOrderResponse> clientResponse =
        new GdnRestSingleResponse<>(null, null, false, marginOrderResponse, null);
    Mockito.when(marginFeign.filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(clientResponse);
    try {
      response = categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
          BUSINESS_PARTNER_CODE,
          CATEGORY_CODE);
    } catch (ClientException exception) {
    } finally {
      Mockito.verify(marginFeign)
          .filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(eq(BUSINESS_PARTNER_CODE),
              eq(CATEGORY_CODE), Mockito.anyString(), Mockito.anyString());
      Assertions.assertNull(response);
    }
  }

  @Test
  void updateCategoryMappingsTest2() {
    when(
        pcbFeign.updateCategoriesWithRestrictedKeywords(eq(CATEGORY_CODE), any(CategoryKeywordUpdateRequestList.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    categoryService.updateCategoryRestrictedKeywordMappings(CATEGORY_CODE, categoryKeywordUpdateRequestList);
    verify(pcbFeign)
        .updateCategoriesWithRestrictedKeywords(eq(CATEGORY_CODE), any(CategoryKeywordUpdateRequestList.class));
  }

  @Test
  void updateCategoryRestrictedKeywordMappingsTestInvalidResponse() {
    when(
        pcbFeign.updateCategoriesWithRestrictedKeywords(eq(CATEGORY_CODE), any(CategoryKeywordUpdateRequestList.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      categoryService.updateCategoryRestrictedKeywordMappings(CATEGORY_CODE, categoryKeywordUpdateRequestList);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(pcbFeign)
          .updateCategoriesWithRestrictedKeywords(eq(CATEGORY_CODE), any(CategoryKeywordUpdateRequestList.class));
    }
  }

  @Test
  void findRestrictedKeywordsTest() {
    List<RestrictedKeywordsResponse> restrictedKeywordsResponseList = new ArrayList<>();
    restrictedKeywordsResponseList.add(restrictedKeywordsResponse);
    GdnRestListResponse<RestrictedKeywordsResponse> response =
        new GdnRestListResponse<>(restrictedKeywordsResponseList, pageMetaData, REQUEST_ID);
    when(pcbFeign.getCategoryRestrictedKeywords(pageable.getPageNumber(), pageable.getPageSize(),
        categoryRestrictedKeywordsRequest)).thenReturn(response);
    this.categoryService.findRestrictedKeywords(categoryRestrictedKeywordsWebRequest, pageable);
    Mockito.verify(pcbFeign).getCategoryRestrictedKeywords(pageable.getPageNumber(), pageable.getPageSize(),
        categoryRestrictedKeywordsRequest);
  }

  @Test
  void findRestrictedKeywordsTestInvalidResponse() {
    when(pcbFeign.getCategoryRestrictedKeywords(pageable.getPageNumber(), pageable.getPageSize(),
        categoryRestrictedKeywordsRequest))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), pageMetaData, REQUEST_ID));
    try {
      categoryService.findRestrictedKeywords(categoryRestrictedKeywordsWebRequest, pageable);
    } finally {
      verify(pcbFeign).getCategoryRestrictedKeywords(pageable.getPageNumber(), pageable.getPageSize(),
          categoryRestrictedKeywordsRequest);
    }
  }

  @Test
  void findWholesaleConfigTest() {
    GdnRestSingleResponse<WholesaleMappingResponse> response =
        new GdnRestSingleResponse<>(null, null, true, wholesaleMappingResponse, REQUEST_ID);
    when(pcbFeign.getCategoryWholesaleConfig(CATEGORY_ID, CATEGORY_CODE)).thenReturn(response);
    this.categoryService.findWholesaleConfig(CATEGORY_ID, CATEGORY_CODE);
    Mockito.verify(pcbFeign).getCategoryWholesaleConfig(CATEGORY_ID, CATEGORY_CODE);
  }

  @Test
  void updateCategoryWholesaleConfigMappingTest() {
    Mockito.when(pcbFeign.updateCategoriesWithWholesaleConfig(eq(CATEGORY_ID), any(WholesaleMappingRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    categoryService.updateCategoryWholesaleConfigMapping(CATEGORY_ID, wholesaleMappingRequest);
    Mockito.verify(pcbFeign).updateCategoriesWithWholesaleConfig(eq(CATEGORY_ID), any(WholesaleMappingRequest.class));
  }

  @Test
  void updateCategoryWholesaleConfigMappingExceptionTest() {
    Mockito.when(pcbFeign.updateCategoriesWithWholesaleConfig(eq(CATEGORY_ID), any(WholesaleMappingRequest.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      categoryService.updateCategoryWholesaleConfigMapping(CATEGORY_ID, wholesaleMappingRequest);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign).updateCategoriesWithWholesaleConfig(eq(CATEGORY_ID), any(WholesaleMappingRequest.class));
    }
  }

  @Test
  void getCategoryListForGenericTemplateTest() {
    when(pcbFeign.getCategoryTree(true)).thenReturn(categoryTreeResponseGdnRestListResponse);
    List<CategoryTreeResponse> responseList = this.categoryService.getCategoryListForGenericTemplate(true);
    Mockito.verify(pcbFeign).getCategoryTree(true);
    assertEquals(StringUtils.EMPTY, responseList.get(0).getParentCategory());
    assertEquals(CATEGORY_CODE, responseList.get(0).getCategoryCode());
    assertEquals(CATEGORY_NAME, responseList.get(0).getCategoryName());
    assertEquals(PARENT_CATEGORY_ID, responseList.get(0).getChildren().get(0).getParentCategory());
    assertEquals(CHILD_CATEGORY_NAME, responseList.get(0).getChildren().get(0).getCategoryName());
    assertEquals(CATEGORY_CODE_2, responseList.get(0).getChildren().get(0).getCategoryCode());
  }

  @Test
  void getCategoryTreeForCategoryCodesTest() {
    when(pcbFeign.getHierarchyByCategoryCodes(categoryCodeRequest)).thenReturn(categoryHierarchyResponses);
    List<CatalogTreeWebResponse> responseList =
        this.categoryService.getCategoryTreeForCategoryCodes(categoryCodeRequest);
    Mockito.verify(pcbFeign).getHierarchyByCategoryCodes(categoryCodeRequest);
    assertEquals(2, responseList.get(0).getCategories().size());
    assertEquals(CATALOG_ID, responseList.get(0).getCatalogCode());
    assertEquals(CATEGORY_CODE_1, responseList.get(0).getCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_ID_1, responseList.get(0).getCategories().get(0).getCategoryId());
  }

  @Test
  void getDocumentListTest() {
    when(pcbFeign.getDocumentList(DOCUMENT_TYPE)).thenReturn(systemParameterResponseGdnRestSingleResponse);
    DocumentWebResponse documentWebResponse = this.categoryService.getDocumentList();
    verify(pcbFeign).getDocumentList(DOCUMENT_TYPE);
    assertTrue(
        Arrays.stream(DOCUMENTS.split(",")).collect(Collectors.toList()).equals(documentWebResponse.getDocumentList()));
    assertTrue(
        Arrays.stream(DOCUMENTS.split(",")).collect(Collectors.toList()).equals(documentWebResponse.getDocumentList()));
  }

  @Test
  void getDocumentListWithNullValueTest() {
    systemParameterResponse.setValue(null);
    systemParameterResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(null, null, true, systemParameterResponse, REQUEST_ID);
    when(pcbFeign.getDocumentList(DOCUMENT_TYPE)).thenReturn(systemParameterResponseGdnRestSingleResponse);
    DocumentWebResponse documentWebResponse = this.categoryService.getDocumentList();
    verify(pcbFeign).getDocumentList(DOCUMENT_TYPE);
    assertTrue(CollectionUtils.isEmpty(documentWebResponse.getDocumentList()));
  }

  @Test
  void getDocumentListWithExceptionTest() {
    when(pcbFeign.getDocumentList(DOCUMENT_TYPE)).thenThrow(ClientException.class);
    try {
      this.categoryService.getDocumentList();
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(pcbFeign).getDocumentList(DOCUMENT_TYPE);
    }
  }

  @Test
  void getSystemParamValueTest() {
    when(pcbFeign.getDocumentList(DOCUMENT_TYPE)).thenReturn(
      systemParameterResponseGdnRestSingleResponse);
    DocumentWebResponse documentWebResponse =
      this.categoryService.getSystemParamValue(DOCUMENT_TYPE);
    verify(pcbFeign).getDocumentList(DOCUMENT_TYPE);
    assertEquals(Arrays.stream(DOCUMENTS.split(",")).collect(Collectors.toList()),
      documentWebResponse.getDocumentList());
    assertEquals(Arrays.stream(DOCUMENTS.split(",")).collect(Collectors.toList()),
      documentWebResponse.getDocumentList());
  }

  @Test
  void getCategorySuggestionByProductNameTest() {
    when(productCategorySuggestionFeign.predictProductCategoriesByProductName(PRODUCT_NAME, 5, 0))
        .thenReturn(productCategoryPredictionResponse);
    List<SuggestedCategoriesWebResponse> suggestedCategoriesWebRespons =
        categoryService.getCategorySuggestionByProductName(PRODUCT_NAME);
    verify(productCategorySuggestionFeign).predictProductCategoriesByProductName(PRODUCT_NAME, 5, 0);
    assertEquals("1", suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryLevel());
    assertEquals(CATEGORY_CODE, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryCode());
    assertEquals(CATEGORY_ID, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryId());
    assertEquals(CATEGORY_NAME, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryName());
    assertEquals(CATEGORY_NAME, suggestedCategoriesWebRespons.get(0).getCategories().get(0).getCategoryNameEnglish());
  }

  @Test
  void validateCategory() {
    when(pcbFeign.validateCategory(CATEGORY_ID)).thenReturn(
        new GdnBaseRestResponse(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorMessage(),
            ErrorCodes.INVALID_CATEGORY_ERROR.getErrorCode(), true, REQUEST_ID));
    BaseResponse baseResponse = categoryService.validateCategory(REQUEST_ID, CATEGORY_ID);
    verify(pcbFeign).validateCategory(CATEGORY_ID);
    assertTrue(baseResponse.isSuccess());
    assertEquals(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorMessage(), baseResponse.getErrorMessage());
    assertEquals(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorCode(), baseResponse.getErrorCode());
  }

  @Test
  void addOriginalSalesCategoryTest() {
    when(pcbFeign.createOriginalSalesCategory(originalSalesCategoryRequest))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, true, REQUEST_ID, ID));
    String response = categoryService.addOriginalSalesCategory(STORE_ID, originalSalesCategoryWebRequest);
    verify(pcbFeign).createOriginalSalesCategory(originalSalesCategoryRequest);
    Assertions.assertEquals(ID, response);
  }

  @Test
  void getOscListingTest() {
    when(pcbFeign.filterOscSummaryResponse(OSC_CODE, KEYWORD, ACTIVATED))
        .thenReturn(new GdnRestListResponse<>(oscSummaryResponseList, pageMetaData, REQUEST_ID));
    List<OscSummaryWebResponse> responses = categoryService.getOscListing(OSC_CODE, KEYWORD, ACTIVATED);
    verify(pcbFeign).filterOscSummaryResponse(OSC_CODE, KEYWORD, ACTIVATED);
    assertNotNull(responses);
    assertEquals(ID, responses.get(0).getId());
    assertTrue(responses.get(0).isActivated());
  }

  @Test
  void getOscListing_NullTest() {
    when(pcbFeign.filterOscSummaryResponse(OSC_CODE, KEYWORD, ACTIVATED))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), pageMetaData, REQUEST_ID));
    List<OscSummaryWebResponse> responses = categoryService.getOscListing(OSC_CODE, KEYWORD, ACTIVATED);
    verify(pcbFeign).filterOscSummaryResponse(OSC_CODE, KEYWORD, ACTIVATED);
    assertNotNull(responses);
  }

  @Test
  void updateOriginalSalesCategoryTest() {
    Mockito.when(pcbFeign.updateOriginalSalesCategory(any(OscInfoUpdateDTO.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    categoryService.updateOriginalSalesCategory(oscInfoUpdateDTO);
    Mockito.verify(pcbFeign).updateOriginalSalesCategory(any(OscInfoUpdateDTO.class));
  }

  @Test
  void updateOriginalSalesCategoryExceptionTest() {
    Mockito.when(pcbFeign.updateOriginalSalesCategory(any(OscInfoUpdateDTO.class)))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      categoryService.updateOriginalSalesCategory(oscInfoUpdateDTO);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign).updateOriginalSalesCategory(any(OscInfoUpdateDTO.class));
    }
  }

  @Test
  void getOriginalSalesCategoryTest() {
    Mockito.when(pcbFeign.getOSCById(ID))
        .thenReturn(new GdnRestSingleResponse<>(originalSalesCategoryResponse, REQUEST_ID));
    OscDetailsWebResponse oscDetailsWebResponse = categoryService.getOriginalSalesCategory(ID);
    Mockito.verify(pcbFeign).getOSCById(ID);
    Assertions.assertEquals(OSC_CODE, oscDetailsWebResponse.getOscCode());
    Assertions.assertEquals(OSC_SHORT_TEXT, oscDetailsWebResponse.getOscShortText());
    Assertions.assertEquals(OSC_LONG_TEXT, oscDetailsWebResponse.getOscLongText());
    Assertions.assertEquals(CATEGORY_CODE, oscDetailsWebResponse.getMasterCategories().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_1, oscDetailsWebResponse.getMasterCategories().get(1).getCategoryCode());
    Assertions.assertEquals(UPDATED_BY, oscDetailsWebResponse.getMasterCategories().get(0).getOscUpdatedBy());
    Assertions.assertEquals(UPDATED_DATE, oscDetailsWebResponse.getMasterCategories().get(0).getOscUpdatedDate());
  }

  @Test
  void getOriginalSalesCategoryWithoutMasterCategoriesTest() {
    originalSalesCategoryResponse.setMasterCategories(null);
    Mockito.when(pcbFeign.getOSCById(ID))
        .thenReturn(new GdnRestSingleResponse<>(originalSalesCategoryResponse, REQUEST_ID));
    OscDetailsWebResponse oscDetailsWebResponse = categoryService.getOriginalSalesCategory(ID);
    Mockito.verify(pcbFeign).getOSCById(ID);
    Assertions.assertEquals(OSC_CODE, oscDetailsWebResponse.getOscCode());
    Assertions.assertEquals(OSC_SHORT_TEXT, oscDetailsWebResponse.getOscShortText());
    Assertions.assertEquals(OSC_LONG_TEXT, oscDetailsWebResponse.getOscLongText());
    Assertions.assertEquals(0, oscDetailsWebResponse.getMasterCategories().size());
  }


  @Test
  void getOriginalSalesCategoryEmptyOriginalSalesCategoryResponseTest() {
    Mockito.when(pcbFeign.getOSCById(ID))
        .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    try {
    OscDetailsWebResponse oscDetailsWebResponse = categoryService.getOriginalSalesCategory(ID);
    } catch (ClientException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(pcbFeign).getOSCById(ID);
    }
  }

  @Test
  void getValueFromPropertiesTest() throws Exception {
    Mockito.when(objectMapper.readValue(eq(RESTRICTED_KEYWORD_ACTION_LISTS), any(TypeReference.class)))
        .thenReturn(restrictedKeywordActionList);
    Mockito.when(objectMapper.readValue(eq(RESTRICTED_KEYWORD_TYPE_LIST), any(TypeReference.class)))
        .thenReturn(restrictedKeywordTypeList);
    categoryService.getValueFromProperties();
    Mockito.verify(objectMapper, Mockito.times(1))
        .readValue(eq(RESTRICTED_KEYWORD_ACTION_LISTS), any(TypeReference.class));
    Mockito.verify(objectMapper, Mockito.times(1))
        .readValue(eq(RESTRICTED_KEYWORD_TYPE_LIST), any(TypeReference.class));
    Assertions.assertEquals(mapper.writeValueAsString(restrictedKeywordActionList), RESTRICTED_KEYWORD_ACTION_LISTS);
    Assertions.assertEquals(mapper.writeValueAsString(restrictedKeywordTypeList), RESTRICTED_KEYWORD_TYPE_LIST);
  }

  @Test
  void getCategoryMarginHierarchyTest() {
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE))
        .thenReturn(new GdnRestListResponse<>(Arrays.asList(categoryResponse), null, REQUEST_ID));
    Mockito.when(marginFeign.filterActiveBasicMargin(new CategoryCodesListRequest(Arrays.asList(
            CATEGORY_CODE)))).thenReturn(new ListBaseResponse<>(null,
            null, true, REQUEST_ID,  Arrays.asList(baseMarginResponse), null));
    categoryService.getCategoryMarginHierarchy(CATEGORY_CODE);
    Mockito.when(pcbFeign.getOSCById(ID))
        .thenReturn(new GdnRestSingleResponse<>(originalSalesCategoryResponse, REQUEST_ID));
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Mockito.verify(marginFeign).filterActiveBasicMargin(new CategoryCodesListRequest(Arrays.asList(CATEGORY_CODE)));
  }



  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeNewMarginTest() {
    ReflectionTestUtils.setField(categoryService,"marginNewChangesEnabled",true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE");
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(
        marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
            eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
        BUSINESS_PARTNER_CODE,
        CATEGORY_CODE);
    Mockito.verify(marginFeign).filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeMarginNullTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeMarginEmptySpecialMarginReplacementTypeBaseTest2() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeSpecialMarginReplacementTypeBaseAddonEmptyTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE-ADDON");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeSpecialMarginReplacementTypeEmptyTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }


  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeBaseMarginReplacementTypEmptyTest5() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeMarginEmptyTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("-");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeMarginisNullTest() {
    ReflectionTestUtils.setField(categoryService, "marginNewChangesEnabled", true);
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    marginList.add(null);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void getProfitMarginTest() {
    MarginCategoryWebResponse response = null;
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    marginList.add(null);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(clientParameterHelper.getClientId()).thenReturn(CLIENT_ID);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(clientParameterHelper.getChannelId()).thenReturn(CHANNEL_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(USERNAME);
    Mockito.when(marginFeign.filterMargin(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), any())).thenReturn(listBaseResponse);
    Mockito.when(xbpFeign.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    categoryService.getProfitMargin(profitMarginWebRequest);
    Mockito.verify(marginFeign)
        .filterMargin(any(), any(), any(), any(), any(), any());
    Mockito.verify(clientParameterHelper).getClientId();
    Mockito.verify(clientParameterHelper).getChannelId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  }
