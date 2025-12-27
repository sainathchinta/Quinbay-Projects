package com.gdn.partners.pcu.master.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.CategoryApiPath;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.request.CategoryCreateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryInfoUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryMappingsUpdateServiceRequest;
import com.gdn.partners.pcu.master.model.request.CategoryStatusChangeServiceRequest;
import com.gdn.partners.pcu.master.service.CategoryService;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.model.request.CategoryAttributeUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryCreateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryInfoUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryKeywordUpdateWebRequestList;
import com.gdn.partners.pcu.master.web.model.request.CategoryKeywordsUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryMappingsUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.CategoryRestrictedKeywordsWebRequest;
import com.gdn.partners.pcu.master.web.model.request.MinWholesaleDiscountWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.request.OriginalSalesCategoryWebRequest;
import com.gdn.partners.pcu.master.web.model.request.ProfitMarginWebRequest;
import com.gdn.partners.pcu.master.web.model.request.WholesaleConfigWebRequest;
import com.gdn.partners.pcu.master.web.model.request.WholesaleMappingWebRequest;
import com.gdn.partners.pcu.master.web.model.response.CatalogTreeWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryInfoWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryMappingResponse;
import com.gdn.partners.pcu.master.web.model.response.CreateCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.DocumentWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MarginCategoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.MinWholesaleDiscountWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscDetailsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.OscSummaryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.ProfitMarginWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleConfigWebResponse;
import com.gdn.partners.pcu.master.web.model.response.WholesaleMappingWebResponse;
import com.gdn.x.productcategorybase.dto.OscInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;

@ExtendWith(MockitoExtension.class)
public class CategoryControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private CategoryService categoryService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private CategoryController categoryController;

  @Captor
  private ArgumentCaptor<CategoryInfoUpdateServiceRequest> categoryInfoUpdateServiceRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryMappingsUpdateServiceRequest> categoryMappingsUpdateServiceRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<CategoryCreateServiceRequest> categoryCreateServiceRequestArgumentCaptor;



  private CategoryMappingsUpdateWebRequest categoryMappingsUpdateWebRequest;
  private CategoryCreateWebRequest categoryCreateWebRequest;
  private OriginalSalesCategoryWebRequest originalSalesCategoryWebRequest;
  private CategoryInfoUpdateWebRequest categoryInfoUpdateWebRequest;
  private List<String> categoryCodes;
  private CategoryInfoWebResponse categoryInfoWebResponse;
  private List<CategoryMappingResponse> categoryMappingResponses;
  private CategoryMappingResponse categoryMappingResponse;
  private List<CategoryResponse> categoryResponseList;
  private RestrictedKeywordsWebResponse restrictedKeywordsWebResponse;
  private List<RestrictedKeywordsWebResponse> restrictedKeywordsWebResponseList;
  private CategoryRestrictedKeywordsWebRequest categoryRestrictedKeywordsWebRequest;
  private CategoryKeywordsUpdateWebRequest categoryKeywordsUpdateWebRequest;
  private CategoryKeywordUpdateWebRequestList categoryKeywordUpdateWebRequestList;
  private WholesaleMappingWebResponse wholesaleMappingWebResponse;
  private WholesaleConfigWebResponse wholesaleConfigWebResponse;
  private MinWholesaleDiscountWebResponse minWholesaleDiscountWebResponse;
  private WholesaleMappingWebRequest wholesaleMappingWebRequest;
  private OriginalSalesCategoryUpdateWebRequest oscUpdateRequest;
  private WholesaleConfigWebRequest wholesaleConfigWebRequest;
  private MinWholesaleDiscountWebRequest minWholesaleDiscountWebRequest;
  private CategoryTreeResponse categoryTreeResponse;
  private CatalogTreeWebResponse catalogTreeWebResponse;
  private List<CategoryTreeResponse> children;
  private List<OscSummaryWebResponse> oscSummaryWebResponses = new ArrayList<>();
  private DocumentWebResponse documentWebResponse;
  private OscDetailsWebResponse oscDetailsWebResponse;
  private ProfitMarginWebRequest profitMarginWebRequest;
  private ProfitMarginWebResponse profitMarginWebResponse;

  private static final String STORE_ID = "storeId";
  private static final String USER_NAME = "userName";
  private static final String USER = "username";
  private static final String CLIENT_ID = "web";
  private static final String CHANNEL_ID = "web";
  private static final String ID = "id";
  private static final String PARENT_CATEGORY_ID = "parentCategoryId";
  private static final String NAME_ENGLISH = "nameEnglish";
  private static final byte[] DESCRIPTION = "defaultDescription".getBytes();
  private static final byte[] DESCRIPTION_ENGLISH = "descriptionEnglish".getBytes();
  private static final Integer INTERNAL_ACTIVATION_INTERVAL = 100;
  private static final Integer LOGISTIC_ADJUSTMENT = 10;
  private static final Integer SEQUENCE = 5;
  private static final String REQUEST_ID = "REQ-001";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_CODE_2 = "category_code_2";
  private static final String CATEGORY_NAME = "category_name";
  private static final String CHILD_CATEGORY_NAME = "child_category_name";
  private static final String CATEGORY_ID = "category_id";
  private static final String NAME = "name";
  private static final String ATTRIBUTE_ID_1 = "attributeId1";
  private static final String ATTRIBUTE_ID_2 = "attributeId2";
  private static final String MASTER_CATEGORY_ID_1 = "masterCategoryId1";
  private static final String MASTER_CATEGORY_ID_2 = "masterCategoryId2";
  private static final String CATALOG_ID = "catalogId";
  private static final String PARENT_ID = "parentId";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final int DEFAULT_DANGEROUS_GOODS_LEVEL = 0;
  private static final String KEYWORD = "keyword";
  private static final String OSC_CODE = "oscCode";
  private static final Boolean ACTIVATED = true;
  private static final String KEYWORD_ID = "keywordId";
  private static final String KEYWORD1 = "keyword1";
  private static final String KEYWORD_ID1 = "keywordId1";
  private static final String GENERIC_TEMPLATE_ELIGIBLE = "genericTemplateEligible";
  private Pageable pageable;
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final Integer QUANTITY = 10;
  private static final Double PERCENTAGE = 10D;
  private static final Double PRICE = 100000D;
  private static final String CONFIGURATION_TYPE = "PERCENTAGE";
  private static final boolean WHOLESALE_CONFIG_ENABLED = true;
  private static final String DOCUMENTS = "Doctor's prescription, Passport, Driving License";
  private static final String PRODUCT_NAME = "productName";
  public static final String RESTRICTED_KEYWORD_MAP =
      "{\"restrictedKeywordTypeList\":[\"Bad\",\"Misc\"],\"restrictedKeywordActionList\":[{\"enName\":\"Straight rejection\",\"inName\":\"Straight rejection\",\"value\":\"3\"},{\"enName\":\"Need revision\",\"inName\":\"Need revision\",\"value\":\"2\"},{\"enName\":\"Change category to\",\"inName\":\"Change category to\",\"value\":\"1\"},{\"enName\":\"No action\",\"inName\":\"No action\",\"value\":\"0\"}]}";
  private CategoryKeywordsUpdateWebRequest categoryKeywordsUpdateWebRequest1;

  @BeforeEach
  void setUp() {
    mockMvc = MockMvcBuilders.standaloneSetup(categoryController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    categoryInfoWebResponse = new CategoryInfoWebResponse();
    categoryInfoWebResponse.setId(CATEGORY_ID);
    categoryInfoWebResponse.setName(NAME);
    categoryInfoWebResponse.setDocumentType(DOCUMENTS);

    categoryInfoUpdateWebRequest = CategoryInfoUpdateWebRequest.builder().categoryCode(CATEGORY_CODE)
        .id(ID).name(NAME).nameEnglish(NAME_ENGLISH).defaultDescription(DESCRIPTION)
        .descriptionEnglish(DESCRIPTION_ENGLISH).display(true).deliveredByMerchant(false)
        .internalActivationInterval(INTERNAL_ACTIVATION_INTERVAL).logisticAdjustment(LOGISTIC_ADJUSTMENT)
        .directFlight(true).specialHandling(true).sequence(SEQUENCE).warranty(true).needIdentity(true)
        .ageLimit(true).parentCategoryId(PARENT_CATEGORY_ID).umkm(true).sizeChartRequired(true).build();
    CategoryAttributeUpdateWebRequest categoryAttributeUpdateWebRequest1 =
        CategoryAttributeUpdateWebRequest.builder().attributeId(ATTRIBUTE_ID_1)
            .mainDefiningAttribute(true).usp(false).sequence(1).build();
    CategoryAttributeUpdateWebRequest categoryAttributeUpdateWebRequest2 =
        CategoryAttributeUpdateWebRequest.builder().attributeId(ATTRIBUTE_ID_2)
            .mainDefiningAttribute(false).usp(false).sequence(2).build();
    categoryKeywordsUpdateWebRequest = CategoryKeywordsUpdateWebRequest.builder().
        keyword(KEYWORD).keywordId(KEYWORD_ID).build();
    categoryKeywordsUpdateWebRequest1= CategoryKeywordsUpdateWebRequest
        .builder().keyword(KEYWORD1).keywordId(KEYWORD_ID1).build();
    wholesaleMappingWebRequest =
        WholesaleMappingWebRequest.builder().configurationType("PERCENTAGE")
            .wholesaleConfig(new ArrayList<>()).build();
    categoryMappingsUpdateWebRequest = CategoryMappingsUpdateWebRequest.builder()
        .addedAttributes(Collections.singletonList(categoryAttributeUpdateWebRequest1))
        .deletedAttributes(Collections.singletonList(categoryAttributeUpdateWebRequest2))
        .addedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_1))
        .deletedMasterCategoryIds(Collections.singletonList(MASTER_CATEGORY_ID_2))
        .addedKeywords(Collections.singletonList(categoryKeywordsUpdateWebRequest))
        .deletedKeywords(Collections.singletonList(categoryKeywordsUpdateWebRequest1))
        .wholesaleMapping(wholesaleMappingWebRequest)
        .build();
    categoryCreateWebRequest = CategoryCreateWebRequest.builder()
        .categoryInfoDetail(categoryInfoUpdateWebRequest)
        .categoryMappingsDetail(categoryMappingsUpdateWebRequest).catalogId(CATALOG_ID)
        .build();

    originalSalesCategoryWebRequest = new OriginalSalesCategoryWebRequest();

    categoryMappingResponse = new CategoryMappingResponse();
    categoryMappingResponse.setId(CATEGORY_ID);
    categoryMappingResponse.setName(NAME);
    categoryMappingResponses = new ArrayList<>();
    categoryMappingResponses.add(categoryMappingResponse);
    categoryCodes = Arrays.asList(CATEGORY_CODE);

    categoryKeywordUpdateWebRequestList = CategoryKeywordUpdateWebRequestList.builder()
        .addedKeywords(Collections.singletonList(categoryKeywordsUpdateWebRequest)).build();


    categoryRestrictedKeywordsWebRequest =
        CategoryRestrictedKeywordsWebRequest.builder().categoryCode(CATEGORY_CODE).keyword(KEYWORD).build();
    restrictedKeywordsWebResponse =
        RestrictedKeywordsWebResponse.builder().keyword(KEYWORD).keywordId(KEYWORD_ID).selected(false).build();
    restrictedKeywordsWebResponseList = new ArrayList<>();
    restrictedKeywordsWebResponseList.add(restrictedKeywordsWebResponse);
    pageable = PageRequest.of(PAGE, SIZE);

    minWholesaleDiscountWebResponse = new MinWholesaleDiscountWebResponse();
    minWholesaleDiscountWebResponse.setPercentage(PERCENTAGE);
    minWholesaleDiscountWebResponse.setPrice(PRICE);
    wholesaleConfigWebResponse = new WholesaleConfigWebResponse();
    wholesaleConfigWebResponse.setQuantity(QUANTITY);
    wholesaleConfigWebResponse.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscountWebResponse));
    wholesaleMappingWebResponse = new WholesaleMappingWebResponse();
    wholesaleMappingWebResponse.setConfigurationType(CONFIGURATION_TYPE);
    wholesaleMappingWebResponse.setWholesaleConfig(Collections.singletonList(wholesaleConfigWebResponse));
    wholesaleMappingWebResponse.setWholesalePriceConfigEnabled(WHOLESALE_CONFIG_ENABLED);

    minWholesaleDiscountWebRequest = new MinWholesaleDiscountWebRequest();
    minWholesaleDiscountWebRequest.setPercentage(PERCENTAGE);
    minWholesaleDiscountWebRequest.setPrice(PRICE);
    wholesaleConfigWebRequest = new WholesaleConfigWebRequest();
    wholesaleConfigWebRequest.setQuantity(QUANTITY);
    wholesaleConfigWebRequest.setMinWholesaleDiscount(Collections.singletonList(minWholesaleDiscountWebRequest));
    wholesaleMappingWebRequest = new WholesaleMappingWebRequest();
    wholesaleMappingWebRequest.setConfigurationType(CONFIGURATION_TYPE);
    wholesaleMappingWebRequest.setWholesaleConfig(Collections.singletonList(wholesaleConfigWebRequest));
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
    catalogTreeWebResponse = new CatalogTreeWebResponse();
    catalogTreeWebResponse.setCatalogCode(CATALOG_ID);
    catalogTreeWebResponse.setCategories(new ArrayList<>());
    oscDetailsWebResponse = new OscDetailsWebResponse();
    profitMarginWebRequest = new ProfitMarginWebRequest();

    documentWebResponse =
        DocumentWebResponse.builder().documentList(Arrays.stream(DOCUMENTS.split(",")).collect(Collectors.toList()))
            .build();
    oscUpdateRequest = OriginalSalesCategoryUpdateWebRequest.builder().activated(true).build();
  }

  @AfterEach
  void tearDown() {
    verifyNoMoreInteractions(categoryService);
    verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void updateCategoryInfoTest() throws Exception {
    categoryInfoUpdateWebRequest.setDocumentType(DOCUMENTS);
    when(categoryService.updateCategoryInfo(
        any(CategoryInfoUpdateServiceRequest.class))).thenReturn(new GdnBaseRestResponse(true));

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_INFO, CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryInfoUpdateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).updateCategoryInfo(categoryInfoUpdateServiceRequestArgumentCaptor.capture());
    assertEquals(STORE_ID, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getStoreId());
    assertEquals(NAME, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getName());
    assertEquals(NAME_ENGLISH, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION),
        Arrays.toString(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH),
        Arrays.toString(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getDescriptionEnglish()));
    assertEquals(CATEGORY_CODE, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getCategoryCode());
    assertEquals(SEQUENCE, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getSequence());
    assertEquals(USER, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getUpdatedBy());
    assertEquals(PARENT_CATEGORY_ID, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getParentCategoryId());
    assertEquals(LOGISTIC_ADJUSTMENT,
        categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getLogisticAdjustment());
    assertEquals(INTERNAL_ACTIVATION_INTERVAL,
        categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getInternalActivationInterval());
    assertFalse(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isDeliveredByMerchant());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isDisplay());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isDirectFlight());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isWarranty());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isSpecialHandling());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isNeedIdentity());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getAgeLimit());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isUmkm());
    assertTrue(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().isSizeChartRequired());
    assertEquals(DEFAULT_DANGEROUS_GOODS_LEVEL, categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getDangerousGoodsLevel());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
    assertTrue(DOCUMENTS.equals(categoryInfoUpdateServiceRequestArgumentCaptor.getValue().getDocumentType()));
  }

  @Test
  void getCategoryInfoWithShippingDetailsSuccessTrueTest() throws Exception {
    when(this.categoryService.getCategoryInfoWithShippingDetail(CATEGORY_ID, REQUEST_ID, false))
        .thenReturn(new SingleBaseResponse<>(null, null, true, REQUEST_ID, categoryInfoWebResponse));
    when(clientParameterHelper.getRequestId())
        .thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.INFO, CATEGORY_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).getCategoryInfoWithShippingDetail(CATEGORY_ID, REQUEST_ID, false);
  }

  @Test
  void activateTest() throws Exception {
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = new CategoryStatusChangeServiceRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.ACTIVATE, CATEGORY_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(toJson(categoryStatusChangeServiceRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(categoryService).activate(REQUEST_ID, categoryStatusChangeServiceRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  void deactivateTest() throws Exception {
    CategoryStatusChangeServiceRequest categoryStatusChangeServiceRequest = new CategoryStatusChangeServiceRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.DEACTIVATE, CATEGORY_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(toJson(categoryStatusChangeServiceRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(categoryService).deactivate(REQUEST_ID, categoryStatusChangeServiceRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  void getCategoryInfoWithShippingDetailsSuccessFalseTest() throws Exception {
    when(clientParameterHelper.getRequestId())
        .thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.INFO, StringUtils.SPACE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void updateCategoryMappingsTest() throws Exception {
    when(categoryService.updateCategoryMappings(
        any(CategoryMappingsUpdateServiceRequest.class))).thenReturn(new GdnBaseRestResponse(true));

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_MAPPINGS, ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryMappingsUpdateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).updateCategoryMappings(categoryMappingsUpdateServiceRequestArgumentCaptor.capture());
    assertEquals(STORE_ID, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue().getStoreId());
    assertEquals(ID, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue().getId());
    assertEquals(ATTRIBUTE_ID_1, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).getAttributeId());
    assertEquals(1, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).getSequence(), 0);
    assertTrue(categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getAddedAttributes().get(0).isMainDefiningAttribute());
    assertFalse(categoryMappingsUpdateServiceRequestArgumentCaptor.getValue().getAddedAttributes().get(0).isUsp());
    assertEquals(ATTRIBUTE_ID_2, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getDeletedAttributes().get(0).getAttributeId());
    assertEquals(MASTER_CATEGORY_ID_1, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getAddedMasterCategoryIds().get(0));
    assertEquals(MASTER_CATEGORY_ID_2, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue()
        .getDeletedMasterCategoryIds().get(0));
    assertEquals(USER, categoryMappingsUpdateServiceRequestArgumentCaptor.getValue().getUpdatedBy());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  void createCategoryTest() throws Exception {
    categoryCreateWebRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    when(categoryService.createCategory(any(CategoryCreateServiceRequest.class)))
        .thenReturn(new CreateCategoryWebResponse(CATEGORY_CODE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCreateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY_CODE)));

    verify(categoryService)
        .createCategory(categoryCreateServiceRequestArgumentCaptor.capture());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(3)).getStoreId();
    verify(clientParameterHelper, times(3)).getUsername();
    CategoryCreateServiceRequest categoryCreateServiceRequest =
        categoryCreateServiceRequestArgumentCaptor.getValue();
    assertEquals(STORE_ID,
        categoryCreateServiceRequest.getStoreId());
    assertEquals(ATTRIBUTE_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getAttributeId());
    assertEquals(1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getSequence(), 0);
    assertTrue(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isMainDefiningAttribute());
    assertFalse(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isUsp());
    assertEquals(MASTER_CATEGORY_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedMasterCategoryIds()
            .get(0));
    assertEquals(USER, categoryCreateServiceRequest.getCreatedBy());
    assertEquals(CATALOG_ID, categoryCreateServiceRequest.getCatalogId());
    assertEquals(NAME,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getName());
    assertEquals(NAME_ENGLISH,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest()
            .getDescriptionEnglish()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDisplay());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDirectFlight());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isUmkm());
    assertTrue(DOCUMENTS.equals(
        categoryCreateServiceRequestArgumentCaptor.getValue().getCategoryInfoUpdateServiceRequest().getDocumentType()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isSizeChartRequired());
  }

  @Test
  void createCategoryTest_nonNullWholesaleConfigWebRequest() throws Exception {
    categoryCreateWebRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryCreateWebRequest.getCategoryMappingsDetail().getWholesaleMapping().setWholesaleConfig(
        Collections.singletonList(new WholesaleConfigWebRequest(1,
            Collections.singletonList(new MinWholesaleDiscountWebRequest(1d, 1.2)))));
    when(categoryService.createCategory(any(CategoryCreateServiceRequest.class)))
        .thenReturn(new CreateCategoryWebResponse(CATEGORY_CODE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCreateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY_CODE)));

    verify(categoryService)
        .createCategory(categoryCreateServiceRequestArgumentCaptor.capture());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(3)).getStoreId();
    verify(clientParameterHelper, times(3)).getUsername();
    CategoryCreateServiceRequest categoryCreateServiceRequest =
        categoryCreateServiceRequestArgumentCaptor.getValue();
    assertEquals(STORE_ID,
        categoryCreateServiceRequest.getStoreId());
    assertEquals(ATTRIBUTE_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getAttributeId());
    assertEquals(1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getSequence(), 0);
    assertTrue(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isMainDefiningAttribute());
    assertFalse(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isUsp());
    assertEquals(MASTER_CATEGORY_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedMasterCategoryIds()
            .get(0));
    assertEquals(USER, categoryCreateServiceRequest.getCreatedBy());
    assertEquals(CATALOG_ID, categoryCreateServiceRequest.getCatalogId());
    assertEquals(NAME,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getName());
    assertEquals(NAME_ENGLISH,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest()
            .getDescriptionEnglish()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDisplay());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDirectFlight());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isUmkm());
    assertTrue(DOCUMENTS.equals(
        categoryCreateServiceRequestArgumentCaptor.getValue().getCategoryInfoUpdateServiceRequest().getDocumentType()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isSizeChartRequired());
  }

  @Test
  void createCategoryTest_nullWholesaleConfigWebRequest() throws Exception {
    categoryCreateWebRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryCreateWebRequest.getCategoryMappingsDetail().getWholesaleMapping().setWholesaleConfig(
        Collections.singletonList(new WholesaleConfigWebRequest(1,
            Collections.singletonList(null))));
    when(categoryService.createCategory(any(CategoryCreateServiceRequest.class)))
        .thenReturn(new CreateCategoryWebResponse(CATEGORY_CODE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCreateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY_CODE)));

    verify(categoryService)
        .createCategory(categoryCreateServiceRequestArgumentCaptor.capture());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(3)).getStoreId();
    verify(clientParameterHelper, times(3)).getUsername();
    CategoryCreateServiceRequest categoryCreateServiceRequest =
        categoryCreateServiceRequestArgumentCaptor.getValue();
    assertEquals(STORE_ID,
        categoryCreateServiceRequest.getStoreId());
    assertEquals(ATTRIBUTE_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getAttributeId());
    assertEquals(1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getSequence(), 0);
    assertTrue(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isMainDefiningAttribute());
    assertFalse(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isUsp());
    assertEquals(MASTER_CATEGORY_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedMasterCategoryIds()
            .get(0));
    assertEquals(USER, categoryCreateServiceRequest.getCreatedBy());
    assertEquals(CATALOG_ID, categoryCreateServiceRequest.getCatalogId());
    assertEquals(NAME,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getName());
    assertEquals(NAME_ENGLISH,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest()
            .getDescriptionEnglish()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDisplay());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDirectFlight());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isUmkm());
    assertTrue(DOCUMENTS.equals(
        categoryCreateServiceRequestArgumentCaptor.getValue().getCategoryInfoUpdateServiceRequest().getDocumentType()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isSizeChartRequired());
  }

  @Test
  void createCategoryTest_nullMinWholesaleDiscountWebRequest() throws Exception {
    categoryCreateWebRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryCreateWebRequest.getCategoryMappingsDetail().getWholesaleMapping().setWholesaleConfig(
        Collections.singletonList(null));
    when(categoryService.createCategory(any(CategoryCreateServiceRequest.class)))
        .thenReturn(new CreateCategoryWebResponse(CATEGORY_CODE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCreateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY_CODE)));

    verify(categoryService)
        .createCategory(categoryCreateServiceRequestArgumentCaptor.capture());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(3)).getStoreId();
    verify(clientParameterHelper, times(3)).getUsername();
    CategoryCreateServiceRequest categoryCreateServiceRequest =
        categoryCreateServiceRequestArgumentCaptor.getValue();
    assertEquals(STORE_ID,
        categoryCreateServiceRequest.getStoreId());
    assertEquals(ATTRIBUTE_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getAttributeId());
    assertEquals(1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getSequence(), 0);
    assertTrue(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isMainDefiningAttribute());
    assertFalse(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isUsp());
    assertEquals(MASTER_CATEGORY_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedMasterCategoryIds()
            .get(0));
    assertEquals(USER, categoryCreateServiceRequest.getCreatedBy());
    assertEquals(CATALOG_ID, categoryCreateServiceRequest.getCatalogId());
    assertEquals(NAME,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getName());
    assertEquals(NAME_ENGLISH,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getNameEnglish());
    assertEquals(Arrays.toString(DESCRIPTION), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDefaultDescription()));
    assertEquals(Arrays.toString(DESCRIPTION_ENGLISH), Arrays.toString(
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest()
            .getDescriptionEnglish()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDisplay());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDirectFlight());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isUmkm());
    assertTrue(DOCUMENTS.equals(
        categoryCreateServiceRequestArgumentCaptor.getValue().getCategoryInfoUpdateServiceRequest().getDocumentType()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isSizeChartRequired());
  }

  @Test
  void createCategoryTest_nullDescription() throws Exception {
    categoryCreateWebRequest.getCategoryInfoDetail().setDocumentType(DOCUMENTS);
    categoryCreateWebRequest.getCategoryInfoDetail().setDefaultDescription(null);
    categoryCreateWebRequest.getCategoryInfoDetail().setDescriptionEnglish(null);
    when(categoryService.createCategory(any(CategoryCreateServiceRequest.class)))
        .thenReturn(new CreateCategoryWebResponse(CATEGORY_CODE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCreateWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value.categoryCode", equalTo(CATEGORY_CODE)));

    verify(categoryService)
        .createCategory(categoryCreateServiceRequestArgumentCaptor.capture());
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(3)).getStoreId();
    verify(clientParameterHelper, times(3)).getUsername();
    CategoryCreateServiceRequest categoryCreateServiceRequest =
        categoryCreateServiceRequestArgumentCaptor.getValue();
    assertEquals(STORE_ID,
        categoryCreateServiceRequest.getStoreId());
    assertEquals(ATTRIBUTE_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getAttributeId());
    assertEquals(1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .getSequence(), 0);
    assertTrue(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isMainDefiningAttribute());
    assertFalse(
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedAttributes().get(0)
            .isUsp());
    assertEquals(MASTER_CATEGORY_ID_1,
        categoryCreateServiceRequest.getCategoryMappingsUpdateServiceRequest().getAddedMasterCategoryIds()
            .get(0));
    assertEquals(USER, categoryCreateServiceRequest.getCreatedBy());
    assertEquals(CATALOG_ID, categoryCreateServiceRequest.getCatalogId());
    assertEquals(NAME,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getName());
    assertEquals(NAME_ENGLISH,
        categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getNameEnglish());
    assertNull(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDefaultDescription());
    assertNull(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().getDescriptionEnglish());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDisplay());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isDirectFlight());
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isUmkm());
    assertTrue(DOCUMENTS.equals(
        categoryCreateServiceRequestArgumentCaptor.getValue().getCategoryInfoUpdateServiceRequest().getDocumentType()));
    assertTrue(categoryCreateServiceRequest.getCategoryInfoUpdateServiceRequest().isSizeChartRequired());
    categoryCreateWebRequest.getCategoryInfoDetail().setDefaultDescription(DESCRIPTION);
    categoryCreateWebRequest.getCategoryInfoDetail().setDescriptionEnglish(DESCRIPTION_ENGLISH);
  }

  @Test
  void getCategoryMappingTest() throws Exception {
    when(categoryService.getSalesCategoryMappingByCategoryCodes(categoryCodes, CATALOG_ID))
        .thenReturn(categoryMappingResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_MAPPINGS_BY_CATEGORY_CODES)
            .param(CATALOG_ID, CATALOG_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryCodes))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).getSalesCategoryMappingByCategoryCodes(categoryCodes, CATALOG_ID);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getMarginByCategoryCodeTest() throws Exception{
    when(this.categoryService.getMarginByCategoryCode(CATEGORY_CODE)).thenReturn(new MarginCategoryWebResponse());
    when(clientParameterHelper.getRequestId())
        .thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_MARGIN_BY_CATEGORY_CODE, CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).getMarginByCategoryCode(CATEGORY_CODE);
  }

  @Test
  void getUsedSequenceTest() throws Exception {
    categoryResponseList = generateCategoryResponses();
    when(clientParameterHelper.getRequestId())
        .thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.findCategorySummaryByParentId(PARENT_ID))
        .thenReturn(categoryResponseList);

    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_USED_SEQUENCE, PARENT_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).findCategorySummaryByParentId(PARENT_ID);
    verify(clientParameterHelper).getRequestId();
  }

  private List<CategoryResponse> generateCategoryResponses() {
    List<CategoryResponse> categoryResponseList = new ArrayList<CategoryResponse>();
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setSequence(1);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setSequence(2);
    categoryResponseList.add(categoryResponse1);
    categoryResponseList.add(categoryResponse2);
    return categoryResponseList;
  }

  @Test
  void getMarginByBusinessPartnerCodeAndCategoryCodeTest() throws Exception {
    when(this.categoryService.getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
        BUSINESS_PARTNER_CODE, CATEGORY_CODE))
        .thenReturn(new MarginCategoryWebResponse());
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder mockHttpServletRequestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_MARGIN_BY_BUSINESS_PARTNER_CODE_AND_CATEGORY_CODE,
        BUSINESS_PARTNER_CODE, CATEGORY_CODE).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(mockHttpServletRequestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).getMarginBusinessPartnerByBusinessPartnerCodeAndCategoryCode(
        BUSINESS_PARTNER_CODE, CATEGORY_CODE);
  }

  @Test
  void updateCategoryRestrictedKeywordMappingTest() throws Exception {
    when(this.categoryService.updateCategoryRestrictedKeywordMappings(eq(CATEGORY_CODE),
        any(CategoryKeywordUpdateRequestList.class))).thenReturn(new GdnBaseRestResponse(true));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_RESTRICTED_KEYWORD_MAPPINGS, CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(categoryKeywordUpdateWebRequestList))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService).updateCategoryRestrictedKeywordMappings(eq(CATEGORY_CODE),
        any(CategoryKeywordUpdateRequestList.class));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  void getCategoryRestrictedKeywordsTest() throws Exception {
    Page<RestrictedKeywordsWebResponse> restrictedKeywordsWebResponses =
        new PageImpl<>(restrictedKeywordsWebResponseList, pageable, restrictedKeywordsWebResponseList.size());
    when(this.categoryService.findRestrictedKeywords(categoryRestrictedKeywordsWebRequest, pageable))
        .thenReturn(restrictedKeywordsWebResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder mockHttpServletRequestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_RESTRICTED_KEYWORD)
            .content(toJson(categoryRestrictedKeywordsWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(mockHttpServletRequestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).findRestrictedKeywords(categoryRestrictedKeywordsWebRequest, pageable);
  }

  @Test
  void getCategoryWholesaleConfigTest() throws Exception {
    when(this.categoryService.findWholesaleConfig(CATEGORY_ID, CATEGORY_CODE)).thenReturn(wholesaleMappingWebResponse);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder mockHttpServletRequestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_WHOLESALE_CONFIG)
            .contentType(MediaType.APPLICATION_JSON).param("categoryId", CATEGORY_ID)
            .param("categoryCode", CATEGORY_CODE).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(mockHttpServletRequestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).findWholesaleConfig(CATEGORY_ID, CATEGORY_CODE);
  }

  @Test
  void updateCategoryWholesaleConfigMappingTest() throws Exception {
    when(this.categoryService.updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_WHOLESALE_CONFIG_MAPPING, CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(wholesaleMappingWebRequest)).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService)
        .updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void updateCategoryWholesaleConfigMappingTest_nullWholesaleConfigWebRequest() throws Exception {
    when(this.categoryService.updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    wholesaleMappingWebRequest.setWholesaleConfig(Collections.singletonList(null));
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_WHOLESALE_CONFIG_MAPPING, CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(wholesaleMappingWebRequest)).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService)
        .updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void updateCategoryWholesaleConfigMappingTest_nullMinWholesaleDiscountWebRequest() throws Exception {
    when(this.categoryService.updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    wholesaleMappingWebRequest.getWholesaleConfig().get(0)
        .setMinWholesaleDiscount(Collections.singletonList(null));
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_CATEGORY_WHOLESALE_CONFIG_MAPPING, CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(wholesaleMappingWebRequest)).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService)
        .updateCategoryWholesaleConfigMapping(eq(CATEGORY_ID), any(WholesaleMappingRequest.class));
    verify(clientParameterHelper).getRequestId();
  }

  void getCategoryListForGenericTemplateTest() throws Exception {
    when(this.categoryService.getCategoryListForGenericTemplate(true)).thenReturn(Arrays.asList(categoryTreeResponse));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_LIST_FOR_GENERIC_TEMPLATE)
            .param(GENERIC_TEMPLATE_ELIGIBLE, String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.categoryService).getCategoryListForGenericTemplate(true);
  }

  @Test
  void getCategoryTreeTest() throws Exception {
    CategoryCodeRequest request = new CategoryCodeRequest();
    request.setCategoryCodes(categoryCodes);
    when(this.categoryService.getCategoryTreeForCategoryCodes(any(CategoryCodeRequest.class)))
        .thenReturn(Collections.singletonList(catalogTreeWebResponse));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = post(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_TREE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(request))
        .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService).getCategoryTreeForCategoryCodes(any(CategoryCodeRequest.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getSupportingDocumentsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getDocumentList()).thenReturn(documentWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_DOCUMENT_LIST).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).getDocumentList();
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getSystemParamValueTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getSystemParamValue(PRODUCT_NAME)).thenReturn(documentWebResponse);

    MockHttpServletRequestBuilder requestBuilder =
      get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_SYSTEM_PARAM_VALUE).param("variable",
          PRODUCT_NAME).contentType(MediaType.APPLICATION_JSON_VALUE)
        .accept(MediaType.APPLICATION_JSON_VALUE)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).getSystemParamValue(PRODUCT_NAME);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getCategoryPredictionsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getCategorySuggestionByProductName(PRODUCT_NAME)).thenReturn(new ArrayList<>());

    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_CATEGORY_SUGGESTIONS).param("productName", PRODUCT_NAME)
            .accept(MediaType.APPLICATION_JSON_VALUE).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).getCategorySuggestionByProductName(PRODUCT_NAME);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void validateCategoryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(this.categoryService.validateCategory(REQUEST_ID, CATEGORY_ID))
        .thenReturn(new BaseResponse(null, null, true, REQUEST_ID));
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.VALIDATE_CATEGORY, CATEGORY_ID)
            .accept(MediaType.APPLICATION_JSON_VALUE).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(categoryService).validateCategory(REQUEST_ID, CATEGORY_ID);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void createOriginalSalesCategoryTest() throws Exception {
    when(categoryService.addOriginalSalesCategory(Mockito.anyString(), any(OriginalSalesCategoryWebRequest.class)))
        .thenReturn(ID);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.ADD_ORIGINAL_SALES_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(originalSalesCategoryWebRequest)).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USER_NAME);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.value", equalTo(ID)));
    verify(categoryService).addOriginalSalesCategory(Mockito.anyString(), any(OriginalSalesCategoryWebRequest.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getOscListingTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getOscListing(OSC_CODE, KEYWORD, ACTIVATED)).thenReturn(oscSummaryWebResponses);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.FETCH_OSC_LIST).param("oscCode", OSC_CODE)
            .param("keyword", KEYWORD).param("activated", String.valueOf(ACTIVATED))
            .accept(MediaType.APPLICATION_JSON_VALUE).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(categoryService).getOscListing(OSC_CODE, KEYWORD, ACTIVATED);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void updateOriginalSalesCategoryTest() throws Exception {
    when(this.categoryService.updateOriginalSalesCategory(any(OscInfoUpdateDTO.class)))
        .thenReturn(new GdnBaseRestResponse(true));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        put(CategoryApiPath.BASE_PATH + CategoryApiPath.UPDATE_OSC_LIST).contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(oscUpdateRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.categoryService).updateOriginalSalesCategory(any(OscInfoUpdateDTO.class));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getOscDetailsByIdTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getOriginalSalesCategory(ID)).thenReturn(oscDetailsWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_ORIGINAL_SALES_CATEGORY_DETAILS_BY_ID, ID)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(categoryService).getOriginalSalesCategory(ID);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getValueFromPropertiesTest() throws Exception {
    Map<String, Object> restrictedKeywordMap = objectMapper.readValue(RESTRICTED_KEYWORD_MAP, Map.class);
    when(this.categoryService.getValueFromProperties()).thenReturn(restrictedKeywordMap);
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.GET_VALUE_FROM_PROPERTIES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(categoryService).getValueFromProperties();
  }

  @Test
  void fetchBaseMarginHierarchyTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.categoryService.getCategoryMarginHierarchy(CATEGORY_CODE)).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        get(CategoryApiPath.BASE_PATH + CategoryApiPath.FETCH_BASE_MARGIN_HIERARCHY, CATEGORY_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(categoryService).getCategoryMarginHierarchy(CATEGORY_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void profitMarginTest() throws Exception {
    when(categoryService.getProfitMargin(profitMarginWebRequest)).thenReturn(profitMarginWebResponse);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(CategoryApiPath.BASE_PATH + CategoryApiPath.PROFIT_MARGIN)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(profitMarginWebRequest))
            .sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(categoryService).getProfitMargin(profitMarginWebRequest);
    verify(clientParameterHelper).getRequestId();
  }

}
