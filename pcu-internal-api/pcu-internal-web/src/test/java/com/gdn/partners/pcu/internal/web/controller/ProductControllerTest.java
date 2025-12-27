package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.TreeMap;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeWebResponse;
import jakarta.servlet.ServletException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.io.IOUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.NestedServletException;

import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ProductApiPath;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductReturnForCorrectionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SelectedMasterProductDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeCheckResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductReviewerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;


@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class ProductControllerTest extends TestHelper {

  private static final String TYPE = "productDraftInternal";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String masterProductId = "master-product-id";
  private static final String duplicateProductId = "duplicate-product-id";
  private static final String DEFAULT_STORE_ID = "storeID";
  private static final String GENERATED_ITEM_NAME = "generated-item-name";
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTR_VALUE = "attr-value";
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String ALLOWED_ATTR_VALUE = "allowed-attr-value";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTR_VALUE = "predefined-allowed-attr-value";
  private static final String USER_NAME = "username";
  private static final String BRAND = "brand";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_TYPE = "3";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATALOG_CODE = "catalog-code";
  private static final String INTERNAL_ACTIVATION_PERIOD = "12";
  private static final String SEARCH_KEYWORD = "searchKeyword";
  private static final String SORT_COLUMN = "sortColumn";
  private static final String SORT_ORDER = "asc";
  private static final String STATUS_FILTER = "statusFilter";
  private static final String TIME_FILTER = "timeFilter";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String UPC_CODE = "upcCode";
  private static final String CATEGORY_ID = "categoryId";
  private static final String PRESENT_CATEGORY_ID = "sourceCategoryId";
  private static final String TARGET_CATEGORY_ID = "categoryId";
  private static final String KEYWORD = "keyword";
  private static final String PREFIX_PRODUCT_REVIEWING = "MTA_SCREENING_";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String BULK_ACTION = "bulkAction";
  private static final String DEFAULT_REASON = "reason";
  private static final PageRequest DEFAULT_PAGE_REQUEST = PageRequest.of(0, 10);
  private static final String ACTION_TYPE = "SUSPEND";
  private static final String TYPE_PARAMETER = "type";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  private static final String PATH = BASE_DIRECTORY + "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";
  private final String SOURCE = "source";
  private static final String CATEGORY_CHANGE_ERROR_MESSAGE = "error message";
  private static final String DEFAULT_CLIENT_ID= "clientId";
  private static final String TRUE = "true";
  private static final String CERTIFICATION_NUMBER = "certificationNumber";
  private static final String ACTIVITY = "activity";
  private static final String PREVIOUS_VALUE = "previousValue";
  private static final String CURRENT_VALUE = "currentValue";
  private static final String CREATED_BY = "createdBy";
  private static final String REQUEST_ID = "requestId";
  private static final String CURATION_STATUS = "curationStatus";
  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private SummaryFilterWebRequest summaryFilterWebRequest;
  private CountWebRequest countWebRequest;
  private HalalProductHistoryWebResponse halalProductHistoryWebResponse;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @InjectMocks
  private ProductController productController;

  @Captor
  private ArgumentCaptor<ProductReturnForCorrectionRequest> productReturnForCorrectionRequest;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequest;

  private ProductReturnForCorrectionWebRequest productReturnForCorrectionWebRequest;

  private ProductWebRequest productWebRequest;

  private SuspensionProductBulkActionsWebRequest suspensionProductBulkActionsWebRequest;

  private SelectedMasterProductDownloadWebRequest selectedMasterProductDownloadWebRequest;

  @BeforeEach
  public void init() {
    mockMvc = MockMvcBuilders.standaloneSetup(productController).build();
    productReturnForCorrectionWebRequest = new ProductReturnForCorrectionWebRequest();

    productWebRequest = new ProductWebRequest();
    productWebRequest.setName(PRODUCT_NAME);
    productWebRequest.setProductCode(PRODUCT_CODE);
    productWebRequest.setVersion(2l);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode("hash-code");
    imageRequest.setLocationPath("location-path");
    List<ImageRequest> imageRequests = new ArrayList<>();
    imageRequests.add(imageRequest);
    productWebRequest.setImages(imageRequests);
    TreeMap<String, String> attributesMap = new TreeMap<String, String>();
    attributesMap.put("image1", "hash-code1");

    List<ProductCategoryWebRequest> productCategories = new ArrayList<>();
    ProductCategoryWebRequest productCategoryWebRequest = new ProductCategoryWebRequest();
    productCategoryWebRequest.setCategoryCode(CATEGORY_CODE);
    CatalogWebRequest catalogWebRequest = new CatalogWebRequest();
    catalogWebRequest.setCatalogCode(CATALOG_CODE);
    productCategoryWebRequest.setCatalog(catalogWebRequest);
    productCategories.add(productCategoryWebRequest);
    productWebRequest.setProductCategories(productCategories);

    List<ProductAttributeWebRequest> productAttributes = new ArrayList<>();
    ProductAttributeWebRequest productAttributeWebRequest = new ProductAttributeWebRequest();
    AttributeWebRequest attributeWebRequest = new AttributeWebRequest();
    attributeWebRequest.setAttributeCode(ATTRIBUTE_CODE);
    attributeWebRequest.setAttributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE);
    List<AllowedAttributeValueWebRequest> allowedAttributeValues = new ArrayList<>();
    AllowedAttributeValueWebRequest allowedAttributeValueWebRequest = new AllowedAttributeValueWebRequest();
    allowedAttributeValueWebRequest.setAllowedAttributeCode(ALLOWED_ATTRIBUTE_CODE);
    allowedAttributeValueWebRequest.setValue(ALLOWED_ATTR_VALUE);
    allowedAttributeValues.add(allowedAttributeValueWebRequest);
    attributeWebRequest.setAllowedAttributeValues(allowedAttributeValues);
    List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValues = new ArrayList<>();
    PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest =
        new PredefinedAllowedAttributeValueWebRequest();
    predefinedAllowedAttributeValueWebRequest.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    predefinedAllowedAttributeValueWebRequest.setValue(PREDEFINED_ALLOWED_ATTR_VALUE);
    predefinedAllowedAttributeValues.add(predefinedAllowedAttributeValueWebRequest);
    attributeWebRequest.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValues);
    productAttributeWebRequest.setAttribute(attributeWebRequest);
    productAttributeWebRequest.setSequence(2);
    productAttributes.add(productAttributeWebRequest);
    productWebRequest.setProductAttributes(productAttributes);

    List<ProductItemWebRequest> productItems = new ArrayList<>();
    ProductItemWebRequest productItemRequest = new ProductItemWebRequest();
    productItemRequest.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemRequest.setImages(imageRequests);
    productItemRequest.setAttributesMap(attributesMap);
    ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest = new ProductItemAttributeValueWebRequest();
    productItemAttributeValueWebRequest.setAttribute(attributeWebRequest);
    productItemAttributeValueWebRequest.setValue(ATTR_VALUE);
    productItemAttributeValueWebRequest.setId("ID");
    List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests = new ArrayList<>();
    productItemAttributeValueWebRequests.add(productItemAttributeValueWebRequest);
    productItemRequest.setProductItemAttributeValues(productItemAttributeValueWebRequests);
    productItems.add(productItemRequest);
    productWebRequest.setProductItems(productItems);

    fileContent = new byte[]{-1, -40, -20, -10};

    suspensionProductBulkActionsWebRequest = new SuspensionProductBulkActionsWebRequest();
    suspensionProductBulkActionsWebRequest.setReason("reason");
    suspensionProductBulkActionsWebRequest.setNotes("notes");
    suspensionProductBulkActionsWebRequest.setAction("ACTIVE");
    suspensionProductBulkActionsWebRequest.setProducts(Arrays.asList(new ProductSuspensionWebRequest()));

    summaryFilterWebRequest =
        SummaryFilterWebRequest.builder().activated(true).viewable(true).categoryCode(CATEGORY_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).reviewPending(null).sortBy(StringUtils.EMPTY).build();
    countWebRequest = new CountWebRequest();

    selectedMasterProductDownloadWebRequest = new SelectedMasterProductDownloadWebRequest();

    halalProductHistoryWebResponse = new HalalProductHistoryWebResponse();
    halalProductHistoryWebResponse.setProductSku(PRODUCT_SKU);
    halalProductHistoryWebResponse.setActivity(ACTIVITY);
    halalProductHistoryWebResponse.setCurrentValue(CURRENT_VALUE);
    halalProductHistoryWebResponse.setPreviousValue(PREVIOUS_VALUE);
    halalProductHistoryWebResponse.setCreatedBy(CREATED_BY);
    halalProductHistoryWebResponse.setUpdatedBy(CREATED_BY);
    }

  @Test
  public void getProductRevisionReasonsTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.REVISION_REASONS)
            .param("type", String.valueOf(TYPE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void returnForCorrectionTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.RETURN_FOR_CORRECTION)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(productReturnForCorrectionWebRequest));

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).returnForCorrection(productReturnForCorrectionRequest.capture());
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void mergeProductTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.MERGE)
            .param("masterProductId", masterProductId)
            .param("duplicateProductId", duplicateProductId).param("isForceMerge", "false")
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).mergeProduct(masterProductId, duplicateProductId, false);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void updateProductTest() throws Exception {

    when(productServiceWrapper
        .updateProduct(Mockito.eq(Constants.REQUEST_ID), Mockito.eq(Constants.USER_TYPE_EXTERNAL),
            Mockito.any(ProductRequest.class), Mockito.eq(false), Mockito.eq(false)))
        .thenReturn(INTERNAL_ACTIVATION_PERIOD);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUserType()).thenReturn(Constants.USER_TYPE_EXTERNAL);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE)
            .param("internalFlow3AddProduct", "false").contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(productServiceWrapper)
        .updateProduct(Mockito.eq(Constants.REQUEST_ID), Mockito.eq(Constants.USER_TYPE_EXTERNAL),
            productRequest.capture(), Mockito.eq(false), Mockito.eq(false));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getBusinessPartnerCode();
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper, times(2)).getUserType();
    verify(clientParameterHelper).set(any(), any());
    ProductRequest productRequestModel = productRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productRequestModel.getName());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productRequestModel.getProductCategories().get(0).getCategory().getCatalog()
            .getCatalogCode());
    Assertions.assertEquals(2l,
        productRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues()
            .get(0).getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute()
            .getPredefinedAllowedAttributeValues().get(0).getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(2L, productRequestModel.getVersion().longValue());
  }

  @Test public void updateProductExceptionTest() throws Exception {

    when(productServiceWrapper
        .updateProduct(Mockito.eq(Constants.REQUEST_ID), Mockito.eq(Constants.USER_TYPE_EXTERNAL),
            Mockito.any(ProductRequest.class), Mockito.eq(false), Mockito.eq(false)))
        .thenThrow(new ClientException(ErrorMessages.ACTION_TYPE_INVALID));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(clientParameterHelper.getUserType()).thenReturn(Constants.USER_TYPE_EXTERNAL);

    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE)
            .param("internalFlow3AddProduct", "false").contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE).content(toJson(productWebRequest));

    SingleBaseResponse response = new SingleBaseResponse<>(ErrorMessages.ACTION_TYPE_INVALID, null, false, Constants.REQUEST_ID, null);

    String errorMessage = null;
    try {
      MvcResult result =
          mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false))).andReturn();
    } catch (ServletException e) {
      errorMessage = e.getMessage();
      Assertions.assertTrue(errorMessage.contains(ErrorMessages.ACTION_TYPE_INVALID));
    } finally {

      verify(productServiceWrapper).updateProduct(Mockito.eq(Constants.REQUEST_ID), Mockito.eq(Constants.USER_TYPE_EXTERNAL),
          productRequest.capture(), Mockito.eq(false), Mockito.eq(false));
      verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getBusinessPartnerCode();
    verify(clientParameterHelper, times(2)).getUsername();
    verify(clientParameterHelper, times(2)).getUserType();
    verify(clientParameterHelper).set(any(), any());
      ProductRequest productRequestModel = productRequest.getValue();
      Assertions.assertEquals(PRODUCT_NAME, productRequestModel.getName());
      Assertions.assertEquals(CATEGORY_CODE,
          productRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

      Assertions.assertEquals(CATALOG_CODE, productRequestModel.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
      Assertions.assertEquals(2l, productRequestModel.getProductAttributes().get(0).getSequence().longValue());
      Assertions.assertEquals(ATTRIBUTE_CODE, productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
      Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
          productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
      Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
          productRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getAllowedAttributeCode());
      Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
          productRequestModel.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getPredefinedAllowedAttributeCode());
      Assertions.assertEquals(2L, productRequestModel.getVersion().longValue());
    }
  }

  @Test
  public void updateProductAssignmentStatusTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_ASSIGNMENT).param("assignedTo", ASSIGNED_TO)
            .param("assignedBy", ASSIGNED_BY).param("productCode", PRODUCT_CODE)
            .param("toAssignNewReviewer", String.valueOf(Boolean.TRUE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();    verify(productService).updateProductAssignmentStatus(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
  }

  @Test
  public void getProductDetailTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getClientId()).thenReturn(Constants.CLIENT_ID);
    when(productService.getProductDetail(PRODUCT_CODE, Boolean.FALSE, DEFAULT_CLIENT_ID, false)).thenReturn(
        new ProductDetailWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.DETAIL, PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getClientId();
    verify(productService).getProductDetail(PRODUCT_CODE, Boolean.FALSE, DEFAULT_CLIENT_ID, false);
  }

  @Test
  public void getFilterCountsTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getFilterCounts(Boolean.FALSE, Boolean.FALSE)).thenReturn(new FilterCountWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_COUNTS)
            .param("activated", String.valueOf(Boolean.FALSE))
            .param("viewable", String.valueOf(Boolean.FALSE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getFilterCounts(Boolean.FALSE, Boolean.FALSE);
  }

  @Test
  public void getReviewProductsTest() throws Exception {
    ReviewProductsFilterRequest request = ReviewProductsFilterRequest.builder()
        .assignedTo(ASSIGNED_TO).categoryCode(CATEGORY_CODE).searchKeyword(SEARCH_KEYWORD)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).sortColumn(SORT_COLUMN).sortOrder(SORT_ORDER)
        .statusFilter(STATUS_FILTER).timeFilter(TIME_FILTER).build();
    List<ReviewProductWebResponse> reviewProductWebResponses = new ArrayList<>();
    ReviewProductWebResponse reviewProductWebResponse = ReviewProductWebResponse.builder()
        .productName(PRODUCT_NAME).assignedTo(ASSIGNED_TO).brand(BRAND).businessPartnerCode(BUSINESS_PARTNER_CODE)
        .build();
    reviewProductWebResponses.add(reviewProductWebResponse);
    Page<ReviewProductWebResponse> response = new PageImpl<>(reviewProductWebResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getReviewProductsByFilterRequest(
        request, Boolean.FALSE, Boolean.FALSE, 0, 100)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.REVIEW_PRODUCTS_FILTER)
            .param("activated", String.valueOf(Boolean.FALSE))
            .param("viewable", String.valueOf(Boolean.FALSE))
            .content(toJson(request))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content.[0].productName", equalTo(PRODUCT_NAME)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getReviewProductsByFilterRequest(request, Boolean.FALSE, Boolean.FALSE, 0, 100);
  }

  @Test
  public void approveDraftTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUserType()).thenReturn(Constants.USER_TYPE);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);

    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.APPROVE_DRAFT)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .accept(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productWebRequest));

    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));

    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUserType();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
    verify(productService).approveDraft(productRequest.capture());
    ProductRequest productRequestModel = productRequest.getValue();
    Assertions.assertEquals(PRODUCT_NAME, productRequestModel.getName());
    Assertions.assertEquals(CATEGORY_CODE,
        productRequestModel.getProductCategories().get(0).getCategory().getCategoryCode());

    Assertions.assertEquals(CATALOG_CODE,
        productRequestModel.getProductCategories().get(0).getCategory().getCatalog()
            .getCatalogCode());
    Assertions.assertEquals(2l,
        productRequestModel.getProductAttributes().get(0).getSequence().longValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeCode());
    Assertions.assertEquals(AttributeType.DEFINING_ATTRIBUTE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAttributeType());
    Assertions.assertEquals(ALLOWED_ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues()
            .get(0).getAllowedAttributeCode());
    Assertions.assertEquals(PREDEFINED_ALLOWED_ATTRIBUTE_CODE,
        productRequestModel.getProductAttributes().get(0).getAttribute()
            .getPredefinedAllowedAttributeValues().get(0).getPredefinedAllowedAttributeCode());
  }

  @Test
  public void getProductHistoryTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getProductHistory(PRODUCT_ID, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Arrays.asList(new ProductHistoryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCT_HISTORY, PRODUCT_ID)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getProductHistory(PRODUCT_ID, PAGE, SIZE);
  }

  @Test
  public void bulkScreeningProductActionsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest =
        ScreeningProductBulkActionsWebRequest.builder().build();
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.BULK_SCREENING_PRODUCT_ACTIONS, BULK_ACTION)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(toJson(screeningProductBulkActionsWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).doScreeningProductsBulkActions(BULK_ACTION, screeningProductBulkActionsWebRequest);
  }

  @Test
  public void getScreeningSuggestionTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID,
        Collections.singletonList(new ProductSuggestionWebRequest()), PageRequest.of(PAGE, SIZE)))
        .thenReturn(Collections.singletonList(new ProductSuggestionWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.SCREENING_SUGGESTION)
            .param("productName", PRODUCT_NAME)
            .param("upcCode", UPC_CODE)
            .param("productCode", PRODUCT_CODE)
            .param("categoryId", CATEGORY_ID)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(Collections.singletonList(new ProductSuggestionWebRequest())));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getScreeningSuggestion(PRODUCT_CODE, PRODUCT_NAME, UPC_CODE, CATEGORY_ID,
        Collections.singletonList(new ProductSuggestionWebRequest()), PageRequest.of(PAGE, SIZE));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void screeningSearchTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE))
        .thenReturn(Arrays.asList(new ProductSuggestionWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.SCREENING_PRODUCT_SEARCH)
            .param("keyword", KEYWORD)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
  }

  @Test
  public void getProductReviewerTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(productService.getProductReviewerList(PREFIX_PRODUCT_REVIEWING.concat(PRODUCT_CODE), USER_NAME))
        .thenReturn(new ProductReviewerWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_REVIEWER_FOR_PRODUCT)
            .param("productCode", PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(productService).getProductReviewerList(PREFIX_PRODUCT_REVIEWING.concat(PRODUCT_CODE), USER_NAME);
  }

  @Test
  public void deleteProductReviewerTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        delete(ProductApiPath.BASE_PATH + ProductApiPath.DELETE_REVIEWER_FOR_PRODUCT)
            .param("productCode", PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper, times(2)).getUsername();
    verify(productService).deleteProductReviewer(PREFIX_PRODUCT_REVIEWING.concat(PRODUCT_CODE), USER_NAME);
  }

  @Test
  public void getProductRevisionHistoryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getProductRevisionHistory(PRODUCT_CODE)).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_REVISION_HISTORY, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getProductRevisionHistory(PRODUCT_CODE);
  }

  @Test
  public void checkCategoryChangeTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.checkCategoryChange(PRESENT_CATEGORY_ID, TARGET_CATEGORY_ID, PRODUCT_CODE, false)).thenReturn
        (new CategoryChangeCheckResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.CATEGORY_CHANGE_CHECK, PRODUCT_CODE)
            .param("presentCategoryId", PRESENT_CATEGORY_ID)
            .param("targetCategoryId", TARGET_CATEGORY_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).checkCategoryChange(PRESENT_CATEGORY_ID, TARGET_CATEGORY_ID, PRODUCT_CODE, false);
  }

  @Test
  public void getAllProductsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getAllProducts((new ProductSuspensionFilterRequest()), PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductSuspensionWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ALL_PRODUCTS)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(new ProductSuspensionFilterRequest()));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getAllProducts((new ProductSuspensionFilterRequest()), PageRequest.of(PAGE, SIZE));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void doSuspensionActionTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(productService).doSuspensionAction(suspensionProductBulkActionsWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.PRODUCTS_SUSPENSION).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).content(toJson(suspensionProductBulkActionsWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).doSuspensionAction(suspensionProductBulkActionsWebRequest);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getSuspensionHistoryTest() throws Exception {
    ProductSuspensionHistoryWebResponse productSuspensionHistory = new ProductSuspensionHistoryWebResponse();
    productSuspensionHistory.setProductSku(PRODUCT_SKU);
    productSuspensionHistory.setReason(DEFAULT_REASON);
    List<ProductSuspensionHistoryWebResponse> historyList = Arrays.asList(productSuspensionHistory);
    Page<ProductSuspensionHistoryWebResponse> productSuspensionHistories = new PageImpl<>(historyList, DEFAULT_PAGE_REQUEST, 10);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE)).thenReturn(productSuspensionHistories);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.SUSPENSION_HISTORY)
            .param("productSku", PRODUCT_SKU)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getSuspensionHistory(PRODUCT_SKU, PAGE, SIZE);
  }

  @Test
  public void bulkProductSuspensionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.BULK_PRODUCT_SUSPENSION)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).param(TYPE_PARAMETER, ACTION_TYPE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(productService)
        .saveProductSuspensionFile(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkProductSuspensionIOExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(productService)
        .saveProductSuspensionFile(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);

    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.BULK_PRODUCT_SUSPENSION)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON).param(TYPE_PARAMETER, ACTION_TYPE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(productService)
        .saveProductSuspensionFile(multipartFile, ACTION_TYPE, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  @Test
  public void updateProductCategoryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(productService.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_CATEGORY, PRODUCT_CODE)
            .param("categoryCode", CATEGORY_CODE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).updateProductCategory(PRODUCT_CODE, CATEGORY_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getActiveProductsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new ProductCollectionWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.GET_ACTIVE_PRODUCTS).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).content(toJson(summaryFilterWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).findProductCollectionSummaryByKeyword(summaryFilterWebRequest, PageRequest.of(PAGE, SIZE));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getFilterCountsBySourceTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(productService.getFilterCountsBySource(countWebRequest)).thenReturn(new MapWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.FILTER_COUNTS_BY_SOURCE).contentType(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(toJson(countWebRequest))
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getFilterCountsBySource(countWebRequest);
    verify(clientParameterHelper).getRequestId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void bulkUpdateMasterProductDataTest() throws Exception {
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(ProductApiPath.BASE_PATH + ProductApiPath.BULK_UPDATE_MASTER_PRODUCT)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(productService)
        .bulkUpdateMasterProductData(multipartFile, Constants.REQUEST_ID, Constants.STORE_ID, USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("dummy-excel", "dummy-excel.xls", null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  @Test
  public void downloadBulkSelectedMasterProductsForInternalTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(productService)
        .downloadBulkSelectedMasterProductsForInternal(USER_NAME, selectedMasterProductDownloadWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_SELECTED_MASTER_PRODUCTS)
            .content(toJson(selectedMasterProductDownloadWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService)
        .downloadBulkSelectedMasterProductsForInternal(USER_NAME, selectedMasterProductDownloadWebRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();

  }


  @Test
  public void downloadAllMasterProductsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(productService)
        .downloadBulKMasterProducts(USER_NAME, summaryFilterWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.DOWNLOAD_MASTER_PRODUCTS)
            .content(toJson(summaryFilterWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService)
        .downloadBulKMasterProducts(USER_NAME, summaryFilterWebRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void getProductHistorySummaryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.findProductHistory(SOURCE, PRODUCT_CODE, PRODUCT_ID, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Collections.singletonList(new HistoryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_PRODUCT_HISTORY).param("source", SOURCE)
            .param("productCode", PRODUCT_CODE).param("productId", PRODUCT_ID).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).findProductHistory(SOURCE, PRODUCT_CODE, PRODUCT_ID, PAGE, SIZE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void retryProductPublishToPDTTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(productService.retryProductPublishToPDT(PRODUCT_CODE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.RETRY_PRODUCT_PUBLISH_TO_PDT).param("productCode", PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService).retryProductPublishToPDT(PRODUCT_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void reindexByProductSku() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doNothing().when(productService).reindexByProductSku(Constants.STORE_ID, PRODUCT_SKU);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.REINDEX_BY_PRODUCT_SKU, PRODUCT_SKU)
            .param("storeId", Constants.STORE_ID).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).reindexByProductSku(Constants.STORE_ID, PRODUCT_SKU);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void reindexByProductCode() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doNothing().when(productService).reindexByProductCode(PRODUCT_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductApiPath.BASE_PATH + ProductApiPath.REINDEX_BY_PRODUCT_CODE, PRODUCT_CODE)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).reindexByProductCode(PRODUCT_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void retryProductNeedRevisionToPBPTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(productService.retryProductNeedRevisionToPBP(Constants.STORE_ID, PRODUCT_CODE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.RETRY_PRODUCT_NEED_REVISION_TO_PBP)
            .param("productCode", PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService).retryProductNeedRevisionToPBP(Constants.STORE_ID, PRODUCT_CODE);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();

  }

  @Test
  public void getHalalProductDetailsByProductSkuTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getHalalProductDetailsByProductSku(PRODUCT_SKU)).thenReturn(new HalalProductWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_HAHAL_PRODUCT_DETAILS_BY_PRODUCT_SKU,
            PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getHalalProductDetailsByProductSku(PRODUCT_SKU);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void checkProductCategoryChangeTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getProductCategoryChangeCheckResponse(PRODUCT_CODE, CATEGORY_ID,
        CATEGORY_ID, true, PRODUCT_TYPE)).thenReturn(new CategoryChangeWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.CHECK_CATEGORY_CHANGE, PRODUCT_CODE).param(
                "oldCategoryId", CATEGORY_ID)
            .param("newCategoryId", CATEGORY_ID)
            .param("isActive", TRUE)
            .param("productType", String.valueOf(3))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getProductCategoryChangeCheckResponse(PRODUCT_CODE, CATEGORY_ID, CATEGORY_ID, true, PRODUCT_TYPE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getInternalDownloadTemplateFilePathsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getInternalDownloadTemplateFilePaths()).thenReturn(new TemplateDownloadFilePathWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_ALL_INTERNAL_TEMPLATE_DOWNLOAD_PATHS).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getInternalDownloadTemplateFilePaths();
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getHalalCertificationDetailsTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getHalalCertificationDetails(CERTIFICATION_NUMBER, PAGE, SIZE)).thenReturn(
        new PageImpl<>(Collections.singletonList(new HalalCertificationWebDetailsResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_HALAL_CERTIFICATION_DETAILS, CERTIFICATION_NUMBER).param(
                "page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).getHalalCertificationDetails(CERTIFICATION_NUMBER, PAGE, SIZE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getHalalProductHistoryTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getHalaProductHistory(PRODUCT_ID, PAGE, SIZE)).thenReturn(
        new PageImpl<>(List.of(new HalalProductHistoryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductApiPath.BASE_PATH + ProductApiPath.GET_HALAL_PRODUCT_HISTORY, PRODUCT_ID).param("page",
                String.valueOf(PAGE)).param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getHalaProductHistory(PRODUCT_ID, PAGE, SIZE);
  }

  @Test
  public void getHalalDashboardProductsTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    HalalProductsFilterWebRequest halalProductsFilterWebRequest = new HalalProductsFilterWebRequest();
    halalProductsFilterWebRequest.setKeyword(StringUtils.EMPTY);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productService.getHalalDashboardProductsResponses(PAGE, SIZE, halalProductsFilterWebRequest)).thenReturn(
        new PageImpl<>(List.of(new HalalDashboardProductsWebResponse())));
    this.mockMvc.perform(post(ProductApiPath.BASE_PATH + ProductApiPath.GET_HALAL_DASHBOARD_PRODUCTS).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(halalProductsFilterWebRequest))
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success").value(true));
    verify(clientParameterHelper).getRequestId();
    verify(productService).getHalalDashboardProductsResponses(PAGE, SIZE, halalProductsFilterWebRequest);
  }

  @Test
  public void updateHalalConfigOfProduct() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(DEFAULT_STORE_ID);
    doNothing().when(productService)
        .updateHalalConfigOfProduct(DEFAULT_STORE_ID, REQUEST_ID, USER_NAME, PRODUCT_SKU, CURATION_STATUS);
    MockHttpServletRequestBuilder requestBuilder =
        put(ProductApiPath.BASE_PATH + ProductApiPath.UPDATE_PRODUCT_HALAL_CONFIG, PRODUCT_SKU).param("curationStatus",
            CURATION_STATUS).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(productService).updateHalalConfigOfProduct(DEFAULT_STORE_ID, REQUEST_ID, USER_NAME, PRODUCT_SKU,
        CURATION_STATUS);
    verify(clientParameterHelper, times(3)).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(productServiceWrapper);
    verifyNoMoreInteractions(clientParameterHelper);
    FileUtils.deleteDirectory(new File(PATH));
  }
}
