package com.gdn.x.mta.distributiontask.dao.impl;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ImageQcProcessedResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gda.mta.product.dto.response.SimpleBooleanResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.PCBFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.ProductAnalyticsFeign;
import com.gdn.x.mta.distributiontask.dao.api.feign.XProductFeign;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
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
import org.slf4j.MDC;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ProductServiceRepositoryBeanTest {

  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "REQ_ID";
  private static final String USERNAME = "USERNAME";
  private static final String CLIENT_ID = "CLIENT_ID";
  private static final String CHANNEL_ID = "CHANNEL_ID";
  private static final String PRODUCT_CODE = "PRODUCTCODE";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0001";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String REJECT_REASON = "rejectReasonRequest";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String CATEGORY_CODE = "category-code";
  private static final String CATEGORY_NAME = "category-name";
  private static final String ERROR_MSG = "error_msg";
  private static final String ERROR_CODE = "error_code";
  private static final String REVIEW_TYPE = "reviewType";
  private static final String PRODUCT_CODE_1 = "PRODUCT_CODE";
  private static final String PRODUCT_ID = "ID";
  private static final String BRAND_CODE = "brandCode";

  private CategoryCodeRequest categoryCodeRequest1 =
    new CategoryCodeRequest(Arrays.asList(CATEGORY_CODE));

  @InjectMocks
  private ProductServiceRepositoryBean instance;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>>
    configurationStatusRequestListArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeleteProductRequest> deleteProductRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductSkuAndProductCodeRequest> productCodeRequestArgumentCaptor;

  private GdnRestListResponse<ProductCollectionResponse> gdnRestListResponse;
  private GdnRestListResponse<ProductDetailResponse> gdnProductDetailResponse;
  private List<String> categoryCodeList = new ArrayList<>();
  private CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
  private CategoryCodeResponse categoryCodeResponse = new CategoryCodeResponse();
  private GdnRestSingleResponse<CategoryCodeResponse> categoryCodeResponseGdnRestListResponse;
  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;
  private GdnBaseRestResponse response = new GdnBaseRestResponse();
  private GdnRestListResponse<ConfigurationStatusResponse> configurationStatusListResponse;
  private ConfigurationStatusResponse configurationStatusResponse;
  private ImageQcProcessedResponse imageQcProcessedResponse;
  private ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse;
  private AutoApprovalTypeRequest autoApprovalTypeRequest;
  private AutoApprovalTypeResponse autoApprovalTypeResponse;
  private ProductHistoryRequest productHistoryRequest;
  private RetryNeedRevisionRequest retryNeedRevisionRequest;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);

    this.gdnRestListResponse = new GdnRestListResponse<>(new ArrayList<ProductCollectionResponse>(),
      new PageMetaData(0, 0, 10), REQUEST_ID);
    this.gdnProductDetailResponse =
      new GdnRestListResponse<>(new ArrayList<ProductDetailResponse>(), new PageMetaData(0, 0, 10),
        REQUEST_ID);
    categoryCodeList.add(DEFAULT_CATEGORY_CODE);
    categoryCodeRequest.setCategoryCodes(categoryCodeList);
    categoryCodeResponse.setCategoryCodes(categoryCodeList);
    categoryCodeResponseGdnRestListResponse =
      new GdnRestSingleResponse<>(null, null, true, categoryCodeResponse, REQUEST_ID);
    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setProductCodes(
      new ArrayList<>(Arrays.asList(PRODUCT_CODE)));
    screeningProductBulkActionsRequest.setAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setCorrectionReason(CORRECTION_REASON);
    screeningProductBulkActionsRequest.setRejectionReason(REJECT_REASON);

    configurationStatusResponse = new ConfigurationStatusResponse();
    configurationStatusResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);

    configurationStatusListResponse = new GdnRestListResponse<>();
    configurationStatusListResponse.setSuccess(true);
    configurationStatusListResponse.setContent(
      Collections.singletonList(configurationStatusResponse));

    imageQcProcessedResponse = new ImageQcProcessedResponse();
    imageQcProcessedResponse.setProductCode(PRODUCT_CODE);

    imageQcProcessedAndBrandResponse = new ImageQcProcessedAndBrandResponse();
    imageQcProcessedAndBrandResponse.setImageQcProcessedResponse(imageQcProcessedResponse);
    autoApprovalTypeRequest = new AutoApprovalTypeRequest();
    autoApprovalTypeRequest.setCategoryCode(CATEGORY_CODE);
    autoApprovalTypeRequest.setReviewType(REVIEW_TYPE);
    autoApprovalTypeRequest.setEdited(Boolean.TRUE);
    autoApprovalTypeResponse = new AutoApprovalTypeResponse(Constants.NA);

    productHistoryRequest = new ProductHistoryRequest();
    productHistoryRequest.setProductCode(PRODUCT_CODE);
    retryNeedRevisionRequest = new RetryNeedRevisionRequest();

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);

  }

  @Test
   void submitHistoryTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    TaskHistory request = new TaskHistory();
    request.setState(WorkflowState.PASSED);
    Mockito.when(
        this.pbpFeign.submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class)))
      .thenReturn(response);
    this.instance.submitHistory(REQUEST_ID, USERNAME, request);
    Mockito.verify(this.pbpFeign)
      .submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class));
  }

  @Test
   void submitHistoryFailTest() throws Exception {
    try {
      GdnBaseRestResponse response = new GdnBaseRestResponse();
      response.setSuccess(false);
      TaskHistory request = new TaskHistory();
      request.setState(WorkflowState.PASSED);
      Mockito.when(
          this.pbpFeign.submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class)))
        .thenReturn(response);
      this.instance.submitHistory(REQUEST_ID, USERNAME, request);
    } finally {
      Mockito.verify(this.pbpFeign)
        .submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class));
    }
  }

  @Test
   void submitHistoryExceptionTest() {
    Assertions.assertThrows(Exception.class,
      () -> this.instance.submitHistory(REQUEST_ID, USERNAME, null));
  }

  @Test
   void addToProductHistoryTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(
        this.pbpFeign.submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class)))
      .thenReturn(response);
    this.instance.addToProductHistory(REQUEST_ID, USERNAME, productHistoryRequest);
    Mockito.verify(this.pbpFeign)
      .submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class));
  }

  @Test
   void addToProductHistorySuccessFalseTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    Mockito.when(
        this.pbpFeign.submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class)))
      .thenReturn(response);
    this.instance.addToProductHistory(REQUEST_ID, USERNAME, productHistoryRequest);
    Mockito.verify(this.pbpFeign)
      .submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductHistoryRequest.class));
  }

  @Test
   void addToProductHistoryErrorTest() {
    Assertions.assertThrows(RuntimeException.class,
      () -> this.instance.addToProductHistory(REQUEST_ID, USERNAME, null));
    Mockito.verify(this.pbpFeign)
      .submitHistory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(null));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
  }

  @Test
   void testDeleteProductCollection() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();

    Mockito.when(this.pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
      (DeleteProductRequest) Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    this.instance.deleteProductCollection(REQUEST_ID, USERNAME, true, rejectProductDTO);
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
        (DeleteProductRequest) Mockito.any());
  }

  @Test
   void testDeleteProductCollectionReasons() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode("test product code");
    rejectProductDTO.setNotes("test notes");
    RejectReasonDto rejectReasonDto = new RejectReasonDto();
    List<String> products = new ArrayList<>();
    products.add("product1");
    products.add("product2");
    rejectReasonDto.setProduct(products);
    List<String> content = new ArrayList<>();
    content.add("content1");
    content.add("content2");
    rejectReasonDto.setContent(content);
    List<String> Images = new ArrayList<>();
    Images.add("Image1");
    Images.add("Image2");
    rejectReasonDto.setImage(Images);
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);
    rejectProductDTO.setBulkAction(true);
    Mockito.when(this.pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
      (DeleteProductRequest) Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    this.instance.deleteProductCollection(REQUEST_ID, USERNAME, true, rejectProductDTO);
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
        (DeleteProductRequest) Mockito.any());
  }

  @Test
   void testDeleteProductCollectionReasonsNull() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode("test product code");
    rejectProductDTO.setNotes("test notes");
    RejectReasonDto rejectReasonDto = new RejectReasonDto();
    rejectReasonDto.setProduct(null);
    rejectReasonDto.setContent(null);
    rejectReasonDto.setImage(null);
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);
    Mockito.when(this.pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
      (DeleteProductRequest) Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    this.instance.deleteProductCollection(REQUEST_ID, USERNAME, true, rejectProductDTO);
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
        (DeleteProductRequest) Mockito.any());
  }

  @Test
   void testDeleteProductCollectionEmptyReasonsNull() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode("test product code");
    rejectProductDTO.setNotes(REJECT_REASON);
    RejectReasonDto rejectReasonDto = new RejectReasonDto();
    rejectReasonDto.setProduct(Collections.singletonList(StringUtils.EMPTY));
    rejectProductDTO.setRejectReasonDto(rejectReasonDto);
    Mockito.when(this.pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
      (DeleteProductRequest) Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    this.instance.deleteProductCollection(REQUEST_ID, USERNAME, true, rejectProductDTO);
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
        deleteProductRequestArgumentCaptor.capture());
    Assertions.assertEquals(REJECT_REASON,
        deleteProductRequestArgumentCaptor.getValue().getNotes());
  }

  @Test
   void testDeleteProductCollectionException() throws Exception {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    Mockito.when(this.pbpFeign.deleteProductCollection(Mockito.anyString(), Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
      Mockito.any())).thenReturn(new GdnBaseRestResponse(false));
    Assertions.assertThrows(Exception.class,
      () -> this.instance.deleteProductCollection(REQUEST_ID, USERNAME, true, rejectProductDTO));
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .deleteProductCollection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(true),
        (DeleteProductRequest) Mockito.any());
  }

  @Test
   void testRepublishToPDT() throws Exception {
    Mockito.when(this.pbpFeign.publishProductToPDTByProductCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME,
      PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(true));
    this.instance.republishToPDT(REQUEST_ID, USERNAME, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .publishProductToPDTByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
  }

  @Test
   void testRepublishToPDTException() throws Exception {
    Mockito.when(this.pbpFeign.publishProductToPDTByProductCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME,
      PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(false));
    Assertions.assertThrows(Exception.class,
      () -> this.instance.republishToPDT(REQUEST_ID, USERNAME, PRODUCT_CODE));
    Mockito.verify(this.pbpFeign, Mockito.times(1))
      .publishProductToPDTByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
  }

  @Test
   void findProductDetailsByProductCodesTest() throws Exception {
    this.gdnProductDetailResponse.setSuccess(true);
    Mockito.when(
        this.pbpFeign.getProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
      .thenReturn(this.gdnProductDetailResponse);
    this.instance.findProductDetailsByProductCodes(REQUEST_ID, USERNAME,
      Arrays.asList(PRODUCT_CODE));
    Mockito.verify(pbpFeign)
      .getProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void findProductDetailsByProductCodesTest_Exception() throws Exception {
    this.gdnProductDetailResponse.setSuccess(false);
    Mockito.when(
        this.pbpFeign.getProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
      .thenReturn(this.gdnProductDetailResponse);
    Assertions.assertThrows(Exception.class,
      () -> this.instance.findProductDetailsByProductCodes(REQUEST_ID, USERNAME,
        Arrays.asList(PRODUCT_CODE)));
    Mockito.verify(pbpFeign)
      .getProductDetailsByProductCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyList());
  }

  @Test
   void getReviewConfigurationChangesTest() {
    Mockito.when(pcbFeign.getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, 10000))
      .thenReturn(configurationStatusListResponse);
    List<ConfigurationStatusResponse> response = instance.getReviewConfigurationChanges(10000);
    Mockito.verify(pcbFeign)
      .getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        REQUEST_ID, USERNAME, 10000);
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, response.get(0).getCategoryCode());
  }

  @Test
   void getReviewConfigurationChangesExceptionTest() {
    configurationStatusListResponse.setSuccess(false);
    Mockito.when(pcbFeign.getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, 10000))
      .thenReturn(configurationStatusListResponse);
    Assertions.assertThrows(Exception.class, () -> instance.getReviewConfigurationChanges(10000));
    Mockito.verify(pcbFeign)
      .getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        REQUEST_ID, USERNAME, 10000);
  }

  @Test
   void getReviewConfigurationChangesExceptionContentNullTest() {
    configurationStatusListResponse.setContent(null);
    Mockito.when(pcbFeign.getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, 10000))
      .thenReturn(configurationStatusListResponse);
    Assertions.assertThrows(Exception.class, () -> instance.getReviewConfigurationChanges(10000));
    Mockito.verify(pcbFeign)
      .getConfigurationChanges(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        REQUEST_ID, USERNAME, 10000);
  }

  @Test
   void updateProductCollectionAsPostLiveTest() {
    Mockito.when(pbpFeign.updateProductAsPostLive(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE))
      .thenReturn(new GdnBaseRestResponse(true));
    instance.updateProductCollectionAsPostLive(STORE_ID, PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .updateProductAsPostLive(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        REQUEST_ID, USERNAME, PRODUCT_CODE);
  }

  @Test
   void updateProductCollectionAsPostLiveExceptionTest() {
    Mockito.when(pbpFeign.updateProductAsPostLive(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE))
      .thenReturn(new GdnBaseRestResponse(false));
    Assertions.assertThrows(Exception.class,
      () -> instance.updateProductCollectionAsPostLive(STORE_ID, PRODUCT_CODE));
    Mockito.verify(pbpFeign)
      .updateProductAsPostLive(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        REQUEST_ID, USERNAME, PRODUCT_CODE);
  }

  @Test
   void getReviewConfigurationTest() {
    List<ConfigurationStatusResponse> configurationStatusResponses = new ArrayList<>();
    GdnRestListResponse<ConfigurationStatusResponse> response =
      new GdnRestListResponse<>(configurationStatusResponses, new PageMetaData(10, 0, 10),
        REQUEST_ID);
    Mockito.when(
      pcbFeign.getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(response);
    instance.getReviewConfiguration(Collections.singletonList(
      ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE)
        .businessPartnerCode(MERCHANT_CODE).build()));
    Mockito.verify(pcbFeign)
      .getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
        configurationStatusRequestListArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE,
        configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
    Assertions.assertEquals(MERCHANT_CODE,
        configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
  }

  @Test
   void getReviewConfigurationExceptionTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
      new GdnRestListResponse<>(ERROR_MSG, ERROR_CODE, false, REQUEST_ID);
    Mockito.when(
      pcbFeign.getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(response);
    Assertions.assertThrows(Exception.class, () -> instance.getReviewConfiguration(
      Collections.singletonList(ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE)
        .businessPartnerCode(MERCHANT_CODE).build())));
    Mockito.verify(pcbFeign)
      .getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
        configurationStatusRequestListArgumentCaptor.capture());
    Assertions.assertEquals(CATEGORY_CODE,
        configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
    Assertions.assertEquals(MERCHANT_CODE,
        configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
  }

  @Test
   void getReviewConfigurationEmptyResultTest() {
    GdnRestListResponse<ConfigurationStatusResponse> response =
      new GdnRestListResponse<>(null, new PageMetaData(10, 0, 10), REQUEST_ID);
    Mockito.when(
      pcbFeign.getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
        Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> instance.getReviewConfiguration(
        Collections.singletonList(ConfigurationStatusRequest.builder().categoryCode(CATEGORY_CODE)
          .businessPartnerCode(MERCHANT_CODE).build())));
    } finally {
      Mockito.verify(pcbFeign)
        .getReviewConfiguration(Mockito.anyString(), Mockito.eq(Constants.DEFAULT_CHANNEL_ID),
          Mockito.eq(Constants.DEFAULT_CLIENT_ID), Mockito.anyString(), Mockito.anyString(),
          configurationStatusRequestListArgumentCaptor.capture());
      Assertions.assertEquals(CATEGORY_CODE,
          configurationStatusRequestListArgumentCaptor.getValue().get(0).getCategoryCode());
      Assertions.assertEquals(MERCHANT_CODE,
          configurationStatusRequestListArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
    }
  }

  @Test
   void getImageQcPredictionResponseTest() {
    Mockito.when(
        pbpFeign.getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE))
      .thenReturn(new GdnRestSingleResponse<>(imageQcProcessedAndBrandResponse, REQUEST_ID));
    ImageQcProcessedAndBrandResponse response = instance.getImageQcPredictionResponse(PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE, response.getImageQcProcessedResponse().getProductCode());
  }

  @Test
   void getImageQcPredictionResponseContentExceptionTest() {
    Mockito.when(
        pbpFeign.getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    ImageQcProcessedAndBrandResponse response = instance.getImageQcPredictionResponse(PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertNull(response);
  }

  @Test
   void getImageQcPredictionResponseExceptionTest() {
    Mockito.when(
      pbpFeign.getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, imageQcProcessedAndBrandResponse, REQUEST_ID));
    ImageQcProcessedAndBrandResponse response = instance.getImageQcPredictionResponse(PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .getImageQcPredictionAndBrandResponse(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, REQUEST_ID, USERNAME, PRODUCT_CODE);
    Assertions.assertNull(response);
  }

  @Test
   void getCategoryHierarchyByCategoryCodeTest() {
    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest1)).thenReturn(
      new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(),
        REQUEST_ID));
    List<CategoryHierarchyResponse> responses =
      instance.getCategoryHierarchyByCategoryCodes(categoryCodeRequest1);
    Mockito.verify(this.pcbFeign).filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest1);
    Assertions.assertNotNull(responses);
  }

  @Test
   void getCategoryHierarchyByCategoryCode_successFalseTest() {
    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest1)).thenReturn(
      new GdnRestListResponse<>(null, null, false, new ArrayList<>(), null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCategoryHierarchyByCategoryCodes(categoryCodeRequest1));
    } finally {
      Mockito.verify(this.pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, categoryCodeRequest1);
    }
  }

  @Test
   void getCategoryHierarchyByCategoryCode_responseNullTest() {
    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, categoryCodeRequest1))
      .thenReturn(new GdnRestListResponse<>(null, null, true, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCategoryHierarchyByCategoryCodes(categoryCodeRequest1));
    } finally {
      Mockito.verify(this.pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, categoryCodeRequest1);
    }
  }

  @Test
   void getAutoApprovalTypeTest() {
    Mockito.when(
      this.pbpFeign.getAutoApprovalType(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        false, PRODUCT_CODE, autoApprovalTypeRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, autoApprovalTypeResponse, REQUEST_ID));
    instance.getAutoApprovalType(STORE_ID, PRODUCT_CODE, false, autoApprovalTypeRequest);
    Mockito.verify(this.pbpFeign)
      .getAutoApprovalType(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        false, PRODUCT_CODE, autoApprovalTypeRequest);
  }

  @Test
   void retryAutoNeedRevisionTest() {
    Mockito.when(
      this.pbpFeign.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        retryNeedRevisionRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new RetryAutoNeedRevisionResponse(),
        REQUEST_ID));
    RetryAutoNeedRevisionResponse response =
      instance.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, retryNeedRevisionRequest);
    Mockito.verify(this.pbpFeign)
      .retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        retryNeedRevisionRequest);
    Assertions.assertNotNull(response);
  }

  @Test
   void retryAutoNeedRevisionSuccessFalseTest() {
    Mockito.when(
        this.pbpFeign.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          retryNeedRevisionRequest))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, retryNeedRevisionRequest));
    } finally {
      Mockito.verify(this.pbpFeign)
        .retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          retryNeedRevisionRequest);
    }
  }

  @Test
   void retryAutoNeedRevisionResponseNullTest() {
    Mockito.when(
        this.pbpFeign.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          retryNeedRevisionRequest))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, retryNeedRevisionRequest));
    } finally {
      Mockito.verify(this.pbpFeign)
        .retryAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          retryNeedRevisionRequest);
    }
  }

  @Test
   void getProductStatusTest() {
    Mockito.when(
      this.pbpFeign.getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse(PRODUCT_CODE),
        REQUEST_ID));
    String response = instance.getProductStatus(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign)
      .getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response, PRODUCT_CODE);
  }

  @Test
   void getProductStatusTestNullTest() {
    Mockito.when(
      this.pbpFeign.getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(null);
    String response = instance.getProductStatus(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign)
      .getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response, StringUtils.EMPTY);
  }

  @Test
   void getProductStatusTestSuccessFalseTest() {
    Mockito.when(
      this.pbpFeign.getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    String response = instance.getProductStatus(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign)
      .getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response, StringUtils.EMPTY);
  }

  @Test
   void getProductStatusTestValueNullTest() {
    Mockito.when(
      this.pbpFeign.getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    String response = instance.getProductStatus(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign)
      .getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response, StringUtils.EMPTY);
  }

  @Test
   void getProductStatusTestValueEmptyResultTest() {
    Mockito.when(
      this.pbpFeign.getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse(null), REQUEST_ID));
    String response = instance.getProductStatus(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.pbpFeign)
      .getProductStatus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response, StringUtils.EMPTY);
  }

  @Test
   void getCategoryDetailByCategoryCodeTest() {
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new CategoryDetailResponse(), REQUEST_ID));
    CategoryDetailResponse responses = instance.getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(this.pcbFeign)
      .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        CATEGORY_CODE);
    Assertions.assertNotNull(responses);
  }

  @Test
   void getCategoryDetailByCategoryCode_sucessTest() {
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCategoryDetailByCategoryCode(CATEGORY_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          CATEGORY_CODE);
    }
  }

  @Test
   void getCategoryDetailByCategoryCode_responseNullTest() {
    Mockito.when(this.pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, CATEGORY_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCategoryDetailByCategoryCode(CATEGORY_CODE));
    } finally {
      Mockito.verify(this.pcbFeign)
        .getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          CATEGORY_CODE);
    }
  }

  @Test
   void validateAutoQcConfigChangeTest() {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest = new AutoQcConfigChangeRequest();
    Mockito.when(
      pbpFeign.verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        autoQcConfigChangeRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new SingleValueResponse("true"), REQUEST_ID));
    instance.validateAutoQcConfigChange(autoQcConfigChangeRequest);
    Mockito.verify(pbpFeign)
      .verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        autoQcConfigChangeRequest);
  }

  @Test
   void validateAutoQcConfigChangeSuccessFalseTest() {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest = new AutoQcConfigChangeRequest();
    Mockito.when(
      pbpFeign.verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        autoQcConfigChangeRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, new SingleValueResponse("true"), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.validateAutoQcConfigChange(autoQcConfigChangeRequest));
    } finally {
      Mockito.verify(pbpFeign)
        .verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          autoQcConfigChangeRequest);
    }
  }

  @Test
   void validateAutoQcConfigChangeNullTest() {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest = new AutoQcConfigChangeRequest();
    Mockito.when(
        pbpFeign.verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          autoQcConfigChangeRequest))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.validateAutoQcConfigChange(autoQcConfigChangeRequest));
    } finally {
      Mockito.verify(pbpFeign)
        .verifyAutoQcConfigChange(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          autoQcConfigChangeRequest);
    }
  }

  @Test
   void getCnCategoryCodesFromC1Test() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new CategoryCodeResponse(), REQUEST_ID));
    instance.getCnCategoryCodesFromC1(categoryCodeRequest);
    Mockito.verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest);
  }

  @Test
   void getCnCategoryCodesFromC1SuccessFalseTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, categoryCodeRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, new CategoryCodeResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCnCategoryCodesFromC1(categoryCodeRequest));
    } finally {
      Mockito.verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, categoryCodeRequest);
    }
  }

  @Test
   void getCnCategoryCodesFromC1NullTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, categoryCodeRequest))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getCnCategoryCodesFromC1(categoryCodeRequest));
    } finally {
      Mockito.verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, categoryCodeRequest);
    }
  }

  @Test
   void validateProtectedBrandTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(
      pcbFeign.validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE, PRODUCT_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new SimpleBooleanResponse(true), REQUEST_ID));
    instance.validateProtectedBrand(PRODUCT_CODE, PRODUCT_CODE);
    Mockito.verify(pcbFeign)
      .validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE, PRODUCT_CODE);
  }

  @Test
   void validateProtectedBrandSuccessFalseTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(
      pcbFeign.validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE, PRODUCT_CODE)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, new SimpleBooleanResponse(true), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.validateProtectedBrand(PRODUCT_CODE, PRODUCT_CODE));
    } finally {
      Mockito.verify(pcbFeign)
        .validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE, PRODUCT_CODE);
    }
  }

  @Test
   void validateProtectedBrandNullTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(
        pcbFeign.validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE, PRODUCT_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.validateProtectedBrand(PRODUCT_CODE, PRODUCT_CODE));
    } finally {
      Mockito.verify(pcbFeign)
        .validateAuthorisedBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE, PRODUCT_CODE);
    }
  }

  @Test
   void filterByBrandNameTest() {
    Mockito.when(
        pcbFeign.filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, new BrandResponse(), REQUEST_ID));
    instance.filterByBrandName(PRODUCT_CODE);
    Mockito.verify(pcbFeign)
      .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE);
  }

  @Test
   void filterByBrandNameSuccessFalseTest() {
    Mockito.when(
        pcbFeign.filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, new BrandResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class, () -> instance.filterByBrandName(PRODUCT_CODE));
    } finally {
      Mockito.verify(pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE);
    }
  }

  @Test
   void filterByBrandNameNullTest() {
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    Mockito.when(
      pcbFeign.filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class, () -> instance.filterByBrandName(PRODUCT_CODE));
    } finally {
      Mockito.verify(pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          PRODUCT_CODE);
    }
  }

  @Test
   void getProductBasicDetailByProductCodeTest() {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    Mockito.when(pcbFeign.getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE_1))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, productResponse, REQUEST_ID));
    instance.getProductBasicDetailByProductCode(PRODUCT_CODE_1);
    Mockito.verify(pcbFeign)
      .getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        PRODUCT_CODE_1);
  }

  @Test
   void getProductBasicDetailByProductCodeSuccessFalse_Test() {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(PRODUCT_ID);
    Mockito.when(pcbFeign.getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE_1))
      .thenReturn(new GdnRestSingleResponse<>(null, null, false, productResponse, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductBasicDetailByProductCode(PRODUCT_CODE_1));
    } finally {
      Mockito.verify(pcbFeign).getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE_1);
    }
  }

  @Test
   void getProductBasicDetailByProductCodeNullTest() {
    Mockito.when(pcbFeign.getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE_1))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductBasicDetailByProductCode(PRODUCT_CODE_1));
    } finally {
      Mockito.verify(pcbFeign).getProductBasicDetailByProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_CODE_1);
    }
  }

  @Test
   void clearProductCacheSyncByProductIdAndProductCodeTest() {
    Mockito.when(pcbFeign.clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_ID, PRODUCT_CODE_1))
      .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    instance.clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1);
    Mockito.verify(pcbFeign)
      .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_ID, PRODUCT_CODE_1);
  }

  @Test
   void clearProductCacheSyncByProductIdAndProductCodeSuccessFalse_Test() {
    Mockito.when(pcbFeign.clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, PRODUCT_ID, PRODUCT_CODE_1))
      .thenReturn(new GdnBaseRestResponse(null, null, false, null));
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.clearProductCacheSyncByProductIdAndProductCode(PRODUCT_ID, PRODUCT_CODE_1));
    } finally {
      Mockito.verify(pcbFeign)
        .clearProductCacheSyncByProductIdAndProductCode(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, PRODUCT_ID, PRODUCT_CODE_1);
    }
  }

  @Test
   void getPredefinedAllowedAttributeValueByAttributeCodeAndValueTest() {
    GdnRestSingleResponse<PredefinedAllowedAttributeValueResponse> gdnRestSingleResponse =
      new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(
        pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, BRAND_CODE, BRAND_CODE, false))
      .thenReturn(gdnRestSingleResponse);
    PredefinedAllowedAttributeValueResponse response =
      instance.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(BRAND_CODE, BRAND_CODE,
        false);
    Mockito.verify(pcbFeign)
      .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, BRAND_CODE, BRAND_CODE, false);
  }

  @Test
   void getPredefinedAllowedAttributeValueByAttributeCodeAndValueWithSuccessFalseTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(
        pcbFeign.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, BRAND_CODE, BRAND_CODE, false))
      .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getPredefinedAllowedAttributeValueByAttributeCodeAndValue(BRAND_CODE,
          BRAND_CODE, false));
    } finally {
      Mockito.verify(pcbFeign)
        .getPredefinedAllowedAttributeValueByAttributeCodeAndValue(Constants.DEFAULT_STORE_ID,
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, BRAND_CODE, BRAND_CODE, false);
    }
  }

  @Test
   void getProductDetailByProductCodeTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(new ProductDetailResponse());
    Mockito.when(pcbFeign.getProductDetailByProductCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, true, PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    instance.getProductDetailByProductCode(PRODUCT_CODE, true, false);
    Mockito.verify(pcbFeign)
      .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, true,
        PRODUCT_CODE, false);
  }

  @Test
   void getProductDetailByProductCodeExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pcbFeign.getProductDetailByProductCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, true, PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductDetailByProductCode(PRODUCT_CODE, true, false));
    } finally {
      Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          true, PRODUCT_CODE, false);
    }
  }

  @Test
   void getProductDetailByProductCodeEmptyResponseExceptionTest() {
    GdnRestSingleResponse gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(pcbFeign.getProductDetailByProductCode(Constants.DEFAULT_STORE_ID,
      Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID,
      Constants.DEFAULT_USERNAME, true, PRODUCT_CODE, false)).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductDetailByProductCode(PRODUCT_CODE, true, false));
    } finally {
      Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          true, PRODUCT_CODE, false);
    }
  }

  @Test
   void processProductVendorSearchAutoHealTest() {
    Mockito.when(
      pbpFeign.processProductVendorSearchAutoHeal(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    instance.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .processProductVendorSearchAutoHeal(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_CODE);
  }


  @Test
   void processProductVendorSearchAutoHealErrorTest() {
    Mockito.when(
      pbpFeign.processProductVendorSearchAutoHeal(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, PRODUCT_CODE)).thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));
    instance.processProductVendorSearchAutoHeal(STORE_ID, PRODUCT_CODE);
    Mockito.verify(pbpFeign)
      .processProductVendorSearchAutoHeal(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_CODE);
  }

  @Test
   void getProductBasicDetailByProductCodeFromXProductTest() {
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setProductCode(PRODUCT_CODE_1);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    GdnRestListResponse<PrdProductResponse> gdnRestListResponse =
      new GdnRestListResponse<>(null, null, true, List.of(prdProductResponse), null, REQUEST_ID);
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest =
      new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE_1);
    Mockito.when(xProductFeign.getPrdProductDetailByProductSkuOrProductCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest))
      .thenReturn(gdnRestListResponse);
    instance.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1);
    Mockito.verify(xProductFeign)
      .getPrdProductDetailByProductSkuOrProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest);
  }

  @Test
   void getProductBasicDetailByProductCodeFromXProductSuccessFalseTest() {
    PrdProductResponse prdProductResponse = new PrdProductResponse();
    prdProductResponse.setProductCode(PRODUCT_CODE_1);
    prdProductResponse.setTakenDown(Boolean.FALSE);
    prdProductResponse.setMarkForDelete(Boolean.FALSE);
    GdnRestListResponse<PrdProductResponse> gdnRestListResponse =
      new GdnRestListResponse<>(null, null, false, List.of(prdProductResponse), null, REQUEST_ID);
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest =
      new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE_1);
    Mockito.when(xProductFeign.getPrdProductDetailByProductSkuOrProductCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest))
      .thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1));
    } finally {
      Mockito.verify(xProductFeign)
        .getPrdProductDetailByProductSkuOrProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest);
    }
  }

  @Test
   void getProductBasicDetailByProductCodeFromXProductWhenContentNullTest() {
    GdnRestListResponse<PrdProductResponse> gdnRestListResponse =
      new GdnRestListResponse<>(null, null, true, null, null, REQUEST_ID);
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest =
      new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_CODE_1);
    Mockito.when(xProductFeign.getPrdProductDetailByProductSkuOrProductCode(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest))
      .thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(Exception.class,
        () -> instance.getProductBasicDetailByProductCodeFromXProduct(PRODUCT_CODE_1));
    } finally {
      Mockito.verify(xProductFeign)
        .getPrdProductDetailByProductSkuOrProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), productSkuAndProductCodeRequest);
    }
  }

  @Test
  public void testGetProductDetailForProduct_Success() {
    ProductSkuDetailResponse expectedResponse = new ProductSkuDetailResponse();
    GdnRestSingleResponse<ProductSkuDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(expectedResponse);
    Mockito.when(pbpFeign.getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(response);
    instance.getProductDetailForProduct(PRODUCT_SKU, PRODUCT_CODE);

    Mockito.verify(pbpFeign)
        .getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void testGetProductDetailForProduct_Failure() {
    GdnRestSingleResponse<ProductSkuDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setErrorMessage(ERROR_MSG);
    Mockito.when(pbpFeign.getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      instance.getProductDetailForProduct(PRODUCT_SKU, PRODUCT_CODE);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(pbpFeign)
          .getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void testGetProductDetailForProduct_Null() {
    GdnRestSingleResponse<ProductSkuDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    response.setErrorMessage(ERROR_MSG);
    Mockito.when(pbpFeign.getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      instance.getProductDetailForProduct(PRODUCT_SKU, PRODUCT_CODE);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(pbpFeign)
          .getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void testGetProductDetailForProduct_ProductNotFound() {
    GdnRestSingleResponse<ProductSkuDetailResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    response.setErrorMessage(ErrorMessages.PRODUCT_SKU_NOT_FOUND_IN_X_PRODUCT);
    Mockito.when(pbpFeign.getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      instance.getProductDetailForProduct(PRODUCT_SKU, PRODUCT_CODE);
    }
    catch (ValidationException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(pbpFeign)
          .getProductSkuDetailResponse(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void getSellerAnalyticsResponseSuccessTest() {
    SellerAnalyticsResponse expectedResponse = new SellerAnalyticsResponse();
    GdnRestSingleResponse<SellerAnalyticsResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(expectedResponse);
    Mockito.when(
            productAnalyticsFeign.getSellerAnalyticsDetailsBySellerCode(Mockito.any(),
                Mockito.any(),
                Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE)))
        .thenReturn(response);

    instance.getSellerAnalyticsResponse(MERCHANT_CODE);
    Mockito.verify(productAnalyticsFeign)
        .getSellerAnalyticsDetailsBySellerCode(Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE));
  }

  @Test
  public void getSellerAnalyticsResponseNullTest() {
    GdnRestSingleResponse<SellerAnalyticsResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    response.setErrorMessage(ERROR_MSG);
    Mockito.when(
            productAnalyticsFeign.getSellerAnalyticsDetailsBySellerCode(Mockito.any(),
                Mockito.any(),
                Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE)))
        .thenReturn(response);
    try {
      instance.getSellerAnalyticsResponse(MERCHANT_CODE);
    } finally {
      Mockito.verify(productAnalyticsFeign)
          .getSellerAnalyticsDetailsBySellerCode(Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE));
    }
  }

  @Test
  public void getSellerAnalyticsResponseSuccessFalseTest() {
    GdnRestSingleResponse<SellerAnalyticsResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setValue(null);
    response.setErrorMessage(ERROR_MSG);
    Mockito.when(
            productAnalyticsFeign.getSellerAnalyticsDetailsBySellerCode(Mockito.any(),
                Mockito.any(),
                Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE)))
        .thenReturn(response);
    try {
      instance.getSellerAnalyticsResponse(MERCHANT_CODE);
    } finally {
      Mockito.verify(productAnalyticsFeign)
          .getSellerAnalyticsDetailsBySellerCode(Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any(), Mockito.eq(MERCHANT_CODE));
    }
  }

  @Test
  public void testSuspendIprProduct_Success() {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(
        pbpFeign.doSuspensionAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any())).thenReturn(response);
    instance.suspendIprProduct(suspensionProductRequest);

    Mockito.verify(pbpFeign)
        .doSuspensionAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void testSuspendIprProduct_Failure() {
    SuspensionProductRequest suspensionProductRequest = new SuspensionProductRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorMessage(ERROR_MSG);
    Mockito.when(
        pbpFeign.doSuspensionAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.any())).thenReturn(response);
    try {
      instance.suspendIprProduct(suspensionProductRequest);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(pbpFeign)
          .doSuspensionAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any());
    }
  }

  @Test
  void fetchParentCategoryFromCnCategoryCodeTest() {
    List<CategoryHierarchyResponse> hierarchyResponsesList = new ArrayList<>();
    List<CategoryResponse> categoryResponsesList = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setParentCategoryId(CATEGORY_CODE);
    categoryResponsesList.add(categoryResponse);

    CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
    hierarchyResponsesList.add(categoryHierarchyResponse);

    categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setName(CATEGORY_NAME);

    categoryResponsesList.add(categoryResponse);
    categoryHierarchyResponse.setCategoryHierarchy(categoryResponsesList);

    Mockito.when(
      this.pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CategoryCodeRequest.class))).thenReturn(
      new GdnRestListResponse<>(null, null, true, hierarchyResponsesList, new PageMetaData(),
        REQUEST_ID));

    CategoryDTO categoryDTO = instance.fetchParentCategoryFromCnCategoryCode(CATEGORY_CODE);

    Mockito.verify(this.pcbFeign)
      .filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(CategoryCodeRequest.class));

    Assertions.assertEquals(CATEGORY_CODE, categoryDTO.getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, categoryDTO.getCategoryName());
  }
}
