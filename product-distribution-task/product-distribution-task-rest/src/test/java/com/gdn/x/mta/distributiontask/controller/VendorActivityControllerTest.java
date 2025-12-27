package com.gdn.x.mta.distributiontask.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.controller.util.ModelConverterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductBusinessPartnerService;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.ScheduledJobService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;

import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
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
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

/**
 * Created by virajjasani on 09/10/16.
 */
public class VendorActivityControllerTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "product1";
  private static final String VENDOR_CODE = "vCode1";
  private static final String VENDOR_ID = "v1";
  private static final Date CURRENT_DATE = new Date(12345678L);
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "user";
  private static final String CONTENT_ASSIGNEE = "content";
  private static final String IMAGE = "image";
  private static final String IMAGE_ASSIGNEE = "image";
  private static final String DAYS = "6";
  private static final String BATCH_SIZE = "100";
  private static final String ACTION = "action";
  private static final String TYPE = "type";
  private static final String PRODUCT_CODE1 = "PRODUCT_CODE";
  private static List<String> PRODUCT_CODES = new ArrayList<>();
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGN = "assign";
  private static final String UNASSIGN = "unassign";
  private static final String NOT_APPLICABLE  = "NA";
  private static final String NOTES = "NOTES";
  private static final String ERROR_MESSAGE = "Reject reason is null";
  private static final String PRODUCT = "PRODUCT";

  private DistributionProductDetailRequest distributionProductDetailRequest;
  private RejectProductVendorRequest rejectProductVendorRequest;
  private RejectProductVendorRequest rejectProductVendorRequestEmptyProductCode;
  private VendorApprovalRequest vendorApprovalRequest;
  private Product product;
  private BulkVendorProductActionsRequest bulkVendorProductActionsRequest;
  private ProductImageQcFeedbackRequest productImageQcFeedbackRequest;
  private BulkVendorProductActionsDTO bulkVendorProductActionsDTO;
  private BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO;
  private BulkScreeningProductActionsRequest
      bulkScreeningProductActionsRequest = new BulkScreeningProductActionsRequest();

  @InjectMocks
  private VendorActivityController vendorActivityController;

  @Mock
  private ModelConverterUtil modelConverterUtil;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private VendorService vendorService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Mock
  private ScheduledJobService scheduledJobService;

  @Captor
  private ArgumentCaptor<Product> productCaptor;

  private MockMvc mockMvc;

  private ObjectMapper objectMapper = new ObjectMapper();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.vendorActivityController).build();
    distributionProductDetailRequest = new DistributionProductDetailRequest();
    distributionProductDetailRequest.setCreatedDate(CURRENT_DATE);
    distributionProductDetailRequest.setProductCode(PRODUCT_CODE);
    distributionProductDetailRequest.setProductName(PRODUCT_NAME);
    List<DistributionProductAttributeRequest> distributionProductAttributeRequestList =
        new ArrayList<>();
    distributionProductAttributeRequestList.add(new DistributionProductAttributeRequest());
    List<DistributionProductImageRequest> distributionProductImageRequestList=new ArrayList<>();
    distributionProductImageRequestList.add(new DistributionProductImageRequest());
    distributionProductDetailRequest.setProductAttributes(distributionProductAttributeRequestList);
    distributionProductDetailRequest.setProductImages(distributionProductImageRequestList);
    distributionProductDetailRequest.setBrand("test-brand");
    distributionProductDetailRequest.setDescription(new byte[] {1, 2, 3});
    distributionProductDetailRequest.setLongDescription(new byte[] {1, 2, 3});
    distributionProductDetailRequest.setUniqueSellingPoint("selling-point-1");
    distributionProductDetailRequest.setLength(12D);
    distributionProductDetailRequest.setWeight(12D);
    distributionProductDetailRequest.setHeight(12D);
    distributionProductDetailRequest.setWidth(12D);
    distributionProductDetailRequest.setShippingWeight(12D);
    rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setNotes("Test");
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    List<String> productList = new ArrayList<>();
    productList.add(PRODUCT);
    rejectReasonRequest.setProduct(productList);
    rejectReasonRequest.setImage(new ArrayList<>());
    rejectReasonRequest.setContent(new ArrayList<>());
    rejectProductVendorRequest.setRejectReasonRequest(rejectReasonRequest);
    vendorApprovalRequest = new VendorApprovalRequest();
    vendorApprovalRequest.setProductCode(PRODUCT_CODE);
    vendorApprovalRequest.setVendorCode(VENDOR_CODE);
    vendorApprovalRequest.setApprove("content");
    Vendor vendor = new Vendor();
    vendor.setVendorCode(VENDOR_CODE);
    vendor.setId(VENDOR_ID);
    product = new Product();
    product.setCurrentVendor(vendor);
    product.setProductCode(PRODUCT_CODE);
    PRODUCT_CODES.add(PRODUCT_CODE1);
    bulkScreeningProductActionsRequest.setProductCodes(PRODUCT_CODES);
    bulkScreeningProductActionsRequest.setAssignedBy(ASSIGNED_BY);
    bulkScreeningProductActionsRequest.setAssignTo(ASSIGNED_TO);

    bulkVendorProductActionsRequest = new BulkVendorProductActionsRequest();
    bulkVendorProductActionsRequest.setBulkScreeningProductActionsRequests(Collections
        .singletonList(bulkScreeningProductActionsRequest));
    bulkVendorProductActionsDTO = new BulkVendorProductActionsDTO();
    bulkScreeningProductActionsDTO = new BulkScreeningProductActionsDTO();
    bulkScreeningProductActionsDTO.setProductCodes(PRODUCT_CODES);
    bulkScreeningProductActionsDTO.setAssignedBy(ASSIGNED_BY);
    bulkScreeningProductActionsDTO.setAssignTo(ASSIGNED_TO);
    bulkVendorProductActionsDTO
        .setBulkScreeningProductActionsRequests(Collections.singletonList(bulkScreeningProductActionsDTO));

    productImageQcFeedbackRequest = new ProductImageQcFeedbackRequest();
    productImageQcFeedbackRequest.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(modelConverterUtil);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(vendorService);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(productImageQcFeedbackService);
  }

  @Test
   void updateProductContentTest() throws Exception {
    product.setImageViolations(IMAGE);
    product.setProductPredictionScore(11);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, CONTENT_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenReturn(product);
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
        distributionProductDetailRequest, NOT_APPLICABLE)).thenReturn(newProduct);
    Mockito
        .when(productWrapperService.updateProductDetails((Product) Mockito.any(), (Product) Mockito.any()))
        .thenReturn(null);
    vendorActivityController.updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, VENDOR_CODE, distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Mockito.verify(modelConverterUtil)
        .convertProductDetailRequestToProductEntity(Mockito.any(), eq(NOT_APPLICABLE));
    Mockito.verify(productWrapperService).updateProductDetails(productCaptor.capture(), Mockito.any());
    Assertions.assertEquals(11, productCaptor.getValue().getProductPredictionScore());
    Assertions.assertEquals(IMAGE, productCaptor.getValue().getImageViolations());
  }

  @Test
   void updateProductContentTest_ProductCodeEmpty() throws Exception {
    distributionProductDetailRequest.setProductCode("");
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_CODE_EMPTY));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_AttributeValueExceed255Character() throws Exception {
    List<DistributionProductAttributeRequest> distributionProductAttributeRequestList =
        new ArrayList<>();
    DistributionProductAttributeRequest distributionProductAttributeRequest =
        new DistributionProductAttributeRequest();
    distributionProductAttributeRequest.setValue(StringUtils.leftPad("Test",256,"*"));
    distributionProductAttributeRequestList.add(distributionProductAttributeRequest);
    distributionProductDetailRequest.setProductAttributes(distributionProductAttributeRequestList);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(response.getErrorMessage().contains(
        DistributionProductMessageUtil.ATTRIBUTE_VALUE_LENGTH_EXCEEDED_LENGTH));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_ProductNameEmpty() throws Exception {
    distributionProductDetailRequest.setProductName("");
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_NAME_EMPTY));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_CreatedDateNull() throws Exception {
    distributionProductDetailRequest.setCreatedDate(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.CREATED_DATE_EMPTY));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_ExistingProductNotFound() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, CONTENT_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE))
        .thenReturn(null);
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil
        .convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE))
        .thenReturn(newProduct);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_NOT_FOUND_ERROR));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentExceptionTestFound() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, CONTENT_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenThrow(new NullPointerException());
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil
            .convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE))
        .thenReturn(newProduct);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void updateProductContentTest_ProductUnassigned() throws Exception {
    product.setCurrentVendor(null);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE))
        .thenReturn(product);
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil
        .convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE))
        .thenReturn(newProduct);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_UNASSIGNED_STATUS_ERROR));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_Passed() throws Exception {
    product.setImageViolations(IMAGE);
    product.setProductPredictionScore(11);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, CONTENT_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenReturn(product);
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
        distributionProductDetailRequest, NOT_APPLICABLE)).thenReturn(newProduct);
    Mockito
        .when(productWrapperService.updateProductDetails((Product) Mockito.any(), (Product) Mockito.any()))
        .thenReturn(null);
    product.setState(WorkflowState.PASSED);
    vendorActivityController.updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME, VENDOR_CODE, distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
  }

  @Test
   void updateProductContentTest_BlankDescription() throws Exception {
    distributionProductDetailRequest.setDescription(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.DESCRIPTION_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankLongDescription() throws Exception {
    distributionProductDetailRequest.setLongDescription(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.LONG_DESCRIPTION_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankLength() throws Exception {
    distributionProductDetailRequest.setLength(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.LENGTH_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankWidth() throws Exception {
    distributionProductDetailRequest.setWidth(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.WIDTH_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankHeight() throws Exception {
    distributionProductDetailRequest.setHeight(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.HEIGHT_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankWeight() throws Exception {
    distributionProductDetailRequest.setWeight(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.WEIGHT_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankShippingWeight() throws Exception {
    distributionProductDetailRequest.setShippingWeight(null);
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_BlankProductAttribute() throws Exception {
    distributionProductDetailRequest.setProductAttributes(new ArrayList<>());
    GdnBaseRestResponse response = vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK));
    Assertions.assertEquals(ErrorCategory.VALIDATION.getCode(), response.getErrorCode());
  }

  @Test
   void updateProductContentTest_Exception() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setVendorCode(VENDOR_ID);
    Product existingProduct = new Product();
    existingProduct.setCurrentVendor(vendor);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE))
        .thenReturn(existingProduct);
    Product newProduct = new Product();
    vendorActivityController
        .updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
  }

  @Test
   void rejectProductTest() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    Mockito.doNothing().when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class),
            Mockito.anyString());
    vendorActivityController.rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        VENDOR_CODE, rejectProductVendorRequest);
    Mockito.verify(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class),
            Mockito.anyString());
  }

  @Test
  void rejectProductTest_validationException() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    Mockito.doThrow(ValidationException.class).when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class),
            Mockito.anyString());
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + "/reject-product")
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .param("vendorCode", VENDOR_CODE)
        .content(objectMapper.writeValueAsString(rejectProductVendorRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class),
            Mockito.anyString());
  }

  @Test
   void rejectProductTest_RejectReasonNull() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    RejectProductVendorRequest rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setNotes(NOTES);
    rejectProductVendorRequest.setRejectReasonRequest(null);
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    Mockito.doNothing().when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class), Mockito.anyString());
    GdnBaseRestResponse gdnBaseRestResponse = vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE, rejectProductVendorRequest);
    Assertions.assertEquals(ERROR_MESSAGE, gdnBaseRestResponse.getErrorMessage());
  }

  @Test
   void rejectProductTest_RejectReasonProductNull() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    RejectProductVendorRequest rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setNotes(NOTES);
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    rejectReasonRequest.setProduct(null);
    rejectReasonRequest.setContent(null);
    rejectReasonRequest.setImage(null);
    rejectProductVendorRequest.setRejectReasonRequest(rejectReasonRequest);
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    Mockito.doNothing().when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class), Mockito.anyString());
    GdnBaseRestResponse gdnBaseRestResponse = vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE, rejectProductVendorRequest);
    Assertions.assertEquals(DistributionProductMessageUtil.REJECT_REASON_OF_PRODUCT_NULL_OR_EMPTY,
        gdnBaseRestResponse.getErrorMessage());
  }

  @Test
   void rejectProductTest_RejectReasonProductEmpty() throws Exception {
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    RejectProductVendorRequest rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setNotes(NOTES);
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    rejectReasonRequest.setProduct(new ArrayList<>());
    rejectReasonRequest.setContent(new ArrayList<>());
    rejectReasonRequest.setImage(new ArrayList<>());
    rejectProductVendorRequest.setRejectReasonRequest(rejectReasonRequest);
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    Mockito.doNothing().when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class), Mockito.anyString());
    GdnBaseRestResponse gdnBaseRestResponse = vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE, rejectProductVendorRequest);
    Assertions.assertEquals(DistributionProductMessageUtil.REJECT_REASON_OF_PRODUCT_NULL_OR_EMPTY,
        gdnBaseRestResponse.getErrorMessage());
  }

  @Test
   void rejectProductTest_EmptyVendorCode() throws Exception {
    Vendor vendor = new Vendor();
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    GdnBaseRestResponse response=vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, "",
            rejectProductVendorRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.VENDOR_CODE_EMPTY_ERROR));
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void rejectProductTest_RejectRequestNull() throws Exception {
    Vendor vendor = new Vendor();
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    GdnBaseRestResponse response=vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            null);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.REJECT_PRODUCT_DETAIL_ERROR));
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void rejectProductTest_RejectRequestProductCodeEmpty() throws Exception {
    rejectProductVendorRequestEmptyProductCode = new RejectProductVendorRequest();
    rejectProductVendorRequestEmptyProductCode.setProductCode("");
    rejectProductVendorRequestEmptyProductCode.setNotes("Test");
    Vendor vendor = new Vendor();
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    GdnBaseRestResponse response=vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            rejectProductVendorRequestEmptyProductCode);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_CODE_EMPTY));
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void rejectProductTest_RejectRequestNotesEmpty() throws Exception {
    rejectProductVendorRequestEmptyProductCode = new RejectProductVendorRequest();
    rejectProductVendorRequestEmptyProductCode.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequestEmptyProductCode.setNotes("");
    Vendor vendor = new Vendor();
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    GdnBaseRestResponse response=vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            rejectProductVendorRequestEmptyProductCode);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.NOTES_EMPTY));
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void rejectProductTest_ImageRejected() throws Exception {
    RejectProductVendorRequest rejectImageProductVendorRequest = new RejectProductVendorRequest();
    rejectImageProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectImageProductVendorRequest.setNotes("Test");
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReasonRequest.setProduct(product);
    rejectReasonRequest.setContent(new ArrayList<>());
    rejectReasonRequest.setImage(new ArrayList<>());
    rejectImageProductVendorRequest.setRejectReasonRequest(rejectReasonRequest);
    Vendor vendor = new Vendor();
    vendor.setAbleToReject(true);
    Mockito.doNothing().when(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class), Mockito.anyString());
    vendorActivityController.rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
        rejectImageProductVendorRequest);
    Mockito.verify(productWrapperService)
        .rejectProductByVendorAndDeleteFromSolr(Mockito.any(RejectProductDTO.class), Mockito.anyString());
  }

  @Test
   void rejectProductTest_InvalidState() throws Exception {
    rejectProductVendorRequestEmptyProductCode = new RejectProductVendorRequest();
    rejectProductVendorRequestEmptyProductCode.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequestEmptyProductCode.setNotes("test");
    Vendor vendor = new Vendor();
    Mockito.when(vendorService.findByVendorCode(VENDOR_CODE)).thenReturn(vendor);
    GdnBaseRestResponse response=vendorActivityController
        .rejectProduct(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            rejectProductVendorRequestEmptyProductCode);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void approveProductTest() throws Exception {
    distributionProductDetailRequest.setNotes(NOTES);
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
        Mockito.any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE))).thenReturn(product);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(
        VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_APPROVAL_NEW)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("vendorCode", VENDOR_CODE)
        .content(objectMapper.writeValueAsString(distributionProductDetailRequest));
    this.mockMvc.perform(request)
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productWrapperService)
        .updateAndApproveProduct(productCaptor.capture(), eq(VENDOR_CODE), Mockito.anyString());
    Assertions.assertEquals(distributionProductDetailRequest.getProductCode(),
        productCaptor.getValue().getProductCode());
    Mockito.verify(modelConverterUtil)
        .convertProductDetailRequestToProductEntity(any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE));
  }

  @Test
   void approveProductTest_ValidationException() throws Exception {
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
        Mockito.any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE))).thenThrow(new NullPointerException());
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(
        VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_APPROVAL_NEW)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("vendorCode", VENDOR_CODE)
        .content("{}");
    try {
      this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    } finally {
      Mockito.verify(modelConverterUtil)
          .convertProductDetailRequestToProductEntity(any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE));
    }
  }

  @Test
   void approveProductTest_ApplicationRuntimeException() throws Exception {
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
            Mockito.any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE)))
        .thenThrow(new ApplicationRuntimeException());
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(
            VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_APPROVAL_NEW)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("vendorCode", VENDOR_CODE)
        .content("{}");
    try {
      this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    } finally {
      Mockito.verify(modelConverterUtil)
          .convertProductDetailRequestToProductEntity(any(DistributionProductDetailRequest.class), eq(NOT_APPLICABLE));
    }
  }

  @Test
   void updateProductImageInfoTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, IMAGE_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(modelConverterUtil
        .convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE))
        .thenReturn(product);
    Mockito.when(productService.updateProductImageDetails((Product) Mockito.any(),
        (Product) Mockito.any(), eq(Boolean.FALSE))).thenReturn(null);
    vendorActivityController
        .updateProductImageInfo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Mockito.verify(modelConverterUtil).convertProductDetailRequestToProductEntity(
        Mockito.any(), eq(NOT_APPLICABLE));
    Mockito.verify(productService)
        .updateProductImageDetails((Product) Mockito.any(), (Product) Mockito.any(), eq(Boolean.FALSE));
  }

  @Test
   void updateProductImageInfoTest_Exception() throws Exception {
    vendorActivityController
        .updateProductImageInfo(null, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
  }

  @Test
   void updateProductImageInfo_EmptyProductImages() throws Exception {
    distributionProductDetailRequest.setProductImages(new ArrayList<>());
    GdnBaseRestResponse response = vendorActivityController
        .updateProductImageInfo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertTrue(
        response.getErrorMessage().contains(DistributionProductMessageUtil.PRODUCT_IMAGES_EMPTY));
    Assertions.assertNull(response.getErrorCode());
  }

  @Test
   void updateProductImageInfoTest_Passed() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, IMAGE_ASSIGNEE);
    product.setState(WorkflowState.PASSED);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.when(modelConverterUtil
        .convertProductDetailRequestToProductEntity(distributionProductDetailRequest, NOT_APPLICABLE))
        .thenReturn(product);
    Mockito.when(productService.updateProductImageDetails((Product) Mockito.any(),
        (Product) Mockito.any(), eq(Boolean.FALSE))).thenReturn(null);
    vendorActivityController
        .updateProductImageInfo(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, VENDOR_CODE,
            distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
  }

  @Test
   void ValidationUtilGetInstance_ThrowsException() {
    try {
      ValidationUtil.getInstance();
    } catch(IllegalAccessError iea) {
      Assertions.assertEquals("Utility class", iea.getMessage());
    }
  }

  @Test
   void doVendorProductActions_wrongActionTest() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_PRODUCT_ACTIONS, ACTION, IMAGE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(bulkScreeningProductActionsRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productWrapperService, Mockito.times(0))
        .updateAssigneeDetails(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, ACTION, ASSIGNED_TO);
  }

  @Test
   void bulkVendorProductActionsTest() throws Exception {
    Mockito.when(modelConverterUtil.toBulkVendorProductActionsDTO(bulkVendorProductActionsRequest))
        .thenReturn(bulkVendorProductActionsDTO);
    bulkVendorProductActionsRequest.setActionType(ASSIGN);
    bulkVendorProductActionsRequest.setAssignmentType(IMAGE);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.BULK_VENDOR_PRODUCT_ACTIONS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(bulkVendorProductActionsRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productWrapperService).bulkUpdateProductAssignee(STORE_ID, bulkVendorProductActionsDTO);
    Mockito.verify(modelConverterUtil).toBulkVendorProductActionsDTO(bulkVendorProductActionsRequest);
  }

  @Test
   void bulkVendorProductActionsWrongActionTest() throws Exception {
    bulkVendorProductActionsRequest.setActionType(ACTION);
    bulkVendorProductActionsRequest.setAssignmentType(TYPE);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.BULK_VENDOR_PRODUCT_ACTIONS, ACTION, TYPE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(bulkVendorProductActionsRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
  }

  @Test
   void doVendorProductActionsTest() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_PRODUCT_ACTIONS, UNASSIGN, IMAGE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(bulkScreeningProductActionsRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productWrapperService)
        .updateAssigneeDetails(STORE_ID, PRODUCT_CODES, ASSIGNED_BY, UNASSIGN, ASSIGNED_TO);
  }

  @Test
   void doDeleteForProductsTest() throws Exception {
    Mockito.doNothing().when(scheduledJobService).deleteProducts(STORE_ID, Integer.parseInt(DAYS), Integer.parseInt(BATCH_SIZE),
        Integer.parseInt(BATCH_SIZE), false);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .delete(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_PRODUCT_DELETE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME).param("days", DAYS)
        .param("batchSize", BATCH_SIZE).param("maxBatchSize", BATCH_SIZE);;
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(scheduledJobService).deleteProducts(STORE_ID, Integer.parseInt(DAYS), Integer.parseInt(BATCH_SIZE),
        Integer.parseInt(BATCH_SIZE), false);
  }

  @Test
   void updateProductContentTestUniqueSellingPointAsNull() throws Exception {
    distributionProductDetailRequest.setUniqueSellingPoint(null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, CONTENT_ASSIGNEE);
    Mockito.when(productService.getAllProductDetailsByCode(PRODUCT_CODE)).thenReturn(product);
    Product newProduct = new Product();
    Mockito.when(modelConverterUtil.convertProductDetailRequestToProductEntity(
            distributionProductDetailRequest, NOT_APPLICABLE)).thenReturn(newProduct);
    Mockito.when(productWrapperService.updateProductDetails((Product) Mockito.any(),
            (Product) Mockito.any())).thenReturn(null);
    vendorActivityController.updateProductContent(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
            USERNAME, VENDOR_CODE, distributionProductDetailRequest);
    Mockito.verify(productService).getAllProductDetailsByCode(Mockito.anyString());
    Mockito.verify(modelConverterUtil)
            .convertProductDetailRequestToProductEntity(Mockito.any(), eq(NOT_APPLICABLE));
    Mockito.verify(productWrapperService)
            .updateProductDetails(Mockito.any(), Mockito.any());
  }

  @Test
   void updateProductImageFeedbackTest() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .put(VendorActivityController.BASE_PATH + VendorActivityController.UPDATE_PRODUCT_IMAGE_FEEDBACK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(productImageQcFeedbackRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
  }

  @Test
   void updateProductImageFeedbackExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .put(VendorActivityController.BASE_PATH + VendorActivityController.UPDATE_PRODUCT_IMAGE_FEEDBACK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(productImageQcFeedbackRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
  }

  @Test
   void updateProductImageFeedbackApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(productImageQcFeedbackService)
        .upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .put(VendorActivityController.BASE_PATH + VendorActivityController.UPDATE_PRODUCT_IMAGE_FEEDBACK)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(productImageQcFeedbackRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productImageQcFeedbackService).upsertImageQcFeedback(productImageQcFeedbackRequest, true, false);
  }

  @Test
   void doDeleteForProductsTest_daysIsZero() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .delete(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_PRODUCT_DELETE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME).param("days", "0")
        .param("batchSize", BATCH_SIZE).param("maxBatchSize", BATCH_SIZE);
    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    });
  }

  @Test
   void doDeleteForProductsTest_batchSizeIsZero() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .delete(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_PRODUCT_DELETE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME).param("days", DAYS)
        .param("batchSize", "0").param("maxBatchSize", "0");
    Assertions.assertThrows(Exception.class, () -> {
      this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    });
  }

  @Test
   void quickApproveProductTest() throws Exception {
    VendorQuickApprovalRequest vendorQuickApprovalRequest = VendorQuickApprovalRequest.builder().build();
    Mockito.when(productWrapperService.quickApproveProduct(vendorQuickApprovalRequest)).thenReturn(
        VendorQuickApprovalResponse.builder().build());
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_QUICK_APPROVAL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(vendorQuickApprovalRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productWrapperService)
        .quickApproveProduct(vendorQuickApprovalRequest);
  }

  @Test
   void quickApproveProductExceptionTest() throws Exception {
    VendorQuickApprovalRequest vendorQuickApprovalRequest = VendorQuickApprovalRequest.builder().build();
    Mockito.when(productWrapperService.quickApproveProduct(vendorQuickApprovalRequest)).thenThrow(Exception.class);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(VendorActivityController.BASE_PATH + VendorActivityController.VENDOR_QUICK_APPROVAL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME)
        .content(objectMapper.writeValueAsString(vendorQuickApprovalRequest));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(productWrapperService)
        .quickApproveProduct(vendorQuickApprovalRequest);
  }
}
