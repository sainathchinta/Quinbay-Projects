package com.gdn.x.mta.distributiontask.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;

/**
 * Created by Alok on 9/22/16.
 */
public class DistributionTaskControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String PRODUCT_CODE = "MTA-0001";
  private static final String CORRECTION_REASON = "correctionReason";
  private static final String REJECT_REASON = "rejectReasonRequest";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String VENDOR_CODE = "vendorCode";
  private static final Long TIMESTAMP = System.currentTimeMillis();

  @InjectMocks
  DistributionTaskController distributionTaskController;

  @Mock
  private DistributionTaskService distributionTaskService;

  @Mock
  private ProductWrapperService productWrapperService;

  private Product product;

  private ProductDistributionTaskRequest createProductDistributionTaskRequest() {
    ProductDistributionTaskRequest productDistributionTaskRequest =
        new ProductDistributionTaskRequest();
    productDistributionTaskRequest.setVendorCode("vendor-code");
    List<String> productCodeList = new ArrayList<>();
    productCodeList.add("product-code");
    productCodeList.add("product-code-1");
    productCodeList.add("product-code-2");
    productDistributionTaskRequest.setProductCodes(productCodeList);
    return productDistributionTaskRequest;
  }

  private List<Product> createProductList() {
    List<Product> productList = new ArrayList<>();
    productList.add(new Product.Builder().businessPartnerCode("business-partner-code")
        .businessPartnerName("business-partner-name").categoryCode("category-code")
        .categoryName("category-name").productName("product-name").build());
    productList.add(new Product.Builder().businessPartnerCode("business-partner-code")
        .businessPartnerName("business-partner-name").categoryCode("category-code")
        .categoryName("category-name").productName("product-name").build());
    return productList;
  }
  
  @SuppressWarnings("unchecked")
  @Test
   void saveProductDistributionTaskTest() throws Exception {
    GdnBaseRestResponse response =
        this.distributionTaskController.saveProductDistributionTask(DistributionTaskControllerTest.STORE_ID,
            DistributionTaskControllerTest.CHANNEL_ID, DistributionTaskControllerTest.CLIENT_ID,
            DistributionTaskControllerTest.REQUEST_ID, DistributionTaskControllerTest.USER_NAME,
            new ProductDistributionTaskRequest());
    Mockito.verify(this.distributionTaskService).assignee(Mockito.any(), Mockito.any());
    Assertions.assertTrue(response.isSuccess());
  }
  
  @SuppressWarnings("unchecked")
  @Test
   void saveProductDistributionTaskWithExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.distributionTaskService)
        .assignee(Mockito.any(), Mockito.any());
    GdnBaseRestResponse response =
        this.distributionTaskController.saveProductDistributionTask(DistributionTaskControllerTest.STORE_ID,
            DistributionTaskControllerTest.CHANNEL_ID, DistributionTaskControllerTest.CLIENT_ID,
            DistributionTaskControllerTest.REQUEST_ID, DistributionTaskControllerTest.USER_NAME,
            new ProductDistributionTaskRequest());
    Mockito.verify(this.distributionTaskService).assignee(Mockito.any(), Mockito.any());
    Assertions.assertTrue(!response.isSuccess());
  }
  
  @Test
   void removeProductDetailsFromPDTTest() throws Exception {
    RemoveProductRequest request = new RemoveProductRequest();
    request.setProductCode(PRODUCT_CODE);
    Mockito.doNothing().when(productWrapperService).removeProductAndDeleteOriginalImages(request);
    GdnBaseRestResponse response = distributionTaskController
        .removeProductDetailsFromPDT(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, request);
    Mockito.verify(productWrapperService).removeProductAndDeleteOriginalImages(request);
    Assertions.assertTrue(response.isSuccess());
  }
  
  @Test
   void removeProductDetailsFromPDTRequestNullTest() throws Exception {
    RemoveProductRequest request = new RemoveProductRequest();
    request.setProductCode(REQUEST_ID);
    GdnBaseRestResponse response = distributionTaskController
        .removeProductDetailsFromPDT(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, null);
    Assertions.assertTrue(!response.isSuccess());
  }
  
  @Test
   void removeProductDetailsFromPDTProductCodeNullTest() throws Exception {
	RemoveProductRequest request = new RemoveProductRequest();
    GdnBaseRestResponse response = distributionTaskController.removeProductDetailsFromPDT(STORE_ID, CHANNEL_ID, CLIENT_ID,
        REQUEST_ID, USER_NAME, request);
    Assertions.assertTrue(!response.isSuccess());
  }

  @Test
   void doProductNeedCorrectionTest() throws Exception {
    NeedRevisionRequest request = new NeedRevisionRequest();
    request.setProductCodes(Collections.singletonList(PRODUCT_CODE));
    request.setAdditionalNotes(ADDITIONAL_NOTES);
    request.setCorrectionReason(CORRECTION_REASON);
    request.setRejectionReason(REJECT_REASON);
    Mockito.when(productWrapperService
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request))
        .thenReturn(new NeedRevisionResponse(true, true ,true, 0));
    GdnBaseRestResponse response = this.distributionTaskController
        .doProductNeedCorrection(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request);
    Mockito.verify(productWrapperService)
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
   void doProductNeedCorrectionRequestFalseTest() throws Exception {
    NeedRevisionRequest request = new NeedRevisionRequest();
    request.setProductCodes(Collections.singletonList(PRODUCT_CODE));
    Mockito.when(productWrapperService
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request))
        .thenReturn(new NeedRevisionResponse(true, true ,false, 1));
    GdnBaseRestResponse response = distributionTaskController
        .doProductNeedCorrection(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request);
    Mockito.verify(productWrapperService)
        .doProductNeedForCorrectionAndReindexSolr(STORE_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, request);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
   void doProductNeedCorrectionRequestExceptionTest() throws Exception {
    GdnBaseRestResponse response = distributionTaskController
        .doProductNeedCorrection(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USER_NAME, VENDOR_CODE, null);
    Assertions.assertFalse(response.isSuccess());
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void setUP() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.doNothing().when(this.distributionTaskService).assignee(Mockito.anyString(), Mockito.anyList());
    product = new Product();
    product.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void shutDown() {
    Mockito.verifyNoMoreInteractions(productWrapperService);
  }


}
