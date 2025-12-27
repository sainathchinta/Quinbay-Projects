package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.List;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.pdt.feign.PDTFeign;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;

public class ProductDistributionTaskRepositoryBeanTest {

  private static final String USER_NAME = "username";
  private static final String PRODUCT_CODE = "product-code";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Mock
  private PDTFeign pdtFeign;

  private GdnRestListResponse<DistributionProductResponse> baseDistributionProductResponses;
  private GdnRestSimpleResponse<ProductImageQcFeedbackResponse> productImageQcFeedbackResponse;
  private ProductActionRetryEvent productActionRetryEvent;

  private DistributionProductDetailResponse generateDistributionProductDetailResponse() throws Exception {
    DistributionProductDetailResponse distributionProductDetailResponse = new DistributionProductDetailResponse();
    return distributionProductDetailResponse;
  }

  private List<DistributionProductResponse> generateDistributionProductResponses() throws Exception {
    List<DistributionProductResponse> distributionProductResponses = new ArrayList<DistributionProductResponse>();
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    distributionProductResponses.add(distributionProductResponse);
    return distributionProductResponses;
  }

  private PDTProductDomainEventModelResponse generatePdtProductDomainEventModelResponse() throws Exception {
    PDTProductDomainEventModelResponse pdtProductDomainEventModelResponse = new PDTProductDomainEventModelResponse();
    return pdtProductDomainEventModelResponse;
  }

  @BeforeEach
  public void initialize_Test() throws Exception {
    MockitoAnnotations.initMocks(this);
    DistributionProductDetailResponse distributionProductDetailResponse =
        this.generateDistributionProductDetailResponse();
    List<DistributionProductResponse> distributionProductResponses = this.generateDistributionProductResponses();
    PDTProductDomainEventModelResponse pdtProductDomainEventModelResponse =
        this.generatePdtProductDomainEventModelResponse();
    GdnBaseRestResponse baseResponse = new GdnBaseRestResponse(REQUEST_ID);
    GdnRestSingleResponse<DistributionProductDetailResponse> baseDistributionProductDetailResponse =
        new GdnRestSingleResponse<>(distributionProductDetailResponse,
            REQUEST_ID);
    baseDistributionProductResponses =
        new GdnRestListResponse<>(distributionProductResponses, new PageMetaData(1, 0, 1),
            REQUEST_ID);
    GdnRestSingleResponse<PDTProductDomainEventModelResponse> basePdtProductDomainEventModelResponse =
        new GdnRestSingleResponse<>(pdtProductDomainEventModelResponse,
            REQUEST_ID);
    
    productImageQcFeedbackResponse = new GdnRestSimpleResponse<>();
    productImageQcFeedbackResponse.setSuccess(true);
    productImageQcFeedbackResponse.setValue(new ProductImageQcFeedbackResponse());
    ReflectionTestUtils.setField(productDistributionTaskRepositoryBean, "addProductToPdtRetry", true);
    productActionRetryEvent =
        new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, Constants.PDT_RETRY_DELETE,
            StringUtils.EMPTY);
  }

  @AfterEach
  public void finalize_Test() {
    Mockito.verifyNoMoreInteractions(this.pdtFeign);
  }

  @Test
  public void moveFailedProductToQC_Test() throws Exception {
    Mockito.when(
        pdtFeign.moveFailedProductToQC(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any())).thenReturn(new GdnBaseRestResponse(true));
    this.productDistributionTaskRepositoryBean
        .moveFailedProductToQC(REQUEST_ID, null, null);
    Mockito.verify(this.pdtFeign).moveFailedProductToQC(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void moveFailedProductToQC_exception_Test() throws Exception {
    Mockito.when(
        pdtFeign.moveFailedProductToQC(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
                Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(IllegalStateException.class, () -> {
        this.productDistributionTaskRepositoryBean.moveFailedProductToQC(REQUEST_ID, USER_NAME,
            PRODUCT_CODE);
      });
    } catch (IllegalStateException e) {
      throw e;
    }
    Mockito.verify(this.pdtFeign).moveFailedProductToQC(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void getDetailsForAnyProduct_Test() throws Exception {
    GdnRestSingleResponse<DistributionProductDetailResponse> baseDistributionProductDetailResponse =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.TRUE, null,
            REQUEST_ID);
    Mockito.when(this.pdtFeign.getDetailsForProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any()))
        .thenReturn(baseDistributionProductDetailResponse);
    this.productDistributionTaskRepositoryBean.getDetailsForAnyProduct(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    Mockito.verify(this.pdtFeign)
        .getDetailsForProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any());
  }

  @Test
  public void getDetailsForAnyProduct_SuccessFalse_Test() throws Exception {
    GdnRestSingleResponse<DistributionProductDetailResponse> baseDistributionProductDetailResponseError =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.FALSE, null,
            REQUEST_ID);
    Mockito.when(this.pdtFeign.getDetailsForProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any()))
        .thenReturn(baseDistributionProductDetailResponseError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productDistributionTaskRepositoryBean.getDetailsForAnyProduct(REQUEST_ID, USER_NAME, PRODUCT_CODE);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(this.pdtFeign).getDetailsForProduct(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void removeProductFromPDT_Test() throws Exception {
    GdnRestSingleResponse baseResponse =
        new GdnRestSingleResponse(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.TRUE, null, REQUEST_ID);
    Mockito.when(this.pdtFeign
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class)))
        .thenReturn(baseResponse);
    this.productDistributionTaskRepositoryBean.removeProductFromPDT(REQUEST_ID, USER_NAME, new RemoveProductRequest());
    Mockito.verify(this.pdtFeign)
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class));
  }

  @Test
  public void removeProductFromPDT_SuccessFalseTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskRepositoryBean, "addProductToPdtRetry", false);
    RemoveProductRequest removeProductRequest = new RemoveProductRequest();
    removeProductRequest.setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse baseMapResponseError =
        new GdnRestSingleResponse(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.FALSE, null,
            REQUEST_ID);
    Mockito.when(this.pdtFeign
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class)))
        .thenReturn(baseMapResponseError);
    this.productDistributionTaskRepositoryBean
        .removeProductFromPDT(REQUEST_ID, USER_NAME, removeProductRequest);
    Mockito.verify(this.pdtFeign)
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class));
  }

  @Test
  public void removeProductFromPDT_SuccessFalseFlagTrueTest() throws Exception {
    RemoveProductRequest removeProductRequest = new RemoveProductRequest();
    removeProductRequest.setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse baseMapResponseError =
        new GdnRestSingleResponse(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.FALSE, null, REQUEST_ID);
    Mockito.when(this.pdtFeign
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class)))
        .thenReturn(baseMapResponseError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productDistributionTaskRepositoryBean.removeProductFromPDT(REQUEST_ID, USER_NAME, removeProductRequest);
      });
    } finally {
      Mockito.verify(this.pdtFeign)
          .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class));
    }
  }

  @Test
  public void removeProductFromPDT_SuccessFalse_BooleanFalseTest() throws Exception {
    ReflectionTestUtils.setField(productDistributionTaskRepositoryBean, "addProductToPdtRetry", false);
    RemoveProductRequest removeProductRequest = new RemoveProductRequest();
    removeProductRequest.setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse baseMapResponseError =
        new GdnRestSingleResponse(StringUtils.EMPTY, ErrorCategory.UNSPECIFIED.name(), Boolean.FALSE, null,
            REQUEST_ID);
    Mockito.when(this.pdtFeign
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class)))
        .thenReturn(baseMapResponseError);
    this.productDistributionTaskRepositoryBean
        .removeProductFromPDT(REQUEST_ID, USER_NAME, removeProductRequest);
    Mockito.verify(this.pdtFeign)
        .removeProductFromPDT(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.any(RemoveProductRequest.class));
  }
  @Test
  public void retryApproveQc_Test() throws Exception {
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(PRODUCT_CODE);
    Mockito
        .when(this.pdtFeign.getPDTDomainModelResponseByCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any()))
        .thenReturn(new GdnRestSingleResponse<>(new PDTProductDomainEventModelResponse(pdtProductDomainEventModel),
            REQUEST_ID));
    PDTProductDomainEventModel response =
        this.productDistributionTaskRepositoryBean.getPDTDomainModelResponseByCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    Mockito.verify(pdtFeign).getPDTDomainModelResponseByCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
        Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
  }

  @Test
  public void checkIfProductExistsInPDTTest() {
    Mockito.when(pdtFeign.checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, true, Constants.DEFAULT_CLIENT_ID, Boolean.TRUE));
    boolean ifProductExistsInPDT = productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(pdtFeign).checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false);
    Assertions.assertTrue(ifProductExistsInPDT);
  }

  @Test
  public void checkIfProductExistsInPDTExceptionFalseTest() {
    Mockito.when(pdtFeign.checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false))
        .thenReturn(
            new GdnRestSimpleResponse<>(StringUtils.EMPTY, null, false, Constants.DEFAULT_CLIENT_ID, null));
    try {
      productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false);
    } catch (IllegalStateException e) {
    } finally {
      Mockito.verify(pdtFeign).checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false);
    }
  }

  @Test
  public void checkIfProductExistsInPDTExceptionTrueTest() {
    Mockito.when(pdtFeign.checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false))
        .thenReturn(
            new GdnRestSimpleResponse<>(StringUtils.EMPTY, null, false, Constants.DEFAULT_CLIENT_ID, Boolean.TRUE));
    try {
      productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false);
    } catch (IllegalStateException e) {
    } finally {
      Mockito.verify(pdtFeign).checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false);
    }
  }

  @Test
  public void checkIfProductExistsInPDTNullTest() {
    try {
      productDistributionTaskRepositoryBean.checkIfProductExistsInPDT(PRODUCT_CODE, false);
    } catch (IllegalStateException e) {
    } finally {
      Mockito.verify(pdtFeign).checkIfProductExistsInPdt(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE, false);
    }
  }

  @Test
  public void retryApproveQc_ExceptionTest() throws Exception {
    Exception exception = null;
    Mockito
        .when(this.pdtFeign.getPDTDomainModelResponseByCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
            Mockito.any()))
        .thenThrow(IllegalStateException.class);
    try {
      this.productDistributionTaskRepositoryBean.getPDTDomainModelResponseByCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(IllegalStateException.class, exception.getClass());
      Mockito.verify(pdtFeign)
          .getPDTDomainModelResponseByCode(Mockito.anyString(),Mockito.anyString(),Mockito.anyString(),Mockito.anyString(), Mockito.any(),
              Mockito.any());
    }
  }

  @Test
  public void getProductImageQcFeedbackTest() {
    Mockito.when(pdtFeign
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedbackResponse);
    productDistributionTaskRepositoryBean.getProductImageQcFeedback(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
  }

  @Test
  public void getProductImageQcFeedbackSuccessFalseTest() {
    productImageQcFeedbackResponse.setSuccess(false);
    Mockito.when(pdtFeign
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedbackResponse);
    ProductImageQcFeedbackResponse response =
        productDistributionTaskRepositoryBean.getProductImageQcFeedback(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
    Assertions.assertNull(response);
  }

  @Test
  public void getProductImageQcFeedbackObejctNullTest() {
    productImageQcFeedbackResponse.setValue(null);
    Mockito.when(pdtFeign
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(productImageQcFeedbackResponse);
    ProductImageQcFeedbackResponse response =
        productDistributionTaskRepositoryBean.getProductImageQcFeedback(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .getProductImageFeedback(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
    Assertions.assertNull(response);
  }

  @Test
  public void sendProductBackToVendorTest() {
    Mockito.when(pdtFeign
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(new GdnBaseRestResponse(true));
    productDistributionTaskRepositoryBean.sendProductBackToVendor(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
  }

  @Test
  public void sendProductBackToVendorSuccessFalseTest() {
    Mockito.when(pdtFeign
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(new GdnBaseRestResponse(false));
        productDistributionTaskRepositoryBean.sendProductBackToVendor(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
  }

  @Test
  public void sendProductBackToVendorObejctNullTest() {
    Mockito.when(pdtFeign
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE))
        .thenReturn(null);
    productDistributionTaskRepositoryBean.sendProductBackToVendor(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pdtFeign)
        .sendProductBackToVendor(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE);
  }

  @Test
  public void sendProductToAutoNeedRevisionTest() {
    Mockito.when(pdtFeign.sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Boolean.FALSE,
        new AutoNeedRevisionRequest())).thenReturn(new GdnBaseRestResponse(true));
    productDistributionTaskRepositoryBean.sendProductToAutoNeedRevision(new AutoNeedRevisionRequest(), Boolean.FALSE);
    Mockito.verify(pdtFeign).sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Boolean.FALSE,
        new AutoNeedRevisionRequest());
  }

  @Test
  public void sendProductToAutoNeedRevisionFalseTest() {
    Mockito.when(pdtFeign.sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Boolean.FALSE,
        new AutoNeedRevisionRequest())).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(IllegalStateException.class, () -> {
        productDistributionTaskRepositoryBean.sendProductToAutoNeedRevision(new AutoNeedRevisionRequest(), Boolean.FALSE);
      });
    } finally {
      Mockito.verify(pdtFeign).sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Boolean.FALSE,
          new AutoNeedRevisionRequest());
    }
  }

  @Test
  public void sendProductToAutoNeedRevisionNullTest() {
    try {
      Assertions.assertThrows(IllegalStateException.class, () -> {
        productDistributionTaskRepositoryBean.sendProductToAutoNeedRevision(new AutoNeedRevisionRequest(), Boolean.FALSE);
      });
    } finally {
      Mockito.verify(pdtFeign).sendProductToAutoNeedRevision(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Boolean.FALSE,
          new AutoNeedRevisionRequest());
    }
  }

  @Test
  public void productRetryStatusUpdateTest() {
    ProductRetryStatusUpdate productRetryStatusUpdate = ProductRetryStatusUpdate.builder().build();
    Mockito.when(pdtFeign.productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE
        ,productRetryStatusUpdate)).thenReturn(new GdnBaseRestResponse(true));
    productDistributionTaskRepositoryBean.productRetryStatusUpdate(PRODUCT_CODE, productRetryStatusUpdate);
    Mockito.verify(pdtFeign).productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE,
        productRetryStatusUpdate);
  }

  @Test
  public void productRetryStatusUpdateResponseNotSuccessTest() {
    ProductRetryStatusUpdate productRetryStatusUpdate = ProductRetryStatusUpdate.builder().build();
    Mockito.when(pdtFeign.productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE
        ,productRetryStatusUpdate)).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(IllegalStateException.class, () -> {
        productDistributionTaskRepositoryBean.productRetryStatusUpdate(PRODUCT_CODE, productRetryStatusUpdate);
      });
    } finally {
      Mockito.verify(pdtFeign).productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE,
          productRetryStatusUpdate);
    }
  }

  @Test
  public void productRetryStatusUpdateNullResponseTest() {
    ProductRetryStatusUpdate productRetryStatusUpdate = ProductRetryStatusUpdate.builder().build();
    try {
      Assertions.assertThrows(IllegalStateException.class, () -> {
        productDistributionTaskRepositoryBean.productRetryStatusUpdate(PRODUCT_CODE, productRetryStatusUpdate);
      });
    } finally {
      Mockito.verify(pdtFeign).productRetryStatusUpdate(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, PRODUCT_CODE,
          productRetryStatusUpdate);
    }
  }

  @Test
  public void updateBrandTest() throws Exception {
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();
    Mockito.when(pdtFeign.updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    productDistributionTaskRepositoryBean.updateProductBrand(changeBrandRequest);
    Mockito.verify(pdtFeign)
        .updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest);
  }

  @Test
  public void updateBrandFalseTest() throws Exception {
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();
    Mockito.when(pdtFeign.updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productDistributionTaskRepositoryBean.updateProductBrand(changeBrandRequest);
      });
    } finally {
      Mockito.verify(pdtFeign)
          .updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest);
    }
  }

  @Test
  public void updateBrandNullTest() throws Exception {
    ChangeBrandRequest changeBrandRequest = new ChangeBrandRequest();

    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productDistributionTaskRepositoryBean.updateProductBrand(changeBrandRequest);
      });
    } finally {
      Mockito.verify(pdtFeign)
          .updateProductBrand(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, changeBrandRequest);
    }
  }

  @Test
  public void updateAppealProductTest() throws Exception {
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    GdnRestSingleResponse<AppealProductResponse> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    Mockito.when(
      pdtFeign.updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
        appealProductRequest)).thenReturn(response);
    productDistributionTaskRepositoryBean.updateAppealProduct(appealProductRequest);
    Mockito.verify(pdtFeign)
      .updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
        appealProductRequest);
  }

  @Test
  public void updateAppealProductFalseTest() throws Exception {
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    Mockito.when(
      pdtFeign.updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
        appealProductRequest)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productDistributionTaskRepositoryBean.updateAppealProduct(appealProductRequest);
      });
    } finally {
      Mockito.verify(pdtFeign)
        .updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
          appealProductRequest);
    }
  }

  @Test
  public void updateAppealProductNullTest() throws Exception {
    AppealProductRequest appealProductRequest = new AppealProductRequest();
    try {
      GdnRestSingleResponse<AppealProductResponse> response = new GdnRestSingleResponse<>();
      Mockito.when(
        pdtFeign.updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
          appealProductRequest)).thenReturn(response);
      Assertions.assertThrows(ApplicationException.class, () -> {
        productDistributionTaskRepositoryBean.updateAppealProduct(appealProductRequest);
      });
    } finally {
      Mockito.verify(pdtFeign)
        .updateAppealProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CLIENT_ID,
          appealProductRequest);
    }
  }
}
