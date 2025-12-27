package com.gdn.mta.bulk.repository;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.BusinessPartnerCodeResponseList;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;


public class ProductRepositoryBeanTest {

  public static final String REQUEST_ID = "request_id";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_CHANNEL_ID = "channelId";
  private static final String DEFAULT_CLIENT_ID = "clientId";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  public static final String STORE_ID = "10001";
  public static final String ERROR_CODE = "ERROR_CODE";
  public static final String ERROR_MESSAGE = "An error occurred";


  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private ProductRepositoryBean productRepositoryBean;

  ProductRequest productRequest;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pbpFeign);
  }

  private ProductResponse getProductResponse() throws Exception {
    Date date = Calendar.getInstance().getTime();
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    productResponse.setStoreId(DEFAULT_STORE_ID);
    productResponse.setCreatedDate(date);
    productResponse.setCreatedBy(DEFAULT_USERNAME);
    productResponse.setUpdatedDate(date);
    productResponse.setUpdatedBy(DEFAULT_USERNAME);
    return productResponse;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    productRequest = new ProductRequest();

    GdnRestSimpleResponse<String> createResponse =
        new GdnRestSimpleResponse<String>(ProductRepositoryBeanTest.DEFAULT_REQUEST_ID,
            UUID.randomUUID().toString());


  }
  
  @Test
  public void updateMasterProducts_HappyFlow_Success() throws Exception {
    Mockito.when(pbpFeign.updateActivatedProductsBulkMasterData(any(),any(),any(),any(),
        any(), any()))
          .thenReturn(new GdnRestSingleResponse<BulkMasterProductUpdateResponse>(null, DEFAULT_REQUEST_ID));
    productRepositoryBean.updateMasterProducts(new BulkMasterProductUpdateRequest());
    Mockito.verify(pbpFeign).updateActivatedProductsBulkMasterData(any(),
        any(), any(),any(),any(),any());
  }

  @Test
  public void updateMasterProducts_ReturnFalse_Success() throws Exception {
    MDC.put("username", DEFAULT_USERNAME);
    Mockito.when(pbpFeign.updateActivatedProductsBulkMasterData(Mockito.any(),Mockito.anyString(),
            Mockito.anyString(),Mockito.anyString(),
        Mockito.anyString(), Mockito.any()))
          .thenReturn(new GdnRestSingleResponse<BulkMasterProductUpdateResponse>(
              null, "DATA_NOT_FOUND", false, null, DEFAULT_REQUEST_ID));
    try{
      Assertions.assertThrows(ApplicationException.class,
          () -> productRepositoryBean.updateMasterProducts(new BulkMasterProductUpdateRequest()));
    } catch(Exception e) { } finally {
      Mockito.verify(pbpFeign).updateActivatedProductsBulkMasterData(Mockito.anyString(),Mockito.anyString(),
          Mockito.anyString(),Mockito.anyString(),
          Mockito.anyString(), Mockito.any());
    }
  }
  
  @Test
  public void generateProductCode_HappyFlow_Success() throws Exception {
    Mockito.when(
            pbpFeign.generateProductCode(Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any()))
      .thenReturn(new GdnRestSimpleResponse<String>(DEFAULT_REQUEST_ID, null));
    productRepositoryBean.generateProductCode(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(pbpFeign).generateProductCode(Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any(),Mockito.any());
  }
  
  @Test
  public void createProduct_HappyFlow_Success() throws Exception {
    Mockito.when(pbpFeign
        .createNewProduct(eq(DEFAULT_STORE_ID), eq(DEFAULT_CHANNEL_ID), eq(DEFAULT_CLIENT_ID),
            eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    pbpFeign
        .createNewProduct(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
            null);
    Mockito.verify(pbpFeign)
        .createNewProduct(eq(DEFAULT_STORE_ID), eq(DEFAULT_CHANNEL_ID), eq(DEFAULT_CLIENT_ID),
            eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME), Mockito.any());
  }


  @Test
  public void testFindProductDetailByProductCodeAndMarkForDeleteFalse() throws Exception {
    ProductResponse productResponse = getProductResponse();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(productResponse, productDetailResponse);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    Mockito.when(pbpFeign.filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        false, DEFAULT_PRODUCT_CODE)).thenReturn(response);
    productRepositoryBean.findProductDetailByProductCodeAndMarkForDeleteFalse(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            false, DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void testFindProductDetailByProductCodeAndMarkForDeleteFalseWithException() throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<ProductDetailResponse>("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            null, DEFAULT_REQUEST_ID);
    Mockito.when(pbpFeign.filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        false, DEFAULT_PRODUCT_CODE)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productRepositoryBean.findProductDetailByProductCodeAndMarkForDeleteFalse(
              DEFAULT_PRODUCT_CODE));
    } catch (Exception e) {} finally {
        Mockito.verify(pbpFeign, AT_LEAST_ONE)
            .filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
                GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
                false, DEFAULT_PRODUCT_CODE);
    }
  }

  @Test
  public void testFindProductDetailByProductCodeAndMarkForDeleteFalseWithNotNullUsername() throws Exception {
    ProductResponse productResponse = getProductResponse();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(productResponse, productDetailResponse);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pbpFeign.filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        false, DEFAULT_PRODUCT_CODE)).thenReturn(response);
    productRepositoryBean.findProductDetailByProductCodeAndMarkForDeleteFalse(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .filterProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            false, DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void testUpdate() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    Mockito
        .when(pbpFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productRequest))
        .thenReturn(response);
    productRepositoryBean.update(productRequest);
    Mockito.verify(pbpFeign, AT_LEAST_ONE).updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productRequest);
  }

  @Test
  public void testUpdateWithException() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    GdnBaseRestResponse response =
        new GdnBaseRestResponse("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false, DEFAULT_REQUEST_ID);
    Mockito
        .when(pbpFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productRequest))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productRepositoryBean.update(productRequest));
    } finally {
      Mockito.verify(pbpFeign).updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), productRequest);
    }
  }

  @Test
  public void testUpdateWithNotNullUsername() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pbpFeign.updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productRequest)).thenReturn(response);
    productRepositoryBean.update(productRequest);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .updateProduct(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productRequest);
  }

  @Test
  public void testGenerateBarcode() throws Exception {
    GdnRestSimpleResponse<String> response =
        new GdnRestSimpleResponse<String>(null, null, true, DEFAULT_REQUEST_ID, DEFAULT_PRODUCT_CODE);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    Mockito.when(pbpFeign.generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername()))
        .thenReturn(response);
    productRepositoryBean.generateBarcode(DEFAULT_REQUEST_ID);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername());
  }

  @Test
  public void testGenerateBarcodeWithException() throws Exception {
    GdnRestSimpleResponse<String> response =
        new GdnRestSimpleResponse<String>("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            DEFAULT_REQUEST_ID, DEFAULT_PRODUCT_CODE);
    Mockito.when(pbpFeign.generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername()))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productRepositoryBean.generateBarcode(DEFAULT_REQUEST_ID));
    } catch (Exception e) { } finally {
        Mockito.verify(pbpFeign, AT_LEAST_ONE).generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername());
      }
    }

  @Test
  public void testGenerateBarcodeNotNullUserName() throws Exception {
    GdnRestSimpleResponse<String> response =
        new GdnRestSimpleResponse<String>(null, null, true, DEFAULT_REQUEST_ID, DEFAULT_PRODUCT_CODE);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pbpFeign.generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername()))
        .thenReturn(response);
    productRepositoryBean.generateBarcode(DEFAULT_REQUEST_ID);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .generateBarcode(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername());
  }

  @Test
  public void createTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, ProductRepositoryBeanTest.DEFAULT_USERNAME);
    CreateProductRequest request = new CreateProductRequest();
    GdnRestSimpleResponse<String> response = new GdnRestSimpleResponse<>();
    response.setSuccess(true);
    Mockito.when(this.pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        request)).thenReturn(response);
    this.productRepositoryBean.create(request);
    Mockito.verify(this.pbpFeign)
        .create(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, request);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void createWithExceptionTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    GdnRestSimpleResponse<String> createResponseError =
        new GdnRestSimpleResponse<String>("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID, null);
    CreateProductRequest request = new CreateProductRequest();
    Mockito.when(this.pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        request)).thenReturn(createResponseError);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.productRepositoryBean.create(request));
    } catch (Exception e) {} finally {
        Mockito.verify(this.pbpFeign)
            .create(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
                GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), false, request);
    }
  }
  
  @Test
  public void createViaApiTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        ProductRepositoryBeanTest.DEFAULT_USERNAME);
    GdnRestSimpleResponse<String> response = 
        new GdnRestSimpleResponse<String>(null, null, true, DEFAULT_REQUEST_ID, "response");
    CreateProductRequest request = new CreateProductRequest();
    
    Mockito.when(pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
            request))
          .thenReturn(response);
    
    this.productRepositoryBean.createViaApi(request);
    
    Mockito.verify(this.pbpFeign).create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        request);
  }

  @Test
  public void createViaApiTestWithNullUsername() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        ProductRepositoryBeanTest.DEFAULT_USERNAME);
    GdnRestSimpleResponse<String> response = 
        new GdnRestSimpleResponse<String>(null, null, true, DEFAULT_REQUEST_ID, "response");
    CreateProductRequest request = new CreateProductRequest();
    
    Mockito.when(pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
            request))
          .thenReturn(response);

    this.productRepositoryBean.createViaApi(request);
    
    Mockito.verify(this.pbpFeign).create(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
        request);
  }
  
  @Test
  public void createViaApiTestWhenFalseReponse() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        ProductRepositoryBeanTest.DEFAULT_USERNAME);
    GdnRestSimpleResponse<String> response =
        new GdnRestSimpleResponse<String>(null, null, false, DEFAULT_REQUEST_ID, "response");
    CreateProductRequest request = new CreateProductRequest();

    Mockito.when(pbpFeign.create(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
            request))
          .thenReturn(response);
    try{
      Assertions.assertThrows(RuntimeException.class,
          () -> this.productRepositoryBean.createViaApi(request));
    } catch(Exception e){ } finally {
      Mockito.verify(this.pbpFeign).create(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), true,
          request);
    }
  }

  @Test
  public void createProductTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(DEFAULT_STORE_ID);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, DEFAULT_REQUEST_ID);
    when(pbpFeign.createNewProduct(anyString(), anyString(), anyString(), anyString(), anyString(),
        any(ProductCreationRequest.class))).thenReturn(response);
    GdnBaseRestResponse response1 =
        productRepositoryBean.createProduct(DEFAULT_REQUEST_ID, DEFAULT_USERNAME, productCreationRequest);
    Mockito.verify(pbpFeign).createNewProduct(anyString(), anyString(), anyString(), anyString(), anyString(),
        any(ProductCreationRequest.class));
  }

  @Test
  public void createProductExceptionTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(DEFAULT_STORE_ID);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, DEFAULT_REQUEST_ID);
    when(pbpFeign.createNewProduct(anyString(), anyString(), anyString(), anyString(), anyString(),
        any(ProductCreationRequest.class))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productRepositoryBean.createProduct(DEFAULT_REQUEST_ID, DEFAULT_USERNAME,
              productCreationRequest));
    } finally {
      Mockito.verify(pbpFeign).createNewProduct(anyString(), anyString(), anyString(), anyString(), anyString(),
          any(ProductCreationRequest.class));
    }
  }

  @Test
  public void fetchNRBusinessPartnerCodes_success() {
    List<String> businessPartnerCodes = List.of("BP-001", "BP-002");
    ReflectionTestUtils.setField(productRepositoryBean, "fetchBatchSizeForNRBPCodes", 10);
    BusinessPartnerCodeResponseList responseList = new BusinessPartnerCodeResponseList();
    responseList.setBusinessPartCodeList(businessPartnerCodes);
    GdnRestListResponse<BusinessPartnerCodeResponseList> mockResponse = new GdnRestListResponse<>();
    mockResponse.setContent(Collections.singletonList(responseList));
    mockResponse.setSuccess(true);
    when(pbpFeign.getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
        Mockito.anyInt(), any())).thenReturn(mockResponse);
    List<String> result = productRepositoryBean.fetchNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    verify(pbpFeign).getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
        Mockito.anyInt(), any());
  }

  @Test
  public void fetchNRBusinessPartnerCodes_failure() {
    List<String> result = new ArrayList<>();
    try {
      ReflectionTestUtils.setField(productRepositoryBean, "fetchBatchSizeForNRBPCodes", 10);
      GdnRestListResponse<BusinessPartnerCodeResponseList> mockResponse = new GdnRestListResponse<>();
      mockResponse.setContent(null);
      mockResponse.setSuccess(false);
      when(pbpFeign.getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
          Mockito.anyInt(), any())).thenReturn(mockResponse);
       result = productRepositoryBean.fetchNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    }
    catch(ApplicationRuntimeException e) {
      Assertions.assertEquals("Can not communicate :", e.getErrorCodes().getMessage());
    }
    finally {
      Assertions.assertTrue(result.isEmpty());
      verify(pbpFeign).getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
          Mockito.anyInt(), any());
    }
  }

  @Test
  public void fetchNRBusinessPartnerCodes_SuccessTrueButContentIsEmptyListValue() {
    ReflectionTestUtils.setField(productRepositoryBean, "fetchBatchSizeForNRBPCodes", 10);
    GdnRestListResponse<BusinessPartnerCodeResponseList> mockResponse = new GdnRestListResponse<>();
    BusinessPartnerCodeResponseList responseList = new BusinessPartnerCodeResponseList();
    responseList.setBusinessPartCodeList(new ArrayList<>());
    mockResponse.setContent(Collections.singletonList(responseList));
    mockResponse.setSuccess(true);
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(100);
    mockResponse.setPageMetaData(pageMetaData);
    when(pbpFeign.getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
        Mockito.anyInt(), any())).thenReturn(mockResponse);
    List<String> result = productRepositoryBean.fetchNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    Assertions.assertTrue(result.isEmpty());
    verify(pbpFeign,times(10)).getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
        Mockito.anyInt(), any());
  }

  @Test
  public void fetchNRBusinessPartnerCodes_nullValue() {
    List<String> result = new ArrayList<>();
    try {
      ReflectionTestUtils.setField(productRepositoryBean, "fetchBatchSizeForNRBPCodes", 10);
      GdnRestListResponse<BusinessPartnerCodeResponseList> mockResponse = new GdnRestListResponse<>();
      mockResponse.setContent(new ArrayList<>());
      mockResponse.setSuccess(true);
      when(pbpFeign.getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
          Mockito.anyInt(), any())).thenReturn(mockResponse);
      result = productRepositoryBean.fetchNRBusinessPartnerCodes(STORE_ID, REQUEST_ID);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals("Can not communicate :", e.getErrorCodes().getMessage());
    } finally {
      Assertions.assertTrue(result.isEmpty());
      verify(pbpFeign).getDistinctBusinessPartnerCodeForRevisedProducts(eq(STORE_ID), eq(REQUEST_ID), Mockito.anyInt(),
          Mockito.anyInt(), any());
    }
  }

  @Test
  public void getEligibilityForNeedRevisionDeletion_success() throws ApplicationException {
    List<NeedRevisionEligibilityRequest> requestList = List.of(new NeedRevisionEligibilityRequest());
    List<NeedRevisionEligibilityResponse> responseList = List.of(new NeedRevisionEligibilityResponse());

    GdnRestListResponse<NeedRevisionEligibilityResponse> restListResponse = new GdnRestListResponse<>();
    restListResponse.setContent(responseList);
    restListResponse.setSuccess(true);

    when(pbpFeign.eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(), any(), eq(requestList)))
      .thenReturn(restListResponse);

    List<NeedRevisionEligibilityResponse> result = productRepositoryBean.getEligibilityForNeedRevisionDeletion(STORE_ID, requestList);

    Assertions.assertEquals(responseList, result);
    verify(pbpFeign).eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(), any(), eq(requestList));
  }


  @Test
  public void getEligibilityForNeedRevisionDeletion_emptyRequestList() throws ApplicationException {
    List<NeedRevisionEligibilityRequest> emptyRequestList = Collections.EMPTY_LIST;

    GdnRestListResponse<NeedRevisionEligibilityResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setContent(Collections.emptyList());
    mockResponse.setSuccess(true);

    when(pbpFeign.eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(), any(), eq(emptyRequestList)))
      .thenReturn(mockResponse);

    List<NeedRevisionEligibilityResponse> result = productRepositoryBean.getEligibilityForNeedRevisionDeletion(STORE_ID, emptyRequestList);

    Assertions.assertTrue(result.isEmpty());
    verify(pbpFeign).eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(), any(), eq(emptyRequestList));
  }

  @Test
  public void getEligibilityForNeedRevisionDeletion_failure() {
    List<NeedRevisionEligibilityRequest> requestList =
      List.of(new NeedRevisionEligibilityRequest());

    GdnRestListResponse<NeedRevisionEligibilityResponse> mockResponse = new GdnRestListResponse<>();
    mockResponse.setSuccess(false);
    mockResponse.setErrorMessage(ERROR_MESSAGE);
    when(
      pbpFeign.eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(), any(),
        eq(requestList))).thenReturn(mockResponse);

    ApplicationException exception = Assertions.assertThrows(ApplicationException.class, () -> {
      productRepositoryBean.getEligibilityForNeedRevisionDeletion(STORE_ID, requestList);
    });
    verify(pbpFeign).eligibilityForNeedRevisionDeletion(eq(STORE_ID), any(), any(), any(),
      any(), eq(requestList));
  }

  @Test
  public void deleteProductCollection_success() {
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();  // Replace with actual request setup if needed
    GdnBaseRestResponse mockResponse = new GdnBaseRestResponse();
    mockResponse.setSuccess(true);
    when(pbpFeign.deleteProductCollection(eq(STORE_ID), any(), any(), any(), any(), eq(true), eq(deleteProductRequest)))
      .thenReturn(mockResponse);
    boolean result = productRepositoryBean.deleteProductCollection(STORE_ID, deleteProductRequest, true);
    Assertions.assertTrue(result);
    verify(pbpFeign).deleteProductCollection(eq(STORE_ID), any(), any(), any(), any(), eq(true), eq(deleteProductRequest));
  }

  @Test
  public void deleteProductCollection_failure() {
    DeleteProductRequest deleteProductRequest = new DeleteProductRequest();
    GdnBaseRestResponse gdnBaseRestResponse = new GdnBaseRestResponse();
    gdnBaseRestResponse.setSuccess(false);

    when(pbpFeign.deleteProductCollection(eq(STORE_ID), any(), any(), any(), any(), anyBoolean(),
      eq(deleteProductRequest)))
      .thenReturn(gdnBaseRestResponse);
    boolean result = productRepositoryBean.deleteProductCollection(STORE_ID, deleteProductRequest, false);
    Assertions.assertFalse(result);
    verify(pbpFeign).deleteProductCollection(eq(STORE_ID), any(), any(), any(), any(), eq(false), eq(deleteProductRequest));
  }
}
