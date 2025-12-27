package com.gdn.mta.bulk.repository;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import com.gdn.partners.pbp.dto.common.ListRequestDTO;
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
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.models.AuditTrailInfo;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.ListRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;

public class ProductBusinessPartnerRepositoryBeanTest {

  private static final String STORE_ID = "10001";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "developer";
  private static final String ID = "123";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CLIENT_HOST = "localhost";
  private static final String USER_NAME = "userName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String ERROR_MESSAGE = "error";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private ProductBusinessPartnerRepositoryBean productBusinessPartnerRepositoryBean;

  private GdnBaseRestResponse response;
  private ProductBusinessPartnerRequest productBusinessPartnerRequest;
  private DeleteOfflineItemResponse deleteOfflineItemResponse;
  private UpsertOfflineItemResponse upsertOfflineItemResponse;
  private AuditTrailInfo auditTrailInfo;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private ListRequestDTO<UpsertOfflineItemRequest> upsertOfflineItemRequestsDTO;
  private GdnRestSingleResponse<ItemBulkArchiveResponse> gdnRestSingleResponse;
  private GdnRestSingleResponse<ProductLevel3Response> gdnRestSingleResponse1;
  private GdnRestSingleResponse<BulkDownloadProductLevel3Response> bulkDownloadSummaryResponse1;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pbpFeign);
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);

    productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    
    upsertOfflineItemResponse = new UpsertOfflineItemResponse();

    deleteOfflineItemResponse = new DeleteOfflineItemResponse();
    
    auditTrailInfo = new AuditTrailInfo();
    auditTrailInfo.setRequestId(DEFAULT_REQUEST_ID);
    auditTrailInfo.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    auditTrailInfo.setRemoteAddress(CLIENT_HOST);
    auditTrailInfo.setUsername(DEFAULT_USERNAME);
    
    upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    upsertOfflineItemRequests = Collections.singletonList(upsertOfflineItemRequest);

    upsertOfflineItemRequestsDTO = new ListRequestDTO<>(upsertOfflineItemRequests);

    deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    deleteOfflineItemRequests = Collections.singletonList(deleteOfflineItemRequest);

    gdnRestSingleResponse = new GdnRestSingleResponse<>(null, null, true, null, Constant.REQUEST_ID);

    gdnRestSingleResponse1 = new GdnRestSingleResponse<>(null, null, true, new ProductLevel3Response(), Constant.REQUEST_ID);
  }
  
  @Test
  public void testSaveWithActivatedFalseReturnId() throws Exception {
    GdnRestSimpleResponse<String> response = new GdnRestSimpleResponse(DEFAULT_REQUEST_ID, "");

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    Mockito.when(pbpFeign.saveProductBusinessPartnerWithActivatedFalseReturnId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(productBusinessPartnerRequest))).thenReturn(response);
    productBusinessPartnerRepositoryBean.saveWithActivatedFalseReturnId(
        productBusinessPartnerRequest);
    Mockito.verify(pbpFeign, AT_LEAST_ONE)
        .saveProductBusinessPartnerWithActivatedFalseReturnId(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.eq(productBusinessPartnerRequest));
  }

  @Test
  public void testSaveWithActivatedFalseReturnIdWithException() throws Exception {
    GdnRestSimpleResponse<String> response =
        new GdnRestSimpleResponse("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            DEFAULT_REQUEST_ID, "");
    Mockito.when(pbpFeign.saveProductBusinessPartnerWithActivatedFalseReturnId(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.eq(productBusinessPartnerRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.saveWithActivatedFalseReturnId(
              productBusinessPartnerRequest));
    } finally {
      Mockito.verify(pbpFeign)
          .saveProductBusinessPartnerWithActivatedFalseReturnId(Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),Mockito.anyString(),
              Mockito.eq(productBusinessPartnerRequest));
    }
  }
  
  @Test
  public void testGetProductBusinessPartnerById() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    ProductBusinessPartnerResponse response = new ProductBusinessPartnerResponse();
    when(
        pbpFeign.getProductBusinessPartner( Mockito.anyString(),  Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(ID))).thenReturn(
        new GdnRestSingleResponse<ProductBusinessPartnerResponse>(response, DEFAULT_REQUEST_ID));
    productBusinessPartnerRepositoryBean.getProductBusinessPartnerById(ID);
    verify(pbpFeign).getProductBusinessPartner( Mockito.anyString(),  Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(ID));
  }
  
  @Test
  public void testGetProductBusinessPartnerByIdNullMandatory() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    ProductBusinessPartnerResponse response = new ProductBusinessPartnerResponse();
    when(
        pbpFeign.getProductBusinessPartner(Mockito.any(), Mockito.any(),
            Mockito.any(),Mockito.any(),
            Mockito.any(), Mockito.any())).thenReturn(
        new GdnRestSingleResponse<ProductBusinessPartnerResponse>(response, DEFAULT_REQUEST_ID));
    productBusinessPartnerRepositoryBean.getProductBusinessPartnerById(ID);
    verify(pbpFeign).getProductBusinessPartner(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void testGetProductBusinessPartnerByIdResponseFalseMandatory() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    when(pbpFeign.getProductBusinessPartner(anyString(), anyString(), anyString(), anyString(),
        anyString(), anyString())).thenReturn(
        new GdnRestSingleResponse<ProductBusinessPartnerResponse>(ErrorCategory.UNSPECIFIED
            .toString(), ErrorCategory.UNSPECIFIED.getCode(), false, null, DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.getProductBusinessPartnerById(ID));
    } catch (Exception e) {} finally {
      verify(pbpFeign).getProductBusinessPartner(anyString(), anyString(), anyString(),Mockito.anyString(),
          Mockito.anyString(), Mockito.anyString());
    }
  }

  @Test
  public void testUpsertOfflineItems() throws Exception {
    upsertOfflineItemResponse.setFailedProducts(new ArrayList<>());

    when(pbpFeign.upsertOfflineItems(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), Mockito.anyString(),
        eq(DEFAULT_USERNAME), eq(BUSINESS_PARTNER_CODE), Mockito.any()))
        .thenReturn(new GdnRestSingleResponse<>(upsertOfflineItemResponse, DEFAULT_REQUEST_ID));

    List<UpsertOfflineItemFailedResponse> responses =
        productBusinessPartnerRepositoryBean.upsertOfflineItems(upsertOfflineItemRequests,
            auditTrailInfo);

    Assertions.assertEquals(0, responses.size());

    verify(pbpFeign).upsertOfflineItems(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), Mockito.anyString(), eq(DEFAULT_USERNAME),
        eq(BUSINESS_PARTNER_CODE), Mockito.any());
  }

  @Test
  public void testUpsertOfflineItems_falseResponse_throwsApplicationException() throws Exception {
    when(pbpFeign.upsertOfflineItems(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), Mockito.anyString(),
        eq(DEFAULT_USERNAME), eq(BUSINESS_PARTNER_CODE), Mockito.any()))
            .thenReturn(new GdnRestSingleResponse<>(null, null, false, upsertOfflineItemResponse,
                DEFAULT_REQUEST_ID));

    try {
      productBusinessPartnerRepositoryBean.upsertOfflineItems(upsertOfflineItemRequests,
          auditTrailInfo);
    } catch (Exception e) {
      Assertions.assertNotNull(e.getMessage());
      verify(pbpFeign).upsertOfflineItems(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
          eq(Constant.CLIENT_ID), Mockito.anyString(), eq(DEFAULT_USERNAME),
          eq(BUSINESS_PARTNER_CODE), Mockito.any());
    }
  }

  @Test
  public void testDeleteOfflineItems() throws Exception {
    deleteOfflineItemResponse.setFailedProducts(new ArrayList<>());

    when(pbpFeign.bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
        Mockito.eq(Constant.CLIENT_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.any(ListRequest.class)))
            .thenReturn(new GdnRestSingleResponse<>(deleteOfflineItemResponse, DEFAULT_REQUEST_ID));

    List<DeleteOfflineItemDetailResponse> responses = productBusinessPartnerRepositoryBean
        .deleteOfflineItems(STORE_ID, deleteOfflineItemRequests, auditTrailInfo);
    Assertions.assertEquals(0, responses.size());

    verify(pbpFeign).bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
        Mockito.eq(Constant.CLIENT_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.any(ListRequest.class));
  }

  @Test
  public void testDeleteOfflineItems_falseResponse_throwsApplicationException() throws Exception {
    when(pbpFeign.bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
        Mockito.eq(Constant.CLIENT_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
        Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.any(ListRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, deleteOfflineItemResponse,
            DEFAULT_REQUEST_ID));
    try {
      productBusinessPartnerRepositoryBean.deleteOfflineItems(
          STORE_ID, deleteOfflineItemRequests, auditTrailInfo);
    } catch (Exception e) {
      Assertions.assertNotNull(e.getMessage());
      verify(pbpFeign).bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
          Mockito.eq(Constant.CLIENT_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
          Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.any(ListRequest.class));
    }
  }

  @Test
  public void bulkArchiveProductSkusResponseNullTest() throws ApplicationException {
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.bulkArchiveProductSkus(DEFAULT_USERNAME,
              DEFAULT_REQUEST_ID, true,
              new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)),
              new HashMap<>()));
    } finally {
      verify(pbpFeign)
          .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
              DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    }
  }

  @Test
  public void bulkArchiveProductSkusSuccessFalseTest() throws ApplicationException {
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.bulkArchiveProductSkus(DEFAULT_USERNAME,
              DEFAULT_REQUEST_ID, true,
              new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)),
              new HashMap<>()));
    } finally {
      verify(pbpFeign)
          .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
              DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    }
  }

  @Test
  public void bulkArchiveProductSkusEmptyListTest() throws ApplicationException {
    gdnRestSingleResponse.setSuccess(true);
    ItemBulkArchiveResponse itemBulkArchiveResponse = new ItemBulkArchiveResponse();
    itemBulkArchiveResponse.setFailedItemSkus(new ArrayList<>());
    gdnRestSingleResponse.setValue(itemBulkArchiveResponse);
    Mockito.when(pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(gdnRestSingleResponse);
    List<String> stringList = productBusinessPartnerRepositoryBean
        .bulkArchiveProductSkus(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, true,
            new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)), new HashMap<>());
    verify(pbpFeign)
          .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
              DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    Assertions.assertTrue(CollectionUtils.isEmpty(stringList));
  }

  @Test
  public void bulkArchiveProductSkusValueEmptyTest() throws ApplicationException {
    gdnRestSingleResponse.setSuccess(true);
    gdnRestSingleResponse.setValue(null);
    Mockito.when(pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(gdnRestSingleResponse);
    List<String> stringList = productBusinessPartnerRepositoryBean
        .bulkArchiveProductSkus(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, true,
            new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)), new HashMap<>());
    verify(pbpFeign)
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    Assertions.assertTrue(CollectionUtils.isEmpty(stringList));
  }

  @Test
  public void bulkArchiveProductTest() throws ApplicationException {
    gdnRestSingleResponse.setSuccess(true);
    ItemBulkArchiveResponse itemBulkArchiveResponse = new ItemBulkArchiveResponse();
    itemBulkArchiveResponse.setFailedItemSkus(Collections.singletonList(PRODUCT_SKU));
    gdnRestSingleResponse.setValue(itemBulkArchiveResponse);
    Mockito.when(pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))
        .thenReturn(gdnRestSingleResponse);
    List<String> stringList = productBusinessPartnerRepositoryBean
        .bulkArchiveProductSkus(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, true,
            new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)), new HashMap<>());
    verify(pbpFeign)
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    Assertions.assertTrue(stringList.contains(PRODUCT_SKU));
  }


  @Test
  public void updateAndReturnTest() throws ApplicationException {
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    Mockito.when(pbpFeign
        .updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request)).thenReturn(gdnRestSingleResponse1);
    GdnRestSingleResponse<ProductLevel3Response> response = productBusinessPartnerRepositoryBean
        .updateAndReturn(productLevel3Request, true, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CLIENT_HOST);
    verify(pbpFeign).updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
        DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request);
  }

  @Test
  public void bulkDownloadSummaryTest() throws ApplicationException {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
    Mockito.when(pbpFeign.bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID,
      Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null)).thenReturn(
        new GdnRestSingleResponse<BulkDownloadProductLevel3Response>(null, null, true,bulkDownloadProductLevel3Response, Constant.REQUEST_ID));
    BulkDownloadProductLevel3Response bulkDownloadSummary =
        productBusinessPartnerRepositoryBean.bulkDownloadSummary(request, BUSINESS_PARTNER_CODE,
            null);
    Mockito.verify(pbpFeign)
        .bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null);
  }

  @Test
  public void bulkDownloadSummaryNullResponseExceptionTest() throws ApplicationException {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    bulkDownloadSummaryResponse1 = null;
    Mockito.when(pbpFeign.bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null)).thenReturn(bulkDownloadSummaryResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.bulkDownloadSummary(request,
              BUSINESS_PARTNER_CODE, null));
    }  finally {
      verify(pbpFeign).bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null);
    }
  }

  @Test
  public void bulkDownloadSummaryNullSuccessFalseTest() throws ApplicationException {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    bulkDownloadSummaryResponse1 =
        new GdnRestSingleResponse<BulkDownloadProductLevel3Response>("Error", "Error", false, null, DEFAULT_REQUEST_ID);
    Mockito.when(pbpFeign.bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null))
        .thenReturn(bulkDownloadSummaryResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.bulkDownloadSummary(request,
              BUSINESS_PARTNER_CODE, null));
    }  finally {
      verify(pbpFeign).bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null);
    }
  }

  @Test
  public void updateAndReturnNullResponseExceptionTest() throws ApplicationException {
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    gdnRestSingleResponse1 = null;
    Mockito.when(pbpFeign
        .updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request)).thenReturn(gdnRestSingleResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.updateAndReturn(productLevel3Request, true,
              DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CLIENT_HOST));
    } catch (Exception e) {
      throw e;
    } finally {
      verify(pbpFeign).updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
          DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request);
    }
  }

  @Test
  public void updateAndReturnNullSuccessFalseTest() throws ApplicationException {
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    gdnRestSingleResponse1 = new GdnRestSingleResponse<ProductLevel3Response>("Error", "Error", false, null, DEFAULT_REQUEST_ID);
    Mockito.when(pbpFeign
        .updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request)).thenReturn(gdnRestSingleResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.updateAndReturn(productLevel3Request, true,
              DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CLIENT_HOST));
    } catch (Exception e) {
      throw e;
    } finally {
      verify(pbpFeign).updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
          DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request);
    }
  }

  @Test
  public void updateAndReturnValueNullTest() throws ApplicationException {
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    gdnRestSingleResponse1 = new GdnRestSingleResponse<ProductLevel3Response>("Error", "Error", true, null, DEFAULT_REQUEST_ID);
    Mockito.when(pbpFeign
        .updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request)).thenReturn(gdnRestSingleResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.updateAndReturn(productLevel3Request, true,
              DEFAULT_REQUEST_ID, DEFAULT_USERNAME, CLIENT_HOST));
    } catch (Exception e) {
      throw e;
    } finally {
      verify(pbpFeign).updateAndReturn(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
          DEFAULT_USERNAME, CLIENT_HOST, true, true, productLevel3Request);
    }
  }

  @Test
  public void testDeleteOfflineItemsNullResponse() throws Exception {
    when(pbpFeign
        .bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID), Mockito.eq(Constant.CLIENT_ID),
            Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME), Mockito.eq(BUSINESS_PARTNER_CODE),
            Mockito.any(ListRequest.class))).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productBusinessPartnerRepositoryBean.deleteOfflineItems(STORE_ID,
              deleteOfflineItemRequests, auditTrailInfo));
    } finally {
      verify(pbpFeign).bulkDeleteOfflineItem(Mockito.eq(STORE_ID), Mockito.eq(Constant.CHANNEL_ID),
          Mockito.eq(Constant.CLIENT_ID), Mockito.eq(DEFAULT_REQUEST_ID), Mockito.eq(DEFAULT_USERNAME),
          Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.any(ListRequest.class));
    }
  }

  @Test
  public void updateProductCategoryTest() {
    Mockito.when(pbpFeign
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.VALIDATION.getCode(), false, null));
    String errorMessage = productBusinessPartnerRepositoryBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, USER_NAME);
    Mockito.verify(pbpFeign)
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(ERROR_MESSAGE, errorMessage);
  }

  @Test
  public void updateProductCategorySystemErrorTest() {
    Mockito.when(pbpFeign
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.DATA_ACCESS.getCode(), false, null));
    String errorMessage = productBusinessPartnerRepositoryBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, USER_NAME);
    Mockito.verify(pbpFeign)
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertEquals(Constant.SYSTEM_ERROR, errorMessage);
  }

  @Test
  public void updateProductCategoryNoErrorTest() {
    Mockito.when(pbpFeign
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE))
        .thenReturn(new GdnBaseRestResponse(ERROR_MESSAGE, ErrorCategory.DATA_ACCESS.getCode(), true, null));
    String errorMessage = productBusinessPartnerRepositoryBean.updateProductCategory(PRODUCT_CODE, CATEGORY_CODE, USER_NAME);
    Mockito.verify(pbpFeign)
        .updateProductCategory(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            USER_NAME, PRODUCT_CODE, CATEGORY_CODE);
    Assertions.assertTrue(StringUtils.isEmpty(errorMessage));
  }

  @Test
  public void bulkDownloadSummaryNullSuccessTrueTest() throws ApplicationException {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    bulkDownloadSummaryResponse1 =
      new GdnRestSingleResponse<BulkDownloadProductLevel3Response>("Error", "Error", true, null,
        DEFAULT_REQUEST_ID);
    Mockito.when(pbpFeign.bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
      Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null))
      .thenReturn(bulkDownloadSummaryResponse1);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productBusinessPartnerRepositoryBean.bulkDownloadSummary(request,
              BUSINESS_PARTNER_CODE, null));
    }  finally {
      verify(pbpFeign).bulkDownloadSummaryFromDb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, true, request, null);
    }
  }

  @Test
  public void bulkArchiveProductFailureTest() throws ApplicationException {
    Map<String, String> productSkuErrorMessageMap = new HashMap<>();
    productSkuErrorMessageMap.put(PRODUCT_SKU,"Product is already archived");
    gdnRestSingleResponse.setSuccess(true);
    ItemBulkArchiveResponse itemBulkArchiveResponse = new ItemBulkArchiveResponse();
    itemBulkArchiveResponse.setProductSkuErrorMessageMap(productSkuErrorMessageMap);
    gdnRestSingleResponse.setValue(itemBulkArchiveResponse);
    Mockito.when(pbpFeign
        .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
          DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))
      .thenReturn(gdnRestSingleResponse);
    List<String> stringList = productBusinessPartnerRepositoryBean
      .bulkArchiveProductSkus(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, true,
        new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)), new HashMap<>());
    verify(pbpFeign)
      .bulkArchiveProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, DEFAULT_REQUEST_ID,
        DEFAULT_USERNAME, true, new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)));
    Assertions.assertFalse(stringList.contains(PRODUCT_SKU));
  }
}
