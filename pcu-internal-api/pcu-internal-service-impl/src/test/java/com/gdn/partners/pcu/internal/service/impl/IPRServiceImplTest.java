package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressWebResponse;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.IPRProductsDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import org.apache.commons.collections.ListUtils;
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
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@ExtendWith(MockitoExtension.class)
public class IPRServiceImplTest {

  @Mock
  private PDTFeign pdtFeign;

  @Mock
  private BPService bpService;

  @Mock
  private PartnersEngineService partnersEngineService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @InjectMocks
  private IPRServiceImpl iprService;

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  public static final String CM = "CM";
  private static final String REQUEST_ID = "requestId";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-0001";
  private static final String DEFAULT_ASSIGNEE_NAME = "blibli";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ROLE_CODE = "roleCode";
  private static final String MESSAGE = "message";
  private static final String SORT_ORDER = "ASC";
  private static final String EMAIL = "abc@gmail.com";
  private final IPRProductListRequest iprProductListRequest = new IPRProductListRequest();
  private final IPRProductDetailResponse iprProductDetailResponse = new IPRProductDetailResponse();
  private GdnRestListResponse<IPRProductListResponse> iprProductListResponseGdnRestListResponse =
      new GdnRestListResponse<>();
  private GdnRestSingleResponse<IPRProductDetailResponse>
      iprProductDetailResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
  private GdnRestListResponse<IprSuspensionInProgressResponse>
      iprSuspensionInProgressResponseGdnRestListResponse = new GdnRestListResponse<>();
  private Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
  private final IPRProductListResponse iprProductListResponse = new IPRProductListResponse();
  private final List<IPRProductListResponse> iprProductListResponseList = new ArrayList<>();
  private final IprSuspensionInProgressResponse iprSuspensionInProgressResponse =
      new IprSuspensionInProgressResponse();
  private final List<IprSuspensionInProgressResponse> iprSuspensionInProgressResponses =
      new ArrayList<>();
  private IPRUpdateAssigneeRequest iprUpdateAssigneeRequest;
  private IprActionRequest iprActionRequest;
  private IPRProductsDownloadWebRequest iprProductsDownloadWebRequest;

  @Captor
  private ArgumentCaptor<IPRProductsDownloadRequest>
      iprProductsDownloadRequestArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    List<ProfileResponse> profileResponseList = new ArrayList<>();
    ProfileResponse profileResponse2 = new ProfileResponse();
    CompanyDTO companyDTO1 = new CompanyDTO();
    companyDTO1.setInternationalFlag(true);
    companyDTO1.setMerchantType(CM);
    profileResponse2.setCompany(companyDTO1);
    profileResponseList.add(profileResponse2);
    GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse =
        new GdnRestListResponse<>(profileResponseList, new PageMetaData(), REQUEST_ID);
    profileResponseMap = profileResponseGdnRestListResponse.getContent().stream()
        .collect(Collectors.toMap(ProfileResponse::getBusinessPartnerCode, Function.identity()));

    iprProductListRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    iprProductListRequest.setAssignedTo(DEFAULT_ASSIGNEE_NAME);

    iprActionRequest = new IprActionRequest();
    iprActionRequest.setProductSku(PRODUCT_SKU);

    iprProductListResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    iprProductListResponse.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    iprProductListResponse.setAssignedTo(DEFAULT_ASSIGNEE_NAME);
    iprProductListResponseList.add(iprProductListResponse);
    iprProductListResponseGdnRestListResponse =
        new GdnRestListResponse<>(iprProductListResponseList, new PageMetaData(), REQUEST_ID);
    iprProductDetailResponse.setProductCode(PRODUCT_SKU);
    iprProductDetailResponseGdnRestSingleResponse =
        new GdnRestSingleResponse<>(iprProductDetailResponse, REQUEST_ID);
    iprUpdateAssigneeRequest= IPRUpdateAssigneeRequest.builder().productSku(Collections.singletonList(PRODUCT_SKU))
        .assignedTo(DEFAULT_ASSIGNEE_NAME).build();
    iprSuspensionInProgressResponse.setProductSku(PRODUCT_SKU);
    iprSuspensionInProgressResponses.add(iprSuspensionInProgressResponse);
    iprSuspensionInProgressResponseGdnRestListResponse =
        new GdnRestListResponse<>(iprSuspensionInProgressResponses, new PageMetaData(), REQUEST_ID);
    iprProductsDownloadWebRequest = new IPRProductsDownloadWebRequest();
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(pdtFeign);
    Mockito.verifyNoMoreInteractions(bpService);
    Mockito.verifyNoMoreInteractions(partnersEngineService);
  }

  @Test
  void getProductListTest() {
    Mockito.when(this.pdtFeign.getIPRProductList(PAGE, SIZE, iprProductListRequest))
        .thenReturn(iprProductListResponseGdnRestListResponse);
    Mockito.when(bpService.getProfileResponseMap(Mockito.anyList())).thenReturn(profileResponseMap);
    Page<IPRProductListResponse> iprProductListResponsePage =
        iprService.getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(this.pdtFeign).getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME,
        iprProductListResponsePage.getContent().get(0).getAssignedTo());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        iprProductListResponsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        iprProductListResponsePage.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  void getSuspensionInProcessProductsTest() {
    Mockito.when(
      this.pdtFeign.findSuspensionInProgressProduct(PAGE, SIZE, DEFAULT_BUSINESS_PARTNER_CODE,
        SORT_ORDER)).thenReturn(iprSuspensionInProgressResponseGdnRestListResponse);
    Page<IprSuspensionInProgressWebResponse> iprProductListResponsePage =
      iprService.getSuspensionInProcessProducts(PAGE, SIZE, DEFAULT_BUSINESS_PARTNER_CODE,
        SORT_ORDER);
    Mockito.verify(this.pdtFeign)
      .findSuspensionInProgressProduct(PAGE, SIZE, DEFAULT_BUSINESS_PARTNER_CODE, SORT_ORDER);
    Assertions.assertEquals(PRODUCT_SKU,
      iprProductListResponsePage.getContent().get(0).getProductSku());
  }

  @Test
  void getProductListNullBPCodeTest() {
    iprProductListResponseGdnRestListResponse.getContent().get(0).setBusinessPartnerCode(null);
    Mockito.when(this.pdtFeign.getIPRProductList(PAGE, SIZE, iprProductListRequest))
        .thenReturn(iprProductListResponseGdnRestListResponse);
    Page<IPRProductListResponse> iprProductListResponsePage =
        iprService.getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(this.pdtFeign).getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(bpService, Mockito.times(1)).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME,
        iprProductListResponsePage.getContent().get(0).getAssignedTo());
    Assertions.assertNull(iprProductListResponsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        iprProductListResponsePage.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  void getProductListXBPExceptionTest() {
    Mockito.when(this.pdtFeign.getIPRProductList(PAGE, SIZE, iprProductListRequest))
        .thenReturn(iprProductListResponseGdnRestListResponse);
    Mockito.when(bpService.getProfileResponseMap(Mockito.anyList())).thenReturn(new HashMap<>());
    Page<IPRProductListResponse> iprProductListResponsePage =
        iprService.getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(this.pdtFeign).getIPRProductList(PAGE, SIZE, iprProductListRequest);
    Mockito.verify(bpService).getProfileResponseMap(Mockito.anyList());
    Assertions.assertEquals(DEFAULT_ASSIGNEE_NAME,
        iprProductListResponsePage.getContent().get(0).getAssignedTo());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        iprProductListResponsePage.getContent().get(0).getBusinessPartnerCode(),
        DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_NAME,
        iprProductListResponsePage.getContent().get(0).getBusinessPartnerName(),
        DEFAULT_BUSINESS_PARTNER_NAME);
  }

  @Test
  void getProductList_expectClientExceptionTest() {
    Mockito.when(this.pdtFeign.getIPRProductList(PAGE, SIZE, iprProductListRequest))
        .thenReturn(null);
    try {
      Page<IPRProductListResponse> productResponsePage =
          iprService.getIPRProductList(PAGE, SIZE, iprProductListRequest);
    } catch (ClientException ignored) {
    } finally {
      Mockito.verify(pdtFeign).getIPRProductList(PAGE, SIZE, iprProductListRequest);
    }
  }

  @Test
  void getProductList_successSetToFalseTest() {
    iprProductListResponseGdnRestListResponse.setSuccess(Boolean.FALSE);
    Mockito.when(this.pdtFeign.getIPRProductList(PAGE, SIZE, iprProductListRequest))
        .thenReturn(iprProductListResponseGdnRestListResponse);
    try {
      iprService.getIPRProductList(PAGE, SIZE, iprProductListRequest);
    } catch (ClientException ignored) {
    } finally {
      Mockito.verify(this.pdtFeign).getIPRProductList(PAGE, SIZE, iprProductListRequest);
    }
  }

  @Test
  void getIPRReviewReviewersTest() throws Exception {
    ReflectionTestUtils.setField(iprService, "iprRoleCodeReviewer", ROLE_CODE);
    Mockito.when(partnersEngineService.getIPRReviewersByRoleCode(ROLE_CODE))
      .thenReturn(List.of(DEFAULT_ASSIGNEE_NAME));
    List<String> result = iprService.getIPRReviewers();
    Mockito.verify(partnersEngineService).getIPRReviewersByRoleCode(ROLE_CODE);
    Assertions.assertEquals(1, result.size());
  }

  @Test
  void getIPRProductDetailsTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfficer(EMAIL);
    profileResponse.setCompany(companyDTO);
    profileResponse.setOfficial(true);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(
            iprProductDetailResponseGdnRestSingleResponse.getValue().getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    Mockito.when(pdtFeign.getIPRProductDetail(PRODUCT_SKU))
        .thenReturn(iprProductDetailResponseGdnRestSingleResponse);
    iprService.getIPRProductDetailByProductSku(PRODUCT_SKU);
    Assertions.assertEquals(EMAIL,
        iprProductDetailResponseGdnRestSingleResponse.getValue().getBpContact());
    Mockito.verify(pdtFeign).getIPRProductDetail(PRODUCT_SKU);
  }

  @Test
  void getIPRProductDetailsNullTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setOfficial(true);
    Mockito.when(bpService.getProfileResponseByBusinessPartnerCode(
            iprProductDetailResponseGdnRestSingleResponse.getValue().getBusinessPartnerCode()))
        .thenReturn(profileResponse);
    Mockito.when(pdtFeign.getIPRProductDetail(PRODUCT_SKU))
        .thenReturn(iprProductDetailResponseGdnRestSingleResponse);
    iprService.getIPRProductDetailByProductSku(PRODUCT_SKU);
    Assertions.assertEquals(Boolean.TRUE,
        iprProductDetailResponseGdnRestSingleResponse.getValue().isOfficial());
    Mockito.verify(pdtFeign).getIPRProductDetail(PRODUCT_SKU);
  }

  @Test
  void getIPRProductDetailsSuccessFalseTest() throws Exception {
    iprProductDetailResponseGdnRestSingleResponse.setSuccess(Boolean.FALSE);
    Mockito.when(pdtFeign.getIPRProductDetail(PRODUCT_SKU))
        .thenReturn(iprProductDetailResponseGdnRestSingleResponse);
    try {
      iprService.getIPRProductDetailByProductSku(PRODUCT_SKU);
    } catch (ClientException ignored) {
    } finally {
      Mockito.verify(this.pdtFeign).getIPRProductDetail(PRODUCT_SKU);
    }
  }

  @Test
  void updateAssigneeTest_success() throws Exception {
    ReflectionTestUtils.setField(iprService, "iprRoleCodeReviewer", ROLE_CODE);
    Mockito.when(partnersEngineService.getIPRReviewersByRoleCode(ROLE_CODE))
        .thenReturn(List.of(DEFAULT_ASSIGNEE_NAME));
    Mockito.when(pdtFeign.updateAssignee(iprUpdateAssigneeRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    iprService.updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(partnersEngineService).getIPRReviewersByRoleCode(ROLE_CODE);
    Mockito.verify(pdtFeign).updateAssignee(iprUpdateAssigneeRequest);
  }

  @Test
  void updateAssigneeTest_invalidUser() throws Exception {
    ReflectionTestUtils.setField(iprService, "iprRoleCodeReviewer", ROLE_CODE);
    Mockito.when(partnersEngineService.getIPRReviewersByRoleCode(ROLE_CODE))
        .thenReturn(ListUtils.EMPTY_LIST);
    try {
      iprService.updateAssignee(iprUpdateAssigneeRequest);
    }
    catch (InvalidStateException e){
      Assertions.assertEquals(ErrorMessages.INVALID_USER_NAME, e.getMessage());
    }
    Mockito.verify(partnersEngineService).getIPRReviewersByRoleCode(ROLE_CODE);
  }

  @Test
  void updateAssigneeTest_emptyAssignee() throws Exception {
    iprUpdateAssigneeRequest.setAssignedTo("");
    Mockito.when(pdtFeign.updateAssignee(iprUpdateAssigneeRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    iprService.updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(pdtFeign).updateAssignee(iprUpdateAssigneeRequest);
  }

  @Test
  void getPrimaryFilterCountsTest() {
    GdnRestSingleResponse<MapResponse> pdtResponse =
      new GdnRestSingleResponse<>(new MapResponse(), REQUEST_ID);
    Mockito.when(pdtFeign.getPrimaryFilterCounts()).thenReturn(pdtResponse);
    iprService.getPrimaryFilterCounts();
    Mockito.verify(pdtFeign).getPrimaryFilterCounts();
  }

  @Test
  void performIprActionTest() {
    Mockito.when(pdtFeign.performIprAction(iprActionRequest))
        .thenReturn(new GdnBaseRestResponse(true));
    iprService.performIprAction(iprActionRequest);
    Mockito.verify(pdtFeign).performIprAction(iprActionRequest);
  }

  @Test
  void performIprActionErrorMessageTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    response.setErrorMessage(MESSAGE);
    response.setErrorCode(MESSAGE);
    Mockito.when(pdtFeign.performIprAction(iprActionRequest)).thenReturn(response);
    iprService.performIprAction(iprActionRequest);
    Mockito.verify(pdtFeign).performIprAction(iprActionRequest);
  }

  @Test
  void fetchHistoryTest() {
    Mockito.when(pdtFeign.fetchIprHistory(PAGE, SIZE, PRODUCT_SKU)).thenReturn(
        new GdnRestListResponse<>(Collections.EMPTY_LIST, new PageMetaData(SIZE, PAGE, SIZE),
            REQUEST_ID));
    iprService.fetchIprProductHistory(PAGE, SIZE, PRODUCT_SKU);
    Mockito.verify(pdtFeign).fetchIprHistory(PAGE, SIZE, PRODUCT_SKU);
  }

  @Test
  void downloadSelectedIPRProductTest() {
    iprProductsDownloadWebRequest.setProductSkuList(List.of(PRODUCT_SKU));
    this.iprService.downloadIPRProducts(Constants.USER_NAME,
        iprProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            iprProductsDownloadRequestArgumentCaptor.capture());
    IPRProductsDownloadRequest iprProductsDownloadRequest =
        iprProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(iprProductsDownloadRequest);
    Assertions.assertEquals(iprProductsDownloadWebRequest.getProductSkuList(),
        iprProductsDownloadRequest.getProductSkuList());
    Assertions.assertEquals(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_SELECTED,
        iprProductsDownloadRequest.getBulkProcessEntity());
  }

  @Test
  void downloadAllIPRProductTest() {
    iprProductsDownloadWebRequest.setKeyword(PRODUCT_SKU);
    this.iprService.downloadIPRProducts(Constants.USER_NAME,
        iprProductsDownloadWebRequest);
    Mockito.verify(this.kafkaPublisher)
        .send(Mockito.eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), Mockito.eq(Constants.USER_NAME),
            iprProductsDownloadRequestArgumentCaptor.capture());
    IPRProductsDownloadRequest iprProductsDownloadRequest =
        iprProductsDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(iprProductsDownloadRequest);
    Assertions.assertEquals(iprProductsDownloadWebRequest.getProductSkuList(),
        iprProductsDownloadRequest.getProductSkuList());
    Assertions.assertEquals(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL,
        iprProductsDownloadRequest.getBulkProcessEntity());
  }

}
