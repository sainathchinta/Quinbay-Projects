package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.download.BrandAuthDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthDataResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.service.PCBOutboundService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;

public class BulkBrandAuthorizationDataServiceBeanTest {
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final Date AUTH_START_DATE = new Date(1689645885000L);
  private static final Date AUTH_END_DATE = new Date(1689645885000L);
  private static final String STATUS = "status";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private BulkBrandAuthorizationDataServiceBean bulkBrandAuthorizationDataServiceBean;

  @Mock
  private PCBOutboundService pcbOutboundService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Captor
  private ArgumentCaptor<BrandAuthFilterRequest> brandAuthFilterRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BusinessPartnerCodesRequest> businessPartnerCodesRequestArgumentCaptor;

  private BrandAuthFilterResponse brandAuthFilterResponse1;
  private BrandAuthFilterResponse brandAuthFilterResponse2;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(bulkBrandAuthorizationDataServiceBean, "simpleDateFormat", "dd/MM/yyyy");
    ReflectionTestUtils.setField(bulkBrandAuthorizationDataServiceBean, "brandAuthDownloadSize", 1);
    ReflectionTestUtils.setField(bulkBrandAuthorizationDataServiceBean, "fetchSellerNameSize", 1);

    brandAuthFilterResponse1 =
        new BrandAuthFilterResponse(BRAND_CODE, BRAND_NAME, SELLER_CODE, AUTH_START_DATE, AUTH_END_DATE, null, null);
    brandAuthFilterResponse2 =
        new BrandAuthFilterResponse(BRAND_CODE, BRAND_NAME, SELLER_CODE, AUTH_START_DATE, AUTH_END_DATE, null, null);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(pcbOutboundService, businessPartnerRepository);
  }

  @Test
  public void getDataTest() throws Exception {
    BrandAuthDownloadRequest brandAuthDownloadRequest = new BrandAuthDownloadRequest(SELLER_CODE, BRAND_NAME, STATUS);
    BusinessPartnerFilterRequest businessPartnerFilterRequest = new BusinessPartnerFilterRequest();
    businessPartnerFilterRequest.setBusinessPartnerCodes(Set.of(SELLER_CODE));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(SELLER_CODE);
    profileResponse.setCompany(CompanyDTO.builder().businessPartnerName(SELLER_NAME).build());
    Mockito.when(pcbOutboundService.getAuthorisations(Mockito.eq(Constant.STORE_ID), Mockito.eq(0), Mockito.eq(1),
        brandAuthFilterRequestArgumentCaptor.capture())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(brandAuthFilterResponse1), new PageMetaData(0, 1, 2),
            REQUEST_ID));
    Mockito.when(pcbOutboundService.getAuthorisations(Mockito.eq(Constant.STORE_ID), Mockito.eq(1), Mockito.eq(1),
        brandAuthFilterRequestArgumentCaptor.capture())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(brandAuthFilterResponse2), new PageMetaData(0, 1, 2),
            REQUEST_ID));
    Mockito.when(
        businessPartnerRepository.filterByBusinessPartnerCodeList(businessPartnerFilterRequest, 0, 1))
      .thenReturn(List.of(profileResponse));

    BrandAuthDataResponse bulkDataResponse = (BrandAuthDataResponse) bulkBrandAuthorizationDataServiceBean.getData(brandAuthDownloadRequest);

    Mockito.verify(pcbOutboundService).getAuthorisations(Constant.STORE_ID,0,1,
        brandAuthFilterRequestArgumentCaptor.getAllValues().get(0));
    Mockito.verify(pcbOutboundService).getAuthorisations(Constant.STORE_ID,1,1,
        brandAuthFilterRequestArgumentCaptor.getAllValues().get(1));
    Mockito.verify(businessPartnerRepository)
      .filterByBusinessPartnerCodeList(businessPartnerFilterRequest, 0, 1);
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getSellerCode());
    Assertions.assertEquals(SELLER_CODE, brandAuthFilterRequestArgumentCaptor.getAllValues().get(1).getSellerCode());
    Assertions.assertEquals(BRAND_NAME, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getBrandName());
    Assertions.assertEquals(BRAND_NAME, brandAuthFilterRequestArgumentCaptor.getAllValues().get(1).getBrandName());
    Assertions.assertEquals(STATUS, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getStatus());
    Assertions.assertEquals(STATUS, brandAuthFilterRequestArgumentCaptor.getAllValues().get(1).getStatus());

    Assertions.assertEquals(SELLER_CODE, bulkDataResponse.getBrandAuthResponseList().get(0).getSellerCode());
    Assertions.assertEquals(SELLER_NAME, bulkDataResponse.getBrandAuthResponseList().get(0).getSellerName());
    Assertions.assertEquals(BRAND_NAME, bulkDataResponse.getBrandAuthResponseList().get(0).getBrandName());
    Assertions.assertEquals(BRAND_CODE, bulkDataResponse.getBrandAuthResponseList().get(0).getBrandCode());
    Assertions.assertEquals("18/07/2023", bulkDataResponse.getBrandAuthResponseList().get(0).getAuthStartDate());
    Assertions.assertEquals("18/07/2023", bulkDataResponse.getBrandAuthResponseList().get(0).getAuthEndDate());
  }

  @Test
  public void getDataEmptyResponseTest() throws Exception {
    BrandAuthDownloadRequest brandAuthDownloadRequest = new BrandAuthDownloadRequest(SELLER_CODE, BRAND_NAME, STATUS);
    Mockito.when(pcbOutboundService.getAuthorisations(Mockito.eq(Constant.STORE_ID), Mockito.eq(0), Mockito.eq(1),
        brandAuthFilterRequestArgumentCaptor.capture())).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 1, 0),
            REQUEST_ID));
    BrandAuthDataResponse bulkDataResponse = (BrandAuthDataResponse) bulkBrandAuthorizationDataServiceBean.getData(brandAuthDownloadRequest);

    Mockito.verify(pcbOutboundService).getAuthorisations(Constant.STORE_ID,0,1,
        brandAuthFilterRequestArgumentCaptor.getAllValues().get(0));

    Assertions.assertEquals(SELLER_CODE, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getSellerCode());
    Assertions.assertEquals(BRAND_NAME, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getBrandName());
    Assertions.assertEquals(STATUS, brandAuthFilterRequestArgumentCaptor.getAllValues().get(0).getStatus());

    Assertions.assertTrue(bulkDataResponse.getBrandAuthResponseList().isEmpty());
  }


}
