package com.gdn.partners.pcu.internal.service.impl;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerProfileWebResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;


import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.XBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BusinessPartnerServiceImplTest {

  @InjectMocks
  private BusinessPartnerServiceImpl businessPartnerService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private XBPFeign xbpFeign;

  private GdnRestListResponse<ProductBusinessPartnerMapperResponse> response;
  private List<ProductBusinessPartnerMapperResponse> partnerMapperResponseList;
  private static final String BUSINESS_PARTNER_NAME = "name";
  private static final long TOTAL_NUM_FOUND = 100;
  private static final String KEYWORD = "keyword";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String FLAG = "MERCHANT";
  private static final String STATUS = "ACTIVE";
  private static final String SORT_DIRECTION = "ASC";
  private GdnRestListResponse<ProfileResponse> profileResponseGdnRestListResponse;
  private List<ProfileResponse> profileResponseList;
  private ProfileResponse profileResponse = new ProfileResponse();
  private static final String DEFAULT_SELLER_NAME = "seller name";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-1001";

  @BeforeEach
  public void setUp() throws Exception {
    partnerMapperResponseList = new ArrayList<>();
    ProductBusinessPartnerMapperResponse productBusinessPartnerMapperResponse = new ProductBusinessPartnerMapperResponse();
    productBusinessPartnerMapperResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    productBusinessPartnerMapperResponse.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    partnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    response = new GdnRestListResponse<>(partnerMapperResponseList, new PageMetaData(PAGE, SIZE, TOTAL_NUM_FOUND),
        Constants.REQUEST_ID);
    profileResponseList = new ArrayList<>();
    profileResponse.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    profileResponse.setCompany(companyDTO);
    profileResponseList.add(profileResponse);
    profileResponseGdnRestListResponse =
        new GdnRestListResponse<>(profileResponseList, new PageMetaData(PAGE, SIZE, TOTAL_NUM_FOUND),
            Constants.REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterTest() {
    SummaryFilterRequest request = SummaryFilterRequest.builder().statusFilter(Constants.DEFAULT_FILTER_STATE).timeFilter(Constants.DEFAULT_FILTER_STATE)
        .searchKeyword(KEYWORD).build();
    ReviewProductsFilterRequest reviewProductsFilterRequest =
        ReviewProductsFilterRequest.builder().statusFilter(Constants.DEFAULT_FILTER_STATE).timeFilter(Constants.DEFAULT_FILTER_STATE)
            .searchKeyword(KEYWORD).build();
    Mockito.doReturn(response).when(this.pbpFeign)
        .getBusinessPartnersByTimeAndStatusFilter(request, Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    Page<BusinessPartnerWebResponse> businessPartnerWebResponsePage = this.businessPartnerService
        .getBusinessPartnersByTimeAndStatusFilter(reviewProductsFilterRequest, Boolean.FALSE, Boolean.FALSE, PAGE,
            SIZE);
    Mockito.verify(this.pbpFeign)
        .getBusinessPartnersByTimeAndStatusFilter(request, Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    Assertions.assertNotNull(businessPartnerWebResponsePage);
    Assertions.assertEquals(businessPartnerWebResponsePage.getContent().get(0).getBusinessPartnerCode(),
        Constants.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getAllActiveMerchantListTest() {
    BusinessPartnerFilterRequest businessPartnerFilterRequest =
        BusinessPartnerFilterRequest.builder().category(Constants.CATEGORY_CHANGED).sortDirection(SORT_DIRECTION)
            .businessPartnerCodes(null).keywords(KEYWORD).merchantTypes(null).tags(null).status(STATUS).flag(FLAG)
            .type(StringUtils.EMPTY).sortedBy(StringUtils.EMPTY).build();

    ProductSuspensionFilterRequest productSuspensionFilterRequest =
        ProductSuspensionFilterRequest.builder().categoryCode(Constants.CATEGORY_CHANGED)
            .businessPartnerCode(Constants.BUSINESS_PARTNER_CODE).searchKeyword(KEYWORD)
            .status(Constants.DEFAULT_FILTER_STATE).build();
    Mockito.doReturn(profileResponseGdnRestListResponse).when(this.xbpFeign)
        .getAllActiveMerchantList(businessPartnerFilterRequest, PAGE, SIZE);
    Page<BusinessPartnerWebResponse> response =
        this.businessPartnerService.getAllActiveMerchantList(productSuspensionFilterRequest, PAGE, SIZE);
    Mockito.verify(this.xbpFeign).getAllActiveMerchantList(businessPartnerFilterRequest, PAGE, SIZE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response.getContent().get(0).getBusinessPartnerCode(), Constants.BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(response.getContent().get(0).getBusinessPartnerName(), BUSINESS_PARTNER_NAME);
  }

 @Test
 public void getBusinessPartnerNameByCodeTest(){
    GdnRestSingleResponse<ProfileResponse> response = new GdnRestSingleResponse<>();
    response.setValue(profileResponse);
    response.setSuccess(Boolean.TRUE);
    Mockito.when(this.xbpFeign.filterByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(response);
    String sellerName =
      this.businessPartnerService.getBusinessPartnerNameByCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_NAME,sellerName);
  }

  @Test
  public void fetchBusinessPartnerFlagsTest(){
    Map<String, Object> flags = new HashMap<>();
    flags.put(Constants.PRODUCT_CONSEQUENCE_LIMITATION, true);
    profileResponse.setFlags(flags);
    GdnRestSingleResponse<ProfileResponse> response = new GdnRestSingleResponse<>();
    response.setValue(profileResponse);
    response.setSuccess(Boolean.TRUE);
    Mockito.when(this.xbpFeign.filterByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(response);
    BusinessPartnerProfileWebResponse businessPartnerProfileWebResponse =
      this.businessPartnerService.fetchBusinessPartnerFlags(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).filterByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(businessPartnerProfileWebResponse.isProductConsequenceLimitation());
  }
}