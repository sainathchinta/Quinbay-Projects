package com.gdn.partners.pcu.external.service.impl;


import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class BrandServiceImplTest {

  private static final String KEYWORD = "apple";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String STORE_ID = "STORE_ID";
  private static final String BP_CODE = "BP_CODE";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String ID = "id";
  private static final String CODE = "code";
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private static final String DEFAULT_BRAND_CODE = "BRD-0001";
  private static final String CATEGORY_ID = "categoryID";
  private static final String VALUE = "value";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response;
  private List<PredefinedAllowedAttributeValueResponse> responseList;
  private PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse;
  private BrandSummaryRequest brandSummaryRequest = new BrandSummaryRequest();
  private GdnRestListResponse<BrandResponse> responses = new GdnRestListResponse<>();
  private List<BrandResponse> brandResponses = new ArrayList<>();
  private BrandResponse brandResponse = new BrandResponse();
  private Pageable pageable;
  private GdnRestSingleResponse<BrandResponse> responseGdnRestSingleResponse = new GdnRestSingleResponse<>();
  private GdnRestListResponse<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponseGdnRestListResponse;
  private GdnRestSingleResponse<SimpleBooleanResponse> simpleBooleanResponseGdnRestSingleResponse = new GdnRestSingleResponse<>();
  private GdnRestListResponse<BrandInReviewResponse> brandInReviewResponseGdnRestListResponse;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private BrandServiceImpl brandService;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setId(ID);
    predefinedAllowedAttributeValueResponse.setPredefinedAllowedAttributeCode(CODE);
    responseList = Arrays.asList(predefinedAllowedAttributeValueResponse);
    response = new GdnRestListResponse<>(responseList, new PageMetaData(), REQUEST_ID);
    brandSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandSummaryRequest.setMarkForDelete(false);
    brandResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandResponse.setBrandCode(DEFAULT_BRAND_CODE);
    responseGdnRestSingleResponse.setValue(brandResponse);
    responseGdnRestSingleResponse.setSuccess(true);
    brandResponses.add(brandResponse);
    responses.setContent(brandResponses);
    responses.setSuccess(true);
    pageable = PageRequest.of(PAGE, SIZE);
    simpleBooleanResponseGdnRestSingleResponse.setValue(new SimpleBooleanResponse(true));
    simpleBooleanResponseGdnRestSingleResponse.setSuccess(true);
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponseGdnRestListResponse =
        new GdnRestListResponse<>(Arrays.asList(predefinedAllowedAttributeValueResponse), new PageMetaData(0, 10, 1),
            REQUEST_ID);

    BrandInReviewResponse brandInReviewResponse = new BrandInReviewResponse();
    brandInReviewResponseGdnRestListResponse =
      new GdnRestListResponse<>(Arrays.asList(brandInReviewResponse), new PageMetaData(0, 10, 1),
        REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
    Mockito.verifyNoMoreInteractions(pbpFeign);
  }

  @Test
  public void getBrandSuggestionsWithIsExternalTrueTest() {
    when(mandatoryParameterHelper.isExternal()).thenReturn(String.valueOf(Boolean.TRUE));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BP_CODE);
    when(this.pcbFeign.getBrandSuggestions(KEYWORD, BP_CODE, Boolean.TRUE, Boolean.TRUE, PAGE, SIZE))
        .thenReturn(response);
    Page<PredefinedAttributeValueWebResponse> predefinedAttributeValueWebResponses =
        brandService.getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE);
    verify(pcbFeign).getBrandSuggestions(KEYWORD, BP_CODE, Boolean.TRUE, Boolean.TRUE, PAGE, SIZE);
    verify(mandatoryParameterHelper).isExternal();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    assertNotNull(predefinedAttributeValueWebResponses);
    assertEquals(predefinedAttributeValueWebResponses.getContent().size(), 1);
  }

  @Test
  public void getBrandSuggestionsWithIsExternalFalseTest() {
    when(mandatoryParameterHelper.isExternal()).thenReturn(String.valueOf(Boolean.FALSE));
    when(this.pcbFeign.getBrandSuggestions(KEYWORD, Constants.DEFAULT_BP_CODE, Boolean.TRUE, Boolean.FALSE, PAGE, SIZE))
        .thenReturn(response);
    Page<PredefinedAttributeValueWebResponse> predefinedAttributeValueWebResponses =
        brandService.getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE);
    verify(pcbFeign).getBrandSuggestions(KEYWORD, Constants.DEFAULT_BP_CODE, Boolean.TRUE, Boolean.FALSE, PAGE, SIZE);
    verify(mandatoryParameterHelper).isExternal();
    assertNotNull(predefinedAttributeValueWebResponses);
    assertEquals(predefinedAttributeValueWebResponses.getContent().size(), 1);
  }

  @Test
  public void getBrandSuggestionsExceptionTest1() {
    when(mandatoryParameterHelper.isExternal()).thenReturn(String.valueOf(Boolean.TRUE));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BP_CODE);
    when(this.pcbFeign.getBrandSuggestions(KEYWORD, BP_CODE, Boolean.TRUE, Boolean.TRUE, PAGE, SIZE))
        .thenThrow(ClientException.class);
    Page<PredefinedAttributeValueWebResponse> predefinedAttributeValueWebResponses = null;
    try {
      predefinedAttributeValueWebResponses =
          brandService.getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE);
    } catch (ClientException e) {
    } finally {
      verify(pcbFeign).getBrandSuggestions(KEYWORD, BP_CODE, Boolean.TRUE, Boolean.TRUE, PAGE, SIZE);
      verify(mandatoryParameterHelper).isExternal();
      verify(mandatoryParameterHelper).getBusinessPartnerCode();
      assertNull(predefinedAttributeValueWebResponses);
    }
  }

  @Test
  public void findSummaryByFilterTest() {
    when(this.pcbFeign.filterSummary(PAGE, SIZE, brandSummaryRequest)).thenReturn(responses);
    Page<BrandResponse> page = this.brandService.findSummaryByFilter(brandSummaryRequest, pageable);
    Mockito.verify(this.pcbFeign).filterSummary(PAGE, SIZE, brandSummaryRequest);
    Assertions.assertEquals(DEFAULT_BRAND_NAME, page.getContent().get(0).getBrandName());
    Assertions.assertEquals(DEFAULT_BRAND_CODE, page.getContent().get(0).getBrandCode());
  }

  @Test
  public void findSummaryByFilterTest_throwsException() {
    Page<BrandResponse> page = null;
    when(this.pcbFeign.filterSummary(PAGE, SIZE, brandSummaryRequest))
        .thenThrow(ClientException.class);
    try{
      page = this.brandService.findSummaryByFilter(brandSummaryRequest, pageable);
    }
    catch (ClientException e) {

    }
    finally {
      Mockito.verify(this.pcbFeign).filterSummary(PAGE, SIZE, brandSummaryRequest);
      assertNull(page);
    }
  }

  @Test
  public void activeBrandsByCategoryIdTest() throws Exception {
    Mockito.when(this.pbpFeign.activeBrandByCategoryId(CATEGORY_ID))
      .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAllowedAttributeValueResponse> responses =
        this.brandService.activeBrandsByCategoryId(CATEGORY_ID);
    Mockito.verify(this.pbpFeign).activeBrandByCategoryId(CATEGORY_ID);
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void activeBrandsByCategoryIdExceptionTest() throws Exception {
    Mockito.when(this.pbpFeign.activeBrandByCategoryId(CATEGORY_ID))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      this.brandService.activeBrandsByCategoryId(CATEGORY_ID);
    } catch (Exception e) {
      Mockito.verify(this.pbpFeign).activeBrandByCategoryId(CATEGORY_ID);
    }
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getBrandSuggestions(Mockito.eq(VALUE), Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(true), Mockito.eq(true),
            Mockito.eq(0), Mockito.anyInt())).thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAllowedAttributeValueResponse> responses =
        this.brandService.getBrandSuggestions(VALUE, BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.pcbFeign)
        .getBrandSuggestions(Mockito.eq(VALUE), Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(true), Mockito.eq(true),
            Mockito.eq(0), Mockito.anyInt());
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }


  @Test
  public void getBrandSuggestionsExceptionTest() throws Exception {
    Mockito.when(this.pcbFeign
        .getBrandSuggestions(Mockito.eq(VALUE), Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(true), Mockito.eq(true),
            Mockito.eq(0), Mockito.anyInt()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    List<PredefinedAllowedAttributeValueResponse> responses = this.brandService.getBrandSuggestions(VALUE, BUSINESS_PARTNER_CODE, true, true);
    Mockito.verify(this.pcbFeign)
        .getBrandSuggestions(Mockito.eq(VALUE), Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(true), Mockito.eq(true),
            Mockito.eq(0), Mockito.anyInt());
    assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void findByNameTest() throws Exception {
    Mockito.when(this.pcbFeign.filterByBrandName(DEFAULT_BRAND_NAME, true)).thenReturn(responseGdnRestSingleResponse);
    BrandResponse brandResponse1 = this.brandService.findByBrandName(DEFAULT_BRAND_NAME);
    Mockito.verify(this.pcbFeign).filterByBrandName(DEFAULT_BRAND_NAME, true);
    Assertions.assertEquals(DEFAULT_BRAND_CODE, brandResponse1.getBrandCode());
    Assertions.assertEquals(DEFAULT_BRAND_NAME, brandResponse1.getBrandName());
  }

  @Test
  public void getDefaultBrandsTest() {
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(this.pcbFeign).getDefaultBrands();
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnTest() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.ACTIVE);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnAppTest() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper, times(2)).getAppType();
    Mockito.verify(this.pcbFeign).getDefaultBrands();
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnAndroidAppTest() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    ReflectionTestUtils.setField(brandService, "noBrandAndroidVersion", 10);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppType()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppVersion()).thenReturn(1);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper, times(2)).getAppType();
    Mockito.verify(mandatoryParameterHelper).getAppVersion();
    Mockito.verify(this.pcbFeign).getDefaultBrands();
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnAndroidApp2Test() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    ReflectionTestUtils.setField(brandService, "noBrandAndroidVersion", 10);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppType()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppVersion()).thenReturn(11);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getAppType();
    Mockito.verify(mandatoryParameterHelper).getAppVersion();
    Mockito.verify(mandatoryParameterHelper).getClientId();
    assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnIosAppTest() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    ReflectionTestUtils.setField(brandService, "noBrandAndroidVersion", 10);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppType()).thenReturn(Constants.IOS_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppVersion()).thenReturn(1);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper, times(2)).getAppType();
    Mockito.verify(mandatoryParameterHelper).getAppVersion();
    assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnIosApp2Test() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    ReflectionTestUtils.setField(brandService, "noBrandAndroidVersion", 10);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppType()).thenReturn(Constants.IOS_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppVersion()).thenReturn(11);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper, times(2)).getAppType();
    Mockito.verify(mandatoryParameterHelper).getAppVersion();
    Mockito.verify(mandatoryParameterHelper).getClientId();
    assertNotNull(responses);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void getDefaultBrandsNoBrandSwitchOnNewAndroidAppTest() {
    ReflectionTestUtils.setField(brandService, "noBrandSwitch", true);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(mandatoryParameterHelper.getAppType()).thenReturn(Constants.APP_CLIENT_ID);
    Mockito.when(this.pcbFeign.getDefaultBrands())
        .thenReturn(predefinedAllowedAttributeValueResponseGdnRestListResponse);
    List<PredefinedAttributeValueWebResponse> responses = this.brandService.getDefaultBrands(mandatoryParameterHelper);
    Mockito.verify(mandatoryParameterHelper).getClientId();
    Mockito.verify(mandatoryParameterHelper, times(2)).getAppType();
    Mockito.verify(mandatoryParameterHelper).getAppVersion();
    Mockito.verify(this.pcbFeign).getDefaultBrands();
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }

  @Test
  public void validateAuthorisedBrandTest() {
    Mockito.when(pcbFeign.validateAuthorisedBrand(DEFAULT_BRAND_CODE, BUSINESS_PARTNER_CODE))
        .thenReturn(simpleBooleanResponseGdnRestSingleResponse);
    boolean authorisedBrand = this.brandService.validateAuthorisedBrand(DEFAULT_BRAND_CODE, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbFeign).validateAuthorisedBrand(DEFAULT_BRAND_CODE, BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(authorisedBrand);
  }

  @Test
  public void getAllInReviewBrandTest() {
    Mockito.when(this.pcbFeign.getAllInReviewBrands(STORE_ID,REQUEST_ID))
      .thenReturn(brandInReviewResponseGdnRestListResponse);
    List<BrandInReviewResponse> responses = this.brandService.getAllInReviewBrand(STORE_ID,REQUEST_ID);
    Mockito.verify(this.pcbFeign).getAllInReviewBrands(STORE_ID,REQUEST_ID);
    assertNotNull(responses);
    Assertions.assertEquals(1, responses.size());
  }
}