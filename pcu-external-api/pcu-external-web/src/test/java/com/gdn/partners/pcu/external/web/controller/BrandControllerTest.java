package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.BrandApiPath;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BrandService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.response.PredefinedAttributeValueWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandSummaryRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class BrandControllerTest extends TestHelper {

  private static final String KEYWORD = "apple";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 10;
  private static final String ID = "id";
  private static final String CODE = "code";
  private static final String IS_SEARCH = "isSearch";
  private static final String IS_EXTERNAL = "isExternal";
  private static final String VALUE = "value";
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private static final String DEFAULT_BRAND_CODE = "BRD-0001";
  private static final String BP_CODE = "BP_CODE";

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private BrandService brandService;

  @InjectMocks
  private BrandController brandController;


  private List<PredefinedAttributeValueWebResponse> predefinedAttributeValueWebResponses;
  private PredefinedAttributeValueWebResponse predefinedAttributeValueWebResponse;
  private BrandResponse brandResponse = new BrandResponse();
  private List<BrandResponse> brandResponseList = new ArrayList<>();
  private BrandSummaryRequest request = new BrandSummaryRequest();
  private Page<BrandResponse> responses;
  private Pageable pageable = PageRequest.of(PAGE, SIZE);

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.brandController).build();
    predefinedAttributeValueWebResponse = new PredefinedAttributeValueWebResponse();
    predefinedAttributeValueWebResponse.setId(ID);
    predefinedAttributeValueWebResponse.setPredefinedAllowedAttributeCode(CODE);
    predefinedAttributeValueWebResponses = Arrays.asList(predefinedAttributeValueWebResponse);
    brandResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandResponseList.add(brandResponse);
    responses = new PageImpl<>(brandResponseList, pageable, 1);
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setMarkForDelete(false);
    pageable = PageRequest.of(PAGE, SIZE);
  }

  @Test
  public void getBrandSuggestionsTest() throws Exception {
    Page<PredefinedAttributeValueWebResponse> responsePage =
        new PageImpl<>(predefinedAttributeValueWebResponses, PageRequest.of(PAGE, SIZE), SIZE);
    when(brandService.getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE))
        .thenReturn(responsePage);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.GET_BRAND_SUGGESTIONS).param(VALUE, KEYWORD)
            .param(IS_SEARCH, String.valueOf(Boolean.TRUE))
            .param(Constants.PAGE, "0").param(Constants.SIZE, "10")
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(brandService).getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE);
    verify(mandatoryParameterHelper).getRequestId();

  }

  @Test
  public void filterSummaryTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.brandService.findSummaryByFilter(Mockito.any(BrandSummaryRequest.class),
        Mockito.any(Pageable.class))).thenReturn(responses);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.FILTER_SUMMARY).param("brandName", DEFAULT_BRAND_NAME)
            .param("markForDelete", String.valueOf(Boolean.FALSE))
            .param(Constants.PAGE, "0").param(Constants.SIZE, "10")
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)))
        .andExpect(jsonPath("$.content.[0].brandName", is(DEFAULT_BRAND_NAME)))
    .andExpect(jsonPath("$.metadata.totalItems", is(1)));
    verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(this.brandService).findSummaryByFilter((Mockito.any(BrandSummaryRequest.class)),
        Mockito.any(Pageable.class));
  }

  @Test
  public void filterSummaryTest_throwsApplicationException() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.brandService.findSummaryByFilter(Mockito.any(BrandSummaryRequest.class),
        Mockito.any(Pageable.class))).thenThrow(RuntimeException.class);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          get(BrandApiPath.BASE_PATH + BrandApiPath.FILTER_SUMMARY).param("brandName", DEFAULT_BRAND_NAME)
              .param("markForDelete", String.valueOf(Boolean.FALSE))
              .param(Constants.PAGE, "0").param(Constants.SIZE, "10")
              .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
      mockMvc.perform(requestBuilder).andExpect(status().isOk());

    } catch (Exception e) {

    }
    finally {
      verify(mandatoryParameterHelper).getRequestId();
      Mockito.verify(this.brandService).findSummaryByFilter((Mockito.any(BrandSummaryRequest.class)),
          Mockito.any(Pageable.class));
    }
  }

  @Test
  public void filterByBrandNameTest() throws Exception{
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(brandService.findByBrandName(DEFAULT_BRAND_NAME)).thenReturn(brandResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.FILTER_BRAND_NAME).param("brandName", DEFAULT_BRAND_NAME)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(this.brandService).findByBrandName(DEFAULT_BRAND_NAME);
  }

  @Test
  public void filterByBrandNameTest_brandDoesntExist() throws Exception{
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(brandService.findByBrandName(DEFAULT_BRAND_NAME)).thenReturn(null);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.FILTER_BRAND_NAME).param("brandName", DEFAULT_BRAND_NAME)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    Mockito.verify(this.brandService).findByBrandName(DEFAULT_BRAND_NAME);
  }

  @Test
  public void getBrandEmptySuggestionsTest() throws Exception {
    Page<PredefinedAttributeValueWebResponse> responsePage =
        new PageImpl<>(Collections.emptyList(), PageRequest.of(PAGE, SIZE), SIZE);
    when(brandService.getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE)).thenReturn(responsePage);
    when(brandService.getDefaultBrands(mandatoryParameterHelper)).thenReturn(predefinedAttributeValueWebResponses);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.GET_BRAND_SUGGESTIONS).param(VALUE, KEYWORD)
            .param(IS_SEARCH, String.valueOf(Boolean.TRUE))
            .param(Constants.PAGE, "0").param(Constants.SIZE, "10")
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(brandService).getBrandSuggestions(KEYWORD, mandatoryParameterHelper, Boolean.TRUE, PAGE, SIZE);
    verify(brandService).getDefaultBrands(mandatoryParameterHelper);
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void validateAuthorisedBrandTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.isExternal()).thenReturn(String.valueOf(true));
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    Mockito.when(brandService.validateAuthorisedBrand(DEFAULT_BRAND_CODE, Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.VALIDATE).param("brandCode", DEFAULT_BRAND_CODE)
            .param(Constants.BUSINESS_PARTNER_CODE, BP_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternal();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(this.brandService).validateAuthorisedBrand(DEFAULT_BRAND_CODE, Constants.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void validateAuthorisedBrandInternalTest() throws Exception {
        when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.isExternal()).thenReturn(String.valueOf(false));
    Mockito.when(brandService.validateAuthorisedBrand(DEFAULT_BRAND_CODE, Constants.BUSINESS_PARTNER_CODE))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.VALIDATE).param("brandCode", DEFAULT_BRAND_CODE)
            .param(Constants.BUSINESS_PARTNER_CODE, BP_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(this.brandService).validateAuthorisedBrand(DEFAULT_BRAND_CODE, BP_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(brandService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }
}