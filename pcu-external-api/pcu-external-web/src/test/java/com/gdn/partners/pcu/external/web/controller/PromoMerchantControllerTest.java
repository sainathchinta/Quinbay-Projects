package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.PromoMerchantApiPath;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class PromoMerchantControllerTest extends TestHelper {

  @InjectMocks
  private PromoMerchantController promoMerchantController;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductPricingService productPricingService;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(promoMerchantController).build();
  }

  private static final String ITEM_SKU = "ITEM_SKU";

  @Test
  public void promoDiscountSkuDetailTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(productPricingService.getPromoDiscountSkuDetail(Constants.STORE_ID, Constants.REQUEST_ID, ITEM_SKU))
        .thenReturn(new PromoItemDetailWebResponse());

    MockHttpServletRequestBuilder requestBuilder =
        get(PromoMerchantApiPath.BASE_PATH + PromoMerchantApiPath.SKU_DETAIL, ITEM_SKU).
            contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(productPricingService).getPromoDiscountSkuDetail(Constants.STORE_ID, Constants.REQUEST_ID, ITEM_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productPricingService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }
}
