package com.gdn.partners.pcu.external.service.impl;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.service.ProductService;
import com.gdn.partners.pcu.external.service.ProductV2Service;
import com.gdn.partners.pcu.external.service.UserPicService;
import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.response.ItemL3ListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

public class ProductWrapperV2ServiceImplTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU_1 = "To6-15120-10686";
  private static final int PAGE = 0;
  private static final int SIZE = 10;

  private ProductSummaryV2WebRequest productSummaryV2WebRequest = new ProductSummaryV2WebRequest();
  private ProductLevel3ListingWebResponse productLevel3ListingWebResponse =
    new ProductLevel3ListingWebResponse();
  private ItemL3ListingWebResponse itemL3ListingWebResponse = new ItemL3ListingWebResponse();

  @Mock
  private ProductV2Service productV2Service;

  @Mock
  private ProductService productService;

  @Mock
  private UserPicService userPicService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private ProductWrapperV2ServiceImpl productWrapperV2Service;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productWrapperV2Service, "multiPickupPointEnabled", false);
    productLevel3ListingWebResponse.setItemSummary(itemL3ListingWebResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productV2Service);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void updateItemListingTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(PRODUCT_SKU);
    productWrapperV2Service.updateItemListing(PRODUCT_SKU, Collections.emptyList());
    Mockito.verify(this.userPicService).validateUserPicPickupPoints(any());
    Mockito.verify(this.productService).updateItemListing(eq(PRODUCT_SKU), Mockito.any());
    Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  public void updateItemListing_switchOnTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(PRODUCT_SKU);
    ReflectionTestUtils.setField(productWrapperV2Service, "multiPickupPointEnabled", true);
    productWrapperV2Service.updateItemListing(PRODUCT_SKU, Collections.emptyList());
    Mockito.verify(this.userPicService).validateUserPicPickupPoints(any());
    Mockito.verify(this.productV2Service).updateItemListing(eq(PRODUCT_SKU),
      Mockito.any(), eq(false));
    Mockito.verify(mandatoryParameterHelper).isExternalOnly();
    Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  public void getProductL3ListingTest() {
    ReflectionTestUtils.setField(productWrapperV2Service, "multiPickupPointEnabled", true);
    productSummaryV2WebRequest.setKeyword(PRODUCT_SKU_1);
    productWrapperV2Service.getProductL3Listing(productSummaryV2WebRequest, PAGE, SIZE, false);
    Mockito.verify(this.productV2Service).getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  public void getProductL3ListingKeywordNullTest() {
    ReflectionTestUtils.setField(productWrapperV2Service, "multiPickupPointEnabled", true);
    productSummaryV2WebRequest.setKeyword(null);
    productWrapperV2Service.getProductL3Listing(productSummaryV2WebRequest, PAGE, SIZE, false);
    Mockito.verify(this.productV2Service).getProductL3List(productSummaryV2WebRequest, PAGE, SIZE, false);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }
}
