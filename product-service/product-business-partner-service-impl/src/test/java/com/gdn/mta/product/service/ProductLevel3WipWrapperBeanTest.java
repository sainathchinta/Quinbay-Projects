package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3AttributeWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.util.ReflectionTestUtils;

public class ProductLevel3WipWrapperBeanTest {

  @InjectMocks
  private ProductLevel3WipWrapperBean productLevel3WipWrapperBean;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private MapperUtil mapperUtil;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  private ProductCollection productCollection;
  private Map<String, List<ProductItemWholesalePriceResponse>> itemSkuToWholesalePriceMap;
  private List<ProductItemWholesalePriceResponse> wholesalePriceResponses;
  private List<ProductItemWholesalePrice> productItemWholesalePrices;
  private ProductLevel3Wip productLevel3Wip;
  private ProfileResponse profileResponse = new ProfileResponse();
  private static final String DEFAULT_PRODUCT_ID = "BLI-00001";
  private static final String NOTES = "notes";
  private static final String GDN_SKU_2 = "BLI-00001-00002-00001";
  private static final String WHOLESALE_RULE_1 = "[{\"quantity\":3,\"wholesaleDiscount\":30.0"
          + "},{\"quantity\":5,\"wholesaleDiscount\":40.0}]";
  private static final String GDN_SKU_1 = "BLI-00001-00001-00001";
  private static final String VALUE = "VALUE";
  private static final String STORE_ID = "10001";
  private static final String MERCHANT_DELIVERY_TYPE = "merchantDeliveryType";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productCollection = new ProductCollection();
    productLevel3Wip = new ProductLevel3Wip();
    productLevel3Wip.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3Wip.setProductLevel1Id(DEFAULT_PRODUCT_ID);
    productLevel3Wip.setAttributes(new ArrayList<>());
    productLevel3Wip.setItems(new ArrayList<>());
    ProductLevel3ItemWip productLevel3ItemWip = new ProductLevel3ItemWip();
    productLevel3ItemWip.setGdnSku(GDN_SKU_1);
    ProductLevel3ItemWip productLevel3ItemWip1 = new ProductLevel3ItemWip();
    productLevel3ItemWip1.setGdnSku(GDN_SKU_2);
    productLevel3Wip.setItems(Arrays.asList(productLevel3ItemWip, productLevel3ItemWip1));
    ProductLevel3AttributeWip productLevel3AttributeWip = new ProductLevel3AttributeWip();
    productLevel3AttributeWip.setValue(VALUE);
    productLevel3Wip.setAttributes(Arrays.asList(productLevel3AttributeWip));
    itemSkuToWholesalePriceMap = new HashMap<>();
    wholesalePriceResponses = new ArrayList<>();
    ProductItemWholesalePriceResponse productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();
    productItemWholesalePriceResponse.setQuantity(3);
    ProductItemWholesalePriceResponse productItemWholesalePriceResponse1 = new ProductItemWholesalePriceResponse();
    productItemWholesalePriceResponse1.setQuantity(5);
    wholesalePriceResponses.add(productItemWholesalePriceResponse);
    wholesalePriceResponses.add(productItemWholesalePriceResponse1);
    productItemWholesalePrices = new ArrayList<>();
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(GDN_SKU_1);
    productItemWholesalePrice.setWholesaleRules(WHOLESALE_RULE_1);
    productItemWholesalePrice.setWholesalePriceActivated(true);
    productItemWholesalePrices.add(productItemWholesalePrice);

    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantDeliveryType(MERCHANT_DELIVERY_TYPE);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(this.productLevel3LogisticsService
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE))
        .thenReturn(new ArrayList<>());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productItemWholesalePriceService, productLevel1CollectionService, mapperUtil);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productLevel3LogisticsService);
  }

  @Test
  public void findByProductActiveFalseTest() throws Exception {
    Mockito.when(productLevel3WipService.findByProductSku(DEFAULT_PRODUCT_ID, false)).thenReturn(productLevel3Wip);
    Mockito
        .when(productItemWholesalePriceService.findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2)))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(mapperUtil.mapStringToResponse(WHOLESALE_RULE_1)).thenReturn(wholesalePriceResponses);
    Mockito.when(productLevel1CollectionService.findByProductId(DEFAULT_PRODUCT_ID)).thenReturn(productCollection);
    ProductLevel3WipDetailResponse response =
        this.productLevel3WipWrapperBean.findByProductSku(STORE_ID, DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel3WipService).findByProductSku(DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel1CollectionService).findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(productItemWholesalePriceService).findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2));
    Mockito.verify(mapperUtil).mapStringToResponse(WHOLESALE_RULE_1);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3LogisticsService)
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getItems().size());
    Assertions.assertEquals(2, response.getItems().get(0).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(true, response.getItems().get(0).getWholesalePriceActivated());
    Assertions.assertEquals(3, response.getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
    Assertions.assertEquals(0, response.getItems().get(1).getProductItemWholesalePriceResponses().size());
  }

  @Test
  public void findByProductActiveFalseWithNotesTest() throws Exception {
    productCollection.setState(ProductLevel1State.NEED_CORRECTION);
    productCollection.setProductId(DEFAULT_PRODUCT_ID);
    Mockito.when(productLevel3WipService.findByProductSku(DEFAULT_PRODUCT_ID, false)).thenReturn(productLevel3Wip);
    Mockito
        .when(productItemWholesalePriceService.findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2)))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(mapperUtil.mapStringToResponse(WHOLESALE_RULE_1)).thenReturn(wholesalePriceResponses);
    Mockito.when(productLevel1CollectionService.findByProductId(DEFAULT_PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(productLevel1CollectionService.findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID))
        .thenReturn(NOTES);
    ProductLevel3WipDetailResponse response =
        this.productLevel3WipWrapperBean.findByProductSku(STORE_ID, DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel3WipService).findByProductSku(DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel1CollectionService).findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2));
    Mockito.verify(mapperUtil).mapStringToResponse(WHOLESALE_RULE_1);
    Mockito.verify(productLevel1CollectionService).findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3LogisticsService)
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getItems().size());
    Assertions.assertEquals(2, response.getItems().get(0).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(true, response.getItems().get(0).getWholesalePriceActivated());
    Assertions.assertEquals(3, response.getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
    Assertions.assertEquals(0, response.getItems().get(1).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(ProductLevel1State.NEED_CORRECTION, response.getState());
    Assertions.assertEquals(NOTES, response.getNotes());
  }

  @Test
  public void findByProductActiveFalseWithNullNotesTest() throws Exception {
    productCollection.setState(ProductLevel1State.NEED_CORRECTION);
    productCollection.setProductId(DEFAULT_PRODUCT_ID);
    Mockito.when(productLevel3WipService.findByProductSku(DEFAULT_PRODUCT_ID, false)).thenReturn(productLevel3Wip);
    Mockito
        .when(productItemWholesalePriceService.findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2)))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(mapperUtil.mapStringToResponse(WHOLESALE_RULE_1)).thenReturn(wholesalePriceResponses);
    Mockito.when(productLevel1CollectionService.findByProductId(DEFAULT_PRODUCT_ID)).thenReturn(productCollection);
    ProductLevel3WipDetailResponse response =
        this.productLevel3WipWrapperBean.findByProductSku(STORE_ID, DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel3WipService).findByProductSku(DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel1CollectionService).findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2));
    Mockito.verify(mapperUtil).mapStringToResponse(WHOLESALE_RULE_1);
    Mockito.verify(productLevel1CollectionService).findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3LogisticsService)
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getItems().size());
    Assertions.assertEquals(2, response.getItems().get(0).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(true, response.getItems().get(0).getWholesalePriceActivated());
    Assertions.assertEquals(3, response.getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
    Assertions.assertEquals(0, response.getItems().get(1).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(ProductLevel1State.NEED_CORRECTION, response.getState());
    Assertions.assertNull(response.getNotes());
  }

  @Test
  public void findByProductActiveFalseWithProductCollectionNulllTest() throws Exception {
    productCollection.setState(ProductLevel1State.NEED_CORRECTION);
    productCollection.setProductId(DEFAULT_PRODUCT_ID);
    Mockito.when(productLevel3WipService.findByProductSku(DEFAULT_PRODUCT_ID, false)).thenReturn(productLevel3Wip);
    Mockito
        .when(productItemWholesalePriceService.findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2)))
        .thenReturn(productItemWholesalePrices);
    Mockito.when(mapperUtil.mapStringToResponse(WHOLESALE_RULE_1)).thenReturn(wholesalePriceResponses);
    Mockito.when(productLevel1CollectionService.findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID))
        .thenReturn(NOTES);
    ProductLevel3WipDetailResponse response =
        this.productLevel3WipWrapperBean.findByProductSku(STORE_ID, DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel3WipService).findByProductSku(DEFAULT_PRODUCT_ID, false);
    Mockito.verify(productLevel1CollectionService).findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(STORE_ID, Arrays.asList(GDN_SKU_1, GDN_SKU_2));
    Mockito.verify(mapperUtil).mapStringToResponse(WHOLESALE_RULE_1);
    Mockito.verify(productLevel1CollectionService).findTop1ProductHistoryNotes(STORE_ID, DEFAULT_PRODUCT_ID);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3LogisticsService)
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getItems().size());
    Assertions.assertEquals(2, response.getItems().get(0).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(true, response.getItems().get(0).getWholesalePriceActivated());
    Assertions.assertEquals(3, response.getItems().get(0).getProductItemWholesalePriceResponses().get(0).getQuantity());
    Assertions.assertEquals(0, response.getItems().get(1).getProductItemWholesalePriceResponses().size());
    Assertions.assertEquals(NOTES, response.getNotes());
  }

  @Test
  public void findByProductActiveTrueTest() throws Exception {
    Mockito.when(productLevel3WipService.findByProductSku(DEFAULT_PRODUCT_ID, true)).thenReturn(productLevel3Wip);
    Mockito.when(productLevel1CollectionService.findByProductId(DEFAULT_PRODUCT_ID)).thenReturn(productCollection);
    ProductLevel3WipDetailResponse response =
        this.productLevel3WipWrapperBean.findByProductSku(STORE_ID, DEFAULT_PRODUCT_ID, true);
    Mockito.verify(productLevel3WipService).findByProductSku(DEFAULT_PRODUCT_ID, true);
    Mockito.verify(productLevel1CollectionService).findByProductId(DEFAULT_PRODUCT_ID);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3LogisticsService)
        .findLogisticsByItemSku(GDN_SKU_1, BUSINESS_PARTNER_CODE, MERCHANT_DELIVERY_TYPE);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(2, response.getItems().size());
    Assertions.assertEquals(0, response.getItems().get(1).getProductItemWholesalePriceResponses().size());
  }

  @Test
  public void countSummaryByFilterTypeTest() throws Exception {
    productLevel3WipWrapperBean.countSummaryByFilterType(BUSINESS_PARTNER_CODE,
      Constants.DEFAULT_STORE_ID, Constants.PRIMARY);
    Mockito.verify(productLevel3WipService).countSummaryByFilterType(BUSINESS_PARTNER_CODE,
      Constants.DEFAULT_STORE_ID, Constants.PRIMARY);
  }

  @Test
  public void countSummaryByFilterTypeSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3WipWrapperBean,
      "fetchProductLevel3CountFromCacheEnabled", true);
    productLevel3WipWrapperBean.countSummaryByFilterType(BUSINESS_PARTNER_CODE,
      Constants.DEFAULT_STORE_ID, Constants.PRIMARY);
    Mockito.verify(productLevel3WipService).countSummaryByFilterTypeCached(BUSINESS_PARTNER_CODE,
      Constants.DEFAULT_STORE_ID, Constants.PRIMARY);
  }

  @Test
  public void countSummaryWithStateTest() throws Exception {
    productLevel3WipWrapperBean.countSummaryWithState(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3WipService).countSummaryWithState(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void countSummaryWithStateFetchFromCacheTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3WipWrapperBean,
      "fetchProductLevel3CountFromCacheEnabled", true);
    productLevel3WipWrapperBean.countSummaryWithState(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3WipService).countSummaryWithStateCached(BUSINESS_PARTNER_CODE);
  }
}