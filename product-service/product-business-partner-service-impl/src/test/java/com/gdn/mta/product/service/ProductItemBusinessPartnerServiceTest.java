package com.gdn.mta.product.service;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ProductItemBusinessPartnerServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_ID = "productId";
  private static final String SKU_CODE = "skuCode";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String ITEM_SKU = "itemSku";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "productBusinessPartnerId";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String USER_NAME = "user-name";
  private static final String ID1 = "id1";
  private static final String ID2 = "id2";
  private static final String ITEM_SKU1 = "itemSku1";
  private static final String ITEM_SKU2 = "itemSku2";
  private static final String PP_CODE1 = "ppCode1";
  private static final String PP_CODE2 = "ppCode2";


  @InjectMocks
  private ProductItemBusinessPartnerServiceImpl productItemBusinessPartnerService;

  @Mock
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Mock
  private ProductItemBusinessPartnerCustomRepository productItemBusinessPartnerCustomRepository;

  private Map<String, String> skuCodesAndProductItemIdsMap;
  private List<ProductItemBusinessPartner> productItemBusinessPartnerList;
  private ProductItemBusinessPartner productItemBusinessPartner;
  private ProductItemBusinessPartner productItemBusinessPartner1;
  private ProductItemBusinessPartner productItemBusinessPartner2;

  @Captor
  private ArgumentCaptor<List<String>> productItemIds;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    skuCodesAndProductItemIdsMap = new HashMap<>();
    skuCodesAndProductItemIdsMap.put(SKU_CODE, PRODUCT_ITEM_ID);
    productItemBusinessPartnerList = new ArrayList<>();
    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setId(ID1);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartnerList.add(productItemBusinessPartner);
    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setId(ID1);
    productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setId(ID1);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU1);
    productItemBusinessPartner1.setPickupPointId(PP_CODE1);
    productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setId(ID2);
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU2);
    productItemBusinessPartner2.setPickupPointId(PP_CODE2);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productItemBusinessPartnerRepository);
  }

  @Test
  public void findReviewPendingProductCodeByItemSku() {
    Mockito.when(productItemBusinessPartnerRepository.findProductCodeByItemSku(ITEM_SKU)).thenReturn(PRODUCT_CODE);
    String response =
        productItemBusinessPartnerService.findReviewPendingProductCodeByItemSku(ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerRepository).findProductCodeByItemSku(ITEM_SKU);
    Assertions.assertEquals(PRODUCT_CODE, response);
  }

  @Test
  public void getSkuCodesAndItemSkusMapTest() {
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(
        eq(STORE_ID), any())).thenReturn(productItemBusinessPartnerList);
    Map<String, String> response = productItemBusinessPartnerService
        .getSkuCodesAndItemSkusMap(STORE_ID, skuCodesAndProductItemIdsMap);
    Mockito.verify(productItemBusinessPartnerRepository)
        .findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(eq(STORE_ID), productItemIds.capture());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemIds.getValue().get(0));
    Assertions.assertEquals(ITEM_SKU, response.get(SKU_CODE));
  }

  @Test
  public void getSkuCodesAndItemSkusMapWithProductItemBusinessPartnerListNullTest() {
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(
        eq(STORE_ID), any())).thenReturn(null);
    Map<String, String> response = productItemBusinessPartnerService
        .getSkuCodesAndItemSkusMap(STORE_ID, skuCodesAndProductItemIdsMap);
    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(eq(STORE_ID), any());
    assertTrue(MapUtils.isEmpty(response));
  }

  @Test
  public void getSkuCodesAndItemSkusMapWithNullMapTest() {
    Map<String, String> response = productItemBusinessPartnerService.getSkuCodesAndItemSkusMap(STORE_ID, null);
    assertTrue(MapUtils.isEmpty(response));
  }

  @Test
  public void findProductItemByProductItemIdTest() {
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ITEM_ID))
        .thenReturn(productItemBusinessPartnerList);
    List<ProductItemBusinessPartner> productItemBusinessPartnes =
        productItemBusinessPartnerService.findProductItemByProductItemId(STORE_ID, PRODUCT_ITEM_ID);
    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndProductItemIdAndMarkForDeleteFalse(STORE_ID, PRODUCT_ITEM_ID);
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartnes.get(0).getProductItemId());
  }

  @Test
  public void findProductItemByItemSkuTest() {
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(STORE_ID, SKU_CODE))
        .thenReturn(productItemBusinessPartnerList.get(0));
    ProductItemBusinessPartner productItemBusinessPartner =
        productItemBusinessPartnerService.findProductItemByItemSku(STORE_ID, SKU_CODE);
    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(STORE_ID, SKU_CODE);
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner.getProductItemId());
  }

  @Test
  public void findProductItemByItemSkuStoreIdNullTest() {
    Assertions.assertThrows(Exception.class, () -> {
      productItemBusinessPartnerService.findProductItemByItemSku(null, SKU_CODE);
    });
  }

  @Test
  public void findProductItemByItemSkuNullItemSkuTest() {
    Assertions.assertThrows(Exception.class, () -> {
      productItemBusinessPartnerService.findProductItemByItemSku(STORE_ID, null);
    });
  }

  @Test
  public void updateMerchantSkuByStoreIdAndItemSkuTest() throws Exception {
    productItemBusinessPartnerService.updateMerchantSkuByStoreIdAndItemSku(STORE_ID, ITEM_SKU, MERCHANT_SKU);
    Mockito.verify(productItemBusinessPartnerRepository).updateMerchantSkuByStoreIdAndItemSku(STORE_ID, ITEM_SKU, MERCHANT_SKU);
  }

  @Test
  public void updateMerchantSkuByStoreIdAndItemSkuStoreIdNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> {
      productItemBusinessPartnerService.updateMerchantSkuByStoreIdAndItemSku(null, ITEM_SKU, MERCHANT_SKU);
    });
  }

  @Test
  public void updateMerchantSkuByStoreIdAndItemSkuItemSkuNullTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> {
      productItemBusinessPartnerService.updateMerchantSkuByStoreIdAndItemSku(null, ITEM_SKU, MERCHANT_SKU);
    });
  }

  @Test
  public void updateMerchantSkuByStoreIdAndItemSkuMerchantSkuNullTest() throws Exception {
    productItemBusinessPartnerService.updateMerchantSkuByStoreIdAndItemSku(STORE_ID, ITEM_SKU, null);
    Mockito.verify(productItemBusinessPartnerRepository)
        .updateMerchantSkuByStoreIdAndItemSku(STORE_ID, ITEM_SKU, StringUtils.EMPTY);
  }

  @Test
  public void getProductItemBusinessPartnerForL5ListingTest() {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Mockito.when(productItemBusinessPartnerCustomRepository.findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(STORE_ID,
            PRODUCT_BUSINESS_PARTNER_ID, itemPickupPointListingL3Request.getItemSku(),
            itemPickupPointListingL3Request.getPickupPointCodes(), PageRequest.of(0, 1), false))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID,
        0, 1, itemPickupPointListingL3Request);
    Mockito.verify(productItemBusinessPartnerCustomRepository)
        .findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(STORE_ID, PRODUCT_BUSINESS_PARTNER_ID,
            itemPickupPointListingL3Request.getItemSku(), itemPickupPointListingL3Request.getPickupPointCodes(),
            PageRequest.of(0, 1), false);
  }

  @Test
  public void getProductItemBusinessPartnerForL5ListingIdNullTest() {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(STORE_ID, null, 0, 1,
          itemPickupPointListingL3Request);
    });
  }

  @Test
  public void getProductItemBusinessPartnerByItemSkuListTest() {
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID,
        Arrays.asList(ITEM_SKU))).thenReturn(productItemBusinessPartnerList);
    productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(STORE_ID, Arrays.asList(ITEM_SKU));
    Mockito.verify(productItemBusinessPartnerRepository)
        .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(ITEM_SKU));
  }

  @Test
  public void getProductItemBusinessPartnerByItemSkuListEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(StringUtils.EMPTY,
          Arrays.asList(ITEM_SKU));
    });
  }

  @Test
  public void getProductItemBusinessPartnerByItemSkuListEmptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(STORE_ID, new ArrayList<>());
    });
  }

  @Test
  public void getProductItemCountByItemSkuTest() {
    Mockito.when(productItemBusinessPartnerRepository
      .findFirst2ByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
      .thenReturn(productItemBusinessPartnerList);
    int result = productItemBusinessPartnerService.getProductItemCountByItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productItemBusinessPartnerRepository)
      .findFirst2ByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Assertions.assertEquals(result, productItemBusinessPartnerList.size());
  }

  @Test
  public void deleteItemSkuForPickupPointDeleteTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USER_NAME);
    productItemBusinessPartnerService.deleteItemsPickupPointDelete(STORE_ID, PICKUP_POINT_CODE,
      Collections.singletonList(ITEM_SKU));
    Mockito.verify(productItemBusinessPartnerRepository)
      .updateMarkForDeleteByStoreIdAndItemSkuInAndPickupPointId(USER_NAME, STORE_ID,
        Collections.singletonList(ITEM_SKU), PICKUP_POINT_CODE);
  }


  @Test
  public void saveTest() {
    productItemBusinessPartnerService.saveAll(productItemBusinessPartnerList);
    Mockito.verify(productItemBusinessPartnerRepository).saveAll(productItemBusinessPartnerList);
  }

  @Test
  public void replaceNewWithExistingProductItemBusinessPartnerTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.replaceNewWithExistingProductItemBusinessPartner(Arrays.asList(
            productItemBusinessPartner), Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2));
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(0), productItemBusinessPartner);
    Assertions.assertEquals(productItemBusinessPartnerList.get(1), productItemBusinessPartner2);
  }

  @Test
  public void replaceNewWithExistingProductItemBusinessPartnerNewNullTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.replaceNewWithExistingProductItemBusinessPartner(Arrays.asList(
                productItemBusinessPartner), null);
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 0);
  }

  @Test
  public void replaceNewWithExistingProductItemBusinessPartnerExistingNullTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.replaceNewWithExistingProductItemBusinessPartner(null,
            Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2));
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(0), productItemBusinessPartner1);
    Assertions.assertEquals(productItemBusinessPartnerList.get(1), productItemBusinessPartner2);
  }

  @Test
  public void mergeProductItemBusinessPartnerListsTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.mergeProductItemBusinessPartnerLists(Arrays.asList(
            productItemBusinessPartner,
            productItemBusinessPartner2), Arrays.asList(productItemBusinessPartner1));
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(0), productItemBusinessPartner2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(1), productItemBusinessPartner1);
  }

  @Test
  public void mergeProductItemBusinessPartnerListsNewNullTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.mergeProductItemBusinessPartnerLists(Arrays.asList(
            productItemBusinessPartner,
            productItemBusinessPartner2), null);
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(0), productItemBusinessPartner);
    Assertions.assertEquals(productItemBusinessPartnerList.get(1), productItemBusinessPartner2);
  }

  @Test
  public void mergeProductItemBusinessPartnerListsExisitingNullTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.mergeProductItemBusinessPartnerLists(null,
            Arrays.asList(productItemBusinessPartner, productItemBusinessPartner2));
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 2);
    Assertions.assertEquals(productItemBusinessPartnerList.get(0), productItemBusinessPartner);
    Assertions.assertEquals(productItemBusinessPartnerList.get(1), productItemBusinessPartner2);
  }

  @Test
  public void mergeProductItemBusinessPartnerListsNullListisTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartnerList =
        productItemBusinessPartnerService.mergeProductItemBusinessPartnerLists(null, null);
    Assertions.assertEquals(productItemBusinessPartnerList.size(), 0);
  }

  @Test
  public void saveForAListOfItemsTest() {
    productItemBusinessPartnerService.save(productItemBusinessPartnerList);
    Mockito.verify(productItemBusinessPartnerRepository).saveAll(productItemBusinessPartnerList);
  }

  @Test
  public void  getItemSkuAndItemCodeMappingUsingItemIdsTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductItemId(ID1);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    Map<String, String> itemIdAndSkuCodeMap = ImmutableMap.of(ID1, SKU_CODE);

    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(STORE_ID, ImmutableSet.of(ID1))).thenReturn(Arrays.asList(productItemBusinessPartner));

    Map<String, String> itemSkuAndItemCodeMap = productItemBusinessPartnerService.getItemSkuAndItemCodeMappingUsingItemIds(STORE_ID, itemIdAndSkuCodeMap);

    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndProductItemIdIn(STORE_ID, ImmutableSet.of(ID1));

    Assertions.assertEquals(1, itemSkuAndItemCodeMap.size());
    Assertions.assertTrue(itemSkuAndItemCodeMap.keySet().contains(ITEM_SKU));
    Assertions.assertEquals(SKU_CODE, itemSkuAndItemCodeMap.get(ITEM_SKU));
  }

  @Test
  public void  getItemSkuAndItemCodeMappingUsingItemIdsNoItemFoundTest() {
    Map<String, String> itemIdAndSkuCodeMap = ImmutableMap.of(ID1, SKU_CODE);

    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(STORE_ID, ImmutableSet.of(ID1))).thenReturn(new ArrayList<>());

    Map<String, String> itemSkuAndItemCodeMap = productItemBusinessPartnerService.getItemSkuAndItemCodeMappingUsingItemIds(STORE_ID, itemIdAndSkuCodeMap);

    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndProductItemIdIn(STORE_ID, ImmutableSet.of(ID1));

    Assertions.assertEquals(0, itemSkuAndItemCodeMap.size());
   }

  @Test
  public void  getItemSkuAndItemCodeMappingUsingItemIdsEmptyMapTest() {

    Map<String, String> itemSkuAndItemCodeMap = productItemBusinessPartnerService.getItemSkuAndItemCodeMappingUsingItemIds(STORE_ID, new HashMap<>());

    Assertions.assertEquals(0, itemSkuAndItemCodeMap.size());
  }

  @Test
  public void deleteProductItemBusinessPartnerByStoreIdAndProductIdTest() {
    productItemBusinessPartnerService.deleteProductItemBusinessPartnerByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productItemBusinessPartnerRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void getDeletedVariantItemSkusTest() {
    ReflectionTestUtils.setField(productItemBusinessPartnerService, "deleteExtraVariantsFromXproduct", true);
    Map<String, String> extraDeletedItems = Map.of(PRODUCT_ITEM_ID, SKU_CODE);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Mockito.when(productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_ITEM_ID)))
        .thenReturn(Arrays.asList(productItemBusinessPartner));
    productItemBusinessPartnerService.getDeletedVariantItemSkus(productVariantUpdateRequest, extraDeletedItems);
    Mockito.verify(productItemBusinessPartnerRepository).findByStoreIdAndProductItemIdIn(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_ITEM_ID));
    Assertions.assertEquals(1, productVariantUpdateRequest.getDeletedProductItems().size());
    Assertions.assertEquals(ITEM_SKU, productVariantUpdateRequest.getDeletedProductItems().get(0).getItemSku());
    Assertions.assertEquals(SKU_CODE, productVariantUpdateRequest.getDeletedProductItems().get(0).getItemCode());
  }

  @Test
  public void getDeletedVariantItemSkusNoExtraDeletedItemsTest() {
    ReflectionTestUtils.setField(productItemBusinessPartnerService, "deleteExtraVariantsFromXproduct", true);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productItemBusinessPartnerService.getDeletedVariantItemSkus(productVariantUpdateRequest, new HashMap<>());
    Assertions.assertEquals(0, productVariantUpdateRequest.getDeletedProductItems().size());
  }

  @Test
  public void getDeletedVariantItemSkusSwitchOffTest() {
    ReflectionTestUtils.setField(productItemBusinessPartnerService, "deleteExtraVariantsFromXproduct", false);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    productItemBusinessPartnerService.getDeletedVariantItemSkus(productVariantUpdateRequest, new HashMap<>());
    Assertions.assertEquals(0, productVariantUpdateRequest.getDeletedProductItems().size());
  }

  @Test
  public void findProductItemByProductItemIdInTest() {
    productItemBusinessPartnerService.findProductItemByProductItemIdIn(STORE_ID,
        Collections.singletonList(PRODUCT_ITEM_ID));
    Mockito.verify(productItemBusinessPartnerRepository)
        .findByStoreIdAndProductItemIdIn(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID));
    Assertions.assertNotNull(PRODUCT_ITEM_ID);
  }
}
