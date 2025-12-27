package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableSet;

public class ListingServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "itemSku-1";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_NAME = "itemName";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final int PAGE = 0;
  private static final int PAGE_SIZE = 1;
  private static final String PICKUP_POINT_NAME = "pickupPointName";
  private static final String MAIN_IMAGE_URL = "mainImageUrl";

  @InjectMocks
  private ListingServiceImpl listingService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemPriceService itemPriceService;

  private ItemPickupPoint itemPickupPoint;
  private Item item;
  private Product product;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint;
  private CategoryResponse categoryResponse;
  private ProductSummaryResponseV2Vo productSummaryResponseV2Vo;
  private ItemRequestV2 itemRequestV2;
  private ItemPickupPointListingRequest itemPickupPointListingRequest;
  private ItemAndItemPickupPointVo itemAndItemPickupPointVo;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPrice(ImmutableSet.of(new Price()));
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig()));

    product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setSynchronized(true);

    ProductScore productScore = new ProductScore();
    productScore.setTotalScore(10.0);
    product.setProductScore(productScore);

    item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    item.setCategoryCode(CATEGORY_CODE);
    item.setMainImageUrl(MAIN_IMAGE_URL);

    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();

    categoryResponse = new CategoryResponse();
    categoryResponse.setName(CATEGORY_NAME);

    productSummaryResponseV2Vo = new ProductSummaryResponseV2Vo();
    productSummaryResponseV2Vo.setCategoryCode(CATEGORY_CODE);
    itemRequestV2 = new ItemRequestV2();
    itemRequestV2.setMerchantCode(MERCHANT_CODE);
    itemRequestV2.setItemSkuList(new ArrayList<>());
    itemRequestV2.getItemSkuList().add(ITEM_SKU);
    itemPickupPointListingRequest =
        ItemPickupPointListingRequest.builder().businessPartnerCode(MERCHANT_CODE).productSku(PRODUCT_SKU)
            .itemSku(ITEM_SKU).keyword(ITEM_SKU).build();

    itemAndItemPickupPointVo=new ItemAndItemPickupPointVo();
    item.setPickupPointCode(PICKUP_POINT_CODE);
    itemAndItemPickupPointVo.setItem(Collections.singletonList(item));
    itemAndItemPickupPointVo.setItemPickupPoints(Collections.singletonList(itemPickupPoint));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(itemPickupPointService, itemService, productService, productHelperService,
        businessPartnerPickupPointService, systemParameterService);
  }

  @Test
  public void getItemPickupPointSummaryTest() throws Exception {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(category);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(masterCatalog);
    product.setMasterDataProduct(masterDataProduct);
    product.setSynchronized(true);
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    itemPickupPointSummaryRequest.setItemSku(ITEM_SKU);
    itemPickupPointSummaryRequest.setItemCodes(ImmutableSet.of(ITEM_CODE));

    Mockito.when(
            itemService.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE), null))
        .thenReturn(new HashSet<>());
    Mockito.when(
            itemPickupPointService.findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));
    Mockito.when(masterDataConstructorService.constructProductAndItemWithMasterData(Mockito.eq(STORE_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(Product.class),
        Mockito.anyList())).thenReturn(new ProductAndItemsVO());

    listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null);

    Mockito.verify(itemService)
        .getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, ImmutableSet.of(ITEM_CODE), null);
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet());
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
    Mockito.verify(masterDataConstructorService).constructProductAndItemWithMasterData(Mockito.eq(STORE_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(Product.class),
        Mockito.anyList());
  }

  @Test
  public void getItemPickupPointSummaryUsePcbDataFalseTest() throws Exception {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    itemPickupPointSummaryRequest.setKeyword(ITEM_NAME);
    product.setCategoryCode(CATEGORY_CODE);

    Mockito.when(
            itemService.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, null, ITEM_NAME))
        .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(
            itemPickupPointService.findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint),PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));

    listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null);

    Mockito.verify(
            itemService).getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, null, ITEM_NAME);
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet());
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getItemPickupPointSummaryFbbRequestNoDataTest() throws Exception {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    itemPickupPointSummaryRequest.setKeyword(ITEM_NAME);

    Mockito.when(
            itemService.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, null, ITEM_NAME))
        .thenReturn(new HashSet<>());

    listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null);

    Mockito.verify(
        itemService).getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(STORE_ID, MERCHANT_CODE, null, ITEM_NAME);
  }

  @Test
  public void getItemPickupPointSummarySyncFalseTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "usePcbMasterData", false);
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(
            itemPickupPointService.findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));

    listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet());
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getItemPickupPointSummaryEmptyTest() throws Exception {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    Mockito.when(
            itemPickupPointService.findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));

    listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointForSummaryListing(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ItemPickupPointSummaryRequest.class), Mockito.anySet());
  }

  @Test
  public void getHalalDashboardProductsResponseTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    HalalDashboardProductsResponseVo halalDashboardProductsResponseVo = new HalalDashboardProductsResponseVo();
    Mockito.when(productSearchService.getHalalDashboardProducts(STORE_ID, 0, 1, halalProductsFilterRequest))
        .thenReturn(new PageImpl<>(List.of(halalDashboardProductsResponseVo), PageRequest.of(0, 1), 1));
    Page<HalalDashboardProductsResponse> halalDashboardProductsResponsePage =
        listingService.getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest);
    Mockito.verify(productSearchService).getHalalDashboardProducts(STORE_ID, 0, 1, halalProductsFilterRequest);
   Assertions.assertEquals(1, halalDashboardProductsResponsePage.getTotalElements());
  }

  @Test
  public void getHalalDashboardProductsEmptyResponseTest() throws Exception {
    HalalProductsFilterRequest halalProductsFilterRequest = new HalalProductsFilterRequest();
    HalalDashboardProductsResponseVo halalDashboardProductsResponseVo = new HalalDashboardProductsResponseVo();
    Mockito.when(productSearchService.getHalalDashboardProducts(STORE_ID, 0, 1, halalProductsFilterRequest))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));
    Page<HalalDashboardProductsResponse> halalDashboardProductsResponsePage =
        listingService.getHalalDashboardProductsResponses(STORE_ID, 0, 1, halalProductsFilterRequest);
    Mockito.verify(productSearchService).getHalalDashboardProducts(STORE_ID, 0, 1, halalProductsFilterRequest);
   Assertions.assertEquals(0, halalDashboardProductsResponsePage.getTotalElements());
  }

  @Test
  public void getProductSummaryTest() throws Exception {
    ProductSummaryRequestV2 productSummaryRequestV2 = new ProductSummaryRequestV2();
    productSummaryRequestV2.setMerchantCode(MERCHANT_CODE);

    Mockito.when(
            productSearchService.getProductSummaryResponseV2(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ProductSummaryRequestV2.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(productSummaryResponseV2Vo), PageRequest.of(0, 1)
          , 1));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));

    listingService.getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2);

    Mockito.verify(productSearchService)
        .getProductSummaryResponseV2(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ProductSummaryRequestV2.class));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getProductSummaryEmptyTest() throws Exception {
    ProductSummaryRequestV2 productSummaryRequestV2 = new ProductSummaryRequestV2();
    productSummaryRequestV2.setMerchantCode(MERCHANT_CODE);
    Mockito.when(
            productSearchService.getProductSummaryResponseV2(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
                Mockito.any(ProductSummaryRequestV2.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));

    listingService.getProductSummary(STORE_ID, 0, 1, productSummaryRequestV2);

    Mockito.verify(productSearchService)
        .getProductSummaryResponseV2(Mockito.eq(STORE_ID), Mockito.eq(0), Mockito.eq(1),
            Mockito.any(ProductSummaryRequestV2.class));
  }

  @Test
  public void getItemPickupPointsByItemSkuTest() throws Exception {
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    product.setSynchronized(true);
    product.setMasterDataProduct(masterDataProduct);
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));
    Mockito.when(masterDataConstructorService.constructProductAndItemWithMasterData(Mockito.eq(STORE_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(Product.class),
        Mockito.anyList())).thenReturn(new ProductAndItemsVO());

    listingService.getItemPickupPointsByItemSku(STORE_ID, PAGE, PAGE_SIZE, itemRequestV2, false);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
    Mockito.verify(masterDataConstructorService).constructProductAndItemWithMasterData(Mockito.eq(STORE_ID),
        Mockito.eq(Constants.DEFAULT_USERNAME), Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.any(Product.class),
        Mockito.anyList());
  }

  @Test
  public void getItemPickupPointsByItemSkuUsePcbDataFalseTest() throws Exception {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));

    listingService.getItemPickupPointsByItemSku(STORE_ID, PAGE, PAGE_SIZE, itemRequestV2, false);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getItemPickupPointsByItemSkuSyncFalseTest() throws Exception {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setMerchantCode(MERCHANT_CODE);
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    Mockito.when(productHelperService.getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, Arrays.asList(CATEGORY_CODE)))
        .thenReturn(Arrays.asList(Arrays.asList(categoryResponse)));

    listingService.getItemPickupPointsByItemSku(STORE_ID, PAGE, PAGE_SIZE, itemRequestV2, false);

    Mockito.verify(itemPickupPointService)
        .findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(productHelperService)
        .getCategoryResponseListByCategoryCodesForProducts(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getItemPickupPointsByItemSkuEmptyTest() throws Exception {
    itemRequestV2.setItemSkuList(null);

    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  listingService.getItemPickupPointsByItemSku(STORE_ID, PAGE, PAGE_SIZE, itemRequestV2,
        false));
  }

  @Test
  public void getItemPickupPointListingTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "replaceOfferPriceWithMerchantPromoPriceInSellerSide", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    Map<String, Set<Price>> itemPickupPointPriceMap = new HashMap<>();
    itemPickupPointPriceMap.put(ITEM_SKU, new HashSet<>());
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
        Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
      .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU))
      .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
      .thenReturn(Arrays.asList(item));
    Mockito.when(itemPriceService.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint)))
      .thenReturn(itemPickupPointPriceMap);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
      businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
      itemPickupPointListingRequest);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemPriceService).getOriginalSellingPrice(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemPickupPointService)
      .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
        Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService)
      .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
      .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithNoL5Test() throws Exception {
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithFetchL4BasedOnProductSkuAndItemSkuTest() {
    ReflectionTestUtils.setField(listingService, "fetchItemByProductSkuAndItemSku", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    itemPickupPointListingRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithFetchL4BasedOnProductSkuAndItemSkuEmptyProductTest() {
    ReflectionTestUtils.setField(listingService, "fetchItemByProductSkuAndItemSku", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    itemPickupPointListingRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithFetchL4BasedOnProductSkuAndItemEmptyProductTest() {
    ReflectionTestUtils.setField(listingService, "fetchItemByProductSkuAndItemSku", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    itemPickupPointListingRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    item.setProductSku(ITEM_CODE);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithFetchL4ItemNullTest() {
    ReflectionTestUtils.setField(listingService, "fetchItemByProductSkuAndItemSku", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    itemPickupPointListingRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    Mockito.when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithNoL5TestForceReviewTrue() throws Exception {
    product.setForceReview(true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 0));
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }


  @Test
  public void getItemPickupPointListing_withMissingItemSkuTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "replaceOfferPriceWithMerchantPromoPriceInSellerSide", true);
    Map<String, Set<Price>> itemPickupPointPriceMap = new HashMap<>();
    itemPickupPointPriceMap.put(ITEM_SKU + Constants.DASH + PICKUP_POINT_CODE, new HashSet<>());
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
        Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
      .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU))
      .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
      .thenReturn(Arrays.asList(item));
    Mockito.when(itemPriceService.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint)))
      .thenReturn(itemPickupPointPriceMap);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
      .thenReturn(Arrays.asList(product));
    Mockito.when(
      businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
      itemPickupPointListingRequest);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint));
    Mockito.verify(itemPickupPointService)
      .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
        Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService)
      .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
      .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithItemSkuAndKeywordTest() throws Exception {
    itemPickupPointListingRequest.setItemSku(null);
    Mockito.when(itemService.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU)).thenReturn(new HashSet<>());

    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT, itemPickupPointListingRequest);

    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingWithItemSkuAndKeywordEmptyTest() throws Exception {
    itemPickupPointListingRequest.setItemSku(null);
    itemPickupPointListingRequest.setKeyword(null);
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));

    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT, itemPickupPointListingRequest);

    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE),
            Mockito.any(ItemPickupPointListingRequestVo.class));
  }

  @Test
  public void getItemL5Listing_Test() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID),
            Mockito.eq(new ArrayList<>()), Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
            Mockito.eq(STORE_ID), Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), null, PAGE, PAGE_SIZE,false, false);
    Mockito.verify(itemService)
        .getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
          Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5ListingGetOnlyBundleVariants_Test() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    itemAndItemPickupPointVo.getItem().get(0).setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID),
            Mockito.eq(new ArrayList<>()), Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
            Mockito.eq(STORE_ID), Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), null, PAGE, PAGE_SIZE,false, true);

    Mockito.verify(itemService)
        .getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5ListingGetOnlyBundleVariantsAndRecipeDontExists_Test() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID),
            Mockito.eq(new ArrayList<>()), Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
            Mockito.eq(STORE_ID), Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), null, PAGE, PAGE_SIZE,false, true);

    Mockito.verify(itemService)
        .getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5Listing_priceNull_Test() throws Exception {
    itemPickupPoint.setPrice(new HashSet<>());
    ItemAndItemPickupPointVo itemAndItemPickupPointVo1 = new ItemAndItemPickupPointVo();
    itemAndItemPickupPointVo1.setItem(Collections.singletonList(item));
    itemAndItemPickupPointVo1.setItemPickupPoints(Collections.singletonList(itemPickupPoint));
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID),
            Mockito.eq(new ArrayList<>()), Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE)))
        .thenReturn(itemAndItemPickupPointVo1);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
            Mockito.eq(STORE_ID), Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), null, PAGE, PAGE_SIZE, false, false);
    Mockito.verify(itemService)
        .getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
          Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5ListingEmptyTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID),
            Mockito.eq(new ArrayList<>()), Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE)))
        .thenReturn(null);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
            Mockito.eq(STORE_ID), Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    Page<ItemL5ListingResponse> responses =  listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), null, PAGE, PAGE_SIZE,false, false);
    Mockito.verify(itemService)
        .getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(null), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

   Assertions.assertEquals(0,responses.getTotalElements());

  }

  @Test
  public void getItemL5ListingPageSizeAndCncNullTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getAllItemAndPickupPointDetailsWithoutPagination(Mockito.eq(STORE_ID),
        Mockito.eq(new ArrayList<>()), Mockito.eq(new ArrayList<>()))).thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), new ArrayList<>(), null, null, null, false);
    Mockito.verify(itemService)
        .getAllItemAndPickupPointDetailsWithoutPagination(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(new ArrayList<>()));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5ListingPageSizeNonNullAndCncNullTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(new ArrayList<>()), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));

    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), new ArrayList<>(), PAGE, PAGE_SIZE, null, false);
    Mockito.verify(itemService).getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
        Mockito.eq(new ArrayList<>()), Mockito.eq(false), Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE));

    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE)));

  }

  @Test
  public void getItemL5ListingPageAndSizeNullAndCncNonNullTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(new ArrayList<>()), Mockito.eq(true), Mockito.eq(null), Mockito.eq(null)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));
    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), new ArrayList<>(), null, null, true, false);
  }

  @Test
  public void getItemL5ListingPageNullAndSizeNonNullAndCncNonNullTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    Mockito.when(itemService.getItemAndPickupPointDetailsForCnc(Mockito.eq(STORE_ID), Mockito.eq(new ArrayList<>()),
            Mockito.eq(new ArrayList<>()), Mockito.eq(true), Mockito.eq(null), Mockito.eq(null)))
        .thenReturn(itemAndItemPickupPointVo);
    Mockito.when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Mockito.eq(STORE_ID),
            Mockito.eq(Collections.singletonList(PICKUP_POINT_CODE))))
        .thenReturn(Collections.singletonList(businessPartnerPickupPoint));
    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), new ArrayList<>(), null, PAGE_SIZE, true, false);
  }

  @Test
  public void getItemL5ListingPageSizeNullAndCncNonNullTest() throws Exception {
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    listingService.getItemL5Listing(STORE_ID, new ArrayList<>(), new ArrayList<>(), PAGE, null, true, false);
  }

  @Test
  public void getItemPickupPointSummaryInvalidTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "validateSummaryRequest", true);
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest =
      new ItemPickupPointSummaryRequest();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> listingService.getItemPickupPointSummary(STORE_ID, 0, 1, itemPickupPointSummaryRequest, null));
  }

  @Test
  public void getItemPickupPointListingReplaceOfferPriceWithMerchantPromoPriceInSellerSideFalseTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "replaceOfferPriceWithMerchantPromoPriceInSellerSide", false);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    Map<String, Set<Price>> itemPickupPointPriceMap = new HashMap<>();
    itemPickupPointPriceMap.put(ITEM_SKU, new HashSet<>());
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU))
        .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemPriceService.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint)))
        .thenReturn(itemPickupPointPriceMap);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getItemPickupPointListingReplaceOfferPriceWithItemFetchedByProductSkuAndItemSKuTest() throws Exception {
    ReflectionTestUtils.setField(listingService, "replaceOfferPriceWithMerchantPromoPriceInSellerSide", false);
    ReflectionTestUtils.setField(listingService, "fetchItemByProductSkuAndItemSku", true);
    itemPickupPointListingRequest.setResponseWithoutPickupPoint(true);
    Map<String, Set<Price>> itemPickupPointPriceMap = new HashMap<>();
    itemPickupPointPriceMap.put(ITEM_SKU, new HashSet<>());
    Mockito.when(itemPickupPointService.getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID),
            Mockito.eq(PAGE), Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(itemPickupPoint), PageRequest.of(0, 1), 1));
    Mockito.when(itemService.getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU))
        .thenReturn(ImmutableSet.of(ITEM_SKU));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemPriceService.getDiscountItemPickupPoints(Arrays.asList(itemPickupPoint)))
        .thenReturn(itemPickupPointPriceMap);
    Mockito.when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    listingService.getItemPickupPointListing(STORE_ID, PAGE, PAGE_SIZE, Constants.DEFAULT,
        itemPickupPointListingRequest);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointListingByProductSku(Mockito.eq(STORE_ID), Mockito.eq(PAGE),
            Mockito.eq(PAGE_SIZE), Mockito.any(ItemPickupPointListingRequestVo.class));
    Mockito.verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    Mockito.verify(productService)
        .findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(businessPartnerPickupPointService)
        .getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, Arrays.asList(PICKUP_POINT_CODE));
    Mockito.verify(itemService).getItemSkuByItemNameKeyword(STORE_ID, PRODUCT_SKU, ITEM_SKU);
  }

}
