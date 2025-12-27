package com.gdn.x.product.service.impl;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAllowedAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.ItemCategoryVOV2;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleProductVO;
import com.gdn.x.product.outbound.api.ProductAnalyticsOutbound;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;

public class ProductSearchHelperServiceImplTest {

  private static final String PRODUCT_CODE_1 = "product-code-1";

  private static final String ITEM_SKU_2 = "item-sku-2";

  private static final String PRODUCT_SKU_1 = "product-sku-1";

  private static final String ITEM_SKU_1 = "item-sku-1";

  private static final String ITEM_CODE_1 = "item-code-1";

  private static final String PRODUCT_CODE = "product-code";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String REQUESTID = "requestid";

  private static final String USERNAME = "username";

  private static final String STORE_ID = "store-id";

  private static final String PRODUCT_CATENTRY_ID = "product-catentry-id";

  private static final String USP = "product_usp";

  private static final String DEFAULT_PRODUCT_CODE = "DEFAULT_PRODUCT_CODE";

  private static final boolean IN_ALL_PRODUCTS = false;

  private static final String PREORDER_TYPE = "preOrderType";

  private static final Integer PREORDER_VALUE = 10;

  private static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private ProductSearchHelperServiceImpl productSearchHelperServiceImpl;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Captor
  private ArgumentCaptor<List<String>> listArgumentCaptor;

  private List<ProductAndItemSolr> solrResult;

  private List<Product> productResult;

  private Set<String> itemCodes;

  private Set<String> productCodes;

  private List<String> itemSkus;

  private Set<String> productSkus;

  private List<Product> products;

  private List<Item> items;

  private HashMap<String, MasterDataProduct> masterDataProduct;

  private HashMap<String, MasterDataItem> masterDataItem;

  private List<Item> itemLists;

  private Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse;

  private PreOrder preOrder;

  @Test
  public void getProductAndItemsByProductCodesTest() throws Exception {
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetail(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, false);
    verify(this.productHelperService).findAndConstructOfflineItems(ProductSearchHelperServiceImplTest.STORE_ID, this.items);
    verify(this.masterDataService).getMasterDataProductDetailResponse(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.productCodes, IN_ALL_PRODUCTS);
    verify(this.itemService).getItemsWithDiscountPriceByProductSkus(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus, true, false);
    verify(this.objectConverterService).convertAndValidateMasterDataExists(this.productResult,
        this.items, this.masterDataProduct, this.masterDataItem);
  }

  @Test
  public void getProductAndItemsByProductCodesForPVTest() throws Exception {
    productCodes.add("DEFAULT_PRODUCT_CODE");
    when(this.itemService
        .getItemsWithDiscountPriceByProductSkus(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus, true, false))
        .thenReturn(this.itemLists);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl
        .getProductAndItemsWithMasterDataDetailByDefaultProduct(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, false);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .getItemsWithDiscountPriceByProductSkus(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus, true, false);
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getProductAndItemsByProductCodesForPV2Test() throws Exception {
    productCodes.add("DEFAULT_PRODUCT_CODE");
    itemLists.forEach(
        item -> item.getPristineDataItem().setDefaultProductCode(ProductSearchHelperServiceImplTest.STORE_ID));
    when(this.itemService.getItemsWithDiscountPriceByProductSkus(ProductSearchHelperServiceImplTest.STORE_ID,
        ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus,
        true, false)).thenReturn(this.itemLists);
    when(this.masterDataService.getMasterDataProductDetailResponse(
        Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
        Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
        Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
        Mockito.eq(new HashSet<>(Arrays.asList(ProductSearchHelperServiceImplTest.STORE_ID, "product-code-1"))),
        Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailByDefaultProduct(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, false);
    verify(this.masterDataService).getMasterDataProductDetailResponse(
        Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
        Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
        Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
        Mockito.eq(new HashSet<>(Arrays.asList(ProductSearchHelperServiceImplTest.STORE_ID, "product-code-1"))),
        Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService).getItemsWithDiscountPriceByProductSkus(ProductSearchHelperServiceImplTest.STORE_ID,
        ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus,
        true, false);
    verify(this.objectConverterService).convertAndValidateMasterDataExists(Mockito.eq(this.productResult),
        Mockito.eq(this.itemLists), Mockito.anyMap(),
        Mockito.anyMap());
  }

  @Test
  public void getProductAndItemWithMasterDataDetailByDefaultProductTest() throws Exception {
    productCodes.add("DEFAULT_PRODUCT_CODE");
    when(this.itemService
        .getItem(ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.REQUESTID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.ITEM_SKU_1,
            true, true, true, false, null, false, false))
        .thenReturn(this.itemLists.get(0));
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl
        .getProductAndItemWithMasterDataDetailByDefaultProduct(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID, this.productResult,
            ProductSearchHelperServiceImplTest.ITEM_SKU_1, true, false);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .getItem(ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.REQUESTID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.ITEM_SKU_1,
            true, true, true, false, null, false, false);
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(Collections.singletonList(this.itemLists.get(0))), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getProductAndItemWithMasterDataDetailTest() throws Exception {
    when(this.itemService
        .getItem(ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.REQUESTID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.ITEM_SKU_1,
            true, true, true, false, null, false, false))
        .thenReturn(this.itemLists.get(0));
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl
        .getProductAndItemWithMasterDataDetail(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID, this.productResult,
            ProductSearchHelperServiceImplTest.ITEM_SKU_1, true, false);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .getItem(ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.REQUESTID,
            ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.ITEM_SKU_1,
            true, true, true, false, null, false, false);
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(Collections.singletonList(this.itemLists.get(0))), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProductTest() throws Exception {
    productResult.get(0).setProductCode(PRODUCT_CODE_1);
    productResult.get(1).setProductCode(DEFAULT_PRODUCT_CODE);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    Set<String> skus = productResult.stream().map(Product :: getProductSku).collect(Collectors.toSet());
    when(this.itemService
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true)))
        .thenReturn(this.itemLists);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct().setReviewPending(true);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog("catalog", new Category("code", "id")));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().getOrDefault(0, new MasterDataItem())
        .setDangerousLevel(1);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    when(productAnalyticsOutbound.checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(new SellerDetailResponse());

    SimpleMasterDataDetailWithProductAndItemsResponseVo defaultProduct =
        this.productSearchHelperServiceImpl.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, null, null);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
    verify(productAnalyticsOutbound).checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending(), true);
    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(DEFAULT_PRODUCT_CODE).isReviewPending(), true);
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct2Test() throws Exception {
    productResult.get(0).setProductCode(PRODUCT_CODE_1);
    productResult.get(1).setProductCode(DEFAULT_PRODUCT_CODE);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    Set<String> skus = productResult.stream().map(Product :: getProductSku).collect(Collectors.toSet());
    SellerDetailResponse sellerDetailResponse = new SellerDetailResponse();
    sellerDetailResponse.setGoodSeller(true);

    when(this.itemService
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true)))
        .thenReturn(this.itemLists);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct().setReviewPending(true);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog("catalog", new Category("code", "id")));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().getOrDefault(0, new MasterDataItem())
        .setDangerousLevel(1);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    when(productAnalyticsOutbound.checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(sellerDetailResponse);

    SimpleMasterDataDetailWithProductAndItemsResponseVo defaultProduct =
        this.productSearchHelperServiceImpl.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, null, null);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
    verify(productAnalyticsOutbound).checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending(), false);
    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(DEFAULT_PRODUCT_CODE).isReviewPending(), false);
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailNoReviewPendingTrueTest() throws Exception {
    productResult.get(0).setProductCode(PRODUCT_CODE_1);
    productResult.get(1).setProductCode(DEFAULT_PRODUCT_CODE);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    masterDataProductDetailResponse.values().forEach(
        masterDataProductAndItemsVO -> masterDataProductAndItemsVO.getMasterDataProduct().setReviewPending(false));
    Set<String> skus = productResult.stream().map(Product::getProductSku).collect(Collectors.toSet());
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog("catalog", new Category("code", "id")));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().getOrDefault(0, new MasterDataItem())
        .setDangerousLevel(1);

    when(this.itemService
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true)))
        .thenReturn(this.itemLists);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);

    SimpleMasterDataDetailWithProductAndItemsResponseVo defaultProduct =
        this.productSearchHelperServiceImpl.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, null, null);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());

    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending(), false);
    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(DEFAULT_PRODUCT_CODE).isReviewPending(), false);
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailExceptionTest() throws Exception {
    productResult.get(0).setProductCode(PRODUCT_CODE_1);
    productResult.get(1).setProductCode(DEFAULT_PRODUCT_CODE);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    masterDataProductDetailResponse.values().forEach(
        masterDataProductAndItemsVO -> masterDataProductAndItemsVO.getMasterDataProduct().setReviewPending(true));
    Set<String> skus = productResult.stream().map(Product::getProductSku).collect(Collectors.toSet());
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog("catalog", new Category("code", "id")));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().getOrDefault(0, new MasterDataItem())
        .setDangerousLevel(1);

    when(this.itemService
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true)))
        .thenReturn(this.itemLists);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    when(productAnalyticsOutbound.checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenThrow(ApplicationRuntimeException.class);

    SimpleMasterDataDetailWithProductAndItemsResponseVo defaultProduct =
        this.productSearchHelperServiceImpl.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, null, null);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
    verify(productAnalyticsOutbound).checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending(), true);
    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(DEFAULT_PRODUCT_CODE).isReviewPending(), true);
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct_withUSPTest() throws Exception {
    productResult.get(1).setMasterDataProduct(new MasterDataProduct());
    productResult.get(1).getMasterDataProduct().setUniqueSellingPoint(USP);
    productResult.get(1).getMasterDataProduct().setReviewPending(false);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct().setUniqueSellingPoint(USP);
    Set<String> skus = productResult.stream().map(Product :: getProductSku).collect(Collectors.toSet());
    when(this.itemService
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true)))
        .thenReturn(this.itemLists);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    SimpleMasterDataDetailWithProductAndItemsResponseVo response = this.productSearchHelperServiceImpl
        .getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, true, null, null);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.itemService)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(skus), Mockito.eq(true));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
    Assertions.assertEquals(USP, response.getMasterDataProducts().get(PRODUCT_CODE_1).getUniqueSellingPoint());
    Assertions.assertEquals(false, response.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending());
  }

  @Test
  public void getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct1Test() throws Exception {
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setAllowedAttributeValue(new MasterDataAllowedAttributeValue());
    masterDataProductAttribute.setMasterDataProductAttributeValues(
        Collections.singletonList(masterDataProductAttributeValue));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttribute));
    productResult.get(0).setProductCode(PRODUCT_CODE_1);
    productResult.get(1).setProductCode(DEFAULT_PRODUCT_CODE);
    productCodes.add(DEFAULT_PRODUCT_CODE);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct().setReviewPending(true);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setProductCode(PRODUCT_CODE);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().put(ITEM_CODE_1, masterDataItem);
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog("catalog", new Category("code", "id")));
    masterDataProductDetailResponse.get(PRODUCT_CODE_1).getMasterDataItems().getOrDefault(0, new MasterDataItem())
        .setDangerousLevel(1);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    when(productAnalyticsOutbound.checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(new SellerDetailResponse());

    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo defaultProduct =
        this.productSearchHelperServiceImpl.getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
            ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, itemLists, null);

    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.objectConverterService)
        .convertAndValidateSimpleMasterDataExistsV2(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.eq(null), Mockito.anyMap(),
            Mockito.anyMap());
    verify(productAnalyticsOutbound).checkGoodSeller(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(PRODUCT_CODE_1).isReviewPending(), true);
    Assertions.assertEquals(defaultProduct.getMasterDataProducts().get(DEFAULT_PRODUCT_CODE).isReviewPending(), true);
    Assertions.assertEquals(defaultProduct.getMasterDataItems().get(ITEM_CODE_1).getProductCode(), PRODUCT_CODE);
  }

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    this.masterDataProduct = new HashMap<String, MasterDataProduct>();
    this.masterDataItem = new HashMap<String, MasterDataItem>();
    ProductAndItemSolr solr1 = new ProductAndItemSolr();
    solr1.setItemCode(ProductSearchHelperServiceImplTest.ITEM_CODE_1);
    solr1.setProductCode(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1);
    solr1.setItemSku(ProductSearchHelperServiceImplTest.ITEM_SKU_1);
    solr1.setProductSku(ProductSearchHelperServiceImplTest.PRODUCT_SKU_1);
    solr1.setSynchronized(true);

    ProductAndItemSolr solr2 = new ProductAndItemSolr();
    solr2.setItemCode(ProductSearchHelperServiceImplTest.ITEM_CODE_1);
    solr2.setProductCode(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1);
    solr2.setItemSku(ProductSearchHelperServiceImplTest.ITEM_SKU_2);
    solr2.setProductSku(ProductSearchHelperServiceImplTest.PRODUCT_SKU_1);
    solr2.setSynchronized(false);
    this.solrResult = Arrays.asList(solr1, solr2);

    this.itemCodes = new HashSet<String>();
    this.itemCodes.add(ProductSearchHelperServiceImplTest.ITEM_CODE_1);
    this.productCodes = new HashSet<String>();
    this.productCodes.add(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1);

    this.itemSkus =
        Arrays.asList(ProductSearchHelperServiceImplTest.ITEM_SKU_1,
            ProductSearchHelperServiceImplTest.ITEM_SKU_2);
    this.productSkus = new HashSet<String>();
    this.productSkus.add(ProductSearchHelperServiceImplTest.PRODUCT_SKU_1);
    this.products = Arrays.asList(new Product());
    Item item1 = new Item();
    item1.setProductSku(PRODUCT_SKU_1);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE");
    pristineDataItem.setPristineId("PRISTINE_ID");
    item1.setPristineDataItem(pristineDataItem);
    Item item2 = new Item();
    PristineDataItem pristineDataItem2 = new PristineDataItem();
    pristineDataItem2.setDefaultProductCode(PRODUCT_CODE_1);
    pristineDataItem2.setPristineId("PRISTINE_ID1");
    item2.setPristineDataItem(pristineDataItem2);
    item2.setProductSku(PRODUCT_SKU_1);
    this.itemLists = new ArrayList<>();
    this.itemLists.add(item1);
    this.itemLists.add(item2);
    this.items = Arrays.asList(new Item());

    this.productResult = new ArrayList<Product>();
    Product product1 = new Product();
    product1.setProductCode(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1);
    product1.setProductSku(ProductSearchHelperServiceImplTest.PRODUCT_SKU_1);
    product1.setSynchronized(true);
    product1.setMerchantCode(SELLER_CODE);
    Product product2 = new Product();
    product2.setProductCode(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1);
    product2.setProductSku(ProductSearchHelperServiceImplTest.PRODUCT_SKU_1);
    product2.setSynchronized(false);
    product2.setMerchantCode(SELLER_CODE);
    this.productResult.add(product1);
    this.productResult.add(product2);
    masterDataProductDetailResponse = new HashMap<>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(new MasterDataProduct());
    masterDataProductAndItemsVO.setMasterDataItems(masterDataItem);
    masterDataProductDetailResponse.put(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1, masterDataProductAndItemsVO);
    masterDataProductDetailResponse.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);

    /* masterDataProduct.put(ProductSearchHelperServiceImplTest.PRODUCT_CODE_1, new MasterDataProduct());
    masterDataProduct.put("DEFAULT_PRODUCT_CODE", new MasterDataProduct());*/
    when(
        this.productService.getProducts(ProductSearchHelperServiceImplTest.STORE_ID,
            this.productSkus)).thenReturn(this.products);
    when(
        this.itemService.getItemsWithDiscountPriceByProductSkus(
            ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productSkus, true, false)).thenReturn(this.items);

    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(PREORDER_VALUE);
    preOrder.setPreOrderDate(new Date());
  }

  @Test
  public void setItemCatalogsWithNeedCategoryHierarchyFalse() throws Exception {
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(new Product(), null);
    this.productSearchHelperServiceImpl.setItemCatalogs(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, false, Arrays.asList(productAndItems),
        this.masterDataProduct);
    Assertions.assertNull(productAndItems.getItems());
  }

  @Test
  public void setItemCatalogsWithNeedCategoryHierarchyTrue() throws Exception {
    Product product = new Product();
    product.setMasterCatalog(new MasterCatalog());
    ProductAndItemsVO productAndItems = new ProductAndItemsVO(product, null);
    this.productSearchHelperServiceImpl.setItemCatalogs(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, true, Arrays.asList(productAndItems),
        this.masterDataProduct);
    verify(this.catalogService).getItemCatalogsWithCategoryHierarchy(
        ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID,
        productAndItems.getProduct());
  }

  void createSalesCatalogForProduct(Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct,
      List<SimpleProductAndItemsVO> productAndItems) {
    SimpleMasterDataProductVO simpleMasterDataProductVO = SimpleMasterDataProductVO
        .toMasterDataProductVo(masterDataProductDetailResponse.get(PRODUCT_CODE_1));
    mapOfMasterDataProduct.put(PRODUCT_CODE_1, simpleMasterDataProductVO);
    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    SalesCatalog salesCatalog = new SalesCatalog();
    Category category1 = new Category("category1", "category1");
    Category category2 = new Category("category2", "category2");
    List<Category> categories = new ArrayList<>();
    categories.add(category1);
    categories.add(category2);
    salesCatalog.setListOfCategories(categories);
    salesCatalogs.add(salesCatalog);
    Product product = productResult.get(1);
    product.setSalesCatalogs(salesCatalogs);
    simpleMasterDataProductVO.setSalesCatalogs(salesCatalogs);
    SimpleProductVO productVO = SimpleProductVO.toSimpleProductVo(simpleMasterDataProductVO, product);
    SimpleProductAndItemsVO simpleProductAndItemsVO = new SimpleProductAndItemsVO();
    simpleProductAndItemsVO.setSimpleProduct(productVO);
    productAndItems.add(simpleProductAndItemsVO);
    simpleProductAndItemsVO.getSimpleProduct().getSimpleAsyncMasterDataProduct().setSalesCatalogs(salesCatalogs);

  }

  void createSalesCatalogForProductAndItem(Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct,
      List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems) {
    SimpleMasterDataProductVO simpleMasterDataProductVO = SimpleMasterDataProductVO
        .toMasterDataProductVo(masterDataProductDetailResponse.get(PRODUCT_CODE_1));
    mapOfMasterDataProduct.put(PRODUCT_CODE_1, simpleMasterDataProductVO);
    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    SalesCatalog salesCatalog = new SalesCatalog();
    Category category1 = new Category("category1", "category1");
    Category category2 = new Category("category2", "category2");
    List<Category> categories = new ArrayList<>();
    categories.add(category1);
    categories.add(category2);
    salesCatalog.setListOfCategories(categories);
    salesCatalogs.add(salesCatalog);
    Product product = productResult.get(1);
    product.setSalesCatalogs(salesCatalogs);
    simpleMasterDataProductVO.setSalesCatalogs(salesCatalogs);
    SimpleProductVO productVO = SimpleProductVO.toSimpleProductVo(simpleMasterDataProductVO, product);
    SimpleProductAndItemsAndItemPickupPointV0 simpleProductAndItemsVO = new SimpleProductAndItemsAndItemPickupPointV0();
    simpleProductAndItemsVO.setSimpleProduct(productVO);
    productAndItems.add(simpleProductAndItemsVO);
    simpleProductAndItemsVO.getSimpleProduct().getSimpleAsyncMasterDataProduct().setSalesCatalogs(salesCatalogs);
  }

  @Test
  public void setSimpleItemCatalogsSuccessTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsVO> productAndItems = new ArrayList<>();
    createSalesCatalogForProduct(mapOfMasterDataProduct, productAndItems);

    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsSuccess_whenProductCodeNullTest() throws Exception{
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsVO> productAndItems = new ArrayList<>();
    createSalesCatalogForProduct(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setSalesCatalogs(null);
    productAndItems.get(0).getSimpleProduct().setProductCode(null);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogs(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
    Assertions.assertNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());

  }

  @Test
  public void setSimpleItemCatalogsInMasterDataTest() throws Exception{
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);

    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterDataWithMasterCategoryHierarchyEmptyMasterCategoryCodeTest()
      throws Exception {
    ReflectionTestUtils.setField(this.productSearchHelperServiceImpl, "populateMasterCategoryHierarchyInReindexApi",
        true);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);

    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
        Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl.setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        productAndItems, mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID), Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterDataWithMasterCategoryHierarchyTest() throws Exception {
    ReflectionTestUtils.setField(this.productSearchHelperServiceImpl, "populateMasterCategoryHierarchyInReindexApi",
        true);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setMasterCategoryCode(PRODUCT_CATENTRY_ID);

    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    List<ItemCatalogVOV2> itemCatalogVOS2 = new ArrayList<>();
    ItemCatalogVOV2 itemCatalogVOV2 = new ItemCatalogVOV2("catalogId", null);
    itemCatalogVOS2.add(itemCatalogVOV2);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
        Mockito.anyList())).thenReturn(itemCatalogVOS).thenReturn(itemCatalogVOS2);
    this.productSearchHelperServiceImpl.setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        productAndItems, mapOfMasterDataProduct);
    Mockito.verify(this.catalogService, times(2))
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID), listArgumentCaptor.capture());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertTrue(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs().stream()
        .anyMatch(itemCatalog -> itemCatalog.getCatalogId().equals("catalogId")));
    Assertions.assertEquals(PRODUCT_CATENTRY_ID, listArgumentCaptor.getAllValues().get(1).get(0));
    Assertions.assertNull(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterDataWithMasterCategoryHierarchyExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.productSearchHelperServiceImpl, "populateMasterCategoryHierarchyInReindexApi",
        true);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setMasterCategoryCode(PRODUCT_CATENTRY_ID);

    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
        Mockito.anyList())).thenReturn(itemCatalogVOS).thenThrow(RuntimeException.class);
    this.productSearchHelperServiceImpl.setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        productAndItems, mapOfMasterDataProduct);
    Mockito.verify(this.catalogService, times(2))
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID), Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterData_whenProductCodeNullTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setSalesCatalogs(null);
    productAndItems.get(0).getSimpleProduct().setProductCode(null);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
    Assertions.assertNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());

  }

  @Test
  public void setSimpleItemCatalogsInMasterData_whenProductCodeNullMasterCategoryHierarchyEmptyTest() throws Exception {
    ReflectionTestUtils.setField(this.productSearchHelperServiceImpl, "populateMasterCategoryHierarchyInReindexApi",
        true);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setSalesCatalogs(null);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setMasterCategoryCode(PRODUCT_CATENTRY_ID);
    productAndItems.get(0).getSimpleProduct().setProductCode(null);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
        Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl.setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        productAndItems, mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID), Mockito.anyList());
    Assertions.assertNotNull(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs());
    Assertions.assertNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterData_whenProductCodeNullMasterCategoryHierarchyTest() throws Exception {
    ReflectionTestUtils.setField(this.productSearchHelperServiceImpl, "populateMasterCategoryHierarchyInReindexApi",
        true);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setSalesCatalogs(null);
    productAndItems.get(0).getSimpleProduct().setProductCode(null);
    productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().setMasterCategoryCode(PRODUCT_CATENTRY_ID);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    List<ItemCatalogVOV2> itemCatalogVOS2 = new ArrayList<>();
    ItemCatalogVOV2 itemCatalogVOV2 = new ItemCatalogVOV2("catalogId", null);
    itemCatalogVOS2.add(itemCatalogVOV2);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
        Mockito.anyList())).thenReturn(itemCatalogVOS).thenReturn(itemCatalogVOS2);
    this.productSearchHelperServiceImpl.setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID,
        productAndItems, mapOfMasterDataProduct);
    Mockito.verify(this.catalogService, times(2))
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            listArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CATENTRY_ID, listArgumentCaptor.getAllValues().get(1).get(0));
    Assertions.assertNotNull(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs());
    Assertions.assertNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertTrue(
        productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct().getItemCatalogs().stream()
            .anyMatch(itemCatalog -> itemCatalog.getCatalogId().equals("catalogId")));
  }

  @Test
  public void setSimpleItemCatalogsInMasterDataEmptyMapOfMasterDataProduct_WhenExceptionTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct = new HashMap();

    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    Mockito.doThrow(new RuntimeException()).when(this.catalogService)
        .getItemCatalogsWithCategoryHierarchy(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Assertions.assertEquals(productAndItems.get(0).getSimpleItems().size(),0);
  }

  @Test
  public void setSimpleItemCatalogsInMasterDataEmptyItemCatalogs_WhenExceptionTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    mapOfMasterDataProduct.get(PRODUCT_CODE_1).setItemCatalogs(Arrays.asList(new ItemCatalogVOV2()));

    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    Mockito.doThrow(new RuntimeException()).when(this.catalogService)
        .getItemCatalogsWithCategoryHierarchy(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Assertions.assertEquals(productAndItems.get(0).getSimpleItems().size(),0);
  }

  @Test
  public void setSimpleItemCatalogsInMasterData_WhenItemCatalogVOV2NotEmptyTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    ItemCategoryVOV2 itemCategoryVOV2 = new ItemCategoryVOV2();
    itemCategoryVOV2.setCategoryActive(true);
    itemCategoryVOV2.setCategory("category1");
    itemCategoryVOV2.setLevel(3);
    List<ItemCategoryVOV2> itemCategories = new ArrayList<>();
    itemCategories.add(itemCategoryVOV2);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    itemCatalogVOS.add(new ItemCatalogVOV2("catalogId",itemCategories));
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterData_WhenItemCategoriesEmptyTest() throws Exception{

    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    List<ItemCatalogVOV2> itemCatalogVOS = new ArrayList<>();
    itemCatalogVOS.add(new ItemCatalogVOV2("catalogId",new ArrayList<>()));
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenReturn(itemCatalogVOS);
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
    Assertions.assertNotNull(mapOfMasterDataProduct.get(PRODUCT_CODE_1).getItemCatalogs());
    Assertions.assertNull(productAndItems.get(0).getSimpleProduct().getSimpleAsyncMasterDataProduct()
        .getItemCatalogs());
  }

  @Test
  public void setSimpleItemCatalogsInMasterData_ExceptionTest() throws Exception{
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    List<SimpleProductAndItemsAndItemPickupPointV0> productAndItems = new ArrayList<>();
    createSalesCatalogForProductAndItem(mapOfMasterDataProduct, productAndItems);
    Mockito.when(this.catalogService
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList())).thenThrow(new RuntimeException());
    this.productSearchHelperServiceImpl
        .setSimpleItemCatalogsInMasterData(STORE_ID, USERNAME, REQUESTID, productAndItems,
            mapOfMasterDataProduct);
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyV2(Mockito.eq(USERNAME), Mockito.eq(REQUESTID),
            Mockito.anyList());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.catalogService);
    verifyNoMoreInteractions(this.masterDataService);
    verifyNoMoreInteractions(this.productAnalyticsOutbound);
  }

  @Test
  public void validateParametersSuccess() {
    Set<String> params = new HashSet<String>();
    params.add(null);
    params.add(null);
    params.add(ProductSearchHelperServiceImplTest.ITEM_CODE_1);
    this.productSearchHelperServiceImpl.validateParameters(
        ProductSearchHelperServiceImplTest.STORE_ID, params);
    assertThat(params.size(), equalTo(1));
  }

  @Test
  public void validateParametersWithEmptySetOfParam() {
    Assertions.assertThrows(Exception.class, () -> this.productSearchHelperServiceImpl.validateParameters(
        ProductSearchHelperServiceImplTest.STORE_ID, new HashSet<String>()));
  }


  @Test
  public void validateParametersWithNullSetOfParam() {
    Assertions.assertThrows(Exception.class, () -> this.productSearchHelperServiceImpl.validateParameters(
        ProductSearchHelperServiceImplTest.STORE_ID, null));
  }

  @Test
  public void validateParametersWithNullStoreId() {
    Assertions.assertThrows(Exception.class, () -> this.productSearchHelperServiceImpl.validateParameters(null, new HashSet<String>()));
  }

  @Test
  public void
  getProductAndItemsByProductCodesAndPristineDetail_whenDefaultProdCodeAndProdCodeNotEqualTest()
      throws Exception {
    Set<String> productCodes = new HashSet<>();
    productCodes.add(productResult.get(0).getProductCode());
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.itemLists, this.productResult,
        productResult.get(0).getProductCode(), productCodes);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExistsForPristine(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
  }


  @Test
  public void getProductAndItemsByProductCodesAndPristineDetail_whenDefaultProductCodeNullTest() throws Exception {
    Set<String> productCodes = new HashSet<>();
    productCodes.add(productResult.get(0).getProductCode());
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.itemLists, this.productResult,
        null, productCodes);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExistsForPristine(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getProductAndItemsByProductCodesAndPristineDetail_whenProductCodeNullTest() throws Exception {
    Set<String> productCodes = new HashSet<>();
    productResult.get(0).setProductCode(null);
    productCodes.add("DEFAULT_PRODUCT_CODE");
    masterDataProductDetailResponse.remove(PRODUCT_CODE_1);
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.itemLists, this.productResult,
        "DEFAULT_PRODUCT_CODE", productCodes);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExistsForPristine(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getProductAndItemsByProductCodesAndPristineDetail_MasterDataDetailResponseSize1Test() throws Exception {
    Set<String> productCodes = new HashSet<>();
    productCodes.add(productResult.get(0).getProductCode());
    masterDataProductDetailResponse.remove("DEFAULT_PRODUCT_CODE");
    when(this.masterDataService
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID),
            Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailAndPristineDetail(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.itemLists, this.productResult,
        productResult.get(0).getProductCode(), productCodes);
    verify(this.masterDataService)
        .getMasterDataProductDetailResponse(Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
            Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
            Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(this.productCodes), Mockito.eq(IN_ALL_PRODUCTS));
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExistsForPristine(Mockito.eq(this.productResult),
            Mockito.eq(this.itemLists), Mockito.anyMap(),
            Mockito.anyMap());
  }

  @Test
  public void getProductAndItemsByProductCodesAndPristineDetailExceptionTest()
      throws Exception {
    Set<String> productCodes = new HashSet<>();
    productCodes.add(productResult.get(0).getProductCode());
    doThrow(RuntimeException.class).when(this.masterDataService)
        .getMasterDataProductDetailResponse(ProductSearchHelperServiceImplTest.STORE_ID,
            ProductSearchHelperServiceImplTest.USERNAME,
            ProductSearchHelperServiceImplTest.REQUESTID, this.productCodes, IN_ALL_PRODUCTS);
    try {
      this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailAndPristineDetail(
          ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
          ProductSearchHelperServiceImplTest.REQUESTID, this.items, this.productResult,
          productResult.get(0).getProductCode(), productCodes);
    } catch (RuntimeException e) {
      verify(this.masterDataService)
          .getMasterDataProductDetailResponse(ProductSearchHelperServiceImplTest.STORE_ID,
              ProductSearchHelperServiceImplTest.USERNAME,
              ProductSearchHelperServiceImplTest.REQUESTID, this.productCodes, IN_ALL_PRODUCTS);
    }
  }

  @Test
  public void getProductMasterDataDetailByProductCodeTest() throws Exception{
    Set set = new HashSet();
    set.add(PRODUCT_CODE_1);
    Mockito.when(this.masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUESTID, set, IN_ALL_PRODUCTS)).thenReturn(masterDataProductDetailResponse);
    Map<String, MasterDataProductAndItemsVO> response = this.productSearchHelperServiceImpl
        .getProductMasterDataDetailByProductCode(STORE_ID, USERNAME, REQUESTID, PRODUCT_CODE_1);
    Mockito.verify(this.masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUESTID, set, IN_ALL_PRODUCTS);
    Assertions.assertNotNull(response.get(PRODUCT_CODE_1));
  }

  @Test
  public void toSimpleProductVoTest() {
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct = new HashMap();
    SimpleMasterDataProductVO simpleMasterDataProductVO = SimpleMasterDataProductVO
        .toMasterDataProductVo(masterDataProductDetailResponse.get(PRODUCT_CODE_1));
    mapOfMasterDataProduct.put(PRODUCT_CODE_1, simpleMasterDataProductVO);
    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    SalesCatalog salesCatalog = new SalesCatalog();
    Category category1 = new Category("category1", "category1");
    Category category2 = new Category("category2", "category2");
    List<Category> categories = new ArrayList<>();
    categories.add(category1);
    categories.add(category2);
    salesCatalog.setListOfCategories(categories);
    salesCatalogs.add(salesCatalog);
    Product product = productResult.get(1);
    product.setPreOrder(preOrder);
    product.setSalesCatalogs(salesCatalogs);
    simpleMasterDataProductVO.setSalesCatalogs(salesCatalogs);
    SimpleProductVO productVO = SimpleProductVO.toSimpleProductVo(simpleMasterDataProductVO, product);
    SimpleProductAndItemsVO simpleProductAndItemsVO = new SimpleProductAndItemsVO();
    simpleProductAndItemsVO.setSimpleProduct(productVO);
    simpleProductAndItemsVO.getSimpleProduct().getSimpleAsyncMasterDataProduct().setSalesCatalogs(salesCatalogs);
    Assertions.assertNull(productVO.getPreOrder());
  }


  @Test
  public void getProductAndItemsWithMasterDataDetailWithoutL5DetailsTest() throws Exception {
    when(this.masterDataService.getMasterDataProductDetailResponse(
        Mockito.eq(ProductSearchHelperServiceImplTest.STORE_ID),
        Mockito.eq(ProductSearchHelperServiceImplTest.USERNAME),
        Mockito.eq(ProductSearchHelperServiceImplTest.REQUESTID), Mockito.eq(this.productCodes),
        Mockito.eq(IN_ALL_PRODUCTS))).thenReturn(masterDataProductDetailResponse);
    this.productSearchHelperServiceImpl.getProductAndItemsWithMasterDataDetailWithoutL5Details(
        ProductSearchHelperServiceImplTest.STORE_ID, ProductSearchHelperServiceImplTest.USERNAME,
        ProductSearchHelperServiceImplTest.REQUESTID, this.productResult, items);
    verify(this.masterDataService).getMasterDataProductDetailResponse(ProductSearchHelperServiceImplTest.STORE_ID,
        ProductSearchHelperServiceImplTest.USERNAME, ProductSearchHelperServiceImplTest.REQUESTID, this.productCodes,
        IN_ALL_PRODUCTS);
    verify(this.objectConverterService)
        .convertAndValidateMasterDataExists(Mockito.eq(this.productResult),
            Mockito.eq(this.items), Mockito.anyMap(),
            Mockito.anyMap());
  }
}
