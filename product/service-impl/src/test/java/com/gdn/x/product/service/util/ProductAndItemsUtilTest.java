package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.vo.HalalDashboardFilterRequestVo;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.WholesaleRule;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestV2Vo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.google.common.collect.ImmutableSet;


public class ProductAndItemsUtilTest {

  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String MERCHANT_SKU_1 = "merchantCode1";
  private static final String MERCHANT_SKU_2 = "merchantCode2";

  private Item item;
  private ItemPickupPoint itemPickupPoint;
  private Product product;
  private ProductSummaryResponseV2Vo productSummaryResponseV2Vo;
  private ProductSummaryResponseV2Vo productSummaryResponseV2Vo2;

  @BeforeEach
  public void setUp() {
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setMerchantSku(MERCHANT_SKU_2);

    product = new Product();

    item = new Item();
    item.setItemCode(ITEM_CODE);
    item.setMerchantSku(MERCHANT_SKU_1);
    item.setCategoryCode(CATEGORY_CODE);
    item.setItemSku(ITEM_SKU);

    productSummaryResponseV2Vo = new ProductSummaryResponseV2Vo();
    productSummaryResponseV2Vo.setCategoryCode(CATEGORY_CODE);
    productSummaryResponseV2Vo2 = new ProductSummaryResponseV2Vo();
    productSummaryResponseV2Vo2.setCategoryCode(null);
  }

  @Test
  public void getItemCodeFromItemTest() {
    List<String> itemCodes = ProductAndItemsUtil.getItemCodeFromItem(Arrays.asList(item));
    Assertions.assertEquals(ITEM_CODE, itemCodes.get(0));
  }

  @Test
  public void getItemCodeFromItemEmptyTest() {
    item.setItemCode(null);
    List<String> itemCodes = ProductAndItemsUtil.getItemCodeFromItem(Arrays.asList(item));
    Assertions.assertEquals(0, itemCodes.size());
  }

  @Test
  public void getItemSkuFromItemPickupPointsTest() {
    Set<String> itemSkus = ProductAndItemsUtil.getItemSkuFromItemPickupPoints(Arrays.asList(itemPickupPoint));
    Assertions.assertTrue(itemSkus.contains(ITEM_SKU));
  }

  @Test
  public void getProductSkuFromItemPickupPointsTest() {
    List<String> productSkus = ProductAndItemsUtil.getProductSkuFromItemPickupPoints(Arrays.asList(itemPickupPoint));
    Assertions.assertEquals(PRODUCT_SKU, productSkus.get(0));
  }

  @Test
  public void toItemPickupPointRequestVoTest() {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoList =
        ProductAndItemsUtil.toItemPickupPointRequestVo(Arrays.asList(itemPickupPointRequest));
    Assertions.assertEquals(ITEM_SKU, itemPickupPointRequestVoList.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointRequestVoList.get(0).getPickupPointCode());
  }

  @Test
  public void getPickupPointCodesFromItemPickupPointsTest() {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    List<String> pickupPoints = ProductAndItemsUtil.getPickupPointCodesFromItemPickupPoints(Arrays.asList(itemPickupPoint));
    Assertions.assertEquals(PICKUP_POINT_CODE, pickupPoints.get(0));
  }

  @Test
  public void toItemPickupPointSummaryRequestVoTest() {
    ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo =
        ProductAndItemsUtil.toItemPickupPointSummaryRequestVo(new ItemPickupPointSummaryRequest(), new HashSet<>());
    Assertions.assertTrue(CollectionUtils.isEmpty(itemPickupPointSummaryRequestVo.getItemPickupPointCode()));
  }

  @Test
  public void toItemPickupPointSummaryRequestVo2Test() {
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest = new ItemPickupPointSummaryRequest();
    itemPickupPointSummaryRequest.setItemPickupPointCode(Arrays.asList(new ItemPickupPointRequest()));
    ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo =
        ProductAndItemsUtil.toItemPickupPointSummaryRequestVo(itemPickupPointSummaryRequest, new HashSet<>());
    Assertions.assertFalse(CollectionUtils.isEmpty(itemPickupPointSummaryRequestVo.getItemPickupPointCode()));
  }

  @Test
  public void getCategoryFromProductTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(category);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(masterCatalog);
    product.setMasterDataProduct(masterDataProduct);
    List<String> categoryCodes = ProductAndItemsUtil.getCategoryFromProduct(Arrays.asList(product),  true);
    Assertions.assertEquals(CATEGORY_CODE, categoryCodes.get(0));
  }

  @Test
  public void getCategoryFromProductFalseTest() {
    product.setCategoryCode(CATEGORY_CODE);
    List<String> categoryCodes = ProductAndItemsUtil.getCategoryFromProduct(Arrays.asList(product), false);
    Assertions.assertEquals(CATEGORY_CODE, categoryCodes.get(0));
  }

  @Test
  public void getCategoryFromProductTrueTest() {
    product.setCategoryCode(null);
    product.setMasterDataProduct(new MasterDataProduct());
    product.getMasterDataProduct()
        .setMasterCatalog(new MasterCatalog(CATEGORY_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    List<String> categoryCodes = ProductAndItemsUtil.getCategoryFromProduct(Arrays.asList(product), true);
    Assertions.assertEquals(CATEGORY_CODE, categoryCodes.get(0));
  }

  @Test
  public void getCategoryFromProductSummaryResponseV2VoTest() {
    List<String> categoryCodes = ProductAndItemsUtil
        .getCategoryFromProductSummaryResponseV2Vo(Arrays.asList(productSummaryResponseV2Vo, productSummaryResponseV2Vo2));
    Assertions.assertEquals(CATEGORY_CODE, categoryCodes.get(0));
    Assertions.assertEquals(1, categoryCodes.size());
  }

  @Test
  public void toProductSummaryRequestV2VoTest() {
    ProductSummaryRequestV2Vo productSummaryRequestV2Vo =
        ProductAndItemsUtil.toProductSummaryRequestV2Vo(new ProductSummaryRequestV2());
    Assertions.assertTrue(CollectionUtils.isEmpty(productSummaryRequestV2Vo.getPickupPointCodes()));
  }

  @Test
  public void totoHalalDashboardFilterRequestVoTest() {
    HalalDashboardFilterRequestVo halalDashboardFilterRequestVo =
        ProductAndItemsUtil.toHalalDashboardFilterRequestVo(new HalalProductsFilterRequest());
    Assertions.assertTrue(CollectionUtils.isEmpty(halalDashboardFilterRequestVo.getCurationStatus()));
  }

  @Test
  public void validateItemPickupPointRequestTest() {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ProductAndItemsUtil.validateItemPickupPointRequest(Arrays.asList(itemPickupPointRequest));
  }

  @Test
  public void validateItemPickupPointRequestItemSkuEmptyTest() {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest(StringUtils.EMPTY, PICKUP_POINT_CODE);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> ProductAndItemsUtil.validateItemPickupPointRequest(Arrays.asList(itemPickupPointRequest)));
  }

  @Test
  public void validateItemPickupPointRequestPickupPointEmptyTest() {
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest(ITEM_SKU, StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> ProductAndItemsUtil.validateItemPickupPointRequest(Arrays.asList(itemPickupPointRequest)));
  }

  @Test
  public void toProductItemsVoCopyL5FieldsTrueTest() {
    ProductItemsVo productItemsVo =
        ProductAndItemsUtil.toProductItemsVo(product, item, itemPickupPoint, true, false);
    Assertions.assertEquals(MERCHANT_SKU_2, productItemsVo.getItemVoList().get(0).getMerchantSku());
  }

  @Test
  public void toProductItemsVoCopyL5FieldsFalseTest() {
    ProductItemsVo productItemsVo =
        ProductAndItemsUtil.toProductItemsVo(product, item, itemPickupPoint, false, false);
    Assertions.assertEquals(MERCHANT_SKU_1, productItemsVo.getItemVoList().get(0).getMerchantSku());
  }

  @Test
  public void toWholesaleRuleVOTest() {
    WholesaleRule wholesaleRule = new WholesaleRule(10, 10, 10.0, 0L);
    List<WholesaleRuleVO> wholesaleRuleVOList = ProductAndItemsUtil.toWholesaleRuleVO(Arrays.asList(wholesaleRule));
    Assertions.assertEquals(10, wholesaleRuleVOList.get(0).getMaxQuantity());
    Assertions.assertEquals(10, wholesaleRuleVOList.get(0).getMinQuantity());
    Assertions.assertEquals(10.0, wholesaleRuleVOList.get(0).getDiscountPercentage(), 0);
  }

  @Test
  public void toItemPickupPointListingRequestVoTest() {
    ItemPickupPointListingRequest itemPickupPointListingRequest =
        ItemPickupPointListingRequest.builder().businessPartnerCode(MERCHANT_SKU_1).productSku(PRODUCT_SKU)
            .pickupPointCodes(ImmutableSet.of(PICKUP_POINT_CODE)).build();
    ItemPickupPointListingRequestVo itemPickupPointListingRequestVo =
        ProductAndItemsUtil.toItemPickupPointListingRequestVo(itemPickupPointListingRequest, ImmutableSet.of(ITEM_SKU));
    Assertions.assertEquals(MERCHANT_SKU_1, itemPickupPointListingRequestVo.getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingRequestVo.getProductSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListingRequestVo.getPickupPointCodes().iterator().next());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingRequestVo.getItemSkus().iterator().next());
  }

  @Test
  public void getItemSkuFromItemTest() {
    Set<String> itemSkus = ProductAndItemsUtil.getItemSkuFromItem(Arrays.asList(item));
    Assertions.assertEquals(ITEM_SKU, itemSkus.iterator().next());
  }

  @Test
  public void filterProductWithMasterDataDetailsUsePcbFetchTrueTest() {
    Product product1 = new Product();
    Product product2 = new Product();
    product2.setCategoryCode(CATEGORY_CODE);

    List<Product> products =
        ProductAndItemsUtil.filterProductWithMasterDataDetails(Arrays.asList(product1, product2), true);

    Assertions.assertEquals(0, products.size());
  }

  @Test
  public void filterProductWithMasterDataDetailsUsePcbFetchFalseTest() {
    Product product1 = new Product();
    Product product2 = new Product();
    product2.setCategoryCode(CATEGORY_CODE);

    List<Product> products =
        ProductAndItemsUtil.filterProductWithMasterDataDetails(Arrays.asList(product1, product2), false);

    Assertions.assertEquals(1, products.size());
  }

  @Test
  public void filterProductWithoutMasterDataDetailsUsePcbFetchTrueTest() {
    Product product1 = new Product();
    Product product2 = new Product();
    product2.setCategoryCode(CATEGORY_CODE);

    List<Product> products =
        ProductAndItemsUtil.filterProductWithoutMasterDataDetails(Arrays.asList(product1, product2), true);

    Assertions.assertEquals(2, products.size());
  }

  @Test
  public void filterProductWithoutMasterDataDetailsUsePcbFetchFalseTest() {
    Product product1 = new Product();
    Product product2 = new Product();
    product2.setCategoryCode(CATEGORY_CODE);

    List<Product> products =
        ProductAndItemsUtil.filterProductWithoutMasterDataDetails(Arrays.asList(product1, product2), false);

    Assertions.assertEquals(1, products.size());
  }

  @Test
  public void getProductSkuFromProductTest() {
    product.setProductSku(PRODUCT_SKU);
    Set<String> productSku = ProductAndItemsUtil.getProductSkuFromProduct(Arrays.asList(product));
    Assertions.assertEquals(PRODUCT_SKU, productSku.iterator().next());
  }

  @Test
  public void filterItemWithoutMasterDataDetailsTest() {
    Item item1 = new Item();
    Item item2 = new Item();
    item1.setProductSku(PRODUCT_SKU);

    List<Item> items = ProductAndItemsUtil.filterItemWithoutMasterDataDetails(Arrays.asList(item1, item2), ImmutableSet.of(PRODUCT_SKU));

    Assertions.assertEquals(1, items.size());
  }

  @Test
  public void filterProductAndItemsVOWithoutMasterDataDetailsTest() {
    Product product1 = new Product();
    Product product2 = new Product();
    product1.setProductSku(PRODUCT_SKU);

    List<ProductAndItemsVO> productAndItemsVOList = ProductAndItemsUtil.filterProductAndItemsVOWithoutMasterDataDetails(Arrays.asList(new ProductAndItemsVO(product1, new ArrayList<>()),
        (new ProductAndItemsVO(product2, new ArrayList<>()))), ImmutableSet.of(PRODUCT_SKU));

    Assertions.assertEquals(1, productAndItemsVOList.size());
  }

  @Test
  public void getItemMapKeyItemSkuTest() {
    Map<String, Item> itemMap = ProductAndItemsUtil.getItemMapKeyItemSku(Arrays.asList(item));
    Assertions.assertTrue(itemMap.containsKey(ITEM_SKU));
  }

  @Test
  public void getItemPickupPointMapKeyItemSkuTest() {
    Map<String, ItemPickupPoint> itemMap =
        ProductAndItemsUtil.getItemPickupPointMapKeyItemSku(Arrays.asList(itemPickupPoint));
    Assertions.assertTrue(itemMap.containsKey(ITEM_SKU));
  }

  @Test
  public void getItemSkuFromItemListingUpdateRequestVoTest() {
    ItemListingUpdateRequestVo itemListingUpdateRequestVo = new ItemListingUpdateRequestVo();
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);

    List<String> itemSkuList =
        ProductAndItemsUtil.getItemSkuFromItemListingUpdateRequestVo(Arrays.asList(itemListingUpdateRequestVo));

    Assertions.assertEquals(ITEM_SKU, itemSkuList.get(0));
  }

  @Test
  public void convertToMasterCatalogResponseTest(){
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(category);
    MasterCatalogDTO masterCatalogDTO = ProductAndItemsUtil.convertToMasterCatalogResponse(masterCatalog);
    Assertions.assertEquals(CATEGORY_CODE,masterCatalogDTO.getCategory().getCategoryCode());
  }

  @Test
  public void convertToMasterCatalogResponseNullMasterCatalogTest(){
    MasterCatalogDTO masterCatalogDTO = ProductAndItemsUtil.convertToMasterCatalogResponse(null);
    Assertions.assertNotNull(masterCatalogDTO);
  }

  @Test
  public void convertToMasterCatalogNullCategoryResponseTest(){
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(null);
    MasterCatalogDTO masterCatalogDTO = ProductAndItemsUtil.convertToMasterCatalogResponse(masterCatalog);
    Assertions.assertNotNull(masterCatalogDTO);
  }

}
