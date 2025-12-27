package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.B2bFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class AddDeleteVariantReconcileUtilTest {
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String SKU_CODE_1 = "skuCode1";
  private static final String SKU_CODE_2 = "skuCode2";
  private static final String SKU_CODE_3 = "skuCode3";
  private static final String SKU_CODE_4 = "skuCode4";
  private static final String ITEM_SKU_1 = "itemSku1";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String ITEM_SKU_3 = "itemSku3";
  private static final String PICKUP_POINT_CODE_1 = "pickupPointCode1";

  private AddDeleteVariantRetryRequest addDeleteVariantRetryRequest;
  private ProductDetailResponse productDetailResponse;
  private ProductCollectionsVo productCollectionsVo;

  @BeforeEach
  public void setUp() {
      addDeleteVariantRetryRequest = getAddDeleteVariantRetryRequest();
      productDetailResponse = getProductDetailResponse();
      productCollectionsVo = getProductCollectionsVo();
  }

  @Test
  public void deleteExtraItemsAndItemPickupPointsTest() {
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.deleteExtraItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo);
   Assertions.assertEquals(1, result.size());
   Assertions.assertTrue(result.get(0).isMarkForDelete());
   Assertions.assertTrue(productCollectionsVo.getItems().get(0).isMarkForDelete());
   Assertions.assertTrue(productCollectionsVo.getItems().get(0).isPermanentDelete());
   Assertions.assertTrue(productCollectionsVo.getItems().get(2).isMarkForDelete());
   Assertions.assertTrue(productCollectionsVo.getItems().get(2).isPermanentDelete());
  }

  @Test
  public void deleteExtraItemsAndItemPickupPointsEmptyItemsInXProductTest() {
    productCollectionsVo.setItems(new ArrayList<>());
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.deleteExtraItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo);
  }

  @Test
  public void deleteExtraItemsAndItemPickupPointsEmptyItemsInPCBTest() {
    productDetailResponse.setProductItemResponses(new HashSet<>());
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.deleteExtraItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo);
   Assertions.assertEquals(0, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsTest() {
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setSkuCode(SKU_CODE_4);
    productDetailResponse.setProductItemResponses(Set.of(productItemResponse1));

    productCollectionsVo.setItemPickupPoints(new ArrayList<>());


    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setListPrice(10.0);
    priceRequest.setOfferPrice(10.0);
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
    itemViewConfigRequest.setBuyable(true);
    itemViewConfigRequest.setDiscoverable(true);
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setBasePrice(10.0);
    ItemPickupPointActivationRequest itemPickupPointActivationRequest =
        ItemPickupPointActivationRequest.builder().pickupPointCode(PICKUP_POINT_CODE_1).price(Set.of(priceRequest))
            .itemViewConfigs(Set.of(itemViewConfigRequest)).b2bFields(b2bFieldsRequest).build();
    ItemActivationRequest itemActivationRequest =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_4).itemSku(ITEM_SKU_1)
            .itemPickupPoints(Arrays.asList(itemPickupPointActivationRequest)).build();

    addDeleteVariantRetryRequest.setItemActivationRequestList(Arrays.asList(itemActivationRequest));

    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);

   Assertions.assertEquals(1, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsB2BNullTest() {
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setSkuCode(SKU_CODE_4);
    productDetailResponse.setProductItemResponses(Set.of(productItemResponse1));

    productCollectionsVo.setItemPickupPoints(new ArrayList<>());


    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setListPrice(10.0);
    priceRequest.setOfferPrice(10.0);
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
    itemViewConfigRequest.setBuyable(true);
    itemViewConfigRequest.setDiscoverable(true);
    B2bFieldsRequest b2bFieldsRequest = null;
    ItemPickupPointActivationRequest itemPickupPointActivationRequest =
        ItemPickupPointActivationRequest.builder().pickupPointCode(PICKUP_POINT_CODE_1).price(Set.of(priceRequest))
            .itemViewConfigs(Set.of(itemViewConfigRequest)).b2bFields(b2bFieldsRequest).build();
    ItemActivationRequest itemActivationRequest =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_4).itemSku(ITEM_SKU_1)
            .itemPickupPoints(Arrays.asList(itemPickupPointActivationRequest)).build();

    addDeleteVariantRetryRequest.setItemActivationRequestList(Arrays.asList(itemActivationRequest));

    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);

   Assertions.assertEquals(1, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsEmptyPcbItemsTest() {
    productDetailResponse.setProductItemResponses(new HashSet<>());
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);
   Assertions.assertEquals(0, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsEmptyItemsInRequestTest() {
    addDeleteVariantRetryRequest.setItemActivationRequestList(new ArrayList<>());
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);
   Assertions.assertEquals(0, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsL4AndL5AlreadyExistsTest() {
    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);

   Assertions.assertEquals(0, result.size());
  }

  @Test
  public void createMissingItemsAndItemPickupPointsMissingItemNotPresentInRequestTest() {
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setSkuCode(SKU_CODE_4);
    productDetailResponse.setProductItemResponses(Set.of(productItemResponse1));

    productCollectionsVo.setItemPickupPoints(new ArrayList<>());


    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setListPrice(10.0);
    priceRequest.setOfferPrice(10.0);
    ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();
    itemViewConfigRequest.setBuyable(true);
    itemViewConfigRequest.setDiscoverable(true);
    B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
    b2bFieldsRequest.setBasePrice(10.0);
    ItemPickupPointActivationRequest itemPickupPointActivationRequest =
        ItemPickupPointActivationRequest.builder().pickupPointCode(PICKUP_POINT_CODE_1).price(Set.of(priceRequest))
            .itemViewConfigs(Set.of(itemViewConfigRequest)).b2bFields(b2bFieldsRequest).build();
    ItemActivationRequest itemActivationRequest =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_1).itemSku(ITEM_SKU_1)
            .itemPickupPoints(Arrays.asList(itemPickupPointActivationRequest)).build();

    addDeleteVariantRetryRequest.setItemActivationRequestList(Arrays.asList(itemActivationRequest));

    List<ItemPickupPoint> result =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);

   Assertions.assertEquals(0, result.size());
  }

  @Test
  public void setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlagTest() {
    AddDeleteVariantReconcileUtil.setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlag(productCollectionsVo);
   Assertions.assertTrue(productCollectionsVo.getProducts().get(0).isCncActivated());
   Assertions.assertTrue(productCollectionsVo.getProducts().get(0).isOnline());
   Assertions.assertTrue(productCollectionsVo.getProducts().get(0).isB2bActivated());
  }

  @Test
  public void setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlagProductNotB2BTest() {
    productCollectionsVo.getItemPickupPoints().forEach(itemPickupPoint -> {
      itemPickupPoint.setCncActive(false);
      itemPickupPoint.getAllItemViewConfigs().forEach(itemViewConfig -> {
        itemViewConfig.setDiscoverable(false);
        itemViewConfig.setBuyable(false);
      });
    });
    AddDeleteVariantReconcileUtil.setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlag(productCollectionsVo);
   Assertions.assertFalse(productCollectionsVo.getProducts().get(0).isCncActivated());
   Assertions.assertFalse(productCollectionsVo.getProducts().get(0).isOnline());
   Assertions.assertFalse(productCollectionsVo.getProducts().get(0).isB2bActivated());
  }

  private AddDeleteVariantRetryRequest getAddDeleteVariantRetryRequest() {
    ItemActivationRequest itemActivationRequest1 =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_1).itemSku(ITEM_SKU_1)
            .markForDelete(true).build();
    ItemActivationRequest itemActivationRequest2 =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_2).itemSku(ITEM_SKU_2).build();
    ItemActivationRequest itemActivationRequest3 =
        ItemActivationRequest.builder().merchantCode(MERCHANT_CODE).itemCode(SKU_CODE_3).itemSku(ITEM_SKU_3).build();

    return AddDeleteVariantRetryRequest.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU)
        .itemActivationRequestList(
            Arrays.asList(itemActivationRequest1, itemActivationRequest2, itemActivationRequest3)).build();
  }

  private ProductDetailResponse getProductDetailResponse() {
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setSkuCode(SKU_CODE_1);
    productItemResponse1.setMarkForDelete(true);

    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setSkuCode(SKU_CODE_2);

    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(Set.of(productItemResponse1, productItemResponse2));

    return productDetailResponse;
  }

  private ProductCollectionsVo getProductCollectionsVo() {
    Product product = new Product();

    Item item1 = new Item();
    item1.setItemCode(SKU_CODE_1);
    item1.setItemSku(ITEM_SKU_1);

    Item item2 = new Item();
    item2.setItemCode(SKU_CODE_2);
    item2.setItemSku(ITEM_SKU_2);

    Item item3 = new Item();
    item3.setItemCode(SKU_CODE_3);
    item3.setItemSku(ITEM_SKU_3);

    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setChannel(Constants.DEFAULT_CHANNEL);

    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setDiscoverable(true);
    itemViewConfig2.setChannel(Constants.DEFAULT_CHANNEL);

    ItemViewConfig itemViewConfig3 = new ItemViewConfig();
    itemViewConfig3.setChannel(Constants.DEFAULT_CHANNEL);

    ItemViewConfig itemViewConfig4 = new ItemViewConfig();
    itemViewConfig4.setChannel(Constants.B2B);
    itemViewConfig4.setBuyable(true);

    ItemViewConfig itemViewConfig5 = new ItemViewConfig();
    itemViewConfig5.setChannel(Constants.B2B);
    itemViewConfig5.setDiscoverable(true);


    ItemPickupPoint itemPickupPoint1 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_1).pickupPointCode(PICKUP_POINT_CODE_1).cncActive(true)
            .itemViewConfig(Set.of(itemViewConfig1)).price(new HashSet<>()).build();
    ItemPickupPoint itemPickupPoint2 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_2).pickupPointCode(PICKUP_POINT_CODE_1)
            .itemViewConfig(Set.of(itemViewConfig2, itemViewConfig5)).price(new HashSet<>()).build();
    ItemPickupPoint itemPickupPoint3 =
        ItemPickupPoint.builder().itemSku(ITEM_SKU_2).pickupPointCode(PICKUP_POINT_CODE_1)
            .itemViewConfig(Set.of(itemViewConfig3, itemViewConfig4)).price(new HashSet<>()).build();


    return new ProductCollectionsVo(Arrays.asList(product), new ArrayList<>(Arrays.asList(item1, item2, item3)),
        new ArrayList<>(Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint3)));
  }



}
