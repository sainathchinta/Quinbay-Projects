package com.gdn.mta.product.service.converter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;


public class UpdateProductItemLevel3ModelConverterBeanTest {

  private static final Integer AVAILABLE_STOCK = 0;
  private static final Boolean IS_SYNC_STOCK = false;
  private static final Boolean IS_O2O = false;
  private static final Boolean IS_DISPLAYABLE = false;
  private static final Boolean IS_BUYABLE = false;
  private static final Boolean LATE_FULFILLMENT = false;
  private static final String MERCHANT_SKU = "test123";
  private static final String ATTRIBUTE_NAME = "name";
  private static final String VALUE = "value";
  private static final Double PRICE = 1000.0;
  private static final Double SALE_PRICE = 1000.0;
  private static final String PRODUCT_NAME = "Produk testing";
  private static final String PRODUCT_DESC = "Desc";
  private static final Integer PRODUCT_TYPE_REGULAR = 1;
  private static final Integer PRODUCT_TYPE_BIG_PRODUCT = 2;
  private static final Integer PRODUCT_TYPE_BOPIS = 3;
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "ppCode";

  UpdateProductItemLevel3ModelConverterBean converter = new UpdateProductItemLevel3ModelConverterBean();

  private ProductLevel3Summary generateProductLevel3Summary() {
    ProductLevel3Summary product =
        ProductLevel3Summary.builder().availableStockLevel2(AVAILABLE_STOCK).synchronizeStock(IS_SYNC_STOCK)
            .off2OnActiveFlag(IS_O2O).merchantSku(MERCHANT_SKU).lateFulfillment(LATE_FULFILLMENT).build();
    List<ProductLevel3Price> productPrices = new ArrayList<>();
    ProductLevel3Price productPrice = new ProductLevel3Price();
    productPrice.setPrice(PRICE);
    productPrice.setSalePrice(SALE_PRICE);
    productPrices.add(productPrice);
    product.setPrices(productPrices);

    List<ProductLevel3ViewConfig> productViewConfigs = new ArrayList<>();
    ProductLevel3ViewConfig productViewConfig = new ProductLevel3ViewConfig();
    productViewConfig.setDisplay(IS_DISPLAYABLE);
    productViewConfig.setBuyable(IS_BUYABLE);
    productViewConfigs.add(productViewConfig);
    product.setViewConfigs(productViewConfigs);
    product.setPickupPointCode("pickup1");
    product.setOff2OnActiveFlag(true);

    return product;
  }

  private ProductLevel3 generateProductLevel3() {
    ProductLevel3 product = new ProductLevel3();
    product.setProductName(PRODUCT_NAME);
    product.setDescription(PRODUCT_DESC);
    product.setProductType(PRODUCT_TYPE_REGULAR);

    List<ProductItemLevel3> productItems = new ArrayList<>();
    ProductItemLevel3 productItem = new ProductItemLevel3();
    productItem.setAvailableStockLevel2(AVAILABLE_STOCK);
    productItem.setSynchronizeStock(IS_SYNC_STOCK);
    productItem.setOff2OnActiveFlag(IS_O2O);
    productItem.setMerchantSku(MERCHANT_SKU);


    List<ProductLevel3Price> productPrices = new ArrayList<>();
    ProductLevel3Price productPrice = new ProductLevel3Price();
    productPrice.setPrice(PRICE);
    productPrice.setSalePrice(SALE_PRICE);
    productPrices.add(productPrice);
    productItem.setPrices(productPrices);

    List<ProductLevel3ViewConfig> productViewConfigs = new ArrayList<>();
    ProductLevel3ViewConfig productViewConfig = new ProductLevel3ViewConfig();
    productViewConfig.setDisplay(IS_DISPLAYABLE);
    productViewConfig.setBuyable(IS_BUYABLE);
    productViewConfigs.add(productViewConfig);
    productItem.setViewConfigs(productViewConfigs);

    productItems.add(productItem);
    product.setItems(productItems);

    ProductLevel3Attribute productAttribute = new ProductLevel3Attribute();
    productAttribute.setAttributeName(ATTRIBUTE_NAME);
    productAttribute.setValues(Arrays.asList(VALUE));
    product.setAttributes(Arrays.asList(productAttribute));

    return product;
  }

  private ProductLevel3DetailResponse generateProductLevel3DetailResponse() {
    ProductLevel3AttributeResponse attributeResponse = new ProductLevel3AttributeResponse();
    attributeResponse.setValues(Collections.singletonList(PRODUCT_DESC));
    ProductLevel3DetailResponse product =
        ProductLevel3DetailResponse.builder().productName(PRODUCT_NAME).description(PRODUCT_DESC)
            .productType(PRODUCT_TYPE_REGULAR).attributes(Arrays.asList(attributeResponse))
            .uniqueSellingPoint(PRODUCT_DESC)
            .productLevel3Logistics(Collections.singletonList(new ProductItemLevel3LogisticResponse()))
            .isLateFulfillment(true).width(2.0).length(2.0).height(2.0).weight(10.0).installationRequired(false)
            .pickupPointCodes(Collections.singletonList(PRODUCT_DESC)).build();
    return product;
  }

  private ProductAndItemsResponse generateProductAndItemsResponse() {
    ProductAndItemsResponse result = new ProductAndItemsResponse();
    ProductResponse product = new ProductResponse();
    product.setProductType(ProductType.REGULAR);
    MasterDataProductDTO masterData = new MasterDataProductDTO();
    masterData.setProductName(PRODUCT_NAME);
    masterData.setDescription(PRODUCT_DESC);
    product.setMasterDataProduct(masterData);
    result.setProduct(product);

    List<ItemResponse> productItems = new ArrayList<>();
    ItemResponse productItem = new ItemResponse();
    productItem.setSynchronized(IS_SYNC_STOCK);
    productItem.setOff2OnChannelActive(IS_O2O);

    List<PriceDTO> productItemPrices = new ArrayList<>();
    PriceDTO productItemPrice1 = new PriceDTO();
    productItemPrice1.setListPrice(PRICE);
    productItemPrice1.setOfferPrice(SALE_PRICE);
    productItemPrice1.setChannel(ChannelName.DEFAULT.name());

    PriceDTO productItemPrice2 = new PriceDTO();
    productItemPrice2.setListPrice(PRICE);
    productItemPrice2.setOfferPrice(SALE_PRICE);
    productItemPrice2.setChannel(ChannelName.DESKTOP_WEB.name());

    productItemPrices.add(productItemPrice1);
    productItemPrices.add(productItemPrice2);
    productItem.setPrice(new HashSet<>(productItemPrices));

    List<ItemViewConfigDTO> productItemViewConfigs = new ArrayList<>();
    ItemViewConfigDTO productItemViewConfig1 = new ItemViewConfigDTO();
    productItemViewConfig1.setBuyable(IS_BUYABLE);
    productItemViewConfig1.setDiscoverable(IS_DISPLAYABLE);
    productItemViewConfig1.setChannel(ChannelName.DEFAULT.name());

    ItemViewConfigDTO productItemViewConfig2 = new ItemViewConfigDTO();
    productItemViewConfig2.setBuyable(IS_BUYABLE);
    productItemViewConfig2.setDiscoverable(IS_DISPLAYABLE);
    productItemViewConfig2.setChannel(ChannelName.DESKTOP_WEB.name());

    productItemViewConfigs.add(productItemViewConfig1);
    productItemViewConfigs.add(productItemViewConfig2);
    productItem.setItemViewConfigs(new HashSet<>(productItemViewConfigs));

    productItems.add(productItem);
    result.setItems(productItems);

    productItems.add(productItem);
    return result;
  }

  @Test
  public void testConvertFromProductLevel3SummaryWholesaleFlag() {
    ProductLevel3Summary productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(Boolean.TRUE);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(productLevel3Summary);
    Assertions.assertEquals(Constants.ACTIVE, model.getWholesalePriceActivated());
  }

  @Test
  public void testConvertFromProductLevel3Summary_withDefaultAndCncViewConfigs() {
    ReflectionTestUtils.setField(converter, "cncForWarehouseFeatureSwitch", true);
    ProductLevel3Summary productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.getViewConfigs().add(
        ProductLevel3ViewConfig.builder().channelId(Constants.CNC_CHANNEL).buyable(Boolean.TRUE).display(Boolean.TRUE).build());
    productLevel3Summary.setWholesalePriceActivated(Boolean.TRUE);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(productLevel3Summary);
    Assertions.assertEquals(Constants.ACTIVE, model.getWholesalePriceActivated());
    Assertions.assertTrue(model.getCncBuyable());
    Assertions.assertTrue(model.getCncDisplayable());
  }

  @Test
  public void testConvertFromProductLevel3SummaryWholesaleActivatedFalseFlag() {
    ProductLevel3Summary productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(Boolean.FALSE);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(productLevel3Summary);
    Assertions.assertEquals(Constants.INACTIVE, model.getWholesalePriceActivated());
  }

  @Test
  public void testConvertFromProductLevel3SummaryWholesaleFlagNull() {
    ProductLevel3Summary productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.setWholesalePriceActivated(null);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(productLevel3Summary);
    Assertions.assertEquals(Constants.INACTIVE, model.getWholesalePriceActivated());
  }

  @Test
  public void testConvertFromProductLevel3Summary() {
    ProductLevel3Summary productLevel3Summary = generateProductLevel3Summary();
    productLevel3Summary.setLateFulfillment(true);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(productLevel3Summary);
    Assertions.assertEquals( "pickup1", model.getPickupPointCode());
    Assertions.assertTrue(model.getOff2OnActiveFlag(), "OffToOn should be true");
  }

  @Test
  public void testConvertFromProductLevel3Summary_WithoutPrices() {
    ProductLevel3Summary product = generateProductLevel3Summary();
    product.setPrices(null);
    converter.convertFromProductLevel3Summary(product);
  }

  @Test
  public void testConvertFromProductLevel3Summary_WithMinStock() {
    ProductLevel3Summary product = generateProductLevel3Summary();
    product.setMinimumStockLevel2(1);
    UpdateProductItemLevel3Model model = converter.convertFromProductLevel3Summary(product);
    Assertions.assertEquals(model.getMinimumStock(), "1");
  }

  @Test
  public void testConvertFromProductLevel3Summary_WithoutViewConfigs() {
    ProductLevel3Summary product = generateProductLevel3Summary();
    product.setViewConfigs(null);
    converter.convertFromProductLevel3Summary(product);
  }

  @Test
  public void testConvertFromProductLevel3() {
    ProductLevel3 productItem = generateProductLevel3();
    productItem.setInstallationRequired(true);
    productItem.setProductType(null);
    productItem.getItems().get(0).setMinimumStock(2);
    productItem.getItems().get(0).setWholesalePriceActivated(true);
    converter.convertFromProductLevel3(productItem, false);
  }

  @Test
  public void testConvertFromProductLevelWhitMapWithAttributeCodeTrue() {
    ProductLevel3 productItem = generateProductLevel3();
    productItem.getItems().get(0).setMinimumStock(2);
    productItem.getItems().get(0).setWholesalePriceActivated(true);
    converter.convertFromProductLevel3(productItem, true);
  }

  @Test
  public void testConvertFromProductLevel3_WithoutProductItems() {
    ProductLevel3 product = generateProductLevel3();
    product.setItems(null);
    converter.convertFromProductLevel3(product, false);
  }

  @Test
  public void testConvertFromProductLevel3_WithoutPrices() {
    ProductLevel3 product = generateProductLevel3();
    product.setProductType(PRODUCT_TYPE_BIG_PRODUCT);
    ProductItemLevel3 productItem = product.getItems().get(0);
    productItem.setPrices(null);
    productItem.setMinimumStock(2);
    converter.convertFromProductLevel3(product, false);
  }

  @Test
  public void testConvertFromProductLevel3_WithPrices() {
    ProductLevel3 product = generateProductLevel3();
    product.setProductType(PRODUCT_TYPE_BIG_PRODUCT);
    ProductItemLevel3 productItem = product.getItems().get(0);
    List<ProductLevel3Price> productPrices = new ArrayList<>();
    ProductLevel3Price productPrice = new ProductLevel3Price();
    productPrice.setPrice(PRICE);
    productPrice.setSalePrice(SALE_PRICE);
    productPrices.add(productPrice);
    productItem.setPrices(productPrices);
    productItem.setMinimumStock(2);
    UpdateProductItemLevel3Model updateProductItemLevel3Model = converter.convertFromProductLevel3(product, false);
    Assertions.assertEquals(PRICE, updateProductItemLevel3Model.getPrice());
    Assertions.assertEquals(SALE_PRICE, updateProductItemLevel3Model.getSalePrice());
  }

  @Test
  public void testConvertFromProductLevel3_WithoutViewConfigs() {
    ProductLevel3 product = generateProductLevel3();
    product.setProductType(PRODUCT_TYPE_BOPIS);
    ProductItemLevel3 productItem = product.getItems().get(0);
    productItem.setViewConfigs(null);
    productItem.setDangerousGoodsLevel(1);
    productItem.setPickupPointCode("12345");
    productItem.setWeight(12.0);
    productItem.setWidth(11.0);
    productItem.setHeight(12.0);
    productItem.setLength(10.0);
    productItem.setMinimumStock(2);
    UpdateProductItemLevel3Model response = converter.convertFromProductLevel3(product, false);
    Assertions.assertEquals(11, response.getWidth().intValue());
    Assertions.assertEquals(10, response.getLength().intValue());
    Assertions.assertEquals(1, response.getDangerousGoodsLevel().intValue());
  }

  @Test
  public void testConvertFromProductLevel3NeedCorrection() {
    ProductLevel3 product = generateProductLevel3();
    product.setProductType(PRODUCT_TYPE_BOPIS);
    product.setNeedCorrection(true);
    product.setLength(10.0);
    product.setWidth(11.0);
    product.setWeight(12.0);
    product.setHeight(11.0);
    product.setDangerousGoodsLevel(1);
    product.setItems(new ArrayList<>());
    UpdateProductItemLevel3Model response = converter.convertFromProductLevel3(product, false);
    Assertions.assertEquals(11, response.getWidth().intValue());
    Assertions.assertEquals(10, response.getLength().intValue());
    Assertions.assertEquals(1, response.getDangerousGoodsLevel().intValue());
  }

  @Test
  public void testConvertFromProductLevel3NeedCorrectionDimensionNull() {
    ProductLevel3 product = generateProductLevel3();
    product.setProductType(PRODUCT_TYPE_BOPIS);
    product.setNeedCorrection(true);
    product.setLength(null);
    product.setWidth(null);
    product.setWeight(null);
    product.setHeight(null);
    product.setDangerousGoodsLevel(null);
    product.setItems(new ArrayList<>());
    UpdateProductItemLevel3Model response = converter.convertFromProductLevel3(product, false);
    Assertions.assertEquals(null, response.getWidth());
    Assertions.assertEquals(null, response.getLength());
    Assertions.assertEquals(null, response.getDangerousGoodsLevel());
  }

  @Test
  public void testConvertFromProductAndItemsResponse() {
    ProductAndItemsResponse product = generateProductAndItemsResponse();
    converter.convertFromProductAndItemsResponse(product);
  }

  @Test
  public void testConvertFromProductAndItemsResponse_WithoutPrices() {
    ProductAndItemsResponse product = generateProductAndItemsResponse();
    ItemResponse productItem = product.getItems().get(0);
    productItem.setPrice(null);
    converter.convertFromProductAndItemsResponse(product);
  }

  @Test
  public void testConvertFromProductAndItemsResponse_WithoutViewConfigs() {
    ProductAndItemsResponse product = generateProductAndItemsResponse();
    ItemResponse productItem = product.getItems().get(0);
    productItem.setItemViewConfigs(null);
    converter.convertFromProductAndItemsResponse(product);
  }

  @Test
  public void testConvertFromProductLevel3Inventory() {
    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    inventory.setWebAvailable(AVAILABLE_STOCK);
    converter.convertFromProductLevel3Inventory(inventory);
  }

  @Test
  public void testConvertFromProductLevel3Detail() {
    ProductLevel3DetailResponse productLevel3DetailResponse = generateProductLevel3DetailResponse();
    converter.convertFromProductLevel3Detail(productLevel3DetailResponse, false);
  }

  @Test
  public void testConvertFromProductLevel3DetailWithMapWithAttributeCodeTrue() {
    ProductLevel3DetailResponse productLevel3DetailResponse = generateProductLevel3DetailResponse();
    converter.convertFromProductLevel3Detail(productLevel3DetailResponse, true);
  }

  @Test
  public void testConvertFromProductLevel3Detail_2() {
    ProductLevel3DetailResponse productLevel3DetailResponse = generateProductLevel3DetailResponse();
    productLevel3DetailResponse.setProductType(PRODUCT_TYPE_BIG_PRODUCT);
    productLevel3DetailResponse.setFreeSample(true);
    UpdateProductItemLevel3Model response = converter.convertFromProductLevel3Detail(productLevel3DetailResponse, false);
    Assertions.assertTrue(response.getFreeSample());
  }

  @Test
  public void testConvertFromProductLevel3Detail_3() {
    ProductLevel3DetailResponse productLevel3DetailResponse = generateProductLevel3DetailResponse();
    productLevel3DetailResponse.setFreeSample(false);
    productLevel3DetailResponse.setProductType(PRODUCT_TYPE_BOPIS);
    UpdateProductItemLevel3Model response = converter.convertFromProductLevel3Detail(productLevel3DetailResponse, false);
    Assertions.assertFalse(response.getFreeSample());
  }

  @Test
  public void testConvertFromProductLevel3Detail_nullAttribute() {
    ProductLevel3DetailResponse productLevel3DetailResponse = generateProductLevel3DetailResponse();
    productLevel3DetailResponse.setProductType(4);
    productLevel3DetailResponse.setAttributes(Collections.emptyList());
    converter.convertFromProductLevel3Detail(productLevel3DetailResponse, false);
  }

  @Test
  public void baseObjectCoverterTestOk() {
    converter.checkSourceObject("abc");
  }

  @Test
  public void baseObjectCoverterTestException() {
    try {
      converter.checkSourceObject(null);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION.toString(), e.getErrorCodes().getCode());
    }
  }

  @Test
  public void convertFromItemPickupPointListingResponseTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setChannelId(ChannelName.DEFAULT.name());
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    PriceResponse priceResponse = new PriceResponse();
    itemPickupPointListingResponse.setPrices(Collections.singletonList(priceResponse));
    converter.convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointListingResponseTestCnc() {
    ReflectionTestUtils.setField(converter, "cncForWarehouseFeatureSwitch", true);
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setChannelId(ChannelName.DEFAULT.name());
    ViewConfigResponse viewConfigResponseCnc = new ViewConfigResponse();
    viewConfigResponseCnc.setChannelId(ChannelName.CNC.name());
    itemPickupPointListingResponse.setViewConfigs(List.of(viewConfigResponse, viewConfigResponseCnc));
    PriceResponse priceResponse = new PriceResponse();
    itemPickupPointListingResponse.setPrices(Collections.singletonList(priceResponse));
    converter.convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointListingResponseFbbTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    PriceResponse priceResponse = new PriceResponse();
    itemPickupPointListingResponse.setPrices(Collections.singletonList(priceResponse));
    itemPickupPointListingResponse.setFbbActive(true);
    converter.convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointListingResponseNewTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    PriceResponse priceResponse = new PriceResponse();
    itemPickupPointListingResponse.setPrices(Collections.singletonList(priceResponse));
    itemPickupPointListingResponse.setWholesalePriceActivated(true);
    converter.convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointListingResponseWholesaleAddedTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    itemPickupPointListingResponse.setViewConfigs(Collections.singletonList(viewConfigResponse));
    PriceResponse priceResponse = new PriceResponse();
    itemPickupPointListingResponse.setPrices(Collections.singletonList(priceResponse));
    itemPickupPointListingResponse.setWholesalePriceActivated(true);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    Map<String, Boolean> wholesaleAdded1stTime = new HashMap<>();
    wholesaleAdded1stTime
        .putIfAbsent(itemPickupPointListingResponse.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE, true);
    converter.convertFromItemPickupPointListingResponse(itemPickupPointListingResponse, wholesaleAdded1stTime);
  }

  @Test
  public void convertFromItemPickupPointTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    converter.convertFromItemPickupPoint(itemPickupPoint, new HashSet<>(), new UpdateProductItemLevel3Model(),
        new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointTestCnc() {
    ReflectionTestUtils.setField(converter, "cncForWarehouseFeatureSwitch", true);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    viewConfigResponse.setChannel(ChannelName.CNC.name());
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    converter.convertFromItemPickupPoint(itemPickupPoint, new HashSet<>(), new UpdateProductItemLevel3Model(),
        new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointFbbTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    itemPickupPoint.setFbbActivated(true);
    converter.convertFromItemPickupPoint(itemPickupPoint, new HashSet<>(), new UpdateProductItemLevel3Model(),
        new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPointWholeSaleTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    converter.convertFromItemPickupPoint(itemPickupPoint,
        Collections.singleton(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE), new UpdateProductItemLevel3Model(),
        new HashMap<>());
  }

  @Test
  public void convertFromItemPickupPoint1stTimeWholeSaleFalseTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    Map<String, Boolean> wholesaleAdded1stTime = new HashMap<>();
    wholesaleAdded1stTime
        .putIfAbsent(itemPickupPoint.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE, false);
    converter.convertFromItemPickupPoint(itemPickupPoint,
        Collections.singleton(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE), new UpdateProductItemLevel3Model(),
        wholesaleAdded1stTime);
  }

  @Test
  public void convertFromItemPickupPoint1stTimeWholeSaleTrueTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    viewConfigResponse.setChannel(ChannelName.DEFAULT.name());
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    Map<String, Boolean> wholesaleAdded1stTime = new HashMap<>();
    wholesaleAdded1stTime
        .putIfAbsent(itemPickupPoint.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE, true);
    converter.convertFromItemPickupPoint(itemPickupPoint,
        Collections.singleton(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE), new UpdateProductItemLevel3Model(),
        wholesaleAdded1stTime);
  }

  @Test
  public void convertFromItemPickupPointWholeSaleTrueTest() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig viewConfigResponse = new ItemViewConfig();
    itemPickupPoint.setItemViewConfig(Collections.singleton(viewConfigResponse));
    Price priceResponse = new Price();
    itemPickupPoint.setPrice(Collections.singleton(priceResponse));
    UpdateProductItemLevel3Model updateProductItemLevel3Model = new UpdateProductItemLevel3Model();
    updateProductItemLevel3Model.setWholesalePriceActivated(Constants.ACTIVE);
    converter.convertFromItemPickupPoint(itemPickupPoint,
        Collections.singleton(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE), updateProductItemLevel3Model,
        new HashMap<>());
  }

}
