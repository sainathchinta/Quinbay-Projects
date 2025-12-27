package com.gdn.partners.pbp.helper;

import com.gda.mta.product.dto.CategoryDetailDto;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductAndItemPickupPontL5Response;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCodeAndIdAndStateResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductItemsCogs;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.master.model.PriceUpdateCriteria;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.BuyableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.DiscoverableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import com.gdn.mta.product.util.BeanUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static com.gdn.partners.pbp.commons.util.SolrFieldNames.CATEGORY_NAME;
import static com.gdn.partners.pbp.commons.util.SolrFieldNames.ID;
import static com.gdn.partners.pbp.commons.util.SolrFieldNames.PICKUP_POINT_CODE;

public class ResponseHelperTest {
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_SKU_CODE = "MTA-0000001-00001";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001-00001";
  private static final String DEFAULT_PICKUP_POINT_CODE = "PP-0000001";
  private static final String ITEM_SKU = "TOT-15014-0001-0001";
  private static final String ITEM_SKU_2 = "TOT-15014-0001-0002";
  private static final String DEFAULT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_ATTRIBUTE_CODE = "AT-0000001";
  private static final String VALUE = "S";
  private static final String VALUE_TYPE = "UK";
  private static final String VALUE_DELIMITER = "\\ \\u200B- ";
  private static final String DEFAULT_ITEM_SKU = "SKU-1000";
  private static final String DEFAULT_LOCATION_PATH = "TEST";
  public static final String ITEM_NAME = "itemName";
  private static final String PRODUCT_BUSINESS_PARTNER_ID = "productBusinessPartnerId";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String PRODUCT_ITEM_ID_2 = "productItemId2";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String SKU_CODE = "skuCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final Double NORMAL_PRICE = 1000.00;
  private static final Double SALE_PRICE = 900.00;

  private static final Double NORMAL_PRICE_2 = 900.00;
  private static final Double SALE_PRICE_2 = 1000.00;
  private static final Double DISCOUNT_PRICE = 100.00;
  private static final String CAMPAIGN_CODE = "campaignCode";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final Double NEW_NORMAL_PRICE = 1100.00;
  private static final Double NEW_SALE_PRICE = 1000.00;
  private static final String CATEGORY_NAME_ENGLISH = "categoryNameEnglish";
  private static final String SIZE_CHART_DELIMITER = "-";
  private static final Date TEN_DAYS_AGO = new Date(System.currentTimeMillis() - (10 * 24 * 60 * 60 * 1000));
  private static final Date TEN_DAYS_AFTER = new Date(System.currentTimeMillis() + (10 * 24 * 60 * 60 * 1000));
  private static final boolean AI_GENERATED_BRAND = true;
  private static final boolean AI_GENERATED_CATEGORY = false;

  static class DummyResponse extends BaseResponse {}

  private ProductCollection productCollection;
  private Image image;
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
  private AttributeResponse attribute;
  private ItemPickupPointListingL3Request itemPickupPointListingL3Request;
  private ItemPickupPointListingResponse itemPickupPointListingResponse;
  private ProductItemResponse productItemResponse;
  private ItemImageResponse itemImageResponse;
  private ProductLevel3Inventory productLevel3Inventory;
  private CampaignPriceSkuResponse campaignPriceSkuResponse;
  private ProductItemWholesalePrice productItemWholesalePrice;
  private ProductItemWholesalePriceResponse productItemWholesalePriceResponse;
  private ProductItemsCogs productItemsCogs;
  private ProductDetailResponse productDetailResponse;
  private BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse;
  private CategoryResponse categoryResponse = new CategoryResponse();

  private ProductL5DetailResponse productL5DetailResponse = new ProductL5DetailResponse();
  private BuyableScheduleResponse buyableScheduleResponse;
  private DiscoverableScheduleResponse discoverableScheduleResponse;

  @BeforeEach
  public void setUp() {
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_ITEM_SKU);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT_CODE);
    productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner.setPrice(100.0);
    productItemBusinessPartner.setSalePrice(90.0);
    productItemBusinessPartner.setBuyable(true);
    productItemBusinessPartner.setDisplay(true);
    productItemBusinessPartner.setProductType(1);
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BUSINESS_PARTNER_CODE);
    productBusinessPartner.setId(PRODUCT_BUSINESS_PARTNER_ID);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner.setCategoryCode(DEFAULT_CATEGORY_CODE);

    productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);

    image = new Image();
    image.setMainImages(true);
    image.setLocationPath(DEFAULT_LOCATION_PATH);
    image.setSequence(1);
    image.setActive(true);
    image.setOriginalImage(false);

    Image image2 = new Image();
    image2.setMainImages(true);
    image2.setLocationPath(DEFAULT_LOCATION_PATH);
    image2.setSequence(1);
    image2.setMarkForDelete(true);

    Image image3 = new Image();
    image3.setMainImages(true);
    image3.setLocationPath(DEFAULT_LOCATION_PATH);
    image3.setSequence(1);
    image3.setOriginalImage(true);

    attribute = new AttributeResponse();
    attribute.setId(DEFAULT_ATTRIBUTE_ID);
    attribute.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);

    itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    itemPickupPointListingL3Request.setProductSku(DEFAULT_PRODUCT_SKU);

    itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setProductSku(DEFAULT_PRODUCT_SKU);
    itemPickupPointListingResponse.setItemSku(DEFAULT_ITEM_SKU);
    itemPickupPointListingResponse.setProductCode(DEFAULT_PRODUCT_CODE);
    itemPickupPointListingResponse.setSkuCode(DEFAULT_SKU_CODE);
    itemPickupPointListingResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);
    itemPickupPointListingResponse.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);
    itemPickupPointListingResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    itemPickupPointListingResponse.setProductType(ProductType.REGULAR);
    itemPickupPointListingResponse.setPrices(Arrays.asList(new PriceResponse()));
    itemPickupPointListingResponse.setViewConfigs(Arrays.asList(new ViewConfigResponse()));

    productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(DEFAULT_SKU_CODE);
    productItemResponse.setImages(Arrays.asList(image, image2, image3));
    productItemResponse.setId(PRODUCT_ITEM_ID);

    ImageResponse imageResponse1 = new ImageResponse();
    BeanUtils.copyProperties(image, imageResponse1);
    imageResponse1.setMainImage(image.isMainImages());

    ImageResponse imageResponse2 = new ImageResponse();
    BeanUtils.copyProperties(image2, imageResponse2);
    imageResponse2.setMainImage(image2.isMainImages());

    ImageResponse imageResponse3 = new ImageResponse();
    BeanUtils.copyProperties(image3, imageResponse3);
    imageResponse3.setMainImage(image3.isMainImages());

    itemImageResponse = new ItemImageResponse();
    itemImageResponse.setItemCode(DEFAULT_SKU_CODE);
    itemImageResponse.setImageResponses(Arrays.asList(imageResponse1, imageResponse2, imageResponse3));

    productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setWebItemSku(DEFAULT_ITEM_SKU);
    productLevel3Inventory.setWebPickupPointCode(DEFAULT_PICKUP_POINT_CODE);

    campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    campaignPriceSkuResponse.setPickUpPointCode(DEFAULT_PICKUP_POINT_CODE);

    productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(DEFAULT_ITEM_SKU);
    productItemWholesalePrice.setPickupPointCode(DEFAULT_PICKUP_POINT_CODE);

    productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();

    productItemsCogs = new ProductItemsCogs();

    productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(ImmutableSet.of(productItemResponse));

    businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
    businessPartnerPickupPointResponse.setCode(DEFAULT_PICKUP_POINT_CODE);
    businessPartnerPickupPointResponse.setName(DEFAULT_PICKUP_POINT_CODE);

    productL5DetailResponse.setItemSku(ITEM_SKU);
    productL5DetailResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productL5DetailResponse.setViewConfigs(Collections.singletonList(new ViewConfigResponse()));

    buyableScheduleResponse = new BuyableScheduleResponse();
    discoverableScheduleResponse = new DiscoverableScheduleResponse();
    buyableScheduleResponse.setBuyable(true);
    buyableScheduleResponse.setStartDateTime(TEN_DAYS_AGO);
    buyableScheduleResponse.setEndDateTime(TEN_DAYS_AFTER);
    discoverableScheduleResponse.setDiscoverable(true);
    discoverableScheduleResponse.setStartDateTime(TEN_DAYS_AGO);
    discoverableScheduleResponse.setEndDateTime(TEN_DAYS_AFTER);
  }

  @Test
  public void toItemPickupPointListingL3ResponseTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseTest_cncForWarehouseTrue() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setChannelId(com.gdn.partners.pbp.commons.constants.Constants.CNC_CHANNEL);
    viewConfigResponse.setDisplay(true);
    viewConfigResponse.setBuyable(true);
    itemPickupPointListingResponse.setViewConfigs(new ArrayList<>(Arrays.asList(viewConfigResponse)));
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, true, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseProductTypeTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.setProductType(null);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseProductType2Test() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.setProductType(null);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, true, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(1, itemPickupPointListingL3ResponseList.get(0).getProductType(), 0);
  }

  @Test
  public void toItemPickupPointListingL3ResponseOnlyDefaultViewConfigTrueTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.getViewConfigs().get(0).setChannelId(Constants.DEFAULT);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), true, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseValueTypeMapNotEmptyTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.getViewConfigs().get(0).setChannelId(Constants.DEFAULT);
    Map<String, String> map = new HashMap<>();
    map.put(DEFAULT_ATTRIBUTE_CODE + VALUE, VALUE_TYPE);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), true, false, false, false, map, StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseValueTypeMapNotEmpty3Test() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.getViewConfigs().get(0).setChannelId(Constants.DEFAULT);
    Map<String, String> attributesMap = new HashMap<>();
    attributesMap.put(DEFAULT_ATTRIBUTE_CODE, VALUE);
    itemPickupPointListingResponse.setAttributesMap(attributesMap);
    Map<String, String> map = new HashMap<>();
    map.put(DEFAULT_ATTRIBUTE_CODE + VALUE, VALUE_TYPE);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), true, false, false, false, map, VALUE_DELIMITER, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(itemPickupPointListingL3ResponseList.get(0).getAttributesMap().get(DEFAULT_ATTRIBUTE_CODE),
        VALUE_TYPE + VALUE_DELIMITER + VALUE);
  }

  @Test
  public void toItemPickupPointListingL3ResponseValueTypeMapNotEmpty4Test() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.getViewConfigs().get(0).setChannelId(Constants.DEFAULT);
    Map<String, String> attributesMap = new HashMap<>();
    attributesMap.put(VALUE_TYPE, VALUE);
    itemPickupPointListingResponse.setAttributesMap(attributesMap);
    Map<String, String> map = new HashMap<>();
    map.put(DEFAULT_ATTRIBUTE_CODE + VALUE, VALUE_TYPE);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), true, false, false, false, map, VALUE_DELIMITER, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponsenB2bFieldTest() {
    itemPickupPointListingResponse.setB2bFields(new B2BResponse(true, 1000.0));
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(1000.0, itemPickupPointListingL3ResponseList.get(0).getB2bFields().getBasePrice(), 0);
    Assertions.assertTrue(itemPickupPointListingL3ResponseList.get(0).getB2bFields().isManaged());
  }

  @Test
  public void toItemPickupPointListingL3ResponseFbbTrueTest() {
    itemPickupPointListingResponse.setFbbActiveAtL3Level(true);
    itemPickupPointListingResponse.setFbbActive(true);
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertTrue(itemPickupPointListingL3ResponseList.get(0).isFbbActivated());
  }

  @Test
  public void toItemPickupPointListingL3ResponseEditedImageTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setEdited(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedImageTest() {
    Set<PriceUpdateCriteria> priceUpdateCriteria = new HashSet<>();
    priceUpdateCriteria.addAll(Arrays.asList(PriceUpdateCriteria.IN_RANGE, PriceUpdateCriteria.ZERO_PERCENTAGE_DISCOUNT));
    campaignPriceSkuResponse.setPriceUpdateCriteria(priceUpdateCriteria);
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemPickupPointListingResponse.getViewConfigs().get(0).setBuyableScheduleResponse(buyableScheduleResponse);
    itemPickupPointListingResponse.getViewConfigs().get(0).setDiscoverableScheduleResponse(discoverableScheduleResponse);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(2, itemPickupPointListingL3ResponseList.get(0).getPriceUpdateCriteria().size());
    Assertions.assertEquals(TEN_DAYS_AGO, itemPickupPointListingL3ResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().getStartDateTime());
    Assertions.assertEquals(TEN_DAYS_AFTER, itemPickupPointListingL3ResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().getEndDateTime());
    Assertions.assertTrue(itemPickupPointListingL3ResponseList
        .get(0).getViewConfigs().get(0).getBuyableScheduleResponse().isBuyable());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedImageOverrideWholesalePriceTest() {
    Set<PriceUpdateCriteria> priceUpdateCriteria = new HashSet<>();
    priceUpdateCriteria.addAll(
        Arrays.asList(PriceUpdateCriteria.IN_RANGE, PriceUpdateCriteria.ZERO_PERCENTAGE_DISCOUNT));
    campaignPriceSkuResponse.setPriceUpdateCriteria(priceUpdateCriteria);
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, true, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(2, itemPickupPointListingL3ResponseList.get(0).getPriceUpdateCriteria().size());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedImageOverrideWholesalePriceListEmptyTest() {
    Set<PriceUpdateCriteria> priceUpdateCriteria = new HashSet<>();
    priceUpdateCriteria.addAll(
        Arrays.asList(PriceUpdateCriteria.IN_RANGE, PriceUpdateCriteria.ZERO_PERCENTAGE_DISCOUNT));
    campaignPriceSkuResponse.setPriceUpdateCriteria(priceUpdateCriteria);
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE), new ArrayList<>()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, true, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(2, itemPickupPointListingL3ResponseList.get(0).getPriceUpdateCriteria().size());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedImageOverrideWholesalePriceListEmptyPriceActivatedNullTest() {
    Set<PriceUpdateCriteria> priceUpdateCriteria = new HashSet<>();
    priceUpdateCriteria.addAll(
        Arrays.asList(PriceUpdateCriteria.IN_RANGE, PriceUpdateCriteria.ZERO_PERCENTAGE_DISCOUNT));
    campaignPriceSkuResponse.setPriceUpdateCriteria(priceUpdateCriteria);
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE), new ArrayList<>()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, true, false, new HashMap<>(), StringUtils.EMPTY, true);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
    Assertions.assertEquals(2, itemPickupPointListingL3ResponseList.get(0).getPriceUpdateCriteria().size());
    Assertions.assertNull(itemPickupPointListingL3ResponseList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedNrProductImageTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    image.setRevised(true);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedOriginalImageFalseTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    itemImageResponse.getImageResponses().get(0).setOriginalImage(false);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseRevisedOriginalImageTrueTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setRevised(true);
    itemImageResponse.getImageResponses().get(0).setOriginalImage(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseOriginalNullTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(false);
    itemImageResponse.getImageResponses().get(0).setEdited(true);
    itemImageResponse.getImageResponses().get(0).setRevised(false);
    itemImageResponse.getImageResponses().get(0).setOriginalImage(null);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }


  @Test
  public void toItemPickupPointListingL3ResponseNullTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(SKU_CODE, itemImageResponse), ImmutableMap.of(PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseNullPricingMppTrueTest() {
    itemPickupPointListingResponse.setWholesalePriceConfigEnabled(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(SKU_CODE, itemImageResponse), ImmutableMap.of(PRODUCT_CODE, new ProductCollection()),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseNeedCorrectionTest() {
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseNeedCorrectionPostliveTest() {
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setPostLive(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseNeedCorrectionEditedTest() {
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setEdited(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse), ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingL3ResponseNeedCorrectionEditedMPPFalseTest() {
    productCollection.setState(WorkflowStates.NEED_CORRECTION.getValue());
    productCollection.setEdited(true);
    List<ItemPickupPointListingL3Response> itemPickupPointListingL3ResponseList =
        ResponseHelper.toItemPickupPointListingL3Response(Arrays.asList(itemPickupPointListingResponse),
            ImmutableMap.of(DEFAULT_SKU_CODE, itemImageResponse),
            ImmutableMap.of(DEFAULT_PRODUCT_CODE, productCollection),
            ImmutableMap.of(DEFAULT_ITEM_SKU, Arrays.asList(productItemWholesalePriceResponse)),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                productLevel3Inventory),
            ImmutableMap.of(CommonUtils.getItemSkuAndPickupPointKey(DEFAULT_ITEM_SKU, DEFAULT_PICKUP_POINT_CODE),
                campaignPriceSkuResponse), false, false, false, false, new HashMap<>(), StringUtils.EMPTY, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingL3ResponseList.get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingL3ResponseList.get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingL3ResponseList.get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingL3ResponseList.get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingL3ResponseList.get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointListingResponseTest() {
    productItemBusinessPartner.setFbbActive(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(new ArrayList<>());
    profileResponse.setCompany(companyDTO);
    Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(productItemBusinessPartner),
            productBusinessPartner, productCollection, productDetailResponse,
            Arrays.asList(businessPartnerPickupPointResponse), Arrays.asList(productItemWholesalePrice),
            profileResponse, false, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingResponseList.getRight().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingResponseList.getRight().get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingResponseList.getRight().get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingResponseList.getRight().get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingResponseList.getRight().get(0).getCategoryCode());
    Assertions.assertTrue(itemPickupPointListingResponseList.getRight().get(0).isFbbActive());
    Assertions.assertFalse(itemPickupPointListingResponseList.getLeft());
  }

  @Test
  public void toItemPickupPointListingResponseB2bTest() {
    ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
    viewConfigResponse.setChannelId(com.gdn.partners.pbp.commons.constants.Constants.CNC_CHANNEL);
    viewConfigResponse.setDisplay(true);
    viewConfigResponse.setBuyable(true);
    itemPickupPointListingResponse.setViewConfigs(new ArrayList<>(Arrays.asList(viewConfigResponse)));
    productItemBusinessPartner.setFbbActive(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(productItemBusinessPartner),
            productBusinessPartner, productCollection, productDetailResponse,
            Arrays.asList(businessPartnerPickupPointResponse), Arrays.asList(productItemWholesalePrice),
            profileResponse, false, true);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingResponseList.getRight().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingResponseList.getRight().get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingResponseList.getRight().get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingResponseList.getRight().get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingResponseList.getRight().get(0).getCategoryCode());
    Assertions.assertTrue(itemPickupPointListingResponseList.getRight().get(0).isFbbActive());
    Assertions.assertFalse(itemPickupPointListingResponseList.getLeft());
  }

  @Test
  public void toItemPickupPointListingResponseB2BActivatedTest() {
    productBusinessPartner.setB2bActivated(true);
    productItemBusinessPartner.setFbbActive(true);
    Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(productItemBusinessPartner),
            productBusinessPartner, productCollection, productDetailResponse,
            Arrays.asList(businessPartnerPickupPointResponse), Arrays.asList(productItemWholesalePrice), null, false, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingResponseList.getRight().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingResponseList.getRight().get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingResponseList.getRight().get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingResponseList.getRight().get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingResponseList.getRight().get(0).getCategoryCode());
    Assertions.assertTrue(itemPickupPointListingResponseList.getRight().get(0).isFbbActive());
    Assertions.assertFalse(itemPickupPointListingResponseList.getLeft());
  }

  @Test
  public void toItemPickupPointListingResponseNullWsPriceTest() {
    Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(productItemBusinessPartner),
            productBusinessPartner, productCollection, productDetailResponse,
            Arrays.asList(businessPartnerPickupPointResponse), new ArrayList<>(), null, false, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingResponseList.getRight().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingResponseList.getRight().get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingResponseList.getRight().get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingResponseList.getRight().get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingResponseList.getRight().get(0).getCategoryCode());
    Assertions.assertFalse(itemPickupPointListingResponseList.getLeft());
  }

  @Test
  public void toItemPickupPointListingResponseWsPriceTest() {
    Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseList =
        ResponseHelper.toItemPickupPointListingResponse(Arrays.asList(productItemBusinessPartner),
            productBusinessPartner, productCollection, productDetailResponse,
            Arrays.asList(businessPartnerPickupPointResponse), Arrays.asList(productItemWholesalePrice), null, false, false);
    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, itemPickupPointListingResponseList.getRight().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_ITEM_SKU, itemPickupPointListingResponseList.getRight().get(0).getItemSku());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, itemPickupPointListingResponseList.getRight().get(0).getProductCode());
    Assertions.assertEquals(DEFAULT_SKU_CODE, itemPickupPointListingResponseList.getRight().get(0).getSkuCode());
    Assertions.assertEquals(DEFAULT_CATEGORY_CODE, itemPickupPointListingResponseList.getRight().get(0).getCategoryCode());
    Assertions.assertFalse(itemPickupPointListingResponseList.getLeft());
  }

  @Test
  public void generateCategoryNameIdAndHierarchyTest() throws Exception {
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponse.setName(CATEGORY_NAME);
    categoryResponse.setNameEnglish(CATEGORY_NAME_ENGLISH);
    categoryResponse.setId(ID);
    categoryResponseList.add(categoryResponse);
    categoryResponseList.add(categoryResponse);
    CategoryDetailDto categoryDetailDto =
      ResponseHelper.generateCategoryNameIdAndHierarchy(categoryResponseList);
    Assertions.assertEquals(CATEGORY_NAME, categoryDetailDto.getCategoryName());
  }

  @Test
  public void generateCategoryNameIdAndSingleListHierarchyTest() throws Exception {
    List<CategoryResponse> categoryResponseList = new ArrayList<>();
    categoryResponse.setName(CATEGORY_NAME);
    categoryResponse.setNameEnglish(CATEGORY_NAME_ENGLISH);
    categoryResponse.setId(ID);
    categoryResponseList.add(categoryResponse);
    CategoryDetailDto categoryDetailDto =
      ResponseHelper.generateCategoryNameIdAndHierarchy(categoryResponseList);
    Assertions.assertEquals(CATEGORY_NAME, categoryDetailDto.getCategoryName());
  }

  @Test
  public void populateQuickEditV2RequestTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
        .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
  }

  @Test
  public void populateQuickEditV2RequestNullTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(null);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
  }

  @Test
  public void populateQuickEditV2RequestNull2Test() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(null);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
  }

  @Test
  public void populateQuickEditV2Request_withPriceTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    DiscountPriceDTO discountPriceDTO =
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).campaignCode(CAMPAIGN_CODE).build();
    priceDTO.setMerchantPromoDiscountPrice(discountPriceDTO);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
    Assertions.assertEquals(DISCOUNT_PRICE, populatedRequest.getPrice().getDiscountAmount());
  }

  @Test
  public void populateQuickEditV2Request_SellerSkuTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    DiscountPriceDTO discountPriceDTO =
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).campaignCode(CAMPAIGN_CODE).build();
    priceDTO.setMerchantPromoDiscountPrice(discountPriceDTO);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertEquals(MERCHANT_SKU, quickEditV2Request.getSellerSku());
  }

  @Test
  public void populateQuickEditV2Request_NoPriceChangeTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setSalePrice(SALE_PRICE);
    productLevel3PriceRequest.setPrice(NORMAL_PRICE);
    productLevel3PriceRequest.setDiscountAmount(DISCOUNT_PRICE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
  }

  @Test
  public void populateQuickEditV2Request_NoDetailsChangeTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    DiscountPriceDTO discountPriceDTO =
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).campaignCode(CAMPAIGN_CODE).build();
    priceDTO.setMerchantPromoDiscountPrice(discountPriceDTO);
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    productLevel3PriceRequest.setDiscountAmount(DISCOUNT_PRICE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setSellerSku(MERCHANT_SKU);
    quickEditV2Request.setOff2OnActiveFlag(Boolean.TRUE);
    quickEditV2Request.setWholeSaleActivated(Boolean.TRUE);
    quickEditV2Request.setCncActive(Boolean.TRUE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertSame(quickEditV2Request, populatedRequest);
  }

  @Test
  public void populateQuickEditV2Request_PriceChangeTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    ItemRequest itemRequest = new ItemRequest();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
        .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    productLevel3PriceRequest.setPrice(NEW_NORMAL_PRICE);
    productLevel3PriceRequest.setDiscountAmount(DISCOUNT_PRICE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    quickEditV2Request.setSellerSku(MERCHANT_SKU);
    quickEditV2Request.setOff2OnActiveFlag(Boolean.TRUE);
    quickEditV2Request.setWholeSaleActivated(Boolean.TRUE);
    quickEditV2Request.setCncActive(Boolean.TRUE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertSame(quickEditV2Request, populatedRequest);
    Assertions.assertNotEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
    Assertions.assertEquals(SALE_PRICE, populatedRequest.getPrice().getSalePrice());
  }

  @Test
  public void populateQuickEditV2Request_salePriceChangeTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setSalePrice(NEW_SALE_PRICE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
        .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    quickEditV2Request.setSellerSku(MERCHANT_SKU);
    quickEditV2Request.setOff2OnActiveFlag(Boolean.TRUE);
    quickEditV2Request.setWholeSaleActivated(Boolean.TRUE);
    quickEditV2Request.setCncActive(Boolean.TRUE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertSame(quickEditV2Request, populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
    Assertions.assertEquals(DISCOUNT_PRICE, populatedRequest.getPrice().getDiscountAmount());
  }

  @Test
  public void populateQuickEditV2Request_ListPriceChangeTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setPrice(NEW_NORMAL_PRICE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
      DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
        .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    quickEditV2Request.setSellerSku(MERCHANT_SKU);
    quickEditV2Request.setOff2OnActiveFlag(Boolean.TRUE);
    quickEditV2Request.setWholeSaleActivated(Boolean.TRUE);
    quickEditV2Request.setCncActive(Boolean.TRUE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
      RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertSame(quickEditV2Request, populatedRequest);
    Assertions.assertEquals(NEW_NORMAL_PRICE, populatedRequest.getPrice().getPrice());
    Assertions.assertEquals(DISCOUNT_PRICE, populatedRequest.getPrice().getDiscountAmount());
  }
  @Test
  public void getInProgressProductResponseSettingProductSkuTest() throws Exception {
    ProductCodeAndIdAndStateResponse productCodeAndIdAndStateResponse = new ProductCodeAndIdAndStateResponse();
    productCodeAndIdAndStateResponse.setProductBusinessPartnerId(PRODUCT_BUSINESS_PARTNER_ID);
    Map<String, String> productBusinessPartnerIdAndProductSkuMap = new HashMap<>();
    productBusinessPartnerIdAndProductSkuMap.put(PRODUCT_BUSINESS_PARTNER_ID, DEFAULT_PRODUCT_SKU);
    ResponseHelper.getInProgressProductResponseSettingProductSku(productCodeAndIdAndStateResponse, productBusinessPartnerIdAndProductSkuMap);
  }

  @Test
  public void getInProgressProductResponseTest() throws Exception {
    ProductCodeAndIdAndStateResponse productCodeAndIdAndStateResponse = new ProductCodeAndIdAndStateResponse();
    productCodeAndIdAndStateResponse.setProductBusinessPartnerId(PRODUCT_BUSINESS_PARTNER_ID);
    Map<String, List<String>> productBusinessPartnerIdAndItemSkuListMap = new HashMap<>();
    productBusinessPartnerIdAndItemSkuListMap.put(PRODUCT_BUSINESS_PARTNER_ID, Arrays.asList(ITEM_SKU));
    ResponseHelper.getInProgressProductResponse(productCodeAndIdAndStateResponse, productBusinessPartnerIdAndItemSkuListMap);
  }

  @Test
  public void isProductEligibleForVendorPublishTest() {
    Assertions.assertFalse(ResponseHelper.isProductEligibleForVendorPublish(3, false, false));
    Assertions.assertFalse(ResponseHelper.isProductEligibleForVendorPublish(2, false, false));
    Assertions.assertTrue(ResponseHelper.isProductEligibleForVendorPublish(2, false, true));
    Assertions.assertTrue(ResponseHelper.isProductEligibleForVendorPublish(2, true, false));
    Assertions.assertTrue(ResponseHelper.isProductEligibleForVendorPublish(1, false, false));
   }

  @Test
  public void toProductDetailResponseTest() {
    PriceResponse priceResponse = new PriceResponse();
    priceResponse.setSalePrice(100);
    priceResponse.setPrice(120);
    productLevel3Inventory.setWebAvailable(10);
    productL5DetailResponse.setPrices(Collections.singletonList(priceResponse));
    List<ProductAndItemPickupPontL5Response> result =
      ResponseHelper.toProductDetailResponse(Collections.singletonList(productL5DetailResponse),
        ImmutableMap.of(ITEM_SKU + "-" + PICKUP_POINT_CODE, productLevel3Inventory),
          ImmutableMap.of(ITEM_SKU + "-" + PICKUP_POINT_CODE, campaignPriceSkuResponse), false);
    Assertions.assertEquals(10, (int) result.get(0).getAvailableStockLevel2());
  }

  @Test
  public void toProductDetailResponseCampaignAndPricingNullTest() {
    productLevel3Inventory.setWebAvailable(10);
    productL5DetailResponse.setPrices(Collections.singletonList(new PriceResponse()));
    List<ProductAndItemPickupPontL5Response> result =
      ResponseHelper.toProductDetailResponse(Collections.singletonList(productL5DetailResponse),
        ImmutableMap.of(ITEM_SKU + PICKUP_POINT_CODE, productLevel3Inventory),
          ImmutableMap.of(ITEM_SKU + PICKUP_POINT_CODE, campaignPriceSkuResponse), false);
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
  }

  @Test
  public void populateQuickEditV2Request_salePriceGreaterThanListPriceTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3PriceRequest productLevel3PriceRequest = new ProductLevel3PriceRequest();
    productLevel3PriceRequest.setSalePrice(SALE_PRICE_2);
    productLevel3PriceRequest.setPrice(NORMAL_PRICE_2);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date()).startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    quickEditV2Request.setSellerSku(MERCHANT_SKU);
    quickEditV2Request.setOff2OnActiveFlag(Boolean.TRUE);
    quickEditV2Request.setWholeSaleActivated(Boolean.TRUE);
    quickEditV2Request.setCncActive(Boolean.TRUE);
    quickEditV2Request.setStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setPrice(productLevel3PriceRequest);
    itemSummaryListResponse.setMerchantSku(MERCHANT_SKU);
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, null, false);
    Assertions.assertNotNull(populatedRequest);
    Assertions.assertSame(quickEditV2Request, populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice());
    Assertions.assertEquals(DISCOUNT_PRICE, populatedRequest.getPrice().getDiscountAmount());
  }

  @Test
  public void updateProductItemIdInProductItemBusinessPartnerTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCreationType(ProductCreationType.MIGRATION.getProductCreationType());

    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setProductItemId(PRODUCT_ITEM_ID);
    ProductItemBusinessPartner productItemBusinessPartner3 = new ProductItemBusinessPartner();
    productItemBusinessPartner3.setGdnProductItemSku(ITEM_SKU_2);
    productItemBusinessPartner3.setProductItemId(PRODUCT_ITEM_ID);

    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(PRODUCT_ITEM_ID_2);

    boolean result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection,
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2),
        ImmutableMap.of(PRODUCT_ITEM_ID_2, productItemResponse1), true);
    Assertions.assertTrue(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());

    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID_2);
    productItemBusinessPartner2.setProductItemId(PRODUCT_ITEM_ID_2);
    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection,
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2),
        ImmutableMap.of(PRODUCT_ITEM_ID_2, productItemResponse1), true);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());

    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection,
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2, productItemBusinessPartner3),
        ImmutableMap.of(PRODUCT_ITEM_ID_2, productItemResponse1), true);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner3.getProductItemId());

    productCollection.setProductCreationType(ProductCreationType.FLOW1_WEB.getProductCreationType());
    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection,
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2, productItemBusinessPartner3),
        ImmutableMap.of(PRODUCT_ITEM_ID_2, productItemResponse1), true);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner3.getProductItemId());

    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection,
        Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2, productItemBusinessPartner3),
        new HashMap<>(), true);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner3.getProductItemId());

    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection, new ArrayList<>(),
        new HashMap<>(), true);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner3.getProductItemId());

    result = ResponseHelper.updateProductItemIdInProductItemBusinessPartner(productCollection, new ArrayList<>(),
        new HashMap<>(), false);
    Assertions.assertFalse(result);
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner1.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID_2, productItemBusinessPartner2.getProductItemId());
    Assertions.assertEquals(PRODUCT_ITEM_ID, productItemBusinessPartner3.getProductItemId());
  }

  @Test
  public void validateResponseNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ResponseHelper.validateResponse(null);
    });
  }

  @Test
  public void validateResponseNotNullTest() throws Exception {
    ApplicationRuntimeException applicationRuntimeException = new ApplicationRuntimeException();
    try {
      ListBaseResponse listBaseResponse =
          new ListBaseResponse("ERROR_MESSAGE", "ERROR_CODE", false, "REQUEST_ID", null, null);
      ResponseHelper.validateResponse(listBaseResponse);
    } catch (ApplicationRuntimeException e) {
      applicationRuntimeException = e;
    } finally {
      Assertions.assertEquals("COMMUNICATION_FAILURE", applicationRuntimeException.getErrorCodes().getCode());
    }
  }

  @Test
  public void validateResponseContentNullSuccessTrueTest() throws Exception {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse("ERROR_MESSAGE", "ERROR_CODE", true, "REQUEST_ID", null, null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ResponseHelper.validateResponse(listBaseResponse);
    });
  }

  @Test
  public void validateResponseSuceesFalseContentNotNullTest() throws Exception {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse("ERROR_MESSAGE", "ERROR_CODE", false, "REQUEST_ID", new ArrayList<>(), null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      ResponseHelper.validateResponse(listBaseResponse);
    });
  }

  @Test
  public void validateResponseSuccesFalseContentTest() throws Exception {
    ListBaseResponse listBaseResponse =
        new ListBaseResponse("ERROR_MESSAGE", "ERROR_CODE", true, "REQUEST_ID", new ArrayList<>(), null);
    ResponseHelper.validateResponse(listBaseResponse);
  }

  @Test
  public void toEditProductV2ResponseTest() {
    EditProductResponse editProductResponse =
        EditProductResponse.builder().productReview(true).reviewType(Constants.DEFAULT)
            .apiErrorCode(ApiErrorCode.PICKUP_POINT_IS_NOT_VALID)
            .variantsErrorList(Arrays.asList(VariantsErrorListResponse.builder().itemSku(ITEM_SKU).build())).build();
    EditProductV2Response editProductV2Response = ResponseHelper.toEditProductV2Response(editProductResponse);
    Assertions.assertTrue(editProductV2Response.isProductReview());
    Assertions.assertEquals(Constants.DEFAULT, editProductV2Response.getReviewType());
  }


  @Test
  public void testSetItemPickupPointUpdateRequestForAutoCategoryChange() {
    boolean takeActionOnShippingForAutoCategoryChange = true;
    ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
    itemPickupPointUpdateRequest.setProductType(ProductType.BOPIS);
    ItemPickupPointQuickEditRequest addL5Request = new ItemPickupPointQuickEditRequest();
    ItemPickupPointQuickEditRequest modifiedL5Request = new ItemPickupPointQuickEditRequest();
    addL5Request.setItemSku(ITEM_SKU);
    addL5Request.setStatus(ProductLevel3Status.ONLINE.name());
    itemPickupPointUpdateRequest.setAddPickupPointRequests(Collections.singletonList(addL5Request));
    modifiedL5Request.setItemSku(ITEM_SKU_2);
    modifiedL5Request.setStatus(ProductLevel3Status.B2B.name());
    modifiedL5Request.setScheduleUpdate(false);
    itemPickupPointUpdateRequest.setQuickEditUpdateRequests(Collections.singletonList(modifiedL5Request));
    ResponseHelper.setItemPickupPointUpdateRequestForAutoCategoryChange(
      takeActionOnShippingForAutoCategoryChange, itemPickupPointUpdateRequest);
    Assertions.assertEquals(ProductLevel3Status.OFFLINE.name(),
      itemPickupPointUpdateRequest.getAddPickupPointRequests().get(0).getStatus());
    Assertions.assertEquals(ProductType.REGULAR,
      itemPickupPointUpdateRequest.getProductType());
    Assertions.assertEquals(ProductLevel3Status.OFFLINE.name(),
      itemPickupPointUpdateRequest.getQuickEditUpdateRequests().get(0).getStatus());
  }

  @Test
  public void testNoActionWhenFlagIsFalse() {
    boolean takeActionOnShippingForAutoCategoryChange = false;
    ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
    itemPickupPointUpdateRequest.setProductType(ProductType.BOPIS);
    ItemPickupPointQuickEditRequest addL5Request = new ItemPickupPointQuickEditRequest();
    ItemPickupPointQuickEditRequest modifiedL5Request = new ItemPickupPointQuickEditRequest();
    addL5Request.setItemSku(ITEM_SKU);
    addL5Request.setStatus(ProductLevel3Status.ONLINE.name());
    itemPickupPointUpdateRequest.setAddPickupPointRequests(Collections.singletonList(addL5Request));
    modifiedL5Request.setItemSku(ITEM_SKU_2);
    modifiedL5Request.setStatus(ProductLevel3Status.B2B.name());
    modifiedL5Request.setScheduleUpdate(false);
    itemPickupPointUpdateRequest.setQuickEditUpdateRequests(Collections.singletonList(modifiedL5Request));
    ResponseHelper.setItemPickupPointUpdateRequestForAutoCategoryChange(takeActionOnShippingForAutoCategoryChange, itemPickupPointUpdateRequest);
    Assertions.assertEquals(ProductLevel3Status.ONLINE.name(),
      itemPickupPointUpdateRequest.getAddPickupPointRequests().get(0).getStatus());
    Assertions.assertEquals(ProductType.BOPIS,
      itemPickupPointUpdateRequest.getProductType());
  }

  @Test
  public void addAllowedAttributeValueWithValueTypeIsNotEmptyTest() {
    AllowedAttributeValueResponse allowedAttributeValueResponse1 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse1.setId(ID);
    allowedAttributeValueResponse1.setValue("UK-S");
    allowedAttributeValueResponse1.setValueType("UK");

    AllowedAttributeValueResponse allowedAttributeValueResponse2 = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse2.setValue("M");

    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setAllowedAttributeValues(
        Arrays.asList(allowedAttributeValueResponse1, allowedAttributeValueResponse2));

    Map<String, String> result =
        ResponseHelper.addAllowedAttributeValueWithValueTypeIsNotEmpty(attributeResponse);

    Assertions.assertEquals("UK-S", result.get(ID));
  }

  @Test
  public void getAttributeCodeAndValueAndValueTypeMapTest() {
    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    masterDataAttributeDTO1.setSizeAttribute(true);

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    masterDataAttributeDTO2.setSizeAttribute(false);

    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO1 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO1.setValue("S");
    masterDataAllowedAttributeValueDTO1.setValueType("UK");
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO1 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO1.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO1);

    MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO2 = new MasterDataAllowedAttributeValueDTO();
    masterDataAllowedAttributeValueDTO2.setValue("L");
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO2 = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeValueDTO2.setAllowedAttributeValue(masterDataAllowedAttributeValueDTO2);

    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);
    masterDataProductAttributeDTO1.setMasterDataProductAttributeValues(
        List.of(masterDataProductAttributeValueDTO1, masterDataProductAttributeValueDTO2));

    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);

    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(
        List.of(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2));

    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        ResponseHelper.getAttributeCodeAndValueAndValueTypeMap(masterDataProductDTO, SIZE_CHART_DELIMITER);

    Assertions.assertEquals("UK-S", attributeCodeAndValueAndValueTypeMap.get(DEFAULT_ATTRIBUTE_CODE).get("S"));
    Assertions.assertEquals("L", attributeCodeAndValueAndValueTypeMap.get(DEFAULT_ATTRIBUTE_CODE).get("L"));
  }

  @Test
  public void getValueAndValueType1Test() {
    Map<String, Map<String, String>> map = Map.of(DEFAULT_ATTRIBUTE_CODE, Map.of("S", "UK-S"));
    Assertions.assertEquals("UK-S", ResponseHelper.getValueAndValueType(map, DEFAULT_ATTRIBUTE_CODE, "S", true, true));
    Assertions.assertEquals("S", ResponseHelper.getValueAndValueType(map, DEFAULT_ATTRIBUTE_CODE, "S", true, false));
    Assertions.assertEquals("S", ResponseHelper.getValueAndValueType(map, DEFAULT_ATTRIBUTE_CODE, "S", false, true));
    Assertions.assertEquals("L", ResponseHelper.getValueAndValueType(map, DEFAULT_ATTRIBUTE_CODE, "L", true, true));
  }

  @Test
  public void getValueAndValueType2Test() {
    Assertions.assertEquals("UK-S", ResponseHelper.getValueAndValueType("UK", "S", "-", true, true));
    Assertions.assertEquals("S", ResponseHelper.getValueAndValueType("UK", "S", "-", true, false));
    Assertions.assertEquals("S", ResponseHelper.getValueAndValueType("UK", "S", "-", false, true));
  }

  @Test
  public void concatValueAndValueTypesInDefiningAttributesTest() {
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap = Map.of(DEFAULT_ATTRIBUTE_CODE, Map.of("S", "UK-S"));

    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeValue("S");

    ProductAttributeDTO productAttributeDTO = new ProductAttributeDTO();
    productAttributeDTO.setProductAttributeDetails(List.of(productAttributeDetailDTO));

    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setDefiningAttributes(List.of(productAttributeDTO));

    ProductLevel3DetailsV2Response product = new ProductLevel3DetailsV2Response();
    product.setProductL3Response(productL3Response);

    ResponseHelper.concatValueAndValueTypesInDefiningAttributes(true, true, product, attributeCodeAndValueAndValueTypeMap);

    Assertions.assertEquals("UK-S", productAttributeDetailDTO.getAttributeValue());
  }

  @Test
  void hasNextPage_shouldReturnFalse_whenPageMetaDataIsNull() {
    GdnRestListResponse<DummyResponse> response = new GdnRestListResponse<>();
    response.setPageMetaData(null);

    boolean result = ResponseHelper.hasNextPage(response);
    Assertions.assertFalse(result);
  }

  @Test
  void hasNextPage_shouldReturnFalse_whenAllRecordsFetched() {
    GdnRestListResponse<DummyResponse> response = new GdnRestListResponse<>();
    PageMetaData metaData = new PageMetaData();
    metaData.setPageNumber(2);
    metaData.setPageSize(50);
    metaData.setTotalRecords(100L); // 2*50 == 100 → No more records

    response.setPageMetaData(metaData);

    boolean result = ResponseHelper.hasNextPage(response);
    Assertions.assertFalse(result);
  }

  @Test
  void hasNextPage_shouldReturnTrue_whenMoreRecordsToFetch() {
    GdnRestListResponse<DummyResponse> response = new GdnRestListResponse<>();
    PageMetaData metaData = new PageMetaData();
    metaData.setPageNumber(1);
    metaData.setPageSize(50);
    metaData.setTotalRecords(200L); // 1*50 = 50 < 200 → More to fetch

    response.setPageMetaData(metaData);

    boolean result = ResponseHelper.hasNextPage(response);
    Assertions.assertTrue(result);
  }

  @Test
  public void filterHideFromSellerAttributesNullAttributes() {
    ProductL3DetailsResponse productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setAttributes(null);
    ResponseHelper.filterHideFromSellerAttributes(productL3DetailsResponse, new ProductL3Response(), true);
    Assertions.assertNotNull(productL3DetailsResponse);
  }

  @Test
  public void filterHideFromSellerAttributesSwitchOff() {
    ProductL3DetailsResponse productL3DetailsResponse = new ProductL3DetailsResponse();
    productL3DetailsResponse.setAttributes(null);
    ResponseHelper.filterHideFromSellerAttributes(productL3DetailsResponse, new ProductL3Response(), false);
    Assertions.assertNotNull(productL3DetailsResponse);
  }

  @Test
  public void testFilterHideFromSellerAttributes_removesHiddenAttributes() {
    ProductL3DetailsResponse product = new ProductL3DetailsResponse();
    ProductL3Response productL3Response = new ProductL3Response();
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataAttributeDTO visibleAttr = new MasterDataAttributeDTO();
    visibleAttr.setAttributeCode("ATTR_VISIBLE");
    visibleAttr.setHideFromSeller(false);
    MasterDataAttributeDTO hiddenAttr = new MasterDataAttributeDTO();
    hiddenAttr.setAttributeCode("ATTR_HIDDEN");
    hiddenAttr.setHideFromSeller(true);

    MasterDataProductAttributeDTO attrVisible = new MasterDataProductAttributeDTO();
    attrVisible.setMasterDataAttribute(visibleAttr);

    MasterDataProductAttributeDTO attrHidden = new MasterDataProductAttributeDTO();
    attrHidden.setMasterDataAttribute(hiddenAttr);

    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>(Arrays.asList(attrVisible, attrHidden, null)));
    productL3Response.setMasterDataProduct(masterDataProduct);
    // Product-level attributes
    ProductLevel3AttributeResponse attr1 = new ProductLevel3AttributeResponse();
    attr1.setAttributeCode("ATTR_VISIBLE");
    ProductLevel3AttributeResponse attr2 = new ProductLevel3AttributeResponse();
    attr2.setAttributeCode("ATTR_HIDDEN");
    product.setAttributes(new ArrayList<>(Arrays.asList(attr1, attr2,null)));

    // Descriptive attributes
    ProductAttributeDetailDTO descAttr1 = new ProductAttributeDetailDTO();
    descAttr1.setAttributeCode("ATTR_VISIBLE");

    ProductAttributeDetailDTO descAttr2 = new ProductAttributeDetailDTO();
    descAttr2.setAttributeCode("ATTR_HIDDEN");

    productL3Response.setDescriptiveAttributes(new ArrayList<>(Arrays.asList(descAttr1, descAttr2, null)));
    product.setProductL3Response(productL3Response);

    ResponseHelper.filterHideFromSellerAttributes(product, productL3Response, true);
    Assertions.assertEquals(2, product.getAttributes().size());
    Assertions.assertEquals("ATTR_VISIBLE", product.getAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(2, productL3Response.getDescriptiveAttributes().size());
    Assertions.assertEquals("ATTR_VISIBLE", productL3Response.getDescriptiveAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(2, productL3Response.getMasterDataProduct().getMasterDataProductAttributes().size());
    Assertions.assertEquals("ATTR_VISIBLE", productL3Response.getMasterDataProduct()
        .getMasterDataProductAttributes().get(0).getMasterDataAttribute().getAttributeCode());
  }

  @Test
  void testGetAiGeneratedFields_whenInputIsNotNull() {
    AiGeneratedFieldsResponse source = new AiGeneratedFieldsResponse();
    source.setAiGeneratedBrand(AI_GENERATED_BRAND);
    source.setAiGeneratedCategory(AI_GENERATED_CATEGORY);

    com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse result =
        ResponseHelper.getAiGeneratedFields(source);

    Assertions.assertTrue(result.isAiGeneratedBrand());
    Assertions.assertFalse(result.isAiGeneratedCategory());
  }

  @Test
  void testGetAiGeneratedFields_whenInputIsNull() {
    com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse result =
        ResponseHelper.getAiGeneratedFields(null);
    Assertions.assertNotNull(result);
  }

}
