package com.gdn.partners.pbp.helper;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.InventoryUpsertModel;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.PickupPointRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductMasterDataUpdateRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.SyncStockUpdateOrInsertVo;
import com.gda.mta.product.dto.UpdateOrInsertStockVo;
import com.gda.mta.product.dto.response.ItemPickupPointSummaryResponse;
import com.gda.mta.product.dto.response.ProductL3ImageResponse;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.response.ProductL3PriceResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.modal.XProdAttributeMigrationEventModel;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.VideoAddEditRequest;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryResponseV2DTO;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.feign.ProductBasicMasterFieldsRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.GeolocationDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.product.domain.event.enums.ProductType;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreResponse;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;


public class RequestHelperTest {

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String PICKUP_POINT_CODE_2 = "pickupPointCode2";
  private static final String ITEM_SKU_PICKUP_POINT_CODE = "itemSku-pickupPointCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String REASON = "reason";
  private static final int AVALIABLE_STOCK = 10;
  private static final int ORGINAL_STOCK = 10;
  private static final String URL = "url";
  private static final String DEFAULT = "default";
  private static final double LIST_PRICE = 12;
  private static final double OFFER_PRICE = 10;
  private static final String ITEM_SKU_3 = "DR6-00001-00001-00001";
  private static final String ITEM_SKU_4 = "DR6-00001-00001-00002";
  private static final String PRODUCT_SKU_2 = "DR6-00001-00001";
  private static final String ID = "id";
  private static final String ALLOWED_ATTRIBUTE_VALUE_ID = "ALLOWED_ATTRIBUTE_VALUE_ID";
  private static final String SIZE_CHART_DELIMITER = "-";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String ATTRIBUTE_CODE_2 = "ATTRIBUTE_CODE_2";
  private static final double NORMAL_PRICE = 1000d;
  private static final double SALE_PRICE = 900d;
  private static final double DISCOUNT_PRICE = 900d;
  private static final String PRODUCT_CODE = "productCode";


  private static final List<String> PICK_UP_POINT = List.of("PP-123456");
  private static final Date DATE = new Date();
  private static final String FAAS_ACTIVATED = "faasActivated";
  public static final String VIDEO_URL = "test-video-url";
  public static final String PRODUCT_NAME = "Test Product";
  public static final String SIZE_CHART_CODE = "SIZE-CHART-1";


  private ItemPickupPointListingL3Request itemPickupPointListingL3Request;
  private ItemPickupPointListingResponse itemPickupPointListingResponse;
  private ItemResponseV2 itemResponseV2 = new ItemResponseV2();
  private ProductL3ListingRequest productL3ListingRequest;
  private WebInventoryResponseV2DTO webInventory;
  private InventoryDetailInfoResponseV2DTO inventoryL5;
  private ItemPickupPointSummaryResponse itemPickupPointSummaryWebResponse;
  private WarehouseInventoryResponseDTO warehouseInventoryResponseDTO;
  private PriceDTO priceDTO;
  private DiscountPriceDTO discountPriceDTO;
  private MasterDataItemImageDTO masterDataItemImageDTO;
  private CampaignPriceSkuResponse campaignPriceSkuResponse;
  private ItemViewConfigDTO itemViewConfigDTO;
  private ProductL3SummaryResponse productL3SummaryResponse;
  private Map<String, String> skuXCategoryName;
  private Map<String, String> skuXSuspensionReasonMap;
  private Map<String, InventoryStockInfoDTO> productSkuInventoryMap;
  private Map<String, InventoryDetailInfoResponseV2DTO> itemInventoryMapL5;
  private Map<String, CampaignPriceSkuResponse> itemCampaignMap;
  private ItemL4SummaryResponse itemL4SummaryResponse;
  private InventoryStockInfoDTO inventoryStockInfoDTO;

  private ProductL5DetailResponse productL5DetailResponse = new ProductL5DetailResponse();
  private PriceResponse priceResponse;
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest;
  private ItemSummaryListResponse itemSummaryListResponse;
  private ItemPickupPointRequest itemPickupPointRequest;
  private QuickEditV2Request quickEditV2Request;
  private ProfileResponse profileResponseData;
  private ProductCollection productCollection;

  @BeforeEach
  public void setUp() {
    itemPickupPointListingL3Request =
        ItemPickupPointListingL3Request.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).productSku(PRODUCT_SKU)
            .itemSku(ITEM_SKU).pickupPointCodes(ImmutableSet.of(PICKUP_POINT_CODE)).build();

    itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemPickupPointListingResponse.setCategoryCode(CATEGORY_CODE);
    itemResponseV2.setItemSku(ITEM_SKU);
    itemResponseV2.setPickUpPointCode(PICKUP_POINT_CODE);
    itemResponseV2.setMerchantCode(BUSINESS_PARTNER_CODE);

    productL3ListingRequest = new ProductL3ListingRequest();
    inventoryL5 = new InventoryDetailInfoResponseV2DTO();
    itemPickupPointSummaryWebResponse = new ItemPickupPointSummaryResponse();
    webInventory = new WebInventoryResponseV2DTO();
    webInventory.setAvailableStock(AVALIABLE_STOCK);
    webInventory.setOriginalStock(ORGINAL_STOCK);
    warehouseInventoryResponseDTO = new WarehouseInventoryResponseDTO();
    warehouseInventoryResponseDTO.setAvailableStock(AVALIABLE_STOCK);
    warehouseInventoryResponseDTO.setOriginalStock(ORGINAL_STOCK);

    priceDTO = new PriceDTO();
    priceDTO.setChannel(DEFAULT);
    priceDTO.setListPrice(LIST_PRICE);
    priceDTO.setOfferPrice(OFFER_PRICE);

    discountPriceDTO = new DiscountPriceDTO();
    discountPriceDTO.setStartDateTime(DATE);

    masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath(URL);
    campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    itemViewConfigDTO = new ItemViewConfigDTO();

    productL3SummaryResponse = new ProductL3SummaryResponse();
    itemL4SummaryResponse = new ItemL4SummaryResponse();
    itemL4SummaryResponse.setItemSku(ITEM_SKU);
    itemL4SummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemL4SummaryResponse.setPrice(new HashSet<>(Arrays.asList(priceDTO)));
    itemL4SummaryResponse.setItemViewConfigs(Arrays.asList(itemViewConfigDTO));

    skuXCategoryName = new HashMap<>();
    skuXSuspensionReasonMap = new HashMap<>();
    productSkuInventoryMap = new HashMap<>();
    itemInventoryMapL5 = new HashMap<>();
    itemCampaignMap = new HashMap<>();
    inventoryStockInfoDTO = new InventoryStockInfoDTO();

    priceResponse = new PriceResponse();
    priceResponse.setSalePrice(LIST_PRICE);

    productL5DetailResponse.setItemSku(ITEM_SKU);
    productL5DetailResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productL5DetailResponse.setMerchantCode(BUSINESS_PARTNER_CODE);
    productL5DetailResponse.setCategoryCode(CATEGORY_CODE);
    productL5DetailResponse.setPrices(Collections.singletonList(priceResponse));

    productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productVariantPriceStockAndImagesRequest.setProductSku(PRODUCT_SKU);
    productVariantPriceStockAndImagesRequest.setSkuCode(ITEM_CODE);

    itemSummaryListResponse = new ItemSummaryListResponse();
    itemSummaryListResponse.setProductSku(PRODUCT_SKU);
    itemSummaryListResponse.setItemCode(ITEM_CODE);

    itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointId(PICKUP_POINT_CODE);
    itemPickupPointRequest.setStock(1);
    itemPickupPointRequest.setMinimumStock(1);
    itemPickupPointRequest.setSynchronizeStock(true);
    itemPickupPointRequest.setFbbActive(true);

    quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    quickEditV2Request.setDeltaStock(1);
    quickEditV2Request.setUseWarehouseStock(true);
    quickEditV2Request.setFbbActivated(true);
    productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    profileResponseData = new ProfileResponse();
  }

  @Test
  public void toItemPickupPointListingRequestTest() {
    ItemPickupPointListingRequest itemPickupPointListingRequest =
        RequestHelper.toItemPickupPointListingRequest(itemPickupPointListingL3Request);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, itemPickupPointListingRequest.getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPointListingRequest.getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListingRequest.getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListingRequest.getPickupPointCodes().iterator().next());
  }

  @Test
  public void inventoryDetailInfoRequestDTOTest() {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
        RequestHelper.inventoryDetailInfoRequestDTO(itemPickupPointListingResponse);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, inventoryDetailInfoRequestDTO.getWebMerchantCode());
    Assertions.assertEquals(ITEM_SKU, inventoryDetailInfoRequestDTO.getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, inventoryDetailInfoRequestDTO.getPickupPointCode());
  }

  @Test
  public void toCampaignPriceRequestV2Test() {
    CampaignPriceRequest campaignPriceRequest =
        RequestHelper.toCampaignPriceRequestV2(Arrays.asList(itemPickupPointListingResponse));
    Assertions.assertEquals(CATEGORY_CODE, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getCategoryCode());
    Assertions.assertEquals(ITEM_SKU, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getPickUpPointCode());
  }

  @Test
  public void toCampaignPriceRequestTest() {
    CampaignPriceRequest campaignPriceRequest =
        RequestHelper.toCampaignPriceRequest(Arrays.asList(itemPickupPointListingResponse));
    Assertions.assertEquals(CATEGORY_CODE, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getCategoryCode());
    Assertions.assertEquals(ITEM_SKU, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getItemSku());
  }

  @Test
  public void toItemPickupPointSummaryRequestTest() {
    ItemPickupPointSummaryRequest response =
        RequestHelper.toItemPickupPointSummaryRequest(BUSINESS_PARTNER_CODE, Collections.singletonList(PRODUCT_SKU),
            true,  null);
    Assertions.assertTrue(response.getOnlineOrCnc());
    Assertions.assertTrue(response.getProductSkuList().contains(PRODUCT_SKU));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getMerchantCode());
  }

  @Test
  public void toItemPickupPointSummaryRequest_onllineOrCncFalseTest() {
    ItemPickupPointSummaryRequest response =
        RequestHelper.toItemPickupPointSummaryRequest(BUSINESS_PARTNER_CODE, Collections.singletonList(PRODUCT_SKU),
            false, null);
    Assertions.assertNull(response.getOnlineOrCnc());
    Assertions.assertTrue(response.getProductSkuList().contains(PRODUCT_SKU));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getMerchantCode());
  }

  @Test
  public void toItemPickupPointSummaryRequest_accessiblePickUpPoint_NotNULL() {
    ItemPickupPointSummaryRequest response =
        RequestHelper.toItemPickupPointSummaryRequest(BUSINESS_PARTNER_CODE, Collections.singletonList(PRODUCT_SKU),
            false,PICK_UP_POINT);
    Assertions.assertNull(response.getOnlineOrCnc());
    Assertions.assertTrue(response.getProductSkuList().contains(PRODUCT_SKU ));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getMerchantCode());
    Assertions.assertEquals(PICK_UP_POINT, response.getPickupPointCodes());
  }
  @Test
  public void toInventoryDetailInfoRequestDTOTest() {
    InventoryDetailInfoRequestDTO response = RequestHelper.toInventoryDetailInfoRequestDTO(itemResponseV2);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getWebMerchantCode());
    Assertions.assertEquals(ITEM_SKU, response.getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.getPickupPointCode());
  }

  @Test
  public void toItemPriceTest() {
    priceDTO.setListOfDiscountPrices(Arrays.asList(discountPriceDTO));
    List<ProductL3PriceResponse> productL3PriceResponses = RequestHelper.toItemPrice(Arrays.asList(priceDTO));
    Assertions.assertEquals(productL3PriceResponses.get(0).getChannelId(), DEFAULT);
    Assertions.assertEquals(productL3PriceResponses.get(0).getPrice(), (Double) LIST_PRICE);
    Assertions.assertEquals(productL3PriceResponses.get(0).getSalePrice(), (Double) OFFER_PRICE);
    Assertions.assertEquals(productL3PriceResponses.get(0).getDiscountStartDate(), discountPriceDTO.getStartDateTime());
  }

  @Test
  public void toItemPriceWithoutDiscountPricesTest() {
    List<ProductL3PriceResponse> productL3PriceResponses = RequestHelper.toItemPrice(Arrays.asList(priceDTO));
    Assertions.assertEquals(DEFAULT, productL3PriceResponses.get(0).getChannelId());
    Assertions.assertEquals((Double) LIST_PRICE, productL3PriceResponses.get(0).getPrice());
    Assertions.assertEquals( (Double) OFFER_PRICE, productL3PriceResponses.get(0).getSalePrice());
  }

  @Test
  public void toItemImageListWithNullTest() {
    List<ProductL3ImageResponse> productL3ImageResponses = RequestHelper.toItemImageList(new ArrayList<>());
    Assertions.assertEquals(productL3ImageResponses, new ArrayList<>());
  }

  @Test
  public void toItemImageListTest() {
    List<ProductL3ImageResponse> productL3ImageResponses = RequestHelper.toItemImageList(Arrays.asList(masterDataItemImageDTO));
    Assertions.assertEquals(productL3ImageResponses.get(0).getLocationPath(), URL);
  }

  @Test
  public void toL5CampaignResponseTest() {
    campaignPriceSkuResponse.setCampaignPrice(LIST_PRICE);
    RequestHelper.toL5CampaignResponse(campaignPriceSkuResponse, itemPickupPointSummaryWebResponse, true);
  }

  @Test
  public void toL5CampaignResponseLockPriceUpdateTest() {
    campaignPriceSkuResponse.setLockPriceUpdate(true);
    RequestHelper.toL5CampaignResponse(campaignPriceSkuResponse, itemPickupPointSummaryWebResponse, true);
  }

  @Test
  public void toL5CampaignResponseLiveTest() {
    campaignPriceSkuResponse.setLive(true);
    RequestHelper.toL5CampaignResponse(campaignPriceSkuResponse, itemPickupPointSummaryWebResponse, true);
  }

  @Test
  public void toL5CampaignResponseLiveAndLockPriceUpdateTest() {
    campaignPriceSkuResponse.setLockPriceUpdate(true);
    campaignPriceSkuResponse.setLive(true);
    RequestHelper.toL5CampaignResponse(campaignPriceSkuResponse, itemPickupPointSummaryWebResponse, true);
  }

  @Test
  public void toItemViewConfigListTest() {
    RequestHelper.toItemViewConfigList(Arrays.asList(itemViewConfigDTO));
  }

  @Test
  public void toProductSummaryRequestTest() {
    productL3ListingRequest.setMerchantCode(MERCHANT_CODE);
    ProductSummaryRequest productSummaryRequest = RequestHelper.toProductSummaryRequest(productL3ListingRequest);
    Assertions.assertEquals(MERCHANT_CODE, productSummaryRequest.getMerchantCode());
  }

  @Test
  public void toL5InventoryResponseTest() {
    inventoryL5.setWebInventoryResponse(webInventory);
    RequestHelper.toL5InventoryResponse(inventoryL5, itemPickupPointSummaryWebResponse);
  }

  @Test
  public void toL5InventoryResponseWareHouseInventoryTest() {
    inventoryL5.setWebInventoryResponse(webInventory);
    inventoryL5.setWarehouseInventoryResponseList(Arrays.asList(warehouseInventoryResponseDTO));
    RequestHelper.toL5InventoryResponse(inventoryL5, itemPickupPointSummaryWebResponse);
  }

  @Test
  public void toL5IdTest() {
    String l5Id = RequestHelper.toL5Id(ITEM_SKU, PICKUP_POINT_CODE);
    Assertions.assertEquals(l5Id, ITEM_SKU_PICKUP_POINT_CODE);
  }

  @Test
  public void toProductDetailPageTest() {
    String productDetailPage = RequestHelper.toProductDetailPage(PRODUCT_SKU, URL);
  }

  @Test
  public void toProductL3ListingResponseTest() {
    RequestHelper.toProductL3ListingResponse(productL3SummaryResponse, skuXCategoryName, skuXSuspensionReasonMap,
        productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap, URL);
  }

  @Test
  public void toProductL3ListingResponseSuspendedProductsTest() {
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    skuXSuspensionReasonMap.put(PRODUCT_SKU, REASON);
    RequestHelper.toProductL3ListingResponse(productL3SummaryResponse, skuXCategoryName, skuXSuspensionReasonMap,
        productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap, URL);
  }

  @Test
  public void toProductL3ListingResponseWithDataTest() {
    inventoryL5.setWebInventoryResponse(webInventory);
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    productL3SummaryResponse.setProductScore(new ProductScoreResponse());
    productL3SummaryResponse.setVariantCount(1);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    itemInventoryMapL5.put(ITEM_SKU_PICKUP_POINT_CODE, inventoryL5);
    itemCampaignMap.put(ITEM_SKU_PICKUP_POINT_CODE, new CampaignPriceSkuResponse());
    ProductL3ListingResponse productL3ListingResponse =
      RequestHelper.toProductL3ListingResponse(productL3SummaryResponse,
      skuXCategoryName, skuXSuspensionReasonMap,
        productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap, URL);
    Assertions.assertEquals(productL3ListingResponse.getTotalActiveStock(), AVALIABLE_STOCK);
  }

  @Test
  public void toProductL3ListingResponseWithDataWithOutDataTest() {
    inventoryL5.setWebInventoryResponse(webInventory);
    productSkuInventoryMap.put(PRODUCT_SKU, inventoryStockInfoDTO);
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    itemL4SummaryResponse.setActivePromoBundlings(new HashSet<>(Arrays.asList(DEFAULT)));
    productL3SummaryResponse.setProductScore(new ProductScoreResponse());
    productL3SummaryResponse.setVariantCount(1);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    ProductL3ListingResponse response =
      RequestHelper.toProductL3ListingResponse(productL3SummaryResponse, skuXCategoryName,
        skuXSuspensionReasonMap, productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap, URL);
    Assertions.assertEquals(1, response.getVariantCount());
  }

  @Test
  public void toWholesalePriceSkuDetailListRequestTest() {
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequests =
        RequestHelper.toWholesalePriceSkuDetailListRequest(Arrays.asList(itemPickupPointListingResponse));
    Assertions.assertEquals(ITEM_SKU, wholesalePriceSkuDetailListRequests.getItemSkus().iterator().next());
    Assertions.assertEquals(ITEM_SKU, wholesalePriceSkuDetailListRequests.getItemInfo().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
      wholesalePriceSkuDetailListRequests.getItemInfo().get(0).getPickupPointCode());
  }

  @Test
  public void inventoryDetailInfoRequestDTONRTest() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setPickupPointId(PICKUP_POINT_CODE);

    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
        RequestHelper.inventoryDetailInfoRequestDTO(productItemBusinessPartner, BUSINESS_PARTNER_CODE);

    Assertions.assertEquals(ITEM_SKU, inventoryDetailInfoRequestDTO.getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, inventoryDetailInfoRequestDTO.getPickupPointCode());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, inventoryDetailInfoRequestDTO.getWebMerchantCode());
  }

  @Test
  public void toInventoryDetailInfoRequestListTest() {
    List<InventoryDetailInfoRequestDTO> result = RequestHelper.toInventoryDetailInfoRequestList(
      Collections.singletonList(productL5DetailResponse));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.get(0).getWebMerchantCode());
    Assertions.assertEquals(ITEM_SKU, result.get(0).getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, result.get(0).getPickupPointCode());
  }

  @Test
  public void toCampaignPriceRequestFromProductDetail() {
    CampaignPriceRequest result = RequestHelper.toCampaignPriceRequestFromProductDetail(
      Collections.singletonList(productL5DetailResponse));
    Assertions.assertEquals(CATEGORY_CODE,
      result.getCampaignPriceSkuRequestList().get(0).getCategoryCode());
  }

  @Test
  public void toItemPickupPointRequestTest() {
    ItemSkuPpCodeRequest itemSkuPpCodeRequest = new ItemSkuPpCodeRequest();
    itemSkuPpCodeRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemSkuPpCodeRequest.setItemSku(ITEM_SKU);
    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> result =
      RequestHelper.toItemPickupPointRequest(Collections.singletonList(itemSkuPpCodeRequest));
    Assertions.assertEquals(ITEM_SKU, result.get(0).getItemSku());
  }

  @Test
  public void toProductL3ListingResponseWithDataSyncStockTrueTest() {
    webInventory.setSyncStock(true);
    inventoryL5.setWebInventoryResponse(webInventory);
    inventoryL5.setWarehouseInventoryResponseList(Arrays.asList(warehouseInventoryResponseDTO));
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    productL3SummaryResponse.setProductScore(new ProductScoreResponse());
    productL3SummaryResponse.setVariantCount(1);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    itemInventoryMapL5.put(ITEM_SKU_PICKUP_POINT_CODE, inventoryL5);
    itemCampaignMap.put(ITEM_SKU_PICKUP_POINT_CODE, new CampaignPriceSkuResponse());
    ProductL3ListingResponse productL3ListingResponse =
      RequestHelper.toProductL3ListingResponse(productL3SummaryResponse, skuXCategoryName,
        skuXSuspensionReasonMap, productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap, URL);
    Assertions.assertEquals(productL3ListingResponse.getTotalActiveStock(), AVALIABLE_STOCK);
  }

  @Test
  public void validateAddPickupPointRequestTest() {
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
    itemPickupPointRequest1.setItemSku(ITEM_SKU);
    itemPickupPointRequest1.setPickupPointId(PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 = new ItemPickupPointRequest();
    itemPickupPointRequest2.setItemSku(ITEM_SKU_2);
    itemPickupPointRequest2.setPickupPointId(PICKUP_POINT_CODE_2);
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Map<String, ItemPickupPointListingResponse> map = new HashMap<>();
    map.put(ITEM_SKU_2 + Constants.HYPHEN + PICKUP_POINT_CODE_2, new ItemPickupPointListingResponse());

    RequestHelper.validateAddPickupPointRequest(productVariantUpdateRequest, map, false);

    productVariantUpdateRequest.setAddPickupPoints(new ArrayList<>());
    RequestHelper.validateAddPickupPointRequest(productVariantUpdateRequest, map, true);

    productVariantUpdateRequest.setAddPickupPoints(Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest2));
    RequestHelper.validateAddPickupPointRequest(productVariantUpdateRequest, map, true);
    Assertions.assertEquals(productVariantUpdateRequest.getAddPickupPoints().size(), 1);
    Assertions.assertEquals(productVariantUpdateRequest.getAddPickupPoints().get(0).getItemSku(), ITEM_SKU);
    Assertions.assertEquals(productVariantUpdateRequest.getAddPickupPoints().get(0).getPickupPointId(), PICKUP_POINT_CODE);
  }

  @Test
  public void isEligibleForXProductUpdateTest() {
    Assertions.assertFalse(
        RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), false, false,
            false, null, new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(
        RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(), new ArrayList<>(), false, false,
            false, null, new EditFlagChangesDTO(), Collections.singletonList(new ProductBundleRecipeRequest())));
    Assertions.assertTrue(
        RequestHelper.isEligibleForXProductUpdate(Collections.singletonList(new ItemPickupPointQuickEditRequest()),
            new ArrayList<>(), new ArrayList<>(), false, false, false, null, new EditFlagChangesDTO(),
            new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(),
        Collections.singletonList(new ItemPickupPointQuickEditRequest()), new ArrayList<>(), false, false, false, null,
        new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        Collections.singletonList(new ItemPickupPointDeleteRequest()), false, false, false, null,
        new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), true, false, false, null,
        new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), false, true, false, null,
        new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), false, false, true, null,
        new EditFlagChangesDTO(), new ArrayList<>()));
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), false, false, false, new AddDeleteVariantRequest(),
        new EditFlagChangesDTO(), new ArrayList<>()));
    EditFlagChangesDTO editFlagChangesDTO = new EditFlagChangesDTO();
    editFlagChangesDTO.setB2bFlagChangedAtL3Level(true);
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), false, false, false, null,
        editFlagChangesDTO, new ArrayList<>()));
    editFlagChangesDTO.setB2bFlagChangedAtL3Level(false);
    editFlagChangesDTO.setB2cFlagChangedAtL3Level(true);
    Assertions.assertTrue(RequestHelper.isEligibleForXProductUpdate(new ArrayList<>(), new ArrayList<>(),
        new ArrayList<>(), false, false, false, null,
        editFlagChangesDTO, new ArrayList<>()));
  }

  @Test
  public void isItemSkuTest() {
    Assertions.assertTrue(RequestHelper.isItemSku(ITEM_SKU_3));
    Assertions.assertFalse(RequestHelper.isItemSku(ITEM_SKU_2));
    Assertions.assertFalse(RequestHelper.isItemSku(StringUtils.EMPTY));
  }

  @Test
  public void removeProductsFromBundleRecipeWhichDontHaveItemSkuTest() {
    BundleRecipeRequest bundleRecipeRequest1 = new BundleRecipeRequest();
    BundleRecipeRequest bundleRecipeRequest2 = new BundleRecipeRequest(ITEM_SKU_3, 1);
    Set<BundleRecipeRequest> bundleRecipe = new HashSet<>();
    bundleRecipe.add(null);
    bundleRecipe.add(bundleRecipeRequest1);
    bundleRecipe.add(bundleRecipeRequest2);
    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    ProductItemCreationRequest productItemCreationRequest2 = new ProductItemCreationRequest();
    productItemCreationRequest2.setBundleRecipe(bundleRecipe);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(
        Arrays.asList(productItemCreationRequest1, productItemCreationRequest2));
    RequestHelper.removeProductsFromBundleRecipeWhichDontHaveItemSku(productCreationRequest);
    Assertions.assertTrue(
        CollectionUtils.isEmpty(productCreationRequest.getProductItemRequests().get(0).getBundleRecipe()));
    Assertions.assertEquals(1, productCreationRequest.getProductItemRequests().get(1).getBundleRecipe().size());
  }

  @Test
  public void getProductSkuForBundleProductChildSkusTest() {
    BundleRecipeRequest bundleRecipeRequest1 = new BundleRecipeRequest(ITEM_SKU_3, 1);
    BundleRecipeRequest bundleRecipeRequest2 = new BundleRecipeRequest(ITEM_SKU_4, 1);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(bundleRecipeRequest1, bundleRecipeRequest2));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Assertions.assertEquals(PRODUCT_SKU_2, RequestHelper.getProductSkuForBundleProductChildSkus(productCreationRequest).get(0));
  }

  @Test
  public void getItemSkuForBundleProductChildSkus() {
    BundleRecipeRequest bundleRecipeRequest1 = new BundleRecipeRequest(ITEM_SKU_3, 1);
    BundleRecipeRequest bundleRecipeRequest2 = new BundleRecipeRequest(ITEM_SKU_4, 1);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setBundleRecipe(ImmutableSet.of(bundleRecipeRequest1, bundleRecipeRequest2));
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest));
    Assertions.assertEquals(ITEM_SKU_3, RequestHelper.getItemSkuForBundleProductChildSkus(productCreationRequest).get(0));
  }

  @Test
  public void setBundleProductRequestsTest() {
    ItemPickupPointUpdateRequest pickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
    RequestHelper.setBundleProductRequests(new ProductVariantUpdateRequest(), new ItemPickupPointUpdateRequest());
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    ProductBundleRecipeRequest productBundleRecipeRequest = new ProductBundleRecipeRequest();
    productBundleRecipeRequest.setItemSku(ITEM_SKU);
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU);
    productBundleRecipeRequest.setBundleRecipe(Collections.singleton(productBundleRecipe));
    productVariantUpdateRequest.setProductBundleRecipe(Collections.singletonList(productBundleRecipeRequest));
    RequestHelper.setBundleProductRequests(productVariantUpdateRequest, pickupPointUpdateRequest);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(pickupPointUpdateRequest.getBundleRecipesRequests()));
  }

  @Test
  public void isL5UpdatedTest() {
    boolean result1 = RequestHelper.isL5Updated(null, null);
    Assertions.assertFalse(result1);
    boolean result2 = RequestHelper.isL5Updated(new EditItemResponse(), null);
    Assertions.assertTrue(result2);
    boolean result3 = RequestHelper.isL5Updated(null, new CombinedEditItemResponse());
    Assertions.assertTrue(result3);
  }

  @Test
  public void getBillOfMaterialSetupTest() {
    List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> requestList;
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    itemSkuAndItemCodeMap.put(ITEM_SKU, ITEM_CODE);
    Set<ProductBundleRecipe> bundleRecipeRequests = new HashSet<>();
    ProductBundleRecipe productBundleRecipe = new ProductBundleRecipe();
    productBundleRecipe.setItemSku(ITEM_SKU);
    productBundleRecipe.setQuantity(5);
    bundleRecipeRequests.add(productBundleRecipe);
    requestList = RequestHelper.getBillOfMaterialSetup(bundleRecipeRequests, itemSkuAndItemCodeMap);
    Assertions.assertEquals(requestList.size(), 1);
  }

  @Test
  public void getBillOfMaterialSetupOnActivationTest() {
    List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> requestList;
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    itemSkuAndItemCodeMap.put(ITEM_SKU, ITEM_CODE);
    Set<BundleRecipeVo> bundleRecipeRequests = new HashSet<>();
    BundleRecipeVo productBundleRecipe = new BundleRecipeVo();
    productBundleRecipe.setItemSku(ITEM_SKU);
    productBundleRecipe.setQuantity(5);
    bundleRecipeRequests.add(productBundleRecipe);
    requestList = RequestHelper.getBillOfMaterialSetupOnActivation(bundleRecipeRequests, itemSkuAndItemCodeMap);
    Assertions.assertEquals(requestList.size(), 1);
  }

  @Test
  public void getBillOfMaterialSetupOnActivationNullCheckForBundleRecipeRequestsTest() {
    List<CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest> requestList;
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    itemSkuAndItemCodeMap.put(ITEM_SKU, ITEM_CODE);
    Set<BundleRecipeVo> bundleRecipeRequests = new HashSet<>();
    BundleRecipeVo productBundleRecipe = new BundleRecipeVo();
    productBundleRecipe.setItemSku(ITEM_SKU);
    productBundleRecipe.setQuantity(5);
    bundleRecipeRequests.add(productBundleRecipe);
    requestList = RequestHelper.getBillOfMaterialSetupOnActivation(null, itemSkuAndItemCodeMap);
    Assertions.assertEquals(0, requestList.size());
  }

  @Test
  public void getInventoryInsertRequestTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, 0, null,
            false, false);
    Assertions.assertTrue(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestSyncStockFalseTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, false, true, true, false, 0, null,
            false, false);
    Assertions.assertTrue(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestFaasSellerTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, true, 0, null, false, false);
    Assertions.assertTrue(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestNonFaasSellerTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, true, 0, null, false, false);
    Assertions.assertTrue(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestPurchaseTermPurchaseOrderTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, 0, null,
            false, false);
    Assertions.assertTrue(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestPurchaseTermPurchaseOrderFbbfalseTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, false, true, false, 0, null,
            false, false);
    Assertions.assertFalse(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void getInventoryInsertRequestPurchaseTermPurchaseOrderMppForWhTest() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    ProductLevel3Inventory productLevel3Inventory =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, false, false, 0, null,
            false, false);
    Assertions.assertFalse(productLevel3Inventory.isFbbPP());
  }

  @Test
  public void hasInvalidImagePathTest() {
    ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    ProductLevel3SummaryDetailsImageRequest imageRequest1 = null;
    imageRequest.setReviewType(Constants.NEW);
    imageRequest.setLocationPath("location-path");
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        false, false));
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest1),
        false, true));
    imageRequest.setReviewType("");
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        false, true));
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        true, true));
    imageRequest.setReviewType(Constants.NEW);
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        false, true));
    imageRequest.setLocationPath("");
    Assertions.assertTrue(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        false, true));

    imageRequest.setReviewType(Constants.NEW);
    imageRequest.setLocationPath(PRODUCT_SKU);
    Assertions.assertFalse(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        true, true));
    imageRequest.setLocationPath("location-path");
    Assertions.assertTrue(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        true, true));
    imageRequest.setLocationPath("");
    Assertions.assertTrue(RequestHelper.hasInvalidImagePath(PRODUCT_SKU, Arrays.asList(imageRequest),
        true, true));
  }

  @Test
  public void toProductLevel3InventoryListTest() throws Exception {
    InventoryUpsertModel inventoryUpsertModel =
        InventoryUpsertModel.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).itemCode(ITEM_CODE)
            .productSku(PRODUCT_SKU).itemSku(ITEM_SKU).oldPickupPointCode(PICKUP_POINT_CODE)
            .newPickupPointCode(PICKUP_POINT_CODE_2).stock(0).minimumStock(0).syncStock(true).fbbActive(true).build();
    List<ProductLevel3Inventory> productLevel3InventoryList = RequestHelper.toProductLevel3InventoryList(
        ProfileResponse.builder().company(CompanyDTO.builder().build()).build(), Arrays.asList(inventoryUpsertModel),
        false, false);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, productLevel3InventoryList.get(0).getWebMerchantCode());
    Assertions.assertEquals(ITEM_CODE, productLevel3InventoryList.get(0).getWarehouseItemSku());
    Assertions.assertEquals(PRODUCT_SKU, productLevel3InventoryList.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, productLevel3InventoryList.get(0).getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_2, productLevel3InventoryList.get(0).getWebPickupPointCode());
    Assertions.assertEquals(0, productLevel3InventoryList.get(0).getWebAvailable().intValue());
    Assertions.assertEquals(0, productLevel3InventoryList.get(0).getWebMinAlert().intValue());
  }

  @Test
  public void toWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOsTest() throws Exception {
    InventoryUpsertModel inventoryUpsertModel =
        InventoryUpsertModel.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).itemCode(ITEM_CODE)
            .productSku(PRODUCT_SKU).itemSku(ITEM_SKU).oldPickupPointCode(PICKUP_POINT_CODE)
            .newPickupPointCode(PICKUP_POINT_CODE_2).stock(0).minimumStock(0).syncStock(true).fbbActive(true).build();
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> productLevel3InventoryList = RequestHelper.toWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(Arrays.asList(inventoryUpsertModel));
    Assertions.assertEquals(ITEM_SKU, productLevel3InventoryList.get(0).getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, productLevel3InventoryList.get(0).getPickupPointCode());
  }

  @Test
  public void toInventoryUpsertModelsTest()  {
    PickupPointRequest pickupPointRequest = new PickupPointRequest();
    pickupPointRequest.setItemSku(ITEM_SKU);
    pickupPointRequest.setItemName(ITEM_SKU);
    pickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    PickupPointUpdateRequest pickupPointUpdateRequest = new PickupPointUpdateRequest();
    pickupPointUpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pickupPointUpdateRequest.setProductSku(PRODUCT_SKU);
    pickupPointUpdateRequest.setFbbActivated(true);
    pickupPointUpdateRequest.setItemsPickupPoint(Arrays.asList(pickupPointRequest));

    List<InventoryUpsertModel> inventoryUpsertModels = RequestHelper.toInventoryUpsertModels(pickupPointUpdateRequest,
        ImmutableMap.of(ITEM_SKU, Pair.of(PICKUP_POINT_CODE_2, ITEM_CODE)));

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, inventoryUpsertModels.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(ITEM_CODE, inventoryUpsertModels.get(0).getItemCode());
    Assertions.assertEquals(PRODUCT_SKU, inventoryUpsertModels.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, inventoryUpsertModels.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_2, inventoryUpsertModels.get(0).getOldPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, inventoryUpsertModels.get(0).getNewPickupPointCode());
    Assertions.assertTrue(inventoryUpsertModels.get(0).getSyncStock());
    Assertions.assertTrue(inventoryUpsertModels.get(0).getFbbActive());
  }

  @Test
  public void toInventoryUpsertModels_2Test() {
    WebInventoryUpdatePickupPointRequestDTO inventoryRequest = new WebInventoryUpdatePickupPointRequestDTO();
    inventoryRequest.setWebItemSku(ITEM_SKU);
    inventoryRequest.setPickupPointCode(PICKUP_POINT_CODE_2);
    inventoryRequest.setNewPickupPointCode(PICKUP_POINT_CODE);
    inventoryRequest.setWebMerchantCode(BUSINESS_PARTNER_CODE);

    List<InventoryUpsertModel> inventoryUpsertModels =
        RequestHelper.toInventoryUpsertModels(BUSINESS_PARTNER_CODE, PRODUCT_SKU, Arrays.asList(inventoryRequest), true,
            ImmutableMap.of(ITEM_SKU, Pair.of(PICKUP_POINT_CODE_2, ITEM_CODE)));

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, inventoryUpsertModels.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(ITEM_CODE, inventoryUpsertModels.get(0).getItemCode());
    Assertions.assertEquals(PRODUCT_SKU, inventoryUpsertModels.get(0).getProductSku());
    Assertions.assertEquals(ITEM_SKU, inventoryUpsertModels.get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_2, inventoryUpsertModels.get(0).getOldPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, inventoryUpsertModels.get(0).getNewPickupPointCode());
    Assertions.assertTrue(inventoryUpsertModels.get(0).getSyncStock());
    Assertions.assertTrue(inventoryUpsertModels.get(0).getFbbActive());
  }

  @Test
  public void isEligibleForPCBUpdate() {
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    editProductDetailRequest.setProductRequest(new ProductRequest());
    boolean result1 = RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest);
    Assertions.assertFalse(result1);
    editProductDetailRequest.setProductRequest(null);
    productDetailEditDTO.setEligibleForPCBUpdate(false);
    boolean result2 = RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest);
    Assertions.assertFalse(result2);
    productDetailEditDTO.setEligibleForPCBUpdate(true);
    boolean result3 = RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest);
    Assertions.assertTrue(result3);
    productDetailEditDTO.setEligibleForPCBUpdate(false);
    productDetailEditDTO.setProductImageEditRequests(Arrays.asList(new ProductImageEditRequest()));
    boolean result4 = RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest);
    Assertions.assertTrue(result4);
    editProductDetailRequest.setProductRequest(new ProductRequest());
    productDetailEditDTO.setProductItemUpcCodeUpdateRequests(Arrays.asList(new ProductItemUpcCodeUpdateRequest()));
    boolean result5 = RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest);
    Assertions.assertTrue(result5);
  }

  @Test
  public void getUpdateOrInsertStockVoTest() {
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, updateOrInsertStockVo.getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, updateOrInsertStockVo.getProductSku());
    Assertions.assertEquals(ITEM_SKU, updateOrInsertStockVo.getItemSku());
    Assertions.assertEquals(ITEM_CODE, updateOrInsertStockVo.getItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, updateOrInsertStockVo.getPickupPointCode());
    Assertions.assertEquals(1, updateOrInsertStockVo.getStock());
    Assertions.assertEquals(1, updateOrInsertStockVo.getMinimumStock());
    Assertions.assertEquals(profileResponseData, updateOrInsertStockVo.getProfileResponse());
    Assertions.assertTrue(updateOrInsertStockVo.isSyncStock());
    Assertions.assertTrue(updateOrInsertStockVo.isFbbActive());
    Assertions.assertTrue(updateOrInsertStockVo.isMppForWhEnabled());
  }


  @Test
  public void getSyncStockUpdateVo() {
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        RequestHelper.getSyncStockUpdateVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, null);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getProductSku());
    Assertions.assertEquals(ITEM_SKU, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getItemSku());
    Assertions.assertEquals(ITEM_CODE, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getPickupPointCode());
    Assertions.assertEquals(0, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getStock());
    Assertions.assertEquals(0, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getMinimumStock());
    Assertions.assertEquals(1, syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().size());
    Assertions.assertEquals(ITEM_SKU, syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).getPickupPointCode());
    Assertions.assertEquals(profileResponseData, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getProfileResponse());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).isSyncStock());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isSyncStock());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isFbbActive());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isMppForWhEnabled());
  }

  @Test
  public void getUpdateOrInsertStockVoTest2() {
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData, itemSummaryListResponse,
            quickEditV2Request);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, updateOrInsertStockVo.getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, updateOrInsertStockVo.getProductSku());
    Assertions.assertEquals(ITEM_SKU, updateOrInsertStockVo.getItemSku());
    Assertions.assertEquals(ITEM_CODE, updateOrInsertStockVo.getItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, updateOrInsertStockVo.getPickupPointCode());
    Assertions.assertEquals(1, updateOrInsertStockVo.getStock());
    Assertions.assertEquals(0, updateOrInsertStockVo.getMinimumStock());
    Assertions.assertEquals(profileResponseData, updateOrInsertStockVo.getProfileResponse());
    Assertions.assertTrue(updateOrInsertStockVo.isSyncStock());
    Assertions.assertTrue(updateOrInsertStockVo.isFbbActive());
    Assertions.assertTrue(updateOrInsertStockVo.isMppForWhEnabled());
  }


  @Test
  public void getSyncStockUpdateVo2() {
    SyncStockUpdateOrInsertVo syncStockUpdateOrInsertVo =
        RequestHelper.getSyncStockUpdateVo(true, BUSINESS_PARTNER_CODE, profileResponseData, itemSummaryListResponse,
            quickEditV2Request, null);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getBusinessPartnerCode());
    Assertions.assertEquals(PRODUCT_SKU, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getProductSku());
    Assertions.assertEquals(ITEM_SKU, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getItemSku());
    Assertions.assertEquals(ITEM_CODE, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getItemCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getPickupPointCode());
    Assertions.assertEquals(0, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getStock());
    Assertions.assertEquals(0, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getMinimumStock());
    Assertions.assertEquals(1, syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().size());
    Assertions.assertEquals(ITEM_SKU, syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).getPickupPointCode());
    Assertions.assertEquals(profileResponseData, syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().getProfileResponse());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getPickupPointSyncStockDtoList().get(0).isSyncStock());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isSyncStock());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isFbbActive());
    Assertions.assertTrue(syncStockUpdateOrInsertVo.getUpdateOrInsertStockVo().isMppForWhEnabled());
  }

  @Test
  public void isFbbPickupPointWithoutPinPointTest() {
    Assertions.assertFalse(RequestHelper.isFbbPickupPointWithoutPinPoint(new ArrayList<>()));
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    Assertions.assertTrue(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
    pickupPointResponse.setGeolocation(new GeolocationDTO());
    Assertions.assertTrue(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
    GeolocationDTO geolocationDTO = new GeolocationDTO();
    geolocationDTO.setLatitude(0.0);
    pickupPointResponse.setGeolocation(geolocationDTO);
    Assertions.assertTrue(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
    geolocationDTO.setLongitude(0.0);
    Assertions.assertTrue(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
    geolocationDTO.setLatitude(1.0);
    Assertions.assertTrue(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
    geolocationDTO.setLongitude(1.0);
    Assertions.assertFalse(RequestHelper.isFbbPickupPointWithoutPinPoint(Collections.singletonList(pickupPointResponse)));
  }

  @Test
  public void getSizeChartAttributeCodeWithValuesWithoutDelimiterTest() {
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();

    Set<String> attributeIds =
        RequestHelper.getSizeChartAttributeCodeWithValuesWithoutDelimiter(productCreationRequest, SIZE_CHART_DELIMITER);

    Assertions.assertEquals(1, attributeIds.size());
    Assertions.assertEquals(ID, attributeIds.stream().findFirst().get());
  }

  @Test
  public void updateProductAllowedAttributeValueTest() {
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();
    Map<String, String> allowedAttributeValueIdAndValueTypeMap = Map.of(ALLOWED_ATTRIBUTE_VALUE_ID, "UK-S");
    Map<String, Map<String, String>> result = RequestHelper.updateProductAllowedAttributeValue(productCreationRequest,
        allowedAttributeValueIdAndValueTypeMap);
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.containsKey(ATTRIBUTE_CODE));
    Assertions.assertTrue(result.get(ATTRIBUTE_CODE).containsKey("S"));
    Assertions.assertEquals("UK-S", result.get(ATTRIBUTE_CODE).get("S"));
    Assertions.assertEquals("UK-S", productCreationRequest.getProductAttributes().get(0).getProductAttributeValues().get(0)
        .getAllowedAttributeValue().getValue());
  }

  @Test
  public void updateProductItemAttributeValueTest() {
    ProductCreationRequest productCreationRequest = getProductRequestForSizeChartValidation();
    Map<String, Map<String, String>> attributeCodeAndValueValueType = Map.of(ATTRIBUTE_CODE, Map.of("S", "UK-S"));
    RequestHelper.updateProductItemAttributeValue(productCreationRequest, attributeCodeAndValueValueType);
    Assertions.assertEquals("UK-S",
        productCreationRequest.getProductItemRequests().get(0).getProductItemAttributeValueRequests().get(0)
            .getValue());
  }

  @Test
  public void populateQuickEditV2Request_switchOnTest() {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
            .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setChannel(Constants.CNC_CHANNEL);
    itemSummaryListResponse.setItemViewConfigs(Collections.singleton(itemViewConfigDTO));
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, status, true);
    assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice(), 0);
  }

  @Test
  public void populateQuickEditV2Request_switchOn_cncPresentTest() {
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    quickEditV2Request.setCncStatus(ProductLevel3Status.ONLINE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
            .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, status, true);
    assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice(), 0);
  }

  @Test
  public void populateQuickEditV2Request_switchOn_statusOfflineTest() {
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
            .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, status, true);
    assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice(), 0);
  }

  @Test
  public void populateQuickEditV2Request_switchOn_statusCncOfflineTest() {
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    quickEditV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
            .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, status, true);
    assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice(), 0);
  }

  @Test
  public void populateQuickEditV2Request_switchOn_statusTeaserTest() {
    String status = ProductLevel3Status.ONLINE.name();
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(ITEM_SKU);
    quickEditV2Request.setPickupPointCode(PICKUP_POINT_CODE);
    quickEditV2Request.setCncStatus(ProductLevel3Status.ONLINE);
    quickEditV2Request.setStatus(ProductLevel3Status.TEASER);
    ItemSummaryListResponse itemSummaryListResponse = new ItemSummaryListResponse();
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(NORMAL_PRICE);
    priceDTO.setOfferPrice(SALE_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(
        DiscountPriceDTO.builder().discountPrice(DISCOUNT_PRICE).endDateTime(new Date())
            .startDateTime(new Date()).build());
    itemSummaryListResponse.setPrice(Collections.singleton(priceDTO));
    QuickEditV2Request populatedRequest =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse, status, status, true);
    assertNotNull(populatedRequest);
    Assertions.assertEquals(NORMAL_PRICE, populatedRequest.getPrice().getPrice(), 0);
    Assertions.assertEquals(ProductLevel3Status.OFFLINE, populatedRequest.getCncStatus());
  }


  private ProductCreationRequest getProductRequestForSizeChartValidation() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
    attributeRequest1.setId(ID);
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setId(UUID.randomUUID().toString());
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    AllowedAttributeValueRequest allowedAttributeValueRequest1 = new AllowedAttributeValueRequest("S", 1, "10001");
    allowedAttributeValueRequest1.setId(ALLOWED_ATTRIBUTE_VALUE_ID);
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setAllowedAttributeValue(allowedAttributeValueRequest1);

    AllowedAttributeValueRequest allowedAttributeValueRequest2 = new AllowedAttributeValueRequest("UK-S", 1, "10001");
    allowedAttributeValueRequest2.setId(UUID.randomUUID().toString());
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setAllowedAttributeValue(allowedAttributeValueRequest2);

    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(
        Arrays.asList(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3));

    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);

    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);

    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();


    ProductItemAttributeValueRequest productItemAttributeValueRequest1 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue("S");

    ProductItemAttributeValueRequest productItemAttributeValueRequest2 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest2.setAttribute(attributeRequest1);
    productItemAttributeValueRequest2.setValue("M");

    ProductItemAttributeValueRequest productItemAttributeValueRequest3 = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest3.setAttribute(attributeRequest2);

    ProductItemAttributeValueRequest productItemAttributeValueRequest4 = new ProductItemAttributeValueRequest();

    TreeMap<String, String> attributesMap = new TreeMap<>(Map.of(ATTRIBUTE_CODE, "S", ATTRIBUTE_CODE_2, "M"));

    ProductItemCreationRequest productItemCreationRequest1 = new ProductItemCreationRequest();
    productItemCreationRequest1.setProductItemAttributeValueRequests(
        Arrays.asList(productItemAttributeValueRequest1, productItemAttributeValueRequest2,
            productItemAttributeValueRequest3, productItemAttributeValueRequest4));
    productItemCreationRequest1.setAttributesMap(attributesMap);

    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setProductAttributes(
        Arrays.asList(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));
    productCreationRequest.setProductItemRequests(Arrays.asList(productItemCreationRequest1));

    return productCreationRequest;
  }

  @Test
  public void validateProductDefiningAttributeRequestTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    AllowedAttributeValueRequest allowedAttributeValueRequest1 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest1.setValue("S");
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setAllowedAttributeValue(allowedAttributeValueRequest1);

    AllowedAttributeValueRequest allowedAttributeValueRequest2 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest2.setValue("UK-M");
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setAllowedAttributeValue(allowedAttributeValueRequest2);

    AllowedAttributeValueRequest allowedAttributeValueRequest3 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest3.setValue("");
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest3.setAllowedAttributeValue(allowedAttributeValueRequest3);

    AllowedAttributeValueRequest allowedAttributeValueRequest4 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest4.setValue("S");
    allowedAttributeValueRequest4.setMarkForDelete(true);
    ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
    productAttributeValueRequest4.setAllowedAttributeValue(allowedAttributeValueRequest4);

    AllowedAttributeValueRequest allowedAttributeValueRequest5 = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest5.setValue("S");
    ProductAttributeValueRequest productAttributeValueRequest5 = new ProductAttributeValueRequest();
    productAttributeValueRequest5.setAllowedAttributeValue(allowedAttributeValueRequest5);
    productAttributeValueRequest5.setMarkForDelete(true);

    ProductAttributeValueRequest productAttributeValueRequest6 = new ProductAttributeValueRequest();

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(
        List.of(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3,
            productAttributeValueRequest4, productAttributeValueRequest5, productAttributeValueRequest6));

    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);

    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);

    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();

    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductAttributes(
        List.of(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));

    RequestHelper.validateProductAttributeRequest(productRequest, "-", true);

    RequestHelper.validateProductAttributeRequest(productRequest, "-", false);

    allowedAttributeValueRequest2.setValue("UK-S");
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      RequestHelper.validateProductAttributeRequest(productRequest, "-", true);
    });
  }


  @Test
  public void validateProductPredefinedAttributeRequestTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest1 = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest1.setValue("S");
    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest1);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest2 = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest2.setValue("UK-M");
    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest2);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest3 = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest3.setValue("");
    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest3);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest4 = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest4.setValue("S");
    predefinedAllowedAttributeValueRequest4.setMarkForDelete(true);
    ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
    productAttributeValueRequest4.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest4);

    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest5 = new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest5.setValue("S");
    ProductAttributeValueRequest productAttributeValueRequest5 = new ProductAttributeValueRequest();
    productAttributeValueRequest5.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest5);
    productAttributeValueRequest5.setMarkForDelete(true);

    ProductAttributeValueRequest productAttributeValueRequest6 = new ProductAttributeValueRequest();

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(
        List.of(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3,
            productAttributeValueRequest4, productAttributeValueRequest5, productAttributeValueRequest6));

    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);

    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);

    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();

    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductAttributes(
        List.of(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));

    RequestHelper.validateProductAttributeRequest(productRequest, "-", true);

    RequestHelper.validateProductAttributeRequest(productRequest, "-", false);

    predefinedAllowedAttributeValueRequest2.setValue("UK-S");
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      RequestHelper.validateProductAttributeRequest(productRequest, "-", true);
    });
  }


  @Test
  public void validateProductDescriptiveAttributeRequestTest() {
    AttributeRequest attributeRequest1 = new AttributeRequest();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE);
    attributeRequest1.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);

    AttributeRequest attributeRequest2 = new AttributeRequest();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);

    ProductAttributeValueRequest productAttributeValueRequest1 = new ProductAttributeValueRequest();
    productAttributeValueRequest1.setDescriptiveAttributeValue("S");

    ProductAttributeValueRequest productAttributeValueRequest2 = new ProductAttributeValueRequest();
    productAttributeValueRequest2.setDescriptiveAttributeValue("UK-M");

    ProductAttributeValueRequest productAttributeValueRequest3 = new ProductAttributeValueRequest();
    productAttributeValueRequest3.setDescriptiveAttributeValue("");

    ProductAttributeValueRequest productAttributeValueRequest4 = new ProductAttributeValueRequest();
    productAttributeValueRequest4.setDescriptiveAttributeValue("S");
    productAttributeValueRequest4.setMarkForDelete(true);

    ProductAttributeValueRequest productAttributeValueRequest5 = new ProductAttributeValueRequest();

    ProductAttributeRequest productAttributeRequest1 = new ProductAttributeRequest();
    productAttributeRequest1.setAttribute(attributeRequest1);
    productAttributeRequest1.setProductAttributeValues(
        List.of(productAttributeValueRequest1, productAttributeValueRequest2, productAttributeValueRequest3,
            productAttributeValueRequest4, productAttributeValueRequest5));

    ProductAttributeRequest productAttributeRequest2 = new ProductAttributeRequest();
    productAttributeRequest2.setAttribute(attributeRequest1);

    ProductAttributeRequest productAttributeRequest3 = new ProductAttributeRequest();
    productAttributeRequest3.setAttribute(attributeRequest2);

    ProductAttributeRequest productAttributeRequest4 = new ProductAttributeRequest();

    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductAttributes(
        List.of(productAttributeRequest1, productAttributeRequest2, productAttributeRequest3,
            productAttributeRequest4));

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      RequestHelper.validateProductAttributeRequest(productRequest, "-", true);
    });

    RequestHelper.validateProductAttributeRequest(productRequest, "-", false);

    productAttributeValueRequest2.setDescriptiveAttributeValue("UK-S");
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      RequestHelper.validateProductAttributeRequest(productRequest, "-", true);
    });
  }


  @Test
  public void validateForFAASSellerTest() {
    profileResponseData.setFlags(Map.of(Constants.FAAS_ACTIVATED, true));
    FbbCreatePickupPointRequest fbbCreatePickupPointRequest = new FbbCreatePickupPointRequest();
    fbbCreatePickupPointRequest.setSynchronizeStock(true);
    FbbCreatePickupPointResponse fbbCreatePickupPointResponse = new FbbCreatePickupPointResponse();

    RequestHelper.validateForFAASSeller(profileResponseData, fbbCreatePickupPointRequest, fbbCreatePickupPointResponse,
        true);
    Assertions.assertEquals(ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getDesc(),
        fbbCreatePickupPointResponse.getReason());

    fbbCreatePickupPointResponse.setReason(StringUtils.EMPTY);
    RequestHelper.validateForFAASSeller(profileResponseData, fbbCreatePickupPointRequest, fbbCreatePickupPointResponse,
        false);
    Assertions.assertEquals(StringUtils.EMPTY, fbbCreatePickupPointResponse.getReason());

    fbbCreatePickupPointRequest.setSynchronizeStock(false);
    RequestHelper.validateForFAASSeller(profileResponseData, fbbCreatePickupPointRequest, fbbCreatePickupPointResponse,
        true);
    Assertions.assertEquals(StringUtils.EMPTY, fbbCreatePickupPointResponse.getReason());
  }

  @Test
  public void testWithValidPickupPointDeleteRequests() {
    List<PickupPointDeleteRequest> pickupPointDeleteRequests = new ArrayList<>();
    pickupPointDeleteRequests.add(new PickupPointDeleteRequest(PICKUP_POINT_CODE, ITEM_SKU));
    pickupPointDeleteRequests.add(new PickupPointDeleteRequest(PICKUP_POINT_CODE_2, ITEM_SKU_2));
    ListRequestDTO<InventoryDetailInfoRequestDTO> result =
        RequestHelper.toInventoryDetailInfoRequestDTO(pickupPointDeleteRequests, BUSINESS_PARTNER_CODE);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(2, result.getList().size());

    InventoryDetailInfoRequestDTO firstRequest = result.getList().get(0);
    Assertions.assertEquals(PICKUP_POINT_CODE, firstRequest.getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, firstRequest.getWebItemSku());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE , firstRequest.getWebMerchantCode());
  }


  @Test
  public void toProductAttributeValueRequestTest() {
    ProductAttributeValue productAttributeValue1 = new ProductAttributeValue();
    productAttributeValue1.setDescriptiveAttributeValue("descriptiveValue");
    productAttributeValue1.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);

    AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
    allowedAttributeValue.setAllowedAttributeCode("allowedAttributeValueCode");
    allowedAttributeValue.setValue("allowedAttributeValue");
    allowedAttributeValue.setValueType("UK");
    allowedAttributeValue.setSequence(2);
    ProductAttributeValue productAttributeValue2 = new ProductAttributeValue();
    productAttributeValue2.setAllowedAttributeValue(allowedAttributeValue);

    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode("predefinedAllowedAttributeValueCode");
    predefinedAllowedAttributeValue.setValue("predefinedAllowedAttributeValue");
    predefinedAllowedAttributeValue.setSequence(3);
    ProductAttributeValue productAttributeValue3 = new ProductAttributeValue();
    productAttributeValue3.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);

    List<ProductAttributeValueRequest> productAttributeValueRequests = RequestHelper.toProductAttributeValueRequest(
        Arrays.asList(productAttributeValue1, productAttributeValue2, productAttributeValue3));

    Assertions.assertEquals(3, productAttributeValueRequests.size());

    Assertions.assertEquals(productAttributeValue1.getDescriptiveAttributeValue(),
        productAttributeValueRequests.get(0).getDescriptiveAttributeValue());
    Assertions.assertEquals(com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE,
        productAttributeValueRequests.get(0).getDescriptiveAttributeValueType());
    Assertions.assertNull(productAttributeValueRequests.get(0).getAllowedAttributeValue());
    Assertions.assertNull(productAttributeValueRequests.get(0).getPredefinedAllowedAttributeValue());

    Assertions.assertEquals(allowedAttributeValue.getAllowedAttributeCode(),
        productAttributeValueRequests.get(1).getAllowedAttributeValue().getAllowedAttributeCode());
    Assertions.assertEquals(allowedAttributeValue.getValue(),
        productAttributeValueRequests.get(1).getAllowedAttributeValue().getValue());
    Assertions.assertEquals(allowedAttributeValue.getValueType(),
        productAttributeValueRequests.get(1).getAllowedAttributeValue().getValueType());
    Assertions.assertEquals(allowedAttributeValue.getSequence(),
        productAttributeValueRequests.get(1).getAllowedAttributeValue().getSequence());
    Assertions.assertNull(productAttributeValueRequests.get(1).getDescriptiveAttributeValue());
    Assertions.assertNull(productAttributeValueRequests.get(1).getDescriptiveAttributeValueType());
    Assertions.assertNull(productAttributeValueRequests.get(1).getPredefinedAllowedAttributeValue());

    Assertions.assertEquals(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode(),
        productAttributeValueRequests.get(2).getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
    Assertions.assertEquals(predefinedAllowedAttributeValue.getValue(),
        productAttributeValueRequests.get(2).getPredefinedAllowedAttributeValue().getValue());
    Assertions.assertEquals(predefinedAllowedAttributeValue.getSequence(),
        productAttributeValueRequests.get(2).getPredefinedAllowedAttributeValue().getSequence());
    Assertions.assertNull(productAttributeValueRequests.get(2).getDescriptiveAttributeValue());
    Assertions.assertNull(productAttributeValueRequests.get(2).getDescriptiveAttributeValueType());
    Assertions.assertNull(productAttributeValueRequests.get(2).getAllowedAttributeValue());
  }

  @Test
  public void testSetProductBusinessPartnerAttribute_WhenAttributeAlreadyPresent() {
    RequestHelper.setProductBusinessPartnerAttribute("1001", "attr_id", "NewValue", new ProductBusinessPartner());
  }

  @Test
  public void testGetXProdAttributeMigrationModel() {
    XProdAttributeMigrationEventModel model =
        RequestHelper.getXProdAttributeMigrationModel("10001", "attr_id", "NewValue", "value",
            false, PRODUCT_SKU);
    assertNotNull(model);
  }

  @Test
  void testSetVideoAddEditRequestForPCB_WhenVideoUpdated() {
    ProductRequest productRequest = new ProductRequest();
    EditProductResponse editProductResponse = new EditProductResponse();
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    editProductResponse.setVideoUpdated(true);
    videoAddEditRequest.setVideoUrl(VIDEO_URL);
    editProductResponse.setVideoAddEditRequest(videoAddEditRequest);
    RequestHelper.setVideoAddEditRequestForPCB(productRequest, editProductResponse);
    assertNotNull(productRequest.getVideoAddEditRequest());
    assertEquals(VIDEO_URL, productRequest.getVideoAddEditRequest().getVideoUrl());
  }

  @Test
  void testSetVideoAddEditRequestForPCB_WhenVideoDeleted() {
    ProductRequest productRequest = new ProductRequest();
    EditProductResponse editProductResponse = new EditProductResponse();
    editProductResponse.setVideoUpdated(true);
    editProductResponse.setVideoAddEditRequest(null);
    RequestHelper.setVideoAddEditRequestForPCB(productRequest, editProductResponse);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testSetVideoAddEditRequestForPCB_WhenVideoNotUpdated() {
    ProductRequest productRequest = new ProductRequest();
    EditProductResponse editProductResponse = new EditProductResponse();
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    editProductResponse.setVideoUpdated(false);
    editProductResponse.setVideoAddEditRequest(videoAddEditRequest);
    RequestHelper.setVideoAddEditRequestForPCB(productRequest, editProductResponse);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testSetVideoAddEditRequestForPCB_WithNullResponse() {
    ProductRequest productRequest = new ProductRequest();
    RequestHelper.setVideoAddEditRequestForPCB(productRequest, null);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testSetVideoAddEditRequestForXProduct_WhenVideoUpdated() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest =
        new com.gdn.x.product.rest.web.model.request.ProductRequest();
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    productLevel3.setVideoUpdated(true);
    videoAddEditRequest.setVideoUrl(VIDEO_URL);
    productLevel3.setVideoAddEditRequest(videoAddEditRequest);
    RequestHelper.setVideoAddEditRequestForXProduct(productLevel3, productRequest);

    assertNotNull(productRequest.getVideoAddEditRequest());
    assertEquals(VIDEO_URL, productRequest.getVideoAddEditRequest().getVideoUrl());
  }

  @Test
  void testSetVideoAddEditRequestForXProduct_WhenVideoRemoved() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest =
        new com.gdn.x.product.rest.web.model.request.ProductRequest();
    productLevel3.setVideoAddEditRequest(null);
    productLevel3.setVideoUpdated(true);
    RequestHelper.setVideoAddEditRequestForXProduct(productLevel3, productRequest);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testSetVideoAddEditRequestForXProduct_WhenVideoNotUpdated() {
    ProductLevel3 productLevel3 = new ProductLevel3();
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest =
        new com.gdn.x.product.rest.web.model.request.ProductRequest();
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    productLevel3.setVideoUpdated(false);
    productLevel3.setVideoAddEditRequest(videoAddEditRequest);
    RequestHelper.setVideoAddEditRequestForXProduct(productLevel3, productRequest);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testSetVideoAddEditRequestForXProduct_WithNullProduct() {
    com.gdn.x.product.rest.web.model.request.ProductRequest productRequest =
        new com.gdn.x.product.rest.web.model.request.ProductRequest();
    RequestHelper.setVideoAddEditRequestForXProduct(null, productRequest);
    assertNull(productRequest.getVideoAddEditRequest());
  }

  @Test
  void testPrepareMasterDataEditRequestForPCBUpdate_WithDimensionsUpdate() {
    // Setup
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();

    request.setProductCode(PRODUCT_CODE);
    request.setCategoryCode("TEST-CAT");
    request.setBusinessPartnerCode("TEST-BP");
    request.setHeight(10.0);
    request.setWidth(20.0);
    request.setLength(30.0);
    request.setWeight(40.0);
    request.setShippingWeight(50.0);
    request.setSizeChartCode("SIZE-CHART");
    request.setUrl("test-url");
    request.setProductName(PRODUCT_NAME);
    request.setDescription("Test Description");

    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoUrl("test-video-url");
    videoAddEditRequest.setVideoName("video_name");
    request.setVideoAddEditRequest(videoAddEditRequest);

    request.getMasterDataEditChangeTypes().add(L3InfoUpdateChangeType.DIMENSIONS_UPDATE);

    masterProductEditDTO.setProductCollection(productCollection);
    masterProductEditDTO.getProductCollection().setReviewPending(true);

    List<ProductImageEditRequest> imageRequests = new ArrayList<>();
    ProductImageEditRequest imageRequest = new ProductImageEditRequest();
    imageRequest.setImagePath("test-image-url");
    imageRequests.add(imageRequest);
    masterProductEditDTO.setProductImageEditRequestList(imageRequests);
    ProductMasterDataUpdateRequest
        result = RequestHelper.prepareMasterDataEditRequestForPCBUpdate(request, masterProductEditDTO);

    // Verify
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertEquals("TEST-CAT", result.getCategoryCode());
    Assertions.assertEquals("TEST-BP", result.getBusinessPartnerCode());
    Assertions.assertEquals(10.0, result.getHeight());
    Assertions.assertEquals(20.0, result.getWidth());
    Assertions.assertEquals(30.0, result.getLength());
    Assertions.assertEquals(40.0, result.getWeight());
    Assertions.assertEquals(50.0, result.getShippingWeight());
    Assertions.assertEquals("SIZE-CHART", result.getSizeChartCode());
    Assertions.assertEquals(PRODUCT_NAME, result.getName());
    Assertions.assertArrayEquals("Test Description".getBytes(StandardCharsets.UTF_8), result.getDescription());
    Assertions.assertTrue(result.isReviewPending());
    Assertions.assertEquals("test-video-url", result.getVideoAddEditRequest().getVideoUrl());
    Assertions.assertEquals(1, result.getCommonImages().size());
    Assertions.assertEquals("test-image-url", result.getCommonImages().get(0).getImagePath());
    Assertions.assertEquals("video_name", result.getVideoAddEditRequest().getVideoName());
  }

  @Test
  void testPrepareMasterDataEditRequestForPCBUpdate_WithoutDimensionsUpdate() {
    // Setup
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();

    request.setProductCode(PRODUCT_CODE);
    request.setCategoryCode("TEST-CAT");
    request.setBusinessPartnerCode("TEST-BP");
    request.setSizeChartCode("SIZE-CHART");
    request.setUrl("test-url");
    request.setProductName(PRODUCT_NAME);
    request.setDescription("Test Description");

    // Don't add DIMENSIONS_UPDATE to change types

    masterProductEditDTO.setProductCollection(new ProductCollection());
    masterProductEditDTO.getProductCollection().setReviewPending(false);

    // Execute
    ProductMasterDataUpdateRequest result = RequestHelper.prepareMasterDataEditRequestForPCBUpdate(request, masterProductEditDTO);

    // Verify
    Assertions.assertEquals(PRODUCT_CODE, result.getProductCode());
    Assertions.assertEquals("TEST-CAT", result.getCategoryCode());
    Assertions.assertEquals("TEST-BP", result.getBusinessPartnerCode());
    // Dimensions should not be set
    Assertions.assertNull(result.getHeight());
    Assertions.assertNull(result.getWidth());
    Assertions.assertNull(result.getLength());
    Assertions.assertNull(result.getWeight());
    Assertions.assertNull(result.getShippingWeight());
    Assertions.assertEquals("SIZE-CHART", result.getSizeChartCode());
    Assertions.assertEquals(PRODUCT_NAME, result.getName());
    Assertions.assertArrayEquals("Test Description".getBytes(StandardCharsets.UTF_8), result.getDescription());
    Assertions.assertFalse(result.isReviewPending());
  }

  @Test
  void testPrepareMasterDataEditRequestForPCBUpdate_WithNullValues() {
    // Setup
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();

    masterProductEditDTO.setProductCollection(new ProductCollection());

    // Execute
    ProductMasterDataUpdateRequest result = RequestHelper.prepareMasterDataEditRequestForPCBUpdate(request, masterProductEditDTO);

    // Verify
    Assertions.assertNull(result.getProductCode());
    Assertions.assertNull(result.getCategoryCode());
    Assertions.assertNull(result.getBusinessPartnerCode());
    Assertions.assertNull(result.getHeight());
    Assertions.assertNull(result.getWidth());
    Assertions.assertNull(result.getLength());
    Assertions.assertNull(result.getWeight());
    Assertions.assertNull(result.getShippingWeight());
    Assertions.assertNull(result.getSizeChartCode());
    Assertions.assertNull(result.getUrl());
    Assertions.assertNull(result.getName());
    Assertions.assertFalse(result.isReviewPending());
    Assertions.assertNotNull(result.getVideoAddEditRequest());
  }

  @Test
  void testGetProductMasterDataEditRequest_NormalCase() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(true);
    req.setProductType(ProductType.BOPIS.getCode());
    req.setSizeChartCode(SIZE_CHART_CODE);
    req.setMasterDataEditChangeTypes(new HashSet<>());
    ProductBasicMasterFieldsRequest result = RequestHelper.getProductMasterDataEditRequest(req);
    Assertions.assertEquals("SKU123", result.getProductSku());
    Assertions.assertTrue(result.isInstore());
    Assertions.assertEquals(SIZE_CHART_CODE, result.getSizeChartCode());
    Assertions.assertFalse(result.isGenerateProductScoreNeeded());
  }

  @Test
  void testGetProductMasterDataEditRequest_ScoreGenerationNeeded() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(ProductType.BIG_PRODUCT.getCode());
    req.setSizeChartCode(SIZE_CHART_CODE);
    HashSet<L3InfoUpdateChangeType> changeTypes = new HashSet<>();
    changeTypes.add(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    req.setMasterDataEditChangeTypes(changeTypes);
    ProductBasicMasterFieldsRequest result = RequestHelper.getProductMasterDataEditRequest(req);
    Assertions.assertTrue(result.isGenerateProductScoreNeeded());
  }

  @Test
  void testGetProductMasterDataEditRequest_AllScoreFlags() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(ProductType.BOPIS.getCode());
    req.setSizeChartCode(SIZE_CHART_CODE);
    HashSet<L3InfoUpdateChangeType> changeTypes = new HashSet<>();
    changeTypes.add(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    changeTypes.add(L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE);
    changeTypes.add(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE);
    changeTypes.add(L3InfoUpdateChangeType.DIMENSIONS_UPDATE);
    changeTypes.add(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE);
    changeTypes.add(L3InfoUpdateChangeType.VIDEO_UPDATE);
    req.setMasterDataEditChangeTypes(changeTypes);
    ProductBasicMasterFieldsRequest result = RequestHelper.getProductMasterDataEditRequest(req);
    Assertions.assertTrue(result.isGenerateProductScoreNeeded());
  }

  @Test
  void testGetProductMasterDataEditRequest_EmptyChangeTypes() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(ProductType.BOPIS.getCode());
    req.setSizeChartCode("SIZE-CHART-4");
    req.setMasterDataEditChangeTypes(new HashSet<>());
    ProductBasicMasterFieldsRequest result = RequestHelper.getProductMasterDataEditRequest(req);
    Assertions.assertFalse(result.isGenerateProductScoreNeeded());
  }

  @Test
  void testGetProductMasterDataEditRequest_NullChangeTypes() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(ProductType.BOPIS.getCode());
    req.setSizeChartCode(SIZE_CHART_CODE);
    req.setMasterDataEditChangeTypes(null);
    // Should not throw NPE, should treat as empty set
    ProductBasicMasterFieldsRequest result = RequestHelper.getProductMasterDataEditRequest(req);
    Assertions.assertFalse(result.isGenerateProductScoreNeeded());
  }

  @Test
  void testGetProductMasterDataEditRequest_NullProductType() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(null);
    req.setSizeChartCode(SIZE_CHART_CODE);
    req.setMasterDataEditChangeTypes(new HashSet<>());
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      RequestHelper.getProductMasterDataEditRequest(req);
    });
  }

  @Test
  void testGetProductMasterDataEditRequest_InvalidProductType() {
    ProductMasterDataEditRequest req = new ProductMasterDataEditRequest();
    req.setProductSku("SKU123");
    req.setInstore(false);
    req.setProductType(-999); // Invalid code
    req.setSizeChartCode(SIZE_CHART_CODE);
    req.setMasterDataEditChangeTypes(new HashSet<>());
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> {
      RequestHelper.getProductMasterDataEditRequest(req);
    });
  }

  @Test
  void testGetProductMasterDataEditRequest_NullRequest() {
    Assertions.assertThrows(NullPointerException.class, () -> {
      RequestHelper.getProductMasterDataEditRequest(null);
    });
  }

  @Test
  public void getUpdateOrInsertStockVoInitialPreOrderQuotaTest() {
    int initialPreOrderQuota = 10;
    itemPickupPointRequest.setInitialPreOrderQuota(initialPreOrderQuota);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null);
    Assertions.assertEquals(initialPreOrderQuota, updateOrInsertStockVo.getInitialPreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockNullVoInitialPreOrderQuotaTest() {
    itemPickupPointRequest.setInitialPreOrderQuota(null);
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null);
    Assertions.assertEquals(0, updateOrInsertStockVo.getInitialPreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockVo_updatePreOrderQuotaTrue_blibliOMGTrue_Test() {
    profileResponseData.setFlags(Map.of(Constants.BLIBLI_OMG, true));
    
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, true, null);
  
    Assertions.assertTrue(updateOrInsertStockVo.isUpdatePreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockVo_updatePreOrderQuotaTrue_blibliOMGFalse_Test() {
    profileResponseData.setFlags(Map.of(Constants.BLIBLI_OMG, false));
    
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, true, null);
    Assertions.assertFalse(updateOrInsertStockVo.isUpdatePreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockVo_updatePreOrderQuotaFalse_blibliOMGTrue_Test() {
    profileResponseData.setFlags(Map.of(Constants.BLIBLI_OMG, true));
    
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null);
    Assertions.assertFalse(updateOrInsertStockVo.isUpdatePreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockVo_updatePreOrderQuotaFalse_blibliOMGFalse_Test() {
    profileResponseData.setFlags(Map.of(Constants.BLIBLI_OMG, false));
    
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, false, null);
    Assertions.assertFalse(updateOrInsertStockVo.isUpdatePreOrderQuota());
  }

  @Test
  public void getUpdateOrInsertStockVo_updatePreOrderQuota_noFlagsSet_Test() {
    profileResponseData.setFlags(null);
    
    UpdateOrInsertStockVo updateOrInsertStockVo =
        RequestHelper.getUpdateOrInsertStockVo(true, BUSINESS_PARTNER_CODE, profileResponseData,
            productVariantPriceStockAndImagesRequest, itemPickupPointRequest, true, null);
    Assertions.assertFalse(updateOrInsertStockVo.isUpdatePreOrderQuota());
  }

  @Test
  public void getInventoryInsertRequest_WithPreOrderDetails_Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG, true));

    Date futureDate = new Date(System.currentTimeMillis() + (60 * 60 * 1000));
    Integer initialQuota = 50;

    ProductLevel3Inventory result =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, initialQuota,
            null, true, false);

    Assertions.assertNull(result.getPreOrderDate());
    Assertions.assertNull(result.getInitialPreOrderQuota());
  }

  @Test
  public void getInventoryInsertRequest_WithoutPreOrderFeature_Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG, true));

    Date futureDate = new Date();
    Integer initialQuota = 50;

    ProductLevel3Inventory result =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, initialQuota,
            futureDate, false, false); // preOrderFeatureSwitch = false

    Assertions.assertNull(result.getPreOrderDate());
    Assertions.assertNull(result.getInitialPreOrderQuota());
  }

  @Test
  public void getInventoryInsertRequest_BlibliOMGDisabled_Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG, false));

    Date futureDate = new Date(System.currentTimeMillis() + (60 * 60 * 1000));
    Integer initialQuota = 50;

    ProductLevel3Inventory result =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, initialQuota,
            futureDate, true, false);

    Assertions.assertNull(result.getPreOrderDate());
    Assertions.assertNull(result.getInitialPreOrderQuota());
  }

  @Test
  public void getInventoryInsertRequest_InvalidPreOrderDate_Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG, true));

    Date pastDate = new Date(System.currentTimeMillis() - (60 * 60 * 1000));
    Integer initialQuota = 50;

    ProductLevel3Inventory result =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, initialQuota,
            pastDate, true, false);

    Assertions.assertNull(result.getPreOrderDate());
    Assertions.assertNull(result.getInitialPreOrderQuota());
  }

  @Test
  public void getInventoryInsertRequest_NullInitialQuota_Test() throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setCncActivated(true);
    companyDTO.setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    profileResponse.setCompany(companyDTO);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG, true));
    Date futureDate = new Date(System.currentTimeMillis() + (60 * 60 * 1000));

    ProductLevel3Inventory result =
        RequestHelper.getInventoryInsertRequest(profileResponse, BUSINESS_PARTNER_CODE, ITEM_CODE,
            PRODUCT_SKU, ITEM_SKU, PICKUP_POINT_CODE, 0, 1, true, true, true, false, null,
            futureDate, true, false);
    Assertions.assertEquals(futureDate, result.getPreOrderDate());
    Assertions.assertEquals(Integer.valueOf(0), result.getInitialPreOrderQuota());
  }

  @Test
  public void getProductItemUomInfoDTOTest() {
    RequestHelper.getProductItemUomInfoDTO(new ProductItemDistributionInfoRequest());
    ProductItemDistributionInfoRequest productItemDistributionInfoRequest = new ProductItemDistributionInfoRequest();
    productItemDistributionInfoRequest.setDistributionItemInfoRequest(new DistributionItemRequest());
    productItemDistributionInfoRequest.setDimensionsAndUOMRequest(
        Collections.singletonList(new DimensionAndUomRequest()));
    ProductItemUomInfoDTO productItemUomInfoDTO =
        RequestHelper.getProductItemUomInfoDTO(productItemDistributionInfoRequest);
    Assertions.assertNotNull(productItemUomInfoDTO);
  }

  @Test
  public void setItemDistributionInfoTest() {
    RequestHelper.setItemDistributionInfo(false, null, null);
    RequestHelper.setItemDistributionInfo(true, new ProductRequest(), new EditProductResponse());
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductItems(Collections.singletonList(new ProductItemRequest()));
    EditProductResponse editProductResponse = new EditProductResponse();
    ProductItemDistributionInfoRequest productItemDistributionInfoRequest = new ProductItemDistributionInfoRequest();
    productItemDistributionInfoRequest.setSkuCode(ITEM_SKU_4);
    editProductResponse.setDistributionAndUOMRequest(Collections.singletonList(productItemDistributionInfoRequest));
    RequestHelper.setItemDistributionInfo(true, productRequest, editProductResponse);
    ProductItemRequest productItemRequest = new ProductItemRequest();
    productItemRequest.setSkuCode(ITEM_SKU_4);
    productRequest.setProductItems(Collections.singletonList(productItemRequest));
    RequestHelper.setItemDistributionInfo(true, productRequest, editProductResponse);
    Assertions.assertNotNull(productRequest);
  }
}
