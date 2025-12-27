package com.gdn.x.product.client;

import java.net.URI;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.fasterxml.jackson.core.type.TypeReference;
import com.gdn.common.client.GdnRestClientConfiguration;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.common.web.client.GdnBaseRestCrudClient;
import com.gdn.common.web.wrapper.request.SimpleRequestHolder;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.rest.web.model.ApiPath;
import com.gdn.x.product.rest.web.model.ItemBuyableRequest;
import com.gdn.x.product.rest.web.model.ItemBuyableScheduleRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableRequest;
import com.gdn.x.product.rest.web.model.ItemDiscoverableScheduleRequest;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.ProductIdentifierWrapper;
import com.gdn.x.product.rest.web.model.ProductWrapper;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSkuDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCategorySequenceDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.GetShippingTypeRequest;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.request.ItemAndBundlingInfoRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.Level2InventoryListOfLevel2InventoryRequestRestWeb;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfferedSummaryRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemsRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleProductListRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.DefaultItemSkuResponse;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.ItemAndBundlingInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemTypeResponse;
import com.gdn.x.product.rest.web.model.response.Level1Level2ListInventoryResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.Level2InventoryCountResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfferedComboSummaryResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.PristineCategoryAttributeMapResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataResponse;
import com.gdn.x.product.rest.web.model.response.PristineProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsAvailabilityResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductsToItemCatalogMappingResponse;
import com.gdn.x.product.rest.web.model.response.ReviewProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductListResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.rest.web.model.response.UpsertOfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;

public class ProductClient extends GdnBaseRestCrudClient {

  private static final String CATALOG_CODE = "catalogCode";

  private static final String OLD_CATEGORY_CODE = "oldCategoryCode";

  private static final String CATEGORY_CODE_KEY = "categoryCode";

  private static final String PRODUCT_CODE_KEY = "productCode";

  private static final String PRISTINE_ID = "pristineId";

  private static final String PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU = "pristineIdOrProductCodeOrSku";

  private static final String ID = "id";

  private static final String DEFAULT_SKU = "defaultSku";

  private static final String STOCK_KEY = "stock";

  private static final String LEVEL_2_MERCHANT_CODE_KEY = "level2MerchantCode";

  private static final String PICKUP_POINT_CODE_KEY = "pickupPointCode";

  private static final String INSTANT_PICKUP_KEY = "instantPickup";

  private static final String ITEM_SKU_KEY = "itemSku";

  private static final String ITEM_CODE = "itemCode";

  private static final String DO_ARCHIVE_FLAG = "doArchive";

  private static final String IS_SUSPENDED_FLAG = "isSuspended";

  private static final String CHANNEL_KEY = "channel";

  private static final String PRODUCT_SKU_KEY = "productSku";

  private static final String SUSPEND_PRODUCT = "suspendProduct";

  private static final String REPLACE_SKU_KEY = "replace";

  private static final String PRODUCT_CATENTRY_ID_KEY = "productCatentryId";

  private static final String OVERWRITE_EXISTING_MASTER_DATA_KEY = "overwriteExistingMasterData";

  private static final String MERCHANT_SKU_KEY = "merchantSku";

  private static final String MERCHANT_CODE_KEY = "merchantCode";

  private static final String NEW_CATEGORY_CODE = "newCategoryCode";

  private static final String SHOW_DELETED = "showDeleted";

  private static final String BRAND = "brand";

  private static final String OFFLINE_ITEM_ID = "offlineItemId";

  private static final String UNIQUE_ID = "uniqueId";

  public ProductClient(GdnRestClientConfiguration clientConfig) {
    super(clientConfig);
    this.setContextPath(ClientConstants.V1);
  }

  public ProductClient(String username, String password, String host, Integer port, String storeId,
      String clientId, String channelId) {
    super(username, password, host, port, clientId, channelId, storeId, ClientConstants.V1);
  }

  public GdnBaseRestResponse activateOff2OnChannelActive(String requestId, String username,
      String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.OFF2ON_ACTIVATE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<SimpleListStringResponse> activateOff2OnChannelActiveBulk(
      String requestId, String username, List<String> itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.OFF2ON_ACTIVATE_BULK, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSku),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleListStringResponse>>() {});
  }

  /**
   * Activate off2on channel by merchant code
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @return list of product failed to activate
   * @throws Exception
   */
  public GdnRestSingleResponse<SimpleListStringResponse> activateOff2OnChannelByMerchantCode(
      String requestId, String username, String merchantCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFF2ON_ACTIVATE_BY_MERCHANT_CODE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleListStringResponse>>() {});
  }

  public GdnBaseRestResponse addItem(String requestId, String username, String productSku,
      ItemDTO itemDTO) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_ADD, requestId, additionalParameterMap, username);
    return this.invokePostType(uri, itemDTO, ItemDTO.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addItemPrice(String requestId, String username,
      PriceRequest priceRequest, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_ADD_PRICE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, priceRequest, PriceRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addItemPriceByMerchantSkuAndMerchantCode(String requestId,
      String username, String merchantSku, String merchantCode, PriceRequest priceRequest)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_SKU_KEY, merchantSku);
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.ITEM_ADD_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE,
        requestId, additionalParameterMap, username);
    return this.invokePostType(uri, priceRequest, PriceRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addItemViewConfig(String requestId, String username, String productSku,
      ItemViewConfigRequest itemViewConfigRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, productSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_ADD_VIEW_CONFIG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, itemViewConfigRequest, ItemViewConfigRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addItemViewConfigByMerchantSkuAndMerchantCode(String requestId,
      String username, String merchantSku, String merchantCode,
      ItemViewConfigRequest itemViewConfigRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_SKU_KEY, merchantSku);
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_ADD_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE,
            requestId, additionalParameterMap, username);
    return this.invokePostType(uri, itemViewConfigRequest, ItemViewConfigRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addProduct(String requestId, String username,
      ProductDTO productDetailDTO) throws Exception {
    URI uri = this.generateURI(ProductApiPath.PRODUCT_ADD, requestId, new HashMap<String, String>(),
        username);
    return this.invokePostType(uri, productDetailDTO, ProductDTO.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<AddProductAndItemsResponse> addProductAndItems(String requestId,
      String username, ProductAndItemsRequest productAndItemRequestDTO) throws Exception {
    URI uri = this.generateURI(ProductApiPath.PRODUCT_ADD_PRODUCT_AND_ITEMS, requestId,
        new HashMap<String, String>(), username);

    return this.invokePostType(uri, productAndItemRequestDTO, ProductAndItemsRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<AddProductAndItemsResponse>>() {});
  }

  public GdnBaseRestResponse addProductSalesCategory(String requestId, String username,
      String catalogCode, List<String> productSkus, String newCategoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.CATALOG_CODE, catalogCode);
    additionalParameterMap.put(ProductClient.NEW_CATEGORY_CODE, newCategoryCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_ADD_SALES_CATEGORY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse alterSalesCategorySequence(String requestId, String username,
      String productSku, List<SalesCategorySequenceDTO> salesCategorySequence) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_ALTER_SALES_CATEGORY_SEQUENCE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, salesCategorySequence, List.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<HandlingFeeResponseRestWeb> calculateHandlingFee(String requestId,
      String username, List<HandlingFeeRequestRestWeb> request) throws Exception {
    URI uri = this.generateURI(ApiPath.HANDLING_FEE_CALCULATE_HANDLING_FEE, requestId,
        new HashMap<String, String>(), username);
    return this.invokePostType(uri, request, List.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<HandlingFeeResponseRestWeb>>() {});
  }

  public GdnRestSingleResponse<Level2InventoryCountResponseRestWeb> countLevel2Inventory(
      String requestId, String username, Integer stock, String level2MerchantCode)
      throws Exception {
    Map<String, String> map = new HashMap<String, String>();
    map.put(ProductClient.STOCK_KEY, stock.toString());
    map.put(ProductClient.LEVEL_2_MERCHANT_CODE_KEY, level2MerchantCode);

    URI uri = this.generateURI(ProductApiPath.COUNT_LEVEL2_INVENTORY, requestId, map, username);

    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<Level2InventoryCountResponseRestWeb>>() {});
  }

  public GdnBaseRestResponse deactivateOff2OnChannelActive(String requestId, String username,
      String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.OFF2ON_DEACTIVATE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<SimpleListStringResponse> deactivateOff2OnChannelActiveBulk(
      String requestId, String username, List<String> itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.OFF2ON_DEACTIVATE_BULK, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSku),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleListStringResponse>>() {});
  }

  /**
   * Activate off2on channel by merchant code
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @return list of product failed to activate
   * @throws Exception
   */
  public GdnRestSingleResponse<SimpleListStringResponse> deactivateOff2OnChannelByMerchantCode(
      String requestId, String username, String merchantCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFF2ON_DEACTIVATE_BY_MERCHANT_CODE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleListStringResponse>>() {});
  }

  public GdnBaseRestResponse deleteItem(String requestId, String username, String itemSku)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_DELETE, requestId, additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  @Deprecated
  public GdnBaseRestResponse toggleArchiveItem(String requestId, String username, String itemSku, Boolean doArchive)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    additionalParameterMap.put(ProductClient.DO_ARCHIVE_FLAG, doArchive.toString());
    URI uri =
        this.generateURI(ProductApiPath.ITEM_ARCHIVE, requestId, additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse toggleSuspensionProduct(String requestId, String username, String productSku, Boolean suspendProduct)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    additionalParameterMap.put(ProductClient.SUSPEND_PRODUCT, suspendProduct.toString());
    URI uri = this.generateURI(ProductApiPath.PRODUCT_SUSPEND, requestId, additionalParameterMap, username);
    return this
        .invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {
        });
  }

  public GdnBaseRestResponse deleteItemPriceByChannel(String requestId, String username,
      String itemSku, String channelName) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    additionalParameterMap.put(ProductClient.CHANNEL_KEY, channelName);
    URI uri = this.generateURI(ProductApiPath.ITEM_DELETE_PRICE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse deleteItemViewConfig(String requestId, String username, String itemSku,
      String channel) throws Exception {

    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    additionalParameterMap.put(ProductClient.CHANNEL_KEY, channel);
    URI uri = this.generateURI(ProductApiPath.ITEM_DELETE_VIEW_CONFIG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse deleteProduct(String requestId, String username, String productSku)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_DELETE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse deleteProductSalesCategory(String requestId, String username,
      String catalogCode, List<String> productSkus, String oldCategoryCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.CATALOG_CODE, catalogCode);
    additionalParameterMap.put(ProductClient.OLD_CATEGORY_CODE, oldCategoryCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_DELETE_SALES_CATEGORY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<SystemParameterResponse> findOne(String requestId, String variable,
      String username) throws Exception {
    URI uri = this.generateURI(ApiPath.SYSTEM_PARAMETER_FIND_ONE, requestId,
        new HashMap<String, String>(), username);
    return this.invokeGetSingle(uri, SystemParameterResponse.class, ClientConstants.JSON_TYPE);
  }

  private URI generateURI(String path, String requestId, Map<String, String> additionalParameterMap,
      String username) throws Exception {
    String location = this.getContextPath() + path;
    return this.getHttpClientHelper().getURI(this.getClientConfig().getHost(),
        this.getClientConfig().getPort(), location,
        this.getMandatoryParameter(this.getDefaultRequestIdValue(requestId), username),
        additionalParameterMap);
  }

  public GdnRestSingleResponse<SystemParameterResponse> getAllSettingOfHandlingFee(String requestId,
      String username) throws Exception {
    URI uri = this.generateURI(ApiPath.HANDLING_FEE_GET_SETTING_OF_HANDLING_FEE, requestId,
        new HashMap<String, String>(), username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SystemParameterResponse>>() {});
  }

  private String getDefaultRequestIdValue(String requestId) {
    if ((requestId == null) || (requestId.trim().length() == 0)) {
      return GdnUUIDHelper.generateUUID();
    }
    return requestId;
  }

  @Deprecated
  public GdnRestSingleResponse<ItemResponse> getItem(String requestId, String username,
      String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_GET, requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, ItemResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<SimpleMapStringResponse> getItemNameByItemSku(String requestId,
      String username, List<String> itemSkus) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_ITEM_NAME, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleMapStringResponse>>() {});
  }

  @Deprecated
  public GdnRestSingleResponse<ItemSummaryResponse> getItemSummaryByItemSku(String requestId,
      String username, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_SINGLE, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, ItemSummaryResponse.class, ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestSingleResponse<ItemSummaryResponse> getArchivedItemSummaryByItemSku(String requestId,
      String username, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri =
        this.generateURI(ProductApiPath.ARCHIVED_ITEM_SUMMARY_SINGLE, requestId, additionalParameterMap,
            username);
    return this.invokeGetSingle(uri, ItemSummaryResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<SimpleProductListResponse> getListOfAllProductSku(String requestId,
      String username) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_ALL_PRODUCT_LIST_SKU, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, SimpleProductListResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<SimpleListStringResponse> getListOfChannel(String requestId,
      String username) throws Exception {
    URI uri = this.generateURI(ProductApiPath.CHANNEL_LIST, requestId,
        new HashMap<String, String>(), username);
    return this.invokeGetSingle(uri, SimpleListStringResponse.class, ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByFilter(String requestId,
      String username, ItemSummaryRequest request, int page, int size, String orderBy, String sortBy) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    additionalParameter.put(ClientConstants.ORDER_BY, orderBy);
    additionalParameter.put(ClientConstants.SORT_BY, sortBy);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_SUMMARY_FILTER, requestId, additionalParameter,
            username);
    return this.invokePostType(uri, request, ItemSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ItemSummaryResponse>>() {});
  }

  public GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByArchivedFilter(String requestId,
      String username, ItemSummaryRequest request, int page, int size) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    URI uri =
        this.generateURI(ProductApiPath.ITEM_SUMMARY_ARCHIVED_FILTER, requestId, additionalParameter,
            username);
    return this.invokePostType(uri, request, ItemSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ItemSummaryResponse>>() {});
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByListOfSimpleProductRequest(
      String requestId, String username, SimpleProductListRequest simpleProductRequestList)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_PRODUCT_BY_SIMPLE_REQUEST, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, simpleProductRequestList, SimpleProductListRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>>() {});
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductCatentryIds(
      String requestId, String username, List<String> productCatentryIds,
      boolean needMasterDataDetail) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productCatentryIds),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>>() {});
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductCodes(
      String requestId, String username, List<String> productCodes, boolean needMasterDataDetail)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_BY_PRODUCT_CODES, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productCodes),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>>() {});
  }

  @Deprecated
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductByProductSkus(
      String requestId, String username, List<String> productSkus, boolean needMasterDataDetail)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_BY_PRODUCT_SKUS, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>>() {});
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getListOfProductSortedByProductCode(
      String requestId, String username, int page, int size) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    URI uri = this.generateURI(ProductApiPath.GET_ALL_PRODUCT_LIST, requestId, additionalParameter,
        username);
    return this.invokeGetSingle(uri, MasterDataDetailWithProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);
  }


  public GdnRestListResponse<ProductSummaryResponse> getListOfProductSummaryBySalesCatalog(
      String requestId, String username, String catalogCode, String categoryCode, int page,
      int size) throws Exception {
    Map<String, String> additionalParameter = new LinkedHashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    additionalParameter.put(ProductClient.CATALOG_CODE, catalogCode);
    additionalParameter.put(ProductClient.CATEGORY_CODE_KEY, categoryCode);
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_BY_SALES_CATALOG, requestId,
        additionalParameter, username);
    return this.invokeGetSummary(uri, ProductSummaryResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<SimpleItemResponse> getOfferPriceAndBuyableStatusByItemSku(
      String requestId, String username, String channelName, List<String> itemSkus)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.CHANNEL_KEY, channelName);
    URI uri = this.generateURI(ProductApiPath.GET_PRICE_BY_ITEM_SKU, requestId, additionalParameter,
        username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimpleItemResponse>>() {});
  }

  public GdnRestListResponse<SimpleItemResponse> getBuyableAndOfferPriceByOnlineAndOfflineItemSku(
      String requestId, String username, String channelName, List<ItemSkuDTO> itemSkuDTOList)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.CHANNEL_KEY, channelName);
    URI uri = this.generateURI(ProductApiPath.FILTER_PRICE_BY_ONLINE_AND_OFFLINE_ITEM_SKU, requestId, additionalParameter,
        username);
    return this.invokePostType(uri, itemSkuDTOList,
        List.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimpleItemResponse>>() {});
  }

  public GdnRestListResponse<Off2OnPriceResponse> getPriceAndOff2OnChannelActiveByItemSku(
      String requestId, String username, String channelName, List<String> itemSkus)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.CHANNEL_KEY, channelName);
    URI uri = this.generateURI(ProductApiPath.OFF2ON_GET_OFF2ON_PRICE, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<Off2OnPriceResponse>>() {});
  }

  public GdnRestSingleResponse<ProductAndItemsAvailabilityResponse> getProductAndItemsAvailability(
      String requestId, String username, List<String> productSkus) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_AVAILABILITY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ProductAndItemsAvailabilityResponse>>() {});
  }

  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsByProductCatentryId(
      String requestId, String username, String productCatentryId) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CATENTRY_ID_KEY, productCatentryId);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID,
        requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, ProductAndItemsResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getProductAndItemsByProductCode(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE,
        requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, MasterDataDetailWithProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsByProductSku(
      String requestId, String username, String productSku) throws Exception {
    return this.getProductAndItemsByProductSku(requestId, username, productSku, false);
  }

  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsByProductSku(
      String requestId, String username, String productSku, boolean showDeleted) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    additionalParameterMap.put(ProductClient.SHOW_DELETED, String.valueOf(showDeleted));
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, ProductAndItemsResponse.class, ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(
      String requestId, String username, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_SINGLE_ITEM, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, ProductAndItemsResponse.class, ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductDetailAndSingleItemByItemSku(String requestId,
      String username, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, ProductAndItemsResponse.class, ClientConstants.JSON_TYPE);
  }

  @Deprecated
  public GdnRestSingleResponse<ProductAndItemsResponse> getProductAndSingleItemByItemSku(
      String requestId, String username, String itemSku, boolean instantPickup,
      String pickupPointCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    additionalParameterMap.put(ProductClient.INSTANT_PICKUP_KEY, String.valueOf(instantPickup));
    additionalParameterMap.put(ProductClient.PICKUP_POINT_CODE_KEY, pickupPointCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_SINGLE_ITEM, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, ProductAndItemsResponse.class, ClientConstants.JSON_TYPE);
  }


  @Deprecated
  public GdnRestListResponse<ProductForTransactionResponse> getProductForTransactionByItemSkus(
      String requestId, String username, List<String> itemSkus) throws Exception {
    Map<String, String> map = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_BY_ITEM_SKU, requestId, map, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ProductForTransactionResponse>>() {});
  }

  public GdnRestListResponse<SimpleProductResponse> getSimpleProductByItemSkus(String requestId,
      String username, List<String> itemSkus) throws Exception {
    Map<String, String> map = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_ITEM_SKUS_CACHED,
            requestId, map, username);
    return this.invokePostType(uri, new SimpleListStringRequest(itemSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimpleProductResponse>>() {});
  }

  public GdnRestSingleResponse<SimpleMapStringResponse> getProductNameByProductSku(String requestId,
      String username, List<String> productSkus) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_PRODUCT_NAME, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleMapStringResponse>>() {});
  }

  public GdnRestSingleResponse<SimpleMapStringResponse> getProductNamesByProductCodes(
      List<String> productCodes, String requestId, String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_NAME_BY_CODE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productCodes),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleMapStringResponse>>() {});
  }

  public GdnRestListResponse<ProductsToItemCatalogMappingResponse> getProductsWithoutSalesCatalog(
      String storeId, String requestId, int size, int month, int year, String username)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("size", Integer.toString(size));
    additionalParameterMap.put("month", Integer.toString(month));
    additionalParameterMap.put("year", Integer.toString(year));
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_WITHOUT_SALES_CATALOG, requestId,
        additionalParameterMap, username);
    return this.invokeGetSummary(uri, ProductsToItemCatalogMappingResponse.class,
        ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<ProductsToItemCatalogMappingResponse> getProductsWithoutSalesCatalogDayRange(
      String storeId, String requestId, int size, int fromDay, int toDay, int month, int year,
      String username) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put("size", Integer.toString(size));
    additionalParameterMap.put("fromDay", Integer.toString(fromDay));
    additionalParameterMap.put("toDay", Integer.toString(toDay));
    additionalParameterMap.put("month", Integer.toString(month));
    additionalParameterMap.put("year", Integer.toString(year));
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE,
        requestId, additionalParameterMap, username);
    return this.invokeGetSummary(uri, ProductsToItemCatalogMappingResponse.class,
        ClientConstants.JSON_TYPE);
  }


  public GdnRestSingleResponse<SimpleListStringResponse> getShippingType(String requestId,
      String username, GetShippingTypeRequest getShippingTypeRequest) throws Exception {
    URI uri = this.generateURI(ProductApiPath.GET_SHIPPING_TYPE, requestId,
        new HashMap<String, String>(), username);
    return this.invokePostType(uri, getShippingTypeRequest, GetShippingTypeRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SimpleListStringResponse>>() {});
  }

  public GdnRestSingleResponse<Level1Level2ListInventoryResponseRestWeb> insertInventoryLevel2(
      String requestId, String username, Level2InventoryListOfLevel2InventoryRequestRestWeb request)
      throws Exception {
    URI uri = this.generateURI(ProductApiPath.INSERT_INVENTORY_LEVEL2, requestId,
        new HashMap<String, String>(), username);
    return this.invokePostType(uri, request,
        Level2InventoryListOfLevel2InventoryRequestRestWeb.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<Level1Level2ListInventoryResponseRestWeb>>() {});
  }

  public GdnRestSingleResponse<SimpleBooleanResponse> isPickupPointCodeUsed(String requestId,
      String username, String pickupPointCode) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put("pickupPointCode", pickupPointCode);
    URI uri = this.generateURI(ProductApiPath.ITEM_IS_PICKUP_POINT_CODE_USED, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, SimpleBooleanResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnBaseRestResponse moveProductSalesCategory(String requestId, String username,
      String catalogCode, List<String> productSkus, String oldCategoryCode, String newCategoryCode)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.CATALOG_CODE, catalogCode);
    additionalParameterMap.put(ProductClient.OLD_CATEGORY_CODE, oldCategoryCode);
    additionalParameterMap.put(ProductClient.NEW_CATEGORY_CODE, newCategoryCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_MOVE_SALES_CATEGORY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, new SimpleListStringRequest(productSkus),
        SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<ProductAndItemsResponse> synchronizeProduct(String requestId,
      String username, String productSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_SYNCHRONIZE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ProductAndItemsResponse>>() {});
  }

  public GdnRestSingleResponse<ProductAndItemsResponse> unsynchronizeProduct(String requestId,
      String username, String productSku, boolean overwriteExistingMasterData) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    additionalParameterMap.put(ProductClient.OVERWRITE_EXISTING_MASTER_DATA_KEY,
        String.valueOf(overwriteExistingMasterData));
    URI uri = this.generateURI(ProductApiPath.PRODUCT_UNSYNCHRONIZE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ProductAndItemsResponse>>() {});
  }

  public GdnRestSingleResponse<SystemParameterResponse> updateAllSettingOfHandlingFee(
      String requestId, String username, SystemParameterRequest request) throws Exception {
    URI uri = this.generateURI(ApiPath.HANDLING_FEE_UPDATE_SETTING_OF_HANDLING_FEE, requestId,
        new HashMap<String, String>(), username);
    return this.invokePostType(uri, request, SystemParameterRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<SystemParameterResponse>>() {});
  }

  public GdnBaseRestResponse updateBuyableDefault(String requestId, String username, String itemSku,
      ItemBuyableRequest buyableRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_BUYABLE_DEFAULT, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, buyableRequest, ItemBuyableRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateBuyableSchedule(String requestId, String username,
      String itemSku, ItemBuyableScheduleRequest buyableScheduleRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_BUYABLE_SCHEDULE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, buyableScheduleRequest, ItemBuyableScheduleRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateDiscoverableDefault(String requestId, String username,
      String itemSku, ItemDiscoverableRequest discoverableRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_DEFAULT,
        requestId, additionalParameterMap, username);
    return this.invokePostType(uri, discoverableRequest, ItemDiscoverableRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateDiscoverableSchedule(String requestId, String username,
      String itemSku, ItemDiscoverableScheduleRequest discoverableScheduleRequest)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_VIEW_CONFIG_UPDATE_DISCOVERABLE_SCHEDULE,
        requestId, additionalParameterMap, username);
    return this.invokePostType(uri, discoverableScheduleRequest,
        ItemDiscoverableScheduleRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  @Deprecated
  public GdnRestSingleResponse<ItemResponse> updateItem(String requestId, String username, ItemRequest itemRequest)
      throws Exception {
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE, requestId, new HashMap<String, String>(), username);
    return this.invokePostType(uri, itemRequest, ItemRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnRestSingleResponse<ItemResponse>>() {});
  }

  @Deprecated
  public GdnBaseRestResponse updateItemPrice(String requestId, String username,
      PriceRequest priceRequest, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_PRICE, requestId, additionalParameterMap,
        username);
    return this.invokePostType(uri, priceRequest, PriceRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateItemPriceByMerchantSkuAndMerchantCode(String requestId,
      String username, String merchantSku, String merchantCode, PriceRequest priceRequest)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_SKU_KEY, merchantSku);
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_PRICE_BY_MERCHANT_SKU_AND_MERCHANT_CODE,
        requestId, additionalParameterMap, username);
    return this.invokePostType(uri, priceRequest, PriceRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  @Deprecated
  public GdnRestSingleResponse<ItemSummaryResponse> updateItemSummary(String requestId,
      String username, String itemSku, String merchantCode,
      UpdateItemSummaryRequest updateItemSummaryRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_UPDATE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, updateItemSummaryRequest, UpdateItemSummaryRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ItemSummaryResponse>>() {});
  }

  @Deprecated
  public GdnBaseRestResponse updateItemViewConfig(String requestId, String username, String itemSku,
      ItemViewConfigRequest itemViewConfigRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_VIEW_CONFIG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, itemViewConfigRequest, ItemViewConfigRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateItemViewConfigByMerchantSkuAndMerchantCode(String requestId,
      String username, String merchantSku, String merchantCode,
      ItemViewConfigRequest itemViewConfigRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_SKU_KEY, merchantSku);
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri =
        this.generateURI(ProductApiPath.ITEM_UPDATE_VIEW_CONFIG_BY_MERCHANT_SKU_AND_MERCHANT_CODE,
            requestId, additionalParameterMap, username);
    return this.invokePostType(uri, itemViewConfigRequest, ItemViewConfigRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateResignMerchantItemsByMerchantCode(String requestId,
      String username, String merchantCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_RESIGN_MERCHANT_ITEMS_BY_MERCHANT_CODE,
        requestId, additionalParameterMap, username);
    return this.invokePostType(uri, null, Object.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  @Deprecated
  public GdnRestSingleResponse<ProductResponse> updateProduct(String requestId, String username,
      ProductRequest productRequest) throws Exception {
    URI uri = this.generateURI(ProductApiPath.PRODUCT_UPDATE, requestId, new HashMap<String, String>(), username);
    return this.invokePostType(uri, productRequest, ProductRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ProductResponse>>() {});
  }

  @Deprecated
  public GdnBaseRestResponse updateProductSalesCatalog(String requestId, String username,
      SalesCatalogRequest salesCatalogRequest, String productSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_SKU_KEY, productSku);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_UPDATE_SALES_CATALOG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, salesCatalogRequest, SalesCatalogRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateProductSalesCatalog(String requestId, String username,
      SalesCatalogRequest salesCatalogRequest, String productCode, boolean replace)
      throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    additionalParameterMap.put(ProductClient.REPLACE_SKU_KEY, String.valueOf(replace));
    URI uri = this.generateURI(ProductApiPath.PRODUCT_UPDATE_SALES_CATALOG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, salesCatalogRequest, SalesCatalogRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse updateProductMasterCatalog(String requestId, String username,
      MasterCatalogRequest masterCatalogRequest, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_UPDATE_MASTER_CATALOG, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, masterCatalogRequest, MasterCatalogRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnBaseRestResponse addProductAttribute(String requestId, String username,
      ProductAttributeRequest productAttributeRequest, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_ADD_PRODUCT_ATTRIBUTE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, productAttributeRequest, ProductAttributeRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestSingleResponse<SimpleLongResponse> getProductsCountByBrand(String requestId,
      String username, String brand) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.BRAND, brand);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_COUNT_BY_BRAND, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, SimpleLongResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByCategoryAndBrandFilter(
      String requestId, String username, CampaignItemSummaryRequest request, int page, int size,
      String orderBy, String sortBy) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    additionalParameter.put(ClientConstants.ORDER_BY, orderBy);
    additionalParameter.put(ClientConstants.SORT_BY, sortBy);
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_CATEGORY_AND_BRAND_FILTER, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, request, CampaignItemSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ItemSummaryResponse>>() {
        });
  }

  public GdnRestListResponse<SimpleProductsAndItemsResponse> getProductAndItemsByProductWrapper(
      String requestId, String username, ProductWrapper productWrapper) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_PRODUCT_BY_PRODUCT_WRAPPER, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, productWrapper, ProductWrapper.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimpleProductsAndItemsResponse>>() {
        });
  }

  public GdnBaseRestResponse updateItemPristineData(String requestId,
      String username, List<PristineDataItemDto> pristineDataItemDto) throws Exception {
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_PRISTINE_DATA, requestId,
        new HashMap<String, String>(), username);

    return this.invokePostType(uri, pristineDataItemDto, List.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestListResponse<SimplePristineProductResponse> getAllProductCodesAndSkusByPristineIds(
      String requestId, String username, Set<String> pristineIds) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_LIST_BY_PRISTINE_IDS, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, pristineIds, Set.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimplePristineProductResponse>>() {
        });
  }

  public GdnRestSingleResponse<DefaultItemSkuResponse> getDefaultItemSkuByPristineId(
      String username, String requestId, String pristineId) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.PRISTINE_ID, pristineId);
    URI uri = this.generateURI(ProductApiPath.GET_DEFAULT_ITEM_SKU_RESPONSE, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, DefaultItemSkuResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<PristineMasterDataResponse> getPristineMasterIdByPristineIdOrProductCodeOrSku(
      String username, String requestId, String pristineIdOrProductCodeOrSku) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.PRISTINE_ID_OR_PRODUCT_CODE_OR_SKU, pristineIdOrProductCodeOrSku);
    URI uri = this.generateURI(ProductApiPath.GET_PRISTINE_MASTER_DATA_RESPONSE, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, PristineMasterDataResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>
  getProductAndItemsByPristineId(
      String requestId, String username, String pristineId, String defaultSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRISTINE_ID, pristineId);
    additionalParameterMap.put(ProductClient.DEFAULT_SKU, defaultSku);
    URI uri =
        this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRISTINEID, requestId,
            additionalParameterMap, username);
    return this.invokeGetSingle(uri, MasterDataDetailWithProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);

  }

  public GdnRestSingleResponse<PristineProductAndItemsResponse>
  getProductAndItemsResponseByPristineId(
      String requestId, String username, String id) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ID, id);
    URI uri = this.generateURI(
        ProductApiPath.PRODUCT_GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID,
        requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, PristineProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<PristineMasterDataDetailResponse>
  getPristineProductAndItemsInfoByItemSku(
      String requestId, String username, String itemSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(
        ProductApiPath.PRISTINE_PRODUCT_GET_PRODUCT_AND_ITEMS_BY_ITEM_SKU, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, PristineMasterDataDetailResponse.class,
        ClientConstants.JSON_TYPE);
  }


  @Deprecated
  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse>
  getProductAndItemsByProductCodeOrPristineId(
      String requestId, String username, String id, String defaultSku) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ID, id);
    additionalParameterMap.put(ProductClient.DEFAULT_SKU, defaultSku);
    URI uri = this.generateURI(
        ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID, requestId,
        additionalParameterMap, username);
    return this.invokeGetSingle(uri, MasterDataDetailWithProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<PristineCategoryAttributeMapResponse>
  getMapForPristineCategoryAttribute(
      String requestId, String username) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_MAP_FOR_PRISTINE_CATEGORY_ATTRIBUTE,
        requestId, additionalParameter, username);
    return this.invokeGetSingle(uri, PristineCategoryAttributeMapResponse.class,
        ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<SimpleProductMasterDataDetailResponse>
  getProductMasterDataDetailByProductCodesAndSkus(
      String requestId, String username, ProductIdentifierWrapper productIdentifierWrapper)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.GET_PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS,
            requestId, additionalParameter, username);
    return this.invokePostType(uri, productIdentifierWrapper, ProductIdentifierWrapper.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<SimpleProductMasterDataDetailResponse>>() {
        });
  }


  public GdnRestSingleResponse<OfflineItemResponse> getOfflineItemsByMerchantCodeAndMerchantSkus(
      String requestId, String username, String merchantCode, List<String> merchantSkus)
      throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_BY_MERCHANT_CODE_AND_MERCHANT_SKUS, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, new SimpleListStringRequest(merchantSkus), SimpleListStringRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<OfflineItemResponse>>() {
        });
  }

  public GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItem(String requestId,
      String username, String merchantCode,
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests) throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_UPSERT_OFFLINE_ITEM, requestId,
        additionalParameter, username);
    return this
        .invokePostType(uri, upsertOfflineItemRequests, List.class, ClientConstants.JSON_TYPE,
            new TypeReference<GdnRestListResponse<UpsertOfflineItemPriceResponse>>() {
            });
  }

  public GdnRestListResponse<UpsertOfflineItemPriceResponse> upsertOfflineItemPrice(
      String requestId, String username, String merchantCode, List<OfflineItemRequest> offlineItemRequest)
      throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_UPSERT_OFFLINE_ITEM_PRICE, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, offlineItemRequest, List.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<UpsertOfflineItemPriceResponse>>() {
        });
  }

  public GdnBaseRestResponse deleteOfflineItem(String requestId, String username,
      String merchantCode, List<DeleteOfflineItemRequest> deleteOfflineItemRequests)
      throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM + ProductApiPath.DELETE_OFFLINE_ITEM, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, deleteOfflineItemRequests, List.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestListResponse<OfflineItemPriceResponse> findOfflinePriceByMerchantCodeAndItemSku(
      String requestId, String username, String merchantCode, String itemSku)
      throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    additionalParameter.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_BY_MERCHANT_CODE_AND_ITEM_SKU, requestId,
        additionalParameter, username);
    return this.invokeGetSummary(uri, OfflineItemPriceResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<ProductAndItemsResponse> findProductAndOfflineItemsByProductSkus(
      String requestId, String username, List<String> productSkus) throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();

    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_BY_PRODUCT_SKUS, requestId,
        additionalParameter, username);

    return this.invokePostType(uri, productSkus, List.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ProductAndItemsResponse>>() {
        });
  }

  public GdnRestSingleResponse<OfferedComboSummaryResponse> getOfferedSummary(
      String requestId, String username, OfferedSummaryRequest offeredSummaryRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.OFFERED + ProductApiPath.OFFERED_SUMMARY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, offeredSummaryRequest, OfferedSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<OfferedComboSummaryResponse>>() {
        });
  }

  public GdnRestSingleResponse<OfferedComboSummaryResponse> getOfferedComboSummary(
      String requestId, String username, OfferedSummaryRequest offeredSummaryRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.OFFERED + ProductApiPath.OFFERED_COMBO_SUMMARY, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, offeredSummaryRequest, OfferedSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<OfferedComboSummaryResponse>>() {
        });
  }

  public GdnRestListResponse<ComboResponse> getActiveCombos(String requestId, String username,
      ActiveComboRequest request, int page, int size, String sortBy, String sortType) throws Exception{
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    additionalParameter.put(ClientConstants.SORT_BY, sortBy);
    additionalParameter.put(ClientConstants.SORT_TYPE, sortType);
    URI uri = this.generateURI(ProductApiPath.GET_ACTIVE_COMBOS, requestId, additionalParameter, username);
    return this.invokePostType(uri, request, ActiveComboRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ComboResponse>>() {
        });
  }

  public GdnRestSingleResponse<WholesaleResponse> getWholesaleDetail(String requestId, String username,
      String itemSku) throws Exception{
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put("itemSku", itemSku);
    URI uri = this.generateURI(ProductApiPath.GET_WHOLESALE_DETAIL, requestId, additionalParameter, username);
    return this.invokeGetSingle(uri, WholesaleResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<ItemAndBundlingInfoResponse> getItemsAndBundlingInfo(
      String requestId, String username, ItemAndBundlingInfoRequest itemAndBundlingInfoRequest)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri =
        this.generateURI(ProductApiPath.GET_ITEMS_AND_BUNDLING_INFO, requestId, additionalParameter,
            username);
    return this.invokePostType(uri, itemAndBundlingInfoRequest, ItemAndBundlingInfoRequest.class,
        ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<ItemAndBundlingInfoResponse>>() {
        });
  }

  @Deprecated
  public GdnRestListResponse<ItemPriceResponse> getItemSkusByItemCode(String requestId,
      String username, String itemCode) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.ITEM_CODE, itemCode);
    URI uri =
        this.generateURI(ProductApiPath.GET_ITEM_SKUS_CLIENT_PATH, requestId, additionalParameter, username);
    return this.invokeGetSummary(uri, ItemPriceResponse.class, ClientConstants.JSON_TYPE, null);
  }

  public GdnRestListResponse<OfflineItemPriceResponse> findAllOfflineItemsOfMultipleMerchants(
      String requestId, String username, String itemSku) throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_ALL_OFFLINE_ITEMS_OF_MULTIPLE_MERCHANTS, requestId,
        additionalParameter, username);
    return this.invokeGetSummary(uri, OfflineItemPriceResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<OfflineItemPriceResponse> findAllOfflineItemsOfSpecificMerchant(
      String requestId, String username, String itemSku) throws Exception {

    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_ALL_OFFLINE_ITEMS_OF_SPECIFIC_MERCHANT, requestId,
        additionalParameter, username);
    return this.invokeGetSummary(uri, OfflineItemPriceResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<OfflineItemPriceResponse> findOfflinePriceByOfflineItemId(
      String requestId, String username, String offlineItemId) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.OFFLINE_ITEM_ID, offlineItemId);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_FIND_BY_OFFLINE_ITEM_ID, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, OfflineItemPriceResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<ItemTypeResponse> checkUniqueIdType(
      String requestId, String username, String uniqueId) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.UNIQUE_ID, uniqueId);
    URI uri = this.generateURI(ProductApiPath.OFFLINE_ITEM_CHECK_UNIQUE_ID_TYPE, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, ItemTypeResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<ReviewProductDetailResponse> getProductDetailForReview(
      String requestId, String username, String id) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.ID, id);
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_DETAIL_RESPONSE_FOR_REVIEW, requestId,
        additionalParameter, username);
    return this.invokeGetSingle(uri, ReviewProductDetailResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestSingleResponse<DefaultItemSkuResponse> getFirstBuyableDiscoverableItemSku(
      String requestId, String username, Set<String> itemSkus) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.GET_FIRST_BUYABLE_DISCOVERABLE_ITEM_SKU_BY_PRISTINE_ID_PATH, requestId,
        additionalParameter, username);
    return this.invokePostType(uri, itemSkus, Set.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestSingleResponse<DefaultItemSkuResponse>>(){});
  }

  public GdnBaseRestResponse updatePromotionPriceForSkuList(String requestId, String username, List<String> itemSkus)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    URI uri = this.generateURI(ProductApiPath.ITEM + ProductApiPath.UPDATE_PROMOTION_PRICE_FOR_SKU_LIST, requestId,
        additionalParameter, username);
    return this
        .invokePostType(uri, itemSkus, List.class, ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {
        });
  }

  public GdnBaseRestResponse updateOfflineItemPriceByItemSku(String requestId, String username,
      String merchantCode, UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest)
      throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(
        ProductApiPath.OFFLINE_ITEM + ProductApiPath.UPDATE_OFFLINE_ITEM_PRICE_BY_ITEM_SKU,
        requestId, additionalParameter, username);
    return this.invokePostType(uri, updateOfflineItemPriceRequest,
        UpdateOfflineItemPriceRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnBaseRestResponse>() {});
  }

  public GdnRestListResponse<ActiveProductResponse> getAllProducts(String requestId, String username, int page,
      int size, ActiveProductRequest activeProductRequest) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    URI uri = this.generateURI(ProductApiPath.GET_ALL_PRODUCTS, requestId, additionalParameter, username);
    return this.invokePostType(uri, activeProductRequest, ActiveProductRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ActiveProductResponse>>() {
        });
  }

  @Deprecated
  public GdnRestListResponse<ItemSummaryResponse> getSuspendedItemList(String requestId, String username, int page,
      int size, ActiveProductRequest activeProductRequest) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    URI uri = this.generateURI(ProductApiPath.GET_SUSPENDED_PRODUCTS, requestId, additionalParameter, username);
    return this.invokePostType(uri, activeProductRequest, ActiveProductRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ItemSummaryResponse>>() {
        });
  }

  public GdnRestListResponse<ProductResponse> getProductsByProductCodeAndMerchantCode(
      String requestId, String username, String productCode, String merchantCode) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    additionalParameter.put(ProductClient.MERCHANT_CODE_KEY, merchantCode);
    URI uri = this.generateURI(ProductApiPath.GET_PRODUCT_RESPONSE_BY_PRODUCT_CODE_AND_MERCHANT_CODE, requestId,
        additionalParameter, username);
    return this.invokeGetSummary(uri, ProductResponse.class, ClientConstants.JSON_TYPE);
  }

  public GdnRestListResponse<ItemSummaryResponse> getListOfItemSummaryByFilter(String requestId,
      String username, ItemSummaryRequest request, int page, int size) throws Exception {
    Map<String, String> additionalParameter = new HashMap<String, String>();
    additionalParameter.put(ClientConstants.PAGE, String.valueOf(page));
    additionalParameter.put(ClientConstants.SIZE, String.valueOf(size));
    URI uri = this.generateURI(ProductApiPath.ITEM_SUMMARY_FILTER, requestId, additionalParameter,
        username);
    return this.invokePostType(uri, request, ItemSummaryRequest.class, ClientConstants.JSON_TYPE,
        new TypeReference<GdnRestListResponse<ItemSummaryResponse>>() {});
  }

  public GdnRestSingleResponse<MasterDataDetailWithProductAndItemsResponse> getProductAndItemsSyncByProductCode(
      String requestId, String username, String productCode) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.PRODUCT_CODE_KEY, productCode);
    URI uri = this.generateURI(ProductApiPath.PRODUCT_GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE,
        requestId, additionalParameterMap, username);
    return this.invokeGetSingle(uri, MasterDataDetailWithProductAndItemsResponse.class,
        ClientConstants.JSON_TYPE);
  }

  public GdnBaseRestResponse updateItemEtdNote(String requestId, String username, String itemSku,
      SimpleRequestHolder etdNoteRequest) throws Exception {
    Map<String, String> additionalParameterMap = new HashMap<String, String>();
    additionalParameterMap.put(ProductClient.ITEM_SKU_KEY, itemSku);
    URI uri = this.generateURI(ProductApiPath.ITEM_UPDATE_ETD_NOTE, requestId,
        additionalParameterMap, username);
    return this.invokePostType(uri, etdNoteRequest, ItemViewConfigRequest.class,
        ClientConstants.JSON_TYPE, new TypeReference<GdnBaseRestResponse>() {});
  }
}
