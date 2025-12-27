package com.gdn.x.product.service.impl;


import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.google.common.base.Predicates.not;
import static java.util.stream.Collectors.toMap;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.api.ItemRepositoryCustom;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemsSummaryDetailRequestVo;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductDataResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemSummaryService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ItemSummaryUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.google.common.base.Functions;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
@Service
public class ItemSummaryServiceImpl implements ItemSummaryService {
  private static final String NOT_FOUND_PRODUCT_WITH_FILTER = "not found product with filter = ";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "store id must not be blank";

  private static final String SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL =
      "set of search param must not be null";

  private static final String ITEMS_NOT_FOUND =
      "No matching item data found for the requested parameters";

  private static final Logger LOGGER = LoggerFactory.getLogger(ItemSummaryServiceImpl.class);
  public static final String DESCRIPTIVE_ATTRIBUTE = "DESCRIPTIVE_ATTRIBUTE";
  public static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";

  @Autowired
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Autowired
  private ItemPriceService itemPriceService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private ObjectConverterService objectConverter;

  @Autowired
  private ProductAndItemSolrConstructorService solrDataConstructor;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private CachedService cachedService;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private ItemViewConfigService itemViewConfigService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ItemSummaryUtil itemSummaryUtil;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  @Qualifier("itemRepositoryImpl")
  private ItemRepositoryCustom itemRepositoryCustom;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private GdnMapper gdnMapper;

  @Value("${solr.string.delimiter:#_#}")
  private String solrStringDelimiter;

  @Value("${product.max.shipping.weight:50}")
  private double maxShippingWeight;

  @Value("${item.sku.list.size}")
  private int itemSkuListSize;

  @Value("${get.item.basic.detail.api.item.sku.list.size}")
  private int getItemBasicDetailApiItemSkuListSize;

  @Value("${product.sku.list.size}")
  private int productSkuListSize;

  @Value("${pcb.get.main.image.data.disabled}")
  private boolean pcbGetMainImageDataDisabled;

  @Value("${populate.item.attribute.detail}")
  private boolean populateItemAttributeDetail;

  @Value("#{'${halal.attributes.values}'.split(',')}")
  private List<String> halalAttributesValues;

  @Value("#{'${storage.attributes.values}'.split(',')}")
  private List<String> storageAttributesValues;

  @Value("${override.latefulfillment.by.product.type}")
  private boolean overrideLateFulfillmentByProductType;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${fetch.pcb.master.data}")
  private boolean fetchPcbMasterData;

  private ItemSummaryResponseVO constructItemResponseVoAfterUpdate(String storeId, String username,
      String requestId, String itemSku, ProductAndItemsVO productAndItems) {
    ItemSummaryResponseVO responseVo =
        this.getItemSummaryByFilter(storeId, username, requestId, new ItemSummaryRequestVO(Arrays.asList(itemSku)),
            null, null, PageRequest.of(0, 1)).getItemSummaryResponses().get(0);
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      Item item = productAndItems.getItems().get(0);
      ItemPickupPoint itemPickupPoint = productAndItems.getItemPickupPoints().get(0);
      responseVo.setIsLateFulfillment(item.isLateFulfillment());
      responseVo.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
      responseVo.setPrice(itemPickupPoint.getPrice());
      responseVo.setPickupPointCode(item.getPickupPointCode());
      responseVo.setMerchantSku(item.getMerchantSku());
      responseVo.setCreatedDate(item.getCreatedDate());
      responseVo.setUpdatedDate(item.getUpdatedDate());
      responseVo.setArchived(item.isArchived());
      responseVo.setMarkForDelete(item.isMarkForDelete());
      responseVo.setVersion(item.getVersion());
      responseVo.setOff2OnChannelActive(item.isOff2OnChannelActive());
    }
    if (productAndItems.getProduct() != null) {
      responseVo.setProductType(productAndItems.getProduct().getProductType());
    }
    return responseVo;
  }

  private List<String> constructItemSkus(List<ProductAndItemSolr> solrResult) {
    List<String> itemSkus = new ArrayList<String>();
    for (ProductAndItemSolr productAndItem : solrResult) {
      itemSkus.add(productAndItem.getItemSku());
    }
    return itemSkus;
  }

  private ItemSummaryPageResponseVo constructItemSummaryResponse(
      Page<ProductAndItemSolr> resultFromSolr, Map<String, Set<Price>> mapOfPrices,
      String pickupPointCodeFilter, Map<String, Item> itemAndItemViewConfigMap, Map<String, Double> mapOfOriginalPrice,
      Map<String, Boolean> preOrderDetails) {
    return this
        .constructItemSummaryResponse(resultFromSolr, mapOfPrices, pickupPointCodeFilter, null,
            itemAndItemViewConfigMap, mapOfOriginalPrice, preOrderDetails, new HashMap<>());
  }

  private ItemSummaryPageResponseVo constructItemSummaryResponseFromL5(Page<ProductAndItemSolr> resultFromSolr,
      Map<String, Set<Price>> mapOfPrices, String pickupPointCodeFilter, List<String> pickupPointCodesFilter,
      Map<String, ItemPickupPoint> itemPickupPointMap, Map<String, Double> mapOfOriginalPrice,
      Map<String, PreOrder> preOrderDetails, Map<String, Item> itemMap) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<ItemSummaryResponseVO>();
    for (ProductAndItemSolr productAndItem : resultFromSolr.getContent()) {
      try {
        productAndItem.setPickupPointCode(Optional.ofNullable(
                Optional.ofNullable(itemMap.get(productAndItem.getItemSku())).orElse(new Item()).getPickupPointCode())
            .orElse(productAndItem.getPickupPointCode()));
        ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(productAndItem.getItemSku());
        if(Objects.isNull(itemPickupPoint)) {
          ItemSummaryServiceImpl.LOGGER.warn("Skipping generating item summary response. ItemSku "
              + ": {} ",
            productAndItem.getItemSku());
          continue;
        }
        ItemSummaryResponseVO itemSummaryResponse =
            CommonUtil.constructItemSummaryResponse(mapOfPrices, mapOfOriginalPrice, preOrderDetails, itemMap,
                productAndItem, itemPickupPoint);
        itemSummaryResponse.setMasterDataItemImages(
            this.solrDataConstructor.constructMasterDataItemMainImages(productAndItem.getItemImages()));
        itemSummaryResponse.setMasterCatalog(
            this.solrDataConstructor.constructMasterCatalog(productAndItem.getMasterCatalog()));
        itemSummaryResponse.setSalesCatalogs(
            this.solrDataConstructor.constructSalesCatalogs(productAndItem.getSalesCatalog()));
        itemSummaryResponse.setOfflinePrices(
            this.itemHelperService.convertOfflineItemPrices(pickupPointCodeFilter, pickupPointCodesFilter,
                productAndItem));
        itemSummaryResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(itemPickupPoint));
        itemSummaryResponse.setPriceEditDisabledReasons(CommonUtil.getPriceEditDisabledReasons(itemPickupPoint));
        itemSummaryResponse.setIsLateFulfillment(
            CommonUtil.getLateFulfillmentFromProductType(itemSummaryResponse.getProductType(),
                overrideLateFulfillmentByProductType, itemSummaryResponse.getIsLateFulfillment()));
        itemSummaryResponses.add(itemSummaryResponse);
      } catch (Exception e) {
        ItemSummaryServiceImpl.LOGGER.warn("Skipping generating item summary response. ItemSku : {}",
            productAndItem.getItemSku(), e);
      }
    }
    return new ItemSummaryPageResponseVo(itemSummaryResponses, resultFromSolr.getTotalElements(),
        resultFromSolr.getTotalPages());
  }

  private ItemSummaryPageResponseVo constructItemSummaryResponse(
      Page<ProductAndItemSolr> resultFromSolr, Map<String, Set<Price>> mapOfPrices,
      String pickupPointCodeFilter, List<String> pickupPointCodesFilter,
      Map<String, Item> itemAndItemViewConfigMap, Map<String, Double> mapOfOriginalPrice,
      Map<String, Boolean> preOrderDetails, Map<String, Set<ItemViewConfig>> mapOfViewConfig) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<ItemSummaryResponseVO>();
    for (ProductAndItemSolr productAndItem : resultFromSolr.getContent()) {
      try {
        ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
        itemSummaryResponse.setStoreId(productAndItem.getStoreId());
        itemSummaryResponse.setVersion(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getVersion());
        itemSummaryResponse.setGeneratedItemName(productAndItem.getItemName());
        itemSummaryResponse.setItemCode(productAndItem.getItemCode());
        itemSummaryResponse.setItemSku(productAndItem.getItemSku());
        itemSummaryResponse.setItemViewConfigs(
            itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getItemViewConfigs());
        itemSummaryResponse.setIsLateFulfillment(productAndItem.isLatefulfillment());
        itemSummaryResponse.setMasterCatalog(this.solrDataConstructor
            .constructMasterCatalog(productAndItem.getMasterCatalog()));
        itemSummaryResponse.setMasterDataItemImages(this.solrDataConstructor
            .constructMasterDataItemMainImages(productAndItem.getItemImages()));
        itemSummaryResponse.setMerchantCode(productAndItem.getMerchantCode());
        itemSummaryResponse.setTicketTemplateCode(productAndItem.getTicketTemplateCode());
        itemSummaryResponse.setPrice(mapOfPrices.get(productAndItem.getItemSku()));
        itemSummaryResponse.setProductSku(productAndItem.getProductSku());
        itemSummaryResponse
            .setOff2OnChannelActive(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).isOff2OnChannelActive());
        itemSummaryResponse.setCreatedDate(productAndItem.getCreatedDate());
        itemSummaryResponse.setUpdatedDate(productAndItem.getUpdatedDate());
        itemSummaryResponse.setArchived(productAndItem.isArchived());
        itemSummaryResponse.setMarkForDelete(productAndItem.isMarkForDelete());
        itemSummaryResponse.setBrand(productAndItem.getBrand());
        itemSummaryResponse.setProductCode(productAndItem.getProductCode());
        itemSummaryResponse.setWholesalePriceActivated(productAndItem.getWholesalePriceActivated());
        if (Objects.nonNull(productAndItem.getProductScoreTotal())) {
          itemSummaryResponse.setProductScore(productAndItem.getProductScoreTotal());
        }
        if (Objects.nonNull(productAndItem.getPristineId())) {
          itemSummaryResponse.setPristineId(productAndItem.getPristineId());
        }
        itemSummaryResponse
            .setPromoBundling(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).isPromoBundling());
        itemSummaryResponse.setCncActivated(productAndItem.isCncActivated());
        itemSummaryResponse
            .setMerchantPromoDiscountActivated(productAndItem.isMerchantPromoDiscountActivated());
        Item item = itemAndItemViewConfigMap.get(productAndItem.getItemSku());
        itemSummaryResponse.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
        itemSummaryResponse.setPickupPointCode(item.getPickupPointCode());
        if (Objects.nonNull(mapOfOriginalPrice.get(productAndItem.getItemSku()))) {
          itemSummaryResponse.setOriginalSellingPrice(mapOfOriginalPrice.get(productAndItem.getItemSku()));
        }
        itemSummaryResponse.setOfflinePrices(this.itemHelperService
            .convertOfflineItemPrices(pickupPointCodeFilter, pickupPointCodesFilter,
                productAndItem));
        itemSummaryResponse.setProductName(productAndItem.getProductName());
        try {
          itemSummaryResponse.setProductType(ProductType.valueOf(productAndItem.getProductType()));
        } catch (Exception e) {
          ItemSummaryServiceImpl.LOGGER.warn(
              "Product Type is not recognize for : {} , for itemSku : {}",
              productAndItem.getProductType(), productAndItem.getItemSku(), e);
        }
        itemSummaryResponse.setSalesCatalogs(this.solrDataConstructor
            .constructSalesCatalogs(productAndItem.getSalesCatalog()));
        itemSummaryResponse.setPriceEditDisabled(
            itemService.isPriceEditDisabled(itemAndItemViewConfigMap.get(productAndItem.getItemSku())));
        itemSummaryResponse.setPriceEditDisabledReasons(
            CommonUtil.getPriceEditDisabledReasons(itemAndItemViewConfigMap.get(productAndItem.getItemSku())));
        itemSummaryResponse.setForceReview(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).isForceReview());
        itemSummaryResponse.setActivePromoBundlings(
            itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getActivePromoBundlings());
        itemSummaryResponse.setSuspended(productAndItem.isSuspended());
        ItemViewConfig itemViewConfig = new ArrayList<ItemViewConfig>(
            itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getItemViewConfigs()).get(0);
        itemSummaryResponse.setDiscoverable(itemViewConfig.isDiscoverable());
        itemSummaryResponse.setBuyable(itemViewConfig.isBuyable());
        itemSummaryResponse.setMerchantSku(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getMerchantSku());
        if (MapUtils.isNotEmpty(preOrderDetails)) {
          Boolean isPreOrderActive = preOrderDetails.get(itemSummaryResponse.getProductSku());
          itemSummaryResponse.setPreOrder(Objects.isNull(isPreOrderActive) ? false : isPreOrderActive);
        }
        itemSummaryResponse.setFreeSample(productAndItem.isFreeSample());
        if (Objects.nonNull(mapOfViewConfig.get(productAndItem.getItemSku()))) {
          itemSummaryResponse
              .setItemViewConfigs(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getItemViewConfigs());
          itemViewConfig =
              new ArrayList<>(itemAndItemViewConfigMap.get(productAndItem.getItemSku()).getItemViewConfigs()).get(0);
          itemSummaryResponse.setDiscoverable(itemViewConfig.isDiscoverable());
          itemSummaryResponse.setBuyable(itemViewConfig.isBuyable());
        }
        itemSummaryResponses.add(itemSummaryResponse);
      } catch (Exception e) {
        ItemSummaryServiceImpl.LOGGER.warn(
            "Skipping generating item summary response. ItemSku : {}", productAndItem.getItemSku(),
            e);
      }
    }
    return new ItemSummaryPageResponseVo(itemSummaryResponses, resultFromSolr.getTotalElements(),
        resultFromSolr.getTotalPages());
  }

  private ItemSummaryPageResponseVo constructItemSummaryResponse(
      Page<ProductAndItemSolr> resultFromSolr) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<ItemSummaryResponseVO>();
    for (ProductAndItemSolr productAndItem : resultFromSolr.getContent()) {
      try {
        ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
        itemSummaryResponse.setStoreId(productAndItem.getStoreId());
        itemSummaryResponse.setGeneratedItemName(productAndItem.getItemName());
        itemSummaryResponse.setItemCode(productAndItem.getItemCode());
        itemSummaryResponse.setItemSku(productAndItem.getItemSku());
        itemSummaryResponse.setMerchantCode(productAndItem.getMerchantCode());
        itemSummaryResponse.setIsLateFulfillment(
            CommonUtil.getLateFulfillmentFromProductType(itemSummaryResponse.getProductType(),
                overrideLateFulfillmentByProductType, itemSummaryResponse.getIsLateFulfillment()));
      } catch (Exception e) {
        ItemSummaryServiceImpl.LOGGER
            .warn("Skipping generating item summary response. ItemSku : {}", productAndItem.getItemSku(), e);
      }
    }
    return new ItemSummaryPageResponseVo(itemSummaryResponses, resultFromSolr.getTotalElements(),
        resultFromSolr.getTotalPages());
  }

  @Override
  public Map<String, String> getItemNameByItemSkus(String storeId, List<String> itemSkus,
      boolean includeMarkForDelete) {
    checkArgument(itemSkus != null && !itemSkus.isEmpty(), "item sku list must not empty");
    itemSkus = this.upperCaseAll(itemSkus);
    Map<String, String> mapOfItemName = new HashMap<String, String>();
      String[] includeFields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME,
          ProductFieldNames.GENERATED_ITEM_NAME_IN_MASTER_DATA_ITEM, ProductFieldNames.IS_SYNCHRONIZED};
      List<List<String>> batches = Lists.partition(itemSkus, itemSkuListSize);
      List<Item> finalItemList = new ArrayList<>();
      for (List<String> itemSkuList : batches) {
        Set<String> itemSkuSet = new HashSet<>(itemSkuList);
        List<Item> items = itemService.getItemsByItemSkus(storeId, itemSkuSet, includeFields);
        if (!includeMarkForDelete) {
          items = items.stream().filter(item -> !item.isMarkForDelete()).collect(Collectors.toList());
        }
        finalItemList.addAll(items);
      }
      for (Item item : finalItemList) {
        if (item.isSynchronized() || Objects.isNull(item.getMasterDataItem())) {
          mapOfItemName.put(item.getItemSku(), item.getGeneratedItemName());
        } else {
          mapOfItemName.put(item.getItemSku(), item.getMasterDataItem().getGeneratedItemName());
        }
      }
    return mapOfItemName;
  }

  @Override
  public ItemSummaryPageResponseVo getPromoItemSummaryByFilter(String storeId, String requestId, String username,
      ItemSummaryRequestVO itemSummaryRequestVO, String orderBy, String sortBy, PageRequest pageRequest) {
    checkArgument(Objects.nonNull(itemSummaryRequestVO), ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId), ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Page<ProductAndItemSolr> resultFromSolr = this.productAndItemSolrRepository
        .getPromoProductAndItemsByFilter(storeId, itemSummaryRequestVO, orderBy, sortBy, pageRequest);
    checkArgument(Objects.nonNull(resultFromSolr),
        ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER + itemSummaryRequestVO);
    if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
      return new ItemSummaryPageResponseVo(new ArrayList<>(), 0, 0);
    }
    List<String> distinctItemSkus = this.constructItemSkus(resultFromSolr.getContent());
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.findByItemSkusAndDelivery(storeId, distinctItemSkus, true);
    Map<String, Double> mapOfOriginalPrice = itemPickupPoints.stream().collect(
        Collectors.toMap(ItemPickupPoint::getItemSku,
            itemPickupPoint -> itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice(),
            (price1, price2) -> price1));
    Map<String, Set<Price>> mapOfPrices = itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    Map<String, Set<ItemViewConfig>> itemAndItemViewConfigMap = itemPickupPoints.stream()
        .collect(Collectors.toMap(ItemPickupPoint::getItemSku, ItemPickupPoint::getItemViewConfig, (v1, v2) -> v2));
    return itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(requestId, username, resultFromSolr, mapOfPrices,
            mapOfOriginalPrice, itemAndItemViewConfigMap);
  }

  @Override
  public ItemSummaryPageResponseVo getItemsSummaryDetailByFilter(String storeId, String username, String requestId,
      ItemsSummaryDetailRequestVo itemFilter, int page, int size){
    checkArgument(StringUtils.isNotBlank(itemFilter.getItemSku()) || StringUtils.isNotBlank(itemFilter.getProductSku()),
        ErrorMessages.EITHER_ITEM_SKU_OR_PRODUCT_SKU_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId), ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    return getItemsSummaryDetailByFilterIncludeAllPickUpPoints(storeId, username, requestId, itemFilter, page, size, true);
  }

  private ItemSummaryPageResponseVo getItemsSummaryDetailByFilterIncludeAllPickUpPoints(String storeId, String username, String requestId,
      ItemsSummaryDetailRequestVo itemFilter, int page, int size, boolean newRAUiDeployed) {
    Page<ItemPickupPoint> itemPickupPointPage = itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(storeId,
            itemFilter.getProductSku(), itemFilter.getItemSku(), page, size);
    long totalElements = itemPickupPointPage.getTotalElements();
    long totalPage = itemPickupPointPage.getTotalPages();
    if (itemFilter.isResponseWithoutPickupPoint() && CollectionUtils.isEmpty(itemPickupPointPage.getContent())) {
      return new ItemSummaryPageResponseVo(new ArrayList<>(), totalElements, totalPage);
    }
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPointPage.getContent()),
        ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER + itemFilter);
    List<Item> items = this.itemService.findByStoreIdAndItemSkusNotCached(storeId,
        itemPickupPointPage.getContent().stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet()));
    Map<String, Double> mapOfOriginalPrice = itemPickupPointPage.getContent().stream().collect(
        Collectors.toMap(itemPickupPoint -> itemPickupPoint.getOfflineItemId(),
            itemPickupPoint -> Optional.ofNullable(itemPickupPoint.getPrice()).orElse(Collections.emptySet()).stream()
                .findFirst().get().getOfferPrice(), (itemSku1, itemSku2) -> itemSku1));
    Map<String, Set<Price>> mapOfPrices =
        itemPriceService.getDiscountItemPickupPoints(itemPickupPointPage.getContent());
    Set<String> itemCodeList = items.stream().map(Item::getItemCode).collect(Collectors.toSet());
    Map<String, Item> itemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity(), (v1, v2) -> v2));
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    try {
      masterDataItemMap = masterDataService.getMasterDataItems(storeId, username, requestId, itemCodeList);
    } catch (Exception e) {
      ItemSummaryServiceImpl.LOGGER.warn("Enable to get masterData for item codes : {} ", itemCodeList, e);
    }
    return this.constructItemSummaryDetailsResponseFromItemPickupPointDetails(itemPickupPointPage.getContent(),
        mapOfPrices, itemMap, masterDataItemMap, storeId, totalElements, totalPage, newRAUiDeployed,
        mapOfOriginalPrice);
  }

  @Override
  public Page<ItemPickupPointVo> getItemPickupPointsAndItemNameByProductSku(String storeId, String productSku, int page,
      int size, boolean fbbActivated) {
    List<ItemPickupPointVo> pickupPointVos = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointsList = new ArrayList<>();
    Page<Item> itemPage = null;
    Page<ItemPickupPoint> itemPickupPoints = null;
      if (fbbActivated) {
         itemPage= itemService.getItemsByProductSkusPaginated(storeId, productSku,
            PageRequest.of(page, size, Sort.by(Constants.ITEM_SKU)));
         itemPage.getContent().stream()
            .filter(not(Item::isMarkForDelete)).forEach(item -> {
          performResultantActionBasedOnFbbL5s(storeId, size, itemPickupPointsList, item);
        });
        itemPickupPoints =
            new PageImpl<>(itemPickupPointsList, PageRequest.of(page, size), itemPage.getTotalElements());
      } else {
        itemPickupPoints = itemPickupPointService
            .findItemPickupPointByProductSkusOrItemSkus(storeId, Collections.singletonList(productSku),
                new ArrayList<>(), page, size);
      }
    if(Objects.nonNull(itemPickupPoints) && CollectionUtils.isNotEmpty(itemPickupPoints.getContent())) {
      Map<String, String> itemNameMap = getItemNameByItemSkus(storeId,
        itemPickupPoints.getContent().stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toList()), false);
      Map<String, String> pickupPointCodeNameMap =
        fetchPickupPointCodeNameMap(storeId, itemPickupPoints.getContent());
      log.info("Item Name Map and PP Code Name Map are : {} and {}", itemNameMap,
        pickupPointCodeNameMap);
      if (MapUtils.isNotEmpty(itemNameMap)) {
        pickupPointVos = itemPickupPoints.getContent().stream().map(
          itemPickupPoint -> generateItemPickupPointVo(itemPickupPoint.getItemSku(), itemNameMap.get(itemPickupPoint.getItemSku()), itemPickupPoint.getPickupPointCode(),
            pickupPointCodeNameMap.getOrDefault(itemPickupPoint.getPickupPointCode(), null))).collect(Collectors.toList());
      }
      return new PageImpl<>(pickupPointVos, PageRequest.of(page, size),
        itemPickupPoints.getTotalElements());
    }
    return new PageImpl<>(Collections.emptyList());
  }

  private void performResultantActionBasedOnFbbL5s(String storeId, int size, List<ItemPickupPoint> itemPickupPointsList,
      Item item) {
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, Collections.singletonList(item.getItemSku())).stream()
        .filter(ItemPickupPoint::isFbbActivated).collect(Collectors.toList());
    Optional<ItemPickupPoint> itemPickupPoint = itemPickupPoints.stream().findFirst();
    itemPickupPoint.ifPresent(itemPickupPointsList::add);
    if (!itemPickupPoint.isPresent()) {
      setItemPickupPointForNonFbbItem(itemPickupPointsList, item);
    }
  }

  private void setItemPickupPointForNonFbbItem(List<ItemPickupPoint> itemPickupPointsList, Item item) {
    ItemPickupPoint itemPickupPointNotFbb = new ItemPickupPoint();
    itemPickupPointNotFbb.setItemSku(item.getItemSku());
    itemPickupPointNotFbb.setPickupPointCode(null);
    itemPickupPointsList.add(itemPickupPointNotFbb);
  }

  private Map<String, String> fetchPickupPointCodeNameMap(String storeId,
    List<ItemPickupPoint> itemPickupPoints) {
    if(itemPickupPoints.stream().anyMatch(ItemPickupPoint->Objects.nonNull(ItemPickupPoint.getPickupPointCode()))) {
      List<BusinessPartnerPickupPoint> businessPartnerPickupPointByPickupPointCodes = Optional.ofNullable(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
          itemPickupPoints.stream().map(ItemPickupPoint::getPickupPointCode).filter(ppCode -> StringUtils.isNotBlank(ppCode)).distinct().collect(Collectors.toList())))
          .orElse(new ArrayList<>());
      if (CollectionUtils.isNotEmpty(businessPartnerPickupPointByPickupPointCodes)) {
        return businessPartnerPickupPointByPickupPointCodes.stream()
            .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::getName));
      }
      return new HashMap<>();
    } else {
      return new HashMap<>();
    }
  }

  private ItemPickupPointVo generateItemPickupPointVo(String itemSku, String name,
    String pickupPointCode,
    String pickupPointName) {
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setItemName(name);
    itemPickupPointVo.setItemSku(itemSku);
    itemPickupPointVo.setPickupPointCode(pickupPointCode);
    itemPickupPointVo.setPickupPointName(pickupPointName);
    return itemPickupPointVo;
  }

  @Override
  public ItemSummaryPageResponseVo getItemSummaryByItemSku(String storeId, String username, String requestId,
      String itemSku, String fetchViewConfigByChannel) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    ProductAndItemsVO product =
        productService.getProductAndSingleItemByItemSku(storeId, requestId, username, itemSku, true, true, false, false,
            null, false, true, false);
    productService.setPriceAndItemViewConfigFromItemPickupPointForItemSummary(storeId, product);
    checkState(Objects.nonNull(product), ErrorMessages.NO_PRODUCT_FOUND + itemSku);
    checkState(Objects.nonNull(product.getProduct()), ErrorMessages.NO_PRODUCT_FOUND + itemSku);
    checkState(CollectionUtils.isNotEmpty(product.getItems()), ErrorMessages.NO_PRODUCT_FOUND + itemSku);
    BusinessPartnerPromo businessPartnerPromo = getBusinessPartnerPromo(storeId, product);
    List<ItemPickupPoint> itemPickupPoints;
    if (cncForWarehouseFeatureSwitch) {
      itemPickupPoints = this.itemPickupPointService.findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(storeId, itemSku);
    } else {
      itemPickupPoints =
          this.itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, itemSku, true,
              false);
    }
    return constructItemSummaryResponse(product.getItems().get(0), product.getProduct(), itemPickupPoints,
        businessPartnerPromo, fetchViewConfigByChannel);
  }

  private BusinessPartnerPromo getBusinessPartnerPromo(String storeId, ProductAndItemsVO product) {
    boolean sellerPromoBunglingSystemParameter = Boolean.parseBoolean(systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.SELLER_PROMO_BUNDLINGS_SWITCH).getValue());
    BusinessPartnerPromo businessPartnerPromo = null;
    if (sellerPromoBunglingSystemParameter) {
      businessPartnerPromo =
          businessPartnerPromoService.findByBusinessPartnerCode(product.getProduct().getMerchantCode());
    }
    return businessPartnerPromo;
  }

  @Override
  public List<ItemSummaryListResponse> getItemSummaryByItemSkusList(String storeId, List<String> itemSkus)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    if (fetchPcbMasterData) {
      Map<String, String> itemCodeAndItemSkuMap = itemService.findListOfItemCodesByItemSkus(storeId, new HashSet<>(itemSkus));
      Map<String, MasterDataItem> masterDataItems = masterDataService
          .getMasterDataItems(storeId, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
              itemCodeAndItemSkuMap.keySet());
      ItemSummaryRequestVO itemFilter = new ItemSummaryRequestVO();
      itemFilter.setItemSkus(itemSkus);
      Page<ProductAndItemSolr> resultFromSolr = this.productAndItemSolrRepository
          .getProductAndItemsByFilter(storeId, itemFilter, null, null, PageRequest.of(0,
        itemSkus.size()));
      checkArgument(resultFromSolr != null, ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER + itemFilter);
      if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
        return new ArrayList<>();
      }
      List<String> fetchedItemSkus = this.constructItemSkus(resultFromSolr.getContent());
       List<ItemPickupPoint> itemPickupPoints =
          itemPickupPointService.findByItemSkusAndDelivery(storeId, fetchedItemSkus, true);
      Map<String, ItemPickupPoint> itemPickupPointMap =
          itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
      return objectConverter.constructItemSummaryListResponse(resultFromSolr.getContent(), itemPickupPointMap,
          masterDataItems);
    } else {
      List<Item> items = itemService.findByStoreIdAndItemSkus(storeId, new HashSet<>(itemSkus));
      List<String> itemSkuList = items.stream().map(Item::getItemSku).collect(Collectors.toList());
      Set<String> productSkus = items.stream().map(Item::getProductSku).collect(Collectors.toSet());
      Map<String, Product> productMap = Optional.ofNullable(productService
          .findByStoreIdAndProductSkuIn(storeId, new ArrayList<>(productSkus))).orElse(new ArrayList<>())
          .stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
      Map<String, ItemPickupPoint> itemPickupPointMap = Optional.ofNullable(itemPickupPointService
          .findByItemSkusAndDelivery(storeId, itemSkuList, true)).orElse(new ArrayList<>())
          .stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
      return objectConverter.constructItemSummaryListResponseUsingNewMasterData(items, productMap, itemPickupPointMap);
    }
  }

  @Override
  public void updateWholeSaleActivationFlag(String storeId, List<String> itemSkus, boolean wholeSalePriceActivated) {
    List<Item> items =
        itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, new HashSet<>(itemSkus));
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByItemSkusAndDelivery(storeId, itemSkus, true);
    itemPickupPoints.stream().forEach(itemPickupPoint -> {
      if (wholeSalePriceActivated) {
        itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
          itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        }
      }
    });
    itemPickupPointService.saveItemPickupPoint(itemPickupPoints);
    this.objectConverter.overrideL4DetailsFromL5(items, itemPickupPoints);
    items.stream().forEach(item -> {
      productAndItemSolrIndexerService.updateSolrOnPriceAndWholesalePriceFlagChange(item, wholeSalePriceActivated);
    });
  }

  @Override
  public Page<ItemCodeBasicDetailResponse> fetchBasicItemDetailsByItemCodes(String storeId,
    String itemCode, String searchKey, int page, int size, String sortBy, String orderBy) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode),
      ErrorMessages.ITEM_CODE_OR_KEYWORD_MUST_NOT_BE_BLANK);
    Page<Item> itemSummaryResponsesByItemCodes =
      itemRepositoryCustom.getItemSummaryResponsesByItemCodes(storeId, itemCode, searchKey, page,
        size, sortBy, orderBy);
    // Return empty page if no items found
    if (itemSummaryResponsesByItemCodes.isEmpty()) {
      return new PageImpl<>(Collections.emptyList(), PageRequest.of(page, size), 0);
    }
    Set<String> productSkus =
      itemSummaryResponsesByItemCodes.getContent().stream().map(Item::getProductSku).collect(Collectors.toSet());
    List<Product> productList = productSkus.stream().map(
        productSku -> productCacheableService.findProductByStoreIdAndProductSku(storeId, productSku))
      .collect(Collectors.toList());
    Map<String, Product> productSkuAndProductMap = productList.stream().filter(Objects::nonNull)
      .collect(toMap(Product::getProductSku, Functions.identity(), (a, b) -> a));
    List<ItemCodeBasicDetailResponse> itemCodeBasicDetailResponses =
      ResponseHelper.toItemCodeBasicDetailResponse(itemSummaryResponsesByItemCodes.getContent(),
        productSkuAndProductMap);
    return new PageImpl<>(itemCodeBasicDetailResponses, PageRequest.of(page, size),
      itemSummaryResponsesByItemCodes.getTotalElements());
    }

  private ItemSummaryPageResponseVo constructItemSummaryResponse(Item item, Product product,
      List<ItemPickupPoint> offlineItems, BusinessPartnerPromo businessPartnerPromo, String fetchViewConfigByChannel) {
    ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
    itemSummaryResponse.setStoreId(item.getStoreId());
    itemSummaryResponse.setVersion(item.getVersion());
    itemSummaryResponse.setItemCode(item.getItemCode());
    itemSummaryResponse.setItemSku(item.getItemSku());
    itemSummaryResponse.setItemViewConfigs(
        ResponseHelper.setViewConfigResponseByRequestString(item.getItemViewConfigs(), fetchViewConfigByChannel,
            cncForWarehouseFeatureSwitch));
    itemSummaryResponse.setIsLateFulfillment(item.getIsLateFulfillment());
    itemSummaryResponse.setMerchantCode(item.getMerchantCode());
    itemSummaryResponse.setPickupPointCode(item.getPickupPointCode());
    itemSummaryResponse.setTicketTemplateCode(item.getTicketTemplateCode());
    itemSummaryResponse.setPrice(item.getPrice());
    itemSummaryResponse.setProductSku(item.getProductSku());
    itemSummaryResponse.setOff2OnChannelActive(item.isOff2OnChannelActive());
    itemSummaryResponse.setFreeSample(item.isFreeSample());
    itemSummaryResponse.setCreatedDate(item.getCreatedDate());
    itemSummaryResponse.setUpdatedDate(item.getUpdatedDate());
    itemSummaryResponse.setArchived(item.isArchived());
    itemSummaryResponse.setMarkForDelete(item.isMarkForDelete());
    itemSummaryResponse.setProductCode(product.getProductCode());
    itemSummaryResponse.setWholesalePriceActivated(CommonUtil.getWholeSalePriceActivated(item));
    itemSummaryResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(item));
    itemSummaryResponse.setPriceEditDisabledReasons(CommonUtil.getPriceEditDisabledReasons(item));
    itemSummaryResponse.setForceReview(item.isForceReview());
    if (Objects.nonNull(businessPartnerPromo)) {
      itemSummaryResponse.setSellerActivePromoBundlings(businessPartnerPromo.getActivePromoBundlings());
    }
    itemSummaryResponse.setPromoBundling(item.isPromoBundling());
    itemSummaryResponse.setActivePromoBundlings(item.getActivePromoBundlings());
    ItemViewConfig itemViewConfig = new ArrayList<>(item.getItemViewConfigs()).get(0);
    itemSummaryResponse.setDiscoverable(itemViewConfig.isDiscoverable());
    itemSummaryResponse.setBuyable(itemViewConfig.isBuyable());
    itemSummaryResponse.setMerchantSku(item.getMerchantSku());
    itemSummaryResponse.setPromoBundling(item.isPromoBundling());
    itemSummaryResponse.setCncActivated(item.isCncActivated());
    itemSummaryResponse.setMerchantPromoDiscountActivated(item.isMerchantPromoDiscountActive());
    itemSummaryResponse.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
    if (Objects.nonNull(item.getMasterDataItem())) {
      itemSummaryResponse.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
      itemSummaryResponse.setMasterDataItemImages(item.getMasterDataItem().getMasterDataItemImages());
    }
    if (Objects.nonNull(product.getMasterDataProduct())) {
      itemSummaryResponse.setProductName(product.getMasterDataProduct().getProductName());
      itemSummaryResponse.setBrand(product.getMasterDataProduct().getBrand());
      itemSummaryResponse.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
    }
    itemSummaryResponse.setSuspended(product.isSuspended());
    if (Objects.nonNull(product.getProductScore())) {
      itemSummaryResponse.setProductScore(product.getProductScore().getTotalScore());
    }
    if (Objects.nonNull(item.getPristineDataItem())) {
      itemSummaryResponse.setPristineId(item.getPristineDataItem().getPristineId());
    }
    if (Objects.nonNull(item.getPrice().stream().findFirst().get().getOfferPrice())) {
      itemSummaryResponse.setOriginalSellingPrice(item.getPrice().stream().findFirst().get().getOfferPrice());
    }
    List<OfflineItemPriceVO> offlinePrices = getOfflineItemPriceVOS(offlineItems);
    itemSummaryResponse.setOfflinePrices(offlinePrices);
    itemSummaryResponse.setProductType(product.getProductType());
    itemSummaryResponse.setSalesCatalogs(product.getSalesCatalogs());
    itemSummaryResponse.setIsLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
            itemSummaryResponse.getIsLateFulfillment()));
    return new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponse), 1, 1);
  }

  private List<OfflineItemPriceVO> getOfflineItemPriceVOS(List<ItemPickupPoint> offlineItems) {
    List<OfflineItemPriceVO> offlinePrices = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(offlineItems)) {
      for (ItemPickupPoint itemPickupPoint : offlineItems) {
        OfflineItemPriceVO offlineItemPriceVO = new OfflineItemPriceVO();
        Price price = itemPickupPoint.getPrice().stream().findFirst().orElse(new Price());
        offlineItemPriceVO.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        offlineItemPriceVO.setOfferPrice(price.getOfferPrice());
        offlineItemPriceVO.setListPrice(validateAndGetListPrice(itemPickupPoint));
        offlinePrices.add(offlineItemPriceVO);
      }
    }
    return offlinePrices;
  }

  private Double validateAndGetListPrice(ItemPickupPoint itemPickupPoint) {
    Price price = itemPickupPoint.getPrice().stream().findFirst().orElse(new Price());
    return Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice());
  }

  private ItemSummaryPageResponseVo constructItemSummaryDetailsResponseFromItemPickupPointDetails(List<ItemPickupPoint> itemPickupPoints,
      Map<String, Set<Price>> mapOfPrices, Map<String, Item> itemMap, Map<String, MasterDataItem> masterDataItemMap,
      String storeId, long totalElements, long totalPage, boolean newRAUiDeployed, Map<String,Double> originalPrices) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<ItemSummaryResponseVO>();
    String productSku = itemPickupPoints.stream().findFirst().get().getProductSku();
    Product product = this.productService.getProductDeletedOrUndeleted(storeId, productSku);
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      try {
        Set<Price> prices = mapOfPrices.get(itemPickupPoint.getOfflineItemId());
        Item item = itemMap.get(itemPickupPoint.getItemSku());
        MasterDataItem masterDataItem = masterDataItemMap.get(item.getItemCode());
        ItemSummaryResponseVO itemSummaryResponseVO =
            constructItemSummaryResponseVO(item, product, itemPickupPoint, prices, masterDataItem,
                newRAUiDeployed);
        itemSummaryResponseVO.setOriginalSellingPrice(originalPrices.get(itemPickupPoint.getOfflineItemId()));
        itemSummaryResponseVO.setCncActivated(
            ResponseHelper.setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
                cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
                ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
        itemSummaryResponseVO.setFbbActivated(itemPickupPoint.isFbbActivated());
        itemSummaryResponses.add(itemSummaryResponseVO);
      } catch (Exception e) {
        log.warn("Skipping generating item summary response. ItemSku : {}", itemPickupPoint.getItemSku(), e);
      }
    }
    log.info(" constructItemSummaryDetailsResponse :: itemSummaryResponses : {}", itemSummaryResponses);
    return new ItemSummaryPageResponseVo(itemSummaryResponses, totalElements, totalPage);
  }

  private ItemSummaryResponseVO constructItemSummaryResponseVO(Item item,
      Product product, ItemPickupPoint itemPickupPoint, Set<Price> prices, MasterDataItem masterDataItem, boolean newRAUiDeployed) {
    ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
    itemSummaryResponse.setStoreId(item.getStoreId());
    itemSummaryResponse.setVersion(newRAUiDeployed ? item.getVersion() : itemPickupPoint.getVersion());
    itemSummaryResponse.setItemCode(item.getItemCode());
    itemSummaryResponse.setItemSku(item.getItemSku());
    itemSummaryResponse.setIsLateFulfillment(item.getIsLateFulfillment());
    itemSummaryResponse.setMasterCatalog(product.getMasterCatalog());
    itemSummaryResponse.setMerchantCode(item.getMerchantCode());
    itemSummaryResponse.setMerchantSku(itemPickupPoint.getMerchantSku());
    itemSummaryResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemSummaryResponse.setTicketTemplateCode(item.getTicketTemplateCode());
    itemSummaryResponse.setPrice(prices);
    itemSummaryResponse.setProductSku(item.getProductSku());
    itemSummaryResponse.setOff2OnChannelActive(item.isOff2OnChannelActive());
    itemSummaryResponse.setCreatedDate(item.getCreatedDate());
    itemSummaryResponse.setUpdatedDate(item.getUpdatedDate());
    itemSummaryResponse.setArchived(item.isArchived());
    itemSummaryResponse.setMarkForDelete(item.isMarkForDelete());
    itemSummaryResponse.setProductCode(product.getProductCode());
    itemSummaryResponse.setWholesalePriceActivated(itemPickupPoint.isWholesalePriceExists() ?
        itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE) :
        null);
    itemSummaryResponse.setFreeSample(item.isFreeSample());
    if (Objects.nonNull(product.getProductScore())) {
      itemSummaryResponse.setProductScore(product.getProductScore().getTotalScore());
    }
    if (Objects.nonNull(item.getPristineDataItem())) {
      itemSummaryResponse.setPristineId(item.getPristineDataItem().getPristineId());
    }
    if (cncForWarehouseFeatureSwitch) {
      itemSummaryResponse.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs());
    } else {
      itemSummaryResponse.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
    }
    itemSummaryResponse.setItemViewConfigB2b(
        Optional.ofNullable(itemPickupPoint.getAllItemViewConfigs()).orElse(new HashSet<>()).stream()
            .filter(viewConfig -> Constants.B2B.equalsIgnoreCase(viewConfig.getChannel())).collect(Collectors.toSet()));
    itemSummaryResponse.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
    itemSummaryResponse.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    itemSummaryResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemSummaryResponse.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    itemSummaryResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(itemPickupPoint));
    itemSummaryResponse.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    itemSummaryResponse.setCncActivated(item.isCncActivated());
    itemSummaryResponse.setMerchantPromoDiscountActivated(
        Optional.ofNullable(prices).orElse(new HashSet<>()).stream().anyMatch(price -> Objects.nonNull(price.getMerchantPromoDiscountPrice())));
    itemSummaryResponse.setProductType(product.getProductType());
    itemSummaryResponse.setSalesCatalogs(product.getSalesCatalogs());
    itemSummaryResponse.setForceReview(item.isForceReview());
    if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
      B2bFields b2bFields =
          new B2bFields(itemPickupPoint.getB2bFields().isManaged(), itemPickupPoint.getB2bFields().getBasePrice());
      itemSummaryResponse.setB2bFields(b2bFields);
    }
    if (Objects.nonNull(masterDataItem) && CollectionUtils.isNotEmpty(masterDataItem.getMasterDataItemImages())) {
      itemSummaryResponse.setUpcCode(masterDataItem.getUpcCode());
      itemSummaryResponse.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
      itemSummaryResponse.setGeneratedItemName(masterDataItem.getGeneratedItemName());
    }
    if (!product.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
      itemSummaryResponse.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
    }
    itemSummaryResponse.setSuspended(product.isSuspended());
    itemSummaryResponse.setBundleRecipe(Optional.ofNullable(item.getBundleRecipe())
        .orElse(new HashSet<>()).stream().map(bundleRecipe -> new BundleRecipeVo(
            bundleRecipe.getItemSku(), bundleRecipe.getQuantity())).collect(Collectors.toSet()));
    itemSummaryResponse.setDistribution(itemPickupPoint.isDistribution());
    return itemSummaryResponse;
  }

  @Override
  public ItemSummaryPageResponseVo getItemSummaryByFilter(String storeId, String username, String requestId,
      ItemSummaryRequestVO itemFilter, String orderBy, String sortBy, PageRequest pageRequest) {
    checkArgument(itemFilter != null, ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    List<String> itemSkus = itemFilter.getItemSkus();
    if (itemSkus != null) {
      itemSkus.removeAll(Collections.singleton(null));
      itemSkus.remove("");
      itemFilter.setItemSkus(this.upperCaseAll(itemFilter.getItemSkus()));
    }

    Page<ProductAndItemSolr> resultFromSolr =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(storeId, itemFilter, orderBy, sortBy, pageRequest);
    checkArgument(resultFromSolr != null, ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER
        + itemFilter);

    if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
      return new ItemSummaryPageResponseVo(new ArrayList<>(), 0, 0);
    }

    List<String> distinctItemSkus = this.constructItemSkus(resultFromSolr.getContent());
    List<Item> itemsFromDb = this.itemService.getItemPriceAndViewConfigs(storeId, distinctItemSkus);
    List<ItemPickupPoint> itemPickupPoints;
    if (itemFilter.getFbbActivated()) {
      itemPickupPoints = this.itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(storeId,
          new HashSet<>(distinctItemSkus), true);
    }
    else {
      itemPickupPoints =
        this.itemPickupPointService.findByItemSkusAndDelivery(storeId, distinctItemSkus, true);
    }
    Map<String, Item> itemMap = itemsFromDb.stream().collect(
        Collectors.toMap(item -> item.getItemSku(), Function.identity()));


    Map<String, PreOrder> preOrderDetails = new HashMap<>();
    if (itemFilter.isPreOrderStatus()) {
      List<String> productSkus = itemsFromDb.stream().map(Item::getProductSku).collect(Collectors.toList());
      preOrderDetails = productService.getPreOrderStatusByProductSkus(productSkus);
    }

    Map<String, Double> mapOfOriginalPrice = itemPickupPoints.stream().collect(
        Collectors.toMap(ItemPickupPoint::getItemSku,
            itemPickupPoint -> itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice(),
            (price1, price2) -> price1));
    Map<String, Set<Price>> mapOfPrices = itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    Map<String, ItemPickupPoint> itemAndItemViewConfigMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));

    return this
        .constructItemSummaryResponseFromL5(resultFromSolr, mapOfPrices, itemFilter.getPickupPointCode(),
            itemFilter.getPickupPointCodes(), itemAndItemViewConfigMap, mapOfOriginalPrice,
            preOrderDetails, itemMap);
  }


  @Override
  public ItemSummaryPageResponseVo getItemNamesByKeyword(String storeId, String username, String requestId,
      ItemSummaryRequestVO itemFilter, String orderBy, String sortBy, PageRequest pageRequest) {
    checkArgument(itemFilter != null, ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId), ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Page<ProductAndItemSolr> resultFromSolr =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(storeId, itemFilter, orderBy, sortBy, pageRequest);
    if (Objects.nonNull(resultFromSolr))
      return this.constructItemSummaryResponse(resultFromSolr);
    else
      return new ItemSummaryPageResponseVo();
  }

  @Override
  public ItemSummaryPageResponseVo getBulkItemSummaryByFilter(String storeId, String username, String requestId,
      BulkItemSummaryRequestVo itemFilter, PageRequest pageRequest, String sortBy, String orderBy) {
    checkArgument(Objects.nonNull(itemFilter), ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId), ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);

    if (Objects.nonNull(itemFilter.getItemSkus())) {
      itemFilter.setItemSkus(itemFilter.getItemSkus().stream()
        .filter(StringUtils::isNotBlank)
        .map(String::toUpperCase)
        .collect(Collectors.toList())
      );
    }
    Page<ProductAndItemSolr> resultFromSolr = this.productAndItemSolrRepository
        .getBulkProductAndItemsByFilter(storeId, itemFilter, pageRequest, sortBy, orderBy);

    checkArgument(Objects.nonNull(resultFromSolr), ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER + itemFilter);

    if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
      return new ItemSummaryPageResponseVo(new ArrayList<>(), 0, 0);
    }

    List<String> distinctItemSkus = this.constructItemSkus(resultFromSolr.getContent());
    List<Item> itemsFromDb = this.itemService.getItemPriceAndViewConfigs(storeId, distinctItemSkus);
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.findByItemSkusAndDelivery(storeId, distinctItemSkus, true);
    Map<String, Item> itemMap = itemsFromDb.stream().collect(
        Collectors.toMap(item -> item.getItemSku(), Function.identity()));
    Map<String, Set<Price>> mapOfPrices = itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));

    return this
        .constructItemSummaryResponseFromL5(resultFromSolr, mapOfPrices, itemFilter.getPickupPointCode(),
            Arrays.asList(itemFilter.getPickupPointCode()), itemPickupPointMap, new HashMap<>(),
            new HashMap<>(), itemMap);
  }

  @Override
  public ItemSummaryPageResponseVo  getItemSummaryByArchivedFilter(String storeId, String username,
      String requestId, ItemSummaryRequestVO itemFilter, PageRequest pageRequest) {
    checkArgument(itemFilter != null, ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    List<String> itemSkus = itemFilter.getItemSkus();
    if (itemSkus != null) {
      itemSkus.removeAll(Collections.singleton(null));
      itemSkus.remove("");
      itemFilter.setItemSkus(this.upperCaseAll(itemFilter.getItemSkus()));
    }
    itemFilter.setArchived(Boolean.TRUE);
    Page<ProductAndItemSolr> resultFromSolr =
        this.productAndItemSolrRepository.getProductAndItemsByFilter(storeId, itemFilter, null, null, pageRequest);
    checkArgument(resultFromSolr != null, ItemSummaryServiceImpl.NOT_FOUND_PRODUCT_WITH_FILTER
        + itemFilter);
    if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
      return new ItemSummaryPageResponseVo(new ArrayList<>(), 0, 0);
    }
    List<String> distinctItemSkus = this.constructItemSkus(resultFromSolr.getContent());
    List<Item> itemsFromDb = this.itemService.getItemPriceAndViewConfigs(storeId, distinctItemSkus);
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.findByItemSkusAndDelivery(storeId, distinctItemSkus, true);
    Map<String, Item> itemMap = itemsFromDb.stream().collect(
        Collectors.toMap(item -> item.getItemSku(), Function.identity()));
    Map<String, Set<Price>> mapOfPrices = itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    Map<String, ItemPickupPoint> itemPickupPointMap = itemPickupPoints.stream()
        .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
    return this.constructItemSummaryResponseFromL5(resultFromSolr, mapOfPrices, itemFilter.getPickupPointCode(),
        itemFilter.getPickupPointCodes(), itemPickupPointMap, new HashMap<>(), new HashMap<>(), itemMap);
  }

  @Override
  public Map<String, String> getProductNameByProductSkus(String storeId, List<String> productSkus) {
    checkArgument(productSkus != null && !productSkus.isEmpty(), "product sku list must not empty");
    productSkus = this.upperCaseAll(productSkus);
    Map<String, String> mapOfProductName = new HashMap<String, String>();
      String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_NAME,
          ProductFieldNames.PRODUCT_NAME_IN_MASTER_DATA_PRODUCT, ProductFieldNames.IS_SYNCHRONIZED};
      List<List<String>> batches = Lists.partition(productSkus, productSkuListSize);
      List<Product> finalProductList = new ArrayList<>();
      for (List<String> productSkuList : batches) {
        Set<String> productSkuSet = new HashSet<>(productSkuList);
        List<Product> productList = this.productService.getProductsByProductSkus(storeId, productSkuSet,  includedField, false);
        finalProductList.addAll(productList);
      }
      for (Product product : finalProductList) {
        if (product.isSynchronized()) {
          mapOfProductName.put(product.getProductSku(), product.getProductName());
        } else {
          mapOfProductName.put(product.getProductSku(), product.getMasterDataProduct().getProductName());
        }
      }
    return mapOfProductName;
  }

  @Override
  public Map<String, String> getProductNamesByProductCodes(String storeId, List<String> productCodes) {
    checkArgument(productCodes != null && !productCodes.isEmpty(),
        "product code list must not empty");
    Set<String> productCodeSet = new HashSet<String>(productCodes);
    List<ProductAndItemSolr> result =
        this.productAndItemSolrRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(
            storeId, productCodeSet);
    Map<String, String> mapOfProductName = new HashMap<String, String>();
    for (ProductAndItemSolr productAndItemSolr : result) {
      mapOfProductName.put(productAndItemSolr.getProductSku(), productAndItemSolr.getProductName());
    }
    return mapOfProductName;
  }

  @Override
  public ItemSummaryResponseVO updateItemSummary(String storeId, String requestId, String username,
      String itemSku, String merchantCode, UpdateItemSummaryRequestVo updateItemSummaryRequestVo,
      Boolean wholesalePriceActivated)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(itemSku), "item sku must not be blank");
    checkArgument(StringUtils.isNotBlank(merchantCode), "merchant code must not be blank");
    checkArgument(updateItemSummaryRequestVo != null, "update vo object must not be null");
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    ItemPickupPoint itemPickupPoint = this.itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    checkState(!item.isForceReview(), CommonUtil.ITEM_NOT_EDITABLE);
    String existingMerchantSku = item.getMerchantSku();
    this.objectConverter
      .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    Product product =
        this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
            item.getProductSku());
    LOGGER.info("Request :#updateItemSummaryRequestVo:{}" +updateItemSummaryRequestVo);
    if(!item.isSynchronized() && Objects.nonNull(product.getMasterDataProduct())) {
      double shippingWeight = product.getMasterDataProduct().getShippingWeight();
      if(ProductType.REGULAR.equals(product.getProductType())
          && shippingWeight > maxShippingWeight) {
        LOGGER.error("cannot update regular product because weight > 50! for gdnSku : {}", itemSku);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            CommonConstants.WEIGHT_EXCEED_ERROR);
      }
    }
    boolean isPromoBundlingChanged = false;
    if (Objects.nonNull(wholesalePriceActivated)) {
      item.setWholesalePriceExists(true);
      itemPickupPoint.setWholesalePriceExists(true);
    }
    if (Objects.isNull(wholesalePriceActivated) || !wholesalePriceActivated) {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings()) && item.getActivePromoBundlings()
          .contains(Constants.WHOLESALE_PRICE)) {
        item.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        isPromoBundlingChanged = true;
      }
    } else {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings())) {
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        item.setActivePromoBundlings(new HashSet<>());
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      }
      isPromoBundlingChanged = true;
    }

    if (Objects.isNull(wholesalePriceActivated) || !wholesalePriceActivated) {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings()) && itemPickupPoint
        .getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE)) {
        itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        isPromoBundlingChanged = true;
      }
    } else {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
        itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        itemPickupPoint.setActivePromoBundlings(new HashSet<>());
        itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      }
      isPromoBundlingChanged = true;
    }

    ProductAndItemsVO productAndItem = new ProductAndItemsVO(product, Arrays.asList(item),
      Arrays.asList(itemPickupPoint));
    boolean isPriceChanged = false;
    boolean isItemDataChanged = false;
    boolean isShippingChanged = false;
    boolean isOff2OnChannelActiveChanged = false;
    boolean isViewConfigChange = false;
    boolean isPickupPointCodeChanged = false;
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    if (!merchantCode.equals(productAndItem.getProduct().getMerchantCode())) {
      throw new Exception("merchant code value is not correct");
    }
    Item itemToSet = productAndItem.getItems().get(0);
    ItemPickupPoint itemPickupPointToSet = productAndItem.getItemPickupPoints().get(0);
    checkArgument(!itemToSet.isArchived(), "cannot update archived product");
    if (Objects.nonNull(updateItemSummaryRequestVo.getIsLateFulfillment()) &&
        Objects.nonNull(itemToSet.getIsLateFulfillment())
        && !itemToSet.getIsLateFulfillment().equals(updateItemSummaryRequestVo.getIsLateFulfillment())) {
      isItemDataChanged = true;
      itemToSet.setLateFulfillment(updateItemSummaryRequestVo.getIsLateFulfillment());
      itemToSet.setLateFullfillmentUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
      itemToSet.setLateFullfillmentUpdatedDate(new Date());
      LOGGER.info("IsLateFulfillment changed for itemSku: {}", itemSku);
    }
    Map<String, Boolean> pickupPointToPureCNCStatusChangeMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(updateItemSummaryRequestVo.getItemViewConfigs())
        && itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(itemToSet,
        updateItemSummaryRequestVo.getItemViewConfigs())){
      pickupPointToPureCNCStatusChangeMap.put(item.getPickupPointCode(),
          CommonUtil.isPureCNCStatusChange(item.getItemViewConfigs().iterator().next(),
              item.getItemViewConfigs().iterator().next(), itemPickupPoint.isCncActive(),
              itemPickupPoint.isCncActive()));
      isItemDataChanged = true;
      isViewConfigChange = true;
      this.productHelperService.updateItemViewConfigForExistingChannel(itemToSet,
          updateItemSummaryRequestVo.getItemViewConfigs());
      itemPickupPointToSet.setPickupPointCode(item.getPickupPointCode());
      LOGGER.info("ItemViewConfigs changed for itemSku: {}", itemSku);
    }
    if (!StringUtils.equals(itemToSet.getMerchantSku(), updateItemSummaryRequestVo.getMerchantSku())) {
      isItemDataChanged = true;
      itemToSet.setMerchantSku(updateItemSummaryRequestVo.getMerchantSku());
      itemPickupPointToSet.setMerchantSku(updateItemSummaryRequestVo.getMerchantSku());
      LOGGER.info("MerchantSku changed for itemSku : {}", itemSku);
    }
    if (StringUtils.isNotBlank(updateItemSummaryRequestVo.getPickupPointCode()) &&
        !StringUtils.equals(itemToSet.getPickupPointCode(), updateItemSummaryRequestVo.getPickupPointCode())) {
      itemChangeEventTypes.add(ItemChangeEventType.SHIPPING_CHANGE);
      itemToSet.setPickupPointCode(updateItemSummaryRequestVo.getPickupPointCode());
      isShippingChanged = true;
      isPickupPointCodeChanged = true;
      getAndSetPickupPointCodes(storeId, updateItemSummaryRequestVo, itemPickupPoint, product);
      itemToSet.setPickupPointCode(updateItemSummaryRequestVo.getPickupPointCode());
      LOGGER.info("PickupPointCode changed for itemSku: {}", itemSku);
    }
    Set<Price> prices = updateItemSummaryRequestVo.getPrice();
    isPriceChanged = itemService.validatePriceChangeAndSet(itemToSet, prices, username);
    if(Objects.nonNull(updateItemSummaryRequestVo.getOff2OnChannelActive()) &&
        updateItemSummaryRequestVo.getOff2OnChannelActive() != itemToSet.isOff2OnChannelActive()) {
      itemToSet.setOff2OnChannelActive(updateItemSummaryRequestVo.getOff2OnChannelActive());
      itemChangeEventTypes.add(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE);
      isOff2OnChannelActiveChanged = true;
      LOGGER.debug("off2OnChannelActive changed for itemSku: {}", itemSku);
    }
    ProductAndItemsVO productAndItems;
    boolean itemChanged = CommonUtil
      .checkItemChanged(isItemDataChanged, isShippingChanged, isOff2OnChannelActiveChanged,
        isPriceChanged, isPromoBundlingChanged);
    Map<String, ItemPickupPoint> updateItemPickupPoint = new HashMap<>();
    LOGGER.info("Item changed for itemSku: {}", itemSku);
    if(isItemDataChanged) {
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
    if(isPriceChanged) {
      itemPickupPointToSet.setPrice(item.getPrice());
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
      itemPriceService.publishItemPriceChangeEvent(username, requestId, product, itemToSet);
    }
    if (isShippingChanged) {
      updateItemPickupPoint =
          itemService.updatePickupPointInItemPickupPoints(item.getPickupPointCode(), itemPickupPointToSet, item,
              pickupPointToPureCNCStatusChangeMap, false, null, false);
    }
    itemToSet.setItemChangeEventTypes(itemChangeEventTypes);
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    if (itemChanged) {
      productAndItem.getItems().get(0).setContentChanged(false);
      SystemParameter systemParameter = systemParameterService.findValueByStoreIdAndVariable(
          storeId, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
      List<Item> saveItems;
      boolean enableDifferredSolrReindex =
          Objects.isNull(systemParameter.getValue()) ? false : Boolean.valueOf(systemParameter.getValue());
      updatedItemPickupPoints = new ArrayList<>(this.itemPickupPointService.saveItemPickupPoint(
          MapUtils.isEmpty(updateItemPickupPoint) ?
              Collections.singletonList(itemPickupPointToSet) :
              new ArrayList<>(updateItemPickupPoint.values())));
      updatedItemPickupPoints.removeIf(itemPickup -> !itemPickup.isDelivery() || itemPickup.isMarkForDelete());
      if (isOff2OnChannelActiveChanged && updateItemSummaryRequestVo.getOff2OnChannelActive()) {
        updateOffToOnItemCount(product, true, 1);
        productAndItem.setProduct(product);
      } else if (isOff2OnChannelActiveChanged) {
        updateOffToOnItemCount(product, false, 1);
        productAndItem.setProduct(product);
      }
      if (enableDifferredSolrReindex) {
        saveItems = processDeferredReindex(productAndItem, isOff2OnChannelActiveChanged, isPickupPointCodeChanged,
            new ArrayList<>(Arrays.asList(itemPickupPoint)));
      } else {
        saveItems = processProductAndItemUpdates(productAndItem, isOff2OnChannelActiveChanged, isPickupPointCodeChanged,
            updatedItemPickupPoints);
      }
      productAndItems = new ProductAndItemsVO(productAndItem.getProduct(), saveItems,
        Collections.singletonList(itemPickupPointToSet));
      if(isViewConfigChange) {
        saveAndPublishService.publishMerchantVoucherViewConfigChange(updatedItemPickupPoints, Arrays.asList(itemToSet));
      }
    } else {
      productAndItems = new ProductAndItemsVO();
    }
    productAndItemSolrIndexerService.updateWholesalePriceActivatedFlag(itemSku, wholesalePriceActivated);
    ItemSummaryResponseVO itemSummaryResponseVO = this.constructItemResponseVoAfterUpdate(
        storeId, username, requestId, itemSku, productAndItems);

    this.itemService.updateItemPickupPointIfMerchantSkuChanged(storeId, existingMerchantSku, itemToSet,
        updatedItemPickupPoints.stream().findFirst().orElse(null));
    itemSummaryResponseVO.setIsLateFulfillment(CommonUtil.getLateFulfillmentFromProductType(product.getProductType(),
        overrideLateFulfillmentByProductType, itemSummaryResponseVO.getIsLateFulfillment()));
    return itemSummaryResponseVO;
  }

  private List<Item> processProductAndItemUpdates(ProductAndItemsVO productAndItem, boolean isOff2OnChannelActiveChanged,
      boolean isPickupPointCodeChanged, List<ItemPickupPoint> updatedItemPickupPoints) {
    List<Item> saveItems;
    if (isPickupPointCodeChanged || isOff2OnChannelActiveChanged) {
      ProductAndItemsVO productAndItemsVO = saveOperationService.saveProductAndItems(productAndItem, new ArrayList<>());
      saveItems = productAndItemsVO.getItems();
    } else {
      saveItems = this.saveOperationService.saveItems(productAndItem.getItems(), updatedItemPickupPoints);
    }
    return saveItems;
  }

  private List<Item> processDeferredReindex(ProductAndItemsVO productAndItem, boolean isOff2OnChannelActiveChanged,
      boolean isPickupPointCodeChanged, List<ItemPickupPoint> itemPickupPoints) {
    List<Item> saveItems;
    if (isPickupPointCodeChanged || isOff2OnChannelActiveChanged) {
      ProductAndItemsVO productAndItemsVO = saveOperationService.saveProductAndItemsWithDeferredReindex(productAndItem);
      saveItems = productAndItemsVO.getItems();
    } else {
      saveItems =
          this.saveOperationService.saveItemsWithDeferredReindexing(productAndItem.getItems(), itemPickupPoints);
    }
    return saveItems;
  }

  private void updateOffToOnItemCount(Product product, boolean activate, int increment) {
    if (activate) {
      increment *= 1;
    } else {
      increment *= -1;
    }
    product.setOff2OnItemCount(product.getOff2OnItemCount() + increment);
  }

  private void getAndSetPickupPointCodes(String storeId, UpdateItemSummaryRequestVo updateItemSummaryRequestVo,
      ItemPickupPoint itemPickupPoint, Product product) {
    product.getPickupPointCodes().add(updateItemSummaryRequestVo.getPickupPointCode());
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(storeId, product.getProductSku(),
            itemPickupPoint.getPickupPointCode());
    itemPickupPoints.removeIf(
        itemPickupPoint1 -> itemPickupPoint1.getOfflineItemId().equalsIgnoreCase(itemPickupPoint.getOfflineItemId()));
    if (CollectionUtils.isEmpty(itemPickupPoints)) {
      product.getPickupPointCodes().remove(itemPickupPoint.getPickupPointCode());
    }
  }

  @Override
  public void updateItemListing(String storeId, String requestId, String username, String productSku,
      ProductType productType, List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos) throws Exception {
    boolean pickupPointChanged = false;
    List<Item> updatedItemList = new ArrayList<>();
    List<Item> viewConfigChanged = new ArrayList<>();
    List<ItemPickupPoint> updatedItemPickupPoint = new ArrayList<>();
    Map<String, Boolean> offlineItemIdToPureCNCStatusChangeMap = new HashMap<>();
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
    List<ItemPickupPoint> offlineItemChangeUpdatedEvent = new ArrayList<>();

    Product product = this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    List<Item> itemList =
        cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(storeId,
            ProductAndItemsUtil.getItemSkuFromItemListingUpdateRequestVo(itemListingUpdateRequestVos));
    Map<String, Item> itemMap = ProductAndItemsUtil.getItemMapKeyItemSku(itemList);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        ProductAndItemsUtil.getItemPickupPointMapKeyItemSku(itemPickupPointList);

    for (ItemListingUpdateRequestVo itemListingUpdateRequestVo : itemListingUpdateRequestVos) {
      Item item = itemMap.get(itemListingUpdateRequestVo.getItemSku());
      ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(itemListingUpdateRequestVo.getItemSku());

      if (Objects.isNull(item) || Objects.isNull(itemPickupPoint)) {
        continue;
      }

      CommonUtil.setItemPickupPointDetailsInItem(item, itemPickupPoint, false);
      item.setItemChangeEventTypes(new ArrayList<>());
      checkAndUpdateWholeSalePromo(item, itemListingUpdateRequestVo);
      if (!cncForWarehouseFeatureSwitch) {
        offlineItemIdToPureCNCStatusChangeMap.putIfAbsent(itemPickupPoint.getOfflineItemId(),
            CommonUtil.isPureCNCStatusChange(
                itemListingUpdateRequestVo.getItemViewConfigs().iterator().next(),
                itemPickupPoint.getItemViewConfig().iterator().next(),
                itemPickupPoint.isCncActive(), itemPickupPoint.isCncActive()));
      }

      boolean isDiscoverableChanged = CommonUtil.isDiscoverableChanged(
          itemListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT),
          itemPickupPoint.getSingleItemViewConfigByChannel(Constants.DEFAULT));

      boolean isCncDiscoverableChanged = false;

      if (cncForWarehouseFeatureSwitch) {
        isCncDiscoverableChanged = CommonUtil.isDiscoverableChanged(
            itemListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.CNC),
            itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC));
      }


      boolean itemDataChanged = checkAndUpdateItemViewConfig(product, item, itemListingUpdateRequestVo);
      boolean isPriceChanged =
          itemService.validatePriceChangeAndSet(item, itemListingUpdateRequestVo.getPrice(), username);
      if (isPriceChanged && itemPickupPoint.isCncActive()) {
        offlineItemChangeUpdatedEvent.add(itemPickupPoint);
      }
      if (isPriceChanged) {
        item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_PRICE_CHANGE);
        itemPriceService.publishItemPriceChangeEvent(username, requestId, product, item);
      }
      pickupPointChanged =
        checkAndUpdateItemData(storeId, item, itemListingUpdateRequestVo, itemDataChanged);
      if (itemDataChanged) {
        viewConfigChanged.add(item);
      }
      updatedItemList.add(item);
      populateL5FromL4(itemPickupPoint, item);
      ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
          objectConverter.convertToItemPickupPointChangeEventModel(itemPickupPoint,
              offlineItemIdToPureCNCStatusChangeMap.get(itemPickupPoint.getOfflineItemId()));
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
          itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
      if (isPriceChanged) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
        itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
      }
      if (isDiscoverableChanged) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
      }
      if (isCncDiscoverableChanged) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
      }
      itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
      updatedItemPickupPoint.add(itemPickupPoint);
    }
    if (Objects.nonNull(productType)) {
      product.setProductType(productType);
    }
    saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
      StringUtils.EMPTY, Collections.EMPTY_MAP);
    saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(updatedItemList);
    if (CollectionUtils.isNotEmpty(updatedItemList)) {
      SystemParameter systemParameter = systemParameterService
          .findValueByStoreIdAndVariable(storeId, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
      boolean enableDifferedSolrReindex =
          Objects.isNull(systemParameter.getValue()) ? false : Boolean.valueOf(systemParameter.getValue());
      if (enableDifferedSolrReindex) {
        this.saveOperationService.deferredReindexItems(storeId, updatedItemList, true,
          ReindexType.ITEM_REINDEX, pickupPointChanged);
      } else {
        productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, updatedItemList, false);
      }
    }
    List<ItemPickupPoint> savedItemPickupPoint = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(updatedItemPickupPoint)) {
      savedItemPickupPoint = itemPickupPointService.saveItemPickupPoint(updatedItemPickupPoint);
    }
    if (CollectionUtils.isNotEmpty(updatedItemList)) {
      saveAndPublishService.publishListOfItems(updatedItemList, savedItemPickupPoint,
          itemPickupPointDataChangeEventModelList, false);
    }
    for (ItemPickupPoint pickupPoint : offlineItemChangeUpdatedEvent) {
      OfflineItemChange offlineItemChange = CommonUtil.convertToOfflineItemChange(pickupPoint, itemMap);
      saveAndPublishService.publishOfflineItemChangeSellerEvent(offlineItemChange);
    }
    saveAndPublishService.publishMerchantVoucherViewConfigChange(savedItemPickupPoint, viewConfigChanged);
  }

  private void populateL5FromL4(ItemPickupPoint itemPickupPoint, Item item) {
    itemPickupPoint.setItemViewConfig(item.getItemViewConfigs());
    CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, channelService.getDefaultChannel());
    itemPickupPoint.setPrice(item.getPrice());
    itemPickupPoint.setWholesalePriceExists(item.isWholesalePriceExists());
    itemPickupPoint.setPromoBundling(item.isPromoBundling());
    itemPickupPoint.setActivePromoBundlings(item.getActivePromoBundlings());
    itemPickupPoint.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
    itemPickupPoint.setFlashSaleActive(item.isFlashSaleActive());
  }

  private void checkAndUpdateWholeSalePromo(Item item, ItemListingUpdateRequestVo itemListingUpdateRequestVo) {
    item.setWholesalePriceActivated(itemListingUpdateRequestVo.getWholesalePriceActivated());
    if (Objects.isNull(itemListingUpdateRequestVo.getWholesalePriceActivated()) || !itemListingUpdateRequestVo
        .getWholesalePriceActivated()) {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings()) && item.getActivePromoBundlings()
          .contains(Constants.WHOLESALE_PRICE)) {
        item.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
    } else {
      if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings())) {
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      } else {
        item.setActivePromoBundlings(new HashSet<>());
        item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
      }
    }
  }

  private boolean checkAndUpdateItemViewConfig(Product product, Item item,
      ItemListingUpdateRequestVo itemListingUpdateRequestVo) {
    boolean itemDataChanged = false;
    if (CollectionUtils.isNotEmpty(item.getItemViewConfigs()) && itemViewConfigService
        .isItemViewConfigChangeForExistingChannelChange(item, itemListingUpdateRequestVo.getItemViewConfigs())) {
      this.productHelperService
          .updateItemViewConfigForExistingChannel(item, itemListingUpdateRequestVo.getItemViewConfigs());
      item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DATA_CHANGE);
      ItemViewConfig itemViewConfig = itemListingUpdateRequestVo.getItemViewConfigs().stream().findFirst().get();
      if ((itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) && item.isArchived()) {
        item.setArchived(false);
        product.setArchived(false);
      } else if ((itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) && !(product.getB2cActivated()
          && product.isOnline())) {
        product.setB2cActivated(true);
        product.setOnline(true);
      }
      itemDataChanged = true;
    }
    return itemDataChanged;
  }

  private boolean checkAndUpdateItemData(String storeId, Item item,
      ItemListingUpdateRequestVo itemListingUpdateRequestVo, boolean itemDataChanged) {
    if (StringUtils.isNotBlank(itemListingUpdateRequestVo.getMerchantSku()) && !StringUtils
        .equals(item.getMerchantSku(), itemListingUpdateRequestVo.getMerchantSku())) {
      item.setMerchantSku(itemListingUpdateRequestVo.getMerchantSku());
      itemService.updateItemPickupPointDeliveryFalse(storeId, item, new ArrayList<>());
      if (!itemDataChanged) {
        item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DATA_CHANGE);
      }
    }
    if (StringUtils.isNotBlank(itemListingUpdateRequestVo.getPickupPointCode()) && !StringUtils
        .equals(item.getPickupPointCode(), itemListingUpdateRequestVo.getPickupPointCode())) {
      item.getItemChangeEventTypes().add(ItemChangeEventType.SHIPPING_CHANGE);
      item.setPickupPointCode(itemListingUpdateRequestVo.getPickupPointCode());
      return true;
    }
    return false;
  }

  private List<String> upperCaseAll(List<String> productSkus) {
    for (int i = 0; i < productSkus.size(); i++) {
      productSkus.set(i, productSkus.get(i).toUpperCase());
    }
    return productSkus;
  }

  @Override
  public ItemSummaryPageResponseVo getCampaignItemSummaryByFilter(String storeId, String username,
      String requestId, CampaignItemSummaryRequestVO campaignItemSummaryRequestVO,
      PageRequest pageRequest) throws ApplicationException {
    checkArgument(campaignItemSummaryRequestVO != null, ItemSummaryServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemSummaryServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);

    List<String> categories;
    List<String> categoriesWithStoreId = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(campaignItemSummaryRequestVO.getCategories())){
      categories = campaignItemSummaryRequestVO.getCategories();
      for(String category : categories ){
        categoriesWithStoreId.add(String.format("%s%s%s", storeId, this.solrStringDelimiter, category));
      }
    }
    List<String> brands = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(campaignItemSummaryRequestVO.getBrands())){
      brands = campaignItemSummaryRequestVO.getBrands();
    }

    Page<ProductAndItemSolr> resultFromSolr = this.productAndItemSolrRepository
        .getProductsWithMerchantCodeAndMasterCatalogInAndBrandAndMarkForDeleteFalse(storeId,
            campaignItemSummaryRequestVO.getMerchantCode(), categoriesWithStoreId, brands,
            campaignItemSummaryRequestVO.getKeyword(),
            campaignItemSummaryRequestVO.getItemSku(),
            pageRequest);
    if (resultFromSolr != null) {
      if (CollectionUtils.isEmpty(resultFromSolr.getContent())) {
        return new ItemSummaryPageResponseVo(new ArrayList<>(), 0, 0);
      }
      List<String> distinctItemSkus = this.constructItemSkus(resultFromSolr.getContent());
      List<Item> itemsFromDb = this.itemService.getItemPriceAndViewConfigs(storeId, distinctItemSkus);
      List<ItemPickupPoint> itemPickupPoints =
          this.itemPickupPointService.findByItemSkusAndDelivery(storeId, distinctItemSkus, true);
      Map<String, Item> itemMap =
          itemsFromDb.stream().collect(Collectors.toMap(item -> item.getItemSku(), Function.identity()));
      Map<String, Set<Price>> mapOfPrices = itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
      Map<String, ItemPickupPoint> itemPickupPointMap =
          itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
      return this.constructItemSummaryResponseFromL5(resultFromSolr, mapOfPrices, null, null, itemPickupPointMap,
          new HashMap<>(), new HashMap<>(), itemMap);
    }
    throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
        ItemSummaryServiceImpl.ITEMS_NOT_FOUND);
  }

  @Override
  public List<ItemImagesListResponse> getItemImagesListResponse(String storeId, Set<String> itemSkus) throws Exception {
    List<ItemImagesListResponse> itemImagesListResponses = new ArrayList<>();
    if (pcbGetMainImageDataDisabled) {
      Map<String, String> itemCodeAndItemSkuMapForPcb = new HashMap<>();
      List<Item> itemList = itemService.getItemsByStoreIdAndItemSkus(storeId, itemSkus);
      Set<String> productSkus = itemList.stream().map(Item::getProductSku).filter(StringUtils::isNotBlank)
        .collect(Collectors.toSet());
      Map<String, Item> itemSkuAndItemMap =
          itemList.stream().collect(Collectors.toMap(Item::getItemSku, item -> item, (a, b) -> b));
      Map<String, Product> productSkuXProductMap = new HashMap<>();
      populateproductSkuXProductMap(storeId, productSkus, productSkuXProductMap);
      for (Item item : itemList) {
        if (StringUtils.isNotEmpty(item.getMainImageUrl())) {
          Product product = productSkuXProductMap.getOrDefault(item.getProductSku(), new Product());
          ItemImagesListResponse itemImagesListResponse = new ItemImagesListResponse();
          setItemAttributeDetail(item, itemImagesListResponse, product);
          itemImagesListResponse.setItemSku(item.getItemSku());
          itemImagesListResponse.setImagesResponseList(Collections.singletonList(
              ItemImagesResponse.builder().mainImage(Boolean.TRUE).locationPath(item.getMainImageUrl()).build()));
          itemImagesListResponses.add(itemImagesListResponse);
        } else {
          itemCodeAndItemSkuMapForPcb.put(item.getItemCode(), item.getItemSku());
        }
      }
      if (MapUtils.isNotEmpty(itemCodeAndItemSkuMapForPcb)) {
        getMainImageDataFromPcb(storeId, itemImagesListResponses, itemCodeAndItemSkuMapForPcb, itemSkuAndItemMap,
            productSkuXProductMap);
      }
    } else {
      Map<String, String> itemCodeAndItemSkuMap = itemService.findListOfItemCodesByItemSkus(storeId, itemSkus);
      getMainImageDataFromPcb(storeId, itemImagesListResponses, itemCodeAndItemSkuMap,
        new HashMap<>(), Collections.emptyMap());
    }
    return itemImagesListResponses;
  }

  private void populateproductSkuXProductMap(String storeId, Set<String> productSkus,
    Map<String, Product> productSkuXProductMap) {
    if (populateItemAttributeDetail) {
      productSkus.forEach(productSku -> {
        productSkuXProductMap.put(productSku,
          productService.getProductDeletedOrUndeleted(storeId, productSku));
      });
    }
  }

  private void setItemAttributeDetail(Item item, ItemImagesListResponse itemImagesListResponse, Product product) {
    if (populateItemAttributeDetail) {
      if (!item.isPermanentDelete()) {
        objectConverter.overrideDefiningAttributeDetailsFromL3ToL4(Collections.singletonList(product),
            Collections.singletonList(item));
      }
      List<ProductAttributeDetailDTO> productAttributeDetailDTOList = new ArrayList<>();
      for (ProductAttributeDetail productAttributeDetail : Optional.ofNullable(item.getDefiningAttributes())
          .orElse(new ArrayList<>())) {
        ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
        productAttributeDetailDTO.setAttributeName(productAttributeDetail.getAttributeName());
        productAttributeDetailDTO.setAttributeValue(productAttributeDetail.getAttributeValue());
        productAttributeDetailDTO.setAttributeCode(productAttributeDetail.getAttributeCode());
        productAttributeDetailDTOList.add(productAttributeDetailDTO);
      }
      itemImagesListResponse.setItemAttributeDetails(productAttributeDetailDTOList);
    }
  }

  private void getMainImageDataFromPcb(String storeId,
    List<ItemImagesListResponse> itemImagesListResponses, Map<String, String> itemCodeAndItemSkuMap,
    Map<String, Item> itemSkuAndItemMap, Map<String, Product> productSkuXProductMap) throws Exception {
    Map<String, MasterDataItem> masterDataItems =
        masterDataService.getMasterDataItems(storeId, Constants.DEFAULT_USERNAME, Constants.DEFAULT_REQUEST_ID,
            itemCodeAndItemSkuMap.keySet());
    if (populateItemAttributeDetail) {
      if (MapUtils.isEmpty(productSkuXProductMap)) {
        Set<String> productSkus =
          itemSkuAndItemMap.values().stream().filter(Objects::nonNull).map(Item::getProductSku)
            .collect(Collectors.toSet());
        productSkus.forEach(productSku -> productSkuXProductMap.put(productSku,
          productService.getProductDeletedOrUndeleted(storeId, productSku)));

      }
    }
    for (Map.Entry<String, MasterDataItem> masterDataItemEntry : masterDataItems.entrySet()) {
      ItemImagesListResponse itemImagesListResponse = new ItemImagesListResponse();
      for (MasterDataItemImage masterDataItemImage : masterDataItemEntry.getValue().getMasterDataItemImages()) {
        if (masterDataItemImage.isMainImage() && CollectionUtils.isEmpty(itemImagesListResponse.getImagesResponseList())) {
          itemImagesListResponse.setItemSku(itemCodeAndItemSkuMap.get(masterDataItemEntry.getValue().getSkuCode()));
          itemImagesListResponse.setImagesResponseList(Collections.singletonList(
              ItemImagesResponse.builder().mainImage(masterDataItemImage.isMainImage()).locationPath(masterDataItemImage.getLocationPath()).sequence(masterDataItemImage.getSequence())
                  .build()));
        }
      }
      if (MapUtils.isNotEmpty(itemSkuAndItemMap) && MapUtils.isNotEmpty(productSkuXProductMap)) {
        Product product =
          productSkuXProductMap.values().stream().filter(Objects::nonNull).filter(productData -> productData.getProductCode()
            .equals(masterDataItemEntry.getValue().getProductCode())).findFirst()
          .orElse(new Product());
        setItemAttributeDetail(itemSkuAndItemMap.get(itemCodeAndItemSkuMap.get(masterDataItemEntry.getValue().getSkuCode())),
            itemImagesListResponse, product);
      }
      itemImagesListResponses.add(itemImagesListResponse);
    }
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailsByProductSku(String storeId, String productSku) {
    List<Item> itemList =
        cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    Map<String, Boolean> productSkuAndSharedProductMap = new HashMap<>();
    if(CollectionUtils.isNotEmpty(itemList)) {
      productSkuAndSharedProductMap = itemService.getProductCodeAndSharedProductMap(itemList);
    }
    return ResponseHelper.toItemBasicDetailV2Response(itemList, new HashMap<>(), new HashMap<>(),
        getBundleRecipeByItemSkus(itemList, storeId), new HashMap<>(), productSkuAndSharedProductMap);
  }


  @Override
  public List<ItemBasicDetailV2Response> getBulkItemDetailsByItemSkus(String storeId, boolean fetchBundleRecipe,
      List<String> itemSkus) {
    List<Item> itemList = itemService.getItemsByStoreIdAndItemSkus(storeId, new HashSet<>(itemSkus));
    Map<String, Boolean> productSkuAndSharedProductMap = itemService.getProductCodeAndSharedProductMap(itemList);
    if (fetchBundleRecipe) {
      return ResponseHelper.toItemBasicDetailV2Response(itemList, new HashMap<>(), new HashMap<>(),
          getBundleRecipeByItemSkus(itemList, storeId), new HashMap<>(), productSkuAndSharedProductMap);
    }
    return ResponseHelper.toItemBasicDetailV2Response(itemList, new HashMap<>(), new HashMap<>(), new HashMap<>(),
        new HashMap<>(), productSkuAndSharedProductMap);
  }

  private Map<String, Item> getBundleRecipeByItemSkus(List<Item> itemList, String storeId) {
    Map<String, Item> bundleItemSkuToItemMap = new HashMap<>();
    Set<String> bundleItemSkus = itemList.stream().filter(item -> Objects.nonNull(item.getBundleRecipe()))
        .flatMap(item -> item.getBundleRecipe().stream().map(BundleRecipe::getItemSku)).collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(bundleItemSkus)) {
      bundleItemSkuToItemMap =
          Optional.of(itemService.getItemsByStoreIdAndItemSkus(storeId, bundleItemSkus)).orElse(new ArrayList<>())
              .stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    }
    return bundleItemSkuToItemMap;
  }


  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailsByItemCodes(String storeId, List<String> itemCodesList) {
    List<Item> itemList = new ArrayList<>();
    Set<String> itemCodes = new HashSet<>(itemCodesList);
    if (CollectionUtils.isNotEmpty(itemCodes)) {
      itemList = itemHelperService.getBasicItemDetailsByItemCodes(storeId, itemCodes);
    }
    return ResponseHelper.toItemBasicDetailV2Response(itemList, new HashMap<>(), new HashMap<>(), new HashMap<>(),
        new HashMap<>(), new HashMap<>());
  }

  @Override
  public List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(String storeId, boolean inAllProducts,
      boolean needProductData, boolean needCategoryData, List<String> itemSkus, boolean needFbbFlag,
      boolean needAttributeData) throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_LIST_NOT_EMPTY);
    GdnPreconditions.checkArgument(itemSkus.size() < getItemBasicDetailApiItemSkuListSize,
        String.format(ErrorMessages.ITEM_SKU_LIST_SIZE_LIMIT_EXCEEDED_ERROR, getItemBasicDetailApiItemSkuListSize));
    Set<String> itemSkusSet = new HashSet<>(itemSkus);
    List<Item> itemList = new ArrayList<>();
    HashMap<String,ProductDataResponse> productSkuToProductDataMap = new HashMap<>();
    Map<String, List<CategoryResponse>> categoryHierarchyMap = new HashMap<>();
    if (inAllProducts) {
      itemList = itemService.getItemsByStoreIdAndItemSkus(storeId, itemSkusSet);
    } else {
      for (String itemSku : itemSkusSet) {
        Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
        if (Objects.nonNull(item)) {
          itemList.add(item);
        }
      }
    }
    Map<String, Boolean> itemSkuFbbMap = new HashMap<>();
    if (needFbbFlag) {
      itemSkuFbbMap =
          Optional.ofNullable(itemPickupPointService.getItemSkuFbbMap(storeId, itemSkusSet)).orElse(new HashMap<>());
    }
    if (needCategoryData) {
      categoryHierarchyMap = getCategoryHierarchyMap(itemList);
    }
    if (needProductData) {
      getProductSkuToProductDataMap(storeId, inAllProducts, itemList, productSkuToProductDataMap, needAttributeData);
    }
    return ResponseHelper.toItemBasicDetailV2Response(itemList, productSkuToProductDataMap, categoryHierarchyMap,
        new HashMap<>(), itemSkuFbbMap, new HashMap<>());
  }

  private String getAttributeValuesForHalalAndStorage(String productCode,
    List<String> attributeCodes, boolean inAllProducts) {
    try {
      ProductAndAttributeDetailResponse productAndAttributeDetailResponse =
        productCategoryBaseOutbound.getProductAndAttributeDetails(productCode, inAllProducts);
      return CommonUtil.getAttributeValue(productAndAttributeDetailResponse, attributeCodes);
    } catch (Exception e) {
      log.error("Error Fetching Attribute Detail Response from PCB for productCode : {} , Error : ",
        productCode, e);
      return null;
    }
  }

  private void getProductSkuToProductDataMap(String storeId, boolean inAllProducts, List<Item> itemList,
      HashMap<String, ProductDataResponse> productSkuToProductDataMap, boolean needAttributeData) throws Exception {
    Set<String> productSkus = Optional.ofNullable(itemList).orElse(new ArrayList<>()).stream()
        .map(item -> Optional.ofNullable(item.getProductSku()).orElse(StringUtils.EMPTY)).collect(Collectors.toSet());
    productSkus.removeIf(String::isEmpty);
    List<Product> productList = inAllProducts ? this.productService.getAllProducts(storeId, productSkus, false) :
        this.productService.getProducts(storeId, productSkus);
    Optional.ofNullable(productList).orElse(new ArrayList<>()).stream().filter(Objects::nonNull).forEach(
        product -> productSkuToProductDataMap.put(product.getProductSku(),
            new ProductDataResponse(product.getProductCode(), product.getProductType(), product.getProductName(),
                product.isSuspended(), needAttributeData ?
                getAttributeValuesForHalalAndStorage(product.getProductCode(), halalAttributesValues, inAllProducts) :
                StringUtils.EMPTY, needAttributeData ?
                getAttributeValuesForHalalAndStorage(product.getProductCode(), storageAttributesValues, inAllProducts) :
                StringUtils.EMPTY, getProductAttributeDTO(
                Optional.ofNullable(product.getDefiningAttributes()).orElse(new ArrayList<>())))));
  }

  public List<ProductAttributeDTO> getProductAttributeDTO(List<ProductAttribute> definingAttributes) {
    return definingAttributes.stream()
        .map(productAttribute -> gdnMapper.deepCopy(productAttribute, ProductAttributeDTO.class))
        .collect(Collectors.toList());
  }

  private Map<String, List<CategoryResponse>> getCategoryHierarchyMap(List<Item> itemList) {
    Set<String> categoryCodes =
        Optional.ofNullable(itemList).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
            .map(item -> Optional.ofNullable(item.getCategoryCode()).orElse(StringUtils.EMPTY))
            .collect(Collectors.toSet());
    return this.cachedService.getParentCategoriesFromDbAndCache(GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes);
  }
}
