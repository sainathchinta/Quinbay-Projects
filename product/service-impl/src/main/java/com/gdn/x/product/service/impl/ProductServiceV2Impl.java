package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.product.enums.Constants.LOCALE_COUNTRY;
import static com.gdn.x.product.enums.Constants.LOCALE_LANGUAGE;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryUpdateRequest;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.DimensionsAndUOMResponse;
import com.gdn.x.product.rest.web.model.response.DistributionInfoByOmniChannelSkusResponse;
import com.gdn.x.product.rest.web.model.response.DistributionItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.OmniChannelSkuDetailResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import com.gdn.x.product.rest.web.model.request.DistributionInfoByOmniChannelSkusRequest;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductServiceV2;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ProductAttributesUtil;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductServiceV2Impl implements ProductServiceV2 {

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductSearchHelperService productSearchHelperService;

  @Autowired
  private ItemPriceService itemPriceService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private PristineCacheableService pristineCacheableService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ProductAttributesUtil productAttributesUtil;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private GdnMapper gdnMapper;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ProductSolrRepository productSolrRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  private String minMaxPriceRangePattern = "%s - %s";

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${exclude.hide.on.customer.side.attributes.enabled}")
  private boolean excludeHideOnCustomerSideAttributesEnabled;

  @Value("#{'${halal.attributes.values}'.split(',')}")
  private List<String> halalAttributesValues;

  @Value("#{'${storage.attributes.values}'.split(',')}")
  private List<String> storageAttributesValues;

  @Value("${omni.channel.sku.max.request.size}")
  private int omniChannelSkuMaxRequestSize;

  @Override
  public DistributionInfoByOmniChannelSkusResponse checkOmniChannelSkusInSeller(String storeId, String requestId,
      String username, DistributionInfoByOmniChannelSkusRequest request) throws ApplicationException {

    GdnPreconditions.checkArgument(Objects.nonNull(request), ErrorMessages.INVALID_ITEM_SUMMARY_REQUEST);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getOmnichannelSkuCodes()),
        ErrorMessages.INVALID_ITEM_SUMMARY_REQUEST);
    GdnPreconditions.checkArgument(request.getOmnichannelSkuCodes().size() <= omniChannelSkuMaxRequestSize,
        ErrorMessages.ITEM_LIST_TOO_LARGE);
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse =
        productCategoryBaseOutbound.checkOmniChannelSkuExistsInSeller(storeId, requestId, username, true,
            new OmniChannelSkuRequest(request.getSellerCode(), request.getOmnichannelSkuCodes()));

    Set<String> skuCodes = new HashSet<>();
    Set<String> productCodes = new HashSet<>();
    Map<String, ProductL1AndL2CodeResponse> skuCodeAndProductL1AndL2CodeResponseMap = new HashMap<>();
    getRequiredData(validOmniChannelSkuResponse, skuCodes, skuCodeAndProductL1AndL2CodeResponseMap, productCodes);
    List<Item> items = itemService.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(storeId, skuCodes);
    Map<String, Item> itemMap = items.stream().collect(toMap(Item::getItemCode, Function.identity()));

    Map<String, ProductAndAttributeDetailResponse> productAndAttributeDetailResponseMap =
        getProductAndAttributeDetailResponseMap(productCodes);

    List<OmniChannelSkuDetailResponse> omniChannelSkuDetailResponses =
        getOmniChannelSkuDetailResponses(skuCodeAndProductL1AndL2CodeResponseMap, itemMap,
            productAndAttributeDetailResponseMap);

    setCategoryName(omniChannelSkuDetailResponses);
    setInsuredAmount(storeId, items, omniChannelSkuDetailResponses);
    return new DistributionInfoByOmniChannelSkusResponse(omniChannelSkuDetailResponses);
  }

  private void setInsuredAmount(String storeId, List<Item> items,
      List<OmniChannelSkuDetailResponse> omniChannelSkuDetailResponses) {
    List<ItemPickupPoint> distributionPickupPoints =
        itemPickupPointService.findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(storeId,
            items.stream().map(Item::getItemSku).collect(toList()));
    Map<String, List<ItemPickupPoint>> distributionL5sPerItem =
        distributionPickupPoints.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
    for (OmniChannelSkuDetailResponse omniChannelSkuDetailResponse : omniChannelSkuDetailResponses) {
      List<CogsResponse> cogsResponseList = new ArrayList<>();
      for (ItemPickupPoint itemPickupPoint : distributionL5sPerItem.getOrDefault(
          omniChannelSkuDetailResponse.getItemSku(), new ArrayList<>())) {
        omniChannelSkuDetailResponse.setDistribution(true);
        CogsResponse cogsResponse = new CogsResponse();
        cogsResponse.setItemSku(itemPickupPoint.getItemSku());
        cogsResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        cogsResponse.setInsuredAmount(itemPickupPoint.getInsuredAmount());
        cogsResponseList.add(cogsResponse);
      }
      omniChannelSkuDetailResponse.setPickupPointResponse(cogsResponseList);
    }
  }

  private void setCategoryName(List<OmniChannelSkuDetailResponse> omniChannelSkuDetailResponses) {
    CategoryNamesResponse categoryNamesResponse = productCategoryBaseOutbound.getCategoryNames(new ArrayList<>(
        omniChannelSkuDetailResponses.stream().map(OmniChannelSkuDetailResponse::getCategoryCode)
            .filter(StringUtils::isNotBlank).collect(Collectors.toSet())));

    if (Objects.nonNull(categoryNamesResponse) && MapUtils.isNotEmpty(categoryNamesResponse.getCategoryMap())) {
      omniChannelSkuDetailResponses.forEach(
          omniChannelSkuDetailResponse -> omniChannelSkuDetailResponse.setCategoryName(
              categoryNamesResponse.getCategoryMap().get(omniChannelSkuDetailResponse.getCategoryCode())));
    }
  }

  private static void getRequiredData(ValidOmniChannelSkuResponse validOmniChannelSkuResponse, Set<String> skuCodes,
      Map<String, ProductL1AndL2CodeResponse> skuCodeAndProductL1AndL2CodeResponseMap, Set<String> productCodes) {
    if (Objects.nonNull(validOmniChannelSkuResponse) && MapUtils.isNotEmpty(
        validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap())) {
      for (Map.Entry<String, ProductL1AndL2CodeResponse> entry : validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap()
          .entrySet()) {
        skuCodes.add(entry.getValue().getSkuCode());
        skuCodeAndProductL1AndL2CodeResponseMap.putIfAbsent(entry.getValue().getSkuCode(), entry.getValue());
        productCodes.add(entry.getValue().getProductCode());
      }
    }
  }

  private Map<String, ProductAndAttributeDetailResponse> getProductAndAttributeDetailResponseMap(
      Set<String> productCodes) {
    Map<String, ProductAndAttributeDetailResponse> productAndAttributeDetailResponseMap = new HashMap<>();
    List<ProductAndAttributeDetailResponse> productAndAttributeDetailResponses = productCodes.stream()
        .map(productCode -> productCategoryBaseOutbound.getProductAndAttributeDetails(productCode, true)).toList();
    productAndAttributeDetailResponseMap = productAndAttributeDetailResponses.stream()
        .collect(Collectors.toMap(ProductAndAttributeDetailResponse::getProductCode, Function.identity(), (a, b) -> a));
    return productAndAttributeDetailResponseMap;
  }

  private List<OmniChannelSkuDetailResponse> getOmniChannelSkuDetailResponses(
      Map<String, ProductL1AndL2CodeResponse> skuCodeAndProductL1AndL2CodeResponseMap, Map<String, Item> itemMap,
      Map<String, ProductAndAttributeDetailResponse> productAndAttributeDetailResponseMap) {
    List<OmniChannelSkuDetailResponse> omniChannelSkuDetailResponses = new ArrayList<>();
    for (Map.Entry<String, ProductL1AndL2CodeResponse> response : skuCodeAndProductL1AndL2CodeResponseMap.entrySet()) {
      ProductL1AndL2CodeResponse pcbResponse = response.getValue();
      Item item = Optional.ofNullable(itemMap.get(response.getKey())).orElse(new Item());
      OmniChannelSkuDetailResponse omniChannelSkuDetailResponse =
          getOmniChannelSkuDetailResponse(response, pcbResponse, item);
      omniChannelSkuDetailResponse.setHalal(
          CommonUtil.getAttributeValue(productAndAttributeDetailResponseMap.get(pcbResponse.getProductCode()),
              halalAttributesValues));
      omniChannelSkuDetailResponse.setStorage(
          CommonUtil.getAttributeValue(productAndAttributeDetailResponseMap.get(pcbResponse.getProductCode()),
              storageAttributesValues));
      omniChannelSkuDetailResponses.add(omniChannelSkuDetailResponse);
    }
    return omniChannelSkuDetailResponses;
  }

  private static OmniChannelSkuDetailResponse getOmniChannelSkuDetailResponse(
      Map.Entry<String, ProductL1AndL2CodeResponse> response, ProductL1AndL2CodeResponse pcbResponse, Item item) {
    OmniChannelSkuDetailResponse omniChannelSkuDetailResponse = new OmniChannelSkuDetailResponse();
    omniChannelSkuDetailResponse.setSkuCode(response.getKey());
    omniChannelSkuDetailResponse.setItemName(pcbResponse.getItemName());
    omniChannelSkuDetailResponse.setCategoryCode(item.getCategoryCode());
    omniChannelSkuDetailResponse.setOmniChannelSku(pcbResponse.getOmniChannelSku());
    omniChannelSkuDetailResponse.setProductSku(item.getProductSku());
    omniChannelSkuDetailResponse.setItemSku(item.getItemSku());
    omniChannelSkuDetailResponse.setMainImageUrl(item.getMainImageUrl());
    omniChannelSkuDetailResponse.setDangerousLevel(Optional.ofNullable(item.getDangerousLevel()).orElse(0));
    if (Objects.nonNull(pcbResponse.getDistributionItemInfoResponse())) {
      DistributionItemInfoResponse distributionItemInfoResponse = new DistributionItemInfoResponse();
      BeanUtils.copyProperties(pcbResponse.getDistributionItemInfoResponse(), distributionItemInfoResponse);
      omniChannelSkuDetailResponse.setDistributionItemInfo(distributionItemInfoResponse);
      List<DimensionsAndUOMResponse> dimensionsAndUOMResponses = new ArrayList<>();
      for (DimensionsAndUomResponse dimensionsAndUomResponse : pcbResponse.getDimensionsAndUomResponse()) {
        DimensionsAndUOMResponse dimensionsAndUOMResponse = new DimensionsAndUOMResponse();
        BeanUtils.copyProperties(dimensionsAndUomResponse, dimensionsAndUOMResponse);
        dimensionsAndUOMResponses.add(dimensionsAndUOMResponse);
      }
      omniChannelSkuDetailResponse.setDimensionsAndUOMResponse(dimensionsAndUOMResponses);
    }
    return omniChannelSkuDetailResponse;
  }

  @Override
  public ProductItemsVo getProductAndSingleItemByItemSkuAndPickupPointCode(String storeId, String username,
      String requestId, String itemSku, String pickupPointCode, boolean includeMarkForDelete) throws Exception {
    checkArgument(this.skuValidator.isItemSku(itemSku), ErrorMessages.INVALID_ITEM_SKU_FORMAT + itemSku);

    ItemPickupPoint itemPickupPoint =
        findItemPickupPointByItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode, includeMarkForDelete);
    checkArgument(Objects.nonNull(itemPickupPoint),
        String.format(ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, itemSku,
            pickupPointCode));

    Item item = findItemByItemSku(storeId, itemSku, includeMarkForDelete);
    checkArgument(Objects.nonNull(item), String.format(ErrorMessages.CANNOT_FIND_ITEM_BY_ITEM_SKU, itemSku));
    if (item.isPermanentDelete()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.ITEM_DELETED.getErrorMessage(),
          ApiErrorCodes.ITEM_DELETED.getErrorCode());
    }
    item.setUniqueId(item.getItemSku());

    Product product = findProductByProductSku(storeId, itemPickupPoint.getProductSku(), includeMarkForDelete);
    checkArgument(Objects.nonNull(product),
        String.format(ErrorMessages.CANNOT_FIND_PRODUCT_BY_PRODUCT_SKU, itemPickupPoint.getProductSku()));

    itemPriceService.getDiscountItemPickupPoint(Arrays.asList(itemPickupPoint));
    productHelperService.findAndConstructOfflineItemsByPickupPointCode(storeId, Arrays.asList(item), pickupPointCode);
    constructMasterDataAndSetItemCatalogs(storeId, username, requestId, product, item);
    ProductItemsVo productItemsVo =
        ProductAndItemsUtil.toProductItemsVo(product, item, itemPickupPoint, true, true);
    productService.setSellerPromoBundlings(storeId, Arrays.asList(productItemsVo));

    return productItemsVo;
  }

  @Override
  public ProductItemsVo getProductAndSingleItemByItemSku(String storeId, String username, String requestId,
      String itemSku, boolean includeMarkForDelete) throws Exception {
    checkArgument(this.skuValidator.isItemSku(itemSku), ErrorMessages.INVALID_ITEM_SKU_FORMAT + itemSku);

    Item item = findItemByItemSku(storeId, itemSku, includeMarkForDelete);
    checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    item.setUniqueId(item.getItemSku());

    Product product = findProductByProductSku(storeId, item.getProductSku(), includeMarkForDelete);
    checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);

    constructMasterDataAndSetItemCatalogs(storeId, username, requestId, product, item);

    return ProductAndItemsUtil.toProductItemsVo(product, item, null, false, false);
  }

  @Override
  public List<ProductItemsVo> getProductAndItemsByItemSkus(String storeId, String username, String requestId,
      Set<String> itemSkus, boolean pristine, boolean off2On) throws Exception {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    List<ProductItemsVo> productItemsVos = new ArrayList<>();
    for (String itemSku : itemSkus) {
      ProductItemsVo productItemsVo = getProductAndSingleItemByItemSku(storeId, username, requestId, itemSku, false);
      if (off2On && !productItemsVo.getProductVo().isOff2OnChannelActive()) {
        continue;
      }
      if (pristine) {
        addPristineListingAttributesToItems(storeId, productItemsVo);
      }
      productItemsVos.add(productItemsVo);
    }
    return productItemsVos;
  }

  private void addPristineListingAttributesToItems(String storeId,
      ProductItemsVo productItemsVo) throws Exception {
    Item item = productItemsVo.getItemVoList().get(0);
    if (Objects.nonNull(item.getPristineDataItem())) {
      PristineItemAndSiblingsVO pristineItemAndSiblingsVO =
          pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(storeId,
              item.getPristineDataItem().getPristineId());
      List<PristineDataItem> pristineDataItems = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(pristineItemAndSiblingsVO.getSiblings())) {
        for (PristineDataItem pristineItem : pristineItemAndSiblingsVO.getSiblings()) {
          if (StringUtils.isBlank(pristineItem.getPristineBrand()) && StringUtils.isBlank(
              pristineItem.getPristineModel())) {
            Item item1 = pristineCacheableService.findFirstItemByPristine(pristineItem);
            if (Objects.nonNull(item1)) {
              item1 = masterDataConstructorService.constructPristineDataItemWithMasterData(item1);
              pristineItem = item1.getPristineDataItem();
              pristineDataItems.add(pristineItem);
            }
          } else {
            pristineDataItems.add(pristineItem);
          }
        }
      }
      List<ProductAttribute> pristineAttributes =
          pristineDataItems.stream().map(objectConverterService::convertPristineAttributeToProductAttribute)
              .collect(toList());
      productItemsVo.getProductVo().setDefiningAttributes(pristineAttributes);
      if (StringUtils.isBlank(item.getPristineDataItem().getPristineBrand()) && StringUtils.isBlank(
          item.getPristineDataItem().getPristineModel())) {
        item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
      }
      item.getPristineDataItem().setPristineListingAttributes(
          productAttributesUtil.translatePristineListingAttributeName(
              item.getPristineDataItem().getPristineListingAttributes(),
              productAttributesUtil.getCategoryListingParameterKey(item.getPristineDataItem())));
    }
  }

  private void constructMasterDataAndSetItemCatalogs(String storeId, String username, String requestId,
      Product product, Item item) throws Exception {
    ProductAndItemsVO productAndItemsVO =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(storeId, username, requestId, product,
            Arrays.asList(item), isProductVisibilityEnabled, false);
    productSearchHelperService.setItemCatalogs(storeId, username, requestId, true,
        Collections.singletonList(productAndItemsVO),
        Collections.singletonMap(productAndItemsVO.getProduct().getProductCode(),
            productAndItemsVO.getProduct().getMasterDataProduct()));
  }


  private ItemPickupPoint findItemPickupPointByItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode, boolean includeMarkForDelete) {
    if (includeMarkForDelete) {
      return itemPickupPointService.findByItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode);
    } else {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode);
      return Objects.nonNull(itemPickupPoint) && itemPickupPoint.isMarkForDelete() ? null : itemPickupPoint;
    }
  }

  private Item findItemByItemSku(String storeId, String itemSku, boolean includeMarkForDelete) {
    if (includeMarkForDelete) {
      return cacheItemHelperService.findCacheableByStoreIdAndItemSku(storeId, itemSku);
    } else {
      return cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    }
  }

  private Product findProductByProductSku(String storeId, String productSku, boolean includeMarkForDelete) {
    if (includeMarkForDelete) {
      return productCacheableService.findProductByStoreIdAndProductSku(storeId, productSku);
    } else {
      return productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    }
  }

  @Override
  public void updateCncFlagAtProductAndItemLevel(String storeId, String username, List<String> itemSkus) throws Exception {
    Set<String> productSkus = new HashSet<>();
    List<Item> itemList = new ArrayList<>();
    for (String itemSku : itemSkus) {
      ItemPickupPoint itemPickupPointFromDb = itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(
          storeId, itemSku, Boolean.TRUE);
      Item item = itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
      if (Objects.nonNull(item) && isCncUpdateRequired(itemPickupPointFromDb, item.isCncActivated())) {
        Item itemUpdated = itemService.updateCncActivated(storeId, itemSku, !item.isCncActivated(), username);
        itemList.add(itemUpdated);
        productSkus.add(item.getProductSku());
      }
    }
    if (CollectionUtils.isNotEmpty(itemList)) {
      productAndItemSolrIndexerService.applyItems(itemList);
    }
    for (String productSku : productSkus) {
      ItemPickupPoint itemPickupPointFromDb = itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(
          storeId, productSku, Boolean.TRUE);
      Product product = productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
      if (isCncUpdateRequired(itemPickupPointFromDb, product.isCncActivated())) {
        productService.updateCncActivatedByProductSkuAndSolrL3(storeId, new HashSet<>(Arrays.asList(productSku)),
            !product.isCncActivated(), username);
      }
    }
  }

  @Override
  public ProductAndItemsSummaryResponseV2 getProductAndItemsForView(String storeId, String requestId,
    String username, String productSku, String pickupPointCode, boolean showDeleted,
    boolean combineOthersBundlings, boolean off2On, boolean needProductData,
    boolean includeForceReview) throws Exception {
    checkArgument(this.skuValidator.isProductSku(productSku),
      ErrorMessages.INVALID_ITEM_SKU_FORMAT.concat(Constants.SPACE).concat(productSku));
    Product product =
      this.productService.getProductForView(storeId, requestId, username, productSku, true,
        showDeleted);
    List<ItemPickupPoint> itemPickupPoints =
      itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(storeId, productSku,
        pickupPointCode);
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints),
      ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE.concat(
        Constants.SPACE).concat(productSku).concat(Constants.HYPHEN).concat(pickupPointCode));
    Map<String, ItemPickupPoint> itemPickupPointMap =
      itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku,
        Function.identity()));
    List<Item> items = itemService.getItemsForViewByProductSkuAndPickUpPoint(storeId, productSku,
      showDeleted, combineOthersBundlings, off2On, itemPickupPointMap);
    checkArgument(CollectionUtils.isNotEmpty(items),
      String.format(ErrorMessages.CANNOT_FIND_ITEM_BY_PRODUCT_SKU_AND_L5_MAP,productSku,
        pickupPointCode));
    items = itemService.getItemsWithDiscountAndPickUpPointDetails(storeId, productSku, pickupPointCode, items, itemPickupPoints, itemPickupPointMap, false);
    Map<String, Item> itemMap =
      items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    return objectConverterService.constructProductAndItemsForView(product,itemMap,
      itemPickupPointMap);
  }

  public boolean isCncUpdateRequired(ItemPickupPoint itemPickupPoint, boolean cncActivated) {
    return ((Objects.isNull(itemPickupPoint) && cncActivated) || (Objects.nonNull(itemPickupPoint)
        && !cncActivated));
  }

  @Override
  public DuplicateProductDetailsResponse validateDuplicateProductBySellerSku(String storeId, String merchantCode, String sellerSku) {
    Item item = itemService.getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(storeId, merchantCode, sellerSku);
    if (Objects.nonNull(item)) {
      Product product = productService.findByStoreIdAndProductSku(storeId, item.getProductSku());
      return objectConverterService.convertProductToDuplicateProductDetailsResponse(product);
    } else {
      List<Product> products = productService.getProductsByStoreIdAndMerchantCodeAndIsSuspended(storeId, merchantCode, true);
      if (Objects.nonNull(products)) {
        List<String> productSkus = products.stream().map(Product::getProductSku).collect(toList());
        item = itemService.getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(storeId, merchantCode,
            productSkus, sellerSku);
        if (Objects.nonNull(item)) {
          Map<String, Product> productMap =
              products.stream().collect(toMap(Product::getProductSku, Function.identity()));
          return objectConverterService.convertProductToDuplicateProductDetailsResponse(productMap.get(item.getProductSku()));
        }
      }
    }
    return null;
  }

  @Override
  public List<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(String storeId,
      ProductSkuAndProductCodeRequest request) {
    List<PrdProductResponse> prdProductResponseList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    if (StringUtils.isNotEmpty(request.getProductSku())) {
      Product product = productCacheableService.findProductByStoreIdAndProductSku(storeId, request.getProductSku());
      productList.add(product);
    } else if (StringUtils.isNotEmpty(request.getProductCode())) {
      productList = productService.findByStoreIdAndProductCode(storeId, request.getProductCode());
    }
    for (Product product : productList) {
      if (Objects.nonNull(product)) {
        PrdProductResponse prdProductResponse = gdnMapper.deepCopy(product, PrdProductResponse.class);
        prdProductResponse.setMasterCatalog(ProductAndItemsUtil.convertToMasterCatalogResponse(product.getMasterCatalog()));
        prdProductResponseList.add(prdProductResponse);
      }
    }
    return prdProductResponseList;
  }

  @Override
  public BasicProductAndItemDTO getBasicProductAndItemDetails(String storeId, String username, String requestId,
      String itemSku, String pickupPointCode, boolean specificationNeeded, boolean descriptionNeeded) throws Exception {
    BasicItemDTO basicItemDTO = itemService.getbasicItemDetails(storeId, itemSku, pickupPointCode);
    Product product = productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, basicItemDTO.getProductSku());
    GdnPreconditions.checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    List<ItemCatalogVO> itemCatalogVOS = catalogService.getItemCatalogsByProduct(username, requestId, product);
    BasicProductAndItemDTO basicProductAndItemDTO = objectConverterService.convertToBasicProductAndItemDTO(product,
        itemCatalogVOS, basicItemDTO);
    if (!product.isSynchronized()) {
      basicProductAndItemDTO.setMasterDataProductAttributes(product.getMasterDataProduct().getMasterDataProductAttributes());
      basicProductAndItemDTO.getMasterDataProduct().setDescription(product.getMasterDataProduct().getDescription());
    } else if (specificationNeeded) {
      ProductAndAttributeDetailResponse productAndAttributeDetailResponse =
          productCategoryBaseOutbound.getProductAndAttributeDetails(product.getProductCode(), false);
      if (excludeHideOnCustomerSideAttributesEnabled) {
        List<ProductAttributeResponse> originalProductAttributeResponseList =
            productAndAttributeDetailResponse.getProductAttributeResponses();
        try {
          Optional.ofNullable(originalProductAttributeResponseList).orElse(new ArrayList<>()).removeIf(
              productAttributeResponse -> Objects.nonNull(productAttributeResponse) && Objects.nonNull(
                  productAttributeResponse.getAttribute()) && productAttributeResponse.getAttribute()
                  .isHideForCustomer());
        } catch (Exception e) {
          log.error("Error during removal of hide from customer side attributes for product code : {} ",
              product.getProductCode(), e);
          productAndAttributeDetailResponse.setProductAttributeResponses(originalProductAttributeResponseList);
        }
      }
      basicProductAndItemDTO.setMasterDataProductAttributes(objectConverterService
          .convertToMasterDataProductAttribute(productAndAttributeDetailResponse));
      basicProductAndItemDTO.getMasterDataProduct().setDescription(objectConverterService.convertBytesToString(
              productAndAttributeDetailResponse.getDescription()));
    } else if (descriptionNeeded) {
      ProductResponse productResponse = productCategoryBaseOutbound.getProductBasicDetails(product.getProductCode());
      basicProductAndItemDTO.getMasterDataProduct().setDescription(objectConverterService.convertBytesToString(
          productResponse.getDescription()));
    }
    return basicProductAndItemDTO;
  }

  @Override
  public List<PriceRangeResponse> getPriceRangeForSkus(String storeId, String merchantCode, List<String> skuList) {
    Set<String> skuSet = new HashSet<>(skuList);
    if (isValid(skuSet, Boolean.TRUE)) {
      return constructPriceRangeResponseForProductSkuList(storeId, merchantCode, skuSet);
    } else if (isValid(skuSet, Boolean.FALSE)) {
      return constructPriceRangeResponseForItemSkuList(storeId, skuList);
    } else {
      log.error("#ProductServiceV2Impl getPriceRangeForSkus invalid skus {}", skuList);
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_SKU.getErrorMessage(),
          ApiErrorCodes.INVALID_SKU.getErrorCode());
    }
  }

  @Override
  public List<HalalProductResponse> getHalalProductResponseByProductSkus(String storeId, List<String> productSkuList) {
    List<Product> productList = new ArrayList<>();
    List<HalalProductResponse> halalProductResponseList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productSkuList)) {
      productList = productService.findByStoreIdAndProductSkuIn(storeId,
          productSkuList.stream().distinct().collect(Collectors.toList()));
    }
    if (CollectionUtils.isNotEmpty(productList)) {
      halalProductResponseList = productList.stream().filter(Objects::nonNull).map(
          product -> HalalProductResponse.builder().productSku(product.getProductSku())
              .productName(product.getProductName()).productCode(product.getProductCode()).brand(product.getBrand()).halalProduct(
                  Optional.ofNullable(product.getCurationStatus()).orElse(CurationStatus.NONE).getValue()
                      == (CurationStatus.APPROVED.getValue()))
              .curationStatus(Optional.ofNullable(product.getCurationStatus()).orElse(CurationStatus.NONE).name())
              .productCreationDate(product.getCreatedDate()).build()).collect(Collectors.toList());
    }
    return halalProductResponseList;
  }

  @Override
  public void updateHalalConfigOfProduct(String storeId, String productSku, String curationStatus, String userName) {
    if (!CurationStatus.validCurationStatus(curationStatus)) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.CURATION_STATUS_INVALID.getErrorMessage(),
          ApiErrorCodes.CURATION_STATUS_INVALID.getErrorCode());
    }
    Product product = productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    if (Objects.isNull(product)) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_PRODUCT.getErrorMessage(),
          ApiErrorCodes.INVALID_PRODUCT.getErrorCode());
    }
    //to use this api to approve existing products where curation status can be null
    if (Objects.isNull(product.getCurationStatus())) {
      product.setCurationStatus(CurationStatus.NEED_CURATION);
    }
    CategoryDetailResponse categoryDetailResponse =
        this.productCategoryBaseOutbound.getCategoryDetailByCategoryCode(Constants.DEFAULT_REQUEST_ID,
            Constants.DEFAULT_USERNAME, product.getCategoryCode());
    SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest = new SalesCategoryMappingUpdateRequest();
    HalalHistoryUpdateEventModel halalHistoryUpdateEventModel = null;
    if (CurationStatus.APPROVED.name().equals(curationStatus)) {
      halalHistoryUpdateEventModel =
          CommonUtil.getHalalHistoryUpdateEventModel(curationStatus, product, halalHistoryUpdateEventModel, storeId,
              userName);
      product.setCurationStatus(CurationStatus.APPROVED);
      for (CategoryReferenceResponse categoryReferenceResponse : categoryDetailResponse.getHalalSalesCategoryReferences()) {
        SalesCategoryUpdateRequest addSalesCategoryUpdateRequest =
            CommonUtil.getSalesCategoryUpdateRequest(categoryReferenceResponse);
        if (Objects.nonNull(addSalesCategoryUpdateRequest)) {
          salesCategoryMappingUpdateRequest.getAddedCategories().add(addSalesCategoryUpdateRequest);
        }
      }

    } else {
      halalHistoryUpdateEventModel =
          CommonUtil.getHalalHistoryUpdateEventModel(curationStatus, product, halalHistoryUpdateEventModel, storeId,
              userName);
      product.setCurationStatus(CurationStatus.REJECTED);
      for (CategoryReferenceResponse categoryReferenceResponse : categoryDetailResponse.getHalalSalesCategoryReferences()) {
        SalesCategoryUpdateRequest deleteSalesCategoryUpdateRequest =
            CommonUtil.getSalesCategoryUpdateRequest(categoryReferenceResponse);
        if (Objects.nonNull(deleteSalesCategoryUpdateRequest)) {
          salesCategoryMappingUpdateRequest.getDeletedCategories().add(deleteSalesCategoryUpdateRequest);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(salesCategoryMappingUpdateRequest.getAddedCategories())
        || CollectionUtils.isNotEmpty(salesCategoryMappingUpdateRequest.getDeletedCategories())) {
      productService.updateSalesCategoryOfProduct(salesCategoryMappingUpdateRequest, product);
    }
    productService.saveProductHalalConfigAndCaptureHistory(product, halalHistoryUpdateEventModel, storeId);
  }

  private List<PriceRangeResponse> constructPriceRangeResponseForItemSkuList(String storeId,
      List<String> skuSet) {

    List<ItemPickupPoint> itemPickupPointSet =
        itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, skuSet);
    Set<String> itemSkuSet = new HashSet<>(skuSet);
    Map<String, Item> itemSkuToItemMap = findItemsByItemSkus(storeId, itemSkuSet);

    return itemPickupPointSet.stream()
        .map(itemPickupPoint -> {
          List<Double> priceListingPrice = itemPickupPoint.getPrice()
              .stream()
              .filter(price -> price.getChannel().equalsIgnoreCase(Constants.DEFAULT))
              .map(Price::getListPrice)
              .collect(toList());

          if (CollectionUtils.isEmpty(priceListingPrice)) {
            log.error(
                "#ProductServiceV2Impl getPriceRangeForSkus failed to get response from db for the skuSet from itemPickupPointRepository: {}",
                skuSet);
            throw new ApiIncorrectInputDataException(ApiErrorCodes.SKUS_NOT_FOUND.getErrorMessage(),
                ApiErrorCodes.SKUS_NOT_FOUND.getErrorCode());
          }

          return constructPriceRangeResponse(itemPickupPoint.getItemSku(),
              itemSkuToItemMap.get(itemPickupPoint.getItemSku()).getGeneratedItemName(),
              priceListingPrice.stream().mapToDouble(v -> v).max().orElseThrow(
                  NoSuchElementException::new),
              priceListingPrice.stream().mapToDouble(v -> v).min().orElseThrow(
                  NoSuchElementException::new));
        }).collect(toList());
  }

  private List<PriceRangeResponse> constructPriceRangeResponseForProductSkuList(String storeId, String merchantCode,
      Set<String> skuSet) {
    List<ProductSolr> productSolrList =
        productSolrRepository.findByProductSkuList(storeId, merchantCode, skuSet);
    if (CollectionUtils.isEmpty(productSolrList)) {
      log.error(
          "#ProductServiceV2Impl getPriceRangeForSkus failed to get response from db for the skuSet from productSolrRepository: {}",
          skuSet);
      throw new ApiIncorrectInputDataException(ApiErrorCodes.SKUS_NOT_FOUND.getErrorMessage(),
          ApiErrorCodes.SKUS_NOT_FOUND.getErrorCode());
    }
    return productSolrList.stream()
        .map(productSolr -> constructPriceRangeResponse(productSolr.getProductSku(),
            productSolr.getProductName(),
            productSolr.getMaximumSellingPrice(),
            productSolr.getMinimumSellingPrice())).collect(toList());
  }

  private PriceRangeResponse constructPriceRangeResponse(String webSku,
      String skuName,
      Double maxPrice,
      Double minPrice) {
    return PriceRangeResponse.builder()
        .skuName(skuName)
        .priceRange(String.format(constructPriceRange(minPrice, maxPrice)))
        .webSku(webSku)
        .build();
  }

  private String constructPriceRange(Double minPrice, Double maxPrice) {
    Locale indonesianLocale = new Locale(LOCALE_LANGUAGE, LOCALE_COUNTRY);
    NumberFormat formatter = NumberFormat.getCurrencyInstance(indonesianLocale);
    formatter.setMaximumFractionDigits(0);
    String min = formatter.format(minPrice);
    String max = formatter.format(maxPrice);
    if (minPrice.equals(maxPrice)) {
      return min;
    } else {
      return String.format(minMaxPriceRangePattern,
          min, max);
    }
  }

  private boolean isValid(Set<String> skuList, boolean isProduct) {
    return skuList.stream()
        .noneMatch(sku -> isProduct ?
            !this.skuValidator.isProductSku(sku) :
            !this.skuValidator.isItemSku(sku));
  }

  private Map<String, Item> findItemsByItemSkus(String storeId, Set<String> itemSkus) {
    Map<String, Item> cachedItems =
        cacheItemHelperService.getCacheableItemsByItemSkus(storeId, itemSkus);
    Set<String> itemSkuNotCachedSet = new HashSet<>();

    for (String itemSku : itemSkus) {
      if (!cachedItems.containsKey(itemSku)) {
        itemSkuNotCachedSet.add(itemSku);
      }
    }
    Map<String, Item> itemMapFromDb =
        Optional.ofNullable(itemService.findByStoreIdAndItemSkus(storeId, itemSkuNotCachedSet))
            .orElse(new ArrayList<>())
            .stream()
            .collect(toMap(Item::getItemSku, Function.identity()));
    cachedItems.putAll(itemMapFromDb);
    return cachedItems;
  }

  @Override
  public BasicProductResponse getBasicProductDetails(String storeId, String productSku, boolean sharedProductInfoNeeded) {
    Product product = productCacheableService.findProductByStoreIdAndProductSku(storeId, productSku);
    if (Objects.isNull(product)) {
      log.error("Product not present for productSku : {}", productSku);
      throw new ApiIncorrectInputDataException(ApiErrorCodes.SKUS_NOT_FOUND.getErrorMessage(),
          ApiErrorCodes.SKUS_NOT_FOUND.getErrorCode());
    }
    BasicProductResponse basicProductResponse = new BasicProductResponse();
    BeanUtils.copyProperties(product, basicProductResponse, "b2cActivated");
    basicProductResponse.setB2cActivated(Optional.ofNullable(product.getB2cActivated()).orElse(true));
    basicProductResponse.setArchived(product.isArchived());
    basicProductResponse.setSyncProduct(product.isSynchronized());
    if (Objects.nonNull(product.getPreOrder())) {
      PreOrderDTO preOrderDTO = new PreOrderDTO();
      BeanUtils.copyProperties(product.getPreOrder(),preOrderDTO);
      basicProductResponse.setPreOrder(preOrderDTO);
    }
    if (sharedProductInfoNeeded) {
      List<Product> productList =
          productRepository.findFirst2ByStoreIdAndProductCodeAndMarkForDelete(storeId, product.getProductCode(), false);
      basicProductResponse.setSharedProduct(productList.size() > 1);
    }
    return basicProductResponse;
  }

  @Override
  public void backFillSpecialAttributesInProduct(String productCode, String attributeCode, String attributeName,
      String attributeValue) {
    List<Product> productList =
        productService.findByStoreIdAndProductCode(mandatoryParameterHelper.getStoreId(), productCode);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productList),
        String.format(ErrorMessages.PRODUCT_NOT_FOUND_ERROR, productCode));
    for (Product product : productList) {
      Optional<ProductSpecialAttribute> existingAttribute =
          Optional.ofNullable(product.getProductSpecialAttributes()).orElse(new ArrayList<>()).stream()
              .filter(Objects::nonNull).filter(attr -> attr.getAttributeCode().equals(attributeCode)).findFirst();
      if (existingAttribute.isEmpty()) {
        ProductSpecialAttribute newAttribute = new ProductSpecialAttribute();
        newAttribute.setAttributeCode(attributeCode);
        newAttribute.setAttributeName(attributeName);
        newAttribute.setAttributeValue(StringUtils.isNotBlank(attributeValue) ? attributeValue : Constants.HYPHEN);
        product.getProductSpecialAttributes().add(newAttribute);
        productRepository.save(product);
        cacheEvictHelperService.evictProductData(product.getStoreId(), product);
      } else {
        existingAttribute.get().setAttributeValue(attributeValue);
        productRepository.save(product);
        cacheEvictHelperService.evictProductData(product.getStoreId(), product);
      }
    }
  }
}
