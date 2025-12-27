package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemNameSkuVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.PristineItemVO;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSkuAndNameVO;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.model.vo.ProductsToItemCatalogMapping;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.ReviewProductDetailVO;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.model.vo.SolrGroupResultVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ProductAttributesUtil;
import com.gdn.x.promotion.enums.PromoBundlingType;
import com.google.common.collect.Lists;

@Service
public class ProductSearchServiceImpl implements ProductSearchService {

  private static final int FIRST = 0;

  private static final String COMMA_DELIMITER = ",";

  private static final String PROMO_BUNDLING_WHOLESALE_TYPE = "WHOLESALE";

  private static final String NOT_FOUND_CATALOG = "not-found-catalog";

  private static final String GET_BY_PRODUCT_CATENTRY_ID_NOT_FOUND_WITH_CATENTRY =
      "#getByProductCatentryId Not found with catentry : ";

  private static final Logger LOG = LoggerFactory.getLogger(ProductSearchServiceImpl.class);

  private static final String RETURN_NOT_FOUND_OF_PRODUCT_CODE =
      "return not found of product code ";

  private static final String RETURN_NOT_FOUND_SYNC_PRODUCT_OF_PRODUCT_CODE =
      "return not found of product code ";

  private static final String RETURN_NOT_FOUND_OF_PRODUCT_SKU = "return not found of product sku ";

  private static final String RETURN_NOT_FOUND_OF_CATENTRY_ID = "return not found of catentry id ";

  private static final int FIRST_DATA = 0;

  private static final String PRODUCT_CATENTRY_ID_MUST_NOT_BE_BLANK =
      "productCatentryId must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";

  private static final String MERCHANT_ID_MUST_NOT_BE_BLANK = "merchantId must not be blank";

  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

  private static final String PRODUCT_SKU_MUST_NOT_BE_BLANK = "productSku must not be blank";

  private static final String ITEM_SKU_NOT_FOUND = "ItemSku not found";

  private static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";

  private static final String ITEM_SKU_NOT_FOUND_FOR_PRISTINEID = "itemSku not found for pristineId";

  private static final String PRODUCT_RESULTS_NOT_FOUND_FOR_PRODUCT_SKUS = "Product results not found for product-skus.";

  private static final String PRISTINE_ITEM_LIST_NOT_FOUND = "Pristine Item List not found.";

  private static final String ITEMSKU_NOT_MAPPED_TO_PRISTINE = "ItemSku not mapped to Pristine";

  private static final String PAGEABLE_MUST_NOT_BE_NULL = "pageable must not be null";

  private static final String ITEM_SKUS_MUST_NOT_BE_EMPTY = "itemSkus must not be empty";
  private static final String CREATED_DATE_MUST_NOT_BE_NULL = "Created date must not be null";

  private static final String PRICE_CHANNEL_POSTFIX = "DEFAULT#_#";

  private static final String IMAGE_DIVIDER = "#_#";

  private static final String ACTIVE = "Active";

  private static final String BANNED = "Banned";

  private static final String TRUE = "true";

  private static final String DEFAULT = "DEFAULT";

  @Value("${pristine.product.similar.parameters}")
  private String pristineSimilarParameters;

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  private static final String PRODUCT_RESULT_MUST_NOT_BE_EMPTY = "productResult must not be empty.";

  @Autowired
  private ProductAndItemSolrRepository solrRepository;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ProductSearchHelperService productSearchHelper;

  @Autowired
  private ProductCacheableService productCacheHelperService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ProductAndItemSolrConstructorService productAndItemSolrConstructorService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private FormulaUtil formulaUtil;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private PromotionOutbound promotionOutbound;

  @Autowired
  private ItemCacheableService itemCacheableService;

  @Autowired
  @Lazy
  private ItemPriceService itemPriceService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private PristineCacheableService pristineCacheableService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductAttributesUtil productAttributesUtil;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ProductSolrRepository productSolrRepository;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Value("${solr.string.delimiter:#_#}")
  private String solrStringDelimiter;

  @Value("${suspension.listing.v2.query.enabled}")
  private boolean suspensionListingV2QueryEnabled;

  private Page<ProductsToItemCatalogMapping> constructMappingProductToItemCatalog(String username,
      String requestId, Pageable page, Page<ProductAndItemSolr> products) {
    Map<String, List<ProductSkuAndNameVO>> masterToSkuMapping =
        new HashMap<String, List<ProductSkuAndNameVO>>();
    for (ProductAndItemSolr itemSolr : products) {
      String masterCatalog = itemSolr.getMasterCatalog();
      if (masterToSkuMapping.get(masterCatalog) == null) {
        masterToSkuMapping.put(masterCatalog, new ArrayList<ProductSkuAndNameVO>());
      }
      masterToSkuMapping.get(masterCatalog)
          .add(new ProductSkuAndNameVO(itemSolr.getProductSku(), itemSolr.getProductName()));
    }
    List<ProductsToItemCatalogMapping> masterToProductMapping =
        new ArrayList<ProductsToItemCatalogMapping>();
    for (Entry<String, List<ProductSkuAndNameVO>> entry : masterToSkuMapping.entrySet()) {
      ItemCatalogVO itemCatalogVO = null;
      try {
        MasterCatalog masterCatalog =
            this.productAndItemSolrConstructorService.constructMasterCatalog(entry.getKey());
        if (masterCatalog != null) {
          List<ItemCatalogVO> categories = this.catalogService.getItemCatalogsWithCategoryHierarchy(
              username, requestId, Arrays.asList(masterCatalog.getCategory().getCategoryCode()));
          itemCatalogVO = categories.get(ProductSearchServiceImpl.FIRST_DATA);
        } else {
          itemCatalogVO = new ItemCatalogVO();
          itemCatalogVO.setCatalogId(ProductSearchServiceImpl.NOT_FOUND_CATALOG);
        }
      } catch (Exception e) {
        ProductSearchServiceImpl.LOG.warn(
            "Failed to fetch hierarchy of masterCatalog {} with sku : {}", entry.getKey(),
            entry.getValue(), e);
        itemCatalogVO = new ItemCatalogVO();
        itemCatalogVO.setCatalogId(entry.getKey());
      }
      ProductsToItemCatalogMapping mapping =
          new ProductsToItemCatalogMapping(entry.getValue(), itemCatalogVO);
      // for backward compatibility with productCenter which still using product v2.0.1
      ArrayList<String> productSkus = new ArrayList<String>();
      for (ProductSkuAndNameVO productSkuAndNameVO : entry.getValue()) {
        productSkus.add(productSkuAndNameVO.getProductSku());
      }
      mapping.setProductSkus(productSkus);
      // please delete after minor version increment
      masterToProductMapping.add(mapping);
    }
    return new PageImpl<ProductsToItemCatalogMapping>(masterToProductMapping, page,
        products.getTotalElements());
  }

  private ProductAndItemsVO convertToProductAndItems(
      MasterDataDetailWithProductAndItemsResponseVo responseVo) {
    ProductAndItemsVO productAndItemsVO = responseVo.getProductAndItems().get(0);
    boolean isSync = productAndItemsVO.getProduct().isSynchronized();
    if (isSync) {
      productAndItemsVO.getProduct().setMasterDataProduct(
          responseVo.getMasterDataProducts().get(productAndItemsVO.getProduct().getProductCode()));
      for (Item item : productAndItemsVO.getItems()) {
        item.setMasterDataItem(responseVo.getMasterDataItems().get(item.getItemCode()));
      }
    }
    return productAndItemsVO;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getAllProductAndItemsSortByProductCodeAsc(
      String storeId, String username, String requestId, boolean needCategoryHierarchies,
      Pageable page) throws Exception {
    Page<ProductAndItemSolr> pageResult =
        this.solrRepository.getListOfActiveProductSkusSortedByProductCode(storeId, page);
    List<ProductAndItemSolr> solrResult = pageResult.getContent();
    MasterDataDetailWithProductAndItemsResponseVo result =
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailForReindexBySolrResult(
            storeId, username, requestId, solrResult);
    result.setActiveProductCount(pageResult.getTotalElements());
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public List<ProductAndItemSolr> getItemSkuAndCodeByStoreIdAndItemSkus(String storeId,
      Set<String> itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(itemSku != null && !itemSku.isEmpty(),
        ProductSearchServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    return solrRepository.findItemSkuAndCodeByStoreIdAndItemSkuIn(storeId, itemSku);
  }

  @Override
  public Page<ProductDetailVo> getProductsForOfficialStore(String storeId, String clientId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(officialStoreRequestVO.getMerchantCodes()),
        ProductSearchServiceImpl.MERCHANT_ID_MUST_NOT_BE_BLANK);
    if (Constants.OXFORD_CLIENT_ID.equalsIgnoreCase(clientId)) {
      return fetchProductsByMerchantCodeAndCatgeoryCodeFromL3Collection(storeId, officialStoreRequestVO, pageable);
    } else {
      SolrGroupResultVO<ProductAndItemSolr> result =
          solrRepository.getProductsForOfficialStore(storeId, officialStoreRequestVO, pageable);
      return toProductDetailVoPage(storeId, result, officialStoreRequestVO);
    }
  }

  private Page<ProductDetailVo> fetchProductsByMerchantCodeAndCatgeoryCodeFromL3Collection(String storeId,
      OfficialStoreRequestVO requestVO, Pageable pageable) {
    List<ProductDetailVo> productDetailVos = new ArrayList<>();
    OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
    BeanUtils.copyProperties(requestVO, officialStoreRequestVO, "buyable", "discoverable");
    SolrGroupResultVO<ProductSolr> l3Result =
        solrRepository.getProductsByCategoryAndMerchantCodeL3(storeId, officialStoreRequestVO, pageable);
    if (l3Result.getNumGroups() > 0) {
      Map<String, List<ItemPickupPoint>> productSkuItemPickupPointMap =
          itemPickupPointService.getItemPickupPointByProductSkuListAndDeliveryTrue(storeId,
              l3Result.getContent().stream().map(ProductSolr::getProductSku).distinct().collect(Collectors.toList()));
      setProductDetailVoForL3(l3Result.getContent(), productSkuItemPickupPointMap, requestVO, productDetailVos);
    }
    return new PageImpl<>(productDetailVos, pageable, l3Result.getNumGroups());
  }

  private void setProductDetailVoForL3(List<ProductSolr> productSolrs,
      Map<String, List<ItemPickupPoint>> productSkuItemPickupPointMap, OfficialStoreRequestVO requestVO,
      List<ProductDetailVo> productDetailVos) {
    for (ProductSolr productSolr : productSolrs) {
      if (Double.compare(productSolr.getMaximumSellingPrice(), 0d) > 0
          && Double.compare(productSolr.getMinimumSellingPrice(), 0d) > 0) {
        productDetailVos.add(CommonUtil.toProductDetailVo(productSolr,
            checkForBuyableAndDiscoverableFlag(productSkuItemPickupPointMap.get(productSolr.getProductSku()),
                requestVO), solrStringDelimiter));
      }
    }
  }

  private Set<String> checkForBuyableAndDiscoverableFlag(List<ItemPickupPoint> itemPickupPointList, OfficialStoreRequestVO requestVO) {
    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      return itemPickupPointList.stream().filter(item -> item.getItemViewConfig().stream().anyMatch(
              itemViewConfig -> itemViewConfig.isBuyable() == requestVO.getBuyable().booleanValue()
                  && itemViewConfig.isDiscoverable() == requestVO.getDiscoverable().booleanValue())).map(ItemPickupPoint::getItemSku)
          .collect(Collectors.toSet());
    }
    return new HashSet<>();
  }

  @Override
  public Page<ActiveProductDetailVo> getActiveProductsListForMerchant(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    if (Objects.isNull(activeProductsRequestVO.getTradingProduct()) || Boolean.FALSE.equals(
        activeProductsRequestVO.getTradingProduct())) {
      checkArgument(StringUtils.isNotBlank(activeProductsRequestVO.getMerchantCode()),
          ProductSearchServiceImpl.MERCHANT_ID_MUST_NOT_BE_BLANK);
    }
    ActiveProductsRequestVO requestVO = new ActiveProductsRequestVO();
    BeanUtils.copyProperties(activeProductsRequestVO, requestVO, "buyable", "discoverable");

    List<ActiveProductDetailVo> activeProductDetailVos = new ArrayList<>();
    SolrGroupResultVO<ProductSolr> result =
        solrRepository.getActiveProductsListForMerchant(storeId, requestVO, pageable);
    if (CollectionUtils.isNotEmpty(result.getContent())) {
      List<String> productSkus =
          result.getContent().stream().map(ProductSolr::getProductSku).distinct().collect(Collectors.toList());
      Map<String, List<ItemPickupPoint>> itemPickupPointMap =
          itemPickupPointService.getItemPickupPointByProductSkuList(storeId, productSkus);
      Map<String, List<ItemNameSkuVO>> itemSkuMap = generateProductSkuAndItemMap(itemPickupPointMap, activeProductsRequestVO);
      CommonUtil.setActiveProductDetailVo(activeProductDetailVos, result.getContent(), itemSkuMap,
          itemPickupPointMap, getItemIfProductHasSingleVariant(storeId, result), solrStringDelimiter);
    }
    return new PageImpl<>(activeProductDetailVos, pageable, result.getNumGroups());
  }

  private Map<String, Item> getItemIfProductHasSingleVariant(String storeId, SolrGroupResultVO<ProductSolr> result)
      throws Exception {
    Map<String, Item> itemMap = new HashMap<>();
    Set<String> productSkusWithSingleVariant = result.getContent().stream().filter(
            productSolr -> Objects.nonNull(productSolr.getVariantCount())
                && Constants.SINGLE_VARIANT_L4_COUNT == productSolr.getVariantCount()).map(ProductSolr::getProductSku)
        .collect(Collectors.toSet());

    if (CollectionUtils.isNotEmpty(productSkusWithSingleVariant)) {
      List<Item> items = itemService.getItemsByProductSkus(storeId, productSkusWithSingleVariant);
      if (CollectionUtils.isNotEmpty(items)) {
        items.forEach(item -> itemMap.put(item.getProductSku(), item));
      }
    }

    return itemMap;
  }

  private Map<String, List<ItemNameSkuVO>> generateProductSkuAndItemMap(
      Map<String, List<ItemPickupPoint>> itemPickupPointMap, ActiveProductsRequestVO activeProductsRequestVO) {
    Map<String, List<ItemNameSkuVO>> itemSkuMap = new HashMap<>();
    for (List<ItemPickupPoint> itemPickupPointList : itemPickupPointMap.values()) {
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream().anyMatch(
            itemViewConfig -> itemViewConfig.isBuyable() == activeProductsRequestVO.getBuyable().booleanValue()
                && itemViewConfig.isDiscoverable() == activeProductsRequestVO.getDiscoverable().booleanValue())) {
          if (itemSkuMap.keySet().contains(itemPickupPoint.getProductSku())) {
            if (!itemSkuMap.get(itemPickupPoint.getProductSku()).contains(itemPickupPoint.getItemSku())) {
              itemSkuMap.get(itemPickupPoint.getProductSku())
                  .add(new ItemNameSkuVO(null, itemPickupPoint.getItemSku()));
            }
          } else {
            List<ItemNameSkuVO> itemNameSkuVOList = new ArrayList<>();
            itemNameSkuVOList.add(new ItemNameSkuVO(null, itemPickupPoint.getItemSku()));
            itemSkuMap.put(itemPickupPoint.getProductSku(), itemNameSkuVOList);
          }
        }
      }
    }
    return itemSkuMap;
  }

  @Override
  public Page<ActiveProductDetailVo> getActiveProductsListForSuspension(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception {
    Page<ProductAndItemSolr> result = suspensionListingV2QueryEnabled ?
        solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatusV2(storeId, activeProductsRequestVO,
            pageable) :
        solrRepository.getProductsByMerchantCodeAndCategoryCodesAndStatus(storeId, activeProductsRequestVO, pageable);
    List<String> productSkus = new ArrayList<>();
    for (ProductAndItemSolr productAndItemSolr : result.getContent()) {
      if (!productSkus.contains(productAndItemSolr.getProductSku())) {
        productSkus.add(productAndItemSolr.getProductSku());
      }
    }
    Map<String, Long> productSkuMap = Lists.partition(productSkus, 10).stream().map(HashSet::new)
        .map(productSkuSet -> cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(storeId, productSkuSet))
        .flatMap(List::stream).collect(groupingBy(Item::getProductSku, Collectors.counting()));
    productSkus.removeAll(productSkuMap.keySet());
    for (String productSku : productSkus) {
      productSkuMap.put(productSku,
          (long) cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku)
              .size());
    }
    return toProductDetailVoListPage(result, productSkuMap, pageable);
  }

  @Override
  public Page<ItemInfoVO> getSuspendedItemList(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception {
    Page<ProductAndItemSolr> result =
        solrRepository.getItemsByMerchantCodeAndCategoryCodes(storeId, activeProductsRequestVO, pageable);
    List<ItemInfoVO> list = new ArrayList<>();
    for (ProductAndItemSolr productAndItemSolr : result) {
      ItemInfoVO itemInfoVO = new ItemInfoVO();
      BeanUtils.copyProperties(productAndItemSolr, itemInfoVO);
      if (CollectionUtils.isNotEmpty(productAndItemSolr.getItemImages())) {
        for(String mainImage : productAndItemSolr.getItemImages()){
          if(mainImage.startsWith(TRUE)){
            String[] imageUrl = mainImage.split("#_#");
            itemInfoVO.setImageUrl((imageUrl.length >= 2) ? imageUrl[1] : StringUtils.EMPTY);
            break;
          }
        }
      }
      list.add(itemInfoVO);
    }
    return new PageImpl<ItemInfoVO>(list, pageable, result.getTotalElements());
  }

  private Map<String, List<ProductAndItemSolr>> getItemsPrice(String storeId,
      List<String> productSkus, OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) throws Exception {
    checkArgument(CollectionUtils.isNotEmpty(productSkus),
        ProductSearchServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Page<ProductAndItemSolr> result =
        solrRepository.getListItemsByProductSkus(storeId, productSkus, officialStoreRequestVO, pageable);
    return result.getContent().stream()
        .collect(groupingBy(ProductAndItemSolr::getProductSku));
  }

  /**
   * to update min max price productDetailVo
   * @param itemPrices must not be blank
   * @param productDetailVo must not be blank
   * @param officialStoreRequestVO must not be null
   */
  private void setMinMaxPrice(Map<String, List<ProductAndItemSolr>> itemPrices,
      ProductDetailVo productDetailVo, OfficialStoreRequestVO officialStoreRequestVO) {
    Set<String> itemSkus = new HashSet<>();
    Double optionalDoubleMin =
        itemPrices.getOrDefault(productDetailVo.getProductSku(), Collections.emptyList()).stream()
            .map(productAndItemSolr -> getOfferPrice(itemSkus, productAndItemSolr, officialStoreRequestVO))
            .min(Double::compare)
            .orElse(0.0);

    Double optionalDoubleMax =
        itemPrices.getOrDefault(productDetailVo.getProductSku(), Collections.emptyList()).stream()
            .map(productAndItemSolr -> getOfferPrice(itemSkus, productAndItemSolr, officialStoreRequestVO))
            .max(Double::compare)
            .orElse(0.0);

      productDetailVo.setMinPrice(optionalDoubleMin);
      productDetailVo.setMaxPrice(optionalDoubleMax);
    productDetailVo.setItemSkus(itemSkus);
  }

  /**
   * Get offer price of a solr item
   * @param itemSkus
   * @param productAndItemSolr
   * @param officialStoreRequestVO
   * @return
   */
  private Double getOfferPrice(Set<String> itemSkus, ProductAndItemSolr productAndItemSolr,
      OfficialStoreRequestVO officialStoreRequestVO) {
    List<Double> offerPrices = productAndItemSolr.getOfferPrice().stream()
        .filter(price -> StringUtils.contains(price, PRICE_CHANNEL_POSTFIX))
        .map(price -> StringUtils.replace(price, PRICE_CHANNEL_POSTFIX, StringUtils.EMPTY))
        .map(Double::valueOf).filter(priceValue -> priceValue >= officialStoreRequestVO.getMinPrice()
                && priceValue <= officialStoreRequestVO.getMaxPrice()).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(offerPrices)) {
      itemSkus.add(productAndItemSolr.getItemSku());
      return offerPrices.get(0);
    } else {
      return 0.0;
    }
  }

  /**
   * to convert from productAndItemSolr to productDetailVo
   *
   * @param productAndItemSolr must not be blank
   * @param itemPrices productSku andProductAndItemSolrs Map
   * @param officialStoreRequestVO must not be null
   */
  private ProductDetailVo toProductDetailVo(ProductAndItemSolr productAndItemSolr,
      Map<String, List<ProductAndItemSolr>> itemPrices, OfficialStoreRequestVO officialStoreRequestVO) {
    ProductDetailVo productDetailVo = new ProductDetailVo();

    BeanUtils.copyProperties(productAndItemSolr, productDetailVo);
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getItemImages())) {
      String[] imageUrl = productAndItemSolr.getItemImages().get(0).split(IMAGE_DIVIDER);
      productDetailVo.setImageUrl(imageUrl[1]);
    }
    if(Objects.nonNull(productAndItemSolr.getMasterCatalog())) {
      productDetailVo.setCategoryCode(productAndItemSolr.getMasterCatalog().split(IMAGE_DIVIDER)[1]);
    }
    setMinMaxPrice(itemPrices, productDetailVo, officialStoreRequestVO);
    return productDetailVo;
  }

  /**
   * to convert from productAndItemSolr to productDetailVoList
   *
   * @param productAndItemSolr must not be blank
   */
  private ActiveProductDetailVo toProductDetailVoList(ProductAndItemSolr productAndItemSolr,
      Map<String, Set<ItemNameSkuVO>> itemNameSku) {
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();

    BeanUtils.copyProperties(productAndItemSolr, activeProductDetailVo);
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getItemImages())) {
      String[] imageUrl = productAndItemSolr.getItemImages().get(0).split(solrStringDelimiter);
      activeProductDetailVo.setImageUrl(imageUrl[1]);
    }

    if(Objects.nonNull(productAndItemSolr.getProductSku())) {
      activeProductDetailVo.setItemDetailVOList(new ArrayList<>(itemNameSku.get(productAndItemSolr.getProductSku())));
    }

    if (productAndItemSolr.isSuspended()) {
      activeProductDetailVo.setStatus(BANNED);
    } else {
      activeProductDetailVo.setStatus(ACTIVE);
    }

    return activeProductDetailVo;
  }

  /**
   * to convert from productAndItemSolr to productDetailVoList
   *
   * @param productAndItemSolr must not be blank
   * @param productSku
   */
  private ActiveProductDetailVo toProductDetailVo(ProductAndItemSolr productAndItemSolr,
      Map<String, Long> productSku) {
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();

    BeanUtils.copyProperties(productAndItemSolr, activeProductDetailVo);

    if(Objects.nonNull(productAndItemSolr.getProductSku())) {
      activeProductDetailVo.setItemCount(productSku.get(productAndItemSolr.getProductSku()).intValue());
    }

    if (productAndItemSolr.isSuspended()) {
      activeProductDetailVo.setStatus(BANNED);
    } else {
      activeProductDetailVo.setStatus(ACTIVE);
    }

    return activeProductDetailVo;
  }

  /**
   * to convert from productAndItemSolrs paging to productDetailVo with paging
   * @param storeId must not be blank
   * @param productAndItemSolrs must not be blank
   * @param officialStoreRequestVO must not be blank
   */
  private Page<ProductDetailVo> toProductDetailVoPage(String storeId,
      SolrGroupResultVO<ProductAndItemSolr> productAndItemSolrs,
      OfficialStoreRequestVO officialStoreRequestVO) throws Exception {
    if (CollectionUtils.isNotEmpty(productAndItemSolrs.getContent())) {
      Map<String, List<ProductAndItemSolr>> itemPrices = productAndItemSolrs.getContent()
          .stream()
          .collect(groupingBy(ProductAndItemSolr::getProductSku));
      List<ProductDetailVo> productDetailVos = new ArrayList<>();
      Set<String> distinctProductSkus = new HashSet<>();
      for (ProductAndItemSolr productAndItemSolr : productAndItemSolrs.getContent()) {
        ProductDetailVo productDetailVo =
            toProductDetailVo(productAndItemSolr, itemPrices, officialStoreRequestVO);
        if (Objects.nonNull(productDetailVo)
            && Double.compare(productDetailVo.getMaxPrice(), 0d) > 0
            && Double.compare(productDetailVo.getMinPrice(), 0d) > 0
            && !distinctProductSkus.contains(productDetailVo.getProductSku())) {
          distinctProductSkus.add(productDetailVo.getProductSku());
          productDetailVos.add(productDetailVo);
        }
      }
      return new PageImpl<>(productDetailVos,
        PageRequest.of(productAndItemSolrs.getPageNumber(), productAndItemSolrs.getPageSize()),
        productAndItemSolrs.getNumGroups());
    }
    return new PageImpl<>(Collections.emptyList(),
      PageRequest.of(productAndItemSolrs.getPageNumber(),
      productAndItemSolrs.getPageSize()),
        productAndItemSolrs.getNumGroups());
  }

  /**
   * to convert from productAndItemSolrs paging to activeProductDetailVo with paging
   * @param productAndItemSolrs must not be blank
   */
  private Page<ActiveProductDetailVo> toProductDetailVoListPage(SolrGroupResultVO<ProductAndItemSolr> productAndItemSolrs,
      Map<String, Set<ItemNameSkuVO>> itemNameSku) {
    List<ActiveProductDetailVo> productDetailVos =
        new ArrayList<>(Optional.ofNullable(productAndItemSolrs.getContent()).orElse(Collections.emptyList())
            .stream().map(
                productAndItemSolr -> toProductDetailVoList(productAndItemSolr, itemNameSku))
            .collect(Collectors.toMap(ActiveProductDetailVo::getProductSku, Function.identity(), (p, q) -> p))
            .values());
    return new PageImpl<>(productDetailVos, PageableHelper
        .generatePageable(productAndItemSolrs.getPageNumber(), productAndItemSolrs.getPageSize()),
        productAndItemSolrs.getNumGroups());
  }

  /**
   * to convert from productAndItemSolrs paging to activeProductDetailVo with paging
   * @param productAndItemSolrs must not be blank
   */
  private Page<ActiveProductDetailVo> toProductDetailVoListPage(Page<ProductAndItemSolr> productAndItemSolrs,
      Map<String, Long> productSku, Pageable pageable) {
    List<ActiveProductDetailVo> productDetailVos = new ArrayList<>(
        Optional.ofNullable(productAndItemSolrs.getContent()).orElse(Collections.emptyList()).stream()
            .map(productAndItemSolr -> toProductDetailVo(productAndItemSolr, productSku))
            .collect(toList()));
    return new PageImpl<>(productDetailVos,
        PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()),
        productAndItemSolrs.getTotalElements());
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getListOfProductsBySimpleProductRequests(
      String storeId, String username, String requestId,
      List<SimpleProductRequestVo> simpleProductRequests) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result =
        this.productSearchHelper.getProductAndItemsWithMasterDataDetailForReindexBySimpleProducts(
            storeId, username, requestId, simpleProductRequests);
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, true,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public ProductAndItemsVO getProductAndItemsByProductCatentryId(String storeId, String username,
      String requestId, String productCatentryId) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCatentryId),
        ProductSearchServiceImpl.PRODUCT_CATENTRY_ID_MUST_NOT_BE_BLANK);
    Set<String> productCatentryIds = new HashSet<String>();
    productCatentryIds.add(productCatentryId);
    MasterDataDetailWithProductAndItemsResponseVo responseVo =
        this.getProductAndItemsByProductCatentryIds(storeId, username, requestId,
            productCatentryIds, true);
    checkArgument(responseVo != null,
        ProductSearchServiceImpl.GET_BY_PRODUCT_CATENTRY_ID_NOT_FOUND_WITH_CATENTRY
            + productCatentryId);
    ProductAndItemsVO productAndItemsVO = this.convertToProductAndItems(responseVo);
    return productAndItemsVO;
  }


  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCatentryIds(
      String storeId, String username, String requestId, Set<String> productCatentryIds,
      boolean needCategoryHierarchies) throws Exception {
    this.productSearchHelper.validateParameters(storeId, productCatentryIds);
    List<Product> products =
        this.productService.getProductsByProductCatentryIds(storeId, productCatentryIds);
    checkArgument(products != null,
        ProductSearchServiceImpl.RETURN_NOT_FOUND_OF_CATENTRY_ID + productCatentryIds);
    MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchHelper
        .getProductAndItemsWithMasterDataDetail(storeId, username, requestId, products, false, false);
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCode(
      String storeId, String username, String requestId, String productCode,
      boolean needCategoryHierarchies) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result = null;
        checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode),
        ProductSearchServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);

    List<Product> productResult = this.productCacheHelperService
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    checkArgument(!productResult.isEmpty(),
        ProductSearchServiceImpl.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productCode);
    if (isProductVisibilityEnabled) {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username, requestId,
              productResult, false, false);
    } else {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetail(storeId, username, requestId, productResult, false, false);
    }
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCodes(
      String storeId, String username, String requestId, Set<String> productCodes,
      boolean needCategoryHierarchies) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    this.productSearchHelper.validateParameters(storeId, productCodes);
    List<Product> productResult =
        this.productService.getProductsByProductCodes(storeId, productCodes);
    checkArgument(!productResult.isEmpty(),
        ProductSearchServiceImpl.RETURN_NOT_FOUND_OF_PRODUCT_CODE + productCodes);
    if (isProductVisibilityEnabled) {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username, requestId,
              productResult, false, false);
    } else {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetail(storeId, username, requestId, productResult, false, false);
    }
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public MasterDataWithProductItemsVo getProductAndItemsByProductSkus(
      String storeId, String username, String requestId, Set<String> productSkus,
      boolean needCategoryHierarchies) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    this.productSearchHelper.validateParameters(storeId, productSkus);
    List<Product> productResult = this.productService.getProducts(storeId, productSkus);
    checkArgument(!productResult.isEmpty(),
        ProductSearchServiceImpl.RETURN_NOT_FOUND_OF_PRODUCT_SKU + productSkus);
    if (isProductVisibilityEnabled) {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username, requestId, productResult, false,
              false);
    } else {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetail(storeId, username, requestId, productResult, false, false);
    }
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    MasterDataWithProductItemsVo masterDataWithProductItemsVo = CommonUtil.toMasterDataWithProductItemsVo(result);
    if(CollectionUtils.isNotEmpty(masterDataWithProductItemsVo.getProductItemsVos())) {
      this.productService.setSellerPromoBundlings(storeId, masterDataWithProductItemsVo.getProductItemsVos());
    }
    return masterDataWithProductItemsVo;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsSyncByProductCodes(String storeId, String username, String requestId, String productCode,
      boolean needCategoryHierarchies, boolean combineOthersBundlings, boolean off2On) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    List<Product> productResult = null;
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    if ((Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED).getValue()))) {
      productResult = this.productCacheHelperService
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode).stream()
          .filter(product -> product.isSynchronized()).collect(toList());
    } else {
      productResult = this.productCacheHelperService
          .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(storeId,
              productCode);
    }
    checkArgument(!productResult.isEmpty(),
        ProductSearchServiceImpl.RETURN_NOT_FOUND_SYNC_PRODUCT_OF_PRODUCT_CODE + productCode);
    if (isProductVisibilityEnabled) {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetailByDefaultProduct(storeId, username, requestId,
              productResult, combineOthersBundlings, off2On);
    } else {
      result = this.productSearchHelper
          .getProductAndItemsWithMasterDataDetail(storeId, username, requestId, productResult, combineOthersBundlings,
              off2On);
    }
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public MasterDataDetailWithProductAndItemResponseVo getMasterDataAndProductAndItemData(
      String storeId, String username, String requestId, String productCode, String itemSku,
      boolean needCategoryHierarchies, boolean combineOthersBundlings, boolean off2On) throws Exception {
    MasterDataDetailWithProductAndItemResponseVo result;
    List<Product> productResult;
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    if ((Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.FETCH_ALL_PRODUCTS_BY_PRODUCT_CODE_ENABLED).getValue()))) {
      productResult = this.productCacheHelperService
          .findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode).stream()
          .filter(Product::isSynchronized).collect(toList());
    } else {
      productResult = this.productCacheHelperService
          .findByStoreIdAndProductCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(storeId, productCode);
    }
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        ProductSearchServiceImpl.RETURN_NOT_FOUND_SYNC_PRODUCT_OF_PRODUCT_CODE + productCode);
    if (isProductVisibilityEnabled) {
      result = this.productSearchHelper
          .getProductAndItemWithMasterDataDetailByDefaultProduct(storeId, username, requestId,
              productResult, itemSku, combineOthersBundlings, off2On);
    } else {
      result = this.productSearchHelper
          .getProductAndItemWithMasterDataDetail(storeId, username, requestId, productResult, itemSku,
              combineOthersBundlings, off2On);
    }
    List<ItemCatalogVO> itemCatalogs =
        this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, result.getProduct());
    result.getProduct().setItemCatalogs(itemCatalogs);
    return result;
  }

  @Override
  public Page<ProductsToItemCatalogMapping> getProductAndItemWithoutSalesCatalog(String storeId,
      String username, String requestId, int month, int year, Pageable page) throws Exception{
    Page<ProductAndItemSolr> products =
        this.solrRepository.getProductsWithNullSalesCatalogAndMarkForDeleteFalse(storeId,
            this.formulaUtil.getStartDate(month, year), this.formulaUtil.getEndDate(month, year),
            page);
    return this.constructMappingProductToItemCatalog(username, requestId, page, products);
  }

  @Override
  public Page<ProductsToItemCatalogMapping> getProductAndItemWithoutSalesCatalogPerDay(
      String storeId, String username, String requestId, int fromDay, int toDay, int month,
      int year, Pageable page) {
    Page<ProductAndItemSolr> products =
        this.solrRepository.getProductsWithNullSalesCatalogAndMarkForDeleteFalse(storeId,
            this.formulaUtil.getStartOfDay(fromDay, month, year),
            this.formulaUtil.getEndOfDay(toDay, month, year), page);
    return this.constructMappingProductToItemCatalog(username, requestId, page, products);
  }

  @Override
  public List<ProductAndItemSolr> getProductSkuAndNameByPickupPointCode(String storeId,
      String pickupPointCode) {
    List<ProductAndItemSolr> productAndItemSolr = this.solrRepository
        .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
    return productAndItemSolr;
  }

  @Override
  public boolean isPickupPointCodeUsed(String storeId, String pickupPointCode) {
    ItemPickupPoint itemPickupPoint =
        this.itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
    return Objects.nonNull(itemPickupPoint);
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByPristineIds (
      String storeId, String requestId, String username, Set<String> pristineIds) throws Exception{
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Map<String, Product> mapOfProducts = new HashMap<>();

    List<Item> itemSkuByPristineIds = this.itemService
        .getItemsByPristineIds(storeId, pristineIds);
    if(CollectionUtils.isNotEmpty(itemSkuByPristineIds)) {
      Set<String> productSkus = new HashSet<>();
      for (Item item : itemSkuByPristineIds) {
        productSkus.add(item.getProductSku());
      }
      List<Product> productResult = this.productService.getProducts(storeId, productSkus);
      for(Product product : productResult){
        mapOfProducts.put(product.getProductSku(), product);
      }
    }
    return createProductAndItemsVOList(storeId, mapOfProducts, itemSkuByPristineIds);
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo
  getProductMasterDataDetailByProductCodesAndSkus(
      String storeId, String username, String requestId, Set<String> productIds,
      Set<String> productSkus) throws Exception {

    List<Product> skusMappedProductResult = null;
    List<Product> productCodesMappedProductResult = null;
    if (CollectionUtils.isNotEmpty(productSkus)) {
      skusMappedProductResult = this.productService.getProducts(storeId, productSkus);
    }
    if (CollectionUtils.isNotEmpty(productIds)) {
      productCodesMappedProductResult =
          this.productService.getProductsByProductCodes(storeId, productIds);
    }
    Set<Product> productResult = new HashSet();
    if (CollectionUtils.isNotEmpty(skusMappedProductResult)) {
      productResult.addAll(skusMappedProductResult);
    }
    if (CollectionUtils.isNotEmpty(productCodesMappedProductResult)) {
      productResult.addAll(productCodesMappedProductResult);
    }
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        ProductSearchServiceImpl.PRODUCT_RESULT_MUST_NOT_BE_EMPTY + productResult);
    MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchHelper
        .getProductAndItemsWithMasterDataDetail(storeId, username, requestId,
            new ArrayList<>(productResult), false, false);
    this.productSearchHelper
        .setItemCatalogs(storeId, username, requestId, true, result.getProductAndItems(),
            result.getMasterDataProducts());
    this.setSalesCategorySequencesInProductItems(username, requestId, result.getProductAndItems());

    return result;
  }

  private void setSalesCategorySequencesInProductItems(String username, String requestId,
      List<ProductAndItemsVO> productsAndItems) throws Exception {
    for (ProductAndItemsVO productAndItem : productsAndItems) {
      Product product = productAndItem.getProduct();
      List<ItemCatalogVO> itemCatalogs =
          this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, product);
      List<SalesCategorySequence> salesCategorySequences =
          itemService.getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogs);
      product.setSalesCategorySequences(salesCategorySequences);
    }
  }


  @Override
  public MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsByProductCodesAndProductSkus(
      String storeId, String username, String requestId, Set<String> productIds,
      Set<String> productSkus) throws Exception {
    List<Item> items = null;

    Map<String, Product> mapOfProducts = new HashMap<>();
    List<Product> productResultBySkus = null;
    List<Product> productResultByProductCodes = null;
    if(CollectionUtils.isNotEmpty(productSkus)) {
      productResultBySkus = this.productService.getProducts(storeId, productSkus);
    }
    if(CollectionUtils.isNotEmpty(productIds)) {
      productResultByProductCodes =
          this.productService.getProductsByProductCodes(storeId, productIds);
    }
    Set<Product> productResult = new HashSet();
    if(CollectionUtils.isNotEmpty(productResultBySkus)){
    productResult.addAll(productResultBySkus);
    }
    if(CollectionUtils.isNotEmpty(productResultByProductCodes)){
    productResult.addAll(productResultByProductCodes);
    }
    if(CollectionUtils.isNotEmpty(productResult)) {
      Set<String> productSkusResult = new HashSet<>();
      for (Product product : productResult) {
        productSkusResult.add(product.getProductSku());
        mapOfProducts.put(product.getProductSku(), product);
      }
      items = this.itemService.getItemsByProductSkus(storeId, productSkusResult);
    }
    return createProductAndItemsVOList(storeId, mapOfProducts, items);
  }

  private MasterDataDetailWithProductAndItemsResponseVo createProductAndItemsVOList(String storeId,
      Map<String, Product> mapOfProducts, List<Item> items) {

    MasterDataDetailWithProductAndItemsResponseVo result = null;

    Map<String, ProductAndItemsVO> mapOfResponse = new HashMap<>();
    if(CollectionUtils.isNotEmpty(items)) {
      result = new MasterDataDetailWithProductAndItemsResponseVo();
      for (Item item : items) {
        Product product = mapOfProducts.get(item.getProductSku());
        if (product == null) {
          continue;
        }
        ProductAndItemsVO response = mapOfResponse.get(item.getProductSku());
        if (response == null) {
          response = new ProductAndItemsVO();
          response.setProduct(product);
          mapOfResponse.put(item.getProductSku(), response);
        }
        response.getItems().add(item);
      }
      result.setProductAndItems(new ArrayList<>(mapOfResponse.values()));
    }
    return result;
  }

  @Override
  public List<SimplePristineProductRequestVo> getProductCodesAndSkusByPristineIds(String storeId,
      String requestId, String username, Set<String> pristineIds) throws Exception {

    Map<String, Set<String>> pristineIdProductSkuMap = new HashMap();
    Set<String> productSkuSet = new HashSet<>();

    List<Item> itemSkuByPristineIds = this.itemService.getItemsByPristineIds(storeId, pristineIds);
    if (CollectionUtils.isNotEmpty(itemSkuByPristineIds)) {

      for (Item item : itemSkuByPristineIds) {
        if (pristineIdProductSkuMap.containsKey(item.getPristineDataItem().getPristineId())) {
          pristineIdProductSkuMap.get(item.getPristineDataItem().getPristineId())
              .add(item.getProductSku());
        } else {
          Set<String> productSkus = new HashSet<>();
          productSkus.add(item.getProductSku());
          pristineIdProductSkuMap.put(item.getPristineDataItem().getPristineId(), productSkus);
        }
        productSkuSet.add(item.getProductSku());
      }
    }
    Map<String, SimpleProductRequestVo> productSkuProductMap =
        getProductSkuAndProductMap(storeId, productSkuSet);
    return getPristineIdAndProductList(pristineIdProductSkuMap, productSkuProductMap);
  }

  private Map<String, SimpleProductRequestVo> getProductSkuAndProductMap(String storeId,
      Set<String> productSkuSet) throws Exception {

    Map<String, SimpleProductRequestVo> productSkuProductMap = new HashMap();
    if(CollectionUtils.isNotEmpty(productSkuSet)) {
      List<Product> productResult = this.productService.getProducts(storeId, productSkuSet);
      for (Product product : productResult) {
        SimpleProductRequestVo simpleProductRequestVo = new SimpleProductRequestVo();
        simpleProductRequestVo.setProductSku(product.getProductSku());
        simpleProductRequestVo.setProductCode(product.getProductCode());
        simpleProductRequestVo.setSynchronized(product.isSynchronized());
        productSkuProductMap.put(product.getProductSku(), simpleProductRequestVo);
      }
    }
    return productSkuProductMap;
  }

  private List<SimplePristineProductRequestVo> getPristineIdAndProductList(
      Map<String, Set<String>> pristineIdProductSkuMap,
      Map<String, SimpleProductRequestVo> productSkuProductMap) {

    List<SimplePristineProductRequestVo> simplePristineProductRequestVoList = new ArrayList<>();
    if(pristineIdProductSkuMap != null && productSkuProductMap != null) {
      for (Entry<String, Set<String>> entry : pristineIdProductSkuMap.entrySet()) {
        SimplePristineProductRequestVo simplePristineProductRequestVo = new SimplePristineProductRequestVo();
        Set<SimpleProductRequestVo> productList = new HashSet<>();
        for (String productSku : entry.getValue()) {
          if (productSkuProductMap.containsKey(productSku))
            productList.add(productSkuProductMap.get(productSku));
        }
        simplePristineProductRequestVo.setPristineId(entry.getKey());
        simplePristineProductRequestVo.setSimpleProductDTOList(productList);
        simplePristineProductRequestVoList.add(simplePristineProductRequestVo);
      }
    }
    return simplePristineProductRequestVoList;
  }

  @Override
  public List<ProductItemsVo> getProductAndItemsInfoForActiveItem(MandatoryRequestParam mandatoryRequestParam,
      Set<String> itemSkus, boolean pristine, boolean fullFetch, boolean needWholesaleData,
      boolean combineOthersBundlings, boolean off2On)
      throws Exception {

    List<ProductAndItemsVO> productAndItemsList = productService.getProductAndItemsByItemSkusForActiveItems(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getRequestId(),
        mandatoryRequestParam.getUsername(), itemSkus, fullFetch, combineOthersBundlings, off2On, true);
    if (pristine) {
      addPristineListingAttributesToItems(mandatoryRequestParam, productAndItemsList);
    }
    setWholesaleRules(needWholesaleData, mandatoryRequestParam, productAndItemsList);
    Optional.ofNullable(productAndItemsList).orElse(new ArrayList<>()).forEach(
      productAndItemsVO -> this.productService.checkProductAndItemsForForceReview(
        Collections.singletonList(productAndItemsVO.getProduct()), productAndItemsVO.getItems()));
    List<ProductItemsVo> productItemsVoList =
      Optional.ofNullable(productAndItemsList).orElse(new ArrayList<>()).stream().map(
        productAndItemsVO -> CommonUtil.toProductItemVo(productAndItemsVO.getProduct(),
          productAndItemsVO.getItems())).collect(toList());
    this.productService.setSellerPromoBundlings(mandatoryRequestParam.getStoreId(), productItemsVoList);
    return productItemsVoList;
  }

  @Override
  public List<ProductAndItemsVO> getProductAndItemsInfoForAllItems(MandatoryRequestParam mandatoryRequestParam,
      Set<String> itemSkus, boolean pristine, boolean fullFetch, boolean needWholesaleData,
      boolean combineOthersBundlings, boolean off2On)
      throws Exception {
    List<ProductAndItemsVO> productAndItemsList;
    if (Boolean.valueOf(this.systemParameterService.findValueByStoreIdAndVariable(mandatoryRequestParam.getStoreId(),
        SystemParameterNames.REVERT_TRANSACTION_API_SWITCH).getValue())) {
       productAndItemsList = productService
          .getProductAndItemsByItemSkusForAllItems(mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getRequestId(),
              mandatoryRequestParam.getUsername(), itemSkus, fullFetch, combineOthersBundlings, off2On);
    } else {
      productAndItemsList = setProductAndItemVoList(mandatoryRequestParam, itemSkus);
      if (CollectionUtils.isEmpty(productAndItemsList)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ErrorMessages.ITEM_MUST_NOT_BE_NULL);
      }
    }
    if (pristine) {
      addPristineListingAttributesToItems(mandatoryRequestParam, productAndItemsList);
    }
    setWholesaleRules(needWholesaleData, mandatoryRequestParam, productAndItemsList);
    return productAndItemsList;
  }

  private List<ProductAndItemsVO> setProductAndItemVoList(
      MandatoryRequestParam mandatoryRequestParam, Set<String> itemSkus) throws Exception {
    List<ProductAndItemsVO> productAndItemsVOList;
    List<Item> itemList = Optional.ofNullable(this.itemService
        .getItemsByStoreIdAndItemSkus(mandatoryRequestParam.getStoreId(), itemSkus))
        .orElse(new ArrayList<>());
    List<String> itemSkusForOfflineItemSearch = CommonUtil.getItemSkusNotFoundInFetch(itemList,
        new ArrayList<>(itemSkus));
    List<Item> itemListForOfflineItemSearch = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemSkusForOfflineItemSearch)) {
      itemListForOfflineItemSearch = itemService
          .getItemsByOfflineItemIds(mandatoryRequestParam.getStoreId(),
              itemSkusForOfflineItemSearch, true);
    }
    this.productHelperService
        .findAndConstructOfflineItems(mandatoryRequestParam.getStoreId(), itemList);
    itemList = Stream.concat(itemList.stream(), itemListForOfflineItemSearch.stream())
        .collect(Collectors.toList());
    List<String> productSkuList =
        itemList.stream().map(Item::getProductSku).distinct().collect(Collectors.toList());
    List<Product> productList = this.productService.
        findByStoreIdAndProductSkuIn(mandatoryRequestParam.getStoreId(), productSkuList);
    if (CollectionUtils.isEmpty(productList)) {
      return Collections.emptyList();
    }
    productAndItemsVOList = masterDataConstructorService
        .constructProductsAndItemsWithMasterData(mandatoryRequestParam.getStoreId(),
            mandatoryRequestParam.getUsername(), mandatoryRequestParam.getRequestId(),
            productList.stream().collect(Collectors.toMap(Product::getProductSku,
                Function.identity())), itemList, true);
    return setItemCatalogForProductAndItemsVo(productAndItemsVOList, mandatoryRequestParam);
  }

  private List<ProductAndItemsVO> setItemCatalogForProductAndItemsVo(
      List<ProductAndItemsVO> productAndItemsVOList,
      MandatoryRequestParam mandatoryRequestParam) {
    Set<String> categoryCodeSet = new HashSet<>();
    Map<String, List<String>> productToCategoryCodeListMap = new HashMap<>();
    productAndItemsVOList.forEach(productAndItemsVO ->
        CommonUtil.fetchCategoryCodeList(productAndItemsVO.getProduct(), categoryCodeSet,
            productToCategoryCodeListMap, Constants.ALL));
    Map<String, List<ItemCatalogVO>> itemCategoryVOListMap = this.catalogService
        .getCategoryCodeToItemCatalogsMap(mandatoryRequestParam.getUsername(),
            mandatoryRequestParam.getRequestId(),
            categoryCodeSet.stream().collect(Collectors.toList()));
    productAndItemsVOList
        .forEach(productAndItemsVO -> CommonUtil.setItemCatalogs(productAndItemsVO.getProduct(),
            itemCategoryVOListMap, productToCategoryCodeListMap.get(
                productAndItemsVO.getProduct().getProductSku()), false));
    return productAndItemsVOList;
  }

  private void addPristineListingAttributesToItems(MandatoryRequestParam mandatoryRequestParam,
      List<ProductAndItemsVO> productAndItemsList) throws Exception {
    for (ProductAndItemsVO productAndItems : productAndItemsList) {
      Item item = productAndItems.getItems().get(ProductSearchServiceImpl.FIRST);
      if (Objects.nonNull(item) && Objects.nonNull(item.getPristineDataItem())) {
        PristineItemAndSiblingsVO pristineItemAndSiblingsVO = pristineCacheableService
            .findPristineItemAndItsSiblingsByPristineId(mandatoryRequestParam.getStoreId(),
                item.getPristineDataItem().getPristineId());
        List<PristineDataItem> pristineDataItems = new ArrayList<>();
        if(CollectionUtils.isNotEmpty(pristineItemAndSiblingsVO.getSiblings())) {
          for (PristineDataItem pristineItem : pristineItemAndSiblingsVO.getSiblings()) {
            if (StringUtils.isBlank(pristineItem.getPristineBrand()) && StringUtils
                .isBlank(pristineItem.getPristineModel())) {
              Item item1 = pristineCacheableService.findFirstItemByPristine(pristineItem);
              if(Objects.nonNull(item1)) {
                item1 = masterDataConstructorService.constructPristineDataItemWithMasterData(item1);
                pristineItem = item1.getPristineDataItem();
                pristineDataItems.add(pristineItem);
              }
            } else {
              pristineDataItems.add(pristineItem);
            }
          }
        }
        List<ProductAttribute> pristineAttributes = pristineDataItems.stream()
            .map(objectConverterService::convertPristineAttributeToProductAttribute)
            .collect(toList());
        productAndItems.getProduct().setDefiningAttributes(pristineAttributes);
        if (StringUtils.isBlank(item.getPristineDataItem().getPristineBrand()) && StringUtils
            .isBlank(item.getPristineDataItem().getPristineModel())) {
          item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
        }
        item.getPristineDataItem().setPristineListingAttributes(productAttributesUtil
            .translatePristineListingAttributeName(item.getPristineDataItem().getPristineListingAttributes(),
                productAttributesUtil.getCategoryListingParameterKey(item.getPristineDataItem())));
      }
    }
  }

  @Override
  public PristineProductAndItemsResponseVO getProductAndItemsResponseByPristineId(String storeId, String username,
      String requestId, String pristineShortId, boolean off2On) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    PristineProductAndItemsResponseVO response = new PristineProductAndItemsResponseVO();
    List<Item> itemList =
        this.itemCacheableService.findItemsByPristineId(storeId, username, requestId, pristineShortId, off2On);
    productHelperService.findAndConstructOfflineItems(storeId, itemList);

    checkArgument(CollectionUtils.isNotEmpty(itemList),
        ProductSearchServiceImpl.ITEM_SKU_NOT_FOUND_FOR_PRISTINEID + pristineShortId);
    List<ProductAttributeDetail> productAttributeDetails = getItemListingAttribute(
        itemList.get(ProductSearchServiceImpl.FIRST), productAttributesUtil
            .getCategoryListingParameterKey(itemList.get(ProductSearchServiceImpl.FIRST).getPristineDataItem()));
    response.setProductAttributeDetails(productAttributeDetails);
    List<PristineItemVO> pristineItemList = itemList.stream().map(item ->
        this.objectConverterService.convertToPristineItem(item)).collect(Collectors.toList());
    response.setPristineItems(pristineItemList);
    return response;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getPristineProductAndItemsInfoByItemSku(String storeId, String username, String requestId, String itemSkuL4OrL5Id,
      boolean needCategoryHierarchies, boolean combineOthersBundlings, boolean off2On) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSkuL4OrL5Id),
        ProductSearchServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    Set<String> productCodes = new HashSet<String>();
    Map<String, Item> itemSkuPristineDataMap = new HashMap<>();
    String defaultProductCode = null;
    Item item =
        this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSkuL4OrL5Id, combineOthersBundlings,
            off2On, false);
    if (item == null) {
      ItemPickupPoint offlineItem = this.itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, itemSkuL4OrL5Id);
      checkArgument(offlineItem != null, ProductSearchServiceImpl.ITEM_SKU_NOT_FOUND);

      String itemSkuFromOfflineItem = offlineItem.getItemSku();
      item = this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSkuFromOfflineItem,
          combineOthersBundlings, off2On, false);
      checkArgument(item != null, ProductServiceImpl.ITEM_NOT_FOUND);
      productHelperService.constructOfflineItem(item, CommonUtil.getOfflineItemByPickupPoint(offlineItem, false, null));
    } else {
      productHelperService.findAndConstructOfflineItems(storeId, Arrays.asList(item));
    }

    item.setUniqueId(item.getItemSku());

    checkArgument(item != null ,
        ProductSearchServiceImpl.ITEM_SKU_NOT_FOUND);
    checkArgument(item.getPristineDataItem() != null,
        ProductSearchServiceImpl.ITEMSKU_NOT_MAPPED_TO_PRISTINE);
    itemSkuPristineDataMap.put(item.getItemSku(), item);
    item = itemService.getItemsWithDiscountPrice(storeId, username, requestId, Collections.singletonList(item)).get(0);
    if (StringUtils.isNotEmpty(item.getPristineDataItem().getDefaultProductCode())) {
      defaultProductCode = item.getPristineDataItem().getDefaultProductCode();
      productCodes.add(defaultProductCode);
    }
    Product product = this.productCacheHelperService
        .findProductByStoreIdAndProductSku(storeId, item.getProductSku());
    checkArgument(product != null,
        ProductSearchServiceImpl.PRODUCT_RESULTS_NOT_FOUND_FOR_PRODUCT_SKUS + item.getProductSku());
    productCodes.add(product.getProductCode());
    if (StringUtils.isBlank(item.getPristineDataItem().getPristineBrand()) && StringUtils
        .isBlank(item.getPristineDataItem().getPristineModel())) {
      item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
    }
    MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchHelper
        .getProductAndItemsWithMasterDataDetailAndPristineDetail(storeId, username, requestId,
            Arrays.asList(item), Arrays.asList(product), defaultProductCode, productCodes);
    updateItemSkuDefiningAttributes(result.getProductAndItems(), itemSkuPristineDataMap);
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  private void setWholesaleRules(boolean needWholesaleData,
      MandatoryRequestParam mandatoryRequestParam, List<ProductAndItemsVO> productAndItemsList) {
    if (needWholesaleData) {
      Set<String> itemSkus =
          productAndItemsList.stream().flatMap(productAndItemsVO -> productAndItemsVO.getItems().stream()).filter(
              item -> CollectionUtils.isNotEmpty(item.getActivePromoBundlings()) && (
                  item.getActivePromoBundlings().contains(PROMO_BUNDLING_WHOLESALE_TYPE) || item
                      .getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE))).map(Item::getItemSku)
              .collect(Collectors.toSet());

      ActivePromoBundlingResponseVO activePromoBundlingResponseVO;
      if(CollectionUtils.isNotEmpty(itemSkus)) {
        activePromoBundlingResponseVO = promotionOutbound
            .getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PromoBundlingType.WHOLESALE, itemSkus);

        Map<String, List<WholesaleRuleVO>> promoDetailResponseMap =
            activePromoBundlingResponseVO.getPromoBundlingDetailResponseVOList().stream().collect(
                Collectors.toMap(PromoBundlingDetailResponseVO::getItemSku,
                    promoBundlingDetailResponseVO -> promoBundlingDetailResponseVO
                        .getWholesaleRules(), (oldValue, newValue) -> newValue));

        productAndItemsList.stream()
            .forEach(productAndItemsVO -> productAndItemsVO.getItems().forEach(item -> {
              constructWholesaleRules(promoDetailResponseMap, item);
            }));
      }
    }
  }

  private void constructWholesaleRules(
      Map<String, List<WholesaleRuleVO>> promoDetailResponseMap, Item item) {
    if (promoDetailResponseMap.containsKey(item.getItemSku())) {
      List<WholesaleRuleVO> wholesaleRuleVOS = promoDetailResponseMap.get(item.getItemSku());
      wholesaleRuleVOS.stream().forEach(wholesaleRuleVO -> {
        wholesaleRuleVO.setFinalPrice(this.itemPriceService
            .getFinalPrice(item.getPrice().stream().findFirst().orElse(new Price()),
                wholesaleRuleVO.getDiscountPercentage()));
      });
      item.setWholesaleRules(wholesaleRuleVOS);
    } else {
      item.setWholesaleRules(new ArrayList<>());
    }
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsSyncByPristineId(
      String storeId, String username, String requestId, String pristineShortId,
      boolean needCategoryHierarchies, String defaultItemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Set<String> productCodes = new HashSet<String>();
    String defaultProductSku = StringUtils.EMPTY;
    Map<String, Item> itemSkuPristineDataMap = new HashMap<>();
    String defaultProductCode = null;
    Set<String> productSkus = new HashSet<>();
    List<Product> productResult = null;
    List<Item> pristineIdMappedItemSkus =
        this.itemCacheableService.findItemsByPristineId(storeId, username, requestId, pristineShortId, false);
    checkArgument(CollectionUtils.isNotEmpty(pristineIdMappedItemSkus),
        ProductSearchServiceImpl.ITEM_SKU_NOT_FOUND_FOR_PRISTINEID + pristineShortId);
    for (Item item : pristineIdMappedItemSkus) {
      itemSkuPristineDataMap.put(item.getItemSku(), item);
      String productName = getProductNameBySimilarParameters(item);
      item.getPristineDataItem().setProductNameBySimilarParameters(productName);
      productSkus.add(item.getProductSku());
      if (StringUtils.isNotEmpty(item.getPristineDataItem().getDefaultProductCode())) {
        defaultProductCode = item.getPristineDataItem().getDefaultProductCode();
        productCodes.add(defaultProductCode);
      }
      if (defaultItemSku.equals(item.getItemSku())) {
        defaultProductSku = item.getProductSku();
      }
    }
    Product product = this.productCacheHelperService
        .findProductByStoreIdAndProductSku(storeId, defaultProductSku);
    productCodes.add(product.getProductCode());
    Map<String, Product> skuProductMap = this.productCacheHelperService
        .findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(storeId, productSkus);
    if (MapUtils.isNotEmpty(skuProductMap)) {
      productResult = new ArrayList(skuProductMap.values());
    }
    checkArgument(CollectionUtils.isNotEmpty(productResult),
        ProductSearchServiceImpl.PRODUCT_RESULTS_NOT_FOUND_FOR_PRODUCT_SKUS + productSkus);
    MasterDataDetailWithProductAndItemsResponseVo result = this.productSearchHelper
        .getProductAndItemsWithMasterDataDetailAndPristineDetail(storeId, username, requestId,
            pristineIdMappedItemSkus, productResult, defaultProductCode, productCodes);
    updateItemSkuDefiningAttributes(result.getProductAndItems(), itemSkuPristineDataMap);
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
        result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  private String getProductNameBySimilarParameters(Item item) throws IOException {

    Map<String, String> listingAttributes =
        item.getPristineDataItem().getPristineListingAttributes();
    Map<String, String> categorySimilarParameterMap = new HashMap();
    categorySimilarParameterMap = this.objectMapper
        .readValue(pristineSimilarParameters, new TypeReference<Map<String, String>>() {
        });
    String[] similarParameterKey =
        categorySimilarParameterMap.get(item.getPristineDataItem().getPristineCategory())
            .split(",");
    StringBuilder productName = new StringBuilder();

    for (String attributeName : similarParameterKey) {
      if (StringUtils.isNotEmpty(attributeName)) {
        productName.append(listingAttributes.get(attributeName.toUpperCase()))
            .append(StringUtils.SPACE);
      }
    }
    return productName.toString();
  }

  private void updateItemSkuDefiningAttributes(List<ProductAndItemsVO> productAndItems,
      Map<String, Item> itemSkuPristineCategoryMap) throws Exception {
    String[] categoryListingParameterKey =
        productAttributesUtil.getCategoryListingParameterKey(productAndItems.get(ProductSearchServiceImpl.FIRST)
            .getItems().get(ProductSearchServiceImpl.FIRST).getPristineDataItem());
    for (ProductAndItemsVO productAndItem : productAndItems) {
      List<ProductAttribute> definingAttributes = new ArrayList<>();
      for (Item item : productAndItem.getItems()){
       ProductAttribute productAttribute = new ProductAttribute();
        productAttribute.setItemSku(item.getItemSku());
        productAttribute.setProductAttributeDetails(
            getItemListingAttribute(itemSkuPristineCategoryMap.get(item.getItemSku()),
            categoryListingParameterKey));
        definingAttributes.add(productAttribute);
    }
      productAndItem.getProduct().setDefiningAttributes(definingAttributes);
    }
  }

  private List<ProductAttributeDetail> getItemListingAttribute(Item item, String[] categoryListingParameterKey)
      throws Exception {
    List<ProductAttributeDetail> productAttributeDetailList = new ArrayList<>();
    if (Objects.isNull(categoryListingParameterKey) || categoryListingParameterKey.length == 0) {
      item = masterDataConstructorService.constructPristineDataItemWithMasterData(item);
      if (Objects.nonNull(item.getPristineDataItem().getPristineListingAttributes())) {
        for (Entry<String, String> entry : item.getPristineDataItem().getPristineListingAttributes().entrySet()) {
          ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
          productAttributeDetail.setAttributeCode(StringUtils.EMPTY);
          productAttributeDetail.setAttributeName(entry.getKey());
          productAttributeDetail.setAttributeValue(entry.getValue());
          productAttributeDetailList.add(productAttributeDetail);
        }
      }
    } else if (categoryListingParameterKey.length > 0) {
      Map<String, String> listingAttributeMap = item.getPristineDataItem().getPristineListingAttributes();
      for (String attribute : categoryListingParameterKey) {
        ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
        String value = listingAttributeMap.get(attribute.toUpperCase());
        productAttributeDetail.setAttributeCode(StringUtils.EMPTY);
        productAttributeDetail.setAttributeName(productAttributesUtil.getAttributeNameTranslationMap().get(attribute));
        productAttributeDetail.setAttributeValue(value);
        productAttributeDetailList.add(productAttributeDetail);
      }
    }
    return productAttributeDetailList;
  }

  @Override
  public ReviewProductDetailVO getProductDetailForReviewByProductCode(String storeId,
      String username, String requestId, String productCode, String itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode),
        ProductSearchServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse = productSearchHelper
        .getProductMasterDataDetailByProductCode(storeId, username, requestId, productCode);
    ReviewProductDetailVO reviewProductDetailVO = new ReviewProductDetailVO();
    if (MapUtils.isNotEmpty(masterDataProductDetailResponse)) {
      MasterDataProductAndItemsVO masterDataProductAndItemsVO =
          masterDataProductDetailResponse.get(productCode);
      if (masterDataProductAndItemsVO != null) {
        MasterDataProduct masterDataProduct = masterDataProductAndItemsVO.getMasterDataProduct();
        reviewProductDetailVO.setProductName(masterDataProduct.getProductName());
        reviewProductDetailVO.setProductUrl(masterDataProduct.getUrl());
        reviewProductDetailVO.setProductCode(productCode);
        List<String> attributes = Optional
            .ofNullable(masterDataProductAndItemsVO.getMasterDataItems().get(itemSku))
            .map(masterDataItem -> masterDataItem.getMasterDataItemAttributeValues().stream()
                .map(MasterDataItemAttributeValue::getAttributeValue).collect(Collectors.toList()))
            .orElse(Collections.emptyList());
        reviewProductDetailVO.setAttributes(attributes);
      }
    }
    return reviewProductDetailVO;
  }

  @Override
  public Page<ProductAndItemSolr> getItemsByMerchantCode(String storeId,
      String merchantCode, Date createdDate, Pageable pageRequest) {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ProductSearchServiceImpl.MERCHANT_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(createdDate), CREATED_DATE_MUST_NOT_BE_NULL);
    return solrRepository.getItemsByMerchantCode(
        storeId, merchantCode, pageRequest);
  }

  @Override
  public ReviewProductDetailVO getProductDetailForReviewByProductSku(String storeId,
      String username, String requestId, String productSku, String itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku),
        ProductSearchServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    ReviewProductDetailVO reviewProductDetailVO = new ReviewProductDetailVO();
    Product product =
        this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
    checkArgument(product != null,
        ProductSearchServiceImpl.PRODUCT_RESULTS_NOT_FOUND_FOR_PRODUCT_SKUS);
    if (product.isSynchronized() || product.getMasterDataProduct() == null) {
      reviewProductDetailVO = getProductDetailForReviewByProductCode(storeId, username, requestId,
          product.getProductCode(), itemSku);
    } else {
      MasterDataProduct masterDataProduct = product.getMasterDataProduct();
      reviewProductDetailVO.setProductName(masterDataProduct.getProductName());
      reviewProductDetailVO.setProductUrl(masterDataProduct.getUrl());
      reviewProductDetailVO.setProductCode(StringUtils.isNotEmpty(product.getProductCode()) ?
          product.getProductCode() :
          StringUtils.EMPTY);
      List<ProductAttributeDetail> attributeDetails = product.getDefiningAttributes().stream()
          .filter(productAttribute -> productAttribute.getItemSku().equalsIgnoreCase(itemSku))
          .map(ProductAttribute::getProductAttributeDetails).findFirst()
          .orElse(Collections.emptyList());
      if (CollectionUtils.isNotEmpty(attributeDetails)) {
        List<String> attributes = attributeDetails.stream()
            .map(ProductAttributeDetail::getAttributeValue).collect(Collectors.toList());
        reviewProductDetailVO.setAttributes(attributes);
      }
      String locationPath = masterDataProduct.getMasterDataProductImages()
              .stream()
              .filter(image -> Boolean.TRUE.equals(image.isMainImage()))
              .map(MasterDataProductImage::getLocationPath)
              .findFirst().orElse(StringUtils.EMPTY);
      reviewProductDetailVO.setImageUrl(locationPath);

    }
    return reviewProductDetailVO;
  }

  @Override
  public Product updateForceReviewForProduct(String storeId, List<Item> itemList, boolean forceReview,
      boolean isArchive) {
    Product product = productService.findByStoreIdAndProductSku(storeId, itemList.get(0).getProductSku());
    Product savedProduct = product;
    if(Objects.nonNull(product)) {
      product.setForceReview(forceReview);
      product.setMarkForDelete(forceReview);
      product.setTakenDown(forceReview);
      if (isArchive) {
        product.setArchived(true);
      }
      savedProduct = productService.saveProductWithoutUpdatingSolr(product,
          Collections.singletonList(ProductChangeEventType.FORCE_REVIEW_FLAG_CHANGE), StringUtils.EMPTY);
    }
    return savedProduct;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo findMasterDataWithProductAndItemsInfoByItemCode(String storeId,
      String username, String requestId, String itemCode, boolean needCategoryHierarchies) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo result = null;
    List<Item> items = this.itemService.getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
    Set<String> productSkuSet = Optional.ofNullable(items).orElse(new ArrayList<>()).stream().map(Item::getProductSku)
        .collect(Collectors.toSet());
    List<Product> productList =
        this.productService.findByStoreIdAndProductSkuIn(storeId, new ArrayList<>(productSkuSet));
    checkState(CollectionUtils.isNotEmpty(productList), PRODUCT_RESULT_MUST_NOT_BE_EMPTY);
    result = this.productSearchHelper.getProductAndItemsWithMasterDataDetailWithoutL5Details(storeId, username, requestId,
            productList, items);
    this.productSearchHelper.setItemCatalogs(storeId, username, requestId, needCategoryHierarchies,
      result.getProductAndItems(), result.getMasterDataProducts());
    return result;
  }

  @Override
  public List<ItemPriceVO> getItemPriceByPristineId(String storeId, String username, String requestId,
      String pristineId) throws IOException, SolrServerException {
    checkArgument(StringUtils.isNotEmpty(storeId), ProductSearchServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pristineId), ProductSearchServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    List<ItemPriceVO> itemPriceVOS = new ArrayList<>();
    List<Item> items = itemService.getItemsByPristineId(storeId, pristineId);
    if (CollectionUtils.isNotEmpty(items)) {
      List<Item> filteredItems =
          items.stream().filter(item -> itemService.isItemBuyableAndDiscoverable(item)).collect(toList());
      filteredItems = itemService.getItemsWithDiscountPrice(storeId, username, requestId, filteredItems);
      addItemToItemPriceVOList(filteredItems, itemPriceVOS);
    }
    return itemPriceVOS;
  }

  @Override
  public ProductCountResponseVo getProductCountByType(String type, String merchantCode, boolean isActiveCounts) {
    if (Constants.ACTIVE.equalsIgnoreCase(type)) {
      return productSolrRepository.getActiveAndOosProductCount(merchantCode);
    } else {
      ProductCountResponseVo productCountResponseVo = new ProductCountResponseVo();
      if (isActiveCounts) {
        productCountResponseVo = productSolrRepository.getActiveAndOosProductCount(merchantCode);
        return productSolrRepository.getSuspendedAndArchivedProductCount(merchantCode, productCountResponseVo);
      } else {
        return productSolrRepository.getSuspendedAndArchivedProductCount(merchantCode, productCountResponseVo);
      }
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.PRODUCT_COUNT_CACHE_MANAGER, value = CacheNames.PRODUCT_COUNT, key = "#merchantCode + '-' + #type + '-' + #isActiveCounts", unless = "#result == null")
  public ProductCountResponseVo getProductCountByTypeCacheable(String type, String merchantCode,
      boolean isActiveCounts) {
    return getProductCountByType(type, merchantCode, isActiveCounts);
  }

  @Override
  public MasterDataWithProductItemsVo getMasterDataWithProductItemsVo(String storeId,
    String username, String requestId, String productCode, boolean needCategoryHierarchies,
    boolean combineOthersBundlings, boolean off2On) throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
      this.getProductAndItemsSyncByProductCodes(storeId, username, requestId, productCode,
        needCategoryHierarchies, combineOthersBundlings, off2On);
    Optional.ofNullable(masterDataDetailWithProductAndItemsResponseVo.getProductAndItems())
      .orElse(new ArrayList<>()).forEach(
        productAndItemsVO -> this.productService.checkProductAndItemsForForceReview(
          Arrays.asList(productAndItemsVO.getProduct()), productAndItemsVO.getItems()));
    MasterDataWithProductItemsVo response =
      CommonUtil.toMasterDataWithProductItemsVo(masterDataDetailWithProductAndItemsResponseVo);
    response.setActiveProductCount(
      masterDataDetailWithProductAndItemsResponseVo.getActiveProductCount());
    this.productService.setSellerPromoBundlings(storeId, response.getProductItemsVos());
    return response;
  }

  @Override
  public Page<ProductSummaryResponseV2Vo> getProductSummaryResponseV2(String storeId, int page, int size,
      ProductSummaryRequestV2 productSummaryRequestV2) {
    return productSolrRepository.getProductSummaryV2(storeId, page, size,
        ProductAndItemsUtil.toProductSummaryRequestV2Vo(productSummaryRequestV2));
  }

  @Override
  public Page<HalalDashboardProductsResponseVo> getHalalDashboardProducts(String storeId, int page, int size,
      HalalProductsFilterRequest halalProductsFilterRequest) {
    return productSolrRepository.getHalalDashboardProductsResponse(storeId, page, size,
        ProductAndItemsUtil.toHalalDashboardFilterRequestVo(halalProductsFilterRequest));
  }

  private void addItemToItemPriceVOList(List<Item> items, List<ItemPriceVO> priceVOS) {
    for (Item item : items) {
      ItemPriceVO itemPriceVO = new ItemPriceVO.ItemPriceBuilder().setItemSku(item.getItemSku())
          .setBuyable(item.getItemViewConfigs().stream().anyMatch(config -> config.isBuyable()))
          .setDiscoverable(item.getItemViewConfigs().stream().anyMatch(config -> config.isDiscoverable()))
          .setMerchantCode(item.getMerchantCode()).build();
      Price price =
          item.getPrice().stream().filter(p -> DEFAULT.equalsIgnoreCase(p.getChannel())).findFirst().orElse(null);
      if (Objects.nonNull(price)) {
        itemPriceVO.setOfferPrice(price.getOfferPrice());
        itemPriceVO.setListPrice(price.getListPrice());
      }
      priceVOS.add(itemPriceVO);
    }
  }
}
