package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductVO;
import com.gdn.x.product.outbound.api.ProductAnalyticsOutbound;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ResponseHelper;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class ProductSearchHelperServiceImpl implements ProductSearchHelperService {

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "store id must not be blank";

  private static final String SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL =
      "set of search param must not be null";
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductSearchHelperServiceImpl.class);


  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Value("${populate.master.category.hierarchy.in.reindex.api}")
  private boolean populateMasterCategoryHierarchyInReindexApi;

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getMasterDataProductAndMasterDataItemForReindex(
      String storeId, String username, String requestId, Set<String> productCodes,
      boolean inAllProducts) throws Exception {
    productCodes.remove(null);
    Map<String, MasterDataProductAndItemsVO> mapOfmasterDataProductAndItemsVo =
        this.masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId,
            productCodes, inAllProducts);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
    for (Entry<String, MasterDataProductAndItemsVO> masterDataProductAndItemsVo : mapOfmasterDataProductAndItemsVo
        .entrySet()) {
      String productCode = masterDataProductAndItemsVo.getKey();
      MasterDataProductAndItemsVO value = masterDataProductAndItemsVo.getValue();
      masterDataProducts.put(productCode, value.getMasterDataProduct());
      masterDataItems.putAll(value.getMasterDataItems());
    }
    return new MasterDataDetailWithProductAndItemsResponseVo(masterDataProducts, masterDataItems,
        null);
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetail(String storeId, String username, String requestId, List<Product> products,
      boolean combineOthersBundlings, boolean off2On) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    for (Product product : products) {
      productCodes.add(product.getProductCode());
      productSkus.add(product.getProductSku());
    }
    List<Item> items =
        this.itemService.getItemsWithDiscountPriceByProductSkus(storeId, username, requestId,
            productSkus, combineOthersBundlings, off2On);
    productHelperService.findAndConstructOfflineItems(storeId, items);
    MasterDataDetailWithProductAndItemsResponseVo finalResult =
        new MasterDataDetailWithProductAndItemsResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId,
            productCodes, false);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse
        .entrySet()) {
      masterDataProducts.put(entry.getKey(), entry.getValue().getMasterDataProduct());
      masterDataItems.putAll(entry.getValue().getMasterDataItems());
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    finalResult.setProductAndItems(this.objectConverterService.convertAndValidateMasterDataExists(
        products, items, finalResult.getMasterDataProducts(), finalResult.getMasterDataItems()));
    return finalResult;
  }

  @Override
  public MasterDataDetailWithProductAndItemResponseVo getProductAndItemWithMasterDataDetail(
      String storeId, String username, String requestId, List<Product> products, String itemSku,
      boolean combineOthersBundlings, boolean off2On) throws Exception {
    Set<String> productCodes = new HashSet<>();
    for (Product product : products) {
      productCodes.add(product.getProductCode());
    }
    Item item = this.itemService
        .getItem(storeId, requestId, username, itemSku, true, true, combineOthersBundlings, false, null, off2On, false);
    productHelperService.findAndConstructOfflineItems(storeId, Collections.singletonList(item));
    MasterDataDetailWithProductAndItemResponseVo finalResult =
        new MasterDataDetailWithProductAndItemResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId,
            productCodes, false);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<>();
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse
        .entrySet()) {
      masterDataProducts.put(entry.getKey(), entry.getValue().getMasterDataProduct());
      masterDataItems.putAll(entry.getValue().getMasterDataItems());
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    this.objectConverterService.convertAndValidateMasterDataExists(
        products, Collections.singletonList(item), finalResult.getMasterDataProducts(), finalResult.getMasterDataItems());
    finalResult.setProduct(products.stream()
        .filter(product -> StringUtils.equals(product.getProductSku(), item.getProductSku())).findFirst().orElse(null));
    finalResult.setItem(item);
    return finalResult;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsWithMasterDataDetailByDefaultProduct(String storeId, String username, String requestId, List<Product> products,
      boolean combineOthersBundlings, boolean off2On) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    Map<String, String> skuProductCodeMap = new HashMap<>();
    Map<String, String> productCodeDefaultPCMap = new HashMap<>();
    for (Product product : products) {
      productCodes.add(product.getProductCode());
      productSkus.add(product.getProductSku());
      skuProductCodeMap.put(product.getProductSku(), product.getProductCode());
    }
    List<Item> items = this.itemService
        .getItemsWithDiscountPriceByProductSkus(storeId, username, requestId, productSkus, combineOthersBundlings,
            off2On);
    productHelperService.findAndConstructOfflineItems(storeId, items);
    for (Item item : items) {
      if (StringUtils.isNotEmpty(skuProductCodeMap.get(item.getProductSku()))
          && item.getPristineDataItem() != null && StringUtils
          .isNotEmpty(item.getPristineDataItem().getDefaultProductCode()) && !skuProductCodeMap
          .get(item.getProductSku()).equals(item.getPristineDataItem().getDefaultProductCode())) {
        productCodeDefaultPCMap.put(skuProductCodeMap.get(item.getProductSku()),
            item.getPristineDataItem().getDefaultProductCode());
        productCodes.add(item.getPristineDataItem().getDefaultProductCode());
      }
    }
    MasterDataDetailWithProductAndItemsResponseVo finalResult =
        new MasterDataDetailWithProductAndItemsResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse
        .entrySet()) {
      if (productCodeDefaultPCMap.containsKey(entry.getKey()) && masterDataProductDetailResponse.containsKey(
          productCodeDefaultPCMap.get(entry.getKey()))) {
        masterDataProducts.put(entry.getKey(),
            CommonUtil.updateMasterDataWithDPCMasterData(entry.getValue().getMasterDataProduct(),
                masterDataProductDetailResponse.get(productCodeDefaultPCMap.get(entry.getKey()))
                    .getMasterDataProduct()));
      } else {
        masterDataProducts.put(entry.getKey(), entry.getValue().getMasterDataProduct());
      }
      masterDataItems.putAll(entry.getValue().getMasterDataItems());
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    finalResult.setProductAndItems(this.objectConverterService
        .convertAndValidateMasterDataExists(products, items, finalResult.getMasterDataProducts(),
            finalResult.getMasterDataItems()));
    return finalResult;
  }

  @Override
  public MasterDataDetailWithProductAndItemResponseVo getProductAndItemWithMasterDataDetailByDefaultProduct(
      String storeId, String username, String requestId, List<Product> products, String itemSku,
      boolean combineOthersBundlings, boolean off2On) throws Exception {
    Set<String> productCodes = new HashSet<>();
    Map<String, String> skuProductCodeMap = new HashMap<>();
    Map<String, String> productCodeDefaultPCMap = new HashMap<>();
    for (Product product : products) {
      productCodes.add(product.getProductCode());
      skuProductCodeMap.put(product.getProductSku(), product.getProductCode());
    }
    Item item = this.itemService
        .getItem(storeId, requestId, username, itemSku, true, true, combineOthersBundlings, false, null, off2On, false);
    productHelperService.findAndConstructOfflineItems(storeId, Collections.singletonList(item));

    if (StringUtils.isNotEmpty(skuProductCodeMap.get(item.getProductSku()))
        && Objects.nonNull(item.getPristineDataItem()) && StringUtils
        .isNotEmpty(item.getPristineDataItem().getDefaultProductCode()) && !skuProductCodeMap
        .get(item.getProductSku()).equals(item.getPristineDataItem().getDefaultProductCode())) {
      productCodeDefaultPCMap.put(skuProductCodeMap.get(item.getProductSku()),
          item.getPristineDataItem().getDefaultProductCode());
      productCodes.add(item.getPristineDataItem().getDefaultProductCode());
    }
    MasterDataDetailWithProductAndItemResponseVo finalResult = new MasterDataDetailWithProductAndItemResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<>();
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse.entrySet()) {
      if (productCodeDefaultPCMap.containsKey(entry.getKey())) {
        masterDataProducts.put(entry.getKey(),
            CommonUtil.updateMasterDataWithDPCMasterData(entry.getValue().getMasterDataProduct(),
                masterDataProductDetailResponse.get(productCodeDefaultPCMap.get(entry.getKey()))
                    .getMasterDataProduct()));
      } else {
        masterDataProducts.put(entry.getKey(), entry.getValue().getMasterDataProduct());
      }
      masterDataItems.putAll(entry.getValue().getMasterDataItems());
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    this.objectConverterService.convertAndValidateMasterDataExists(
        products, Collections.singletonList(item), finalResult.getMasterDataProducts(), finalResult.getMasterDataItems());
    finalResult.setProduct(products.stream()
        .filter(product -> StringUtils.equals(product.getProductSku(), item.getProductSku())).findFirst().orElse(null));
    finalResult.setItem(item);
    return finalResult;
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsResponseVo
  getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
      String storeId, String username, String requestId, List<Product> products,
      boolean combineOthersBundlings, List<Item> items, String pickupPointCode) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    Map<String, String> skuProductCodeMap = new HashMap<>();
    Map<String, String> productCodeDefaultPCMap = new HashMap<>();
    products.forEach(product -> {
      productCodes.add(product.getProductCode());
      productSkus.add(product.getProductSku());
      skuProductCodeMap.put(product.getProductSku(), product.getProductCode());
    });
    if(CollectionUtils.isEmpty(items)) {
      items = this.itemService
          .findByStoreIdAndMarkForDeleteFalseAndProductSkus(storeId, username, requestId,
              productSkus, combineOthersBundlings);
    }
    if(StringUtils.isNotBlank(pickupPointCode)) {
      productHelperService.findAndConstructOfflineItemsByPickupPointCode(storeId, items, pickupPointCode);
    }
    getProductCodeDefaultProductCodeMap(items, productCodes, skuProductCodeMap);
    SimpleMasterDataDetailWithProductAndItemsResponseVo finalResult =
        this.getMasterDataProductDetail(storeId, username, requestId, productCodes,
            productCodeDefaultPCMap, products);
    finalResult.setProductAndItems(this.objectConverterService
        .convertAndValidateSimpleMasterDataExists(products, items, finalResult.getMasterDataProducts(),
            finalResult.getMasterDataItems()));
    return finalResult;
  }

  @Override
  public SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
      String storeId, String username, String requestId, List<Product> products, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    Map<String, String> skuProductCodeMap = new HashMap<>();
    products.forEach(product -> {
      productCodes.add(product.getProductCode());
      productSkus.add(product.getProductSku());
      skuProductCodeMap.put(product.getProductSku(), product.getProductCode());
    });
    getProductCodeDefaultProductCodeMap(items, productCodes, skuProductCodeMap);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo finalResult =
        this.getMasterDataProductDetailV2(storeId, username, requestId, productCodes, products);
    finalResult.setProductAndItems(this.objectConverterService
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, finalResult.getMasterDataProducts(),
            finalResult.getMasterDataItems()));
    return finalResult;
  }

  /**
   * Get ProductCode and DefaultProductCode Map
   * @param items
   * @param productCodes
   * @param skuProductCodeMap
   * @return
   */
  private Map<String, String> getProductCodeDefaultProductCodeMap(List<Item> items,
      Set<String> productCodes, Map<String, String> skuProductCodeMap){
    Map<String, String> productCodeDefaultPCMap = new HashMap<>();
    items.stream().filter(
        item -> StringUtils.isNotEmpty(skuProductCodeMap.get(item.getProductSku()))
            && item.getPristineDataItem() != null && StringUtils
            .isNotEmpty(item.getPristineDataItem().getDefaultProductCode()) && !skuProductCodeMap
            .get(item.getProductSku()).equals(item.getPristineDataItem().getDefaultProductCode()))
        .map(item -> {
          productCodeDefaultPCMap.put(skuProductCodeMap.get(item.getProductSku()),
              item.getPristineDataItem().getDefaultProductCode());
          productCodes.add(item.getPristineDataItem().getDefaultProductCode());
          return productCodeDefaultPCMap;
        });
    return productCodeDefaultPCMap;
  }

  /**
   * Get MasterDataProduct and MasterDataItem detail by productCode
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @param productCodeDefaultPCMap
   * @return
   * @throws Exception
   */
  private SimpleMasterDataDetailWithProductAndItemsResponseVo getMasterDataProductDetail(
      String storeId, String username, String requestId, Set<String> productCodes,
      Map<String, String> productCodeDefaultPCMap, List<Product> products) throws Exception{

    SimpleMasterDataDetailWithProductAndItemsResponseVo finalResult =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);
    setReviewPendingFlagBasedOnSellerType(storeId, username, requestId, products, masterDataProductDetailResponse);
    Map<String, SimpleMasterDataProductVO> masterDataProducts =
        new HashMap<String, SimpleMasterDataProductVO>();
    Map<String, SimpleMasterDataItemVO> masterDataItems =
        new HashMap<String, SimpleMasterDataItemVO>();
    masterDataProductDetailResponse.entrySet().forEach( entry -> {
      if (productCodeDefaultPCMap.containsKey(entry.getKey())) {
        entry.getValue()
            .setMasterDataProduct(CommonUtil.updateMasterDataWithDPCMasterData(entry.getValue()
                    .getMasterDataProduct(),
                masterDataProductDetailResponse.get(productCodeDefaultPCMap.get(entry.getKey()))
                    .getMasterDataProduct()));
        masterDataProducts.put(entry.getKey(),
            SimpleMasterDataProductVO.toMasterDataProductVo(entry.getValue()));
      } else {
        masterDataProducts
            .put(entry.getKey(), SimpleMasterDataProductVO.toMasterDataProductVo(entry.getValue()));
      }
      masterDataItems
          .putAll(SimpleMasterDataItemVO.toMasterDataItemVo(entry.getValue().getMasterDataItems(), new HashMap<>()));
    });
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    return finalResult;
  }

  private SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2(
      String storeId, String username, String requestId, Set<String> productCodes, List<Product> products) throws Exception{

    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo finalResult =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);
    setReviewPendingFlagBasedOnSellerType(storeId, username, requestId, products, masterDataProductDetailResponse);
    Map<String, SimpleMasterDataProductVO> masterDataProducts =
        new HashMap<String, SimpleMasterDataProductVO>();
    Map<String, SimpleMasterDataItemVO> masterDataItems =
        new HashMap<String, SimpleMasterDataItemVO>();
    masterDataProductDetailResponse.entrySet().forEach( entry -> {
      Map<String,String> valueAndValueTypeMap = new HashMap<>();
      getValueAndValueTypeMap(valueAndValueTypeMap, entry.getValue());
        masterDataProducts
            .put(entry.getKey(), SimpleMasterDataProductVO.toMasterDataProductVo(entry.getValue()));
      masterDataItems
          .putAll(SimpleMasterDataItemVO.toMasterDataItemVo(entry.getValue().getMasterDataItems(), valueAndValueTypeMap));
    });
    ResponseHelper.setVideoUrl(products, masterDataProducts);
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    return finalResult;
  }

  private Map<String, String> getValueAndValueTypeMap(Map<String, String> valueAndValueTypeMap,
      MasterDataProductAndItemsVO masterDataProductAndItemsVO) {
    masterDataProductAndItemsVO.getMasterDataProduct().getMasterDataProductAttributes().forEach(
        masterDataProductAttribute -> masterDataProductAttribute.getMasterDataProductAttributeValues().stream().filter(
                masterDataProductAttributeValue -> Objects.nonNull(
                    masterDataProductAttributeValue.getAllowedAttributeValue()))
            .forEach(masterDataProductAttributeValue -> {
              String attributeValue = masterDataProductAttributeValue.getAllowedAttributeValue().getValue();
              String valueType = masterDataProductAttributeValue.getAllowedAttributeValue().getValueType();
              valueAndValueTypeMap.put(attributeValue, valueType);
            }));
    return valueAndValueTypeMap;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsWithMasterDataDetailForReindexBySimpleProducts(
      String storeId, String username, String requestId,
      List<SimpleProductRequestVo> simpleProductRequests) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    for (SimpleProductRequestVo result : simpleProductRequests) {
      productCodes.add(result.getProductCode());
      productSkus.add(result.getProductSku());
    }
    MasterDataDetailWithProductAndItemsResponseVo responseVo =
        this.getProductAndItemsWithMasterDataForReindex(storeId, username, requestId, productSkus,
            productCodes);
    return responseVo;
  }

  private void setReviewPendingFlagBasedOnSellerType(String storeId, String username, String requestId,
      List<Product> products, Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse) {
    try {
      Set<String> productCodes = masterDataProductDetailResponse.entrySet().stream()
          .filter(entry -> Boolean.TRUE.equals(entry.getValue().getMasterDataProduct().isReviewPending()))
          .map(entry -> entry.getKey()).collect(Collectors.toSet());
      if (CollectionUtils.isNotEmpty(productCodes)) {
        Map<String, Boolean> goodSellerMapByProductCode =
            getGoodSellerBySellerCode(storeId, username, requestId, productCodes, products);
        productCodes.forEach(productCode -> masterDataProductDetailResponse.get(productCode).getMasterDataProduct()
            .setReviewPending(Boolean.FALSE.equals(goodSellerMapByProductCode.get(productCode))));
      }
    } catch (Exception e) {
      LOGGER.error("Exception caught while setting review pending flag based ongood seller type. products : {} ",
          products, e);
    }
  }

  private Map<String, Boolean> getGoodSellerBySellerCode(String storeId, String username, String requestId,
      Set<String> productCodes, List<Product> products) {
    Map<String, Boolean> goodSellerMapByProductCode = new HashMap<>();
    Map<String, Boolean> goodSellerMapBySellerCode = new HashMap<>();
    Map<String, String> sellerCodeMap =
        products.stream().collect(Collectors.toMap(Product::getProductCode, Product::getMerchantCode, (v1, v2) -> v2));
    for (String productCode : productCodes) {
      String sellerCode = sellerCodeMap.get(productCode);
      if (!goodSellerMapBySellerCode.containsKey(sellerCode)) {
        SellerDetailResponse sellerDetailResponse =
            productAnalyticsOutbound.checkGoodSeller(storeId, requestId, Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID_X_PRODUCT, username, sellerCode);
        goodSellerMapByProductCode.put(productCode, sellerDetailResponse.isGoodSeller());
        goodSellerMapBySellerCode.put(sellerCode, sellerDetailResponse.isGoodSeller());
      } else {
        goodSellerMapByProductCode.put(productCode, goodSellerMapBySellerCode.get(sellerCode));
      }
    }
    return goodSellerMapByProductCode;
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetailForReindexBySolrResult(
      String storeId, String username, String requestId, List<ProductAndItemSolr> solrResult)
      throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    for (ProductAndItemSolr result : solrResult) {
      productCodes.add(result.getProductCode());
      productSkus.add(result.getProductSku());
    }
    MasterDataDetailWithProductAndItemsResponseVo responseVo =
        this.getProductAndItemsWithMasterDataForReindex(storeId, username, requestId, productSkus,
            productCodes);
    return responseVo;
  }

  private MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataForReindex(
      String storeId, String username, String requestId, Set<String> productSkus,
      Set<String> productCodes) throws Exception {
    List<Product> products = this.productService.getProducts(storeId, productSkus);
    List<Item> items =
        this.itemService.getItemsWithDiscountPriceByProductSkus(storeId, username, requestId,
            productSkus, false, false);
    MasterDataDetailWithProductAndItemsResponseVo responseVo =
        this.getMasterDataProductAndMasterDataItemForReindex(storeId, username, requestId,
            productCodes, false);
    responseVo.setProductAndItems(this.objectConverterService.convertAndValidateMasterDataExists(
        products, items, responseVo.getMasterDataProducts(), responseVo.getMasterDataItems()));
    return responseVo;
  }

  @Override
  public void setItemCatalogs(String storeId, String username, String requestId,
      boolean needCategoryHierarchy, List<ProductAndItemsVO> productAndItems,
      Map<String, MasterDataProduct> mapOfMasterDataProduct) throws Exception {
    if (needCategoryHierarchy) {
      for (ProductAndItemsVO productAndItem : productAndItems) {
        Product product = productAndItem.getProduct();
        if (product.getMasterCatalog() == null) {
          MasterDataProduct masterDataProduct =
              mapOfMasterDataProduct.get(product.getProductCode());
          if (masterDataProduct != null) {
            product.setMasterCatalog(masterDataProduct.getMasterCatalog());
          }
        }
        List<ItemCatalogVO> itemCatalogs =
          this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, product);
        product.setItemCatalogs(itemCatalogs);
      }
    }
  }


  @Override
  public void setSimpleItemCatalogs(String storeId, String username, String requestId,
      List<SimpleProductAndItemsVO> productAndItems,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct) throws Exception {
    productAndItems.forEach(productAndItem -> {
      SimpleProductVO simpleProductVO = productAndItem.getSimpleProduct();
      if (StringUtils.isNotBlank(simpleProductVO.getProductCode()) && Objects
          .nonNull(mapOfMasterDataProduct.get(simpleProductVO.getProductCode())) && Objects
          .isNull(mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).getItemCatalogs())) {
        this.setItemCatalogsInMasterDataProduct(username, requestId, simpleProductVO,
            mapOfMasterDataProduct);
      } else {
        this.setItemCatalogsProductCodeNotExistProducts(username, requestId, simpleProductVO);
      }
    });
  }

  @Override
  public void setSimpleItemCatalogsInMasterData(String storeId, String username, String requestId,
      List<SimpleProductAndItemsAndItemPickupPointV0> productAndItemsAndItemPickupPointV0s,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct) {
    productAndItemsAndItemPickupPointV0s.forEach(productAndItem -> {
      SimpleProductVO simpleProductVO = productAndItem.getSimpleProduct();
      if (StringUtils.isNotBlank(simpleProductVO.getProductCode()) && Objects
          .nonNull(mapOfMasterDataProduct.get(simpleProductVO.getProductCode())) && Objects
          .isNull(mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).getItemCatalogs())) {
        this.setItemCatalogsInMasterDataProduct(username, requestId, simpleProductVO,
            mapOfMasterDataProduct);
      } else {
        this.setItemCatalogsProductCodeNotExistProducts(username, requestId, simpleProductVO);
      }
    });
  }

  /**
   * Set ItemCatalogs for older products does not have ProductCode
   * @param username
   * @param requestId
   * @param simpleProductVO
   */
  private void setItemCatalogsProductCodeNotExistProducts(String username, String requestId,
      SimpleProductVO simpleProductVO) {
    if (StringUtils.isBlank(simpleProductVO.getProductCode())) {
      List<ItemCatalogVOV2> itemCatalogVOV2s = getCategoryCodesFromSalesCatalog(username, requestId,
          simpleProductVO.getSimpleAsyncMasterDataProduct().getSalesCatalogs());
      simpleProductVO.getSimpleAsyncMasterDataProduct().setItemCatalogs(itemCatalogVOV2s);
      populateMasterCategoryHierarchyForUnsyncProducts(username, requestId, simpleProductVO);
    }
  }

  private void populateMasterCategoryHierarchyForUnsyncProducts(String username, String requestId, SimpleProductVO simpleProductVO) {
    if (populateMasterCategoryHierarchyInReindexApi) {
      try {
        String masterCategoryCode = simpleProductVO.getSimpleAsyncMasterDataProduct().getMasterCategoryCode();
        if (StringUtils.isNotBlank(masterCategoryCode)) {
          List<ItemCatalogVOV2> masterCategoryHierarchy =
              getItemCatalogVOV2s(username, requestId, masterCategoryCode);
          Optional.ofNullable(simpleProductVO.getSimpleAsyncMasterDataProduct().getItemCatalogs())
              .orElse(new ArrayList<>()).addAll(masterCategoryHierarchy);
        }
      } catch (Exception e) {
        LOGGER.error("Error while getting category hierarchy for productCode : {} ", simpleProductVO.getProductCode(),
            e);
      }
    }
  }

  private List<ItemCatalogVOV2> getItemCatalogVOV2s(String username, String requestId, String masterCategoryCode)
      throws Exception {
    return this.catalogService.getItemCatalogsWithCategoryHierarchyV2(username, requestId,
        Collections.singletonList(masterCategoryCode));
  }

  /**
   * Set ItemCatalogs in SimpleMasterDataProduct
   *
   * @param username
   * @param requestId
   * @param simpleProductVO
   * @param mapOfMasterDataProduct
   */
  private void setItemCatalogsInMasterDataProduct(String username, String requestId, SimpleProductVO simpleProductVO,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct) {
    List<ItemCatalogVOV2> itemCatalogVOV2s = getCategoryCodesFromSalesCatalog(username, requestId,
        mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).getSalesCatalogs());
    mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).setItemCatalogs(itemCatalogVOV2s);
    populateMasterCategoryHierarchy(username, requestId, simpleProductVO, mapOfMasterDataProduct);
  }

  private void populateMasterCategoryHierarchy(String username, String requestId, SimpleProductVO simpleProductVO,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct) {
    if (populateMasterCategoryHierarchyInReindexApi) {
      try {
        String masterCategoryCode =
            mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).getMasterCategoryCode();
        if (StringUtils.isNotBlank(masterCategoryCode)) {
          List<ItemCatalogVOV2> masterCategoryHierarchy = getItemCatalogVOV2s(username, requestId, masterCategoryCode);
          Optional.ofNullable(mapOfMasterDataProduct.get(simpleProductVO.getProductCode()).getItemCatalogs())
              .orElse(new ArrayList<>()).addAll(masterCategoryHierarchy);
        }
      } catch (Exception e) {
        LOGGER.error("Error while getting category hierarchy for productCode : {} ", simpleProductVO.getProductCode(),
            e);
      }
    }
  }

  /**
   * Get CategoryCodes product's SalesCatalog
   * @param username
   * @param requestId
   * @param salesCatalogs
   * @return
   */
  private List<ItemCatalogVOV2> getCategoryCodesFromSalesCatalog(String username, String requestId,
      List<SalesCatalog> salesCatalogs) {
    List<String> categoryCodes = new ArrayList<>();
    List<ItemCatalogVOV2> itemCatalogVOV2s = new ArrayList<>();
    try {
      if (CollectionUtils.isNotEmpty(salesCatalogs)) {
        categoryCodes = salesCatalogs.stream().map(SalesCatalog :: getListOfCategories)
            .flatMap(List :: stream)
            .map(Category:: getCategoryCode)
            .collect(Collectors.toList());
        itemCatalogVOV2s = this.catalogService
            .getItemCatalogsWithCategoryHierarchyV2(username, requestId, categoryCodes);
      }
    } catch (Exception ex) {
      LOGGER.error("Error while getting category hierarchy for categoryCodes : {}", categoryCodes);
    }
    return itemCatalogVOV2s;
  }

  @Override
  public void validateParameters(String storeId, Set<String> param) {
    checkArgument(param != null,
        ProductSearchHelperServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
    param.removeAll(Collections.singleton(null));
    param.remove("");
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductSearchHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!param.isEmpty(),
        ProductSearchHelperServiceImpl.SET_OF_SEARCH_PARAM_MUST_NOT_BE_NULL);
  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsWithMasterDataDetailAndPristineDetail(
      String storeId, String username, String requestId, List<Item> items, List<Product> products,
      String defaultProductCode, Set<String> productCodes)
      throws Exception {
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
    MasterDataDetailWithProductAndItemsResponseVo finalResult =
        new MasterDataDetailWithProductAndItemsResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService
            .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse
        .entrySet()) {
      if (masterDataProductDetailResponse.size() == 1) {
        masterDataProducts.put(entry.getKey(),
            masterDataProductDetailResponse.get(entry.getKey()).getMasterDataProduct());
        if (CollectionUtils.isNotEmpty(products) && StringUtils
            .isNotBlank(products.get(0).getProductCode()) && products.get(0).getProductCode()
            .equals(entry.getKey())) {
          masterDataItems.putAll(entry.getValue().getMasterDataItems());
        }
      } else {
        if (StringUtils.isNotEmpty(defaultProductCode) && !defaultProductCode
            .equals(entry.getKey())) {
          masterDataProducts.put(entry.getKey(),
              CommonUtil.updateMasterDataWithDPCMasterData(entry.getValue().getMasterDataProduct(),
                  masterDataProductDetailResponse.get(defaultProductCode).getMasterDataProduct()));
          masterDataItems.putAll(entry.getValue().getMasterDataItems());
        }
      }
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    finalResult.setProductAndItems(this.objectConverterService
        .convertAndValidateMasterDataExistsForPristine(products, items, finalResult.getMasterDataProducts(),
            finalResult.getMasterDataItems()));
    return finalResult;
  }

  @Override
  public Map<String, MasterDataProductAndItemsVO> getProductMasterDataDetailByProductCode(
      String storeId, String username, String requestId, String productCode) throws Exception {
    Set productCodes = new HashSet();
    productCodes.add(productCode);
    return this.masterDataService
        .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);

  }

  @Override
  public MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetailWithoutL5Details(
      String storeId, String username, String requestId, List<Product> products, List<Item> items) throws Exception {
    Set<String> productSkus = new HashSet<String>();
    Set<String> productCodes = new HashSet<String>();
    for (Product product : products) {
      productCodes.add(product.getProductCode());
      productSkus.add(product.getProductSku());
    }
    MasterDataDetailWithProductAndItemsResponseVo finalResult =
        new MasterDataDetailWithProductAndItemsResponseVo();
    Map<String, MasterDataProductAndItemsVO> masterDataProductDetailResponse =
        this.masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId,
            productCodes, false);
    Map<String, MasterDataProduct> masterDataProducts = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItems = new HashMap<String, MasterDataItem>();
    for (Entry<String, MasterDataProductAndItemsVO> entry : masterDataProductDetailResponse
        .entrySet()) {
      masterDataProducts.put(entry.getKey(), entry.getValue().getMasterDataProduct());
      masterDataItems.putAll(entry.getValue().getMasterDataItems());
    }
    finalResult.setMasterDataProducts(masterDataProducts);
    finalResult.setMasterDataItems(masterDataItems);
    finalResult.setProductAndItems(this.objectConverterService.convertAndValidateMasterDataExists(
        products, items, finalResult.getMasterDataProducts(), finalResult.getMasterDataItems()));
    return finalResult;
  }
}
